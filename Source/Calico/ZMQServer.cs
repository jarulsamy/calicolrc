// IPython kernel backend in C#
// Doug Blank

using System;
using System.Text;
using System.IO;
using System.Threading;
using ZeroMQ;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters; // CustomCreationConverter
using System.Collections.Generic; // IDictionary
using System.Security.Cryptography;

// IList
using System.Collections;
using System.Reflection;// IEnumerator

using Calico;

public static class ZMQServer {

    public static string encode(IDictionary<string, object> dict) {
	return JsonConvert.SerializeObject(dict);
    }

    public static string now() {
	return DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.ffffff");
    }

    public static string msg_id() {
	return System.Guid.NewGuid().ToString();
    }

    public static IDictionary<string, object> decode(string json) {
	return JsonConvert.DeserializeObject<IDictionary<string, object>>(
			    json, new JsonConverter[] {new MyConverter()});
    }

    public class Authorization {
	public string secure_key;
	public string digestmod;
	HMACSHA256 hmac;

	public Authorization(string secure_key, string digestmod) {
	    this.secure_key = secure_key;
	    this.digestmod = digestmod;
	    hmac = new HMACSHA256(Encoding.UTF8.GetBytes(secure_key));
	}
	
	public string sign(List<string> list) {
            hmac.Initialize();
            foreach (string item in list) {
		byte [] sourcebytes = Encoding.UTF8.GetBytes(item);
                hmac.TransformBlock(sourcebytes, 0, sourcebytes.Length, null, 0);
            }
            hmac.TransformFinalBlock(new byte [0], 0, 0);
	    return BitConverter.ToString(hmac.Hash).Replace("-", "").ToLower();
	}
    }
    
    class MyConverter : CustomCreationConverter<IDictionary<string, object>> {
	public override IDictionary<string, object> Create(Type objectType) {
	    return new Dictionary<string, object>();
	}

	public override bool CanConvert(Type objectType) {
	    return objectType == typeof(object) || base.CanConvert(objectType);
	}

	public override object ReadJson(JsonReader reader, 
					Type objectType, 
					object existingValue, 
					JsonSerializer serializer) {
	    if ((reader.TokenType == JsonToken.StartObject) || 
		(reader.TokenType == JsonToken.Null)) {
		return base.ReadJson(reader, objectType, existingValue, serializer);
	    } else {
		return serializer.Deserialize(reader);
	    }
	}
    }

    public class Session {
	public string filename;
	public string session_id;
	public HeartBeatChannel hb_channel;
	public ShellChannel shell_channel;
	public IOPubChannel iopub_channel;
	public ControlChannel control_channel;
	public StdInChannel stdin_channel;
	public Authorization auth;
	public string engine_id;
	public Calico.MainWindow calico;
	public int current_execution_count = 0;
	public IDictionary<string, object> parent_header;
	public System.IO.StreamWriter log;

	public Session(Calico.MainWindow calico, string filename) {
	    this.calico = calico;
	    this.filename = filename;
	    //this.log = new System.IO.StreamWriter("zmqserver.log");
	    session_id = System.Guid.NewGuid().ToString();
	    engine_id = System.Guid.NewGuid().ToString();
	    string json = File.ReadAllText(filename);
	    var config = decode(json);
	    auth = new Authorization(config["key"].ToString(),
				     config["signature_scheme"].ToString());
	    hb_channel = new HeartBeatChannel(this, auth,
		    config["transport"].ToString(), 
		    config["ip"].ToString(),
		    config["hb_port"].ToString());
	    shell_channel = new ShellChannel(this, auth,
		    config["transport"].ToString(), 
		    config["ip"].ToString(),
		    config["shell_port"].ToString());
	    iopub_channel = new IOPubChannel(this, auth,
		    config["transport"].ToString(), 
		    config["ip"].ToString(),
		    config["iopub_port"].ToString());
	    control_channel = new ControlChannel(this, auth,
		    config["transport"].ToString(), 
		    config["ip"].ToString(),
		    config["control_port"].ToString());
	    stdin_channel = new StdInChannel(this, auth,
		    config["transport"].ToString(), 
		    config["ip"].ToString(),
		    config["stdin_port"].ToString());
	}

	public void start() {
	    hb_channel.thread.Start();
	    shell_channel.thread.Start();
	    control_channel.thread.Start();
	    stdin_channel.thread.Start();
	}

	public void SetOutputs(int execution_count, IDictionary<string, object> m_header) {
	    current_execution_count = execution_count;
	    parent_header = m_header;
	}

	public void StdErrWrite(string message) {
	    if (current_execution_count > 0) {
		var header = Header("stream");
		var metadata = new Dictionary<string, object>();
		var content = new Dictionary<string, object> {
		    {"data", message},
		    {"name", "stderr"}
		};
		iopub_channel.send(iopub_channel, header, parent_header, metadata, content);
	    } else {
		if (log != null) {
		    log.Write(message);
		    log.Flush();
		}
	    }
	}

	// Session:
	public void StdOutWrite(string message) {
	    if (current_execution_count > 0) {
		var header = Header("stream");
		var metadata = new Dictionary<string, object>();
		var content = new Dictionary<string, object> {
		    {"data", message},
		    {"name", "stdout"}
		};
		iopub_channel.send(iopub_channel, header, parent_header, metadata, content);
	    } else {
		if (log != null) {
		    log.Write(message);
		    log.Flush();
		}
	    }
	}

	public Dictionary<string, string> GetRepresentations(object obj) {
	    var data = new Dictionary<string, string>();
	    // Everything has a text fallback:
	    if (obj == null) {
		data["text/plain"] = "";
	    } else {
		data["text/plain"] = calico.Repr(obj);
		// Now, let's get additional ones, if they exist
		Type type = obj.GetType();
		System.Reflection.MethodInfo method = type.GetMethod("GetRepresentations");
		if (method != null) {
		    IDictionary<string, string> reprs = (IDictionary<string, string>) method.Invoke(obj, new object [] {});
		    foreach (KeyValuePair<string, string> kvp in (IDictionary<string, string>)reprs) {
			data[kvp.Key] = kvp.Value;
		    }
		}
	    }
	    return data;
	}

	public IDictionary<string, object> Header(string msg_type) {
	    Dictionary<string,object> dict = new Dictionary<string,object>();
	    dict["date"] = now();
	    dict["msg_id"] = msg_id();
	    dict["username"] = "kernel";
	    dict["session"] = session_id;
	    dict["msg_type"] = msg_type;
	    return dict;
	}

	public IDictionary<string, object> Header(string msg_type, string session_id) {
	    Dictionary<string,object> dict = new Dictionary<string,object>();
	    dict["date"] = now();
	    dict["msg_id"] = msg_id();
	    dict["username"] = "kernel";
	    dict["session"] = session_id;
	    dict["msg_type"] = msg_type;
	    return dict;
	}

	public void update_status(string status, IDictionary<string, object> parent_header) { 
	    // "busy", "idle"
	    var header = Header("status");
	    var metadata = new Dictionary<string, object>();
	    var content = new Dictionary<string, object>() {
		{"execution_state", status}
	    };
	    iopub_channel.send(iopub_channel, header, parent_header, metadata, content);
	}

	public void update_state(Widgets.Widget widget, IDictionary<string, object> parent_header) { 
	    var header = Header("comm_msg");
	    var metadata = new Dictionary<string, object>();
	    var content = widget.GetState();
	    iopub_channel.send(iopub_channel, header, parent_header, metadata, content);
	}

	public void clear_output(Widgets.Widget widget, bool wait) {
	    var header = Header("clear_output");
	    var metadata = new Dictionary<string, object>();
	    var content = widget.GetDisplay();
	    content["wait"] = wait;
	    iopub_channel.send(iopub_channel, header, new Dictionary<string, object>(), metadata, content);
	}

	public void display_widget(Widgets.Widget widget) {
	    widget.execution_count = session.current_execution_count;
	    var header = Header("comm_open");
	    var metadata = new Dictionary<string, object>();
	    var content = widget.GetInitialState();
	    iopub_channel.send(iopub_channel, header, parent_header, metadata, content);
	    update_state(widget, parent_header);
	    header = Header("comm_msg");
	    metadata = new Dictionary<string, object>();
	    content = widget.GetDisplay();
	    iopub_channel.send(iopub_channel, header, parent_header, metadata, content);
	}

	public void clear_output() {
	    clear_output(false);
	}

	public void clear_output(bool wait) {
	    var header = Header("clear_output");
	    var metadata = new Dictionary<string, object>();
	    var content = new Dictionary<string, object> {
		{"wait", wait},
	    };
	    iopub_channel.send(iopub_channel, header, parent_header, metadata, content);
	}

	public void display(object obj) {
	    if (obj is Widgets.Widget) {
		display_widget((Widgets.Widget)obj);
		return;
	    }
	    var header = Header("display_data");
	    var metadata = new Dictionary<string, object>();
	    var content = new Dictionary<string, object> {
		{"source", "display"},
		{"data", GetRepresentations(obj)}
	    };
	    iopub_channel.send(iopub_channel, header, parent_header, metadata, content);
	}
    }

    public class Channel {
	public Session session;
	public string transport;
	public string address;
	public ZmqContext context;
	public ZmqSocket socket;
	public string port;
	public Authorization auth;
	public Thread thread;

	public Channel(Session session, 
		       Authorization auth, 
		       string transport, 
		       string address, 
		       string port, 
		       SocketType socket_type) {
	    this.session = session;
	    this.auth = auth;
	    this.transport = transport;
	    this.address = address;
	    this.port = port;
	    context = ZmqContext.Create();
	    socket = context.CreateSocket(socket_type);
	    socket.Bind(
		String.Format("{0}://{1}:{2}", 
			      this.transport, this.address, this.port));
	    thread = new Thread (new ThreadStart (loop));
	}
		   
	public virtual void loop() {
	    while (true) {
		string message = socket.Receive(Encoding.UTF8);
		while (message != "<IDS|MSG>") {
		    message = socket.Receive(Encoding.UTF8);
		}
		string signature = socket.Receive(Encoding.UTF8);
		string s_header = socket.Receive(Encoding.UTF8);
		string s_parent_header = socket.Receive(Encoding.UTF8);
		string s_metadata = socket.Receive(Encoding.UTF8);
		string s_content = socket.Receive(Encoding.UTF8);
		
		string comp_sig = auth.sign(new List<string>() {
			s_header, s_parent_header, s_metadata, s_content});
		
		if (comp_sig != signature) {
		    throw new Exception("Error: signatures don't match!");
		}

		IDictionary<string, object> header = decode(s_header);
		IDictionary<string, object> parent_header = decode(s_parent_header);
		IDictionary<string, object> metadata = decode(s_metadata);
		IDictionary<string, object> content = decode(s_content);
		on_recv(signature, header, parent_header, metadata, content);
	    }
	}

	public virtual void on_recv(string m_signature, 
				    IDictionary<string, object> m_header, 
				    IDictionary<string, object> m_parent_header, 
				    IDictionary<string, object> m_metadata, 
				    IDictionary<string, object> m_content) {
	}

	public void send(Channel channel,
			 IDictionary<string,object> header,
			 IDictionary<string,object> parent_header,
			 IDictionary<string,object> metadata,
			 IDictionary<string,object> content) {
	    send(channel, encode(header), encode(parent_header), encode(metadata),
		 encode(content));
	}


	public void send(Channel channel,
			 string header,
			 string parent_header,
			 string metadata,
			 string content) {
	    string signature = auth.sign(new List<string>() {header, parent_header, 
							     metadata, content});
	    send_multipart(channel,
			   new List<string>() 
			   {"<IDS|MSG>",
				   signature,
				   header,
				   parent_header,
				   metadata,
				   content});
	}

	public void send_multipart(Channel channel, List<string> list) {
	    int count = 0;
	    foreach (string msg in list) {
		if (count < list.Count - 1) {
		    channel.socket.SendMore(msg, Encoding.UTF8);
		} else {
		    channel.socket.Send(msg, Encoding.UTF8);
		}
		count++;
	    }
	}
    }

    public class ShellChannel : Channel {
	public int execution_count = 1;

	public ShellChannel(Session session, 
			    Authorization auth, 
			    string transport, 
			    string address, 
			    string port) : 
	    base(session, auth, transport, address, port, SocketType.DEALER) {
	}

	public override void on_recv(string m_signature, 
				IDictionary<string, object> m_header, 
				IDictionary<string, object> m_parent_header, 
				IDictionary<string, object> m_metadata, 
				IDictionary<string, object> m_content) {

	    // Shell handler
	    if (m_header["msg_type"].ToString() == "execute_request") {
		var header = session.Header("status", m_header["session"].ToString());
		var metadata = new Dictionary<string, object>();
		var content = new Dictionary<string, object>
		{
		    {"execution_state", "busy"}
		};
		send(session.iopub_channel, header, m_header, metadata, content);
		// ---------------------------------------------------
		header = session.Header("pyin", m_header["session"].ToString());
		metadata = new Dictionary<string, object>();
		content = new Dictionary<string, object>
		{
		    {"execution_count", execution_count},
		    {"code", m_content["code"].ToString()},
		};
		send(session.iopub_channel, header, m_header, metadata, content);
		// ---------------------------------------------------
		// First, handle any Calico metacommands:
		string code = m_content["code"].ToString().Trim();
		if (code.StartsWith(":lang") && session.calico != null) { // :lang 
		    string [] lines = code.Split(new string[] { "\r\n", "\n" }, 
						 System.StringSplitOptions.None);
		    string language = lines[0].Substring(6).Trim().ToLower();
		    if (lines.Length > 1) {
			code = String.Join("\n", lines.Slice(1)).Trim();
		    } else {
			code = "";
		    }
		    string message = String.Format("Unknown language: \"{0}\"", language);
		    if (session.calico.manager.ContainsKey(language)) {
			session.calico.ActivateLanguage(language, session.calico.CurrentLanguage);
			message = String.Format("Calico Language is now \"{0}\"", session.calico.GetCurrentProperLanguage());
		    }
		    header = session.Header("pyout", m_header["session"].ToString());
		    content = new Dictionary<string, object>
			{
			    {"execution_count", execution_count},
			    {"data", ZMQServer.session.GetRepresentations(message)},
			    {"metadata", new Dictionary<string, object>()}
			};
		    send(session.iopub_channel, header, m_header, metadata, content);
		}
		if (code == "") {
		    header = session.Header("status", m_header["session"].ToString());
		    metadata = new Dictionary<string, object>();
		    content = new Dictionary<string, object>
			{
			    {"execution_state", "idle"}
			};
		    send(session.iopub_channel, header, m_header, metadata, content);
		    // ---------------------------------------------------
		    header = session.Header("execute_reply", m_header["session"].ToString());
		    metadata = new Dictionary<string, object>
			{
			    {"dependencies_met", true},
			    {"engine", session.engine_id},
			    {"status", "ok"},
			    {"started", now()}
			};
		    content = new Dictionary<string, object>
			{
			    {"status", "ok"},
			    {"execution_count", execution_count},
			    {"user_variables", new Dictionary<string, object>()},
			    {"payload", new List<object>()},
			    {"user_expressions", new Dictionary<string, object>()}
			};
		    send(session.shell_channel, header, m_header, metadata, content);
		    return;
		}
		// Execute in background, and report results
		ExecuteInBackground(code, m_header, execution_count);
		execution_count += 1;
	    } else if (m_header["msg_type"].ToString() == "kernel_info_request") {
		var header = session.Header("kernel_info_reply", m_header["session"].ToString());
		var metadata = new Dictionary<string, object>();
		var content = new Dictionary<string, object>
		    {
			{"protocol_version", new List<int>() {4, 0}},
			{"ipython_version", new List<object>() {1, 1, 0, ""}},
			{"language_version", new List<int>() {0, 0, 1}},
			{"language", "simple_kernel"},
		    };
		send(session.shell_channel, header, m_header, metadata, content);
	    } else if (m_header["msg_type"].ToString() == "history_request") {
		var header = session.Header("kernel_info_reply", m_header["session"].ToString());
		var content = new Dictionary<string, object>
		    {
			{"output", false}
		    };
		send(session.shell_channel, header, m_header, m_metadata, content);
	    } else if (m_header["msg_type"].ToString() == "object_info_request") {
		// FIXME: add object_info_request, shutdown_request, complete_request
	    } else if (m_header["msg_type"].ToString() == "comm_msg") {
		Widgets.Dispatch(m_content["comm_id"].ToString(),
				 (IDictionary<string, object>)m_content["data"],
				 m_header);
	    } else {
		throw new Exception("unknown msg_type: " + m_header["msg_type"]);
	    }
	}

	public void ExecuteInBackground(string code, IDictionary<string, object> m_header, int execution_count) {
	    var header = session.Header("pyout", m_header["session"].ToString());
	    var metadata = new Dictionary<string, object>();
	    object retval = null;
	    session.SetOutputs(execution_count, m_header);
	    if (session.calico != null) {
		try {
		    retval = session.calico.Evaluate(code);
		} catch (Exception e) {
		    retval = e;
		}
	    }
	    Dictionary<string, object> content = null;
	    if (retval != null) {
		if (retval is Widgets.Widget) {
		    // Widgets inject themselves to output, but have no return repr
		    session.display_widget((Widgets.Widget)retval);
		} else {
		    content = new Dictionary<string, object>
			{
			    {"execution_count", execution_count},
			    {"data", ZMQServer.session.GetRepresentations(retval)},
			    {"metadata", new Dictionary<string, object>()}
			};
		    send(session.iopub_channel, header, m_header, metadata, content);
		}
	    }
	    session.SetOutputs(0, null); // wait till after widget displays
	    // ---------------------------------------------------
	    header = session.Header("status", m_header["session"].ToString());
	    metadata = new Dictionary<string, object>();
	    content = new Dictionary<string, object>
		{
		    {"execution_state", "idle"}
		};
	    send(session.iopub_channel, header, m_header, metadata, content);
	    // ---------------------------------------------------
	    header = session.Header("execute_reply", m_header["session"].ToString());
	    metadata = new Dictionary<string, object>
		{
		    {"dependencies_met", true},
		    {"engine", session.engine_id},
		    {"status", "ok"},
		    {"started", now()}
		};
	    content = new Dictionary<string, object>
		{
		    {"status", "ok"},
		    {"execution_count", execution_count},
		    {"user_variables", new Dictionary<string, object>()},
		    {"payload", new List<object>()},
		    {"user_expressions", new Dictionary<string, object>()}
		};
	    send(session.shell_channel, header, m_header, metadata, content);
	}
    }

    public class IOPubChannel : Channel {
	public IOPubChannel(Session session, 
			    Authorization auth, 
			    string transport, 
			    string address, 
			    string port) : 
	    base(session, auth, transport, address, port, SocketType.PUB) {
	}
    }

    public class ControlChannel : Channel {
	public ControlChannel(Session session, 
			    Authorization auth, 
			    string transport, 
			    string address, 
			    string port) : 
	    base(session, auth, transport, address, port, SocketType.DEALER) {
	}
    }

    public class StdInChannel : Channel {
	public StdInChannel(Session session, 
			    Authorization auth, 
			    string transport, 
			    string address, 
			    string port) : 
	    base(session, auth, transport, address, port, SocketType.DEALER) {
	}
    }

    public class HeartBeatChannel : Channel {
	public HeartBeatChannel(Session session, 
			    Authorization auth, 
			    string transport, 
			    string address, 
			    string port) : 
	    base(session, auth, transport, address, port, SocketType.REP) {
	}

	public override void loop() {
	    while (true) {
		string message = socket.Receive(Encoding.UTF8);
		socket.Send(message, Encoding.UTF8);
	    }
	}
    }

    public static Session session;

    // ZMQServer:
    public static void StdErrWrite(string message) {
	if (session != null) {
	    session.StdErrWrite(message);
	} else {
	    //System.Console.Error.Write(message);
	}
    }

    // ZMQServer:
    public static void StdOutWrite(string message) {
	if (session != null) {
	    session.StdOutWrite(message);
	} else {
	    //System.Console.Write(message);
	}
    }

    /*
    public static void Main(string [] args) {
	session = new Session(null, args[0]);
	session.start();
	while (true) {
	    Thread.Sleep ((int)(1 * 1000)); // seconds
	}
    }
    */

    public static void Start(Calico.MainWindow calico, string config_file) {
	session = new Session(calico, config_file);
	session.start();
	while (true) {
	    Thread.Sleep ((int)(1 * 1000)); // seconds
	}
    }
}
