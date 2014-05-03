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

namespace Calico {

    public class MagicBase {
	public string command = "magic";         // magic text, eg %%file
	public string mtype = "";                // "cell", "line", or "notebook"
	public bool evaluate = true;             // should we evaluate code next?
	public string code = null;               // code to return after init
	public ZMQServer.Session session = null; // current session

	public string __doc__() {
	    return String.Format("This is some helpful documentation on \"{0}\"", 
				 command);
	}

	public MagicBase() {
	}
	
	public MagicBase(ZMQServer.Session session, string code, 
		     string mtype, string args) {
	    // The type of magic we are doing
	    this.session = session;
	    this.code = code;
	    this.mtype = mtype;
	    // --------------------------------
	    if (mtype == "line") {
		line(args);
	    } else if (mtype == "cell") {
		cell(args);
	    } else if (mtype == "notebook") {
		notebook(args);
	    }
	}

	public virtual void line(string text) {
	}
	
	public virtual void cell(string args) {
	}

	public virtual void notebook(string args) {
	}

	public virtual object post_process(object result) {
	    // possiblility to post process the result of
	    // computation, decorator style
	    return result;
	}

	public virtual string get_code() {
	    // return rest of code to process after 
	    // construction
	    return code;
	}

	public virtual List<string> Split(string args) {
	    args = args.Trim();
	    List<string> retval = new List<string>();
	    string current = "";
	    string state = null;
	    for (int i=0; i < args.Length; i++) {
		string c = args.Substring(i, 1);
		if (c == "\\") {
		    i++;
		    c = args.Substring(i, 1);
		    current += c;
		} else if (state == null) {
		    if (c == "\"") {
			state = "double-quote";
			if (current != "") {
			    retval.Add(current);
			}
			current = "";
		    } else if (c == " ") { // delimiter
			if (current != "") {
			    retval.Add(current);
			}
			current = "";
		    } else {
			current += c;
		    }
		} else if (state == "double-quote") {
		    if (c == "\"") {
			state = null;
			retval.Add(current);
			current = "";
		    } else {
			current += c;
		    }
		}
	    }
	    if (current != "") {
		retval.Add(current);
	    }
	    return retval;
	}
    }
}

public static class ZMQServer {

    public static Dictionary<string,object> pack(params object [] args) {
	var retval = new Dictionary<string,object>();
	for (int i=0; i < args.Length; i += 2) {
	    retval[args[i].ToString()] = args[i + 1];
	}
	return retval;
    }

    public static List<string> list(params object [] args) {
	var retval = new List<string>();
	foreach(object arg in args) {
	    retval.Add(arg.ToString());
	}
	return retval;
    }

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
	internal bool blocking = true;
	public bool need_restart = false;
	public bool request_quit = false;
	public string filename;
	public string session_id;
	public HeartBeatChannel hb_channel;
	public ShellChannel shell_channel;
	public IOPubChannel iopub_channel;
	public ControlChannel control_channel;
	public StdInChannel stdin_channel;
	public Authorization auth;
	public string engine_identity;
	public int engine_identity_int = -1;
	public Calico.MainWindow calico;
	public int current_execution_count = 0;
	public IDictionary<string, object> parent_header;
	public System.IO.StreamWriter log;
	public Assembly magic_assembly;
	public IDictionary<string,object> config;
	public Dictionary<int,string> In = new Dictionary<int,string>();
	public Dictionary<int,object> Out = new Dictionary<int,object>();
	public object __ = null;
	public object ___ = null;
	public object _ii = null;
	public object _iii = null;
	public bool starting = true;

	public Session(Calico.MainWindow calico, string filename) {
	    this.calico = calico;
	    this.filename = filename;
	    //this.log = new System.IO.StreamWriter("zmqserver.log");
	    session_id = System.Guid.NewGuid().ToString();
	    engine_identity = System.Guid.NewGuid().ToString();
	    string json;
	    if (this.filename != "") {
		json = File.ReadAllText(this.filename);
		config = decode(json);
	    } else {
		config = pack("key", System.Guid.NewGuid().ToString(),
			      "signature_scheme", "hmac-sha256",
			      "transport", "tcp",
			      "ip", "127.0.0.1",
			      "hb_port", "0",
			      "shell_port", "0",
			      "iopub_port", "0",
			      "control_port", "0",
			      "stdin_port", "0");
	    }
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

	    // FIXME: load all files in this directory, and user local directory:
	    magic_assembly = Assembly.LoadFrom(Path.Combine(calico.path, "..", "magics", "Magics.dll"));

	    calico.SetVariable("In", In);
	    calico.SetVariable("Out", Out);
	    if (this.filename == "") {
		config["hb_port"] = hb_channel.port;
		config["shell_port"] = shell_channel.port;
		config["iopub_port"] = iopub_channel.port;
		config["control_port"] = control_channel.port;
		config["stdin_port"]  = stdin_channel.port;
		string kernelname = String.Format("kernel-{0}.json", 
			      System.Diagnostics.Process.GetCurrentProcess().Id);
		string ipython_config = ("{{\n" +
					 "  \"hb_port\": {0},\n" +
					 "  \"shell_port\": {1},\n" +
					 "  \"iopub_port\": {2},\n" +
					 "  \"control_port\": {3},\n" +
					 "  \"stdin_port\": {4},\n" +
					 "  \"ip\": \"{5}\",\n" +
					 "  \"signature_scheme\": \"{6}\",\n" +
					 "  \"key\": \"{7}\",\n" +
					 "  \"transport\": \"{8}\"\n" +
					 "}}");
		ipython_config = String.Format(ipython_config, 
					       config["hb_port"],
					       config["shell_port"],
					       config["iopub_port"],
					       config["control_port"],
					       config["stdin_port"],
					       config["ip"],
					       config["signature_scheme"],
					       config["key"],
					       config["transport"]);
		this.filename = System.IO.Path.Combine(((string)calico.config.GetValue("ipython", "security")), kernelname);
		System.IO.StreamWriter sw = new System.IO.StreamWriter(this.filename);
		sw.Write(ipython_config);
		sw.Close();
		calico.stdout.WriteLine("IPython config file written to:");
		calico.stdout.WriteLine("   \"{0}\"", this.filename);
		calico.stdout.WriteLine("To exit, you will have to explicitly quit this process, by either sending");
		calico.stdout.WriteLine("\"quit\" from a client, or using Ctrl-\\ in UNIX-like environments.");
		calico.stdout.WriteLine("To read more about this, see https://github.com/ipython/ipython/issues/2049");
		calico.stdout.WriteLine("To connect another client to this kernel, use:");
 		calico.stdout.WriteLine("    --existing {0} --profile calico", kernelname);
	    } 
	}

	public List<string> route(string topic) {
	    string routing_info;
	    if (engine_identity_int >= 0) {
		routing_info = String.Format("engine.{0}.{1}", engine_identity_int, topic);
	    } else {
		routing_info = String.Format("kernel.{0}.{1}", engine_identity, topic);
	    }
	    return list(routing_info); 
	}

	public void SetBlocking(bool blocking) {
	    this.blocking = blocking;
	}

	public bool GetBlocking() {
	    return blocking;
	}

	public string TitleCase(string text) {
	    return char.ToUpper(text[0]) + text.Substring(1);
	}

	public MagicBase GetMagic(string text) {
	    string [] lines = text.Split(new string[] { "\r\n", "\n" },
		System.StringSplitOptions.None);
	    string code = "";
	    string args = "";
	    string name;
	    string mtype;
	    if (lines.Length > 1) {
		for (int i = 1; i < lines.Length; i++) {
		    code += lines[i] + "\n";
		}
	    }
	    string [] command = lines[0].Split(new char [] {' '}, 2, 
					       System.StringSplitOptions.None);
	    if (command.Length > 1) {
		args = command[1].Trim();
	    } 
	    if (command[0].StartsWith("%%%")) {
		// Notebook command
		name = "Calico." + TitleCase(command[0].Substring(3));
		mtype = "notebook";
	    } else if (command[0].StartsWith("%%")) {
		// Cell
		name = "Calico." + TitleCase(command[0].Substring(2));
		mtype = "cell";
	    } else if (command[0].StartsWith("%")) {
		// Line
		name = "Calico." + TitleCase(command[0].Substring(1));
		mtype = "line";
	    } else {
		throw new Exception("invalid magic");
	    }
	    //Console.WriteLine("Checking for magic... type: '{0}', args: '{1}'", mtype, args);
	    if (magic_assembly != null) {
		//Console.WriteLine("Checking for name... name: '{0}'", name);
		Type type = magic_assembly.GetType(name);
		if (type != null) {
		    ConstructorInfo constructor = type.GetConstructor(
		        new[] {typeof(Session), typeof(string), typeof(string), typeof(string) });
		    if (constructor != null) {
			MagicBase retval = (MagicBase)constructor.Invoke(new object [] {this, code, mtype, args});
			//Console.WriteLine("Checking for magic: '{0}'", retval);
			return retval;
		    }
		}
	    }
	    return null;
	}

	public void start() {
	    hb_channel.thread.Start();
	    shell_channel.thread.Start();
	    control_channel.thread.Start();
	    stdin_channel.thread.Start();
	}

	public void stop() {
	    try {
		hb_channel.stop();
		shell_channel.stop();
		control_channel.stop();
		stdin_channel.stop();
	    } catch {
		// ignore errors, shutting down
	    }
	}

	public void SetOutputs(int execution_count, IDictionary<string, object> m_header) {
	    current_execution_count = execution_count;
	    parent_header = m_header;
	}

	public void send(Channel channel,
			 IList<string> identities,
			 IDictionary<string,object> header,
			 IDictionary<string,object> parent_header,
			 IDictionary<string,object> metadata,
			 IDictionary<string,object> content) {
	    send(channel, identities, encode(header), encode(parent_header), encode(metadata),
		 encode(content));
	}

	public void send(Channel channel,
			 IList<string> identities,
			 string header,
			 string parent_header,
			 string metadata,
			 string content) {
	    string signature = auth.sign(new List<string>() {header, parent_header, 
							     metadata, content});
	    send_multipart(channel, identities, 
			   list("<IDS|MSG>",
				signature,
				header,
				parent_header,
				metadata,
				content));
	}
	
	public void send_multipart(Channel channel, 
				   IList<string> identities, 
				   IList<string> parts) {
	    int count = 0;
	    foreach (string msg in identities) {
		channel.socket.SendMore(msg, Encoding.UTF8);
	    }
	    foreach (string msg in parts) {
		if (count < parts.Count - 1) {
		    channel.socket.SendMore(msg, Encoding.UTF8);
		} else {
		    channel.socket.Send(msg, Encoding.UTF8);
		}
		count++;
	    }
	}

	public void StdErrWrite(string message) {
	    if (current_execution_count > 0) {
		var header = Header("stream");
		var metadata = pack();
		var content = pack("data", message,
				   "name", "stderr");
		send(iopub_channel, route("pyerr"), header, parent_header, metadata, content);
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
		var metadata = pack();
		var content = pack("data", message,
				   "name", "stdout");
		send(iopub_channel, route("pyout"), header, parent_header, metadata, content);
	    } else {
		if (log != null) {
		    log.Write(message);
		    log.Flush();
		}
	    }
	}

	public Dictionary<string, string> GetRepresentations(object obj) {
	    return GetRepresentations(obj, null);
	}

	public Dictionary<string, string> GetRepresentations(object obj, string mime_type) {
	    var data = new Dictionary<string,string>();
	    // Everything has a text fallback:
	    if (obj == null) {
		data["text/plain"] = "";
	    } else {
		data["text/plain"] = calico.Repr(obj);
		// Now, let's get additional ones, if they exist
		Type type = obj.GetType();
		System.Reflection.MethodInfo method = type.GetMethod("GetRepresentations");
		if (method != null) {
		    // FIXME: don't make extra reprs if not needed (pass in mime_type to filter):
		    IDictionary<string, string> reprs = (IDictionary<string, string>) method.Invoke(obj, new object [] {});
		    if (mime_type != null) {
			try {
			    data[mime_type] = reprs[mime_type];
			} catch {
			    data["plain/text"] = String.Format("Error: no such mime_type: '{0}'", mime_type);
			}
		    } else {
			foreach (KeyValuePair<string, string> kvp in (IDictionary<string, string>)reprs) {
			    data[kvp.Key] = kvp.Value;
			}
		    }
		} else if (obj is System.Drawing.Bitmap) {
		    var reprs = new Graphics.Picture((System.Drawing.Bitmap)obj).GetRepresentations();
		    foreach (KeyValuePair<string, string> kvp in (IDictionary<string, string>)reprs) {
			data[kvp.Key] = kvp.Value;
		    }
		}
	    }
	    return data;
	}

	public IDictionary<string, object> Header(string msg_type) {
	    Dictionary<string,object> dict = pack();
	    dict["date"] = now();
	    dict["msg_id"] = msg_id();
	    dict["username"] = "kernel";
	    dict["session"] = session_id;
	    dict["msg_type"] = msg_type;
	    return dict;
	}

	public IDictionary<string, object> Header(string msg_type, string session_id) {
	    Dictionary<string,object> dict = pack();
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
	    var metadata = pack();
	    var content = pack("execution_state", status);
	    send(iopub_channel, route("status"), header, parent_header, metadata, content);
	}

	public void update_state(Widgets.Widget widget, IDictionary<string, object> parent_header) { 
	    var header = Header("comm_msg");
	    var metadata = pack();
	    var content = widget.GetState();
	    send(iopub_channel, route("comm_msg"), header, parent_header, metadata, content);
	}

	public void clear_output(Widgets.Widget widget, bool wait) {
	    var header = Header("clear_output");
	    var metadata = pack();
	    var content = widget.GetDisplay();
	    content["wait"] = wait;
	    send(iopub_channel, route("pyout"), header, pack(), metadata, content);
	}

	public void added_child_widget(Widgets.Widget widget) {
	    IDictionary<string,object> header;
	    IDictionary<string,object> metadata;
	    IDictionary<string,object> content;
	    widget.execution_count = session.current_execution_count;
	    header = Header("comm_open");
	    metadata = pack();
	    content = widget.GetInitialState();
	    send(iopub_channel, route("comm_open"), header, parent_header, metadata, content);
	    update_state(widget, parent_header);
	}

	public void display_widget(Widgets.Widget widget) {
	    IDictionary<string,object> header;
	    IDictionary<string,object> metadata;
	    IDictionary<string,object> content;
	    widget.execution_count = session.current_execution_count;
	    header = Header("comm_open");
	    metadata = pack();
	    content = widget.GetInitialState();
	    send(iopub_channel, route("comm_open"), header, parent_header, metadata, content);
	    update_state(widget, parent_header);
	    header = Header("comm_msg");
	    metadata = pack();
	    content = widget.GetDisplay();
	    send(iopub_channel, route("comm_open"), header, parent_header, metadata, content);
	}

	public void clear_output() {
	    clear_output(false);
	}

	public void clear_output(bool wait) {
	    var header = Header("clear_output");
	    var metadata = pack();
	    var content = pack("wait", wait);
	    send(iopub_channel, route("pyout"), header, parent_header, metadata, content);
	}

	public string input(string prompt="") {
	    var header = Header("input_request");
	    var metadata = pack();
	    var content = pack("prompt", prompt);
	    stdin_channel.SetState("waiting", ""); // wait for recv "input_reply"
	    send(stdin_channel, list(parent_header["session"]), header, parent_header, metadata, content);
	    while (stdin_channel.GetState() == "waiting") {
		Thread.Sleep(100); // miliseconds
	    }
	    return stdin_channel.input_reply;
	}

	public void display(object obj) {
	    if (obj is Widgets.Widget) {
		display_widget((Widgets.Widget)obj);
		return;
	    }
	    var header = Header("display_data");
	    var metadata = pack();
	    var content = pack("source", "display",
			       "data", GetRepresentations(obj));
	    send(iopub_channel, route("pyout"), header, parent_header, metadata, content);
	}

	public void display_mimetype(object obj, string mimetype) {
	    if (obj is Widgets.Widget) {
		display_widget((Widgets.Widget)obj);
		return;
	    }
	    var header = Header("display_data");
	    var metadata = pack();
	    var content = pack("source", "display",
			       "data", GetRepresentations(obj, mimetype));
	    send(iopub_channel, route("pyout"), header, parent_header, metadata, content);
	}

	public void display_png(object obj) {
	    display_mimetype(obj, "image/png");
	}
	public void display_jpeg(object obj) {
	    display_mimetype(obj, "image/jpeg");
	}
	public void display_html(object obj) {
	    display_mimetype(obj, "text/html");
	}
	public void display_javascript(object obj) {
	    display_mimetype(obj, "application/javascript");
	}
	public void display_json(object obj) {
	    display_mimetype(obj, "application/json");
	}
	public void display_latex(object obj) {
	    display_mimetype(obj, "text/latex");
	}
	public void display_pdf(object obj) {
	    display_mimetype(obj, "application/pdf");
	}
	public void display_svg(object obj) {
	    display_mimetype(obj, "image/svg+xml");
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
	    if (port == "0") {
		Random rand = new Random();
		int min_port = 49152; 
		int max_port = 65536;
		int max_tries = 100;
		int p = 0;
		int i;
		for (i = 0; i < max_tries; i++) {
		    p = rand.Next(min_port, max_port);
		    string addr = String.Format("{0}://{1}:{2}", 
						this.transport, this.address, p);
		    try {
			socket.Bind(addr);
		    } catch {
			continue;
		    }
		    break;
		}
		if (i == 100) {
		    throw new Exception("Exhausted tries looking for random port");
		}
		this.port = "" + p;
	    } else {
		socket.Bind(String.Format("{0}://{1}:{2}", 
			       this.transport, this.address, this.port));
	    }
	    socket.Identity = Encoding.UTF8.GetBytes(session.session_id);
	    thread = new Thread (new ThreadStart (loop));
	}
		   
	public virtual void loop() {
	    string message, signature, s_header, s_parent_header, s_metadata, s_content;
	    var identities = new List<string>();
	    while (! session.request_quit) {
		try {
		    message = socket.Receive(Encoding.UTF8);
		    while (message != "<IDS|MSG>") {
			identities.Add(message);
			message = socket.Receive(Encoding.UTF8);
		    }
		    signature = socket.Receive(Encoding.UTF8);
		    s_header = socket.Receive(Encoding.UTF8);
		    s_parent_header = socket.Receive(Encoding.UTF8);
		    s_metadata = socket.Receive(Encoding.UTF8);
		    s_content = socket.Receive(Encoding.UTF8);
		} catch {
		    continue;
		}
		string comp_sig = auth.sign(new List<string>() {
			s_header, s_parent_header, s_metadata, s_content});
		
		if (comp_sig != signature) {
		    throw new Exception("Error: signatures don't match!");
		}

		IDictionary<string, object> header = decode(s_header);
		IDictionary<string, object> parent_header = decode(s_parent_header);
		IDictionary<string, object> metadata = decode(s_metadata);
		IDictionary<string, object> content = decode(s_content);
		on_recv(identities, signature, header, parent_header, metadata, content);
	    }
	}

	public virtual void on_recv(List<string> identities, 
				    string m_signature, 
				    IDictionary<string, object> m_header, 
				    IDictionary<string, object> m_parent_header, 
				    IDictionary<string, object> m_metadata, 
				    IDictionary<string, object> m_content) {
	    throw new Exception(this.ToString() + ": unknown msg_type: " + m_header["msg_type"]);
	}

	public void stop() {
	    thread.Abort();
	    socket.Linger = System.TimeSpan.FromSeconds(1);
	    socket.Close();
	    //context.Terminate();
	}
    }

    public class ShellChannel : Channel {
	public int execution_count = 1;
	public static string USAGE = (
"\n" +
"ICalico -- An enhanced Interactive Scripting Shell for Calico\n"+
"=============================================================\n"+
"\n"+
"ICalico offers a combination of convenient shell features, special commands\n"+
"and a history mechanism for both input (command history) and output (results\n"+
"caching, similar to Mathematica).\n" +
"\n"+
"MAIN FEATURES\n"+
"-------------\n"+
"\n"+
"* Magic commands: type %magic for information on the magic subsystem.\n"+
"\n"+
"* Dynamic object information:\n"+
"\n"+
"  Typing ?word or word? prints detailed information about an object.\n"+
"\n"+
"* Completion in the local namespace, by typing TAB at the prompt.\n"+
"\n"+
"  At any time, hitting tab will complete any available commands or\n"+
"  variable names, and show you a list of the possible completions if there's\n"+
"  no unambiguous one. It will also complete filenames in the current directory.\n"+
"\n"+
"  - %hist: search history by index\n"+
"\n"+
"* Persistent command history across sessions.\n"+
"\n"+
"* Logging of input with the ability to save and restore a working session.\n"+
"\n"+
"* Input caching system:\n"+
"\n"+
"  ICalico offers numbered prompts (In/Out) with input and output caching. All\n"+
"  input is saved and can be retrieved as variables (besides the usual arrow\n"+
"  key recall).\n"+
"\n"+
"  The following GLOBAL variables always exist (so don't overwrite them!):\n"+
"  _i: stores previous input.\n"+
"  _ii: next previous.\n"+
"  _iii: next-next previous.\n"+
"  _ih : a list of all input _ih[n] is the input from line n.\n"+
"\n"+
"  Additionally, global variables named _i<n> are dynamically created (<n>\n"+
"  being the prompt counter), such that _i<n> == _ih[<n>]\n"+
"\n"+
"  For example, what you typed at prompt 14 is available as _i14 and _ih[14].\n"+
"\n"+
"  You can create macros which contain multiple input lines from this history,\n"+
"  for later re-execution, with the %macro function.\n"+
"\n"+
"  The history function %hist allows you to see any part of your input history\n"+
"  by printing a range of the _i variables. Note that inputs which contain\n"+
"  magic functions (%) appear in the history with a prepended comment. This is\n"+
"  because they aren't really valid code, so you can't execute them.\n"+
"\n"+
"* Output caching system:\n"+
"\n"+
"  For output that is returned from actions, a system similar to the input\n"+
"  cache exists but using _ instead of _i. Only actions that produce a result\n"+
"  (NOT assignments, for example) are cached. If you are familiar with\n"+
"  Mathematica, ICalico's _ variables behave exactly like Mathematica's %\n"+
"  variables.\n"+
"\n"+
"  The following GLOBAL variables always exist (so don't overwrite them!):\n"+
"  _ (one underscore): previous output.\n"+
"  __ (two underscores): next previous.\n"+
"  ___ (three underscores): next-next previous.\n"+
"\n"+
"  Global variables named _<n> are dynamically created (<n> being the prompt\n"+
"  counter), such that the result of output <n> is always available as _<n>.\n"+
"\n"+
"  Finally, a global dictionary named _oh exists with entries for all lines\n"+
"  which generated output.\n");

	public ShellChannel(Session session, 
			    Authorization auth, 
			    string transport, 
			    string address, 
			    string port) : 
	    base(session, auth, transport, address, port, SocketType.ROUTER) {
	}

	// Shell
	public override void on_recv(List<string> identities, 
				     string m_signature, 
				     IDictionary<string, object> m_header, 
				     IDictionary<string, object> m_parent_header, 
				     IDictionary<string, object> m_metadata, 
				     IDictionary<string, object> m_content) {

	    // Shell handler
	    string msg_type = m_header["msg_type"].ToString();
	    if (msg_type == "execute_request") {
		var header = session.Header("status", m_header["session"].ToString());
		var metadata = pack();
		var content = pack("execution_state", "busy");
		session.send(session.iopub_channel, session.route("status"), header, m_header, metadata, content);
		// ---------------------------------------------------
		header = session.Header("pyin", m_header["session"].ToString());
		metadata = pack();
		content = pack("execution_count", execution_count,
			       "code", m_content["code"].ToString());
		session.send(session.iopub_channel, session.route("pyin"), header, m_header, metadata, content);
		// ---------------------------------------------------
		string code = m_content["code"].ToString().Trim();
		// Execute in background, and report results
		ExecuteInBackground(code, identities, m_header, execution_count);
		execution_count += 1;
	    } else if (msg_type == "kernel_info_request") {
		if (session.starting) {
		    session.send(session.iopub_channel, session.route("status"), session.Header("status", session.session_id), 
			 pack(), pack(), pack("execution_state", "starting"));
		    session.starting = false;
		}
		var header = session.Header("kernel_info_reply", m_header["session"].ToString());
		var metadata = pack();
		var content = pack("protocol_version", new List<int>() {4, 1},
				   "ipython_version", new List<object>() {2, 0, 0, ""},
				   "language_version", new List<int>() {3, 0, 2},
				   "language", "calico");
		session.send(session.shell_channel, list(m_header["session"]), header, m_header, metadata, content); // ok
	    } else if (msg_type == "history_request") {
		// FIXME: handle history_request
		var header = session.Header("history_reply", m_header["session"].ToString());
		var content = pack("output", false);
		session.send(session.shell_channel, list(m_header["session"]), header, m_header, m_metadata, content);
	    } else if (msg_type == "object_info_request") {
		// for filling in details on function calls: x(<pause>
		// content: {"detail_level":0,"oname":"x"}
		string oname = m_content["oname"].ToString();
		// ask language to give help on oname
		// return:
		var header = session.Header("object_info_reply");
		var meta = pack();
		var content = session.calico.GetHelpOnFunctionCall(oname);
		session.send(session.shell_channel, list(m_header["session"]), header, m_header, meta, content);
	    } else if (msg_type == "complete_request") {
		// content: {"text":"","line":"x.he","block":null,"cursor_pos":4}']
		string to_match = m_content["line"].ToString();
		// ask language to complete to_match
		var tc = session.calico.GetTabCompletion(to_match);
		// return:
		var header = session.Header("complete_reply");
		var meta = pack();
		var content = pack("matches", tc.getItems(),
				   "status", "ok",
				   "matched_text", tc.full_prefix);
		session.send(session.shell_channel, list(m_header["session"]), header, m_header, meta, content);
	    } else if (msg_type == "comm_msg") {
		Widgets.Dispatch(m_content["comm_id"].ToString(),
				 (IDictionary<string, object>)m_content["data"],
				 m_header);
	    } else if (msg_type == "shutdown_request") {
		// respond?
		//var header = session.Header("status", m_header["session"].ToString());
		//var metadata = pack();
		//var content = pack();
		// pause, then:
		if (Convert.ToBoolean(m_content["restart"])) { 
		    session.need_restart = true;
		} else {
		    session.request_quit = true;
		}
	    } else {
		throw new Exception("ShellChannel: unknown msg_type: " + msg_type);
	    }
	}

	public void ExecuteInBackground(string code, IList<string> identities, IDictionary<string, object> m_header, int execution_count) {
	    var header = session.Header("pyout", m_header["session"].ToString());
	    var metadata = pack();
	    object retval = null;
	    session.SetOutputs(execution_count, m_header);
	    if (session.calico != null) {
		session.calico.executeThread = new System.Threading.Thread(new System.Threading.ThreadStart(delegate {
			    MagicBase magic = null;
			    List<MagicBase> stack = new List<MagicBase>();
			    List<object> payload = new List<object>();
			    // --------------------------------------
			    // Handle query:
			    if (code.Trim() == "?") {
				Dictionary<string,object> message = pack();
				message["text"] = USAGE;
				message["html"] = null;
				message["start_line_number"] = 0;
				message["source"] = "page";
				payload.Add(message);
			    } else if (code.StartsWith("?") || code.EndsWith("?")) {
				if (code.StartsWith("?"))
				    code = code.Substring(1);
				else
				    code = code.Substring(0, code.Length - 1);
				// give help on command
				Dictionary<string,object> message = pack();
				message["text"] = session.calico.GetHelpOn(code);
				message["html"] = null;
				message["start_line_number"] = 0;
				message["source"] = "page";
				payload.Add(message);
			    } else { // Handle code and magics:
				// --------------------------------------
				// Handle magics:
				while (code.StartsWith("%")) { //----------------- Magics
				    magic = session.GetMagic(code);
				    if (magic != null) {
					stack.Add(magic);
					code = magic.get_code();
					if (! magic.evaluate) // signal to exit, maybe error or no block
					    break;
				    } else {
					break;
				    }
				} 
				//---------------- Code
				if ((magic == null || ((magic != null) && magic.evaluate)) && code.Trim() != "") {
				    try {
					retval = session.calico.Evaluate(code);
				    } catch (Exception e) {
					retval = e;
				    }
				}
				stack.Reverse();
				foreach (MagicBase m in stack) {
				    retval = m.post_process(retval);
				}
			    }
			    // --------------------------------------
			    // Handle in's
			    Dictionary<string, object> content = null;
			    session.In[execution_count] = code;
			    session.calico.SetVariable("_iii", session._iii);
			    session.calico.SetVariable("_ii", session._ii);
			    session.calico.SetVariable("_i", code);
			    session.calico.SetVariable("_i" + execution_count, code);
			    session._iii = session._ii;
			    session._ii = code;
			    if (retval != null) {
				// --------------------------------------
				// Handle out's (only when non-null)
				session.Out[execution_count] = retval;
				session.calico.SetVariable("___", session.___);
				session.calico.SetVariable("__", session.__);
				session.calico.SetVariable("_", retval);
				session.calico.SetVariable("_" + execution_count, retval);
				session.___ = session.__;
				session.__ = retval;
				if (retval is Widgets.Widget) {
				    // Widgets inject themselves to output, but have no return repr
				    session.display_widget((Widgets.Widget)retval);
				    //} else if (retval is Representation) {
				    //session.display((Representation)retval);
				} else {
				    content = pack("execution_count", execution_count,
						   "data", ZMQServer.session.GetRepresentations(retval),
						   "metadata", pack());
				    session.send(session.iopub_channel, session.route("pyout"), header, m_header, metadata, content);
				}
			    }
			    // ---------------------------------------------------
			    header = session.Header("status", m_header["session"].ToString());
			    metadata = pack();
			    content = pack("execution_state", "idle");
			    session.send(session.iopub_channel, session.route("status"), header, m_header, metadata, content);
			    // ---------------------------------------------------
			    // FIXME: also send the other kind of execute_reply:
			    // identity: 'execute_reply', ... [m_header["session"], ...]
			    header = session.Header("execute_reply", m_header["session"].ToString());
			    content = pack("status", "ok", 
					   "execution_count", execution_count,
					   "user_variables", pack(),
					   "payload", payload,
					   "user_expressions", pack());
			    metadata = pack("dependencies_met", true,
					    "engine", session.engine_identity,
					    "status", "ok",
					    "started", now());
			    session.send(session.shell_channel, list(m_header["session"]), header, m_header, metadata, content); // ok
			    //session.SetOutputs(0, null); // wait till after widget displays
			}));
		session.calico.executeThread.IsBackground = true;
		session.calico.executeThread.Start();
		// wait here to finish for non-blocking behavior
		// control+c at this point will kill just the thread
		if (session.blocking)
		    session.calico.executeThread.Join();
		// TODO: for a non-blocking kernel... what else needs
		// to change?
	    }
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
	    base(session, auth, transport, address, port, SocketType.ROUTER) {
	}

	// Control
	public override void on_recv(List<string> identities, 
				     string m_signature, 
				     IDictionary<string, object> m_header, 
				     IDictionary<string, object> m_parent_header, 
				     IDictionary<string, object> m_metadata, 
				     IDictionary<string, object> m_content) {
	    // Control handler
	    string msg_type = m_header["msg_type"].ToString();
	    if (msg_type == "shutdown_request") {
		session.request_quit = true;
	    } else {
		throw new Exception("ControlChannel: unknown msg_type: " + msg_type);
	    }
	}
    }

    public class StdInChannel : Channel {
	string state = "normal"; // "waiting", "ready"
	public string input_reply = "";
	
	public string GetState() {
	    lock(state) {
		return state;
	    }
	}
	
	public void SetState(string newstate, string result) {
	    lock(state) {
		state = newstate;
		input_reply = result;
	    }
	}

	public StdInChannel(Session session, 
			    Authorization auth, 
			    string transport, 
			    string address, 
			    string port) : 
	    base(session, auth, transport, address, port, SocketType.ROUTER) {
	}

	
	// StdIn
	public override void on_recv(List<string> identities, 
				     string m_signature, 
				     IDictionary<string, object> m_header, 
				     IDictionary<string, object> m_parent_header, 
				     IDictionary<string, object> m_metadata, 
				     IDictionary<string, object> m_content) {
	    // StdIn handler
	    string msg_type = m_header["msg_type"].ToString();
	    if (msg_type == "input_reply") {
		SetState("ready", m_content["value"].ToString());
	    } else {
		throw new Exception("StdInChannel: unknown msg_type: " + msg_type);
	    }
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
	    while (! session.request_quit) {
		try {
		    string message = socket.Receive(Encoding.UTF8);
		    socket.Send(message, Encoding.UTF8);
		} catch {
		    break; // all done?
		}
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
	config_file = session.filename;
	session.start();
	while (true) {
	    if (session.need_restart) {
		session.need_restart = false;
		session.stop();
		session = new Session(calico, config_file);
		session.start();
	    }
	    if (session.request_quit) {
		session.stop();
		// this takes a few seconds, but does eventually stop
		try {
		    Gtk.Application.Quit();
		} catch {
		    // ignore
		}
		System.Environment.Exit(0);
	    }
	    Thread.Sleep ((int)(1 * 1000)); // seconds
	}
    }

    public class Client {

	public Client(string path=null, string sshserver=null) {
	}

	// % ipython profile create pcalico --parallel
	// % ipcluster start np=5 --profile calico
	// open the profile_calico/security/ipcontroller-client.json
	/*
	  {
	  "control": 37052, 
	  "task": 53034, 
	  "notification": 37699, 
	  "task_scheme": "leastload", 
	  "mux": 36320, 
	  "iopub": 39858, 
	  "ssh": "", 
	  "key": "181729da-2fa6-467c-857e-9d5c619fd22a", 
	  "registration": 46506, 
	  "interface": "tcp://127.0.0.1", 
	  "signature_scheme": "hmac-sha256", 
	  "pack": "json", 
	  "unpack": "json", 
	  "location": "192.168.1.100"
	  }
	*/
	// talk to hub: get machine list
	// send it a command, machine list
	// receive results (json?)
    }
}
