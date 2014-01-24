// IPython kernel backend in C#
// Doug Blank

using System;
using System.Text;
using System.IO;
using ZeroMQ;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters; // CustomCreationConverter
using System.Collections.Generic; // IDictionary
using System.Security.Cryptography;

public static class ZMQServer {

    public static string encode(IDictionary<string, object> dict) {
	return JsonConvert.SerializeObject(dict);
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
	public ShellChannel shell_channel;
	public IOPubChannel iopub_channel;
	public Authorization auth;

	public Session(string filename) {
	    this.filename = filename;
	    session_id = System.Guid.NewGuid().ToString();
	    string json = File.ReadAllText(filename);
	    var config = decode(json);
	    auth = new Authorization(config["key"].ToString(),
				     config["signature_scheme"].ToString());
	    shell_channel = new ShellChannel(auth,
		    config["transport"].ToString(), 
		    config["ip"].ToString(),
		    config["shell_port"].ToString());
	    iopub_channel = new IOPubChannel(auth,
		    config["transport"].ToString(), 
		    config["ip"].ToString(),
		    config["iopub_port"].ToString());
	}
    }

    public class Channel {
	public string transport;
	public string address;
	public ZmqContext context;
	public ZmqSocket socket;
	public string port;
	public Authorization auth;

	public Channel(Authorization auth, string transport, 
		       string address, 
		       string port, 
		       SocketType socket_type) {
	    this.auth = auth;
	    this.transport = transport;
	    this.address = address;
	    this.port = port;
	    context = ZmqContext.Create();
	    socket = context.CreateSocket(socket_type);
	    socket.Bind(
		String.Format("{0}://{1}:{2}", 
			      this.transport, this.address, this.port));
	}

	public void loop() {
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
		    Console.WriteLine("on_recv: " + signature);
		    Console.WriteLine("       : " + comp_sig);
		    Console.WriteLine("Error: signatures don't match!");
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

	public void send(IDictionary<string,object> header,
			 IDictionary<string,object> parent_header,
			 IDictionary<string,object> metadata,
			 IDictionary<string,object> content) {
	    send(encode(header), encode(parent_header), encode(metadata),
		 encode(content));
	}


	public void send(string header,
			 string parent_header,
			 string metadata,
			 string content) {
	    string signature = auth.sign(new List<string>() {header, parent_header, 
							     metadata, content});
	    Console.WriteLine("send: [{0} {1} {2} {3} {4} {5}]",
			      "<IDS|MSG>",
			      signature,
			      header,
			      parent_header,
			      metadata,
			      content);
	    send_multipart(new List<string>() 
			   {"<IDS|MSG>",
				   signature,
				   header,
				   parent_header,
				   metadata,
				   content});
	}

	public void send_multipart(List<string> list) {
	    int count = 0;
	    foreach (string msg in list) {
		if (count < list.Count) {
		    socket.SendMore(msg, Encoding.UTF8);
		} else {
		    socket.Send(msg, Encoding.UTF8);
		}
		count++;
	    }
	}
    }

    public class ShellChannel : Channel {
	public ShellChannel(Authorization auth, string transport, string address, 
			    string port) : 
	    base(auth, transport, address, port, SocketType.DEALER) {
	}

	public IDictionary<string, object> Header(string date, 
		      string msg_id,
		      string username,
		      string session,
		      string msg_type) {
	    Dictionary<string,object> dict = new Dictionary<string,object>();
	    dict["date"] = date;
	    dict["msg_id"] = msg_id;
	    dict["username"] = username;
	    dict["session"] = session;
	    dict["msg_type"] = msg_type;
	    return dict;
	}

	public static string now() {
	    return DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffffff");
	}

	public static string msg_id() {
	    return System.Guid.NewGuid().ToString();
	}

	public override void on_recv(string m_signature, 
				IDictionary<string, object> m_header, 
				IDictionary<string, object> m_parent_header, 
				IDictionary<string, object> m_metadata, 
				IDictionary<string, object> m_content) {

	    Console.WriteLine("on_recv" + m_header["msg_type"]);
	    if (m_header["msg_type"].ToString() == "execute_request") {
		var header = Header(now(),
				    msg_id(),
				    "kernel",
				    m_header["session"].ToString(),
				    "status"
				    );
		var metadata = new Dictionary<string, object>();
		var content = new Dictionary<string, object>
		{
		    {"execution_state", "busy"}
		};
		send(header, m_header, metadata, content);
		/*
        header = {
            "date": datetime.datetime.now().isoformat(),
            "msg_id": msg_id(),
            "username": "kernel",
            "session": m_header["session"],
            "msg_type": "pyin",
        }
        metadata = {}
        content = {
            'execution_count': execution_count,
            'code': m_content["code"],
        }
        send(iopub_stream, header, m_header, metadata, content)
        #######################################################################
        header = {
            "date": datetime.datetime.now().isoformat(),
            "msg_id": msg_id(),
            "username": "kernel",
            "session": m_header["session"],
            "msg_type": "pyout",
        }
        metadata = {}
        content = {
            'execution_count': execution_count,
            'data': {"text/plain": "result!"},
            'metadata': {}
        }
        send(iopub_stream, header, m_header, metadata, content)
        #######################################################################
        header = {
            "date": datetime.datetime.now().isoformat(),
            "msg_id": msg_id(),
            "username": "kernel",
            "session": m_header["session"],
            "msg_type": "status",
        }
        metadata = {}
        content = {
            'execution_state': "idle",
        }
        send(iopub_stream, header, m_header, metadata, content)
        #######################################################################
        header = {
            "date": datetime.datetime.now().isoformat(),
            "msg_id": msg_id(),
            "username": "kernel",
            "session": m_header["session"],
            "msg_type": "execute_reply",
        }
        metadata = {
            "dependencies_met": True,
            "engine": engine_id,
            "status": "ok",
            "started": datetime.datetime.now().isoformat(),
        }
        content = {
            "status": "ok",
            "execution_count": execution_count,
            "user_variables": {},
            "payload": [],
            "user_expressions": {},
        }
        send(shell_stream, header, m_header, metadata, content)
        execution_count += 1
		*/
	    } else if (m_header["msg_type"].ToString() == "kernel_info_request") {
		var header = Header(now(),
				    msg_id(),
				    "kernel",
				    m_header["session"].ToString(),
				    "kernel_info_reply"
				    );
		var metadata = new Dictionary<string, object>();
		var content = new Dictionary<string, object>
		    {
			{"protocol_version", new List<int>() {4, 0}},
			{"ipython_version", new List<object>() {1, 1, 0, ""}},
			{"language_version", new List<int>() {0, 0, 1}},
			{"language", "simple_kernel"},
			{"execution_state", "busy"}
		    };
		send(header, m_header, metadata, content);
	    /*
    elif m_header["msg_type"] == "history_request":
        header = {
            "date": datetime.datetime.now().isoformat(),
            "msg_id": msg_id(),
            "username": m_header["username"],
            "session": m_header["session"],
            "msg_type": "kernel_info_reply",
        } 
        content = {
            'output' : False,
        }
        send(shell_stream, header, m_header, m_metadata, content)
		*/
	    } else {
		throw new Exception("unknown msg_type: " + m_header["msg_type"]);
	    }
	}
    }

    public class IOPubChannel : Channel {
	public IOPubChannel(Authorization auth, string transport, string address, 
			    string port) : 
	    base(auth, transport, address, port, SocketType.PUB) {
	}
    }

    static void Main(string[] args) {
	Session session = new Session(args[0]);
	session.shell_channel.loop();
    }
}
