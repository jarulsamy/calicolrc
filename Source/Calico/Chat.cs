using System.Collections.Generic;
using System; // List

namespace Calico {

    public class Chat {
    
        string server = "myro.roboteducation.org";
        int port = 5222;
        List<List<string>> messages = new List<List<string>>();
        agsXMPP.XmppClientConnection client;
        Calico.MainWindow calico;
        public string user;
        public string password;
        public string status;
        bool alert;
        bool debug;

        public Chat(Calico.MainWindow calico, string user, string password) :
            this(calico, user, password, true) {
        }

        public static string ParseFrom(string sfrom) {
            string [] parts = sfrom.Split(new char [] {'@'});
            return parts[0];
        }

        public Chat(Calico.MainWindow calico, string user, string password, bool debug) {
            this.calico = calico;
            this.user = user;
            this.password = password;
            this.debug = debug;

            client = new agsXMPP.XmppClientConnection(server, port);
            status = "offline";
            alert = true;

            client.OnReadXml   += new agsXMPP.XmlHandler(OnReadXml);
            client.OnWriteXml  += new agsXMPP.XmlHandler(OnWriteXml);
            client.OnLogin     += OnLogin;
            client.OnMessage   += new agsXMPP.protocol.client.MessageHandler(OnMessage);
            client.OnError     += new agsXMPP.ErrorHandler(OnError);
            client.OnAuthError += new agsXMPP.XmppElementHandler(OnAuthError);
            client.OnClose     += OnClose;

            try {
                client.Open(user, password, "Calico", 5);
            } catch (Exception e) {
                calico.Print(Tag.Error, e.Message);
            }
        }

        public void Send(string to, string text) {
            client.Send(
                new agsXMPP.protocol.client.Message(String.Format("{0}@{1}", to, server),
                                                    agsXMPP.protocol.client.MessageType.chat,
                                                    text));
        }

        public List<List<string>> Receive() {
            List<List<string>> retval = new List<List<string>>();
            lock (messages) {
                foreach(List<string> message in messages) {
                    retval.Add(message);
                }
                messages.Clear();
            }
            return retval;
        }
    
        public void OnReadXml(object sender, string xml) {
        }
    
        public void OnWriteXml(object sender, string xml) {
        }

        public void OnLogin(object sender) {
            if (user == "testname") {
                return;
            }
            status = "online";
            Send("", "2"); // make this my only login
            if (alert) {
                calico.Print(String.Format("You are now logged in as '{0}'.\n", user));
                Send("admin", String.Format("[broadcast]\nroom: {0}\n{1} has joined the discussion\n",
                                            "General",
                                            user));
            }
            if (debug) {
                calico.Print(String.Format("LOGIN: {0}\n", user));
                //self.client.SendMyPresence()
            }
        }

        public void OnMessage(object sender, agsXMPP.protocol.client.Message msg) {
            // Handle system messages here
            //calico.ChatPrint(Tag.Info, String.Format("Chat from {0}: {1}\n", msg.From, msg.Body));
            if (msg.Body.ToString().StartsWith("[blast]")) {
                string [] lines = msg.Body.ToString().Split('\n');             // [blast]
                if (lines.Length >= 5) {
                    string [] address  = lines[1].Split(new char[] {':'}, 2);  // from:
                    string [] type     = lines[2].Split(new char [] {':'}, 2); // type:
                    string [] filename = lines[3].Split(new char [] {':'}, 2); // file:
                    System.Text.StringBuilder code = new System.Text.StringBuilder();
                    for (int i = 4; i < lines.Length - 1; i++) {
                        code.AppendLine(lines[i]);                             // code
                    }
                    // Last line, no return:
                    code.Append(lines[lines.Length - 1]);
                    // Receive blast:
                    if (address[1] != this.user)
                        calico.ReceiveBlast(address[1].Trim(), type[1].Trim(), filename[1].Trim(), code.ToString());
                    else
                        calico.Print("Blast sent!\n");
                    return;
                }
                calico.Print(Tag.Error, String.Format("ERROR in Chat from {0}, not enough lines: {1}\n", msg.From, msg.Body));

            } else if (msg.Body.ToString().StartsWith("[list-cloud]")) {
                string [] lines = msg.Body.ToString().Split('\n');             // [list-cloud]
                if (lines.Length >= 0) {
                    string [] filenames  = lines[1].Split(new char[] {':'}, 2);  // filenames:
		    calico.OnOpenFromCloudCallback (filenames[1].Split(new char[] {','}));
		}
            } else if (msg.Body.ToString().StartsWith("[file]")) {
                string [] lines = msg.Body.ToString().Split('\n');             // [file]
                if (lines.Length >= 1) {
                    string [] filename  = lines[1].Split(new char[] {':'}, 2);  // filename:
                    string [] mode  = lines[2].Split(new char[] {':'}, 2);  // mode: open | save
                    System.Text.StringBuilder code = new System.Text.StringBuilder();
                    for (int i = 3; i < lines.Length - 1; i++) {
                        code.AppendLine(lines[i]);                             // code
                    }
                    // Last line, no return:
                    code.Append(lines[lines.Length - 1]);
                    // Receive blast:
		    calico.ReceiveBlast(msg.From, mode[1].ToString().Trim(), filename[1].Trim(), code.ToString());
                    return;
                }
                calico.Print(Tag.Error, String.Format("ERROR in Chat from {0}, not enough lines: {1}\n", msg.From, msg.Body));
            } else if (msg.Body.ToString().StartsWith("[data]")) {
                messages.Add(new List<string>() {msg.From, msg.Body});
            } else if (msg.Body.ToString().StartsWith("[info]")) {
                calico.Print(Tag.Info,
                   String.Format("Chat info: {0}\n", msg.Body));
            } else if (msg.Body.ToString().StartsWith("[broadcast]")) {
                string [] lines = msg.Body.ToString().Split('\n');
                string [] sfrom = lines[1].Split(':');
                calico.ChatPrint(Tag.Info,
                   String.Format("{0} to all: {1}\n", ParseFrom(sfrom[1].Trim()), lines[2]));
            } else if (msg.From.ToString().StartsWith("myro.roboteducation.org")) {
                // system message
            } else {
                calico.ChatPrint(Tag.Info,
                   String.Format("{0} to {1}: {2}\n", 
				 ParseFrom(msg.From), 
				 user,
				 msg.Body));
            }
        }

        public void OnError(object sender, Exception e) {
            calico.Print(Tag.Error, String.Format("ERROR in Chat for user '{0}': {1}\n", user, e.Message));
        }

        public void OnAuthError(object sender, agsXMPP.Xml.Dom.Element element) {
        }
    
        public void Close() {
            client.Close();
        }

        public void OnClose(object sender) {
            Close();
        }

	public bool GetFileFromCloud(string filename) {
	    string cloud_path = (string)calico.config.GetValue("config", "cloud-path");
            if (!System.IO.Directory.Exists(cloud_path)) {
                System.IO.Directory.CreateDirectory(cloud_path);
            }
	    // FIXME: get file and put in directory:
	    // if successful, return true:
	    Send("admin", String.Format("[load]\nfilename: {0}", filename));
	    return true;
	}

	public bool SaveFileToCloud(string filename, string basename) {
	    System.IO.TextReader reader = new System.IO.StreamReader(filename);
	    string file_text = reader.ReadToEnd();
	    reader.Close();
	    // FIXME: get file and put in directory:
	    Send("admin", String.Format("[save]\nfilename: {0}\n{1}",
					basename,
					file_text));
	    // if successful, return true:
	    return true;
	}
    }
}
