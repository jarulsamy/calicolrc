using System.Collections.Generic;
using System; // List

namespace Calico {

    public class Chat {
    
        string server = "myro.roboteducation.org";
        int port = 5222;
        List<string> messages = new List<string>();
        agsXMPP.XmppClientConnection client;
        Calico.MainWindow calico;
        string user;
        string password;
        string status;
        bool alert;
        bool debug;

        public Chat(Calico.MainWindow calico, string user, string password) :
            this(calico, user, password, true) {
        }

        public Chat(Calico.MainWindow calico, string user, string password, bool debug) {
            this.calico = calico;
            this.user = user;
            this.password = password;
            this.debug = debug;

            client = new agsXMPP.XmppClientConnection(server, port);
            //client.UseSSL = false;
            //client.AutoRoster = false;
            //client.UseCompression = false;
            // Not supported //client.UseSso = false;
            //client.UseStartTLS = false;

            status = "offline";
            alert = true;

            //client.OnReadXml   += new agsXMPP.XmlHandler(OnReadXml);
            //client.OnWriteXml  += new agsXMPP.XmlHandler(OnWriteXml);
            //client.OnLogin     += OnLogin;
            //client.OnMessage   += new agsXMPP.protocol.client.MessageHandler(OnMessage);
            //client.OnError     += new agsXMPP.ErrorHandler(OnError);
            //client.OnAuthError += new agsXMPP.XmppElementHandler(OnAuthError);
            //client.OnClose     += OnClose;

            try {
                client.Open(user, password, "CalicoClient", 5);
            } catch (Exception e) {
                calico.Print(Tag.Error, e.Message);
            }
        }

        public void send(string to, string text) {
            client.Send(
                new agsXMPP.protocol.client.Message(String.Format("{0}@{1}", to, server),
                                                    agsXMPP.protocol.client.MessageType.chat,
                                                    text));
        }

        public string [] receive() {
            string [] retval;
            lock (messages) {
                retval = new string [messages.Count];
                messages.CopyTo(retval);
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
            send("", "2"); // make this my only login
            if (alert) {
                calico.Print(String.Format("You are now logged in as '{0}'.", user));
                send("admin", String.Format("[broadcast]\nroom: {0}\n{1}",
                                            "General",
                                            String.Format("{0} has joined the discussion", user)));
            }
            if (debug) {
                calico.Print(String.Format("LOGIN: {0}", user));
                //self.client.SendMyPresence()
            }
        }

        public void OnMessage(object sender, agsXMPP.protocol.client.Message msg) {
        }

        public void OnError(object sender, Exception e) {
            calico.Print(Tag.Error, String.Format("ERROR in Chat for user '{0}': {1}\n", user, e.Message));
        }

        public void OnAuthError(object sender, agsXMPP.Xml.Dom.Element element) {
        }
    
        public void OnClose(object sender) {
            client.Close();
        }
    }
}