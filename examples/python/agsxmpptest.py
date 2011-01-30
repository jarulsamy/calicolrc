#
# Pyjama - Scripting Environment
#
# Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $Id: $

import clr
clr.AddReference("agsXMPP")
import agsXMPP
import traceback

class Chat:
    def __init__(self, user, password):
        self.user = user
        self.password = password
        self.server = "myro.roboteducation.org"
        self.port = 5222

        self.client = agsXMPP.XmppClientConnection(self.server, self.port)
        self.client.UseSSL = False

        self.client.OnReadXml += agsXMPP.XmlHandler(self.OnReadXml)
        self.client.OnWriteXml += agsXMPP.XmlHandler(self.OnWriteXml)
        self.client.OnLogin += self.OnLogin
        self.client.OnMessage += agsXMPP.protocol.client.MessageHandler(self.OnMessage)
        self.client.OnError += agsXMPP.ErrorHandler(self.OnError)
        self.client.MessageGrabber.Add(
            agsXMPP.Jid("%s@%s" % (self.user, self.server)),
            None,
            agsXMPP.MessageCB(self.OnGetMessage))

        try:
            self.client.Open(self.user, self.password, "PyjamaClient", 5)
        except Exception, exp:
            traceback.print_exp()

    def OnGetMessage(self, sender, msg, data):
        if (msg.Body != None):
            print "msg:", msg.From.User, msg.Body
            #Console.ForegroundColor = ConsoleColor.Red;
            #Console.WriteLine("{0}>> {1}", msg.From.User, msg.Body);
            #Console.ForegroundColor = ConsoleColor.Green;

    def send(self, to, text):
        self.client.Send(
            agsXMPP.protocol.client.Message("%s@myro.roboteducation.org" % to,
                                                   agsXMPP.protocol.client.MessageType.chat,
                                                   text))
    def close(self):
        self.client.Close()

    def OnLogin(self, sender):
        print("we are logged in to the server now")
        #print("set presence")
        #self.client.SendMyPresence()

    def OnError(self, sender, exp):
        print(exp)

    def OnReadXml(self, sender, xml):
        print("RECV XML: " + xml )

    def OnWriteXml(self, sender, xml):
        print("SEND XML: " + xml )

    def OnMessage(self, sender, xml):
        print("MESSAGE: " + xml)

