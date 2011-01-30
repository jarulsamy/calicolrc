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
import System

class Chat:
    def __init__(self, pyjama, user, password, debug=False):
        self.status = "off-line"
        self.pyjama = pyjama
        self.user = user
        self.password = password
        self.debug = debug
        self.alert = True
        self.server = "myro.roboteducation.org"
        self.port = 5222
        self.messages = []

        self.client = agsXMPP.XmppClientConnection(self.server, self.port)
        self.client.UseSSL = False

        self.client.OnReadXml += agsXMPP.XmlHandler(self.OnReadXml)
        self.client.OnWriteXml += agsXMPP.XmlHandler(self.OnWriteXml)
        self.client.OnLogin += self.OnLogin
        self.client.OnMessage += agsXMPP.protocol.client.MessageHandler(self.OnMessage)
        self.client.OnError += agsXMPP.ErrorHandler(self.OnError)
        self.client.OnAuthError += agsXMPP.XmppElementHandler(self.OnAuthError)
        self.client.OnClose += self.close

        try:
            self.client.Open(self.user, self.password, "PyjamaClient", 5)
        except Exception, exp:
            traceback.print_exp()

    def send(self, to, text):
        self.client.Send(
            agsXMPP.protocol.client.Message("%s@%s" % (to, self.server),
                                            agsXMPP.protocol.client.MessageType.chat,
                                            text))

    def messages_waiting(self):
        return len(self.messages) > 0

    def receive(self):
        retval, self.messages[:] = self.messages[:], []
        return retval

    def close(self, sender=None):
        self.status = "off-line"
        self.client.Close()

    def OnLogin(self, sender):
        self.status = "login"
        if self.debug:
            print "LOGIN:", self.user
            #self.client.SendMyPresence()
        self.send("", "2") # make this my only login

    def OnError(self, sender, exp):
        if self.debug:
            print "ERROR:", self.user, exp

    def OnAuthError(self, sender, xml):
        self.status = "rejected"
        if self.debug:
            print "AUTHERROR:", self.user, xml

    def OnReadXml(self, sender, xml):
        if self.debug:
            print "READXML:", self.user, xml

    def OnWriteXml(self, sender, xml):
        if self.debug:
            print "WRITEXML:", self.user, xml

    def OnMessage(self, sender, msg):
        """
        msg.From = "id@server/client"
        """
        mfrom = "%s@%s" % (msg.From.User, msg.From.Server)
        self.messages.append((mfrom, msg.Body))
        if self.alert:
            self.pyjama.Print("Message from %s just received.\n" % mfrom)
        if self.debug:
            print "MESSAGE:", self.user, msg
