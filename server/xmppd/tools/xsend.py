#!/usr/bin/python
# $Id: xsend.py,v 1.1 2004/06/20 09:45:09 snakeru Exp $
import sys,os,xmpp

tojid=sys.argv[1]
text=' '.join(sys.argv[2:])
fromjid = "test@127.0.0.1"
password = 'test'

jid=xmpp.protocol.JID(fromjid)
cl=xmpp.Client(jid.getDomain(),debug=[])

cl.connect()
cl.auth(jid.getNode(),password)

#cl.SendInitialPresence()
cl.send(xmpp.protocol.Message(tojid,text))

cl.disconnect()
