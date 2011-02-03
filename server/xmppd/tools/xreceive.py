import sys
import xmpp
import os
import signal
import time

def messageCB(conn,msg):
    print "Sender: " + str(msg.getFrom())
    print "Content: " + str(msg.getBody())
    print msg

def StepOn(conn):
    try:
        conn.Process(1)
    except KeyboardInterrupt:
        return 0
    return 1

def GoOn(conn):
    while StepOn(conn):
        pass

def main():
    jid="newname@127.0.0.1"
    pwd="secret"

    jid=xmpp.protocol.JID(jid)
    cl = xmpp.Client(jid.getDomain(), debug=[])
    cl.connect()
    cl.auth(jid.getNode(),pwd)
    cl.RegisterHandler('message', messageCB) # was 'message'
    cl.sendInitPresence()
    GoOn(cl)
    
main()
