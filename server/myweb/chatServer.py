"""
Module that supports robot-to-robot communication via IM.

This module uses the jabber protocol for allowing
robot-to-robot communication. You can run your own
server and use that, as long as it supports
open registration of new users.

See the Myro Technical Guide for more details at:
http://wiki.roboteducation.org/
"""

import threading, time, random, pickle, sys
import StringIO, Image, os, sqllib
from functions import fptime2stime, encrypt
import base64
import traceback

__version__ = "$Revision: 1.14 $"
__author__  = "Doug Blank <dblank@cs.brynmawr.edu>"

db = sqllib.Database("web.db")

class FakeFile:
    """ To trick stderr into not printing warnings from xmpp. """
    def write(self, *args, **keys):
        pass
temp = sys.stderr        # save the stderr
sys.stderr = FakeFile()  # replace with fake
try:
    import xmpp              # import xmpp
except:
    xmpp = None
sys.stderr = temp        # replace stderr
temp = None              # clean up
del temp

def deliver_package(package):
    print "Received package!", package

class Chat:
    def __init__(self, name, password, debug = [], log = None):
	"""
        Constructs a connection for communicating to an IM server.

        name: can be "laura" to login into the default IM server,
        or can be "laura@yourserver.edu" to connect to the server
        running at "yourserver.edu".

        password: any password of your choosing. First time use
        will require the password to match the username on subsequent
        use.

        debug: can be [] for no debugging information, or can be
        ['always'] to get see all of the debugging information.
        """
        self.log = log
        self.lock = threading.Lock()
        self.messages = []
        # Start a thread up in here
        self.password = password
	if "@" not in name:
	    self.name, self.server = name, "myro.roboteducation.org"
	else:
            self.name, self.server = name.split("@")
	self.debug = debug
        self.client = xmpp.Client(self.server, debug=self.debug)
        print >> self.log, "Making connection to server..."
        self.log.flush()
        self.client.connect()
        print >> self.log, "Registering '%s'..." % self.name
        self.log.flush()
        self.register(self.name.lower(), self.password)
        try:
            self.open()
            self.ok = 1
        except:
            self.ok = 0
            print >> self.log, "ERROR: Invalid name/password combination"
            self.log.flush()

    def register(self, name, password):
	""" Register a username/password. """
        xmpp.features.register(self.client, self.server,
                               {"username": name.lower(),
                                "password": password})

    def messageCB(self, conn, msg):
	""" Message handling callback function. """
        self.lock.acquire()
        self.messages.append(msg)
        self.lock.release()

    def receive(self):
	"""
	Get all of the pending messages, and return them as a list.
	"""
        self.client.Process(1) # this should be in a thread
        self.lock.acquire()
        retval = self.messages
        self.messages = []
        self.lock.release()
        retvalList = []
        for m in retval:
            fromName = str(m.getFrom().node + "@" + m.getFrom().getDomain())
            message = str(m.getBody())
            retvalList.append( (fromName, message) )
        return retvalList

    def send(self, to, message):
	""" 
	Send a message to a named recipient. They must be logged in.
	"""
        self.client.send(
           xmpp.protocol.Message(to.lower() + "@" + self.server, message))

    def open(self):
	"""
	Open a connection to the server.
	"""
        print >> self.log, "Authenticating password for '%s'..." % self.name
        self.log.flush()
        try:
            self.client.auth(self.name.lower(), self.password)
        except IOError:
            self.client = xmpp.Client(self.server, debug=self.debug)
            self.client.connect()
            self.client.auth(self.name.lower(), self.password)
        print >> self.log, "Registering message handler..."
        self.log.flush()
        self.client.RegisterHandler('message', self.messageCB) 
        self.client.sendInitPresence()
        self.send("", "2") # make this the only place I'm logged in        
        messages = self.receive()
        count = 0
        while len(messages) == 0 and count < 5:
            print >> self.log, "   waiting for response..."
            self.log.flush()
            time.sleep(1)
            messages = self.receive()
            count += 1
        if count >= 5:
            print >> self.log, "Giving up!"
            self.log.flush()
            self.ok = 0
        else:
            print >> self.log, "Done!"
            self.log.flush()
            self.ok = 1

    def close(self):
	"""
	Close the connection to the server.
	"""
        self.client.disconnect()
        print >> self.log, "Disconnected!"
        self.log.flush()

    def __del__(self):
	""" Close the connection on destruction. """
        self.close()

if __name__ == "__main__":
    log = open("log", "w")
    ch = Chat("admin", "ipreweb", log=log)
    package = {}
    conferences = {"Developers": set(), "General": set()}
    if ch.ok == 1:
        while 1:
            #print "listening..."
            data = ch.receive()
            for msg in data:
                fromaddress = msg[0]
                fromname, school = fromaddress.split("@")
                print >> log, "From:", fromaddress, "at", time.time()
                log.flush()
                line0, rest = msg[1].split("\n", 1)
                print >> log, ("   line0: '%s'" % line0)
                if line0 == "password reset":
                    print >> log, "   Reset password request", fromaddress
                    log.flush()
                    lines = rest.split("\n")
                    data = {}
                    for line in lines:
                        if line.strip() == "": continue
                        field, value = line.split(": ", 1)
                        if field.strip() == "password":
                            print >> log, "      Field:", field, "value:", encrypt(value)
                        else:
                            print >> log, "      Field:", field, "value:", value
                        log.flush()
                        data[field.strip()] = value.strip()
                    rows = db.q("select * from accounts where username = %s and email = %s;",
                                data['username'].lower(),
                                data['email'])
                    if len(rows) > 0:
                        db.q("update accounts set password = %s where username = %s;",
                             encrypt(data['password']), data['username'].lower())
                        ch.send(fromname, "[result]\npassword reset!")
                    else:
                        ch.send(fromname, "[result]\ninvalid name/email combination!")
                elif line0 == "register":
                    data = {}
                    print >> log, "   Registering", fromaddress
                    log.flush()
                    lines = rest.split("\n")
                    for line in lines:
                        if line.strip() == "": continue
                        field, value = line.split(": ", 1)
                        if field.strip() == "password":
                            print >> log, "      Field:", field, "value:", encrypt(value)
                        else:
                            print >> log, "      Field:", field, "value:", value
                        log.flush()
                        data[field.strip()] = value.strip()
                    insert = 0
                    if data['keyword'] == "owls":
                        data['school'] = "brynmawr"
                        data['section'] = "cs110"
                        insert = 1
                    elif data['keyword'] == "attach":
                        data['school'] = "gatech"
                        data['section'] = "cs1301a"
                        insert = 1
                    elif data['keyword'] == "CS1301C":
                        data['school'] = "gatech"
                        data['section'] = "cs1301c"
                        insert = 1
                    elif data['keyword'] == "walkercs":
                        data['school'] = "walker"
                        data['section'] = "TE2010"
                        insert = 1
                    if insert:
                        # FIX: check for 'rename'
                        # if rename, delete, and rename image subdir
                        # check if name already in; if so, update
                        rows = db.q("select * from accounts where username = %s;", data['username'].lower())
                        if len(rows) > 0:
                            # update
                            db.q("update accounts set school = %s, section = %s, last_changed = %s, password = %s, email = %s where username = %s;",
                                 data['school'], 
                                 data['section'],
                                 fptime2stime(time.time()),
                                 encrypt(data['password']),
                                 data['email'],
                                 data['username'].lower())
                            ch.send(fromname, "[result]\nupdated registration!")
                        else:
                            db.q("insert into accounts (username, school, section, last_changed, password, email, domain) VALUES (%s, %s, %s, %s, %s, %s, %s);",
                                 data['username'].lower(), data['school'], 
                                 data['section'],
                                 fptime2stime(time.time()),
                                 encrypt(data['password']),
                                 data['email'],
                                 'myro.roboteducation.org')
                            ch.send(fromname, "[result]\nreceived registration!")
                    else:
                        ch.send(fromname, "[result]\ninvalid course keyword!")
                elif line0 == "package":
                    header = {'data': '', 'from': msg[0]}
                    for line in rest.split("\n"):
                        key, value = [item.strip() for item in line.split(":")]
                        header[key] = value
                    if "id" in header.keys():
                        package[header["id"]] = header
                        print >> log, "initiating package: id=%s" % header["id"]
                    else:
                        print >> log, "invalid package: no id"
                elif line0 == "segment":
                    line1, data = rest.split("\n", 1)
                    id, segment = [item.strip() for item in line1.split(" ", 1)]
                    package[id]["data"] += data
                    print >> log, "received segment: id=%s, segment=%s" % (package[id]["id"], segment)
                    if segment == package[id]["segments"]:
                        print >> log, "delivering package: id=%s" % package[id]["id"]
                        deliver_package(package[id])
                        del package[id]
                elif line0 == "[join]":
                    prop, value = rest.split(":", 1)
                    value = value.strip()
                    if value in conferences:
                        # FIXME: no permissions, just joining it
                        conferences[value].add(msg[0]) # from address
                        print >> log, ("/join: %s to %s" % (msg[0], value))
                        ch.send("%s/%s" % (msg[0], "PyjamaClient"), "[update]\nroom: %s" % value)
                        ch.send("%s/%s" % (msg[0], "PyjamaClient"), "[result]\nSuccessfully joined '%s'" % value)
                    else:
                        print >> log, ("ERROR: cannot /join: %s to %s, no such conference" % (msg[0], value))
                        ch.send("%s/%s" % (msg[0], "PyjamaClient"), "[result]\nNo such conference '%s'" % value)
                elif line0 == "[broadcast]":
                    line1, rest = rest.split("\n", 1)
                    prop, value = line1.split(":")
                    value = value.strip()
                    if value in conferences:
                        # FIXME: no permissions, just joining it
                        if msg[0] not in conferences[value]:
                            conferences[value].add(msg[0]) # from address
                        for address in conferences[value]:
                            ch.send("%s/%s" % (address, "PyjamaClient"), "[broadcast]\nfrom: %s\n%s" % (msg[0], rest))
                            print >> log, ("/broadcast to %s" % address)
                elif line0 == "[blast]":
                    line1, rest = rest.split("\n", 1)
                    prop, value = line1.split(":") # to:
                    value = value.strip()
                    if value in conferences: # send to a conference
                        # FIXME: no permissions, just joining it
                        if msg[0] not in conferences[value]:
                            conferences[value].add(msg[0]) # from address
                        for address in conferences[value]:
                            ch.send("%s/%s" % (address, "PyjamaClient"), "[blast]\nfrom: %s\n%s" % (msg[0], rest))
                            print >> log, ("/broadcast to %s" % address)
                    else:
                        # send to an individual
                        ch.send("%s@myro.roboteducation.org/%s" % (value, "PyjamaClient"), "[blast]\nfrom: %s\n%s" % (msg[0], rest))
                        print >> log, ("/broadcast to %s" % value)
                elif line0 == "[file]":
                    print >> log, "   receiving file..."
                    try:
                        line1, raw = rest.split("\n", 1)
                        field, value = line1.split(": ") # filename
                        print >> log, "      filename:", value
                        data = base64.b64decode(raw)
                        path = "data/" + fromname.strip()
                        os.system("mkdir -p %s" % path)
                        fp = open(path + "/" + value.strip(), "wb")
                        fp.write(data)
                        ch.send(fromname, "[result]\nreceived file!")
                        print >> log, "      from", fromname, "received file!"
                    except:
                        ch.send(fromname, "[result]\nfailed in sending file!")
                        print >> log, "      failed from", fromname
                        traceback.print_exc()
                    log.flush()
                elif line0 == "photo":
                    print >> log, "   receiving photo..."
                    try:
                        line1, raw = rest.split("\n", 1)
                        field, value = line1.split(": ")
                        print >> log, "      name:", value
                        ascii = raw.encode("ascii")
                        unpickled = pickle.loads(ascii)
                        sio = StringIO.StringIO(unpickled)
                        image = Image.open(sio)
                        path = "data/" + fromname.strip()
                        os.system("mkdir -p %s" % path)
                        image.save(path + "/" + value.strip() + ".jpg")
                        ch.send(fromname, "[result]\nreceived photo!")
                        print >> log, "      from", fromname, "received photo!"
                    except:
                        ch.send(fromname, "[result]\nfailed in sending photo!")
                        print >> log, "      failed from", fromname
                    print >> log, "   done!"
                    log.flush()
                else:
                    print >> log, " unknown mode:", line0[:50]
                    log.flush()
            time.sleep(1)
    else:
        print >> log, "Not logged in as adminstration"
        log.flush()


