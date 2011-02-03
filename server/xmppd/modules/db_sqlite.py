from xmpp import *
import sys
sys.path.append("/var/www/html/myweb")
from sqllib import Database

import random, crypt, string

def makesalt(chars = string.letters + string.digits):
    # generate a random 2-character 'salt'
    return random.choice(chars) + random.choice(chars)

def encrypt(password, salt = makesalt()):
    return crypt.crypt(password, salt)

db= Database("/var/www/html/myweb/web.db")

class AUTH(PlugIn):
    NS=''
    def getpassword(self, node, domain):
        global db
        data = db.q("""select * from accounts where domain = %s and username = %s;""", domain, node)
        if len(data) == 1:
            return data[0]['password']
        else:
            return ""

    def isuser(self, node, domain):
        global db
        data = db.q("""select * from accounts where domain = %s and username = %s;""", domain, node)
        return len(data) == 1

class DB(PlugIn):
    NS=''
    def plugin(self, server):
        global db
        self.DEBUG('Building Database tree!','info')
        self._db = db

    def store(self,domain,node,stanza,id='next_unique_id'):
        self.DEBUG("Could not store in database:\n%s:%s::%s:%s"%(domain,node,id,stanza),'error')
        return False

    def get_store(self,domain,node,id):
        self.DEBUG("Could not get_store in database:\n%s:%s::%s"%(domain,node,id),'error')
        return False

    def addUser(self, domain, node, password):
        data = db.q("""select * from accounts where domain = %s and username = %s;""", domain, node)
        if len(data) == 0:
            db.q("""insert into accounts (domain, username, password) VALUES (%s, %s, %s);""",
                 domain, node, encrypt(password))
        
    def save(self,domain,node,stanza,id='next_unique_id'):
        self.DEBUG("DB ERR: Could not save to database:\n%s:%s::%s:%s"%(domain,node,id,stanza),'error')
        return False
    
    def save_to_roster(self,domain,node,jid,info,add_only_if_already=False):
        self.DEBUG("Saving roster info to database %s-->(%s) [%s]:\n"%(jid,node+'@'+domain,str(info)),'info')

    def pull_roster(self,domain,node,jid):
        return {'subscription':'to','name':"Help Desk"}

    def del_from_roster(self,domain,node,jid):
        self.DEBUG("Deleting roster info from database %s--X(%s):\n"%(jid,node+'@'+domain),'info')
        return True

    def del_from_roster_jid(self,domain,node,jid,what):
        self.DEBUG("Deleting roster info from database %s--X(%s):\n"%(jid,node+'@'+domain),'info')
        return True

    def save_groupie(self,domain,node,jid,groups):
        self.DEBUG("Saving groupie jid to database %s-->(%s) [%s]:\n"%(jid,node+'@'+domain,unicode(groups).encode('utf-8')),'info')

    def del_groupie(self,domain,node,jid):
        self.DEBUG("Deleting groupie from database %s--X(%s):\n"%(jid,node+'@'+domain),'info')
    
    def get(self,domain,node,what):
        if what == "roster":
            return {domain: {'subscription':'to','name':"Help Desk"}}
        elif what == "anon_allow":
            return 'yes'
        else:
            self.DEBUG('DB ERR: Could not retrieve %s::%s::%s'%(domain,node,what),'error') 

    def delete(self,domain,node,what):
        self.DEBUG('DB ERR: Could not delete %s::%s::%s'%(domain,node,what),'error') 

    def getNumRegistered(self, server):
        data = db.q("""select count() from accounts where domain = %s;""", server)
        return data[0]["count()"]

if __name__ == "__main__":
    a = AUTH()
    a.getpassword("daisy", "myro.roboteducation.org")
    d = DB()
    d.addUser("myro.roboteducation.org", "daisy", "passw0rd")
    print a.getpassword("daisy", "myro.roboteducation.org")
