
from xmpp import *

db={}

def addUser(database, name, password):
    database[name] = {}
    database[name]['storage'] = {'karma':{'down': 307200,
                                          'up': 307200,
                                          'last_time': 0.0,
                                          'tot_down': 0,
                                          'tot_up': 0}}
    database[name]['password'] = password
    database[name]['name'] = name
    # Anon_allow tells the privacy subsystem if it's okay for someone to contact you
    # without any subscription at all.
    database[name]['anon_allow'] = 'yes'
    database[name]['roster'] = {}
    database[name]['groups'] = {}
    database[name]['groups']['Friends'] = [] 

def build_database(server_instance):
    global db
    for a_registered_server in server_instance.servernames:
        server_instance.DEBUG('server','DB: Building database tree for %s'%a_registered_server,'info')
        db[a_registered_server]={}
        addUser(db[a_registered_server], '__ir__', 'test')
        addUser(db[a_registered_server], 'test', 'test')
        for guy in db[a_registered_server].keys():
            db[a_registered_server][guy]['roster'][a_registered_server] = {'subscription':'to','name':"Help Desk"}
            
class AUTH(PlugIn):
    NS=''
    def getpassword(self, node, domain):
        try: return db[domain][node]['password']
        except KeyError: pass

    def isuser(self, node, domain):
        try: return db[domain].has_key(node)
        except KeyError: pass

class DB(PlugIn):
    NS=''
    def plugin(self,server):
        global db
        self.DEBUG('Building Database tree!','info')
        build_database(server) #Building database!
        self._db = db

    def store(self,domain,node,stanza,id='next_unique_id'):
        global db
        try:
            self.DEBUG("Storing to database:\n%s:%s::%s:%s"%(domain,node,id,stanza),'info')
            db[domain][node]['storage'][id] = stanza
            return True
        except KeyError:
            self.DEBUG("Could not store in database:\n%s:%s::%s:%s"%(domain,node,id,stanza),'error')
            return False

    def addUser(self, domain, name, password):
        global db
        if name in db[domain].keys():
            self.DEBUG("DB: Already a user '%s' for '%s'" % (name, domain,), 'warn')
            return
        self.DEBUG("DB: Ading user '%s' for '%s'" % (name, domain,), 'info')
        db[domain][name] = {}
        db[domain][name]['storage'] = {'karma':{'down': 307200,
                                                      'up': 307200,
                                                      'last_time': 0.0,
                                                      'tot_down': 0,
                                                      'tot_up': 0}}
        db[domain][name]['password'] = password
        db[domain][name]['name'] = name
        # Anon_allow tells the privacy subsystem if it's okay for someone to contact you
        # without any subscription at all.
        db[domain][name]['anon_allow'] = 'yes'
        db[domain][name]['roster'] = {domain: {'subscription':'to','name':"Help Desk"}}
        db[domain][name]['groups'] = {}
        db[domain][name]['groups']['Friends'] = [] 
        self.DEBUG("DB: Done adding user '%s' for '%s'" % (name, domain,), 'info')

    def get_store(self,domain,node,id):
        try:
            return db[domain][node]['storage'][id]
        except KeyError:
            return False

    def save(self,domain,node,stanza,id='next_unique_id'):
        try:
            self.DEBUG("Saving to database:\n%s:%s::%s:%s"%(domain,node,id,stanza),'info')
            db[domain][node][id] = stanza
            return True
        except KeyError:
            self.DEBUG("DB ERR: Could not save to database:\n%s:%s::%s:%s"%(domain,node,id,stanza),'error')
            return False
    
    def save_to_roster(self,domain,node,jid,info,add_only_if_already=False):
        self.DEBUG("Saving roster info to database %s-->(%s) [%s]:\n"%(jid,node+'@'+domain,str(info)),'info')
        if db[domain][node]['roster'].has_key(jid) and add_only_if_already==False:
            db[domain][node]['roster'][jid].update(info)
        else:
            db[domain][node]['roster'][jid] = info


    def pull_roster(self,domain,node,jid):
        try:
            data = db[domain][node]['roster'][jid]
            if data.has_key('subscription') == False:
                data.update({'subscription':'none'})
            return data
        except KeyError:
            self.DEBUG('DB ERR: Could not retrieve %s::%s::roster::%s'%(domain,node,jid),'error') 
            return None

    def del_from_roster(self,domain,node,jid):
        self.DEBUG("Deleting roster info from database %s--X(%s):\n"%(jid,node+'@'+domain),'info')
        try:
            del(db[domain][node]['roster'][jid])
            return True
        except KeyError, err:
            self.DEBUG('DB ERR: A Client tried to remove a contact that wasn\'t even added! (%s::%s::%s)'%(domain,node,jid),'error') 
            return False

    def del_from_roster_jid(self,domain,node,jid,what):
        self.DEBUG("Deleting roster info from database %s--X(%s):\n"%(jid,node+'@'+domain),'info')
        try:
            del(db[domain][node]['roster'][jid][what])
            return True
        except KeyError, err:
            self.DEBUG('DB ERR: A Client tried to remove a contact attr that wasn\'t even added! (%s::%s::%s)'%(domain,node,jid),'error') 
            return False

    def save_groupie(self,domain,node,jid,groups):
        temp = []
        for x in groups:
            if type(x)==type(u''): x = x.encode('utf-8')
            elif type(x)==type(u''): x = unicode(x).encode('utf-8')
            temp += [x]
        group_list = x
        self.DEBUG("Saving groupie jid to database %s-->(%s) [%s]:\n"%(jid,node+'@'+domain,unicode(groups).encode('utf-8')),'info')
        for gn,gm in db[domain][node]['groups'].iteritems():
            if gn not in group_list and jid in db[domain][node]['groups'][gn]:
                db[domain][node]['groups'][gn].remove(jid)
            elif gn in group_list and jid not in db[domain][node]['groups'][gn]:
                db[domain][node]['groups'][gn] += [jid]

    def del_groupie(self,domain,node,jid):
        try:
            self.DEBUG("Deleting groupie from database %s--X(%s):\n"%(jid,node+'@'+domain),'info')
            for gn,gm in db[domain][node]['groups'].iteritems():
                if jid in db[domain][node]['groups'][gn]:
                    db[domain][node]['groups'][gn].remove(jid)
        except Exception, err:
            self.DEBUG('DB ERR: A groupie went mad! %s::%s::%s'%(domain,node,jid),'error') 
    
    def get(self,domain,node,what):
        try:
            return db[domain][node][what]
        except KeyError:
            self.DEBUG('DB ERR: Could not retrieve %s::%s::%s'%(domain,node,what),'error') 
            return None

    def delete(self,domain,node,what):
        try:
            del(db[domain][node][what])
            return True
        except KeyError:
            self.DEBUG('DB ERR: Could not delete %s::%s::%s'%(domain,node,what),'error') 
            return None

    def getNumRegistered(self,server):
        return len(db[server].keys())
