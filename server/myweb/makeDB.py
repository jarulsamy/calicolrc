from sqllib import Database
from functions import *
db = Database("web.db")
#db.query("""drop table accounts;""")
#db.query("""drop table institutions;""")
#db.query("""create table institutions (
#                 schoolname TEXT,
#                 school TEXT,
#                 sectionname TEXT,
#                 section TEXT,
#                 webpage TEXT);""")
#db.query("""create table accounts (
#                  username TEXT,
#                  email TEXT,
#                  password TEXT,
#                  perms TEXT,
#                  session TEXT,
#                  webpage TEXT,
#                  school TEXT,
#                  section TEXT,
#                  last_changed DATETIME,
#                  last_chat DATETIME
#                  );""")
#for (schoolname, school, sectionname, section) in [("Bryn Mawr College",
#                                                    "brynmawr",
#                                                    "CS110",
#                                                    "section"),
#                                                   ("Georgia Institute of Technology",
#                                                    "gatech",
#                                                    "Section #1",
#                                                    "sec1"),                                                   
#                                                   ("Georgia Institute of Technology",
#                                                    "gatech",
#                                                    "Section #2",
#                                                    "sec2"),
#                                                   ]:
for (schoolname, school, sectionname, section) in [("The Walker School",
                                                    "walker",
                                                    "TE2010",
                                                    "TE2010"),
                                                   ]:
    db.q("""insert into institutions (schoolname, school, sectionname, section)
            values (%s, %s, %s, %s);""",
         schoolname, school, sectionname, section)

#for (name, school, section) in [
#    ("kitty", "brynmawr", "section"),
#    ("bill", "brynmawr", "section"),
#    ("fritz", "brynmawr", "section"),
#    ("ratbreath", "gatech", "sec1"),
#    ("jimbo", "gatech", "sec1"),
#    ("larry", "gatech", "sec1"),
#    ("robot", "gatech", "sec2"),
#    ("evil", "gatech", "sec2"),
#    ("drevil", "gatech", "sec2")]:
#    db.q("""insert into accounts
#         (username, email, password, session, school, section, last_changed) values (%s, %s, %s, %s, %s, %s, %s);""",
#         name, name + "@" + school + ".edu", encrypt(name), makeSessionId(), school, section, fptime2stime(time.time()))

#data = db.q("""select * from webdata;""")
#for row in data:
#    if row["webpage"] != None and row["webpage"] != "":
#        # look up in accounts and replace
#        db.q("""update accounts set webpage = %s where username = %s;""", row["webpage"], row["username"])
#db.query("""drop table webdata;""")
print "Ok"
