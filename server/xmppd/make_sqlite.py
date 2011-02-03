import sys
sys.path.append("/var/www/html/myweb")
from sqllib import Database

db= Database("/var/www/html/myweb/web.db")

db.query("""drop table jabber;""")

db.query("""create table jabber (
                  domain TEXT,
                  node TEXT,
                  password TEXT
                  );""")

