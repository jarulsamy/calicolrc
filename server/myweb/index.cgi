#!/usr/bin/env python

import cgi, Cookie, os, random, datetime, sys, glob
# Traceback:
import cgitb; cgitb.enable() # later: (display=0, logdir="/tmp")
from functions import *
from sqllib import Database

form = cgi.FieldStorage()
db = Database("web.db")

########################################
cookie = Cookie.Cookie( os.environ.get("HTTP_COOKIE", "") )
uid = None
if cookie.get("session") != None:
    uid = verify_session(cookie["session"].value, db)
    if uid != None:
        if form.has_key("logout"):   ### Trying to logout
            expire = datetime.datetime.now() - datetime.timedelta(100)
            cookie["session"]["expires"] = expire.strftime("%a, %d %b %Y %H:00:00 GMT")
            httpHeader(cookie)
        else:
            httpHeader()
    else:
        # added to try to fix cookie bug
        expire = datetime.datetime.now() - datetime.timedelta(100)
        cookie["session"]["expires"] = expire.strftime("%a, %d %b %Y %H:00:00 GMT")
        httpHeader(cookie)
else:
    if form.has_key("verify"): # signals that you just tried to log in
        session = verify_password(form, db)
        if session != None:
            cookie = Cookie.Cookie()
            cookie["session"] = session # login, create, and set to expire in 1 day
            expire = datetime.datetime.now() + datetime.timedelta(1)
            cookie["session"]["expires"] = expire.strftime("%a, %d %b %Y %H:00:00 GMT")
            httpHeader(cookie)
            uid = form["username"].value
            if uid:
                uid = uid.strip()
        else:
            httpHeader()
            # not logged in
    else:
        httpHeader()

def lookup_user_path(db, user):
    """
    Returns the school/section/user/ path.
    """
    data = db.q("""select * from accounts where username = %s;""", user)
    if len(data) == 1:
        return data[0]["school"] + "/" + data[0]["section"] + "/" + user + "/"
    else:
        return "unknown/section/%s/" % user

def lookup_school(db, school):
    '''
    Returns the school
    '''
    data = db.q("""select distinct schoolname from institutions where school = %s;""", school)
    if len(data) == 1:
        return data[0]["schoolname"] 
    else:
        return "Unknown"

def make_html_path(db, school = None, schoolname = None, section = None, sectionname = None, robot = None):
    if school == None:
        data = db.q("""select distinct school, schoolname from institutions order by schoolname;""")
        options = []
        for row in data:
            options.append( [row["school"], row["schoolname"]])
        retval = "<form action=\"index.cgi\" method=\"post\" name=\"myform\">"
        retval += "<a href=\"/myweb/\" target=\"_top\">Myweb</a> / "
        retval += picklist("select", "", options, blankOption = 1, onChange="go()")
        retval += "</form>"
    elif section == None:
        data = db.q("select distinct school, schoolname, section, sectionname from institutions where school = %s order by schoolname;",
                    school)
        options = []
        for row in data:
            options.append( [row["school"] + "/" + row["section"], row["sectionname"]])
        retval = "<form action=\"index.cgi\" method=\"post\" name=\"myform\">"
        retval += ("<a href=\"/myweb/\" target=\"_top\">Myweb</a> / " +
                   ("<a href=\"/myweb/%s/\" target=\"_top\">%s</a> / " % (school, schoolname)))
        retval += picklist("select", "", options, blankOption = 1, onChange="go()")
        retval += "</form>"
    elif robot == None:
        data = db.q("select school, section, username from accounts where school = %s and section = %s order by username;",
                    school, section)
        options = []
        for row in data:
            options.append( [row["school"] + "/" + row["section"] + "/" + row["username"],
                             row["username"]])
        retval = "<form action=\"index.cgi\" method=\"post\" name=\"myform\">"
        retval += ("<a href=\"/myweb/\" target=\"_top\">Myweb</a> / " +
                   ("<a href=\"/myweb/%s/\" target=\"_top\">%s</a> / " % (school, schoolname)) +
                   ("<a href=\"/myweb/%s/%s/\" target=\"_top\">%s</a> / " % (school, section, sectionname)))
        retval += picklist("select", "", options, blankOption = 1, onChange="go()")
        retval += "</form>"
    else:
        data = db.q("select distinct school, section, username from accounts where school = %s and section = %s order by username;",
                    school, section)
        options = []
        for row in data:
            options.append( [row["school"] + "/" + row["section"] + "/" + row["username"], row["username"]])
        retval = "<form action=\"index.cgi\" method=\"post\" name=\"myform\">"
        retval += ("<a href=\"/myweb/\" target=\"_top\">Myweb</a> / " +
                   ("<a href=\"/myweb/%s/\" target=\"_top\">%s</a> / " % (school, schoolname)) +
                   ("<a href=\"/myweb/%s/%s/\" target=\"_top\">%s</a> / " % (school, section, sectionname)))
        retval += picklist("select", school + "/" + section + "/" + robot, options, onChange="go()")
        retval += "</form>"
    return retval

def lookup_sectionname(db, school, section):
    data = db.q("""select * from institutions where school = %s and section = %s;""", school, section)
    if len(data) == 1:
        return data[0]["sectionname"] 
    else:
        return "Unknown Section"

def show_header(form, db, uid): 
    print "<html>"
    print begin("head")
    print """
    <SCRIPT LANGUAGE="JavaScript" TYPE="text/javascript">
    <!--
    function onChange(path) {
        this.form.click.value = path;
        this.form.method.value = "post";
        this.form.submit();
    }
    function go() {
        box = document.forms[0].select;
        destination = box.options[box.selectedIndex].value;
        if (destination)
           //confirm(destination);
           top.location.href = "/myweb/" + destination + "/";
    }
    // -->
    </SCRIPT>
"""
    print end("end")
    print "<body bgcolor=\"white\">"
    retval = ""
    retval += "<b><a href=\"http://www.roboteducation.org/\" target=\"_top\" style=\"text-decoration: none; color: #6392e4;\">Institute for Personal Robots in Education</a></b><br>" 
    args = form.getvalue('args')
    if form.getvalue("verify"):
        if uid == None:
            retval += make_html_path(db)
        else:
            path = lookup_user_path(db, uid)
            school, section, robot, temp = path.split("/", 3)
            schoolname = lookup_school(db, school)
            sectionname = lookup_sectionname(db, school, section)
            retval += make_html_path(db, school, schoolname, section, sectionname, robot)
    elif args == None:
        retval += make_html_path(db)
    elif args.count("/") == 1:
        school, index = args.split("/", 1)
        schoolname = lookup_school(db, school)
        retval += make_html_path(db, school, schoolname)
    elif args.count("/") == 2:
        school, section, index = args.split("/", 2)
        schoolname = lookup_school(db, school)
        sectionname = lookup_sectionname(db, school, section)
        retval += make_html_path(db, school, schoolname, section, sectionname)
    elif args.count("/") >= 3:
        school, section, robot, index = args.split("/", 3)
        schoolname = lookup_school(db, school)
        sectionname = lookup_sectionname(db, school, section)
        retval += make_html_path(db, school, schoolname, section, sectionname, robot)
    if uid == None:
        menu = "<a href=index.cgi?login=1 target=\"body\">Log in</a> | <a href=\"http://wiki.roboteducation.org/Introduction_to_Computer_Science_via_Robots\" target=\"_blank\">Textbook</a><br>"
    else:
        menu = "Logged in as <a href=\"/myweb/%s\" target=\"_top\">%s</a> | <a href=/myweb/index.cgi?logout=1 target=\"_top\">log out</a> | <a href=\"http://wiki.roboteducation.org/Introduction_to_Computer_Science_via_Robots\" target=\"_blank\">Textbook</a><br>" % (lookup_user_path(db, uid), uid)
        menu += (link("Edit your page", lookup_user_path(db, uid) + "edit/", target="_top") +
                 " | " +
                 link("HTML Help", "http://wiki.roboteducation.org/HTML_Help", target="_blank"))
    print begin("table", width="100%")
    print tr( td("<a href=\"http://www.roboteducation.org/\" target=\"_top\"><img border=0 src=\"/myweb/images/ipre.png\"></img></a>", width=150) + td(retval)+ td(" ") + tdr(menu)) 
    print end("table")
    print "</body>"
    print "</html>"

def show_body_head():
    print begin("html")
    print begin("body")
    print "<body bgcolor=\"white\">" # #63cae4\">"

def show_body_foot():
    print p()
    print p()
    print p()
    print """
<i>Copyright 2007, <a href="http://roboteducation.org/">Institute for Personal Robots in Education</a></i>
"""
    print "</body>"
    print "</html>"    

def get_section_list(form, db, uid, school):
    data = db.q("select distinct section, school from accounts where school = %s order by section;", school)
    print begin("ul")
    for row in data:
        sectionname = lookup_sectionname(db, school, row["section"])
        print li(link(sectionname, "./" + row["school"] + "/" + row["section"] + "/", target="_top"))
    print end("ul")

def get_school_list(form, db, uid):
    print begin("ul")
    data = db.q("select distinct school, schoolname from institutions order by schoolname;")
    for row in data:
        print li(link(row["schoolname"], "./" + row["school"] + "/", target="_top"))
    print end("ul")

def get_robots(form, db, uid, school, section):
    data = db.q("select school, section, username, last_changed from accounts where school = %s and section = %s order by username;",
                school, section)
    institute = db.q("select * from institutions where school = %s and section = %s order by school;",
                     school, section)
    print h2("The Robots from %s, %s" % (institute[0]["schoolname"], institute[0]["sectionname"]))
    print begin("ul")
    for row in data:
        print li( link( row["username"], "./" + row["school"] + "/" + row["section"] + "/" + row["username"] + "/",
                        target = "_top") + " last changed at " + row["last_changed"])
    print end("ul")

def after_succ_login(form, db, uid):
    path = lookup_user_path(db, uid)
    school, section, robot, temp = path.split("/", 3)
    data = db.q("""select webpage from accounts where username = %s;""",
                uid)
    if len(data) == 1 and data[0]["webpage"] != None:
        print data[0]["webpage"]
        show_files(form, db, uid)
    else:
        show_body_head()
        print p("This is <b>%s</b>'s page!" % robot)
        show_body_foot()

def show_files(form, db, uid):
    # data files:
    if uid is None: return
    filelist = glob.glob("data/" + uid + "/*")
    if len(filelist) == 0:
        print p("You don't have any files uploaded yet.")
    else:
        print p("You have the following files uploaded:")
        print begin("ul")
        for file in filelist:
            print li( link("/myweb/" + file, file) +
                      format("TT", """    &lt;img src="/myweb/%s"&gt;""" % file))
        print end("ul")
    
def show_student_page(form, db, uid):
    args = form.getvalue('args')
    school, section, robot, index = args.split("/", 3)
    data = db.q("""select * from accounts where username = %s;""", robot)
    if uid == None:
        print p("Log in to edit your page.")
    if len(data) == 0:
        show_body_head()
        print p("This is <b>%s</b>'s page, but <b>%s</b> hasn't created it yet!" % (robot, robot))
        show_body_foot()
    elif data[0]["webpage"] == None:
        show_body_head()
        print p("This is <b>%s</b>'s page, but <b>%s</b> hasn't created it yet!" % (robot, robot))
        show_body_foot()
    else:
        print data[0]["webpage"]
        show_files(form, db, uid)

def edit_mode(form, db, uid):
    args = form.getvalue('args')
    school, section, robot, mode, index = args.split("/", 4)
    show_body_head()
    print begin("FORM", name="MAIN", method="POST", # PUT
                action=URLPATH + school + "/" + section + "/" + robot + "/save/",
                enctype="multipart/form-data", target="_top")
    data = db.q("""select * from accounts where username = %s;""",
                uid)
    if len(data) == 1:
        webpage = data[0]["webpage"]
    else:
        webpage = ""
    print input("webpage", webpage, "TEXTAREA", rows=20, cols=60)
    print p()
    print submit("Save")
    print end("FORM")
    filelist = glob.glob("data/" + uid + "/*")
    if len(filelist) == 0:
        print p("You don't have any files uploaded yet.")
    else:
        print p("You have the following files uploaded:")
        print begin("ul")
        for file in filelist:
            print li( link("/myweb/" + file, file) +
                      format("TT", """    &lt;img src="/myweb/%s"&gt;""" % file))
        print end("ul")
    
    show_body_foot()

def save_mode(form, db, uid):
    args = form.getvalue('args')
    school, section, robot, mode, index = args.split("/", 4)
    data = db.q("""select webpage from accounts where username = %s;""",
                uid)
    if len(data) == 1:
        print data[0]["webpage"]

def show_ipre_body(): print """

<p><a href="http://roboteducation.org/">The Institute for Personal Robots
in Education</a> (IPRE) applies and evaluates robots as a context for
computer science education. IPRE is a joint effort between <a
href="http://www.gatech.edu/">Georgia Tech</a> and <a
href="http://brynmawr.edu/">Bryn Mawr College</a> sponsored by <a
href="http://research.microsoft.com/">Microsoft Research</a>.</p>

<p>This website is an experimental homepage for student projects using
the Myro software. To see the robot websites at a particular school,
click one of the following links: </p>

"""
        
def show_body(form, db, uid):
    args = form.getvalue('args')
    if form.has_key("verify"):
        if uid == None:
            show_body_head()
            print p("Invalid ID or password.")
            show_body_foot()
        else:
            after_succ_login(form, db, uid)
    elif args == None: # show schools
        show_body_head()
        show_ipre_body()
        print h2("Schools")
        get_school_list(form, db, uid)
        show_body_foot()
    elif args.count("/") == 1: # show section at school
        school, index = args.split("/", 1)
        schoolname = lookup_school(db, school)
        show_body_head()
        print h2("Sections at %s" % schoolname)
        get_section_list(form, db, uid, school)
        show_body_foot()
    elif args.count("/") == 2:
        school, section, index = args.split("/", 2)
        show_body_head()
        get_robots(form, db, uid, school, section)
        show_body_foot()
    elif args.count("/") == 3:
        # get and show student page:
        show_student_page(form, db, uid)
    elif args.count("/") == 4:
        school, section, robot, mode, index = args.split("/", 4)
        if mode == "edit":
            edit_mode(form, db, uid)
        elif mode == "save":
            save_mode(form, db, uid)
            # redisplay here

def show_frames(form, db, uid):
    print "<HTML>"
    print " <HEAD>"
    print "  <TITLE>IPRE Myweb</TITLE>"
    print " </HEAD>"
    print " <FRAMESET rows=\"120px,*\">"
    if form.has_key("verify"):
        print "  <FRAME name=\"header\" noresize src=\"index.cgi?header=1&verify=1\">"
        print "  <FRAME name=\"body\" noresize src=\"index.cgi?body=1&verify=1\">"
    else:
        print "  <FRAME name=\"header\" noresize src=\"index.cgi?header=1\">"
        print "  <FRAME name=\"body\" noresize src=\"index.cgi?body=1\">"
    print "  <NOFRAMES>"
    print "   Please use a browser that supports frames."
    print "  </NOFRAMES>"
    print " </frameset>"
    print "</html>"

print """<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">"""
args = form.getvalue('args')
if args != None and "admin" in args.split("/"):
    # maybe have some admin functions here
    show_header(form, db, uid)
    data = db.q("""select username, password, school, section from accounts;""")
    print data.makeTable()
elif form.has_key("header"):
    show_header(form, db, uid)
elif form.has_key("body"):
    show_body(form, db, uid)
elif form.has_key("login"):
    login(form, db)
else:
    if form.has_key("webpage") and uid != None:
        data = db.q("""select * from accounts where username = %s;""",
                    uid)
        if len(data) == 0:
            db.q("""insert into accounts (webpage, username) values (%s, %s);""",
                 form.getvalue("webpage"), uid)
        else:
            db.q("""update accounts set webpage = %s, last_changed = %s where username = %s;""",
                 form.getvalue("webpage"), fptime2stime(time.time()), uid)
    show_frames(form, db, uid)
