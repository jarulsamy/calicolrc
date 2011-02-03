# Functions for reference web site
# By Doug Blank

import md5, time, base64, random, string, crypt, os, sys, urllib, math
import StringIO, re
from PIL import Image

URL = "http://myro.roboteducation.org/"
URLPATH = "/myweb/"
FILEPATH = "/home/dblank/html/myweb/"
TITLE = "IPRE"
RECORDS_PER_PAGE = 10
MONTHS = ["January",
          "February",
          "March",
          "April",
          "May",
          "June",
          "July",
          "August",
          "September",
          "October",
          "November",
          "December",
          ]

def error(form, db, uid, msg):
    print header(form, db, uid, "Error")
    print msg

def isempty(value):
    return (value == 'None' or value == None or value == "" or value == 0)

def httpHeader(*more):
    print "Content-Type: text/html"
    for m in more:
        print m, "\n"
    print

def header(form, db, uid = None, title=TITLE, field="", value=""):
    if title != TITLE:
        title = TITLE + ": " + title
    if value == "%27%27":
        value = ""
    retval = """<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<HTML>
<HEAD>
    <TITLE>%s</TITLE>
    <link rel="stylesheet" type="text/css" href="//myro.roboteducation.org/~dblank/reference/citeulike.css">
    <SCRIPT LANGUAGE="JavaScript" TYPE="text/javascript">
    <!--
    function onChange(path) {
        myform.click.value = path;
        myform.method.value = "post";
        myform.submit();
    // -->
    </SCRIPT>
</HEAD>
<BODY>
<div style="position:absolute; top: 18px; left: 20px; padding: 0px;">
  <div class="logo">
     <!-- <img src="slogo.gif"> -->
     <h1>%s</h1>
  </div>
</div>
<div class="topright">""" % (title, title)
    query = "&" + urllib.urlencode([("f", form.getvalue("f")),
                                    ("o", form.getvalue("o")),
                                    ("q", form.getvalue("q")),
                                    ("u", form.getvalue("u")),
                                    ("start", form.getvalue("start")),
                                    ("page", form.getvalue("page")),
                                    ("bibid", form.getvalue("bibid")),
                                    ("m", "search")])
    if uid != None:
        retval += """Logged in as <a href="?u=%s&f=last_name&q=''&o=datetime&m=search">%s</a> | <a href="?logout=1">Log out</a> | <a href="?m=faq">FAQ</a> | <a href="?m=advanced">Advanced Search</a>""" % (uid, uid)
    else:
        retval += """<a href="?login=1%s">Log in</a> | <a href="?m=faq">FAQ</a> | <a href="?m=advanced">Advanced Search</a>""" % query
    #<H1>%s</H1>
    retval += """</div> <!-- close topright -->"""

    retval += begin("FORM", name="MAIN", method="POST",
                    action=URLPATH,
                    enctype="multipart/form-data")
    retval += hidden("m", "search")
    retval += """
    <div style="position: absolute; text-align: right; right: 20px; top: 35px; width: 450px; height: 20px;">
    <br>
    """
    options = [["title", "Article title"],
               ["last_name", "Author/Editor"],
               ["abstract", "Abstract"],
               ["journal", "Journal name"],
               ["tag", "Tag"],
               ["date", "Published Date"],
               ["pdate", "Posted Date"],
               ]
    retval += "Quick search: "
    retval += picklist("f", field, options)
    retval += """
    <input onfocus="this.style.border='2px solid #000'" onblur="this.style.border='2px solid #ccc'" name="q" size="20" value="%s">
    <input value="Search" type="submit">
    </div>
    """ % value
    retval += end("FORM")
    retval += """<div id="navleft">"""
    retval += format("H2", "Navigation")
    if uid != None:
        retval += makemenu([("Home", "?m=home"),
                            ("Log out", "?logout=1"),
                            ("Advanced Search", "?m=advanced"),
                            ])
        if permcheck(db, uid, "admin"):
            retval += format("H2", "Admin")
            retval += makemenu([
                                ("SQL Search", "?m=query"),
                                ])
        retval += format("H2", "Your Library")
        retval += makemenu([("View recent",
                             "?" + urllib.urlencode([("u", uid),
                                                     ("f", "last_name"),
                                                     ("q", "''"),
                                                     ("o", "datetime"),
                                                     ("m", "search")])
                             ),
                            ])
        if permcheck(db, uid, "add"):
            retval += makemenu([
                ("Manual entry", "?m=manual"),
                ("Make bookmark", "?m=bookmark"),
                ("Import from Bibtex", "?m=import&format=bibtex"),
                ("Import from Endnote", "?m=import&format=endnote"),
                                ])
    else:
        query = "&" + urllib.urlencode([("f", form.getvalue("f")),
                                        ("o", form.getvalue("o")),
                                        ("q", form.getvalue("q")),
                                        ("u", form.getvalue("u")),
                                        ("start", form.getvalue("start")),
                                        ("page", form.getvalue("page")),
                                        ("bibid", form.getvalue("bibid")),
                                        ("m", "search")])
        retval += makemenu([("Home", "?m=home"),
                            ("Log in", "?login=1" + query),
                            ("Advanced Search", "?m=advanced"),
                            ])
    retval += "</div>" # close navleft
    retval += """<div class="content">"""
    retval += """<div class="thead">"""
    retval += h3(title)
    retval += """</div> <!-- thead -->"""
#    retval += """<div class="contentonly">"""
    return retval

def makeAccount(db, name, password, perms):
    db.q("""insert into accounts (username, password, session, perms) values (%s, %s, %s, %s);""",
         name,
         encrypt(password),
         makeSessionId(),
         perms)

def makemenu(items):
    retval = ""
    retval += "<ul>"
    for item in items:
        if type(item) == type( (1,) ):
            (text, link) = item
            if link == '':
                retval += """<LI><span class="black">%s</span></LI>""" % text
            else:
                retval += button(text, link)
        else:
            retval += makemenu(item)
    retval += "</ul>"
    return retval

def make_tag_size(n, counts):
    # return font sizes 10 to 20
    # based on counts (biggest to smallest)
    position = 10 - (10 * (float(counts.index(n)) / (len(counts) - 1)))
    return int(max(min(position, 10), 0) + 10)

def footer(db):
    # </div> <!-- contentonly -->
    retval = ""
    retval += """
</div> <!-- content -->
<div id="navright">
<H2>Tags</H2>
"""
    tags = {}
    data = db.q("select tags from bibliography;")
    for row in data:
        if row["tags"]:
            words = row["tags"].split(",")
            for word in words:
                word = word.strip().lower()
                tags[word] = tags.get(word, 0) + 1
    counts = list(set(tags.values()))
    counts.sort()
    counts.reverse()
    alltags = tags.keys()
    alltags.sort()
    for tag in alltags:
        retval += link(tag.replace(" ", "&nbsp;"),
                       "?" + urllib.urlencode([("f", "tag"),
                                               ("q", tag),
                                               ("o", "datetime"),
                                               ("m", "search")]),
                       CLASS="tag",
                       style=("font-size: %dpx; text-decoration: none;" %
                              make_tag_size(tags[tag], counts)),
                       rel="tag" ) + "\n"
    retval += p()
    # --------------------------------------------------------------------
    retval += "<HR><H2>Authors and Editors</H2>"
    names = {}
    data = db.q("select author, editor from bibliography;")
    for row in data:
        if row["author"]:
            words = row["author"].split(" and ")
            for word in words:
                word = word.strip()
                if word.count(",") > 0:
                    last, first = word.split(",", 1)
                elif " " in word:
                    first, last = word.split(" ", 1)
                else:
                    first = word
                    last = ""
                last = last.strip()
                first = first.strip()
                if len(first) > 0:
                    searchname = last + ", " + first[0]
                else:
                    searchname = word
                names[searchname] = names.get(searchname, 0) + 1
#         if row["editor"]:
#             words = row["editor"].split(" and ")
#             for word in words:
#                 word = word.strip()
#                 if word.count(",") > 0:
#                     last, first = word.split(",", 1)
#                 else:
#                     last, first = word.split(" ", 1)
#                 last = last.strip()
#                 first = first.strip()
#                 if len(first) > 0:
#                     searchname = last + ", " + first[0]
#                 else:
#                     searchname = word
#                 names[searchname] = names.get(searchname, 0) + 1
    counts = list(set(names.values()))
    counts.sort()
    counts.reverse()
    allnames = names.keys()
    allnames.sort()
    for name in allnames:
        if names[name] < 2: continue
        if "," in name:
            searchname, first = name.split(",", 1)
        else:
            searchname = name
        retval += link(searchname.replace(" ", "&nbsp;"),
                       "?" + urllib.urlencode([("f", "last_name"),
                                               ("q", name),
                                               ("o", "datetime"),
                                               ("m", "search")]),
                       CLASS="tag",
                       style=("font-size: %dpx; text-decoration: none;" %
                              make_tag_size(names[name], counts)),
                       rel="tag" ) + "\n"
    retval += p()
    retval += """
</div>
</BODY>
</HTML>
""" 
    return retval

def format_match(s):
    url_string = s.group()
    if "/" in url_string[7:]:
        path, filename = url_string.rsplit("/", 1)
        return link(filename, url_string)
    else:
        return link(url_string, url_string)

def format_html(s):
    return re.sub("(http://[^ ]*)", format_match, s)
    
def format(tag, msg = "", endIt = 1, **args):
    argList = ""
    if tag.lower() == "input": endIt = 0
    if args:
        for key in args:
            if args[key] != "":
                argList += """ %s="%s" """ % (key, args[key])
    retval = "<%s%s>%s" % (tag, argList, msg)
    if " " in tag:
        justTag, rest = tag.split(" ", 1)
    else:
        justTag = tag
    if endIt: retval += "</%s>" % (justTag, )
    return retval

f = format

def begin(tag, **args):
    return format(tag, endIt = 0, **args)

def end(tag):
    return "</%s>" % tag

def h3(msg):
    return format("H3", msg, style="color: #6392e4;")

def h2(msg):
    return format("H2", msg, style="color: #6392e4;")

def h1(msg):
    return format("H1", msg)

def bold(msg):
    return format("B", msg)

def italics(msg):
    return format("I", msg)

def p(msg=''):
    return format("P", msg)

def br():
    return "<BR />"

def makeComparison(*args):
    """ cmp maker for multi-field comparisons for use with sort """
    def compare(a, b, index = 0):
        if index == len(args):
            return 0
        colName = args[index]
        if colName[0] == "-":
            colName = colName[1:]
            c = -cmp(a[colName], b[colName])
        else:
            c = cmp(a[colName], b[colName])
        if c == 0:
            return compare(a, b, index + 1)
        else:
            return c
    return compare

def makeList(data, headers = None, sort = None):
    """ Takes a list of cols, and list of dicts
    (optional: list of fields to sort by) """
    if sort:
        data.sort(makeComparison(*sort))
    retval = ""
    for row in data:
        if headers:
            rowdata = ""
            for h in headers:
                if h in row:
                    rowdata += "%s" % row[h]
            retval += format("LI", rowdata)
        else: # data is not a dict
            rowdata = "%s" % row
            retval += format("LI", rowdata)
    return retval

def makeQList(q, sort = None, cols = None):
    """ Take a db query and list it """
    if cols == None:
        cols = q.listfields()
    return makeList(q.dictresult(), cols, sort=sort)

def makeTable(tableName, data, headers, sort = None, altHeaders = {},
              border = 1, width=None, showHeaders=1, groupBy = None,
              formats = []):
    """ Takes a list of cols, and list of dicts """
    retval = ""
    if showHeaders:
        for h in headers:
            if h in altHeaders:
                h = altHeaders[h]
            retval += format("TH", h)
        retval = format("TR", retval)
    if sort:
        data.sort(makeComparison(*sort))
    lastGroup = None
    for row in data:
        rowdata = ""
        # replace items with formatting options
        replace(tableName, row, formats)
        if groupBy != None and lastGroup != row[groupBy]:
            rowdata += format("TR", format("TH colspan=%d" % len(headers),
                                           row[groupBy]))
            lastGroup = row[groupBy]
        for h in headers:
            if h in row:
                if type(row[h]) == type("") and len(row[h]) > 1 and row[h][-1] == ":":
                    rowdata += format("TD", row[h], align="right")
                else:
                    if not isempty(row[h]):
                        rowdata += format("TD", row[h])
                    else:
                        rowdata += format("TD", "&nbsp;")
            else:
                rowdata += format("TD")
        retval += format("TR", rowdata)
    if width:
        return format("TABLE", retval, border = border, width = width)
    else:
        return format("TABLE", retval, border = border)

def list2dict(mylist):
    retval = {}
    for dict in mylist:
        retval[dict["field"].strip()] = dict["value"].strip()
    return retval 

def removeQuotes(data):
    if len(data) < 2: return
    if data[0] == "\"" and data[-1] == "\"":
        data = data[1:-1]
    if data[0] == "{" and data[-1] == "}":
        data = data[1:-1]
    if data[0] == "\"" and data[-1] == "\"":
        data = data[1:-1]
    return data.replace("}","").replace("{","")

def makeQTable(q, sort = None, border = 1, altHeaders = {}, cols = None, width = None):
    """ Turns a query into an HTML table """
    if cols == None:
        cols = q.listfields()
    return makeTable(q.dictresult(), cols, sort=sort, altHeaders=altHeaders, border=border, width=width)

def button(text, address):
    return "<LI>%s</LI>" % link(text, address)

def submit(value, name="submit"):
    return input(name, value, "SUBMIT")

def hidden(name, value):
    return input(name, value, "HIDDEN")

def link(text, address, CLASS="", style="", rel="", target=""):
    if (address.startswith("http://") or
        address.startswith("mailto:") or
        address.startswith("#") or
        address.startswith("javascript:")):
        return format('A', text, href=address, CLASS=CLASS, style=style, rel=rel, target=target)
    else:
        return format("A", text, href=URLPATH + address, CLASS=CLASS, style=style, rel=rel, target=target)

def picklist(name, default, options, blankOption = 0, deleteOption = 0, onChange = None):
    retval = """<SELECT name="%s" """ % name
    if onChange != None:
        retval += "onChange=%s " % onChange
    retval += ">"
    if blankOption:
        retval += '<OPTION value="NONE"></OPTION>'
    for (value, text) in options:
        retval += '<OPTION '
        if str(value) == str(default):
            retval += 'SELECTED '
        retval += 'value="%s">%s</OPTION>' % (value, text)
    if deleteOption:
        retval += '<OPTION value="DELETE">DELETE</OPTION>'
    retval += '</SELECT>'
    return retval 

def yesnoPicklist(name, default):
    return picklist(name, default, (('t', 'Yes'), ('f', 'No')),
                    blankOption=0, deleteOption=0)

def yesnoCheckbox(name, default):
    return checkbox() + checkbox()

def checkbox(name, text, value = ""):
    """ value is checked or empty """
    return "<INPUT TYPE=CHECKBOX " + value + (" NAME=%s" % name) + ">" + text + "</INPUT>"

def input(name, value="", type="TEXT", **args): # type can be SUBMIT, HIDDEN, CHECKBOX, or TEXT
    if value == None:
        value = ""
    if type == "TEXTAREA":
        return format("TEXTAREA", value, name=name, **args)
    else:
        return format("INPUT", "", type=type, name=name, value=value, **args)

def tr(data, **args):
    return format("TR", data, **args)
def td(data = '', **args):
    return format("TD", data, **args)
def hr(**args):
    return begin("HR", **args)
def tdr(data = '', **args):
    return format("TD", data, align="right", **args)
def tdc(data = '', **args):
    return format("TD", data, align="center", **args)

def stime2fptime(s):
    """ Converts 'YYYY-MM-DD HH:MM' to float time """
    if type(s) != type(''): return 0.0
    if s.strip() == '': return 0.0
    return time.mktime(time.strptime(s, "%Y-%m-%d %H:%M"))

def fptime2stime(fp):
    """ Converts float time to 'YYYY-MM-DD HH:MM' """
    if type(fp) not in [type(0), type(0.0)]: return ""
    if fp == 0.0: return ""
    return time.strftime("%Y-%m-%d %H:%M", time.localtime(fp))

def require(form, db, uid, check):
    if not permcheck(db, uid, check):
        error(form, db, uid, "User does not have permission to %s" % check)
        print footer(db)
        sys.exit()

def permcheck(db, uid, check):
    user = db.q("select * from accounts where username = %s", uid)
    if len(user) != 1: return 0
    permissions = user[0]["perms"] # [admin] [edit] [add] [del]
    if type(permissions) != str: return 0
    if "[" + check + "]" in permissions:
        return 1
    if "[admin]" in permissions:
        return 1
    return 0

def bibtex_fields():
    return "abstract address affiliation annote author booktitle chapter contents copyright crossref doi edition editor howpublished institution ISBN ISSN journal key keywords language LCCN location month mrnumber note number organization pages price publisher school series size title type url volume year citeulike-article-id priority bib_type bib_key start end"

def extra_fields():
    return "affiliation annote contents copyright crossref ISBN ISSN keywords language LCCN location mrnumber number price size url citeulike-article-id"

def import_filename_bibtex(form, db, uid):
    print begin("FORM", name="MAIN", method="POST",
                action=URLPATH,
                enctype="multipart/form-data")
    print begin("TABLE")
    print tr(tdc("Select a local file to upload", colspan=2))
    print tr(tdr(bold("Bibtex file:")) + td(input("filename", "", size=50, type="file")))
    print tr(tdc("OR Enter Bibtex data", colspan=2))
    print tr(tdr(bold("Bibtex data:")) +
             td(input("data", "", "TEXTAREA", rows=20, cols=60)))
    print tr(td() + td(submit("Upload")))
    print end("table")
    print hidden("m", "upload")
    print hidden("format", "bibtex")
    print end("FORM")
    

def import_filename_endnote(form, db, uid):
    print begin("FORM", name="MAIN", method="POST",
                action=URLPATH,
                enctype="multipart/form-data")
    print begin("TABLE")
    print tr(tdc("Select a local file to upload", colspan=2))
    print tr(tdr(bold("Endnote file:")) + td(input("filename", "", size=50, type="file")))
    print tr(tdc("OR Enter Endnote data", colspan=2))
    print tr(tdr(bold("Endnote data:")) +
             td(input("data", "", "TEXTAREA", rows=20, cols=60)))
    print tr(td() + td(submit("Upload")))
    print end("table")
    print hidden("m", "upload")
    print hidden("format", "endnote")
    print end("FORM")

def cleanName(name):
    """ Clean up a name """
    name = name.replace("""\n""", " ")
    name = name.replace("""\r""", " ")
    name = name.replace("""""", " ")
    while name.count("  ") != 0:
        name = name.replace("  ", " ")
    name = name.replace(" Jr,", " Jr.,")
    name = name.replace(" Sr,", " Sr.,")
    return name.strip()

def splitName(name):
    name = cleanName(name)
    if name.count(" ") == 0:
        return name, ""
    elif name.count(" ") == 1: # just two words
        first, last = name.split(" ")
        return last, first
    else: # more than two words
        # Find the spaces that should not break
        # and replace them with tildes, then
        # break on first space from right.
        # van, von, de, la are prefixes
        # jr sr are suffixes
        name = "[" + name + "]"
        name = name.replace(" de ", " de~")
        name = name.replace(" da ", " da~")
        name = name.replace(" di ", " di~")
        name = name.replace(" le ", " le~")
        name = name.replace(" la ", " la~")
        name = name.replace(" von ", " von~")
        name = name.replace(" van ", " van~")
        name = name.replace(" der ", " der~")
        name = name.replace(" Jr.]", "~Jr.]")
        name = name.replace(" Sr.]", "~Sr.]")
        name = name.replace(" Jr]", "~Jr.]")
        name = name.replace(" Sr]", "~Sr.]")
        name = name[1:-1]
        first, last = name.rsplit(" ", 1)
        last = last.replace("~", " ")
        first = first.replace("~", " ")
        return last, first

def endnote_fields():
    return "A2 AD AU EP ER ID IS JF KW N2 PB PY SN SP T2 TI TY UR VL"
# TY  - CONF bib_type inproceedings
# ID  - citeulike:323018 bib_key
# N2  - Acquiring abstract
# AD/cy  - San Francisco address
# EP  - 120 end
# PB  - Morgan publisher
# SP  - 109 start
# T2  - KR2000: Principles of Knowledge Representation and Reasoning booktitle
# TI  - Representing the Knowledge of a Robot title
# KW  - depth keywords
# AU/A1  - Thielscher, Michael author
# A2  - Cohn, Anthony editor
# A2  - Giunchiglia, Fausto editor
# A2  - Selman, Bart editor
# PY/Y1  - 2000/// publish year
# UR/DP  - http://citeseer.ist.psu.edu/thielscher00representing.html url
# SN  - doi?
# ER  - end record

# %H     Header commentary which is printed before the reference. 
# %A     Author's name. First Last, Jr.
# %Q     Corporate author. 
# %T     Title of the article or book. 
# %S     Title of the series. 
# %J     Journal containing the article. 
# %B     Book containing article. 
# %R     Report, paper, or thesis type. 
# %V     Volume. 
# %N     Number with volume. 
# %E     Editor of book containing article. 
# %P     Page number(s). 
# %I     Issuer. This is the publisher. 
# %C     City where published. This is the publishers address. 
# %D     Date of publication. Year Month-name
# %O     Other information which is printed after the reference. 
# %K     Keywords used by refer to help locate the reference. 
# %L     Label used to number references when the -k flag of refer is used. 
# %X     Abstract. This is not normally printed in a reference.

def import_endnote(form, db, uid, filename="endnote.ris", udir="./tmp"):
    retval = save_uploaded_file(form, "filename", filename=filename, upload_dir=udir)
    if retval:
        fp = file(udir + "/" + filename)
        import_endnote_fileobject(form, db, uid, fp)
    if form.has_key('data'):
        data = form.getvalue("data")
        if data != None:
            data = data.strip()
            if data != '':
                fp = StringIO.StringIO(data)
                import_endnote_fileobject(form, db, uid, fp)

def endnoteTypeToBibtex(stuff):
    stuff = stuff.upper()
    if stuff.startswith("JOUR"):   # (journal reference)
        return "article"
    elif stuff.startswith("CHAP"): # (book chapter reference)
        return "inbook"
    elif stuff.startswith("ABST"): # (abstract reference)
        return "abstract"
    elif stuff.startswith("ADVS"): # (audiovisual material)
        return "misc"
    elif stuff.startswith("ART"): # (art work)
        return "art"
    elif stuff.startswith("BILL"): # (bill/resolution)
        return "misc"
    elif stuff.startswith("BOOK"): # (whole book reference)
        return "book"
    elif stuff.startswith("CASE"): # (case)
        return "misc"
    elif stuff.startswith("COMP"): # (computer program)
        return ""
    elif stuff.startswith("CONF"): # (conference proceeding)
        return "inproceedings"
    elif stuff.startswith("CTLG"): # (catalog)
        return "misc"
    elif stuff.startswith("DATA"): # (data file)
        return "data"
    elif stuff.startswith("ELEC"): # (electronic citation)
        return "electronic"
    elif stuff.startswith("GEN"): # (generic)
        return "misc"
    elif stuff.startswith("ICOMM"): # (internet communication)
        return "electronic"
    elif stuff.startswith("INPR"): # (in press reference)
        return "press"
    elif stuff.startswith("JFULL"): # (journal - full)
        return "proceedings"
    elif stuff.startswith("MAP"): # (map)
        return "map"
    elif stuff.startswith("MGZN"): # (magazine article)
        return "article"
    elif stuff.startswith("MPCT"): # (motion picture)
        return "video"
    elif stuff.startswith("MUSIC"): # (music score)
        return "misc"
    elif stuff.startswith("NEWS"): # (newspaper)
        return "press"
    elif stuff.startswith("PAMP"): # (pamphlet)
        return "booklet"
    elif stuff.startswith("PAT"): # (patent)
        return "patent"
    elif stuff.startswith("PCOMM"): # (personal communication)
        return "misc"
    elif stuff.startswith("RPRT"): # (report)
        return "technicalreport"
    elif stuff.startswith("SER"): # (serial - book, monograph)
        return "book"
    elif stuff.startswith("SLIDE"): # (slide)
        return "misc"
    elif stuff.startswith("SOUND"): # (sound recording)
        return "misc"
    elif stuff.startswith("STAT"): # (statute)
        return "misc"
    elif stuff.startswith("THES"): # (thesis/dissertation)
        return "phdthesis"
    elif stuff.startswith("UNBILL"): # (unenacted bill/resolution)
        return "misc"
    elif stuff.startswith("UNPB"): # (unpublished work reference)
        return "unpublished"
    elif stuff.startswith("VIDEO"): # (video recording)
        return "misc"

#     [["book", "Book"],
#      ["inbook", "Book chapter/section"],
#      ["booklet", "Booklet"],
#      ["incollection", "Book part (with own title)"],
#      ["inproceedings", "Conference proceedings (article)"],
#      ["proceedings", "Conference proceedings (whole)"],
#      ["electronic", "Electronic citation"],
#      ["press", "In the press"],
#      ["article", "Journal article"],
#      ["manual", "Manual (technical documentation)"],
#      ["misc", "Miscellaneous"],
#      ["techreport", "Technical report"],
#      ["technical-standard", "Technical Standard"], #?
#      ["mastersthesis", "Thesis (Master's)"],
#      ["phdthesis", "Thesis (PhD)"],
#      ["unpublished", "Unpublished work"],]

def endnote_type(s):
    s = s.strip().lower()
    if s == "journal article":
        return "article"
    elif s == "conference article":
        return "inproceedings"
    elif s == "technical report":
        return "techreport"
    elif s == "patent":
        return "patent"
    elif s == "workshop":
        return "workshop"
    elif s == "book":
        return "book"
    return "misc"

def import_endnote_fileobject(form, db, uid, fp):
    # FIX: remove HTML in titles, abstract, etc
    # get ready
    fields = endnote_fields().split(' ')
    # parse file
    records = []
    current = []
    blankLineSignalsEnd = 0
    for line in fp:
        line = line.strip()
        line = line.replace("""\n""", " ")
        line = line.replace("""\r""", " ")
        line = line.replace("""""", " ")
        if line == '':
            if blankLineSignalsEnd:
                if len(current) > 0:
                    records.append(current)
                    current = []
            continue
        if line.startswith("%"):
            key, value = line[:2], line[3:]
            current.append( (key, value) )
            blankLineSignalsEnd = 1
            continue
        elif line.startswith('ER  -'): # end record
            if len(current) > 0:
                records.append(current)
                current = []
            continue
        try:
            key, value = line.split("  - ")
        except:
            continue
        current.append( (key, value) )
    if len(current) > 0:
        records.append(current)
    for rec in records: 
        data = []
        for pair in rec:
            field, value = pair
            field = field.strip().upper()
            if field == "AU" or field == "A2":
                if "," in value:
                    last, first = value.split(",", 1)
                else:
                    last, first = value, ""
                data.append((field, (last.strip(), first.strip())))
            else: # other than author/editor
                data.append((field, value))
        # Continuing on with a Record
        if len(data) > 0:
            data_dict = {}
            for item in data:
                field, stuff = item
                #print "field = '%s'; value = '%s'<br>" % (field, stuff)
                if type(stuff) == tuple:
                    stuff = stuff[0] + ", " + stuff[1]
                if field.startswith("%"):
                    if field[1] == "H": # Header commentary
                        pass
                    elif field[1] == "A": #     Author's name. First Last, Jr.
                        if "," in stuff:
                            last, first = stuff.split(",")
                        else:
                            last, first = splitName(stuff)
                        if data_dict.get("AU",''):
                            data_dict["AU"] += " and "
                        data_dict["AU"] = data_dict.get("AU",'') + last + ", " + first
                    elif field[1] == "Q": #     Corporate author.
                        last, first = splitName(stuff)
                        if data_dict.get("AU",''):
                            data_dict["AU"] += " and "
                        data_dict["AU"] = data_dict.get("AU",'') + last + ", " + first
                    elif field[1] == "T": #     Title of the article or book.
                        data_dict["TI"] = stuff
                    elif field[1] == "S": #     Title of the series.
                        data_dict["series"] = stuff
                    elif field[1] == "J": #     Journal containing the article.
                        data_dict["JF"] = stuff
                    elif field[1] == "B": #     Book containing article.
                        data_dict["T2"] = stuff
                    elif field[1] == "R": #     Report, paper, or thesis type.
                        data_dict["UR"] = stuff
                    elif field[1] == "V": #     Volume.
                        data_dict["VL"] = stuff
                    elif field[1] == "N": #     Number with volume.
                        data_dict["issue"] = stuff
                    elif field[1] == "#": #     Number with volume.
                        data_dict["doi"] = stuff
                    elif field[1] == "E": #     Editor of book containing article.
                        last, first = splitName(stuff)
                        if data_dict.get("A2",''):
                            data_dict["A2"] += " and "
                        data_dict["A2"] = data_dict.get("A2",'') + last + ", " + first
                    elif field[1] == "P": #     Page number(s).
                        data_dict["start_page"] = stuff
                    elif field[1] == "I": #     Issuer. This is the publisher.
                        data_dict["publisher"] = stuff
                    elif field[1] == "C": #     City where published. 
                        data_dict["address"] = stuff
                    elif field[1] == "D": #     Date of publication. Month Year
                        month, year, day = "", "", ""
                        if " " in stuff:
                            month, year = stuff.split(" ", 1)
                        else:
                            year = stuff
                        data_dict["PY"] = year + "/" + month
                    elif field[1] == "0": #     Type (zero)
                        data_dict["TY"] = endnote_type(stuff)
                    elif field[1] == "K": #     Keywords used by refer 
                        data_dict["tags"] = stuff
                    elif field[1] == "L": #     Label used to number 
                        data_dict["ID"] = stuff
                    elif field[1] == "1": #     Label used to number 
                        data_dict["ID"] = stuff
                    elif field[1] == "X": #     Abstract. 
                        data_dict["abstract"] = stuff
                elif field not in fields:
                    print "Unknown field: %s<br>" % field
                elif field in data_dict:
                    # make sure it is one that can have multis
                    # or handle special:
                    if field in ["AU", "A2"]:
                        data_dict[field] += " and " + stuff
                    elif field == "KW":
                        data_dict[field] += ", " + stuff
                    else:
                        print p("ignoring repeated field '%s'" % field)
                elif field == "TY":
                    data_dict[field] = endnoteTypeToBibtex(stuff)
                else:
                    data_dict[field] = stuff
                #print p()
                #for field in data_dict:
                #    print bold(field), ":", data_dict[field], br()
                year_data = data_dict.get("PY", "///")
                if year_data.count("/") == 3:
                    year, month, day, t = year_data.split("/", 3)
                elif year_data.count("/") == 2:
                    year, month, day = year_data.split("/", 2)
                elif year_data.count("/") == 1:
                    year, month = year_data.split("/", 1)
            # first check to see if it is already in:
            title = data_dict.get("TI", '')
            bib_type = data_dict.get("TY", '')
            if title and bib_type:
                check_dup = db.q("""select * from bibliography where title = %s and bib_type = %s;""", title, bib_type)
                if len(check_dup) > 0:
                    print p("Error: A record with this title and type already exists: '%s' %s" % (title, bib_type))
                    continue
            db.query("""insert into bibliography (
                        bib_type,
                        bib_key,
                        title,
                        datetime,
                        author,
                        editor,
                        journal,
                        volume,
                        issue,
                        chapter,
                        edition,
                        start,
                        end,
                        year,
                        month,
                        day,
                        date_other,
                        book_title,
                        how_published,
                        institution,
                        organization,
                        publisher,
                        address,
                        school,
                        series,
                        abstract,
                        doi,
                        tags,
                        priority,
                        user_id,
                        url
                        ) VALUES (
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s
                        );""",
                     data_dict.get("TY", ''),
                     data_dict.get("ID", ''),
                     data_dict.get("TI", ''),
                     data_dict.get("datetime", fptime2stime(time.time())),
                     data_dict.get("AU", ''),
                     data_dict.get("A2", ''),
                     data_dict.get("JF", ''),
                     data_dict.get("VL", ''),
                     data_dict.get("IS", ''),
                     data_dict.get("chapter", ''),
                     data_dict.get("edition", ''),
                     data_dict.get("SP", ''),
                     data_dict.get("EP", ''),
                     year,
                     month,
                     day,
                     '',
                     data_dict.get("T2", ''),
                     data_dict.get("howpublished", ''),
                     data_dict.get("institution", ''),
                     data_dict.get("organization", ''),
                     data_dict.get("publisher", ''),
                     data_dict.get("AD", ''),
                     data_dict.get("school", ''),
                     data_dict.get("series", ''),
                     data_dict.get("N2", ''),
                     data_dict.get("doi", ''),
                     data_dict.get("tags", ''),
                     data_dict.get("priority", 0),
                     uid,
                     data_dict.get("UR", ''),
                     )
            bib_id = db.db.insert_id()
            if data_dict.get("note", None) != None:
                db.q("""insert into note (bib_id, note, public) VALUES (
                     %s,
                     %s,
                     %s
                     );""",
                     bib_id,
                     data_dict.get("note", ''),
                     1
                     )
                    #try:
                    #    print bold(field) + ": " + unicode(stuff) + br()
                    #except UnicodeEncodeError:
                    #    print bold(field) + " (unicode error)" + br()
            view_record(db, bib_id)
    # show upload

def import_bibtex(form, db, uid, filename="bibtex.bib", udir="./tmp"):
    retval = save_uploaded_file(form, "filename", filename=filename, upload_dir=udir)
    if retval:
        fp = open(udir + "/" + filename, "r")
        import_bibtex_fileobject(form, db, uid, fp)
    if form.has_key('data'):
        data = form.getvalue("data")
        if data != None:
            data = data.strip()
            if data != '':
                fp = StringIO.StringIO(data)
                import_bibtex_fileobject(form, db, uid, fp)

def import_bibtex_fileobject(form, db, uid, fp):
    from Pyblio.Parsers.Syntax.BibTeX.Parser import read, Comment, Record, Join, Block, ATComment
    from Pyblio.Exceptions import ParserError
    fields = [word.lower() for word in bibtex_fields().split()]
    try:
        records = read(fp)
    except ParserError, err:
        print "ERROR: %s at line %d: %s" % (file, err[0], err[1])
        return
    for rec in records: # each rec is a Comment, ATComment, or Record
        data = []
        if type(rec) == Comment:
            #print bold("comment: "), rec, br()
            continue
        elif type(rec) == ATComment:
            #print bold("ATcomment: "), rec, br()
            continue
        elif type(rec) == Record:
            data.append(("bib_type", rec.type))
            data.append(("bib_key", rec.key))
            for pair in rec.fields:
                field, value = pair
                field = field.strip().lower()
                if type(value) == Join:
                    for part in value:
                        if type(part) == Block:
                            if field == "author" or field == "editor":
                                part = part.flat()
                                part = part.replace("""\n""", " ")
                                part = part.replace("""\r""", " ")
                                part = part.replace("""""", " ")
                                names = part.split(" and ")
                                for name in names:
                                    if name.count(",") == 0:
                                        data.append((field, splitName(name)))
                                    elif name.count(",") == 1:
                                        name = cleanName(name)
                                        last, first = name.split(",", 1)
                                        data.append((field, (last.strip(), first.strip())))
                                    else: # more than 1 comma
                                        people = name.split(",")
                                        for peep in people:
                                            data.append((field, splitName(peep)))
                            elif field == "pages":
                                part = part.flat()
                                part = part.replace("--", "-")
                                if "-" in part:
                                    start, end = part.split("-", 1)
                                else:
                                    start, end = part, ""
                                start = start.strip()
                                end = end.strip()
                                data.append(("start", start))
                                data.append(("end", end))
                            else: # other than author/editor, or page
                                data.append((field, part.flat()))
                        else: # other than block
                            data.append((field, part.flat()))
                else: # other than join
                    data.append((field, value.flat()))
        else:
            print bold("Unknown record type: "), type(rec), rec, br()
            continue
        # Continuing on with a Record
        if len(data) > 0:
            data_dict = {}
            for item in data:
                field, stuff = item
                if type(stuff) == tuple:
                    stuff = stuff[0] + unicode(", ") + stuff[1]
                if field not in fields:
                    print "Unknown field: %s<br>" % field
                else:
                    if field in data_dict:
                        # make sure it is one that can have multis
                        data_dict[field] += " and " + unicode(stuff)
                    else:
                        data_dict[field] = unicode(stuff)
            #print p()
            #for field in data_dict:
            #    print bold(field), ":", data_dict[field], br()
            # first check to see if it is already in:
            title = data_dict.get("title", '')
            bib_type = data_dict.get("bib_type", '')
            if title and bib_type:
                check_dup = db.q("""select * from bibliography where title = %s and bib_type = %s;""", title, bib_type)
                if len(check_dup) > 0:
                    print p("Error: A record with this title and type already exists: '%s' %s" % (title, bib_type))
                    continue
            db.query("""insert into bibliography (
                        bib_type,
                        bib_key,
                        title,
                        datetime,
                        author,
                        editor,
                        journal,
                        volume,
                        issue,
                        chapter,
                        edition,
                        start,
                        end,
                        year,
                        month,
                        day,
                        date_other,
                        book_title,
                        how_published,
                        institution,
                        organization,
                        publisher,
                        address,
                        school,
                        series,
                        abstract,
                        doi,
                        tags,
                        priority,
                        user_id,
                        url
                        ) VALUES (
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s
                        );""",
                     data_dict.get("bib_type", ''),
                     data_dict.get("bib_key", ''),
                     data_dict.get("title", ''),
                     data_dict.get("datetime", fptime2stime(time.time())),
                     data_dict.get("author", ''),
                     data_dict.get("editor", ''),
                     data_dict.get("journal", ''),
                     data_dict.get("volume", ''),
                     data_dict.get("issue", ''),
                     data_dict.get("chapter", ''),
                     data_dict.get("edition", ''),
                     data_dict.get("start", ''),
                     data_dict.get("end", ''),
                     data_dict.get("year", None),
                     data_dict.get("month", None),
                     data_dict.get("day", ''),
                     data_dict.get("date_other", ''),
                     data_dict.get("booktitle", ''),
                     data_dict.get("howpublished", ''),
                     data_dict.get("institution", ''),
                     data_dict.get("organization", ''),
                     data_dict.get("publisher", ''),
                     data_dict.get("address", ''),
                     data_dict.get("school", ''),
                     data_dict.get("series", ''),
                     data_dict.get("abstract", ''),
                     data_dict.get("doi", ''),
                     data_dict.get("tags", ''),
                     data_dict.get("priority", 0),
                     uid,
                     data_dict.get("url", ""),
                     )
            bib_id = db.db.insert_id()
            if data_dict.get("note", None) != None:
                db.q("""insert into note (bib_id, note, public) VALUES (
                     %s,
                     %s,
                     %s
                     );""",
                     bib_id,
                     data_dict.get("note", ''),
                     1
                     )
                    #try:
                    #    print bold(field) + ": " + unicode(stuff) + br()
                    #except UnicodeEncodeError:
                    #    print bold(field) + " (unicode error)" + br()
            view_record(db, bib_id)
    # show upload

def next_prev_buttons(page, query, total_recs):
    """
    page - what page are we on
    query - the query that lead us here
    total_rec - total matching
    """
    if total_recs <= RECORDS_PER_PAGE: return
    if page > 1:
        print link("<< Previous", query + "&page=%d" % (page - 1),
                   style="text-decoration: none")
    else:
        print "<< Previous"
    print " | Page: "
    startpage = ((page - 1)/ 10) * 10 + 1
    lastpage = (total_recs / RECORDS_PER_PAGE) 
    pages = min( lastpage + 2, startpage + 10)
    if startpage > 1:
        print link("1", query + ("&page=1")) + " ... "
    for pa in range(startpage, pages):
        if pa != (page):
            print link(str(pa), query + ("&page=%d" % (pa))) 
        else:
            print bold("%s" % pa)
        print "&nbsp;"
    if (pages - 1)  < lastpage:
        print "... " + link("%s" % (lastpage + 1), query + ("&page=%d" % (lastpage + 1)))
    print " | "
    if (page) * RECORDS_PER_PAGE < total_recs:
        print link("Next >>", query + "&page=%d" % (page + 1),
                   style="text-decoration: none")
    else:
        print "Next >>"

def view_records(form, db, title, data, page, total_recs, query):
    if total_recs > 0:
        heading = "<BR>Page %d of %d; total records: %d" % (
            page,
            int(math.ceil(float(total_recs)/RECORDS_PER_PAGE)),
            total_recs,
            )
        print h2(title + heading)
        start = ((page - 1) * RECORDS_PER_PAGE) + 1
        print begin("OL", start=start)
        if total_recs > 1:
            # particular order?
            orderedby = re.match(".*[\b\&\?]o=(\w*)", query)
            if orderedby != None:
                orderedby = orderedby.groups()[0]
            # remove orderbys
            squery =  query.replace("o=datetime&", "")
            squery = squery.replace("o=datetime", "")
            squery = squery.replace("o=title&", "")
            squery = squery.replace("o=title", "")
            squery = squery.replace("o=author&", "")
            squery = squery.replace("o=author", "")
            heading = ""
            if orderedby != "datetime":
                heading += ("[ " +
                            link("Order by Posted Date", squery + "&o=datetime") +
                            " ]")
            else:
                heading += "[ Order by Posted Date ]"
            if orderedby != "title":
                heading += ("[ " +
                            link("Order by Title", squery + "&o=title") +
                            " ]")
            else:
                heading += "[ Order by Title ]"
            if orderedby != "author":
                heading += ("[ " +
                            link("Order by First Author", squery + "&o=author") +
                            " ]")
            else:
                heading += "[ Order by First Author ]"
            print heading + br()
            if orderedby in ["title", "author"] and total_recs > RECORDS_PER_PAGE * 2:
                letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                retval = ""
                for letter in letters:
                    retval += (link(letter,
                                    squery +
                                    ("&start=%s&o=%s" % (letter, orderedby)))
                               + " ")
                print retval + br()
        if form.has_key("start"):
            squery = query.replace("m=search", "start=" + form.getvalue("start"))
        elif form.has_key("page"):
            squery = query.replace("m=search", "page=" + form.getvalue("page"))
        else:
            squery = query.replace("m=search", "")
        for row in data:
            print format("LI", format_record(row, "&" + squery[1:]))
        print end("OL")
        next_prev_buttons(page, query, total_recs)
    else:
        print h2(title)
        print p("No records were found that match.")

def format_names(names):
    retval = ""
    names = names.split(" and ")
    for name in names:
        if name.count(",") > 0:
            last, first = name.split(",", 1)
        else:
            last, first = name.split(" ", 1)
        last = last.strip()
        first = first.strip()
        if retval != "":
            retval += ", "
        if len(first) > 0:
            searchname = last + ", " + first[0]
        else:
            searchname = name
        retval += link("%s %s" % (first, last),
                       "?" + urllib.urlencode([("f", "last_name"),
                                               ("q", searchname),
                                               ("m", "search")]))
    return retval

def cleanup_names(names):
    retval = ""
    names = names.split(" and ")
    for name in names:
        name = name.strip()
        name = name.replace("""\n""", " ")
        name = name.replace("""\r""", " ")
        name = name.replace("""""", " ")
        if name.count(",") > 0:
            last, first = name.split(",", 1)
        elif " " in name:
            first, last = name.split(" ", 1)
        else:
            first, last = "", ""
        last = last.strip()
        first = first.strip()
        if retval != "":
            retval += " and "
        retval += "%s, %s" % (last, first)
    return retval

def make_url(url):
    if type(url) == str:
        if url.startswith("http://"):
            return url
        else:
            return "http://" + url
    else:
        return "" # Bad! 

def format_record(row, query=''):
    retval = ''
    retval += link(get_title(row["title"]),
                   ("?bibid=%s%s" % (row["bib_id"], query)), CLASS="title")
    retval += begin("DIV", CLASS="vague")
    if row["journal"] or row["book_title"]:
        if row["book_title"]:
            retval += "<I>%s</I>" % row["book_title"]
        elif row["journal"]:
            retval += "<i>%s</i>" % link(row["journal"],
                                         "?" + urllib.urlencode([("f", "journal"),
                                                                 ("q", row["journal"]),
                                                                 ("m", "search")]))
        if row["volume"]:
            retval += ", Vol. %s" % row["volume"]
        if row["edition"]:
            retval += ", Ed. %s" % row["edition"]
        if row["issue"]:
            retval += ", No. %s" % row["issue"]
        if row["year"]:
            retval += ", ("
            year = ""
            if row["month"]:
                if (row["month"] - 1) in range(12):
                    year += "%s " % MONTHS[row["month"] - 1]
            year += "%s" % row["year"]
            retval += link(year,
                            "?" + urllib.urlencode([("f", "date"),
                                                    ("q", year),
                                                    ("m", "search")]))
            retval += ")"
        if row["start"]:
            retval += ", pp. %s" % (row["start"],)
            if row["end"]:
                retval += "-%s" % (row["end"], )
        retval += ".<BR />"
    if row["author"] != '':
        retval += " by " + format_names(row["author"]) + ".<BR />"
    if row["tags"] != '':
        words = row["tags"].split(",")
        tags = ""
        for word in words:
            word = word.strip()
            if tags:
                tags += ", "
            tags += link(word, 
                         "?" + urllib.urlencode([("f", "tag"),
                                                 ("q", word),
                                                 ("o", "datetime"),
                                                 ("m", "search")]))
        retval += "Tags: " + tags + br()
    retval += "posted on %s by %s. " % (row["datetime"],
                                        link(row["user_id"],
                                             "?" + urllib.urlencode([("u", row["user_id"]),
                                                                     ("f", "last_name"),
                                                                     ("q", "''"),
                                                                     ("o", "datetime"),
                                                                     ("m", "search")])))
    if row["url"]:
        retval += link("On-line", make_url(row["url"]))
    if row["doi"]:
        retval += " " + link("DOI",
                             "http://dx.doi.org/" + row["doi"])
    retval += end("DIV")
    return retval

def view_record(db, bib_id):
    data = db.q("""select * from bibliography where bib_id = %s;""", bib_id)
    if len(data) == 1:
        print begin("OL")
        print format("LI", format_record(data[0]))
        print end("OL")

def lookup_type(ctype):
    options = get_types()
    for pair in options:
        if pair[0].lower().strip() == ctype.lower().strip():
            return pair[1]
    return "Unknown type: " + ctype

def get_title(title):
    if title:
        return title
    else:
        return "(No Title)"

def format_key(data):
    if type(data) == str and data.startswith("citeulike:"):
        return link(data, "http://www.citeulike.org/article/" + data[10:])
    return data

def view_record_detail(form, db, uid, bib_id, query = ''):
    if query:
        print p(link("^ Return to list", query))
    data = db.q("""select * from bibliography where bib_id = %s;""", bib_id)
    if len(data) != 1: return
    row = data[0]
    print begin("TABLE")
    print tr(tdr(bold("Type:")) + td(lookup_type(row["bib_type"])))
    if row["bib_key"] != '':
        print tr(tdr(bold("Key:")) + td(format_key(row["bib_key"])))
    if row["title"] != '':
        print tr(tdr(bold("Title:")) + td(italics(row["title"])))
    if row["datetime"] != '':
        print tr(tdr(bold("Date/time:")) + td(row["datetime"]))
    if row["author"] != '':
        print tr(tdr(bold("Authors:")) + td(format_names(row["author"])))
    if row["editor"] != '':
        print tr(tdr(bold("Editors:")) + td(format_names(row["editor"])))
    if row["tags"] != '':
        print tr(tdr(bold("Tags:")) + td(row["tags"]))
    if row["year"] != None:
        print tr(tdr(bold("Year:")) + td(row["year"]))
    if row["journal"] != '':
        print tr(tdr(bold("Journal:")) + td(row["journal"]))
    if row["volume"] != '':
        print tr(tdr(bold("Volume:")) + td(row["volume"]))
    if row["issue"] != '':
        print tr(tdr(bold("Issue:")) + td(row["issue"]))
    if row["chapter"] != '':
        print tr(tdr(bold("Chapter:")) + td(row["chapter"]))
    if row["edition"] != '':
        print tr(tdr(bold("Edition:")) + td(row["edition"]))
    if row["start"] != '':
        if row["end"] != '':
            print tr(tdr(bold("Pages:")) + td(row["start"]+" - "+row["end"]))
        else:
            print tr(tdr(bold("Start Page:")) + td(row["start"]))
    else:
        if row["end"] != '':
            print tr(tdr(bold("End page:")) + td(row["start"]))
    if row["month"] != 0 and row["month"] != None:
        print tr(tdr(bold("Month:")) + td(lookup_month(row["month"])))
    if row["day"] != '':
        print tr(tdr(bold("Day:")) + td(row["day"]))
    if row["date_other"] != '':
        print tr(tdr(bold("Other date:")) + td(row["date_other"]))
    if row["book_title"] != '':
        print tr(tdr(bold("Book title:")) + td(row["book_title"]))
    if row["how_published"] != '':
        print tr(tdr(bold("How published:")) + td(row["how_published"]))
    if row["institution"] != '':
        print tr(tdr(bold("Institution:")) + td(row["institution"]))
    if row["organization"] != '':
        print tr(tdr(bold("Organization:")) + td(row["organization"]))
    if row["publisher"] != '':
        print tr(tdr(bold("Publisher:")) + td(row["publisher"]))
    if row["address"] != '':
        print tr(tdr(bold("Address:")) + td(row["address"]))
    if row["school"] != '':
        print tr(tdr(bold("School:")) + td(row["school"]))
    if row["series"] != '':
        print tr(tdr(bold("Series:")) + td(row["series"]))
    if row["abstract"] != '':
        print tr(tdr(bold("Abstract:")) + td(format_html(row["abstract"])))
    if row["doi"] != '':
        print tr(tdr(bold("DOI:")) + td(link(row["doi"],
                                             "http://dx.doi.org/" + row["doi"])))
    if row["url"] != '':
        print tr(tdr(bold("URL:")) + td(link(row["url"],
                                             make_url(row["url"])) +
                                        """ <a href="http://scholar.google.com/scholar?q=%22""" + row["title"] + """%22"><img src="images/favicon_google.gif" border=0></a>"""))
    elif row["title"] != '':
        print tr(tdr(bold("URL:")) + td(""" <a href="http://scholar.google.com/scholar?q=%22""" + row["title"] + """%22"><img src="images/favicon_google.gif" border=0></a>"""))

    if row["priority"] != 0:
        print tr(tdr(bold("Priority:")) + td(row["priority"]))
    print end("TABLE")
    options = ""
    if uid == row["user_id"] or permcheck(db, uid, "edit"):
        options += "[ " + link("Edit", "?m=edit&bibid=%s" % bib_id) + " ] "
        options += "[ " + link("Delete", "?m=delete&bibid=%s" % bib_id) + " ] "
    options += "[ " + link("Make a comment", "?m=comment&bibid=%s" % bib_id) + " ] "
    print options

def makeDefaultRecord():
    return {
        "bib_type": "",
        "bib_key": "",
        "title": "",
        "author": "",
        "editor": "",
        "journal": "",
        "volume": "",
        "issue": "",
        "chapter": "",
        "edition": "",
        "start": "",
        "end": "",
        "year": "",
        "month": "",
        "day": "",
        "date_other": "",
        "book_title": "",
        "how_published": "",
        "institution": "",
        "organization": "",
        "publisher": "",
        "address": "",
        "school": "",
        "series": "",
        "abstract": "",
        "doi": "",
        "tags": "",
        "priority": "",
        "user_id": "",
        "url": ""
        }

def lookup_month(m):
    if 1 <= m <= 12:
        return MONTHS[m - 1]
    else:
        return ""

def get_months():
    months = []
    i = 1
    for m in MONTHS:
        months.append( [i, m] )
        i += 1
    return months

def get_types():
    return [["abstract", "Abstract"],
            ["art", "Art"], 
            ["book", "Book"],
            ["inbook", "Book chapter/section"],
            ["booklet", "Booklet"],
            ["incollection", "Book part (with own title)"],
            ["inproceedings", "Conference proceedings (article)"],
            ["proceedings", "Conference proceedings (whole)"],
            ["data", "Data"],
            ["electronic", "Electronic citation"],
            ["unpublished", "In the press"],
            ["article", "Journal article"],
            ["manual", "Manual (technical documentation)"],
            ["map", "Map"], 
            ["misc", "Miscellaneous"],
            ["patent", "Patent"],
            ["techreport", "Technical report"],
            ["techstandard", "Technical Standard"], #?
            ["mastersthesis", "Thesis (Master's)"],
            ["phdthesis", "Thesis (PhD)"],
            ["unpublished", "Unpublished work"],
            ["video", "Video"],
            ["workshop", "Workshop"],
            ]

def record_delete(form, db, uid, bib_id):
    data = db.q("""delete from bibliography where bib_id = %s;""", bib_id)
    browse_user_records(form, db, uid)
    
def record_edit(form, db, uid, bib_id = None):
    if bib_id != None:
        data = db.q("""select * from bibliography where bib_id = %s;""", bib_id)
        if len(data) != 1: return
        row = data[0]
    else:
        row = makeDefaultRecord()
    print begin("FORM", name="MAIN", method="POST",
                action=URLPATH,
                enctype="multipart/form-data")
    options = get_types()
    months = get_months()
    print begin("TABLE")
    print tr(tdr(bold("Type:")) + td(
        picklist("bib_type", row["bib_type"], options)))
    print tr(tdr(bold("Title:")) + td(input("title", row["title"],
                                            "TEXTAREA", rows=3, cols=38)))
    print tr(tdr(bold("Authors:")) + td(input("author", row["author"], size=50)
                                        + "<BR><i>Lastname, Firstname and Lastname, Firstname and ...</i>"))
    print tr(tdr(bold("Editors:")) + td(input("editor", row["editor"], size=50)
                                        + "<BR><i>Lastname, Firstname and Lastname, Firstname and ...</i>"))
    print tr(tdr(bold("Tags:")) + td(input("tags", row["tags"], size=50)
                                     + "<BR><i>tag, tag, tag, ... , tag</i>"))
    print tr(tdr(bold("Year:")) + td(input("year", row["year"], size=10)))
    print tr(tdr(bold("Journal:")) + td(input("journal", row["journal"], size=50)))
    print tr(tdr(bold("Volume:")) +
             td(input("volume", row["volume"], size=10) + 
                ("&nbsp;" * 10) + 
                bold("Issue: ") + input("issue", row["issue"], size=10)))
    print tr(tdr(bold("Chapter:")) + td(input("chapter", row["chapter"], size=10)))
    print tr(tdr(bold("Edition:")) + td(input("edition", row["edition"], size=10)))
    print tr(tdr(bold("Start page:")) +
             td(input("start", row["start"], size=10) +
                ("&nbsp;" * 10) + 
                bold("End page: ") + input("end", row["end"], size=10)))
    print tr(tdr(bold("Month:")) +
             td(picklist("month", row["month"], months, blankOption=1)))
    print tr(tdr(bold("Day:")) + td(input("day", row["day"], size=10)))
    print tr(tdr(bold("Other date:")) +
             td(input("date_other", row["date_other"], size=10)))
    print tr(tdr(bold("Book title:")) +
             td(input("book_title", row["book_title"], size=50)))
    print tr(tdr(bold("How published:")) +
             td(input("how_published", row["how_published"], size=50)))
    print tr(tdr(bold("Institution:")) +
             td(input("institution", row["institution"], size=50)))
    print tr(tdr(bold("Organization:")) +
             td(input("organization", row["organization"], size=50)))
    print tr(tdr(bold("Publisher:")) +
             td(input("publisher", row["publisher"], size=50)))
    print tr(tdr(bold("Address:")) + td(input("address", row["address"], size=50)))
    print tr(tdr(bold("School:")) + td(input("school", row["school"], size=50)))
    print tr(tdr(bold("Series:")) + td(input("series", row["series"], size=50)))
    print tr(tdr(bold("Bibtex Key:")) + td(input("bib_key", row["bib_key"], size=25)))
    print tr(tdr(bold("DOI:")) + td(input("doi", row["doi"], size=50)))
    print tr(tdr(bold("URL:")) + td(input("url", row["url"], size=50)))
    #print tr(tdr(bold("User_id:")) + td(input("user_id", row["user_id"], size=10))) # FIX, make hidden
    print tr(tdr(bold("Abstract:")) +
             td(input("abstract", row["abstract"], "TEXTAREA", cols=50, rows=10)))
    print tr(tdr(bold("Priority:")) + td(input("priority", row["priority"], size=10)))
    print end("TABLE")
    print hidden("m", "save")
    if bib_id != None:
        print hidden("bibid", bib_id)
    print hidden("user_id", row["user_id"])
    print submit("Save")
    print end("FORM")

def record_save(form, db, uid, bib_id = None):
    bib_fields = ["bib_type",
                  "bib_key",
                  "title",
                  "author",
                  "editor",
                  "journal",
                  "volume",
                  "issue",
                  "chapter",
                  "edition",
                  "start",
                  "end",
                  "year",
                  "month",
                  "day",
                  "date_other",
                  "book_title",
                  "how_published",
                  "institution",
                  "organization",
                  "publisher",
                  "address",
                  "school",
                  "series",
                  "abstract",
                  "doi",
                  "tags",
                  "priority",
                  "user_id",
                  "url",
                  ] # FIX: add note
    data_dict = {}
    for field in bib_fields:
        data_dict[field] = form.getvalue(field)
    data_dict["author"] = cleanup_names(data_dict["author"])
    if data_dict["doi"].startswith("http://doi.acm.org/"):
        data_dict["doi"] = data_dict["doi"][19:]
    if bib_id == None:
        # first check to see if it is already in:
        title = data_dict.get("title", '')
        bib_type = data_dict.get("bib_type", '')
        if title and bib_type:
            check_dup = db.q("""select * from bibliography where title = %s and bib_type = %s;""", title, bib_type)
            if len(check_dup) > 0:
                print p("Error: A record with this title and type already exists: '%s' %s" % (title, bib_type))
                return
        db.query("""insert into bibliography (
                    bib_type,
                    bib_key,
                    title,
                    datetime,
                    author,
                    editor,
                    journal,
                    volume,
                    issue,
                    chapter,
                    edition,
                    start,
                    end,
                    year,
                    month,
                    day,
                    date_other,
                    book_title,
                    how_published,
                    institution,
                    organization,
                    publisher,
                    address,
                    school,
                    series,
                    abstract,
                    doi,
                    tags,
                    priority,
                    user_id,
                    url
                    ) VALUES (
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s,
                    %s
                    );""",
                 data_dict.get("bib_type", ''),
                 data_dict.get("bib_key", ''),
                 data_dict.get("title", ''),
                 data_dict.get("datetime", fptime2stime(time.time())),
                 data_dict.get("author", ''),
                 data_dict.get("editor", ''),
                 data_dict.get("journal", ''),
                 data_dict.get("volume", ''),
                 data_dict.get("issue", ''),
                 data_dict.get("chapter", ''),
                 data_dict.get("edition", ''),
                 data_dict.get("start", ''),
                 data_dict.get("end", ''),
                 data_dict.get("year", None),
                 data_dict.get("month", None),
                 data_dict.get("day", ''),
                 data_dict.get("date_other", ''),
                 data_dict.get("booktitle", ''),
                 data_dict.get("howpublished", ''),
                 data_dict.get("institution", ''),
                 data_dict.get("organization", ''),
                 data_dict.get("publisher", ''),
                 data_dict.get("address", ''),
                 data_dict.get("school", ''),
                 data_dict.get("series", ''),
                 data_dict.get("abstract", ''),
                 data_dict.get("doi", ''),
                 data_dict.get("tags", ''),
                 data_dict.get("priority", 0),
                 uid,
                 data_dict.get("url", ""),
                 )
        bib_id = db.db.insert_id()
        if data_dict.get("note", None) != None:
            db.q("""insert into note (bib_id, note, public) VALUES (
                 %s,
                 %s,
                 %s
                 );""",
                 bib_id,
                 data_dict.get("note", ''),
                 1
                 )
    else: # update (not insert) the record
        db.query("""update bibliography set
                    bib_type = %s,
                    bib_key = %s,
                    title = %s,
                    datetime = %s,
                    author = %s,
                    editor = %s,
                    journal = %s,
                    volume = %s,
                    issue = %s,
                    chapter = %s,
                    edition = %s,
                    start = %s,
                    end = %s,
                    year = %s,
                    month = %s,
                    day = %s,
                    date_other = %s,
                    book_title = %s,
                    how_published = %s,
                    institution = %s,
                    organization = %s,
                    publisher = %s,
                    address = %s,
                    school = %s,
                    series = %s,
                    abstract = %s,
                    doi = %s,
                    tags = %s,
                    priority = %s,
                    user_id = %s,
                    url = %s
                    WHERE bib_id = %s
                    ;""",
                 data_dict.get("bib_type", ''),
                 data_dict.get("bib_key", ''),
                 data_dict.get("title", ''),
                 data_dict.get("datetime", fptime2stime(time.time())),
                 data_dict.get("author", ''),
                 data_dict.get("editor", ''),
                 data_dict.get("journal", ''),
                 data_dict.get("volume", ''),
                 data_dict.get("issue", ''),
                 data_dict.get("chapter", ''),
                 data_dict.get("edition", ''),
                 data_dict.get("start", ''),
                 data_dict.get("end", ''),
                 data_dict.get("year", None),
                 data_dict.get("month", None),
                 data_dict.get("day", ''),
                 data_dict.get("date_other", ''),
                 data_dict.get("booktitle", ''),
                 data_dict.get("howpublished", ''),
                 data_dict.get("institution", ''),
                 data_dict.get("organization", ''),
                 data_dict.get("publisher", ''),
                 data_dict.get("address", ''),
                 data_dict.get("school", ''),
                 data_dict.get("series", ''),
                 data_dict.get("abstract", ''),
                 data_dict.get("doi", ''),
                 data_dict.get("tags", ''),
                 data_dict.get("priority", 0),
                 data_dict.get("user_id", uid),
                 data_dict.get("url", ""),
                 bib_id,
                 )
    view_record(db, bib_id)

def advanced_search_query(form, db, uid):
    print "Perform query: Coming Soon!"

def advanced_search(form, db, uid):
    print "Advanced Search"
    print begin("FORM", name="MAIN", method="POST",
                action=URLPATH,
                enctype="multipart/form-data")
    options = get_types()
    months  = get_months()
    print begin("TABLE")
    print tr(tdr(bold("Type:")) + td(
        picklist("bib_type", "", options, blankOption=1)))
    print tr(tdr(bold("Key:")) + td(input("bib_key", size=50)))
    print tr(tdr(bold("Title:")) + td(input("title", size=50)))
    print tr(tdr(bold("Authors:")) + td(input("author", size=50)))
    print tr(tdr(bold("Editors:")) + td(input("editor", size=50)))
    print tr(tdr(bold("Tags:")) + td(input("tags", size=50)))
    print tr(tdr(bold("Journal:")) + td(input("journal", size=50)))
    print tr(tdr(bold("Volume:")) + td(input("volume", size=50)))
    print tr(tdr(bold("Issue:")) + td(input("issue", size=50)))
    print tr(tdr(bold("Chapter:")) + td(input("chapter", size=50)))
    print tr(tdr(bold("Edition:")) + td(input("edition", size=50)))
    print tr(tdr(bold("Start page:")) + td(input("start", size=50)))
    print tr(tdr(bold("End page:")) + td(input("end", size=50)))
    print tr(tdr(bold("Year:")) + td(input("year", size=50)))
    print tr(tdr(bold("Month:")) +
             td(picklist("month", "", months, blankOption=1)))
    print tr(tdr(bold("Day:")) + td(input("day", size=50)))
    print tr(tdr(bold("Other date:")) + td(input("date_other", size=50)))
    print tr(tdr(bold("Book title:")) + td(input("book_title", size=50)))
    print tr(tdr(bold("How published:")) + td(input("how_published", size=50)))
    print tr(tdr(bold("Institution:")) + td(input("institution", size=50)))
    print tr(tdr(bold("Organization:")) + td(input("organization", size=50)))
    print tr(tdr(bold("Publisher:")) + td(input("publisher", size=50)))
    print tr(tdr(bold("Address:")) + td(input("address", size=50)))
    print tr(tdr(bold("School:")) + td(input("school", size=50)))
    print tr(tdr(bold("Series:")) + td(input("series", size=50)))
    print tr(tdr(bold("Abstract:")) + td(input("abstract", size=50)))
    print tr(tdr(bold("DOI:")) + td(input("doi", size=50)))
    print tr(tdr(bold("Priority:")) + td(input("priority", size=50)))
    print tr(tdr(bold("URL:")) + td(input("url", size=50)))
    print end("TABLE")
    print hidden("m", "advanced_search")
    print submit("Search")
    print end("FORM")

def makeQuery(field, value):
    current = ''
    state = None
    query = ''
    for ch in value:
        if ch == '"':
            if state == "inquote": # then, end it
                state = None
                if query:
                    query += " and "
                query += current
                current = ''
            else:
                current += ch
        elif ch == ' ':
            if query:
                query += " and "
    return query

def orderbyText(orderby):
    if orderby == "datetime": return "date"
    return orderby

def plugin_key_translate(key):
    dict_data = {"start_page":      "start",
                 "authors":         "author",
                 "end_page":        "end",
                 "type":            "bib_type",
                 "title_secondary": "booktitle",
                 "title_series":    "series",
                 }
    if key in dict_data:
        return dict_data[key]
    else:
        return key

def plugin_name_parse(data):
    data = data.strip()
    retval = [] # all names
    current_name = [] # 4 parts
    current_piece = ""
    count = 0
    state = 0
    for ch in data:
        if ch == "{":
            state += 1 # level of {}
        elif ch == "}" and state == 1:
            state -= 1
        elif ch == "}" and state == 2:
            count += 1 # part
            state -= 1 # level of {}
            current_name.append( current_piece.strip() )
            current_piece = ""
        elif ch == " " and state == 2:
            current_piece += ch
        elif ch == " " and state == 1:
            if current_piece != "":
                count += 1
                current_name.append( current_piece.strip() )
                current_piece = ""
        else:
            current_piece += ch
        if count == 4: # end of name!
            retval.append(current_name)
            current_name = []
            count = 0
    return " and ".join([r[0] + ", " + r[1] for r in retval])
# {{de Hoon} Michiel MJ {de Hoon, Michiel J. L.}} {Makita Yuko Y {Makita, Yuko}} {Nakai Kenta K {Nakai, Kenta}} {Miyano Satoru S {Miyano, Satoru}}

def getStartPage(db, field, value, orderbyfield, start_letter, user = None):
    title, query = getSelectQuery(field, value)
    if user != None:
        query += " and user_id = '%s'" % user
    if orderbyfield != 'None':
        query += " ORDER BY " + orderbyfield
    if orderbyfield == "datetime":
        query += " DESC "
    data = db.q(query)
    page = 1
    count = 0
    for row in data:
        count += 1
        if row[orderbyfield][0].upper() >= start_letter:
            break
        if count % RECORDS_PER_PAGE == 0:
            page += 1
    return page

def sqlencode(s):
    s = s.replace("'", "''")
    return s

def getSelectQuery(field, value):
    if field == "last_name":
        title = "Author/Editor containing '%s'" % value
        query = "select * from bibliography where (author LIKE '%" + sqlencode(value) + "%' or editor LIKE '%" + sqlencode(value) + "%')"
    elif field == "title":
        title = "Title containing '%s'" % value
        query = "select * from bibliography where title LIKE '%" + sqlencode(value) + "%'"
    elif field == "abstract":
        title = "Abstract containing '%s'" % sqlencode(value)
        query = "select * from bibliography where abstract LIKE '%" + sqlencode(value) + "%'"
    elif field == "journal":
        title = "Journal containing '%s'" % value
        query = "select * from bibliography where journal LIKE '%" + value + "%'"
    elif field == "tag":
        title = "Tags containing '%s'" % value
        query = "select * from bibliography where tags LIKE '%" + value + "%'"
    elif field == "pdate":
        title = "Posted Date is '%s'" % value
        query = "select * from bibliography where datetime LIKE '%" + sqlencode(value) + "%'"
    elif field == "date":
        title = "Published Date is '%s'" % value
        if " " in value:
            month, year = value.split()
            month = MONTHS.index(month) + 1
            query = "select * from bibliography where (month = %s and year = %s)" % (month, year)
        else:
            query = "select * from bibliography where year = " + value
    elif field == 'None':
        title = "Browse"
        query = "select * from bibliography"
    else:
        raise AttributeError, ("field='%s'" %field)
    return title, query

def view_records_rss(form, db, data, query):
    pass

def main_menu(form, db, uid):
    if form.has_key('bml'): # screen scraper
        username = form.getvalue("username")
        url = form.getvalue("url")
        title = form.getvalue("title")
        print header(form, db, uid, "Bookmark post") # home
        os.system("cd %sciteulike/plugins; tclsh driver.tcl parse '%s' > %soutput" % (FILEPATH, url, FILEPATH ))
        fp = open("output", "r")
        data_dict = {}
        for line in fp:
            line = line.strip()
            if " -> " in line:
                key, data = line.split(" -> ", 1)
                key = plugin_key_translate(key)
                if key == "author":
                    data = plugin_name_parse(data)
                elif key == "bib_type":
                    data = endnoteTypeToBibtex(data)
                data_dict[key] = data
        fp.close()
        if data_dict.get("title", None) == None:
            print "ERROR: no data imported"
            return
        # next check to see if it is already in:
        title = data_dict.get("title", '')
        bib_type = data_dict.get("bib_type", '')
        if title and bib_type:
            check_dup = db.q("""select * from bibliography where title = %s and bib_type = %s;""", title, bib_type)
            if len(check_dup) > 0:
                print p("Error: A record with this title and type already exists: '%s' %s" % (title, bib_type))
                return
        db.query("""insert into bibliography (
                        bib_type,
                        bib_key,
                        title,
                        datetime,
                        author,
                        editor,
                        journal,
                        volume,
                        issue,
                        chapter,
                        edition,
                        start,
                        end,
                        year,
                        month,
                        day,
                        date_other,
                        book_title,
                        how_published,
                        institution,
                        organization,
                        publisher,
                        address,
                        school,
                        series,
                        abstract,
                        doi,
                        tags,
                        priority,
                        user_id,
                        url
                        ) VALUES (
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s,
                        %s
                        );""",
                 data_dict.get("bib_type", ''),
                 data_dict.get("bib_key", ''),
                 data_dict.get("title", ''),
                 data_dict.get("datetime", fptime2stime(time.time())),
                 data_dict.get("author", ''),
                 data_dict.get("editor", ''),
                 data_dict.get("journal", ''),
                 data_dict.get("volume", ''),
                 data_dict.get("issue", ''),
                 data_dict.get("chapter", ''),
                 data_dict.get("edition", ''),
                 data_dict.get("start", ''),
                 data_dict.get("end", ''),
                 data_dict.get("year", None),
                 data_dict.get("month", None),
                 data_dict.get("day", ''),
                 data_dict.get("date_other", ''),
                 data_dict.get("booktitle", ''),
                 data_dict.get("howpublished", ''),
                 data_dict.get("institution", ''),
                 data_dict.get("organization", ''),
                 data_dict.get("publisher", ''),
                 data_dict.get("address", ''),
                 data_dict.get("school", ''),
                 data_dict.get("series", ''),
                 data_dict.get("abstract", ''),
                 data_dict.get("doi", ''),
                 data_dict.get("tags", ''),
                 data_dict.get("priority", 0),
                 uid,
                 data_dict.get("url", ""),
                 )
        bib_id = db.db.insert_id()
        view_record(db, bib_id)
    elif form.has_key('bibid'): ######################### Bib_id View
        bib_id = form.getvalue("bibid")
        if form.has_key('m'):
            mode = form.getvalue("m")
            if mode == "delete":
                print header(form, db, uid, "View record") # home
                record_delete(form, db, uid, bib_id)
            elif mode == "edit":
                print header(form, db, uid, "View record") # home
                record_edit(form, db, uid, bib_id)
            elif mode == "save":
                print header(form, db, uid, "Save record") # home
                record_save(form, db, uid, bib_id)
            elif mode == "comment":
                print header(form, db, uid, "Leave a comment") # home
                #record_save(form, db, uid)
                print p("coming soon!")
            else:
                error(form, db, uid, "Unknown bibid mode %s" % mode)                
        else:
            print header(form, db, uid, "View record") # home
            query = "?" + urllib.urlencode([("f", form.getvalue("f")),
                                            ("o", form.getvalue("o")),
                                            ("q", form.getvalue("q")),
                                            ("u", form.getvalue("u")),
                                            ("start", form.getvalue("start")),
                                            ("page", form.getvalue("page")),
                                            ("m", "search")])
            view_record_detail(form, db, uid,
                               form.getvalue("bibid"),
                               query)
    elif form.has_key('m'):
        mode = form.getvalue("m")
        if mode == "home":
            print header(form, db, uid) # home
            main_menu_default(form, db, uid)
        elif mode == "save":
            print header(form, db, uid, "New record") # home
            record_save(form, db, uid)
        elif mode == "advanced":
            print header(form, db, uid) # home
            advanced_search(form, db, uid)
        elif mode == "advanced_search":
            print header(form, db, uid) # home
            advanced_search_query(form, db, uid)
        elif mode == "bookmark":
            print header(form, db, uid, "Make bookmark")
            print p("No popup window: " + link("Post to IPRE", "javascript:location.href='%s%s?username=%s&bml=nopopup&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)" % (URL, URLPATH, uid)))
            print p("With popup window: " + link("IPRE Popup Post", "javascript:var pw=window.open('%s%s?username=%s&bml=popup&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title), 'ipre_popup_post', 'width=800,height=600,scrollbars=1,resizable=1'); void(window.setTimeout('pw.focus()',250));" % (URL, URLPATH, uid)))
            print p("The bookmarkers above work from the following pages:")
            pages = """
<a href="http://pubs.acs.org/">American Chem. Soc. Publications</a>
<a href="http://aip.org/">AIP Scitation</a>
<a href="http://www.anthrosource.net/">Anthrosource</a>
<a href="http://xxx.lanl.gov/">arXiv.org e-Print archive</a>
<a href="http://blackwell-synergy.com/</a>">Blackwell Synergy</a>
<a href="http://biomedcentral.com/">BioMed Central</a>
<a href="http://bmjjournals.com/">BMJ</a>
<a href="http://citeseer.ist.psu.edu/">CiteSeer</a>
<a href="http://eprint.iacr.org/">Cryptology ePrint Archive</a>
<a href="http://aaasmember.sciencemag.org/">HighWire</a>
<a href="http://www.informaworld.com/">informaworld</a>
<a href="http://www.ingentaconnect.com/">Ingenta</a>
<a href="http://iop.org/">IoP Electronic Journals</a>
<a href="http://jstor.org/">JSTOR</a>
<a href="http://www.isrl.uiuc.edu/">Language Evolution and Computation</a>
<a href="http://www.ams.org/">MathSciNet</a>
<a href="http://adsabs.harvard.edu/">NASA Astrophysics Data System</a>
<a href="http://www.nature.com/">Nature</a>
<a href="http://plosjournals.org/">PLoS</a>
<a href="http://muse.jhu.edu/">Project MUSE</a>
<a href="http://prola.aps.org/">Physical Review Online Archive</a>
<a href="http://journals.royalsoc.ac.uk/">Royal Society</a>
<a href="http://sciencedirect.com/">ScienceDirect</a>
<a href="http://scopus.com/">Scopus</a>
<a href="http://springerlink.com/">SpringerLink</a>
<a href="http://www.usenix.org/">Usenix</a>
<a href="http://interscience.wiley.com/">Wiley InterScience</a>
"""
            for line in pages.split("\n"):
                if line:
                    print line, " | "
        elif mode == "manual":
            require(form, db, uid, "add")
            print header(form, db, uid, "Import")
            record_edit(form, db, uid)
        elif mode == "import":
            require(form, db, uid, "add")
            print header(form, db, uid, "Import")
            if form.getvalue("format") == "bibtex":
                import_filename_bibtex(form, db, uid)
            elif form.getvalue("format") == "endnote":
                import_filename_endnote(form, db, uid)
        elif mode == "upload":
            require(form, db, uid, "add")
            print header(form, db, uid, "Upload")
            if form.getvalue("format") == "bibtex":
                import_bibtex(form, db, uid)
            elif form.getvalue("format") == "endnote":
                import_endnote(form, db, uid)
        elif mode == "faq":
            print header(form, db, uid)
            print "FAQ"
        elif mode == "search":
            if form.has_key('q'): # search box
                value = form.getvalue("q")
                if value in ["%27%27", "''"]:
                    value = ""
                field = form.getvalue("f")
                if form.has_key('o'): # which page
                    orderbyfield = form.getvalue("o")
                else:
                    orderbyfield = "title"
                if form.has_key('page') and form.getvalue("page") != 'None': # which page
                    page = int(form.getvalue("page"))
                elif form.has_key('start') and form.getvalue('start') != 'None': # which page to start
                    if form.has_key('u') and form.getvalue('u') != 'None': # which user
                        page = getStartPage(db,
                                            field,
                                            value,
                                            orderbyfield,
                                            form.getvalue('start'),
                                            form.getvalue('u'))
                    else:
                        page = getStartPage(db,
                                            field,
                                            value,
                                            orderbyfield,
                                            form.getvalue('start'))
                else:
                    page = 1
                title, query = getSelectQuery(field, value)
                if form.has_key('u'): # which user
                    user = form.getvalue("u")
                    if user and user != 'None':
                        query += " and user_id = '%s'" % user
                    else:
                        user = ""
                else:
                    user = ""
                if orderbyfield != 'None':
                    query += " ORDER BY " + orderbyfield
                if orderbyfield == "datetime":
                    query += " DESC "
                data = db.q(query + " LIMIT %d,%d" %
                            ((page - 1) * RECORDS_PER_PAGE, RECORDS_PER_PAGE))
                total_data = db.q(query)
                if value in ["", "%27%27"]:
                    value = urllib.quote("''")
                    if user:
                        title = "Browse %s's recent records" % user
                    else:
                        title = "Browse recent records"
                title += " ordered by " + orderbyText(orderbyfield)
                if form.has_key("rss"):
                    view_records_rss(form, db, data, 
                                     "?" + urllib.urlencode([("f", field),
                                                             ("o", orderbyfield),
                                                             ("q", value),
                                                             ("u", user),
                                                             ("m", "search")]))
                else:
                    print header(form, db, uid, field=field, value=value)
                    view_records(form, db, title, data, page,
                                 len(total_data),
                                 "?" + urllib.urlencode([("f", field),
                                                         ("o", orderbyfield),
                                                         ("q", value),
                                                         ("u", user),
                                                         ("m", "search")]))
            elif permcheck(db, uid, "admin"):
                require(form, db, uid, "admin")
                print header(form, db, uid, "System search")
                search = form.getvalue("search")
                print begin("FORM", name="MAIN", method="POST",
                            action=URLPATH,
                            enctype="multipart/form-data")
                print p("SQL: " + input("search", search, size=60))
                print hidden("m", "search")
                print submit("Search")
                print end("FORM")
                print p(italics("example: select username from accounts;"))
                data = db.q(search)
                print data.makeTable()
            else:
                # FIX: else could be logging in from no place in particular
                print header(form, db, uid)
                browse_user_records(form, db, uid)
        elif mode == "query":
            require(form, db, uid, "admin")
            print header(form, db, uid)
            print begin("FORM", name="MAIN", method="POST",
                        action=URLPATH,
                        enctype="multipart/form-data")
            print p("SQL: " + input("search", size=60))
            print hidden("m", "search")
            print submit("Search")
            print end("FORM")
            print italics("example: select username from accounts;")
        else: # ?m=something but something not matching
            error(form, db, uid, "Unknown mode %s" % mode)
    else: # nothing given, so assume home
        print header(form, db, uid)
        main_menu_default(form, db, uid)

def browse_user_records(form, db, uid):
    if uid:
        query = """select * from bibliography where user_id = '%s' order by datetime DESC""" % uid
        page = 1
        data = db.q(query + " LIMIT %d,%d" %
                    ((page - 1)* RECORDS_PER_PAGE, RECORDS_PER_PAGE))
        total_data = db.q(query)
        title = "Browse %s's recent records ordered by date" % uid
        view_records(form, db, title, data, page,
                     len(total_data),
                     "?" + urllib.urlencode([("o", "datetime"),
                                             ("f", "last_name"),
                                             ("u", uid),
                                             ("q", "''"),
                                             ("m", "search")]))
    else:
        main_menu_default(form, db, uid)

def main_menu_default(form, db, uid):
    query = """select * from bibliography order by datetime DESC"""
    page = 1
    data = db.q(query + " LIMIT %d,%d" %
                ((page - 1)* RECORDS_PER_PAGE, RECORDS_PER_PAGE))
    total_data = db.q(query)
    title = "Browse recent records ordered by date"
    view_records(form, db, title, data, page,
                 len(total_data),
                 "?" + urllib.urlencode([("o", "datetime"),
                                         ("f", "last_name"),
                                         ("q", "''"),
                                         ("m", "search")]))

def save_uploaded_file(form, form_field, filename=None, upload_dir = ""):
    """This saves a file uploaded by an HTML form.
       The form_field is the name of the file input field from the form.
       For example, the following form_field would be "file_1":
           <input name="file_1" type="file">
       The upload_dir is the directory where the file will be written.
       If no file was uploaded or if the field does not exist then
       this does nothing.
    """
    if not form.has_key(form_field): return 0
    if form[form_field].file == None: return 0
    if filename == None: return 0
    fileitem = form[form_field]
    if not fileitem.file: return 0
    # fileitem.filename was what it was called
    # filename is what we want to callit
    if filename == None:
        filename = fileitem.filename
    fout = file(os.path.join(upload_dir, filename), 'wb')
    while 1:
        chunk = fileitem.file.read(100000)
        if not chunk: break
        fout.write(chunk)
    fout.close()
    return 1

def makeDatetimeTag(datetime, mid = None):
    if datetime == None or len(datetime) != len("YYYY-MM-DD HH:MM"):
        if mid == None:
            return "Unknown date"
        else:
            return link("Unknown date", ("?m=view&mid=%s" % mid))
    date_, time_ = datetime.split(" ")
    year, month, day = date_.split("-", 2)
    hour, minute = time_.split(":")
    monthNames = ["January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December"]
    month = monthNames[int(month) - 1]
    day = int(day)
    hour = int(hour)
    if hour > 12:
        hour = hour - 12
        ampm = "pm"
    else:
        ampm = "am"
    if mid == None:
        retval = "%s %s, %s at %s:%s%s" % (month, day, year, hour, minute, ampm)
    else:
        retval = link("%s %s, %s at %s:%s%s" % (month, day, year, hour, minute, ampm), ("?m=view&mid=%s" % mid))
    return retval

def makeImageTag(email, size = 64): # size is 32, 64, 128
    retval = ""
    if type(email) == str and "@" in email:
        email, domain = email.split("@")
    imagefile = "T2_images/%s_%d.jpg" % (email, size)
    if file_exists(imagefile):
        retval += """ <img width="%d" src="%s" align="center" alt="%s" title="%s"> """ % (size, imagefile, email, email)
    else:
        retval += """ <img width="%d" src="%s" align="center" alt="%s" title="%s"> """ % (size, "T2_images/silhouette.jpg", email, email)
    return retval

def makeThumbnail(filename, size):
    thumbnaildir = "T2_images"
    infile = "images/" + filename
    im = Image.open(infile)
    x = im.size[0]
    y = im.size[1]
    newy = int(size * float(y)/x)
    im.thumbnail((size, newy), Image.ANTIALIAS)
    basename, ext = filename.split(".", 1)
    im.save(thumbnaildir + "/" + basename + "_" + str(size) + ".jpg", "JPEG")
    
def file_exists(filename):
    retval = 0
    try:
        os.stat(filename)
        retval = 1
    except:
        retval = 0
    return retval

def login(form, db):
    #print header(form, db)
    print begin("FORM", name="MAIN", method="POST",
                action=URLPATH,
                target="_top",
                enctype="multipart/form-data")
    print p("Enter your <b>ID</b> and <b>password</b> in the blanks below.")
    print begin("TABLE", border=0)
    print tr(td("ID:") + td(input("username", "")) + td(""))
    print tr(td("Password:") + td(input("password", "", "PASSWORD")) + td(""))
    print hidden("verify", "1")
    print tr(td("") + td(submit("Login")))
    print end("TABLE")
    print end("FORM")

def verify_password(form, db):
    if form.has_key("username") and form.has_key("password"):
        username = form["username"].value
        password = form["password"].value
        if username == "": return None
        username = username.strip().lower()
        # first, try a local lookup:
        data = db.q("select * from accounts where username = %s;", username)
        if len(data) == 1: # has a system account, or has logged in before
            if decrypt(password, data[0]["password"]): # password, md5code
                # save the session to pick up where we left off on next page
                session = makeSessionId()
                db.q("update accounts set session = %s where username = %s;",
                     session, username)
                return session
        # else, they can't log in (or add another method of verification here)
    return None

def verify_session(session, db):
    data = db.q("select * from accounts where session = %s;", session)
    if len(data) == 1:
        return data[0]["username"]
    return None

def makesalt(chars = string.letters + string.digits):
    # generate a random 2-character 'salt'
    return random.choice(chars) + random.choice(chars)

def encrypt(password, salt = makesalt()):
    return crypt.crypt(password, salt)

def decrypt(password, c2):
    if type(c2) == str and type(password) == str and len(c2) >= 2:
        c1 = encrypt(password, c2[:2])
        return c1 == c2
    else: return 0

def makeSessionId(st = "bryn mawr college"):
    m = md5.new()
    m.update('now is the time that all good people must come to the aid of their country')
    m.update(str(time.time()))
    m.update(str(st))
    return string.replace(base64.encodestring(m.digest())[:-3], '/', '$')

def li(item):
    return format("LI", item)
