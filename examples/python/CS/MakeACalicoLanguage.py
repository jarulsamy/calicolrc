import Myro
import os
import System

data = {"name": "Test",
        "system_name": "test",
        "mime_type": "text/x-test",
        "extension": "test",
        "comment": "##"}

data = Myro.ask(data, "New Language Details")
if data is None:
    raise Exception("Language Creation Aborted")

def mkdirs(path): # missing in IronPython os?
    parts = os.path.split(path)
    root = ""
    for part in parts:
        root = os.path.join(root, part)
        if not os.path.exists(root):
            os.mkdir(root)

data_dir = os.path.join(calico.path, "..", "data")

if (Myro.askQuestion("Where do you want to put your language?",
                     ["Calico Global Languages", "My Local Languages"]) == "Calico Global Languages"):
    dest_dir = os.path.join(calico.path, "..", "languages")
else:
    dest_dir = System.Environment.GetFolderPath(System.Environment.SpecialFolder.ApplicationData)
    dest_dir = os.path.join(dest_dir, "calico", "languages")

fp = open(os.path.join(data_dir, "CalicoPython.py"))
language_text = "".join(fp.readlines())
fp.close()

fp = open(os.path.join(data_dir, "SyntaxMode.xml"))
syntax_text = "".join(fp.readlines())
fp.close()

mkdirs(os.path.join(dest_dir, data["name"], "SyntaxModes"))
mkdirs(os.path.join(dest_dir, data["name"], "examples"))

lang_file = os.path.join(dest_dir, data["name"], "Calico%s.py" % data["name"])
lang = open(lang_file, "w")
lang.write(language_text % data)
lang.close()

hello_text = """%(comment)s This is a sample HelloWorld program in %(name)s.
%(comment)s Please edit this to provide details of your language.


"""
hello_file = os.path.join(dest_dir, data["name"], "examples", "HelloWorld.%(extension)s" % data)
hello = open(hello_file, "w")
hello.write(hello_text % data)
hello.close()

syntax_file = os.path.join(dest_dir, data["name"], "SyntaxModes", "%sSyntaxMode.xml" % data["name"])
syntax = open(syntax_file, "w")
syntax.write(syntax_text % data)
syntax.close()

calico.Open(lang_file)
calico.Open(syntax_file)
calico.Open(hello_file)
