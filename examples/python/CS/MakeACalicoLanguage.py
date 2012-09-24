import Myro
import os

data = {"name": "Test", "system_name": "test", "mime_type": "text/x-test", "extension": "test"}

data = Myro.ask(data, "Fill in data")

def mkdirs(path): # missing in IronPython os?
    parts = os.path.split(path)
    root = ""
    for part in parts:
        root = os.path.join(root, part)
        if not os.path.exists(root):
            os.mkdir(root)

data_dir = os.path.join(calico.path, "..", "data")

fp = open(os.path.join(data_dir, "CalicoPython.py"))
language_text = "".join(fp.readlines())
fp.close()

fp = open(os.path.join(data_dir, "SyntaxMode.xml"))
syntax_text = "".join(fp.readlines())
fp.close()

mkdirs(os.path.join(calico.path, "..", "languages", data["name"], "SyntaxModes"))

lang_file = os.path.join(calico.path, "..", "languages", data["name"], "Calico%s.py" % data["name"])
lang = open(lang_file, "w")
lang.write(language_text % data)
lang.close()

syntax_file = os.path.join(calico.path, "..", "languages", data["name"], "SyntaxModes", "%sSyntaxMode.xml" % data["name"])
syntax = open(syntax_file, "w")
syntax.write(syntax_text % data)
syntax.close()

calico.Open(lang_file)
calico.Open(syntax_file)