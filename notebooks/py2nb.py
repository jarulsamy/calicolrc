#-----------------------------------------------------------------------------
#  Copyright (C) 2014, Doug Blank <doug.blank@gmail.com>
#
#  Distributed under the terms of the BSD License.  The full license is in
#  the file COPYING, distributed as part of this software.
#-----------------------------------------------------------------------------

# based on code in IPython

import os
import json
import uuid
import hmac
import hashlib

encoder = json.encoder.JSONEncoder(sort_keys=True, indent=1)

def convert(py_file):
    py_full_path = os.path.abspath(py_file)
    base_path, base_name = os.path.split(py_full_path)
    base, ext = os.path.splitext(base_name)
    code = open(py_full_path).readlines()
    nb_full_path = os.path.join(base_path, base + ".ipynb")
    ## ---------------------
    notebook = make_notebook(code)
    notebook["metadata"]["signature"] = sign_notebook(notebook)
    fp = open(nb_full_path, "w")
    fp.write(encoder.encode(notebook))
    fp.close()

def cast_bytes(s, encoding=None):
    if not isinstance(s, bytes):
        return encode(s, encoding)
    return s

unicode_type = unicode

def yield_everything(obj):
    if isinstance(obj, dict):
        for key in sorted(obj):
            value = obj[key]
            yield cast_bytes(key)
            for b in yield_everything(value):
                yield b
    elif isinstance(obj, (list, tuple)):
        for element in obj:
            for b in yield_everything(element):
                yield b
    elif isinstance(obj, unicode_type):
        yield obj.encode('utf8')
    else:
        yield unicode_type(obj).encode('utf8')

def sign_notebook(notebook, digestmod=hashlib.sha256):
    h = hmac.HMAC(SECRET, digestmod=digestmod)
    for b in yield_everything(notebook):
        h.update(b)
    return "sha256:" + h.hexdigest()

def make_notebook(code):
    notebook = {}
    notebook["metadata"] = {
        "name": "",
##        "signature": make_signature(),
    }
    notebook["nbformat"] = 3
    notebook["nbformat_minor"] = 0
    notebook["worksheets"] = [make_worksheet(code)]
    return notebook

def make_worksheet(code):
    worksheet = {}
    worksheet["cells"] = [make_cell(code)]
    worksheet["metadata"] = {}
    return worksheet

def make_cell(code):
    cell = {
        "cell_type": "code",
        "collapsed": False,
        "input": code, ## code here: ["line\n", ...]
        "language": "python",
        "metadata": {},
        "outputs": [],
    }
    return cell

def make_signature():
    return "" 

def read_secret_key():
    filename = os.path.expanduser("~/.ipython/profile_calico/security/notebook_secret")
    with open(filename, 'rb') as f:
        return f.read()

key = read_secret_key()
SECRET = unicode(key).encode("ascii")

if __name__ == '__main__':
    import sys
    for arg in sys.argv[1:]:
        convert(arg)
