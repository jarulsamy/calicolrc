
import glob
import os

for filename in glob.glob("*.ipynb"):
    os.system("wget \"http://nbviewer.ipython.org/urls/bitbucket.org/ipre/calico/raw/master/notebooks/%s?create=1\" -o \"/tmp/%s\"" % (filename, filename))
