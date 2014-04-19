
from code2notebook import *
import glob
import os

ignore = [".ipynb_checkpoints", "attic", "Bugs", "reveal.js", "profile", "rebuild", "images"]

def contains(root_path, items):
    for item in items:
        if item in root_path:
            return True
    return False

def nice_name(path):
    if path == ".":
        return "top folder"
    else:
        return path

def clean_file(url):
    return url.rsplit("/")[-1].split(".")[0]

def clean_dir(url, with_file=True):
    if with_file:
        return "/".join(url.split("/")[-2:])
    else:
        return "/".join(url.split("/")[-2:-1])

site_cells = []

header =  ["<table><tr><td>\n",
           "<img width=\"300\" src=\"http://calicoproject.org/wiki/images/d/d1/CalicoLogo.gif\">\n",
           "</td>\n",
           "<td>\n",
           "The [Calico Project](http://calicoproject.org/) was designed to allow the easy exploration of computing ideas in a variety of programming languages. It is the product of NSF DUE grant #0920539 supporting the [Institute for Personal Robots in Education](http://wiki.roboteducation.org/).\n",
           "<br>\n",
           "<br>\n",
           "You can find out more about these notebooks here: [ICalico](http://calicoproject.org/ICalico).\n",
           "</td></tr></table>\n",
           "\n",
           ]

for root, dirs, dir_files in os.walk(".", topdown=False):
    for path in dirs + ["."]:
        if not contains(root, ignore) and not contains(path, ignore):
            ##print("Processing %s/%s..." % (root, path))
            nb = make_notebook()
            ### Readme in this folder:
            if os.path.isfile(path + "/README.md"):
                readme = open(path + "/README.md").readlines()
                add_cell(nb, make_cell(readme, "markdown"))
            ### Dirs in this folder:
            subfolders = sorted(glob.glob(root + "/" + path + "/*/Index.ipynb"))
            if subfolders:
                sublist = ["Subfolders in <b>%s</b>:\n" % nice_name(path), "\n"]
                ##print("Subfolders:", subfolders)
                for subfolder in subfolders:
                    sublist += ["* [%s](%s)\n" % (clean_dir(subfolder, with_file=False), clean_dir(subfolder))]
                add_cell(nb, make_cell(sublist, "markdown"))
            ### Notebooks in this folder:
            ##print("Checking " + root + "/" + path + "/*.ipynb")
            nb_files = sorted(glob.glob(root + "/" + path + "/*.ipynb"))
            if nb_files:
                cell = ["Notebooks in <b>%s</b>:\n" % nice_name(path), "\n"]
                for file in nb_files:
                    if not file.endswith("Index.ipynb"):
                        cell += ["*  [%s](%s)\n" % (clean_file(file), file.rsplit("/")[-1])]
                add_cell(nb, make_cell(cell, "markdown"))
            if len(nb["worksheets"][0]["cells"]) > 0:
                nb["worksheets"][0]["cells"].insert(0, make_cell(header, "markdown"))
                save(nb, root + "/" + path + "/Index.ipynb")

site_files = []
for root, dirs, dir_files in os.walk("."):
    for file in dir_files:
        if file.endswith(".ipynb") and not contains(root +"/" + file, ignore):
            site_files.append(root + "/" + file)

site_map = make_notebook()
add_cell(site_map, make_cell(header, "markdown"))

cell = []
cell.append("\n")
cell.append("<b>Top folder</b>:\n")
current = "."
for file in sorted(site_files, key=lambda v: v.rsplit("/", 1)):
    path, name = file.rsplit("/", 1)
    if path != current:
        cell.append("\n")
        cell.append("<b>%s</b>:\n" % path[2:])
        current = path
    cell.append("* [%s](%s)\n" % (name, path))

add_cell(site_map, make_cell(cell, "markdown"))
save(site_map, "SiteMap.ipynb")
