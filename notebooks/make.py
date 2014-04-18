
from code2notebook import *
import glob
import os

ignore = [".ipynb_checkpoints", "attic", "Bugs", "reveal.js", "profile"]

def contains(root_path, items):
    for item in items:
        if item in root_path:
            return True
    return False

def clean_file(url):
    return url.rsplit("/")[-1].split(".")[0]

def clean_dir(url, with_file=True):
    if with_file:
        return "/".join(url.split("/")[-2:])
    else:
        return "/".join(url.split("/")[-2:-1])

site_cells = []

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
                sublist = ["Subfolders in %s:\n" % path, "\n"]
                ##print("Subfolders:", subfolders)
                for subfolder in subfolders:
                    sublist += ["* [%s](%s)\n" % (clean_dir(subfolder, with_file=False), clean_dir(subfolder))]
                add_cell(nb, make_cell(sublist, "markdown"))
            ### Notebooks in this folder:
            ##print("Checking " + root + "/" + path + "/*.ipynb")
            nb_files = sorted(glob.glob(root + "/" + path + "/*.ipynb"))
            if nb_files:
                cell = ["Notebooks in %s:\n" % path, "\n"]
                for file in nb_files:
                    if not file.endswith("Index.ipynb"):
                        cell += ["*  [%s](%s)\n" % (clean_file(file), file.rsplit("/")[-1])]
                add_cell(nb, make_cell(cell, "markdown"))
            if len(nb["worksheets"][0]["cells"]) > 0:
                save(nb, root + "/" + path + "/Index.ipynb")
