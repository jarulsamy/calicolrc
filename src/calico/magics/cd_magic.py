# Copyright (c) Calico Development Team.
# Distributed under the terms of the Modified BSD License.
# http://calicoproject.org/

from calico import Magic
import os

# FIXME: make work like IPython's %cd, as per docstring

class CDMagic(Magic):
    """%cd help:
  cd 'dir': changes to directory 'dir'.

  cd -: changes to the last visited directory.

  cd -<n>: changes to the n-th directory in the directory history.

  cd --foo: change to directory that matches 'foo' in history

  cd -b <bookmark_name>: jump to a bookmark set by %bookmark
     (note: cd <bookmark_name> is enough if there is no
      directory <bookmark_name>, but a bookmark with the name exists.)
      'cd -b <tab>' allows you to tab-complete bookmark names.

Options:

-q: quiet.  Do not print the working directory after the cd command is
executed.  By default IPython's cd command does print this directory,
since the default prompts do not display path information.
    """
    name = "cd"
    help_lines = [" %cd PATH - change current directory of session"]

    def line(self, args):
        try:
            os.chdir(args)
            retval = os.path.abspath(args)
        except Exception as e:
            self.kernel.Error(e.message)
            retval = None
        if retval:
            self.kernel.Print(retval)

def register_magics(magics):
    magics[CDMagic.name] = CDMagic
    
