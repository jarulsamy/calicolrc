#
# Pyjama - Scripting Environment
#
# Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# $Id: pyjama.py 363 2011-03-13 18:13:56Z dblank $

from utils import Plugin
import time

class Logger(Plugin):
    """
    Record editing activity and save to log file.
    """
    def init(self):
        print "Logger Plugin created"

    def on_key_press(self, eventkey):
        print time.time(), "keystroke", self.window_type, [eventkey.State], eventkey.Key

    def on_switch_page(self, notebook, args):
        page = notebook.GetNthPage(args.PageNum)
        print time.time(), "switch", self.window_type, page.document.filename

# Uncomment to activate:
#def make_plugin(pyjama):
#    return Logger(pyjama)
