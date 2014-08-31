from __future__ import print_function

from jupyter_kernel import MagicKernel
import calico.scheme
import os
import logging

class CalicoSchemeKernel(MagicKernel):
    implementation = 'Scheme'
    implementation_version = '1.0'
    language = 'scheme'
    language_version = '3.0'
    banner = "Calico Scheme"

    def __init__(self, *args, **kwargs):
        super(CalicoSchemeKernel, self).__init__(*args, **kwargs)
        self.log.setLevel(logging.INFO)

    def get_usage(self):
        return """Calico Scheme 
=========================================

Calico Scheme offers a combination of convenient shell features,
special commands and a history mechanism for both input (command
history) and output (results caching, similar to Mathematica). 

MAIN FEATURES
-------------

* Magic commands: type %magic for information on the magic subsystem.

* Dynamic object information:

  Typing ?word prints detailed information about an object.  If
  certain strings in the object are too long (docstrings, code, etc.) they get
  snipped in the center for brevity.

  Typing ??word gives access to the full information without
  snipping long strings. Long strings are sent to the screen through the less
  pager if longer than the screen, printed otherwise.

* Completion in the local namespace, by typing TAB at the prompt.

  At any time, hitting tab will complete any available commands or
  variable names, and show you a list of the possible completions if there's
  no unambiguous one. It will also complete filenames in the current directory.

  This feature requires the readline and rlcomplete modules, so it won't work
  if your system lacks readline support (such as under Windows).

* Search previous command history in two ways (also requires readline):

  - Start typing, and then use Ctrl-p (previous,up) and Ctrl-n (next,down) to
    search through only the history items that match what you've typed so
    far. If you use Ctrl-p/Ctrl-n at a blank prompt, they just behave like
    normal arrow keys.

  - Hit Ctrl-r: opens a search prompt. Begin typing and the system searches
    your history for lines that match what you've typed so far, completing as
    much as it can.

  - %hist: search history by index (this does *not* require readline).

* Persistent command history across sessions.

* Logging of input with the ability to save and restore a working session.

* System escape with !. Typing !ls will run 'ls' in the current directory.

* Input caching system:

  Offers numbered prompts (In/Out) with input and output caching. All
  input is saved and can be retrieved as variables (besides the usual arrow
  key recall).

  The following GLOBAL variables always exist (so don't overwrite them!):
  _i: stores previous input.
  _ii: next previous.
  _iii: next-next previous.
  _ih : a list of all input _ih[n] is the input from line n.

  Additionally, global variables named _i<n> are dynamically created (<n>
  being the prompt counter), such that _i<n> == _ih[<n>]

  For example, what you typed at prompt 14 is available as _i14 and _ih[14].

* Output caching system:

  For output that is returned from actions, a system similar to the input
  cache exists but using _ instead of _i. Only actions that produce a result
  (NOT assignments, for example) are cached. If you are familiar with
  Mathematica, Jupyter's _ variables behave exactly like Mathematica's %
  variables.

  The following GLOBAL variables always exist (so don't overwrite them!):
  _ (one underscore): previous output.
  __ (two underscores): next previous.
  ___ (three underscores): next-next previous.

  Global variables named _<n> are dynamically created (<n> being the prompt
  counter), such that the result of output <n> is always available as _<n>.

  Finally, a global dictionary named _oh exists with entries for all lines
  which generated output.

* Directory history:

  Your history of visited directories is kept in the global list _dh, and the
  magic %cd command can be used to go to any entry in that list.
"""

    def add_complete(self, matches, token):
        # from the language environment:
        slist = calico.scheme.execute_string_rm("(dir)")
        if not calico.scheme.exception_q(slist):
            for item in slist:
                item_str = str(item)
                if item_str.startswith(token) and item_str not in matches:
                    matches.append(item_str)
        # special forms and constants:
        for item in ["define", "define!", "func", "callback", "if",
                     "help", "define-syntax", "begin", "lambda", "trace-lambda",
                     "try", "catch", "finally", "raise", "choose"]:
            if item.startswith(token) and item not in matches:
                matches.append(item)
        # add items from calico.scheme.ENVIRONMENT
        for item in calico.scheme.ENVIRONMENT:
            if item.startswith(token) and item not in matches:
                matches.append(item)
        # add properties and attributes if token is "numpy.ar"
        if "." in token:
            components, partial = token.rsplit(".", 1)
            slist = calico.scheme.execute_string_rm("(dir %s)" % components)
            if not calico.scheme.exception_q(slist):
                for item in slist:
                    item_str = str(item)
                    if item_str.startswith(partial) and item_str not in matches:
                        matches.append(components + "." + item_str)
        # done with language-specific completitions

    def do_inspect(self, code, cursor_pos, detail_level=0):
        # Object introspection
        content = {'status': 'aborted', 'data': {}, 'found': False}
        docstring = "Help!"
        if docstring:
            content["data"] = self.formatter(docstring)
            content["status"] = "ok"
            content["found"] = True
        return content

    def help_patterns(self):
        # Longest first:
        return [
            ("^\?\?(.*)$", 2,
             "??item - get detailed help on item"), # "??code"
            ("^\?(.*)$", 1, 
             "?item - get help on item"),   # "?code"
        ]
        
    def set_variable(self, name, value):
        """
        Set a variable in the kernel's enviroment.
        """
        calico.scheme.ENVIRONMENT[name] = value

    def _get_help_on(self, expr, level=0):
        result = calico.scheme.execute_string_rm("(help %s)" % expr)
        if not calico.scheme.exception_q(result):
            return result
        elif expr in ["define", "define!", "func", "callback", "if",
                     "help", "define-syntax", "begin", "lambda", "trace-lambda",
                     "try", "catch", "finally", "raise", "choose"]:
            help_text = {
                "define": "(define NAME [DOCSTRING] VALUE) define a global variable (special form)", 
                "define!": "(define! NAME [DOCSTRING] VALUE) define a variable in the host system (special form)", 
                "func": "(func PROCEDURE) for wrapping Scheme procedures as a system function (special form)", 
                "callback": "(callback PROCEDURE) returns a host system function for system callbacks (special form)", 
                "if": "(if TEXT-EXPR TRUE-EXPR FALSE-EXPR) (special form)",
                "help": "(help ITEM) (special form)", 
                "define-syntax": "(define-syntax NAME NAME-TEST ...) (special form)", 
                "begin": "(begin EXPR...) (special form)", 
                "lambda": "(lambda (VAR...) EXPR...) or (lambda VAR EXPR...) (special form)", 
                "trace-lambda": "(trace-lambda NAME (VAR...) EXPR...) or (trace-lambda NAME VAR EXPR...) (special form)",
                "try": "(try EXPR (catch EXCEPTION NAME ...)...) (special form)", 
                "catch": "(try EXPR (catch EXCEPTION NAME ...) ...) (special form)", 
                "finally": "(try EXPR (catch EXCEPTION NAME ...)... (finally ...)) (special form)", 
                "raise": "(raise EXCEPTION) (special form)", 
                "choose": "Use (choose ITEM...) to setup non-deterministic interpreter, or use (choose) to go to next choice (special form)"
            }
            return help_text[expr]
        else:
            return "No available help on '%s'" % expr

    def repr(self, item):
        if isinstance(item, list): # a scheme vector
            items = " ".join(map(self.repr, item))
            return "#%d(%s)" % (len(item), items)
        elif isinstance(item, calico.scheme.cons): # a scheme list
            retval = repr(item)
            if retval.startswith("#"):
                return retval
            retval = []
            current = item
            while isinstance(current, calico.scheme.cons): 
                retval.append(self.repr(current.car))
                current = current.cdr
            retval = " ".join(retval)
            if current != calico.scheme.symbol_emptylist:
                retval += " . " + self.repr(current)
            return "(%s)" % retval
        elif isinstance(item, str):
            retval = repr(item)
            if retval.startswith("'"):
                retval = retval.replace('"', '\\"')
                retval = retval.replace('\n', '\\n')
                return '"' + retval[1:-1] + '"'
        # FIXME: newlines, chars?
        elif isinstance(item, bool):
            return '#t' if item else '#f'
        return repr(item)

    def do_execute_direct(self, code):
        try:
            retval = calico.scheme.execute_string_rm(code)
        except:
            return "Unhandled Error: " + code
        if calico.scheme.exception_q(retval):
            self.Error(calico.scheme.get_traceback_string(retval))
            retval = None
        if retval is calico.scheme.void_value:
            retval = None
        return retval

if __name__ == '__main__':
    from IPython.kernel.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=CalicoSchemeKernel)
