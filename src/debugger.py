from __future__ import print_function

import bdb
import time
import linecache
import Gtk

class Debugger(bdb.Bdb):
    def __init__(self, calico, interactive, show_trace):
        bdb.Bdb.__init__(self)
        self.calico = calico
        self.interactive = interactive
        self.show_trace = show_trace
        self.trace_level = 0

    def user_line(self, frame):
        indent = " " * self.trace_level
        if self.show_trace:
            print("%s Line %s: %s" % (indent,
                                      frame.f_lineno,
                                      linecache.getline(
                        frame.f_code.co_filename,
                        frame.f_lineno)), end="")
        self.interact(frame)

    def user_return(self, frame, payload):
        indent = " " * self.trace_level
        if payload:
            if self.show_trace:
                print(indent, "Return:", repr(payload))
        self.interact(frame)
        self.trace_level -= 1

    def user_call(self, frame, arg_list):
        indent = " " * self.trace_level
        if self.show_trace:
            print("%sFile \"%s\", line %s, in debugger" % (indent,
                                                       frame.f_code.co_filename,
                                                       frame.f_lineno))
            print("%s Line %s: %s" % (indent,
                                      frame.f_lineno,
                                      linecache.getline(
                        frame.f_code.co_filename,
                        frame.f_lineno)), end="")
        self.interact(frame)
        self.trace_level += 1

    def user_exception(self, frame, (exc_type, exc_value, exc_traceback)):
        indent = " " * self.trace_level
        if self.show_trace:
            print(indent, result, frame.f_lineno)
        self.interact(frame)
        self.trace_level -= 1

    def interact(self, frame):
        if self.interactive:
            if self.calico:
                self.calico.editor.select_or_open(frame.f_code.co_filename,
                                                  frame.f_lineno)
                time.sleep(.1)

