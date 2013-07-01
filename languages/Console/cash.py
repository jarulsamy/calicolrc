from __future__ import print_function, division

## Calico Shell: Console

## A cross-platform bash-like shell, integrated with Calico
## http://calicoproject.org/Calico_Console

## GPL, version 3 or greater
## Doug Blank

## TODO: add argparse to all commands

## Commands are written below in the following style:
## command(name, incoming-sequence or [], tty?, arguments, stack)
## -> returns a sequence [eg, returns a seq, or defines a generator]
## __doc__ : first line, summary; rest for a complete help doc

import glob
import os
import re
import shutil
import inspect
import traceback
import time
import argparse
# Needed in case you want to run this in Calico Python interactively:
import clr
clr.AddReference("Calico")
# Above not needed, unless interactive
import Calico

## ------------------------------------------------------------
## Globals:

lastcd = [os.getcwd()]
stack = []
trace = False
trace_pause = False
debug = False

# Reset below:
unix_commands = {}
dos_commands = {}
commands = {}
command_set = None

## ------------------------------------------------------------
## API to Calico:

def setTrace(value):
    global trace
    trace = value

def setPause(value):
    global trace_pause
    trace_pause = value

def executeLines(calico, text, stack):
    # put calico in the environment:
    globals()["calico"] = calico
    # compute the width, height of the output window:
    try:
        w = calico.Output.Allocation.Width
        h = calico.Output.Allocation.Height
        scale = (calico.GetFont().Size/1024)
        globals()["width"] = int(w/(scale - 2))
        globals()["height"] = int(h/(scale * 1.7))
    except:
        globals()["width"] = 80
        globals()["height"] = 24
    retval = True
    lineno = stack[-1][1] # last lineno
    for line in text.split("\n"):
        try:
            execute(line, stack=stack[:])
        except ConsoleException, e:
            calico.ErrorLine("Console stack trace:")
            calico.ErrorLine("Traceback (most recent call last):")
            for s in e.stack:
                calico.ErrorLine("  File \"%s\", line %s, from %s" % (s[0], s[1], s[4]))
            calico.ErrorLine(e.message)
            retval = False
            break
        except Exception, e:
            if debug:
                calico.ErrorLine("".join(traceback.format_exc()))
            calico.ErrorLine(str(e))
            retval = False
            break
        except SystemExit, e:
            retval = False
            break
        lineno += 1
        stack[-1][1] = lineno # increment
    return retval

def execute(text, return_value=False, stack=None, offset=0):
    """
    Execute a shell command line.
    """
    if debug: print("execute:", text, stack)
    if not stack:
        raise Exception("execute called without stack")
    # Break the command into parts:
    line = splitLines(text, stack)
    count = 0
    # Process each command, chaining to the next:
    incoming = None
    for data in line:
        if not data:
            continue
        # only last one is a tty
        tty = (count == len(line) - 1) and not return_value # pipe to a tty or not
        if len(data) == 2:
            command_name, args = data
        else:
            continue
        if command_name:
            if hasattr(command_name, "start") and hasattr(command_name, "end"):
                handleDebug(stack[-1][0], stack[-1][1], command_name.start + offset, command_name.end + offset)
            else:
                handleDebug(stack[-1][0], stack[-1][1], offset, offset + len(command_name))
            lcommand_name = command_name.lower()
            if command_set == "unix" and lcommand_name.startswith("#"):
                continue
            elif command_set == "dos" and lcommand_name == "rem":
                continue
            if debug: print("args:", args)
            if command_name.lower() == "set" and args[1] == "=": # SET x = y; Python evaluates RHS as string
                expr = " ".join(args[2:])
                var = args[0]
                calico.Execute("%s = '%s'" % (var, expr), "python")
                continue
            elif len(args) > 1 and args[0] == "=": # Console evalutes RHS, then Python
                expr = args[1:]
                calico.Execute("%s = %s" % (command_name, " ".join(expr)), "python")
                continue
            if command_name.lower() in commands:
                command = commands[command_name.lower()]
            else:
                raise ConsoleException("Console: no such command: '%s'. Try 'help'" % command_name, stack)
            if hasattr(command_name, "start") and hasattr(command_name, "end"):
                stack.append([stack[-1][0], stack[-1][1], command_name.start + offset, command_name.end + offset, command_name])
            else:
                stack.append([stack[-1][0], stack[-1][1], offset, offset + len(command_name), command_name])
            incoming = command(command_name, incoming, tty, args, stack)
        count += 1
    # and display the output
    if return_value:
        return incoming
    else:
        if incoming:
            display(incoming)

def setTraceButtons(filename, lineno, start, end):
    document = calico.GetDocument(filename)
    if document:
        calico.playResetEvent.Reset()
        calico.PlayButton.Sensitive = True
        calico.PauseButton.Sensitive = False
        document.GotoLine(lineno)
        if (start == -1 or end == -1):
            document.SelectLine(lineno)
        else:
            document.texteditor.SetSelection(lineno, start + 1, lineno, end + 1)

def handleDebug(filename, lineno, start, end):
    ## First, see if we need to do something:
    try:
        if (calico.CurrentDocument == None):
            return
        elif (calico.CurrentDocument.HasBreakpointSetAtLine(lineno)):
            # don't return! Fall through and wait
            pass
        elif (calico.ProgramSpeedValue == 100):
            return
    except:
        return
    # Ok, we have something to do:
    Calico.MainWindow.Invoke(lambda: setTraceButtons(filename, lineno, start, end))
    psv = calico.ProgramSpeedValue
    if (psv == 0 or
        calico.CurrentDocument.HasBreakpointSetAtLine(lineno) or
        trace_pause):
        calico.playResetEvent.WaitOne()
    elif (psv < 100): ## then we are in a delay:
        pause = (2.0 / psv)
        ## Force at least a slight sleep, else no GUI controls
        time.sleep(pause)

## ------------------------------------------------------------
## General functions and classes:

class ConsoleException(Exception):
    def __init__(self, message, stack):
        self.message = message
        self.stack = stack

def display(g):
    """
    The terminal emulator for displaying output.
    """
    for item in g:
        print(item)

def list_file(f, pargs, tty):
    """
    List a file
    """
    if pargs.long:
        return os.path.abspath(f)
    elif tty:
        return os.path.basename(f)
    else:
        return os.path.abspath(f)

def make_line(retval, item):
    if retval:
        retval += " "
    retval += str(item)
    return retval

def list_dir(d, pargs, tty):
    """
    List the contents of a directory
    """
    retval = ""
    if pargs.long or not tty:
        pass
    else:
        yield "%s:" % os.path.relpath(d)
    for f in glob.glob(os.path.join(d, "*")):
        if pargs.long or not tty:
            yield os.path.abspath(f)
        else:
            i = os.path.basename(f)
            if len(make_line(retval, i)) < width:
                retval = make_line(retval, i)
            elif len(str(i)) > width:
                if retval:
                    yield retval
                    retval = ""
                yield str(i)
            else:
                yield retval
                retval = str(i)
    if retval:
        yield retval
    return

def counts(text):
    chars = 0
    words = 0
    state = "non-word"
    for ch in text:
        chars += 1
        if ch in [" ", "\n", "\t"]:
            if state == "non-word":
                pass # still in non-word
            else:
                state = "non-word"
        else:
            if state == "word":
                pass # still in word
            else:
                words += 1
                state = "word"
    return words, chars

def match(pattern, item):
    # Dots are literal:
    pattern = pattern.replace(".", "\.")
    # asterisks mean match anything
    pattern = pattern.replace("*", ".*")
    if pattern.startswith("^"):
        pattern = pattern[1:]
    else: ## match anything on front end:
        pattern = ".*" + pattern
    return re.match(pattern, item)

def splitArgs(arg_list):
    """
    Split into command and args
    """
    ## FIXME: remove this function, and replace this with argsparse
    if debug: print("splitArgs:", arg_list)
    args = []
    flags = []
    for arg in arg_list:
        if arg in ["-", "--"]:
            args.append(arg)
        elif arg.startswith("-"):
            flags.append(arg)
        else:
            args.append(arg)
    if debug: print("returning:", args, flags)
    return args, flags

def expand(pattern, stack):
    """
    Looks to see if pattern has file-matching characters. If so,
    expand now.
    """
    if pattern.startswith("`"):
        return execute(pattern[1:], True, stack[:], pattern.start + 1) # offset
    elif pattern.startswith("$"):
        ## expand variable, which could be a pattern:
        return expand(str(calico.Evaluate(pattern[1:], "python")), stack)
    elif ("*" in pattern or 
        "?" in pattern or
        "~" in pattern):
        pattern = pattern.replace("~", os.path.expanduser("~"))
        retval = []
        for f in glob.glob("*"):
            if match(pattern, f):
                retval.append(f)
        return retval
    elif pattern == ".":
        return [os.getcwd()]
    elif pattern == "..":
        return [os.path.join(os.getcwd(), os.pardir)]
    else:
        return [pattern]

class AnnotatedString(str):
    pass

def makeAnnotatedString(s, start=-1, end=-1):
    retval = AnnotatedString(s)
    retval.start = start
    retval.end = end
    return retval

def addAnnotatedStrings(annotatedString, string, start):
    retval = makeAnnotatedString(annotatedString + string)
    retval.end = annotatedString.end
    if annotatedString.start == -1:
        retval.start = start
    else:
        retval.start = annotatedString.start
    return retval

def splitParts(text, stack):
    # OOP, needed to protect raw-objects
    if debug: print("splitParts:", text, stack)
    try:
        length = len(text)
    except:
        return [text]
    retval = []
    i = 0
    current = makeAnnotatedString("")
    mode = "start"
    while i < length:
        if mode == "start":
            if text[i] == "\\":
                i += 1
                current = addAnnotatedStrings(current, text[i], i)
            elif text[i].startswith("#"):
                # ignore here to end of line
                break
            elif text[i] == " ":
                if current:
                    current.end = i
                    retval.extend(expand(current, stack))
                    current = makeAnnotatedString("")
                else:
                    pass ## skip
            elif text[i] == "=":
                if current:
                    current.end = i
                    retval.append(current)
                    current = makeAnnotatedString("")
                retval.append(makeAnnotatedString('=', i, i + 1))
            elif text[i] == "'":
                if current:
                    current.end = i
                    retval.append(current)
                    current = makeAnnotatedString("", i)
                mode = "quote"
            elif text[i] == '"':
                if current:
                    current.end = i
                    retval.append(current)
                    current = makeAnnotatedString("", i)
                mode = "double-quote"
            elif text[i] == '`':
                if current:
                    current.end = i
                    retval.append(current)
                    # signal expand that this is an expr:
                current = makeAnnotatedString("`", i)
                mode = "back-quote"
            else:
                current = addAnnotatedStrings(current, text[i], i)
        elif mode == "quote":
            if text[i] == "'":
                current.end = i
                retval.append(current)
                current = makeAnnotatedString("", -1)
                mode = "start"
            else:
                current = addAnnotatedStrings(current, text[i], i)
        elif mode == "double-quote":
            if text[i] == '"':
                current.end = i
                retval.append(current)
                current = makeAnnotatedString("", -1)
                mode = "start"
            else:
                current = addAnnotatedStrings(current, text[i], i)
        elif mode == "back-quote":
            if text[i] == '`':
                current.end = i
                retval.extend(expand(current, stack))
                current = makeAnnotatedString("", -1)
                mode = "start"
            else:
                current = addAnnotatedStrings(current, text[i], i)
        i += 1
    if current: # some still left:
        if mode == "start":
            current.end = i
            retval.extend(expand(current, stack))
        elif mode == "quote":
            raise ConsoleException("Console: unended quote", stack)
        elif mode == "double-quote":
            raise ConsoleException("Console: unended double-quote", stack)
        elif mode == "back-quote":
            raise ConsoleException("Console: unended back-quote: leftover: '%s'" % current, stack)
    return retval

def splitLines(command_list, stack):
    commands = []
    command = []
    for symbol in splitParts(command_list, stack):
        if symbol in ["|"]:
            commands.append([command[0], command[1:]])
            command = []
        else:
            command.append(symbol) 
    if command:
        commands.append([command[0], command[1:]])
    return commands

## ------------------------------------------------------------
## Commands:

def cd(name, incoming, tty, args, stack):
    """
    change directory

    To change directories, use cd folder, where folder is either
    relative or absolute.

    cd
    cd -
    cd ..
    cd /

    See also: pwd, mkdir
    """
    global lastcd
    parser = argparse.ArgumentParser(
        prog=name, 
        epilog="No argument will %(prog)s to HOME")
    parser.add_argument('--number', '-n', action="store_true", 
                        help='%(prog)s will prefix each line with its line number')
    parser.add_argument('directory', nargs="?", 
                        help='the directory to change to')
    parser.add_argument('-', action="store_true", 
                        dest="goback",
                        help='go back to the previous directory')
    pargs = parser.parse_args(args)
    if debug: print(name, pargs)
    # ---
    if pargs.goback:
        if len(lastcd) > 1:
            pargs.directory = lastcd[-2]
        else:
            pargs.directory = None
        # else pass
    if pargs.directory:
        os.chdir(pargs.directory)
    else:
        os.chdir(os.path.expanduser("~"))
    lastcd.append(os.getcwd())
    return [lastcd[-1]]

def rm(name, incoming, tty, args, stack):
    """
    remove file or folders

    Use rm to delete files.
    """
    args, flags = splitArgs(args)
    # FIXME: remove files or directories
    os.remove(args[0])
    #shutil.rmtree() # removes directory and contents
    return []

def mkdir(name, incoming, ttyp, args, stack):
    """
    make a directory
    """
    args, flags = splitArgs(args)
    for filename in args:
        os.makedirs(filename)
    return []

def rmdir(name, incoming, tty, args, stack):
    """
    remove folders

    Use rmdir to delete folders.
    """
    args, flags = splitArgs(args)
    for folder in args:
        os.rmdir(folder) # removes an empty directory
    return []

def cp(name, incoming, tty, args, stack):
    """
    copy files and folders

    Use cp to copy files.
    """
    args, flags = splitArgs(args)
    shutil.copy(args[0], args[1])
    return []

def mv(name, incoming, tty, args, stack):
    """
    move files and folders

    Use mv to move files.
    """
    args, flags = splitArgs(args)
    # FIXME: mv can also move
    os.rename(args[0], args[1])
    return []

def pwd(name, incoming, tty, args, stack):
    """
    print working directory

    Use pwd to see your current directory.
    """
    #args, flags = splitArgs(args)
    return [os.getcwd()]

def ls(name, incoming, tty, args, stack):
    """
    list files

    Use ls to list the files in a folder.

    Flags:
       -a   see all
       -l   long format
    """
    parser = argparse.ArgumentParser(
        prog=name)
    parser.add_argument('--long', '-l', action="store_true", 
                        help='use a long listing format')
    parser.add_argument('--all', '-a', action="store_true", 
                        help='do not ignore files starting with .')
    parser.add_argument('file', nargs="*", 
                        help='files to list')
    pargs = parser.parse_args(args)
    if incoming:
        data = incoming
    else:
        data = pargs.file
    if not data:
        data = ["."]
    for f in data:
        if not os.path.exists(f):
            calico.ErrorLine("%s: cannot access '%s': no such file or directory" % (name, f))
            continue
        if os.path.isfile(f):
            yield list_file(f, pargs, tty)
        else:
            for item in list_dir(f, pargs, tty):
                yield item

def grep(name, incoming, tty, args, stack):
    """
    search for matches

    Grep through arguments, searching for matches.
    """
    parser = argparse.ArgumentParser(
        prog=name)
    parser.add_argument('--invert-match', '-v', action="store_true", 
                        help='Invert the sense of matching, to select non-matching lines.')
    parser.add_argument('pattern', help='pattern to use in matching')
    parser.add_argument('file', nargs="*", 
                        help='files to list')
    pargs = parser.parse_args(args)
    if incoming:
        for i in incoming:
            if pargs.invert_match:
                if not match(pargs.pattern, i):
                    yield i
            elif match(pargs.pattern, i):
                yield i
    else:
        for f in pargs.file:
            if os.path.exists(f) and os.path.isfile(f):
                text = open(f).readlines()
                for line in text:
                    line = line[:-1]
                    if pargs.invert_match:
                        if not match(pargs.pattern, line):
                            yield "%s: %s" % (f, line)
                    elif match(pargs.pattern, line):
                        yield "%s: %s" % (f, line)
            elif os.path.isdir(f):
                calico.ErrorLine("%s: '%s' is a directory" % (name, f))
            else:
                calico.ErrorLine("%s: no such file '%s'" % (name, f))
    return # ends yields

def wc(name, incoming, tty, args, stack):
    """
    word count; counts characters, words, and lines
    """
    args, flags = splitArgs(args)
    lines = 0
    chars = 0
    words = 0
    if incoming:
        for line in incoming:
            w, c = counts(line)
            lines += 1
            words += w
            chars += c + 1
        yield "%6d %6d %6d" % (lines, words, chars)
    else:
        for f in args:
            if os.path.exists(f) and os.path.isfile(f):
                text = open(f).readlines()
                for line in text:
                    w, c = counts(line)
                    lines += 1
                    words += w
                    chars += c + 1
            elif os.path.isdir(f):
                calico.ErrorLine("%s: '%s' is a directory" % (name, f))
            else:
                calico.ErrorLine("%s: no such file '%s'" % (name, f))
        yield "%6d %6d %6d total" % (lines, words, chars)
    return # ends yields

def more(name, incoming, tty, args, stack):
    """
    see output one page at a time
    """
    args, flags = splitArgs(args)
    count = 0
    lines = height - 2
    if incoming:
        for i in incoming:
            count += 1
            yield i
            if tty and (count % lines == 0):
                # can you tell if there are more?
                print("--more--")
                yn = calico.yesno("More?")
                if not yn:
                    return
    else:
        for f in args:
            if os.path.exists(f) and os.path.isfile(f):
                text = open(f).readlines()
                for line in text:
                    count += 1
                    yield line
                    if tty and (count % lines == 0):
                        # can you tell if there are more?
                        print("--more--")
                        yn = calico.yesno("More?")
                        if not yn:
                            return
            elif os.path.isdir(f):
                calico.ErrorLine("%s: '%s' is a directory" % (name, f))
            else:
                raise ConsoleException("%s: no such file '%s'" % (name, f), stack)

def cat(name, incoming, tty, args, stack):
    """
    concatenate files
    """
    parser = argparse.ArgumentParser(
        prog=name)
    parser.add_argument('--number', '-n', action="store_true", 
                        help='number all output lines')
    parser.add_argument('file', nargs="*", 
                        help='files to concatenate')
    try: 
        pargs = parser.parse_args(args)
    except: # OOP: arg is a generator?
        from collections import namedtuple
        makePargs = namedtuple("Pargs", "number")
        pargs = makePargs(number=False) # FIXME: handle args better
        incoming = args[0] # FIXME: make more flexible
    count = 0
    if incoming:
        for i in incoming:
            count += 1
            if pargs.number:
                yield "%6d %s" % (count, i)
            else:
                yield i
    else:
        for filename in pargs.file:
            if os.path.isdir(filename):
                raise ConsoleException("%s: '%s' is a directory" % (name, filename), stack)
            if not os.path.exists(filename) or not os.path.isfile(filename):
                raise ConsoleException("%s: file does not exist: '%s'" % (name, filename), stack)
            fp = open(filename)
            for line in fp:
                i = line[:-1]
                count += 1
                if pargs.number:
                    yield "%6d %s" % (count, i)
                else:
                    yield i

def sort(name, incoming, tty, args, stack):
    """
    sort data
    """
    args, flags = splitArgs(args)
    return sorted(list(args) + list(incoming))

def help_cmd(name, incoming, tty, args, stack):
    """
    get help on commands
    """
    parser = argparse.ArgumentParser(
        prog=name)
    parser.add_argument('command', nargs="*", 
                        help='commands on which to get help')
    pargs = parser.parse_args(args)
    #yield inspect.getdoc(help_cmd)
    if incoming:
        for command_name in incoming:
            command_name = command_name.strip()
            pcommand_name = command_name
            rcommand_name = command_name
            if command_set == "dos":
                pcommand_name = command_name.lower()
                rcommand_name = command_name.upper()
            if pcommand_name not in commands.keys():
                raise ConsoleException("%s: no such command '%s'" % (name, command_name), stack)
            yield ("%s: " % rcommand_name) + commands[pcommand_name].__doc__.strip()
    elif len(pargs.command) == 0:
        for command in sorted(commands.keys()):
            if command_set == "dos":
                command_name = command.upper()
            else:
                command_name = command
            yield "%s - %s" % (command_name, commands[command].__doc__.strip().split("\n")[0].strip())
    elif len(pargs.command) > 0:
        for command_name in pargs.command:
            pcommand_name = command_name
            rcommand_name = command_name
            if command_set == "dos":
                pcommand_name = command_name.lower()
                rcommand_name = command_name.upper()
            if pcommand_name not in commands.keys():
                raise ConsoleException("%s: no such command '%s'" % (name, command_name), stack)
            yield ("%s: " % rcommand_name) + commands[pcommand_name].__doc__.strip()

def show(name, incoming, tty, args, stack):
    """
    show an image graphically
    """
    args, flags = splitArgs(args)
    import Myro
    if incoming:
        for filename in incoming:
            if os.path.isfile(filename):
                pic = Myro.makePicture(filename)
                Myro.show(pic, filename)
            else:
                calico.ErrorLine("%s: ignoring '%s'; no such file" % (name, filename))
    else:
        for filename in args:
            if os.path.isfile(filename):
                pic = Myro.makePicture(filename)
                Myro.show(pic, filename)
            else:
                calico.ErrorLine("%s: ignoring '%s'; no such file" % (name, filename))
    return []

def open_cmd(name, incoming, tty, args, stack):
    """
    open a file in Calico
    """
    args, flags = splitArgs(args)
    if incoming:
        for filename in incoming:
            if os.path.isfile(filename):
                calico.Open(filename)
            else:
                calico.ErrorLine("%s: ignoring '%s'; no such file" % (name, filename))
    else:
        for filename in args:
            if os.path.isfile(filename):
                calico.Open(filename)
            else:
                calico.ErrorLine("%s: ignoring '%s'; no such file" % (name, filename))
    return []

def exec_cmd(name, incoming, tty, args, stack):
    """
    execute a file in Calico
    """
    if incoming:
        for item in incoming:
            parts = splitParts(item, stack)
            if len(parts) == 1:
                if os.path.isfile(parts[0]):
                    return [calico.ExecuteFile(parts[0])]
                elif os.path.isdir(f):
                    raise ConsoleException("%s: '%s' is a directory" % (name, f), stack)
                else:
                    raise ConsoleException("%s: no such file: '%s'" % (name, parts[0]), stack)
            else:
                return [calico.Execute(parts[0], parts[1])]
    else:
        if len(args) == 1:
            if os.path.isfile(args[0]):
                return [calico.ExecuteFile(args[0])]
            elif os.path.isdir(f):
                raise ConsoleException("%s: '%s' is a directory" % (name, f), stack)
            else:
                raise ConsoleException("%s: no such file: '%s'" % (name, args[0]), stack)
        else:
            return [calico.Execute(args[0], args[1])]
    return [1]

def eval_cmd(name, incoming, tty, args, stack):
    """
    evaluate text in Calico
    """
    args, flags = splitArgs(args)
    if incoming:
        # if there are args, then eval to a function
        if len(args) > 0:
            for item in incoming:
                # Can be dangerous, treating line as command-line args:
                try:
                    parts = splitParts(item, stack)
                except:
                    parts = [item]
                f = calico.Evaluate(args[0], args[1])
                # Can return objects, and other non-string things
                # OOP, pass raw-object
                yield f(*parts)
        # else, make the incoming be the "expr language"
        else:
            for item in incoming:
                parts = splitParts(item, stack)
                if len(parts) == 2:
                    yield calico.Evaluate(parts[0], parts[1])
                else:
                    raise ConsoleException("%s: need to specify a language with '%s'" % (name, parts[0]), stack)
    else:
        text = args[0]
        language = args[1]
        yield calico.Evaluate(text, language)

def echo(name, incoming, tty, args, stack):
    """
    create output
    """
    parser = argparse.ArgumentParser(
        prog=name)
    parser.add_argument('-e', action="store_true", 
                        help='enable interpretation of backslash escapes')
    parser.add_argument('arg', nargs="*", 
                        help='arguments to echo')
    ## OOP, if we wanted to allow every function to handle raw objects:
    #try: # can't parse objects
    #    pargs = parser.parse_args(args)
    #    data = " ".join(pargs.arg)
    #except:
    #    return args
    pargs = parser.parse_args(args)
    data = " ".join(pargs.arg)
    if pargs.e:
        return data.split("\\n")
    return [data]

def printf(name, incoming, tty, args, stack):
    """
    display output
    """
    args, flags = splitArgs(args)
    print(" ".join(args))
    return []

def switch(name, incoming, tty, args, stack):
    """
    to unix or dos
    """
    global commands, command_set
    args, flags = splitArgs(args)
    if len(args) > 0:
        if args[0] == "unix":
            commands = unix_commands
            command_set = "unix"
        elif args[0] == "dos":
            commands = dos_commands
            command_set = "dos"
        else:
            calico.ErrorLine("%s: cannot switch to '%s': use 'unix' or 'dos'" % (name, args[0]))
    else:
        return [command_set]
    return []

def head(name, incoming, tty, args, stack):
    """
    Get the first N lines from files.
    """
    parser = argparse.ArgumentParser(
        prog=name)
    parser.add_argument('--lines', '-n', nargs=1, type=int, default=[10],
                        help='how many lines to show')
    parser.add_argument('file', nargs="*", 
                        help='files to list')
    pargs = parser.parse_args(args)
    lines = pargs.lines[0]
    count = 0
    if incoming:
        for line in incoming:
            if count < lines:
                yield line
            else:
                break
            count += 1
    else:
        for filename in pargs.file:
            count = 0
            fp = open(filename)
            for line in fp:
                if count < lines:
                    yield line
                else:
                    break
                count += 1

def tail(name, incoming, tty, args, stack):
    """
    Get the last N lines from files.
    """
    parser = argparse.ArgumentParser(
        prog=name)
    parser.add_argument('--lines', '-n', nargs=1, type=int, default=[10],
                        help='how many lines to show')
    parser.add_argument('file', nargs="*", 
                        help='files to list')
    pargs = parser.parse_args(args)
    lines = pargs.lines[0]
    count = 0
    if incoming:
        # FIXME: too expensive; holds all in memory:
        for line in list(incoming)[-lines:]:
            yield line
            count += 1
    else:
        for filename in pargs.file:
            count = 0
            fp = open(filename)
            # FIXME: too expensive; holds all in memory:
            for line in fp.readlines()[-lines:]:
                yield line
                count += 1

# FIXME add these: head tail find pushd popd wget wc cal date du df
# uname cut plot time hostname id < > cls TAB-completion

### Add new commands above this point

## ------------------------------------------------------------
# Commands

unix_commands = {"ls": ls, "more": more, "cd": cd, "grep": grep,
                 "cat": cat, "help": help_cmd, "pwd": pwd, "cp": cp,
                 "rm": rm, "less": more, "show": show, "open": open_cmd,
                 "sort": sort, "exec": exec_cmd, "eval": eval_cmd,
                 "mkdir": mkdir, "mv": mv, "rmdir": rmdir, "echo": echo, 
                 "edit": open_cmd, "switch": switch, "printf": printf, 
                 "wc": wc, "head": head, "tail": tail}
dos_commands = {"dir": ls, "more": more, "cd": cd, "chdir": cd,
                "help": help_cmd, "pwd": pwd, "copy": cp,
                "del": rm, "erase": rm, "show": show, "open": open_cmd,
                "sort": sort, "exec": exec_cmd, "eval": eval_cmd,
                "mkdir": mkdir, "md": mkdir, "move": mv, "rename": mv, 
                "ren": mv, "rmdir": rmdir, 
                "rd": rmdir, "echo": printf, 
                "edit": open_cmd, "switch": switch}
commands = unix_commands
command_set = "unix"
