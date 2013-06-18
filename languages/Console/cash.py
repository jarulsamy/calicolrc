from __future__ import print_function, division

## Calico Shell: Console

## A cross-platform bash-like shell, integrated with Calico
## http://calicoproject.org/Calico_Console

## GPL, version 3 or greater
## Doug Blank

import glob
import os
import re
import shutil
import inspect
import traceback
import clr
clr.AddReference("Calico")
import Calico
import time
import argparse

## ------------------------------------------------------------
## Globals:

lastcd = [os.getcwd()]
stack = []
trace = False
trace_pause = False
debug = False

def setTrace(value):
    global trace
    trace = value

def setPause(value):
    global trace_pause
    trace_pause = value

class ConsoleException(Exception):
    def __init__(self, message, stack):
        self.message = message
        self.stack = stack

## ------------------------------------------------------------
## Commands:

## command(name, incoming-sequence, tty?, arguments, stack)
## -> returns a sequence [eg, returns a seq, or defines a generator]
## __doc__ : first line, summary; rest for a complete help doc
## all should handle -h --help
## 

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
    os.rename(args[0], args[1])
    return []

def pwd(name, incoming, tty, args, stack):
    """
    print working directory

    Use pwd to see your current directory.
    """
    args, flags = splitArgs(args)
    return [os.getcwd()]

def list_file(f, flags, tty):
    """
    List a file
    """
    if "-l" in flags:
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

def list_dir(d, flags, tty):
    """
    List the contents of a directory
    """
    retval = ""
    if "-l" in flags or not tty:
        pass
    else:
        yield "%s:" % os.path.relpath(d)
    for f in glob.glob(os.path.join(d, "*")):
        if "-l" in flags or not tty:
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
    yield ""

def ls(name, incoming, tty, args, stack):
    """
    list files

    Use ls to list the files in a folder.

    Flags:
       -a   see all
       -l   long format
    """
    args, flags = splitArgs(args)
    if incoming:
        data = incoming
    else:
        data = args
    if not data:
        data = ["."]
    for f in data:
        if not os.path.exists(f):
            calico.ErrorLine("ls: cannot access '%s': no such file for directory" % f)
            continue
        if os.path.isfile(f):
            yield list_file(f, flags, tty)
        else:
            for item in list_dir(f, flags, tty):
                yield item

def grep(name, incoming, tty, args, stack):
    """
    search for matches

    Grep through arguments, searching for matches.
    """
    args, flags = splitArgs(args)
    if len(args) > 0:
        pattern = args[0]
    else:
        raise ConsoleException("grep: error, no pattern given", stack)
    if incoming:
        for i in incoming:
            if "-v" in flags:
                if not match(pattern, i):
                    yield i
            elif match(pattern, i):
                yield i
    else:
        for f in args[1:]:
            if os.path.exists(f) and os.path.isfile(f):
                text = open(f).readlines()
                for line in text:
                    line = line.strip()
                    if "-v" in flags:
                        if not match(pattern, line):
                            yield "%s: %s" % (f, line)
                    elif match(pattern, line):
                        yield "%s: %s" % (f, line)
            else:
                calico.ErrorLine("grep: no such file '%s'" % f)
    return # ends yields

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
            else:
                calico.ErrorLine("wc: no such file '%s'" % f)
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
                    yield line.strip()
                    if tty and (count % lines == 0):
                        # can you tell if there are more?
                        print("--more--")
                        yn = calico.yesno("More?")
                        if not yn:
                            return
            else:
                raise ConsoleException("more: no such file '%s'" % f, stack)

def cat(name, incoming, tty, args, stack):
    """
    concatenate files
    """
    args, flags = splitArgs(args)
    count = 0
    if incoming:
        for i in incoming:
            count += 1
            if "-n" in flags:
                yield "%6d %s" % (count, i)
            else:
                yield i
    else:
        for filename in args:
            if not os.path.exists(filename) or not os.path.isfile(filename):
                raise ConsoleException("cat: file does not exist: '%s'" % filename, stack)
            fp = open(filename)
            for line in fp:
                i = line.strip()
                count += 1
                if "-n" in flags:
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
    args, flags = splitArgs(args)
    if "--help" in flags:
        yield inspect.getdoc(help_cmd)
        return
    if len(args) == 0:
        for command in sorted(commands.keys()):
            command_name = command
            if command_set == "dos":
                command_name = command.upper()
            yield "%s - %s" % (command_name, commands[command].__doc__.strip().split("\n")[0].strip())
    elif args[0] in commands:
        command_name = args[0]
        if command_set == "dos":
            command_name = args[0].upper()
        yield ("%s: " % command_name) + commands[args[0]].__doc__.strip()
    else:
        raise ConsoleException("help: I don't have help on '%s'" % args[0], stack)

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
        for filename in args:
            if os.path.isfile(filename):
                pic = Myro.makePicture(filename)
                Myro.show(pic, filename)
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
        for filename in args:
            if os.path.isfile(filename):
                calico.Open(filename)
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
                    calico.ExecuteFile(parts[0])
                else:
                    raise ConsoleException("exec: no such file: '%s'" % parts[0], stack)
            else:
                return [calico.Execute(parts[0], parts[1])]
    else:
        if len(args) == 1:
            if os.path.isfile(args[0]):
                calico.ExecuteFile(args[0])
            else:
                raise ConsoleException("exec: no such file: '%s'" % args[0], stack)
        else:
            return [calico.Execute(args[0], args[1])]
    return []

def eval_cmd(name, incoming, tty, args, stack):
    """
    evaluate text in Calico
    """
    args, flags = splitArgs(args)
    if incoming:
        for item in incoming:
            parts = splitParts(item, stack)
            if len(parts) == 2:
                return [calico.Evaluate(parts[0], parts[1])]
            else:
                raise ConsoleException("eval: need to specify a language with '%s'" % parts[0], stack)
    else:
        text = args[0]
        language = args[1]
        return [calico.Evaluate(text, language)]

def echo(name, incoming, tty, args, stack):
    """
    create output
    """
    args, flags = splitArgs(args)
    data = " ".join(args)
    if "-e" in flags:
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
            calico.ErrorLine("switch: cannot switch to '%s': use 'unix' or 'dos'" % args[0])
    else:
        return [command_set]
    return []

## ------------------------------------------------------------

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
    if debug: print("splitParts:", text, stack)
    retval = []
    i = 0
    current = makeAnnotatedString("")
    mode = "start"
    while i < len(text):
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
            raise ConsoleException("console: unended quote", stack)
        elif mode == "double-quote":
            raise ConsoleException("console: unended double-quote", stack)
        elif mode == "back-quote":
            raise ConsoleException("console: unended back-quote: leftover: '%s'" % current, stack)
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
            calico.ErrorLine("".join(traceback.format_exc()))
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
            if len(args) > 1 and args[0] == "=":
                expr = args[1:]
                calico.Execute("%s = %s" % (command_name, " ".join(expr)), "python")
                continue
            if command_name.lower() in commands:
                command = commands[command_name.lower()]
            else:
                raise ConsoleException("console: no such command: '%s'. Try 'help'" % command_name, stack)
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

def display(g):
    """
    The terminal emulator for displaying output.
    """
    for item in g:
        print(item)

# FIXME add these: head tail find pushd popd wget wc cal date du df
# uname cut plot time hostname id < > cls TAB-completion

# Commands
unix_commands = {"ls": ls, "more": more, "cd": cd, "grep": grep,
                 "cat": cat, "help": help_cmd, "pwd": pwd, "cp": cp,
                 "rm": rm, "less": more, "show": show, "open": open_cmd,
                 "sort": sort, "exec": exec_cmd, "eval": eval_cmd,
                 "mkdir": mkdir, "mv": mv, "rmdir": rmdir, "echo": echo, 
                 "edit": open_cmd, "switch": switch, "printf": printf, 
                 "wc": wc}
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
