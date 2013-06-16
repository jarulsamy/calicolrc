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

## ------------------------------------------------------------
## Globals:

lastcd = [os.getcwd()]

## ------------------------------------------------------------
## Commands:

## command(incoming-sequence, tty?, arguments)
## -> returns a sequence [eg, returns a seq, or defines a generator]
## __doc__ : first line, summary; rest for a complete help doc
## all should handle --help

def cd(incoming, tty, args):
    """
    change directory

    To change directories, use cd folder, where folder is either
    relative or absolute.

    see also: pwd, mkdir
    """
    global lastcd
    args, flags = splitArgs(args)
    directory = None
    if len(args) > 0:
        directory = args[0]
    # ---
    if directory == "-":
        if len(lastcd) > 1:
            directory = lastcd[-2]
        else:
            directory = None
        # else pass
    if directory:
        os.chdir(directory)
    else:
        os.chdir(os.path.expanduser("~"))
    lastcd.append(os.getcwd())
    return [lastcd[-1]]

def rm(incoming, tty, args):
    """
    remove file or folders

    Use rm to delete files.
    """
    args, flags = splitArgs(args)
    os.remove(args[0])
    #shutil.rmtree() # removes directory and contents
    return []

def mkdir(incoming, ttyp, args):
    """
    make a directory
    """
    args, flags = splitArgs(args)
    for filename in args:
        os.makedirs(filename)
    return []

def rmdir(incoming, tty, args):
    """
    remove folders

    Use rmdir to delete folders.
    """
    args, flags = splitArgs(args)
    for folder in args:
        os.rmdir(folder) # removes an empty directory
    return []

def cp(incoming, tty, args):
    """
    copy files and folders

    Use cp to copy files.
    """
    args, flags = splitArgs(args)
    shutil.copy(args[0], args[1])
    return []

def mv(incoming, tty, args):
    """
    move files and folders

    Use mv to move files.
    """
    args, flags = splitArgs(args)
    os.rename(args[0], args[1])
    return []

def pwd(incoming, tty, args):
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

def ls(incoming, tty, args):
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

def grep(incoming, tty, args):
    """
    search for matches

    Grep through arguments, searching for matches.
    """
    args, flags = splitArgs(args)
    if len(args) > 0:
        pattern = args[0]
    else:
        raise Exception("grep: error, no pattern given")
    if incoming:
        for i in incoming:
            if match(pattern, i):
                yield i
    else:
        for f in args[1:]:
            if os.path.exists(f) and os.path.isfile(f):
                text = open(f).readlines()
                for line in text:
                    line = line.strip()
                    if match(pattern, line):
                        yield "%s: %s" % (f, line)
            else:
                calico.ErrorLine("grep: no such file '%s'" % f)
    return # ends yields

def more(incoming, tty, args):
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
                raise Exception("more: no such file '%s'" % f)

def cat(incoming, tty, args):
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
                raise Exception("cat: file does not exist: '%s'" % filename)
            fp = open(filename)
            for line in fp:
                i = line.strip()
                count += 1
                if "-n" in flags:
                    yield "%6d %s" % (count, i)
                else:
                    yield i

def sort(incoming, tty, args):
    """
    sort data
    """
    args, flags = splitArgs(args)
    return sorted(list(args) + list(incoming))

def help_cmd(incoming, tty, args):
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
        raise Exception("help: I don't have help on '%s'" % args[0])

def show(incoming, tty, args):
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

def open_cmd(incoming, tty, args):
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

def exec_cmd(incoming, tty, args):
    """
    execute a file in Calico
    """
    if incoming:
        for item in incoming:
            parts = splitParts(item)
            if len(parts) == 1:
                if os.path.isfile(parts[0]):
                    calico.ExecuteFile(parts[0])
                else:
                    raise Exception("exec: no such file: '%s'" % parts[0])
            else:
                calico.Execute(parts[0], parts[1])
    else:
        if len(args) == 1:
            if os.path.isfile(args[0]):
                calico.ExecuteFile(args[0])
            else:
                raise Exception("exec: no such file: '%s'" % args[0])
        else:
            calico.Execute(args[0], args[1])
    return []

def eval_cmd(incoming, tty, args):
    """
    evaluate text in Calico
    """
    args, flags = splitArgs(args)
    if incoming:
        for item in incoming:
            parts = splitParts(item)
            if len(parts) == 2:
                calico.Evaluate(parts[0], parts[1])
            else:
                raise Exception("eval: need to specify a language with '%s'" % parts[0])
    else:
        text = args[0]
        language = args[1]
        return [calico.Evaluate(text, language)]

def echo(incoming, tty, args):
    """
    create output
    """
    args, flags = splitArgs(args)
    return [" ".join(args)]

def printf(incoming, tty, args):
    """
    display output
    """
    args, flags = splitArgs(args)
    return []

def switch(incoming, tty, args):
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
    args = []
    flags = []
    for arg in arg_list:
        arg = arg.strip()
        if arg in ["-", "--"]:
            args.append(arg)
        elif arg.startswith("-"):
            flags.append(arg)
        else:
            args.append(arg)
    return args, flags

def expand(pattern):
    """
    Looks to see if pattern has file-matching characters. If so,
    expand now.
    """
    if pattern.startswith("`"):
        return execute(calico, pattern[1:], True)
    elif pattern.startswith("$"):
        ## expand variable, which could be a pattern:
        return expand(str(calico.Evaluate(pattern[1:], "python")))
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

def splitParts(text):
    retval = []
    i = 0
    current = ""
    mode = "start"
    while i < len(text):
        if mode == "start":
            if text[i].startswith("#"):
                # ignore here to end of line
                break
            elif text[i] == " ":
                if current:
                    retval.extend(expand(current))
                    current = ""
                else:
                    pass ## skip
            elif text[i] == "=":
                if current:
                    retval.append(current)
                    current = ""
                retval.append('=')
            elif text[i] == "'":
                if current:
                    retval.append(current)
                    current = ""
                mode = "quote"
            elif text[i] == '"':
                if current:
                    retval.append(current)
                    current = ""
                mode = "double-quote"
            elif text[i] == '`':
                if current:
                    retval.append(current)
                    # signal expand that this is an expr:
                current = "`"
                mode = "back-quote"
            else:
                current += text[i]
        elif mode == "quote":
            if text[i] == "'":
                retval.append(current)
                current = ""
                mode = "start"
            else:
                current += text[i]
        elif mode == "double-quote":
            if text[i] == '"':
                retval.append(current)
                current = ""
                mode = "start"
            else:
                current += text[i]
        elif mode == "back-quote":
            if text[i] == '`':
                retval.extend(expand(current))
                current = ""
                mode = "start"
            else:
                current += text[i]
        i += 1
    if current: # some still left:
        if mode == "start":
            retval.extend(expand(current))
        elif mode == "quote":
            raise Exception("console: unended quote")
        elif mode == "double-quote":
            raise Exception("console: unended double-quote")
        elif mode == "back-quote":
            raise Exception("console: unended back-quote: leftover: '%s'" % current)
    return retval

def splitLines(command_list):
    commands = []
    command = []
    for symbol in splitParts(command_list):
        if symbol in ["|"]:
            commands.append([command[0], command[1:]])
            command = []
        else:
            command.append(symbol)
    if command:
        commands.append([command[0], command[1:]])
    return commands
            
def execute(calico, text, return_value=False):
    """
    Execute a shell command line.
    """
    # put calico in the environment:
    globals()["calico"] = calico
    try:
        w = calico.Output.Allocation.Width
        h = calico.Output.Allocation.Height
        scale = (calico.GetFont().Size/1024)
        globals()["width"] = int(w/(scale - 2))
        globals()["height"] = int(h/(scale * 1.7))
    except:
        globals()["width"] = 80
        globals()["height"] = 24
    # compute the width, height of the output window:
    # Break the command into parts:
    line = splitLines(text)
    incoming = None
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
            command_name = command_name.lower()
            if command_set == "unix" and command_name.startswith("#"):
                continue
            elif command_set == "dos" and command_name == "rem":
                continue
            if len(args) > 1 and args[0] == "=":
                expr = args[1:]
                calico.Execute("%s = %s" % (command_name, " ".join(expr)), "python")
                continue
            if command_name in commands:
                command = commands[command_name]
            else:
                raise Exception("console: no such command: '%s'. Try 'help'" % command_name)
            incoming = command(incoming, tty, args)
        count += 1
    # and display the output
    if return_value:
        return incoming
    else:
        if incoming:
            display(incoming)

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
                 "edit": open_cmd, "switch": switch, "printf": printf}
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
