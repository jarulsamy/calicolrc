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
    cd - change directory

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
    rm - remove file or folders

    Use rm to delete files.
    """
    args, flags = splitArgs(args)
    os.remove(args[0])
    #shutil.rmtree() # removes directory and contents
    return []

def mkdir(incoming, ttyp, args):
    """
    mkdir - make a directory
    """
    args, flags = splitArgs(args)
    for filename in args:
        os.makedirs(filename)
    return []

def rmdir(incoming, tty, args):
    """
    rmdir - remove folders

    Use rmdir to delete folders.
    """
    args, flags = splitArgs(args)
    for folder in args:
        os.rmdir(folder) # removes an empty directory
    return []

def cp(incoming, tty, args):
    """
    cp - copy files and folders

    Use cp to copy files.
    """
    args, flags = splitArgs(args)
    shutil.copy(args[0], args[1])
    return []

def mv(incoming, tty, args):
    """
    mv - move files and folders

    Use mv to move files.
    """
    args, flags = splitArgs(args)
    os.rename(args[0], args[1])
    return []

def pwd(incoming, tty, args):
    """
    pwd - print working directory

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
            if len(retval + " " + str(i)) < width:
                retval += " " + str(i)
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
    ls - list files

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
    grep - search for matches

    Grep through arguments, searching for matches.
    """
    args, flags = splitArgs(args)
    if incoming:
        if len(args) > 0:
            pattern = args[0]
            for i in incoming:
                if match(pattern, i):
                    yield i
    else:
        ## FIXME: look in the contents of args
        pass 
    return # ends yields

def more(incoming, tty, args):
    """
    more - see output one page at a time
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
    #FIXME: cat file contents
    #else: # 
    #    for arg in args:

def cat(incoming, tty, args):
    """
    cat - concatenate files
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
    sort - sort data
    """
    args, flags = splitArgs(args)
    return sorted(list(args) + list(incoming))

def help_cmd(incoming, tty, args):
    """
    help - get help on commands
    """
    args, flags = splitArgs(args)
    if "--help" in flags:
        yield inspect.getdoc(help_cmd)
        return
    if len(args) == 0:
        for command in sorted(commands.keys()):
            yield commands[command].__doc__.strip().split("\n")[0].strip()
    elif args[0] in commands:
        yield commands[args[0]].__doc__.strip()
    else:
        raise Exception("I don't have help on '%s'" % args[0])

def show(incoming, tty, args):
    """
    show - show an image graphically
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
    open - open a file in Calico
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
    exec - execute a file in Calico
    """
    if incoming:
        for item in incoming:
            parts = splitParts(item)
            if len(parts) == 1:
                if os.path.isfile(parts[0]):
                    calico.ExecuteFile(parts[0])
                else:
                    raise Exception("no such file: '%s'" % parts[0])
            else:
                calico.Execute(parts[0], parts[1])
    else:
        if len(args) == 1:
            if os.path.isfile(args[0]):
                calico.ExecuteFile(args[0])
            else:
                raise Exception("no such file: '%s'" % args[0])
        else:
            calico.Execute(args[0], args[1])
    return []

def eval_cmd(incoming, tty, args):
    """
    eval - evaluate text in Calico
    """
    args, flags = splitArgs(args)
    if incoming:
        for item in incoming:
            parts = splitParts(item)
            if len(parts) == 2:
                calico.Evaluate(parts[0], parts[1])
            else:
                raise Exception("no such file: '%s'" % parts[0])
    else:
        text = args[0]
        language = args[1]
        return [calico.Evaluate(text, language)]

def echo(incoming, tty, args):
    """
    echo - create output
    """
    args, flags = splitArgs(args)
    return [" ".join(args)]

def printf(incoming, tty, args):
    """
    print - display output
    """
    args, flags = splitArgs(args)
    print(" ".join(args))
    return []

def switch(incoming, tty, args):
    """
    switch - to unix or dos
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
    if ("*" in pattern or 
        "?" in pattern or
        "~" in pattern):
        pattern = pattern.replace("~", os.path.expanduser("~"))
        return match(pattern, glob.glob("*"))
    elif pattern.startswith("$"):
        ## expand variable, which could be a pattern:
        return expand(str(calico.Evaluate(pattern[1:], "python")))
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
                    retval.extend(expand(current))
                    current = ""
                else:
                    pass ## skip
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
        i += 1
    if current:
        if mode == "start":
            retval.extend(expand(current))
        elif mode == "quote":
            raise Exception("Unended quote")
        elif mode == "double-quote":
            raise Exception("Unended double-quote")
    return retval

def parse(command_text):
    """
    Break the command_text up into the command, and args + flags.
    """
    command_line = splitParts(command_text)
    if command_line:
        command_name = command_line[0]
        args = command_line[1:]
        # Make lower-case and clean up:
        command_name = command_name.lower()
        command_name = command_name.strip()
        return command_name, args
    else:
        return []

def execute(calico, text):
    """
    Execute a shell command line.
    """
    # put calico in the environment:
    globals()["calico"] = calico
    w = calico.Output.Allocation.Width
    h = calico.Output.Allocation.Height
    scale = (calico.GetFont().Size/1024)
    # compute the width, height of the output window:
    globals()["width"] = int(w/(scale - 2))
    globals()["height"] = int(h/(scale * 1.7))
    # Break the command into parts:
    line = text.split("|")
    incoming = None
    count = 0
    # Process each command, chaining to the next:
    incoming = None
    for command_text in line:
        command_text = command_text.strip()
        if not command_text:
            continue
        # only last one is a tty
        tty = (count == len(line) - 1) # pipe to a tty or not
        data = parse(command_text)
        if len(data) == 2:
            command_name, args = data
        else:
            continue
        if command_name:
            if command_set == "unix" and command_name.startswith("#"):
                continue
            elif command_set == "dos" and command_name == "rem":
                continue
            if len(args) > 1 and args[0] == "=":
                calico.Execute("%s = %s" % (command_name, " ".join(args[1:])), "python")
                continue
            if command_name in commands:
                command = commands[command_name]
            else:
                raise Exception("No such command: '%s'. Try 'help'" % command_name)
            incoming = command(incoming, tty, args)
        count += 1
    # and display the output
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
