## Calico Shell: Console

import glob
import os
import re
import shutil

## ------------------------------------------------------------
## Commands:

## command(incoming-sequence, tty?, arguments, flags)
## -> returns a sequence [eg, returns a seq, or defines a generator]
## __doc__ : first line, summary; rest for a complete help doc

def cd(incoming, tty, args, flags):
    """
    cd - change directory

    To change directories, use cd folder, where folder is either
    relative or absolute.

    see also: pwd, mkdir
    """
    directory = None
    if len(args) > 0:
        directory = args[0]
    # ---
    if directory:
        os.chdir(directory)
    else:
        os.chdir(os.path.expanduser("~"))
    return pwd([], False, [], [])

def rm(incoming, tty, args, flags):
    """
    rm - remove file or folders

    Use rm to delete files.
    """
    os.remove(args[0])
    #shutil.rmtree() # removes directory and contents
    return []

def mkdir(incoming, ttyp, args, flags):
    for filename in args:
        os.makedirs(filename)
    return []

def rmdir(incoming, tty, args, flags):
    """
    rmdir - remove folders

    Use rmdir to delete folders.
    """
    for folder in args:
        os.rmdir(folder) # removes an empty directory
    return []

def cp(incoming, tty, args, flags):
    """
    cp - copy files and folders

    Use cp to copy files.
    """
    shutil.copy(args[0], args[1])
    return []

def mv(incoming, tty, args, flags):
    """
    mv - move files and folders

    Use mv to move files.
    """
    os.rename(args[0], args[1])
    return []

def pwd(incoming, tty, args, flags):
    """
    pwd - print working directory

    Use pwd to see your current directory.
    """
    return [os.getcwd()]

def ls(incoming, tty, args, flags):
    """
    ls - list files

    Use ls to list the files in a folder.

    Flags:
       -a   see all
       -l   long format
    """
    if len(args) > 0:
        patterns = args
    else:
        patterns = ["*"]
    data = []
    for pattern in patterns:
        data.extend(matchFiles(pattern))
    retval = ""
    for f in data:
        if "-l" in flags:
            yield os.path.abspath(f)
        elif tty:
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
        else:
            yield os.path.basename(f)
    if retval:
        yield retval

def grep(incoming, tty, args, flags):
    """
    grep - search for matches

    Grep through arguments, searching for matches.
    """
    if incoming:
        pattern = args[0]
        for i in match(pattern, incoming):
            yield i
    else:
        pass # look in the contents of args
    return

def more(incoming, tty, args, flags):
    """
    more - see output one page at a time
    """
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

def cat(incoming, tty, args, flags):
    """
    cat - concatenate files
    """
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

def sort(incoming, tty, args, flags):
    """
    sort - sort data
    """
    return sorted(list(args) + list(incoming))

def help_cmd(incoming, tty, args, flags):
    """
    help - get help on commands
    """
    if len(args) == 0:
        for command in sorted(commands.keys()):
            yield commands[command].__doc__.strip().split("\n")[0].strip()
    elif args[0] in commands:
        yield commands[args[0]].__doc__.strip()
    else:
        raise Exception("I don't have help on '%s'" % args[0])

def show(incoming, tty, args, flags):
    """
    show - show an image graphically
    """
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

def open_cmd(incoming, tty, args, flags):
    """
    open - open a file in Calico
    """
    if incoming:
        for filename in incoming:
            if os.path.isfile(filename):
                calico.Open(filename)
    else:
        for filename in args:
            if os.path.isfile(filename):
                calico.Open(filename)
    return []

def exec_cmd(incoming, tty, args, flags):
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
                raise Exception("no such file: '%s'" % parts[0])
        else:
            calico.Execute(args[0], args[1])
    return []

def eval_cmd(incoming, tty, args, flags):
    """
    eval - evaluate text in Calico
    """
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

## ------------------------------------------------------------

def matchFiles(pattern):
    return match(pattern, glob.glob("*"))

def match(pattern, data):
    # Dots are literal:
    pattern = pattern.replace(".", "\.")
    # asterisks mean match anything
    pattern = pattern.replace("*", ".*")
    if pattern.startswith("^"):
        pattern = pattern[1:]
    else: ## match anything on front end:
        pattern = ".*" + pattern
    retval = []
    for i in data:
        if re.match(pattern, i):
            retval.append(i)
    return retval

def split(arg_text):
    """
    Split into command and args
    """
    return arg_text.split(" ", 1)

def splitParts(text):
    retval = []
    i = 0
    current = ""
    mode = "start"
    while i < len(text):
        if mode == "start":
            if text[i] == " ":
                if current:
                    retval.append(current)
                    current = ""
                else:
                    pass ## skip
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
            retval.append(current)
        elif mode == "quote":
            retval.append(current)
        elif mode == "double-quote":
            retval.append(current)
    return retval

def splitArgs(arg_text):
    args = []
    flags = []
    for arg in splitParts(arg_text):
        arg = arg.strip()
        if arg in ["-", "--"]:
            args.append(arg)
        elif arg.startswith("-"):
            flags.append(arg)
        else:
            args.append(arg)
    return args, flags

def parse(command_text):
    command_text = command_text.strip()
    if " " in command_text:
        command_name, arg_text = command_text.split(" ", 1)
    else:
        command_name = command_text
        arg_text = ""
    command_name = command_name.strip()
    if command_name in commands:
        command = commands[command_name]
    else:
        raise Exception("No such command: '%s'. Try 'help'" % command_name)
    args, flags = splitArgs(arg_text)
    return command, args, flags

def execute(calico, text):
    globals()["calico"] = calico
    w = calico.Output.Allocation.Width
    h = calico.Output.Allocation.Height
    scale = (calico.GetFont().Size/1024)
    globals()["width"] = int(w/(scale - 2))
    globals()["height"] = int(h/(scale * 1.7))
    line = text.split("|")
    incoming = None
    count = 0
    for command_text in line:
        tty = (count == len(line) - 1) # pipe to a tty or not
        command, args, flags = parse(command_text)
        incoming = command(incoming, tty, args, flags)
        count += 1
    display(incoming)

def display(g):
    for item in g:
        print(item)

# Unix commands
commands = {"ls": ls, "more": more, "cd": cd, "grep": grep,
            "cat": cat, "help": help_cmd, "pwd": pwd, "cp": cp,
            "rm": rm, "less": more, "show": show, "open": open_cmd,
            "sort": sort, "exec": exec_cmd, "eval": eval_cmd,
            "mkdir": mkdir, "mv": mv, "rmdir": rmdir}

#execute("ls")
