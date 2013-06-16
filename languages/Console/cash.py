## Calico Shell: Console

import glob
import os
import re
import shutil

## ------------------------------------------------------------
## Globals:

lastcd = None

## ------------------------------------------------------------
## Commands:

## command(incoming-sequence, tty?, arguments)
## -> returns a sequence [eg, returns a seq, or defines a generator]
## __doc__ : first line, summary; rest for a complete help doc

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
        if lastcd:
            os.chdir(lastcd)
        # else pass
    if directory:
        os.chdir(directory)
    else:
        os.chdir(os.path.expanduser("~"))
    lastcd = os.getcwd()
    return [os.getcwd()]

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

def ls(incoming, tty, args):
    """
    ls - list files

    Use ls to list the files in a folder.

    Flags:
       -a   see all
       -l   long format
    """
    args, flags = splitArgs(args)
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

def grep(incoming, tty, args):
    """
    grep - search for matches

    Grep through arguments, searching for matches.
    """
    args, flags = splitArgs(args)
    if incoming:
        pattern = args[0]
        for i in match(pattern, incoming):
            yield i
    else:
        pass # look in the contents of args
    return

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
                raise Exception("no such file: '%s'" % parts[0])
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
    echo - display output
    """
    args, flags = splitArgs(args)
    return [" ".join(args)]

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
    args = splitParts(arg_text)
    return command, args

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
        command, args = parse(command_text)
        incoming = command(incoming, tty, args)
        count += 1
    display(incoming)

def display(g):
    for item in g:
        print(item)

# FIXME: head tail find pushd popd wget wc cal date du df uname cut plot printf time hostname id < >

# Unix commands
commands = {"ls": ls, "more": more, "cd": cd, "grep": grep,
            "cat": cat, "help": help_cmd, "pwd": pwd, "cp": cp,
            "rm": rm, "less": more, "show": show, "open": open_cmd,
            "sort": sort, "exec": exec_cmd, "eval": eval_cmd,
            "mkdir": mkdir, "mv": mv, "rmdir": rmdir, "echo": echo, 
            "edit": open_cmd}

#execute("ls")
