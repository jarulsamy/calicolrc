## Calico Shell: Console

import glob
import os
import re
import shutil

## ------------------------------------------------------------
## Commands:

## command(incoming-sequence, tty?, arguments, flags)
## -> returns a sequence [eg, returns a seq, or defines a generator]

def cd(incoming, tty, args, flags):
    """
    cd - change directory
    """
    directory = None
    if len(args) > 0:
        directory = args[0]
    if directory:
        os.chdir(directory)
    else:
        os.chdir(os.path.expanduser("~"))
    return pwd([], False, [], [])

def rm(incoming, tty, args, flags):
    """
    rm - remove file or folders
    """
    os.remove(args[0])
    #os.rmdir() # removes an empty directory
    #shutil.rmtree() # removes directory and contents
    return []

def cp(incoming, tty, args, flags):
    """
    cp - copy files and folders
    """
    shutil.copy(args[0], args[1])
    return []

def pwd(incoming, tty, args, flags):
    """
    pwd - print working directory
    """
    return [os.getcwd()]

def ls(incoming, tty, args, flags):
    """
    ls - list files
    """
    pattern = "*"
    if incoming:
        data = match(pattern, incoming)
    else:
        data = match(pattern, glob.glob("*"))
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
    """
    pattern = args[0]
    for i in match(pattern, incoming):
        yield i

def more(incoming, tty, args, flags):
    """
    more - see output one page at a time
    """
    count = 0
    lines = height - 2
    for i in incoming:
        count += 1
        yield i
        if tty and (count % lines == 0):
            # can you tell if there are more?
            print("--more--")
            yn = calico.yesno("More?")
            if not yn:
                return

def cat(incoming, tty, args, flags):
    """
    cat - concatenate files
    """
    count = 0
    for i in incoming:
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
            yield commands[command].__doc__.strip()
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


## ------------------------------------------------------------

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
    # FIXME: parse for strings, escapes, etc
    return arg_text.split()

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
    args = []
    flags = []
    restAreFlags = False
    for arg in split(arg_text):
        arg = arg.strip()
        if restAreFlags:
            flags.append(arg)
        elif arg.startswith("-"):
            flags.append(arg)
            restAreFlags = True
        else:
            args.append(arg)
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
            "sort": sort}

#execute("ls")
