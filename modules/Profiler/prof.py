depth = 0

def format(frame):
    filename = frame.f_code.co_filename
    name = frame.f_code.co_name
    return '%s:%s' % (filename, name)

def f(frame, event, arg):
    global depth
    if event == 'call':
        name, indent = format(frame), ' ' * depth
        print '%sENTER: %s' % (indent, name)
        depth += 1
    elif event == 'return':
        depth -= 1
        name, indent = format(frame), ' ' * depth
        print '%sLEAVE: %s' % (indent, name)

import sys
sys.setprofile(f)
execfile(sys.argv[1])
sys.setprofile(None)
