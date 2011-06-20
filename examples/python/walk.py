ignore = [
"CompareTo",
"Equals",
"Finalize",
"Format",
"GetHashCode",
"GetName",
"GetNames",
"GetType",
"GetTypeCode",
"GetUnderlyingType",
"GetValues",
"IsDefined",
"MemberwiseClone",
"Parse",
"ReferenceEquals",
"ToBoolean",
"ToByte",
"ToChar",
"ToDateTime",
"ToDecimal",
"ToDouble",
"ToInt16",
"ToInt32",
"ToInt64",
"ToObject",
"ToSByte",
"ToSingle",
"ToString",
"ToType",
"ToUInt16",
"ToUInt32",
"ToUInt64",
"PropertyType",
"GetValue",
"DeclaringType",
"Info",
"SetValue",
"FieldType",
]

def walk(todo, depth=0, maxDepth=4):
    expand = []
    for obj in todo:
        items = obj.split(".")
        if "__" in items[-1] or items[-1] in ignore:
            pass
        else:
            print obj
            try:
                expand += ["%s.%s" % (obj,i) for i in dir(eval(obj))]
            except:
                pass
    if depth + 1 < maxDepth:
        walk(expand, depth + 1, maxDepth)

def search(todo, text, depth=0, maxDepth=4):
    expand = []
    for obj in todo:
        items = obj.split(".")
        if "__" in items[-1] or items[-1] in ignore:
            pass
        else:
            if text in obj:
                print "Found:", obj
            try:
                expand += ["%s.%s" % (obj,i) for i in dir(eval(obj))]
            except:
                pass
    if depth + 1 < maxDepth:
        search(expand, text, depth + 1, maxDepth)

