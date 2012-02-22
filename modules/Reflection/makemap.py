import Reflection

maps = {}

def create():
    for assembly in ["mscorlib"]:
        maps[assembly] = Reflection.Utils.Mapping(assembly)
        maps[assembly].SaveAsCSharp("Math.cs")
        #maps[assembly] = Reflection.Utils.Mapping(assembly, "level-1")

create()
