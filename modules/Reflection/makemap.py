import Reflection

maps = {}

def create():
    for assembly in ["Myro", "Graphics"]:
        #maps[assembly] = Reflection.Utils.Mapping(assembly)
        #maps[assembly].Save("level-1")
        maps[assembly] = Reflection.Utils.Mapping(assembly, "level-1")

create()
