from calico import Magic

class MagicsMagic(Magic):
    name = "magics"
    help_lines = [" %magics - show installed magics"]

    def line(self, args):
        retval = []
        for magic in self.kernel.magics:
            retval.extend(self.kernel.magics[magic].help_lines)
        self.kernel.Print("Line magics:")
        self.kernel.Print("    " + ("\n    ".join(sorted([line for line in retval if line.startswith(" ")]))))
        self.kernel.Print("")
        self.kernel.Print("Cell magics:")
        self.kernel.Print("    " + ("\n    ".join(sorted([line for line in retval if line.startswith("%")]))))
        self.kernel.Print("")
        self.kernel.Print("Shell shortcut:")
        self.kernel.Print("    ! command ... - execute comand in shell")

def register_magics(magics):
    magics[MagicsMagic.name] = MagicsMagic
    

