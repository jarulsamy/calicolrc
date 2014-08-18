from calico import Magic

class ReloadMagicsMagic(Magic):
    name = "reload_magics"
    def post_process(self, retval):
        self.kernel.reload_magics()
        result = "Magics reloaded: %s\n" % ", ".join(self.kernel.magics.keys())
        self.kernel.Print(result)
        return retval

def register_magics(magics):
    magics[ReloadMagicsMagic.name] = ReloadMagicsMagic
    
