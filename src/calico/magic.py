class Magic(object):
    name = None
    def __init__(self, kernel, code, mtype, args):
        self.kernel = kernel
        self.code = code
        self.mtype = mtype
        self.args = args
        self.evaluate = True
        if mtype == "line":
            self.line(args)
        elif mtype == "cell":
            self.cell(args)
        elif mtype == "notebook":
            self.notebook(args)

    def cell(self, args):
        pass

    def line(self, args):
        pass

    def notebook(self, args):
        """
        Turn on magic for each cell.
        """
        name = "%%" + self.name
        if name in self.kernel.sticky_magics:
            del self.kernel.sticky_magics[name]
        else:
            self.kernel.sticky_magics[name] = args

    def get_code(self):
        return self.code
        
    def post_process(self, retval):
        return retval
