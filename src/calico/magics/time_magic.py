from calico import Magic
import time

class TimeMagic(Magic):
    name = "time"
    def cell(self, args):
        self.start = time.time()

    def line(self, args):
        self.start = time.time()

    def notebook(self, args):
        super(TimeMagic, self).notebook(args)
        self.start = time.time()

    def post_process(self, retval):
        if self.code.strip():
            result = "Time: %s seconds.\n" % (time.time() - self.start)
            self.Print(result)
        return retval

def register_magics(magics):
    magics[TimeMagic.name] = TimeMagic
    
