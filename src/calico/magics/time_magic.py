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
        result = "Time: %s seconds.\n" % (time.time() - self.start)
        stream_content = {'name': 'stdout', 'data': result}
        self.kernel.send_response(self.kernel.iopub_socket, 'stream', stream_content)
        return retval

def register_magics(magics):
    magics[TimeMagic.name] = TimeMagic
    
