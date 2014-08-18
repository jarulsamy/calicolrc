from calico import Magic
from IPython.display import Javascript

class JavascriptMagic(Magic):
    name = "javascript"

    def line(self, args):
        jscode = Javascript(args)
        self.kernel.send_response(self.kernel.iopub_socket, 'display_data', 
                                  {'data': self.kernel.formatter(jscode)})

    def cell(self, args):
        if self.code.strip():
            jscode = Javascript(self.code)
            self.kernel.send_response(self.kernel.iopub_socket, 'display_data', 
                                      {'data': self.kernel.formatter(jscode)})
            self.evaluate = False

def register_magics(magics):
    magics[JavascriptMagic.name] = JavascriptMagic
    
