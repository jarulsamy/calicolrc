from calico import Magic
from IPython.display import Javascript

class JavascriptMagic(Magic):
    name = "javascript"
    def cell(self, args):
        jscode = Javascript(self.code)
        self.kernel.send_response(self.kernel.iopub_socket, 'display_data', 
                                  {'data': self.kernel.formatter(jscode)})
        self.evaluate = False

def register_magics(magics):
    magics[JavascriptMagic.name] = JavascriptMagic
    
