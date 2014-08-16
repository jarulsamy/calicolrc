from calico import Magic
from IPython.display import HTML

class HTMLMagic(Magic):
    name = "html"
    def cell(self, args):
        html = HTML(self.code)
        self.kernel.send_response(self.kernel.iopub_socket, 'display_data', 
                                  {'data': self.kernel.formatter(html)})
        self.evaluate = False

def register_magics(magics):
    magics[HTMLMagic.name] = HTMLMagic
    
