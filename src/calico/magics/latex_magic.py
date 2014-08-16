from calico import Magic
from IPython.display import Latex

class LatexMagic(Magic):
    name = "latex"
    def cell(self, args):
        latex = Latex(self.code)
        self.kernel.send_response(self.kernel.iopub_socket, 'display_data', 
                                  {'data': self.kernel.formatter(latex)})
        self.evaluate = False

def register_magics(magics):
    magics[LatexMagic.name] = LatexMagic
    
