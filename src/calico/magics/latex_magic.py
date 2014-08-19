from calico import Magic
from IPython.display import Latex

class LatexMagic(Magic):
    name = "latex"
    help_lines = ["%%latex - display contents of cell as LaTeX",
                  " %latex - display argument as LaTex"]

    def line(self, args):
        latex = Latex(args)
        self.kernel.Display(latex)

    def cell(self, args):
        latex = Latex(self.code)
        self.kernel.Display(latex)
        self.evaluate = False

def register_magics(magics):
    magics[LatexMagic.name] = LatexMagic
    
