from calico import Magic
from IPython.display import HTML

class HTMLMagic(Magic):
    name = "html"
    help_lines = ["%%html - display contents of cell as HTML",
                  " %html - display argument as HTML"]

    def line(self, args):
        html = HTML(args)
        self.kernel.Display(html)

    def cell(self, args):
        html = HTML(self.code)
        self.kernel.Display(html)
        self.evaluate = False

def register_magics(magics):
    magics[HTMLMagic.name] = HTMLMagic
    
