from calico import Magic
from IPython.display import display_javascript, Javascript

class JavascriptMagic(Magic):
    name = "javascript"
    def cell(self, args):
        display_javascript(Javascript(self.code));
        self.evaluate = False

def register_magics(magics):
    magics[JavascriptMagic.name] = JavascriptMagic
    
