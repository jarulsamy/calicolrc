from calico import Magic
import subprocess

class ShellMagic(Magic):
    name = "shell"
    help_lines = [" %shell - run the line as a shell command",
                  "%%shell - run the contents of the cell as shell commands"]

    def line(self, args):
        try:
            process = subprocess.Popen(args, shell=True, 
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            retval, error = process.communicate()
            if error:
                self.kernel.Error(error)
        except Exception as e:
            self.kernel.Error(e.message)
            retval = None
        self.kernel.Print(retval)

    def cell(self, args):
        self.line(self.code)
        self.evaluate = False

def register_magics(magics):
    magics[ShellMagic.name] = ShellMagic
    

