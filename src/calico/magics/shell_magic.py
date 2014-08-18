from calico import Magic
import subprocess

class ShellMagic(Magic):
    name = "shell"
    def line(self, args):
        try:
            process = subprocess.Popen(args, shell=True, 
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            retval, error = process.communicate()
            if error:
                retval = error
        except Exception as e:
            retval = e.message
        self.kernel.send_response(self.kernel.iopub_socket, 'display_data', 
                                  {'data': self.kernel.formatter(retval)})

    def cell(self, args):
        try:
            process = subprocess.Popen(self.code, shell=True, 
                                       stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            retval, error = process.communicate()
            if error:
                retval = error
        except Exception as e:
            retval = e.message
        self.kernel.send_response(self.kernel.iopub_socket, 'display_data', 
                                  {'data': self.kernel.formatter(retval)})
        self.execute = False

def register_magics(magics):
    magics[ShellMagic.name] = ShellMagic
    

