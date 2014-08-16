from __future__ import print_function

from calico import MagicKernel
import os

class EchoKernel(MagicKernel):
    implementation = 'Echo'
    implementation_version = '1.0'
    language = 'no-op'
    language_version = '0.1'
    banner = "Echo kernel - as useful as a parrot"
    env = {}

    def get_usage(self):
        return "This is a usage statement."

    def set_variable(self, name, value):
        """
        Set a variable in the kernel language.
        """
        self.env[name] = value

    def get_help_on(self, expr):
        return "Sorry, no help is available on '%s'." % expr

    def do_execute_direct(self, code):
        try:
            return eval(code.strip(), self.env)
        except:
            try:
                exec code.strip() in self.env
            except:
                return "Error: " + code

if __name__ == '__main__':
    from IPython.kernel.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=EchoKernel)
