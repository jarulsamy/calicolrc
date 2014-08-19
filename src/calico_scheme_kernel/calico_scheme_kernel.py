from __future__ import print_function

from calico import MagicKernel
import calico.scheme
import os

class CalicoSchemeKernel(MagicKernel):
    implementation = 'Scheme'
    implementation_version = '1.0'
    language = 'scheme'
    language_version = '3.0'
    banner = "Calico Scheme"

    def get_usage(self):
        return "This is a usage statement."

    def set_variable(self, name, value):
        """
        Set a variable in the kernel language.
        """
        pass

    def get_help_on(self, expr):
        return "Sorry, no help is available on '%s'." % expr

    def do_execute_direct(self, code):
        try:
            retval = calico.scheme.execute_string_rm(code)
        except:
            return "Unhandled Error: " + code
        if calico.scheme.exception_q(retval):
            self.Error(calico.scheme.get_traceback_string(retval))
            retval = None
        return retval

if __name__ == '__main__':
    from IPython.kernel.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=CalicoSchemeKernel)
