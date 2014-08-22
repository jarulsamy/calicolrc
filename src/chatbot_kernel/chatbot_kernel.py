from __future__ import print_function

from IPython.kernel.zmq.kernelbase import Kernel
import aiml
import os

class ChatbotKernel(Kernel):
    implementation = 'Chatbot'
    implementation_version = '1.0'
    language = 'python'
    language_version = '0.1'
    banner = "Chatbot kernel - a chatbot for IPython"
    env = {}

    def __init__(self, *args, **kwargs):
        self.kernel = aiml.Kernel()
        os.chdir(os.path.join(os.path.dirname(aiml.__file__), 
                              "standard"))
        self.kernel.learn("startup.xml")
        self.kernel.respond("load aiml b")
        os.chdir(os.path.join(os.path.dirname(aiml.__file__), 
                              "alice"))
        self.kernel.learn("startup.xml")
        self.kernel.respond("load alice")
        super(ChatbotKernel, self).__init__(*args, **kwargs)

    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):
        message = self.kernel.respond(code)
        stream_content = {'name': 'stdout', 'data': message}
        self.send_response(self.iopub_socket, 'stream', stream_content)
        return {
            'status': 'ok',
            # The base class increments the execution count
            'execution_count': self.execution_count,
            'payload': [],
            'user_expressions': {},
        }

if __name__ == '__main__':
    from IPython.kernel.zmq.kernelapp import IPKernelApp
    IPKernelApp.launch_instance(kernel_class=ChatbotKernel)
