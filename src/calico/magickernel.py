from IPython.kernel.zmq.kernelbase import Kernel
import os
import sys
import glob
import base64

class MagicKernel(Kernel):
    def __init__(self, *args, **kwargs):
        super(MagicKernel, self).__init__(*args, **kwargs)
        self.sticky_magics = {}
        self._i = None
        self._ii = None
        self._iii = None
        self._ = None
        self.__ = None
        self.___ = None
        self.reload_magics()

    def reload_magics(self):
        self.magics = {}
        magic_directory = os.path.join(os.path.dirname(os.path.abspath(__file__)), "magics")
        sys.path.append(magic_directory)
        magic_files = glob.glob(os.path.join(magic_directory, "*.py"))
        for magic in magic_files:
            try:
                module = __import__(os.path.splitext(os.path.basename(magic))[0])
                module.register_magics(self.magics)
            except Exception as e:
                print("Skipping %s: %s" % (magic, e.message))

    def get_usage(self):
        return "This is a usage statement."

    def parse(self, text):
        lines = text.split("\n")
        command = lines[0]
        args = []
        code = "\n".join(lines[1:])
        return command, args, code

    def get_magic(self, text):
        # if first line matches a magic,
        # return Magic(self, code, args)
        parts = self.parse(text)
        if parts:
            command, args, code = parts
            if command.startswith("%%%"):
                name = command[3:]
                mtype = "notebook"
            elif command.startswith("%%"):
                name = command[2:]
                mtype = "cell"
            elif command.startswith("%"):
                name = command[1:]
                mtype = "line"
            else:
                return None
            if name in self.magics:
                klass = self.magics[name]
                return klass(self, code, mtype, args)
            else:
                # FIXME: Raise an error
                return None
        return None

    def set_variable(self, name, value):
        """
        Set a variable in the kernel language.
        """
        pass

    def get_help_on(self, expr):
        return "Sorry, no help is available."

    def get_sticky_magics(self):
        retval = ""
        for key in self.sticky_magics:
            retval += (key + " " + " ".join(self.sticky_magics[key])).strip() + "\n"
        return retval

    def formatter(self, data):
        retval = {}
        retval["text/plain"] = repr(data)
        if hasattr(data, "_repr_png_"):
            obj = data._repr_png_()
            if obj:
                retval["image/png"] = base64.encodestring(obj)
        if hasattr(data, "_repr_jpeg_"):
            obj = data._repr_jpeg_()
            if obj:
                retval["image/jpeg"] = base64.encodestring(obj)
        if hasattr(data, "_repr_html_"):
            obj = data._repr_html_()
            if obj:
                retval["text/html"] = obj
        if hasattr(data, "_repr_markdown_"):
            obj = data._repr_markdown_()
            if obj:
                retval["text/markdown"] = obj
        if hasattr(data, "_repr_svg_"):
            obj = data._repr_svg_()
            if obj:
                retval["image/svg+xml"] = obj
        if hasattr(data, "_repr_latex_"):
            obj = data._repr_latex_()
            if obj:
                retval["text/latex"] = obj
        if hasattr(data, "_repr_json_"):
            obj = data._repr_json_()
            if obj:
                retval["application/json"] = obj
        if hasattr(data, "_repr_javascript_"):
            obj = data._repr_javascript_()
            if obj:
                retval["application/javascript"] = obj
        if hasattr(data, "_repr_pdf_"):
            obj = data._repr_pdf_()
            if obj:
                retval["application/pdf"] = obj
        return retval

    def do_execute(self, code, silent, store_history=True, user_expressions=None,
                   allow_stdin=False):
        # Handle Magics
        payload = []
        if code.strip() == "?":            # help!
            payload.append({"start_line_number": 0,
                            "data": {"text/plain": self.get_usage()},
                            "source": "page"})
        elif code.rstrip().endswith("?"):   # doc info
            payload.append({"data": {"text/plain": self.get_help_on(code.rstrip()[:-1])}, 
                            "start_line_number": 0,
                            "source": "page"})
        elif code.startswith("?"): # doc info
            payload.append({"data": {"text/plain": self.get_help_on(code[1:].rstrip())}, 
                            "start_line_number": 0,
                            "source": "page"})
        else:                      # process magics/code/shell
            retval = None
            if self.sticky_magics:
                code = self.get_sticky_magics() + code
            stack = []
            # Handle magics:
            magic = None
            while code.startswith("%"):
                magic = self.get_magic(code)
                if magic != None:
                    stack.append(magic)
                    code = magic.get_code()
                    if not magic.evaluate: # signal to exit, maybe error or no block
                        break
                else:
                    break
            # Execute code, if any:
            if ((magic is None or magic.evaluate) and code.strip() != ""):
                # execute code
                if code.startswith("!"): # shell command
                    import subprocess
                    try:
                        process = subprocess.Popen(code[1:], shell=True, 
                                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
                        retval, error = process.communicate()
                        if error:
                            retval = error
                    except Exception as e:
                        retval = e.message
                else:
                    retval = self.do_execute_direct(code)
            # Post-process magics:
            for magic in reversed(stack):
                retval = magic.post_process(retval)
            ## Handle in's
            self.set_variable("_iii", self._iii);
            self.set_variable("_ii", self._ii);
            self.set_variable("_i", code);
            self.set_variable("_i" + str(self.execution_count), code);
            self._iii = self._ii;
            self._ii = code;
            if (retval is not None):
                ## --------------------------------------
                ## Handle out's (only when non-null)
                self.set_variable("___", self.___)
                self.set_variable("__", self.__)
                self.set_variable("_", retval)
                self.set_variable("_" + str(self.execution_count), retval)
                self.___ = self.__
                self.__ = retval
                content = {'execution_count': self.execution_count, 'data': self.formatter(retval)}
                self.send_response(self.iopub_socket, 'execute_result', content)
        return {
            'status': 'ok',
            # The base class increments the execution count
            'execution_count': self.execution_count,
            'payload': payload,
            'user_expressions': {},
        }

