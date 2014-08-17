from calico import Magic

class ConnectFileMagic(Magic):
    name = "connect_file"

    def line(self, args):
        connection_file = self.kernel.config["IPKernelApp"]["connection_file"]
        config = eval(open(connection_file).read())
        retval = """{
  "stdin_port": %(stdin_port)s,
  "shell_port": %(shell_port)s,
  "iopub_port": %(iopub_port)s,
  "hb_port": %(hb_port)s,
  "ip": "%(ip)s",
  "key": "%(key)s",
  "signature_scheme": "%(signature_scheme)s",
  "transport": "%(transport)s"
}

Paste the above JSON into a file, and connect with:
    $> ipython <app> --existing <file>
or, if you are local, you can connect with just:
    $> ipython <app> --existing %(key)s 

or even just:
    $> ipython <app> --existing
if this is the most recent ICalico session you have started.
""" % config
        self.Print(retval)

    def cell(self, args):
        self.line(args)

def register_magics(magics):
    magics[ConnectFileMagic.name] = ConnectFileMagic
