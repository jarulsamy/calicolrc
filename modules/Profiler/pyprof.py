import clr
clr.AddReference('../Profiler.dll')

from System.IO import StreamReader
from FePy import Profiler

import threading
import atexit

class ProfilerThread(threading.Thread):

    def __init__(self):
        threading.Thread.__init__(self)
        profiler = Profiler()
        atexit.register(profiler.Close)
        self.reader = StreamReader(profiler.Stream)

    def run(self):
        while True:
            line = self.reader.ReadLine()
            if not line:
                break
            print(line)

thread = ProfilerThread()
thread.start()


