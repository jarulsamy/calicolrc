using System.IO;
using System.Runtime.InteropServices;

using Mono.Unix;

namespace FePy {
    public class Profiler {
        [DllImport("libmono-profiler-py.so")]
        private static extern int pyprof_pipe();

        [DllImport("libmono-profiler-py.so")]
        private static extern void pyprof_close();

        private int handle;
        private Stream stream;

        public Profiler() {
            handle = pyprof_pipe();
            stream = new UnixStream(handle);
        }

        public Stream Stream {
            get {
                return stream;
            }
        }

        public void Close() {
            pyprof_close();
        }
    }
}
