//  
//  CustomStream.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2011-2014 The Calico Project
// 
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
// 
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
// 
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
using System;
using System.IO;

namespace Calico {
    public enum Tag {Error, Warning, Info, Normal}

    public class CustomStream : Stream {
        public Calico.Tag tag;
        Calico.MainWindow calico;
	bool DEBUG = false;
	// FIXME: Ruby has strange \r\n and a zero-length string
	// that messes up the output when not in a console (eg, web)
	// This hack makes output visible:
	bool skip_next = false; 

        public CustomStream(Calico.MainWindow calico, Tag tag) : base() {
            this.calico = calico;
            this.tag = tag;
        }

        public override void Write(Byte [] bytes, int offset, int count) {
            // FIXME: Ruby generates \r\n 13 10
            if (bytes[0] == 10 && skip_next) {
		skip_next = false;
		return;
	    }
            if (count == 3 && offset == 0 && bytes [0] == 239 && bytes [1] == 187 && bytes [2] == 191) {
                // clear screen?
            } else {
		if (count > 0) {
		    string text = System.Text.Encoding.UTF8.GetString(bytes, offset, count);
		    if (DEBUG) {
			if (count == 1)
			    PrintLine(tag, String.Format("DEBUG: Write(\"{0}\", {1}, {2})", bytes[0], offset, count));
			else if (count == 2)
			    PrintLine(tag, String.Format("DEBUG: Write(\"{0}{1}\", {2}, {3})", bytes[0], bytes[1], offset, count));
			else
			    PrintLine(tag, String.Format("DEBUG: Write(\"{0}\", {1}, {2})", text, offset, count));
		    }
		    calico.Print(tag, text);              
		} else {
		    if (DEBUG) PrintLine(tag, String.Format("DEBUG: Write(\"{0}\", {1}, {2})", "", offset, count));
		    calico.PrintLine(tag, "");              
		    skip_next = true;
		}
            }
        }

        public override void WriteByte(Byte b){
	    if (DEBUG) PrintLine(tag, String.Format("DEBUG: WriteByte({0})", b));
            Write(new byte[] {b}, 0, 1);
        }

        public void PrintLine(string text) {
            calico.Print(tag, text + "\n");
        }

        public void Print(string text) {
            calico.Print(tag, text);
        }

        public void PrintLine(Calico.Tag mytag, string text) {
            calico.Print(mytag, text + "\n");
        }

        public void Print(Calico.Tag mytag, string text) {
            calico.Print(mytag, text);
        }

        public override int Read(Byte [] bytes, int offset, int count) {
            return 0;
        }

        public override bool CanRead {
            get {
		if (DEBUG) PrintLine(tag, String.Format("DEBUG: CanRead -> false"));
		return false;
	    }
        }

        public override bool CanSeek {
            get {
		if (DEBUG) PrintLine(tag, String.Format("DEBUG: CanSeek -> false"));
		return false;
	    }
        }

        public override bool CanWrite {
            get {
		if (DEBUG) PrintLine(tag, String.Format("DEBUG: CanWrite -> true"));
		return true;
	    }
        }

        public override void Flush() {
        }

        public override void Close() {
        }

        public override long Length {
            get {return 0;}
        }

        public override long Position {
            get {return 0;}
            set {}
        }

        public override long Seek(long l, SeekOrigin origin) {
            return 0;
        }

        public void SetLength() {
        }

        public override void SetLength(long l) {
	    if (DEBUG) PrintLine(tag, String.Format("DEBUG: SetLength({0})", l));
        }
    }
}
