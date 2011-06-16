/*
Calico - Scripting Environment

Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

$Id: $
*/

using System;
using System.Diagnostics; // Process
using System.IO; // DirectoryInfo, FileInfo
using System.IO.Ports;
using System.Threading;
using IronPython.Runtime; // List
using IronRuby.Builtins; // RubyArray
using System.Collections.Generic; // IList
using System.Collections; // IEnumerator

public static class Games {

  public static void beep() {
    
  }
  
  public static MemoryStream createTone(int length, int frequency) {
    byte [] array = new byte[length];
    double angle = 0.0;
    for (int i=0; i < length; i++) {
      array[i] = (byte)(255 * Math.Cos(angle)); // 255 amplitude?
      angle += 3.14159 / frequency;
      if (angle > 2.0 * 3.14159) {
	angle -= 2.0 * 3.14159;
      }
    }
    return new MemoryStream(array);
  }
}