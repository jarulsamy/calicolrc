//  
//  Engine.cs
//  
//  Author:
//       Douglas S. Blank <dblank@cs.brynmawr.edu>
// 
//  Copyright (c) 2011 The Calico Project
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

namespace Calico
{
	
	public class Project {
	}
	
	public class Engine
	{
		public Engine ()
		{
		}
		
		public virtual bool ReadyToExecute(string text) {
			return true;
		}
		
		public virtual bool execute(string text) {
			return true;
		}
		
		public virtual void setup() {
		}
		
		public virtual void start() {
		}
		
		public virtual void set_redirects(string stdout, string stderr, string stdin) {
		}
		
	    public virtual void set_manager(EngineManager manager) {
	  	}
		
	}
	
}