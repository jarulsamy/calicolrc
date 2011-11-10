

using System;
using Jigsaw;
using System.Collections.Generic;

namespace Jigsaw
{
	public class Exporter
	{
		int indentLevel;
		string indentString = "    ";
		string comment = "#";
		System.IO.FileStream fp;
		string filename = "";
		Jigsaw.Canvas cvs;
		Dictionary<string,bool> Imports;
		static int incr_count = 1;
		bool endBlock = false;
		
		public Exporter (Jigsaw.Canvas cvs)
		{
			this.cvs = cvs;
			ToPython("export.py");
		}
		
		public void ToPython(string filename) {
			this.filename = filename;
			//fp = System.IO.File.OpenWrite(filename);
			indentLevel = 0;
			// Go through blocks first, and look for:
			// * parallel code (multiple start blocks)
			// * Libraries to import
			Imports = new Dictionary<string, bool>();
			foreach (Diagram.CShape s in cvs.shapes) {
				if (s is CControlStart) {
					CControlStart cs = (CControlStart)s;
					if (cs.IsFactory)
						continue;
					// Found a starting place!
					Process(s, false);
				}
			}
			// Ok, now let's write out the file:
			WriteFormatLine("{0} File: {1}", comment, this.filename);
			WriteFormatLine("{0} This Python script was automatically generated", comment);
			WriteFormatLine("{0} by Calico Jigsaw", comment);
			WriteFormatLine("{0} http://calicoproject.org", comment);
			WriteLine("");
			foreach(string key in Imports.Keys) {
					WriteFormatLine("import {0}", key);				
			}
			WriteLine("");
			// now, go through again, and write them out
			incr_count = 1;
			foreach (Diagram.CShape s in cvs.shapes) {
				if (s is CControlStart) {
					CControlStart cs = (CControlStart)s;
					if (cs.IsFactory)
						continue;
					// Found a starting place!
					Process(s, true);
				}
			}
		}
		
		public static string Py(string exp) {
			// FIXME: Expression -> Python
			// true -> True
			// false -> False
			return exp;
		}
		
		public static string GetNextIncr() {
			return String.Format("i{0}", incr_count);
			incr_count++;
		}
		
		public void Process(Diagram.CShape s, bool write) {
			if (s is CControlStart) {
				CControlStart cs = (CControlStart)s;
				if (write)
					WriteMain(cs);
				// Write body:
				indentLevel++;
				endBlock = false;
				foreach (CEdge edge in cs.Edges) {
					if ( edge.Type != EdgeType.In && edge.IsConnected ) 
						Process((Diagram.CShape)edge.LinkedTo.Block, write);
				}
				indentLevel--;
				return;
			} else if (s is CControlIf) {
				CControlIf c = (CControlIf)s;
				if (write)
					WriteFormatLine("if {0}:", Py(c["IfTest"])); 
				indentLevel++;
				CEdge edge = c.IfEdge;
				if ( edge.Type != EdgeType.In && edge.IsConnected ) 
					Process(edge.LinkedTo.Block, write);
				indentLevel--;
			} else if (s is CAssignment) {
				CAssignment c = (CAssignment)s;
				if (write)
					WriteFormatLine("{0} = {1}", c["Variable"], Py(c["Expression"])); 
			} else if (s is CMethodBlock) {
				CMethodBlock c = (CMethodBlock)s;
				string parameter_list = "";
				if (c.names != null) {
					for (int n = 0; n < c.names.Count; n++) {
					  if (parameter_list == "")
					    parameter_list = Py(c[c.names[n]]);
					  else
					    parameter_list += ", " + Py(c[c.names[n]]);
					}
				}
				if (c.return_type.ToString().CompareTo("System.Void") != 0) { // return value
					if (write)
						WriteFormatLine("{0} = {1}.{2}({3})", 
							c["Variable"], 
							c.type_name,
							c.method_name,
							parameter_list); 
					else
						Imports[c.type_name] = true;
				} else {
					if (write)
						WriteFormatLine("{0}.{1}({2})", 
							c.type_name,
							c.method_name,
							parameter_list); 
					else
						Imports[c.type_name] = true;
				}
			} else if (s is CControlEnd) {
				// nothing left to do
				endBlock = true;
				return;
			} else if (s is CControlRepeat) {
				CControlRepeat c = (CControlRepeat)s;
				if (write)
					WriteFormatLine("for {0} in range({1}):", GetNextIncr(), Py(c["Repetitions"])); 
				indentLevel++;
				CEdge edge = c.LoopEdge;
				if ( edge.Type != EdgeType.In && edge.IsConnected ) {
					Process(edge.LinkedTo.Block, write);
				}
				indentLevel--;
			} else if (s is CControlIfElse) {
				CControlIfElse c = (CControlIfElse)s;
				if (write)
					WriteFormatLine("if {0}:", Py(c["IfTest"])); 
				indentLevel++;
				CEdge edge = c.IfEdge;
				if ( edge.Type != EdgeType.In && edge.IsConnected ) 
					Process(edge.LinkedTo.Block, write);
				indentLevel--;
				if (write)
					WriteLine("else:"); 
				indentLevel++;
				edge = c.ElseEdge;
				if ( edge.Type != EdgeType.In && edge.IsConnected ) 
					Process(edge.LinkedTo.Block, write);
				indentLevel--;
			} else if (s is CControlWhile) {
				CControlWhile c = (CControlWhile)s;
				if (write)
					WriteFormatLine("while {1}:", GetNextIncr(), Py(c["WhileTest"])); 
				indentLevel++;
				CEdge edge = c.LoopEdge;
				if ( edge.Type != EdgeType.In && edge.IsConnected ) {
					Process(edge.LinkedTo.Block, write);
				}
				indentLevel--;
			} else if (s is CIOPrint) {
				CIOPrint c = (CIOPrint)s;
				if (write)
					WriteFormatLine("print({0})", Py(c["Expression"])); 
			} else {
				// not handled
				if (write)
					WriteFormatLine("{0} Unhandled block: {1}", comment, s.Text); 
			}
			foreach (CEdge edge in ((CBlock)s).Edges) {
				if (endBlock) {
					return;
				}
				if ( edge.Type != EdgeType.In && edge.IsConnected ) 
						Process((Diagram.CShape)edge.LinkedTo.Block, write);
			}
		}
		
		public static string replicate(string s, int t) {
			string retval = "";
			for (int i=0; i<t; i++) {
				retval += s;
			}
			return retval;
		}
		
		public void WriteMain(CControlStart cs) {
			// FIXME: keep count of mains, increment if more than one, main1(), main2()
			Console.Write(replicate(indentString, indentLevel));
			Console.WriteLine("def main():");
		}

		public void WriteLine(string text) {
			Console.Write(replicate(indentString, indentLevel));
			Console.WriteLine(text);
		}
		public void WriteFormatLine(string text, params object [] args) {
			Console.Write(replicate(indentString, indentLevel));
			Console.WriteLine(text, args);
		}
		
	}
}

