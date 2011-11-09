using System;
using Jigsaw;

namespace Jigsaw
{
	public class ExportAsPython
	{
		int indentLevel;
		string indentString = "    ";
		
		public ExportAsPython (Jigsaw.Canvas cvs)
		{
			indentLevel = 0;
			// Go through first, and look for:
			// parallel code (multiple start blocks)
			foreach (Diagram.CShape s in cvs.shapes) {
				if (s is CControlStart) {
					CControlStart cs = (CControlStart)s;
					if (cs.IsFactory)
						continue;
					// Found a starting place
					WriteMain(cs);
					// Write body:
					indentLevel++;
					foreach (CEdge edge in cs.Edges) {
						if ( edge.Type != EdgeType.In && edge.IsConnected ) 
							Process((Diagram.CShape)edge.LinkedTo.Block);
					}
					indentLevel--;
				}
			}
		}
		
		public void Process(Diagram.CShape s) {
			if (s is CControlIf) {
				CControlIf c = (CControlIf)s;
				WriteFormatLine("if {0}:", c["IfTest"]); // Expression -> Python
				indentLevel++;
				foreach (CEdge edge in c.Edges) {
					if ( edge.Type != EdgeType.In && edge.IsConnected ) 
						Process(edge.LinkedTo.Block);
				}
				indentLevel--;
			} else if (s is CAssignment) {
				CAssignment c = (CAssignment)s;
				WriteFormatLine("{0} = {1}", c["Variable"], c["Expression"]); // Expression -> Python
			} else {
				// not handled yet
				WriteLine("FIXME");
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

