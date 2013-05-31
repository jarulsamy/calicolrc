//
//  IOBlocks.cs
//  
//  Author:
//       Mark F. Russo <russomf@gmail.com>
// 
//  Copyright (c) 2013 The Calico Project
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
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using Cairo;
using Microsoft.Scripting.Hosting;

namespace Jigsaw
{
	// -----------------------------------------------------------------------
    public class CInputOutput : CBlock
    {	// Input/Output block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CInputOutput(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)}),
				palette )
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkBlue;
			this.FillColor = Diagram.Colors.LightBlue;
			this.Sizable = false;
		}
		
		public CInputOutput(Double X, Double Y) : this(X, Y, null) {}
    }
	
	// -----------------------------------------------------------------------
    public class CIOPrint : CInputOutput
    {	// Print block shape class
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOPrint(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Expr = new CExpressionProperty("Expression", "'hello'");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Expression"] = Expr;
			this.OnPropertyChanged(null, null);
		}
		public CIOPrint(Double X, Double Y) : this(X, Y, null) {}
				
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("print({0})", this["Expression"]);
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python statement - - - - - - - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				o.AppendFormat("{0}print({1})\n", sindent, this["Expression"]);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CIOPrint.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Expr = (CExpressionProperty)_properties["Expression"];
			try {
				Expr.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public static string Repr(object obj)
		{
			return Repr(obj, 0);
		}

		public static string Repr(object obj, int depth)
		{
			if (depth > 3) // FIXME: cheap trick to avoid recursive data structures
				return "...";
			if (obj == null) {
				return "None";
//			} else if (obj is IronPython.Runtime.List) {
//				return ((IronPython.Runtime.List)obj).__repr__ (
//		  			IronPython.Runtime.DefaultContext.Default);
//			} else if (obj is IronPython.Runtime.PythonTuple) {
//				return obj.ToString();
			} else if (obj is Array) {
				return ArrayTypeToString((Array)obj, depth);
			} else if (obj is object []) {
				return ArrayTypeToString((object [])obj, depth);
			} else if (obj is IList) {
				return ListToString((IList)obj, depth);
			} else if (obj is IDictionary) {
				return DictionaryToString((IDictionary)obj, depth);
			} else if (obj is IronPython.Runtime.SetCollection) {
				return SetToString((IronPython.Runtime.SetCollection)obj, depth);
			} else {
				return obj.ToString ();
			}
		}
		
		public static string SetToString(IronPython.Runtime.SetCollection set, int depth)
		{
		    string retval = "";
		    foreach(object item in set) {
			if (retval != "") {
			    retval += ", ";
			}
			retval += Repr(item, depth + 1);
		    }
		    return "Set(" + retval + ")";
		}

		public static string ArrayTypeToString(Array args, int depth) 
		{
		    string retval = "";
		    if (args != null) {
			int count = ((Array)args).Length;
			for (int i = 0; i < count; i++) {
			    if (retval != "") {
				retval += ", ";
			    }
			    retval += Repr(args.GetValue(i), depth + 1);
			}
		    }
		    return "[" + retval + "]";
		}

		public static string ArrayToString(object[] args, int depth)
		{
			string retval = "";
			if (args != null) {
				int count = ((Array)args).Length;
				for (int i = 0; i < count; i++) {
					if (retval != "")
						retval += ", ";
					retval += Repr(args[i], depth + 1);
				}
			}
			return String.Format("Array[{0}]", retval);
		}

		public static string ListToString(IList args, int depth)
		{
			string retval = "";
			if (args != null) {
				foreach(object item in args) {
					if (retval != "")
						retval += ", ";
					retval += Repr(item, depth + 1);
				}
			}
			return String.Format("[{0}]", retval);
		}

		public static string DictionaryToString(IDictionary args, int depth)
		{
			string retval = "";
			if (args != null) {
				foreach(object key in args.Keys) {
					if (retval != "")
						retval += ", ";
					retval += String.Format("{0}: {1}", 
								Repr(key, depth + 1), 
								Repr(args[key], depth + 1));
				}
			}
			return String.Format("{{{0}}}", retval);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute print statement

			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
				yield return rr;
			}
			
			// - - - Do the print - - - - - - - - - - - - - - - - - - - -

			try {
				CExpressionProperty Expr = (CExpressionProperty)_properties["Expression"];
				object o = Expr.Evaluate(scope);
				string toPrint = Repr(o);
				Console.WriteLine(toPrint);
				//((InspectorWindow)builtins["Inspector"]).WriteLine(toPrint);
				//((InspectorWindow)scope.GetVariable("_inspector")).WriteLine(toPrint);
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;

			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.OutEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}
	}
	
	// -----------------------------------------------------------------------
    public class CIOAsk : CInputOutput
    {	// Display a message dialog and collect result
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOAsk(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Question = new CExpressionProperty("ask", "'question: '");
			CVarNameProperty Answer = new CVarNameProperty("answer", "result");
			Question.PropertyChanged += OnPropertyChanged;
			Answer.PropertyChanged += OnPropertyChanged;
			_properties["ask"] = Question;
			_properties["answer"] = Answer;
			this.OnPropertyChanged(null, null);
		}
		
		public CIOAsk(Double X, Double Y) : this(X, Y, null) {}
		
		// - - - Get properties - - - - - - - - - - - - - - -
		private String Question 
		{
			get
			{
				return _properties["ask"].Text;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		private String Answer 
		{
			get 
			{
				return _properties["answer"].Text.Trim();
			}
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs E)
		{	// Update text when property changes
			if (this.Answer.Length > 0) {
				this.Text = String.Format("`{0}` = ask({1})", this.Answer, this.Question);
			} else {
				this.Text = String.Format("ask({0})", this.Question);
			}
			RaiseBlockChanged();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Ask = (CExpressionProperty)_properties["ask"];
			try {
				Ask.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}

		// - - -
		private string ToPython ()
		{
			string code = String.Format("Common.Dialogs.ask({0})", this.Question);
			if (this.Answer.Length > 0) {
				code = String.Format("{0} = {1}", this.Answer, code);
			}
			return code;
		}
		
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				
				string code = this.ToPython ();
				o.AppendFormat("{0}{1}\n", sindent, code);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
			
			} catch (Exception ex){
				Console.WriteLine("{0} (in CIOAsk.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner( ScriptScope scope, CallStack stack ) 
		{
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
				yield return rr;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			try {
				CExpressionProperty Ask = (CExpressionProperty)_properties["ask"];
				string question = String.Format ("{0}", Ask.Evaluate(scope));
				object answer = Common.Dialogs.ask (question);
				SetVariable(scope, Answer, answer);
				//Compiler.ExecAssignment (scope, Answer, answer);
				
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.OutEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}
	}
	
	// -----------------------------------------------------------------------
    public class CIOTell : CInputOutput
    {	// Display a message
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOTell(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Tell = new CExpressionProperty("tell", "'message'");
			Tell.PropertyChanged += OnPropertyChanged;
			_properties["tell"] = Tell;
			this.OnPropertyChanged(null, null);
		}
		
		public CIOTell(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs E)
		{	// Update text when property changes
			this.Text = String.Format("tell({0})",  this["tell"]);
			RaiseBlockChanged();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Tell = (CExpressionProperty)_properties["tell"];
			try {
				Tell.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - -
		private string ToPython ()
		{
			string code = String.Format("Common.Dialogs.tell({0})", _properties["tell"].Text);
			return code;
		}
		
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				string code = this.ToPython ();
				o.AppendFormat("{0}{1}\n", sindent, code);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
			
			} catch (Exception ex){
				Console.WriteLine("{0} (in CIOTell.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner( ScriptScope scope, CallStack stack ) 
		{
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
				yield return rr;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			try {
				CExpressionProperty Tell = (CExpressionProperty)_properties["tell"];
				string msg = String.Format ("{0}", Tell.Evaluate(scope));
				Common.Dialogs.tell (msg);
				
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.OutEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}
	}

	// -----------------------------------------------------------------------
    public class CBeep : CBlock
    {	// System beep
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CBeep(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + CBlock.BlockWidth, Y + 20)	}),
				palette ) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.DarkBlue;
			this.FillColor = Diagram.Colors.LightBlue;
			this.Sizable = false;
			
			// Properties
			CStatementProperty Stat = new CStatementProperty("Statement", "Common.Utils.beep()");
			//Stat.PropertyChanged += OnPropertyChanged;
			Stat._Visible = false;
			_properties["Statement"] = Stat;
			this.OnPropertyChanged(null, null);
		}
		public CBeep(Double X, Double Y) : this(X, Y, null) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = "beep()";
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python statement - - - - -
		private string ToPython ()
		{
			string code = String.Format("Common.Utils.beep()");
			return code;
		}
		
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				string code = this.ToPython ();
				o.AppendFormat("{0}{1}\n", sindent, code);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
			
			} catch (Exception ex){
				Console.WriteLine("{0} (in CBeep.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CStatementProperty Stat = (CStatementProperty)_properties["Statement"];
			try {
				Stat.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a variable assignment
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				yield return rr;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Evaluate
			try {
				CStatementProperty Stat = (CStatementProperty)_properties["Statement"];
				Stat.Evaluate(scope);

			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}

			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;

			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.OutEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}
    }

	// -----------------------------------------------------------------------
    public class CBeepFreq : CInputOutput
    {	// System beep with freq
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBeepFreq(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Expr = new CExpressionProperty("Duration", "1");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Duration"] = Expr;
			// Frequency
			Expr = new CExpressionProperty("Frequency", "440");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Frequency"] = Expr;
			this.OnPropertyChanged(null, null);
		}
		public CBeepFreq(Double X, Double Y) : this(X, Y, null) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format ("beep({0},{1})", this["Duration"], this["Frequency"]);
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python statement - - - - -
		private string ToPython ()
		{
		    string code = String.Format("Common.Utils.beep({0}, {1})", 
						this["Duration"], this["Frequency"]);
		    return code;
		}
		
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				string code = this.ToPython ();
				o.AppendFormat("{0}{1}\n", sindent, code);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
			
			} catch (Exception ex){
				Console.WriteLine("{0} (in CBeepFreq.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Expr1 = (CExpressionProperty)_properties["Duration"];
			CExpressionProperty Expr2 = (CExpressionProperty)_properties["Frequency"];
			try {
				Expr1.Compile(engine);
				Expr2.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a variable assignment
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				yield return rr;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Evaluate
			try {
				CExpressionProperty Duration = (CExpressionProperty)_properties["Duration"];
				CExpressionProperty Frequency = (CExpressionProperty)_properties["Frequency"];
				double duration = Double.Parse (Duration.Evaluate(scope).ToString ());
				double frequency = Double.Parse (Frequency.Evaluate(scope).ToString ());
				Common.Utils.beep(duration, frequency);
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}

			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;

			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.OutEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}
    }

	// -----------------------------------------------------------------------
    public class CBeepFreq2 : CInputOutput
    {	// System beep with freq
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBeepFreq2(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Expr = new CExpressionProperty("Duration", "1");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Duration"] = Expr;
			// Frequency1
			Expr = new CExpressionProperty("Frequency1", "440");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Frequency1"] = Expr;
			// Frequency2
			Expr = new CExpressionProperty("Frequency2", "660");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Frequency2"] = Expr;
			this.OnPropertyChanged(null, null);
		}
		public CBeepFreq2(Double X, Double Y) : this(X, Y, null) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format ("beep({0},{1},{2})", 
			                           this["Duration"], this["Frequency1"], this["Frequency2"]);
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python statement - - - - -
		private string ToPython ()
		{
			string code = String.Format("Common.Utils.beep({0}, {1}, {2})", 
			                            this["Duration"], this["Frequency1"], this["Frequency2"]);
			return code;
		}
		
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				string code = this.ToPython ();
				o.AppendFormat("{0}{1}\n", sindent, code);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
			
			} catch (Exception ex){
				Console.WriteLine("{0} (in CBeepFreq2.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Expr1 = (CExpressionProperty)_properties["Duration"];
			CExpressionProperty Expr2 = (CExpressionProperty)_properties["Frequency1"];
			CExpressionProperty Expr3 = (CExpressionProperty)_properties["Frequency2"];
			try {
				Expr1.Compile(engine);
				Expr2.Compile(engine);
				Expr3.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a variable assignment
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				yield return rr;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Evaluate
			try {
				CExpressionProperty Duration = (CExpressionProperty)_properties["Duration"];
				CExpressionProperty Frequency1 = (CExpressionProperty)_properties["Frequency1"];
				CExpressionProperty Frequency2 = (CExpressionProperty)_properties["Frequency2"];
				double duration = Double.Parse (Duration.Evaluate(scope).ToString ());
				double frequency1 = Double.Parse (Frequency1.Evaluate(scope).ToString ());
				double frequency2 = Double.Parse (Frequency2.Evaluate(scope).ToString ());
				Common.Utils.beep(duration, frequency1, frequency2);
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}

			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;

			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.OutEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}
    }

	// -----------------------------------------------------------------------
    public class CSpeak : CInputOutput
    {	// Print block shape class
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CSpeak(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Expr = new CExpressionProperty("Expression", "'hello'");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Expression"] = Expr;
			this.OnPropertyChanged(null, null);
		}
		public CSpeak(Double X, Double Y) : this(X, Y, null) {}
		
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("speak({0})", this["Expression"]);
			RaiseBlockChanged();
		}
		
		// - - - Generate and return Python statement - - - - -
		private string ToPython ()
		{
			string code = String.Format("Common.Utils.speak({0})", this["Expression"]);
			return code;
		}
		
		// - - - Generate and return Python statement - - - - - - - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', Constant.SPACES * indent);
				string code = this.ToPython ();
				o.AppendFormat("{0}{1}\n", sindent, code);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CSpeak.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing speak involves evaluting the given exression
			CExpressionProperty Expr = (CExpressionProperty)_properties["Expression"];
			try {
				Expr.Compile(engine);
			} catch (Exception ex) {
				Console.WriteLine ("Block {0} failed compilation: {1}", this.Name, ex.Message);
				return false;
			}
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public static string Repr(object obj)
		{
			return Repr(obj, 0);
		}

		public static string Repr(object obj, int depth)
		{
			if (depth > 3) // FIXME: cheap trick to avoid recursive data structures
				return "...";
			if (obj == null) {
				return "None";
//			} else if (obj is IronPython.Runtime.List) {
//				return ((IronPython.Runtime.List)obj).__repr__ (
//		  			IronPython.Runtime.DefaultContext.Default);
//			} else if (obj is IronPython.Runtime.PythonTuple) {
//				return obj.ToString();
			} else if (obj is Array) {
				return ArrayToString((object[])obj, depth);
			} else if (obj is IList) {
				return ListToString((IList)obj, depth);
			} else if (obj is IDictionary) {
				return DictionaryToString((IDictionary)obj, depth);
			} else {
				return obj.ToString ();
			}
		}
		
		public static string ArrayToString(object[] args, int depth)
		{
			string retval = "";
			if (args != null) {
				int count = ((Array)args).Length;
				for (int i = 0; i < count; i++) {
					if (retval != "")
						retval += ", ";
					retval += Repr(args[i], depth + 1);
				}
			}
			return String.Format("Array[{0}]", retval);
		}

		public static string ListToString(IList args, int depth)
		{
			string retval = "";
			if (args != null) {
				foreach(object item in args) {
					if (retval != "")
						retval += ", ";
					retval += Repr(item, depth + 1);
				}
			}
			return String.Format("[{0}]", retval);
		}

		public static string DictionaryToString(IDictionary args, int depth)
		{
			string retval = "";
			if (args != null) {
				foreach(object key in args.Keys) {
					if (retval != "")
						retval += ", ";
					retval += String.Format("{0}: {1}", 
								Repr(key, depth + 1), 
								Repr(args[key], depth + 1));
				}
			}
			return String.Format("{{{0}}}", retval);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute speak statement

			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = RunningState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				//rr.Frame = null;
				yield return rr;
			}
			
			// - - - Do the print - - - - - - - - - - - - - - - - - - - -

			try {
				CExpressionProperty Expr = (CExpressionProperty)_properties["Expression"];
				object o = Expr.Evaluate(scope);
				string toPrint = Repr(o);
				Common.Utils.speak(toPrint);
				//((InspectorWindow)builtins["Inspector"]).WriteLine(toPrint);
				//((InspectorWindow)scope.GetVariable("_inspector")).WriteLine(toPrint);
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				this.State = RunningState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

			// Go into a loop while block remains in an error state
			while (this.State == RunningState.Error) yield return rr;

			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Frame = this.OutEdge.LinkedTo.Block.Frame(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Frame = null;
			}
			
			// Indicate that the block is no longer running
			this.State = RunningState.Idle;
			yield return rr;
		}
	}

	// -----------------------------------------------------------------------
    public class CIOWriteToFile : CInputOutput
    {	// Write an item to a file path
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CIOWriteToFile(Double X, Double Y, Widgets.CBlockPalette palette=null) 
			: base(X, Y, palette )
		{
			// Properties
			CExpressionProperty Expr = new CExpressionProperty("Expression", "expr");
			CExpressionProperty Path = new CExpressionProperty("FilePath", "file");
			Expr.PropertyChanged += OnPropertyChanged;
			Path.PropertyChanged += OnPropertyChanged;
			_properties["Expression"] = Expr;
			_properties["FilePath"] = Path;
			this.OnPropertyChanged(null, null);
		}
		
		public CIOWriteToFile(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs E)
		{	// Update text when property changes
			this.Text = String.Format("write {0} to {1}", this["Expression"], this["FilePath"]);
			RaiseBlockChanged();
		}
	}
}
