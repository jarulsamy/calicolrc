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
				new Diagram.CPoint(X + 175, Y + 20)}),
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
			CExpressionProperty Expr = new CExpressionProperty("Expression", "X");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Expression"] = Expr;
			this.OnPropertyChanged(null, null);
		}
		public CIOPrint(Double X, Double Y) : this(X, Y, null) {}
		
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("print({0})", this["Expression"]);
		}
		
		// - - - Generate and return Python statement - - - - - - - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
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
		{	// Execute print statement

			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = BlockState.Running;				// Indicate that the block is running
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
				this.State = BlockState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;

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
			this.State = BlockState.Idle;
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
			CExpressionProperty Question = new CExpressionProperty("Ask", "'Question: '");
			CVarNameProperty Answer = new CVarNameProperty("Answer", "X");
			Question.PropertyChanged += OnPropertyChanged;
			Answer.PropertyChanged += OnPropertyChanged;
			_properties["Ask"] = Question;
			_properties["Answer"] = Answer;
			this.OnPropertyChanged(null, null);
		}
		
		public CIOAsk(Double X, Double Y) : this(X, Y, null) {}
		
		// - - - Get properties - - - - - - - - - - - - - - -
		private String Question 
		{
			get
			{
				return _properties["Ask"].Text;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		private String Answer 
		{
			get 
			{
				return _properties["Answer"].Text.Trim();
			}
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs E)
		{	// Update text when property changes
			if (this.Answer.Length > 0) {
				this.Text = String.Format("{0} = Ask: {1}", this.Answer, this.Question);
			} else {
				this.Text = String.Format("Ask: {0}", this.Question);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Ask = (CExpressionProperty)_properties["Ask"];
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
				string sindent = new string (' ', 2*indent);
				
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
			this.State = BlockState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				rr.Frame = null;
				yield return rr;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			try {
				CExpressionProperty Ask = (CExpressionProperty)_properties["Ask"];
				string question = String.Format ("{0}", Ask.Evaluate(scope));
				object answer = Common.Dialogs.ask (question);
				scope.SetVariable(Answer, answer);
				
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = BlockState.Error;
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
			this.State = BlockState.Idle;
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
			CExpressionProperty Tell = new CExpressionProperty("Tell", "'message'");
			Tell.PropertyChanged += OnPropertyChanged;
			_properties["Tell"] = Tell;
			this.OnPropertyChanged(null, null);
		}
		
		public CIOTell(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs E)
		{	// Update text when property changes
			this.Text = String.Format("Tell: {0}",  this["Tell"]);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile(Microsoft.Scripting.Hosting.ScriptEngine engine, Jigsaw.Canvas cvs)
		{
			// Executing a print involves evaluting the given exression
			CExpressionProperty Tell = (CExpressionProperty)_properties["Tell"];
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
			string code = String.Format("Common.Dialogs.tell({0})", _properties["Tell"].Text);
			return code;
		}
		
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				
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
			this.State = BlockState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Pause;				// so that engine can stop
				rr.Frame = null;
				yield return rr;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			try {
				CExpressionProperty Tell = (CExpressionProperty)_properties["Tell"];
				string msg = String.Format ("{0}", Tell.Evaluate(scope));
				Common.Dialogs.tell (msg);
				
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = BlockState.Error;
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
			this.State = BlockState.Idle;
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
				new Diagram.CPoint(X + 175, Y + 20)	}),
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
				string sindent = new string (' ', 2*indent);
				
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
			this.State = BlockState.Running;				// Indicate that the block is running
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
				
				this.State = BlockState.Error;
				rr.Action = EngineAction.Error;
				rr.Frame = null;
			}

			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;

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
			this.State = BlockState.Idle;
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
		}
	}
}
