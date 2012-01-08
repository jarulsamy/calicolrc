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
    public class CProcedureStart : CBlock
    {	// Procedure start block shape class
		
		// To hold ordered list of arguments passed to procedure when called
		public List<object> Args = null;
		
		// Private list of argument names
		private List<string> _argnames = new List<string>();
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CProcedureStart(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 30) }),
				palette) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.Purple;
			this.FillColor = Diagram.Colors.Thistle;
			this.Sizable = false;
			this.Text = "define ...";
			
			_textYOffset = 10;							// Block text offset
			
			// Properties
			CVarNameProperty ProcName = new CVarNameProperty("Procedure Name", "MyProc");
			CVarNameProperty Arg1 = new CVarNameProperty("Arg1", "");
			CVarNameProperty Arg2 = new CVarNameProperty("Arg2", "");
			CVarNameProperty Arg3 = new CVarNameProperty("Arg3", "");
			
			ProcName.PropertyChanged += OnPropertyChanged;
			Arg1.PropertyChanged += OnPropertyChanged;
			Arg2.PropertyChanged += OnPropertyChanged;
			Arg3.PropertyChanged += OnPropertyChanged;
			
			_properties["Procedure Name"] = ProcName;
			_properties["Arg1"] = Arg1;
			_properties["Arg2"] = Arg2;
			_properties["Arg3"] = Arg3;
			
			_argnames.Add ("Arg1");
			_argnames.Add ("Arg2");
			_argnames.Add ("Arg3");
			
			this.OnPropertyChanged(null, null);
		}
		
		public CProcedureStart(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - Private util to build delimited arg list string - - - - - -
		private string argListString
		{
			get {
				List<String> arglist = new List<String>();
				foreach (string aname in _argnames) {
					if (_properties.ContainsKey(aname) && _properties[aname].Text.Length > 0) 
						arglist.Add (_properties[aname].Text);
				}
				return String.Join (", ", arglist);
			}
		}

		// - - - Get procedure name - - - - - - - - - - - - - - -
		public String ProcedureName 
		{
			get 
			{
				return _properties["Procedure Name"].Text;
			}
		}
		
		// - - - Update label when property changes - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			// Update block text
			this.Text = String.Format("define {0} ({1})", this.ProcedureName, argListString);
		}

		// - - - Generate and return Python translation of a procedure - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				o.AppendFormat("{0}def {1}({2}):\n", sindent, this.ProcedureName, this.argListString);
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent+1);
				} else {
					string sindent2 = new string (' ', 2*(indent+1));
					o.AppendFormat ("{0}pass\n", sindent2);
				}
				
			} catch (Exception ex){
				Console.WriteLine(ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - Execute a procedure - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = BlockState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Break;				// so that engine can stop
				rr.Runner = null;
				yield return rr;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			// Create a new ScriptScope using passed variables
			try
			{
				// Assemble dictionary of parameters passed to block as initial local scope
				List<String> arglist = new List<String>();
				foreach (string aname in _argnames) {
					if (_properties.ContainsKey(aname) && _properties[aname].Text.Length > 0) 
						arglist.Add (_properties[aname].Text);
				}
				Dictionary<string, object> locals = new Dictionary<string, object>();
				for (int i=0; i<arglist.Count; i++) locals[arglist[i]] = Args[i];
				
				ChainedDictionary chaining  = new ChainedDictionary(locals, stack.globals);
				scope = scope.Engine.CreateScope(chaining);
			} catch (Exception ex) {
				this["Message"] = ex.Message;
				this.State = BlockState.Error;
				rr.Action = EngineAction.NoAction;
				rr.Runner = null;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;

			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Runner = this.OutEdge.LinkedTo.Block.Runner(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Runner = null;
			}
			
			// Clean up
			this.Args = null;
			
			// Indicate that the block is no longer running
			this.State = BlockState.Idle;
			yield return rr;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			// Procedure start blocks only have an output edge
			get {
				return new List<CEdge>() { this.OutEdge };
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
			
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+10);
			g.Arc(    x+50, y+95, 100, -0.665*Math.PI, -0.324*Math.PI);
			g.LineTo( x+w-r, y+10);
			g.Arc(    x+w-r, y+10+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+27, y+h );
			g.LineTo( x+24, y+h+4 );
			g.LineTo( x+14, y+h+4 );
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+10 );
            g.ClosePath();
		}
    }
	
	// -----------------------------------------------------------------------
    public class CProcedureCall : CBlock
    {	// Procedure call block shape class
		
		// This property gets set to the procedure start block to be called
		private CProcedureStart procStartBlock = null;
		
		// Private list of argument names
		private List<string> _argnames = new List<string>();

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CProcedureCall(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y),
				new Diagram.CPoint(X + 175, Y + 20)	}),
				palette ) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.Purple;
			this.FillColor = Diagram.Colors.Thistle;;
			this.Sizable = false;
			
			this.Text = "call ...";
			
			// Properties
			CVarNameProperty VarName = new CVarNameProperty("Variable", "X");
			CVarNameProperty ProcName = new CVarNameProperty("Procedure Name", "MyProc");
			CExpressionProperty Arg1 = new CExpressionProperty("Arg1", "");
			CExpressionProperty Arg2 = new CExpressionProperty("Arg2", "");
			CExpressionProperty Arg3 = new CExpressionProperty("Arg3", "");
			
			VarName.PropertyChanged += OnPropertyChanged;
			ProcName.PropertyChanged += OnPropertyChanged;
			Arg1.PropertyChanged += OnPropertyChanged;
			Arg2.PropertyChanged += OnPropertyChanged;
			Arg3.PropertyChanged += OnPropertyChanged;
			
			_properties["Variable"] = VarName;
			_properties["Procedure Name"] = ProcName;
			_properties["Arg1"] = Arg1;
			_properties["Arg2"] = Arg2;
			_properties["Arg3"] = Arg3;
			
			_argnames.Add ("Arg1");
			_argnames.Add ("Arg2");
			_argnames.Add ("Arg3");
			
			this.OnPropertyChanged(null, null);
		}
		
		public CProcedureCall(Double X, Double Y) : this(X, Y, null) {}
		
        // - - - Private util to build delimited arg list string - - - - - -
		private string argListString
		{
			get {
				List<String> arglist = new List<String>();
				foreach (string aname in _argnames) {
					if (_properties.ContainsKey(aname) && _properties[aname].Text.Length > 0) 
						arglist.Add (_properties[aname].Text);
				}
				return String.Join (", ", arglist);
			}
		}
		
		// - - - Get procedure name - - - - - - - - - - - - - - -
		private String ProcedureName 
		{
			get 
			{
				return _properties["Procedure Name"].Text.Trim();
			}
		}

		// - - - Get return variable name - - - - - - - - - - - - - - -
		private String VariableName 
		{
			get 
			{
				return _properties["Variable"].Text.Trim();
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override bool Compile( ScriptEngine engine, Jigsaw.Canvas cvs )
		{	// Compile the block code into something that can be executed.
			
			// Look for the procedure block being called and save a reference
			procStartBlock = null;
			string pname = this.ProcedureName;
			foreach (CBlock b in cvs.AllBlocks ())
			{
				if (b is CProcedureStart)
				{
					CProcedureStart tblock = (CProcedureStart)b;
					
					if (pname == tblock.ProcedureName) 
					{	// Found it. Save and break.
						procStartBlock = tblock;
						break;
					}
				}
			}
			
			// Check for compile error. Can't find block.
			if (procStartBlock == null) {
				string errmsg = "Compile error. Cannot find procedure named " + pname;
				_properties["Message"].Text = errmsg;
				Console.WriteLine(errmsg);
				return false;
			}
			
			// Compile all expressions passed to procedure
			foreach (string aname in _argnames) {
				CExpressionProperty Arg1 = (CExpressionProperty)_properties[aname];
				if (Arg1.Text.Length > 0) Arg1.Compile(engine);
			}
			
			return true;
		}
		
        // - - - Update text when property changes - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{
			if (this.VariableName.Length > 0) {
				this.Text = String.Format("{0} = {1}({2})", this.VariableName, this.ProcedureName, argListString);
			} else {
				this.Text = String.Format("{0}({1})", this.ProcedureName, argListString);
			}
		}
		
		// - - - Generate and return Python procedure call - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				if (this.VariableName.Length > 0) {
					o.AppendFormat("{0}{1} = {2}({3})\n", sindent, this.VariableName, this.ProcedureName, this.argListString);
				} else {
					o.AppendFormat("{0}{1}({2})\n", sindent, this.ProcedureName, this.argListString);
				}
				
				if (this.OutEdge.IsConnected) {
					CBlock b = this.OutEdge.LinkedTo.Block;
					b.ToPython(o, indent);
				}
				
			} catch (Exception ex){
				Console.WriteLine("{0} (in CProcedureCall.ToPython)", ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Execute a procedure call
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = BlockState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Break;				// so that engine can stop
				rr.Runner = null;
				yield return rr;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			try 
			{	
				// Set Args to be passed to start block
				List<object> args = new List<object>();
				
				foreach (string aname in _argnames) {
					CExpressionProperty Arg1 = (CExpressionProperty)_properties[aname];
					if (Arg1.Text.Length > 0) args.Add( Arg1.Evaluate(scope) );
				}
				procStartBlock.Args = args;
				
				// Create a new Runner from called procedure start block and push on to this call stack
				rr.Action = EngineAction.Add;
				rr.Runner = procStartBlock.Runner(scope, stack);
				
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = BlockState.Error;
				rr.Action = EngineAction.NoAction;
				rr.Runner = null;
			}
			
			yield return rr;
			
			try
			{	// When continue, grab the returned value and assign to variable in this scope, if specified
				CVarNameProperty VarName = (CVarNameProperty)_properties["Variable"];
				scope.SetVariable(VarName.Text, stack.RetVal);	// Why does this not throw an exception when VarName is a zero-length string?
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = BlockState.Error;
				rr.Action = EngineAction.NoAction;
				rr.Runner = null;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;

			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Runner = this.OutEdge.LinkedTo.Block.Runner(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Runner = null;
			}
			
			// Indicate that the block is no longer running
			this.State = BlockState.Idle;
			yield return rr;
		}
    }
	
	// -----------------------------------------------------------------------
    public class CProcedureReturn : CBlock
    {	// Procedure return block shape class
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public CProcedureReturn(Double X, Double Y, Widgets.CBlockPalette palette = null) 
			: base(new List<Diagram.CPoint>(new Diagram.CPoint[] { 
				new Diagram.CPoint(X, Y), 
				new Diagram.CPoint(X + 175, Y + 20)}),
				palette) 
		{
			this.LineWidth = 2;
			this.LineColor = Diagram.Colors.Purple;
			this.FillColor = Diagram.Colors.Thistle;
			this.Sizable = false;
			this.Text = "return";
			
			// Properties - Variable to return
			CExpressionProperty Expr = new CExpressionProperty("Expression", "0");
			Expr.PropertyChanged += OnPropertyChanged;
			_properties["Expression"] = Expr;
			this.OnPropertyChanged(null, null);
		}
		
		public CProcedureReturn(Double X, Double Y) : this(X, Y, null) {}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override List<CEdge> Edges 
		{	// Return a list of all edges
			// Control end blocks only have an input edge
			get {
				return new List<CEdge>() { this.InEdge };
			}
		}
		
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void OnPropertyChanged(object sender, EventArgs e)
		{	// Update text when property changes
			this.Text = String.Format("return {0}", this["Expression"]);
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
		public override IEnumerator<RunnerResponse> Runner(ScriptScope scope, CallStack stack) 
		{	// Return a value from a procedure call
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Always place this block of code at the top of all block runners
			this.State = BlockState.Running;				// Indicate that the block is running
			RunnerResponse rr = new RunnerResponse();		// Create and return initial response object
			yield return rr;
			
			if (this.BreakPoint == true) {					// Indicate if breakpoint is set on this block
				rr.Action = EngineAction.Break;				// so that engine can stop
				rr.Runner = null;
				yield return rr;
			}
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			
			try
			{	// Remove this Runner from call stack and return value
				CExpressionProperty Expr = (CExpressionProperty)_properties["Expression"];
				rr.RetVal = Expr.Evaluate(scope);
				
			} catch (Exception ex) {
				Console.WriteLine(ex.Message);
				this["Message"] = ex.Message;
				
				this.State = BlockState.Error;
				rr.Action = EngineAction.NoAction;
				rr.Runner = null;
			}
			
			// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
			// Go into a loop while block remains in an error state
			while (this.State == BlockState.Error) yield return rr;

			// If connected, replace this runner with the next runner to the stack.
			if (this.OutEdge.IsConnected) {
				rr.Action = EngineAction.Replace;
				rr.Runner = this.OutEdge.LinkedTo.Block.Runner(scope, stack);
			} else {
				// If not connected, just remove this runner
				rr.Action = EngineAction.Remove;
				rr.Runner = null;
			}
			
			// Indicate that the block is no longer running
			this.State = BlockState.Idle;
			yield return rr;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		protected override void SetPath(Cairo.Context g) 
		{
			double x = this.left;
            double y = this.top;
			
            double w = this.width;
            double h = this.height;
			double r = 6.0;
			double hpi = 0.5*Math.PI;
			
			g.MoveTo( x, y+r );
			g.Arc(    x+r, y+r, r, Math.PI, -hpi );
			g.LineTo( x+11, y );
			g.LineTo( x+14, y+4 );
			g.LineTo( x+24, y+4 );
			g.LineTo( x+27, y );
			g.LineTo( x+w-r, y );
			g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
			g.LineTo( x+w, y+h-r );
			g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
			g.LineTo( x+11, y+h );
			g.LineTo( x+r, y+h );
			g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
			g.LineTo( x, y+r );
            g.ClosePath();
		}

		// - - - Generate and return Python translation of a procedure return - - - - -
		public override bool ToPython (StringBuilder o, int indent)
		{
			try
			{
				string sindent = new string (' ', 2*indent);
				o.AppendFormat("{0}return {1}\n", sindent, this["Expression"]);
				
			} catch (Exception ex){
				Console.WriteLine(ex.Message);
				return false;
			}
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    }

}
