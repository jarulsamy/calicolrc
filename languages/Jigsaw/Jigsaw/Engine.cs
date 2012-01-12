using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;

using IronPython;
using IronPython.Hosting;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;

// This file contains all code to implement the cooperative multitasking engine that executes Jigsaw programs.

namespace Jigsaw
{
	// -----------------------------------------------------------------------
	public enum EngineAction {
		NoAction = 0, 	// Just keep going.
		Add = 1, 		// Add new block runner to call stack.
		Replace = 2, 	// Replace current block runner with new one.
		Remove = 3, 	// Remove current block runner only.
		Break = 4,		// Breakpoint. Stop timer.
		Return = 5		// Stack is done. Return value and remove entire stack
	}
	
	// -----------------------------------------------------------------------
	public class RunnerResponse
	{	// A tiny struct with the sole purpose of allowing Runners to return multiple items
		public EngineAction Action = EngineAction.NoAction;
		public IEnumerator<RunnerResponse> Runner = null;
		public object RetVal = null;
		
		public RunnerResponse(EngineAction ea, IEnumerator<RunnerResponse> rr) {
			Action = ea;
			Runner = rr;
		}

		public RunnerResponse()
		{}
	}
	
	// -----------------------------------------------------------------------
	public class CallStack
	{	// Call stack manages the stack of block runners (calls) currently executing
		
		private List<IEnumerator<RunnerResponse>> _stack = null;
		
		// This is the globals for this call stack
		public Dictionary<string,object> globals = null;
		
		// This is where returned values are saved by called procedures before being fetched by calling procedures
		public object RetVal = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CallStack(Dictionary<string,object> globals) {
			_stack = new List<IEnumerator<RunnerResponse>>();
			this.globals = globals;	// Keep track of this call stacks globals
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void AppendRunner(CBlock b, ScriptScope scope) 
		{	// Create a block runner given a block and append it to the end of the stack
			_stack.Add( b.Runner(scope, this) );
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public int Count {
			get { return _stack.Count; }	
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public IEnumerator<RunnerResponse> GetTopRunner() {
			if (_stack.Count > 0) return _stack[0];
			return null;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void PopRunner(IEnumerator<RunnerResponse> runner) {
			_stack.Remove(runner);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public void PopProcedure() {
//			// Keep popping runners until procedure top found
//			while( !_stack[0] is C
//			_stack.Remove(runner);
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Clear() {
			_stack.Clear ();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void PushRunner(IEnumerator<RunnerResponse> runner) {
			_stack.Insert(0, runner);
		}
	}
	
	// -----------------------------------------------------------------------
	public class Engine
	{
		private List<CallStack> _callStacks = null;
		//private InspectorWindow _inspector = null;
		private bool _inStep = false;		// true if currently executing a single step
		private string _modulePath = null;
		
		//private Expression.DLRScope scope = null;
		public ScriptRuntimeSetup scriptRuntimeSetup;
		public ScriptRuntime scriptRuntime;
		public LanguageSetup languageSetup;
		public CompilerOptions compiler_options;
		public ScriptEngine engine;
		
		// This dictionary is injected into all ScriptScope objects as the global namespace
		private Dictionary<string,object> globals = null;
		
		// -- This boolean is used by the GLib.TimeoutHandler method in Run
		// and is returned by OnTimerElapsed. When OnTimerElapsed returns true, 
		// GLib.TimeoutHandler reschedules it automatically. When it returns false,
		// It is not rescheduled. When the Jigsaw program is running, _timerContinue is set to true
		// so OnTimerElapsed contineus to be rescheduled and continues to run. 
		// To stop the run, _timerContinue is set to false.
		private bool _timerContinue = false;
		
		// Engine events
		public event EventHandler EngineRun;
		public event EventHandler EngineStep;
		public event EventHandler EngineStop;
		public event EventHandler EnginePause;
		public event EventHandler EngineReset;
		
		public Engine (string modulePath)
		{
			// Set the path to find any DLLs for the engine
			ModulePath = modulePath;
			scriptRuntimeSetup = new ScriptRuntimeSetup();
			languageSetup = Python.CreateLanguageSetup(null);
			
			// Set LanguageSetup options here:
			languageSetup.Options ["FullFrames"] = true; // for debugging
			scriptRuntimeSetup.LanguageSetups.Add(languageSetup); // add to local
			
			// Create engine and global scope:
			scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);
			engine = scriptRuntime.GetEngine("py");
			globals = new Dictionary<string, object>();
			
			// Reload all assemblies
			Assembly assembly = null;
			foreach (string name in new string [] {"Myro", "Shapes", "Graphics"}) {
				String tname = System.IO.Path.Combine (ModulePath, 
				                           			   String.Format ("{0}.dll", name));
				try {
					assembly = Assembly.LoadFrom(tname);
				} catch { //(System.IO.FileNotFoundException) {
//#pragma warning disable 612
//					assembly = Assembly.LoadWithPartialName (tname);
//#pragma warning restore 612
				}
				
				try {
					scriptRuntime.LoadAssembly(assembly);
				} catch {
					Console.WriteLine ("Failed to load assembly " + name);
				}
			}

			// Populate scopes with assemblies
			ScriptSource source;
			ScriptScope scope = engine.CreateScope(globals);
			source = engine.CreateScriptSourceFromString("import Myro", SourceCodeKind.Statements);
			source.Execute(scope);
			source = engine.CreateScriptSourceFromString("import Shapes", SourceCodeKind.Statements);
			source.Execute(scope);
			source = engine.CreateScriptSourceFromString("import Graphics", SourceCodeKind.Statements);
			source.Execute(scope);
			//scope.SetVariable("_inspector", _inspector);
			
			compiler_options = engine.GetCompilerOptions();
			((IronPython.Compiler.PythonCompilerOptions)compiler_options).PrintFunction = true;
			((IronPython.Compiler.PythonCompilerOptions)compiler_options).AllowWithStatement = true;
			((IronPython.Compiler.PythonCompilerOptions)compiler_options).TrueDivision = true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool IsRunning
		{	// Determine if the program is running
			get {
				if (_callStacks == null) return false;
				if (_callStacks.Count > 0) return true;
				return false;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public List<CallStack> CallStacks
		{	// Return the list if CallStacks currently in the engine
			get { return _callStacks; }
		}
		
		public string ModulePath
		{
			// Returns the absolute module path for DLLs
			get { return _modulePath; }
			// Set with relative or absolute; saves absolute
			set { 
				_modulePath = System.IO.Path.GetFullPath(value); 
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool Reset(Jigsaw.Canvas cvs)
		{
			// Save reference to the InspectorWindow
			//_inspector = inspector;
			//_inspector.ClearOutput();
			
			// Recreate top level list of all call stacks. 
			// There can be more than one because Jigsaw allows multiple stacks to run simultaneously.
			if (_callStacks == null) 
				_callStacks = new List<CallStack>();
			else
				_callStacks.Clear();
			
			// Reset all blocks
			foreach (Diagram.CShape s in cvs.shapes) {
				if (s is CBlock) {								// If a shape is a block ...
					CBlock b = (CBlock)s;
					b.State = BlockState.Idle;					// Set block state to Idle
					b["Message"] = "";							// Clear current block message
					
					if (b.IsFactory == false) {					// If block is not a factory block ...
						b.Compile(engine, cvs);					// Ask block to compile itself
						
						if (s is CControlStart) {				// If also a ControlStart block ...
							CControlStart cs = (CControlStart)s;
							CallStack stack = new CallStack(globals);	// Add new runner to call stack
							ScriptScope scope = this.CreateScope(globals);
							stack.AppendRunner( (CControlStart)s, scope );
							_callStacks.Add(stack);
						}
					}
				}
			}
			
			RaiseEngineReset();
			return true;
		}

		// - - - Build and return a new ScriptScope based on a ChainedDictionary - - - - -
		public ScriptScope CreateScope(Dictionary<string,object> globals)
		{
			Dictionary<string,object> locals = new Dictionary<string, object>();
			return this.CreateScope(globals, locals);
		}
		
		// - - - Build and return a new ScriptScope based on two ChainedDictionaries - - - - -
		public ScriptScope CreateScope(Dictionary<string,object> globals, Dictionary<string,object> locals)
		{
			ChainedDictionary chaining  = new ChainedDictionary(locals, globals);
			ScriptScope scope = engine.CreateScope(chaining);
			
			return scope;
		}
		
		// - - Advance the entire system one step - - - - - - - - - - -
		public bool Step()
		{
			// Exit if already running a step to prevent reentrance
			if (_inStep == true) return false;
			_inStep = true;
			
			for (int i=_callStacks.Count-1; i>=0; i--)
			{
				CallStack stack = _callStacks[i];
				
				if (stack.Count > 0) {						// Get the top-most block runner
					IEnumerator<RunnerResponse> br = stack.GetTopRunner();	
					
					bool rslt = br.MoveNext();				// Advance block runner and see if fell off end
					RunnerResponse rr = br.Current;			// Check the response
					
					if (rslt == false) {
						stack.PopRunner(br);				// Enumerator has passed the end of the collection
					} else {
						switch (rr.Action) {				// Take appropriate action
						case EngineAction.Break:
							this.Stop();
							break;
						case EngineAction.Remove:
							stack.PopRunner(br);
							stack.RetVal = rr.RetVal;		// Copy returned value to temp location in stack
							break;
//						case EngineAction.Return:
//							stack.PopProcedure();
//							stack.RetVal = rr.RetVal;		// Copy returned value to temp location in stack
//							break;
						case EngineAction.Replace:
							stack.PopRunner(br);
							stack.PushRunner(rr.Runner);
							rr.Runner.MoveNext();
							break;
						case EngineAction.Add:
							stack.PushRunner(rr.Runner);
							rr.Runner.MoveNext();
							break;
						case EngineAction.NoAction:
							break;
						}
					}
					
//					// If there is a breakpoint, stop timer
//					if (rr.Action == EngineAction.Break)
//					{
//						this.Stop();
//					}
//					
//					// Conditions for removing the block runner from the stack
//					if (rr.Action == EngineAction.Remove ||
//					    rr.Action == EngineAction.Replace ||
//					    rslt == false) {
//						stack.RemoveRunner(br);
//					}
					
//					// Conditions for adding a new block runner
//					if (rr.Action == EngineAction.Replace ||
//			            rr.Action == EngineAction.Add ||
//			            rr.Runner != null) {
//						stack.PushRunner(rr.Runner);
//						rr.Runner.MoveNext();						// Take first step right away
//					}

				} else {
					// If call stack is empty, the block runner stack has completed execution.
					// Remove stack from list of all stacks.
					_callStacks.RemoveAt(i);
				}
			}
			
			_inStep = false;
			RaiseEngineStep();
			
			// Check for nothing more to run
			if (_callStacks.Count == 0) this.Stop();
			
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Run()
		{
			_timerContinue = true;
			
			// Dealing with cross-thread issues...
			GLib.Timeout.Add(50, new GLib.TimeoutHandler(OnTimerElapsed));
			RaiseEngineRun();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Stop()
		{
			_timerContinue = false;
			RaiseEngineStop();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Pause()
		{
			_timerContinue = false;
			RaiseEnginePause();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		private bool OnTimerElapsed()
		{
			// Execute one step
			this.Step();
			
			// If the step advanced the engine to a completion point, stop
			if (!this.IsRunning) this.Stop();
			
			// Return the current state of the desire to continue
			return _timerContinue;
		}
		
		// - - Raise the EngineRun event  - - - - - - - - - - - - - - 
        public void RaiseEngineRun()
        {
            if (EngineRun != null)
            {
				EventArgs e = new EventArgs();
            	EngineRun(this, e);
            }
        }
		
		// - - Raise the EngineStep event - - - - - - - - - - - - - -
        public void RaiseEngineStep()
        {
            if (EngineStep != null)
            {
				EventArgs e = new EventArgs();
            	EngineStep(this, e);
            }
        }
		
		// - - Raise the EngineStop event - - - - - - - - - - - - -
        public void RaiseEngineStop()
        {
            if (EngineStop != null)
            {
				EventArgs e = new EventArgs();
            	EngineStop(this, e);
            }
        }

		// - - Raise the EnginePause event - - - - - - - - - - - - -
        public void RaiseEnginePause()
        {
            if (EnginePause != null)
            {
				EventArgs e = new EventArgs();
            	EnginePause(this, e);
            }
        }
		
		// - - Raise the EngineReset event - - - - - - - - - - - - - - -
        public void RaiseEngineReset()
        {
            if (EngineReset != null)
            {
				EventArgs e = new EventArgs();
            	EngineReset(this, e);
            }
        }

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void SetModulePath (string module_path)
		{
			throw new NotImplementedException ();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	}
	
	// --- A Dictionary<string,object> made by chaining two others -----------------
	public class ChainedDictionary : IDictionary<string,object>
	{
		public Dictionary<string,object> locals = null;
		public Dictionary<string,object> globals = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public ChainedDictionary(Dictionary<string,object> locals, Dictionary<string,object> globals) 
		{
			this.locals = locals;
			this.globals = globals;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	    #region ICollection Members
	    public bool IsSynchronized { get { return false; } }
	    
		public object SyncRoot { 
			get { 
				throw new NotImplementedException(); 
			} 
		}
	    
		public int Count { 
			get {
				return (locals.Count + globals.Count); 
			} 
		}
	    
		// - - Copies the elements of the IDictionary<string,object> to an Array - - - 
		// - - starting at a particular Array index.
		public void CopyTo(KeyValuePair<string,object>[] pairs, int index)
		{
			throw new NotImplementedException();
		}
	    #endregion
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		#region IDictionary Members
	    public bool IsReadOnly { 
			get { return false; } 
		}

		public bool Contains(KeyValuePair<string,object> pair)
	    {
			// True if referenced object is in either locals or globals
			if ( locals.ContainsKey(pair.Key) )
			{
				return locals[pair.Key] == pair.Value;
			}
			else if ( globals.ContainsKey( pair.Key ) )
			{
				return globals[pair.Key] == pair.Value;
			} else {
				return false;
			}
	    }
		
	    public bool ContainsKey(string key)
	    {	
			// True if key is in either locals or globals
	       	return locals.ContainsKey(key) || globals.ContainsKey(key);
	    }
	    
		public bool IsFixedSize
		{	// Not fixed 
			get { return false; }
		}

		public bool Remove(KeyValuePair<string,object> pair)
	    {	// If key in locals, remove that. Otherwise try to remove from globals.
			if (locals.ContainsKey(pair.Key)) {
				if (locals[pair.Key] == pair.Value) {
					return locals.Remove(pair.Key);
				}
			}
			if (globals.ContainsKey(pair.Key)) {
				if (globals[pair.Key] == pair.Value) {
					return globals.Remove(pair.Key);
				}
			}
			return false;
	    }
		
		public bool Remove(string key)
	    {	
			// If key in locals, remove that. Otherwise try to remove from globals.
			if (locals.ContainsKey(key) == true) {
			 	return locals.Remove(key);
			} else {
				return globals.Remove(key);
			}
	    }
	    
		public void Clear()
		{	// Clear both dictionaries
			locals.Clear();
			globals.Clear();
		}
		
	    public void Add(KeyValuePair<string,object> pair) 
	    {	// If adding, add to locals. Globals is read-only for now
	        locals.Add(pair.Key, pair.Value);
	    }

		public void Add(string key, object value) 
	    {	// If adding, add to locals. Globals is read-only for now
	        locals.Add (key, value);
	    }
		
	    public ICollection<string> Keys
	    {	// Return both sets of keys in one array
	        get
	        {   // Return an array where each item is a key.
				string[] keys = new string[locals.Count + globals.Count];
	            locals.Keys.CopyTo(keys, 0);
				globals.Keys.CopyTo(keys, locals.Count);
	            return keys;
	        }
	    }
	    
		public ICollection<object> Values
	    {	// Return both sets of objects in one array
	        get
	        {	// Return an array where each item is a value.
	            object[] values = new object[locals.Count + globals.Count];
				int index = 0;
				foreach (object v in locals.Values)
				{
					values[index] = v;
					index++;
				}
				foreach (object v in globals.Values)
				{
					values[index] = v;
					index++;
				}
	            return values;
	        }
	    }
		
	    public object this[string key]
	    {
	        get
	        {   
				//Console.Write ("getting this[{0}] ... ", key);
	            if (locals.ContainsKey(key))
				{
					//Console.WriteLine ("got from locals");
					return locals[key];
				} else if (globals.ContainsKey(key)) {
					//Console.WriteLine ("got from globals");
					return globals[key];
				} else {
					//Console.WriteLine ("not found");
					return null;
				}
	        }
	
	        set
	        {	
				if ( locals.ContainsKey(key) ) {
					//Console.WriteLine ("set in locals this[{0}] = {1}", key, value);
					locals[key] = value;
				} else if ( globals.ContainsKey(key) ) {
					//Console.WriteLine ("set in globals this[{0}] = {1}", key, value);
					globals[key] = value;
				} else {
					//Console.WriteLine ("added to locals this[{0}] = {1}", key, value);
					locals[key] = value;
				}					
	        }
	    }
		
	    public bool TryGetValue(string key, out object value)
	    {	//Console.Write ("TryGetValue({0}) ... ", key);
			
			if ( locals.ContainsKey(key) ) {
				//Console.WriteLine ("got from locals");
				value = locals[key];
				return true;
			} else if (globals.ContainsKey(key)) {
				//Console.WriteLine ("got from globals");
				value = globals[key];
				return true;
			} else {
				//Console.WriteLine ("not found");
				value = null;
				return false;
			}
	    }
		
	    private class ChainedDictionaryEnumerator : IDictionaryEnumerator
	    {
			private IDictionaryEnumerator lenum = null;
			private IDictionaryEnumerator genum = null;
			private bool secondEnum = false;
	
	        public ChainedDictionaryEnumerator(ChainedDictionary cd)
	        {
				lenum = cd.locals.GetEnumerator();
				genum = cd.globals.GetEnumerator();
				secondEnum = false;
	        }
	
	        // Return the current item.
	        public object Current {
				get {
					if (secondEnum == false) {
						return lenum.Current;
					} else {
						return genum.Current;
					}
				}
			}
	
	        // Return the current dictionary entry.
	        public DictionaryEntry Entry
	        {
	            get {
					return (DictionaryEntry) Current;
				}
	        }
	
	        // Return the key of the current item.
	        public Object Key { 
				get {
					if (secondEnum == false) {
						return lenum.Key; 
					} else {
						return genum.Key;
					}
				}
			}
	
	        // Return the value of the current item.
	        public object Value { 
				get {
					if (secondEnum == false) {
						return lenum.Value; 
					} else {
						return genum.Value;
					}
				}
			}
	
	        // Advance to the next item.
	        public bool MoveNext()
	        {
				if (lenum.MoveNext() == true)
				{
					return true;
				} else {
					secondEnum = true;
					return genum.MoveNext ();
				}
	        }
	
	        // Reset the enumeration.
	        public void Reset()
	        {
				lenum.Reset ();
				genum.Reset ();
	            secondEnum = false;
	        }
	    }
	    public IDictionaryEnumerator GetEnumerator()
	    {
	        // Construct and return an enumerator.
	        return new ChainedDictionaryEnumerator(this);
	    }
	    #endregion

		// - - Construct and return an enumerator - - - - - - - - - -
	    #region IEnumerable Members
	    IEnumerator IEnumerable.GetEnumerator() 
	    {
	        return ((IDictionary)this).GetEnumerator();
	    }
		
		IEnumerator<KeyValuePair<string,object>> IEnumerable<KeyValuePair<string,object>>.GetEnumerator()
		{
			return (IEnumerator<KeyValuePair<string,object>>)this.GetEnumerator();
		}

	    #endregion
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	}
}