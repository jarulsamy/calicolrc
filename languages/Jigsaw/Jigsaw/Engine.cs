//
//  Engine.cs
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
using System.Text;
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
	public class MessageBox
	{
		public static void Show(string Msg)
		{
			Gtk.MessageDialog md = new Gtk.MessageDialog (null, Gtk.DialogFlags.Modal, Gtk.MessageType.Info, Gtk.ButtonsType.Ok, Msg);
			md.Run ();
			md.Destroy();
		}
	}

	// -----------------------------------------------------------------------
	public enum EngineAction {
		NoAction = 0, 	// Just keep going.
		Add = 1, 		// Add new block runner to call stack.
		Replace = 2, 	// Replace current block runner with new one.
		Remove = 3, 	// Remove current block runner only.
		Pause = 4,		// Breakpoint. Pause execution
		Break = 5,		// Break from a loop or conditional
		Return = 6,		// Stack is done. Return value and remove entire stack
		Error = 7,		// A runtime error occurred
		Stop = 10		// Stop engine execution
	}
	
	// -----------------------------------------------------------------------
	public class RunnerResponse
	{	// A tiny struct with the sole purpose of allowing Runners to return multiple items
		public EngineAction Action = EngineAction.NoAction;
		public StackFrame Frame = null;
		public object RetVal = null;
		
		public RunnerResponse(EngineAction ea, StackFrame frame) {
			Action = ea;
			Frame = frame;
		}

		public RunnerResponse()
		{}
	}

	// -----------------------------------------------------------------------
	public enum EngineState {
		Stopped = 0,
		Paused = 1,
		Running = 2
	}

	// -----------------------------------------------------------------------
	public class StackFrame
	{
		// Block whose runner is managed
		internal CBlock _block = null;
		
		// Internal runner encapsulated by the stack frame
		internal IEnumerator<RunnerResponse> _runner = null;

		// script scope used by the runner
		internal ScriptScope _scope = null;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public StackFrame(CBlock blk, IEnumerator<RunnerResponse> runner, ScriptScope scope) {
			_block = blk;
			_runner = runner;
			_scope = scope;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public IEnumerator<RunnerResponse> Runner
		{
			get	{ return _runner; }
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public ScriptScope Scope
		{
			get	{ return _scope; }
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public RunnerResponse Current
		{
			get { return _runner.Current; }
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool MoveNext()
		{
			return _runner.MoveNext ();
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBlock GetBlock()
		{
			return _block;
		}
	}
	
	// -----------------------------------------------------------------------
	public class CallStack
	{	// Call stack manages the stack of block runners (calls) currently executing
		
		private List<StackFrame> _stack = null;
		
		// This is the globals for this call stack
		public Dictionary<string,object> globals = null;
		
		// This is where returned values are saved by called procedures before being fetched by calling procedures
		public object RetVal = null;
		
		// lastSelectedPage, this call stack is enabled. If false, call stack is paused.
		public bool Enabled = false;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CallStack(Dictionary<string,object> globals) {
			_stack = new List<StackFrame>();
			this.globals = globals;	// Keep track of this call stacks globals
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Dump() {
			foreach (StackFrame f in _stack) Console.WriteLine (f._block.Text);
			Console.WriteLine ("---");
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void AppendFrame(CBlock b, ScriptScope scope) 
		{	// Create a block runner given a block
			// createa a StackFrame and append it to the end of the stack
			IEnumerator<RunnerResponse> runner = b.Runner(scope, this);
			_stack.Add( new StackFrame(b, runner, scope ));
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public int Count {
			get { return _stack.Count; }	
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public StackFrame GetTopFrame() {
			if (_stack.Count > 0) return _stack[0];
			return null;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void PopFrame(StackFrame frame) {
			_stack.Remove(frame);
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void PopProcedure() {
			// Keep popping runners until entire procedure is removed, or call stack is empty
			while ( _stack.Count > 0 && !(_stack[0]._block is CProcedureCall)) {
				_stack[0]._block.State = RunningState.Idle;
				_stack.RemoveAt(0);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CBlock PopBreak() {
			// Pop inner-most loop or conditional to respond to break statement
			while ( _stack.Count > 0 && _stack[0]._block._breakStop == false) {
				_stack[0]._block.State = RunningState.Idle;
				_stack.RemoveAt(0);
			}
			
			// Return next if outedge is connected
			CBlock b = _stack[0]._block;
			
			// If connected, return the next frame
			CBlock nextBlock = null;
			if (b.OutEdge.IsConnected) {
				nextBlock = b.OutEdge.LinkedTo.Block;
			}

			_stack[0]._block.State = RunningState.Idle;
			_stack.RemoveAt(0);
			
			return nextBlock;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Clear() {
			_stack.Clear ();
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void PushFrame(StackFrame frame) {
			_stack.Insert(0, frame);
		}

	        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	        public Dictionary<string,object> GetGlobals ()
	        {
		        return globals;
		}
	}
	
	// -----------------------------------------------------------------------
	public class Engine
	{
		private List<CallStack> _callStacks = null;
		//private InspectorWindow _inspector = null;
		private bool _inStep = false;		// true if currently executing a single step
		private bool _inTimeout = false;	// true of in the timeout handler
		private bool _turbo = false;		// true if in turbo mode
		
		public ScriptRuntimeSetup scriptRuntimeSetup;
		public ScriptRuntime scriptRuntime;
		public LanguageSetup languageSetup;
		//internal CompilerOptions compiler_options;
		public ScriptEngine engine;
		
		// This dictionary is injected into all ScriptScope objects as the global namespace
		private Dictionary<string,object> globals = null;
		
		// List of all paths to DLLs that are loaded. Paths are stored in keys to be unique
		internal Dictionary<string, bool> loadedAssemblies = new Dictionary<string, bool>();
		
		// The following private variables manage the timer.
		//private bool _timerRunning = false;		// true if a program is running, not the timer
		//private bool _timerContinue = false;	// false to stop timer
		private uint _timerID = 0;				// ID of currently installed timeout handler
		//private bool _timerReset = false;		// true to stop and start timer with new timeout
		private uint _timeOut = 100;			// timer timeout value
		//private bool _isRunning = false;		// true if a program is running, not the timer
		private EngineState _state = EngineState.Stopped;
		
		// Engine events
		public event EventHandler EngineRun;
		public event EventHandler EngineStep;
		public event EventHandler EngineStop;
		public event EventHandler EnginePause;
		public event EventHandler EngineReset;
		public event EventHandler EngineError;
		
		public Engine ()
		{
			scriptRuntimeSetup = new ScriptRuntimeSetup();
			languageSetup = Python.CreateLanguageSetup(null);
			
			// Set LanguageSetup options here:
			languageSetup.Options ["FullFrames"] = true; // for debugging
			scriptRuntimeSetup.LanguageSetups.Add(languageSetup); // add to local
			
			// Create engine and global scope:
			scriptRuntime = new Microsoft.Scripting.Hosting.ScriptRuntime(scriptRuntimeSetup);

			engine = scriptRuntime.GetEngine("py");
//			var compiler_options = new Dictionary<string, object>();
//			compiler_options["DivisionOptions"] = PythonDivisionOptions.New;
//			engine = Python.CreateEngine (compiler_options);

			globals = new Dictionary<string, object>();
			
//			Microsoft.Scripting.CompilerOptions compiler_options = engine.GetCompilerOptions();
//			((IronPython.Compiler.PythonCompilerOptions)compiler_options).PrintFunction = true;
//			((IronPython.Compiler.PythonCompilerOptions)compiler_options).AllowWithStatement = true;
//			((IronPython.Compiler.PythonCompilerOptions)compiler_options).TrueDivision = true;

			//%%%
			//GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(OnTimerElapsed));
		}
		
		// - - - Load assembly into global scope - - - - - - - - - - - -
		public Assembly LoadAssembly(string dllPath)
		{
			// Get root name
			string assemblyName = System.IO.Path.GetFileNameWithoutExtension(dllPath);
			assemblyName = assemblyName.Trim ();
			
			// If not found, fail
			if (assemblyName.Length == 0) {
				Console.Error.WriteLine ("(Engine.LoadAssembly) Assembly name not found in module path {0}", dllPath);
				return null;
			}
			
			Assembly assembly = null;
			try {
				assembly = Assembly.LoadFrom(dllPath);
			} catch (Exception ex){ //(System.IO.FileNotFoundException) {
				Console.Error.WriteLine ("(Engine.LoadAssembly) Failed to load assembly from module path {0}: {1}", dllPath, ex.Message);
				return null;
//#pragma warning disable 612
//					assembly = Assembly.LoadWithPartialName (tname);
//#pragma warning restore 612
			}
			
			try {
				scriptRuntime.LoadAssembly(assembly);
			} catch (Exception ex){
				Console.Error.WriteLine ("(Engine.LoadAssembly) Failed to load assembly {0} into script runtime: {1}", assemblyName, ex.Message);
				return null;
			}

			// Populate scope with assembly
			try {
				ScriptSource source;
				ScriptScope scope = engine.CreateScope(globals);
				string importStatement = String.Format ("import {0}", assemblyName);
				source = engine.CreateScriptSourceFromString(importStatement, SourceCodeKind.Statements);
				source.Execute(scope);
			} catch (Exception ex) {
				Console.Error.WriteLine ("(Engine.LoadAssembly) Error importing assembly {0} into global scope: {1}", assemblyName, ex.Message);
				return null;
			}
			
			// Add assembly reference to collection.
			// Do not add if already in keys to avoid problem with using foreach statement to load assemblies
			if (!loadedAssemblies.ContainsKey(dllPath)) loadedAssemblies[dllPath] = true;
			
			return assembly;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public uint TimeOut 
		{
			set
			{
				// Reset timeout value
				_timeOut = value;

				// Remove any existing timeout handler
				if (_timerID > 0) {
					GLib.Source.Remove(_timerID);
					_timerID = 0;
				}

				// Manage turbo flag

				// If turbo is not on and timeout setting warrents it, then turn on
				if (!_turbo && _timeOut <= 20) {
					_turbo = true;
					Console.WriteLine ("Turbo on");

				// If turbo is on and new timeout is too slow, turn it off
				} else if (_turbo && _timeOut > 20) {
					_turbo = false;
					Console.WriteLine ("Turbo off");
				}

				// If currently running, then reset timeout handler now
				if (_state == EngineState.Running) {
					_timerID = GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(OnTimerElapsed));
				}
			}
			get
			{
				return _timeOut;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		public bool IsRunning
//		{	// Determine if the program is running
//			get {
//				return _isRunning;
//
//				//return _timerRunning;
////				if (_callStacks == null) return false;
////				if (_callStacks.Count > 0) {
////					foreach (CallStack cs in _callStacks) {
////						if (cs.Enabled == true) {
////							return true;
////						}
////					}
////				}
////				return false;
//			}
//		}

		public void SetGlobalVariable (string variable, object value)
		{
			globals[variable] = value;
		}


		public object GetGlobalVariable (string variable)
		{
			return globals[variable];
		}

	        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public Dictionary<string,object> GetGlobals ()
		{
			return globals;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public Dictionary<string,object> GetLocals ()
		{
			Dictionary<string,object> locals = new Dictionary<string,object> ();
			//MessageBox.Show(String.Format ("{0} CallStacks", CallStacks.Count));
			for (int i=CallStacks.Count-1; i>=0; i--) {
				CallStack stack = CallStacks [i];
				StackFrame stack_frame = stack.GetTopFrame ();
				if (stack_frame != null) {
					ScriptScope scope = stack_frame.Scope;
					if (scope != null) {
						foreach (string name in scope.GetVariableNames()) {
							locals [name] = scope.GetVariable (name);
						}
					}
				}

				//RunnerResponse rr = stack_frame.Current;
				//if (rr.RetVal is ScriptScope) {
				//	ScriptScope scope = (ScriptScope)rr.RetVal;
				//	foreach (string name in scope.GetVariableNames()) {
				//		locals [name] = scope.GetVariable (name);
				//	}
				//}

			}
			return locals;
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public EngineState State
		{
			get {
				return _state;
			}
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public List<CallStack> CallStacks
		{	// Return the list if CallStacks currently in the engine
			get { return _callStacks; }
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool Reset(Jigsaw.Canvas cvs, InspectorWindow inspector)
		{
			// Recreate top level list of all call stacks. 
			// There can be more than one because Jigsaw allows multiple stacks to run simultaneously.
			if (_callStacks == null) 
				_callStacks = new List<CallStack>();
			else
				_callStacks.Clear();

			// Recreate globals and reload all assemblies
			globals = new Dictionary<string, object>();
			foreach (string dllPath in loadedAssemblies.Keys) LoadAssembly(dllPath);

			// A list of CControlStart block ScriptScopes
			List<ScriptScope> scopes = new List<ScriptScope> ();

			// A list of CProcedureStart blocks that are accumulated
			List<CProcedureStart> procStartBlocks = new List<CProcedureStart> ();

			// Reset the timeout parameter. This also resets turbo mode if appropriate.
			this.TimeOut = _timeOut;

			// Reset all blocks
			foreach (Diagram.CShape s in cvs.shapes) {

				if (s is CBlock) {								// If a shape is a block ...
					CBlock b = (CBlock)s;

					b.State = RunningState.Idle;					// Set block state to Idle
					b["Message"] = "";							// Clear current block message

					if (b.IsFactory == false) {					// If block is not a factory block ...
						b.Compile(engine, cvs);					// Ask block to compile itself

						// If also a ControlStart block ...
						if (s is CControlStart) {
							CControlStart cs = (CControlStart)s;
							ScriptScope scope = this.LoadBlockStack( cs );
							scopes.Add (scope);					// Temporarily save reference to new start block scope
						}

						// If also a CProcedureStart block, accumulate ref
						if (s is CProcedureStart) {
							CProcedureStart ps = (CProcedureStart)s;
							procStartBlocks.Add (ps);			// Temporarily save ref to procedure start blocks							
						}
					}
				}
			}
			
			// When we're all done, look to see if there are any procedures.
			// If so, get python of procedure and eval in scope of all start blocks 
			// to make them available to functions that use procedures as data.
			foreach (CProcedureStart ps in procStartBlocks) {
				StringBuilder sb = new StringBuilder();
				if (ps.ToPython(sb, 0) == true) {
					string py = sb.ToString();
					foreach (ScriptScope ss in scopes) {
						try
						{
							engine.Execute(py, ss);
						} catch (Exception ex) {
							Console.Error.WriteLine ("Error in Engine.Reset: {0}", ex.Message);
						}
					}
				} else {
					Console.Error.WriteLine ("Error in Engine.Reset: Could not generate source code for procedure {0}", ps.ProcedureName);
				}
			}

			RaiseEngineReset();
			return true;
		}
		
		// - - - Recompile a stack of blocks
		public bool CompileStack(CBlock start, Jigsaw.Canvas cvs) 
		{
			try
			{
				List<CBlock> stack = new List<CBlock>();
				stack.Add (start);
				
				while (stack.Count > 0) {
					// Compile the top of the stack
					CBlock b = stack[0];
					stack.RemoveAt (0);
					b.Compile(this.engine, cvs);
					
					// Add all subordinate blocks to the stack
					foreach (CEdge e in b.Edges) {
						if (e.Type != EdgeType.In && e.LinkedTo != null) {
							stack.Add ( e.LinkedTo.Block );
						}
					}
				}
				
				return true;
			} catch (Exception ex) {
				Console.Error.WriteLine ("Error on Engine.CompileStack: {0}", ex.Message);
				return false;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public ScriptScope LoadBlockStack( CBlock b, bool enabled = false ) {
			CallStack stack = new CallStack(globals);					// Add new runner to call stack
			ScriptScope scope = this.CreateScope(globals, globals);		// For the main start block, there is no local scope
			stack.AppendFrame( b, scope );
			_callStacks.Add(stack);
			stack.Enabled = enabled;
			
			return scope;
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
		public bool Step(bool runOnce = false)
		{
			// Block reentrance
			if (_inStep == true) return false;
			_inStep = true;

			// Take care of any pending events so we are up to date
			while (Gtk.Application.EventsPending ()) Gtk.Application.RunIteration ();

			int enabledCount = 0;
			for (int i=_callStacks.Count-1; i>=0; i--)
			{
				CallStack stack = _callStacks[i];
				
				if (stack.Enabled || runOnce) 
				{
					if (stack.Count > 0) 							// Get the top-most block runner
					{
						enabledCount++;
						StackFrame frame = stack.GetTopFrame();	
						
						bool rslt = frame.MoveNext();				// Advance block runner and see if fell off end
						RunnerResponse rr = frame.Current;			// Check the response

						// At any time, if requested to stop, wipe out all call stacks and exit
						if (rr.Action == EngineAction.Stop) {
							this.Stop ();
							break;
						}
						
						// React to other requests from execution blocks
						if (rslt == false) {						// The enumerator is done. Pop frame from stack.
							stack.PopFrame(frame);					// Enumerator has passed the end of the collection
						} else {
							switch (rr.Action) {					// Take appropriate action as directed by current runner
							case EngineAction.Pause:				// Pause the engine
								this.Pause();
								break;
							case EngineAction.Remove:				// Remove top frame and continue
								stack.PopFrame(frame);
								stack.RetVal = rr.RetVal;			// Copy returned value to temp location in stack
								break;
							case EngineAction.Return:				// Procedure returning
								stack.PopProcedure();				// Pop all the way up to procedure call
								stack.RetVal = rr.RetVal;			// Copy returned value to temp location in stack
								break;								// When proc call continues, it will pick up retval
							case EngineAction.Replace:				// This implements the blocks sequence
								stack.PopFrame(frame);				// Current frame is removed
								stack.PushFrame(rr.Frame);			// Next frame pushed, as indicated by runner
								rr.Frame.MoveNext();
								break;
							case EngineAction.Add:					// Pushes a new frame on to the stack
								stack.PushFrame(rr.Frame);
								rr.Frame.MoveNext();
								break;
							case EngineAction.Break:
								CBlock bnext = stack.PopBreak();	// Return block connected to out edge, if there is one
								if (bnext != null) {				// Scope was hidden in RetVal. Get it and make next frame.
									ScriptScope scope = (ScriptScope) rr.RetVal;
									StackFrame frame2 = bnext.Frame(scope, stack);
									stack.PushFrame(frame2);			// Push next frame and get started
									frame2.MoveNext();
								}
								break;
							case EngineAction.Error:
								this.Stop ();
								RaiseEngineError();
								break;
							case EngineAction.NoAction:
								break;
							}
						}
	
					} else {
						// If call stack is empty, the block runner stack has completed execution.
						// Remove stack from list of all stacks.
						_callStacks.RemoveAt(i);
					}
				}
			}
			
			//_inspector.Update (globals);
			if (!_turbo) RaiseEngineStep ();
			
			// If at least one stack is enabled and not paused, make sure the timer is started
			if (enabledCount > 0 && !runOnce) {
				if (_timerID == 0 && _timeOut > 0.0 && _state != EngineState.Paused) {
					_timerID = GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(OnTimerElapsed));
					_state = EngineState.Running;
				}
			}
			
			// If no call stacks, or none enabled and not paused, stop
			if ((enabledCount == 0 && _state != EngineState.Paused) || _callStacks.Count == 0 ) {
				this.Stop ();
			}
			
			//System.GC.Collect();		// Force garbage collection
			_inStep = false;
			return true;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Run()
		{
			//Console.WriteLine ("Engine.Run()");
			if (_callStacks.Count == 0)	return;
			foreach (CallStack s in _callStacks) s.Enabled = true;
			_state = EngineState.Running;	// Must set state to signal intention to other parts of program
			RaiseEngineRun();				// Indicate running is starting
			this.Step ();					// Kick-start running
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Stop()
		{
			// Update state to Stopped
			_state = EngineState.Stopped;

			// Remove any existing timeout handler
			if (_timerID > 0) {
				GLib.Source.Remove(_timerID);
				_timerID = 0;
			}

			// If turbo flag is on, turn off
			if (_turbo) {
				_turbo = false;   // Testing turbo mode
				Console.WriteLine ("Turbo off");
			}

			// Disable all call stacks
			foreach (CallStack s in _callStacks) s.Enabled = false;

			RaiseEngineStop ();	// Testing turbo mode
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void Pause()
		{
			if (_state == EngineState.Paused) return;

			// Update state to Paused
			_state = EngineState.Paused;

			// Remove any existing timeout handler
			if (_timerID > 0) {
				GLib.Source.Remove(_timerID);
				_timerID = 0;
			}

			// Disable all call stacks
			foreach (CallStack s in _callStacks) s.Enabled = false;

			RaiseEnginePause();
		}
		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		private bool StartTimer() 
//		{
//			// Start timer if not already started
//			if (_timerID == 0 && _timeOut > 0.0) {
//				_timerID = GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(OnTimerElapsed));
//				return true;
//			}
//
//			// The timer is started if not already running and if there is a valid timeout value	
////			if (_timerRunning == false && _timeOut > 0.0) {
////				//Console.WriteLine ("Engine.StartTimer()");
////				GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(OnTimerElapsed));
////				_timerContinue = true;
////				_timerRunning = true;
////				return true;			// Call timer repeatedly
////			}
//
//			return false;
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		private void StopTimer() 
//		{
//			if (_timerID > 0) {
//				GLib.Source.Remove (_timerID);
//				_timerID = 0;
//			}
//			_turbo = false;
//			//_timerContinue = false;
//			//_timerRunning = false;
//			//Console.WriteLine ("Engine.StopTimer()");
//		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		private bool OnTimerElapsed()
		{
			// Install new timeout if requested and stop this one
//			if (_timerReset == true) {
//				GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(OnTimerElapsed));
//				_timerReset = false;
//				return false;				// Terminate last timer and new one will take over
//			}
			
//			Console.WriteLine (_timeOut);
			
			// Install new timeout if requested and stop this one
//			if (_timerReset == true) {
//				if (_timeOut <= 20) {
//					_turbo = true;					// Testing turbo mode
//					while(_turbo) this.Step (true);	// Testing turbo mode
//					_timerReset = false;
//					return false;
//				} else {
//					_turbo = false;
//					GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(OnTimerElapsed));
//					_timerReset = false;
//					return false;
//				}
//			}
			
			// Exit if requested
//			if (_timerContinue == false) {
//				_timerRunning = false;
//				//Console.WriteLine ("OnTimerElapsed: Timer Stopped");
//				return false;
//			}
			
			//Console.WriteLine ("OnTimerElapsed: {0}", DateTime.Now.ToString ());

			if (_inTimeout) return true;
			_inTimeout = true;

			// Execute one step, unless in turbo mode, then execute multiple steps
			do {
				this.Step();
			} while (_turbo);

			_inTimeout = false;

			return true;

			// If the step advanced the engine to a completion point, stop
			//if (!this.IsRunning) this.Stop();
			
			// Return the current state indicating the desire to continue
			//return _timerContinue;
		}
		
//		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//		private bool OnTimerElapsed()
//		{
//			// Exit if requested
//			if (_timerContinue == false) return false;
//			
//			// Execute one step
//			this.Step();
//			
//			// If the step advanced the engine to a completion point, stop
//			if (!this.IsRunning) this.Stop();
//			
//			// Install new timeout if requested and stop this one
//			if (_timerReset) {
//				GLib.Timeout.Add(_timeOut, new GLib.TimeoutHandler(OnTimerElapsed));
//				_timerReset = false;
//				return false;
//			}
//			
//			// Return the current state indicating the desire to continue
//			return _timerContinue;
//		}
		
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
		
		// - - Raise the EngineError event - - - - - - - - - - - - - - -
        public void RaiseEngineError()
        {
            if (EngineError != null)
            {
				EventArgs e = new EventArgs();
            	EngineError(this, e);
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