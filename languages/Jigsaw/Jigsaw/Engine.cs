using System;
using System.Collections;
using System.Collections.Generic;

// This file contains all code to implement the cooperative multitasking engine that executes Jigsaw programs.

namespace Jigsaw
{
	// -----------------------------------------------------------------------
	public enum EngineAction {
		NoAction = 0, 	// Just keep going.
		Add = 1, 		// Add new block runner to call stack.
		Replace = 2, 	// Replace current block runner with new one.
		Remove = 3, 	// Remove current block runner only.
		Break = 4		// Breakpoint. Stop timer.
	}
	
	// -----------------------------------------------------------------------
	// A tiny struct with the sole purpose of allowing Runners to return multiple items
	public class RunnerResponse {
		public EngineAction Action = EngineAction.NoAction;
		public IEnumerator<RunnerResponse> Runner = null;

		public RunnerResponse(EngineAction ea, IEnumerator<RunnerResponse> rr) {
			Action = ea;
			Runner = rr;
		}

		public RunnerResponse()
		{
		}
	}
	
	// -----------------------------------------------------------------------
	// Call stack manages the stack of block runners (calls) currently executing
	public class CallStack
	{
		private List<IEnumerator<RunnerResponse>> _stack = null;
		private Expression.Scope _locals = null;
		private Dictionary<string, object> _globals = null;
		private Dictionary<string, object> _builtins = null;
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CallStack(Dictionary<string, object> globals, 
						 Dictionary<string, object> builtins) {
			_stack = new List<IEnumerator<RunnerResponse>>();
			_locals = Expression.Engine.makeScope();
			_globals = globals;
			_builtins = builtins;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Return the locals
		public Expression.Scope Locals
		{
			get { return _locals; }
		}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Create a block runner given a block and append it to the end of the stack
		public void AppendRunner(CBlock b) {
			_stack.Add( b.Runner(_locals, _builtins) );
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
		public void RemoveRunner(IEnumerator<RunnerResponse> runner) {
			_stack.Remove(runner);
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public void PushRunner(IEnumerator<RunnerResponse> runner) {
			_stack.Insert(0, runner);
		}
	}
	
	// -----------------------------------------------------------------------
	public class Engine
	{
		//private List<List<IEnumerator<RunnerResponse>>> _callStacks = null;
		private List<CallStack> _callStacks = null;
		private InspectorWindow _inspector = null;
		private bool _inStep = false;		// true if currently executing a single step
		
		// This boolean is used by the GLib.TimeoutHandler method in Run
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
		public event EventHandler EngineReset;
		
		public Engine ()
		{
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Determine if the program is running
		public bool IsRunning {
			get {
				if (_callStacks == null) return false;
				if (_callStacks.Count > 0) return true;
				return false;
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Return the list if CallStacks currently in the engine
		public List<CallStack> CallStacks
		{
			get { return _callStacks; }
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public bool Reset(Jigsaw.Canvas cvs, InspectorWindow inspector)
		{
			// Save reference to the InspectorWindow
			_inspector = inspector;
			_inspector.ClearOutput();
			
			// Recreate top level list of all call stacks. 
			// There can be more than one because Jigsaw allows multiple stacks to run simultaneously.
			if (_callStacks == null) 
				_callStacks = new List<CallStack>(); //List<IEnumerator<RunnerResponse>>>();
			else
				_callStacks.Clear();
			
			// Reset all blocks
			// Find all CControlStart blocks on the canvas, setup the call stack and create a new scope.
			
			// Init the builtins namespace
			Dictionary<string, object> builtins = new Dictionary<string, object>();
			builtins["Inspector"] = _inspector;
			
			Dictionary<string, object> globals = new Dictionary<string, object>();
			
			// Init locals scope and use to create all control start block runners
			// Add runners to new call stacks in engine
			//Dictionary<string, object> locals;
			
			foreach (Diagram.CShape s in cvs.shapes) {
				if (s is CBlock) {
					CBlock b = (CBlock)s;
					b.State = BlockState.Idle;	// Move these statements to a CBlock method called Reset()
					b["Message"] = "";
					//b.MsgProp.Text = "";
				}
				if (s is CControlStart) {
					CControlStart cs = (CControlStart)s;
					if (cs.IsFactory == false) {
						CallStack stack = new CallStack(globals, builtins);
						stack.AppendRunner( (CControlStart)s );
						_callStacks.Add(stack);
						
//						List<IEnumerator<RunnerResponse>> stack = new List<IEnumerator<RunnerResponse>>();
//						locals = new Dictionary<string, object>();
//						stack.Add( (s as CControlStart).Runner(locals, builtins) );
					}
				}
			}
			
			RaiseEngineReset();
			return true;
		}
		
		/// <summary>
		/// Advance the entire system one step. 
		/// </summary>
		/// <returns>
		/// A <see cref="System.Boolean"/>
		/// </returns>
		public bool Step()
		{
			// Exit if already running a step
			if (_inStep == true) return false;
			_inStep = true;
			
			for (int i=_callStacks.Count-1; i>=0; i--) {
				
				//List<IEnumerator<RunnerResponse>> stack = _callStacks[i];
				CallStack stack = _callStacks[i];
				
				if (stack.Count > 0) {
					IEnumerator<RunnerResponse> br = stack.GetTopRunner();		// Get the top-most block runner
					//IEnumerator<RunnerResponse> br = stack[0];		// Get the top-most block runner
					
					bool rslt = br.MoveNext();						// Advance block runner and see if fell off end
					RunnerResponse rr = br.Current;					// Check the response
					
					// If there is a breakpoint, stop timer
					if (rr.Action == EngineAction.Break)
					{
						this.Stop();
					}
					
					// Conditions for removing the block runner from the stack
					if (rr.Action == EngineAction.Remove ||
					    rr.Action == EngineAction.Replace ||
					    rslt == false) {
						stack.RemoveRunner(br);
					}
					
					// Conditions for adding a new block runner
					if (rr.Action == EngineAction.Replace ||
			            rr.Action == EngineAction.Add ||
			            rr.Runner != null) {
						stack.PushRunner(rr.Runner);
						//stack.Insert(0, rr.Runner);
						rr.Runner.MoveNext();						// Take first step right away
					}

				} else {
					// If call stack is empty, the block runner stack has completed execution.
					// Remove stack from list of all stacks.
					_callStacks.RemoveAt(i);
				}
			}
			
			_inStep = false;
			RaiseEngineStep();
			
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
		private bool OnTimerElapsed()
		{
			// Execute one step
			this.Step();
			
			// If the step advanced the engine to a completion point, stop
			if (!this.IsRunning) this.Stop();
			
			// Return the current state of the desire to continue
			return _timerContinue;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Raise the EngineRun event
        public void RaiseEngineRun()
        {
            if (EngineRun != null)
            {
				EventArgs e = new EventArgs();
            	EngineRun(this, e);
            }
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Raise the EngineStep event
        public void RaiseEngineStep()
        {
            if (EngineStep != null)
            {
				EventArgs e = new EventArgs();
            	EngineStep(this, e);
            }
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Raise the EngineStop event
        public void RaiseEngineStop()
        {
            if (EngineStop != null)
            {
				EventArgs e = new EventArgs();
            	EngineStop(this, e);
            }
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		// Raise the EngineReset event
        public void RaiseEngineReset()
        {
            if (EngineReset != null)
            {
				EventArgs e = new EventArgs();
            	EngineReset(this, e);
            }
        }
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	}
}

