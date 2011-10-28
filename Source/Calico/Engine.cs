using System;
using System.Collections.Generic; // IList


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
	
	public class EngineManager {
		public Project calico;
		//public Microsoft.Scripting.Hosting.ScriptRuntimeSetup scriptRuntimeSetup;
		public Dictionary<string,Engine> engines;
        //public Microsoft.Scripting.Hosting.ScriptRuntime runtime;
        //public Microsoft.Scripting.Hosting.ScriptScope scope;
        public string stderr; 
		public string stdout; 
		public string stdin;

		
	    public EngineManager(Project calico) {
	        this.calico = calico;
	        //scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
			//Microsoft.Scripting.Hosting.LanguageSetup language = Microsoft.Scripting.Hosting.LanguageSetup(); //IronPython.Hosting.Python.CreateLanguageSetup(null);
    		//language.Options["FullFrames"] = true;
    		//scriptRuntimeSetup.LanguageSetups.Add(language);
	        //self.scriptRuntimeSetup.DebugMode = True
	        engines = new Dictionary<string,Engine>();
		}

	    //def __getitem__(self, name):
        //return self.engine[name]

	    public string [] get_languages() {
			// FIXME: sort
			string[] keys = new string[engines.Count];
			engines.Keys.CopyTo(keys, 0);
	        return keys;
		}

	    public void register(Language language) {
		    try {
	           	engines[language.name] = language.make_engine();
			} catch {
		        Console.WriteLine("Skipping language {0}", language.name);
			}
		}

	    public void setup() {
			//scriptRuntimeSetup = new Microsoft.Scripting.Hosting.ScriptRuntimeSetup();
			//Microsoft.Scripting.Hosting.LanguageSetup language = IronPython.Hosting.Python.CreateLanguageSetup(null);
    		//language.Options["FullFrames"] = true;
    		//scriptRuntimeSetup.LanguageSetups.Add(language);
			// 
			//runtime = new Microsoft.Scripting.Hosting.ScriptRuntime(
	        //    scriptRuntimeSetup);
	        //scope = runtime.CreateScope();
	        // Create calico as a module:
	        //scope.SetVariable("calico", this.calico);
	        // set up other items which can be imported:
	        //self.runtime.Globals.SetVariable("goodname", badname)
	        //[x for x in self.runtime.Globals.GetVariableNames()]
	        // Set up language engines:
	        foreach (string engine in engines.Keys) {
	            //try {
	                engines[engine].setup();
				//} catch {
	                //print("Engine failed to initialize: %s" % engine);
	                //del self.engine[engine]
				//	}
				}
			}

	    public void set_redirects(string stdout, string stderr, string stdin) { // textviews
	        this.stderr = stderr; 
			this.stdout = stdout; 
			this.stdin = stdin;
	        foreach (string engine in engines.Keys) {
	            engines[engine].set_redirects(this.stdout, this.stderr, this.stdin);
				}
			}
	
	    public void start() {
	        foreach (string engine in engines.Keys) {
	            engines[engine].start();
			}
					}
	
	    public void reset() {
	        setup();
	        start();
	        set_redirects(stdout, stderr, stdin);
			}
		}
}

