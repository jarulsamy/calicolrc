// CSharp code to assign Lua's print function
// D.S. Blank

public static class LuaEnv {
  static LuaSharp.ClrFunction print_function;
  static LuaSharp.LuaFunction cprint_function;

  // Can't do this in Python, so we do it here, and pass the items from Python:
  public static void setEnvironment(LuaSharp.Lua state) {
    LuaEnv.print_function = (LuaSharp.ClrFunction)new MyPrint();
    // console print, the old print
    LuaEnv.cprint_function = (LuaSharp.LuaFunction)state["print"]; 
  }
  
  public static void resetEnvironment(LuaSharp.Lua state) {
    state["cprint"] = cprint_function; // console print, the old print
    state["print"] = print_function; // gui print
  }
  
  // A Lua Function that calls a Python Function:
  public class MyPrint : LuaSharp.ClrFunction {
    public MyPrint() {
    }
    
    protected override object[] OnInvoke(LuaSharp.Lua state, object[] args) {
      try {
	if (args.Length == 0) {
	  System.Console.WriteLine("");
	} else if (args.Length == 1) {
	  System.Console.WriteLine(args[0]);
	} else if (args.Length > 1) {
	  foreach(object item in args) {
	    System.Console.Write(item);
	    System.Console.Write("\t");
	  }
	  System.Console.WriteLine("");
	}
      } catch {
	// pass
      }
      return new object[] {null};
    }
  }
}
