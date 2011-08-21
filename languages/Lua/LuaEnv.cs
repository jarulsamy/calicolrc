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
    
    public static void print(object item) {
      if (item == null) {
	System.Console.Write("nil");
      } else if (item is LuaSharp.LuaTable) {
	// FIXME: needs to be a safe_print, to handle self-recursive data structures
	System.Console.Write("{");
	bool needs_a_comma = false;
	foreach (System.Collections.Generic.KeyValuePair<object,object> pair in (LuaSharp.LuaTable)item) {
	  if (needs_a_comma)
	    System.Console.Write(", ");
	  needs_a_comma = true;
	  if (pair.Key is System.Double) {
	    print(pair.Value);
	  } else {
	    print(pair.Key);
	    print("=");
	    if (pair.Value is LuaSharp.LuaTable)
	      print("{...}");
	    else
	      print(pair.Value);
	  }
	}
	System.Console.Write("}");
      } else {
	System.Console.Write(item);
      }
    }
    
    protected override object[] OnInvoke(LuaSharp.Lua state, object[] args) {
      try {
	if (args.Length == 0) {
	  System.Console.WriteLine("");
	} else if (args.Length > 0) {
	  int count = 0;
	  foreach(object item in args) {
	    print(item);
	    if (count < args.Length - 1)
	      System.Console.Write("\t");
	    count++;
	  }
	  System.Console.WriteLine("");
	}
      } catch {
	// pass
      }
      return new object[] {null};
    }
  }
  /*
  public static System.Collections.Generic.List<string> ToList(LuaSharp.LuaTable table) {
    System.Collections.Generic.List<string> list = new System.Collections.Generic.List<string>();
    //foreach (object pair in table) {
    //System.Collections.Generic.KeyValuePair<object,object> pair in table) {
    //list.Add(pair.Key.ToString());
    list.Add(table.ToString());
    //}
    return list;
  }
  */

  public static System.Collections.Generic.List<object> ToList(LuaSharp.LuaTable table) {
    System.Collections.Generic.List<object> list = new System.Collections.Generic.List<object>();
    System.Collections.IDictionaryEnumerator ienum = (System.Collections.IDictionaryEnumerator)table.GetEnumerator();
    while (ienum.MoveNext()) {
      list.Add(ienum.Key); //, ienum.Value);
    }
    return list;
  }
}
