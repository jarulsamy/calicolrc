// CSharp code to assign Lua's print function
// D.S. Blank

using IronPython.Runtime; // PythonFunction
using System; // Func

public static class Extensions {
  public static T[] Slice<T>(this T[] source, int start, int end) {
    if (start == 0 && end == source.Length)
        return source;
        // Handles negative ends.
    if (end < 0) {
      end = source.Length + end;
    }
    int len = end - start;
    // Return new array.
    T[] res = new T[len];
    for (int i = 0; i < len; i++) {
      res[i] = source[i + start];
    }
    return res;
  }

  public static T[] Slice<T>(this T[] source, int start) {
    int end = source.Length - start;
    if (start == 0 && end == source.Length) 
      return source;
    int len = end - start;
    // Return new array.
    T[] res = new T[len];
    for (int i = 0; i < len; i++) {
      res[i] = source[i + start];
    }
    return res;
  }
}

public static class LuaEnv {

  // Can't do this in Python, so we do it here, and pass the items from Python:
  public static void setEnvironment(LuaSharp.Lua state, PythonFunction print_function, 
				    PythonFunction input_function) {
    state["cprint"] = state["print"]; // console print
    state["print"] = new MyPrint(print_function); // gui print
    // state["clr"] = new MyCLR(); // clr eval
    // FIXME: can't call a Gtk function from here?
    //((LuaSharp.LuaTable)state["io"])["read"] = new MyRead(input_function);
  }

  // A Lua Function that calls a Python Function:
  public class MyPrint : LuaSharp.ClrFunction {
    PythonFunction calico_print;

    public MyPrint(PythonFunction function) {
      calico_print = function;
    }

    protected override object[] OnInvoke(LuaSharp.Lua state, object[] args) {
      Gtk.Application.Invoke( delegate {
			IronPython.Runtime.Operations.PythonCalls.Call(calico_print, args);
		  });
      return new object[] {};
	}
  }

  // A Lua Function that calls a Python Function:
  public class MyRead : LuaSharp.ClrFunction {
    PythonFunction calico_ask;

    public MyRead(PythonFunction function) {
      calico_ask = function;
    }

    protected override object[] OnInvoke(LuaSharp.Lua state, object[] args) {
      Gtk.Application.Invoke( delegate {
	  IronPython.Runtime.Operations.PythonCalls.Call(calico_ask, args);
	});
      return new object[] {};
    }
  }

  // A Lua Function that calls a Python Function:
  public class MyCLR : LuaSharp.ClrFunction {

    public MyCLR() {
    }

    protected override object[] OnInvoke(LuaSharp.Lua state, object[] args) {
      object result = null;
      Gtk.Application.Invoke( delegate {
	  if (args[0] is PythonFunction) {
	    result = IronPython.Runtime.Operations.PythonCalls.Call(args[0], args.Slice(1));
	  } else {
	    Func<object,object> f = (Func<object,object>)args[0];
	    result = f(args.Slice(1));
	  }
	});
      return new object[] {result};
    }
  }
}
