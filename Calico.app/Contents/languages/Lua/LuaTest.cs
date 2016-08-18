// CSharp code to assign Lua's print function
// D.S. Blank

public static class LuaEnv {

  public static void Main() {
    LuaSharp.Lua state = new LuaSharp.Lua();
    setEnvironment(state);
    LuaSharp.LuaFunction p = (LuaSharp.LuaFunction)state["print"];
    p.Call(1);
    p.Call(2);
    p.Call(3);
  }

  public static void setEnvironment(LuaSharp.Lua state) {
    state["print"] = new MyPrint(); 
  }
  
  public class MyPrint : LuaSharp.ClrFunction {
    
    public MyPrint() {
    }
    
    protected override object[] OnInvoke(LuaSharp.Lua state, object[] args) {
      System.Console.Write("MyPrint: ");
      System.Console.WriteLine(args[0]);
      return new object[] {null};
    }
  }
}
