using System; // Type
using System.Reflection; // Method...
using System.Collections.Generic; // List
using System.Threading; // Thread

namespace Relfection {

  public class Utils {


    public static List<string> getAssemblyNames() {
      return getAssemblyNames(Assembly.GetExecutingAssembly());
    }

    public static List<string> getAssemblyNames(Assembly assembly) {
      // Get all the referenced assemblies
      List<string> retval = new List<string>();
      foreach (AssemblyName an in assembly.GetReferencedAssemblies() ) {
	retval.Add(an.Name);
      }
      return retval;
    }

    public static Assembly getAssembly() {
      return Assembly.GetExecutingAssembly();
    }

    public static Assembly getAssembly(String name) {
      return getAssembly(name, Thread.GetDomain().GetAssemblies());
    }

    public static Assembly getAssembly(String name, Assembly [] assemblies) {
      // given a name and set of assemblies, return the named assembly
      for(int i = 0; i < assemblies.Length; i++) {
	if (String.Compare(assemblies[i].GetName().Name, name) == 0)
	  return assemblies[i];
      }      
      return null;
    }

    public static List<string> getTypeNames() {
      return getTypeNames(Assembly.GetExecutingAssembly());
    }

    public static List<string> getTypeNames(Assembly assembly) {
      // Classes
      List<string> retval = new List<string>();
      foreach (Type t in assembly.GetExportedTypes() ) {
	retval.Add(t.Name);
      }
      return retval;
    }

    public static Type getType(string assembly_name, string type_name) {
      Assembly assembly = getAssembly(assembly_name);
      return getType(assembly, type_name);
    }

    public static Type getType(Assembly assembly, string type_name) {
      foreach (Type t in assembly.GetExportedTypes() ) {
	if (String.Compare(t.Name, type_name) == 0) {
	  return t;
	}
      }
      return null;
    }

    public static Type[] getTypes(Assembly assembly) {
      return assembly.GetExportedTypes();
    }

    public static Type[] getTypes(object [] objects) {
      Type [] retval = new Type[objects.Length];
      int count = 0;
      foreach (object obj in objects) {
	retval[count] = obj.GetType();
	count++;
      }
      return retval;
    }

    public static void getMethods(object cls) {
      Type type = cls.GetType();
      // get the methods of a Class (cls) given these flags:
      MethodInfo[] methodInfos = type.GetMethods(BindingFlags.Public |
						 BindingFlags.Static);
      // sort methods by name:
      Array.Sort(methodInfos,
		 delegate(MethodInfo methodInfo1, MethodInfo methodInfo2)
		 { return methodInfo1.Name.CompareTo(methodInfo2.Name); });
      
      // print them out:
      foreach (MethodInfo methodInfo in methodInfos) {
	Console.WriteLine(methodInfo.Name);
      }
    }

    public static MethodInfo getMethodFromArgValues(object cls, 
						    string methodName, 
						    params object [] args) {
      // get a method given arg values
      Type type = cls.GetType();
      return type.GetMethod(methodName, getTypes(args));
    }

    public static MethodInfo getMethodFromArgTypes(object cls, 
						   string methodName, 
						   Type [] args) {
      // get a method given types
      Type type = cls.GetType();
      return type.GetMethod(methodName, args);
    }

    public static int add1(int i) {
      return i + 1;
    }

    public static void Main(string [] args) {
      Utils cls = new Utils();
      Assembly assembly = getAssembly();
      getAssemblyNames(assembly);
      List<string> assemblies = getAssemblyNames();
      foreach (string aname in assemblies) {
	Assembly ass = getAssembly(aname);
	getTypes(ass);
	List<string> type_names = getTypeNames(ass);
	foreach (string tname in type_names) {
	  getType(aname, tname);
	}
      }
      
      MethodInfo func = getMethodFromArgValues(cls, "add1", 1);
      func.Invoke(cls, new object [] {1});
    }
  }
}

