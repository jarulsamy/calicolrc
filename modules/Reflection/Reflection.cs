// Relfection.Utils
// D.S. Blank <dblank@cs.brynmawr.edu>
// GPL, version 2

using System; // Type
using System.Reflection; // Method...
using System.Collections.Generic; // List
using System.Threading; // Thread

/*
  To get all types in an assembly:
  >>> Reflection.Utils.getTypeNames("Myro")
  ['AudioManager', 'AudioSpecCallbackDelegate', 'Computer', 'Extensions', 
   'Gamepads', 'MessageDialog', 'Myro', 'MyTexView', 'Randomizer', 'Robot', 
   'Scribbler', 'SimScribbler', 'Simulation']

  To get parameter names of those types:
  >>> type = Reflection.Utils.getType("Myro", "Scribbler")
  >>> Reflection.Utils.getConstructorParameterNames(type)
  [['serial'], ['port'], ['port', 'baud']]

  To get types of those parameters:
  >>> type = Reflection.Utils.getType("Myro", "Scribbler")
  >>> Reflection.Utils.getConstructorParameterTypes(type)
  [[<System.IO.Ports.SerialPort>], 
   [<System.String>], [<System.String>, <System.Int32>]]

  To get static methods of Assembly:
  >>> Reflection.Utils.getStaticMethodNames("Myro")
  ['CreateQualifiedName', 'GetAssembly', 'GetCallingAssembly', 
   'GetEntryAssembly', 'GetExecutingAssembly', 'Load', 'Load', 'Load', 'Load', 
   'Load', 'Load', 'Load', 'LoadFile', 'LoadFile', 'LoadFrom', 'LoadFrom', 
   'LoadFrom', 'LoadWithPartialName', 'LoadWithPartialName', 
   'ReflectionOnlyLoad', 'ReflectionOnlyLoad', 'ReflectionOnlyLoadFrom']

  To get static methods of Assembly Class:
  >>> Reflection.Utils.getStaticMethodNames("Myro", "Myro")
  ['ask', 'ask', 'ask', 'askQuestion', 'askQuestion', 'askQuestion', 
   'backward', 'backward', 'beep', 'beep', 'beep', 'beep', 'beep', 'beep', 
   'close_module', 'Color', 'Color', 'Contains', 'copyPicture', 
   'currentTime', ...]

  #NOTE: there is a name for each different signature.

  To get the parameters for an Assembly Class method:
  >>> Reflection.Utils.getParameterNames("Myro", "Myro", "beep")
  [['duration', 'frequency'], ['duration', 'frequency', 'frequency2'], 
   ['duration', 'frequency'], ['duration', 'frequency', 'frequency2'], 
   ['duration', 'frequency'], ['duration', 'frequency', 'frequency2']]

  #NOTE: there is a list for each set of types
  >>> Reflection.Utils.getParameterTypes("Myro", "Myro", "beep")
  [[<System.Double>, <System.Double>], ...]

  Finally, to call a method from a type:
  func = Reflection.Utils.getMethodFromArgValues(Myro, "beep", 1, 440)
  func.Invoke(Myro, new object [] {1, 440});

 */

namespace Reflection {
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
      retval.Sort();
      return retval;
    }

    public static Assembly getAssembly() {
      return Assembly.GetExecutingAssembly();
    }

    public static Assembly getAssembly(String name) {
		try {
      		return getAssembly(name, Thread.GetDomain().GetAssemblies());
		} catch {
			return null;
		}
    }

    public static Assembly getAssembly(String name, Assembly [] assemblies) {
      // given a name and set of assemblies, return the named assembly
      for(int i = 0; i < assemblies.Length; i++) {
	if (String.Compare(assemblies[i].GetName().Name, name) == 0)
	  return assemblies[i];
      }      
      // else, try to load it:
      Assembly assembly = null;
      try {
	assembly = Assembly.LoadFrom(name);
      } catch (System.IO.FileNotFoundException) {
#pragma warning disable 612
	assembly = Assembly.LoadWithPartialName(name);
#pragma warning restore 612
      }
      return assembly;
    }

    public static List<string> getTypeNames(Assembly assembly) {
      List<string> retval = new List<string>();
      foreach (Type type in assembly.GetExportedTypes()) {
		if (!retval.Contains(type.Name))
			retval.Add(type.Name);
      }
      retval.Sort();
      return retval;
    }

    public static List<string> getTypeNames(string assembly_name) {
      Assembly assembly = getAssembly(assembly_name);
      return getTypeNames(assembly);
    }

    public static List<string> getTypeNames() {
      return getTypeNames(Assembly.GetExecutingAssembly());
    }

    public static Type getType(string assembly_name, string type_name) {
      Assembly assembly = getAssembly(assembly_name);
	  if (assembly != null)
      	return getType(assembly, type_name);
	  else {
	  	return null;
	  }
    }

    public static Type getType(Assembly assembly, string type_name) {
      foreach (Type t in assembly.GetExportedTypes() ) {
	if (String.Compare(t.Name, type_name) == 0) {
	  return t;
	}
      }
      return null;
    }

    public static Type[] getTypesOfArgs(object [] objects) {
      Type [] retval = new Type[objects.Length];
      int count = 0;
      foreach (object obj in objects) {
	retval[count] = obj.GetType();
	count++;
      }
      return retval;
    }

    public static MethodInfo getMethodFromArgValues(Type type,
						    string methodName, 
						    params object [] args) {
      // get a method given arg values
      return type.GetMethod(methodName, getTypesOfArgs(args));
    }

    public static MethodInfo getMethodFromArgValues(object cls, 
						    string methodName, 
						    params object [] args) {
      // get a method given arg values
      Type type = cls.GetType();
      return type.GetMethod(methodName, getTypesOfArgs(args));
    }

    public static MethodInfo getMethodFromArgTypes(Type type,
						   string methodName, 
						   Type [] args) {
      // get a method given types
      return type.GetMethod(methodName, args);
    }

    public static MethodInfo getMethodFromArgTypes(object cls, 
						   string methodName, 
						   Type [] args) {
      // get a method given types
      Type type = cls.GetType();
      return type.GetMethod(methodName, args);
    }

    public static List<string> getMemberNames(Type type) {
      List<string> retval = new List<string>();
      foreach (MemberInfo mi in type.GetMembers() ) {
	retval.Add(mi.Name);
      }
      retval.Sort();
      return retval;
    }

    public static List<string> getStaticMethodNames(string aname) {
      Assembly assembly = getAssembly(aname);
      List<string> retval = new List<string>();
      foreach (MethodInfo mi in 
	       assembly.GetType().GetMethods(BindingFlags.Public |
					     BindingFlags.Static)) {
	retval.Add(mi.Name);
      }
      retval.Sort();
      return retval;
    }

    public static List<string> getStaticMethodNames(string aname, string tname) {
      Type type = getType(aname, tname);
      List<string> retval = new List<string>();
      foreach (MethodInfo mi in type.GetMethods(BindingFlags.Public |
						BindingFlags.Static)) {
		if (!retval.Contains(mi.Name))
			retval.Add(mi.Name);
      }
      retval.Sort();
      return retval;
    }

    public static MethodInfo[] getStaticMethods(Type type) {
      // get the methods of a Class (cls) given these flags:
      MethodInfo[] methodInfos = type.GetMethods(BindingFlags.Public |
						 BindingFlags.Static);
      // sort methods by name:
      Array.Sort(methodInfos,
		 delegate(MethodInfo methodInfo1, MethodInfo methodInfo2)
		 { return methodInfo1.Name.CompareTo(methodInfo2.Name); });
      return methodInfos;
    }

    public static List<string> getMethodNames(string aname, string tname) {
      Type type = getType(aname, tname);
      return getMethodNames(type);
    }

    public static List<string> getMethodNames(Type type) {
      List<string> retval = new List<string>();
      foreach (MemberInfo mi in type.GetMembers() ) {
	if (mi.MemberType==MemberTypes.Method) {
	  retval.Add(mi.Name);
	}
      }
      retval.Sort();
      return retval;
    }

    public static MemberInfo getMemberInfo(Type type, string mname) {
      foreach (MemberInfo mi in type.GetMembers() ) {
	if (String.Compare(mi.Name, mname) == 0) {
	  return mi;
	}
      }
      return null;
    }

    public static List<ConstructorInfo> getConstructorInfos(Type type) {
      List<ConstructorInfo> infos = new List<ConstructorInfo>();
      foreach (ConstructorInfo ci in type.GetConstructors() ) {
	infos.Add(ci);
      }
      return infos;
    }

    public static List<MethodInfo> getMethodInfos(Type type, string mname) {
      List<MethodInfo> methodinfos = new List<MethodInfo>();
      foreach (MethodInfo mi in type.GetMethods() ) {
		if (String.Compare(mi.Name, mname) == 0) {
	  		methodinfos.Add(mi);
		}
      }
      return methodinfos;
    }

    public static List<string> getPropertyNames(Type type, string mname) {
      List<string> retval = new List<string>();
      MemberInfo mi = getMemberInfo(type, mname);
      foreach (MethodInfo am in ((PropertyInfo) mi).GetAccessors() ) {
	retval.Add(am.Name);
      }
      retval.Sort();
      return retval;
    }

    public static List<List<string>> getConstructorParameterNames(Type type) {
      List<List<string>> retval = new List<List<string>>();
      foreach (ConstructorInfo mi in getConstructorInfos(type)) {
	List<string> parameters = new List<string>();
	foreach (ParameterInfo pi in mi.GetParameters() ) {
	  parameters.Add(pi.Name);
	}
	retval.Add(parameters);
      }
	return retval;
    }

    public static List<List<Type>> getConstructorParameterTypes(Type type) {
      List<List<Type>> retval = new List<List<Type>>();
      foreach (ConstructorInfo mi in getConstructorInfos(type)) {
	List<Type> parameters = new List<Type>();
	foreach (ParameterInfo pi in mi.GetParameters() ) {
	  parameters.Add(pi.ParameterType);
	}
	retval.Add(parameters);
      }
	return retval;
    }

    public static List<List<string>> getParameterNames(string aname, 
						 string tname, 
						 string mname) {
      List<List<string>> retval = new List<List<string>>();
      Type type = getType(aname, tname);
      	foreach (MethodInfo mi in getMethodInfos(type, mname)) {
			List<string> parameters = new List<string>();
			foreach (ParameterInfo pi in mi.GetParameters() ) {
	  			parameters.Add(pi.Name);
			}
			retval.Add(parameters);
      	}
      return retval;
    }

    public static List<List<Type>> getParameterTypes(string aname, string tname, string mname) {
      List<List<Type>> retval = new List<List<Type>>();
      Type type = getType(aname, tname);
      foreach (MethodInfo mi in getMethodInfos(type, mname)) {
	List<Type> types = new List<Type>();
	foreach (ParameterInfo pi in mi.GetParameters() ) {
	  types.Add(pi.ParameterType);
	}
	retval.Add(types);
      }
      return retval;
    }

    public static int add1(int i) {
      return i + 1;
    }
  }
}

