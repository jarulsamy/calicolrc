// Utility Functions for Running Scheme in CSharp
/*
  ;; Calico Scheme interpreter with support for choose
  ;;
  ;; Written by Douglas S. Blank and James B. Marshall
  ;; dblank@brynmawr.edu
  ;; http://cs.brynmawr.edu/~dblank
  ;; jmarshall@slc.edu
  ;; http://science.slc.edu/~jmarshall
*/

using System;
using System.IO; // File
using System.Reflection; // Assembly
using Microsoft.Scripting.Math;
using Microsoft.Scripting.Hosting;
using System.Collections; // Hashtable
using System.Collections.Generic; // List
using IronPython;

public class Method {
  public object classobj;
  public MethodInfo method;

  public Method(object classobj, MethodInfo method) {
    this.classobj = classobj;
    this.method = method;
  }
}

public class MethodNeedsArgs {
  public object classobj;
  public string name;
  
  public MethodNeedsArgs(object classobj, string name) {
    this.classobj = classobj;
    this.name = name;
  }
}

public class Config {
  public int DEBUG = 0;
  public bool NEED_NEWLINE = false;
  Hashtable symbol_table = new Hashtable(); //Default one
  public List<Assembly> assemblies = new List<Assembly>();
  int symbol_count = 1;

  public Config() {
  }

  public Symbol symbol(string ssymbol) {
	if (!symbol_table.ContainsKey(ssymbol)) {
	  var newsym = new Symbol(ssymbol, symbol_count);
	  symbol_table.Add(ssymbol, newsym);
	  symbol_count++;
	  return newsym;
	}
	return (Symbol) symbol_table[ssymbol];
  }

  public void AddAssembly(Assembly assembly) {
	assemblies.Add(assembly);
  }
}

public class Symbol : IList {
  string id;
  int val;
  
  public Symbol(string id, int val) {
	this.id = id;
	this.val = val;
  }
  
  public override bool Equals(object other) {
	return ((other is Symbol) && (this.val == ((Symbol)other).val));
  }
  
  public override int GetHashCode() {
	return id.GetHashCode();
  }
  
  public override string ToString() {
	return this.id;
  }

  public String __repr__() {
      return id;
  }

  // Items necessary for it to be an IList
  // (used in other DLR interfaces)
  public bool IsFixedSize {
	get {
	  return true;
	}
  }

  public bool IsReadOnly {
	get {
	  return true;
	}
  }

  public bool IsSynchronized {
	get {
	  return false;
	}
  }

  public void CopyTo(System.Array array, int index) {
  }

  public int Add(object value) {
	return 0; 
  }

  public int Count {
    get {
      return 0;
    }
  }

  public void Remove(object value) {
  }

  public void RemoveAt(int index) {
  }

  public void Clear() {
  }

  public bool Contains(object value) {
	return false;
  }

  public int IndexOf(object value) {
	return -1;
  }

  public void Insert(int index, object value) {
  }

  public object this[int index] {
	get {
	  throw new Exception("EmptyList does not contain any elements.");
	}
	set { // value is the item
	}
  }
  public object SyncRoot {
	get {
	  return this;
	}
  }
  public IEnumerator GetEnumerator() {
    // FIXME: return enumerator that returns nothing
    // Refer to the IEnumerator documentation for an example of
    // implementing an enumerator.
    throw new Exception("The method or operation is not implemented.");
  }
}

public class Rational {
  public int numerator;
  public int denominator;
  
  public Rational(int num) {
	this.numerator = num;
	this.denominator = 1;
  }
  
  public Rational(int numerator, int denominator) {
	if (denominator == 0)
	  throw new Exception("cannot represent rationals with a zero denominator");
	int gcd = GCD(numerator, denominator);
	this.numerator = numerator/gcd;
	this.denominator = denominator/gcd;
  }
  
  public static int GCD(int n1, int n2) {
	// Greatest Common Denominator
	n1 = Math.Abs(n1);
	n2 = Math.Abs(n2);
	if (n1 == 0) return n2;
	if (n2 == 0) return n1;
	if (n1 > n2) return GCD(n2, n1 % n2);
	else         return GCD(n1, n2 % n1);
  }
  
  public static int LCM(int n1, int n2) {
	// Least Common Multiple
	n1 = Math.Abs(n1);
	n2 = Math.Abs(n2);
	if (n1 > n2) return checked((n2 / GCD(n1, n2)) * n1);
	else         return checked((n1 / GCD(n1, n2)) * n2);
  }
  
  public override bool Equals(object other) {
	return (other is Rational &&
		(this.numerator == ((Rational)other).numerator &&
			this.denominator == ((Rational)other).denominator));
  }
  
  public override int GetHashCode() {
	double d = ((double) numerator) / denominator;
	return d.GetHashCode();
  }
  
  public String __repr__() {
      return ToString();
  }

  public override string ToString() {
	//if (denominator != 1)
	return string.Format("{0}/{1}", numerator, denominator);
	//else
	//return numerator.ToString();
  }
  
  public static implicit operator double(Rational f) {
	return (((double) f.numerator) / ((double) f.denominator));
  }
  
  public static implicit operator int(Rational f) {
	return f.numerator / f.denominator;
  }
  
  public static implicit operator float(Rational f) {
	return (((float) f.numerator) / ((float) f.denominator));
  }
  
  public static Rational operator +(Rational f1, Rational f2) {
	int lcm = LCM(f1.denominator, f2.denominator);
	return new Rational((f1.numerator * lcm/f1.denominator +
			f2.numerator * lcm/f2.denominator),
		lcm);
  }
  
  public static Rational operator +(Rational f1, int i) {
	int lcm = LCM(f1.denominator, 1);
	return new Rational((f1.numerator * lcm/f1.denominator +
			i * lcm/1),
		lcm);
  }
  
  public static Rational operator +(int i, Rational f1) {
	int lcm = LCM(f1.denominator, 1);
	return new Rational((f1.numerator * lcm/f1.denominator +
			i * lcm/1),
		lcm);
  }

  public static Rational operator -(Rational f1, Rational f2) {
	int lcm = LCM(f1.denominator, f2.denominator);
	return new Rational((f1.numerator * lcm/f1.denominator -
			f2.numerator * lcm/f2.denominator),
		lcm);
  }
  
  public static Rational operator -(Rational f1, int i) {
	int lcm = LCM(f1.denominator, 1);
	return new Rational((f1.numerator * lcm/f1.denominator -
			i * lcm/1),
		lcm);
  }
  
  public static Rational operator -(int i, Rational f1) {
	int lcm = LCM(f1.denominator, 1);
	return new Rational((i * lcm/1) - (f1.numerator * lcm/f1.denominator),
		lcm);
  }

  public static Rational operator *(Rational f1, Rational f2) {
	return new Rational((f1.numerator * f2.numerator),
		(f1.denominator * f2.denominator));
  }
  
  public static Rational operator *(Rational f1, int i) {
	return new Rational((f1.numerator * i), f1.denominator);
  }
  
  public static Rational operator *(int i, Rational f1) {
	return new Rational((f1.numerator * i), f1.denominator);
  }

  public static Rational operator /(Rational f1, Rational f2) {
	return new Rational((f1.numerator * f2.denominator),
		                (f1.denominator * f2.numerator));
  }
  
  public static Rational operator /(Rational f1, int i) {
	return new Rational(f1.numerator, f1.denominator * i);
  }
  
  public static Rational operator /(int i, Rational f1) {
	return new Rational(f1.denominator * i, f1.numerator);
  }

}

public class Scheme {

  public static int CONS_ID = 0;

  private static ScriptScope _dlr_env;
  private static ScriptRuntime _dlr_runtime;

    //static LineEditor lineEditor = new LineEditor(null);
  public static Config config = new Config();

  public static Symbol EmptyList = (Symbol) symbol("()");

  public delegate object Closure(params object[] args);
  public delegate void Function();
  public delegate bool Predicate(object obj);
  public delegate bool Predicate2(object obj1, object obj2);

  public delegate object Procedure0();
  public delegate void Procedure0Void();
  public delegate bool Procedure0Bool();

  public delegate object Procedure1(object args);
  public delegate void Procedure1Void(object args);
  public delegate bool Procedure1Bool(object args);

  public delegate object Procedure2(object args1, object args2);
  public delegate void Procedure2Void(object args1, object args2);
  public delegate bool Procedure2Bool(object args1, object args2);

  public delegate object Procedure3(object args1, object args2, object args3);
  public delegate void Procedure3Void(object args1, object args2, object args3);
  public delegate bool Procedure3Bool(object args1, object args2, object args3);

    public delegate object Procedure4(object a0, object a1, object a2, object a3);
    public delegate object Procedure5(object a0, object a1, object a2, object a3, object a4);
    public delegate object Procedure6(object a0, object a1, object a2, object a3, object a4, object a5);
    public delegate object Procedure7(object a0, object a1, object a2, object a3, object a4, object a5, object a6);
    public delegate object Procedure8(object a0, object a1, object a2, object a3, object a4, object a5, object a6, object a7);
    public delegate object Procedure9(object a0, object a1, object a2, object a3, object a4, object a5, object a6, object a7, object a8);
    public delegate object Procedure10(object a0, object a1, object a2, object a3, object a4, object a5, object a6, object a7, object a8, object a9);

    public delegate object ProcedureN(params object [] args);

  public static object symbol(object symbol) {
	return config.symbol(symbol.ToString());
  }

  public static BigInteger makeBigInteger(int value) {
	int sign = +1;
	if (value < 0) {
	  sign = -1;
	  value *= -1;
	}
	return new BigInteger(sign, (uint)value);
  }

  public static BigInteger BigIntegerParse(string value) {
	int radix = 10;
	BigInteger multiplier = makeBigInteger(1);
	BigInteger result = makeBigInteger(0);
	value = (value.ToUpper()).Trim();
	int limit = 0;
	if(value[0] == '-')
	  limit = 1;
	for(int i = value.Length - 1; i >= limit ; i--) {
	  int posVal = (int)value[i];
	  if(posVal >= '0' && posVal <= '9')
		posVal -= '0';
	  else if(posVal >= 'A' && posVal <= 'Z')
		posVal = (posVal - 'A') + 10;
	  else
		posVal = 9999999;       // error flag
	  
	  if(posVal >= radix)
		throw(new ArithmeticException("Invalid string in constructor."));
	  else {
		if(value[0] == '-')
		  posVal = -posVal;
		result = result + (multiplier * posVal);
		if((i - 1) >= limit)
		  multiplier = multiplier * radix;
	  }
	}
	return result;
  }
  
  public class Proc {
	object proc = null;
	int args = -1;
	int returntype = 1;
	public string repr = null;

	public Proc(string repr, object proctype, int args, int returntype) {
	  this.repr = repr;
	  this.proc = proctype;
	  this.args = args;
	  this.returntype = returntype;
	}
	public object Call(object actual) {
	  object retval = null;
	  if (returntype == 0) { // void return
		if (args == -1) 
		  ((Procedure1Void)proc)(actual);
		else if (args == 0) 
		  ((Procedure0Void)proc)();
		else if (args == 1) {
		  if (pair_q(actual))
			((Procedure1Void)proc)(car(actual));
		  else
			((Procedure1Void)proc)(actual);
		} else if (args == 2) {
		  ((Procedure2Void)proc)(car(actual), cadr(actual));
		} else if (args == 3) {
		  ((Procedure3Void)proc)(car(actual), cadr(actual), caddr(actual));
		} else {
		  throw new Exception(string.Format("error in call: invalid args count"));
		}
	  } else if (returntype == 1) { // return object
		if (args == -1) 
		  retval = ((Procedure1)proc)(actual);
		else if (args == 0) 
		  retval = ((Procedure0)proc)();
		else if (args == 1) {
		  if (pair_q(actual))
			retval = ((Procedure1)proc)(car(actual));
		  else
			retval = ((Procedure1)proc)(actual);
		} else if (args == 2) {
		  retval = ((Procedure2)proc)(car(actual), cadr(actual));
		} else if (args == 3) {
		  retval = ((Procedure3)proc)(car(actual), cadr(actual), caddr(actual));
		} else {
		  throw new Exception(string.Format("error in call: invalid args count"));
		}
	  } else if (returntype == 2) { // return bool
		if (args == -1) 
		  retval = ((Procedure1Bool)proc)(actual);
		else if (args == 0) 
		  retval = ((Procedure0Bool)proc)();
		else if (args == 1) {
		  if (pair_q(actual))
			retval = ((Procedure1Bool)proc)(car(actual));
		  else
			retval = ((Procedure1Bool)proc)(actual);
		} else if (args == 2) {
		  retval = ((Procedure2Bool)proc)(car(actual), cadr(actual));
		} else if (args == 3) {
		  retval = ((Procedure3Bool)proc)(car(actual), cadr(actual), caddr(actual));
		} else {
		  throw new Exception(string.Format("error in call: invalid args count"));
		}
	  } else {
	    throw new Exception(string.Format("error in call: invalid return type"));
	  }
	  return retval;
	}

	public object Call(object args1, object args2) {
	  object retval = null;
	  if (returntype == 0) { // return void
		((Procedure2Void)proc)(args1, args2);
	  } else if (returntype == 1) { // return object
		retval = ((Procedure2)proc)(args1, args2);
	  } else if (returntype == 2) { // return bool
		retval = ((Procedure2Bool)proc)(args1, args2);
	  } else {
	    throw new Exception(string.Format("error in call: invalid return type"));
	  }
	  return retval;
	}

        public String __repr__() {
	    return ToString();
	}

	public override string ToString() {
	  return String.Format("#<procedure {0}>", repr);
	}

  }

  // ProcedureN - N is arg count coming in
  // -1, 1, 2 - number of pieces to call app with (-1 is all)
  // 0, 1, 2 - return type 0 = void, 1 = object, 2 = bool

    public enum ReturnType { ReturnVoid=0, ReturnObject=1, ReturnBool=2 };
    public enum TakesType { TakesAll=-1, TakesOne=1, TakesTwo=2 };

  public static Proc Add_proc = new Proc("+", (Procedure1)Add, -1, 1);
  // FIXME: make these four different:
    /*
      1. eq? evaluates to #f unless its parameters represent the same
      data object in memory;
      
      2. eqv? is generally the same as eq? but treats primitive
      objects (e.g. characters and numbers) specially so that numbers
      that represent the same value are eqv? even if they do not refer
      to the same object;
      
      3. equal? compares data structures such as lists, vectors and
      strings to determine if they have congruent structure and eqv?
      contents.(R5RS sec. 6.1)[3]

      4. = compares numbers
      
    */
  public static Proc Equal_proc = new Proc("equal?", (Procedure1Bool) Equal, -1, 2);
  public static Proc Eq_proc = new Proc("eq?", (Procedure1Bool) Eq, -1, 2);
  public static Proc Eqv_proc = new Proc("eqv?", (Procedure1Bool) Eqv, -1, 2);
  public static Proc EqualSign_proc = new Proc("=", (Procedure1Bool) EqualSign, -1, 2);
  public static Proc GreaterThan_proc = new Proc(">", (Procedure1Bool) GreaterThan, -1, 2);
  public static Proc LessThan_proc = new Proc("<", (Procedure1Bool) LessThan, -1, 2);
  public static Proc LessThan_is__proc = new Proc("<=", (Procedure1Bool) LessThanOrEqual, -1, 2);
  public static Proc GreaterOrEqual_proc = new Proc(">=", (Procedure1Bool) GreaterThanOrEqual, -1, 2);
  public static Proc Multiply_proc = new Proc("*", (Procedure1) Multiply, -1, 1);
  public static Proc modulo_proc = new Proc("modulo", (Procedure2) modulo, 2, 1);
  public static Proc Divide_proc = new Proc("/", (Procedure1) Divide, -1, 1);
  public static Proc Subtract_proc = new Proc("-", (Procedure1) Subtract, -1, 1);
  public static Proc car_proc = new Proc("car", (Procedure1) car, 1, 1);
  public static Proc cdr_proc = new Proc("cdr", (Procedure1) cdr, 1, 1);
  public static Proc cadr_proc = new Proc("cadr", (Procedure1) cadr, 1, 1);
  public static Proc make_external_proc_proc = new Proc("make-external-proc", (Procedure1) make_external_proc, 1, 1);
  public static Proc vector_proc = new Proc("vector", (ProcedureN) vector, -1, 1);
  public static Proc vector_native_proc = new Proc("vector", (Procedure1) vector_native, -1, 1);
  public static Proc caddr_proc = new Proc("caddr", (Procedure1) caddr, 1, 1);
  public static Proc cons_proc = new Proc("cons", (Procedure2) cons, 2, 1);
  public static Proc make_vector_proc = new Proc("make-vector", (Procedure1) make_vector, 1, 1);
  public static Proc list_to_vector_proc = new Proc("list->vector", (Procedure1) list_to_vector, 1, 1);
  public static Proc vector_ref_proc = new Proc("vector-ref", (Procedure2) vector_ref, 2, 1);
  public static Proc vector_length_proc = new Proc("vector-length", (Procedure1) vector_length, 1, 1);
  public static Proc memq_proc = new Proc("memq", (Procedure2) memq, 2, 1);
  public static Proc range_proc = new Proc("range", (Procedure1) range, -1, 1);
  public static Proc reverse_proc = new Proc("reverse", (Procedure1) reverse, 1, 1);
  public static Proc sort_proc = new Proc("sort", (Procedure2) sort, 2, 1);
  public static Proc set_car_b_proc = new Proc("set-car!", (Procedure2Void) set_car_b, 2, 0);
  public static Proc set_cdr_b_proc = new Proc("set-cdr!", (Procedure2Void) set_cdr_b, 2, 0);
  public static Proc sqrt_proc = new Proc("sqrt", (Procedure1) sqrt, -1, 1);
  public static Proc abs_proc = new Proc("abs", (Procedure1) abs, -1, 1);
  public static Proc string_to_symbol_proc = new Proc("string->symbol", (Procedure1) string_to_symbol, 1, 1);
  public static Proc string_to_number_proc = new Proc("string->number", (Procedure1) string_to_number, 1, 1);
  public static Proc stringLessThan_q_proc = new Proc("string<?", (Procedure2Bool) stringLessThan_q, 2, 2);
  public static Proc symbol_q_proc = new Proc("symbol?", (Procedure1Bool) symbol_q, 1, 2);
  public static Proc number_q_proc = new Proc("number?", (Procedure1Bool) number_q, 1, 2);
  public static Proc boolean_q_proc = new Proc("boolean?", (Procedure1Bool) boolean_q, 1, 2);
  public static Proc string_q_proc = new Proc("string?", (Procedure1Bool) string_q, 1, 2);
  public static Proc char_q_proc = new Proc("char?", (Procedure1Bool) char_q, 1, 2);
  public static Proc char_to_string_proc = new Proc("char->string", (Procedure1) char_to_string, 1, 1);
  public static Proc char_whitespace_q_proc = new Proc("char-whitespace?", (Procedure1Bool) char_whitespace_q, 1, 2);
  public static Proc char_alphabetic_q_proc = new Proc("char-alphabetic?", (Procedure1Bool) char_alphabetic_q, 1, 2);
  public static Proc char_numeric_q_proc = new Proc("char-numeric?", (Procedure1Bool) char_numeric_q, 1, 2);
  public static Proc char_is__q_proc = new Proc("char=?", (Procedure2Bool) char_eq_q, 2, 2);
  public static Proc list_to_string_proc = new Proc("list->string", (Procedure1) list_to_string, 1, 1);
  public static Proc pair_q_proc = new Proc("pair?", (Procedure1Bool) pair_q, 1, 2);
  public static Proc format_proc = new Proc("format", (Procedure1) format_prim, -1, 1);
  public static Proc null_q_proc = new Proc("null?", (Procedure1Bool) null_q, 1, 2);
  public static Proc atom_q_proc = new Proc("atom?", (Procedure1Bool) atom_q, 1, 2);
  public static Proc assq_proc = new Proc("assq", (Procedure2) assq, 2, 1);
  public static Proc string_append_proc = new Proc("string-append", (Procedure1) string_append, -1, 1);
  public static Proc string_length_proc = new Proc("string-length", (Procedure1) string_length, 1, 1);
  public static Proc string_ref_proc = new Proc("string-ref", (Procedure2) string_ref, 2, 1);
  public static Proc display_proc = new Proc("display", (Procedure1Void) display, 1, 0);
  public static Proc pretty_print_proc = new Proc("pretty-print", (Procedure1Void) pretty_print, -1, 0);
  //  public static Proc append_proc = new Proc("append", (Procedure1) append, -1, 1);
  //public static Proc make_binding_proc = new Proc("make-binding",(Procedure2)make_binding, 2, 1);
  public static Proc make_binding_proc = new Proc("make-binding",(Procedure2)PJScheme.make_binding, 2, 1);
  public static Proc printf_prim_proc = new Proc("printf",(Procedure1)printf_prim, -1, 1);
  public static Proc get_member_proc = new Proc("get-member", (Procedure2)get_external_member_name, 2, 1);
  public static Proc dlr_env_contains_proc = new Proc("dlr-env-contains",(Procedure1Bool)dlr_env_contains, 1, 2);
  public static Proc dlr_env_lookup_proc = new Proc("dlr-env-lookup",(Procedure1)dlr_env_lookup, 1, 1);
  public static Proc car_hat_proc = new Proc("car^",(Procedure1)PJScheme.car_hat, 1, 1);
  public static Proc cdr_hat_proc = new Proc("cdr^",(Procedure1)PJScheme.cdr_hat, 1, 1);
  public static Proc cadr_hat_proc = new Proc("cadr^",(Procedure1)PJScheme.cadr_hat, 1, 1);
  public static Proc cddr_hat_proc = new Proc("cddr^",(Procedure1)PJScheme.cddr_hat, 1, 1);
  public static Proc caddr_hat_proc = new Proc("caddr^",(Procedure1)PJScheme.caddr_hat, 1, 1);
  public static Proc cdddr_hat_proc = new Proc("cdddr^",(Procedure1)PJScheme.cdddr_hat, 1, 1);
  public static Proc cadddr_hat_proc = new Proc("cadddr^",(Procedure1)PJScheme.cadddr_hat, 1, 1);
  public static Proc safe_print_proc = new Proc("safe-print", (Procedure1Void)safe_print, 1, 0);
  public static Proc list_ref_proc = new Proc("list-ref", (Procedure2) list_ref, 2, 1);
  public static Proc aunparse_proc = new Proc("unparse", (Procedure1) PJScheme.aunparse, 1, 1);
  public static Proc string_is__q_proc = new Proc("string=?", (Procedure1Bool) PJScheme.string_eq_q, -1, 2);

  public static Proc binding_variable_proc = new Proc("binding_variable", 
							(Procedure1) PJScheme.binding_variable,
							2, 1);
  public static Proc get_variables_from_frame_proc = new Proc("get_variables_from_frame", 
								(Procedure1) PJScheme.get_variables_from_frame,
								1, 1);

    // Add new procedures above here!
    // Then add low-level C# code below

  public static char TILDE = '~';
  public static char NULL = '\0';
  public static char NEWLINE = '\n';
  public static char SINGLEQUOTE = '\'';
  public static char DOUBLEQUOTE = '"';
  public static char BACKQUOTE = '`';
  public static char BACKSPACE = '\b';
  public static char BACKSLASH = '\\';
  public static char SLASH = '/';
  public static char[] SPLITSLASH = {SLASH};
  public static string NEWLINE_STRING = "\n";

  public static bool true_q (object v) {
      // only #f is false, everything else is true
      if (v is bool) {
	  return ((bool) v);
      } else
	  return true;
  }

  public static Func<object,bool> module_q = tagged_list(symbol("module"), (Predicate2)GreaterOrEqual, 1);
  public static Func<object,bool> environment_q = tagged_list(symbol("environment"), (Predicate2)GreaterOrEqual, 1);

  public static object get_current_time() {
	DateTime baseTime = new DateTime(1970, 1, 1, 8, 0, 0);
	DateTime nowInUTC = DateTime.UtcNow;
	return ((nowInUTC - baseTime).Ticks / 10000000.0);
  }

  public static object symbol_to_string (object x) {
	return x.ToString();
  }

  public static object group(object chars, object delimiter) {
	// given list of chars and a delim char, return a list of strings
	object retval = EmptyList;
	object buffer = EmptyList;
	object current1 = chars;
	while (!Eq(current1, EmptyList)) {
	  if (Eq(car(current1), delimiter)) {
		retval = cons(list_to_string(reverse(buffer)), retval);
		buffer = EmptyList;
	  } else {
		buffer = cons(car(current1), buffer);
	  }
	  current1 = cdr(current1);
	}
	if (!Eq(buffer, EmptyList))
	  retval = cons(list_to_string(reverse(buffer)), retval);
	return reverse(retval);
  }

  public static object make_proc(params object[] args) {
	return cons(symbol("procedure"), list(args));
  }

  // given a name, return a function that given an array, returns object
  public static Procedure2 make_instance_proc(object tname) {
	return (path, args) => call_external_proc(tname, path, args);
  }


  
  public static object make_initial_env_extended (object env) {
      // ProcedureN - N is arg count coming in
      // -1, 1, 2 - number of pieces to call app with (-1 is all)
      // 0, 1, 2 - return type 0 = void, 1 = object, 2 = bool
      object primitives = list(
	       list(symbol("assq"), assq_proc),
	       list(symbol("atom?"), atom_q_proc),
	       list(symbol("caaaar"), new Proc("caaaar", (Procedure1)caaaar, 1, 1)),
	       list(symbol("caaadr"), new Proc("caaadr", (Procedure1)caaadr, 1, 1)),
	       list(symbol("caaar"), new Proc("caaar", (Procedure1)caaar, 1, 1)),
	       list(symbol("caadar"), new Proc("caadar", (Procedure1)caadar, 1, 1)),
	       list(symbol("caaddr"), new Proc("caaddr", (Procedure1)caaddr, 1, 1)),
	       list(symbol("caadr"), new Proc("caadr", (Procedure1)caadr, 1, 1)),
	       list(symbol("caar"), new Proc("caar", (Procedure1)caar, 1, 1)),
	       list(symbol("cadaar"), new Proc("cadaar", (Procedure1)cadaar, 1, 1)),
	       list(symbol("cadadr"), new Proc("cadadr", (Procedure1)cadadr, 1, 1)),
	       list(symbol("cadar"), new Proc("cadar", (Procedure1)cadar, 1, 1)),
	       list(symbol("caddar"), new Proc("caddar", (Procedure1)caddar, 1, 1)),
	       list(symbol("cadddr"), new Proc("cadddr", (Procedure1)cadddr, 1, 1)),
	       list(symbol("caddr"), new Proc("caddr", (Procedure1)caddr, 1, 1)),
	       list(symbol("cadr"), new Proc("cadr", (Procedure1)cadr, 1, 1)),
	       list(symbol("cdaaar"), new Proc("cdaaar", (Procedure1)cdaaar, 1, 1)),
	       list(symbol("cdaadr"), new Proc("cdaadr", (Procedure1)cdaadr, 1, 1)),
	       list(symbol("cdaar"), new Proc("cdaar", (Procedure1)cdaar, 1, 1)),
	       list(symbol("cdadar"), new Proc("cdadar", (Procedure1)cdadar, 1, 1)),
	       list(symbol("cdaddr"), new Proc("cdaddr", (Procedure1)cdaddr, 1, 1)),
	       list(symbol("cdadr"), new Proc("cdadr", (Procedure1)cdadr, 1, 1)),
	       list(symbol("cdar"), new Proc("cdar", (Procedure1)cdar, 1, 1)),
	       list(symbol("cddaar"), new Proc("cddaar", (Procedure1)cddaar, 1, 1)),
	       list(symbol("cddadr"), new Proc("cddadr", (Procedure1)cddadr, 1, 1)),
	       list(symbol("cddar"), new Proc("cddar", (Procedure1)cddar, 1, 1)),
	       list(symbol("cdddar"), new Proc("cdddar", (Procedure1)cdddar, 1, 1)),
	       list(symbol("cddddr"), new Proc("cddddr", (Procedure1)cddddr, 1, 1)),
	       list(symbol("cdddr"), new Proc("cdddr", (Procedure1)cdddr, 1, 1)),
	       list(symbol("cddr"), new Proc("cddr", (Procedure1)cddr, 1, 1)),
	       list(symbol("debug"), new Proc("debug", (Procedure1)debug, -1, 1)),
	       list(symbol("equal?"), Equal_proc),
	       list(symbol("eqv?"), Eqv_proc),
	       list(symbol("float"), new Proc("float", (Procedure1)ToDouble, 1, 1)),
	       list(symbol("format"), new Proc("format", (Procedure1)format_list, -1, 1)),
	       list(symbol("get-member"), get_member_proc),
	       list(symbol("globals"), new Proc("globals", (Procedure0)dlr_env_list, 0, 1)),
	       list(symbol("group"), new Proc("group", (Procedure2) group, 2, 1)),
	       list(symbol("int"), new Proc("int", (Procedure1)ToInt, 1, 1)),
	       list(symbol("iter?"), new Proc("iter?", (Procedure1Bool)iter_q, 1, 2)),
	       list(symbol("length"), new Proc("length", (Procedure1)length, 1, 1)),
	       list(symbol("list->vector"), list_to_vector_proc),
	       list(symbol("list-head"), new Proc("list-head", (Procedure2)list_head, 2, 1)),
	       list(symbol("list-tail"), new Proc("list-tail", (Procedure2)list_tail, 2, 1)),
	       list(symbol("list?"), new Proc("list?", (Procedure1Bool)list_q, 1, 2)),
	       list(symbol("member"), new Proc("member", (Procedure2)member, 2, 1)),
	       list(symbol("procedure?"), new Proc("procedure?", (Procedure1Bool)procedure_q_proc, 1, 2)),
	       list(symbol("property"), new Proc("property", (Procedure1)property, -1, 1)),
	       list(symbol("rational"), new Proc("int", (Procedure2)ToRational, 2, 1)),
	       list(symbol("safe-print"), safe_print_proc),
	       list(symbol("sort"), new Proc("sort", (Procedure2)sort, 2, 1)),
	       list(symbol("string->list"), new Proc("string->list", (Procedure1) string_to_list, 1, 1)),
	       list(symbol("string->symbol"), new Proc("string->symbol", (Procedure1) string_to_symbol, 1, 1)),
	       list(symbol("string-append"), string_append_proc),
	       list(symbol("string<?"), new Proc("string<?", (Procedure2Bool) stringLessThan_q, 2, 2)),
	       list(symbol("string=?"), string_is__q_proc),
	       list(symbol("symbol"), new Proc("symbol", (Procedure1)symbol, 1, 1)),
	       list(symbol("symbol->string"), new Proc("symbol->string", (Procedure1) symbol_to_string, 1, 1)),
	       list(symbol("typeof"), new Proc("typeof", (Procedure1)get_type, 1, 1)),
	       list(symbol("vector->list"), new Proc("vector->list", (Procedure1)vector_to_list, 1, 1)),
	       list(symbol("vector-set!"), new Proc("vector-set!", (Procedure3)vector_set_b, 3, 1)),
	       list(symbol("vector?"), new Proc("vector?", (Procedure1Bool)vector_q, 1, 2)),
	       list(symbol("use-lexical-address"), new Proc("use-lexical-address", (Procedure1)PJScheme.use_lexical_address, -1, 1)),
	       list(symbol("use-tracing"), new Proc("use-tracing", (Procedure1)PJScheme.tracing_on, -1, 1)),
	       list(symbol("reset-toplevel-env"), new Proc("reset-toplevel-env", (Procedure0Void)reset_toplevel_env, 0, 0))
			       );
      
      // object settings = list(
      // 			     list(symbol("*use-lexical-address*"), ((object)value => PJScheme.use_lexical_address))
      // 			     );
      // env = PJScheme.extend(env,  map(car_proc, settings), map(cadr_proc, settings));
      return PJScheme.extend(env,  map(car_proc, primitives), map(make_external_proc_proc, map(cadr_proc, primitives)));
  }
  
  public static void reset_toplevel_env() {
      PJScheme.initialize_globals();
  }
    
  public static object make_external_proc(object val) {
      return PJScheme.make_external_proc(val);
  }
    
  public static object debug(object args) {
	if (((int) length(args)) == 0)
	  return config.DEBUG;
	else 
	  config.DEBUG = (int)car(args);
	return config.DEBUG;
  }

  public static object get_type(object obj) {
	// implements "typeof"
	return obj.GetType();
  }

  public static string[] get_parts(String filename, String delimiter) {
	int pos = filename.IndexOf(delimiter);
	string[] parts = null;
	if (pos != -1) {
	  parts = new string[2];
	  parts[0] = filename.Substring(0, pos);
	  parts[1] = filename.Substring(pos + 1, filename.Length - pos - 1);
	} else {
	  parts = new string[1];
	  parts[0] = filename;
	}
	return parts;
  }

  static public Type[] get_arg_types(object objs) {
	int i = 0;
	Type[] retval = new Type[(int)length(objs)];
	object current = objs;
	while (!Eq(current, EmptyList)) {
	  object obj = car(current);
	  if (Equal(obj, "null"))
		retval[i] = Type.GetType("System.Object");
	  else
		retval[i] = obj.GetType();
	  i++;
	  current = cdr(current);
	}
	return retval;
  }

  public static Type get_the_type(String tname) {
	foreach (Assembly assembly in config.assemblies) {
	  Type type = assembly.GetType(tname);
	  if (type != null) {
		return type;
	  }
	}
	return null;
  }

  public static object property (object args) {
	object the_obj = car(args);
	object property_list = cdr(args);
	return call_external_proc(the_obj, property_list, null);
  }

  public static object get_external_member_name(object obj, object name) {
      return get_external_member(obj, name.ToString());
  }

  public static object get_external_member(object obj, string name) {
    //printf("get_external_member: {0}, {1}\n", obj, name);
    Type type = obj.GetType();
    MethodInfo method;
    try {
      method = type.GetMethod(name);
    } catch (System.Reflection.AmbiguousMatchException) {
      // wait till you have more info from args
      return (object)new MethodNeedsArgs(obj, name);
    }
    if (method != null) {
      //printf("GetMethod: {0}\n", method);
      return new Method(obj, method);
    }
    FieldInfo field = type.GetField(name);
    if (field != null) {
      //printf("GetField: {0}\n", field.GetValue(obj));
      return field.GetValue(obj);
    }
    PropertyInfo property = type.GetProperty(name);
    if (property != null) {
      //printf("GetProperty: {0}\n", property);
      return property;
      //return IronPython.Runtime.Types.DynamicHelpers.
      //GetPythonTypeFromType(property.GetType());
    }
    throw new Exception(String.Format("no such member: '{0}'", name));
  }

  public static object call_external_proc(object obj, object path, object args) {
      //string name = obj.ToString();
      //Type type = null;
      //type = get_the_type(name.ToString());
      Type type = obj.GetType();
      if (type != null) {
	  //Type[] types = get_arg_types(args);
	  object[] arguments = list_to_array(args);
	  object result = get_external_member(obj, car(path).ToString());
	  if (!null_q(result)) {
		if (Eq(car(result), symbol("method"))) {
		  string method_name = cadr(result).ToString();
		  MethodInfo method = (MethodInfo) caddr(result);
		  object retval = method.Invoke(method_name, arguments);
		  return retval;
		} else if (Eq(car(result), symbol("field"))) {
		  //string field_name = (string) cadr(result);
		  FieldInfo field = (FieldInfo) caddr(result);
		  try {
			return field.GetValue(null); // null for static
		  } catch {
			return field.GetValue(obj); // use obj for instance
		  }
		} else if (Eq(car(result), symbol("constructor"))) {
		  //string ctor_name = (string) cadr(result);
		  ConstructorInfo constructor = (ConstructorInfo) caddr(result);
		  object retval = constructor.Invoke(arguments);
		  return retval;
		} else if (Eq(car(result), symbol("property"))) {
		  string property_name = cadr(result).ToString();
		  PropertyInfo property = (PropertyInfo) caddr(result);
		  // ParameterInfo[] indexes = property.GetIndexParameters();
		  // to use interface, OR
		  // PropertyType.IsArray; to just get object then access
		  return property.GetValue(property_name, null); // non-indexed
		  //property.GetValue(property_name, index); // indexed
		} else {
		  throw new Exception(String.Format("don't know how to handle '{0}'?", result));
		}
	  }
	  return result;
	}
	throw new Exception(String.Format("no such external type '{0}'", type));
  }

  public static object using_prim(object args, object env) {
      // implements "using"
      Assembly assembly = null;
      if (list_q(args)) {
	  int len = (int) length(args);
	  if (len > 0) { // (using "file.dll"), (using "System")
	      String filename = car(args).ToString();
	      try {
		  // FAILS without trying/catch when DEBUG in MonoDevelop
		  assembly = Assembly.Load(filename);
	      } catch {
#pragma warning disable 612
		      assembly = Assembly.LoadWithPartialName(filename);
#pragma warning restore 612
              }
	      // add assembly to assemblies
	      if (assembly != null) {
		  config.AddAssembly(assembly);
		  if (_dlr_runtime != null) {
		      _dlr_runtime.LoadAssembly(assembly);
		      // then add each type to environment
		      // FIXME: optionally module name
		      foreach (Type type in assembly.GetTypes()) {
			  if (type.IsPublic) {
			      _dlr_env.SetVariable(type.Name, 
						   IronPython.Runtime.Types.DynamicHelpers. 
						   GetPythonTypeFromType(type));
			  }
		      }
		  } else {
		      throw new Exception("DLR Runtime not available");
		  }
	      } else {
		  throw new Exception(String.Format("external library '{0}' could not be loaded", filename));
	      }
	  } else {
	      throw new Exception("using takes a DLL name, and optionally a moduleName");
	  }
      } else {
	  throw new Exception("using takes a DLL name, and optionally a moduleName");
      }
      return PJScheme.symbol("<void>");
  }

  public static object ToDouble(object obj) {
	try {
	  return Convert.ToDouble(obj);
	} catch {
	  if (obj is Rational) {
		return (double)((Rational)obj);
	  } else
		throw new Exception(string.Format("can't convert object of type '{0}' to float", obj.GetType()));
	}
  }

  public static object ToInt(object obj) {
	try {
	  return Convert.ToInt32(obj);
	} catch {
	  if (obj is Rational) {
		return (int)((Rational)obj);
	  } else	  
		throw new Exception(string.Format("can't convert object of type '{0}' to int", obj.GetType()));
	}
  }
  
  public static object ToRational(object obj1, object obj2) {
    return new Rational((int) obj1, (int)obj2);
  }
  
  public static object range(object args) { // range(start stop incr)
	if (list_q(args)) {
	  int len = (int) length(args);
	  if (len == 1) { // (range 100)
	    return make_range(0, car(args), 1); 
	  } else if (len == 2) { // (range start stop)
	    return make_range(car(args), cadr(args), 1); 
	  } else if (len == 3) { // (range start stop incr)
	    return make_range(car(args), cadr(args), caddr(args)); 
	  }
	}
	throw new Exception("improper args to range");
  }

  public static object make_range(object start, object stop, object incr) {
	object retval = EmptyList;
	object tail = EmptyList;
	if (LessThan(start, stop)) {
	  for (object i = start; LessThan(i, stop); i = Add(i, incr)) {
	    if (Eq(tail, EmptyList)) {
	      retval = list(i); // start of list
	      tail = retval;
	    } else { // a pair
	      set_cdr_b(tail, cons(i, EmptyList));
	      tail = cdr(tail);
	    }
	  }
	} else {
	  for (object i = start; GreaterThan(i, stop); i = Add(i, incr)) {
	    if (Eq(tail, EmptyList)) {
	      retval = list(i); // start of list
	      tail = retval;
	    } else { // a pair
	      set_cdr_b(tail, cons(i, EmptyList));
	      tail = cdr(tail);
	    }
	  }
        }
        return retval;
  }

  public static object list_tail(object lyst, object pos) {
	if (null_q(lyst)) {
	  if (EqualSign(pos, 0))
		return EmptyList;
	  else
		throw new Exception("list-tail position beyond list");
	} else if (pair_q(lyst)) {
	  object current = lyst;
	  int current_pos = 0;
	  while (!EqualSign(current_pos, pos)) {
		current = cdr(current);
		current_pos++;
	  }
	  return current;
	}
	throw new Exception("list-tail takes a list and a pos");
  }

  public static object list_head(object lyst, object pos) {
	if (null_q(lyst)) {
	  if (EqualSign(pos, 0))
		return EmptyList;
	  else
		throw new Exception("list-head position beyond list");
	} else if (pair_q(lyst)) {
	  object retval = EmptyList;
	  object current = lyst;
	  object tail = EmptyList;
	  int current_pos = 0;
	  while (!EqualSign(current_pos, pos)) {
		if (Eq(retval, EmptyList)) {
		  retval = cons(car(current), EmptyList);
		  tail = retval;
		} else {
		  set_cdr_b(tail, cons(car(current), EmptyList));
		  tail = cdr(tail);
		}
		current = cdr(current);
		current_pos++;
	  }
	  return retval;
	}
	throw new Exception("list-head takes a list and a pos");
  }

  public static object read_line(object prompt) {
	//return Console.ReadLine();
	//string s;
	//s = lineEditor.Edit(prompt.ToString(), "");
	return "";
  }

  public static object file_exists_q(object path_filename) {
	return File.Exists(path_filename.ToString());
  }

  public static bool stringLessThan_q(object a, object b) {
	// third argument is ignoreCase
	return (String.Compare(a.ToString(), b.ToString(), false) < 0);
  }

  public static object string_length(object str) {
	return str.ToString().Length;
  }

  public static object substring(object str, object start, object stop) {
	return str.ToString().Substring(((int)start), ((int)stop) - ((int)start));
  }

  public static void for_each(object proc, object items) {
	object current1 = items;
	// FIXME: compare on empty list assumes proper list
	// fix to work with improper lists
	while (!Eq(current1, EmptyList)) {
	  apply(proc, list(car(current1)));
	  current1 = cdr(current1);
	}
  }

  public static object apply(object proc, object args) {
	if (proc is Proc)
	  return ((Proc)proc).Call(args);
	else {
	    if (procedure_q(proc) && Eq(cadr(proc), symbol("<extension>"))) {
		return ((Proc)caddr(proc)).Call(args);
	    } else {
		throw new Exception(string.Format("invalid procedure: {0}", proc));
	    }
	}
  }

  public static object apply(object proc, object args1, object args2) {
	if (proc is Proc)
	  return ((Proc)proc).Call(args1, args2);
	else
	  if (procedure_q(proc) && Eq(cadr(proc), symbol("<extension>")))
		return ((Proc)caddr(proc)).Call(args1, args2);
	  else
		throw new Exception(string.Format("invalid procedure: {0}", proc));
  }

  public static void apply_handler () {
	// will be replaced
  }

  public static object map(object proc, object args) {
	object retval = EmptyList;
	object tail = retval;
	object current1 = args;
	while (!Eq(current1, EmptyList)) {
	  object result;
	  if (pair_q(car(current1)))
		result = apply(proc, list(car(current1)));
	  else
		result = apply(proc, car(current1));
	  if (Eq(tail, EmptyList)) {
		retval = list(result); // start of list
		tail = retval;
	  } else { // pair
		set_cdr_b(tail, cons(result, EmptyList));
		tail = cdr(tail);
	  }
	  current1 = cdr(current1);
	}
	return retval;
  }

	public static object map_hat (object f_hat, object asexp)
	{
		// (lambda (f^ asexp)
		//   (cond
		//     ((null?^ asexp) (list atom-tag '() 'none))
		//     (else (cons^ (f^ (car^ asexp)) (map^ f^ (cdr^ asexp)) 'none)))))
		if ((bool)PJScheme.null_q_hat(asexp))
			return list(PJScheme.atom_tag, EmptyList, symbol("none"));
		else
			return PJScheme.cons_hat(apply(f_hat, list(PJScheme.car_hat (asexp))),
			                         map_hat (f_hat, PJScheme.cdr_hat (asexp)),
			                         symbol("none"));
	}
	
	/*
		object retval = EmptyList;
		object tail = retval;
		object current = asexp;
		while (!(bool)PJScheme.null_q_hat(current)) {
			object result;
			if ((bool)PJScheme.list_q_hat (PJScheme.car_hat (current)))
				result = apply (f_hat, list (PJScheme.car_hat (current)));
			else
				result = apply (f_hat, PJScheme.car_hat (current));
			if ((bool)PJScheme.null_q_hat (tail)) {
				retval = list (PJScheme.atom_tag, result, symbol ("none")); // start of list
				tail = retval;
			} else { // pair
				set_cdr_b (tail, PJScheme.cons_hat (result, EmptyList, symbol ("none")));
				tail = PJScheme.cdr_hat (tail);
			}
			current = PJScheme.cdr_hat (current);
		}
		return retval;
	}
	*/

  public static object filter(object proc, object args) {
	object retval = EmptyList;
	object tail = retval;
	object current1 = args;
	while (!Eq(current1, EmptyList)) {
	  object result;
	  if (pair_q(car(current1)))
		result = apply(proc, list(car(current1)));
	  else
		result = apply(proc, car(current1));
	  if (true_q(result)) {
	      if (Eq(tail, EmptyList)) {
		  retval = list(result); // start of list
		  tail = retval;
	      } else { // pair
		  set_cdr_b(tail, cons(result, EmptyList));
		  tail = cdr(tail);
	      }
	  }
	  current1 = cdr(current1);
	}
	return retval;
  }

  public static object map(object proc, object args1, object args2) {
	object retval = EmptyList;
	object tail = EmptyList;
	object current1 = args1;
	object current2 = args2;
	while (!Eq(current1, EmptyList)) {
	  object result = apply(proc, car(current1), car(current2));
	  if (Eq(retval, EmptyList)) {
		retval = cons( result, EmptyList);
		tail = retval;
	  } else {
		set_cdr_b( tail, cons(result, EmptyList));
		tail = cdr(tail);
	  }
	  current1 = cdr(current1);
	  current2 = cdr(current2);
	}
	return retval;
  }
	
	/*
(define-native tagged-list^
  (lambda (keyword op len)
    (lambda (asexp)
      (and (list?^ asexp)
	   (op (length^ asexp) len)
	   (symbol?^ (car^ asexp))
	   (eq?^ (car^ asexp) keyword)))))
	 */
	public static Func<object,bool> tagged_list_hat(object test_string, object pred, object value) {
	return (object asexp) => {
	  return ((bool)pair_q(asexp) &&
		      ((bool)PJScheme.list_q_hat(asexp)) && 
		      ((bool)((Predicate2)pred)(PJScheme.length_hat(asexp), value)) &&
			  ((bool)(PJScheme.symbol_q_hat(PJScheme.car_hat(asexp)))) &&
			  ((bool)(PJScheme.eq_q_hat(PJScheme.car_hat(asexp), string_to_symbol(test_string))))
	         );
		//printf("is this a {0}? ({1} ...) compare {2} => {3}\n", test_string, car(lyst), value, retval);
	};
  }
	
  public static Func<object,bool> tagged_list(object test_string, object pred, object value) {
		return (object lyst) => {
	        	return (pair_q(lyst) &&
		                ((bool)Eq(car(lyst), string_to_symbol(test_string))) && 
                        ((bool)((Predicate2)pred)(length_safe(lyst), value)));
			};
  }
	
  public static Func<object,bool> procedure_q = tagged_list(symbol("procedure"), (Predicate2)GreaterOrEqual, 1);

  public static bool procedure_q_proc(object obj) {
	return (bool) procedure_q(obj);
  }

  public static object vector_to_list(object obj) {
    return ((Vector)obj).ToList();
  }

  public static object list_to_vector(object lyst) {
      if (list_q(lyst)) {
	  int len = (int) length(lyst);
	  object current = lyst;
	  object[] retval = new object[len];
	  for (int i = 0; i < len; i++) {
	      retval[i] = car(current);
	      current = cdr(current);
	  }
	  return new Vector(retval);
      } else {
	  throw new Exception("argument is not a proper list");
      }
  }

    public static object vector_native(object lyst) {
	return list_to_vector(lyst);
    }

  public static object vector(params object [] array) {
	  object lyst = list (array);
      return list_to_vector(lyst);
  }

  public static object make_vector(object size) {
    int len = (int) size;
    object[] retval = new object[len];
    for (int i = 0; i < len; i++) {
      retval[i] = 0;
    }
    return new Vector(retval);
  }

  public static object vector_set_b(object vector, object index, object value) {
    Vector v = (Vector) vector;
    int pos = (int) index;
    v.set(pos, value);
    return vector;
  }

  public static object vector_ref(object vector, object index) {
    Vector v = (Vector) vector;
    int pos = (int) index;
    return v.get(pos);
  }

  public static object vector_length(object vector) {
    Vector v = (Vector) vector;
    return v.length();
  }

  public static object [] list_to_array(object lyst) {
		return list_to_array(lyst, 0);
  }
	
  public static object [] list_to_array(object lyst, int start) {
	int len = (int) length(lyst) - start;
	object current = lyst;
	int j = 0;
	while (j < start) {
		current = cdr(current);
		j++;
	}
	object[] retval = new object[len];
	for (int i = 0; i < len; i++) {
	  retval[i] = car(current);
	  current = cdr(current);
	}
	return retval;
  }

  public static object string_ref(object args) {
      object s = car(args);
      object i = cadr(args);
      return s.ToString()[(int)i];
  }

  public static object string_ref(object s, object i) {
	return s.ToString()[(int)i];
  }

  public static object make_string(object obj) {
	if (obj == null || obj == (object) NULL) {
	  return (object) "\0";
	}
	return obj.ToString();
  }

  public static bool number_q(object datum) {
	return ((datum is int) ||
		(datum is double) ||
		(datum is Rational) ||
		(datum is BigInteger));
  }

  public static bool boolean_q(object datum) {
	return (datum is bool);
  }

  public static bool char_q(object datum) {
	return (datum is char);
  }

    public static bool char_eq_q(object c1, object c2) {
      return Equals(c1, c2);
  }

  public static bool string_q(object datum) {
	return (datum is string);
  }

  public static bool vector_q(object obj) {
	return (obj is Vector);
  }

  public static bool char_numeric_q(object c) {
	if (c == null) return false;
	return (('0' <= ((char)c)) && (((char)c) <= '9'));
  }

  public static bool symbol_q(object x) {
	return (x is Symbol && x != EmptyList);
  }

  public static bool char_alphabetic_q(object o) {
	if (o is char) {
	  char c = (char) o;
	  return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
	} 
	return false;
  }

  public static bool char_whitespace_q(object o) {
	if (o is char) {
	  char c = (char) o;
	  return (c == ' ' || c == '\t' || c == '\n' || c == '\r');
	}
	return false;
  }

  public static bool char_is__q(object c1, object c2) {
	return ((c1 is char) && (c2 is char) && ((char)c1) == ((char)c2));
	
  }

  public static bool string_is__q(object o1, object o2) {
	return ((o1 is string) && (o2 is string) && ((string)o1) == ((string)o2));
  }
  
  public static object string_to_list(object str) {
	object retval = EmptyList;
	object tail = EmptyList;
	if (str != null) {
	  string sstr = str.ToString();
	  for (int i = 0; i < sstr.Length; i++) {
		if (Eq(retval, EmptyList)) {
		  retval = cons(sstr[i], EmptyList);
		  tail = retval;
		} else {
		  set_cdr_b(tail, cons(sstr[i], EmptyList));
		  tail = cdr(tail);
		}
	  }
	}
	return retval;
  }

  public static object string_to_symbol(object s) {
	return symbol(s.ToString());
  }

  public static object string_to_number(object s) {
      string ss = s.ToString();
      if (ss.Contains("."))
	  return string_to_decimal(ss);
      else if (ss.Contains("/"))
	  return string_to_rational(ss);
      else
	  return string_to_integer(ss);
  }

  public static object string_to_integer(object str) {
	try {
	  return int.Parse(str.ToString());
	} catch (OverflowException) {
	  return BigIntegerParse(str.ToString());
	}
  }

  public static object string_to_decimal(object str) {
	return double.Parse(str.ToString());
  }

  public static object string_to_rational(object str) {
	string[] part = (str.ToString()).Split(SPLITSLASH);
	Rational retval = new Rational(int.Parse(part[0]), int.Parse(part[1]));
	if (retval.denominator == 1)
	  return retval.numerator;
	else
	  return retval;
  }

  public static void error(object code, object msg, params object[] rest) {
	config.NEED_NEWLINE = false;
	Console.Error.WriteLine("Error in {0}: {1}", (code.ToString()), format(msg, rest));
  }

  public static void newline() {
	config.NEED_NEWLINE = false;
	Console.WriteLine("");
  }

  public static void display(object obj) {
	// FIXME: pass stdout port setting
	display(obj, null);
  }

  public static void display(object obj, object port) {
	// FIXME: add output port type
	string s = ToString(obj);
	int len = s.Length;
	if (s.Substring(len - 1, 1) != "\n") 
	  config.NEED_NEWLINE = true;
	Console.Write(s);
  }

  public static object dlr_exp_q(object rator) {
      //Console.WriteLine("dlr-exp? {0}");
      //return (! pair_q(rator));
      return ((rator is IronPython.Runtime.Types.BuiltinFunction) ||
	      (rator is IronPython.Runtime.PythonFunction) ||
	      (rator is MethodNeedsArgs) ||
	      (rator is Method) ||
	      (rator is IronPython.Runtime.Types.PythonType) ||
	      (rator is Closure));
  }
  
  public static Type[] get_types(object [] objects) {
    Type [] retval = new Type[objects.Length];
    int count = 0;
    foreach (object obj in objects) {
      retval[count] = obj.GetType();
      count++;
    }
    return retval;
  }

  public static object dlr_apply(object proc, object args) {
	//printf("dlr_apply({0}, {1})\n", proc, args);
	object retval;
	if (proc is Method) {
	  retval = ((Method)proc).method.Invoke(((Method)proc).classobj, 
		  list_to_array(args));
  	    return retval != null ? retval : symbol("<void>");
	} else if (proc is MethodNeedsArgs) {
	  // get the method based on args
	  Type type = ((MethodNeedsArgs)proc).classobj.GetType();
	  MethodInfo method;
	  method = type.GetMethod(((MethodNeedsArgs)proc).name, 
				  get_types(list_to_array(args)));
	  // then invoke
	  retval = method.Invoke(((MethodNeedsArgs)proc).classobj, 
			       list_to_array(args));
  	    return retval != null ? retval : symbol("<void>");
	} else if (proc is Closure) {
	    return (proc as Closure)(list_to_array(args));
	} else {
	    if (_dlr_runtime != null) {
		retval = _dlr_runtime.Operations.Invoke(proc, list_to_array(args));
		return retval != null ? retval : symbol("<void>");
	    } else {
		throw new Exception(String.Format("DLR Runtime not available"));
	    }
	}
  }

  public static bool dlr_env_contains(object variable) {
	bool retval = true;
	if (_dlr_env != null) {
	    try {
		_dlr_env.GetVariable(variable.ToString());
	    } catch {
		retval = false;
	    }
	} else {
	    retval = false;
	}
	return retval;
  }

  public static object dlr_env_lookup(object variable) {
	object retval = null;
	if (_dlr_env != null) 
	    retval = _dlr_env.GetVariable(variable.ToString());
	else
	    throw new Exception(String.Format("DLR Environment not available"));
	return PJScheme.make_binding("dlr", retval);
  }

  public static object dlr_env_list() {
    object retval = list();
    try {
	  if (_dlr_env != null) {
		foreach (string variable in _dlr_env.GetVariableNames()) {
		  retval = new Cons(variable, retval);
		}
	  } else {
		throw new Exception(String.Format("DLR Environment not available"));
	  }
    } catch {
    }
    return retval;
  }

  public static bool dlr_object_q(object result) {
	// FIXME: can we see if it is a dlr object?
    //printf("dlr_object_q: {0}\n", result);
    return true;
    //return (result is IronPython.Runtime.Types.PythonType);
  }

  public static object dlr_lookup_components(object result, object parts_list) {
      //printf("dlr_lookup_components: {0}, {1}\n", result, parts_list);
      object retobj = result;
      while (pair_q(parts_list)) {
	  //printf("...loop: {0}\n", parts_list);
	  // fixme: needs to use args to get method
	  try{
	      if (_dlr_runtime != null) {
		  retobj = _dlr_runtime.Operations.GetMember(retobj, 
							     car(parts_list).ToString());
	      } else {
		  throw new Exception(String.Format("DLR Runtime not available"));
	      }
	  } catch {
	      object binding = PJScheme.make_binding("dlr", get_external_member(result, car(parts_list).ToString()));
	      return binding;
	  }
	  parts_list = cdr(parts_list);
      }
      return PJScheme.make_binding("dlr", retobj);
  }

  public static object printf_prim(object args) {
	int len = ((int) length(args)) - 1;
	// FIXME: surely there is a better way?:
	if (len == 0)
	  Console.Write(car(args));
	else if (len == 1)
	  Console.Write(format(car(args), cadr(args)));
	else if (len == 2)
	  Console.Write(format(car(args), cadr(args), caddr(args)));
	else if (len == 3)
	  Console.Write(format(car(args), cadr(args), caddr(args), cadddr(args)));
	else if (len == 4)
	  Console.Write(format(car(args), cadr(args), caddr(args), cadddr(args), 
			  cadddr(cdr(args))));
	else if (len == 5)
	  Console.Write(format(car(args), cadr(args), caddr(args), cadddr(args), 
			  cadddr(cdr(args)), cadddr(cdr(cdr(args)))));
	else if (len == 6)
	    Console.Write(format(car(args), cadr(args), caddr(args), cadddr(args), 
				 cadddr(cdr(args)), cadddr(cdr(cdr(args))), cadddr(cdr(cdr(cdr(args))))));
	return null;
  }
  
  public static void printf(object fmt, params object[] objs) {
	Console.Write(format(fmt, objs));
  }

  public static string ToString(object obj) {
	if (obj == null) {
	  return "<void>"; // FIXME: should give void when forced
	} else if (obj is System.Boolean) {
	  return ((bool)obj) ? "#t" : "#f";
	} else if (obj is IronPython.Runtime.List) {
	  return ((IronPython.Runtime.List)obj).__repr__(
		  IronPython.Runtime.DefaultContext.Default);
	} else if (obj is IronPython.Runtime.PythonDictionary) {
	  return ((IronPython.Runtime.PythonDictionary)obj).__repr__(
		  IronPython.Runtime.DefaultContext.Default);
	} else if (obj is IronPython.Runtime.PythonTuple) {
	  return obj.ToString();
	} else if (obj is Array) {
	    //System.Console.WriteLine("Here 1");
	  return (string)array_to_string((object[]) obj);
	} else if (obj is double) {
	  string s = obj.ToString();
	  if (s.Contains("."))
		return s;
	  else
		return String.Format("{0}.0", s);
	} else if (obj is String) {
	  return String.Format("{0}", obj);
	} else if (obj is Symbol) {
	    //System.Console.WriteLine("Here 2");
	  return obj.ToString();
	} else if (pair_q(obj)) {
	    //System.Console.WriteLine("Here 3");
	  if (procedure_q(obj)) {
		return "#<procedure>";
	  } else if (Eq(car(obj), symbol("environment"))) {
		return "#<environment>"; //, car(obj));
	  } else {
	      //System.Console.WriteLine("Here 4");
	      string retval = "";
	      object current = (Cons)obj;
	      Dictionary<int,bool> ids = new Dictionary<int,bool>();
	      ids[((Cons)current).id] = true;
	      while (pair_q(current)) {
		  //System.Console.WriteLine("Here 5");
		  if (retval != "") {
		      retval += " ";
		  } 
		  object car_current = car(current);
		  if (pair_q(car_current) && ids.ContainsKey(((Cons)car_current).id)) {
		      retval += " ...";
		      current = null;
		  } else {
		      retval += ToString(car_current);
		      current = cdr(current);
		      if (pair_q(current) && ids.ContainsKey(((Cons)current).id)) {
			  retval += " ...";
			  current = null;
		      } else {
			  if (!pair_q(current) && !Eq(current, EmptyList)) {
			      retval += " . " + ToString(current); // ...
			  }
		      }
		  }
	      }
	      return "(" + retval + ")";
	  }
	} else {
	    return obj.ToString();
	}
  }

  public static string repr(object obj) {
	if (obj == null) {
	  return "<void>"; // FIXME: should give void when forced
	} else if (obj is System.Boolean) {
	  return ((bool)obj) ? "#t" : "#f";
	} else if (obj is IronPython.Runtime.List) {
	  return ((IronPython.Runtime.List)obj).__repr__(
		  IronPython.Runtime.DefaultContext.Default);
	} else if (obj is IronPython.Runtime.PythonDictionary) {
	  return ((IronPython.Runtime.PythonDictionary)obj).__repr__(
		  IronPython.Runtime.DefaultContext.Default);
	} else if (obj is IronPython.Runtime.PythonTuple) {
	  return obj.ToString();
	} else if (obj is Array) {
	    //System.Console.WriteLine("Here 1");
	  return (string)array_to_string((object[]) obj);
	} else if (obj is double) {
	  string s = obj.ToString();
	  if (s.Contains("."))
		return s;
	  else
		return String.Format("{0}.0", s);
	} else if (obj is String) {
	  return String.Format("\"{0}\"", obj);
	} else if (obj is Symbol) {
	    //System.Console.WriteLine("Here 2");
	  return obj.ToString();
	} else if (pair_q(obj)) {
	    //System.Console.WriteLine("Here 3");
	  if (procedure_q(obj)) {
		return "#<procedure>";
	  } else if (Eq(car(obj), symbol("environment"))) {
		return "#<environment>"; //, car(obj));
	  } else {
	      //System.Console.WriteLine("Here 4");
	      string retval = "";
	      object current = (Cons)obj;
	      Dictionary<int,bool> ids = new Dictionary<int,bool>();
	      ids[((Cons)current).id] = true;
	      while (pair_q(current)) {
		  //System.Console.WriteLine("Here 5");
		  if (retval != "") {
		      retval += " ";
		  } 
		  object car_current = car(current);
		  if (pair_q(car_current) && ids.ContainsKey(((Cons)car_current).id)) {
		      retval += " ...";
		      current = null;
		  } else {
		      retval += repr(car_current);
		      current = cdr(current);
		      if (pair_q(current) && ids.ContainsKey(((Cons)current).id)) {
			  retval += " ...";
			  current = null;
		      } else {
			  if (!pair_q(current) && !Eq(current, EmptyList)) {
			      retval += " . " + repr(current); // ...
			  }
		      }
		  }
	      }
	      return "(" + retval + ")";
	  }
	} else {
	    return obj.ToString();
	}
  }

  public static string format_list(object args) {
	if (pair_q(args)) {
	  int len = (int)length(args);
	  if (len == 1)
		return format(car(args), new object[0]);
	  else if (len > 1) {
		object[] options = list_to_array(cdr(args));
		return format(car(args), options);
	  }
	}
	throw new Exception("invalid args to format");
  }

  public static MethodInfo get_method(object obj, string method_name) {
	Type t = obj.GetType();
    MethodInfo[] mi = t.GetMethods();
    foreach(MethodInfo m in mi) {
	  if (m.Name == method_name) {
		return m;
	  }
	}
	return null;
  }

  public static string format(object msg, params object[] rest) {
	string retval = "";
	string new_msg = "";
	string smsg = msg.ToString();
	int count = 0;
	for (int i = 0; i < smsg.Length; i++) {
	    if (smsg[i] == TILDE) {
		if (smsg[i+1] == 's') {
		    new_msg += string.Format("{0}", ToString(rest[count]));
		    count += 1;
		    i++;
		} else if (smsg[i+1] == 'a') {
		    new_msg += string.Format("{0}", ToString(rest[count]));
		    count += 1;
		    i++;
		} else if (smsg[i+1] == '%') {
		    new_msg += "\n";
		    i++;
		} else
		    throw new Exception(string.Format("format needs to handle: \"{0}\"", 
						      smsg));
	    } else {
		new_msg += smsg[i];
	    }
	}
	retval = String.Format(new_msg, rest);
	return retval;
  }

    public static bool string_eq_q(object args) {
	return (car(args).ToString() == (cadr(args).ToString()));
    }

  public static bool GreaterOrEqual(object args) {
	return GreaterOrEqual(car(args), cadr(args));
  }

  public static bool GreaterOrEqual(object obj1, object obj2) {
	if (obj1 is Symbol) {
	  // 	  if (obj2 is Symbol) {
	  // 		return (((Symbol)obj1) >= ((Symbol)obj2));
	  // 	  } else 
	  return false;
	} else {
	  try {
		return (ObjectType.ObjTst(obj1, obj2, false) >= 0);
	  } catch {
		return false;
	  }
	}
  }

  public static int cmp(object obj1, object obj2) {
	if (obj1 is Symbol) {
	  if (obj2 is Symbol) {
		return cmp(obj1.ToString(), obj2.ToString());
	  } else 
		return cmp(obj1.ToString(), obj2);
	} else {
	  if (obj2 is Symbol) {
		return cmp(obj1, obj2.ToString());
	  } else {
		return ObjectType.ObjTst(obj1, obj2, false);
	  }
	}
  }

  public static bool Equal(object obj) {
	object item = car(obj);
	object current = cdr(obj);
	while (!Eq(current, EmptyList)) {
	  if (! Equal(item, car(current)))
		return false;
	  current = cdr(current);
	}
	return true;
  }

  public static bool EqualSign(object obj) {
	object item = car(obj);
	object current = cdr(obj);
    return EqualSign(item, car(current));
  }

  public static bool Eq(object obj) {
	object item = car(obj);
	object current = cadr(obj);
    return Eq(item, current);
  }

  public static bool Eqv(object obj) {
	object item = car(obj);
	object current = cadr(obj);
    return Eqv(item, current);
  }

  public static bool Eq(object obj1, object obj2) {
      if ((obj1 is Symbol) || (obj2 is Symbol)) { 
	  if ((obj1 is Symbol) && (obj2 is Symbol))
	      return ((Symbol)obj1).Equals(obj2);
	  else return false;
      } else if (pair_q(obj1) && pair_q(obj2)) {
	  if (null_q(obj1) && null_q(obj2))
	      return true;
	  else {
	      return (((Cons)obj1).id == ((Cons)obj2).id);
	  }
      } if (pair_q(obj1) || pair_q(obj2)) {
	  return false;
      } else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
	      try {
		  return (ObjectType.ObjTst(obj1, obj2, false) == 0);
	      } catch {
		  return false;
	      }
	  } else {
	      if (obj1 is BigInteger) {
		  return ((BigInteger)obj1).Equals(obj2);
	      } else {
		  return ((BigInteger)obj2).Equals(obj1);
	      }
	  }
      }
  }

  public static bool Eqv(object obj1, object obj2) {
      if ((obj1 is Symbol) || (obj2 is Symbol)) { 
	  if ((obj1 is Symbol) && (obj2 is Symbol))
	      return ((Symbol)obj1).Equals(obj2);
	  else return false;
      } else if (pair_q(obj1) && pair_q(obj2)) {
	  if (null_q(obj1) && null_q(obj2))
	      return true;
	  else {
	      return (((Cons)obj1).id == ((Cons)obj2).id);
	  }
      } if (pair_q(obj1) || pair_q(obj2)) {
	  return false;
      } else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
	      try {
		  return (ObjectType.ObjTst(obj1, obj2, false) == 0);
	      } catch {
		  return false;
	      }
	  } else {
	      if (obj1 is BigInteger) {
		  return ((BigInteger)obj1).Equals(obj2);
	      } else {
		  return ((BigInteger)obj2).Equals(obj1);
	      }
	  }
      }
  }

  public static bool Equal(object obj1, object obj2) {
	if ((obj1 is Symbol) || (obj2 is Symbol)) { 
	  if ((obj1 is Symbol) && (obj2 is Symbol))
		return ((Symbol)obj1).Equals(obj2);
	  else return false;
	} else if (pair_q(obj1) && pair_q(obj2)) {
	  if (null_q(obj1) && null_q(obj2))
		return true;
	  else if (null_q(obj1))
		return false;
	  else if (null_q(obj2))
		return false;
	  else if (Equal(car(obj1),  car(obj2)))
		return Equal(cdr(obj1), cdr(obj2));
	  else
		return false;
    } if (pair_q(obj1) || pair_q(obj2)) {
			return false;
	} else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
        try {
          return (ObjectType.ObjTst(obj1, obj2, false) == 0);
        } catch {
          return false;
        }
      } else {
        if (obj1 is BigInteger) {
          return ((BigInteger)obj1).Equals(obj2);
        } else {
          return ((BigInteger)obj2).Equals(obj1);
        }
      }
	}
  }

  public static bool EqualSign(object obj1, object obj2) {
 	if ((obj1 is Symbol) && (obj2 is Symbol)) {
 	  return (((Symbol)obj1).Equals(obj2));
	} else if ((obj1 is int) && (obj2 is int)) {
	  return ((int)obj1) == ((int)obj2);
	} else if ((obj1 is double) && (obj2 is double)) {
	  return ((double)obj1) == ((double)obj2);
	} else if (obj1 is Rational) {
      if (obj2 is Rational) {
        return (((Rational)obj1).Equals((Rational)obj2));
      } else if (obj2 is int) {
        return (((double)((Rational)obj1)) == ((int)obj2));
      } else if (obj2 is double) {
        return (((double)((Rational)obj1)) == ((double)obj2));
      } else {
        throw new Exception("can't compare rational with this object");
      }
    } else if (obj2 is Rational) {
      if (obj1 is Rational) {
        return (((Rational)obj2).Equals((Rational)obj1));
      } else if (obj1 is int) {
        return (((double)((Rational)obj2)) == ((int)obj1));
      } else if (obj1 is double) {
        return (((double)((Rational)obj2)) == ((double)obj1));
      } else {
        throw new Exception("can't compare rational with this object");
      }
    } else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
        try {
          return (ObjectType.ObjTst(obj1, obj2, false) == 0);
        } catch {
          return false;
        }
      } else {
        if (obj1 is BigInteger) {
          return ((BigInteger)obj1).Equals(obj2);
        } else {
          return ((BigInteger)obj2).Equals(obj1);
        }
      }
	}
  }

  public static bool LessThan(object obj) {
	return LessThan(car(obj), cadr(obj));
  }

  public static bool LessThan(object obj1, object obj2) {
	if (obj1 is Rational) {
	  if (obj2 is Rational) {
		return (((Rational)obj1) < ((Rational)obj2));
	  } else if (obj2 is int) {
		return (((Rational)obj1) < ((int)obj2));
	  } else if (obj2 is double) {
		return (((double)((Rational)obj1)) < ((double)obj2));
	  }
	} else if (obj2 is Rational) {
	  if (obj1 is Rational) {
		return (((Rational)obj1) < ((Rational)obj2));
	  } else if (obj1 is int) {
		return (((Rational)obj2) < ((int)obj1));
	  } else if (obj1 is double) {
		return (((double)((Rational)obj2)) < ((double)obj1));
	  }
	} else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
		try {
		  return (ObjectType.ObjTst(obj1, obj2, false) < 0);
		} catch {
		  // ignore and continue
		}
	  }
	  BigInteger b1 = null;
	  BigInteger b2 = null;
	  if (obj1 is int) {
		b1 = makeBigInteger((int) obj1);
	  } else if (obj1 is BigInteger) {
		b1 = (BigInteger)obj1;
	  } else
		throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
	  if (obj2 is int) {
		b2 = makeBigInteger((int) obj2);
	  } else if (obj2 is BigInteger) {
		b2 = (BigInteger)obj2;
	  } else
		throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
	  return b1 < b2;
	}
	throw new Exception(String.Format("unable to compare {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  public static bool LessThanOrEqual(object obj) {
	return (LessThan(car(obj), cadr(obj)) || Equal(car(obj), cadr(obj)));
  }

  public static bool GreaterThanOrEqual(object obj) {
	return (GreaterThan(car(obj), cadr(obj)) || Equal(car(obj), cadr(obj)));
  }

  public static bool GreaterThan(object args) {
	return GreaterThan(car(args), cadr(args));
  }

  public static bool GreaterThan(object obj1, object obj2) {
	if (obj1 is Rational) {
	  if (obj2 is Rational) {
		return (((Rational)obj1) > ((Rational)obj2));
	  } else if (obj2 is int) {
		return (((Rational)obj1) > ((int)obj2));
	  } else if (obj2 is double) {
		return (((double)((Rational)obj1)) > ((double)obj2));
	  }
	} else if (obj2 is Rational) {
	  if (obj1 is Rational) {
		return (((Rational)obj1) > ((Rational)obj2));
	  } else if (obj1 is int) {
		return (((Rational)obj2) > ((int)obj1));
	  } else if (obj1 is double) {
		return (((double)((Rational)obj2)) > ((double)obj1));
	  }
	} else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
        try {
          return (ObjectType.ObjTst(obj1, obj2, false) > 0);
        } catch {
          // continue
        }
      }
      BigInteger b1 = null;
      BigInteger b2 = null;
      if (obj1 is int) {
        b1 = makeBigInteger((int) obj1);
      } else if (obj1 is BigInteger) {
        b1 = (BigInteger)obj1;
      } else
        throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
      if (obj2 is int) {
        b2 = makeBigInteger((int) obj2);
      } else if (obj2 is BigInteger) {
        b2 = (BigInteger)obj2;
      } else
        throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
      return b1 > b2;
	}
	throw new Exception(String.Format("unable to compare {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  public static object not(object obj) {
	return (! true_q(obj));
  }

  public static object Add(object obj) {
	// For adding 0 or more numbers in list
	object retval = 0;
	object current = obj;
	while (!Eq(current, EmptyList)) {
	  retval = Add(retval, car(current));
	  current = cdr(current);
	}
	return retval;
  }

  public static object Multiply(object obj) {
	// For multiplying 0 or more numbers in list
	object retval = 1;
	object current = obj;
	while (!Eq(current, EmptyList)) {
	  retval = Multiply(retval, car(current));
	  current = cdr(current);
	}
	return retval;
  }

    public static object modulo(object obj1, object obj2) {
	return (ObjectType.Modulo(obj1, obj2));
    }

  public static object Subtract(object obj) {
	// For subtracting 1 or more numbers in list
	object retval = car(obj);
	object current = cdr(obj);
	if (((int)length(current)) == 0) {
	  retval = Multiply(-1, retval);
	} else {
	  while (!Eq(current, EmptyList)) {
		retval = Subtract(retval, car(current));
		current = cdr(current);
	  }
	}
	return retval;
  }
	
  public static object Divide(object obj) {
	// For dividing 1 or more numbers in list
	object retval = car(obj);
	object current = cdr(obj);
	if (((int)length(current)) == 0) {
	  retval = Divide(1, retval);
	} else {
	  while (!Eq(current, EmptyList)) {
		retval = Divide(retval, car(current));
		current = cdr(current);
	  }
	}
	return retval;
  }
	
  public static object Add(object obj1, object obj2) {
	if (obj1 is Rational) {
	  if (obj2 is Rational) {
		return (((Rational)obj1) + ((Rational)obj2));
	  } else if (obj2 is int) {
		return (((Rational)obj1) + ((int)obj2));
	  } else if (obj2 is double) {
		return (((double)((Rational)obj1)) + ((double)obj2));
	  }
	} else if (obj2 is Rational) {
	  if (obj1 is Rational) {
		return (((Rational)obj1) + ((Rational)obj2));
	  } else if (obj1 is int) {
		return (((Rational)obj2) + ((int)obj1));
	  } else if (obj1 is double) {
		return (((double)((Rational)obj2)) + ((double)obj1));
	  }
	} else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
		try {
		  return (ObjectType.AddObj(obj1, obj2));
		} catch {
		  // pass
		}
	  }
	  BigInteger b1 = null;
	  BigInteger b2 = null;
	  if (obj1 is int) {
		b1 = makeBigInteger((int) obj1);
	  } else if (obj1 is BigInteger) {
		b1 = (BigInteger)obj1;
	  } else
		throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
	  if (obj2 is int) {
		b2 = makeBigInteger((int) obj2);
	  } else if (obj2 is BigInteger) {
		b2 = (BigInteger)obj2;
	  } else
		throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
	  return b1 + b2;
	}
	throw new Exception(String.Format("unable to add {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  public static object Subtract(object obj1, object obj2) {
	if (obj1 is Rational) {
	  if (obj2 is Rational) {
		return (((Rational)obj1) - ((Rational)obj2));
	  } else if (obj2 is int) {
		return (((Rational)obj1) - ((int)obj2));
	  } else if (obj2 is double) {
		return (((double)((Rational)obj1)) - ((double)obj2));
	  }
	} else if (obj2 is Rational) {
	  if (obj1 is Rational) {
		return (((Rational)obj1) - ((Rational)obj2));
	  } else if (obj1 is int) {
		return (((int)obj1) - ((Rational)obj2));
	  } else if (obj1 is double) {
		return (((double)((double)obj1)) - ((Rational)obj2));
	  }
	} else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
        try {
          return (ObjectType.SubObj(obj1, obj2));
        } catch {
          // ignore and continue
        }
      }
      BigInteger b1 = null;
      BigInteger b2 = null;
      if (obj1 is int) {
        b1 = makeBigInteger((int) obj1);
      } else if (obj1 is BigInteger) {
        b1 = (BigInteger)obj1;
      } else
        throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
      if (obj2 is int) {
        b2 = makeBigInteger((int) obj2);
      } else if (obj2 is BigInteger) {
        b2 = (BigInteger)obj2;
      } else
        throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
      return b1 - b2;
	}
	throw new Exception(String.Format("unable to subtract {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  public static object Multiply(object obj1, object obj2) {
	// FIXME: need hierarchy of numbers, handle rational/complex/etc
	if (obj1 is int && ((int)obj1) == 1) return obj2;
	if (obj2 is int && ((int)obj2) == 1) return obj1;
	if (obj1 is Rational) {
	  if (obj2 is Rational) {
		return (((Rational)obj1) * ((Rational)obj2));
	  } else if (obj2 is int) {
		return (((Rational)obj1) * ((int)obj2));
	  } else if (obj2 is double) {
		return (((double)((Rational)obj1)) * ((double)obj2));
	  }
	} else if (obj2 is Rational) {
	  if (obj1 is Rational) {
		return (((Rational)obj1) * ((Rational)obj2));
	  } else if (obj1 is int) {
		return (((Rational)obj2) * ((int)obj1));
	  } else if (obj1 is double) {
		return (((double)((Rational)obj2)) * ((double)obj1));
	  }
	} else {
	  if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
        try {
          return ObjectType.MulObj(obj1, obj2);
        } catch {
          // continue
        }
      }
      BigInteger b1 = null;
      BigInteger b2 = null;
      if (obj1 is int) {
        b1 = makeBigInteger((int) obj1);
      } else if (obj1 is BigInteger) {
        b1 = (BigInteger)obj1;
      } else
        throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
      if (obj2 is int) {
        b2 = makeBigInteger((int) obj2);
      } else if (obj2 is BigInteger) {
        b2 = (BigInteger)obj2;
      } else
        throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
      return b1 * b2;
	}
	throw new Exception(String.Format("unable to multiply {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  public static object Divide(object obj1, object obj2) {
    Rational rat;
	if ((obj1 is int) && (obj2 is int)) {
	  rat = new Rational((int)obj1, (int)obj2);
      if (rat.denominator == 1)
        return rat.numerator;
      else
        return rat;
	} else {
	  if (obj1 is Rational) {
		if (obj2 is Rational) {
		  rat = (((Rational)obj1) / ((Rational)obj2));
          if (rat.denominator == 1)
            return rat.numerator;
          else
            return rat;
		} else if (obj2 is int) {
		  rat = (((Rational)obj1) / ((int)obj2));
          if (rat.denominator == 1)
            return rat.numerator;
          else
            return rat;
		} else if (obj2 is double) {
		  return (((double)((Rational)obj1)) / ((double)obj2));
		}
	  } else if (obj2 is Rational) {
		if (obj1 is Rational) {
		  rat = (((Rational)obj1) / ((Rational)obj2));
          if (rat.denominator == 1)
            return rat.numerator;
          else
            return rat;
		} else if (obj1 is int) {
		  rat = (((int)obj1) / ((Rational)obj2));
          if (rat.denominator == 1)
            return rat.numerator;
          else
            return rat;
		} else if (obj1 is double) {
		  return (((double)obj1) / ((Rational)obj2));
		}
	  } else {
        if (! ((obj1 is BigInteger) || (obj2 is BigInteger))) {
          try {
            return (ObjectType.DivObj(obj1, obj2));
          } catch {
            // ignore and continue
          }
        }
        BigInteger b1 = null;
        BigInteger b2 = null;
        if (obj1 is int) {
          b1 = makeBigInteger((int) obj1);
        } else if (obj1 is BigInteger) {
          b1 = (BigInteger)obj1;
        } else
          throw new Exception(string.Format("can't convert {0} to bigint", obj1.GetType()));
        if (obj2 is int) {
          b2 = makeBigInteger((int) obj2);
        } else if (obj2 is BigInteger) {
          b2 = (BigInteger)obj2;
        } else
          throw new Exception(string.Format("can't convert {0} to bigint", obj2.GetType()));
        return b1 / b2;
	  }
	}
	throw new Exception(String.Format("unable to divide {0} and {1}", 
			obj1.GetType().ToString(), obj2.GetType().ToString()));
  }

  // List functions -----------------------------------------------

  public static object member(object obj1, object obj2) {
	if (null_q(obj2)) {
	  return false;
	} else if (pair_q(obj2)) {
	  object current = obj2;
	  while (pair_q(current)) {
		if (Equal(obj1, car(current)))
		  return current;
		current = cdr(current);
	  }
	  return false;
	}
	throw new Exception("member takes an object and a list");
  }

  public static object array_ref(object array, object pos) {
	return ((object [])array)[(int) pos];
  }

    // Add new low-level procedures here!

    public static object number_to_string(object number) {
	return ((number != null) ? number.ToString() : "");
    }

    public static object char_to_string(object number) {
	return ((number != null) ? number.ToString() : "");
    }

  public static object list_ref(object obj, object pos) {
	//printf("calling list-ref({0}, {1})\n", obj, pos);
	if (pair_q(obj)) {
	  Cons result = ((Cons)obj);
	  for (int i = 0; i < ((int)pos); i++) {
		try {
		  result = (Cons) cdr(result);
		} catch {
		  throw new Exception(string.Format("error in list_ref: improper access (list_ref {0} {1})",
				  obj, pos));
		}
	  }
	  return (object)car(result);
	} else
	  throw new Exception(string.Format("list_ref: object is not a list: list-ref({0},{1})",
			  obj, pos));
  }

  public static bool null_q(object o1) {
	return ((o1 is Symbol) && (((Symbol)o1) == EmptyList));
  }

  public static bool pair_q(object x) {
    return (x is Cons);
  }

  public static bool iter_q(object x) {
    return (pair_q(x) || vector_q(x) || string_q(x));
  }

  public static object length(object obj) {
	if (null_q(obj)) {
	  return 0;
	} else if (pair_q(obj)) {
	  int len = 0;
	  object current = (Cons)obj;
	  while (pair_q(current)) {
		len++;
		current = cdr(current);
	  }
	  if (Eq(current, EmptyList)) {
		return len;
	  } else {
		throw new Exception(
            String.Format("attempt to take length of an improper list: {0}", obj));
	  }
	} else if (string_q(obj)) {
	  return obj.ToString().Length;
	} else if (vector_q(obj)) {
	  return ((Vector)obj).length();
	} else
	  throw new Exception(
          String.Format("attempt to take length of a non-iterator: {0}", obj));
  }
  
  public static object length_safe(object obj) {
	if (null_q(obj)) {
	  return 0;
	} else if (pair_q(obj)) {
	  int len = 0;
	  object current = (Cons)obj;
	  while (pair_q(current)) {
		len++;
		current = cdr(current);
	  }
	  return len;
	}
	return -1;
  }

  public static IEnumerator get_iterator(object obj) {
    if (obj is IEnumerable)
      return ((IEnumerable)obj).GetEnumerator();
    else
      throw new Exception("not an enumerable");
  }

  public static object next_item(object iterator) {
    object retval = EmptyList;
    if (((IEnumerator)iterator).MoveNext()) {
      retval = ((IEnumerator)iterator).Current;
    }
    return retval;
  }

  public static bool iterator_q(object obj) {
    //System.Console.WriteLine("iterator_q: {0} {1}", obj.ToString(), (! list_q(obj)));
    return (! list_q(obj));
  }

  public static bool list_q(object obj) {
	if (null_q(obj)) {
	  return true;
	} else if (pair_q(obj)) {
	  object current = obj;
	  while (pair_q(current)) {
		current = cdr(current);
	  }
	  return Eq(current, EmptyList);
	}
	//printf("false\n");
	return false;
  }

  public static object list_to_string(object lyst) {
	String retval = "";
	if (lyst is Cons) {
	  object current = lyst;
	  while (!Eq(current, EmptyList)) {
		retval += make_string(car(current));
		current = cdr(current);
	  }
	}
	return retval;
  }

  static object pivot (object p, object l) {
	if (null_q(l))
	  return symbol("done");
	else if (null_q(cdr(l)))
	  return symbol("done");
	// FIXME: use p to compare
	else if (cmp(car(l), cadr(l)) <= 0)
	  return pivot(p, cdr(l));
	else
	  return car(l);
  }

  // usage: (partition 4 '(6 4 2 1 7) () ()) -> returns partitions
  static object partition (object p, object piv,  object l, object p1, object p2) {
	if (null_q(l))
	  return list(p1, p2);
	// FIXME: use p to compare
	else if (cmp(car(l), piv) < 0)
	  return partition(p, piv, cdr(l), cons(car(l), p1), p2);
	else
	  return partition(p, piv, cdr(l), p1, cons(car(l), p2));
  }

  public static object sort(object p, object l) {
	object piv = pivot(p, l);
	if (Eq(piv, symbol("done"))) return l;
	object parts = partition(p, piv, l, EmptyList, EmptyList);
	return append(sort(p, car(parts)),
		sort(p, cadr(parts)));
  }
  
  public static object reverse(object lyst) {
	if (lyst is Cons) {
	  object result = EmptyList;
	  object current = ((Cons)lyst);
	  while (!Eq(current, EmptyList)) {
		result = new Cons(car(current), result);
		current = cdr(current);
	  }
	  return result;
	} else {
	  return EmptyList;
	}
  }

  public static object array_to_string(object[] args) {
	string retval = "";
	if (args != null) {
	  int count = ((Array)args).Length;
	  for (int i = 0; i < count; i++) {
		  if (args[i] is object[]) {
			retval += array_to_string((object[])args[i]);
		  } else {
			if (retval != "")
			  retval += " ";
			retval += args[i];
		  }
	  }
	}
	return "(vector " + retval + ")";
  }
  

  // cons/list needs to work with object[]

  public static object list(params object[] args) {
	//printf("calling list({0})\n", array_to_string(args));
	Object result = EmptyList;
	if (args != null) {
	  int count = ((Array)args).Length;
	  for (int i = 0; i < count; i++) {
		Object item = args[count - i - 1];
		//if (item == null) 
		//  result = EmptyList;
        // else
		if (item is object[])
		  result = append( list((object[]) item), result);
		else
		  result = new Cons(item, result);
	  }
	}
	//printf("returning list: {0}\n", pretty_print(result));	
	return result;
  }

  public static object rdc(object lyst) {
	if (null_q(cdr(lyst)))
	  return EmptyList;
	else
	  return cons(car(lyst), rdc(cdr(lyst)));
  }
	
  public static void set_cdr_b(object obj) {
	set_cdr_b(car(obj), cadr(obj));
  }
  
  public static void set_cdr_b(object lyst, object item) {
	Cons cell = (Cons) lyst;
	cell.cdr = item;
  }
  
  public static void set_car_b(object obj) {
	set_car_b(car(obj), cadr(obj));
  }

  public static void set_car_b(object lyst, object item) {
	Cons cell = (Cons) lyst;
	cell.car = item;
  }

  public static object append(params object[] obj) {
		object current = obj[0];
		for (int i = 1; i < obj.Length; i++) {
			current = append2(current, obj[i]);
		}
		return current;
  }

	public static object append2 (object obj1, object obj2)
	{
		if (! list_q (obj1)) {
			throw new Exception (string.Format ("error in append: {0} is not a list", obj1));
		} else if (obj1 is object[]) {
			Object lyst = list(obj1);
			return copy_of(lyst, obj2);
		} else  {
			return copy_of(obj1, obj2);
		}
	}

	public static object copy_of(object pair, object item) {
		object retval = item;
		object current = pair;
		while (current != EmptyList) {
			retval = cons(PJScheme.rac(current), retval);
			current = rdc(current);
		}
		return retval;
	}
	
  public static string format_prim(object obj) {
	return format(car(obj), list_to_array(obj, 1));
  }
	
  public static object sqrt(object obj) {
	if (pair_q(obj)) {
	  obj = car(obj);
	  if (obj is int)
		return Math.Sqrt((int)obj);
	  else
		throw new Exception(String.Format("can't take sqrt of this type of number: {0}", obj));
	}
	throw new Exception("need to apply procedure to list");
  }

  public static object abs(object obj) {
	if (pair_q(obj)) {
	  obj = car(obj);
	  if (obj is int)
		return Math.Abs((int)obj);
	  else
		throw new Exception(String.Format("can't take absolute value of this type of number: {0}", obj));
	}
	throw new Exception("need to apply procedure to list");
  }

  public static object cons(object obj) {
	return cons(car(obj), cadr(obj));
  }

  public static object cons(object obj1, object obj2) {
	return (object) new Cons(obj1, obj2);
  }

  public static object cdr(object obj) {
	if (obj is Cons) 
	  return ((Cons)obj).cdr;
	else
	  throw new Exception(string.Format("cdr: object is not a pair: {0}",
			  obj));
  }

  public static object car(object obj) {
	if (obj is Cons) 
	  return ((Cons)obj).car;
	else
	  throw new Exception(string.Format("car: object is not a pair: {0}",
			  obj));
  }

  public static object   caar(object x) {	return car(car(x));   }
  public static object   cadr(object x) { 	return car(cdr(x));   }
  public static object   cdar(object x) {	return cdr(car(x));   }
  public static object   cddr(object x) {	return cdr(cdr(x));   }
  public static object  caaar(object x) {	return car(car(car(x)));   }
  public static object  caadr(object x) { 	return car(car(cdr(x)));   }
  public static object  cadar(object x) {	return car(cdr(car(x)));   }
  public static object  caddr(object x) {	return car(cdr(cdr(x)));   }
  public static object  cdaar(object x) {	return cdr(car(car(x)));   }
  public static object  cdadr(object x) { 	return cdr(car(cdr(x)));   }
  public static object  cddar(object x) {	return cdr(cdr(car(x)));   }
  public static object  cdddr(object x) {	return cdr(cdr(cdr(x)));   }
  public static object caaaar(object x) {	return car(car(car(car(x))));   }
  public static object caaadr(object x) {	return car(car(car(cdr(x))));   }
  public static object caadar(object x) {	return car(car(cdr(car(x))));   }
  public static object caaddr(object x) {	return car(car(cdr(cdr(x))));   }
  public static object cadaar(object x) {	return car(cdr(car(car(x))));   }
  public static object cadadr(object x) {	return car(cdr(car(cdr(x))));   }
  public static object caddar(object x) {	return car(cdr(cdr(car(x))));   }
  public static object cadddr(object x) {	return car(cdr(cdr(cdr(x))));   }
  public static object cdaaar(object x) {	return cdr(car(car(car(x))));   }
  public static object cdaadr(object x) {	return cdr(car(car(cdr(x))));   }
  public static object cdadar(object x) {	return cdr(car(cdr(car(x))));   }
  public static object cdaddr(object x) {	return cdr(car(cdr(cdr(x))));   }
  public static object cddaar(object x) {	return cdr(cdr(car(car(x))));   }
  public static object cddadr(object x) {	return cdr(cdr(car(cdr(x))));   }
  public static object cdddar(object x) {	return cdr(cdr(cdr(car(x))));   }
  public static object cddddr(object x) {	return cdr(cdr(cdr(cdr(x))));   }

  public static void pretty_print(object obj) {
      // FIXME: repr is safe, just not pretty
      System.Console.WriteLine(repr(obj));
  }  

  public static void safe_print(object arg) {
    config.NEED_NEWLINE = false;
    object sarg = make_safe(arg);
    pretty_print(sarg);
  }

  // FIXME: need to cps/registerize this to avoid reliance on C-sharp's stack
  public static object make_safe(object x) {
    if (procedure_object_q(x))
      return (symbol("<procedure>"));
    else if (environment_object_q(x))
      return (symbol("<environment>"));
    else if (pair_q(x))
      return (cons(make_safe(car(x)), make_safe(cdr(x))));
    else if (vector_q(x))
      return (list_to_vector(make_safe(vector_to_list(x))));
    else
      return (x);
  }

// FIXME: Rewrite without recursion
  public static string string_append(object x) {
      if (null_q(x))
	  return "";
      else 
	  return (car(x).ToString() + string_append(cdr(x)));
  }

// FIXME: Rewrite without recursion
  public static object assq(object x, object ls) {
      if (null_q(ls)) 
	  return false;
      else if (Eq(x, caar(ls)))
	  return car(ls);
      else
	  return assq(x, cdr(ls));
  }

// assq compares keys with eq?
// assv uses eqv? 
// assoc uses equal?. 

// FIXME: Rewrite without recursion
  public static object assv(object x, object ls) {
      if (null_q(ls)) 
	  return false;
      else if (Eqv(x, caar(ls)))
	  return car(ls);
      else
	  return assv(x, cdr(ls));
  }

// FIXME: Rewrite without recursion
  public static object assoc(object x, object ls) {
      if (null_q(ls)) 
	  return false;
      else if (Equal(x, caar(ls)))
	  return car(ls);
      else
	  return assv(x, cdr(ls));
  }

  public static bool atom_q(object x) {
    return ((! pair_q(x)) && (! (null_q(x))));
  }

  public static bool procedure_object_q(object x) {
    return (procedure_q(x) || (pair_q(x) && Eq(car(x), symbol("procedure"))));
  }

  public static bool environment_object_q(object x) {
    return (pair_q(x) && Eq(car(x), symbol("environment")));
  }

  public static bool isTokenType(List<object> token, string tokenType) {
	return Equal(token[0], tokenType);
  }
  
  public static string arrayToString(object[] array) {
	string retval = "";
	foreach (object item in array) {
	  if (retval != "")
		retval += " ";
	  retval += item.ToString();
	}
	return retval;
  }

  public static object memq(object obj) {
	return memq(car(obj), cadr(obj));
  }

  public static object memq(object item1, object list) {
	if (list is Cons) {
	  object current = list;
	  while (! Eq(current, EmptyList)) {
		if (Equal(item1, car(current))) {
		    return current;
		}
		current = cdr(current);
	  }
	  return false;
	}
	return false;
  }

  public static object memv(object obj) {
	return memv(car(obj), cadr(obj));
  }

  public static object memv(object item1, object list) {
	if (list is Cons) {
	  object current = list;
	  while (! Eq(current, EmptyList)) {
		if (Equal(item1, car(current))) {
		    return current;
		}
		current = cdr(current);
	  }
	  return false;
	}
	return false;
  }

  public static object read_content(object filename) {
    System.IO.StreamReader fp = File.OpenText(filename.ToString());
	string text = fp.ReadToEnd();
    fp.Close();
    return text;
  }

  public static object string_append(object obj1, object obj2) {
	return (obj1.ToString() + obj2.ToString());
  }

  //--------------------------------------------------------------------------------------------
  // support for annotated s-expressions

  public static object atom_tag = new Cons(symbol("atom_tag"), EmptyList);

  public static object pair_tag = new Cons(symbol("pair_tag"), EmptyList);

  public static bool aatom_q(object x) {
    return (pair_q(x) && Eq(car(x), atom_tag));
  }

  public static bool apair_q(object x) {
    return (pair_q(x) && Eq(car(x), pair_tag));
  }

  public static bool annotated_q(object x) {
    return (pair_q(x) && (Eq(car(x), atom_tag) || Eq(car(x), pair_tag)));
  }

    /*
      assignment_q_hat
      begin_q_hat
      catch_q_hat
      choose_q_hat
      continuation_object_q
      define_b_q_hat
      define_q_hat
      define_syntax_q_hat
      f_hat
      f_hat_proc
      finally_q_hat
      func_q_hat
      if_else_q_hat
      if_then_q_hat
      lambda_q_hat
      make_safe_continuation_proc
      quasiquote_q_hat
      quote_q_hat
      raise_q_hat
      trace_lambda_q_hat
      try_q_hat
      unquote_q_hat
      unquote_splicing_q_hat
    */

    /*
  public static bool list_q_hat(object x) {
    return (asexp_q(x) && list_of_asexp_q(get_sexp(x)));
  }

  public static int length_hat(object asexp) {
    return ((int)length(get_sexp(asexp)));
  }

  public static bool symbol_q_hat(object asexp) { return (symbol_q(get_sexp(asexp))); }
  public static object car_hat(object asexp) { return (car(get_sexp(asexp))); }
  public static object cdr_hat(object asexp) { return (cdr(get_sexp(asexp))); }
  public static object cadr_hat(object asexp) { return (cadr(get_sexp(asexp))); }
  public static object cddr_hat(object asexp) { return (cddr(get_sexp(asexp))); }
  public static object caddr_hat(object asexp) { return (caddr(get_sexp(asexp))); }
  public static object cdddr_hat(object asexp) { return (cdddr(get_sexp(asexp))); }
  public static object cadddr_hat(object asexp) { return (cadddr(get_sexp(asexp))); }

  public static object map_hat(object proc, object asexp) {
    return (map(proc, get_sexp(asexp)));
   }

  public static Func<object,bool> tagged_list_hat(object test_string, object pred, object value) {
    return (object x) => {
      bool retval = (list_q_hat(x) &&
		     (((Predicate2)pred)(length_hat(x), value)) &&
		     symbol_q_hat(car_hat(x)) &&
		     Eq(get_sexp(car_hat(x)), string_to_symbol(test_string)));
      return retval;
    };
  }

  static object x_reg = null;
  static object k_reg = null;
  static bool value_reg = false;
  static Function pc = null;

  public static bool list_of_asexp_q(object x) {
    x_reg = x;
    k_reg = EmptyList;
    pc = list_of_asexp1;
    while (pc != null) pc();
    return value_reg;
  }

  public static void list_of_asexp1() {
    if (null_q(x_reg)) {
      value_reg = true;
      pc = list_of_asexp2;
    } else if (pair_q(x_reg)) {
      if (asexp_q(car(x_reg))) {
	k_reg = cons(x_reg, k_reg);
	x_reg = cdr(x_reg);
	pc = list_of_asexp1;
      } else {
	value_reg = false;
	pc = list_of_asexp2;
      }
    } else {
      value_reg = false;
      pc = list_of_asexp2;
    }
  }

  public static void list_of_asexp2() {
    if (null_q(k_reg)) {
      pc = null;
    } else {
      object x = car(k_reg);
      object k = cdr(k_reg);
      if (value_reg) {
	k_reg = k;
	pc = list_of_asexp2;
      } else if (asexp_q(cdr(x))) {
	k_reg = k;
	x_reg = get_sexp(cdr(x));
	pc = list_of_asexp1;
      } else {
	k_reg = k;
	pc = list_of_asexp2;
      }
    }
  }
    */

  //--------------------------------------------------------------------------------------------

  public class Cons : IList {
      public object car;
      public object cdr;
      public int id;
  
  public Cons(object a, object b) {
      Scheme.CONS_ID++;
      this.id = Scheme.CONS_ID;
	this.car = a;
	if (b is object[] || b == null) 
	  this.cdr = list(b);
	else
	  this.cdr = b;
  }

  // Items necessary for it to be an IList
  public bool IsFixedSize {
	get {
	  return false;
	}
  }

  public bool IsReadOnly {
	get {
	  return false;
	}
  }

  public bool IsSynchronized {
	get {
	  return false;
	}
  }

  public void CopyTo(System.Array array, int index) {
  }

  public int Add(object value) {
	return 1; // should return count
  }

  public int Count {
	get {
	  if (cdr(this) == EmptyList)
	    return 1;
	  else
	    return 1 + ((Cons)cdr(this)).Count;
	}
  }

  public void Remove(object value) {
  }

  public void RemoveAt(int index) {
  }

  public void Clear() {
  }

  public bool Contains(object value) {
	return false;
  }

  public int IndexOf(object value) {
	return -1;
  }

  public void Insert(int index, object value) {
  }

  public object this[int index] {
	get {
	  object retval = null;
	  object mylist = (object)this;
	  while (index != -1) {
		if (mylist is Cons) {
		  retval = ((Cons)mylist).car;
		  mylist = ((Cons)mylist).cdr;
		}
		index--;
	  }
	  return retval;
	}
	set { // value is the item
	}
  }
  public object SyncRoot {
	get {
	  return this;
	}
  }
  public IEnumerator GetEnumerator() {
	// Refer to the IEnumerator documentation for an example of
	// implementing an enumerator.
	throw new Exception("The method or operation is not implemented.");
  }

  /// ---------------------------------------------------------------
  public string SafeToString() {
	if (this.car == symbol("quote") &&
		(this.cdr is Cons) &&
		((Cons)this.cdr).cdr == EmptyList) {
	  return String.Format("'{0}", ((Cons)this.cdr).car);
	} else if (this.car == symbol("quasiquote") &&
		(this.cdr is Cons) &&
		((Cons)this.cdr).cdr == EmptyList) {
	  return String.Format("`{0}", ((Cons)this.cdr).car);
	} else if (this.car == symbol("unquote") &&
		(this.cdr is Cons) &&
		((Cons)this.cdr).cdr == EmptyList) {
	  return String.Format(",{0}", ((Cons)this.cdr).car);
	} else if (this.car == symbol("unquote-splicing") &&
		(this.cdr is Cons) &&
		((Cons)this.cdr).cdr == EmptyList) {
	  return String.Format(",@{0}", ((Cons)this.cdr).car);
	} else {
	  return String.Format("({0} ...)", this.car); //...
	}
  }

  public String __repr__() {
      return ToString();
  }
  
  public override string ToString() { // Unsafe
	if (procedure_q(this)) 
	  return "#<procedure>";
	else if (module_q(this)) 
	  return String.Format("#<module {0}>", this.car);
	else if (environment_q(this)) 
	  return String.Format("#<environment>");
	else if (this.car == symbol("quote") &&
		(this.cdr is Cons) &&
		((Cons)this.cdr).cdr == EmptyList) {
	  return String.Format("'{0}", ((Cons)this.cdr).car);
	} else if (this.car == symbol("quasiquote") &&
		(this.cdr is Cons) &&
		((Cons)this.cdr).cdr == EmptyList) {
	  return String.Format("`{0}", ((Cons)this.cdr).car);
	} else if (this.car == symbol("unquote") &&
		(this.cdr is Cons) &&
		((Cons)this.cdr).cdr == EmptyList) {
	  return String.Format(",{0}", ((Cons)this.cdr).car);
	} else if (this.car == symbol("unquote-splicing") &&
		(this.cdr is Cons) &&
		((Cons)this.cdr).cdr == EmptyList) {
	  return String.Format(",@{0}", ((Cons)this.cdr).car);
	} else {
	  string s = String.Format("({0}", this.car);
	  object sexp = this.cdr;
	  while (sexp is Cons) {
		s += String.Format(" {0}", ((Cons)sexp).car);
		sexp = ((Cons)sexp).cdr;
	  }
	  if (Eq(sexp, EmptyList)) {
		s += ")";
	  } else {
		s += String.Format(" . {0})", sexp);
	  }
	  return s;
	}
  }
}

    public class Vector : IList {
  
  private object[] values;
  
  public Vector(object[] args) {
    values = args;
  }

  public object get(int index) {
    return values[index];
  }

  public object length() {
    return values.Length;
  }

  public object ToList() {
    return list(values);
  }

  public void set(int index, object value) {
    values[index] = value;
  }

  public override string ToString() {
    string retval = "";
    for (int i = 0; i < values.Length; i++) {
      retval += values[i].ToString();
      if (i < values.Length-1) {
	retval += " ";
      }
    }
    return String.Format("#{0}({1})", values.Length, retval);
  }

  public String __repr__() {
      return ToString();
  }
  
  // Items necessary for it to be an IList
  public bool IsFixedSize {
	get {
	  return true;
	}
  }

  public bool IsReadOnly {
	get {
	  return true;
	}
  }

  public bool IsSynchronized {
	get {
	  return false;
	}
  }

  public void CopyTo(System.Array array, int index) {
  }

  public int Add(object value) {
	return 0; // should return count
  }

  public int Count {
    get {
      return values.Length;
    }
  }

  public void Remove(object value) {
  }

  public void RemoveAt(int index) {
  }

  public void Clear() {
  }

  public bool Contains(object value) {
      for (int i = 0; i < values.Length; i++) {
	  if (Eq(values[i], value)) {
	      return true;
	  }
      }
      return false;
  }

  public int IndexOf(object value) {
      for (int i = 0; i < values.Length; i++) {
	  if (Eq(values[i], value)) {
	      return i;
	  }
      }
      return -1;
  }

  public void Insert(int index, object value) {
  }

  public object this[int index] {
	get {
	    return values[index];
	}
	set { // value is the item
	    values[index] = value;
	}
  }
  public object SyncRoot {
	get {
	  return this;
	}
  }
  public IEnumerator GetEnumerator() {
    // FIXME: return enumerator that returns nothing
    // Refer to the IEnumerator documentation for an example of
    // implementing an enumerator.
    throw new Exception("The method or operation is not implemented.");
  }
}

  public static void Main(string [] args) {
	//printf(append(list(symbol("a")), list(symbol("b"))));
	//printf(append(list(symbol("a"), symbol("b")), list(symbol("c"))));
	//printf(append(list(symbol("a")), list(symbol("b")), list(symbol("c"))));
			
	//printf ("  (load \"sllgen.ss\"): {0}\n",
	//		PJScheme.execute_string_rm("(load \"/home/dblank/Calico/trunk/examples/scheme/sllgen.ss\")"));
	printf ("  (print (list 1 2 3): {0}\n",
			PJScheme.execute_string_rm("(print (list 1 2 3))"));
	//printf ("  (1): {0}\n",
		//PJScheme.execute_string_rm("(1)"));
	// ----------------------------------
	// Math:
//	if (false) {
//		printf ("  (using \"Graphics\"): {0}\n",
//			PJScheme.execute("(using \"Graphics\")"));
//		} else {
//	printf ("  Add(1,1), Result: {0}, Should be: {1}\n", 
//		Add(1, 1), 1 + 1);
//	printf ("  Multiply(10,2), Result: {0}, Should be: {1}\n", 
//		Multiply(10, 2), 10 * 2);
//	printf ("  Divide(5,2), Result: {0}, Should be: {1}\n", 
//		Divide(5, 2), 5/2.0);
//	printf ("  Subtract(22,7), Result: {0}, Should be: {1}\n", 
//		Subtract(22, 7), 22 - 7);
//	// -----------------------------------
//	// Equal tests:
//	printf("hello == hello: {0}\n", Equal("hello", "hello"));
//	printf("hello == hel: {0}\n", Equal("hello", "hel"));
//	printf("hello == helloo: {0}\n", Equal("hello", "helloo"));
//	printf("4.1 == 4: {0}\n", Equal(4.1, 4));
//	printf("hello == true: {0}\n", Equal("hello", true));
//	
//	printf("() == (): {0}\n", Equal(list(), list()));
//
//	object t = list(2);
//	printf("t = list(2): {0}\n", t);
//	printf("list? t: {0}\n", list_q(t));
//	printf("null? t: {0}\n", null_q(t));
//
//	//cons("a", EmptyList).ToString();
//	printf("cons('a', ()): {0}\n", cons("a", EmptyList));
//	
//	t = cons("b", cons("a", t));
//	t = cons("c", cons("a", t));
//	t = cons("d", cons("a", t));
//	printf("t = : {0}\n", t);
//
//	printf("null? cdr(t): {0} {1}\n", 
//		null_q(cdr(t)), 
//		cdr(t));
//	printf("null? cddr(t): {0}\n", null_q(cddr(t)));
//
//	//	printf("null? cdddr(t): {0}\n", null_q(cdddr(t)));
// 	printf("Member test: \n");
//	t = cons("hello", t);
//	printf("t = {0}\n", repr(t));
//	printf("member(hello, t) : {0}\n", repr(member("hello", t)));
//	printf("member(a, t) : {0}\n", repr(member("a", t)));
//	printf("member(c, t) : {0}\n", repr(member("c", t)));
//	printf("(): {0}\n", repr(list()));
//	printf("list(t): {0}\n", repr(list(t)));
//	printf("length(list(t)): {0}\n", length(list(t)));
//	printf("length(cdr(list(t))): {0}\n", length(cdr(list(t))));
//	printf("length(car(list(t))): {0}\n", length(car(list(t))));
//	printf("cons(\"X\", list(t))): {0}\n", repr(cons("X", list(t))));
//	printf("x is: {0}\n", repr("x"));
//	printf("t is: {0}\n", repr(t));
//	printf("list(): {0}\n", list());
//	printf("cons('a', list()): {0}\n", cons("a", list()));
//	printf("cons('a', 'b'): {0}\n", cons("a", "b"));
//
//	printf("cons('a', null): {0}\n", cons("a", null));
//
//	printf("string-append('test', NULL): \"{0}\"\n", 	 
//		string_append ((object) "test",
//			(object) make_string ((object) NULL)));
//
//	int val = 15;
//	printf("BigInteger, long, int:\n");
//	printf("  {0}: {1} == {2} == WRONG! {3}\n", val,
//		bigfact(makeBigInteger(val)), longfact(val), intfact(val));
//	printf("Multiply:\n");
//	printf("15: {0} \n", Multiply(Multiply( Multiply( intfact(12), 13), 14), 15));
//
//	printf("1827391823712983712983712938: {0}\n", BigIntegerParse("1827391823712983712983712938"));
//
//    printf("display(list_to_vector( list(1, 2, 3))): ");
//    display(list_to_vector( list(1, 2, 3)));
//    printf("\n");
//		}
  }

  public static long longfact(long n) {
	if (n == 1) return n;
	return n * longfact(n - 1);
  }

  public static int intfact(int n) {
	if (n == 1) return n;
	return n * intfact(n - 1);
  }

  public static BigInteger bigfact(BigInteger n) {
	if (n == 1) return n;
	return n * bigfact(n - 1);
  }

  public static void set_dlr(ScriptScope scope, ScriptRuntime runtime) {
    _dlr_env = scope;
    _dlr_runtime = runtime;
  }

  public static void set_global_value_b(object var, object value) {
	if (_dlr_env != null) {
	  _dlr_env.SetVariable(var.ToString(), 
		  value);
	} else {
	  throw new Exception(String.Format("DLR Environment not available"));
	}
  }

  public static void set_global_docstring_b(object var, object value) {
      // FIXME: how to set docstring?
  }

  public static double currentTime () {
      System.TimeSpan t = System.DateTime.UtcNow - new System.DateTime (1970, 1, 1);
      return t.TotalSeconds;
  }

  public static void wait (double seconds) {
      if (seconds < .1)
	  System.Threading.Thread.Sleep ((int)(seconds * 1000));
      else {
	  double start = currentTime ();
	  while (seconds > currentTime () - start) {
	      while (Gtk.Application.EventsPending ())
		  Gtk.Application.RunIteration ();
	      System.Threading.Thread.Sleep (100); 
	  }
      }
  }

  public static void highlight_expression(object exp) {
      // This should be made fast, as it happens on each step!
      Calico.MainWindow calico;
      object info = PJScheme.rac(exp);
      if (Equal(info, symbol("none"))) {
	  return;
      }
      int start_line = (int)PJScheme.get_start_line(info);

		/* force CLI output
      string filename = PJScheme.get_srcfile(info).ToString();
      int start_col = (int)PJScheme.get_start_char(info);
      int end_line = (int)PJScheme.get_end_line(info);
      int end_col = (int)PJScheme.get_end_char(info);
	  printf("[~s] at line ~s, column ~s~%", filename, start_line, start_col);
      printf("~s~%", PJScheme.aunparse(exp));
	  return;			
	}
		 */
 	  if (_dlr_env != null) {
	  calico = (Calico.MainWindow)_dlr_env.GetVariable("calico");
	  if (calico != null) {
	      if (calico.CurrentDocument == null) {
		  return;
	      } else if (calico.CurrentDocument.HasBreakpointSetAtLine(start_line)) {
		  // don't return! Fall through and print
	      } else if (calico.ProgramSpeed.Value == 100) {
		  return;
	      }
	  } else {
	      return;
	  }
      } else {
	  return;
      }
      string filename = PJScheme.get_srcfile(info).ToString();
      Calico.TextDocument document = (Calico.TextDocument)calico.GetDocument(filename);
      int start_col = (int)PJScheme.get_start_char(info);
      int end_line = (int)PJScheme.get_end_line(info);
      int end_col = (int)PJScheme.get_end_char(info);
      calico.playResetEvent.Reset (); 
      Calico.MainWindow.Invoke ( delegate {
	      document.GotoLine(start_line);
	      document.texteditor.SetSelection(start_line, start_col, end_line, end_col + 1);
          });
      if (! Equal(car(exp), symbol("lit-aexp"))) {
	  printf("{0}call: ", 
		 string_append(PJScheme.repeat(" |", 
					       (object)PJScheme.get_closure_depth())));
	  //try {
	      printf("~s~%", PJScheme.aunparse(exp));
	  //} catch {
	  //    printf("{0}return: ERROR in PRINTING AUNPARSE!~%", 
	//	     string_append(PJScheme.repeat(" |", 
	//					   (object)PJScheme.get_closure_depth())));
	  //}
	  PJScheme.increment_closure_depth();
      }
  }

  public static void handle_debug_info(object exp, object result) {
      // This should be made fast, as it happens on each step!
      Calico.MainWindow calico;
      object info = PJScheme.rac(exp);
      if (Equal(info, symbol("none"))) {
	  return;
      }
	  /*
	  Force CLI output 
      printf("result: ~s~%", result);
      */		
      int start_line = (int)PJScheme.get_start_line(info);
      if (_dlr_env != null) {
	  calico = (Calico.MainWindow)_dlr_env.GetVariable("calico");
	  if (calico != null) {
	      if (calico.CurrentDocument == null) {
		  return;
	      } else if (calico.CurrentDocument.HasBreakpointSetAtLine(start_line)) {
		  // don't return! Fall through and wait
	      } else if (calico.ProgramSpeed.Value == 100) {
		  return;
	      }
	  } else {
	      return;
	  }
      } else {
	  return;
      }
      // We have a calico defined and we should trace and/or stop
      string filename = PJScheme.get_srcfile(info).ToString();
      Calico.TextDocument document = (Calico.TextDocument)calico.GetDocument(filename);
      // (lit-aexp (a b c) (stdin 1 1 1 1 8 8))
      // <sourcefile>    1  1
      // <startline>     1  8
      // <startchar>     1  8
      // <startposition> 1  1 
      // <endline>       1  8
      // <endchar>       8  8
      // <endposition>   8  1
      //                   17
      //		   17

      int start_col = (int)PJScheme.get_start_char(info);
      int end_line = (int)PJScheme.get_end_line(info);
      int end_col = (int)PJScheme.get_end_char(info);
      calico.playResetEvent.Reset (); 
      Calico.MainWindow.Invoke ( delegate {
	      calico.PlayButton.Sensitive = true;
	      calico.PauseButton.Sensitive = false;
	      document.GotoLine(start_line);
	      document.texteditor.SetSelection(start_line, start_col, end_line, end_col + 1);
          });
      if (! Equal(car(exp), symbol("lit-aexp"))) {
	  PJScheme.decrement_closure_depth();
	  try {
	      printf("{0}return: ", 
		     string_append(PJScheme.repeat(" |", 
						   (object)PJScheme.get_closure_depth())));
	      printf("~s~%", repr(result));
	  } catch {
	      printf("{0}return: ERROR in PRINTING RESULT!~%", 
		     string_append(PJScheme.repeat(" |", 
						   (object)PJScheme.get_closure_depth())));
	  }
      }
      if (calico.ProgramSpeed.Value == 0 || calico.CurrentDocument.HasBreakpointSetAtLine(start_line)) {
	  printf("{0}trace: Paused!~%", 
		 string_append(PJScheme.repeat(" |", 
					       (object)PJScheme.get_closure_depth())));
	  calico.playResetEvent.WaitOne();
      } else if (calico.ProgramSpeed.Value < 100) { // then we are in a delay:
	  double pause = ((100.0 - calico.ProgramSpeed.Value) / 100.0) * 2.0;
	  // Force at least a slight sleep, else no GUI controls
	  wait(pause);
      }
  }

  public static object search_frame(object frame, object variable) {
      if (PJScheme.empty_frame_q(frame)) {
	  return false;
      } else {
	  if (frame is Vector) {
	      Vector v = (Vector)frame;
	      for (int i = 0; i < (int)v.length(); i++) {
		  if (Eq(PJScheme.binding_variable(v.get(i)), variable)) {
		      return v.get(i);
		  }
	      }
	  } else {
	      throw new Exception("frame is not a vector");
	  }
      }
      return false;
  }

    public static object my_get_lexical_address_frame(object frame, object variable, object depth, object offset, object info) {
	// returns (#t pos) or (#f) signifying bound or free, respectively
	if (PJScheme.empty_frame_q(frame)) {
	    return list(false); // not in this frame
	} else if ((int)offset >= (int)vector_length(frame)) {
	    return list(false); // not here
	} else {
	    for (int i = (int)offset; i < (int)((Vector)frame).length(); i++) {
		if (Eq(PJScheme.binding_variable(vector_ref(frame, i)), variable)) {
		    return list(true, PJScheme.lexical_address_aexp(depth, i, variable, info));
		}
	    }
	    return list(false); // not here
	}
    }
}
