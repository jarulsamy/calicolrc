/*
  ;; Calico Scheme interpreter with support for choose
  ;;
  ;; Written by James B. Marshall and Douglas S. Blank
  ;; jmarshall@slc.edu
  ;; http://science.slc.edu/~jmarshall
  ;; dblank@brynmawr.edu
  ;; http://cs.brynmawr.edu/~dblank
*/

using System;

public class ObjectType {
    public static int ObjTst(System.Object o1, System.Object o2, bool TextCompare) {
	check {
	    // comparing null objects
	    // if both are null, return 0
	    // if one is a type, convert the other to its 'null value'
	    if ((o1 == null) && (o2 == null)) {
		return 0;
	    }
	    if (o1 == null) {
		o1 = CreateNullObjectType(o2);
	    }
	    if (o2 == null) {
		o2 = CreateNullObjectType(o1);
	    }
	    // FIXME: Add Elseif implementation for all types
	    if ((o1 is Double) || (o2 is Double)) {
		Double dbl1 = Convert.ToDouble(o1);
		Double dbl2 = Convert.ToDouble(o2);
		if (dbl1 < dbl2) {
		    return -1;
		}
		if (dbl1 > dbl2) {
		    return 1;
		}
		// if dbl1 = dbl2
		return 0;
	    } else if ((o1 is Single) || (o2 is Single)) {
		Single f1 = Convert.ToSingle(o1);
		Single f2 = Convert.ToSingle(o2);
		if (f1 < f2) {
		    return (-1);
		}
		if (f1 > f2) {
		    return 1;
		}
		// if f1 = f2
		return 0;
	    } else if ((o1 is Decimal) || (o2 is Decimal)) {
		Decimal dec1 = Convert.ToDecimal(o1);
		Decimal dec2 = Convert.ToDecimal(o2);
		
		if (dec1 < dec2) {
		    return -1;
		}

		if (dec1 > dec2) {
		    return 1;
		}
	    
		// if dec1 = dec2
		return 0;
	    
	    } else if ((o1 is Int64) || (o2 is Int64)) {
		Int64 l1 = Convert.ToInt64(o1);
		Int64 l2 = Convert.ToInt64(o2);
	    
		if (l1 < l2) {
		    return -1;
		}
	    
		if (l1 > l2) {
		    return 1;
		}
	    
		// if l1 = l2
		return 0;
	    } else if ((o1 is int) || (o2 is int)) {
		int i1 = Convert.ToInt32(o1);
		int i2 = Convert.ToInt32(o2);
	    
		if (i1 < i2) {
		    return -1;
		}

		if (i1 > i2) {
		    return 1;
		}

		// if i1 = i2
		return 0;
	    } else if ((o1 is short) || (o2 is short)) {
		short short1 = Convert.ToInt16(o1);
		short short2 = Convert.ToInt16(o2);
	    
		if (short1 < short2) {
		    return -1;
		}
	    
		if (short1 > short2) {
		    return 1;
		}

		// if short1 = short2
		return 0;

		/*	} else if ((o1 is Date) || (o2 is Date)) {
			dt1 = DateType.FromObject(o1);
			dt2 = DateType.FromObject(o2);
			return dt1.CompareTo(dt2);
		*/
	    } else if ((o1 is String) || (o2 is String)) {
		string s1 = Convert.ToString(o1);
		string s2 = Convert.ToString(o2);
	    
		return s1.CompareTo(s2);
	    
	    } else if ((o1 is Char) || (o2 is Char)) {
		char c1 = Convert.ToChar(o1);
		char c2 = Convert.ToChar(o2);
	    
		return c1.CompareTo(c2);
	    
	    } else if ((o1 is Byte) || (o2 is Byte)) {
		byte b1 = Convert.ToByte(o1);
		byte b2 = Convert.ToByte(o2);
	    
		return b1.CompareTo(b2);
	    
	    } else if ((o1 is Boolean) || (o2 is Boolean)) {
		bool bool1 = Convert.ToBoolean(o1);
		bool bool2 = Convert.ToBoolean(o2);
	    
		return bool1.CompareTo(bool2);
	    
	    } else { // Not implemented case
		throw new Exception("Case Not Implemented");
	    }
	}
    }

    public static object AddObj(System.Object o1, System.Object o2) {
	checked {
	    if ((o1 == null) && (o2 == null)) {
		return 0;
	    }
	    if (o1 == null) {
		o1 = CreateNullObjectType(o2);
	    }
	    if (o2 == null) {
		o2 = CreateNullObjectType(o1);
	    }
	
	    //FIXME: Add defense for checking overflow.
	    //FIXME: Add support for Date
	    if ((o1 is Double) || (o2 is Double)) {
		Double dbl1 = Convert.ToDouble(o1);
		Double dbl2 = Convert.ToDouble(o2);
	    
		return dbl1 + dbl2;
	    } else if ((o1 is Single) || (o2 is Single)) {
		Single f1 = Convert.ToSingle(o1);
		Single f2 = Convert.ToSingle(o2);
	    
		return f1 + f2;

	    } else if ((o1 is Decimal) || (o2 is Decimal)) {
		Decimal dec1 = Convert.ToDecimal(o1);
		Decimal dec2 = Convert.ToDecimal(o2);
	    
		return dec1 + dec2;

	    } else if ((o1 is Int64) || (o2 is Int64)) {
		Int64 l1 = Convert.ToInt64(o1);
		Int64 l2 = Convert.ToInt64(o2);
	    
		return l1 + l2;
	    } else if ((o1 is int) || (o2 is int)) {
		int i1 = Convert.ToInt32(o1);
		int i2 = Convert.ToInt32(o2);
	    
		return i1 + i2;
	    
	    } else if ((o1 is Int16) || (o2 is Int16)) {
		Int16 short1 = Convert.ToInt16(o1);
		Int16 short2 = Convert.ToInt16(o2);
	    
		return short1 + short2;
	    
	    } else if ((o1 is Byte) || (o2 is Byte)) {
		Byte b1 = Convert.ToByte(o1);
		Byte b2 = Convert.ToByte(o2);
	    
		return b1 + b2;
	    
	    } else if ((o1 is Boolean) || (o2 is Boolean)) {
		Boolean bool1 = Convert.ToBoolean(o1);
		Boolean bool2 = Convert.ToBoolean(o2);
	    
		return Convert.ToInt16(bool1) + Convert.ToInt16(bool2);
	    
	    } else if (((o1 is String) && (o2 is String)) ||
		       ((o1 is Char) && (o2 is Char)) || 
		       ((o1 is String) && (o2 is Char)) || 
		       ((o1 is Char) && (o2 is String))) {
		// both are String, its a Concat
		// both are Char, its a Concat
		// one is String and one is Char, its a Concat
		String s1 = Convert.ToString(o1);
		String s2 = Convert.ToString(o2);
	    
		return s1 + s2;
	    
	    } else if ((o1 is String) || (o2 is String)) {
		// one is String, its a numeric Add
	    
		String s1 = Convert.ToString(o1);
		String s2 = Convert.ToString(o2);
	    
		if ((s1 == null) || (s1.Length == 0)) { s1 = "0"; }
		if ((s2 == null) || (s2.Length == 0)) { s2 = "0"; }
	    
		Double dbl1 = Convert.ToDouble(s1);
		Double dbl2 = Convert.ToDouble(s2);
	    
		return dbl1 + dbl2;
	    } else {// Not implemented case
		throw new Exception("Implement me: " + o1.GetType().Name + " " + o2.GetType().Name);
	    }
	}
    }

    public static object SubObj(System.Object o1, System.Object o2) {
	checked {
	    if ((o1 == null) && (o2 == null)) {
		return 0;
	    }
	    if (o1 == null) {
		o1 = CreateNullObjectType(o2);
	    }
	    if (o2 == null) {
		o2 = CreateNullObjectType(o1);
	    }
	
	    //FIXME: Add defense for checking overflow.
	    //FIXME: Add support for Date
	    if ((o1 is Double) || (o2 is Double)) {
		Double dbl1 = Convert.ToDouble(o1);
		Double dbl2 = Convert.ToDouble(o2);
	    
		return dbl1 + dbl2;
	    } else if ((o1 is Single) || (o2 is Single)) {
		Single f1 = Convert.ToSingle(o1);
		Single f2 = Convert.ToSingle(o2);
	    
		return f1 - f2;

	    } else if ((o1 is Decimal) || (o2 is Decimal)) {
		Decimal dec1 = Convert.ToDecimal(o1);
		Decimal dec2 = Convert.ToDecimal(o2);
	    
		return dec1 - dec2;

	    } else if ((o1 is Int64) || (o2 is Int64)) {
		Int64 l1 = Convert.ToInt64(o1);
		Int64 l2 = Convert.ToInt64(o2);
	    
		return l1 - l2;
	    } else if ((o1 is int) || (o2 is int)) {
		int i1 = Convert.ToInt32(o1);
		int i2 = Convert.ToInt32(o2);
	    
		return i1 - i2;
	    
	    } else if ((o1 is Int16) || (o2 is Int16)) {
		Int16 short1 = Convert.ToInt16(o1);
		Int16 short2 = Convert.ToInt16(o2);
	    
		return short1 - short2;
	    
	    } else if ((o1 is Byte) || (o2 is Byte)) {
		Byte b1 = Convert.ToByte(o1);
		Byte b2 = Convert.ToByte(o2);
	    
		return b1 - b2;
	    
	    } else if ((o1 is Boolean) || (o2 is Boolean)) {
		Boolean bool1 = Convert.ToBoolean(o1);
		Boolean bool2 = Convert.ToBoolean(o2);
	    
		return Convert.ToInt16(bool1) - Convert.ToInt16(bool2);
	    
	    } else if ((o1 is String) || (o2 is String)) {
		// one is String, its a numeric Add
	    
		String s1 = Convert.ToString(o1);
		String s2 = Convert.ToString(o2);
	    
		if ((s1 == null) || (s1.Length == 0)) { s1 = "0"; }
		if ((s2 == null) || (s2.Length == 0)) { s2 = "0"; }
	    
		Double dbl1 = Convert.ToDouble(s1);
		Double dbl2 = Convert.ToDouble(s2);
	    
		return dbl1 - dbl2;
	    } else {// Not implemented case
		throw new Exception("Implement me: " + o1.GetType().Name + " " + o2.GetType().Name);
	    }
	}
    }

    public static object MulObj(System.Object o1, System.Object o2) {
	checked {
	    if ((o1 == null) && (o2 == null)) {
		return 0;
	    }
	    if (o1 == null) {
		o1 = CreateNullObjectType(o2);
	    }
	    if (o2 == null) {
		o2 = CreateNullObjectType(o1);
	    }
	
	    //FIXME: Add defense for checking overflow.
	    //FIXME: Add support for Date
	    if ((o1 is Double) || (o2 is Double)) {
		Double dbl1 = Convert.ToDouble(o1);
		Double dbl2 = Convert.ToDouble(o2);
		return dbl1 * dbl2;
	    } else if ((o1 is Single) || (o2 is Single)) {
		Single f1 = Convert.ToSingle(o1);
		Single f2 = Convert.ToSingle(o2);
		return f1 * f2;
	    } else if ((o1 is Decimal) || (o2 is Decimal)) {
		Decimal dec1 = Convert.ToDecimal(o1);
		Decimal dec2 = Convert.ToDecimal(o2);
		return dec1 * dec2;
	    } else if ((o1 is Int64) || (o2 is Int64)) {
		Int64 l1 = Convert.ToInt64(o1);
		Int64 l2 = Convert.ToInt64(o2);
		return l1 * l2;
	    } else if ((o1 is int) || (o2 is int)) {
		int i1 = Convert.ToInt32(o1);
		int i2 = Convert.ToInt32(o2);
		return i1 * i2;
	    } else if ((o1 is Int16) || (o2 is Int16)) {
		Int16 short1 = Convert.ToInt16(o1);
		Int16 short2 = Convert.ToInt16(o2);
		return short1 * short2;
	    } else if ((o1 is Byte) || (o2 is Byte)) {
		Byte b1 = Convert.ToByte(o1);
		Byte b2 = Convert.ToByte(o2);
		return b1 * b2;
	    } else if ((o1 is Boolean) || (o2 is Boolean)) {
		Boolean bool1 = Convert.ToBoolean(o1);
		Boolean bool2 = Convert.ToBoolean(o2);
		return Convert.ToInt16(bool1) * Convert.ToInt16(bool2);
	    } else if ((o1 is String) || (o2 is String)) {
		// one is String, its a numeric Add
		String s1 = Convert.ToString(o1);
		String s2 = Convert.ToString(o2);
		if ((s1 == null) || (s1.Length == 0)) { s1 = "0"; }
		if ((s2 == null) || (s2.Length == 0)) { s2 = "0"; }
		Double dbl1 = Convert.ToDouble(s1);
		Double dbl2 = Convert.ToDouble(s2);
		return dbl1 * dbl2;
	    } else {// Not implemented case
		throw new Exception("Implement me: " + o1.GetType().Name + " " + o2.GetType().Name);
	    }
	}
    }

    public static object DivObj(System.Object o1, System.Object o2) {
	checked {
	    if ((o1 == null) && (o2 == null)) {
		return 0;
	    }
	    if (o1 == null) {
		o1 = CreateNullObjectType(o2);
	    }
	    if (o2 == null) {
		o2 = CreateNullObjectType(o1);
	    }
	
	    //FIXME: Add defense for checking overflow.
	    //FIXME: Add support for Date
	    if ((o1 is Double) || (o2 is Double)) {
		Double dbl1 = Convert.ToDouble(o1);
		Double dbl2 = Convert.ToDouble(o2);
	    
		return dbl1 / dbl2;
	    } else if ((o1 is Single) || (o2 is Single)) {
		Single f1 = Convert.ToSingle(o1);
		Single f2 = Convert.ToSingle(o2);
	    
		return f1 / f2;

	    } else if ((o1 is Decimal) || (o2 is Decimal)) {
		Decimal dec1 = Convert.ToDecimal(o1);
		Decimal dec2 = Convert.ToDecimal(o2);
	    
		return dec1 / dec2;

	    } else if ((o1 is Int64) || (o2 is Int64)) {
		Int64 l1 = Convert.ToInt64(o1);
		Int64 l2 = Convert.ToInt64(o2);
	    
		return l1 / l2;
	    } else if ((o1 is int) || (o2 is int)) {
		int i1 = Convert.ToInt32(o1);
		int i2 = Convert.ToInt32(o2);
	    
		return i1 / i2;
	    
	    } else if ((o1 is Int16) || (o2 is Int16)) {
		Int16 short1 = Convert.ToInt16(o1);
		Int16 short2 = Convert.ToInt16(o2);
	    
		return short1 / short2;
	    
	    } else if ((o1 is Byte) || (o2 is Byte)) {
		Byte b1 = Convert.ToByte(o1);
		Byte b2 = Convert.ToByte(o2);
	    
		return b1 / b2;
	    
	    } else if ((o1 is Boolean) || (o2 is Boolean)) {
		Boolean bool1 = Convert.ToBoolean(o1);
		Boolean bool2 = Convert.ToBoolean(o2);
	    
		return Convert.ToInt16(bool1) / Convert.ToInt16(bool2);
	    
	    } else if ((o1 is String) || (o2 is String)) {
		// one is String, its a numeric Add
	    
		String s1 = Convert.ToString(o1);
		String s2 = Convert.ToString(o2);
	    
		if ((s1 == null) || (s1.Length == 0)) { s1 = "0"; }
		if ((s2 == null) || (s2.Length == 0)) { s2 = "0"; }
	    
		Double dbl1 = Convert.ToDouble(s1);
		Double dbl2 = Convert.ToDouble(s2);
	    
		return dbl1 / dbl2;
	    } else {// Not implemented case
		throw new Exception("Implement me: " + o1.GetType().Name + " " + o2.GetType().Name);
	    }
	}
    }

    private static object CreateNullObjectType(Object otype) {
	if (otype is Byte) {
	    return Convert.ToByte(0);
	} else if (otype is Boolean) {
	    return Convert.ToBoolean(false);
	} else if (otype is Int64) {
	    return Convert.ToInt64(0);
	} else if (otype is Decimal) {
	    return Convert.ToDecimal(0);
	} else if (otype is short) {
	    return Convert.ToInt16(0);
	} else if (otype is int) {
	    return Convert.ToInt32(0);
	} else if (otype is Double) {
	    return Convert.ToDouble(0);
	} else if (otype is Single) {
	    return Convert.ToSingle(0);
	} else if (otype is String) {
	    return Convert.ToString("");
	} else if (otype is Char) {
	    return Convert.ToChar(0);
	    //} else if (otype is Date) {
	    //return null;
	} else {
	    throw new Exception("Implement me: " + otype.GetType().Name);
	}
    }
}
