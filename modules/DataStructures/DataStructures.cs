// Data Structures Items for Calico

public static class DataStructures
{
  public static IronPython.Runtime.PythonTuple Tuple(params object [] items)
  {
	return new IronPython.Runtime.PythonTuple(items);
  }
  
  public static IronPython.Runtime.List List(params object [] items)
  {
	var list = new IronPython.Runtime.List();
	foreach (object item in items) {
	  list.append(item);
	}
	return list;
  }
  
  public static IronPython.Runtime.SetCollection Set(params object [] items)
  {
	var s = new IronPython.Runtime.SetCollection();
	foreach (object item in items) {
	  s.add(item);
	}
	return s;
  }
  
  public static IronPython.Runtime.PythonDictionary Dictionary()
  {
	return new IronPython.Runtime.PythonDictionary();
  }

  public static object Array(string type="integer", string spreadsheet="Spreadsheet.csv")
  {
	CalicoSpreadsheetDocument document = new CalicoSpreadsheetDocument(null, spreadsheet);
	int size = 0;
	string which = "row";
	if (document.MaxCols > document.MaxRows) {
	  size = document.MaxCols;
	  which = "cols";
	} else {
	  size = document.MaxRows;
	  which = "rows";
	}
	if (type == "integer") {
	  int[] array = (int[])Array(type, size);
	  for (int i = 0; i < size; i++) {
		if (which == "cols")
		  array[i] = System.Convert.ToInt32(document[i][0]);
		else
		  array[i] = System.Convert.ToInt32(document[0][i]);
	  }
	  return array;
	} else if (type == "double") {
	  double[] array = (double[])Array(type, size);
	  for (int i = 0; i < size; i++) {
		if (which == "cols")
		  array[i] = System.Convert.ToDouble(document[i][0]);
		else
		  array[i] = System.Convert.ToDouble(document[0][i]);
	  }
	  return array;
	} else if (type == "string") {
	  string[] array = (string[])Array(type, size);
	  for (int i = 0; i < size; i++) {
		if (which == "cols")
		  array[i] = (string)document[i][0];
		else
		  array[i] = (string)document[0][i];
	  }
	  return array;
	} else {
	  throw new System.Exception("Unknown array type: should be 'string', 'integer', or 'double'");
	}
  }
  
  public static object Array(string type="integer", int size=10)
  {
	if (type == "string") {
	  return new string[size];
	} else if (type == "int" || type == "integer") {
	  return new int[size];
	} else if (type == "double") {
	  return new double[size];
	} else {
	  throw new System.Exception("Unknown array type: should be 'string', 'integer', or 'double'");
	}
  }
  
  public static void set(object structure, int position, object value)
  {
	if (structure is string[]) {
	  (structure as string[])[position] = (string)value;
	} else if (structure is int[]) {
	  (structure as int[])[position] = (int)value;
	} else if (structure is double[]) {
	  (structure as double[])[position] = (double)value;
	} else {
	  throw new System.Exception("Invalid collection type");
	} 
  }

  public static object get(object structure, int position)
  {
	if (structure is string[]) {
	  return (structure as string[])[position];
	} else if (structure is int[]) {
	  return (structure as int[])[position];
	} else if (structure is double[]) {
	  return (structure as double[])[position];
	} else {
	  throw new System.Exception("Invalid collection type");
	} 
  }

  
}