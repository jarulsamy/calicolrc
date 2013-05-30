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
  
  public static void set(object structure, object position, object value)
  {
      if (structure is string[]) {
	  if (position is int) {
	      int pos = (int)position;
	      (structure as string[])[pos] = (string)value;
	  } else {
	      throw new System.Exception("position must be an integer");
	  }	  
      } else if (structure is int[]) {
	  if (position is int) {
	      int pos = (int)position;
	      (structure as int[])[pos] = (int)value;
	  } else {
	      throw new System.Exception("position must be an integer");
	  }
      } else if (structure is double[]) {
	  if (position is int) {
	      int pos = (int)position;
	      (structure as double[])[pos] = (double)value;
	  } else {
	      throw new System.Exception("position must be an integer");
	  }
      } else if (structure is IronPython.Runtime.SetCollection) {
	  throw new System.Exception("Set is unscriptable");
      } else if (structure is IronPython.Runtime.List) {
	  if (position is int) {
	      int pos = (int)position;
	      (structure as IronPython.Runtime.List)[pos] = value;
	  } else {
	      throw new System.Exception("position must be an integer");
	  }
      } else if (structure is IronPython.Runtime.PythonTuple) {
	  throw new System.Exception("Tuple is readonly");
      } else if (structure is IronPython.Runtime.PythonDictionary) {
	  (structure as IronPython.Runtime.PythonDictionary)[position] = value;
      } else {
	  throw new System.Exception("Invalid collection type");
      } 
  }

  public static object get(object structure, object position)
  {
      if (structure is string[]) {
	  if (position is int) {
	      int pos = (int)position;
	      return (structure as string[])[pos];
	  } else {
	      throw new System.Exception("position must be an integer");
	  }
      } else if (structure is int[]) {
	  if (position is int) {
	      int pos = (int)position;
	      return (structure as int[])[pos];
	  } else {
	      throw new System.Exception("position must be an integer");
	  }
      } else if (structure is double[]) {
	  if (position is int) {
	      int pos = (int)position;
	      return (structure as double[])[pos];
	  } else {
	      throw new System.Exception("position must be an integer");
	  }
      } else if (structure is IronPython.Runtime.SetCollection) {
	  throw new System.Exception("Set is unscriptable");
      } else if (structure is IronPython.Runtime.List) {
	  if (position is int) {
	      int pos = (int)position;
	      return (structure as IronPython.Runtime.List)[pos];
	  } else {
	      throw new System.Exception("position must be an integer");
	  }
      } else if (structure is IronPython.Runtime.PythonTuple) {
	  if (position is int) {
	      int pos = (int)position;
	      return (structure as IronPython.Runtime.PythonTuple)[pos];
	  } else {
	      throw new System.Exception("position must be an integer");
	  }
      } else if (structure is IronPython.Runtime.PythonDictionary) {
	  return (structure as IronPython.Runtime.PythonDictionary)[position];
      } else {
	  throw new System.Exception("Invalid collection type");
      } 
  }
}