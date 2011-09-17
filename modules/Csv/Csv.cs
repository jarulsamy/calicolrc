using System;
using System.IO;
using System.Collections; // IEnumerable
using System.Text; // Encoding
using IronPython.Runtime; // Operations, List, Tuple, Dict, ...

namespace Csv {

  public class reader : IDisposable, IEnumerable {

    private Stream stream;
    private StreamReader _reader;
    private char separator;

    public reader(Stream s) : this(s, null, ',') { }

    public reader(Stream s, char separator) : this(s, null, separator) { }

    public reader(Stream s, Encoding enc) : this(s, enc, ',') { }

    public reader(Stream s, Encoding enc, char separator) {

      this.separator = separator;
      this.stream = s;
      if (!s.CanRead) {
        throw new readerException("Could not read the given data stream!");
      }
      _reader = (enc != null) ? new StreamReader(s, enc) : new StreamReader(s);
    }

    public reader(string filename) : this(filename, null, ',') { }

    public reader(string filename, char separator) : this(filename, null, separator) { }

    public reader(string filename, Encoding enc) 
      : this(filename, enc, ',') { }

    public reader(string filename, Encoding enc, char separator) 
      : this(new FileStream(filename, FileMode.Open), enc, separator) { }

    public char Separator {
      get { return separator; }
      set { separator = value; }
    }

    public IEnumerator GetEnumerator() {
      while (! _reader.EndOfStream) {
	yield return readLine();
      }
      close();
    }

    public List readLines() {
      List retval = new List();
      List line;
      while (! _reader.EndOfStream) {
	line = readLine();
	retval.Add(line);
      }
      close();
      return retval;
    }

    public List readLine() {
      string data = _reader.ReadLine();
      List result = new List();

      if (data == null) return null;
      if (data.Length == 0) return result;
      
      ParseFields(result, data);
      
      return result;
    }

    // Parses the fields and pushes the fields into the result arraylist
    private void ParseFields(List result, string data) {

      int pos = -1;
      while (pos < data.Length)
        result.Add(ParseField(data, ref pos));
    }

    // Parses the field at the given position of the data, modified pos to match
    // the first unparsed position and returns the parsed field
    private string ParseField(string data, ref int startSeparatorPosition) {

      if (startSeparatorPosition == data.Length-1) {
        startSeparatorPosition++;
        // The last field is empty
        return "";
      }

      int fromPos = startSeparatorPosition + 1;

      // Determine if this is a quoted field
      if (data[fromPos] == '"') {
        // If we're at the end of the string, let's consider this a field that
        // only contains the quote
        if (fromPos == data.Length-1) {
          fromPos++;
          return "\"";
        }

        // Otherwise, return a string of appropriate length with double quotes collapsed
        // Note that FSQ returns data.Length if no single quote was found
        int nextSingleQuote = FindSingleQuote(data, fromPos+1);
        startSeparatorPosition = nextSingleQuote+1;
        return data.Substring(fromPos+1, nextSingleQuote-fromPos-1).Replace("\"\"", "\"");
      }

      // The field ends in the next separator or EOL
      int nextSeparator = data.IndexOf(separator, fromPos);
      if (nextSeparator == -1) {
        startSeparatorPosition = data.Length;
        return data.Substring(fromPos);
      }
      else {
        startSeparatorPosition = nextSeparator;
        return data.Substring(fromPos, nextSeparator - fromPos);
      }
    }

    // Returns the index of the next single quote mark in the string 
    // (starting from startFrom)
    private static int FindSingleQuote(string data, int startFrom) {

      int i = startFrom-1;
      while (++i < data.Length)
        if (data[i] == '"') {
          // If this is a double quote, bypass the chars
          if (i < data.Length-1 && data[i+1] == '"') {
            i++;
            continue;
          }
          else
            return i;
        }
      // If no quote found, return the end value of i (data.Length)
      return i;
    }

    public void close() {
      // Closing the reader closes the underlying stream, too
      if (_reader != null) 
	_reader.Close();
      else if (stream != null)
        stream.Close(); // In case we failed before the reader was constructed
    }

    public void Dispose() {
      close();
      GC.SuppressFinalize(this);
    }
  }

  [Serializable]
  public class readerException : ApplicationException { 

    public readerException() : this("The CSV Reader encountered an error.") { }

    public readerException(string message) : base(message) { }

    public readerException(string message, Exception inner) : base(message, inner) { }

    protected readerException(System.Runtime.Serialization.SerializationInfo info, 
                                 System.Runtime.Serialization.StreamingContext context) 
      : base(info, context) { }
   
  }


  public class writer : StreamWriter {

    private char separator;

    public writer(string filename) 
      : this(filename, ',', false) { }

    public writer(string filename, bool append) 
      : this(filename, ',', append) { }

    public writer(string filename, Encoding enc, bool append) 
      : this(filename, enc, ',', append) { }

    public writer(string filename, char separator, bool append) 
      : base(filename, append) { this.separator = separator; }

    public writer(string filename, Encoding enc, char separator, bool append) 
      : base(filename, append, enc) { this.separator = separator; }

    public writer(Stream s) 
      : this(s, ',') { }

    public writer(Stream s, char separator) 
      : base(s) { this.separator = separator; }

    public writer(Stream s, Encoding enc, char separator) 
      : base(s, enc) { this.separator = separator; }

    public char Separator {
      get { return separator; }
      set { separator = value; }
    }

    public void WriteFields(params object[] content) {
      string s;
      for (int i = 0; i < content.Length; ++i) {
        s = (content[i] != null ? content[i].ToString() : "");
        if (s.IndexOfAny(new char[] { Separator, '"' }) >= 0)
          // We have to quote the string
          s = "\"" + s.Replace("\"", "\"\"") + "\"";
        Write(s);

        // Write the separator unless we're at the last position
        if (i < content.Length-1)
          Write(separator);
      }
      Write(NewLine);
    }
  }
}
