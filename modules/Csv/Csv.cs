using System;
using System.Collections;
using System.IO;
using System.Text;

namespace Csv {

  /// <summary>
  /// A data-reader style interface for reading Csv (and
  /// otherwise-char-separated) files.
  /// </summary>
  public class reader : IDisposable {

    #region Private variables

    private Stream stream;
    private StreamReader _reader;
    private char separator;

    #endregion

    #region Constructors

    /// <summary>
    /// Creates a new Csv reader for the given stream.
    /// </summary>
    /// <param name="s">The stream to read the CSV from.</param>
    public reader(Stream s) : this(s, null, ',') { }

    /// <summary>
    /// Creates a new reader for the given stream and separator.
    /// </summary>
    /// <param name="s">The stream to read the separator from.</param>
    /// <param name="separator">The field separator character</param>
    public reader(Stream s, char separator) : this(s, null, separator) { }

    /// <summary>
    /// Creates a new Csv reader for the given stream and encoding.
    /// </summary>
    /// <param name="s">The stream to read the CSV from.</param>
    /// <param name="enc">The encoding used.</param>
    public reader(Stream s, Encoding enc) : this(s, enc, ',') { }

    /// <summary>
    /// Creates a new reader for the given stream, encoding and separator character.
    /// </summary>
    /// <param name="s">The stream to read the data from.</param>
    /// <param name="enc">The encoding used.</param>
    /// <param name="separator">The separator character between the fields</param>
    public reader(Stream s, Encoding enc, char separator) {

      this.separator = separator;
      this.stream = s;
      if (!s.CanRead) {
        throw new readerException("Could not read the given data stream!");
      }
      _reader = (enc != null) ? new StreamReader(s, enc) : new StreamReader(s);
    }

    /// <summary>
    /// Creates a new Csv reader for the given text file path.
    /// </summary>
    /// <param name="filename">The name of the file to be read.</param>
    public reader(string filename) : this(filename, null, ',') { }

    /// <summary>
    /// Creates a new reader for the given text file path and
    /// separator character.
    /// </summary>
    /// <param name="filename">The name of the file to be read.</param>
    /// <param name="separator">The field separator character</param>
    public reader(string filename, char separator) : this(filename, null, separator) { }

    /// <summary>
    /// Creates a new Csv reader for the given text file path and encoding.
    /// </summary>
    /// <param name="filename">The name of the file to be read.</param>
    /// <param name="enc">The encoding used.</param>
    public reader(string filename, Encoding enc) 
      : this(filename, enc, ',') { }

    /// <summary>
    /// Creates a new reader for the given text file path, encoding
    /// and field separator.
    /// </summary>
    /// <param name="filename">The name of the file to be read.</param>
    /// <param name="enc">The encoding used.</param>
    /// <param name="separator">The field separator character.</param>
    public reader(string filename, Encoding enc, char separator) 
      : this(new FileStream(filename, FileMode.Open), enc, separator) { }

    #endregion

    #region Properties

    /// <summary>
    /// The separator character for the fields. Comma for normal CSV.
    /// </summary>
    public char Separator {
      get { return separator; }
      set { separator = value; }
    }

    #endregion

    #region Parsing

    /// <summary>
    /// Returns the fields for the next row of data (or null if at eof)
    /// </summary>
    /// <returns>A string array of fields or null if at the end of file.</returns>
    public string[] getLine() {

      string data = _reader.ReadLine();
      if (data == null) return null;
      if (data.Length == 0) return new string[0];
      
      ArrayList result = new ArrayList();

      ParseFields(result, data);
      
      return (string[])result.ToArray(typeof(string));
    }

    // Parses the fields and pushes the fields into the result arraylist
    private void ParseFields(ArrayList result, string data) {

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

    #endregion


    /// <summary>
    /// Disposes the reader. The underlying stream is closed.
    /// </summary>
    public void Dispose() {
      // Closing the reader closes the underlying stream, too
      if (_reader != null) _reader.Close();
      else if (stream != null)
        stream.Close(); // In case we failed before the reader was constructed
      GC.SuppressFinalize(this);
    }
  }


  /// <summary>
  /// Exception class for reader exceptions.
  /// </summary>
  [Serializable]
  public class readerException : ApplicationException { 

    /// <summary>
    /// Constructs a new readerException.
    /// </summary>
    public readerException() : this("The CSV Reader encountered an error.") { }

    /// <summary>
    /// Constructs a new exception with the given message.
    /// </summary>
    /// <param name="message">The exception message.</param>
    public readerException(string message) : base(message) { }

    /// <summary>
    /// Constructs a new exception with the given message and the inner exception.
    /// </summary>
    /// <param name="message">The exception message.</param>
    /// <param name="inner">Inner exception that caused this issue.</param>
    public readerException(string message, Exception inner) : base(message, inner) { }

    /// <summary>
    /// Constructs a new exception with the given serialization information.
    /// </summary>
    /// <param name="info"></param>
    /// <param name="context"></param>
    protected readerException(System.Runtime.Serialization.SerializationInfo info, 
                                 System.Runtime.Serialization.StreamingContext context) 
      : base(info, context) { }
   
  }


  /// <summary>
  /// A tool class for writing Csv and other char-separated field files.
  /// </summary>
  public class writer : StreamWriter {

    #region Private variables

    private char separator;

    #endregion

    #region Constructors

    /// <summary>
    /// Creates a new Csv writer for the given filename (overwriting existing contents).
    /// </summary>
    /// <param name="filename">The name of the file being written to.</param>
    public writer(string filename) 
      : this(filename, ',', false) { }

    /// <summary>
    /// Creates a new Csv writer for the given filename.
    /// </summary>
    /// <param name="filename">The name of the file being written to.</param>
    /// <param name="append">True if the contents shall be appended to the
    /// end of the possibly existing file.</param>
    public writer(string filename, bool append) 
      : this(filename, ',', append) { }

    /// <summary>
    /// Creates a new Csv writer for the given filename and encoding.
    /// </summary>
    /// <param name="filename">The name of the file being written to.</param>
    /// <param name="enc">The encoding used.</param>
    /// <param name="append">True if the contents shall be appended to the
    /// end of the possibly existing file.</param>
    public writer(string filename, Encoding enc, bool append) 
      : this(filename, enc, ',', append) { }

    /// <summary>
    /// Creates a new writer for the given filename and separator.
    /// </summary>
    /// <param name="filename">The name of the file being written to.</param>
    /// <param name="separator">The field separator character used.</param>
    /// <param name="append">True if the contents shall be appended to the
    /// end of the possibly existing file.</param>
    public writer(string filename, char separator, bool append) 
      : base(filename, append) { this.separator = separator; }

    /// <summary>
    /// Creates a new writer for the given filename, separator and encoding.
    /// </summary>
    /// <param name="filename">The name of the file being written to.</param>
    /// <param name="enc">The encoding used.</param>
    /// <param name="separator">The field separator character used.</param>
    /// <param name="append">True if the contents shall be appended to the
    /// end of the possibly existing file.</param>
    public writer(string filename, Encoding enc, char separator, bool append) 
      : base(filename, append, enc) { this.separator = separator; }

    /// <summary>
    /// Creates a new Csv writer for the given stream.
    /// </summary>
    /// <param name="s">The stream to write the CSV to.</param>
    public writer(Stream s) 
      : this(s, ',') { }

    /// <summary>
    /// Creates a new writer for the given stream and separator character.
    /// </summary>
    /// <param name="s">The stream to write the CSV to.</param>
    /// <param name="separator">The field separator character used.</param>
    public writer(Stream s, char separator) 
      : base(s) { this.separator = separator; }

    /// <summary>
    /// Creates a new writer for the given stream, separator and encoding.
    /// </summary>
    /// <param name="s">The stream to write the CSV to.</param>
    /// <param name="enc">The encoding used.</param>
    /// <param name="separator">The field separator character used.</param>
    public writer(Stream s, Encoding enc, char separator) 
      : base(s, enc) { this.separator = separator; }

    #endregion

    #region Properties

    /// <summary>
    /// The separator character for the fields. Comma for normal CSV.
    /// </summary>
    public char Separator {
      get { return separator; }
      set { separator = value; }
    }

    #endregion

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
