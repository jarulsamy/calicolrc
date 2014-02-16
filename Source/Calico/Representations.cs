
using System;
using System.IO;
using System.Collections.Generic; // IDictionary
using System.Net;

namespace Calico {
    
    public class Representation {
	
	public Representation() {
	}
	
	public virtual IDictionary<string, string> GetRepresentations() {
	    return new Dictionary<string, string>();
	}
    }

    public class AudioRepresentation : Representation {
	public string filename;
	public bool autoplay;

	public AudioRepresentation(string filename) {
	    this.filename = filename;
	    this.autoplay = false;
	}

	public AudioRepresentation(string filename, bool autoplay) {
	    this.filename = filename;
	    this.autoplay = autoplay;
	}

	public static byte[] ReadToEnd(System.IO.Stream stream) {
	    long originalPosition = 0;
	    
	    if (stream.CanSeek) {
		originalPosition = stream.Position;
		stream.Position = 0;
	    }
	    try {
		byte[] readBuffer = new byte[4096];
		
		int totalBytesRead = 0;
		int bytesRead;
		
		while ((bytesRead = stream.Read(readBuffer, totalBytesRead, readBuffer.Length - totalBytesRead)) > 0) {
		    totalBytesRead += bytesRead;
		    if (totalBytesRead == readBuffer.Length) {
			int nextByte = stream.ReadByte();
			if (nextByte != -1) {
			    byte[] temp = new byte[readBuffer.Length * 2];
			    Buffer.BlockCopy(readBuffer, 0, temp, 0, readBuffer.Length);
			    Buffer.SetByte(temp, totalBytesRead, (byte)nextByte);
			    readBuffer = temp;
			    totalBytesRead++;
			}
		    }
		}
		byte[] buffer = readBuffer;
		if (readBuffer.Length != totalBytesRead) {
		    buffer = new byte[totalBytesRead];
		    Buffer.BlockCopy(readBuffer, 0, buffer, 0, totalBytesRead);
		}
		return buffer;
	    } finally {
		if (stream.CanSeek) {
		    stream.Position = originalPosition; 
		}
	    }
	}
	
	public override IDictionary<string, string> GetRepresentations() {
	    byte [] buffer = new byte[0];
	    if (filename.StartsWith ("http://")) {
		try {
		    HttpWebRequest req = (HttpWebRequest)WebRequest.Create (filename);
		    req.KeepAlive = false;
		    req.Timeout = 10000;        
		    WebResponse resp = req.GetResponse ();
		    Stream s = resp.GetResponseStream ();
		    buffer = ReadToEnd(s);
		} catch (Exception e) {
		    System.Console.Error.WriteLine(e.ToString());
		}
	    } else {
		buffer = File.ReadAllBytes(filename);
	    }
	    string autoplay_text = (autoplay ? "autoplay=\"autoplay\"" : "");
	    string encodestring = System.Convert.ToBase64String(buffer, 0, buffer.Length);
	    string data = String.Format("data:audio/wav;base64,{0}", encodestring);
	    string control = String.Format("<audio controls=\"controls\" {0}>" +
					   "<source controls src=\"{1}\" type=\"audio/wav\" />" +
					   "Your browser does not support the audio element." +
					   "</audio>", autoplay_text, data);
	    var retval = new Dictionary<string, string>();
	    retval["text/html"] = control;
	    retval["text/plain"] = "<HTML>";
	    return retval;
	}
	
    }

    public class MimeRepresentation : Representation {
	string [] args;
	
	public MimeRepresentation(params string [] args) {
	    this.args = args;
	}
	
	public override IDictionary<string, string> GetRepresentations() {
	    var retval = new Dictionary<string, string>();
	    for (int i = 0; i < args.Length; i++) {
		retval[args[i]] = args[i+1];
		i++;
	    }
	    return retval;
	}
    }
    
    public class DictRepresentation : Representation {
	public IDictionary<string, string> dict;
	
	public DictRepresentation(IDictionary<string, string> dict) {
	    this.dict = dict;
	}
	
	public override IDictionary<string, string> GetRepresentations() {
	    return dict;
	} 
	
	public override string ToString() {
	    if (dict.ContainsKey("text/plain")) {
		return dict["text/plain"];
	    } else {
		return "<indescribable object>";
	    }
	} 
    }
    
    public class ImageRepresentation : Representation {
	string filename;
	
	public ImageRepresentation(string filename) {
	    this.filename = filename;
	}
	
	public override IDictionary<string, string> GetRepresentations() {
	    Gdk.Pixbuf pixbuf;
	    byte [] buffer;
	    string png_string;
	    string jpg_string;
	    if (filename.StartsWith ("http://")) {
		try {
		    HttpWebRequest req = (HttpWebRequest)WebRequest.Create (filename);
		    req.KeepAlive = false;
		    req.Timeout = 10000;        
		    WebResponse resp = req.GetResponse ();
		    Stream s = resp.GetResponseStream ();
		    pixbuf = new Gdk.Pixbuf (s);
		} catch (Exception e) {
		    System.Console.Error.WriteLine(e.ToString());
		}
	    } else {
		pixbuf = new Gdk.Pixbuf (filename);
	    }
	    
	    try {
		buffer = pixbuf.SaveToBuffer("png");
		png_string = System.Convert.ToBase64String(buffer, 0, buffer.Length);
	    } catch {
		png_string = "";
	    }
	    try {
		buffer = pixbuf.SaveToBuffer("jpeg");
		jpg_string = System.Convert.ToBase64String(buffer, 0, buffer.Length);
	    } catch {
		jpg_string = "";
	    }
	    var retval = new Dictionary<string, string>();
	    retval["text/plain"] =  String.Format("<Image: \"{0}\">", filename);
	    if (png_string != "")
		retval["image/png"] = png_string;
	    if (jpg_string != "")
		retval["image/jpeg"] = jpg_string;
	    return retval;
	}
    }
}
