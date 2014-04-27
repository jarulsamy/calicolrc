
using System;
using System.IO;
using System.Collections; // 
using System.Collections.Generic; // IDictionary
using System.Net;

namespace Calico {
    
    public class Representation {
	
	public Representation() {
	}
	
	public virtual IDictionary<string, string> GetRepresentations() {
	    return new Dictionary<string, string>();
	}

	public static AudioRepresentation Audio(string filename) {
	    return new AudioRepresentation(filename);
	}
	
	public static MimeRepresentation HTML(string text) {
	    return new MimeRepresentation("text/html", text, 
					  "text/plain", "<HTML viewable in notebook>");
	}
	
	public static MimeRepresentation SVG(string text) {
	    return new MimeRepresentation("image/svg+xml", text, 
					  "text/plain", "<SVG viewable in notebook>");
	}
	
	public static MimeRepresentation JSON(string text) {
	    return new MimeRepresentation("application/json", text, 
					  "text/plain", "<JSON viewable in notebook>");
	}
	
	public static MimeRepresentation Latex(string text) {
	    return new MimeRepresentation("text/latex", text, 
					  "text/plain", "<Latex viewable in notebook>");
	}
	
	public static MimeRepresentation Math(string text) {
	    return new MimeRepresentation("text/latex", "$$" + text + "$$", 
					  "text/plain", "<Math viewable in notebook>");
	}
	
	public static MimeRepresentation Javascript(string text) {
	    return new MimeRepresentation("application/javascript", text,
					  "text/plain", "<JavaScript viewable in executing notebook>");
	}
	
	public static MimeRepresentation FileLink(string filename) {
	    return new MimeRepresentation("text/html", 
					  String.Format("<a href=\"{0}\" target=\"_blank\">{0}</a>", filename),
					  "text/plain", String.Format("'{0}'", filename));
	}
	
	public static MimeRepresentation FileLinks(string directory) {
	    string html_retval = "";
	    string text_retval = "";
	    DirectoryInfo dir = new DirectoryInfo(directory);
	    // FIXME: traverse subdirectories, all the way down:
	    foreach (FileInfo file in dir.GetFiles()) {
		if (html_retval != "")
		    html_retval += "<br/>";
		if (text_retval != "")
		    html_retval += "\n";
		html_retval += String.Format("<a href=\"{0}\" target=\"_blank\">{0}</a>", file.Name);
		text_retval += String.Format("'{0}'", file.Name);
	    }
	    return new MimeRepresentation("text/html", html_retval,
					  "text/plain", text_retval);
	}
	
	public static MimeRepresentation YouTubeVideo(string id) {
	    return new MimeRepresentation("text/html",  
					  String.Format("<iframe width=\"400\" height=\"300\" " + 
							"src=\"http://www.youtube.com/embed/{0}\" " +
							"frameborder=\"0\" allowfullscreen=\"\"></iframe>", id),
					  "text/plain", "<YouTubeVideo viewable in executing notebook>");
	}
	
	public static MimeRepresentation VimeoVideo(string id) {
	    return new MimeRepresentation("text/html",  
					  String.Format("<iframe width=\"400\" height=\"300\" " + 
							"src=\"https:/player.vimeo.com/video/{0}\" " +
							"frameborder=\"0\" allowfullscreen=\"\"></iframe>", id),
					  "text/plain", "<VimeoVideo viewable in executing notebook>");
	}
	
	public static MimeRepresentation IFrame(string url, int width, int height) {
	    return new MimeRepresentation("text/html",  
					  String.Format("<iframe width=\"{0}\" height=\"{1}\" " + 
							"src=\"{2}\" " +
							"frameborder=\"0\" allowfullscreen=\"\"></iframe>", 
							width, height, url),
					  "text/plain", "<IFrame viewable in executing notebook>");
	}
	
	public static MimeRepresentation Javascript(string text, string lib) {
	    return new MimeRepresentation("application/javascript",  
					  String.Format("require(['{0}'], function () {{\n{1}\n}});\n", lib, text),
					  "text/plain", "<JavaScript viewable in executing notebook>");
	}
	
	public static ImageRepresentation Image(string filename) {
	    return new ImageRepresentation(filename);
	}
	
	public static MimeRepresentation Table(IList<IList> list) {
	    string text = "<table border=\"1\">";
	    int count = 1;
	    text += "<tr>";
	    foreach (IList cols in list) {
		foreach (object col in cols) {
		    text += String.Format("<th>Item{0}</th>", count);
		    count += 1;
		}
		break;
	    }
	    text += "</tr>";
	    foreach (IList cols in list) {
		text += "<tr>";
		foreach (object col in cols) {
		    text += String.Format("<td>{0}</td>", col);
		}
		text += "</tr>";
	    }
	    text += "</table>";
	    return new MimeRepresentation("text/html", text, 
					  "text/plain", "<Table>");
	}

	public override string ToString() {
	    IDictionary<string,string> repr = this.GetRepresentations();
	    if (repr.ContainsKey("image/svg+xml")) {
		return repr["image/svg+xml"];
	    } else if (repr.ContainsKey("text/html")) {
		return repr["text/plain"];
	    } else if (repr.ContainsKey("text/plain")) {
		return repr["text/plain"];
	    } else {
		return "<Representation>";
	    }
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
		if (System.IO.File.Exists(filename)) {
		    buffer = File.ReadAllBytes(filename);
		} else {
		    var retval1 = new Dictionary<string, string>();
		    retval1["text/plain"] = String.Format("File does not exist: '{0}'", filename);
		    return retval1;
		}
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
            Gdk.Pixbuf pixbuf = null;
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
