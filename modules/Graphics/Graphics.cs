
/*
   Calico - Scripting Environment

   Copyright (c) 2011, Doug Blank <dblank@cs.brynmawr.edu>

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   $Id: $
   */
using IronPython.Runtime;
// Operations, List, Tuple, Dict, ...
using System.Runtime.InteropServices;
// Marshal
using System.Collections.Generic;
using System.Collections;
// IEnumerable
using System.Threading;
using System.Drawing;
// Image, Bitmap
using System.Net;
// WebRequest
using System.IO;
// MemoryStream
using System;
using Microsoft.Xna.Framework; // Vector2, Matrix
using System.Diagnostics;
// Font:
using Cairo;

// Animated Gifs:
using System.Drawing.Imaging;

// Extensions:
using System.ComponentModel;

// Events
using EventsManager=Events;

public static class Extensions
{
    // Can't use extension methods to override existing methods
    public static string MyString<T> (this T[] array)
    {
        string retval = "";
        foreach( object item in array) {
            if (retval != "")
                retval += ", ";
            retval += item.ToString();
        }
        return "[" + retval + "]";
    }

    public static System.Drawing.Bitmap ToBitmap(this Gdk.Pixbuf pix, params object [] args) {
        Dictionary<string,System.Drawing.Imaging.PixelFormat> formats = new Dictionary<string,System.Drawing.Imaging.PixelFormat>();
        formats["Alpha"] = System.Drawing.Imaging.PixelFormat.Alpha;
        formats["Canonical"] = System.Drawing.Imaging.PixelFormat.Canonical;
        formats["DontCare"] = System.Drawing.Imaging.PixelFormat.DontCare;
        formats["Extended"] = System.Drawing.Imaging.PixelFormat.Extended;
        formats["Format16bppArgb1555"] = System.Drawing.Imaging.PixelFormat.Format16bppArgb1555;
        formats["Format16bppGrayScale"] = System.Drawing.Imaging.PixelFormat.Format16bppGrayScale;
        formats["Format16bppRgb555"] = System.Drawing.Imaging.PixelFormat.Format16bppRgb555;
        formats["Format16bppRgb565"] = System.Drawing.Imaging.PixelFormat.Format16bppRgb565;
        formats["Format1bppIndexed"] = System.Drawing.Imaging.PixelFormat.Format1bppIndexed;
        formats["Format24bppRgb"] = System.Drawing.Imaging.PixelFormat.Format24bppRgb;
        formats["Format32bppArgb"] = System.Drawing.Imaging.PixelFormat.Format32bppArgb;
        formats["Format32bppPArgb"] = System.Drawing.Imaging.PixelFormat.Format32bppPArgb;
        formats["Format32bppRgb"] = System.Drawing.Imaging.PixelFormat.Format32bppRgb;
        formats["Format48bppRgb"] = System.Drawing.Imaging.PixelFormat.Format48bppRgb;
        formats["Format4bppIndexed"] = System.Drawing.Imaging.PixelFormat.Format4bppIndexed;
        formats["Format64bppArgb"] = System.Drawing.Imaging.PixelFormat.Format64bppArgb;
        formats["Format64bppPArgb"] = System.Drawing.Imaging.PixelFormat.Format64bppPArgb;
        formats["Format8bppIndexed"] = System.Drawing.Imaging.PixelFormat.Format8bppIndexed;
        formats["Gdi"] = System.Drawing.Imaging.PixelFormat.Gdi;
        formats["Indexed"] = System.Drawing.Imaging.PixelFormat.Indexed;
        formats["Max"] = System.Drawing.Imaging.PixelFormat.Max;
        formats["PAlpha"] = System.Drawing.Imaging.PixelFormat.PAlpha;
        formats["Undefined"] = System.Drawing.Imaging.PixelFormat.Undefined;
        System.Drawing.Imaging.PixelFormat format;
        System.Drawing.Bitmap bitmap;
        if (args.Length > 0) {
            format = formats[args[0].ToString()];
        } else {
            format = formats["Format24bppRgb"];
        }
        bitmap = new System.Drawing.Bitmap(pix.Width, pix.Height, format);
        for (int x=0; x < pix.Width; x++) {
            for (int y=0; y < pix.Height; y++) {
                int r = Marshal.ReadByte (pix.Pixels, y * pix.Rowstride +
                        x * pix.NChannels + 0);
                int g = Marshal.ReadByte (pix.Pixels, y * pix.Rowstride +
                        x * pix.NChannels + 1);
                int b = Marshal.ReadByte (pix.Pixels, y * pix.Rowstride +
                        x * pix.NChannels + 2);
                int a = Marshal.ReadByte (pix.Pixels, y * pix.Rowstride +
                        x * pix.NChannels + 3);
                System.Drawing.Color color = System.Drawing.Color.FromArgb(a, r, g, b);
                bitmap.SetPixel(x, y, color);
            }
        }
        return bitmap;
    }
}

public static class Graphics
{
    public static int gui_thread_id = -1;
    public static string startup_path;
    public static string os_name;
    public static bool warn_missing_dot = false;
    public delegate void InvokeDelegate ();

    [method: JigsawTab(null)]
    public static void initialize_module(string path, string os) {
        Graphics.startup_path = path;
        Graphics.os_name = os;
    }


    [method: JigsawTab(null)]
    public static void InvokeBlocking (InvokeDelegate invoke)
    {
        System.Exception exception = null;
        ManualResetEvent ev = new ManualResetEvent(false);
        if (needInvoke ()) {
            Gtk.Application.Invoke (delegate {
                try {
                    invoke ();
                } catch (Exception e) {
                    exception = e;
                }
                ev.Set();
            });
            ev.WaitOne();
        } else {
            try {
                invoke ();
            } catch (Exception e) {
                exception = e;
            }
        }
        if (exception != null)
            throw exception;
    }

    [method: JigsawTab(null)]
    public static void Invoke (InvokeDelegate invoke)
    {
        System.Exception exception = null;
        if (needInvoke ()) {
            Gtk.Application.Invoke (delegate {
                try {
                    invoke ();
                } catch (Exception e) {
                    // not waiting for result
                    System.Console.Error.WriteLine(e);
                }
            });
        } else {
            try {
                invoke ();
            } catch (Exception e) {
                exception = e;
            }
        }
        if (exception != null)
            throw exception;
    }

    [method: JigsawTab(null)]
    public static bool needInvoke ()
    {
        return ((Thread.CurrentThread.ManagedThreadId != gui_thread_id) &&
                (gui_thread_id != -1));
    }

    [method: JigsawTab(null)]
    public static void set_gui_thread_id (int gui_thread_id)
    {
        Graphics.gui_thread_id = gui_thread_id;
    }

    private static WindowClass _lastWindow = null;
    public static readonly Dictionary<string,Color> colors = 
        new Dictionary<string,Color> () {
            {"black", new Color(0x00, 0x00, 0x00)}, 
            {"navy", new Color(0x00, 0x00, 0x80)}, 
            {"darkblue", new Color(0x00, 0x00, 0x8B)}, 
            {"mediumblue", new Color(0x00, 0x00, 0xCD)}, 
            {"blue", new Color(0x00, 0x00, 0xFF)}, 
            {"darkgreen", new Color(0x00, 0x64, 0x00)}, 
            {"green", new Color(0x00, 0x80, 0x00)}, 
            {"teal", new Color(0x00, 0x80, 0x80)}, 
            {"darkcyan", new Color(0x00, 0x8B, 0x8B)}, 
            {"deepskyblue", new Color(0x00, 0xBF, 0xFF)}, 
            {"darkturquoise", new Color(0x00, 0xCE, 0xD1)}, 
            {"mediumspringgreen", new Color(0x00, 0xFA, 0x9A)}, 
            {"lime", new Color(0x00, 0xFF, 0x00)}, 
            {"springgreen", new Color(0x00, 0xFF, 0x7F)}, 
            {"aqua", new Color(0x00, 0xFF, 0xFF)}, 
            {"cyan", new Color(0x00, 0xFF, 0xFF)}, 
            {"midnightblue", new Color(0x19, 0x19, 0x70)}, 
            {"dodgerblue", new Color(0x1E, 0x90, 0xFF)}, 
            {"lightseagreen", new Color(0x20, 0xB2, 0xAA)}, 
            {"forestgreen", new Color(0x22, 0x8B, 0x22)}, 
            {"seagreen", new Color(0x2E, 0x8B, 0x57)}, 
            {"darkslategray", new Color(0x2F, 0x4F, 0x4F)}, 
            {"darkslategrey", new Color(0x2F, 0x4F, 0x4F)}, 
            {"limegreen", new Color(0x32, 0xCD, 0x32)}, 
            {"mediumseagreen", new Color(0x3C, 0xB3, 0x71)}, 
            {"turquoise", new Color(0x40, 0xE0, 0xD0)}, 
            {"royalblue", new Color(0x41, 0x69, 0xE1)}, 
            {"steelblue", new Color(0x46, 0x82, 0xB4)}, 
            {"darkslateblue", new Color(0x48, 0x3D, 0x8B)}, 
            {"mediumturquoise", new Color(0x48, 0xD1, 0xCC)}, 
            {"indigo", new Color(0x4B, 0x00, 0x82)}, 
            {"darkolivegreen", new Color(0x55, 0x6B, 0x2F)}, 
            {"cadetblue", new Color(0x5F, 0x9E, 0xA0)}, 
            {"cornflowerblue", new Color(0x64, 0x95, 0xED)}, 
            {"mediumaquamarine", new Color(0x66, 0xCD, 0xAA)}, 
            {"dimgray", new Color(0x69, 0x69, 0x69)}, 
            {"dimgrey", new Color(0x69, 0x69, 0x69)}, 
            {"slateblue", new Color(0x6A, 0x5A, 0xCD)}, 
            {"olivedrab", new Color(0x6B, 0x8E, 0x23)}, 
            {"slategray", new Color(0x70, 0x80, 0x90)}, 
            {"slategrey", new Color(0x70, 0x80, 0x90)}, 
            {"lightslategray", new Color(0x77, 0x88, 0x99)}, 
            {"lightslategrey", new Color(0x77, 0x88, 0x99)}, 
            {"mediumslateblue", new Color(0x7B, 0x68, 0xEE)}, 
            {"lawngreen", new Color(0x7C, 0xFC, 0x00)}, 
            {"chartreuse", new Color(0x7F, 0xFF, 0x00)}, 
            {"aquamarine", new Color(0x7F, 0xFF, 0xD4)}, 
            {"maroon", new Color(0x80, 0x00, 0x00)}, 
            {"purple", new Color(0x80, 0x00, 0x80)}, 
            {"olive", new Color(0x80, 0x80, 0x00)}, 
            {"gray", new Color(0x80, 0x80, 0x80)}, 
            {"grey", new Color(0x80, 0x80, 0x80)}, 
            {"skyblue", new Color(0x87, 0xCE, 0xEB)}, 
            {"lightskyblue", new Color(0x87, 0xCE, 0xFA)}, 
            {"blueviolet", new Color(0x8A, 0x2B, 0xE2)}, 
            {"darkred", new Color(0x8B, 0x00, 0x00)}, 
            {"darkmagenta", new Color(0x8B, 0x00, 0x8B)}, 
            {"saddlebrown", new Color(0x8B, 0x45, 0x13)}, 
            {"darkseagreen", new Color(0x8F, 0xBC, 0x8F)}, 
            {"lightgreen", new Color(0x90, 0xEE, 0x90)}, 
            {"mediumpurple", new Color(0x93, 0x70, 0xD8)}, 
            {"darkviolet", new Color(0x94, 0x00, 0xD3)}, 
            {"palegreen", new Color(0x98, 0xFB, 0x98)}, 
            {"darkorchid", new Color(0x99, 0x32, 0xCC)}, 
            {"yellowgreen", new Color(0x9A, 0xCD, 0x32)}, 
            {"sienna", new Color(0xA0, 0x52, 0x2D)}, 
            {"brown", new Color(0xA5, 0x2A, 0x2A)}, 
            {"darkgray", new Color(0xA9, 0xA9, 0xA9)}, 
            {"darkgrey", new Color(0xA9, 0xA9, 0xA9)}, 
            {"lightblue", new Color(0xAD, 0xD8, 0xE6)}, 
            {"greenyellow", new Color(0xAD, 0xFF, 0x2F)}, 
            {"paleturquoise", new Color(0xAF, 0xEE, 0xEE)}, 
            {"lightsteelblue", new Color(0xB0, 0xC4, 0xDE)}, 
            {"powderblue", new Color(0xB0, 0xE0, 0xE6)}, 
            {"firebrick", new Color(0xB2, 0x22, 0x22)}, 
            {"darkgoldenrod", new Color(0xB8, 0x86, 0x0B)}, 
            {"mediumorchid", new Color(0xBA, 0x55, 0xD3)}, 
            {"rosybrown", new Color(0xBC, 0x8F, 0x8F)}, 
            {"darkkhaki", new Color(0xBD, 0xB7, 0x6B)}, 
            {"silver", new Color(0xC0, 0xC0, 0xC0)}, 
            {"mediumvioletred", new Color(0xC7, 0x15, 0x85)}, 
            {"indianred", new Color(0xCD, 0x5C, 0x5C)}, 
            {"peru", new Color(0xCD, 0x85, 0x3F)}, 
            {"chocolate", new Color(0xD2, 0x69, 0x1E)}, 
            {"tan", new Color(0xD2, 0xB4, 0x8C)}, 
            {"lightgray", new Color(0xD3, 0xD3, 0xD3)}, 
            {"lightgrey", new Color(0xD3, 0xD3, 0xD3)}, 
            {"palevioletred", new Color(0xD8, 0x70, 0x93)}, 
            {"thistle", new Color(0xD8, 0xBF, 0xD8)}, 
            {"orchid", new Color(0xDA, 0x70, 0xD6)}, 
            {"goldenrod", new Color(0xDA, 0xA5, 0x20)}, 
            {"crimson", new Color(0xDC, 0x14, 0x3C)}, 
            {"gainsboro", new Color(0xDC, 0xDC, 0xDC)}, 
            {"plum", new Color(0xDD, 0xA0, 0xDD)}, 
            {"burlywood", new Color(0xDE, 0xB8, 0x87)}, 
            {"lightcyan", new Color(0xE0, 0xFF, 0xFF)}, 
            {"lavender", new Color(0xE6, 0xE6, 0xFA)}, 
            {"darksalmon", new Color(0xE9, 0x96, 0x7A)}, 
            {"violet", new Color(0xEE, 0x82, 0xEE)}, 
            {"palegoldenrod", new Color(0xEE, 0xE8, 0xAA)}, 
            {"lightcoral", new Color(0xF0, 0x80, 0x80)}, 
            {"khaki", new Color(0xF0, 0xE6, 0x8C)}, 
            {"aliceblue", new Color(0xF0, 0xF8, 0xFF)}, 
            {"honeydew", new Color(0xF0, 0xFF, 0xF0)}, 
            {"azure", new Color(0xF0, 0xFF, 0xFF)}, 
            {"sandybrown", new Color(0xF4, 0xA4, 0x60)}, 
            {"wheat", new Color(0xF5, 0xDE, 0xB3)}, 
            {"beige", new Color(0xF5, 0xF5, 0xDC)}, 
            {"whitesmoke", new Color(0xF5, 0xF5, 0xF5)}, 
            {"mintcream", new Color(0xF5, 0xFF, 0xFA)}, 
            {"ghostwhite", new Color(0xF8, 0xF8, 0xFF)}, 
            {"salmon", new Color(0xFA, 0x80, 0x72)}, 
            {"antiquewhite", new Color(0xFA, 0xEB, 0xD7)}, 
            {"linen", new Color(0xFA, 0xF0, 0xE6)}, 
            {"lightgoldenrodyellow", new Color(0xFA, 0xFA, 0xD2)}, 
            {"oldlace", new Color(0xFD, 0xF5, 0xE6)}, 
            {"red", new Color(0xFF, 0x00, 0x00)}, 
            {"fuchsia", new Color(0xFF, 0x00, 0xFF)}, 
            {"magenta", new Color(0xFF, 0x00, 0xFF)}, 
            {"deeppink", new Color(0xFF, 0x14, 0x93)}, 
            {"orangered", new Color(0xFF, 0x45, 0x00)}, 
            {"tomato", new Color(0xFF, 0x63, 0x47)}, 
            {"hotpink", new Color(0xFF, 0x69, 0xB4)}, 
            {"coral", new Color(0xFF, 0x7F, 0x50)}, 
            {"darkorange", new Color(0xFF, 0x8C, 0x00)}, 
            {"lightsalmon", new Color(0xFF, 0xA0, 0x7A)}, 
            {"orange", new Color(0xFF, 0xA5, 0x00)}, 
            {"lightpink", new Color(0xFF, 0xB6, 0xC1)}, 
            {"pink", new Color(0xFF, 0xC0, 0xCB)}, 
            {"gold", new Color(0xFF, 0xD7, 0x00)}, 
            {"peachpuff", new Color(0xFF, 0xDA, 0xB9)}, 
            {"navajowhite", new Color(0xFF, 0xDE, 0xAD)}, 
            {"moccasin", new Color(0xFF, 0xE4, 0xB5)}, 
            {"bisque", new Color(0xFF, 0xE4, 0xC4)}, 
            {"mistyrose", new Color(0xFF, 0xE4, 0xE1)}, 
            {"blanchedalmond", new Color(0xFF, 0xEB, 0xCD)}, 
            {"papayawhip", new Color(0xFF, 0xEF, 0xD5)}, 
            {"lavenderblush", new Color(0xFF, 0xF0, 0xF5)}, 
            {"seashell", new Color(0xFF, 0xF5, 0xEE)}, 
            {"cornsilk", new Color(0xFF, 0xF8, 0xDC)}, 
            {"lemonchiffon", new Color(0xFF, 0xFA, 0xCD)}, 
            {"floralwhite", new Color(0xFF, 0xFA, 0xF0)}, 
            {"snow", new Color(0xFF, 0xFA, 0xFA)}, 
            {"yellow", new Color(0xFF, 0xFF, 0x00)}, 
            {"lightyellow", new Color(0xFF, 0xFF, 0xE0)}, 
            {"ivory", new Color(0xFF, 0xFF, 0xF0)}, 
            {"white", new Color(0xFF, 0xFF, 0xFF)}, 
        };

    [method: JigsawTab("G/Windows")]
    public static PythonTuple getMouse ()
    {
        return getWindow ().getMouse ();
    }

    [method: JigsawTab("G/Windows")]
    public static PythonTuple getMouseNow ()
    {
        return getWindow ().getMouseNow ();
    }

    [method: JigsawTab("G/Windows")]
    public static string getMouseState ()
    {
        return getWindow ().getMouseState ();
    }

    [method: JigsawTab("G/Windows")]
    public static string getLastKey ()
    {
        return getWindow ().getLastKey ();
    }

    [method: JigsawTab("G/Windows")]
    public static bool getKeyPressed ()
    {
        return getWindow ().getKeyPressed ();
    }

    [method: JigsawTab("G/Windows")]
    public static bool getKeyPressed (string key)
    {
        return getWindow ().getKeyPressed (key);
    }

    [method: JigsawTab("G/Windows")]
    public static void run ()
    {
        getWindow ().run ();
    }

    [method: JigsawTab("G/Windows")]
    public static void run (double step_time)
    {
        getWindow ().run (step_time);
    }

    [method: JigsawTab("G/Windows")]
    public static void run (Func<object> function)
    {
        getWindow ().run (function);
    }

    // Callbacks:

    [method: JigsawTab("G/Windows")]
    public static void onMouseUp (Func<object,Event,object> function)
    {
        getWindow ().onMouseUp (function);
    }

    [method: JigsawTab("G/Windows")]
    public static void onMouseDown (Func<object,Event,object> function)
    {
        getWindow ().onMouseDown (function);
    }

    [method: JigsawTab("G/Windows")]
    public static void onMouseMovement (Func<object,Event,object> function)
    {
        getWindow ().onMouseMovement (function);
    }

    [method: JigsawTab("G/Windows")]
    public static void onKeyPress (Func<object,Event,object> function)
    {
        getWindow ().onKeyPress (function);
    }

    [method: JigsawTab("G/Windows")]
    public static void onKeyRelease (Func<object,Event,object> function)
    {
        getWindow ().onKeyRelease (function);
    }

    [method: JigsawTab("G/Pictures")]
    public static List getColorNames ()
    {
        List retval = new List ();
        foreach (String key in colors.Keys) {
            retval.append (key);
        }
        return retval;
    }

    [method: JigsawTab(null)]
    public static List PyList (params object [] items)
    {
        // make a list from an array
        List retval = new List ();
        for (int i = 0; i < items.Length; i++) {
            retval.append (items [i]);
        }
        return retval;
    }

    [method: JigsawTab(null)]
    public static PythonTuple PyTuple (params object [] items)
    {
        // make a tuple from an array
        return new PythonTuple (items);
    }

    private static Dictionary<string, Graphics.WindowClass> _windows =
        new Dictionary<string, Graphics.WindowClass> ();

    [method: JigsawTab("G/Pictures")]
    public static Color getColor (Picture picture, int x, int y)
    {
        return picture.getColor (x, y);
    }

    [method: JigsawTab("G/Pictures")]
    public static Pixel getPixel (Picture picture, int x, int y)
    {
        return picture.getPixel (x, y);
    }

    [method: JigsawTab("G/Pictures")]
    public static IEnumerable<Pixel> getPixels (Picture picture)
    {
        for (int x=0; x < picture._cacheWidth; x++) {
            for (int y=0; y < picture._cacheHeight; y++) {
                yield return picture.getPixel(x, y);
            }
        }
    }

    [method: JigsawTab("G/Pictures")]
    public static void setPixels (Picture picture, Picture picture2)
    {
        InvokeBlocking( delegate {
            for (int x=0; x < picture._cacheWidth; x++) {
                for (int y=0; y < picture._cacheHeight; y++) {
                    picture.setPixel (x, y, picture2.getPixel (x, y));
                }
            }
        });
    }

    [method: JigsawTab("G/Pictures")]
    public static void setPixel (Picture picture, int x, int y, Color color)
    {
        picture.setPixel (x, y, color);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setPixel (Picture picture, int x, int y, Pixel pixel)
    {
        picture.setPixel (x, y, pixel);
    }

    [method: JigsawTab("G/Pictures")]
    public static Color getColor (Pixel pixel)
    {
        return pixel.getColor ();
    }

    [method: JigsawTab("G/Pictures")]
    public static void setColor (Pixel pixel, Color color)
    {
        pixel.setColor (color);
    }

    [method: JigsawTab("G/Pictures")]
    public static PythonTuple getRGB (Pixel pixel)
    {
        return pixel.getRGB ();
    }

    [method: JigsawTab("G/Pictures")]
    public static PythonTuple getRGBA (Pixel pixel)
    {
        return pixel.getRGBA ();
    }

    [method: JigsawTab("G/Pictures")]
    public static int getGray (Pixel pixel)
    {
        return pixel.getGray ();
    }

    [method: JigsawTab("G/Pictures")]
    public static int getRed (Pixel pixel)
    {
        return pixel.getRed ();
    }

    [method: JigsawTab("G/Pictures")]
    public static int getGreen (Pixel pixel)
    {
        return pixel.getGreen ();
    }

    [method: JigsawTab("G/Pictures")]
    public static int getBlue (Pixel pixel)
    {
        return pixel.getBlue ();
    }

    [method: JigsawTab("G/Pictures")]
    public static int getAlpha (Pixel pixel)
    {
        return pixel.getAlpha ();
    }

    [method: JigsawTab("G/Pictures")]
    public static void setRGB (Pixel pixel, IList rgb)
    {
        pixel.setRGB ((int)rgb [0], (int)rgb [1], (int)rgb [2]);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setRGB (Pixel pixel, float red, float green, float blue)
    {
        pixel.setRGB ((int)Math.Round(red), (int)Math.Round(green), (int)Math.Round(blue));
    }

    [method: JigsawTab("G/Pictures")]
    public static void setRGB (Pixel pixel, int red, int green, int blue)
    {
        pixel.setRGB (red, green, blue);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setRGBA (Pixel pixel, int red, int green, int blue, int alpha)
    {
        pixel.setRGBA (red, green, blue, alpha);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setGray (Pixel pixel, int value)
    {
        pixel.setGray (value);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setRed (Pixel pixel, int value)
    {
        pixel.setRed (value);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setGreen (Pixel pixel, int value)
    {
        pixel.setGreen (value);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setBlue (Pixel pixel, int value)
    {
        pixel.setBlue (value);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setAlpha (Pixel pixel, int value)
    {
        pixel.setAlpha (value);
    }

    [method: JigsawTab("G/Pictures")]
    public static void setTransparent (Picture picture, Graphics.Color color) {
        picture.setTransparent(color);
    }

    [method: JigsawTab("G/Pictures")]
    public static void savePicture (IList pictures, string filename, Graphics.Color color=null) {
        AnimatedGifEncoder gif = new AnimatedGifEncoder();
        if (color != null) {
            gif.SetTransparent(System.Drawing.Color.FromArgb(color.alpha, color.red, color.green, color.blue));
        }
        gif.Start();
        gif.SetRepeat(0);
        foreach (Picture picture in pictures) {
            gif.AddFrame(picture);
        }
        gif.Finish();
        gif.Output(filename);
    }

    [method: JigsawTab("G/Pictures")]
    public static void savePicture (List pictures, string filename, short delay, bool repeat, Graphics.Color color=null) {
        AnimatedGifEncoder gif = new AnimatedGifEncoder();
        if (color != null) {
            gif.SetTransparent(System.Drawing.Color.FromArgb(color.alpha, color.red, color.green, color.blue));
        }
        gif.Start();
        gif.SetRepeat(repeat ? 0 : -1);
        gif.SetDelay(delay);
        foreach (Picture picture in pictures) {
            gif.AddFrame(picture);
        }
        gif.Finish();
        gif.Output(filename);
    }

    [method: JigsawTab("G/Pictures")]
    public static void savePicture (Picture picture, string filename)
    {
        picture.savePicture (filename);
    }

    [method: JigsawTab("G/Pictures")]
    public static void savePicture (List list, string filename)
    {
        savePicture (list, filename, 10, false);
    }

    [method: JigsawTab("G/Pictures")]
    public static void savePicture (List list, string filename, short delay, Graphics.Color color=null)
    {
        savePicture (list, filename, delay, false, color);
    }

    [method: JigsawTab("G/Pictures")]
    public static Picture makePicture (int x, int y)
    {
        return new Picture (x, y);
    }

    public static System.Drawing.Bitmap toBitmap (object obj, params object [] args) {
        Type type = obj.GetType();
        System.Reflection.MethodInfo method = type.GetMethod("toBitmap");
        if (method != null) {
            return (System.Drawing.Bitmap) method.Invoke(obj, args);
        } else {
            Graphics.Picture picture = null;
            if (obj is System.Drawing.Bitmap) {
                picture = new Graphics.Picture((System.Drawing.Bitmap)obj);
            }
            if (picture != null) {
                return picture.toBitmap(args);
            } else {
                throw new Exception("Cannot convert object to bitmap");
            }
        }
    }

    public static Gdk.Pixbuf toPixbuf (object obj) {
        Type type = obj.GetType();
        System.Reflection.MethodInfo method = type.GetMethod("toPixbuf");
        if (method != null) {
            return (Gdk.Pixbuf) method.Invoke(obj, new object [] {});
        } else {
            Graphics.Picture picture = null;
            if (obj is System.Drawing.Bitmap) {
                picture = new Graphics.Picture((System.Drawing.Bitmap)obj);
            }
            if (picture != null) {
                return picture.toPixbuf();
            } else {
                throw new Exception("Cannot convert object to pixbuf");
            }
        }
    }

    [method: JigsawTab("G/Pictures")]
    public static Picture makePicture (int x, int y, Color c)
    {
        return new Picture (x, y, c);
    }

    [method: JigsawTab("G/Pictures")]
    public static Picture makePicture (string filename)
    {
        return new Picture (filename);
    }

    [method: JigsawTab("G/Pictures")]
    public static Picture copyPicture (Picture picture)
    {
        return new Picture (picture);
    }

    [method: JigsawTab(null)]
    public static void Init ()
    { 
        // Start a thread in Background to run Graphics
        // Only for use in non-GUI environments
        Thread t = new Thread (GraphicsLoop);
        t.Start ();
    }

    [method: JigsawTab(null)]
    public static void GraphicsLoop ()
    {
        Gtk.Application.Init ();
        Gtk.Application.Run ();
    }

    [method: JigsawTab("G/Pictures")]
    public static Picture makePicture (WindowClass window)
    { 
        return new Picture (window);
    }

    [method: JigsawTab("G/Pictures")]
    public static Picture makePicture (Gtk.Window gtk_window)
    { 
        return new Picture (gtk_window);
    }

    [method: JigsawTab("G/Pictures")]
    public static Picture makePicture (System.Drawing.Bitmap bitmap)
    { 
        return new Picture (bitmap);
    }

    [method: JigsawTab("G/Misc")]
    public static void waitSeconds (double seconds)
    {
        if (seconds > 0) 
            Thread.Sleep ((int)(seconds * 1000));
    }

    [method: JigsawTab("G/Windows")]
    public static Graphics.WindowClass makeWindow (string title="Calico Graphics",
            int width=300, 
            int height=300)
    {
		// JH FIXME: This invoke blocking may actually be the source of a race
		// condition that can crash calico
        InvokeBlocking( delegate {
            if (!(_windows.ContainsKey (title) && (_windows [title].canvas.IsRealized))) {
                _windows [title] = new Graphics.WindowClass (title, width, height);
            } else {
                _windows [title].reset(false);
                _windows [title].Resize(width, height);
            }
            _lastWindow = _windows [title];
            _lastWindow.KeepAbove = true;
            _lastWindow.Show();
        });
        return _windows [title];
    }

    [method: JigsawTab("G/Windows")]
    public static Graphics.WindowClass makeWindowFast (string title="Calico Graphics",
            int width=300, 
            int height=300,
            Picture picture=null)
    {
        InvokeBlocking( delegate {
            if (!(_windows.ContainsKey (title) && (_windows [title].canvas.IsRealized))) {
                _windows [title] = new Graphics.WindowClass (title, width, height);
                _windows [title].onMouseDown (delegate (object obj, Event evt) {
                    Picture pic = (Picture)_windows[title].canvas.shapes[0];
                    Graphics.Color color = pic.getColor(System.Convert.ToInt32(evt.x), 
                        System.Convert.ToInt32(evt.y));
                    System.Console.WriteLine(String.Format("Pixel: (x={0}, y={1}), (red={2}, green={3}, blue={4}, alpha={5})",
                            evt.x, evt.y, color.red, color.green, color.blue, color.alpha));
                    return true;
                });
            } else {
                _windows [title].canvas.shapes.Clear();
                _windows [title].Resize(width, height);
            }
            _lastWindow = _windows [title];
            _lastWindow.KeepAbove = true;
        });
        if (picture != null)
            picture.draw(_lastWindow);
        return _windows [title];
    }

    [method: JigsawTab("G/Windows")]
    public static Graphics.WindowClass makeWindow (string title, Gtk.Widget widget)
    {
        InvokeBlocking( delegate {
            if (!(_windows.ContainsKey (title) && (_windows [title].IsRealized))) {
                _windows [title] = new Graphics.WindowClass (title, widget);
            } else {
                _lastWindow.Remove(_lastWindow.widget);
                _lastWindow.Add(widget);
                _lastWindow.widget = widget;
                if (gui_thread_id != -1)
            widget.ShowAll();
            }
            _lastWindow = _windows [title];
            _lastWindow.KeepAbove = true;
            _lastWindow.Show();
        });
        return _windows [title];
    }

    [method: JigsawTab("G/Windows")]
    public static Graphics.WindowClass getWindow ()
    {
        if (_lastWindow != null) {
            return _lastWindow;
        } else {
            throw new Exception ("no windows exist yet");
        }
    }

    [method: JigsawTab("G/Windows")]
    public static Graphics.WindowClass getWindow (string title)
    {
        if (_windows.ContainsKey (title)) {
            return _windows [title];
        } else {
            return null;
        }
    }

    [method: JigsawTab("G/Windows")]
    public static Graphics.WindowClass Window (string title="Calico Graphics",
            int width=300, 
            int height=300)
    {
        return makeWindow (title, width, height);
    }

    [method: JigsawTab("G/Windows")]
    public static Graphics.WindowClass Window (int width,
            int height)
    {
        return makeWindow ("Calico Graphics", width, height);
    }

    [method: JigsawTab("G/Pictures")]
    public static Color makeColor (string color)
    {
        return new Color (color);
    }

    [method: JigsawTab("G/Pictures")]
    public static Color makeColor (int r, int g, int b)
    {
        return new Color (r, g, b);
    }

    [method: JigsawTab("G/Pictures")]
    public static Color makeColor (int r, int g, int b, int a)
    {
        return new Color (r, g, b, a);
    }

    [method: JigsawTab("G/Pictures")]
    public static Color makeColor (double r, double g, double b)
    {
        return new Color (r, g, b);
    }

    [method: JigsawTab("G/Pictures")]
    public static Color makeColor (double r, double g, double b, double a)
    {
        return new Color (r, g, b, a);
    }


    public class Plot
    {

        public WindowClass window;
        public List data = new List ();
        public Line line = new Line ();
        int border = 50;
        public Text xLabel;
        public Text yLabel;

        public Plot (IList list) : this("Plot", 640, 480)
        {
            foreach (double item in list) {
                append (item);
            }
        }

        public Plot (string title, int width, int height)
        {
            window = makeWindow (title, width, height);
            Line tick;
            Rectangle rect = new Rectangle (new Point (border, border), 
                    new Point (width - border, height - border));
            rect.fill = new Color ("white");
            rect.outline = new Color ("black");
            rect.tag = "line";
            rect.draw (window);
            // y ticks:
            int interval = (height - border * 2) / 10;
            for (int y = height - border; y >= border; y -= interval) {
                tick = new Line (new Point (border - 10, y), 
                        new Point (border, y));
                tick.outline = new Color ("black");
                tick.tag = "line";
                tick.draw (window);
            }
            yLabel = new Text (new Point (border / 3, height / 2), "y legend");
            yLabel.fill = new Color ("black");
            yLabel.rotate (90);
            yLabel.draw (window);
            xLabel = new Text (new Point (width / 2, height - border / 3), "x legend");
            xLabel.fill = new Color ("black");
            xLabel.draw (window);
        }

        public void append (int datum)
        {
            append (System.Convert.ToDouble (datum));
        }

        public void append (double datum)
        {
            line.undraw ();
            data.Add (datum);
            double h2 = window.height;
            if (data.Count > 1) {
                line = new Line ();
                line.outline = new Color ("red");
                int col = 0;
                double h;
                int increment = Math.Max((window.width - 2 * border) / (data.Count - 1), 1);
                double min = 10000;
                double max = -10000;
                foreach (double i in data) {
                    min = Math.Min (min, i);
                    max = Math.Max (max, i);
                }
                foreach (double i in data) {
                    if (max != min) {
                        h = (h2 - border * 2) * (i - min) / (max - min);
                    } else {
                        h = (h2 - border * 2) * .5;
                    }
                    line.append (new Point (border + col, h2 - border - h));
                    col += increment;
                }
                line.set_points ();
                line.draw (window);

                // remove previous tick numbers:
                List to_remove = new List ();
                lock (window.canvas.shapes) {
                    foreach (Shape shape in window.canvas.shapes) {
                        if (shape.tag == "tick")
                            to_remove.Add (shape);
                    }
                    foreach (Shape shape in to_remove) {
                        window.canvas.shapes.Remove (shape);
                    }
                }
                // x ticks:
                Line tick;
                int count = 1;
                increment = Math.Max((window.width - 2 * border) / (data.Count - 1), 1);
                Text text;
                int last_tick = 0;
                for (int x = border; x <= window.width - border; x += increment) {
                    if (x - last_tick > 40) {
                        tick = new Line (new Point (x, window.height - border), 
                                new Point (x, window.height - border + 10));
                        tick.outline = new Color ("black");
                        tick.tag = "tick";
                        tick.draw (window);
                        text = new Text (new Point (x, h2 - border + 20), count.ToString ());
                        text.outline = new Color ("black");
                        text.fontSize = 9;
                        text.tag = "tick";
                        text.draw (window);
                        last_tick = x;
                    }
                    count += 1;
                }
                // y ticks:
                int interval = (((int)h2) - border * 2) / 10;
                double interval_value = (max - min) / 10;
                double sum = min;
                for (int y = ((int)h2) - border; y >= border; y -= interval) {
                    text = new Text (new Point (border - 20, y), String.Format("{0:0.###}", sum));
                    text.outline = new Color ("black");
                    text.fontSize = 9;
                    text.tag = "tick";
                    text.draw (window);
                    sum += interval_value;
                }
            }
        }

        public IDictionary<string, string> GetRepresentations() {
            IDictionary<string, string> retval = window.GetRepresentations();
            retval["text/plain"] =  this.ToString();
            return retval;
        }
    } // end of Plot

    public class BarChart
    {

        public WindowClass window;
        public List data = new List ();
        int border = 50;
        public Text xLabel;
        public Text yLabel;

        public BarChart (string title, int width, int height)
        {
            window = makeWindow (title, width, height);
            Line tick;
            Rectangle rect = new Rectangle (new Point (border, border), 
                    new Point (width - border, height - border));
            rect.fill = new Color ("white");
            rect.outline = new Color ("black");
            rect.tag = "line";
            rect.draw (window);
            // x ticks:
            int interval = (width - border * 2) / 10;
            for (int x = border; x <= width - border; x += interval) {
                tick = new Line (new Point (x, height - border), 
                        new Point (x, height - border + 10));
                tick.outline = new Color ("black");
                tick.tag = "line";
                tick.draw (window);
            }
            // y ticks:
            interval = (height - border * 2) / 10;
            for (int y = height - border; y >= border; y -= interval) {
                tick = new Line (new Point (border - 10, y), 
                        new Point (border, y));
                tick.outline = new Color ("black");
                tick.tag = "line";
                tick.draw (window);
            }
            yLabel = new Text (new Point (border / 3, height / 2), "y legend");
            yLabel.fill = new Color ("black");
            yLabel.rotate (90);
            yLabel.draw (window);
            xLabel = new Text (new Point (width / 2, height - border / 3), "x legend");
            xLabel.fill = new Color ("black");
            xLabel.draw (window);
        }

        public void append (int datum)
        {
            append (System.Convert.ToDouble (datum));
        }

        public void append (double datum)
        {
            data.Add (datum);
        }

        public IDictionary<string, string> GetRepresentations() {
            IDictionary<string, string> retval = window.GetRepresentations();
            retval["text/plain"] =  this.ToString();
            return retval;
        }
    }

    public class Color
    {
        internal Cairo.Color _cairo;
        public WindowClass window;  // for setting color of a Window()
        public Picture picture; // for setting color of Picture()
        public int x = -1; // for use with picture, above
        public int y = -1; // for use with picture, above

        public Cairo.Color getCairo ()
        {
            return _cairo;
        }

        public Color (string name)
        {
            if (name.StartsWith ("#")) {
                int r = (int)System.Convert.ToUInt32 (name.Substring (1, 2), 16);
                int g = (int)System.Convert.ToUInt32 (name.Substring (3, 2), 16);
                int b = (int)System.Convert.ToUInt32 (name.Substring (5, 2), 16);
                _cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b));
            } else if (colors.ContainsKey (name.ToLower ())) {
                _cairo = colors [name.ToLower ()]._cairo;
            } else {
                throw new Exception (String.Format ("unknown colorname '{0}'", name));
            }
        }

        public Color (Color color)
        {
            _cairo = new Cairo.Color (color._cairo.R, color._cairo.G, color._cairo.B, color._cairo.A);
        }

        public Color (int r, int g, int b)
        {
            _cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b), 1.0);
        }

        public Color (int r, int g, int b, int a)
        {
            _cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b), ToCairo (a));
        }

        public Color (double r, double g, double b)
        {
            _cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b), 1.0);
        }

        public Color (double r, double g, double b, double a)
        {
            _cairo = new Cairo.Color (ToCairo (r), ToCairo (g), ToCairo (b), ToCairo (a));
        }

        public Color Copy ()
        {
            return new Color (red, green, blue, alpha);
        }

        public void QueueDraw ()
        { // color
            if (window is WindowClass) {
                if (window.getMode () == "auto" || 
                        window.getMode () == "bitmap" || 
                        window.getMode () == "physics")
                    window.update ();
                // else, manually call step()
            } 
        }

        public int red {
            get {
                return FromCairo (_cairo.R);
            }
            set {
                _cairo.R = ToCairo (value);
                if (picture is Picture) {
                    if (x >= 0 && y >= 0)
                        picture.setRed (x, y, value);
                    else {
                        foreach (Pixel pixel in picture.getPixels()) {
                            pixel.setRed (value);
                        }
                    }
                } else
                    QueueDraw ();
            }
        }

        public int green {
            get {
                return FromCairo (_cairo.G);
            }
            set {
                _cairo.G = ToCairo (value);
                if (picture is Picture) {
                    if (x >= 0 && y >= 0)
                        picture.setGreen (x, y, value);
                    else {
                        foreach (Pixel pixel in picture.getPixels()) {
                            pixel.setGreen (value);
                        }
                    }
                } else
                    QueueDraw ();
            }
        }

        public int blue {
            get {
                return FromCairo (_cairo.B);
            }
            set {
                _cairo.B = ToCairo (value);
                if (picture is Picture) {
                    if (x >= 0 && y >= 0)
                        picture.setBlue (x, y, value);
                    else {
                        foreach (Pixel pixel in picture.getPixels()) {
                            pixel.setBlue (value);
                        }
                    }
                } else
                    QueueDraw ();
            }
        }

        public int alpha {
            get {
                return FromCairo (_cairo.A);
            }
            set {
                _cairo.A = ToCairo (value);
                if (picture is Picture) {
                    if (x >= 0 && y >= 0)
                        picture.setAlpha (x, y, value);
                    else {
                        foreach (Pixel pixel in picture.getPixels()) {
                            pixel.setAlpha (value);
                        }
                    }
                } else
                    QueueDraw ();
            }
        }

        double ToCairo (int value)
        {
            return Math.Max (Math.Min ((value / 255.0), 1.0), 0.0);
        }

        double ToCairo (double value)
        {
            return Math.Max (Math.Min ((value / 255.0), 1.0), 0.0);
        }

        int FromCairo (double value)
        {
            return (int)(value * 255);
        }

        public override string ToString ()
        {
            return String.Format ("<Color (r={0},g={1},b={2},a={3})>", 
                    red, green, blue, alpha);
        }
        public string __repr__ ()
        {
            return ToString();
        }    

        public bool Equals(Graphics.Color other) 
        {
            return (red == other.red &&
                    green == other.green &&
                    blue == other.blue);
        }
    } // end of Color

    public class ColorStop
    {
		public Color color;
		public double offset;    
    }

    public class Gradient
    {
		public string gtype;
        public Color c1, c2;
        public Point p1, p2;
        public double radius1, radius2;

		public Gradient(Gradient other)
		{
			this.gtype = other.gtype;
            this.p1 = new Point (other.p1);
            this.radius1 = other.radius1;
            this.c1 = new Color(other.c1);
            this.p2 = new Point (other.p2);
            this.radius2 = other.radius2;
            this.c2 = new Color(other.c2);
		}
		
        // ("linear", (100, 200), Color("red"), (200, 100), Color("blue"))
        public Gradient (string gtype, IList p1, Color c1, IList p2, Color c2)
        {
            this.gtype = gtype;
            this.p1 = new Point (p1 [0], p1 [1]);
            this.p2 = new Point (p2 [0], p2 [1]);
            this.c1 = c1;
            this.c2 = c2;
        }

        public Gradient (string gtype, 
                IList p1, double radius1, Color c1, 
                IList p2, double radius2, Color c2)
        {
            this.gtype = gtype;
            this.p1 = new Point (p1 [0], p1 [1]);
            this.radius1 = radius1;
            this.c1 = c1;
            this.p2 = new Point (p2 [0], p2 [1]);
            this.radius2 = radius2;
            this.c2 = c2;
        }
    }

    //******************************************************//
    //JH: Huge experiment start                             //
    //******************************************************//
    public class DockableWindowClass : Gtk.ScrolledWindow
    {
        internal Canvas _canvas;
        public Gtk.Widget widget;
        internal bool _dirty = false;
        private bool timer_running = false;
        private DateTime last_update = new DateTime (2000, 1, 1);
        internal double _update_interval = .1; // how often, in seconds, to update
        public List onClickCallbacks = new List ();
        public List onMouseMovementCallbacks = new List ();
        public List onMouseUpCallbacks = new List ();
        public List onKeyPressCallbacks = new List ();
        public List onKeyReleaseCallbacks = new List ();
	public List onConfigureCallbacks = new List ();
        public PythonTuple _lastClick;
        public string _lastKey = "";
        public string _mouseState = "up";
        public bool _keyState = false;
        public Dictionary<string, bool> _keyStates;
        ManualResetEvent _lastClickFlag = new ManualResetEvent (false);
        public double time = 0.0;
        public double simulationStepTime = 0.01;
        public string state = "init";
        public bool requestStop = false;
        Gtk.ScrolledWindow _scrolledWindow = null;
        public int _cacheHeight;
        public int _cacheWidth;
        public bool HandleKeyPressOnShape = false;
        public bool HandleKeyReleaseOnShape = false;

        public DockableWindowClass (string title, Gtk.Widget widget) : base()
        {
            this.widget = widget;
            Add(widget);
            DeleteEvent += OnDelete;
            if (gui_thread_id != -1)
                ShowAll();
        }

        public DockableWindowClass (string title="Calico Graphics",
                int width=300, 
                int height=300) : base()
        {
            _canvas = new Canvas ("auto", width, height);
            _cacheWidth = width;
            _cacheHeight = height;
            
	    //JH: These do not exist in ScrolledWindow
            //AllowGrow = true;
            //AllowShrink = true;

            _keyStates = new Dictionary<string, bool>();

	    //JH: This does not exist
            //SetDefaultSize (width, height);
            AddEvents ((int)Gdk.EventMask.ButtonPressMask);
            AddEvents ((int)Gdk.EventMask.ButtonReleaseMask);
            AddEvents ((int)Gdk.EventMask.PointerMotionMask);
            AddEvents ((int)Gdk.EventMask.KeyReleaseMask);
            AddEvents ((int)Gdk.EventMask.KeyPressMask);
            // ------------------------------------------
            ButtonPressEvent += HandleClickCallbacks;
            ButtonReleaseEvent += HandleMouseUpCallbacks;
            ButtonPressEvent += saveLastClick;
            ButtonReleaseEvent += updateMouseState;
            MotionNotifyEvent += HandleMouseMovementCallbacks;
            KeyPressEvent += HandleKeyPressCallbacks;
            KeyReleaseEvent += HandleKeyReleaseCallbacks;
            // ------------------------------------------
            //ConfigureEvent += configureEventBefore;
            DeleteEvent += OnDelete;
            Add(_canvas);
            if (gui_thread_id != -1)
                ShowAll ();
        }

        /*
           new public void BeginResizeDrag (Gdk.WindowEdge edge, int button, int root_x, int root_y, uint timestamp) {
           base.BeginResizeDrag(edge, button, root_x, root_y, timestamp);
           _cacheWidth = width;
           _cacheHeight = height;
           }
           */
	protected override bool OnConfigureEvent(Gdk.EventConfigure args)
	{
	  base.OnConfigureEvent(args);
	  foreach (Func<Gdk.EventConfigure,object> function in onConfigureCallbacks) {
	    try {
	      Invoke (delegate {
		  Func<Gdk.EventConfigure,object > f = (Func<Gdk.EventConfigure,object>)function;
		  f (args);
		});
	    } catch (Exception e) {
	      Console.Error.WriteLine ("Error in OnConfigureEvent function");
	      Console.Error.WriteLine (e.Message);
	    }        
	  }
	  return true;
	}

        public string title {
            get {
                string retval = "";
                InvokeBlocking( delegate {
		    //JH: Title does not exist
		    //retval = Title;
                    retval = "not available";
                });
                return retval;
            }
            set {
                InvokeBlocking( delegate {
		    //JH: Title does not exist
                    //Title = value;
                });
            }
        }

        public bool isRealized() {
            bool retval = false;
            InvokeBlocking( delegate {
                retval = IsRealized;
            });
            return retval;
        }

        public bool isVisible() {
            bool retval = false;
            InvokeBlocking( delegate {
                retval = Visible;
            });
            return retval;
        }

        public void removeTagged(String tag) {
            List to_remove = new List ();
            lock (_canvas.shapes) {
                foreach (Shape shape in _canvas.shapes) {
                    if (shape.tag == tag)
                        to_remove.Add (shape);
                }
                foreach (Shape shape in to_remove) {
                    _canvas.shapes.Remove (shape);
                }
            }
        }

        public void saveToSVG(string filename) {
            canvas.saveToSVG(filename);
        }

        public Calico.Representation toSVG() {
            return canvas.toSVG();
        }

        public void addScrollbars(int width, int height) {
            InvokeBlocking( delegate {
                if (Child == _canvas) {
                    Remove(_canvas);
                    _scrolledWindow = new Gtk.ScrolledWindow();
                    Add(_scrolledWindow);
                    _scrolledWindow.Add (_canvas);
                    _canvas.resize(width, height);
                    _scrolledWindow.Show();
                } else {
                    _canvas.resize(width, height);
                }
            });
        }

        public void reset ()
        {
            reset (true);
        }

        public void reset (bool redraw)
        {
            Invoke (delegate {
                /* bad, causes lock up in IPython... don't know why
                   _canvas.surface = new Cairo.ImageSurface (Cairo.Format.Argb32, width, height);
                   _canvas.need_to_draw_surface = false;
                   */
                mode = "auto";
                //Resize (width, height); // removed because of scrollbar issue
                timer_running = false;
                last_update = new DateTime (2000, 1, 1);
                _update_interval = .1; // how often, in seconds, to update
                onClickCallbacks = new List ();
                onMouseMovementCallbacks = new List ();
                onMouseUpCallbacks = new List ();
                onKeyPressCallbacks = new List ();
                onKeyReleaseCallbacks = new List ();
				onConfigureCallbacks = new List ();

                // clear listeners:
                clearListeners();
                _lastKey = "";
                _mouseState = "up";
                _keyState = false;
                _keyStates = new Dictionary<string, bool>();
                time = 0.0;
                simulationStepTime = 0.01;
                state = "init";
                lock (_canvas.shapes)
                    _canvas.shapes.Clear ();
                foreach (Gtk.Widget child in _canvas.Children) {
                    _canvas.Remove (child);
                }
                Gdk.Color bg = new Gdk.Color (242, 241, 240);
                _canvas.ModifyBg (Gtk.StateType.Normal, bg);
                if (redraw)
                    QueueDraw();
            });
        }

        public void resetBlocking(bool redraw)
        {
            InvokeBlocking (delegate {
                /* bad, causes lock up in IPython... don't know why
                   _canvas.surface = new Cairo.ImageSurface (Cairo.Format.Argb32, width, height);
                   _canvas.need_to_draw_surface = false;
                   */
                mode = "auto";
                //Resize (width, height); // removed because of scrollbar issue
                timer_running = false;
                last_update = new DateTime (2000, 1, 1);
                _update_interval = .1; // how often, in seconds, to update
                onClickCallbacks = new List ();
                onMouseMovementCallbacks = new List ();
                onMouseUpCallbacks = new List ();
                onKeyPressCallbacks = new List ();
                onKeyReleaseCallbacks = new List ();
				onConfigureCallbacks = new List ();

                // clear listeners:
                clearListeners();
                _lastKey = "";
                _mouseState = "up";
                _keyState = false;
                _keyStates = new Dictionary<string, bool>();
                time = 0.0;
                simulationStepTime = 0.01;
                state = "init";
                lock (_canvas.shapes)
                    _canvas.shapes.Clear ();
                foreach (Gtk.Widget child in _canvas.Children) {
                    _canvas.Remove (child);
                }
                Gdk.Color bg = new Gdk.Color (242, 241, 240);
                _canvas.ModifyBg (Gtk.StateType.Normal, bg);
                if (redraw)
                    QueueDraw();
            });
        }

        public Gtk.ScrolledWindow ScrolledWindow {
            get { return _scrolledWindow; }
        }

        public Microsoft.Xna.Framework.Vector2 gravity {
            get {
                return canvas.world.Gravity;
            }
            set {
                canvas.world.Gravity = value;
            }
        }

        public void close ()
        {
            Invoke (delegate { 
                Hide ();
            });
        }

        public void setBackground (Color color)
        {
            Invoke( delegate {
                _canvas.setBackground(color);
            });
        }

        public Gdk.Drawable getDrawable ()
        {
            return GdkWindow;
        }

        private void OnDelete (object obj, Gtk.DeleteEventArgs args)
        {
	  //JH: Title does not exist
          //  _windows.Remove (Title);
        }

        public int getWidth ()
        {
            int _width = 0, _height = 0;
            InvokeBlocking( delegate {
		//JH: GetSize does not exist
                //this.GetSize (out _width, out _height);
            });
            return _width;
        }

        public int _getWidth ()
        {
            int _width = 0, _height = 0;
	    //JH: GetSize does not exist
            //this.GetSize (out _width, out _height);
            return _width;
        }

        public int getHeight ()
        {
            int _width = 0, _height = 0;
            InvokeBlocking( delegate {
		//JH: GetSize does not exist
                //this.GetSize (out _width, out _height);
            });
            return _height;
        }

        public int _getHeight ()
        {
            int _width = 0, _height = 0;
	    //JH: GetSize does not exist
            //this.GetSize (out _width, out _height);
            return _height;
        }

        public GraphicsRepresentation draw (Shape shape)
        {
            shape.draw (this);
            return new GraphicsRepresentation(this);
        }

        public GraphicsRepresentation drawAt (Shape shape, IList iterable)
        {
            shape.moveTo(System.Convert.ToDouble(iterable[0]), 
                    System.Convert.ToDouble(iterable[1]));
            shape.draw (this);
            return new GraphicsRepresentation(this);
        }

        public void undraw (Shape shape)
        {
            shape.undraw ();
        }

        public void stackOnTop (Shape shape)
        {
            // last drawn is on top
            if (_canvas.shapes.Contains (shape)) {
                Invoke( delegate {
                    lock (_canvas.shapes) {
                        _canvas.shapes.Remove (shape);
                        _canvas.shapes.Insert (_canvas.shapes.Count, shape);
                    }
                });
                QueueDraw ();
            }
        }

        public void stackOnBottom (Shape shape)
        {
            // first drawn is on bottom
            if (_canvas.shapes.Contains (shape)) {
                Invoke( delegate {
                    lock (_canvas.shapes) {
                        _canvas.shapes.Remove (shape);
                        _canvas.shapes.Insert (0, shape);
                    }
                    QueueDraw ();
                });
            } else {
                throw new Exception ("shape not drawn on window");
            }
        }

        void saveLastClick (object obj, Gtk.ButtonPressEventArgs args)
        {
            _mouseState = "down";
            _lastClick = PyTuple (args.Event.X, args.Event.Y);
            _lastClickFlag.Set ();
        }

        void updateMouseState (object obj, Gtk.ButtonReleaseEventArgs args)
        {
            _mouseState = "up";
        }

        private void HandleMouseMovementCallbacks (object obj,
                Gtk.MotionNotifyEventArgs args)
        {
            Event evt = new Event (args);
            foreach (Func<object,Event,object> function in onMouseMovementCallbacks) {
                try {
                    Invoke (delegate {
                        Func<object,Event,object > f = (Func<object,Event,object>)function;
                        f (obj, evt);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in onMouseMove function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

        public void clearListeners() {
            MotionNotifyEvent -= HandleMouseMovementOnShape;
            ButtonPressEvent -= HandleClickOnShape;
            ButtonReleaseEvent -= HandleMouseUpOnShape;
            HandleKeyPressOnShape = false;
            HandleKeyReleaseOnShape = false;
        }

        public void listen(string evt) {
            if (evt == "mouse-motion") {
                // remove if already there:
                MotionNotifyEvent -= HandleMouseMovementOnShape;
                MotionNotifyEvent += HandleMouseMovementOnShape;
            } else if (evt == "mouse-press") {
                // remove if already there:
                ButtonPressEvent -= HandleClickOnShape;
                ButtonPressEvent += HandleClickOnShape;
            } else if (evt == "mouse-release") {
                // remove if already there:
                ButtonReleaseEvent -= HandleMouseUpOnShape;
                ButtonReleaseEvent += HandleMouseUpOnShape;
            } else if (evt == "key-press") {
                HandleKeyPressOnShape = true;
            } else if (evt == "key-release") {
                HandleKeyReleaseOnShape = true;
            } else {
                throw new Exception(String.Format("invalid event '{0}': use 'mouse-motion', 'mouse-press', 'mouse-release', 'key-press', or 'key-release'.", evt));
            }
        }

        public void HandleMouseMovementOnShape (object obj,
                Gtk.MotionNotifyEventArgs args)
        {
            lock (canvas.shapes) {
                foreach (Shape shape in canvas.shapes) {
                    if (shape.hit(args.Event.X, args.Event.Y)) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
        }

        public void HandleClickOnShape (object obj, Gtk.ButtonPressEventArgs args)
        {
            lock (canvas.shapes) {
                foreach (Shape shape in canvas.shapes) {
                    if (shape.hit(args.Event.X, args.Event.Y)) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
        }

        public void HandleMouseUpOnShape (object obj,
                Gtk.ButtonReleaseEventArgs args) {
            lock (canvas.shapes) {
                foreach (Shape shape in canvas.shapes) {
                    if (shape.hit(args.Event.X, args.Event.Y)) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
        }

        private void HandleClickCallbacks (object obj,
                Gtk.ButtonPressEventArgs args)
        {
            Event evt = new Event (args);
            foreach (Func<object,Event,object> function in onClickCallbacks) {
                try {
                    Invoke (delegate {
                        Func<object,Event,object > f = (Func<object,Event,object>)function;
                        f (obj, evt);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in onMouseDown function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

        private void HandleMouseUpCallbacks (object obj,
                Gtk.ButtonReleaseEventArgs args)
        {
            Event evt = new Event (args);
            foreach (Func<object,Event,object> function in onMouseUpCallbacks) {
                try {
                    Invoke (delegate {
                        Func<object,Event,object > f = (Func<object,Event,object>)function;
                        f (obj, evt);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in onMouseUp function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

        [GLib.ConnectBefore]
        private void HandleKeyPressCallbacks (object obj,
                Gtk.KeyPressEventArgs args)
        {
            _lastKey = args.Event.Key.ToString ();
            _keyState = true;
            _keyStates[_lastKey] = true;
            if (HandleKeyPressOnShape){
                lock (canvas.shapes) {
                    foreach (Shape shape in canvas.shapes) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
            Event evt2 = new Event (args);
            foreach (Func<object,Event,object> function in onKeyPressCallbacks) {
                try {
                    Invoke (delegate {
                        Func<object,Event,object > f = (Func<object,Event,object>)function;
                        f (obj, evt2);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in onKeypress function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
	    args.RetVal = true;
        }

        private void HandleKeyReleaseCallbacks (object obj,
                Gtk.KeyReleaseEventArgs args)
        {
            _keyState = false;
            string lastKey = args.Event.Key.ToString ();
            _keyStates[lastKey] = false;
            if (HandleKeyReleaseOnShape){
                lock (canvas.shapes) {
                    foreach (Shape shape in canvas.shapes) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
            Event evt2 = new Event (args);
            foreach (Func<object,Event,object> function in onKeyReleaseCallbacks) {
                try {
                    Invoke (delegate {
                        Func<object,Event,object > f = (Func<object,Event,object>)function;
                        f (obj, evt2);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in onKeyRelease function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

        public void onClick (Func<object,Event,object> function)
        {
            onClickCallbacks.Add (function);
        }

        public void onMouseDown (Func<object,Event,object> function)
        {
            onClickCallbacks.Add (function);
        }

        public void onConfigure (Func<Gdk.EventConfigure,object> function)
        {
            onConfigureCallbacks.Add (function);
        }

        public void run (Func<object> function)
        {
            try {
                // FIXME: Understand why:
                // This does not like a Gtk Application Invoke here
                function ();
            } catch (Exception e) {
                if (!e.Message.Contains ("Thread was being aborted")) {
                    Console.Error.WriteLine ("Error in run function");
                    Console.Error.WriteLine (e.Message);
                }
            }
        }

        public void run () {
            run(.01);
        }

        public void run (double step_time)
        {
            requestStop = false;
            while (! requestStop) {
                try {
                    step (step_time);
                } catch {
                    requestStop = true;
                }
            }
        }

        public void onMouseUp (Func<object,Event,object> function)
        {
            onMouseUpCallbacks.Add (function);
        }

        public void onMouseMovement (Func<object,Event,object> function)
        {
            onMouseMovementCallbacks.Add (function);
        }

        public void onKeyPress (Func<object,Event,object> function)
        {
            onKeyPressCallbacks.Add (function);
        }

        public void onKeyRelease (Func<object,Event,object> function)
        {
            onKeyReleaseCallbacks.Add (function);
        }

        public double updateInterval {
            get {
                return _update_interval;
            }
            set {
                _update_interval = value;
            }
        }

        public double getUpdateInterval ()
        {
            return updateInterval;
        }
        public void setUpdateInterval (double u)
        {
            updateInterval = u;
        }

        public int height {
            get {
                int _width = 0, _height = 0;
                InvokeBlocking( delegate {
                    if (Child == _canvas || Child == widget) {
		      //JH: GetSize does not exit
                      //this.GetSize (out _width, out _height);
                    } else { // scrollbars
                        _height = _canvas.height;
                    }
                });
                return _height;
            }
        }

        public int _height {
            get {
                int _width = 0, _height = 0;
                if (Child == _canvas || Child == widget) {
		  //JH: GetSize does not exit
		  //this.GetSize (out _width, out _height);
                } else { // scrollbars
                    _height = _canvas.height;
                }
                return _height;
            }
        }

        public int width {
            get {
                int _width = 0, _height = 0;
                InvokeBlocking( delegate {
                    if (Child == _canvas || Child == widget) {
		      //JH: GetSize does not exit
                      //this.GetSize (out _width, out _height);
                    } else { // scrollbars
                        _width = _canvas.width;
                    }
                });
                return _width;
            }
        }

        public int _width {
            get {
                int _width = 0, _height = 0;
                if (Child == _canvas || Child == widget) {
		  //JH: GetSize does not exit
                  //  this.GetSize (out _width, out _height);
                } else { // scrollbars
                    _width = _canvas.width;
                }
                return _width;
            }
        }

        public Canvas getCanvas ()
        {
            return _canvas;
        }

        public Canvas canvas {
            get {
                return _canvas;
            }
        }

        public PythonTuple getMouse ()
        {
            _lastClickFlag.Reset();
            _lastClickFlag.WaitOne ();
            return _lastClick;
        }

        public PythonTuple getMouseNow ()
        {
            int x = 0, y = 0;
            InvokeBlocking (delegate { 
                GetPointer (out x, out y);
            });
            return PyTuple (x, y);
        }

        public string getMouseState ()
        {
            return _mouseState;
        }

        public string getLastKey ()
        {
            string lk = _lastKey;
            //_lastKey = ""; //consume the event
            return lk;
        }

        public bool getKeyPressed ()
        {
            return _keyState;
        }

        public bool getKeyPressed (string key)
        {
            return _keyStates.ContainsKey(key) ? _keyStates[key]: false;
        }

        public new void Show ()
        {
            Invoke (delegate { 
                DateTime now = DateTime.Now;
                last_update = now;
                _dirty = false;
                if (gui_thread_id != -1)
                base.Show (); 
            });
        }

        public new void ShowAll ()
        {
            Invoke (delegate { 
                DateTime now = DateTime.Now;
                last_update = now;
                _dirty = false;
                if (gui_thread_id != -1)
                base.ShowAll (); 
            });
        }

        public new void Resize (int width, int height)
        {
            _canvas.resize (width, height);
            Invoke (delegate {
		//JH: Resize does not exist
                //base.Resize (width, height);
            });
        }

        public void need_to_redraw ()
        { 
            _dirty = true;
            DateTime now = DateTime.Now;
            // diff is TimeSpan
            if ((now - last_update).TotalMilliseconds < (updateInterval * 1000)) {
                // pass, too soon!
                // but we need to make sure that someone checks
                // in the future. 
                if (timer_running) {
                    // already one running!
                    // we'll just wait
                } else {
                    // let's spawn one to check in 100 ms or so
                    timer_running = true;
                    GLib.Timeout.Add ((uint)(updateInterval * 1000), 
                            new GLib.TimeoutHandler (_redraw_now));
                }
            } else { // it is not too soon
                if (timer_running) {
                    // no need to start another
                } else {
                    // let's spawn one to check in 100 ms or so
                    timer_running = true;
                    GLib.Timeout.Add ((uint)(updateInterval * 1000), 
                            new GLib.TimeoutHandler (_redraw_now));
                }
            }
        }

        private bool _redraw_now ()
        {
            DateTime now = DateTime.Now;
            if (_dirty) {
                last_update = now;
                _dirty = false;
                QueueDraw (); // gtk
            }
            timer_running = false;
            return false; // return true to try again
        }

        public void setMode(String mode) {
            this.mode = mode;
        }

        public string getMode ()
        {
            return _canvas.mode;
        }

        public string mode {
            get {
                return _canvas.mode;
            }
            set {
                if (value == "auto" || value == "manual" || value == "physics" || 
                        value == "bitmap" || value == "bitmapmanual" || value == "physicsmanual")
                    canvas.mode = value;
                else
                    throw new Exception ("window mode must be 'auto', 'manual', 'bitmap', 'bitmapmanual', 'physicsmanual', or 'physics'");
            }
        }

		public void handleEvents()
		{
            while (Gtk.Application.EventsPending()){
                Gtk.Application.RunIteration ();
			}
		}

        public void updateNow ()
        { // Window
            need_to_redraw ();
            // Manual call to update, let's update then:
			handleEvents();
        }

        public void update ()
        { // Window
            if (mode == "manual" || mode == "bitmapmanual" || mode == "physicsmanual") {
                _canvas.need_to_draw_surface = true;
                Invoke( delegate {
                    QueueDraw ();
                });
            } else {
                need_to_redraw ();
            }
        }

        public void step ()
        { // Window
            step (0);
        }

        public void step (double step_time)
        { // Window, in seconds
            // Same as update, but will make sure it 
            // doesn't update too fast.
            // handle physics
            if (_canvas == null) {
                requestStop = true;
                return;
            }
            // kjo
            _canvas.need_to_draw_surface = true;

            if (mode == "physics" || mode == "physicsmanual") {
                lock(_canvas.world){
                    _canvas.world.Step ((float)simulationStepTime);
                }
                time += simulationStepTime; 
                // update the sprites
                lock (_canvas.shapes) {
                    foreach (Shape shape in _canvas.shapes) {
                        shape.updateFromPhysics ();
                    }
                }
            }

            //JH: Patching the framerate limiter start
            // and now the update
            DateTime now = DateTime.Now;
            // diff is TimeSpan, converted to seconds:
            double diff = (now - last_update).TotalMilliseconds / 1000.0;
            double remain = step_time - diff; //JH: calculate the remaining time
            while (remain > 0) { // seconds   //JH: spin on remaining time
                //System.Console.Write(".");
                //System.Console.Write("spinning...");
                if (remain > 0.01) {
                    //JH: wait based on the remaining time!
                    //JH: Previous bug made it such that the longer the update took, the longer you would have to wait!
                    //System.Console.Write("sleeping...");
                    Thread.Sleep ((int)(remain / 2 * 1000));
                } //Else just spin-lock
                //System.Console.Write("+");

                //JH: Recalculate the differences
                now = DateTime.Now;
                diff = (now - last_update).TotalMilliseconds / 1000.0;
                remain = step_time - diff;
            }
            //System.Console.Write("Waited: ");
            //System.Console.Write(diff.ToString());
            //System.Console.Write("\n");
            last_update = DateTime.Now;
            //JH: Patching the framerate limiter end

            if (mode == "bitmapmanual") {
                using (Cairo.Context g = new Cairo.Context(_canvas.finalsurface)) {
                    g.Save ();
                    g.Operator = Cairo.Operator.Source;
                    g.SetSourceSurface (_canvas.surface, 0, 0);
                    g.Paint ();
                    g.Restore ();
                }      
            }      

            _dirty = false;
            InvokeBlocking (delegate { 
                try {
                    QueueDraw ();
                    GdkWindow.ProcessUpdates (true);
                    //System.Console.Write("!");
                } catch {
                    requestStop = true;
                }
            });
        }

        public void refresh()
        { // Window, in seconds
            // Same as update, but will make sure it 
            // doesn't update too fast.
            // handle physics
            // kjo
            _canvas.need_to_draw_surface = true;

            if (mode == "physics" || mode == "physicsmanual") {
                // update the sprites
                lock (_canvas.shapes) {
                    foreach (Shape shape in _canvas.shapes) {
                        shape.updateFromPhysics ();
                    }
                }
            }
            if (mode == "bitmapmanual") {
                using (Cairo.Context g = new Cairo.Context(_canvas.finalsurface)) {
                    g.Save ();
                    g.Operator = Cairo.Operator.Source;
                    g.SetSourceSurface (_canvas.surface, 0, 0);
                    g.Paint ();
                    g.Restore ();
                }      
            }      

            _dirty = false;
            InvokeBlocking (delegate { 
                try {
                    QueueDraw ();
                    GdkWindow.ProcessUpdates (true);
                    //System.Console.Write("!");
                } catch {
                    requestStop = true;
                }
            });
        }

        public override string ToString ()
        {
	  //JH: Title does not exist
          //  return String.Format ("<Window (title='{0}',width={1},height={2})>", 
          //          Title, width, height);
	  return String.Format ("<Window (title='{0}',width={1},height={2})>", 
				"not available", width, height);
        }
        public string __repr__ ()
        {
            return ToString();
        }

        public IDictionary<string, string> GetRepresentations() {
            IDictionary<string, string> retval = new Picture (this).GetRepresentations();
            retval["text/plain"] =  this.ToString();
            IDictionary<string, string> svg_rep = toSVG().GetRepresentations();
            foreach(KeyValuePair<string,string> kvp in (IDictionary<string,string>)svg_rep) {
                retval[kvp.Key] = kvp.Value;
            }
            return retval;
        }
    }

    //**********************************************************//
    //JH: Huge experiment end                                   //
    //**********************************************************//



    public class WindowClass : Gtk.Window
    {
        internal Canvas _canvas;
        public Gtk.Widget widget;
        internal bool _dirty = false;
        private bool timer_running = false;
        private DateTime last_update = new DateTime (2000, 1, 1);
		// how often, in seconds, to update
        internal double _update_interval = .1; 
		// Callbacks
        public List<Func<object,Event,object> > onClickCallbacks =
			new List<Func<object,Event,object> > ();
        public List<Func<object,Event,object> > onMouseMovementCallbacks =
			new List<Func<object,Event,object> > ();
        public List<Func<object,Event,object> > onMouseUpCallbacks =
			new List<Func<object,Event,object> > ();
        public List<Func<object,Event,object> > onKeyPressCallbacks =
			new List<Func<object,Event,object> > ();
        public List<Func<object,Event,object> > onKeyReleaseCallbacks =
			new List<Func<object,Event,object> > ();
		public List<Func<Gdk.EventConfigure,object> > onConfigureCallbacks =
			new List<Func<Gdk.EventConfigure,object> > ();
		public List<Func<Gdk.Event,object> > onDeleteCallbacks =
			new List<Func<Gdk.Event,object> > ();
		public List<Func<object, Event, object> > onDestroyCallbacks =
			new List<Func<object, Event, object> > ();
		/* public List removedMouseUpCallbacks = new List (); */
        public PythonTuple _lastClick;
        public string _lastKey = "";
        public string _mouseState = "up";
        public bool _keyState = false;
        public Dictionary<string, bool> _keyStates;
        ManualResetEvent _lastClickFlag = new ManualResetEvent (false);
        public double time = 0.0;
        public double simulationStepTime = 0.01;
        public string state = "init";
        public bool requestStop = false;
        Gtk.ScrolledWindow _scrolledWindow = null;
        public int _cacheHeight;
        public int _cacheWidth;
        public bool HandleKeyPressOnShape = false;
        public bool HandleKeyReleaseOnShape = false;
		// JH: Drag, enter, and leave code - start
		protected List _clickedOnShapes = new List ();
		
		public void startDrag(Shape shape){
			if(!_clickedOnShapes.Contains(shape)){
				_clickedOnShapes.Add(shape);
			}
			shape.setClickedOn(true);
		}
		// JH: Drag, enter, and leave code - end

        public WindowClass (string title, Gtk.Widget widget) : base(title)
        {
            this.widget = widget;
            Add(widget);
            DeleteEvent += OnDelete;
            if (gui_thread_id != -1)
                ShowAll();
        }

        public WindowClass (string title="Calico Graphics",
                int width=300, 
                int height=300) : base(title)
        {
            _canvas = new Canvas ("auto", width, height);
            _cacheWidth = width;
            _cacheHeight = height;
            //DoubleBuffered = false;
            AllowGrow = true;
            AllowShrink = true;
            _keyStates = new Dictionary<string, bool>();
            SetDefaultSize (width, height);
            AddEvents ((int)Gdk.EventMask.ButtonPressMask);
            AddEvents ((int)Gdk.EventMask.ButtonReleaseMask);
            AddEvents ((int)Gdk.EventMask.PointerMotionMask);
            AddEvents ((int)Gdk.EventMask.KeyReleaseMask);
            AddEvents ((int)Gdk.EventMask.KeyPressMask);
            // ------------------------------------------
            ButtonPressEvent += HandleClickCallbacks;
            ButtonReleaseEvent += HandleMouseUpCallbacks;
            ButtonPressEvent += saveLastClick;
            ButtonReleaseEvent += updateMouseState;
            MotionNotifyEvent += HandleMouseMovementCallbacks;
            KeyPressEvent += HandleKeyPressCallbacks;
            KeyReleaseEvent += HandleKeyReleaseCallbacks;
            // ------------------------------------------
            //ConfigureEvent += configureEventBefore;
            DeleteEvent += OnDelete;
			Destroyed += OnDestroyEvent;
            Add(_canvas);
            if (gui_thread_id != -1)
                ShowAll ();
        }



        /*
           new public void BeginResizeDrag (Gdk.WindowEdge edge, int button, int root_x, int root_y, uint timestamp) {
           base.BeginResizeDrag(edge, button, root_x, root_y, timestamp);
           _cacheWidth = width;
           _cacheHeight = height;
           }
           */


        public string title {
            get {
                string retval = "";
                InvokeBlocking( delegate {
                    retval = Title;
                });
                return retval;
            }
            set {
                InvokeBlocking( delegate {
                    Title = value;
                });
            }
        }

        public bool isRealized() {
            bool retval = false;
            InvokeBlocking( delegate {
                retval = IsRealized;
            });
            return retval;
        }

        public bool isVisible() {
            bool retval = false;
            InvokeBlocking( delegate {
                retval = Visible;
            });
            return retval;
        }

        public void removeTagged(String tag) {
            List to_remove = new List ();
            lock (_canvas.shapes) {
                foreach (Shape shape in _canvas.shapes) {
                    if (shape.tag == tag)
                        to_remove.Add (shape);
                }
                foreach (Shape shape in to_remove) {
                    _canvas.shapes.Remove (shape);
                }
            }
        }

        public void saveToSVG(string filename) {
            canvas.saveToSVG(filename);
        }

        public Calico.Representation toSVG() {
            return canvas.toSVG();
        }

        public void addScrollbars(int width, int height) {
            InvokeBlocking( delegate {
                if (Child == _canvas) {
                    Remove(_canvas);
                    _scrolledWindow = new Gtk.ScrolledWindow();
                    Add(_scrolledWindow);
                    _scrolledWindow.Add (_canvas);
                    _canvas.resize(width, height);
                    _scrolledWindow.Show();
                } else {
                    _canvas.resize(width, height);
                }
            });
        }

        public void reset ()
        {
            reset (true);
        }

		protected void resetNow (bool redraw)
        {
			mode = "auto";
			//Resize (width, height); // removed because of scrollbar issue
			timer_running = false;
			last_update = new DateTime (2000, 1, 1);
			_update_interval = .1; // how often, in seconds, to update
			onClickCallbacks = new List<Func<object,Event,object> > ();
			onMouseMovementCallbacks = new List<Func<object,Event,object> > ();
			onMouseUpCallbacks = new List<Func<object,Event,object> > ();
			//removedMouseUpCallbacks = new List ();
			onKeyPressCallbacks = new List<Func<object,Event,object> > ();
			onKeyReleaseCallbacks = new List<Func<object,Event,object> > ();
			onConfigureCallbacks = new List<Func<Gdk.EventConfigure,object> >();
			onDeleteCallbacks = new List<Func<Gdk.Event,object> > ();
			onDestroyCallbacks = new List<Func<object, Event,object> > ();
			
			// clear listeners:
			clearListeners();
			_lastKey = "";
			_mouseState = "up";
			_keyState = false;
			_keyStates = new Dictionary<string, bool>();
			time = 0.0;
			simulationStepTime = 0.01;
			state = "init";
			lock (_canvas.shapes)
				_canvas.shapes.Clear ();
			foreach (Gtk.Widget child in _canvas.Children) {
				_canvas.Remove (child);
			}
			Gdk.Color bg = new Gdk.Color (242, 241, 240);
			_canvas.ModifyBg (Gtk.StateType.Normal, bg);
			if (redraw)
				QueueDraw();
		}

		// TODO: Do we have any use for this version of the reset function?
        public void resetAsync (bool redraw)
        {
            Invoke (delegate {
					resetNow(redraw);
            });
        }

        public void reset(bool redraw)
        {
            InvokeBlocking (delegate {
					resetNow(redraw);
            });
        }

        public Gtk.ScrolledWindow ScrolledWindow {
            get { return _scrolledWindow; }
        }

        public Microsoft.Xna.Framework.Vector2 gravity {
            get {
                return canvas.world.Gravity;
            }
            set {
                canvas.world.Gravity = value;
            }
        }

        public void close ()
        {
            Invoke (delegate { 
                Hide ();
            });
        }

        public void setBackground (Color color)
        {
            Invoke( delegate {
                _canvas.setBackground(color);
            });
        }

        public Gdk.Drawable getDrawable ()
        {
            return GdkWindow;
        }

        private void OnDelete (object obj, Gtk.DeleteEventArgs args)
        {
            _windows.Remove (Title);
        }

        public int getWidth ()
        {
            int _width = 0, _height = 0;
            InvokeBlocking( delegate {
                this.GetSize (out _width, out _height);
            });
            return _width;
        }

        public int _getWidth ()
        {
            int _width = 0, _height = 0;
            this.GetSize (out _width, out _height);
            return _width;
        }

        public int getHeight ()
        {
            int _width = 0, _height = 0;
            InvokeBlocking( delegate {
                this.GetSize (out _width, out _height);
            });
            return _height;
        }

        public int _getHeight ()
        {
            int _width = 0, _height = 0;
            this.GetSize (out _width, out _height);
            return _height;
        }

        public GraphicsRepresentation draw (Shape shape)
        {
            shape.draw (this);
            return new GraphicsRepresentation(this);
        }

        public GraphicsRepresentation drawAt (Shape shape, IList iterable)
        {
            shape.moveTo(System.Convert.ToDouble(iterable[0]), 
                    System.Convert.ToDouble(iterable[1]));
            shape.draw (this);
            return new GraphicsRepresentation(this);
        }

        public void undraw (Shape shape)
        {
            shape.undraw ();
        }

        public void stackOnTop (Shape shape)
        {
            // last drawn is on top
            if (_canvas.shapes.Contains (shape)) {
                Invoke( delegate {
                    lock (_canvas.shapes) {
                        _canvas.shapes.Remove (shape);
                        _canvas.shapes.Insert (_canvas.shapes.Count, shape);
                    }
                });
                QueueDraw ();
            }
        }

        public void stackOnBottom (Shape shape)
        {
            // first drawn is on bottom
            if (_canvas.shapes.Contains (shape)) {
                Invoke( delegate {
                    lock (_canvas.shapes) {
                        _canvas.shapes.Remove (shape);
                        _canvas.shapes.Insert (0, shape);
                    }
                    QueueDraw ();
                });
            } else {
                throw new Exception ("shape not drawn on window");
            }
        }

        void saveLastClick (object obj, Gtk.ButtonPressEventArgs args)
        {
            _mouseState = "down";
            _lastClick = PyTuple (args.Event.X, args.Event.Y);
            _lastClickFlag.Set ();
        }

        void updateMouseState (object obj, Gtk.ButtonReleaseEventArgs args)
        {
            _mouseState = "up";
        }


		// Listeners
		// TODO start: Do we use any of this stuff below? 
        public void clearListeners() {
            MotionNotifyEvent -= HandleMouseMovementOnShape;
            ButtonPressEvent -= HandleClickOnShape;
            ButtonReleaseEvent -= HandleMouseUpOnShape;
            HandleKeyPressOnShape = false;
            HandleKeyReleaseOnShape = false;
        }

        public void listen(string evt) {
            if (evt == "mouse-motion") {
                // remove if already there:
                MotionNotifyEvent -= HandleMouseMovementOnShape;
                MotionNotifyEvent += HandleMouseMovementOnShape;
            } else if (evt == "mouse-press") {
                // remove if already there:
                ButtonPressEvent -= HandleClickOnShape;
                ButtonPressEvent += HandleClickOnShape;
            } else if (evt == "mouse-release") {
                // remove if already there:
                ButtonReleaseEvent -= HandleMouseUpOnShape;
                ButtonReleaseEvent += HandleMouseUpOnShape;
            } else if (evt == "key-press") {
                HandleKeyPressOnShape = true;
            } else if (evt == "key-release") {
                HandleKeyReleaseOnShape = true;
            } else {
                throw new Exception(String.Format("invalid event '{0}': use 'mouse-motion', 'mouse-press', 'mouse-release', 'key-press', or 'key-release'.", evt));
            }
        }

        public void HandleMouseMovementOnShape (object obj,
                Gtk.MotionNotifyEventArgs args)
        {
            lock (canvas.shapes) {
                foreach (Shape shape in canvas.shapes) {
                    if (shape.hit(args.Event.X, args.Event.Y)) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
        }

        public void HandleClickOnShape (object obj, Gtk.ButtonPressEventArgs args)
        {
            lock (canvas.shapes) {
                foreach (Shape shape in canvas.shapes) {
                    if (shape.hit(args.Event.X, args.Event.Y)) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
        }

        public void HandleMouseUpOnShape (object obj,
										  Gtk.ButtonReleaseEventArgs args)
		{
            lock (canvas.shapes) {
                foreach (Shape shape in canvas.shapes) {
                    if (shape.hit(args.Event.X, args.Event.Y)) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
        }
		// TODO end.

		// Callback handlers
        private void HandleMouseMovementCallbacks (object obj,
                Gtk.MotionNotifyEventArgs args)
        {
            Event evt = new Event (args);
			Invoke (delegate {
					foreach (Func<object,Event,object> function in onMouseMovementCallbacks.ToArray()) {
						try {
                    
							Func<object,Event,object > f = (Func<object,Event,object>)function;
							f (obj, evt);
                    
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in onMouseMove function");
							Console.Error.WriteLine (e.Message);
						}        
					}
				});
        }


        private void HandleClickCallbacks (object obj,
                Gtk.ButtonPressEventArgs args)
        {
            Event evt = new Event (args);
			Invoke (delegate {
					foreach (Func<object,Event,object> function in onClickCallbacks.ToArray()) {
						try {
                    
							Func<object,Event,object > f = (Func<object,Event,object>)function;
							f (obj, evt);
                   
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in onMouseDown function");
							Console.Error.WriteLine (e.Message);
						}        
					}
				});
        }

        private void HandleMouseUpCallbacks (object obj,
                Gtk.ButtonReleaseEventArgs args)
        {
            Event evt = new Event (args);
			/* foreach (Func<object,Event,object> function in removedMouseUpCallbacks) { */
			/* 	if(onMouseUpCallbacks.Contains(function)){ */
			/* 		onMouseUpCallbacks.Remove(function); */
			/* 	} */
			/* } */
			Invoke (delegate {
					foreach (Func<object,Event,object> function in onMouseUpCallbacks.ToArray()) {
						try {
							Func<object,Event,object > f = (Func<object,Event,object>)function;
							f (obj, evt);
							
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in onMouseUp function");
							Console.Error.WriteLine (e.Message);
						}        
					}
					// JH: Drag, leave, and enter code - start
					//System.Console.Write("Handeling mouse up callbacks.\n");
					foreach(Shape shape in _clickedOnShapes){
						shape.setClickedOn(false);
						shape.handleDropCallbacks(shape, evt);
					}
					_clickedOnShapes.Clear();
				});
			// JH: Drag, leave, and enter code - end
        }

        [GLib.ConnectBefore]
        private void HandleKeyPressCallbacks (object obj,
                Gtk.KeyPressEventArgs args)
        {
            _lastKey = args.Event.Key.ToString ();
            _keyState = true;
            _keyStates[_lastKey] = true;
			// TODO Start: Do we still use the code below?
            if (HandleKeyPressOnShape){
                lock (canvas.shapes) {
                    foreach (Shape shape in canvas.shapes) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
			// TODO End.
            Event evt2 = new Event (args);
			Invoke (delegate {
					foreach (Func<object,Event,object> function in onKeyPressCallbacks.ToArray()) {
						try {
                    
							Func<object,Event,object > f = (Func<object,Event,object>)function;
							f (obj, evt2);
                    
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in onKeypress function");
							Console.Error.WriteLine (e.Message);
						}        
					}
				});
        }

        private void HandleKeyReleaseCallbacks (object obj,
                Gtk.KeyReleaseEventArgs args)
        {
            _keyState = false;
            string lastKey = args.Event.Key.ToString ();
            _keyStates[lastKey] = false;

			// TODO Start: Do we still use the code below?
            if (HandleKeyReleaseOnShape){
                lock (canvas.shapes) {
                    foreach (Shape shape in canvas.shapes) {
                        Event evt = new Event (args);
                        evt.obj = shape;
                        EventsManager.publish(evt);
                    }
                }
            }
			// TODO End.
            Event evt2 = new Event (args);
			Invoke (delegate {
					foreach (Func<object,Event,object> function in onKeyReleaseCallbacks.ToArray()) {
						try {
							Func<object,Event,object > f = (Func<object,Event,object>)function;
							f (obj, evt2);
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in onKeyRelease function");
							Console.Error.WriteLine (e.Message);
						}        
					}
				});
        }

		protected override bool OnConfigureEvent(Gdk.EventConfigure args)
		{
			base.OnConfigureEvent(args);
			Invoke (delegate {
					foreach (Func<Gdk.EventConfigure,object> function in onConfigureCallbacks.ToArray()) {
						try {
							Func<Gdk.EventConfigure,object > f = (Func<Gdk.EventConfigure,object>)function;
							f (args);
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in OnConfigureEvent function");
							Console.Error.WriteLine (e.Message);
						}        
					}
				});
			return true;
		}
		
		protected override bool OnDeleteEvent(Gdk.Event args)
		{
			base.OnDeleteEvent(args);
			Invoke (delegate {
					foreach (Func<Gdk.Event,object> function in onDeleteCallbacks.ToArray()) {
						try {
							Func<Gdk.Event,object > f = (Func<Gdk.Event,object>)function;
							f (args);
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in OnDeleteEvent function");
							Console.Error.WriteLine (e.Message);
						}        
					}
					//Console.Error.WriteLine ("Launching on-delete event");
				});
			return false;
		}

		protected void OnDestroyEvent(object obj, EventArgs args)
		{
            Event evt = new Event (args);
			Invoke (delegate {
					foreach (Func<object,Event,object> function in onDestroyCallbacks.ToArray()) {
						try {
                    
							Func<object,Event,object > f = (Func<object,Event,object>)function;
							f (obj, evt);
                    
						} catch (Exception e) {
							Console.Error.WriteLine ("Error in OnDestroy function");
							Console.Error.WriteLine (e.Message);
						}        
					}
				});
			//Console.WriteLine("OnDestroy");
			//Application.Quit();
		}

		
	    // Add callbacks
        public Func<object,Event,object> onMouseUp (Func<object,Event,object> function)
        {
            onMouseUpCallbacks.Add (function);
			return function;
        }

        public Func<object,Event,object> onMouseMovement (Func<object,Event,object> function)
        {
            onMouseMovementCallbacks.Add (function);
			return function;
        }

		public Func<object,Event,object> onMouseDown (Func<object,Event,object> function)
        {
            onClickCallbacks.Add (function);
			return function;
        }

		public Func<object,Event,object> onClick (Func<object,Event,object> function)
        {
            onClickCallbacks.Add (function);
			return function;
        }

        public Func<object,Event,object> onKeyPress (Func<object,Event,object> function)
        {
            onKeyPressCallbacks.Add (function);
			return function;
        }

        public Func<object,Event,object> onKeyRelease (Func<object,Event,object> function)
        {
            onKeyReleaseCallbacks.Add (function);
			return function;
        }

        public Func<Gdk.EventConfigure,object> onConfigure (Func<Gdk.EventConfigure,object> function)
        {
            onConfigureCallbacks.Add (function);
			return function;
        }

		public Func<Gdk.Event,object> onDelete (Func<Gdk.Event,object> function)
        {
            onDeleteCallbacks.Add (function);
			return function;
        }

		public Func<object, Event,object> onDestroy (Func<object, Event,object> function)
        {
            onDestroyCallbacks.Add (function);
			return function;
        }

		
		// Remove callbacks
		public void removeMouseUp (Func<object,Event,object> function)
        {
			Invoke (delegate {
					onMouseUpCallbacks.Remove(function);
				});
        }

		public void removeMouseMovement (Func<object,Event,object> function)
        {
			Invoke (delegate {
					onMouseMovementCallbacks.Remove (function);
				});
        }

        public void removeKeyPress (Func<object,Event,object> function)
        {
			Invoke (delegate {
					onKeyPressCallbacks.Remove (function);
				});
        }

        public void removeKeyRelease (Func<object,Event,object> function)
        {
			Invoke (delegate {
					onKeyReleaseCallbacks.Remove (function);
				});
        }

        public void removeClick (Func<object,Event,object> function)
        {
			Invoke (delegate {
					onClickCallbacks.Remove (function);
				});
        }

        public void removeMouseDown (Func<object,Event,object> function)
        {
			Invoke (delegate {
					onClickCallbacks.Remove (function);
				});
        }

        public void removeConfigure (Func<Gdk.EventConfigure,object> function)
        {
			Invoke (delegate {
					onConfigureCallbacks.Remove (function);
				});
        }

		public void removeDelete (Func<Gdk.Event,object> function)
        {
			Invoke (delegate {
					onDeleteCallbacks.Remove (function);
				});
        }

		public void removeDestroy (Func<object, Event,object> function)
        {
			Invoke (delegate {
					onDestroyCallbacks.Remove (function);
				});
        }

        public void run (Func<object> function)
        {
            try {
                // FIXME: Understand why:
                // This does not like a Gtk Application Invoke here
				//
				// JH (fixed): Because usually, the function would be something
				// like:
				//
				// while True:
				//     # do stuff
				//
				// Gtk Application Invoke will then move that function to
				// the Calico event thread, and execute it as an event. However,
				// because the function contains a "while True" loop, it never
				// terminates, meaning Calico now becomes entirely unresponsive.
                function ();
            } catch (Exception e) {
                if (!e.Message.Contains ("Thread was being aborted")) {
                    Console.Error.WriteLine ("Error in run function");
                    Console.Error.WriteLine (e.Message);
                }
            }
        }

        public void run () {
            run(.01);
        }

        public void run (double step_time)
        {
            requestStop = false;
            while (! requestStop) {
                try {
                    step (step_time);
                } catch {
                    requestStop = true;
                }
            }
        }


        public double updateInterval {
            get {
                return _update_interval;
            }
            set {
                _update_interval = value;
            }
        }

        public double getUpdateInterval ()
        {
            return updateInterval;
        }
        public void setUpdateInterval (double u)
        {
            updateInterval = u;
        }

        public int height {
            get {
                int _width = 0, _height = 0;
                InvokeBlocking( delegate {
                    if (Child == _canvas || Child == widget) {
                        this.GetSize (out _width, out _height);
                    } else { // scrollbars
                        _height = _canvas.height;
                    }
                });
                return _height;
            }
        }

        public int _height {
            get {
                int _width = 0, _height = 0;
                if (Child == _canvas || Child == widget) {
                    this.GetSize (out _width, out _height);
                } else { // scrollbars
                    _height = _canvas.height;
                }
                return _height;
            }
        }

        public int width {
            get {
                int _width = 0, _height = 0;
                InvokeBlocking( delegate {
                    if (Child == _canvas || Child == widget) {
                        this.GetSize (out _width, out _height);
                    } else { // scrollbars
                        _width = _canvas.width;
                    }
                });
                return _width;
            }
        }

        public int _width {
            get {
                int _width = 0, _height = 0;
                if (Child == _canvas || Child == widget) {
                    this.GetSize (out _width, out _height);
                } else { // scrollbars
                    _width = _canvas.width;
                }
                return _width;
            }
        }

        public Canvas getCanvas ()
        {
            return _canvas;
        }

        public Canvas canvas {
            get {
                return _canvas;
            }
        }

        public PythonTuple getMouse ()
        {
            _lastClickFlag.Reset();
            _lastClickFlag.WaitOne ();
            return _lastClick;
        }

        public PythonTuple getMouseNow ()
        {
            int x = 0, y = 0;
            InvokeBlocking (delegate { 
                GetPointer (out x, out y);
            });
            return PyTuple (x, y);
        }

        public string getMouseState ()
        {
            return _mouseState;
        }

        public string getLastKey ()
        {
            string lk = _lastKey;
            //_lastKey = ""; //consume the event
            return lk;
        }

        public bool getKeyPressed ()
        {
            return _keyState;
        }

        public bool getKeyPressed (string key)
        {
            return _keyStates.ContainsKey(key) ? _keyStates[key]: false;
        }

        public new void Show ()
        {
            Invoke (delegate { 
                DateTime now = DateTime.Now;
                last_update = now;
                _dirty = false;
                if (gui_thread_id != -1)
                base.Show (); 
            });
        }

        public new void ShowAll ()
        {
            Invoke (delegate { 
                DateTime now = DateTime.Now;
                last_update = now;
                _dirty = false;
                if (gui_thread_id != -1)
                base.ShowAll (); 
            });
        }

        public new void Resize (int width, int height)
        {
            _canvas.resize (width, height);
            Invoke (delegate {
                base.Resize (width, height);
            });
        }

        public void need_to_redraw ()
        { 
            _dirty = true;
            DateTime now = DateTime.Now;
            // diff is TimeSpan
            if ((now - last_update).TotalMilliseconds < (updateInterval * 1000)) {
                // pass, too soon!
                // but we need to make sure that someone checks
                // in the future. 
                if (timer_running) {
                    // already one running!
                    // we'll just wait
                } else {
                    // let's spawn one to check in 100 ms or so
                    timer_running = true;
                    GLib.Timeout.Add ((uint)(updateInterval * 1000), 
                            new GLib.TimeoutHandler (_redraw_now));
                }
            } else { // it is not too soon
                if (timer_running) {
                    // no need to start another
                } else {
                    // let's spawn one to check in 100 ms or so
                    timer_running = true;
                    GLib.Timeout.Add ((uint)(updateInterval * 1000), 
                            new GLib.TimeoutHandler (_redraw_now));
                }
            }
        }

        private bool _redraw_now ()
        {
            DateTime now = DateTime.Now;
            if (_dirty) {
                last_update = now;
                _dirty = false;
                QueueDraw (); // gtk
            }
            timer_running = false;
            return false; // return true to try again
        }

        public void setMode(String mode) {
            this.mode = mode;
        }

        public string getMode ()
        {
            return _canvas.mode;
        }

        public string mode {
            get {
                return _canvas.mode;
            }
            set {
                if (value == "auto" || value == "manual" || value == "physics" || 
		    value == "bitmap" || value == "bitmapmanual" || value == "physicsmanual")
                    canvas.mode = value;
                else
                    throw new Exception ("window mode must be 'auto', 'manual', 'bitmap', 'bitmapmanual', 'physicsmanual', or 'physics'");
            }
        }

		public void handleEvents()
		{
            while (Gtk.Application.EventsPending()){
                Gtk.Application.RunIteration ();
			}
		}

        public void updateNow ()
        { // Window
            need_to_redraw ();
            // Manual call to update, let's update then:
			handleEvents();
        }

        public void update ()
        { // Window
            if (mode == "manual" || mode == "bitmapmanual" || mode == "physicsmanual") {
                _canvas.need_to_draw_surface = true;
                Invoke( delegate {
                    QueueDraw ();
                });
            } else {
                need_to_redraw ();
            }
        }

        public void step ()
        { // Window
            step (0);
        }

        public void step (double step_time)
        { // Window, in seconds
            // Same as update, but will make sure it 
            // doesn't update too fast.
            // handle physics
            if (_canvas == null) {
                requestStop = true;
                return;
            }
            // kjo
            _canvas.need_to_draw_surface = true;

            if (mode == "physics" || mode == "physicsmanual") {
                lock(_canvas.world){
                    _canvas.world.Step ((float)simulationStepTime);
                }
                time += simulationStepTime; 
                // update the sprites
                lock (_canvas.shapes) {
                    foreach (Shape shape in _canvas.shapes) {
                        shape.updateFromPhysics ();
                    }
                }
            }

            //JH: Patching the framerate limiter start
            // and now the update
            DateTime now = DateTime.Now;
            // diff is TimeSpan, converted to seconds:
            double diff = (now - last_update).TotalMilliseconds / 1000.0;
            double remain = step_time - diff; //JH: calculate the remaining time
            while (remain > 0) { // seconds   //JH: spin on remaining time
                //System.Console.Write(".");
                //System.Console.Write("spinning...");
                if (remain > 0.01) {
                    //JH: wait based on the remaining time!
                    //JH: Previous bug made it such that the longer the update took, the longer you would have to wait!
                    //System.Console.Write("sleeping...");
                    Thread.Sleep ((int)(remain / 2 * 1000));
                } //Else just spin-lock
                //System.Console.Write("+");

                //JH: Recalculate the differences
                now = DateTime.Now;
                diff = (now - last_update).TotalMilliseconds / 1000.0;
                remain = step_time - diff;
            }
            //System.Console.Write("Waited: ");
            //System.Console.Write(diff.ToString());
            //System.Console.Write("\n");
            last_update = DateTime.Now;
            //JH: Patching the framerate limiter end

            if (mode == "bitmapmanual") {
                using (Cairo.Context g = new Cairo.Context(_canvas.finalsurface)) {
                    g.Save ();
                    g.Operator = Cairo.Operator.Source;
                    g.SetSourceSurface (_canvas.surface, 0, 0);
                    g.Paint ();
                    g.Restore ();
                }      
            }      

            _dirty = false;
            InvokeBlocking (delegate { 
                try {
                    QueueDraw ();
                    GdkWindow.ProcessUpdates (true);
                    //System.Console.Write("!");
                } catch {
                    requestStop = true;
                }
            });
        }

        public void refresh()
        { // Window, in seconds
            // Same as update, but will make sure it 
            // doesn't update too fast.
            // handle physics
            // kjo
            _canvas.need_to_draw_surface = true;

            if (mode == "physics" || mode == "physicsmanual") {
                // update the sprites
                lock (_canvas.shapes) {
                    foreach (Shape shape in _canvas.shapes) {
                        shape.updateFromPhysics ();
                    }
                }
            }
            if (mode == "bitmapmanual") {
                using (Cairo.Context g = new Cairo.Context(_canvas.finalsurface)) {
                    g.Save ();
                    g.Operator = Cairo.Operator.Source;
                    g.SetSourceSurface (_canvas.surface, 0, 0);
                    g.Paint ();
                    g.Restore ();
                }      
            }      

            _dirty = false;
            InvokeBlocking (delegate { 
                try {
                    QueueDraw ();
                    GdkWindow.ProcessUpdates (true);
                    //System.Console.Write("!");
                } catch {
                    requestStop = true;
                }
            });
        }

        public override string ToString ()
        {
            return String.Format ("<Window (title='{0}',width={1},height={2})>", 
                    Title, width, height);
        }
        public string __repr__ ()
        {
            return ToString();
        }

        public IDictionary<string, string> GetRepresentations() {
            IDictionary<string, string> retval = new Picture (this).GetRepresentations();
            retval["text/plain"] =  this.ToString();
            IDictionary<string, string> svg_rep = toSVG().GetRepresentations();
            foreach(KeyValuePair<string,string> kvp in (IDictionary<string,string>)svg_rep) {
                retval[kvp.Key] = kvp.Value;
            }
            return retval;
        }
    }

    [method: JigsawTab("G/Windows")]
    public static void ShowAll (object o)
    {
        if (gui_thread_id != -1)
            Invoke (delegate {
                ((Gtk.Widget)o).ShowAll (); });
    }

    [method: JigsawTab("G/Windows")]
    public static void Show (object o)
    {
        if (gui_thread_id != -1)
            Invoke (delegate {
                ((Gtk.Widget)o).Show (); });
    }

    [method: JigsawTab(null)]
    public static Vector2 VectorRotate (Vector2 v, double angle)
    {
        Microsoft.Xna.Framework.Matrix m = Microsoft.Xna.Framework.Matrix.CreateRotationZ ((float)angle);
        return Vector2.Transform (v, m);
    }

    [method: JigsawTab(null)]
    public static Vector2 Vector (int x, int y)
    {
        return new Vector2 ((float)x, (float)y);
    }

    [method: JigsawTab(null)]
    public static Vector2 Vector (double x, double y)
    {
        return new Vector2 ((float)x, (float)y);
    }

    public class Point : IList
    {
        public double x;
        public double y;

        public double gx;
        public double gy;

        public Point (IList iterable): this(iterable[0], iterable[1])
        {
        }

        public Point (object x, object y)
        {
            this.x = System.Convert.ToDouble (x);
            this.y = System.Convert.ToDouble (y);
        }

        public Point (int x, int y)
        {
            this.x = System.Convert.ToDouble (x);
            this.y = System.Convert.ToDouble (y);
        }

        public Point (double x, double y)
        {
            this.x = x;
            this.y = y;
        }

        public Point (Dot dot)
        {
            this.x = dot.x;
            this.y = dot.y;
        }

        public double getX()
        {
            return x;
        }

        public double getY()
        {
            return y;
        }

        public void setX(double x)
        {
            this.x = x;
        }

        public void getY(double y)
        {
            this.y = y;
        }

        public static double operator -(Point p1, Point p2) {
            return p1.distance(p2);
        }

        public double distance (Point p)
        {
            return Math.Sqrt (Math.Pow (x - p.x, 2) + Math.Pow (y - p.y, 2));
        }

        public override string ToString ()
        {
            return String.Format ("<Point (x={0},y={1})>", x, y);
        }

        public string __repr__ ()
        {
            return String.Format ("<Point (x={0},y={1})>", x, y);
        }

        public void drawAt (WindowClass window, IList iterable)
        {
            throw new Exception ("Can't draw a point; use Dot instead");
        }

        public void draw (WindowClass window)
        {
            throw new Exception ("Can't draw a point; use Dot instead");
        }

        // Items necessary for it to be an IList
        public bool IsFixedSize {
            get {
                return true;
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

        public void CopyTo (System.Array array, int index)
        {
        }

        public int Add (object value)
        {
            return 1; // should return count
        }

        public int Count {
            get {
                return 2;
            }
        }

        public void Remove (object value)
        {
        }

        public void RemoveAt (int index)
        {
        }

        public void Clear ()
        {
        }

        public bool Contains (object value)
        {
            return false;
        }

        public int IndexOf (object value)
        {
            return -1;
        }

        public void Insert (int index, object value)
        {
        }

        public object this [int index] {
            get {
                if (index == 0) {
                    return x;
                } else if (index == 1) {
                    return y;
                } else {
                    throw new Exception ("invalid access of Point");
                }
            }
            set { // value is the item
                if (index == 0) {
                    x = System.Convert.ToDouble (value);
                } else if (index == 1) {
                    y = System.Convert.ToDouble (value);
                }
            }
        }

        public object SyncRoot {
            get {
                return this;
            }
        }

        public IEnumerator GetEnumerator ()
        {
            // Refer to the IEnumerator documentation for an example of
            // implementing an enumerator.
            throw new Exception ("The method or operation is not implemented.");
        }
    }

    public class Canvas : Gtk.Layout
    {

        // Shape.draw() will add them here:
        public List<Shape> shapes = new List<Shape> ();
        private string _mode;
        public FarseerPhysics.Dynamics.World world;
        public object document;
        public int width = 800, height = 600 ;
        public Cairo.ImageSurface surface = null;
        public Cairo.ImageSurface finalsurface;
        public bool need_to_draw_surface = false;
        public Color background_color = null;

        public Canvas (int width, int height) : this("bitmap", width, height)
        {
        }

        public Canvas (string mode, int width, int height) : base(null, null)
        {
            this.mode = mode;
            resize (width, height);
        }

        public Canvas (string mode, Gtk.Adjustment h, Gtk.Adjustment v) : base(h, v)
        {
            this.mode = mode;
            resize (width, height);
        }

        public string mode {
            get {
                return _mode;
            }
            set {
                if (value == "manual" || value == "auto" || value == "physics" || 
                        value == "bitmap" || value == "bitmapmanual" || value == "physicsmanual") {
                    _mode = value;

                    if (value == "physics" || value == "physicsmanual")
                        initPhysics ();
                    resetSurfaces ();

                } else
                    throw new Exception ("canvas mode must be 'manual', 'auto', 'bitmap', 'bitmapmanual', 'physicsmanual' or 'physics'");
            }
        }

        public void setMode(string m)
        {
            mode = m;
        }

        public string getMode()
        {
            return mode;
        }

        public void setBackground(Graphics.Color color) {
            background_color = color;
            Gdk.Color bg = new Gdk.Color ((byte)color.red, 
                    (byte)color.green, 
                    (byte)color.blue);
            if (mode == "bitmap" || mode == "bitmapmanual") {
                var r = new Rectangle(new List<int> {0,0}, new List<int> {width,height});
                r.color = background_color;
                r.draw(this);
            }
            ModifyBg (Gtk.StateType.Normal, bg);
        }

        void initPhysics ()
        {
            world = new FarseerPhysics.Dynamics.World (new Vector2 (0.0f, 9.8f));
        }

        void resetSurfaces ()
        {
            surface = new Cairo.ImageSurface (Cairo.Format.Argb32, width, height);    
            if (mode == "bitmapmanual") {
                finalsurface = new Cairo.ImageSurface (Cairo.Format.Argb32, width, height);
            } else {
                finalsurface = surface;               
            }      
        }

        public void resize (int width, int height)
        {
            this.width = width;
            this.height = height;
            SetSize((uint)width, (uint)height);
            resetSurfaces ();
        }

        protected override bool OnExposeEvent (Gdk.EventExpose args)
        {
            using (Cairo.Context g = Gdk.CairoHelper.Create(args.Window)) {
                if (need_to_draw_surface) {
                    g.Save ();
                    g.SetSourceSurface (finalsurface, 0, 0);
                    g.Paint ();
                    g.Restore ();
                }
                List<Shape> s = new List<Shape>(shapes);
                foreach (Shape shape in s) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
            }
            return base.OnExposeEvent (args);
        }

        public Calico.Representation toSVG()
        {            
            string fileName = System.IO.Path.GetTempPath() + Guid.NewGuid().ToString() + ".svg";
            saveToSVG(fileName);
            System.IO.TextReader reader = new System.IO.StreamReader(fileName);
            string text = reader.ReadToEnd();
            reader.Close();
            return Calico.Representation.SVG(text);
        }

        public void saveToSVG(string filename)
        {
            var svg = new Cairo.SvgSurface(filename, width, height);
            using (Cairo.Context g = new Cairo.Context(svg)) {
                if (mode == "bitmap" || mode == "bitmapauto"){
                    Cairo.ImageSurface surface = finalsurface;
                    g.SetSourceSurface(surface, 0, 0);
                    g.Paint();
                } else {
                    if (background_color != null) {
                        // draw background
                        Rectangle background = new Rectangle(new Point(0,0), new Point(width, height));
                        background.color = background_color;
                        background.render(g);
                    }
                    List<Shape> s = new List<Shape>(shapes);
                    foreach (Shape shape in s) {
                        shape.render (g);
                    }
                }
            }
            svg.Finish();
        }

        public IDictionary<string, string> GetRepresentations() {
            IDictionary<string, string> retval = new Picture (this).GetRepresentations();
            retval["text/plain"] =  this.ToString();
            return retval;
        }

        public Canvas draw(Shape shape) {
            shape.draw(this);
            return this;
        }

    }

    public class Shape
    {
        public Point center;
        public string tag;
        public WindowClass window;

		//JH: This is part of the great experiment
		public DockableWindowClass dockableWindow;


        public Shape drawn_on_shape = null;
        internal double _rotation; // internally radians
        internal double _scaleFactor; // percent
        public FarseerPhysics.Dynamics.World world;
        public FarseerPhysics.Dynamics.Body body;
        internal FarseerPhysics.Dynamics.BodyType _bodyType;
        internal float _bounce;
        internal float _friction;
        internal float _density;
        public List<Shape> shapes = new List<Shape> ();
        public List<Shape> joints = new List<Shape> ();
        public double gx = 0.0;
        public double gy = 0.0;
        public double z = 0.0; // height off ground (3D) 0 - 1
        public double zHeight = 1.0; // height of shape (3D) 0 - 1
        public bool visible = true;
        public Point[] points;
        internal Color _fill;
        internal Color _outline;
        public Gradient _gradient;
        private int _border;
        public bool wrap = false;
        private Pen _pen;
        private bool _has_pen;
        internal bool close_path = true;
        SpeechBubble speechBubble = null;
		public object UserData = null;
		public bool clip = false;
		// JH: Drag, leave, and enter code - start
		protected bool _clickedOn = false;
		protected bool _hoveredOver = false;
		protected List _clickCallbacks = new List();
		protected List _releaseCallbacks = new List();
		protected List _moveCallbacks = new List();
		protected List _enterCallbacks = new List();
		protected List _leaveCallbacks = new List();
		protected List _dragCallbacks = new List();
		protected List _dropCallbacks = new List();
		protected Func<object,Event,object> _clickHandler = null;
		protected Func<object,Event,object> _moveHandler = null;
		protected Func<object,Event,object> _releaseHandler = null;

		protected void handleClickCallbacks (object obj, Event evt)
        {
			/* System.Console.Write("Click callbacks executed\n"); */
            foreach (Func<object,Event,object> function in _clickCallbacks) {
                try {
					Func<object,Event,object > f;
					f = (Func<object,Event,object>)function;
					f (obj, evt);
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in ClickCallback function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

		protected void handleReleaseCallbacks (object obj, Event evt)
        {
			/* System.Console.Write("Release callbacks executed\n"); */
            foreach (Func<object,Event,object> function in _releaseCallbacks) {
                try {
					Func<object,Event,object > f;
					f = (Func<object,Event,object>)function;
					f (obj, evt);
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in ReleaseCallback function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }
		
        protected void handleMoveCallbacks (object obj, Event evt)
        {
			/* System.Console.Write("Move callbacks executed\n"); */
            foreach (Func<object,Event,object> function in _moveCallbacks) {
                try {
					Func<object,Event,object > f;
					f = (Func<object,Event,object>)function;
					f (obj, evt);
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in MoveCallback function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }
		
        protected void handleEnterCallbacks (object obj, Event evt)
        {
			/* System.Console.Write("Enter callbacks executed\n"); */
            foreach (Func<object,Event,object> function in _enterCallbacks) {
                try {
                    Invoke (delegate {
							Func<object,Event,object > f;
							f = (Func<object,Event,object>)function;
							f (obj, evt);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in EnterCallback function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

        protected void handleLeaveCallbacks (object obj, Event evt)
        {
			/* System.Console.Write("Leave callbacks executed\n"); */
            foreach (Func<object,Event,object> function in _leaveCallbacks) {
                try {
                    Invoke (delegate {
							Func<object,Event,object > f;
							f = (Func<object,Event,object>)function;
							f (obj, evt);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in LeaveCallback function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

        protected void handleDragCallbacks (object obj, Event evt)
        {
			/* System.Console.Write("Drag callbacks executed\n"); */
            foreach (Func<object,Event,object> function in _dragCallbacks) {
                try {
                    Invoke (delegate {
							Func<object,Event,object > f;
							f = (Func<object,Event,object>)function;
							f (obj, evt);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in DragCallback function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

		internal void handleDropCallbacks (object obj, Event evt)
        {
			/* System.Console.Write("Drop callbacks executed\n"); */
            foreach (Func<object,Event,object> function in _dropCallbacks) {
                try {
                    Invoke (delegate {
							Func<object,Event,object > f;
							f = (Func<object,Event,object>)function;
							f (obj, evt);
                    });
                } catch (Exception e) {
                    Console.Error.WriteLine ("Error in DropCallback function");
                    Console.Error.WriteLine (e.Message);
                }        
            }
        }

		public bool isHoveredOver(){
			return _hoveredOver;
		}

		public void setHoveredOver(bool value){
			_hoveredOver = value;
		}

		public bool isClickedOn(){
			return _clickedOn;
		}

		public void setClickedOn(bool value){
			_clickedOn = value;
		}
		
		// JH: Drag, leave, and enter code - end

		// JH: Copy - start
		/**
		 * JH: Some notes about copying:
		 * - When copying an object, the copy still has to be drawn to a
		 *   window, even if the original was already drawn to a window.
		 *   This probably will feel more intuitive than having copies 
		 *   automatically draw themselves to a window, but I can see arguments
		 *   for either solution.
		 * - Copying of physics objects is not currently supported, so
		 *   properties applied to the original physics object may not be 
		 *   inherited by the copy.
		 * - Connected functions are not copied, meaning you will have to 
		 *   reconnect the copy. I believe this shouldn't be a problem in 
		 *   general, as you'll generally want to connect a different function
		 *   to the copy.
		 * - Pen, Joint, and SpeechBubbles are currently not copied, as that
		 *   requires extra work, and probably will never come up in practice.
		 */
		public void copyAttributes(Shape other){
			//System.Console.Write("Copying attributes\n");
			tag = other.tag;
			window = other.window;
			//JH: This is part of the great experiment
			dockableWindow = other.dockableWindow;
			drawn_on_shape = other.drawn_on_shape;
			_rotation = other._rotation; // internally radians
			_scaleFactor = other._scaleFactor; // percent
			//System.Console.Write("- Copying windows and tag - done\n");
			//System.Console.Write("- Copying points - start\n");
			set_points(other.points);
			//System.Console.Write("- Copying points - done\n");
			center = new Point(other.center);

			// These attributes should be recreated by adding this object to
			// physics.
			//public FarseerPhysics.Dynamics.World world;
			//public FarseerPhysics.Dynamics.Body body;
			_bodyType = other._bodyType;
			_bounce = other._bounce;
			_friction = other._friction;
			_density = other._density;
			//System.Console.Write("- Copying physics attributes - done\n");

			// To copy these attributes, we need to create a copy of each shape
			// drawn onto the other shape.
			//public List<Shape> shapes = new List<Shape> ();
			//public List<Shape> joints = new List<Shape> ();
			foreach(Shape shape in other.shapes){
				shapes.Add(shape.copy());
			}
			gx = other.gx;
			gy = other.gy;
			z = other.z; // height off ground (3D) 0 - 1
			zHeight = other.zHeight; // height of shape (3D) 0 - 1
			visible = other.visible;
			// We need to create new points for each
			//public Point[] points;

			//System.Console.Write("- Copying colors - start\n");
			_fill = null;
			_outline = null;
			_gradient = null;
			if(other._fill != null) _fill = new Color(other._fill);
			if(other._outline != null) _outline = new Color(other._outline);
			if(other._gradient != null) _gradient = new Gradient(other._gradient);
			//System.Console.Write("- Copying colors - done\n");
			_border = other.border;
			wrap = other.wrap;
			//System.Console.Write("- Copying color and position attributes - done\n");
			
			// JH: For now, I will not attempt to copy the pen; I doubt there will
			// be any practical situation where this will be necessary.
			//_pen = new Pen(other.pen);
			//_has_pen = other._has_pen;
			close_path = other.close_path;

			// JH: For now, I will not attept to copy the speech bubble; I doubt
			// there will be any practical situtation where this will be
			// necessary.
			//speechBubble = new SpeechBubble(other.speechBubble);
			
			UserData = other.UserData;
			clip = other.clip;
			//_clickedOn = other._clickedOn;
			//_hoveredOver = other._hoveredOver;
			//_hoveredTrackerRegistered = other._hoveredTrackerRegistered;
			// We need to copy these callback-by-callback
			//protected List _enterCallbacks = new List();
			//protected List _leaveCallbacks = new List();
			//foreach(Object callback in _enterCallbacks){
			//	_enterCallbacks.Add(callback);
			//}
			//foreach(Object callback in _leaveCallbacks){
			//	_leaveCallbacks.Add(callback);
			//}
			//System.Console.Write("- Copying callbacks - done\n");

			// If the other shape was added to the physics simulator, we should
			// add ourselve as well.
			//if(other.body != null){
			//	addToPhysics();
			//}
			//System.Console.Write("Copying attributes end\n");
		}

		public virtual Shape copy(){
			Shape shapeCopy = new Shape();
			shapeCopy.copyAttributes(this);
			return shapeCopy;
		}

        public Shape (bool has_pen=true)
        {
            _rotation = 0;
            _scaleFactor = 1.0;
            center = new Point (0, 0);
            this.has_pen = has_pen;
            if (this.has_pen) 
                pen = new Pen (new Color (0, 0, 0), 1);
            color = new Color ("purple");
            outline = new Color ("black");
            border = 1;
            _bounce = 0.8f;
            _friction = 0.5f;
            _density = 1.0f;
            _bodyType = FarseerPhysics.Dynamics.BodyType.Dynamic;
        }

		//JH: This is part of the great experiment
		public Canvas getCanvas(){
			if(window != null){
				return window._canvas;
			} else if(dockableWindow != null){
				return dockableWindow._canvas;
			} else {
				return null;
			}
		}


        // FIXME: points are in relative to center coordinates
        // FIXME: set x,y of points should go from screen_coords to relative
        // FIXME: should call QueueDraw on set

        public int subscribe(string message, Func<object,Event,object> procedure) {
            List<string> messages = new List<string>() {"mouse-press", "mouse-release", "mouse-motion", "key-press", "key-release"};
            if (messages.Contains(message)) {
                if (window != null) {
                    window.listen(message);
                } else {
                    throw new Exception(String.Format("subscribe('{0}', ...) should be called after draw()", message));
                }
                return Events.subscribe(message, procedure, this);
            } else {
                return Events.subscribe(message, procedure, this);
            }
        }

        public virtual object makeJointTo(Shape other, string joint_type, params float [] args) {
            if (joint_type != "angle" &&
                    joint_type != "distance" &&
                    //joint_type != "fixed-angle" &&
                    //joint_type != "fixed-distance" &&
                    //joint_type != "fixed-friction" &&
                    //joint_type != "fixed-prismatic" &&
                    //joint_type != "fixed-revolute" &&
                    joint_type != "friction" &&
                    //joint_type != "gear" &&
                    joint_type != "line" &&
                    joint_type != "prismatic" &&
                    joint_type != "pulley" &&
                    joint_type != "revolute" &&
                    joint_type != "slider" &&
                    joint_type != "weld") {
                throw new Exception(String.Format("invalid joint type: '{0}'; should be one of: {1}", 
                            joint_type,
                            "'angle', 'distance', 'friction', 'gear', 'line', 'prismatic', 'pully', 'revolute', 'slider', or 'weld'"));
            }
            joints.Add(other);
            if (joint_type == "angle") {
                // CreateAngleJoint 
                return FarseerPhysics.Factories.
                    JointFactory.
                    CreateAngleJoint( 
                            world, // world
                            this.body.FixtureList[0].Body, // body1
                            other.body.FixtureList[0].Body); // body2
            } else if (joint_type == "distance") {
                // CreateDistanceJoint
                //var body1 = this.body.FixtureList[0].Body;
                //var body2 = other.body.FixtureList[0].Body;
                //Vector2 sep1 = body2.Position - body1.Position;   
                //Vector2 sep2 = body1.Position - body2.Position;

                if(args.Length==4)
                {  

                    return FarseerPhysics.Factories.JointFactory.CreateDistanceJoint(
                            world, // world 
                            this.body.FixtureList[0].Body, // body1
                            other.body.FixtureList[0].Body, // body2
                            new Vector2(args[0],args[1]),
                            new Vector2(args[2],args[3]));
                }
                else
                {


                    return FarseerPhysics.Factories.
                        JointFactory.
                        CreateDistanceJoint( 
                                world, // world
                                this.body.FixtureList[0].Body, // body1
                                other.body.FixtureList[0].Body, // body2
                                Vector2.Zero,
                                Vector2.Zero);
                }
                //Vector2.Multiply(sep1, .5f),  // anchor 1
                //Vector2.Multiply(sep2, .5f));  // anchor 2
                //} else if (joint_type != "fixed-angle") {
                // CreateFixedAngleJoint
                //} else if (joint_type != "fixed-distance") {
                // CreateFixedDistanceJoint
                //} else if (joint_type != "fixed-friction") {
                // CreateFixedFrictionJoint
                //} else if (joint_type != "fixed-prismatic") {
                // CreateFixedPrismaticJoint
                //} else if (joint_type != "fixed-revolute") {
                // CreateFixedRevoluteJoint

        } else if (joint_type == "friction") {
            // CreateFrictionJoint
            return FarseerPhysics.Factories.
                JointFactory.
                CreateFrictionJoint( 
                        world, // world
                        this.body.FixtureList[0].Body, // body1
                        other.body.FixtureList[0].Body, // body2
                        Vector2.Zero,  // anchor 2
                        Vector2.Zero);  // axis
            //} else if (joint_type != "gear") {
            // CreateGearJoint
        } else if (joint_type == "line") {
            // CreateLineJoint 
            return FarseerPhysics.Factories.
                JointFactory.
                CreateLineJoint( 
                        world, // world
                        this.body.FixtureList[0].Body, // body1
                        other.body.FixtureList[0].Body, // body2
                        Vector2.Zero,  // anchor 2
                        Vector2.Zero);  // axis
        } else if (joint_type == "prismatic") {
            // CreatePrismaticJoint
            return FarseerPhysics.Factories.
                JointFactory.
                CreatePrismaticJoint( 
                        world, // world
                        this.body.FixtureList[0].Body, // body1
                        other.body.FixtureList[0].Body, // body2
                        Vector2.Zero,  // anchor 2
                        Vector2.Zero);  // axis
        } else if (joint_type == "pulley") {
            // CreatePulleyJoint
            return FarseerPhysics.Factories.
                JointFactory.
                CreatePulleyJoint(world, 
                        this.body.FixtureList[0].Body, // body1
                        other.body.FixtureList[0].Body, // body2
                        Vector2.Zero,  // groundAnchorA,
                        Vector2.Zero,  // groundAnchorB, 
                        Vector2.Zero, // anchorA, 
                        Vector2.Zero, // anchorB, 
                        args[0]); // float ratio
        } else if (joint_type == "revolute") {
            // CreateRevoluteJoint
            return FarseerPhysics.Factories.
                JointFactory.
                CreateRevoluteJoint( 
                        world, // world
                        this.body.FixtureList[0].Body, // body1
                        other.body.FixtureList[0].Body, // body2
                        Vector2.Zero);  // anchor1
        } else if (joint_type == "slider") {
            // CreateSliderJoint
            return FarseerPhysics.Factories.
                JointFactory.
                CreateSliderJoint( 
                        world, // world
                        this.body.FixtureList[0].Body, // body1
                        other.body.FixtureList[0].Body, // body2
                        Vector2.Zero,  // anchor1
                        Vector2.Zero,  // anchor2
                        args[0],  // min distance
                        args[1]); // max distance
        } else if (joint_type == "weld") {
            // CreateWeldJoint
            //var body1 = this.body.FixtureList[0].Body;
            //var body2 = other.body.FixtureList[0].Body;
            //Vector2 sep1 = body2.Position - body1.Position;   
            //Vector2 sep2 = body1.Position - body2.Position;

            if(args.Length==4)
            {  


                return FarseerPhysics.Factories.
                    JointFactory.
                    CreateWeldJoint( 
                            world, // world
                            this.body.FixtureList[0].Body, // body1
                            other.body.FixtureList[0].Body, // body2
                            new Vector2(args[0],args[1]),
                            new Vector2(args[2],args[3]));


            }
            else
            {

                return FarseerPhysics.Factories.
                    JointFactory.
                    CreateWeldJoint( 
                            world, // world
                            this.body.FixtureList[0].Body, // body1
                            other.body.FixtureList[0].Body, // body2
                            Vector2.Zero,  // anchor 1
                            Vector2.Zero);  // anchor 2
            }


        }
        return null;
        }

        public virtual void setPenColor(Color color) {
            penUp();
            pen.color = color;
            penDown();
        }

        public void connect (string signal, Func<object,Event,object> function)
        {
			if(window == null){
				throw new Exception ("Shape needs to be drawn before calling connect.");
			}
			
			if(signal == "move" || signal == "drag" ||
			   signal ==  "enter" || signal == "leave")
			{
				/* System.Console.Write("Move handler requested\n"); */
				if(_moveHandler == null){
					/* System.Console.Write("Move handler registered\n"); */
					_moveHandler = window.onMouseMovement (
						delegate (object obj, Event evt) {
							if (hit (evt.x, evt.y)) {
								if(!_hoveredOver){
									// Call enter callback
									handleEnterCallbacks(this, evt);
									_hoveredOver = true;
								}
							} else {
								if(_hoveredOver){
									// Call leave callback
									handleLeaveCallbacks(this, evt);
									_hoveredOver = false;
								}
							}
							if (_clickedOn) {
								handleDragCallbacks(this, evt);
							}
							handleMoveCallbacks(this, evt);
							return true;
						});
				}
			}

			if (signal == "click" || signal == "drag" || signal == "drop") 
			{
				if(_clickHandler == null){
					/* System.Console.Write("Click handler registered\n"); */
					_clickHandler = window.onMouseDown (
						delegate (object obj, Event evt) {
							if (hit (evt.x, evt.y)) {
								window.startDrag(this);
								handleClickCallbacks(this, evt);
							}
							return true;
						});
				}
			}

			if (signal == "release") 
			{
				if(_releaseHandler == null){
					/* System.Console.Write("Release handler registered\n"); */
					_releaseHandler = window.onMouseUp (
						delegate (object obj, Event evt) {
							if (hit (evt.x, evt.y)) {
								handleReleaseCallbacks(this, evt);
							}
							return true;
						});
				}
			}

			if (signal == "click") { 
				_clickCallbacks.Add(function);
			} else if (signal == "release") {
				_releaseCallbacks.Add(function);
			} else if (signal == "move") {
				_moveCallbacks.Add(function);
			} else if (signal == "drop") {
				_dropCallbacks.Add(function);
			} else if (signal == "drag") {
				_dragCallbacks.Add(function);
			} else if(signal == "enter") {
				_enterCallbacks.Add(function);
			} else if(signal == "leave") {
				_leaveCallbacks.Add(function);
			} else {
                throw new Exception ("invalid signal for this object");
			}
        }

		protected void disconnectNow(){
			_clickCallbacks.Clear();
			_releaseCallbacks.Clear();
			_moveCallbacks.Clear();
			_dropCallbacks.Clear();
			_dragCallbacks.Clear();
			_enterCallbacks.Clear();
			_leaveCallbacks.Clear();
			if(window != null){
				if(_clickHandler != null){
					window.removeMouseDown(_clickHandler);
				}
				if(_moveHandler != null){
					window.removeMouseMovement(_moveHandler);
				}
				if(_releaseHandler != null){
					window.removeMouseUp(_releaseHandler);
				}
			}
			_clickHandler = null;
			_moveHandler = null;
			_releaseHandler = null;
		}

		public void disconnect(){
			InvokeBlocking(delegate(){
					disconnectNow();
				});
		}

        public double bounce {
            get {
                if (body != null)
                    return body.Restitution;
                else
                    return _bounce;
            }
            set {
                if (body != null)
                    body.Restitution = (float)value;
                else
                    _bounce = (float)value;
            }
        }

        public double friction {
            get {
                if (body != null)
                    return body.Friction;
                else
                    return _friction;
            }
            set {
                if (body != null)
                    body.Friction = (float)value;
                else
                    _friction = (float)value;
            }
        }

        public double density {
            get {
                return _density;
            }
            set {
                _density = (float)value;
            }
        }

        public double mass {
            get {
                if (body != null)
                    return body.Mass;
                else
                    throw new Exception ("need to draw shape first");
            }
            set {
                if (body != null)
                    body.Mass = (float)value;
                else
                    throw new Exception ("need to draw shape first");
            }
        }

        public string bodyType { // shape
            get {
                if (_bodyType == FarseerPhysics.Dynamics.BodyType.Dynamic) {
                    return "dynamic";
                } else if (_bodyType == FarseerPhysics.Dynamics.BodyType.Static) {
                    return "static";
                } else {
                    return "unkown";
                }
            }
            set {
                if (value == "dynamic") {
                    _bodyType = FarseerPhysics.Dynamics.BodyType.Dynamic;
                    if (body != null)
                        body.IsStatic = false;
                } else if (value == "static") {
                    _bodyType = FarseerPhysics.Dynamics.BodyType.Static;
                    if (body != null)
                        body.IsStatic = true;
                } else {
                    throw new Exception ("bodyType must be 'dynamic' or 'static'");
                }
            }
        }

        public virtual void addToPhysics ()
        { // Shape
        }

        public virtual void removeFromPhysics ()
        { // Shape
            //if (body != null) {
            //  body.UserData = null; // point back to this shape
            //  body.FixtureList[0].UserData = null; // point back to this shape
            //}
            if (body != null) {
                lock(world) {
                    body.DestroyFixture(body.FixtureList[0]);
                    body = null;
                }
            }
        }

        public virtual void updateFromPhysics ()
        {
            // get from body, put in sprite
            if (body != null) {
                lock(world) {
                    float MeterInPixels = 64.0f;
                    if (wrap) {
                        float x = (float)wrap_width ((float)(body.Position.X * MeterInPixels));
                        float y = (float)wrap_height ((float)(body.Position.Y * MeterInPixels));
                        body.Position = new Vector2 (x / MeterInPixels, y / MeterInPixels);
                    }
                    Vector2 position = body.Position * MeterInPixels;
                    double rotation = body.Rotation * 180.0 / Math.PI; 
                    // Move it
                    _moveTo (position.X, position.Y);
                    _rotateTo (rotation);
                }
            }
        }

        public void updatePhysics ()
        {
            // get from sprite, put in body
            if (body != null) {
                lock(world) {
                    float MeterInPixels = 64.0f;
                    body.Position = new Vector2 (((float)x) / MeterInPixels, 
                            ((float)y) / MeterInPixels);
                    // FIXME: undo operation; call rotateTo()?
                    body.Rotation = (float)_rotation;
                    body.Awake = true;
                }
            }
        }

        public void stackOnTop ()
        {
            if (window != null) {
                window.stackOnTop (this);
            }
			if(drawn_on_shape != null){
				drawn_on_shape.stackOnBottom (this);
			}
        }

        public void stackOnBottom ()
        {
            if (window != null) {
                window.stackOnBottom (this);
            }
			if(drawn_on_shape != null){
				drawn_on_shape.stackOnBottom (this);
			}
        }

		public void stackOnTop (Shape shape)
        {
            // last drawn is on top
            if (shapes.Contains (shape)) {
                Invoke( delegate {
                    lock (shapes) {
                        shapes.Remove (shape);
                        shapes.Insert (shapes.Count, shape);
                    }
                });
                QueueDraw ();
            }
        }

        public void stackOnBottom (Shape shape)
        {
            // first drawn is on bottom
            if (shapes.Contains (shape)) {
                Invoke( delegate {
                    lock (shapes) {
                        shapes.Remove (shape);
                        shapes.Insert (0, shape);
                    }
                });
				QueueDraw ();
            }
        }

        public Point getP1 ()
        {
            return getScreenPoint (points [0]);
        }

        public virtual Point getP2 ()
        {
            return getScreenPoint (points [2]);
        }

        //JH: Helper function start for fixing the 'click' not corresponding to screen coordinates
        private void _setTransformation(Cairo.Context g){
            if(drawn_on_shape != null){
                drawn_on_shape._setTransformation(g);
            }
            Point temp = screen_coord (center);
            g.Translate (temp.x, temp.y);
            g.Rotate (_rotation);
            g.Scale (_scaleFactor, _scaleFactor);
        }

        public Point getTrueScreenPoint (IList iterable)
        {
            // p is relative to center, rotate, and scale; returns
            // screen coordinate of p
            double px = 0, py = 0;
            InvokeBlocking (delegate {
                using (Cairo.ImageSurface draw = new Cairo.ImageSurface (Cairo.Format.Argb32, 70, 150)){
                    using (Cairo.Context g = new Cairo.Context(draw)) {
                        _setTransformation(g);
                        px = System.Convert.ToDouble (iterable [0]);
                        py = System.Convert.ToDouble (iterable [1]);
                        g.UserToDevice (ref px, ref py);
                    }
                }
            });
            //System.Console.Write("Screen point: " + px.ToString() + ", " + py.ToString() + "\n");
            return new Point (px, py);
        }
        //JH:Helper functions end

        public Point getScreenPoint (IList iterable)
        {
            // p is relative to center, rotate, and scale; returns
            // screen coordinate of p
            double px = 0, py = 0;
            InvokeBlocking (delegate {
                using (Cairo.ImageSurface draw = new Cairo.ImageSurface (Cairo.Format.Argb32, 70, 150)){
                    using (Cairo.Context g = new Cairo.Context(draw)) {
                        //using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
                        Point temp = screen_coord (center);
                        g.Translate (temp.x, temp.y);
                        g.Rotate (_rotation);
                        g.Scale (_scaleFactor, _scaleFactor);
                        px = System.Convert.ToDouble (iterable [0]);
                        py = System.Convert.ToDouble (iterable [1]);
                        g.UserToDevice (ref px, ref py);
                    }
                }
            });
            return new Point (px, py);
       }

            public Point getCenter ()
            {
                return center;
            }

            public bool has_pen {
                get {
                    return _has_pen;
                }
                set {
                    _has_pen = value;
                }
            }

            public double rotation {
                get {
                    double temp = (_rotation * 180.0 / Math.PI);
                    if (temp == 0)
                        return 0.0;
                    else
                        return -temp;
                }
                set {
                    rotateTo (value);
                }
            }

            public double scaleFactor {
                get {
                    return _scaleFactor;
                }
                set {
                    scaleTo (value);
                }
            }

            public double x {
                get {
                    return center.x;
                }
                set {
                    moveTo (value, center.y);
                }
            }

            public double getX ()
            {
                return center.x;
            }

            public double getY ()
            {
                return center.y;
            }

            public void setX (double value)
            {
                moveTo (value, center.y);
            }

            public void setY (double value)
            {
                moveTo (center.x, value);
            }

            public double y {
                get {
                    return center.y;
                }
                set {
                    moveTo (center.x, value);
                }
            }

            public void setBorderWidth (int value)
            {
                _border = value;
                QueueDraw ();
            }

            public int border {
                get {
                    return _border;
                }
                set {
                    _border = value;
                    QueueDraw ();
                }
            }

            public Pen pen {
                get {
                    return _pen;
                }
                set {
                    if (has_pen) {
                        _pen = value;
                        _pen.center.x = center.x;
                        _pen.center.y = center.y;
                        QueueDraw ();
                    } else
                        throw new Exception ("this shape cannot have a pen");
                }
            }

            public void QueueDraw ()
            { // shape
                if (window is WindowClass) {
                    if (window.getMode () == "auto" ||
                            window.getMode () == "bitmap" || 
                            window.getMode () == "physics")
                        window.update ();
                    // else, manually call step()
                } else if (dockableWindow is DockableWindowClass) {
                    if (dockableWindow.getMode () == "auto" ||
                            dockableWindow.getMode () == "bitmap" || 
                            dockableWindow.getMode () == "physics")
                        dockableWindow.update ();
                    // else, manually call step()
                }
            }

            public virtual bool hit (double x, double y)
            {
                Point p = new Point (x, y);
                int counter = 0;
                double xinters;
                Point p1, p2;
                if (points != null) {
                    //JH: Fixing hit to actually work on screen coordinates
                    //JH: Old code
                    //p1 = points [0];
                    //JH: New code start
                    p1 = getTrueScreenPoint(points [0]);
                    //JH: New code end
                    for (int i=1; i<=points.Length; i++) {
                        //JH: Fixing hit to actually work on screen coordinates
                        //JH: Old code
                        //p2 = points [i % points.Length];
                        //if (p.y > (Math.Min (p1.y, p2.y) + center.y)) {
                        //    if (p.y <= (Math.Max (p1.y, p2.y) + center.y)) {
                        //        if (p.x <= (Math.Max (p1.x, p2.x) + center.x)) {
                        //            if (p1.y != p2.y) {
                        //                xinters = (p.y - p1.y) * (p2.x - p1.x) / (p2.y - p1.y) + p1.x + center.x;
                        //                if (p1.x == p2.x || p.x <= xinters)
                        //                    counter++;
                        //            }
                        //        }
                        //    }
                        //}
                        //JH: New code start
                        p2 = getTrueScreenPoint(points [i % points.Length]);
                        if (p.y > (Math.Min (p1.y, p2.y))) {
                            if (p.y <= (Math.Max (p1.y, p2.y))) {
                                if (p.x <= (Math.Max (p1.x, p2.x))) {
                                    if (p1.y != p2.y) {
                                        xinters = (p.y - p1.y) * (p2.x - p1.x) / (p2.y - p1.y) + p1.x;
                                        if (p1.x == p2.x || p.x <= xinters)
                                            counter++;
                                    }
                                }
                            }
                        }
                        //JH: New code end
                        p1 = p2;
                    }
                    return (counter % 2 != 0); // hit?
                } else {
                    return false;
                }
            }

            public void set_points (params Point [] ps)
            {
                // 1. set center to absolute
                double sumx = 0.0;
                double sumy = 0.0;
                if (ps.Length > 0) {
                    for (int i = 0; i < ps.Length; i++) {
                        sumx += ps [i].x;
                        sumy += ps [i].y;
                    }
                    center.x = sumx / ps.Length;
                    center.y = sumy / ps.Length;
                } else {
                    center.x = 0;
                    center.y = 0;
                }
                // 2. compute this.points in relative terms to center
                points = new Point [ps.Length];
                for (int i = 0; i < ps.Length; i++) {
                    points [i] = new Point (ps [i].x - center.x, 
                            ps [i].y - center.y);
                }
            }

            public void append (IList iterable)
            {
                // add a new point to list of points
                // first copy old points
                // FIXME: could make points a list
                Point [] new_points = new Point [points.Length + 1];
                for (int i = 0; i < points.Length; i++) {
                    new_points [i] = points [i];
                }
                // Now add new point:
                new_points [points.Length] = new Point (iterable);
                points = new_points;
            }

            public void set_points ()
            {
                // 1. set center to absolute
                double sumx = 0.0;
                double sumy = 0.0;
                if (points.Length > 0) {
                    for (int i = 0; i < points.Length; i++) {
                        sumx += points [i].x;
                        sumy += points [i].y;
                    }
                    center.x = sumx / points.Length;
                    center.y = sumy / points.Length;
                } else {
                    center.x = 0;
                    center.y = 0;
                }
                // 2. compute this.points in relative terms to center
                for (int i = 0; i < points.Length; i++) {
                    points [i].x = points [i].x - center.x;
                    points [i].y = points [i].y - center.y;
                }
            }

            public void forward (double distance)
            {
                center.x += distance * Math.Cos (_rotation);
                center.y += distance * Math.Sin (_rotation);
                updatePen ();
                QueueDraw ();
            }

            public void updatePen ()
            {
                if (has_pen && pen.down) {
                    pen.appendPath (new Point (center.x, center.y));
                }
            }

            public void  updateGlobalPosition (Cairo.Context g)
            {
                gx = center.x; // shape's global x
                gy = center.y; // shape's global y
                g.UserToDevice (ref gx, ref gy);
                if (points != null) { // bounding box
                    foreach (Point p in points) {
                        p.gx = p.x;
                        p.gy = p.y;
                        g.UserToDevice (ref p.gx, ref p.gy);
                    }
                }
            }

            public Line penUp ()
            {
                return penUp(false, null);
            }

            public Line penUp (string fillColor)
            {
                return penUp(false, fillColor);
            }
            //testing

            public Line penUp (bool getLine, string fillColor)
            {
                if (pen.down) {
                    if (getLine) {
                        pen._down = false;
                        Line line = pen.resetPath ();
                        line.color = pen.color;
                        if (fillColor == null)
                            line.fill = null;
                        else
                            line.fill =new Color(fillColor);
                        return line;
                    } else {
                        if (window != null) {
                            pen._down = false;
                            Line line = pen.resetPath ();
                            line.color = pen.color;
                            if (fillColor == null)
                                line.fill = null;
                            else
                                line.fill = new Color(fillColor);
                            line.draw(window);
                        }
                        return null;
                    }
                }
                return null;
            }

            public void penDown ()
            {
                pen._down = true;
                pen.appendPath (new Point (center.x, center.y));
            }

            public void backward (double distance)
            {
                forward (-distance);
            }

            public virtual void render (Cairo.Context g)
            { // Shape
                if (!visible)
                    return;
                g.Save ();
                Point temp;
                if (points != null) {
                    g.LineWidth = border;
                    temp = screen_coord (center);
                    g.Translate (temp.x, temp.y);
                    g.Rotate (_rotation);
                    g.Scale (_scaleFactor, _scaleFactor);
                    temp = screen_coord (points [0]);
                    g.MoveTo (temp.x, temp.y);
                    for (int p = 1; p < points.Length; p++) {
                        temp = screen_coord (points [p]);
                        g.LineTo (temp.x, temp.y);
                    }
                    if (close_path)
                        g.ClosePath ();
					if (clip){
						g.ClipPreserve();
					}
                    if (gradient != null) {
                        Cairo.Gradient pat;
                        if (gradient.gtype == "linear")
                            pat = new Cairo.LinearGradient (gradient.p1.x,
                                    gradient.p1.y, 
                                    gradient.p2.x, 
                                    gradient.p2.y);
                        else
                            pat = new Cairo.RadialGradient (gradient.p1.x,
                                    gradient.p1.y, 
                                    gradient.radius1,
                                    gradient.p2.x, 
                                    gradient.p2.y,
                                    gradient.radius2);

                        pat.AddColorStop (0, gradient.c1.getCairo ());
                        pat.AddColorStop (1, gradient.c2.getCairo ());
                        g.Pattern = pat;
                        g.FillPreserve ();
                    } else if (_fill != null) {
                        g.Color = _fill.getCairo ();
                        g.FillPreserve ();
                    }
                    if (_outline != null) {
                        g.Color = _outline.getCairo ();
                        g.Stroke ();
                    }
                }
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Restore ();
                g.Save();
                // This time, don't rotate
                temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                foreach (Shape shape in joints) {
                    // draw a line from center here to center there
                    g.MoveTo (0, 0); //center of this object
                    temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                shape.center.y - this.center.y));
                    g.LineTo (temp.x, temp.y);
                    g.Stroke ();
                }
                g.Restore ();
                if (has_pen)
                    pen.render (g);
            }

            public Point screen_coord (IList iterable)
            {
                // FIXME: return in coords of screen
                return new Point (iterable); 
                // new Point(point.x - center.x, point.y - center.y);
            }

            internal double wrap_width (double x)
            {
                if (x < 0)
                    return wrap_width (window._cacheWidth + x);
                else if (x >= window._cacheWidth)
                    return wrap_width (x - window._cacheWidth);
                else
                    return x;
            }

            internal double wrap_height (double y)
            {
                if (y < 0)
                    return wrap_height (window._cacheHeight + y);
                else if (y >= window._cacheHeight)
                    return wrap_height (y - window._cacheHeight);
                else
                    return y;
            }

            public virtual void move (double dx, double dy)
            {
                center.x += dx;
                center.y += dy;
                updatePen ();
                if (wrap) {
                    center.x = wrap_width (center.x);
                    center.y = wrap_height (center.y);
                }
                if (body != null)
                    updatePhysics ();
                QueueDraw ();
            }

            public virtual void moveShape (double dx, double dy)
            {
                // move points relative to center point
                // this will appear when shape rotates about center
                for (int i = 0; i < points.Length; i++) {
                    points [i].x += dx;
                    points [i].y += dy;
                }
                if (body != null)
                    updatePhysics ();
                QueueDraw ();
            }

            public virtual void moveShapeTo (double dx, double dy)
            {
                // move points relative to center point
                // this will appear when shape rotates about center
                for (int i = 0; i < points.Length; i++) {
                    points [i].x -= (dx + center.x);
                    points [i].y -= (dy + center.y);
                }
                if (body != null)
                    updatePhysics ();
                QueueDraw ();
            }

            public virtual void connect(Shape s2) {
                // Connect two shapes together
                // The shape's center is now shared
                // with the second shape
                double dx = center.x;
                double dy = center.y;
                s2.center = center;
                s2.moveShape(dx, dy);
            }

            public virtual void disconnect(Shape s2) {
                // Disconnect two shapes who share a center
                double dx = center.x;
                double dy = center.y;
                s2.center = new Point(0, 0);
                s2.set_points();
                s2.moveTo(dx, dy);
            }

            public virtual void recenter ()
            {
                double dx = x;
                double dy = y;
                set_points();
                center.x = dx;
                center.y = dy;
                if (body != null)
                    updatePhysics ();
                QueueDraw ();
            }

            public virtual void moveTo (double x, double y)
            {
                double dx = x - center.x;
                double dy = y - center.y;
                move (dx, dy);
            }

            public virtual void moveTo(double x, double y, double seconds) {
                if (seconds > 0) {
                    double updateTime = 0.025;
                    double intervals = seconds/updateTime;
                    double dx = (x - this.x)/intervals;
                    double dy = (y - this.y)/intervals;
                    for(int i = 0; i < intervals; i++){
                        move(dx, dy);
                        if (window != null) {
                            window.update();
                        }
                        waitSeconds(updateTime);
                    }
                }
                //in case we are not at an integer coordinate
                moveTo(x, y);
            }

            public virtual void move(double x, double y, double seconds) {
                double ox = this.x, oy = this.y;
                if (seconds > 0) {
                    double updateTime = 0.025;
                    double intervals = seconds/updateTime;
                    double dx = x/intervals;
                    double dy = y/intervals;
                    for(int i = 0; i < intervals; i++){
                        move(dx, dy);
                        if (window != null) {
                            window.update();
                        }
                        waitSeconds(updateTime);
                    }
                }
                //in case we are not at an integer coordinate
                moveTo(ox + x, oy + y);
            }

            public void _moveTo (double x, double y)
            {
                double dx = x - center.x;
                double dy = y - center.y;
                _move (dx, dy);
            }

            public void _move (double dx, double dy)
            {
                center.x += dx;
                center.y += dy;
                updatePen ();
            }

            public virtual void rotate (double degrees)
            {
                _rotation -= (Math.PI / 180.0) * degrees;
                if (body != null)
                    updatePhysics ();
                QueueDraw ();
            }

            public virtual void rotateTo (double degrees)
            {
                _rotation = degrees * (Math.PI) / 180.0;
                if (body != null)
                    updatePhysics ();
                QueueDraw ();
            }

            public void _rotate (double degrees)
            {
                _rotation -= (Math.PI / 180.0) * degrees;
            }

            public void _rotateTo (double degrees)
            {
                _rotation = degrees * (Math.PI) / 180.0;
            }

            public void scale (double percent)
            {
                _scaleFactor *= percent;
                QueueDraw ();
            }

            public void scaleTo (double percent)
            {
                _scaleFactor = percent;
                if (body != null)
                    updatePhysics ();
                QueueDraw ();
            }

            public void update ()
            { // Shape
                // Alias to QueueDraw
                QueueDraw (); 
            }

			public void setWindow(WindowClass win){
				window = win;
				lock (shapes) {
					foreach (Shape shape in shapes) {
						shape.setWindow(win);
					}
				}
			}

            public GraphicsRepresentation drawAt (WindowClass win, IList iterable) {
                moveTo(System.Convert.ToDouble(iterable[0]), 
                        System.Convert.ToDouble(iterable[1]));
                draw(win);
                return new GraphicsRepresentation(win);
            }

	    //JH: Part of the great experiment
            public GraphicsRepresentation draw (DockableWindowClass win)
            { // Shape
				System.Console.Write("Drawing object\n");
                InvokeBlocking( delegate {
                    // Add this shape to the Canvas list.
                    if (win.IsRealized) {
                        if (win.mode == "bitmap" || win.mode == "bitmapmanual") {
                            win.canvas.need_to_draw_surface = true;
                            using (Cairo.Context g = new Cairo.Context(win.canvas.surface)) {
                                render (g);
                            }
                        } else {
                            lock (win.getCanvas().shapes) {
                                if (! win.getCanvas ().shapes.Contains (this)) {
                                    win.getCanvas ().shapes.Add (this);
                                    //System.Console.Error.WriteLine("Added to win!");
                                }
                            }
                            // Make sure each subshape is associated with this window
                            // so QueueDraw will redraw:
                            lock (shapes) {
                                foreach (Shape shape in shapes) {
                                    shape.dockableWindow = win;
                                }
                            }
                        }
                        dockableWindow = win;
                        if (win.getCanvas().world != null) {
                            addToPhysics ();
                        }
                        QueueDraw ();
                    }
                });
                return new GraphicsRepresentation(win);
            }
	    //JH: part of the great experiment

            public GraphicsRepresentation draw (WindowClass win)
            { // Shape
                InvokeBlocking( delegate {
                    // Add this shape to the Canvas list.
                    if (win.IsRealized) {
                        if (win.mode == "bitmap" || win.mode == "bitmapmanual") {
                            win.canvas.need_to_draw_surface = true;
                            using (Cairo.Context g = new Cairo.Context(win.canvas.surface)) {
                                render (g);
                            }
                        } else {
                            lock (win.getCanvas().shapes) {
                                if (! win.getCanvas ().shapes.Contains (this)) {
                                    win.getCanvas ().shapes.Add (this);
                                    //System.Console.Error.WriteLine("Added to win!");
                                }
                            }
                            // Make sure each subshape is associated with this window
                            // so QueueDraw will redraw:
							
                        }
                        setWindow(win);
                        if (window._canvas.world != null) {
                            addToPhysics ();
                        }
                        QueueDraw ();
                    }
                });
                return new GraphicsRepresentation(win);
            }			

            public GraphicsRepresentation drawAt (Canvas canvas, IList iterable)
            {
                moveTo(System.Convert.ToDouble(iterable[0]), 
                        System.Convert.ToDouble(iterable[1]));
                draw(canvas);
                return new GraphicsRepresentation(canvas);
            }

            public GraphicsRepresentation draw (Canvas canvas)
            { // Shape
                InvokeBlocking( delegate {
                    // Add this shape to the Canvas list.
                    if (canvas.mode == "bitmap" || canvas.mode == "bitmapmanual") {
                        canvas.need_to_draw_surface = true;
                        using (Cairo.Context g = new Cairo.Context(canvas.surface)) {
                            render (g);
                        }
                    } else {
                        lock (canvas.shapes) {
                            if (! canvas.shapes.Contains (this)) 
                    canvas.shapes.Add (this);
                        }
                    }
                    if (canvas.world != null) {
                        addToPhysics ();
                    }
                    QueueDraw ();
                });
                return new GraphicsRepresentation(canvas);
            }

            /* public GraphicsRepresentation draw (Shape shape, IList iterable) */
            /* { */
            /*     double x = System.Convert.ToDouble(iterable[0]); */
            /*     double y = System.Convert.ToDouble(iterable[1]); */
            /*     moveTo(x, y); */
            /*     draw(shape); */
            /*     return new GraphicsRepresentation(this); */
            /* } */

			public GraphicsRepresentation draw (Shape shape)
			{
				return draw(shape, -1);
			}

            public GraphicsRepresentation draw (Shape shape, int index)
            { // Shape
                InvokeBlocking( delegate {
                    // Add this shape to the shape's list.
                    lock (shape.shapes) {
                        if (! shape.shapes.Contains (this)) {
							if(index != -1){
								shape.shapes.Insert(index, this);
							} else {
								shape.shapes.Add (this);
							}
                            //System.Console.Error.WriteLine("Added to shape!");
                            //if (window.canvas.world != null) {
                            //_bodyType = FarseerPhysics.Dynamics.BodyType.Static; // must be static, to stay
                            // where it is
                            //addToPhysics();
                            //}
                        }
                    }
					if(shape.window != null){
						setWindow(shape.window);
					}
                    drawn_on_shape = shape;
                    QueueDraw ();
                });
                return new GraphicsRepresentation(shape);
            }


            public void undraw ()
            {
                InvokeBlocking( delegate {
				disconnectNow();
				if (drawn_on_shape != null && drawn_on_shape.shapes != null) {
					lock (drawn_on_shape.shapes) {
						if (drawn_on_shape.shapes.Contains (this)) {
							drawn_on_shape.shapes.Remove (this);
							drawn_on_shape.QueueDraw();
							//System.Console.Error.WriteLine("Removed from shape!");
						}
					}
					drawn_on_shape = null;
				}
				if (window != null) {
					if (window._canvas != null && window._canvas.shapes != null) {
						lock (window.getCanvas().shapes) {
							if (window._canvas.world != null) {
								removeFromPhysics ();
							}
							if (window.getCanvas ().shapes.Contains (this)) {
								window.getCanvas ().shapes.Remove (this);
								//System.Console.Error.WriteLine("Removed from win!");
								window.QueueDraw();
								window = null;
							}
						}
					}
				}
                });
            }

            public Gradient gradient {
                set {
                    _gradient = value;
                    QueueDraw ();
                }
                get {
                    return _gradient;
                } 
            }

            public Gradient getGradient(){
                return gradient;
            }

            public void setGradient(Gradient g){
                gradient = g;
            }

            public Color color {
                set {
                    if (value != null) {
                        _fill = ((Color)value).Copy ();
                        _outline = _fill;
                    } else {
                        _fill = null;
                        _outline = null;
                    }
                    QueueDraw ();
                }
                get {
                    if (_fill != null) {
                        _fill.window = this.window;
                        return _fill; // share!
                    } else
                        return null;
                }
            }       

            public void setColor (Color value)
            {
                color = value;
            }

            public Color getColor ()
            {
                return color;
            }

            public void setFill (Color value)
            {
                if (value == null) {
                    _fill = null;
                } else {
                    _fill = ((Color)value).Copy ();
                }
                QueueDraw ();
            }

            public Color getFill ()
            {
                return fill;
            }

            public virtual Color fill {
                set {
                    if (value == null) {
                        _fill = null;
                    } else {
                        _fill = ((Color)value).Copy ();
                    }
                    QueueDraw ();
                }
                get {
                    if (_fill != null) {
                        _fill.window = this.window;
                        return _fill;
                    } else
                        return null;
                }
            }

            public void setOutline (Color value)
            {
                if (value == null) {
                    _outline = null;
                } else {
                    _outline = ((Color)value).Copy ();
                }
                QueueDraw ();
            }

            public Color getOutline ()
            {
                return outline;
            }

            public Color outline {
                set {
                    if (value == null) {
                        _outline = null;
                    } else {
                        _outline = ((Color)value).Copy ();
                    }
                    QueueDraw ();
                }
                get {
                    if (_outline != null) {
                        _outline.window = this.window;
                        return _outline; // share!
                    } else
                        return null;
                }
            }

            public void speak(string text) {
                if (speechBubble != null) {
                    speechBubble.undraw();
                }
                if (text != "" && this.window != null) {
                    speechBubble = new SpeechBubble(text);
                    speechBubble.window = this.window;
                    // these are expensive:
                    double [] text_widths = speechBubble.getExtents(speechBubble.words);
                    double [] space_width = speechBubble.getExtents(new string [1] {"k"});
                    //double text_height = speechBubble.height;

                    // figure out bounding box for text
                    // find the best ratio of width/height (9/6 maybe)
                    double diff_ratio = 1000, best_x = 0, best_y = 0;
                    for (int width = 100; width < 500; width += 100) {
                        for (int height = 100; height < 500; height += 100) {
                            double xx = space_width[0];
                            double yy = 3.0+speechBubble.fontSize;
                            double ws = space_width[0];             // Word spacing
                            double ls = speechBubble.fontSize + 2.0;        // Line spacing
                            int i = 0;                  // Word counter
                            double max_row = 0, max_col = 0, col = 0;
                            for (;;) {
                                // save the max row and max col:
                                col = xx + text_widths[i] + ws;
                                max_col = Math.Max(col, max_col);
                                max_row = yy + ls/2;

                                // Move to next x position and next word
                                xx = xx + text_widths[i] + ws;
                                i++;
                                if (i < speechBubble.words.Length) {
                                    // If the next word would be rendered outside the clip region, 
                                    // move to next line. Otherwise, continue.
                                    if ( xx + text_widths[i] > width ) {
                                        xx = space_width[0];
                                        yy = yy + ls;
                                    }
                                } else {
                                    break;
                                }
                            }
                            if (Math.Abs(max_col/max_row - 9.0/6.0) < diff_ratio) {
                                diff_ratio = Math.Abs(max_col/max_row - 9.0/6.0);
                                best_x = max_col;
                                best_y = max_row;
                            }
                            //Console.WriteLine("x: {0}, y: {1} ({2}x{3})", best_x, best_y, width, height);
                        }
                    }

                    Point anchor = points[0];
                    Point top_left = new Point(anchor.x - best_x, 
                            anchor.y - best_y - 50);
                    Point bottom_right = new Point(anchor.x, anchor.y - 50);
                    speechBubble.setPositions(top_left, bottom_right, anchor);
                    speechBubble.draw(this);
                    QueueDraw();
                }
            }

            public void clear() {
                lock (shapes) {
                    shapes.Clear ();
                }
            }

	    public virtual double getWidth() {
                double minx = 0.0;
                double maxx = 0.0;
		if(points == null) return 0;
		if(points.Length > 0){
			minx = points[0].x;
			maxx = points[0].x;
		}
		for (int i = 1; i < points.Length; i++) {
			if(minx > points[i].x){
				minx = points[i].x;
			}
			if(maxx < points[i].x){
				maxx = points[i].x;
			}
		}
		return maxx - minx;
	    }

	    public virtual double getHeight() {
                double miny = 0.0;
                double maxy = 0.0;
		if(points == null) return 0;
		if(points.Length > 0){
			miny = points[0].y;
			maxy = points[0].y;
		}
		for (int i = 1; i < points.Length; i++) {
			if(miny > points[i].y){
				miny = points[i].y;
			}
			if(maxy < points[i].y){
				maxy = points[i].y;
			}
		}
		return maxy - miny;
	    }
        }

        public class Text : Shape
        {
            public string _text;
            public string _fontFace = "sans serif";
            public Cairo.FontWeight fontWeight = Cairo.FontWeight.Normal;
            public Cairo.FontSlant fontSlant = Cairo.FontSlant.Normal;
            double _fontSize = 18;
            public string _xJustification = "center"; // left, center, right
            public string _yJustification = "center"; // top, center, bottom

            // FIXME: add wrappers around weight, slant
            public Text (IList iterable, string text)
            {
                this.text = text;
                set_points (new Point (iterable));
            }
			
			// JH: copy - start
			public Text(){}
			
			public void copyAttributes(Text other){
				base.copyAttributes(other);
			    _text = other._text;
				_fontFace = other._fontFace;
				fontWeight = other.fontWeight;
				fontSlant = other.fontSlant;
				_fontSize = other.fontSize;
				_xJustification = other.xJustification;
				_yJustification = other.yJustification;
			}
			
			public override Shape copy(){
				Text shapeCopy = new Text();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public string xJustification {
                get {
                    return _xJustification;
                }
                set {
                    if (value == "left" ||
                            value == "right" ||
                            value == "center") {
                        _xJustification = value;
                        QueueDraw ();
                    } else {
                        throw new Exception("xJustification should be 'left', 'right', or 'center'");
                    }
                }
            }

            public string yJustification {
                get {
                    return _yJustification;
                }
                set {
                    if (value == "top" ||
                            value == "bottom" ||
                            value == "center") {
                        _yJustification = value;
                        QueueDraw ();
                    } else {
                        throw new Exception("yJustification should be 'top', 'bottom', or 'center'");
                    }
                }
            }


            public double fontSize {
                get {
                    return _fontSize;
                }
                set {
                    _fontSize = value;
                    QueueDraw ();
                }
            }

            public void setFontSize(double fontSize)
            {
                _fontSize = fontSize;
            }


            public double getFontSize()
            {
                return _fontSize;
            }


            public string fontFace {
                get {
                    return _fontFace;
                }
                set {
                    _fontFace = value;
                    QueueDraw ();
                }
            }

            public string getFontFace()
            {
                return _fontFace;
            }

            public void setFontFace(string fontFace)
            {
                _fontFace = fontFace;
            }


            public string getXJustification()
            {
                return xJustification;
            }

            public void setXJustification(string just)
            {
                xJustification = just;
            }

            public string getYJustification()
            {
                return yJustification;
            }

            public void setYJustification(string just)
            {
                yJustification = just;
            }

            public string getText()
            {
                return _text;
            }

            public void setText(string text)
            {
                _text = text;
                QueueDraw ();
            }

            public string text {
                get {
                    return _text;
                }
                set {
                    _text = value;
                    QueueDraw ();
                }
            }

            public override void render (Cairo.Context g)
            {
                // MEMORY LEAK!
                if (!visible)
                    return;
                g.Save ();
                Point temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                g.Rotate (_rotation);
                g.Scale (_scaleFactor, _scaleFactor);
                if (gradient != null) {
                    Cairo.Gradient pat;
                    if (gradient.gtype == "linear")
                        pat = new Cairo.LinearGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.p2.x, 
                                gradient.p2.y);
                    else
                        pat = new Cairo.RadialGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.radius1,
                                gradient.p2.x, 
                                gradient.p2.y,
                                gradient.radius2);

                    pat.AddColorStop (0, gradient.c1.getCairo ());
                    pat.AddColorStop (1, gradient.c2.getCairo ());
                    g.Pattern = pat;
                    g.FillPreserve ();
                } else if (_fill != null)
                    g.Color = _fill._cairo;
                else
                    g.Color = new Cairo.Color (0, 0, 0); // default color when none given

                //g.SelectFontFace(fontFace, FontSlant.Normal, FontWeight.Normal);
                g.SelectFontFace(fontFace, fontSlant, fontWeight);
                g.SetFontSize(fontSize);
                Point p = new Point (0, 0);
                double line_offset = 0;
                // Get total height:
                double total_height = 0.0;
                double max_width = 0.0;
                foreach (string line in text.Split('\n')) {
					//JH: Experiment which might make the text look better
					if (total_height != 0.0)
						total_height += fontSize*0.25; //Line skip
					total_height += fontSize;
					
					//JH: Pre-experiment code commented out below
                    /* TextExtents te = g.TextExtents(line); */
                    /* if (total_height != 0.0) */
                    /*     total_height += te.Height * .5; */
                    /* total_height += te.Height; */
                    /* max_width = Math.Max(te.Width, max_width); */
                }
                // Draw the text:
                foreach (string line in text.Split('\n')) {
                    TextExtents te = g.TextExtents(line);
                    if (xJustification == "center") {
                        p.x = points [0].x - te.Width / 2 - te.XBearing;
                    } else if (xJustification == "left") {
                        p.x = points [0].x;
                    } else if (xJustification == "right") {
                        p.x = points [0].x - te.Width;
                    }
                    if (yJustification == "center") {
                        p.y = points [0].y - total_height / 2 - te.YBearing;
                    } else if (yJustification == "bottom") {
						//JH: Experiment which might make the text look better
						p.y = points [0].y - total_height + fontSize;
						//JH: Pre-experiment code commented out below
						/* p.y = points [0].y - total_height + te.Height; */		      
                    } else if (yJustification == "top") {
						//JH: Experiment which might make the text look better
						p.y = points [0].y + fontSize;
						//JH: Pre-experiment code commented out below
						//p.y = points [0].y + te.Height;
	
                    }
                    temp = screen_coord (p);
                    g.MoveTo (temp.x, temp.y - line_offset);
                    g.ShowText(line);
					//JH: Experiment which might make the text look better
					line_offset += fontSize * -1.25;
					//JH: Pre-experiment code commented out below
                    /* line_offset += te.YBearing * 1.5; */
                }
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Stroke ();
                g.Restore ();
                g.Save();
                // This time, don't rotate
                temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                foreach (Shape shape in joints) {
                    // draw a line from center here to center there
                    g.MoveTo (0, 0); //center of this object
                    temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                shape.center.y - this.center.y));
                    g.LineTo (temp.x, temp.y);
                    g.Stroke ();
                }
                g.Restore ();
                if (has_pen)
                    pen.render (g);
            }

            public double width {
                get {
                    double retval = 0.0;
                    InvokeBlocking( delegate {
                        if (window != null && window.IsRealized) {
                            using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
                                g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
                                g.SetFontSize(this.fontSize);
                                foreach (string line in text.Split('\n')) {
                                    TextExtents te = g.TextExtents(line);
                                    retval = Math.Max(te.Width, retval);
                                }
                            }
                        }
                    });
                    return retval;
                }
            }

            public double height {
                get {
                    double retval = 0.0;
                    InvokeBlocking( delegate {
                        if (window != null && window.IsRealized) {
                            using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
                                g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
                                g.SetFontSize(this.fontSize);
                                foreach (string line in text.Split('\n')) {
                                    TextExtents te = g.TextExtents(line);
                                    if (retval != 0.0)
                        retval += te.Height * .5;
                    retval += te.Height;
                                }
                            }
                        }
                    });
                    return retval;
                }
            }

	    public override double getWidth() {
		    return width;
	    }

	    public override double getHeight() {
		    return height;
	    }

            public double [] getExtents(string [] text) {
                double [] retval = new double [text.Length];
                InvokeBlocking( delegate {
                    if (window != null && window.IsRealized) {
                        using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
                            g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
                            g.SetFontSize(this.fontSize);
                            int pos = 0;
                            foreach (string word in text) {
                                Cairo.TextExtents te = g.TextExtents (word);
                                retval [pos++]= te.Width;
                            }
                        }
                    }
                });
                return retval;
            }


            public override void addToPhysics ()
            { // Text
                Invoke( delegate {
                    world = window._canvas.world;
                    lock(world) {
                        double width = 0;
                        double height = 0;
                        if (window.IsRealized) {
                            using (Cairo.Context g = Gdk.CairoHelper.Create(window.canvas.GdkWindow)) {
                                g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
                                g.SetFontSize(this.fontSize);
                                Cairo.TextExtents te = g.TextExtents (text);
                                // FIXME: need to adjust based on justification
                                // This works with x centered, y centered
                                width = te.Width;
                                height = te.Height;
                            }
                        }
                        float MeterInPixels = 64.0f;
                        // from x,y to meters of window
                        // arbitrary:
                        Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                            ((float)y) / MeterInPixels);
                        body = FarseerPhysics.Factories.BodyFactory.CreateRectangle (
                                world,
                                (float)(width / MeterInPixels), // radius in meters
                                (float)(height / MeterInPixels), // radius in meters
                                _density, // density
                                position);                        // center
                        // Give it some bounce and friction
                        body.Restitution = _bounce;
                        body.Rotation = (float)_rotation;
                        body.Friction = _friction;
                        body.BodyType = _bodyType;
                        body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
                        body.UserData = this; // point back to this shape
                        body.FixtureList [0].UserData = this; // point back to this shape
                    }
                });
            }
        }
        
		public class TextBox: Rectangle
		{
			public Text textObject;
    
			public TextBox (IList iterable1, IList iterable2, string text) : base(iterable1, iterable2)
			{
      
				this.textObject=new Text(new int[] { 0,0},text);
				//this.textObject.setXJustification("center");
				//this.textObject.setYJustification("top");
				this.textObject.fill=new Color("black");
				this.textObject.draw(this);
			}
			public void setText(string text)
			{
				this.textObject.setText(text);
			}
		}

        public class SpeechBubble : Text {
            public Point anchor;
            public Point point2;
            public string [] words;

            public SpeechBubble (string text) : base(new int [] {0,0}, text) {
                this.words = text.Split(' ');
            }

            public SpeechBubble (IList iterable, IList point2, string text, IList anchor) : base(iterable, text) {
                this.words = text.Split(' ');
                this.point2 = new Point(point2);
                this.anchor = new Point(anchor);
                this.border = 2;
            }

            public void setPositions(IList iterable, IList point2, IList anchor) {
                set_points (new Point (iterable));
                this.point2 = new Point(point2);
                this.anchor = new Point(anchor);
                this.border = 2;
            }

            public override void render (Cairo.Context g) {

                //Added by RV at suggestion of JH.
                //All other overrides contain this line
                if (!visible)
                    return;
                int _textYOffset = (int)this.fontSize;  
                double x = center.x;
                double y = center.y;
                double w = point2.x - center.x;
                double h = point2.y - center.y;
                double r = 6.0;
                double hpi = 0.5*Math.PI;

                g.Save();
                // SetPath:
                g.MoveTo( x, y+r );
                g.Arc(    x+r, y+r, r, Math.PI, -hpi );
                // Top:
                g.LineTo( x+w-r, y );
                g.Arc(    x+w-r, y+r, r, -hpi, 0.0 );
                // Right:
                g.LineTo( x+w, y+h-r );
                g.Arc(    x+w-r, y+h-r, r, 0.0, hpi);
                // Bottom:
                g.LineTo( x+r +20, y+h );
                g.LineTo( anchor.x, anchor.y );
                g.LineTo( x+r +10, y+h );
                g.LineTo( x+r, y+h );
                g.Arc(    x+r, y+h-r, r, hpi, Math.PI );
                // Left
                g.LineTo( x, y+r );
                g.ClosePath();
                // end
                g.Color = new Cairo.Color(255, 255, 255);
                g.FillPreserve();
                g.Color = new Cairo.Color(0, 0, 0);
                g.LineWidth = border;
                g.Stroke();

                // draw text:
                if (this.text.Length > 0 && words.Length > 0) {
                    g.Save ();
                    g.Rectangle(x, y, w-1.0, h-1.0);
                    g.Clip ();
                    if (_fill != null)
                        g.Color = _fill._cairo;
                    else
                        g.Color = new Cairo.Color (0, 0, 0); // default color when none given
                    g.SelectFontFace(this.fontFace, this.fontSlant, this.fontWeight);
                    g.SetFontSize(this.fontSize);

                    // Do word wrap. Slightly insane.

                    // @@@ Note that in the following there are several hard-coded 
                    // size tweaks that probably ought to be a function of the font size, 
                    // not fixed values. Some day this should be fixed.
                    TextExtents space = g.TextExtents("k"); // space extents

                    double xx = x+ (space.Width);
                    double yy = y+3.0+_textYOffset;
                    double ws = space.Width;                        // Word spacing
                    double ls = this.fontSize + 2.0;        // Line spacing
                    int i = 0;                              // Word counter
                    string word = words[i];                 // First word
                    TextExtents te = g.TextExtents(word);   // Word extents

                    for (;;) {
                        // Render one word at next position
                        g.MoveTo(xx, yy);
                        g.ShowText(word);

                        // Move to next x position and next word
                        xx = xx + te.Width + ws;
                        i++;
                        if (i < words.Length) {
                            word = words[i];

                            // Get extents for next word
                            te = g.TextExtents(word);

                            // If the next word would be rendered outside the clip region, 
                            // move to next line. Otherwise, continue.
                            if ( xx + te.Width > x + w ) {
                                xx = x + (space.Width);
                                yy = yy + ls;
                            }
                        } else {
                            break;
                        }
                    }
                    g.Restore();
                }
                g.Restore();
            }
        }

        public class Line : Shape
        {
            public Line (params object [] points) : base(true)
            {
                Point [] temp = new Point [points.Length];
                int count = 0;
                foreach (object o in points) {
                    if (o is Point)
                        temp [count] = (Point)o;
                    else if (o is IList) {
                        IList i = (IList)o;
                        temp [count] = new Point (i [0], i [1]);
                    } else {
                        throw new Exception ("Line: can't convert arg to a point");
                    }
                    count++;
                }
                set_points (temp);
                close_path = false;
                fill = null;
            }

            public Line (IList iterable1, IList iterable2) :
                this(true, iterable1, iterable2)
            {
                close_path = false;
                fill = null;
            }

            public Line (bool has_pen, IList iterable1, IList iterable2)
            {
                set_points (new Point (iterable1), 
                        new Point (iterable2));
                close_path = false;
                fill = null;
            }

            public Line () : base(true)
            {
                points = new Point[0];
                close_path = false;
                fill = null;
            }

			// JH: copy - start
			public override Shape copy(){
				Line shapeCopy = new Line();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public override Point getP2 ()
            {
                return getScreenPoint (points [1]);
            }

        }

        public class Curve : Shape
        {
            public Curve (IList iterable0, 
                    IList iterable1, 
                    IList iterable2, 
                    IList iterable3):
                this(true, iterable0, iterable1, iterable2, iterable3)
            {
            }

            public Curve (bool has_pen, 
                    IList iterable0, 
                    IList iterable1, 
                    IList iterable2, 
                    IList iterable3) : 
                base(has_pen)
            {
                set_points (new Point (iterable0), new Point (iterable1), 
                        new Point (iterable2), new Point (iterable3));
                fill = null;
            }

			// JH: copy - start
			public Curve(){}
			
			public override Shape copy(){
				Curve shapeCopy = new Curve();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public override void render (Cairo.Context g)
            {
                if (!visible)
                    return;
                g.Save ();
                Point temp, p1, p2, p3;
                if (points != null) {
                    g.LineWidth = border;
                    temp = screen_coord (center);
                    g.Translate (temp.x, temp.y);
                    g.Rotate (_rotation);
                    g.Scale (_scaleFactor, _scaleFactor);
                    temp = screen_coord (points [0]);
                    g.MoveTo (temp.x, temp.y);
					if (clip){
						g.ClipPreserve();
					}
                    for (int p = 1; p < points.Length; p += 3) {
                        p1 = screen_coord (points [p]);
                        p2 = screen_coord (points [p + 1]);
                        p3 = screen_coord (points [p + 2]);
                        g.CurveTo (p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
                    }
                    if (gradient != null) {
                        Cairo.Gradient pat;
                        if (gradient.gtype == "linear")
                            pat = new Cairo.LinearGradient (gradient.p1.x,
                                    gradient.p1.y, 
                                    gradient.p2.x, 
                                    gradient.p2.y);
                        else
                            pat = new Cairo.RadialGradient (gradient.p1.x,
                                    gradient.p1.y, 
                                    gradient.radius1,
                                    gradient.p2.x, 
                                    gradient.p2.y,
                                    gradient.radius2);

                        pat.AddColorStop (0, gradient.c1.getCairo ());
                        pat.AddColorStop (1, gradient.c2.getCairo ());
                        g.Pattern = pat;
                        g.FillPreserve ();
                    } else if (_fill != null) {
                        g.Color = _fill._cairo;
                        g.FillPreserve ();
                    }
                    if (_outline != null) {
                        g.Color = _outline._cairo;
                        g.Stroke ();
                    }
                }
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Restore ();
                g.Save();
                // This time, don't rotate
                temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                foreach (Shape shape in joints) {
                    // draw a line from center here to center there
                    g.MoveTo (0, 0); //center of this object
                    temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                shape.center.y - this.center.y));
                    g.LineTo (temp.x, temp.y);
                    g.Stroke ();
                }
                g.Restore ();
                if (has_pen)
                    pen.render (g);
            }
        }

        public class Arrow : Shape
        {
            public Arrow (IList iterable) :  this(iterable, 0)
            {
            }

            public Arrow (IList iterable, double degrees) :  base(true)
            {
                set_points (new Point (0, 0),
                        new Point (0, -5), 
                        new Point (11, 0),
                        new Point (0, 5) 
                        );
                center.x = System.Convert.ToDouble (iterable [0]);
                center.y = System.Convert.ToDouble (iterable [1]);
                rotate (degrees);
            }

			// JH: copy - start
			public Arrow(){}
			
			public override Shape copy(){
				Arrow shapeCopy = new Arrow();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end
        }

        public class Turtle : Shape
        {
            public Turtle (IList iterable) :  this(iterable, 0)
            {
            }

            public Turtle (IList iterable, double degrees) :  base(true)
            {
                set_points (new Point (0, 0),
                        new Point (-5, -5), 
                        new Point (5, 0),
                        new Point (-5, 5) 
                        );
                center.x = System.Convert.ToDouble (iterable [0]);
                center.y = System.Convert.ToDouble (iterable [1]);
                rotate (degrees);
            }

			// JH: copy - start
			public Turtle(){}
			
			public override Shape copy(){
				Turtle shapeCopy = new Turtle();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end
        }

        public class Pen : Shape
        {
            private List<Point> _path; // = new List<Point>();
            public bool _down;
            public double minDistance = 1;

            public Pen (Color color, int border) : base(false)
            {
                _down = false;
                _path = new List<Point> ();
                this.color = color;
                this.border = border;
            }

            public Pen (int border) : base(false)
            {
                _down = false;
                _path = new List<Point> ();
                this.color = new Color (0, 0, 0);
                this.border = border;
            }

			// JH: copy - start
			public Pen(){}

			public void copyAttributes(Pen other){
				base.copyAttributes(other);
				foreach(Point point in other._path){
					_path.Add(new Point(point));
				}
				_down = other._down;
				minDistance = other.minDistance;
			}
			
			public override Shape copy(){
				Pen shapeCopy = new Pen();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public List<Point> getPath ()
            {
                return _path;
            }

            public Line resetPath ()
            {
                Line temp = new Line (_path.ToArray ());
                _path = new List<Point> ();
                return temp;
            }

            public void appendPath (IList iterable)
            {
                Point temp = new Point (iterable);
                if (_path.Count > 0) {
                    if (_path [_path.Count - 1].distance (temp) > minDistance)
                        _path.Add (temp);
                } else {
                    _path.Add (temp);
                }
            }

            public bool down {
                get {
                    return _down;
                }
            }

            public List<Point> path {
                get {
                    return _path;
                }
            }

            public override void render (Cairo.Context g)
            {
                if (!visible)
                    return;
                // render path
                g.Save ();
                Point temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                g.Rotate (_rotation);
                g.Scale (_scaleFactor, _scaleFactor);
                if (path != null && path.Count > 0) {
                    g.LineWidth = border;
                    temp = screen_coord (path [0]);
                    g.MoveTo (temp.x, temp.y);
                    for (int p = 1; p < path.Count; p++) {
                        temp = screen_coord (path [p]);
                        g.LineTo (temp.x, temp.y);
                    }
					if (clip){
						g.ClipPreserve();
					}
                    if (_outline != null) {
                        g.Color = _outline._cairo;
                        g.Stroke ();
                    }
                }
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Restore ();
            }

	    public override double getWidth() {
                double minx = 0.0;
                double maxx = 0.0;
		if(_path == null) return 0;
		if(_path.Count > 0){
			minx = _path[0].x;
			maxx = _path[0].x;
		}
		for (int i = 1; i < _path.Count; i++) {
			if(minx > _path[i].x){
				minx = _path[i].x;
			}
			if(maxx < _path[i].x){
				maxx = _path[i].x;
			}
		}
		return maxx - minx;
	    }

	    public override double getHeight() {
                double miny = 0.0;
                double maxy = 0.0;
		if(_path == null) return 0;
		if(_path.Count > 0){
			miny = _path[0].y;
			maxy = _path[0].y;
		}
		for (int i = 1; i < _path.Count; i++) {
			if(miny > _path[i].y){
				miny = _path[i].y;
			}
			if(maxy < _path[i].y){
				maxy = _path[i].y;
			}
		}
		return maxy - miny;
	    }
	    
        }

        public class Pixel: IComparable
        {
            private Picture picture;
            public int x;
            public int y;

            public Pixel (Picture picture, int x, int y)
            {
                // FIXME: wrap_width uses raw picture height, width
                // but if we put it in gui-thread, it will be too slow
                this.picture = picture;
                this.x = picture.wrap_width (x);
                this.y = picture.wrap_height (y);
            }

            public Color getColor ()
            {
                return picture.getColor (x, y);
            }

            public PythonTuple getRGB ()
            {
                return picture.getRGB (x, y);
            }

            public PythonTuple getRGBA ()
            {
                return picture.getRGBA (x, y);
            }

            public int getGray ()
            {
                return picture.getGray (x, y);
            }

            public int getRed ()
            {
                return picture.getRed (x, y);
            }

            public int getGreen ()
            {
                return picture.getGreen (x, y);
            }

            public int getBlue ()
            {
                return picture.getBlue (x, y);
            }

            public int getAlpha ()
            {
                return picture.getAlpha (x, y);
            }

            public void setColor (Color color)
            {
                picture.setColor (x, y, color);
            }

            public void setRGB (int red, int green, int blue)
            {
                picture.setRGB (x, y, red, green, blue);
            }

            public void setRGBA (int red, int green, int blue, int alpha)
            {
                picture.setRGBA (x, y, red, green, blue, alpha);
            }

            public void setGray (int value)
            {
                picture.setGray (x, y, value);
            }

            public void setRed (int value)
            {
                picture.setRed (x, y, value);
            }

            public void setGreen (int value)
            {
                picture.setGreen (x, y, value);
            }

            public void setBlue (int value)
            {
                picture.setBlue (x, y, value);
            }

            public void setAlpha (int value)
            {
                picture.setAlpha (x, y, value);
            }
            public int CompareTo(Object obj)
            {
                if (obj == null) return 1;

                Pixel otherPixel = obj as Pixel;
                if (getGray() == otherPixel.getGray()) return 0;
                else if (getGray() < otherPixel.getGray()) return -1;
                else return 1;
            }

        }

        public class Picture : Shape
        {
            Gdk.Pixbuf _pixbuf; // in memory rep of picture
            public string filename;
            public int _cacheWidth;
            public int _cacheHeight;

            public Gdk.Pixbuf pixbuf {
                get {
                    return _pixbuf;
                }
            }

			// JH: copy - start
			public void copyAttributes(Picture original){
				base.copyAttributes(original);
                this.filename = original.filename;
                InvokeBlocking (delegate {
						// Colorspace, has_alpha, bits_per_sample, width, height:
						_pixbuf = new Gdk.Pixbuf (original._pixbuf.Colorspace, true, 8, (int)original.getWidth (), (int)original.getHeight ());
						if (!_pixbuf.HasAlpha) {
							_pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
						}
						for (int x=0; x < _pixbuf.Width; x++) {
							for (int y=0; y < _pixbuf.Height; y++) {
                            byte r = (byte)original.getRed (x, y);
                            byte g = (byte)original.getGreen (x, y);
                            byte b = (byte)original.getBlue (x, y);
                            byte a = (byte)original.getAlpha (x, y);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
											   x * _pixbuf.NChannels + 0, r);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
											   x * _pixbuf.NChannels + 1, g);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, b);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
											   x * _pixbuf.NChannels + 3, a);
							}
						}
						set_points (original.points);
                    center = original.center;
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
					});
			}
			
			public override Shape copy(){
				return new Picture(this);
			}
			// JH: copy - end

            public Picture (string filename) : this(true)
            {
                this.filename = filename;
                InvokeBlocking (delegate {
                    if (filename.StartsWith ("http://") || filename.StartsWith ("https://")) {
                        HttpWebRequest req = (HttpWebRequest)WebRequest.Create (filename);
                        req.KeepAlive = false;
                        req.Timeout = 10000;        
                        WebResponse resp = req.GetResponse ();
                        Stream s = resp.GetResponseStream ();
                        _pixbuf = new Gdk.Pixbuf (s);
                    } else if (filename.StartsWith("data:")) {
                        // "data:image/png;base64,..."
                        string [] parts = filename.Split(new char[] {','}, 2, StringSplitOptions.None);
                        Byte [] bytes = System.Convert.FromBase64String(parts[1]);
                        _pixbuf = new Gdk.Pixbuf(bytes);
                    } else {
                        _pixbuf = new Gdk.Pixbuf (filename);
                    }
                    if (!_pixbuf.HasAlpha) {
                        _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                    }
                    set_points (new Point (0, 0), 
                        new Point (_pixbuf.Width, 0),
                        new Point (_pixbuf.Width, _pixbuf.Height), 
                        new Point (0, _pixbuf.Height));
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
                });
				_outline = null;
            }

	    //JH: This is part of the big experiment
            public Picture (DockableWindowClass dockableWindow) : this(true)
            {
	      Canvas canvas = dockableWindow.canvas;
                InvokeBlocking (delegate {                    
                    int w = canvas.width;
                    int h = canvas.height;
                    Gdk.Pixmap pixmap = new Gdk.Pixmap(null, w, h, 24);
                    using (Cairo.Context g = Gdk.CairoHelper.Create(pixmap))
                {        
                    // draw background                                                    
                    Rectangle background = new Rectangle(new Point(0,0), new Point(w, h));
                    if (canvas.background_color != null) {
                        background.color = canvas.background_color;
                    }
                    else {
                        // default background of white?
                        background.color = new Color(255, 255, 255, 255);
                    }
                background.render(g);

                if (canvas.mode == "bitmap" || canvas.mode == "bitmapauto"){
                    Cairo.ImageSurface surface = canvas.finalsurface;
                    g.SetSourceSurface(surface, 0, 0);
                    g.Paint();                        
                }
                else{
                    foreach (Shape shape in canvas.shapes) {
                        shape.render (g);
                    }                                                
                }
                }

                Gdk.Colormap colormap = pixmap.Colormap;

                _pixbuf = Gdk.Pixbuf.FromDrawable(pixmap, colormap,
                        0, 0, 0, 0, w, h);

                if (!_pixbuf.HasAlpha) {
                    _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                }
                set_points (new Point (0, 0), 
                        new Point (_pixbuf.Width, 0),
                        new Point (_pixbuf.Width, _pixbuf.Height), 
                        new Point (0, _pixbuf.Height));         
                _cacheWidth = _pixbuf.Width;
                _cacheHeight = _pixbuf.Height;
                //if (on_mac()) swap_red_blue();
                });
		_outline = null;
            }
	    //JH: This is part of the big experiment

            public Picture (Gtk.Window gtk_window) : this(true)
            {
                InvokeBlocking (delegate {
                    Gdk.Pixbuf pixbuf = null;
                    Gdk.Drawable drawable = gtk_window.GdkWindow;
                    Gdk.Colormap colormap = drawable.Colormap;
                    int _width = 0;
                    int _height = 0;
                    drawable.GetSize (out _width, out _height);
                    pixbuf = Gdk.Pixbuf.FromDrawable (drawable, colormap, 0, 0, 0, 0, _width, _height);
                    // Now, do what Picture(pixbuf) does:
                    _pixbuf = pixbuf;
                    if (!_pixbuf.HasAlpha) {
                        //_pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                    }

                    set_points (new Point (0, 0), 
                        new Point (_pixbuf.Width, 0),
                        new Point (_pixbuf.Width, _pixbuf.Height), 
                        new Point (0, _pixbuf.Height));         
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
                    if (on_mac()) swap_red_blue();
                });
				_outline = null;
            }

            public Picture (Canvas canvas) : this(true)
            { 
                InvokeBlocking (delegate {                    
                    int w = canvas.width;
                    int h = canvas.height;
                    Gdk.Pixmap pixmap = new Gdk.Pixmap(null, w, h, 24);
                    using (Cairo.Context g = Gdk.CairoHelper.Create(pixmap))
                {        
                    // draw background                                                    
                    Rectangle background = new Rectangle(new Point(0,0), new Point(w, h));
                    if (canvas.background_color != null) {
                        background.color = canvas.background_color;
                    }
                    else {
                        // default background of white?
                        background.color = new Color(255, 255, 255, 255);
                    }
                background.render(g);

                if (canvas.mode == "bitmap" || canvas.mode == "bitmapauto"){
                    Cairo.ImageSurface surface = canvas.finalsurface;
                    g.SetSourceSurface(surface, 0, 0);
                    g.Paint();                        
                }
                else{
                    foreach (Shape shape in canvas.shapes) {
                        shape.render (g);
                    }                                                
                }
                }

                Gdk.Colormap colormap = pixmap.Colormap;

                _pixbuf = Gdk.Pixbuf.FromDrawable(pixmap, colormap,
                        0, 0, 0, 0, w, h);

                if (!_pixbuf.HasAlpha) {
                    _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                }
                set_points (new Point (0, 0), 
                        new Point (_pixbuf.Width, 0),
                        new Point (_pixbuf.Width, _pixbuf.Height), 
                        new Point (0, _pixbuf.Height));         
                _cacheWidth = _pixbuf.Width;
                _cacheHeight = _pixbuf.Height;
                //if (on_mac()) swap_red_blue();
                });
				_outline = null;
            }

            public Picture (WindowClass window) : this(window._canvas)
            {
		    _outline = null;
            }

            // THIS IS IS A SUPER HACK
            public bool on_mac()
            {
                return System.IO.Directory.Exists("/Applications");            
            }

            // THIS IS IS A SUPER HACK^2
            public void swap_red_blue()
            {
                for (int x=0; x < _pixbuf.Width; x++) {
                    for (int y=0; y < _pixbuf.Height; y++) {

                        byte r = Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 0);
                        byte b = Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2);
                        Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 0, b);
                        Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, r);
                    }
                }
            }

            public Picture (System.Drawing.Bitmap bitmap) : this(bitmap, bitmap.Width, bitmap.Height)
            {
                _outline = null;
            }

            public Picture (bool has_pen) : base(has_pen)
            {
                this._fill.picture = this;
                _outline = null;
            }


            public Picture (Picture original, double scale) : this((int)(original.width * scale), (int)(original.height * scale)) {
                Picture temp = new Picture(original);
                for (int w=0; w < width; w++) {
                    for (int h=0; h < height; h++) {
                        int x = (int)(((double)w)/width * temp.width);
                        int y = (int)(((double)h)/height * temp.height);
                        setColor(w, h, temp.getColor(x,y));
                    }
                }
            }

            public Picture (Picture original) : this(true)
            {
                this.filename = original.filename;
                InvokeBlocking (delegate {
						// Colorspace, has_alpha, bits_per_sample, width, height:
						_pixbuf = new Gdk.Pixbuf (original._pixbuf.Colorspace, true, 8, (int)original.getWidth (), (int)original.getHeight ());
						if (!_pixbuf.HasAlpha) {
							_pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
						}
						for (int x=0; x < _pixbuf.Width; x++) {
							for (int y=0; y < _pixbuf.Height; y++) {
                            byte r = (byte)original.getRed (x, y);
                            byte g = (byte)original.getGreen (x, y);
                            byte b = (byte)original.getBlue (x, y);
                            byte a = (byte)original.getAlpha (x, y);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
											   x * _pixbuf.NChannels + 0, r);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
											   x * _pixbuf.NChannels + 1, g);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, b);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
											   x * _pixbuf.NChannels + 3, a);
							}
						}
						set_points (original.points);
                    center = original.center;
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
					});
            }

            public Picture (Gdk.Pixbuf pixbuf) : this(true)
            {
                InvokeBlocking (delegate {
                    _pixbuf = pixbuf;
                    if (!_pixbuf.HasAlpha) {
                        _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                    }
                    set_points (new Point (0, 0), 
								new Point (_pixbuf.Width, 0),
								new Point (_pixbuf.Width, _pixbuf.Height), 
								new Point (0, _pixbuf.Height));
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
					});
				_outline = null;
            }


            public Picture (System.Drawing.Bitmap bitmap, bool fluke1=false) : this(bitmap, bitmap.Width, bitmap.Height, fluke1) {
                _outline = null;
            }

            public Picture (System.Drawing.Bitmap bitmap, int width, int height, bool fluke1=false) : this(true) {
                InvokeBlocking (delegate {
                    // Colorspace, has_alpha, bits_per_sample, width, height:
                    // FIXME: convert bitmap.palette to colormap
                    _pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
                    if (!_pixbuf.HasAlpha) {
                        _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                    }
                    int xstep = 1;
                    if (fluke1) xstep = 2;
                    for (int x=0; x < _pixbuf.Width; x += xstep) {
                        for (int y=0; y < _pixbuf.Height; y++) {
                            System.Drawing.Color pixel = bitmap.GetPixel (x / xstep, y);
                            // First pixel
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 0, pixel.R);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 1, pixel.G);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, pixel.B);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 3, pixel.A);
                            if (fluke1){
                                // Second pixel
                                int x2 = x + 1;
                                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                        x2 * _pixbuf.NChannels + 0, pixel.R);
                                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                        x2 * _pixbuf.NChannels + 1, pixel.G);
                                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                        x2 * _pixbuf.NChannels + 2, pixel.B);
                                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                        x2 * _pixbuf.NChannels + 3, pixel.A);
                            }
                        }
                    }
                    set_points (new Point (0, 0), 
                            new Point (_pixbuf.Width, 0),
                            new Point (_pixbuf.Width, _pixbuf.Height), 
                            new Point (0, _pixbuf.Height));
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
                });
                _outline = null;
            }
	    

            public System.Drawing.Bitmap toBitmap (params object [] args) {
                return _pixbuf.ToBitmap(args);
            }

            public Gdk.Pixbuf toPixbuf () {
                return _pixbuf.Copy();
            }

            public void saveToSVG(string filename)
            {
                var svg = new Cairo.SvgSurface(filename, width, height);
                using (Cairo.Context g = new Cairo.Context(svg)) {
                    render(g);
                }
                svg.Finish();
            }

            public Calico.Representation toSVG() {
                string fileName = System.IO.Path.GetTempPath() + Guid.NewGuid().ToString() + ".svg";
                saveToSVG(fileName);
                System.IO.TextReader reader = new System.IO.StreamReader(fileName);
                string text = reader.ReadToEnd();
                reader.Close();
                return Calico.Representation.SVG(text);
            }

            public void fromArray (Byte [] buffer, string format)
            {
                InvokeBlocking( delegate {
                    if (format == "BGRX") { // b, r, g, ignore
                        int count = 0;
                        for (int i=0; i < buffer.Length; i+=4) {
                            byte b = buffer [i];
                            byte g = buffer [i + 1];
                            byte r = buffer [i + 2];
                            // NOTE: x is from the other side
                            int x = _pixbuf.Width - count % _pixbuf.Width;
                            int y = count / _pixbuf.Width;
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 0, r);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 1, g);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, b);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 3, 255);
                            count++;
                        }
                    } else if (format == "GRAY") { 
                        int count = 0;
                        for (int i=0; i < buffer.Length; i++) {
                            byte g = buffer [i];
                            // NOTE: x is from the other side
                            int x = _pixbuf.Width - count % _pixbuf.Width;
                            int y = count / _pixbuf.Width;
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 0, g);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 1, g);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 2, g);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 3, 255);
                            count++;
                        }
                    } else {
                        throw new Exception ("Picture.fromArray(array, format): invalid format");
                    }
                });
                QueueDraw ();
            }

            public Picture (int width, int height, byte [] buffer, int depth) : this(true)
            {
                InvokeBlocking (delegate {
                    // depth should be 1
                    // Colorspace, has_alpha, bits_per_sample, width, height:
                    _pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
                    if (!_pixbuf.HasAlpha) {
                        _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                    }
                    for (int x=0; x < _pixbuf.Width; x++) {
                        for (int y=0; y < _pixbuf.Height; y++) {
                            byte r = buffer [(y * width + x)];
                            byte g = r;
                            byte b = r;
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 0, r);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 1, g);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, b);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 3, 255);
                        }
                    }
                    set_points (new Point (0, 0), 
                            new Point (_pixbuf.Width, 0),
                            new Point (_pixbuf.Width, _pixbuf.Height), 
                            new Point (0, _pixbuf.Height));
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
                });
                _outline = null;
            }

            public Picture (int width, int height, byte [] buffer) : this(true)
            {
                InvokeBlocking (delegate {
                    // Colorspace, has_alpha, bits_per_sample, width, height:
                    _pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
                    if (!_pixbuf.HasAlpha) {
                        _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                    }
                    for (int x=0; x < _pixbuf.Width; x++) {
                        for (int y=0; y < _pixbuf.Height; y++) {
                            byte r = buffer [(y * width + x) * 3 + 0];
                            byte g = buffer [(y * width + x) * 3 + 1];
                            byte b = buffer [(y * width + x) * 3 + 2];
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 0, r);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 1, g);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, b);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 3, 255);
                        }
                    }
                    set_points (new Point (0, 0), 
                            new Point (_pixbuf.Width, 0),
                            new Point (_pixbuf.Width, _pixbuf.Height), 
                            new Point (0, _pixbuf.Height));
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
                });
                _outline = null;
            }

            public Picture (int width, int height) : this(true)
            {
                InvokeBlocking (delegate {
                    // Colorspace, has_alpha, bits_per_sample, width, height:
                    _pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
                    if (!_pixbuf.HasAlpha) {
                        _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                    }
                    // WORKAROUND: image needs alpha set to zero (full opacity/no
                    // transparency). Might as well set default color, too:
                    for (int x=0; x < _pixbuf.Width; x++) {
                        for (int y=0; y < _pixbuf.Height; y++) {
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 0, 255);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 1, 255);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, 255);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 3, 255);
                        }
                    }
                    set_points (new Point (0, 0), 
                        new Point (_pixbuf.Width, 0),
                        new Point (_pixbuf.Width, _pixbuf.Height), 
                        new Point (0, _pixbuf.Height));
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
                });
                _outline = null;
            }

            public Picture (int width, int height, Color color) : this(true)
            {
                InvokeBlocking (delegate {
                    // Colorspace, has_alpha, bits_per_sample, width, height:
                    _pixbuf = new Gdk.Pixbuf (new Gdk.Colorspace (), true, 8, width, height);
                    if (!_pixbuf.HasAlpha) {
                        _pixbuf = _pixbuf.AddAlpha (false, 0, 0, 0); 
                    }
                    // WORKAROUND: image needs alpha set to zero (full opacity/no
                    // transparency). Might as well set default color, too:
                    for (int x=0; x < _pixbuf.Width; x++) {
                        for (int y=0; y < _pixbuf.Height; y++) {
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 0, (byte)color.red);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 1, (byte)color.green);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 2, (byte)color.blue);
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                x * _pixbuf.NChannels + 3, (byte)color.alpha);
                        }
                    }
                    set_points (new Point (0, 0), 
                        new Point (_pixbuf.Width, 0),
                        new Point (_pixbuf.Width, _pixbuf.Height), 
                        new Point (0, _pixbuf.Height));
                    _cacheWidth = _pixbuf.Width;
                    _cacheHeight = _pixbuf.Height;
                });
                _outline = null;
            }

            public Picture getRegion (IList iterable, int width, int height, double degrees)
            {
                Picture pic = null;
                InvokeBlocking( delegate {
                    pic = new Picture (width, height);
                    Point p = new Point (iterable);
                    double angle = degrees * Math.PI / 180.0;
                    double px, py;
                    int ox = 0;
                    for (int x = -width/2; x < width/2; x++) {
                        int oy = 0;
                        for (int y = -height/2; y < height/2; y++) {
                            // rotate that x,y:
                            px = x * Math.Cos (angle) - y * Math.Sin (angle);
                            py = x * Math.Sin (angle) + y * Math.Cos (angle);
                            // set the color of the new image from the offset of this:
                            pic.setColor (ox, oy, this.getColor ((int)(p.x + px), 
                                    (int)(p.y + py)));
                            oy += 1;
                        }
                        ox += 1;
                    }
                });
                return pic;
            }

            public Picture getRegion (IList iterable, int width, int height)
            {
                Picture pic = null;
                InvokeBlocking( delegate {
                    pic = new Picture (width, height);
                    Point p = new Point (iterable);
                    for (int x = 0; x < width; x++) {
                        for (int y = 0; y < height; y++) {
                            pic.setColor (x, y, getColor((int)(p.x + x), 
                                    (int)(p.y + y)));
                        }
                    }
                });
                return pic;
            }

            public void setRegion (IList iterable, Picture picture)
            {
                InvokeBlocking( delegate {
                    // FIXME: better way would use Context to draw the image onto
                    // another image. Also, consider making this pic.draw(picture)
                    // This doesn't respect the color pallette of picture.
                    Point p = new Point (iterable);
                    for (int x = 0; x < picture._cacheWidth; x++) {
                        for (int y = 0; y < picture._cacheHeight; y++) {
                            Color c1 = this.getColor ((int)(p.x + x), 
                                (int)(p.y + y));
                            Color c2 = picture.getColor ((int)(x), 
                                (int)(y));
                            int t2 = c2.alpha;
                            int t1 = Math.Max (Math.Min (255 - t2, 255), 0);
                            this.setColor ((int)(p.x + x), 
                                (int)(p.y + y), 
                                new Color (t1 * c1.red + t2 * c2.red,
                                    t1 * c1.green + t2 * c2.green,
                                    t1 * c1.blue + t2 * c2.blue));
                        }
                    }
                });
            }

            public void fillRegion (IList iterable, Graphics.Color color)
            {
                InvokeBlocking( delegate {
                    Point p = new Point (iterable);
                    Graphics.Color target = getColor((int)p.x, (int)p.y);
                    Stack<Point> q = new Stack<Point>();
                    q.Push(p);
                    while (q.Count > 0) {
                        Point n = q.Pop();
                        if (getColor((int)n.x, (int)n.y).Equals(target)) {
			  setColor((int)n.x, (int)n.y, color);
			  if (n.x > 0)
			    q.Push(new Point(n.x - 1, n.y));
			  if (n.x < width)
			    q.Push(new Point(n.x + 1, n.y));
			  if (n.y > 0)
			    q.Push(new Point(n.x, n.y - 1));
			  if (n.x < height)
			    q.Push(new Point(n.x, n.y + 1));
                        }
                    }
                });
            }

            public void flipHorizontal ()
            {
                InvokeBlocking( delegate {
                    for (int x = 0; x < _cacheWidth/2; x++) {
                        for (int y = 0; y < _cacheHeight; y++) {
                            Color c1 = getColor (x, y);
                            Color c2 = getColor (_cacheWidth - x - 1, y);
                            setColor(x, y, c2);
                            setColor(_cacheWidth - x - 1, y, c1);
                        }
                    }
                });
            }

            public void flipVertical ()
            {
                InvokeBlocking( delegate {
                    for (int x = 0; x < _cacheWidth; x++) {
                        for (int y = 0; y < _cacheHeight/2; y++) {
                            Color c1 = getColor(x, y);
                            Color c2 = getColor(x, _cacheHeight - y - 1);
                            setColor(x, y, c2);
                            setColor(x, _cacheHeight - y - 1, c1);
                        }
                    }
                });
            }

            public void setRegion (IList iterable, int width, int height, double degrees,
                    Picture picture)
            {
                InvokeBlocking( delegate {
                    Point p = new Point (iterable);
                    double angle = degrees * Math.PI / 180.0;
                    double px, py;
                    int tx, ty;
                    for (int x = -width/2; x < width/2; x++) {
                        for (int y = -height/2; y < height/2; y++) {
                            // rotate that x,y:
                            px = x * Math.Cos (angle) - y * Math.Sin (angle);
                            py = x * Math.Sin (angle) + y * Math.Cos (angle);
                            // set the color of the new image from the offset of this:
                            tx = (int)(p.x + px);
                            ty = (int)(p.y + py);
                            this.setPixel (tx, ty, color);
                            // FIXME: a lame way to not skip any pixels:
                            // Need a region fill algorithm
                            if ((int)px + 1 < width / 2) {
                                this.setColor (tx + 1, ty, color);
                                if ((int)py + 1 < height / 2) {
                                    this.setColor (tx + 1, ty + 1, color);
                                    this.setColor (tx, ty + 1, color);
                                }
                            } else {
                                if ((int)py + 1 < height / 2) {
                                    this.setColor (tx, ty + 1, picture.getColor (x + width / 2, 
                                                y + height / 2));
                                }
                            }
                        }
                    }
                });
            }

            public void setRegion (IList iterable, int width, int height, double degrees,
                    Color color)
            {
                InvokeBlocking( delegate {
                    Point p = new Point (iterable);
                    double angle = degrees * Math.PI / 180.0;
                    double px, py;
                    int tx, ty;
                    for (int x = -width/2; x < width/2; x++) {
                        for (int y = -height/2; y < height/2; y++) {
                            // rotate that x,y:
                            px = x * Math.Cos (angle) - y * Math.Sin (angle);
                            py = x * Math.Sin (angle) + y * Math.Cos (angle);
                            // set the color of the new image from the offset of this:
                            tx = (int)(p.x + px);
                            ty = (int)(p.y + py);
                            this.setColor (tx, ty, color);
                            // FIXME: a lame way to not skip any pixels:
                            // Need a region fill algorithm
                            if ((int)px + 1 < width / 2) {
                                this.setColor (tx + 1, ty, color);
                                if ((int)py + 1 < height / 2) {
                                    this.setColor (tx + 1, ty + 1, color);
                                    this.setColor (tx, ty + 1, color);
                                }
                            } else {
                                if ((int)py + 1 < height / 2) {
                                    this.setColor (tx, ty + 1, color);
                                }
                            }
                        }
                    }
                });
            }

            public Gdk.Pixbuf getPixbuf ()
            {
                return _pixbuf;
            }

            public override double getWidth ()
            {
                int retval = 0;
                InvokeBlocking( delegate {
                    retval = _pixbuf.Width;
                });
                return retval;
            }

            public int _getWidth ()
            {
                return _pixbuf.Width;
            }

            public override double getHeight ()
            {
                int retval = 0;
                InvokeBlocking( delegate {
                    retval = _pixbuf.Height;
                });
                return retval;
            }

            public int _getHeight ()
            {
                return _pixbuf.Height;
            }

            public void setTransparent(Graphics.Color color)
            {
                InvokeBlocking( delegate {
                    for (int x = 0; x < _cacheWidth; x++) {
                        for (int y = 0; y < _cacheHeight; y++) {
                            Color c1 = getColor (x, y);
                            if (c1.Equals(color)) {
                                setAlpha(x, y, 0);
                            }
                        }
                    }
                });
            }

            public void savePicture () {
                if (filename != null) {
                    savePicture(filename, null);
                } else {
                    throw new Exception("Picture does not have an associated filename");
                }
            }

            public void savePicture (string filename, Graphics.Color color=null)
            {
                // png, and jpg
                String format;
                if (filename.Substring (filename.Length - 3, 3) == "png") {
                    format = "png";
                } else if (filename.Substring (filename.Length - 3, 3) == "jpg") {
                    format = "jpeg";
                } else if (filename.Substring (filename.Length - 3, 3) == "gif") {
                    AnimatedGifEncoder gif = new AnimatedGifEncoder();
                    if (color != null) {
                        gif.SetTransparent(System.Drawing.Color.FromArgb(color.alpha, color.red, color.green, color.blue));
                    }
                    gif.Start();
                    gif.AddFrame(this);
                    gif.Finish();
                    gif.Output(filename);
                    return;
                } else {
                    throw new Exception ("unknown image type; use gif, jpg or png");
                }
                _pixbuf.Save (filename, format);
            }

            public Pixel getPixel (int x, int y)
            {
                return new Pixel (this, x, y);
            }

            public void setPixel (int x, int y, Color color)
            {
                int red = color.red;
                int green = color.green;
                int blue = color.blue;
                int alpha = color.alpha;
                this.setRGBA (x, y, red, green, blue, alpha);
            }

            public void setPixel (int x, int y, Pixel pixel)
            {
                int red = pixel.getRed ();
                int green = pixel.getGreen ();
                int blue = pixel.getBlue ();
                int alpha = pixel.getAlpha ();
                this.setRGBA (x, y, red, green, blue, alpha);
            }

            public IEnumerable<Pixel> getPixels ()
            {
                for (int x=0; x < _cacheWidth; x++) {
                    for (int y=0; y < _cacheHeight; y++) {
                        yield return getPixel(x, y);
                    }
                }
            }

            public void setPixels (Picture picture)
            {
                for (int x=0; x < _cacheWidth; x++) {
                    for (int y=0; y < _cacheHeight; y++) {
                        setPixel (x, y, picture.getPixel (x, y));
                    }
                }
            }

            public Color getColor (int x, int y)
            {
                // red, green, blue, alpha
                Color temp = new Color (getRed (x, y), getGreen (x, y), getBlue (x, y), getAlpha (x, y));
                temp.picture = this;
                temp.x = x;
                temp.y = y;
                return temp;
            }

            public PythonTuple getRGB (int x, int y)
            {
                // red, green, blue, alpha
                return PyTuple (getRed (x, y), getGreen (x, y), getBlue (x, y));
            }

            public PythonTuple getRGBA (int x, int y)
            {
                // red, green, blue, alpha
                return PyTuple (getRed (x, y), getGreen (x, y), getBlue (x, y), getAlpha (x, y));
            }

            internal int wrap_width (int x)
            {
                if (x < 0)
                    return wrap_width ((int)(_cacheWidth + x));
                else if (x >= _cacheWidth)
                    return wrap_width ((int)(x - _cacheWidth));
                else
                    return x;
            }

            internal int wrap_height (int y)
            {
                if (y < 0)
                    return wrap_height ((int)(_cacheHeight + y));
                else if (y >= _cacheHeight)
                    return wrap_height ((int)(y - _cacheHeight));
                else
                    return y;
            }

            public int getGray (int x, int y)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                int r = Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 0);
                int g = Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 1);
                int b = Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 2);
                return (int)(((double)(r + g + b)) / 3.0);
            }

            public int getRed (int x, int y)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                return Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 0);
            }

            public int getGreen (int x, int y)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                return Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 1);
            }

            public int getBlue (int x, int y)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                return Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 2);
            }

            public int getAlpha (int x, int y)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                return Marshal.ReadByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 3);
            }

            public void setColor (int x, int y, Color color)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 0, (byte)color.red);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 1, (byte)color.green);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 2, (byte)color.blue);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 3, (byte)color.alpha);
                QueueDraw();
            }

            public void setGray (int x, int y, int value)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                byte bvalue = (byte)Math.Min(Math.Max((byte)0, value), (byte)255);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 0, bvalue);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 1, bvalue);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 2, bvalue);
                QueueDraw();
            }

            public void setRed (int x, int y, int value)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                byte bvalue = (byte)Math.Min(Math.Max((byte)0, value), (byte)255);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 0, bvalue);
                QueueDraw();
            }

            public void setGreen (int x, int y, int value)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                byte bvalue = (byte)Math.Min(Math.Max((byte)0, value), (byte)255);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 1, bvalue);
                QueueDraw();
            }

            public void setBlue (int x, int y, int value)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                byte bvalue = (byte)Math.Min(Math.Max((byte)0, value), (byte)255);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 2, bvalue);
                QueueDraw();
            }

            public void setAlpha (int x, int y, int value)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                byte bvalue = (byte)Math.Min(Math.Max((byte)0, value), (byte)255);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 3, bvalue);
                QueueDraw();
            }

            public void setAlpha (int value)
            {
                byte bvalue = (byte)Math.Min(Math.Max((byte)0, value), (byte)255);
                for (int x = 0; x < _cacheWidth; x++) {
                    for (int y = 0; y < _cacheHeight; y++) {
                        if (getRed(x,y) == 0 && getGreen(x,y) == 0 && getBlue(x,y) == 0) {
                            // Don't change alpha here
                        } else {
                            Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                                    x * _pixbuf.NChannels + 3, bvalue);
                        }
                    }
                }
                QueueDraw();
            }

            public void setRGB (int x, int y, int red, int green, int blue)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                byte bred = (byte)Math.Min(Math.Max((byte)0, red), (byte)255);
                byte bgreen = (byte)Math.Min(Math.Max((byte)0, green), (byte)255);
                byte bblue = (byte)Math.Min(Math.Max((byte)0, blue), (byte)255);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 0, bred);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 1, bgreen);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 2, bblue);
                QueueDraw();
            }

            public void setRGBA (int x, int y, int red, int green, int blue, 
                    int alpha)
            {
                // red, green, blue, alpha
                x = wrap_width (x);
                y = wrap_height (y);
                byte bred = (byte)Math.Min(Math.Max((byte)0, red), (byte)255);
                byte bgreen = (byte)Math.Min(Math.Max((byte)0, green), (byte)255);
                byte bblue = (byte)Math.Min(Math.Max((byte)0, blue), (byte)255);
                byte balpha = (byte)Math.Min(Math.Max((byte)0, alpha), (byte)255);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 0, bred);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 1, bgreen);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 2, bblue);
                Marshal.WriteByte (_pixbuf.Pixels, y * _pixbuf.Rowstride +
                        x * _pixbuf.NChannels + 3, balpha);
                QueueDraw();
            }

            public override Color fill {
                // operate on picture
                get {
                    return _fill;
                }
                set {
                    value.picture = this;
                }
            }

            public override void render (Cairo.Context g)
            { // picture
                if (!visible)
                    return;
                g.Save ();
                Point temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                g.Rotate (_rotation);
                g.Scale (_scaleFactor, _scaleFactor);
                Gdk.CairoHelper.SetSourcePixbuf (g, _pixbuf, -_pixbuf.Width / 2, -_pixbuf.Height / 2);
                g.Paint ();
				g.MoveTo (-_pixbuf.Width / 2, -_pixbuf.Height / 2);
				g.LineTo (_pixbuf.Width / 2, -_pixbuf.Height / 2);
				g.LineTo (_pixbuf.Width / 2, _pixbuf.Height / 2);
				g.LineTo (-_pixbuf.Width / 2, _pixbuf.Height / 2);
				g.ClosePath ();
				
				if (clip){
					g.ClipPreserve();
				}
                if (_outline != null) {
					g.LineWidth = border;
					g.Color = _outline._cairo;
					g.Stroke ();
                }
				// The method name is a bit deceptive, but this is the correct
				// way of clearing the old path.
				g.NewPath();
                
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Restore ();
                g.Save();
                // This time, don't rotate
                temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                foreach (Shape shape in joints) {
                    // draw a line from center here to center there
                    g.MoveTo (0, 0); //center of this object
                    temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                shape.center.y - this.center.y));
                    g.LineTo (temp.x, temp.y);
                    g.Stroke ();
                }
                g.Restore ();
                if (has_pen)
                    pen.render (g);
            }

            public int width {
                get {
                    return _cacheWidth;
                }
            }

            public int height {
                get {
                    return _cacheHeight;
                }
            }

            public Pixel [] get_pixels ()
            {
                return new Pixel[10];
            }

            public override string ToString ()
            {
                return String.Format ("<Picture (width={0}, height={1})>", _cacheWidth, _cacheHeight);
            }

            public string __repr__ ()
            {
                return ToString();
            }

            public IDictionary<string, string> GetRepresentations() {
                byte [] buffer;
                string png_string;
                string jpg_string;

                try {
                    buffer = _pixbuf.SaveToBuffer("png");
                    png_string = System.Convert.ToBase64String(buffer, 0, buffer.Length);
                } catch {
                    png_string = "";
                }
                try {
                    buffer = _pixbuf.SaveToBuffer("jpeg");
                    jpg_string = System.Convert.ToBase64String(buffer, 0, buffer.Length);
                } catch {
                    jpg_string = "";
                }

                var retval = new Dictionary<string, string>();
                retval["text/plain"] =  this.ToString();
                if (png_string != "")
                    retval["image/png"] = png_string;
                if (jpg_string != "")
                    retval["image/jpeg"] = jpg_string;
                return retval;
            }

            public void setAlphaColor(int red, int green, int blue) {
                _pixbuf = _pixbuf.AddAlpha (true, Convert.ToByte(red), Convert.ToByte(green), Convert.ToByte(blue)); 
            }

        } // -- end of Picture class


        private static double currentTime ()
        {
            System.TimeSpan t = System.DateTime.UtcNow - new System.DateTime (1970, 1, 1);
            return t.TotalSeconds;
        }


        // Gtk-based Objects
        // -----------------------------------------------------------------------------------
        // FIXME: check on if these are Thread-Safe
        public class Button : Gtk.Button
        {
            public double _x, _y;
            public WindowClass window;

            public Button (IList iterable, string label) : base(label)
            {
                _x = System.Convert.ToDouble (iterable [0]);
                _y = System.Convert.ToDouble (iterable [1]);
            }

            public double x {
                get {
                    return _x;
                }
                set {
                    moveTo (value, _y);
                }
            }

            public double y {
                get {
                    return _y;
                }
                set {
                    moveTo (_x, value);
                }
            }

            public void moveTo (object x, object y)
            {
                _x = System.Convert.ToDouble (x);
                _y = System.Convert.ToDouble (y);
                InvokeBlocking (delegate {
                    window.canvas.Move(this, (int)_x, (int)_y);
                    window.QueueDraw ();
                });
            }

            public GraphicsRepresentation draw (WindowClass win)
            { // button
                InvokeBlocking (delegate {
                    window = win;
                    if (gui_thread_id != -1)
                    Show ();
                window.getCanvas ().Put (this, (int)_x, (int)_y);
                window.QueueDraw ();
                });
                return new GraphicsRepresentation(win);
            }

            public void connect (string signal, Func<object,Event,object> function)
            {
                Clicked += delegate(object obj, System.EventArgs args) {
                    Event evt = new Event ("click", Graphics.currentTime ());
                    try {
                        Invoke (delegate {
                            function (obj, evt);
                        });
                    } catch (Exception e) {
                        Console.Error.WriteLine ("Error in connected function");
                        Console.Error.WriteLine (e.Message);
                    }        
                };
            }
        }

        public class Entry : Gtk.Entry
        {
            public double _x, _y;
            public WindowClass window;

            public Entry (IList iterable, int size) : base()
            {
                _x = System.Convert.ToDouble (iterable [0]);
                _y = System.Convert.ToDouble (iterable [1]);
                WidthChars = size;
                Text = "";
            }

            public string text {
                get {return Text;}
                set {Text = value;}
            }

            public double x {
                get {
                    return _x;
                }
                set {
                    moveTo (value, _y);
                }
            }

            public double y {
                get {
                    return _y;
                }
                set {
                    moveTo (_x, value);
                }
            }

            public void moveTo (object x, object y)
            {
                _x = System.Convert.ToDouble (x);
                _y = System.Convert.ToDouble (y);
                InvokeBlocking (delegate {
                    window.canvas.Move(this, (int)_x, (int)_y);
                    window.QueueDraw ();
                });
            }

            public GraphicsRepresentation draw (WindowClass win)
            { // button
                window = win;
                Invoke (delegate {
                    if (gui_thread_id != -1)
                    Show ();
                window.getCanvas ().Put (this, (int)_x, (int)_y);
                window.QueueDraw ();
                });
                return new GraphicsRepresentation(win);
            }

            /*
               public void connect (string signal, Func<object,Event,object> function)
               {
               Clicked += delegate(object obj, System.EventArgs args) {
               Event evt = new Event ("click", Graphics.currentTime ());
               try {
               Invoke (delegate {
               function (obj, evt);
               });
               } catch (Exception e) {
               Console.Error.WriteLine ("Error in connected function");
               Console.Error.WriteLine (e.Message);
               }
               };
               }
               */
        }

        public class HSlider : Gtk.HScale
        {
            public WindowClass window;
            public double _width;
            public double _x, _y;

            public HSlider (IList iterable, object width) : 
                base(new Gtk.Adjustment (0.0, 0.0, 101.0, 0.1, 1.0, 1.0))
            {
                UpdatePolicy = Gtk.UpdateType.Continuous;
                Digits = 0;
                ValuePos = Gtk.PositionType.Top;
                DrawValue = true;    
                this.width = System.Convert.ToDouble (width);
                _x = System.Convert.ToDouble (iterable [0]);
                _y = System.Convert.ToDouble (iterable [1]);
            }

            public double x {
                get {
                    return _x;
                }
                set {
                    moveTo (value, _y);
                }
            }

            public double y {
                get {
                    return _y;
                }
                set {
                    moveTo (_x, value);
                }
            }

            public double width {
                get {
                    return _width;
                }
                set {
                    _width = value;
                    SetSizeRequest ((int)_width, -1);
                }
            }

            public void moveTo (object x, object y)
            {
                _x = System.Convert.ToDouble (x);
                _y = System.Convert.ToDouble (y);
                InvokeBlocking (delegate {
                    window.canvas.Move(this, (int)_x, (int)_y);
                    window.QueueDraw ();
                });
            }

            public GraphicsRepresentation draw (WindowClass win)
            { // hslider
                window = win;
                Invoke (delegate {
                    if (gui_thread_id != -1)
                    Show ();
                window.getCanvas ().Put (this, (int)_x, (int)_y);
                window.QueueDraw ();
                });
                return new GraphicsRepresentation(win);
            }

            public void connect (string signal, Func<object,Event,object> function)
            {
                if (signal.Equals ("change-value")) {
                    ChangeValue += delegate(object obj, Gtk.ChangeValueArgs args) {
                        Event evt = new Event (signal, (object)Value, Graphics.currentTime ());
                        try {
                            Invoke (delegate {
                                function (obj, evt);
                            });
                        } catch (Exception e) {
                            Console.Error.WriteLine ("Error in connected function");
                            Console.Error.WriteLine (e.Message);
                        }        
                    };
                } else {
                    throw new Exception ("invalid signal for this object");
                }
            }
        }

        public class VSlider : Gtk.VScale
        {
            public WindowClass window;
            public double _height;
            public double _x, _y;

            public VSlider (IList iterable, object height) : 
                base(new Gtk.Adjustment (0.0, 0.0, 101.0, 0.1, 1.0, 1.0))
            {
                UpdatePolicy = Gtk.UpdateType.Continuous;
                Digits = 0;
                ValuePos = Gtk.PositionType.Left;
                DrawValue = true;    
                this.height = System.Convert.ToDouble (height);
                _x = System.Convert.ToDouble (iterable [0]);
                _y = System.Convert.ToDouble (iterable [1]);
            }

            public double x {
                get {
                    return _x;
                }
                set {
                    moveTo (value, _y);
                }
            }

            public double y {
                get {
                    return _y;
                }
                set {
                    moveTo (_x, value);
                }
            }

            public double height {
                get {
                    return _height;
                }
                set {
                    _height = value;
                    SetSizeRequest (-1, (int)_height);
                }
            }

            public void moveTo (object x, object y)
            {
                _x = System.Convert.ToDouble (x);
                _y = System.Convert.ToDouble (y);
                InvokeBlocking (delegate {
                    window.canvas.Move(this, (int)_x, (int)_y);
                    window.QueueDraw ();
                });
            }

            public GraphicsRepresentation draw (WindowClass win)
            { // hslider
                window = win;
                Invoke (delegate {
                    if (gui_thread_id != -1)
                    Show ();
                window.getCanvas ().Put (this, (int)_x, (int)_y);
                window.QueueDraw ();
                });
                return new GraphicsRepresentation(win);
            }

            public void connect (string signal, Func<object,Event,object> function)
            {
                if (signal.Equals ("change-value")) {
                    ChangeValue += delegate(object obj, Gtk.ChangeValueArgs args) {
                        Event evt = new Event (signal, (object)Value, Graphics.currentTime ());
                        try {
                            Invoke (delegate {
                                function (obj, evt);
                            });
                        } catch (Exception e) {
                            Console.Error.WriteLine ("Error in connected function");
                            Console.Error.WriteLine (e.Message);
                        }        
                    };
                } else {
                    throw new Exception ("invalid signal for this object");
                }
            }
        }

        public class CheckButton : Gtk.CheckButton
        {
            public WindowClass window;
            public double _x, _y;

            public CheckButton (IList iterable, string label) : 
                base(label)
            {
                _x = System.Convert.ToDouble (iterable [0]);
                _y = System.Convert.ToDouble (iterable [1]);
            }

            public double x {
                get {
                    return _x;
                }
                set {
                    moveTo (value, _y);
                }
            }

            public double y {
                get {
                    return _y;
                }
                set {
                    moveTo (_x, value);
                }
            }

            public void moveTo (object x, object y)
            {
                _x = System.Convert.ToDouble (x);
                _y = System.Convert.ToDouble (y);
                InvokeBlocking (delegate {
                    window.canvas.Move(this, (int)_x, (int)_y);
                    window.QueueDraw ();
                });
            }

            public GraphicsRepresentation draw (WindowClass win)
            { // CheckButton
                window = win;
                Invoke (delegate {
                    if (gui_thread_id != -1)
                    Show ();
                window.getCanvas ().Put (this, (int)_x, (int)_y);
                window.QueueDraw ();
                });
                return new GraphicsRepresentation(win);
            }

            public void connect (string signal, Func<object,Event,object> function)
            {
                if (signal.Equals ("change-value")) {
                    Toggled += delegate(object obj, System.EventArgs args) {
                        Event evt = new Event (signal, (object)Active, Graphics.currentTime ());
                        try {
                            Invoke (delegate {
                                function (obj, evt);
                            });
                        } catch (Exception e) {
                            Console.Error.WriteLine ("Error in connected function");
                            Console.Error.WriteLine (e.Message);
                        }        
                    };
                } else {
                    throw new Exception ("invalid signal for this object");
                }
            }
        }

        public class RadioButton : Gtk.RadioButton
        {
            public WindowClass window;
            public double _x, _y;

            public RadioButton (IList iterable, string label) : 
                base(label)
            {
                Active = true;
                _x = System.Convert.ToDouble (iterable [0]);
                _y = System.Convert.ToDouble (iterable [1]);
            }

            public RadioButton (IList iterable, string label, RadioButton button) : 
                base(button, label)
            {
                // Add this button to Group
                button.Group.Append(this);
                // Now, make sure all buttons in that group
                // have all:
                foreach(Gtk.RadioButton b1 in button.Group) {
                    foreach(Gtk.RadioButton b2 in button.Group) {
                        bool found = false;
                        foreach (Gtk.RadioButton b3 in b1.Group) {
                            if (b3 == b2) {
                                found = true;
                                break;
                            }
                        }
                        if (! found) {
                            b1.Group.Append(b2);
                        }           
                    }
                }
                Active = false;
                _x = System.Convert.ToDouble (iterable [0]);
                _y = System.Convert.ToDouble (iterable [1]);
            }

            public double x {
                get {
                    return _x;
                }
                set {
                    moveTo (value, _y);
                }
            }

            public double y {
                get {
                    return _y;
                }
                set {
                    moveTo (_x, value);
                }
            }

            public void moveTo (object x, object y)
            {
                _x = System.Convert.ToDouble (x);
                _y = System.Convert.ToDouble (y);
                InvokeBlocking (delegate {
                    window.canvas.Move(this, (int)_x, (int)_y);
                    window.QueueDraw ();
                });
            }

            public GraphicsRepresentation draw (WindowClass win)
            { // CheckButton
                window = win;
                Invoke (delegate {
                    if (gui_thread_id != -1)
                    Show ();
                window.getCanvas ().Put (this, (int)_x, (int)_y);
                window.QueueDraw ();
                });
                return new GraphicsRepresentation(win);
            }

            public void connect (string signal, Func<object,Event,object> function)
            {
                if (signal.Equals ("change-value")) {
                    Toggled += delegate(object obj, System.EventArgs args) {
                        Event evt = new Event (signal, (object)Active, Graphics.currentTime ());
                        try {
                            Invoke (delegate {
                                function (obj, evt);
                            });
                        } catch (Exception e) {
                            Console.Error.WriteLine ("Error in connected function");
                            Console.Error.WriteLine (e.Message);
                        }        
                    };
                } else {
                    throw new Exception ("invalid signal for this object");
                }
            }
        }

        // End of Gtk-based objects
        // ----------------------------------------------------------------------------------

        public class Rectangle : Shape
        {			
            public Rectangle (IList iterable1, IList iterable2) : base(true)
            {
                set_points (new Point (iterable1 [0], iterable1 [1]),
                        new Point (iterable2 [0], iterable1 [1]),
                        new Point (iterable2 [0], iterable2 [1]),
                        new Point (iterable1 [0], iterable2 [1]));
            }

			// JH: copy - start
			public Rectangle(): base(true){}
			
			public override Shape copy(){
				Rectangle shapeCopy = new Rectangle();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public double width {
                get { return points [2].x - points [0].x;}
            }

            public double height {
                get { return points [2].y - points [0].y;}
            }

            public override void addToPhysics ()
            { // Rectangle
                world = window._canvas.world;
                lock(world) {
                    float MeterInPixels = 64.0f;
                    // from x,y to meters of window
                    // arbitrary:
                    Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                            ((float)y) / MeterInPixels);
                    body = FarseerPhysics.Factories.BodyFactory.CreateRectangle (
                            world,
                            (float)(width / MeterInPixels), // radius in meters
                            (float)(height / MeterInPixels), // radius in meters
                            _density, // density
                            position);                        // center
                    // Give it some bounce and friction
                    body.Restitution = _bounce;
                    body.Rotation = (float)_rotation;
                    body.Friction = _friction;
                    body.BodyType = _bodyType;
                    body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
                    body.UserData = this; // point back to this shape
                    body.FixtureList [0].UserData = this; // point back to this shape
                }
            }
        }

        public class RoundedRectangle : Shape
        {
            public double radius = 0.0;

			// JH: copy - start
			public RoundedRectangle(){}
			
			public void copyAttributes(RoundedRectangle other){
				base.copyAttributes(other);
				radius = other.radius;
			}
			
			public override Shape copy(){
				RoundedRectangle shapeCopy = new RoundedRectangle();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public RoundedRectangle (IList iterable1, IList iterable2, double radius) : base(true)
            {
                set_points (new Point (iterable1 [0], iterable1 [1]),
                        new Point (iterable2 [0], iterable1 [1]),
                        new Point (iterable2 [0], iterable2 [1]),
                        new Point (iterable1 [0], iterable2 [1]));
                this.radius = radius;
            }

            public override void render (Cairo.Context g)
            {
                if (!visible)
                    return;
                // draws rectangles with rounded (circular arc) corners 
                g.Save ();
                Point temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                g.Rotate (_rotation);
                g.Scale (_scaleFactor, _scaleFactor);
                if (points != null) {
                    g.LineWidth = border;
                    double top = points [0].y + radius;
                    double bottom = points [2].y - radius;
                    double left = points [0].x + radius;
                    double right = points [2].x - radius;

                    //g.MoveTo(p1.x, p2.y);
                    g.Arc (left, top, radius, .50 * (Math.PI * 2.0), .75 * (Math.PI * 2.0));
                    g.Arc (right, top, radius, .75 * (Math.PI * 2.0), 1.0 * (Math.PI * 2.0));
                    g.Arc (right, bottom, radius, 1.0 * (Math.PI * 2.0), .25 * (Math.PI * 2.0));
                    g.Arc (left, bottom, radius, .25 * (Math.PI * 2.0), .50 * (Math.PI * 2.0));

                    g.ClosePath ();
					if(clip){
						g.ClipPreserve ();
					}
                    if (_fill != null) {
                        g.Color = _fill._cairo;
                        g.FillPreserve ();
                    }
                    if (_outline != null) {
                        g.Color = _outline._cairo;
                        g.Stroke ();
                    }
                    foreach (Shape shape in shapes) {
                        shape.render (g);
                        shape.updateGlobalPosition (g);
                    }
                    g.Restore ();
                    g.Save();
                    // This time, don't rotate
                    temp = screen_coord (center);
                    g.Translate (temp.x, temp.y);
                    foreach (Shape shape in joints) {
                        // draw a line from center here to center there
                        g.MoveTo (0, 0); //center of this object
                        temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                    shape.center.y - this.center.y));
                        g.LineTo (temp.x, temp.y);
                        g.Stroke ();
                    }
                    g.Restore ();
                    if (has_pen)
                        pen.render (g);
                }
            }
        }

        public class Polygon : Shape
        {

            public Polygon (Line line) : this(line.points)
            {
                center = line.center;
            }

			// The params keyword specifies a variable number of arguments, thus
			// this is also the constructor used when no arguments are provided.
            public Polygon (params object [] points) : base(true)
            {
                Point [] temp = new Point [points.Length];
                int count = 0;
                foreach (object o in points) {
                    if (o is Point)
                        temp [count] = (Point)o;
                    else if (o is IList) {
                        IList i = (IList)o;
                        temp [count] = new Point (i [0], i [1]);
                    } else {
                        throw new Exception ("Polygon: can't convert arg to a point");
                    }
                    count++;
                }
                set_points (temp);
            }

			// JH: copy - start
   			public override Shape copy(){
				Polygon shapeCopy = new Polygon();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end
			

            public override void addToPhysics ()
            { // Polygon
                world = window._canvas.world;
                lock(world) {
                    float MeterInPixels = 64.0f;
                    // from x,y to meters of window
                    FarseerPhysics.Common.Vertices vertices = new FarseerPhysics.Common.Vertices ();
                    // Position is absolute:
                    Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                            ((float)y) / MeterInPixels);
                    // Points should be relative to position:
                    foreach (Point point in points) {
                        vertices.Add (new Vector2 ((float)((point.x) / MeterInPixels), 
                                    (float)((point.y) / MeterInPixels)));
                    }

                    List<FarseerPhysics.Common.Vertices> list = FarseerPhysics.Common.Decomposition.BayazitDecomposer.ConvexPartition(vertices);
                    body = FarseerPhysics.Factories.BodyFactory.CreateCompoundPolygon (world,
                            list, 
                            _density,
                            position);
                    // Give it some bounce and friction
                    body.BodyType = _bodyType;
                    body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
                    body.Restitution = _bounce;
                    body.Rotation = (float)_rotation;
                    body.Friction = _friction;
                    body.UserData = this; // point back to this shape
                    body.FixtureList [0].UserData = this; // point back to this shape
                }
            }
        }

        public class Dot : Circle
        {
            public Dot (int x, int y) : base(new List() {x, y}, 0)
            {
            }

            public Dot (double x, double y) : base(new List() {x, y}, 0)
            {
            }

            public Dot (IList iterable) : base(iterable, 0)
            {
            }

            public Dot (Dot dot) : base(new List() {dot.x, dot.y}, 0)
            {
            }

        }

        public class Circle : Shape
        {
            int _radius;

            public int radius {
                get {
                    return _radius;
                }
                set {
                    _radius = value;
                    QueueDraw ();
                }
            }

            public Circle (IList iterable, int radius) : base(true)
            {
                set_points (new Point (iterable));
                _radius = radius;
            }

			// JH: copy - start
			public Circle(){}
			
			public void copyAttributes(Circle other){
				base.copyAttributes(other);
			    _radius = other._radius;
			}
			
			public override Shape copy(){
				Circle shapeCopy = new Circle();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public override bool hit (double x, double y)
            {
                return (Math.Sqrt(Math.Pow(center.x - x, 2) + Math.Pow(center.y - y, 2)) < _radius);
            }

            public override void addToPhysics ()
            { // Circle
                world = window._canvas.world;
                lock(world) {
                    float MeterInPixels = 64.0f;
                    // from x,y to meters of window
                    // arbitrary:
                    Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                            ((float)y) / MeterInPixels);
                    body = FarseerPhysics.Factories.BodyFactory.CreateCircle (
                            world,
                            radius / MeterInPixels, // radius in meters
                            _density, // density
                            position);                        // center
                    // Give it some bounce and friction
                    body.BodyType = _bodyType;
                    body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
                    body.Restitution = _bounce;
                    body.Rotation = (float)_rotation;
                    body.Friction = _friction;
                    body.UserData = this; // point back to this shape
                    body.FixtureList [0].UserData = this; // point back to this shape
                }
            }

            public override void render (Cairo.Context g)
            {
                if (!visible)
                    return;
                g.Save ();
                // Center is in global screen coords, whatever they are
                Point temp = screen_coord (center);
                // Temp is in Gtk coordinate system
                g.Translate (temp.x, temp.y);
                g.Rotate (_rotation);
                g.Scale (_scaleFactor, _scaleFactor);
                // Now move to 0,0 as origin of shape
                temp = screen_coord (points [0]);
                g.LineWidth = border;
                if (radius == 0) { // actual point
                    g.MoveTo (temp.x, temp.y);
                    g.LineTo (temp.x + 1, temp.y + 1);
                    g.ClosePath ();
                    if (_outline != null) {
                        g.Color = _outline._cairo;
                    }
                    g.Stroke ();
                } else { // radius starting with 1
                    g.LineWidth = border;
                    g.Arc (temp.x, temp.y, radius, 0.0, 2.0 * Math.PI); // x, y, radius, start, end
                    g.ClosePath ();
                }
				if(clip){
					g.ClipPreserve();
				}
                if (gradient != null) {
                    Cairo.Gradient pat;
                    if (gradient.gtype == "linear")
                        pat = new Cairo.LinearGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.p2.x, 
                                gradient.p2.y);
                    else
                        pat = new Cairo.RadialGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.radius1,
                                gradient.p2.x, 
                                gradient.p2.y,
                                gradient.radius2);

                    pat.AddColorStop (0, gradient.c1.getCairo ());
                    pat.AddColorStop (1, gradient.c2.getCairo ());
                    g.Pattern = pat;
                    g.FillPreserve ();
                } else if (_fill != null) {
                    g.Color = _fill._cairo;
                    g.FillPreserve ();
                }
                if (_outline != null) {
                    g.Color = _outline._cairo;
                    g.Stroke ();
                }
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Restore ();
                g.Save();
                // This time, don't rotate
                temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                foreach (Shape shape in joints) {
                    // draw a line from center here to center there
                    g.MoveTo (0, 0); //center of this object
                    temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                shape.center.y - this.center.y));
                    g.LineTo (temp.x, temp.y);
                    g.Stroke ();
                }
                g.Restore ();
                if (has_pen)
                    pen.render (g);
            }

	    public override double getWidth(){
		    return _radius*2;
	    }

	    public override double getHeight(){
		    return _radius*2;
	    }
        }

        public class Oval : Shape
        {
            int _xRadius;
            int _yRadius;

            public int xRadius {
                get {
                    return _xRadius;
                }
                set {
                    _xRadius = value;
                    QueueDraw ();
                }
            }

            public int yRadius {
                get {
                    return _yRadius;
                }
                set {
                    _yRadius = value;
                    QueueDraw ();
                }
            }

            public Oval (IList iterable, int xRadius, int yRadius) : base(true)
            {
                set_points (new Point (iterable));
                _xRadius = xRadius;
                _yRadius = yRadius;
            }

			// JH: copy - start
			public Oval(){}
			
			public void copyAttributes(Oval other){
				base.copyAttributes(other);
			    _xRadius = other._xRadius;
				_yRadius = other._yRadius;
			}
			
			public override Shape copy(){
				Oval shapeCopy = new Oval();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public override void render (Cairo.Context g)
            {
                if (!visible)
                    return;
                g.Save ();
                // Center is in global screen coords, whatever they are
                Point temp = screen_coord (center);
                // Temp is in Gtk coordinate system
                g.Translate (temp.x, temp.y);
                g.Rotate (_rotation);
                g.Scale (_scaleFactor, _scaleFactor);
                // Now, turn into an Oval:
                g.Scale (1.0, ((double)_yRadius) / ((double)_xRadius));
                // Now move to 0,0 as origin of shape
                temp = screen_coord (points [0]);
                g.LineWidth = border;
                g.Arc (temp.x, temp.y, _xRadius, 0.0, 2.0 * Math.PI); // x, y, radius, start, end
                g.ClosePath ();
				if(clip){
					g.ClipPreserve();
				}
                if (gradient != null) {
                    Cairo.Gradient pat;
                    if (gradient.gtype == "linear")
                        pat = new Cairo.LinearGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.p2.x, 
                                gradient.p2.y);
                    else
                        pat = new Cairo.RadialGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.radius1,
                                gradient.p2.x, 
                                gradient.p2.y,
                                gradient.radius2);

                    pat.AddColorStop (0, gradient.c1.getCairo ());
                    pat.AddColorStop (1, gradient.c2.getCairo ());
                    g.Pattern = pat;
                    g.FillPreserve ();
                } else if (_fill != null) {
                    g.Color = _fill._cairo;
                    g.FillPreserve ();
                }
                if (_outline != null) {
                    g.Color = _outline._cairo;
                    g.Stroke ();
                }
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Stroke ();
                g.Restore ();
                g.Save();
                // This time, don't rotate
                temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                foreach (Shape shape in joints) {
                    // draw a line from center here to center there
                    g.MoveTo (0, 0); //center of this object
                    temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                shape.center.y - this.center.y));
                    g.LineTo (temp.x, temp.y);
                    g.Stroke ();
                }
                g.Restore ();
                if (has_pen)
                    pen.render (g);
            }

            public override void addToPhysics ()
            { // Circle
                world = window._canvas.world;
                lock(world) {
                    float MeterInPixels = 64.0f;
                    // from x,y to meters of window
                    // arbitrary:
                    Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                            ((float)y) / MeterInPixels);
                    body = FarseerPhysics.Factories.BodyFactory.CreateEllipse (
                            world,
                            xRadius / MeterInPixels, // x radius in meters
                            yRadius / MeterInPixels, // y radius in meters
                            20,
                            _density, // density
                            position);                        // center
                    // Give it some bounce and friction
                    body.BodyType = _bodyType;
                    body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
                    body.Restitution = _bounce;
                    body.Rotation = (float)_rotation;
                    body.Friction = _friction;
                    body.UserData = this; // point back to this shape
                    body.FixtureList [0].UserData = this; // point back to this shape
                }
            }

	    public override double getWidth(){
		    return _xRadius*2;
	    }

	    public override double getHeight(){
		    return _yRadius*2;
	    }
        }

        public class Pie : Shape
        {
            int _radius;
            double _start;
            double _stop;

            public Pie (IList iterable, int radius, double start, double stop) : base(true)
            {
                set_points (new Point (iterable));
                _radius = radius;
                _start = start;
                _stop = stop;
            }

			// JH: copy - start
			public Pie(){}
			
			public void copyAttributes(Pie other){
				base.copyAttributes(other);
			    _radius = other._radius;
				_start = other._start;
				_stop = other._stop;
			}
			
			public override Shape copy(){
				Pie shapeCopy = new Pie();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public override void addToPhysics ()
            { // Circle
                world = window._canvas.world;
                lock(world) {
                    float MeterInPixels = 64.0f;
                    // from x,y to meters of window
                    // arbitrary:
                    Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                            ((float)y) / MeterInPixels);
                    body = FarseerPhysics.Factories.BodyFactory.CreateCircle (
                            world,
                            radius / MeterInPixels, // radius in meters
                            _density, // density
                            position);                        // center
                    // Give it some bounce and friction
                    body.BodyType = _bodyType;
                    body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
                    body.Restitution = _bounce;
                    body.Rotation = (float)_rotation;
                    body.Friction = _friction;
                    body.UserData = this; // point back to this shape
                    body.FixtureList [0].UserData = this; // point back to this shape
                }
            }

            /*
               public override void addToPhysics() { // Pie
               world = window._canvas.world;
               float MeterInPixels = 64.0f;
            // from x,y to meters of window
            // arbitrary:
            Vector2 position = new Vector2(((float)x)/MeterInPixels, 
            ((float)y)/MeterInPixels);
            // FIXME: set rotation in radians
            body = FarseerPhysics.Factories.BodyFactory.CreateLineArc(
            world,
            (float)((stop - start) * Math.PI/ 180.0),   // radians
            10,                                         // sides
            (float)(radius / MeterInPixels),            // radius in meters
            position,                                   // position
            (float)(0 * Math.PI/ 180.0),                // angle
            true);                                      // close
            // Give it some bounce and friction
            body.BodyType = _bodyType;
            body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
            body.Restitution = _bounce;
            body.Friction = _friction;
            }
            */

            public override void render (Cairo.Context g)
            {
                if (!visible)
                    return;
                g.Save ();
                // Center is in global screen coords, whatever they are
                Point temp = screen_coord (center);
                // Temp is in Gtk coordinate system
                g.Translate (temp.x, temp.y);
                g.Rotate (_rotation);
                g.Scale (_scaleFactor, _scaleFactor);
                // Now move to 0,0 as origin of shape
                temp = screen_coord (points [0]);
                g.LineWidth = border;
                double tstart = start * (Math.PI) / 180.0;
                double tstop = stop * (Math.PI) / 180.0;
                g.MoveTo (temp.x, temp.y);
                g.Arc (temp.x, temp.y, radius, tstart, tstop); // x, y, radius, start, end
                g.ClosePath ();
				if(clip){
					g.ClipPreserve();
				}
                if (gradient != null) {
                    Cairo.Gradient pat;
                    if (gradient.gtype == "linear")
                        pat = new Cairo.LinearGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.p2.x, 
                                gradient.p2.y);
                    else
                        pat = new Cairo.RadialGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.radius1,
                                gradient.p2.x, 
                                gradient.p2.y,
                                gradient.radius2);

                    pat.AddColorStop (0, gradient.c1.getCairo ());
                    pat.AddColorStop (1, gradient.c2.getCairo ());
                    g.Pattern = pat;
                    g.FillPreserve ();
                } else if (_fill != null) {
                    g.Color = _fill._cairo;
                    g.FillPreserve ();
                }
                if (_outline != null) {
                    g.Color = _outline._cairo;
                    g.Stroke ();
                }
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Restore ();
                g.Save();
                // This time, don't rotate
                temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                foreach (Shape shape in joints) {
                    // draw a line from center here to center there
                    g.MoveTo (0, 0); //center of this object
                    temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                shape.center.y - this.center.y));
                    g.LineTo (temp.x, temp.y);
                    g.Stroke ();
                }
                g.Restore ();
                if (has_pen)
                    pen.render (g);
            }

            public int radius {
                get {
                    return _radius;
                }
                set {
                    _radius = value;
                    QueueDraw ();
                }
            }

            public double start {
                get {
                    return _start;
                }
                set {
                    _start = value;
                    QueueDraw ();
                }
            }

            public double stop {
                get {
                    return _stop;
                }
                set {
                    _stop = value;
                    QueueDraw ();
                }
            }

	    public override double getWidth(){
		    return _radius*2;
	    }

	    public override double getHeight(){
		    return _radius*2;
	    }
        }

        public class Arc : Shape
        {
            int _radius;
            double _start;
            double _stop;

            public Arc (IList iterable, int radius, double start, double stop) 
                : base(true)
            {
                set_points (new Point (iterable));
                fill = null;
                _radius = radius;
                _start = start;
                _stop = stop;
            }

			// JH: copy - start
			public Arc(){}
			
			public void copyAttributes(Arc other){
				base.copyAttributes(other);
			    _radius = other._radius;
				_start = other._start;
				_stop = other._stop;
			}
			
			public override Shape copy(){
				Arc shapeCopy = new Arc();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public override void addToPhysics ()
            { // Circle
                world = window._canvas.world;
                lock(world) {
                    float MeterInPixels = 64.0f;
                    // from x,y to meters of window
                    // arbitrary:
                    Vector2 position = new Vector2 (((float)x) / MeterInPixels, 
                            ((float)y) / MeterInPixels);
                    body = FarseerPhysics.Factories.BodyFactory.CreateCircle (
                            world,
                            radius / MeterInPixels, // radius in meters
                            _density, // density
                            position);                        // center
                    // Give it some bounce and friction
                    body.BodyType = _bodyType;
                    body.IsStatic = (_bodyType == FarseerPhysics.Dynamics.BodyType.Static);
                    body.Restitution = _bounce;
                    body.Rotation = (float)_rotation;
                    body.Friction = _friction;
                    body.UserData = this; // point back to this shape
                    body.FixtureList [0].UserData = this; // point back to this shape
                }
            }

            public override void render (Cairo.Context g)
            {
                if (!visible)
                    return;
                g.Save ();
                // Center is in global screen coords, whatever they are
                Point temp = screen_coord (center);
                // Temp is in Gtk coordinate system
                g.Translate (temp.x, temp.y);
                g.Rotate (_rotation);
                g.Scale (_scaleFactor, _scaleFactor);
                // Now move to 0,0 as origin of shape
                temp = screen_coord (points [0]);
                g.LineWidth = border;
                double tstart = start * (Math.PI) / 180.0;
                double tstop = stop * (Math.PI) / 180.0;
                g.Arc (temp.x, temp.y, radius, tstart, tstop); // x, y, radius, start, end
				if(clip){
					g.ClipPreserve();
				}
                if (gradient != null) {
                    Cairo.Gradient pat;
                    if (gradient.gtype == "linear")
                        pat = new Cairo.LinearGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.p2.x, 
                                gradient.p2.y);
                    else
                        pat = new Cairo.RadialGradient (gradient.p1.x,
                                gradient.p1.y, 
                                gradient.radius1,
                                gradient.p2.x, 
                                gradient.p2.y,
                                gradient.radius2);

                    pat.AddColorStop (0, gradient.c1.getCairo ());
                    pat.AddColorStop (1, gradient.c2.getCairo ());
                    g.Pattern = pat;
                    g.FillPreserve ();
                } else if (_fill != null) {
                    g.Color = _fill._cairo;
                    g.FillPreserve ();
                }
                if (_outline != null) {
                    g.Color = _outline._cairo;
                    g.Stroke ();
                }
                foreach (Shape shape in shapes) {
                    shape.render (g);
                    shape.updateGlobalPosition (g);
                }
                g.Restore ();
                g.Save();
                // This time, don't rotate
                temp = screen_coord (center);
                g.Translate (temp.x, temp.y);
                foreach (Shape shape in joints) {
                    // draw a line from center here to center there
                    g.MoveTo (0, 0); //center of this object
                    temp = screen_coord(new Point(shape.center.x - this.center.x, 
                                shape.center.y - this.center.y));
                    g.LineTo (temp.x, temp.y);
                    g.Stroke ();
                }
                g.Restore ();
                if (has_pen)
                    pen.render (g);
            }

            public int radius {
                get {
                    return _radius;
                }
                set {
                    _radius = value;
                    QueueDraw ();
                }
            }

            public double start {
                get {
                    return _start;
                }
                set {
                    _start = value;
                    QueueDraw ();
                }
            }

            public double stop {
                get {
                    return _stop;
                }
                set {
                    _stop = value;
                    QueueDraw ();
                }
            }

	    public override double getWidth(){
		    return _radius*2;
	    }

	    public override double getHeight(){
		    return _radius*2;
	    }
        }

        public class Frame : Shape
        {
            public Frame()
            {
	        // Only used by ClippedFrame, since we don't want to create
	        // useless points.
            }

			// JH: copy - start			
			public override Shape copy(){
				Frame shapeCopy = new Frame();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public Frame (int x, int y)
            {
                set_points (new Point (x, y));
            }

            public Frame (IList iterable)
            {
                set_points (new Point (iterable));
            }

            public override void updateFromPhysics(){
                foreach(Shape shape in shapes) {
                    shape.updateFromPhysics();
                }
            }

        }


        public class ClippedFrame : Frame
        {	      
            public ClippedFrame (IList iterable1, IList iterable2)
            {
				points = new Point[4];
				points[0] = new Point (iterable1 [0], iterable1 [1]);
				points[1] = new Point (iterable2 [0], iterable1 [1]);
				points[2] = new Point (iterable2 [0], iterable2 [1]);
				points[3] = new Point (iterable1 [0], iterable2 [1]);
                /* set_points (new Point (iterable1 [0], iterable1 [1]), */
                /*         new Point (iterable2 [0], iterable1 [1]), */
                /*         new Point (iterable2 [0], iterable2 [1]), */
                /*         new Point (iterable1 [0], iterable2 [1])); */
				_fill = null;
				_outline = null;
				clip = true;
            }

			// JH: copy - start
			public ClippedFrame(){}
			
			public override Shape copy(){
				ClippedFrame shapeCopy = new ClippedFrame();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public double width {
                get { return points[2].x - points[0].x;}
            }

            public double height {
                get { return points[2].y - points[0].y;}
            }


            public override void render(Cairo.Context context){
				if(!visible) return;
				
				Point temp;
				temp = screen_coord (center);
		
                context.Save();
                context.Translate (temp.x, temp.y);
                context.Rotate (_rotation);
                context.Scale (_scaleFactor, _scaleFactor);
                context.NewPath();
                context.MoveTo(points[0].x, points[0].y);
                for(int i=1; i<points.Length; ++i){
                    context.LineTo(points[i].x, points[i].y);
                }
				context.ClosePath ();
				if (clip){
					context.ClipPreserve();
				}
				if (gradient != null) {
					Cairo.Gradient pat;
					if (gradient.gtype == "linear")
						pat = new Cairo.LinearGradient (gradient.p1.x,
														gradient.p1.y, 
														gradient.p2.x, 
														gradient.p2.y);
					else
						pat = new Cairo.RadialGradient (gradient.p1.x,
														gradient.p1.y, 
														gradient.radius1,
														gradient.p2.x, 
														gradient.p2.y,
														gradient.radius2);
					
					pat.AddColorStop (0, gradient.c1.getCairo ());
					pat.AddColorStop (1, gradient.c2.getCairo ());
					context.Pattern = pat;
					context.FillPreserve ();
				} else if (_fill != null) {
					context.Color = _fill.getCairo ();
					context.FillPreserve ();
				}
				context.NewPath();
				
				foreach (Shape shape in shapes) {
                    shape.render (context);
                    shape.updateGlobalPosition (context);
                }
                //base.render(context);
                context.Restore();

				// Because we don't want the border of the clipped frame to be
				// clipped, while we still want to respect any previous applied
				// clips, we have draw the frame completely seperately.
				if (_outline != null) {
					context.Save();
					context.Translate (temp.x, temp.y);
					context.Rotate (_rotation);
					context.Scale (_scaleFactor, _scaleFactor);
					context.NewPath();
					context.MoveTo(points[0].x, points[0].y);
					for(int i=1; i<points.Length; ++i){
						context.LineTo(points[i].x, points[i].y);
					}
					context.ClosePath ();
					context.LineWidth = border;
					context.Color = _outline.getCairo ();
					context.Stroke ();
					context.Restore();
				}
            }
        }

        public class Node
        {
            public Graph graph;
            public string name;
            public string label;
            public string shape;

            public Node(string name) {
                this.name = name;
            }
            public Shape getShape() {
                return graph.vertices[name]["shape"];
            }
        }

        public class Edge
        {
            public Graph graph;
            public string label;
            public string recordFrom;
            public string recordTo;
            public string nameFrom;
            public string nameTo;

            public Edge(string nameFrom, string recordFrom, string nameTo, string recordTo) {
                this.nameFrom = nameFrom;
                this.recordFrom = recordFrom;
                this.nameTo = nameTo;
                this.recordTo = recordTo;
            }

            public string getFromName() {
                if (recordFrom != null)
                    return String.Format("{0}:{1}", nameFrom, recordFrom);
                return nameFrom;
            }
            public string getToName() {
                if (recordTo != null)
                    return String.Format("{0}:{1}", nameTo, recordTo);
                return nameTo;
            }
        }

        public class Graph
        {
            public static double graph_count = 1;
            public static double node_count = 1;
            public Graphics.WindowClass window = null;
            public Dictionary<string,Dictionary<string,Shape>> vertices = new Dictionary<string,Dictionary<string,Shape>> ();
            public Dictionary<string,Dictionary<string,object>> edges = new Dictionary<string, Dictionary<string,object>> ();
            public Dictionary<string,object> options = new Dictionary<string,object> {
                {"default_shape", "circle"},
                    {"default_outline", "black"},
                    {"fill", "white"},
                    {"line_type", "curve"},
            };
            public Graphviz4Net.Dot.AntlrParser.AntlrParserAdapter<string> parser = null;
            public Graphviz4Net.Dot.DotGraph<string> graph = null;
            public Dictionary<string,Node> graphNodes = new Dictionary<string,Node>();
            public List<Edge> graphEdges = new List<Edge>();
            public string pre_text;
            public string post_text;
            double scale = 1.0;

            public Graph () {
            }

            public Graph (string text) {
                layout(text, true);
                draw();
            }

            public Graph (string text, bool process) {
                layout(text, process);
                draw();
            }

            public Graph (string text, 
                    IDictionary<string,object> options, 
                    bool process) {
                layout(text, process);
                foreach(KeyValuePair<string,object> kvp in (IDictionary<string,object>)options) {
                    this.options[kvp.Key] = kvp.Value;
                }
                draw();
            }

            public void addNode(string name) {
                graphNodes[name] = new Graphics.Node(name);
                graphNodes[name].graph = this;
            }

            public void addNode(Node node) {
                graphNodes[node.name] = node;
                graphNodes[node.name].graph = this;
            }

            public Node lookupNode(string name) {
                foreach(Node node in graphNodes.Values) {
                    if (node.name == name)
                        return node;
                }
                return null;
            }

            public List<Shape> getEdgeLines(object id) {
                return (List<Shape>)edges[id.ToString()]["line"];
            }

            public Shape getNode(object id) {
                return (Shape)vertices[id.ToString()]["shape"];
            }

            public void addEdge(string nameFrom, string nameTo) {
                string recordFrom = null;
                string recordTo = null;
                if (nameFrom.Contains(":")) {
                    string [] parts = nameFrom.Split(':');
                    nameFrom = parts[0];
                    recordFrom = parts[1];
                }
                if (nameTo.Contains(":")) {
                    string [] parts = nameTo.Split(':');
                    nameTo = parts[0];
                    recordTo = parts[1];
                }
                Edge edge = new Edge(nameFrom, recordFrom, nameTo, recordTo);
                graphEdges.Add(edge);
                edge.graph = this;
            }

            Point translate(double x, double y) {
                // used in graphviz graphics
                Point p = null;
                InvokeBlocking( delegate {
                    p = new Point(x * scale + window._width/2 - graph.Width/ 2 * scale + 16,
                        (graph.Height - y) * scale + window._height/2 - graph.Height/ 2 * scale + 16);
                });
                return p;
            }

            Point translate(double x, double y, double width, double height) {
                // used in graphviz graphics
                Point p = null;
                InvokeBlocking( delegate {
                    p = new Point(x * scale + width/2 - graph.Width/ 2 * scale + 16,
                        (graph.Height - y) * scale + height/2 - graph.Height/ 2 * scale + 16);
                });
                return p;
            }

            public string recurseEdges(IList list, int left, int root, int right) {
                string edges = "";
                if (list == null || list.Count == 0) {
                    // Pass
                } else {
                    if (left < list.Count && list[left] != null) {
                        if (list[left] is IList && ((IList)list[left]).Count > 0) {
                            edges += String.Format("  {0}:left -> {1};\n", list[root], ((IList)list[left])[root]);
                            edges += recurseEdges((IList)list[left], left, root, right);
                        } else if (list[left] != null && !(list[left] is IList)) {
                            edges += String.Format("  {0}:left -> {1};\n", list[root], list[left]);
                        }
                    }
                    if (right < list.Count && list[right] != null) {
                        if (list[right] is IList && ((IList)list[right]).Count > 0) {
                            edges += String.Format("  {0}:right -> {1};\n", list[root], ((IList)list[right])[root]);
                            edges += recurseEdges((IList)list[right], left, root, right);
                        } else if (list[right] != null  && !(list[right] is IList)) {
                            edges += String.Format("  {0}:right -> {1};\n", list[root], list[right]);
                        }
                    }
                }
                return edges;
            }

            public string recurseNodes(IList list, int left, int root, int right) {
                string nodes = "";
                if (list != null && list.Count > 0) {
                    // Left:
                    if (left < list.Count && list[left] is IList)
                        nodes += recurseNodes((IList)list[left], left, root, right);
                    else if (left < list.Count && list[left] != null)
                        nodes += String.Format("  {0} [label=\"<left> | {0} | <right>\"];\n", list[left].ToString());
                    // Root:
                    nodes += String.Format("  {0} [label=\"<left> | {0} | <right>\"];\n", list[root].ToString());
                    // Right:
                    if (right < list.Count && list[right] is IList)
                        nodes += recurseNodes((IList)list[right], left, root, right);
                    else if (right < list.Count && list[right] != null)
                        nodes += String.Format("  {0} [label=\"<left> | {0} | <right>\"];\n", list[right].ToString());
                }
                return nodes;
            }

            public void layout(IList list) {
                layout(list, 0, 1, 2);
            }

            public void layout(IList list, int left, int root, int right) {
                string edges = "digraph {\n";
                edges += @"  node [label=""\\N"", shape=record];";
                edges += "\n";
                edges += recurseNodes(list, left, root, right); // left, root, right
                edges += recurseEdges(list, left, root, right); // left, root, right
                edges += "}";
                layout(edges);
            }

            public string getLayout() {
                string edges = "digraph {\n";
                Dictionary<string,int> tnodes = new Dictionary<string,int>();
                int count = 1;
                foreach (Edge edge in graphEdges) {
                    foreach (string tnode in new string[] {edge.getFromName(), edge.getToName()}) {
                        if (!tnodes.ContainsKey(tnode)) {
                            tnodes[tnode] = count;
                            count++;
                        }
                    }
                }
                foreach (KeyValuePair<string,int> tnode in tnodes) {
                    edges += String.Format("    N{0} [label=\"{1}\"];\n", tnode.Value, tnode.Key);
                }
                foreach (Edge edge in graphEdges) {
                    edges += String.Format("    N{0} -> N{1}\n", tnodes[edge.getFromName()], tnodes[edge.getToName()]);
                }
                edges += "}";
                return edges;
            }

            public void layout() {
                string edges = getLayout();
                layout(edges);
            }

            public void layout (string contents) {
                layout(contents, true);
            }

            public void layout (string contents, bool process)
            {
                pre_text = contents; // for debugging
                if (process) {
                    string new_contents = "";
                    // remove any line that starts with graph [...]
                    foreach(string line in contents.Split('\n')) {
                        if (line.Trim().StartsWith("graph [")) {
                            // skip it
                        } else {
                            new_contents += line + "\n";
                        }
                    }
                    contents = new_contents;
                    contents = processDot(contents);
                }
                post_text = contents; // for debugging
                parser = Graphviz4Net.Dot.AntlrParser.AntlrParserAdapter<string>.GetParser();
                graph = parser.Parse (contents);
            }

            public GraphicsRepresentation draw(WindowClass window) {
                this.window = window;
                return draw();
            }

            public GraphicsRepresentation draw(WindowClass window, IDictionary options) {
                this.window = window;
                return draw(options);
            }

            public GraphicsRepresentation draw(IDictionary options) {
                foreach(KeyValuePair<object,object> kvp in (IDictionary<object,object>)options) {
                    this.options[kvp.Key.ToString()] = kvp.Value;
                }
                return draw();
            }

            public GraphicsRepresentation draw() {
                int _width = 0, _height = 0;
                if (window == null) {
                    string label;
                    if (options.ContainsKey("label")) {
                        label = options["label"].ToString();
                    } else if (graph.Attributes.ContainsKey("label")) {
                        label = graph.Attributes["label"].Trim().Split('\n')[0].Trim();
                    } else {
                        label = String.Format("Graph #{0}", Graph.graph_count);
                        Graph.graph_count++;
                    }
                    if (options.ContainsKey("width")) {
                        _width = ((int)options["width"]) + 64;
                    } else {
                        _width = ((int)graph.Width) + 64;
                    }
                    if (options.ContainsKey("height")) {
                        _height = ((int)options["height"]) + 64;
                    } else {
                        _height = ((int)graph.Height) + 64;
                    }
                    window = Graphics.Window(label, _width, _height);
                    // in case we need them:
                    window.addScrollbars(((int)graph.Width) + 32, ((int)graph.Height) + 32);
                }
                if (options.ContainsKey("scale")) {
                    scale = (double)options["scale"];
                } else {
                    scale = Math.Min(_width/(double)graph.Width, _height/(double)graph.Height) * .95;
                }
                foreach(Graphviz4Net.Dot.DotVertex<string> v in graph.Vertices) {
                    if (v.Position == null) {
                        continue;
                    }
                    Point c = translate(((Graphviz4Net.Point)v.Position).X, ((Graphviz4Net.Point)v.Position).Y);
                    int width = (int)(((double)v.Width) * 72 * scale);
                    int height = (int)(((double)v.Height) * 72 * scale);
                    string shape;
                    if (options.ContainsKey("shape")) {
                        shape = options["shape"].ToString();
                    } else if (v.Attributes.ContainsKey("shape")) {
                        shape = v.Attributes["shape"];
                    } else {
                        shape = options["default_shape"].ToString();
                    }
                    Graphics.Color outline;
                    if (options.ContainsKey("outline")) {
                        outline = new Graphics.Color(options["outline"].ToString());
                    } else if  (v.Attributes.ContainsKey("color")) {
                        outline = new Graphics.Color(v.Attributes["color"]);
                    } else {
                        outline = new Graphics.Color(options["default_outline"].ToString());
                    }
                    // Shapes:
                    Shape obj1 = null;
                    Shape obj2 = null;
                    if (shape == "circle" || shape == "ellipse") {
                        obj1 = new Graphics.Oval(c, width/2, height/2);
                    } else if (shape == "doublecircle") {
                        obj1 = new Graphics.Oval(c, width/2, height/2);
                        obj2 = new Graphics.Oval(c, width/2 - 4, height/2 - 4);
                    } else if (shape == "box") {
                        obj1 = new Graphics.Rectangle(new Point(c.x - width/2, c.y - height/2),
                                new Point(c.x + width/2, c.y + height/2));
                        //elif shape == "diamond":
                    } else {
                        throw new Exception(String.Format("unknown shape: {0}", shape));
                    }
                    if (obj1 != null) {
                        obj1.outline = new Graphics.Color(outline);
                        obj1.fill = new Graphics.Color(options["fill"].ToString());
                        obj1.border = 2;
                        obj1.draw(window);
                    }
                    if (obj2 != null) {
                        obj2.outline = new Graphics.Color(outline);
                        obj2.fill = new Graphics.Color(options["fill"].ToString());
                        obj2.border = 2;
                        obj2.draw(window);
                    }
                    // Text:
                    string label;
                    if (v.Attributes.ContainsKey("label")) {
                        label = v.Attributes["label"].Trim();
                    } else {
                        label = v.Id.Trim();
                    }
                    if (label.Contains("|")) {
                        string [] labels = label.Split('|');
                        int parts = labels.Length;
                        for (int divider = 0; divider < labels.Length - 1; divider++) {
                            double x1 = c.x - width/2 + (divider + 1) * width/parts;
                            double y1 = c.y - height/2;
                            double x2 = x1;
                            double y2 = c.y + height/2;
                            Line line = new Graphics.Line(new Point(x1, y1), new Point(x2, y2));
                            line.outline = new Graphics.Color("black");
                            line.draw(window);
                        }
                        label = labels[1].Trim(); // FIXME: should draw each part
                    }
                    // Add label as tag to objects
                    if (obj1 != null) {
                        obj1.tag = label;
                    }
                    if (obj2 != null) {
                        obj2.tag = label;
                    }
                    // Draw text:
                    Graphics.Text text = new Graphics.Text(c, label);
                    text.fontSize = 10 * scale;
                    text.color = new Graphics.Color("black");
                    text.draw(window);
                    // Add group to shape list:
                    vertices[label] = new Dictionary<string,Shape>();
                    if (obj1 != null && obj2 != null) {
                        vertices[label]["shape"] = new Graphics.Group(obj1, obj2);
                    } else {
                        vertices[label]["shape"] = obj1;
                    }
                    vertices[label]["label"] = text;
                }
                int count = 0;
                foreach(Graphviz4Net.Dot.DotEdge<string> e in graph.Edges) {
                    string index;
                    if (e.LabelPos != null) {
                        index = e.Label.Trim();
                    } else {
                        index = count.ToString();
                    }
                    edges[index] = new Dictionary<string,object>();
                    edges[index]["line"] = new List<Shape>();
                    List<Point> points = new List<Point>();
                    foreach(Graphviz4Net.Point p in e.Path) {
                        points.Add(new Graphics.Point(translate(p.X, p.Y)));
                    }
                    string color;
                    if (e.Attributes.ContainsKey("color")) {
                        color = e.Attributes["color"];
                    } else {
                        color = "black";
                    }
                    // Line:
                    if (options["line_type"].ToString() == "curve") {
                        for (int i = 0; i < points.Count/3; i++) {
                            int j = i * 3;
                            Curve line = new Graphics.Curve(points[j], points[j + 1], points[j + 2], points[j + 3]);
                            line.outline = new Graphics.Color(color);
                            line.border = 2;
                            line.draw(window);
                            ((List<Shape>)edges[index]["line"]).Add(line);
                        }
                    } else {
                        Line line = new Graphics.Line(points[0], points[points.Count - 1]);
                        line.outline = new Graphics.Color(color);
                        line.border = 2;
                        line.draw(window);
                        ((List<Shape>)edges[index]["line"]).Add(line);
                    }
                    // Arrows:
                    double w;
                    double h;
                    if (e.SourceArrowEnd != null) {
                        Arrow arrow = new Graphics.Arrow(points[0]);
                        if (options["line_type"].ToString() == "curve") {
                            w = points[0].x - points[1].x;
                            h = points[0].y - points[1].y;
                        } else {
                            w = points[0].x - points[points.Count - 1].x;
                            h = points[0].y - points[points.Count - 1].y;
                        }
                        double degrees = System.Math.Atan2(w, h) * 180/System.Math.PI + 90;
                        arrow.fill = new Graphics.Color(color);
                        arrow.rotate(degrees);
                        arrow.scale(scale);
                        arrow.draw(window);
                        edges[index]["source_arrow"] = arrow;
                    }
                    if (e.DestinationArrowEnd != null) {
                        Arrow arrow = new Graphics.Arrow(points[points.Count - 1]);
                        if (options["line_type"].ToString() == "curve") { // FIXME: these may be backwards:
                            w = points[points.Count - 2].x - points[points.Count - 1].x;
                            h = points[points.Count - 2].y - points[points.Count - 1].y;
                        } else {
                            w = points[0].x - points[points.Count - 1].x;
                            h = points[0].y - points[points.Count - 1].y;
                        }
                        double degrees = System.Math.Atan2(w, h) * 180/System.Math.PI + 90;
                        arrow.fill = new Graphics.Color(color);
                        arrow.rotate(degrees);
                        arrow.scale(scale);
                        arrow.draw(window);
                        edges[index]["destination_arrow"] = arrow;
                    }
                    if (e.LabelPos != null) {
                        Point p = translate(((Graphviz4Net.Point)e.LabelPos).X, 
                                ((Graphviz4Net.Point)e.LabelPos).Y);
                        Text text = new Graphics.Text(p, e.Label);
                        text.fontSize = 10 * scale;
                        text.color = new Graphics.Color("black");
                        text.draw(window);
                        edges[index]["label"] = text;
                    }
                    count++;
                }
                // force it to update, before returning:
                InvokeBlocking( delegate {
                    window.step(0);
                });
                return new GraphicsRepresentation(window);
            }

            public GraphicsRepresentation render(IDictionary options) {
                foreach(KeyValuePair<object,object> kvp in (IDictionary<object,object>)options) {
                    this.options[kvp.Key.ToString()] = kvp.Value;
                }
                return render();
            }

            public GraphicsRepresentation render() { // on Canvas; no window
                int _width = 0, _height = 0;
                string label;
                if (options.ContainsKey("label")) {
                    label = options["label"].ToString();
                } else if (graph.Attributes.ContainsKey("label")) {
                    label = graph.Attributes["label"].Trim().Split('\n')[0].Trim();
                } else {
                    label = String.Format("Graph #{0}", Graph.graph_count);
                    Graph.graph_count++;
                }
                if (options.ContainsKey("width")) {
                    _width = ((int)options["width"]) + 64;
                } else {
                    _width = ((int)graph.Width) + 64;
                }
                if (options.ContainsKey("height")) {
                    _height = ((int)options["height"]) + 64;
                } else {
                    _height = ((int)graph.Height) + 64;
                }
                Graphics.Canvas canvas = new Graphics.Canvas(_width, _height);
                if (options.ContainsKey("scale")) {
                    scale = (double)options["scale"];
                } else {
                    scale = 1.0;
                }
                foreach(Graphviz4Net.Dot.DotVertex<string> v in graph.Vertices) {
                    if (v.Position == null) {
                        continue;
                    }
                    Point c = translate(((Graphviz4Net.Point)v.Position).X, ((Graphviz4Net.Point)v.Position).Y, _width, _height);
                    int width = (int)(((double)v.Width) * 72 * scale);
                    int height = (int)(((double)v.Height) * 72 * scale);
                    string shape;
                    if (options.ContainsKey("shape")) {
                        shape = options["shape"].ToString();
                    } else if (v.Attributes.ContainsKey("shape")) {
                        shape = v.Attributes["shape"];
                    } else {
                        shape = options["default_shape"].ToString();
                    }
                    Graphics.Color outline;
                    if (options.ContainsKey("outline")) {
                        outline = new Graphics.Color(options["outline"].ToString());
                    } else if  (v.Attributes.ContainsKey("color")) {
                        outline = new Graphics.Color(v.Attributes["color"]);
                    } else {
                        outline = new Graphics.Color(options["default_outline"].ToString());
                    }
                    // Shapes:
                    Shape obj1 = null;
                    Shape obj2 = null;
                    if (shape == "circle" || shape == "ellipse") {
                        obj1 = new Graphics.Oval(c, width/2, height/2);
                    } else if (shape == "doublecircle") {
                        obj1 = new Graphics.Oval(c, width/2, height/2);
                        obj2 = new Graphics.Oval(c, width/2 - 4, height/2 - 4);
                    } else if (shape == "box") {
                        obj1 = new Graphics.Rectangle(new Point(c.x - width/2, c.y - height/2),
                                new Point(c.x + width/2, c.y + height/2));
                        //elif shape == "diamond":
                    } else {
                        throw new Exception(String.Format("unknown shape: {0}", shape));
                    }
                    if (obj1 != null) {
                        obj1.outline = new Graphics.Color(outline);
                        obj1.fill = new Graphics.Color(options["fill"].ToString());
                        obj1.border = 2;
                        obj1.draw(canvas);
                    }
                    if (obj2 != null) {
                        obj2.outline = new Graphics.Color(outline);
                        obj2.fill = new Graphics.Color(options["fill"].ToString());
                        obj2.border = 2;
                        obj2.draw(canvas);
                    }
                    // Text:
                    if (v.Attributes.ContainsKey("label")) {
                        label = v.Attributes["label"].Trim();
                    } else {
                        label = v.Id.Trim();
                    }
                    if (label.Contains("|")) {
                        string [] labels = label.Split('|');
                        int parts = labels.Length;
                        for (int divider = 0; divider < labels.Length - 1; divider++) {
                            double x1 = c.x - width/2 + (divider + 1) * width/parts;
                            double y1 = c.y - height/2;
                            double x2 = x1;
                            double y2 = c.y + height/2;
                            Line line = new Graphics.Line(new Point(x1, y1), new Point(x2, y2));
                            line.outline = new Graphics.Color("black");
                            line.draw(canvas);
                        }
                        label = labels[1].Trim(); // FIXME: should draw each part
                    }
                    // Add label as tag to objects
                    if (obj1 != null) {
                        obj1.tag = label;
                    }
                    if (obj2 != null) {
                        obj2.tag = label;
                    }
                    // Draw text:
                    Graphics.Text text = new Graphics.Text(c, label);
                    text.fontSize = 10 * scale;
                    text.color = new Graphics.Color("black");
                    text.draw(canvas);
                    // Add group to shape list:
                    vertices[label] = new Dictionary<string,Shape>();
                    if (obj1 != null && obj2 != null) {
                        vertices[label]["shape"] = new Graphics.Group(obj1, obj2);
                    } else {
                        vertices[label]["shape"] = obj1;
                    }
                    vertices[label]["label"] = text;
                }
                int count = 0;
                foreach(Graphviz4Net.Dot.DotEdge<string> e in graph.Edges) {
                    string index;
                    if (e.LabelPos != null) {
                        index = e.Label.Trim();
                    } else {
                        index = count.ToString();
                    }
                    edges[index] = new Dictionary<string,object>();
                    edges[index]["line"] = new List<Shape>();
                    List<Point> points = new List<Point>();
                    foreach(Graphviz4Net.Point p in e.Path) {
                        points.Add(new Graphics.Point(translate(p.X, p.Y, _width, _height)));
                    }
                    string color;
                    if (e.Attributes.ContainsKey("color")) {
                        color = e.Attributes["color"];
                    } else {
                        color = "black";
                    }
                    // Line:
                    if (options["line_type"].ToString() == "curve") {
                        for (int i = 0; i < points.Count/3; i++) {
                            int j = i * 3;
                            Curve line = new Graphics.Curve(points[j], points[j + 1], points[j + 2], points[j + 3]);
                            line.outline = new Graphics.Color(color);
                            line.border = 2;
                            line.draw(canvas);
                            ((List<Shape>)edges[index]["line"]).Add(line);
                        }
                    } else {
                        Line line = new Graphics.Line(points[0], points[points.Count - 1]);
                        line.outline = new Graphics.Color(color);
                        line.border = 2;
                        line.draw(canvas);
                        ((List<Shape>)edges[index]["line"]).Add(line);
                    }
                    // Arrows:
                    double w;
                    double h;
                    if (e.SourceArrowEnd != null) {
                        Arrow arrow = new Graphics.Arrow(points[0]);
                        if (options["line_type"].ToString() == "curve") {
                            w = points[0].x - points[1].x;
                            h = points[0].y - points[1].y;
                        } else {
                            w = points[0].x - points[points.Count - 1].x;
                            h = points[0].y - points[points.Count - 1].y;
                        }
                        double degrees = System.Math.Atan2(w, h) * 180/System.Math.PI + 90;
                        arrow.fill = new Graphics.Color(color);
                        arrow.rotate(degrees);
                        arrow.scale(scale);
                        arrow.draw(canvas);
                        edges[index]["source_arrow"] = arrow;
                    }
                    if (e.DestinationArrowEnd != null) {
                        Arrow arrow = new Graphics.Arrow(points[points.Count - 1]);
                        if (options["line_type"].ToString() == "curve") { // FIXME: these may be backwards:
                            w = points[points.Count - 2].x - points[points.Count - 1].x;
                            h = points[points.Count - 2].y - points[points.Count - 1].y;
                        } else {
                            w = points[0].x - points[points.Count - 1].x;
                            h = points[0].y - points[points.Count - 1].y;
                        }
                        double degrees = System.Math.Atan2(w, h) * 180/System.Math.PI + 90;
                        arrow.fill = new Graphics.Color(color);
                        arrow.rotate(degrees);
                        arrow.scale(scale);
                        arrow.draw(canvas);
                        edges[index]["destination_arrow"] = arrow;
                    }
                    if (e.LabelPos != null) {
                        Point p = translate(((Graphviz4Net.Point)e.LabelPos).X, 
                                ((Graphviz4Net.Point)e.LabelPos).Y, _width, _height);
                        Text text = new Graphics.Text(p, e.Label);
                        text.fontSize = 10 * scale;
                        text.color = new Graphics.Color("black");
                        text.draw(canvas);
                        edges[index]["label"] = text;
                    }
                    count++;
                }
                // force it to update, before returning:
                //InvokeBlocking( delegate {
                //  window.step(0);
                //    });
                return new GraphicsRepresentation(canvas);
            }

            public GraphicsRepresentation rerender() {
                int _width = 0, _height = 0;
                if (options.ContainsKey("width")) {
                    _width = ((int)options["width"]) + 64;
                } else {
                    _width = ((int)graph.Width) + 64;
                }
                if (options.ContainsKey("height")) {
                    _height = ((int)options["height"]) + 64;
                } else {
                    _height = ((int)graph.Height) + 64;
                }
                Graphics.Canvas canvas = new Graphics.Canvas(_width, _height);
                foreach(var item in vertices.Values) {
                    foreach(Shape shape in item.Values) {
                        shape.draw(canvas);
                    }
                }
                foreach(var item in edges.Values) {
                    foreach(var thing in item.Values) {
                        if (thing is IList) {
                            foreach(Shape line in ((IList)thing)) {
                                line.draw(canvas);
                            }
                        } else {
                            ((Shape)thing).draw(canvas);
                        }
                    }
                }
                return new GraphicsRepresentation(canvas);
            }

            [method: JigsawTab("G/Misc")]
            public static string processDot(string text) {
                string retval = null;
                Process myProcess = new Process();

                // create a temporary file with the text to be processed
                var textpath = System.IO.Path.GetTempFileName();
                using (TextWriter writer = File.CreateText(textpath)){
                    writer.WriteLine(text);
                }

                try {
                    myProcess.StartInfo.UseShellExecute = false;

                    if (Graphics.os_name == "Windows") {
                        string file = Graphics.startup_path;
                        //file = Path.Combine(file, "bin");
                        file = System.IO.Path.Combine(file, "windows");
                        file = System.IO.Path.Combine(file, "dot");
                        myProcess.StartInfo.FileName = System.IO.Path.Combine(file, "dot.exe");
                    } else {
                        myProcess.StartInfo.FileName = "dot";
                    }
                    myProcess.StartInfo.CreateNoWindow = true;
                    myProcess.StartInfo.RedirectStandardOutput = true;
                    myProcess.StartInfo.RedirectStandardError = true;
                    myProcess.StartInfo.Arguments = ("-Tdot " + textpath);
                    myProcess.Start();
                    retval = myProcess.StandardOutput.ReadToEnd();
                    string errors = myProcess.StandardError.ReadToEnd();
                    if (errors != "") {
                        Console.Error.WriteLine();
                    }
                    myProcess.WaitForExit();
                } catch (Exception) {
                    if (warn_missing_dot) {
                        Console.WriteLine("WARNING: missing dot command");
                        warn_missing_dot = false; // just once
                    }
                }
                return retval;
            }

        }

        public class Group : Shape
        {
            public List<Shape> items = new List<Shape> ();
            public string mode = "individual"; // squadron or individual

            public Group (params Shape [] shapes) : base(false)
            {
                foreach (Shape shape in shapes) {
                    items.Add (shape);
                }
                center = new Point (0, 0);
            }

			// JH: copy - start
			public Group(): base(false){}
			
			public void copyAttributes(Group other){
				base.copyAttributes(other);
			    foreach(Shape item in other.items){
					items.Add(item);
				}
				mode = other.mode;
			}
			
			public override Shape copy(){
				Group shapeCopy = new Group();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public override void rotate (double degrees)
            {
                // rotate group
                if (mode == "individual") {
                    foreach (Shape shape in items) {
                        shape.rotate (degrees);
                    }
                } else {
                    // find center of screen positions
                    double x = 0, y = 0;
                    foreach (Shape shape in items) {
                        x += shape.center.x;
                        y += shape.center.y;
                    }
                    center.x = x / (items.Count);
                    center.y = y / (items.Count);
                    // rotate them
                    foreach (Shape shape in items) {
                        shape.rotate (degrees);
                    }
                }
            }

            public override void rotateTo (double degrees)
            {
                foreach (Shape shape in items) {
                    shape.rotateTo (degrees);
                }
            }

            public override void moveTo (double x, double y)
            {
                // need an offset to moveTo for each
                // first compute center
                if (items.Count > 0) {
                    double cx = 0;
                    double cy = 0;
                    foreach (Shape shape in items) {
                        cx += shape.x;
                        cy += shape.y;
                    }
                    cx = cx / (items.Count);
                    cy = cy / (items.Count);
                    foreach (Shape shape in items) {
                        shape.move(x -cx, y - cy);
                    }
                }
            }

            public override void move (double x, double y)
            {
                foreach (Shape shape in items) {
                    shape.move (x, y);
                }
            }
        }

        public class AnimatedGifEncoder
        {
            protected int width; // image size
            protected int height;
            protected System.Drawing.Color transparent = System.Drawing.Color.Empty; // transparent color if given
            protected int transIndex; // transparent index in color table
            protected int repeat = -1; // no repeat
            protected int delay = 0; // frame delay (hundredths)
            protected bool started = false; // ready to output frames
            //  protected BinaryWriter bw;
            protected MemoryStream  ms;
            //      protected FileStream fs;

            //protected Image image; // current frame
            protected Picture picture;
            protected byte[] pixels; // BGR byte array from frame
            protected byte[] indexedPixels; // converted frame indexed to palette
            protected int colorDepth; // number of bit planes
            protected byte[] colorTab; // RGB palette
            protected bool[] usedEntry = new bool[256]; // active palette entries
            protected int palSize = 7; // color table size (bits-1)
            protected int dispose = -1; // disposal code (-1 = use default)
            protected bool closeStream = false; // close stream when finished
            protected bool firstFrame = true;
            protected bool sizeSet = false; // if false, get size from first frame
            protected int sample = 10; // default sample interval for quantizer

            /**
             * Sets the delay time between each frame, or changes it
             * for subsequent frames (applies to last frame added).
             *
             * @param ms int delay time in milliseconds
             */
            public void SetDelay(int ms) 
            {
                delay = ( int ) Math.Round(ms / 10.0f);
            }

            /**
             * Sets the GIF frame disposal code for the last added frame
             * and any subsequent frames.  Default is 0 if no transparent
             * color has been set, otherwise 2.
             * @param code int disposal code.
             */
            public void SetDispose(int code) 
            {
                if (code >= 0) 
                {
                    dispose = code;
                }
            }

            /**
             * Sets the number of times the set of GIF frames
             * should be played.  Default is 1; 0 means play
             * indefinitely.  Must be invoked before the first
             * image is added.
             *
             * @param iter int number of iterations.
             * @return
             */
            public void SetRepeat(int iter) 
            {
                if (iter >= 0) 
                {
                    repeat = iter;
                }
            }

            /**
             * Sets the transparent color for the last added frame
             * and any subsequent frames.
             * Since all colors are subject to modification
             * in the quantization process, the color in the final
             * palette for each frame closest to the given color
             * becomes the transparent color for that frame.
             * May be set to null to indicate no transparent color.
             *
             * @param c System.Drawing.Color to be treated as transparent on display.
             */
            public void SetTransparent(System.Drawing.Color c) 
            {
                transparent = c;
            }

            /**
             * Adds next GIF frame.  The frame is not written immediately, but is
             * actually deferred until the next frame is received so that timing
             * data can be inserted.  Invoking <code>finish()</code> flushes all
             * frames.  If <code>setSize</code> was not invoked, the size of the
             * first image is used for all subsequent frames.
             *
             * @param im BufferedImage containing frame to write.
             * @return true if successful.
             */
            public bool AddFrame(Picture pic) 
            {
                if ((pic == null) || !started) 
                {
                    return false;
                }
                bool ok = true;
                try 
                {
                    if (!sizeSet) 
                    {
                        // use first frame's size
                        SetSize(pic.width, pic.height);
                    }
                    picture = pic;
                    GetImagePixels(); // convert to correct format if necessary
                    AnalyzePixels(); // build color table & map pixels
                    if (firstFrame) 
                    {
                        WriteLSD(); // logical screen descriptior
                        WritePalette(); // global color table
                        if (repeat >= 0) 
                        {
                            // use NS app extension to indicate reps
                            WriteNetscapeExt();
                        }
                    }
                    WriteGraphicCtrlExt(); // write graphic control extension
                    WriteImageDesc(); // image descriptor
                    if (!firstFrame) 
                    {
                        WritePalette(); // local color table
                    }
                    WritePixels(); // encode and write pixel data
                    firstFrame = false;
                } 
                catch (IOException) 
                {
                    ok = false;
                }

                return ok;
            }

            /**
             * Flushes any pending data and closes output file.
             * If writing to an OutputStream, the stream is not
             * closed.
             */
            public bool Finish() 
            {
                if (!started) return false;
                bool ok = true;
                started = false;
                try 
                {
                    ms.WriteByte( 0x3b ); // gif trailer
                    ms.Flush();
                    if (closeStream) 
                    {
                        //                  ms.Close();
                    }
                } 
                catch (IOException) 
                {
                    ok = false;
                }

                // reset for subsequent use
                transIndex = 0;
                //          fs = null;
                //image = null;
                picture = null;
                pixels = null;
                indexedPixels = null;
                colorTab = null;
                closeStream = false;
                firstFrame = true;

                return ok;
            }

            /**
             * Sets frame rate in frames per second.  Equivalent to
             * <code>setDelay(1000/fps)</code>.
             *
             * @param fps float frame rate (frames per second)
             */
            public void SetFrameRate(float fps) 
            {
                if (fps != 0f) 
                {
                    delay = ( int ) Math.Round(100f / fps);
                }
            }

            /**
             * Sets quality of color quantization (conversion of images
             * to the maximum 256 colors allowed by the GIF specification).
             * Lower values (minimum = 1) produce better colors, but slow
             * processing significantly.  10 is the default, and produces
             * good color mapping at reasonable speeds.  Values greater
             * than 20 do not yield significant improvements in speed.
             *
             * @param quality int greater than 0.
             * @return
             */
            public void SetQuality(int quality) 
            {
                if (quality < 1) quality = 1;
                sample = quality;
            }

            /**
             * Sets the GIF frame size.  The default size is the
             * size of the first frame added if this method is
             * not invoked.
             *
             * @param w int frame width.
             * @param h int frame width.
             */
            public void SetSize(int w, int h) 
            {
                if (started && !firstFrame) return;
                width = w;
                height = h;
                if (width < 1) width = 320;
                if (height < 1) height = 240;
                sizeSet = true;
            }

            /**
             * Initiates GIF file creation on the given stream.  The stream
             * is not closed automatically.
             *
             * @param os OutputStream on which GIF images are written.
             * @return false if initial write failed.
             */

            public bool Start( MemoryStream os) 
            {
                if (os == null) return false;
                bool ok = true;
                closeStream = false;
                ms = os;
                try 
                {
                    WriteString("GIF89a"); // header
                } 
                catch (IOException) 
                {
                    ok = false;
                }
                return started = ok;
            }

            /**
             * Initiates writing of a GIF file to a memory stream.
             *
             * @return false if open or initial write failed.
             */
            public bool Start() 
            {
                bool ok = true;
                try 
                {
                    ok = Start(new MemoryStream(10*1024));
                    closeStream = true;
                } 
                catch (IOException) 
                {
                    ok = false;
                }
                return started = ok;
            }

            /**
             * Initiates writing of a GIF file with the specified name.
             *
             * @return false if open or initial write failed.
             */
            public bool Output(string file) 
            {
                try 
                {
                    FileStream fs = new FileStream( file, FileMode.OpenOrCreate, FileAccess.Write, FileShare.None );
                    fs.Write(ms.ToArray(),0,(int) ms.Length);
                    fs.Close();
                }
                catch (IOException) 
                {
                    return false;
                }
                return true;
            }

            public MemoryStream Output()
            {
                return ms;
            }

            /**
             * Analyzes image colors and creates color map.
             */
            protected void AnalyzePixels() 
            {
                int len = pixels.Length;
                int nPix = len / 3;
                indexedPixels = new byte[nPix];
                NeuQuant nq = new NeuQuant(pixels, len, sample);
                // initialize quantizer
                colorTab = nq.Process(); // create reduced palette
                // convert map from BGR to RGB
                //          for (int i = 0; i < colorTab.Length; i += 3) 
                //          {
                //              byte temp = colorTab[i];
                //              colorTab[i] = colorTab[i + 2];
                //              colorTab[i + 2] = temp;
                //              usedEntry[i / 3] = false;
                //          }
                // map image pixels to new palette
                int k = 0;
                for (int i = 0; i < nPix; i++) 
                {
                    int index =
                        nq.Map(pixels[k++] & 0xff,
                                pixels[k++] & 0xff,
                                pixels[k++] & 0xff);
                    usedEntry[index] = true;
                    indexedPixels[i] = (byte) index;
                }
                pixels = null;
                colorDepth = 8;
                palSize = 7;
                // get closest match to transparent color if specified
                if (transparent != System.Drawing.Color.Empty ) 
                {
                    transIndex = FindClosest(transparent);
                }
            }

            /**
             * Returns index of palette color closest to c
             *
             */
            protected int FindClosest(System.Drawing.Color c) 
            {
                if (colorTab == null) return -1;
                int r = c.R;
                int g = c.G;
                int b = c.B;
                int minpos = 0;
                int dmin = 256 * 256 * 256;
                int len = colorTab.Length;
                for (int i = 0; i < len;) 
                {
                    int dr = r - (colorTab[i++] & 0xff);
                    int dg = g - (colorTab[i++] & 0xff);
                    int db = b - (colorTab[i] & 0xff);
                    int d = dr * dr + dg * dg + db * db;
                    int index = i / 3;
                    if (usedEntry[index] && (d < dmin)) 
                    {
                        dmin = d;
                        minpos = index;
                    }
                    i++;
                }
                return minpos;
            }

            /**
             * Extracts image pixels into byte array "pixels"
             */
            protected void GetImagePixels() 
            {
                pixels = new Byte [ 3 * picture.width * picture.height ];
                int p = 0;
                for (int row=0; row < picture.height; row++) {
                    for (int col=0; col < picture.width; col++) {
                        pixels[p++] = (Byte)picture.getRed(col, row);
                        pixels[p++] = (Byte)picture.getGreen(col, row);
                        pixels[p++] = (Byte)picture.getBlue(col, row);
                    }
                }
            }

            /**
             * Writes Graphic Control Extension
             */
            protected void WriteGraphicCtrlExt() 
            {
                ms.WriteByte(0x21); // extension introducer
                ms.WriteByte(0xf9); // GCE label
                ms.WriteByte(4); // data block size
                int transp, disp;
                if (transparent == System.Drawing.Color.Empty ) 
                {
                    transp = 0;
                    disp = 0; // dispose = no action
                } 
                else 
                {
                    transp = 1;
                    disp = 2; // force clear if using transparent color
                }
                if (dispose >= 0) 
                {
                    disp = dispose & 7; // user override
                }
                disp <<= 2;

                // packed fields
                ms.WriteByte( Convert.ToByte( 0 | // 1:3 reserved
                            disp | // 4:6 disposal
                            0 | // 7   user input - 0 = none
                            transp )); // 8   transparency flag

                WriteShort(delay); // delay x 1/100 sec
                ms.WriteByte( Convert.ToByte( transIndex)); // transparent color index
                ms.WriteByte(0); // block terminator
            }

            /**
             * Writes Image Descriptor
             */
            protected void WriteImageDesc()
            {
                ms.WriteByte(0x2c); // image separator
                WriteShort(0); // image position x,y = 0,0
                WriteShort(0);
                WriteShort(width); // image size
                WriteShort(height);
                // packed fields
                if (firstFrame) 
                {
                    // no LCT  - GCT is used for first (or only) frame
                    ms.WriteByte(0);
                } 
                else 
                {
                    // specify normal LCT
                    ms.WriteByte( Convert.ToByte( 0x80 | // 1 local color table  1=yes
                                0 | // 2 interlace - 0=no
                                0 | // 3 sorted - 0=no
                                0 | // 4-5 reserved
                                palSize ) ); // 6-8 size of color table
                }
            }

            /**
             * Writes Logical Screen Descriptor
             */
            protected void WriteLSD()  
            {
                // logical screen size
                WriteShort(width);
                WriteShort(height);
                // packed fields
                ms.WriteByte( Convert.ToByte (0x80 | // 1   : global color table flag = 1 (gct used)
                            0x70 | // 2-4 : color resolution = 7
                            0x00 | // 5   : gct sort flag = 0
                            palSize) ); // 6-8 : gct size

                ms.WriteByte(0); // background color index
                ms.WriteByte(0); // pixel aspect ratio - assume 1:1
            }

            /**
             * Writes Netscape application extension to define
             * repeat count.
             */
            protected void WriteNetscapeExt()
            {
                ms.WriteByte(0x21); // extension introducer
                ms.WriteByte(0xff); // app extension label
                ms.WriteByte(11); // block size
                WriteString("NETSCAPE" + "2.0"); // app id + auth code
                ms.WriteByte(3); // sub-block size
                ms.WriteByte(1); // loop sub-block id
                WriteShort(repeat); // loop count (extra iterations, 0=repeat forever)
                ms.WriteByte(0); // block terminator
            }

            /**
             * Writes color table
             */
            protected void WritePalette()
            {
                ms.Write(colorTab, 0, colorTab.Length);
                int n = (3 * 256) - colorTab.Length;
                for (int i = 0; i < n; i++) 
                {
                    ms.WriteByte(0);
                }
            }

            /**
             * Encodes and writes pixel data
             */
            protected void WritePixels()
            {
                LZWEncoder encoder = new LZWEncoder(indexedPixels, colorDepth);
                encoder.Encode( ms );
            }

            /**
             *    Write 16-bit value to output stream, LSB first
             */
            protected void WriteShort(int value)
            {
                ms.WriteByte( Convert.ToByte( value & 0xff));
                ms.WriteByte( Convert.ToByte( (value >> 8) & 0xff ));
            }

            /**
             * Writes string to output stream
             */
            protected void WriteString(String s)
            {
                char[] chars = s.ToCharArray();
                for (int i = 0; i < chars.Length; i++) 
                {
                    ms.WriteByte((byte) chars[i]);
                }
            }
        }

        public class LZWEncoder 
        {
            private static readonly int EOF = -1;
            private byte[] pixAry;
            private int initCodeSize;

            // GIFCOMPR.C       - GIF Image compression routines
            //
            // Lempel-Ziv compression based on 'compress'.  GIF modifications by
            // David Rowley (mgardi@watdcsu.waterloo.edu)

            // General DEFINEs

            static readonly int BITS = 12;

            static readonly int HSIZE = 5003; // 80% occupancy

            // GIF Image compression - modified 'compress'
            //
            // Based on: compress.c - File compression ala IEEE Computer, June 1984.
            //
            // By Authors:  Spencer W. Thomas      (decvax!harpo!utah-cs!utah-gr!thomas)
            //              Jim McKie              (decvax!mcvax!jim)
            //              Steve Davies           (decvax!vax135!petsd!peora!srd)
            //              Ken Turkowski          (decvax!decwrl!turtlevax!ken)
            //              James A. Woods         (decvax!ihnp4!ames!jaw)
            //              Joe Orost              (decvax!vax135!petsd!joe)

            int n_bits; // number of bits/code
            int maxbits = BITS; // user settable max # bits/code
            int maxcode; // maximum code, given n_bits
            int maxmaxcode = 1 << BITS; // should NEVER generate this code

            int[] htab = new int[HSIZE];
            int[] codetab = new int[HSIZE];

            int hsize = HSIZE; // for dynamic table sizing

            int free_ent = 0; // first unused entry

            // block compression parameters -- after all codes are used up,
            // and compression rate changes, start over.
            bool clear_flg = false;

            // Algorithm:  use open addressing double hashing (no chaining) on the
            // prefix code / next character combination.  We do a variant of Knuth's
            // algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
            // secondary probe.  Here, the modular division first probe is gives way
            // to a faster exclusive-or manipulation.  Also do block compression with
            // an adaptive reset, whereby the code table is cleared when the compression
            // ratio decreases, but after the table fills.  The variable-length output
            // codes are re-sized at this point, and a special CLEAR code is generated
            // for the decompressor.  Late addition:  construct the table according to
            // file size for noticeable speed improvement on small files.  Please direct
            // questions about this implementation to ames!jaw.

            int g_init_bits;

            int ClearCode;
            int EOFCode;

            // output
            //
            // Output the given code.
            // Inputs:
            //      code:   A n_bits-bit integer.  If == -1, then EOF.  This assumes
            //              that n_bits =< wordsize - 1.
            // Outputs:
            //      Outputs code to the file.
            // Assumptions:
            //      Chars are 8 bits long.
            // Algorithm:
            //      Maintain a BITS character long buffer (so that 8 codes will
            // fit in it exactly).  Use the VAX insv instruction to insert each
            // code in turn.  When the buffer fills up empty it and start over.

            int cur_accum = 0;
            int cur_bits = 0;

            int [] masks =
            {
                0x0000,
                0x0001,
                0x0003,
                0x0007,
                0x000F,
                0x001F,
                0x003F,
                0x007F,
                0x00FF,
                0x01FF,
                0x03FF,
                0x07FF,
                0x0FFF,
                0x1FFF,
                0x3FFF,
                0x7FFF,
                0xFFFF };

                // Number of characters so far in this 'packet'
                int a_count;

                // Define the storage for the packet accumulator
                byte[] accum = new byte[256];

                //----------------------------------------------------------------------------
                public LZWEncoder(byte[] pixels, int color_depth) 
                {
                    pixAry = pixels;
                    initCodeSize = Math.Max(2, color_depth);
                }

                // Add a character to the end of the current packet, and if it is 254
                // characters, flush the packet to disk.
                void Add(byte c, Stream outs)
                {
                    accum[a_count++] = c;
                    if (a_count >= 254)
                        Flush(outs);
                }

                // Clear out the hash table

                // table clear for block compress
                void ClearTable(Stream outs)
                {
                    ResetCodeTable(hsize);
                    free_ent = ClearCode + 2;
                    clear_flg = true;

                    Output(ClearCode, outs);
                }

                // reset code table
                void ResetCodeTable(int hsize) 
                {
                    for (int i = 0; i < hsize; ++i)
                        htab[i] = -1;
                }

                void Compress(int init_bits, Stream outs)
                {
                    int fcode;
                    int i /* = 0 */;
                    int c;
                    int ent;
                    int disp;
                    int hsize_reg;
                    int hshift;

                    // Set up the globals:  g_init_bits - initial number of bits
                    g_init_bits = init_bits;

                    // Set up the necessary values
                    clear_flg = false;
                    n_bits = g_init_bits;
                    maxcode = MaxCode(n_bits);

                    ClearCode = 1 << (init_bits - 1);
                    EOFCode = ClearCode + 1;
                    free_ent = ClearCode + 2;

                    a_count = 0; // clear packet

                    var iter = new PixelIter(pixAry);
                    ent = iter.NextPixel();

                    hshift = 0;
                    for (fcode = hsize; fcode < 65536; fcode *= 2)
                        ++hshift;
                    hshift = 8 - hshift; // set hash code range bound

                    hsize_reg = hsize;
                    ResetCodeTable(hsize_reg); // clear hash table

                    Output(ClearCode, outs);

outer_loop : while ((c = iter.NextPixel()) != EOF) 
             {
                 fcode = (c << maxbits) + ent;
                 i = (c << hshift) ^ ent; // xor hashing

                 if (htab[i] == fcode) 
                 {
                     ent = codetab[i];
                     continue;
                 } 
                 else if (htab[i] >= 0) // non-empty slot
                 {
                     disp = hsize_reg - i; // secondary hash (after G. Knott)
                     if (i == 0)
                         disp = 1;
                     do 
                     {
                         if ((i -= disp) < 0)
                             i += hsize_reg;

                         if (htab[i] == fcode) 
                         {
                             ent = codetab[i];
                             goto outer_loop;
                         }
                     } while (htab[i] >= 0);
                 }
                 Output(ent, outs);
                 ent = c;
                 if (free_ent < maxmaxcode) 
                 {
                     codetab[i] = free_ent++; // code -> hashtable
                     htab[i] = fcode;
                 } 
                 else
                     ClearTable(outs);
             }
             // Put out the final code.
             Output(ent, outs);
             Output(EOFCode, outs);
                }

                //----------------------------------------------------------------------------
                public void Encode( Stream os)
                {
                    os.WriteByte( Convert.ToByte( initCodeSize) ); // write "initial code size" byte
                    Compress(initCodeSize + 1, os); // compress and write the pixel data
                    os.WriteByte(0); // write block terminator
                }

                // Flush the packet to disk, and reset the accumulator
                void Flush(Stream outs)
                {
                    if (a_count > 0) 
                    {
                        outs.WriteByte( Convert.ToByte( a_count ));
                        outs.Write(accum, 0, a_count);
                        a_count = 0;
                    }
                }

                int MaxCode(int n_bits) 
                {
                    return (1 << n_bits) - 1;
                }

                void Output(int code, Stream outs)
                {
                    cur_accum &= masks[cur_bits];

                    if (cur_bits > 0)
                        cur_accum |= (code << cur_bits);
                    else
                        cur_accum = code;

                    cur_bits += n_bits;

                    while (cur_bits >= 8) 
                    {
                        Add((byte) (cur_accum & 0xff), outs);
                        cur_accum >>= 8;
                        cur_bits -= 8;
                    }

                    // If the next entry is going to be too big for the code size,
                    // then increase it, if possible.
                    if (free_ent > maxcode || clear_flg) 
                    {
                        if (clear_flg) 
                        {
                            maxcode = MaxCode(n_bits = g_init_bits);
                            clear_flg = false;
                        } 
                        else 
                        {
                            ++n_bits;
                            if (n_bits == maxbits)
                                maxcode = maxmaxcode;
                            else
                                maxcode = MaxCode(n_bits);
                        }
                    }

                    if (code == EOFCode) 
                    {
                        // At EOF, write the rest of the buffer.
                        while (cur_bits > 0) 
                        {
                            Add((byte) (cur_accum & 0xff), outs);
                            cur_accum >>= 8;
                            cur_bits -= 8;
                        }

                        Flush(outs);
                    }
                }
        }

        public class NeuQuant 
        {
            protected static readonly int netsize = 256; /* number of colours used */
            /* four primes near 500 - assume no image has a length so large */
            /* that it is divisible by all four primes */
            protected static readonly int prime1 = 499;
            protected static readonly int prime2 = 491;
            protected static readonly int prime3 = 487;
            protected static readonly int prime4 = 503;
            protected static readonly int minpicturebytes = ( 3 * prime4 );
            /* minimum size for input image */
            /* Program Skeleton
               ----------------
               [select samplefac in range 1..30]
               [read image from input file]
               pic = (unsigned char*) malloc(3*width*height);
               initnet(pic,3*width*height,samplefac);
               learn();
               unbiasnet();
               [write output image header, using writecolourmap(f)]
               inxbuild();
               write output image using inxsearch(b,g,r)      */

            /* Network Definitions
               ------------------- */
            protected static readonly int maxnetpos = (netsize - 1);
            protected static readonly int netbiasshift = 4; /* bias for colour values */
            protected static readonly int ncycles = 100; /* no. of learning cycles */

            /* defs for freq and bias */
            protected static readonly int intbiasshift = 16; /* bias for fractions */
            protected static readonly int intbias = (((int) 1) << intbiasshift);
            protected static readonly int gammashift = 10; /* gamma = 1024 */
            protected static readonly int gamma = (((int) 1) << gammashift);
            protected static readonly int betashift = 10;
            protected static readonly int beta = (intbias >> betashift); /* beta = 1/1024 */
            protected static readonly int betagamma =
                (intbias << (gammashift - betashift));

            /* defs for decreasing radius factor */
            protected static readonly int initrad = (netsize >> 3); /* for 256 cols, radius starts */
            protected static readonly int radiusbiasshift = 6; /* at 32.0 biased by 6 bits */
            protected static readonly int radiusbias = (((int) 1) << radiusbiasshift);
            protected static readonly int initradius = (initrad * radiusbias); /* and decreases by a */
            protected static readonly int radiusdec = 30; /* factor of 1/30 each cycle */

            /* defs for decreasing alpha factor */
            protected static readonly int alphabiasshift = 10; /* alpha starts at 1.0 */
            protected static readonly int initalpha = (((int) 1) << alphabiasshift);

            protected int alphadec; /* biased by 10 bits */

            /* radbias and alpharadbias used for radpower calculation */
            protected static readonly int radbiasshift = 8;
            protected static readonly int radbias = (((int) 1) << radbiasshift);
            protected static readonly int alpharadbshift = (alphabiasshift + radbiasshift);
            protected static readonly int alpharadbias = (((int) 1) << alpharadbshift);

            /* Types and Global Variables
               -------------------------- */

            protected byte[] thepicture; /* the input image itself */
            protected int lengthcount; /* lengthcount = H*W*3 */

            protected int samplefac; /* sampling factor 1..30 */

            //   typedef int pixel[4];                /* BGRc */
            protected int[][] network; /* the network itself - [netsize][4] */

            protected int[] netindex = new int[256];
            /* for network lookup - really 256 */

            protected int[] bias = new int[netsize];
            /* bias and freq arrays for learning */
            protected int[] freq = new int[netsize];
            protected int[] radpower = new int[initrad];
            /* radpower for precomputation */

            /* Initialise network in range (0,0,0) to (255,255,255) and set parameters
               ----------------------------------------------------------------------- */
            public NeuQuant(byte[] thepic, int len, int sample) 
            {

                int i;
                int[] p;

                thepicture = thepic;
                lengthcount = len;
                samplefac = sample;

                network = new int[netsize][];
                for (i = 0; i < netsize; i++) 
                {
                    network[i] = new int[4];
                    p = network[i];
                    p[0] = p[1] = p[2] = (i << (netbiasshift + 8)) / netsize;
                    freq[i] = intbias / netsize; /* 1/netsize */
                    bias[i] = 0;
                }
            }

            public byte[] ColorMap() 
            {
                byte[] map = new byte[3 * netsize];
                int[] index = new int[netsize];
                for (int i = 0; i < netsize; i++)
                    index[network[i][3]] = i;
                int k = 0;
                for (int i = 0; i < netsize; i++) 
                {
                    int j = index[i];
                    map[k++] = (byte) (network[j][0]);
                    map[k++] = (byte) (network[j][1]);
                    map[k++] = (byte) (network[j][2]);
                }
                return map;
            }

            /* Insertion sort of network and building of netindex[0..255] (to do after unbias)
               ------------------------------------------------------------------------------- */
            public void Inxbuild() 
            {

                int i, j, smallpos, smallval;
                int[] p;
                int[] q;
                int previouscol, startpos;

                previouscol = 0;
                startpos = 0;
                for (i = 0; i < netsize; i++) 
                {
                    p = network[i];
                    smallpos = i;
                    smallval = p[1]; /* index on g */
                    /* find smallest in i..netsize-1 */
                    for (j = i + 1; j < netsize; j++) 
                    {
                        q = network[j];
                        if (q[1] < smallval) 
                        { /* index on g */
                            smallpos = j;
                            smallval = q[1]; /* index on g */
                        }
                    }
                    q = network[smallpos];
                    /* swap p (i) and q (smallpos) entries */
                    if (i != smallpos) 
                    {
                        j = q[0];
                        q[0] = p[0];
                        p[0] = j;
                        j = q[1];
                        q[1] = p[1];
                        p[1] = j;
                        j = q[2];
                        q[2] = p[2];
                        p[2] = j;
                        j = q[3];
                        q[3] = p[3];
                        p[3] = j;
                    }
                    /* smallval entry is now in position i */
                    if (smallval != previouscol) 
                    {
                        netindex[previouscol] = (startpos + i) >> 1;
                        for (j = previouscol + 1; j < smallval; j++)
                            netindex[j] = i;
                        previouscol = smallval;
                        startpos = i;
                    }
                }
                netindex[previouscol] = (startpos + maxnetpos) >> 1;
                for (j = previouscol + 1; j < 256; j++)
                    netindex[j] = maxnetpos; /* really 256 */
            }

            /* Main Learning Loop
               ------------------ */
            public void Learn() 
            {

                int i, j, b, g, r;
                int radius, rad, alpha, step, delta, samplepixels;
                byte[] p;
                int pix, lim;

                if (lengthcount < minpicturebytes)
                    samplefac = 1;
                alphadec = 30 + ((samplefac - 1) / 3);
                p = thepicture;
                pix = 0;
                lim = lengthcount;
                samplepixels = lengthcount / (3 * samplefac);
                delta = samplepixels / ncycles;
                alpha = initalpha;
                radius = initradius;

                rad = radius >> radiusbiasshift;
                if (rad <= 1)
                    rad = 0;
                for (i = 0; i < rad; i++)
                    radpower[i] =
                        alpha * (((rad * rad - i * i) * radbias) / (rad * rad));

                //fprintf(stderr,"beginning 1D learning: initial radius=%d\n", rad);

                if (lengthcount < minpicturebytes)
                    step = 3;
                else if ((lengthcount % prime1) != 0)
                    step = 3 * prime1;
                else 
                {
                    if ((lengthcount % prime2) != 0)
                        step = 3 * prime2;
                    else 
                    {
                        if ((lengthcount % prime3) != 0)
                            step = 3 * prime3;
                        else
                            step = 3 * prime4;
                    }
                }

                i = 0;
                while (i < samplepixels) 
                {
                    b = (p[pix + 0] & 0xff) << netbiasshift;
                    g = (p[pix + 1] & 0xff) << netbiasshift;
                    r = (p[pix + 2] & 0xff) << netbiasshift;
                    j = Contest(b, g, r);

                    Altersingle(alpha, j, b, g, r);
                    if (rad != 0)
                        Alterneigh(rad, j, b, g, r); /* alter neighbours */

                    pix += step;
                    if (pix >= lim)
                        pix -= lengthcount;

                    i++;
                    if (delta == 0)
                        delta = 1;
                    if (i % delta == 0) 
                    {
                        alpha -= alpha / alphadec;
                        radius -= radius / radiusdec;
                        rad = radius >> radiusbiasshift;
                        if (rad <= 1)
                            rad = 0;
                        for (j = 0; j < rad; j++)
                            radpower[j] =
                                alpha * (((rad * rad - j * j) * radbias) / (rad * rad));
                    }
                }
                //fprintf(stderr,"finished 1D learning: readonly alpha=%f !\n",((float)alpha)/initalpha);
            }

            /* Search for BGR values 0..255 (after net is unbiased) and return colour index
               ---------------------------------------------------------------------------- */
            public int Map(int b, int g, int r) 
            {

                int i, j, dist, a, bestd;
                int[] p;
                int best;

                bestd = 1000; /* biggest possible dist is 256*3 */
                best = -1;
                i = netindex[g]; /* index on g */
                j = i - 1; /* start at netindex[g] and work outwards */

                while ((i < netsize) || (j >= 0)) 
                {
                    if (i < netsize) 
                    {
                        p = network[i];
                        dist = p[1] - g; /* inx key */
                        if (dist >= bestd)
                            i = netsize; /* stop iter */
                        else 
                        {
                            i++;
                            if (dist < 0)
                                dist = -dist;
                            a = p[0] - b;
                            if (a < 0)
                                a = -a;
                            dist += a;
                            if (dist < bestd) 
                            {
                                a = p[2] - r;
                                if (a < 0)
                                    a = -a;
                                dist += a;
                                if (dist < bestd) 
                                {
                                    bestd = dist;
                                    best = p[3];
                                }
                            }
                        }
                    }
                    if (j >= 0) 
                    {
                        p = network[j];
                        dist = g - p[1]; /* inx key - reverse dif */
                        if (dist >= bestd)
                            j = -1; /* stop iter */
                        else 
                        {
                            j--;
                            if (dist < 0)
                                dist = -dist;
                            a = p[0] - b;
                            if (a < 0)
                                a = -a;
                            dist += a;
                            if (dist < bestd) 
                            {
                                a = p[2] - r;
                                if (a < 0)
                                    a = -a;
                                dist += a;
                                if (dist < bestd) 
                                {
                                    bestd = dist;
                                    best = p[3];
                                }
                            }
                        }
                    }
                }
                return (best);
            }
            public byte[] Process() 
            {
                Learn();
                Unbiasnet();
                Inxbuild();
                return ColorMap();
            }

            /* Unbias network to give int values 0..255 and record position i to prepare for sort
               ----------------------------------------------------------------------------------- */
            public void Unbiasnet() 
            {
                for (int i = 0; i < netsize; i++) 
                {
                    network[i][0] >>= netbiasshift;
                    network[i][1] >>= netbiasshift;
                    network[i][2] >>= netbiasshift;
                    network[i][3] = i; /* record colour no */
                }
            }

            /* Move adjacent neurons by precomputed alpha*(1-((i-j)^2/[r]^2)) in radpower[|i-j|]
               --------------------------------------------------------------------------------- */
            protected void Alterneigh(int rad, int i, int b, int g, int r) 
            {

                int j, k, lo, hi, a, m;
                int[] p;

                lo = i - rad;
                if (lo < -1)
                    lo = -1;
                hi = i + rad;
                if (hi > netsize)
                    hi = netsize;

                j = i + 1;
                k = i - 1;
                m = 1;
                while ((j < hi) || (k > lo)) 
                {
                    a = radpower[m++];
                    if (j < hi) 
                    {
                        p = network[j++];
                        try 
                        {
                            p[0] -= (a * (p[0] - b)) / alpharadbias;
                            p[1] -= (a * (p[1] - g)) / alpharadbias;
                            p[2] -= (a * (p[2] - r)) / alpharadbias;
                        } 
                        catch (Exception) 
                        {
                        } // prevents 1.3 miscompilation
                    }
                    if (k > lo) 
                    {
                        p = network[k--];
                        try 
                        {
                            p[0] -= (a * (p[0] - b)) / alpharadbias;
                            p[1] -= (a * (p[1] - g)) / alpharadbias;
                            p[2] -= (a * (p[2] - r)) / alpharadbias;
                        } 
                        catch (Exception) 
                        {
                        }
                    }
                }
            }

            /* Move neuron i towards biased (b,g,r) by factor alpha
               ---------------------------------------------------- */
            protected void Altersingle(int alpha, int i, int b, int g, int r) 
            {

                /* alter hit neuron */
                int[] n = network[i];
                n[0] -= (alpha * (n[0] - b)) / initalpha;
                n[1] -= (alpha * (n[1] - g)) / initalpha;
                n[2] -= (alpha * (n[2] - r)) / initalpha;
            }

            /* Search for biased BGR values
               ---------------------------- */
            protected int Contest(int b, int g, int r) 
            {

                /* finds closest neuron (min dist) and updates freq */
                /* finds best neuron (min dist-bias) and returns position */
                /* for frequently chosen neurons, freq[i] is high and bias[i] is negative */
                /* bias[i] = gamma*((1/netsize)-freq[i]) */

                int i, dist, a, biasdist, betafreq;
                int bestpos, bestbiaspos, bestd, bestbiasd;
                int[] n;

                bestd = ~(((int) 1) << 31);
                bestbiasd = bestd;
                bestpos = -1;
                bestbiaspos = bestpos;

                for (i = 0; i < netsize; i++) 
                {
                    n = network[i];
                    dist = n[0] - b;
                    if (dist < 0)
                        dist = -dist;
                    a = n[1] - g;
                    if (a < 0)
                        a = -a;
                    dist += a;
                    a = n[2] - r;
                    if (a < 0)
                        a = -a;
                    dist += a;
                    if (dist < bestd) 
                    {
                        bestd = dist;
                        bestpos = i;
                    }
                    biasdist = dist - ((bias[i]) >> (intbiasshift - netbiasshift));
                    if (biasdist < bestbiasd) 
                    {
                        bestbiasd = biasdist;
                        bestbiaspos = i;
                    }
                    betafreq = (freq[i] >> betashift);
                    freq[i] -= betafreq;
                    bias[i] += (betafreq << gammashift);
                }
                freq[bestpos] += beta;
                bias[bestpos] -= betagamma;
                return (bestbiaspos);
            }
        }

        public class PixelIter {
            private static readonly int EOF = -1;
            private byte[] pixAry;
            IEnumerator iter;

            public PixelIter(byte[] pixAry) {
                this.pixAry = pixAry;
                iter = Iter();
                iter.MoveNext(); // position on first
            }

            public int NextPixel() {
                int b = (int)iter.Current;
                iter.MoveNext();
                return b;
            }

            IEnumerator Iter()
            {
                for (int i = 0; i < pixAry.Length; i++) {
                    yield return (pixAry[i] & 0xff);
                }
                yield return EOF;
            }
        }

        [method: JigsawTab(null)]
        public static void Main(string [] args) {
            Gtk.Application.Init();
            var ag = new AnimatedGifEncoder();
            ag.Start();
            ag.AddFrame(new Picture(100, 200));
            ag.Finish();
            ag.Output("test.gif");
        }

        //---------------------------------------------------SPRITE-------------------------------------------------

        public static List<string> getSpriteNames() {
            List<string> sprites = new List<string>();
            string codeBase = System.Reflection.Assembly.GetExecutingAssembly().CodeBase;
            System.UriBuilder uri = new System.UriBuilder(codeBase);
            string path = System.Uri.UnescapeDataString(uri.Path);
            string AssemblyDirectory = System.IO.Path.GetDirectoryName(path);
            string folderPath = System.IO.Path.Combine(AssemblyDirectory, "../examples/images/sprites");

            // First, search raw files in this directory:
            foreach(string file in Directory.EnumerateFiles(folderPath, "*.*")){
                string sname = System.IO.Path.GetFileNameWithoutExtension(file);
                Picture temp = null;
                try {
                    temp = new Graphics.Picture(file);
                } catch { // skip non-image files
                    // not a valid image, keep looking!
                }
                if (temp != null) {
                    sprites.Add(sname);
                }
            }   
            // Next, search each directory:
            foreach(string directory in Directory.EnumerateDirectories(folderPath, "*")){
                // for each costume folder:
                string sname = new DirectoryInfo(directory).Name;
                sprites.Add(sname);
            }
            return sprites;
        }

        public static Dictionary<string, List<Graphics.Shape>> getCostumes(string name) {
            string codeBase = System.Reflection.Assembly.GetExecutingAssembly().CodeBase;
            System.UriBuilder uri = new System.UriBuilder(codeBase);
            string path = System.Uri.UnescapeDataString(uri.Path);
            string AssemblyDirectory = System.IO.Path.GetDirectoryName(path);
            string folderPath = System.IO.Path.Combine(AssemblyDirectory, "../examples/images/sprites");

            Dictionary<string, List<Graphics.Shape>> costumes = new Dictionary<string, List<Graphics.Shape>>();
            // First, search raw files in this directory:
            foreach(string file in Directory.EnumerateFiles(folderPath, "*.*")){
                string sname = System.IO.Path.GetFileNameWithoutExtension(file);
                if (sname == name) { // match!
                    Shape temp = null;
                    try {
                        temp = new Graphics.Picture(file);
                    } catch { // skip non-image files
                        // not a valid image, keep looking!
                    }
                    if (temp != null) {
                        temp.tag = sname;
                        temp.border = 0;
                        temp.x = 0;
                        temp.y = 0;
                        costumes["default"] = new List<Shape>();
                        costumes["default"].Add(temp);
                        return costumes;
                    }
                }
            }   
            // Next, search each directory:
            foreach(string directory in Directory.EnumerateDirectories(folderPath, "*")){
                // for each costume folder:
                string sname = new DirectoryInfo(directory).Name;
                if (sname == name) { // match!
                    // first we find single-picture costumes:
                    foreach(string filename in Directory.EnumerateFiles(directory, "*.*")){
                        Shape temp = null;
                        try {
                            temp = new Graphics.Picture(filename);
                        } catch { // skip non-image files
                            // not valid, skip it
                        }
                        if (temp != null) {
                            string fname = System.IO.Path.GetFileNameWithoutExtension(filename);
                            temp.tag = fname;
                            temp.border = 0;
                            temp.x = 0;
                            temp.y = 0;
                            costumes[fname] = new List<Shape>();
                            costumes[fname].Add(temp);
                        } // else skip
                    }
                    // next we find folder costumes:
                    foreach(string subdirectory in Directory.EnumerateDirectories(directory, "*")){
                        string subname = new DirectoryInfo(subdirectory).Name;
                        costumes[subname] = new List<Shape>();
                        foreach(string file in Directory.EnumerateFiles(subdirectory, "*.*")){
                            try {
                                Picture temp = new Picture(file);
                                string fname = System.IO.Path.GetFileNameWithoutExtension(file);
                                temp.tag = fname;
                                temp.border = 0;
                                temp.x = 0;
                                temp.y = 0;
                                costumes[subname].Add(temp);
                            } catch {
                                // skip non-image files
                            }
                        }
                        //costumes[subname].Sort((x,y) => x.filename.CompareTo(y.filename));
                    }
                    return costumes;
                }
            }
            return null; // nothing found
        }

        public class Sprite : Shape {
            public Graphics.Shape shape; 
            public string costume;
            public Dictionary<string, List<Graphics.Shape> > costumes;
            public int frame = 0;
            public string name;

            public Sprite () {
                name = "undefined";
                costume = "default";
                costumes = new Dictionary<string, List<Graphics.Shape>>();
                costumes["default"] = new List<Graphics.Shape>();
                center = new Point(0,0);
                set_points(center);
            }

			// JH: copy - start
			public void copyAttributes(Sprite other){
				base.copyAttributes(other);
				costume = other.costume;
				foreach(KeyValuePair<string, List<Graphics.Shape> > entry in other.costumes)
				{
					List<Graphics.Shape> newFrames = new List<Graphics.Shape>();
					foreach(Shape shape in entry.Value){
						newFrames.Add(shape.copy());
					}
					costumes[entry.Key] = newFrames;
				}
				frame = other.frame;
				name = other.name;
				shape = costumes[costume][frame];
			}
			
			public override Shape copy(){
				Sprite shapeCopy = new Sprite();
				shapeCopy.copyAttributes(this);
				return shapeCopy;
			}
			// JH: copy - end

            public Sprite (string name) : this(new int [] {0,0}, name){
            }

            public Sprite (IList iterable, string name) : base(true){
                this.name = name;
                costumes = Graphics.getCostumes(name);
                if (costumes == null) {
                    throw new Exception(String.Format("Sprite: no such sprite '{0}'", name));
                }

                costume = getDefaultCostumeName(); // finds a good name for default
                shape = costumes[costume][0]; // get the first picture as default

                center = new Point(iterable);
                set_points(center);

                // Draw picture on sprite:
                shape.draw(this);
            }

            public void addCostume(string cname, Shape shape) {
                // if not pre-existing, adds a new costume name and costume, or
                // if costume name already exists, adds a new frame

	        // JH: Do not undraw, that is highly inefficient
                //if (this.shape != null) {
                //    this.shape.undraw();
                //}
                this.shape = shape;
                shape.draw(this);
                costume = cname;
                if (!costumes.ContainsKey(cname)) {
                    costumes[cname] = new List<Graphics.Shape>();
                }
                costumes[costume].Add(shape);
            }

            public void addFrame(Shape shape) {
                // adds a new frame to the current costume

	        // JH: Do not undraw, that is highly inefficient
                //if (this.shape != null) {
                //    this.shape.undraw();
                //}
                this.shape = shape;
                this.shape.x = 0;
                this.shape.y = 0;
                shape.draw(this);
                costumes[costume].Add(shape);
            }

            public string getDefaultCostumeName() {
                return getDefaultCostumeName(name);
            }

            public string getDefaultCostumeName(string name) {
                if (costumes.ContainsKey("default")) {
                    return "default";
                } else {
                    foreach (KeyValuePair<string,List<Shape>> kvp in costumes) {
                        return kvp.Key; // get first one
                    }
                }
                return null;
            }

            public List<string> getCostumeNames() {
                return new List<string>(costumes.Keys);
            }

            public void updateCostumes() {
                costumes = Graphics.getCostumes(name);
                costume = getDefaultCostumeName(); // finds a good one for default
            }

            public List<Shape> getFrames(string costume) {
                return costumes[costume];
            }

            public List<Shape> getFrames() {
                return costumes[costume];
            }

            public void changeCostume(string name) {
                if (name == costume) { // nothing needed to do
                    return;
                } else {
                    costume = name;
                    frame = 0;
		    // JH: do not undraw
                    //shape.undraw();
                    shape = costumes[name][frame];

		    // JH: do not draw either
		    // Also, don't step the window here
                    //if (window != null) {
                    //    shape.draw(this);
                    //    window.step();
                    //}
                }
            }

            public void selectFrameByTag(string tag) {
                int count = 0;
                foreach (Shape shape in costumes[costume]) {
                    if (shape.tag == tag) {
                        flipToFrame(count);
                        return;
                    } else {
                        count++;
                    }
                }
                throw new Exception(String.Format("no such frame tagged with '{0}' in costume '{1}'", tag, costume));
            }

            public List<string> getFrameTags() {
                List<string> retval = new List<string>();
                foreach (Shape shape in costumes[costume]) {
                    retval.Add(shape.tag);
                }
                return retval;
            }

            public bool isLastFrame() {
                return frame == (costumes[costume].Count - 1);
            }

            public bool isFirstFrame() {
                return frame == 0;
            }

            public void flipToFirstFrame() {
                flipToFrame(0);
            }

            public void flipToLastFrame() {
                flipToFrame(costumes[costume].Count - 1);
            }

            public void flipToNextFrame() {
                flipToFrame((frame + 1) % costumes[costume].Count);
            }

            public void flipToPreviousFrame() {
                flipToFrame((frame - 1) % costumes[costume].Count);
            }

            public void flipToFrame(int frame) {
                if (this.frame == frame) {
                    return;
                } else {
                    Shape previous = this.shape;
                    this.frame = frame;
                    costumes[costume][frame].draw(this);
                    if (previous != null) {
                        previous.undraw();
                    }
                    shape = costumes[costume][frame];
                    if (window != null) {
                        window.step();
                    }
                }
            }

            public void animate(double seconds, double delay=.1) {
                // Animate the shape for N seconds
                Shape previous = shape;
                double timeSteps = seconds/delay;
                int iterations = (int)timeSteps;
                for (int i = 0; i < iterations; i++) {
                    frame = i % costumes[costume].Count;
                    costumes[costume][frame].draw(this);
                    if (previous != null) {
                        previous.undraw();
                    }
                    shape = costumes[costume][frame];
                    if (window != null) {
                        window.step();
                    }
                    previous = shape;
                    waitSeconds(delay);
                }
            }

            public void animateLoop(int iterations=1, double delay=.1) {
                // Animate shape for a number of loops through frames
                Shape previous = shape;
                for (int i = 0; i < iterations; i++) {
                    frame = i % costumes[costume].Count;
                    costumes[costume][frame].draw(this);
                    if (previous != null) {
                        previous.undraw();
                    }
                    shape = costumes[costume][frame];
                    if (window != null) {
                        window.step();
                    }
                    previous = shape;
                    waitSeconds(delay);
                }
            }

            public void animateTo(IList iterable, double seconds, double delay=.1) {
                // Animate to a place
                // FIXME: if no time given, compute how much time it would
                // take to get to x,y
                double x = System.Convert.ToDouble(iterable[0]);
                double y = System.Convert.ToDouble(iterable[1]);
                if (costumes[costume].Count == 0) {
                    throw new Exception("\'" + costume + "\' does not contain any frames.");
                }
                double timeSteps = seconds/delay;
                int iterations = (int)timeSteps;
                Shape previous = shape;
                double dx = (x - this.x)/iterations;
                double dy = (y - this.y)/iterations;
                for (int i = 0; i < iterations; i++) {
                    frame = i % costumes[costume].Count;
                    costumes[costume][frame].draw(this);
                    if (previous != null) {  
                        previous.undraw();
                    }
                    shape = costumes[costume][frame];
                    previous = shape;
                    this.move(dx, dy);
                    if (window != null) {
                        shape.window.step();
                    }
                    waitSeconds(delay);
                }
                //in case we are not at an integer coordinate
                this.moveTo(x, y);
            }

            public override bool hit (double x, double y)
            {
		    if(shape != null){
			    return shape.hit(x, y);
		    } else {
			    return false;
		    }
                // ask the currenCostume what if it got hit:
                /* Point p = new Point (x, y); */
                /* int counter = 0; */
                /* double xinters; */
                /* Point p1, p2; */
                /* if (shape.points != null) { */
                /*     p1 = shape.points [0]; */
                /*     for (int i=1; i<=shape.points.Length; i++) { */
                /*         p2 = shape.points [i % shape.points.Length]; */
                /*         if (p.y > (Math.Min (p1.gy, p2.gy))) { */
                /*             if (p.y <= (Math.Max (p1.gy, p2.gy))) { */
                /*                 if (p.x <= (Math.Max (p1.gx, p2.gx))) { */
                /*                     if (p1.gy != p2.gy) { */
                /*                         xinters = (p.y - p1.gy) * (p2.gx - p1.gx) / (p2.gy - p1.gy) + p1.gx; */
                /*                         if (p1.gx == p2.gx || p.x <= xinters) */
                /*                             counter++; */
                /*                     } */
                /*                 } */
                /*             } */
                /*         } */
                /*         p1 = p2; */
                /*     } */
                /*     return (counter % 2 != 0); // hit? */
                /* } else { */
                /*     return false; */
                /* } */
            }

	    public override double getWidth(){
		    if(shape != null){
			    return shape.getWidth();
		    } else {
			    return 0;
		    }
	    }

	    public override double getHeight(){
		    if(shape != null){
			    return shape.getHeight();
		    } else {
			    return 0;
		    }
	    }
        }
        //----------------------------------------------END OF SPRITE CLASS-----------------------------------------

        public class GraphicsRepresentation {
            WindowClass window = null;

	    //JH: part of the great experiment
	    DockableWindowClass dockableWindow = null;

            Canvas canvas = null;
            Shape shape = null;
            string text = null;

            public GraphicsRepresentation() {
            }

            public GraphicsRepresentation(WindowClass window) {
                this.window = window;
            }

	    //JH: part of the great experiment
	    public GraphicsRepresentation(DockableWindowClass window) {
                this.dockableWindow = window;
            }
	    //JH: part of the great experiment

            public GraphicsRepresentation(string text) {
                this.text = text;
            }

            public GraphicsRepresentation(Canvas canvas) {
                this.canvas = canvas;
            }

            public GraphicsRepresentation(Shape shape) {
                this.shape = shape;
            }

            public virtual IDictionary<string, string> GetRepresentations() {
                if (window != null) {
                    window.step();
                } else if (canvas != null) {
                } else if (shape != null) {
                } else {
                }
                IDictionary<string, string> retval = null;
                if (text != null) {
                    retval = new Dictionary<string, string>();
                    retval["text/plain"] = text;
                } else if (window != null) {
                    retval = window.GetRepresentations();
                } else if (canvas != null) {
                    retval = canvas.GetRepresentations();
                } else if (shape != null) {
                    // find something that this is represented on, and return it
                    //retval = canvas.GetRepresentations();
                } else {
                    retval = new Dictionary<string, string>();
                    retval["text/plain"] = "<drawing event>";
                }
                return retval;
            }
        }
    }
