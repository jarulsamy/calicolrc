using System;
using System.Collections.Generic;

namespace Dinah
{

	public static class Colors {
		public static readonly Gdk.Color White          = makeColor(1.0,    1.0,    1.0);
		public static readonly Gdk.Color Silver         = makeColor(0.75,   0.75,   0.75);
		public static readonly Gdk.Color Gray           = makeColor(0.5,    0.5,    0.5);
		public static readonly Gdk.Color LightGray      = makeColor(0.8242, 0.8242, 0.8242);
		public static readonly Gdk.Color DarkGray       = makeColor(0.6601, 0.6601, 0.6601);
		public static readonly Gdk.Color SlateGray      = makeColor(0.4375, 0.5,    0.5625);
		public static readonly Gdk.Color DarkSlateGray  = makeColor(0.1562, 0.3086, 0.3086);
		public static readonly Gdk.Color LightSlateGray = makeColor(0.4648, 0.5312, 0.5977);
		public static readonly Gdk.Color WhiteSmoke     = makeColor(0.9570, 0.9570, 0.9570);
		public static readonly Gdk.Color Black          = makeColor(0.0,    0.0,    0.0);
		public static readonly Gdk.Color Yellow         = makeColor(1.0,    1.0,    0.0);
		public static readonly Gdk.Color LightYellow    = makeColor(1.0,    1.0,    0.875);
		public static readonly Gdk.Color DarkGoldenrod  = makeColor(0.7187, 0.5234, 0.0430);
		public static readonly Gdk.Color PaleGoldenrod  = makeColor(0.9297, 0.9062, 0.6641);
		public static readonly Gdk.Color Honeydew       = makeColor(0.9375, 1.0,    0.9375);
		public static readonly Gdk.Color LightBlue      = makeColor(0.6758, 0.8437, 0.8984);
		public static readonly Gdk.Color DarkBlue       = makeColor(0.0,    0.0,    0.5430);
		public static readonly Gdk.Color Red            = makeColor(1.0,    0.0,    0.0);
		public static readonly Gdk.Color DarkRed        = makeColor(0.5430, 0.0,    0.0);
		public static readonly Gdk.Color LightPink      = makeColor(1.0,    0.7109, 0.7539);
		public static readonly Gdk.Color DarkGreen      = makeColor(0.0,    0.3910, 0.0);
		public static readonly Gdk.Color LightGreen     = makeColor(0.5625, 0.9297, 0.5625);
		
		public static Gdk.Color makeColor(double r, double g, double b) {
			return new Gdk.Color((byte)(r * 255), (byte)(g * 255), (byte)(b * 255));
		}
	}
	
	public class TargetTable {
		public static Gtk.TargetEntry [] target_table = new Gtk.TargetEntry [] {
            new Gtk.TargetEntry ("STRING", 0, (uint) TargetType.String ),
	    };		
	}
	
	public enum TargetType {
            String,
    };
	
	public class EmptyExpression : Gtk.Button {
		public EmptyExpression(string text) : base(text) {
			ModifyBg(Gtk.StateType.Normal, Colors.LightPink);
			Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, TargetTable.target_table, Gdk.DragAction.Copy | Gdk.DragAction.Move);
			DragDataReceived += HandleDragDataReceived;
		}

		private static void HandleDragDataReceived (object sender, Gtk.DragDataReceivedArgs args)
	    {
			// sender is what you dropped on
			// args.SelectionData.Text says what to make
	        Console.WriteLine ("Empty Expression Received: {0}", args.SelectionData.Text);
	        Console.WriteLine ("Empty Expression From: {0}", sender);
            //Gtk.Drag.Finish (args.Context, true, false, args.Time);			
		}
	}
}

