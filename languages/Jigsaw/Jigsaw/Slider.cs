using System;
using Cairo;

namespace Widgets
{
	public delegate void SliderEventHandler(CSlider slider, SliderEventArgs e);

	// -----------------------------------------------------------------------
	public class SliderEventArgs {
		public double value = 0.0;
		public SliderEventArgs(double val) {
			value = val;
		}
	}
	
	// -----------------------------------------------------------------------
	public class CSlider : Diagram.CShape
	{	// A slider widget
		
		internal double slideVal = 1.0; 				// The current slider value [0.0, 1.0]
		private Diagram.CRoundedRectangle button;		// The slider button
		private double bHeight = 40.0;					// Height of button
		private bool sliding = false;					// True if sliding the button
		private double mouseOffset = 0.0;
		
		public event SliderEventHandler ValueChanged;	// Raised when slider value changes
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public CSlider(double x, double y, double w, double h, double val) : base(x, y, w, h)
		{
			slideVal = val;
			button = new Diagram.CRoundedRectangle(x, y+h-bHeight, w, bHeight);
		}
		
		public CSlider(double x, double y) : base(x, y) { }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override Diagram.CShape Clone(double x, double y) 
		{	// Produce a clone of the shape. Most often for dragging.
			CSlider clone = new CSlider(x, y, this.Width, this.Height, slideVal);
			clone.slideVal = this.slideVal;
			return (Diagram.CShape)clone;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseDown (Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
			if (button.ContainsPoint(cvs.mouseDownExact, cvs) == true) {
				this.sliding = true;
				mouseOffset = cvs.mouseDownExact.Y - this.button.Top - 0.5*bHeight;
			} else {
				base.OnMouseDown (cvs, e);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseMove (Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
			if (this.sliding == true) {
				double oldSlideVal = slideVal;
				// Compute the new location of the slider button
				slideVal = (cvs.mouseExact.Y - mouseOffset - (this.Top + 0.5*bHeight))/(this.Height-bHeight);
				if (slideVal < 0.0) slideVal = 0.0;
				if (slideVal > 1.0) slideVal = 1.0;
				if (slideVal != oldSlideVal) RaiseValueChanged(slideVal);
				cvs.Invalidate();
			} else {
				base.OnMouseMove (cvs, e);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		public override void OnMouseUp (Diagram.Canvas cvs, Diagram.MouseEventArgs e)
		{
			if (this.sliding == true ) {
				RaiseValueChanged(slideVal);
				this.sliding = false;
			} else {
				base.OnMouseUp (cvs, e);
			}
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public override void Draw(Cairo.Context g)
        {
			// Draw a gray line down the center of the widget's rectangle
			double x1  = this.Left;
			double w   = this.Width;
			double bh  = this.bHeight;
			double bhh = 0.5 * bh;
			double h   = this.Height;
			double y1  = this.Top + bhh;
			double y2  = y1 + h - bhh;
			double x2  = x1 + w;
			double hw  = 0.5 * w;
			double cx  = x1 + hw;

			double by1 = y1 + (h-bh)*slideVal - bhh;
			double by2 = by1 + bh;
			double d   = 4;
			
			// Draw background vertical line
			g.MoveTo(cx, y1);
			g.LineTo(cx, y2-bhh);
			g.Color = Diagram.Colors.DarkGray;
			g.LineWidth = 3;
			g.Stroke();
			
			// Move and draw button
			button.Top = by1;
			button.Left = x1;
			button.Width = this.Width;
			button.Draw(g);
			
			// Draw lines on button
			g.MoveTo(x1+d, by1+bhh-d);
			g.LineTo(x2-d, by1+bhh-d);
			g.MoveTo(x1+d, by1+bhh);
			g.LineTo(x2-d, by1+bhh);
			g.MoveTo(x1+d, by1+bhh+d);
			g.LineTo(x2-d, by1+bhh+d);
			g.LineWidth = 1;
			g.Color = Diagram.Colors.Black;
			g.Stroke();;
		}
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        public void RaiseValueChanged(double val)
        {	// Raised when the value of the slider changes
            if (ValueChanged != null)
            {
                try
                {
					SliderEventArgs e = new SliderEventArgs(val);
                    ValueChanged(this, e);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }
        }
		
		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	}

}

