﻿/*
 * Florence - A charting library for .NET
 * 
 * PlotParticles.cs
 * Copyright (C) 2003-2006 Matt Howlett and others.
 * Copyright (C) 2013 Scott Stephens
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of Florence nor the names of its contributors may
 *    be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
 * OF THE POSSIBILITY OF SUCH DAMAGE.
 */

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

using Florence;

namespace DemoLib.PlotSurface2DDemo
{
    public class PlotParticles : IDemo
    {
        public void Cleanup() { }

        public string[] Description
        {
            get
            {
                return new string[] {
                "Particles Example. Demonstrates - ",
                "  * How to chart multiple data sets against multiple axes at the same time."};
            }
        }

        public void CreatePlot(InteractivePlotSurface2D plotSurface)
        {
            plotSurface.Clear();

            Grid mygrid = new Grid();
            mygrid.HorizontalGridType = Grid.GridType.Fine;
            mygrid.VerticalGridType = Grid.GridType.Fine;
            plotSurface.Add(mygrid);

            // in this example we synthetize a particle distribution
            // in the x-x' phase space and plot it, with the rms Twiss
            // ellipse and desnity distribution
            const int Particle_Number = 500;
            float[] x = new float[Particle_Number];
            float[] y = new float[Particle_Number];
            // Twiss parameters for the beam ellipse
            // 5 mm mrad max emittance, 1 mm beta function
            float alpha, beta, gamma, emit;
            alpha = -2.0f;
            beta = 1.0f;
            gamma = (1.0f + alpha * alpha) / beta;
            emit = 4.0f;

            float da, xmax, xpmax;
            da = -alpha / gamma;
            xmax = (float)Math.Sqrt(emit / gamma);
            xpmax = (float)Math.Sqrt(emit * gamma);

            Random rand = new Random();

            // cheap randomizer on the unit circle
            for (int i = 0; i < Particle_Number; i++)
            {
                float r;
                do
                {
                    x[i] = (float)(2.0f * rand.NextDouble() - 1.0f);
                    y[i] = (float)(2.0f * rand.NextDouble() - 1.0f);
                    r = (float)Math.Sqrt(x[i] * x[i] + y[i] * y[i]);
                } while (r > 1.0f);
            }

            // transform to the tilted twiss ellipse
            for (int i = 0; i < Particle_Number; ++i)
            {
                y[i] *= xpmax;
                x[i] = x[i] * xmax + y[i] * da;
            }
            plotSurface.Title = "Beam Horizontal Phase Space and Twiss ellipse";

            PointPlot pp = new PointPlot();
            pp.OrdinateData = y;
            pp.AbscissaData = x;
            pp.Marker = new Marker(Marker.MarkerType.FilledCircle, 4, new Pen(Color.Blue));
            plotSurface.Add(pp, PlotSurface2D.XAxisPosition.Bottom, PlotSurface2D.YAxisPosition.Left);

            // set axes
            LinearAxis lx = (LinearAxis)plotSurface.XAxis1;
            lx.Label = "Position - x [mm]";
            lx.NumberOfSmallTicks = 2;
            LinearAxis ly = (LinearAxis)plotSurface.YAxis1;
            ly.Label = "Divergence - x' [mrad]";
            ly.NumberOfSmallTicks = 2;

            // Draws the rms Twiss ellipse computed from the random data
            float[] xeli = new float[40];
            float[] yeli = new float[40];

            float a_rms, b_rms, g_rms, e_rms;

            Twiss(x, y, out a_rms, out b_rms, out g_rms, out e_rms);
            TwissEllipse(a_rms, b_rms, g_rms, e_rms, ref xeli, ref yeli);

            LinePlot lp = new LinePlot();
            lp.OrdinateData = yeli;
            lp.AbscissaData = xeli;
            plotSurface.Add(lp, PlotSurface2D.XAxisPosition.Bottom, PlotSurface2D.YAxisPosition.Left);
            lp.Pen = new Pen(Color.Red, 2.0f);
            // Draws the ellipse containing 100% of the particles
            // for a uniform distribution in 2D the area is 4 times the rms
            float[] xeli2 = new float[40];
            float[] yeli2 = new float[40];
            TwissEllipse(a_rms, b_rms, g_rms, 4.0F * e_rms, ref xeli2, ref yeli2);

            LinePlot lp2 = new LinePlot();
            lp2.OrdinateData = yeli2;
            lp2.AbscissaData = xeli2;
            plotSurface.Add(lp2, PlotSurface2D.XAxisPosition.Bottom, PlotSurface2D.YAxisPosition.Left);
            Pen p2 = new Pen(Color.Red, 2.0f);
            float[] pattern = { 5.0f, 40.0f };
            p2.DashPattern = pattern;
            lp2.Pen = p2;

            // now bin the particle position to create beam density histogram
            float range, min, max;
            min = (float)lx.WorldMin;
            max = (float)lx.WorldMax;
            range = max - min;

            const int Nbin = 30;
            float dx = range / Nbin;
            float[] xbin = new float[Nbin + 1];
            float[] xh = new float[Nbin + 1];

            for (int j = 0; j <= Nbin; ++j)
            {
                xbin[j] = min + j * range;
                if (j < Nbin) xh[j] = 0.0F;
            }
            for (int i = 0; i < Particle_Number; ++i)
            {
                if (x[i] >= min && x[i] <= max)
                {
                    int j;
                    j = Convert.ToInt32(Nbin * (x[i] - min) / range);
                    xh[j] += 1;
                }
            }
            StepPlot sp = new StepPlot();
            sp.OrdinateData = xh;
            sp.AbscissaData = new StartStep(min, range / Nbin);
            sp.Center = true;
            plotSurface.Add(sp, PlotSurface2D.XAxisPosition.Bottom, PlotSurface2D.YAxisPosition.Right);
            // axis formatting
            LinearAxis ly2 = (LinearAxis)plotSurface.YAxis2;
            ly2.WorldMin = 0.0f;
            ly2.Label = "Beam Density [a.u.]";
            ly2.NumberOfSmallTicks = 2;
            sp.Pen = new Pen(Color.Green, 2);

            // Finally, refreshes the plot
            plotSurface.Refresh();
        }

        // Fill the array containing the rms twiss ellipse data points
        // ellipse is g*x^2+a*x*y+b*y^2=e
        private void TwissEllipse(float a, float b, float g, float e, ref float[] x, ref float[] y)
        {
            float rot, sr, cr, brot;
            if (a == 0)
            {
                rot = 0;
            }
            else
            {
                rot = (float)(.5 * Math.Atan(2.0 * a / (g - b)));
            }
            sr = (float)Math.Sin(rot);
            cr = (float)Math.Cos(rot);
            brot = g * sr * sr - 2.0F * a * sr * cr + b * cr * cr;
            int npt = x.Length;
            float theta;

            for (int i = 0; i < npt; ++i)
            {
                float xr, yr;
                theta = i * 2.0F * (float)Math.PI / (npt - 1);
                xr = (float)(Math.Sqrt(e * brot) * Math.Cos(theta));
                yr = (float)(Math.Sqrt(e / brot) * Math.Sin(theta));
                x[i] = xr * cr - yr * sr;
                y[i] = xr * sr + yr * cr;
            }
        }
        // Evaluates the rms Twiss parameters from the particle coordinates
        private void Twiss(float[] x, float[] y, out float a, out float b, out float g, out float e)
        {
            float xave, xsqave, yave, ysqave, xyave;
            float sigmaxsq, sigmaysq, sigmaxy;
            int Npoints = x.Length;
            xave = 0;
            yave = 0;
            for (int i = 0; i < Npoints; ++i)
            {
                xave += x[i];
                yave += y[i];
            }
            xave /= Npoints;
            yave /= Npoints;
            xsqave = 0;
            ysqave = 0;
            xyave = 0;
            for (int i = 0; i < Npoints; i++)
            {
                xsqave += x[i] * x[i];
                ysqave += y[i] * y[i];
                xyave += x[i] * y[i];
            }
            xsqave /= Npoints;
            ysqave /= Npoints;
            xyave /= Npoints;
            sigmaxsq = xsqave - xave * xave;
            sigmaysq = ysqave - yave * yave;
            sigmaxy = xyave - xave * yave;
            // Now evaluates rms Twiss parameters
            e = (float)Math.Sqrt(sigmaxsq * sigmaysq - sigmaxy * sigmaxy);
            a = -sigmaxy / e;
            b = sigmaxsq / e;
            g = (1.0F + a * a) / b;
        }
    }
}