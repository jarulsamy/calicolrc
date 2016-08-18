﻿/*
 * Florence - A charting library for .NET
 * 
 * PlotWavelet.cs
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
    public class PlotWavelet : IDemo
    {
        public void Cleanup() { }

        public string[] Description
        {
            get 
            { 
                return new string[] {
                "Wavelet Example. Demonstrates - ",
                "  * Reversing axes, setting number of tick marks on axis explicitly." };
            }
        }

        public void CreatePlot(InteractivePlotSurface2D plotSurface)
        {
            plotSurface.Clear();

            // Create a new line plot from array data via the ArrayAdapter class.
            LinePlot lp = new LinePlot();
            lp.DataSource = makeDaub(256);
            lp.Color = Color.Green;
            lp.Label = "Daubechies Wavelet"; // no legend, but still useful for copy data to clipboard.

            Grid myGrid = new Grid();
            myGrid.VerticalGridType = Grid.GridType.Fine;
            myGrid.HorizontalGridType = Grid.GridType.Coarse;
            plotSurface.Add(myGrid);

            // And add it to the plot surface
            plotSurface.Add(lp);
            plotSurface.Title = "Reversed / Upside down Daubechies Wavelet";

            // Ok, the above will produce a decent default plot, but we would like to change
            // some of the Y Axis details. First, we'd like lots of small ticks (10) between 
            // large tick values. Secondly, we'd like to draw a grid for the Y values. To do 
            // this, we create a new LinearAxis (we could also use Label, Log etc). Rather than
            // starting from scratch, we use the constructor that takes an existing axis and
            // clones it (values in the superclass Axis only are cloned). PlotSurface2D
            // automatically determines a suitable axis when we add plots to it (merging
            // current requirements with old requirements), and we use this as our starting
            // point. Because we didn't specify which Y Axis we are using when we added the 
            // above line plot (there is one on the left - YAxis1 and one on the right - YAxis2)
            // PlotSurface2D.Add assumed we were using YAxis1. So, we create a new axis based on
            // YAxis1, update the details we want, then set the YAxis1 to be our updated one.
            LinearAxis myAxis = new LinearAxis(plotSurface.YAxis1);
            myAxis.NumberOfSmallTicks = 2;
            plotSurface.YAxis1 = myAxis;

            // We would also like to modify the way in which the X Axis is printed. This time,
            // we'll just modify the relevant PlotSurface2D Axis directly. 
            plotSurface.XAxis1.WorldMax = 100.0f;

            plotSurface.PlotBackColor = Color.OldLace;
            plotSurface.XAxis1.Reversed = true;
            plotSurface.YAxis1.Reversed = true;

            // Force a re-draw of the control. 
            plotSurface.Refresh();
        }
        private float[] makeDaub(int len)
        {
            float[] daub4_h = { 0.482962913145f, 0.836516303737f, 0.224143868042f, -0.129409522551f };

            float[] daub4_g = { -0.129409522551f, -0.224143868042f, 0.836516303737f, -0.482962913145f };

            float[] a = new float[len];
            a[8] = 1.0f;
            float[] t;

            int ns = 4;  // number smooth
            while (ns < len / 2)
            {
                t = (float[])a.Clone();

                ns *= 2;

                for (int i = 0; i < (ns * 2); ++i)
                {
                    a[i] = 0.0f;
                }

                // wavelet contribution
                for (int i = 0; i < ns; ++i)
                {
                    for (int j = 0; j < 4; ++j)
                    {
                        a[(2 * i + j) % (2 * ns)] += daub4_g[j] * t[i + ns];
                    }
                }
                // smooth contribution
                for (int i = 0; i < ns; ++i)
                {
                    for (int j = 0; j < 4; ++j)
                    {
                        a[(2 * i + j) % (2 * ns)] += daub4_h[j] * t[i];
                    }
                }
            }
            return a;
        }
    }
}
