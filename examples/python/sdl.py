"""
This file is part of SnowDemo
* (c) 2005 David Hudson
* Based on code by Sijmen Mulder
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
"""
import sys
sys.path.append("/home/dblank/Desktop/sdldotnet-6.1.1beta/bin")

import System
import SdlDotNet

class Snowflake(SdlDotNet.Graphics.Sprites.Sprite):
    """
    A snowflakes is simply a 5x5 pixel white Surface. 
    It will start out at a random speed and sideways movement.
    """
    def __init__(self):
        super(Snowflake, self).__init__(SdlDotNet.Graphics.Surface(4, 4))
        self.random = System.Random()
        self.speed = 0.0
        self.wind = 0.0
        self.delta = 0.05

        self.Initialize()
        self.Reset()
        self.Y = -1 * self.random.Next(5000 - self.Surface.Height)


    def Initialize(self):
        pass
        #self.Surface.Fill(Color.White)
        #self.Surface.TransparentColor = Color.FromArgb(255, 0, 255)
        #self.Rectangle = Rectangle(self.Surface.Width, 
        #                           self.Surface.Height, 0, 0)

    def Reset(self):
        wind = self.random.Next(3) / 10.0
        self.X = int(self.random.Next(-1 * int(wind * 640), 
                                  640 - self.Surface.Width))
        self.Y = 0 - self.Width
        speed = self.random.Next(50, 150)
        self.Surface.Alpha = ((150 - 50) / (speed - 50) * -255)
        self.Surface.AlphaBlending = True
        
    def Update(self, args):
        change = self.delta * self.speed
        self.Y += int(change)
        self.X += int(System.Math.Ceiling(change * wind))
        if (self.Y > 480):
            self.Reset()
            
