# based on:
# https://sourceforge.net/apps/mediawiki/cs-sdl/index.php?title=HelloWorld

import SdlDotNet

def handleQuit(sender, args):
    SdlDotNet.Core.Events.QuitApplication()

SdlDotNet.Graphics.Video.SetVideoMode(400, 300)
SdlDotNet.Graphics.Video.WindowCaption = "Hello World!"
SdlDotNet.Core.Events.Quit += handleQuit

SdlDotNet.Core.Events.Run()
