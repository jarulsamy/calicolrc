from Tao.Sdl import Sdl

spec = Sdl.SDL_AudioSpec()
MemoryStream stream = create_tone()
IntPtr pSpec = Marshal.AllocHGlobal(Marshal.SizeOf(stream.Spec))
Marshal.StructureToPtr(spec, pSpec, false)
Sdl.SDL_OpenAudio(pSpec, IntPtr.Zero)
spec = (Sdl.SDL_AudioSpec)Marshal.PtrToStructure(pSpec, typeof(Sdl.SDL_AudioSpec))
if (((ushort)stream.Spec.format & 0x8000) == 0x8000)    // signed
{
    stream.Offset = 0;
}
else
{
    stream.Offset = 2 << ((byte)stream.Spec.format - 2);
}
Mixer.AudioOpen = true;


MemoryStream create_tone(int len) {
  byte [] array = new byte[len];
  double angle = 0.0;
  for (int i=0; i<len; i++) {
    array[i] = 255*cos(angle);
    angle += 3.14159/100;
    if (angle > 2.0*3.14159) {
      angle -= 2.0*3.14159;
    }
  }
  return new MemoryStream(array);
}

"""
using System;
using System.Collections.Generic;
using System.Drawing;
using System.Threading;
using SdlDotNet.Graphics;
using SdlDotNet.Input;
using SdlDotNet.Core;
using SdlDotNet.Audio;

using Font = SdlDotNet.Graphics.Font;

public class AudioTest
{
    private static Surface m_VideoScreen;
    private static Surface m_FileNameSurface;
    private static Font m_Font = new Font( "Arial.ttf", 60 );
    private static bool m_Terminate = false;

    public static void Main( string[] args )
    {
        m_VideoScreen = Video.SetVideoMode( 800, 600, 32, false, false, false, true, true );

        Events.Quit += new EventHandler<QuitEventArgs>( ApplicationQuitEventHandler );
        Events.Tick += new EventHandler<TickEventArgs>( ApplicationTickEventHandler );

        Events.UserEvent += new EventHandler<UserEventArgs>( ApplicationUserEventHandler );

        Thread audioThread = new Thread( new ThreadStart( AudioPlaybackThread ));
        audioThread.Start();

        Events.Run();
    }

    private static void AudioPlaybackThread()
    {
        // Play background Music
        Music bgMusic = new Music( "back.ogg" );
        MusicPlayer.Volume = 30;
        MusicPlayer.Load( bgMusic );
        MusicPlayer.Play();

        // Start playing sound Effects
        List<string> audioFiles = new List<string>( new string[] { "three.ogg", "two.ogg", "one.ogg", "fight.ogg" } );
        int cnt = 0;
        while( ! m_Terminate )
        {
            UserEventArgs userEvent = new UserEventArgs( audioFiles[cnt++] );
            if( cnt >= audioFiles.Count )
                cnt = 0;

            Events.PushUserEvent( userEvent );

            SdlDotNet.Core.Timer.DelaySeconds( 2 );
        }
    }

    private static void ApplicationUserEventHandler( object sender, UserEventArgs args )
    {
        // Play Ogg File
        if(( args.UserEvent as string ).EndsWith( "ogg" ))
        {
            Sound snd = new Sound( args.UserEvent as string );
            m_FileNameSurface = m_Font.Render(( args.UserEvent as string ).Replace( ".ogg", String.Empty ), Color.White );
            snd.Play();
        }
    }

    private static void ApplicationTickEventHandler( object sender, TickEventArgs args )
    {
        m_VideoScreen.Fill( Color.Black );
        if( m_FileNameSurface != null )
            m_VideoScreen.Blit(  m_FileNameSurface, new Point( m_VideoScreen.Width/2 - m_FileNameSurface.Width/2,
                m_VideoScreen.Height / 2 - m_FileNameSurface.Height / 2 ));
        m_VideoScreen.Update();
    }

    private static void ApplicationQuitEventHandler( object sender, QuitEventArgs args )
    {
        m_Terminate = true;
        Events.QuitApplication();
    }
}
"""