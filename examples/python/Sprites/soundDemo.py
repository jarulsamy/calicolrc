import Common
import Sprites

def main():
    Sprites.init(False)   #automatically hides Sprite on creation, see spritesInitDemo.jig example for details

    Sprites.addBackground("stage.png")
    Sprites.changeBackground("stage")

    Sprites.moveTo(450, 250)
    Sprites.show()

    ''''
    The Sprite moves in a triangle shape starting at
    the bottom right. The Asterisk sound is played
    at the very beginning and as soon as the
    Sprite gets to the bottom left point of
    the triangle using play(filename).  The
    Exclamation sound is played continuously
    as the Sprite moves down the right side of
    the triangle using playFor(filename, seconds).
    Then the Asterisk sound is played as soon as
    the glide function has completed.
    '''
    Sprites.play("Asterisk.wav")
    Sprites.glideBackward(250, 3)
    Sprites.play("Asterisk.wav")
    Sprites.glideTo(325, 100, 3)
    Sprites.playFor("Exclamation.wav", 7)
    Sprites.glideTo(450, 250, 5)
    Sprites.play("Asterisk.wav")



main()