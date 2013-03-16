from Myro import *
from Graphics import *

def same(c1, c2):
    return (c1.red == c2.red and
            c1.green == c2.green and
            c1.blue == c2.blue)

class Simulator:
    def __init__(self, stack_count=5, width=600, height=600):
        self.stack_count = stack_count
        self.stacks = [[] for s in range(self.stack_count)]
        self.width = width
        self.height = height
        self.border = 0
        self.window = Window("The Claw", self.width, self.height)
        self.window.mode = "manual"
        self.offset = 20
        self.col_width = (self.width - (2 * self.border) - self.offset * self.stack_count)/self.stack_count
        self.position = 0
        left = self.border
        for col in range(self.stack_count):
            platform = Rectangle((left + self.offset, self.height - 50),
                                 (left + self.col_width, self.height))
            platform.fill = Color("brown")
            platform.draw(self.window)
            left += self.col_width + self.offset

        self.claw = Frame((self.border + (self.offset + self.col_width)/2, 0))
        self.claw.draw(self.window)
        self.arm = Rectangle((-10, -self.height), (10, 100))
        self.arm.fill = Color("black")
        self.arm.draw(self.claw)
        self.window.update()
        self.speed = .005
        self.block = None

    def add_block(self, stack, color=None, block=None):
        block_size = self.col_width - 80
        x = stack * (self.col_width + self.offset) +  (self.col_width + self.offset)/2
        y = self.height - len(self.stacks[stack]) * block_size - 50
        if block is None:
            block = Rectangle((x - block_size/2, y - block_size), (x + block_size/2, y))
            block.fill = Color(color)
        else:
            block.moveTo(x, y - block_size/2)
        block.draw(self.window)
        self.stacks[stack].append(block)

    def left(self):
        self.position -= 1
        if self.position < 0:
            speak("Game over!")
            raise Exception("too far left")
        for p in range(0, int(self.col_width + self.offset), 2):
            self.claw.move(-2, 0)
            self.window.step(self.speed)

    def right(self):
        self.position += 1
        if self.position >= self.stack_count:
            speak("Game over!")
            raise Exception("too far right")
        for p in range(0, int(self.col_width + self.offset), 2):
            self.claw.move(2, 0)
            self.window.step(self.speed)

    def down(self):
        stack = self.position
        block_size = self.col_width - 80
        y = self.height - len(self.stacks[stack]) * block_size - 50
        if self.block:
            y -= block_size
        for p in range(0, int(y - 100), 5):
            self.claw.move(0, 5)
            self.window.step(self.speed)
        if self.block: # drop it
            self.block.undraw()
            ## Calico version prior to 2.0.5 need this:
            ##self.claw.shapes.Remove(self.block)
            self.add_block(stack, block=self.block)
            self.update()
            self.block = None
        else: # pick it up, if one
            if len(self.stacks[stack]) > 0:
                block = self.stacks[stack].pop()
                block.undraw()
                block.moveTo(0, 100 + block_size/2)
                block.draw(self.claw)
                self.update()
                self.block = block
        for p in range(0, int(y - 100), 5):
            self.claw.move(0, -5)
            self.window.step(self.speed)

    def check(self, color):
        return self.block and same(self.block.fill, Color(color))

    def update(self):
        self.window.update()


sim = Simulator()
sim.add_block(0, "yellow")
sim.add_block(0, "red")
sim.add_block(1, "blue")
sim.update()

def down():
    sim.down()

def up():
    sim.up()

def left():
    sim.left()

def right():
    sim.right()

def check(color):
    return sim.check(color)

def program_1():
    down()
    program_2()
    if check("blue"):
        right()
    down()
    left()
    left()
    down()
    program_1()

def program_2():
    if check("red"):
        right()
    down()
    left()
    program_3()

def program_3():
    right()
    right()
    left()
    down()

def program_4():
    down()
    right()
    right()
    down()
    left()
    left()
    program_4()

program_4()