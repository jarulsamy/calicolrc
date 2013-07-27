from Processing import *

class Vector:

    def __init__(self, x = 0, y = 0, z = 0):
        self.x = x
        self.y = y
        self.z = z
        print(".",)

    def reset(self):
        self.x = self.y = self.z = 0

    def __neg__(self):
        return Vector(-self.x, -self.y, -self.z)

    def __add__(self, other):
        if isinstance(other, Vector):
            return Vector(other.x + self.x, other.y + self.y, other.z + self.z)
        else:
            raise TypeError

    def __iadd__(self, other):
        if isinstance(other, Vector):
            self.x += other.x
            self.y += other.y
            self.z += other.z
            return self
        else:
            raise TypeError

    def __sub__(self, other):
        return self + -other

    def __rsub__(self, other):
        return -self + other

    def __isub__(self, other):
        if isinstance(other, Vector):
            self.x -= other.x
            self.y -= other.y
            self.z -= other.z
            return self
        else:
            raise TypeError

    def __mul__(self, other):
        if isinstance(other, Vector):
            return Vector(self.x * other.x,
                          self.y * other.y,
                          self.z * other.z)
        elif isinstance(other, float) or isinstance(other, int):
            return Vector(other * self.x, other * self.y, other * self.z)
        else:
            raise TypeError

    __rmul__ = __mul__

    def __imul__(self, other):
        if isinstance(other, Vector):
            self.x *= other.x
            self.y *= other.y
            self.z *= other.z
            return self
        elif isinstance(other, float) or isinstance(other, int):
            self.x *= other
            self.y *= other
            self.z *= other
            return self
        else:
            raise TypeError

    def __truediv__(self, other):
        if isinstance(other, Vector):
            return Vector(self.x / other.x,
                          self.y / other.y,
                          self.z / other.z)
        elif isinstance(other, float) or isinstance(other, int):
            return Vector(self.x/other, self.y/other, self.z/other)
        else:
            raise TypeError

    def __rtruediv__(self, other):
        if isinstance(other, Vector):
            return Vector(other.x / self.x,
                          other.y / self.y,
                          other.z / self.z)
        elif isinstance(other, float) or isinstance(other, int):
            return Vector(other/self.x, other/self.y, other/self.z)
        else:
            raise TypeError

    def __itruediv__(self, other):
        if isinstance(other, Vector):
            self.x /= other.x
            self.y /= other.y
            self.z /= other.z
            return self
        elif isinstance(other, float) or isinstance(other, int):
            self.x /= other
            self.y /= other
            self.z /= other
            return self
        else:
            raise TypeError

    def dist(self, other=None):
        if other == None:
            d = (self.x**2 +
                 self.y**2 +
                 self.z**2)**0.5
            return d
        elif isinstance(other, Vector):
            d = ((other.x-self.x)**2 +
                 (other.y-self.y)**2 +
                 (other.z-self.z)**2)**0.5
            return d
        else:
            return TypeError

    def norm(self):
        return Vector (self.x/self.dist(), self.y/self.dist(), self.z/self.dist())

    def normalize(self):
        d = self.dist()
        if (d > 0):
            self.x = self.x / d
            self.y = self.y / d
            self.z = self.z / d


ps = []
vs = []

num_p = 5
threshold = 100
size = 10
sx = 0
sy = 0

coh_dist = 35
sep_dist = 30
mch_dist = 30

coh_gain = .1
sep_gain = .15
mch_gain = .1

window(600, 600)
frameRate(30)
noStroke()
smooth()

for i in range(num_p):
    x = random(100, 350)
    y = random(100, 350)
    ps.append(Vector(x, y))
    vs.append(Vector(random(-.5, .5), random(-.5, .5)))


def addParticleStart(o, e):
    global sx, sy
    sx = mouseX()
    sy = mouseY()

onMousePressed += addParticleStart

def addParticleEnd(o, e):
    global ps, vs, sx, sy
    ps.append(Vector(mouseX(), mouseY()))
    vs.append(Vector(mouseX()- sx, mouseY()- sy))

onMouseReleased += addParticleEnd

def setGoal(o, e):
    global vs, ps
    for i in range(len(ps)):
        vs[i].x = .01*(mouseX() - ps[i].x)
        vs[i].y = .01*(mouseY() - ps[i].y)

onMouseMoved += setGoal

cohpos = Vector()
seppos = Vector()
mchvel = Vector()

def draw(o, e):
    global ps, vs, cohpos, seppos, mchvel
    #fill(128, 128)
    #rect(0, 0, width()+1, height()+1)
    background(196)
    noStroke()
    fill(24)
    for i in range(len(ps)):
        ellipse(ps[i].x, ps[i].y, size, size);

        cohpos.reset()
        cohn = 0
        seppos.reset()
        sepn = 0
        mchvel.reset()
        mchn = 0
        for j in range(len(ps)):
            if ps[i].dist(ps[j]) < coh_dist and i != j:
                cohpos += ps[j]
                cohn += 1

            if ps[i].dist(ps[j]) < sep_dist and i != j:
                seppos += ps[j]
                sepn += 1

            if ps[i].dist(ps[j]) < mch_dist and i != j:
                mchvel += vs[j]
                mchn += 1

        if cohn > 0: cohpos /= cohn
        if sepn > 0: seppos /= sepn
        if mchn > 0: mchvel /= mchn

        cohpos -= ps[i]
        seppos -= ps[i]
        seppos *= -1
        mchvel -= vs[i]

        cohpos.normalize()
        seppos.normalize()
        mchvel.normalize()

        vs[i].x += coh_gain * cohpos.x +\
                   sep_gain * seppos.x +\
                   mch_gain * mchvel.x
        vs[i].y += coh_gain * cohpos.y +\
                   sep_gain * seppos.y +\
                   mch_gain * mchvel.y

        ps[i] += vs[i]

        if (ps[i].x + size/2 >= width() or ps[i].x - size/2 < 0):
            vs[i].x *= -1
            if (ps[i].x + size/2 >= width()):
                ps[i].x = width()-size/2
            else:
                ps[i].x = size/2

        if (ps[i].y + size/2 >= height() or ps[i].y + size/2 < 0):
            vs[i].y *= -1
            if (ps[i].y + size/2 >= height()):
                ps[i].y = height()-size/2

onLoop += draw
loop()
