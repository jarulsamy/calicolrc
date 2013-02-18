from sympy.series.kauers import finite_diff
from sympy.abc import x, y, z, w, n
from sympy import sin, cos
from sympy import pi


def test_finite_diff():
    assert finite_diff(x**2 + 2*x + 1, x) == 2*x + 3
    assert finite_diff(y**3 + 2*y**2 + 3*y +5, y) == 3*y**2 + 7*y + 6
    assert finite_diff(z**2 - 2*z + 3, z) == 2*z - 1
    assert finite_diff(w**2 + 3*w -2, w) == 2*w + 4
    assert finite_diff(sin(x), x,  pi/6) == -sin(x) + sin(x + pi/6)
    assert finite_diff(cos(y),y,  pi/3) == -cos(y) + cos(y + pi/3)
    assert finite_diff(x**2 - 2*x + 3, x,  2) == 4*x
    assert finite_diff(n**2 - 2*n + 3, n, 3) == 6*n + 3
