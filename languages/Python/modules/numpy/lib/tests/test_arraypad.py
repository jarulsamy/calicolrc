'''
Tests for the pad functions.
'''

from numpy.testing import TestCase, run_module_suite, assert_array_equal
from numpy.testing import assert_raises, assert_array_almost_equal
import numpy as np
from numpy.lib import pad


class TestStatistic(TestCase):
    def test_check_mean_stat_length(self):
        a = np.arange(100).astype('f')
        a = pad(a, ((25, 20), ), 'mean', stat_length=((2, 3), ))
        b = np.array([
                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                     0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                     0.5, 0.5, 0.5, 0.5, 0.5,

                      0.,  1.,  2.,  3.,  4.,  5.,  6.,  7.,  8.,  9.,
                     10., 11., 12., 13., 14., 15., 16., 17., 18., 19.,
                     20., 21., 22., 23., 24., 25., 26., 27., 28., 29.,
                     30., 31., 32., 33., 34., 35., 36., 37., 38., 39.,
                     40., 41., 42., 43., 44., 45., 46., 47., 48., 49.,
                     50., 51., 52., 53., 54., 55., 56., 57., 58., 59.,
                     60., 61., 62., 63., 64., 65., 66., 67., 68., 69.,
                     70., 71., 72., 73., 74., 75., 76., 77., 78., 79.,
                     80., 81., 82., 83., 84., 85., 86., 87., 88., 89.,
                     90., 91., 92., 93., 94., 95., 96., 97., 98., 99.,

                     98., 98., 98., 98., 98., 98., 98., 98., 98., 98.,
                     98., 98., 98., 98., 98., 98., 98., 98., 98., 98.])
        assert_array_equal(a, b)

    def test_check_maximum_1(self):
        a = np.arange(100)
        a = pad(a, (25, 20), 'maximum')
        b = np.array([
                     99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
                     99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
                     99, 99, 99, 99, 99,

                     0,   1,  2,  3,  4,  5,  6,  7,  8,  9,
                     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                     40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                     50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                     60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                     70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                     80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                     90, 91, 92, 93, 94, 95, 96, 97, 98, 99,

                     99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
                     99, 99, 99, 99, 99, 99, 99, 99, 99, 99])
        assert_array_equal(a, b)

    def test_check_maximum_2(self):
        a = np.arange(100) + 1
        a = pad(a, (25, 20), 'maximum')
        b = np.array([
                     100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
                     100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
                     100, 100, 100, 100, 100,

                       1,   2,   3,   4,   5,   6,   7,   8,   9,  10,
                      11,  12,  13,  14,  15,  16,  17,  18,  19,  20,
                      21,  22,  23,  24,  25,  26,  27,  28,  29,  30,
                      31,  32,  33,  34,  35,  36,  37,  38,  39,  40,
                      41,  42,  43,  44,  45,  46,  47,  48,  49,  50,
                      51,  52,  53,  54,  55,  56,  57,  58,  59,  60,
                      61,  62,  63,  64,  65,  66,  67,  68,  69,  70,
                      71,  72,  73,  74,  75,  76,  77,  78,  79,  80,
                      81,  82,  83,  84,  85,  86,  87,  88,  89,  90,
                      91,  92,  93,  94,  95,  96,  97,  98,  99, 100,

                     100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
                     100, 100, 100, 100, 100, 100, 100, 100, 100, 100])
        assert_array_equal(a, b)

    def test_check_minimum_1(self):
        a = np.arange(100)
        a = pad(a, (25, 20), 'minimum')
        b = np.array([
                       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                       0,  0,  0,  0,  0,

                       0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
                      10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                      20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                      30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                      40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                      50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                      60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                      70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                      80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                      90, 91, 92, 93, 94, 95, 96, 97, 98, 99,

                       0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                       0,  0,  0,  0,  0,  0,  0,  0,  0,  0])
        assert_array_equal(a, b)

    def test_check_minimum_2(self):
        a = np.arange(100) + 2
        a = pad(a, (25, 20), 'minimum')
        b = np.array([
                       2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
                       2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
                       2,  2,  2,  2,  2,

                       2,  3,  4,  5,  6,  7,  8,  9, 10, 11,
                      12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
                      22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                      32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
                      42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
                      52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
                      62, 63, 64, 65, 66, 67, 68, 69, 70, 71,
                      72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
                      82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
                      92, 93, 94, 95, 96, 97, 98, 99, 100, 101,

                       2,  2,  2,  2,  2,  2,  2,  2,  2,  2,
                       2,  2,  2,  2,  2,  2,  2,  2,  2,  2])
        assert_array_equal(a, b)

    def test_check_median(self):
        a = np.arange(100).astype('f')
        a = pad(a, (25, 20), 'median')
        b = np.array([
                   49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5,
                   49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5,
                   49.5, 49.5, 49.5, 49.5, 49.5,

                     0.,   1.,   2.,   3.,   4.,   5.,   6.,   7.,   8.,   9.,
                    10.,  11.,  12.,  13.,  14.,  15.,  16.,  17.,  18.,  19.,
                    20.,  21.,  22.,  23.,  24.,  25.,  26.,  27.,  28.,  29.,
                    30.,  31.,  32.,  33.,  34.,  35.,  36.,  37.,  38.,  39.,
                    40.,  41.,  42.,  43.,  44.,  45.,  46.,  47.,  48.,  49.,
                    50.,  51.,  52.,  53.,  54.,  55.,  56.,  57.,  58.,  59.,
                    60.,  61.,  62.,  63.,  64.,  65.,  66.,  67.,  68.,  69.,
                    70.,  71.,  72.,  73.,  74.,  75.,  76.,  77.,  78.,  79.,
                    80.,  81.,  82.,  83.,  84.,  85.,  86.,  87.,  88.,  89.,
                    90.,  91.,  92.,  93.,  94.,  95.,  96.,  97.,  98.,  99.,

                   49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5,
                   49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5])
        assert_array_equal(a, b)

    def test_check_median_01(self):
        a = np.array([[3, 1, 4], [4, 5, 9], [9, 8, 2]])
        a = pad(a, 1, 'median')
        b = np.array([
                    [4,   4, 5, 4,   4],

                    [3,   3, 1, 4,   3],
                    [5,   4, 5, 9,   5],
                    [8,   9, 8, 2,   8],

                    [4,   4, 5, 4,   4]])
        assert_array_equal(a, b)

    def test_check_median_02(self):
        a = np.array([[3, 1, 4], [4, 5, 9], [9, 8, 2]])
        a = pad(a.T, 1, 'median').T
        b = np.array([
                    [5,   4, 5, 4,   5],

                    [3,   3, 1, 4,   3],
                    [5,   4, 5, 9,   5],
                    [8,   9, 8, 2,   8],

                    [5,   4, 5, 4,   5]])
        assert_array_equal(a, b)

    def test_check_mean_shape_one(self):
        a = [[4, 5, 6]]
        a = pad(a, (5, 7), 'mean', stat_length=2)
        b = np.array([
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],

                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],

                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5],
                   [4, 4, 4, 4, 4,   4, 5, 6,   5, 5, 5, 5, 5, 5, 5]])
        assert_array_equal(a, b)

    def test_check_mean_2(self):
        a = np.arange(100).astype('f')
        a = pad(a, (25, 20), 'mean')
        b = np.array([
                   49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5,
                   49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5,
                   49.5, 49.5, 49.5, 49.5, 49.5,

                     0.,   1.,   2.,   3.,   4.,   5.,   6.,   7.,   8.,   9.,
                    10.,  11.,  12.,  13.,  14.,  15.,  16.,  17.,  18.,  19.,
                    20.,  21.,  22.,  23.,  24.,  25.,  26.,  27.,  28.,  29.,
                    30.,  31.,  32.,  33.,  34.,  35.,  36.,  37.,  38.,  39.,
                    40.,  41.,  42.,  43.,  44.,  45.,  46.,  47.,  48.,  49.,
                    50.,  51.,  52.,  53.,  54.,  55.,  56.,  57.,  58.,  59.,
                    60.,  61.,  62.,  63.,  64.,  65.,  66.,  67.,  68.,  69.,
                    70.,  71.,  72.,  73.,  74.,  75.,  76.,  77.,  78.,  79.,
                    80.,  81.,  82.,  83.,  84.,  85.,  86.,  87.,  88.,  89.,
                    90.,  91.,  92.,  93.,  94.,  95.,  96.,  97.,  98.,  99.,

                   49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5,
                   49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5, 49.5])
        assert_array_equal(a, b)


class TestConstant(TestCase):
    def test_check_constant(self):
        a = np.arange(100)
        a = pad(a, (25, 20), 'constant', constant_values=(10, 20))
        b = np.array([10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
                     10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
                     10, 10, 10, 10, 10,

                      0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
                     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                     40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                     50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                     60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                     70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                     80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                     90, 91, 92, 93, 94, 95, 96, 97, 98, 99,

                     20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
                     20, 20, 20, 20, 20, 20, 20, 20, 20, 20])
        assert_array_equal(a, b)


class TestLinearRamp(TestCase):
    def test_check_simple(self):
        a = np.arange(100).astype('f')
        a = pad(a, (25, 20), 'linear_ramp', end_values=(4, 5))
        b = np.array([
                    4.00, 3.84, 3.68, 3.52, 3.36, 3.20, 3.04, 2.88, 2.72, 2.56,
                    2.40, 2.24, 2.08, 1.92, 1.76, 1.60, 1.44, 1.28, 1.12, 0.96,
                    0.80, 0.64, 0.48, 0.32, 0.16,

                    0.00, 1.00, 2.00, 3.00, 4.00, 5.00, 6.00, 7.00, 8.00, 9.00,
                    10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0,
                    20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0, 29.0,
                    30.0, 31.0, 32.0, 33.0, 34.0, 35.0, 36.0, 37.0, 38.0, 39.0,
                    40.0, 41.0, 42.0, 43.0, 44.0, 45.0, 46.0, 47.0, 48.0, 49.0,
                    50.0, 51.0, 52.0, 53.0, 54.0, 55.0, 56.0, 57.0, 58.0, 59.0,
                    60.0, 61.0, 62.0, 63.0, 64.0, 65.0, 66.0, 67.0, 68.0, 69.0,
                    70.0, 71.0, 72.0, 73.0, 74.0, 75.0, 76.0, 77.0, 78.0, 79.0,
                    80.0, 81.0, 82.0, 83.0, 84.0, 85.0, 86.0, 87.0, 88.0, 89.0,
                    90.0, 91.0, 92.0, 93.0, 94.0, 95.0, 96.0, 97.0, 98.0, 99.0,

                    94.3, 89.6, 84.9, 80.2, 75.5, 70.8, 66.1, 61.4, 56.7, 52.0,
                    47.3, 42.6, 37.9, 33.2, 28.5, 23.8, 19.1, 14.4,  9.7,  5.])
        assert_array_almost_equal(a, b, decimal=5)


class TestReflect(TestCase):
    def test_check_simple(self):
        a = np.arange(100)
        a = pad(a, (25, 20), 'reflect')
        b = np.array([
                     25, 24, 23, 22, 21, 20, 19, 18, 17, 16,
                     15, 14, 13, 12, 11, 10,  9,  8,  7,  6,
                      5,  4,  3,  2,  1,

                      0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
                     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                     40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                     50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                     60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                     70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                     80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                     90, 91, 92, 93, 94, 95, 96, 97, 98, 99,

                     98, 97, 96, 95, 94, 93, 92, 91, 90, 89,
                     88, 87, 86, 85, 84, 83, 82, 81, 80, 79])
        assert_array_equal(a, b)

    def test_check_large_pad(self):
        a = [[4, 5, 6], [6, 7, 8]]
        a = pad(a, (5, 7), 'reflect')
        b = np.array([
               [7, 6, 7, 8, 7,   6, 7, 8,   7, 6, 7, 8, 7, 6, 7],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [7, 6, 7, 8, 7,   6, 7, 8,   7, 6, 7, 8, 7, 6, 7],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [7, 6, 7, 8, 7,   6, 7, 8,   7, 6, 7, 8, 7, 6, 7],

               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [7, 6, 7, 8, 7,   6, 7, 8,   7, 6, 7, 8, 7, 6, 7],

               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [7, 6, 7, 8, 7,   6, 7, 8,   7, 6, 7, 8, 7, 6, 7],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [7, 6, 7, 8, 7,   6, 7, 8,   7, 6, 7, 8, 7, 6, 7],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [7, 6, 7, 8, 7,   6, 7, 8,   7, 6, 7, 8, 7, 6, 7],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5]])
        assert_array_equal(a, b)

    def test_check_shape(self):
        a = [[4, 5, 6]]
        a = pad(a, (5, 7), 'reflect')
        b = np.array([
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],

               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],

               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5],
               [5, 4, 5, 6, 5,   4, 5, 6,   5, 4, 5, 6, 5, 4, 5]])
        assert_array_equal(a, b)

    def test_check_01(self):
        a = pad([1, 2, 3], 2, 'reflect')
        b = np.array([3, 2, 1, 2, 3, 2, 1])
        assert_array_equal(a, b)

    def test_check_02(self):
        a = pad([1, 2, 3], 3, 'reflect')
        b = np.array([2, 3, 2, 1, 2, 3, 2, 1, 2])
        assert_array_equal(a, b)

    def test_check_03(self):
        a = pad([1, 2, 3], 4, 'reflect')
        b = np.array([1, 2, 3, 2, 1, 2, 3, 2, 1, 2, 3])
        assert_array_equal(a, b)


class TestWrap(TestCase):
    def test_check_simple(self):
        a = np.arange(100)
        a = pad(a, (25, 20), 'wrap')
        b = np.array([
                     75, 76, 77, 78, 79, 80, 81, 82, 83, 84,
                     85, 86, 87, 88, 89, 90, 91, 92, 93, 94,
                     95, 96, 97, 98, 99,

                      0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
                     10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
                     20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                     30, 31, 32, 33, 34, 35, 36, 37, 38, 39,
                     40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
                     50, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                     60, 61, 62, 63, 64, 65, 66, 67, 68, 69,
                     70, 71, 72, 73, 74, 75, 76, 77, 78, 79,
                     80, 81, 82, 83, 84, 85, 86, 87, 88, 89,
                     90, 91, 92, 93, 94, 95, 96, 97, 98, 99,

                      0,  1,  2,  3,  4,  5,  6,  7,  8,  9,
                     10, 11, 12, 13, 14, 15, 16, 17, 18, 19])
        assert_array_equal(a, b)

    def test_check_large_pad(self):
        a = np.arange(12)
        a = np.reshape(a, (3, 4))
        a = pad(a, (10, 12), 'wrap')
        b = np.array([
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11],
       [2,  3,  0,  1,  2,  3,  0,  1,  2,  3,    0,  1,  2,  3,    0,  1,  2,
         3,  0,  1,  2,  3,  0,  1,  2,  3],
       [6,  7,  4,  5,  6,  7,  4,  5,  6,  7,    4,  5,  6,  7,    4,  5,  6,
         7,  4,  5,  6,  7,  4,  5,  6,  7],
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11],
       [2,  3,  0,  1,  2,  3,  0,  1,  2,  3,    0,  1,  2,  3,    0,  1,  2,
         3,  0,  1,  2,  3,  0,  1,  2,  3],
       [6,  7,  4,  5,  6,  7,  4,  5,  6,  7,    4,  5,  6,  7,    4,  5,  6,
         7,  4,  5,  6,  7,  4,  5,  6,  7],
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11],
       [2,  3,  0,  1,  2,  3,  0,  1,  2,  3,    0,  1,  2,  3,    0,  1,  2,
         3,  0,  1,  2,  3,  0,  1,  2,  3],
       [6,  7,  4,  5,  6,  7,  4,  5,  6,  7,    4,  5,  6,  7,    4,  5,  6,
         7,  4,  5,  6,  7,  4,  5,  6,  7],
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11],

       [2,  3,  0,  1,  2,  3,  0,  1,  2,  3,    0,  1,  2,  3,    0,  1,  2,
         3,  0,  1,  2,  3,  0,  1,  2,  3],
       [6,  7,  4,  5,  6,  7,  4,  5,  6,  7,    4,  5,  6,  7,    4,  5,  6,
         7,  4,  5,  6,  7,  4,  5,  6,  7],
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11],

       [2,  3,  0,  1,  2,  3,  0,  1,  2,  3,    0,  1,  2,  3,    0,  1,  2,
         3,  0,  1,  2,  3,  0,  1,  2,  3],
       [6,  7,  4,  5,  6,  7,  4,  5,  6,  7,    4,  5,  6,  7,    4,  5,  6,
         7,  4,  5,  6,  7,  4,  5,  6,  7],
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11],
       [2,  3,  0,  1,  2,  3,  0,  1,  2,  3,    0,  1,  2,  3,    0,  1,  2,
         3,  0,  1,  2,  3,  0,  1,  2,  3],
       [6,  7,  4,  5,  6,  7,  4,  5,  6,  7,    4,  5,  6,  7,    4,  5,  6,
         7,  4,  5,  6,  7,  4,  5,  6,  7],
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11],
       [2,  3,  0,  1,  2,  3,  0,  1,  2,  3,    0,  1,  2,  3,    0,  1,  2,
         3,  0,  1,  2,  3,  0,  1,  2,  3],
       [6,  7,  4,  5,  6,  7,  4,  5,  6,  7,    4,  5,  6,  7,    4,  5,  6,
         7,  4,  5,  6,  7,  4,  5,  6,  7],
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11],
       [2,  3,  0,  1,  2,  3,  0,  1,  2,  3,    0,  1,  2,  3,    0,  1,  2,
         3,  0,  1,  2,  3,  0,  1,  2,  3],
       [6,  7,  4,  5,  6,  7,  4,  5,  6,  7,    4,  5,  6,  7,    4,  5,  6,
         7,  4,  5,  6,  7,  4,  5,  6,  7],
       [10, 11,  8,  9, 10, 11,  8,  9, 10, 11,    8,  9, 10, 11,    8,  9, 10,
        11,  8,  9, 10, 11,  8,  9, 10, 11]])
        assert_array_equal(a, b)

    def test_check_01(self):
        a = pad([1, 2, 3], 3, 'wrap')
        b = np.array([1, 2, 3, 1, 2, 3, 1, 2, 3])
        assert_array_equal(a, b)

    def test_check_02(self):
        a = pad([1, 2, 3], 4, 'wrap')
        b = np.array([3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1])
        assert_array_equal(a, b)


class TestStatLen(TestCase):
    def test_check_simple(self):
        a = np.arange(30)
        a = np.reshape(a, (6, 5))
        a = pad(a, ((2, 3), (3, 2)), mode='mean', stat_length=(3,))
        b = np.array([[6,  6,  6,     5,  6,  7,  8,  9,     8,  8],
                     [6,  6,  6,     5,  6,  7,  8,  9,     8,  8],

                     [1,  1,  1,     0,  1,  2,  3,  4,     3,  3],
                     [6,  6,  6,     5,  6,  7,  8,  9,     8,  8],
                     [11, 11, 11,    10, 11, 12, 13, 14,    13, 13],
                     [16, 16, 16,    15, 16, 17, 18, 19,    18, 18],
                     [21, 21, 21,    20, 21, 22, 23, 24,    23, 23],
                     [26, 26, 26,    25, 26, 27, 28, 29,    28, 28],

                     [21, 21, 21,    20, 21, 22, 23, 24,    23, 23],
                     [21, 21, 21,    20, 21, 22, 23, 24,    23, 23],
                     [21, 21, 21,    20, 21, 22, 23, 24,    23, 23]])
        assert_array_equal(a, b)


class TestEdge(TestCase):
    def test_check_simple(self):
        a = np.arange(12)
        a = np.reshape(a, (4, 3))
        a = pad(a, ((2, 3), (3, 2)), 'edge' )
        b = np.array([
                     [0,  0,  0,    0,  1,  2,    2,  2],
                     [0,  0,  0,    0,  1,  2,    2,  2],

                     [0,  0,  0,    0,  1,  2,    2,  2],
                     [3,  3,  3,    3,  4,  5,    5,  5],
                     [6,  6,  6,    6,  7,  8,    8,  8],
                     [9,  9,  9,    9, 10, 11,   11, 11],

                     [9,  9,  9,    9, 10, 11,   11, 11],
                     [9,  9,  9,    9, 10, 11,   11, 11],
                     [9,  9,  9,    9, 10, 11,   11, 11]])
        assert_array_equal(a, b)


class ValueError1(TestCase):
    def test_check_simple(self):
        arr = np.arange(30)
        arr = np.reshape(arr, (6, 5))
        kwargs = dict(mode='mean', stat_length=(3, ))
        assert_raises(ValueError, pad, arr, ((2, 3), (3, 2), (4, 5)),
                **kwargs)

    def test_check_negative_stat_length(self):
        arr = np.arange(30)
        arr = np.reshape(arr, (6, 5))
        kwargs = dict(mode='mean', stat_length=(-3, ))
        assert_raises(ValueError, pad, arr, ((2, 3), (3, 2)),
                **kwargs)

    def test_check_negative_pad_width(self):
        arr = np.arange(30)
        arr = np.reshape(arr, (6, 5))
        kwargs = dict(mode='mean', stat_length=(3, ))
        assert_raises(ValueError, pad, arr, ((-2, 3), (3, 2)),
                **kwargs)


class ValueError2(TestCase):
    def test_check_simple(self):
        arr = np.arange(30)
        arr = np.reshape(arr, (6, 5))
        kwargs = dict(mode='mean', stat_length=(3, ))
        assert_raises(ValueError, pad, arr, ((2, 3, 4), (3, 2)),
                **kwargs)


class ValueError3(TestCase):
    def test_check_simple(self):
        arr = np.arange(30)
        arr = np.reshape(arr, (6, 5))
        kwargs = dict(mode='mean', stat_length=(3, ))
        assert_raises(ValueError, pad, arr, ((-2, 3), (3, 2)),
                **kwargs)


if __name__ == "__main__":
    run_module_suite()
