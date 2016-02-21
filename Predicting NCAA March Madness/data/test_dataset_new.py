# -*- coding: utf-8 -*-
"""
Created on Thu Nov 26 14:35:04 2015

@author: Sanchit
"""

#!/usr/bin/env python

#import dataset as ds
import unittest

def skewness(x):
    return 3*(np.mean(x) - np.median(x)) / np.std(x)


def kurtosis(x):
    n = float(len(x))
    xbar = np.mean(x)
    x4 = sum([float(xi - xbar)**4 for xi in x])/n
    return x4/(np.var(x)**2) - 3

class TestDatasetFunctions(unittest.TestCase):
    def test_math(self):
        x = [-0.266729789, 0.884511105, 0.216610259, 0.672462351, 0.783022071, \
             -0.585179766,-0.461727589, 1.294586152, 0.461396560,-0.323491546, \
              0.871289069, 1.367804638, 0.806931778, 0.941363602, 0.007648933, \
             -0.981460136, 0.644835017,-0.979045711, 1.445556615,-2.015880637, \
             -0.160248095,-1.631220571, 0.490733295, 0.729739514, 1.432014827, \
              0.750250396,-0.140150204, 1.391561010, 1.574257893, 0.104479709]
        self.assertTrue(abs(skewness(x) + 0.847844) < 0.0001)
        self.assertTrue(abs(kurtosis(x) + 0.564814) < 0.0001)
    
if __name__ == '__main__':
    unittest.main()
