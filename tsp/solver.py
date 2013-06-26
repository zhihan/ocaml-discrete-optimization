#!/usr/bin/python
# -*- coding: utf-8 -*-
import os
from subprocess import Popen, PIPE


def solveIt(inputData):
    # Modify this code to run your optimization algorithm

    # parse the input
    tmpFileName = 'tmp.data'
    tmpFile = open(tmpFileName, 'w')
    tmpFile.write(inputData)
    tmpFile.close()

    process = Popen(['./greedy_nn.native', './tmp.data'], 
                    stdout = PIPE)
    (stdout, stderr) = process.communicate()
    return stdout.strip()
 


import sys

if __name__ == '__main__':
    if len(sys.argv) > 1:
        fileLocation = sys.argv[1].strip()
        inputDataFile = open(fileLocation, 'r')
        inputData = ''.join(inputDataFile.readlines())
        inputDataFile.close()
        print solveIt(inputData)
    else:
        print 'This test requires an input file.  Please select one from the data directory. (i.e. python solver.py ./data/tsp_51_1)'

