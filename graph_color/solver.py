#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
from subprocess import Popen, PIPE


def solveIt(inputData):

    tmpFileName = 'tmp.data'
    tmpFile = open(tmpFileName, 'w')
    tmpFile.write(inputData)
    tmpFile.close()
    
    if len(inputData) < 300000:
        process = Popen(['./main.native', './tmp.data'], 
                        stdout = PIPE)
    else:
        process = Popen(['./greedy.native', './tmp.data'],
                        stdout = PIPE)
    (stdout, stderr) = process.communicate()
    return stdout.strip()
    


import sys

if __name__ == '__main__':
    if len(sys.argv) > 1:
        fileLocation = sys.argv[1].strip()
        print solveIt(fileLocation)
    else:
        print 'This test requires an input file.  Please select one from the data directory. (i.e. python solver.py ./data/ks_4_0)'

