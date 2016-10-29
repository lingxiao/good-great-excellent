############################################################
# Module  : Read linguistic data
# Date    : Oct. 28th
# Author  : Xiao Ling
############################################################

import os

############################################################
# Project Text File Config

patts = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"
p_ws  = os.path.join(patts,"strong-weak-patterns.txt")
p_sw  = os.path.join(patts,"weak-strong-patterns.txt")
path  = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/word-count/good-bad-weak-strong"
ai    = "good"
aj    = "better"

# @Use: Given adjectives `ai`  and `aj`, and filepath `path`
# output 

# popuulate :: String -> String -> FilePath -> IO Float
def populate(ai,aj,path):
	xs   = readFile(path, ai + "-" + aj)
	return xs



############################################################
# Read Linguistic Patterns
############################################################

# @Use: given `path` to directory holding strong-weak-pattern
# read pattern and output as list

# toStrongWeak :: DirectoryPath -> Eff [IO, Error String] [String]
def toStrongWeak(path):
	return readFile(path, 'strong-weak-patterns')

# @Use: given `path` to directory holding weak-strong-pattern
# read pattern and output as list

# toWeakStrong :: DirectoryPath -> Eff [IO, Error String] [String]
def toWeakStrong(path):
	return readFile(path, 'weak-strong-patterns')


############################################################
# Utils
############################################################

# @Use: given `path` to directory with file
# named `name` (no file extension), parse file 
# and output as list of lines
# Throw exception if file Not Found

# readFile :: DirectoryPath -> String -> Eff [IO, Error String] [String]
def readFile(path,name):
	path = os.path.join(path,name + ".txt")
	if os.path.isfile(path):
		h  = open(path,'r')
		ps = h.read()
		return ps.split("\n")
	else:
		raise NameError("no file found at: ", path)

