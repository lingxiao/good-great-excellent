############################################################
# Module  : Read linguistic data
# Date    : Oct. 28th
# Author  : Xiao Ling
############################################################

import os

############################################################
# Project Text File Config
############################################################


# @USE: Given project configurations:
# 			- directory to linguistic patterns
# 			- directory to strong-weak pattern counts
# 			- directory to weak-strong pattern counts
# Output CONFIG mapping name to directories if all directories
# valid. else throw error

# config :: DirectoryPath -> DirectoryPath -> DirectoryPath 
#        -> Dict String DirectoryPath
def config(pattern, weakStrong, strongWeak):

	if os.path.isdir(patternDir) and \
	   os.path.isdir(weakStrong) and \
	   os.path.isdir(strongWeak):

		return { "patterns"    : patternDir \
	      	   , "weak-strong" : weakStrongDir \
	      	   , "strong-weak" : strongWeakDir}
	else:
		raise NameError("invalid directory: ", pattern, weakStrong, strongWeak)

patternDir    = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"
strongWeakDir = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/strong-weak-words"
weakStrongDir = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/weak-strong-words"

ai  = "good"
aj  = "better"
CON = config(patternDir, weakStrongDir, strongWeakDir)

############################################################
# Populate word statistics
############################################################

# @Use: Given adjectives `ai`  and `aj`, and filepath `path`
# output 

# populate :: FilePath -> String -> String -> IO Float
def populate(path,ai,aj):
	xs   = readFile(path, ai + "-" + aj)
	return xs





############################################################
# Read Linguistic Patterns
############################################################

# @Use: given `path` to directory holding strong-weak-pattern
# read pattern and output as list

# toStrongWeak :: CON -> Eff [IO, Error String] [String]
def toStrongWeak(con):
	return readFile(con['patterns'], 'strong-weak-patterns')

# @Use: given `path` to directory holding weak-strong-pattern
# read pattern and output as list

# toWeakStrong :: CON -> Eff [IO, Error String] [String]
def toWeakStrong(con):
	return readFile(con['patterns'], 'weak-strong-patterns')


############################################################
# Utils
############################################################

# @Use: given `path` to directory with file
# named `name` (no file extension), parse file 
# and output as list of lines
# Throw exception if file Not Found

# readFile :: CONFIG
#          -> DirectoryPath 
#          -> Eff [IO, Error String] [String]
def readFile(path,name):
	path = os.path.join(path,name + ".txt")
	if os.path.isfile(path):
		h  = open(path,'r')
		ps = h.read()
		return ps.split("\n")
	else:
		raise NameError("no file found at: ", path)

