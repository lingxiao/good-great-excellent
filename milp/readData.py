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
#       - directory to linguistic patterns
#       - directory to strong-weak pattern counts
#       - directory to weak-strong pattern counts
# Output CONFIG mapping name to directories if all 
# directories valid. else throw error

# config :: DirectoryPath -> DirectoryPath -> DirectoryPath 
#        -> Eff [IO, Error String] (Dict String DirectoryPath)
def config(pattern, weakStrong, strongWeak):

    if os.path.isdir(patternDir) and \
       os.path.isdir(weakStrong) and \
       os.path.isdir(strongWeak):

        return { "patterns"    : patternDir \
               , "weak-strong" : weakStrongDir \
               , "strong-weak" : strongWeakDir}
    else:
        raise NameError("invalid directory: ", pattern, weakStrong, strongWeak)

############################################################
# Populate word statistics
############################################################

# @Use: Given list of adjectives `ws = [.. ai,..,aj,...]`
#       number of times "R ai aj" appear in corpus
#       where R is some weak-strong relationship lingustic
#       pattern

# weakOver :: CON -> [String] 
#          -> Eff [IO, Error String] (Dict String [Float])
def weakOver(con,ws):
    return counts(con,ws,toWeakStrong,countWeak)

# @Use: Given list of adjectives `ws = [.. ai,..,aj,...]`
#       number of times "R ai aj" appear in corpus
#       where R is some strong-weak relationship lingustic
#       pattern

# strongOver :: CON -> [String] 
#             -> Eff [IO, Error String] (Dict String [Float])
def strongOver(con,ws):
    return counts(con,ws,toStrongWeak,countStrong)

# counts :: CON 
#        -> [String]
#        -> (CON -> [String])
#        -> (CON -> [String] -> String -> Eff [IO, Error String] Float)
#        -> Eff [IO, Error String] (Dict String [Float])
def counts(con,ws,toPattern,count):
    pws      = [(ai,aj) for ai in ws for aj in ws]
    patterns = toPattern(con)
    totals   = dict()
    for (ai,aj) in pws:
        aij = ai + "-" + aj
        totals[ai+'-'+aj] = count(con,patterns,aij)
    return totals

# @Use: Given adjectives `ai`  and `aj`, and filepath `path` output

# countweak :: CON -> [String] -> String 
#           -> Eff [IO, Error String] Float
def countWeak(con,patterns,aij):
    xs  = readFile(con['weak-strong'], aij)
    return [total(p,xs) for p in patterns]
    

# countStrong :: FilePath -> [String] -> String
#           -> Eff [IO, Error String] Float
def countStrong(con,patterns,aij):
    xs  = readFile(con['strong-weak'], aij)
    return [total(p,xs) for p in patterns]

# @USE: find occurences of `pattern` in list of strings `xxs`
#       and output `total: ####` associated with pattern
#       Note if string malformed, we throw error
# total :: String -> [String] -> Error String Float
def total(pattern,xxs):
    k  = xxs.index(pattern)
    xs = xxs[k+2]
    if xs and "total: " in xs:
        ys = xs.replace("total: ", "")
        return float(ys)
    else:
        raise NameError("`total` failed: string malformed")

############################################################
# Read Linguistic Patterns
############################################################

# @Use: given `path` to directory holding strong-weak-pattern
#       read pattern and output as list

# toStrongWeak :: CON -> Eff [IO, Error String] [String]
def toStrongWeak(con):
    return readFile(con['patterns'], 'strong-weak-patterns')

# @Use: given `path` to directory holding weak-strong-pattern
#       read pattern and output as list

# toWeakStrong :: CON -> Eff [IO, Error String] [String]
def toWeakStrong(con):
    return readFile(con['patterns'], 'weak-strong-patterns')


############################################################
# Utils
############################################################

# @Use: given `path` to directory with file
#       named `name` (no file extension), parse file 
#       and output as list of lines
#       Throw exception if file Not Found

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

