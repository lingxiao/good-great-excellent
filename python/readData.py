############################################################
# Module  : Read data
#           Note code here is #tightly# coupled with
#           assumed structure of the data files
# Date    : Oct. 29th
# Author  : Xiao Ling
############################################################

import os

path = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/total-freq.txt"

############################################################
# Class 
############################################################

class DataReader(object):

    def __init__(self, pattern_dir, word_dir, total_dir, weak_dir, strong_dir):
        self.Config = config(pattern_dir \
                            ,word_dir    \
                            ,total_dir   \
                            ,weak_dir    \
                            ,strong_dir  )

    def total(self):
        return totalNorm(self.Config['total'])

    def count(self, words):
        return wordCount(self.Config,words)

    def weak(self,words):
        return weakOver(self.Config,words)

    def strong(self,words):
        return strongOver(self.Config,words)

############################################################
# Project Text File Config
############################################################

# @USE: Given project configurations:
#       - directory to linguistic patterns
#       - directory to words
#       - directory to strong-weak pattern counts
#       - directory to weak-strong pattern counts
# Output CONFIG mapping name to directories if all 
# directories valid. else throw error

# config :: DirectoryPath 
#        -> DirectoryPath
#        -> DirectoryPath
#        -> DirectoryPath
#        -> DirectoryPath
#        -> Eff [IO, Error String] (Dict String DirectoryPath)
def config(pattern, words, total, weak, strong):

    if  os.path.isdir  (pattern ) \
    and os.path.isdir  (words   ) \
    and os.path.exists (total   ) \
    and os.path.isdir  (weak    ) \
    and os.path.isdir  (strong  ):
        return { "patterns"    : pattern  \
               , "words"       : words    \
               , "total"       : total
               , "weak-strong" : weak     \
               , "strong-weak" : strong   }
    else:
        raise NameError( "invalid directory: " \
                       , pattern    \
                       , words      \
                       , total      \
                       , weakStrong \
                       , strongWeak )

############################################################
# Populate Pattern Statistics
############################################################

# @Input:  given path to file with cumulative counts of all patterns
#          over the corpus over all words
# @Output: dictonary mapping weak-strong and strong-weak pattern
#          to their respective counts

# totalNorm :: FileDirectory -> Error String (Dict String Float)
def totalNorm(path):
    if os.path.exists(path):

        h     = open(path,'r')
        xxs   = h.read()
        xxs   = xxs.split("\n")
        weak  = parsePatternTotal("weak-total"  ,xxs)
        strng = parsePatternTotal("strong-total",xxs)
        return { "weak-strong-normalization": weak \
               , "strong-weak-normalization": strng}
    else:
        raise NameError("Path not valid: " + path)


# parsePatternTotal :: String -> [String] 
#                   -> Error String (Dict String Float)
def parsePatternTotal(key,xxs):

    total = [xs for xs in xxs if key in xs]

    if total:
        total = total[0].split("\t")
        if len(total) >= 3:
            return float(total[2])
        else:
            raise NameError("malformed text")
    else:
        raise NameError("malformed text")



############################################################
# Populate word statistics
############################################################

# @Use: Given list of words [..ai,..]
#       output occurences of `ai` for each i

# wordCount :: CON 
#           -> [String] 
#           -> Eff [IO, Error String] (Dict String Float)
def wordCount(CON,words):
    out = dict()
    for word in words:
        out[word] = go(CON,word)
    return out

def go(CON,word):
    xxs  = readFile(CON['words'],word)
    nxs  = [x for x in xxs if  "total" in x]
    if nxs:
        n    = float(nxs[0].replace("total: ", ""))
        return n
    else:
        raise NameError("malformed string: ", xxs)

# @Use: Given list of adjectives `ws = [.. ai,..,aj,...]`
#       output number of times "R ai aj" appear in corpus
#       for every i and j
#       where R is some weak-strong linguistic relationship 

# weakOver :: CON -> [String] 
#          -> Eff [IO, Error String] (Dict String [Float])
def weakOver(con,ws):
    return counts(con,ws,toWeakStrong,countWeak)

# @Use: Given list of adjectives `ws = [.. ai,..,aj,...]`
#       output number of times "R ai aj" appear in corpus
#       for every i and j
#       where R is some weak-strong linguistic relationship 

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
    return [parseTotal(p,xs) for p in patterns]
    

# countStrong :: FilePath -> [String] -> String
#           -> Eff [IO, Error String] Float
def countStrong(con,patterns,aij):
    xs  = readFile(con['strong-weak'], aij)
    return [parseTotal(p,xs) for p in patterns]

# @USE: find occurences of `pattern` in list of strings `xxs`
#       and output `total: ####` associated with pattern
#       Note if string malformed, we throw error
# parseTotal :: String -> [String] -> Error String Float
def parseTotal(pattern,xxs):
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




