############################################################
# Module  : Run Milp on all word clusters
#           assumed structure of the data files
# Date    : Oct. 30th
# Author  : Xiao Ling
############################################################

from copy import deepcopy
import readData
import score
import milp
from numpy import matrix


############################################################
# Directories
############################################################

pattern_dir = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"
word_dir    = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/words"
total_dir   = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/total-freq.txt"
strong_dir  = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/strong-weak-words"
weak_dir    = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/weak-strong-words"

############################################################
# word set
############################################################

good = [ "good"           \
       , "acceptable"     \
       , "satisfactory"   \
       , "great"          \
       , "solid"          \
       , "superb"]

wet  = [ "wet"     \
       , "humid"   \
       , "tacky"   \
       , "moist"   \
       , "damp"    \
       , "steamy"  \
       , "wet"     \
       , "drippy"  \
       , "watery"  \
       , "boggy"   \
       , "soggy"   \
       , "rainy"   \
       , "waterlogged"]


bad  = [ "bad"      \
       , "mediocre" \
       , "poor"     \
       , "bad"      \
       , "worse"    \
       , "awful"    \
       , "worst"    \
       , "terrible"]

simple = [ "simple"   \
         , "naive"    \
         , "childlike"]

special = ["characteristic" \
          , "special"       \
          , "peculiar"      \
          , "specific"      \
          , "particular"    \
          , "unique"        ]   

############################################################
# reader and scores
############################################################

reader = DataReader(pattern_dir \
	               ,word_dir    \
	               ,total_dir   \
	               ,weak_dir    \
	               ,strong_dir  )


good_scores   = scores(reader,good          )
# wet_scores    = scores(reader,wet           )
# bad_scores    = scores(reader,bad           )
# simple_scores = scores(reader,simple        )
char_scores   = scores(reader,characteristic)

############################################################
# rank
############################################################

good_rank = PaperMilp(good_scores)


# good_bad_count, good_bad_weak, good_bad_strong :: Dict String Float
# good_bad_count  = reader.count (good_bad)
# good_bad_strong = reader.strong(good_bad)
# good_bad_weak   = reader.weak  (good_bad)

# good_bad_scores = score.scores(reader,good_bad)

# paper = PaperMilp(good_bad_scores)
# paper.solve()







