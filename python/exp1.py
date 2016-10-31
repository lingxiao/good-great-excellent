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
total_dir   = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/normalization/total-freq.txt"
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

good_scores   = scores(reader,good    )
bad_scores    = scores(reader,bad     )
simple_scores = scores(reader,simple  )
wet_scores    = scores(reader,wet     )
# char_scores   = scores(reader,special )

############################################################
# rank
############################################################

good_milp    = PaperMilp(good_scores  )
bad_milp     = PaperMilp(bad_scores   )
simple_milp  = PaperMilp(simple_scores)
# wet_rank     = PaperMilp(wet_scores   )
# char_rank    = PaperMilp(char_scores  )


good_milp.solve().prettyPrint(good)
bad_milp.solve().prettyPrint(bad)
simple_milp.solve().prettyPrint(simple)







