############################################################
# Module  : Run Milp on all word clusters
#           assumed structure of the data files
# Date    : Oct. 30th
# Author  : Xiao Ling
############################################################

import readData
import score
import milp


############################################################
# Directories
############################################################

pattern_dir = "/Users/lingxiao/Documents/research/code/good-great-excellent/inputs/"
word_dir    = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/words"
total_dir   = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/total-freq.txt"
strong_dir  = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/strong-weak-words"
weak_dir    = "/Users/lingxiao/Documents/research/code/good-great-excellent/out/weak-strong-words"

reader = DataReader(pattern_dir \
	               ,word_dir    \
	               ,total_dir   \
	               ,weak_dir    \
	               ,strong_dir  )

############################################################
# words
############################################################

good_bad =   [ "good"           \
             , "better"         \
             , "best"           \
             , "acceptable"     \
             , "satisfactory"   \
             , "great"          \
             , "solid"          \
             , "superb"]
# good_bad   = ["good", "better", "great"]

############################################################
# counts and scores
############################################################

# good_bad_count, good_bad_weak, good_bad_strong :: Dict String Float
good_bad_count  = reader.count (good_bad)
good_bad_strong = reader.strong(good_bad)
good_bad_weak   = reader.weak  (good_bad)

good_bad_scores = score.scores(reader,good_bad)

rank            = solve(paperMilp(good_bad_scores))







