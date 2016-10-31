############################################################
# Module  : Scores functio
# Date    : Oct. 30th
# Author  : Xiao Ling
############################################################


############################################################
# Score Function
############################################################

# @Use    : Given DataReader Object and a list of words `ws`, 
# @Output : score(ai,aj) for each i j in ws, 
#           normalized by min score

# scores :: DataReader -> [String] -> Error String (Dict String Float)
def scores(O,ws):
	pws  = [(u,v) for u in ws for v in ws]
	d    = dict()
	minv = 1e10

	for (u,v) in pws:
		n = toScore(O,u,v)
		if n and abs(n) < minv: minv = abs(n)
		d[u + "_" + v] = n

	for uv in d:
		d[uv] = d[uv]/minv

	return d

# @Use   : Given DataReader Object and words ai aj, output:
# @Output: score(ai,ak) where:
# 
#          score(ai,ak) =   (W1 - S1) - (W2 - S2)
#                         --------------------------
#                           cnt(ai) * cnt(ak)
# 
#          where W1 = 1/P1 * w1 = 1/P1 * sum_{p in P_ws} cnt(p(ai,ak))
#          where W2 = 1/P1 * w2 = 1/P1 * sum_{p in P_ws} cnt(p(ak,ai))
#          where S1 = 1/P2 * s1 = 1/P2 * sum_{p in P_sw} cnt(p(ai,ak))
#          where W2 = 1/P2 * s2 = 1/P2 * sum_{p in P_sw} cnt(p(ak,ai))

# toScore :: DataReader -> String -> String -> Error String Float
def toScore(O,ai,ak):
	ws = [ai,ak]
	d  = O.total()
	P1 = d['weak-strong-normalization']
	P2 = d['strong-weak-normalization']

	return goScores( O.count (ws) \
		           , O.strong(ws) \
		           , O.weak  (ws) \
		           , P1
		           , P2
		           , ai
		           , ak)

# goScores :: Dict String Float
#        -> Dict String Float
#        -> Dict String Float
#        -> Dict String Float
#        -> Float
#        -> Float
#        -> String
#        -> String
#        -> Error String Float
def goScores(count,strong,weak,P1,P2,ai,ak):

	if ai in count and ak in count:
		w1 = sumcnt(weak   , ai , ak)
		w2 = sumcnt(weak   , ak , ai)
		s1 = sumcnt(strong , ai , ak)
		s2 = sumcnt(strong , ak , ai)

		top = (w1/P1 - s1/P2) - (w2/P1 - s2/P2)
		bot = count[ai] * count[ak]

		return (top/bot)

	else:
		raise NameError("words: " + ai + " and " + ak + "not found")


# @Use: Given a `scores` dictonary mapping word pairs to 
#       their weak and strong scores
#       and words `u` `v`, output their total occurences

# sumcnt :: Dict String Float -> String -> String -> Error String Float
def sumcnt(scores, u, v):
	name = u + "-" + v
	if name in scores:
		return sum(scores[name])
	else:
		raise NameError("key " + name + " not found")





