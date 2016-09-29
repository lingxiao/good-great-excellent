\* rank: {good,great}, E = {} *\
Maximize
OBJ: __dummy
Subject To
_C1: dist:good_good + 0 good = 0
_C10: dist:good_great >= 0
_C11: dist:great_great >= 0
_C12: dist:great_good >= 0
_C2: dist:good_great - good + great = 0
_C3: dist:great_great + good - great = 0
_C4: dist:great_good + 0 great = 0
_C5: dist:good_good <= 0
_C6: dist:good_great <= 0
_C7: dist:great_great <= 0
_C8: dist:great_good <= 0
_C9: dist:good_good >= 0
Bounds
__dummy = 0
dist:good_good <= 1
dist:good_great <= 1
dist:great_good <= 1
dist:great_great <= 1
good <= 1
great <= 1
End
