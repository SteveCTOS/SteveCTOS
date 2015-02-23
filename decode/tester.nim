import strutils

#echo "fred $2$1$2 $3" % ["dog", "horse", "3"]

type
  Page = object
    data: seq[string]

var
  tags: ref seq[string]
  data: ref seq[Page]
  nopages: int = 0
  notags: int = 0

proc addlist[T](data: ref seq[T], rec: T, index: ref int, delta: int = 32) =
  if index mod delta == 0:
    realloc(data, sizeof(rec) * index+delta)
  data[index] = rec
  inc(index)   
  
var x = "ts"
addlist[string](tags, x, notags)