# -*- coding: utf-8 -*-

def subArray(nums):
  i = 0
  if (nums[i]==7 and nums[i+1]==0 and nums[i+2]==4 and
      nums[i+3]==3 and nums[i+4]==2 and nums[i+5]==1):
      return True
  return False

scoreboard = []
scoreboard.extend([3, 7])

#targetRecipe = 704321
targetRecipe = 30000000
maxRecipe = targetRecipe + 10
elf1 = 0
elf2 = 1
recCounter = 0
while len(scoreboard) <= maxRecipe:
  rec1 = scoreboard[elf1]
  rec2 = scoreboard[elf2]
  newRec = rec1 + rec2
  scoreboard.extend([int(d) for d in str(newRec)])
  elf1 = elf1 + rec1 + 1
  elf2 = elf2 + rec2 + 1
  elf1 = elf1 % len(scoreboard)
  elf2 = elf2 % len(scoreboard)
  recCounter = len(scoreboard)

if targetRecipe == 704321:
    print "".join(str(x) for x in scoreboard[targetRecipe:(targetRecipe + 10)])
    
if targetRecipe == 30000000:
    for i in range(len(scoreboard) - 6):
        if subArray(scoreboard[i:(i + 6)]):
            print i
            break
