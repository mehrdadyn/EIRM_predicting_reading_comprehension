# -*- coding: utf-8 -*-
"""EDPY 607.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1EmXIOOVWv1ld7S_v96kg9g5MFeCYiS0l
"""

# a = readline("transpose 1.txt", "r")
# print(a)
a = []
with open("transpose 1.txt") as file:
  for i in file:
    b = str(i)
    a.append(b)

print(a)
# print(file.read())

import csv

list = []

with open("transpose reflect_evaluate.tsv", "r", encoding="utf8") as file:
    a = csv.reader(file, delimiter="\t")
    for i in a:
      b = str(i)
      list.append(b)

print(list)

new_list = ['CR424Q03S', 'CR424Q07S', 'CR545Q06S', 'CR545Q07S', 'CR220Q02S', 'CR220Q04S', 'CR540Q03S', 'CR540Q06S', 'CR111Q01S', 'CR546Q04S', 'CR549Q12S', 'CR558Q06S', 'CR558Q10S', 'CR437Q06S', 'CR561Q04S', 'CR561Q08S', 'CR564Q01S', 'CR565Q03S', 'CR565Q09S', 'CR453Q01S', 'CR432Q06S', 'CR543Q04S', 'CR552Q09S', 'CR552Q06S', 'CR556Q03S', 'CR556Q05S', 'CR556Q12S', 'CR544Q10S', 'CR544Q12S', 'CR544Q14S', 'CR227Q01S', 'CR541Q01S', 'CR541Q10S', 'CR551Q06S', 'CR551Q09S', 'CR102Q07S', 'CR567Q10S', 'CR566Q04S', 'CR566Q05S', 'DR067Q05C', 'DR420Q06C', 'DR455Q02C', 'DR111Q02BC', 'DR558Q04C', 'DR558Q12C', 'DR561Q07C', 'DR565Q02C', 'DR565Q05C', 'DR453Q04C', 'DR453Q06C', 'DR553Q06C', 'DR432Q05C', 'DR543Q15C', 'DR552Q11C', 'DR544Q07C', 'DR544Q13C', 'DR541Q04C', 'DR541Q09C', 'DR541Q11C', 'DR551Q11C', 'DR568Q06C', 'DR568Q13C', 'DR570Q10C', 'DR566Q12C']
len(new_list)