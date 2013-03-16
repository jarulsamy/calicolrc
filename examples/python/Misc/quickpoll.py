from Myro import *

count = askQuestion("How many answers are there?", map(str, [x + 1 for x in range(4)]))
print(count)
count = askQuestion("How many answers are there?", map(str, [x + 1 for x in range(5)]))
print(count)
question = ask("Question:")
print(question)
