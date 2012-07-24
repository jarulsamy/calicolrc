from Myro import doTogether, wait

data = [12, 2, 7, 3, 1]
doTogether(*[lambda n=n: wait(n/100) or print(n) for n in data])

