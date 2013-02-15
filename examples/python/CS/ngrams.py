## Show what word follows what word in a text
## Doug Blank

from Myro import *
import Graphics
import string

def process(filename):
    global g
    fp = open(filename)
    prev = None
    ngram = {}
    for line in fp:
        #print("Line:", line)
        remove = ".-?:/=(),[]#><{}\"'\\"
        line = line.translate(string.maketrans(remove, " " * len(remove)))
        line = line.translate(None, "0123456789")
        line = line.lower()
        for word in line.split():
            if prev in ngram:
                if word not in ngram[prev]:
                    ngram[prev].append(word)
            elif prev != None:
                ngram[prev] = [word]
            prev = word

    g = Graphics.Graph()
    ##win = Window(500, 500)
    ##win.addScrollbars(5000, 5000)
    counts = list(reversed(list(set([len(ngram[key]) for key in ngram]))))
    print(ngram)
    print(counts)
    print("NGrams read:", len(ngram))
    for key in ngram.keys()[:20]:
        #print(key, ngram[key])
        #if 10 < len(ngram[key]) < 15:
        for word in ngram[key]:
            ##print("edge:", key, word)
            g.addEdge(key, word)
    g.layout()
    ##g.draw(win, {"width": 500, "height": 500})
    g.draw()
    # Resize to something reasonable:
    g.window.Resize(500, 500)


process(calico.relativePath("../examples/data/obamajobs.txt"))
