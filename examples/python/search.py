# Search

from random import random
from Graphics import Graph, Window, Color

class Tree:
    def __init__(self, left, item, right):
        self.item = item
        self.left = left
        self.right = right

    def expand(self):
        retval = []
        if self.left:
            retval.append(self.left)
        if self.right:
            retval.append(self.right)
        return retval

MAX = 1000

def makeRandomTree(n):
    if n == 0: return None
    left = makeRandomTree(n - 1)
    right = makeRandomTree(n - 1)
    return Tree(left, int(random() * MAX), right)

def treeToList(tree):
    retval = []
    if tree.left == None or tree.right == None:
        return tree.item
    retval.append(treeToList(tree.left))
    retval.append(tree.item)
    retval.append(treeToList(tree.right))
    return retval

class Queue:
    def __init__(self, start=None):
        self.list = [] if start == None else start

    def append(self, *items):
        for item in items:
            self.list.append(item)

    def next(self):
        return self.list.pop()

    def is_empty(self):
        return len(self.list) == 0

def search(items, match):
    while not items.is_empty():
        node = items.next()
        shape = g.getNode(str(node.item))
        shape.fill = Color("pink")
        if node.item == match:
            return True
        items.append(*node.expand())
    return False

def reset_color(tree):
    node = g.getNode(str(tree.item))
    node.fill = Color("white")
    if tree.left:
        reset_color(tree.left)
    if tree.right:
        reset_color(tree.right)

tree = makeRandomTree(5)
g = Graph()
g.layout(treeToList(tree))
win = Window(1000, 300)
g.draw(win, {"label": "Random Graph", "default_shape": "box", "line_type": "line"})
print(search(Queue([tree]), 302))
