import time


class Tree:
    def __init__(self, item, depth):
        self.item = item
        self.depth = depth
        if depth > 0:
            self.left = Tree(2 * item - 1, depth - 1)
            self.right = Tree(2 * item, depth - 1)
        else:
            self.left = None
            self.right = None

    def check(self):
        if self.left:
            return self.item + self.left.check() - self.right.check()
        else:
            return self.item


min_depth = 4
max_depth = 14
stretch_depth = max_depth + 1

start = time.time()
print("stretch tree of depth " + repr(stretch_depth))
print("check: " + repr(Tree(0, stretch_depth).check()))

long_lived_tree = Tree(0, max_depth)
iterations = 2 ** max_depth

depth = min_depth
while depth < stretch_depth:
    check = 0
    for i in range(1, iterations + 1):
        check += Tree(i, depth).check() + Tree(-i, depth).check()

    print("num tree: " + repr(iterations * 2))
    print("depth: " + repr(depth))
    print("check: " + repr(check))

    iterations //= 4
    depth += 2

print("long lived tree of depth: " + repr(max_depth))
print("check: " + repr(long_lived_tree.check()))
print("elapsed: " + repr(time.time() - start))