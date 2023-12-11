import time

start = time.time()
xs = []
x = 117418
mask = (1 << 64) - 1
for i in range(1000000):
    xs.append(x)
    x ^= x << 13 & mask
    x ^= x >> 7 & mask
    x ^= x << 17 & mask
print("check: " + repr(x))

def sort(xs, l, h, lt):
    if h - l <= 1:
        return
    saved_l = l
    saved_h = h
    pivot = xs[l]
    h -= 1
    while l < h:
        x = xs[l]
        if lt(x, pivot):
            l += 1
            continue
        y = xs[h]
        if lt(pivot, y):
            h -= 1
            continue
        if l < h:
            xs[l] = y
            xs[h] = x
    sort(xs, saved_l, h, lt)
    sort(xs, h + 1, saved_h, lt)

sort(xs, 0, i + 1, lambda a, b: a < b)
# xs.sort()

sum1 = sum2 = 0
for x in xs:
    sum1 = (sum1 + x) % (1 << 32)
    sum2 = (sum1 + sum2) % (1 << 32)
sum = sum2 << 32 | sum1
print("checksum: " + repr(sum))
print("elapsed: " + repr(time.time() - start))