from math import sqrt
from sys import stdin

def nCr(n, r):
    if n < r:
        return 0
    return n * (n - 1) * (n - 2) // 6

def add(val, cnt, total):
    cnt[val] += 1
    total += nCr(cnt[val], 3) - nCr(cnt[val] - 1, 3)
    return total

def remove(val, cnt, total):
    total -= nCr(cnt[val], 3) - nCr(cnt[val] - 1, 3)
    cnt[val] -= 1
    return total

def mos_algorithm(N, Q, A, queries):
    B = int(sqrt(N))
    sorted_queries = sorted(enumerate(queries), key=lambda x: (x[1][0] // B, x[1][1]))

    L, R = 0, 0
    cnt = [0] * (2 * 10**5 + 1)
    total = 0
    results = [0] * Q

    for idx, (lq, rq) in sorted_queries:
        while L < lq:
            total = remove(A[L], cnt, total)
            L += 1
        while L > lq:
            L -= 1
            total = add(A[L], cnt, total)
        while R <= rq:
            total = add(A[R], cnt, total)
            R += 1
        while R > rq + 1:
            R -= 1
            total = remove(A[R], cnt, total)

        results[idx] = total

    return results

# Input
N, Q = map(int, input().split())
A = list(map(int, input().split()))
queries = [tuple(map(lambda x: int(x)-1, input().split())) for _ in range(Q)]

# Solve problem
results = mos_algorithm(N, Q, A, queries)

# Output
for result in results:
    print(result)
