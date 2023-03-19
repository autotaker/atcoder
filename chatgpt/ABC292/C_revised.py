from functools import lru_cache
import math

@lru_cache(maxsize=None)
def comb(k):
    count = 0
    for i in range(1, int(math.sqrt(k)) + 1):
        if k % i == 0:
            count += 1
            if i != k // i:
                count += 1
    return count

def main():
    N = int(input())

    total = 0
    for k in range(1, N):
        total += comb(k) * comb(N - k)

    print(total)

if __name__ == '__main__':
    main()
