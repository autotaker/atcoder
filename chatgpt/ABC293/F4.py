import math

def is_binary_representation(N, b):
    while N > 0:
        if N % b > 1:
            return False
        N //= b
    return True

def binary_search(N, a):
    left, right = 2, N
    while left <= right:
        mid = (left + right) // 2
        f_x = 0
        p_b = 1
        a1 = a
        while a1 > 0:
            f_x += (a1 & 1) * p_b
            p_b *= mid
            a1 //= 2
        if f_x == N:
            return True
        elif f_x < N:
            left = mid + 1
        else:
            right = mid - 1
    return False

def count_base_with_binary_digits(N):
    count = 0
    d_upper_limit = math.ceil(math.log2(N) + 1)

    for a in range(1024):
        if binary_search(N, a):
            count += 1
    for d in range(11, d_upper_limit + 1):
        lower_bound = math.ceil((N/2)**(1/(d-1)))
        upper_bound = math.floor(N**(1/(d-1)))

        for b in range(lower_bound, upper_bound+1):
            if is_binary_representation(N, b):
                count += 1
    return count

def main():
    T = int(input())
    for _ in range(T):
        N = int(input())
        print(count_base_with_binary_digits(N))

if __name__ == "__main__":
    main()
