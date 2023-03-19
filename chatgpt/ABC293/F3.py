import math

def is_binary_representation(N, b):
    while N > 0:
        if N % b > 1:
            return False
        N //= b
    return True

def count_base_with_binary_digits(N):
    count = 0

    # Calculate the upper limit for d
    d_upper_limit = math.ceil(math.log2(N) + 1)

    count += 1 # d = 2, b = N - 1
    # Iterate through possible values of d
    for d in range(4, d_upper_limit + 1):
        lower_bound = max(2, math.ceil((N/2)**(1/(d-1))))
        upper_bound = math.floor(N**(1/(d-1)))
        print(d, lower_bound, upper_bound)
        # Check if there is a possible value of b
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
