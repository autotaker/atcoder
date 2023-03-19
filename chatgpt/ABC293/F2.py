import math

def is_binary_representation(N, b):
    while N > 0:
        if N % b > 1:
            return False
        N //= b
    return True

def count_base_with_binary_digits(N):
    count = 0

    # Iterate through possible values of d
    d = 2
    while True:
        lower_bound = math.ceil((N/2)**(1/(d-1)))
        upper_bound = math.floor(N**(1/(d-1)))
        print (N,d, lower_bound, upper_bound)

        # Check if there is a possible value of b
        if lower_bound <= upper_bound:
            for b in range(lower_bound, upper_bound+1):
                if b > 1 and is_binary_representation(N, b):
                    count += 1
        d += 1
        if lower_bound >= upper_bound:
            break
        

    return count

def main():
    T = int(input())
    for _ in range(T):
        N = int(input())
        print(count_base_with_binary_digits(N))

if __name__ == "__main__":
    main()
