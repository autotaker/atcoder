import math

def check(x, A, B):
    theta = math.atan2(x, A)
    l = math.sqrt(x*x + A*A) 

    x_prime = l * math.sin(theta + math.pi / 3)
    y_prime = l * math.cos(theta + math.pi / 3)

    # print(x, A, B, l, theta, x_prime, y_prime)
    if 0 <= x_prime <= B and 0 <= y_prime <= A:
        return True
    else:
        return False

def find_max_triangle_length(A, B):
    left = 0
    right = B
    eps = 1e-10

    while right - left > eps:
        mid = (left + right) / 2
        if check(mid, A, B):
            left = mid
        else:
            right = mid

    x = left
    l = math.sqrt(x*x + A*A)

    return l

def main():
    A, B = map(int, input().split())

    if A > B:
        A, B = B, A

    max_triangle_length = find_max_triangle_length(A, B)
    print(max_triangle_length)

if __name__ == "__main__":
    main()
