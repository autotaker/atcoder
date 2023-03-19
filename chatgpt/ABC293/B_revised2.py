def main():
    # Input
    N = int(input())
    A = list(map(int, input().split()))

    # Initialize variables
    called_numbers = set()
    called = [False] * N

    # Record called numbers
    for i, a in enumerate(A):
        if not called[i]:
            called_numbers.add(a)
            called[a - 1] = True  # Fix: Set True to the called number's index

    # Find not called numbers
    not_called = [i for i in range(1, N + 1) if i not in called_numbers]

    # Output
    print(len(not_called))
    print(*not_called)

if __name__ == "__main__":
    main()
