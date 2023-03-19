def main():
    N = int(input())
    A = list(map(int, input().split()))

    called = [False] * (N + 1)

    for a in A:
        called[a] = True

    not_called = []
    for i in range(1, N + 1):
        if not called[i]:
            not_called.append(i)

    print(len(not_called))
    print(*not_called)

if __name__ == "__main__":
    main()
