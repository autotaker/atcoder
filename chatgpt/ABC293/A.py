def main():
    # 入力
    N = int(input())
    A = list(map(int, input().split()))

    # 各人の番号が呼ばれたかどうかを記録するリスト
    called = [False] * N

    # 人 i の行動
    for i in range(N):
        called[A[i] - 1] = True

    # 最後まで番号が呼ばれなかった人を昇順に列挙
    not_called = [i + 1 for i in range(N) if not called[i]]

    # 出力
    K = len(not_called)
    print(K)
    if K > 0:
        print(" ".join(map(str, not_called)))


if __name__ == "__main__":
    main()
