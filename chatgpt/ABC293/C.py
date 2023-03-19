def dfs(H, W, A, i, j, visited):
    if i < 0 or i >= H or j < 0 or j >= W:
        return 0
    if (i, j) in visited:
        return 0
    if i == H - 1 and j == W - 1:
        return 1

    visited.add((i, j))
    count = dfs(H, W, A, i + 1, j, visited) + dfs(H, W, A, i, j + 1, visited)
    visited.remove((i, j))

    return count

def main():
    H, W = map(int, input().split())
    A = [list(map(int, input().split())) for _ in range(H)]

    visited = set()
    print(dfs(H, W, A, 0, 0, visited))

if __name__ == "__main__":
    main()
