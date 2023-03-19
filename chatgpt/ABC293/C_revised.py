def dfs(i, j, visited, grid, H, W):
    if i == H - 1 and j == W - 1:
        return 1 if len(visited) == H + W - 1 else 0

    count = 0

    if j < W - 1 and grid[i][j + 1] not in visited:
        visited.add(grid[i][j + 1])
        count += dfs(i, j + 1, visited, grid, H, W)
        visited.remove(grid[i][j + 1])

    if i < H - 1 and grid[i + 1][j] not in visited:
        visited.add(grid[i + 1][j])
        count += dfs(i + 1, j, visited, grid, H, W)
        visited.remove(grid[i + 1][j])

    return count


def main():
    H, W = map(int, input().split())
    grid = [list(map(int, input().split())) for _ in range(H)]

    visited = set([grid[0][0]])
    happy_paths = dfs(0, 0, visited, grid, H, W)

    print(happy_paths)


if __name__ == "__main__":
    main()
