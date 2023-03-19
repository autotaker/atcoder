from sys import stdin
from collections import defaultdict

def main():
    N, M = map(int, stdin.readline().split())
    connections = defaultdict(list)

    for _ in range(M):
        A, B, C, D = stdin.readline().split()
        A, C = int(A), int(C)
        connections[(A, B)].append((C, D))
        connections[(C, D)].append((A, B))

    visited = set()
    cycle_count = 0
    non_cycle_count = 0

    def dfs(node, parent):
        if node in visited:
            return 0
        visited.add(node)
        total = 1
        for child in connections[node]:
            if child == parent:
                continue
            total += dfs(child, node)
        return total

    for i in range(1, N+1):
        for color in ['R', 'B']:
            node = (i, color)
            if node not in visited:
                size = dfs(node, None)
                if size % 2 == 0:
                    cycle_count += 1
                else:
                    non_cycle_count += 1

    print(cycle_count // 2, non_cycle_count // 2)

main()
