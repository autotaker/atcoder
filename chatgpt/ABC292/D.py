from collections import defaultdict

def dfs(u, graph, visited, cnt):
    visited[u] = True
    cnt[0] += 1
    cnt[1] += len(graph[u])

    for v in graph[u]:
        if not visited[v]:
            dfs(v, graph, visited, cnt)

def is_all_components_balanced(N, M, edges):
    graph = defaultdict(list)

    for u, v in edges:
        graph[u].append(v)
        graph[v].append(u)

    visited = [False] * (N + 1)

    for u in range(1, N + 1):
        if not visited[u]:
            cnt = [0, 0]
            dfs(u, graph, visited, cnt)

            if cnt[0] * 2 != cnt[1]:
                return "No"

    return "Yes"

# Test cases
print(is_all_components_balanced(3, 3, [(2, 3), (1, 1), (2, 3)]))
print(is_all_components_balanced(5, 5, [(1, 2), (2, 3), (3, 4), (3, 5), (1, 5)]))
print(is_all_components_balanced(13, 16, [(7, 9), (7, 11), (3, 8), (1, 13), (11, 11), (6, 11), (8, 13), (2, 11), (3, 3), (8, 12), (9, 11), (1, 11), (5, 13), (3, 12), (6, 9), (1, 10)]))
