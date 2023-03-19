class UnionFind:
    def __init__(self, n):
        self.parent = list(range(n))
        self.rank = [0] * n

    def find(self, x):
        if self.parent[x] != x:
            self.parent[x] = self.find(self.parent[x])
        return self.parent[x]

    def union(self, x, y):
        root_x, root_y = self.find(x), self.find(y)
        if root_x == root_y:
            return
        if self.rank[root_x] > self.rank[root_y]:
            root_x, root_y = root_y, root_x
        self.parent[root_x] = root_y
        if self.rank[root_x] == self.rank[root_y]:
            self.rank[root_y] += 1

def main():
    n, m = map(int, input().split())
    uf = UnionFind(2*n)
    num_edges = [0] * n

    for _ in range(m):
        a, b, c, d = input().split()
        a, c = int(a) - 1, int(c) - 1

        if b == 'R':
            a += n
        if d == 'R':
            c += n

        uf.union(a, c)
        num_edges[uf.find(a)] += 1

    cyclic_groups = non_cyclic_groups = 0
    visited = [False] * (2 * n)
    for i in range(n):
        root = uf.find(i)
        if not visited[root]:
            visited[root] = True
            if uf.find(root + n) == root:
                cyclic_groups += 1
            else:
                non_cyclic_groups += 1

    print(cyclic_groups, non_cyclic_groups)

if __name__ == "__main__":
    main()
