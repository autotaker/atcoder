#include <iostream>
#include <vector>

using namespace std;

class UnionFind {
public:
    vector<int> parent;
    vector<int> rank;
    vector<int> size;

    UnionFind(int n) {
        parent.resize(n);
        rank.resize(n, 0);
        size.resize(n, 1);
        for (int i = 0; i < n; i++) {
            parent[i] = i;
        }
    }

    int find(int x) {
        if (parent[x] == x) {
            return x;
        } else {
            return parent[x] = find(parent[x]);
        }
    }

    void unite(int x, int y) {
        x = find(x);
        y = find(y);
        if (x == y) return;

        if (rank[x] < rank[y]) {
            parent[x] = y;
            size[y] += size[x];
        } else {
            parent[y] = x;
            size[x] += size[y];
            if (rank[x] == rank[y]) rank[x]++;
        }
    }

    bool same(int x, int y) {
        return find(x) == find(y);
    }

    int group_size(int x) {
        return size[find(x)];
    }
};
int main() {
    int N, M;
    cin >> N >> M;

    UnionFind uf(N);
    vector<pair<int, int>> connections;

    for (int i = 0; i < M; i++) {
        int A, C;
        char B, D;
        cin >> A >> B >> C >> D;

        A--; C--;

        connections.push_back(make_pair(A, C));
    }

    for (const auto& connection : connections) {
        uf.unite(connection.first, connection.second);
    }

    vector<int> connection_count(N, 0);

    for (const auto& connection : connections) {
        int root = uf.find(connection.first);
        connection_count[root]++;
    }

    int connected_rings = 0;
    int not_connected_rings = 0;

    for (int i = 0; i < N; i++) {
        if (uf.find(i) == i) {
            if (uf.group_size(i) == connection_count[i]) {
                connected_rings++;
            } else {
                not_connected_rings++;
            }
        }
    }

    cout << connected_rings << " " << not_connected_rings << endl;
    return 0;
}
