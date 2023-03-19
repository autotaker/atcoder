#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

void dfs(int node, vector<vector<int> >& adj, vector<bool>& visited) {
    visited[node] = true;
    for (int next : adj[node]) {
        if (!visited[next]) {
            dfs(next, adj, visited);
        }
    }
}

int main() {
    int N, M;
    cin >> N >> M;
    vector<vector<int> > adj(N);
    for (int i = 0; i < M; i++) {
        int u, v;
        cin >> u >> v;
        u--; v--; // 0-indexed
        adj[u].push_back(v);
    }

    int cnt = 0;
    for (int i = 0; i < N; i++) {
        vector<bool> visited(N, false);
        dfs(i, adj, visited);
        for (int j = 0; j < N; j++) {
            if (i != j && visited[j] && adj[i].end() == find(adj[i].begin(), adj[i].end(), j)) {
                cnt++;
            }
        }
    }

    cout << cnt << endl;
    return 0;
}
