#include <iostream>
#include <vector>
#include <string>

using namespace std;

const int MOD = 998244353;

int main() {
    int N, M;
    cin >> N >> M;
    vector<string> S(N);
    for (int i = 0; i < N; i++) {
        cin >> S[i];
    }

    // dp[k][i][j]の初期化
    vector<vector<vector<long long>>> dp(M + 1, vector<vector<long long>>(N, vector<long long>(N, 0)));

    // sub[k][i][j][d]の初期化
    vector<vector<vector<vector<long long>>>> sub(M + 1, vector<vector<vector<long long>>>(N, vector<vector<long long>>(N, vector<long long>(10, 0))));

    for( int i = 0; i < N; i++ ) {
        for( int j = i; j < N; j++ ) {
            dp[M][i][j] = 1; // Only ""
        }
    } 
    for( int k = M-1; k >= 0; k-- ) {  // 20
        // sub を更新する
        // sub[k][i][j][d] 
        for( int i = N-1; i >= 0; i-- ) { // 20
            for( int j = i; j < N; j++ ) { // 20
                for( int d = 0; d < 10; d++ ) { // 10
                    // S[i][k] == d の時、S[i][k:] < ... < S[j][k:]となる場合の数
                    for( int l = i; l <= j; l++ ) { // 20
                        // S[i][k] == S[i+1][k] == ... == S[l][k] < S[l+1][k] となる時
                        if( S[l][k] != '?' && S[l][k] != '0' + d ) {
                            // これより先は条件を満たさないのでスキップ
                            break;
                        }
                        if( k == M - 1 && l != i ) {
                            // 末尾１桁が２桁以上揃うのはNG
                            break;
                        }
                        if( l == j ) {
                            sub[k][i][j][d] += dp[k+1][i][l];
                            sub[k][i][j][d] %= MOD;
                        } else {
                            for( int d2 = d+1; d2 < 10; d2++ ) { // 10
                                sub[k][i][j][d] += dp[k+1][i][l] * sub[k][l+1][j][d2] % MOD;
                                sub[k][i][j][d] %= MOD;
                            }
                        }
                    }
                    // cerr << "sub " << k << " " << i << " " << j << " " << d << " = " << sub[k][i][j][d] << endl;
                }
            }
        }
        // dpを更新する
        for( int i = 0; i < N; i++ ) {
            for( int j = i; j < N; j++ ) {
                // S[i][k:] < ... < S[j][k:]となる場合の数
                for( int d = 0; d < 10; d++) {
                    dp[k][i][j] += sub[k][i][j][d];
                    dp[k][i][j] %= MOD;
                }
                // cerr << "dp" << k << " " << i << " " << j << " = " << dp[k][i][j] << endl;
            }
        }
    }
    // 答えを計算する部分
    int ans = dp[0][0][N-1];

    cout << ans << endl;

    return 0;
}
/*
dp 1 0 0 = 1 S[0][1:] < .. < S[0][1:]
dp 1 0 1 = 10 S[0][1:] < .. < S[1][1:]
dp 1 0 2 = 6 S[0][1:]
dp 1 1 1 = 10
dp 1 1 2 = 6
dp 1 2 2 = 1
*/
