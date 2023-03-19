#include <iostream>
#include <cmath>
#include <unordered_map>

int comb(int k, std::unordered_map<int, int>& memo) {
    if (memo.find(k) != memo.end()) {
        return memo[k];
    }

    int count = 0;
    for (int i = 1; i <= std::sqrt(k); ++i) {
        if (k % i == 0) {
            count += 1;
            if (i != k / i) {
                count += 1;
            }
        }
    }
    
    memo[k] = count;
    return count;
}

int main() {
    int N;
    std::cin >> N;

    std::unordered_map<int, int> memo;
    int64_t total = 0;

    for (int k = 1; k < N; ++k) {
        total += comb(k, memo) * comb(N - k, memo);
    }

    std::cout << total << std::endl;

    return 0;
}
