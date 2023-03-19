#include <iostream>
#include <cmath>

using namespace std;

bool is_binary_representation(long long N, long long b) {
    while (N > 0) {
        if (N % b > 1) {
            return false;
        }
        N /= b;
    }
    return true;
}

bool binary_search(long long N, int a) {
    // b^(d-1) < N ==> b < pow(N, 1/(d-1))
    int d = 0;
    int c = a;
    while (c != 0) {
        d++;
        c /= 2;
    }
    long long left = 2, right = floor(pow(N, 1/(d-1)))+1;
    
    while (left <= right) {
        long long mid = (left + right) / 2;
        long long f_x = 0;
        long long p_b = 1;
        int a1 = a;
        while (a1 > 0) {
            f_x += (a1 & 1) * p_b;
            p_b *= mid;
            a1 /= 2;
        }
        if (f_x == N) {
            return true;
        } else if (f_x < N) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }
    return false;
}

int count_base_with_binary_digits(long long N) {
    int count = 0;
    int d_upper_limit = ceil(log2(N) + 1);

    for (int a = 2; a < 1024; a++) {
        if (binary_search(N, a)) {
            count++;
        }
    }
    for (int d = 11; d <= d_upper_limit; d++) {
        long long lower_bound = ceil(pow(N / 2, 1.0 / (d - 1)));
        long long upper_bound = floor(pow(N, 1.0 / (d - 1)));

        for (long long b = lower_bound; b <= upper_bound; b++) {
            if (is_binary_representation(N, b)) {
                count++;
            }
        }
    }
    return count;
}

int main() {
    int T;
    cin >> T;
    for (int i = 0; i < T; i++) {
        long long N;
        cin >> N;
        cout << count_base_with_binary_digits(N) << endl;
    }
    return 0;
}
