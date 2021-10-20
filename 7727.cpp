#include <iostream>
#include <string>
#include <cstring>

using namespace std;

string str_a, str_b;
string answer;

short cache_f[2][10001], cache_b[2][10001];

void solve(const int start_a, const int end_a, const int start_b, const int end_b)
{
    // 1. Initialize & Split str_a in half    
    for (int i = 0; i < 2; i++)
    {
        memset(cache_f[i] + start_b - 1, 0, sizeof(short) * (end_b - start_b + 2));
        memset(cache_b[i] + start_b, 0, sizeof(short) * (end_b - start_b + 2));
    }
    const int mid = (start_a + end_a) / 2;

    // 2. Forwarding DP
    for (int i = start_a; i <= mid; i++)
        for (int j = start_b; j <= end_b; j++)
            if (str_a[i - 1] == str_b[j - 1])
                cache_f[i & 1][j] = cache_f[i & 1 ^ 1][j - 1] + 1;
            else
                cache_f[i & 1][j] = max(cache_f[i & 1 ^ 1][j], cache_f[i & 1][j - 1]);

    // 3. Reverse DP
    for (int i = end_a; i > mid; i--)
        for (int j = end_b; j >= start_b; j--)
            if (str_a[i - 1] == str_b[j - 1])
                cache_b[i & 1][j] = cache_b[i & 1 ^ 1][j + 1] + 1;
            else
                cache_b[i & 1][j] = max(cache_b[i & 1 ^ 1][j], cache_b[i & 1][j + 1]);

    // 4. Find maximum index & answer
    int idx, max;

    if (cache_f[mid & 1][end_b] > cache_b[mid & 1 ^ 1][start_b])
    {
        idx = end_b;
        max = cache_f[mid & 1][end_b];
    }
    else
    {
        idx = start_b - 1;
        max = cache_b[mid & 1][start_b];
    }

    for (int i = start_b; i < end_b; i++)
    {
        const int cur = cache_f[mid & 1][i] + cache_b[mid & 1 ^ 1][i + 1];
        if (cur <= max)
            continue;

        max = cur;
        idx = i;
    }

    // 5. Check & record the answer and split into two problems.
    if (mid == start_a)
    {
        if (cache_f[mid & 1][idx] > 0)
            answer.push_back(str_a[start_a - 1]);
    }
    else
        solve(start_a, mid, start_b, idx);

    if (mid < end_a)
        solve(mid + 1, end_a, idx + 1, end_b);
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
    
    int len_a, len_b;

    cin >> len_a >> len_b;
    cin >> str_a >> str_b;
    solve(1, len_a, 1, len_b);
    cout << answer.length() << "\n" << answer;
    return 0;
}