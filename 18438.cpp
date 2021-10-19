#include <iostream>
#include <string>

using namespace std;

string str_a, str_b;
string answer;

short cache_f[7001][2], cache_b[7001][2];

void solve(const short start_a, const short end_a, const short start_b, const short end_b)
{
    if (start_a < 1 || start_b < 1 || start_a > end_a || start_b > end_b)
        return;

    // 1. Initialize & Split str_b in half
    memset(cache_f, 0, sizeof(short) * 7001 * 2);
    memset(cache_b, 0, sizeof(short) * 7001 * 2);
    const short mid = (start_b + end_b) / 2;

    // 1. Forwarding DP
    for (short i = start_a; i <= end_a; i++)
	    for (short j = start_b; j <= mid; j++)
            if (str_a[i - 1] == str_b[j - 1])
                cache_f[i][j & 1] = cache_f[i - 1][j - 1 & 1] + 1;
            else
                cache_f[i][j & 1] = max(cache_f[i - 1][j & 1], cache_f[i][j - 1 & 1]);
    
    // 2. Reverse DP
    for (short i = end_a - 1; i >= start_a; i--)
        for (short j = end_b - 1; j > mid; j--)        
            if (str_a[i] == str_b[j])
                cache_b[i][j & 1] = cache_b[i + 1][j + 1 & 1] + 1;
            else
                cache_b[i][j & 1] = max(cache_b[i + 1][j & 1], cache_b[i][j + 1 & 1]);

    // 3. Find maximum answer
    short idx = -1, max = -1;
    for (short i = start_a; i < end_a; i++)
    {
        const short cur = cache_f[i][mid & 1] + cache_b[i + 1][mid + 1 & 1];
        if (cur <= max)
            continue;

        max = cur;
        idx = i;
    }
    
    if (max <= cache_f[end_a][mid & 1])
        idx = end_a;
    
    // 4. Record the answer and split two problems.
    solve(start_a, idx - 1, start_b, mid - 1);

    if (idx > -1)
        answer.push_back(str_a[idx]);

    solve(idx + 1, end_a, mid + 1, end_b);
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    str_a.reserve(7000);
    str_b.reserve(7000);
    answer.reserve(7000);

    cin >> str_a;
    cin >> str_b;

    solve(1, str_a.length(), 1, str_b.length());

    cout << answer.length() << "\n" << answer;
    return 0;
}