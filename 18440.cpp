#include <iostream>
#include <string>
#include <cstring>

using namespace std;

using namespace std;

#define get_bit(arr, x) ((arr[x >> 6] & 1LL << (x & 63)) != 0)
#define set_bit(arr, x) (arr[x >> 6] |= 1LL << (x & 63))

typedef unsigned long long uint64;

string str_a, str_b;
string answer;
int len_a, len_b;

uint64 cache_f[782], cache_b[782], match[26][782], r_match[26][782];
short result_f[50001], result_b[50001];

uint64 subtract(uint64& x, const uint64 y)
{
    const uint64 temp = x;
    x = temp - y;
    return x > temp;
}

int count(uint64 num)
{
    uint64 v = num;
    int count = 0;
    for (; v; count++)
        v &= v - 1;

    return count;
}


void solve(const int start_a, const int end_a, const int start_b, const int end_b)
{
    // 1. Initialize & Split str_a in half
    const int mid = (start_a + end_a) / 2;
    const int start_block = start_b >> 6, end_block = end_b >> 6;

    memset(cache_f + start_b - 1, 0, sizeof(short) * (end_b - start_b + 2));
    memset(cache_b + start_b, 0, sizeof(short) * (end_b - start_b + 2));

    // 2. Forwarding DP    
    string str_a_f = str_a.substr(start_a, mid - start_a);
    string str_b_f = str_b.substr(start_b, end_b - start_b);

    for (int i = 0; i < str_b_f.size(); i++)
        set_bit(match[str_b_f[i] - 'A'], i);

    for (int i = start_a; i <= mid; i++)
    {
        uint64 shift_carry = 1, subtract_carry = 0;

        for (int k = start_block; k <= end_block; k++)
        {
            uint64 temp1 = match[str_a[i - 1] - 'A'][k] >> start_b - 1 | cache_f[k];

            const uint64 temp2 = cache_f[k] << 1 | shift_carry;
            shift_carry = cache_f[k] >> 63;

            uint64 temp3 = temp1;
            subtract_carry = subtract(temp3, subtract_carry);
            subtract_carry += subtract(temp3, temp2);

            cache_f[k] = temp1 ^ temp1 & temp3;
        }
    }

    for (int i = start_a; i <= mid; i++)
        for (int j = start_b; j <= end_b; j++)
            if (str_a[i - 1] == str_b[j - 1])
                cache_f[i & 1][j] = cache_f[i & 1 ^ 1][j - 1] + 1;
            else
                cache_f[i & 1][j] = max(cache_f[i & 1 ^ 1][j], cache_f[i & 1][j - 1]);

    // 3. Reverse DP
    string str_a_r = str_a.substr(start_a, end_a - mid);
    string str_b_r = str_b.substr(start_b, end_b - start_b);
    reverse(str_a_r.begin(), str_a_r.end());
    reverse(str_b_r.begin(), str_b_r.end());
    

    for (int i = 0; i < str_b_r.size(); i++)
        set_bit(r_match[str_b_r[i] - 'A'], i);

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

    cin >> str_a >> str_b;
    solve(0, static_cast<int>(str_a.length() - 1), 0, static_cast<int>(str_b.length() - 1));
    cout << answer.length() << "\n" << answer;
    return 0;
}