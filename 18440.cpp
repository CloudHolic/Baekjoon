#include <iostream>
#include <string>
#include <cstring>

using namespace std;

#define set_bit(arr, x) (arr[x >> 6] |= 1LL << (x & 63))

typedef unsigned long long uint64;

string str_a, str_b;
string answer;
short cache_f[2][7001], cache_b[2][7001];
uint64 cache[782], match[26][782];

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

int get_lcs()
{
    const int len_a = static_cast<int>(str_a.size()), len_b = static_cast<int>(str_b.size());
    const int block_size = (len_b >> 6) + 1;

    for (int j = 0; j < len_b; j++)
        set_bit(match[str_b[j] - 'A'], j);

    for (int i = 0; i < len_a; i++)
    {
        uint64 shift_carry = 1, subtract_carry = 0;

        for (int k = 0; k < block_size; k++)
        {
            uint64 temp1 = match[str_a[i] - 'A'][k] | cache[k];

            const uint64 temp2 = cache[k] << 1 | shift_carry;
            shift_carry = cache[k] >> 63;

            uint64 temp3 = temp1;
            subtract_carry = subtract(temp3, subtract_carry);
            subtract_carry += subtract(temp3, temp2);

            cache[k] = temp1 ^ temp1 & temp3;
        }
    }

    int answer = 0;
    for (int i = 0; i < block_size; i++)
        answer += count(cache[i]);

    return answer;
}

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

    cin >> str_a >> str_b;
    solve(1, static_cast<int>(str_a.length()), 1, static_cast<int>(str_b.length()));
    cout << answer.length() << "\n" << answer;
    return 0;
}