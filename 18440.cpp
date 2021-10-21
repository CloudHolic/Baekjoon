#include <iostream>
#include <string>
#include <cstring>

using namespace std;

#define get_bit(arr, x) ((arr[x >> 6] & 1LL << (x & 63)) != 0)
#define set_bit(arr, x) (arr[x >> 6] |= 1LL << (x & 63))

typedef unsigned long long uint64;

string str_a, r_str_a, str_b, r_str_b;
string answer;

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

    result_f[start_a - 1] = result_b[end_b + 1] = 0;
    memset(cache_f + start_block, 0, sizeof(uint64) * (end_block - start_block + 1));
    memset(cache_b + start_block, 0, sizeof(uint64) * (end_block - start_block + 1));

    // 2. Forwarding DP
    for (int i = start_a; i <= mid; i++)
    {
        uint64 shift_carry = 1, subtract_carry = 0;

        for (int k = start_block; k < end_block + 1; k++)
        {
            uint64 temp1 = match[str_a[i - 1] - 'A'][k] | cache_f[k];

            const uint64 temp2 = cache_f[k] << 1 | shift_carry;
            shift_carry = cache_f[k] >> 63;

            uint64 temp3 = temp1;
            subtract_carry = subtract(temp3, subtract_carry);
            subtract_carry += subtract(temp3, temp2);

            cache_f[k] = temp1 ^ temp1 & temp3;
        }
    }

    for (int i = start_b; i <= end_b; i++)
        result_f[i] = result_f[i - 1] + get_bit(cache_f, i - 1);

    // 3. Reverse DP
    for (int i = start_a; i < end_a - mid; i++)
    {
        uint64 shift_carry = 1, subtract_carry = 0;

        for (int k = start_block; k < end_block + 1; k++)
        {
            uint64 temp1 = r_match[r_str_a[i - 1] - 'A'][k] | cache_b[k];

            const uint64 temp2 = cache_b[k] << 1 | shift_carry;
            shift_carry = cache_b[k] >> 63;

            uint64 temp3 = temp1;
            subtract_carry = subtract(temp3, subtract_carry);
            subtract_carry += subtract(temp3, temp2);

            cache_b[k] = temp1 ^ temp1 & temp3;
        }
    }

    for (int i = end_b; i >= start_b; i--)
        result_b[i] = result_b[i + 1] + get_bit(cache_b, end_b - i);

    // 4. Find maximum index & answer
    int idx, max;

    if (result_f[end_b] > result_b[start_b])
    {
        idx = end_b;
        max = result_f[end_b];
    }
    else
    {
        idx = start_b - 1;
        max = result_b[start_b];
    }

    for (int i = start_b; i < end_b; i++)
    {
        const int cur = result_f[i] + result_b[i + 1];
        if (cur <= max)
            continue;

        max = cur;
        idx = i;
    }

    // 5. Check & record the answer and split into two problems.
    if (mid == start_a)
    {
        if (result_f[idx] > 0)
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

	r_str_a = str_a;
    r_str_b = str_b;
    reverse(r_str_a.begin(), r_str_a.end());
    reverse(r_str_b.begin(), r_str_b.end());

    const int len_a = static_cast<int>(str_a.size()), len_b = static_cast<int>(str_b.size());

    for (int j = 0; j < len_b; j++)
    {
        set_bit(match[str_b[j] - 'A'], j);
        set_bit(r_match[r_str_b[j] - 'A'], j);
    }

    solve(1, len_a, 1, len_b);
    cout << answer.length() << "\n" << answer;
    return 0;
}