#include <iostream>
#include <string>
#include <cstring>

using namespace std;

#define set_bit(arr, x) (arr[x >> 6] |= 1LL << (x & 63))

typedef unsigned long long uint64;

string str_a, str_b;

uint64 cache[782], match[782];

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

int solve()
{
    const int len_a = static_cast<int>(str_a.size()), len_b = static_cast<int>(str_b.size());
    const int block_size = (len_b >> 6) + 1;
    int answer = 0;

    for (int i = 0; i < len_b; i++)
        if (str_a[0] == str_b[i])
        {
            set_bit(cache, i);
            break;
        }

    for (int i = 0; i < len_a; i++)
    {
        memset(match, 0, sizeof(uint64) * block_size);

        for (int j = 0; j < len_b - 1; j++)
            if (str_a[i + 1] == str_b[j + 1])
                set_bit(match, j);

        uint64 shift_carry = 1, subtract_carry = 0;

        for (int k = 0; k < block_size; k++)
        {
            uint64 temp1 = match[k] | cache[k];

            const uint64 temp2 = cache[k] << 1 | shift_carry;
            shift_carry = cache[k] >> 63;

            uint64 temp3 = temp1;
            subtract_carry = subtract(temp3, subtract_carry);
            subtract_carry += subtract(temp3, temp2);

            cache[k] = temp1 ^ temp1 & temp3;
        }
    }

    for (int i = 0; i < block_size; i++)
        answer += count(cache[i]);

    return answer;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    cin >> str_a >> str_b;
    cout << solve();
    return 0;
}