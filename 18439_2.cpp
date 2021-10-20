#include <iostream>
#include <string>
#include <cstring>

using namespace std;

#define set_bit(arr, x) (arr[x >> 6] |= 1 << (x & 63))

string str_a, str_b;

unsigned long long cache[782], match[782];

unsigned long long subtract(unsigned long long& x, const unsigned long long y)
{
    const unsigned long long temp = x;
    x = temp - y;
    return x > temp;
}

int count(unsigned long long num)
{
    unsigned long long v = num;
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

    for (int i = 0; i < len_a; i++)
    {
        memset(match, 0, sizeof(unsigned long long) * block_size);

        for (int j = 0; j < len_b; j++)
            if (str_a[i + 1] == str_b[j + 1])
                set_bit(match, j);

        unsigned long long shift_carry = 1, subtract_carry = 0;

        for (int k = 0; k < block_size; k++)
        {
            unsigned long long temp1 = match[k] | cache[k];

            const unsigned long long temp2 = cache[k] << 1 | shift_carry;
            shift_carry = cache[k] >> 63;

            unsigned long long temp3 = temp1;
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