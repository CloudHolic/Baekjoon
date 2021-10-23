#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

#pragma warning (disable: 4554)
#pragma warning (disable: 26451)

using namespace std;

#define get_bit(arr, x) ((arr[x >> 6] & 1LL << (x & 63)) != 0)
#define set_bit(arr, x) (arr[x >> 6] |= 1LL << (x & 63))

typedef unsigned long long uint64;

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

string solve(const string& str_a, const string& str_b)
{
    const int len_a = static_cast<int>(str_a.size()), len_b = static_cast<int>(str_b.size());
    const int block_size = (len_b >> 6) + 1;
    vector<uint64> cache(block_size, 0);
    vector<vector<uint64>> match(26, vector<uint64>(block_size, 0));
    vector<vector<int>> dp_array(len_a + 1, vector<int>(len_b + 1, 0));

    for (int j = 0; j < len_b; j++)
        set_bit(match[str_b[j] - 'a'], j);

    for (int i = 0; i < len_a; i++)
    {
        uint64 shift_carry = 1, subtract_carry = 0;

        for (int k = 0; k < block_size; k++)
        {
            uint64 temp1 = match[str_a[i] - 'a'][k] | cache[k];

            const uint64 temp2 = cache[k] << 1 | shift_carry;
            shift_carry = cache[k] >> 63;

            uint64 temp3 = temp1;
            subtract_carry = subtract(temp3, subtract_carry);
            subtract_carry += subtract(temp3, temp2);

            cache[k] = temp1 ^ temp1 & temp3;
        }

        for (int j = 1; j <= len_b; j++)
            dp_array[i + 1][j] = dp_array[i + 1][j - 1] + get_bit(cache, j - 1);
    }

    string answer;
    int i = len_a, j = len_b;
    while (dp_array[i][j] != 0)
    {
        if (dp_array[i][j] == dp_array[i][j - 1])
            j--;
        else if (dp_array[i][j] == dp_array[i - 1][j])
            i--;
        else if (dp_array[i][j] - 1 == dp_array[i - 1][j - 1])
        {
            answer.push_back(str_a[i - 1]);
            i--;
        	j--;
        }
    }

    reverse(answer.begin(), answer.end());
    return answer;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    string str, result, cur;

    cin >> n;
    for(int i = 1; i <= n; i++)
    {
        cin >> str;
        const size_t len = str.length();
        cur.clear();

        for(size_t j = 1; j < len; j++)
        {
            if (cur.length() + j >= len)
                break;

            string answer = solve(str.substr(0, j), str.substr(j, len - j));            

            if (cur.length() < answer.length())
                cur = answer;
        }

        cur += cur;
        result += "Case #" + to_string(i) + ": " + to_string(cur.length()) + "\n";
        if (cur.length() > 0)
            result += cur + "\n";
    }

    cout << result;
    return 0;
}