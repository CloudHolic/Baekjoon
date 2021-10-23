#include <algorithm>
#include <iostream>
#include <string>

#pragma warning (disable: 4554)
#pragma warning (disable: 26451)

using namespace std;

#define get_bit(arr, x) ((arr[x >> 6] & 1LL << (x & 63)) != 0)
#define set_bit(arr, x) (arr[x >> 6] |= 1LL << (x & 63))

typedef unsigned long long uint64;

int global_count;
uint64 cache[3000][47] = {};
uint64 match[3000][26][47] = {};
int dp_array[3001][3001] = {};
string answer[3000];

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

void solve(const string& str_a, const string& str_b)
{
    const int len_a = static_cast<int>(str_a.size()), len_b = static_cast<int>(str_b.size());
    const int block_size = (len_b >> 6) + 1;

    for (int j = 0; j < len_b; j++)
        set_bit(match[global_count][str_b[j] - 'a'], j);

    for (int i = 0; i < len_a; i++)
    {
        uint64 shift_carry = 1, subtract_carry = 0;

        for (int k = 0; k < block_size; k++)
        {
            uint64 temp1 = match[global_count][str_a[i] - 'a'][k] | cache[global_count][k];

            const uint64 temp2 = cache[global_count][k] << 1 | shift_carry;
            shift_carry = cache[global_count][k] >> 63;

            uint64 temp3 = temp1;
            subtract_carry = subtract(temp3, subtract_carry);
            subtract_carry += subtract(temp3, temp2);

            cache[global_count][k] = temp1 ^ temp1 & temp3;
        }

        for (int j = 1; j <= len_b; j++)
            dp_array[i + 1][j] = dp_array[i + 1][j - 1] + get_bit(cache[global_count], j - 1);
    }

    int i = len_a, j = len_b;
    while (i > 0 && j > 0 && dp_array[i][j] != 0)
    {
        if (dp_array[i][j] == dp_array[i][j - 1])
            j--;
        else if (dp_array[i][j] == dp_array[i - 1][j])
            i--;
        else if (dp_array[i][j] - 1 == dp_array[i - 1][j - 1])
        {
            answer[global_count].push_back(str_a[i - 1]);
            i--;
            j--;
        }
    }
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    global_count = -1;
    string str, result;

    cin >> n;
    for (int i = 1; i <= n; i++)
    {
        int idx = -1;
        cin >> str;
        const size_t len = str.length();

        for (size_t j = 1; j < len; j++)
        {
            global_count++;

            if (idx > 0 && answer[idx].length() + j >= len)
                break;

            solve(str.substr(0, j), str.substr(j, len - j));

            if (idx < 0 || answer[idx].length() < answer[global_count].length())
                idx = global_count;            
        }
        
        string cur = idx > 0 ? answer[idx] : "";
        reverse(cur.begin(), cur.end());
        cur += cur;
        result += "Case #" + to_string(i) + ": " + to_string(cur.length()) + "\n";
        if (cur.length() > 0)
            result += cur + "\n";
    }

    cout << result;
    return 0;
}