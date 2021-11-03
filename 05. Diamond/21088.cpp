#include <bits/stdc++.h>

using namespace std;

typedef long long int64;
typedef __int128 int128;

namespace MillerRabin
{
    int64 multiply(int64 x, int64 y, int64 m)
    {
        return (int128)x * y % m;
    }

    int64 fast_pow(int64 n, int64 k, int64 m)
    {
        int64 result = 1, next_exp = n % m;
        while(k)
        {
            if (k & 1)
                result = multiply(result, next_exp, m);
            next_exp = multiply(next_exp, next_exp, m);
            k >>= 1;
        }
        return result;
    }

    bool miller_rabin_test(int64 num, int64 a)
    {
        if (num % a == 0)
            return false;
        int64 d = num - 1;

        for(;;)
        {
            int64 cur = fast_pow(a, d, num);
            if(d & 1)
                return cur != 1 && cur != num - 1;
            if (cur == num - 1)
                return false;
            d >>= 1;
        }
    }

    bool miller_rabin(int64 num)
    {
        if (num < 2 || num % 6 % 4 != 1)
            return (num | 1) == 3;

        for(auto &i: {2, 325, 9375, 28178, 450775, 9780504, 1795265022})    // {2, 7, 61} for 32-bit int.
        {
            if (num <= i)
                break;
            if (miller_rabin_test(num, i))
                return false;
        }
        return true;
    }
}

int64 arr[1000];
vector<bool> sieve(1000000);
vector<int64> prime_list;

int64 binary_gcd(int64 x, int64 y)
{
    if (x == 0)
        return y;
    if (y == 0)
        return x;

    int shift = __builtin_ctz(x | y);
    x >>= __builtin_ctz(x);

    do {
        y >>= __builtin_ctz(y);
        if (x > y)
            swap(x, y);
        y -= x;
    } while (y != 0);

    return x << shift;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    int64 result = 0;

    for (int i = 2; i < 1000; i++)
    {
        if (sieve[i])
            continue;
        for (int j = i * i; j < 1000000; j += i)
            sieve[j] = true;
    }

    for (int i = 2; i < 1000000; i++)
    {
        if (sieve[i])
            continue;
        prime_list.push_back(i);
    }

    cin >> n;
    for (int i = 0; i < n; i++)
        cin >> arr[i];

    for (const auto &f: prime_list)
    {
        int64 len = 0;
        for (int i = 0; i < n; i++)
        {
            if (arr[i] == 1 || arr[i] % f != 0)
            {
                if (len >0)
                    result ^= len;
                len = 0;
                continue;
            }

            while (arr[i] % f == 0)
                arr[i] /= f;

            len++;
        }
        result ^= len;
    }

    for (int i = 0; i < n; i++)
    {
        if (arr[i] == 1)
            continue;

        int64 square = -1;
        bool is_prime = MillerRabin::miller_rabin(arr[i]);
        if (!is_prime)
        {
            auto root = static_cast<int64>(sqrt(arr[i]));
            if (arr[i] == root * root)
                square = root;
        }

        // arr[i] = p || arr[i] = p^2
        if (is_prime || square > 0)
        {
            int64 f = is_prime ? arr[i] : square;
            int64 len = 0;
            for (int j = i; j < n; j++)
            {
                if (arr[j] == 1 || arr[j] % f != 0)
                {
                    if (len > 0)
                        result ^= len;
                    len = 0;
                    continue;
                }

                while (arr[j] % f == 0)
                    arr[j] /= f;

                len++;
            }
            result ^= len;
        }
        // arr[i] = p * q
        else
        {
            int64 f = arr[i];
            int64 len = 0;
            for (int j = i; j < n; j++)
            {
                if (arr[j] == 1)
                {
                    len = 0;
                    continue;
                }

                if (arr[j] % f != 0)
                {
                    int64 gcd = binary_gcd(arr[j], f);
                    if (gcd == 1)
                    {
                        len = 0;
                        continue;
                    }

                    result ^= len;
                    for (; j < n; j++)
                    {
                        if (arr[j] == 1 || arr[j] % gcd != 0)
                        {
                            result ^= len;
                            len = 0;
                            break;
                        }

                        while (arr[j] % gcd == 0)
                            arr[j] /= gcd;

                        len++;
                    }
                }

                while (arr[j] % f == 0)
                    arr[j] /= f;

                len++;
            }
        }
    }

    cout << (result ? "First" : "Second") << endl;
    return 0;
}