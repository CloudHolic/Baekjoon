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

namespace PollardRho
{
    using namespace MillerRabin;

    int64 calc(int64 x, int64 c, int64 m)
    {
        int64 square = (int128)x * x % m;
        return (square + c) % m;
    }

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

    void inner(int64 num, vector<int64> &factors)
    {
        if (num == 1)
            return;
        if (!(num & 1))
        {
            factors.push_back(2);
            inner(num / 2, factors);
            return;
        }
        if (miller_rabin(num))
        {
            factors.push_back(num);
            return;
        }

        int64 x, y, c;
        for(;;)
        {
            x = rand() % (num - 2) + 1;
            c = rand() % 20 + 1;
            y = x;

            do
            {
                x = calc(x, c,  num);
                y = calc(calc(y, c, num), c, num);
            } while(binary_gcd(num, abs(x - y)) == 1);

            if (x != y)
                break;
        }

        int64 g = binary_gcd(num, abs(x - y));
        inner(g, factors);
        inner(num / g, factors);
    }

    vector<int64> pollard_rho(int64 num)
    {
        vector<int64> result;
        inner(num, result);
        sort(result.begin(), result.end());
        return result;
    }
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int64 num;
    cin >> num;

    // num = 4^x * (8y + 7) if answer = 4
    int64 remainder = num;
    while (!(remainder & 3))
        remainder >>= 2;
    if ((remainder & 7) == 7)
    {
        cout << "4\n";
        return 0;
    }

    // Factorize num
    // There are some 4a+3 prime that appears odd number if answer = 3
    vector<int64> factors;
    factors = PollardRho::pollard_rho(num);

    int64 cur = 0;
    int count = 0;
    for (auto &f: factors)
    {
        if (cur < f)
        {
            if (count > 0 && (count & 1) == 1)
                break;

            cur = f, count = 0;
        }
        if (cur == f && (f & 3) == 3)
            count++;
    }
    if (count & 1)
    {
        cout << "3\n";
        return 0;
    }

    // sqrt * sqrt != n if answer = 2
    auto root = static_cast<int64>(sqrt(num));
    if (root * root != num)
    {
        cout << "2\n";
        return 0;
    }

    // answer = 1
    cout << "1\n";
    return 0;
}