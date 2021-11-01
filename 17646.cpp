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

namespace QuadraticResidue
{
    // Compute x * y mod m
    int64 multiply(int64 x, int64 y, int64 m)
    {
        return (int128)x * y % m;
    }

    // Compute n^k mod m.
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

    // Compute n^(-1) mod m.
    int64 inverse(int64 n, int64 m)
    {
        if (m == 1)
            return 0;

        int64 x_0 = 0, x_1 = 1, origin_m = m;
        while (n >1)
        {
            int64 q = n / m, t = m;

            m = n % m;
            n = t;
            t = x_0;
            x_0 = x_1 - q * x_0;
            x_1 = t;
        }

        // Make x1 positive
        if (x_1 < 0)
            x_1 += origin_m;

        return x_1;
    }

    // Apply Chinese Remainder Theorem.
    // Find x such that
    // x = remainders[0] mod nums[0],
    // x = remainders[1] mod nums[1],
    // ... and so on.
    int64 chinese_remainder(vector<int64> nums, vector<int64> remainders)
    {
        int64 product = 1, result = 0;
        size_t size = nums.size();
        if (remainders.size() != size)
            return -1;

        for (int64 &num : nums)
            product *= num;

        // Apply above formula
        for (size_t i = 0; i < size; i++) {
            int64 pp = product / nums[i];
            result += multiply(multiply(remainders[i], inverse(pp, nums[i]), product), pp, product);
        }

        return result % product;
    }

    // Apply Hensel's Lifting.
    int64 hensel_lifting()
    {

    }

    // Apply Tonelli-Shanks Algorithm.
    // Find an integer x such that x^2 = n mod p, where p is a prime number.
    int64 tonelli_shanks(int64 n, int64 p)
    {
        if (n == 0)
            return 0;

        // 1. Check if n is quadratic residue in mod p.
        if (fast_pow(n, (p - 1) / 2, p) != 1)
            return -1;

        // 2. Find odd number q and non-negative integer s such that p - 1 = q * 2^s
        int64 q = p -1, s = 0;
        while(!(q & 1))
        {
            s++;
            q >>= 1;
        }

        // 3. Find quadratic non-residue integer z in mod p.
        int64 z;
        for(int64 i = 2; i < p; i++)
        {
            if (fast_pow(i, (p - 1) / 2, p) != 1)
            {
                z = i;
                break;
            }
        }

        // 4. Let m = s mod p, c = z^q mod p, t = n^q mod p, and r = n^((q + 1) / 2) mod p
        int64 m = s % p;
        int64 c = fast_pow(z, q, p);
        int64 t = fast_pow(n, q, p);
        int64 r = fast_pow(n, (q + 1) / 2, p);

        // 5. Repeat the following.
        // (1) if t = 0 -> return 0
        // (2) if t = 1 -> return r
        // (3) find the first integer i such that 0 < i < m and t^(2^i) = 1 mod p.
        // (4) let b = c^(2^(m - i - 1)) mod p
        // (5) m = i mod p, c = b^2 mod p, t = t * b^2 mod p, r = r * b mod p
        for( ; ; )
        {
            if (t == 0)
                return 0;
            if (t == 1)
                return r;

            int64 i;
            int64 temp = multiply(t, t, p);
            for(int64 j = 1; j < m; j++)
            {
                if (temp == 1)
                {
                    i = j;
                    break;
                }
                temp = multiply(temp, temp, p);
            }

            int64 b = fast_pow(c, fast_pow(2, m - i - 1, p - 1), p);

            m = i % p;
            c = multiply(b, b, p);
            t = multiply(t, c, p);
            r = multiply(r, b, p);
        }

        // 6. The answer is r, p - r.
    }

    // Solve x^2 = n mod m.
    vector<int64> quadratic_residue(int64 n, int64 m)
    {
        vector<int64> result;

        return result;
    }
}

namespace Cornacchia
{
    using namespace QuadraticResidue;

    // Find integers x, y such that x^2 + d * y^2 = n, where 1 <= d < m.
    pair<int64, int64> cornacchia(int64 d, int64 n)
    {
        // 1. Find r_0 such that r_0^2 = -d mod n.
        int64 sol = tonelli_shanks(n - d, n);
        vector<int64> possible;
        if(sol > 0)
            possible = {sol, n - sol};

        for(auto &r: possible)
        {
            if (r > n / 2)
                r = n - r;

            // 2. Let r_1 = n mod r_0
            int64 prev_r = r, prev_prev_r;
            r = n % r;

            // 3. For k > 1, let r_k = r_(k-2) mod r_(k-1)
            // Repeat this until r_k < sqrt n.
            auto root = static_cast<int64>(sqrt(n));
            while(r > root)
            {
                prev_prev_r = prev_r;
                prev_r = r;
                r = prev_prev_r % prev_r;
            }

            // 4. Check if s = sqrt((n - r_k^2) / d) is an integer.
            // If so, then x = r_k, and y = s. Otherwise, try with another r_0.
            int64 temp = n - r * r;
            if (temp % d != 0)
                continue;
            temp /= d;

            root = static_cast<int64>(sqrt(temp));
            if(root * root != temp)
                continue;

            return make_pair(r, root);
        }

        // No answer.
        return make_pair(-1, -1);
    }
}

int main()
{
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