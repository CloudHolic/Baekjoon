#include <bits/stdc++.h>

using namespace std;

typedef long long int64;
typedef __int128 int128;

namespace QuadraticResidue
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

    // Find an integer x such that x^2 = n mod p.
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

        // 6. The answer is r, -r.
    }
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    using namespace QuadraticResidue;

    int t;
    cin >> t;

    for (int i = 0; i < t; i++)
    {
        int64 p, a_0, a_1;
        cin >> p >> a_0 >> a_1;

        if (p == 2)
        {
            if (a_0 == 0 && a_1 == 0)
                cout << "0 0\n";
            else if (a_0 == 0 && a_1 == 1)
                cout << "0 1\n";
            else if (a_0 == 1 && a_1 == 0)
                cout << "1 1\n";
            else
                cout << "-1\n";

            continue;
        }

        int64 k = (a_1 & 1) ? ((a_1 + p) / 2) : (a_1 / 2);
        int64 n = (k * k - a_0 + p) % p;
        int64 ans = tonelli_shanks(n, p);
        if (ans == -1)
        {
            cout << "-1\n";
            continue;
        }

        int64 result_1 = (k + ans) % p;
        int64 result_2 = (a_1 - result_1 + p) % p;
        if (result_1 < result_2)
            cout << result_1 << " " << result_2 << "\n";
        else
            cout << result_2 << " " << result_1 << "\n";
    }

    return 0;
}