#include <bits/stdc++.h>

using namespace std;

typedef long long int64;
typedef __int128 int128;

namespace MillerRabin
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

        for(auto &i: {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37})
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
    using namespace MillerRabin;

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

        // Make x_1 positive
        if (x_1 < 0)
            x_1 += origin_m;

        return x_1;
    }

    // Apply Tonelli-Shanks Algorithm.
    // Find an integer x such that x^2 = n mod p, where p is a prime number.
    int64 tonelli_shanks(int64 n, int64 p)
    {
        if (n == 0)
            return 0;

        if (n >= p)
            n %= p;

        if (p == 2)
            return n;

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
}

namespace Cornacchia
{
    using namespace QuadraticResidue;

    // Find integers x, y such that x^2 + d * y^2 = n, where 1 <= d < m.
    pair<int64, int64> cornacchia(int64 d, int64 n)
    {
        // 0. if n == 2, then d must be 1, and the solution is (1, 1)
        if (n == 2)
            return make_pair(1, 1);

        // 1. Find r_0 such that r_0^2 = -d mod n.
        int64 sol = tonelli_shanks(n - d, n);
        if (sol == -1)
            return make_pair(-1, -1);

        vector<int64> possible = {sol, n - sol};
        for(auto &r: possible)
        {
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

int get_number_of_squares(int64 num, vector<int64>& factors)
{
    // num = 4^x * (8y + 7) if answer = 4
    int64 remainder = num, quotient = 0;
    while (!(remainder & 3))
    {
        quotient++;
        remainder >>= 2;
    }
    if ((remainder & 7) == 7)
    {
        factors.push_back(quotient);
        return 4;
    }

    // Factorize num
    // There are some 4a+3 prime that appears odd number if answer = 3
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
        return 3;

    // sqrt * sqrt != n if answer = 2
    auto root = static_cast<int64>(sqrt(num));
    if (root * root != num)
        return 2;

    return 1;
}

bool find_2_squares(int64 num, vector<int64>& factors, vector<int64>& squares, int64 base = 1)
{
    // Factorize num.
    int64 multiplier = 1;
    vector<int64> single_factors;

    size_t size = factors.size();
    int count = 0;
    for (size_t i = 0; i < size; i++)
    {
        if (count == 0)
        {
            count++;
            continue;
        }

        if (factors[i] == factors[i - 1])
        {
            multiplier *= factors[i];
            count = 0;
        }
        else
            single_factors.push_back(factors[i - 1]);
    }

    if (count == 1)
        single_factors.push_back(factors[size - 1]);

    // Use Cornacchia's Algorithm for each single factors.
    int64 x = 0, y = 0;
    for (auto &f: single_factors)
    {
        auto answer = Cornacchia::cornacchia(1, f);
        if (answer.first == -1 || answer.second == -1)
            return false;

        if (x == 0 || y == 0)
        {
            x = answer.first;
            y = answer.second;
            continue;
        }

        int64 old_x = x, old_y = y;
        if (old_x < old_y)
            swap(old_x, old_y);
        if (old_x * answer.first - old_y * answer.second > 0)
        {
            x = old_x * answer.second + old_y * answer.first;
            y = old_x * answer.first - old_y * answer.second;
        }
        else if (old_x * answer.second - old_y * answer.first > 0)
        {
            x = old_x * answer.second - old_y * answer.first;
            y = old_x * answer.first + old_y * answer.second;
        }
        else
            return false;
    }

    squares.push_back(x * multiplier * base);
    squares.push_back(y * multiplier * base);
    return true;
}

bool find_3_squares(int64 num, vector<int64>& factors, vector<int64>& squares)
{
    // Factorize num.
    int64 multiplier = 1;
    int64 single_factors = 1;

    size_t size = factors.size();
    int count = 0;
    for (size_t i = 0; i < size; i++)
    {
        if (count == 0)
        {
            count++;
            continue;
        }

        if (factors[i] == factors[i - 1])
        {
            multiplier *= factors[i];
            count = 0;
        }
        else
            single_factors *= factors[i - 1];
    }

    if (count == 1)
        single_factors *= factors[size - 1];

    // Define num = t^2 + x^2 + y^2.
    // For t >= 1, find out which (num - t^2) can be represented of sum of two square numbers.
    auto root = static_cast<int64>(single_factors);
    for (int64 i = 1; i <= root; i++)
    {
        int64 n = single_factors - i * i;
        vector<int64> sub_factors;
        if(get_number_of_squares(n, sub_factors) != 2)
            continue;

        if(find_2_squares(num - i * i, sub_factors, squares, multiplier))
        {
            squares.push_back(i * multiplier);
            break;
        }
    }
    return true;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    vector<int64> squares, factors;

    int64 num;
    cin >> num;

    int count = get_number_of_squares(num, factors);
    if (count == 4)
    {
        int64 element = MillerRabin::fast_pow(2, factors[0],  num);
        squares.push_back(element);

        int64 square = element * element;
        factors = PollardRho::pollard_rho(num - square);
        find_3_squares(num - square, factors, squares);
    }
    else if (count == 3)
        find_3_squares(num, factors, squares);
    else if (count == 2)
        find_2_squares(num, factors, squares);
    else // if (count == 1)
        squares.push_back(static_cast<int64>(sqrt(num)));

    cout << count << "\n";
    sort(squares.begin(), squares.end());
    copy(squares.begin(), squares.end(), ostream_iterator<int64>(cout, " "));
    return 0;
}