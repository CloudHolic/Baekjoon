#define _USE_MATH_DEFINES

#include <algorithm>
#include <cmath>
#include <complex>
#include <iostream>
#include <stack>
#include <vector>

using namespace std;

#define sz(v) ((int)(v).size())
#define all(v) (v).begin(),(v).end()

typedef complex<double> base;

void fft(vector <base>& a, const bool invert)
{
    const int n = sz(a);
    for (int i = 1, j = 0; i < n; i++)
    {
        int bit = n >> 1;
        for (; j >= bit; bit >>= 1)
            j -= bit;
        j += bit;

        if (i < j)
            swap(a[i], a[j]);
    }

    for (int len = 2; len <= n; len <<= 1)
    {
        const double ang = 2 * M_PI / len * (invert ? -1 : 1);
        const base wlen(cos(ang), sin(ang));

        for (int i = 0; i < n; i += len)
        {
            base w(1);
            for (int j = 0; j < len / 2; j++)
            {
                base u = a[i + j];
                base v = a[i + j + len / 2] * w;

                a[i + j] = u + v;
                a[i + j + len / 2] = u - v;

                w *= wlen;
            }
        }
    }

    if (invert)
        for (int i = 0; i < n; i++)
            a[i] /= n;
}

void multiply(const vector<int>& a, const vector<int>& b, vector<int>& res)
{
    vector <base> fa(all(a)), fb(all(b));

    int n = 1;
    while (n < sz(a) + sz(b) - 1)
        n <<= 1;

    fa.resize(n);
    fb.resize(n);

    fft(fa, false);
    fft(fb, false);

    for (int i = 0; i < n; i++)
        fa[i] *= fb[i];

    fft(fa, true);

    res.resize(n);
    for (int i = 0; i < n; i++)
        res[i] = static_cast<int>(fa[i].real() + (fa[i].real() > 0 ? 0.5 : -0.5));
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    string num1, num2, answer;
    cin >> num1 >> num2;

    vector<int> x(num1.size()), y(num2.size());
    const size_t len1 = num1.size(), len2 = num2.size();
    for (size_t i = 0; i < max(len1, len2); i++)
    {
        if (i < len1)
            x[len1 - i - 1] = num1[i] - '0';
        if (i < len2)
            y[len2 - i - 1] = num2[i] - '0';
    }

    vector<int> result;
    multiply(x, y, result);

    stack<int> stk;

    for(const auto& cur : result)
    {
        int last = 0;
        if (!stk.empty())
        {
            last = stk.top();
            stk.pop();
        }
        
        answer.push_back((last + cur) % 10 + '0');

        last = (last + cur) / 10;
        if (last != 0)
            stk.push(last);
    }

    if (!stk.empty())
        answer.push_back(stk.top() + '0');    
    
    answer.erase(answer.find_last_not_of('0') + 1);
    reverse(answer.begin(), answer.end());
    cout << (answer.empty() ? "0" : answer);

    return 0;
}