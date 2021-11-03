#define private public
#include <bitset>
#undef private

#include <bits/stdc++.h>
#include <x86intrin.h>

using namespace std;

template<size_t _Nw>
void _M_do_sub(_Base_bitset<_Nw> &A, const _Base_bitset<_Nw> &B)
{
	for(int i=0, c=0; i<_Nw; i++)
		c=_subborrow_u64(c, A._M_w[i], B._M_w[i], (unsigned long long*)&A._M_w[i]);
}

template<>
void _M_do_sub(_Base_bitset<1> &A, const _Base_bitset<1> &B)
{
	A._M_w-=B._M_w;
}

template<size_t _Nb>
bitset<_Nb>& operator-=(bitset<_Nb> &A, const bitset<_Nb> &B)
{
	_M_do_sub(A, B);
	return A;
}

template<size_t _Nb>
inline bitset<_Nb> operator-(const bitset<_Nb> &A, const bitset<_Nb> &B)
{
	bitset<_Nb> C(A);
	return C-=B;
}

template<size_t size>
string getLcs(const string& str_a, const string& str_b)
{
    string result;
    bitset<size> cache, match[26];
    vector<bitset<size>> dp_array(str_a.size() + 1, bitset<size>());

    for (int i = 0; i < str_b.size(); i++)
        match[str_b[i] - 'a'].set(i);

    for (int i = 0; i < str_a.size(); i++)
    {
        cache = match[str_a[i] - 'a'] | dp_array[i];
        dp_array[i + 1] = dp_array[i] << 1;
        dp_array[i + 1].set(0);
        dp_array[i + 1] = cache & (cache ^ (cache - dp_array[i + 1]));
    }

    for (int x = str_a.size(), y = str_b.size() - 1; x > 0; x--)
    {
        while (y >= 0 && !dp_array[x][y])
            y--;
        if (y < 0)
            break;

        while (x > 0 && dp_array[x - 1][y])
            x--;

        result.push_back(str_b[y--]);
    }

    return result;
}

string solve(const string& str_a, const string& str_b)
{
    size_t size = str_b.size();

    if (size <= 128)
        return getLcs<128>(str_a, str_b);
    else if (size <= 256)
        return getLcs<256>(str_a, str_b);
    else if (size <= 384)
        return getLcs<384>(str_a, str_b);
    else if (size <= 512)
        return getLcs<512>(str_a, str_b);
    else if (size <= 640)
        return getLcs<640>(str_a, str_b);
    else if (size <= 768)
        return getLcs<768>(str_a, str_b);
    else if (size <= 896)
        return getLcs<896>(str_a, str_b);
    else if (size <= 1024)
        return getLcs<1024>(str_a, str_b);
    else if (size <= 1152)
        return getLcs<1152>(str_a, str_b);
    else if (size <= 1280)
        return getLcs<1280>(str_a, str_b);
    else if (size <= 1408)
        return getLcs<1408>(str_a, str_b);

    return getLcs<1536>(str_a, str_b);
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    string str, cur, result;

    cin >> n;
    for (int i = 1; i <= n; i++)
    {
        cin >> str;
        string str_a, str_b;
        size_t len = str.length(), len_cur = 0;
        cur.clear();

        for (size_t j = 1; j < len; j++)
        {
            if (len_cur >= len - j)
                break;

            str_a = str.substr(0, j);
            str_b = str.substr(j, len - j);

            string answer = str_a.size() > str_b.size() ? solve(str_a, str_b) : solve(str_b, str_a);
            if (len_cur < answer.length())
            {
                cur = answer;
                len_cur = cur.length();
            }
        }

        reverse(cur.begin(), cur.end());
        result += "Case #" + to_string(i) + ": " + to_string(len_cur * 2) + "\n";
        if (len_cur > 0)
            result += cur + cur + "\n";
    }

    cout << result;
    return 0;
}