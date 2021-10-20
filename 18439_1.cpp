// Solved by std::bitset
// It returns correct answer, but exceeds the time limit.

#include <bitset>
#include <iostream>
#include <string>

using namespace std;

string str_a, str_b;
bitset<50001> cache, match, temp1, temp2;
bitset<50001> one = bitset<50001>(1);

bool subtract_helper(bool b1, bool b2, bool& borrow)
{
    bool diff;
    if (borrow) {
        diff = !(b1 ^ b2);
        borrow = !b1 || b1 && b2;
    }
    else {
        diff = b1 ^ b2;
        borrow = !b1 && b2;
    }
    return diff;
}

bitset<50001> subtract(bitset<50001> x, bitset<50001> y)
{
    bool borrow = false;

    bitset<50001> ans;
    for (int i = 0; i < 50000; i++) {
        ans[i] = subtract_helper(x[i], y[i], borrow);
    }
    return ans;
}

int solve()
{
    const int len_a = static_cast<int>(str_a.size()), len_b = static_cast<int>(str_b.size());

    for(int i = 0; i < len_a; i++)
    {
        for(int j = 0; j < len_b; j++)        
            match.set(j, str_a[i + 1] == str_b[j + 1]);

        temp1 = match | cache;
        temp2 = cache << 1 | one;

        cache = temp1 ^ temp1 & subtract(temp1, temp2);
    }

    return cache.count();
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    cin >> str_a >> str_b;
    cout << solve();
    return 0;
}