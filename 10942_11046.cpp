#include <iostream>
#include <vector>

using namespace std;

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    int radius = -1, center = -1;
    vector<int> seq, palindrome;

    cin >> n;
    const int len = 2 * n + 1;
    seq.reserve(len);
    palindrome.resize(len);

    for (int i = 0; i < n; i++)
    {
        int temp;
        cin >> temp;
        seq.push_back(-1);
        seq.push_back(temp);
    }
    seq.push_back(-1);

    for (int i = 0; i < len; i++)
    {
        if (radius >= i)
            palindrome[i] = min(radius - i, palindrome[center * 2 - i]);
        else
            palindrome[i] = 0;

        while (i + palindrome[i] + 1 < len && i - palindrome[i] - 1 >= 0 && seq[i + palindrome[i] + 1] == seq[i - palindrome[i] - 1])
            palindrome[i]++;

        if (i + palindrome[i] > radius)
        {
            radius = i + palindrome[i];
            center = i;
        }
    }
	
    cin >> m;

    for(int i = 0; i < m; i++)
    {
        int s, e;	    
        cin >> s >> e;

        const int trans_s = 2 * s - 1;
        const int trans_e = 2 * e - 1;
        const int trans_center = (trans_s + trans_e) / 2;

        cout << (trans_center - trans_s <= palindrome[trans_center]) << "\n";
    }
	
    return 0;
}