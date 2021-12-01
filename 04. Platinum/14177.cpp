#include <functional>
#include <iostream>
#include <vector>

using namespace std;

typedef long long int64;

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);    

    int n, m;
    cin >> n >> m;

    vector<vector<int64>> awkwardness(n + 1, vector<int64>(n + 1, 0));
    for (int i = 1; i <= n; i++)
        for (int j = 1; j <= n; j++)
			cin >> awkwardness[i][j];

    for (int i = 1; i <= n; i++)
        for (int j = i + 1; j <= n; j++)
            awkwardness[i][j] += awkwardness[i][j - 1];

    for (int j = 1; j <= n; j++)
        for (int i = j - 1; i > 0; i--)
            awkwardness[i][j] += awkwardness[i + 1][j];

    vector<vector<int64>> cache(m + 1, vector<int64>(n + 1, 0));
    for (int i = 1; i <= n; i++)
        cache[0][i] = INT32_MAX;

    function<void (int, int, int, int, int)> solve = [&](int step, int s, int e, int l, int r)
    {
        if (s > e)
            return;

        int mid = s + e >> 1;
        pair<int64, int> best = make_pair<int64, int>(INT32_MAX, -1);
        for (int k = l; k <= min(mid, r); k++)
            best = min(best, { cache[step - 1][k - 1] + awkwardness[k][mid], k });

        cache[step][mid] = best.first;
        solve(step, s, mid - 1, l, best.second);
        solve(step, mid + 1, e, best.second, r);
    };

    for (int i = 1; i <= m; i++)
        solve(i, 1, n, 1, n);

    cout << cache[m][n];
    return 0;
}