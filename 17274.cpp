#include <iostream>
#include <algorithm>
#include <vector>
#include <ppl.h>

using namespace std;
using namespace concurrency;

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    vector<int> front, back, query;
    long long result = 0;

    cin >> n >> m;
    front.reserve(n);
    back.reserve(n);
    query.reserve(m);
    
    for(int i = 0; i < n; i++)
    {
        int a, b;
        cin >> a >> b;
        front.push_back(a);
        back.push_back(b);
    }

    for(int i = 0; i < m; i++)
    {
        int k;
        cin >> k;
        query.push_back(k);
    }

    parallel_for(int(0), n, [&](int i)
    {
        bool cur = front[i] > back[i];
        int maxNum = max(front[i], back[i]);
        int minNum = min(front[i], back[i]);
            
        int lastQuery = -1;
        for (int i = m - 1; i > -1; i--)
        {
            if (query[i] >= maxNum || query[i] < minNum)
                continue;
            
            lastQuery = i;
            break;
        }
    
        int largeQueryCount = 0;
        for (int i = lastQuery + 1; i < m; i++)
            if (query[i] >= maxNum)
                largeQueryCount++;
    
        result += (lastQuery > -1) ^ (largeQueryCount % 2 == 0) ? minNum : maxNum;
    });

    cout << result;
    return 0;
}