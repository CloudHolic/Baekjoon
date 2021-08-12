#include <iostream>
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
        int max = cur ? front[i] : back[i];
        int min = cur ? back[i] : front[i];

        for(int j = 0; i < m; i++)
        {
            if(query[j] > max)            
                continue;            
            else if(query[j] < min)            
                cur = !cur;            
            else if(!cur)
                cur = !cur;            
        }

        result += cur ? max : min;
    });

    cout << result;
    return 0;
}