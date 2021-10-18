#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int a[100001], b[100001], inverse[100001];

int main()
{
    cin.tie(0);
	ios_base::sync_with_stdio(0);

    int n;
	cin >> n;

	for (int i = 1; i <= n; i++)
        cin >> a[i];

	for (int i = 1; i <= n; i++)
        cin >> b[i];

	for (int i = 1; i <= n; i++)
		inverse[b[i]] = i;
	
	for (int i = 1; i <= n; i++)
		a[i] = inverse[a[i]];	

	int ans = 0;
	vector<int> v;
    v.push_back(-1);
    
	for (int i = 1; i <= n; i++){
		if (v.back() < a[i]){
			v.push_back(a[i]);
			ans++;
		}
		else{
			auto it = lower_bound(v.begin(), v.end(), a[i]);
			*it = a[i];
		}
	}
	cout << ans;
}