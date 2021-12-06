#include <iostream>
#include <map>

using namespace std;

typedef long long int64;

map<tuple<int64, int64>, int> cache;

int solve(int64 x, int64 y)
{
	if (x == 0 && y == 0)
		return 0;

	if (cache.find({ x, y }) != cache.end())
		return cache[{x, y}];

	int result = 0;
	for (int i = 1; i <= 40; i++)
	{
		int64 temp = (1LL << i) - 1;
		const int64 st = max(temp, x);
		const int64 en = min(2 * temp, y);

		if (st <= en)
			result = max(result, solve(st - temp, en - temp) + i);
	}

	cache[{x, y}] = result;
	return result;
}

int main()
{
	ios::sync_with_stdio(false);
	cin.tie(nullptr);

	int t;
	cin >> t;

	for (; t > 0; t--)
	{
		int64 x, y;
		cin >> x >> y;
		cout << solve(x, y) << "\n";
	}

	return 0;
}