#include "wiring.h"
#include <bits/stdc++.h>

using namespace std;

typedef long long int64;

struct point
{
	int value;
	bool color;
};

long long min_total_length(std::vector<int> r, std::vector<int> b) {
    const int n = static_cast<int>(r.size()), m = static_cast<int>(b.size()), size = n + m;

	vector<point> arr;
	vector<int> near, sp, last;
	vector<int64> matching, cache;

	arr.resize(size + 1);
	near.resize(size + 1);
	sp.resize(size + 1);
	last.resize(2 * (size + 1));
	matching.resize(size + 1);
	cache.resize(size + 1);

	for (int i = 1, j = 1, k = 1; k <= size; k++)
	{
		if (j > m || i <= n && r[i - 1] < b[j - 1])
			arr[k] = { r[i++ - 1], false };
		else
			arr[k] = { b[j++ - 1], true };
	}

	for (int i = 1; i <= size; i++)
		near[i] = INT_MAX;

	int color[2] = { 0, };
	for (int i = 1; i <= size; i++)
	{
		if (color[!arr[i].color])
			near[i] = min(near[i], arr[i].value - arr[color[!arr[i].color]].value);
		color[arr[i].color] = i;
	}

	memset(color, 0, sizeof color);
	for (int i = size; i > 0; i--)
	{
		if (color[!arr[i].color])
			near[i] = min(near[i], arr[color[!arr[i].color]].value - arr[i].value);
		color[arr[i].color] = i;
	}

	for (int& i : sp)
		i = -1;
	for (int& i : last)
		i = -1;

	last[size] = 0;
	int now = 0;

	for (int i = 1; i <= size; i++)
	{
		if (arr[i].color)
		{
			now++;
			matching[i] = arr[i].value;
		}
		else
		{
			now--;
			matching[i] = -arr[i].value;
		}

		matching[i] += matching[i - 1];
		if (last[now + size] >= 0)
			sp[i] = last[now + size];
		last[now + size] = i;
	}

	for (int i = 1; i <= size; i++)
	{
		cache[i] = cache[i - 1] + near[i];
		if (sp[i] >= 0)
			cache[i] = min(cache[i], cache[sp[i]] + abs(matching[i] - matching[sp[i]]));		
	}

	return cache[size];
}