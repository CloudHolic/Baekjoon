#include <algorithm>
#include <cmath>
#include <iostream>
#include <list>
#include <vector>

using namespace std;

class query
{
public:
	int index, left, right;
	int sqrt;

	bool operator<(const query& other) const
	{
		if (left / sqrt ^ other.left / sqrt)
			return left / sqrt < other.left / sqrt;
		return right < other.right;
	}
};

int sqrt_n;
vector<int> nums, counts, sqds, results;
vector<list<int>> exi;
vector<query> queries;

void add(const int k)
{
	if (exi[nums[k]].size() >= 2)
	{
		const int diff = exi[nums[k]].back() - exi[nums[k]].front();
		counts[diff]--;
		sqds[diff / sqrt_n]--;
	}

	if (exi[nums[k]].empty() || exi[nums[k]].back() < k)
		exi[nums[k]].push_back(k);
	else
		exi[nums[k]].push_front(k);

	if (exi[nums[k]].size() >= 2)
	{
		const int diff = exi[nums[k]].back() - exi[nums[k]].front();
		counts[diff]++;
		sqds[diff / sqrt_n]++;
	}
}

void erase(const int k)
{
	if (exi[nums[k]].size() >= 2)
	{
		const int diff = exi[nums[k]].back() - exi[nums[k]].front();
		counts[diff]--;
		sqds[diff / sqrt_n]--;
	}

	if (exi[nums[k]].back() == k)
		exi[nums[k]].pop_back();
	else
		exi[nums[k]].pop_front();

	if (exi[nums[k]].size() >= 2)
	{
		const int diff = exi[nums[k]].back() - exi[nums[k]].front();
		counts[diff]++;
		sqds[diff / sqrt_n]++;
	}
}

int main()
{
	ios::sync_with_stdio(false);
	cin.tie(nullptr);

	int n;
	cin >> n;
	nums.resize(n + 2);
	counts.resize(n + 2);
	exi.resize(n + 2);

	int sum = 0, minimum = 0;
	for (int i = 1; i <= n; i++)
	{
		int temp;
		cin >> temp;

		sum += temp;
		minimum = min(minimum, sum);

		nums[i] = sum;
	}

	for (int i = 0; i <= n; i++)
		nums[i] -= minimum;

	sqrt_n = static_cast<int>(sqrt(n)) + 1;
	sqds.resize(sqrt_n);

	int m;
	cin >> m;
	queries.resize(m);
	results.resize(m);
	for (int i = 0; i < m; i++)
	{
		queries[i].index = i;
		cin >> queries[i].left >> queries[i].right;
		queries[i].sqrt = sqrt_n;
		queries[i].left--;
	}
	sort(queries.begin(), queries.end());

	counts[0] = sqds[0] = 1;
	add(0);

	for (int i = 0, l = 0, r = 0; i < m; i++)
	{
		const int st = queries[i].left;
		const int en = queries[i].right;

		while (l > st)
			add(--l);
		while (r < en)
			add(++r);

		while (l < st)
			erase(l++);
		while (r > en)
			erase(r--);

		int sq = (n + 1) / sqrt_n;
		while (!sqds[sq])
			sq--;

		int q = min(n, (sq + 1) * sqrt_n);
		while (!counts[q])
			q--;

		results[queries[i].index] = q;
	}

	for (int i = 0; i < m; i++)
		cout << results[i] << "\n";

	return 0;
}