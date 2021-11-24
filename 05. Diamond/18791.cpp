#include <algorithm>
#include <cmath>
#include <iostream>
#include <numeric>
#include <vector>

using namespace std;

vector<bool> solve(int n, const vector<int>& nums);

vector<bool> solve_composite(const int a, const int b, const vector<int>& nums)
{
	vector<int> sub_sums(2 * b - 1, 0);
	vector<vector<int>> sub_answers(2 * b - 1);
	vector<bool> index_used(2 * a * b - 1, false);

	for (int i = 0; i < 2 * b - 1; i++)
	{
		vector<int> cur_nums;

		for (int j = 0; j < 2 * a * b - 1; j++)
		{
			if (index_used[j])
				continue;

			cur_nums.push_back(nums[j] % a);
			if (cur_nums.size() == static_cast<size_t>(2 * a - 1))
				break;
		}

		vector<bool> cur_answer = solve(a, cur_nums);
		for (int j = 0, k = 0; j < 2 * a * b - 1 && k < 2 * a - 1; j++)
		{
			if (index_used[j])
				continue;

			if (cur_answer[k])
			{
				sub_answers[i].push_back(j);
				sub_sums[i] += nums[j];
				index_used[j] = true;
			}

			k++;
		}
	}

	for (int i = 0; i < 2 * b - 1; i++)
		sub_sums[i] = sub_sums[i] % b;

	const vector<bool> cur_answer = solve(b, sub_sums);

	vector<bool> result(2 * a * b - 1, false);
	for (int i = 0; i < 2 * b - 1; i++)
	{
		if (cur_answer[i])
		{
			for (const auto& cur : sub_answers[i])
				result[cur] = true;
		}
	}

	return result;
}

vector<bool> solve_prime(const int p, const vector<int>& nums)
{
	vector<bool> result(2 * p - 1, false);
	vector<int> idx(2 * p - 1, 0);

	iota(idx.begin(), idx.end(), 0);
	sort(idx.begin(), idx.end(), [&](const int n, const int m) { return nums[n] < nums[m]; });

	for (int i = 0; i < p; i++)
	{
		if (nums[idx[i]] == nums[idx[i + p - 1]])
		{
			for (int j = i; j < i + p; j++)
				result[idx[j]] = true;
			return result;
		}
	}

	vector<vector<int>> sums_list(p);
	for (auto& v : sums_list)
		v = vector<int>(p, -1);

	// a_0 = b_0 = idx[0]
	// a_i = idx[i] / b_i = idx[i + p - 1] (i = 1 ~ p-1)
	sums_list[0][nums[idx[0]]] = idx[0];	// s_0 = idx[0] only
	for (int i = 1; i < p; i++)
	{
		for (int j = 0; j < p; j++)
		{
			if (sums_list[i - 1][j] == -1)
				continue;

			const int a_sum = (j + nums[idx[i]]) % p;
			const int b_sum = (j + nums[idx[i + p - 1]]) % p;

			if (sums_list[i][a_sum] == -1)
				sums_list[i][a_sum] = idx[i];

			if (b_sum != a_sum && sums_list[i][b_sum] == -1)
				sums_list[i][b_sum] = idx[i + p - 1];
		}
	}

	for (int i = p - 1, cur_sum = 0; i >= 0; i--)
	{
		int cur_idx = sums_list[i][cur_sum];
		result[cur_idx] = true;
		cur_sum = (cur_sum - nums[cur_idx] + p) % p;
	}

	return result;
}

vector<bool> solve(const int n, const vector<int>& nums)
{
	const int root = static_cast<int>(sqrt(n));
	for (int i = 2; i <= root; i++)
		if (n % i == 0)
			return solve_composite(i, n / i, nums);

	return solve_prime(n, nums);
}

int main()
{
	ios::sync_with_stdio(false);
	cin.tie(nullptr);

	int size;
	vector<int> nums;

	cin >> size;
	for (int i = 0; i < 2 * size - 1; i++)
	{
		int temp;
		cin >> temp;
		nums.push_back(temp);
	}

	if (size == 1)
		cout << nums[0];
	else
	{
		const vector<bool> answer = solve(size, nums);
		for (int i = 0; i < 2 * size - 1; i++)
			if (answer[i])
				cout << nums[i] << " ";
	}

	return 0;
}