#include <algorithm>
#include <cmath>
#include <numeric>
#include <iostream>
#include <vector>

using namespace std;

vector<bool> solve(int n, vector<int> nums);

vector<bool> solve_composite(int a, int b, vector<int> nums)
{
	vector<int> sub_sums;
	vector<vector<int>> sub_answers(2 * b - 1);
	vector<bool> index_used;

	for (int i = 0; i < 2 * b - 1; i++)
	{
		vector<int> cur_nums;

		for (int j = 0; j < 2 * a * b - 1; j++)
		{
			if (index_used[j])
				continue;

			cur_nums.push_back(nums[j]);
			if (cur_nums.size() == 2 * a - 1)
				break;
		}

		vector<bool> cur_answer = solve(a, cur_nums);
		for (int j = 0, k = 0; j < 2 * a * b - 1 || k < 2 * a - 1; j++)
		{
			if (index_used[j])
				continue;

			if (cur_answer[k])
			{
				sub_answers[i].push_back(j);
				sub_sums[i] += cur_nums[k];
				index_used[j] = true;
			}

			k++;
		}
	}

	for (int i = 0; i < 2 * b - 1; i++)
		sub_sums[i] = sub_sums[i] / a % b;

	vector<bool> cur_answer = solve(b, sub_sums);

	vector<bool> result(2 * a * b - 1, false);
	for (int i = 0; i < 2 * b - 1; i++)
	{
		if (cur_answer[i])
		{
			for (auto& cur : sub_answers[i])
				result[cur] = true;
		}
	}

	return result;
}

vector<bool> solve_prime(int p, vector<int> nums)
{
	vector<bool> result(2 * p - 1, false);
	vector<int> idx(2 * p - 1, 0);
	
	iota(idx.begin(), idx.end(), 0);
	sort(idx.begin(), idx.end(), [&](int n, int m) { return nums[n] < nums[m]; });

	for (int i = 0; i < p; i++)
	{
		if (nums[idx[i]] == nums[idx[i + p - 1]])
		{
			for (int j = i; j < i + p; j++)
				result[idx[j]] = true;
			return result;
		}
	}

	int cur_mod = 0;
	for (int i = 0; i < p; i++)
	{
		cur_mod += nums[idx[i]];
		cur_mod %= p;
		result[idx[i]] = true;
	}

	if (cur_mod == 0)
		return result;

	vector<bool> sub_sums(p);


	return result;
}

vector<bool> solve(int n, vector<int> nums)
{
	int root = static_cast<int>(sqrt(n));
	for (int i = 2; i <= root; i++)
		if (n % i == 0)
			return solve_composite(n, n / i, nums);

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
		vector<bool> answer = solve(size, nums);
		for (int i = 0; i < 2 * size - 1; i++)
			if (answer[i])
				cout << nums[i] << " ";
	}

	return 0;
}