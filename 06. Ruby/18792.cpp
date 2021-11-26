#include <algorithm>
#include <cmath>
#include <cstring>
#include <iostream>
#include <numeric>
#include <queue>
#include <vector>

using namespace std;

#define set_bit(arr, x) (arr[x >> 6] |= 1ULL << (63 - (x & 63)))
#define get_bit(arr, x) (arr[x >> 6] & (1ULL << (63 - (x & 63)))) != 0

typedef unsigned long long uint64;

uint64 sums_list[50000][782];

void left_shift(int p, int index, int x)
{
	const uint64(&src)[782] = sums_list[index - 1];
	uint64(&dest)[782] = sums_list[index];

	const int bit_count = p + 63 >> 6;
	const int remainder = p & 63;
	const int q = x >> 6;
	const int r = x & 63;

	if (r)
	{
		for (int i = 0; i < bit_count - q - 1; i++)
			dest[i] |= src[i + q] << r | src[i + q + 1] >> 64 - r;
		dest[bit_count - q - 1] |= src[bit_count - 1] << r;
	}
	else
	{
		for (int i = 0; i < bit_count - q; i++)
			dest[i] |= src[i + q];
	}

	if (remainder)
	{
		const uint64 mask = (1UL << 64 - remainder) - 1;
		dest[bit_count - 1] &= ~mask;
	}	
}

void right_shift(int p, int index, int x)
{
	const uint64(&src)[782] = sums_list[index - 1];
	uint64(&dest)[782] = sums_list[index];

	const int bits = p + 63 >> 6;
	const int remainder = p & 63;
	const int q = x >> 6;
	const int r = x & 63;

	if (r)
	{
		dest[q] |= src[0] >> r;
		for (int i = 1; i < bits - q; i++)
			dest[i + q] |= src[i - 1] << 64 - r | src[i] >> r;
	}
	else
	{
		for (int i = 0; i < bits - q; i++)
			dest[i + q] |= src[i];
	}

	if (remainder)
	{
		const uint64 mask = (1UL << 64 - remainder) - 1;
		dest[bits - 1] &= ~mask;
	}
}

vector<bool> solve(int n, const vector<int>& nums);

vector<bool> solve_composite(const int a, const int b, const vector<int>& nums)
{
	vector<int> sub_sums(2 * b - 1, 0), cur_index(2 * a - 1);
	vector<vector<int>> sub_answers(2 * b - 1);
	queue<int> index;

	for (int i = 0; i < 2 * a - 1; i++)
		cur_index[i] = i;
	for (int i = 2 * a - 1; i < 2 * a * b - 1; i++)
		index.push(i);

	for (int i = 0; i < 2 * b - 1; i++)
	{
		vector<int> cur_nums(2 * a - 1);
		for (int j = 0; j < 2 * a - 1; j++)
			cur_nums[j] = nums[cur_index[j]] % a;

		vector<bool> cur_answer = solve(a, cur_nums);

		for (int j = 0; j < 2 * a - 1; j++)
		{
			if (cur_answer[j])
			{
				sub_answers[i].push_back(cur_index[j]);
				sub_sums[i] += nums[cur_index[j]];

				if (i < 2 * b - 2)
				{
					cur_index[j] = index.front();
					index.pop();
				}
			}
		}
	}

	for (int i = 0; i < 2 * b - 1; i++)
		sub_sums[i] = sub_sums[i] / a % b;

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
	
	int mod = 0;
	for (int i = 0; i < p; i++)
	{
		mod = (mod + nums[idx[i]]) % p;
		result[idx[i]] = true;
	}
	if (mod == 0)
		return result;

	// a_0 = b_0 = idx[0]
	// a_i = idx[i] / b_i = idx[i + p - 1] (i = 1 ~ p-1)
	const int bits = p + 63 >> 6;
	memset(sums_list[0], 0, sizeof(uint64) * bits);
	set_bit(sums_list[0], mod);

	int step = 1;
	for (; step < p; step++)
	{
		const int diff = nums[idx[step + p - 1]] - nums[idx[step]];
		memcpy(sums_list[step], sums_list[step - 1], sizeof(uint64) * bits);
		right_shift(p, step, diff);
		left_shift(p, step, p - diff);

		if (get_bit(sums_list[step], 0))
			break;
	}

	for (int i = step, cur_sum = 0; i >= 1; i--)
	{
		const int diff = nums[idx[i + p - 1]] - nums[idx[i]];
		int temp_sum = (cur_sum - diff + p) % p;

		if (get_bit(sums_list[i - 1], temp_sum))
		{
			result[idx[i]] = false;
			result[idx[i + p - 1]] = true;
			cur_sum = temp_sum;
		}
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