#include <iostream>
#include <tuple>
#include <vector>

using namespace std;

void algorithm_1(int g, size_t size, vector<int>& num, int type, vector<int>& x, vector<int>& y, vector<int>& z);
void algorithm_2(int g, size_t size, vector<int>& num, int type, vector<int>& x, vector<int>& y, vector<int>& z);
void algorithm_3(int g, size_t size, vector<int>& num, int type, vector<int>& x, vector<int>& y, vector<int>& z);
void algorithm_4(int g, size_t size, vector<int>& num, int type, vector<int>& x, vector<int>& y, vector<int>& z);
void algorithm_5(int g, size_t size, vector<int>& num, vector<int>& x, vector<int>& y, vector<int>& z);
void algorithm_6(int g, size_t size, vector<int>& num, int type, vector<int>& x, vector<int>& y, vector<int>& z);

int change(const char c)
{
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'A' && c <= 'Z')
		return c - 'A' + 10;
	if (c >= 'a' && c <= 'z')
		return c - 'a' + 36;

	return 0;
}

char reverse_change(const int i)
{
	if (i >= 0 && i <= 9)
		return static_cast<char>(i + '0');
	if (i >= 10 && i <= 35)
		return static_cast<char>(i + 'A' - 10);
	if (i >= 36 && i <= 61)
		return static_cast<char>(i + 'a' - 36);

	return 0;
}

inline int det (const int mod, int n)
{
	while (n < 0 || n >= mod)
	{
		if (n < 0)
			n += mod;
		if (n >= mod)
			n -= mod;
	}

	return n;
}

pair<int, int> get_type(const int g, const size_t len, const vector<int>& num)
{	
	const size_t m = len / 2;
	pair<int, int> result;

	// Small numbers
	if (len < 5)
		return { 4, 0 };
	
	// Type A
	if (num[1] > 2)
	{
		if (det(g, num[len - 1] - num[0] - num[1] + 1) != 0)
			result = { 1, 1 };
		else
			result = { 1, 2 };
	}
	else if (num[1] <= 2 && num[0] != 1)
	{
		if (det(g, num[len - 1] - num[0] + 2) != 0)
			result = { 1, 3 };
		else
			result = { 1, 4 };
	}
	else if (num[0] == 1 && num[1] == 0)
	{
		if (num[2] <= 3 && det(g, num[len - 1] - num[2]) != 0)
			result = { 1, 5 };
		if (num[2] <= 2 && det(g, num[len - 1] - num[2]) == 0)
			result = { 1, 6 };
	}

	// Type B
	else if (num[0] == 1 && num[1] <= 2)
	{
		if (num[2] >= 4 && det(g, num[len - 1] - num[2]) != 0)
			result = { 2, 1 };
		else if (num[2] >= 3 && det(g, num[len - 1] - num[2]) == 0)
			result = { 2, 2 };
		else if (num[1] != 0)
		{
			if (num[2] <= 1 && num[len - 1] == 0)
				result = { 2, 3 };
			else if (num[2] >= 2 && num[2] <= 3 && num[len - 1] == 0)
				result = { 2, 4 };
			else if (num[2] <= 2 && num[len - 1] != 0)
				result = { 2, 5 };
			else if (num[2] == 3 && det(g, num[len - 1] - 3) != 0)
				result = { 2, 6 };
			else if (num[2] == 3 && num[len - 1] == 3)
				result = { 2, 7 };
		}
	}

	if (len == 5 || len == 6)
		return { 4, result.second };

	// Special numbers
	bool even;
	const bool special = num[len - m - 1] == 0 || num[len - m] == 0;

	if (result.first == 1 && (result.second == 5 || result.second == 6))
		even = len >= 7 && len % 2 == 1;
	else
		even = len >= 7 && len % 2 == 0;

	if (even && special)
		result = { 3, 0 };

	return result;
}

tuple<int, int, int> get_init(const pair<int, int> type, const int g, const size_t len, vector<int>& num)
{
	const auto [fst, snd] = type;
	if (fst == 1)
	{
		switch(snd)
		{
		case 1:
			return { num[0], num[1] - 1, det(g, num[len - 1] - num[0] - num[1] + 1) };
		case 2:
			return { num[0], num[1] - 2, 1 };
		case 3:
			return { num[0] - 1, g - 1, det(g, num[len - 1] - num[0] + 2) };
		case 4:
			return { num[0] - 1, g - 2, 1 };
		case 5:
			return { g - 1, num[2] + 1, det(g, num[len - 1] - num[2]) };
		case 6:
			return { g - 1, num[2] + 2, g - 1 };
		default:
			return { 0, 0, 0 };
		}
	}
	if (fst == 2)
	{
		switch(snd)
		{
		case 1:
			return { num[1], num[2] - 1, det(g, num[len - 1] - num[2]) };
		case 2:
			return { num[1], num[2] - 2, 1 };
		case 3:
			return { num[1] - 1, g - 2, 1 };
		case 4:
			return { num[1], 1, g - 2 };
		case 5:
			return { num[1] - 1, g - 1, num[len - 1] };
		case 6:
			return { num[1], 2, det(g, num[len - 1] - 3) };
		case 7:
			return { num[1], 1, 1 };
		default:
			return { 0, 0, 0 };
		}
	}

	return { 0, 0, 0 };
}

void algorithm_1(const int g, const size_t size, vector<int>& num, const int type, vector<int>& x, vector<int>& y, vector<int>& z)
{
	const size_t m = (size - 1) / 2;
	const size_t factor = size - 2 * m - 1;
	x.resize(2 * m + 1); y.resize(2 * m); z.resize(2 * m - 1);

	// Init value
	auto [x0, y0, z0] = get_init({ 1, type }, g, size, num);
	x[0] = x[2 * m] = x0;
	y[0] = y[2 * m - 1] = y0;
	z[0] = z[2 * m - 2] = z0;
	int c = (x[0] + y[0] + z[0]) / g;

	// Temporary solution
	for (size_t i = 1; i < m; i++)
	{
		if (i == 1)
			x[i] = x[2 * m - i] = det(g, num[factor + i] - y[i - 1] - (z[i - 1] >= num[factor + i + 1] ? 1 : 0));
		else
			x[i] = x[2 * m - i] = z[i - 1] >= num[factor + i + 1] ? 0 : 1;

		y[i] = y[2 * m - i - 1] = det(g, num[factor + i + 1] - z[i - 1] - 1);
		z[i] = z[2 * m - i - 2] = det(g, num[size - 1 - i] - x[i] - y[i] - c);
		c = (x[i] + y[i] + z[i] + c - num[size - 1 - i]) / g;
	}

	// Adjustment step
	if (c == 0)
		x[m] = 1;
	else if (c == 2)
	{
		x[m] = 1;
		y[m] -= 1; y[m - 1] -= 1;
		z[m - 1] = 0;
	}
}

void algorithm_2(const int g, const size_t size, vector<int>& num, const int type, vector<int>& x, vector<int>& y, vector<int>& z)
{
	const size_t m = size / 2;
	const size_t factor = size - 2 * m;
	x.resize(2 * m); y.resize(2 * m - 1); z.resize(2 * m - 2);

	// Init value
	auto [x0, y0, z0] = get_init({ 1, type }, g, size, num);
	x[0] = x[2 * m - 1] = x0;
	y[0] = y[2 * m - 2] = y0;
	z[0] = z[2 * m - 3] = z0;
	int c = (x[0] + y[0] + z[0]) / g;

	// Temporary solution
	for (size_t i = 1; i < m - 1; i++)
	{
		if (i == 1)
			x[i] = x[2 * m - i - 1] = det(g, num[factor + i] - y[i - 1] - (z[i - 1] >= num[factor + i + 1] ? 1 : 0));
		else
			x[i] = x[2 * m - i - 1] = z[i - 1] >= num[factor + i + 1] ? 0 : 1;

		y[i] = y[2 * m - i - 2] = det(g, num[factor + i + 1] - z[i - 1] - 1);
		z[i] = z[2 * m - i - 3] = det(g, num[size - 1 - i] - x[i] - y[i] - c);
		c = (x[i] + y[i] + z[i] + c - num[size - 1 - i]) / g;
	}
	y[m - 1] = det(g, num[size - m] - z[m - 2] - c);
	c = (x[m - 1] + y[m - 1] + z[m - 1] + c - num[size - m]) / g;

	// Adjustment step
	if (c == 0)
	{
		if (y[m - 1] != 0)
		{
			x[m - 1] = x[m] = 1;
			y[m - 1] -= 1;
		}
		else
		{
			if (y[m - 2] != 0)
			{
				x[m - 1] = x[m] = 1;
				y[m - 1] = g - 2;
				y[m - 2] -= 1; y[m] -= 1;
				z[m - 2] += 1; z[m - 1] += 1;
			}
			else if (z[m - 2] != 0)
			{
				y[m - 2] = y[m - 1] = y[m] = 1;
				z[m - 2] += 1; z[m - 1] += 1;
			}
			else
			{
				x[m - 2] -= 1; x[m + 1] -= 1;
				x[m - 1] = x[m] = 1;
				y[m - 2] = y[m] = g - 1;
				y[m - 1] = g - 4;
				z[m - 4] = z[m + 1] = 0;
				z[m - 2] = z[m - 1] = 2;
			}
		}
	}
	else if (c == 2)
	{
		x[m - 1] = x[m] = 1;
		y[m - 1] = g - 2;
		y[m - 2] -= 1; y[m] -= 1;
		z[m - 2] = z[m - 1] = 0;
	}
}

void algorithm_3(const int g, const size_t size, vector<int>& num, const int type, vector<int>& x, vector<int>& y, vector<int>& z)
{
	const size_t m = (size - 1) / 2;
	x.resize(2 * m + 1); y.resize(2 * m - 1); z.resize(2 * m - 2);

	// Init value
	auto [x0, y0, z0] = get_init({ 2, type }, g, size, num);
	x[0] = x[2 * m] = 1;
	x[1] = x[2 * m - 1] = x0;
	y[0] = y[2 * m - 2] = y0;
	z[0] = z[2 * m - 3] = z0;
	int c = (x[0] + y[0] + z[0]) / g;

	// Temporary solution
	for (size_t i = 1; i < m - 1; i++)
	{
		if (i == 1)
			x[i + 1] = x[2 * m - i - 1] = det(g, num[2] - y[i - 1] - (z[i - 1] >= num[i + 2] ? 1 : 0));
		else
			x[i + 1] = x[2 * m - i - 1] = z[i - 1] >= num[i + 2] ? 0 : 1;

		y[i] = y[2 * m - i - 2] = det(g, num[i + 2] - z[i - 1] - 1);
		z[i] = z[2 * m - i - 3] = det(g, num[size - 1 - i] - x[i] - y[i] - c);
		c = (x[i] + y[i] + z[i] + c - num[size - 1 - i]) / g;
	}
	y[m - 1] = det(g, num[m + 1] - z[m - 2] - x[m - 1] - c);
	c = (x[m - 1] + y[m - 1] + z[m - 1] + c - num[m + 1]) / g;

	// Adjustment step
	if (c == 0)
		x[m] = 1;
	else if (c == 2)
	{
		if (y[m - 2] != 0 && z[m - 2] != g - 1)
		{
			y[m - 2] -= 1; y[m] -= 1;
			y[m - 1] -= 1;
			z[m - 2] += 1; z[m - 1] += 1;
		}
		else if (y[m - 2] != 0 && z[m - 2] == g - 1)
		{
			x[m] = 1;
			y[m - 2] -= 1; y[m] -= 1;
			z[m - 2] = z[m - 1] = 0;
		}
		else if (y[m - 2] == 0 && z[m - 2] != g - 1)
		{
			x[m - 1] -= 1; x[m + 1] -= 1;
			y[m - 2] = y[m] = g - 1;
			y[m - 1] -= 1;
			z[m - 2] += 1; z[m - 1] += 1;
		}
		else if (y[m - 2] == 0 && z[m - 2] == g - 1)
		{
			x[m - 1] -= 1; x[m + 1] -= 1;
			x[m] = 1;
			y[m - 2] = y[m] = g - 1;
			z[m - 2] = z[m - 1] = 0;
		}
	}
}

void algorithm_4(const int g, const size_t size, vector<int>& num, const int type, vector<int>& x, vector<int>& y, vector<int>& z)
{
	const size_t m = size / 2;
	x.resize(2 * m); y.resize(2 * m - 2); z.resize(2 * m - 3);

	// Init value
	auto [x0, y0, z0] = get_init({ 2, type }, g, size, num);
	x[0] = x[2 * m - 1] = 1;
	x[1] = x[2 * m - 2] = x0;
	y[0] = y[2 * m - 3] = y0;
	z[0] = z[2 * m - 4] = z0;
	int c = (x[0] + y[0] + z[0]) / g;

	// Temporary solution
	for (size_t i = 1; i < m - 2; i++)
	{
		if (i == 1)
			x[i + 1] = x[2 * m - i - 2] = det(g, num[2] - y[i - 1] - (z[i - 1] >= num[i + 2] ? 1 : 0));
		else
			x[i + 1] = x[2 * m - i - 2] = z[i - 1] >= num[i + 2] ? 0 : 1;

		y[i] = y[2 * m - i - 3] = det(g, num[i + 2] - z[i - 1] - 1);
		z[i] = z[2 * m - i - 4] = det(g, num[size - 1 - i] - x[i] - y[i] - c);
		c = (x[i] + y[i] + z[i] + c - num[size - 1 - i]) / g;
	}
	x[m - 1] = x[m] = z[m - 3] >= num[m] ? 0 : 1;
	y[m - 2] = y[m - 1] = det(g, num[m] - z[m - 3] - 1);
	z[m - 2] = det(g, num[m + 1] - x[m - 2] - y[m - 2] - c);
	c = (x[m - 2] + y[m - 2] + z[m - 2] + c - num[m + 1]) / g;

	// Adjustment step
	if (x[m - 1] + c == 0 && y[m - 2] != g - 1)
	{
		if (z[m - 2] != 0)
		{
			y[m - 2] += 1; y[m - 1] += 1;
			z[m - 2] -= 1;
		}
		else if (z[m - 2] == 0 && y[m - 3] != 0)
		{
			if (y[m - 2] != 1 && z[m - 3] != g - 1)
			{
				x[m - 1] = x[m] = 1;
				y[m - 3] -= 1; y[m] -= 1;
				y[m - 2] -= 1; y[m - 1] -= 1;
				z[m - 3] += 1; z[m - 1] += 1;
				z[m - 2] = 1;
			}
			else if (y[m - 2] != 1 && z[m - 3] == g - 1)
			{
				x[m - 1] = x[m] = 2;
				y[m - 3] -= 1; y[m] -= 1;
				y[m - 2] -= 2; y[m - 1] -= 2;
				z[m - 3] = z[m - 1] = 0;
				z[m - 2] = g - 3;
			}
			else if(y[m - 2] == 1)
			{
				x[m - 1] = x[m] = 1;
				y[m - 3] -= 1; y[m] -= 1;
				y[m - 2] = y[m - 1] = g - 1;
				z[m - 3] = z[m - 1] = 0;
				z[m - 2] = 3;
			}
		}
		else if (z[m - 2] == 0 && y[m - 3] == 0)
		{
			if (z[m - 3] != g - 1)
			{
				x[m - 2] -= 1; x[m + 1] -= 1;
				x[m - 1] = x[m] = 1;
				y[m - 3] = y[m] = g - 1;
				y[m - 2] -= 1; y[m - 1] -= 1;
				z[m - 3] += 1; z[m - 1] += 1;
				z[m - 2] = 1;
			}
			else if (z[m - 3] == g - 1 && y[m - 2] != 1)
			{
				x[m - 2] -= 1; x[m + 1] -= 1;
				x[m - 1] = x[m] = 2;
				y[m - 3] = y[m] = g - 1;
				y[m - 2] -= 2; y[m - 1] -= 2;
				z[m - 3] = z[m - 1] = 0;
				z[m - 2] = 3;
			}
			else if (z[m - 3] == g - 1 && y[m - 2] == 1)
			{
				x[m - 2] -= 1; x[m + 1] -= 1;
				x[m - 1] = x[m] = 1;
				y[m - 3] = y[m] = g - 1;
				y[m - 2] = y[m - 1] = g - 1;
				z[m - 3] = z[m - 1] = 0;
				z[m - 2] = 3;
			}
		}
	}
	else if (x[m - 1] + c == 0 && y[m - 2] == g - 1)
	{
		x[m - 1] = x[m] = 1;
		y[m - 3] -= 1; y[m] -= 1;
		y[m - 2] = y[m - 1] = g - 2;
		z[m - 3] += 1; z[m - 1] += 1;
		z[m - 2] = 1;
	}
	else if (x[m - 1] == 0 && c == 2)
	{
		if (z[m - 2] != g - 1)
		{
			y[m - 2] -= 1; y[m - 1] -= 1;
			z[m - 2] += 1;
		}
		else if (z[m - 2] == g - 1 && z[m - 3] != g - 1)
		{
			if (y[m - 3] != 0)
			{
				x[m - 1] = x[m] = 1;
				y[m - 3] -= 1; y[m] -= 1;
				y[m - 2] -= 2; y[m - 1] -= 2;
				z[m - 3] += 1; z[m - 1] += 1;
				z[m - 2] = 1;
			}
			else if (y[m - 3] == 0)
			{
				x[m - 2] -= 1; x[m + 1] -= 1;
				x[m - 1] = x[m] = 1;
				y[m - 3] = y[m] = g - 1;
				y[m - 2] -= 2; y[m - 1] -= 2;
				z[m - 3] += 1; z[m - 1] += 1;
				z[m - 2] = 1;
			}
		}
		else if (z[m - 2] == g - 1 && z[m - 3] == g - 1)
		{
			if (y[m - 2] != g - 1 && y[m - 2] != g - 2)
			{
				if (y[m - 3] != g - 1)
				{
					x[m - 2] -= 1; x[m + 1] -= 1;
					x[m - 1] = x[m] = g - 2;
					y[m - 3] += 1; y[m] += 1;
					y[m - 2] += 2; y[m - 1] += 2;
					z[m - 3] = z[m - 1] = g - 2;
					z[m - 2] = g - 2;
				}
				else if (y[m - 3] == g - 1)
				{
					x[m - 1] = x[m] = g - 2;
					y[m - 3] = y[m] = 0;
					y[m - 2] += 2; y[m - 1] += 2;
					z[m - 3] = z[m - 1] = g - 2;
					z[m - 2] = g - 2;
				}
			}
			else if (y[m - 2] == g - 1 || y[m - 2] == g - 2)
			{
				if (y[m - 3] >= 1)
				{
					x[m - 1] = x[m] = 2;
					y[m - 3] -= 1; y[m] -= 1;
					y[m - 2] -= 3; y[m - 1] -= 3;
					z[m - 3] = z[m - 1] = 0;
					z[m - 2] = 3;
				}
				else if (y[m - 3] == 0 && x[m - 2] >= 1)
				{
					x[m - 2] -= 1; x[m + 1] -= 1;
					x[m - 1] = x[m] = 2;
					y[m - 3] = y[m] = g - 1;
					y[m - 2] -= 3; y[m - 1] -= 3;
					z[m - 3] = z[m - 1] = 0;
					z[m - 2] = 3;
				}
			}
		}
	}
	else if (x[m - 1] == 1 && c == 1)
	{
		if (z[m - 2] != g - 1 && y[m - 2] != 0)
		{
			y[m - 2] -= 1; y[m - 1] -= 1;
			z[m - 2] += 1;
		}
		else if (z[m - 2] != g - 1 && y[m - 2] == 0)
		{
			x[m - 1] = x[m] = 0;
			y[m - 2] = y[m - 1] = g - 1;
			z[m - 2] += 1;
		}
		else if (z[m - 2] == g - 1 && z[m - 3] != 0)
		{
			if (y[m - 3] != g - 1)
			{
				x[m - 1] = x[m] = 0;
				y[m - 3] += 1; y[m] += 1;
				y[m - 2] += 1; y[m - 1] += 1;
				z[m - 3] -= 1; z[m - 1] -= 1;
				z[m - 2] = g - 2;
			}
			else if (y[m - 3] == g - 1 && y[m - 2] > 1)
			{
				x[m - 1] = x[m] = 2;
				y[m - 3] = y[m] = g - 2;
				y[m - 2] -= 2; y[m - 1] -= 2;
				z[m - 3] += 1; z[m - 1] += 1;
				z[m - 2] = 1;
			}
			else if (y[m - 3] == g - 1 && y[m - 2] == 0)
			{
				y[m - 3] = y[m] = g - 2;
				y[m - 2] = y[m - 1] = g - 2;
				z[m - 3] += 1; z[m - 1] += 1;
				z[m - 2] = 1;
			}
			else if (y[m - 3] == g - 1 && y[m - 2] == 1)
			{
				y[m - 3] = y[m] = g - 2;
				y[m - 2] = y[m - 1] = g - 1;
				z[m - 3] += 1; z[m - 1] += 1;
				z[m - 2] = 1;
			}
		}
		else if (z[m - 2] == g - 1 && z[m - 3] == 0 && y[m - 3] != 0)
		{
			if (y[m - 2] > 1)
			{
				x[m - 1] = x[m] = 2;
				y[m - 3] -= 1; y[m] -= 1;
				y[m - 2] -= 2; y[m - 1] -= 2;
				z[m - 3] = z[m - 1] = 1;
				z[m - 2] = 1;
			}
			else if (y[m - 2] == 0)
			{
				y[m - 3] -= 1; y[m] -= 1;
				y[m - 2] = y[m - 1] = g - 2;
				z[m - 3] = z[m - 1] = 1;
				z[m - 2] = 1;
			}
			else if (y[m - 2] == 1)
			{
				y[m - 3] -= 1; y[m] -= 1;
				y[m - 2] = y[m - 1] = g - 1;
				z[m - 3] = z[m - 1] = 1;
				z[m - 2] = 1;
			}
		}
		else if (z[m - 2] == g - 1 && z[m - 3] == 0 && y[m - 3] == 0)
		{
			if (y[m - 2] > 1)
			{
				x[m - 2] -= 1; x[m + 1] -= 1;
				x[m - 1] = x[m] = 2;
				y[m - 3] = y[m] = g - 1;
				y[m - 2] -= 2; y[m - 1] -= 2;
				z[m - 3] = z[m - 1] = 1;
				z[m - 2] = 1;
			}
			else if (y[m - 2] == 0)
			{
				x[m - 2] -= 1; x[m + 1] -= 1;
				y[m - 3] = y[m] = g - 1;
				y[m - 2] = y[m - 1] = g - 2;
				z[m - 3] = z[m - 1] = 1;
				z[m - 2] = 1;
			}
			else if (y[m - 2] == 1)
			{
				x[m - 2] -= 1; x[m + 1] -= 1;
				y[m - 3] = y[m] = g - 1;
				y[m - 2] = y[m - 1] = g - 1;
				z[m - 3] = z[m - 1] = 1;
				z[m - 2] = 1;
			}
		}
	}
	else if(x[m - 1] == 1 && c == 2)
	{
		y[m - 2] -= 1; y[m - 1] -= 1;
		z[m - 2] = 0;
	}
}

void algorithm_5(const int g, size_t size, vector<int>& num, vector<int>& x, vector<int>& y, vector<int>& z)
{
	size_t m = size / 2;
	int n = 0;

	auto subtract = [&]
	{
		num[size - m] -= 1;
		num[size - m - 1] -= 1;
		for (int i = static_cast<int>(size - m); i > 0; i--)
		{
			if (num[i] < 0)
			{
				num[i - 1] -= 1;
				num[i] += g;
			}
		}

		if (num[0] == 0)
		{
			num.erase(num.begin());
			size -= 1;
			m = size / 2;
		}
		n++;
	};

	subtract();
	if (num[size - m - 1] == 0 || num[size - m] == 0)
		subtract();
	
	if (num[0] == 1 && num[1] == 0 && num[2] == 3)
	{
		const int type = det(g, num[size - 1] - num[2]) == 0 ? 2 : 1;
		algorithm_4(g, size, num, type, x, y, z);
	}
	else
	{
		if (const auto [fst, snd] = get_type(g, size, num); fst == 1)
			algorithm_2(g, size, num, snd, x, y, z);
		else if (fst == 2)
			algorithm_4(g, size, num, snd, x, y, z);
		else if (fst == 4)
			algorithm_6(g, size, num, snd, x, y, z);
	}

	const size_t p1 = x.size();
	x[p1 - m] += n; x[p1 - m - 1] += n;
}

void algorithm_6(const int g, const size_t size, vector<int>& num, int type, vector<int>& x, vector<int>& y, vector<int>& z)
{
	auto digit_1 = [](const vector<int>& number, vector<int>& a, vector<int>& b, vector<int>& c)
	{
		a.resize(1); b.resize(1); c.resize(1);
		a[0] = number[0];
	};

	auto digit_2 = [g](const vector<int>& number, vector<int>& a, vector<int>& b, vector<int>& c)
	{
		a.resize(2); b.resize(1); c.resize(1);
		if (number[0] <= number[1])
		{
			a[0] = a[1] = number[0];
			b[0] = number[1] - number[0];
		}
		else if (number[0] == number[1] + 1)
		{
			if (number[1] == 0)
			{
				a.resize(1);
				a[0] = g - 1;
				b[0] = 1;
			}
			else
			{
				a[0] = a[1] = number[1];
				b[0] = g - 1;
				c[0] = 1;

			}
		}
		else if (number[0] > number[1] + 1)
		{
			a[0] = a[1] = number[0] - 1;
			b[0] = g + number[1] - number[0] + 1;
		}
	};

	auto digit_3 = [g](const vector<int>& number, vector<int>& a, vector<int>& b, vector<int>& c)
	{
		a.resize(3); b.resize(1); c.resize(1);
		if (number[0] <= number[2])
		{
			a[0] = a[2] = number[0];
			a[1] = number[1];
			b[0] = number[2] - number[0];
		}
		else if (number[0] >= number[2] + 1 && number[1] != 0)
		{
			a[0] = a[2] = number[0];
			a[1] = number[1] - 1;
			b[0] = g + number[2] - number[0];
		}
		else if (number[0] >= number[2] + 1 && number[1] == 0 && det(g, number[0] - number[2] - 1) != 0)
		{
			a[0] = a[2] = number[0] - 1;
			a[1] = g - 1;
			b[0] = g + number[2] - number[0] + 1;
		}
		else if (number[0] == number[2] + 1 && number[1] == 0)
		{
			if (number[0] >= 3)
			{
				b.resize(3);
				a[0] = a[2] = number[0] - 2;
				a[1] = g - 1;
				b[0] = b[1] = b[2] = 1;
			}
			else if (number[0] == 2)
			{
				b.resize(2);
				a[0] = a[2] = 1;
				a[1] = 0;
				b[0] = b[1] = g - 1;
				c[0] = 1;
			}
			else if (number[0] == 1)
			{
				a.resize(2);
				a[0] = a[1] = g - 1;
				b[0] = 1;
			}
		}
	};

	auto digit_4 = [g, &digit_1, &digit_2, &digit_3](const vector<int>& number, vector<int>& a, vector<int>& b, vector<int>& c)
	{
		if (number[0] == 1 && number[1] == 0 && number[2] == 0 && number[3] == 0)
		{
			a.resize(3); b.resize(1); c.resize(1);
			a[0] = a[1] = a[2] = g - 1;
			b[0] = 1;
		}
		else if (number[1] == 0 && number[2] == 0 && number[3] <= number[0] - 1 && number[0] != 1)
		{
			a.resize(4); b.resize(1); c.resize(1);
			a[0] = a[3] = number[0] - 1;
			a[1] = a[2] = g - 1;
			b[0] = g + number[3] - number[0];
			c[0] = 1;
		}
		else
		{
			vector remain = { number[1], number[2], number[3] - number[0] };
			for (int i = 2; i > 0; i--)
			{
				if (remain[i] < 0)
				{
					remain[i - 1] -= 1;
					remain[i] += g;
				}
			}

			if (remain[0] == 0)
				remain.erase(remain.begin());
			if (remain[0] == 0)
				remain.erase(remain.begin());

			if (const size_t remain_len = remain.size(); remain_len == 3 && remain[0] == 2 && remain[1] == 0 && remain[2] == 1)
			{
				a.resize(4); b.resize(2); c.resize(1);
				if (number[0] == 1)
				{
					a[0] = a[1] = a[2] = a[3] = 1;
					b[0] = b[1] = g - 2;
					c[0] = 3;
				}
				else if (number[0] == g - 1)
				{
					a[0] = a[3] = g - 1;
					a[1] = a[2] = 1;
					b[0] = b[1] = g - 2;
					c[0] = 3;
				}
				else
				{
					b.resize(3);
					a[0] = a[3] = number[0] - 1;
					a[1] = a[2] = g - 1;
					b[0] = b[2] = 2;
					b[1] = 1;
				}
			}
			else if (remain_len == 2 && remain[0] == remain[1] + 1 && remain[1] >= 1)
			{
				if (number[0] + remain[1] == number[3] && number[0] == 1)
				{
					a.resize(3); b.resize(2); c.resize(1);
					a[0] = a[1] = a[2] = g - 1;
					b[0] = b[1] = remain[1] + 1;
					c[0] = 1;
				}
				else
				{
					a.resize(4); b.resize(3); c.resize(2);
					a[0] = a[3] = number[0] - 1;
					a[1] = a[2] = g - 1;
					b[0] = b[2] = 1;
					b[1] = 3;
					c[0] = c[1] = remain[1];
				}
			}
			else
			{
				if (remain_len == 1)
					digit_1(remain, b, c, a);
				else if (remain_len == 2)
					digit_2(remain, b, c, a);
				else if (remain_len == 3)
					digit_3(remain, b, c, a);

				a.resize(4);
				a[0] = a[3] = number[0];
				a[1] = a[2] = 0;
			}
		}
	};

	auto digit_5 = [g, type, &digit_1, &digit_2, &digit_3](vector<int>& number, vector<int>& a, vector<int>& b, vector<int>& c)
	{
		if (number[0] == 1)
		{
			if (number[1] == 0 && number[2] == 0 && number[3] == 0 && number[4] == 0)
			{
				a.resize(4); b.resize(1); c.resize(1);
				a[0] = a[1] = a[2] = a[3] = g - 1;
				b[0] = 1;
			}
			else
			{
				vector remain = { number[2], number[3] - number[1], number[4] - 1 };
				for (int i = 2; i > 0; i--)
				{
					if (remain[i] < 0)
					{
						remain[i - 1] -= 1;
						remain[i] += g;
					}
				}

				if (remain[0] == 0)
					remain.erase(remain.begin());
				if (remain[0] == 0)
					remain.erase(remain.begin());

				if (remain[0] < 0)
				{
					remain[0] += 1; remain[1] += 1;
					if (remain[1] > g - 1)
					{
						remain[0] += 1;
						remain[1] -= g;
					}

					if (remain[0] == 0)
						remain.erase(remain.begin());
					if (remain[0] == 0)
						remain.erase(remain.begin());

					if (remain.size() == 2 && remain[0] == remain[1] + 1)
					{
						a.resize(5); b.resize(3); c.resize(1);
						a[0] = a[4] = 1;
						a[1] = a[3] = number[1] - 1;
						a[2] = g - 2;
						b[0] = b[2] = 1;
						b[1] = remain[0];
						c[0] = remain[1] - 1;
					}
					else
					{
						if (remain.size() == 2)
							digit_2(remain, b, c, a);
						else // if (remain.size() == 3)
							digit_3(remain, b, c, a);

						a.resize(5);
						a[0] = a[4] = 1;
						a[1] = a[3] = number[1] - 1;
						a[2] = g - 1;
					}
				}
				else
				{
					if(const size_t remain_len = remain.size(); remain_len == 3 && remain[0] == 2 && remain[1] == 0 && remain[2] == 1)
					{
						a.resize(5); b.resize(3); c.resize(1);
						a[0] = a[2] = a[4] = 1;
						a[1] = a[3] = number[1];
						b[0] = b[2] = 1;
						b[1] = 0;
					}
					else if (remain_len == 2 && remain[0] == remain[1] + 1 && remain[1] >= 1)
					{
						if (number[1] == 0)
						{
							a.resize(4); b.resize(2); c.resize(1);
							a[0] = a[1] = a[2] = a[3] = g - 1;
							b[0] = b[1] = remain[1] + 1;
							c[0] = 1;
						}
						else
						{
							a.resize(5); b.resize(3); c.resize(1);
							a[0] = a[2] = a[4] = 1;
							a[1] = a[3] = number[1] - 1;
							b[0] = b[2] = g - 1;
							b[1] = remain[0];
							c[0] = remain[0];
						}
					}
					else
					{
						if (remain_len == 1)
							digit_1(remain, b, c, a);
						else if (remain_len == 2)
							digit_2(remain, b, c, a);
						else if (remain_len == 3)
							digit_3(remain, b, c, a);

						a.resize(5);
						a[0] = a[4] = 1;
						a[1] = a[3] = number[1];
						a[2] = 0;
					}
				}
			}
		}
		else
			// Use algorithm 1.
			algorithm_1(g, 5, number, type, a, b, c);		
	};

	auto digit_6 = [g, type, &digit_2, &digit_3](vector<int>& number, vector<int>& a, vector<int>& b, vector<int>& c)
	{
		if (number[0] == 1)
		{
			int r, temp;
			if (det(g, number[5] - number[1] + 1) != 0 && det(g, number[5] - number[1] + 2) != 0)
			{
				a.resize(5); b.resize(5); c.resize(3);

				temp = g + number[1] - 1;
				c[0] = c[2] = det(g, number[5] - number[1] + 1);
				b[0] = b[4] = temp / 2;
				a[0] = a[4] = temp - b[0];
				r = (a[0] + b[0] + c[0] - number[5]) / g;

				temp = g + number[2] - 1;
				a[1] = a[3] = temp / 2;
				b[1] = b[3] = temp - a[1];
				c[1] = det(g, number[4] - a[1] - b[1] - r);
				r = (a[1] + b[1] + c[1] + r - number[4]) / g;

				temp = g + number[3] - r - c[0];
				a[2] = temp / 2;
				b[2] = temp - a[2];
			}
			else if (det(g, number[5] - number[1] + 2) == 0 && number[3] != 0)
			{
				a.resize(5); b.resize(5); c.resize(3);

				temp = g + number[1] - 1;
				a[0] = a[4] = temp / 2;
				b[0] = b[4] = temp - a[0];
				c[0] = c[2] = g - 1;
				r = (a[0] + b[0] + c[0] - number[5]) / g;

				temp = g + number[2] - 1;
				a[1] = a[3] = temp / 2;
				b[1] = b[3] = temp - a[1];
				c[1] = det(g, number[4] - a[1] - b[1] - r);
				r = (a[1] + b[1] + c[1] + r - number[4]) / g;

				temp = 1 + number[3] - r;
				a[2] = temp / 2;
				b[2] = temp - a[2];
			}
			else if (det(g, number[5] - number[1]+ 2) == 0 && number[3] == 0)
			{
				if (number[1] < 2)
				{
					a.resize(5); b.resize(5); c.resize(4);
					a[0] = a[4] = g - 2 - number[1];
					b[0] = b[4] = 1;
					c[0] = c[3] = g - 1;

					a[1] = a[3] = number[2] / 2;
					b[1] = b[3] = number[2] - a[1];
					c[1] = c[2] = det(g, number[4] - a[1] - b[1] - 1);
					r = (a[1] + b[1] + c[1] + 1 - number[4]) / g;

					temp = g - c[1] - r;
					a[2] = temp / 2;
					b[2] = temp - a[2];
				}
				else if (number[1] == 2)
				{
					a.resize(5); b.resize(5); c.resize(4);
					a[0] = a[4] = g - 1;
					b[0] = b[4] = 2;
					c[0] = c[3] = g - 1;

					a[1] = a[3] = number[2] / 2;
					b[1] = b[3] = number[2] - a[1];
					c[1] = c[2] = det(g, number[4] - a[1] - b[1] - 2);
					r = (a[1] + b[1] + c[1] + 2 - number[4]) / g;

					temp = g - c[1] - r;
					if (r != 2)
					{
						a[2] = temp / 2;
						b[2] = temp - a[2];
					}
					else
					{
						a.resize(6); b.resize(3); c.resize(1);
						a[0] = a[5] = 1;
						a[1] = a[4] = 2;
						a[2] = a[3] = g - 2;
						b[0] = b[2] = 1;
						b[1] = g - 3;
						c[0] = g - 2;
					}
				}
				else // if (number[1] > 2)
				{
					a.resize(6); b.resize(5); c.resize(3);
					a[0] = a[5] = 1;
					b[0] = b[4] = number[1] - 1;
					c[0] = c[2] = g - 2;

					b[1] = b[3] = det(g, number[2] - 1);
					temp = (b[1] + 1 - number[2]) / g;
					a[1] = a[4] = 1 - temp;

					c[1] = det(g, number[4] - number[2] - 1 + temp);
					b[2] = 2 - (2 - temp + b[1] + c[1] - number[4]) / g;
				}
			}
			else if (det(g, number[5] - number[1] + 1) == 0 && number[2] != 0)
			{
				if (number[1] != g - 1)
				{
					a.resize(5); b.resize(5); c.resize(3);
					temp = g + number[1];
					a[0] = a[4] = temp / 2;
					b[0] = b[4] = temp - a[0];
					c[0] = c[2] = g - 1;
					r = (a[0] + b[0] + c[0] - number[5]) / g;

					temp = number[2] - 1;
					a[1] = a[3] = temp / 2;
					b[1] = b[3] = temp - a[1];
					c[1] = det(g, number[4] - a[1] - b[1] - r);
					r = (a[1] + b[1] + c[1] + r - number[4]) / g;

					temp = 1 + number[3] - r;
					a[2] = temp / 2;
					b[2] = temp - a[2];
				}
				else // if (number[1] == g - 1)
				{
					int u = 0, x1 = 0, y1;
					for (y1 = 1; y1 <= 3; y1++)
					{
						if (const int t = det(g, number[4] - 3 - y1); t != g - 2 && t != g - 1)
						{
							x1 = det(g, number[2] - y1);
							break;
						}
					}
					
					const int c1 = (3 + y1 + det(g, number[4] - 3 - y1) - number[4]) / g;
					int c2 = (x1 + det(g, number[3] - x1 - 1 - c1 + u) + c1 + 1 - number[3]) / g;
					if (c2 == 2)
					{
						u = 1;
						c2 = 1;
					}
					const int c3 = (x1 + y1 - number[2]) / g;

					a.resize(6); b.resize(5); c.resize(3);
					a[0] = a[5] = 1;
					a[1] = a[4] = 3 - c3;
					a[2] = a[3] = x1 - u;
					b[0] = b[4] = g - 4;
					b[1] = b[3] = y1 - c2 + u;
					b[2] = det(g, number[3] - x1 - 1 - c1 + u);
					c[0] = c[2] = 1;
					c[1] = det(g, number[4] - 3 - y1) + c2 - u + c3;
				}
			}
			else if (det(g, number[5] - number[1] + 1) == 0 && number[2] == 0)
			{
				if (number[1] == 0)
				{
					if (number[3] != 0)
					{
						const vector remain = { number[3], number[4], g - 2 };
						digit_3(remain, b, c, a);
						a.resize(6);
						a[0] = a[5] = 1;
						a[1] = a[2] = a[3] = a[4] = 0;
					}
					else if (number[3] == 0 && number[4] != 0 && number[4] != g - 1)
					{
						const vector remain = { number[4], g - 2 };
						digit_2(remain, b, c, a);
						a.resize(6);
						a[0] = a[5] = 1;
						a[1] = a[2] = a[3] = a[4] = 0;
					}
					else if (number[3] == 0 && number[4] == 0)
					{
						a.resize(6); b.resize(1); c.resize(1);
						a[0] = a[5] = 1;
						a[1] = a[2] = a[3] = a[4] = 0;
						b[0] = g - 2;
					}
					else if (number[3] == 0 && number[4] == g - 1)
					{
						a.resize(5); b.resize(4); c.resize(3);
						a[0] = a[4] = g - 1;
						a[2] = 1;
						b[0] = b[3] = g - 1;
						b[1] = b[2] = g - 2;
						c[0] = c[2] = 1;						
					}
				}
				else if (number[1] == 1)
				{
					if (number[3] > 1 || (number[3] == 1 && number[4] > 1))
					{
						vector remain = { number[3], number[4] - 1, number[5] - 1 };
						if (remain[2] < 0)
						{
							remain[1] -= 1;
							remain[2] += g;
						}

						digit_2(remain, b, c, a);
						a.resize(6);
						a[0] = a[1] = a[4] = a[5] = 1;						
					}
					else if (number[3] == 1 && number[4] == 0)
					{
						a.resize(6); b.resize(3); c.resize(1);
						a[0] = a[5] = 1;
						a[2] = a[3] = g - 1;
						b[0] = b[2] = 1;
						b[1] = g - 1;
						c[0] = g - 2;
					}
					else if (number[3] == 1 && number[4] == 1)
					{
						a.resize(6); b.resize(2); c.resize(1);
						a[0] = a[1] = a[4] = a[5] = 1;
						b[0] = b[1] = g - 1;
					}
					else if (number[3] == 0 && number[4] > 1)
					{
						a.resize(6); b.resize(2); c.resize(1);
						a[0] = a[1] = a[4] = a[5] = 1;
						b[0] = b[1] = number[4] - 2;
						c[0] = g - number[4] + 1;
					}
					else if (number[3] == 0 && number[4] == 1)
					{
						a.resize(6); b.resize(5); c.resize(1);
						a[0] = a[5] = 1;
						b[0] = b[4] = 1;
						c[0] = g - 2;
					}
					else if (number[3] == 0 && number[4] == 0)
					{
						a.resize(6); b.resize(4); c.resize(1);
						a[0] = a[5] = 1;
						b[0] = b[1] = b[2] = b[3] = g - 1;
					}
				}
				else if (number[1] == 2)
				{
					if (number[3] > 1 || (number[3] == 1 && number[4] > 1))
					{
						vector remain = { number[3], number[4] - 2, number[5] - 1 };
						if (remain[2] < 0)
						{
							remain[1] -= 1;
							remain[2] += g;
						}

						digit_2(remain, b, c, a);
						a.resize(6);
						a[0] = a[1] = a[4] = a[5] = 1;
					}
					else if (number[3] == 1 && number[4] == 0)
					{
						a.resize(6); b.resize(3); c.resize(1);
						a[0] = a[1] = a[4] = a[5] = 1;
						a[2] = a[3] = g - 1;
						b[0] = b[2] = 1;
						b[1] = g - 2;
						c[0] = g - 1;
					}
					else if (number[3] == 1 && number[4] == 1)
					{
						a.resize(6); b.resize(3); c.resize(1);
						a[0] = a[1] = a[4] = a[5] = 1;
						a[2] = a[3] = g - 1;
						b[0] = b[2] = 1;
						b[1] = g - 1;
						c[0] = g - 1;
					}
					else if (number[3] == 0 && number[4] > 3)
					{
						a.resize(6); b.resize(2); c.resize(1);
						a[0] = a[5] = 1;
						a[1] = a[4] = 2;
						b[0] = b[1] = number[4] - 3;
						c[0] = g - number[4] + 3;
					}
					else if (number[3] == 0 && number[4] == 3)
					{
						a.resize(6); b.resize(1); c.resize(1);
						a[0] = a[5] = 1;
						a[1] = a[4] = 2;
						b[0] = g - 1;
						c[0] = 1;
					}
					else if (number[3] == 0 && number[4] == 2)
					{
						a.resize(6); b.resize(3); c.resize(1);
						a[0] = a[1] = a[4] = a[5] = 1;
						a[2] = a[3] = g - 1;
						b[0] = b[2] = 1;
						c[0] = g - 1;
					}
					else if (number[3] == 0 && number[4] == 1)
					{
						a.resize(6); b.resize(5); c.resize(1);
						a[0] = a[5] = 1;
						b[0] = b[4] = 2;
						c[0] = g - 2;
					}
					else if (number[3] == 0 && number[4] == 0)
					{
						a.resize(6); b.resize(2); c.resize(1);
						a[0] = a[1] = a[4] = a[5] = 1;
						a[2] = a[3] = g - 1;
						b[0] = b[1] = g - 2;
						c[0] = 2;
					}
				}
				else if (number[1] == 3)
				{
					int y1;
					for (y1 = 1; y1 <= 3; y1++)
					{
						if (const int t = det(g, number[4] - 1 - y1); t != 0 && t != g - 1)
							break;
					}

					const int c1 = (2 + y1 + det(g, number[4] - 1 - y1) - number[4]) / g;
					const int c2 = (g - y1 - 1 + det(g, number[3] + y1 + 2) + g - 1 - number[3]) / g;
					
					a.resize(6); b.resize(5); c.resize(3);
					a[0] = a[5] = 1;
					a[2] = a[3] = g - y1 - 1 - c1;
					b[0] = b[4] = 2;
					b[1] = b[3] = y1 - c2 + 1 + c1;
					b[2] = det(g, number[3] + y1 + 2);
					c[0] = c[2] = g - 1;
					c[1] = det(g, number[4] - 1 - y1) + (c2 - 1) - c1;					
				}
				else if (number[1] >= 4)
				{
					int y1;
					for (y1 = 1; y1 <= 3; y1++)
					{
						if (const int t = det(g, number[4] - 1 - y1); t != 0 && t != g - 1)
							break;
					}

					const int c1 = (1 + y1 + det(g, number[4] - 1 - y1) - number[4]) / g;
					const int c2 = (g - y1 + 1 + det(g, number[3] + y1 - 1) - number[3]) / g;

					a.resize(6); b.resize(5); c.resize(3);
					a[0] = a[5] = 1;
					a[1] = a[4] = 2;
					a[2] = a[3] = g - y1 - c1;
					b[0] = b[4] = number[1] - 3;
					b[1] = b[3] = y1 - c2 + c1;
					b[2] = det(g, number[3] + y1 - 1);
					c[0] = c[2] = 1;
					c[1] = det(g, number[4] - 2 - y1) + c2 - c1;
				}
			}
		}
		else
		{
			// Use algorithm 2, with different adjustment.
			a.resize(6); b.resize(5); c.resize(4);

			// Init value
			auto [x0, y0, z0] = get_init({ 1, type }, g, 6, number);
			a[0] = a[5] = x0;
			b[0] = b[4] = y0;
			c[0] = c[3] = z0;
			int r = (a[0] + b[0] + c[0]) / g;

			// Temporary solution
			a[1] = a[4] = det(g, number[1] - b[0] - (c[0] >= number[2] ? 1 : 0));
			b[1] = b[3] = det(g, number[2] - c[0] - 1);
			c[1] = c[2] = det(g, number[4] - a[1] - b[1] - r);
			r = (a[1] + b[1] + c[1] + r - number[4]) / g;

			b[2] = det(g, number[3] - c[1] - r);

			// Adjustment step, slightly different
			if (const int r2 = (a[2] + b[2] + c[2] + r - number[3]) / g; r2 == 0)
			{
				if (b[2] != 0)
				{
					a[2] = a[3] = 1;
					b[2] -= 1;
				}
				else
				{
					if (b[1] != 0)
					{
						a[2] = a[3] = 1;
						b[1] -= 1; b[3] -= 1;
						b[2] = g - 2;
						c[1] += 1; c[2] += 1;
					}
					else if (c[1] != 0)
					{
						b[1] = b[2] = b[3] = 1;
						c[1] += 1; c[2] += 1;
					}
					else
					{
						if (number[3] != 0)
						{
							a[1] -= 1; a[4] -= 1;
							a[2] = a[3] = 1;
							b[1] = b[3] = g - 1;
							b[2] = g - 4;
							c[1] = c[2] = 2;
						}
						else
						{
							if (a[1] != 0)
							{
								a[1] -= 1; a[4] -= 1;
								a[2] = a[3] = g - 1;
								b[1] = b[2] = b[3] = 1;
							}
							else
							{
								if (a[0] == 1)
								{
									b.resize(2); c.resize(1);
									a[0] = a[5] = 2;
									b[0] = b[1] = 1;
									c[0] = g - 4;
								}
								else if (a[0] != 1 && b[0] != g - 1)
								{
									a[0] -= 1; a[5] -= 1;
									a[1] = a[4] = g - 1;
									b[0] += 1; b[4] += 1;
									b[2] = g - 2;
									c[1] = c[2] = 1;
								}
								else if (a[0] != g - 1 && b[0] == g - 1 && c[0] == g - 1)
								{
									b.resize(2); c.resize(1);
									a[0] += 1; a[5] += 1;
									b[0] = b[1] = 1;
									c[0] = g - 4;
								}
							}
						}
					}
				}
			}
			else if (r2 == 2)
			{
				a[2] = a[3] = 1;
				b[1] -= 1; b[3] -= 1;
				b[2] = g - 2;
				c[1] = c[2] = 0;
			}			
		}
	};

	switch(size)
	{
	case 1:
		digit_1(num, x, y, z);
		return;
	case 2:
		digit_2(num, x, y, z);
		return;
	case 3:
		digit_3(num, x, y, z);
		return;
	case 4:
		digit_4(num, x, y, z);
		return;
	case 5:
		digit_5(num, x, y, z);
		return;
	case 6:
		digit_6(num, x, y, z);
		return;
	default:
		return;
	}
}

void solve()
{
	int g;
	string n;
	cin >> g >> n;

	const size_t size = n.length();

	vector<int> num(size), x, y, z;
	for (size_t i = 0; i < size; i++)
		num[i] = change(n[i]);

	if (const auto [fst, snd] = get_type(g, size, num); fst == 1)
	{
		const size_t m = size / 2;
		const bool flag = num[size - m - 1] != 0 && num[size - m - 2] != 0;
		if (size % 2 == 1)
		{
			if (snd >= 1 && snd <= 4)
				algorithm_1(g, size, num, snd, x, y, z);
			if (flag && (snd == 5 || snd == 6))
				algorithm_2(g, size, num, snd, x, y, z);
		}
		else // if (size % 2 == 0)
		{
			if (snd == 5 || snd == 6)
				algorithm_1(g, size, num, snd, x, y, z);
			if (flag && (snd >= 1 && snd <= 4))
				algorithm_2(g, size, num, snd, x, y, z);
		}
	}
	else if (fst == 2 && size % 2 == 1)
		algorithm_3(g, size, num, snd, x, y, z);
	else if (fst == 2 && size % 2 == 0)
		algorithm_4(g, size, num, snd, x, y, z);
	else if (fst == 3)	// special numbers
		algorithm_5(g, size, num, x, y, z);
	else if (fst == 4)	// small numbers
		algorithm_6(g, size, num, snd, x, y, z);

	// Print x, y, z
	for (const auto& ch : x)
		cout << reverse_change(ch);
	cout << " ";
	for (const auto& ch : y)
		cout << reverse_change(ch);
	cout << " ";
	for (const auto& ch : z)
		cout << reverse_change(ch);
	cout << "\n";
}

int main()
{
	ios_base::sync_with_stdio(false);
	cin.tie(nullptr);

	int t;
	cin >> t;

	while(t--)	
		solve();	

	return 0;
}