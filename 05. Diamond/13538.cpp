#include <iostream>

using namespace std;

#define SIZE 524288

class node
{
public:
	int value;
	node *left, *right;

	node(int v, node* l, node* r) : value(v), left(l), right(r) {}

	node* update(int idx, int st, int en)
	{
		if (st <= idx && en >= idx)
		{
			if (st == en)
				return new node(value + 1, nullptr, nullptr);

			const int mid = st + en >> 1;
			return new node(value + 1, left->update(idx, st, mid), right->update(idx, mid + 1, en));
		}

		return this;
	}

} *segtree[500001];

int query_xor(const node* left, const node* right, const int st, const int en, const int target)
{
	if (st == en)
		return st;

	const int mid = st + en >> 1;
	const int value = (en - st + 1) / 2 & target;
	if (value && left->left->value == right->left->value || !value && left->right->value != right->right->value)
		return query_xor(left->right, right->right, mid + 1, en, target);

	return query_xor(left->left, right->left, st, mid, target);
}

int query_sum(const node* seg, const int left, const int right, const int st, const int en)
{
	if (left > en || right < st)
		return 0;

	if (left <= st && en <= right)
		return seg->value;

	const int mid = st + en >> 1;
	return query_sum(seg->left, left, right, st, mid) + query_sum(seg->right, left, right, mid + 1, en);
}

int query_kth(const node* left, const node* right, const int st, const int en, const int target)
{
	if (st == en)
		return st;

	const int mid = st + en >> 1;
	const int value = right->left->value - left->left->value;
	if (value >= target)
		return query_kth(left->left, right->left, st, mid, target);

	return query_kth(left->right, right->right, mid + 1, en, target - value);
}

int main()
{
	ios::sync_with_stdio(false);
	cin.tie(nullptr);

	int m, n = 0;
	cin >> m;

	segtree[0] = new node(0, nullptr, nullptr);
	segtree[0]->left = segtree[0]->right = segtree[0];

	for (; m > 0; m--)
	{
		int q;
		cin >> q;

		switch(q)
		{
			int x, y, z;
		case 1:
			cin >> x;
			n += 1;
			segtree[n] = segtree[n - 1]->update(x, 0, SIZE - 1);
			break;
		case 2:
			cin >> x >> y >> z;
			cout << query_xor(segtree[x - 1], segtree[y], 0, SIZE - 1, z) << '\n';
			break;
		case 3:
			cin >> x;
			n -= x;
			break;
		case 4:
			cin >> x >> y >> z;
			cout << query_sum(segtree[y], 1, z, 0, SIZE - 1) - query_sum(segtree[x - 1], 1, z, 0, SIZE - 1) << '\n';
			break;
		default:
			cin >> x >> y >> z;
			cout << query_kth(segtree[x - 1], segtree[y], 0, SIZE - 1, z) << '\n';
			break;
		}
	}

	return 0;
}