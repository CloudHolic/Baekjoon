#include <iostream>
#include <algorithm>
#include <vector>
#include <queue>

using namespace std;

typedef long long int64;

#define MAXSIZE 200000

int nums;
int64 matrix[MAXSIZE + 3];
int64 cp[MAXSIZE + 3];

class HArc
{
public:
    int id, u, v, low;
    int64 mul, base, num, density;
    static int count;

    HArc()
    {
        this->id = this->u = this->v = this->low = 0;
        this->mul = this->base = this->num = this->density = 0;
    }

    HArc(int u, int v)
    {
        // Assume that u <= v
        int64 mul = matrix[u] * matrix[v];

        this->id = count;
        this->u = u;
        this->v = v;
        this->low = matrix[u] < matrix[v] ? u : v;
        this->mul = mul;
        this->base = cp[v] - cp[u] - mul;
        this->num = 0;
        this->density = 0;
    }

    bool contains(HArc& b) const
    {
        return u <= b.u && b.v <= v;
    }

    int64 support() const
    {
        return num / density;
    }

    bool operator< (const HArc& b) const
    {
        return support() < b.support();
    }

    bool operator<= (const HArc& b) const
    {
        return support() <= b.support();
    }

    bool operator== (const HArc& b) const
    {
        return support() == b.support();
    }
};

int HArc::count = 0;
HArc arcs[MAXSIZE + 3];
vector<int> childs[MAXSIZE + 3];

int pqs, qid[MAXSIZE + 3], sub[MAXSIZE + 3];
vector<HArc> con[MAXSIZE + 3];
priority_queue<HArc> pq[MAXSIZE + 3];

int64 multiply(int node)
{
    if (node == 1)
        return matrix[1] * matrix[2] + matrix[1] * matrix[nums];

    HArc& cur_arc = arcs[node];
    if (cur_arc.u == cur_arc.low)
    {
        if (con[cur_arc.u].empty() || !cur_arc.contains(con[cur_arc.u].back()))
            return matrix[cur_arc.u] * matrix[cur_arc.u + 1];
        else
            return con[cur_arc.u].back().mul;
    }
    else
    {
        if (con[cur_arc.v].empty() || !cur_arc.contains(con[cur_arc.v].back()))
            return matrix[cur_arc.v] * matrix[cur_arc.v - 1];
        else
            return con[cur_arc.v].back().mul;
    }

    return 0;
}

void add_arc(int node, HArc& arc)
{
    pq[qid[node]].push(arc);
    con[arc.u].push_back(arc);
    con[arc.v].push_back(arc);
}

void remove_arc(int node)
{
    const HArc& arc = pq[qid[node]].top();
    con[arc.u].pop_back();
    con[arc.v].pop_back();
    pq[qid[node]].pop();
}

#pragma warning (disable: 6385)
void merge_pq(int node)
{
    int max_child = -1;

    for (auto& i : childs[node])
        if (max_child == -1 || sub[max_child] < sub[i])
            max_child = i;

    qid[node] = qid[max_child];
    priority_queue<HArc>& cur_pq = pq[qid[node]];

    for (auto& i : childs[node])
    {
        if (i != max_child)
        {
            priority_queue<HArc>& child_pq = pq[qid[i]];
            while (!child_pq.empty())
            {
                cur_pq.push(child_pq.top());
                child_pq.pop();
            }
        }
    }
}
#pragma warning (default: 6385)

void dfs(int node)
{
    sub[node] = 1;
    HArc& cur_arc = arcs[node];

    if (childs[node].empty())
    {
        qid[node] = ++pqs;
        cur_arc.density = cur_arc.base;
        cur_arc.num = matrix[cur_arc.low] * (cur_arc.density + cur_arc.mul - multiply(node));
        add_arc(node, cur_arc);
    }
    else
    {
        cur_arc.density = cur_arc.base;
        for (auto& i : childs[node])
        {
            dfs(i);
            sub[node] += sub[i];
            cur_arc.density -= arcs[i].base;
        }

        cur_arc.num = matrix[cur_arc.low] * (cur_arc.density + cur_arc.mul - multiply(node));
        merge_pq(node);

        priority_queue<HArc>& cur_pq = pq[qid[node]];
        while (!cur_pq.empty() && cur_pq.top().support() >= matrix[cur_arc.low])
        {
            HArc arc = cur_pq.top();
            cur_arc.density += arc.density;
            remove_arc(node);
            cur_arc.num = matrix[cur_arc.low] * (cur_arc.density + cur_arc.mul - multiply(node));
        }

        while (!cur_pq.empty() && cur_arc <= cur_pq.top())
        {
            HArc arc = cur_pq.top();
            cur_arc.density += arc.density;
            remove_arc(node);
            cur_arc.num += arc.num;
        }

        add_arc(node, cur_arc);
    }
}

int64 solve()
{
    // Prepare
    int64 idx = min_element(matrix + 1, matrix + nums + 1) - matrix;
    rotate(matrix + 1, matrix + idx, matrix + nums + 1);
    matrix[nums + 1] = matrix[1];
    for (int i = 1; i <= nums + 1; i++)
    {
        cp[i] = matrix[i] * matrix[i - 1];
        cp[i] += cp[i - 1];
    }

    // Sweep
    vector<int> stack;
    vector<pair<int, int>> temp, list;

    for (int i = 1; i <= nums; i++)
    {
        while (stack.size() >= 2 && matrix[stack.back()] > matrix[i])
        {
            temp.push_back({ stack[stack.size() - 2], i });
            stack.pop_back();
        }
        stack.push_back(i);
    }

    while (stack.size() >= 4)
    {
        temp.push_back({ 1, stack[stack.size() - 2] });
        stack.pop_back();
    }

    for (auto& pair : temp)
    {
        if (pair.first == 1 || pair.second == 1)
            continue;
        list.push_back(pair);
    }

    // Build HArc tree
    vector<int>().swap(stack);
    arcs[++HArc::count] = HArc(1, nums + 1);    // root
    for (auto& pair : list)
    {
        arcs[++HArc::count] = HArc(pair.first, pair.second);
        while (!stack.empty() && arcs[HArc::count].contains(arcs[stack.back()]))
        {
            childs[HArc::count].push_back(stack.back());
            stack.pop_back();
        }
        stack.push_back(HArc::count);
    }

    while (!stack.empty())
    {
        childs[1].push_back(stack.back());
        stack.pop_back();
    }

    // Find an answer
    int64 result = 0;
    dfs(1);
    priority_queue<HArc>& cur_pq = pq[qid[1]];
    while (!cur_pq.empty())
    {
        result += cur_pq.top().num;
        cur_pq.pop();
    }

    return result;
}

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    cin >> nums;

    int temp;
    for (int i = 1; i < nums; i++)
        cin >> matrix[i] >> temp;
    cin >> matrix[nums] >> matrix[nums + 1];

    if (nums == 1)
        cout << matrix[1] * matrix[2];
    else
    {
        nums++;
        cout << solve();
    }

    return 0;
}