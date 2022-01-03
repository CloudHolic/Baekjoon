#include <algorithm>
#include <iostream>
#include <queue>
#include <vector>

using namespace std;

typedef long long int64;

class partial {
public:
    int time, cur, photo, deadline;
    int64 start_sum;
    bool after_photo;

    partial(const int time, const int cur, const int photo, const int deadline, const int64 sum, const bool after)
        : time(time), cur(cur), photo(photo), deadline(deadline), start_sum(sum), after_photo(after) {}

    bool operator<(const partial& p) const {
        if (time != p.time)
            return time > p.time;
        if (photo != p.photo)
            return photo < p.photo;
        return start_sum < p.start_sum;
    }
};

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, t;
    cin >> n >> t;

    vector<pair<int, int>> photos(n);
    for(auto& cur: photos)
        cin >> cur.first >> cur.second;
    sort(photos.begin(), photos.end());

    bool result = false;
    priority_queue<partial> pq;
    vector<priority_queue<int>> deadlines(1);
    pq.push(partial(0, 0, 0, 0, 0, false));

    while (!pq.empty())
    {
        partial cur = pq.top();
        while (!pq.empty() && pq.top().time == cur.time)
            pq.pop();

        if (cur.photo == n)
        {
            result = true;
            break;
        }

        if (cur.after_photo)
            deadlines[cur.deadline].pop();
        else
        {
            deadlines.push_back(deadlines[cur.deadline]);
            cur.deadline = static_cast<int>(deadlines.size()) - 1;
        }

        auto& deadline = deadlines[cur.deadline];
        while (cur.cur < n && photos[cur.cur].first <= cur.time)
        {
            deadline.push(-1 * photos[cur.cur].second);
            cur.cur++;
        }

        if (deadline.empty() || cur.time + t + deadline.top() <= 0)
        {
            if (cur.cur < n && (deadline.empty() || photos[cur.cur].first < cur.time + t))            
                pq.push(partial(photos[cur.cur].first, cur.cur, cur.photo, cur.deadline, cur.start_sum, false));

            if (!deadline.empty())
                pq.push(partial(cur.time + t, cur.cur, cur.photo + 1, cur.deadline, cur.start_sum + cur.time, true));
        }
    }

    cout << (result ? "yes" : "no");
    return 0;
}