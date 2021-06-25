#include <iostream>
#include <queue>

using namespace std;

int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, diff1 = -1, diff2 = -1;
    int seq1[2] = { 0, }, seq2[2] = { 0, };
    priority_queue<int, vector<int>, greater<>> pqueue;

    cin >> n;

    for (int i = 0; i < n; i++)
    {
        int temp;
        cin >> temp;
        pqueue.push(temp);
    }

    const int min = pqueue.top();
    pqueue.pop();

    seq1[0] = seq2[0] = min;

    for (int i = 0; i < n; i++)
    {
        if (pqueue.empty())
        {
            cout << "Yes";
            return 0;
        }

        int cur = pqueue.top();
        pqueue.pop();

        if (seq1[1] < 1)
        {
            seq1[1] = cur;
            diff1 = seq1[1] - seq1[0];

            if(diff1 == 0)
            {
                cout << "No";
                return 0;
            }

            continue;
        }

        if (cur - seq1[1] == diff1)
        {
            seq1[0] = seq1[1];
            seq1[1] = cur;
            continue;
        }

        if (seq2[1] < 1)
        {
            seq2[1] = cur;
            diff2 = seq2[1] - seq2[0];

            if(diff2 == 0)
            {
                cout << "No";
                return 0;
            }

            continue;
        }

        if (cur - seq2[1] == diff2)
        {
            seq2[0] = seq2[1];
            seq2[1] = cur;
            continue;
        }

        if (seq1[1] - seq2[1] == diff2 && cur - seq1[1] == diff2)
        {
            seq2[0] = seq1[1];
            seq2[1] = cur;
            seq1[1] = seq1[0];
            continue;
        }

        if (seq2[1] - seq1[1] == diff1 && cur - seq2[1] == diff2)
        {
            seq1[0] = seq2[1];
            seq1[1] = cur;
            seq2[1] = seq2[0];
            continue;
        }

        if (seq2[0] == min && seq1[1] - min == seq2[1] - seq1[1] && cur - seq2[1] == seq2[1] - seq1[1])
        {
            seq2[0] = seq2[1];
            seq2[1] = cur;
            seq1[1] = seq1[0];            
            diff2 = seq2[1] - seq2[0];

            continue;
        }

        if (seq1[0] == min && seq2[1] - min == seq1[1] - seq2[1] && cur - seq1[1] == seq1[1] - seq2[1])
        {
            seq1[0] = seq1[1];
            seq1[1] = cur;
            seq2[1] = seq2[0];
            diff1 = seq1[1] - seq1[0];
            
            continue;
        }

        cout << "No";
        return 0;
    }
}