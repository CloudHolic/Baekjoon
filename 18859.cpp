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

        // 수열 1이 비어있을 때
        if (seq1[1] < 1)
        {
            seq1[1] = cur;
            diff1 = seq1[1] - seq1[0];

            // 공차가 0이면 불가능
            if(diff1 == 0)
            {
                cout << "No";
                return 0;
            }

            continue;
        }

        // 수열 1의 뒤에 놓을 수 있을 때
        if (cur - seq1[1] == diff1)
        {
            seq1[0] = seq1[1];
            seq1[1] = cur;
            continue;
        }

        // 수열 2가 비어있을 때
        if (seq2[1] < 1)
        {
            seq2[1] = cur;
            diff2 = seq2[1] - seq2[0];

            // 공차가 0이면 불가능
            if(diff2 == 0)
            {
                cout << "No";
                return 0;
            }

            continue;
        }

        // 수열 2의 뒤에 놓을 수 있을 때
        if (cur - seq2[1] == diff2)
        {
            seq2[0] = seq2[1];
            seq2[1] = cur;
            continue;
        }

        // 어느 쪽 수열에도 놓을 수 없을 때

        // 수열 1의 마지막 숫자로 수열 2의 마지막 숫자와 min을 연결 할 수 있을 때
        if (seq1[1] - seq2[1] == diff2 && cur - seq1[1] == diff2)
        {
            seq2[0] = seq1[1];
            seq2[1] = cur;
            seq1[1] = seq1[0];
            continue;
        }

        // 수열 2의 마지막 숫자로 수열 1의 마지막 숫자와 min을 연결할 수 있을 때
        if (seq2[1] - seq1[1] == diff1 && cur - seq2[1] == diff2)
        {
            seq1[0] = seq2[1];
            seq1[1] = cur;
            seq2[1] = seq2[0];
            continue;
        }

        // 수열 1의 마지막 숫자가 수열 2의 2번째 숫자로 들어갈 수 있을 때
        if (seq2[0] == min && seq1[1] - min == seq2[1] - seq1[1] && cur - seq2[1] == seq2[1] - seq1[1])
        {
            seq2[0] = seq2[1];
            seq2[1] = cur;
            seq1[1] = seq1[0];
            
            // 새로 계산한 공차는 0이 될 수 없다.
            diff2 = seq2[1] - seq2[0];

            continue;
        }

        // 수열 2의 마지막 숫자가 수열 1의 2번째 숫자로 들어갈 수 있을 때
        if (seq1[0] == min && seq2[1] - min == seq1[1] - seq2[1] && cur - seq1[1] == seq1[1] - seq2[1])
        {
            seq1[0] = seq1[1];
            seq1[1] = cur;
            seq2[1] = seq2[0];

            // 새로 계산한 공차는 0이 될 수 없다.
            diff1 = seq1[1] - seq1[0];
            
            continue;
        }

        // 위의 경우에 하나도 해당되지 않는다면 불가능
        cout << "No";
        return 0;
    }
}