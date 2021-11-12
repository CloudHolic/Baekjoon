#include <iostream>
#include <vector>
#include <queue>

using namespace std;

typedef long long int64;

struct HArc
{
    int id, u, v, low;
    int64 base, mul, num, density;

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

int64 matrix[200003];
int64 cp[200003];


int main()
{
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int size;
    cin >> size;



    return 0;
}