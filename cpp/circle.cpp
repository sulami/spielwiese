#include <iostream>

using namespace std;

int main(void)
{
    float i;
    cout << "Enter circle radius: ";
    cin >> i;
    cout << "Circumfence: " << i * 3.1415f * 2 << endl;
    cout << "Area: " << i * i * 3.1415f << endl;
    return 0;
}
