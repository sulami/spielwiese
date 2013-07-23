#include <stdio.h>

#define PI 3.1414f

static float sphere(float);

int main(void)
{
    float volume;
    float radius;
    printf("Input Radius: ");
    scanf("%f", &radius);
    volume = sphere(radius);
    printf("Volume: %f\n", volume);
    return 0;
}

float sphere(float rad)
{
    float result;
    result = rad * rad * rad;
    result = 4 * PI * result / 3;
    return result;
}
