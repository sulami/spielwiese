import std.stdio;

int main()
{
    int i;

    for (i = 1; i <= 100; i++) {
        if (!(i % 3))
            write("Fizz");
        if (!(i % 5))
            write("Buzz");
        if (!(i % 3) || !(i % 5)) {
            write("\n");
            continue;
        }
        writef("%d\n", i);
    }

    return 0;
}

