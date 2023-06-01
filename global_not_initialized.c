int foo;

int bar() {
    foo = 2;
    return 0;
}

int main() {
    bar();
    for (int i = 0; i < 3; i = i + 1)
        foo = foo + 1;
    return foo;
}
