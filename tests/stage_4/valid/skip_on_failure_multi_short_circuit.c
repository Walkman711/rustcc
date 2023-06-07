// #include <stdio.h>
int main() {
    int a = 0;
    a || (a = 3) || (a = 4);
    // printf("%d", a);
    return a;
}
