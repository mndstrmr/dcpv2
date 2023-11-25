int main() {
    int a = 4;
    for (int i = 0; i < 10; i++) {
        if (i < 4)
            a += i;
        else a -= 4;
    }
    return a;
    // int b = 5;
    // if (a > b) return a + b;
    // else return a - b;
}
