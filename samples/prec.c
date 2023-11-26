int a(int b, int c) {
    return (b+c) * (b+c);
}

int b(int x) {
    return a(x + 4, 3) >= 3? 2:0;
}

int main() {
    int x = 0;
    for (int i = 0; i < 10; i++) {
        x += b(i);
        if (x > 10) break;
    }
    return x;
}
