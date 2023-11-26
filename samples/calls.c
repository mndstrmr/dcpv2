int a(int b) {
    return b + 2;
}

int b(int x) {
    return a(x + 4);
}

int main() {
    int x = 0;
    for (int i = 0; i < 10; i++) {
        x += b(i);
    }
    return x;
}
