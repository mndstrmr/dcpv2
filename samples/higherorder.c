int f(int x);

int main() {
    int a = 4;
    int(*x)(int y) = f;
    return x(a);
}

int f(int x) {
    return x + 5;
}