int f() {
    return 5;
}

int main() {
    int(*x)() = f;
    return x();
}