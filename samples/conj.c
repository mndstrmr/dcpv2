int main() {
    int a = 1;
    int b = 2;

    if (a < b && a == 3 && b == 4) {
        return 4;
    }

    if (a < b || a == 3 || b == 4) {
        return 5;
    }

    return 6;
}
