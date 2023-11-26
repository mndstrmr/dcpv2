int xtime(int a) {
    return a;
}

int inv_mix_cols(int a) {
    int x = 0;
    for (int i = 0; i < 10; i++) {
        x += xtime(xtime(xtime(a)));
    }
    return x;
}
