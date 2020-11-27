int coucou(int c, int d) {}
int main() {}

int (*a(int c))(int b[5]) {}

int b(int *cmp) {}

int c(int (*cmp)(void*,void*)) {}

int always_more_awful(int (*cmp[4])(void*,void*)) {}


/*
int coucou(int c) {
    int b = 9;
    int a = 2;
    return 3;
}

int main() {
    int a = 0;
    int i;
    for (i = 0; i<10; i++) {
        a = a+i;
    }
    return a;
}*/
