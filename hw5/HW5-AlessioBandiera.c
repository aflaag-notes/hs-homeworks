#include <stdio.h>
#include <stdlib.h>

void esercizio1() {
    unsigned int x = 1;
    char* first_x = (char*) &x;

    if (*first_x == 1) {
        printf("The processor is little-endian.\n");
    } else {
        printf("The processor is big-endian.\n");
    }
}

typedef struct {
    int n;
    int k;
    int res;

    struct cBinTree* sx;
    struct cBinTree* dx;
} cBinTree;

cBinTree* cBinInvocation(int n, int k) {
    cBinTree* tree = calloc(1, sizeof(cBinTree));

    tree->n = n;
    tree->k = k;

    if (n == k || k == 0) {
        tree->res = 1;
    } else {
        tree->sx = (struct cBinTree*) cBinInvocation(n - 1, k - 1);
        tree->dx = (struct cBinTree*) cBinInvocation(n - 1, k);

        tree->res = ((cBinTree*) tree->sx)->res + ((cBinTree*) tree->dx)->res;
    }

    return tree;
}

cBinTree* cBinInvocationSharing(int n, int k) {
    cBinTree*** T = calloc(n + 1, sizeof(cBinTree**));

    for (int y = 0; y < n + 1; y++) {
        T[y] = calloc(k + 1, sizeof(cBinTree*));

        for (int x = 0; x < k + 1; x++) {
            T[y][x] = calloc(1, sizeof(cBinTree));

            if (y >= x) {
                T[y][x]->n = y;
                T[y][x]->k = x;

                if (y == x || x == 0) {
                    printf("%d %d\n", x, y);
                    T[y][x]->res = 1;
                } else {
                    T[y][x]->sx = (struct cBinTree*) T[y - 1][x - 1];
                    T[y][x]->dx = (struct cBinTree*) T[y - 1][x];

                    T[y][x]->res = ((cBinTree*) T[y][x]->sx)->res + ((cBinTree*) T[y][x]->dx)->res;
                }
            }
        }
    }

    return T[n][k];
}

void print_spaces(int amount) {
    for (int i = 0; i < amount; i++) {
        printf("  ");
    }
}

void print_cBinTree_aux(cBinTree* tree, int depth) {
    printf("(%d, %d, %d, %p)\n", tree->n, tree->k, tree->res, tree);

    if (tree->sx != NULL) {
        print_spaces(depth + 1);

        print_cBinTree_aux((cBinTree*) tree->sx, depth + 1);
    }

    if (tree->dx != NULL) {
        print_spaces(depth + 1);

        print_cBinTree_aux((cBinTree*) tree->dx, depth + 1);
    }
}

void print_cBinTree(cBinTree* tree) {
    print_cBinTree_aux(tree, 0);
    printf("\n");
}

int main() {
    // esercizio1();

    cBinTree* tree = cBinInvocation(5, 3);
    print_cBinTree(tree);

    // cBinTree* tree = cBinInvocationSharing(5, 3);
    // print_cBinTree(tree);

    return 0;
}
