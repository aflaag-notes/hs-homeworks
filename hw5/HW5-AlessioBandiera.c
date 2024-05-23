#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void print_array(int* arr, int len) {
    for (int i = 0; i < len; i++) {
        printf("%02d ", arr[i]);
    }

    printf("\n");
}

// ### Eserizio 1
void endianness() {
    unsigned int x = 1;
    char* first_x = (char*) &x;

    if (*first_x == 1) {
        printf("The processor is little-endian.\n");
    } else {
        printf("The processor is big-endian.\n");
    }
}


// ### Esercizio 2
int compare(const void* a, const void* b) {
    int int_a = *((int*) a);
    int int_b = *((int*) b);

    if (int_a == int_b) {
        return 0;
    } else if (int_a < int_b) {
        return -1;
    } else {
        return 1;
    }
}

int binary_search_first(int* arr, int len, int target) {
    int left = 0;
    int right = len - 1;
    int result = -1;

    while (left <= right) {
        int mid = left + (right - left) / 2;
        if (arr[mid] == target) {
            result = mid;
            right = mid - 1; 
        } else if (arr[mid] < target) {
            left = mid + 1;
        } else {
            right = mid - 1;
        }
    }

    return result;
}

int push_duplicates(int* arr, int len) {
    int sorted_arr[len];

    memcpy(sorted_arr, arr, len * sizeof(int));

    qsort(sorted_arr, len, sizeof(int), compare);
    
    int occ[len];

    memset(occ, 0, len * sizeof(int));

    for (int i = 0; i < len; i++) {
        int index = binary_search_first(sorted_arr, len, arr[i]);

        occ[index]++;
    }

    int n_duplicates = 0;

    for (int i = 0; i < len; i++) {
        if (occ[i] == 0) {
            n_duplicates++;
        }
    }

    int final[len];

    int a = 0;
    int b = len - n_duplicates;

    for (int i = 0; i < len; i++) {
        int index = binary_search_first(sorted_arr, len, arr[i]);

        if (occ[index] != 0) {
            occ[index] = 0;
            final[a] = arr[i];
            a++;
        } else {
            final[b] = arr[i];
            b++;
        }
    }

    memcpy(arr, final, len * sizeof(int));

    return len - n_duplicates;
}


// ### Esercizio 3
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


// ### Esercizio 4
typedef struct {
    int fst;
    int snd;
} Pair;

typedef struct {
    int value;

    struct Node* next;
} Node;

Node* append(Node* list, int value) {
    Node* next = calloc(1, sizeof(Node));

    next.value = value;

    list->next = (struct Node*) next;

    list = list->res;
}

Pair* eulerSieve(int n) {
    Pair* succ_prec = calloc(n - 1, sizeof(Pair));

    int pos[n - 1];

    for (int i = 0; i < n - 1; i++) {
        pos[i] = i + 2;

        succ_prec[i].fst = 1;
        succ_prec[i].snd = 1;
    }
    print_array(pos, n - 1);

    succ_prec[0].snd = -1;

    // for (int i = 0; i < n - 1; i++) {
    for (int i = 0; i < 1; i++) {
        Node* head = calloc(1, sizeof(int));

        Node* temp = head;

        int succ = -1;

        for (int j = i; pos[i] * pos[j] < n - 1; j += succ) {
            int prod = pos[i] * pos[j];

            append(temp, prod);

            // int succ_prod = succ_prec[prod].fst;
            // int prec_prod = succ_prec[prod].snd;
            //
            // succ_prec[prod - succ_prod].fst += succ_prod;
            // succ_prec[prod + prec_prod].snd += prec_prod;
            //
            // succ_prec[prod].fst = -1;
            // succ_prec[prod].snd = -1;

            succ = succ_prec[j].fst;
        }
    }

    return succ_prec;
}


// Utils

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

void print_pair_value(int value) {
    if (value != -1) {
        printf("%02d ", value);
    } else {
        printf("## ");
    }
}

void print_pairs_array(Pair* pairs_array, int len) {
    for (int i = 0; i < len; i++) {
        print_pair_value(pairs_array[i].fst);
    }

    printf("\n");

    for (int i = 0; i < len; i++) {
        print_pair_value(pairs_array[i].snd);
    }

    printf("\n");
}


int main() {
    // endianness();

    // int arr[7] = {5, 4, 5, 3, 5, 2, 3};
    // int rest = push_duplicates(arr, 7);
    // print_array(arr, 7);
    // printf("%d\n", rest);

    // cBinTree* tree = cBinInvocation(5, 3);
    // print_cBinTree(tree);

    // cBinTree* tree = cBinInvocationSharing(5, 3);
    // print_cBinTree(tree);

    Pair* pairs = eulerSieve(24);
    print_pairs_array(pairs, 23);

    return 0;
}
