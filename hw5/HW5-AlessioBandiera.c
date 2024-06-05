#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// ### Eserizio 1
void endianness() {
    /*
        REQ: -
        ENS: -
        MOD: -
    */
    
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
    /*
        REQ: a and b castabili ad int;
        ENS: se a == b ritorna 0;
             se a < b ritorna -1;
             se a > b ritorna 1;
        MOD: -
    */

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
    /*
        REQ: arr è ordinato;
             len è la lunghezza di arr;
        ENS: se target, è in arr, ritorna il più piccolo i tale che arr[i] = target;
             se target non è in arr, ritorna -1;
        MOD: -
    */

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
    /*
        REQ: len è la lunghezza di arr;
        ENS: ritorna il numero di elementi senza duplicati di arr;
        MOD: arr contiene tutti gli elementi senza duplicati all'inizio, preservando l'ordine originale;
             arr contiene tutti i duplicati in fondo, preservando l'ordine originale;
    */

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


// ### Esercizio 3.1
typedef struct {
    int n;
    int k;
    int res;

    struct cBinTree* sx;
    struct cBinTree* dx;
} cBinTree;

void free_tree(cBinTree* tree) {
    /*
        REQ: tree non punta a memoria già liberata;
        ENS: -
        MOD: libera tutto il sottoalbero radicato in tree;
    */

    if (tree != NULL) {
        free_tree((cBinTree*) tree->sx);
        free_tree((cBinTree*) tree->dx);

        free(tree);
    }
}

cBinTree* cBinInvocation(int n, int k) {
    /*
        REQ: n >= k;
        ENS: ritorna l'albero dei coefficienti binomiali ottenuti dalla formula di Stifel;
             ritorna NULL in caso di errori di allocazioni;
        MOD: -
    */

    cBinTree* tree = calloc(1, sizeof(cBinTree));

    if (tree == NULL) {
        return NULL;
    }

    tree->n = n;
    tree->k = k;

    if (n == k || k == 0) {
        tree->res = 1;
    } else {
        cBinTree* sx = cBinInvocation(n - 1, k - 1);

        if (sx == NULL) {
            free(tree);
            return NULL;
        }

        cBinTree* dx = cBinInvocation(n - 1, k);

        if (dx == NULL) {
            free_tree(sx);
            free(tree);
            return NULL;
        }

        tree->sx = (struct cBinTree*) sx;
        tree->dx = (struct cBinTree*) dx;

        tree->res = ((cBinTree*) tree->sx)->res + ((cBinTree*) tree->dx)->res;
    }

    return tree;
}


// ### Esercizio 3.2
cBinTree*** cBinInvocationSharing(int n, int k) {
    /*
        REQ: n >= k;
        ENS: ritorna una matrice (n + 1) x (k + 1) di alberi di coefficienti binomiali, ottenuti dalla
             formula di Stifel;
             non esistono alberi aventi nodi che possano avere stesso valore di n e k;
             ritorna NULL in caso di errori allocazione;
        MOD: -
    */

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

    return T;
}

cBinTree* get_tree(cBinTree*** T, int n, int k) {
    /*
        REQ: n >= k;
             T è la matrice calcolata attraverso cBinInvocationSharing;
        ENS: ritorna l'albero dei coefficienti binomiali, radicato in (n k),
             ottenuti dalla formula di Stifel;
             non esistono nodi aventi stesso valore di n e k;
             ritorna NULL in caso di errori di allocazione;
        MOD: -
    */

    return T[n][k];
}

void free_matrix(cBinTree*** matrix, int n, int k) {
    /*
        REQ: matrix ha dimensione (n + 1) x (k + 1);
             matrix non punta a memoria già liberata;
        ENS: -
        MOD: libera matrix;
    */

    for (int y = 0; y < n + 1; y++) {
        for (int x = 0; x < k + 1; x++) {
            free(matrix[y][x]);
        }
    }
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

int append(Node* list, int value) {
    /*
        REQ: list non è NULL
        ENS: se l'allocazione del nuovo nodo è andata a buon fine ritorna 0;
             se l'allocazione del nuovo nodo non è andata a buon fine ritorna -1;
        MOD: modifica list->next creando un nuovo Node contenente il valore value;
             la lista non viene avanzata;
    */

    Node* next = calloc(1, sizeof(Node));

    if (next == NULL) {
        return -1;
    }

    next->value = value;

    list->next = (struct Node*) next;

    return 0;
}

void free_list(Node* list) {
    /*
        REQ: list non punta a memoria già liberata;
        ENS: -
        MOD: libera tutta la lista avente list in testa;
    */

    Node* curr = (Node*) list;

    while (curr != NULL) {
        Node* next = (Node*) curr->next;

        free(curr);

        curr = next;
    }
}


Pair* eulerSieve(int n) {
    /*
        REQ: n >= 0;
        ENS: ritorna l'array di Pair dal quale è possibile estrapolare i primi;
        MOD: -
    */

    Pair* succ_prec = calloc(n - 1, sizeof(Pair));

    if (succ_prec == NULL) {
        return NULL;
    }

    for (int i = 0; i < n - 1; i++) {
        succ_prec[i].fst = 1;
        succ_prec[i].snd = 1;
    }

    succ_prec[0].snd = 0;

    int i = 0;

    while ((i + 2) * (i + 2) <= n) {
        Node* head = calloc(1, sizeof(int));

        if (head == NULL) {
            free(succ_prec);
            return NULL;
        }

        Node* curr = head;

        int j = i;
        int prod = (i + 2) * (j + 2);

        while (prod <= n) {
            if (append(curr, prod) == -1) {
                free_list((Node*) head);
                return NULL;
            }

            curr = (Node*) curr->next;
 
            j += succ_prec[j].fst;
            prod = (i + 2) * (j + 2);
        }

        curr = (Node*) head->next;

        while (curr != NULL) {
            int prod = curr->value - 2;

            int succ_prod = succ_prec[prod].fst;
            int prec_prod = succ_prec[prod].snd;

            succ_prec[prod - prec_prod].fst += succ_prod;
            succ_prec[prod + succ_prod].snd += prec_prod;

            succ_prec[prod].fst = 0;
            succ_prec[prod].snd = 0;

            curr = (Node*) curr->next;
        }

        free_list((Node*) head->next);

        i += succ_prec[i].fst;
    }


    return succ_prec;
}

void printPrimes(Pair* pairs_array, int len) {
    /*
        REQ: len è la lunghezza di pairs_array;
        ENS: -
        MOD: -
    */

    for (int i = 0; i < len; i++) {
        if (pairs_array[i].fst != 0) {
            printf("%d ", i + 2);
        }
    }

    printf("\n");
}


// ### Utils
void print_array(int* arr, int len) {
    /*
        REQ: len è la lunghezza di arr;
        ENS: -
        MOD: -
    */

    for (int i = 0; i < len; i++) {
        printf("%02d ", arr[i]);
    }

    printf("\n");
}

void print_spaces(int amount) {
    /*
        REQ: amount >= 0;
        ENS: -
        MOD: -
    */

    for (int i = 0; i < amount; i++) {
        printf("  ");
    }
}

void print_cBinTree_aux(cBinTree* tree, int depth) {
    /*
        REQ: depth >= 0;
        ENS: -
        MOD: -
    */

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
    /*
        REQ: -
        ENS: -
        MOD: -
    */

    print_cBinTree_aux(tree, 0);
    printf("\n");
}

void print_pair_value(int value) {
    /*
        REQ: -
        ENS: -
        MOD: -
    */

    if (value != 0) {
        printf("%02d ", value);
    } else {
        printf("## ");
    }
}

void print_pairs_array(Pair* pairs_array, int len) {
    /*
        REQ: len è la lunghezza di pairs_array;
        ENS: -
        MOD: -
    */

    for (int i = 0; i < len; i++) {
        print_pair_value(pairs_array[i].fst);
    }

    printf("\n");

    for (int i = 0; i < len; i++) {
        print_pair_value(pairs_array[i].snd);
    }

    printf("\n");
}

bool check_pairs_array(Pair* pairs_array, int len) {
    /*
        REQ: len è la lunghezza di pairs_array;
        ENS: se in pairs_array non ci sono coppie in cui solo un elemento è 0, ritorna true;
             se in pairs_array ci sono coppie in cui solo un elemento è 0, ritorna false;
        MOD: -
    */

    for (int i = 1; i < len; i++) {
        int fst = pairs_array[i].fst;
        int snd = pairs_array[i].snd;
        
        if ((fst == 0 && snd != 0) || (fst != 0 && snd == 0)) {
            return false;
        }
    }

    return true;
}

bool is_prime(int n) {
    /*
        REQ: -
        ENS: se n è primo ritorna true;
             se n non è primo ritorna false;
        MOD: -
    */

    if (n < 2) {
        return false;
    }

    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) {
            return false;
        }
    }

    return true;
}

bool check_primes(Pair* pairs_array, int len) {
    /*
        REQ: len è la lunghezza di pairs_array
        ENS: se tutti i numeri considerati primi in pairs_array sono primi, restituisce true;
             se in pairs_array ci sono numeri considerati primi, non realmente primi,
             restituisce false (dunque la funzione non garantisce la completezza);
        MOD: -
    */

    for (int i = 0; i < len; i++) {
        if (pairs_array[i].fst != 0) {
            if (!is_prime(i + 2)) {
                return false;
            }
        }
    }

    return true;
}

void print_list(Node* list) {
    /*
        REQ: -
        ENS: -
        MOD: -
    */

    Node* curr = list;

    while (curr != NULL) {
        printf("%02d ", curr->value);

        curr = (Node*) curr->next;
    }

    printf("\n");
}


int main() {
    printf("Alessio Bandiera 1985878");

    // ### Esercizio 1
    // endianness();

    // ### Esercizio 2
    // int arr[7] = {5, 4, 5, 3, 5, 2, 3};
    // int rest = push_duplicates(arr, 7);
    // print_array(arr, 7);
    // printf("%d\n", rest);

    // ### Esercizio 3.1
    // cBinTree* tree = cBinInvocation(5, 3);
    // if (tree != NULL) {
    //     print_cBinTree(tree);
    //     free_tree(tree);
    // }

    // ### Esercizio 3.2
    // int n = 5;
    // int k = 3;
    // cBinTree*** matrix = cBinInvocationSharing(n, k);
    // if (matrix != NULL) {
    //     cBinTree* tree_no_dups = get_tree(matrix, n, k);
    //     print_cBinTree(tree_no_dups);
    //     free_matrix(matrix, n, k);
    // }

    // ### Esercizio 4
    // int m = 1000;
    // Pair* pairs = eulerSieve(m);
    // if (pairs != NULL) {
    //     printPrimes(pairs, m - 1);
    //     printf("%d\n", check_pairs_array(pairs, m - 1));
    //     printf("%d\n", check_primes(pairs, m - 1));
    // }

    return 0;
}
