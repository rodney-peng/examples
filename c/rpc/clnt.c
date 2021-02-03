#include "test.h"

list *mk_list(char *data, int key, color c)
{
    list *lst;

    lst = (list*)malloc(sizeof(list));
    if (lst == NULL)
        return NULL;
    lst->data = data;
    lst->key = key;
    lst->col = c;
    lst->next = NULL;
    return lst;
}

int main(int argc, char *argv[])
{
    list *l, *new;
    CLIENT *cl;
    int *result;

    l = new = mk_list("three", 1393, ORANGE);
    new = new->next = mk_list("two", -1781, PUCE);
    new = new->next = mk_list("one", 8383, TURQUOISE);

    new->next = NULL;

    cl = clnt_create(argv[1], PRINTER, PRINTER_V1, "udp");
    if (cl == NULL) {
        printf("error: could not connect to server.\n");
        clnt_pcreateerror("clnt_create");
        return 1;
    }
    result = print_list_1(l, cl);
    if (result == NULL) {
        printf("error: RPC failed!\n");
        return 1;
    }
    printf("client: server says it printed %d items.\n", *result);

    result = sum_list_1(l, cl);
    if (result == NULL) {
        printf("error: RPC failed!\n");
        return 1;
    }
    printf("client: sum = %d.\n", *result);

    clnt_destroy( cl );
    return 0;
}

