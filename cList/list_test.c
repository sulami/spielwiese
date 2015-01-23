#include <assert.h>
#include <stdio.h>

#include "list.h"

#define run_test(fn_name) \
    printf("%s\n", #fn_name); \
        fn_name();

void test_list_add_once()
{
    struct list_head *lh = list_add(NULL, NULL);
    assert(lh);
    assert(list_length(lh) == 1);
}

void test_list_add_twice()
{
    struct list_head *lh, *bu;

    bu = lh = list_add(NULL, NULL);
    assert(lh);

    lh = list_add(lh, NULL);
    assert(lh == bu);
    assert(list_length(lh) == 2);
}

void test_list_add_thrice()
{
    struct list_head *lh, *bu;

    bu = lh = list_add(NULL, NULL);
    assert(lh);

    lh = list_add(lh, NULL);
    assert(lh == bu);
    lh = list_add(lh, NULL);
    assert(lh == bu);
    assert(list_length(lh) == 3);
}

void test_empty_list_len()
{
    assert(list_length(NULL) == 0);
}

void test_list_remove()
{
    struct list_head *lh, *bu;

    bu = lh = list_add(NULL, NULL);
    assert(lh);

    lh = list_add(lh, NULL);
    assert(lh == bu);
    lh = list_add(lh, NULL);
    assert(lh == bu);
    assert(list_length(lh) == 3);

    lh = list_remove(lh, lh->next);
    assert(lh == bu);
    assert(list_length(lh) == 2);
}

void test_list_remove_root()
{
    struct list_head *lh, *bu;

    bu = lh = list_add(NULL, NULL);
    assert(lh);

    lh = list_add(lh, NULL);
    assert(lh == bu);
    lh = list_add(lh, NULL);
    assert(lh == bu);
    assert(list_length(lh) == 3);

    lh = list_remove(lh, lh);
    assert(lh != bu);
    assert(list_length(lh) == 2);
}

void test_list_remove_last()
{
    struct list_head *lh = list_add(NULL, NULL);
    assert(lh);
    assert(list_length(lh) == 1);

    lh = list_remove(lh, lh);
    assert(lh == NULL);
}

void test_list_find_element()
{
    struct list_head *lh = list_add(NULL, NULL);
    int *toast = malloc(sizeof(int) * 2);
    assert(toast);

    lh = list_add(lh, NULL);
    lh = list_add(lh, NULL);
    lh = list_add(lh, toast);

    assert(list_find(lh, toast) != NULL);
    assert(list_find(lh, toast+1) == NULL);

    lh = list_add(lh, NULL);

    assert(list_find(lh, toast) != NULL);
    assert(list_find(lh, toast+1) == NULL);
}

int main()
{
    run_test(test_list_add_once);
    run_test(test_list_add_twice);
    run_test(test_list_add_thrice);
    run_test(test_empty_list_len);
    run_test(test_list_remove);
    run_test(test_list_remove_root);
    run_test(test_list_remove_last);
    run_test(test_list_find_element);
    return 0;
}

