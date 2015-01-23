#ifndef LIST_H
#define LIST_H

#include <errno.h>
#include <stdlib.h>

struct list_head {
    struct list_head *prev;
    struct list_head *next;
    void *payload;
};

struct list_head *list_add(struct list_head *, void *);
struct list_head *list_remove(struct list_head *, struct list_head *);
void list_delete(struct list_head *);
unsigned int list_length(struct list_head *);

#endif /* LIST_H */

