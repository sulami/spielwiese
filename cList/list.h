#ifndef LIST_H
#define LIST_H

#include <errno.h>
#include <stdlib.h>

struct list_head {
    struct list_head *prev;
    struct list_head *next;
    void *payload;
};

/*
 * list_add - create a new list or append to an existing one
 *
 * Passing NULL as list_head * will result in creation of a new list
 * which will be returned. Takes the payload of the new element as
 * second argument.
 */
struct list_head *list_add(struct list_head *, void *);

/*
 * list_remove - remove an item from a list
 *
 * Pass the list root element as well as the list_head * of the element
 * to be removed.
 */
struct list_head *list_remove(struct list_head *, struct list_head *);

/*
 * list_delete - delete an entire list
 */
void list_delete(struct list_head *);

/*
 * list_length - returns the length of a list
 */
unsigned int list_length(struct list_head *);

#endif /* LIST_H */

