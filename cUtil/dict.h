/*
 * Dictionary-like data structure in C
 *
 * Derived from an example in The C Programming Language (K&R)
 */

#include <string.h>

struct dictentry {
    struct dictentry *next;
    char *key;
    char *content;
};

#define HASHSIZE 101

unsigned hash(unsigned char *s)
{
    unsigned hashval;
    for (hashval = 0; strcmp(s, "\0"); s++)
        hashval = (unsigned)*s + 31 * hashval;
    return hashval % HASHSIZE;
}

struct dictentry *lookup(struct dictentry *table, unsigned char *s)
{
    struct dictentry *np;
    for (np = &table[hash(s)]; np != NULL; np = np->next)
        if (strcmp(s, np->key) == 0)
            return np; /* found */
    return NULL; /* not found */
}

struct dictentry *install(struct dictentry *table, unsigned char *key,
                          char *content)
{
    struct dictentry *np;
    unsigned hashval;
    if ((np = lookup(table, key)) == NULL) { /* not found */
        np = (struct nlist *) malloc(sizeof(*np));
        if (np == NULL || (np->key = strdup(key)) == NULL)
            return NULL;
        hashval = hash(key);
        np->next = &table[hashval];
        table[hashval] = *np;
    } else /* already there */
        free((void *) np->content); /*free previous content */
    if ((np->content = strdup(content)) == NULL)
        return NULL;
    return np;
}

char *strdup(unsigned char *s)
{
    char *p;
    p = (char *) malloc(strlen(s) + 1);
    if (p != NULL)
        strcpy(p, s);
    return p;
}

/* vim: set ft=c: */

