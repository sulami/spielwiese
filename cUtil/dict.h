/* Dictionary-like data structure in C */

#include <string.h>

struct nlist { /* table entry: */
    struct nlist *next; /* next entry in chain */
    char *key;
    char *content;
};

unsigned hash(char *s);
struct nlist *lookup(struct nlist *table, char *s);
struct nlist *install(struct nlist *table, char *key, char *content);
char *strdup(char *);

#define HASHSIZE 101
static struct nlist *hashtab[HASHSIZE]; /* pointer table */

/* hash: form hash value for string s */
unsigned hash(char *s) {
    unsigned hashval;
    for (hashval = 0; strcmp(s, "\0"); s++)
      hashval = (unsigned)*s + 31 * hashval;
    return hashval % HASHSIZE;
}

/* lookup: look for s in hashtab */
struct nlist *lookup(struct nlist *table, char *s) {
    struct nlist *np;
    for (np = &table[hash(s)]; np != NULL; np = np->next)
        if (strcmp(s, np->key) == 0)
          return np; /* found */
    return NULL; /* not found */
}

/* install: put (key, content) in hashtab */
struct nlist *install(struct nlist *table, char *key, char *content) {
    struct nlist *np;
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

char *strdup(char *s) { /* make a duplicate of s */
    char *p;
    p = (char *) malloc(strlen(s)+1); /* +1 for ’\0’ */
    if (p != NULL)
       strcpy(p, s);
    return p;
}

/* vim: set ft=c: */

