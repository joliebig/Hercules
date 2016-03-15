#ifndef STACK_H
#define STACK_H

#include <stdbool.h>

struct Stack {
    void *data;
    struct Stack *next;
};

/*
 * We declare a pointer to a Stack structure thereby making use of incomplete
 * types. Clients that pull in stack.h will be able to declare variables of type
 * pstack which are pointers to pointers to Stack structures.
 * */
typedef struct Stack *pstack;

bool is_empty(pstack *s);
void make_empty(pstack *s);
void push(pstack *s, void *new_num, int isString);
int pushUnique(pstack *s, void *new_num, int isString);
void *pop(pstack *s, int isString);
void *top(pstack *s);
void stack_content(pstack *s, char** result);
int stack_size(pstack *s);

#endif /* STACK_H */
