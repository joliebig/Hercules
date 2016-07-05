#ifndef STACK_C
#define STACK_C
#include "stack.h"


bool is_empty(pstack *s) { return !s; }
bool is_empty2(pstack s) { return !s; }

void make_empty(pstack *s)
{
    if (!is_empty(s))
        pop(s, 0);
}

void *pop(pstack *s, int isString)
{
    struct Stack *tmp;
    void *i;

    if (is_empty(s))
        exit(EXIT_FAILURE);

    tmp = *s;
    i = (*s)->data;
    *s = (*s)->next;
    free(tmp);
    return i;
}

void *top(pstack *s)
{
    if (is_empty(s))
        exit(EXIT_FAILURE);

    return (*s)->data;
}

void stack_content(pstack *s, char** result) {
    struct Stack *tmp;
    char **content;
    // Starting array size for the number of context strings
    int currentContentArraySize = 10;
    content = (char**) malloc(currentContentArraySize * sizeof(char*));
    tmp = *s;
    int i = 0;
    while (!is_empty2(tmp)) {
    	if (i == currentContentArraySize) {
    		// Expand context string array size
    		currentContentArraySize *= 2;
    		content = (char**) realloc(content, currentContentArraySize * sizeof(char*));
    	}
    	content[i] = (char*) malloc((strlen((char*) tmp->data) + 1) * sizeof(char));
        strcpy(content[i], (char*) tmp->data);
        i++;
        tmp = tmp->next;
    }
    if (i == 1) {
    	// Only one element in the stack: malloc and done
    	*result = (char*) malloc((strlen(content[0]) + 1) * sizeof(char));
    	strcpy(*result, content[0]);
    	free(content[0]);
    	content = (char**) realloc(content, sizeof(char*));
    } else {
        int j;
        /* Start with the last index of content since that is the last element from stack iteration
           which is the first element added into the stack */
        for (j=i-1; j > 0; j--) {
        	if (j == i-1) {
        		// Don't forget malloc, +2 for '#' divider and '\0' string terminator
            	*result = (char*) malloc((strlen(content[j]) + 2) * sizeof(char));
        		strcpy(*result, content[j]);
                strcat(*result, "#");
        	} else {
        		// Realloc, +2 for '#' divider and '\0' string terminator
        		*result = (char*) realloc(*result, ((strlen(*result) + strlen(content[j]) + 2) * sizeof(char)));
                strcat(*result, content[j]);
                strcat(*result, "#");
        	}
        	free(content[j]);
        }
        // Add last element without adding a '#' divider
		*result = (char*) realloc(*result, ((strlen(*result) + strlen(content[0]) + 1) * sizeof(char)));
        strcat(*result, content[0]);
    	free(content[0]);
    }
    free(content);
}

void push(pstack *s, void *new_num, int isString)
{
    struct Stack *tmp;
    tmp = *s;
    struct Stack *new_node = (struct Stack*) malloc(sizeof(struct Stack));
    if (!new_node) {
    	//printf("EXIT\n");
        exit(EXIT_FAILURE);
    }
    new_node->data = new_num;
    new_node->next = *s;
    *s = new_node;
}

int pushUnique(pstack *s, void *new_num, int isString)
{
    struct Stack *tmp_iterator;
    int isNewValue = 1;
    tmp_iterator = *s;

    while (!is_empty2(tmp_iterator)) {
    	// Don't add context information if it is already present inside the stack from previous points
        if (tmp_iterator->data == new_num) {
            isNewValue = 0;
        }
        tmp_iterator = tmp_iterator->next;
    }
    if (isNewValue) {
        struct Stack *new_node = (struct Stack*) malloc(sizeof(struct Stack));
        if (!new_node) {
            exit(EXIT_FAILURE);
        }
        new_node->data = new_num;
        new_node->next = *s;
        *s = new_node;
    }
    return isNewValue;
}

int stack_size(pstack *s) {
    struct Stack *tmp_iterator;
    tmp_iterator = *s;
    int currentStackSize = 0;
    while (!is_empty2(tmp_iterator)) {
    	currentStackSize++;
        tmp_iterator = tmp_iterator->next;
    }
    return currentStackSize;
}
#endif
