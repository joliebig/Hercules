#ifndef STACK_C
#define STACK_C
#include "stack.h"


bool is_empty(pstack *s) { return !s; }

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
    if (isString) {
      //printf("Popping %s\n", i);
    } else {
      //printf("Popping time\n");
    }
    return i;
}

void *top(pstack *s)
{
    if (is_empty(s))
        exit(EXIT_FAILURE);

    return (*s)->data;
}

int stack_content(pstack *s, char* result) {
	//printf("CONTENT\n");
    struct Stack *tmp;
    char **content;
    content = malloc(1000 * sizeof(char*));
    tmp = *s;
    int i = 0;
    int strlength = 0;
    while (!is_empty(tmp)) {
        //content[i] = malloc((strlen(tmp->data)+1) * sizeof(char*));
        //strcpy(content[i], tmp->data);
        content[i] = tmp->data;
        // increase string length counter for malloc later
        strlength = strlength + strlen(tmp->data);
        //printf("CO: %s\n%d\n", content[i], strlength);
        i++;
        tmp = tmp->next;
    }
    // add spaces für '&'s to string length counter
    strlength += i;
    int j;
    for (j=i-1; j > 0; j--) {
        strcat(result, content[j]);
        strcat(result, "#");
    }
    strcat(result, content[0]);
    result[strlength] = '\0';
    //printf("wtf: %s %zu\n", result, strlength);
    content = realloc(content, i * sizeof(char*));
    //free(content);
    return strlength;
}

void push(pstack *s, void *new_num, int isString)
{
    struct Stack *tmp;
    tmp = *s;
    	// /*!is_empty(tmp) && tmp->data == new_num*/
        // don't push the same context twice in a row!
    struct Stack *new_node = malloc(sizeof(struct Stack));
    if (!new_node) {
    	//printf("EXIT\n");
        exit(EXIT_FAILURE);
    }
    new_node->data = new_num;
    if (isString) {
      //printf("Pushing %s\n", new_num);
    } else {
      //printf("Pushing time\n");
    }
    new_node->next = *s;
    //stack_elements++;
    *s = new_node;
}
#endif
