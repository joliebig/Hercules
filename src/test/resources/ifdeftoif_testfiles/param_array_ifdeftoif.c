#include <stdio.h>

struct  ifdef_options {
  int config_feature_find_paren ;
  int config_feature_find_inum ;
  int config_feature_find_group ;
  int config_feature_find_newer ;
  int config_feature_find_exec ;
  int config_feature_find_regex ;
  int config_feature_find_path ;
  int config_feature_find_user ;
  int config_feature_find_delete ;
  int config_feature_find_context ;
  int config_feature_find_mtime ;
  int config_feature_find_prune ;
  int config_feature_find_type ;
  int config_feature_find_perm ;
  int config_feature_find_links ;
  int config_feature_find_size ;
  int config_feature_find_not ;
  int config_desktop ;
  int config_feature_find_depth ;
  int config_feature_find_print0 ;
  int config_feature_find_mmin ;
}  id2i;

char *id2i_strcat(char *t,char *s)
{
  int x,y,z;
  int i,j=0,k;
  char*name;
  x=str_length(t);
  y=str_length(s);
  z=x+y+1;
  name=(char*)malloc(z*sizeof(char));
  for(i=0;i<x;i++)
  {
   *(name+i)=*(t+i);
  }
   for(k=x+1;k<z;k++)
  {
   *(name+k)=*(s+j);
    j++;
  }    
 return  name; 
}

char *id2i_params() {
    char *start = "-start\0";
    char *append = NULL;
    if (1) {
        append =
            "-a\0"
            "-o\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_not) {
        append =
            "!\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_desktop) {
        append =
            "-and\0"
            "-or\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_not) {
        append =
            "-not\0";
        start = id2i_strcat(start, append);
    }
    if (1) {
        append =
            "-print\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_print0) {
        append =
            "-print0\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_depth) {
        append =
            "-depth\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_prune) {
        append =
            "-prune\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_delete) {
        append =
            "-delete\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_exec) {
        append =
            "-exec\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_paren) {
        append =
            "-(\0";
        start = id2i_strcat(start, append);
    }
    if (1) {
        append =
            "-name\0"
            "-iname\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_path) {
        append =
            "-path\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_regex) {
        append =
            "-regex\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_type) {
        append =
            "-type\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_perm) {
        append =
            "-perm\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_mtime) {
        append =
            "-mtime\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_mmin) {
        append =
            "-mmin\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_newer) {
        append =
            "-newer\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_inum) {
        append =
            "-inum\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_user) {
        append =
            "-user\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_group) {
        append =
            "-group\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_size) {
        append =
            "-size\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_context) {
        append =
            "-context\0";
        start = id2i_strcat(start, append);
    }
    if (id2i.config_feature_find_links) {
        append =
            "-links\0";
        start = id2i_strcat(start, append);
    }
    
    return start;
}

int str_length(const char *input){
    int length = 0;
    while((input[length]!='\0') || (input[length +1] == '-'))
    {
        length++;
    }
    return length;
}
void id2i_init()  {
  (id2i.config_feature_find_regex = 1);
  (id2i.config_feature_find_exec = 1);
  (id2i.config_feature_find_size = 1);
  (id2i.config_feature_find_mtime = 1);
  (id2i.config_feature_find_paren = 1);
  (id2i.config_feature_find_path = 1);
  (id2i.config_feature_find_newer = 1);
  (id2i.config_feature_find_mmin = 1);
  (id2i.config_feature_find_perm = 1);
  (id2i.config_feature_find_depth = 1);
  (id2i.config_feature_find_delete = 1);
  (id2i.config_feature_find_group = 1);
  (id2i.config_feature_find_user = 1);
  (id2i.config_desktop = 1);
  (id2i.config_feature_find_prune = 1);
  (id2i.config_feature_find_inum = 1);
  (id2i.config_feature_find_type = 1);
  (id2i.config_feature_find_links = 1);
  (id2i.config_feature_find_print0 = 1);
  (id2i.config_feature_find_not = 1);
  (id2i.config_feature_find_context = 0);
}

int main(int argc, char *argv[])
{
    id2i_init();
    const char *hello = "Hello Jörg\n";
    printf("%s", hello);
    
    char *id2i_param_pointer = id2i_params();
    int id2i_pp_length = str_length(id2i_param_pointer);
    char param[id2i_pp_length +1];
    int i;
    for (i=0; i <= id2i_pp_length; i++) {
        param[i] = id2i_param_pointer[i];
    }
    for (i=0; i <= id2i_pp_length; i++) {
        if (param[i] == '\0') {
            printf("\\0");
        } else {
            printf("%c", param[i]);
        }
    }
}
