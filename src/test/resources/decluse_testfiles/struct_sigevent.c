struct forward;
struct _IO_FILE;

typedef struct example
  {
    int number;
  } example_instance;

extern int function1 (struct example *test);

struct _IO_FILE {
	struct _IO_FILE *_chain;
};
struct forward {
	int forward_int;
};

extern int function2 (struct example *test2);

void main() {
	example_instance instance;
	instance.number = 1337;
}