typedef long unsigned int size_t;
extern void *malloc (size_t __size) __attribute__ ((__nothrow__)) __attribute__ ((__malloc__)) ;

typedef struct len_and_sockaddr {
	union {
		int i;
	} u;
} len_and_sockaddr;
enum {
	LSA_LEN_SIZE = __builtin_offsetof (len_and_sockaddr, u)
};

void main(){
	struct len_and_sockaddr *s =
	malloc(sizeof(*s));
}