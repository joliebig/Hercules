typedef unsigned int u32;
typedef unsigned long long u64;

#if definedEx(CONFIG_PHYS_ADDR_T_64BIT)
typedef u64 phys_addr_t;
#endif
#if !definedEx(CONFIG_PHYS_ADDR_T_64BIT)
typedef u32 phys_addr_t;
#endif
typedef phys_addr_t resource_size_t;

struct resource {
	resource_size_t start;
	resource_size_t end;
	struct resource *parent, *sibling, *child;
};

struct resource *lookup_resource(struct resource *root, resource_size_t start);
//int adjust_resource(struct resource *res, resource_size_t start, resource_size_t size);
//resource_size_t resource_alignment(struct resource *res);

static 
 resource_size_t resource_size(const struct resource *res)
{
	return res->end - res->start + 1;
}
