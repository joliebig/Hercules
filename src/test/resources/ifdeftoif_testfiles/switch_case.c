int main(int argc, char **argv) {
    int a = argv[1][0] - '0';
	int toReturn = 1;
    switch (a) { 
        case 0: break;  
        case 1: toReturn++;
#ifdef A 
        case 2: toReturn = toReturn*2; break;  
#endif 
        case 3: toReturn = toReturn + 3; break;  
    }
	return toReturn;
}