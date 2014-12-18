int main(int argc, char **argv) {
    int a = argv[1][0] - '0';
	int toReturn = 1;
    switch (a) { 
        case 0: break;  
        case 1: toReturn++;
#ifdef A 
        case 2: toReturn = toReturn*2; 
#endif 
#ifndef A
		case 2: toReturn = toReturn*(-2);
#endif
#ifdef B
		case 3: toReturn = toReturn*4; break;
#endif
        case 4: toReturn = toReturn + 3; break;
#ifdef A
		default: toReturn = toReturn*(-1); break;
#endif
    }
	return toReturn;
}