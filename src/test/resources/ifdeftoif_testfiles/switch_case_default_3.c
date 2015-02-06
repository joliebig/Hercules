int main(int argc, char **argv) {
    int a = argv[1][0] - '0';
	int toReturn = 1;
    switch (a) { 
        case 0: break;  
        case 1: toReturn++;

#ifdef A
#ifdef B
        default: toReturn = toReturn*5; break;
#endif
#ifndef B
		default: toReturn = toReturn*20; break;
#endif
#endif
	

#ifndef A
		case 2: toReturn = toReturn*3;
		default: toReturn = toReturn*30; break;
#endif 
    }
	return toReturn;
}