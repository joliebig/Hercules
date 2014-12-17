int main(int argc, char **argv) {
    int a = argv[1][0] - '0';
	int toReturn = 1;
    switch (a) { 
        case 0: toReturn = 0; break;  
        case 1: toReturn = toReturn++;
#ifdef A 
        case 4: toReturn = toReturn * 4; break;
#endif 
        case 3: toReturn = toReturn * 3; break;  
        default: toReturn = toReturn * (-1); break; 
    }
	return toReturn;
}