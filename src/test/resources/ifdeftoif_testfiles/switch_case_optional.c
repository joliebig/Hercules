int main() {  
    int a = 1;
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