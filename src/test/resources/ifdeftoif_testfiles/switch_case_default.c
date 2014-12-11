int main() {  
    int a = 1;
	int toReturn = 1;
    switch (a) { 
        case 0: toReturn = 0; break;  
        case 1: toReturn = toReturn++;
#ifdef A 
        case 4: toReturn = toReturn * 4; break;
#endif 
        case 3: toReturn = 3; break;  
        default: toReturn = -1; break; 
    }
	return toReturn;
}