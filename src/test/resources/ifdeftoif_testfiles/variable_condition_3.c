int main(int argc, char **argv) {
    int toReturn=1;

    if (1)
      #if defined(ERROR)
    {
      toReturn += 10;
    }
  #else
    {
      toReturn += 100;
    }
  #endif

  #if defined(ELSE)
    else {
      toReturn += 1000;
    }
  #endif
    return toReturn;
}
