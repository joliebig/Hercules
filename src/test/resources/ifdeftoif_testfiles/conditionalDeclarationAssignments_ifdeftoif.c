struct  ifdef_options {
  int a ;
  int z ;
  int c ;
  int b ;
}  id2i;
int _1_a =  (5 * 1);
int _2_a =  (5 * 0);
void main()  {
  int x =  (((id2i.a ) ) ? (5 * 0) : (5 * 1));
  int _6_y =  (((id2i.a ) ) ? (5 * (0 * _2_a)) : (5 * (1 * _1_a)));
  int _610_z =  (((id2i.a && id2i.b) ) ? (5 * (0 * _2_a)) : ((((! id2i.a) && id2i.b) ) ? (5 * (0 * _1_a)) : (((id2i.a && (! id2i.b)) ) ? (5 * (1 * _2_a)) : (5 * (1 * _1_a)))));
}