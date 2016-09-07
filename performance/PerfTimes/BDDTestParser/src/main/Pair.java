package main;


/**
 * A generic Triple class based on Pair.java
 */
public class Pair<A, B> {
    public A _1;
    public B _2;

    public Pair(A first, B second) {
        this._1 = first;
        this._2 = second;
    }

    public static <A, B, C> Pair<A, B> of(A first, B second) {
      return new Pair<A, B>(first, second);
    }
    
    public A getFirst() { return _1; }
    public B getSecond() { return _2; }

    @Override
    public String toString() {
        return "(" + _1 + ", " + _2 + ")";
    }

    private static boolean equals(Object x, Object y) {
        return (x == null && y == null) || (x != null && x.equals(y));
    }

    @Override
    public boolean equals(Object other) {
    return (other instanceof Pair<?,?>)
        && equals(_1,  ((Pair<?,?>)other)._1)
        && equals(_2, ((Pair<?,?>)other)._2);
    }

    @Override
    public int hashCode() {
        if (_1 == null && _2 == null) return 0;
        else if (_1 == null) return _2.hashCode() + 2;
        else if (_2 == null) return _1.hashCode() + 3;
        else return _1.hashCode() * 17 + _2.hashCode() * 5;
    }
}
