using System;

static class Program {
    static void Main () {
        Func<Mu, int> toInt =
            n => n.Catamorphism (Nat.Phi (0, x => x+1));

        Mu mynum = Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Zero ())))));

        // Factorial
        Func<Mu, Mu> fac =
            n => Mu.Hylomorphism<Mu, Mu>
                ( List.Phi (Nat.Succ (Nat.Zero ()), (a,b) => Nat.Mul (a,b))
                , mu => ((Nat<Mu>) mu.Out).Elim
                    ( new List<Mu, Mu> ()
                    , x => new List<Mu, Mu> ( Nat.Succ (x), x )
                    )
                , n
                );

        Console.WriteLine
            ( "fac {0} = {1}"
            , toInt (mynum)
            , toInt (fac (mynum))); }
}

// Interface for the functor, with fmap

// Note that due to limitations in the C# type system a lot of functions
// need to be boxed/unboxed to/from this interface type.

interface Functor<A> {
    Functor<B> Fmap<B> (Func<A,B> f);
}

// Functor fixed point

class Mu {
    public Functor<Mu> Out { get; private set; }

    public Mu (Functor<Mu> f) {
        this.Out = f; }

    public R Catamorphism<R> (Func<Functor<R>, R> phi) {
        return phi (Out.Fmap(mu => mu.Catamorphism(phi))); }

    public static Mu Anamorphism<R> (Func<R, Functor<R>> psi, R seed) {
        return new Mu (psi (seed).Fmap( r => Mu.Anamorphism (psi, r) )); }

    public static B Hylomorphism<A,B> (Func<Functor<B>, B> phi, Func<A, Functor<A>> psi, A seed) {
        return Mu.Anamorphism<A> (psi, seed).Catamorphism<B> (phi); }
}

// Example functor: List

class List<A,F> : Functor<F> {
    private bool isNull;
    public A item { get; private set; }
    public F tail { get; private set; }

    public List (A a, F f) {
        this.item = a;
        this.tail = f;
        this.isNull = false; }

    public List () {
        this.item = default (A);
        this.tail = default (F);
        this.isNull = true; }

    // Elimination primitive
    public R Elim<R> (R z, Func<A, F, R> f) {
        if (this.isNull)
             return z;
        else return f (item, tail);
    }

    // Functor implementation
    public Functor<R> Fmap<R> (Func<F,R> f) {
        if (this.isNull)
             return new List<A,R> ();
        else return new List<A,R> (item, f (tail)); }
}

// Class for static methods
class List {
    // Phi builder via elim
    public static Func< Functor<A>, A > Phi<A> (A a, Func<A, A, A> f) {
        return list => ((List<A, A>) list).Elim (a, f); }

    // Psi builder
    public static Func< A, Functor<A> > Psi<A> (Func<A, bool> p, Func<A, A> pi1, Func<A, A> pi2) {
        return a =>
            /*if*/ p (a)
            /*then*/? new List<A,A> ()
            /*else*/: new List<A,A> (pi1 (a), pi2 (a)); }

    // Mu-lifted cons / empty
    public static Mu Empty<A> () {
        return new Mu (new List<A,Mu> ()); }

    public static Mu Cons<A> (A a, Mu mu) {
        return new Mu (new List<A,Mu> (a, mu)); }
}

// Another example functor: Natural numbers

class Nat<F> : Functor<F> {
    private bool isNull;
    public F pred { get; private set; }

    public Nat () {
        this.pred = default (F);
        this.isNull = true; }

    public Nat (F f) {
        this.pred = f;
        this.isNull = false; }

    // Elimination primitive
    public R Elim<R> (R z, Func<F, R> f) {
        if (this.isNull)
             return z;
        else return f (pred); }

    // Functor implementation
    public Functor<R> Fmap<R> (Func<F,R> f) {
        if (this.isNull)
             return new Nat<R> ();
        else return new Nat<R> (f (pred)); }
}

// Static methods
class Nat {
    // Phi builder
    public static Func< Functor<A>, A > Phi<A> (A a, Func<A, A> f) {
        return nat => ((Nat<A>) nat).Elim (a, f); }

    // Psi builder
    public static Func< A, Functor<A> > Psi<A> (Func<A, bool> p, Func<A, A> pi) {
        return a =>
            /*if*/ p (a)
            /*then*/? new Nat<A> ()
            /*else*/: new Nat<A> (pi (a)); }

    // Mu-lifted succ / empty
    public static Mu Zero () {
        return new Mu (new Nat<Mu> ()); }

    public static Mu Succ (Mu n) {
        return new Mu (new Nat<Mu> (n)); }

    public static Mu Add (Mu a, Mu b) {
        return b.Catamorphism ( Phi (a, n => Succ (n))); }

    public static Mu Mul (Mu a, Mu b) {
        return b.Catamorphism ( Phi (Zero (), n => Add (a, n))); }
}
