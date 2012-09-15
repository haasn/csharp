using System;

class Program { static void Main() {
    Maybe<int> m = new Just<int> (5);
    Maybe<int> n = new Nothing ().coerce<int> ();

    Console.WriteLine (m.elim (() => "Nothing", a => "Just " + a));
    Console.WriteLine (n.elim (() => "Nothing", a => "Just " + a)); }}

interface Maybe<A> {
    B elim<B> (Func<B> n, Func<A,B> j); }

// Parametrized Nothing

class Nothing_<A> : Maybe<A> {
    public B elim<B> (Func<B> n, Func<A,B> j) {
        return n (); }}

class Just<A> : Maybe<A> {
    A a;

    public Just(A a) { this.a = a; }

    public B elim<B> (Func<B> n, Func<A,B> j) {
        return j (a); }}


// Bottom type

class Bottom {
    public A coerce<A> () {
        throw new Exception ("⊥"); }}

// Proper Nothing

class Nothing : Nothing_<Bottom> {
    public Maybe<A> coerce<A> () {
        return FunctorDicts<Bottom,A>.MaybeD.Fmap
            (b => b.coerce<A> (), this); }}


// Functor dictionary

class FunctorDict<A,B,FA,FB> {
    public readonly Func<Func<A,B>,FA,FB> Fmap;

    public FunctorDict (Func<Func<A,B>,FA,FB> Fmap) {
        this.Fmap = Fmap; }
}

class FunctorDicts<A,B> {
    public static FunctorDict<A,B,Maybe<A>,Maybe<B>> MaybeD =
        new FunctorDict<A,B,Maybe<A>,Maybe<B>>
            ( (f,ma) => ma.elim<Maybe<B>> (() => new Nothing_<B> ()
                                          , a => new Just<B> (f (a))) ); }
