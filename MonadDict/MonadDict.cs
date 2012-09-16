using System;
using System.Collections.Generic;
using System.Linq;

class Program {
  // Example of polymorphic code: Doubling a number
  // Another way would be dict.Mul(a, dict.FromInt(2))
  public static A Double<A> (NumDict<A> dict, A a) {
    return dict.Add(a, a); }


  // Example of polymorphic higher-kind code: Mapping double over a functor
  public static FA MapDouble<A,FA> (NumDict<A> nd, FunctorDict<A,A,FA,FA> fd, FA fa) {
    return fd.Fmap (a => Double<A> (nd, a), fa); }

  // Example of monad code: Join in terms of bind
  public static MA Join<A,MA,MMA> (MonadDict<MA,A,MMA,MA> md, MMA mma) {
    return md.Bind (mma, ma => ma); }

  public static void Main() {
    var list = new List<int> (new int[] { 1, 3, 5, 6, 8 });

    var test = MapDouble (NumDicts.IntD, FunctorDicts<int,int>.ListD, list);

    foreach (var x in test)
        Console.WriteLine (x);

    Console.WriteLine ("-----");

    var foo = new List<List<int>> ();
    foo.Add(list);
    foo.Add(test);

    var flatten = Join (MonadDicts<List<int>, int>.ListD, foo);

    foreach (var x in flatten)
        Console.WriteLine (x);

    // Example of ‘cool stuff’ in use:
    var sinTwice = Double (NumDicts.FunD<double,double> (NumDicts.DoubleD), Math.Sin);

    Console.WriteLine ("2 * sin 2 = {0}", sinTwice (2)); }}

// Warming up

class NumDict<A> {
  public readonly Func<A,A,A> Add;
  public readonly Func<A,A,A> Mul;
  public readonly Func<A,A> Negate;
  public readonly Func<A,A> Abs;
  public readonly Func<int,A> FromInt;

  // zzz
  public NumDict (Func<A,A,A> Add, Func<A,A,A> Mul, Func<A,A> Negate, Func<A,A> Abs, Func<int,A> FromInt) {
    this.Add = Add; this.Mul = Mul; this.Negate = Negate; this.Abs = Abs; this.FromInt = FromInt; }}

class NumDicts {
  public static NumDict<Int32> IntD = new NumDict<Int32>
    ( (a,b) => a+b
    , (a,b) => a*b
    , a => -a
    , a => Math.Abs (a)
    , a => a
    );

  public static NumDict<Double> DoubleD = new NumDict<Double>
    ( (a,b) => a+b
    , (a,b) => a*b
    , a => -a
    , a => Math.Abs (a)
    , a => a
    );

  // Example of ‘cool stuff’: Num b => Num (a -> b)
  public static NumDict<Func<A,B>> FunD<A,B> (NumDict<B> BD) { return new NumDict<Func<A,B>>
    ( (a,b) => x => BD.Add (a (x), b (x))
    , (a,b) => x => BD.Mul (a (x), b (x))
    , a => x => BD.Negate (a (x))
    , a => x => BD.Abs (a (x))
    , a => x => BD.FromInt (a)
    ); }}

// Higher-order type

class FunctorDict<A,B,FA,FB> {
  public readonly Func<Func<A,B>,FA,FB> Fmap;

  public FunctorDict (Func<Func<A,B>,FA,FB> Fmap) {
    this.Fmap = Fmap; }}

class FunctorDicts<A,B> {
  public static FunctorDict<A,B,List<A>,List<B>> ListD = new FunctorDict<A,B,List<A>,List<B>>
    ( (f,la) => new List<B> ( la.Select (f) )
    );
}

// Pointed

class MonadDict<A,B,MA,MB> {
  public readonly Func<A, MA> Return;
  public readonly Func<MA, Func<A,MB>, MB> Bind;
  // Alternatively: MonadDict<A,MA,MMA> ... Func<MMA, MA> Join

  public MonadDict (Func<A, MA> Return, Func<MA, Func<A,MB>, MB> Bind) {
    this.Return = Return; this.Bind = Bind; }}

class MonadDicts<A,B> {
  public static MonadDict<A,B,List<A>,List<B>> ListD = new MonadDict<A,B,List<A>,List<B>>
    ( a      => new List<A> ( new A[] { a })
    , (la,f) => new List<B> ( la.SelectMany<A,B,B> (a => f (a), (_,b) => b) )
    ); }
