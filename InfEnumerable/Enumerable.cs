using System;
using System.Linq;
using System.Collections.Generic;

// A few basic types needed

class Mul<A,B> {
  public readonly A π1;
  public readonly B π2;
  public Mul(A pi1, B pi2) { π1 = pi1; π2 = pi2; }}

class Add<A,B> {
  private bool dir;
  private Func<A> ι1;
  private Func<B> ι2;

  public Add (A i1) { dir = true;  ι1 = () => i1; }
  public Add (B i2) { dir = false; ι2 = () => i2; }

  public C Elim<C> (Func<A,C> a, Func<B,C> b) {
    return dir ? a (ι1 ()) : b (ι2 ()); }}

// Note the class Zero, which can *never* be instantiated,
// as it has no public constructors, the private one is never used,
// and it's sealed.

sealed class Zero {
  private Zero () {}
  public A coerce<A> () { throw new Exception ("⊥"); }}

sealed class One {
  private One () {}
  public static readonly One it = new One (); }

// Since the built in one doesn't suffice here, I'll make my own
// generalized enumerable - technically a free monad of a product
// functor, but I'll define it directly for sake of simplicity.

interface IEnumerableF<A, F> {
  // Read in english: The next element in the sequence is either
  // an ‘F’ (final value) or an ‘A’ together with another sequence
  Add<F, Mul<A, IEnumerableF<A, F>>> Next { get; }}

// Note that IEnumerable<A> ≅ IEnumerableF<A, One>

// I want to make List<A> : IEnumerableF<A, One> but apparently
// I can't make existing classes instances of interfaces, so I'll
// just add a wrapper class to demonstrate.

class MyList<A> : List<A>, IEnumerableF<A, One> {
  public Add<One, Mul<A, IEnumerableF<A, One>>> Next { get { return
    (this.Count == 0)
      // If the list is empty: return the unit (one)
      ? new Add<One, Mul<A, IEnumerableF<A, One>>> (One.it)

      // Otherwise, return the first element + a new enumerable,
      // which is simply the rest of the list (skip 1) wrapped in MyList
      : new Add<One, Mul<A, IEnumerableF<A, One>>>
          (new Mul<A, IEnumerableF<A, One>>
            (this.First (),
             new MyList<A> (this.Skip (1)))) ;}}

  // Note: IEnumerable here refers to the one from System, not mine
  public MyList (IEnumerable<A> xs) : base (xs) {}}

// A possible IEnumerableF<A, Zero>, a natural number counter

class NatFrom : IEnumerableF<ulong, Zero> {
  private ulong n;

  public Add<Zero, Mul<ulong, IEnumerableF<ulong, Zero>>> Next { get {
    return new Add<Zero, Mul<ulong, IEnumerableF<ulong, Zero>>>
      (new Mul<ulong, IEnumerableF<ulong, Zero>>
        (n, new NatFrom (n+1))); }}

  public NatFrom (ulong n) { this.n = n; }}

// Usage examples

static class Program {
  static void Main () {
    var list = new MyList<int> (new int[] { 1, 2, 3, 4 });

    PrintAll (list);

    var fromfive = new NatFrom (5);

    // The following would be a type error, since One ≠ Zero
    // PrintForever (list);

    // However, this works, because it's guaranteed infinite
    PrintForever (fromfive); }

  // Print all members of an IEnumerableF, stopping when we reach F
  static F PrintAll<A,F> (IEnumerableF<A,F> xs) {
    return xs.Next.Elim (f => f,
      delegate (Mul<A, IEnumerableF<A,F>> a) {
        Console.WriteLine (a.π1);
        return PrintAll (a.π2); }); }

  // A modified variant, this one only accepts enumerables where F=Void,
  // the implementation is literally the same.

  // Note that for this to return, a left Zero would have to be produced
  // by the IEnumerableF, which is impossible because Zero has no constructors.
  // As such, it's impossible for the IEnumerableF to ever stop generating,
  // unless it throws an exception. But that is none of our business, since
  // we aren't necessarily writing the code for it - not our bug.

  static Zero PrintForever<A> (IEnumerableF<A, Zero> xs) {
    return PrintAll (xs); }
}
