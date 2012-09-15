using System;
using System.Linq;
using System.Collections.Generic;

// Natural numbers as fixed points of the (1+) constructor

class Maybe<A> {
  private bool isJust;
  private A just;

  public Maybe ()    { this.isJust = false; }
  public Maybe (A a) { this.isJust = true; this.just = a; }

  public B Elim<B> (Func<B> n, Func<A,B> f) {
    return isJust ? f (just) : n (); }}

class MFix : Maybe<MFix> {
  public MFix (MFix a) : base (a) {}
  public MFix ()       : base () {}}

// Seems to be missing in my stdlib ?

class Mul<A,B> {
  public readonly A a;
  public readonly B b;
  public Mul(A a, B b) { this.a = a; this.b = b; }}

// (Rose) trees as fixed points of the Λα f. (α, [f]) constructor

class Tree<A> : Mul<A, List<Tree<A>>> {
  public Tree (A a, params Tree<A>[] bs)
    : base (a, new List<Tree<A>> (bs)) {}}

class Program {
  static MFix toMFix (int i) {
    return i == 0
      ? new MFix ()
      : new MFix (toMFix (i-1)); }

  static int fromMFix (MFix m) {
    return m.Elim (() => 0, a => 1 + fromMFix (a)); }

  static int depth<A> (Tree<A> t) {
    return 1 + t.b.Select (a => depth (a)).Aggregate
      (0, (a,b) => Math.Max (a,b)); }

  static void Main () {
    var mfix = toMFix (7);
    Console.WriteLine (fromMFix (mfix));

    var tree =
      new Tree<int> (5
      , new Tree<int> (4, new Tree<int> (2))
      , new Tree<int> (6
        , new Tree<int> (7
          , new Tree<int> (8)
          , new Tree<int> (9, new Tree<int> (10)))));

    Console.WriteLine (depth (tree)); }}
