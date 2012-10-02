using System;
using System.Collections.Generic;

// Mutable indirection node
class IVar<A> { public A it; }

// Recursive type
class Rec : Mul<bool, IVar<Rec>> {
  public Rec (bool pi1, IVar<Rec> pi2) : base (pi1, pi2) {}}

static partial class Program {
  static Queue<Action> ctors;

  static One Ctor (Action a) {
    if (ctors == null)
      ctors = new Queue<Action> ();

    ctors.Enqueue (a);
    return One.it; }

  static Program () {
    foreach (var a in ctors) a(); }

  static void PrintRec (Rec r) {
    Console.WriteLine (r.π1);
    PrintRec (r.π2.it); }

  static void Main () {
    PrintRec (a.it); }}

// Actual variables

static partial class Program {
  public static IVar<Rec> a = new IVar<Rec> ();

  static One ctor_a = Ctor (delegate {
    a.it = new Rec (true, b); }); }

static partial class Program {
  public static IVar<Rec> b = new IVar<Rec> ();

  static One ctor_b = Ctor (delegate {
    b.it = new Rec (false, a); }); }
