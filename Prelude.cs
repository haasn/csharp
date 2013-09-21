using System;

// Dicts

public class FunctorDict<A,B,FA,FB> {
  public readonly Func<Func<A,B>,FA,FB> F;

  public FunctorDict (Func<Func<A,B>,FA,FB> f) { F = f; }}

public class MonadDict<A,MA,MMA> {
  public readonly Func<A, MA>   η;
  public readonly Func<MMA, MA> μ;

  public MonadDict (Func<A, MA> eta, Func<MMA, MA> mu) { η = eta; μ = mu; }}

public static class Monad {
  public static MB Bind<A,MA,B,MB,MMB> ( FunctorDict<A,MB,MA,MMB> fd
                                       , MonadDict<B,MB,MMB> md
                                       , MA ma, Func<A,MB> f ) {
    return md.μ (fd.F (f, ma)); }}

// Type arithmetic

public class Mul<A,B> {
  public readonly A π1;
  public readonly B π2;
  public Mul (A pi1, B pi2) { π1 = pi1; π2 = pi2; }}

public class Add<A,B> {
  private bool dir;
  private Func<A> ι1;
  private Func<B> ι2;

  public Add (A i1) { dir = false; ι1 = () => i1; }
  public Add (B i2) { dir = true;  ι2 = () => i2; }

  public C Elim<C> (Func<A,C> a, Func<B,C> b) {
    return dir ? a (ι1 ()) : b (ι2 ()); }}

public sealed class Zero {
  private Zero () {}
  public A coerce<A> () { throw new Exception ("⊥"); }}

public sealed class One {
  private One () {}
  public static readonly One it = new One (); }
