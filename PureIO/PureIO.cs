using System;
using System.Collections.Generic;
using System.Linq;

class IO<A> {
  private bool pure;
  private A a;

  private Func<object, object> f;
  private object i;
  private Func<object, IO<A>> o;

  public A unsafePerform () { return
    pure ? a : o (f (i)).unsafePerform (); }

  // Monad/Functor stuff
  public static FunctorDict<A,B,IO<A>,IO<B>> FunctorD<B> () { return
    new FunctorDict<A,B,IO<A>,IO<B>> ( (f, fa) => fmap<B> (f, fa) );}

  private static IO<B> fmap<B> (Func<A,B> f, IO<A> fa) { return
    fa.pure
      ? IO<B>.Pure (f (fa.a))
      : IO<B>.FFI<object,object> (fa.f, fa.i, x => fmap (f ,fa.o (x))) ;}

  public static MonadDict<A, IO<A>, IO<IO<A>>> MonadD =
    new MonadDict<A,IO<A>,IO<IO<A>>> ( a => Pure (a) , m => join (m) );

  private static IO<A> join (IO<IO<A>> m) { return
    m.pure ? m.a : FFI<object,object> (m.f, m.i, x => join (m.o (x))); }

  public IO<B> Bind<B> (Func<A,IO<B>> f) { return
    Monad.Bind<A,IO<A>,B,IO<B>,IO<IO<B>>>
      ( FunctorD<IO<B>> (), IO<B>.MonadD, this, f); }

  // Smart constructors
  public static IO<A> Pure (A a) {
    var io = new IO<A> ();
    io.pure = true;
    io.a = a;
    return io; }

  public static IO<A> Act (Func<object,A> f) {
    return Lift<object> (f) (null); }

  public static Func<B,IO<A>> Lift<B> (Func<B,A> f) {
    return a => FFI<B,A> (f, a, b => Pure (b)); }

  private static IO<A> FFI<I,O>(Func<I,O> f, I i, Func<O,IO<A>> o) {
    var io = new IO<A> ();
    io.pure = false;
    io.f = (x => f ((I) x));
    io.i = i;
    io.o = (x => o ((O) x));
    return io; }}

class Program {
  static Func<string, IO<One>> putStrLn = IO<One>.Lift<string>
    ( delegate (string x) { Console.WriteLine (x); return One.it; });

  static IO<string> getLine = IO<string>.Act ( x => Console.ReadLine () );

  public static void Main () {
    var x = getLine.Bind (putStrLn);

    x.unsafePerform ();
    x.unsafePerform ();
  }}
