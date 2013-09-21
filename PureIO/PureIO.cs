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
    new MonadDict<A,IO<A>,IO<IO<A>>> ( a => Pure (a) , m => Join (m) );

  public static IO<A> Join (IO<IO<A>> m) { return
    m.pure ? m.a : FFI<object,object> (m.f, m.i, x => Join (m.o (x))); }

  public IO<B> Bind<B> (Func<A,IO<B>> f) { return
    Monad.Bind ( FunctorD<IO<B>> (), IO<B>.MonadD, this, f); }

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

static class IO {
  // Linq interface
  public static IO<C> SelectMany<A,B,C> (this IO<A> m, Func<A,IO<B>> f, Func<A,B,C> g) {
    return m.Bind (a => f (a).Bind (b => IO<C>.Pure (g (a,b)))); }}

class Program {
  static Func<string, IO<One>> putStrLn = IO<One>.Lift<string>
    ( delegate (string x) { Console.WriteLine (x); return One.it; });

  static IO<string> getLine = IO<string>.Act ( x => Console.ReadLine () );

  public static void Main () {
    var main =
      from _0 in putStrLn ("Please enter your name: ")
      from name in getLine
      from _1 in putStrLn ("Hello, " + name + "!")
      from _2 in putStrLn ("Goodbye, " + name + "!")
      select One.it;

    main.unsafePerform ();
  }}
