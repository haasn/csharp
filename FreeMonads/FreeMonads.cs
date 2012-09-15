using System;

// Example program using the below

class Program {
    static void Main() {
        var subroutine =
            from x in Toy.Output ("Foo")
            from y in Toy.Output ("Bar")
            select Unit.It;

        var program =
            from x in subroutine
            from y in Toy.Bell ()
            from z in Toy.Done ()
            select Unit.It;

        // Show program first
        Console.WriteLine (Toy.AsString (program));

        // Wait for keypress
        Console.WriteLine ("--- Press any key to run ---");
        Console.ReadKey (true);

        // Interpret normally
        Toy.Interpret (program);
    }
}

// Functor/Monad logic
interface Functor<A> {
    Functor<B> Fmap<B> (Func<A,B> f); }

class FreeMonad<A> {
    internal bool isFree;
    internal Functor<FreeMonad<A>> Free;
    internal A Pure;

    // Smart constructors
    public FreeMonad(A a) {
        isFree = false;
        Pure = a; }

    public FreeMonad(Functor<FreeMonad<A>> f) {
        isFree = true;
        Free = f; }

    // Bind implementation
    public FreeMonad<B> Bind<B> (Func<A, FreeMonad<B>> f) {
        return isFree
          ? new FreeMonad<B> (Free.Fmap (m => m.Bind (f)))
          : f (Pure) ;}

    // SelectMany in terms of Bind
    public FreeMonad<C> SelectMany<B,C> (Func<A, FreeMonad<B>> f, Func<A,B,C> s) {
        return Bind (a => f (a).Bind (b => new FreeMonad<C> (s (a, b)))); }

    // liftF injection
    public static FreeMonad<A> Lift (Functor<A> f) {
        return new FreeMonad<A> (f.Fmap (a => new FreeMonad<A> (a))); }
}

// Example: Toy language


// Unit type for ()
enum Unit { It }

class Toy {
    // Tagged sum type
    enum ToyT { Output, Bell, Done }

    // Internal functor type with non-public constructors
    class ToyF<N> : Functor<N> {
        internal ToyT Type;
        internal string Output;
        internal N Next;

        // Smart constructors
        internal ToyF(string b, N next) {
            Output = b;
            Next   = next;
            Type   = ToyT.Output; }

        internal ToyF(N next) {
            Next   = next;
            Type   = ToyT.Bell; }

        internal ToyF() {
            Type   = ToyT.Done; }

        // Functor implementation
        public Functor<T> Fmap<T> (Func<N,T> f) {
            switch (Type) {
                case ToyT.Output: return new ToyF<T> (Output, f (Next));
                case ToyT.Bell  : return new ToyF<T> (f (Next));
                case ToyT.Done  : return new ToyF<T> ();

                default: throw new Exception("Should never happen."); }}
    }

    // Lifted constructors
    public static FreeMonad<Unit> Done () {
        return FreeMonad<Unit>.Lift (new ToyF<Unit> ()); }

    public static FreeMonad<Unit> Bell () {
        return FreeMonad<Unit>.Lift (new ToyF<Unit> (Unit.It)); }

    public static FreeMonad<Unit> Output (string b) {
        return FreeMonad<Unit>.Lift (new ToyF<Unit> (b, Unit.It)); }

    // Normal interpreter for this toy language, with internal unboxing
    public static void Interpret (FreeMonad<Unit> p) {
        if (p.isFree) {
            var toy = (ToyF<FreeMonad<Unit>>) p.Free;
            switch (toy.Type) {
                case ToyT.Output:
                    Console.WriteLine (toy.Output);
                    Interpret (toy.Next);
                    break;

                case ToyT.Bell:
                    Console.WriteLine ("Bell!");
                    Interpret (toy.Next);
                    break;

                default: break; }}

        else throw new Exception("Incorrect termination."); }

    // Pure interpreter, outputs the program description as string
    public static string AsString (FreeMonad<Unit> p) {
        if (p.isFree) {
            var toy = (ToyF<FreeMonad<Unit>>) p.Free;
            switch (toy.Type) {
                case ToyT.Output: return "Output(" + toy.Output + ")\n" + AsString (toy.Next);
                case ToyT.Bell  : return "Bell()\n" + AsString (toy.Next);
                case ToyT.Done  : return "Done()";

                default: throw new Exception("Should never happen."); }}

        else return "<< CONT >>"; }
}
