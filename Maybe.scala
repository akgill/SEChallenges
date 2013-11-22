sealed trait Maybe[+A] {
      import Maybe._
     
      // The single abstract method to be used for implementation of all remaining
      // methods. In each case, replace ??? with an implementation that satisfies
      // the type signiature.
      def cata[X](some: A => X, none: => X): X
    
      def map[B](f: A => B): Maybe[B] = cata(a => some(f(a)), none)
      def flatMap[B](f: A => Maybe[B]): Maybe[B] = cata(a => f(a), none)
      def getOrElse[AA >: A](e: => AA): AA = cata(aa => aa, e)
      def filter(p: A => Boolean): Maybe[A] = cata(a => if (p(a)) this else none, this)
      def foreach(f: A => Unit): Unit = cata(a => f(a), ())
      def isDefined: Boolean = cata(a => true, false)
      def isEmpty: Boolean = !isDefined
      def get: A = cata(a => a, sys.error("Can't call get on None"))
      def orElse[AA >: A](o: Maybe[AA]): Maybe[AA] = cata(aa => this, o)
      def toLeft[X](right: => X): Either[A, X] = cata(Left(_), Right(right))
      def toRight[X](left: => X): Either[X, A] = cata(Right(_), Left(left))
      def toList: List[A] = cata(a => a::Nil, Nil)
      def iterator: Iterator[A] = cata(a => Iterator(a), Iterator())
      def equalTo[AA >: A](other: Maybe[AA])(implicit aeq: (AA, AA) => Boolean): Boolean = cata(aa => aeq(aa, other.get), other.isEmpty)
    }
     
    // Companion object which provides constructors for the only two members of the
    // sum type 'Maybe.' No modifications should be made to this object.
    object Maybe {
      def none[A] = new Maybe[A] {
        def cata[X](s: A => X, n: => X) = n
      }
     
      def some[A](a: A) = new Maybe[A] {
        def cata[X](s: A => X, n: => X) = s(a)
      }
    }
