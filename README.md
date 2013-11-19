Simple Energy Coding Challenge - Junior Big Data Engineer
=========================================================

Part 1: Functional Thinking
---------------------------

I read about catamorphisms to help with the implementations here.  In searching for information about catamorphisms, I also stumbled across Tony Morris's catamorphisms MyOption problem, from which this seems derived.  However, my solutions here are my own work.


Part 2: Monads
--------------

The implementation of `fmap` in trait `Monad` calls `fmap` as defined in trait `Functor`.

The implementations of `point` and `bind` in `IdMonad`, `MaybeMonad`, and `ReaderMonad` all call `point` and `bind` as defined in trait `Monad`.
