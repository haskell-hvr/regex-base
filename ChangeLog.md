See also http://pvp.haskell.org/faq

## 0.94.0.2

2021-11-16 Andreas Abel

- Allow `text-2.0`
- Remove unused dependency `mtl`
- Warning free up to GHC 9.2
- Tested with GHC 7.0 - 9.2

## 0.94.0.1 Revision 1

2021-08-12 Andreas Abel

- Allow `base-4.16`, for GHC 9.2

## 0.94.0.1

2021-02-20 Andreas Abel

- Workaround for `{-# LANGUAGE Haskell2010 #-}` parser regression introduced in GHC 9.0
- Optimization flag `-O2` has been removed

## 0.94.0.0

2019-09-25 Herbert Valerio Riedel

- **Breaking change**: Switch RegExp API from the previously used `Monad(fail)` to `MonadFail(fail)` to denote matching failures
- Define `Extract Text` instances for strict and lazy `Text` types
- Compatibility with `base-4.13.0`
- Explicitly declare all modules `Safe` under SafeHaskell for GHC 7.4 and higher

----
