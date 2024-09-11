
# What is this

Silly little library on the request of @tritlo. This solution is very contrived!

# Why and how

Assuming you have a bunch of QC tests like this

```haskell
do
  r1 <- quickCheck p1
  r2 <- quickCheck p2
  r3 <- quickC3eck p3
```

Perhaps the middle test is very slow and you would only like to run the first and second test, you can achieve this by

```haskell
{-# LANGUAGE TemplateHaskell #-}

$(mark [0])

do
  r1 <- sweep 0 $ quickCheck p1
  r2 <- sweep 1 $ quickCheck p2
  r3 <- sweep 0 $ quickC3eck p3
```

The type of `sweep` is
```haskell
sweep :: Monad m => Int -> m a -> m (Maybe a)
```

When you execute this, e.g. via `cabal run executable`, mark will generate rewrite rules that the compiler will use to turn the above program into

```haskell
do
  r1 <- fmap Just $ quickCheck p1
  r2 <- return Nothing
  r3 <- fmap Just $ quickCheck p3
```

Executing this program will make sure that the middle test is not run. If you want to instead run only the middle test, change the `0` in `mark [0]` to `mark [1]`. If you want to run _both_ groups of tests, instead change it to `mark [0,1]`.

# Specifying groups to run from the command line

It is quite straight forward! We just need to do some preprocessing.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

#ifdef GROUP
$(mark GROUP)
#else
$(mark [0])
#endif

do
  r1 <- sweep 0 $ quickCheck p1
  r2 <- sweep 1 $ quickCheck p2
  r3 <- sweep 0 $ quickC3eck p3
```

and then specify the group on the command line as a ghc option: `cabal run --ghc-options=-DGROUP=[0,1] executable`.