# ghc-hs-meta

[![Hackage](https://img.shields.io/hackage/v/ghc-hs-meta)](https://hackage.haskell.org/package/ghc-hs-meta)

Generate Template Haskell expressions from Haskell source code using the GHC parser.

## Usage

Pass a String containing Haskell source code to `parseExp`.
Example from the tests:

```haskell
case parseExp "a @b" of
    Right exp -> exp `shouldBe` TH.AppTypeE (TH.VarE (TH.mkName "a")) (TH.VarT (TH.mkName "b"))
    Left (_, _, errMsg) -> error errMsg
```

See Hackage documentation for more documentation.

## Thank you, PyF

This code originated from the excellent parser included in the [`PyF`](https://github.com/guibou/PyF) package.
I extracted the relevant code and refactored/renamed things to be usable in a more general context.
Without PyF, this could wouldn't have been possible. Thank you!

The original license for PyF can be found in the `LICENSE-PyF` file included in this repository.
