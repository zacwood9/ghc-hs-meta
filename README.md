# ghc-meta

Generate Template Haskell expressions from Haskell source code using the GHC parser.

## Thank you, PyF

This code originated from the excellent parser included in the [`PyF`](https://github.com/guibou/PyF) package. 
I extracted the relevant code and refactored/renamed things to be usable in a more general context.
Without PyF, this could wouldn't have been possible. Thank you!

The original license for PyF can be found in the `LICENSE-PyF` file included in this repository.

## Usage

See `test/Main.hs` for how to use this package.

```haskell
case parseExp "a @b" of
    Right exp -> exp `shouldBe` TH.AppTypeE (TH.VarE (TH.mkName "a")) (TH.VarT (TH.mkName "b"))
    Left (_, _, errMsg) -> error errMsg
```