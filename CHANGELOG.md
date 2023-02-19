Changelog for the `operational` package
---------------------------------------

**0.2.4.2** -- Maintenance release.

* Compatibility with `mtl-2.3.1`

**0.2.4.1** -- Maintenance release.

* Restrict dependencies to ensure GHC >= 7.10.

**0.2.4.0** -- Feature release.

* Update to build with GHC 9.0.1.
* Add utility functions `interpretWithMonadT`, `unviewT` and `mapInstr`
* Add utility `Functor`, `Applicative`, and `Monad` instances for `ProgramViewT` type.

**0.2.3.5** -- Maintenance release.

* Update references to other packages.
* Modernize `.cabal` file.

**0.2.3.4** -- Maintenance release.

* Restrict dependencies to ensure GHC >= 7.2.

**0.2.3.3** -- Maintenance release.

* Minor fixes to documentation and examples

**0.2.3.2** -- Maintenance release.

* Bump `mtl` dependency to allow 2.3

**0.2.3.1** -- Maintenance release.

* Bump `mtl` dependency to allow 2.2

**0.2.3.0** -- Maintenance release.

* added instance for `MonadReader` class
* clean up documentation

**0.2.2.0** -- Feature release.

* add utility function `interpretWithMonad`

**0.2.1.0** -- Maintenance release.

* minor change: eta-reduce `Program` and `ProgramView` type synonyms

**0.2.0.3** -- Maintenance release.

* moved project repository to github

**0.2.0.0** -- Feature release.

* changed name of view type to `ProgramView`
* added instances for  mtl  classes
* new function `liftProgram` to embed `Program` in `ProgramT`
* new example `TicTacToe.hs`
* various documentation updates

**0.1.0.0**

* initial release
