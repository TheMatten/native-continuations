# `native-continuations` (WIP)

Library providing native delimited continuations implemented using GHC primops.

## Requirements

This library only works with GHC 9.6 or newer.

## Credits

Thanks to Alexis King for implementation of actual runtime primitives and for extensive documentation and hints on how to use them correctly.

See [proposal] for more information about internal details.

[proposal]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0313-delimited-continuation-primops.rst