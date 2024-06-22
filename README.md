# OOpetris Haskell Wrapper


## What is this?

This wraps [oopetris](https://github.com/OpenBrickProtocolFoundation/oopetris) functionality for Haskell





## Platform support

We officially support Linux, Windows and MacOS, if any error occurs regarding those platform, feel free to open an issue

## How to obtain

You have to compile it yourself, we don't provide prebuilt-packages atm. The CI may produce some artifacts, that are useable, but not optimized for installation or usage.

## How to build it yourself

This uses stack to build the wrapper, you need to install `liboopetris_c_wrapper` (`oopetris-c-wrapper` in pkg-config terms) to be able to use this.

Than you just can invoke `stack build` to build the library adn examples.

You can run the example by invoking `stack run -- test/files/correct.rec`
