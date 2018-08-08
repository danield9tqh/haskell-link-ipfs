# haskell-link-ipfs

A quick implementation of a Haskell global namespace using IPFS hashes

# Motivation

Programming languages have long had issues with dependency management. The struggle centers around which code is actually being imported. For a particular library, which version of the code is used? Additionally, in the case of similar libraries there may be name-spacing issues. Increasingly, this problem is being solved in many environments by referencing code by its hash value. [IPFS](https://github.com/ipfs/ipfs) provides a very low level implementation of a global file namespace using hashes, so it seems like perfect fit solve this problem. This repo is meant to be a proof of concept of this solution, as well as a learning experience for myself. I don't intend it to be a fully featured solution, although I hope to eventually work on a project that will be.

# Usage
### Command Line Utility
This distribution includes a command line utility `haskell-ipfs` which attempts to link Haskell `import` statements by their IPFS hash value. To install the command, you will need haskell's Stack build utility. Then just follow these steps

```
git clone https://github.com/danield9tqh/haskell-link-ipfs.git
cd haskell-link-ipfs
stack install
```

Now you should have the utility `haskell-ipfs` installed in `/Users/{username}/.local/bin/haskell-ipfs` (on Unix)

Once the utility is installed, you can try it on a sample project. The utility takes as input, the entry haskell. Below is a simple Haskell file (`Main.hs`) that imports the utility method `sayHello`. If the correct Util module is linked, this code should print "Hello, IPFS".
```haskell
module Main where
import Util (sayHello)

main :: IO ()
main = print (sayHello "IPFS")
```

### Linking a file
In the same directory, we'll create another file, `Main.ipfs`, specifying the IPFS hash of the imports we want to link.
```
Util: QmYtfmQZ9HYsqkiqGNUoNcLDW2ngNk34Rv92qD5mXSAvco
```

This hash, although still a valid, may not be hosted by anyone on the IPFS network so you may have to host it yourself. Here is the source of the Util.hs file and should give the same hash value when added to IPFS. (For more instructions on IPFS usage, see the website: https://ipfs.io/docs/getting-started/)

```haskell
module Util (sayHello) where

sayHello :: String -> String
sayHello name = "Hello, " ++ name

```

Finally, we can run the command to fetch all the necesarry files from IPFS. 
```
haskell-ipfs Main.hs
```
This should fetch the file `Util.hs` from IPFS and move it to the same directory as `Main.hs`. Once it is in the same directory we can use `runhaskell Main.hs` and we should see "Hello, IPFS".

### Example
You can check out an example implementation over at https://github.com/danield9tqh/haskell-link-ipfs-example
