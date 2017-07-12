# pine

## Setup

### Stack ###

#### Building this project ####

```
stack new pine
cd pine
stack build pine
stack exec pine-exe
```

#### Dependencies ####

Not sure if I have to manually do this. I updated the pine.cabal file with all
the dependencies after installing `wreq`.

```
stack install wreq
```

### [obsolete] Cabal ###

Initial setup:

```
cabal sandbox init
```

Dependencies
```
cabal install -j --disable-tests wreq
# cabal install parsec
# cabal install hspec
```

