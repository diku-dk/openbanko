# OpenBanko: Scaling for the Elder Generation

Tools and toys for Big Banko processing

[![Build Status](https://travis-ci.org/diku-dk/openbanko.svg?branch=master)](https://travis-ci.org/diku-dk/openbanko)


## Bankopladeformat

The specification for banko cards: [bankopladeformat.md](bankopladeformat.md)


## Utilities

+ [bankoconv](bankoconv/README.md): Convert between banko formats
+ [bankopak](bankopak/README.md): Compress and decompress banko boards
+ [bankopladeformat](bankopladeformat/README.md): Perform miscellaneous
  operations on banko cards in the bankopladeformat format
+ [bankoviser](bankoviser/README.md): Interact with banko boards in the terminal
+ [barc](barc/README.md): Check if boards are valid banko boards -- also
  generalizes to other games through a DSL
+ [cbankosim](cbankosim/): Automatically simulate playing banko


## Citation

If you use any of this code for research, please cite it as:

    @Misc{bigbanko,
      author =   {Henriksen, Troels; Fuck, Brain; Serup, Niels Gustav Westphal},
      title =    {Big Banko},
      institution = {EggsML}
      year = {2015--2018}
    }
