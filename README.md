# HapiDoc

HapiDoc is a Haskell library for parsing and interacting with [TEI](https://tei-c.org/) XML
[EpiDoc](https://epidoc.stoa.org/) files.

HapiDoc has been designed for use, in the first instance, 
with the [I.Sicily](http://sicily.classics.ox.ac.uk/) corpus.
For information on the encoding of I.Sicily texts in TEI EpiDoc, see
the [I.Sicily GitHub wiki](https://github.com/ISicily/ISicily/wiki).

**NB: HapiDoc is currently under active development.**

## Monoids

Inspired by the approach of [HaTeX](https://github.com/Daniel-Diaz/HaTeX), the Haskell LaTeX library, HapiDoc models an EpiDoc document as a monoid. For further details on the behaviour of monoids, see the [HaTeX guide](https://github.com/Daniel-Diaz/hatex-guide). 


## Acknowledgements

The software for HapiDoc was written by Robert Crellin as part of the Crossreads project at the Faculty of Classics, University of Oxford, and is licensed under the BSD 3-clause license. This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme (grant agreement No 885040, “Crossreads”).

Example and test ```.xml``` files are either directly form, or derived from, the [I.Sicily corpus](https://github.com/ISicily/ISicily), which are made available under the [CC-BY-4.0 licence](https://creativecommons.org/licenses/by/4.0/).
