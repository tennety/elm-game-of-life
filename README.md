# Elm Game of Life

This project is a very basic implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway's_Game_of_Life) in [Elm](https://elm-lang.org). I have been using it as a hobby project to teach myself Elm.

The game features a square grid of 100 &times; 100 cells, with the edges wrapping around, so that cells on any edge treat cells on the opposite edge as neighbors. This is different from an "infinite" grid, so some patterns may not behave as expected. I used this technique for simplicity in calculating each cell's neighbors from one generation to the next.

With the primary purpose of this project being learning the language, it is not yet optimized for performance or even accuracy. It is not meant as general purpose Game of Life simulation software. There is, however a [tested](tests/RleParserTest.elm) [RLE](https://www.conwaylife.com/wiki/Run_Length_Encoded) [parser](src/RleParser.elm) built into it, which can be used to seed the grid with patterns. There are also a few built-in patterns (which are also written in RLE and parsed).
