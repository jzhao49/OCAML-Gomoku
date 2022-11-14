# OCAML-Gomoku

This project creates an OCAML version of the popular game Gomoku! It allows for multiplayer games to be played on the same machine or a single player verison where the user plays against a minimax AI!

## Prerequisites

- Node.js (tested using v19.0.1) and the npm CLI (tested using v9.1.1)
- OCaml 4.14.0

## Build instructions

- From the `backend` directory, run `opam install .` to install all necessary dependencies
- From the 'frontend' directory, run `npm install` to install all necessary dependencies
    - Then `npm run res:build` and `node src/App.bs.js`