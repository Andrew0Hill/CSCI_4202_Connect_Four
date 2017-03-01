# CSCI 4202 Connect Four Agent

Project for CSCI 4202 Artificial Intelligence.

Implementation of Alpha-Beta Pruning for Connect Four in Clojure.

## Installation

Clone this repository. This is a Leiningen Project developed using Cursive. Use Leiningen to build the project into an executable JAR using `lein uberjar`. 

If you don't want to build it yourself, you can download the latest compiled JAR from the Releases page.

The program can be run using `java -jar <name>`.

## Use

The program runs from the commandline, and accepts a JSON object containing the game's current state. The JSON object is structured as follows:

`{"grid":[[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]],"height":6,"player":2,"width":7}` 

The grid represents the game board as a column-major vector of vectors. The program will analyze the game's current state and return its chosen move in a JSON object of the form:

`{"move":2}`


