## Purpose 

A visualization of a "Lunar Rover" problem solution.

Written in Elm.

## The problem

This is a variation of the ["Jeep problem"](https://en.wikipedia.org/wiki/Jeep_problem).

The rover must make a full trip around the moon, with given constraints:

* Can carry one spare tank in addition to the fuel tank
* Can deposit (full) spare tank on the surface to use later
* Can fill the tank from the carried spare tank
* One tank is enough to cover 1/5 of the trip

The path around the moon is supposed to be circular (wrapping around).

## Requirements

You need to have [Elm installed](https://guide.elm-lang.org/install.html).

To install the dependencies, run:
```bash
$ make setup
```

The program was tested on Elm v0.18


## Building and running

First build by running:
```
$ make

```

After that, you can open the `index.html` file in your browser.

Running via elm-reactor is also possible, but it currently does not support a nice way to resolve path to external files, so be prepared to not seeing any graphics.


## Running unit tests

```bash 
$ make test
```

## Screenshot

![](assets/screen.png)


## Online demo

You can try the online demo [here](http://www.ruslans.com/lunar-viz).

Each step is displayed in the bottom part of the screen, one can click on a step to fast-forward/rewind.

## License 

The contents of this repository are covered under the [MIT License](LICENSE).
