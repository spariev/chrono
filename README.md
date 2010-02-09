# chrono

Chrono is the fork of incanter.chrono, awesome date utility library for Clojure written by Matt Moriarity, Phil Hagelberg, and Bradford Cross.

**But, why fork ?**

I did it for the following reasons:

* **Ease of installation and use** - currently you have include the whole Incanter as a dependency to use small date utility library. spariev.chrono depends only on JodaTime and can be easily used in your leiningen project.
* **JodaTime based** - spariev.chrono wraps JodaTime DateTime class, it doesn't make any use of java.util.Calendar and related classes. Still, you can use Calendar and java.util/java.sql.Date as the source for date object.
* **Locale-aware parsing/formatting** - library uses default system locale for parsing and formatting dates and strings, but you can set your own specific locale via binding.


## Installation

You can just add this project as a dependency like it says on (http://clojars.org/spariev/chrono).

## Usage

#TODO Write usage example

## License

Copyright (C) 2010 Matt Moriarity, Phil Hagelberg, Bradford Cross and Sergey Pariev

Distributed under the Eclipse Public License, the same as Clojure uses. See the file epl-v10.html
