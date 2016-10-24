![try it out](http://remixcast.com/editor/)

##TODO

* preview mix
* build list of jumps from clips
* zoom
* drag to change order
* move selections to delete or something
* save current setup OR maybe outputing a remix spec is enough?

#Dev Setup

There's a build process, run make. I use https://github.com/mattgreen/watchexec
but watch works fine until the dependencies get beyond what I know how to
describe with a Makefile.

    watch make
    watchexec --ignore bundle.js --ignore bundle.js.map make
    # ignores shouldn't be necessary if makefile is good

Requires a server that accespts range requests in dev, like

    pip install rangehttpserver
    python -m RangeHTTPServer

Open index.html.
