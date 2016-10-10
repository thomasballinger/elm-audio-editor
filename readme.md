
##TODO

* zoom
* drag to change selections
* build list of jumps from clips
* move selections to delete or something

#Dev Setup

There's a build process, run make. I use https://github.com/mattgreen/watchexec
but watch works fine until the dependencies get beyond what I know how to
describe with a Makefile.

    watch make
    watchexec make

Requires a server that accespts range requests in dev, like

    pip install rangehttpserver
    python -m RangeHTTPServer

Open index.html.


##old plan

interface:
[ ] - list portions to skip
[ ] - load audio by url or podcast feed
[ ] - minimap of whole track
[ ] - up-close view
[ ] - drag boxes over parts you want to cut out in normal mode
[ ] - drag boxes over parts you want to keep and collect these
[ ] - another section for composing collected clips
[ ] - create a clip and name it
