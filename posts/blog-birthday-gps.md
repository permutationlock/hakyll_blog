---
title: A birthday GPS
author: A.
published: August 19, 2023
tags: electronics
---

Around seven years ago I bought a Raspberry Pi, a GPS HAT, and a webcam.
The idea was to build a simple device to attach to bike handlebar bag and
generate a geotagged timelapse of a bike tour.
As these things often go, I got distracted with other projects.

A while later, I came back to
the project with the new idea to make a GPS console with a 7" touchscreen
display.
Alas, I became obsesed with algebraic geomtery. I ended up doing
nothing but mathematics for four years, completely 
forgotting about my electronics box... Until a few months ago.

My parents are really into the idea of vanlife. Over
the past several years they have slowly turned a Promaster into a really cool
adventure camper. With my dad's birthday coming I up, I had the idea to
drag my old GPS project out from the basement and construct a
dashboard console for their van.

![I know, I did a horrible job soldering.](../images/blog-birthday-gps-solder.jpg)

The first step was to solder the GPS HAT pins, and also solder a pair of wires
to connect the touchscreen to the +5v and GND lines on the GPS HAT (since the
hat commandeers all of the Pi's GPIO pins).

Next, I dumped the lastest Raspbery Pi OS image onto a 32GB micro SD card
and booted the thing up. Setting up
up [gpsd](https://gpsd.io/) was easy enough,
I simply followed the [provided instructions][6].
Realizing that a GPS antenna needs to have a view of the sky, I bought an
external antenna that could be placed out on the
roof of the van.

![A magnetic GPS antenna for the van roof.](../images/blog-birthday-gps-antenna.jpg)

I was finally at the point where I could think about software. I first
planned to use an editor like [Viking][1] with
[OpenStreetMaps][2] (OSM) data. Unfortunately, the Viking interface was far from
ideal on a small touch screen, and it had far more features than my device
needed. So I needed a new map viewer.

It turns out, I also needed new maps. It is simple enough downloaded OSM files
for the entire US, Mexico, and Canada. However, the raw data available from
OpenStreetMaps is just that: binary data
(or XML data). In order to actually view a map using such a file, a set of
256x256 pixel map tiles (or vector tiles) must be rendered for the given
region and zoom level.

![The console adhered to the dash with 3M
velcro.](../images/blog-birthday-gps-console.jpg)


OSM provides [their own tile server][3], but
many of the places that my parents plan to travel will not have cell service.
I couldn't just pull tiles from OSM on the fly.

The birthday was closing in,
and I wasn't having much luck. I started planning to buy a different present and
look into a longer term project that involved writing a touch screen friendly
map viewer myself.

When all seemed lost, I stumbled upon [FoxtrotGPS][4]!
Foxtrot is a wonderfully lightweight piece of
software, designed specifically for small touchscreen devices.
It provides a built in way
to cache maps for offline use by simply downloading tiles for a region at
selected zoom levels (you can specify which tile server to use).

I set up a systemd
unit to start gpsd and load FoxtrotGPS in
fullscreen when the system boots. I
cached the maps for my parents' next planned trip and got the system working in
the nick of time.

![FoxtrotGPS in action on the device.](../images/blog-birthday-gps-foxtrot.jpg)

Unfortunately, my work is not done. Caching tile images at all
necessary zoom resolutions for any significant region would
take an inordinant amount of space. The raw OSM data is far more practical to
use: the entire North American region is only on the order of several
gigabytes. My new plan is to follow a process something like [this guide][5]
to set up a local tile server to render maps on the fly.

[1]: https://viking-gps.github.io/
[2]: https://www.openstreetmap.org
[3]: https://openmaptiles.org
[4]: https://www.foxtrotgps.org/
[5]: https://switch2osm.org/serving-tiles/manually-building-a-tile-server-20-04-lts/
[6]: https://gpsd.gitlab.io/gpsd/installation.html
[7]: https://gpsd.io/
