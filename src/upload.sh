#!/bin/sh

make && make applet
scp applet/* kjetism@marcel:/site/www/htdocs/notam/hurtigmikser/
scp applet/* ksvalast@login.ifi.uio.no:www_docs/hurtigmikser/

