#!/bin/bash

#echo Preparing to start Hurtigmikser
#echo Setting up classpath
#CLASSDIR=`dirname $0`/classes

#CLASSPATH=$CLASSPATH:$CLASSDIR/sisc-opt.jar:$CLASSDIR/sisc.jar:$CLASSDIR/jass.jar:$CLASSDIR/jl1.0.jar:$CLASSDIR/vorbisspi1.0.2.jar:$CLASSDIR/tritonus_share.jar:$CLASSDIR/jorbis-0.0.15.jar:$CLASSDIR/jogg-0.0.7.jar:$CLASSDIR/mp3plugin.jar:$CLASSDIR/jflac-1.2.jar:$CLASSDIR/tritonus_remaining.jar:hurtigmixer.jar:.

#export JAVA_HOME=`pwd`/jre1.6.0_12

#echo Launching Hurtigmikser
#java -Djava.library.path=`dirname $0`/lib -cp $CLASSPATH HurtigMikser

java -jar hurtigmixer.jar

