rem Preparing to start Hurtigmikser
rem Setting up classpath
rem set CLASSDIR=classes

rem set CLASSPATH=%CLASSPATH%;%CLASSDIR%/sisc-opt.jar;%CLASSDIR%/sisc.jar;%CLASSDIR%/jass.jar;%CLASSDIR%/jl1.0.jar;%CLASSDIR%/vorbisspi1.0.2.jar;%CLASSDIR%/tritonus_share.jar;%CLASSDIR%/jorbis-0.0.15.jar;%CLASSDIR%/jogg-0.0.7.jar;%CLASSDIR%/mp3plugin.jar;%CLASSDIR%/jflac-1.2.jar;%CLASSDIR%/tritonus_remaining.jar;hurtigmixer.jar

rem Launching Hurtigmikser
rem java -Djava.library.path=lib -cp %CLASSPATH% HurtigMikser

java -jar hurtigmixer.jar
