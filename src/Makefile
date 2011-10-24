
#HOME=${HOME}
SISC=$(HOME)/sisc-1.16.6
JASSPATH=$(HOME)/hurtigmixer/jass-sdk/examples/jar/jass.jar

CLASSPATH=soundengine/:sisc.jar:sisc-lib.jar:sisc-opt.jar:$(JASSPATH):$(HOME)/hurtigmixer/lib/tritonus_share.jar:$(HOME)/hurtigmixer/lib/tritonus_remaining.jar:$(HOME)/hurtigmixer/lib/vorbisspi1.0.2.jar:$(HOME)/hurtigmixer/lib/jflac-1.2.jar:$(HOME)/DSP2/build/lib/jjack.jar:.

JAVAC=/site/jdk1.5/bin/javac

ll: all-classes

run2: all-classes
	java  -cp $(CLASSPATH) DasTest

run:
	java -cp $(CLASSPATH) HurtigMikser
#	java -cp $(CLASSPATH) atest $(HOME)/hurtigmixer/src/heap.shp


run-applet:
	appletviewer  -J-Djava.security.policy=my.policy http://folk.uio.no/ksvalast/hurtigmikser/DasApplet.html


run-melodigenerator-applet:
	appletviewer  -J-Djava.security.policy=my.policy http://users.notam02.no/~kjetism/melodigenerator/


test: all-classes
	java -cp $(CLASSPATH) Test $(HOME)/hurtigmixer/src/heap.shp

all-classes:
	rm -f *.class soundengine/*.class
	rm -f keystore.*
	$(JAVAC) -cp $(CLASSPATH) *.java soundengine/*.java
	cp soundengine/*.class .

#	java-config -L|grep 1.5.0.14|grep \*\)

export:
	@echo export SISC=$(HOME)/sisc-1.16.6
	@echo export JASSPATH=$(HOME)/hurtigmixer/jass-sdk/examples/jar/jass.jar
	@echo export CLASSPATH=$(SISC)/sisc.jar:$(SISC)/sisc-lib.jar:$(SISC)/sisc-opt.jar:$(JASSPATH):.

heap: heap.shp
	sh makeheap.sh

#JFileFilter*.class Envelope*.class AudioFileSaver*.class CreateFrame*.class DasApplet*.class DasMixer*.class MyAudioFileBuffer*.class MyPlayer*.class NonApplet*.class RingBuffer*.class SoundObject*.class Message*.class FileRequester*.class SoundHolder*.class Envelope*.class JFileFilter*.class MyStream*.class 

clean:
	rm -fr ../standalone/*
	rm -f *~ */*~
	rm -fr temp/*
	rm -fr scc/*
	rm -fr applet/*
	rm -f applet.tar.gz
	rm -f *.class */*.class
	rm -fr gencode/*
	rm -f hurtigmixer.jar

standalone: jar
	rm -fr ../standalone/*
	mkdir ../standalone/classes
	cp hurtigmixer.jar run_linux.sh run_macosx.command run_windows.bat ../standalone/
	cp -a ../configtest ../standalone/config
	cp sisc.jar sisc-opt.jar ../lib/* ../standalone/classes
	mv ../standalone HurtigMikser
	rm HurtigMikser.zip
	zip -r HurtigMikser.zip HurtigMikser
	mv HurtigMikser ../standalone/
	cp HurtigMikser.zip /hom/kjetism/uio/www_docs/

test-applet:
	make 
	make jar
	make applet
	rm -fr ../../uio/www_docs/hurtigmixer_testing/*
	cp -f applet/* ../../uio/www_docs/hurtigmixer_testing/
	appletviewer  -J-Djava.security.policy=my.policy http://users.notam02.no/~kjetism/hurtigmixer_testing/melodigenerator.html

#run-applet:
#	appletviewer  -J-Djava.security.policy=my.policy http://users.notam02.no/~kjetism/hurtigmixer_testing/melodigenerator.html

applet: jar
	sh dosign.sh	
	rm -f applet/*
	cp hurtigmixer.jar sisc.jar sisc-opt.jar ../lib/* DasApplet.html melodigenerator.html applet/

jar:
	rm -f hurtigmixer.jar
	jar xvf $(HOME)/DSP2/v04-proposal/lib/jjack.jar
	jar cmf META-INF/MANIFEST.MF hurtigmixer.jar *.class de main.scm main-applet.scm main-standalone.scm main-applet-melodigenerator.scm various.scc scc heap.shp hurtigmixer_bakgrunn.png *.ttf
