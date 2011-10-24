#!/bin/sh

/site/j2sdk/bin/keytool -genkey -alias hurtigmixer -dname "cn=Musikkverksted.no, ou=Musikkverksted.no, o=Musikkverksted.no, c=no" -keystore keystore.$$$ -storepass 123abc -keypass 123abc -validity 100
/site/j2sdk/bin/keytool -export -alias hurtigmixer -rfc -file hurtigmixer.ser -keystore keystore.$$$ -storepass 123abc
jarsigner -keystore keystore.$$$ -storepass 123abc hurtigmixer.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc sisc.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc sisc-opt.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/tritonus_remaining.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/tritonus_share.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/jogg-0.0.7.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/jorbis-0.0.15.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/vorbisspi1.0.2.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/jass.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/jl1.0.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/mp3plugin.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/mp3spi1.9.4.jar hurtigmixer
jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/jflac-1.2.jar hurtigmixer
#jarsigner -keystore keystore.$$$ -storepass 123abc ../lib/jass.jar hurtigmixer

