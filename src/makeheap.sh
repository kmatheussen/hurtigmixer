

export SISC=$HOME/sisc-1.16.6
export JASSPATH=$HOME/hurtigmixer/jass-sdk/examples/jar/jass.jar
export CLASSPATH=$SISC/sisc.jar:$SISC/sisc-lib.jar:$SISC/sisc-opt.jar:$JASSPATH:.

cd $SISC/src/sisc/boot
java sisc.boot.GenerateHeap -out heap.shp -files init.sce compat.sce psyntax.sce analyzer.sce eval.sce init2.scm repl.scm $HOME/hurtigmixer/src/frametest.scm

