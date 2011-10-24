
import java.io.*;
import java.net.*;
import sisc.interpreter.*;
import sisc.util.*;
import sisc.ser.*;
import sisc.modules.*;
import sisc.REPL;
import sisc.*;

/*
export CLASSPATH=/hom/kjetism/sisc-1.16.5/sisc.jar:$SISC_HOME:/hom/kjetism/sisc-1.16.5/sisc-lib.jar:/hom/kjetism/sisc-1.16.5/sisc-opt.jar:.
*/

public class atest{
    public static void main(String args[]){

	AppContext ctx = new AppContext();
	//AppContext ctx=Context.getDefaultAppContext();

	if(true){
	    try{
		//URL yes=ctx.findHeap(new URL("http://www.notam02.no/~kjetism/hurtigmixer/src/sisc.shp"));
		//URL yes=ctx.findHeap(new URL("file:///hom/kjetism/hurtigmixer/src/sisc.shp"));
		URL yes=ctx.findHeap(new URL("file://" + args[0]));//"file:///hom/kjetism/hurtigmixer/src/heap.shp"));
		//URL yes=ctx.findHeap(new URL("file:///home/kjetil/hurtigmixer/src/heap.shp"));
		SeekableInputStream sis=ctx.openHeap(yes);
		System.out.println("sis: "+sis);
		System.out.println("addheap: "+ctx.addHeap(sis));
		//addDefaultHeap();
	    }catch(java.io.IOException e){
		System.out.println("oops:"+e);
	    }catch(java.lang.ClassNotFoundException e){
		e.printStackTrace();
		System.out.println("oops2: "+e);
	    }
	}else{
	    try{
		ctx.addDefaultHeap();
	    }catch(java.io.IOException e){
		System.out.println("oops\n");
	    }
	}

	System.out.println("yepp\n");
	
	Interpreter r=Context.enter(ctx);
	System.out.println("r: "+r+" ctx:"+ctx);
	 
	//r.eval(someProcedure, new Value[] { ... some arguments ... });

	try{
	    r.eval("(display 50)(newline)(load \"frametest.scm\")");
	}catch(sisc.interpreter.SchemeException e){
	    e.printStackTrace();
	    System.out.println("no");
	}catch(java.io.IOException e){
	    System.out.println("no2");
	}

	System.out.println("yes2");

	Context.exit();
	
	System.out.println("yes");
    }
}
