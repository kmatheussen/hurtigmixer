
import java.io.*;
import java.net.*;
import java.awt.*;
import java.awt.event.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.modules.s2j.*;
import sisc.ser.*;
import sisc.env.*;

public class Events extends Thread implements MouseListener,MouseMotionListener,MouseWheelListener,KeyListener{
    public Procedure mousePressProc;
    public Procedure mouseDragProc;
    public Procedure mouseReleaseProc;
    public Procedure mouseWheelProc;
    public Procedure keyPressProc;
    public Procedure keyReleaseProc;
    public Procedure paintProc;

    public static AppContext ctx;
    public static Interpreter interpreter=null;

    public Container container;
    public Graphics g=null;

    public int lastX=0;
    public int lastY=0;


    public static String userName="kjetil";
    public static String getFilesURL="http://archive.notam02.no/hurtigmikser/getfiles.php";
    public static String uploadURL="http://archive.notam02.no/hurtigmikser/gotfile.php";
    //public static String lydsettURL="http://archive.notam02.no/hurtigmikser/lydsettet/";
    public static String lydsettURL="http://folk.uio.no/ksvalast/hurtigmikser/lydsettet/";
    public static String configURL=""; //http://archive.notam02.no/hurtigmikser/configtest/";
    public static String askName="true";

    public static boolean running_melodigenerator=false;

    // Better let everything go through here to ensure all exceptions are traced.
    public void evalSomething_nonsynch(Procedure proc,Value[] args, Interpreter interpreter){
	//System.out.println("proc:"+proc+", args:"+args);
	try{
	    interpreter.eval(proc,args);
	}catch(Exception e){
	    if(e!=null)
		e.printStackTrace();
	    try{
		interpreter.eval("(bt)");
	    }catch(sisc.interpreter.SchemeException e2){
		e2.printStackTrace();
	    }catch(java.io.IOException e2){
		e2.printStackTrace();
		System.out.println("no");
	    }
	}

	/*
	try{
	    r.eval(p,new Nothing[0]);
	    //p.apply(r);
	}catch(sisc.interpreter.ContinuationException e){
	    e.printStackTrace();
	    System.out.println("no");
	}
	*/
    }

    // Better let everything go through here to ensure all exceptions are traced.
    public void evalSomething(Procedure proc,Value[] args, Interpreter interpreter){
	//System.out.println("Waiting for sync (if this is the last message seen, there's a deadlock here)");
	synchronized(CreateFrame.globalLock){
	    //System.out.println("Got sync. No deadlock");
	    evalSomething_nonsynch(proc,args,interpreter);
	}
    }

    public void evalSomething(Procedure proc,Value[] args){
	evalSomething(proc,args,interpreter);
    }


    /*
    private void evalMouse(Procedure proc,MouseEvent e){
 	Quantity args[]={Quantity.valueOf(java.awt.event.MouseEvent.BUTTON1==e.getButton()?1:
					  java.awt.event.MouseEvent.BUTTON2==e.getButton()?2:3),
			 Quantity.valueOf(e.getX()),
			 Quantity.valueOf(e.getY())};
	evalSomething(proc,args);

    }
    */

    public void mousePressed(MouseEvent e){
	lastX=e.getX();
	lastY=e.getY();
 	Value args[]={new JavaObject(e)};
	evalSomething(mousePressProc,args);
	//evalMouse(mousePressProc,e);
    }    
    public void mouseClicked(MouseEvent e){
	lastX=e.getX();
	lastY=e.getY();
    }
    public  void mouseReleased(MouseEvent e){
	lastX=e.getX();
	lastY=e.getY();
 	Value args[]={new JavaObject(e)};
	evalSomething(mouseReleaseProc,args);
	//evalMouse(mouseReleaseProc,e);
    }
    public void mouseEntered(MouseEvent e){
	lastX=e.getX();
	lastY=e.getY();
    }
    public void mouseExited(MouseEvent e){
	lastX=e.getX();
	lastY=e.getY();
    }
    public void mouseDragged(MouseEvent e){
	lastX=e.getX();
	lastY=e.getY();
 	Value args[]={new JavaObject(e)};
	evalSomething(mouseDragProc,args);
	//evalMouse(mouseDragProc,e);
    }

    class MousePlacementEvent{
        MousePlacementEvent next;
        int x,y,x2,y2;
        Procedure callback;
        boolean isInside(int x,int y){
            if(x >= this.x &&
               y >= this.y &&
               x <  this.x2 &&
               y <  this.y2)
                return true;
            return false;
        }
    }
        
    MousePlacementEvent lastMousePlacementEvent=null;
    MousePlacementEvent mousePlacementEvents=null;
    public Procedure nowhereMousePlacementCallback = null;

    public void addMousePlacementEvent(int x,int y,int width,int height,Procedure callback){
        MousePlacementEvent placement = new MousePlacementEvent();
        placement.x = x;
        placement.y = y;
        placement.x2 = x + width;
        placement.y2 = y + height;
        placement.callback = callback;
        placement.next = mousePlacementEvents;
        mousePlacementEvents = placement;
    }

    public void mouseMoved(MouseEvent e){
	lastX=e.getX();
	lastY=e.getY();
        int x = lastX;
        int y = lastY;

	//System.out.println("gakkgakkgakk "+e.getX()+", "+e.getY());
        if(lastMousePlacementEvent != null &&
           lastMousePlacementEvent.isInside(x,y)
           )
            return;

        MousePlacementEvent placement = mousePlacementEvents;
        while(placement != null){
            if(placement.isInside(x,y)){
                lastMousePlacementEvent = placement;
                Value args[]={};
                evalSomething(placement.callback,args);
                return;
            }
            placement = placement.next;
        }
        if(lastMousePlacementEvent!=null){
            if(nowhereMousePlacementCallback !=null){
                Value args[]={};
                evalSomething(nowhereMousePlacementCallback,args);
            }
            lastMousePlacementEvent = null;
        }
    }
    public void mouseWheelMoved(MouseWheelEvent e){
	lastX=e.getX();
	lastY=e.getY();
	/*
	Quantity args[]={Quantity.valueOf(e.getWheelRotation()>0?4:5),
			 Quantity.valueOf(e.getX()),
			 Quantity.valueOf(e.getY())};
	evalSomething(mousePressProc,args);
	*/
 	Value args[]={new JavaObject(e)};
	evalSomething(mouseWheelProc,args);
    }

    public void keyPressed(KeyEvent e){
 	Value args[]={new JavaObject(e)};
	//System.out.println("keyPressed "+e.getKeyCode());
	evalSomething(keyPressProc,args);
    }
    public void keyReleased(KeyEvent e){
 	Value args[]={new JavaObject(e)};
	//System.out.println("keyReleased "+e.getKeyCode());
	evalSomething(keyReleaseProc,args);
    }
    public void keyTyped(KeyEvent e){
	//System.out.println("keyTyped "+e.getKeyCode());
    }
    public void keyRepeated(int keyCode){
	//System.out.println("keyRepeated "+keyCode);
    }


    void paintCursor(Graphics g, Interpreter interpreter){
	//System.out.println("Events.paintCursor"+cursor);
	if(cursor!=null){
	    Procedure cursorCallback=cursor.cursorCallback;
	    Quantity args[]={};
	    Events events=CreateFrame.events;
	    events.evalSomething(cursorCallback,args,interpreter);
	    //cursor.paintCursor(interpreter);
	}
    }

    public void run(){
	Thread.currentThread().setPriority(Thread.MIN_PRIORITY);
	Interpreter interpreter=Context.enter(ctx);
	while(true){
	    try{
		sleep(50000);
	    }catch(java.lang.InterruptedException e){
		if(g!=null){
		    paintCursor(g,interpreter);
		}
	    }
	    //System.out.println("finished sleeping\n");
	}
    }

    MyCursor cursor=null;


    // Called from the player thread.
    public void scheduleCursorDrawing(MyCursor cursor){
	this.cursor=cursor;
	interrupt();
    }


    public void addReturnFunc(Procedure returnFunc,Value[] returnValues, Interpreter interpreter){
	evalSomething(returnFunc,returnValues,interpreter);
    }

    public void paint(Graphics g){
	//System.out.println("Events.paint. paintProc: "+paintProc);
	this.g=g;

	Value args[]={new JavaObject(g)};
	evalSomething(paintProc,args);

	//cursor.paintCursor();
    }


    static boolean isloaded=false;


    static public void runSchemeEntry(String urlbase,String fileToLoad){

	if(isloaded==false){
	    try{
		System.out.println("a gakk1");
		ctx = new AppContext();
		//ctx = Context.getDefaultAppContext();
		System.out.println("a gakk2");
	    }catch(java.lang.RuntimeException b){
		b.printStackTrace();
	    }
	    
	    
	    try{
                System.out.println("Opening url \""+urlbase+"heap.shp\"");
                URL yes=ctx.findHeap(new URL(urlbase+"heap.shp"));
                System.out.println("open heap");
		SeekableInputStream sis=AppContext.openHeap(yes);
		System.out.println("sis: "+sis);
		System.out.println("addheap: "+ctx.addHeap(sis));
	    }catch(java.io.IOException e2){
		e2.printStackTrace();
		System.out.println("oops: "+e2);
	    }catch(java.lang.ClassNotFoundException e3){
		e3.printStackTrace();
		System.out.println("oops2 "+e3);
	    }
	    interpreter=new Interpreter(new ThreadContext(),new DynamicEnvironment(ctx));  
	}

	System.out.println("yepp"+ctx);
	
	//interpreter=Context.enter(ctx);
	
	//interpreter=Context.enter(new DynamicEnvironment(ctx));

	System.out.println("interpreter:"+interpreter);

	//File file=new File("main-applet.scm");
	
	if(isloaded==false){
	    try{
		interpreter.eval("(define urlbase \""+urlbase+"\")");
		interpreter.eval("(load \""+urlbase+fileToLoad+"\")");//"main-applet.scm\")");
	    }catch(sisc.interpreter.SchemeException e){
		e.printStackTrace();
		System.out.println("no");
		try{
		    interpreter.eval("(bt)");
		}catch(sisc.interpreter.SchemeException e2){
		}catch(java.io.IOException e2){
		}
	    }catch(java.io.IOException e){
		System.out.println("no2");
		e.printStackTrace();
	    }catch(java.lang.NullPointerException e){
		System.out.println("no3");
		e.printStackTrace();
		try{
		    interpreter.eval("(bt)");
		}catch(sisc.interpreter.SchemeException e2){
		}catch(java.io.IOException e2){
		}
	    }
	    isloaded=true;
	}else{
	    interpreter=new Interpreter(new ThreadContext(),new DynamicEnvironment(ctx));  
	    //interpreter=Context.enter(new DynamicEnvironment(ctx));
	    try{
		//interpreter.eval("(load \"main.scm\")");
		interpreter.eval("(create-hurtigmixer)");
	    }catch(sisc.interpreter.SchemeException e){
		e.printStackTrace();
		System.out.println("no");
		try{
		    interpreter.eval("(bt)");
		}catch(sisc.interpreter.SchemeException e2){
		}catch(java.io.IOException e2){
		}
	    }catch(java.io.IOException e){
		System.out.println("no2");
		e.printStackTrace();
	    }catch(java.lang.NullPointerException e){
		System.out.println("no3");
		e.printStackTrace();
		try{
		    interpreter.eval("(bt)");
		}catch(sisc.interpreter.SchemeException e2){
		}catch(java.io.IOException e2){
		}
	    }


	}
	System.out.println("yes2");

    }


}
