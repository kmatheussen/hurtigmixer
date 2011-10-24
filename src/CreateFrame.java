
import java.awt.*;
import java.awt.event.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.data.*;
import sisc.modules.s2j.*;

class NonApplet extends Events implements WindowListener{

    void printDebugging(String string){
        //System.out.println(string);
    }

    public void windowClosing(WindowEvent e) {printDebugging("windowClosing");System.exit(0);}
    public void windowClosed(WindowEvent e) {printDebugging("windowClosed");}
    public void windowActivated(WindowEvent e){printDebugging("windowAcdtivated");}
    public void windowDeiconified(WindowEvent e){printDebugging("windowDeiconified");}
    public void windowOpened(WindowEvent e){printDebugging("windowOpened");}
    public void windowIconified(WindowEvent e){printDebugging("windowIconified");}
    public void windowDeactivated(WindowEvent e){printDebugging("windowDeactivated");}

    

    public NonApplet(Procedure mousePressProc,Procedure mouseDragProc,Procedure mouseReleaseProc,Procedure mouseWheelProc,Procedure keyPressProc,Procedure keyReleaseProc,Procedure paintProc,NonApplet2 frame,boolean is_running_applet,boolean is_running_standalone){
	this.mousePressProc=mousePressProc;
	this.mouseDragProc=mouseDragProc;
	this.mouseReleaseProc=mouseReleaseProc;
	this.mouseWheelProc=mouseWheelProc;
	this.keyPressProc=keyPressProc;
	this.keyReleaseProc=keyReleaseProc;
	this.paintProc=paintProc;

	System.out.println("CreateFrame constructor");

	AppContext ctx=null;

	if(is_running_standalone==true)
	    ctx=Events.ctx;
	else if(is_running_applet==false)
	    ctx=Context.getDefaultAppContext();
	else
	    System.out.println("Error (not really)");
	//ctx=DasApplet.ctx;


	System.out.println("CreateFrame constructor2");

	interpreter=Context.enter(ctx);

	System.out.println("CreateFrame constructor3");

	container=frame;
	frame.events=this;

	frame.addMouseListener(this);
	frame.addMouseMotionListener(this);
	frame.addMouseWheelListener(this);
	frame.addKeyListener(this);
	frame.addWindowListener((WindowListener)this);
    }


}


// I can't remember ever written more ugly code.

public class CreateFrame{
    static public CreateFrame globalLock;
    static public Container frame=null;
    static public Events events=null;
    

    public CreateFrame(Procedure mousePressProc,Procedure mouseDragProc,Procedure mouseReleaseProc,Procedure mouseWheelProc,Procedure keyPressProc,Procedure keyReleaseProc,Procedure paintProc,boolean is_running_applet, boolean is_running_standalone){
	globalLock=this;
	System.out.println("createframe 1");
	if(is_running_applet==false){
	    System.out.println("createframe 2");
	    frame=new NonApplet2();
	    System.out.println("createframe 3");
	    events=new NonApplet(mousePressProc,mouseDragProc,mouseReleaseProc,mouseWheelProc,keyPressProc,keyReleaseProc,paintProc,(NonApplet2)frame,is_running_applet,is_running_standalone);
	    System.out.println("createframe 4");
	    events.ctx = new AppContext();
	    System.out.println("createframe 5");
	}else{
	    DasApplet ai=DasApplet.uberthis;
	    System.out.println("ai:" + ai);
	    ai.dasAppletInstance.setVariables(mousePressProc,mouseDragProc,mouseReleaseProc,mouseWheelProc,keyPressProc,keyReleaseProc,paintProc,is_running_applet);
	    frame=ai;
	    events=ai.dasAppletInstance;
	}
	System.out.println("1234123412341234 STARTKNG  EVENTS@!@!!!! "+events);
	events.start();
	System.out.println("1234123412341234 ENDING  EVENTS@!@!!!! "+events);
    }

}




