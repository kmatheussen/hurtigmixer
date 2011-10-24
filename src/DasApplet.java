


// Hmmmmmmmmmmmmm.



import java.awt.Graphics;

import java.awt.*;
import java.awt.event.*;
import sisc.data.*;

import java.io.*;
import java.net.*;
import sisc.interpreter.*;
import sisc.util.*;
import sisc.ser.*;
import sisc.modules.*;
import sisc.REPL;
import sisc.*;
import sisc.env.*;

import sisc.modules.s2j.*;
import sisc.ser.*;
import java.util.zip.*;
import java.util.jar.*;
import java.util.ResourceBundle;



class DasAppletInstance extends Events implements ComponentListener{
    
    static public DasAppletInstance uberthis=null;

    //final String urlbase="http://archive.notam02.no/~kjetism/hurtigmixer/";
    //final String urlbasebase="http://archive.notam02.no/hurtigmikser/";
    //final String urlbasebase="http://folk.uio.no/ksvalast/hurtigmikser/";
    //final String urlbasebase="http://users.notam02.no/~kjetism/melodigenerator/";
    final String urlbasebase=DasApplet.uberthis.getCodeBase().toString();
    //final public static String urlbase="http://193.157.245.38/DasApplet/";
    //final public static String urlbasebase="http://127.0.0.1/DasApplet/applet/";
    final public String urlbase="jar:"+urlbasebase+"hurtigmixer.jar!/"; //jar:file:/home/duke/duke.jar!/");

    public void init(){
	runSchemeEntry(urlbase,"main-applet.scm");
    }



    public void setVariables(Procedure mousePressProc,Procedure mouseDragProc,Procedure mouseReleaseProc,Procedure mouseWheelProc,Procedure keyPressProc,Procedure keyReleaseProc,Procedure paintProc,boolean is_running_applet){
	this.mousePressProc=mousePressProc;
	this.mouseDragProc=mouseDragProc;
	this.mouseReleaseProc=mouseReleaseProc;
	this.mouseWheelProc=mouseWheelProc;
	this.keyPressProc=keyPressProc;
	this.keyReleaseProc=keyReleaseProc;
	this.paintProc=paintProc;

	System.out.println("CreateFrame constructor");

	/*
	AppContext ctx;

	if(is_running_applet==false)
	    ctx=Context.getDefaultAppContext();
	else
	    ctx=DasApplet.ctx;
	*/

	System.out.println("CreateFrame constructor2");

	//interpreter=Context.enter(ctx);

	System.out.println("CreateFrame constructor3");

	//addWindowListener((WindowListener)this);

    }

    /*
    public CreateFrame(Procedure mousePressProc,Procedure mouseDragProc,Procedure mouseReleaseProc,Procedure paintProc,boolean is_running_applet){
	setVariables(mousePressProc,mouseDragProc,mouseReleaseProc,paintProc,is_running_applet);
    }
    */
    public void componentHidden(ComponentEvent e){
    }
    public void componentMoved(ComponentEvent e) {
    }
    public void componentResized(ComponentEvent e) {
    }
    public void componentShown(ComponentEvent e){
	//init();
    }

    public DasAppletInstance(){
	uberthis=this;
    }

}


class DasFrame extends Thread{
    public Frame frame;
    public Graphics g;
    public DasApplet dasApplet;
    public boolean stopme=false;

    public void run(){
	while(stopme==false){
	    try{
		sleep(50000);
	    }catch(java.lang.InterruptedException e){
		dasApplet.paintMessage(g);
	    }
	}
	frame.setVisible(false);
	frame.dispose();
    }

    public DasFrame(DasApplet dasApplet){
	this.dasApplet=dasApplet;
	frame=new Frame();
	frame.setVisible(true);
	frame.setBounds(500,400,400,400);
	g=frame.getGraphics();
	start();
    }
}

public class DasApplet extends java.applet.Applet{
    public static DasApplet uberthis;

    public DasAppletInstance dasAppletInstance=null;
    public boolean loaded=false;

    public DasFrame dasFrame=null;

    // Copied from http://forum.java.sun.com/thread.jspa?threadID=611506&messageID=3735617
    public boolean testVersion()
    {
	String str = System.getProperty("java.version");
	String major = str.substring(0,str.indexOf("_"));
	String minor = str.substring(str.indexOf("_")+1,str.length());
	int minorval = Integer.parseInt(minor);
	java.util.StringTokenizer strtk = new java.util.StringTokenizer(major,".",false);
	int count = strtk.countTokens();
	int strtks[] = new int[count];
	for(int i=0;strtk.hasMoreTokens();i++){
	    strtks[i]=Integer.parseInt(strtk.nextToken());
	}
	if(strtks[0]>1)
	    {
		return true;
	    }
	else if(strtks[0]==1 && strtks[1]>4)
	    return true;
	else
	    return false;
    }
 
    public void init() {
	System.out.println("GAKKGAKK\n");
	uberthis=this;
	forcePaintMessage();
    }


    public String message="Vennligst vent / Please wait";
    public void paintMessage(Graphics g){
	System.out.println("Message: "+message);
	g.setClip(0,0,1000,200);
	g.setColor(Color.white);
	g.fillRect(0,0,1000,200);
	g.setColor(Color.black);
	g.drawString(message,40,40);
    }


    public void forcePaintMessage(){
	//paintMessage(getGraphics());
	//repaint();
	update(getGraphics());
	if(dasFrame==null)
	    dasFrame=new DasFrame(this);
	dasFrame.interrupt();
    }

   
    public void downCount(int downCount,String filename){
	message="Loading "+filename+", "+downCount;
	forcePaintMessage();
    }

    boolean startitit=false;
    public void startIt(){
	System.out.println("GAKKGAKK2\n");
	if(testVersion()==false){
	    message="Java maa oppgraderes / Java needs to be upgraded. (minimum V1.5)";
	    forcePaintMessage();
	}else{
	    forcePaintMessage();
	    System.out.println("gakkgakk3");
	    dasAppletInstance=new DasAppletInstance();
	    if(getParameter("melodigenerator")!=null && getParameter("melodigenerator").equals("true"))
		Events.running_melodigenerator=true;
	    dasAppletInstance.userName = getParameter("userName");
	    dasAppletInstance.getFilesURL = getParameter("getFilesURL");
	    dasAppletInstance.uploadURL = getParameter("uploadURL");
	    if(getParameter("lydsettURL")!=null)
		dasAppletInstance.lydsettURL = getParameter("lydsettURL");
	    if(getParameter("configURL")!=null)
		dasAppletInstance.configURL = getParameter("configURL");
	    if(getParameter("askName")!=null)
		dasAppletInstance.askName = getParameter("askName");

	    dasAppletInstance.container=this;
	    System.out.println("gakkgakk4");
	    addComponentListener(dasAppletInstance);
	    System.out.println("gakkgakk5");
	    addMouseListener(dasAppletInstance);
	    System.out.println("gakkgakk6");
	    addMouseMotionListener(dasAppletInstance);
	    System.out.println("gakkgakk7");
	    addMouseWheelListener(dasAppletInstance);
	    System.out.println("gakkgakk8");
	    addKeyListener(dasAppletInstance);
	    System.out.println("gakkgakk9");
	    repaint();
	    System.out.println("gakkgakk10");
	    dasAppletInstance.init();
	    System.out.println("gakkgakk11");
	}
    }

    public void start(){
	System.out.println("starting");


	if(dasAppletInstance.uberthis!=null){
	    // (almost all the mess in this file is the result of trying to make it restart properly, but I give up)
	    // So here is das final solution: 
	    message="For aa starte programmet paa nytt maa du avslutte browseren foerst. / Please restart your browser.";
	    uberthis=this;
	    //startIt();
	    //loaded=true;
	}

	repaint();

    }

    public void stop(){
	System.out.println("stopping1");
	//Context.exit();
	//destroy();
	//System.gc();
	//System.runFinalization();
	//System.exit(0);
	//dasAppletInstance.interpreter=null;
	System.out.println("stopping2");
        //ResourceBundle.clearCache(ClassLoader.getSystemClassLoader());
    }

    public void destroy(){
	System.out.println("destroying1");
	//Context.exit();
	//	dasAppletInstance.interpreter=null;
	System.out.println("destroying2");
        //ResourceBundle.clearCache(ClassLoader.getSystemClassLoader());
    }

    public void paint(Graphics g){
	if(dasAppletInstance!=null && loaded==true){
	    if(dasFrame!=null){
		dasFrame.stopme=true;
		dasFrame.interrupt();
		dasFrame=null;
	    }
	    dasAppletInstance.paint(g);
	}
	else{
	    if(startitit==false){
		startitit=true;
		startIt();
	    }
	    paintMessage(g);
	}
    }

}

