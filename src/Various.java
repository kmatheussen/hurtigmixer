
import java.awt.*;
import java.awt.image.MemoryImageSource;

import java.io.*;
import java.net.*;
import java.util.*;
import java.lang.Thread;

import javax.swing.*;

import sisc.interpreter.*;
import sisc.data.*;


import java.lang.reflect.Method;
import java.util.Arrays;
import javax.swing.JOptionPane;


class SpawnThread extends Thread{
    public Procedure thunk;

    public void run(){	       
	Thread.currentThread().setPriority(Thread.MIN_PRIORITY);
	Interpreter interpreter=Context.enter(Events.ctx);
	Value args[]={};
	CreateFrame.events.evalSomething_nonsynch(thunk,args,interpreter);
    }

    public SpawnThread(Procedure thunk){
	this.thunk=thunk;
    }
}


public class Various{

    // Using (new <java.awt.Color>) r g b) may (very well) fail because java.scm doesn't differ between the int and the float versions of the Color constructor.
    //  (It would be better if java.scm checked for ambiguity, and in case required java arguments)
    public static Color getColor(float r,float g,float b){
	return new Color(r,g,b);
    }
    public static Color getColor(float r,float g,float b,float a){
	return new Color(r,g,b,a);
    }

    public static Font getFont(String name, int size){
	return new Font(name,Font.PLAIN,size);
    }

    public static float scale(float x,float x1,float x2,float y1,float y2){
	return y1+( ((x-x1)*(y2-y1))/(x2-x1));
    }

    public static void spawnThread(Procedure thunk){
	SpawnThread st=new SpawnThread(thunk);
	st.start();
    }
    public static void waitForImage(Image image){
	MediaTracker mediaTracker = new MediaTracker(CreateFrame.frame);
	mediaTracker.addImage(image, 0);
        while(true){
            try{
                mediaTracker.waitForID(0);
                break;
            } catch(InterruptedException ie){
                System.out.println("Oops"+ie);
            }
        }
    }

    public static String getStringDialog(String message){
	String ret=JOptionPane.showInputDialog(null,message);
	System.out.println("ret:"+ret);
	return ret;
    }

    public static long getThreadId(){
	return Thread.currentThread().getId();
    }

    // Copied from http://www.rgagnon.com/javadetails/java-0440.html
    private static int[] pixels = new int[16 * 16];
    private static Image image = Toolkit.getDefaultToolkit().createImage(new MemoryImageSource(16, 16, pixels, 0, 16));
    public static java.awt.Cursor transparentCursor = Toolkit.getDefaultToolkit().createCustomCursor(image, new Point(0, 0), "invisibleCursor");


    // Copied from:
    //http://209.85.135.104/search?q=cache:IwetCRn_yxAJ:www.codecodex.com/wiki/index.php%3Ftitle%3DSave_(download)_a_file_via_HTTP+%22download+a+file%22+java&hl=en&ct=clnk&cd=18&client=opera
    public static boolean downloadFileFromURL(String fetchUrl, File saveFile, final Procedure progressFunc)
	throws IOException,FileNotFoundException,IOException {
	return downloadFileFromURL(fetchUrl,saveFile,progressFunc,true);
    }

    private static boolean downloadFileFromURL(String fetchUrl, File saveFile, final Procedure progressFunc, boolean useProgressFunc)
	throws IOException,FileNotFoundException,IOException {

	final Events events=CreateFrame.events;
	Interpreter interpreter=null;
	if(events!=null)
	    interpreter=Context.enter(events.ctx);
	    
	HttpURLConnection c;
	
	//save file    	
	URL url = new URL(fetchUrl);
	c = (HttpURLConnection)url.openConnection();
	
	//connect
	c.connect();
	
	//input stream
	BufferedInputStream in = new BufferedInputStream(c.getInputStream());
	
	//save the file
	OutputStream out = new BufferedOutputStream(new FileOutputStream(saveFile));
	byte[] buf = new byte[256];
	int n = 0;
	
	{
	    int length=c.getContentLength();
	    ProgressMonitor progressMonitor=null;

	    if(progressFunc==null && useProgressFunc){
		progressMonitor=new ProgressMonitor(CreateFrame.events.container,"Please wait, dowloading encoder.","",0,length==-1?2500000:length);
		progressMonitor.setMillisToPopup(10);
		progressMonitor.setMillisToDecideToPopup(10);
		progressMonitor.setProgress(50);
	    }
	    int pos=0;
	    
	    while ((n=in.read(buf))>=0) {
		out.write(buf, 0, n);
		pos+=n;
		if(progressFunc==null){
		    if(progressMonitor!=null)
			progressMonitor.setProgress(pos);
		}else{
		    Quantity args[]={Quantity.valueOf(pos),Quantity.valueOf(length==-1?2500000:length)};
		    CreateFrame.events.addReturnFunc(progressFunc,args,interpreter);
		}
	    }
	    System.out.println("Hai!");
	    if(progressMonitor!=null)
		progressMonitor.close();
	}
	
	out.flush();
	out.close();
	
	return true;	    
    }


    public static boolean checkDownload(String fetchUrl){
	File tempFile=null;
	try{
	    tempFile=File.createTempFile("prefix","suffix");
	    downloadFileFromURL(fetchUrl,tempFile,null,false);
	    tempFile.delete();
	}catch(Exception e){
	    if(tempFile!=null)
		tempFile.delete();
	    return false;
	}
	return true;
    }

    // errMsg and openURL picked from
    // http://centerkey.com/java/browser/
    private static final String errMsg = "Error attempting to launch web browser";
    
    public static void openURL(String url) {
        String osName = System.getProperty("os.name");
      try {
          if (osName.startsWith("Mac OS")) {
              Class fileMgr = Class.forName("com.apple.eio.FileManager");
              Method openURL = fileMgr.getDeclaredMethod("openURL",
                                                         new Class[] {String.class});
              openURL.invoke(null, new Object[] {url});
            }
          else if (osName.startsWith("Windows"))
            Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url);
         else { //assume Unix or Linux
            String[] browsers = {
               "firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape" };
            String browser = null;
            for (int count = 0; count < browsers.length && browser == null; count++)
               if (Runtime.getRuntime().exec(
                     new String[] {"which", browsers[count]}).waitFor() == 0)
                  browser = browsers[count];
            if (browser == null)
               throw new Exception("Could not find web browser");
            else
               Runtime.getRuntime().exec(new String[] {browser, url});
            }
         }
      catch (Exception e) {
         JOptionPane.showMessageDialog(null, errMsg + ":\n" + e.getLocalizedMessage());
         }
      }

}



