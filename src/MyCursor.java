
import java.awt.*;
import sisc.interpreter.*;
import sisc.data.*;

public class MyCursor extends Cursor{
    Graphics g;

    int startPos;
    int endPos;
    int f_x1;
    int f_x2;
    int f_y1;
    int f_y2;

    int lastX1;
    int lastX2;
    int lastY1;
    int lastY2;

    /*
    public void removeCursor(){
	if(g == null)
	    return;

	g.setXORMode(Color.white);
	g.drawLine(lastX1,0,lastX1,f_y2);
	g.setPaintMode();
    }
    */


    Events events=null;

    public void request_paint(){
	if(events==null)
	    events=CreateFrame.events;
	if(events!=null)
	    events.scheduleCursorDrawing(this);
    }

    public Procedure cursorCallback=null;

    public void addCursorCallback(Procedure cursorCallback){
	this.cursorCallback=cursorCallback;
    }



    public void setCursorData(Graphics g,float startTime,float endTime,int x1,int x2,int y1,int y2){
	this.g=g;
	this.startPos=(int)(startTime*mixer.srate);
	this.endPos=(int)(endTime*mixer.srate);
	this.lastX1=x1;
	this.f_x1=x1;
	this.f_x2=x2;
	this.f_y1=y1;
	this.f_y2=y2;
    }

    public MyCursor(DasMixer mixer){
	super(mixer);
    }
}

