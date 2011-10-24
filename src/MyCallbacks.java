import sisc.data.*;
import sisc.interpreter.*;


public class MyCallbacks extends Callbacks{
    Procedure returnFunc;
    Procedure progressFunc;
    Events events;
    Interpreter interpreter;

    void callBoolean(boolean val){
	Value args[]={val==true?SchemeBoolean.TRUE:SchemeBoolean.FALSE};
	if(returnFunc!=null)
	    events.evalSomething(returnFunc,args);
    }
    void callProgress(int a,int b){
	Quantity args[]={Quantity.valueOf(a),Quantity.valueOf(b)};
	if(progressFunc!=null)
	    events.addReturnFunc(progressFunc,args,interpreter);
    }

    public MyCallbacks(Procedure returnFunc,Procedure progressFunc){
	this.returnFunc=returnFunc;
	this.progressFunc=progressFunc;
	events=CreateFrame.events;
	interpreter=Context.enter(events.ctx);
    }
}

