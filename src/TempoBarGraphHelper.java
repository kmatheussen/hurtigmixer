

// Note: getValue() returns _time_, not tempo. The values[] array contains time slices.

public class TempoBarGraphHelper extends BarGraphHelper{
    float minTimeTimeslice=0.1f;
    float maxTimeTimeslice=2.0f;

    public static float scale(float x,float x1,float x2,float y1,float y2){
	return y1+( ((x-x1)*(y2-y1))/(x2-x1));
    }

    @Override public float mapValue(float value){
	if(value<0.5)
	    return scale(value,0.0f,0.5f,maxTimeTimeslice,1.0f);	
	else
	    return scale(value,0.5f,1.0f,1.0f,minTimeTimeslice);
    }

    public TempoBarGraphHelper(int size,float defaultValue,float minTimeTimeslice, float maxTimeTimeslice){
	super(size,defaultValue);
	this.minTimeTimeslice=minTimeTimeslice;
	this.maxTimeTimeslice=maxTimeTimeslice;
	calculateIntegratedValues(0); // recalculate since the values depend on minTempo/maxTempo
    }
}
