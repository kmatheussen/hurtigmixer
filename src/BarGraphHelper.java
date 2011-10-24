
import java.util.Arrays;


// Sisc was a bit too slow for this.

public class BarGraphHelper{
    float[] values;
    float[] integratedValues;

    public float mapValue(float value){
	return value;
    }

    /*
      i:                       0       1            2                etc.
      values (examples):       0.5     0.6          0.7              etc.
      integrated values:       0.5     (0.5+0.6)    (0.5+0.6+0.7)    etc.
      integrated avg. values:  0.5/1   (0.5+0.6)/2  (0.5+0.6+0.7)/3  etc.
    */
    public void calculateIntegratedValues(int start){
	float sum = 0.0f;

	if(start>0)
	    sum = integratedValues[start-1];

	for(int i=start;i<values.length;i++){
	    sum = sum + getValue(i);
	    integratedValues[i] = sum;
	}
    }

    public float getIntegratedValue(int pos){
	return integratedValues[pos];
    }

    public float getIntegratedAverageValue(int pos){
	return getIntegratedValue(pos) / (float)(pos+1);
    }

    public void setValue(int pos,float val){
	values[pos]=val;
    }
    public float getValue(int pos){
	return mapValue(values[pos]);
    }
    public float getUnmappedValue(int pos){
	return values[pos];
    }

    public BarGraphHelper(int size,float defaultValue){
	values=new float[size];
	Arrays.fill(values,defaultValue);
	integratedValues=new float[size];
	calculateIntegratedValues(0);
    }
}

