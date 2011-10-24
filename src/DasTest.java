



public class DasTest{
    
    public static void main (String args[]) throws Exception {
	final boolean useRTMixer=true;
	final float srate=44100;
	final int nFrames=4096;
	final float length=18;
	
	DasMixer dasMixer=new DasMixer(nFrames,srate,useRTMixer,length);
	SoundHolder soundHolder=new SoundHolder();
	
	String fileName="/home/kjetil/Blub_mono16.wav";
	fileName="/hom/kjetism/2_channel_short.wav";
	fileName="/hom/kjetism/Blub_mono16.wav";
	
	soundHolder.addSound(fileName,srate,null,null);
	soundHolder.waitForSound(fileName);
	
	AudioFileInfo audioFileInfo=soundHolder.getAudioFileInfo(fileName,srate);
	
	
	MyAudioFileBuffer afb=new MyAudioFileBuffer(audioFileInfo);
	

	//AudioFileBuffer afb=new AudioFileBuffer("/gammelhd/home/kjetil/brenning/cemb2_b.wav");
	SoundObject a=new SoundObject(afb, -1.0f, 0.5f, dasMixer,false);
	SoundObject b=new SoundObject(afb,  0.0f,  0.5f, dasMixer,false);
	SoundObject c=new SoundObject(afb, +1.0f, 0.5f, dasMixer,false);
	
	
	/*
	dasMixer.requestTo_add(a);
	dasMixer.requestTo_add(b);
	dasMixer.requestTo_add(c);

	a.requestTo_setStartReadPos(0.5f);
	b.requestTo_setEndReadPos(0.2f);
	c.vol=0.5f;
	*/

	dasMixer.srate_change=8.13;//0.700000f;//0.75f;
	//dasMixer.srate_change=0.0432;//0.700000f;//0.75f;
	dasMixer.srate_change=4.5732931726;
	
	dasMixer.srate_change=0.39;
	//dasMixer.srate_change=1.0;

	SoundObject so=new SoundObject(afb,0.0f,0.2f,dasMixer,false);
	dasMixer.requestTo_add(so,(int)(0.1*dasMixer.srate));


	SoundObject so2=new SoundObject(afb,0.0f,0.2f,dasMixer,false);
	dasMixer.requestTo_add(so2,(int)(2.1*dasMixer.srate));

	/*
	so.requestTo_setStartReadPos(0.1f);
	so.requestTo_setEndReadPos(0.2f);
	*/

	dasMixer.startPlaying();

	//so.requestTo_setSRate(0.9432f);
	//so.requestTo_setPos(2.0f);

	    /*
	so.requestTo_setSRate(0.0432f);
	//so.requestTo_setSRate(8.230f);
	so.requestTo_setSRate(1.0f);
	so.requestTo_setSRate(3.30452674627304077f);
	*/
	//so.requestTo_setSRate(1.1f);

	Thread.sleep(10000);
	System.out.println("back to 0");
	dasMixer.scheduler.setPlayPos(0);

	//so.requestTo_setPos(2.0f);

	//System.out.println("Ai "+afb.bitsPerSample+"\n");

	Thread.sleep(10000);
	dasMixer.stopPlaying();
	dasMixer.exitPlayer();
    }

}

