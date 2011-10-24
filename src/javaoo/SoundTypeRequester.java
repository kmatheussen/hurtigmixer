
import javax.swing.*;


public class SoundTypeRequester{
    public String fileType="mp3";
    public String fileName="";

    public JFrame jframe;
    public JPanel jpanel;

    public ButtonGroup radio;
    public JRadioButton ogg;
    public JRadioButton mp3;
    public JRadioButton wav;
    

    public SoundTypeRequester(){
	jframe=new JFrame();
	jpanel= new JPanel(new BorderLayout());
	radio=new ButtonGroup();
	mp3=new JRadioButton("mp3");
	ogg=new JRadioButton("ogg");
	wav=new JRadioButton("wav");
	radio.add(mp3);
	radio.add(ogg);
	radio.add(wav);
	jpanel.add(mp3,BorderLayout.WEST);
	jpanel.add(ogg,BorderLayout.WEST);
	jpanel.add(wav,BorderLayout.WEST);

	jframe.pack();
    }

    public static void main(String []args){
	SoundTypeRequester str=new SoundTypeRequester();
    }
}

