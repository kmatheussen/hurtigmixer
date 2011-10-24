
import java.awt.*;
import javax.swing.*;
import javax.swing.text.*;
import java.awt.event.*;        //for action events

import sisc.data.*;
import sisc.interpreter.*;


public class SoundTypeRequester implements ActionListener{

    Procedure contFunc;

    public String fileType="mp3";
    public String fileName="";

    public JFrame jframe;
    public JPanel jpanel;
    public JPanel jpanel2;
    public JPanel jpanel3;

    public ButtonGroup radio;
    public JRadioButton ogg;
    public JRadioButton mp3;
    public JRadioButton wav;
    public JRadioButton midi;

    public JTextField jtext=null;
    public JTextField jtext2=null;
    JButton ok;
    JButton cancel;

    boolean includeFileName;
    boolean askName;

    public void actionPerformed(ActionEvent e) {
	final Events events=CreateFrame.events;

	System.out.println(e.getSource());
	//System.out.println(jtext.getText());

	jframe.setVisible(false);


	if(e.getSource()==cancel)
	    return;

	String format="";
	    
	if(ogg.isSelected())
	    format="ogg";
	else if(mp3.isSelected())
	    format="mp3";
	else if(wav.isSelected())
	    format="wav";
        else
            format="midi";

	if(includeFileName==false){
	    Value args[]={
		new SchemeString(format)
	    };
	    events.evalSomething(contFunc,args,Context.enter(events.ctx));
	}else{
            String composer=jtext2==null?"":jtext2.getText().equals("")?"":jtext2.getText()+"-";
            String composition=jtext.getText();
            if( composition.endsWith(".ogg")==false
                && format.equals("ogg"))
                composition+=".ogg";
            if( composition.endsWith(".mp3")==false
                && format.equals("mp3"))
                composition+=".mp3";
            if( composition.endsWith(".mid")==false
                && format.equals("midi"))
                composition+=".mid";
	    Value args[]={
		new SchemeString(composer+composition),
		new SchemeString(format)
	    };
	    events.evalSomething(contFunc,args,Context.enter(events.ctx));
	}
    }


    public SoundTypeRequester(String title,boolean includeWav,boolean includeMidi,boolean includeFileName,boolean askName,Procedure contFunc){
	this.contFunc=contFunc;
	this.includeFileName=includeFileName;
	this.askName=askName;

	FlowLayout layout=new FlowLayout();
	jframe=new JFrame(title);

	if(includeFileName)
	    jframe.setLayout(new GridLayout(3,1));
	else
	    jframe.setLayout(new GridLayout(2,1));
	

	{
	    jpanel= new JPanel(layout);
	    radio=new ButtonGroup();
	    mp3=new JRadioButton("mp3");
	    ogg=new JRadioButton("ogg");
	    wav=new JRadioButton("wav");
	    midi=new JRadioButton("midi");
	    radio.add(mp3);
	    radio.add(ogg);
	    if(includeWav)
		radio.add(wav);
	    if(includeMidi)
		radio.add(midi);
	    mp3.setSelected(true);
	    
            int selpos=0;
	    jpanel.add(mp3,selpos++);
	    jpanel.add(ogg,selpos++);
	    if(includeWav)
		jpanel.add(wav,selpos++);
	    if(includeMidi)
		jpanel.add(midi,selpos++);
	    
	    jframe.add(jpanel);
	    jframe.pack();
	}

	if(includeFileName)
	    {
                jpanel2=new JPanel(new FlowLayout());
                
		if(askName){
		    jpanel2.add(new JLabel("Ditt navn:"));
		    jtext2=new JTextField(20);
		    jpanel2.add(jtext2);
		    jtext2.addActionListener(this);
		}

		jframe.add(jpanel2);

                jpanel2.add(new JLabel("Filnavn:"));
                jtext=new JTextField(20);
                jpanel2.add(jtext);
                jtext.addActionListener(this);
                
                jframe.add(jpanel2);
	    }

	{
	    jpanel3=new JPanel(new FlowLayout());
	    ok=new JButton("OK");
	    jpanel3.add(ok);
	    cancel=new JButton("Avbryt");
	    jpanel3.add(cancel);
	    ok.addActionListener(this);
	    cancel.addActionListener(this);
	    
	    jframe.add(jpanel3);
	}


	jframe.pack();

	jframe.setLocationRelativeTo(null);
	jframe.setVisible(true);
    }

    /*
    public static void main(String []args){
	SoundTypeRequester str=new SoundTypeRequester("Aiai",true,true);
    }
    */
}

