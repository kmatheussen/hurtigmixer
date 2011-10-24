
import java.awt.*;
import javax.swing.*;
import sisc.data.*;
import sisc.interpreter.*;


public class FileRequester extends JFileChooser{
    /*
    public String load(String text){
	jFileChooser.showOpenDialog(frame);
	System.out.println("Got it!\n"+jFileChooser.getSelectedFile().getAbsolutePath());
	return jFileChooser.getSelectedFile().getAbsolutePath();

    }
    */

    public void myShowDialog(boolean isLoading, Procedure returnFunc){
	final Events events=CreateFrame.events;
	int result;
	Container frame=events.container;
	    
	if(isLoading)
	    result=showOpenDialog(frame);
	else
	    result=showSaveDialog(frame);

	Quantity args[]={Quantity.valueOf(result)};
	
	events.evalSomething(returnFunc,args,Context.enter(events.ctx)); // Don't quite know how the program flow is after showDialog is called. Create a new Interpreter to be sure everything's okay.
    }
}

/*
(def-class (<filerequester> parent :optional directory)


  (define frame (if is-running-applet
		    (new <java.awt.Frame>)
		    (-> parent get-frame)))


  (define jfile-chooser (new <javax.swing.JFileChooser>))
  ;;(-> jfile-chooser setMinimumSize (new <java.awt.Dimension> 500 700))

  (-> jfile-chooser setSize 500 800)
      
  (define size (-> jfile-chooser getSize))


  (define jfile-filter (new <JFileFilter>))
  (for-each  (<- jfile-filter addType)
	     '("mp3" "mp2" "ogg" "wav" "aif" "aifc" "aiff" "wave"))
  (-> jfile-chooser addChoosableFileFilter jfile-filter)
  

  (define (request is-loading text newdirectory)
    (-> jfile-chooser setSize size)
    (define result (if is-loading
		       (-> jfile-chooser showOpenDialog frame)
		       (-> jfile-chooser showSaveDialog frame)))
    (set! size (-> jfile-chooser getSize))

    (define directory (-> (-> jfile-chooser getCurrentDirectory) getAbsolutePath))
    (define file (and (= result (-> jfile-chooser APPROVE_OPTION))
		      (-> (-> jfile-chooser getSelectedFile) getName)))
    

    (c-display "directory:" directory)

    (c-display "file:" file)


    (values directory
	    file
	    (if file
		(-> (-> jfile-chooser getSelectedFile) getAbsolutePath)
		directory)))


#|
  (define (request is-loading text newdirectory)
    (define filedialog (new <java.awt.FileDialog> frame text))

    (if newdirectory
	(-> filedialog setDirectory newdirectory))
    (if is-loading
	(-> filedialog setMode (-> filedialog LOAD))
	(-> filedialog setMode (-> filedialog SAVE)))

    (-> filedialog setVisible #t)

    (set! directory (-> filedialog getDirectory))

    (c-display "filerequest dir / file: \"" directory "\" / \"" (-> filedialog getFile) "\".")

    (values directory
	    (-> filedialog getFile)
	    (if (-> filedialog getFile)
		(string-append directory (-> filedialog getFile))
		directory)))
|#

  (def-method (load text :optional (newdirectory directory))
    (request #t text newdirectory))

  (def-method (save text :optional (newdirectory directory))
    (request #f text newdirectory)))

*/
