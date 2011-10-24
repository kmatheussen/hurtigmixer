

// Code made by looking at http://www.jguru.com/faq/view.jsp?EID=62798




import java.io.*;
import java.net.*;


import sisc.interpreter.*;
import sisc.data.*;



public class Upload  {

    public static void upload(String fileName, File dasFile, String urlString_base, String userName,final Procedure progressFunc)
	throws Exception
    {

	final Events events=CreateFrame.events;
	Interpreter interpreter=null;
	if(events!=null)
	    interpreter=Context.enter(events.ctx);

	String urlString=urlString_base+"?username="+userName+"";
	
	HttpURLConnection conn = null;
	BufferedReader br = null;
	DataOutputStream dos = null;
	DataInputStream inStream = null;
	
	InputStream is = null;
	OutputStream os = null;
	boolean ret = false;
	String StrMessage = "";
	String exsistingFileName = fileName;
	
	String lineEnd = "\r\n";
	//String lineEnd = "\n";
	String twoHyphens = "--";
	String boundary =  "*****";
	

	int bytesRead, bytesAvailable, bufferSize;
	
	byte[] buffer;
	
	int maxBufferSize = 32*1024;
	
	
	String responseFromServer = "";
	
	
	try
	    {
		//------------------ CLIENT REQUEST
		
		FileInputStream fileInputStream = new FileInputStream(dasFile);
		
		// open a URL connection to the Servlet 
		
		URL url = new URL(urlString);
		
		
		// Open a HTTP connection to the URL
		
		conn = (HttpURLConnection) url.openConnection();
		
		// Allow Inputs
		conn.setDoInput(true);
		
		// Allow Outputs
		conn.setDoOutput(true);
		
		// Don't use a cached copy.
		conn.setUseCaches(false);
		
		// Use a post method.
		conn.setRequestMethod("POST");
		
		conn.setRequestProperty("Connection", "Keep-Alive");
		
		conn.setRequestProperty("Content-Type", "multipart/form-data;boundary="+boundary);
		
		dos = new DataOutputStream( conn.getOutputStream() );
		
		dos.writeBytes(twoHyphens + boundary + lineEnd);
		if(true){
		    dos.writeBytes("Content-Disposition: mutipart/form-data; name=\"userfile\";"
				   + " filename=\"" + exsistingFileName +"\";"
				   //+ "; name2=\"username\"; namename=\""+ userName + "\";"  
				   + lineEnd
				   );
		    //		    dos.writeBytes("form-data; name=\"username\";"
		    //		   + " namename=\""+ userName + "\"" + lineEnd);
		    dos.writeBytes("Content-Disposition: form-data; name=\"username\"; a=\"50\"; " 
				   //+ " namename=\""+ userName + "\"" 
				   + lineEnd);
		    
		}else{
		    dos.writeBytes(
				   "<FORM ENCTYPE='multipart/form-data'\n"
				   +" method='POST' action='gotfile.php'>\n"
				   +"<INPUT TYPE='file' NAME='userfile' VALUE='/hom/kjetism/testing.mp3'>\n"
				   +"<INPUT TYPE='submit' VALUE='upload'>\n"
				   +"</FORM>\n");
		}

		dos.writeBytes(lineEnd);		    

		// create a buffer of maximum size
		
		bytesAvailable = fileInputStream.available();
		bufferSize = Math.min(bytesAvailable, maxBufferSize);
		buffer = new byte[bufferSize];
		
		// read file and write it into form...
		
		bytesRead = fileInputStream.read(buffer, 0, bufferSize);
		int totalBytes=0;

		while (bytesRead > 0)
		    {
			dos.write(buffer, 0, bufferSize);
			bytesAvailable = fileInputStream.available();
			bufferSize = Math.min(bytesAvailable, maxBufferSize);
			bytesRead = fileInputStream.read(buffer, 0, bufferSize);

			if(progressFunc!=null){
			    totalBytes+=bytesRead;
			    Quantity args[]={Quantity.valueOf(totalBytes)};
			    CreateFrame.events.addReturnFunc(progressFunc,args,interpreter);
			}
		    }
		
		// send multipart form data necesssary after file data...
		
		dos.writeBytes(lineEnd);
		dos.writeBytes(twoHyphens + boundary + twoHyphens + lineEnd);
		
		// close streams
		
		fileInputStream.close();
		dos.flush();
		dos.close();
		
		
	    }
	catch (MalformedURLException ex)
	    {
		System.out.println("From ServletCom CLIENT REQUEST:"+ex);
	    }
	
	catch (IOException ioe)
	    {
		System.out.println("From ServletCom CLIENT REQUEST:"+ioe);
	    }

	
	//------------------ read the SERVER RESPONSE
	

	try
	    {
		inStream = new DataInputStream ( conn.getInputStream() );
		String str;
		while (( str = inStream.readLine()) != null)
		    {
			System.out.println("Server response is: "+str);
			System.out.println("");
		    }
		inStream.close();
		
	    }
	catch (IOException ioex)
	    {
		System.out.println("From (ServerResponse): "+ioex);
		
	    }

    }

    public static void main(String []args){
	try{
	    upload("/hom/kjetism/testing.mp3",new File("/hom/kjetism/testing.mp3"),"http://127.0.0.1/kjetism/updown/gotfile.php","kjetil",null);
	}catch(Exception e){
	    e.printStackTrace();
	}
    }
    
}


