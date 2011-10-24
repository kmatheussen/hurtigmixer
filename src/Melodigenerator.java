
public class Melodigenerator{
    public static void main(String args[]){
	String urlbase="jar:file:hurtigmixer.jar!/";
	Events.configURL="file:config/config.txt";
	Events.runSchemeEntry(urlbase,"main-standalone.scm");
    }
}

