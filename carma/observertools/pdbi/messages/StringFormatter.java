package carma.observertools.pdbi.messages;

import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * Class to format strings to make them more readable
 * @author D. N. Friedel
 *
 */
public class StringFormatter {
	/**
	 * method to word wrap the given string
	 * @param data the string to be word wrapped
	 * @param length the length of the line in characters
	 * @return the formatted string in html
	 */
	public static String wordWrapHTML(String data,int length){
		String wrap = "";
		// break up the string
		StringTokenizer st = new StringTokenizer(data);
		int count = 0;
		// put it back together with line breaks
		while(st.hasMoreTokens()){
			String word = st.nextToken();
			int len = word.length();
			if(len > length){
				if(count == 0){
					wrap += word + "<br>";
				}
				else{
					wrap += "<br>" + word + "<br>";
					count = 0;
				}
			}
			else{
				// don't break up font elements
				if(count + len > length){
					if(word.contains("color=")){
						wrap += " " + word + "<br>";
						count = 0;
					}
					else{
						wrap += "<br>" + word;
						count = len;
					}
				}
				else{
					wrap += " " + word;
					count += len + 1;
				}
			}
		}
		return wrap;
	}

	/**
	 * method to word wrap the given string and return it as an array
	 * @param data the string to be word wrapped
	 * @param length the length of each line
	 * @return the array of lines
	 */
	public static ArrayList<String> wordWrap(String data,int length){
		ArrayList<String> wrap = new ArrayList<String>();
		String line = "";
		// break up the string
		StringTokenizer st = new StringTokenizer(data);
		int count = 0;
		// put it back together
		while(st.hasMoreTokens()){
			String word = st.nextToken();
			int len = word.length();
			if(len > length){
				if(count == 0){
					line += word;
					wrap.add(line);
					line = "";
				}
				else{
					wrap.add(word);
					line = "";
					count = 0;
				}
			}
			else{
				if(count + len > length){
					wrap.add(line);
					line = word;
					count = len;
				}
				else{
					line += " " + word;
					count += len + 1;
				}
			}
		}
		if(line.length() != 0){
			wrap.add(line);
		}
		return wrap;
	}
}
