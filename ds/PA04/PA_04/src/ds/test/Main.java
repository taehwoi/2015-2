package ds.test;

import java.io.*;
import java.util.Scanner;

import ds.sort.Sorter;


public class Main {
	private static final int ERROR = -1;
	private static final int ADD = 0;
	private static final int REMOVE = 1;
	private static final int SORT = 2;
	private static final int TOP = 3;

	public static void main(String[] args){
    BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
    String line;
		int size = 100001;
		Sorter sorter = new Sorter(size);
		
		while((line = br.readLine()) != null){
      Scanner i_scanner = new Scanner(new StringReader(line));
			//String cmd = i_scanner.next();
			int value = 0;
			int k = 0;
			String type = null;
      final String ERROR = "The value does not exist";
			
			switch(getCommandNum(line.charAt(0))){
			case ADD:
        try {
          value = Integer.parseInt(line.substring(4));
          System.out.println(value);
        } catch (IOException e) { 
          System.out.println(e.toString());
        }
        sorter.add(value);
        System.out.printf("ADD: %d\n",value);
				break;

			case REMOVE:
				value = i_scanner.nextInt();
        if (sorter.remove(value))
          System.out.printf("REMOVE: %d\n",value);
        else
          System.out.println(ERROR);
				break;

			case SORT:
				type = i_scanner.next();
        sorter.sort(type);
        System.out.print("SORT: ");
        System.out.println(sorter);
				break;
				
			case TOP:
				
				k = i_scanner.nextInt();
				type = i_scanner.next();
        System.out.print("TOP: ");
        System.out.println(sorter.top(k,type));
								
				// fill your code
				
				break;
			
			}
			i_scanner.close();
		}
		
		//scanner.close();
	}
	
	private static int getCommandNum(char cmd){
    if (cmd == 'a') 
			return ADD;
    if (cmd == 'r') 
			return REMOVE;
    if (cmd == 's') 
			return SORT;
    if (cmd == 't') 
			return TOP;
		else 
			return ERROR;
	}

}
