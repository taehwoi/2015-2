package ds.test;

import java.io.StringReader;
import java.util.Scanner;

import ds.sort.Sorter;


public class Main {
	private static final int ERROR = -1;
	private static final int ADD = 0;
	private static final int REMOVE = 1;
	private static final int SORT = 2;
	private static final int TOP = 3;

	public static void main(String[] args){
		Scanner scanner = new Scanner(System.in);
		int size = 200000;
		Sorter sorter = new Sorter(size);
		
		while(scanner.hasNext()){
			String line = scanner.nextLine();
			Scanner i_scanner = new Scanner(new StringReader(line));
			String cmd = i_scanner.next();
			int value = 0;
			int k = 0;
			String type = null;
      final String ERROR = "The value does not exist";
			
			switch(getCommandNum(cmd)){
			case ADD:
				value = i_scanner.nextInt();
        sorter.add(value);
        System.out.print("ADD: " + value + "\n");
				break;

			case REMOVE:
				value = i_scanner.nextInt();
        if (sorter.remove(value))
          System.out.print("REMOVE: "+ value + "\n");
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
		
		scanner.close();
	}
	
	private static int getCommandNum(String cmd){
    if (cmd.charAt(0) == 'a') 
			return ADD;
    if (cmd.charAt(0) == 'r') 
			return REMOVE;
    if (cmd.charAt(0) == 's') 
			return SORT;
    if (cmd.charAt(0) == 't') 
			return TOP;
		else 
			return ERROR;
	}

}
