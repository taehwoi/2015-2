package ds.bst;
import java.io.StringReader;
import java.util.Scanner;

import ds.bst.BookSearch;

public class Main {
	private static final int ADD = 0;
	private static final int REMOVE = 1;
	private static final int GET = 2;
	private static final int SIZE = 3;
	private static final int ORDER = 4;
	private static final int FIRST = 5;
	private static final int LAST = 6;
	private static final int RANGE = 7;
	private static final int DELMIN = 8;
	private static final int ERROR = 9;

	public static void main(String[] args){
		Scanner scanner = new Scanner(System.in);
		BST<Integer,Integer> bookSearch = new BST<>();
		
		while(scanner.hasNext()){
			String line = scanner.nextLine();
			Scanner i_scanner = new Scanner(new StringReader(line));
			String cmd = i_scanner.next();
			int name = 0;
			int location = 0;
			
			switch(getCommandNum(cmd)){
			case ADD:
				name = i_scanner.nextInt();
				location = i_scanner.nextInt();
				bookSearch.insert(name, location);
				
				// fill your code
			
				break;
			case REMOVE:
        name = i_scanner.nextInt();
				name = bookSearch.remove(name);
        break;
				// fill your code
			case GET:
        name = i_scanner.nextInt();
				System.out.println(bookSearch.find(name));
				// fill your code
				break;
			case SIZE:	

				// fill your code
				
				break;
			case ORDER:
        bookSearch.inorder();
				break;
			case FIRST:
        System.out.println(bookSearch.min().element());
				break;
			case LAST:
        System.out.println(bookSearch.max().element());
				break;
			case RANGE:
				int from = i_scanner.nextInt();
				int to = i_scanner.nextInt();
        System.out.println(bookSearch.rangeSearch(from,to));
				break;
			case DELMIN:
        bookSearch.delmin();
				break;
			case ERROR:
				break;
			}
			
			
			i_scanner.close();
		}
		
		scanner.close();
	}
	
	private static int getCommandNum(String cmd){
		if(cmd.equals("add"))
			return ADD;
		else if(cmd.equals("remove"))
			return REMOVE;
		else if(cmd.equals("get"))
			return GET;
		else if(cmd.equals("size"))
			return SIZE;
		else if(cmd.equals("order"))
			return ORDER;
		else if(cmd.equals("first"))
			return FIRST;
		else if(cmd.equals("last"))
			return LAST;
		else if(cmd.equals("range"))
			return RANGE;
		else if(cmd.equals("delmin"))
			return DELMIN;
		return ERROR;
	}

}
