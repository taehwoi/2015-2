package ds.test;
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
	private static final int ERROR = 8;

	public static void main(String[] args){
		Scanner scanner = new Scanner(System.in);
		BookSearch bookSearch = new BookSearch();
    String ERRORMSG_find
      = "BookSearch can not find the book";
    String ERRORMSG_order
      = "BookSearch does not have any book";

    while(scanner.hasNext()){
      String line = scanner.nextLine();
      Scanner i_scanner = new Scanner(new StringReader(line));
      String cmd = i_scanner.next();
      String name = null;
      String location = null;

      switch(getCommandNum(cmd)){
        case ADD:
          name = i_scanner.next();
          location = i_scanner.next();
          bookSearch.add(name, location);
          System.out.println("ADD: "+name+" "+location);

          // fill your code

          break;
        case REMOVE:
          name = i_scanner.next();
          location = bookSearch.remove(name);
          if (location==null)
            System.out.println(ERRORMSG_find);
          else
            System.out.println("REMOVE: "+name);

          // fill your code
          break;
        case GET:
          name = i_scanner.next();
          location = bookSearch.get(name);
          if (location==null)
            System.out.println(ERRORMSG_find);
          else
            System.out.println("GET: " +name+ " "+location);
          break;
        case SIZE:	
          System.out.println("CURRENT_SIZE: "+bookSearch.size());
          break;
        case ORDER:
          if (bookSearch.size() == 0) 
            System.out.println(ERRORMSG_order);
          bookSearch.order();
          break;
        case FIRST:
          name = bookSearch.first();
          if (name==null) 
            System.out.println(ERRORMSG_order);
          else
            System.out.println("FIRST: " + name);
          break;
        case LAST:
          name = bookSearch.last();
          if (name==null) 
            System.out.println(ERRORMSG_order);
          else
            System.out.println("LAST: " + name);
          break;
        case RANGE:
          String from = i_scanner.next();
          String to = i_scanner.next();
          System.out.println("RANGE: "+bookSearch.range(from,to));
          break;
        case ERROR:
          break;
      }


      i_scanner.close();
    }

    scanner.close();
  }

  private static int getCommandNum(String cmd){
    //System.out.println(cmd);
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
    return ERROR;
  }

}
