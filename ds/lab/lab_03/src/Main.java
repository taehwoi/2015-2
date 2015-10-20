
import java.io.StringReader;
import java.util.Scanner;

public class Main {
	private static final int INSERT = 0;
	private static final int PREORDER = 1;
	private static final int INORDER = 2;
	private static final int POSTORDER = 3;
	private static final int SIZE = 4;
	private static final int ERROR = 5;

	public static void main(String[] args){
		Scanner scanner = new Scanner(System.in);
		BST<Integer,Integer> BSTree = new BST<>();
		
		while(scanner.hasNext()){
			String line = scanner.nextLine();
			Scanner i_scanner = new Scanner(new StringReader(line));
			String cmd = i_scanner.next();
			int key = 0;
			int value = 0;
			
			switch(getCommandNum(cmd)){
			case INSERT:
				key = i_scanner.nextInt();
				value = i_scanner.nextInt();
        BSTree.insert(key,value);
				
				// fill your code
			
				break;
			case PREORDER:
        BSTree.preorder();
				
				// fill your code
			
        break;
			case INORDER:
        BSTree.inorder();

				// fill your code
			
				break;
			case POSTORDER:	
        BSTree.postorder();

				// fill your code
				
				break;
			case SIZE:	

				// fill your code
				
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
		if(cmd.equals("insert"))
			return INSERT;
		else if(cmd.equals("preorder"))
			return PREORDER;
		else if(cmd.equals("inorder"))
			return INORDER;
		else if(cmd.equals("postorder"))
			return POSTORDER;
		else if(cmd.equals("size"))
			return SIZE;
		return ERROR;
	}

}
