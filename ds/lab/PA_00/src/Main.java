
import java.util.Scanner;

/**
 * 
 * @author Data Structure T.A.
 * @since 2015-09-18
 * 
 */
public class Main {
	public static void main(String[] args){
		System.out.println("This is an example of a programming assignment");
		
		Scanner scan = new Scanner(System.in);
		while(scan.hasNext()){
			int value = scan.nextInt();
			YourDataStructure ds = new YourDataStructure();
			ds.setValue(value);
			System.out.println(ds.getValue());
		}
		scan.close();
	}
}
