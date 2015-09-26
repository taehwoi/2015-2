import java.util.Scanner;
public class test 
{
  public static void main (String[] args) {
    SingleLinkedList<Integer> ll = new SingleLinkedList<>();
    ll.insert(5);
    ll.insert(3);
    ll.append(9);
    ll.append(7);
    ll.append(10);
    //ll.insert(4);
    //ll.insert(4);
    ll.moveToEnd();
    ll.remove();
    ll.moveToStart();
    ll.remove();
    System.out.println(ll.getValue());
    System.out.println(ll.toString());

    
    ll.moveToPos(0);


  }
}
