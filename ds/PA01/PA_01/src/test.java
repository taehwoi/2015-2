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
    ll.insert(4);
    ll.insert(4);
    ll.moveToEnd();
    System.out.println(ll.remove());
    System.out.println(ll.remove());
    ll.moveToStart();
    ll.remove();
    ll.moveToPos(2);
    System.out.println(ll.length());

    System.out.println(ll.getValue());
    System.out.println(ll.toString());

    
    ll.moveToPos(0);
  }
}
