import java.util.Scanner;
public class test 
{
  public static void main (String[] args) {
    SingleLinkedList<Integer> ll = new SingleLinkedList<>();
    ll.insert(7);
    ll.insert(0);
    ll.append(1);
    ll.append(2);
    ll.append(3);
    ll.append(4);
    System.out.println(ll.length());
    System.out.println("list: "+ ll.toString());
    

    ll.moveToEnd();
    System.out.println(ll.remove());
    System.out.println(ll.remove());//should be 3
    System.out.println("list: "+ ll.toString());
    ll.moveToStart();
    ll.remove();
    System.out.println(ll.toString());

  }
}
