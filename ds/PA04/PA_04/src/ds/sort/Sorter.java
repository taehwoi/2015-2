package ds.sort;
import java.util.ArrayList;
import java.util.Random;
import java.util.ListIterator;
import java.util.Collections;

public class Sorter{
  ArrayList<Integer> array;
  Random rand;
  boolean sorted;

  final String ASCEND = "ascend";
  final String DESCEND = "descend";
  final String LARGEST = "largest";
  final String SMALLEST = "smallest";
	
	public Sorter(int n){
    array = new ArrayList<>(n);
    rand = new Random();
    sorted= false;
	}
	
 
	public void add(int value)
  { 
    array.add(value);
    sorted = false;
	}
	
	public boolean remove(int value)
  {//doesn't change sorted state

    //box value to use it as a value, not index
    return array.remove((Integer) value); 
	}
	
	public void sort(String type)
  {
    if (!sorted)
      quickSort(0,array.size()-1);
    switch (type)
    {
      case ASCEND:
        sorted = true;
        break;
      case DESCEND: //reverse the array.
        Collections.reverse(array);
        sorted = false;
        break;
    }
    return;
	}
	
	public String top(int k, String type){
    ListIterator<Integer> I;
    String result = "";

    if (sorted) {
      switch (type) {
        case SMALLEST:
          I = array.listIterator();
          while(k-- > 0)
            result += I.next() + " ";
          break;
        case LARGEST:
          I = array.listIterator(array.size());
          while(k-- > 0)
            result += I.previous() + " ";
          break;
      }
    }
    else {
      sort(ASCEND); //will change sort state
      return top(k,type);
    }
    return result;
  }

  public String toString()
  {//return space seperated values
    String ret = array.toString() //csv surrounded with []
      .replace(",","") //remove commas
      .replace("[","") //remove []s
      .replace("]","")
      .trim();

    return ret;
  }

  private void quickSort(int first, int last)
  {
    int temp;
    int f = first;
    int l = last;

    int pivotIndex = rand.nextInt(last-first + 1) + first;
    int pivot = array.get(pivotIndex);

    while (f <= l) {

      while (array.get(f) < pivot) f++;
      while (array.get(l) > pivot) l--;

      if (f <= l){
        temp = array.get(f);
        array.set(f, array.get(l));
        array.set(l,temp);

        f++;
        l--;
      }
    }

    if (f < last) quickSort(f, last);
    if (first < l) quickSort(first, l);
  }

}
