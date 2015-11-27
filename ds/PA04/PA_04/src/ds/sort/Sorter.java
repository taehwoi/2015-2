package ds.sort;
import java.util.Random;
import java.util.ListIterator;
import java.util.Arrays;

//TODO: use array bast implmentations.
public class Sorter{
  Integer[] array;
  Random rand;
  boolean sorted;

  final String ASCEND = "ascend";
  final String DESCEND = "descend";
  final String LARGEST = "largest";
  final String SMALLEST = "smallest";
  private int cnt;
	
	public Sorter(int n) {
    array = new Integer[n];
    rand = new Random();
    sorted= false;
    cnt = 0;
	}
	
 
	public void add(int value)
  { 
    array[cnt] = value;
    sorted = false;
	}
	
	public boolean remove(int value)
  {//doesn't change sorted state
    int pos = find(value);

    //from pos, shift everything.
    //return array.remove((Integer) value); 
    return true;
	}

  private int find(int value)
  {
    int index = 0;
    if (sorted) //bin search
      index = binSearch(0,cnt-1,value); 
    else
      index = seqSearch(value); 

    return index;
  }

  private int binSearch(int first, int last, int val)
  { 
    int f = first;
    int l = last;
    int pivot = (f + l) / 2;

    while (f <= l){
      pivot = (f + l) / 2;

      if (array[pivot] < val) f = pivot + 1;
      else if (array[pivot] > val) l = pivot - 1;
      else return pivot;
    }
    return -1; //no such element
  }

  private int seqSearch(int val)
  {
    for (int i = 0; i<cnt; i++) 
    {
      if (array[i] == val) 
        return i;
    }
    return -1;
  }
	
	public void sort(String type)
  {
    if (!sorted)
      quickSort(0,cnt-1);
    switch (type)
    {
      case ASCEND:
        sorted = true;
        break;
      case DESCEND: //reverse the array.
        //FIXME //don't reverse, just print it reversed?
        //Collections.reverse(array);
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
          for (int i=0; i<k; i++ ) 
            result += array[i] + " ";
          break;
        case LARGEST:
          for (int i=0; i<k; i++ ) 
            result += array[cnt-i-1] + " ";
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
    String ret = Arrays.toString(array) //csv surrounded with []
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
    int pivot = array[ pivotIndex ];

    while (f <= l) {

      while (array[f] < pivot) f++;
      while (array[l] > pivot) l--;

      if (f <= l){
        temp = array[f];
        array[f] = array[l];
        array[l] = temp;

        f++;
        l--;
      }
    }

    if (f < last) quickSort(f, last);
    if (first < l) quickSort(first, l);
  }

}
