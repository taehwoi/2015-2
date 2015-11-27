package ds.sort;
import java.util.Random;
import java.util.ListIterator;
import java.util.Arrays;

//TODO: use array bast implmentations.
public class Sorter{
  static Integer[] array;
  static Random rand;
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
    array[cnt++] = value;
    sorted = false;
	}
	
	public boolean remove(int value)
  {//doesn't change sorted state
    int pos = find(value);

    if (pos != -1) {
      for (int i = pos; i < cnt ;i++ ) 
        array[i] = array[i+1];
      array[cnt] = null;
      cnt--;
      return true;
    }
    return false;
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
        rev();
        sorted = false;
        break;
    }
    return;
	}

  private void rev()
  {
    for (int i=0; i < cnt/2 ; i++) 
    {
      int tmp = array[i];
      array[i] = array[cnt - i - 1];
      array[cnt - i - 1] = tmp;
    }
  }
	
	public String top(int k, String type){
    StringBuilder sb = new StringBuilder(k*5);

    if (sorted) {
      switch (type) 
      {
        case SMALLEST:
          for (int i=0; i<k; i++) {
            sb.append(array[i]);
            sb.append(' ');
          }
          break;
        case LARGEST:
          for (int i=0; i<k; i++) {
            sb.append(array[cnt-i-1]);
            sb.append(' ');
          }
          break;
      }
    }
    else {
      sort(ASCEND); //will change sort state
      return top(k,type);
    }
    return sb.toString();
  }

  public String toString()
  {//return space seperated values
    //String ret = Arrays.toString(array) //csv surrounded with []
      //.replace(",","") //remove commas
      //.replace("[","") //remove []s
      //.replace("]","")
      //.trim();
    //String ret;
    StringBuilder sb = new StringBuilder(cnt*5);

    for (int i=0; i<cnt; i++) {
      sb.append(array[i]);
      sb.append(' ');
    }

    return sb.toString();
  }

  private static void quickSort(int first, int last)
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
