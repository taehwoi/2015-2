package ds.hash;

public class HashTable 
{
  int[] ht;
		
  public HashTable(int n) {
    ht = new int[n];
    // fill your code
  }

  public void Start(String policy, int c1, int c2, int c3){

    // fill your code
  }

  public void Insert(int value) {

    // fill your code
  }

  public int find(int value) {

    // fill your code
    return -1;
  }


  private class Probe
  {
    int c1, c2, c3;
    String policy;
    public Probe(String policy, int c1, int c2, int c3)
    {
      this.c1 = c1;
      this.c2 = c2;
      this.c3 = c3;
      this.policy = policy;
    }

    public int p(int i)
    {
      int val = 0;
      switch (policy) {
        case "linear" :
          val =  c1 * i;
          break;
        case "quadratic" :
          val = (c1 * i * i) + (c2 * i) + c3;
          break;
      }     
      return val;
    }
  }
}
