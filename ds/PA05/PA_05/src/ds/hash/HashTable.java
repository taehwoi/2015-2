package ds.hash;
//(h(K) + p(k,i)) mod M
public class HashTable 
{
  int[] ht;
  private int M;
  Probe probe;
  
		
  public HashTable(int n) {
    M = n;
    ht = new int[M];
  }

  public void Start(String policy, int c1, int c2, int c3){
    probe = new Probe(policy, c1, c2, c3);
  }

  public void Insert(int value) {
    int key=0;
    for (int i=0;;i++) {
      key = (h(value) + probe.p(i)) % M;
      if (ht[key] == (Integer) null) {
        ht[key] = value;
        break;
      }
    }
  }

  public int find(int value) {
    int key=0;
    for (int i=0;;i++) {
      key = (h(value) + probe.p(i)) % M;
      if (ht[key] == value) 
        return key;
      else if (ht[key] == (Integer) null) {
        break;
      }
    }
    return -1;
  }

  private int h(int K)
  {
    return K % M;
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
