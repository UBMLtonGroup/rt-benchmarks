/*Allocate single element arrays till heap runs out */

import java.util.*;

public class fragger2
{

public static void main(String[] args)
{

  /*  List<X[]> arrlist = new ArrayList<X[]>();
    int i =0;
    try
    {
    while(true)
    {
        X[] arr = new X[1];
        arr[0] = new X(i);
        arrlist.add(arr);
        i++;
        System.out.println(i);
    }
    }
    catch(OutOfMemoryError e)
    {
        System.out.println("Out of Memory");
    }
    */
    /*Allocate Large array*/
    //32 MB heap - 622965
    //986 MB heap - 20767725
   int kArraySize = 20767725;
    List<X[]> arrlist = new ArrayList<X[]>();
    for(int i=0;i<kArraySize;i++)
    {
        X[] arr = new X[1];
        arr[0] = new X(i);
        arrlist.add(arr);
    }
   //System.out.println("Allocated large Array");
  
    /*Fragment Array*/
    for(int i =0 ; i<arrlist.size();i+=2)
    {
        arrlist.get(i)[0]=null;
        
    }
    
   //System.out.println("fragmented Array");
    /* Allocate Second Array */
   long startTime =System.nanoTime();
    X[] arr2 = new X[kArraySize/2];
    long endTime   = System.nanoTime();
    long totalTime = endTime - startTime;
    System.out.println(totalTime);
   
}
}
