/*allocate on elarge array method*/

public class fragger
{

public static void main(String[] args)
{
    /*Allocate Large array*/
    int kArraySize = 50000000;
    X[] arr = new X[kArraySize];
    for(int i=0;i<arr.length;i++)
    {
        arr[i] = new X(i);
    }
    //System.out.println("Allocated large Array");
    
    /*Fragment Array*/
    for(int i =0 ; i<arr.length;i+=2)
    {
        arr[i]=null;
    }

    /* Allocate Second Array */
    long startTime =System.nanoTime();
    X[] arr2 = new X[kArraySize/2];
    long endTime   = System.nanoTime();
    long totalTime = endTime - startTime;
    System.out.println(totalTime);
}
}
