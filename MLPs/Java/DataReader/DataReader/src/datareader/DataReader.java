/*
 * Im Gonna warn you right now... My java is dog shit.
 */
package datareader;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;



public class DataReader {

    /**
     * @param args the command line arguments
     */
    
    
    final String DELIM=" ";
    public int[][] FILE_HOLDER;
    public double[][] DATA;
    public int[][] LABS;
    public BufferedReader F_READ;
    public String FILE_STRING[][];
    public int H = 1000;
    public int W=0;
    public String DEFAULT_FPATH = "DataFiles\\Data1.txt";
    
    
    public static void main(String[] args) 
    {
        
        DataReader DR = new DataReader();
        
        DR.LoadFile();
        int j = 0;
        
        
    }
    
    
    public void LoadFile()
    {
        
        String F_PATH = DEFAULT_FPATH;
        String CURR_LINE = "";
        try
        {
            F_READ = new BufferedReader(new FileReader(F_PATH));   
            F_READ.readLine();   
            int i = 0;
            int j = 0;
            while ((CURR_LINE = F_READ.readLine())!= null)
            {   if (W==0)
                {
                    W = CURR_LINE.length();
                    FILE_STRING = new String[H][W];
                }
                
                FILE_STRING[i][CURR_LINE.length()-1] = CURR_LINE;
                i=i+1;
             }
            
            TokenProcessor();
        
        } catch (Exception E) {
            System.out.println("THAT AINT A FILE BRUH");
            
        }
        
    }
    
    public void TokenProcessor() 
    {
        /*GOD I HATE FILE READING IN JAVA!
         WHY DID I AGREE TO DO THI!!*/
        
        
        
        
        
        
        
    }
    
    
}
