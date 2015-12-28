import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class MLP2 
{
	public static double[][] Sigmoid(double[][] x) 
	{
		double[][] temp = new double [1000][784];
		for(int i = 0; i < 1000; i++)
			for(int j = 0; j < 784; j++)
				temp[i][j] = (.5 * (Math.tanh(x[i][j]*.5)+1));
		return temp;
	}
	
	public static double[][] dSigmoid(double[][] x) 
	{
		double[][] temp2 = new double [1000][784];
		double[][] temp3 = new double [1000][784];
		temp2 = Sigmoid(x);
		for(int i = 0; i < 1000; i++)
			for(int j = 0; j < 784; j++)
				temp3[i][j] = temp2[i][j] *(1 - temp2[i][j]);
		return temp3;
	}
	
	public static void main(String[] args) throws FileNotFoundException 
	{
		Scanner input = new Scanner(new File("DATA0.txt"));		
		Double[][] IN = new Double[1000][784];		
		String line;
		String[] arrLine;		
		for(int i = 0; i < 1000; i++)
		{
			line = input.nextLine();
			arrLine = line.split(" ");
			
			for(int j = 0; j < 784; j++)
			{
				IN[i][j] = Double.parseDouble(arrLine[j]); 
			}
		}		
		input.close();
		int[] size = new int[1000];
		for(int i = 0; i < 1000; i++)
		{
			size[i] = 0;
		}		
		for(int i = 0; i < 1000; i++)
		{
			for(int j = 0; j < 784; j++)
			{
				size[i] = (int) (size[i] + IN[i][j]);
			}
		}	
		for(int i = 0; i < 1000; i++)
		{
			for(int j = 0; j < 784; j++)
			{
				IN[i][j] = IN[i][j] / size[i];
			}
		}	
		
		Scanner weights = new Scanner(new File("weight1.txt"));		
		Double[][] w1 = new Double[784][784];		
		for(int i = 0; i < 784; i++)
		{
			line = weights.nextLine();
			arrLine = line.split(" ");
			
			for(int j = 0; j < 784; j++)
			{
				w1[i][j] = Double.parseDouble(arrLine[j]); 
			}
		}		
		weights.close();
		
		Scanner weights2 = new Scanner(new File("weight2.txt"));		
		Double[][] w2 = new Double[784][784];		
		for(int i = 0; i < 784; i++)
		{
			line = weights2.nextLine();
			arrLine = line.split(" ");
			
			for(int j = 0; j < 784; j++)
			{
				w2[i][j] = Double.parseDouble(arrLine[j]); 
			}
		}		
		weights2.close();
		
		double[] bias_in = new double[784];		
		for(int i = 0; i < 784; i++)
			bias_in[i] = 0;
		
		double[] bias_out = new double[784];		
		for(int i = 0; i < 784; i++)
			bias_out[i] = 0;
		
		// Variable declarations 
		double[][] act_in = new double[1000][784];
		double[][] hidden = new double[1000][784];
		double[][] d_act_in = new double[1000][784];
		double[][] act_out = new double[1000][784];
		double[][] out = new double[1000][784];
		double[][] d_act_out = new double[1000][784];
		double[][] err = new double[1000][784];
		double[][] hidd_2_out_err = new double[1000][784];
		double[][] in_2_hidd_err = new double[1000][784];
		double[][] w2Transpose = new double[784][784];
		double[][] INTranspose = new double[784][1000];
		double[][] hiddenTranspose = new double[784][1000];
		double[] sum1 = new double[784]; 
		double[] sum2 = new double[784];
		double[][] AVG = new double[1000][784];
		double[][] TARG = new double[1000][784];
		for(int i = 0; i < 1000; i++)
			for(int j = 0; j < 784; j++)
				TARG[i][j] = .91;
		int count = 1000 * 784;
		double avg = 0;
		
		int epoch = 0;
		double sse = 0;
		double WLC = .001;
		double BLC = .001;
		long startTime = System.nanoTime();
		for(int x = 0; x < 100; x++) 
		{
			System.out.print(epoch);
			// act_in = DAT * w1 + bias_in
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					act_in[i][j] = 0;
					for(int k = 0; k < 784; k++)
					{
						act_in[i][j] = act_in[i][j] + IN[i][k] * w1[k][j];
					}
					act_in[i][j] = act_in[i][j] + bias_in[j];
				}
			
			// hidden = Sigmoid(act_in)
			hidden = Sigmoid(act_in);
			
			// d_act_in = dSigmoid(act_in)
			d_act_in = dSigmoid(act_in);
			
			// act_out = hidden * w2 + bias_out
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					act_out[i][j] = 0;
					for(int k = 0; k < 784; k++)
					{
						act_out[i][j] = act_out[i][j] + hidden[i][k] * w2[k][j];
					}
					act_out[i][j] = act_out[i][j] + bias_out[j];
				}
			
			// out = Sigmoid(act_out)
			out = Sigmoid(act_out);
			
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					sse = sse + (out[i][j] - IN[i][j]) * (out[i][j] - IN[i][j]);
				}
			System.out.println("sse "+sse); 
			
			// d_act_out = dSigmoid(act_out)
			d_act_out = dSigmoid(act_out);
			
			// err = - out - IN
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					err[i][j] = (out[i][j]*-1 - IN[i][j]) ;
				}

			// hidd_2_out_err = err .* d_act_out
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					hidd_2_out_err[i][j] = err[i][j] * d_act_out[i][j];
				}
			
			/*// H2OE = H2OE + -(TARG/AVG) + (log(1-TARG)/log(1-AVG))
			// AVG[1000][784]
			// TARG[1000][784] = .91
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					avg = avg + hidden[i][j];
				}
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					AVG[i][j] = avg / count;
				}
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					hidd_2_out_err[i][j] = hidd_2_out_err[i][j] - (-1*(TARG[i][j] / AVG[i][j]) + (Math.log(1 - TARG[i][j]) / Math.log(1 - AVG[i][j])));
				}*/
			
			// in_2_hidd_err = hidd_2_out_err * w2’ .* d_act_in
			for(int i = 0; i < 784; i++)
				for(int j = 0; j < 784; j++)
				{
					w2Transpose[i][j] = w2[j][i];
				}
			for(int i = 0; i < 1000; i++)
			{
				
				for(int j = 0; j < 784; j++)
				{
					in_2_hidd_err[i][j] = 0;
					for(int k = 0; k < 784; k++)
					{
						in_2_hidd_err[i][j] = in_2_hidd_err[i][j] +hidd_2_out_err[i][k] * w2Transpose[k][j];
					}
					in_2_hidd_err[i][j] = in_2_hidd_err[i][j] * d_act_in[i][j];
				}
			}
			
			// bias_in = bias_in + sum(in_2_hidd_err, axis=1)
			for(int i = 0; i < 784; i++)
				sum1[i] = 0;
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					sum1[j] = sum1[j] + in_2_hidd_err[i][j];
				}
			for(int i = 0; i < 784; i++)
				bias_in[i] = bias_in[i] + sum1[i] * BLC;

			// bias_out  = bias_out + sum(hidd_2_out_err, axis=1) 
			for(int i = 0; i < 784; i++)
				sum2[i] = 0;
			for(int i = 0; i < 1000; i++)
				for(int j = 0; j < 784; j++)
				{
					sum2[j] = sum2[j] + hidd_2_out_err[i][j];
				}
			for(int i = 0; i < 784; i++)
				bias_out[i] = bias_out[i] + sum2[i] * BLC;

			// w1 = w1 + IN’ * in_2_hidd_err
			for(int i = 0; i < 784; i++)
				for(int j = 0; j < 1000; j++)
				{
					INTranspose[i][j] = IN[j][i];
				}
			double[][] temp = new double[784][784]; 
			for(int i = 0; i < 784; i++)
			{				
				for(int j = 0; j < 784; j++)
				{
					temp[i][j] = 0;
				}
			}
			for(int i = 0; i < 784; i++)
			{
				
				for(int j = 0; j < 784; j++)
				{
					for(int k = 0; k < 1000; k++)
					{
						temp[i][j] = temp[i][j] + INTranspose[i][k] * in_2_hidd_err[k][j];
					}
					w1[i][j] = w1[i][j] + temp[i][j] * WLC;
				}
			}
			

			// w2 = w2 + hidden‘ * hidd_2_out_err 
			for(int i = 0; i < 784; i++)
				for(int j = 0; j < 1000; j++)
				{
					hiddenTranspose[i][j] = hidden[j][i];
				}
			for(int i = 0; i < 784; i++)
			{				
				for(int j = 0; j < 784; j++)
				{
					temp[i][j] = 0;
				}
			}
			for(int i = 0; i < 784; i++)
			{
				
				for(int j = 0; j < 784; j++)
				{
					for(int k = 0; k < 1000; k++)
					{
						temp[i][j] = temp[i][j] + hiddenTranspose[i][k] * hidd_2_out_err[k][j];
					}
					w2[i][j] = w2[i][j] + temp[i][j] *WLC;
				}
			}
			epoch++;
		}
		long endTime = System.nanoTime();
		System.out.println("Took "+(endTime - startTime)/1000000000 + " seconds"); 
		
	}

}
