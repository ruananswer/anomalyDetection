package com.oneapm.twitterAnomalyDetect;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Arrays;
import com.github.brandtg.*;
import org.apache.commons.math3.util.MathUtils;
import org.apache.commons.math3.distribution.TDistribution;
import org.apache.commons.math3.stat.descriptive.moment.Mean;
import org.apache.commons.math3.stat.descriptive.moment.Variance;
import org.apache.commons.math3.stat.descriptive.rank.Median;

public class AnomalyDetection {
	/**
	   * Accepts an input CSV file containing raw time series, e.g.
	   *
	   * "date","value"
	   *  2015-02-26 08:00:00,28.9189627228525
	   *  2015-02-26 09:00:00,32.5127691537306
	   * And outputs the twitter anomaly detect result
	   */
	
	public static void main(String[] args) {
		// input data
		ArrayList<Double> rawData = inputData("data.csv");
		
		// detect
		ArrayList<AnomalyPoint> anoms = detectAnoms(rawData, 0.49, 0.05, 24, true);
		System.out.print("ss");
		
	}
	
	class AnomalyPoint{
		private int idx;
		private double score;
	};
	
	// input data
	public static ArrayList<Double> inputData(String fileName) {
		if (fileName.length() < 1)
			return null;
		ArrayList<Double> data = new ArrayList<Double>();
		try {
			File csv = new File(fileName);
			BufferedReader br = new BufferedReader(new FileReader(csv));
			String line = "";
			br.readLine();
			while ((line = br.readLine()) != null) {
				StringTokenizer st = new StringTokenizer(line, ",");
				st.nextToken();
				String temp = st.nextToken();
				if (temp == null)
					break;
				data.add(Double.parseDouble(temp));
			}
			br.close();
		}catch (FileNotFoundException e) {
			e.printStackTrace();
		}catch (IOException e) {
			e.printStackTrace();
		}
		return data;
	}
	
	// anomaly detection vec
	
	
	/* detect anoms 
	Detects anomalies in a time series
	Args:
	    data: Time series to perform anomaly detection on.
	  	k: Maximum number of anomalies that S-H-ESD will detect as a percentage of the data.
	  	alpha: The level of statistical significance with which to accept or reject anomalies.
	  	num_obs_per_period: Defines the number of observations in a single period, and used during seasonal decomposition.
	  	verbose: Additionally printing for debugging.
  	Returns:
  		A list containing the anomalies (anoms) with score
	*/
	public static ArrayList<AnomalyPoint> detectAnoms(ArrayList<Double> rawData, double k, double alpha, int numObsPerPeriod, boolean verbose) {
		if (numObsPerPeriod <= 0) {
			System.out.println("must supply period length for time series decomposition");
			return null;
		}
		
		int numObs = rawData.size();
		
		if (numObs < numObsPerPeriod * 2) {
			System.out.println("Anom detection needs at least 2 periods worth of data");
			return null;
		}
		
		// use stl function
		// Step 1: Decompose data. This returns a univarite remainder which will be used for anomaly detection. Optionally, we might NOT decompose.
		double[] timePart = new double[numObs];
		double[] dataPart = new double[numObs];
		for (int i = 0; i < numObs; i++) {
			timePart[i] = i + 1;
			dataPart[i] = rawData.get(i);
		}
		
		final StlConfig config = new StlConfig();
		config.setNumberOfObservations(numObsPerPeriod);
		config.setNumberOfInnerLoopPasses(10);
	    config.setNumberOfRobustnessIterations(1);
	    config.setSeasonalComponentBandwidth(0.75);
	    config.setLowPassFilterBandwidth(0.30);
	    config.setTrendComponentBandwidth(0.10);
		config.setNumberOfDataPoints(dataPart.length);
		
		final StlDecomposition stl = new StlDecomposition(config);
		final StlResult res = stl.decompose(timePart, dataPart);
		
		Mean mean = new Mean(); 
		Variance variance = new Variance();
		Median median = new Median();		
		
		// generate data 
		// Remove the seasonal component, and the median of the data to create the univariate remainder
		double[] data = new double[numObs];
	    double[] trend = res.getTrend();
	    double[] seasonal = res.getSeasonal();
	    double[] remainder = res.getRemainder();
	    double rawMedian = median.evaluate(data);
		for (int i = 0; i < numObs; i++) {
			data[i] = dataPart[i] - trend[i] - seasonal[i] - rawMedian; 
		}
		
		int maxOutliers = (int) (numObs * k);
		if (maxOutliers == 0) {
			System.out.println("With longterm=TRUE, AnomalyDetection splits the data into 2 week periods by default. You have" + numObs + "observations in a period, which is too few. Set a higher piecewise_median_period_weeks.");
			return null; 
		}
		
		double[] rIdx = new double[maxOutliers];
		double[] rScore = new double[maxOutliers];
		int numAnoms = 0;
		
		double dataMean = mean.evaluate(data);
		double dataVariance = variance.evaluate(data);
		double dataMedian = median.evaluate(data);
		
		if (Math.abs(dataVariance) < 1e-5) {
			System.out.println("Input data does not have enough variance");
			return null;
		}
		
		class sortTemp {
			private int id; 
			private double ares;
			
			public sortTemp(int i, double d) {
				id = i;
				ares = d;
			}
			
			public int getId() {
				return id;
			}
			
			public double getAres() {
				return ares;
			}
		};
		
		class tempComparator implements Comparator<sortTemp> {
			@Override
			public int compare(sortTemp s1, sortTemp s2) {
				if (s1.getAres() > s2.getAres())
					return 1;
				else if (s1.getAres() < s2.getAres())
					return -1;
				else {
					if (s1.getId() > s2.getId())
						return 1;
					else 
						return -1;
				}
			}
		}
		
		List<sortTemp> dataAres = new ArrayList<sortTemp>();
		
		for (int i = 0; i < numObs; i++) {
			dataAres.add(new sortTemp(i, Math.abs(data[i] - dataMedian) / dataVariance));
		}
		//ascending order
		Collections.sort(dataAres, new tempComparator());
		
		int medianIndex = numObs / 2;
		int left = 0;
		int right = numObs - 1;
		int currentLength = numObs;
		for (int outlierIndex = 1; outlierIndex <= maxOutliers; outlierIndex++) {
			double p = 1.0 - alpha/(2*(numObs - outlierIndex + 1));
			TDistribution td = new TDistribution(numObs - outlierIndex - 1);
			double t = td.density(p);
			double lambdaCritical = t * (numObs - outlierIndex) / Math.sqrt((numObs - outlierIndex - 1 + t*t) * (numObs - outlierIndex + 1));
			if (left >= right) break;
			if (currentLength < 1) break;
			
			// remove the largest
			//if (Math.abs(data[]))
		}
		
		
		
		ArrayList<AnomalyPoint> anoms = new ArrayList<AnomalyPoint>();
		return anoms;
	}
};
