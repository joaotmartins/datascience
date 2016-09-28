package org.jmartins;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;

public class WordParser {

	class TableEntry {
		long[] ngram;
		long frequency;
		
		public TableEntry(long[] ngram, long frequency) {
			super();
			this.ngram = ngram;
			this.frequency = frequency;
		}
	}
	
	class UniEntry {
		String word;
		long index;
		long frequency;
		
		public UniEntry(String word, long index, long frequency) {
			super();
			this.word = word;
			this.index = index;
			this.frequency = frequency;
		}
	}
	
	HashMap<String, UniEntry> words = null;
	
	
	public static void main(String[] args) {
		String pref1 = "work/tables/ngram";
		String pref2 = "data/p_num_ngram";

		if (args.length != 2) {
			System.out.println("No filename prefixes given, using defaults.");
		} else {
			pref1 = args[0];
			pref2 = args[1];
		}
		
		try {
			
			WordParser wp = new WordParser();
			wp.parseWords(new File(pref1 + "1.txt"), new File(pref2 + "_words.csv"));
			System.out.println("Parsed words.");
			
			long start = System.currentTimeMillis();
			for (int i = 1; i <= 5; i++) {
				File in = new File(pref1 + i + ".txt");
				File out = new File(pref2 + i + ".csv");
				
				long s = System.currentTimeMillis();
				wp.run(in, out);
				long e = System.currentTimeMillis();
				System.out.printf("Completed %d-gram in %.2f seconds\n", i, ((double)(e - s) / 1000));
			}
			long end = System.currentTimeMillis();
			
			System.out.printf("Completed total in %.2f seconds\n", ((double)(end - start) / 1000));
			
		} catch (Exception e) {
			System.out.println(e.getMessage());
			e.printStackTrace(System.out);
			System.exit(2);
		}

	}
	
	void parseWords(File in, File out) throws IOException {
		BufferedReader br = new BufferedReader(new FileReader(in));
		String l = "";
		long ind = 1;
		this.words = new HashMap<String, UniEntry>();
		
		while ((l = br.readLine()) != null) {
			String s[] = l.split(" ");
			String word = s[0].substring(1, s[0].length() - 1);
			long freq = Long.parseUnsignedLong(s[1]);
			
			this.words.put(word, new UniEntry(word,ind,freq));
			ind++;
		}
		
		br.close();
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(out));
		
		for(UniEntry e: this.words.values()) {
			bw.write("\"" + e.word + "\"," + e.index + "," + e.frequency + "\n");
		}
		
		bw.flush();
		bw.close();
	}
	
	
	void run(File in, File out) throws IOException {
		BufferedReader br = new BufferedReader(new FileReader(in));
		
		String l = ""; 
		ArrayList<TableEntry> table = new ArrayList<TableEntry>();
			
		while ((l = br.readLine()) != null) {
			String s[] = l.split(" ");
			String ng = s[0].substring(1, s[0].length() - 1);
			long freq = Long.parseUnsignedLong(s[1]);
			
			String n[] = ng.split("_");
			
			long[] ngram = wordToIndex(n);
			
			table.add(new TableEntry(ngram, freq));
		}
		
		br.close();
		
		System.out.println("Read in " + table.size() + " tokens");
		
		BufferedWriter bw = new BufferedWriter(new FileWriter(out));
		
		for (TableEntry e: table) {
			bw.write(writeNgram(e.ngram) + e.frequency + "\n");
		}
		
		bw.flush();
		bw.close();
	}
	
	long[] wordToIndex(String[] words) {
		long[] ret = new long[words.length];
		
		for(int i = 0; i < words.length; i++) {
			ret[i] = this.words.get(words[i]).index; 
		}
		return ret;
	}
	
	String writeNgram(long[] ng) {
		String ret = "";
		for (int i = 0; i < ng.length; i++) {
			ret = ret.concat(ng[i] + ",");
		}
		
		return ret;
	}
}
