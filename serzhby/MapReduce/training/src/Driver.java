import java.io.IOException;
import java.util.List;
import java.util.StringTokenizer;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.BooleanWritable;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.type.TypeFactory;

public class Driver {

  public static void main(String[] args) throws Exception {
    startFollowersJob(args);
    //startTextJob(args);
  }
  
  private static void startFollowersJob(String[] args) throws IllegalArgumentException, IOException, ClassNotFoundException, InterruptedException {
	  Configuration conf = new Configuration();
	    Job job = Job.getInstance(conf, "Links");
	    job.setJarByClass(Driver.class);
	    job.setMapperClass(FollowersMapper.class);
	    //job.setCombinerClass(MyReducer.class);
	    job.setReducerClass(FollowersReducer.class);
	    //job.setInputFormatClass(FileIn.class);
	    job.setOutputKeyClass(Text.class);
	    job.setOutputValueClass(IntWritable.class);
	    
	    job.setInputFormatClass(TextInputFormat.class);
	    job.setOutputFormatClass(TextOutputFormat.class);
	    
	    FileInputFormat.addInputPath(job, new Path(args[0]));
	    FileOutputFormat.setOutputPath(job, new Path(args[1] + System.currentTimeMillis())); 
	    job.waitForCompletion(true);
  }
  
  private static void startTextJob(String[] args) throws IOException, ClassNotFoundException, InterruptedException {
	  Configuration conf = new Configuration();
	    Job job = Job.getInstance(conf, "Links");
	    job.setJarByClass(Driver.class);
	    job.setMapperClass(TextMapper.class);
	    //job.setCombinerClass(MyReducer.class);
	    job.setReducerClass(TextReducer.class);
	    //job.setInputFormatClass(FileIn.class);
	    job.setOutputKeyClass(Text.class);
	    job.setOutputValueClass(Text.class);
	    
	    job.setInputFormatClass(TextInputFormat.class);
	    job.setOutputFormatClass(TextOutputFormat.class);
	    
	    FileInputFormat.addInputPath(job, new Path(args[2]));
	    FileOutputFormat.setOutputPath(job, new Path(args[3] + System.currentTimeMillis())); 
	    job.waitForCompletion(true);
  }
  
  public static class TextMapper extends Mapper<LongWritable, Text, Text, Text> {

	@Override
	protected void map(LongWritable key, Text value, Context context)
			throws IOException, InterruptedException {
		ObjectMapper mapper = new ObjectMapper();
		TypeFactory typeFactory = mapper.getTypeFactory();
		List<String> strings = mapper.readValue(value.toString(), typeFactory.constructCollectionType(List.class, String.class));
		String doc = strings.get(0);
		String text = strings.get(1);
		StringTokenizer itr = new StringTokenizer(text);
	      while (itr.hasMoreTokens()) {
	    	  Text word = new Text();
	        word.set(itr.nextToken());
	        context.write(word, new Text(doc));
	      }
	}
	  
  }
  
  private static class TextReducer extends Reducer<Text, Text, Text, Text> {

	@Override
	protected void reduce(Text key, Iterable<Text> values, Context context)
			throws IOException, InterruptedException {
		for(Text doc : values) {
			context.write(key, doc);
		}
	}
	  
  }
  
  public static class FollowersMapper extends Mapper<LongWritable, Text, Text, IntWritable> {

	@Override
	protected void map(LongWritable key, Text value, Context context)
			throws IOException, InterruptedException {
		ObjectMapper mapper = new ObjectMapper();
		TypeFactory typeFactory = mapper.getTypeFactory();
		List<String> strings = mapper.readValue(value.toString(), typeFactory.constructCollectionType(List.class, String.class));
		String left = strings.get(0);
		String right = strings.get(1);
		boolean needSwap = left.compareTo(right) > 0;
		context.write(needSwap ? new Text(left + " " + right) : new Text(right + " " + left), 
				new IntWritable(needSwap ? 1 : -1));
	}
	  
  }
  
  private static class FollowersReducer extends Reducer<Text, IntWritable, Text, Text> {

	@Override
	protected void reduce(Text key, Iterable<IntWritable> values, Context context)
			throws IOException, InterruptedException {
		
		int result = 0;
		for(IntWritable bw : values) {
			result += bw.get();
		}
		if(result != 0) {
			context.write(key, new Text(result + ""));
		}
	}
	  
  }
}

