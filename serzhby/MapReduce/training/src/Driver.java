import java.io.File;
import java.io.IOException;
import java.util.List;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
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
	}

	private static void startFollowersJob(String[] args)
			throws IllegalArgumentException, IOException,
			ClassNotFoundException, InterruptedException {
		
		if (args.length != 2) {
			System.out.println("Input must have exactly 2 arguments: input file and output path");
			System.exit(1);
		}
		
		Configuration conf = new Configuration();
		Job job = Job.getInstance(conf, "Links");
		job.setJarByClass(Driver.class);
		job.setMapperClass(FollowersMapper.class);
		// job.setCombinerClass(MyReducer.class);
		job.setReducerClass(FollowersReducer.class);
		// job.setInputFormatClass(FileIn.class);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(IntWritable.class);

		job.setInputFormatClass(TextInputFormat.class);
		job.setOutputFormatClass(TextOutputFormat.class);

		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job,
				new Path(args[1] + File.separator + System.currentTimeMillis()));
		job.waitForCompletion(true);
	}

	public static class FollowersMapper extends
			Mapper<LongWritable, Text, Text, IntWritable> {

		@Override
		protected void map(LongWritable key, Text value, Context context)
				throws IOException, InterruptedException {
			ObjectMapper mapper = new ObjectMapper();
			TypeFactory typeFactory = mapper.getTypeFactory();
			List<String> strings = mapper.readValue(value.toString(),
					typeFactory.constructCollectionType(List.class,
							String.class));
			String left = strings.get(0);
			String right = strings.get(1);
			boolean needSwap = left.compareTo(right) > 0;
			context.write(needSwap ? new Text(left + " " + right) : new Text(
					right + " " + left), new IntWritable(needSwap ? 1 : -1));
		}

	}

	private static class FollowersReducer extends
			Reducer<Text, IntWritable, Text, Text> {

		@Override
		protected void reduce(Text key, Iterable<IntWritable> values,
				Context context) throws IOException, InterruptedException {

			int result = 0;
			for (IntWritable bw : values) {
				result += bw.get();
			}
			if (result != 0) {
				context.write(key, new Text(result + ""));
			}
		}

	}
}

