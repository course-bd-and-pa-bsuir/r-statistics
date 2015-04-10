package by.serzh.spark;

import org.apache.spark.SparkConf;
import org.apache.spark.SparkContext;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.sql.DataFrame;
import org.apache.spark.sql.SQLContext;
import org.apache.spark.sql.hive.HiveContext;

public class SparkSqlFollowers {

    public static void main(String[] args) {
        if(args.length < 1) {
            System.out.println("Input file argument is missing");
            System.exit(1);
        }
        SparkConf conf = new SparkConf().setAppName("Followers").setMaster("local[4]");
        JavaSparkContext sc = new JavaSparkContext(conf);
        //SparkContext sc = new SparkContext(conf);
        //HiveContext sqlContext = new HiveContext(sc);
        SQLContext sqlContext = new SQLContext(sc);
        sqlContext.sql("DROP TABLE IF EXISTS links");
        sqlContext.sql("DROP TABLE IF EXISTS parsed");
        sqlContext.sql("DROP TABLE IF EXISTS result");
        sqlContext.sql("CREATE TABLE links (link STRING)");
        sqlContext.sql("LOAD DATA INPATH '" + args[0] + "' OVERWRITE INTO TABLE links");
        sqlContext.sql("CREATE TABLE parsed AS SELECT regexp_extract(link, '\\\\[\"(.*?)\", \"(.*?)\"\\\\]', 1) AS left, regexp_extract(link, '\\\\[\"(.*?)\", \"(.*?)\"\\\\]', 2) AS right FROM links");
        sqlContext.sql("CREATE TABLE result AS SELECT * FROM parsed a WHERE NOT EXISTS (SELECT * FROM parsed b WHERE a.left = b.right AND a.right = b.left)");
        sqlContext.sql("SELECT * FROM result");
//        sqlContext.sql("CREATE TABLE links (link STRING);");
//        sqlContext.sql("CREATE TABLE links (link STRING);");
    }

}
