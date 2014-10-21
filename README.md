Wolf: A Scalable PSL Inferencing Framework [![Build Status](https://travis-ci.org/uzh/triplerush.svg?branch=master)](https://travis-ci.org/uzh/wolf/branches)
=====================================================

How to Compile the Project
--------------------------
Ensure Java 8 is available on the system, verify with `java -version` on the command line.

Ensure that the https://github.com/uzh/signal-collect project is placed in the same root folder as this project. 

Install SBT: http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html

Go to the project folder and start SBT on the command line.

To generate an Eclipse project, use the `eclipse` command on the SBT prompt.

To generate a JAR file, use the `assembly` command on the SBT prompt.


How to try out an example
-------------------------

Run the `assembly` command according to the instructions above, then execute for example `./wolf.sh examples/movies.psl` to run the inferencer on the `movies.psl` file.

If there is no shell available, you can also run `java -Xmx5000m -cp ./target/scala-2.11/wolf-assembly-1.0-SNAPSHOT.jar com.signalcollect.psl.CommandLinePslInferencer examples/movies.psl`.


