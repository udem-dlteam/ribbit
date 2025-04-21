#!/bin/sh

# Compile Kotlin code to Java bytecode
kotlinc $1 -include-runtime -d $1.jar

# Run the compiled program using Java
java -jar $1.jar

