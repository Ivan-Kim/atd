#!/bin/sh

CLASSPATH='.:kotlinx-serialization-core-jvm-1.7.0-RC.jar:kotlinx-serialization-json-jvm-1.7.0-RC.jar:junit-4.8.2.jar'

kotlinc -Xplugin="kotlin-serialization-compiler-plugin.jar" -classpath $CLASSPATH everything.kt
kotlinc -classpath $CLASSPATH EverythingTest.kt -d out
kotlin -classpath "out:$CLASSPATH" org.junit.runner.JUnitCore org.example.EverythingTest
