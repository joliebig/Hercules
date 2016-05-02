#!/bin/sh

cd "$(dirname "$0")"

java -Xmx768M -Xss10M -jar sbt-launch.jar compile copy-resources mkrun
