#!/bin/sh

java -Xmx1024M -Xss256M -XX:PermSize=256M -XX:MaxPermSize=512M -jar sbt-launch.jar compile copy-resources mkrun
