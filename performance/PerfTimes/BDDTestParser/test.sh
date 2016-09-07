echo "Schritt1 Lexer und Parser generieren"
ant

echo "Schritt2 kompillieren"
javac -cp ./src/:./lib/java-cup-11a.jar:./lib/javabdd-1.0b2.jar ./src/main/Main.java

echo "Schritt3 ausführen"
java -cp ./src/:./lib/java-cup-11a.jar:./lib/javabdd-1.0b2.jar main.Main $1
