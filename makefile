default: jar

test jar::
	lein $@

push: clean
	lein jar
	ln -s chlorine-*.jar chlorine.jar
	lein push

clean:
	rm -f *.jar
