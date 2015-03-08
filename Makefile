SCALAC=scalac


MCFA.class: MCFA.scala
	$(SCALAC) -deprecation MCFA.scala


clean:
	rm -f -v *.class *~


