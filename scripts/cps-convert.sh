#!/bin/bash

cat templates/cps.header.scm 
scala -cp . CPSConverterTests $1
cat templates/cps.footer.scm 

