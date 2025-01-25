#!/bin/bash
#if [ -f *.out ]
#then
#    rm *.out;
#fi
#if [ -f *.log ]
#then
#    rm *.log;
#fi
#if [ -f out/sim_1.out ]
#then
#    rm out/sim*.out;
#fi
#if [ -f res/res_1.rda ]
#then
#    rm res/res*.rda;
#fi

R CMD BATCH --vanilla --no-save --slave m3.R & 

