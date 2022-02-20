#!/bin/bash
# File: frameshell.sh
#
#$ -cwd
#$ -j y
#$ -q all.q
#$ -N rake
#$ -m be
#$ -M p.j.swatton@essex.ac.uk
#$ -pe smp 11
#$ -S /bin/bash
./03a_rake.R
