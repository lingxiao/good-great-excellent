#!/bin/bash

set -x


# $HOME/sample_applications/sample_sge_script_1.sh


# wallclock time reservation (format is hours:minutes:seconds).
# man 5 complex
#$ -l h_rt=10:10:0

# request x gigabyte of RAM 
# man 5 complex
#$ -l mem=50G

# name of job
# man 1 qsub
#$ -N main

# working directory (check for specific requirements for your research group)
# man 1 qsub


# make sure I set my $CWD (current working directory)
cd $HOME/xiao/good-great-excellent/shell

# You must set the error and output locations, as the default log output
# location is the directory you qsub'ed from. For instance, say I cd to
# "/home/whaun/blargh" and qsub from there, not specifying "-e" or "-o." The
# output location for the error and output logs is going to default to
# "/home/whaun/blargh." To make it easier you may add -e and -o to your job
# script so you do not have to specify them every qsub call. Ex.:
#$ -o $PWD/logs
#$ -e $PWD/logs


# when am I running
#/bin/date

# where am I running
#/bin/hostname

# what environment variables are available to this job script, e.g. $JOB_ID
#/usr/bin/env
#echo $JOB_ID $SGE_STDOUT_PATH 

# run my scripts
$HOME/xiao/good-great-excellent/.stack-work/install/x86_64-linux/lts-6.11/7.10.3/bin/good-great-excellent-exe

