#!/usr/bin/env perl

use warnings;
use strict;

print "Motivating example.\n";
print `Rscript chernozhukov2021-motivating-example.R`;
print "Chernozkhukov (2021) figure 1.\n";
print `Rscript chernozhukov2021-fig1.R`;
print "Lei (2018) figure 1.\n";
print `Rscript lei2018-fig3.R`;
print "Simulation analysis.\n";
print `Rscript simulation-analysis.R`;
print "Rain analysis.\n";
print `Rscript rain-analysis.R`
