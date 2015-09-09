#!/usr/bin/perl
use strict;
use warnings;

unless ($ARGV[4]) {
	die "USAGE: domainXplorer.pl [DIRECTORY WITH THE AUXILIARY SCRIPTS] [NAME OUTPUT FILES] [FILE WITH MUTATIONS] [FILE WITH SCORES] [FILE WITH FEATURES] [-optional:FLAG TO CORRECT BY BATCH (0 = CORRECT | 1 = DO NOT CORRECT)]\n";
}

my ($dir_scripts, $analysis_name, $file_mutations, $file_scores, $file_features, $flag_batch) = @ARGV;

unless ($flag_batch) {
	$flag_batch = 0;
}

#THIS PREPARES THE FILES

my $path_prepare = $dir_scripts."/prepare_input_files_domainXplorer.pl";

unless (-e $path_prepare) {
	die "CAN'T FIND THE SCRIPT \"prepare_input_files_domainXplorer.pl\" IN \"$dir_scripts\"\nMAYBE THIS FOLDER DOES NOT CONTAIN THE SCRIPTS?\n";
}

system ("perl", $path_prepare, $analysis_name, $file_mutations, $file_scores, $file_features, $flag_batch);
if ($?) {
	die "THERE WAS AN ERROR WHEN TRYING TO RUN \"$path_prepare\"\n";
}

#CHECK THAT THE FILES HAVE BEEN GENERATED

my $input_analysis_d = $analysis_name."_matrix_domain.txt";
my $input_analysis_p = $analysis_name."_matrix_protein.txt";

unless (-e $input_analysis_d) {
	die "SOMETHING WRONG HAPPENNED, domainXplorer CANNOT FIND THE DOMAIN MATRIX \"$input_analysis_d\"\n";
}

unless (-e $input_analysis_p) {
	die "SOMETHING WRONG HAPPENNED, domainXplorer CANNOT FIND THE PROTEIN MATRIX \"$input_analysis_p\"\n";
}

#ONCE THE FILES ARE READY WE RUN THE CORRELATION ANALYSIS

my $path_rscript_correlation = $dir_scripts."/run_correlations.r";

unless (-e $path_rscript_correlation) {
	die "CAN'T FIND THE SCRIPT \"path_rscript_correlation.pl\" IN \"$dir_scripts\"\nMAYBE THIS FOLDER DOES NOT CONTAIN THE SCRIPTS?\n";
}

my $output_analysis_d = $analysis_name."_output_domain.txt";
run_analysis ($path_rscript_correlation, $input_analysis_d, $output_analysis_d, $flag_batch, "D");

my $output_analysis_p = $analysis_name."_output_protein.txt";
run_analysis ($path_rscript_correlation, $input_analysis_p, $output_analysis_p, $flag_batch, "P");

#CHECK THE FILES

unless (-e $output_analysis_d) {
	die "SOMETHING WRONG HAPPENNED, domainXplorer CANNOT FIND THE RESULTS FILE \"$output_analysis_d\"\n";
}

unless (-e $output_analysis_p) {
	die "SOMETHING WRONG HAPPENNED, domainXplorer CANNOT FIND THE RESULTS FILE \"$output_analysis_p\"\n";
}
	

############
sub run_analysis {
	
	my ($script, $input, $output, $flag, $mode) = @_;
		
	system ("Rscript", $script, $input, $output, $flag, $mode);
	if ($?) {
		die "THERE WAS A PROBLEM RUNNING THE Rscript \"$path_rscript_correlation\" WITH THE FILE \"$input\"";
	}
}







