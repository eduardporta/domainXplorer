use strict;
use warnings;

unless ($ARGV[3]) {
	die "SOMETHING STRANGE HAPPENNED WITH \"prepare_input_files_edgeXplorer.pl\"\n";
}

my ($analysis_name, $file_mutations, $file_scores, $file_features, $flag_batch) = @ARGV;

#THIS PART PARSES THE FILE WITH THE SCORES

my %scores;
my %samples;

open IN, $file_scores or die "COULD NOT FIND THE FILE WITH THE SCORES \"$file_scores\"\n";

while (<IN>) {
	
	chomp;
	
	my ($sample, $batch, $score) = split ("\t", $_);

	$samples{$sample} = $batch;
	$scores{$sample} = $score;
}

close IN or die "COULD NOT CLOSE FILEHANDLER TO \"$file_scores\"\n";

unless (scalar (keys %scores) > 0) {
	die "COULD NOT FIND ANY SCORE IN THE FILE \"$file_scores\"\nPERHAPS THE FILE IS NOT IN THE RIGHT FORMAT?\n";
}

unless (scalar (keys %samples) > 0) {
	die "COULD NOT FIND ANY SAMPLE IN THE FILE \"$file_scores\"\nPERHAPS THE FILE IS NOT IN THE RIGHT FORMAT?\n";
}

#THIS PART PARSES THE FILE WITH THE FEATURES

my %features;

open IN, $file_features or die "COULD NOT FIND THE FILE WITH THE FEATURES \"$file_features\"\n";

while (<IN>) {
	
	chomp;
	my @array = split ("\t", $_);
	my $protein_symbol = shift @array;
	
	foreach my $region (@array) {
		$features{$protein_symbol}{$region} = 1;
	}
}

close IN or die "COULD NOT CLOSE FILEHANDLER TO \"$file_features\"\n";

#THIS PART PARSES THE FILE WITH THE MUTATIONS

my %mutations;
my %samples_with_mutations;

open IN, $file_mutations or die "COULD NOT FIND THE FILE WITH THE MUTATIONS \"$file_mutations\"\n";

while (<IN>) {
	
	chomp;
	my ($sample, $protein_symbol, $position) = split ("\t", $_);
	
	unless (exists $samples{$sample}) {
		next;
	}
	
	$samples_with_mutations{$sample} = 1;

	$mutations{"protein"}{$protein_symbol}{$sample} = 1;
	
	my $ref_feat = $features{$protein_symbol};
	
	while (my ($region, $x) = each %$ref_feat) {

		my ($name, $start, $end) = split ("-", $region);
		
		if ($start <= $position && $end >= $position) {
			$mutations{"domain"}{$protein_symbol."-".$region}{$sample} = 1;
		}
	}
}

close IN or die "COULD NOT CLOSE FILEHANDLER TO \"$file_mutations\"\n";

unless (scalar (keys %mutations) > 0) {
	die "COULD NOT FIND ANY MUTATION IN THE FILE \"$file_mutations\"\nPERHAPS THE FILES ARE NOT IN THE APPROPRIATE FORMAT?\n";
}

#NOW WE PREPARE THE FILES FOR THE ANALYSIS

my %batch_names;

while (my ($sample, $x) = each %samples) {
	$batch_names{$samples{$sample}} = 1;
}

my @samples = keys %samples_with_mutations;

while (my ($group, $ref_group) = each %mutations) {

	my $filename = $analysis_name."_matrix_".$group.".txt";

	open OUT, ">$filename" or die "COULD NOT OPEN FILEHANDLER TO FILE \"$filename\" TO PREPARE THE ANALYSIS FILES\n";

	foreach my $sample (@samples) {
		print OUT "\t$sample";
	}
	
	print OUT "\n";
	
	print OUT "Score";

	foreach my $sample (@samples) {
		print OUT "\t", $scores{$sample};
	}

	print OUT "\n";
	
	print OUT "Batch";
	
	foreach my $sample (@samples) {
		print OUT "\t$samples{$sample}";
	}
	
	print OUT "\n";

	while (my ($feature, $ref_feat) = each %$ref_group) {
	
		unless (scalar (keys %$ref_feat) > 2) {
			next;
		}
	
		print OUT $feature;
	
		foreach my $sample (@samples) {

			if 	(exists ${$ref_feat}{$sample}) {
				print OUT "\tFeat";
			}
			
			elsif ($group eq 'domain') {
				
				my ($ensp, $domain, $start, $end) = split ("-", $feature);
				
				if (exists $mutations{'protein'}{$ensp}{$sample}) {
					print OUT "\tRest";
				}
				else {
					print OUT "\tWT";
				}
			}
		
			else {
				print OUT "\tWT";
			}
		}
	
		print OUT "\n";
	}
	
	close OUT or die;
}
