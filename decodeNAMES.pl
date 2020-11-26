#!/usr/bin/perl

# (c) Dominic Beesley 2020
# https://github.com/dominicbeesley/CommunicatorBasic100

# Decode the names tables from the NAMES module

use strict;

# name of input file
my $fnIN = "NAMES100.bin";

# offset in binary of the first letter lookup table
my $namesOffs = 0x016d;

open(my $fh_in, "<:raw:", $fnIN) or die "Cannot open $fnIN $!";
seek($fh_in, $namesOffs, 0) or die "Error seeking $!";

#keep track of offset
my $curoffs = 0;
my @all;

#read 1st letter offsets
my @firstLetterOffs;
for (my $i=0; $i<26; $i++) {
	read($fh_in, my $bb, 2) == 2 or die "Unexpected end of file";
	@firstLetterOffs[$i] = unpack("v", $bb);
	$curoffs+=2;
}

sub header {
	my ($fh) = @_;
	print $fh "; (c) Dominic Beesley 2020\n";
	print $fh "; https://github.com/dominicbeesley/CommunicatorBasic100\n";
	print $fh "; This file generated by decodeNAMES.pl from the Acorn Communicator rom\n\n";
}


open(my $fh_o_alpha, ">", "names_alpha.txt") or die "Cannot open names_alpha.txt $!";
header $fh_o_alpha;
open(my $fh_o_num, ">", "names_num.txt") or die "Cannot open names_num.txt $!";
header $fh_o_num;
open(my $fh_o_inc, ">", "names.inc") or die "Cannot open names_num.txt $!";
header $fh_o_inc;

#scan rest of file, assuming that it is in first letter order
for (my $i=0; $i<26; $i++) {
	my $firstLetter = chr(ord('A') + $i);

	print $fh_o_alpha "\n\n############################ $firstLetter ######################\n";

	my $offs = @firstLetterOffs[$i];
	if ($offs) {
		$offs == $curoffs or die "Lost Sync at offset $curoffs <> $offs";

		my $ok = 1;
		while ($ok) {
			read($fh_in, my $bb_flags, 1) or die "Unexpected end of file at $curoffs";
			$curoffs++;
			if (ord($bb_flags) == 0) {
				$ok = 0;
				last;
			}
			my $curlen = ord($bb_flags) & 0x0F;
			my $curcodeorsz = (ord($bb_flags) & 0xF0) >> 4;
			my $curname = $firstLetter;
			my $addr = 0;
			my $bb_namech;
			do {
				read($fh_in, $bb_namech, 1) or die "Unexpected end of file at $curoffs";
				$curoffs++;
				$curname = $curname . chr(ord($bb_namech) & 0x7F);
			} while (ord($bb_namech) < 0x80);

			$curname =~ s/\s+$//;

			print $fh_o_alpha "\@$curname%=";
			if ($curcodeorsz >= 4) {
				printf $fh_o_alpha "&%01.01X\n", $curcodeorsz-4;
				$addr = $curcodeorsz-4;
			} else {
				print $fh_o_alpha "&";
				for (my $j=0; $j < $curcodeorsz+1; $j++) {
					read($fh_in, my $n, 1) or die "Unexpected end of file at $curoffs";
					$curoffs++;
					printf $fh_o_alpha "%02.02X", ord($n);
					$addr = ($addr << 8) + ord($n);
				}
				print $fh_o_alpha "\n";
			}

			push @all, {name=>$curname, addr=> $addr};
		}
	}	
}

foreach my $h (sort { $a->{addr} <=> $b->{addr} } @all) {
	printf $fh_o_num "&%X=%s\n", $h->{addr}, $h->{name};
}

foreach my $h (sort { $a->{addr} <=> $b->{addr} } @all) {
	if ($h->{name} =~ /^OP/) {
		printf $fh_o_inc "COP_%02.02X_%s\t= \$%02.02X\n", $h->{addr}, $h->{name}, $h->{addr};
	}
}

foreach my $h (sort { $a->{name} cmp $b->{name} } @all) {
	if (! ($h->{name} =~ /^OP/) ) {
		if ($h->{addr} >= 0xFF00) {
			printf $fh_o_inc "_%s\t= \$%06.06X\n", $h->{name}, $h->{addr};
		}
	}
}

foreach my $h (sort { $a->{name} cmp $b->{name} } @all) {
	if (! ($h->{name} =~ /^OP/) ) {
		if ($h->{addr} < 0xFF00) {
			printf $fh_o_inc "_%s\t= \$%02.02X\n", $h->{name}, $h->{addr};
		}
	}
}



