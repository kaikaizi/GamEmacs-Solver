#!/usr/bin/perl -w

# Copyright (c) 2012, Liu Lukai (liulukai@gmail.com)
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or
# without modification, are permitted provided that the
# following conditions are met: 
# 
# 1. Redistributions of source code must retain the above
# copyright notice, this list of conditions and the following
# disclaimer.
# 2. Redistributions in binary form must reproduce the above
# copyright notice, this list of conditions and the following
# disclaimer in the documentation and/or other materials
# provided with the distribution. 
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# The views and conclusions contained in the software and
# documentation are those of the authors and should not be
# interpreted as representing official policies, either
# expressed or implied, of the FreeBSD Project.

use Modern::Perl "2013";
use English '-no_match_vars';
use feature ":5.10";
use charnames ':full';
use warnings;
use diagnostics;
use autodie;
use Carp;
binmode STDOUT,':utf8';
binmode STDIN,':utf8';

my %py2ch;	# Pinyin to Hanzi, each value is an array
my %ch2py;	# Hanzi to Pinyin
my @chengyu;
sub buildPY2Chr{	# init, call once
   open my $fd, '<:utf8', 'ChinesePinyin.txt' or croak 'Cannot open ChinesePinyin.txt';
   my $pat='(?<py>[a-zA-Z]*)(?<chs>.*)';
   while(<$fd>){
	next if $_=~qr/^\s.*$/;
	$_=~s/\s//g; $_=~qr/$pat/;
	my @chs;
	foreach(0 .. length($+{chs})-1){
	   my $cur=substr $+{chs},$_,1;
	   push @chs, $cur;
	   $ch2py{+$cur}=$+{py};
	}
	@{$py2ch{$+{py}}}=@chs;
   }
   close $fd;
}

sub buildChYu{
   open my $fd,'<:utf8', 'ChineseChengyu.txt' or croak 'Cannot open ChineseChengyu.txt';
   while(<$fd>){
   	$_=~s/\s.*//;
   	push @chengyu, $_;
   }
}

sub indexChrs{
   my $chr=shift;
   my @chrs;
   printf "::%x(%s)\n", ord $chr, $chr;
   while(my ($k,$v)=each %py2ch){
	foreach(@{$v}){
	   if(ord $_ == ord $chr){
		say 'found '.$_.'=='.$chr.': '.$k;
		foreach(@{$v}){push @chrs,$_;}
		last;
	   }
	}
   }
   return @chrs;
}

sub nextChengyu{
   my $word=shift;
   # get characters with same pronounciation with last character of $word
   my $lastCh=substr $word,-2,1;
   my $pron=$ch2py{$lastCh};
   croak "$lastCh not found in dictionary??" if !defined $pron;
   # Chengyu are partially alphabetically ordered: 
   # characters with same pronounciation are grouped together
   my $start=0; my @collection;
   my $counter=0;
   foreach(@chengyu){
   	next if !defined $_;
   	my $cur=$ch2py{+substr $_,0,1};
	next if !defined $cur or $start==0 && ($cur cmp $pron);
	$start=$counter if $start==0;
	++$counter;
	push @collection,$_;
	last if !defined $cur or ($cur cmp $pron);
   }
   my $index=int rand scalar @collection;
   my $res=$collection[$index];
   $counter=0;
   foreach(@chengyu){
   	next if ++$counter<$start;
   	if(defined $chengyu[$counter] and defined $res and !($chengyu[$counter] cmp $res)){
	   undef $chengyu[$counter]; 
	   last;
	}
   }
   return $res;
}

sub prtTable{
   my $counter=0;
   local $,='*';
   while(my ($k,$v)=each %py2ch and ++$counter<10){
   	print $k.': ';
   	foreach(@{$v}){printf "%x(%s),", ord $_, $_;}
   	print "\n";
   }
   $counter=0;
   while(my ($k,$v)=each %ch2py and ++$counter<10){
   	say $k.'->'.$v;
   }
}

sub main{
   buildPY2Chr;
   buildChYu;
   my $cur=$chengyu[int rand scalar @chengyu];
   print $cur;
   for(1 .. $ARGV[0]){
	$cur=nextChengyu $cur;
	print $cur;
   }
}

exit main() ? 0 : 1;
