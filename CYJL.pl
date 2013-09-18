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

use charnames ':full';
use warnings;
use diagnostics;
use autodie;
use Carp qw(carp croak);
binmode STDOUT,':utf8';

our (%py2ch, %ch2py, @chengyu);

sub buildPY2Chr{
   return if %py2ch;	# init, call once
   open my $fd, '<:utf8', 'ChinesePinyin.txt'
   	or croak 'Cannot open ChinesePinyin.txt';
   while(<$fd>){
	next if $_=~qr/^\s.*$/;
	my @spt=split ' ',$_;
	my ($py,@hz)=($spt[0],
	   split '', join '', @spt[1..scalar @spt-1]);
	@{$py2ch{$py}}=@hz;
	push @{$ch2py{$_}},$py for @hz;
   }
   close $fd;
}

sub buildChYu{
   return if @chengyu;
   open my $fd,'<:utf8', 'ChineseChengyu.txt'
   	or croak 'Cannot open ChineseChengyu.txt';
   while(<$fd>){
	$_=~s/\s.*//;
	push @chengyu, $_;
   }
   close $fd;
}

sub rmChengyuEntry{
   my ($word,$start)=shift;
   foreach($start//0 .. @chengyu){
   	my $cur=\$chengyu[$_];
   	if(defined $cur and !($cur cmp $word)){
   	   undef $cur; last;
	}
   }
   $word;
}

sub nextChengyu{
   my $word=shift;
   # get characters with same pronounciation with last character of $word
   my $lastCh=substr $word,-2,1;
   # for Duo Yin Zi (multiple pronounciations for one character),
   # choose one pronounciation at random
   my $prons=\@{$ch2py{$lastCh}};
   my $pron=$$prons[int rand scalar @$prons];
   croak "$lastCh not found in dictionary??" if !defined $pron;
   # Chengyu are partially alphabetically ordered: 
   # characters with same pronounciation are grouped together
   my ($start,$counter)=(0,0); my @collection;
   foreach(@chengyu){
   	next if !defined $_;
   	my $candy=\@{$ch2py{+substr $_,0,1}};
   	my $cur=$$candy[int rand scalar @$candy];
	next if !defined $cur or $start==0 && ($cur cmp $pron);
	$start=$counter if $start==0;
	++$counter;
	push @collection,$_;
	last if !defined $cur or ($cur cmp $pron);
   }
   rmChengyuEntry $collection[int rand scalar @collection],$start;
}

sub chkChengyuEntries{
   my %unknowns;
   foreach(@chengyu){
   	my ($fst,$last)=(substr($_,0,1),substr($_,-2,1));
   	$unknowns{$fst}=1 unless defined $ch2py{$fst};
   	$unknowns{$last}=1 unless defined $ch2py{$last};
   }
   print for keys %unknowns;
}

sub main{
   buildPY2Chr; buildChYu;
   print rmChengyuEntry my $cur=$chengyu[int rand scalar @chengyu];
   print $cur=nextChengyu $cur for 1..$ARGV[0] ;
}

exit main() ? 0 : 1;
