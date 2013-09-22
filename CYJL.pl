#!/usr/bin/perl -w
# Copyright (c) 2013, Liu Lukai (liulukai@gmail.com)
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

use List::MoreUtils qw(uniq any first_index);
use Scalar::Util 'dualvar';
use charnames ':full';
use Modern::Perl;
use diagnostics;
use autodie;
use Carp qw(carp croak);
use utf8;
binmode STDOUT,':utf8';

sub main; exit !main;

our (%py2ch, %ch2py, @chengyu, @chengyu1stPY, %chengyu);

sub buildPY2Chr{
   return if %py2ch;	# init, call once
   open my $fd, '<:utf8', 'ChinesePinyin.txt'
   	or croak 'Cannot open ChinesePinyin.txt';
   while(<$fd>){
	next if $_=~qr/^\s.*$/;
	my @spt=split ' ',$_;
	my ($py,@hz)=($spt[0], split '', join '', @spt[1..@spt-1]);
	@{$py2ch{$py}}=@hz;
	push @{$ch2py{$_}},$py for @hz;
   }
   close $fd;
   while(my ($k,$v)=each %ch2py){
   	$v=uniq $v;
   }
   while(my ($k,$v)=each %py2ch){
   	$v=uniq $v;
   }
}

sub transPyNum{
   state %PyNum;
   my ($py,$num)=(shift,0);
   return $PyNum{$py} if defined $PyNum{$py};
   $num=$num*26+(ord substr $py,$_,1)-ord('a') for 0..length($py)-1;
   $num*=26 for length $py .. 6;
   $PyNum{$py}=$num;
}
sub buildChYu;
sub annotateChengyuPinyin{ # annotate Pinyin of first character of all @chengyus
   return if @chengyu1stPY;
   buildPY2Chr; buildChYu;
   my ($counter,$cur,$ord)=(0,0,0);  # experimental sub
   foreach(@chengyu){
   	my @pys=sort {transPyNum($a)<=>transPyNum($b)} @{$ch2py{substr $_,0,1}};
   	croak "ch2py map empty for $_" unless @pys;
   	if(!@chengyu1stPY){
   	   push @chengyu1stPY,dualvar $counter,$pys[0];
   	   $ord=transPyNum $chengyu1stPY[0].'';
	}else{
	   if(1==@pys){
		push @chengyu1stPY,dualvar $counter,$pys[0]
		   if($chengyu1stPY[-1].'' cmp $pys[0]);
		next;
	   }
	   my $index=first_index {transPyNum($_)>=$ord} @pys;
	   my ($a,$b)=(transPyNum($chengyu1stPY[-1].''), transPyNum $pys[$index]);
	   if($a<$b){
		push @chengyu1stPY,dualvar $counter,$pys[$index];
		$ord=transPyNum $chengyu1stPY[-1].'';
	   }
	}
   }continue{++$counter;}
   push @chengyu1stPY,dualvar $counter-1,transPyNum 'zz';  # sentinal
#    printf("%d:%s\n",$_+1,$_.'')for@chengyu1stPY;
}

sub buildChYu{
   return if @chengyu && !%py2ch;
   open my $fd,'<:utf8', 'ChineseChengyu.txt'
   	or croak 'Cannot open ChineseChengyu.txt';
   my $counter=0;
   while(<$fd>){
	$_=~s/\s.*\n//;
	next if /$^/;
	push @chengyu, ::Scalar::Util::dualvar $counter++,$_;
	# and hash of chengyu by leading character, ref-valued.
	push @{$chengyu{substr $_,0,1}}, \$chengyu[-1];
   }
   close $fd;
}

sub rmChengyuEntry{  # from array
   my $word=shift;
   undef $chengyu[$word+0];
   $word;
}

sub nextChengyu{   # get characters with same pronounciation with last character of $word
   my $word=shift;
   my $lastCh=substr $word,-1;
   my ($prons,$candy,$counter,$pron)=(\@{$ch2py{$lastCh}},
   	\@{$chengyu{$lastCh}}, 0);
   if(defined $candy){	# prioritize Chengyu with same leading character
	if(any {defined $$_} @$candy){
	   $pron=$$prons[rand @$prons];
	   my ($index, $word);
	   do{	   # pick an available chengyu
		$index=rand @{$candy};
		$word=$$candy[$index];
	   }until defined $$word;
	   undef $$candy[$index];
	   undef @$candy unless any {defined $$_} @$candy;
	   return (rmChengyuEntry($$word), $pron);
	}
   }
   # pick one pronounciation for last character
   my ($start,$tries,@collection)=(0,0);
   do{
	$pron=$$prons[rand @$prons];
	carp "$lastCh not found in dictionary??" if !defined $pron;
	my $index=first_index {!($pron cmp $_.'')} @chengyu1stPY;
	for($chengyu1stPY[$index]+0..$chengyu1stPY[$index+1]+0){
	   push @collection, $chengyu[$_] if defined $chengyu[$_];
	}
	++$tries;
   }until @collection or $tries>@$prons;
   return (@collection?rmChengyuEntry$collection[rand @collection]:undef, $pron);
}

sub main{
   annotateChengyuPinyin;
   print rmChengyuEntry(my $cur=$chengyu[rand @chengyu])."\n";
   my ($cnt,$prev,$prev2,$counter,$pron)=2;
   until($cnt>$ARGV[0]){
	$prev2=$prev; $prev=$cur;
	($cur,$pron)=nextChengyu $cur;
	printf "[%-3d]%-6s ",$cnt++,$pron;
	print defined($cur) ? "$cur\n" : "...继续无力\n";
	next if defined $cur;
	$counter=0; # 1-step backtracking.
	do{
	   ($cur,$pron)=nextChengyu $prev2;
	}until $cur // ++$counter>5;
	last unless defined $cur;
	$cnt-=2;
	printf "[%-3d]%-6s %s\n",$cnt,$pron,$cur;
   }
   defined $cur;
}
