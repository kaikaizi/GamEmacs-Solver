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

use List::MoreUtils qw(uniq any first_index last_index);
use Scalar::Util qw(dualvar looks_like_number);
use charnames ':full';
use Modern::Perl;
use diagnostics;
use autodie;
use Carp qw(carp croak);
use utf8;
binmode STDOUT,':utf8';

sub main; exit !main;

our (%py2ch, %ch2py, %chengyu, @chengyu, @chengyu1stPY);

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
   while(my ($k,$v)=each %ch2py){$v=uniq $v}
   while(my ($k,$v)=each %py2ch){$v=uniq $v}
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
   buildPY2Chr unless %py2ch; buildChYu unless @chengyu;
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
   push @chengyu1stPY,dualvar $counter-1,'zz';  # sentinal
}

sub buildChYu{
   return if @chengyu && !%py2ch;
   open my $fd,'<:utf8', 'ChineseChengyu.txt'
   	or croak 'Cannot open ChineseChengyu.txt';
   my $counter=0;
   while(<$fd>){
	$_=~s/\s.*\n//;
	next if /$^/;
	push @chengyu, dualvar $counter++,$_;
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
   my ($candy,$counter,$pron)=(\@{$chengyu{$lastCh}}, 0);
   if(defined $candy){	# prioritize Chengyu with same leading character
	if(any {defined $$_} @$candy){
	   my ($index, $word);
	   do{	   # pick an available chengyu
		$index=rand @{$candy};
		$word=$$candy[$index];
	   }until defined $$word;
	   $pron=$chengyu1stPY[last_index {$$word+0>=$_+0} @chengyu1stPY].'';
	   undef $$candy[$index];
	   undef @$candy unless any {defined $$_} @$candy;
	   return (rmChengyuEntry($$word), $pron);
	}
   }
   my ($prons,$tries,$index,@collection)=(\@{$ch2py{$lastCh}},0);
   do{   # pick one pronounciation for last character
	$pron=$$prons[rand @$prons];
	carp "$lastCh not found in dictionary??" if !defined $pron;
	$index=first_index {!($pron cmp $_.'')} @chengyu1stPY;
	for(($chengyu1stPY[$index]+0)..($chengyu1stPY[$index+1]+0)){
	   push @collection, $chengyu[$_] if defined $chengyu[$_];
	}
	++$tries;
   }until @collection or $tries>@$prons;
   (@collection?rmChengyuEntry$collection[rand @collection]:undef, $pron);
}

sub main{
   annotateChengyuPinyin;
   use Encode 'decode';
   my ($cnt,$prev,$prev2,$counter,$pron,$cur,%history)=2;
   if(@ARGV>1){
	$cur=decode 'utf8',$ARGV[1];
	$counter=first_index{!($cur cmp $_)} @chengyu;
	defined $counter?
	   $cur=dualvar($counter,$cur) : undef $cur;
   }
   $cur//=$chengyu[rand @chengyu];
   printf "[1  ]%-6s %s\n",'',rmChengyuEntry($cur);
   $history{1}=$cur;
   until($cnt>$ARGV[0]){
	$prev2=$prev; $prev=$cur;
	($cur,$pron)=nextChengyu $cur;
	printf "[%-3d]%-6s ",$cnt++,$pron;
	print defined($cur) ? "$cur\n" : "...继续无力\n";
	if(defined $cur){
	   $history{$cnt-1}=$cur; next;
	}
	$counter=0; # 1-step backtracking.
	do{
	   ($cur,$pron)=nextChengyu $prev2;
	}until $cur // ++$counter>5;
	last unless defined $cur;
	$cnt-=2;
	printf "[%-3d]%-6s %s\n",$cnt,$pron,$history{$cnt-1}=$cur;
   }
   open my $fd,'<:utf8', 'ChineseChengyu.txt'	# definition lookup
	or croak 'Cannot open ChineseChengyu.txt';
   print "词义解释：\n\$";
   while(<STDIN>){
	my $option=$_;
	print '?' unless(looks_like_number $option);
	next unless defined $history{0+$option};
	seek $fd,0,0; $cnt=0;
	while($cnt<$history{0+$option}){
	   next if <$fd>=~/^\s*$/;
	   ++$cnt;
	}
	print $_=<$fd>;
	print '$';
   }
   close $fd;
   defined $cur;
}
