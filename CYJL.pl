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

my (%拼音2成语, %成语2拼音, %成语, @成语, @成语首字拼音);

sub 字典_拼音2汉字{
   return if %拼音2成语;	# init, call once
   open my $文件句柄, '<:utf8', '汉字拼音.txt'
   	or croak '无法打开“汉字拼音.txt”';
   while(<$文件句柄>){
	next if $_=~qr/^\s.*$/;
	my @spt=split ' ',$_;
	my ($拼音,@hz)=(lc $spt[0], split '', join '', @spt[1..@spt-1]);
	@{$拼音2成语{$拼音}}=@hz;
	push @{$成语2拼音{$_}},$拼音 for @hz;
   }
   close $文件句柄;
   while(my ($k,$v)=each %成语2拼音){$v=uniq $v}
   while(my ($k,$v)=each %拼音2成语){$v=uniq $v}
}

sub 翻译_拼音2汉字{
   state %拼音序列;
   my ($拼音,$序列)=(shift,0);
   return $拼音序列{$拼音} if exists $拼音序列{$拼音};
   $序列=$序列*26+(ord substr $拼音,$_,1)-ord('a') for 0..length($拼音)-1;
   $序列*=26 for length $拼音 .. 6;
   $拼音序列{$拼音}=$序列;
}
sub 构建_成语词库;
sub 标注成语首字拼音{ # annotate Pinyin of first character of all @成语s
   return if @成语首字拼音;
   字典_拼音2汉字 unless %拼音2成语; 构建_成语词库 unless @成语;
   my ($计数器,$cur,$ord)=(0,0,0);  # experimental sub
   foreach(@成语){
   	my @拼音=sort {翻译_拼音2汉字($a)<=>翻译_拼音2汉字($b)} @{$成语2拼音{substr $_,0,1}};
   	croak "成语2拼音 映射为空 for $_" unless @拼音;
   	if(!@成语首字拼音){
   	   push @成语首字拼音,dualvar $计数器,$拼音[0];
   	   $ord=翻译_拼音2汉字 $成语首字拼音[0].'';
	}else{
	   if(1==@拼音){
		push @成语首字拼音,dualvar $计数器,$拼音[0]
		   if($成语首字拼音[-1].'' cmp $拼音[0]);
		next;
	   }
	   my $index=first_index {翻译_拼音2汉字($_)>=$ord} @拼音;
	   my ($a,$b)=(翻译_拼音2汉字($成语首字拼音[-1].''), 翻译_拼音2汉字 $拼音[$index]);
	   if($a<$b){
		push @成语首字拼音,dualvar $计数器,$拼音[$index];
		$ord=翻译_拼音2汉字 $成语首字拼音[-1].'';
	   }
	}
   }continue{++$计数器;}
   push @成语首字拼音,dualvar $计数器-1,'zz';  # sentinal
}

sub 构建_成语词库{
   return if @成语 && !%拼音2成语;
   open my $文件句柄,'<:utf8', '成语词库.txt'
   	or croak '打不开“成语词库.txt”';
   my $计数器=0;
   while(<$文件句柄>){
	$_=~s/\s.*\n//;
	next if /$^/;
	push @成语, dualvar $计数器++,$_;
	# and hash of 成语 by leading character, ref-valued.
	push @{$成语{substr $_,0,1}}, \$成语[-1];
   }
   close $文件句柄;
}

sub 删除该成语{  # undef a 该成语 from array by
   my $word=shift;   # negating its numerical value.
   $成语[$word]=dualvar -$成语[$word],$成语[$word] unless $成语[$word]<0;
   $word;
}

sub 下一个_成语{   # get characters with same pronounciation with last character of $word
   my $该成语=shift;
   my $上一成语=substr $该成语,-1;
   my ($candy,$计数器,$发音)=(\@{$成语{$上一成语}}, 0);
   if(defined $candy){	# prioritize Chengyu with same leading character
	if(any {$$_>=0} @$candy){
	   my ($检索, $该成语);
	   do{	   # pick an available 成语
		$检索=rand @{$candy};
		$该成语=$$candy[$检索];
	   }until $$该成语>=0;
	   $发音=$成语首字拼音[last_index {$$该成语>=$_} @成语首字拼音];
	   undef $candy unless any {$$_>=0} @$candy;
	   return (删除该成语($$该成语), $发音);
	}
   }
   my ($多音,$tries,$index,@collection)=(\@{$成语2拼音{$上一成语}},0);
   do{   # pick one pronounciation for last character
	$发音=$$多音[rand @$多音];
	carp "$上一成语 不在成语词库中??" if !defined $发音;
	$index=first_index {!($发音 cmp $_.'') && $_>=0} @成语首字拼音;
	for(($成语首字拼音[$index]+0)..($成语首字拼音[$index+1]+0)){
	   push @collection, $成语[$_] if $成语[$_]>=0;
	}
	++$tries;
   }until @collection or $tries>@$多音;
   (@collection?删除该成语$collection[rand @collection]:undef, $发音);
}

sub 搜索成语{	# -1 on lookup failure
   my $该成语=shift;
   first_index {!($该成语 cmp $_)} @成语;
}

sub main{
   标注成语首字拼音;
   use Encode 'decode';
   my ($cnt,$prev,$prev2,$计数器,$发音,$cur,%历史记录)=2;
   if(@ARGV>1){
	$计数器=搜索成语 $cur=decode 'utf8',$ARGV[1];
	$cur=defined $计数器 ? dualvar $计数器,$cur : undef;
   }
   $cur//=$成语[rand @成语];
   printf "[1  ]%-6s %s\n", '', 删除该成语 $cur;
   $历史记录{1}=$cur;
   until($cnt>$ARGV[0]){
	$prev2=$prev; $prev=$cur;
	($cur,$发音)=下一个_成语 $cur;
	printf "[%-3d]%-6s ",$cnt++,$发音;
	print defined $cur ? "$cur\n" : "...继续无力\n";
	if(defined $cur){
	   $历史记录{$cnt-1}=$cur; next;
	}
	$计数器=0; # 1-step backtracking.
	do{
	   ($cur,$发音)=下一个_成语 $prev2;
	}until $cur // ++$计数器>5;
	last unless defined $cur;
	$cnt-=2;
	printf "[%-3d]%-6s %s\n",$cnt,$发音,$历史记录{$cnt-1}=$cur;
   }
   open my $文件句柄,'<:utf8', '成语词库.txt'	# definition lookup
	or croak '无法打开“成语词库.txt”';
   print "词义解释：\n\$"; $prev=0;
   while(my $option=<STDIN>){
	my $检索=looks_like_number($option) ? $历史记录{0+$option} :
	   搜索成语 decode 'utf8',substr $option,0,-1;
	unless(defined $检索 and $检索>=0){
	   print "?\n\$"; next;
	}
	seek $文件句柄,$cnt=0,0;
	while($cnt<$检索){
	   next if <$文件句柄>=~/^\s*$/; ++$cnt;
	}
	print $_=<$文件句柄>.'$';
   }
   close $文件句柄;
   defined $cur;
}
