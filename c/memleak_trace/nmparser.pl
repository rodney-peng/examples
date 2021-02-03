#!/usr/bin/perl -W
#

$obj = (defined $ARGV[0]) ? $ARGV[0] : die "Please specify the object file!\n";
$cfile = (defined $ARGV[1]) ? $ARGV[1] : die "Please specify the output file!\n";
$funcs = 0;
$nmpath = "arm-linux-nm";

open(IN, "$nmpath -anS $obj | grep \" [t|T] \"|") || die "Can not open pipe for nm: $!\n";
open(C, ">$cfile") || die "Can't open $cfile: $!\n";

print C
"#include <stdlib.h>

typedef struct
{
    unsigned long address;
    unsigned long size;
    char          type;
    const char *  name;
    
} SYMBOL_TABLE;

SYMBOL_TABLE SymbolTable[] =
{
";

while ($line = <IN>)
{
    @col = split(/ /, $line);
    if (defined $col[3])
    {
      chomp($col[3]);
      if (($col[2] eq "T" || $col[2] eq "t") && ($col[2] !~ /\./) && $funcs++ ne "%")
      {
        print C "{ 0x$col[0], 0x$col[1], '$col[2]' , \"$col[3]\"},\n";
      }
    }
}

print C
"{ 0xffffffff, 0, '\\0', NULL }
};
";

print C "\nunsigned long SymbolTableSize = $funcs;\n";

close C;
close IN;

