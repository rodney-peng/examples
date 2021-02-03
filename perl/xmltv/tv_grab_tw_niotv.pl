#!/usr/bin/perl -w -C

#################################################################
# initializations

use strict;

#use encoding 'big5', STDOUT => 'big5';

use lib "/home/user1/MediaCenter/EPG/XMLTV/xmltv-0.5.41/blib/lib";

use Data::Dumper;
use Date::Manip;
use Time::Local;
use SOAP::Lite;
use File::Temp qw(tempfile);
use Getopt::Long;
use XML::Twig 3.10;

use XMLTV;
use XMLTV::Ask;
use XMLTV::Config_file;
use XMLTV::ProgressBar;
use XMLTV::Version '$Id: tv_grab_tw_niotv.in,v 0.1 2005/11/08 16:51:48 rpong Exp $ ';
use XMLTV::Usage <<END
$0: get lists from www.niotv.com in XMLTV format

    $0 [--version] [--help]
  
To Configure
    $0 --configure [--config-file FILE] [--dd-data FILE] [--gui OPTION]
                              [--reprocess] [--auto-config add|ignore]
To get listings
    $0 [--config-file FILE] [--dd-data FILE]
                  [--reprocess] [--auto-config add|ignore]
                  [--days N]  [--offset N] [--quiet]  [--old-chan-id]
                  [--low-mem] [--output FILE]
                  [--download-only]
END
;
use XMLTV::Get_nice;

use Sort::Naturally;
use XML::Writer;
use IO::File;

use Date::Calc qw(Add_Delta_Days);

use Encode;
#use String::Multibyte;

#use encoding::warnings 'FATAL';

#
# module version checking doesn't work with XMLTV version numbers
#
die "ERROR: XMLTV.PM 0.5.32 required\n" if $XMLTV::VERSION lt '0.5.32';

#######################################
# Function prototypes

sub load_config_file( $ );
sub generate_channel_list( $ );
sub get_channel_map( $ );

sub generate_program_info( $$$%$ );
sub grab_program_info;
sub extract_program_info( $% );
sub extract_program_detail( $ );
sub merge_program_info;

sub trimTail( $$ );
sub checkBig5Encoding( $$ );
sub xml_write( $$$ );
sub get_boolean( $ );
sub fix_unicode( $ );

#######################################
# Variables

  my %channellist;       # virtual ID => channel name
  my $maxdays = 1;
  my $listingfile = '';
  my $cfgGetDetail = 0;
  my $cfgMaxDescLen = 300;

  my %channelmap;        # channel name => real ID

  my $channelid = "";
  my $channelname = "";
  my %chaninfo;
  my %chanlist;

  my %CategoryMap = ( '電影' => 'Movie',
		                  '影集' => 'Episode',
		                  '連劇' => 'Soap',
		                  '綜藝' => 'Shows',
		                  '音樂' => 'Music',
		                  '知性' => 'Knowledge',
		                  '體育' => 'Sport',
		                  '卡通' => 'Cartoon',
		                  '新聞' => 'News',
		                  '戲曲' => 'Folk',
  		                '其他' => 'Misc' 
  		              );
  my %SubCategoryMap = ( '喜劇搞笑' => 'Comedy',
                         '懸疑推理' => 'Thriller',
                         '靈異驚悚' => 'Horror',
                         '功夫武俠' => 'Kong-Fu',
                         '動作冒險' => 'Action and Adventure',
                         '科幻神怪' => 'Sci-Fi',
                         '愛情文藝' => 'Romance',
                         '曲折奇情' => 'Thriller',
                         '偶像青春' => 'Idol(YA)',
                         '戰爭災難' => 'War and Disaster',
                         '歷史傳記' => 'Historical Epic',
                         '溫馨倫理' => 'Drama',
                         '其他'     => 'Other',
                         '歌舞音樂' => 'Musical',
                         '社會寫實' => 'Gangster and Crime',
                         '情色'     => 'Adult',
                         '動畫'     => 'Animation',
                         '喜鬧搞笑' => 'Entertainment',
                         '流行資訊' => 'Fashion',
                         '軟性訪談' => 'Talk Show',
                         '益智競賽' => 'Game Show',
                         '交友聯誼' => 'Match',
                         '節慶頒獎' => 'Ceremony',
                         '歌唱'    => 'Singing',
                         '演奏'    => 'Performance',
                         'MTV'     => 'MTV',
                         '科學新知' => 'Science',
                         '動物生態' => 'Animal',
                         '奇聞軼事' => 'Mystery',
                         '民俗采風' => 'Tradition',
                         '休閒旅遊' => 'Travel',
                         '美食餐飲' => 'Food',
                         '生活健康' => 'Real Life',
                         '婦女兒童' => 'Woman and Children',
                         '投資理財' => 'Investment',
                         '命理風水' => 'Fortune-Telling',
                         '教育教學' => 'Education',
                         '宗教心靈' => 'Religion',
                         '藝術藝文' => 'Art',
                         '棒球'     => 'Baseball',
                         '籃球'     => 'Basketball',
                         '足球'     => 'Soccer',
                         '撞球'     => 'Snooker',
                         '高爾夫球' => 'Golf',
                         '網球'     => 'Tennis',
                         '競速'     => 'Racing',
                         '武術搏擊' => 'Martial Art',
                         '播報'     => 'Press',
                         '報導雜誌' => 'Press',
                         '訪談座談' => 'Commentary',
                         '評論'     => 'Commentary',
                         '國劇'     => 'Local theatre',
                         '歌仔戲'   => 'Local theatre',
                         '布袋戲'   => 'Local theatre',
                         '廣告'     => 'AD'
  		                 );

  my %LanguageMap = (
				'國語' => 'Chinese',
				'台語' => 'Taiwan',
				'客語' => 'Hokka',
				'香港' => 'Hong Kong',
				'大陸' => 'China',
				'日本' => 'Japan',
				'韓國' => 'Korea',
				'英美' => 'English',
				'法義' => 'Franch/Italian',
				'其他' => 'Other'
				);

#######################################
# Main program

#test_encoding();
#exit;

my $begin_time = localtime;

#load_config_file( 'tv_grab_tw_niotv.conf' );
load_config_file( 'test.conf' );

die "\nListing file is not defined !" if (length($listingfile) == 0);

generate_channel_list( 'ChannelInfo.xml' );

get_channel_map( 'chlist_all.htm' );

grab_program_info();
merge_program_info();

my $end_time = localtime;

print "\nOK\n";

$ENV{'TZ'} = 'CST';

print "\nBegin at $begin_time, End at $end_time, Elapsed " . DateCalc( ParseDate( $begin_time ), ParseDate( $end_time ) );

#######################################
# Configuration and Channel list

sub load_config_file( $ )
{
  my $cfgfile = XMLTV::Config_file::filename( shift, 'tv_grab_tw', 0 );
  
  foreach (XMLTV::Config_file::read_lines( $cfgfile ))
  {
        next unless defined $_;
        chomp;
        my( $setting, $val ) = split( /:\s+/o, $_, 2 );

        if ( $setting =~ /^(not )?channel$/o )
        {
            my( $vch_id, $ch_name ) = split( /\s+/, $val, 2 );
            
            $channellist{ $vch_id } = substr( $ch_name, 1, length( $ch_name ) - 2 );
        }
        elsif ($setting =~ /^(not )?days$/o )
        {
            if ($val > 0)
            {
                $maxdays = $val;
            }
        }
        elsif ($setting =~ /^(not )?listing$/o )
        {
            $listingfile = substr( $val, 1, length( $val ) - 2 );
        }
        elsif ($setting =~ /^(not )?cfgGetDetail$/o )
        {
            $cfgGetDetail = get_boolean( $val );
        }
        elsif ($setting =~ /^(not )?cfgMaxDescLen$/o )
        {
        	  if ($val >= 0)
        	  {
                $cfgMaxDescLen = $val;
            }
        }
  }
}

sub generate_channel_list( $ )
{
  my $filename = shift;
  my $output = new IO::File( "> $filename" );	
  my $writer = XML::Writer->new( OUTPUT => $output, NEWLINES => 1 );
	
  $writer->xmlDecl( 'Big5', 'Yes');
  
  $writer->startTag('NewDataSet');
  
  foreach my $vch_id (nsort( keys %channellist ))
  {
    my $ch_name = $channellist{ $vch_id };
    
    $writer->startTag('channel');
    
    $writer->startTag('name');
    xml_write( $output, $writer, $ch_name );
    $writer->endTag('name');

    $writer->startTag('channelID');
    xml_write( $output, $writer, $ch_name );
    $writer->endTag('channelID');

    $writer->startTag('virtualchannel');
    xml_write( $output, $writer, $vch_id );
    $writer->endTag('virtualchannel');

    $writer->endTag('channel');
  }

  $writer->endTag('NewDataSet');
  
  $writer->end();
  
  $output->close();
}

sub get_channel_map( $ )
{
=rem
	my $fn = shift;
  my $fpTEST;
  my $content;
  my $rch_id;
  my $ch_name;

  open(fpTEST,"< $fn"); # '/' is root directory
  {
    local $/;
    $/ = undef;
    $content = <fpTEST>;
  }
  close(fpTEST);
  
 	while ($content =~ m/<input type=hidden name=sch_id value=([^>]+)>[\r\n\x09]+<input type=hidden name=ch_name value=([^>]+)>/g)
 	{
 		$rch_id  = $1; # real ID
 		$ch_name = $2;
 		
 		$channelmap{ $ch_name } = $rch_id;
 		
 		print "\nMap $rch_id \"$ch_name\"";
  }
=cut

$channelmap{ '台視'        }= '11';
$channelmap{ '客家電視'    }= '12';
$channelmap{ '中視'        }= '13';
$channelmap{ '中天娛樂'    }= '14';
$channelmap{ '華視'        }= '15';
$channelmap{ '民視無線'    }= '16';
$channelmap{ '公共電視'    }= '17';
$channelmap{ '超視'        }= '18';
$channelmap{ '衛視中文'    }= '19';
$channelmap{ 'TVBS'        }= '20';
$channelmap{ 'GTV綜合'     }= '21';
$channelmap{ 'GTV戲劇'     }= '22';
$channelmap{ '東森綜合'    }= '23';
$channelmap{ '緯來綜合'    }= '24';
$channelmap{ 'MUCH-TV'     }= '25';
$channelmap{ '霹靂衛視'    }= '26';
$channelmap{ '中天綜合'    }= '27';
$channelmap{ '緯來戲劇'    }= '29';
$channelmap{ 'TVBS-G'      }= '32';
$channelmap{ 'GTV第一'     }= '33';
$channelmap{ '三立台灣'    }= '34';
$channelmap{ '三立都會'    }= '35';
$channelmap{ '東森戲劇'    }= '36';
$channelmap{ '東風衛視'    }= '37';
$channelmap{ '三立新聞'    }= '38';
$channelmap{ '年代新聞'    }= '40';
$channelmap{ 'TVBS-N'      }= '41';
$channelmap{ '東森新聞'    }= '42';
$channelmap{ '中天新聞'    }= '44';
$channelmap{ '民視新聞'    }= '45';
$channelmap{ 'HBO'         }= '46';
$channelmap{ 'STAR_MOVIES' }= '47';
$channelmap{ '東森洋片'    }= '48';
$channelmap{ 'CINEMAX'     }= '49';
$channelmap{ 'AXN'         }= '50';
$channelmap{ '好萊塢電影'  }= '52';
$channelmap{ '緯來育樂'    }= '53';
$channelmap{ '衛視電影'    }= '55';
$channelmap{ '東森電影'    }= '56';
$channelmap{ '緯來電影'    }= '57';
$channelmap{ 'DISCOVERY'   }= '58';
$channelmap{ '國家地理'    }= '59';
$channelmap{ '太陽衛視'    }= '60';
$channelmap{ '動物星球'    }= '61';
$channelmap{ '旅遊生活'    }= '62';
$channelmap{ '迪士尼'      }= '63';
$channelmap{ '東森幼幼'    }= '64';
$channelmap{ '卡通Network' }= '65';
$channelmap{ '緯來體育'    }= '66';
$channelmap{ 'ESPN'        }= '67';
$channelmap{ '衛視體育'    }= '68';
$channelmap{ 'MTV'         }= '69';
$channelmap{ 'Channel[V]'  }= '70';
$channelmap{ 'JET日本'     }= '71';
$channelmap{ '緯來日本'    }= '72';
$channelmap{ '國興衛視'    }= '73';
$channelmap{ 'NHK'         }= '74';
$channelmap{ 'Z頻道'       }= '75';
$channelmap{ '非凡商業'    }= '77';
$channelmap{ '非凡新聞'    }= '79';
$channelmap{ '大愛電視'    }= '80';
$channelmap{ '好消息'      }= '81';
$channelmap{ 'ANIMAX'      }= '84';
$channelmap{ '超級X'       }= '97';
$channelmap{ '華視教育'    }= '138';
$channelmap{ '高點電視'    }= '143';
$channelmap{ 'BBC'         }= '144';
$channelmap{ 'TV5'         }= '145';
$channelmap{ 'viva購物2'   }= '146';
}

#######################################
# Grab program info

sub grab_program_info
{
	  my( $currday, $currmonth, $curryear) = (localtime)[3,4,5];
	  
	  $curryear  += 1900; # 1900 based
	  $currmonth += 1;    # 0 based
	  # $currday is 1 based

	  my $prgdate = sprintf( "%04u%02u%02u", $curryear, $currmonth, $currday );
	
	  for my $i (1..$maxdays)
	  {
	      if (! -d $prgdate)
	      {
	      	  next unless mkdir $prgdate;
	      }	

		    foreach my $vch_id (nsort( keys %channellist ))
		    {
			      my $ch_name = $channellist{ $vch_id };
			      
			      if ($channelmap{ $ch_name }) # check if real channel id exists ?
			      {	      	  
  		      	  my $rch_id  = $channelmap{ $ch_name };
                my %programinfo;

			      	  if (! -s "$prgdate/CH$rch_id.htm")
			      	  {
		                my $output  = "-o \"$prgdate/CH$rch_id.htm\"";
		                my $getchid = "-d sch_id=$rch_id";
		                my $getname = "-d ch_name=\"$ch_name\"";
		                my $getday  = "-d day=$curryear-$currmonth-$currday";
		                my $posturl = "http://www.niotv.com/day.php?act2=&userid=&username=";
		                
		                print "\nGetting $prgdate/CH$rch_id.htm";
		
						        system( "curl $output -d act=select $getchid $getname $getday -d tstring= -d grp_id=-1 \"$posturl\"" );
				        }

			      	  if ((-s "$prgdate/CH$rch_id.htm") && (! -s "$prgdate/CH$rch_id.xml"))
			      	  {
		                print "\nGetting $prgdate/CH$rch_id.xml";

			      	  	  if (extract_program_info( "$prgdate/CH$rch_id.htm", \%programinfo ))
			      	  	  {
			      	  	      generate_program_info( $vch_id, $ch_name, $prgdate, \%programinfo, "$prgdate/CH$rch_id.xml" );
			      	  	  }
			      	  	  else
			      	  	  {
			      	  	  	  unlink( "$prgdate/CH$rch_id.htm" );
			      	  	  	
			      	  	  	  print "\nWarning: No program listed in $prgdate/CH$rch_id.htm";
			      	  	  }
			      	  }

			      	  if (! -s "$prgdate/CH$rch_id.xml")
			      	  {
				      	    print "\nWarning: Cannot retrieve channel $vch_id \"$ch_name\".";
			      	  }
			      }
			      else
			      {
                if (! -s "$prgdate/_CH$vch_id.htm")
                {
                    my %programinfo;
                	
        	  	      generate_program_info( $vch_id, $ch_name, $prgdate, \%programinfo, "$prgdate/_CH$vch_id.xml" );
                }
			      	
				      	print "\nWarning: Cannot locate channel $vch_id \"$ch_name\".";
			      }
		    }
	  	
	      ($curryear, $currmonth, $currday) = Add_Delta_Days($curryear, $currmonth, $currday, 1);
	    
	      $prgdate = sprintf( "%04u%02u%02u", $curryear, $currmonth, $currday );
	  }
}

sub merge_program_info
{
	  # Merge all program info from each single day and each single channel

	  my( $currday, $currmonth, $curryear) = (localtime)[3,4,5];
	  
	  $curryear  += 1900; # 1900 based
	  $currmonth += 1;    # 0 based
	  # $currday is 1 based
	
	  my $prgdate = sprintf( "%04u%02u%02u", $curryear, $currmonth, $currday );
	  
	  my @allfiles;
	  my %args;
	
	  for my $i (1..$maxdays)
	  {
		    foreach my $vch_id (nsort( keys %channellist ))
		    {
			      my $ch_name = $channellist{ $vch_id };
			      
			      if ($channelmap{ $ch_name }) # check if real channel id exists ?
			      {	      	  
  		      	  my $rch_id = $channelmap{ $ch_name };
  		      	  
			      	  if (-s "$prgdate/CH$rch_id.xml")
			      	  {
						  		  push( @allfiles, "$prgdate/CH$rch_id.xml" );
						  	}
						  	else
						  	{
						  		  print "\nWarning: $prgdate/CH$rch_id.xml not found";
						  	}
						}
						elsif (-s "$prgdate/_CH$vch_id.xml")
						{
    		  		  push( @allfiles, "$prgdate/_CH$vch_id.xml" );
						}
				}
	
	    ($curryear, $currmonth, $currday) = Add_Delta_Days( $curryear, $currmonth, $currday, +1 );

	    $prgdate = sprintf( "%04u%02u%02u", $curryear, $currmonth, $currday );
	  }
	  
	  my $fh = new IO::File "> $listingfile";
	  %args = ( OUTPUT => $fh );
	  
	  XMLTV::catfiles( \%args, @allfiles );
	  
	  $fh->close();
	  
	  fix_unicode( $listingfile );
}

sub extract_program_info( $% )
{
  my $input = $_[0];

  my $fpTEST;
  my $content;

  open(fpTEST,"< $input");
  {
    local $/;
    $/ = undef;
    $content = <fpTEST>;
  }
  close(fpTEST);
  
  my $channel;
  my $prgtime;
  my $prgname;
  my $prgcategory;
  my $prgextra;
  my $prgdetail;

  my $lastprg;
  my $lastprgstart;
  my $lastprgend;
  
 	while ($content =~ m/<td class=lin-down[^>]*>(.*)<\/td>[\n\x09]+<td class=lin-down[^>]*>(.*)<\/td>[\n\x09]+<td class=lin-down[^>]*>(.*)<\/td>[\n\x09]+<td class=lin-down[^>]*>(.*)<\/td>[\n\x09]+<td class=lin-down[^>]*>(.*)<\/td>/g)
 	{
 		$channel     = $1;
 		$prgtime     = $2;
 		$prgname     = $3;
 		$prgcategory = $4;
 		$prgextra    = $5;
 		
 		if ($prgname =~ m/javascript:link\('([^']+)'/)
 		{
 			  $prgdetail = $1;
 		}
 		else
 		{
 			  $prgdetail = '';
 		}
 		
    $prgtime     =~ s/<[^>]*>//g;
 		$prgname     =~ s/<[^>]*>//g;
 		$prgcategory =~ s/<[^>]*>//g;

 		if ($prgtime =~ m/(\d\d):(\d\d)~(\d\d):(\d\d)/)
 		{
		    my $prgstart = $1 . $2;
		    my $prgend   = $3 . $4;
		    
    		$prgname = checkBig5Encoding( $prgname, -1 );

		 		my @program = ( $prgname, $prgcategory, $prgdetail );
		 		
		 		if ($lastprg && ($lastprg->[0] eq $prgname) && ($lastprgend eq $prgstart))
		 		{
				 		# Merge to the last program if the program names are the same and
				 		# the program times are adjacent
				 		
				 		my $lastprgtime = $lastprgstart . '-' . $lastprgend;
				 		
				 		delete( $_[1]{$lastprgtime} );
				 		
				 		$prgtime = $lastprgstart . '-' . $prgend;
				 		
				 		$_[1]{$prgtime} = \@$lastprg;

					  print "\nInfo: Merged $prgname: $lastprgend -> $prgend";
				
					  $lastprgend = $prgend;					  
			  }
			  else
			  {
    		    $prgtime = $prgstart . '-' . $prgend;

				 		$_[1]{$prgtime} = \@program;

					  $lastprg      = \@program;
					  $lastprgstart = $prgstart;
					  $lastprgend   = $prgend;
			  }			  
	  }
  }
  
  return ($lastprg) && ($lastprgstart gt $lastprgend);
}

sub generate_program_info( $$$%$ )
{
	  my $vch_id      = $_[0];
	  my $ch_name     = $_[1];
	  my $prgdate     = $_[2];
  	my %programinfo = %{$_[3]};
	  my $filename    = $_[4];

    my $output = new IO::File("> $filename");
    my $writer = XML::Writer->new( OUTPUT => $output, NEWLINES => 1 );

    $writer->xmlDecl( 'Big5');
    $writer->doctype('tv', undef, 'xmltv.dtd');
  
    $writer->startTag('tv');
    
    #-------------------------------------------------------------

    $writer->startTag('channel', 'id' => $ch_name);
    
    $writer->startTag('display-name');
    xml_write( $output, $writer, $ch_name );
    $writer->endTag('display-name');

    #---------------- (for mythfilldatabase) put channel number after channel name

    $writer->startTag('display-name');
    xml_write( $output, $writer, $vch_id );
    $writer->endTag('display-name');

    $writer->endTag('channel');

    #-------------------------------------------------------------
    
	  foreach my $prgtime (nsort(keys %programinfo))
	  {
	  	  my( $prgname, $prgcategory, $prgdetail ) = @{$programinfo{$prgtime}};
	  	 
	  	  $prgtime =~ m/(\d\d\d\d)-(\d\d\d\d)/;

	  	  my $prgstart = $1;
	  	  my $prgend   = $2;

	  	  if ($prgstart < $prgend)
	  	  {
    	  	  $prgstart = $prgdate . $prgstart;
	  	  }
	  	  elsif ($prgstart > $prgend)
	  	  {
    	  	  $prgstart = $prgdate . $prgstart;

					  my $prgyear  = substr( $prgdate, 0, 4 );
					  my $prgmonth = substr( $prgdate, 4, 2 );
					  my $prgday   = substr( $prgdate, 6, 2 );

    	      ($prgyear, $prgmonth, $prgday) = Add_Delta_Days($prgyear, $prgmonth, $prgday, +1 );
    	      $prgdate = sprintf( "%04u%02u%02u", $prgyear, $prgmonth, $prgday );
	  	  }
	  	  else
	  	  {
	  	  	  next;
	  	  }

	  	  $prgend = $prgdate . $prgend;

        $writer->startTag( 'programme', 'start' => $prgstart . '00 +0800', 'stop' => $prgend . '00 +0800', 'channel' => $ch_name );
    
        $writer->startTag('title', 'lang' => 'zh-tw' );
        xml_write( $output, $writer, $prgname );
        $writer->endTag('title');
        
        if ($prgdetail && $cfgGetDetail)
        {
        	  print "\nGetting ($prgtime)$prgname";
        	
        	  my $detail = XMLTV::Get_nice::get_nice( 'http://www.niotv.com/' . $prgdetail );
        	  
        	  my ( $desc, $directors, $actors, $writers, $producers, $presenters ) = extract_program_detail( $detail );
        	  
        	  if (length( $desc ) > 0)
        	  {
				        $writer->startTag('desc', 'lang' => 'zh-tw' );
				        xml_write( $output, $writer, $desc );
				        $writer->endTag('desc');
        	  }
        	  
        	  if (@$directors || @$actors || @$writers || @$producers || @$presenters)
        	  {
		            $writer->startTag('credits', 'lang' => 'zh-tw' );
		        	  
		        	  foreach my $stuff (@$directors)
		        	  {
						        $writer->startTag('director', 'lang' => 'zh-tw' );
						        xml_write( $output, $writer, $stuff );
						        $writer->endTag('director');
		        	  }
		
		        	  foreach my $stuff (@$actors)
		        	  {
						        $writer->startTag('actor', 'lang' => 'zh-tw' );
						        xml_write( $output, $writer, $stuff );
						        $writer->endTag('actor');
		        	  }
		
		        	  foreach my $stuff (@$writers)
		        	  {
						        $writer->startTag('actor', 'lang' => 'zh-tw' );
						        xml_write( $output, $writer, $stuff );
						        $writer->endTag('actor');
		        	  }
		
		        	  foreach my $stuff (@$producers)
		        	  {
						        $writer->startTag('actor', 'lang' => 'zh-tw' );
						        xml_write( $output, $writer, $stuff );
						        $writer->endTag('actor');
		        	  }
		
		        	  foreach my $stuff (@$presenters)
		        	  {
						        $writer->startTag('actor', 'lang' => 'zh-tw' );
						        xml_write( $output, $writer, $stuff );
						        $writer->endTag('actor');
		        	  }
		
		            $writer->endTag('credits' );
            }
        }
        
        my ($cat1, $cat2, $cat3) = split( /-/, $prgcategory );

        if ($cat1 && ($cat1 ne "&nbsp;"))
        {
        	  if ($CategoryMap{$cat1})
        	  {
				        $writer->startTag('category', 'lang' => 'zh-tw' );
				        xml_write( $output, $writer, $CategoryMap{$cat1} );
				        $writer->endTag('category');
        	  }
        }

        if ($cat2 && ($cat1 ne $cat2))
        {
        	  if ($SubCategoryMap{$cat2})
        	  {
				        $writer->startTag('category', 'lang' => 'zh-tw' );
	  			      xml_write( $output, $writer, $SubCategoryMap{$cat2} );
				        $writer->endTag('category');
  		      }
        }

        if ($cat3)
        {
        	  if ($LanguageMap{$cat3})
        	  {
				        $writer->startTag('country', 'lang' => 'zh-tw' );
				        xml_write( $output, $writer, $LanguageMap{$cat3} );
				        $writer->endTag('country');
				    }
        }

        $writer->endTag('programme');
	  }

  $writer->endTag('tv');

  $writer->end();  
  $output->close();
}

sub extract_program_detail( $ )
{
	my $content = $_[0];

  my $posStart = index( $content, '劇情簡介：' );
  my $posEnd   = -1;
  
  if ($posStart > 0)
  {
  	  $posStart += length( '劇情簡介：');
  	
  	  $posStart = index( $content, '</td></tr>', $posStart );
  	  
  	  if ($posStart >= 0)
  	  {
      	  $posStart += length( '</td></tr>');

  	  	  $posEnd = index( $content, '</td></tr>', $posStart );
  	  }
  }

  my $desc = '';
  
  if ($posEnd > 0)
  {
  	  $desc = substr( $content, $posStart, $posEnd - $posStart );  	  
  	  $desc =~ s/<[^>]*>//g;
  	  $desc =~ s/[\r\n\x09]//g;
  	  $desc =~ s/&nbsp\;//;
  	  
  	  $desc =~ s/&amp;(#\d+;)/&$1/g;    # correct typo
  	  $desc =~ s/&amp;egrave;/&#232;/g; # correct typo

  	  $desc = checkBig5Encoding( $desc, $cfgMaxDescLen );
  }

  $posStart = index( $content, '相關人物：' );
  $posEnd   = -1;

  if ($posStart > 0)
  {
  	  $posStart += length( '相關人物：');
  	
  	  $posStart = index( $content, 'valign="top">', $posStart );
  	  
  	  if ($posStart >= 0)
  	  {
      	  $posStart += length( 'valign="top">');

  	  	  $posEnd = index( $content, '</td>', $posStart );
  	  }
  }

  my @directors;
  my @actors;
  my @writers;
  my @adapters;
  my @producers;
  my @presenters;
  my @commentators;
  my @guests;

  if ($posEnd > 0)
  {
      my $credits = '';

  	  $credits = substr( $content, $posStart, $posEnd - $posStart );
  	  $credits =~ s/[\r\n\x09]//g;
  	  $credits =~ s/&nbsp\;//;
  	  $credits =~ s/<br\ \/>/\n/g;

  	  $credits =~ s/&amp;(#\d+;)/&$1/g;    # correct typo
  	  $credits =~ s/&amp;egrave;/&#232;/g; # correct typo
  	  
  	  my $curr_role  = '演員';
  	  
  	  my @allcredit = split( /\n/, $credits );
  	  
  	  foreach my $credit (@allcredit)
  	  {
		  	  $credit =~ s/：/:/g;
		  	  
		  	  trimTail( $credit, ' ' );
		  	  
      	  $credit = checkBig5Encoding( $credit, -1 );
      	  
      	  next if (length( $credit ) < 2);
  	  	  next if ($credit =~ m/無相關人物/);
  	  	  
  	  	  my $role;
  	  	  
  	  	  if ($credit =~ m/:/)
  	  	  {
  	  	  	  my @stuff = split( /:/, $credit );
  	  	  	  
  	  	  	  $credit = trimTail( $stuff[1], ' ' );
  	  	  	  
 	  	  	    if ($credit)
 	  	  	    {
      	  	  	  $role = $stuff[0];

 	  	  	    	  $curr_role = '演員';
 	  	  	    }
 	  	  	    else
 	  	  	    {
 	  	  	    	  $curr_role = $stuff[0];
 	  	  	    }
  	  	  }
  	  	  else
  	  	  {
      	  	  $role = $curr_role;
  	  	  }
  	  	  
  	  	  if ($credit)
  	  	  {
  	  	  	  $_ = $role;
  	  	  	
		  	  	  if (m/^Director/i || m/^導演/ || m/^監督/ || m/^總導演/ || m/^總監督/)
		  	  	  {
      	  	      push( @directors, $credit );
		  	  	  }
		  	  	  elsif ((m/^主演/) || (m/^演員/) || (m/^演出/) || (m/^配音/))
		  	  	  {
      	  	      push( @actors, $credit );
		  	  	  }
		  	  	  elsif (m/^原作/ || m/^作者/ || m/^編劇/)
		  	  	  {
      	  	      push( @writers, $role . ':' . $credit );
		  	  	  }
		  	  	  elsif (m/^製片人/ || m/^製作人/ || m/^製作/)
		  	  	  {
      	  	      push( @producers, $role . ':' . $credit );
		  	  	  }
		  	  	  elsif (m/^主持人/ || m/^代班主持人/ || m/^VJ/ || m/^主播/ || m/^主持/ ||
		  	  	         m/^主講人/ || m/^營養師/ || m/^主持群/
		  	  	        )
		  	  	  {
      	  	      push( @presenters, $role . ':' . $credit );
		  	  	  }
		  	  	  else
		  	  	  {
      	  	      push( @actors, $role . ':' . $credit );
		  	  	  }		
  	  	  }
  	  }
  }

=rem
	foreach my $stuff (@directors)
	{
	    print "\nDirector-$stuff";
	}
	
	foreach my $stuff (@actors)
	{
	    print "\nActor-$stuff";
	}
	
	foreach my $stuff (@writers)
	{
	    print "\nWriter-$stuff";
	}
	
	foreach my $stuff (@adapters)
	{
	    print "\nadapter-$stuff";
	}
	
	foreach my $stuff (@producers)
	{
	    print "\nProducer-$stuff";
	}
	
	foreach my $stuff (@presenters)
	{
	    print "\nPresenter-$stuff";
	}
	
	foreach my $stuff (@commentators)
	{
	    print "\nCommentator-$stuff";
	}
	
	foreach my $stuff (@guests)
	{
	    print "\nGuest-$stuff";
	}
=cut
  
  return ( $desc, \@directors, \@actors, \@writers, \@producers, \@presenters );
}

sub trimTail( $$ )
{
	  my $content = $_[0];
	  my $lookfor = $_[1];

=rem
	  while ($content && (substr( $content, 0, 1 ) eq $lookfor))
	  {
	  	  substr( $content, 0, 1 ) = '';
	  }
=cut
	  
	  while ($content && (substr( $content, -1, 1 ) eq $lookfor))
	  {
	  	  substr( $content, -1, 1 ) = '';
	  }
	  
	  return $content;
}

sub checkBig5Encoding( $$ )
{
	  my $content = $_[0];
	  my $maxlen  = $_[1];
	  my $fix = '';
	  my $len = 0;
	  
    while ($content =~ m/([\x00-\x7F]|[\xA1-\xF9][\x40-\x7E\xA1-\xFE])/g)
    {
    	  if ($1 eq "\xC8\xC5")
    	  {
    	  	  $fix .= "\xC5";
    	  	  
    	  	  print "\n---C8-C5---\n";
    	  }
    	  else
    	  {
    	      $fix .= $1;
    	  }
    	  $len++;
    	  last if ($len == $maxlen);
    }

    $fix =~ s/\xF9\xD6/&#30849;/g;
    $fix =~ s/\xF9\xD7/&#37561;/g;
    $fix =~ s/\xF9\xD8/&#35023;/g;
    $fix =~ s/\xF9\xD9/&#22715;/g;
    $fix =~ s/\xF9\xDA/&#24658;/g;
    $fix =~ s/\xF9\xDB/&#31911;/g;
    $fix =~ s/\xF9\xDC/&#23290;/g;
    
    return $fix;

=rem
    my $str = String::Multibyte->new('Big5');

    $content = $str->substr( $content, 0, $maxlen ) if (0 < $maxlen && $str->length($content) > $maxlen);

    while ((length($content) > 0) && (! $str->islegal($content)))
    {
        substr( $content, -1, 1 ) = '';
    }
=cut

=rem
    {
		    my $check;
		    
		    $@ = " ";
		
		    while ($@ && (length( $content ) > 0))    
		    {
		        $check = $content;
		    	
				    eval { Encode::from_to( $check, 'big5-eten', 'utf-8', Encode::FB_CROAK ); };
				    
				    if ($@) # error				    
				    {
				    	  print "\n$@";
				    	
		    		 		chop( $content );
				    	  
		            undef $check;
				    }
		    }
    }
	  
	  $maxlen = length( $content ) if ($maxlen <= 0 || length( $content ) < $maxlen);
	  
	  {
	  	  # Truncates string to the $maxlen and ensures it ends at double-bytes character boundary
	  	
			  my $i = 0;
			  
			  while ($i < $maxlen)
			  {
			      if (ord( substr( $content, $i, 1 ) ) > 127)
			      {
			      	  last if ($i + 1 == $maxlen);
			      	  
			      	  $i++;
			      }
		
		     	  $i++;
			  }
			  
			  if ($i > 0)
			  {
			      $content = substr( $content, 0, $i );
			  }
			  else
			  {
			  	  $content = '';
			  }
    }
    
    # replace BIG-5 extension to unicode equivalent

    $content =~ s/\xF9\xD6/&#30849;/g;
    $content =~ s/\xF9\xD7/&#37561;/g;
    $content =~ s/\xF9\xD8/&#35023;/g;
    $content =~ s/\xF9\xD9/&#22715;/g;
    $content =~ s/\xF9\xDA/&#24658;/g;
    $content =~ s/\xF9\xDB/&#31911;/g;
    $content =~ s/\xF9\xDC/&#23290;/g;

    return $content;
=cut
}

sub xml_write( $$$ )
{
	  # Separate unicode character and output as raw data
	
	  my $file     = $_[0];
	  my $writer   = $_[1];
	  my @contents = split( /(&#\d+;)/, $_[2] );
	  
	  foreach my $content (@contents)
	  {
	  	  if ($content =~ m/^&#\d+;$/)
	  	  {
	  	  	  print $file $content;
	  	  }
	  	  else
	  	  {
	  	  	  $writer->characters( $content );
	  	  }
	  }
}

sub get_boolean( $ )
{
	  my $bexp = $_[0];
	  
	  $bexp = 0 if ($bexp && ($bexp =~ m/^no$/i));
	  
	  return $bexp;
}

sub fix_unicode( $ )
{
	my $fnIn = $_[0];
	my $fnOut = $fnIn;

	$fnOut .= '-fix' if (! ($fnOut =~ s/\./-fix\./));

  my $fpInput;
  my $output = new IO::File("> $fnOut");
  my $content;

  open( fpInput, "< $fnIn" );

  while (! eof(fpInput))
  {
    $content = readline(fpInput);
    $content =~ s/&amp;(#\d+;)/&$1/g;
    print $output $content;
  }

  close( fpInput );
  $output->close();
}

sub test_encoding
{
  my $fpTEST;
  my $content;

  open(fpTEST,"< epg_detail.htm");
  {
    local $/;
    $/ = undef;
    $content = <fpTEST>;
  }
  close(fpTEST);

  my ( $desc, $directors, $actors, $writers, $producers, $presenters ) = extract_program_detail( $content );
  print "\n\n[$desc]\n\n";
    my $output = new IO::File("> epg_detail.xml");
    my $writer = XML::Writer->new( OUTPUT => $output, NEWLINES => 1 );

    $writer->xmlDecl( 'Big5');
    $writer->doctype('tv', undef, 'xmltv.dtd');
  
    $writer->startTag('tv');
        $writer->startTag( 'programme', 'start' => '20050101010100 +0800', 'stop' => '20050101020200 +0800', 'channel' => 'HBO' );
    
        $writer->startTag('desc', 'lang' => 'zh-tw' );
        xml_write( $output, $writer, $desc );
        $writer->endTag('desc');
 
             $writer->startTag('credits', 'lang' => 'zh-tw' );
        	  
        	  foreach my $stuff (@$directors)
        	  {
				        $writer->startTag('director', 'lang' => 'zh-tw' );
				        xml_write( $output, $writer, $stuff );
				        $writer->endTag('director');
        	  }

        	  foreach my $stuff (@$actors)
        	  {
				        $writer->startTag('actor', 'lang' => 'zh-tw' );
				        xml_write( $output, $writer, $stuff );
				        $writer->endTag('actor');
        	  }

        	  foreach my $stuff (@$writers)
        	  {
				        $writer->startTag('writer', 'lang' => 'zh-tw' );
				        xml_write( $output, $writer, $stuff );
				        $writer->endTag('writer');
        	  }

        	  foreach my $stuff (@$producers)
        	  {
				        $writer->startTag('producer', 'lang' => 'zh-tw' );
				        xml_write( $output, $writer, $stuff );
				        $writer->endTag('producer');
        	  }

        	  foreach my $stuff (@$presenters)
        	  {
				        $writer->startTag('presenter', 'lang' => 'zh-tw' );
				        xml_write( $output, $writer, $stuff );
				        $writer->endTag('presenter');
        	  }

            $writer->endTag('credits' );
       
        $writer->endTag( 'programme' );
    
  $writer->endTag('tv');

  $writer->end();  
  $output->close();
}

1;
