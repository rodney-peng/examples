#!/usr/bin/perl -w

use strict;
use Git;
use feature 'switch';
use File::Path qw(make_path);
use File::stat qw(stat);
use List::Util 1.33 'any';

sub patch_tree( $$$$$ );
sub hash_type( $$ );
sub create_tree( $ );
sub merge_file( $$$$$ );
sub copy_file( $$$$$$ );
sub add_tree( $$$$ );
sub add_file( $$$$$$ );
sub del_tree( $$$ );
sub del_file( $$$ );
sub check_e30_status( $$ );

# exclude update to these files and folders
my @excludes = ( 'git-subtree', 'edgewater',
  'snapgear-3.3.0/lib/rtk_voip2/.snapgear.subtree',
  'snapgear-3.3.0/lib/rtk_voip2/maserati_2.0',
  'snapgear-3.3.0/lib/rtk_voip2/maserati_2.1',
  'snapgear-3.3.0/lib/rtk_voip2/solar_1.5',
  'snapgear-3.3.0/lib/rtk_voip2/solar_1.6',
  'snapgear-3.3.0/linux-3.18.20.x/.snapgear.subtree'
);


my $debug = 1;
my $dryrun = $ENV{'dryrun'};
my $version = Git::command_oneline('version');

print "$version\n" if ($debug == 1);

my $num_args = $#ARGV + 1;
die "\nUsage: patch-e30.pl <base> <head> <path>" .
    "\n\nPlease note change of <base> itself is not included!" .
    "\n\n" if ($num_args != 3);

die "\n$ARGV[2] not exist!\n" if (! -d "$ARGV[2]");
my $patchTo = $ARGV[2];
$patchTo .= '/' if (substr($patchTo, -1) ne '/');
die "\n'$patchTo' is not a git repository!\n" if (! -e "$patchTo.git");
print "Patch to: '$patchTo'\n";

print "\n========== dryrun ==========\n\n" if ($dryrun);

my $src_repo = Git->repository();
my $dst_repo = Git->repository( Directory => "$patchTo" );

check_e30_status( $patchTo, $dst_repo );

patch_tree( $ARGV[0], $ARGV[1], '', $ARGV[0], $ARGV[1] );

sub patch_tree( $$$$$ )
{
  my $base = $_[0];
  my $head = $_[1];
  my $root = $_[2];
  my $commit_base = $_[3];
  my $commit_head = $_[4];

  my @tree = $src_repo->command( 'diff-tree', '--no-renames', '--ignore-submodules', $base, $head );

  $root .= '/' if ($root ne '' && substr($root, -1) ne '/');

  foreach my $line (@tree)
  {
    my ($mod1, $mod2, $hash1, $hash2, $type_name) = split( / /, $line );
    my ($type, $name) = split( /\t/, $type_name );

    my $mod_tree = 0;
    my $hash_type = '----';
    my $status = '_';

    given ($type)
    {
    when (/M/) {
        $hash_type = hash_type( $src_repo, $hash1 );

        if (any { $_ eq "$root$name" } @excludes)
        {
          $status = 'exclude';
        }
        elsif ($hash_type eq "tree")
        {
          create_tree( "$patchTo$root$name" );
          patch_tree( $hash1, $hash2, "$root$name", $commit_base, $commit_head );
        }
        elsif ($hash_type eq "blob")
        {
          if (-f "$patchTo$root$name")
          {
            $status = merge_file( "$root$name", $patchTo, $dst_repo, $commit_base, $commit_head );
          }
          else
          {
            $status = copy_file( $root, $name, $patchTo, $dst_repo, $head, $hash2 );
          }
        }
        else
        {
          die "Unknow type: $hash1 $hash_type $root$name\n";
        }
      }
    when (/A/) {
        $hash_type = hash_type( $src_repo, $hash2 );

        if (any { $_ eq "$root$name" } @excludes)
        {
          $status = 'exclude';
        }
        elsif ($hash_type eq "tree")
        {
          $status = add_tree( "$root$name", $patchTo, $dst_repo, $hash2 );
        }
        elsif ($hash_type eq "blob")
        {
          $status = add_file( $root, $name, $patchTo, $dst_repo, $head, $hash2 );
        }
        else
        {
          die "Unknow type: $hash1 $hash_type $root$name\n";
       	}
      }
    when (/D/) { 
        $hash_type = hash_type( $src_repo, $hash1 );

        if (any { $_ eq "$root$name" } @excludes)
        {
          $status = 'exclude';
        }
        elsif ($hash_type eq "tree")
        {
          $status = del_tree( $patchTo, "$root$name", $dst_repo );
        }
        elsif ($hash_type eq "blob")
        {
          $status = del_file( $patchTo, "$root$name", $dst_repo );
        }
        else
        {
          die "Unknow type: $hash1 $hash_type $root$name\n";
       	}
      }
    default { $status = 'ignore'; }
    }

    if ($debug == 1 && $status ne '_') 
    {
      if ($status ne '')
      {
        print "$type $hash_type $root$name [$status]\n";
      }
      else
      {
        print "$type $hash_type $root$name\n";
      }
    }
  }

  return 1;
}

sub hash_type( $$ )
{
  # remove trailing new line
  return substr( $_[0]->command('cat-file','-t', $_[1]), 0, -1 );
}

sub create_tree( $ )
{
  if ((! -d $_[0]) && (! $dryrun))
  {
    
    make_path( $_[0], {error => \my $err} );
    die "\nFailed to create $_[0]!\n" if (@$err);
  }
}

sub merge_file( $$$$$ )
{
  my $file = $_[0];
  my $path = $_[1];
  my $repo = $_[2];
  my $commit1 = $_[3];
  my $commit2 = $_[4];

  if ($dryrun ||
      system( "git diff --binary $commit1 $commit2 -- '$file' | (cd '$path'; git apply -3 --ignore-whitespace -)" ) == 0)
  {
    return 'merge';
  }

  return '';
}

sub copy_file( $$$$$$ )
{
  my $root = $_[0];
  my $name = $_[1];
  my $path = $_[2];
  my $repo = $_[3];
  my $tree = $_[4];
  my $blob = $_[5];

  my $file = "$root$name";
  my $prefix = '';

  $root .= '/' if ($root ne '' && substr($root, -1) ne '/');
  $prefix = "--prefix='$root'" if ($root ne '');

  # 'git cat-file -p' doesn't work for symbolic link
  if ($dryrun ||
      system( "git archive $prefix $tree '$name' | (cd '$path'; tar -x)" ) == 0)
  {
    $repo->command( 'add', '--', $file ) if (! $dryrun);

    return 'copy';
  }

  return '';
}

sub add_tree( $$$$ )
{
  my $tree = $_[0];
  my $path = $_[1];
  my $repo = $_[2];
  my $hash = $_[3];

  die "\n$tree not exist !\n" if (! -d "$tree");
  
  $repo->command( 'rm', '-rf', '--', $tree ) if ((! $dryrun) && -e "$path$tree");

  if ($dryrun || system( "git archive --prefix='$tree/' $hash | (cd '$path'; tar -x)" ) == 0)
  {
    $repo->command( 'add', '--', $tree ) if (! $dryrun);
    return 'add';
  }

  return '';
}

sub add_file( $$$$$$ )
{
  my $root = $_[0];
  my $name = $_[1];
  my $path = $_[2];
  my $repo = $_[3];
  my $tree = $_[4];
  my $blob = $_[5];

  my $file = "$root$name";

  # A tree with the same name may exist! '-r' is requried!
  $repo->command( 'rm', '-rf', '--', $file ) if ((! $dryrun) && -e "$path$file");

  if (copy_file( $root, $name, $path, $repo, $tree, $blob ) eq 'copy')
  {
    return 'add';
  }

  return '';
}

# The destination tree may not exist because it is from the /usr/local/tar/archive.
sub del_tree( $$$ )
{
  my $path = $_[0];
  my $tree = $_[1];
  my $repo = $_[2];

  if (-d "$path$tree")
  {
# doesn't work for "snapgear-3.3.0/linux-3.18.20.x", why?
#    $repo->command( 'rm', '-r', '--', $tree ) if (! $dryrun);

    if ($dryrun || system( "cd '$path' && git rm -rfq -- '$tree'" ) == 0)
    {
      return 'del';
    }
  }

  return '';
}

# The destination file may not exist because it is from the /usr/local/tar/archive.
sub del_file( $$$ )
{
  my $path = $_[0];
  my $file = $_[1];
  my $repo = $_[2];

  if (-e "$path$file")
  {
    $repo->command( 'rm', '-f', '--', $file ) if (! $dryrun);
    return 'del';
  }

  return '';
}

sub check_item_status( $$ )
{
  my $item   = $_[0];
  my $status = $_[1];

  if ($status)
  {
    print "$item [done]\n";
  }
  else
  {
    die "$item [failed]\n";
  }
}

sub check_e30_status( $$ )
{
  my $path = $_[0];
  my $repo = $_[1];
  my $root;
  my $name;
  my $item;
  my $status;

  return if (! -d "${path}snapgear-3.3.0/linux-3.18.20.x/rtk_voip_package");

  print "\nMigrating files for subtree workflow...\n\n";

  # from 5039f52b0297c52ce1d405f6f01709182c3dde81

  $item = 'config/ewua/Makefile';
  $status = ($dryrun || merge_file( $item, $path, $repo,
                            '5039f52b0297c52ce1d405f6f01709182c3dde81^', '5039f52b0297c52ce1d405f6f01709182c3dde81' ) eq 'merge');
  check_item_status( $item, $status );

  $item = 'config/voice/Makefile';
  $status = ($dryrun || merge_file( $item, $path, $repo,
                            '5039f52b0297c52ce1d405f6f01709182c3dde81^', '5039f52b0297c52ce1d405f6f01709182c3dde81' ) eq 'merge');
  check_item_status( $item, $status );

  $item = 'snapgear-3.3.0/linux-3.18.20.x/drivers/Kconfig';
  $status = ($dryrun || del_file( $path, $item, $repo ) eq 'del');
  check_item_status( $item, $status );

  $item = 'snapgear-3.3.0/linux-3.18.20.x/drivers/Makefile';
  $status = ($dryrun || del_file( $path, $item, $repo ) eq 'del');
  check_item_status( $item, $status );

  $item = 'snapgear-3.3.0/linux-3.18.20.x/rtk_autoconf.h';
  $status = ($dryrun || del_file( $path, $item, $repo ) eq 'del');
  check_item_status( $item, $status );

  $item = 'snapgear-3.3.0/linux-3.18.20.x/rtk_voip_package';
  $status = ($dryrun || del_tree( $path, $item, $repo ) eq 'del');
  check_item_status( $item, $status );

  $root = 'snapgear-3.3.0/linux-3.18.20.x/';
  $name = 'Makefile';
  $item = "$root$name";
  $status = ($dryrun || copy_file( $root, $name, $path, $repo,
                            'ab8d9b5f660227fc977e1f57682244f04e466935', '231ada5ff6f7fa653128ddea4a7ed41f744d2e22' ) eq 'copy');
  check_item_status( $item, $status );

  $root = 'snapgear-3.3.0/linux-3.18.20.x/drivers/char/';
  $name = 'aipc';
  $item = "$root$name";
  $status = ($dryrun || add_file( $root, $name, $path, $repo,
                            '1720ed2178bc82bbd652ad772962d29451f29bd3', '4df0455140c1cdaa9e910ec501ba9b56bd4e8b4f' ) eq 'add');
  check_item_status( $item, $status );

  $root = 'snapgear-3.3.0/linux-3.18.20.x/';
  $name = 'rtk_voip';
  $item = "$root$name";
  $status = ($dryrun || add_file( $root, $name, $path, $repo,
                            'ab8d9b5f660227fc977e1f57682244f04e466935', '64edae9a0d2dcb3071b82addaf2767155afa493b' ) eq 'add');
  check_item_status( $item, $status );

  $item = 'snapgear-3.3.0/user/aipc_util';
  $status = ($dryrun || add_tree( $item, $path, $repo, '1d3a673d113304d35d17cde5794c7ca4c0222cec'  ) eq 'add');
  check_item_status( $item, $status );

=rem
  $item = 'snapgear-3.3.0/vendors/Edgewater/E250V2';
  $status = ($dryrun || patch_tree( 'b71ad9d5290062fecc84d747674ccba2fbfaac87', '0afcc71b80e5d330eab3141eba3c4de11a5171e4', $item,
                                    '5039f52b0297c52ce1d405f6f01709182c3dde81^', '5039f52b0297c52ce1d405f6f01709182c3dde81' ));
  check_item_status( $item, $status );
=cut

  # from 8400acf4eed2d1b10a309e23965da770a7a5159f

  $item = 'snapgear-3.3.0/lib/rtk_voip2';
  $status = ($dryrun || add_tree( $item, $path, $repo, 'e0cd072d8049ce4e79f851949230f6e45cc7eeba' ) eq 'add');
  check_item_status( $item, $status );

  $item = 'snapgear-3.3.0/lib/rtk_voip2/.snapgear.subtree';
  $status = ($dryrun || del_file( $path, $item, $repo ) eq 'del');
  check_item_status( $item, $status );

  $root = 'snapgear-3.3.0/vendors/Edgewater/E250V2/';
  $name = 'config.arch';
  $item = "$root$name";
  $status = ($dryrun || add_file( $root, $name, $path, $repo,
                            '68b812f7158094d7e341cffbfd707144b84e730e', 'c1d8508aceee84ece194fdf62788e56d2cf5645d' ) eq 'add');
  check_item_status( $item, $status );

  $root = 'snapgear-3.3.0/vendors/Edgewater/E250V2/';
  $name = 'config.linux-3.18.20.x';
  $item = "$root$name";
  $status = ($dryrun || add_file( $root, $name, $path, $repo,
                            '68b812f7158094d7e341cffbfd707144b84e730e', '6b544eb8567d469147424f88f2514b4b22e07350' ) eq 'add');
  check_item_status( $item, $status );

  $root = 'snapgear-3.3.0/vendors/Edgewater/E250V2/';
  $name = 'rtk_autoconf.h';
  $item = "$root$name";
  $status = ($dryrun || add_file( $root, $name, $path, $repo,
                            '68b812f7158094d7e341cffbfd707144b84e730e', 'fba6ce93cc379acaebbe697c70c703e71a531b31' ) eq 'add');
  check_item_status( $item, $status );

  $repo->command( 'commit', '-m', 'NA: E250V2, move linux-3.18.20.x/rtk_voip_package to lib/rtk_voip2 and keep original layout from Realtek SDK' );

  print "\nMigration done.\n\n";
}
