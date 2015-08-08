# recdump
Erlang recods saving (and loading) to the text files with field names (not as tagged tuple)

## Overview

Typical Erlang record looks like this.

    #file_info{size = 456,type = regular,access = read_write,
               atime = {{2015,8,8},{14,47,14}},
               mtime = {{2015,8,8},{14,47,14}},
               ctime = {{2015,8,8},{14,22,14}},
               mode = 33206,links = 1,major_device = 3,minor_device = 0,
               inode = 0,uid = 0,gid = 0}


This record can be saved to the file in following format, which can be read
using file:consult/1.

    {file_info,456,regular,read_write,
               {{2015,8,8},{14,47,14}},
               {{2015,8,8},{14,47,14}},
               {{2015,8,8},{14,22,14}},
               33206,1,3,0,0,0,0}


Record dumper can save record above in another format. Which is more user friendly.

    {file_info,[{size,0},
                {type,regular},
                {access,read_write},
                {atime,{{2015,8,8},{14,22,14}}},
                {mtime,{{2015,8,8},{14,22,14}}},
                {ctime,{{2015,8,8},{14,22,14}}},
                {mode,33206},
                {links,1},
                {major_device,3},
                {minor_device,0},
                {inode,0},
                {uid,0},
                {gid,0}]}


## Example

Start application (there are some necessary ETS tables).

    1> recdump:start().
    ok

Prepare data.

    2> rr(file).
    [file_descriptor,file_info]

    3> {ok, Record} = file:read_file_info("README.md").
    {ok,#file_info{size = 456,type = regular,
                   access = read_write,
                   atime = {{2015,8,8},{14,47,14}},
                   mtime = {{2015,8,8},{14,47,14}},
                   ctime = {{2015,8,8},{14,22,14}},
                   mode = 33206,links = 1,major_device = 3,minor_device = 0,
                   inode = 0,uid = 0,gid = 0}}

Save the data to the file "rec.txt". Note parameter "file", which is a name of the
module, where are necessary records definitions (there could be list of modules).

    4> recdump:save(Record, "rec.txt", file).
    ok

Load data back and compare it with a original.

    5> {ok, Record2} = recdump:load("rec.txt", file).
    6> Record == Record2.
    true


See the content of the dump file.

    $ cat rec.txt
    {file_info,[{size,456},
                {type,regular},
                {access,read_write},
                {atime,{{2015,8,8},{14,47,14}}},
                {mtime,{{2015,8,8},{14,47,14}}},
                {ctime,{{2015,8,8},{14,22,14}}},
                {mode,33206},
                {links,1},
                {major_device,3},
                {minor_device,0},
                {inode,0},
                {uid,0},
                {gid,0}]}.

