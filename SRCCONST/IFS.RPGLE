000100000000      **********************************************************************
000200000000      * Some CCSID definitions that I've found useful
000300000000      **********************************************************************
000400000000     D CP_MSDOS        C                   437
000500000000     D CP_ISO8859_1    C                   819
000600000000     D CP_WINDOWS      C                   1252
000700000000     D CP_UTF8         C                   1208
000800000000     D CP_UCS2         C                   1200
000900000000     D CP_CURJOB       C                   0
001000110113     D CP_ITALIAN      C                   1144
001100110113
001200000000      **********************************************************************
001300000000      *  Flags for use in open()
001400000000      *
001500000000      * More than one can be used -- add them together.
001600000000      **********************************************************************
001700000000      *                                            Reading Only
001800000000     D O_RDONLY        C                   1
001900000000      *                                            Writing Only
002000000000     D O_WRONLY        C                   2
002100000000      *                                            Reading & Writing
002200000000     D O_RDWR          C                   4
002300000000      *                                            Create File if it does
002400000000      *                                              not exist.
002500000000     D O_CREAT         C                   8
002600000000      *                                            Exclusively create --
002700000000      *                                              open will fail if it
002800000000      *                                              already exists.
002900000000     D O_EXCL          C                   16
003000000000      *                                            Assign a CCSID
003100000000     D O_CCSID         C                   32
003200000000      *                                            Truncate File to 0 bytes
003300000000     D O_TRUNC         C                   64
003400000000      *                                            Append to File
003500000000     D O_APPEND        C                   256
003600000000      *                                            Synchronous write
003700000000     D O_SYNC          C                   1024
003800000000      *                                            Sync write, data only
003900000000     D O_DSYNC         C                   2048
004000000000      *                                            Sync read
004100000000     D O_RSYNC         C                   4096
004200000000      *                                            No controlling terminal
004300000000     D O_NOCTTY        C                   32768
004400000000      *                                            Share with readers only
004500000000     D O_SHARE_RDONLY  C                   65536
004600000000      *                                            Share with writers only
004700000000     D O_SHARE_WRONLY  C                   131072
004800000000      *                                            Share with read & write
004900000000     D O_SHARE_RDWR    C                   262144
005000000000      *                                            Share with nobody.
005100000000     D O_SHARE_NONE    C                   524288
005200000000      *                                            Assign a code page
005300000000     D O_CODEPAGE      C                   8388608
005400000000      *                                            Open in text-mode
005500000000     D O_TEXTDATA      C                   16777216
005600000000      /if defined(*V5R2M0)
005700000000      *                                            Allow text translation
005800000000      *                                            on newly created file.
005900000000      * Note: O_TEXT_CREAT requires all of the following flags to work:
006000000000      *           O_CREAT+O_TEXTDATA+(O_CODEPAGE or O_CCSID)
006100000000     D O_TEXT_CREAT    C                   33554432
006200000000      /endif
006300000000      *                                            Inherit mode from dir
006400000000     D O_INHERITMODE   C                   134217728
006500000000      *                                            Large file access
006600000000      *                                            (for >2GB files)
006700000000     D O_LARGEFILE     C                   536870912
006800000000
006900000000      **********************************************************************
007000000000      * Access mode flags for access() and accessx()
007100000000      *
007200000000      *   F_OK = File Exists
007300000000      *   R_OK = Read Access
007400000000      *   W_OK = Write Access
007500000000      *   X_OK = Execute or Search
007600000000      **********************************************************************
007700000000     D F_OK            C                   0
007800000000     D R_OK            C                   4
007900000000     D W_OK            C                   2
008000000000     D X_OK            C                   1
008100000000
008200000000      **********************************************************************
008300000000      * class of users flags for accessx()
008400000000      *
008500000000      *   ACC_SELF = Check access based on effective uid/gid
008600000000      *   ACC_INVOKER = Check access based on real uid/gid
008700000000      *                 ( this is equvalent to calling access() )
008800000000      *   ACC_OTHERS = Check access of someone not the owner
008900000000      *   ACC_ALL = Check access of all users
009000000000      **********************************************************************
009100000000     D ACC_SELF        C                   0
009200000000     D ACC_INVOKER     C                   1
009300000000     D ACC_OTHERS      C                   8
009400000000     D ACC_ALL         C                   32
009500000000
009600000000      **********************************************************************
009700000000      *      Mode Flags.
009800000000      *         basically, the mode parm of open(), creat(), chmod(),etc
009900000000      *         uses 9 least significant bits to determine the
010000000000      *         file's mode. (peoples access rights to the file)
010100000000      *
010200000000      *           user:       owner    group    other
010300000000      *           access:     R W X    R W X    R W X
010400000000      *           bit:        8 7 6    5 4 3    2 1 0
010500000000      *
010600000000      * (This is accomplished by adding the flags below to get the mode)
010700000000      **********************************************************************
010800000000      *                                         owner authority
010900000000     D S_IRUSR         C                   256
011000000000     D S_IWUSR         C                   128
011100000000     D S_IXUSR         C                   64
011200000000     D S_IRWXU         C                   448
011300000000      *                                         group authority
011400000000     D S_IRGRP         C                   32
011500000000     D S_IWGRP         C                   16
011600000000     D S_IXGRP         C                   8
011700000000     D S_IRWXG         C                   56
011800000000      *                                         other people
011900000000     D S_IROTH         C                   4
012000000000     D S_IWOTH         C                   2
012100000000     D S_IXOTH         C                   1
012200000000     D S_IRWXO         C                   7
012300000000      *                                         special modes:
012400000000      *                                         Set effective GID
012500000000     D S_ISGID         C                   1024
012600000000      *                                         Set effective UID
012700000000     D S_ISUID         C                   2048
012800000000
012900000000      **********************************************************************
013000000000      * My own special MODE shortcuts for open() (instead of those above)
013100000000      **********************************************************************
013200000000     D M_RDONLY        C                   const(292)
013300000000     D M_RDWR          C                   const(438)
013400000000     D M_RWX           C                   const(511)
013500000000
013600000000      **********************************************************************
013700000000      * "whence" constants for use with seek(), lseek() and others
013800000000      **********************************************************************
013900000000     D SEEK_SET        C                   CONST(0)
014000000000     D SEEK_CUR        C                   CONST(1)
014100000000     D SEEK_END        C                   CONST(2)
014200000000
014300000000      **********************************************************************
014400000000      * flags specified in the f_flags element of the ds_statvfs
014500000000      *   data structure used by the statvfs() API
014600000000      **********************************************************************
014700000000     D ST_RDONLY...
014800000000     D                 C                   CONST(1)
014900000000     D ST_NOSUID...
015000000000     D                 C                   CONST(2)
015100000000     D ST_CASE_SENSITITIVE...
015200000000     D                 C                   CONST(4)
015300000000     D ST_CHOWN_RESTRICTED...
015400000000     D                 C                   CONST(8)
015500000000     D ST_THREAD_SAFE...
015600000000     D                 C                   CONST(16)
015700000000     D ST_DYNAMIC_MOUNT...
015800000000     D                 C                   CONST(32)
015900000000     D ST_NO_MOUNT_OVER...
016000000000     D                 C                   CONST(64)
016100000000     D ST_NO_EXPORTS...
016200000000     D                 C                   CONST(128)
016300000000     D ST_SYNCHRONOUS...
016400000000     D                 C                   CONST(256)
016500000000
016600000000      **********************************************************************
016700000000      * Constants used by pathconf() API
016800000000      **********************************************************************
016900000000     D PC_CHOWN_RESTRICTED...
017000000000     D                 C                   0
017100000000     D PC_LINK_MAX...
017200000000     D                 C                   1
017300000000     D PC_MAX_CANON...
017400000000     D                 C                   2
017500000000     D PC_MAX_INPUT...
017600000000     D                 C                   3
017700000000     D PC_NAME_MAX...
017800000000     D                 C                   4
017900000000     D PC_NO_TRUNC...
018000000000     D                 C                   5
018100000000     D PC_PATH_MAX...
018200000000     D                 C                   6
018300000000     D PC_PIPE_BUF...
018400000000     D                 C                   7
018500000000     D PC_VDISABLE...
018600000000     D                 C                   8
018700000000     D PC_THREAD_SAFE...
018800000000     D                 C                   9
018900000000
019000000000      **********************************************************************
019100000000      * Constants used by sysconf() API
019200000000      **********************************************************************
019300000000     D SC_CLK_TCK...
019400000000     D                 C                   2
019500000000     D SC_NGROUPS_MAX...
019600000000     D                 C                   3
019700000000     D SC_OPEN_MAX...
019800000000     D                 C                   4
019900000000     D SC_STREAM_MAX...
020000000000     D                 C                   5
020100000000     D SC_CCSID...
020200000000     D                 C                   10
020300000000     D SC_PAGE_SIZE...
020400000000     D                 C                   11
020500000000     D SC_PAGESIZE...
020600000000     D                 C                   12
