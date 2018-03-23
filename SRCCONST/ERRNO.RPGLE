000100090326     ***************************************************************************
000200090326     ** Errno Values for UNIX-Type Functions
000300090326     ** Programs using the UNIX(R)-type functions may receive error information
000400090326     ** as errno values.
000500090326     ** http://publib.boulder.ibm.com/infocenter/iseries/v5r4/topic/apis/unix14.htm
000600090326     ***************************************************************************
000700081210     D EDOM            C                   3001                                 domain error in math
000800081210     D ERANGE          C                   3002                                 range error in math
000900081210     D ETRUNC          C                   3003                                 truncation on I/O op
001000081210     D ENOTOPEN        C                   3004                                 file has not been op
001100081210     D ENOTREAD        C                   3005                                 file not opened for
001200081210     D EIO             C                   3006                                 input/output error
001300081210     D ENODEV          C                   3007                                 no such device
001400081210     D ERECIO          C                   3008                                 file opened for reco
001500081210     D ENOTWRITE       C                   3009                                 file not opened for
001600081210     D ESTDIN          C                   3010                                 stdin cannot be open
001700081210     D ESTDOUT         C                   3011                                 stdout cannot be ope
001800081210     D ESTDERR         C                   3012                                 stderr cannot be ope
001900081210     D EBADSEEK        C                   3013                                 bad offset to seek t
002000081210     D EBADNAME        C                   3014                                 invalid file name sp
002100081210     D EBADMODE        C                   3015                                 invalid file mode sp
002200081210     D EBADPOS         C                   3017                                 invalid position spe
002300081210     D ENOPOS          C                   3018                                 no record at specifi
002400081210     D ENUMMBRS        C                   3019                                 no ftell if more tha
002500081210     D ENUMRECS        C                   3020                                 no ftell if too many
002600081210     D EINVAL          C                   3021                                 invalid argument
002700081210     D EBADFUNC        C                   3022                                 invalid function poi
002800081210     D ENOENT          C                   3025                                 no such file or libr
002900081210     D ENOREC          C                   3026                                 record not found
003000081210     D EPERM           C                   3027                                 operation not permit
003100081210     D EBADDATA        C                   3028                                 message data invalid
003200081210     D EBUSY           C                   3029                                 resource busy
003300081210     D EBADOPT         C                   3040                                 bad option on I/O fu
003400081210     D ENOTUPD         C                   3041                                 file not opened for
003500081210     D ENOTDLT         C                   3042                                 file not opened for
003600081210     D EPAD            C                   3043                                 padding occurred on
003700081210     D EBADKEYLN       C                   3044                                 bad key length optio
003800081210     D EPUTANDGET      C                   3080                                 illegal write after
003900081210     D EGETANDPUT      C                   3081                                 illegal read after w
004000081210     D EIOERROR        C                   3101                                 I/O exception non-re
004100081210     D EIORECERR       C                   3102                                 I/O exception recove
004200081210     D EACCES          C                   3401                                 Permission denied.
004300081210     D ENOTDIR         C                   3403                                 Not a directory.
004400081210     D ENOSPC          C                   3404                                 No space available.
004500081210     D EXDEV           C                   3405                                 Improper link.
004600081210     D EWOULDBLOCK     C                   3406                                 Operation would have
004700081210     D EAGAIN          C                   3406                                 Operation would have
004800081210     D EINTR           C                   3407                                 Interrupted function
004900081210     D EFAULT          C                   3408                                 The address used for
005000081210     D ETIME           C                   3409                                 Operation timed out
005100081210     D ENXIO           C                   3415                                 No such device or ad
005200081210     D ECLOSED         C                   3417                                 Socket closed
005300081210     D EADDRINUSE      C                   3420                                 Address already in u
005400081210     D EADDRNOTAVAIL   C                   3421                                 Address not availabl
005500081210     D EAFNOSUPPORT    C                   3422                                 The type of socket i
005600081210     D EALREADY        C                   3423                                 Operation already in
005700081210     D ECONNABORTED    C                   3424                                 Connection ended abn
005800081210     D ECONNREFUSED    C                   3425                                 A remote host refuse
005900081210     D ECONNRESET      C                   3426                                 A connection with a
006000081210     D EDESTADDRREQ    C                   3427                                 Operation requires d
006100081210     D EHOSTDOWN       C                   3428                                 A remote host is not
006200081210     D EHOSTUNREACH    C                   3429                                 A route to the remot
006300081210     D EINPROGRESS     C                   3430                                 Operation in progres
006400081210     D EISCONN         C                   3431                                 A connection has alr
006500081210     D EMSGSIZE        C                   3432                                 Message size out of
006600081210     D ENETDOWN        C                   3433                                 The network is not c
006700081210     D ENETRESET       C                   3434                                 A socket is connecte
006800081210     D ENETUNREACH     C                   3435                                 Cannot reach the des
006900081210     D ENOBUFS         C                   3436                                 There is not enough
007000081210     D ENOPROTOOPT     C                   3437                                 The protocol does no
007100081210     D ENOTCONN        C                   3438                                 Requested operation
007200081210     D ENOTSOCK        C                   3439                                 The specified descri
007300081210     D ENOTSUP         C                   3440                                 Operation not suppor
007400081210     D EOPNOTSUPP      C                   3440                                 Operation not suppor
007500081210     D EPFNOSUPPORT    C                   3441                                 The socket protocol
007600081210     D EPROTONOSUPPOR  C                   3442                                 No protocol of the s
007700081210     D EPROTOTYPE      C                   3443                                 The socket type or p
007800081210     D ERCVDERR        C                   3444                                 An error indication
007900081210     D ESHUTDOWN       C                   3445                                 Cannot send data aft
008000081210     D ESOCKTNOSUPPOR  C                   3446                                 The specified socket
008100081210     D ETIMEDOUT       C                   3447                                 A remote host did no
008200081210     D EUNATCH         C                   3448                                 The protocol require
008300081210     D EBADF           C                   3450                                 Descriptor not valid
008400081210     D EMFILE          C                   3452                                 Too many open files
008500081210     D ENFILE          C                   3453                                 Too many open files
008600081210     D EPIPE           C                   3455                                 Broken pipe.
008700081210     D EEXIST          C                   3457                                 File exists.
008800081210     D EDEADLK         C                   3459                                 Resource deadlock av
008900081210     D ENOMEM          C                   3460                                 Storage allocation r
009000081210     D EOWNERTERM      C                   3462                                 The synchronization
009100081210     D EDESTROYED      C                   3463                                 The synchronization
009200081210     D ETERM           C                   3464                                 Operation terminated
009300081210     D EMLINK          C                   3468                                 Maximum link count f
009400081210     D ESPIPE          C                   3469                                 Seek request not sup
009500081210     D ENOSYS          C                   3470                                 Function not impleme
009600081210     D EISDIR          C                   3471                                 Specified target is
009700081210     D EROFS           C                   3472                                 Read-only file syste
009800081210     D EUNKNOWN        C                   3474                                 Unknown system state
009900081210     D EITERBAD        C                   3475                                 Iterator is invalid.
010000081210     D EDAMAGE         C                   3484                                 A damaged object was
010100081210     D ELOOP           C                   3485                                 A loop exists in the
010200081210     D ENAMETOOLONG    C                   3486                                 A path name is too l
010300081210     D ENOLCK          C                   3487                                 No locks available
010400081210     D ENOTEMPTY       C                   3488                                 Directory not empty.
010500081210     D ENOSYSRSC       C                   3489                                 System resources not
010600081210     D ECONVERT        C                   3490                                 Conversion error.
010700081210     D E2BIG           C                   3491                                 Argument list too lo
010800081210     D EILSEQ          C                   3492                                 Conversion stopped d
010900081210     D ESOFTDAMAGE     C                   3497                                 Object has soft dama
011000081210     D ENOTENROLL      C                   3498                                 User not enrolled in
011100081210     D EOFFLINE        C                   3499                                 Object is suspended.
011200081210     D EROOBJ          C                   3500                                 Object is a read onl
011300081210     D ELOCKED         C                   3506                                 Area being read from
011400081210     D EFBIG           C                   3507                                 Object too large.
011500081210     D EIDRM           C                   3509                                 The semaphore, share
011600081210     D ENOMSG          C                   3510                                 The queue does not c
011700081210     D EFILECVT        C                   3511                                 File ID conversion o
011800081210     D EBADFID         C                   3512                                 A File ID could not
011900081210     D ESTALE          C                   3513                                 A File ID could not
012000081210     D ESRCH           C                   3515                                 No such process.
012100081210     D ENOTSIGINIT     C                   3516                                 Process not enabled
012200081210     D ECHILD          C                   3517                                 No child process.
012300081210     D ETOOMANYREFS    C                   3523                                 The operation would
012400081210     D ENOTSAFE        C                   3524                                 Function not allowed
012500081210     D EOVERFLOW       C                   3525                                 Object is too large
012600081210     D EJRNDAMAGE      C                   3526                                 Journal damaged.
012700081210     D EJRNINACTIVE    C                   3527                                 Journal inactive.
012800081210     D EJRNRCVSPC      C                   3528                                 Journal space or sys
012900081210     D EJRNRMT         C                   3529                                 Journal is remote.
013000081210     D ENEWJRNRCV      C                   3530                                 New journal receiver
013100081210     D ENEWJRN         C                   3531                                 New journal is neede
013200081210     D EJOURNALED      C                   3532                                 Object already journ
013300081210     D EJRNENTTOOLONG  C                   3533                                 Entry too large to s
013400081210     D EDATALINK       C                   3534                                 Object is a Datalink
013500081210     D ENOTAVAIL       C                   3535                                 Independent ASP is n
013600081210     D ENOTTY          C                   3536                                 Inappropriate IO con
013700081210     D EFBIG2          C                   3540                                 Write or truncate pa
013800081210     D ETXTBSY         C                   3543                                 Text file busy
013900081210     D EASPGRPNOTSET   C                   3544                                 ASP group not set fo
014000081210     D ERESTART        C                   3545                                 System call can be r
014100081210     D ESCANFAILURE    C                   3546                                 Object marked as sca
