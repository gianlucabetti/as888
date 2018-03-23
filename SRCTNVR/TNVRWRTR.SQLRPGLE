000100000601     H DECEDIT('0,') DATEDIT(*DMY.)
000200000601      *
000300170526     FFnart01L  IF   E           K DISK
000400170526     FFNARB01L  IF   E           K DISK
000500170526     Fwfarw01L  IF a E           K DISK
000600170526     Fwrt_NCDDOPiF   E           K DISK    rename(WRT_NCDDOP:dop000)
000700170526     f                                     prefix(d_)
000800170526     Fwrt_NCDERRiF   E           K DISK    rename(WRT_NCDERR:err000)
000900170526     FQsysprt   O    F  132        printer oflind(*inof)
001000000602      *--------------------------------------------------
001100150126     D WLBDAT          DS                  INZ
001200150126     D  G02DAT                 1      8  0
001300150126     D  G02INV                 9     16  0
001400150126     D  G02ERR                17     17
001500150126     D  G02TGI                18     22  0
001600150126     D WDAT8           DS
001700150126     D  DATADA                 1      8  0
001800150126     D  DATAA                  9     16  0
001900150126     D  GGL                   17     21  0
002000150126
002100170526     D dsart         E DS                  EXTNAME(fnart00f)
002200170526     d segnac          s              7  0
002300150126      *--------------------------------------------------
002400120111     c     karb          klist
002500150126     c                   kfld                    artaas
002600150126     c                   kfld                    artlnp
002700150126     c                   kfld                    artnrs
002800150126     c                   kfld                    artnsp
002900150127
003000120111     c                   clear                   conta             7 0
003100170526     c                   except    testa
003200021216      *
003300170526     C                   read      wrt_ncderr
003400170526    1c                   DOW       not %EOF(wrt_ncderr)
003500170526     c                   add       1             conta
003600170526     c                   clear                   salta             1
003700170526     c* escluso se il segnacollo è in DOP
003800170526     C/EXEC SQL
003900170526     C+ DECLARE dop   CURSOR FOR
004000170526     C+ SELECT wrt_ncddop.ncd from wrt_ncddop
004100170526     C+ WHERE  wrt_ncddop.ncd =:ncd
004200170526     C+ FOR READ ONLY
004300170526     C/END-EXEC
004400170526     C
004500170526     C/EXEC SQL
004600170526     C+ OPEN dop
004700170526     C/END-EXEC
004800170526    2C                   DOU       SqlCod <> 0
004900170526     c
005000170526     C/EXEC SQL
005100170526     C+ FETCH NEXT FROM dop   INTO :segnac
005200170526     C/END-EXEC
005300170526    3C                   if        SqlCod = 100
005400170526     c                   leave
005500170526   x3C                   else
005600170526     c* se trovo il segnacollo stampo e devo saltare il segnacollo
005700170526     c                   except    s_dop
005800170526     c                   eval      salta='S'
005900170526    3c                   endif
006000170526    2c                   enddo
006100170526     c
006200170526     C/EXEC SQL
006300170526     C+ CLOSE dop
006400170526     C/END-EXEC
006500170526   1ac                   if        salta=' '
006600150121     c*
006700150126     C/EXEC SQL
006800150126     C+ DECLARE fnart CURSOR FOR
006900170526     C+ SELECT * from fnart00f
007000170526     C+ WHERE artnrs in(40, 41) and artnsc=:ncd
007100150126     C+ FOR READ ONLY
007200150126     C/END-EXEC
007300150126     C
007400150126     C/EXEC SQL
007500150126     C+ OPEN fnart
007600150126     C/END-EXEC
007700170526    2C                   DOU       SqlCod <> 0
007800150126     C/EXEC SQL
007900150126     C+ FETCH NEXT FROM fnart INTO :dsart
008000150126     C/END-EXEC
008100170526    3C                   if        SqlCod = 100
008200150126     c                   leave
008300170526   x3C                   else
008400150126     c* leggo fnarb
008500170526     c                   clear                   w88               2
008600150126     c     karb          chain     fnarb01l
008700170526    4c                   if        %found(fnarb01l)
008800170530     c                   if        (artnpp=90 or artnpp=88) and (artdam=0 or
008900170526     c                             artdam>0 and artnap=85)
009000170530     c                   if        artnpp=90
009100170530     c                   eval      w88='90'
009200170530     c                   else
009300170530     c                   eval      w88='88'
009400170530     c                   endif
009500170526     c* Scrivo il file
009600170526     c                   clear                   wfarw000
009700170526     c                   eval      arwfile='E'
009800170526     c                   eval      arwaas =arbaas
009900170526     c                   eval      arwlnp =arblnp
010000170526     c                   eval      arwnrs =arbnrs
010100170526     c                   eval      arwnsp =arbnsp
010200170526     c                   eval      arwrmn =arbrmn
010300170526     c                   eval      arwrma =arbrma
010400170526     c                   eval      arwmgs =arbmgs
010500170526     c                   eval      arwlna =arblna
010600170526     c                   eval      arwncl =arbncl
010700170526     c                   eval      arwdcp =arbdcp
010800170526     c                   eval      arwdcm =arbdcm
010900170526     c                   eval      arwddc =arbddc
011000170526     c                   eval      arwndc =arbndc
011100170526     c                   eval      arww88 =w88
011200170526     c                   eval      arw_encd= ncd
011300170526     c                   eval      arw_ermn= rmn
011400170526     c                   write     wfarw000
011500170526     c                   endif
011600170526     c
011700170526     c                   except    det
011800021213      *
011900170526    4c                   endif
012000170526    3c                   endif
012100170526    2c                   enddo
012200170526     C/EXEC SQL
012300170526     C+ CLOSE fnart
012400170526     C/END-EXEC
012500170526   1ac                   endif
012600150126
012700170526     C                   read      wrt_ncderr
012800120111    1c                   ENDDO
012900150127
013000021213      *
013100021213     C                   eval       *inlr = *on
013200170526     OQsysPrt   E            Testa       1
013300170526     o                                        +   0 'Bolle WURTH con segnac.err'
013400170526     O          E            det         1
013500170526     o                       conta         z
013600170526     o                                        +   0 ')'
013700170526     o                       arbaas           +   1
013800170526     o                       arblnp           +   1
013900170526     o                       arbnrs           +   1
014000170526     o                       arbnsp        z  +   1
014100170526     o                       arbncl        z  +   1
014200170526     o                       artnsc        z  +   1
014300170526     o                       w88              +   1
014400170526     O          E            s_dop       1
014500170526     o                       conta         z
014600170526     o                                        +   0 ')'
014700170526     o                       NCD           z  +   1
014800170526     o                                        +   1 'Segnacollo in DOP'
