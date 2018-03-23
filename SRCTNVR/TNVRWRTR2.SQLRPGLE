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
003100170530     c                   clear                   savncd            7 0
003200170526     c                   except    testa
003300021216      *
003400170526     C                   read      wrt_ncderr
003500170526    1c                   DOW       not %EOF(wrt_ncderr)
003600170526     c                   add       1             conta
003700170526     c                   clear                   salta             1
003800170530     c* se il swegnacollo è = al precedente, lo salto
003900170530   1ac                   if        ncd<>savncd
004000170530     c                   eval      savncd=ncd
004100170526     c* escluso se il segnacollo è in DOP
004200170526     C/EXEC SQL
004300170526     C+ DECLARE dop   CURSOR FOR
004400170526     C+ SELECT wrt_ncddop.ncd from wrt_ncddop
004500170526     C+ WHERE  wrt_ncddop.ncd =:ncd
004600170526     C+ FOR READ ONLY
004700170526     C/END-EXEC
004800170526     C
004900170526     C/EXEC SQL
005000170526     C+ OPEN dop
005100170526     C/END-EXEC
005200170526    2C                   DOU       SqlCod <> 0
005300170526     c
005400170526     C/EXEC SQL
005500170526     C+ FETCH NEXT FROM dop   INTO :segnac
005600170526     C/END-EXEC
005700170526    3C                   if        SqlCod = 100
005800170526     c                   leave
005900170526   x3C                   else
006000170529     c* se trovo il segnacollo lo elaboro
006100170529     c                   eval      salta='N'
006200170526    3c                   endif
006300170526    2c                   enddo
006400170526     c
006500170526     C/EXEC SQL
006600170526     C+ CLOSE dop
006700170526     C/END-EXEC
006800170529   1ac                   if        salta='N'
006900150121     c*
007000150126     C/EXEC SQL
007100150126     C+ DECLARE fnart CURSOR FOR
007200170526     C+ SELECT * from fnart00f
007300170526     C+ WHERE artnrs in(40, 41) and artnsc=:ncd
007400150126     C+ FOR READ ONLY
007500150126     C/END-EXEC
007600150126     C
007700150126     C/EXEC SQL
007800150126     C+ OPEN fnart
007900150126     C/END-EXEC
008000170526    2C                   DOU       SqlCod <> 0
008100150126     C/EXEC SQL
008200150126     C+ FETCH NEXT FROM fnart INTO :dsart
008300150126     C/END-EXEC
008400170526    3C                   if        SqlCod = 100
008500150126     c                   leave
008600170526   x3C                   else
008700150126     c* leggo fnarb
008800170526     c                   clear                   w88               2
008900150126     c     karb          chain     fnarb01l
009000170526    4c                   if        %found(fnarb01l)
009100170530     c                   if        (artnpp=90 or artnpp=88) and (artdam=0 or
009200170526     c                             artdam>0 and artnap=85)
009300170530     c                   if        artnpp=90
009400170530     c                   eval      w88='D9'
009500170530     c                   else
009600170530     c                   eval      w88='D8'
009700170530     c                   endif
009800170526     c* Scrivo il file
009900170526     c                   clear                   wfarw000
010000170526     c                   eval      arwfile='E'
010100170526     c                   eval      arwaas =arbaas
010200170526     c                   eval      arwlnp =arblnp
010300170526     c                   eval      arwnrs =arbnrs
010400170526     c                   eval      arwnsp =arbnsp
010500170526     c                   eval      arwrmn =arbrmn
010600170526     c                   eval      arwrma =arbrma
010700170526     c                   eval      arwmgs =arbmgs
010800170526     c                   eval      arwlna =arblna
010900170526     c                   eval      arwncl =arbncl
011000170526     c                   eval      arwdcp =arbdcp
011100170526     c                   eval      arwdcm =arbdcm
011200170526     c                   eval      arwddc =arbddc
011300170526     c                   eval      arwndc =arbndc
011400170526     c                   eval      arww88 =w88
011500170526     c                   eval      arw_encd= ncd
011600170526     c                   eval      arw_ermn= rmn
011700170526     c                   write     wfarw000
011800170526     c                   endif
011900170526     c
012000170526     c                   except    det
012100021213      *
012200170526    4c                   endif
012300170526    3c                   endif
012400170526    2c                   enddo
012500170526     C/EXEC SQL
012600170526     C+ CLOSE fnart
012700170526     C/END-EXEC
012800170526   1ac                   endif
012900170530   1ac                   endif
013000150126
013100170526     C                   read      wrt_ncderr
013200120111    1c                   ENDDO
013300150127
013400021213      *
013500021213     C                   eval       *inlr = *on
013600170526     OQsysPrt   E            Testa       1
013700170526     o                                        +   0 'Bolle WURTH con segnac.err'
013800170526     O          E            det         1
013900170526     o                       conta         z
014000170526     o                                        +   0 ')'
014100170526     o                       arbaas           +   1
014200170526     o                       arblnp           +   1
014300170526     o                       arbnrs           +   1
014400170526     o                       arbnsp        z  +   1
014500170526     o                       arbncl        z  +   1
014600170526     o                       artnsc        z  +   1
014700170526     o                       w88              +   1
014800170526     O          E            s_dop       1
014900170526     o                       conta         z
015000170526     o                                        +   0 ')'
015100170526     o                       NCD           z  +   1
015200170526     o                                        +   1 'Segnacollo in DOP'
