000100011003     H DECEDIT('0,') DATEDIT(*DMY.)
000200121005     H*
000300121005     FTABEL00F  IF   E           K DISK
000400121005     FAZORG01L  IF   E           K DISK
000500121005     Fcnaco00f  IF   E           K DISK
000600121005     Ftas_f7    if   E             DISK    rename(tas_f7:titas0f7)
000700121003     F                                     prefix(tf7:3)
000800121003     FTITAS30c  if   E           k DISK
000900121005     FTITAA30c  if   E           k DISK
001000121005     FTITAScsF  if A E             DISK
001100121003     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200121005     d dsQt1         e ds
001300091117     D KPJBA         E DS
001400121005     d wTara           s                   Like(§QtTpc)
001500121005     d wNtara          s                   Like(TasPkf)
001600121005     D KKUT            S                   LIKE(TBLkut) inz(1)
001700121005     D KCOD            S                   LIKE(TBLcod)
001800121005     D KKEY            S                   LIKE(TBLkey)
001801121005     D scarto          S              7  0
001802121005     D contat          S              7  0
001803121005     D conta102        S              7  0
001804121005     D conta           S              7  0
001900121005
002000011003     C*------------------------------------------------------------------------*
002100121005     c**/exec sql
002200121005     c**+  delete from titascsf
002300121005     c**/end-exec
002400121005     c
002500091117     c     *entry        plist
002600091117     c                   parm                    kpjba
002700121003     c
002800121005     C     keytab        KLIST
002900121005     C                   KFLD                    KKUT
003000121005     C                   KFLD                    KCOD
003100121005     C                   KFLD                    KKEY
003200121005      *
003300121003     c     ktaf7         klist
003400121003     c                   kfld                    tf7aas
003500121003     c                   kfld                    tf7lnp
003600121003     c                   kfld                    tf7nrs
003700121003     c                   kfld                    tf7nsp
003800121005     c
003900121005     c     kaco          klist
004000121005     c                   kfld                    kkut
004100121005     c                   kfld                    kcc               4 0
004200121005     c                   kfld                    tasksc
004300121005     c                   eval      kcc=151
004301121005     c                   clear                   conta
004400121003     c* leggo titas
004500121003     c                   read      titas0f7
004600121005    1c                   dow       not %eof(tas_f7)
004700121005     c*
004800121005    2c                   if        Tf7lod='NOlegata  NOGIAC. SENZA CO.ANOM'
004900121005     c
005000121005     c***                if        tf7tbl='F7' and tf7cbo='A'
005001121005    3c                   if        tf7tbl='F7'
005100121005     c                   clear                   titascs0
005101121005     c
005102121005   3ac                   if        tf7lnp=102
005103121005     c                   eval      conta102=conta102+1
005104121005  x3ac                   else
005200121005     c
005300121005     c     ktaf7         chain     titas30c
005400121005    4c                   if        %found(titas30c)
005500121005     c
005600121005      * Prendo il peso fatturato
005700121005    5c                   If        TasFpf = 'R'
005800121005     c                   Z-Add     TasPkf        taspkg
005900121005    5c                   EndIf
006000121005      * Prendo il peso da cml - la tara
006100121005If  5c                   If        TasFpf = 'V'
006200121005     c                   Clear                   dsQt1
006300121005     c                   Movel     'QT'          Kcod
006400121005     c                   Movel(P)  1             Kkey
006500121005     c     KeyTab        Chain     Tabel00f
006600121005    6c                   If        %Found(Tabel00f) and TblFlg = *Blanks
006700121005     c                   Movel     TblUni        dsQt1
006800121005    6c                   EndIf
006900121005     c     §QtTpc        Mult      TasNcp        wTara
007000121005     c     TasPkc        Sub(h)    wTara         wNtara
007100121005     c                   Z-Add     wNtara        taspkg
007200121005    5c                   EndIf
007300121005      *
007400121005     C* IMPOSTO FILIALE KSC
007500121005     C                   MOVEL     TASKSC        TASKFIL
007600121005     C     TASKFIL       CHAIN     AZORG01L
007700121005    5C                   IF        %FOUND(AZORG01L)
007800121005     c                   movel     orgdes        taskdfil
007900121005    5c                   endif
008000121005     c
008100121005     c                   eval      taskcar=orgcar
008200121005     c                   eval      taskdiv=orgfl3
008300121005     c                   Movel     '17'          Kcod
008400121005     c                   Movel(P)  taskdiv       Kkey
008500121005     c     KeyTab        Chain     Tabel00f
008600121005    5c                   If        %Found(Tabel00f) and TblFlg = *Blanks
008700121005     c                   movel     tbluni        taskddiv
008800121005    5c                   endif
008900121005     c                   Movel     '05'          Kcod
009000121005     c                   Movel(P)  taskcar       Kkey
009100121005     c     KeyTab        Chain     Tabel00f
009200121005    5c                   If        %Found(Tabel00f) and TblFlg = *Blanks
009300121005     c                   movel     tbluni        taskdcar
009400121005    5c                   endif
009500121005     c
009600121005     c                   move      tasksc        w0040             4 0
009700121005    5c                   if        w0040=8888
009800121005     c     ktaf7         chain     titaa30c
009900121005    6C                   IF        %FOUND(titaa30c)
010000121005     c                   movel     taarsc        tasrsm
010100121005    6c                   endif
010200121005     c
010300121005     c                   else
010400121005     c     kaco          chain     cnaco00f
010500121005    6C                   IF        %FOUND(cnaco00f)
010600121005     c                   movel     acorag        tasrsm
010700121005    6c                   endif
010800121005    5c                   endif
010900121005     c
010902121005     c                   eval      conta=conta+1
010903121005     c
010904121005     c                   except    bolla
010905121005     c
010906121005     c                   if        %subst(kpjbu:1:1)='S'
011000121005     c                   write     titascs0
011100121005    4c                   endif
011101121005     c
011102121005    4c                   endif
011103121005   3ac                   endif
011104121005   x3c**                 else
011105121005     c*
011106121005     c**                 except    noA
011107121005     c***                eval      scarto=scarto+1
011200121005    3c                   endif
011300121005    2c                   endif
011400121003     c
011600121003     c
011700121003     c                   read      titas0f7
011800121005    1c                   enddo
011900121003     c
011901121005     c                   eval      contat=conta+conta102+scarto
011902121005     c                   except    tot
012000091117     c                   seton                                        lr
012100121003     OQSYSPRT   E            bolla       1
012200121003     O                       tf7aas           +   1
012300121003     O                       tf7lnp           +   1
012400121003     O                       tf7nrs        z  +   1
012500121003     O                       tf7nsp        2  +   1
012600121005     o                                        +   1 'ksc'
012700121005     O                       tf7ksc           +   1
012800121005     o                                        +   1 'fil'
012900121005     o                       taskfil          +   1
013000121005     o                       taskdfil         +   1
013100121005     o                       taskcar          +   1
013200121005     o                       taskdcar         +   1
013300121005     o                       taskdiv          +   1
013400121005     o                       taskddiv         +   1
013401121005     OQSYSPRT   E            noA         1
013402121005     O                       tf7aas           +   1
013403121005     O                       tf7lnp           +   1
013404121005     O                       tf7nrs        z  +   1
013405121005     O                       tf7nsp        2  +   1
013406121005     o                                        +   1 'ksc'
013407121005     O                       tf7ksc           +   1
013408121005     o                                        +   1 'tbl / cbo'
013409121005     O                       tf7tbl           +   1
013410121005     o                                        +   1 '/'
013411121005     O                       tf7cbo           +   1
013500121005     OQSYSPRT   E            tot         1
013501121005     o                                        +   1 'tot'
013600121005     O                       conta         2  +   1
013700121005     o                                        +   1 'tot102'
013800121005     O                       conta102      2  +   1
013801121005     o                                        +   1 'scarto'
013802121005     O                       scarto        2  +   1
013900121005     o                                        +   1 'completo'
014000121005     O                       contat        2  +   1
