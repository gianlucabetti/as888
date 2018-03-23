000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300160525     fazorg01L  iF   E           K DISK
000400160525     ffnbrv07l  iF   E           K DISK
000500160525     fFNblp01L  iF   E           K DISK
000600160525     fFNblt38L  iF   E           K DISK
000700140314     fFNART01L  IF   E           K DISK
000800140313     fFiars01L  IF   E           K DISK
000900140313     fQSYSPRT   O    F  132        PRINTER  oflind(*INOF)
001000980611      * ---------------------------------------------------------
001100160525     c     *entry        plist
001200160525     c                   parm                    data              8
001300160525     c                   if        data>*zeros
001400160525     c                   movel     data          kdfv              8 0
001500160525     c                   else
001600160525     c                   movel     20160523      kdfv
001700160525     c                   endif
001800160525     c
001900160525     C     Kblp          KLIST
002000160525     C                   KFLD                    bltaas
002100160525     C                   KFLD                    bltlnp
002200160525     C                   KFLD                    bltnrs
002300160525     C                   KFLD                    bltnsp
002400140313     C     KARs1         KLIST
002500160525     C                   KFLD                    bltfls
002600160525     C                   KFLD                    bltlna
002700160525     C                   KFLD                    bltnrs
002800160525     C                   KFLD                    bltnsc
002900140313     C                   KFLD                    trc               1
003000160525     C     Kbrv          KLIST
003100160525     C                   KFLD                    bltfls
003200160525     C                   KFLD                    bltlna
003300160525     C                   KFLD                    bltnrs
003400160525     C                   KFLD                    bltnsc
003500160525     C     KARt          KLIST
003600160525     C                   KFLD                    bltaas
003700160525     C                   KFLD                    bltlnp
003800160525     C                   KFLD                    bltnrs
003900160525     C                   KFLD                    bltnsp
004000160525     C                   KFLD                    bltnsc
004100160525     C     Kblt          KLIST
004200160525     C                   KFLD                    kfil
004300160525     C                   KFLD                    kdfv
004400160525     c
004500140313     c                   eval      trc='C'
004600160525     c                   clear                   esistP            1
004700160525
004800160525     c     *loval        setll     azorg01l
004900160525     c                   read      azorg01l
005000160525    1c                   dow       not %eof(azorg01l)
005100160525    2c                   if        orgfag='F' or orgfag='A'
005200160525
005300160525     c                   movel     orgfil        kfil              3 0
005400160525     c     kblt          setll     fnblt38l
005500160525     c     kblt          reade     fnblt38l
005600160525    3c                   dow       not %eof(fnblt38l)
005700160525     c
005800160525    4c                   if        bltnpp=88 or bltnpp=90
005900160525     c                             or  bltnpp=90
006000160525     c* non entrato ma arrivato
006100160525    5c                   if        bltdse=0 or bltnpp=90
006200160525     c* chain bolla
006300160525     c     kblp          chain     fnblp01l
006400160525    7c                   if        %found(fnblp01l) and blpcca<>'7'
006500160525     c                             and blpcca<>'5'
006600160525     c*
006700160525     c                   eval      esistP='N'
006800160525     c
006900160525     c* se locale escludo se presente altra spunta partenza
007000160525    9c                   if        blptfp=blptfa
007100160525     c     kbrv          setll     fnbrv07l
007200160525     c     kbrv          reade     fnbrv07l
007300160525   10c                   dow       not %eof(fnbrv07l)
007400160525     c                   if        brvdcs>0
007500160525   11c                   if        (brvnpg=1 or brvnpg=5)
007600160525     c                             and brvnps<>90
007700160525     c                             and brvatr=' '
007800160525     c                   eval      esistP='S'
007900160525     c                   leave
008000160525   11c                   endif
008100160525   11c                   endif
008200160525     c
008300160525     c     kbrv          reade     fnbrv07l
008400160525   10c                   enddo
008500160525    9c                   endif
008600160525     c
008700160525     c* se con serie e presenete disugido, ignoro se la data partenza
008800160525     c*  uguale alla data uscita transito
008900160525    9c                   if        esistp='N' and bltnrs>0
009000160525     c                             and bltdut>0 and bltdut=bltdfv
009100160525     c* devo trovare una spunta reale D cat 5 o 1
009200160525     c     kbrv          setll     fnbrv07l
009300160525     c     kbrv          reade     fnbrv07l
009400160525   10c                   dow       not %eof(fnbrv07l)
009500160525     c                   if        brvdcs>0
009600160525   11c                   if        (brvnpg=1 or brvnpg=5)
009700160525     c                             and brvnps<>90
009800160525     c                             and brvatr='D'
009900160525     c                   eval      esistP='S'
010000160525     c                   leave
010100160525   11c                   endif
010200160525   11c                   endif
010300160525     c
010400160525     c     kbrv          reade     fnbrv07l
010500160525   10c                   enddo
010600160525    9c                   endif
010700160525     c
010800160525   8ac                   if        esistp<>'S'
010900160525     c                   add       1             tot               9 0
011000160525     c     kart          chain     fnart01l
011100160525     c                   if        (%found(fnart01l) and
011200160525    8c                             artdam>0  and artnap<> 85) or
011300160525     c                             (not %found(fnart01l) and
011400160525     c                             bltdam>0)
011500160525     c* verifico dal
011600160525      * disk   C
011700160525     c     kars1         chain     fiars01l
011800160525     c                   select
011900160525    9c                   when      %found(fiars01l)
012000160525     c                   add       1             diskC             9 0
012100160525     c                   when      bltnrs>0
012200160525     c                   add       1             diskB             9 0
012300160525     c                   other
012400160525     c                   add       1             altro             9 0
012500160525
012600160525    9c                   endsl
012700160525     c                   else
012800160525     c                   add       1             nonar             9 0
012900160525    8c                   endif
013000160525   8ac                   endif
013100160525
013200160525    7c                   endif
013300160525    5c                   endif
013400160525    4c                   endif
013500041222     C
013600160525     c     kblt          reade     fnblt38l
013700160525    3c                   enddo
013800160525
013900160525    2c                   endif
014000160525     c                   read      azorg01l
014100160525    1c                   enddo
014200140314     c
014300140313     c                   except    totale
014400041222     c                   seton                                        lr
014500041222     c
014600980611      *------------------------------------------------------------
014700140313     OQSYSPRT   E            bolla       1
014800140313     OQSYSPRT   E            totale      1
014900160525     O                                        +   0 'totale colli non spuntati '
015000160525     o                                        +   0 'in entrata e partenza del'
015100160525     O                       data             +   2
015200160525     O                       tot           2  +   2
015300160525     OQSYSPRT   E            totale      1
015400160525     o                                        +   0 '     ARRIVATI'
015500160525     O                                        +   2 'disk C'
015600160525     O                       diskc         2  +   2
015700160525     O                                        +   2 'disk B'
015800160525     O                       diskb         2  +   2
015900160525     O                                        +   2 'altro'
016000160525     O                       altro         2  +   2
016100160525     OQSYSPRT   E            totale      1
016200160525     o                                        +   0 'NON  ARRIVATI'
016300160525     O                       nonar         2  +   2
