000100990504     H* ACTGRP=*CALLER
000200081118     H DEBUG DECEDIT('0,') DATEDIT(*DMY/) Option(*nodebugio)
000300081104     f*_______________________________________________________________________________
000400081104      * occorre che il file soccod0f contenga il nome della società e il codice
000500081104      * del fornitore questo deve essere fornito dall'ufficio traini.
000600081104      * In questo modo leggendo il file è possibile trascodificare le anagrafiche
000700081104      * dall'attuale tracciato ai nuovi predisponendo testata societaria e dettagli
000800081104      * relativamente a automezzi e autisti legati alla società
000900081104     f*_______________________________________________________________________________
001000081104     faztra02l  uf   e           k disk
001100081110     fsoccod    if   e             disk    rename(soccod:soccodd)
001200081110     Fansog01l  IF   E           K DISK
001300080911     Fanfrn01l  IF   E           K DISK
001400080911     faiats01l  uf a e           k disk
001500080911     faiatm01l  uf a e           k disk
001600080911     faiata01l  uf a e           k disk
001700030509     fqsysprt   o    f  198        printer infds(stamp)
001800030618     d kpjba         e ds
001900030618     d trul33ds      e ds
002000030507     d dataiso         s               d   datfmt(*iso)
002100030507     d dataeur         s               d   datfmt(*eur)
002200030509     d stamp           ds
002300030509     d  lin                  367    368b 0
002400081110     D WLBDA8          DS
002500081110     D  G02DAT                 1      8  0
002600081110     D  G02INV                 9     16  0
002700081110     D  G02ERR                17     17
002800081110     D  G02TGI                18     22  0
002900081113     d WGIDAT          DS
003000081113     d  GI8DAT                 1      8  0
003100081113     d  gi8INV                 9     16  0
003200081113     d  GI8TGI                17     21  0
003300081113     d
003400000607      *--------------------------------------------------------------------
003500030509     c                   z-add     66            lin
003600030509     c                   except    testa
003700081110     c     *start        setll     soccod
003800080911     c                   do        *hival
003900081110     c                   read      soccod
004000081110     c                   if        %eof(soccod)
004100080911     c                   leave
004200080911     c                   endif
004300080911
004400080911     c                   clear                   societa
004500000607     c
004600081113     c                   movel     f2            f2k              25
004700081113     c     f2k           setll     aztra02l
004800000607     c                   do        *hival
004900081113     c     f2k           reade     aztra02l
005000080911     c                   if        %eof(aztra02l)
005100080911     c                   leave
005200080911     c                   endif
005300080911      *societa
005400080911     c                   if        societa = *blank
005500080911     c                   clear                   aiats000
005600080911     c                   exsr      scrivisoc
005700081104     c                   move      'X'           societa           1
005800080911     c                   endif
005900080911      * autisti
006000080911     c                   clear                   aiata000
006100081218     c                   if        traaut <> *blank and
006200081218     c                             traaut <> 'MEZZO DI RISERVA'
006300081113     c                   z-add     f3            ATAKSC
006400081110     c                   move      soc           ATASOC
006500080911     c                   movel     traaut        ATAAUT
006600081110     c                   z-add     tradin        g02dat
006700081110     C                   MOVEL     *blank        G02ERR
006800081110     C                   CALL      'XSRDA8'
006900081110     C                   PARM                    WLBDA8
007000081110     C     G02ERR        IFEQ      '1'
007100081110     C                   z-add     0             ATADTINR
007200081110     C                   else
007300081110     c                   move      g02inv        ATADTINR
007400081110     C                   endif
007500081112     c                   z-add     tradfi        g02dat
007600081112     C                   MOVEL     *blank        G02ERR
007700081112     C                   CALL      'XSRDA8'
007800081112     C                   PARM                    WLBDA8
007900081112     C     G02ERR        IFEQ      '1'
008000081112     C                   z-add     0             ATADTfiR
008100081112     C                   else
008200081112     c                   move      g02inv        ATADTfiR
008300081112     C                   endif
008400080911     c                   movel     tratel        ATATEL
008500080911     c                   movel     trapat        ATAPAT
008600080911     c                   movel     tratmp        ATACQUAL
008700080911     c                   movel     traide        ATAFOTO
008800080911     c                   movel     trapen        ATACPENA
008900081104     c                   movel     traccp        ATACPEND
009000081218     c     kata          chain     aiata01l
009100081218     c                   if        %found(aiata01l)
009200081218     c                   if        TRADFI = 0
009300081218     C                   z-add     0             atadtfir
009400081218     C                   update    aiata000
009500081218     C                   endif
009600081218     c                   else
009700080911     c                   write     aiata000
009800081218     c                   endif
009900080911     c                   endif
010000030507
010100080911      * mezzi
010200080911     c                   clear                   aiatm000
010300080911     c                   if        trataa <> *blank and trataa <> 'X'
010400081218     c                             and tradfi = 0
010500081113     c                   z-add     f3            ATMKSC
010600081110     c                   move      soc           ATMSOC
010700081110     c                   z-add     tradin        g02dat
010800081110     C                   MOVEL     *blank        G02ERR
010900081110     C                   CALL      'XSRDA8'
011000081110     C                   PARM                    WLBDA8
011100081110     C     G02ERR        IFEQ      '1'
011200081110     C                   z-add     0             atmdtinr
011300081110     C                   else
011400081110     c                   move      g02inv        atmdtinr
011500081110     C                   endif
011600081112     c                   z-add     tradfi        g02dat
011700081112     C                   MOVEL     *blank        G02ERR
011800081112     C                   CALL      'XSRDA8'
011900081112     C                   PARM                    WLBDA8
012000081112     C     G02ERR        IFEQ      '1'
012100081112     C                   z-add     0             atmdtfir
012200081112     C                   else
012300081112     c                   move      g02inv        atmdtfir
012400081112     C                   endif
012500081110     c                   movel     trataa        ATMTARGA
012600080911     c                   movel     traact        ATMCTER
012700080911     c                   movel     traiaa        ATMISCRA
012800080911     c                   movel     tratia        ATMTIPO
012900081110     c                   z-add     tradcl        g02dat
013000081110     C                   MOVEL     *blank        G02ERR
013100081110     C                   CALL      'XSRDA8'
013200081110     C                   PARM                    WLBDA8
013300081110     C     G02ERR        IFEQ      '1'
013400081110     C                   z-add     0             ATMDTCON
013500081110     C                   else
013600081110     c                   move      g02inv        ATMDTCON
013700081110     C                   endif
013800080911     c                   movel     traass        ATMASSIC
013900080911     c                   movel     traant        ATMANTIF
014000080911     c                   movel     trafot        ATMFOTO
014100081110     c     katm          chain     aiatm01l
014200081110     c                   if        %found(aiatm01l)
014300081110     c                   except    riga
014400081110     c                   else
014500080911     c                   write     aiatm000
014600081110     c                   endif
014700080911     c                   endif
014800080911     c                   enddo
014900080911     c                   enddo
015000000607     c                   seton                                        lr
015100080911     c*------------------------------------------------------*
015200080911     c     scrivisoc     begsr
015300080911     c*------------------------------------------------------*
015400080911     C     Krco          KLIST
015500080911     C                   KFLD                    soc               3
015600080911     C                   KFLD                    cod8              8
015700081110     C     Katm          KLIST
015800081110     C                   KFLD                    atmsoc
015900081110     C                   KFLD                    atmksc
016000081110     C                   KFLD                    atmtarga
016100081110     C     Kata          KLIST
016200081110     C                   KFLD                    atasoc
016300081110     C                   KFLD                    ataksc
016400081110     C                   KFLD                    ataaut
016500080911     c                   move      *zeros        cod8
016600081113     c                   move      f3            cod8
016700080911     C                   move      '248'         soc
016800080911     c     krco          chain     anfrn01l
0169000812181    c                   if        %found(anfrn01l)
017000080911     c     frnsogg       chain     ansog01l
0171000812182    c                   if        %found(ansog01l)
017200080911     c                   move      sogdes        atsrags
017300081110     c                   movel     sogpartiva    atspiva
017400081113     c                   z-add     f3            atsksc
017500080911     c                   move      soc           atssoc
017600081113     C                   clear                   Wgidat
0177000812183    c                   if        f1 = 0
017800081217     c                   z-add     19400101      atsdtinc
017900081218x3   c                   else
018000081113     c                   z-add     f1            gi8tgi
018100081113     C                   CALL      'XSRGI8'
018200081113     C                   PARM                    Wgidat
018300081113     c                   move      gi8inv        atsdtinc
018400081218e3   c                   endif
018500080911     c                   write     aiats000
018600081218x2   c                   else
018700081218     c                   except    rigasogg
018800081218 e2  c                   endif
018900081218 x1  c                   else
019000081218     c                   except    rigaforn
019100081218 e1  c                   endif
019200080911     c                   endsr
019300030509     O*------------------------------------------------------*
019400081218     O*IATA000  E            AGGDATA
019500081218     O*                      ATADTFIR
019600030509     OQSYSPRT   e            TESTA             2
019700030509     O                                           70 'Anomalie riscontrate tradu-
019800030509     O                                              zione anagrafiche AZTRA'
019900030512     O          e            testa       1
020000030603     o                                           31 ' '
020100080911     o                                           15 'Cod fornitore'
020200080911     o                                          + 1 'Societa'
020300080911     o                                           50 'Societa'
020400030509     O          e            riga        1
020500081113     o                       f3                  12
020600081113     o                       f2                  40
020700080911     o                       traaut             + 1
020800081104     o                       trataa             + 1
020900081112     o                       tradfi             + 1 '  /  /  '
021000081218     O          e            rigasogg    1
021100081218     o                       f3                  12
021200081218     o                       f2                  40
021300081218     o                       traaut             + 1
021400081218     o                       trataa             + 1
021500081218     o                       tradfi             + 1 '  /  /  '
021600081218     o                                          + 1 'Non trovato soggetto'
021700081218     O          e            rigaforn    1
021800081218     o                       f3                  12
021900081218     o                       f2                  40
022000081218     o                       traaut             + 1
022100081218     o                       trataa             + 1
022200081218     o                       tradfi             + 1 '  /  /  '
022300081218     o                                          + 1 'Non trovato fornitore'
