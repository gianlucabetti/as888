000100990504     H* ACTGRP=*CALLER
000200990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300080911     faztra02l  uf   e           k disk
000400080911     fsoccod0f  if   e             disk
000500080911     Fansog01l  IF   E           K DISK
000600080911     Fanfrn01l  IF   E           K DISK
000700080911     faiats01l  uf a e           k disk
000800080911     faiatm01l  uf a e           k disk
000900080911     faiata01l  uf a e           k disk
001000030509     fqsysprt   o    f  198        printer infds(stamp)
001100030618     d kpjba         e ds
001200030618     d trul33ds      e ds
001300030507     d dataiso         s               d   datfmt(*iso)
001400030507     d dataeur         s               d   datfmt(*eur)
001500030509     d stamp           ds
001600030509     d  lin                  367    368b 0
001700000607      *--------------------------------------------------------------------
001800030509     c                   z-add     66            lin
001900030509     c                   except    testa
002000080911     c     *start        setll     soccod0f
002100080911     c                   do        *hival
002200080911     c                   read      soccod0f
002300080911     c                   if        %eof(soccod0f)
002400080911     c                   leave
002500080911     c                   endif
002600080911
002700080911     c                   clear                   societa
002800000607     c
002900080911     c     nomsoc        setll     aztra02l
003000000607     c                   do        *hival
003100080911     c     nomsoc        reade     aztra02l
003200080911     c                   if        %eof(aztra02l)
003300080911     c                   leave
003400080911     c                   endif
003500080911     c                   if        tradfi > 0
003600080911     c                   iter
003700080911     c                   endif
003800080911      *societa
003900080911     c                   if        societa = *blank
004000080911     c                   clear                   aiats000
004100080911     c                   exsr      scrivisoc
004200080911     c                   move      'X'           societa
004300080911     c                   endif
004400080911      * autisti
004500080911     c                   clear                   aiata000
004600080911     c                   if        traaut <> *blank
004700080911     c                   z-add     cod           ATAKSC
004800080911     c                   move      soc           ATASOC
004900080911     c                   movel     traaut        ATAAUT
005000080911     c                   z-add     tradin        ATADTINR
005100080911     c                   z-add     tradfi        ATADTFIR
005200080911     c                   movel     tratel        ATATEL
005300080911     c                   movel     trapat        ATAPAT
005400080911     c                   movel     tratmp        ATACQUAL
005500080911     c                   movel     traide        ATAFOTO
005600080911     c                   movel     trapen        ATACPENA
005700080911     c                   movel     traccn        ATACPEND
005800080911     c                   write     aiata000
005900080911     c                   endif
006000030507
006100080911      * mezzi
006200080911     c                   clear                   aiatm000
006300080911     c                   if        trataa <> *blank and trataa <> 'X'
006400080911     c                   z-add     cod           ATMKSC
006500080911     c                   move      soc           ATMSOC
006600080911     c                   movel     tradin        ATMDTINR
006700080911     c                   z-add     tradfi        ATMDTFIR
006800080911     c                   z-add     trataa        ATMTARGA
006900080911     c                   movel     traact        ATMCTER
007000080911     c                   movel     traiaa        ATMISCRA
007100080911     c                   movel     tratia        ATMTIPO
007200080911     c                   movel     tradcl        ATMDTCON
007300080911     c                   movel     traass        ATMASSIC
007400080911     c                   movel     traant        ATMANTIF
007500080911     c                   movel     trafot        ATMFOTO
007600080911     c                   write     aiatm000
007700080911     c                   endif
007800080911     c                   enddo
007900080911     c                   enddo
008000000607     c                   seton                                        lr
008100080911     c*------------------------------------------------------*
008200080911     c     scrivisoc     begsr
008300080911     c*------------------------------------------------------*
008400080911     C     Krco          KLIST
008500080911     C                   KFLD                    soc               3
008600080911     C                   KFLD                    cod8              8
008700080911     c                   move      *zeros        cod8
008800080911     c                   move      cod           cod8
008900080911     C                   move      '248'         soc
009000080911     c     krco          chain     anfrn01l
009100080911     c                   if        %found(anfrn01l)
009200080911     c     frnsogg       chain     ansog01l
009300080911     c                   if        %found(ansog01l)
009400080911     c                   move      sogdes        atsrags
009500080911     c                   move      sogpartiva    atspiva
009600080911     c                   z-add     cod           atsksc
009700080911     c                   move      soc           atssoc
009800080911     c                   write     aiats000
009900080911     c                   else
010000080911     c                   except    riga
010100080911     c                   endif
010200080911     c                   else
010300080911     c                   except    riga2
010400080911     c                   endif
010500080911     c                   endsr
010600030509     O*------------------------------------------------------*
010700030509     OQSYSPRT   e            TESTA             2
010800030509     O                                           70 'Anomalie riscontrate tradu-
010900030509     O                                              zione anagrafiche AZTRA'
011000030512     O          e            testa       1
011100030603     o                                           31 ' '
011200080911     o                                           15 'Cod fornitore'
011300080911     o                                          + 1 'Societa'
011400080911     o                                           50 'Societa'
011500030509     O          e            riga        1
011600080911     o                       cod                 12
011700080911     o                       nomsoc              40
011800080911     o                       traaut             + 1
