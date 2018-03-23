000100010418     H DECEDIT('0,') DATEDIT(*DMY.)
000200010418
000300010418      *****************************************************************
000400010418      *                                                               *
000500010418      *       STAMPA ANAGRAFICA RITIRI FISSI                          *
000600010418      *                                                               *
000700010418      *****************************************************************
000800010418
000900010418     Ffnors01l  if   e           k disk
001000010418     Ffnors04l  if   e           k disk    rename(fnors000:fnors4)
001100010418     Ffnacr01l  if   e           k disk
001200000314     Fazorg01l  if   e           k disk
001300010418     Ffior22p   o    e             printer oflind(*in90)
001400010418
001500070418     D ORD1            C                   CONST('Fil.rit.+Cod.Mitt. ')
001600000317     D ORD2            C                   CONST('Codice ritiro      ')
001700010418
001800010418     d savpor          s                   like(orspor)
001900010418
002000000314     dkpjba          e ds
002100010418     D fior21ds      e ds
002200010418     D UT§DSE0f      E DS
002300000313     D CNCR80        E DS
002400020809     d Trul06Ds      e ds
002500020809     d  Lin                    1     90  0 Dim(30)
002600010418
002700000309     C     *entry        plist
002800000309     C                   parm                    kpjba
002900010418     C                   movel     kpjbu         fior21ds
003000000313     C                   TIME                    WTIME            14 0
003100000313     C                   MOVE      WTIME         WDATE             8 0
003200000313     C                   MOVEL     WTIME         UTIME             6 0
003300000313     C                   Z-ADD     1             CODUT
003400000313     C                   CALL      'X§PARUT'
003500010418     C                   PARM                    UT§DSE0f
003600010418
003700000313     C                   MOVEL     RAGUT         RSUT
003800000313     C                   MOVEL     REC80         CNCR80
003900020809
004000020809      * Se p.o. gestione 1° livello
004100020809     c                   If        D21pog = Simfel
004200020809      * Carico i p.o.
004300020809     c                   Clear                   Trul06Ds
004400020809     c                   Move      '£1'          D06COD
004500020809     c                   Movel     SIMFEL        D06KEY
004600020809     c                   Movel     Trul06Ds      KPJBU
004700020809     c                   Call      'TRUL06R'
004800020809     c                   Parm                    KPJBA
004900020809     c                   Movel     KPJBU         Trul06Ds
005000020809      * Se p.o. gestione non è 1° livello solo se stesso
005100020809     c                   Else
005200020809     c                   Clear                   Lin
005300020809     c                   Move      D21pog        Lin(1)
005400020809     c                   EndIf
005500010418
005600010418      * preparo i campi di stampa per la stampa dell'intestazione
005700010418
005800010418     c                   z-add     d21por        pttpor
005900010418     c     pttpor        ifeq      *zeros
006000010418     c                   movel     'TUTTI'       pttdpr
006100000313     c                   else
006200010418     c     pttpor        chain     azorg01l                           30
006300010418     c  n30              movel     orgdes        pttdpr
006400010418     c   30              clear                   pttdpr
006500000313     c                   endif
006600010418     c     d21mp         ifeq      'M'
006700010418     c                   movel(P)  'MATT.'       prtmp
006800000313     c                   else
006900010418     c     d21mp         ifeq      'P'
007000010418     c                   movel(P)  'POM.'        prtmp
007100000313     c                   else
007200010418     c                   movel(P)  'TUTTI'       prtmp
007300000313     c                   endif
007400000313     c                   endif
007500010418     c                   z-add     d21poe        prtpoe
007600010418     c     prtpoe        ifeq      *zeros
007700010418     c                   movel     'TUTTI'       prtdpe
007800000313     c                   else
007900010418     c     prtpoe        chain     azorg01l                           30
008000010418     C  N30              MOVEL     ORGDES        prtdpe
008100010418     C   30              clear                   prtdpe
008200000313     c                   endif
008300010418     c     d21ord        ifeq      '1'
008400000317     c                   movel     ord1          prtord
008500000317     c                   else
008600000317     c                   movel     ord2          prtord
008700000317     c                   endif
008800000313
008900010418     c                   write     fi22t1
009000000313
009100010418      * ordinamento per p.o. ritiro e ragione sociale
009200010418     c     d21ord        ifeq      '1'
009300010418     c     *loval        setll     fnors04l
009400010418     c                   else
009500010418      * ordinamento per cod. cliente ritiro
009600010418     c     *loval        setll     fnors01l
009700010418     c                   endif
009800010418
009900010418     c                   do        *hival
010000010418
010100010418     c     d21ord        ifeq      '1'
010200010418     c                   read      fnors04l
010300010418     c                   else
010400010418     c                   read      fnors01l
010500010418     c                   endif
010600010418
010700010418     c                   if        %eof
010800010418     c                   leave
010900010418     c                   endif
011000040818      * escludo gli annullati
011100040818     c                   If        OrsAtb <> *Blanks
011200040818     c                   Iter
011300040818     c                   EndIf
011400010418
011500010418      * Selezioni
011600010418     c                   if        d21por <> 0
011700010418     c                             and orspor <> d21por
011800010418     c                   iter
011900010418     c                   endif
012000010418
012100010418     c                   if        d21mp <> *blanks
012200010418     c                             and orsrmp <> d21mp
012300010418     c                   iter
012400010418     c                   endif
012500010418
012600010418     c                   if        d21poe <> 0
012700010418     c                             and orspoe <> d21poe
012800010418     c                   iter
012900010418     c                   endif
013000020809
013100020809     c                   If        D21por = *Zeros and D21poe = *Zeros
013200020809     c     Orspor        Lookup    Lin                                    30
013300020809     c                   If        Not *In30
013400020809     c     Orspoe        Lookup    Lin                                    30
013500020809     c                   If        Not *In30
013600020809     c                   Iter
013700020809     c                   EndIf
013800020809     c                   EndIf
013900020809     c                   EndIf
014000010418
014100010418     c                   exsr      stampa
014200010418
014300010418     c                   enddo
014400010418
014500010418      * Stampa "Fine stampa"
014600010418     c                   if        *in90 = *on
014700010418     c                   write     fi22t1
014800000314     c                   endif
014900010418     c                   write     fi22e2
015000010418
015100010418     c                   eval      *inlr = *on
015200010418      **********************************************************************
015300010418      * STAMPA
015400010418      **********************************************************************
015500010418     c     stampa        begsr
015600010418
015700000314     c* se richiesto ordinamento per p.o. ritiro salto pagina a rottura
015800010418     c     d21ord        ifeq      '1'
015900010418     c     orspor        andne     savpor
016000010418     c     orspor        chain     azorg01l                           30
016100010418     c  n30              movel     orgdes        prtdpr
016200010418     c   30              clear                   prtdpr
016300010418     c                   move      orspor        prtpor
016400010418     c                   if        savpor <> *zeros
016500010418     c                   write     fi22t1
016600010418     c                   endif
016700010418     c                   write     fi22t1p
016800010418     c                   move      orspor        savpor
016900000314     c                   endif
017000010418
017100010418     c                   clear                   prtrag
017200010418     c                   clear                   prtind
017300010419     c                   clear                   prtloc
017400010419     c                   clear                   prtprv
017500010418     c     orscra        chain     fnacr01l
017600010418     c                   if        %found(fnacr01l)
017700010418     c                   movel     acrrsr        prtrag
017800010418     c                   movel     acrinr        prtind
017900010419     c                   movel     acrlor        prtloc
018000010419     c                   movel     acrprr        prtprv
018100010418     c                   endif
018200010418
018300010418     c                   move      orspor        prtpor
018400010418     c                   if        *in90 = *on
018500010418     c                   write     fi22t1
018600010418     c     d21ord        ifeq      '1'
018700010418     c                   write     fi22t1p
018800000314     c                   endif
018900010418     c                   eval      *in90 = *off
019000000314     c                   endif
019100010418     c                   write     fi22d1
019200010418
019300000313     c                   endsr
