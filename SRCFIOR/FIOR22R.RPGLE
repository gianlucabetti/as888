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
001900130620     d w003a           s              3
002000130620
002100130620     d SKpog           s              3    dim(250) inz(*zeros)                 Filiali Gestibili
002200010418
002300000314     dkpjba          e ds
002400010418     D fior21ds      e ds
002500130620     d azuteds       e ds                  extname(azute00f)
002600130620     d ddatiute      e ds
002700130620     d tibs34ds      e ds
002800130620     d TRUL31DS      e ds
002900010418
003000000309     C     *entry        plist
003100000309     C                   parm                    kpjba
003200130620
003300010418     C                   movel     kpjbu         fior21ds
003400130620
003500000313     C                   TIME                    WTIME            14 0
003600000313     C                   MOVE      WTIME         WDATE             8 0
003700000313     C                   MOVEL     WTIME         UTIME             6 0
003800130620
003900130620     c     *dtaara       define    §azute        azuteds
004000130620     c     *dtaara       define    §datiute      ddatiute
004100130620     c                   in(e)     *dtaara
004200130620     c                   if        %error or rsut = *blanks
004300130620     c                   clear                   tibs34ds
004400130620     c                   call      'TIBS34R'
004500130620     c                   parm                    tibs34ds
004600130620     c                   in        *dtaara
004700130620     c                   endif
004800130620
004900130620      * Reperimento delle filiali gestibili dall'utente
005000130620     c                   clear                   TRUL31DS
005100130620     c                   eval      I31abi = D21abi
005200130620     c                   eval      I31cdi = DUTdis
005300130620     c                   eval      I31car = DUTare
005400130620     c                   eval      I31cpo = DUTpou
005500130620     c                   call      'TRUL31R'
005600130620     c                   parm                    KPJBA
005700130620     c                   parm                    TRUL31DS
005800130620     c                   IF        O31pog > *zeros
005900130620     c                   movea     O31pog        SKpog
006000130620     c                   ENDIF
006100010418
006200010418      * preparo i campi di stampa per la stampa dell'intestazione
006300010418
006400010418     c                   z-add     d21por        pttpor
006500010418     c     pttpor        ifeq      *zeros
006600010418     c                   movel     'TUTTI'       pttdpr
006700000313     c                   else
006800010418     c     pttpor        chain     azorg01l                           30
006900010418     c  n30              movel     orgdes        pttdpr
007000010418     c   30              clear                   pttdpr
007100000313     c                   endif
007200010418     c     d21mp         ifeq      'M'
007300010418     c                   movel(P)  'MATT.'       prtmp
007400000313     c                   else
007500010418     c     d21mp         ifeq      'P'
007600010418     c                   movel(P)  'POM.'        prtmp
007700000313     c                   else
007800010418     c                   movel(P)  'TUTTI'       prtmp
007900000313     c                   endif
008000000313     c                   endif
008100010418     c                   z-add     d21poe        prtpoe
008200010418     c     prtpoe        ifeq      *zeros
008300010418     c                   movel     'TUTTI'       prtdpe
008400000313     c                   else
008500010418     c     prtpoe        chain     azorg01l                           30
008600010418     C  N30              MOVEL     ORGDES        prtdpe
008700010418     C   30              clear                   prtdpe
008800000313     c                   endif
008900010418     c     d21ord        ifeq      '1'
009000000317     c                   movel     ord1          prtord
009100000317     c                   else
009200000317     c                   movel     ord2          prtord
009300000317     c                   endif
009400000313
009500010418     c                   write     fi22t1
009600000313
009700010418      * ordinamento per p.o. ritiro e ragione sociale
009800010418     c     d21ord        ifeq      '1'
009900010418     c     *loval        setll     fnors04l
010000010418     c                   else
010100010418      * ordinamento per cod. cliente ritiro
010200010418     c     *loval        setll     fnors01l
010300010418     c                   endif
010400010418
010500010418     c                   do        *hival
010600010418
010700010418     c     d21ord        ifeq      '1'
010800010418     c                   read      fnors04l
010900010418     c                   else
011000010418     c                   read      fnors01l
011100010418     c                   endif
011200010418
011300010418     c                   if        %eof
011400010418     c                   leave
011500010418     c                   endif
011600040818      * escludo gli annullati
011700040818     c                   If        OrsAtb <> *Blanks
011800040818     c                   Iter
011900040818     c                   EndIf
012000010418
012100010418      * Selezioni
012200010418     c                   if        d21por <> 0
012300010418     c                             and orspor <> d21por
012400010418     c                   iter
012500010418     c                   endif
012600010418
012700010418     c                   if        d21mp <> *blanks
012800010418     c                             and orsrmp <> d21mp
012900010418     c                   iter
013000010418     c                   endif
013100010418
013200010418     c                   if        d21poe <> 0
013300010418     c                             and orspoe <> d21poe
013400010418     c                   iter
013500010418     c                   endif
013600020809
013700020809     c                   If        D21por = *Zeros and D21poe = *Zeros
013800130620     c                   move      ORSpor        w003a
013900130620     c     w003a         lookup    SKpog                                  30
014000020809     c                   If        Not *In30
014100130620     c                   move      ORSpoe        w003a
014200130620     c     w003a         lookup    SKpog                                  30
014300020809     c                   If        Not *In30
014400020809     c                   Iter
014500020809     c                   EndIf
014600020809     c                   EndIf
014700020809     c                   EndIf
014800010418
014900010418     c                   exsr      stampa
015000010418
015100010418     c                   enddo
015200010418
015300010418      * Stampa "Fine stampa"
015400010418     c                   if        *in90 = *on
015500010418     c                   write     fi22t1
015600000314     c                   endif
015700010418     c                   write     fi22e2
015800010418
015900010418     c                   eval      *inlr = *on
016000010418      **********************************************************************
016100010418      * STAMPA
016200010418      **********************************************************************
016300010418     c     stampa        begsr
016400010418
016500000314     c* se richiesto ordinamento per p.o. ritiro salto pagina a rottura
016600010418     c     d21ord        ifeq      '1'
016700010418     c     orspor        andne     savpor
016800010418     c     orspor        chain     azorg01l                           30
016900010418     c  n30              movel     orgdes        prtdpr
017000010418     c   30              clear                   prtdpr
017100010418     c                   move      orspor        prtpor
017200010418     c                   if        savpor <> *zeros
017300010418     c                   write     fi22t1
017400010418     c                   endif
017500010418     c                   write     fi22t1p
017600010418     c                   move      orspor        savpor
017700000314     c                   endif
017800010418
017900010418     c                   clear                   prtrag
018000010418     c                   clear                   prtind
018100010419     c                   clear                   prtloc
018200010419     c                   clear                   prtprv
018300010418     c     orscra        chain     fnacr01l
018400010418     c                   if        %found(fnacr01l)
018500010418     c                   movel     acrrsr        prtrag
018600010418     c                   movel     acrinr        prtind
018700010419     c                   movel     acrlor        prtloc
018800010419     c                   movel     acrprr        prtprv
018900010418     c                   endif
019000010418
019100010418     c                   move      orspor        prtpor
019200010418     c                   if        *in90 = *on
019300010418     c                   write     fi22t1
019400010418     c     d21ord        ifeq      '1'
019500010418     c                   write     fi22t1p
019600000314     c                   endif
019700010418     c                   eval      *in90 = *off
019800000314     c                   endif
019900010418     c                   write     fi22d1
020000010418
020100000313     c                   endsr
