000100980521      ****************************************************************
000200980521      *  RIEPILOGO INDICATORI
000300981008      ****************************************************************
000400990603      * 01 -
000500990317      ****************************************************************
000600980521     H DECEDIT('0,') DATEDIT(*DMY.)
000700980623
000800990602      *------------------------------------------------------------------------*
000900141119     ftnss90d   cf   E             workstn
000901081002     FTABEL00F  IF   E           K DISK
000902141119     Fazorg01l  IF   E           K DISK
001000980521      *------------------------------------------------------------------------*
001100980702
001200990602      * Definizione campi di work ------------------------------------------
001201081002     d ktbcod          s                   like(tblcod)
001202081003     d ktbkey          s                   like(tblkey)
001300990609     D XX              s              3  0
001400990603     d dini_eur        s               D   datfmt(*eur)
001500990603     d dini_iso        s               D   datfmt(*iso)
001600990603     d dfin_eur        s               D   datfmt(*eur)
001700990603     d dfin_iso        s               D   datfmt(*iso)
001701141119     d codut           s              1  0 inz(1)
001702141119     d w0030           s              3  0
001900990602      * Schiere ------------------------------------------------------------
002000081003     D MSG             S             78    DIM(5) CTDATA PERRCD(1)
002100141119     d vfil            s              3    dim(5)
002200141119     d dfil            s             20    dim(5)
002300990602      * DS esterne ---------------------------------------------------------
003000141119     D tnss90ds      E DS
003001141119     d  fil                           3    dim(5)  overlay(tnss90ds:17)
003002141119     d Tibs34ds      e ds
003003141119     d Azuteds       e ds                  Extname(Azute00f)
003004141119     d dDatiute      e ds
003200990602
003601081002     D DS$3          E DS
003700990602     D KPJBA         E DS
003800990602      * DS interne ---------------------------------------------------------
004800990602
004900980525      *
005000990601
005100141119     Iss90d01
005300141119     I              v1cfi1                      vfil(1)
005400141119     I              v1cfi2                      vfil(2)
005500141119     I              v1cfi3                      vfil(3)
005600141119     I              v1cfi4                      vfil(4)
005601141119     I              v1cfi5                      vfil(5)
006300141119     I              v1dfi1                      dfil(1)
006400141119     I              v1dfi2                      dfil(2)
006500141119     I              v1dfi3                      dfil(3)
006600141119     I              v1dfi4                      dfil(4)
006700141119     I              v1dfi5                      dfil(5)
007200980525      *
007300990601
007400990603     c                   clear                   v1cdti
007500990603     c                   clear                   v1cdtf
007600141119     c                   clear                   vfil
007700141119     c                   clear                   dfil
007701141124     c                   eval      vfil(1)='194'
007702141125     c     194           chain     azorg01l
007703141125     c                   if        %found(azorg01l)
007704141125     c                   eval      dfil(1)=orgdes
007705141125     c                   endif
007706141124     c                   eval      vfil(2)='341'
007707141125     c     341           chain     azorg01l
007708141125     c                   if        %found(azorg01l)
007709141125     c                   eval      dfil(2)=orgdes
007710141125     c                   endif
007711141124     c* di default imposto 194 e 341
007900990603     c     emetti        tag
008000141119     c                   exfmt     ss90d01
008100990603     c                   setoff                                       2829
008200990603     c     *inkc         cabeq     '1'           fine
008300990603     c                   exsr      ctrd01
008400990603     c   28
008500990603     cornkf
008600990603     cor 29              goto      emetti
008800141119     c                   move      dini_iso      SS90DSD
009000141119     c                   move      dfin_iso      SS90DSA
009100141119     c     SS90DSA       ifeq      *zeros
009300141119     c                   move      dini_iso      SS90DSA
009400990604     c                   end
009500141119     c                   do        5             ii
009600141119     c     vfil(ii)      ifeq      *blanks
009700141119     c                   move      *zeros        vfil(ii)
009800990609     c                   endif
009900990609     c                   enddo
010000141119     c                   movea     vfil          fil
010200141119     c                   movel     tnss90ds      kpjbu
010300141119     c                   movel     'SS91'        kcoaz
010400990603     c                   call      'BCH10'
010500990603     c                   parm                    kpjba
010600990603     c     fine          tag
011300990603
011400990603     c                   seton                                        lr
011500990602
011600990603     c**********************************************************************
011700990603     c     *inzsr        begsr
011800990603
011900990603     C     *entry        plist
012000990603     C                   parm                    kpjba
012100990603
012101141119     c     *dtaara       define    §azute        azuteds
012102141119     c     *dtaara       define    §datiute      ddatiute
012103141119     c                   in(E)     *dtaara
012104141119     c                   If        %error  or RSUT = *blanks
012105141119     c                   CLEAR                   tibs34ds
012106141119     c                   CALL      'TIBS34R'
012107141119     c                   PARM                    tibs34ds
012108141119     c                   in        *dtaara
012109141119     c                   EndIf
016101081002      *
016102081002     C     KE2TAB        KLIST                                                  *TABEL00F
016103081002     C                   KFLD                    codut                           -UTENTE
016104081002     C                   KFLD                    KTBCOD                          -TABELLA
016105081003     C                   KFLD                    ktbkey
016107081003      * Reperimento date ultima pulizia archivi bolle
016108081003      * SEDE
016110081002     C                   CLEAR                   DS$3
016111081002     C                   MOVEL     '$3'          KTBCOD
016112081003     C                   MOVEL(P)  '1'           ktbkey
016113081003     C     ke2tab        CHAIN     TABEL00F
016114081003    2C                   IF        %found(tabel00f)
016115081002     C                   MOVEL     TBLUNI        DS$3
016116081003    2c                   endif
016169081002     c
016200990603
016300990603     c                   endsr
016400990603
016500990603     c**********************************************************************
016600990603     c     ctrd01        begsr
016700990603     c                   setoff                                       010203
016800141119     c                   setoff                                       0405
017000141119     c                   setoff                                       1112
017100990603     c* controllo data dal
017200990603     c     *dmy          test(d)                 v1cdti                 30
017300990603     c                   if        *in30 = '0'
017400990603     c     *dmy          move      v1cdti        dini_eur
017500990603     c                   else
017600990603     c     *eur          test(d)                 v1cdti                 31
017700990603     c                   if        *in31 = '0'
017800990603     c                   move      v1cdti        dini_eur
017900990603     c                   else
018000990603     c                   seton                                        1128
018100990604     c                   movel     msg(1)        v1zmsg
018200990603     c                   goto      ectd01
018300990603     c                   endif
018400990603     c                   endif
018500990603     c                   move      dini_eur      dini_iso
018600990603     c                   move      dini_eur      v1cdti
018601081002     c* Verifico se per la data sono già trascorsi i giorni di pulizia
018602081002     c                   move      dini_iso      wdat              8 0
018604081002     c                   if        wdat<=§$3dp0
018605081002     c                   seton                                        1128
018606081002     c                   movel     msg(5)        v1zmsg
018607081002     c                   goto      ectd01
018608081002     c                   endif
018700990603
018800990604     c* controllo data al se immessa
018900990604     c                   clear                   dfin_eur
019000990604     c                   clear                   dfin_iso
019100990604     c     v1cdtf        ifgt      *zeros
019200990603     c     *dmy          test(d)                 v1cdtf                 30
019300990603     c                   if        *in30 = '0'
019400990603     c     *dmy          move      v1cdtf        dfin_eur
019500990603     c                   else
019600990603     c     *eur          test(d)                 v1cdtf                 31
019700990603     c                   if        *in31 = '0'
019800990603     c                   move      v1cdtf        dfin_eur
019900990603     c                   else
020000990603     c                   seton                                        1228
020100990604     c                   movel     msg(1)        v1zmsg
020200990603     c                   goto      ectd01
020300990603     c                   endif
020400990603     c                   endif
020500990603     c                   move      dfin_eur      dfin_iso
020600990603     c                   move      dfin_eur      v1cdtf
020700990609     c* errore se data dal > di data al
020800990604     c     dini_iso      ifgt      dfin_iso
020900990604     c                   seton                                        1128
021000990604     c                   movel     msg(2)        v1zmsg
021100990604     c                   goto      ectd01
021200990604     c                   endif
021300990604     c                   endif
021400990604     c
021500990603
021600141119     c* controllo codici filiale
021700141119     c                   do        5             ii                2 0
021800141119     c     vfil(ii)      ifne      *blanks
021900141119     c     vfil(ii)      andne     *zeros
022000141119     C     '?'           SCAN      vfil(ii)                               32
022100990609IF  2C     *in32         ifeq      *on
022101141119     C                   MOVEL     *BLANKS       §COD1             3
022102141119     C                   MOVE      *BLANKS       §DES1
022103141119     C                   MOVE      *BLANKS       §PRA
022104141119     C                   CALL      'TNSD24R'
022105141119     C                   PARM                    §COD1
022106141119     C                   PARM                    §TIP              1
022107141119     C                   PARM                    §DES1            25
022108141119     C                   PARM                    §PRA              3
022109141119     C                   MOVEL     §COD1         vfil(ii)
022110141119     C                   MOVEL     §DES1         dfil(ii)
023000990609     c                   seton                                        29
023100990609     c                   goto      ectd01
023200990609     c                   endif
023300141119     C* CODICE filiale NUMERICO
023400990609     C                   SETOFf                                       32
023500141119     C                   TESTN                   Vfil(ii)             32
023600990609IF  3C     *in32         ifeq      *on
023700141119     C                   move      vfil(ii)      W001              1
023800990609IF  4C     w001          iflt      '0'                                      RA
023900990609     c                   seton                                        28
024000990609     C                   move      '1'           *in(ii)
024100990609     C                   movel     msg(3)        v1zmsg
024200990609     C                   goto      ectd01
024300990609E   4C                   ENDIF
024400990609X   3C                   ELSE                                               MERO
024500990609     c                   seton                                        28
024600990609     C                   move      '1'           *in(ii)
024700990609     C                   movel     msg(3)        v1zmsg
024800990609     C                   goto      ectd01
024900990609E   3C                   ENDIF
025000990609     c*
025001141119     c                   move      vfil(ii)      w0030
025002141119     c     w0030         chain     azorg01l
025003141119     c                   if        not %found(azorg01l) or
025004141119     c                             (orgfag <>'F' and orgfag<>'A')
026000990603      *
026100141119IF  2C                   eval      *IN28 = *on
026300141119     c                   move      '1'           *in(ii)
026400141119     c                   movel     msg(3)        v1zmsg
026500141119     c                   goto      ectd01
026501141119     c                   endif
026600141119     c                   movel     orgdes        dfil(ii)
026700141119     c* errore se codice filiale ripetuto
026800990609     c                   z-add     1             yy                2 0
026900141119     c     vfil(ii)      lookup    vfil(yy)                               30
027000990609     c   30ii            ifne      yy
027100990609     c                   seton                                        28
027200990609     c   28              move      '1'           *in(ii)
027300990609     c   28              movel     msg(4)        v1zmsg
027400990609     c   28              goto      ectd01
027500990609     c                   endif
027600990603     c                   else
027700141119     c                   clear                   dfil(ii)
027800990603     c                   end
027900990603     c                   enddo
028000990603     c     ectd01        endsr
028100990604**
028200990604Data errata                                                                    1
028300990604Data dal > di Data al                                                          2
028400141119Codice filiale errato                                                          3
028500141119Codice filiale ripetuto                                                        4
028600081003Data non elaborabile per mancanza di dati
