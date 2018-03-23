000100970910     H*------------------------------------------------------------------------*
000200141126     H*   Creazione file PC per traini        - filtro
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000701150514     fsisdr01l  if   e           K disk
000800141126     FTNTLs2D   CF   E             WORKSTN
000900060421     d
001000150514     D MSG             S             78    DIM(7) CTDATA PERRCD(1)
001100970910     D*------------
001200970910     D* DS PARAMETRI DI LANCIO
001300970910     D*------------
001400060421     d
001500970910     D KPJBA         E DS
001600141126     D tntls5ds      E DS
001700141126     D tntls6ds      E DS
001800060421     d Azuteds       e ds                  extname(Azute00f)
001900060421     d dDatiute      e ds
002000060421     D TIBS34DS      E DS
002100110504     d Dataiso         s               d   datfmt(*iso)
002200110504     d Dataeur         s               d   datfmt(*eur)
002300141126     d amgdal          s              6  0
002400141126     d amgal           s              6  0
002500110504     d amgdri          s              8  0
002600141126     d wdat            s              8  0
002700141126     d amgdat          s              8  0
002800141126     d dateu           s              8  0
002801150514     d kann            s                   like(sdrann)
002802150514     d kflp            s                   like(sdrflp)  inz(001)
002803150514     d kfla            s                   like(sdrfla)  inz(001)
002804150514     d kmes            s                   like(sdrmes)
002900141126     d Datasys         s               d   datfmt(*iso)  inz(*sys)
003000060421     d
003100970910     C*------------------------------------------------------------------------*
003200970910     C*
003300060420     c* Impostazioni iniziali
003400141126     c                   eval      dataiso=datasys
003500141126     c                   eval      dataeur=dataiso
003600141126     c                   eval      dateu  =%dec(dataiso)
003700110504     c                   clear                   v1cdri
003800141126     c                   clear                   v1cdal
003900141126     c                   clear                   v1cal
004000141126     c                   eval      v1cdri =%dec(dataeur)
004100060421     C* EMETTE IL VIDEO
004200060421     c     for01         tag
004300141126     C                   EXFMT     tls2D01
004400060421     c                   setoff                                       2890
004500110504     c                   setoff                                       41
004600060421     c
004700060421     c* F3 - Fine
004800060421     c   kc              goto      fine
004900970910     C*
005000060421     c* controlli
005100060421     c                   EXSR      CTRD01
005200060421     c  nkf
005300060421     cor 90              goto      for01
005400060421     c
005500060421     c* F6 - Elaborazione
005600060421     c                   if        *inkf
005700141126     c                   if        v1cdri>0
005800141126     c                   clear                   tntls5ds
005900141126     c                   eval      TLS5DRF=amgdri
006000141126     C                   MOVEL(P)  tntls5ds      KPJBU
006100141126     C                   MOVEL     'TLS5'        KCOAZ
006200060421     C                   CALL      'BCH10'
006300060421     C                   PARM                    KPJBA
006400060421     c                   endif
006500141126     c                   if        v1cdal>0
006600141126     c                   clear                   tntls6ds
006700141126     c                   eval      TLS6AAMMD=amgdal
006800141126     c                   eval      TLS6AAMMa=amgal
006801150514     c                   if        amgdri>0
006802150514     c                   eval      tls6drf=amgdri
006803150514     c                   else
006804150514     c                   eval      tls6drf=dateu
006805150514     c                   endif
006900141126     C                   MOVEL(P)  tntls6ds      KPJBU
007000141126     C                   MOVEL     'TLS6'        KCOAZ
007100141126     C                   CALL      'BCH10'
007200141126     C                   PARM                    KPJBA
007300141126     c                   endif
007400141126     c                   endif
007500970910     C* FINE PGM
007600060421     c     fine          tag
007700970910     C                   SETON                                        LR
007800970910     C*------------------------------------------------------------------------*
007900060421     C* CONTROLLA CAMPI E DECODIFICA
008000970910     C*------------------------------------------------------------------------*
008100060421     C     CTRD01        BEGSR
008200110504     c                   clear                   amgdat
008300110504     c                   clear                   amgdri
008400110504     c
008500110504     c                   if        v1cdri>0
008600110504     c                   eval      wdat=v1cdri
008700110504     c                   exsr      CTRDEV
008800110504     c                   eval      v1cdri=wdat
008900110504
009000110504     c                   if        *in90
009100141126     c                   seton                                        4128
009200110504     C                   MOVEL     MSG(1)        V1cMSG
009300110504     c                   goto      endct1
009400110504    1c                   endif
009500110504     c                   eval      amgdri=amgdat
009600141126     c* no minore di oggi
009700141126     c                   if        amgdri< dateu
009800141126     c                   seton                                        412890
009900141126     C                   MOVEL     MSG(5)        V1cMSG
010000141126     c                   goto      endct1
010100141126    1c                   endif
010200110504     c                   endif
010300141126     c
010400141126     c                   if        v1cdal>0
010500141126     c                   clear                   wdat
010600141126     c                   movel     01            wdat
010700141126     c                   move      v1cdal        wdat
010800141126     c                   exsr      CTRDEV
010900141126     c                   move      wdat          v1cdal
011000141126     c
011100141126
011200141126     c                   if        *in90
011300141126     c                   seton                                        4228
011400141126     C                   MOVEL     MSG(2)        V1cMSG
011500141126     c                   goto      endct1
011600141126    1c                   endif
011700141126     c                   MOVEL     amgdat        AMGDAL
011800141126     c                   if        amgdat>dateu
011900141126     c                   seton                                        422890
012000141126     C                   MOVEL     MSG(2)        V1cMSG
012100141126     c                   goto      endct1
012200141126    1c                   endif
012201150514     C* IL MESE/ANNO DEVE ESSERE PRESENTE NEL FILE DEI SALDI
012202150514     c                   movel     amgdal        kann
012203150514     c                   move      amgdal        kmes
012204150514     c     ksdr          chain     sisdr01l
012205150514     c                   if        not %found(sisdr01l)
012206150514     c                   seton                                        422890
012207150514     C                   MOVEL     MSG(7)        V1cMSG
012208150514     c                   goto      endct1
012209150514     c                   endif
012300141126     c                   endif
012400141126     C
012500141126     c                   if        v1cal>0
012600141126     c                   clear                   wdat
012700141126     c                   movel     01            wdat
012800141126     c                   move      v1cal         wdat
012900141126     c                   exsr      CTRDEV
013000141126     c                   move      wdat          v1cal
013100141126     c
013200141126
013300141126     c                   if        *in90
013400141126     c                   seton                                        4328
013500141126     C                   MOVEL     MSG(2)        V1cMSG
013600141126     c                   goto      endct1
013700141126    1c                   endif
013800141126     c                   MOVEL     amgdat        AMGAL
013900141126     c                   if        amgdat>dateu
014000141126     c                   seton                                        432890
014100141126     C                   MOVEL     MSG(2)        V1cMSG
014200141126     c                   goto      endct1
014300141126    1c                   endif
014400141126     C* VERIFICO CHE data AL non si minore della DAL
014500141126     c                   if        amgal< amgdal
014600141126     c                   seton                                        439028
014700141126     C                   MOVEL     MSG(3)        V1cMSG
014800141126     c                   goto      endct1
014900141126    1c                   endif
014901141128     c* max  6 mesi
014902141128     c                   if        amgal-amgdal>6
014903141128     c                   seton                                        439028
014904141128     C                   MOVEL     MSG(6)        V1cMSG
014905141128     c                   goto      endct1
014906141128    1c                   endif
015000141126     c
015100141126     c                   if        amgdal=0
015200141126     c                   seton                                        429028
015300141126     C                   MOVEL     MSG(4)        V1cMSG
015400141126     c                   goto      endct1
015500141126    1c                   endif
015501150514     C* IL MESE/ANNO DEVE ESSERE PRESENTE NEL FILE DEI SALDI
015502150514     c                   movel     amgal         kann
015503150514     c                   move      amgal         kmes
015504150514     c     ksdr          chain     sisdr01l
015505150514     c                   if        not %found(sisdr01l)
015506150514     c                   seton                                        432890
015507150514     C                   MOVEL     MSG(7)        V1cMSG
015508150514     c                   goto      endct1
015509150514     c                   endif
015600141126     c                   endif
015700970910     C*
015800060421     C     endct1        ENDSR
015900110504      *-----------------------------------------------------***********
016000110504      * CONTROLLo Data
016100110504      *-----------------------------------------------------***********
016200110504     c     CTRDEV        BEGSR
016300110504    1c                   if        wdat>0
016400110504     c                   movel     wdat          w002a             2
016500110504     c     *eur          test(d)                 wdat                   30
016600110504    2c                   if        *in30  and w002a='00'
016700110504     c     *dmy          test(d)                 wdat                   30
016800110504     c                   endif
016900110504     c* Errore
017000110504    3c                   if        *in30
017100110504     c                   seton                                        9028
017200110504     c                   goto      endctdev
017300110504     c                   endif
017400110504     c
017500110504     c                   if        w002a='00'
017600110504     c     *dmy          move      wdat          dataeur
017700110504     c                   else
017800110504     c     *eur          move      wdat          dataeur
017900110504     c                   endif
018000110504     c
018100110504     c                   else
018200110504     c                   seton                                        9028
018300110504     c                   goto      endctdev
018400110504     c                   endif
018500110504     c* la trasformo in AAAAAMMGG - iso
018600110504     c                   move      dataeur       wdat
018700110504     c                   movel     dataeur       dataiso
018800110504     c     *iso          movel     dataiso       amgdat            8 0
018900110504     c
019000110504     c
019100110504     c     endctdev      ENDSR
019200970910     C*------------------------------------------------------------------------*
019300970910     C* *INZSR - OPERAZIONI INIZIALI
019400970910     C*------------------------------------------------------------------------*
019500970910     C     *INZSR        BEGSR
019600970910     C*---
019700970910     C* RICEVIMENTO PARAMETRI
019800970910     C*---
019900970910     C     *ENTRY        PLIST
020000970612     C                   PARM                    KPJBA
020100060421     c
020200110504     c                   z-add     1             codut             1 0
020300060421      *
020400060421     c     *dtaara       define    §azute        azuteds
020500060421     c     *dtaara       define    §datiute      ddatiute
020600060421     c                   in(E)     *dtaara
020700060421    1c                   If        %error  or RSUT = *blanks
020800060421     c                   Clear                   Tibs34ds
020900060421     c                   Call      'TIBS34R'
021000060421     c                   Parm                    Tibs34ds
021100060421     c                   In        *dtaara
021200060421    1c                   EndIf
021300060421    cc                   movel     rsut          v1crsu
021301150514     c
021302150514     c     ksdr          klist
021303150514     c                   kfld                    kann
021304150514     c                   kfld                    kflp
021305150514     c                   kfld                    kfla
021306150514     c                   kfld                    kmes
021400060421     c                   ENDSR
021500970910     O*------------------------------------------------------------------------*
021600970910** MSG - MESSAGGI
021700110504Data di riferimento errata
021800141126Mese/anno errato
021900141126Mese/anno AL minore  mese/anno DAL
022000141126Immettere anche mese/anno  DAL
022100141126Data riferimento no minore di oggi
022200141128Il range di elaborazione non deve essere superiore ai 6 mesi
022300150514Mese/anno non presente nel file Saldi
