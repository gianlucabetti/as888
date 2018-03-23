000100970910     H*------------------------------------------------------------------------*
000200141126     H*   Creazione file PC per traini        - filtro
000300970910     H*------------------------------------------------------------------------*
000400970910     H DECEDIT('0,') DATEDIT(*DMY.)
000500970910     F*------------------------------------------------------------------------*
000600970910     F* DATA BASE
000700970910     F*------------------------------------------------------------------------*
000800150514     fsisdr01l  if   e           K disk
000900141126     FTNTLs2D   CF   E             WORKSTN
001000060421     d
001100150514     D MSG             S             78    DIM(7) CTDATA PERRCD(1)
001200970910     D*------------
001300970910     D* DS PARAMETRI DI LANCIO
001400970910     D*------------
001500060421     d
001600970910     D KPJBA         E DS
001700141126     D tntls5ds      E DS
001800141126     D tntls6ds      E DS
001900151103     D tntls7ds      E DS
002000060421     d Azuteds       e ds                  extname(Azute00f)
002100060421     d dDatiute      e ds
002200060421     D TIBS34DS      E DS
002300110504     d Dataiso         s               d   datfmt(*iso)
002400110504     d Dataeur         s               d   datfmt(*eur)
002500141126     d amgdal          s              6  0
002600141126     d amgal           s              6  0
002700110504     d amgdri          s              8  0
002800141126     d wdat            s              8  0
002900141126     d amgdat          s              8  0
003000141126     d dateu           s              8  0
003100150514     d kann            s                   like(sdrann)
003200150514     d kflp            s                   like(sdrflp)  inz(001)
003300150514     d kfla            s                   like(sdrfla)  inz(001)
003400150514     d kmes            s                   like(sdrmes)
003500141126     d Datasys         s               d   datfmt(*iso)  inz(*sys)
003600060421     d
003700970910     C*------------------------------------------------------------------------*
003800970910     C*
003900060420     c* Impostazioni iniziali
004000141126     c                   eval      dataiso=datasys
004100141126     c                   eval      dataeur=dataiso
004200141126     c                   eval      dateu  =%dec(dataiso)
004300110504     c                   clear                   v1cdri
004400141126     c                   clear                   v1cdal
004500141126     c                   clear                   v1cal
004600141126     c                   eval      v1cdri =%dec(dataeur)
004700060421     C* EMETTE IL VIDEO
004800060421     c     for01         tag
004900141126     C                   EXFMT     tls2D01
005000060421     c                   setoff                                       2890
005100110504     c                   setoff                                       41
005200060421     c
005300060421     c* F3 - Fine
005400060421     c   kc              goto      fine
005500970910     C*
005600060421     c* controlli
005700060421     c                   EXSR      CTRD01
005800060421     c  nkf
005900060421     cor 90              goto      for01
006000060421     c
006100060421     c* F6 - Elaborazione
006200060421     c                   if        *inkf
006300151027     c* Eaborazione orari
006400151027     c                   if        v1c1liv<>' '
006500151027     c                   clear                   kpjbu
006600151103     c                   clear                   tntls7ds
006700151103     c                   eval      TLS7DRF=amgdri
006800151103     c                   eval      TLS7tela=v1c1liv
006900151111     c                   eval      tls7date=dateu
007000151111     c                   movel     tntls7ds      kpjbu
007100151028     C                   MOVEL     'TLS7'        KCOAZ
007200151028     C                   CALL      'BCH10'
007300151028     C                   PARM                    KPJBA
007400151027     c                   endif
007500151103     c
007600151103     c**                 if        v1c2liv<>' '
007700151103     c**                 clear                   kpjbu
007800151103     c**                 movel     v1c2liv       kpjbu
007900151103     C**                 MOVEL     'TLS8'        KCOAZ
008000151103     C**                 CALL      'BCH10'
008100151103     C**                 PARM                    KPJBA
008200151103     c**                 endif
008300151027
008400151103     c                   if        v1ctwf='S'
008500141126     c                   clear                   tntls5ds
008600141126     c                   eval      TLS5DRF=amgdri
008700141126     C                   MOVEL(P)  tntls5ds      KPJBU
008800141126     C                   MOVEL     'TLS5'        KCOAZ
008900060421     C                   CALL      'BCH10'
009000060421     C                   PARM                    KPJBA
009100060421     c                   endif
009200151027
009300141126     c                   if        v1cdal>0
009400141126     c                   clear                   tntls6ds
009500141126     c                   eval      TLS6AAMMD=amgdal
009600141126     c                   eval      TLS6AAMMa=amgal
009700150514     c                   if        amgdri>0
009800150514     c                   eval      tls6drf=amgdri
009900150514     c                   else
010000150514     c                   eval      tls6drf=dateu
010100150514     c                   endif
010200141126     C                   MOVEL(P)  tntls6ds      KPJBU
010300141126     C                   MOVEL     'TLS6'        KCOAZ
010400141126     C                   CALL      'BCH10'
010500141126     C                   PARM                    KPJBA
010600141126     c                   endif
010700151027     c                   endif
010800970910     C* FINE PGM
010900060421     c     fine          tag
011000970910     C                   SETON                                        LR
011100970910     C*------------------------------------------------------------------------*
011200060421     C* CONTROLLA CAMPI E DECODIFICA
011300970910     C*------------------------------------------------------------------------*
011400060421     C     CTRD01        BEGSR
011500110504     c                   clear                   amgdat
011600110504     c                   clear                   amgdri
011700110504     c
011800110504     c                   eval      wdat=v1cdri
011900110504     c                   exsr      CTRDEV
012000110504     c                   eval      v1cdri=wdat
012100110504
012200110504     c                   if        *in90
012300141126     c                   seton                                        4128
012400110504     C                   MOVEL     MSG(1)        V1cMSG
012500110504     c                   goto      endct1
012600110504    1c                   endif
012700110504     c                   eval      amgdri=amgdat
012800141126     c* no minore di oggi
012900141126     c                   if        amgdri< dateu
013000141126     c                   seton                                        412890
013100141126     C                   MOVEL     MSG(5)        V1cMSG
013200141126     c                   goto      endct1
013300141126    1c                   endif
013400141126     c
013500141126     c                   if        v1cdal>0
013600141126     c                   clear                   wdat
013700141126     c                   movel     01            wdat
013800141126     c                   move      v1cdal        wdat
013900141126     c                   exsr      CTRDEV
014000141126     c                   move      wdat          v1cdal
014100141126     c
014200141126
014300141126     c                   if        *in90
014400141126     c                   seton                                        4228
014500141126     C                   MOVEL     MSG(2)        V1cMSG
014600141126     c                   goto      endct1
014700141126    1c                   endif
014800141126     c                   MOVEL     amgdat        AMGDAL
014900141126     c                   if        amgdat>dateu
015000141126     c                   seton                                        422890
015100141126     C                   MOVEL     MSG(2)        V1cMSG
015200141126     c                   goto      endct1
015300141126    1c                   endif
015400150514     C* IL MESE/ANNO DEVE ESSERE PRESENTE NEL FILE DEI SALDI
015500150514     c                   movel     amgdal        kann
015600150514     c                   move      amgdal        kmes
015700150514     c     ksdr          chain     sisdr01l
015800150514     c                   if        not %found(sisdr01l)
015900150514     c                   seton                                        422890
016000150514     C                   MOVEL     MSG(7)        V1cMSG
016100150514     c                   goto      endct1
016200150514     c                   endif
016300141126     c                   endif
016400141126     C
016500141126     c                   if        v1cal>0
016600141126     c                   clear                   wdat
016700141126     c                   movel     01            wdat
016800141126     c                   move      v1cal         wdat
016900141126     c                   exsr      CTRDEV
017000141126     c                   move      wdat          v1cal
017100141126     c
017200141126
017300141126     c                   if        *in90
017400141126     c                   seton                                        4328
017500141126     C                   MOVEL     MSG(2)        V1cMSG
017600141126     c                   goto      endct1
017700141126    1c                   endif
017800141126     c                   MOVEL     amgdat        AMGAL
017900141126     c                   if        amgdat>dateu
018000141126     c                   seton                                        432890
018100141126     C                   MOVEL     MSG(2)        V1cMSG
018200141126     c                   goto      endct1
018300141126    1c                   endif
018400141126     C* VERIFICO CHE data AL non si minore della DAL
018500141126     c                   if        amgal< amgdal
018600141126     c                   seton                                        439028
018700141126     C                   MOVEL     MSG(3)        V1cMSG
018800141126     c                   goto      endct1
018900141126    1c                   endif
019000141128     c* max  6 mesi
019100141128     c                   if        amgal-amgdal>6
019200141128     c                   seton                                        439028
019300141128     C                   MOVEL     MSG(6)        V1cMSG
019400141128     c                   goto      endct1
019500141128    1c                   endif
019600141126     c
019700141126     c                   if        amgdal=0
019800141126     c                   seton                                        429028
019900141126     C                   MOVEL     MSG(4)        V1cMSG
020000141126     c                   goto      endct1
020100141126    1c                   endif
020200150514     C* IL MESE/ANNO DEVE ESSERE PRESENTE NEL FILE DEI SALDI
020300150514     c                   movel     amgal         kann
020400150514     c                   move      amgal         kmes
020500150514     c     ksdr          chain     sisdr01l
020600150514     c                   if        not %found(sisdr01l)
020700150514     c                   seton                                        432890
020800150514     C                   MOVEL     MSG(7)        V1cMSG
020900150514     c                   goto      endct1
021000150514     c                   endif
021100141126     c                   endif
021200970910     C*
021300060421     C     endct1        ENDSR
021400110504      *-----------------------------------------------------***********
021500110504      * CONTROLLo Data
021600110504      *-----------------------------------------------------***********
021700110504     c     CTRDEV        BEGSR
021800110504    1c                   if        wdat>0
021900110504     c                   movel     wdat          w002a             2
022000110504     c     *eur          test(d)                 wdat                   30
022100110504    2c                   if        *in30  and w002a='00'
022200110504     c     *dmy          test(d)                 wdat                   30
022300110504     c                   endif
022400110504     c* Errore
022500110504    3c                   if        *in30
022600110504     c                   seton                                        9028
022700110504     c                   goto      endctdev
022800110504     c                   endif
022900110504     c
023000110504     c                   if        w002a='00'
023100110504     c     *dmy          move      wdat          dataeur
023200110504     c                   else
023300110504     c     *eur          move      wdat          dataeur
023400110504     c                   endif
023500110504     c
023600110504     c                   else
023700110504     c                   seton                                        9028
023800110504     c                   goto      endctdev
023900110504     c                   endif
024000110504     c* la trasformo in AAAAAMMGG - iso
024100110504     c                   move      dataeur       wdat
024200110504     c                   movel     dataeur       dataiso
024300110504     c     *iso          movel     dataiso       amgdat            8 0
024400110504     c
024500110504     c
024600110504     c     endctdev      ENDSR
024700970910     C*------------------------------------------------------------------------*
024800970910     C* *INZSR - OPERAZIONI INIZIALI
024900970910     C*------------------------------------------------------------------------*
025000970910     C     *INZSR        BEGSR
025100970910     C*---
025200970910     C* RICEVIMENTO PARAMETRI
025300970910     C*---
025400970910     C     *ENTRY        PLIST
025500970612     C                   PARM                    KPJBA
025600060421     c
025700110504     c                   z-add     1             codut             1 0
025800060421      *
025900060421     c     *dtaara       define    §azute        azuteds
026000060421     c     *dtaara       define    §datiute      ddatiute
026100060421     c                   in(E)     *dtaara
026200060421    1c                   If        %error  or RSUT = *blanks
026300060421     c                   Clear                   Tibs34ds
026400060421     c                   Call      'TIBS34R'
026500060421     c                   Parm                    Tibs34ds
026600060421     c                   In        *dtaara
026700060421    1c                   EndIf
026800060421    cc                   movel     rsut          v1crsu
026900150514     c
027000150514     c     ksdr          klist
027100150514     c                   kfld                    kann
027200150514     c                   kfld                    kflp
027300150514     c                   kfld                    kfla
027400150514     c                   kfld                    kmes
027500060421     c                   ENDSR
027600970910     O*------------------------------------------------------------------------*
027700970910** MSG - MESSAGGI
027800110504Data di riferimento errata
027900141126Mese/anno errato
028000141126Mese/anno AL minore  mese/anno DAL
028100141126Immettere anche mese/anno  DAL
028200141126Data riferimento no minore di oggi
028300141128Il range di elaborazione non deve essere superiore ai 6 mesi
028400150514Mese/anno non presente nel file Saldi
