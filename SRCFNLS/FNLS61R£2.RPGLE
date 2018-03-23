000100020402     H DECEDIT('0,') DATEDIT(*DMY/)
000200940929     H* FNLS61R *-----------------------------------------------------*
000300941006     H*             -  PULIZIA BOLLE PARTENZE -           ESEGUE      *
000400060120      * COMPILARE CON
000500060120      *               OVRDBF FILE(TNCSB03L) TOFILE(GAITRAGRPS/TNCSB03L)
000600930906     F*---------------------------------------------------------------*
000700930906     F* DATA BASE                                                     *
000800930906     F*---------------------------------------------------------------*
000900000801     FFNBLP37L  IF   E           K DISK
001000940929     F                                     RENAME(FNBLP000:FNBLP037)
001100050906     FTigcp01L  IF   E           K DISK
001200020528     FFndcf01l  IF   E           K DISK
001300020529     FFndfa01l  IF   E           K DISK
001400051115     FFiar901L  IF   E           K DISK
001500060428     FFiar601L  IF   E           K DISK
001600060216     FFiar401L  IF   E           K DISK
001601130903     FFiar501L  IF   E           K DISK
001602130903     FFiar801L  IF   E           K DISK
001700990810     FFNEVB04L  IF   E           K DISK
001800990810     F                                     RENAME(FNEVB000:FNEVB004)
001900941102     FFNCSB01L  IF   E           K DISK
002000980721     FAZORG01L  IF   E           K DISK
002100010605     Ffnarb01l  IF   E           K DISK
002200020408     FTABEL00F  if   E           K disk
002300020409     F* ARCHIVI  PADRONCINI DETTAGLIO TASSAZIONE
002400060206     Ffiftd25L  IF   E           K DISK
002500020409     Ffifsd05L  IF   E           K DISK
002600020409     F                                     RENAME(fiftd000:fifsd000)
002700060120     F* ARCHIVIO Contrassegni di sede
002800060120     FTNCSB03L  IF   E           K DISK
002900060120     f
003000020404     Ffnls61p   O    E             PRINTER USROPN oflind(*in99)
003100040409     F* ARCHIVI  RECLAMI
003200040409     Ffitgd02l  IF   E           K DISK
003300940930     F*
003400010605     d msg             s             70    dim(2) CTDATA PERRCD(1)
003500020408     d cmd             s             50    dim(2) CTDATA PERRCD(1)
003600060428     D CBP             S              2    DIM(99)
003700101007     D ris             S              3    DIM(50)
003800020402
003900020402     d data_EUR        s               d   datfmt(*EUR)
004000020404     d data_DMY        s               d   datfmt(*DMY)
004100020408     d data_ISO        s               d   datfmt(*ISO)
004200020409     d wdate_inv       s               d   datfmt(*ISO)
004300020402     d data_Alfa       s             10a
004400020408     d dtsita_ymd      s              8  0
004500020408     d dtsimp_ymd      s              8  0
004600020408     d dtsrsv_ymd      s              8  0
004700020408     d dtscan_ymd      s              8  0
004800020408     d dtsncn_ymd      s              8  0
004900020408     d dpsita_ymd      s              8  0
005000020408     d dpsimp_ymd      s              8  0
005100020408     d dpsrsv_ymd      s              8  0
005200020408     d dpscan_ymd      s              8  0
005300020404     d wopn            s              1a
005400040409
005500040409     d ktgtor          s                   like(tgdtor) inz('S')
005600040409     d ktgogg          s                   like(tgdogg)
005700060428     d k6trc           s                   like(ar6trc) inz('1')
005701130903     d ktrd            s                   like(ar5trd) inz('GEN')
005702130903     d ktmd            s                   like(ar8tmd) inz('§PDF')
005703130903     d wesitopdf       s              1A
005704130903       // - Parametri per richiamo pgm pulizia file PDF
005705130903     d pIn_FolderPDF   s            256a
005706130903     d pIn_NamePDF     s            256a
005707130903       // '0'=OK '1'=errore
005708130903     d pOut_esito      s              1A
005800040409
005900930906     D*---------------------------------------------------------------*
006000930906     D* DS                                                            *
006100930906     D*---------------------------------------------------------------*
006200930906     D*-------------
006300961204     D* PARAMETRI RICEVUTI DAI PGM CHIAMANTI
006400930906     D*-------------
006500940930     D PARAM1          DS
006600000529     D***
006700961204     D* DATA PULIZIA BOLLE PARTENZE (CON LNP NON ESTERA)
006800940930     D  PA1DBR                 1      8  0
006900980721     D***
007000961204     D* DATA PULIZIA BOLLE PARTENZE (CON LNP ESTERA - BOLLE IMPORT)
007100961204     D  PA1PBI               137    144  0
007200980721     D***
007300980721     D* DATA PULIZIA BOLLE CON RISERVA
007400980721     D  PA1BLR               161    168  0
007500000529     D***
007600990126     D* DATA PULIZIA BOLLE CON C.A.
007700990714     D  PA1BCA               169    176  0
007800000529     D***
007900000529     D* DATA PULIZIA BOLLE PARTENZE - POSTE
008000000529     D  POSTBR               177    184  0
008100000529     D***
008200000529     D* DATA PULIZIA BOLLE PARTENZE - BOLLE IMPORT - POSTE
008300000529     D  POSTBI               185    192  0
008400000529     D***
008500000529     D* DATA PULIZIA BOLLE CON RISERVA - POSTE
008600000529     D  POSTLR               193    200  0
008700000529     D***
008800000529     D* DATA PULIZIA BOLLE CON C.A. - POSTE
008900000529     D  POSTCA               201    208  0
009000040423     D***
009100040423     D* DATA PULIZIA BOLLE CON RECLAMI
009200040423     D  DATREC               233    240  0
009300010605     d PARAM           DS
009400010605     d  msgaas                 1      4  0
009500010605     d  barrw1                 5      5
009600010605     d  msglnp                 6      8  0
009700010605     d  barrw2                 9      9
009800010605     d  msgnrs                10     11  0
009900010605     d  barrw3                12     12
010000010605     d  msgnsp                13     19  0
010100930906     D*-------------
010200930906     D* PARAMETRI ESTERNI
010300930906     D*-------------
010400000801     D* DS PER FNLS63R - CANCELLAZIONE BOLLA PARTENZA SINGOLA
010500000801     D DSLS63        E DS                  EXTNAME(FNLS63DS)
010600000801     D*
010700000529     D OG143         E DS
010800010129     D*
010900010129     D DSBL4J        E DS
010901130903     D*
010902130903     D Dar5gen       E DS
010903130903     D Dar8§pdf      E DS
011000010507      *
011100010507     D FNLGZ5DS      E DS
011200010507     D*
011300010507     D KPJBA         E DS
011400000801     D*
011500990823     D* DS PER TIBS02DS- RICERCA TABELLE
011600990823     D DSBS02        E DS                  EXTNAME(TIBS02DS)
011700010605     D fidn11ds      E DS
011800010605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
011900010605     D CNCR80        E DS
012000020408     D*
012100020408     D ds5a2         E DS
012200020408     D ds5a3         E DS
012300060428     D ds3a          E DS
012400101007     D ds2a          E DS
012500930906     D*-------------
012600930906     D* DS DATA SPEDIZIONE
012700930906     D*-------------
012800900620     D                 DS
012900940929     D  BLPAAS                 1      4  0
013000940929     D  BLPMGS                 5      8  0
013100940929     D  BLPDSP                 1      8  0
013200040409     D*-------------
013300040409     D* DS OGGETTO RECLAMO
013400040409     D*-------------
013500040409     D                 DS
013600040409     D  W_lnp                         3  0
013700040409     D  W_nrs                         2  0
013800040409     D  W_nsp                         7  0
013900040409     D  W_aas                         4  0
014000040409     D  Wspe                   1     16  0 inz
014100040812
014200040812      * ds per il controllo della presenza di CA per la spedizione richiesta
014300040812     d FIDN12DS      e ds
014400040812     d  skKey                 26    305    dim(20)
014500040812     d  skAnn                306    325    dim(20)
014600040812     d  skDca                326    485  0 dim(20)
014700040812     d  skFca                486    545  0 dim(20)
014800040812     d  skDch                546    705  0 dim(20)
014900040812     d  skCch                706    745    dim(20)
015000050413
015100040812     d*
015200040812     d dsKey           ds
015300040812     d  dsaac                         4  0
015400040812     d  dsfil                         3  0
015500040812     d  dsnca                         7  0
015600060428     D cndizion      E DS
015700060428     D                                     BASED(nullPtr)
015800060428     D                                     PREFIX(diz)
015900040812
016000040812     D*
016100040812     D* DEFINIZIONE VARIABILI D WRK
016200040812     D*
016300040812     D WDCH            S              8  0
016400040812     D WDCH_dcb        S              8  0
016500060428
016600060428     D nullPtr         S               *
016700060428     D prmAas          S
016800060428     D                                     LIKE(diz$Aas)
016900060428     D prmLnp          S
017000060428     D                                     LIKE(diz$Lnp)
017100060428     D prmNrs          S
017200060428     D                                     LIKE(diz$Nrs)
017300060428     D prmNsp          S
017400060428     D                                     LIKE(diz$Nsp)
017500060428     D prmFiv          S
017600060428     D                                     LIKE(diz$Fiv)
017700060428     D prmDft          S
017800060428     D                                     LIKE(diz$Dft)
017900060428     D prmNft          S
018000060428     D                                     LIKE(diz$Nft)
018100060428     D prmEsito        S             10I 0
018200040812
018300020402     D*
018400020402     D* DEFINIZIONE COSTANTI
018500020402     D*
018600020528     D CBGIAC          C                   CONST('Bolla giacente in fase')
018700020402     D CBRISE          C                   CONST('Bolla con riserva consegnata -
018800020402     D                                     il            ')
018900020528     D CBCANA          C                   CONST('Bolla con C.A. aperta - Fase')
019000020403     D CBCANC          C                   CONST('Bolla con C.A. chiusa il')
019100020402     D CBCANN1         C                   CONST('Presente C.A. chiusa il ')
019200020402     D CBCANN2         C                   CONST(' non ancora stampata nell''el-
019300020402     D                                     enco C.A. da archiviare')
019400020403     D CBCcass         C                   CONST('Bolla con C/Assegno - Dati in-
019500020403     D                                     casso mancanti')
019600020408     D CBncon          C                   CONST('Bolla non consegnata')
019700020411     D CBnconca        C                   CONST('Bolla non consegnata con C.A.-
019800020411     D                                     ')
019900020409     D CBcpdr          C                   CONST('Bolla presente su archivio co-
020000020409     D                                     nteggi padroncini')
020100080521     D CBcrec          C                   CONST('Bolla con reclamo APERTO')
020200060502     D CBcprp          C                   CONST('Prepagato non incassato -')
020201130904     D CBcpdf          C                   CONST('Pulizia file PDF non riuscita-
020202130904     d                                     ')
020300930906     C*---------------------------------------------------------------*
020400930906     C*                    MAIN                                       *
020500930906     C* PROGRAMMA PRINCIPALE PULIZIA BOLLE PARTITE                    *
020600930906     C*---------------------------------------------------------------*
020700000801      *
020800940929     C                   Z-ADD     *ZEROS        KFIL
020900930907     C                   SETOFF                                           34
021000930907     C*
021100930907DO  1C     *IN34         DOWEQ     *OFF
021200940929     C     KFIL          SETGT     FNBLP37L
021300940929     C                   READ      FNBLP37L                               34
021400930907     C*
021500930907IF  2C     *IN34         IFEQ      *OFF
021600940929     C                   Z-ADD     BLPLNP        KFIL
021700940929     C                   Z-ADD     1             KDCM
021800980721     C**
021900980721     C* VEDO SE SI TRATTA DI LNP ESTERA
022000020211     C                   MOVE      *blanks       lnpntw
022100980721     C     BLPLNP        CHAIN     AZORG01L                           35
022200000529     C  N35              MOVEL     ORGDE3        OG143
022300020211     C  N35              MOVEL     §ogntw        lnpntw
022400020211     c**
022500020211     C     lnpntw        ifeq      'EEX'
022600020211     C     lnpntw        oreq      'EUP'
022700020211     C     lnpntw        oreq      'FED'
022800020211     C     lnpntw        oreq      'DPD'
022900000529     C* IMPOSTO DATA PULIZIA BOLLE IMPORT (POSTE O NON POSTE)
023000980721     C                   Z-ADD     PA1PBI        WDAPUL            8 0
023100020408     C                   Z-ADD     dtsimp_ymd    WDAprt            8 0
023200980721     C                   ELSE
023300000529     C* IMPOSTO DATA PULIZIA BOLLE EXPORT E ITALIA (POSTE O NON POSTE)
023400020211     C     lnpntw        IFNE      'PPT'
023500980721     C                   Z-ADD     PA1DBR        WDAPUL            8 0
023600020408     C                   Z-ADD     dtsita_ymd    WDAprt            8 0
023700000529     C                   ELSE
023800020408     C                   Z-ADD     dpsita_ymd    WDAprt            8 0
023900000529     C                   ENDIF
024000980721     C                   ENDIF
024100930907     C*
024200930907     C* MI POSIZIONO PER LNP TROVATA E DATA CONSEGNA MERCE > DI ZERO
024300940929     C     KBL2          SETLL     FNBLP37L
024400940929     C     KFIL          READE     FNBLP37L                               35
024500930907     C*
024600930907     C* FINCHE FINE FILE E DATA CONSEGNA MERCE MINORE ALLA DATA PULIZIA
024700930907DO  3C     *IN35         DOWEQ     *OFF
024800980721     C     BLPDCM        ANDLT     WDAPUL
024900930907     C* PULIZIA BOLLE
025000930907     C                   EXSR      CANC
025100930907     C*
025200940929     C     KFIL          READE     FNBLP37L                               35
025300930907E   3C                   ENDDO
025400930907     C*
025500930907E   2C                   ENDIF
025600930907E   1C                   ENDDO
025700020408     c*
025800020409     c* II° CICLO PER STAMPARE SPEDIZIONI NON CONSEGNATE OLTRE X GIORNI
025900020409     c* DATA SPEDIZIONE
026000020408     C                   Z-ADD     *ZEROS        KFIL
026100020408     C                   SETOFF                                           34
026200020408     C*
026300020408DO  1C     *IN34         DOWEQ     *OFF
026400020408     C     KFIL          SETGT     FNBLP37L
026500020408     C                   READ      FNBLP37L                               34
026600020408     C*
026700020408IF  2C     *IN34         IFEQ      *OFF
026800020408     C                   Z-ADD     BLPLNP        KFIL
026900020408     C                   Z-ADD     0             KDCM
027000020408     c**
027100020408     C*
027200020408     C* MI POSIZIONO PER LNP TROVATA E DATA CONSEGNA MERCE = 0
027300020408     C     KBL2          SETLL     FNBLP37L
027400020408     C     Kbl2          READE     FNBLP37L                               35
027500020408     C*
027600020408DO  3C     *IN35         DOWEQ     *OFF
027700020409     C     BLPDsp        iflt      dtsncn_ymd
027800080604     c* Escludo la linea 101 gestita dalla sede
027900080604     c     blplna        andne     101
028000020411     c* Guardo se presente c.a.
028100040812      *
028200040812      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
028300040812      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
028400040812      * legate alla spedizione che passo
028500040812     c                   clear                   fidn12ds
028600040812     c                   eval      i12aas = BLPAAS
028700040812     c                   eval      i12lnp = BLPLNP
028800040812     c                   eval      i12nrs = BLPNRS
028900040812     c                   eval      i12nsp = BLPNSP
029000040812     c                   eval      i12fel = 'E'
029100040812     c*
029200040812     c                   call      'FIDN12R'
029300040812     c                   parm                    fidn12ds
029400040812     c*
029500040812     c                   setoff                                       32
029600040812     c*
029700040812     c* se non ci sono errori
029800080521     c****               if        o12err = ' '
029900040812     c* se numero di CA trovate maggiore di zero
030000080521     c****               if        o12nca > 0
030100080521     c****               seton                                        32
030200080521     c****               endif
030300080521     c****               endif
030400080521     c
030500080521     c                   clear                   wchiusaCA         1
030600080521    5c                   if        o12err = ' ' AND o12nca > 0
030700080521     c                   seton                                        32
030800080521     c                   eval      WchiusaCA='S'
030900080521     c
031000080521     c* Stampo solo se tutte le c.a. sono chiuse
031100080521     c                   z-add     *zeros        ii                2 0
031200080521    6c     1             do        o12nca        ii
031300080521     c                   movea     skkey(ii)     dskey
031400080521     C                   Z-ADD     skdch(ii)     dctdch            8 0
031500080521     c
031600080521      * se CA chiusa stampo,imposto il flag e vado a fine
031700080521    7c                   If        dctdch = 0
031800080521     c                   clear                   wchiusaCA
031900080521    7c                   endif
032000080521    6c                   enddo
032100080521    5c                   endif
032200080521     c
032300040812     c**
032400020411     c   32§ogpt         ifne      'S'
032500020411     C                   Z-ADD     dtscan_ymd    WDAprt
032600020411     c                   else
032700020411     C                   Z-ADD     dpscan_ymd    WDAprt
032800020411     c                   endif
032900080521     C***  *in32         ifeq      *off
033000080521     c***  blpdsp        orlt      wdaprt
033100080521    c
033200080521     c                   if        (*in32 and wchiusaCA='S' and
033300080521    5c                             BLPdsp<wdaprt) or not *in32
033400080521     c
033500020409     c                   exsr      ovrprt
033600020411     c  n32              movel(P)  cbncon        p01mes
033700020411     c   32              movel(P)  cbnconca      p01mes
033800020409     c                   exsr      ried1
033900020409     c                   write     ls61d1
034000020411     c                   endif
034100020408     c                   endif
034200020408     C*
034300020408     C     Kbl2          READE     FNBLP37L                               35
034400020408E   3C                   ENDDO
034500020408     C*
034600020408E   2C                   ENDIF
034700020408E   1C                   ENDDO
034800990823     C*
034900020404     c     wopn          ifeq      '1'
035000020404     c                   write     ls61z1
035100020404     c                   endif
035200990823     C* CHIUDO CON LR
035300990823     C                   CLEAR                   DSBS02
035400990823     C                   MOVEL     'C'           T02TLA
035500990823     C                   CALL      'TIBS02R'
035600990823     C                   PARM                    KPJBA
035700990823     C                   PARM                    DSBS02
035800000925     C*
035900000925     C                   CLEAR                   DSLS63
036000000925     C                   MOVEL     'C'           I63TLA
036100000925     C                   MOVEL     DSLS63        KPJBU
036200000925     C                   CALL      'FNLS63R'
036300000925     C                   PARM                    KPJBA
036400951010     C*
036500930907     C                   SETON                                            LR
036600930902     C*---------------------------------------------------------------*
036700930902     C* ROUTINE DI CANCELLAZIONE BOLLE PARTITE                        *
036800930902     C*---------------------------------------------------------------*
036900990126     C     CANC          BEGSR
037000010502      *
037100990126     C                   MOVEL     'S'           WOK               1
037200010502     C                   clear                   WGIACE            1
037300090305     c
037400090305     c* Se c'e' bolla arrivi, non cancello la bolla
037500090305     c     kblp          setll     fnarb01l
037600090305     c                   if        %equal(fnarb01l)
037700090305     c                   clear                   WOK
037800090305     c                   endif
037900090305     c
038000090305     c                   if        wok='S'
038100010502      *
038200010502      * GIACENZA: se non trovata CANCELLO BOLLA
038300050906      *           se trovata ma è annullata o in fase 60 CANCELLO BOLLA
038400010502      *           negli altri casi non CANCELLO BOLLA
038500050906     C     KGCP          CHAIN     tigcp01L
038600010417      *
038700050906     c                   SELECT
038800050906     c                   WHEN      not %found(tigcp01L)
038900050906     c                   WHEN      GCPATB <> *blanks or GCPFAS = 60
039000050906     c                   OTHER
039100050906     c                   eval      Wgiace = 'S'
039200010502     c                   clear                   WOK
039300020402     c* stampo motivazione
039400020408     c     blpdsp        iflt      wdaprt
039500020402     c                   exsr      ovrprt
039600020402     c                   movel(P)  cbgiac        p01mes
039700020528     c* prendo la decodifica fase
039800020528     C                   MOVEL     '2Y'          COD
039900050906     C                   MOVEL(P)  gcpfas        KEY
040000020528     C     ktab          CHAIN     TABEL00F                           30
040100020528     C   30              MOVEL     *ALL'?'       decfas           45
040200020528     C  N30              MOVEL     TBLUNI        decfas
040300050906     c                   move      gcpfas        wcpfas            3
040400020530     c                   eval      p01mes = %trim(p01mes) + ' ' + wcpfas + '-'
040500020528     c                             + decfas
040600020404     c                   exsr      ried1
040700020404     c                   write     ls61d1
040800020408     c                   endif
040900050906     c                   ENDSL
041000010502      *
041100000529     C* IMPOSTO DATA A SECONDA SE E' POSTE O NO
041200000529     C     §OGPT         IFNE      'S'
041300000529     C                   Z-ADD     PA1BLR        WDATA£            8 0
041400020408     C                   Z-ADD     dtsrsv_ymd    WDApr£            8 0
041500000529     C                   ELSE
041600000529     C                   Z-ADD     POSTLR        WDATA£            8 0
041700020408     C                   Z-ADD     dpsrsv_ymd    WDApr£            8 0
041800000529     C                   ENDIF
041900000529     C*
042000980721     C* SE LA BOLLA PRESENTA UN EVENTO DI RISERVA, CANCELLO SOLO SE
042100980721     C*  LA DATA DI CONSEGNA < ALLA DATA PULIZIA BOLLE CON RISERVA
042200980721    1C     WOK           IFEQ      'S'
042300101007     c                   setoff                                       32
042400101007     c                   z-add     1             yy
042500101007
042600101007     c                   dow       ris(YY)<>*blanks
042700101007     c                   eval      wcev=ris(YY)
042800101007     c
042900101007     C     KEVB4         SETLL     FNEVB04L
043000101007     c                   if        %equal(fnevb04l)
043100101007     c                   seton                                        32
043200101007     c                   leave
043300101007     c                   else
043400101007     c                   endif
043500101007     c                   add       1             yy
043600101007     c                   enddo
043700101007     c
043800101007     C***  KEVB4         SETLL     FNEVB04L                               32
043900980721    2C     *IN32         IFEQ      *ON
044000000529     C     BLPDCM        ANDGE     WDATA£
044100020402     c* stampo motivazione
044200020408     c     blpdsp        iflt      wdapr£
044300020402     c                   exsr      ovrprt
044400020402     c     *ymd          move      blpdcm        data_EUR
044500020402     c                   move      data_EUR      data_Alfa
044600020402     c                   movel(P)  cbrise        p01mes
044700020402     c                   eval      p01mes = %trim(p01mes) + ' ' + data_Alfa
044800020404     c                   exsr      ried1
044900020404     c                   write     ls61d1
045000020408     c                   endif
045100980721     C                   CLEAR                   WOK
045200980721    2C                   ENDIF
045300980721    1C                   ENDIF
045400990126     C**
045500010502     C* SE C'E' C.A. DEVE ESSERE CHIUSA
045600990126     C* E LA DATA DI PULIZIA DEVE ESSERE > DELLA DATA CHIUSURA
045700990126     C                   CLEAR                   WDCH
045800020530     C                   CLEAR                   WDCH_dcb
045900020408     C* IMPOSTO DATA stampa A SECONDA SE E' POSTE O NO
046000020408     C     §OGPT         IFNE      'S'
046100020408     C                   Z-ADD     dtscan_ymd    WDApr£
046200020408     C                   ELSE
046300020408     C                   Z-ADD     dpscan_ymd    WDApr£
046400020408     C                   ENDIF
046500040812      *
046600040812      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
046700040812      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
046800040812      * legate alla spedizione che passo
046900040812     c                   clear                   fidn12ds
047000040812     c                   eval      i12aas = BLPAAS
047100040812     c                   eval      i12lnp = BLPLNP
047200040812     c                   eval      i12nrs = BLPNRS
047300040812     c                   eval      i12nsp = BLPNSP
047400040812     c                   eval      i12fel = 'E'
047500040812     c                   eval      i12fan = 'E'
047600040812     c*
047700040812     c                   call      'FIDN12R'
047800040812     c                   parm                    fidn12ds
047900040812     c*
048000040812     c                   setoff                                       32
048100040812     c*
048200040812     c* se non ci sono errori
048300040812     c                   if        o12err <> ' '
048400040812     c                   seton                                        32
048500040812     c                   else
048600040812     c* se numero di CA trovate maggiore di zero
048700040812     c                   if        o12nca = 0
048800040812     c                   seton                                        32
048900040812     c                   else
049000040812     c                   z-add     1             jj                2 0
049100040812     c                   dow       jj <= o12nca and *in32 = *off and WOK = 'S'
049200040812     c                   movea     skKey(jj)     dskey
049300040812     c*
049400040812     C     skDch(jj)     IFEQ      0
049500040812     c* stampo motivazione
049600080325     c**   blpdsp        iflt      wdapr£
049700080325     c***                exsr      ovrprt
049800080325     c***                movel(P)  cbcana        p01mes
049900040812     c* chaino file delle fasi per reperire fase, data, e p.o.
050000080325     c**   kdcf          setgt     fndcf01l
050100080325     c**   kdcf          readpe    fndcf01l
050200080325     c**                 if        not %eof
050300080325     c**   *ymd          move      dcfdfc        data_EUR
050400080325     c**                 move      data_EUR      data_Alfa
050500040812     c* chaino file anagrafica fasi per decodificare la fase
050600080325     c**                 clear                   fndfa000
050700080325     c**   dcffca        chain     fndfa01l
050800080325     c**                 movel     dfades        wfades           40
050900080325     c**                 else
051000080325     c**                 clear                   data_Alfa
051100080325     c**                 clear                   dcffca
051200080325     c**                 clear                   dcffev
051300080325     c**                 endif
051400080325     c**                 move      dcffca        wcffca            3
051500080325     c**                 move      dcffev        wcffev            3
051600080325     c**                 eval      p01mes = %trim(p01mes) + ' ' + wcffca
051700080325     c**                           + ' ' + wfades
051800080325     c**                           + ' del ' + data_Alfa + ' p.o. ' + wcffev
051900080325     c**                 exsr      ried1
052000080325     c**                 write     ls61d1
052100080325     c**                 endif
052200040812     C                   CLEAR                   WOK
052300040812     C                   ELSE
052400040812     C* MEMORIZZO LA DATA CHIUSURA PIU' ALTA
052500040812     C     skDch(jj)     IFGT      WDCH
052600040812     C                   Z-ADD     skDch(jj)     WDCH
052700040812     C                   ENDIF
052800040812     c* memorizzo la data chiusura più alta se p.o. C.A. = lnp bolla
052900040812     c     skDch(jj)     ifgt      wdch_dcb
053000040812     c     dsfil         andeq     blplnp
053100040812     c                   z-add     skDch(jj)     wdch_dcb
053200040812     c                   endif
053300040812     C                   ENDIF
053400040812     c*
053500040812     c                   add       1             jj
053600040812     c                   enddo
053700040812     c*
053800040812     c                   seton                                        32
053900040812     c*
054000040812     c                   endif
054100040812     c                   endif
054200040812     c**
054300000529     C*
054400000529     C* IMPOSTO DATA A SECONDA SE E' POSTE O NO
054500000529     C     §OGPT         IFNE      'S'
054600000529     C                   Z-ADD     PA1BCA        WDATA£            8 0
054700000529     C                   ELSE
054800000529     C                   Z-ADD     POSTCA        WDATA£            8 0
054900000529     C                   ENDIF
055000090305     c
055100090305     C                   ENDIF
055200990126     C**
055300990126     C* SE C.A. TUTTE CHIUSE DEVONO ESSERE PASSATI I GIORNI DALLA DATA
055400990126     C*  CHIUSURA PIU' ALTA
055500990126     C     WOK           IFEQ      'S'
055600000529     C     WDCH          ANDGE     WDATA£
055700990126     C                   CLEAR                   WOK
055800990126     C                   ENDIF
055900990908     C* NON POSSO PULIRE BOLLA  SE NON HO ANCORA STAMPATO LA C.A.
056000990823     C* NELL'ELENCO C.A. DA ARCHIVIARE
056100990823    1C     WOK           IFEQ      'S'
056200020530     c     wdch_dcb      andgt     *zeros
056300990823     C                   CLEAR                   DSBS02
056400990824     C                   MOVEL     'C'           T02MOD
056500990823     C                   MOVEL     KNSIF         T02SIF
056600990823     C                   MOVEL     'DCB'         T02COD
056700990823     C                   MOVEL     BLPLNP        T02KE1
056800990823     C                   CALL      'TIBS02R'
056900990823     C                   PARM                    KPJBA
057000990823     C                   PARM                    DSBS02
057100990823    2C     T02ERR        IFEQ      *BLANKS
057200990823     C                   MOVEL     T02UNI        WDTACA            8 0
057300990823     C                   ELSE
057400990823     C                   Z-ADD     19990101      WDTACA
057500990823    2C                   ENDIF
057600020530    2C     WDCH_dcb      IFGT      WDTACA
057700020402     c* stampo motivazione
057800020408     c     blpdsp        iflt      wdapr£
057900020402     c                   exsr      ovrprt
058000020530     c     *ymd          move      wdch_dcb      data_EUR
058100020402     c                   move      data_EUR      data_Alfa
058200020402     c                   eval      p01mes = cbcann1 + data_alfa + cbcann2
058300020404     c                   exsr      ried1
058400020404     c                   write     ls61d1
058500020408     c                   endif
058600990823     C                   CLEAR                   WOK
058700990823    2C                   ENDIF
058800990823    1C                   ENDIF
058900020409     c*
059000020409     C* PULISCO SOLO SE NON TROVO RECORD NEI FILE TASSAZIONE PADRONCINI
059100020503     c     wok           ifeq      'S'
059200020409     c* se manca cod. padroncino ignoro ricerca perchè si tratta di
059300020409     c* prapagato non confermato
059400020409     c     blppdr        andgt     *zeros
059500020409     c                   clear                   p01mes
059600020409     c     blpfpp        ifeq      'M'
059700020409     c                   z-add     1000          wndc
059800020409     c                   else
059900020409     c                   z-add     5000          wndc
060000020409     c                   endif
060100020409     c                   add       blpnrt        wndc
060200020409     C     Kftd          SETLL     fiftd000                               32
060300020409     c     *in32         ifeq      *on
060400020409     c                   movel(P)  cbcpdr        p01mes
060500020409     c                   endif
060600020409     C  N32Kftd          SETLL     fifsd000                               32
060700020409     c     *in32         ifeq      *on
060800020409     c     blpdsp        iflt      wdaprt
060900020409     c     p01mes        ifeq      *blanks
061000020409     c                   movel(P)  cbcpdr        p01mes
061100020409     c                   eval      p01mes = %trim(p01mes) + ' di simulazione'
061200020409     c                   endif
061300020409     c                   exsr      ovrprt
061400020409     c                   exsr      ried1
061500020409     c                   write     ls61d1
061600020409     c                   endif
061700020409     c                   clear                   wok
061800020409     c                   endif
061900020409     c                   endif
062000040409      *
062100040409      * RECLAMI : se non trovata CANCELLO BOLLA
062200040409      *
062300040409     C*
062400040409    1C     WOK           IFEQ      'S'
062500040409     c                   movel     blpaas        W_aas
062600040409     c                   movel     blplnp        W_lnp
062700040409     c                   movel     blpnrs        W_nrs
062800040409     c                   movel     blpnsp        W_nsp
062900040409      *
063000040409     c                   movel     wspe          ktgogg
063100040409      *
063200040409     c     ktgd          setll     fitgd02l
063300040409     c                   do        *hival
063400040409     c     ktgd          reade     fitgd02l
063500040409
063600040409     c                   if        %eof(fitgd02l)
063700040409     c                   leave
063800040409     c                   endif
063900040423      * se annullato leggo il successivo
064000040423     c                   If        TGDatb <> ' '
064100040423     c                   iter
064200040423     c                   endif
064300040423      * verifico se reclamo ancora aperto
064400040423     c                   If        TGDdcd = 0  or
064500040423      * se reclamo chiuso verifico la data chiusura con la dta di pulizia
064600040423     c                             TGDdcd > datrec
064700040409     c                   clear                   wok
064800040409     c* stampo motivazione
064900080521     c                   If        TGDdcd = 0
065000040409     c                   exsr      ovrprt
065100040409     c                   movel(P)  cbcrec        p01mes
065200040409     c                   exsr      ried1
065300040409     c                   write     ls61d1
065400040409     c                   leave
065500040409     c                   endif
065600080521     c                   endif
065700040409
065800040409     c                   enddo
065900040409
066000040409    1c                   endif
066100060428      *
066200060428      * Prepagati: Se prepagato non incassato non pulisco
066300060428      *
066400060428     C*
066500060428    1C     WOK           IFEQ      'S'
066600060428     c     blpcbo        lookup    cbp                                    30
066700060428   1ac                   if        *in30 = *on
066800060428     c     kar6          chain     fiar601l
066900060428    2c                   if        %found(fiar601l) and ar6nft > 0
067000060428     c                   z-add     ar6aas        prmaas
067100060428     c                   z-add     ar6lnp        prmlnp
067200060428     c                   z-add     ar6nrs        prmnrs
067300060428     c                   z-add     ar6nsp        prmnsp
067400060428     c                   z-add     ar6fiv        prmfiv
067500060428     c                   z-add     ar6nft        prmnft
067600060428     c                   z-add     ar6dft        prmdft
067700060428     c                   clear                   prmesito
067800060428     c                   call(E)   'YCOW0121R'
067900060428     C                   PARM                    prmaas
068000060428     C                   PARM                    prmLnp
068100060428     C                   PARM                    prmNrs
068200060428     C                   PARM                    prmNsp
068300060428     C                   PARM                    prmFiv
068400060428     C                   PARM                    prmDft
068500060428     C                   PARM                    prmNft
068600060428     C                   PARM                    prmEsito
068700060428    3c                   if        prmesito=0
068800060428     c                   clear                   wok
068900060428     c* stampo motivazione
069000060428     c                   exsr      ovrprt
069100060428     c                   movel(P)  cbcprp        p01mes
069200060428     c     *ymd          move      ar6dft        data_EUR
069300060428     c                   move      data_EUR      data_Alfa
069400060428     c                   eval      p01mes = %trim(p01mes)
069500060502     c                             + ' Fattura n.' + %editc(ar6nft:'Z')
069600060428     c                             + ' del ' + data_Alfa
069700060428     c                   exsr      ried1
069800060428     c                   write     ls61d1
069900060428    3c                   endif
070000060428    2c                   endif
070100060428   1ac                   endif
070200060428    1c                   endif
070300930906     C*
070400000801    1C     WOK           IFEQ      'S'
070500930906     C*
070600931122     C* NON DELETO LA BOLLA SE C'E' IL C/ASSEGNO E NON C'E' IL
070700941102     C*   RECORD IN FNCSB00F
070800931122     C                   SETON                                        36
070900931122     C*
071000940929     C* TENTO DI REPERIRE IL RECORD PER CONTRASSEGNO
071100051115     C     KBLP          CHAIN     Fiar901L                           32
071200051115     C   32              Z-ADD     *ZERO         ar9CAS
071300940929     C*
071400981007     C* SE C'E' LA CONSEGNA ANOMALA=7 VUOLL DIRE CHE IL RECORD BOLLA
071500981007     C*  E' STATO CHIUSO IN PARTENZA PER CUI NON CI SARA' MAI IL RECORD
071600981007     C* DI INCASSO DEL C/A CHE QUINDI NON DEVE NEMMENO ESSERE CERCATO
071700981007     C     BLPCCA        IFNE      '7'
071800051115     C     ar9CAS        ANDGT     0
071900941102     C     KBLP          SETLL     FNCSB000                               36
072000010129     C* SE BOLLA POSTE AUTOGENERATA POTREI NON AVERE MAI IL RECORD DI
072100060216     C* INCASSO DEL C/A (LO VEDO NEL ar4 TIPO RECORD "J")9
072200010129     C     *IN36         IFEQ      *OFF
072300010129     C     §OGPT         ANDEQ     'S'
072400010129     C     BLPSOP        ANDEQ     '&'
072500010129     C                   MOVEL     'J'           WTRC
072600060216     C     KBL4          CHAIN     Fiar401L                           31
072700060216     C  N31              MOVEL     ar4NOT        DSBL4J
072800010129     C   31              CLEAR                   DSBL4J
072900010129     C     §B4CSB        IFEQ      'S'
073000010129     C                   SETON                                        36
073100010129     C                   ENDIF
073200010129     C                   ENDIF
073300060120     c* Se non trovato record su csb prima di dire che si tratta di dati inc
073400060120     c* asso mancanti devo controllare lo stato del c'a sul file contrassegn
073500060120     c* di sede
073600060120     c     *in36         ifeq      *off
073700060120     C     KBLP          chain     tncsb03l
073800060120     c                   if        %found(tncsb03l)
073900060120     c* stato 9 --> Annullato; Data distinta > 0 --> c/a incassato
074000060120     c                   if        csbsta = 9 or csbddc > 0
074100060120     c                   seton                                        36
074200060120     c                   endif
074300060120     c                   endif
074400060120     c                   endif
074500931122     C                   ENDIF
074600020402     c*
074700020402     c     *in36         ifeq      *off
074800020402     c* stampo motivazione
074900020408     c     blpdsp        iflt      wdaprt
075000020402     c                   exsr      ovrprt
075100020402     c                   movel(P)  cbccass       p01mes
075200020404     c                   exsr      ried1
075300020404     c                   write     ls61d1
075400020408     c                   endif
075500931122     C*
075600020402     c                   else
075700010605     C* CANCELLO LE C.A.
075800010605     C* (NON LE CANCELLO SE BOLLA LOCALE PRESENTE ANCHE IN FNARB)
075900020611     c                   eval      *in96 = *off
076000010605     C                   move      *blanks       WERR              1
076100020610     c     kblp          setll     fnarb01l                               96
076200020610     c                   if        not *in96
076300010605      *
076400040812      *
076500040812      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
076600040812      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
076700040812      * legate alla spedizione che passo
076800040812     c                   clear                   fidn12ds
076900040812     c                   eval      i12aas = BLPAAS
077000040812     c                   eval      i12lnp = BLPLNP
077100040812     c                   eval      i12nrs = BLPNRS
077200040812     c                   eval      i12nsp = BLPNSP
077300040812     c                   eval      i12fel = 'E'
077400040812     c*
077500040812     c                   call      'FIDN12R'
077600040812     c                   parm                    fidn12ds
077700040812     c*
077800040812     c                   setoff                                       32
077900040812     c*
078000040812     c* se non ci sono errori
078100040812     c                   if        o12err <> ' '
078200040812     c                   seton                                        32
078300040812     c                   else
078400040812     c* se numero di CA trovate maggiore di zero
078500040812     c                   if        o12nca = 0
078600040812     c                   seton                                        32
078700040812     c                   else
078800040812     c                   z-add     1             jj                2 0
078900040812     c                   dow       jj <= o12nca and *in32 = *off
079000040812     c                   movea     skKey(jj)     dskey
079100040812     c*
079200040812     C                   CLEAR                   fidn11ds
079300040812     C                   MOVEL     'D'           I11MOD
079400040812     C                   Z-ADD     dsaac         I11AAC
079500040812     C                   Z-ADD     dsfil         I11FIL
079600040812     C                   Z-ADD     dsnca         I11NCA
079700040812     C                   CALL      'FIDN11R'
079800040812     C                   PARM                    KPJBA
079900040812     C                   PARM                    fidn11ds
080000040812      *
080100040812    7C     O11ERR        IFEQ      'E'
080200040812     C                   MOVEL     '1'           WERR
080300040812      *
080400111103      * ES 03 11 11 --> non invio più messaggio unico errore
080500111103      *                 è tipo richiamo del pgm errato
080600040812      * SE ERRORE INVIO ANCHE MESSAGGIO IN SEDE CON
080700040812      * RIFERIMENTI BOLLA: ANNO/LINEA/NR.SERIE/NR.SPEDIZ.
080800040812      *
080900111103     C**                 Z-ADD     blpAAS        MSGAAS
081000111103     C**                 Z-ADD     blplNP        MSGLNP
081100111103     C**                 Z-ADD     blpNRS        MSGNRS
081200111103     C**                 Z-ADD     blpNSP        MSGNSP
081300040812      *
081400111103     C**                 MOVEL(p)  MSG(1)        MSGTES
081500111103     C**                 MOVEL     MSG(2)        WWWW41           41
081600111103     C**                 MOVE      PARAM         WWWW41
081700111103     C**                 MOVEL(P)  WWWW41        MSGDET
081800040812      *
081900040812    7C                   ENDIF
082000040812     c*
082100040812     c                   add       1             jj
082200040812     c                   enddo
082300040812     c*
082400040812     c                   seton                                        32
082500040812     c*
082600040812     c                   endif
082700040812     c                   endif
082800040812     c**
082900010605    5C                   endif
083000010605     c*
083100010605     C* SE NON SONO RIUSCITA A CANCELLARE TUTTE LE C.A. NON POSSO
083200010605     C* CANCELLARE LA BOLLA
083300010605     c     werr          ifeq      *blanks
083301130903
083302130903     c* Cancello eventuali PDF
083305130903     c                   exsr      Canpdf
083306130903     c* Cancello la bolla solo se non ci sono stati errori nella cancellazione
083307130903     c* di eventuali PDF
083308130903     c     wesitopdf     ifeq      *blanks
083400000801      *
083500000801      * CANCELLAZIONE FISICA BOLLA E FILES ALLEGATI
083600000801      *
083700000801     C                   CLEAR                   DSLS63
083800000801     C                   MOVE      BLPAAS        I63AAS
083900000801     C                   MOVE      BLPLNP        I63LNP
084000000801     C                   MOVE      BLPNRS        I63NRS
084100000801     C                   MOVE      BLPNSP        I63NSP
084200000801     C                   MOVEL     DSLS63        KPJBU
084300020503     C                   CALL      'FNLS63R'
084400020503     C                   PARM                    KPJBA
084500000801      *
084600050906      * Pulizia Giacenza: Se wgiace = 'S' vuol dire che c'è giacenza aperta
084700050906      *                   in questo caso non posso pulire la giacenza
084800050906      *                   Posso pulire la giacenza quando non ci sono
084900050906      *                   più bolle su ARB legate alla bolla originale
085000050906if  4c                   If        WGIACE = *Blanks
085100050906     c     kgcp          Setll     Tigcp01L
085200050906      * Entro nel ciclo solo se ho trovato una giacenza
085300050906if  5c                   If        %Equal(Tigcp01l)
085400050906do  6c                   Do        *Hival
085500050906     c     kgcp          Reade     Tigcp01L
085600050906      * Fine file
085700050906     c                   If        %Eof(Tigcp01l)
085800050906     c                   Leave
085900050906     c                   EndIf
086000050906      * Giacenza ancora aperta torno a leggere
086100050906     c                   If        GCPATB = *blanks and GCPFAS < 60
086200050906     c                   Iter
086300050906     c                   EndIf
086400050906      * Controllo se esiste la bolla su ARB
086500050906     c     karb          Setll     Fnarb01l
086600050906      * Esiste la bolla su ARB esco dal ciclo tanto non devo pulire la giacenza
086700050906      * se esiste anche solo 1 bolla
086800050906      * imposto anche il flag x pilotare la pulizia delle giacenze
086900050906if  7c                   If        %Equal(Fnarb01l)
087000050906     c                   eval      Wgiace = 'S'
087100050906     c                   Leave
087200050906    7c                   EndIf
087300050906    6c                   EndDo
087400050906    5c                   EndIf
087500050906    4c                   EndIf
087600050906
087700050906      * Pulizia Giacenza: Se wgiace = 'S' vuol dire che ho trovato anche solo 1
087800050906      *                   bolla su ARB e quindi non devo pulire la giacenza
087900050906if  4c                   If        wgiace = *Blanks
088000010507     c                   z-add     blpaas        iz5aas
088100010507     c                   z-add     blplnp        iz5lnp
088200010507     c                   z-add     blpnrs        iz5nrs
088300010507     c                   z-add     blpnsp        iz5nsp
088400050404     c                   call      'FNLGZ5R'
088500020503     c                   PARM                    FNLGZ5DS
088600010502---  C                   ENDIF
088700010605     C                   ENDIF
088701130903     C                   ENDIF
088800010502      *
088900000801---  C                   ENDIF
089000931122     C*
089100000801    1C                   ENDIF
089200930903     C*
089300930902     C                   ENDSR
089301130903     C*---------------------------------------------------------------*
089302130903     C* Cancellazione PDF - PACKING LIST                              *
089303130903     C*---------------------------------------------------------------*
089304130903     C     canpdf        BEGSR
089305130903     c                   eval      wesitoPDF=' '
089306130903     c     kar5          chain     fiar501l
089307130903    1c                   if        %found(fiar501l)
089308130903     c                   eval      dar5gen=ar5uni
089309130903    2c                   if        §ar5alm='S' or §ar5alm='E' or
089310130903     c                             §ar5als='S' or §ar5als='E'
089311130903     c     kar8          setll     fiar801l
089312130903     c     kar8          reade     fiar801l
089313130903    3c                   dow       not %eof(fiar801l)
089314130903     c                   eval      dar8§pdf=ar8uni
089315130903     c                   eval      pIn_FolderPDF = §ar8ppdfp
089316130903     c                   eval      pIn_NamePDF = §ar8ppdfn
089317130903     c                   call      'TIS7P2SR3'
089318130903     c                   parm                    pIn_FolderPDF
089319130903     c                   parm                    pIN_NamePDF
089320130903     c                   parm                    pOut_Esito
089321130903    4c                   if        pOut_Esito='1'
089322130903     c                   eval      wesitoPDF='E'
089330130903    4c                   endif
089331130903     c
089332130903     c     kar8          reade     fiar801l
089333130903    3c                   enddo
089334130903    2c                   endif
089335130903    1c                   endif
089336130903     c* Se errori nella cancellazione del/i PDF stampo motivo di non pulizia
089337130903     c                   if        wesitoPdf='E'
089338130903     c* stampo motivazione
089339130904     c***  blpdsp        iflt      wdaprt
089340130903     c                   exsr      ovrprt
089341130903     c                   movel(P)  cbcpdf        p01mes
089342130903     c                   exsr      ried1
089343130903     c                   write     ls61d1
089344130904     c***                endif
089345130903     c                   endif
089346130903     c                   endsr
089400020404     C*---------------------------------------------------------------*
089500020404     C* OVRPRT - Operazioni su file printer                           *
089600020404     C*---------------------------------------------------------------*
089700020404     C     ovrprt        BEGSR
089800020404     c* override su file printer + apertura
089900020404     c     wopn          ifeq      *blanks
090000020409     c                   movel     cmd(1)        comman          100
090100020409     c                   eval      %subst(cmd(2):6:4) = whld
090200020409     c                   eval      %subst(cmd(2):17:4) = wsav
090300020409     c                   eval      comman = %trim(comman) + '('
090400020408     c                             + %trim(§5alib) + '/'
090500020409     c                             + %trim(§5aotq) + ') ' + %trim(cmd(2))
090600020408     C                   Z-ADD     100           LUNGH
090700020404     C                   CALL      'QCMDEXC'
090800020404     C                   PARM                    COMMAN
090900020404     C                   PARM                    LUNGH            15 5
091000020404     c                   open      fnls61p
091100020404     c* valorizzazione campi di stampa testata
091200020404     c* data bolle lnp italia
091300020404     c     *ymd          move      pa1dbr        data_dmy
091400020404     c                   move      data_dmy      dtpita
091500020404     c* data bolle import
091600020404     c     *ymd          move      pa1pbi        data_dmy
091700020404     c                   move      data_dmy      dtpimp
091800020404     c* data bolle con riserva
091900020404     c     *ymd          move      pa1blr        data_dmy
092000020404     c                   move      data_dmy      dtprsv
092100020404     c* data bolle con c.a.
092200020404     c     *ymd          move      pa1bca        data_dmy
092300020404     c                   move      data_dmy      dtpcan
092400020404     c* Bolle poste: data bolle lnp italia
092500020404     c     *ymd          move      postbr        data_dmy
092600020404     c                   move      data_dmy      dppita
092700020404     c*              data bolle import
092800020404     c     *ymd          move      postbi        data_dmy
092900020404     c                   move      data_dmy      dppimp
093000020404     c*              data bolle con riserva
093100020404     c     *ymd          move      postlr        data_dmy
093200020404     c                   move      data_dmy      dpprsv
093300020404     c*              data bolle con c.a.
093400020404     c     *ymd          move      postca        data_dmy
093500020404     c                   move      data_dmy      dppcan
093600020404     c*
093700020404     c                   write     ls61t1
093800020404     c                   movel     '1'           wopn
093900020404     c                   endif
094000020404     c* stampa intestazione a rottura
094100020404     c   99              write     ls61t1
094200020404     c                   setoff                                       99
094300020404     c                   endsr
094400020404     C*---------------------------------------------------------------*
094500020404     C* RIED1  - Valorizzazione campi di stampa dettaglio             *
094600020404     C*---------------------------------------------------------------*
094700020404     C     RIED1         BEGSR
094800020404     c                   z-add     blplnp        p1clnp
094900020404     c                   z-add     blplna        p1clna
095000020404     c                   z-add     blpnrs        p1cnrs
095100020404     c                   z-add     blpnsp        p1cnsp
095200020404     c     *ymd          move      blpdsp        data_EUR
095300020404     c                   move      data_EUR      p1cdsp
095400080521     c                   MOVEL     BLPCBO        p1cCBO
095500080521     c                   MOVEL     BLPCCA        p1cCCA
095600080521     c                   if        BLPdcm>0
095700080521     c     *ymd          move      BLPDCM        data_EUR
095800080521     c                   move      data_EUR      p1cDCM
095900080521     c                   else
096000080521     c                   clear                   p1cdcm
096100080521     c                   endif
096200020404     c                   endsr
096300930901     C*---------------------------------------------------------------*
096400940930     C*                    *INZSR                                     *
096500930901     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
096600930901     C*---------------------------------------------------------------*
096700940930     C     *INZSR        BEGSR
096800930901     C*
096900930901     C     *ENTRY        PLIST
097000930901     C                   PARM                    KPJBA
097100940930     C                   MOVEL     KPJBU         PARAM1
097200930901     C*
097300010502     C* LETTURA ESTENSIONI EVENTI, GIACENZE, DETTAGLIO BOLLA
097400930901     C     KBLP          KLIST
097500930901     C                   KFLD                    BLPAAS
097600930901     C                   KFLD                    BLPLNP
097700930901     C                   KFLD                    BLPNRS
097800930901     C                   KFLD                    BLPNSP
097900060428     C     Kar6          KLIST
098000060428     C                   KFLD                    BLPAAS
098100060428     C                   KFLD                    BLPLNP
098200060428     C                   KFLD                    BLPNRS
098300060428     C                   KFLD                    BLPNSP
098400060428     c                   kfld                    k6trc
098401130903     C     Kar5          KLIST
098402130903     C                   KFLD                    BLPAAS
098403130903     C                   KFLD                    BLPLNP
098404130903     C                   KFLD                    BLPNRS
098405130903     C                   KFLD                    BLPNSP
098406130903     c                   kfld                    ktrd
098407130903     C     Kar8          KLIST
098408130903     C                   KFLD                    BLPAAS
098409130903     C                   KFLD                    BLPLNP
098410130903     C                   KFLD                    BLPNRS
098411130903     C                   KFLD                    BLPNSP
098412130903     c                   kfld                    ktmd
098500050413
098600050906     C     KGCP          KLIST
098700050906     C                   KFLD                    BLPAAS
098800050906     C                   KFLD                    BLPLNP
098900050906     C                   KFLD                    BLPNRS
099000050906     C                   KFLD                    BLPNSP
099100050906     C                   KFLD                    KFRG
099200050404
099300050906     c     karb          klist
099400050906     c                   kfld                    GcpAasa
099500050906     c                   kfld                    GcpLnpa
099600050906     c                   kfld                    GcpNrsa
099700050906     c                   kfld                    GcpNspa
099800050404
099900010129     C     KBL4          KLIST
100000010129     C                   KFLD                    BLPAAS
100100010129     C                   KFLD                    BLPLNP
100200010129     C                   KFLD                    BLPNRS
100300010129     C                   KFLD                    BLPNSP
100400010129     C                   KFLD                    WTRC
100500980721     C     KEVB4         KLIST
100600101007     C                   KFLD                    blpaas
100700980721     C                   KFLD                    BLPLNP
100800980721     C                   KFLD                    BLPNRS
100900980721     C                   KFLD                    BLPNSP
101000980721     C                   KFLD                    WCEV
101100930906     C*
101200930906     C* LETTURA BOLLA (37C)
101300930906     C     KBL2          KLIST
101400940929     C                   KFLD                    KFIL
101500940929     C                   KFLD                    KDCM
101600020408     C*
101700020408     C     KTAB          KLIST
101800020408     C                   KFLD                    CODUT             1 0
101900020408     C                   KFLD                    COD
102000020408     C                   KFLD                    KEY
102100060428     C     KTAB2         KLIST
102200060428     C                   KFLD                    CODUT             1 0
102300060428     C                   KFLD                    COD
102400060428
102500020408     C                   Z-ADD     1             CODUT
102600020409     c*
102700020409     C     Kftd          KLIST
102800020409     C                   KFLD                    blppdr
102900020409     C                   KFLD                    wtsr
103000020409     C                   KFLD                    wndc
103100020409     C                   KFLD                    blpdrt
103200020409     C                   KFLD                    blpAAS
103300020409     C                   KFLD                    blpLNP
103400020409     C                   KFLD                    blpNRS
103500020409     C                   KFLD                    blpNSP
103600020409     c                   movel     'R'           wtsr
103700020530     c*
103800020530     c     kdcf          klist
103900040812     c                   kfld                    dsaac
104000040812     c                   kfld                    dsfil
104100040812     c                   kfld                    dsnca
104200040409     c*  File Fitgd02l    RECLAMI
104300040409     c     ktgd          klist
104400040409     c                   kfld                    ktgtor
104500040409     c                   kfld                    ktgogg
104600940930     C*
104700951010     C     *LIKE         DEFINE    BLPLNP        KFIL
104800020211     C     *LIKE         DEFINE    BLPLNP        savlnp
104900020211     C     *LIKE         DEFINE    §ogntw        lnpntw
105000940929     C     *LIKE         DEFINE    BLPDCM        KDCM
105100980721     C     *LIKE         DEFINE    EVBCEV        WCEV
105200060216     C     *LIKE         DEFINE    ar4TRC        WTRC
105300050906     C     *LIKE         DEFINE    GCPFRG        KFRG
105400020408     C     *LIKE         DEFINE    TBLCOD        COD
105500020408     C     *LIKE         DEFINE    TBLKEY        KEY
105600020409     C     *LIKE         DEFINE    ftdtsr        wtsr
105700020409     C     *LIKE         DEFINE    ftdndc        wndc
105800020408     c*
105900020408     C                   TIME                    wtime            14 0
106000020408     C                   MOVE      WTIME         WDATE             8 0
106100020409     c     *eur          move      wdate         wdate_inv
106200980721     C*
106300050906     C                   clear                   kfrg
106400101007     C*****              MOVEL     'R  '         WCEV
106500010605     c*
106600010605     C                   z-add     1             codut             1 0
106700010605     C                   call      'X§PARUT'
106800010605     C                   parm                    UT§DSE
106900010605     C                   movel     rec80         CNCR80
107000020404     c                   movel     ragut         rsut
107100010605     c*
107200010605      * COSTANTI MESSAGGIO
107300010605     C                   MOVE      '/'           BARRW1
107400010605     C                   MOVE      '/'           BARRW2
107500010605     C                   mOVE      '/'           BARRW3
107600020408     c*
107700020408     c                   exsr      caldts
107800060428     c*
107900060428     c* carico schiera contenente codici bolla validi per prepagati
108000101007     C                   Z-ADD     0             X
108100060428     C                   MOVEL     '3A'          COD
108200060428     C     KTAB2         SETLL     TABEL00F
108300060428     C                   DO        *HIVAL
108400060428     C     KTAB2         READE     TABEL00F
108500060428     C                   IF        %eof(tabel00f)
108600060428     C                   LEAVE
108700060428     C                   ENDIF
108800060428     C                   IF        tblflg <> *BLANKS
108900060428     C                   ITER
109000060428     C                   ENDIF
109100060428     C                   MOVEL     TBLUNI        DS3A
109200061107     c                   if        %subst(§3atb1:1:1)='F' and §3arbl<>'C'
109300060428     C                   ADD       1             X                 2 0
109400060428     C                   MOVEL     TBLKEY        CBP(X)
109500060428     c                   endif
109600060428     C                   ENDDO
109700940930     C*
109800101007     c* Carico gli eventi di riserva dalla tabella 2A
109900101007     C                   CLEAR                   YY                3 0
110000101007     C                   MOVEL     '2A'          COD
110100101007     C     KTAB2         SETLL     TABEL
110200101007     C     KTAB2         READE     TABEL                                  30
110300101007     C     *IN30         DOWEQ     *OFF
110400101007     C     TBLFLG        IFEQ      ' '
110500101007     C                   MOVEL     TBLUNI        DS2A
110600101007     C     §2Atip        IFeq      'R'
110700101007     C                   ADD       1             YY
110800101007     C                   MOVEL     TBLKEY        ris(YY)
110900101007     C                   ENDIF
111000101007     C                   ENDIF
111100101007     C     KTAB2         READE     TABEL                                  30
111200101007     C                   ENDDO
111300930901     C                   ENDSR
111400020408     C*---------------------------------------------------------------*
111500020408     C* caldts - Calcolo date stampa                                  *
111600020408     C*---------------------------------------------------------------*
111700020408     C     CALDTS        BEGSR
111800020408      * LETTURA giorni per date stampa e coda di output
111900020408     C                   MOVEL     '5A'          COD
112000020408     C                   MOVEL     '2       '    KEY
112100020408     C     ktab          CHAIN     taBEL00F                           30
112200020408     C  N30              MOVEL     TBLUNI        DS5A2
112300020408     C   30              CLEAR                   DS5A2
112400020408     C*
112500020408     C                   MOVEL     '5A'          COD
112600020408     C                   MOVEL     '3       '    KEY
112700020408     C     ktab          CHAIN     TABEL00F                           30
112800020408     C  n30              MOVEL     TBLUNI        DS5A3
112900020408     C   30              CLEAR                   DS5A3
113000020409     c     §5ahld        ifeq      'S'
113100020409     c                   movel     '*YES'        whld              4
113200020409     c                   else
113300020409     c                   movel     '*NO '        whld
113400020409     c                   endif
113500020409     c     §5asav        ifeq      'S'
113600020409     c                   movel     '*YES'        wsav              4
113700020409     c                   else
113800020409     c                   movel     '*NO '        wsav
113900020409     c                   endif
114000020408     c* BOLLE NO POSTE
114100020408     c* 1) data stampa bolle lnp italia
114200020408     c     *ymd          move      pa1dbr        data_iso
114300020408     c                   subdur    §5agal:*d     data_iso
114400020408     c                   move      data_iso      dtsita_ymd
114500020408     c                   move      data_iso      data_dmy
114600020408     c                   move      data_dmy      dtsita
114700020408     c* 2) data stampa bolle import
114800020408     c     *ymd          move      pa1pbi        data_iso
114900020408     c                   subdur    §5agal:*d     data_iso
115000020408     c                   move      data_iso      dtsimp_ymd
115100020408     c                   move      data_iso      data_dmy
115200020408     c                   move      data_dmy      dtsimp
115300020408     c* 3) data stampa bolle con riserva
115400020408     c     *ymd          move      pa1blr        data_iso
115500020408     c                   subdur    §5agal:*d     data_iso
115600020408     c                   move      data_iso      dtsrsv_ymd
115700020408     c                   move      data_iso      data_dmy
115800020408     c                   move      data_dmy      dtsrsv
115900020408     c* 4) data stampa bolle con c.a.
116000020408     c     *ymd          move      pa1bca        data_iso
116100020409     c                   subdur    §5agcn:*d     data_iso
116200020408     c                   move      data_iso      dtscan_ymd
116300020408     c                   move      data_iso      data_dmy
116400020408     c                   move      data_dmy      dtscan
116500020411     c                   move      data_dmy      dtsncc
116600020408     c* BOLLE POSTE
116700020408     c* 1) data stampa bolle lnp italia
116800020408     c     *ymd          move      postbi        data_iso
116900020408     c                   subdur    §5agal:*d     data_iso
117000020408     c                   move      data_iso      dpsita_ymd
117100020408     c                   move      data_iso      data_dmy
117200020408     c                   move      data_dmy      dpsita
117300020408     c* 2) data stampa bolle import
117400020408     c     *ymd          move      postbr        data_iso
117500020408     c                   subdur    §5agal:*d     data_iso
117600020408     c                   move      data_iso      dpsimp_ymd
117700020408     c                   move      data_iso      data_dmy
117800020408     c                   move      data_dmy      dpsimp
117900020408     c* 3) data stampa bolle con riserva
118000020408     c     *ymd          move      postlr        data_iso
118100020408     c                   subdur    §5agal:*d     data_iso
118200020408     c                   move      data_iso      dpsrsv_ymd
118300020408     c                   move      data_iso      data_dmy
118400020408     c                   move      data_dmy      dpsrsv
118500020408     c* 4) data stampa bolle con c.a.
118600020408     c     *ymd          move      postca        data_iso
118700020409     c                   subdur    §5agcn:*d     data_iso
118800020408     c                   move      data_iso      dpscan_ymd
118900020408     c                   move      data_iso      data_dmy
119000020408     c                   move      data_dmy      dpscan
119100020411     c                   move      data_dmy      dpsncc
119200020408     c*
119300020409     c* Data per stampare le spedizioni non consegnate
119400020409     c                   subdur    §5agsn:*d     wdate_inv
119500020409     c                   move      wdate_inv     dtsncn_ymd
119600020409     c                   move      wdate_inv     data_dmy
119700020409     c                   move      data_dmy      dtsncn
119800020409     c                   move      data_dmy      dpsncn
119900020411     c* Per quanto riguarda la data delle non consegnate con C.A. l'ho
120000020411     c* già impostata come la data per le spediz. con c.a. (vedi sopra)
120100020408     c                   endsr
120200010605**
120300010605ERRORE "PULIZIA BOLLE PARTENZE"
120400010605PGM: FNLS61R - BOLLA:
120500020404**
120600020408OVRPRTF FNLS61P OUTQ
120700120221HOLD(    ) SAVE(    ) USRDTA('PulBoPar')
