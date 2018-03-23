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
001200051115     FFiar901L  IF   E           K DISK
001300060428     FFiar601L  IF   E           K DISK
001400130903     FFiar501L  IF   E           K DISK
001500130903     FFiar801L  IF   E           K DISK
001600990810     FFNEVB04L  IF   E           K DISK
001700990810     F                                     RENAME(FNEVB000:FNEVB004)
001800941102     FFNCSB01L  IF   E           K DISK
001900980721     FAZORG01L  IF   E           K DISK
002000010605     Ffnarb01l  IF   E           K DISK
002100020408     FTABEL00F  if   E           K disk
002200020409     F* ARCHIVI  PADRONCINI DETTAGLIO TASSAZIONE
002300060206     Ffiftd25L  IF   E           K DISK
002400020409     Ffifsd05L  IF   E           K DISK
002500020409     F                                     RENAME(fiftd000:fifsd000)
002600060120     F* ARCHIVIO Contrassegni di sede
002700060120     FTNCSB03L  IF   E           K DISK
002800060120     f
002900020404     Ffnls61p   O    E             PRINTER USROPN oflind(*in99)
003000040409     F* ARCHIVI  RECLAMI
003100040409     Ffitgd02l  IF   E           K DISK
003200940930     F*
003300010605     d msg             s             70    dim(2) CTDATA PERRCD(1)
003400020408     d cmd             s             50    dim(2) CTDATA PERRCD(1)
003500060428     D CBP             S              2    DIM(99)
003600101007     D ris             S              3    DIM(50)
003700020402
003800020402     d data_EUR        s               d   datfmt(*EUR)
003900020404     d data_DMY        s               d   datfmt(*DMY)
004000020408     d data_ISO        s               d   datfmt(*ISO)
004100020409     d wdate_inv       s               d   datfmt(*ISO)
004200020402     d data_Alfa       s             10a
004300020408     d dtsita_ymd      s              8  0
004400020408     d dtsimp_ymd      s              8  0
004500020408     d dtsrsv_ymd      s              8  0
004600020408     d dtscan_ymd      s              8  0
004700020408     d dtsncn_ymd      s              8  0
004800150519     d dtsrim_ymd      s              8  0
004900020404     d wopn            s              1a
005000040409
005100040409     d ktgtor          s                   like(tgdtor) inz('S')
005200040409     d ktgogg          s                   like(tgdogg)
005300060428     d k6trc           s                   like(ar6trc) inz('1')
005400130903     d ktrd            s                   like(ar5trd) inz('GEN')
005500130903     d ktmd            s                   like(ar8tmd) inz('§PDF')
005600130903     d wesitopdf       s              1A
005700130903       // - Parametri per richiamo pgm pulizia file PDF
005800130903     d pIn_FolderPDF   s            256a
005900130903     d pIn_NamePDF     s            256a
006000130903       // '0'=OK '1'=errore
006100130903     d pOut_esito      s              1A
006200040409
006300930906     D*---------------------------------------------------------------*
006400930906     D* DS                                                            *
006500930906     D*---------------------------------------------------------------*
006600930906     D*-------------
006700961204     D* PARAMETRI RICEVUTI DAI PGM CHIAMANTI
006800930906     D*-------------
006900940930     D PARAM1          DS
007000000529     D***
007100961204     D* DATA PULIZIA BOLLE PARTENZE (CON LNP NON ESTERA)
007200940930     D  PA1DBR                 1      8  0
007300980721     D***
007400961204     D* DATA PULIZIA BOLLE PARTENZE (CON LNP ESTERA - BOLLE IMPORT)
007500961204     D  PA1PBI               137    144  0
007600980721     D***
007700980721     D* DATA PULIZIA BOLLE CON RISERVA
007800980721     D  PA1BLR               161    168  0
007900000529     D***
008000990126     D* DATA PULIZIA BOLLE CON C.A.
008100990714     D  PA1BCA               169    176  0
008200000529     D***
008300000529     D* DATA PULIZIA BOLLE PARTENZE - POSTE
008400000529     D  POSTBR               177    184  0
008500000529     D***
008600000529     D* DATA PULIZIA BOLLE PARTENZE - BOLLE IMPORT - POSTE
008700000529     D  POSTBI               185    192  0
008800000529     D***
008900000529     D* DATA PULIZIA BOLLE CON RISERVA - POSTE
009000000529     D  POSTLR               193    200  0
009100000529     D***
009200000529     D* DATA PULIZIA BOLLE CON C.A. - POSTE
009300000529     D  POSTCA               201    208  0
009400040423     D***
009500040423     D* DATA PULIZIA BOLLE CON RECLAMI
009600040423     D  DATREC               233    240  0
009700010605     d PARAM           DS
009800010605     d  msgaas                 1      4  0
009900010605     d  barrw1                 5      5
010000010605     d  msglnp                 6      8  0
010100010605     d  barrw2                 9      9
010200010605     d  msgnrs                10     11  0
010300010605     d  barrw3                12     12
010400010605     d  msgnsp                13     19  0
010500930906     D*-------------
010600930906     D* PARAMETRI ESTERNI
010700930906     D*-------------
010800000801     D* DS PER FNLS63R - CANCELLAZIONE BOLLA PARTENZA SINGOLA
010900000801     D DSLS63        E DS                  EXTNAME(FNLS63DS)
011000000801     D*
011100000529     D OG143         E DS
011200130903     D*
011300130903     D Dar5gen       E DS
011400130903     D Dar8§pdf      E DS
011500010507      *
011600010507     D FNLGZ5DS      E DS
011700010507     D*
011800010507     D KPJBA         E DS
011900000801     D*
012000990823     D* DS PER TIBS02DS- RICERCA TABELLE
012100990823     D DSBS02        E DS                  EXTNAME(TIBS02DS)
012200010605     D fidn11ds      E DS
012300010605     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
012400010605     D CNCR80        E DS
012500020408     D*
012600020408     D ds5a2         E DS
012700020408     D ds5a3         E DS
012800060428     D ds3a          E DS
012900101007     D ds2a          E DS
013000930906     D*-------------
013100930906     D* DS DATA SPEDIZIONE
013200930906     D*-------------
013300900620     D                 DS
013400940929     D  BLPAAS                 1      4  0
013500940929     D  BLPMGS                 5      8  0
013600940929     D  BLPDSP                 1      8  0
013700040409     D*-------------
013800040409     D* DS OGGETTO RECLAMO
013900040409     D*-------------
014000040409     D                 DS
014100040409     D  W_lnp                         3  0
014200040409     D  W_nrs                         2  0
014300040409     D  W_nsp                         7  0
014400040409     D  W_aas                         4  0
014500040409     D  Wspe                   1     16  0 inz
014600040812
014700040812      * ds per il controllo della presenza di CA per la spedizione richiesta
014800040812     d FIDN12DS      e ds
014900040812     d  skKey                 26    305    dim(20)
015000040812     d  skAnn                306    325    dim(20)
015100040812     d  skDca                326    485  0 dim(20)
015200040812     d  skFca                486    545  0 dim(20)
015300040812     d  skDch                546    705  0 dim(20)
015400040812     d  skCch                706    745    dim(20)
015500050413
015600040812     d*
015700040812     d dsKey           ds
015800040812     d  dsaac                         4  0
015900040812     d  dsfil                         3  0
016000040812     d  dsnca                         7  0
016100060428     D cndizion      E DS
016200060428     D                                     BASED(nullPtr)
016300060428     D                                     PREFIX(diz)
016400040812
016500040812     D*
016600040812     D* DEFINIZIONE VARIABILI D WRK
016700040812     D*
016800040812     D WDCH            S              8  0
016900040812     D WDCH_dcb        S              8  0
017000060428
017100060428     D nullPtr         S               *
017200060428     D prmAas          S
017300060428     D                                     LIKE(diz$Aas)
017400060428     D prmLnp          S
017500060428     D                                     LIKE(diz$Lnp)
017600060428     D prmNrs          S
017700060428     D                                     LIKE(diz$Nrs)
017800060428     D prmNsp          S
017900060428     D                                     LIKE(diz$Nsp)
018000060428     D prmFiv          S
018100060428     D                                     LIKE(diz$Fiv)
018200060428     D prmDft          S
018300060428     D                                     LIKE(diz$Dft)
018400060428     D prmNft          S
018500060428     D                                     LIKE(diz$Nft)
018600060428     D prmEsito        S             10I 0
018700040812
018800020402     D*
018900020402     D* DEFINIZIONE COSTANTI
019000020402     D*
019100020528     D CBGIAC          C                   CONST('Bolla giacente in fase')
019200020402     D CBRISE          C                   CONST('Bolla con riserva consegnata -
019300020402     D                                     il            ')
019400020528     D CBCANA          C                   CONST('Bolla con C.A. aperta - Fase')
019500020403     D CBCANC          C                   CONST('Bolla con C.A. chiusa il')
019600020402     D CBCANN1         C                   CONST('Presente C.A. chiusa il ')
019700020402     D CBCANN2         C                   CONST(' non ancora stampata nell''el-
019800020402     D                                     enco C.A. da archiviare')
019900020403     D CBCcass         C                   CONST('Bolla con C/Assegno - Dati in-
020000020403     D                                     casso mancanti')
020100020408     D CBncon          C                   CONST('Bolla non consegnata')
020200020411     D CBnconca        C                   CONST('Bolla non consegnata con C.A.-
020300020411     D                                     ')
020400020409     D CBcpdr          C                   CONST('Bolla presente su archivio co-
020500020409     D                                     nteggi padroncini')
020600080521     D CBcrec          C                   CONST('Bolla con reclamo APERTO')
020700060502     D CBcprp          C                   CONST('Prepagato non incassato -')
020800130904     D CBcpdf          C                   CONST('Pulizia file PDF non riuscita-
020900130904     d                                     ')
021000930906     C*---------------------------------------------------------------*
021100930906     C*                    MAIN                                       *
021200930906     C* PROGRAMMA PRINCIPALE PULIZIA BOLLE PARTITE                    *
021300930906     C*---------------------------------------------------------------*
021400000801      *
021500940929     C                   Z-ADD     *ZEROS        KFIL
021600930907     C                   SETOFF                                           34
021700150519     c                   clear                   pulbol
021800930907     C*
021900930907DO  1C     *IN34         DOWEQ     *OFF
022000940929     C     KFIL          SETGT     FNBLP37L
022100940929     C                   READ      FNBLP37L                               34
022200930907     C*
022300930907IF  2C     *IN34         IFEQ      *OFF
022400940929     C                   Z-ADD     BLPLNP        KFIL
022500940929     C                   Z-ADD     1             KDCM
022600980721     C**
022700980721     C* VEDO SE SI TRATTA DI LNP ESTERA
022800020211     C                   MOVE      *blanks       lnpntw
022900980721     C     BLPLNP        CHAIN     AZORG01L                           35
023000000529     C  N35              MOVEL     ORGDE3        OG143
023100020211     C  N35              MOVEL     §ogntw        lnpntw
023200020211     c**
023300020211     C     lnpntw        ifeq      'EEX'
023400020211     C     lnpntw        oreq      'EUP'
023500020211     C     lnpntw        oreq      'FED'
023600020211     C     lnpntw        oreq      'DPD'
023700150519     C* IMPOSTO DATA PULIZIA BOLLE IMPORT
023800980721     C                   Z-ADD     PA1PBI        WDAPUL            8 0
023900020408     C                   Z-ADD     dtsimp_ymd    WDAprt            8 0
024000980721     C                   ELSE
024100150519     C* IMPOSTO DATA PULIZIA BOLLE EXPORT E ITALIA
024200980721     C                   Z-ADD     PA1DBR        WDAPUL            8 0
024300020408     C                   Z-ADD     dtsita_ymd    WDAprt            8 0
024400980721     C                   ENDIF
024500930907     C*
024600930907     C* MI POSIZIONO PER LNP TROVATA E DATA CONSEGNA MERCE > DI ZERO
024700940929     C     KBL2          SETLL     FNBLP37L
024800940929     C     KFIL          READE     FNBLP37L                               35
024900930907     C*
025000930907     C* FINCHE FINE FILE E DATA CONSEGNA MERCE MINORE ALLA DATA PULIZIA
025100930907DO  3C     *IN35         DOWEQ     *OFF
025200980721     C     BLPDCM        ANDLT     WDAPUL
025300930907     C* PULIZIA BOLLE
025400930907     C                   EXSR      CANC
025500930907     C*
025600940929     C     KFIL          READE     FNBLP37L                               35
025700930907E   3C                   ENDDO
025800930907     C*
025900930907E   2C                   ENDIF
026000930907E   1C                   ENDDO
026100020408     c*
026200020409     c* II° CICLO PER STAMPARE SPEDIZIONI NON CONSEGNATE OLTRE X GIORNI
026300150519     c* DATA SPEDIZIONE ( uso la data bolle con c.a.)
026400020408     C                   Z-ADD     *ZEROS        KFIL
026500020408     C                   SETOFF                                           34
026600020408     C*
026700020408DO  1C     *IN34         DOWEQ     *OFF
026800020408     C     KFIL          SETGT     FNBLP37L
026900020408     C                   READ      FNBLP37L                               34
027000020408     C*
027100020408IF  2C     *IN34         IFEQ      *OFF
027200020408     C                   Z-ADD     BLPLNP        KFIL
027300020408     C                   Z-ADD     0             KDCM
027400020408     c**
027500020408     C* MI POSIZIONO PER LNP TROVATA E DATA CONSEGNA MERCE = 0
027600020408     C     KBL2          SETLL     FNBLP37L
027700020408     C     Kbl2          READE     FNBLP37L                               35
027800020408     C*
027900020408DO  3C     *IN35         DOWEQ     *OFF
028000020409     C     BLPDsp        iflt      dtsncn_ymd
028100080604     c* Escludo la linea 101 gestita dalla sede
028200080604     c     blplna        andne     101
028300020411     c* Guardo se presente c.a.
028400040812      *
028500040812      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
028600040812      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
028700040812      * legate alla spedizione che passo
028800040812     c                   clear                   fidn12ds
028900040812     c                   eval      i12aas = BLPAAS
029000040812     c                   eval      i12lnp = BLPLNP
029100040812     c                   eval      i12nrs = BLPNRS
029200040812     c                   eval      i12nsp = BLPNSP
029300040812     c                   eval      i12fel = 'E'
029400040812     c*
029500040812     c                   call      'FIDN12R'
029600040812     c                   parm                    fidn12ds
029700040812     c*
029800040812     c                   setoff                                       32
029900080521     c
030000080521     c                   clear                   wchiusaCA         1
030100080521    5c                   if        o12err = ' ' AND o12nca > 0
030200080521     c                   seton                                        32
030300080521     c                   eval      WchiusaCA='S'
030400080521     c
030500080521     c* Stampo solo se tutte le c.a. sono chiuse
030600080521     c                   z-add     *zeros        ii                2 0
030700080521    6c     1             do        o12nca        ii
030800080521     c                   movea     skkey(ii)     dskey
030900080521     C                   Z-ADD     skdch(ii)     dctdch            8 0
031000080521     c
031100080521      * se CA chiusa stampo,imposto il flag e vado a fine
031200080521    7c                   If        dctdch = 0
031300080521     c                   clear                   wchiusaCA
031400080521    7c                   endif
031500080521    6c                   enddo
031600080521    5c                   endif
031700080521     c
031800080521    c
031900080521     c                   if        (*in32 and wchiusaCA='S' and
032000150519    5c                             BLPdsp<dtscan_ymd) or not *in32
032100080521     c
032200150519     c* Se la bolla è bloccata, stampo solo se passato il tempo
032300150519     c     kblp          chain     fnarb01l
032400150519     c*
032500150519     c                   if        not %found(fnarb01l) or
032600150519     c                             (arbfbc<>' ' and blpdsp<dtsrim_ymd) or
032700150519     c                             arbfbc=' '
032800020409     c                   exsr      ovrprt
032900020411     c  n32              movel(P)  cbncon        p01mes
033000020411     c   32              movel(P)  cbnconca      p01mes
033100020409     c                   exsr      ried1
033200020409     c                   write     ls61d1
033300020411     c                   endif
033400150519
033500150519     c                   endif
033600020408     c                   endif
033700020408     C*
033800020408     C     Kbl2          READE     FNBLP37L                               35
033900020408E   3C                   ENDDO
034000020408     C*
034100020408E   2C                   ENDIF
034200020408E   1C                   ENDDO
034300990823     C*
034400150519     c* Stampo le bolle cancellate
034500150519     c                   clear                   p1clnp
034600150519     c                   clear                   p1clna
034700150519     c                   clear                   p1cnrs
034800150519     c                   clear                   p1cnsp
034900150519     c                   clear                   p1cdsp
035000150519     c                   clear                   p1ccbo
035100150519     c                   clear                   p1ccca
035200150519     c                   clear                   p1cdcm
035300150519     c                   clear                   p1cfbc
035400150519     c                   clear                   p01mes
035500150519     c                   evaL      P01MES='Bolle cancellate:'+
035600150519     c                             %editc(pulbol:'2')
035700150519     c                   exsr      ovrprt
035800150519     c                   write     ls61d1
035900150519     c
036000150519     c*  Stampo piedi
036100020404     c     wopn          ifeq      '1'
036200020404     c                   write     ls61z1
036300020404     c                   endif
036400990823     C* CHIUDO CON LR
036500990823     C                   CLEAR                   DSBS02
036600990823     C                   MOVEL     'C'           T02TLA
036700990823     C                   CALL      'TIBS02R'
036800990823     C                   PARM                    KPJBA
036900990823     C                   PARM                    DSBS02
037000000925     C*
037100000925     C                   CLEAR                   DSLS63
037200000925     C                   MOVEL     'C'           I63TLA
037300000925     C                   MOVEL     DSLS63        KPJBU
037400000925     C                   CALL      'FNLS63R'
037500000925     C                   PARM                    KPJBA
037600951010     C*
037700930907     C                   SETON                                            LR
037800930902     C*---------------------------------------------------------------*
037900930902     C* ROUTINE DI CANCELLAZIONE BOLLE PARTITE                        *
038000930902     C*---------------------------------------------------------------*
038100990126     C     CANC          BEGSR
038200010502      *
038300990126     C                   MOVEL     'S'           WOK               1
038400010502     C                   clear                   WGIACE            1
038500090305     c
038600090305     c* Se c'e' bolla arrivi, non cancello la bolla
038700090305     c     kblp          setll     fnarb01l
038800090305     c                   if        %equal(fnarb01l)
038900090305     c                   clear                   WOK
039000090305     c                   endif
039100090305     c
039200090305     c                   if        wok='S'
039300010502      *
039400010502      * GIACENZA: se non trovata CANCELLO BOLLA
039500050906      *           se trovata ma è annullata o in fase 60 CANCELLO BOLLA
039600010502      *           negli altri casi non CANCELLO BOLLA
039700050906     C     KGCP          CHAIN     tigcp01L
039800010417      *
039900050906     c                   SELECT
040000050906     c                   WHEN      not %found(tigcp01L)
040100050906     c                   WHEN      GCPATB <> *blanks or GCPFAS = 60
040200050906     c                   OTHER
040300050906     c                   eval      Wgiace = 'S'
040400010502     c                   clear                   WOK
040500020402     c* stampo motivazione
040600020408     c     blpdsp        iflt      wdaprt
040700020402     c                   exsr      ovrprt
040800020402     c                   movel(P)  cbgiac        p01mes
040900020528     c* prendo la decodifica fase
041000020528     C                   MOVEL     '2Y'          COD
041100050906     C                   MOVEL(P)  gcpfas        KEY
041200020528     C     ktab          CHAIN     TABEL00F                           30
041300020528     C   30              MOVEL     *ALL'?'       decfas           45
041400020528     C  N30              MOVEL     TBLUNI        decfas
041500050906     c                   move      gcpfas        wcpfas            3
041600020530     c                   eval      p01mes = %trim(p01mes) + ' ' + wcpfas + '-'
041700020528     c                             + decfas
041800020404     c                   exsr      ried1
041900020404     c                   write     ls61d1
042000020408     c                   endif
042100050906     c                   ENDSL
042200010502      *
042300150519     C*
042400000529     C                   Z-ADD     PA1BLR        WDATA£            8 0
042500020408     C                   Z-ADD     dtsrsv_ymd    WDApr£            8 0
042600000529     C*
042700980721     C* SE LA BOLLA PRESENTA UN EVENTO DI RISERVA, CANCELLO SOLO SE
042800980721     C*  LA DATA DI CONSEGNA < ALLA DATA PULIZIA BOLLE CON RISERVA
042900980721    1C     WOK           IFEQ      'S'
043000101007     c                   setoff                                       32
043100101007     c                   z-add     1             yy
043200101007
043300101007     c                   dow       ris(YY)<>*blanks
043400101007     c                   eval      wcev=ris(YY)
043500101007     c
043600101007     C     KEVB4         SETLL     FNEVB04L
043700101007     c                   if        %equal(fnevb04l)
043800101007     c                   seton                                        32
043900101007     c                   leave
044000101007     c                   else
044100101007     c                   endif
044200101007     c                   add       1             yy
044300101007     c                   enddo
044400101007     c
044500980721    2C     *IN32         IFEQ      *ON
044600000529     C     BLPDCM        ANDGE     WDATA£
044700020402     c* stampo motivazione
044800150519     c* ES --> non serve stampare: quando passano i giorni di pulizia
044900150519     c*        verrà sicuramente pulita, almeno per questo motivo...
045000150519     c**   blpdsp        iflt      wdapr£
045100150519     c**                 exsr      ovrprt
045200150519     c**   *ymd          move      blpdcm        data_EUR
045300150519     c**                 move      data_EUR      data_Alfa
045400150519     c**                 movel(P)  cbrise        p01mes
045500150519     c**                 eval      p01mes = %trim(p01mes) + ' ' + data_Alfa
045600150519     c**                 exsr      ried1
045700150519     c**                 write     ls61d1
045800150519     c**                 endif
045900150519
046000980721     C                   CLEAR                   WOK
046100980721    2C                   ENDIF
046200980721    1C                   ENDIF
046300990126     C**
046400010502     C* SE C'E' C.A. DEVE ESSERE CHIUSA
046500150519     C* E LA DATA DI PULIZIA DEVE ESSERE > DELLA DATA CHIUSURA
046600990126     C                   CLEAR                   WDCH
046700020530     C                   CLEAR                   WDCH_dcb
046800150519     C*
046900020408     C                   Z-ADD     dtscan_ymd    WDApr£
047000040812      *
047100040812      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
047200040812      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
047300040812      * legate alla spedizione che passo
047400040812     c                   clear                   fidn12ds
047500040812     c                   eval      i12aas = BLPAAS
047600040812     c                   eval      i12lnp = BLPLNP
047700040812     c                   eval      i12nrs = BLPNRS
047800040812     c                   eval      i12nsp = BLPNSP
047900040812     c                   eval      i12fel = 'E'
048000150521     c*****              eval      i12fan = 'E'
048100040812     c*
048200040812     c                   call      'FIDN12R'
048300040812     c                   parm                    fidn12ds
048400040812     c*
048500040812     c                   setoff                                       32
048600040812     c*
048700040812     c* se non ci sono errori
048800040812     c                   if        o12err <> ' '
048900040812     c                   seton                                        32
049000040812     c                   else
049100040812     c* se numero di CA trovate maggiore di zero
049200040812     c                   if        o12nca = 0
049300040812     c                   seton                                        32
049400040812     c                   else
049500040812     c                   z-add     1             jj                2 0
049600040812     c                   dow       jj <= o12nca and *in32 = *off and WOK = 'S'
049700040812     c                   movea     skKey(jj)     dskey
049800040812     c*
049900040812     C     skDch(jj)     IFEQ      0
050000040812     C                   CLEAR                   WOK
050100040812     C                   ELSE
050200040812     C* MEMORIZZO LA DATA CHIUSURA PIU' ALTA
050300040812     C     skDch(jj)     IFGT      WDCH
050400040812     C                   Z-ADD     skDch(jj)     WDCH
050500040812     C                   ENDIF
050600040812     c* memorizzo la data chiusura più alta se p.o. C.A. = lnp bolla
050700040812     c     skDch(jj)     ifgt      wdch_dcb
050800040812     c     dsfil         andeq     blplnp
050900040812     c                   z-add     skDch(jj)     wdch_dcb
051000040812     c                   endif
051100040812     C                   ENDIF
051200040812     c*
051300040812     c                   add       1             jj
051400040812     c                   enddo
051500040812     c*
051600040812     c                   seton                                        32
051700040812     c*
051800040812     c                   endif
051900040812     c                   endif
052000040812     c**
052100000529     C                   Z-ADD     PA1BCA        WDATA£            8 0
052200090305     c
052300090305     C                   ENDIF
052400990126     C**
052500990126     C* SE C.A. TUTTE CHIUSE DEVONO ESSERE PASSATI I GIORNI DALLA DATA
052600990126     C*  CHIUSURA PIU' ALTA
052700990126     C     WOK           IFEQ      'S'
052800000529     C     WDCH          ANDGE     WDATA£
052900990126     C                   CLEAR                   WOK
053000990126     C                   ENDIF
053100990908     C* NON POSSO PULIRE BOLLA  SE NON HO ANCORA STAMPATO LA C.A.
053200990823     C* NELL'ELENCO C.A. DA ARCHIVIARE
053300990823    1C     WOK           IFEQ      'S'
053400020530     c     wdch_dcb      andgt     *zeros
053500990823     C                   CLEAR                   DSBS02
053600990824     C                   MOVEL     'C'           T02MOD
053700990823     C                   MOVEL     KNSIF         T02SIF
053800990823     C                   MOVEL     'DCB'         T02COD
053900990823     C                   MOVEL     BLPLNP        T02KE1
054000990823     C                   CALL      'TIBS02R'
054100990823     C                   PARM                    KPJBA
054200990823     C                   PARM                    DSBS02
054300990823    2C     T02ERR        IFEQ      *BLANKS
054400990823     C                   MOVEL     T02UNI        WDTACA            8 0
054500990823     C                   ELSE
054600990823     C                   Z-ADD     19990101      WDTACA
054700990823    2C                   ENDIF
054800020530    2C     WDCH_dcb      IFGT      WDTACA
054900020402     c* stampo motivazione
055000020408     c     blpdsp        iflt      wdapr£
055100020402     c                   exsr      ovrprt
055200020530     c     *ymd          move      wdch_dcb      data_EUR
055300020402     c                   move      data_EUR      data_Alfa
055400020402     c                   eval      p01mes = cbcann1 + data_alfa + cbcann2
055500020404     c                   exsr      ried1
055600020404     c                   write     ls61d1
055700020408     c                   endif
055800990823     C                   CLEAR                   WOK
055900990823    2C                   ENDIF
056000990823    1C                   ENDIF
056100020409     c*
056200020409     C* PULISCO SOLO SE NON TROVO RECORD NEI FILE TASSAZIONE PADRONCINI
056300020503     c     wok           ifeq      'S'
056400020409     c* se manca cod. padroncino ignoro ricerca perchè si tratta di
056500020409     c* prapagato non confermato
056600020409     c     blppdr        andgt     *zeros
056700020409     c                   clear                   p01mes
056800020409     c     blpfpp        ifeq      'M'
056900020409     c                   z-add     1000          wndc
057000020409     c                   else
057100020409     c                   z-add     5000          wndc
057200020409     c                   endif
057300020409     c                   add       blpnrt        wndc
057400020409     C     Kftd          SETLL     fiftd000                               32
057500020409     c     *in32         ifeq      *on
057600020409     c                   movel(P)  cbcpdr        p01mes
057700020409     c                   endif
057800020409     C  N32Kftd          SETLL     fifsd000                               32
057900020409     c     *in32         ifeq      *on
058000150519     c     blpdsp        iflt      wdaprt
058100020409     c     p01mes        ifeq      *blanks
058200020409     c                   movel(P)  cbcpdr        p01mes
058300020409     c                   eval      p01mes = %trim(p01mes) + ' di simulazione'
058400020409     c                   endif
058500020409     c                   exsr      ovrprt
058600020409     c                   exsr      ried1
058700020409     c                   write     ls61d1
058800020409     c                   endif
058900020409     c                   clear                   wok
059000020409     c                   endif
059100020409     c                   endif
059200040409      *
059300040409      * RECLAMI : se non trovata CANCELLO BOLLA
059400040409      *
059500040409     C*
059600040409    1C     WOK           IFEQ      'S'
059700040409     c                   movel     blpaas        W_aas
059800040409     c                   movel     blplnp        W_lnp
059900040409     c                   movel     blpnrs        W_nrs
060000040409     c                   movel     blpnsp        W_nsp
060100040409      *
060200040409     c                   movel     wspe          ktgogg
060300040409      *
060400040409     c     ktgd          setll     fitgd02l
060500040409     c                   do        *hival
060600040409     c     ktgd          reade     fitgd02l
060700040409
060800040409     c                   if        %eof(fitgd02l)
060900040409     c                   leave
061000040409     c                   endif
061100040423      * se annullato leggo il successivo
061200040423     c                   If        TGDatb <> ' '
061300040423     c                   iter
061400040423     c                   endif
061500151021      * verifico se reclamo ancora aperto da filiale
061600151021      * ( posso annullare se reclamo aperto da sede--> come se non ci fosse
061700151021     c                   If        (TGDdcd = 0 and tgdpog<>46) or
061800040423      * se reclamo chiuso verifico la data chiusura con la dta di pulizia
061900040423     c                             TGDdcd > datrec
062000040409     c                   clear                   wok
062100150519     c* stampo motivazione per bolle < 4 mesi
062200150519     c                   If        TGDdcd = 0  and blpdsp<wdaprt
062300040409     c                   exsr      ovrprt
062400040409     c                   movel(P)  cbcrec        p01mes
062500040409     c                   exsr      ried1
062600040409     c                   write     ls61d1
062700040409     c                   leave
062800040409     c                   endif
062900080521     c                   endif
063000040409
063100040409     c                   enddo
063200040409
063300040409    1c                   endif
063400060428      *
063500060428      * Prepagati: Se prepagato non incassato non pulisco
063600060428      *
063700060428     C*
063800060428    1C     WOK           IFEQ      'S'
063900060428     c     blpcbo        lookup    cbp                                    30
064000060428   1ac                   if        *in30 = *on
064100060428     c     kar6          chain     fiar601l
064200060428    2c                   if        %found(fiar601l) and ar6nft > 0
064300060428     c                   z-add     ar6aas        prmaas
064400060428     c                   z-add     ar6lnp        prmlnp
064500060428     c                   z-add     ar6nrs        prmnrs
064600060428     c                   z-add     ar6nsp        prmnsp
064700060428     c                   z-add     ar6fiv        prmfiv
064800060428     c                   z-add     ar6nft        prmnft
064900060428     c                   z-add     ar6dft        prmdft
065000060428     c                   clear                   prmesito
065100060428     c                   call(E)   'YCOW0121R'
065200060428     C                   PARM                    prmaas
065300060428     C                   PARM                    prmLnp
065400060428     C                   PARM                    prmNrs
065500060428     C                   PARM                    prmNsp
065600060428     C                   PARM                    prmFiv
065700060428     C                   PARM                    prmDft
065800060428     C                   PARM                    prmNft
065900060428     C                   PARM                    prmEsito
066000060428    3c                   if        prmesito=0
066100060428     c                   clear                   wok
066200060428     c* stampo motivazione
066300060428     c                   exsr      ovrprt
066400060428     c                   movel(P)  cbcprp        p01mes
066500060428     c     *ymd          move      ar6dft        data_EUR
066600060428     c                   move      data_EUR      data_Alfa
066700060428     c                   eval      p01mes = %trim(p01mes)
066800060502     c                             + ' Fattura n.' + %editc(ar6nft:'Z')
066900060428     c                             + ' del ' + data_Alfa
067000060428     c                   exsr      ried1
067100060428     c                   write     ls61d1
067200060428    3c                   endif
067300060428    2c                   endif
067400060428   1ac                   endif
067500060428    1c                   endif
067600930906     C*
067700000801    1C     WOK           IFEQ      'S'
067800930906     C*
067900931122     C* NON DELETO LA BOLLA SE C'E' IL C/ASSEGNO E NON C'E' IL
068000941102     C*   RECORD IN FNCSB00F
068100931122     C                   SETON                                        36
068200931122     C*
068300940929     C* TENTO DI REPERIRE IL RECORD PER CONTRASSEGNO
068400051115     C     KBLP          CHAIN     Fiar901L                           32
068500051115     C   32              Z-ADD     *ZERO         ar9CAS
068600940929     C*
068700981007     C* SE C'E' LA CONSEGNA ANOMALA=7 VUOLL DIRE CHE IL RECORD BOLLA
068800981007     C*  E' STATO CHIUSO IN PARTENZA PER CUI NON CI SARA' MAI IL RECORD
068900981007     C* DI INCASSO DEL C/A CHE QUINDI NON DEVE NEMMENO ESSERE CERCATO
069000981007     C     BLPCCA        IFNE      '7'
069100051115     C     ar9CAS        ANDGT     0
069200941102     C     KBLP          SETLL     FNCSB000                               36
069300010129     C* SE BOLLA POSTE AUTOGENERATA POTREI NON AVERE MAI IL RECORD DI
069400060216     C* INCASSO DEL C/A (LO VEDO NEL ar4 TIPO RECORD "J")9
069500150519     C***  *IN36         IFEQ      *OFF
069600150519     C***  §OGPT         ANDEQ     'S'
069700150519     C***  BLPSOP        ANDEQ     '&'
069800150519     C***                MOVEL     'J'           WTRC
069900150519     C***  KBL4          CHAIN     Fiar401L                           31
070000150519     C**N31              MOVEL     ar4NOT        DSBL4J
070100150519     C***31              CLEAR                   DSBL4J
070200150519     C***  §B4CSB        IFEQ      'S'
070300150519     C***                SETON                                        36
070400150519     C***                ENDIF
070500150519     C***                ENDIF
070600060120     c* Se non trovato record su csb prima di dire che si tratta di dati inc
070700060120     c* asso mancanti devo controllare lo stato del c'a sul file contrassegn
070800060120     c* di sede
070900060120     c     *in36         ifeq      *off
071000060120     C     KBLP          chain     tncsb03l
071100060120     c                   if        %found(tncsb03l)
071200060120     c* stato 9 --> Annullato; Data distinta > 0 --> c/a incassato
071300060120     c                   if        csbsta = 9 or csbddc > 0
071400060120     c                   seton                                        36
071500060120     c                   endif
071600060120     c                   endif
071700060120     c                   endif
071800931122     C                   ENDIF
071900020402     c*
072000020402     c     *in36         ifeq      *off
072100150519     c* stampo motivazione SUBITO: manca ARB non può non essere incassato
072200150519     c***  blpdsp        iflt      wdaprt
072300020402     c                   exsr      ovrprt
072400020402     c                   movel(P)  cbccass       p01mes
072500020404     c                   exsr      ried1
072600020404     c                   write     ls61d1
072700150519     c***                endif
072800931122     C*
072900020402     c                   else
073000010605     C* CANCELLO LE C.A.
073100150521     c*****              eval      *in96 = *off
073200150521     C*****              move      *blanks       WERR              1
073300150521     c*****kblp          setll     fnarb01l                               96
073400150521     c*****              if        not *in96
073500010605      *
073600040812      *
073700150521     c****               clear                   fidn12ds
073800150521     c****               eval      i12aas = BLPAAS
073900150521     c****               eval      i12lnp = BLPLNP
074000150521     c****               eval      i12nrs = BLPNRS
074100150521     c****               eval      i12nsp = BLPNSP
074200150521     c****               eval      i12fel = 'E'
074300040812     c*
074400150521     c****               call      'FIDN12R'
074500150521     c****               parm                    fidn12ds
074600040812     c*
074700040812     c                   setoff                                       32
074800150521     c* Le c.a. le ho già caricate prima: rileggo semplicemente le schiere
074900040812     c                   if        o12err <> ' '
075000150521     C                   MOVEL     '1'           WERR              1
075100040812     c                   else
075200040812     c* se numero di CA trovate maggiore di zero
075300150521     c                   if        o12nca > 0
075400040812     c                   z-add     1             jj                2 0
075500040812     c                   dow       jj <= o12nca and *in32 = *off
075600040812     c                   movea     skKey(jj)     dskey
075700040812     c*
075800040812     C                   CLEAR                   fidn11ds
075900040812     C                   MOVEL     'D'           I11MOD
076000040812     C                   Z-ADD     dsaac         I11AAC
076100040812     C                   Z-ADD     dsfil         I11FIL
076200040812     C                   Z-ADD     dsnca         I11NCA
076300151021     C**                 CALL      'FIDN11R'
076400151021     C**                 PARM                    KPJBA
076500151021     C**                 PARM                    fidn11ds
076600040812      *
076700040812    7C     O11ERR        IFEQ      'E'
076800040812     C                   MOVEL     '1'           WERR
076900040812      *
077000111103      * ES 03 11 11 --> non invio più messaggio unico errore
077100111103      *                 è tipo richiamo del pgm errato
077200040812      * SE ERRORE INVIO ANCHE MESSAGGIO IN SEDE CON
077300040812      * RIFERIMENTI BOLLA: ANNO/LINEA/NR.SERIE/NR.SPEDIZ.
077400040812      *
077500111103     C**                 Z-ADD     blpAAS        MSGAAS
077600111103     C**                 Z-ADD     blplNP        MSGLNP
077700111103     C**                 Z-ADD     blpNRS        MSGNRS
077800111103     C**                 Z-ADD     blpNSP        MSGNSP
077900040812      *
078000111103     C**                 MOVEL(p)  MSG(1)        MSGTES
078100111103     C**                 MOVEL     MSG(2)        WWWW41           41
078200111103     C**                 MOVE      PARAM         WWWW41
078300111103     C**                 MOVEL(P)  WWWW41        MSGDET
078400040812      *
078500040812    7C                   ENDIF
078600040812     c*
078700040812     c                   add       1             jj
078800040812     c                   enddo
078900040812     c*
079000040812     c                   endif
079100040812     c                   endif
079200040812     c**
079300010605     C* SE NON SONO RIUSCITA A CANCELLARE TUTTE LE C.A. NON POSSO
079400010605     C* CANCELLARE LA BOLLA
079500150519    5c     werr          ifeq      *blanks
079600151021     c                   add       1             pulbol            9 0
079700151021    5c     werr          ifeq      *blanks
079800151021    5c     werr          andne     *blanks
079900130903
080000130903     c* Cancello eventuali PDF
080100130903     c                   exsr      Canpdf
080200130903     c* Cancello la bolla solo se non ci sono stati errori nella cancellazione
080300130903     c* di eventuali PDF
080400150519    6c     wesitopdf     ifeq      *blanks
080500000801      *
080600000801      * CANCELLAZIONE FISICA BOLLA E FILES ALLEGATI
080700000801      *
080800000801     C                   CLEAR                   DSLS63
080900000801     C                   MOVE      BLPAAS        I63AAS
081000000801     C                   MOVE      BLPLNP        I63LNP
081100000801     C                   MOVE      BLPNRS        I63NRS
081200000801     C                   MOVE      BLPNSP        I63NSP
081300000801     C                   MOVEL     DSLS63        KPJBU
081400151021     C**                 CALL      'FNLS63R'
081500151021     C**                 PARM                    KPJBA
081600150519     c*
081700000801      *
081800050906      * Pulizia Giacenza: Se wgiace = 'S' vuol dire che c'è giacenza aperta
081900050906      *                   in questo caso non posso pulire la giacenza
082000050906      *                   Posso pulire la giacenza quando non ci sono
082100050906      *                   più bolle su ARB legate alla bolla originale
082200050906if  4c                   If        WGIACE = *Blanks
082300050906     c     kgcp          Setll     Tigcp01L
082400050906      * Entro nel ciclo solo se ho trovato una giacenza
082500050906if  5c                   If        %Equal(Tigcp01l)
082600050906do  6c                   Do        *Hival
082700050906     c     kgcp          Reade     Tigcp01L
082800050906      * Fine file
082900050906     c                   If        %Eof(Tigcp01l)
083000050906     c                   Leave
083100050906     c                   EndIf
083200050906      * Giacenza ancora aperta torno a leggere
083300050906     c                   If        GCPATB = *blanks and GCPFAS < 60
083400050906     c                   Iter
083500050906     c                   EndIf
083600050906      * Controllo se esiste la bolla su ARB
083700050906     c     karb          Setll     Fnarb01l
083800050906      * Esiste la bolla su ARB esco dal ciclo tanto non devo pulire la giacenza
083900050906      * se esiste anche solo 1 bolla
084000050906      * imposto anche il flag x pilotare la pulizia delle giacenze
084100050906if  7c                   If        %Equal(Fnarb01l)
084200050906     c                   eval      Wgiace = 'S'
084300050906     c                   Leave
084400050906    7c                   EndIf
084500050906    6c                   EndDo
084600050906    5c                   EndIf
084700050906    4c                   EndIf
084800050906
084900050906      * Pulizia Giacenza: Se wgiace = 'S' vuol dire che ho trovato anche solo 1
085000050906      *                   bolla su ARB e quindi non devo pulire la giacenza
085100150519if  5c                   If        wgiace = *Blanks
085200010507     c                   z-add     blpaas        iz5aas
085300010507     c                   z-add     blplnp        iz5lnp
085400010507     c                   z-add     blpnrs        iz5nrs
085500010507     c                   z-add     blpnsp        iz5nsp
085600151021     c**                 call      'FNLGZ5R'
085700151021     c**                 PARM                    FNLGZ5DS
085800151021 ---5C                   ENDIF
085900151021 ---5C                   ENDIF
086000150519    4C                   ENDIF
086100150519    3C                   ENDIF
086200010502      *
086300150519--- 2C                   ENDIF
086400931122     C*
086500000801    1C                   ENDIF
086600930903     C*
086700930902     C                   ENDSR
086800130903     C*---------------------------------------------------------------*
086900130903     C* Cancellazione PDF - PACKING LIST                              *
087000130903     C*---------------------------------------------------------------*
087100130903     C     canpdf        BEGSR
087200130903     c                   eval      wesitoPDF=' '
087300130903     c     kar5          chain     fiar501l
087400130903    1c                   if        %found(fiar501l)
087500130903     c                   eval      dar5gen=ar5uni
087600130903    2c                   if        §ar5alm='S' or §ar5alm='E' or
087700130903     c                             §ar5als='S' or §ar5als='E'
087800130903     c     kar8          setll     fiar801l
087900130903     c     kar8          reade     fiar801l
088000130903    3c                   dow       not %eof(fiar801l)
088100130903     c                   eval      dar8§pdf=ar8uni
088200130903     c                   eval      pIn_FolderPDF = §ar8ppdfp
088300130903     c                   eval      pIn_NamePDF = §ar8ppdfn
088400151021     c**                 call      'TIS7P2SR3'
088500151021     c**                 parm                    pIn_FolderPDF
088600151021     c**                 parm                    pIN_NamePDF
088700151021     c**                 parm                    pOut_Esito
088800130903    4c                   if        pOut_Esito='1'
088900130903     c                   eval      wesitoPDF='E'
089000130903    4c                   endif
089100130903     c
089200130903     c     kar8          reade     fiar801l
089300130903    3c                   enddo
089400130903    2c                   endif
089500130903    1c                   endif
089600130903     c* Se errori nella cancellazione del/i PDF stampo motivo di non pulizia
089700130903     c                   if        wesitoPdf='E'
089800130903     c* stampo motivazione
089900130904     c***  blpdsp        iflt      wdaprt
090000130903     c                   exsr      ovrprt
090100130903     c                   movel(P)  cbcpdf        p01mes
090200130903     c                   exsr      ried1
090300130903     c                   write     ls61d1
090400130904     c***                endif
090500130903     c                   endif
090600130903     c                   endsr
090700020404     C*---------------------------------------------------------------*
090800020404     C* OVRPRT - Operazioni su file printer                           *
090900020404     C*---------------------------------------------------------------*
091000020404     C     ovrprt        BEGSR
091100020404     c* override su file printer + apertura
091200020404     c     wopn          ifeq      *blanks
091300020409     c                   movel     cmd(1)        comman          100
091400020409     c                   eval      %subst(cmd(2):6:4) = whld
091500020409     c                   eval      %subst(cmd(2):17:4) = wsav
091600020409     c                   eval      comman = %trim(comman) + '('
091700020408     c                             + %trim(§5alib) + '/'
091800020409     c                             + %trim(§5aotq) + ') ' + %trim(cmd(2))
091900020408     C                   Z-ADD     100           LUNGH
092000020404     C                   CALL      'QCMDEXC'
092100020404     C                   PARM                    COMMAN
092200020404     C                   PARM                    LUNGH            15 5
092300020404     c                   open      fnls61p
092400020404     c* valorizzazione campi di stampa testata
092500020404     c* data bolle lnp italia
092600020404     c     *ymd          move      pa1dbr        data_dmy
092700020404     c                   move      data_dmy      dtpita
092800020404     c* data bolle import
092900020404     c     *ymd          move      pa1pbi        data_dmy
093000020404     c                   move      data_dmy      dtpimp
093100020404     c* data bolle con riserva
093200020404     c     *ymd          move      pa1blr        data_dmy
093300020404     c                   move      data_dmy      dtprsv
093400020404     c* data bolle con c.a.
093500020404     c     *ymd          move      pa1bca        data_dmy
093600020404     c                   move      data_dmy      dtpcan
093700020404     c*
093800020404     c                   write     ls61t1
093900020404     c                   movel     '1'           wopn
094000020404     c                   endif
094100020404     c* stampa intestazione a rottura
094200020404     c   99              write     ls61t1
094300020404     c                   setoff                                       99
094400020404     c                   endsr
094500020404     C*---------------------------------------------------------------*
094600020404     C* RIED1  - Valorizzazione campi di stampa dettaglio             *
094700020404     C*---------------------------------------------------------------*
094800020404     C     RIED1         BEGSR
094900020404     c                   z-add     blplnp        p1clnp
095000020404     c                   z-add     blplna        p1clna
095100020404     c                   z-add     blpnrs        p1cnrs
095200020404     c                   z-add     blpnsp        p1cnsp
095300020404     c     *ymd          move      blpdsp        data_EUR
095400020404     c                   move      data_EUR      p1cdsp
095500080521     c                   MOVEL     BLPCBO        p1cCBO
095600080521     c                   MOVEL     BLPCCA        p1cCCA
095700150519     c                   MOVEL     BLPfbr        p1cfbc
095800080521     c                   if        BLPdcm>0
095900080521     c     *ymd          move      BLPDCM        data_EUR
096000080521     c                   move      data_EUR      p1cDCM
096100080521     c                   else
096200080521     c                   clear                   p1cdcm
096300080521     c                   endif
096400020404     c                   endsr
096500930901     C*---------------------------------------------------------------*
096600940930     C*                    *INZSR                                     *
096700930901     C* ROUTINE DI DEFINIZIONE VARIABILI, KLIST E PLIST               *
096800930901     C*---------------------------------------------------------------*
096900940930     C     *INZSR        BEGSR
097000930901     C*
097100930901     C     *ENTRY        PLIST
097200930901     C                   PARM                    KPJBA
097300940930     C                   MOVEL     KPJBU         PARAM1
097400930901     C*
097500010502     C* LETTURA ESTENSIONI EVENTI, GIACENZE, DETTAGLIO BOLLA
097600930901     C     KBLP          KLIST
097700930901     C                   KFLD                    BLPAAS
097800930901     C                   KFLD                    BLPLNP
097900930901     C                   KFLD                    BLPNRS
098000930901     C                   KFLD                    BLPNSP
098100060428     C     Kar6          KLIST
098200060428     C                   KFLD                    BLPAAS
098300060428     C                   KFLD                    BLPLNP
098400060428     C                   KFLD                    BLPNRS
098500060428     C                   KFLD                    BLPNSP
098600060428     c                   kfld                    k6trc
098700130903     C     Kar5          KLIST
098800130903     C                   KFLD                    BLPAAS
098900130903     C                   KFLD                    BLPLNP
099000130903     C                   KFLD                    BLPNRS
099100130903     C                   KFLD                    BLPNSP
099200130903     c                   kfld                    ktrd
099300130903     C     Kar8          KLIST
099400130903     C                   KFLD                    BLPAAS
099500130903     C                   KFLD                    BLPLNP
099600130903     C                   KFLD                    BLPNRS
099700130903     C                   KFLD                    BLPNSP
099800130903     c                   kfld                    ktmd
099900050413
100000050906     C     KGCP          KLIST
100100050906     C                   KFLD                    BLPAAS
100200050906     C                   KFLD                    BLPLNP
100300050906     C                   KFLD                    BLPNRS
100400050906     C                   KFLD                    BLPNSP
100500050906     C                   KFLD                    KFRG
100600050404
100700050906     c     karb          klist
100800050906     c                   kfld                    GcpAasa
100900050906     c                   kfld                    GcpLnpa
101000050906     c                   kfld                    GcpNrsa
101100050906     c                   kfld                    GcpNspa
101200050404
101300980721     C     KEVB4         KLIST
101400101007     C                   KFLD                    blpaas
101500980721     C                   KFLD                    BLPLNP
101600980721     C                   KFLD                    BLPNRS
101700980721     C                   KFLD                    BLPNSP
101800980721     C                   KFLD                    WCEV
101900930906     C*
102000930906     C* LETTURA BOLLA (37C)
102100930906     C     KBL2          KLIST
102200940929     C                   KFLD                    KFIL
102300940929     C                   KFLD                    KDCM
102400020408     C*
102500020408     C     KTAB          KLIST
102600020408     C                   KFLD                    CODUT             1 0
102700020408     C                   KFLD                    COD
102800020408     C                   KFLD                    KEY
102900060428     C     KTAB2         KLIST
103000060428     C                   KFLD                    CODUT             1 0
103100060428     C                   KFLD                    COD
103200060428
103300020408     C                   Z-ADD     1             CODUT
103400020409     c*
103500020409     C     Kftd          KLIST
103600020409     C                   KFLD                    blppdr
103700020409     C                   KFLD                    wtsr
103800020409     C                   KFLD                    wndc
103900020409     C                   KFLD                    blpdrt
104000020409     C                   KFLD                    blpAAS
104100020409     C                   KFLD                    blpLNP
104200020409     C                   KFLD                    blpNRS
104300020409     C                   KFLD                    blpNSP
104400020409     c                   movel     'R'           wtsr
104500040409     c*  File Fitgd02l    RECLAMI
104600040409     c     ktgd          klist
104700040409     c                   kfld                    ktgtor
104800040409     c                   kfld                    ktgogg
104900940930     C*
105000951010     C     *LIKE         DEFINE    BLPLNP        KFIL
105100020211     C     *LIKE         DEFINE    BLPLNP        savlnp
105200020211     C     *LIKE         DEFINE    §ogntw        lnpntw
105300940929     C     *LIKE         DEFINE    BLPDCM        KDCM
105400980721     C     *LIKE         DEFINE    EVBCEV        WCEV
105500050906     C     *LIKE         DEFINE    GCPFRG        KFRG
105600020408     C     *LIKE         DEFINE    TBLCOD        COD
105700020408     C     *LIKE         DEFINE    TBLKEY        KEY
105800020409     C     *LIKE         DEFINE    ftdtsr        wtsr
105900020409     C     *LIKE         DEFINE    ftdndc        wndc
106000020408     c*
106100020408     C                   TIME                    wtime            14 0
106200020408     C                   MOVE      WTIME         WDATE             8 0
106300020409     c     *eur          move      wdate         wdate_inv
106400980721     C*
106500050906     C                   clear                   kfrg
106600010605     c*
106700010605     C                   z-add     1             codut             1 0
106800010605     C                   call      'X§PARUT'
106900010605     C                   parm                    UT§DSE
107000010605     C                   movel     rec80         CNCR80
107100020404     c                   movel     ragut         rsut
107200010605     c*
107300010605      * COSTANTI MESSAGGIO
107400010605     C                   MOVE      '/'           BARRW1
107500010605     C                   MOVE      '/'           BARRW2
107600010605     C                   mOVE      '/'           BARRW3
107700020408     c*
107800020408     c                   exsr      caldts
107900060428     c*
108000060428     c* carico schiera contenente codici bolla validi per prepagati
108100101007     C                   Z-ADD     0             X
108200060428     C                   MOVEL     '3A'          COD
108300060428     C     KTAB2         SETLL     TABEL00F
108400060428     C                   DO        *HIVAL
108500060428     C     KTAB2         READE     TABEL00F
108600060428     C                   IF        %eof(tabel00f)
108700060428     C                   LEAVE
108800060428     C                   ENDIF
108900060428     C                   IF        tblflg <> *BLANKS
109000060428     C                   ITER
109100060428     C                   ENDIF
109200060428     C                   MOVEL     TBLUNI        DS3A
109300061107     c                   if        %subst(§3atb1:1:1)='F' and §3arbl<>'C'
109400060428     C                   ADD       1             X                 2 0
109500060428     C                   MOVEL     TBLKEY        CBP(X)
109600060428     c                   endif
109700060428     C                   ENDDO
109800940930     C*
109900101007     c* Carico gli eventi di riserva dalla tabella 2A
110000101007     C                   CLEAR                   YY                3 0
110100101007     C                   MOVEL     '2A'          COD
110200101007     C     KTAB2         SETLL     TABEL
110300101007     C     KTAB2         READE     TABEL                                  30
110400101007     C     *IN30         DOWEQ     *OFF
110500101007     C     TBLFLG        IFEQ      ' '
110600101007     C                   MOVEL     TBLUNI        DS2A
110700101007     C     §2Atip        IFeq      'R'
110800101007     C                   ADD       1             YY
110900101007     C                   MOVEL     TBLKEY        ris(YY)
111000101007     C                   ENDIF
111100101007     C                   ENDIF
111200101007     C     KTAB2         READE     TABEL                                  30
111300101007     C                   ENDDO
111400930901     C                   ENDSR
111500020408     C*---------------------------------------------------------------*
111600020408     C* caldts - Calcolo date stampa                                  *
111700020408     C*---------------------------------------------------------------*
111800020408     C     CALDTS        BEGSR
111900020408      * LETTURA giorni per date stampa e coda di output
112000020408     C                   MOVEL     '5A'          COD
112100020408     C                   MOVEL     '2       '    KEY
112200020408     C     ktab          CHAIN     taBEL00F                           30
112300020408     C  N30              MOVEL     TBLUNI        DS5A2
112400020408     C   30              CLEAR                   DS5A2
112500020408     C*
112600020408     C                   MOVEL     '5A'          COD
112700020408     C                   MOVEL     '3       '    KEY
112800020408     C     ktab          CHAIN     TABEL00F                           30
112900020408     C  n30              MOVEL     TBLUNI        DS5A3
113000020408     C   30              CLEAR                   DS5A3
113100020409     c     §5ahld        ifeq      'S'
113200020409     c                   movel     '*YES'        whld              4
113300020409     c                   else
113400020409     c                   movel     '*NO '        whld
113500020409     c                   endif
113600020409     c     §5asav        ifeq      'S'
113700020409     c                   movel     '*YES'        wsav              4
113800020409     c                   else
113900020409     c                   movel     '*NO '        wsav
114000020409     c                   endif
114100150519     c* BOLLE
114200020408     c* 1) data stampa bolle lnp italia
114300020408     c     *ymd          move      pa1dbr        data_iso
114400020408     c                   subdur    §5agal:*d     data_iso
114500020408     c                   move      data_iso      dtsita_ymd
114600020408     c                   move      data_iso      data_dmy
114700020408     c                   move      data_dmy      dtsita
114800020408     c* 2) data stampa bolle import
114900020408     c     *ymd          move      pa1pbi        data_iso
115000020408     c                   subdur    §5agal:*d     data_iso
115100020408     c                   move      data_iso      dtsimp_ymd
115200020408     c                   move      data_iso      data_dmy
115300020408     c                   move      data_dmy      dtsimp
115400020408     c* 3) data stampa bolle con riserva
115500020408     c     *ymd          move      pa1blr        data_iso
115600020408     c                   subdur    §5agal:*d     data_iso
115700020408     c                   move      data_iso      dtsrsv_ymd
115800020408     c                   move      data_iso      data_dmy
115900020408     c                   move      data_dmy      dtsrsv
116000020408     c* 4) data stampa bolle con c.a.
116100020408     c     *ymd          move      pa1bca        data_iso
116200020409     c                   subdur    §5agcn:*d     data_iso
116300020408     c                   move      data_iso      dtscan_ymd
116400020408     c                   move      data_iso      data_dmy
116500020408     c                   move      data_dmy      dtscan
116600020411     c                   move      data_dmy      dtsncc
116700150519     c* 4) Rimpalli: 5 volte bolle con c.a.
116800150519     c     *ymd          move      pa1bca        data_iso
116900150519     c     §5agcn        mult      5             wrimp             5 0
117000150519     c                   subdur    wrimp:*d      data_iso
117100150519     c                   move      data_iso      dtsrim_ymd
117200150519     c                   move      data_iso      data_dmy
117300150519     c                   move      data_dmy      dtsrim
117400020408     c*
117500020409     c* Data per stampare le spedizioni non consegnate
117600020409     c                   subdur    §5agsn:*d     wdate_inv
117700020409     c                   move      wdate_inv     dtsncn_ymd
117800020409     c                   move      wdate_inv     data_dmy
117900020409     c                   move      data_dmy      dtsncn
118000020411     c* Per quanto riguarda la data delle non consegnate con C.A. l'ho
118100020411     c* già impostata come la data per le spediz. con c.a. (vedi sopra)
118200020408     c                   endsr
118300010605**
118400010605ERRORE "PULIZIA BOLLE PARTENZE"
118500010605PGM: FNLS61R - BOLLA:
118600020404**
118700020408OVRPRTF FNLS61P OUTQ
118800120221HOLD(    ) SAVE(    ) USRDTA('PulBoPar')
