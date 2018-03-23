000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941212     H* FNLR84R *-----------------------------------------------------*
000300930628     H*                 - PULIZIA BOLLE ARRIVI -                      *
000400040831      *
000500040831      * COMPILARE CON
000600040831      *               OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
000700060123      *               OVRDBF FILE(TNCSB03L) TOFILE(GAITRAGRPS/TNCSB03L)
000800930628     H*---------------------------------------------------------------*
000900971002     FFNBLP01L  IF   E           K DISK
001000000927     FTABEL00F  IF   E           K DISK
001100980722     FAZORG01L  IF   E           K DISK
001200060502     FFNARB54L  IF   E           K DISK    infds(arbd1)
001300060502     FFNARB00F  UF   E             DISK    rename(fnarb000:fnarbfis)
001400941207     F*
001500941207     F* ESTENSIONE DESCRIZIONI
001600060220     FFiAR401L  UF   E           K DISK
001700030108     F*
001800030108     F* ESTENSIONE TESTATA BOLLA
001900030108     FFIAR501L  UF   E           K DISK
002000040604     FFIAR801L  UF   E           K DISK
002100941207     F*
002200941207     F* TASSAZIONE 1 BOLLA
002300991104     FFIAR601L  UF   E           K DISK
002400941207     F*
002500941207     F* ESTENSIONE TASSAZIONE 1 BOLLA
002600991104     FFIAR701L  UF   E           K DISK
002700941207     F*
002800941207     F* C/ASSEGNO
002900051115     FFiAR901L  UF   E           K DISK
003000991104     F**
003100991104     FFIARI01L  UF   E           K DISK
003200061107     F**
003300061107     FFIARG01L  UF   E           K DISK
003400070503     FFIARP01L  UF   E           K DISK
003500070503     FFIARN01L  UF   E           K DISK
003600941207     F*
003700941207     F* ARCHIVIO VARIAZIONI BOLLE
003800991104     FFIARBF1C  UF   E           K DISK
003900991104     FFIARBU1L  UF   E           K DISK
004000941207     F*
004100941207     F* DETTAGLIO COLLI
004200941207     FFNART01L  UF   E           K DISK
004300051012     FFIARS01L  UF   E           K DISK
004400941207     F*
004500941207     F* ARCHIVIO EVENTI
004600990806     FFNEVB01L  UF   E           K DISK
004700990806     FFNEVB04L  IF   E           K DISK
004800990806     F                                     RENAME(FNEVB000:FNEVB004)
004900941207     F*
005000961125     F* ARCHIVI  EVENTI SEGNACOLLI
005100961125     FFNEVS01L  UF   E           K DISK
005200961125     FFNEVS02L  UF   E           K DISK
005300961125     F                                     RENAME(FNEVS000:FNEVS002)
005400961125     F*
005500941207     F* ARCHIVIO BOLLE GIACENZA IN ARRIVO
005600050906     FTigcp21L  IF   E           K DISK
005700941207     F*
005800950110     F* ARCHIVI  PADRONCINI DETTAGLIO TASSAZIONE
005900060206     Ffiftd22L  IF   E           K DISK
006000011121     Ffifsd02L  IF   E           K DISK
006100011121     F                                     RENAME(fiftd000:fifsd000)
006200990126     F* ARCHIVI  C.A.
006300020530     FFndcf01l  IF   E           K DISK
006400020530     FFndfa01l  IF   E           K DISK
006500040423     F
006600040423     F* ARCHIVI  RECLAMI
006700040423     Ffitgd02l  IF   E           K DISK
006800040831      * Bolle sede
006900040831     fTitas30c  if   e           k Disk    UsrOpn
007000060123     F* ARCHIVIO Contrassegni di sede
007100060123     FTNCSB03L  IF   E           K DISK    usropn
007200040423     F*
007300020404     Ffnlr84p   O    E             PRINTER USROPN oflind(*in99)
007400950110     D*
007500060502     D arbD1           DS
007600060502     D  ARBNRR               397    400B 0
007700060502     d
007800000927     D CBO             S              2    DIM(50)                              COD BOLLA DOPP
007900101007     D ris             S              3    DIM(50)                              COD BOLLA DOPP
008000020409     d cmd             s             50    dim(2) CTDATA PERRCD(1)
008100060123     d cmdt            s             60    dim(02) ctdata perrcd(1)
008200020404     D*
008300020404     d data_EUR        s               d   datfmt(*EUR)
008400020404     d data_DMY        s               d   datfmt(*DMY)
008500020409     d data_ISO        s               d   datfmt(*ISO)
008600020409     d wdate_inv       s               d   datfmt(*ISO)
008601120216     d wdateu          s               d   datfmt(*ISO)
008700020404     d data_Alfa       s             10a
008800020409     d dtsita_ymd      s              8  0
008900020409     d dtsexp_ymd      s              8  0
009000020409     d dtsrsv_ymd      s              8  0
009100020409     d dtscan_ymd      s              8  0
009200020409     d dtsncn_ymd      s              8  0
009201120220     d dtsgdc_ymd      s              8  0
009300020409     d dpsita_ymd      s              8  0
009400020409     d dpsexp_ymd      s              8  0
009500020409     d dpsrsv_ymd      s              8  0
009600020409     d dpscan_ymd      s              8  0
009700020404     d wopn            s              1a
009800040812     d dctdch          s              8  0
009900040812     d dctcch          s              2a
010000040831     d wlib            s             10
010100040831     d commant         s             80
010200040831     d lenght          s             15  5 inz(80)
010300040423
010400040423     d ktgtor          s                   like(tgdtor) inz('S')
010500040423     d ktgogg          s                   like(tgdogg)
010600080711     d ktrc            s                   like(ar4trc)
010700080711     d W008a           s              8
010800080711     d W0080           s              8  0
010900080711     d Indx            s              3  0
011000040423
011100941207     D*
011200961204     D* PARAMETRI RICEVUTI DAI PGM CHIAMANTI
011300900618     D PARAM           DS
011400961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA NON ESTERA)
011500941207     D  COMDPB                17     24  0
011600961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA ESTERA - BOLLE EXPORT)
011700961204     D  COMPBE                68     75  0
011800980722     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA
011900990707     D  COMARR               191    198  0
012000990126     D* DATA PULIZIA BOLLE ARRIVI CON C.A.
012100990707     D  COMBCA               199    206  0
012200000530     D* DATA PULIZIA BOLLE ARRIVI - POSTE
012300000530     D  POSDPB               207    214  0
012400000530     D* DATA PULIZIA BOLLE ARRIVI - BOLLE EXPORT - POSTE
012500000530     D  POSPBE               215    222  0
012600000530     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA - POSTE
012700000530     D  POSARR               223    230  0
012800000530     D* DATA PULIZIA BOLLE ARRIVI CON C.A. - POSTE
012900000530     D  POSBCA               231    238  0
013000040423     D* DATA PULIZIA BOLLE CON RECLAMI
013100040423     D  COMREC               239    246  0
013200961204     D*
013300920324     D                 DS
013400941207     D  ARBAAS                 1      4  0
013500941207     D  ARBMGS                 5      8  0
013600941207     D  ARBDSP                 1      8  0
013700020405     D                 DS
013800040813     D  dctAAc2                1      4  0
013900020405     D  dctmgc                 5      8  0
014000020405     D  dctdca                 1      8  0
014100941207     D*
014200990823     D* DS PER TIBS02DS- RICERCA TABELLE
014300990823     D DSBS02        E DS                  EXTNAME(TIBS02DS)
014400990823     D DSDN11        E DS                  EXTNAME(FIDN11DS)
014500941207     D*
014600000530     D OG143         E DS
014700101007     D DS2A          E DS
014800101007     D DS3A          E DS
014900020409     D DS5A2         E DS
015000020409     D DS5A3         E DS
015100000000     D KPJBA         E DS
015200930507     D CNCR80        E DS
015300930507     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
015400050404     D FNLGZ5DS      E DS
015500040812      * ds per il controllo della presenza di CA per la spedizione richiesta
015600040812     d FIDN12DS      e ds
015700040812     d  skKey                 26    305    dim(20)
015800040812     d  skAnn                306    325    dim(20)
015900040812     d  skDca                326    485  0 dim(20)
016000040812     d  skFca                486    545  0 dim(20)
016100040812     d  skDch                546    705  0 dim(20)
016200040812     d  skCch                706    745    dim(20)
016300040812
016400040812     d dsKey           ds
016500040812     d  dctaac                        4  0
016600040812     d  dctfil                        3  0
016700040812     d  dctnca                        7  0
016800040812      *
016900040423     D*-------------
017000040423     D* DS OGGETTO RECLAMO
017100040423     D*-------------
017200040423     D                 DS
017300040423     D  W_lnp                         3  0
017400040423     D  W_nrs                         2  0
017500040423     D  W_nsp                         7  0
017600040423     D  W_aas                         4  0
017700040423     D  Wspe                   1     16  0 inz
017800020404     D*
017900020404     D* DEFINIZIONE COSTANTI
018000020404     D*
018100020530     D CBGIAC          C                   CONST('Bolla giacente in fase')
018200020404     D CBRISE          C                   CONST('Bolla con riserva consegnata -
018300020404     D                                     il            ')
018400020530     D CBCANA          C                   CONST('Bolla con C.A. aperta - Fase')
018500020404     D CBCANC          C                   CONST('Bolla con C.A. chiusa il')
018600020404     D CBCANN1         C                   CONST('Presente C.A. chiusa il ')
018700020404     D CBCANN2         C                   CONST(' non ancora stampata nell''el-
018800020404     D                                     enco C.A. da archiviare')
018900080711     D CBcass          C                   CONST('C/Assegno non incassato')
019000020405     D CBercan         C                   CONST('Errore in cancellazione C.A. -
019100020405     D                                     n.')
019200020405     D CBcpdr          C                   CONST('Bolla presente su archivio co-
019300020404     D                                     nteggi padroncini')
019400080515     D CBChoprN        C                   CONST('Manca numero MANDATO INTROITO-
019500080515     D                                      ma c''è data')
019600080515     D CBChoprD        C                   CONST('Chiusura operatore non effett-
019700080515     D                                     uata')
019800020405     D CBinc           C                   CONST('Spedizione ancora da incassar-
019900020405     D                                     e')
020000020409     D CBncon          C                   CONST('Bolla non consegnata')
020100020411     D CBnconca        C                   CONST('Bolla non consegnata con C.A.-
020200080515     D                                     chiusa')
020300080515     D CBcrec          C                   CONST('Bolla con Reclamo APERTO')
020400040831     D CBdt1           C                   CONST('Bolla con data trasmissione <-
020500040831     D                                     data mandato di introito')
020600010508      *
020700941207     IFNARBD00      01
020800941207     IFNARBK00      02
020900991104     IFIARBT00      03
021000991104     IFNARBP00      04
021100941207     IFNARBG00      05
021200941207     IFNARBM00      06
021300941207     IFNARBV00      07
021400930507     C*---------------------------------------------------------------*
021500950516     C* MI SALVO LA KPJBU
021600950516     C                   MOVEL     KPJBU         WSAJBU
021700950516     C*
021800020611     C* CARICO TABELLE
021900930507     C                   EXSR      CARTAB
022000930507     C*
022100920306     C                   Z-ADD     0             FIL               3 0
022200950526     C                   Z-ADD     1             WDCM
022300010508     C                   eval      *in30 = *off
022400920306     C*
022500941207    1C     *IN30         DOWEQ     *OFF
022600941207     C     FIL           SETGT     FNARB000
022700941207     C                   READ      FNARB000                               30
022800920306     C*
022900941207    2C     *IN30         IFEQ      *OFF
023000000724     C                   CLEAR                   SAVLNP
023100920324     C                   Z-ADD     ARBLNA        FIL
023200980722     C     ARBLNA        CHAIN     AZORG01L                           35
023300020207     C  N35              MOVE      ORGde3        og143
023400020207     C  n35              movel     §ogntw        lnantw
023500020207     C   35              CLEAR                   lnantw
023600000809     C*
023700020207     C     lnantw        IFEQ      'EEX'
023800020207     C     lnantw        OREQ      'EUP'
023900020207     C     lnantw        OREQ      'FED'
024000020211     C     lnantw        OREQ      'DPD'
024100000809     C     COMPBE        IFGT      POSPBE
024200000809     C                   Z-ADD     COMPBE        WDACNF            8 0
024300000809     C                   ELSE
024400000809     C                   Z-ADD     POSPBE        WDACNF
024500000809     C                   ENDIF
024600000809     C                   ELSE
024700000809     C     COMDPB        IFGT      POSDPB
024800000809     C                   Z-ADD     COMDPB        WDACNF            8 0
024900000809     C                   ELSE
025000000809     C                   Z-ADD     POSDPB        WDACNF
025100000809     C                   ENDIF
025200000809     C                   ENDIF
025300920306     C*
025400950526     C     KARB2         SETLL     FNARB000
025500941207     C     FIL           READE     FNARB000                               31
025600920306     C*
025700941207    3C     *IN31         DOWEQ     *OFF
025800000724     C     ARBLNP        IFNE      SAVLNP
025900000724     C                   MOVE      ARBLNP        SAVLNP
026000020207     C                   MOVE      '   '         lnpntw
026100000724     C     ARBLNP        CHAIN     AZORG01L                           35
026200000724     C  N35              MOVEL     ORGDE3        OG143
026300020207     C  N35              MOVEL     §ogntw        lnpntw
026400000724     C                   ENDIF
026500020211     c* pulizia bolle estere COMPRESA LA DPD
026600020207     C     lnantw        IFEQ      'EEX'
026700020207     C     lnantw        OREQ      'EUP'
026800020207     C     lnantw        OREQ      'FED'
026900020211     C     lnantw        OREQ      'DPD'
027000000724     C* DATA PULIZIA BOLLE EXPORT (POSTE O NON POSTE)
027100000724     C                   Z-ADD     COMPBE        WDAPUL            8 0
027200020409     C                   Z-ADD     dtsexp_ymd    WDAprt            8 0
027300000724     C                   ELSE
027400000724     C* DATA PULIZIA BOLLE ITALIE E IMPORT (POSTE O NON POSTE)
027500020207     C     lnpntw        IFNE      'PPT'
027600000724     C                   Z-ADD     COMDPB        WDAPUL
027700020409     C                   Z-ADD     dtsita_ymd    WDAprt            8 0
027800000724     C                   ELSE
027900000724     C                   Z-ADD     POSDPB        WDAPUL
028000020409     C                   Z-ADD     dpsita_ymd    WDAprt            8 0
028100000724     C                   ENDIF
028200000724     C                   ENDIF
028300930628     C*
028400950526     C* CONSIDERO SOLO LE BOLLE
028500961204     C*
028600000809     C* SE DATA CONSEGNA  > DATA PULIZIA PIU' ALTA LEGGO ALTRA LINEA
028700000809    5C     ARBDCM        IFGT      WDACNF
028800920306     C                   SETON                                        31
028900941207   X5C                   ELSE
029000000809     C* ALTRIMENTI ELABORO SE DATA CONSEGNA <= DATA PULIZIA
029100000809     C* SE INVECE E' MAGGIORE IGNORO QUESTO RECORD
029200000809   5AC     ARBDCM        IFLE      WDAPUL
029300930507     C*
029400970623     C                   CLEAR                   WOK
029500000530     C*
029600000530     C* IMPOSTO DATA A SECONDA SE E' POSTE O NO
029700020207     C     lnpntw        IFNE      'PPT'
029800000530     C                   Z-ADD     COMARR        WDATA£            8 0
029900020409     C                   Z-ADD     dtsrsv_ymd    WDApr£            8 0
030000000530     C                   ELSE
030100000530     C                   Z-ADD     POSARR        WDATA£            8 0
030200020409     C                   Z-ADD     dpsrsv_ymd    WDApr£            8 0
030300000530     C                   ENDIF
030400980722     C**
030500980722     C* SE BOLLA CON RISERVA PULISCO CONSIDERANDO ANCHE L'ALTRA DATA
030600101007     c                   setoff                                       33
030700101007     c                   z-add     1             yy
030800101007     c                   dow       ris(YY)<>*blanks
030900101007     c                   eval      wcev=ris(YY)
031000101007     c
031100101007     C     KEVB4         SETLL     FNEVB04L
031200101007     c                   if        %equal(fnevb04l)
031300101007     c                   seton                                        33
031400101007     c                   leave
031500101007     c                   else
031600101007     c                   endif
031700101007     c                   add       1             yy
031800101007     c                   enddo
031900101007     c
032000980722     C     *IN33         IFEQ      *ON
032100000530     C     ARBDCM        ANDGE     WDATA£
032200020404     c* stampo motivazione
032300020409     c     arbdsp        iflt      wdapr£
032400020404     c                   exsr      ovrprt
032500020404     c     *ymd          move      arbdcm        data_EUR
032600020404     c                   move      data_EUR      data_Alfa
032700020404     c                   movel(P)  cbrise        p01mes
032800020404     c                   eval      p01mes = %trim(p01mes) + ' ' + data_Alfa
032900020404     c                   exsr      ried1
033000020404     c                   write     lr84d1
033100020409     c                   endif
033200980722     C                   MOVE      'N'           WOK               1
033300980722     C                   ENDIF
033400990126     C**
033500990126     C* SE BOLLA CON C.A. PULISCO PER DATA CHIUSURA C.A.
033600990126     C                   CLEAR                   WDCH
033700020530     C                   CLEAR                   WDCH_dcb
033800020409     C* IMPOSTO DATA stampa A SECONDA SE E' POSTE O NO
033900020409     C     lnpntw        IFNE      'PPT'
034000020409     C                   Z-ADD     dtscan_ymd    WDApr£
034100020409     C                   ELSE
034200020409     C                   Z-ADD     dpscan_ymd    WDApr£
034300020409     C                   ENDIF
034400020409     c*
034500040812      * Sostituisco la lettura del file FNDTC02 con
034600040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
034700040812      * legate alla spedizione che passo
034800040812      *
034900040812     c                   clear                   fidn12ds
035000040812     c                   eval      i12aas = arbaas
035100040812     c                   eval      i12lnp = arblnp
035200040812     c                   eval      i12nrs = arbnrs
035300040812     c                   eval      i12nsp = arbnsp
035400040812     c                   eval      i12fel = 'E'
035500040812     c                   eval      i12fan = 'E'
035600040812      *
035700040812     c                   call      'FIDN12R'
035800040812     c                   parm                    fidn12ds
035900040812      *
036000040812      * se non ci sono errori
036100040812    6c                   if        o12err = ' '
036200040812      * se numero di CA trovate maggiore di zero
036300040812    7c                   if        o12nca > 0
036400040812     c
036500040812     c                   z-add     *zeros        ii                2 0
036600040812    8c     1             do        o12nca        ii
036700040812     c                   movea     skkey(ii)     dskey
036800040812     C                   Z-ADD     skdca(ii)     dctdca
036900040812     C                   Z-ADD     skdch(ii)     dctdch
037000040812     C                   movea     skcch(ii)     dctcch
037100040812      * se CA aperta stampo,imposto il flag e vado a fine
037200040812    9c                   If        dctdch = 0
037300040812     c* stampo motivazione
037400080325   10c**   arbdsp        iflt      wdapr£
037500080325     c**                 exsr      ovrprt
037600080325     c**                 movel(P)  cbcana        p01mes
037700040812     c* chaino file delle fasi per reperire fase, data, e p.o.
037800080325     c**   kdcf          setgt     fndcf01l
037900080325     c**   kdcf          readpe    fndcf01l
038000080325   11c**                 if        not %eof(fndcf01l)
038100080325     c**   *ymd          move      dcfdfc        data_EUR
038200080325     c**                 move      data_EUR      data_Alfa
038300040812     c* chaino file anagrafica fasi per decodificare la fase
038400080325     c**                 clear                   fndfa000
038500080325     c**   dcffca        chain     fndfa01l
038600080325   12c**                 if        %found(fndfa01l)
038700080325     c**                 movel     dfades        wfades           40
038800080325   12c**                 endif
038900080325  x11c**                 else
039000080325     c**                 clear                   data_Alfa
039100080325     c**                 clear                   dcffca
039200080325     c**                 clear                   dcffev
039300080325   11c**                 endif
039400080325     c**                 move      dcffca        wcffca            3
039500080325     c**                 move      dcffev        wcffev            3
039600080325     c**                 eval      p01mes = %trim(p01mes) + ' ' + wcffca
039700080325     c**                           + ' ' + wfades
039800080325     c**                           + ' del ' + data_Alfa + ' p.o. ' + wcffev
039900080325     c**                 exsr      ried1
040000080325     c**                 write     lr84d1
040100080325   10c**                 endif
040200040812     C                   MOVE      'N'           WOK               1
040300040812     c                   leave
040400040812  x 9c                   else
040500040812      * se CA chiusa
040600040812   10C     DCTDCH        IFGT      WDCH
040700040812     C                   Z-ADD     DCTDCH        WDCH
040800040812   10C                   ENDIF
040900040812     c* memorizzo la data chiusura più alta se p.o. C.A. = lna bolla
041000040812   10c     dctdch        ifgt      wdch_dcb
041100040812     c     dctfil        andeq     arblna
041200040812     c                   z-add     dctdch        wdch_dcb
041300040812   10c                   endif
041400040812    9C                   ENDIF
041500040812     c
041600040812    8c                   enddo
041700040812      *
041800040812    7c                   endif
041900040812    6c                   endif
042000040812     C*
042100000530     C*
042200000530     C* IMPOSTO DATA A SECONDA SE E' POSTE O NO
042300020207     C     lnpntw        IFNE      'PPT'
042400000530     C                   Z-ADD     COMBCA        WDATA£            8 0
042500000530     C                   ELSE
042600000530     C                   Z-ADD     POSBCA        WDATA£            8 0
042700000530     C                   ENDIF
042800990126     C**
042900990126     C* SE C.A. TUTTE CHIUSE DEVONO ESSERE PASSATI I GIORNI DALLA DATA
043000990126     C*  CHIUSURA PIU' ALTA
043100990126     C     WOK           IFEQ      ' '
043200000530     C     WDCH          ANDGE     WDATA£
043300990126     C                   MOVE      'N'           WOK               1
043400990126     C                   ENDIF
043500990823     C* NON POSSO PULIRE BOLLA E C.A. SE NON L'HO ANCORA STAMPATA
043600990823     C* NELL'ELENCO C.A. DA ARCHIVIARE
043700990823    6C     WOK           IFEQ      ' '
043800020530     c     wdch_dcb      andgt     *zeros
043900990823     C                   CLEAR                   DSBS02
044000990824     C                   MOVEL     'C'           T02MOD
044100990823     C                   MOVEL     KNSIF         T02SIF
044200990823     C                   MOVEL     'DCB'         T02COD
044300990823     C                   MOVEL     ARBLNA        T02KE1
044400990823     C                   CALL      'TIBS02R'
044500990823     C                   PARM                    KPJBA
044600990823     C                   PARM                    DSBS02
044700990823    7C     T02ERR        IFEQ      *BLANKS
044800990823     C                   MOVEL     T02UNI        WDTACA            8 0
044900990823     C                   ELSE
045000990823     C                   Z-ADD     19990101      WDTACA
045100990823    7C                   ENDIF
045200020530    7C     WDCH_dcb      IFGT      WDTACA
045300020404     c* stampo motivazione
045400020409     c     arbdsp        iflt      wdapr£
045500020404     c                   exsr      ovrprt
045600020530     c     *ymd          move      wdch_dcb      data_EUR
045700020404     c                   move      data_EUR      data_Alfa
045800020404     c                   eval      p01mes = cbcann1 + data_alfa + cbcann2
045900020404     c                   exsr      ried1
046000020404     c                   write     lr84d1
046100020409     c                   endif
046200990823     C                   MOVE      'N'           WOK
046300990823    7C                   ENDIF
046400990823    6C                   ENDIF
046500990823     C*
046600970623   6AC     WOK           IFEQ      *BLANKS
046700050906      *
046800050906      * GIACENZA: se non trovata CANCELLO BOLLA
046900050906      *           se trovata ma è annullata o in fase 60 CANCELLO BOLLA
047000050906      *           negli altri casi non CANCELLO BOLLA
047100050906     C                   clear                   WGIACE            1
047200050906     C     KGCA          CHAIN     Tigcp21L
047300010508      *
047400050906     c                   SELECT
047500050906     c                   WHEN      not %found(Tigcp21L)
047600050906     c                   eval      *in33 = *off
047700050906     c                   WHEN      GCPATB <> *blanks or GCPFAS = 60
047800050906     c     kblp          Setll     Fnblp01l
047900050906     c                   eval      *in33 = *off
048000110210     c
048100110210     c                   If        Not %Equal(Fnblp01l)
048200050906     c                   Eval      wgiace = 'S'
048300050906     c                   EndIf
048400050906     c                   OTHER
048500020404     c* stampo motivazione
048600020409     c     arbdsp        iflt      wdaprt
048700020404     c                   exsr      ovrprt
048800020404     c                   movel(P)  cbgiac        p01mes
048900020530     c* prendo la decodifica fase
049000020530     C                   MOVEL     '2Y'          COD
049100050906     C                   MOVEL(P)  gcpfas        KEY
049200020530     C     ktab1         CHAIN     TABEL00F                           37
049300020530     C   37              MOVEL     *ALL'?'       decfas           45
049400020530     C  N37              MOVEL     TBLUNI        decfas
049500050906     c                   move      gcpfas        wcpfas            3
049600020530     c                   eval      p01mes = %trim(p01mes) + ' ' + wcpfas + '-'
049700020530     c                             + decfas
049800020404     c                   exsr      ried1
049900020404     c                   write     lr84d1
050000020409     c                   endif
050100050906      * Imposto flag x non pulire la bolla
050200050906     c                   MOVE      'N'           WOK
050300010508     c                   eval      *in33 = *on
050400050906     c                   ENDSL
050500020405     c*
050600020415     c  n33arbnmi        ifeq      *zeros
050700020405     c* stampo motivazione
050800020409     c     arbdsp        iflt      wdaprt
050900020405     c                   exsr      ovrprt
051000080515     c                   if        arbdmi>0
051100080515     c                   movel(P)  cbchoprn      p01mes
051200080515     c                   else
051300080515     c                   movel(P)  cbchoprd      p01mes
051400080515     c                   endif
051500080515     c
051600020405     c                   exsr      ried1
051700020405     c                   write     lr84d1
051800020409     c                   endif
051900020405     c                   else
052000020405     c     arbfip        ifeq      'I'
052100020405     c* stampo motivazione
052200020409     c     arbdsp        iflt      wdaprt
052300020405     c                   exsr      ovrprt
052400020405     c                   movel(P)  cbinc         p01mes
052500020405     c                   exsr      ried1
052600020405     c                   write     lr84d1
052700020409     c                   endif
052800020405     c                   endif
052900020405     c                   endif
053000920410     C*
053100920505     C* PULISCO SOLO SE NON E' DA TORNARE ALL'INCASSO
053200950110     C*   E SE HA GIA' IL MANDATO DI INTROITO
053300000927    6C  N33ARBFIP        IFNE      'I'
053400930308     C     ARBNMI        ANDNE     0
053500000927     C*
053600000927    7C     ARBICA        IFEQ      ' '
053700000927     C     ARBICA        OREQ      'R'
053800000927     C* VERIFICO SE PER CASO SI TRATTA DI UN FINTO RITORNO ALL'INCASSO
053900000927     C                   EXSR      CTRICA
054000000927    7C                   ENDIF
054100020405     c*
054200020405     c     arbica        ifeq      ' '
054300020405     c     arbica        oreq      'R'
054400020405     c* stampo motivazione
054500020409     c     arbdsp        iflt      wdaprt
054600020405     c                   exsr      ovrprt
054700020405     c                   movel(P)  cbinc         p01mes
054800020405     c                   exsr      ried1
054900020405     c                   write     lr84d1
055000020409     c                   endif
055100020405     c                   endif
055200000927     C*
055300000927   7AC     ARBICA        IFNE      ' '
055400000927     C     ARBICA        ANDNE     'R'
055500930308     C*
055600051115     C* AGGANCIO FiAR900F PER VEDERE SE C'E' IL C/ASSEGNO
055700051115     c                   setoff                                       34
055800051115     c     arbicc        ifeq      ' '
055900020405     c     arbicc        oreq      'R'
056000051115     C     KARB          SETLL     FiAR9000                               34
056100060123     c     *in34         ifeq      *on
056200060123     C     Karb          chain     tncsb03l
056300060123     c                   if        %found(tncsb03l)
056400060123     c* stato 9 --> Annullato; Data distinta > 0 --> c/a incassato
056500060123     c                   if        csbsta = 9 or csbddc > 0
056600060123     c                   setoff                                       34
056700060123     c                   endif
056800060123     c                   endif
056900060123     c                   endif
057000020405     c* stampo motivazione
057100080711    8c   34arbdsp        iflt      wdaprt
057200080711     c* Se si tratta di C/Ass omesso, verifico se devo davvero stampare
057300080711     c*  in base alla data di rigenerazione
057400080711     c                   clear                   w008a
057500080711     c                   clear                   w0080
057600080711     c                   eval      ktrc='8'
057700080711     c     kar4          chain(n)  fiar401l
057800080711    9c                   if        %found(fiar401l)
057900080711     c                   eval      Indx=%scan('RIGENERATA':ar4not)
058000080711   10c                   if        Indx>0
058100080711     c                   eval      ktrc='9'
058200080711     c     kar4          chain(n)  fiar401l
058300080711   11c                   if        %found(fiar401l)
058400080711     c                   eval      Indx=%scan('OMESSO':ar4not)
058500080711   12c                   if        Indx>0
058600080711     c                   eval      w008a=%subst(ar4not:26:8)
058700080711   13c                   if        w008a>*zeros
058800080711     c                   movel     w008a         w0080
058900080711   13c                   endif
059000080711   12c                   endif
059100080711   11c                   endif
059200080711   10c                   endif
059300080711    9c                   endif
059400080711     c
059500080711    9c                   if        w0080=0 or w0080<wdapul
059600020405     c                   exsr      ovrprt
059700080711     c                   if        w0080>0
059800020405     c                   movel(P)  cbcass        p01mes
059900080711     c                   eval      p01mes=%trim(p01mes)+' rigenerata il '+
060000080711     c                             %editw(w0080:'    /  /  ')
060100080711     c                   endif
060200080711     c
060300020405     c                   exsr      ried1
060400020405     c                   write     lr84d1
060500080711    9c                   endif
060600080711    8c                   endif
060700080711   7ac                   endif
060800941207     C*
060900950110     C* PULISCO SOLO SE NON C'E' IL C/ASSEGNO OPPURE SE C'E' MA E'
061000950110     C*   GIA' STATO INCASSATO
061100941212    7C     *IN34         IFEQ      *ON
061200930121     C     ARBICC        ANDNE     ' '
061300950119     C     ARBICC        ANDNE     'R'
061400941212     C     *IN34         OREQ      *OFF
061500950110     C*
061600950110     C* PULISCO SOLO SE NON TROVO RECORD NEI FILE TASSAZIONE PADRONCINI
061700020405     c                   clear                   p01mes
061800011121     C     KARB          SETLL     fiftd000                               36
061900020405     c     *in36         ifeq      *on
062000020405     c                   movel(P)  cbcpdr        p01mes
062100020405     c                   endif
062200011121     C  N36KARB          SETLL     fifsd000                               36
062300080711     c* visto che normalmente rimangono i record consegna e quindi scritti
062400080711     c*  in FTD per data consegna enon per data spedizione, stampo solo se
062500080711     c*  la data consegna < data stampa
062600020405     c     *in36         ifeq      *on
062700020409     c     arbdsp        andlt     wdaprt
062800080711     c     arbdcm        andeq     0
062900080711     c     *in36         oreq      *on
063000080711     c     arbdcm        andlt     wdaprt
063100080711     c     arbdcm        andgt     0
063200080711     c
063300020405     c     p01mes        ifeq      *blanks
063400020405     c                   movel(P)  cbcpdr        p01mes
063500020405     c                   eval      p01mes = %trim(p01mes) + ' di simulazione'
063600020405     c                   endif
063700020405     c                   exsr      ovrprt
063800020405     c                   exsr      ried1
063900020405     c                   write     lr84d1
064000020405     c                   endif
064100040831     c*
064200040831     c* Posso pulire solo le bolle:
064300040831     C* 1) le cui date di consegna sono state trasmesse in partenza e sede
064400040831     c*     arbft1 = 'T'
064500040831     c* 2) autogenerate poste con data ricezione >= data mandato introito
064600040831     c*    (sono quelle ex vedi pacco che ricevo tempo dopo)
064700040831     c*    arbft1 = 'R' e
064800040831     c* in caso contrario stampo segnalazione
064900040831    8c     *IN36         IFEQ      *OFF
065000040831     c*
065100040831     c     arbft1        ifne      'T'
065200040831     c     arbdt1        andlt     arbdmi
065300040831     c                   eval      wok = 'N'
065400040831     c* stampo motivazione
065500040831     c     arbdsp        iflt      wdaprt
065600040831     c                   exsr      ovrprt
065700040831     c                   movel(P)  cbdt1         p01mes
065800040831     c                   exsr      ried1
065900040831     c                   write     lr84d1
066000040831     c                   endif
066100040831     c                   endif
066200950110     C*
066300991104     C****** CANCELLAZIONE C.A. *******
066400040831   8Ac     wok           ifeq      *blanks
066500991104     C* SE BOLLA LOCALE NON CANCELLO LA C.A. SE PRESENTE BOLLA IN BLP
066600020611     c                   eval      *in96 = *off
066700020611     c     karb          setll     fnblp01l                               96
066800991104     C**
066900020611     c                   if        not *in96
067000991104     C**
067100991104     C                   MOVE      *BLANKS       WERR              1
067200040812      * Sostituisco la lettura del file FNDTC02 con
067300040813      * richiamo del programma FIDN12R passando la DS richiedendo le CA
067400040812      * legate alla spedizione che passo
067500040812      *
067600040812     c                   clear                   fidn12ds
067700040812     c                   eval      i12aas = arbaas
067800040812     c                   eval      i12lnp = arblnp
067900040812     c                   eval      i12nrs = arbnrs
068000040812     c                   eval      i12nsp = arbnsp
068100040812     c                   eval      i12fel = 'E'
068200040812      *
068300040812     c                   call      'FIDN12R'
068400040812     c                   parm                    fidn12ds
068500040812      *
068600040812      * se non ci sono errori
068700040812     c                   if        o12err = ' '
068800040812      * se numero di CA trovate maggiore di zero
068900040812     c                   if        o12nca > 0
069000040812     c
069100040812     c                   z-add     *zeros        ii                2 0
069200040812     c     1             do        o12nca        ii
069300040812     c                   movea     skkey(ii)     dskey
069400040812     C                   Z-ADD     skdca(ii)     dctdca
069500040813     C                   CLEAR                   DSDN11
069600040813     C                   MOVEL     'D'           I11MOD
069700040813     C                   Z-ADD     DCTAAC        I11AAC
069800040813     C                   Z-ADD     DCTFIL        I11FIL
069900040813     C                   Z-ADD     DCTNCA        I11NCA
070000040813     C                   CALL      'FIDN11R'
070100040813     C                   PARM                    KPJBA
070200040813     C                   PARM                    DSDN11
070300040813   11C     O11ERR        IFEQ      'E'
070400040813     C                   MOVEL     '1'           WERR
070500040813   11C                   END
070600040813      *
070700040813     c                   enddo
070800040813      *
070900040813     c                   endif
071000040813     c                   endif
071100991104     C**
071200991104     C* SE NON SONO RIUSCITA A CANCELLARE TUTTE LE C.A. NON POSSO
071300991104     C* CANCELLARE LA BOLLA
071400991104   10C     WERR          IFNE      *BLANKS
071500020405     c* stampo motivazione
071600020405     c                   exsr      ovrprt
071700020405     c                   movel(P)  cbercan       p01mes
071800020405     c     *ymd          move      dctdca        data_EUR
071900020405     c                   move      data_EUR      data_Alfa
072000020405     c                   eval      p01mes = %trim(p01mes) + %editc(dctfil:'Z')
072100020405     c                             + %editc(dctnca:'Z') + 'del'  + data_Alfa
072200020405     c                   exsr      ried1
072300020405     c                   write     lr84d1
072400991104     C                   MOVE      'N'           WOK
072500991104   10C                   ENDIF
072600991104    9C                   ENDIF
072700040423      *
072800040423      * RECLAMI : se non trovata CANCELLO BOLLA
072900040423      *
073000040423     C*
073100040423    1C     WOK           IFEQ      ' '
073200040423     c                   movel     arbaas        W_aas
073300040423     c                   movel     arblnp        W_lnp
073400040423     c                   movel     arbnrs        W_nrs
073500040423     c                   movel     arbnsp        W_nsp
073600040423      *
073700040423     c                   movel     wspe          ktgogg
073800040423      *
073900040423     c     ktgd          setll     fitgd02l
074000040423     c                   do        *hival
074100040423     c     ktgd          reade     fitgd02l
074200040423
074300040423     c                   if        %eof(fitgd02l)
074400040423     c                   leave
074500040423     c                   endif
074600040423      * se annullato leggo il successivo
074700040423     c                   If        TGDatb <> ' '
074800040423     c                   iter
074900040423     c                   endif
075000040423      * verifico se reclamo ancora aperto
075100040423     c                   If        TGDdcd = 0  or
075200040423      * se reclamo chiuso verifico la data chiusura con la dta di pulizia
075300040423     c                             TGDdcd > comrec
075400040423     c                   eval      wok = 'N'
075500080515     c
075600080515     c* stampo motivazione  solo se aperto
075700080515     c                   If        TGDdcd = 0
075800040423     c                   exsr      ovrprt
075900040423     c                   movel(P)  cbcrec        p01mes
076000040423     c                   exsr      ried1
076100040423     c                   write     lr84d1
076200040423     c                   leave
076300080515     c                   endif
076400080515     c
076500040423     c                   endif
076600040423
076700040423     c                   enddo
076800040423
076900040423    1c                   endif
077000991104     C****** PULIZIA ALTRI FILE ********+
077100991104    9C     WOK           IFEQ      ' '
077200020411     C                   EXSR      PULIZ
077300991104    9C                   ENDIF
077400991104     C**
077500040831   8Ac                   endif
077600991104    8C                   ENDIF
077700991104     C**
077800941207    7C                   ENDIF
077900000927   7AC                   ENDIF
078000941207    6C                   ENDIF
078100970623   6AC                   ENDIF
078200920306     C*
078300000809   5AC                   ENDIF
078400941207     C     FIL           READE     FNARB000                               31
078500941207    5C                   ENDIF
078600950118     C*
078700941207     C*
078800941207    3C                   ENDDO
078900941207    2C                   ENDIF
079000941207    1C                   ENDDO
079100020404     c*
079200020409     C                   Z-ADD     0             FIL               3 0
079300020409     C                   Z-ADD     0             WDCM
079400020409     C                   eval      *in30 = *off
079500020409     C*
079600020409    1C     *IN30         DOWEQ     *OFF
079700020409     C     FIL           SETGT     FNARB000
079800020409     C                   READ      FNARB000                               30
079900020409     C*
080000020409    2C     *IN30         IFEQ      *OFF
080100020409     C                   Z-ADD     ARBLNA        FIL
080200020409     C*
080300020409     C     KARB2         SETLL     FNARB000
080400020409     C     KARB2         READE     FNARB000                               31
080500020409     C*
080600020409    3C     *IN31         DOWEQ     *OFF
080700080515    4c     arbdsp        iflt      dtsncn_ymd
080800020411     c* Guardo se presente c.a.
080900040813      * Sostituisco la lettura del file FNDTC02 con
081000040813      * richiamo del programma FIDN12R passando la DS richiedendo le CA
081100040813      * legate alla spedizione che passo
081200040813      *
081300040813     c                   clear                   fidn12ds
081400040813     c                   eval      i12aas = arbaas
081500040813     c                   eval      i12lnp = arblnp
081600040813     c                   eval      i12nrs = arbnrs
081700040813     c                   eval      i12nsp = arbnsp
081800040813     c                   eval      i12fel = 'E'
081900040813      *
082000040813     c                   call      'FIDN12R'
082100040813     c                   parm                    fidn12ds
082200040813      *
082300040813      * se non ci sono errori  e num.ca magg. di zero accendo il 32
082400080515     c                   clear                   wchiusaCA         1
082401120217     C                   CLEAR                   WDCH_dcb
082500080515    5c                   if        o12err = ' ' AND o12nca > 0
082600040813     c                   seton                                        32
082700080515     c                   eval      WchiusaCA='S'
082800080515     c
082900080515     c* Stampo solo se tutte le c.a. sono chiuse
083000080515     c                   z-add     *zeros        ii                2 0
083100080515    6c     1             do        o12nca        ii
083200080515     c                   movea     skkey(ii)     dskey
083300080515     C                   Z-ADD     skdca(ii)     dctdca
083400080515     C                   Z-ADD     skdch(ii)     dctdch
083500080515     C                   movea     skcch(ii)     dctcch
083600080515     c
083700080515      * se CA chiusa stampo,imposto il flag e vado a fine
083800080515    7c                   If        dctdch = 0
083900080515     c                   clear                   wchiusaCA
083901120217     c                   else
083902120217     c* memorizzo la data chiusura più alta se p.o. C.A. = lna bolla
083903120217    8c     dctdch        ifgt      wdch_dcb
083904120217     c     dctfil        andeq     arblna
083905120217     c                   z-add     dctdch        wdch_dcb
083906120217    8c                   endif
084000080515    7c                   endif
084100080515    6c                   enddo
084200080515    5c                   endif
084300080515     c
084400040813      *
084500120220    5c** 32lnpntw        ifne      'PPT'
084600120220     C**                 Z-ADD     dtscan_ymd    WDAprt
084700120220     c**                 else
084800120220     C**                 Z-ADD     dpscan_ymd    WDAprt
084900120220    5c**                 endif
085000080515    5C***  *in32         ifeq      *off
085100080515     c***  arbdsp        orlt      wdaprt
085200080515    c
085300080515     c                   if        (*in32 and wchiusaCA='S' and
085400120222    5c                             wdch_dcb<dtsgdc_ymd and arbdsp<dtscan_ymd)
085401120217    5c                             or not *in32
085500080515     c
085600020409     c                   exsr      ovrprt
085700020411     c  n32              movel(P)  cbncon        p01mes
085800020411     c   32              movel(P)  cbnconca      p01mes
085900020409     c                   exsr      ried1
086000020409     c                   write     lr84d1
086100080515    5c                   endif
086200080515    4c                   endif
086300020409     C*
086400020409     C     karb2         READE     FNARB000                               31
086500020409     C*
086600020409    3C                   ENDDO
086700020409    2C                   ENDIF
086800020409    1C                   ENDDO
086900020409     c*
087000020404     c     wopn          ifeq      '1'
087100020404     c                   write     lr84z1
087200020404     c                   endif
087300990823     C*
087400990823     C* CHIUDO CON LR
087500990823     C                   CLEAR                   DSBS02
087600990823     C                   MOVEL     'C'           T02TLA
087700990823     C                   CALL      'TIBS02R'
087800990823     C                   PARM                    KPJBA
087900990823     C                   PARM                    DSBS02
088000040813     C*
088100040813     C                   MOVEL     'C'           I12TLA
088200040813     C                   CALL      'FIDN12R'
088300040813     C                   PARM                    FIDN12DS
088400920205     C*
088500950516     C                   MOVEL     WSAJBU        KPJBU
088600920205     C                   SETON                                        LR
088700930628     C*---------------------------------------------------------------*
088800930628     C*                    PULIZ                                      *
088900930628     C* ROUTINE DI PULIZIA FILE BOLLE E FILE COLLEGATI                *
089000930628     C*---------------------------------------------------------------*
089100920324     C     PULIZ         BEGSR
089200060502     c* Chaino file fisico: se riesco pulisco, altrimenti leggo altra bolla
089300060502     c     arbnrr        chain(E)  fnarb00f
089400060502     c                   if        not %error and %found(fnarb00f)
089500941207     C*
089600051115     c                   if        not *in96
089700060220     C*
089800060220     C****  FIAR400F ---> ESTENSIONE DESCRIZIONI  ****
089900060220     C     KARB          SETLL     FIAR4000                               32
090000060220    1C     *IN32         IFEQ      *ON
090100060220     C     KARB          READE     FIAR4000                               32
090200060220     C*
090300060220    2C     *IN32         DOWEQ     *OFF
090400060220     C                   DELETE    FIAR4000
090500060220     C     KARB          READE     FIAR4000                               32
090600060220    2C                   ENDDO
090700060220    1C                   ENDIF
090800060220     C****  FiAR900F ---> DATI C/ASSEGNO  ****
090900051115     C     KARB          CHAIN     FiAR9000                           32
091000051115     C  N32              DELETE    FiAR9000
091100051115     c                   endif
091200941207     C*
091300930628     C*
091400941207     C****  FNART00F ---> DETTAGLIO COLLI  ****
091500941207     C     KARB          SETLL     FNART000
091600941207     C     KARB          READE     FNART000                               32
091700920324     C*
091800941207    1C     *IN32         DOWEQ     *OFF
091900961125     C* CANCELLO GLI EVENTI SEGNACOLLI SENZA NUMERO SPEDIZIONE
092000961125     C                   SETOFF                                       36
092100961125     C     KART          SETLL     FNEVS02L
092200961125    2C     *IN36         DOWEQ     *OFF
092300961125     C     KART          READE     FNEVS02L                               36
092400961125     C  N36              DELETE    FNEVS002
092500961125    2C                   ENDDO
092600051012     c
092700980331     C* CANCELLO L'ESTENSIONE DETTAGLIO SEGNACOLLO
092800051012     c* solo se non più presente la bolla in fnblp
092900051012     c                   if        not *in96
093000051012     c
093100980331     C                   SETOFF                                       36
093200051012     C     KART          SETLL     FIARS01L
093300980331    2C     *IN36         DOWEQ     *OFF
093400051012     C     KART          READE     FIARS01L                               36
093500051012     C  N36              DELETE    FIARS000
093600980331    2C                   ENDDO
093700051012     c
093800051012     c                   endif
093900961125     C*
094000941207     C                   DELETE    FNART000
094100941207     C     KARB          READE     FNART000                               32
094200941207    1C                   ENDDO
094300930628     C*
094400961125     C****  FNEVS00F ---> EVENTI SEGNACOLLI  ****
094500961125     C* CANCELLO GLI EVENTI SEGNCOLLI CON NUMERO SPEDIZIONE
094600961125     C     KARB          SETLL     FNEVS01L
094700961125     C     KARB          READE     FNEVS01L                               32
094800961125     C*
094900961125    1C     *IN32         DOWEQ     *OFF
095000961125     C                   DELETE    FNEVS000
095100961125     C     KARB          READE     FNEVS01L                               32
095200961125    1C                   ENDDO
095300991104     C*
095400991104     C****  FIARI00F ---> FILE DEGLI INCASSI  ****
095500991104     C     KARB          SETLL     FIARI000                               32
095600991104    1C     *IN32         IFEQ      *ON
095700991104     C     KARB          READE     FIARI000                               32
095800991104     C*
095900991104    2C     *IN32         DOWEQ     *OFF
096000991104     C                   DELETE    FIARI000
096100991104     C     KARB          READE     FIARI000                               32
096200991104    2C                   ENDDO
096300991104    1C                   ENDIF
096400061107     C*
096500061107     C****  FIARG00F ---> FILE GEOREFER/GIRI DI CONSEGNA
096600061107     C     KARB          SETLL     FIARG000                               32
096700061107    1C     *IN32         IFEQ      *ON
096800061107     C     KARB          READE     FIARG000                               32
096900061107     C*
097000061107    2C     *IN32         DOWEQ     *OFF
097100061107     C                   DELETE    FIARG000
097200061107     C     KARB          READE     FIARG000                               32
097300061107    2C                   ENDDO
097400061107    1C                   ENDIF
097500070503     C*
097600070503     C****  FIARP00F ---> FILE proposte DCr che dovrebbe essere vuoto
097700070503     C     KARB          SETLL     FIARP000                               32
097800070503    1C     *IN32         IFEQ      *ON
097900070503     C     KARB          READE     FIARP000                               32
098000070503     C*
098100070503    2C     *IN32         DOWEQ     *OFF
098200070503     C                   DELETE    FIARP000
098300070503     C     KARB          READE     FIARP000                               32
098400070503    2C                   ENDDO
098500070503    1C                   ENDIF
098600070503     C*
098700070503     C****  FIARN00F ---> FILE note LDV
098800070503     C     KARB          SETLL     FIARN000                               32
098900070503    1C     *IN32         IFEQ      *ON
099000070503     C     KARB          READE     FIARN000                               32
099100070503     C*
099200070503    2C     *IN32         DOWEQ     *OFF
099300070503     C                   DELETE    FIARN000
099400070503     C     KARB          READE     FIARN000                               32
099500070503    2C                   ENDDO
099600070503    1C                   ENDIF
099700961125     C*
099800941207     C*****
099900991104     C***  VARIAZIONI BOLLA  E TASSAZIONE
100000941207     C*****
100100930602     C*
100200971002     C* NON PULISCO I LOCALI E I SUOI EVENTI SE LA BOLLA PARTENZA
100300971002     C*  C'E' ANCORA
100400030108     C* (Ho cancellato la lokup su l1 e la setll su fnblp perchè
100500030108     C* le stesse specifiche sono già state eseguite per gestire
100600030108     C* la cancellazione di C.A. di bolla locale - (vedi sopra))
100700020611     c                   if        not *in96
100800991104     C**
100900991104     C****  FIAR600F ---> TASSAZIONE BOLLA  ****
101000991104     C     KARB          CHAIN     FIAR6000                           32
101100991104    2C     *IN32         DOWEQ     *OFF
101200991104     C*
101300991104     C****  FIAR700F ---> ESTENSIONE TASSAZIONE BOLLA  ****
101400991104     C     KARB7         SETLL     FIAR7000                               35
101500991104    3C     *IN35         IFEQ      *ON
101600991104     C     KARB7         READE     FIAR7000                               35
101700991104     C*
101800991104    4C     *IN35         DOWEQ     *OFF
101900991104     C                   DELETE    FIAR7000
102000991104     C     KARB7         READE     FIAR7000                               35
102100991104    4C                   ENDDO
102200991104    3C                   ENDIF
102300991104     C*
102400991104     C                   DELETE    FIAR6000
102500991104     C**
102600991104     C     KARB          READE     FIAR6000                               32
102700991104    2C                   ENDDO
102800930602     C*
102900920410     C                   SETOFF                                       010203
103000941207     C                   SETOFF                                       040506
103100941207     C                   SETOFF                                       07
103200991104     C     KARB          SETLL     FIARBF1C                               32
103300941207    2C     *IN32         IFEQ      *ON
103400991104     C     KARB          READE     FIARBF1C                               32
103500920324     C*
103600941207    3C     *IN32         DOWEQ     *OFF
103700941207     C   01              DELETE    FNARBD00
103800941207     C   02              DELETE    FNARBK00
103900950630     C*
104000950630    4C     *IN03         IFEQ      *ON
104100950630     C* SE C'E' LA 3° VARIA CONTROLLO SE CE NE SONO ANCHE IN FNARBU0F
104200950630    5C     ARBSV3        IFNE      ' '
104300950630     C*
104400991104     C     KARB          SETLL     FIARBU00                               35
104500950630    6C     *IN35         IFEQ      *ON
104600991104     C     KARB          READE     FIARBU00                               35
104700950630     C*
104800950630    7C     *IN35         DOWEQ     *OFF
104900991104     C                   DELETE    FIARBU00
105000991104     C     KARB          READE     FIARBU00                               35
105100950630    7C                   ENDDO
105200950630    6C                   ENDIF
105300950630    5C                   ENDIF
105400950630     C*
105500991104     C                   DELETE    FIARBT00
105600950630    4C                   ENDIF
105700950630     C*
105800991104     C   04              DELETE    FNARBP00
105900941207     C   05              DELETE    FNARBG00
106000941207     C   06              DELETE    FNARBM00
106100941207     C   07              DELETE    FNARBV00
106200920410     C                   SETOFF                                       010203
106300941207     C                   SETOFF                                       040506
106400941207     C                   SETOFF                                       07
106500991104     C     KARB          READE     FIARBF1C                               32
106600941207    3C                   ENDDO
106700941207    2C                   ENDIF
106800030108     C*
106900030108     C****  FIAR500F ---> PULIZIA ESTENSIONI TESTATA BOLLA  ****
107000030108     C     KARB          setll     FIAR5000                               32
107100030108    2C                   if        *IN32
107200030108     C     KARB          reade     FIAR5000                               32
107300030108    3C                   dow       not *IN32
107400030108     C                   DELETE    FIAR5000
107500030108     C     KARB          reade     FIAR5000                               32
107600030108    3C                   enddo
107700030108    2C                   endif
107800040604     c*
107900040604     C****  FIAR800F ---> PULIZIA ESTENSIONI TESTATA BOLLA  ****
108000040604     C     KARB          setll     FIAR8000                               32
108100040604    2C                   if        *IN32
108200040604     C     KARB          reade     FIAR8000                               32
108300040604    3C                   dow       not *IN32
108400040604     C                   DELETE    FIAR8000
108500040604     C     KARB          reade     FIAR8000                               32
108600040604    3C                   enddo
108700040604    2C                   endif
108800930628     C*
108900990806     C****  FNEVB00F ---> PULIZIA EVENTI  ****
109000991104     C     KARB          SETLL     FNEVB01L                               32
109100941207    2C     *IN32         IFEQ      *ON
109200040830     c* se bolla presente in titas non posso pulire
109300040830     c     karb          setll     titas30c
109400040831     c                   if        not %equal(titas30c)
109500991104     C     KARB          READE     FNEVB01L                               32
109600941207    3C     *IN32         DOWEQ     *OFF
109700990806     C                   DELETE    FNEVB01L
109800991104     C     KARB          READE     FNEVB01L                               32
109900941207    3C                   ENDDO
110000040831     c                   endif
110100941207    2C                   ENDIF
110200930628     C*
110300941207    1C                   ENDIF
110400920324     C**
110500060502     C* CANCELLO LA BOLLA dal file fisico fnarb00F
110600920324     C**
110700060502     C                   DELETE    FNARBfis
110800010508     C**
110900050906      * Pulizia Giacenza: Se wgiace = 'S' vuol dire che ho giacenza chiusa e non c'è
111000050906      *                   la bolla originale su FNBLP in questo caso posso pulire la giacenza
111100050906     c                   If        WGIACE = 'S'
111200050404     c                   z-add     arbaas        iz5aas
111300050404     c                   z-add     arblnp        iz5lnp
111400050404     c                   z-add     arbnrs        iz5nrs
111500050404     c                   z-add     arbnsp        iz5nsp
111600050404     c                   call      'FNLGZ5C'
111700050404     c                   PARM                    FNLGZ5DS
111800050404---  C                   ENDIF
111900060502     c
112000060502---  C                   ENDIF
112100010508      *
112200920306     C                   ENDSR
112300000927     C*---------------------------------------------------------------*
112400000927     C* CONTROLLO SE SI TRATTA DI VERO RITRONO ALL'INCASSO
112500000927     C*---------------------------------------------------------------*
112600000927     C     CTRICA        BEGSR
112700000927     C* VERIFICO SE BOLLA SINGOLA O DOPPIA
112800000927     C     ARBCBO        LOOKUP    CBO                                    32
112900000927     C**
113000000927     C* BOLLA SINGOLA IN ASSEGNATO
113100000927    1C     *IN32         IFEQ      *OFF
113200000927     C                   MOVE      ARBKSC        W0040             4 0
113300000927    2C     W0040         IFNE      9999
113400000927     C* CHAIN SU FIAR6 PER VEDERE SE C'E' FATTURA IMMEDIATA
113500000927     C                   MOVEL     '1'           AR6TRC
113600000927     C     KARB7         CHAIN(N)  FIAR601L                           34
113700000927     C**
113800000927     C* SE NON C'E' O C'E' MA SENZA FATT IMMEDIATA, E' ERRATO
113900000927    3C     *IN34         IFEQ      *ON
114000000927     C     AR6DFT        OREQ      0
114100000927     C     AR6NFT        ANDEQ     0
114200000927     C                   MOVEL     'Y'           ARBICA
114300000927    3C                   ENDIF
114400000927    2C                   ENDIF
114500000927     C**
114600000927   X1C                   ELSE
114700000927     C* CHAIN SU FIAR6 PER VEDERE SE C'E' FATTURA IMMEDIATA
114800000927     C                   MOVEL     '2'           AR6TRC
114900000927     C     KARB7         CHAIN(N)  FIAR601L                           34
115000000927     C**
115100000927     C* SE CE' ED E' S.F. E' FINOTO
115200000927    2C     *IN34         IFEQ      *OFF
115300000927     C     AR6DFT        ANDEQ     0
115400000927     C     AR6NFT        ANDEQ     0
115500000927     C                   MOVE      AR6KSC        W0040
115600000927     C**
115700000927    3C     W0040         IFNE      9999
115800000927     C                   MOVEL     'Y'           ARBICA
115900000927    3C                   ENDIF
116000000927    2C                   ENDIF
116100000927    1C                   ENDIF
116200000927     C                   ENDSR
116300930628     C*---------------------------------------------------------------*
116400020611     C* ROUTINE DI CARICAMENTO TABELLE                                *
116500930628     C*---------------------------------------------------------------*
116600930507     C     CARTAB        BEGSR
116700000927     C*
116800000927     C* CARICO I TIPI BOLLA DOPPI
116900000927     C                   CLEAR                   YY
117000000927     C                   MOVEL     '3A'          COD
117100000927     C     KTAB          SETLL     TABEL
117200000927     C     KTAB          READE     TABEL                                  30
117300000927     C     *IN30         DOWEQ     *OFF
117400000927     C     TBLFLG        IFEQ      ' '
117500000927     C                   MOVEL     TBLUNI        DS3A
117600000927     C     §3ATB2        IFNE      *BLANKS
117700000927     C                   ADD       1             YY                3 0
117800000927     C                   MOVEL     TBLKEY        CBO(YY)
117900000927     C                   ENDIF
118000000927     C                   ENDIF
118100000927     C     KTAB          READE     TABEL                                  30
118200000927     C                   ENDDO
118300930628     C*
118400101007     c* Carico gli eventi di riserva dalla tabella 2A
118500101007     C                   CLEAR                   YY
118600101007     C                   MOVEL     '2A'          COD
118700101007     C     KTAB          SETLL     TABEL
118800101007     C     KTAB          READE     TABEL                                  30
118900101007     C     *IN30         DOWEQ     *OFF
119000101007     C     TBLFLG        IFEQ      ' '
119100101007     C                   MOVEL     TBLUNI        DS2A
119200101007     C     §2Atip        IFeq      'R'
119300101007     C                   ADD       1             YY                3 0
119400101007     C                   MOVEL     TBLKEY        ris(YY)
119500101007     C                   ENDIF
119600101007     C                   ENDIF
119700101007     C     KTAB          READE     TABEL                                  30
119800101007     C                   ENDDO
119900930507     C                   ENDSR
120000020404     C*---------------------------------------------------------------*
120100020404     C* OVRPRT - Operazioni su file printer                           *
120200020404     C*---------------------------------------------------------------*
120300020404     C     ovrprt        BEGSR
120400020404     c* override su file printer + apertura
120500020404     c     wopn          ifeq      *blanks
120600020409     C                   MOVEl     cmd(1)        COMMAN          100
120700020409     c                   eval      %subst(cmd(2):6:4) = whld
120800020409     c                   eval      %subst(cmd(2):17:4) = wsav
120900020409     c                   eval      comman = %trim(comman) + '('
121000020409     c                             + %trim(§5alib) + '/'
121100020409     c                             + %trim(§5aotq) + ') ' + %trim(cmd(2))
121200020409     C                   Z-ADD     100           LUNGH
121300020404     C                   CALL      'QCMDEXC'
121400020404     C                   PARM                    COMMAN
121500020404     C                   PARM                    LUNGH            15 5
121600020404     c                   open      fnlr84p
121700020404     c* valorizzazione campi di stampa testata
121800020404     c* data bolle lnp italia
121900020404     c     *ymd          move      comdpb        data_dmy
122000020404     c                   move      data_dmy      dtpita
122100020404     c* data bolle import
122200020404     c     *ymd          move      compbe        data_dmy
122300020404     c                   move      data_dmy      dtpexp
122400020404     c* data bolle con riserva
122500020404     c     *ymd          move      comarr        data_dmy
122600020404     c                   move      data_dmy      dtprsv
122700020404     c* data bolle con c.a.
122800020404     c     *ymd          move      combca        data_dmy
122900020404     c                   move      data_dmy      dtpcan
123000020404     c* Bolle poste: data bolle lnp italia
123100120220     c***  *ymd          move      posdpb        data_dmy
123200120220     c***                move      data_dmy      dppita
123300020404     c*              data bolle import
123400120220     c***  *ymd          move      pospbe        data_dmy
123500120220     c***                move      data_dmy      dppexp
123600020404     c*              data bolle con riserva
123700120220     c***  *ymd          move      posarr        data_dmy
123800120220     c***                move      data_dmy      dpprsv
123900020404     c*              data bolle con c.a.
124000120220     c**   *ymd          move      posbca        data_dmy
124100120220     c***                move      data_dmy      dppcan
124200020404     c*
124300020404     c                   write     lr84t1
124400020404     c                   movel     '1'           wopn
124500020404     c                   endif
124600020404     c* stampa intestazione a rottura
124700020404     c   99              write     lr84t1
124800020404     c                   setoff                                       99
124900020404     c                   endsr
125000020404     C*---------------------------------------------------------------*
125100020404     C* RIED1  - Valorizzazione campi di stampa dettaglio             *
125200020404     C*---------------------------------------------------------------*
125300020404     C     RIED1         BEGSR
125400020404     c                   z-add     arblnp        p1clnp
125500020404     c                   z-add     arblna        p1clna
125600020404     c                   z-add     arbnrs        p1cnrs
125700020404     c                   z-add     arbnsp        p1cnsp
125800080515     c                   MOVEL     arbCBO        p1cCBO
125900080515     c                   MOVEL     arbCCA        p1cCCA
126000020404     c     *ymd          move      arbdsp        data_EUR
126100020404     c                   move      data_EUR      p1cdsp
126200080519     c                   if        arbdcm>0
126300080515     c     *ymd          move      arbDCM        data_EUR
126400080515     c                   move      data_EUR      p1cDCM
126500080519     c                   else
126600080519     c                   clear                   p1cdcm
126700080519     c                   endif
126800020404     c                   endsr
126900010508     C*---------------------------------------------------------------*
127000010508     C* ROUTINE INIZIALE
127100010508     C*---------------------------------------------------------------*
127200010508     C     *inzsr        BEGSR
127300010508      *
127400010508     C     *ENTRY        PLIST
127500010508     C                   PARM                    KPJBA
127600010508     C                   MOVEL     KPJBU         PARAM
127700010508     C                   Z-ADD     1             CODUT
127800010508     C                   CALL      'X§PARUT'
127900010508     C                   PARM                    UT§DSE
128000010508     C                   MOVEL     REC80         CNCR80
128100020404     c                   movel     ragut         rsut
128200020404     C*
128300020404     C                   TIME                    wtime            14 0
128400020404     C                   MOVE      WTIME         WDATE             8 0
128500020409     c     *eur          move      wdate         wdate_inv
128501120216     c     *eur          move      wdate         wdateu
128600010508     C*---------------------------------------------------------------*
128700010508     C     KTAB          KLIST
128800010508     C                   KFLD                    CODUT
128900010508     C                   KFLD                    COD
129000020409     C     KTAB1         KLIST
129100020409     C                   KFLD                    CODUT
129200020409     C                   KFLD                    COD
129300020409     c                   kfld                    key
129400010508     C     KARB          KLIST
129500010508     C                   KFLD                    ARBAAS
129600010508     C                   KFLD                    ARBLNP
129700010508     C                   KFLD                    ARBNRS
129800010508     C                   KFLD                    ARBNSP
129900080711     C     KAR4          KLIST
130000080711     C                   KFLD                    ARBAAS
130100080711     C                   KFLD                    ARBLNP
130200080711     C                   KFLD                    ARBNRS
130300080711     C                   KFLD                    ARBNSP
130400080711     C                   KFLD                    ktrc
130500010508     C     KARB7         KLIST
130600010508     C                   KFLD                    ARBAAS
130700010508     C                   KFLD                    ARBLNP
130800010508     C                   KFLD                    ARBNRS
130900010508     C                   KFLD                    ARBNSP
131000010508     C                   KFLD                    AR6TRC
131100010508     C     KARB2         KLIST
131200010508     C                   KFLD                    FIL
131300010508     C                   KFLD                    WDCM
131400010508     C     KART          KLIST
131500010508     C                   KFLD                    ARTFLS
131600010508     C                   KFLD                    ARTLNA
131700010508     C                   KFLD                    ARTNRS
131800010508     C                   KFLD                    ARTNSC
131900010508     C     KEVB4         KLIST
132000010508     C                   KFLD                    ARBAAS
132100010508     C                   KFLD                    ARBLNP
132200010508     C                   KFLD                    ARBNRS
132300010508     C                   KFLD                    ARBNSP
132400010508     C                   KFLD                    WCEV
132500050413
132600050906     C     KGCA          KLIST
132700050906     C                   KFLD                    ARBAAS
132800050906     C                   KFLD                    ARBLNP
132900050906     C                   KFLD                    ARBNRS
133000050906     C                   KFLD                    ARBNSP
133100050906     C                   KFLD                    KFRG
133200050404
133300050906     c     kblp          klist
133400050906     c                   kfld                    GcpAas
133500050906     c                   kfld                    GcpLnp
133600050906     c                   kfld                    GcpNrs
133700050906     c                   kfld                    GcpNsp
133800020530     c*
133900020530     c     kdcf          klist
134000020530     c                   kfld                    dctaac
134100020530     c                   kfld                    dctfil
134200020530     c                   kfld                    dctnca
134300010508      *
134400101007     C****               MOVEL     'R  '         WCEV
134500050906     C                   clear                   kfrg
134600040423     c*  File Fitgd02l    RECLAMI
134700040423     c     ktgd          klist
134800040423     c                   kfld                    ktgtor
134900040423     c                   kfld                    ktgogg
135000040423     C*
135100010508      *
135200010508      * DEFINIZIONE CAMPI
135300010508     C     *LIKE         DEFINE    EVBCEV        WCEV
135400010508     C     *LIKE         DEFINE    KPJBU         WSAJBU
135500010508     C     *LIKE         DEFINE    ARBDCM        WDCM
135600010508     C     *LIKE         DEFINE    AR4TRC        WTRC
135700010508     C     *LIKE         DEFINE    DCTDCH        WDCH
135800020530     C     *LIKE         DEFINE    DCTDCH        WDCH_dcb
135900010508     C     *LIKE         DEFINE    ARBLNP        SAVLNP
136000010508     C     *LIKE         DEFINE    TBLCOD        COD
136100020409     C     *LIKE         DEFINE    TBLKEY        KEY
136200050906     C     *LIKE         DEFINE    GCPFRG        KFRG
136300020207     C     *LIKE         DEFINE    §ogntw        lnantw
136400040831     C     *LIKE         DEFINE    §ogntw        lnpntw
136500040831     c*
136600040831      * Se sono in ambiente buono - GAITRAGRU
136700040831     c                   If        knsif = 'FILTRA201 '
136800040831     c                   Eval      wlib = 'GAITRAGRU '
136900040831      *  Se sono in ambiente di prova - GAITRAGRPS
137000040831     c                   Else
137100040831     c                   Eval      wlib = 'GAITRAGRPS'
137200040831     c                   EndIf
137300040831     c*
137400040831     c                   Clear                   commant
137500040831     c                   Movel(p)  cmdt(1)       commant
137600040831     c                   Eval      %Subst(commant:30:10) = wlib
137700040831     c                   Eval      commant =
137800040831     c                             %trim(commant) + '/TITAS30C)'
137900040831     c                   Call      'QCMDEXC'                            99
138000040831     c                   Parm                    commant
138100040831     c                   Parm                    lenght
138200040831     c                   Open      Titas30c
138300060123     c*
138400060123     c                   Clear                   commant
138500060123     c                   Movel(p)  cmdt(2)       commant
138600060123     c                   Eval      %Subst(commant:30:10) = wlib
138700060123     c                   Eval      commant =
138800060123     c                             %trim(commant) + '/TNCSB03L)'
138900060123     c                   Call      'QCMDEXC'                            99
139000060123     c                   Parm                    commant
139100060123     c                   Parm                    lenght
139200060123     c                   Open      TNCSB03L
139300101007     c
139400020409     c*
139500020409     c                   exsr      caldts
139600010508      *
139700010508     C                   ENDSR
139800020409     C*---------------------------------------------------------------*
139900020409     C* caldts - Calcolo date stampa                                  *
140000020409     C*---------------------------------------------------------------*
140100020409     C     CALDTS        BEGSR
140200020409      * LETTURA giorni per date stampa e coda di output
140300020409     C                   MOVEL     '5A'          COD
140400020409     C                   MOVEL     '2       '    KEY
140500020409     C     ktab1         CHAIN     taBEL00F                           30
140600020409     C  N30              MOVEL     TBLUNI        DS5A2
140700020409     C   30              CLEAR                   DS5A2
140800020409     C*
140900020409     C                   MOVEL     '5A'          COD
141000020409     C                   MOVEL     '3       '    KEY
141100020409     C     ktab1         CHAIN     TABEL00F                           30
141200020409     C  n30              MOVEL     TBLUNI        DS5A3
141300020409     C   30              CLEAR                   DS5A3
141400020409     c     §5ahld        ifeq      'S'
141500020409     c                   movel     '*YES'        whld              4
141600020409     c                   else
141700020409     c                   movel     '*NO '        whld
141800020409     c                   endif
141900020409     c     §5asav        ifeq      'S'
142000020409     c                   movel     '*YES'        wsav              4
142100020409     c                   else
142200020409     c                   movel     '*NO '        wsav
142300020409     c                   endif
142400020409     c* BOLLE NO POSTE
142500020409     c* 1) data stampa bolle lna italia
142600020409     c     *ymd          move      comdpb        data_iso
142700020409     c                   subdur    §5agal:*d     data_iso
142800020409     c                   move      data_iso      dtsita_ymd
142900020409     c                   move      data_iso      data_dmy
143000020409     c                   move      data_dmy      dtsita
143100020409     c* 2) data stampa bolle export
143200020409     c     *ymd          move      compbe        data_iso
143300020409     c                   subdur    §5agal:*d     data_iso
143400020409     c                   move      data_iso      dtsexp_ymd
143500020409     c                   move      data_iso      data_dmy
143600020409     c                   move      data_dmy      dtsexp
143700020409     c* 3) data stampa bolle con riserva
143800020409     c     *ymd          move      comarr        data_iso
143900020409     c                   subdur    §5agal:*d     data_iso
144000020409     c                   move      data_iso      dtsrsv_ymd
144100020409     c                   move      data_iso      data_dmy
144200020409     c                   move      data_dmy      dtsrsv
144300020409     c* 4) data stampa bolle con c.a.
144400020409     c     *ymd          move      combca        data_iso
144500020409     c                   subdur    §5agcn:*d     data_iso
144600020409     c                   move      data_iso      dtscan_ymd
144700020409     c                   move      data_iso      data_dmy
144800020409     c                   move      data_dmy      dtscan
144900120222     c                   move      data_dmy      dtsncc
145000020409     c* BOLLE POSTE
145100020409     c* 1) data stampa bolle lna italia
145200020409     c     *ymd          move      posdpb        data_iso
145300020409     c                   subdur    §5agal:*d     data_iso
145400020409     c                   move      data_iso      dpsita_ymd
145500120220     c***                move      data_iso      data_dmy
145600120220     c****               move      data_dmy      dpsita
145700020409     c* 2) data stampa bolle export
145800020409     c     *ymd          move      pospbe        data_iso
145900020409     c                   subdur    §5agal:*d     data_iso
146000020409     c                   move      data_iso      dpsexp_ymd
146100120220     c***                move      data_iso      data_dmy
146200120220     c***                move      data_dmy      dpsexp
146300020409     c* 3) data stampa bolle con riserva
146400020409     c     *ymd          move      posarr        data_iso
146500020409     c                   subdur    §5agal:*d     data_iso
146600020409     c                   move      data_iso      dpsrsv_ymd
146700120220     c***                move      data_iso      data_dmy
146800120220     c***                move      data_dmy      dpsrsv
146900020409     c* 4) data stampa bolle con c.a.
147000020409     c     *ymd          move      posbca        data_iso
147100020409     c                   subdur    §5agcn:*d     data_iso
147200020409     c                   move      data_iso      dpscan_ymd
147300120220     c****               move      data_iso      data_dmy
147400120220     c****               move      data_dmy      dpscan
147500120220     c******             move      data_dmy      dpsncc
147600020409     c*
147700020409     c* Data per stampare le spedizioni non consegnate
147800020409     c                   subdur    §5agsn:*d     wdate_inv
147900020409     c                   move      wdate_inv     dtsncn_ymd
148000020409     c                   move      wdate_inv     data_dmy
148100020409     c                   move      data_dmy      dtsncn
148200120220     c****               move      data_dmy      dpsncn
148201120220
148202120220     c                   move      wdateu        data_iso
148203120220     c                   subdur    §5agdc:*d     data_iso
148204120220     c                   move      data_iso      dtsgdc_ymd
148205120220     c                   move      data_iso      data_dmy
148206120222     c                   move      data_dmy      dtsnccd
148300020409     c                   endsr
148400020404**
148500020409OVRPRTF FNLR84P OUTQ
148600120220HOLD(    ) SAVE(    ) USRDTA('PulBoArr')
148700040831** cmdt
148800040831OVRDBF FILE(TITAS30C) TOFILE(
148900060123OVRDBF FILE(TNCSB03L) TOFILE(
