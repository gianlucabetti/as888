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
001001141209     FTntbe01l  IF   E           K DISK
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
003501140214     FFIsia01L  UF   E           K DISK
003600941207     F*
003700941207     F* ARCHIVIO VARIAZIONI BOLLE
003800131014     FFIARBF2C  UF   E           K DISK
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
004801131029     FFNEVB22L  UF   E           K DISK
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
006701141209     F* Disposizioni di consegna
006702141209     Ftiidc01l  uF   E           K DISK
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
007901141209     d mach            s              3    dim(50)
008000020409     d cmd             s             50    dim(2) CTDATA PERRCD(1)
008100060123     d cmdt            s             60    dim(02) ctdata perrcd(1)
008200020404     D*
008300020404     d data_EUR        s               d   datfmt(*EUR)
008400020404     d data_DMY        s               d   datfmt(*DMY)
008500020409     d data_ISO        s               d   datfmt(*ISO)
008600020409     d wdate_inv       s               d   datfmt(*ISO)
008700120216     d wdateu          s               d   datfmt(*ISO)
008800020404     d data_Alfa       s             10a
008900020409     d dtsita_ymd      s              8  0
009000020409     d dtsexp_ymd      s              8  0
009100020409     d dtsrsv_ymd      s              8  0
009200020409     d dtscan_ymd      s              8  0
009300020409     d dtsncn_ymd      s              8  0
009400120220     d dtsgdc_ymd      s              8  0
009500020409     d dpsita_ymd      s              8  0
009600020409     d dpsexp_ymd      s              8  0
009700020409     d dpsrsv_ymd      s              8  0
009800020409     d dpscan_ymd      s              8  0
009900020404     d wopn            s              1a
010000040812     d dctdch          s              8  0
010100040812     d dctcch          s              2a
010200040831     d wlib            s             10
010300040831     d commant         s             80
010400040831     d lenght          s             15  5 inz(80)
010500040423
010600040423     d ktgtor          s                   like(tgdtor) inz('S')
010700040423     d ktgogg          s                   like(tgdogg)
010800080711     d ktrc            s                   like(ar4trc)
010900080711     d W008a           s              8
011000080711     d W0080           s              8  0
011100080711     d Indx            s              3  0
011101141209     d weladata        s                   like(idceladata) inz(0)
011200040423
011300941207     D*
011400961204     D* PARAMETRI RICEVUTI DAI PGM CHIAMANTI
011500900618     D PARAM           DS
011600961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA NON ESTERA)
011700941207     D  COMDPB                17     24  0
011800961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA ESTERA - BOLLE EXPORT)
011900961204     D  COMPBE                68     75  0
012000980722     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA
012100990707     D  COMARR               191    198  0
012200990126     D* DATA PULIZIA BOLLE ARRIVI CON C.A.
012300990707     D  COMBCA               199    206  0
012400000530     D* DATA PULIZIA BOLLE ARRIVI - POSTE
012500000530     D  POSDPB               207    214  0
012600000530     D* DATA PULIZIA BOLLE ARRIVI - BOLLE EXPORT - POSTE
012700000530     D  POSPBE               215    222  0
012800000530     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA - POSTE
012900000530     D  POSARR               223    230  0
013000000530     D* DATA PULIZIA BOLLE ARRIVI CON C.A. - POSTE
013100000530     D  POSBCA               231    238  0
013200040423     D* DATA PULIZIA BOLLE CON RECLAMI
013300040423     D  COMREC               239    246  0
013400961204     D*
013500920324     D                 DS
013600941207     D  ARBAAS                 1      4  0
013700941207     D  ARBMGS                 5      8  0
013800941207     D  ARBDSP                 1      8  0
013900020405     D                 DS
014000040813     D  dctAAc2                1      4  0
014100020405     D  dctmgc                 5      8  0
014200020405     D  dctdca                 1      8  0
014300941207     D*
014400990823     D* DS PER TIBS02DS- RICERCA TABELLE
014500990823     D DSBS02        E DS                  EXTNAME(TIBS02DS)
014600990823     D DSDN11        E DS                  EXTNAME(FIDN11DS)
014700941207     D*
014800000530     D OG143         E DS
014900101007     D DS2A          E DS
015000101007     D DS3A          E DS
015100020409     D DS5A2         E DS
015200020409     D DS5A3         E DS
015201141209     D Dmar          E DS
015300000000     D KPJBA         E DS
015400930507     D CNCR80        E DS
015500930507     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
015600050404     D FNLGZ5DS      E DS
015700040812      * ds per il controllo della presenza di CA per la spedizione richiesta
015800040812     d FIDN12DS      e ds
015900040812     d  skKey                 26    305    dim(20)
016000040812     d  skAnn                306    325    dim(20)
016100040812     d  skDca                326    485  0 dim(20)
016200040812     d  skFca                486    545  0 dim(20)
016300040812     d  skDch                546    705  0 dim(20)
016400040812     d  skCch                706    745    dim(20)
016401141209
016402141209     d FIDNc0DS      e ds
016500040812
016600040812     d dsKey           ds
016700040812     d  dctaac                        4  0
016800040812     d  dctfil                        3  0
016900040812     d  dctnca                        7  0
017000040812      *
017100040423     D*-------------
017200040423     D* DS OGGETTO RECLAMO
017300040423     D*-------------
017400040423     D                 DS
017500040423     D  W_lnp                         3  0
017600040423     D  W_nrs                         2  0
017700040423     D  W_nsp                         7  0
017800040423     D  W_aas                         4  0
017900040423     D  Wspe                   1     16  0 inz
018000020404     D*
018100020404     D* DEFINIZIONE COSTANTI
018200020404     D*
018300020530     D CBGIAC          C                   CONST('Bolla giacente in fase')
018400020404     D CBRISE          C                   CONST('Bolla con riserva consegnata -
018500020404     D                                     il            ')
018600020530     D CBCANA          C                   CONST('Bolla con C.A. aperta - Fase')
018700020404     D CBCANC          C                   CONST('Bolla con C.A. chiusa il')
018800020404     D CBCANN1         C                   CONST('Presente C.A. chiusa il ')
018900020404     D CBCANN2         C                   CONST(' non ancora stampata nell''el-
019000020404     D                                     enco C.A. da archiviare')
019100080711     D CBcass          C                   CONST('C/Assegno non incassato')
019200020405     D CBercan         C                   CONST('Errore in cancellazione C.A. -
019300020405     D                                     n.')
019400020405     D CBcpdr          C                   CONST('Bolla presente su archivio co-
019500020404     D                                     nteggi padroncini')
019600080515     D CBChoprN        C                   CONST('Manca numero MANDATO INTROITO-
019700080515     D                                      ma c''è data')
019800080515     D CBChoprD        C                   CONST('Chiusura operatore non effett-
019900080515     D                                     uata')
020000020405     D CBinc           C                   CONST('Spedizione ancora da incassar-
020100020405     D                                     e')
020200020409     D CBncon          C                   CONST('Bolla non consegnata')
020300020411     D CBnconca        C                   CONST('Bolla non consegnata con C.A.-
020400080515     D                                     chiusa')
020500080515     D CBcrec          C                   CONST('Bolla con Reclamo APERTO')
020600040831     D CBdt1           C                   CONST('Bolla con data trasmissione <-
020700040831     D                                     data mandato di introito')
020800010508      *
020900941207     IFNARBD00      01
021000941207     IFNARBK00      02
021100991104     IFIARBT00      03
021200991104     IFNARBP00      04
021300941207     IFNARBG00      05
021400941207     IFNARBM00      06
021500941207     IFNARBV00      07
021600131014     IFNARBN00      08
021700930507     C*---------------------------------------------------------------*
021800950516     C* MI SALVO LA KPJBU
021900950516     C                   MOVEL     KPJBU         WSAJBU
022000950516     C*
022100020611     C* CARICO TABELLE
022200930507     C                   EXSR      CARTAB
022300930507     C*
022400920306     C                   Z-ADD     0             FIL               3 0
022500950526     C                   Z-ADD     1             WDCM
022600010508     C                   eval      *in30 = *off
022700920306     C*
022800941207    1C     *IN30         DOWEQ     *OFF
022900941207     C     FIL           SETGT     FNARB000
023000941207     C                   READ      FNARB000                               30
023100920306     C*
023200941207    2C     *IN30         IFEQ      *OFF
023300000724     C                   CLEAR                   SAVLNP
023400920324     C                   Z-ADD     ARBLNA        FIL
023500980722     C     ARBLNA        CHAIN     AZORG01L                           35
023600020207     C  N35              MOVE      ORGde3        og143
023700020207     C  n35              movel     §ogntw        lnantw
023800020207     C   35              CLEAR                   lnantw
023900000809     C*
024000020207     C     lnantw        IFEQ      'EEX'
024100020207     C     lnantw        OREQ      'EUP'
024200020207     C     lnantw        OREQ      'FED'
024300020211     C     lnantw        OREQ      'DPD'
024400000809     C     COMPBE        IFGT      POSPBE
024500000809     C                   Z-ADD     COMPBE        WDACNF            8 0
024600000809     C                   ELSE
024700000809     C                   Z-ADD     POSPBE        WDACNF
024800000809     C                   ENDIF
024900000809     C                   ELSE
025000000809     C     COMDPB        IFGT      POSDPB
025100000809     C                   Z-ADD     COMDPB        WDACNF            8 0
025200000809     C                   ELSE
025300000809     C                   Z-ADD     POSDPB        WDACNF
025400000809     C                   ENDIF
025500000809     C                   ENDIF
025600920306     C*
025700950526     C     KARB2         SETLL     FNARB000
025800941207     C     FIL           READE     FNARB000                               31
025900920306     C*
026000941207    3C     *IN31         DOWEQ     *OFF
026100000724     C     ARBLNP        IFNE      SAVLNP
026200000724     C                   MOVE      ARBLNP        SAVLNP
026300020207     C                   MOVE      '   '         lnpntw
026400000724     C     ARBLNP        CHAIN     AZORG01L                           35
026500000724     C  N35              MOVEL     ORGDE3        OG143
026600020207     C  N35              MOVEL     §ogntw        lnpntw
026700000724     C                   ENDIF
026800020211     c* pulizia bolle estere COMPRESA LA DPD
026900020207     C     lnantw        IFEQ      'EEX'
027000020207     C     lnantw        OREQ      'EUP'
027100020207     C     lnantw        OREQ      'FED'
027200020211     C     lnantw        OREQ      'DPD'
027300000724     C* DATA PULIZIA BOLLE EXPORT (POSTE O NON POSTE)
027400000724     C                   Z-ADD     COMPBE        WDAPUL            8 0
027500020409     C                   Z-ADD     dtsexp_ymd    WDAprt            8 0
027600000724     C                   ELSE
027700000724     C* DATA PULIZIA BOLLE ITALIE E IMPORT (POSTE O NON POSTE)
027800020207     C     lnpntw        IFNE      'PPT'
027900000724     C                   Z-ADD     COMDPB        WDAPUL
028000020409     C                   Z-ADD     dtsita_ymd    WDAprt            8 0
028100000724     C                   ELSE
028200000724     C                   Z-ADD     POSDPB        WDAPUL
028300020409     C                   Z-ADD     dpsita_ymd    WDAprt            8 0
028400000724     C                   ENDIF
028500000724     C                   ENDIF
028600930628     C*
028700950526     C* CONSIDERO SOLO LE BOLLE
028800961204     C*
028900000809     C* SE DATA CONSEGNA  > DATA PULIZIA PIU' ALTA LEGGO ALTRA LINEA
029000000809    5C     ARBDCM        IFGT      WDACNF
029100920306     C                   SETON                                        31
029200941207   X5C                   ELSE
029300000809     C* ALTRIMENTI ELABORO SE DATA CONSEGNA <= DATA PULIZIA
029400000809     C* SE INVECE E' MAGGIORE IGNORO QUESTO RECORD
029500000809   5AC     ARBDCM        IFLE      WDAPUL
029600930507     C*
029700970623     C                   CLEAR                   WOK
029800000530     C*
029900000530     C* IMPOSTO DATA A SECONDA SE E' POSTE O NO
030000020207     C     lnpntw        IFNE      'PPT'
030100000530     C                   Z-ADD     COMARR        WDATA£            8 0
030200020409     C                   Z-ADD     dtsrsv_ymd    WDApr£            8 0
030300000530     C                   ELSE
030400000530     C                   Z-ADD     POSARR        WDATA£            8 0
030500020409     C                   Z-ADD     dpsrsv_ymd    WDApr£            8 0
030600000530     C                   ENDIF
030700980722     C**
030800980722     C* SE BOLLA CON RISERVA PULISCO CONSIDERANDO ANCHE L'ALTRA DATA
030900101007     c                   setoff                                       33
031000101007     c                   z-add     1             yy
031100101007     c                   dow       ris(YY)<>*blanks
031200101007     c                   eval      wcev=ris(YY)
031300101007     c
031400101007     C     KEVB4         SETLL     FNEVB04L
031500101007     c                   if        %equal(fnevb04l)
031600101007     c                   seton                                        33
031700101007     c                   leave
031800101007     c                   else
031900101007     c                   endif
032000101007     c                   add       1             yy
032100101007     c                   enddo
032200101007     c
032300980722     C     *IN33         IFEQ      *ON
032400000530     C     ARBDCM        ANDGE     WDATA£
032500020404     c* stampo motivazione
032600020409     c     arbdsp        iflt      wdapr£
032700020404     c                   exsr      ovrprt
032800020404     c     *ymd          move      arbdcm        data_EUR
032900020404     c                   move      data_EUR      data_Alfa
033000020404     c                   movel(P)  cbrise        p01mes
033100020404     c                   eval      p01mes = %trim(p01mes) + ' ' + data_Alfa
033200020404     c                   exsr      ried1
033300020404     c                   write     lr84d1
033400020409     c                   endif
033500980722     C                   MOVE      'N'           WOK               1
033600980722     C                   ENDIF
033700990126     C**
033800990126     C* SE BOLLA CON C.A. PULISCO PER DATA CHIUSURA C.A.
033900990126     C                   CLEAR                   WDCH
034000020530     C                   CLEAR                   WDCH_dcb
034100020409     C* IMPOSTO DATA stampa A SECONDA SE E' POSTE O NO
034200020409     C     lnpntw        IFNE      'PPT'
034300020409     C                   Z-ADD     dtscan_ymd    WDApr£
034400020409     C                   ELSE
034500020409     C                   Z-ADD     dpscan_ymd    WDApr£
034600020409     C                   ENDIF
034700020409     c*
034800040812      * Sostituisco la lettura del file FNDTC02 con
034900040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
035000040812      * legate alla spedizione che passo
035100040812      *
035200040812     c                   clear                   fidn12ds
035300040812     c                   eval      i12aas = arbaas
035400040812     c                   eval      i12lnp = arblnp
035500040812     c                   eval      i12nrs = arbnrs
035600040812     c                   eval      i12nsp = arbnsp
035700040812     c                   eval      i12fel = 'E'
035800040812     c                   eval      i12fan = 'E'
035900040812      *
036000040812     c                   call      'FIDN12R'
036100040812     c                   parm                    fidn12ds
036200040812      *
036300040812      * se non ci sono errori
036400040812    6c                   if        o12err = ' '
036500040812      * se numero di CA trovate maggiore di zero
036600040812    7c                   if        o12nca > 0
036700040812     c
036800040812     c                   z-add     *zeros        ii                2 0
036900040812    8c     1             do        o12nca        ii
037000040812     c                   movea     skkey(ii)     dskey
037100040812     C                   Z-ADD     skdca(ii)     dctdca
037200040812     C                   Z-ADD     skdch(ii)     dctdch
037300040812     C                   movea     skcch(ii)     dctcch
037400040812      * se CA aperta stampo,imposto il flag e vado a fine
037500040812    9c                   If        dctdch = 0
037600040812     c* stampo motivazione
037700080325   10c**   arbdsp        iflt      wdapr£
037800080325     c**                 exsr      ovrprt
037900080325     c**                 movel(P)  cbcana        p01mes
038000040812     c* chaino file delle fasi per reperire fase, data, e p.o.
038100080325     c**   kdcf          setgt     fndcf01l
038200080325     c**   kdcf          readpe    fndcf01l
038300080325   11c**                 if        not %eof(fndcf01l)
038400080325     c**   *ymd          move      dcfdfc        data_EUR
038500080325     c**                 move      data_EUR      data_Alfa
038600040812     c* chaino file anagrafica fasi per decodificare la fase
038700080325     c**                 clear                   fndfa000
038800080325     c**   dcffca        chain     fndfa01l
038900080325   12c**                 if        %found(fndfa01l)
039000080325     c**                 movel     dfades        wfades           40
039100080325   12c**                 endif
039200080325  x11c**                 else
039300080325     c**                 clear                   data_Alfa
039400080325     c**                 clear                   dcffca
039500080325     c**                 clear                   dcffev
039600080325   11c**                 endif
039700080325     c**                 move      dcffca        wcffca            3
039800080325     c**                 move      dcffev        wcffev            3
039900080325     c**                 eval      p01mes = %trim(p01mes) + ' ' + wcffca
040000080325     c**                           + ' ' + wfades
040100080325     c**                           + ' del ' + data_Alfa + ' p.o. ' + wcffev
040200080325     c**                 exsr      ried1
040300080325     c**                 write     lr84d1
040400080325   10c**                 endif
040500040812     C                   MOVE      'N'           WOK               1
040600040812     c                   leave
040700040812  x 9c                   else
040800040812      * se CA chiusa
040900040812   10C     DCTDCH        IFGT      WDCH
041000040812     C                   Z-ADD     DCTDCH        WDCH
041100040812   10C                   ENDIF
041200040812     c* memorizzo la data chiusura più alta se p.o. C.A. = lna bolla
041300040812   10c     dctdch        ifgt      wdch_dcb
041400040812     c     dctfil        andeq     arblna
041500040812     c                   z-add     dctdch        wdch_dcb
041600040812   10c                   endif
041700040812    9C                   ENDIF
041800040812     c
041900040812    8c                   enddo
042000040812      *
042100040812    7c                   endif
042200040812    6c                   endif
042300040812     C*
042400000530     C*
042500000530     C* IMPOSTO DATA A SECONDA SE E' POSTE O NO
042600020207     C     lnpntw        IFNE      'PPT'
042700000530     C                   Z-ADD     COMBCA        WDATA£            8 0
042800000530     C                   ELSE
042900000530     C                   Z-ADD     POSBCA        WDATA£            8 0
043000000530     C                   ENDIF
043100990126     C**
043200990126     C* SE C.A. TUTTE CHIUSE DEVONO ESSERE PASSATI I GIORNI DALLA DATA
043300990126     C*  CHIUSURA PIU' ALTA
043400990126     C     WOK           IFEQ      ' '
043500000530     C     WDCH          ANDGE     WDATA£
043600990126     C                   MOVE      'N'           WOK               1
043700990126     C                   ENDIF
043800990823     C* NON POSSO PULIRE BOLLA E C.A. SE NON L'HO ANCORA STAMPATA
043900990823     C* NELL'ELENCO C.A. DA ARCHIVIARE
044000990823    6C     WOK           IFEQ      ' '
044100020530     c     wdch_dcb      andgt     *zeros
044200990823     C                   CLEAR                   DSBS02
044300990824     C                   MOVEL     'C'           T02MOD
044400990823     C                   MOVEL     KNSIF         T02SIF
044500990823     C                   MOVEL     'DCB'         T02COD
044600990823     C                   MOVEL     ARBLNA        T02KE1
044700990823     C                   CALL      'TIBS02R'
044800990823     C                   PARM                    KPJBA
044900990823     C                   PARM                    DSBS02
045000990823    7C     T02ERR        IFEQ      *BLANKS
045100990823     C                   MOVEL     T02UNI        WDTACA            8 0
045200990823     C                   ELSE
045300990823     C                   Z-ADD     19990101      WDTACA
045400990823    7C                   ENDIF
045500020530    7C     WDCH_dcb      IFGT      WDTACA
045600020404     c* stampo motivazione
045700020409     c     arbdsp        iflt      wdapr£
045800020404     c                   exsr      ovrprt
045900020530     c     *ymd          move      wdch_dcb      data_EUR
046000020404     c                   move      data_EUR      data_Alfa
046100020404     c                   eval      p01mes = cbcann1 + data_alfa + cbcann2
046200020404     c                   exsr      ried1
046300020404     c                   write     lr84d1
046400020409     c                   endif
046500990823     C                   MOVE      'N'           WOK
046600990823    7C                   ENDIF
046700990823    6C                   ENDIF
046800990823     C*
046900970623   6AC     WOK           IFEQ      *BLANKS
047000050906      *
047100050906      * GIACENZA: se non trovata CANCELLO BOLLA
047200050906      *           se trovata ma è annullata o in fase 60 CANCELLO BOLLA
047300050906      *           negli altri casi non CANCELLO BOLLA
047400050906     C                   clear                   WGIACE            1
047500050906     C     KGCA          CHAIN     Tigcp21L
047600010508      *
047700050906     c                   SELECT
047800050906     c                   WHEN      not %found(Tigcp21L)
047900050906     c                   eval      *in33 = *off
048000050906     c                   WHEN      GCPATB <> *blanks or GCPFAS = 60
048100050906     c     kblp          Setll     Fnblp01l
048200050906     c                   eval      *in33 = *off
048300110210     c
048400110210     c                   If        Not %Equal(Fnblp01l)
048500050906     c                   Eval      wgiace = 'S'
048600050906     c                   EndIf
048700050906     c                   OTHER
048800020404     c* stampo motivazione
048900020409     c     arbdsp        iflt      wdaprt
049000020404     c                   exsr      ovrprt
049100020404     c                   movel(P)  cbgiac        p01mes
049200020530     c* prendo la decodifica fase
049300020530     C                   MOVEL     '2Y'          COD
049400050906     C                   MOVEL(P)  gcpfas        KEY
049500020530     C     ktab1         CHAIN     TABEL00F                           37
049600020530     C   37              MOVEL     *ALL'?'       decfas           45
049700020530     C  N37              MOVEL     TBLUNI        decfas
049800050906     c                   move      gcpfas        wcpfas            3
049900020530     c                   eval      p01mes = %trim(p01mes) + ' ' + wcpfas + '-'
050000020530     c                             + decfas
050100020404     c                   exsr      ried1
050200020404     c                   write     lr84d1
050300020409     c                   endif
050400050906      * Imposto flag x non pulire la bolla
050500050906     c                   MOVE      'N'           WOK
050600010508     c                   eval      *in33 = *on
050700050906     c                   ENDSL
050800020405     c*
050900020415     c  n33arbnmi        ifeq      *zeros
051000020405     c* stampo motivazione
051100020409     c     arbdsp        iflt      wdaprt
051200020405     c                   exsr      ovrprt
051300080515     c                   if        arbdmi>0
051400080515     c                   movel(P)  cbchoprn      p01mes
051500080515     c                   else
051600080515     c                   movel(P)  cbchoprd      p01mes
051700080515     c                   endif
051800080515     c
051900020405     c                   exsr      ried1
052000020405     c                   write     lr84d1
052100020409     c                   endif
052200020405     c                   else
052300020405     c     arbfip        ifeq      'I'
052400020405     c* stampo motivazione
052500020409     c     arbdsp        iflt      wdaprt
052600020405     c                   exsr      ovrprt
052700020405     c                   movel(P)  cbinc         p01mes
052800020405     c                   exsr      ried1
052900020405     c                   write     lr84d1
053000020409     c                   endif
053100020405     c                   endif
053200020405     c                   endif
053300920410     C*
053400920505     C* PULISCO SOLO SE NON E' DA TORNARE ALL'INCASSO
053500950110     C*   E SE HA GIA' IL MANDATO DI INTROITO
053600000927    6C  N33ARBFIP        IFNE      'I'
053700930308     C     ARBNMI        ANDNE     0
053800000927     C*
053900000927    7C     ARBICA        IFEQ      ' '
054000000927     C     ARBICA        OREQ      'R'
054100000927     C* VERIFICO SE PER CASO SI TRATTA DI UN FINTO RITORNO ALL'INCASSO
054200000927     C                   EXSR      CTRICA
054300000927    7C                   ENDIF
054400020405     c*
054500020405     c     arbica        ifeq      ' '
054600020405     c     arbica        oreq      'R'
054700020405     c* stampo motivazione
054800020409     c     arbdsp        iflt      wdaprt
054900020405     c                   exsr      ovrprt
055000020405     c                   movel(P)  cbinc         p01mes
055100020405     c                   exsr      ried1
055200020405     c                   write     lr84d1
055300020409     c                   endif
055400020405     c                   endif
055500000927     C*
055600000927   7AC     ARBICA        IFNE      ' '
055700000927     C     ARBICA        ANDNE     'R'
055800930308     C*
055900051115     C* AGGANCIO FiAR900F PER VEDERE SE C'E' IL C/ASSEGNO
056000051115     c                   setoff                                       34
056100051115     c     arbicc        ifeq      ' '
056200020405     c     arbicc        oreq      'R'
056300051115     C     KARB          SETLL     FiAR9000                               34
056400060123     c     *in34         ifeq      *on
056500060123     C     Karb          chain     tncsb03l
056600060123     c                   if        %found(tncsb03l)
056700060123     c* stato 9 --> Annullato; Data distinta > 0 --> c/a incassato
056800060123     c                   if        csbsta = 9 or csbddc > 0
056900060123     c                   setoff                                       34
057000060123     c                   endif
057100060123     c                   endif
057200060123     c                   endif
057300020405     c* stampo motivazione
057400080711    8c   34arbdsp        iflt      wdaprt
057500080711     c* Se si tratta di C/Ass omesso, verifico se devo davvero stampare
057600080711     c*  in base alla data di rigenerazione
057700080711     c                   clear                   w008a
057800080711     c                   clear                   w0080
057900080711     c                   eval      ktrc='8'
058000080711     c     kar4          chain(n)  fiar401l
058100080711    9c                   if        %found(fiar401l)
058200080711     c                   eval      Indx=%scan('RIGENERATA':ar4not)
058300080711   10c                   if        Indx>0
058400080711     c                   eval      ktrc='9'
058500080711     c     kar4          chain(n)  fiar401l
058600080711   11c                   if        %found(fiar401l)
058700080711     c                   eval      Indx=%scan('OMESSO':ar4not)
058800080711   12c                   if        Indx>0
058900080711     c                   eval      w008a=%subst(ar4not:26:8)
059000080711   13c                   if        w008a>*zeros
059100080711     c                   movel     w008a         w0080
059200080711   13c                   endif
059300080711   12c                   endif
059400080711   11c                   endif
059500080711   10c                   endif
059600080711    9c                   endif
059700080711     c
059800080711    9c                   if        w0080=0 or w0080<wdapul
059900020405     c                   exsr      ovrprt
060000080711     c                   if        w0080>0
060100020405     c                   movel(P)  cbcass        p01mes
060200080711     c                   eval      p01mes=%trim(p01mes)+' rigenerata il '+
060300080711     c                             %editw(w0080:'    /  /  ')
060400080711     c                   endif
060500080711     c
060600020405     c                   exsr      ried1
060700020405     c                   write     lr84d1
060800080711    9c                   endif
060900080711    8c                   endif
061000080711   7ac                   endif
061100941207     C*
061200950110     C* PULISCO SOLO SE NON C'E' IL C/ASSEGNO OPPURE SE C'E' MA E'
061300950110     C*   GIA' STATO INCASSATO
061400941212    7C     *IN34         IFEQ      *ON
061500930121     C     ARBICC        ANDNE     ' '
061600950119     C     ARBICC        ANDNE     'R'
061700941212     C     *IN34         OREQ      *OFF
061800950110     C*
061900950110     C* PULISCO SOLO SE NON TROVO RECORD NEI FILE TASSAZIONE PADRONCINI
062000020405     c                   clear                   p01mes
062100011121     C     KARB          SETLL     fiftd000                               36
062200020405     c     *in36         ifeq      *on
062300020405     c                   movel(P)  cbcpdr        p01mes
062400020405     c                   endif
062500011121     C  N36KARB          SETLL     fifsd000                               36
062600080711     c* visto che normalmente rimangono i record consegna e quindi scritti
062700080711     c*  in FTD per data consegna enon per data spedizione, stampo solo se
062800080711     c*  la data consegna < data stampa
062900020405     c     *in36         ifeq      *on
063000020409     c     arbdsp        andlt     wdaprt
063100080711     c     arbdcm        andeq     0
063200080711     c     *in36         oreq      *on
063300080711     c     arbdcm        andlt     wdaprt
063400080711     c     arbdcm        andgt     0
063500080711     c
063600020405     c     p01mes        ifeq      *blanks
063700020405     c                   movel(P)  cbcpdr        p01mes
063800020405     c                   eval      p01mes = %trim(p01mes) + ' di simulazione'
063900020405     c                   endif
064000020405     c                   exsr      ovrprt
064100020405     c                   exsr      ried1
064200020405     c                   write     lr84d1
064300020405     c                   endif
064400040831     c*
064500040831     c* Posso pulire solo le bolle:
064600040831     C* 1) le cui date di consegna sono state trasmesse in partenza e sede
064700040831     c*     arbft1 = 'T'
064800040831     c* 2) autogenerate poste con data ricezione >= data mandato introito
064900040831     c*    (sono quelle ex vedi pacco che ricevo tempo dopo)
065000040831     c*    arbft1 = 'R' e
065100040831     c* in caso contrario stampo segnalazione
065200040831    8c     *IN36         IFEQ      *OFF
065300040831     c*
065400040831     c     arbft1        ifne      'T'
065500040831     c     arbdt1        andlt     arbdmi
065600040831     c                   eval      wok = 'N'
065700040831     c* stampo motivazione
065800040831     c     arbdsp        iflt      wdaprt
065900040831     c                   exsr      ovrprt
066000040831     c                   movel(P)  cbdt1         p01mes
066100040831     c                   exsr      ried1
066200040831     c                   write     lr84d1
066300040831     c                   endif
066400040831     c                   endif
066500950110     C*
066600991104     C****** CANCELLAZIONE C.A. *******
066700040831   8Ac     wok           ifeq      *blanks
066800991104     C* SE BOLLA LOCALE NON CANCELLO LA C.A. SE PRESENTE BOLLA IN BLP
066900020611     c                   eval      *in96 = *off
067000020611     c     karb          setll     fnblp01l                               96
067100991104     C**
067200020611     c                   if        not *in96
067300991104     C**
067400991104     C                   MOVE      *BLANKS       WERR              1
067500040812      * Sostituisco la lettura del file FNDTC02 con
067600040813      * richiamo del programma FIDN12R passando la DS richiedendo le CA
067700040812      * legate alla spedizione che passo
067800040812      *
067900040812     c                   clear                   fidn12ds
068000040812     c                   eval      i12aas = arbaas
068100040812     c                   eval      i12lnp = arblnp
068200040812     c                   eval      i12nrs = arbnrs
068300040812     c                   eval      i12nsp = arbnsp
068400040812     c                   eval      i12fel = 'E'
068500040812      *
068600040812     c                   call      'FIDN12R'
068700040812     c                   parm                    fidn12ds
068800040812      *
068900040812      * se non ci sono errori
069000040812     c                   if        o12err = ' '
069100040812      * se numero di CA trovate maggiore di zero
069200040812     c                   if        o12nca > 0
069300040812     c
069400040812     c                   z-add     *zeros        ii                2 0
069500040812     c     1             do        o12nca        ii
069600040812     c                   movea     skkey(ii)     dskey
069700040812     C                   Z-ADD     skdca(ii)     dctdca
069800040813     C                   CLEAR                   DSDN11
069900040813     C                   MOVEL     'D'           I11MOD
070000040813     C                   Z-ADD     DCTAAC        I11AAC
070100040813     C                   Z-ADD     DCTFIL        I11FIL
070200040813     C                   Z-ADD     DCTNCA        I11NCA
070300040813     C                   CALL      'FIDN11R'
070400040813     C                   PARM                    KPJBA
070500040813     C                   PARM                    DSDN11
070600040813   11C     O11ERR        IFEQ      'E'
070700040813     C                   MOVEL     '1'           WERR
070800040813   11C                   END
070900040813      *
071000040813     c                   enddo
071100040813      *
071200040813     c                   endif
071300040813     c                   endif
071400991104     C**
071500991104     C* SE NON SONO RIUSCITA A CANCELLARE TUTTE LE C.A. NON POSSO
071600991104     C* CANCELLARE LA BOLLA
071700991104   10C     WERR          IFNE      *BLANKS
071800020405     c* stampo motivazione
071900020405     c                   exsr      ovrprt
072000020405     c                   movel(P)  cbercan       p01mes
072100020405     c     *ymd          move      dctdca        data_EUR
072200020405     c                   move      data_EUR      data_Alfa
072300020405     c                   eval      p01mes = %trim(p01mes) + %editc(dctfil:'Z')
072400020405     c                             + %editc(dctnca:'Z') + 'del'  + data_Alfa
072500020405     c                   exsr      ried1
072600020405     c                   write     lr84d1
072700991104     C                   MOVE      'N'           WOK
072800991104   10C                   ENDIF
072900991104    9C                   ENDIF
073000040423      *
073100040423      * RECLAMI : se non trovata CANCELLO BOLLA
073200040423      *
073300040423     C*
073400040423    1C     WOK           IFEQ      ' '
073500040423     c                   movel     arbaas        W_aas
073600040423     c                   movel     arblnp        W_lnp
073700040423     c                   movel     arbnrs        W_nrs
073800040423     c                   movel     arbnsp        W_nsp
073900040423      *
074000040423     c                   movel     wspe          ktgogg
074100040423      *
074200040423     c     ktgd          setll     fitgd02l
074300040423     c                   do        *hival
074400040423     c     ktgd          reade     fitgd02l
074500040423
074600040423     c                   if        %eof(fitgd02l)
074700040423     c                   leave
074800040423     c                   endif
074900040423      * se annullato leggo il successivo
075000040423     c                   If        TGDatb <> ' '
075100040423     c                   iter
075200040423     c                   endif
075201141209     c                   select
075202141209      * se ancora aperto ma si può chiudere: cancello R.A.,
075203141209      *                                      chiudo eventuale dispo di cons da TIIDC
075204141209     c                   when      tgddcd = 0 and %lookup(tgdmad:mach)>0
075205141209     c                   clear                   fidnc0ds
075206141209     c                   eval      IC0ANT=tgdant
075207141209     c                   eval      IC0NUT=tgdnut
075208141209     c                   eval      IC0PDT=999
075209141209     c                   call      'FIDNC0R'
075210141209     c                   parm                    fidnc0ds
075211141209     c     kidc          setll     tiidc01l
075212141209     c     kidc          reade     tiidc01l
075213141209     c                   dow       not %eof(tiidc01l)
075214141209     c                   if        idcaNT=TGDANT and idcnuT=tgdnut and
075215141209     c                             idcpdt=tgdpdt and idcelaflg='E'
075216141209     c                   eval      IDCELADATA=%int( %subst( %char( %dec
075217141209     c                                        ( %timestamp() ) ) :1 :14 ) )
075218141209     c                   eval      idcelaute=knmus
075219141209     c                   update    tiidc000
075220141209     c                   else
075221141209     c                   unlock    tiidc01l
075222141209     c                   endif
075223141209     c     kidc          reade     tiidc01l
075224141209     c                   enddo
075300040423      * verifico se reclamo ancora aperto
075400141209     c                   when      TGDdcd = 0  or
075500040423      * se reclamo chiuso verifico la data chiusura con la dta di pulizia
075600040423     c                             TGDdcd > comrec
075700040423     c                   eval      wok = 'N'
075800080515     c
075900080515     c* stampo motivazione  solo se aperto
076000080515     c                   If        TGDdcd = 0
076100040423     c                   exsr      ovrprt
076200040423     c                   movel(P)  cbcrec        p01mes
076300040423     c                   exsr      ried1
076400040423     c                   write     lr84d1
076500040423     c                   leave
076600080515     c                   endif
076700080515     c
076800141209     c                   endsl
076900040423
077000040423     c                   enddo
077100040423
077200040423    1c                   endif
077300991104     C****** PULIZIA ALTRI FILE ********+
077400991104    9C     WOK           IFEQ      ' '
077500020411     C                   EXSR      PULIZ
077600991104    9C                   ENDIF
077700991104     C**
077800040831   8Ac                   endif
077900991104    8C                   ENDIF
078000991104     C**
078100941207    7C                   ENDIF
078200000927   7AC                   ENDIF
078300941207    6C                   ENDIF
078400970623   6AC                   ENDIF
078500920306     C*
078600000809   5AC                   ENDIF
078700941207     C     FIL           READE     FNARB000                               31
078800941207    5C                   ENDIF
078900950118     C*
079000941207     C*
079100941207    3C                   ENDDO
079200941207    2C                   ENDIF
079300941207    1C                   ENDDO
079400020404     c*
079500020409     C                   Z-ADD     0             FIL               3 0
079600020409     C                   Z-ADD     0             WDCM
079700020409     C                   eval      *in30 = *off
079800020409     C*
079900020409    1C     *IN30         DOWEQ     *OFF
080000020409     C     FIL           SETGT     FNARB000
080100020409     C                   READ      FNARB000                               30
080200020409     C*
080300020409    2C     *IN30         IFEQ      *OFF
080400020409     C                   Z-ADD     ARBLNA        FIL
080500020409     C*
080600020409     C     KARB2         SETLL     FNARB000
080700020409     C     KARB2         READE     FNARB000                               31
080800020409     C*
080900020409    3C     *IN31         DOWEQ     *OFF
081000080515    4c     arbdsp        iflt      dtsncn_ymd
081100020411     c* Guardo se presente c.a.
081200040813      * Sostituisco la lettura del file FNDTC02 con
081300040813      * richiamo del programma FIDN12R passando la DS richiedendo le CA
081400040813      * legate alla spedizione che passo
081500040813      *
081600040813     c                   clear                   fidn12ds
081700040813     c                   eval      i12aas = arbaas
081800040813     c                   eval      i12lnp = arblnp
081900040813     c                   eval      i12nrs = arbnrs
082000040813     c                   eval      i12nsp = arbnsp
082100040813     c                   eval      i12fel = 'E'
082200040813      *
082300040813     c                   call      'FIDN12R'
082400040813     c                   parm                    fidn12ds
082500040813      *
082600040813      * se non ci sono errori  e num.ca magg. di zero accendo il 32
082700080515     c                   clear                   wchiusaCA         1
082800120217     C                   CLEAR                   WDCH_dcb
082900080515    5c                   if        o12err = ' ' AND o12nca > 0
083000040813     c                   seton                                        32
083100080515     c                   eval      WchiusaCA='S'
083200080515     c
083300080515     c* Stampo solo se tutte le c.a. sono chiuse
083400080515     c                   z-add     *zeros        ii                2 0
083500080515    6c     1             do        o12nca        ii
083600080515     c                   movea     skkey(ii)     dskey
083700080515     C                   Z-ADD     skdca(ii)     dctdca
083800080515     C                   Z-ADD     skdch(ii)     dctdch
083900080515     C                   movea     skcch(ii)     dctcch
084000080515     c
084100080515      * se CA chiusa stampo,imposto il flag e vado a fine
084200080515    7c                   If        dctdch = 0
084300080515     c                   clear                   wchiusaCA
084400120217     c                   else
084500120217     c* memorizzo la data chiusura più alta se p.o. C.A. = lna bolla
084600120217    8c     dctdch        ifgt      wdch_dcb
084700120217     c     dctfil        andeq     arblna
084800120217     c                   z-add     dctdch        wdch_dcb
084900120217    8c                   endif
085000080515    7c                   endif
085100080515    6c                   enddo
085200080515    5c                   endif
085300080515     c
085400040813      *
085500120220    5c** 32lnpntw        ifne      'PPT'
085600120220     C**                 Z-ADD     dtscan_ymd    WDAprt
085700120220     c**                 else
085800120220     C**                 Z-ADD     dpscan_ymd    WDAprt
085900120220    5c**                 endif
086000080515    5C***  *in32         ifeq      *off
086100080515     c***  arbdsp        orlt      wdaprt
086200080515    c
086300080515     c                   if        (*in32 and wchiusaCA='S' and
086400120222    5c                             wdch_dcb<dtsgdc_ymd and arbdsp<dtscan_ymd)
086500120217    5c                             or not *in32
086600080515     c
086700020409     c                   exsr      ovrprt
086800020411     c  n32              movel(P)  cbncon        p01mes
086900020411     c   32              movel(P)  cbnconca      p01mes
087000020409     c                   exsr      ried1
087100020409     c                   write     lr84d1
087200080515    5c                   endif
087300080515    4c                   endif
087400020409     C*
087500020409     C     karb2         READE     FNARB000                               31
087600020409     C*
087700020409    3C                   ENDDO
087800020409    2C                   ENDIF
087900020409    1C                   ENDDO
088000020409     c*
088100020404     c     wopn          ifeq      '1'
088200020404     c                   write     lr84z1
088300020404     c                   endif
088400990823     C*
088500990823     C* CHIUDO CON LR
088600990823     C                   CLEAR                   DSBS02
088700990823     C                   MOVEL     'C'           T02TLA
088800990823     C                   CALL      'TIBS02R'
088900990823     C                   PARM                    KPJBA
089000990823     C                   PARM                    DSBS02
089100040813     C*
089200040813     C                   MOVEL     'C'           I12TLA
089300040813     C                   CALL      'FIDN12R'
089400040813     C                   PARM                    FIDN12DS
089500920205     C*
089600950516     C                   MOVEL     WSAJBU        KPJBU
089700920205     C                   SETON                                        LR
089800930628     C*---------------------------------------------------------------*
089900930628     C*                    PULIZ                                      *
090000930628     C* ROUTINE DI PULIZIA FILE BOLLE E FILE COLLEGATI                *
090100930628     C*---------------------------------------------------------------*
090200920324     C     PULIZ         BEGSR
090300060502     c* Chaino file fisico: se riesco pulisco, altrimenti leggo altra bolla
090400060502     c     arbnrr        chain(E)  fnarb00f
090500060502     c                   if        not %error and %found(fnarb00f)
090600941207     C*
090700051115     c                   if        not *in96
090800060220     C*
090900060220     C****  FIAR400F ---> ESTENSIONE DESCRIZIONI  ****
091000060220     C     KARB          SETLL     FIAR4000                               32
091100060220    1C     *IN32         IFEQ      *ON
091200060220     C     KARB          READE     FIAR4000                               32
091300060220     C*
091400060220    2C     *IN32         DOWEQ     *OFF
091500060220     C                   DELETE    FIAR4000
091600060220     C     KARB          READE     FIAR4000                               32
091700060220    2C                   ENDDO
091800060220    1C                   ENDIF
091900060220     C****  FiAR900F ---> DATI C/ASSEGNO  ****
092000051115     C     KARB          CHAIN     FiAR9000                           32
092100051115     C  N32              DELETE    FiAR9000
092200051115     c                   endif
092300941207     C*
092400930628     C*
092500941207     C****  FNART00F ---> DETTAGLIO COLLI  ****
092600941207     C     KARB          SETLL     FNART000
092700941207     C     KARB          READE     FNART000                               32
092800920324     C*
092900941207    1C     *IN32         DOWEQ     *OFF
093000961125     C* CANCELLO GLI EVENTI SEGNACOLLI SENZA NUMERO SPEDIZIONE
093100961125     C                   SETOFF                                       36
093200961125     C     KART          SETLL     FNEVS02L
093300961125    2C     *IN36         DOWEQ     *OFF
093400961125     C     KART          READE     FNEVS02L                               36
093500961125     C  N36              DELETE    FNEVS002
093600961125    2C                   ENDDO
093700051012     c
093800980331     C* CANCELLO L'ESTENSIONE DETTAGLIO SEGNACOLLO
093900051012     c* solo se non più presente la bolla in fnblp
094000051012     c                   if        not *in96
094100051012     c
094200980331     C                   SETOFF                                       36
094300051012     C     KART          SETLL     FIARS01L
094400980331    2C     *IN36         DOWEQ     *OFF
094500051012     C     KART          READE     FIARS01L                               36
094600051012     C  N36              DELETE    FIARS000
094700980331    2C                   ENDDO
094800051012     c
094900051012     c                   endif
095000961125     C*
095100941207     C                   DELETE    FNART000
095200941207     C     KARB          READE     FNART000                               32
095300941207    1C                   ENDDO
095400930628     C*
095500961125     C****  FNEVS00F ---> EVENTI SEGNACOLLI  ****
095600961125     C* CANCELLO GLI EVENTI SEGNCOLLI CON NUMERO SPEDIZIONE
095700961125     C     KARB          SETLL     FNEVS01L
095800961125     C     KARB          READE     FNEVS01L                               32
095900961125     C*
096000961125    1C     *IN32         DOWEQ     *OFF
096100961125     C                   DELETE    FNEVS000
096200961125     C     KARB          READE     FNEVS01L                               32
096300961125    1C                   ENDDO
096400991104     C*
096500991104     C****  FIARI00F ---> FILE DEGLI INCASSI  ****
096600991104     C     KARB          SETLL     FIARI000                               32
096700991104    1C     *IN32         IFEQ      *ON
096800991104     C     KARB          READE     FIARI000                               32
096900991104     C*
097000991104    2C     *IN32         DOWEQ     *OFF
097100991104     C                   DELETE    FIARI000
097200991104     C     KARB          READE     FIARI000                               32
097300991104    2C                   ENDDO
097400991104    1C                   ENDIF
097500061107     C*
097600061107     C****  FIARG00F ---> FILE GEOREFER/GIRI DI CONSEGNA
097700061107     C     KARB          SETLL     FIARG000                               32
097800061107    1C     *IN32         IFEQ      *ON
097900061107     C     KARB          READE     FIARG000                               32
098000061107     C*
098100061107    2C     *IN32         DOWEQ     *OFF
098200061107     C                   DELETE    FIARG000
098300061107     C     KARB          READE     FIARG000                               32
098400061107    2C                   ENDDO
098500061107    1C                   ENDIF
098600070503     C*
098700070503     C****  FIARP00F ---> FILE proposte DCr che dovrebbe essere vuoto
098800070503     C     KARB          SETLL     FIARP000                               32
098900070503    1C     *IN32         IFEQ      *ON
099000070503     C     KARB          READE     FIARP000                               32
099100070503     C*
099200070503    2C     *IN32         DOWEQ     *OFF
099300070503     C                   DELETE    FIARP000
099400070503     C     KARB          READE     FIARP000                               32
099500070503    2C                   ENDDO
099600070503    1C                   ENDIF
099700070503     C*
099800070503     C****  FIARN00F ---> FILE note LDV
099900070503     C     KARB          SETLL     FIARN000                               32
100000070503    1C     *IN32         IFEQ      *ON
100100070503     C     KARB          READE     FIARN000                               32
100200070503     C*
100300070503    2C     *IN32         DOWEQ     *OFF
100400070503     C                   DELETE    FIARN000
100500070503     C     KARB          READE     FIARN000                               32
100600070503    2C                   ENDDO
100700070503    1C                   ENDIF
100701140214     C****  FISIA00F ---> FILE spia per invio Alert FD e L.avviso
100702140214     C     KARB          SETLL     FIsia01l
100703140214    1C                   if        %equal(fisia01l)
100704140214     C     KARB          READE     FIsia01l
100705140214     C*
100706140214    2C                   DOW       not %eof(fisia01l)
100707140214     C                   DELETE    FIsia000
100708140214     C     KARB          READE     fisia01l
100709140214    2C                   ENDDO
100710140214    1C                   ENDIF
100800961125     C*
100900941207     C*****
101000991104     C***  VARIAZIONI BOLLA  E TASSAZIONE
101100941207     C*****
101200930602     C*
101300971002     C* NON PULISCO I LOCALI E I SUOI EVENTI SE LA BOLLA PARTENZA
101400971002     C*  C'E' ANCORA
101500030108     C* (Ho cancellato la lokup su l1 e la setll su fnblp perchè
101600030108     C* le stesse specifiche sono già state eseguite per gestire
101700030108     C* la cancellazione di C.A. di bolla locale - (vedi sopra))
101800020611     c                   if        not *in96
101900991104     C**
102000991104     C****  FIAR600F ---> TASSAZIONE BOLLA  ****
102100991104     C     KARB          CHAIN     FIAR6000                           32
102200991104    2C     *IN32         DOWEQ     *OFF
102300991104     C*
102400991104     C****  FIAR700F ---> ESTENSIONE TASSAZIONE BOLLA  ****
102500991104     C     KARB7         SETLL     FIAR7000                               35
102600991104    3C     *IN35         IFEQ      *ON
102700991104     C     KARB7         READE     FIAR7000                               35
102800991104     C*
102900991104    4C     *IN35         DOWEQ     *OFF
103000991104     C                   DELETE    FIAR7000
103100991104     C     KARB7         READE     FIAR7000                               35
103200991104    4C                   ENDDO
103300991104    3C                   ENDIF
103400991104     C*
103500991104     C                   DELETE    FIAR6000
103600991104     C**
103700991104     C     KARB          READE     FIAR6000                               32
103800991104    2C                   ENDDO
103900930602     C*
104000920410     C                   SETOFF                                       010203
104100941207     C                   SETOFF                                       040506
104200131014     C                   SETOFF                                       0708
104300131014     C     KARB          SETLL     FIARBF2C                               32
104400941207    2C     *IN32         IFEQ      *ON
104500131014     C     KARB          READE     FIARBF2C                               32
104600920324     C*
104700941207    3C     *IN32         DOWEQ     *OFF
104800941207     C   01              DELETE    FNARBD00
104900941207     C   02              DELETE    FNARBK00
105000950630     C*
105100950630    4C     *IN03         IFEQ      *ON
105200950630     C* SE C'E' LA 3° VARIA CONTROLLO SE CE NE SONO ANCHE IN FNARBU0F
105300950630    5C     ARBSV3        IFNE      ' '
105400950630     C*
105500991104     C     KARB          SETLL     FIARBU00                               35
105600950630    6C     *IN35         IFEQ      *ON
105700991104     C     KARB          READE     FIARBU00                               35
105800950630     C*
105900950630    7C     *IN35         DOWEQ     *OFF
106000991104     C                   DELETE    FIARBU00
106100991104     C     KARB          READE     FIARBU00                               35
106200950630    7C                   ENDDO
106300950630    6C                   ENDIF
106400950630    5C                   ENDIF
106500950630     C*
106600991104     C                   DELETE    FIARBT00
106700950630    4C                   ENDIF
106800950630     C*
106900991104     C   04              DELETE    FNARBP00
107000941207     C   05              DELETE    FNARBG00
107100941207     C   06              DELETE    FNARBM00
107200941207     C   07              DELETE    FNARBV00
107300131014     C   08              DELETE    FNARBN00
107400920410     C                   SETOFF                                       010203
107500941207     C                   SETOFF                                       040506
107600131014     C                   SETOFF                                       0708
107700131014     C     KARB          READE     FIARBF2C                               32
107800941207    3C                   ENDDO
107900941207    2C                   ENDIF
108000030108     C*
108100030108     C****  FIAR500F ---> PULIZIA ESTENSIONI TESTATA BOLLA  ****
108200030108     C     KARB          setll     FIAR5000                               32
108300030108    2C                   if        *IN32
108400030108     C     KARB          reade     FIAR5000                               32
108500030108    3C                   dow       not *IN32
108600030108     C                   DELETE    FIAR5000
108700030108     C     KARB          reade     FIAR5000                               32
108800030108    3C                   enddo
108900030108    2C                   endif
109000040604     c*
109100040604     C****  FIAR800F ---> PULIZIA ESTENSIONI TESTATA BOLLA  ****
109200040604     C     KARB          setll     FIAR8000                               32
109300040604    2C                   if        *IN32
109400040604     C     KARB          reade     FIAR8000                               32
109500040604    3C                   dow       not *IN32
109600040604     C                   DELETE    FIAR8000
109700040604     C     KARB          reade     FIAR8000                               32
109800040604    3C                   enddo
109900040604    2C                   endif
110000930628     C*
110100990806     C****  FNEVB00F ---> PULIZIA EVENTI  ****
110200991104     C     KARB          SETLL     FNEVB01L                               32
110300941207    2C     *IN32         IFEQ      *ON
110400040830     c* se bolla presente in titas non posso pulire
110500040830     c     karb          setll     titas30c
110600040831     c                   if        not %equal(titas30c)
110700991104     C     KARB          READE     FNEVB01L                               32
110800941207    3C     *IN32         DOWEQ     *OFF
110900990806     C                   DELETE    FNEVB01L
110901131029     C     KARB2f        chain     FNEVB22L
110902131029     c                   if        %found(fnevb22l)
110903131029     c                   delete    fnevb22l
110904131029     c                   endif
111000991104     C     KARB          READE     FNEVB01L                               32
111100941207    3C                   ENDDO
111200040831     c                   endif
111300941207    2C                   ENDIF
111400930628     C*
111500941207    1C                   ENDIF
111600920324     C**
111700060502     C* CANCELLO LA BOLLA dal file fisico fnarb00F
111800920324     C**
111900060502     C                   DELETE    FNARBfis
112000010508     C**
112100050906      * Pulizia Giacenza: Se wgiace = 'S' vuol dire che ho giacenza chiusa e non c'è
112200050906      *                   la bolla originale su FNBLP in questo caso posso pulire la giacenza
112300050906     c                   If        WGIACE = 'S'
112400050404     c                   z-add     arbaas        iz5aas
112500050404     c                   z-add     arblnp        iz5lnp
112600050404     c                   z-add     arbnrs        iz5nrs
112700050404     c                   z-add     arbnsp        iz5nsp
112800050404     c                   call      'FNLGZ5C'
112900050404     c                   PARM                    FNLGZ5DS
113000050404---  C                   ENDIF
113100060502     c
113200060502---  C                   ENDIF
113300010508      *
113400920306     C                   ENDSR
113500000927     C*---------------------------------------------------------------*
113600000927     C* CONTROLLO SE SI TRATTA DI VERO RITRONO ALL'INCASSO
113700000927     C*---------------------------------------------------------------*
113800000927     C     CTRICA        BEGSR
113900000927     C* VERIFICO SE BOLLA SINGOLA O DOPPIA
114000000927     C     ARBCBO        LOOKUP    CBO                                    32
114100000927     C**
114200000927     C* BOLLA SINGOLA IN ASSEGNATO
114300000927    1C     *IN32         IFEQ      *OFF
114400000927     C                   MOVE      ARBKSC        W0040             4 0
114500000927    2C     W0040         IFNE      9999
114600000927     C* CHAIN SU FIAR6 PER VEDERE SE C'E' FATTURA IMMEDIATA
114700000927     C                   MOVEL     '1'           AR6TRC
114800000927     C     KARB7         CHAIN(N)  FIAR601L                           34
114900000927     C**
115000000927     C* SE NON C'E' O C'E' MA SENZA FATT IMMEDIATA, E' ERRATO
115100000927    3C     *IN34         IFEQ      *ON
115200000927     C     AR6DFT        OREQ      0
115300000927     C     AR6NFT        ANDEQ     0
115400000927     C                   MOVEL     'Y'           ARBICA
115500000927    3C                   ENDIF
115600000927    2C                   ENDIF
115700000927     C**
115800000927   X1C                   ELSE
115900000927     C* CHAIN SU FIAR6 PER VEDERE SE C'E' FATTURA IMMEDIATA
116000000927     C                   MOVEL     '2'           AR6TRC
116100000927     C     KARB7         CHAIN(N)  FIAR601L                           34
116200000927     C**
116300000927     C* SE CE' ED E' S.F. E' FINOTO
116400000927    2C     *IN34         IFEQ      *OFF
116500000927     C     AR6DFT        ANDEQ     0
116600000927     C     AR6NFT        ANDEQ     0
116700000927     C                   MOVE      AR6KSC        W0040
116800000927     C**
116900000927    3C     W0040         IFNE      9999
117000000927     C                   MOVEL     'Y'           ARBICA
117100000927    3C                   ENDIF
117200000927    2C                   ENDIF
117300000927    1C                   ENDIF
117400000927     C                   ENDSR
117500930628     C*---------------------------------------------------------------*
117600020611     C* ROUTINE DI CARICAMENTO TABELLE                                *
117700930628     C*---------------------------------------------------------------*
117800930507     C     CARTAB        BEGSR
117900000927     C*
118000000927     C* CARICO I TIPI BOLLA DOPPI
118100000927     C                   CLEAR                   YY
118200000927     C                   MOVEL     '3A'          COD
118300000927     C     KTAB          SETLL     TABEL
118400000927     C     KTAB          READE     TABEL                                  30
118500000927     C     *IN30         DOWEQ     *OFF
118600000927     C     TBLFLG        IFEQ      ' '
118700000927     C                   MOVEL     TBLUNI        DS3A
118800000927     C     §3ATB2        IFNE      *BLANKS
118900000927     C                   ADD       1             YY                3 0
119000000927     C                   MOVEL     TBLKEY        CBO(YY)
119100000927     C                   ENDIF
119200000927     C                   ENDIF
119300000927     C     KTAB          READE     TABEL                                  30
119400000927     C                   ENDDO
119500930628     C*
119600101007     c* Carico gli eventi di riserva dalla tabella 2A
119700101007     C                   CLEAR                   YY
119800101007     C                   MOVEL     '2A'          COD
119900101007     C     KTAB          SETLL     TABEL
120000101007     C     KTAB          READE     TABEL                                  30
120100101007     C     *IN30         DOWEQ     *OFF
120200101007     C     TBLFLG        IFEQ      ' '
120300101007     C                   MOVEL     TBLUNI        DS2A
120400101007     C     §2Atip        IFeq      'R'
120500101007     C                   ADD       1             YY                3 0
120600101007     C                   MOVEL     TBLKEY        ris(YY)
120700101007     C                   ENDIF
120800101007     C                   ENDIF
120900101007     C     KTAB          READE     TABEL                                  30
121000101007     C                   ENDDO
121001141209     c* Carico motivi apertura R.A. che si possono chiudere se ancora aperti
121002141209     c                   clear                   yy
121003141209     c     'MAR'         setll     tntbe01l
121004141209     c     'MAR'         reade     tntbe01l
121005141209     c                   dow       not %eof(tntbe01l)
121006141209     c                   movel     tbeuni        dmar
121007141209     c                   if        §MARCANPA='S'
121008141209     c                   eval      yy+=1
121009141209     c                   movel     tbeke1        mach(yy)
121010141209     c                   endif
121011141209     c     'MAR'         reade     tntbe01l
121012141209     c                   enddo
121100930507     C                   ENDSR
121200020404     C*---------------------------------------------------------------*
121300020404     C* OVRPRT - Operazioni su file printer                           *
121400020404     C*---------------------------------------------------------------*
121500020404     C     ovrprt        BEGSR
121600020404     c* override su file printer + apertura
121700020404     c     wopn          ifeq      *blanks
121800020409     C                   MOVEl     cmd(1)        COMMAN          100
121900020409     c                   eval      %subst(cmd(2):6:4) = whld
122000020409     c                   eval      %subst(cmd(2):17:4) = wsav
122100020409     c                   eval      comman = %trim(comman) + '('
122200020409     c                             + %trim(§5alib) + '/'
122300020409     c                             + %trim(§5aotq) + ') ' + %trim(cmd(2))
122400020409     C                   Z-ADD     100           LUNGH
122500020404     C                   CALL      'QCMDEXC'
122600020404     C                   PARM                    COMMAN
122700020404     C                   PARM                    LUNGH            15 5
122800020404     c                   open      fnlr84p
122900020404     c* valorizzazione campi di stampa testata
123000020404     c* data bolle lnp italia
123100020404     c     *ymd          move      comdpb        data_dmy
123200020404     c                   move      data_dmy      dtpita
123300020404     c* data bolle import
123400020404     c     *ymd          move      compbe        data_dmy
123500020404     c                   move      data_dmy      dtpexp
123600020404     c* data bolle con riserva
123700020404     c     *ymd          move      comarr        data_dmy
123800020404     c                   move      data_dmy      dtprsv
123900020404     c* data bolle con c.a.
124000020404     c     *ymd          move      combca        data_dmy
124100020404     c                   move      data_dmy      dtpcan
124200020404     c* Bolle poste: data bolle lnp italia
124300120220     c***  *ymd          move      posdpb        data_dmy
124400120220     c***                move      data_dmy      dppita
124500020404     c*              data bolle import
124600120220     c***  *ymd          move      pospbe        data_dmy
124700120220     c***                move      data_dmy      dppexp
124800020404     c*              data bolle con riserva
124900120220     c***  *ymd          move      posarr        data_dmy
125000120220     c***                move      data_dmy      dpprsv
125100020404     c*              data bolle con c.a.
125200120220     c**   *ymd          move      posbca        data_dmy
125300120220     c***                move      data_dmy      dppcan
125400020404     c*
125500020404     c                   write     lr84t1
125600020404     c                   movel     '1'           wopn
125700020404     c                   endif
125800020404     c* stampa intestazione a rottura
125900020404     c   99              write     lr84t1
126000020404     c                   setoff                                       99
126100020404     c                   endsr
126200020404     C*---------------------------------------------------------------*
126300020404     C* RIED1  - Valorizzazione campi di stampa dettaglio             *
126400020404     C*---------------------------------------------------------------*
126500020404     C     RIED1         BEGSR
126600020404     c                   z-add     arblnp        p1clnp
126700020404     c                   z-add     arblna        p1clna
126800020404     c                   z-add     arbnrs        p1cnrs
126900020404     c                   z-add     arbnsp        p1cnsp
127000080515     c                   MOVEL     arbCBO        p1cCBO
127100080515     c                   MOVEL     arbCCA        p1cCCA
127200020404     c     *ymd          move      arbdsp        data_EUR
127300020404     c                   move      data_EUR      p1cdsp
127400080519     c                   if        arbdcm>0
127500080515     c     *ymd          move      arbDCM        data_EUR
127600080515     c                   move      data_EUR      p1cDCM
127700080519     c                   else
127800080519     c                   clear                   p1cdcm
127900080519     c                   endif
128000020404     c                   endsr
128100010508     C*---------------------------------------------------------------*
128200010508     C* ROUTINE INIZIALE
128300010508     C*---------------------------------------------------------------*
128400010508     C     *inzsr        BEGSR
128500010508      *
128600010508     C     *ENTRY        PLIST
128700010508     C                   PARM                    KPJBA
128800010508     C                   MOVEL     KPJBU         PARAM
128900010508     C                   Z-ADD     1             CODUT
129000010508     C                   CALL      'X§PARUT'
129100010508     C                   PARM                    UT§DSE
129200010508     C                   MOVEL     REC80         CNCR80
129300020404     c                   movel     ragut         rsut
129400020404     C*
129500020404     C                   TIME                    wtime            14 0
129600020404     C                   MOVE      WTIME         WDATE             8 0
129700020409     c     *eur          move      wdate         wdate_inv
129800120216     c     *eur          move      wdate         wdateu
129900010508     C*---------------------------------------------------------------*
130000010508     C     KTAB          KLIST
130100010508     C                   KFLD                    CODUT
130200010508     C                   KFLD                    COD
130300020409     C     KTAB1         KLIST
130400020409     C                   KFLD                    CODUT
130500020409     C                   KFLD                    COD
130600020409     c                   kfld                    key
130700010508     C     KARB          KLIST
130800010508     C                   KFLD                    ARBAAS
130900010508     C                   KFLD                    ARBLNP
131000010508     C                   KFLD                    ARBNRS
131100010508     C                   KFLD                    ARBNSP
131101131029     C     KARB2f        KLIST
131103131029     C                   KFLD                    EVBAAS
131104131029     C                   KFLD                    EVBLNP
131105131029     C                   KFLD                    EVBNRS
131106131029     C                   KFLD                    EVBNSP
131108131029     C                   KFLD                    EVBDTV
131109131029     C                   KFLD                    EVBORV
131110131029     C                   KFLD                    EVBCEV
131200080711     C     KAR4          KLIST
131300080711     C                   KFLD                    ARBAAS
131400080711     C                   KFLD                    ARBLNP
131500080711     C                   KFLD                    ARBNRS
131600080711     C                   KFLD                    ARBNSP
131700080711     C                   KFLD                    ktrc
131800010508     C     KARB7         KLIST
131900010508     C                   KFLD                    ARBAAS
132000010508     C                   KFLD                    ARBLNP
132100010508     C                   KFLD                    ARBNRS
132200010508     C                   KFLD                    ARBNSP
132300010508     C                   KFLD                    AR6TRC
132400010508     C     KARB2         KLIST
132500010508     C                   KFLD                    FIL
132600010508     C                   KFLD                    WDCM
132700010508     C     KART          KLIST
132800010508     C                   KFLD                    ARTFLS
132900010508     C                   KFLD                    ARTLNA
133000010508     C                   KFLD                    ARTNRS
133100010508     C                   KFLD                    ARTNSC
133200010508     C     KEVB4         KLIST
133300010508     C                   KFLD                    ARBAAS
133400010508     C                   KFLD                    ARBLNP
133500010508     C                   KFLD                    ARBNRS
133600010508     C                   KFLD                    ARBNSP
133700010508     C                   KFLD                    WCEV
133800050413
133900050906     C     KGCA          KLIST
134000050906     C                   KFLD                    ARBAAS
134100050906     C                   KFLD                    ARBLNP
134200050906     C                   KFLD                    ARBNRS
134300050906     C                   KFLD                    ARBNSP
134400050906     C                   KFLD                    KFRG
134500050404
134600050906     c     kblp          klist
134700050906     c                   kfld                    GcpAas
134800050906     c                   kfld                    GcpLnp
134900050906     c                   kfld                    GcpNrs
135000050906     c                   kfld                    GcpNsp
135100020530     c*
135200020530     c     kdcf          klist
135300020530     c                   kfld                    dctaac
135400020530     c                   kfld                    dctfil
135500020530     c                   kfld                    dctnca
135600010508      *
135700101007     C****               MOVEL     'R  '         WCEV
135800050906     C                   clear                   kfrg
135900040423     c*  File Fitgd02l    RECLAMI
136000040423     c     ktgd          klist
136100040423     c                   kfld                    ktgtor
136200040423     c                   kfld                    ktgogg
136201141209     C     Kidc          KLIST
136202141209     C                   KFLD                    ARBAAS
136203141209     C                   KFLD                    ARBLNP
136204141209     C                   KFLD                    ARBNRS
136205141209     C                   KFLD                    ARBNSP
136206141209     C                   KFLD                    weladata
136300040423     C*
136400010508      *
136500010508      * DEFINIZIONE CAMPI
136600010508     C     *LIKE         DEFINE    EVBCEV        WCEV
136700010508     C     *LIKE         DEFINE    KPJBU         WSAJBU
136800010508     C     *LIKE         DEFINE    ARBDCM        WDCM
136900010508     C     *LIKE         DEFINE    AR4TRC        WTRC
137000010508     C     *LIKE         DEFINE    DCTDCH        WDCH
137100020530     C     *LIKE         DEFINE    DCTDCH        WDCH_dcb
137200010508     C     *LIKE         DEFINE    ARBLNP        SAVLNP
137300010508     C     *LIKE         DEFINE    TBLCOD        COD
137400020409     C     *LIKE         DEFINE    TBLKEY        KEY
137500050906     C     *LIKE         DEFINE    GCPFRG        KFRG
137600020207     C     *LIKE         DEFINE    §ogntw        lnantw
137700040831     C     *LIKE         DEFINE    §ogntw        lnpntw
137800040831     c*
137900040831      * Se sono in ambiente buono - GAITRAGRU
138000040831     c                   If        knsif = 'FILTRA201 '
138100040831     c                   Eval      wlib = 'GAITRAGRU '
138200040831      *  Se sono in ambiente di prova - GAITRAGRPS
138300040831     c                   Else
138400040831     c                   Eval      wlib = 'GAITRAGRPS'
138500040831     c                   EndIf
138600040831     c*
138700040831     c                   Clear                   commant
138800040831     c                   Movel(p)  cmdt(1)       commant
138900040831     c                   Eval      %Subst(commant:30:10) = wlib
139000040831     c                   Eval      commant =
139100040831     c                             %trim(commant) + '/TITAS30C)'
139200040831     c                   Call      'QCMDEXC'                            99
139300040831     c                   Parm                    commant
139400040831     c                   Parm                    lenght
139500040831     c                   Open      Titas30c
139600060123     c*
139700060123     c                   Clear                   commant
139800060123     c                   Movel(p)  cmdt(2)       commant
139900060123     c                   Eval      %Subst(commant:30:10) = wlib
140000060123     c                   Eval      commant =
140100060123     c                             %trim(commant) + '/TNCSB03L)'
140200060123     c                   Call      'QCMDEXC'                            99
140300060123     c                   Parm                    commant
140400060123     c                   Parm                    lenght
140500060123     c                   Open      TNCSB03L
140600101007     c
140700020409     c*
140800020409     c                   exsr      caldts
140900010508      *
141000010508     C                   ENDSR
141100020409     C*---------------------------------------------------------------*
141200020409     C* caldts - Calcolo date stampa                                  *
141300020409     C*---------------------------------------------------------------*
141400020409     C     CALDTS        BEGSR
141500020409      * LETTURA giorni per date stampa e coda di output
141600020409     C                   MOVEL     '5A'          COD
141700020409     C                   MOVEL     '2       '    KEY
141800020409     C     ktab1         CHAIN     taBEL00F                           30
141900020409     C  N30              MOVEL     TBLUNI        DS5A2
142000020409     C   30              CLEAR                   DS5A2
142100020409     C*
142200020409     C                   MOVEL     '5A'          COD
142300020409     C                   MOVEL     '3       '    KEY
142400020409     C     ktab1         CHAIN     TABEL00F                           30
142500020409     C  n30              MOVEL     TBLUNI        DS5A3
142600020409     C   30              CLEAR                   DS5A3
142700020409     c     §5ahld        ifeq      'S'
142800020409     c                   movel     '*YES'        whld              4
142900020409     c                   else
143000020409     c                   movel     '*NO '        whld
143100020409     c                   endif
143200020409     c     §5asav        ifeq      'S'
143300020409     c                   movel     '*YES'        wsav              4
143400020409     c                   else
143500020409     c                   movel     '*NO '        wsav
143600020409     c                   endif
143700020409     c* BOLLE NO POSTE
143800020409     c* 1) data stampa bolle lna italia
143900020409     c     *ymd          move      comdpb        data_iso
144000020409     c                   subdur    §5agal:*d     data_iso
144100020409     c                   move      data_iso      dtsita_ymd
144200020409     c                   move      data_iso      data_dmy
144300020409     c                   move      data_dmy      dtsita
144400020409     c* 2) data stampa bolle export
144500020409     c     *ymd          move      compbe        data_iso
144600020409     c                   subdur    §5agal:*d     data_iso
144700020409     c                   move      data_iso      dtsexp_ymd
144800020409     c                   move      data_iso      data_dmy
144900020409     c                   move      data_dmy      dtsexp
145000020409     c* 3) data stampa bolle con riserva
145100020409     c     *ymd          move      comarr        data_iso
145200020409     c                   subdur    §5agal:*d     data_iso
145300020409     c                   move      data_iso      dtsrsv_ymd
145400020409     c                   move      data_iso      data_dmy
145500020409     c                   move      data_dmy      dtsrsv
145600020409     c* 4) data stampa bolle con c.a.
145700020409     c     *ymd          move      combca        data_iso
145800020409     c                   subdur    §5agcn:*d     data_iso
145900020409     c                   move      data_iso      dtscan_ymd
146000020409     c                   move      data_iso      data_dmy
146100020409     c                   move      data_dmy      dtscan
146200120222     c                   move      data_dmy      dtsncc
146300020409     c* BOLLE POSTE
146400020409     c* 1) data stampa bolle lna italia
146500020409     c     *ymd          move      posdpb        data_iso
146600020409     c                   subdur    §5agal:*d     data_iso
146700020409     c                   move      data_iso      dpsita_ymd
146800120220     c***                move      data_iso      data_dmy
146900120220     c****               move      data_dmy      dpsita
147000020409     c* 2) data stampa bolle export
147100020409     c     *ymd          move      pospbe        data_iso
147200020409     c                   subdur    §5agal:*d     data_iso
147300020409     c                   move      data_iso      dpsexp_ymd
147400120220     c***                move      data_iso      data_dmy
147500120220     c***                move      data_dmy      dpsexp
147600020409     c* 3) data stampa bolle con riserva
147700020409     c     *ymd          move      posarr        data_iso
147800020409     c                   subdur    §5agal:*d     data_iso
147900020409     c                   move      data_iso      dpsrsv_ymd
148000120220     c***                move      data_iso      data_dmy
148100120220     c***                move      data_dmy      dpsrsv
148200020409     c* 4) data stampa bolle con c.a.
148300020409     c     *ymd          move      posbca        data_iso
148400020409     c                   subdur    §5agcn:*d     data_iso
148500020409     c                   move      data_iso      dpscan_ymd
148600120220     c****               move      data_iso      data_dmy
148700120220     c****               move      data_dmy      dpscan
148800120220     c******             move      data_dmy      dpsncc
148900020409     c*
149000020409     c* Data per stampare le spedizioni non consegnate
149100020409     c                   subdur    §5agsn:*d     wdate_inv
149200020409     c                   move      wdate_inv     dtsncn_ymd
149300020409     c                   move      wdate_inv     data_dmy
149400020409     c                   move      data_dmy      dtsncn
149500120220     c****               move      data_dmy      dpsncn
149600120220
149700120220     c                   move      wdateu        data_iso
149800120220     c                   subdur    §5agdc:*d     data_iso
149900120220     c                   move      data_iso      dtsgdc_ymd
150000120220     c                   move      data_iso      data_dmy
150100120222     c                   move      data_dmy      dtsnccd
150200020409     c                   endsr
150300020404**
150400020409OVRPRTF FNLR84P OUTQ
150500120220HOLD(    ) SAVE(    ) USRDTA('PulBoArr')
150600040831** cmdt
150700040831OVRDBF FILE(TITAS30C) TOFILE(
150800060123OVRDBF FILE(TNCSB03L) TOFILE(
