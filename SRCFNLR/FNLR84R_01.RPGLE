000100150928     /*CMD addlible lib(GAITRAGRPS) position(*last)
000200150928     /*PRM dbgview(*source)
000300150928     /*END rmvlible lib(GAITRAGRPS)
000400150928     /*END
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600941212     H* FNLR84R *-----------------------------------------------------*
000700930628     H*                 - PULIZIA BOLLE ARRIVI -                      *
000800040831      *
000900040831      * COMPILARE CON
001000040831      *               OVRDBF FILE(TITAS30C) TOFILE(GAITRAGRPS/TITAS30C)
001100060123      *               OVRDBF FILE(TNCSB03L) TOFILE(GAITRAGRPS/TNCSB03L)
001200930628     H*---------------------------------------------------------------*
001300971002     FFNBLP01L  IF   E           K DISK
001400000927     FTABEL00F  IF   E           K DISK
001500141209     FTntbe01l  IF   E           K DISK
001600980722     FAZORG01L  IF   E           K DISK
001700060502     FFNARB54L  IF   E           K DISK    infds(arbd1)
001800060502     FFNARB00F  UF   E             DISK    rename(fnarb000:fnarbfis)
001900941207     F*
002000941207     F* ESTENSIONE DESCRIZIONI
002100060220     FFiAR401L  UF   E           K DISK
002200030108     F*
002300030108     F* ESTENSIONE TESTATA BOLLA
002400030108     FFIAR501L  UF   E           K DISK
002500040604     FFIAR801L  UF   E           K DISK
002600941207     F*
002700941207     F* TASSAZIONE 1 BOLLA
002800991104     FFIAR601L  UF   E           K DISK
002900941207     F*
003000941207     F* ESTENSIONE TASSAZIONE 1 BOLLA
003100991104     FFIAR701L  UF   E           K DISK
003200941207     F*
003300941207     F* C/ASSEGNO
003400051115     FFiAR901L  UF   E           K DISK
003500991104     F**
003600991104     FFIARI01L  UF   E           K DISK
003700061107     F**
003800061107     FFIARG01L  UF   E           K DISK
003900070503     FFIARP01L  UF   E           K DISK
004000070503     FFIARN01L  UF   E           K DISK
004100140214     FFIsia01L  UF   E           K DISK
004200941207     F*
004300941207     F* ARCHIVIO VARIAZIONI BOLLE
004400131014     FFIARBF2C  UF   E           K DISK
004500991104     FFIARBU1L  UF   E           K DISK
004600941207     F*
004700941207     F* DETTAGLIO COLLI
004800941207     FFNART01L  UF   E           K DISK
004900051012     FFIARS01L  UF   E           K DISK
005000941207     F*
005100941207     F* ARCHIVIO EVENTI
005200990806     FFNEVB01L  UF   E           K DISK
005300990806     FFNEVB04L  IF   E           K DISK
005400990806     F                                     RENAME(FNEVB000:FNEVB004)
005500131029     FFNEVB22L  UF   E           K DISK
005600941207     F*
005700961125     F* ARCHIVI  EVENTI SEGNACOLLI
005800961125     FFNEVS01L  UF   E           K DISK
005900961125     FFNEVS02L  UF   E           K DISK
006000961125     F                                     RENAME(FNEVS000:FNEVS002)
006100961125     F*
006200941207     F* ARCHIVIO BOLLE GIACENZA IN ARRIVO
006300050906     FTigcp21L  IF   E           K DISK
006400941207     F*
006500950110     F* ARCHIVI  PADRONCINI DETTAGLIO TASSAZIONE
006600060206     Ffiftd22L  IF   E           K DISK
006700011121     Ffifsd02L  IF   E           K DISK
006800011121     F                                     RENAME(fiftd000:fifsd000)
006900990126     F* ARCHIVI  C.A.
007000020530     FFndcf01l  IF   E           K DISK
007100020530     FFndfa01l  IF   E           K DISK
007200040423     F
007300040423     F* ARCHIVI  RECLAMI
007400040423     Ffitgd02l  IF   E           K DISK
007500141209     F* Disposizioni di consegna
007600141209     Ftiidc01l  uF   E           K DISK
007700040831      * Bolle sede
007800040831     fTitas30c  if   e           k Disk    UsrOpn
007900060123     F* ARCHIVIO Contrassegni di sede
008000060123     FTNCSB03L  IF   E           K DISK    usropn
008100040423     F*
008200020404     Ffnlr84p   O    E             PRINTER USROPN oflind(*in99)
008300950110     D*
008400060502     D arbD1           DS
008500060502     D  ARBNRR               397    400B 0
008600060502     d
008700000927     D CBO             S              2    DIM(50)                              COD BOLLA DOPP
008800101007     D ris             S              3    DIM(50)                              COD BOLLA DOPP
008900141209     d mach            s              3    dim(50)
009000020409     d cmd             s             50    dim(2) CTDATA PERRCD(1)
009100060123     d cmdt            s             60    dim(02) ctdata perrcd(1)
009200020404     D*
009300020404     d data_EUR        s               d   datfmt(*EUR)
009400020404     d data_DMY        s               d   datfmt(*DMY)
009500020409     d data_ISO        s               d   datfmt(*ISO)
009600020409     d wdate_inv       s               d   datfmt(*ISO)
009700120216     d wdateu          s               d   datfmt(*ISO)
009800020404     d data_Alfa       s             10a
009900020409     d dtsita_ymd      s              8  0
010000020409     d dtsexp_ymd      s              8  0
010100020409     d dtsrsv_ymd      s              8  0
010200020409     d dtscan_ymd      s              8  0
010300020409     d dtsncn_ymd      s              8  0
010400120220     d dtsgdc_ymd      s              8  0
010500020409     d dpsita_ymd      s              8  0
010600020409     d dpsexp_ymd      s              8  0
010700020409     d dpsrsv_ymd      s              8  0
010800020409     d dpscan_ymd      s              8  0
010900020404     d wopn            s              1a
011000040812     d dctdch          s              8  0
011100040812     d dctcch          s              2a
011200040831     d wlib            s             10
011300040831     d commant         s             80
011400040831     d lenght          s             15  5 inz(80)
011500040423
011600040423     d ktgtor          s                   like(tgdtor) inz('S')
011700040423     d ktgogg          s                   like(tgdogg)
011800080711     d ktrc            s                   like(ar4trc)
011900080711     d W008a           s              8
012000080711     d W0080           s              8  0
012100080711     d Indx            s              3  0
012200141209     d weladata        s                   like(idceladata) inz(0)
012300040423
012400941207     D*
012500961204     D* PARAMETRI RICEVUTI DAI PGM CHIAMANTI
012600900618     D PARAM           DS
012700961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA NON ESTERA)
012800941207     D  COMDPB                17     24  0
012900961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA ESTERA - BOLLE EXPORT)
013000961204     D  COMPBE                68     75  0
013100980722     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA
013200990707     D  COMARR               191    198  0
013300990126     D* DATA PULIZIA BOLLE ARRIVI CON C.A.
013400990707     D  COMBCA               199    206  0
013500000530     D* DATA PULIZIA BOLLE ARRIVI - POSTE
013600000530     D  POSDPB               207    214  0
013700000530     D* DATA PULIZIA BOLLE ARRIVI - BOLLE EXPORT - POSTE
013800000530     D  POSPBE               215    222  0
013900000530     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA - POSTE
014000000530     D  POSARR               223    230  0
014100000530     D* DATA PULIZIA BOLLE ARRIVI CON C.A. - POSTE
014200000530     D  POSBCA               231    238  0
014300040423     D* DATA PULIZIA BOLLE CON RECLAMI
014400040423     D  COMREC               239    246  0
014500961204     D*
014600920324     D                 DS
014700941207     D  ARBAAS                 1      4  0
014800941207     D  ARBMGS                 5      8  0
014900941207     D  ARBDSP                 1      8  0
015000020405     D                 DS
015100040813     D  dctAAc2                1      4  0
015200020405     D  dctmgc                 5      8  0
015300020405     D  dctdca                 1      8  0
015400941207     D*
015500990823     D* DS PER TIBS02DS- RICERCA TABELLE
015600990823     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015700990823     D DSDN11        E DS                  EXTNAME(FIDN11DS)
015800941207     D*
015900000530     D OG143         E DS
016000101007     D DS2A          E DS
016100101007     D DS3A          E DS
016200020409     D DS5A2         E DS
016300020409     D DS5A3         E DS
016400141209     D Dmar          E DS
016500000000     D KPJBA         E DS
016600930507     D CNCR80        E DS
016700930507     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
016800050404     D FNLGZ5DS      E DS
016900040812      * ds per il controllo della presenza di CA per la spedizione richiesta
017000040812     d FIDN12DS      e ds
017100040812     d  skKey                 26    305    dim(20)
017200040812     d  skAnn                306    325    dim(20)
017300040812     d  skDca                326    485  0 dim(20)
017400040812     d  skFca                486    545  0 dim(20)
017500040812     d  skDch                546    705  0 dim(20)
017600040812     d  skCch                706    745    dim(20)
017700141209
017800150928     d FIDNCADS      e ds                  inz
017900040812
018000040812     d dsKey           ds
018100040812     d  dctaac                        4  0
018200040812     d  dctfil                        3  0
018300040812     d  dctnca                        7  0
018400040812      *
018500040423     D*-------------
018600040423     D* DS OGGETTO RECLAMO
018700040423     D*-------------
018800040423     D                 DS
018900040423     D  W_lnp                         3  0
019000040423     D  W_nrs                         2  0
019100040423     D  W_nsp                         7  0
019200040423     D  W_aas                         4  0
019300040423     D  Wspe                   1     16  0 inz
019400020404     D*
019500020404     D* DEFINIZIONE COSTANTI
019600020404     D*
019700020530     D CBGIAC          C                   CONST('Bolla giacente in fase')
019800020404     D CBRISE          C                   CONST('Bolla con riserva consegnata -
019900020404     D                                     il            ')
020000020530     D CBCANA          C                   CONST('Bolla con C.A. aperta - Fase')
020100020404     D CBCANC          C                   CONST('Bolla con C.A. chiusa il')
020200020404     D CBCANN1         C                   CONST('Presente C.A. chiusa il ')
020300020404     D CBCANN2         C                   CONST(' non ancora stampata nell''el-
020400020404     D                                     enco C.A. da archiviare')
020500080711     D CBcass          C                   CONST('C/Assegno non incassato')
020600020405     D CBercan         C                   CONST('Errore in cancellazione C.A. -
020700020405     D                                     n.')
020800020405     D CBcpdr          C                   CONST('Bolla presente su archivio co-
020900020404     D                                     nteggi padroncini')
021000080515     D CBChoprN        C                   CONST('Manca numero MANDATO INTROITO-
021100080515     D                                      ma c''è data')
021200080515     D CBChoprD        C                   CONST('Chiusura operatore non effett-
021300080515     D                                     uata')
021400020405     D CBinc           C                   CONST('Spedizione ancora da incassar-
021500020405     D                                     e')
021600020409     D CBncon          C                   CONST('Bolla non consegnata')
021700020411     D CBnconca        C                   CONST('Bolla non consegnata con C.A.-
021800080515     D                                     chiusa')
021900080515     D CBcrec          C                   CONST('Bolla con Reclamo APERTO')
022000040831     D CBdt1           C                   CONST('Bolla con data trasmissione <-
022100040831     D                                     data mandato di introito')
022200010508      *
022300941207     IFNARBD00      01
022400941207     IFNARBK00      02
022500991104     IFIARBT00      03
022600991104     IFNARBP00      04
022700941207     IFNARBG00      05
022800941207     IFNARBM00      06
022900941207     IFNARBV00      07
023000131014     IFNARBN00      08
023100930507     C*---------------------------------------------------------------*
023200950516     C* MI SALVO LA KPJBU
023300950516     C                   MOVEL     KPJBU         WSAJBU
023400950516     C*
023500020611     C* CARICO TABELLE
023600930507     C                   EXSR      CARTAB
023700930507     C*
023800920306     C                   Z-ADD     0             FIL               3 0
023900950526     C                   Z-ADD     1             WDCM
024000010508     C                   eval      *in30 = *off
024100920306     C*
024200941207    1C     *IN30         DOWEQ     *OFF
024300941207     C     FIL           SETGT     FNARB000
024400941207     C                   READ      FNARB000                               30
024500920306     C*
024600941207    2C     *IN30         IFEQ      *OFF
024700000724     C                   CLEAR                   SAVLNP
024800920324     C                   Z-ADD     ARBLNA        FIL
024900980722     C     ARBLNA        CHAIN     AZORG01L                           35
025000020207     C  N35              MOVE      ORGde3        og143
025100020207     C  n35              movel     §ogntw        lnantw
025200020207     C   35              CLEAR                   lnantw
025300000809     C*
025400020207     C     lnantw        IFEQ      'EEX'
025500020207     C     lnantw        OREQ      'EUP'
025600020207     C     lnantw        OREQ      'FED'
025700020211     C     lnantw        OREQ      'DPD'
025800000809     C     COMPBE        IFGT      POSPBE
025900000809     C                   Z-ADD     COMPBE        WDACNF            8 0
026000000809     C                   ELSE
026100000809     C                   Z-ADD     POSPBE        WDACNF
026200000809     C                   ENDIF
026300000809     C                   ELSE
026400000809     C     COMDPB        IFGT      POSDPB
026500000809     C                   Z-ADD     COMDPB        WDACNF            8 0
026600000809     C                   ELSE
026700000809     C                   Z-ADD     POSDPB        WDACNF
026800000809     C                   ENDIF
026900000809     C                   ENDIF
027000920306     C*
027100950526     C     KARB2         SETLL     FNARB000
027200941207     C     FIL           READE     FNARB000                               31
027300920306     C*
027400941207    3C     *IN31         DOWEQ     *OFF
027500000724     C     ARBLNP        IFNE      SAVLNP
027600000724     C                   MOVE      ARBLNP        SAVLNP
027700020207     C                   MOVE      '   '         lnpntw
027800000724     C     ARBLNP        CHAIN     AZORG01L                           35
027900000724     C  N35              MOVEL     ORGDE3        OG143
028000020207     C  N35              MOVEL     §ogntw        lnpntw
028100000724     C                   ENDIF
028200020211     c* pulizia bolle estere COMPRESA LA DPD
028300020207     C     lnantw        IFEQ      'EEX'
028400020207     C     lnantw        OREQ      'EUP'
028500020207     C     lnantw        OREQ      'FED'
028600020211     C     lnantw        OREQ      'DPD'
028700000724     C* DATA PULIZIA BOLLE EXPORT (POSTE O NON POSTE)
028800000724     C                   Z-ADD     COMPBE        WDAPUL            8 0
028900020409     C                   Z-ADD     dtsexp_ymd    WDAprt            8 0
029000000724     C                   ELSE
029100000724     C* DATA PULIZIA BOLLE ITALIE E IMPORT (POSTE O NON POSTE)
029200020207     C     lnpntw        IFNE      'PPT'
029300000724     C                   Z-ADD     COMDPB        WDAPUL
029400020409     C                   Z-ADD     dtsita_ymd    WDAprt            8 0
029500000724     C                   ELSE
029600000724     C                   Z-ADD     POSDPB        WDAPUL
029700020409     C                   Z-ADD     dpsita_ymd    WDAprt            8 0
029800000724     C                   ENDIF
029900000724     C                   ENDIF
030000930628     C*
030100950526     C* CONSIDERO SOLO LE BOLLE
030200961204     C*
030300000809     C* SE DATA CONSEGNA  > DATA PULIZIA PIU' ALTA LEGGO ALTRA LINEA
030400000809    5C     ARBDCM        IFGT      WDACNF
030500920306     C                   SETON                                        31
030600941207   X5C                   ELSE
030700000809     C* ALTRIMENTI ELABORO SE DATA CONSEGNA <= DATA PULIZIA
030800000809     C* SE INVECE E' MAGGIORE IGNORO QUESTO RECORD
030900000809   5AC     ARBDCM        IFLE      WDAPUL
031000930507     C*
031100970623     C                   CLEAR                   WOK
031200000530     C*
031300000530     C* IMPOSTO DATA A SECONDA SE E' POSTE O NO
031400020207     C     lnpntw        IFNE      'PPT'
031500000530     C                   Z-ADD     COMARR        WDATA£            8 0
031600020409     C                   Z-ADD     dtsrsv_ymd    WDApr£            8 0
031700000530     C                   ELSE
031800000530     C                   Z-ADD     POSARR        WDATA£            8 0
031900020409     C                   Z-ADD     dpsrsv_ymd    WDApr£            8 0
032000000530     C                   ENDIF
032100980722     C**
032200980722     C* SE BOLLA CON RISERVA PULISCO CONSIDERANDO ANCHE L'ALTRA DATA
032300101007     c                   setoff                                       33
032400101007     c                   z-add     1             yy
032500101007     c                   dow       ris(YY)<>*blanks
032600101007     c                   eval      wcev=ris(YY)
032700101007     c
032800101007     C     KEVB4         SETLL     FNEVB04L
032900101007     c                   if        %equal(fnevb04l)
033000101007     c                   seton                                        33
033100101007     c                   leave
033200101007     c                   else
033300101007     c                   endif
033400101007     c                   add       1             yy
033500101007     c                   enddo
033600101007     c
033700980722     C     *IN33         IFEQ      *ON
033800000530     C     ARBDCM        ANDGE     WDATA£
033900020404     c* stampo motivazione
034000020409     c     arbdsp        iflt      wdapr£
034100020404     c                   exsr      ovrprt
034200020404     c     *ymd          move      arbdcm        data_EUR
034300020404     c                   move      data_EUR      data_Alfa
034400020404     c                   movel(P)  cbrise        p01mes
034500020404     c                   eval      p01mes = %trim(p01mes) + ' ' + data_Alfa
034600020404     c                   exsr      ried1
034700020404     c                   write     lr84d1
034800020409     c                   endif
034900980722     C                   MOVE      'N'           WOK               1
035000980722     C                   ENDIF
035100990126     C**
035200990126     C* SE BOLLA CON C.A. PULISCO PER DATA CHIUSURA C.A.
035300990126     C                   CLEAR                   WDCH
035400020530     C                   CLEAR                   WDCH_dcb
035500020409     C* IMPOSTO DATA stampa A SECONDA SE E' POSTE O NO
035600020409     C     lnpntw        IFNE      'PPT'
035700020409     C                   Z-ADD     dtscan_ymd    WDApr£
035800020409     C                   ELSE
035900020409     C                   Z-ADD     dpscan_ymd    WDApr£
036000020409     C                   ENDIF
036100020409     c*
036200040812      * Sostituisco la lettura del file FNDTC02 con
036300040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
036400040812      * legate alla spedizione che passo
036500040812      *
036600040812     c                   clear                   fidn12ds
036700040812     c                   eval      i12aas = arbaas
036800040812     c                   eval      i12lnp = arblnp
036900040812     c                   eval      i12nrs = arbnrs
037000040812     c                   eval      i12nsp = arbnsp
037100040812     c                   eval      i12fel = 'E'
037200040812     c                   eval      i12fan = 'E'
037300040812      *
037400040812     c                   call      'FIDN12R'
037500040812     c                   parm                    fidn12ds
037600040812      *
037700040812      * se non ci sono errori
037800040812    6c                   if        o12err = ' '
037900040812      * se numero di CA trovate maggiore di zero
038000040812    7c                   if        o12nca > 0
038100040812     c
038200040812     c                   z-add     *zeros        ii                2 0
038300040812    8c     1             do        o12nca        ii
038400040812     c                   movea     skkey(ii)     dskey
038500040812     C                   Z-ADD     skdca(ii)     dctdca
038600040812     C                   Z-ADD     skdch(ii)     dctdch
038700040812     C                   movea     skcch(ii)     dctcch
038800040812      * se CA aperta stampo,imposto il flag e vado a fine
038900040812    9c                   If        dctdch = 0
039000040812     c* stampo motivazione
039100080325   10c**   arbdsp        iflt      wdapr£
039200080325     c**                 exsr      ovrprt
039300080325     c**                 movel(P)  cbcana        p01mes
039400040812     c* chaino file delle fasi per reperire fase, data, e p.o.
039500080325     c**   kdcf          setgt     fndcf01l
039600080325     c**   kdcf          readpe    fndcf01l
039700080325   11c**                 if        not %eof(fndcf01l)
039800080325     c**   *ymd          move      dcfdfc        data_EUR
039900080325     c**                 move      data_EUR      data_Alfa
040000040812     c* chaino file anagrafica fasi per decodificare la fase
040100080325     c**                 clear                   fndfa000
040200080325     c**   dcffca        chain     fndfa01l
040300080325   12c**                 if        %found(fndfa01l)
040400080325     c**                 movel     dfades        wfades           40
040500080325   12c**                 endif
040600080325  x11c**                 else
040700080325     c**                 clear                   data_Alfa
040800080325     c**                 clear                   dcffca
040900080325     c**                 clear                   dcffev
041000080325   11c**                 endif
041100080325     c**                 move      dcffca        wcffca            3
041200080325     c**                 move      dcffev        wcffev            3
041300080325     c**                 eval      p01mes = %trim(p01mes) + ' ' + wcffca
041400080325     c**                           + ' ' + wfades
041500080325     c**                           + ' del ' + data_Alfa + ' p.o. ' + wcffev
041600080325     c**                 exsr      ried1
041700080325     c**                 write     lr84d1
041800080325   10c**                 endif
041900040812     C                   MOVE      'N'           WOK               1
042000040812     c                   leave
042100040812  x 9c                   else
042200040812      * se CA chiusa
042300040812   10C     DCTDCH        IFGT      WDCH
042400040812     C                   Z-ADD     DCTDCH        WDCH
042500040812   10C                   ENDIF
042600040812     c* memorizzo la data chiusura più alta se p.o. C.A. = lna bolla
042700040812   10c     dctdch        ifgt      wdch_dcb
042800040812     c     dctfil        andeq     arblna
042900040812     c                   z-add     dctdch        wdch_dcb
043000040812   10c                   endif
043100040812    9C                   ENDIF
043200040812     c
043300040812    8c                   enddo
043400040812      *
043500040812    7c                   endif
043600040812    6c                   endif
043700040812     C*
043800000530     C*
043900000530     C* IMPOSTO DATA A SECONDA SE E' POSTE O NO
044000020207     C     lnpntw        IFNE      'PPT'
044100000530     C                   Z-ADD     COMBCA        WDATA£            8 0
044200000530     C                   ELSE
044300000530     C                   Z-ADD     POSBCA        WDATA£            8 0
044400000530     C                   ENDIF
044500990126     C**
044600990126     C* SE C.A. TUTTE CHIUSE DEVONO ESSERE PASSATI I GIORNI DALLA DATA
044700990126     C*  CHIUSURA PIU' ALTA
044800990126     C     WOK           IFEQ      ' '
044900000530     C     WDCH          ANDGE     WDATA£
045000990126     C                   MOVE      'N'           WOK               1
045100990126     C                   ENDIF
045200990823     C* NON POSSO PULIRE BOLLA E C.A. SE NON L'HO ANCORA STAMPATA
045300990823     C* NELL'ELENCO C.A. DA ARCHIVIARE
045400990823    6C     WOK           IFEQ      ' '
045500020530     c     wdch_dcb      andgt     *zeros
045600990823     C                   CLEAR                   DSBS02
045700990824     C                   MOVEL     'C'           T02MOD
045800990823     C                   MOVEL     KNSIF         T02SIF
045900990823     C                   MOVEL     'DCB'         T02COD
046000990823     C                   MOVEL     ARBLNA        T02KE1
046100990823     C                   CALL      'TIBS02R'
046200990823     C                   PARM                    KPJBA
046300990823     C                   PARM                    DSBS02
046400990823    7C     T02ERR        IFEQ      *BLANKS
046500990823     C                   MOVEL     T02UNI        WDTACA            8 0
046600990823     C                   ELSE
046700990823     C                   Z-ADD     19990101      WDTACA
046800990823    7C                   ENDIF
046900020530    7C     WDCH_dcb      IFGT      WDTACA
047000020404     c* stampo motivazione
047100020409     c     arbdsp        iflt      wdapr£
047200020404     c                   exsr      ovrprt
047300020530     c     *ymd          move      wdch_dcb      data_EUR
047400020404     c                   move      data_EUR      data_Alfa
047500020404     c                   eval      p01mes = cbcann1 + data_alfa + cbcann2
047600020404     c                   exsr      ried1
047700020404     c                   write     lr84d1
047800020409     c                   endif
047900990823     C                   MOVE      'N'           WOK
048000990823    7C                   ENDIF
048100990823    6C                   ENDIF
048200990823     C*
048300970623   6AC     WOK           IFEQ      *BLANKS
048400050906      *
048500050906      * GIACENZA: se non trovata CANCELLO BOLLA
048600050906      *           se trovata ma è annullata o in fase 60 CANCELLO BOLLA
048700050906      *           negli altri casi non CANCELLO BOLLA
048800050906     C                   clear                   WGIACE            1
048900050906     C     KGCA          CHAIN     Tigcp21L
049000010508      *
049100050906     c                   SELECT
049200050906     c                   WHEN      not %found(Tigcp21L)
049300050906     c                   eval      *in33 = *off
049400050906     c                   WHEN      GCPATB <> *blanks or GCPFAS = 60
049500050906     c     kblp          Setll     Fnblp01l
049600050906     c                   eval      *in33 = *off
049700110210     c
049800110210     c                   If        Not %Equal(Fnblp01l)
049900050906     c                   Eval      wgiace = 'S'
050000050906     c                   EndIf
050100050906     c                   OTHER
050200020404     c* stampo motivazione
050300020409     c     arbdsp        iflt      wdaprt
050400020404     c                   exsr      ovrprt
050500020404     c                   movel(P)  cbgiac        p01mes
050600020530     c* prendo la decodifica fase
050700020530     C                   MOVEL     '2Y'          COD
050800050906     C                   MOVEL(P)  gcpfas        KEY
050900020530     C     ktab1         CHAIN     TABEL00F                           37
051000020530     C   37              MOVEL     *ALL'?'       decfas           45
051100020530     C  N37              MOVEL     TBLUNI        decfas
051200050906     c                   move      gcpfas        wcpfas            3
051300020530     c                   eval      p01mes = %trim(p01mes) + ' ' + wcpfas + '-'
051400020530     c                             + decfas
051500020404     c                   exsr      ried1
051600020404     c                   write     lr84d1
051700020409     c                   endif
051800050906      * Imposto flag x non pulire la bolla
051900050906     c                   MOVE      'N'           WOK
052000010508     c                   eval      *in33 = *on
052100050906     c                   ENDSL
052200020405     c*
052300020415     c  n33arbnmi        ifeq      *zeros
052400020405     c* stampo motivazione
052500020409     c     arbdsp        iflt      wdaprt
052600020405     c                   exsr      ovrprt
052700080515     c                   if        arbdmi>0
052800080515     c                   movel(P)  cbchoprn      p01mes
052900080515     c                   else
053000080515     c                   movel(P)  cbchoprd      p01mes
053100080515     c                   endif
053200080515     c
053300020405     c                   exsr      ried1
053400020405     c                   write     lr84d1
053500020409     c                   endif
053600020405     c                   else
053700020405     c     arbfip        ifeq      'I'
053800020405     c* stampo motivazione
053900020409     c     arbdsp        iflt      wdaprt
054000020405     c                   exsr      ovrprt
054100020405     c                   movel(P)  cbinc         p01mes
054200020405     c                   exsr      ried1
054300020405     c                   write     lr84d1
054400020409     c                   endif
054500020405     c                   endif
054600020405     c                   endif
054700920410     C*
054800920505     C* PULISCO SOLO SE NON E' DA TORNARE ALL'INCASSO
054900950110     C*   E SE HA GIA' IL MANDATO DI INTROITO
055000000927    6C  N33ARBFIP        IFNE      'I'
055100930308     C     ARBNMI        ANDNE     0
055200000927     C*
055300000927    7C     ARBICA        IFEQ      ' '
055400000927     C     ARBICA        OREQ      'R'
055500000927     C* VERIFICO SE PER CASO SI TRATTA DI UN FINTO RITORNO ALL'INCASSO
055600000927     C                   EXSR      CTRICA
055700000927    7C                   ENDIF
055800020405     c*
055900020405     c     arbica        ifeq      ' '
056000020405     c     arbica        oreq      'R'
056100020405     c* stampo motivazione
056200020409     c     arbdsp        iflt      wdaprt
056300020405     c                   exsr      ovrprt
056400020405     c                   movel(P)  cbinc         p01mes
056500020405     c                   exsr      ried1
056600020405     c                   write     lr84d1
056700020409     c                   endif
056800020405     c                   endif
056900000927     C*
057000000927   7AC     ARBICA        IFNE      ' '
057100000927     C     ARBICA        ANDNE     'R'
057200930308     C*
057300051115     C* AGGANCIO FiAR900F PER VEDERE SE C'E' IL C/ASSEGNO
057400051115     c                   setoff                                       34
057500051115     c     arbicc        ifeq      ' '
057600020405     c     arbicc        oreq      'R'
057700051115     C     KARB          SETLL     FiAR9000                               34
057800060123     c     *in34         ifeq      *on
057900060123     C     Karb          chain     tncsb03l
058000060123     c                   if        %found(tncsb03l)
058100060123     c* stato 9 --> Annullato; Data distinta > 0 --> c/a incassato
058200060123     c                   if        csbsta = 9 or csbddc > 0
058300060123     c                   setoff                                       34
058400060123     c                   endif
058500060123     c                   endif
058600060123     c                   endif
058700020405     c* stampo motivazione
058800080711    8c   34arbdsp        iflt      wdaprt
058900080711     c* Se si tratta di C/Ass omesso, verifico se devo davvero stampare
059000080711     c*  in base alla data di rigenerazione
059100080711     c                   clear                   w008a
059200080711     c                   clear                   w0080
059300080711     c                   eval      ktrc='8'
059400080711     c     kar4          chain(n)  fiar401l
059500080711    9c                   if        %found(fiar401l)
059600080711     c                   eval      Indx=%scan('RIGENERATA':ar4not)
059700080711   10c                   if        Indx>0
059800080711     c                   eval      ktrc='9'
059900080711     c     kar4          chain(n)  fiar401l
060000080711   11c                   if        %found(fiar401l)
060100080711     c                   eval      Indx=%scan('OMESSO':ar4not)
060200080711   12c                   if        Indx>0
060300080711     c                   eval      w008a=%subst(ar4not:26:8)
060400080711   13c                   if        w008a>*zeros
060500080711     c                   movel     w008a         w0080
060600080711   13c                   endif
060700080711   12c                   endif
060800080711   11c                   endif
060900080711   10c                   endif
061000080711    9c                   endif
061100080711     c
061200080711    9c                   if        w0080=0 or w0080<wdapul
061300020405     c                   exsr      ovrprt
061400080711     c                   if        w0080>0
061500020405     c                   movel(P)  cbcass        p01mes
061600080711     c                   eval      p01mes=%trim(p01mes)+' rigenerata il '+
061700080711     c                             %editw(w0080:'    /  /  ')
061800080711     c                   endif
061900080711     c
062000020405     c                   exsr      ried1
062100020405     c                   write     lr84d1
062200080711    9c                   endif
062300080711    8c                   endif
062400080711   7ac                   endif
062500941207     C*
062600950110     C* PULISCO SOLO SE NON C'E' IL C/ASSEGNO OPPURE SE C'E' MA E'
062700950110     C*   GIA' STATO INCASSATO
062800941212    7C     *IN34         IFEQ      *ON
062900930121     C     ARBICC        ANDNE     ' '
063000950119     C     ARBICC        ANDNE     'R'
063100941212     C     *IN34         OREQ      *OFF
063200950110     C*
063300950110     C* PULISCO SOLO SE NON TROVO RECORD NEI FILE TASSAZIONE PADRONCINI
063400020405     c                   clear                   p01mes
063500011121     C     KARB          SETLL     fiftd000                               36
063600020405     c     *in36         ifeq      *on
063700020405     c                   movel(P)  cbcpdr        p01mes
063800020405     c                   endif
063900011121     C  N36KARB          SETLL     fifsd000                               36
064000080711     c* visto che normalmente rimangono i record consegna e quindi scritti
064100080711     c*  in FTD per data consegna enon per data spedizione, stampo solo se
064200080711     c*  la data consegna < data stampa
064300020405     c     *in36         ifeq      *on
064400020409     c     arbdsp        andlt     wdaprt
064500080711     c     arbdcm        andeq     0
064600080711     c     *in36         oreq      *on
064700080711     c     arbdcm        andlt     wdaprt
064800080711     c     arbdcm        andgt     0
064900080711     c
065000020405     c     p01mes        ifeq      *blanks
065100020405     c                   movel(P)  cbcpdr        p01mes
065200020405     c                   eval      p01mes = %trim(p01mes) + ' di simulazione'
065300020405     c                   endif
065400020405     c                   exsr      ovrprt
065500020405     c                   exsr      ried1
065600020405     c                   write     lr84d1
065700020405     c                   endif
065800040831     c*
065900040831     c* Posso pulire solo le bolle:
066000040831     C* 1) le cui date di consegna sono state trasmesse in partenza e sede
066100040831     c*     arbft1 = 'T'
066200040831     c* 2) autogenerate poste con data ricezione >= data mandato introito
066300040831     c*    (sono quelle ex vedi pacco che ricevo tempo dopo)
066400040831     c*    arbft1 = 'R' e
066500040831     c* in caso contrario stampo segnalazione
066600040831    8c     *IN36         IFEQ      *OFF
066700040831     c*
066800040831     c     arbft1        ifne      'T'
066900040831     c     arbdt1        andlt     arbdmi
067000040831     c                   eval      wok = 'N'
067100040831     c* stampo motivazione
067200040831     c     arbdsp        iflt      wdaprt
067300040831     c                   exsr      ovrprt
067400040831     c                   movel(P)  cbdt1         p01mes
067500040831     c                   exsr      ried1
067600040831     c                   write     lr84d1
067700040831     c                   endif
067800040831     c                   endif
067900950110     C*
068000991104     C****** CANCELLAZIONE C.A. *******
068100040831   8Ac     wok           ifeq      *blanks
068200991104     C* SE BOLLA LOCALE NON CANCELLO LA C.A. SE PRESENTE BOLLA IN BLP
068300020611     c                   eval      *in96 = *off
068400020611     c     karb          setll     fnblp01l                               96
068500991104     C**
068600020611     c                   if        not *in96
068700991104     C**
068800991104     C                   MOVE      *BLANKS       WERR              1
068900040812      * Sostituisco la lettura del file FNDTC02 con
069000040813      * richiamo del programma FIDN12R passando la DS richiedendo le CA
069100040812      * legate alla spedizione che passo
069200040812      *
069300040812     c                   clear                   fidn12ds
069400040812     c                   eval      i12aas = arbaas
069500040812     c                   eval      i12lnp = arblnp
069600040812     c                   eval      i12nrs = arbnrs
069700040812     c                   eval      i12nsp = arbnsp
069800040812     c                   eval      i12fel = 'E'
069900040812      *
070000040812     c                   call      'FIDN12R'
070100040812     c                   parm                    fidn12ds
070200040812      *
070300040812      * se non ci sono errori
070400040812     c                   if        o12err = ' '
070500040812      * se numero di CA trovate maggiore di zero
070600040812     c                   if        o12nca > 0
070700040812     c
070800040812     c                   z-add     *zeros        ii                2 0
070900040812     c     1             do        o12nca        ii
071000040812     c                   movea     skkey(ii)     dskey
071100040812     C                   Z-ADD     skdca(ii)     dctdca
071200040813     C                   CLEAR                   DSDN11
071300040813     C                   MOVEL     'D'           I11MOD
071400040813     C                   Z-ADD     DCTAAC        I11AAC
071500040813     C                   Z-ADD     DCTFIL        I11FIL
071600040813     C                   Z-ADD     DCTNCA        I11NCA
071700040813     C                   CALL      'FIDN11R'
071800040813     C                   PARM                    KPJBA
071900040813     C                   PARM                    DSDN11
072000040813   11C     O11ERR        IFEQ      'E'
072100040813     C                   MOVEL     '1'           WERR
072200040813   11C                   END
072300040813      *
072400040813     c                   enddo
072500040813      *
072600040813     c                   endif
072700040813     c                   endif
072800991104     C**
072900991104     C* SE NON SONO RIUSCITA A CANCELLARE TUTTE LE C.A. NON POSSO
073000991104     C* CANCELLARE LA BOLLA
073100991104   10C     WERR          IFNE      *BLANKS
073200020405     c* stampo motivazione
073300020405     c                   exsr      ovrprt
073400020405     c                   movel(P)  cbercan       p01mes
073500020405     c     *ymd          move      dctdca        data_EUR
073600020405     c                   move      data_EUR      data_Alfa
073700020405     c                   eval      p01mes = %trim(p01mes) + %editc(dctfil:'Z')
073800020405     c                             + %editc(dctnca:'Z') + 'del'  + data_Alfa
073900020405     c                   exsr      ried1
074000020405     c                   write     lr84d1
074100991104     C                   MOVE      'N'           WOK
074200991104   10C                   ENDIF
074300991104    9C                   ENDIF
074400040423      *
074500040423      * RECLAMI : se non trovata CANCELLO BOLLA
074600040423      *
074700040423     C*
074800040423    1C     WOK           IFEQ      ' '
074900040423     c                   movel     arbaas        W_aas
075000040423     c                   movel     arblnp        W_lnp
075100040423     c                   movel     arbnrs        W_nrs
075200040423     c                   movel     arbnsp        W_nsp
075300040423      *
075400040423     c                   movel     wspe          ktgogg
075500040423      *
075600040423     c     ktgd          setll     fitgd02l
075700040423     c                   do        *hival
075800040423     c     ktgd          reade     fitgd02l
075900040423
076000040423     c                   if        %eof(fitgd02l)
076100040423     c                   leave
076200040423     c                   endif
076300040423      * se annullato leggo il successivo
076400040423     c                   If        TGDatb <> ' '
076500040423     c                   iter
076600040423     c                   endif
076700141209     c                   select
076800141209      * se ancora aperto ma si può chiudere: cancello R.A.,
076900141209      *                                      chiudo eventuale dispo di cons da TIIDC
077000141209     c                   when      tgddcd = 0 and %lookup(tgdmad:mach)>0
077100150928     c                   clear                   fidnCAds
077200141209     c                   eval      IC0ANT=tgdant
077300141209     c                   eval      IC0NUT=tgdnut
077400141209     c                   eval      IC0PDT=999
077500150928     c                   call      'FIDNCAR'
077600150928     c                   parm                    fidnCAds
077700141209     c     kidc          setll     tiidc01l
077800141209     c     kidc          reade     tiidc01l
077900141209     c                   dow       not %eof(tiidc01l)
078000141209     c                   if        idcaNT=TGDANT and idcnuT=tgdnut and
078100141209     c                             idcpdt=tgdpdt and idcelaflg='E'
078200141209     c                   eval      IDCELADATA=%int( %subst( %char( %dec
078300141209     c                                        ( %timestamp() ) ) :1 :14 ) )
078400141209     c                   eval      idcelaute=knmus
078500141209     c                   update    tiidc000
078600141209     c                   else
078700141209     c                   unlock    tiidc01l
078800141209     c                   endif
078900141209     c     kidc          reade     tiidc01l
079000141209     c                   enddo
079100040423      * verifico se reclamo ancora aperto
079200141209     c                   when      TGDdcd = 0  or
079300040423      * se reclamo chiuso verifico la data chiusura con la dta di pulizia
079400040423     c                             TGDdcd > comrec
079500040423     c                   eval      wok = 'N'
079600080515     c
079700080515     c* stampo motivazione  solo se aperto
079800080515     c                   If        TGDdcd = 0
079900040423     c                   exsr      ovrprt
080000040423     c                   movel(P)  cbcrec        p01mes
080100040423     c                   exsr      ried1
080200040423     c                   write     lr84d1
080300040423     c                   leave
080400080515     c                   endif
080500080515     c
080600141209     c                   endsl
080700040423
080800040423     c                   enddo
080900040423
081000040423    1c                   endif
081100991104     C****** PULIZIA ALTRI FILE ********+
081200991104    9C     WOK           IFEQ      ' '
081300020411     C                   EXSR      PULIZ
081400991104    9C                   ENDIF
081500991104     C**
081600040831   8Ac                   endif
081700991104    8C                   ENDIF
081800991104     C**
081900941207    7C                   ENDIF
082000000927   7AC                   ENDIF
082100941207    6C                   ENDIF
082200970623   6AC                   ENDIF
082300920306     C*
082400000809   5AC                   ENDIF
082500941207     C     FIL           READE     FNARB000                               31
082600941207    5C                   ENDIF
082700950118     C*
082800941207     C*
082900941207    3C                   ENDDO
083000941207    2C                   ENDIF
083100941207    1C                   ENDDO
083200020404     c*
083300020409     C                   Z-ADD     0             FIL               3 0
083400020409     C                   Z-ADD     0             WDCM
083500020409     C                   eval      *in30 = *off
083600020409     C*
083700020409    1C     *IN30         DOWEQ     *OFF
083800020409     C     FIL           SETGT     FNARB000
083900020409     C                   READ      FNARB000                               30
084000020409     C*
084100020409    2C     *IN30         IFEQ      *OFF
084200020409     C                   Z-ADD     ARBLNA        FIL
084300020409     C*
084400020409     C     KARB2         SETLL     FNARB000
084500020409     C     KARB2         READE     FNARB000                               31
084600020409     C*
084700020409    3C     *IN31         DOWEQ     *OFF
084800080515    4c     arbdsp        iflt      dtsncn_ymd
084900020411     c* Guardo se presente c.a.
085000040813      * Sostituisco la lettura del file FNDTC02 con
085100040813      * richiamo del programma FIDN12R passando la DS richiedendo le CA
085200040813      * legate alla spedizione che passo
085300040813      *
085400040813     c                   clear                   fidn12ds
085500040813     c                   eval      i12aas = arbaas
085600040813     c                   eval      i12lnp = arblnp
085700040813     c                   eval      i12nrs = arbnrs
085800040813     c                   eval      i12nsp = arbnsp
085900040813     c                   eval      i12fel = 'E'
086000040813      *
086100040813     c                   call      'FIDN12R'
086200040813     c                   parm                    fidn12ds
086300040813      *
086400040813      * se non ci sono errori  e num.ca magg. di zero accendo il 32
086500080515     c                   clear                   wchiusaCA         1
086600120217     C                   CLEAR                   WDCH_dcb
086700080515    5c                   if        o12err = ' ' AND o12nca > 0
086800040813     c                   seton                                        32
086900080515     c                   eval      WchiusaCA='S'
087000080515     c
087100080515     c* Stampo solo se tutte le c.a. sono chiuse
087200080515     c                   z-add     *zeros        ii                2 0
087300080515    6c     1             do        o12nca        ii
087400080515     c                   movea     skkey(ii)     dskey
087500080515     C                   Z-ADD     skdca(ii)     dctdca
087600080515     C                   Z-ADD     skdch(ii)     dctdch
087700080515     C                   movea     skcch(ii)     dctcch
087800080515     c
087900080515      * se CA chiusa stampo,imposto il flag e vado a fine
088000080515    7c                   If        dctdch = 0
088100080515     c                   clear                   wchiusaCA
088200120217     c                   else
088300120217     c* memorizzo la data chiusura più alta se p.o. C.A. = lna bolla
088400120217    8c     dctdch        ifgt      wdch_dcb
088500120217     c     dctfil        andeq     arblna
088600120217     c                   z-add     dctdch        wdch_dcb
088700120217    8c                   endif
088800080515    7c                   endif
088900080515    6c                   enddo
089000080515    5c                   endif
089100080515     c
089200040813      *
089300120220    5c** 32lnpntw        ifne      'PPT'
089400120220     C**                 Z-ADD     dtscan_ymd    WDAprt
089500120220     c**                 else
089600120220     C**                 Z-ADD     dpscan_ymd    WDAprt
089700120220    5c**                 endif
089800080515    5C***  *in32         ifeq      *off
089900080515     c***  arbdsp        orlt      wdaprt
090000080515    c
090100080515     c                   if        (*in32 and wchiusaCA='S' and
090200120222    5c                             wdch_dcb<dtsgdc_ymd and arbdsp<dtscan_ymd)
090300120217    5c                             or not *in32
090400080515     c
090500020409     c                   exsr      ovrprt
090600020411     c  n32              movel(P)  cbncon        p01mes
090700020411     c   32              movel(P)  cbnconca      p01mes
090800020409     c                   exsr      ried1
090900020409     c                   write     lr84d1
091000080515    5c                   endif
091100080515    4c                   endif
091200020409     C*
091300020409     C     karb2         READE     FNARB000                               31
091400020409     C*
091500020409    3C                   ENDDO
091600020409    2C                   ENDIF
091700020409    1C                   ENDDO
091800020409     c*
091900020404     c     wopn          ifeq      '1'
092000020404     c                   write     lr84z1
092100020404     c                   endif
092200990823     C*
092300990823     C* CHIUDO CON LR
092400990823     C                   CLEAR                   DSBS02
092500990823     C                   MOVEL     'C'           T02TLA
092600990823     C                   CALL      'TIBS02R'
092700990823     C                   PARM                    KPJBA
092800990823     C                   PARM                    DSBS02
092900040813     C*
093000040813     C                   MOVEL     'C'           I12TLA
093100040813     C                   CALL      'FIDN12R'
093200040813     C                   PARM                    FIDN12DS
093300150928      *
093400150928     c                   clear                   fidnCAds
093500150928     c                   eval      IC0tla = 'C'
093600150928     c                   call      'FIDNCAR'
093700150928     c                   parm                    fidnCAds
093800920205     C*
093900950516     C                   MOVEL     WSAJBU        KPJBU
094000920205     C                   SETON                                        LR
094100930628     C*---------------------------------------------------------------*
094200930628     C*                    PULIZ                                      *
094300930628     C* ROUTINE DI PULIZIA FILE BOLLE E FILE COLLEGATI                *
094400930628     C*---------------------------------------------------------------*
094500920324     C     PULIZ         BEGSR
094600060502     c* Chaino file fisico: se riesco pulisco, altrimenti leggo altra bolla
094700060502     c     arbnrr        chain(E)  fnarb00f
094800060502     c                   if        not %error and %found(fnarb00f)
094900941207     C*
095000051115     c                   if        not *in96
095100060220     C*
095200060220     C****  FIAR400F ---> ESTENSIONE DESCRIZIONI  ****
095300060220     C     KARB          SETLL     FIAR4000                               32
095400060220    1C     *IN32         IFEQ      *ON
095500060220     C     KARB          READE     FIAR4000                               32
095600060220     C*
095700060220    2C     *IN32         DOWEQ     *OFF
095800060220     C                   DELETE    FIAR4000
095900060220     C     KARB          READE     FIAR4000                               32
096000060220    2C                   ENDDO
096100060220    1C                   ENDIF
096200060220     C****  FiAR900F ---> DATI C/ASSEGNO  ****
096300051115     C     KARB          CHAIN     FiAR9000                           32
096400051115     C  N32              DELETE    FiAR9000
096500051115     c                   endif
096600941207     C*
096700930628     C*
096800941207     C****  FNART00F ---> DETTAGLIO COLLI  ****
096900941207     C     KARB          SETLL     FNART000
097000941207     C     KARB          READE     FNART000                               32
097100920324     C*
097200941207    1C     *IN32         DOWEQ     *OFF
097300961125     C* CANCELLO GLI EVENTI SEGNACOLLI SENZA NUMERO SPEDIZIONE
097400961125     C                   SETOFF                                       36
097500961125     C     KART          SETLL     FNEVS02L
097600961125    2C     *IN36         DOWEQ     *OFF
097700961125     C     KART          READE     FNEVS02L                               36
097800961125     C  N36              DELETE    FNEVS002
097900961125    2C                   ENDDO
098000051012     c
098100980331     C* CANCELLO L'ESTENSIONE DETTAGLIO SEGNACOLLO
098200051012     c* solo se non più presente la bolla in fnblp
098300051012     c                   if        not *in96
098400051012     c
098500980331     C                   SETOFF                                       36
098600051012     C     KART          SETLL     FIARS01L
098700980331    2C     *IN36         DOWEQ     *OFF
098800051012     C     KART          READE     FIARS01L                               36
098900051012     C  N36              DELETE    FIARS000
099000980331    2C                   ENDDO
099100051012     c
099200051012     c                   endif
099300961125     C*
099400941207     C                   DELETE    FNART000
099500941207     C     KARB          READE     FNART000                               32
099600941207    1C                   ENDDO
099700930628     C*
099800961125     C****  FNEVS00F ---> EVENTI SEGNACOLLI  ****
099900961125     C* CANCELLO GLI EVENTI SEGNCOLLI CON NUMERO SPEDIZIONE
100000961125     C     KARB          SETLL     FNEVS01L
100100961125     C     KARB          READE     FNEVS01L                               32
100200961125     C*
100300961125    1C     *IN32         DOWEQ     *OFF
100400961125     C                   DELETE    FNEVS000
100500961125     C     KARB          READE     FNEVS01L                               32
100600961125    1C                   ENDDO
100700991104     C*
100800991104     C****  FIARI00F ---> FILE DEGLI INCASSI  ****
100900991104     C     KARB          SETLL     FIARI000                               32
101000991104    1C     *IN32         IFEQ      *ON
101100991104     C     KARB          READE     FIARI000                               32
101200991104     C*
101300991104    2C     *IN32         DOWEQ     *OFF
101400991104     C                   DELETE    FIARI000
101500991104     C     KARB          READE     FIARI000                               32
101600991104    2C                   ENDDO
101700991104    1C                   ENDIF
101800061107     C*
101900061107     C****  FIARG00F ---> FILE GEOREFER/GIRI DI CONSEGNA
102000061107     C     KARB          SETLL     FIARG000                               32
102100061107    1C     *IN32         IFEQ      *ON
102200061107     C     KARB          READE     FIARG000                               32
102300061107     C*
102400061107    2C     *IN32         DOWEQ     *OFF
102500061107     C                   DELETE    FIARG000
102600061107     C     KARB          READE     FIARG000                               32
102700061107    2C                   ENDDO
102800061107    1C                   ENDIF
102900070503     C*
103000070503     C****  FIARP00F ---> FILE proposte DCr che dovrebbe essere vuoto
103100070503     C     KARB          SETLL     FIARP000                               32
103200070503    1C     *IN32         IFEQ      *ON
103300070503     C     KARB          READE     FIARP000                               32
103400070503     C*
103500070503    2C     *IN32         DOWEQ     *OFF
103600070503     C                   DELETE    FIARP000
103700070503     C     KARB          READE     FIARP000                               32
103800070503    2C                   ENDDO
103900070503    1C                   ENDIF
104000070503     C*
104100070503     C****  FIARN00F ---> FILE note LDV
104200070503     C     KARB          SETLL     FIARN000                               32
104300070503    1C     *IN32         IFEQ      *ON
104400070503     C     KARB          READE     FIARN000                               32
104500070503     C*
104600070503    2C     *IN32         DOWEQ     *OFF
104700070503     C                   DELETE    FIARN000
104800070503     C     KARB          READE     FIARN000                               32
104900070503    2C                   ENDDO
105000070503    1C                   ENDIF
105100140214     C****  FISIA00F ---> FILE spia per invio Alert FD e L.avviso
105200140214     C     KARB          SETLL     FIsia01l
105300140214    1C                   if        %equal(fisia01l)
105400140214     C     KARB          READE     FIsia01l
105500140214     C*
105600140214    2C                   DOW       not %eof(fisia01l)
105700140214     C                   DELETE    FIsia000
105800140214     C     KARB          READE     fisia01l
105900140214    2C                   ENDDO
106000140214    1C                   ENDIF
106100961125     C*
106200941207     C*****
106300991104     C***  VARIAZIONI BOLLA  E TASSAZIONE
106400941207     C*****
106500930602     C*
106600971002     C* NON PULISCO I LOCALI E I SUOI EVENTI SE LA BOLLA PARTENZA
106700971002     C*  C'E' ANCORA
106800030108     C* (Ho cancellato la lokup su l1 e la setll su fnblp perchè
106900030108     C* le stesse specifiche sono già state eseguite per gestire
107000030108     C* la cancellazione di C.A. di bolla locale - (vedi sopra))
107100020611     c                   if        not *in96
107200991104     C**
107300991104     C****  FIAR600F ---> TASSAZIONE BOLLA  ****
107400991104     C     KARB          CHAIN     FIAR6000                           32
107500991104    2C     *IN32         DOWEQ     *OFF
107600991104     C*
107700991104     C****  FIAR700F ---> ESTENSIONE TASSAZIONE BOLLA  ****
107800991104     C     KARB7         SETLL     FIAR7000                               35
107900991104    3C     *IN35         IFEQ      *ON
108000991104     C     KARB7         READE     FIAR7000                               35
108100991104     C*
108200991104    4C     *IN35         DOWEQ     *OFF
108300991104     C                   DELETE    FIAR7000
108400991104     C     KARB7         READE     FIAR7000                               35
108500991104    4C                   ENDDO
108600991104    3C                   ENDIF
108700991104     C*
108800991104     C                   DELETE    FIAR6000
108900991104     C**
109000991104     C     KARB          READE     FIAR6000                               32
109100991104    2C                   ENDDO
109200930602     C*
109300920410     C                   SETOFF                                       010203
109400941207     C                   SETOFF                                       040506
109500131014     C                   SETOFF                                       0708
109600131014     C     KARB          SETLL     FIARBF2C                               32
109700941207    2C     *IN32         IFEQ      *ON
109800131014     C     KARB          READE     FIARBF2C                               32
109900920324     C*
110000941207    3C     *IN32         DOWEQ     *OFF
110100941207     C   01              DELETE    FNARBD00
110200941207     C   02              DELETE    FNARBK00
110300950630     C*
110400950630    4C     *IN03         IFEQ      *ON
110500950630     C* SE C'E' LA 3° VARIA CONTROLLO SE CE NE SONO ANCHE IN FNARBU0F
110600950630    5C     ARBSV3        IFNE      ' '
110700950630     C*
110800991104     C     KARB          SETLL     FIARBU00                               35
110900950630    6C     *IN35         IFEQ      *ON
111000991104     C     KARB          READE     FIARBU00                               35
111100950630     C*
111200950630    7C     *IN35         DOWEQ     *OFF
111300991104     C                   DELETE    FIARBU00
111400991104     C     KARB          READE     FIARBU00                               35
111500950630    7C                   ENDDO
111600950630    6C                   ENDIF
111700950630    5C                   ENDIF
111800950630     C*
111900991104     C                   DELETE    FIARBT00
112000950630    4C                   ENDIF
112100950630     C*
112200991104     C   04              DELETE    FNARBP00
112300941207     C   05              DELETE    FNARBG00
112400941207     C   06              DELETE    FNARBM00
112500941207     C   07              DELETE    FNARBV00
112600131014     C   08              DELETE    FNARBN00
112700920410     C                   SETOFF                                       010203
112800941207     C                   SETOFF                                       040506
112900131014     C                   SETOFF                                       0708
113000131014     C     KARB          READE     FIARBF2C                               32
113100941207    3C                   ENDDO
113200941207    2C                   ENDIF
113300030108     C*
113400030108     C****  FIAR500F ---> PULIZIA ESTENSIONI TESTATA BOLLA  ****
113500030108     C     KARB          setll     FIAR5000                               32
113600030108    2C                   if        *IN32
113700030108     C     KARB          reade     FIAR5000                               32
113800030108    3C                   dow       not *IN32
113900030108     C                   DELETE    FIAR5000
114000030108     C     KARB          reade     FIAR5000                               32
114100030108    3C                   enddo
114200030108    2C                   endif
114300040604     c*
114400040604     C****  FIAR800F ---> PULIZIA ESTENSIONI TESTATA BOLLA  ****
114500040604     C     KARB          setll     FIAR8000                               32
114600040604    2C                   if        *IN32
114700040604     C     KARB          reade     FIAR8000                               32
114800040604    3C                   dow       not *IN32
114900040604     C                   DELETE    FIAR8000
115000040604     C     KARB          reade     FIAR8000                               32
115100040604    3C                   enddo
115200040604    2C                   endif
115300930628     C*
115400990806     C****  FNEVB00F ---> PULIZIA EVENTI  ****
115500991104     C     KARB          SETLL     FNEVB01L                               32
115600941207    2C     *IN32         IFEQ      *ON
115700040830     c* se bolla presente in titas non posso pulire
115800040830     c     karb          setll     titas30c
115900040831     c                   if        not %equal(titas30c)
116000991104     C     KARB          READE     FNEVB01L                               32
116100941207    3C     *IN32         DOWEQ     *OFF
116200990806     C                   DELETE    FNEVB01L
116300131029     C     KARB2f        chain     FNEVB22L
116400131029     c                   if        %found(fnevb22l)
116500131029     c                   delete    fnevb22l
116600131029     c                   endif
116700991104     C     KARB          READE     FNEVB01L                               32
116800941207    3C                   ENDDO
116900040831     c                   endif
117000941207    2C                   ENDIF
117100930628     C*
117200941207    1C                   ENDIF
117300920324     C**
117400060502     C* CANCELLO LA BOLLA dal file fisico fnarb00F
117500920324     C**
117600060502     C                   DELETE    FNARBfis
117700010508     C**
117800050906      * Pulizia Giacenza: Se wgiace = 'S' vuol dire che ho giacenza chiusa e non c'è
117900050906      *                   la bolla originale su FNBLP in questo caso posso pulire la giacenza
118000050906     c                   If        WGIACE = 'S'
118100050404     c                   z-add     arbaas        iz5aas
118200050404     c                   z-add     arblnp        iz5lnp
118300050404     c                   z-add     arbnrs        iz5nrs
118400050404     c                   z-add     arbnsp        iz5nsp
118500050404     c                   call      'FNLGZ5C'
118600050404     c                   PARM                    FNLGZ5DS
118700050404---  C                   ENDIF
118800060502     c
118900060502---  C                   ENDIF
119000010508      *
119100920306     C                   ENDSR
119200000927     C*---------------------------------------------------------------*
119300000927     C* CONTROLLO SE SI TRATTA DI VERO RITRONO ALL'INCASSO
119400000927     C*---------------------------------------------------------------*
119500000927     C     CTRICA        BEGSR
119600000927     C* VERIFICO SE BOLLA SINGOLA O DOPPIA
119700000927     C     ARBCBO        LOOKUP    CBO                                    32
119800000927     C**
119900000927     C* BOLLA SINGOLA IN ASSEGNATO
120000000927    1C     *IN32         IFEQ      *OFF
120100000927     C                   MOVE      ARBKSC        W0040             4 0
120200000927    2C     W0040         IFNE      9999
120300000927     C* CHAIN SU FIAR6 PER VEDERE SE C'E' FATTURA IMMEDIATA
120400000927     C                   MOVEL     '1'           AR6TRC
120500000927     C     KARB7         CHAIN(N)  FIAR601L                           34
120600000927     C**
120700000927     C* SE NON C'E' O C'E' MA SENZA FATT IMMEDIATA, E' ERRATO
120800000927    3C     *IN34         IFEQ      *ON
120900000927     C     AR6DFT        OREQ      0
121000000927     C     AR6NFT        ANDEQ     0
121100000927     C                   MOVEL     'Y'           ARBICA
121200000927    3C                   ENDIF
121300000927    2C                   ENDIF
121400000927     C**
121500000927   X1C                   ELSE
121600000927     C* CHAIN SU FIAR6 PER VEDERE SE C'E' FATTURA IMMEDIATA
121700000927     C                   MOVEL     '2'           AR6TRC
121800000927     C     KARB7         CHAIN(N)  FIAR601L                           34
121900000927     C**
122000000927     C* SE CE' ED E' S.F. E' FINOTO
122100000927    2C     *IN34         IFEQ      *OFF
122200000927     C     AR6DFT        ANDEQ     0
122300000927     C     AR6NFT        ANDEQ     0
122400000927     C                   MOVE      AR6KSC        W0040
122500000927     C**
122600000927    3C     W0040         IFNE      9999
122700000927     C                   MOVEL     'Y'           ARBICA
122800000927    3C                   ENDIF
122900000927    2C                   ENDIF
123000000927    1C                   ENDIF
123100000927     C                   ENDSR
123200930628     C*---------------------------------------------------------------*
123300020611     C* ROUTINE DI CARICAMENTO TABELLE                                *
123400930628     C*---------------------------------------------------------------*
123500930507     C     CARTAB        BEGSR
123600000927     C*
123700000927     C* CARICO I TIPI BOLLA DOPPI
123800000927     C                   CLEAR                   YY
123900000927     C                   MOVEL     '3A'          COD
124000000927     C     KTAB          SETLL     TABEL
124100000927     C     KTAB          READE     TABEL                                  30
124200000927     C     *IN30         DOWEQ     *OFF
124300000927     C     TBLFLG        IFEQ      ' '
124400000927     C                   MOVEL     TBLUNI        DS3A
124500000927     C     §3ATB2        IFNE      *BLANKS
124600000927     C                   ADD       1             YY                3 0
124700000927     C                   MOVEL     TBLKEY        CBO(YY)
124800000927     C                   ENDIF
124900000927     C                   ENDIF
125000000927     C     KTAB          READE     TABEL                                  30
125100000927     C                   ENDDO
125200930628     C*
125300101007     c* Carico gli eventi di riserva dalla tabella 2A
125400101007     C                   CLEAR                   YY
125500101007     C                   MOVEL     '2A'          COD
125600101007     C     KTAB          SETLL     TABEL
125700101007     C     KTAB          READE     TABEL                                  30
125800101007     C     *IN30         DOWEQ     *OFF
125900101007     C     TBLFLG        IFEQ      ' '
126000101007     C                   MOVEL     TBLUNI        DS2A
126100101007     C     §2Atip        IFeq      'R'
126200101007     C                   ADD       1             YY                3 0
126300101007     C                   MOVEL     TBLKEY        ris(YY)
126400101007     C                   ENDIF
126500101007     C                   ENDIF
126600101007     C     KTAB          READE     TABEL                                  30
126700101007     C                   ENDDO
126800141209     c* Carico motivi apertura R.A. che si possono chiudere se ancora aperti
126900141209     c                   clear                   yy
127000141209     c     'MAR'         setll     tntbe01l
127100141209     c     'MAR'         reade     tntbe01l
127200141209     c                   dow       not %eof(tntbe01l)
127300141209     c                   movel     tbeuni        dmar
127400141209     c                   if        §MARCANPA='S'
127500141209     c                   eval      yy+=1
127600141209     c                   movel     tbeke1        mach(yy)
127700141209     c                   endif
127800141209     c     'MAR'         reade     tntbe01l
127900141209     c                   enddo
128000930507     C                   ENDSR
128100020404     C*---------------------------------------------------------------*
128200020404     C* OVRPRT - Operazioni su file printer                           *
128300020404     C*---------------------------------------------------------------*
128400020404     C     ovrprt        BEGSR
128500020404     c* override su file printer + apertura
128600020404     c     wopn          ifeq      *blanks
128700020409     C                   MOVEl     cmd(1)        COMMAN          100
128800020409     c                   eval      %subst(cmd(2):6:4) = whld
128900020409     c                   eval      %subst(cmd(2):17:4) = wsav
129000020409     c                   eval      comman = %trim(comman) + '('
129100020409     c                             + %trim(§5alib) + '/'
129200020409     c                             + %trim(§5aotq) + ') ' + %trim(cmd(2))
129300020409     C                   Z-ADD     100           LUNGH
129400020404     C                   CALL      'QCMDEXC'
129500020404     C                   PARM                    COMMAN
129600020404     C                   PARM                    LUNGH            15 5
129700020404     c                   open      fnlr84p
129800020404     c* valorizzazione campi di stampa testata
129900020404     c* data bolle lnp italia
130000020404     c     *ymd          move      comdpb        data_dmy
130100020404     c                   move      data_dmy      dtpita
130200020404     c* data bolle import
130300020404     c     *ymd          move      compbe        data_dmy
130400020404     c                   move      data_dmy      dtpexp
130500020404     c* data bolle con riserva
130600020404     c     *ymd          move      comarr        data_dmy
130700020404     c                   move      data_dmy      dtprsv
130800020404     c* data bolle con c.a.
130900020404     c     *ymd          move      combca        data_dmy
131000020404     c                   move      data_dmy      dtpcan
131100020404     c* Bolle poste: data bolle lnp italia
131200120220     c***  *ymd          move      posdpb        data_dmy
131300120220     c***                move      data_dmy      dppita
131400020404     c*              data bolle import
131500120220     c***  *ymd          move      pospbe        data_dmy
131600120220     c***                move      data_dmy      dppexp
131700020404     c*              data bolle con riserva
131800120220     c***  *ymd          move      posarr        data_dmy
131900120220     c***                move      data_dmy      dpprsv
132000020404     c*              data bolle con c.a.
132100120220     c**   *ymd          move      posbca        data_dmy
132200120220     c***                move      data_dmy      dppcan
132300020404     c*
132400020404     c                   write     lr84t1
132500020404     c                   movel     '1'           wopn
132600020404     c                   endif
132700020404     c* stampa intestazione a rottura
132800020404     c   99              write     lr84t1
132900020404     c                   setoff                                       99
133000020404     c                   endsr
133100020404     C*---------------------------------------------------------------*
133200020404     C* RIED1  - Valorizzazione campi di stampa dettaglio             *
133300020404     C*---------------------------------------------------------------*
133400020404     C     RIED1         BEGSR
133500020404     c                   z-add     arblnp        p1clnp
133600020404     c                   z-add     arblna        p1clna
133700020404     c                   z-add     arbnrs        p1cnrs
133800020404     c                   z-add     arbnsp        p1cnsp
133900080515     c                   MOVEL     arbCBO        p1cCBO
134000080515     c                   MOVEL     arbCCA        p1cCCA
134100020404     c     *ymd          move      arbdsp        data_EUR
134200020404     c                   move      data_EUR      p1cdsp
134300080519     c                   if        arbdcm>0
134400080515     c     *ymd          move      arbDCM        data_EUR
134500080515     c                   move      data_EUR      p1cDCM
134600080519     c                   else
134700080519     c                   clear                   p1cdcm
134800080519     c                   endif
134900020404     c                   endsr
135000010508     C*---------------------------------------------------------------*
135100010508     C* ROUTINE INIZIALE
135200010508     C*---------------------------------------------------------------*
135300010508     C     *inzsr        BEGSR
135400010508      *
135500010508     C     *ENTRY        PLIST
135600010508     C                   PARM                    KPJBA
135700010508     C                   MOVEL     KPJBU         PARAM
135800010508     C                   Z-ADD     1             CODUT
135900010508     C                   CALL      'X§PARUT'
136000010508     C                   PARM                    UT§DSE
136100010508     C                   MOVEL     REC80         CNCR80
136200020404     c                   movel     ragut         rsut
136300020404     C*
136400020404     C                   TIME                    wtime            14 0
136500020404     C                   MOVE      WTIME         WDATE             8 0
136600020409     c     *eur          move      wdate         wdate_inv
136700120216     c     *eur          move      wdate         wdateu
136800010508     C*---------------------------------------------------------------*
136900010508     C     KTAB          KLIST
137000010508     C                   KFLD                    CODUT
137100010508     C                   KFLD                    COD
137200020409     C     KTAB1         KLIST
137300020409     C                   KFLD                    CODUT
137400020409     C                   KFLD                    COD
137500020409     c                   kfld                    key
137600010508     C     KARB          KLIST
137700010508     C                   KFLD                    ARBAAS
137800010508     C                   KFLD                    ARBLNP
137900010508     C                   KFLD                    ARBNRS
138000010508     C                   KFLD                    ARBNSP
138100131029     C     KARB2f        KLIST
138200131029     C                   KFLD                    EVBAAS
138300131029     C                   KFLD                    EVBLNP
138400131029     C                   KFLD                    EVBNRS
138500131029     C                   KFLD                    EVBNSP
138600131029     C                   KFLD                    EVBDTV
138700131029     C                   KFLD                    EVBORV
138800131029     C                   KFLD                    EVBCEV
138900080711     C     KAR4          KLIST
139000080711     C                   KFLD                    ARBAAS
139100080711     C                   KFLD                    ARBLNP
139200080711     C                   KFLD                    ARBNRS
139300080711     C                   KFLD                    ARBNSP
139400080711     C                   KFLD                    ktrc
139500010508     C     KARB7         KLIST
139600010508     C                   KFLD                    ARBAAS
139700010508     C                   KFLD                    ARBLNP
139800010508     C                   KFLD                    ARBNRS
139900010508     C                   KFLD                    ARBNSP
140000010508     C                   KFLD                    AR6TRC
140100010508     C     KARB2         KLIST
140200010508     C                   KFLD                    FIL
140300010508     C                   KFLD                    WDCM
140400010508     C     KART          KLIST
140500010508     C                   KFLD                    ARTFLS
140600010508     C                   KFLD                    ARTLNA
140700010508     C                   KFLD                    ARTNRS
140800010508     C                   KFLD                    ARTNSC
140900010508     C     KEVB4         KLIST
141000010508     C                   KFLD                    ARBAAS
141100010508     C                   KFLD                    ARBLNP
141200010508     C                   KFLD                    ARBNRS
141300010508     C                   KFLD                    ARBNSP
141400010508     C                   KFLD                    WCEV
141500050413
141600050906     C     KGCA          KLIST
141700050906     C                   KFLD                    ARBAAS
141800050906     C                   KFLD                    ARBLNP
141900050906     C                   KFLD                    ARBNRS
142000050906     C                   KFLD                    ARBNSP
142100050906     C                   KFLD                    KFRG
142200050404
142300050906     c     kblp          klist
142400050906     c                   kfld                    GcpAas
142500050906     c                   kfld                    GcpLnp
142600050906     c                   kfld                    GcpNrs
142700050906     c                   kfld                    GcpNsp
142800020530     c*
142900020530     c     kdcf          klist
143000020530     c                   kfld                    dctaac
143100020530     c                   kfld                    dctfil
143200020530     c                   kfld                    dctnca
143300010508      *
143400101007     C****               MOVEL     'R  '         WCEV
143500050906     C                   clear                   kfrg
143600040423     c*  File Fitgd02l    RECLAMI
143700040423     c     ktgd          klist
143800040423     c                   kfld                    ktgtor
143900040423     c                   kfld                    ktgogg
144000141209     C     Kidc          KLIST
144100141209     C                   KFLD                    ARBAAS
144200141209     C                   KFLD                    ARBLNP
144300141209     C                   KFLD                    ARBNRS
144400141209     C                   KFLD                    ARBNSP
144500141209     C                   KFLD                    weladata
144600040423     C*
144700010508      *
144800010508      * DEFINIZIONE CAMPI
144900010508     C     *LIKE         DEFINE    EVBCEV        WCEV
145000010508     C     *LIKE         DEFINE    KPJBU         WSAJBU
145100010508     C     *LIKE         DEFINE    ARBDCM        WDCM
145200010508     C     *LIKE         DEFINE    AR4TRC        WTRC
145300010508     C     *LIKE         DEFINE    DCTDCH        WDCH
145400020530     C     *LIKE         DEFINE    DCTDCH        WDCH_dcb
145500010508     C     *LIKE         DEFINE    ARBLNP        SAVLNP
145600010508     C     *LIKE         DEFINE    TBLCOD        COD
145700020409     C     *LIKE         DEFINE    TBLKEY        KEY
145800050906     C     *LIKE         DEFINE    GCPFRG        KFRG
145900020207     C     *LIKE         DEFINE    §ogntw        lnantw
146000040831     C     *LIKE         DEFINE    §ogntw        lnpntw
146100040831     c*
146200040831      * Se sono in ambiente buono - GAITRAGRU
146300040831     c                   If        knsif = 'FILTRA201 '
146400040831     c                   Eval      wlib = 'GAITRAGRU '
146500040831      *  Se sono in ambiente di prova - GAITRAGRPS
146600040831     c                   Else
146700040831     c                   Eval      wlib = 'GAITRAGRPS'
146800040831     c                   EndIf
146900040831     c*
147000040831     c                   Clear                   commant
147100040831     c                   Movel(p)  cmdt(1)       commant
147200040831     c                   Eval      %Subst(commant:30:10) = wlib
147300040831     c                   Eval      commant =
147400040831     c                             %trim(commant) + '/TITAS30C)'
147500040831     c                   Call      'QCMDEXC'                            99
147600040831     c                   Parm                    commant
147700040831     c                   Parm                    lenght
147800040831     c                   Open      Titas30c
147900060123     c*
148000060123     c                   Clear                   commant
148100060123     c                   Movel(p)  cmdt(2)       commant
148200060123     c                   Eval      %Subst(commant:30:10) = wlib
148300060123     c                   Eval      commant =
148400060123     c                             %trim(commant) + '/TNCSB03L)'
148500060123     c                   Call      'QCMDEXC'                            99
148600060123     c                   Parm                    commant
148700060123     c                   Parm                    lenght
148800060123     c                   Open      TNCSB03L
148900101007     c
149000020409     c*
149100020409     c                   exsr      caldts
149200010508      *
149300010508     C                   ENDSR
149400020409     C*---------------------------------------------------------------*
149500020409     C* caldts - Calcolo date stampa                                  *
149600020409     C*---------------------------------------------------------------*
149700020409     C     CALDTS        BEGSR
149800020409      * LETTURA giorni per date stampa e coda di output
149900020409     C                   MOVEL     '5A'          COD
150000020409     C                   MOVEL     '2       '    KEY
150100020409     C     ktab1         CHAIN     taBEL00F                           30
150200020409     C  N30              MOVEL     TBLUNI        DS5A2
150300020409     C   30              CLEAR                   DS5A2
150400020409     C*
150500020409     C                   MOVEL     '5A'          COD
150600020409     C                   MOVEL     '3       '    KEY
150700020409     C     ktab1         CHAIN     TABEL00F                           30
150800020409     C  n30              MOVEL     TBLUNI        DS5A3
150900020409     C   30              CLEAR                   DS5A3
151000020409     c     §5ahld        ifeq      'S'
151100020409     c                   movel     '*YES'        whld              4
151200020409     c                   else
151300020409     c                   movel     '*NO '        whld
151400020409     c                   endif
151500020409     c     §5asav        ifeq      'S'
151600020409     c                   movel     '*YES'        wsav              4
151700020409     c                   else
151800020409     c                   movel     '*NO '        wsav
151900020409     c                   endif
152000020409     c* BOLLE NO POSTE
152100020409     c* 1) data stampa bolle lna italia
152200020409     c     *ymd          move      comdpb        data_iso
152300020409     c                   subdur    §5agal:*d     data_iso
152400020409     c                   move      data_iso      dtsita_ymd
152500020409     c                   move      data_iso      data_dmy
152600020409     c                   move      data_dmy      dtsita
152700020409     c* 2) data stampa bolle export
152800020409     c     *ymd          move      compbe        data_iso
152900020409     c                   subdur    §5agal:*d     data_iso
153000020409     c                   move      data_iso      dtsexp_ymd
153100020409     c                   move      data_iso      data_dmy
153200020409     c                   move      data_dmy      dtsexp
153300020409     c* 3) data stampa bolle con riserva
153400020409     c     *ymd          move      comarr        data_iso
153500020409     c                   subdur    §5agal:*d     data_iso
153600020409     c                   move      data_iso      dtsrsv_ymd
153700020409     c                   move      data_iso      data_dmy
153800020409     c                   move      data_dmy      dtsrsv
153900020409     c* 4) data stampa bolle con c.a.
154000020409     c     *ymd          move      combca        data_iso
154100020409     c                   subdur    §5agcn:*d     data_iso
154200020409     c                   move      data_iso      dtscan_ymd
154300020409     c                   move      data_iso      data_dmy
154400020409     c                   move      data_dmy      dtscan
154500120222     c                   move      data_dmy      dtsncc
154600020409     c* BOLLE POSTE
154700020409     c* 1) data stampa bolle lna italia
154800020409     c     *ymd          move      posdpb        data_iso
154900020409     c                   subdur    §5agal:*d     data_iso
155000020409     c                   move      data_iso      dpsita_ymd
155100120220     c***                move      data_iso      data_dmy
155200120220     c****               move      data_dmy      dpsita
155300020409     c* 2) data stampa bolle export
155400020409     c     *ymd          move      pospbe        data_iso
155500020409     c                   subdur    §5agal:*d     data_iso
155600020409     c                   move      data_iso      dpsexp_ymd
155700120220     c***                move      data_iso      data_dmy
155800120220     c***                move      data_dmy      dpsexp
155900020409     c* 3) data stampa bolle con riserva
156000020409     c     *ymd          move      posarr        data_iso
156100020409     c                   subdur    §5agal:*d     data_iso
156200020409     c                   move      data_iso      dpsrsv_ymd
156300120220     c***                move      data_iso      data_dmy
156400120220     c***                move      data_dmy      dpsrsv
156500020409     c* 4) data stampa bolle con c.a.
156600020409     c     *ymd          move      posbca        data_iso
156700020409     c                   subdur    §5agcn:*d     data_iso
156800020409     c                   move      data_iso      dpscan_ymd
156900120220     c****               move      data_iso      data_dmy
157000120220     c****               move      data_dmy      dpscan
157100120220     c******             move      data_dmy      dpsncc
157200020409     c*
157300020409     c* Data per stampare le spedizioni non consegnate
157400020409     c                   subdur    §5agsn:*d     wdate_inv
157500020409     c                   move      wdate_inv     dtsncn_ymd
157600020409     c                   move      wdate_inv     data_dmy
157700020409     c                   move      data_dmy      dtsncn
157800120220     c****               move      data_dmy      dpsncn
157900120220
158000120220     c                   move      wdateu        data_iso
158100120220     c                   subdur    §5agdc:*d     data_iso
158200120220     c                   move      data_iso      dtsgdc_ymd
158300120220     c                   move      data_iso      data_dmy
158400120222     c                   move      data_dmy      dtsnccd
158500020409     c                   endsr
158600020404**
158700020409OVRPRTF FNLR84P OUTQ
158800120220HOLD(    ) SAVE(    ) USRDTA('PulBoArr')
158900040831** cmdt
159000040831OVRDBF FILE(TITAS30C) TOFILE(
159100060123OVRDBF FILE(TNCSB03L) TOFILE(
