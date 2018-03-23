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
004400160914     F**FIARBF2C  UF   E           K DISK
004500160914     F**FIARBU1L  UF   E           K DISK
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
007400150929     Ffitgd32l  IF   E           K DISK
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
009200160601     d lnaR            s              3  0 dim(10) ctdata perrcd(10)
009300020404     D*
009400020404     d data_EUR        s               d   datfmt(*EUR)
009500020404     d data_DMY        s               d   datfmt(*DMY)
009600020409     d data_ISO        s               d   datfmt(*ISO)
009700020409     d wdate_inv       s               d   datfmt(*ISO)
009800120216     d wdateu          s               d   datfmt(*ISO)
009900020404     d data_Alfa       s             10a
010000020409     d dtsita_ymd      s              8  0
010100020409     d dtsexp_ymd      s              8  0
010200020409     d dtsrsv_ymd      s              8  0
010300020409     d dtscan_ymd      s              8  0
010400020409     d dtsncn_ymd      s              8  0
010500120220     d dtsgdc_ymd      s              8  0
010600020404     d wopn            s              1a
010700040812     d dctdch          s              8  0
010800040812     d dctcch          s              2a
010900040831     d wlib            s             10
011000040831     d commant         s             80
011100040831     d lenght          s             15  5 inz(80)
011200040423
011300040423     d ktgtor          s                   like(tgdtor) inz('S')
011400040423     d ktgogg          s                   like(tgdogg)
011500080711     d ktrc            s                   like(ar4trc)
011600080711     d W008a           s              8
011700080711     d W0080           s              8  0
011800080711     d Indx            s              3  0
011900141209     d weladata        s                   like(idceladata) inz(0)
012000040423
012100941207     D*
012200961204     D* PARAMETRI RICEVUTI DAI PGM CHIAMANTI
012300900618     D PARAM           DS
012400961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA NON ESTERA)
012500941207     D  COMDPB                17     24  0
012600961204     D* DATA PULIZIA BOLLE ARRIVI (CON LNA ESTERA - BOLLE EXPORT)
012700961204     D  COMPBE                68     75  0
012800980722     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA
012900990707     D  COMARR               191    198  0
013000990126     D* DATA PULIZIA BOLLE ARRIVI CON C.A.
013100990707     D  COMBCA               199    206  0
013200160601     d
013300160601     d* tutte le date POSTE le ho eliminate dal pgm - ES 01/06/16
013400160601     d
013500160601     D* DATA PULIZIA BOLLE ARRIVI             - POSTE
013600000530     D  POSDPB               207    214  0
013700000530     D* DATA PULIZIA BOLLE ARRIVI - BOLLE EXPORT - POSTE
013800000530     D  POSPBE               215    222  0
013900000530     D* DATA PULIZIA BOLLE ARRIVI CON RISERVA - POSTE
014000000530     D  POSARR               223    230  0
014100160601     D* DATA PULIZIA BOLLE ARRIVI CON C.A.    - POSTE
014200000530     D  POSBCA               231    238  0
014300160601     d
014400160601     d
014500040423     D* DATA PULIZIA BOLLE CON RECLAMI
014600040423     D  COMREC               239    246  0
014700961204     D*
014800920324     D                 DS
014900941207     D  ARBAAS                 1      4  0
015000941207     D  ARBMGS                 5      8  0
015100941207     D  ARBDSP                 1      8  0
015200020405     D                 DS
015300040813     D  dctAAc2                1      4  0
015400020405     D  dctmgc                 5      8  0
015500020405     D  dctdca                 1      8  0
015600941207     D*
015700990823     D* DS PER TIBS02DS- RICERCA TABELLE
015800990823     D DSBS02        E DS                  EXTNAME(TIBS02DS)
015900990823     D DSDN11        E DS                  EXTNAME(FIDN11DS)
016000941207     D*
016100000530     D OG143         E DS
016200101007     D DS2A          E DS
016300101007     D DS3A          E DS
016400020409     D DS5A2         E DS
016500020409     D DS5A3         E DS
016600141209     D Dmar          E DS
016700000000     D KPJBA         E DS
016800930507     D CNCR80        E DS
016900930507     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
017000050404     D FNLGZ5DS      E DS
017100040812      * ds per il controllo della presenza di CA per la spedizione richiesta
017200040812     d FIDN12DS      e ds
017300040812     d  skKey                 26    305    dim(20)
017400040812     d  skAnn                306    325    dim(20)
017500040812     d  skDca                326    485  0 dim(20)
017600040812     d  skFca                486    545  0 dim(20)
017700040812     d  skDch                546    705  0 dim(20)
017800040812     d  skCch                706    745    dim(20)
017900141209
018000150928     d FIDNCADS      e ds                  inz
018100040812
018200040812     d dsKey           ds
018300040812     d  dctaac                        4  0
018400040812     d  dctfil                        3  0
018500040812     d  dctnca                        7  0
018600040812      *
018700040423     D*-------------
018800040423     D* DS OGGETTO RECLAMO
018900040423     D*-------------
019000040423     D                 DS
019100040423     D  W_lnp                         3  0
019200040423     D  W_nrs                         2  0
019300040423     D  W_nsp                         7  0
019400040423     D  W_aas                         4  0
019500040423     D  Wspe                   1     16  0 inz
019600020404     D*
019700020404     D* DEFINIZIONE COSTANTI
019800020404     D*
019900020530     D CBGIAC          C                   CONST('Bolla giacente in fase')
020000020404     D CBRISE          C                   CONST('Bolla con riserva consegnata -
020100020404     D                                     il            ')
020200020530     D CBCANA          C                   CONST('Bolla con C.A. aperta - Fase')
020300020404     D CBCANC          C                   CONST('Bolla con C.A. chiusa il')
020400020404     D CBCANN1         C                   CONST('Presente C.A. chiusa il ')
020500020404     D CBCANN2         C                   CONST(' non ancora stampata nell''el-
020600020404     D                                     enco C.A. da archiviare')
020700080711     D CBcass          C                   CONST('C/Assegno non incassato')
020800020405     D CBercan         C                   CONST('Errore in cancellazione C.A. -
020900020405     D                                     n.')
021000020405     D CBcpdr          C                   CONST('Bolla presente su archivio co-
021100020404     D                                     nteggi padroncini')
021200080515     D CBChoprN        C                   CONST('Manca numero MANDATO INTROITO-
021300080515     D                                      ma c''è data')
021400080515     D CBChoprD        C                   CONST('Chiusura operatore non effett-
021500080515     D                                     uata')
021600020405     D CBinc           C                   CONST('Spedizione ancora da incassar-
021700020405     D                                     e')
021800020409     D CBncon          C                   CONST('Bolla non consegnata')
021900020411     D CBnconca        C                   CONST('Bolla non consegnata con C.A.-
022000080515     D                                     chiusa')
022100080515     D CBcrec          C                   CONST('Bolla con Reclamo APERTO')
022200040831     D CBdt1           C                   CONST('Bolla con data trasmissione <-
022300040831     D                                     data mandato di introito')
022400010508      *
022500160914     I*FNARBD00      01
022600160914     I*FNARBK00      02
022700160914     I*FIARBT00      03
022800160914     I*FNARBP00      04
022900160914     I*FNARBG00      05
023000160914     I*FNARBM00      06
023100160914     I*FNARBV00      07
023200160914     I*FNARBN00      08
023300930507     C*---------------------------------------------------------------*
023400950516     C* MI SALVO LA KPJBU
023500950516     C                   MOVEL     KPJBU         WSAJBU
023600950516     C*
023700020611     C* CARICO TABELLE
023800930507     C                   EXSR      CARTAB
023900930507     C*
024000920306     C                   Z-ADD     0             FIL               3 0
024100950526     C                   Z-ADD     1             WDCM
024200010508     C                   eval      *in30 = *off
024300920306     C*
024400941207    1C     *IN30         DOWEQ     *OFF
024500941207     C     FIL           SETGT     FNARB000
024600941207     C                   READ      FNARB000                               30
024700920306     C*
024800941207    2C     *IN30         IFEQ      *OFF
024900000724     C                   CLEAR                   SAVLNP
025000920324     C                   Z-ADD     ARBLNA        FIL
025100980722     C     ARBLNA        CHAIN     AZORG01L                           35
025200020207     C  N35              MOVE      ORGde3        og143
025300020207     C  n35              movel     §ogntw        lnantw
025400020207     C   35              CLEAR                   lnantw
025500000809     C*
025600020207     C     lnantw        IFEQ      'EEX'
025700020207     C     lnantw        OREQ      'EUP'
025800020207     C     lnantw        OREQ      'FED'
025900020211     C     lnantw        OREQ      'DPD'
026000000809     C                   Z-ADD     COMPBE        WDACNF            8 0
026100160531
026200160531     c* Se la linea di arrivo è a "rischio" devo tenerla molto di più
026300160531     c                   if        %lookup(arblna:lnaR)>0
026400160601     c                   z-add     dtpexp_ymd    WDACNF
026500160531     c                   endif
026600160531
026700000809     C                   ELSE
026800160531
026900000809     C                   Z-ADD     COMDPB        WDACNF            8 0
027000000809     C                   ENDIF
027100920306     C*
027200950526     C     KARB2         SETLL     FNARB000
027300941207     C     FIL           READE     FNARB000                               31
027400920306     C*
027500941207    3C     *IN31         DOWEQ     *OFF
027600160601
027700160601     c** mi serviva per le POSTE : ora non serve più
027800160601     C**   ARBLNP        IFNE      SAVLNP
027900160601     C**                 MOVE      ARBLNP        SAVLNP
028000160601     C**                 MOVE      '   '         lnpntw
028100160601     C**   ARBLNP        CHAIN     AZORG01L                           35
028200160601     C*+N35              MOVEL     ORGDE3        OG143
028300160601     C**N35              MOVEL     §ogntw        lnpntw
028400160601     C**                 ENDIF
028500160601
028600020211     c* pulizia bolle estere COMPRESA LA DPD
028700160601
028800020207     C     lnantw        IFEQ      'EEX'
028900020207     C     lnantw        OREQ      'EUP'
029000020207     C     lnantw        OREQ      'FED'
029100020211     C     lnantw        OREQ      'DPD'
029200160601     C* DATA PULIZIA BOLLE EXPORT
029300000724     C                   Z-ADD     COMPBE        WDAPUL            8 0
029400160601     C                   Z-ADD     dtsexp_ymd    WDAprt            8 0
029500160601
029600160601     c* Se la linea di arrivo è a "rischio" devo tenerla molto di più
029700160601     c                   if        %lookup(arblna:lnaR)>0
029800160601     c                   z-add     dtpexp_ymd    WDAPUL
029900160601     C                   Z-ADD     dtpexp_ymd    WDAprt            8 0
030000160601     c                   endif
030100000724     C                   ELSE
030200160601     C* DATA PULIZIA BOLLE ITALIE E IMPORT
030300000724     C                   Z-ADD     COMDPB        WDAPUL
030400020409     C                   Z-ADD     dtsita_ymd    WDAprt            8 0
030500000724     C                   ENDIF
030600930628     C*
030700950526     C* CONSIDERO SOLO LE BOLLE
030800961204     C*
030900000809     C* SE DATA CONSEGNA  > DATA PULIZIA PIU' ALTA LEGGO ALTRA LINEA
031000000809    5C     ARBDCM        IFGT      WDACNF
031100920306     C                   SETON                                        31
031200941207   X5C                   ELSE
031300000809     C* ALTRIMENTI ELABORO SE DATA CONSEGNA <= DATA PULIZIA
031400000809     C* SE INVECE E' MAGGIORE IGNORO QUESTO RECORD
031500000809   5AC     ARBDCM        IFLE      WDAPUL
031600930507     C*
031700970623     C                   CLEAR                   WOK
031800000530     C*
031900160601     C* IMPOSTO DATA
032000000530     C                   Z-ADD     COMARR        WDATA£            8 0
032100020409     C                   Z-ADD     dtsrsv_ymd    WDApr£            8 0
032200980722     C**
032300980722     C* SE BOLLA CON RISERVA PULISCO CONSIDERANDO ANCHE L'ALTRA DATA
032400101007     c                   setoff                                       33
032500101007     c                   z-add     1             yy
032600101007     c                   dow       ris(YY)<>*blanks
032700101007     c                   eval      wcev=ris(YY)
032800101007     c
032900101007     C     KEVB4         SETLL     FNEVB04L
033000101007     c                   if        %equal(fnevb04l)
033100101007     c                   seton                                        33
033200101007     c                   leave
033300101007     c                   else
033400101007     c                   endif
033500101007     c                   add       1             yy
033600101007     c                   enddo
033700101007     c
033800980722     C     *IN33         IFEQ      *ON
033900000530     C     ARBDCM        ANDGE     WDATA£
034000020404     c* stampo motivazione
034100020409     c     arbdsp        iflt      wdapr£
034200020404     c                   exsr      ovrprt
034300020404     c     *ymd          move      arbdcm        data_EUR
034400020404     c                   move      data_EUR      data_Alfa
034500020404     c                   movel(P)  cbrise        p01mes
034600020404     c                   eval      p01mes = %trim(p01mes) + ' ' + data_Alfa
034700020404     c                   exsr      ried1
034800020404     c                   write     lr84d1
034900020409     c                   endif
035000980722     C                   MOVE      'N'           WOK               1
035100980722     C                   ENDIF
035200990126     C**
035300990126     C* SE BOLLA CON C.A. PULISCO PER DATA CHIUSURA C.A.
035400990126     C                   CLEAR                   WDCH
035500020530     C                   CLEAR                   WDCH_dcb
035600160601     C* IMPOSTO DATA
035700020409     C                   Z-ADD     dtscan_ymd    WDApr£
035800020409     c*
035900040812      * Sostituisco la lettura del file FNDTC02 con
036000040812      * richiamo del programma FIDN12R passando la DS richiedendo le CA non annul
036100040812      * legate alla spedizione che passo
036200040812      *
036300040812     c                   clear                   fidn12ds
036400040812     c                   eval      i12aas = arbaas
036500040812     c                   eval      i12lnp = arblnp
036600040812     c                   eval      i12nrs = arbnrs
036700040812     c                   eval      i12nsp = arbnsp
036800040812     c                   eval      i12fel = 'E'
036900040812     c                   eval      i12fan = 'E'
037000040812      *
037100040812     c                   call      'FIDN12R'
037200040812     c                   parm                    fidn12ds
037300040812      *
037400040812      * se non ci sono errori
037500040812    6c                   if        o12err = ' '
037600040812      * se numero di CA trovate maggiore di zero
037700040812    7c                   if        o12nca > 0
037800040812     c
037900040812     c                   z-add     *zeros        ii                2 0
038000040812    8c     1             do        o12nca        ii
038100040812     c                   movea     skkey(ii)     dskey
038200040812     C                   Z-ADD     skdca(ii)     dctdca
038300040812     C                   Z-ADD     skdch(ii)     dctdch
038400040812     C                   movea     skcch(ii)     dctcch
038500040812      * se CA aperta stampo,imposto il flag e vado a fine
038600040812    9c                   If        dctdch = 0
038700040812     c* stampo motivazione
038800080325   10c**   arbdsp        iflt      wdapr£
038900080325     c**                 exsr      ovrprt
039000080325     c**                 movel(P)  cbcana        p01mes
039100040812     c* chaino file delle fasi per reperire fase, data, e p.o.
039200080325     c**   kdcf          setgt     fndcf01l
039300080325     c**   kdcf          readpe    fndcf01l
039400080325   11c**                 if        not %eof(fndcf01l)
039500080325     c**   *ymd          move      dcfdfc        data_EUR
039600080325     c**                 move      data_EUR      data_Alfa
039700040812     c* chaino file anagrafica fasi per decodificare la fase
039800080325     c**                 clear                   fndfa000
039900080325     c**   dcffca        chain     fndfa01l
040000080325   12c**                 if        %found(fndfa01l)
040100080325     c**                 movel     dfades        wfades           40
040200080325   12c**                 endif
040300080325  x11c**                 else
040400080325     c**                 clear                   data_Alfa
040500080325     c**                 clear                   dcffca
040600080325     c**                 clear                   dcffev
040700080325   11c**                 endif
040800080325     c**                 move      dcffca        wcffca            3
040900080325     c**                 move      dcffev        wcffev            3
041000080325     c**                 eval      p01mes = %trim(p01mes) + ' ' + wcffca
041100080325     c**                           + ' ' + wfades
041200080325     c**                           + ' del ' + data_Alfa + ' p.o. ' + wcffev
041300080325     c**                 exsr      ried1
041400080325     c**                 write     lr84d1
041500080325   10c**                 endif
041600040812     C                   MOVE      'N'           WOK               1
041700040812     c                   leave
041800040812  x 9c                   else
041900040812      * se CA chiusa
042000040812   10C     DCTDCH        IFGT      WDCH
042100040812     C                   Z-ADD     DCTDCH        WDCH
042200040812   10C                   ENDIF
042300040812     c* memorizzo la data chiusura più alta se p.o. C.A. = lna bolla
042400040812   10c     dctdch        ifgt      wdch_dcb
042500040812     c     dctfil        andeq     arblna
042600040812     c                   z-add     dctdch        wdch_dcb
042700040812   10c                   endif
042800040812    9C                   ENDIF
042900040812     c
043000040812    8c                   enddo
043100040812      *
043200040812    7c                   endif
043300040812    6c                   endif
043400040812     C*
043500160601     C* IMPOSTO DATA
043600000530     C                   Z-ADD     COMBCA        WDATA£            8 0
043700990126     C**
043800990126     C* SE C.A. TUTTE CHIUSE DEVONO ESSERE PASSATI I GIORNI DALLA DATA
043900990126     C*  CHIUSURA PIU' ALTA
044000990126     C     WOK           IFEQ      ' '
044100000530     C     WDCH          ANDGE     WDATA£
044200990126     C                   MOVE      'N'           WOK               1
044300990126     C                   ENDIF
044400990823     C* NON POSSO PULIRE BOLLA E C.A. SE NON L'HO ANCORA STAMPATA
044500990823     C* NELL'ELENCO C.A. DA ARCHIVIARE
044600990823    6C     WOK           IFEQ      ' '
044700020530     c     wdch_dcb      andgt     *zeros
044800990823     C                   CLEAR                   DSBS02
044900990824     C                   MOVEL     'C'           T02MOD
045000990823     C                   MOVEL     KNSIF         T02SIF
045100990823     C                   MOVEL     'DCB'         T02COD
045200990823     C                   MOVEL     ARBLNA        T02KE1
045300990823     C                   CALL      'TIBS02R'
045400990823     C                   PARM                    KPJBA
045500990823     C                   PARM                    DSBS02
045600990823    7C     T02ERR        IFEQ      *BLANKS
045700990823     C                   MOVEL     T02UNI        WDTACA            8 0
045800990823     C                   ELSE
045900990823     C                   Z-ADD     19990101      WDTACA
046000990823    7C                   ENDIF
046100020530    7C     WDCH_dcb      IFGT      WDTACA
046200020404     c* stampo motivazione
046300020409     c     arbdsp        iflt      wdapr£
046400020404     c                   exsr      ovrprt
046500020530     c     *ymd          move      wdch_dcb      data_EUR
046600020404     c                   move      data_EUR      data_Alfa
046700020404     c                   eval      p01mes = cbcann1 + data_alfa + cbcann2
046800020404     c                   exsr      ried1
046900020404     c                   write     lr84d1
047000020409     c                   endif
047100990823     C                   MOVE      'N'           WOK
047200990823    7C                   ENDIF
047300990823    6C                   ENDIF
047400990823     C*
047500970623   6AC     WOK           IFEQ      *BLANKS
047600050906      *
047700050906      * GIACENZA: se non trovata CANCELLO BOLLA
047800050906      *           se trovata ma è annullata o in fase 60 CANCELLO BOLLA
047900050906      *           negli altri casi non CANCELLO BOLLA
048000050906     C                   clear                   WGIACE            1
048100050906     C     KGCA          CHAIN     Tigcp21L
048200010508      *
048300050906     c                   SELECT
048400050906     c                   WHEN      not %found(Tigcp21L)
048500050906     c                   eval      *in33 = *off
048600050906     c                   WHEN      GCPATB <> *blanks or GCPFAS = 60
048700050906     c     kblp          Setll     Fnblp01l
048800050906     c                   eval      *in33 = *off
048900110210     c
049000110210     c                   If        Not %Equal(Fnblp01l)
049100050906     c                   Eval      wgiace = 'S'
049200050906     c                   EndIf
049300050906     c                   OTHER
049400020404     c* stampo motivazione
049500020409     c     arbdsp        iflt      wdaprt
049600020404     c                   exsr      ovrprt
049700020404     c                   movel(P)  cbgiac        p01mes
049800020530     c* prendo la decodifica fase
049900020530     C                   MOVEL     '2Y'          COD
050000050906     C                   MOVEL(P)  gcpfas        KEY
050100020530     C     ktab1         CHAIN     TABEL00F                           37
050200020530     C   37              MOVEL     *ALL'?'       decfas           45
050300020530     C  N37              MOVEL     TBLUNI        decfas
050400050906     c                   move      gcpfas        wcpfas            3
050500020530     c                   eval      p01mes = %trim(p01mes) + ' ' + wcpfas + '-'
050600020530     c                             + decfas
050700020404     c                   exsr      ried1
050800020404     c                   write     lr84d1
050900020409     c                   endif
051000050906      * Imposto flag x non pulire la bolla
051100050906     c                   MOVE      'N'           WOK
051200010508     c                   eval      *in33 = *on
051300050906     c                   ENDSL
051400020405     c*
051500020415     c  n33arbnmi        ifeq      *zeros
051600020405     c* stampo motivazione
051700020409     c     arbdsp        iflt      wdaprt
051800020405     c                   exsr      ovrprt
051900080515     c                   if        arbdmi>0
052000080515     c                   movel(P)  cbchoprn      p01mes
052100080515     c                   else
052200080515     c                   movel(P)  cbchoprd      p01mes
052300080515     c                   endif
052400080515     c
052500020405     c                   exsr      ried1
052600020405     c                   write     lr84d1
052700020409     c                   endif
052800020405     c                   else
052900020405     c     arbfip        ifeq      'I'
053000020405     c* stampo motivazione
053100020409     c     arbdsp        iflt      wdaprt
053200020405     c                   exsr      ovrprt
053300020405     c                   movel(P)  cbinc         p01mes
053400020405     c                   exsr      ried1
053500020405     c                   write     lr84d1
053600020409     c                   endif
053700020405     c                   endif
053800020405     c                   endif
053900920410     C*
054000920505     C* PULISCO SOLO SE NON E' DA TORNARE ALL'INCASSO
054100950110     C*   E SE HA GIA' IL MANDATO DI INTROITO
054200000927    6C  N33ARBFIP        IFNE      'I'
054300930308     C     ARBNMI        ANDNE     0
054400000927     C*
054500000927    7C     ARBICA        IFEQ      ' '
054600000927     C     ARBICA        OREQ      'R'
054700000927     C* VERIFICO SE PER CASO SI TRATTA DI UN FINTO RITORNO ALL'INCASSO
054800000927     C                   EXSR      CTRICA
054900000927    7C                   ENDIF
055000020405     c*
055100020405     c     arbica        ifeq      ' '
055200020405     c     arbica        oreq      'R'
055300020405     c* stampo motivazione
055400020409     c     arbdsp        iflt      wdaprt
055500020405     c                   exsr      ovrprt
055600020405     c                   movel(P)  cbinc         p01mes
055700020405     c                   exsr      ried1
055800020405     c                   write     lr84d1
055900020409     c                   endif
056000020405     c                   endif
056100000927     C*
056200000927   7AC     ARBICA        IFNE      ' '
056300000927     C     ARBICA        ANDNE     'R'
056400930308     C*
056500051115     C* AGGANCIO FiAR900F PER VEDERE SE C'E' IL C/ASSEGNO
056600051115     c                   setoff                                       34
056700051115     c     arbicc        ifeq      ' '
056800020405     c     arbicc        oreq      'R'
056900051115     C     KARB          SETLL     FiAR9000                               34
057000060123     c     *in34         ifeq      *on
057100060123     C     Karb          chain     tncsb03l
057200060123     c                   if        %found(tncsb03l)
057300060123     c* stato 9 --> Annullato; Data distinta > 0 --> c/a incassato
057400060123     c                   if        csbsta = 9 or csbddc > 0
057500060123     c                   setoff                                       34
057600060123     c                   endif
057700060123     c                   endif
057800060123     c                   endif
057900020405     c* stampo motivazione
058000080711    8c   34arbdsp        iflt      wdaprt
058100080711     c* Se si tratta di C/Ass omesso, verifico se devo davvero stampare
058200080711     c*  in base alla data di rigenerazione
058300080711     c                   clear                   w008a
058400080711     c                   clear                   w0080
058500080711     c                   eval      ktrc='8'
058600080711     c     kar4          chain(n)  fiar401l
058700080711    9c                   if        %found(fiar401l)
058800080711     c                   eval      Indx=%scan('RIGENERATA':ar4not)
058900080711   10c                   if        Indx>0
059000080711     c                   eval      ktrc='9'
059100080711     c     kar4          chain(n)  fiar401l
059200080711   11c                   if        %found(fiar401l)
059300080711     c                   eval      Indx=%scan('OMESSO':ar4not)
059400080711   12c                   if        Indx>0
059500080711     c                   eval      w008a=%subst(ar4not:26:8)
059600080711   13c                   if        w008a>*zeros
059700080711     c                   movel     w008a         w0080
059800080711   13c                   endif
059900080711   12c                   endif
060000080711   11c                   endif
060100080711   10c                   endif
060200080711    9c                   endif
060300080711     c
060400080711    9c                   if        w0080=0 or w0080<wdapul
060500020405     c                   exsr      ovrprt
060600080711     c                   if        w0080>0
060700020405     c                   movel(P)  cbcass        p01mes
060800080711     c                   eval      p01mes=%trim(p01mes)+' rigenerata il '+
060900080711     c                             %editw(w0080:'    /  /  ')
061000080711     c                   endif
061100080711     c
061200020405     c                   exsr      ried1
061300020405     c                   write     lr84d1
061400080711    9c                   endif
061500080711    8c                   endif
061600080711   7ac                   endif
061700941207     C*
061800950110     C* PULISCO SOLO SE NON C'E' IL C/ASSEGNO OPPURE SE C'E' MA E'
061900950110     C*   GIA' STATO INCASSATO
062000941212    7C     *IN34         IFEQ      *ON
062100930121     C     ARBICC        ANDNE     ' '
062200950119     C     ARBICC        ANDNE     'R'
062300941212     C     *IN34         OREQ      *OFF
062400950110     C*
062500950110     C* PULISCO SOLO SE NON TROVO RECORD NEI FILE TASSAZIONE PADRONCINI
062600020405     c                   clear                   p01mes
062700011121     C     KARB          SETLL     fiftd000                               36
062800020405     c     *in36         ifeq      *on
062900020405     c                   movel(P)  cbcpdr        p01mes
063000020405     c                   endif
063100011121     C  N36KARB          SETLL     fifsd000                               36
063200080711     c* visto che normalmente rimangono i record consegna e quindi scritti
063300080711     c*  in FTD per data consegna enon per data spedizione, stampo solo se
063400080711     c*  la data consegna < data stampa
063500020405     c     *in36         ifeq      *on
063600020409     c     arbdsp        andlt     wdaprt
063700080711     c     arbdcm        andeq     0
063800080711     c     *in36         oreq      *on
063900080711     c     arbdcm        andlt     wdaprt
064000080711     c     arbdcm        andgt     0
064100160620     c     arbddc        andlt     wdaprt
064200080711     c
064300020405     c     p01mes        ifeq      *blanks
064400020405     c                   movel(P)  cbcpdr        p01mes
064500020405     c                   eval      p01mes = %trim(p01mes) + ' di simulazione'
064600020405     c                   endif
064700020405     c                   exsr      ovrprt
064800020405     c                   exsr      ried1
064900020405     c                   write     lr84d1
065000020405     c                   endif
065100040831     c*
065200040831     c* Posso pulire solo le bolle:
065300040831     C* 1) le cui date di consegna sono state trasmesse in partenza e sede
065400040831     c*     arbft1 = 'T'
065500040831     c* in caso contrario stampo segnalazione
065600040831    8c     *IN36         IFEQ      *OFF
065700040831     c*
065800040831     c     arbft1        ifne      'T'
065900040831     c     arbdt1        andlt     arbdmi
066000040831     c                   eval      wok = 'N'
066100040831     c* stampo motivazione
066200040831     c     arbdsp        iflt      wdaprt
066300040831     c                   exsr      ovrprt
066400040831     c                   movel(P)  cbdt1         p01mes
066500040831     c                   exsr      ried1
066600040831     c                   write     lr84d1
066700040831     c                   endif
066800040831     c                   endif
066900950110     C*
067000991104     C****** CANCELLAZIONE C.A. *******
067100040831   8Ac     wok           ifeq      *blanks
067200991104     C* SE BOLLA LOCALE NON CANCELLO LA C.A. SE PRESENTE BOLLA IN BLP
067300020611     c                   eval      *in96 = *off
067400020611     c     karb          setll     fnblp01l                               96
067500991104     C**
067600020611     c                   if        not *in96
067700991104     C**
067800991104     C                   MOVE      *BLANKS       WERR              1
067900040812      * Sostituisco la lettura del file FNDTC02 con
068000040813      * richiamo del programma FIDN12R passando la DS richiedendo le CA
068100040812      * legate alla spedizione che passo
068200040812      *
068300040812     c                   clear                   fidn12ds
068400040812     c                   eval      i12aas = arbaas
068500040812     c                   eval      i12lnp = arblnp
068600040812     c                   eval      i12nrs = arbnrs
068700040812     c                   eval      i12nsp = arbnsp
068800040812     c                   eval      i12fel = 'E'
068900040812      *
069000040812     c                   call      'FIDN12R'
069100040812     c                   parm                    fidn12ds
069200040812      *
069300040812      * se non ci sono errori
069400040812     c                   if        o12err = ' '
069500040812      * se numero di CA trovate maggiore di zero
069600040812     c                   if        o12nca > 0
069700040812     c
069800040812     c                   z-add     *zeros        ii                2 0
069900040812     c     1             do        o12nca        ii
070000040812     c                   movea     skkey(ii)     dskey
070100040812     C                   Z-ADD     skdca(ii)     dctdca
070200040813     C                   CLEAR                   DSDN11
070300040813     C                   MOVEL     'D'           I11MOD
070400040813     C                   Z-ADD     DCTAAC        I11AAC
070500040813     C                   Z-ADD     DCTFIL        I11FIL
070600040813     C                   Z-ADD     DCTNCA        I11NCA
070700040813     C                   CALL      'FIDN11R'
070800040813     C                   PARM                    KPJBA
070900040813     C                   PARM                    DSDN11
071000040813   11C     O11ERR        IFEQ      'E'
071100040813     C                   MOVEL     '1'           WERR
071200040813   11C                   END
071300040813      *
071400040813     c                   enddo
071500040813      *
071600040813     c                   endif
071700040813     c                   endif
071800991104     C**
071900991104     C* SE NON SONO RIUSCITA A CANCELLARE TUTTE LE C.A. NON POSSO
072000991104     C* CANCELLARE LA BOLLA
072100991104   10C     WERR          IFNE      *BLANKS
072200020405     c* stampo motivazione
072300020405     c                   exsr      ovrprt
072400020405     c                   movel(P)  cbercan       p01mes
072500020405     c     *ymd          move      dctdca        data_EUR
072600020405     c                   move      data_EUR      data_Alfa
072700020405     c                   eval      p01mes = %trim(p01mes) + %editc(dctfil:'Z')
072800020405     c                             + %editc(dctnca:'Z') + 'del'  + data_Alfa
072900020405     c                   exsr      ried1
073000020405     c                   write     lr84d1
073100991104     C                   MOVE      'N'           WOK
073200991104   10C                   ENDIF
073300991104    9C                   ENDIF
073400040423      *
073500040423      * RECLAMI : se non trovata CANCELLO BOLLA
073600040423      *
073700040423     C*
073800040423    1C     WOK           IFEQ      ' '
073900040423     c                   movel     arbaas        W_aas
074000040423     c                   movel     arblnp        W_lnp
074100040423     c                   movel     arbnrs        W_nrs
074200040423     c                   movel     arbnsp        W_nsp
074300040423      *
074400040423     c                   movel     wspe          ktgogg
074500040423      *
074600150929     c     ktgd          setll     fitgd000
074700040423     c                   do        *hival
074800150929     c     ktgd          reade     fitgd000
074900040423
075000150929     c                   if        %eof(fitgd32l)
075100040423     c                   leave
075200040423     c                   endif
075300040423      * se annullato leggo il successivo
075400040423     c                   If        TGDatb <> ' '
075500040423     c                   iter
075600040423     c                   endif
075700141209     c                   select
075800141209      * se ancora aperto ma si può chiudere: cancello R.A.,
075900141209      *                                      chiudo eventuale dispo di cons da TIIDC
076000141209     c                   when      tgddcd = 0 and %lookup(tgdmad:mach)>0
076100150928     c                   clear                   fidnCAds
076200141209     c                   eval      IC0ANT=tgdant
076300141209     c                   eval      IC0NUT=tgdnut
076400141209     c                   eval      IC0PDT=999
076500150928     c                   call      'FIDNCAR'
076600150928     c                   parm                    fidnCAds
076700141209     c     kidc          setll     tiidc01l
076800141209     c     kidc          reade     tiidc01l
076900141209     c                   dow       not %eof(tiidc01l)
077000141209     c                   if        idcaNT=TGDANT and idcnuT=tgdnut and
077100141209     c                             idcpdt=tgdpdt and idcelaflg='E'
077200141209     c                   eval      IDCELADATA=%int( %subst( %char( %dec
077300141209     c                                        ( %timestamp() ) ) :1 :14 ) )
077400141209     c                   eval      idcelaute=knmus
077500141209     c                   update    tiidc000
077600141209     c                   else
077700141209     c                   unlock    tiidc01l
077800141209     c                   endif
077900141209     c     kidc          reade     tiidc01l
078000141209     c                   enddo
078100040423      * verifico se reclamo ancora aperto
078200151022     c                   when      (TGDdcd = 0 and tgdpog<>046) or
078300040423      * se reclamo chiuso verifico la data chiusura con la dta di pulizia
078400040423     c                             TGDdcd > comrec
078500040423     c                   eval      wok = 'N'
078600080515     c
078700080515     c* stampo motivazione  solo se aperto
078800080515     c                   If        TGDdcd = 0
078900040423     c                   exsr      ovrprt
079000040423     c                   movel(P)  cbcrec        p01mes
079100040423     c                   exsr      ried1
079200040423     c                   write     lr84d1
079300040423     c                   leave
079400080515     c                   endif
079500080515     c
079600141209     c                   endsl
079700040423
079800040423     c                   enddo
079900040423
080000040423    1c                   endif
080100991104     C****** PULIZIA ALTRI FILE ********+
080200991104    9C     WOK           IFEQ      ' '
080300020411     C                   EXSR      PULIZ
080400151022     c                   add       1             pulbol            9 0
080500991104    9C                   ENDIF
080600991104     C**
080700040831   8Ac                   endif
080800991104    8C                   ENDIF
080900991104     C**
081000941207    7C                   ENDIF
081100000927   7AC                   ENDIF
081200941207    6C                   ENDIF
081300970623   6AC                   ENDIF
081400920306     C*
081500000809   5AC                   ENDIF
081600941207     C     FIL           READE     FNARB000                               31
081700941207    5C                   ENDIF
081800950118     C*
081900941207     C*
082000941207    3C                   ENDDO
082100941207    2C                   ENDIF
082200941207    1C                   ENDDO
082300020404     c*
082400020409     C                   Z-ADD     0             FIL               3 0
082500020409     C                   Z-ADD     0             WDCM
082600020409     C                   eval      *in30 = *off
082700020409     C*
082800020409    1C     *IN30         DOWEQ     *OFF
082900020409     C     FIL           SETGT     FNARB000
083000020409     C                   READ      FNARB000                               30
083100020409     C*
083200020409    2C     *IN30         IFEQ      *OFF
083300020409     C                   Z-ADD     ARBLNA        FIL
083400020409     C*
083500020409     C     KARB2         SETLL     FNARB000
083600020409     C     KARB2         READE     FNARB000                               31
083700020409     C*
083800020409    3C     *IN31         DOWEQ     *OFF
083900080515    4c     arbdsp        iflt      dtsncn_ymd
084000020411     c* Guardo se presente c.a.
084100040813      * Sostituisco la lettura del file FNDTC02 con
084200040813      * richiamo del programma FIDN12R passando la DS richiedendo le CA
084300040813      * legate alla spedizione che passo
084400040813      *
084500040813     c                   clear                   fidn12ds
084600040813     c                   eval      i12aas = arbaas
084700040813     c                   eval      i12lnp = arblnp
084800040813     c                   eval      i12nrs = arbnrs
084900040813     c                   eval      i12nsp = arbnsp
085000040813     c                   eval      i12fel = 'E'
085100040813      *
085200040813     c                   call      'FIDN12R'
085300040813     c                   parm                    fidn12ds
085400040813      *
085500040813      * se non ci sono errori  e num.ca magg. di zero accendo il 32
085600080515     c                   clear                   wchiusaCA         1
085700120217     C                   CLEAR                   WDCH_dcb
085800080515    5c                   if        o12err = ' ' AND o12nca > 0
085900040813     c                   seton                                        32
086000080515     c                   eval      WchiusaCA='S'
086100080515     c
086200080515     c* Stampo solo se tutte le c.a. sono chiuse
086300080515     c                   z-add     *zeros        ii                2 0
086400080515    6c     1             do        o12nca        ii
086500080515     c                   movea     skkey(ii)     dskey
086600080515     C                   Z-ADD     skdca(ii)     dctdca
086700080515     C                   Z-ADD     skdch(ii)     dctdch
086800080515     C                   movea     skcch(ii)     dctcch
086900080515     c
087000080515      * se CA chiusa stampo,imposto il flag e vado a fine
087100080515    7c                   If        dctdch = 0
087200080515     c                   clear                   wchiusaCA
087300120217     c                   else
087400120217     c* memorizzo la data chiusura più alta se p.o. C.A. = lna bolla
087500120217    8c     dctdch        ifgt      wdch_dcb
087600120217     c     dctfil        andeq     arblna
087700120217     c                   z-add     dctdch        wdch_dcb
087800120217    8c                   endif
087900080515    7c                   endif
088000080515    6c                   enddo
088100080515    5c                   endif
088200080515     c
088300040813      *
088400080515    c
088500080515     c                   if        (*in32 and wchiusaCA='S' and
088600120222    5c                             wdch_dcb<dtsgdc_ymd and arbdsp<dtscan_ymd)
088700120217    5c                             or not *in32
088800080515     c
088900020409     c                   exsr      ovrprt
089000020411     c  n32              movel(P)  cbncon        p01mes
089100020411     c   32              movel(P)  cbnconca      p01mes
089200020409     c                   exsr      ried1
089300020409     c                   write     lr84d1
089400080515    5c                   endif
089500080515    4c                   endif
089600020409     C*
089700020409     C     karb2         READE     FNARB000                               31
089800020409     C*
089900020409    3C                   ENDDO
090000020409    2C                   ENDIF
090100020409    1C                   ENDDO
090200020409     c*
090300151022     c* stampo totale bolle pulite
090400151022     c                   clear                   p1clnp
090500151022     c                   clear                   p1clna
090600151022     c                   clear                   p1cnrs
090700151022     c                   clear                   p1cnsp
090800151022     c                   clear                   p1cdsp
090900151022     c                   clear                   p1ccbo
091000151022     c                   clear                   p1ccca
091100151022     c                   clear                   p1cdcm
091200151022     c                   clear                   p01mes
091300151022     c                   evaL      P01MES='Bolle cancellate:'+
091400151022     c                             %editc(pulbol:'2')
091500151022     c                   exsr      ovrprt
091600151023     c                   write     lr84d1
091700151022
091800020404     c     wopn          ifeq      '1'
091900020404     c                   write     lr84z1
092000020404     c                   endif
092100990823     C*
092200990823     C* CHIUDO CON LR
092300990823     C                   CLEAR                   DSBS02
092400990823     C                   MOVEL     'C'           T02TLA
092500990823     C                   CALL      'TIBS02R'
092600990823     C                   PARM                    KPJBA
092700990823     C                   PARM                    DSBS02
092800040813     C*
092900040813     C                   MOVEL     'C'           I12TLA
093000040813     C                   CALL      'FIDN12R'
093100040813     C                   PARM                    FIDN12DS
093200150928      *
093300150928     c                   clear                   fidnCAds
093400150928     c                   eval      IC0tla = 'C'
093500150928     c                   call      'FIDNCAR'
093600150928     c                   parm                    fidnCAds
093700920205     C*
093800950516     C                   MOVEL     WSAJBU        KPJBU
093900920205     C                   SETON                                        LR
094000930628     C*---------------------------------------------------------------*
094100930628     C*                    PULIZ                                      *
094200930628     C* ROUTINE DI PULIZIA FILE BOLLE E FILE COLLEGATI                *
094300930628     C*---------------------------------------------------------------*
094400920324     C     PULIZ         BEGSR
094500060502     c* Chaino file fisico: se riesco pulisco, altrimenti leggo altra bolla
094600060502     c     arbnrr        chain(E)  fnarb00f
094700060502     c                   if        not %error and %found(fnarb00f)
094800941207     C*
094900051115     c                   if        not *in96
095000060220     C*
095100060220     C****  FIAR400F ---> ESTENSIONE DESCRIZIONI  ****
095200060220     C     KARB          SETLL     FIAR4000                               32
095300060220    1C     *IN32         IFEQ      *ON
095400060220     C     KARB          READE     FIAR4000                               32
095500060220     C*
095600060220    2C     *IN32         DOWEQ     *OFF
095700060220     C                   DELETE    FIAR4000
095800060220     C     KARB          READE     FIAR4000                               32
095900060220    2C                   ENDDO
096000060220    1C                   ENDIF
096100060220     C****  FiAR900F ---> DATI C/ASSEGNO  ****
096200051115     C     KARB          CHAIN     FiAR9000                           32
096300051115     C  N32              DELETE    FiAR9000
096400051115     c                   endif
096500941207     C*
096600930628     C*
096700941207     C****  FNART00F ---> DETTAGLIO COLLI  ****
096800941207     C     KARB          SETLL     FNART000
096900941207     C     KARB          READE     FNART000                               32
097000920324     C*
097100941207    1C     *IN32         DOWEQ     *OFF
097200961125     C* CANCELLO GLI EVENTI SEGNACOLLI SENZA NUMERO SPEDIZIONE
097300961125     C                   SETOFF                                       36
097400961125     C     KART          SETLL     FNEVS02L
097500961125    2C     *IN36         DOWEQ     *OFF
097600961125     C     KART          READE     FNEVS02L                               36
097700961125     C  N36              DELETE    FNEVS002
097800961125    2C                   ENDDO
097900051012     c
098000980331     C* CANCELLO L'ESTENSIONE DETTAGLIO SEGNACOLLO
098100051012     c* solo se non più presente la bolla in fnblp
098200051012     c                   if        not *in96
098300051012     c
098400980331     C                   SETOFF                                       36
098500051012     C     KART          SETLL     FIARS01L
098600980331    2C     *IN36         DOWEQ     *OFF
098700051012     C     KART          READE     FIARS01L                               36
098800051012     C  N36              DELETE    FIARS000
098900980331    2C                   ENDDO
099000051012     c
099100051012     c                   endif
099200961125     C*
099300941207     C                   DELETE    FNART000
099400941207     C     KARB          READE     FNART000                               32
099500941207    1C                   ENDDO
099600930628     C*
099700961125     C****  FNEVS00F ---> EVENTI SEGNACOLLI  ****
099800961125     C* CANCELLO GLI EVENTI SEGNCOLLI CON NUMERO SPEDIZIONE
099900961125     C     KARB          SETLL     FNEVS01L
100000961125     C     KARB          READE     FNEVS01L                               32
100100961125     C*
100200961125    1C     *IN32         DOWEQ     *OFF
100300961125     C                   DELETE    FNEVS000
100400961125     C     KARB          READE     FNEVS01L                               32
100500961125    1C                   ENDDO
100600991104     C*
100700991104     C****  FIARI00F ---> FILE DEGLI INCASSI  ****
100800991104     C     KARB          SETLL     FIARI000                               32
100900991104    1C     *IN32         IFEQ      *ON
101000991104     C     KARB          READE     FIARI000                               32
101100991104     C*
101200991104    2C     *IN32         DOWEQ     *OFF
101300991104     C                   DELETE    FIARI000
101400991104     C     KARB          READE     FIARI000                               32
101500991104    2C                   ENDDO
101600991104    1C                   ENDIF
101700061107     C*
101800061107     C****  FIARG00F ---> FILE GEOREFER/GIRI DI CONSEGNA
101900061107     C     KARB          SETLL     FIARG000                               32
102000061107    1C     *IN32         IFEQ      *ON
102100061107     C     KARB          READE     FIARG000                               32
102200061107     C*
102300061107    2C     *IN32         DOWEQ     *OFF
102400061107     C                   DELETE    FIARG000
102500061107     C     KARB          READE     FIARG000                               32
102600061107    2C                   ENDDO
102700061107    1C                   ENDIF
102800070503     C*
102900070503     C****  FIARP00F ---> FILE proposte DCr che dovrebbe essere vuoto
103000070503     C     KARB          SETLL     FIARP000                               32
103100070503    1C     *IN32         IFEQ      *ON
103200070503     C     KARB          READE     FIARP000                               32
103300070503     C*
103400070503    2C     *IN32         DOWEQ     *OFF
103500070503     C                   DELETE    FIARP000
103600070503     C     KARB          READE     FIARP000                               32
103700070503    2C                   ENDDO
103800070503    1C                   ENDIF
103900070503     C*
104000070503     C****  FIARN00F ---> FILE note LDV
104100070503     C     KARB          SETLL     FIARN000                               32
104200070503    1C     *IN32         IFEQ      *ON
104300070503     C     KARB          READE     FIARN000                               32
104400070503     C*
104500070503    2C     *IN32         DOWEQ     *OFF
104600070503     C                   DELETE    FIARN000
104700070503     C     KARB          READE     FIARN000                               32
104800070503    2C                   ENDDO
104900070503    1C                   ENDIF
105000140214     C****  FISIA00F ---> FILE spia per invio Alert FD e L.avviso
105100140214     C     KARB          SETLL     FIsia01l
105200140214    1C                   if        %equal(fisia01l)
105300140214     C     KARB          READE     FIsia01l
105400140214     C*
105500140214    2C                   DOW       not %eof(fisia01l)
105600140214     C                   DELETE    FIsia000
105700140214     C     KARB          READE     fisia01l
105800140214    2C                   ENDDO
105900140214    1C                   ENDIF
106000961125     C*
106100941207     C*****
106200160914     C***  TASSAZIONE
106300941207     C*****
106400930602     C*
106500971002     C* NON PULISCO I LOCALI E I SUOI EVENTI SE LA BOLLA PARTENZA
106600971002     C*  C'E' ANCORA
106700030108     C* (Ho cancellato la lokup su l1 e la setll su fnblp perchè
106800030108     C* le stesse specifiche sono già state eseguite per gestire
106900030108     C* la cancellazione di C.A. di bolla locale - (vedi sopra))
107000020611     c                   if        not *in96
107100991104     C**
107200991104     C****  FIAR600F ---> TASSAZIONE BOLLA  ****
107300991104     C     KARB          CHAIN     FIAR6000                           32
107400991104    2C     *IN32         DOWEQ     *OFF
107500991104     C*
107600991104     C****  FIAR700F ---> ESTENSIONE TASSAZIONE BOLLA  ****
107700991104     C     KARB7         SETLL     FIAR7000                               35
107800991104    3C     *IN35         IFEQ      *ON
107900991104     C     KARB7         READE     FIAR7000                               35
108000991104     C*
108100991104    4C     *IN35         DOWEQ     *OFF
108200991104     C                   DELETE    FIAR7000
108300991104     C     KARB7         READE     FIAR7000                               35
108400991104    4C                   ENDDO
108500991104    3C                   ENDIF
108600991104     C*
108700991104     C                   DELETE    FIAR6000
108800991104     C**
108900991104     C     KARB          READE     FIAR6000                               32
109000991104    2C                   ENDDO
109100930602     C*
109101160914     c* Le variazioni bolla vengono spostate in UNITRAGRU e verranno
109102160914     c* quindi pulite con pgm apposito
109200160914     C***                SETOFF                                       010203
109300160914     C***                SETOFF                                       040506
109400160914     C***                SETOFF                                       0708
109500160914     C***  KARB          SETLL     FIARBF2C                               32
109600160914    2C***  *IN32         IFEQ      *ON
109700160914     C***  KARB          READE     FIARBF2C                               32
109800920324     C*
109900160914    3C***  *IN32         DOWEQ     *OFF
110000160914     C***01              DELETE    FNARBD00
110100160914     C***02              DELETE    FNARBK00
110200950630     C*
110300160914    4C***  *IN03         IFEQ      *ON
110400950630     C* SE C'E' LA 3° VARIA CONTROLLO SE CE NE SONO ANCHE IN FNARBU0F
110500160914    5C***  ARBSV3        IFNE      ' '
110600950630     C*
110700160914     C***  KARB          SETLL     FIARBU00                               35
110800160914    6C***  *IN35         IFEQ      *ON
110900160914     C***  KARB          READE     FIARBU00                               35
111000950630     C*
111100160914    7C***  *IN35         DOWEQ     *OFF
111200160914     C***                DELETE    FIARBU00
111300160914     C***  KARB          READE     FIARBU00                               35
111400160914    7C***                ENDDO
111500160914    6C***                ENDIF
111600160914    5C***                ENDIF
111700950630     C*
111800160914     C***                DELETE    FIARBT00
111900160914    4C***                ENDIF
112000950630     C*
112100160914     C***04              DELETE    FNARBP00
112200160914     C***05              DELETE    FNARBG00
112300160914     C***06              DELETE    FNARBM00
112400160914     C***07              DELETE    FNARBV00
112500160914     C***08              DELETE    FNARBN00
112600160914     C***                SETOFF                                       010203
112700160914     C***                SETOFF                                       040506
112800160914     C***                SETOFF                                       0708
112900160914     C***  KARB          READE     FIARBF2C                               32
113000160914    3C***                ENDDO
113100160914    2C***                ENDIF
113200030108     C*
113300030108     C****  FIAR500F ---> PULIZIA ESTENSIONI TESTATA BOLLA  ****
113400030108     C     KARB          setll     FIAR5000                               32
113500030108    2C                   if        *IN32
113600030108     C     KARB          reade     FIAR5000                               32
113700030108    3C                   dow       not *IN32
113800030108     C                   DELETE    FIAR5000
113900030108     C     KARB          reade     FIAR5000                               32
114000030108    3C                   enddo
114100030108    2C                   endif
114200040604     c*
114300040604     C****  FIAR800F ---> PULIZIA ESTENSIONI TESTATA BOLLA  ****
114400040604     C     KARB          setll     FIAR8000                               32
114500040604    2C                   if        *IN32
114600040604     C     KARB          reade     FIAR8000                               32
114700040604    3C                   dow       not *IN32
114800040604     C                   DELETE    FIAR8000
114900040604     C     KARB          reade     FIAR8000                               32
115000040604    3C                   enddo
115100040604    2C                   endif
115200930628     C*
115300990806     C****  FNEVB00F ---> PULIZIA EVENTI  ****
115400991104     C     KARB          SETLL     FNEVB01L                               32
115500941207    2C     *IN32         IFEQ      *ON
115600040830     c* se bolla presente in titas non posso pulire
115700040830     c     karb          setll     titas30c
115800040831     c                   if        not %equal(titas30c)
115900991104     C     KARB          READE     FNEVB01L                               32
116000941207    3C     *IN32         DOWEQ     *OFF
116100990806     C                   DELETE    FNEVB01L
116200131029     C     KARB2f        chain     FNEVB22L
116300131029     c                   if        %found(fnevb22l)
116400131029     c                   delete    fnevb22l
116500131029     c                   endif
116600991104     C     KARB          READE     FNEVB01L                               32
116700941207    3C                   ENDDO
116800040831     c                   endif
116900941207    2C                   ENDIF
117000930628     C*
117100941207    1C                   ENDIF
117200920324     C**
117300060502     C* CANCELLO LA BOLLA dal file fisico fnarb00F
117400920324     C**
117500060502     C                   DELETE    FNARBfis
117600010508     C**
117700050906      * Pulizia Giacenza: Se wgiace = 'S' vuol dire che ho giacenza chiusa e non c'è
117800050906      *                   la bolla originale su FNBLP in questo caso posso pulire la giacenza
117900050906     c                   If        WGIACE = 'S'
118000050404     c                   z-add     arbaas        iz5aas
118100050404     c                   z-add     arblnp        iz5lnp
118200050404     c                   z-add     arbnrs        iz5nrs
118300050404     c                   z-add     arbnsp        iz5nsp
118400050404     c                   call      'FNLGZ5C'
118500050404     c                   PARM                    FNLGZ5DS
118600050404---  C                   ENDIF
118700060502     c
118800060502---  C                   ENDIF
118900010508      *
119000920306     C                   ENDSR
119100000927     C*---------------------------------------------------------------*
119200000927     C* CONTROLLO SE SI TRATTA DI VERO RITRONO ALL'INCASSO
119300000927     C*---------------------------------------------------------------*
119400000927     C     CTRICA        BEGSR
119500000927     C* VERIFICO SE BOLLA SINGOLA O DOPPIA
119600000927     C     ARBCBO        LOOKUP    CBO                                    32
119700000927     C**
119800000927     C* BOLLA SINGOLA IN ASSEGNATO
119900000927    1C     *IN32         IFEQ      *OFF
120000000927     C                   MOVE      ARBKSC        W0040             4 0
120100000927    2C     W0040         IFNE      9999
120200000927     C* CHAIN SU FIAR6 PER VEDERE SE C'E' FATTURA IMMEDIATA
120300000927     C                   MOVEL     '1'           AR6TRC
120400000927     C     KARB7         CHAIN(N)  FIAR601L                           34
120500000927     C**
120600000927     C* SE NON C'E' O C'E' MA SENZA FATT IMMEDIATA, E' ERRATO
120700000927    3C     *IN34         IFEQ      *ON
120800000927     C     AR6DFT        OREQ      0
120900000927     C     AR6NFT        ANDEQ     0
121000000927     C                   MOVEL     'Y'           ARBICA
121100000927    3C                   ENDIF
121200000927    2C                   ENDIF
121300000927     C**
121400000927   X1C                   ELSE
121500000927     C* CHAIN SU FIAR6 PER VEDERE SE C'E' FATTURA IMMEDIATA
121600000927     C                   MOVEL     '2'           AR6TRC
121700000927     C     KARB7         CHAIN(N)  FIAR601L                           34
121800000927     C**
121900000927     C* SE CE' ED E' S.F. E' FINOTO
122000000927    2C     *IN34         IFEQ      *OFF
122100000927     C     AR6DFT        ANDEQ     0
122200000927     C     AR6NFT        ANDEQ     0
122300000927     C                   MOVE      AR6KSC        W0040
122400000927     C**
122500000927    3C     W0040         IFNE      9999
122600000927     C                   MOVEL     'Y'           ARBICA
122700000927    3C                   ENDIF
122800000927    2C                   ENDIF
122900000927    1C                   ENDIF
123000000927     C                   ENDSR
123100930628     C*---------------------------------------------------------------*
123200020611     C* ROUTINE DI CARICAMENTO TABELLE                                *
123300930628     C*---------------------------------------------------------------*
123400930507     C     CARTAB        BEGSR
123500000927     C*
123600000927     C* CARICO I TIPI BOLLA DOPPI
123700000927     C                   CLEAR                   YY
123800000927     C                   MOVEL     '3A'          COD
123900000927     C     KTAB          SETLL     TABEL
124000000927     C     KTAB          READE     TABEL                                  30
124100000927     C     *IN30         DOWEQ     *OFF
124200000927     C     TBLFLG        IFEQ      ' '
124300000927     C                   MOVEL     TBLUNI        DS3A
124400000927     C     §3ATB2        IFNE      *BLANKS
124500000927     C                   ADD       1             YY                3 0
124600000927     C                   MOVEL     TBLKEY        CBO(YY)
124700000927     C                   ENDIF
124800000927     C                   ENDIF
124900000927     C     KTAB          READE     TABEL                                  30
125000000927     C                   ENDDO
125100930628     C*
125200101007     c* Carico gli eventi di riserva dalla tabella 2A
125300101007     C                   CLEAR                   YY
125400101007     C                   MOVEL     '2A'          COD
125500101007     C     KTAB          SETLL     TABEL
125600101007     C     KTAB          READE     TABEL                                  30
125700101007     C     *IN30         DOWEQ     *OFF
125800101007     C     TBLFLG        IFEQ      ' '
125900101007     C                   MOVEL     TBLUNI        DS2A
126000101007     C     §2Atip        IFeq      'R'
126100101007     C                   ADD       1             YY                3 0
126200101007     C                   MOVEL     TBLKEY        ris(YY)
126300101007     C                   ENDIF
126400101007     C                   ENDIF
126500101007     C     KTAB          READE     TABEL                                  30
126600101007     C                   ENDDO
126700141209     c* Carico motivi apertura R.A. che si possono chiudere se ancora aperti
126800141209     c                   clear                   yy
126900141209     c     'MAR'         setll     tntbe01l
127000141209     c     'MAR'         reade     tntbe01l
127100141209     c                   dow       not %eof(tntbe01l)
127200141209     c                   movel     tbeuni        dmar
127300141209     c                   if        §MARCANPA='S'
127400141209     c                   eval      yy+=1
127500141209     c                   movel     tbeke1        mach(yy)
127600141209     c                   endif
127700141209     c     'MAR'         reade     tntbe01l
127800141209     c                   enddo
127900930507     C                   ENDSR
128000020404     C*---------------------------------------------------------------*
128100020404     C* OVRPRT - Operazioni su file printer                           *
128200020404     C*---------------------------------------------------------------*
128300020404     C     ovrprt        BEGSR
128400020404     c* override su file printer + apertura
128500020404     c     wopn          ifeq      *blanks
128600020409     C                   MOVEl     cmd(1)        COMMAN          100
128700020409     c                   eval      %subst(cmd(2):6:4) = whld
128800020409     c                   eval      %subst(cmd(2):17:4) = wsav
128900020409     c                   eval      comman = %trim(comman) + '('
129000020409     c                             + %trim(§5alib) + '/'
129100020409     c                             + %trim(§5aotq) + ') ' + %trim(cmd(2))
129200020409     C                   Z-ADD     100           LUNGH
129300020404     C                   CALL      'QCMDEXC'
129400020404     C                   PARM                    COMMAN
129500020404     C                   PARM                    LUNGH            15 5
129600020404     c                   open      fnlr84p
129700020404     c* valorizzazione campi di stampa testata
129800020404     c* data bolle lnp italia
129900020404     c     *ymd          move      comdpb        data_dmy
130000020404     c                   move      data_dmy      dtpita
130100020404     c* data bolle import
130200020404     c     *ymd          move      compbe        data_dmy
130300020404     c                   move      data_dmy      dtpexp
130400020404     c* data bolle con riserva
130500020404     c     *ymd          move      comarr        data_dmy
130600020404     c                   move      data_dmy      dtprsv
130700020404     c* data bolle con c.a.
130800020404     c     *ymd          move      combca        data_dmy
130900020404     c                   move      data_dmy      dtpcan
131000020404     c*
131100020404     c                   write     lr84t1
131200020404     c                   movel     '1'           wopn
131300020404     c                   endif
131400020404     c* stampa intestazione a rottura
131500020404     c   99              write     lr84t1
131600020404     c                   setoff                                       99
131700020404     c                   endsr
131800020404     C*---------------------------------------------------------------*
131900020404     C* RIED1  - Valorizzazione campi di stampa dettaglio             *
132000020404     C*---------------------------------------------------------------*
132100020404     C     RIED1         BEGSR
132200020404     c                   z-add     arblnp        p1clnp
132300020404     c                   z-add     arblna        p1clna
132400020404     c                   z-add     arbnrs        p1cnrs
132500020404     c                   z-add     arbnsp        p1cnsp
132600080515     c                   MOVEL     arbCBO        p1cCBO
132700080515     c                   MOVEL     arbCCA        p1cCCA
132800020404     c     *ymd          move      arbdsp        data_EUR
132900020404     c                   move      data_EUR      p1cdsp
133000080519     c                   if        arbdcm>0
133100080515     c     *ymd          move      arbDCM        data_EUR
133200080515     c                   move      data_EUR      p1cDCM
133300080519     c                   else
133400080519     c                   clear                   p1cdcm
133500080519     c                   endif
133600020404     c                   endsr
133700010508     C*---------------------------------------------------------------*
133800010508     C* ROUTINE INIZIALE
133900010508     C*---------------------------------------------------------------*
134000010508     C     *inzsr        BEGSR
134100010508      *
134200010508     C     *ENTRY        PLIST
134300010508     C                   PARM                    KPJBA
134400010508     C                   MOVEL     KPJBU         PARAM
134500010508     C                   Z-ADD     1             CODUT
134600010508     C                   CALL      'X§PARUT'
134700010508     C                   PARM                    UT§DSE
134800010508     C                   MOVEL     REC80         CNCR80
134900020404     c                   movel     ragut         rsut
135000020404     C*
135100020404     C                   TIME                    wtime            14 0
135200020404     C                   MOVE      WTIME         WDATE             8 0
135300020409     c     *eur          move      wdate         wdate_inv
135400120216     c     *eur          move      wdate         wdateu
135500010508     C*---------------------------------------------------------------*
135600010508     C     KTAB          KLIST
135700010508     C                   KFLD                    CODUT
135800010508     C                   KFLD                    COD
135900020409     C     KTAB1         KLIST
136000020409     C                   KFLD                    CODUT
136100020409     C                   KFLD                    COD
136200020409     c                   kfld                    key
136300010508     C     KARB          KLIST
136400010508     C                   KFLD                    ARBAAS
136500010508     C                   KFLD                    ARBLNP
136600010508     C                   KFLD                    ARBNRS
136700010508     C                   KFLD                    ARBNSP
136800131029     C     KARB2f        KLIST
136900131029     C                   KFLD                    EVBAAS
137000131029     C                   KFLD                    EVBLNP
137100131029     C                   KFLD                    EVBNRS
137200131029     C                   KFLD                    EVBNSP
137300131029     C                   KFLD                    EVBDTV
137400131029     C                   KFLD                    EVBORV
137500131029     C                   KFLD                    EVBCEV
137600080711     C     KAR4          KLIST
137700080711     C                   KFLD                    ARBAAS
137800080711     C                   KFLD                    ARBLNP
137900080711     C                   KFLD                    ARBNRS
138000080711     C                   KFLD                    ARBNSP
138100080711     C                   KFLD                    ktrc
138200010508     C     KARB7         KLIST
138300010508     C                   KFLD                    ARBAAS
138400010508     C                   KFLD                    ARBLNP
138500010508     C                   KFLD                    ARBNRS
138600010508     C                   KFLD                    ARBNSP
138700010508     C                   KFLD                    AR6TRC
138800010508     C     KARB2         KLIST
138900010508     C                   KFLD                    FIL
139000010508     C                   KFLD                    WDCM
139100010508     C     KART          KLIST
139200010508     C                   KFLD                    ARTFLS
139300010508     C                   KFLD                    ARTLNA
139400010508     C                   KFLD                    ARTNRS
139500010508     C                   KFLD                    ARTNSC
139600010508     C     KEVB4         KLIST
139700010508     C                   KFLD                    ARBAAS
139800010508     C                   KFLD                    ARBLNP
139900010508     C                   KFLD                    ARBNRS
140000010508     C                   KFLD                    ARBNSP
140100010508     C                   KFLD                    WCEV
140200050413
140300050906     C     KGCA          KLIST
140400050906     C                   KFLD                    ARBAAS
140500050906     C                   KFLD                    ARBLNP
140600050906     C                   KFLD                    ARBNRS
140700050906     C                   KFLD                    ARBNSP
140800050906     C                   KFLD                    KFRG
140900050404
141000050906     c     kblp          klist
141100050906     c                   kfld                    GcpAas
141200050906     c                   kfld                    GcpLnp
141300050906     c                   kfld                    GcpNrs
141400050906     c                   kfld                    GcpNsp
141500020530     c*
141600020530     c     kdcf          klist
141700020530     c                   kfld                    dctaac
141800020530     c                   kfld                    dctfil
141900020530     c                   kfld                    dctnca
142000010508      *
142100101007     C****               MOVEL     'R  '         WCEV
142200050906     C                   clear                   kfrg
142300150929     c*  File Fitgd32l    RECLAMI
142400040423     c     ktgd          klist
142500040423     c                   kfld                    ktgtor
142600040423     c                   kfld                    ktgogg
142700150929     c*  File TIIDC01L :  Disposizioni di Consegna da Web
142800141209     C     Kidc          KLIST
142900141209     C                   KFLD                    ARBAAS
143000141209     C                   KFLD                    ARBLNP
143100141209     C                   KFLD                    ARBNRS
143200141209     C                   KFLD                    ARBNSP
143300141209     C                   KFLD                    weladata
143400010508      *
143500010508      * DEFINIZIONE CAMPI
143600010508     C     *LIKE         DEFINE    EVBCEV        WCEV
143700010508     C     *LIKE         DEFINE    KPJBU         WSAJBU
143800010508     C     *LIKE         DEFINE    ARBDCM        WDCM
143900010508     C     *LIKE         DEFINE    AR4TRC        WTRC
144000010508     C     *LIKE         DEFINE    DCTDCH        WDCH
144100020530     C     *LIKE         DEFINE    DCTDCH        WDCH_dcb
144200010508     C     *LIKE         DEFINE    ARBLNP        SAVLNP
144300010508     C     *LIKE         DEFINE    TBLCOD        COD
144400020409     C     *LIKE         DEFINE    TBLKEY        KEY
144500050906     C     *LIKE         DEFINE    GCPFRG        KFRG
144600020207     C     *LIKE         DEFINE    §ogntw        lnantw
144700040831     c*
144800040831      * Se sono in ambiente buono - GAITRAGRU
144900040831     c                   If        knsif = 'FILTRA201 '
145000040831     c                   Eval      wlib = 'GAITRAGRU '
145100040831      *  Se sono in ambiente di prova - GAITRAGRPS
145200040831     c                   Else
145300040831     c                   Eval      wlib = 'GAITRAGRPS'
145400040831     c                   EndIf
145500040831     c*
145600040831     c                   Clear                   commant
145700040831     c                   Movel(p)  cmdt(1)       commant
145800040831     c                   Eval      %Subst(commant:30:10) = wlib
145900040831     c                   Eval      commant =
146000040831     c                             %trim(commant) + '/TITAS30C)'
146100040831     c                   Call      'QCMDEXC'                            99
146200040831     c                   Parm                    commant
146300040831     c                   Parm                    lenght
146400040831     c                   Open      Titas30c
146500060123     c*
146600060123     c                   Clear                   commant
146700060123     c                   Movel(p)  cmdt(2)       commant
146800060123     c                   Eval      %Subst(commant:30:10) = wlib
146900060123     c                   Eval      commant =
147000060123     c                             %trim(commant) + '/TNCSB03L)'
147100060123     c                   Call      'QCMDEXC'                            99
147200060123     c                   Parm                    commant
147300060123     c                   Parm                    lenght
147400060123     c                   Open      TNCSB03L
147500101007     c
147600020409     c*
147700020409     c                   exsr      caldts
147800010508      *
147900010508     C                   ENDSR
148000020409     C*---------------------------------------------------------------*
148100020409     C* caldts - Calcolo date stampa                                  *
148200020409     C*---------------------------------------------------------------*
148300020409     C     CALDTS        BEGSR
148400020409      * LETTURA giorni per date stampa e coda di output
148500020409     C                   MOVEL     '5A'          COD
148600020409     C                   MOVEL     '2       '    KEY
148700020409     C     ktab1         CHAIN     taBEL00F                           30
148800020409     C  N30              MOVEL     TBLUNI        DS5A2
148900020409     C   30              CLEAR                   DS5A2
149000020409     C*
149100020409     C                   MOVEL     '5A'          COD
149200020409     C                   MOVEL     '3       '    KEY
149300020409     C     ktab1         CHAIN     TABEL00F                           30
149400020409     C  n30              MOVEL     TBLUNI        DS5A3
149500020409     C   30              CLEAR                   DS5A3
149600020409     c     §5ahld        ifeq      'S'
149700020409     c                   movel     '*YES'        whld              4
149800020409     c                   else
149900020409     c                   movel     '*NO '        whld
150000020409     c                   endif
150100020409     c     §5asav        ifeq      'S'
150200020409     c                   movel     '*YES'        wsav              4
150300020409     c                   else
150400020409     c                   movel     '*NO '        wsav
150500020409     c                   endif
150600160601     c* BOLLE
150700020409     c* 1) data stampa bolle lna italia
150800020409     c     *ymd          move      comdpb        data_iso
150900020409     c                   subdur    §5agal:*d     data_iso
151000020409     c                   move      data_iso      dtsita_ymd
151100020409     c                   move      data_iso      data_dmy
151200020409     c                   move      data_dmy      dtsita
151300020409     c* 2) data stampa bolle export
151400020409     c     *ymd          move      compbe        data_iso
151500020409     c                   subdur    §5agal:*d     data_iso
151600020409     c                   move      data_iso      dtsexp_ymd
151700020409     c                   move      data_iso      data_dmy
151800020409     c                   move      data_dmy      dtsexp
151900020409     c* 3) data stampa bolle con riserva
152000020409     c     *ymd          move      comarr        data_iso
152100020409     c                   subdur    §5agal:*d     data_iso
152200020409     c                   move      data_iso      dtsrsv_ymd
152300020409     c                   move      data_iso      data_dmy
152400020409     c                   move      data_dmy      dtsrsv
152500020409     c* 4) data stampa bolle con c.a.
152600020409     c     *ymd          move      combca        data_iso
152700020409     c                   subdur    §5agcn:*d     data_iso
152800020409     c                   move      data_iso      dtscan_ymd
152900020409     c                   move      data_iso      data_dmy
153000020409     c                   move      data_dmy      dtscan
153100120222     c                   move      data_dmy      dtsncc
153200020409     c*
153300020409     c* Data per stampare le spedizioni non consegnate
153400020409     c                   subdur    §5agsn:*d     wdate_inv
153500020409     c                   move      wdate_inv     dtsncn_ymd
153600020409     c                   move      wdate_inv     data_dmy
153700020409     c                   move      data_dmy      dtsncn
153800120220     c****               move      data_dmy      dpsncn
153900120220
154000120220     c                   move      wdateu        data_iso
154100120220     c                   subdur    §5agdc:*d     data_iso
154200120220     c                   move      data_iso      dtsgdc_ymd
154300120220     c                   move      data_iso      data_dmy
154400120222     c                   move      data_dmy      dtsnccd
154500160531     c
154600160531     c                   move      wdateu        data_iso
154700160531     c                   subdur    4:*m          data_iso
154800160531     c                   move      data_iso      dtpEXP_ymd        8 0
154900160531     c                   move      data_iso      data_dmy
155000160531     c                   move      data_dmy      dtpexp_dmy        8 0
155100160531     c
155200020409     c                   endsr
155300020404**
155400020409OVRPRTF FNLR84P OUTQ
155500120220HOLD(    ) SAVE(    ) USRDTA('PulBoArr')
155600040831** cmdt
155700040831OVRDBF FILE(TITAS30C) TOFILE(
155800060123OVRDBF FILE(TNCSB03L) TOFILE(
155900160531**
156000160601333334000000000000000000000000
