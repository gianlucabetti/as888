000100090107     H DECEDIT('0,') DATEDIT(*DMY.) DFTACTGRP(*NO) ACTGRP(*CALLER)
000200020128     H* ficn11R  *---------------------------------------------------*
000300931005     H*         GESTIONE TARIFFE PARTICOLARI PADRONCINI
000400931005     H*--------------------------------------------------------------*
000500020128     Fficn11D   CF   E             WORKSTN
000600020128     F                                     SFILE(cn11S06:NR2)
000700090108     Ffifpt01L  UF A E           K DISK    commit(flg) usropn
000800090108     Ffifpd01L  UF A E           K DISK    commit(flg) usropn
000900931005     F                                     INFDS(TAR)
001000931005     FTABEL00F  IF   E           K DISK
001100011128     Ftntbe01L  IF   E           K DISK
001200020201     D WTT             S              1    DIM(6)                               WORK TIPO TARIFFA
001300020201     D TPG             S              1    DIM(6)                               TIPO TARIFFA
001400020201     D DPG             S             13    DIM(6)                               DESCR.TIPO TARIFFA
001500980508     D RPV             S              1    DIM(50)                              RAPP.PESO/VOLUME
001600980508     D ARR             S              1    DIM(50)                              ARROTONDAMENTI
001700980508     D TAP             S              1    DIM(50)                              TARIFFA IN PERCENTUA
001800020214     D tipitsm         S              1    DIM(6)                               WORK TIPO TARIFFA
001900931006     D*
002000940919     D* RICEZ. PARAMETRI PGM GESTIONE TARIF.PADRONC.  - TNTA80R -
002100931116     D PARAM4          DS
002200931005     D  VIDTES                 1     37
002300020130     d  parmod                37     37
002400931005     D  VIDFIL                38     40  0
002500931007     D  VIDPDR                41     44
002600931007     D  COMPDR                38     44  0
002700940922     D  DESPDR                45     79
002800940922     D  COMSML                80     80
002900940922     D  VIDTSR                81     81
003000940922     D  DESTSR                82     96
003100940922     D  VIDCTR                97     99  0
003200011029     D  DESCTR               100    113
003300021210     D  paranac              114    114
003400940922     D  VIDDCV               115    129
003500940922     D  VIDPRG               130    132  0
003600940922     D  FLGCM3               133    133
003700940922     D  FLGDUV               134    134
003800970718     D  COMDDT               135    142  0
003900000620     D  COMTPT               143    143
004000011029     D  viddiv               144    146
004100020128     D  vidctd               147    149
004200020128     D  de2ctd               150    174
004300931116     D*
004400931116     D* RICHIAMO PGM DI DECODIFICA C.A.P.             - TRUL05R -
004500931116     D PARAM           DS
004600970718     D  PARTLA                 1      1
004700970718     D  PARCAP                 2     10
004800970718     D  PARDRI                11     18  0
004900970718     D  PARNAR                19     21    INZ
005000970718     D  PARNDE                22     23  0
005100970718     D  PARNDF                24     25  0
005200970718     D  PARDEC                26     60
005300970718     D  PARE01                61     95
005400970718     D  PARE02                96    130
005500970718     D  PARE03               131    165
005600970718     D  PARE04               166    200
005700970718     D  PARE05               201    235
005800970718     D  PARE06               236    270
005900970718     D  PARE07               271    305
006000970718     D  PARE08               306    340
006100970718     D  PARE09               341    375
006200970718     D  PARE10               376    410
006300970718     D  PARTCD               411    411    INZ
006400931005     D*
006500091202      ***** da modificare nel prossimo step x i decimali a 5 dei campi FPT e FPD
006600931006     D                 DS
006700940929     D  VI2SGL                 1     10  0
006800100112     D  VI2ITR                11     21  5
006900100112     D  VI2MIN                22     32  5
007000100112     D  VI2MAX                33     43  5
007100100112     D  COMREC                 1     43
007200100112     D  VI2TAR                44     49  3
007300100112     D  CO2REC                11     49
007400091202     D*
007500091202      ***** da modificare nel prossimo step x i decimali a 5 dei campi FPT e FPD
007600931006     D                 DS
007700100112     D  SAVITR                 1     11  5
007800100112     D  SAVMIN                12     22  5
007900100112     D  SAVMAX                23     33  5
008000100112     D  SAVTAR                34     39  3
008100100112     D  SAVREC                 1     39
008200941221     D*
008300970718     D* DS PER TISI95R - CONTROLLO CAP
008400970718     D DSSI95        E DS                  EXTNAME(TISI95DS)
008500020214      * ds di comodo per impostare campo filler in caso di presenza BONUS
008600020214     d dsbonus         ds
008700020214     d   vi2tbn                       1
008800020214     d   vi2pab                       3  0
008900020214     d   vi2sab                       3  0
009000020214     d   vi2pmd                       1
009100941221     D*
009200931006     D KPJBA         E DS
009300931006     D TAR             DS
009400931005     D  FPDNRR               397    400B 0
009500020129     D FPTDS         E DS                  EXTNAME(fifpt00F) inz
009600020129     D comfpt          s                   like(fptds) inz
009700931005     D DS8A          E DS
009800980318     D DSTR          E DS
009900931006     D CNCR80        E DS
010000011128     D Dgedcn        E DS
010100931006     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
010200040206     D WLBDAT          DS
010300040206     D  G02DAT                 1      8  0
010400040206     D  G02INV                 9     16  0
010500040206     D  G02ERR                17     17
010600040206     D  G02TGI                18     22  0
010700941221     D*
010800941221     D***  DEFINIZIONE COSTANTI  ***
010900941221     D CIT000          C                   CONST('IT000    ')
011000941221     D CIT100          C                   CONST('IT100    ')
011100931006     I/SPACE 3
011200931006     C*****************************************************************
011300931006     C* RIEPILOGO INDICATORI
011400931006     C*****************************************************************
011500931006     C* 01    - IMMISSIONE
011600931006     C* 02    - VARIAZIONE
011700931006     C* 03    - PREMUTO CMD6 -AGGIORNAMENTO
011800931006     C* 04    - PREMUTO CMD16-ANNULLAMENTO
011900980508     C* 05    - INDICA TIPO TARIFFA " A VALORE" CON TARIFFA IN PERCENTUALE
012000931028     C* 06    - TARIFFA PARTICOLARE "N - C/ASS PRONTAM. RECUPERATI"
012100931006     C* 07    - INDICA IL C.A.P. 'IT000'
012200931006     C* 08    - INDICA IL C.A.P. 'IT100'
012300931006     C* 09    - ROLLUP
012400931006     C* 10    - USATO IN INTERROGAZIONE PER PROTEGGERE I CAMPI
012500931007     C* 11    - USATO PER PROTEGGERE CAMPI DEL FORMATO
012600931014     C* 12    - INDICA LA FASE DI SIMULAZIONE
012700931117     C* 13    - INDICA IL C.A.P. =  BLANK
012800931025     C* 14    - INDICA IL C.A.P. >< BLANK
012900931006     C* 15    - USATO IN VARIAZIONE PER PROTEGGERE I CAMPI
013000931006     C* 16    - USATO IN VARIAZIONE PER POSIZIONARMI SU C.A.P.
013100931015     C* 17 OFF- USATO IN VARIAZIONE PER POSIZIONARMI SU IMPORTO TARIFFA
013200931007     C* 18/24 - TASTI DI DUPLICA
013300950114     C* 25    - USATO NELLE RIGHE DI DETTAGLIO PER VISUALIZZARE LA
013400950114     C*         LETTERA "M" CHE INDICA LA PRESENZA DEL MINIMO O MASSIMO
013500931007     C* 26    - CONDIZIONA L'EMISSIONE DEGLI ARROTONDAMENTI
013600931028     C* 27    - CONDIZIONA L'EMISSIONE DEL RAPPORTO PESO/VOLUME
013700931006     C* 29    - USATO PER AZZERARE I CAMPI DEL SFLCONTROL NEL CASO IN
013800931006     C*         CUI VENGA VARIATO IL TIPO TARIFFA
013900931006     C* 30/33 - DI COMODO
014000931006     C* 35    - LOKUP E TESTN
014100931117     C* 40/56 - INDICATORI PER MESSAGGI DI ERRORE
014200011029     C* 60    - INDICA CHE SONO STATE EFFETTUATE MODIFICHE SU fifpd00F
014300000621     C* 67    - PROTEGGE IL CAMPO SOSTITUZIONE
014400931005     C* 87/88 - PULIZIA SUBFILE
014500931005     C* 90    - SEGNALA LA PRESENZA DI ERRORI
014600931006     C* 99    - DI COMODO
014700931006     C*****************************************************************
014800931005     C     *ENTRY        PLIST
014900931005     C                   PARM                    KPJBA
015000931005     C                   Z-ADD     1             CODUT
015100931005     C                   CALL      'X§PARUT'
015200931005     C                   PARM                    UT§DSE
015300931005     C                   MOVEL     RAGUT         RSUT             20
015400931005     C                   MOVEL     REC80         CNCR80
015500931116     C                   MOVEL     KPJBU         PARAM4
015600040206      *data del giorno
015700040206     C                   TIME                    W0120            14 0
015800040206     C                   MOVE      W0120         WDAT              8 0
015900040206     C*
016000040206     C                   Z-ADD     WDAT          G02DAT
016100040206     C                   MOVEL     *BLANK        G02ERR
016200040206     C                   CALL      'XSRDA8'
016300040206     C                   PARM                    WLBDAT
016400040206     C* UDATE A 8 IN AAAA/MM/GG
016500040206     C                   Z-ADD     G02INV        WUDA              8 0
016600020130     c
016700020130     c     parmod        comp      ' '                                1010
016800090108     c                   if        parmod <> ' '
016900090108     c                   eval      flg = *off
017000090108     c                   else
017100090108     c                   eval      flg = *on
017200090108     c                   end
017300090108     c                   open      fifpd01l
017400090108     c                   open      fifpt01l
017500931005     C*---------------------------------------------------------------*
017600931005     C* ACCESSO TABEL
017700931005     C     KTAB          KLIST
017800931005     C                   KFLD                    CODUT
017900940919     C                   KFLD                    COD
018000940919     C                   KFLD                    KEY
018100931005     C     KTAB2         KLIST
018200931005     C                   KFLD                    CODUT
018300931005     C                   KFLD                    COD
018400011029     C* ACCESSO fifpt / fifpd
018500931005     C     KFPT          KLIST
018600931005     C                   KFLD                    COMPDR
018700931014     C                   KFLD                    COMSML
018800931005     C                   KFLD                    VIDTSR
018900931005     C                   KFLD                    VIDCTR
019000931005     C                   KFLD                    VIDPRG
019100020128     C                   KFLD                    vidctd
019200011029     C* ACCESSO fifpd
019300931005     C     KFPD          KLIST
019400931005     C                   KFLD                    COMPDR
019500931014     C                   KFLD                    COMSML
019600931005     C                   KFLD                    VIDTSR
019700931005     C                   KFLD                    VIDCTR
019800931005     C                   KFLD                    VIDPRG
019900020128     C                   KFLD                    vidctd
020000931005     C                   KFLD                    VI2CAP
020100940929     C                   KFLD                    COMSGL
020200011128     C* ACCESSO Tntbe
020300011128     C     Ktbe          KLIST
020400011128     C                   KFLD                    kcod1
020500011128     C                   KFLD                    kke1
020600020214     C     Ktbe2         KLIST
020700020214     C                   KFLD                    kcod              3
020800020214     C                   KFLD                    key1             15
020900020214     C                   KFLD                    key2             15
021000020214     c                   movel     'CTD'         kcod
021100011128     C*
021200940919     C*
021300940919     C* DEFINIZIONE CAMPI
021400011128     C     *LIKE         DEFine    TBECOD        KCOD1
021500011128     C     *LIKE         DEFine    TBEKE1        KKE1
021600940919     C     *LIKE         DEFINE    TBLCOD        COD
021700940919     C     *LIKE         DEFINE    TBLKEY        KEY
021800940919     C*
021900940929     C     *LIKE         DEFINE    FPDSGL        COMSGL
022000940919     C     *LIKE         DEFINE    FPDCAP        DUPCAP
022100940929     C     *LIKE         DEFINE    VI2SGL        DUPSGL
022200940929     C     *LIKE         DEFINE    VI2ITR        DUPITR
022300940929     C     *LIKE         DEFINE    VI2TAR        DUPTAR
022400940919     C     *LIKE         DEFINE    FPDMIN        DUPMIN
022500940919     C     *LIKE         DEFINE    FPDMAX        DUPMAX
022600940919     C*
022700931005     C*---------------------------------------------------------------*
022800931005     C* IMPOSTAZIONI INIZIALI
022900931006     C     UNO           IFEQ      ' '
023000931007     C*
023100931005     C*
023200931006     C                   MOVEL     '1'           UNO               1
023300931005     C                   ENDIF
023400931005     C*
023500931005     C*
023600931005     C                   MOVEL     '1'           ALF1              1
023700931005     C*
023800931005     C* ALF1 = 'F' ===> VADO A FINE PGM
023900931005     C     ALF1          DOWNE     'F'
024000931005     C*
024100931005     C* ALF1 = '1' ===> GESTIONE VIDEATA SELEZIONE TARIFFE PARTICOLARI
024200931005     C     ALF1          IFEQ      '1'
024300931005     C                   EXSR      VIDEO1
024400931005     C                   ENDIF
024500931005     C*
024600931005     C                   ENDDO
024700931116     C* CALL A VUOTO AL PGM TRUL05R PER CHIUDERE I FILES
024800931116     C     TRUL05        IFEQ      '1'
024900931116     C                   MOVEL     'C'           PARTLA                         *TIPO LANCIO
025000931116     C                   CALL      'TRUL05R'
025100931116     C                   PARM                    PARAM
025200931116     C                   ENDIF
025300941221     C*
025400970718     C* CALL A VUOTO AL PGM TISI95R PER CHIUDERE I FILES
025500970718     C     TISI95        IFEQ      '1'
025600970718     C                   CLEAR                   DSSI95
025700970718     C                   MOVEL     'C'           I95TLA                         *TIPO LANCIO
025800970718     C                   CALL      'TISI95R'
025900970718     C                   PARM                    DSSI95
026000941221     C                   ENDIF
026100931116     C*
026200950315     C* ESEGUO SETLL FITTIZIA PER SVINCOLARE IL RECORD
026300011029     C     *HIVAL        SETLL     fifpt000
026400950315     C*
026500020128     C                   seton                                        lr
026600931005     C*
026700931005     C*--- CARICAMENTO TABELLE ---------------------------------------*
026800931005     C     CARTAB        BEGSR
026900931005     C****  CODICI PAGAMENTO E DESCRIZIONI: TPG / DPG  ****
027000931005     C                   Z-ADD     1             X                 3 0
027100020129     c                   clear                   key
027200980318     C                   MOVEL     'TR'          COD
027300980318     C* CARICO I TIPI TARIFFE CHE VENGONO GESTITE DALLA SINGOLA TARIFFA
027400980318     C* PARTICOLARE
027500980318    1C     WTT(X)        DOWNE     ' '
027600980318     C                   MOVEL     WTT(X)        KEY
027700980318     C     KTAB          CHAIN     TABEL                              30
027800980318    2C     *IN30         IFEQ      '0'
027900980318     C     TBLFLG        ANDEQ     *BLANKS
028000980318     C                   MOVEL     TBLUNI        DSTR
028100980318     C*
028200980318     C* CARICO SOLO I CODICI TARIFFA VALIDI PER LE TARIFFE PARTICOLARI
028300980318    3C     §TRUPA        IFEQ      'S'
028400980318     C                   MOVEL     TBLKEY        TPG(X)
028500980318     C                   MOVEL     §TRDES        DPG(X)
028600980508     C* ARROTONDAMENTI IN TARIFFA
028700980508     C                   MOVEL     §TRARR        ARR(X)
028800980508     C* RAPPORTO PESO VOLUME
028900980508     C                   MOVEL     §TRRPV        RPV(X)
029000980508     C* TARIFFA IN PERCENTUALE
029100980508     C                   MOVEL     §TRTAP        TAP(X)
029200980508     C*
029300980318    3C                   END
029400980318     C*
029500980318    2C                   END
029600980318     C*
029700980318     C                   ADD       1             X
029800980318    1C                   ENDDO
029900011128     C***
030000011128     C* CARICO TABELLA GED per validità divise
030100011128     C***
030200011128     C                   MOVEL     'GED'         KCOD1
030300011128     C                   MOVEL     *BLANKS       KKE1
030400011128     C                   MOVEL     'CN'          KKE1
030500011128     C     KTBE          CHAIN     TNTBE01L                           96
030600011128     C     *IN96         IFEQ      '0'
030700011128     C                   MOVEL     TBEUNI        DGEDcn
030800011128     C                   END
030900011128     C***
031000931005     C                   ENDSR
031100931005     C*
031200931005     C*--- GESTIONE DELLA PRIMA VIDEATA ------------------------------*
031300931005     C     VIDEO1        BEGSR
031400931014     C                   SETOFF                                       12
031500931006     C*
031600931014     C* 12 ON  - SONO IN FASE DI SIMULAZIONE
031700931014     C     COMSML        IFNE      ' '
031800931014     C                   SETON                                        12
031900931014     C                   ENDIF
032000931005     C*
032100931005     C     INIZIO        TAG
032200931005     C*
032300020128     C* DESCRIZIONE TIPO TARIFFA
032400020128     C                   EXSR      DESTPG
032500931005     C                   EXSR      PULCTL
032600931005     C*
032700931005     C* CARICO TIPO TARIFFA
032800020128     C     KFPT          CHAIN     fifpt000                           33
032900020128     C*--- CARICO DATI testata e CONTROL -------------------------------*
033000020128     C     *IN33         IFEQ      *OFF
033100090925     c     fptatb        andeq     ' '
033200020128     c                   z-add     fptata        vidata
033300020128     c                   move      fpttpg        vi2tpg
033400020212     c                   move      fpttsm        vi2tsm
033500020212     c                   move      fpttat        vi2tat
033600020212     c                   move      fpttpp        vi2tpp
033700020128     C* ARROTONDAMENTI
033800020128     C                   Z-ADD     FPTARL        VI2ARL
033900020128     C                   Z-ADD     FPTARF        VI2ARF
034000020128     C                   Z-ADD     FPTARO        VI2ARO
034100020128     C* RAPPORTO PESO/VOLUME
034200020128     C                   Z-ADD     FPTRPV        VI2RPV
034300020410     c     vidctd        comp      'BON'                                  28
034400020214     c   28              movel     fptflr        dsbonus
034500020214     c  n28              clear                   dsbonus
034600020129     c                   else
034700020214     c                   clear                   dsbonus
034800020129     c                   z-add     0             vidata
034900020129     c                   move      *blank        vi2tpg
035000020212     c                   move      *blank        vi2tsm
035100020214     c                   move      §8alvl        vi2tat
035200020212     c                   move      *blank        vi2tpp
035300020129     C* ARROTONDAMENTI
035400020129     C                   Z-ADD     0             VI2ARL
035500020129     C                   Z-ADD     0             VI2ARF
035600020129     C                   Z-ADD     0             VI2ARO
035700020129     C* RAPPORTO PESO/VOLUME
035800020129     C                   Z-ADD     0             VI2RPV
035900020128     C                   ENDIF
036000020206     C* PULIZIA SUBFILE
036100020206     C                   EXSR      PULSFL
036200020206     C* CARICAMENTO SUBFILE
036300020206     C                   EXSR      CARSFL
036400020206     C*                  Z-ADD     0             SAVNR2
036500020128     C*
036600020204     c                   do        *hival
036700020204
036800020201     C     FOR01         TAG
036900020328     c                   setoff                                       29
037000020328     c                   clear                   vidmsg
037100020328
037200020315     c     vidctd        comp      'BON'                                  28
037300020315     c     vidctd        comp      'PCK'                                  39
037400020315     c     vidctd        comp      'BAC'                                  62
037500020315     c  n62vidctd        comp      'BAR'                                  62
037600020315     c  n62              eval      vidatad = 'Importo Tariffa'
037700020315     c   62              eval      vidatad = 'Tar.finita fino'
037800021218      *se gestione coop non considero campi arrotondamento
037900021218     c                   if        paranac <> 'C'
038000020315     c                   z-add     1             xy                3 0
038100020315     c     vi2tpg        lookup    tpg(xy)                                66
038200020315     c   66arr(xy)       comp      'S'                                    38
038300020315     c   66rpv(xy)       comp      'S'                                    30
038400021218     c                   endif
038500020315     c  n30              z-add     0             vi2rpv
038600020315     c  n38              z-add     0             vi2arl
038700020315     c  n38              z-add     0             vi2arf
038800020315     c  n38              z-add     0             vi2aro
038900020315     c                   if        vidctd = 'BAC' or vidctd ='BAR' or
039000021210     c                             vi2tpg = 'F' or vi2tpg = 'B'    or
039100021210     c                             paranac = 'C'
039200020315     c                   seton                                        61
039300020315     c                   else
039400020315     c                   z-add     0             vidata
039500020315     c                   setoff                                       61
039600020315     c                   end
039700020315     c                   exsr      contrtsm
039800020328      * controllo valore campo importo deve essere valorizzato
039900020328      *
040000020328     c                   if        vidata = 0 and vi2tpg = 'F' or
040100021218     c                             vidata = 0 and vi2tpg = 'B' or
040200021218     c                             vidata = 0 and paranac='C'and vi2tpg <>*blank
040300020328     c                   eval      vidmsg = 'Importo obbligatorio'
040400020328     c                   seton                                        29
040500020328     c                   endif
040600020204      * se valore in testata non ammette il SFL apro solo il primo video
040700020201     c                   if        vi2tpg = 'F' or vi2tpg = 'B' or vi2tpg = ' '
040800021218     c                             or errtesta <> *blank or paranac = 'C'
040900090922     C                   SETOFF                                       1189
041000020129     C                   write     cn11T01
041100020129     C                   exfmt     cn11Z02
041200020129     c                   read      cn11t01
041300020128     C** CMD12 - FINE LAVORO
041400020128     C   KL              move      'F'           alf1
041500020128     C   KL              GOTO      endvd2
041600020219     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
041700020219     C   KQ              EXSR      ANNTAR
041800020219     C   Kq              move      'F'           alf1
041900020219     C   KQ              GOTO      ENDVD2
042000020219
042100020201     c                   exsr      contr
042200020315     c   90              move      'X'           errtesta          1
042300020315     c  n90              clear                   errtesta
042400020328     c   29
042500020328     cor 90              goto      for01
042600020128     c                   else
042700020204      * se valore non in testata ammette il SFL gestisco le emissioni anche
042800020204      * della testata all'interno del loop
042900931006     C                   Z-ADD     0             NR2
043000931007     C                   SETON                                        11
043100931006     C* SCRIVO RECORD DI NUOVA IMMISSIONE
043200931006     C   01              EXSR      SCRIVI
043300931006     C*
043400931006     C                   Z-ADD     1             REC2
043500020201     C                   WRITE     CN11T01
043600020201     C                   WRITE     CN11z07
043700931006     C     FORCT         TAG
043800020128     C                   EXFMT     cn11C06
043900020129     C                   read      cn11T01
044000020128      *
044100931006     C*
044200931006     C** CMD12 - FINE LAVORO
044300020128     C   KL              move      'F'           alf1
044400020128     C   KL              GOTO      endvd2
044500931006     C*
044600931006     C** CMD16 - ANNULLAMENTO TOTALE TARIFFA PARTICOLARE
044700931006     C   KQ              EXSR      ANNTAR
044800020219     C   Kq              move      'F'           alf1
044900931006     C   KQ              GOTO      ENDVD2
045000931006     C*
045100931006     C** CMD9  - AGGIUNTA RIGHE
045200931006     C   KI
045300931006     C* SCORRIMENTO
045400931006     COR 09              DO
045500931006     C                   SETON                                        16
045600931006     C                   EXSR      SCRIVI
045700020204     C*                  GOTO      FORCT
045800931007     C                   ENDDO
045900020204     c                   exsr      contr
046000020315     c   90              move      'X'           errtesta          1
046100020315     c  n90              clear                   errtesta
046200020204     c   90              goto      for01
046300020204     c                   if        vi2tpg = 'F' or vi2tpg = 'B' or vi2tpg = ' '
046400020204     c                   goto      for01
046500020204     c                   end
046600931006     C*
046700931006     C* CONTROLLO DATI SUBFILE CONTROL
046800931006     C                   EXSR      CONCTL
046900931006     C   90              GOTO      FORCT
047000931006     C*
047100931006     C* CONTROLLO DATI SUBFILE
047200931006     C                   EXSR      CONSFL
047300931006     C   90              GOTO      FORCT
047400931006     C*
047500020201     C** CMD6 - REGISTRA
047600931006     C* REGISTRO CAMPI DEL SUBFILE
047700020220     c                   if        not *in10 and *inkf
047800020220     C                   EXSR      REGIS
047900020130     c                   end
048000020128     c                   end
048100931006     C* REGISTRO CAMPI DEL SUBFILE CONTROL
048200020328     c                   if        not *in10 and *inkf and not *in29
048300020220     C                   EXSR      REGIS2
048400020220     C                   move      'F'           alf1
048500020220     C                   GOTO      ENDVD2
048600020130     c                   end
048700931006     C*
048800020204     c                   enddo
048900020204
049000931005     C     ENDVD2        ENDSR
049100980318     C*
049200020128     C*--- DESCRIZIONE TIPO TARIFFA ----------------------------------*
049300020128     C     DESTPG        BEGSR
049400020128     C*----------------------------------------------*
049500020128     C* PULIZIA DELLE SCHIERE CODICI TARIFFE PER LA SINGOLA TARIFFA PARTICOLARE
049600020128     C                   MOVEL     *BLANKS       TPG
049700020128     C                   MOVEL     *BLANKS       DPG
049800020128     C                   MOVEL     *BLANKS       WTT
049900020128     C                   MOVEL     *BLANKS       ARR
050000020128     C                   MOVEL     *BLANKS       RPV
050100020128     C                   MOVEL     *BLANKS       TAP
050200100419     C                   MOVEL     *BLANKS       minmaxOBBLIG      1
050300020213
050400020429     C* CARICO LA TABELLA DEI TIPI TARIFFE UTILIZZATI DALLA TARIFFA
050500020429     C* do dettaglio sELEZIONATA
050600020429     C                   MOVEL     '8A'          COD
050700020429     C                   MOVEL     *BLANKS       KEY
050800020429     C                   MOVEL     Vidctd        KEY
050900020429     C     KTAB          CHAIN     TABEL                              30
051000020429     C     *IN30         IFEQ      *OFF
051100020429     C*
051200020429     C                   MOVEL     TBLUNI        DS8A
051300020429     c                   endif
051400020213      *forzo il primo codice della tariffa se si tratta di tariffa base
051500020327     c                   if        vidctd = 'BAR' or vidctd = 'BAC'
051600020213     c                   movel     vidctr        wtt(1)
051700020213     c                   else
051800020429      *solo se ho trovato la tabella
051900020128     C     *IN30         IFEQ      *OFF
052000100419     C*
052100100419     C*  Obbligatorietà del minimo e massimo
052200100419     C                   if        §8AobbMIMA = 'S'
052300100419     C                   MOVEL     'S'           minmaxOBBLIG
052400100419     c                   end
052500100419     C*
052600020128     C*
052700020201     C* CARICO I TIPI TARIFFE ABILITATI A QUESTA TARIFFA PARTICOLARE
052800020201     c                   select
052900020201     c                   when      vidctr < 999
053000020128     C                   MOVEL     §8ATT0        WTT(1)
053100020128     C                   MOVEL     §8ATT1        WTT(2)
053200020128     C                   MOVEL     §8ATT2        WTT(3)
053300020128     C                   MOVEL     §8ATT3        WTT(4)
053400020128     C                   MOVEL     §8ATT4        WTT(5)
053500020128     C                   MOVEL     §8ATT5        WTT(6)
053600020201     c                   when      vidctr = 999 and vidtsr = 'C' or
053700020201     c                             vidctr = 999 and vidtsr = 'R'
053800020201     C                   MOVEL     §8ATT6        WTT(1)
053900020201     C                   MOVEL     §8ATT7        WTT(2)
054000020201     C                   MOVEL     §8ATT8        WTT(3)
054100020201     C                   MOVEL     §8ATT9        WTT(4)
054200020201     C                   MOVEL     §8ATTV        WTT(5)
054300020201     C                   MOVEL     *blank        WTT(6)
054400020201     c                   when      vidctr = 999 and vidtsr = 'G'
054500020201     C                   MOVEL     §8ATb1        WTT(1)
054600020201     C                   MOVEL     §8ATb2        WTT(2)
054700020201     C                   MOVEL     §8ATb3        WTT(3)
054800020201     C                   MOVEL     §8ATb4        WTT(4)
054900020201     C                   MOVEL     §8ATb5        WTT(5)
055000020201     C                   MOVEL     *blank        WTT(6)
055100020201
055200020201     c                   endsl
055300020213
055400020128     C                   ENDIF
055500020213
055600020213     c                   endif
055700020213
055800020128     C*
055900020128     C* CARICO TABELLA CODICI TARIFFA
056000020128     C                   EXSR      CARTAB
056100020128     C*
056200020128     C                   MOVEL     *BLANKS       TPG1
056300020128     C                   MOVEL     *BLANKS       DPG1
056400020128     C                   MOVEL     *BLANKS       TPG2
056500020128     C                   MOVEL     *BLANKS       DPG2
056600020128     C                   MOVEL     *BLANKS       TPG3
056700020128     C                   MOVEL     *BLANKS       DPG3
056800020128     C                   MOVEL     *BLANKS       TPG4
056900020128     C                   MOVEL     *BLANKS       DPG4
057000020128     C*
057100020128     C******     *IN06     IFEQ *ON
057200020128     C*
057300020128     C     TPG(1)        IFNE      ' '
057400020128     C                   MOVEL     TPG(1)        TPG1
057500020128     C                   MOVEL     '='           DPG1
057600020214     C                   MOVEl     DPG(1)        DPG1
057700020128     C                   ENDIF
057800020128     C     TPG(2)        IFNE      ' '
057900020128     C                   MOVEL     TPG(2)        TPG2
058000020128     C                   MOVEL     '='           DPG2
058100020214     C                   MOVEl     DPG(2)        DPG2
058200020128     C                   ENDIF
058300020128     C     TPG(3)        IFNE      ' '
058400020128     C                   MOVEL     TPG(3)        TPG3
058500020128     C                   MOVEL     '='           DPG3
058600020214     C                   MOVEl     DPG(3)        DPG3
058700020128     C                   ENDIF
058800020128     C     TPG(4)        IFNE      ' '
058900020128     C                   MOVEL     TPG(4)        TPG4
059000020128     C                   MOVEL     '='           DPG4
059100020214     C                   MOVEl     DPG(4)        DPG4
059200020128     C                   ENDIF
059300020128     C*
059400020128     C                   ENDSR
059500020128     C*------------------------------------------------
059600931006     C*--- PULIZIA SUBFILE CONTROL -----------------------------------*
059700931006     C     PULCTL        BEGSR
059800020128     C*------------------------------------------------
059900940919     C                   Z-ADD     0             VI2ARL
060000940919     C                   Z-ADD     0             VI2ARF
060100940919     C                   Z-ADD     0             VI2ARO
060200931005     C                   Z-ADD     0             VI2RPV
060300020129     C                   MOVEL     *BLANKS       COMFPT
060400931005     C                   ENDSR
060500931005     C*
060600931006     C*--- ANNULLAMENTO TOTALE TARIFFA -------------------------------*
060700931006     C     ANNTAR        BEGSR
060800020128     C*------------------------------------------------
060900931007     C                   SETOFF                                       32
061000931006     C* ANNULLO DETTAGLIO
061100011029     C     KFPT          SETLL     fifpd000
061200931006     C     *IN32         DOWEQ     *OFF
061300011029     C     KFPT          READE     fifpd000                               32
061400931006     C  N32FPDATB        IFEQ      ' '
061500931006     C                   MOVEL     'A'           FPDATB
061600091210     C**********         MOVEL     'T'           FPDFTR
061700091210     C**********         MOVEL     *ALL'9'       FPDDTR
061800011029     C                   UPDATE    fifpd000
061900931006     C                   ENDIF
062000931006     C                   ENDDO
062100931006     C*
062200931006     C* ANNULLO TESTATA
062300011029     C     KFPT          CHAIN     fifpt000                           32
062400931006     C     *IN32         IFEQ      *OFF
062500931006     C                   MOVEL     'A'           FPTATB
062600091210     C**********         MOVEL     'T'           FPTFTR
062700091210     C**********         MOVEL     *ALL'9'       FPTDTR
062800011029     C                   UPDATE    fifpt000
062900931006     C                   ENDIF
063000931006     C*
063100931006     C* RIEMPIO IL FLAG IN MODO CHE SE ANNULLANO UNA CONSEGNA PARTIC.
063200940919     C*   AGGIORNO LA DATA ULTIMA VARIAZIONE (PRESENTE NEL TNTA80R)
063300931006     C                   MOVEL     'A'           FLGDUV
063400931006     C*
063500931007     C* 04 ON  - E' STATA ANNULLATA UNA TARIFFA PARTICOLARE
063600931006     C                   SETON                                        04
063700931006     C                   ENDSR
063800931006     C*
063900931006     C*
064000931006     C*--- PULIZIA SUBFILE -------------------------------------------*
064100931007     C     PULSFL        BEGSR
064200020128     C*------------------------------------------------
064300931005     C                   SETON                                        88
064400020128     C                   WRITE     cn11C06
064500931005     C                   SETOFF                                       88
064600931006     C                   ENDSR
064700931006     C*
064800931025     C*--- CARICAMENTO SUBFILE ---------------------------------------*
064900931007     C     CARSFL        BEGSR
065000020128     C*------------------------------------------------
065100931006     C                   SETOFF                                       90
065200931008     C                   SETON                                          15
065300931006     C                   Z-ADD     0             NR2               4 0
065400020228     c     vi2tpg        comp      'V'                                    05
065500931006     C*
065600011029     C     KFPT          SETLL     fifpd000
065700011029     C     KFPT          READE     fifpd000                               32
065800931006     C*
065900931006     C     *IN32         DOWEQ     *OFF
066000931006     C* ESCLUDO RECORD ANNULLATI
066100931006     C     FPDATB        IFEQ      ' '
066200090922     c* ind 89 protegge i campi di input del sflctl t01
066300090922     c                   seton                                        89
066400931006     C* C.A.P.
066500931006     C                   MOVEL     FPDCAP        VI2CAP
066600931006     C* DECODIFICA C.A.P.
066700970718     C                   CLEAR                   PARAM
066800931116     C                   MOVEL     FPDCAP        PARCAP                         *C.A.P.
066900931006     C                   EXSR      DECCAP
067000931006     C* SCAGLIONE
067100940929     C                   Z-ADD     FPDSGL        VI2SGL
067200931006     C* IMPORTO TARIFFA
067300931007     C     *IN05         IFEQ      *OFF
067400940919     C                   Z-ADD     FPDITR        VI2ITR
067500940919     C                   Z-ADD     FPDITR        SAVITR
067600931006     C                   Z-ADD     0             VI2TAR
067700931006     C                   Z-ADD     0             SAVTAR
067800931006     C                   ELSE
067900940919     C                   Z-ADD     FPDITR        VI2TAR
068000940919     C                   Z-ADD     FPDITR        SAVTAR
068100940919     C                   Z-ADD     0             VI2ITR
068200940919     C                   Z-ADD     0             SAVITR
068300931006     C                   ENDIF
068400931006     C* MINIMO  TARIFFA
068500011127     C                   z-add     FPDMIN        VI2MIN
068600011127     C                   z-add     FPDMIN        SAVMIN
068700931006     C* MASSIMO TARIFFA
068800011127     C                   z-add     FPDMAX        VI2MAX
068900011127     C                   z-add     FPDMAX        SAVMAX
069000950117     C*
069100950117     C* 25 ON - SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
069200950117     C*   LA LETTERA "M" SULLA RELATIVA RIGA DI DETTAGLIO
069300950117     C                   SETOFF                                       25
069400950117    2C     VI2MIN        IFGT      0
069500950117     C     VI2MAX        ORGT      0
069600950117     C                   SETON                                        25
069700950117    2C                   ENDIF
069800950117     C*
069900931006     C* FLAG ANNULLAMENTO
070000931006     C                   MOVEL     ' '           VI2ATB
070100931006     C*
070200931006     C                   Z-ADD     FPDNRR        VI2NRR
070300931006     C*
070400931006     C                   ADD       1             NR2
070500020128     C                   WRITE     cn11S06
070600931006     C                   ENDIF
070700931006     C*
070800011029     C     KFPT          READE     fifpd000                               32
070900931006     C                   ENDDO
071000931006     C*
071100931006     C                   Z-ADD     NR2           SAVNR2            4 0
071200931006     C*
071300931006     C     NR2           IFGT      0
071400931013     C     NR2           DIV       8             DIVIS             3 0
071500931006     C                   MVR                     RESTO             3 0
071600931006     C     RESTO         IFGT      0
071700931006     C                   EXSR      SCRIVI
071800931006     C                   ENDIF
071900931006     C*
072000931006     C                   ELSE
072100931006     C                   SETON                                        16
072200931006     C                   EXSR      SCRIVI
072300931006     C                   ENDIF
072400931006     C*
072500931006     C                   ENDSR
072600931006     C*
072700931006     C*--- SUBROUTINE DI AGGIUNTA RIGHE ------------------------------*
072800931006     C     SCRIVI        BEGSR
072900020128     C*------------------------------------------------
073000950117     C                   SETOFF                                       1525
073100931006     C                   Z-ADD     0             B                 3 0
073200931006     C*
073300931006     C     RESTO         IFNE      0
073400931006     C                   Z-ADD     RESTO         B
073500931006     C                   Z-ADD     0             RESTO
073600931006     C                   ENDIF
073700931006     C*
073800931006     C                   Z-ADD     SAVNR2        NR2
073900931006     C* CAMPI DEL SUBFILE
074000931013     C     B             DOWLT     8
074100931006     C                   MOVEL     ' '           VI2ATB
074200931006     C                   MOVEL     *BLANKS       VI2CAP
074300931006     C                   MOVEL     *BLANKS       DE2CAP
074400940919     C                   Z-ADD     0             VI2SGL
074500940919     C                   Z-ADD     0             VI2ITR
074600931006     C                   Z-ADD     0             VI2TAR
074700940919     C                   Z-ADD     0             VI2MIN
074800940919     C                   Z-ADD     0             VI2MAX
074900931006     C*
075000940919     C                   Z-ADD     0             SAVITR
075100931006     C                   Z-ADD     0             SAVTAR
075200940919     C                   Z-ADD     0             SAVMIN
075300940919     C                   Z-ADD     0             SAVMAX
075400931006     C                   Z-ADD     0             VI2NRR
075500931006     C*
075600931006     C                   ADD       1             NR2
075700931006     C                   ADD       1             B
075800020128     C                   WRITE     cn11S06
075900931006     C*
076000931006     C                   SETOFF                                       16
076100931006     C                   ENDDO
076200931006     C*
076300931006     C                   Z-ADD     NR2           SAVNR2
076400931006     C                   Z-ADD     NR2           REC2
076500931006     C                   ENDSR
076600020201      *--------------------------------------------------
076700020201     C     CONTR         BEGSR
076800020201      *--------------------------------------------------
076900020201     C                   SETOFF                                       9099
077000020201     C*
077100020201     C****  TIPO TARIFFA  ****  DA TABELLA TR
077200020201     C     VI2TPG        IFEQ      '?'
077300020201     C                   MOVEL     'TR'          COD
077400020201     C                   MOVE      ' '           VI2TPG
077500020201     C                   MOVEL     *BLANKS       DES              25
077600020201     C                   CALL      'X§TABER'
077700020201     C                   PARM                    CODUT
077800020201     C                   PARM                    COD
077900020201     C                   PARM                    KEY
078000020201     C                   PARM                    DES
078100020201     C                   MOVEL     KEY           VI2TPG
078200020201     C                   SETON                                        99
078300020201     C                   GOTO      ENDCTR
078400020201     C                   ENDIF
078500020201     C*
078600020201     C* TIPO TARIFFA OBBLIGATORIO
078700020201     C     VI2TPG        IFEQ      ' '
078800020201     C                   SETON                                        4090
078900020201     C                   GOTO      ENDCTR
079000020201     C                   ENDIF
079100020201     C* CONTROLLO SE ESISTE TIPO TARIFFA
079200020201     C                   Z-ADD     1             X
079300020201     C     VI2TPG        LOOKUP    TPG(X)                                 35
079400020201     C  N35              SETON                                        4190
079500020201     C  N35              GOTO      ENDCTR
079600020201     C* IL TIPO TARIFFA "V" HA PRESO IL POSTO DEL TIPO TARIFFA "0"
079700020201     C* IN VARIAZIONE: - SE TIPO TARIFFA =  V NON SI PUO' VARIARE
079800020201     C*                - SE TIPO TARIFFA <> V NON SI PUO' IMMETTERE 0
079900020201     C   02SAVTPG        IFEQ      'V'
080000020201     C     VI2TPG        IFNE      'V'
080100020201     C                   SETON                                        4290
080200020201     C                   GOTO      ENDCTR
080300020201     C                   ENDIF
080400020201     C                   ELSE
080500020201     C     VI2TPG        IFEQ      '0'
080600020201     C                   SETON                                        4290
080700020201     C                   GOTO      ENDCTR
080800020201     C                   ENDIF
080900020201     C                   ENDIF
081000020214      * controllo valore inserito nel campo tipo somma
081100020214      *
081200020315     c                   if        vi2tsm = *blank
081300020315     c                   seton                                        3490
081400020315     C                   GOTO      ENDCTR
081500020315     c                   else
081600020315     c                   exsr      contrtsm
081700020315     c   90              goto      endctr
081800020315     c                   endif
081900020201     C*
082000020204     C****  ARROTONDAMENTI  ****
082100020204     C* DEVONO ESSERE O TUTTI <> 0  OPPURE  TUTTI = 0
082200020204     C     VI2ARL        IFEQ      0
082300020204     C*
082400020204     C     VI2ARF        IFNE      0
082500020204     C                   SETON                                        4390
082600020204     C                   GOTO      ENDCTr
082700020204     C                   ENDIF
082800020204     C     VI2ARO        IFNE      0
082900020204     C                   SETON                                        4390
083000020204     C                   GOTO      ENDCTr
083100020204     C                   ENDIF
083200020204     C*
083300020204     C                   ELSE
083400020204     C     VI2ARF        IFEQ      0
083500020204     C                   SETON                                        4390
083600020204     C                   GOTO      ENDCTr
083700020204     C                   ENDIF
083800020204     C     VI2ARO        IFEQ      0
083900020204     C                   SETON                                        4390
084000020204     C                   GOTO      ENDCTr
084100020204     C                   ENDIF
084200020204     C*
084300020204     C                   ENDIF
084400100113     C*
084500100113      * non può il valore essere inferiore a 0,001
084600100210     c                   if        vidata < 0,001 and vidata <> 0
084700100113     c                   seton                                        7090
084800100113     C                   GOTO      ENDCTr
084900100113     c                   endif
085000100113     C*
085100020201     C     ENDCTR        ENDSR
085200020201     C*
085300931006     C*
085400020315      *--------------------------------------------------
085500020315     c     contrtsm      begsr
085600020315      *--------------------------------------------------
085700020315     c                   clear                   key1
085800020315     c                   clear                   key2
085900020315     c                   clear                   tipitsm
086000020315     c
086100020315     c                   eval      key1 = vidctd + 'TSM'
086200020315     c                   if        vidctr < 999
086300020315     c                   movel     'D'           key2
086400020315     c                   else
086500020315     c                   if        vidtsr = 'G'
086600020315     c                   movel     'G'           key2
086700020315     c                   else
086800020315     c                   movel     'P'           key2
086900020315     c                   end
087000020315     c                   end
087100020315     c     ktbe2         chain     tntbe01l                           91
087200020315     c                   if        not *in91
087300020315     c                   movea     tbeuni        tipitsm
087400020315     c                   movel     tbeuni        vi2tsmd
087500020315     c                   endif
087600020315     c     vi2tsm        lookup    tipitsm                                37
087700031104      * attenzione le poste per il carriaggio di consegna possono essere solo
087800031104      * a spedizione e non a Stop:
087900031104     c                   if        ComTPT = 'O' and VidCTD ='BAC' and
088000031104     c                             vi2TSM <>'S'
088100031104     c                   setoff                                       37
088200031104     c                   end
088300020315     c  n37              seton                                        3690
088400020315     c   37              setoff                                       36
088500020315      *
088600020315     c                   endsr
088700931006     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
088800931006     C     CONCTL        BEGSR
088900020128     C*------------------------------------------------
089000931006     C                   SETOFF                                       90
089100020228     c     vi2tpg        comp      'V'                                    05
089200931006     C*
089300931006     C     ENDCTL        ENDSR
089400931006     C*
089500931122     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
089600931122     C     CONSFL        BEGSR
089700020128     C*------------------------------------------------
089800931122     C                   SETOFF                                       900708
089900931122     C                   SETOFF                                       1314
090000931122     C                   SETON                                        17
090100931122     C                   Z-ADD     1             NR2
090200931122     C*
090300020128     C     NR2           CHAIN     cn11S06                            33
090400950114     C*
090500950114    0C     *IN33         DOWEQ     *OFF
090600931122     C*
090700941221    1C     VI2CAP        IFNE      *BLANKS
090800931122     C     COMREC        ORNE      *ZEROS
090900940929     C     COMREC        ANDNE     *BLANKS
091000931122     C*
091100931122     C* VI2ATB = 'A'  SALTO I CONTROLLI RELATIVI AL C.A.P. E SCAGLIONE
091200941221    2C     VI2ATB        IFEQ      ' '
091300931122     C*
091400931122     C****  C. A. P.  ****
091500941221    3C     VI2CAP        IFEQ      *BLANKS
091600931122     C* 13 ON  - C.A.P. VUOTO
091700931122     C                   SETON                                        13
091800941221   X3C                   ELSE
091900931122     C* 14 ON  - C.A.P. PIENO
092000931122     C                   SETON                                        14
092100941221     C* 07 ON  - INDICA IL C.A.P. "IT000"
092200941221    4C     VI2CAP        IFEQ      CIT000
092300941221     C                   SETON                                        07
092400941221    4C                   ENDIF
092500931122     C*
092600941221     C* 08 ON  - INDICA IL C.A.P. "IT100"
092700941221    4C     VI2CAP        IFEQ      CIT100
092800931122     C                   SETON                                        08
092900941221    4C                   ENDIF
093000941221    3C                   ENDIF
093100931122     C*
093200941221    2C                   ENDIF
093300931122     C*
093400931122     C* VI2NRR = 0  ===> SI TRATTA DI RECORD DI NUOVA IMMISSIONE QUINDI
093500931122     C*                  SPENGO *IN15 PER NON PROTEGGERE CAMPI DEL SFL
093600941221    2C     VI2NRR        IFEQ      0
093700931122     C                   SETOFF                                       15
093800931122     C*
093900931122     C* PRESENTI RIGHE SENZA CAP: IMPOSSIBILE INSERIRE RIGHE CON CAP
094000941221    3C   13VI2CAP        IFNE      *BLANKS
094100931122     C                   SETON                                        5690
094200931122     C                   GOTO      ENDSFL
094300941221    3C                   ENDIF
094400941221     C* PRESENTI RIGHE CON CAP: IMPOSSIBILE INSERIRE RIGHE SENZA CAP
094500941221    3C   14VI2CAP        IFEQ      *BLANKS
094600931122     C                   SETON                                        5490
094700931122     C                   GOTO      ENDSFL
094800941221    3C                   ENDIF
094900931122     C*
095000941221    3C     VI2CAP        IFNE      *BLANKS
095100941221     C*
095200950114    4C     VI2CAP        IFNE      CIT000
095300950110     C     VI2CAP        ANDNE     CIT100
095400950110     C*
095500970718     C                   CLEAR                   DSSI95
095600970718     C                   MOVEL     '3'           I95TCN                         *TIPO CONTROLLO
095700970718     C                   MOVEL     '  '          I95NAR                         *NAZIONE
095800020218      *Devo cercare cap generico provincia con blank anzichè con 000
095900020218      *perchè sul cappario non esiste più  (es. se cap ="17000" lo passo al
096000020218      *TISI95 ="17010"  suggerimento di L.B.
096100020218     C                   movel     VI2CAP        W5                5
096200020218     C                   move      W5            W3                3
096300020218     C     W3            ifeq      '000'
096400020218     C                   MOVEL     VI2CAP        I95CAP                         *C.A.P.
096500020218     C                   move      '010    '     I95CAP
096600020218     C                   else
096700020218     C                   MOVEL     VI2CAP        I95CAP                         *C.A.P.
096800020218     C                   END
096900040206     C                   MOVEL     wuda          I95DAT
097000970718     C                   CALL      'TISI95R'
097100970718     C                   PARM                    DSSI95
097200970718     C                   MOVEL     '1'           TISI95            1
097300941221     C*
097400970718    5C     O95ERR        IFNE      *BLANKS
097500970718     C     O95CAP        ORNE      I95CAP
097600941221     C                   SETON                                        4590
097700941221     C                   GOTO      ENDSFL
097800950114    5C                   ENDIF
097900950114    4C                   ENDIF
098000941221     C*
098100970718     C                   CLEAR                   PARAM
098200941221     C                   MOVEL     VI2CAP        PARCAP                         *C.A.P.
098300931122     C                   EXSR      DECCAP
098400941221    3C                   ENDIF
098500931122     C*
098600931122     C****  SCAGLIONE  ****
098700931122     C* SCAGLIONE OBBLIGATORIO
098800950114    3C     VI2SGL        IFEQ      0
098900931122     C                   SETON                                        4690
099000931122     C                   GOTO      ENDSFL
099100950114    3C                   ENDIF
099200931122     C*
099300931122     C* CONTROLLO SE RIGHE DOPPIE
099400940929     C* MUOVO LO SCAGLIONE IN UN CAMPO DI COMODO PRIMA DI EFFETTUARE
099500940929     C*   LA CHAIN PERCHE' A VIDEO LO SCAGLIONE E' LUNGO 10 SENZA
099600940929     C*   DECIMALI MENTRE NEL FILE E' LUNGO 13 CON 3 DECIMALI
099700940929     C                   Z-ADD     VI2SGL        COMSGL
099800011029     C     KFPD          CHAIN     fifpd000                           32
099900931122     C  N32FPDNRR        IFNE      VI2NRR
100000950114    3C     FPDATB        ANDEQ     ' '
100100931122     C                   SETON                                        4790
100200931122     C                   GOTO      ENDSFL
100300950114    3C                   ENDIF
100400931122     C*
100500950114   X2C                   ELSE
100600931122     C* VI2NRR <> 0 ===> NON SI TRATTA DI RECORD DI NUOVA IMMISSIONE
100700931122     C*                  QUINDI ACCENDO *IN15 PER PROTEGGERE I CAMPI
100800931122     C                   SETON                                        15
100900950114    2C                   ENDIF
101000100112     C*controllo decimali se divisa itl  (da 3 ora portati a 5)
101100011128     c                   if        viddiv = §geditl
101200100112     c                   z-add     0             dec5              5 0
101300011128     c                   if        vi2itr > 0
101400100112     c                   move      vi2itr        dec5
101500100112     c                   if        dec5 > 0
101600011128     c                   seton                                        5990
101700011128     C                   GOTO      endsfl
101800011128     c                   endif
101900011128     c                   endif
102000100112     c                   z-add     0             dec5
102100011128     c                   if        vi2min > 0
102200100112     c                   move      vi2min        dec5
102300100112     c                   if        dec5 > 0
102400100113     c                   seton                                        589097
102500011128     C                   GOTO      endsfl
102600011128     c                   endif
102700011128     c                   endif
102800100112     c                   z-add     0             dec5
102900011128     c                   if        vi2max > 0
103000100112     c                   move      vi2max        dec5
103100100112     c                   if        dec5 > 0
103200100113     c                   seton                                        579097
103300011128     C                   GOTO      endsfl
103400011128     c                   endif
103500011128     c                   endif
103600931122     C*
103700011128     c                   endif
103800931122     C****  IMPORTO TARIFFA  ****
103900020604    2C*    *IN05         IFEQ      *OFF
104000931122     C* SI PUO' ACCETTARE L'IMPORTO TARIFFA = 0  CON IL CMD14-FORZATURA
104100020604     C*    VI2ITR        IFEQ      0
104200020604     C* NKN              SETON                                        5090
104300020604     C*  90              GOTO      ENDSFL
104400020604     C*                  ENDIF
104500950114     C*
104600020604   X2C*                  ELSE
104700100113      * non può il valore essere inferiore a 0,001
104800100210     c                   if        vi2itr < 0,001 and vi2itr <> 0
104900100113     c                   seton                                        7190
105000100113     C                   GOTO      endsfl
105100100113     c                   endif
105200931122     C* SI PUO' ACCETTARE IL TASSO % = 0  CON IL CMD14-FORZATURA
105300020604     C*    VI2TAR        IFEQ      0
105400020604     C* NKN              SETON                                        5190
105500020604     C*  90              GOTO      ENDSFL
105600020604     C*                  ENDIF
105700020604    2C*                  ENDIF
105800100113      *
105900100113      * non può il valore essere inferiore a 0,001
106000100210     c                   if        vi2min < 0,001 and vi2min <> 0
106100100113     c                   seton                                        729097
106200100113     C                   GOTO      endsfl
106300100113     c                   endif
106400100113      *
106500100113      * non può il valore essere inferiore a 0,001
106600100210     c                   if        vi2max < 0,001 and vi2max <> 0
106700100113     c                   seton                                        739097
106800100113     C                   GOTO      endsfl
106900100113     c                   endif
107000931122     C*
107100931122     C****  IMPORTO MASSIMO TARIFFA  ****
107200931122     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
107300950114    2C     VI2MAX        IFGT      0
107400940919     C     VI2MAX        ANDLT     VI2MIN
107500100113     C                   SETON                                        529097
107600931122     C                   GOTO      ENDSFL
107700950114    2C                   ENDIF
107800100419      *
107900100419      *  Obbligatorietà del Minimo e Massimo se dettaglio
108000100419     c                   if        vi2itr > 0 and minmaxOBBLIG = 'S' and
108100100419     c                             (vi2MIN = 0 or vi2MAX = 0)
108200100419     c                   seton                                        749097
108300100419     C                   GOTO      endsfl
108400100419     c                   endif
108500100419     C*
108600931122     C*
108700931122     C* SE NON CI SONO ERRORI AGGIORNO E VADO AVANTI A LEGGERE
108800931122     C                   MOVEL     VI2CAP        DUPCAP
108900940919     C                   Z-ADD     VI2SGL        DUPSGL
109000940919     C  N05              Z-ADD     VI2ITR        DUPITR
109100931122     C   05              Z-ADD     VI2TAR        DUPTAR
109200940919     C                   Z-ADD     VI2MIN        DUPMIN
109300940919     C                   Z-ADD     VI2MAX        DUPMAX
109400931122     C*
109500950114   X1C                   ELSE
109600950117     C                   SETOFF                                       15
109700950114    1C                   ENDIF
109800931122     C*
109900931122     C     ENDSFL        TAG
110000931122     C   90              Z-ADD     NR2           REC2
110100931122     C*
110200950117     C* 25 ON - SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
110300950117     C*   LA LETTERA "M" SULLA RELATIVA RIGA DI DETTAGLIO
110400950117     C                   SETOFF                                       25
110500950117    1C     VI2MIN        IFGT      0
110600950117     C     VI2MAX        ORGT      0
110700950117     C                   SETON                                        25
110800950117    1C                   ENDIF
110900950117     C*
111000020128     C                   UPDATE    cn11S06
111100931122     C   90              SETON                                        33
111200931122     C*
111300931122     C                   ADD       1             NR2
111400020128     C  N90NR2           CHAIN     cn11S06                            33
111500950114    0C                   ENDDO
111600931122     C*
111700950114     C*
111800931122     C* SOLO SE NON SONO PRESENTI ERRORI
111900950114    0C     *IN90         IFEQ      *OFF
112000931122     C*
112100931122     C* SE INSERITO C.A.P."IT000" OCCORRE INSERIRE ANCHE C.A.P."IT100"
112200931122     C   07
112300931122     CANN08              SETON                                        4890
112400931122     C* SE INSERITO C.A.P."IT100" OCCORRE INSERIRE ANCHE C.A.P."IT000"
112500931122     C  N07
112600931122     CAN 08              SETON                                        4990
112700931122     C* NON E' STATA INSERITA NEMMENO UNA RIGA DI DETTAGLIO
112800931122     C  N13
112900931122     CANN14              SETON                                        5390
113000931122     C*
113100950114    0C                   ENDIF
113200931122     C*
113300931122     C                   SETOFF                                       17
113400931122     C                   ENDSR
113500931122     C*
113600931006     C*--- AGGIORNO CAMPI DEL SFL ------------------------------------*
113700931006     C     REGIS         BEGSR
113800020128     C*------------------------------------------------
113900931006     C                   Z-ADD     1             NR2
114000931006     C*
114100020128     C     NR2           CHAIN     cn11S06                            31
114200931006     C     *IN31         DOWEQ     *OFF
114300931006     C*
114400931014     C     COMREC        IFNE      *ZEROS
114500940929     C     COMREC        ANDNE     *BLANKS
114600931014     C*
114700931006     C* 60 ON  - SONO STATE EFFETTUATE MODIFICHE SU TFFGD00F
114800931006     C                   SETOFF                                       60
114900931006     C*
115000931006     C* VI2NRR = 0  ===> SI TRATTA DI RECORD DI NUOVA IMMISSIONE QUINDI
115100931006     C*                  ACCENDO *IN60 PER REGISTRARE IL RECORD
115200931006     C     VI2NRR        IFEQ      0
115300931006     C                   SETON                                        60
115400931006     C*
115500931006     C                   ELSE
115600931006     C     VI2ATB        IFEQ      ' '
115700931007     C     CO2REC        IFNE      SAVREC
115800931006     C                   SETON                                        60
115900931006     C                   ENDIF
116000931006     C*
116100931006     C                   ELSE
116200931006     C                   SETON                                        60
116300931006     C                   ENDIF
116400931006     C                   ENDIF
116500931006     C*
116600940929     C* MUOVO LO SCAGLIONE IN UN CAMPO DI COMODO PRIMA DI EFFETTUARE
116700940929     C*   LA CHAIN PERCHE' A VIDEO LO SCAGLIONE E' LUNGO 10 SENZA
116800940929     C*   DECIMALI MENTRE NEL FILE E' LUNGO 13 CON 3 DECIMALI
116900940929     C                   Z-ADD     VI2SGL        COMSGL
117000940929     C*
117100011029     C   60KFPD          CHAIN     fifpd000                           33
117200931006     C   60VI2ATB        IFNE      ' '
117300931006     C*
117400931006     C     *IN33         IFEQ      *OFF
117500931006     C                   MOVEL     'A'           FPDATB
117600091210     C**********         MOVEL     'T'           FPDFTR
117700091210     C**********         MOVEL     *ALL'9'       FPDDTR
117800011029     C                   UPDATE    fifpd000
117900931006     C*
118000931006     C* RIEMPIO IL FLAG IN MODO CHE SE ANNULLANO DELLE RIGHE DI
118100011029     C*   DETTAGLIO (fifpd00F), AGGIORNO LA DATA ULTIMA VARIAZIONE
118200011029     C*   PRESENTE NEL FILE fifpt00F
118300931006     C                   MOVEL     'A'           FLGDUV
118400931006     C                   ENDIF
118500931006     C*
118600931006     C                   ELSE
118700931006     C* 33 ON  - IMMISSIONE
118800931006     C* 33 OFF - VARIAZIONE
118900020110     C   33              clear                   fifpd000
119000020110     c
119100940919     C  N05              Z-ADD     VI2ITR        FPDITR
119200940919     C   05              Z-ADD     VI2TAR        FPDITR
119300940919     C                   Z-ADD     VI2MIN        FPDMIN
119400940919     C                   Z-ADD     VI2MAX        FPDMAX
119500931006     C*
119600931006     C                   MOVEL     VI2ATB        FPDATB
119700091210     C**********         MOVEL     'T'           FPDFTR
119800091210     C**********         MOVEL     *ALL'9'       FPDDTR
119900931006     C* SOLO PER IMMISSIONE
120000931006     C     *IN33         IFEQ      *ON
120100931006     C                   Z-ADD     COMPDR        FPDPDR
120200931014     C                   MOVEL     COMSML        FPDSML
120300931014     C                   MOVEL     VIDTSR        FPDTSR
120400931006     C                   Z-ADD     VIDCTR        FPDCTR
120500931006     C                   Z-ADD     VIDPRG        FPDPRG
120600020128     C                   MOVEL     vidctd        FPDctd
120700931006     C                   MOVEL     VI2CAP        FPDCAP
120800940919     C                   Z-ADD     VI2SGL        FPDSGL
120900931006     C                   ENDIF
121000931006     C*
121100011029     C  N33              UPDATE    fifpd000
121200011029     C   33              WRITE     fifpd000
121300931006     C*
121400931006     C* RIEMPIO IL FLAG IN MODO CHE SE INSERISCONO O VARIANO DELLE
121500011029     C*   RIGHE DI DETTAGLIO (fifpd00F), AGGIORNO LA DATA ULTIMA
121600011029     C*   VARIAZIONE PRESENTE NEL FILE fifpt00F
121700931006     C                   MOVEL     'A'           FLGDUV
121800931006     C*
121900931006     C                   ENDIF
122000931014     C                   ENDIF
122100931006     C*
122200931006     C                   ADD       1             NR2
122300020128     C     NR2           CHAIN     cn11S06                            31
122400931006     C                   ENDDO
122500931006     C                   ENDSR
122600931006     C*
122700931005     C*--- AGGIORNO CAMPI DEL SFLCTL ---------------------------------*
122800931005     C     REGIS2        BEGSR
122900020128     C*------------------------------------------------
123000931005     C*
123100011029     C     KFPT          CHAIN     fifpt000                           32
123200931005     C* 32 ON  - IMMISSIONE
123300931005     C* 32 OFF - VARIAZIONE
123400020110     C   32              clear                   fifpt000
123500931005     C                   MOVEL     ' '           FPTATB
123600931005     C                   MOVEL     VI2TPG        FPTTPG
123700020212     C                   MOVEL     Vi2tsm        FPTTsm
123800020214     C                   MOVEL     vi2tat        FPTTat
123900020212     C                   MOVEL     Vi2tpp        FPTTpp
124000020129     C                   z-add     VIdata        FPTata
124100940919     C                   Z-ADD     VI2ARL        FPTARL
124200940919     C                   Z-ADD     VI2ARF        FPTARF
124300020201     C                   Z-ADD     VI2ARO        FPTARO
124400931005     C                   Z-ADD     VI2RPV        FPTRPV
124500020129     C                   MOVEL     VIDTSR        FPTTSR
124600020214     c   28              movel     dsbonus       fptflr
124700020214     c  n28              clear                   fptflr
124800931005     C* SOLO PER IMMISSIONE
124900931005     C     *IN32         IFEQ      *ON
125000931005     C                   Z-ADD     COMPDR        FPTPDR
125100931014     C                   MOVEL     COMSML        FPTSML
125200931005     C                   Z-ADD     VIDCTR        FPTCTR
125300931005     C                   Z-ADD     VIDPRG        FPTPRG
125400020128     C                   MOVEL     vidctd        FPTctd
125500931005     C                   ENDIF
125600931005     C*
125700011029     C* SE VIENE VARIATO fifpt ABBLENCO FLAG E DATA TRASMISSIONE
125800931005     C     COMFPT        IFNE      FPTDS
125900091210     C**********         MOVEL     'T'           FPTFTR
126000091210     C**********         MOVEL     *ALL'9'       FPTDTR
126100931005     C*
126200011029     C* RIEMPIO IL FLAG IN MODO CHE SE MI VARIANO fifpt AGGIORNO
126300940919     C*   IL CAMPO DATA ULTIMA VARIAZIONE (PRESENTE NEL TNTA80R)
126400931005     C                   MOVEL     'A'           FLGDUV
126500931005     C                   ENDIF
126600950309     C*
126700011029     C  N32              UPDATE    fifpt000
126800011029     C   32              WRITE     fifpt000
126900931005     C*
127000931006     C                   SETON                                        0203
127100931005     C                   SETOFF                                       01
127200931005     C*
127300931005     C                   MOVEL     VI2TPG        SAVTPG            1
127400931005     C                   ENDSR
127500931005     C*
127600931006     C*--- DECODIFICA  C.A.P. ----------------------------------------*
127700931006     C     DECCAP        BEGSR
127800020128     C*------------------------------------------------
127900931116     C*
128000931116     C                   Z-ADD     3             PARNDE                         *NUMERO DECODIFICHE
128100040206     C                   Z-ADD     wuda          PARDRI
128200931116     C*
128300931116     C                   CALL      'TRUL05R'
128400931116     C                   PARM                    PARAM
128500931116     C*
128600931116     C                   MOVEL     PARDEC        DE2CAP
128700931116     C                   MOVEL     '1'           TRUL05            1
128800931116     C*
128900931006     C                   ENDSR
