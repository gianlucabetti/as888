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
050200020213
050300020429     C* CARICO LA TABELLA DEI TIPI TARIFFE UTILIZZATI DALLA TARIFFA
050400020429     C* do dettaglio sELEZIONATA
050500020429     C                   MOVEL     '8A'          COD
050600020429     C                   MOVEL     *BLANKS       KEY
050700020429     C                   MOVEL     Vidctd        KEY
050800020429     C     KTAB          CHAIN     TABEL                              30
050900020429     C     *IN30         IFEQ      *OFF
051000020429     C*
051100020429     C                   MOVEL     TBLUNI        DS8A
051200020429     c                   endif
051300020213      *forzo il primo codice della tariffa se si tratta di tariffa base
051400020327     c                   if        vidctd = 'BAR' or vidctd = 'BAC'
051500020213     c                   movel     vidctr        wtt(1)
051600020213     c                   else
051700020429      *solo se ho trovato la tabella
051800020128     C     *IN30         IFEQ      *OFF
051900020128     C*
052000020201     C* CARICO I TIPI TARIFFE ABILITATI A QUESTA TARIFFA PARTICOLARE
052100020201     c                   select
052200020201     c                   when      vidctr < 999
052300020128     C                   MOVEL     §8ATT0        WTT(1)
052400020128     C                   MOVEL     §8ATT1        WTT(2)
052500020128     C                   MOVEL     §8ATT2        WTT(3)
052600020128     C                   MOVEL     §8ATT3        WTT(4)
052700020128     C                   MOVEL     §8ATT4        WTT(5)
052800020128     C                   MOVEL     §8ATT5        WTT(6)
052900020201     c                   when      vidctr = 999 and vidtsr = 'C' or
053000020201     c                             vidctr = 999 and vidtsr = 'R'
053100020201     C                   MOVEL     §8ATT6        WTT(1)
053200020201     C                   MOVEL     §8ATT7        WTT(2)
053300020201     C                   MOVEL     §8ATT8        WTT(3)
053400020201     C                   MOVEL     §8ATT9        WTT(4)
053500020201     C                   MOVEL     §8ATTV        WTT(5)
053600020201     C                   MOVEL     *blank        WTT(6)
053700020201     c                   when      vidctr = 999 and vidtsr = 'G'
053800020201     C                   MOVEL     §8ATb1        WTT(1)
053900020201     C                   MOVEL     §8ATb2        WTT(2)
054000020201     C                   MOVEL     §8ATb3        WTT(3)
054100020201     C                   MOVEL     §8ATb4        WTT(4)
054200020201     C                   MOVEL     §8ATb5        WTT(5)
054300020201     C                   MOVEL     *blank        WTT(6)
054400020201
054500020201     c                   endsl
054600020213
054700020128     C                   ENDIF
054800020213
054900020213     c                   endif
055000020213
055100020128     C*
055200020128     C* CARICO TABELLA CODICI TARIFFA
055300020128     C                   EXSR      CARTAB
055400020128     C*
055500020128     C                   MOVEL     *BLANKS       TPG1
055600020128     C                   MOVEL     *BLANKS       DPG1
055700020128     C                   MOVEL     *BLANKS       TPG2
055800020128     C                   MOVEL     *BLANKS       DPG2
055900020128     C                   MOVEL     *BLANKS       TPG3
056000020128     C                   MOVEL     *BLANKS       DPG3
056100020128     C                   MOVEL     *BLANKS       TPG4
056200020128     C                   MOVEL     *BLANKS       DPG4
056300020128     C*
056400020128     C******     *IN06     IFEQ *ON
056500020128     C*
056600020128     C     TPG(1)        IFNE      ' '
056700020128     C                   MOVEL     TPG(1)        TPG1
056800020128     C                   MOVEL     '='           DPG1
056900020214     C                   MOVEl     DPG(1)        DPG1
057000020128     C                   ENDIF
057100020128     C     TPG(2)        IFNE      ' '
057200020128     C                   MOVEL     TPG(2)        TPG2
057300020128     C                   MOVEL     '='           DPG2
057400020214     C                   MOVEl     DPG(2)        DPG2
057500020128     C                   ENDIF
057600020128     C     TPG(3)        IFNE      ' '
057700020128     C                   MOVEL     TPG(3)        TPG3
057800020128     C                   MOVEL     '='           DPG3
057900020214     C                   MOVEl     DPG(3)        DPG3
058000020128     C                   ENDIF
058100020128     C     TPG(4)        IFNE      ' '
058200020128     C                   MOVEL     TPG(4)        TPG4
058300020128     C                   MOVEL     '='           DPG4
058400020214     C                   MOVEl     DPG(4)        DPG4
058500020128     C                   ENDIF
058600020128     C*
058700020128     C                   ENDSR
058800020128     C*------------------------------------------------
058900931006     C*--- PULIZIA SUBFILE CONTROL -----------------------------------*
059000931006     C     PULCTL        BEGSR
059100020128     C*------------------------------------------------
059200940919     C                   Z-ADD     0             VI2ARL
059300940919     C                   Z-ADD     0             VI2ARF
059400940919     C                   Z-ADD     0             VI2ARO
059500931005     C                   Z-ADD     0             VI2RPV
059600020129     C                   MOVEL     *BLANKS       COMFPT
059700931005     C                   ENDSR
059800931005     C*
059900931006     C*--- ANNULLAMENTO TOTALE TARIFFA -------------------------------*
060000931006     C     ANNTAR        BEGSR
060100020128     C*------------------------------------------------
060200931007     C                   SETOFF                                       32
060300931006     C* ANNULLO DETTAGLIO
060400011029     C     KFPT          SETLL     fifpd000
060500931006     C     *IN32         DOWEQ     *OFF
060600011029     C     KFPT          READE     fifpd000                               32
060700931006     C  N32FPDATB        IFEQ      ' '
060800931006     C                   MOVEL     'A'           FPDATB
060900091210     C**********         MOVEL     'T'           FPDFTR
061000091210     C**********         MOVEL     *ALL'9'       FPDDTR
061100011029     C                   UPDATE    fifpd000
061200931006     C                   ENDIF
061300931006     C                   ENDDO
061400931006     C*
061500931006     C* ANNULLO TESTATA
061600011029     C     KFPT          CHAIN     fifpt000                           32
061700931006     C     *IN32         IFEQ      *OFF
061800931006     C                   MOVEL     'A'           FPTATB
061900091210     C**********         MOVEL     'T'           FPTFTR
062000091210     C**********         MOVEL     *ALL'9'       FPTDTR
062100011029     C                   UPDATE    fifpt000
062200931006     C                   ENDIF
062300931006     C*
062400931006     C* RIEMPIO IL FLAG IN MODO CHE SE ANNULLANO UNA CONSEGNA PARTIC.
062500940919     C*   AGGIORNO LA DATA ULTIMA VARIAZIONE (PRESENTE NEL TNTA80R)
062600931006     C                   MOVEL     'A'           FLGDUV
062700931006     C*
062800931007     C* 04 ON  - E' STATA ANNULLATA UNA TARIFFA PARTICOLARE
062900931006     C                   SETON                                        04
063000931006     C                   ENDSR
063100931006     C*
063200931006     C*
063300931006     C*--- PULIZIA SUBFILE -------------------------------------------*
063400931007     C     PULSFL        BEGSR
063500020128     C*------------------------------------------------
063600931005     C                   SETON                                        88
063700020128     C                   WRITE     cn11C06
063800931005     C                   SETOFF                                       88
063900931006     C                   ENDSR
064000931006     C*
064100931025     C*--- CARICAMENTO SUBFILE ---------------------------------------*
064200931007     C     CARSFL        BEGSR
064300020128     C*------------------------------------------------
064400931006     C                   SETOFF                                       90
064500931008     C                   SETON                                          15
064600931006     C                   Z-ADD     0             NR2               4 0
064700020228     c     vi2tpg        comp      'V'                                    05
064800931006     C*
064900011029     C     KFPT          SETLL     fifpd000
065000011029     C     KFPT          READE     fifpd000                               32
065100931006     C*
065200931006     C     *IN32         DOWEQ     *OFF
065300931006     C* ESCLUDO RECORD ANNULLATI
065400931006     C     FPDATB        IFEQ      ' '
065500090922     c* ind 89 protegge i campi di input del sflctl t01
065600090922     c                   seton                                        89
065700931006     C* C.A.P.
065800931006     C                   MOVEL     FPDCAP        VI2CAP
065900931006     C* DECODIFICA C.A.P.
066000970718     C                   CLEAR                   PARAM
066100931116     C                   MOVEL     FPDCAP        PARCAP                         *C.A.P.
066200931006     C                   EXSR      DECCAP
066300931006     C* SCAGLIONE
066400940929     C                   Z-ADD     FPDSGL        VI2SGL
066500931006     C* IMPORTO TARIFFA
066600931007     C     *IN05         IFEQ      *OFF
066700940919     C                   Z-ADD     FPDITR        VI2ITR
066800940919     C                   Z-ADD     FPDITR        SAVITR
066900931006     C                   Z-ADD     0             VI2TAR
067000931006     C                   Z-ADD     0             SAVTAR
067100931006     C                   ELSE
067200940919     C                   Z-ADD     FPDITR        VI2TAR
067300940919     C                   Z-ADD     FPDITR        SAVTAR
067400940919     C                   Z-ADD     0             VI2ITR
067500940919     C                   Z-ADD     0             SAVITR
067600931006     C                   ENDIF
067700931006     C* MINIMO  TARIFFA
067800011127     C                   z-add     FPDMIN        VI2MIN
067900011127     C                   z-add     FPDMIN        SAVMIN
068000931006     C* MASSIMO TARIFFA
068100011127     C                   z-add     FPDMAX        VI2MAX
068200011127     C                   z-add     FPDMAX        SAVMAX
068300950117     C*
068400950117     C* 25 ON - SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
068500950117     C*   LA LETTERA "M" SULLA RELATIVA RIGA DI DETTAGLIO
068600950117     C                   SETOFF                                       25
068700950117    2C     VI2MIN        IFGT      0
068800950117     C     VI2MAX        ORGT      0
068900950117     C                   SETON                                        25
069000950117    2C                   ENDIF
069100950117     C*
069200931006     C* FLAG ANNULLAMENTO
069300931006     C                   MOVEL     ' '           VI2ATB
069400931006     C*
069500931006     C                   Z-ADD     FPDNRR        VI2NRR
069600931006     C*
069700931006     C                   ADD       1             NR2
069800020128     C                   WRITE     cn11S06
069900931006     C                   ENDIF
070000931006     C*
070100011029     C     KFPT          READE     fifpd000                               32
070200931006     C                   ENDDO
070300931006     C*
070400931006     C                   Z-ADD     NR2           SAVNR2            4 0
070500931006     C*
070600931006     C     NR2           IFGT      0
070700931013     C     NR2           DIV       8             DIVIS             3 0
070800931006     C                   MVR                     RESTO             3 0
070900931006     C     RESTO         IFGT      0
071000931006     C                   EXSR      SCRIVI
071100931006     C                   ENDIF
071200931006     C*
071300931006     C                   ELSE
071400931006     C                   SETON                                        16
071500931006     C                   EXSR      SCRIVI
071600931006     C                   ENDIF
071700931006     C*
071800931006     C                   ENDSR
071900931006     C*
072000931006     C*--- SUBROUTINE DI AGGIUNTA RIGHE ------------------------------*
072100931006     C     SCRIVI        BEGSR
072200020128     C*------------------------------------------------
072300950117     C                   SETOFF                                       1525
072400931006     C                   Z-ADD     0             B                 3 0
072500931006     C*
072600931006     C     RESTO         IFNE      0
072700931006     C                   Z-ADD     RESTO         B
072800931006     C                   Z-ADD     0             RESTO
072900931006     C                   ENDIF
073000931006     C*
073100931006     C                   Z-ADD     SAVNR2        NR2
073200931006     C* CAMPI DEL SUBFILE
073300931013     C     B             DOWLT     8
073400931006     C                   MOVEL     ' '           VI2ATB
073500931006     C                   MOVEL     *BLANKS       VI2CAP
073600931006     C                   MOVEL     *BLANKS       DE2CAP
073700940919     C                   Z-ADD     0             VI2SGL
073800940919     C                   Z-ADD     0             VI2ITR
073900931006     C                   Z-ADD     0             VI2TAR
074000940919     C                   Z-ADD     0             VI2MIN
074100940919     C                   Z-ADD     0             VI2MAX
074200931006     C*
074300940919     C                   Z-ADD     0             SAVITR
074400931006     C                   Z-ADD     0             SAVTAR
074500940919     C                   Z-ADD     0             SAVMIN
074600940919     C                   Z-ADD     0             SAVMAX
074700931006     C                   Z-ADD     0             VI2NRR
074800931006     C*
074900931006     C                   ADD       1             NR2
075000931006     C                   ADD       1             B
075100020128     C                   WRITE     cn11S06
075200931006     C*
075300931006     C                   SETOFF                                       16
075400931006     C                   ENDDO
075500931006     C*
075600931006     C                   Z-ADD     NR2           SAVNR2
075700931006     C                   Z-ADD     NR2           REC2
075800931006     C                   ENDSR
075900020201      *--------------------------------------------------
076000020201     C     CONTR         BEGSR
076100020201      *--------------------------------------------------
076200020201     C                   SETOFF                                       9099
076300020201     C*
076400020201     C****  TIPO TARIFFA  ****  DA TABELLA TR
076500020201     C     VI2TPG        IFEQ      '?'
076600020201     C                   MOVEL     'TR'          COD
076700020201     C                   MOVE      ' '           VI2TPG
076800020201     C                   MOVEL     *BLANKS       DES              25
076900020201     C                   CALL      'X§TABER'
077000020201     C                   PARM                    CODUT
077100020201     C                   PARM                    COD
077200020201     C                   PARM                    KEY
077300020201     C                   PARM                    DES
077400020201     C                   MOVEL     KEY           VI2TPG
077500020201     C                   SETON                                        99
077600020201     C                   GOTO      ENDCTR
077700020201     C                   ENDIF
077800020201     C*
077900020201     C* TIPO TARIFFA OBBLIGATORIO
078000020201     C     VI2TPG        IFEQ      ' '
078100020201     C                   SETON                                        4090
078200020201     C                   GOTO      ENDCTR
078300020201     C                   ENDIF
078400020201     C* CONTROLLO SE ESISTE TIPO TARIFFA
078500020201     C                   Z-ADD     1             X
078600020201     C     VI2TPG        LOOKUP    TPG(X)                                 35
078700020201     C  N35              SETON                                        4190
078800020201     C  N35              GOTO      ENDCTR
078900020201     C* IL TIPO TARIFFA "V" HA PRESO IL POSTO DEL TIPO TARIFFA "0"
079000020201     C* IN VARIAZIONE: - SE TIPO TARIFFA =  V NON SI PUO' VARIARE
079100020201     C*                - SE TIPO TARIFFA <> V NON SI PUO' IMMETTERE 0
079200020201     C   02SAVTPG        IFEQ      'V'
079300020201     C     VI2TPG        IFNE      'V'
079400020201     C                   SETON                                        4290
079500020201     C                   GOTO      ENDCTR
079600020201     C                   ENDIF
079700020201     C                   ELSE
079800020201     C     VI2TPG        IFEQ      '0'
079900020201     C                   SETON                                        4290
080000020201     C                   GOTO      ENDCTR
080100020201     C                   ENDIF
080200020201     C                   ENDIF
080300020214      * controllo valore inserito nel campo tipo somma
080400020214      *
080500020315     c                   if        vi2tsm = *blank
080600020315     c                   seton                                        3490
080700020315     C                   GOTO      ENDCTR
080800020315     c                   else
080900020315     c                   exsr      contrtsm
081000020315     c   90              goto      endctr
081100020315     c                   endif
081200020201     C*
081300020204     C****  ARROTONDAMENTI  ****
081400020204     C* DEVONO ESSERE O TUTTI <> 0  OPPURE  TUTTI = 0
081500020204     C     VI2ARL        IFEQ      0
081600020204     C*
081700020204     C     VI2ARF        IFNE      0
081800020204     C                   SETON                                        4390
081900020204     C                   GOTO      ENDCTr
082000020204     C                   ENDIF
082100020204     C     VI2ARO        IFNE      0
082200020204     C                   SETON                                        4390
082300020204     C                   GOTO      ENDCTr
082400020204     C                   ENDIF
082500020204     C*
082600020204     C                   ELSE
082700020204     C     VI2ARF        IFEQ      0
082800020204     C                   SETON                                        4390
082900020204     C                   GOTO      ENDCTr
083000020204     C                   ENDIF
083100020204     C     VI2ARO        IFEQ      0
083200020204     C                   SETON                                        4390
083300020204     C                   GOTO      ENDCTr
083400020204     C                   ENDIF
083500020204     C*
083600020204     C                   ENDIF
083700100113     C*
083800100113      * non può il valore essere inferiore a 0,001
083900100210     c                   if        vidata < 0,001 and vidata <> 0
084000100113     c                   seton                                        7090
084100100113     C                   GOTO      ENDCTr
084200100113     c                   endif
084300100113     C*
084400020201     C     ENDCTR        ENDSR
084500020201     C*
084600931006     C*
084700020315      *--------------------------------------------------
084800020315     c     contrtsm      begsr
084900020315      *--------------------------------------------------
085000020315     c                   clear                   key1
085100020315     c                   clear                   key2
085200020315     c                   clear                   tipitsm
085300020315     c
085400020315     c                   eval      key1 = vidctd + 'TSM'
085500020315     c                   if        vidctr < 999
085600020315     c                   movel     'D'           key2
085700020315     c                   else
085800020315     c                   if        vidtsr = 'G'
085900020315     c                   movel     'G'           key2
086000020315     c                   else
086100020315     c                   movel     'P'           key2
086200020315     c                   end
086300020315     c                   end
086400020315     c     ktbe2         chain     tntbe01l                           91
086500020315     c                   if        not *in91
086600020315     c                   movea     tbeuni        tipitsm
086700020315     c                   movel     tbeuni        vi2tsmd
086800020315     c                   endif
086900020315     c     vi2tsm        lookup    tipitsm                                37
087000031104      * attenzione le poste per il carriaggio di consegna possono essere solo
087100031104      * a spedizione e non a Stop:
087200031104     c                   if        ComTPT = 'O' and VidCTD ='BAC' and
087300031104     c                             vi2TSM <>'S'
087400031104     c                   setoff                                       37
087500031104     c                   end
087600020315     c  n37              seton                                        3690
087700020315     c   37              setoff                                       36
087800020315      *
087900020315     c                   endsr
088000931006     C*--- CONTROLLO DATI SUBFILE CONTROL ----------------------------*
088100931006     C     CONCTL        BEGSR
088200020128     C*------------------------------------------------
088300931006     C                   SETOFF                                       90
088400020228     c     vi2tpg        comp      'V'                                    05
088500931006     C*
088600931006     C     ENDCTL        ENDSR
088700931006     C*
088800931122     C*--- CONTROLLO DATI SUBFILE ------------------------------------*
088900931122     C     CONSFL        BEGSR
089000020128     C*------------------------------------------------
089100931122     C                   SETOFF                                       900708
089200931122     C                   SETOFF                                       1314
089300931122     C                   SETON                                        17
089400931122     C                   Z-ADD     1             NR2
089500931122     C*
089600020128     C     NR2           CHAIN     cn11S06                            33
089700950114     C*
089800950114    0C     *IN33         DOWEQ     *OFF
089900931122     C*
090000941221    1C     VI2CAP        IFNE      *BLANKS
090100931122     C     COMREC        ORNE      *ZEROS
090200940929     C     COMREC        ANDNE     *BLANKS
090300931122     C*
090400931122     C* VI2ATB = 'A'  SALTO I CONTROLLI RELATIVI AL C.A.P. E SCAGLIONE
090500941221    2C     VI2ATB        IFEQ      ' '
090600931122     C*
090700931122     C****  C. A. P.  ****
090800941221    3C     VI2CAP        IFEQ      *BLANKS
090900931122     C* 13 ON  - C.A.P. VUOTO
091000931122     C                   SETON                                        13
091100941221   X3C                   ELSE
091200931122     C* 14 ON  - C.A.P. PIENO
091300931122     C                   SETON                                        14
091400941221     C* 07 ON  - INDICA IL C.A.P. "IT000"
091500941221    4C     VI2CAP        IFEQ      CIT000
091600941221     C                   SETON                                        07
091700941221    4C                   ENDIF
091800931122     C*
091900941221     C* 08 ON  - INDICA IL C.A.P. "IT100"
092000941221    4C     VI2CAP        IFEQ      CIT100
092100931122     C                   SETON                                        08
092200941221    4C                   ENDIF
092300941221    3C                   ENDIF
092400931122     C*
092500941221    2C                   ENDIF
092600931122     C*
092700931122     C* VI2NRR = 0  ===> SI TRATTA DI RECORD DI NUOVA IMMISSIONE QUINDI
092800931122     C*                  SPENGO *IN15 PER NON PROTEGGERE CAMPI DEL SFL
092900941221    2C     VI2NRR        IFEQ      0
093000931122     C                   SETOFF                                       15
093100931122     C*
093200931122     C* PRESENTI RIGHE SENZA CAP: IMPOSSIBILE INSERIRE RIGHE CON CAP
093300941221    3C   13VI2CAP        IFNE      *BLANKS
093400931122     C                   SETON                                        5690
093500931122     C                   GOTO      ENDSFL
093600941221    3C                   ENDIF
093700941221     C* PRESENTI RIGHE CON CAP: IMPOSSIBILE INSERIRE RIGHE SENZA CAP
093800941221    3C   14VI2CAP        IFEQ      *BLANKS
093900931122     C                   SETON                                        5490
094000931122     C                   GOTO      ENDSFL
094100941221    3C                   ENDIF
094200931122     C*
094300941221    3C     VI2CAP        IFNE      *BLANKS
094400941221     C*
094500950114    4C     VI2CAP        IFNE      CIT000
094600950110     C     VI2CAP        ANDNE     CIT100
094700950110     C*
094800970718     C                   CLEAR                   DSSI95
094900970718     C                   MOVEL     '3'           I95TCN                         *TIPO CONTROLLO
095000970718     C                   MOVEL     '  '          I95NAR                         *NAZIONE
095100020218      *Devo cercare cap generico provincia con blank anzichè con 000
095200020218      *perchè sul cappario non esiste più  (es. se cap ="17000" lo passo al
095300020218      *TISI95 ="17010"  suggerimento di L.B.
095400020218     C                   movel     VI2CAP        W5                5
095500020218     C                   move      W5            W3                3
095600020218     C     W3            ifeq      '000'
095700020218     C                   MOVEL     VI2CAP        I95CAP                         *C.A.P.
095800020218     C                   move      '010    '     I95CAP
095900020218     C                   else
096000020218     C                   MOVEL     VI2CAP        I95CAP                         *C.A.P.
096100020218     C                   END
096200040206     C                   MOVEL     wuda          I95DAT
096300970718     C                   CALL      'TISI95R'
096400970718     C                   PARM                    DSSI95
096500970718     C                   MOVEL     '1'           TISI95            1
096600941221     C*
096700970718    5C     O95ERR        IFNE      *BLANKS
096800970718     C     O95CAP        ORNE      I95CAP
096900941221     C                   SETON                                        4590
097000941221     C                   GOTO      ENDSFL
097100950114    5C                   ENDIF
097200950114    4C                   ENDIF
097300941221     C*
097400970718     C                   CLEAR                   PARAM
097500941221     C                   MOVEL     VI2CAP        PARCAP                         *C.A.P.
097600931122     C                   EXSR      DECCAP
097700941221    3C                   ENDIF
097800931122     C*
097900931122     C****  SCAGLIONE  ****
098000931122     C* SCAGLIONE OBBLIGATORIO
098100950114    3C     VI2SGL        IFEQ      0
098200931122     C                   SETON                                        4690
098300931122     C                   GOTO      ENDSFL
098400950114    3C                   ENDIF
098500931122     C*
098600931122     C* CONTROLLO SE RIGHE DOPPIE
098700940929     C* MUOVO LO SCAGLIONE IN UN CAMPO DI COMODO PRIMA DI EFFETTUARE
098800940929     C*   LA CHAIN PERCHE' A VIDEO LO SCAGLIONE E' LUNGO 10 SENZA
098900940929     C*   DECIMALI MENTRE NEL FILE E' LUNGO 13 CON 3 DECIMALI
099000940929     C                   Z-ADD     VI2SGL        COMSGL
099100011029     C     KFPD          CHAIN     fifpd000                           32
099200931122     C  N32FPDNRR        IFNE      VI2NRR
099300950114    3C     FPDATB        ANDEQ     ' '
099400931122     C                   SETON                                        4790
099500931122     C                   GOTO      ENDSFL
099600950114    3C                   ENDIF
099700931122     C*
099800950114   X2C                   ELSE
099900931122     C* VI2NRR <> 0 ===> NON SI TRATTA DI RECORD DI NUOVA IMMISSIONE
100000931122     C*                  QUINDI ACCENDO *IN15 PER PROTEGGERE I CAMPI
100100931122     C                   SETON                                        15
100200950114    2C                   ENDIF
100300100112     C*controllo decimali se divisa itl  (da 3 ora portati a 5)
100400011128     c                   if        viddiv = §geditl
100500100112     c                   z-add     0             dec5              5 0
100600011128     c                   if        vi2itr > 0
100700100112     c                   move      vi2itr        dec5
100800100112     c                   if        dec5 > 0
100900011128     c                   seton                                        5990
101000011128     C                   GOTO      endsfl
101100011128     c                   endif
101200011128     c                   endif
101300100112     c                   z-add     0             dec5
101400011128     c                   if        vi2min > 0
101500100112     c                   move      vi2min        dec5
101600100112     c                   if        dec5 > 0
101700100113     c                   seton                                        589097
101800011128     C                   GOTO      endsfl
101900011128     c                   endif
102000011128     c                   endif
102100100112     c                   z-add     0             dec5
102200011128     c                   if        vi2max > 0
102300100112     c                   move      vi2max        dec5
102400100112     c                   if        dec5 > 0
102500100113     c                   seton                                        579097
102600011128     C                   GOTO      endsfl
102700011128     c                   endif
102800011128     c                   endif
102900931122     C*
103000011128     c                   endif
103100931122     C****  IMPORTO TARIFFA  ****
103200020604    2C*    *IN05         IFEQ      *OFF
103300931122     C* SI PUO' ACCETTARE L'IMPORTO TARIFFA = 0  CON IL CMD14-FORZATURA
103400020604     C*    VI2ITR        IFEQ      0
103500020604     C* NKN              SETON                                        5090
103600020604     C*  90              GOTO      ENDSFL
103700020604     C*                  ENDIF
103800950114     C*
103900020604   X2C*                  ELSE
104000100113      * non può il valore essere inferiore a 0,001
104100100210     c                   if        vi2itr < 0,001 and vi2itr <> 0
104200100113     c                   seton                                        7190
104300100113     C                   GOTO      endsfl
104400100113     c                   endif
104500931122     C* SI PUO' ACCETTARE IL TASSO % = 0  CON IL CMD14-FORZATURA
104600020604     C*    VI2TAR        IFEQ      0
104700020604     C* NKN              SETON                                        5190
104800020604     C*  90              GOTO      ENDSFL
104900020604     C*                  ENDIF
105000020604    2C*                  ENDIF
105100100113      *
105200100113      * non può il valore essere inferiore a 0,001
105300100210     c                   if        vi2min < 0,001 and vi2min <> 0
105400100113     c                   seton                                        729097
105500100113     C                   GOTO      endsfl
105600100113     c                   endif
105700100113      *
105800100113      * non può il valore essere inferiore a 0,001
105900100210     c                   if        vi2max < 0,001 and vi2max <> 0
106000100113     c                   seton                                        739097
106100100113     C                   GOTO      endsfl
106200100113     c                   endif
106300931122     C*
106400931122     C****  IMPORTO MASSIMO TARIFFA  ****
106500931122     C* SE INSERITO DEVE ESSERE MAGGIORE O UGUALE AL MINIMO
106600950114    2C     VI2MAX        IFGT      0
106700940919     C     VI2MAX        ANDLT     VI2MIN
106800100113     C                   SETON                                        529097
106900931122     C                   GOTO      ENDSFL
107000950114    2C                   ENDIF
107100931122     C*
107200931122     C* SE NON CI SONO ERRORI AGGIORNO E VADO AVANTI A LEGGERE
107300931122     C                   MOVEL     VI2CAP        DUPCAP
107400940919     C                   Z-ADD     VI2SGL        DUPSGL
107500940919     C  N05              Z-ADD     VI2ITR        DUPITR
107600931122     C   05              Z-ADD     VI2TAR        DUPTAR
107700940919     C                   Z-ADD     VI2MIN        DUPMIN
107800940919     C                   Z-ADD     VI2MAX        DUPMAX
107900931122     C*
108000950114   X1C                   ELSE
108100950117     C                   SETOFF                                       15
108200950114    1C                   ENDIF
108300931122     C*
108400931122     C     ENDSFL        TAG
108500931122     C   90              Z-ADD     NR2           REC2
108600931122     C*
108700950117     C* 25 ON - SE ESISTONO IMPORTO MINIMO O IMPORTO MASSIMO VISUALIZZO
108800950117     C*   LA LETTERA "M" SULLA RELATIVA RIGA DI DETTAGLIO
108900950117     C                   SETOFF                                       25
109000950117    1C     VI2MIN        IFGT      0
109100950117     C     VI2MAX        ORGT      0
109200950117     C                   SETON                                        25
109300950117    1C                   ENDIF
109400950117     C*
109500020128     C                   UPDATE    cn11S06
109600931122     C   90              SETON                                        33
109700931122     C*
109800931122     C                   ADD       1             NR2
109900020128     C  N90NR2           CHAIN     cn11S06                            33
110000950114    0C                   ENDDO
110100931122     C*
110200950114     C*
110300931122     C* SOLO SE NON SONO PRESENTI ERRORI
110400950114    0C     *IN90         IFEQ      *OFF
110500931122     C*
110600931122     C* SE INSERITO C.A.P."IT000" OCCORRE INSERIRE ANCHE C.A.P."IT100"
110700931122     C   07
110800931122     CANN08              SETON                                        4890
110900931122     C* SE INSERITO C.A.P."IT100" OCCORRE INSERIRE ANCHE C.A.P."IT000"
111000931122     C  N07
111100931122     CAN 08              SETON                                        4990
111200931122     C* NON E' STATA INSERITA NEMMENO UNA RIGA DI DETTAGLIO
111300931122     C  N13
111400931122     CANN14              SETON                                        5390
111500931122     C*
111600950114    0C                   ENDIF
111700931122     C*
111800931122     C                   SETOFF                                       17
111900931122     C                   ENDSR
112000931122     C*
112100931006     C*--- AGGIORNO CAMPI DEL SFL ------------------------------------*
112200931006     C     REGIS         BEGSR
112300020128     C*------------------------------------------------
112400931006     C                   Z-ADD     1             NR2
112500931006     C*
112600020128     C     NR2           CHAIN     cn11S06                            31
112700931006     C     *IN31         DOWEQ     *OFF
112800931006     C*
112900931014     C     COMREC        IFNE      *ZEROS
113000940929     C     COMREC        ANDNE     *BLANKS
113100931014     C*
113200931006     C* 60 ON  - SONO STATE EFFETTUATE MODIFICHE SU TFFGD00F
113300931006     C                   SETOFF                                       60
113400931006     C*
113500931006     C* VI2NRR = 0  ===> SI TRATTA DI RECORD DI NUOVA IMMISSIONE QUINDI
113600931006     C*                  ACCENDO *IN60 PER REGISTRARE IL RECORD
113700931006     C     VI2NRR        IFEQ      0
113800931006     C                   SETON                                        60
113900931006     C*
114000931006     C                   ELSE
114100931006     C     VI2ATB        IFEQ      ' '
114200931007     C     CO2REC        IFNE      SAVREC
114300931006     C                   SETON                                        60
114400931006     C                   ENDIF
114500931006     C*
114600931006     C                   ELSE
114700931006     C                   SETON                                        60
114800931006     C                   ENDIF
114900931006     C                   ENDIF
115000931006     C*
115100940929     C* MUOVO LO SCAGLIONE IN UN CAMPO DI COMODO PRIMA DI EFFETTUARE
115200940929     C*   LA CHAIN PERCHE' A VIDEO LO SCAGLIONE E' LUNGO 10 SENZA
115300940929     C*   DECIMALI MENTRE NEL FILE E' LUNGO 13 CON 3 DECIMALI
115400940929     C                   Z-ADD     VI2SGL        COMSGL
115500940929     C*
115600011029     C   60KFPD          CHAIN     fifpd000                           33
115700931006     C   60VI2ATB        IFNE      ' '
115800931006     C*
115900931006     C     *IN33         IFEQ      *OFF
116000931006     C                   MOVEL     'A'           FPDATB
116100091210     C**********         MOVEL     'T'           FPDFTR
116200091210     C**********         MOVEL     *ALL'9'       FPDDTR
116300011029     C                   UPDATE    fifpd000
116400931006     C*
116500931006     C* RIEMPIO IL FLAG IN MODO CHE SE ANNULLANO DELLE RIGHE DI
116600011029     C*   DETTAGLIO (fifpd00F), AGGIORNO LA DATA ULTIMA VARIAZIONE
116700011029     C*   PRESENTE NEL FILE fifpt00F
116800931006     C                   MOVEL     'A'           FLGDUV
116900931006     C                   ENDIF
117000931006     C*
117100931006     C                   ELSE
117200931006     C* 33 ON  - IMMISSIONE
117300931006     C* 33 OFF - VARIAZIONE
117400020110     C   33              clear                   fifpd000
117500020110     c
117600940919     C  N05              Z-ADD     VI2ITR        FPDITR
117700940919     C   05              Z-ADD     VI2TAR        FPDITR
117800940919     C                   Z-ADD     VI2MIN        FPDMIN
117900940919     C                   Z-ADD     VI2MAX        FPDMAX
118000931006     C*
118100931006     C                   MOVEL     VI2ATB        FPDATB
118200091210     C**********         MOVEL     'T'           FPDFTR
118300091210     C**********         MOVEL     *ALL'9'       FPDDTR
118400931006     C* SOLO PER IMMISSIONE
118500931006     C     *IN33         IFEQ      *ON
118600931006     C                   Z-ADD     COMPDR        FPDPDR
118700931014     C                   MOVEL     COMSML        FPDSML
118800931014     C                   MOVEL     VIDTSR        FPDTSR
118900931006     C                   Z-ADD     VIDCTR        FPDCTR
119000931006     C                   Z-ADD     VIDPRG        FPDPRG
119100020128     C                   MOVEL     vidctd        FPDctd
119200931006     C                   MOVEL     VI2CAP        FPDCAP
119300940919     C                   Z-ADD     VI2SGL        FPDSGL
119400931006     C                   ENDIF
119500931006     C*
119600011029     C  N33              UPDATE    fifpd000
119700011029     C   33              WRITE     fifpd000
119800931006     C*
119900931006     C* RIEMPIO IL FLAG IN MODO CHE SE INSERISCONO O VARIANO DELLE
120000011029     C*   RIGHE DI DETTAGLIO (fifpd00F), AGGIORNO LA DATA ULTIMA
120100011029     C*   VARIAZIONE PRESENTE NEL FILE fifpt00F
120200931006     C                   MOVEL     'A'           FLGDUV
120300931006     C*
120400931006     C                   ENDIF
120500931014     C                   ENDIF
120600931006     C*
120700931006     C                   ADD       1             NR2
120800020128     C     NR2           CHAIN     cn11S06                            31
120900931006     C                   ENDDO
121000931006     C                   ENDSR
121100931006     C*
121200931005     C*--- AGGIORNO CAMPI DEL SFLCTL ---------------------------------*
121300931005     C     REGIS2        BEGSR
121400020128     C*------------------------------------------------
121500931005     C*
121600011029     C     KFPT          CHAIN     fifpt000                           32
121700931005     C* 32 ON  - IMMISSIONE
121800931005     C* 32 OFF - VARIAZIONE
121900020110     C   32              clear                   fifpt000
122000931005     C                   MOVEL     ' '           FPTATB
122100931005     C                   MOVEL     VI2TPG        FPTTPG
122200020212     C                   MOVEL     Vi2tsm        FPTTsm
122300020214     C                   MOVEL     vi2tat        FPTTat
122400020212     C                   MOVEL     Vi2tpp        FPTTpp
122500020129     C                   z-add     VIdata        FPTata
122600940919     C                   Z-ADD     VI2ARL        FPTARL
122700940919     C                   Z-ADD     VI2ARF        FPTARF
122800020201     C                   Z-ADD     VI2ARO        FPTARO
122900931005     C                   Z-ADD     VI2RPV        FPTRPV
123000020129     C                   MOVEL     VIDTSR        FPTTSR
123100020214     c   28              movel     dsbonus       fptflr
123200020214     c  n28              clear                   fptflr
123300931005     C* SOLO PER IMMISSIONE
123400931005     C     *IN32         IFEQ      *ON
123500931005     C                   Z-ADD     COMPDR        FPTPDR
123600931014     C                   MOVEL     COMSML        FPTSML
123700931005     C                   Z-ADD     VIDCTR        FPTCTR
123800931005     C                   Z-ADD     VIDPRG        FPTPRG
123900020128     C                   MOVEL     vidctd        FPTctd
124000931005     C                   ENDIF
124100931005     C*
124200011029     C* SE VIENE VARIATO fifpt ABBLENCO FLAG E DATA TRASMISSIONE
124300931005     C     COMFPT        IFNE      FPTDS
124400091210     C**********         MOVEL     'T'           FPTFTR
124500091210     C**********         MOVEL     *ALL'9'       FPTDTR
124600931005     C*
124700011029     C* RIEMPIO IL FLAG IN MODO CHE SE MI VARIANO fifpt AGGIORNO
124800940919     C*   IL CAMPO DATA ULTIMA VARIAZIONE (PRESENTE NEL TNTA80R)
124900931005     C                   MOVEL     'A'           FLGDUV
125000931005     C                   ENDIF
125100950309     C*
125200011029     C  N32              UPDATE    fifpt000
125300011029     C   32              WRITE     fifpt000
125400931005     C*
125500931006     C                   SETON                                        0203
125600931005     C                   SETOFF                                       01
125700931005     C*
125800931005     C                   MOVEL     VI2TPG        SAVTPG            1
125900931005     C                   ENDSR
126000931005     C*
126100931006     C*--- DECODIFICA  C.A.P. ----------------------------------------*
126200931006     C     DECCAP        BEGSR
126300020128     C*------------------------------------------------
126400931116     C*
126500931116     C                   Z-ADD     3             PARNDE                         *NUMERO DECODIFICHE
126600040206     C                   Z-ADD     wuda          PARDRI
126700931116     C*
126800931116     C                   CALL      'TRUL05R'
126900931116     C                   PARM                    PARAM
127000931116     C*
127100931116     C                   MOVEL     PARDEC        DE2CAP
127200931116     C                   MOVEL     '1'           TRUL05            1
127300931116     C*
127400931006     C                   ENDSR
