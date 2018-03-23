000100000000     H*------------------------------------------------------------------------*
000200000000     H*                LANCIO AUTOMATICO 1° BLOCCO STATISTICHE                 *
000300980403     H* NOTE: Non si può lanciare in prova perchè forza il s.informativo       *
000400000000     H*------------------------------------------------------------------------*
000500000000     H DECEDIT('0,') DATEDIT(*DMY.)
000600000000     F*------------------------------------------------------------------------*
000700000000     F* DATA BASE
000800000000     F*------------------------------------------------------------------------*
000900020118     FTISE01D   CF   E             WORKSTN INFDS(DEVDS1)
001000020118     F                                     SFILE(SE01DVM:MSGULT)
001100980902     FTABEL00F  IF   E           K DISK
001200061123     Ftntbe01l  IF   E           K DISK    usropn
001300160223     FFNTNA03L  IF   E           K DISK
001400000000     D*------------------------------------------------------------------------*
001500000000     D* SCHIERE
001600000000     D*------------------------------------------------------------------------*
001700000000     D*-------------------
001800000000     D* MESSAGGI E FUNZIONI
001900000000     D*-------------------
002000000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
002100000000     D FUN             S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO
002200061123     D
002300061123     d cmdt            s             60    dim(01) ctdata perrcd(1)
002400000000     D*-------------------
002500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
002600000000     D*-------------------
002700000000     D FFU             S              6    DIM(31)                              *FUNZIONE
002800000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
002900000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
003000000000     D*-------------------
003100000000     D* TABELLA DESCRIZIONI A VIDEO
003200000000     D*-------------------
003300000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
003400000000     D*-------------------
003500000000     D* TABELLA TASTI FUNZIONALI PREMUTI
003600000000     D*-------------------
003700000000     D KEYS            S              1    DIM(40)
003800000000     D*------------------------------------------------------------------------*
003900000000     D* INPUT
004000000000     D*------------------------------------------------------------------------*
004100000000     D*-------------------
004200000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
004300000000     D*-------------------
004400000000     D DSFUN         E DS
004500000000     D  FUNRIG                 1     40
004600000000     D*-------------------
004700000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
004800000000     D*-------------------
004900000000     D DSMSG         E DS
005000991027     D*-------------------
005100991027     D* PARAMETRI DEFAULT DA TABELLA "TP" X PESI
005200991027     D*-------------------
005300991027     D DSTP          E DS
005400130130      *-------------------
005500130130      * Ds date statistiche
005600130130     d dSdf          e ds
005700130130      *-------------------
005800130130      * DS Dati vari per Statistiche -DI GRUPPO-
005900130130     d ds5S          e ds                  inz
006000000000     D*-------------------
006100000000     D* DS PER GESTIONE TASTI FUNZIONALI
006200000000     D*-------------------
006300000000     D DSKEYS        E DS
006400000000     D  KEYRIG                 1     40
006500000000     D*-------------------
006600000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
006700000000     D*-------------------
006800000000     D                SDS
006900000000     D  USER                 254    263
007000000000     D*-------------------
007100000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
007200000000     D*-------------------
007300000000     D DEVDS1          DS
007400000000     D  KEY                  369    369
007500000000     D  CURSOR               370    371
007600000000     D*-------------------
007700000000     D* DS "XSRDA8"
007800000000     D*-------------------
007900000000     D WLBDA8          DS                  INZ
008000000000     D  G08DAT                 1      8  0
008100000000     D  G08INV                 9     16  0
008200000000     D  G08ERR                17     17
008300000000     D  G08TGI                18     22  0
008400980902     D*-------------------
008500980902     D* DS DATA GENERICA (8)
008600980902     D*-------------------
008700980902     D                 DS                  INZ
008800980902     D  DSANN                  1      4  0
008900980902     D  DSMES                  5      6  0
009000980902     D  DSGIO                  7      8  0
009100980902     D  DSDAT                  1      8  0
009200000208     D  DSAAM                  1      6  0
009300021202     D*-------------------
009400021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
009500021202     D*-------------------
009600160308     D TRULC7DS      E DS
009700000000     D*-------------------
009800000000     D* ARCHITETTURA
009900000000     D*-------------------
010000000000     D KPJBA         E DS
010100980403     D  SIFPRV                98     98
010200000000     D*-------------------
010300020916     D* DS REPERIMENTO DATI UTENTE
010400000000     D*-------------------
010500020916     D TIBS34DS      E DS                                                       *Profili utente
010600020916     D DDATIUTE      E DS                                                       *Dati utente
010700020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
010800000000     D*-------------------
010900000000     D* DS  V O L U M I
011000000000     D*-------------------
011100991227     D PARVOL          DS                  INZ
011200991223     D  VOLTLA                 1      1
011300991223     D  VOLANN                 2      5  0
011400991223     D  VOLMMI                 6      7  0
011500991223     D  VOLMMF                 8      9  0
011600991223     D  VOLSNR                10     10
011700991223     D  VOLPID                27     31  2
011800991223     D  VOLDIV                32     32
011900991223     D  VOLARE                33     35  0
012000991223     D  VOLFIL                36     38  0
012100991223     D  VOLDET                39     39
012200991223     D  VOLTOT                40     40
012300991223     D  VOLRG1                41     44  1
012400991223     D  VOLRG2                45     48  1
012500991223     D  VOLPIA                49     53  2
012600991223     D  VOLPSD                54     58  2
012700991223     D  VOLPSA                59     63  2
012800991223     D  VOLREA                64     64
012900991223     D  VOLOK                 65     66
013000991230     D  VOLMIN                67     71  3
013100000706     D  VOLPOS                72     72
013200030911     D  VOLNT1                73     75    inz(*blanks)
013300030911     D  VOLNT2                76     78    inz(*blanks)
013400030911     D  VOLNT3                79     81    inz(*blanks)
013500030911     D  VOLNT4                82     84    inz(*blanks)
013600050124     D  VOLSAL                85     85
013700991027     D*-------------------
013800991027     D* DS  P E S I
013900991027     D*-------------------
014000991027     D PARPES          DS                  INZ
014100991027     D  PESTLA                 1      1
014200991027     D  PESANN                 2      5  0
014300991027     D  PESMMI                 6      7  0
014400991027     D  PESMMF                 8      9  0
014500991027     D  PESSNR                10     10
014600991027     D  PESPEI                27     31  2
014700991027     D  PESDIV                32     32
014800991027     D  PESARE                33     35  0
014900991027     D  PESFIL                36     38  0
015000991027     D  PESDET                39     39
015100991027     D  PESTOT                40     40
015200991027     D  PESRG1                41     44  1
015300991027     D  PESRG2                45     48  1
015400991027     D  PESTAR                49     53  3
015500991027     D  PESPES                54     58  2
015600991230     D  PESMIN                59     63  3
015700000706     D  PESPOS                64     64
015800011112     D  PESNEG                65     65
015900050124     D  PESSAL                66     66
016000000000     D*-------------------
016100000000     D* DS  A F F I D A B I L I T A'
016200000000     D*-------------------
016300000000     D PARAFF          DS                  INZ
016400000000     D  AFFANN                 1      4  0
016500000000     D  AFFMMI                 5      6  0
016600000000     D  AFFMMF                 7      8  0
016700000000     D  AFFSNC                 9      9
016800000000     D  AFFSDC                10     10
016900000000     D  AFFSNA                11     11
017000000000     D  AFFSDA                12     12
017100000000     D  AFFSNR                13     13
017200000000     D  AFFDIV                14     14
017300000000     D  AFFARE                15     17  0
017400000000     D  AFFFLP                18     20  0
017500000000     D  AFFTFP                21     23  0
017600000000     D  AFFFLA                24     26  0
017700000000     D  AFFTFA                27     29  0
017800000000     D  AFFDTI                30     37  0
017900000000     D  AFFDTF                38     45  0
018000000000     D  AFFTLA                46     46
018100000000     D  AFFSNP                47     47
018200000000     D  AFFTOT                48     48
018300000000     D  AFFIOE                49     49
018400970827     D  AFFCFP                50     50
018500040108     D*-------------------
018600040108     D* DS  A F F I D A B I L I T A'   X   D A T A   R I T I R O
018700040108     D*-------------------
018800040108     D PARAFR          DS                  INZ
018900040108     D  AFRANN                 1      4  0
019000040108     D  AFRMMI                 5      6  0
019100040108     D  AFRMMF                 7      8  0
019200040108     D  AFRSNC                 9      9
019300040108     D  AFRSDC                10     10
019400040108     D  AFRSNA                11     11
019500040108     D  AFRSDA                12     12
019600040108     D  AFRSNR                13     13
019700040108     D  AFRDIV                14     14
019800040108     D  AFRARE                15     17  0
019900040108     D  AFRFLP                18     20  0
020000040108     D  AFRTFP                21     23  0
020100040108     D  AFRFLA                24     26  0
020200040108     D  AFRTFA                27     29  0
020300040108     D  AFRDTI                30     37  0
020400040108     D  AFRDTF                38     45  0
020500040108     D  AFRTLA                46     46
020600040108     D  AFRSNP                47     47
020700040108     D  AFRTOT                48     48
020800040108     D  AFRIOE                49     49
020900040108     D  AFRCFP                50     50
021000970825     D*-------------------
021100000000     D* DS  T R A I N I
021200000000     D*-------------------
021300000000     D PARTRA          DS                  INZ
021400990609     D  TRADAT                 1      6  0
021500990609     D  TRAAAI                 1      4  0
021600990609     D  TRAMMI                 5      6  0
021700990609     D  TRADA2                 7     12  0
021800990609     D  TRAAAF                 7     10  0
021900990609     D  TRAMMF                11     12  0
022000990609     D  TRAMPA                13     13
022100990609     D  TRAFPC                14     14
022200990609     D  TRACRE                15     15
022300000706     D  TRAPOS                16     16
022400000000     D*-------------------
022500000000     D* DS  T R A N S I T I
022600000000     D*-------------------
022700000000     D PARTRS          DS                  INZ
022800990609     D  TRSANI                 1      4  0
022900990609     D  TRSMMI                 5      6  0
023000990609     D  TRSANF                 7     10  0
023100990609     D  TRSMMF                11     12  0
023200131011     D*-------------------
023300131011     D* DS  CLIENTI FATT.ESTERO
023400131011     D*-------------------
023500131011     D PARCEX          DS                  INZ
023600131011     D   cexPGM                1     10
023700131011     D   cexFIL               11     20
023800131011     D   cexPTF               21     30
023900131011     D   cexAWF               31     40
024000131011     D   cexLST               41     41
024100131011     D   cexNTW               42     51
024200131011     D   cexDAN               52     55  0
024300131011     D   cexDME               56     57  0
024400131011     D   cexANN               58     61  0
024500131011     D   cexMES               62     63  0
024600000000     D*-------------------
024700000000     D* DS  R I T A R D I  TRAINI
024800000000     D*-------------------
024900000000     D PARRIT          DS                  INZ
025000990609     D  RITDAT                 1      6  0
025100990609     D  RITAAI                 1      4  0
025200990609     D  RITMMI                 5      6  0
025300990609     D  RITDA2                 7     12  0
025400990609     D  RITAAF                 7     10  0
025500990609     D  RITMMF                11     12  0
025600990609     D  RITCRE                13     13
025700000710     D  RITPOS                14     14
025800000710     D  RITTAD                15     15
025900000000     D*-------------------
026000000000     D* DS  M O V I M E N T O
026100000000     D*-------------------
026200000000     D PARMOV          DS                  INZ
026300990609     D  MOVANI                 1      4  0
026400990609     D  MOVMMI                 5      6  0
026500990609     D  MOVANF                 7     10  0
026600990609     D  MOVMMF                11     12  0
026700990609     D  MOVMPA                13     13
026800990609     D  MOVFIL                14     16  0
026900990609     D  MOVARE                17     19  0
027000990609     D  MOVDIV                20     20
027100990609     D  MOVSER                21     21
027200990609     D  MOVPOR                22     22
027300990609     D  MOVSPE                23     23
027400990609     D  MOVPRO                24     24
027500990609     D  MOVDET                25     25
027600990609     D  MOVFPC                26     26
027700990609     D  MOVIOE                27     27
027800000703     D  MOVPOS                28     28
027900000000     D*-------------------
028000000000     D* DS  AGGIORNAMENTO SALDI DA CONTABILITA'
028100000000     D*-------------------
028200000000     D PARAGG          DS                  INZ
028300990609     D  AGGAAI                 1      4  0
028400990609     D  AGGMMI                 5      6  0
028500990609     D  AGGAAF                 7     10  0
028600990609     D  AGGMMF                11     12  0
028700990609     D  AGGAGG                13     13
028800000000     D*-------------------
028900000000     D* DS  D E L T A
029000000000     D*-------------------
029100990609     D PARDEL          DS                  INZ
029200990609     D  DELDAT                 1      6  0
029300990609     D  DELAAI                 1      4  0
029400990609     D  DELMMI                 5      6  0
029500990609     D  DELDA2                 7     12  0
029600990609     D  DELAAF                 7     10  0
029700990609     D  DELMMF                11     12  0
029800990609     D  DELDCK                13     13
029900990609     D  DELDCC                14     14
030000990609     D  DELDCF                15     15
030100990609     D  DELFIC                16     18  0
030200990609     D  DELARC                19     21  0
030300990609     D  DELDIC                22     22
030400990609     D  DELDFF                23     23
030500990609     D  DELCOF                24     30  0
030600990609     D  DELDIF                31     31
030700990609     D  DELSTA                32     32
030800000825     D  DELSCE                33     33
030900990609     D  DELTSP                34     34
031000990609     D  DELIOE                35     35
031100990609     D  DELCLV                36     36
031200990609     D  DELDPT                37     42  0
031300990609     D  DELAPI                37     40  0
031400990609     D  DELMPI                41     42  0
031500990609     D  DELDP2                43     48  0
031600990609     D  DELAFP                43     46  0
031700990609     D  DELMFP                47     48  0
031800990609     D  DELSPD                49     52  1
031900990609     D  DELPDD                53     56  1
032000990609     D  DELPDA                57     60  1
032100000707     D  DELPOS                61     61
032200000825     D  DELCRE                62     62
032300010618     D  DELCL1                63     63
032400010618     D  DELCL2                64     64
032500010618     D  DELSIN                65     65
032600020409     D  DELCL3                66     66
032700020409     D  DELCL4                67     67
032800020409     D  DELNTW                68     68
032900020702     D  DELDFD                69     72  1
033000020702     D  DELDFA                73     76  1
033100020702     D  DELDFDP               77     80  1
033200020702     D  DELDFAP               81     84  1
033300030311     D  DELFPC                85     85
033400030912     D  DELWPC                86     86
033500030925     D  DELWCP                87     87
033600090824     D  DELWfdpv              88     88
033700090824      * Stat. Clienti Persi (Trimestrale x il momento)                          lancio trimestre(T)
033800090824     D  DELCLPtmp             89     89                                         M/B/T/Q/S
033900130130     D*-------------------
034000130130     D* DS  D E L T A   PER FASCE DI PESO
034100130130     D*-------------------
034200130208     D ParDFP          DS                  inz
034300130208     D  DFPann                        4s 0 inz
034400130208     D  DFPmes                        2s 0 inz
034500120329     D*-------------------
034600120329     D* DS  CREAZIONE ANAGRAFICA CLIENTI COLLEGATI A POTENZILE
034700120329     D*-------------------
034800120329     D PARACP          DS                  INZ
034900120329     D  ACPANN                        4s 0
035000120329     D  ACPMES                        2s 0
035100980428     D*-------------------
035200980428     D* DS  FATTURAZIONE INFRAGRUPPO  M C U B I
035300980428     D*-------------------
035400980902     D PARFIM          DS                  INZ
035500990420     D  FIMANN                 1      4  0
035600990420     D  FIMMES                 5      6  0
035700990420     D  FIMSIM                 7      7
035800980831     D*-------------------
035900980831     D* DS  R E P O R T  COMMERCIALE
036000980831     D*-------------------
036100980902     D PARREP          DS                  INZ
036200980831     D  REPAMI                 1      6  0
036300980831     D  REPAMF                 7     12  0
036400980902     D*-------------------
036500980902     D* DS  C.A.T. CREAZIONE DATI TIPO
036600980902     D*-------------------
036700980902     D PARCAT          DS                  INZ
036800980902     D  CATKSC                 1      7  0
036900980902     D  CATFPA                 8     10  0
037000980902     D  CATFAR                11     13  0
037100980902     D  CATFAP                14     16  0
037200980902     D  CATDS1                17     24  0
037300980902     D  CATDS2                25     32  0
037400980902     D  CATTAR                33     35
037500980902     D  CATCTO                36     36
037600980902     D  CATKOV                37     43  0
037700980902     D  CATCTR                44     46  0
037800980902     D  CATPRG                47     49  0
037900980902     D  CATLNP                50     52  0
038000980902     D  CATFIE                53     53
038100980902     D  CATSIM                54     54
038200980902     D  CATELA                55     55
038300011012     D  CATDIV                56     58
038400041206     D  catUNI                59     59
038500041206     D  catsta                60     60
038600041206     D  catint                61     61
038700041206     D  catsie                62     62
038800000208     D*-------------------
038900000208     D* DS  DELTA SPEDIZIONI > 10 Q.LI
039000000208     D*-------------------
039100000208     D PARDSP          DS                  INZ
039200000208     D  DSPDAI                 1      6  0
039300000208     D  DSPDAF                 7     12  0
039400000706     D  DSPPOS                13     13
039500060124     D*------------------
039600060124     D* DS X PASSAGGIO PARAMETRI Crea wrk-file VARIE fattura
039700060124     D*------------------
039800060124     D PARric          DS
039900060124     D  MMAAAAFAT              1      6
040000060124     D  MMAAAAFATNUM           1      6  0
040100060124     D  MMFAT                  1      2  0
040200060124     D  AAAAFAT                3      6  0
040300060124     D  DATAFATFINMES          7     14  0
040400060124     D  GGFATFINMES            7      8  0
040500060124     D  MMFATFINMES            9     10  0
040600060124     D  AAAAFATFINMES         11     14  0
040700060124     D  DETTAGLIO             15     15
040800060124     D  SOLOVARIA             16     16
040900060124     D  PERIODOCEAAAA         17     20  0
041000060124     D  PERIODOCEMM           21     22  0
041100060124     D  PERIODOCE             17     22  0
041200060124     D  STORNONACC            23     23
041300060124     D  ELABORA_CE            24     24
041400121127      *-------------------
041500121127      * DS  STATO APPLICAZIONI ISTAT
041600121127      *-------------------
041700121127     d TNTA79ds        ds                  inz
041800121127     d  SAIdip                        6  0 inz(*loval)
041900121127     d  SAIdfp                        6  0 inz(*hival)
042000121127     d  SAIdru                        8  0 inz(*hival)
042100121211     d  SAIdvt                        8  0 inz
042200121128     d  SAI10f                        1    inz('S')
042300130403
042400130403      *-------------------
042500130403      * DS  STATO AVANZAMENTO ADESIONE FUEL
042600130403      *-------------------
042700130403     d TNTA77ds        ds                  inz
042800130403     d  SFSdip                        6  0 inz(*loval)
042900130403     d  SFSdfp                        6  0 inz(*hival)
043000130403     d  SFSdru                        8  0 inz(*hival)
043100130403     d  SFSdvt                        8  0 inz
043200130403     d  SFS10f                        1    inz('S')
043300130403
043400000000     D***
043500000000     D* DATA INIZIALE PRECEDENTE
043600000000     D***
043700000000     D                 DS
043800990609     D  ANPAAI                 1      4  0
043900990609     D  ANPMMI                 5      6  0
044000990609     D  ANPDAT                 1      6  0
044100000000     D***
044200000000     D* DATA FINALE PRECEDENTE
044300000000     D***
044400000000     D                 DS
044500990609     D  ANPAAF                 1      4  0
044600990609     D  ANPMMF                 5      6  0
044700990609     D  ANPDA2                 1      6  0
044800021203     D***
044900021203     D* COSTANTI
045000021203     D***
045100021203     D LIB201          C                   CONST('GAITRA201 ')
045200121127     D***
045300121127     D* VARIABILI
045400121127     D***
045500060125     d dtaeur          s               d   datfmt(*eur)
045600130130     d wDateI          s               d   inz  datfmt(*iso)
045700130130     d wDateF          s               d   inz  datfmt(*iso)
045800121127     d wDate_Iso       s               d   inz  datfmt(*iso)
045900121127     d wDate           s              8  0 inz
046000160223     d DATAfineM       s               d   inz  datfmt(*iso)
046100121127      *
046200061123     d wlib            s             10
046300061123     d comman          s             80
046400061123     d lenght          s             15  5 inz(80)
046500061123     d kTbeCod         s                   Like(TbeCod)
046600061123     d kTbeKe1         s                   Like(TbeKe1)
046700121127
046800000000     C*------------------------------------------------------------------------*
046900000000     C* MAIN LINE
047000000000     C*------------------------------------------------------------------------*
047100000000     C*
047200000000     C* CICLO FINO A CHE:
047300000000DO  1C     $FINE         DOUEQ     '1'
047400000000     C*
047500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
047600000000     C*
047700000000     C* CONTROLLA GESTIONE VIDEO
047800000000IF  2C     $GES          IFEQ      'V1'
047900000000     C                   EXSR      GESV1                                        *PARAMETRI
048000000000E   2C                   ENDIF
048100000000     C*
048200000000E   1C                   ENDDO
048300000000     C*
048400000000     C* FINE MAIN DEL PGM
048500021203     C                   MOVEL     WKNSIF        KNSIF
048600000000     C                   SETON                                        LR
048700000000     C*------------------------------------------------------------------------*
048800000000     C* GESV1 - GESTIONE VIDEO UNO
048900000000     C*------------------------------------------------------------------------*
049000000000     C     GESV1         BEGSR
049100000000     C*
049200000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
049300000000     C                   EXSR      CARV1                                        *CARICA VIDEO
049400000000E   1C                   ENDIF
049500000000     C*
049600000000IF  1C     MSGULT        IFEQ      *ZEROS                                       *SE CI SONO MESSAGGG
049700000000     C                   MOVEL     *BLANKS       $MSG                           *EMETTE BLANK
049800000000     C                   EXSR      CARMSG                                       *CARICA MESSAGGIO
049900000000E   1C                   ENDIF
050000000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
050100000000     C*
050200020118     C                   EXFMT     SE01DV1                                      *PARAMETRI
050300000000     C*
050400000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
050500000000     C*
050600000000IF  1C     $ERR          IFEQ      '0'                                          *TASTO ABILITATO
050700000000SL  2C                   SELECT
050800000000     C     KEY           WHENEQ    ENTER
050900000000     C                   EXSR      ENTV1                                        *GEST.ENTER VIDEO V1
051000000000     C     KEY           WHENEQ    F6
051100000000     C                   EXSR      F06GES                                       *GEST.F6 (LANCIO)
051200000000     C                   OTHER
051300000000     C                   EXSR      FUNSTD                                       *GEST.TASTI STANDARD
051400000000E   2C                   ENDSL
051500000000E   1C                   ENDIF
051600000000     C*
051700000000     C                   ENDSR
051800000000     C*------------------------------------------------------------------------*
051900000000     C* CARV1 - CARICA VIDEO UNO
052000000000     C*------------------------------------------------------------------------*
052100000000     C     CARV1         BEGSR
052200000000     C*
052300000000     C                   MOVEL     'V1'          $GES                           *GESTIONE ATTUALE
052400000000     C*
052500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
052600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
052700000000     C*
052800020118     C                   WRITE     SE01DVT                                      *TESTATA
052900020118     C                   WRITE     SE01DVF                                      *FUNZIONI
053000000000     C*
053100000000     C                   ENDSR
053200000000     C*------------------------------------------------------------------------*
053300000000     C* ENTV1 - GESTIONE TASTO ENTER VIDEO UNO
053400000000     C*------------------------------------------------------------------------*
053500000000     C     ENTV1         BEGSR
053600000000     C*
053700000000     C* CONTROLLA I CAMPI IMMESSI
053800000000     C                   EXSR      DECODI
053900000000     C*
054000000000     C                   ENDSR
054100000000     C*------------------------------------------------------------------------*
054200000000     C* F06GES - GEST. TASTO FUNZ. F06 ==> CONFERMA LANCIO
054300000000     C*------------------------------------------------------------------------*
054400000000     C     F06GES        BEGSR
054500000000     C*
054600000000     C* CONTROLLA I CAMPI IMMESSI
054700000000     C                   EXSR      DECODI                                       *CONTROLLO CAMPI
054800000000     C*
054900000000     C* NON CI SONO ERRORI
055000000000IF  1C     $ERR          IFEQ      '0'
055100000000     C***
055200000000     C* LANCIA LE STATISTICHE SELEZIONATE
055300000000     C***
055400000000     C*--------------------
055500000000     C* V O L U M I
055600000000     C*--------------------
055700980403     C                   MOVEL     LIB201        KNSIF
055800000000IF  2C     V1CVOL        IFEQ      '1'
055900000000     C                   EXSR      LANVOL
056000000000E   2C                   ENDIF
056100991027     C*--------------------
056200991027     C* P E S I
056300991027     C*--------------------
056400991027     C                   MOVEL     LIB201        KNSIF
056500991027IF  2C     V1CPES        IFEQ      '1'
056600991027     C                   EXSR      LANPES
056700991027E   2C                   ENDIF
056800000000     C*--------------------
056900000000     C* A F F I D A B I L I T A'
057000000000     C*--------------------
057100980403     C                   MOVEL     LIB201        KNSIF
057200000000IF  2C     V1CAFF        IFEQ      '1'
057300000000     C                   EXSR      LANAFF
057400000000E   2C                   ENDIF
057500040108     C*--------------------
057600040108     C* A F F I D A B I L I T A'   X   D A T A   R I T I R O
057700040108     C*--------------------
057800160509     C**                 MOVEL     LIB201        KNSIF
057900160509IF  2C**   V1CAFR        IFEQ      '1'
058000160509     C**                 EXSR      LANAFR
058100160509E   2C**                 ENDIF
058200000000     C*--------------------
058300000000     C* T R A I N I
058400000000     C*--------------------
058500980403     C                   MOVEL     LIB201        KNSIF
058600000000IF  2C     V1CTRA        IFEQ      '1'
058700000000     C                   EXSR      LANTRA
058800000000E   2C                   ENDIF
058900000000     C*--------------------
059000000000     C* T R A N S I T I
059100000000     C*--------------------
059200980403     C                   MOVEL     LIB201        KNSIF
059300000000IF  2C     V1CTRS        IFEQ      '1'
059400000000     C                   EXSR      LANTRS
059500000000E   2C                   ENDIF
059600000000     C*--------------------
059700000000     C* R I T A R D I  TRAINI
059800000000     C*--------------------
059900980403     C                   MOVEL     LIB201        KNSIF
060000000000IF  2C     V1CRIT        IFEQ      '1'
060100000000     C                   EXSR      LANRIT
060200000000E   2C                   ENDIF
060300000000     C*--------------------
060400000000     C* M O V I M E N T O
060500000000     C*--------------------
060600980403     C                   MOVEL     LIB201        KNSIF
060700000000IF  2C     V1CMOV        IFEQ      '1'
060800000000     C                   EXSR      LANMOV
060900000000E   2C                   ENDIF
061000000000     C*--------------------
061100000000     C* A G G I O R N A M E N T O  SALDI CLIENTE DA CONTABILITA'
061200000000     C*--------------------
061300980403     C                   MOVEL     LIB201        KNSIF
061400000000IF  2C     V1CAGG        IFEQ      '1'
061500980403     C                   EXSR      LANAGG                                       *AGGIORNAMENTO BAR
061600000000E   2C                   ENDIF
061700980407     C*--------------------
061800980407     C* T R A S M I S S I O N E  SALDI
061900980407     C*--------------------
062000980407     C                   MOVEL     LIB201        KNSIF
062100980407IF  2C     V1CTSF        IFEQ      '1'
062200980407     C                   EXSR      LANTSF
062300980407E   2C                   ENDIF
062400000000     C*--------------------
062500000000     C* D E L T A
062600000000     C*--------------------
062700980403     C                   MOVEL     LIB201        KNSIF
062800000000IF  2C     V1CDEL        IFEQ      '1'
062900000000     C                   EXSR      LANDEL
063000000000E   2C                   ENDIF
063100980831     C*--------------------
063200980831     C* R E P O R T  COMMERCIALE
063300980831     C*--------------------
063400980831     C                   MOVEL     LIB201        KNSIF
063500980831IF  2C     V1CREP        IFEQ      '1'
063600980831     C                   EXSR      LANREP
063700980831E   2C                   ENDIF
063800980902     C*--------------------
063900980902     C* C.A.T. CREAZIONE DATI TIPO
064000980902     C*--------------------
064100980902     C                   MOVEL     LIB201        KNSIF
064200980902IF  2C     V1CCAT        IFEQ      '1'
064300980902     C                   EXSR      LANCAT
064400980902E   2C                   ENDIF
064500060124     C*--------------------
064600060124     C* Crea wrk-file VARIE fattura per quadratura ricavi con c/e
064700060124     C*--------------------
064800060124     C                   MOVEL     LIB201        KNSIF
064900081128IF  2C     V1Cqua        IFEQ      '1'
065000060124     C                   EXSR      LANric
065100081128E   2C                   ENDIF
065200120403     C*--------------------
065300120403     C* C R E A Z I O N E   ANAGRAFICA CLIENTI COLLEGATI A POTENZIALE
065400120403     C*--------------------
065500120403     C                   MOVEL     LIB201        KNSIF
065600120403IF  2C     V1CACP        IFEQ      '1'
065700120403     C                   EXSR      LANACP
065800120403E   2C                   ENDIF
065900121127     C*--------------------
066000121127     C* STATISTICA APPLICAZIONI   I S T A T
066100121127     C*--------------------
066200121127     C                   MOVEL     LIB201        KNSIF
066300121127IF  2C     V1CSAI        IFEQ      '1'
066400121127     C                   EXSR      LANSAI
066500121127E   2C                   ENDIF
066600130403     C*--------------------
066700130403     C* STATISTICA AVANZAMNETO ADESIONE FUEL
066800130403     C*--------------------
066900130403     C                   MOVEL     LIB201        KNSIF
067000130403IF  2C     V1CSFS        IFEQ      '1'
067100130403     C                   EXSR      LANSFS
067200130403E   2C                   ENDIF
067300130204     C*--------------------
067400130204     C* D E L T A   PER FASCE DI PESO
067500130204     C*--------------------
067600130204     C                   MOVEL     LIB201        KNSIF
067700130204IF  2C     V1CDFP        IFEQ      '1'
067800130204     C                   EXSR      LANDFP
067900130204E   2C                   ENDIF
068000160512     C*--------------------
068100160512     C* FATTURAZIONE INFRAGRUPPO  M C U B I
068200160512     C*--------------------
068300160512     C                   MOVEL     LIB201        KNSIF
068400160512IF  2C     V1CFIM        IFEQ      '1'
068500160512     C                   EXSR      LANFIM
068600160512E   2C                   ENDIF
068700160512     C*--------------------
068800160512     C* W F C E X    Stat.CLIENTI con FATTURATO solo EXPORT
068900160512     C*--------------------
069000160512     C                   MOVEL     LIB201        KNSIF
069100160512IF  2C     V1Ccex        IFEQ      '1'
069200160512     C                   EXSR      LANcex
069300160512E   2C                   ENDIF
069400980902     C*
069500000000     C                   MOVEL     '1'           $FINE                          *USCITA DAL PGM
069600000000E   1C                   ENDIF
069700000000     C*
069800000000     C                   ENDSR
069900000000     C*------------------------------------------------------------------------*
070000000000     C* DECODI - CONTROLLA CAMPI E DECODIFICA
070100000000     C*------------------------------------------------------------------------*
070200000000     C     DECODI        BEGSR
070300000000     C*
070400000000     C* IMPOSTA INDICATORI DI ERRORE SUI CAMPI
070500980428     C                   SETOFF                                         6061
070600000000     C                   SETOFF                                       626364
070700000000     C                   SETOFF                                       656667
070800000000     C                   SETOFF                                       686970
070900061123     C                   SETOFF                                       717377
071000000208     C                   SETOFF                                       747576
071100000000DO  1C                   DO
071200980403     C*
071300980403     C* NON LANCIABILE DA MENU' DI PROVA
071400980403IF  2C     SIFPRV        IFEQ      'P'
071500980403     C                   MOVEL     '1'           $ERR
071600980403     C                   MOVEL     MSG(11)       $MSG
071700980403     C                   EXSR      CARMSG
071800061123     C                   LEAVE
071900980403E   2C                   ENDIF
072000000000     C*--------------------
072100000000     C* ANNO
072200000000     C*--------------------
072300000000IF  2C     V1CANN        IFEQ      *ZEROS
072400000000     C                   MOVEL     '1'           $ERR
072500000000     C                   MOVEL     MSG(8)        $MSG
072600000000     C                   EXSR      CARMSG
072700000000     C                   SETON                                        60
072800000000     C                   LEAVE
072900000000E   2C                   ENDIF
073000000000     C*--------------------
073100000000     C* MESE
073200000000     C*--------------------
073300000000     C                   MOVEL     V1CANN        ANNMES            6 0
073400000000     C                   MOVE      V1CMES        ANNMES
073500000000     C                   MOVEL     ANNMES        G08INV
073600000000     C                   MOVE      01            G08INV
073700000000     C                   Z-ADD     *ZEROS        G08DAT
073800000000     C                   MOVEL     '3'           G08ERR
073900000000     C                   CALL      'XSRDA8'
074000000000     C                   PARM                    WLBDA8
074100000000IF  2C     G08ERR        IFEQ      '1'
074200000000     C                   MOVEL     '1'           $ERR
074300000000     C                   MOVEL     MSG(9)        $MSG
074400000000     C                   EXSR      CARMSG
074500000000     C                   SETON                                        61
074600000000     C                   LEAVE
074700000000E   2C                   ENDIF
074800160223     C* calcola data Fine mese da elaborare x test seguenti
074900160223     C     *ISO          MOVE      G08INV        dataFineM
075000160223      *
075100160223     C* Aggiungo un mese e tolgo un giorno ed ottengo la data di fine mese
075200160223      *  es.: 20160201 --> +1m  20160301 --> -1d  20160229 <<--
075300160223     c                   adddur    1:*m          dataFineM
075400160223     c                   subdur    1:*d          dataFineM
075500000000     C***
075600000000     C* SCELTA STATISTICHE
075700000000     C***
075800000000     C*--------------------
075900000000     C* VOLUMI
076000000000     C*--------------------
076100000000IF  2C     V1CVOL        IFNE      *BLANKS
076200000000     C     V1CVOL        ANDNE     '1'
076300000000     C                   MOVEL     '1'           $ERR
076400000000     C                   MOVEL     MSG(10)       $MSG
076500000000     C                   EXSR      CARMSG
076600000000     C                   SETON                                        62
076700000000     C                   LEAVE
076800000000E   2C                   ENDIF
076900991027     C*--------------------
077000991027     C* PESI
077100991027     C*--------------------
077200991027IF  2C     V1CPES        IFNE      *BLANKS
077300991027     C     V1CPES        ANDNE     '1'
077400991027     C                   MOVEL     '1'           $ERR
077500991027     C                   MOVEL     MSG(10)       $MSG
077600991027     C                   EXSR      CARMSG
077700991027     C                   SETON                                        79
077800991027     C                   LEAVE
077900991027E   2C                   ENDIF
078000000000     C*--------------------
078100000000     C* AFFIDABILITA'
078200000000     C*--------------------
078300000000IF  2C     V1CAFF        IFNE      *BLANKS
078400000000     C     V1CAFF        ANDNE     '1'
078500000000     C                   MOVEL     '1'           $ERR
078600000000     C                   MOVEL     MSG(10)       $MSG
078700000000     C                   EXSR      CARMSG
078800000000     C                   SETON                                        63
078900000000     C                   LEAVE
079000000000E   2C                   ENDIF
079100040108     C*--------------------
079200040108     C* AFFIDABILITA' X DATA RITIRO
079300040108     C*--------------------
079400160509IF  2C**   V1CAFR        IFNE      *BLANKS
079500160509     C**   V1CAFR        ANDNE     '1'
079600160509     C**                 MOVEL     '1'           $ERR
079700160509     C**                 MOVEL     MSG(10)       $MSG
079800160509     C**                 EXSR      CARMSG
079900160509     C**                 SETON                                        74
080000160509     C**                 LEAVE
080100160509E   2C**                 ENDIF
080200000000     C*--------------------
080300000000     C* TRAINI
080400000000     C*--------------------
080500000000IF  2C     V1CTRA        IFNE      *BLANKS
080600000000     C     V1CTRA        ANDNE     '1'
080700000000     C                   MOVEL     '1'           $ERR
080800000000     C                   MOVEL     MSG(10)       $MSG
080900000000     C                   EXSR      CARMSG
081000000000     C                   SETON                                        64
081100000000     C                   LEAVE
081200000000E   2C                   ENDIF
081300000000     C*--------------------
081400000000     C* TRANSITI
081500000000     C*--------------------
081600000000IF  2C     V1CTRS        IFNE      *BLANKS
081700000000     C     V1CTRS        ANDNE     '1'
081800000000     C                   MOVEL     '1'           $ERR
081900000000     C                   MOVEL     MSG(10)       $MSG
082000000000     C                   EXSR      CARMSG
082100000000     C                   SETON                                        65
082200000000     C                   LEAVE
082300000000E   2C                   ENDIF
082400000000     C*--------------------
082500000000     C* RITARDI TRAINI
082600000000     C*--------------------
082700000000IF  2C     V1CRIT        IFNE      *BLANKS
082800000000     C     V1CRIT        ANDNE     '1'
082900000000     C                   MOVEL     '1'           $ERR
083000000000     C                   MOVEL     MSG(10)       $MSG
083100000000     C                   EXSR      CARMSG
083200000000     C                   SETON                                        70
083300000000     C                   LEAVE
083400000000E   2C                   ENDIF
083500000000     C*--------------------
083600000000     C* MOVIMENTO
083700000000     C*--------------------
083800000000IF  2C     V1CMOV        IFNE      *BLANKS
083900000000     C     V1CMOV        ANDNE     '1'
084000000000     C                   MOVEL     '1'           $ERR
084100000000     C                   MOVEL     MSG(10)       $MSG
084200000000     C                   EXSR      CARMSG
084300000000     C                   SETON                                        66
084400000000     C                   LEAVE
084500000000E   2C                   ENDIF
084600160224     C*--------------------
084700160224     C* Clienti con fattura EXPORT
084800160224     C*--------------------
084900160224IF  2C     V1Ccex        IFNE      *BLANKS
085000160224     C     V1Ccex        ANDNE     '1'
085100160224     C                   MOVEL     '1'           $ERR
085200160224     C                   MOVEL     MSG(10)       $MSG
085300160224     C                   EXSR      CARMSG
085400160224     C                   SETON                                        72
085500160224     C                   LEAVE
085600160224E   2C                   ENDIF
085700000000     C*--------------------
085800000000     C* AGGIORNAMENTO SALDI CLIENTE DA CONTABILITA'
085900000000     C*--------------------
086000000000IF  2C     V1CAGG        IFNE      *BLANKS
086100000000     C     V1CAGG        ANDNE     '1'
086200000000     C                   MOVEL     '1'           $ERR
086300000000     C                   MOVEL     MSG(10)       $MSG
086400000000     C                   EXSR      CARMSG
086500000000     C                   SETON                                        67
086600000000     C                   LEAVE
086700000000E   2C                   ENDIF
086800160223      *
086900160223      **  controlla se vi sono ancora "Richieste NOTE di addebito"
087000160223      *    NON si possono aggiornare i SALDI
087100160223IF  2C     V1CAGG        IFEQ      '1'
087200160223     c     dataFineM     setgt     FNTNA03L
087300160223     c                   readP     FNTNA03L
087400160223     c                   if        not %EoF(FNTNA03L)
087500160223     C                   MOVEL     '1'           $ERR
087600160223     C                   MOVEL     MSG(13)       $MSG
087700160223     C                   EXSR      CARMSG
087800160223     C                   SETON                                        67
087900160223     C                   LEAVE
088000160223     c                   end
088100160223E   2C                   ENDIF
088200980407     C*--------------------
088300980407     C* TRASMISSIONE SALDI
088400980407     C*--------------------
088500980407IF  2C     V1CTSF        IFNE      *BLANKS
088600980407     C     V1CTSF        ANDNE     '1'
088700980407     C                   MOVEL     '1'           $ERR
088800980407     C                   MOVEL     MSG(10)       $MSG
088900980407     C                   EXSR      CARMSG
089000980407     C                   SETON                                        71
089100980407     C                   LEAVE
089200980407E   2C                   ENDIF
089300130130     C*--------------------
089400130130     C* DELTA PER FASCE DI PESO
089500130130     C*--------------------
089600130130IF  2C     V1CDFP        IFNE      *BLANKS
089700130130     C     V1CDFP        ANDNE     '1'
089800130130     C                   MOVEL     '1'           $ERR
089900130130     C                   MOVEL     MSG(10)       $MSG
090000130130     C                   EXSR      CARMSG
090100130130     C                   SETON                                        70
090200130130     C                   LEAVE
090300130130E   2C                   ENDIF
090400160224      *
090500160224      **  controlla se vi sono ancora "Richieste NOTE di addebito"
090600160224      *    NON si possono aggiornare i SALDI
090700160224IF  2C     V1Cdfp        IFEQ      '1'
090800160224     c     dataFineM     setgt     FNTNA03L
090900160224     c                   readP     FNTNA03L
091000160224     c                   if        not %EoF(FNTNA03L)
091100160224     C                   MOVEL     '1'           $ERR
091200160224     C                   MOVEL     MSG(13)       $MSG
091300160224     C                   EXSR      CARMSG
091400160224     C                   SETON                                        70
091500160224     C                   LEAVE
091600160224     c                   end
091700160224E   2C                   ENDIF
091800000000     C*--------------------
091900000000     C* DELTA
092000000000     C*--------------------
092100000000IF  2C     V1CDEL        IFNE      *BLANKS
092200000000     C     V1CDEL        ANDNE     '1'
092300000000     C                   MOVEL     '1'           $ERR
092400000000     C                   MOVEL     MSG(10)       $MSG
092500000000     C                   EXSR      CARMSG
092600000000     C                   SETON                                        68
092700000000     C                   LEAVE
092800000000E   2C                   ENDIF
092900160224      *
093000160224      **  controlla se vi sono ancora "Richieste NOTE di addebito"
093100160224      *    NON si possono aggiornare i SALDI
093200160224IF  2C     V1Cdel        IFEQ      '1'
093300160224     c     dataFineM     setgt     FNTNA03L
093400160224     c                   readP     FNTNA03L
093500160224     c                   if        not %EoF(FNTNA03L)
093600160224     C                   MOVEL     '1'           $ERR
093700160224     C                   MOVEL     MSG(13)       $MSG
093800160224     C                   EXSR      CARMSG
093900160224     C                   SETON                                        68
094000160224     C                   LEAVE
094100160224     c                   end
094200160224E   2C                   ENDIF
094300120329     C*--------------------
094400120329     C* C R E A Z I O N E   ANAGRAFICA CLIENTI COLLEGATI A POTENZIALE
094500120329     C*--------------------
094600120329IF  2C     V1CACP        IFNE      *BLANKS
094700120329     C     V1CACP        ANDNE     '1'
094800120329     C                   MOVEL     '1'           $ERR
094900120329     C                   MOVEL     MSG(10)       $MSG
095000120329     C                   EXSR      CARMSG
095100120329     C                   SETON                                        69
095200120329     C                   LEAVE
095300120329E   2C                   ENDIF
095400980831     C*--------------------
095500980831     C* REPORT COMMERCIALE
095600980831     C*--------------------
095700980831IF  2C     V1CREP        IFNE      *BLANKS
095800980831     C     V1CREP        ANDNE     '1'
095900980831     C                   MOVEL     '1'           $ERR
096000980831     C                   MOVEL     MSG(10)       $MSG
096100980831     C                   EXSR      CARMSG
096200061123     C                   SETON                                        77
096300980831     C                   LEAVE
096400980831E   2C                   ENDIF
096500061123     c
096600061123     c* Verifico se elaborati i saldi
096700061123     c                   if        v1crep='1'
096800061123     C                   MOVEL     V1CANN        REPAMF                         *AA/MM FINALE
096900061123     C                   MOVE      V1CMES        REPAMF
097000061123     c* La data statistica di filiale, se <>0, sede essere >= data
097100061123     c*  di lancio
097200061123     c                   if        repamf>§sdfStcu
097300061123     C                   MOVEL     '1'           $ERR
097400061123     C                   MOVEL     MSG(12)       $MSG
097500061123     C                   EXSR      CARMSG
097600061123     C                   SETON                                        77
097700061123     C                   LEAVE
097800061123     c                   endif
097900061123     c                   endif
098000121127     C*--------------------
098100121127     C* STATISTICA APPLICAZIONI ISTAT
098200121127     C*--------------------
098300121127IF  2C     V1CSAI        IFNE      *BLANKS
098400121127     C     V1CSAI        ANDNE     '1'
098500121127     C                   MOVEL     '1'           $ERR
098600121127     C                   MOVEL     MSG(10)       $MSG
098700121127     C                   EXSR      CARMSG
098800121127     C                   SETON                                        78
098900121127     C                   LEAVE
099000121127E   2C                   ENDIF
099100130403     C*--------------------
099200130403     C* STATISTICA AVANZAMNETO ADESIONE FUEL
099300130403     C*--------------------
099400130403IF  2C     V1CSFS        IFNE      *BLANKS
099500130403     C     V1CSFS        ANDNE     '1'
099600130403     C                   MOVEL     '1'           $ERR
099700130403     C                   MOVEL     MSG(10)       $MSG
099800130403     C                   EXSR      CARMSG
099900130403     C                   SETON                                        76
100000130403     C                   LEAVE
100100130403E   2C                   ENDIF
100200980902     C*--------------------
100300980902     C* C.A.T. CREAZIONE DATI TIPO
100400980902     C*--------------------
100500980902IF  2C     V1CCAT        IFNE      *BLANKS
100600980902     C     V1CCAT        ANDNE     '1'
100700980902     C                   MOVEL     '1'           $ERR
100800980902     C                   MOVEL     MSG(10)       $MSG
100900980902     C                   EXSR      CARMSG
101000980902     C                   SETON                                        75
101100980902     C                   LEAVE
101200980902E   2C                   ENDIF
101300981124     C*--------------------
101400981124     C* FATTURAZIONE INFRAGRUPPO MCUBI
101500981124     C*--------------------
101600981124IF  2C     V1CFIM        IFNE      *BLANKS
101700981124     C     V1CFIM        ANDNE     '1'
101800981124     C                   MOVEL     '1'           $ERR
101900981124     C                   MOVEL     MSG(10)       $MSG
102000981124     C                   EXSR      CARMSG
102100981124     C                   SETON                                        73
102200981124     C                   LEAVE
102300981124E   2C                   ENDIF
102400081128     C*--------------------
102500081128     C* SCELTA quadratura ricavi con C/E
102600081128     C*--------------------
102700081128IF  2C     V1Cqua        IFNE      *BLANKS
102800081128     C     V1Cqua        ANDNE     '1'
102900081128     C                   MOVEL     '1'           $ERR
103000081128     C                   MOVEL     MSG(10)       $MSG
103100081128     C                   EXSR      CARMSG
103200081128     C                   SETON                                        80
103300081128     C                   LEAVE
103400081128E   2C                   ENDIF
103500000208     C*
103600000000E   1C                   ENDDO
103700000000     C*
103800000000     C                   ENDSR
103900991223     C*------------------------------------------------------------------------*
104000991223     C* LANVOL - LANCIA LA STATISTICA VOLUMI
104100991223     C*------------------------------------------------------------------------*
104200991223     C     LANVOL        BEGSR
104300991223     C***
104400991223     C* 1° LANCIO
104500991223     C***
104600991223     C                   CLEAR                   KPJBU
104700991223     C                   Z-ADD     V1CANN        VOLANN                         *ANNO
104800991223     C                   Z-ADD     01            VOLMMI                         *MESE INIZIALE
104900991223     C                   Z-ADD     V1CMES        VOLMMF                         *MESE FINALE
105000991223     C                   MOVEL     'S'           VOLSNR                         *RIGENERA WFILE
105100050124     C                   MOVEL     'S'           VOLSAL                         *RIGENERA WRKF Saldi
105200000706     C                   MOVEL     'N'           VOLPOS                         *NO DATI POSTE
105300991223     C* INSERISCO DATI DFT DA TABELLA "TP"
105400991223     C                   MOVEL     'TP'          KTBCOD
105500991223     C                   MOVEL(P)  '1'           KTBKEY
105600991223     C     KEYTAB        CHAIN     TABEL00F                           98
105700130213IF  1c                   if        *in98
105800130213     c                   leavesr
105900130213     c                   endif
106000130213IF  1C*//     *IN98         IFEQ      *OFF
106100991223     C                   MOVEL     TBLUNI        DSTP
106200991223     C                   Z-ADD     §TVDPD        VOLRG1
106300991223     C     VOLRG1        MULT      -1            VOLRG1
106400991223     C                   Z-ADD     §TVDPA        VOLRG2
106500991223     C                   Z-ADD     §TVCID        VOLPID
106600991223     C                   Z-ADD     §TVCSD        VOLPSD
106700991230     C                   Z-ADD     §TVMIN        VOLMIN
106800130213     C*//                ELSE
106900130213     C*//                Z-ADD     999,00        VOLRG1
107000130213     C*//  VOLRG1        MULT      -1            VOLRG1
107100130213     C*//                Z-ADD     999,00        VOLRG2
107200130213     C*//                Z-ADD     030,00        VOLPID
107300130213     C*//                Z-ADD     030,00        VOLPSD
107400130213     C*//                Z-ADD     04,000        VOLMIN
107500130213     C*//                ENDIF
107600991223     C                   MOVEL     'S'           VOLDET                         *SINTETICA
107700991223     C                   MOVEL     'N'           VOLTOT                         *NO TOTALI
107800000110     C                   MOVEL     'N'           VOLREA                         *NO "REALI"
107900991227     C                   MOVEL     'ST19'        KCOAZ
108000991223     C                   MOVEL     PARVOL        KPJBU
108100991223     C                   CALL      'BCH10'
108200991223     C                   PARM                    KPJBA
108300991223     C***
108400991223     C* 2° LANCIO
108500991223     C***
108600991223     C                   MOVEL     'N'           VOLSNR                         *NON RIGENERA WFILE
108700050124     C                   MOVEL     'N'           VOLSAL                         *NON RIGENERA WRKF Saldi
108800991223     C                   MOVEL     'D'           VOLDET                         *DETTAGLIO
108900991227     C                   MOVEL     'ST19'        KCOAZ
109000991223     C                   MOVEL     PARVOL        KPJBU
109100991223     C                   CALL      'BCH10'
109200991223     C                   PARM                    KPJBA
109300991223     C*
109400991223     C                   ENDSR
109500991027     C*------------------------------------------------------------------------*
109600991027     C* LANPES - LANCIA LA STATISTICA PESI
109700991027     C*------------------------------------------------------------------------*
109800991027     C     LANPES        BEGSR
109900991027     C***
110000991027     C* 1° LANCIO
110100991027     C***
110200991027     C                   CLEAR                   KPJBU
110300991027     C                   Z-ADD     V1CANN        PESANN                         *ANNO
110400991027     C                   Z-ADD     01            PESMMI                         *MESE INIZIALE
110500991027     C                   Z-ADD     V1CMES        PESMMF                         *MESE FINALE
110600991027     C                   MOVEL     'S'           PESSNR                         *RIGENERA WFILE
110700050124     C                   MOVEL     'S'           PESSAL                         *RIGENERA WRKF Saldi
110800000706     C                   MOVEL     'N'           PESPOS                         *NO DATI POSTE
110900011112     C                   MOVEL     'N'           PESNEG                         *NO DATI POSTE
111000991027     C* INSERISCO DATI DFT DA TABELLA "TP"
111100991027     C                   MOVEL     'TP'          KTBCOD
111200991027     C                   MOVEL(P)  '1'           KTBKEY
111300991027     C     KEYTAB        CHAIN     TABEL00F                           98
111400130213IF  1c                   if        *in98
111500130213     c                   leavesr
111600130213     c                   endif
111700130213IF  1C*//  *IN98         IFEQ      *OFF
111800991027     C                   MOVEL     TBLUNI        DSTP
111900991027     C                   Z-ADD     §TPDPD        PESRG1
112000991027     C     PESRG1        MULT      -1            PESRG1
112100991027     C                   Z-ADD     §TPDPA        PESRG2
112200991027     C                   Z-ADD     §TPCOI        PESPEI
112300991027     C                   Z-ADD     §TPCOS        PESPES
112400991027     C                   Z-ADD     §TPTAR        PESTAR
112500991230     C                   Z-ADD     §TPMIN        PESMIN
112600130213     C*//                ELSE
112700130213     C*//                Z-ADD     999,0         PESRG1
112800130213     C*//  PESRG1        MULT      -1            PESRG1
112900130213     C*//                Z-ADD     999,0         PESRG2
113000130213     C*//                Z-ADD     010,00        PESPEI
113100130213     C*//                Z-ADD     010,00        PESPES
113200130213     C*//                Z-ADD     000,00        PESTAR
113300130213     C*//                Z-ADD     01,000        PESMIN
113400130213     C*//                ENDIF
113500991027     C                   MOVEL     'S'           PESDET                         *SINTETICA
113600991027     C                   MOVEL     'N'           PESTOT                         *NO TOTALI
113700991027     C                   MOVEL     'ST16'        KCOAZ
113800991027     C                   MOVEL     PARPES        KPJBU
113900991027     C                   CALL      'BCH10'
114000991027     C                   PARM                    KPJBA
114100991027     C***
114200991027     C* 2° LANCIO
114300991027     C***
114400991027     C                   MOVEL     'N'           PESSNR                         *NON RIGENERA WFILE
114500050124     C                   MOVEL     'N'           PESSAL                         *NON RIGENERA WRKF Saldi
114600991027     C                   MOVEL     'D'           PESDET                         *DETTAGLIO
114700991027     C                   MOVEL     'ST16'        KCOAZ
114800991129     C                   MOVEL     PARPES        KPJBU
114900991027     C                   CALL      'BCH10'
115000991027     C                   PARM                    KPJBA
115100991027     C*
115200991027     C                   ENDSR
115300000000     C*------------------------------------------------------------------------*
115400000000     C* LANAFF - LANCIA LA STATISTICA AFFIDABILITA'
115500000000     C*------------------------------------------------------------------------*
115600000000     C     LANAFF        BEGSR
115700000000     C***
115800000000     C* 1° LANCIO
115900000000     C***
116000000000     C                   CLEAR                   KPJBU
116100000000     C                   Z-ADD     V1CANN        AFFANN                         *ANNO
116200000000     C                   Z-ADD     V1CMES        AFFMMI                         *MESE INIZIALE
116300000000     C                   Z-ADD     V1CMES        AFFMMF                         *MESE FINALE
116400000000     C                   MOVEL     'S'           AFFSNC                         *AFFID.TA' CLIENTE
116500000000     C                   MOVEL     'S'           AFFSDC                         *CLIENTE SINTETICA
116600000000     C                   MOVEL     'S'           AFFSNA                         *AFFID.TA' CONSEGNA
116700000000     C                   MOVEL     'S'           AFFSDA                         *CONSEGNA SINTETICA
116800000000     C                   MOVEL     'S'           AFFTLA                         *LANCIO PER SALDI
116900000000     C                   MOVEL     'S'           AFFSNR                         *RIGENERA WFILE
117000970827     C                   MOVEL     'S'           AFFCFP                         *RIGENERA FILE PC
117100000000     C                   MOVEL     'S'           AFFSNP                         *AFFID.TA' PARTENZA
117200000000     C                   MOVEL     'N'           AFFTOT                         *NO TOTALI
117300000000     C                   MOVEL     'I'           AFFIOE                         *DATI ITALIA
117400000000     C                   MOVEL     'SS31'        KCOAZ
117500000000     C                   MOVEL     PARAFF        KPJBU
117600000000     C                   CALL      'BCH10'
117700000000     C                   PARM                    KPJBA
117800000000     C***
117900000000     C* 2° LANCIO
118000000000     C***
118100000000     C                   MOVEL     'S'           AFFSNC                         *AFFID.TA' CLIENTE
118200000000     C                   MOVEL     'D'           AFFSDC                         *CLIENTE DETTAGLIO
118300000000     C                   MOVEL     'S'           AFFSNA                         *AFFID.TA' CONSEGNA
118400000000     C                   MOVEL     'D'           AFFSDA                         *CONSEGNA DETTAGLIO
118500000000     C                   MOVEL     'N'           AFFSNR                         *NON RIGENERA WFILE
118600970827     C                   MOVEL     'N'           AFFCFP                         *NON RIGENERA FILE P
118700000000     C                   MOVEL     'N'           AFFSNP                         *NO AFFID.TA PARTENZ
118800000000     C                   MOVEL     'I'           AFFIOE                         *DATI ITALIA
118900000000     C                   MOVEL     'SS31'        KCOAZ
119000000000     C                   MOVEL     PARAFF        KPJBU
119100000000     C                   CALL      'BCH10'
119200000000     C                   PARM                    KPJBA
119300000000     C***
119400000000     C* 3° LANCIO
119500000000     C***
119600000000     C                   MOVEL     'S'           AFFSNC                         *AFFID.TA' CLIENTE
119700000000     C                   MOVEL     'S'           AFFSDC                         *CLIENTE SINTETICA
119800000000     C                   MOVEL     'S'           AFFSNA                         *AFFID.TA' CONSEGNA
119900000000     C                   MOVEL     'S'           AFFSDA                         *CONSEGNA SINTETICA
120000000000     C                   MOVEL     'N'           AFFSNR                         *NON RIGENERA WFILE
120100970827     C                   MOVEL     'N'           AFFCFP                         *NON RIGENERA FILE P
120200000000     C                   MOVEL     'S'           AFFSNP                         *AFFID.TA' PARTENZA
120300000000     C                   MOVEL     'M'           AFFIOE                         *DATI IMPORT
120400000000     C                   MOVEL     'SS31'        KCOAZ
120500000000     C                   MOVEL     PARAFF        KPJBU
120600000000     C                   CALL      'BCH10'
120700000000     C                   PARM                    KPJBA
120800000000     C***
120900000000     C* 4° LANCIO
121000000000     C***
121100000000     C                   MOVEL     'S'           AFFSNC                         *AFFID.TA' CLIENTE
121200000000     C                   MOVEL     'D'           AFFSDC                         *CLIENTE DETTAGLIO
121300000000     C                   MOVEL     'N'           AFFSNA                         *NO AFFID.TA CONSEGN
121400000000     C                   MOVEL     'N'           AFFSNR                         *NON RIGENERA WFILE
121500970827     C                   MOVEL     'N'           AFFCFP                         *NON RIGENERA FILE P
121600000000     C                   MOVEL     'N'           AFFSNP                         *NO AFFID.TA PARTENZ
121700000000     C                   MOVEL     'M'           AFFIOE                         *DATI IMPORT
121800000000     C                   MOVEL     'SS31'        KCOAZ
121900000000     C                   MOVEL     PARAFF        KPJBU
122000000000     C                   CALL      'BCH10'
122100000000     C                   PARM                    KPJBA
122200000000     C***
122300000000     C* 5° LANCIO
122400000000     C***
122500000000     C                   MOVEL     'S'           AFFSNC                         *AFFID.TA' CLIENTE
122600000000     C                   MOVEL     'S'           AFFSDC                         *CLIENTE SINTETICA
122700000000     C                   MOVEL     'S'           AFFSNA                         *AFFID.TA' CONSEGNA
122800000000     C                   MOVEL     'S'           AFFSDA                         *CONSEGNA SINTETICA
122900000000     C                   MOVEL     'N'           AFFSNR                         *NON RIGENERA WFILE
123000970827     C                   MOVEL     'N'           AFFCFP                         *NON RIGENERA FILE P
123100000000     C                   MOVEL     'S'           AFFSNP                         *AFFID.TA' PARTENZA
123200000000     C                   MOVEL     'X'           AFFIOE                         *DATI EXPORT
123300000000     C                   MOVEL     'SS31'        KCOAZ
123400000000     C                   MOVEL     PARAFF        KPJBU
123500000000     C                   CALL      'BCH10'
123600000000     C                   PARM                    KPJBA
123700000000     C***
123800000000     C* 6° LANCIO
123900000000     C***
124000000000     C                   MOVEL     'S'           AFFSNC                         *AFFID.TA' CLIENTE
124100000000     C                   MOVEL     'D'           AFFSDC                         *CLIENTE DETTAGLIO
124200000000     C                   MOVEL     'N'           AFFSNA                         *NO AFFID.TA CONSEGN
124300000000     C                   MOVEL     'N'           AFFSNR                         *NON RIGENERA WFILE
124400970827     C                   MOVEL     'N'           AFFCFP                         *NON RIGENERA FILE P
124500000000     C                   MOVEL     'N'           AFFSNP                         *NO AFFID.TA PARTENZ
124600000000     C                   MOVEL     'X'           AFFIOE                         *DATI EXPORT
124700000000     C                   MOVEL     'SS31'        KCOAZ
124800000000     C                   MOVEL     PARAFF        KPJBU
124900000000     C                   CALL      'BCH10'
125000000000     C                   PARM                    KPJBA
125100000000     C*
125200000000     C                   ENDSR
125300040108     C*------------------------------------------------------------------------*
125400040108     C* LANAFR - LANCIA LA STATISTICA AFFIDABILITA' X DATA RITIRO
125500040108     C*------------------------------------------------------------------------*
125600160509     C***  LANAFR        BEGSR
125700040108     C***
125800040108     C* 1° LANCIO
125900040108     C***
126000160509     C***                CLEAR                   KPJBU
126100160509     C***                Z-ADD     V1CANN        AFRANN                         *ANNO
126200160509     C***                Z-ADD     V1CMES        AFRMMI                         *MESE INIZIALE
126300160509     C***                Z-ADD     V1CMES        AFRMMF                         *MESE FINALE
126400160509     C***                MOVEL     'N'           AFRSNC                         *AFFID.TA' CLIENTE
126500160509     C***                MOVEL     'N'           AFRSDC                         *CLIENTE SINTETICA
126600160509     C***                MOVEL     'N'           AFRSNA                         *AFFID.TA' CONSEGNA
126700160509     C***                MOVEL     'N'           AFRSDA                         *CONSEGNA SINTETICA
126800160509     C***                MOVEL     'S'           AFRSNR                         *RIGENERA WFILE
126900160509     C***                MOVEL     'S'           AFRTLA                         *LANCIO PER SALDI
127000160509     C***                MOVEL     'S'           AFRSNP                         *AFFID.TA' PARTENZA
127100160509     C***                MOVEL     'N'           AFRTOT                         *NO TOTALI
127200160509     C***                MOVEL     'I'           AFRIOE                         *DATI ITALIA
127300160509     C***                MOVEL     'N'           AFRCFP                         *RIGENERA FILE PC
127400160509     C***                MOVEL     'SS36'        KCOAZ
127500160509     C***                MOVEL     PARAFR        KPJBU
127600160509     C***                CALL      'BCH10'
127700160509     C***                PARM                    KPJBA
127800040108     C*
127900160509     C***                ENDSR
128000000000     C*------------------------------------------------------------------------*
128100000000     C* LANTRA - LANCIA LA STATISTICA TRAINI
128200000000     C*------------------------------------------------------------------------*
128300000000     C     LANTRA        BEGSR
128400000000     C***
128500000000     C* 1° LANCIO
128600000000     C***
128700000000     C                   CLEAR                   KPJBU
128800990609     C                   Z-ADD     V1CANN        TRAAAI                         *ANNO INIZIALE
128900000000     C                   Z-ADD     01            TRAMMI                         *MESE INIZIALE
129000990609     C                   Z-ADD     V1CANN        TRAAAF                         *ANNO FINALE
129100000000     C                   Z-ADD     V1CMES        TRAMMF                         *MESE FINALE
129200000000     C                   MOVEL     'P'           TRAMPA                         *PARTENZA
129300000000     C                   MOVEL     'S'           TRAFPC                         *SCRIVE FILE PC
129400000000     C                   MOVEL     'S'           TRACRE                         *AGGIORNA WORKFILE
129500000706     C                   MOVEL     'N'           TRAPOS                         *DATI POSTE
129600000000     C                   MOVEL     'SS75'        KCOAZ
129700000000     C                   MOVEL     PARTRA        KPJBU
129800000000     C                   CALL      'BCH10'
129900000000     C                   PARM                    KPJBA
130000000000     C*
130100000000     C                   ENDSR
130200000000     C*------------------------------------------------------------------------*
130300000000     C* LANTRS - LANCIA LA STATISTICA TRANSITI
130400000000     C*------------------------------------------------------------------------*
130500000000     C     LANTRS        BEGSR
130600000000     C***
130700000000     C* 1° LANCIO
130800000000     C***
130900000000     C                   CLEAR                   KPJBU
131000990609     C                   Z-ADD     V1CANN        TRSANI                         *ANNO INIZIALE
131100000000     C                   Z-ADD     V1CMES        TRSMMI                         *MESE INIZIALE
131200990609     C                   Z-ADD     V1CANN        TRSANF                         *ANNO FINALE
131300000000     C                   Z-ADD     V1CMES        TRSMMF                         *MESE FINALE
131400000000     C                   MOVEL     'SS85'        KCOAZ
131500000000     C                   MOVEL     PARTRS        KPJBU
131600000000     C                   CALL      'BCH10'
131700000000     C                   PARM                    KPJBA
131800000000     C*
131900000000     C                   ENDSR
132000000000     C*------------------------------------------------------------------------*
132100000000     C* LANRIT - LANCIA LA STATISTICA RITARDI TRAINI
132200000000     C*------------------------------------------------------------------------*
132300000000     C     LANRIT        BEGSR
132400000000     C***
132500000000     C* 1° LANCIO
132600000000     C***
132700000000     C                   CLEAR                   KPJBU
132800990609     C                   Z-ADD     V1CANN        RITAAI                         *ANNO INIZIALE
132900000000     C                   Z-ADD     01            RITMMI                         *MESE INIZIALE
133000990609     C                   Z-ADD     V1CANN        RITAAF                         *ANNO FINALE
133100000000     C                   Z-ADD     V1CMES        RITMMF                         *MESE FINALE
133200000000     C                   MOVEL     'S'           RITCRE                         *CREAZIONE WORKFILE
133300000710     C                   MOVEL     'N'           RITPOS                         *NO DATI POSTE
133400000000     C                   MOVEL     'SS87'        KCOAZ
133500000000     C                   MOVEL     PARRIT        KPJBU
133600000000     C                   CALL      'BCH10'
133700000000     C                   PARM                    KPJBA
133800000000     C*
133900000000     C                   ENDSR
134000000000     C*------------------------------------------------------------------------*
134100000000     C* LANMOV - LANCIA LA STATISTICA MOVIMENTO
134200000000     C*------------------------------------------------------------------------*
134300000000     C     LANMOV        BEGSR
134400000000     C***
134500000000     C* 1° LANCIO
134600000000     C***
134700000000     C                   CLEAR                   KPJBU
134800990609     C                   Z-ADD     V1CANN        MOVANI                         *ANNO INIZIALE
134900000000     C                   Z-ADD     01            MOVMMI                         *MESE INIZIALE
135000990609     C                   Z-ADD     V1CANN        MOVANF                         *ANNO FINALE
135100000000     C                   Z-ADD     V1CMES        MOVMMF                         *MESE FINALE
135200000000     C                   MOVEL     'P'           MOVMPA                         *PARTENZA
135300000000     C                   MOVEL     'N'           MOVDET                         *NO DETTAGLIO FIL/FI
135400000000     C                   MOVEL     'S'           MOVFPC                         *CREA FILE PC
135500000000     C                   MOVEL     'I'           MOVIOE                         *DATI ITALIA
135600000703     C                   MOVEL     'N'           MOVPOS                         *NO DATI POSTE
135700020118     C                   MOVEL     'SE46'        KCOAZ
135800000000     C                   MOVEL     PARMOV        KPJBU
135900000000     C                   CALL      'BCH10'
136000000000     C                   PARM                    KPJBA
136100000000     C***
136200000000     C* 2° LANCIO
136300000000     C***
136400000000     C                   MOVEL     'P'           MOVMPA                         *PARTENZA
136500000000     C                   MOVEL     'S'           MOVFPC                         *CREA FILE PC
136600000000     C                   MOVEL     'M'           MOVIOE                         *DATI IMPORT
136700020118     C                   MOVEL     'SE46'        KCOAZ
136800000000     C                   MOVEL     PARMOV        KPJBU
136900000000     C                   CALL      'BCH10'
137000000000     C                   PARM                    KPJBA
137100000000     C***
137200000000     C* 3° LANCIO
137300000000     C***
137400000000     C                   MOVEL     'P'           MOVMPA                         *PARTENZA
137500000000     C                   MOVEL     'S'           MOVFPC                         *CREA FILE PC
137600000000     C                   MOVEL     'X'           MOVIOE                         *DATI EXPORT
137700020118     C                   MOVEL     'SE46'        KCOAZ
137800000000     C                   MOVEL     PARMOV        KPJBU
137900000000     C                   CALL      'BCH10'
138000000000     C                   PARM                    KPJBA
138100000000     C***
138200000000     C* 4° LANCIO
138300000000     C***
138400000000     C                   MOVEL     'A'           MOVMPA                         *ARRIVO
138500000000     C                   MOVEL     'S'           MOVFPC                         *CREA FILE PC
138600000000     C                   MOVEL     'I'           MOVIOE                         *DATI ITALIA
138700020118     C                   MOVEL     'SE46'        KCOAZ
138800000000     C                   MOVEL     PARMOV        KPJBU
138900000000     C                   CALL      'BCH10'
139000000000     C                   PARM                    KPJBA
139100000000     C***
139200000000     C* 5° LANCIO
139300000000     C***
139400000000     C                   MOVEL     'A'           MOVMPA                         *ARRIVO
139500000000     C                   MOVEL     'S'           MOVFPC                         *CREA FILE PC
139600000000     C                   MOVEL     'M'           MOVIOE                         *DATI IMPORT
139700020118     C                   MOVEL     'SE46'        KCOAZ
139800000000     C                   MOVEL     PARMOV        KPJBU
139900000000     C                   CALL      'BCH10'
140000000000     C                   PARM                    KPJBA
140100000000     C***
140200000000     C* 6° LANCIO
140300000000     C***
140400000000     C                   MOVEL     'A'           MOVMPA                         *ARRIVO
140500000000     C                   MOVEL     'S'           MOVFPC                         *CREA FILE PC
140600000000     C                   MOVEL     'X'           MOVIOE                         *DATI EXPORT
140700020118     C                   MOVEL     'SE46'        KCOAZ
140800000000     C                   MOVEL     PARMOV        KPJBU
140900000000     C                   CALL      'BCH10'
141000000000     C                   PARM                    KPJBA
141100000830     C***
141200000830     C* 7° LANCIO
141300000830     C***
141400100511     C**                 MOVEL     'P'           MOVMPA                         *PARTENZA
141500100511     C*+                 MOVEL     'S'           MOVFPC                         *CREA FILE PC
141600100511     C**                 MOVEL     'I'           MOVIOE                         *DATI ITALIA
141700100511     C**                 MOVEL     'S'           MOVPOS                         *SI DATI POSTE
141800100511     C**                 MOVEL     'SE46'        KCOAZ
141900100511     C**                 MOVEL     PARMOV        KPJBU
142000100511     C**                 CALL      'BCH10'
142100100511     C**                 PARM                    KPJBA
142200000830     C***
142300000830     C* 10° LANCIO
142400000830     C***
142500100511     C**                 MOVEL     'A'           MOVMPA                         *ARRIVO
142600100511     C**                 MOVEL     'S'           MOVFPC                         *CREA FILE PC
142700100511     C**                 MOVEL     'I'           MOVIOE                         *DATI ITALIA
142800100511     C**                 MOVEL     'SE46'        KCOAZ
142900100511     C**                 MOVEL     PARMOV        KPJBU
143000100511     C**                 CALL      'BCH10'
143100100511     C**                 PARM                    KPJBA
143200000000     C*
143300000000     C                   ENDSR
143400000000     C*------------------------------------------------------------------------*
143500000000     C* LANAGG - LANCIA L'AGGIORNAMENTO SALDI CLIENTE DA CONTABILITA'
143600000000     C*------------------------------------------------------------------------*
143700000000     C     LANAGG        BEGSR
143800070313     C***
143900070313     C* 1° LANCIO
144000070313     C***
144100100505     c* per i primi 4 mesi dell'anno sottometto anche l'anno prima
144200100505     c                   if        v1cmes <= 4
144300070313     C                   CLEAR                   KPJBU
144400070313     c                   eval      aggaai = v1cann - 1
144500070313     c                   eval      aggaaf = v1cann - 1
144600070313     C                   Z-ADD     01            AGGMMI                         *MESE INIZIALE
144700070313     C                   Z-ADD     12            AGGMMF                         *MESE FINALE
144800070313     C                   MOVEL     'S'           AGGAGG                         *SI AGGIORNAMENTO
144900070313     C                   MOVEL     'Y902'        KCOAZ
145000070313     C                   MOVEL     PARAGG        KPJBU
145100070313     C                   CALL      'BCH10'
145200070313     C                   PARM                    KPJBA
145300070313     c                   end
145400000000     C***
145500070313     C* 2° LANCIO
145600000000     C***
145700070313     C                   CLEAR                   KPJBU
145800070313     C                   Z-ADD     V1CANN        AGGAAI                         *ANNO INIZIALE
145900070313     C                   Z-ADD     01            AGGMMI                         *MESE INIZIALE
146000070313     C                   Z-ADD     V1CANN        AGGAAF                         *ANNO FINALE
146100070313     C                   Z-ADD     V1CMES        AGGMMF                         *MESE FINALE
146200070313     C                   MOVEL     'S'           AGGAGG                         *SI AGGIORNAMENTO
146300070313     C                   MOVEL     'Y902'        KCOAZ
146400070313     C                   MOVEL     PARAGG        KPJBU
146500070313     C                   CALL      'BCH10'
146600070313     C                   PARM                    KPJBA
146700000000     C*
146800000000     C                   ENDSR
146900980407     C*------------------------------------------------------------------------*
147000980407     C* LANTSF - LANCIA LA TRASMISSIONE DEI SALDI IN FILIALE
147100980407     C*------------------------------------------------------------------------*
147200980407     C     LANTSF        BEGSR
147300980407     C*
147400980407     C                   CLEAR                   KPJBU
147500980407     C                   MOVEL     'TRE0'        KCOAZ
147600980407     C                   CALL      'BCH10'
147700980407     C                   PARM                    KPJBA
147800980407     C*
147900980407     C                   ENDSR
148000000000     C*------------------------------------------------------------------------*
148100000000     C* LANDEL - LANCIA LA STATISTICA DELTA
148200000000     C*------------------------------------------------------------------------*
148300000000     C     LANDEL        BEGSR
148400990713     C***
148500990713     C* 1° LANCIO
148600990713     C***
148700990713     C* SOLO DELTA DIVISIONE DA -20% A -999%
148800000208     C*                    CLEARKPJBU
148900000208     C*                    Z-ADDV1CANN    DELAAI           *ANNO INIZIALE
149000000208     C*                    Z-ADDV1CMES    DELMMI           *MESE INIZIALE
149100000208     C*                    Z-ADDV1CANN    DELAAF           *ANNO FINALE
149200000208     C*                    Z-ADDV1CMES    DELMMF           *MESE FINALE
149300000208     C*                    MOVEL'N'       DELDCK           *SI 1^ STATISTICA
149400000208     C*                    MOVEL'N'       DELDCC           *SI 2^ STATISTICA
149500000208     C*                    MOVEL'S'       DELDCF           *SI 3^ STATISTICA
149600000208     C*                    MOVEL'N'       DELDFF           *SI 4^ STATISTICA
149700000208     C*                    MOVEL'S'       DELCRE           *CREA WFILE
149800000208     C*                    MOVEL'I'       DELIOE           *DATI ITALIA
149900000707     C*                    MOVEL'N'       DELPOS           *NO DATI POSTE
150000000208     C*                    MOVEL*BLANKS   DELCLV           *QUALITA' CLIENTE
150100000208     C*                    Z-ADD*ZEROS    DELSPD           *% SCOST.DELTA FASCE
150200000208     C*                    Z-ADD-999,9    DELPDD           *% SCOST.DELTA TOT.DA
150300000208     C*                    Z-ADD-20,0     DELPDA           *% SCOST.DELTA TOT.A
150400000208     C*                    EXSR X12PRE                     *CALCOLA 12 MM PRECED
150500000208     C*                    Z-ADDANPDAT    DELDPT           AA/MM INIZ.PRECEDENTE
150600000208     C*                    Z-ADDANPDA2    DELDP2           AA/MM FIN. PRECEDENTE
150700090824     C*
150800020118     C*                    MOVEL'SE91'    KCOAZ
150900000208     C*                    MOVELPARDEL    KPJBU
151000000208     C*                    CALL 'BCH10'
151100000208     C*                    PARM           KPJBA
151200990713     C***
151300990713     C* 2° LANCIO
151400990713     C***
151500990713     C* SOLO DELTA DIVISIONE DA -40% A -999%
151600000208     C*                    Z-ADD-999,9    DELPDD           *% SCOST.DELTA TOT.DA
151700000208     C*                    Z-ADD-40,0     DELPDA           *% SCOST.DELTA TOT.A
151800020118     C*                    MOVEL'SE91'    KCOAZ
151900000208     C*                    MOVELPARDEL    KPJBU
152000000208     C*                    CALL 'BCH10'
152100000208     C*                    PARM           KPJBA
152200000000     C***
152300990713     C* 3° LANCIO
152400000000     C***
152500990713     C* LANCIO STANDARD
152600000000     C                   CLEAR                   KPJBU
152700990609     C                   Z-ADD     V1CANN        DELAAI                         *ANNO INIZIALE
152800000000     C                   Z-ADD     01            DELMMI                         *MESE INIZIALE
152900990609     C                   Z-ADD     V1CANN        DELAAF                         *ANNO FINALE
153000000000     C                   Z-ADD     V1CMES        DELMMF                         *MESE FINALE
153100000000     C                   MOVEL     'S'           DELDCK                         *SI 1^ STATISTICA
153200000000     C                   MOVEL     'S'           DELDCC                         *SI 2^ STATISTICA
153300000000     C                   MOVEL     'S'           DELDCF                         *SI 3^ STATISTICA
153400000000     C                   MOVEL     'S'           DELDFF                         *SI 4^ STATISTICA
153500000000     C                   MOVEL     'S'           DELCRE                         *CREA WFILE
153600000000     C                   MOVEL     'I'           DELIOE                         *DATI ITALIA
153700000707     C                   MOVEL     'N'           DELPOS                         *NO DATI POSTE
153800000000     C                   MOVEL     *BLANKS       DELCLV                         *QUALITA' CLIENTE
153900001122     C                   MOVEL     'U'           DELSCE                         *SCELTA CLIENTE
154000980528     C                   Z-ADD     10            DELSPD                         *% SCOST.DELTA FASCE
154100980528     C                   Z-ADD     -999,9        DELPDD                         *% SCOST.DELTA TOT.D
154200980528     C                   Z-ADD     +999,9        DELPDA                         *% SCOST.DELTA TOT.A
154300020702     C                   Z-ADD     -999,9        DELDFD                         *% SCOST.FATT. AP MESE DA
154400020702     C                   Z-ADD     -20,0         DELDFA                         *% SCOST.FATT. AP MESE  A
154500020702     C                   Z-ADD     -999,9        DELDFDP                        *% SCOST.FATT. AP PROG DA
154600020702     C                   Z-ADD     -5,0          DELDFAP                        *% SCOST.FATT. AP PROG  A
154700990713     C                   EXSR      X12PRE                                       *CALCOLA 12 MM PRECE
154800000000     C                   Z-ADD     ANPDAT        DELDPT                         AA/MM INIZ.PRECEDENT
154900000000     C                   Z-ADD     ANPDA2        DELDP2                         AA/MM FIN. PRECEDENT
155000010618     C                   MOVEL     'N'           DELSIN                         *STAMPA SINTETICA
155100020409     C                   MOVEL     'N'           DELNTW                         *ROTTURA NETWORK IN ESTERO
155200030408     C                   MOVEL     'S'           DELFPC                         *CREAZIONE FILE PC
155300030912     C                   MOVEL     'S'           DELWPC                         *CREAZIONE FILE PC del TISE94R
155400030923     C                   MOVEL     'S'           DELWCP                         *CREAZIONE FILE PC del TISE93R
155500090824      * Stat. Clienti Persi (Trimestrale x il momento)                          lancio trimestre(T)
155600090824     c                   eval      DELCLPtmp = 'N'
155700020118     C                   MOVEL     'SE91'        KCOAZ
155800000000     C                   MOVEL     PARDEL        KPJBU
155900000000     C                   CALL      'BCH10'
156000000000     C                   PARM                    KPJBA
156100000000     C***
156200990713     C* 4° LANCIO
156300000000     C***
156400990713     C* LANCIO STANDARD
156500000000     C                   MOVEL     'N'           DELCRE                         *NON CREA WFILE
156600000000     C                   MOVEL     'E'           DELIOE                         *DATI ESTERO
156700020624     C                   MOVEL     'S'           DELNTW                         *ROTTURA NETWORK IN ESTERO
156800050408     C                   MOVEL     'S'           DELFPC                         *CREAZIONE FILE PC
156900030912     C                   MOVEL     'S'           DELWPC                         *CREAZIONE FILE PC del TISE94R
157000030923     C                   MOVEL     'S'           DELWCP                         *CREAZIONE FILE PC del TISE93R
157100090908      * Stat. Clienti Persi (Lanciato trimestralmente)
157200090908     c                   eval      DELCLPtmp = 'T'
157300020118     C                   MOVEL     'SE91'        KCOAZ
157400000000     C                   MOVEL     PARDEL        KPJBU
157500000000     C                   CALL      'BCH10'
157600000000     C                   PARM                    KPJBA
157700000000     C*
157800000000     C                   ENDSR
157900130130      *------------------------------------------------------------------------*
158000130130      * LanDFP - LANCIA LA STATISTICA DELTA PER FASCE DI PESO
158100130130      *------------------------------------------------------------------------*
158200130130     c     LanDFP        BegSr
158300130130      *
158400130208      * - Il reperimento dati da tab. 5S=Dati statistiche sede/filiale e la
158500130208      *   corretta impostazione del periodo verranno eseguiti dal sottomesso
158600130208      *   (TISEA21R)
158700130208      *
158800130208     c                   z-add     V1Cann        DFPann                         *Anno
158900130208     c                   z-add     V1Cmes        DFPmes                         *Mese
159000130130      *
159100130208     c                   eval      KCOAZ = 'SEAB'
159200130130     c                   eval      KPJBU = ParDFP
159300130130     c                   call      'BCH10'
159400130130     c                   parm                    KPJBA
159500130130      *
159600130130     c                   EndSr
159700131011      *------------------------------------------------------------------------*
159800131011      * LanCEX - LANCIA LA STATISTICA Fatturato CLIENTI EXPORT
159900131011      *------------------------------------------------------------------------*
160000131011     c     LanCEX        BegSr
160100131011      *
160200131011      * - Esegue la statistica con il Fatturato Clienti Export
160300131011      *   a Gennaio  storicizza l'anno precedente.
160400131011      *   (TISE84C)
160500131011     c                   clear                   kpjbu
160600131011     c                   clear                   parCEX
160700131011     c                   movel     'TISE84R'     cexPGM                         *PGM CHIAMATO
160800131011     c                   movel     'WFCEX00F'    cexFIL                         *WORKFILE
160900131011     c                   movel     'N'           cexLST                         *STAMPA S/N
161000131011      * i 3 ntw DPD/EEX/FED
161100131011     c                   movel     'SSS'         cexNTW                         *NETWORK SCELTI
161200131011      * Finestra di 12 mesi
161300131011     c                   if        V1Cmes < 12
161400131011     c     V1Cann        sub       1             cexDAN                         * DA ANNO
161500131011     c     V1Cmes        add       1             cexDME                         * DA MESE
161600131011     c                   else
161700131011      *    su tutto l'anno
161800131011     c                   z-add     V1Cann        cexDAN                         * DA ANNO
161900131011     c                   z-add     1             cexDME                         * DA MESE
162000140109     c                   movel     'WACEX00S'    cexAWF                         *A FILE STORICO
162100131011     c                   end
162200131011      *
162300131011     c                   z-add     V1Cann        cexANN                         *FINO A ANNO
162400131011     c                   z-add     V1Cmes        cexMES                         *FINO A MESE
162500131011      *
162600131011     c                   eval      KCOAZ = 'SE84'
162700131011     c                   eval      KPJBU = ParCEX
162800131011     c                   call      'BCH10'
162900131011     c                   parm                    KPJBA
163000131011      *
163100131011     c                   EndSr
163200120329      *------------------------------------------------------------------------*
163300120329      * LanACP - LANCIA LA CREAZIONE DEL FILE ANAGRAFICO CLIENTI COLLEGATI A
163400120329      *          POTENZIALE
163500120329      *------------------------------------------------------------------------*
163600120329     c     LanACP        BegSr
163700120329      *
163800130130     c                   movel     'MKQ6'        KCOAZ
163900120329     c                   clear                   ParACP
164000120329     c                   z-add     V1Cann        ACPann                         *Anno
164100120329     c                   z-add     V1Cmes        ACPmes                         *Mese
164200120403     c                   movel(p)  ParACP        KPJBU
164300120329     c                   call      'BCH10'
164400120329     c                   parm                    KPJBA
164500120329      *
164600120329     c                   EndSr
164700000000     C*------------------------------------------------------------------------*
164800000000     C* X12PRE - CALCOLA 12 MESI PRECEDENTI ALLA DATA
164900000000     C*------------------------------------------------------------------------*
165000000000     C     X12PRE        BEGSR
165100000000     C*--------------------
165200000000     C* DATA FINALE PRECEDENTE = DATA INIZIALE RICHIESTA - 1 MESE
165300000000     C*--------------------
165400000000     C                   Z-ADD     DELAAI        ANPAAF                         *ANNO FINALE
165500000000     C     DELMMI        SUB       1             ANPMMF                         *MESE FINALE
165600000000IF  1C     ANPMMF        IFEQ      *ZEROS                                       *SE ERA GENNAIO
165700000000     C                   Z-ADD     12            ANPMMF                         *IMPOSTO DICEMBRE
165800000000     C     DELAAI        SUB       1             ANPAAF                         *DELL'ANNO PRECEDENT
165900000000E   1C                   ENDIF
166000000000     C*--------------------
166100000000     C* DATA INIZIALE PRECEDENTE = DATA FINALE PRECEDENTE - 12 MESI
166200000000     C*--------------------
166300990609     C                   Z-ADD     ANPAAF        WAAF              4 0
166400990609     C                   Z-ADD     ANPMMF        WMMF              2 0
166500000000     C                   Z-ADD     *ZEROS        J                 4 0
166600000000     C*
166700000000     C                   Z-ADD     WAAF          ANPAAI                         *ANNO INIZIALE
166800000000DO  1C     WMMF          DOUEQ     *ZEROS
166900000000     C                   ADD       1             J
167000000000     C                   Z-ADD     WMMF          ANPMMI                         *MESE INIZIALE
167100000000     C                   SUB       1             WMMF
167200000000E   1C                   ENDDO
167300000000     C*
167400000000IF  1C     J             IFLT      12
167500000000     C                   Z-ADD     12            WMMF
167600000000     C                   SUB       1             WAAF
167700000000     C*
167800000000     C                   Z-ADD     WAAF          ANPAAI                         *ANNO INIZIALE
167900000000DO  2C     J             DOUEQ     12
168000000000     C                   ADD       1             J
168100000000     C                   Z-ADD     WMMF          ANPMMI                         *MESE INIZIALE
168200000000     C                   SUB       1             WMMF
168300000000E   2C                   ENDDO
168400000000     C*
168500000000E   1C                   ENDIF
168600000000     C*
168700000000     C                   ENDSR
168800980428     C*------------------------------------------------------------------------*
168900980428     C* LANFIM - LANCIA LA FATTURAZIONE INFRAGRUPPO MCUBI
169000980428     C*------------------------------------------------------------------------*
169100980428     C     LANFIM        BEGSR
169200980428     C***
169300980428     C* 1° LANCIO
169400980428     C***
169500980428     C                   CLEAR                   KPJBU
169600980428     C                   Z-ADD     V1CANN        FIMANN                         *DATA
169700980428     C                   Z-ADD     V1CMES        FIMMES
169800990323     C                   MOVEL     'S'           FIMSIM                         *SI SIMULAZIONE
169900980428     C                   MOVEL     'EC26'        KCOAZ
170000980428     C                   MOVEL     PARFIM        KPJBU
170100980428     C                   CALL      'BCH10'
170200980428     C                   PARM                    KPJBA
170300980428     C*
170400980428     C                   ENDSR
170500980831     C*------------------------------------------------------------------------*
170600980831     C* LANREP - REPORT COMMERCIALE
170700980831     C*------------------------------------------------------------------------*
170800980831     C     LANREP        BEGSR
170900980831     C***
171000980831     C* 1° LANCIO
171100980831     C***
171200980831     C                   CLEAR                   KPJBU
171300980831     C                   MOVEL     V1CANN        REPAMI                         *AA/MM INIZIALE
171400980902     C                   MOVE      V1CMES        REPAMI
171500980831     C                   MOVEL     V1CANN        REPAMF                         *AA/MM FINALE
171600980831     C                   MOVE      V1CMES        REPAMF
171700980903     C                   MOVEL     'ST11'        KCOAZ
171800980902     C                   MOVEL     PARREP        KPJBU
171900980831     C                   CALL      'BCH10'
172000980831     C                   PARM                    KPJBA
172100980831     C*
172200980831     C                   ENDSR
172300121127      *------------------------------------------------------------------------*
172400121127      * LANSAI - STATO APPLICAZIONI ISTAT
172500121127      *------------------------------------------------------------------------*
172600121127     c     LANSAI        BEGSR
172700121127      *
172800121127     c                   reset                   TNTA79ds
172900121127      * -?AaaaMm Iniziale?
173000121127     c                   eval      SAIdip = ( V1Cann * 100 ) + 01
173100121127      * -?AaaaMm Finale?
173200121127     c                   eval      SAIdfp = ( V1Cann * 100 ) + V1Cmes
173300121127      * -?Data Ricerca Unificante?
173400121127     c                   eval      wDate  = ( V1Cann * 10000 )
173500121127     c                                    + ( V1Cmes * 100 ) + 01
173600121127     c                   eval      wDate_Iso = %date( wDate : *iso )
173700121127     c                                       + %months(1) - %days(1)
173800121127     c                   eval      SAIdru = %dec( wDate_Iso )
173900121211      * -?Data Tariffa in vigore al?
174000121211     c                   eval      SAIdvt = SAIdru
174100121128      * -?Flag x WFIST10F?
174200121128     c*//                eval      SAI10f = 'S'        ?(già così)?
174300121127      *
174400121127      * -?Lancio Batch?
174500121127     c                   clear                   KPJBU
174600121127     c                   eval      Kcoaz = 'TA79'
174700121127     c                   eval      Kpjbu = TNTA79ds
174800121127     c                   call      'BCH10'
174900121127     c                   parm                    KPJBA
175000121127      *
175100121127     c                   ENDSR
175200130403
175300130403      *------------------------------------------------------------------------*
175400130403      * LANSFS - STATO AVANZAMENTO ADESIONE FUEL
175500130403      *------------------------------------------------------------------------*
175600130403     c     LANSFS        BEGSR
175700130403      *
175800130403     c                   reset                   TNTA77ds
175900130403      * -?AaaaMm Iniziale?
176000130403     c                   eval      SFSdip = ( V1Cann * 100 ) + 01
176100130403      * -?AaaaMm Finale?
176200130403     c                   eval      SFSdfp = ( V1Cann * 100 ) + V1Cmes
176300130403      * -?Data Ricerca Unificante?
176400130403     c                   eval      wDate  = ( V1Cann * 10000 )
176500130403     c                                    + ( V1Cmes * 100 ) + 01
176600130403     c                   eval      wDate_Iso = %date( wDate : *iso )
176700130403     c                                       + %months(1) - %days(1)
176800130403     c                   eval      SFSdru = %dec( wDate_Iso )
176900130403      * -?Data Tariffa in vigore al?
177000130403     c                   eval      SFSdvt = SFSdru
177100130403      * -?Flag x WFIST10F?
177200130403     c*//                eval      SFS10f = 'S'        ?(già così)?
177300130403      *
177400130403      * -?Lancio Batch?
177500130403     c                   clear                   KPJBU
177600130403     c                   eval      Kcoaz = 'TA77'
177700130403     c                   eval      Kpjbu = TNTA77ds
177800130403     c                   call      'BCH10'
177900130403     c                   parm                    KPJBA
178000130403      *
178100130403     c                   ENDSR
178200130403
178300980902     C*------------------------------------------------------------------------*
178400980902     C* LANCAT - C.A.T. CREAZIONE DATI TIPO
178500980902     C*------------------------------------------------------------------------*
178600980902     C     LANCAT        BEGSR
178700980902     C***
178800980902     C* 1° LANCIO
178900980902     C***
179000980902     C                   CLEAR                   KPJBU
179100980902     C                   Z-ADD     V1CANN        WANNO             4 0
179200980902     C                   Z-ADD     V1CMES        WMESE             2 0
179300980902     C                   EXSR      X06PRE                                       *CALCOLA 6 MM DIETRO
179400980902     C                   Z-ADD     ANNO          DSANN
179500980902     C                   Z-ADD     MESE          DSMES
179600980902     C                   Z-ADD     01            DSGIO
179700980902     C                   Z-ADD     DSDAT         CATDS1                         *DATA INIZIALE
179800980902     C                   Z-ADD     V1CANN        WANNO             4 0
179900980902     C                   Z-ADD     V1CMES        WMESE             2 0
180000980902     C                   EXSR      CALUGM                                       *CALCOLA ULT.GG MESE
180100980902     C                   Z-ADD     GIORNO        DSGIO
180200980902     C                   Z-ADD     V1CMES        DSMES
180300980902     C                   Z-ADD     V1CANN        DSANN
180400980902     C                   Z-ADD     DSDAT         CATDS2                         *DATA FINALE
180500980902     C                   MOVEL     'T'           CATCTO                         *TARIFFA
180600980902     C                   Z-ADD     CARKSC        CATKOV                         *CLIENTE     CARTELL
180700980902     C                   Z-ADD     CARCTR        CATCTR                         *TARIFFA     CARTELL
180800021202     C                   MOVE(P)   CARPRG        CATPRG                         *PROGRESSIVO CARTELL
180900980902     C                   Z-ADD     999           CATLNP                         *TUTTE LE LNP
181000980902     C                   MOVEL     'N'           CATSIM                         *NO SIMULAZIONE
181100980902     C                   MOVEL     'S'           CATELA                         *DATI SEDE
181200060830     C                   MOVEL     'TE6N'        KCOAZ
181300980902     C                   MOVEL     PARCAT        KPJBU
181400060830     C                   MOVE      'TNTE41C   '  KPJBU
181500980902     C                   CALL      'BCH10'
181600980902     C                   PARM                    KPJBA
181700980902     C*
181800980902     C                   ENDSR
181900060124     C*------------------------------------------------------------------------*
182000060124     C* LANric - Crea wrk-file VARIE fattura
182100060124     C*------------------------------------------------------------------------*
182200060124     C     LANric        BEGSR
182300060124     C***
182400060125     C                   clear                   parric
182500060124     C                   Z-ADD     V1CANN        aaaafat
182600060124     C                   Z-ADD     V1CMES        mmfat
182700060124     c                   movel     01            dtafat
182800060124     c                   move      mmaaaafat     dtafat            8 0
182900060125     c                   move      dtafat        dtaeur
183000060125     c                   adddur    1:*m          dtaeur
183100060125     c                   move      dtaeur        datafatfinmes
183200060124     C                   MOVEL     'N'           dettaglio
183300060124     c                   movel     v1cann        periodoce
183400060124     c                   move      v1cmes        periodoce
183500060124     C                   MOVEL     'N'           stornonacc
183600060124     C                   MOVEL     'S'           elabora_ce
183700060124     C                   MOVEL     'ST72'        KCOAZ
183800060124     C                   MOVEL(p)  PARric        KPJBU
183900060124     C                   CALL      'BCH10'
184000060124     C                   PARM                    KPJBA
184100060124     C*
184200060124     C                   ENDSR
184300980902     C*------------------------------------------------------------------------*
184400980902     C* X06PRE - CALCOLA 6 MESI PRECEDENTI ALLA DATA
184500980902     C*------------------------------------------------------------------------*
184600980902     C     X06PRE        BEGSR
184700980902     C*
184800980902     C* REIMPOSTA LE VARIABILI DI LAVORO
184900980902     C                   Z-ADD     *ZEROS        ANNO              4 0
185000980902     C                   Z-ADD     *ZEROS        MESE              2 0
185100980902     C*---
185200980902     C* DATA FINALE   = DATA CORRENTE - 1 MESE  ==-> GIA' RICHIESTA
185300980902     C* DATA INIZIALE = DATA FINALE   - 5 MESI  ==-> -5 (=-6)
185400980902     C*---
185500980902     C* DATA INIZIALE
185600980902     C     WMESE         SUB       5             MESE                           MESE - 5
185700980902IF  1C     MESE          IFGT      *ZEROS                                       .MESE > 0
185800980902     C                   Z-ADD     WANNO         ANNO                            °ANNO CORRENTE
185900980902X   1C                   ELSE
186000980902IF  2C     MESE          IFEQ      *ZEROS                                       .MESE = 0
186100980902     C     WANNO         SUB       1             ANNO                            °ANNO - 1
186200980902     C                   Z-ADD     12            MESE                            °MESE = 12
186300980902X   2C                   ELSE
186400980902IF  3C     MESE          IFLT      *ZEROS                                       .MESE < 0
186500980902     C     WANNO         SUB       1             ANNO                            °ANNO - 1
186600980902     C     12            ADD       MESE          MESE                            °MESE = 12-DIFFEREN
186700980902E   3C                   ENDIF
186800980902E   2C                   ENDIF
186900980902E   1C                   ENDIF
187000980902     C*
187100980902     C                   ENDSR
187200980902     C*------------------------------------------------------------------------*
187300980902     C* CALUGM - CALCOLA L'ULTIMO GIORNO DEL MESE
187400980902     C*------------------------------------------------------------------------*
187500980902     C     CALUGM        BEGSR
187600980902     C*
187700980902     C* REIMPOSTA LE VARIABILI DI LAVORO
187800980902     C                   Z-ADD     *ZEROS        GIORNO            2 0
187900980902     C*
188000980902IF  1C     WMESE         IFEQ      01
188100980902     C     WMESE         OREQ      03
188200980902     C     WMESE         OREQ      05
188300980902     C     WMESE         OREQ      07
188400980902     C     WMESE         OREQ      08
188500980902     C     WMESE         OREQ      10
188600980902     C     WMESE         OREQ      12
188700980902     C                   Z-ADD     31            GIORNO
188800980902E   1C                   ENDIF
188900980902IF  1C     WMESE         IFEQ      04
189000980902     C     WMESE         OREQ      06
189100980902     C     WMESE         OREQ      09
189200980902     C     WMESE         OREQ      11
189300980902     C                   Z-ADD     30            GIORNO
189400980902E   1C                   ENDIF
189500980902IF  1C     WMESE         IFEQ      02
189600980902     C     WANNO         DIV       4             N4                4 0
189700980902     C                   MVR                     RESTO             1 0
189800980902IF  2C     RESTO         IFEQ      *ZEROS
189900980902     C                   MOVEL     29            GIORNO
190000980902X   2C                   ELSE
190100980902     C                   Z-ADD     28            GIORNO
190200980902E   2C                   ENDIF
190300980902E   1C                   ENDIF
190400980902     C*
190500980902     C                   ENDSR
190600980902     C*------------------------------------------------------------------------*
190700980902     C* CARTAB  - CARICA TABELLE OCCORRENTI
190800980902     C*------------------------------------------------------------------------*
190900980902     C     CARTAB        BEGSR
191000021202     C*---
191100021202     C* TARIFFA DI CARTELLO
191200021202     C*---
191300160308     C     *LIKE         DEFINE    oc7KSCc       CARKSC
191400160308     C     *LIKE         DEFINE    Oc7CTRc       CARCTR
191500160308     C     *LIKE         DEFINE    Oc7PRGc       CARPRG
191600021202     C                   Z-ADD     *ZEROS        CARKSC
191700021202     C                   Z-ADD     *ZEROS        CARCTR
191800021202     C                   MOVE(P)   *ZEROS        CARPRG
191900160308     C                   CLEAR                   TRULC7DS
192000160308     C                   EVAL      Ic7TLA = 'L'
192100160308     c                   eval      ic7tfie='I'
192200160308     C                   EVAL      Ic7DTA = DATCOR
192300160308     C                   CALL      'TRULC7R'
192400160308     C                   PARM                    TRULC7DS
192500021202     C*
192600160308     C                   IF        Oc7ERR <> 'E'
192700160308     C                   Z-ADD     Oc7KSCc       CARKSC
192800160308     C                   Z-ADD     Oc7CTRc       CARCTR
192900160308     C                   MOVE(P)   Oc7PRGc       CARPRG
193000021202     C                   ENDIF
193100061123     c* Verifico se è stata elaborata la statistica attività commwerciale
193200061123     c*  x il report di sede
193300061123     c* apertura file in filtraxxx
193400061123    1c                   if        %subst(knsif:7:3)='201'
193500061123     c                   Eval      wlib = 'FILTRAGRU '
193600061123      *  Se sono in ambiente di prova - FILTRAPRD
193700061123     c                   Else
193800061123     c                   Eval      wlib = 'FILTRAGRPF'
193900061123    1c                   EndIf
194000061123     c
194100061123     c                   Movel(p)  cmdt(1)       comman
194200061123     c                   Eval      %Subst(comman:30:10) = wlib
194300061123     c                   Eval      comman =
194400061123     c                             %trim(comman) + '/TNTBE01L)'
194500061123     c                   Call      'QCMDEXC'                            99
194600061123     c                   Parm                    comman
194700061123     c                   Parm                    lenght
194800061123     c  n99              Open      TNTBE01L
194900061123     C
195000061123     c                   Clear                   dSdf
195100061123     C                   IF        %OPEN(TNTBE01L)
195200061123     c                   Eval      kTbeCod = 'SDF'
195300061123     c                   Movel(p)  046           kTbeKe1
195400061123     c     Ktbe          Chain     Tntbe01l
195500061123     c                   if        %found(tntbe01l)
195600061123     c                   movel     tbeuni        dsdf
195700061123     c                   endif
195800061123     c
195900061123     c                   close     tntbe01l
196000061123     c                   endif
196100980902     C*
196200980902     C                   ENDSR
196300020916     C*--------------------------------------------------------------------------------------------*
196400020916     C* REPERISCE I DATI UTENTE
196500020916     C*--------------------------------------------------------------------------------------------*
196600020916     C     REPDATIUTE    BEGSR
196700020916     C*
196800020916     C* INIZIALIZZA VARIABILI DI WRK
196900020916     C                   CLEAR                   TIBS34DS
197000020916     C                   CLEAR                   AZUTEDS
197100020916     C                   CLEAR                   DDATIUTE
197200020916     C*
197300020916     C     *DTAARA       DEFINE    §azute        azuteds
197400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
197500020916     C                   IN(E)     *DTAARA
197600020916if  1C                   IF        %Error
197700020916     c                   EVAL      I34Tla = 'L'
197800020916     C                   CALL      'TIBS34R'
197900020916     C                   PARM                    Tibs34Ds
198000020916     C                   IN        *DTAARA
198100020916e   1C                   ENDIF
198200020916     C*
198300020916     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
198400020916     C                   MOVEL(P)  RSUT          DSFIRS           20
198500020916     C*
198600020916     C                   ENDSR
198700000000     C*------------------------------------------------------------------------*
198800000000     C* *INZSR  - ROUTINE INIZIALE
198900000000     C*------------------------------------------------------------------------*
199000000000     C     *INZSR        BEGSR
199100000000     C*
199200000000     C* RICEVIMENTO PARAMETRI
199300000000     C     *ENTRY        PLIST
199400000000     C                   PARM                    KPJBA
199500021203     C*
199600021203     C                   EVAL      WKNSIF = KNSIF
199700980902     C*
199800980902     C* VARIABILI RIFERITE AL DATABASE
199900980902     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
200000980902     C     *LIKE         DEFINE    TBLCOD        KTBCOD
200100980902     C     *LIKE         DEFINE    TBLKEY        KTBKEY
200200021203     C     *LIKE         DEFINE    KNSIF         WKNSIF
200300130130     C                   Z-ADD     1             KTBKUT
200400980902     C*
200500980902     C* CHIAVI DI LETTURA
200600980902     C     KEYTAB        KLIST                                                  *TABEL00F
200700980902     C                   KFLD                    KTBKUT                          -UTENTE
200800980902     C                   KFLD                    KTBCOD                          -TABELLA
200900980902     C                   KFLD                    KTBKEY                          -CHIAVE
201000061123     c     Ktbe          Klist
201100061123     c                   Kfld                    kTbeCod
201200061123     c                   Kfld                    kTbeKe1
201300980902     C*
201400980902     C* REPERISCE LA DATA CORRENTE
201500980902     C     *LIKE         DEFINE    G08INV        DATCOR
201600980902     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
201700980902     C                   MOVE      N14           N8                8 0          *DATA (8) IN G/M/AA
201800980902     C                   Z-ADD     N8            G08DAT
201900980902     C                   Z-ADD     *ZEROS        G08INV
202000980902     C                   MOVEL     '0'           G08ERR
202100980902     C                   CALL      'XSRDA8'
202200980902     C                   PARM                    WLBDA8
202300980902     C                   Z-ADD     G08INV        DATCOR                         *DATA CORRENTE AA/M/
202400000000     C*
202500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
202600000000     C                   EXSR      INZSTD
202700980902     C*
202800980902     C* CARICAMENTO TABELLE OCCORRENTI
202900980902     C                   EXSR      CARTAB
203000980902     C*
203100980902     C* IMPOSTAZIONE INIZIALE VIDEO
203200980902     C                   EXSR      IMPVID
203300000000     C*
203400000000     C* EMETTO FORMATO VIDEO
203500000000     C                   EXSR      CARV1                                        *EMISSIONE VIDEO
203600000000     C*
203700000000     C                   ENDSR
203800000000     C*------------------------------------------------------------------------*
203900000000     C* IMPVID - IMPOSTAZIONI INIZIALI VIDEO
204000000000     C*------------------------------------------------------------------------*
204100000000     C     IMPVID        BEGSR
204200000000     C*--------------------
204300000000     C* ANNO/MESE (ANNO/MESE CORRENTE - 1)
204400000000     C*--------------------
204500990609     C                   Z-ADD     DATCOR        DSDAT
204600990609     C                   Z-ADD     DSANN         AAAA              4 0
204700990609     C                   Z-ADD     DSMES         MMMM              2 0
204800000000     C                   SUB       1             MMMM
204900000000IF  1C     MMMM          IFEQ      *ZEROS                                       *SE GENNAIO
205000000000     C                   Z-ADD     12            MMMM                           *IMPOSTO DICEMBRE
205100000000     C                   SUB       1             AAAA                           *DELL'ANNO PRECEDENT
205200000000E   1C                   ENDIF
205300000000     C                   Z-ADD     AAAA          V1CANN
205400000000     C                   Z-ADD     MMMM          V1CMES
205500000000     C*--------------------
205600000000     C* SCELTA STATISTICHE
205700000000     C*--------------------
205800151001     C                   MOVEL     ' '           V1CVOL
205900000000     C                   MOVEL     '1'           V1CAFF
206000160509     C***                MOVEL     ' '           V1CAFR
206100970827     C                   MOVEL     '1'           V1CTRA
206200000000     C                   MOVEL     '1'           V1CTRS
206300020118     C*                  MOVEL     '1'           V1CRIT
206400000000     C                   MOVEL     '1'           V1CMOV
206500000000     C                   MOVEL     '1'           V1CAGG
206600980407     C                   MOVEL     '1'           V1CTSF
206700000000     C                   MOVEL     '1'           V1CDEL
206800131011     C                   MOVEL     '1'           V1CCEX
206900120329     C                   MOVEL     '1'           V1CACP
207000981113     C                   MOVEL     '1'           V1CREP
207100981013     C                   MOVEL     '1'           V1CCAT
207200990903     C                   MOVEL     '1'           V1CFIM
207300151001     C                   MOVEL     ' '           V1CPES
207400081128     C                   MOVEL     '1'           V1Cqua
207500121127     C                   MOVEL     '1'           V1Csai
207600130130     C                   MOVEL     '1'           V1Cdfp
207700130403     C                   MOVEL     '1'           V1Csfs
207800000000     C*
207900000000     C                   ENDSR
208000000000     C*------------------------------------------------------------------------*
208100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
208200000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
208300000000     C*      LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
208400000000     C*      STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
208500000000     C*      NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
208600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
208700000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
208800000000     C*------------------------------------------------------------------------*
208900000000     C* INZSTD - ROUTINE INIZIALE STANDARD
209000000000     C*------------------------------------------------------------------------*
209100000000     C     INZSTD        BEGSR
209200000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
209300000000     C                   CALL      'XKEYS'
209400000000     C                   PARM                    DSKEYS
209500000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
209600000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
209700000000     C     XMSGPA        PLIST
209800000000     C                   PARM                    DSMSMS
209900000000     C                   PARM                    DSMSPG
210000000000     C                   PARM                    DSMSKE
210100000000     C                   PARM                    DSMSER
210200000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
210300020118     C                   MOVEL     'TISE01R'     DSMSPG                         *NOME PGM CHIAMANTE
210400020118     C                   MOVEL     'TISE01R'     VMCQUE                         *CODA MESSAGGI DESTI
210500000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
210600000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
210700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
210800000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
210900000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
211000000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
211100000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
211200000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
211300000000     C                   MOVEL     '1'           $TESVD            1            *1=RIEMETT.TEST.VIDE
211400000000     C                   MOVEL     'I'           $TGES             1            *I=INTER. G=GESTIONE
211500000000     C* LANCIO ROUTINE PER VERIFICARE SE
211600000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
211700000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
211800020916     C                   EXSR      REPDATIUTE
211900000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
212000000000     C                   BITOFF    '01234567'    POSMI1            1
212100000000     C                   BITOFF    '01234567'    POSMI2            1
212200000000     C                   BITOFF    '01234567'    POSMA1            1
212300000000     C                   BITOFF    '01234567'    POSMA2            1
212400000000     C                   BITON     '34'          POSMI1            1            *18
212500000000     C                   BITON     '7'           POSMI2            1            *01
212600000000     C                   BITON     '34'          POSMA1            1            *18
212700000000     C                   BITON     '0'           POSMA2            1            *80
212800000000     C                   MOVE      POSMI2        POSMIN            2
212900000000     C                   MOVEL     POSMI1        POSMIN
213000000000     C                   MOVE      POSMA2        POSMAX            2
213100000000     C                   MOVEL     POSMA1        POSMAX
213200000000     C                   ENDSR
213300000000     C*------------------------------------------------------------------------*
213400000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
213500000000     C*------------------------------------------------------------------------*
213600000000     C     FUNSTD        BEGSR
213700000000     C                   SELECT
213800000000     C     KEY           WHENEQ    F3
213900000000     C                   EXSR      F03STD
214000000000     C     KEY           WHENEQ    F12
214100000000     C                   EXSR      F12STD
214200000000     C     KEY           WHENEQ    F24
214300000000     C                   EXSR      F24STD
214400000000     C                   OTHER
214500000000     C                   MOVEL     MSG(2)        $MSG
214600000000     C                   EXSR      CARMSG
214700000000     C                   ENDSL
214800000000     C                   ENDSR
214900000000     C*------------------------------------------------------------------------*
215000000000     C* F03STD - GEST. TASTO FUNZ. F03 STANDARD ==> FINE
215100000000     C*------------------------------------------------------------------------*
215200000000     C     F03STD        BEGSR
215300000000     C*
215400000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
215500000000     C                   MOVEL     '1'           $FINE
215600000000     C*
215700000000     C                   ENDSR
215800000000     C*------------------------------------------------------------------------*
215900000000     C* F12STD - GEST. TASTO FUNZ. F12 STANDARD ==> FINE
216000000000     C*------------------------------------------------------------------------*
216100000000     C     F12STD        BEGSR
216200000000     C*
216300000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
216400000000     C                   MOVEL     '1'           $FINE
216500000000     C*
216600000000     C                   ENDSR
216700000000     C*------------------------------------------------------------------------*
216800000000     C* F24STD - GEST. TASTO FUNZ. F24 STANDARD ==> ALTRI TASTI
216900000000     C*------------------------------------------------------------------------*
217000000000     C     F24STD        BEGSR
217100000000     C*
217200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
217300000000     C                   EXSR      OUTFUN
217400000000     C*
217500000000     C                   ENDSR
217600000000     C*------------------------------------------------------------------------*
217700000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
217800000000     C*          CODA MESSAGGI DEL PGM
217900000000     C*------------------------------------------------------------------------*
218000000000     C     CARMSG        BEGSR
218100000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
218200000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
218300000000     C     MSGULT        IFEQ      0
218400000000     C                   MOVEL     '1'           *IN40
218500020118     C                   WRITE     SE01DVK
218600000000     C                   MOVEL     '0'           *IN40
218700000000     C                   ENDIF
218800000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
218900000000     C                   MOVEL     $MSG          DSMSMS
219000000000     C                   CALL      'XMSG'        XMSGPA
219100000000     C     DSMSER        IFEQ      0
219200000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
219300000000     C                   ENDIF
219400000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
219500000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
219600020118     C                   WRITE     SE01DVM                                      *SFL MESS
219700000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
219800000000     C                   MOVEL     *BLANKS       $MSG
219900000000     C                   ENDSR
220000000000     C*------------------------------------------------------------------------*
220100000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
220200000000     C*------------------------------------------------------------------------*
220300000000     C     OUTMSG        BEGSR
220400000000     C* VISUALIZZAZIONE MESSAGGIO
220500000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
220600020118     C                   WRITE     SE01DVK                                      *CONTR MESS
220700000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
220800000000     C                   Z-ADD     *ZEROS        MSGULT
220900000000     C                   ENDSR
221000000000     C*------------------------------------------------------------------------*
221100000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
221200000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
221300000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
221400000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
221500000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
221600000000     C*------------------------------------------------------------------------*
221700000000     C     CARFUN        BEGSR
221800000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
221900000000     C     1             DO        31            I                 5 0          --- 1 -->
222000000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
222100000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
222200000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
222300020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
222400000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
222500000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
222600000000     C                   ELSE                                                   *INTERROG.
222700000000     C                   MOVEL     FUNASI        FAT(I)
222800000000     C                   ENDIF
222900000000     C                   ELSE                                                   <-- 2 --> *FILIALE
223000000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
223100000000     C                   MOVEL     FUNAFG        FAT(I)
223200000000     C                   ELSE                                                   *INTERROG.
223300000000     C                   MOVEL     FUNAFI        FAT(I)
223400000000     C                   ENDIF
223500000000     C                   ENDIF                                                  <-- 2 ---
223600000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
223700000000     C                   MOVEL     USER          WEDP              3            *PROFILO
223800000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
223900000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
224000000000     C                   ELSE
224100000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
224200000000     C                   ENDIF
224300000000     C                   ENDIF
224400000000     C                   ENDDO                                                  <-- 1 ---
224500000000     C                   ENDSR
224600000000     C*------------------------------------------------------------------------*
224700000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
224800000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
224900000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
225000000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
225100000000     C*------------------------------------------------------------------------*
225200000000     C     OUTFUN        BEGSR
225300000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
225400000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
225500000000     C                   ENDDO
225600000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
225700000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
225800000000     C     1             DO        31            I
225900000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
226000000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
226100000000     C                   ADD       1             FUNNAT
226200000000     C                   ENDIF
226300000000     C                   ENDDO
226400000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
226500000000     C     FUNNAT        IFGT      8
226600000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
226700000000     C                   ENDIF
226800000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
226900000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
227000000000     C                   Z-ADD     8             FUNVIS            5 0
227100000000     C     FAT(23)       IFEQ      'S'
227200000000     C                   SUB       1             FUNVIS
227300000000     C                   ENDIF
227400000000     C     FAT(24)       IFEQ      'S'
227500000000     C                   SUB       1             FUNVIS
227600000000     C                   ENDIF
227700000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
227800000000     C                   Z-ADD     *ZEROS        W                 5 0
227900000000     C                   ADD       1             FUNULT
228000000000     C     FUNULT        IFGT      31
228100000000     C                   Z-ADD     1             FUNULT
228200000000     C                   ENDIF
228300000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
228400000000     C     FUNULT        DO        31            I                              --- 1 -->
228500000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
228600000000     C     W             IFLT      FUNVIS                                       --- 2 -->
228700000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
228800000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
228900000000     C     FDE(I)        ANDNE     *BLANKS
229000000000     C     I             IFEQ      23                                           *TOLGO F23
229100000000     C     I             OREQ      24                                           *E F24
229200000000     C                   ELSE
229300000000     C                   ADD       1             W
229400000000     C                   MOVEL     FDE(I)        FUV(W)
229500000000     C                   ENDIF
229600000000     C                   ENDIF                                                  <-- 3 ---
229700000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
229800000000     C                   Z-ADD     I             FUNULT
229900000000     C                   ENDIF                                                  <-- 2 ---
230000000000     C                   ENDDO                                                  <-- 1 ---
230100000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
230200000000     C                   MOVEL     FUV(1)        VFD001
230300000000     C                   MOVEL     FUV(2)        VFD002
230400000000     C                   MOVEL     FUV(3)        VFD003
230500000000     C                   MOVEL     FUV(4)        VFD004
230600000000     C                   MOVEL     FUV(5)        VFD005
230700000000     C                   MOVEL     FUV(6)        VFD006
230800000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
230900000000     C     FAT(24)       ANDEQ     'N'
231000000000     C                   MOVEL     FUV(7)        VFD007
231100000000     C                   MOVEL     FUV(8)        VFD008
231200000000     C                   ELSE                                                   <-- 1 -->
231300000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
231400000000     C     FAT(24)       ANDEQ     'S'
231500000000     C                   MOVEL     FDE(23)       VFD007
231600000000     C                   MOVEL     FDE(24)       VFD008
231700000000     C                   ELSE                                                   <-- 2 -->
231800000000     C                   MOVEL     FUV(7)        VFD007
231900000000     C     FAT(23)       IFEQ      'S'
232000000000     C                   MOVEL     FDE(23)       VFD008
232100000000     C                   ELSE
232200000000     C                   MOVEL     FDE(24)       VFD008
232300000000     C                   ENDIF
232400000000     C                   ENDIF                                                  <-- 2 ---
232500000000     C                   ENDIF                                                  <-- 1 ---
232600000000     C                   MOVEL     '0'           $TESVD                         *REIMPOSTA EMIS TEST
232700000000     C                   ENDSR
232800000000     C*------------------------------------------------------------------------*
232900000000     C* CONFUN - CONTROLLA SE LA FUNZIONE SCELTA E' ATTIVA
233000000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
233100000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
233200000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
233300000000     C*------------------------------------------------------------------------*
233400000000     C     CONFUN        BEGSR
233500000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
233600000000     C     1             DO        31            I                              --- 1 -->
233700000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
233800000000     C* CONTROLLO SE IL TASTO E' ATTIVO
233900000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
234000000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
234100000000     C                   EXSR      CARMSG
234200000000     C                   MOVEL     '1'           $ERR
234300000000     C                   ENDIF                                                  <-- 3 ---
234400000000     C                   ENDIF                                                  <-- 2 ---
234500000000     C                   ENDDO                                                  <-- 1 ---
234600000000     C                   ENDSR
234700000000     O*------------------------------------------------------------------------*
234800000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
234900020118TISE01R- Tasto funzionale non valido.                                          1
235000020118TISE01R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
235100020118TISE01R- Opzione non valida.                                                   3
235200020118TISE01R- Opzione abilitata ma non gestita. Avvertire CED.                      4
235300020118TISE01R- Inizio elenco.                                                        5
235400020118TISE01R- Fine elenco.                                                          6
235500020118TISE01R- Non si possono lanciare le statistiche di gruppo in SDI !!!           7
235600020118TISE01R- Anno errato                                                           8
235700020118TISE01R- Mese errato                                                           9
235800020118TISE01R- Scelta errata                                                        10
235900020118TISE01R- Impossibile lanciare questo programma da un s.informativo di prova   11
236000061123Impossibile lancio Report Commerciale: mancano dati di filiale (TNTA66R)
236100160224Ci sono Richieste note debito da contabilizzare prima di aggiornare i SALDI   13
236200020118TISE01R- 14
236300020118TISE01R- 15
236400020118TISE01R- 16
236500020118TISE01R- 17
236600020118TISE01R- 18
236700020118TISE01R- 19
236800020118TISE01R- 20
236900000000** FUN  (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
237000000000F1    -                   -    NN     NN
237100000000F2    -                   -    NN     NN
237200000000F3    -F3=Fine            -    SS     SS
237300000000F4    -                   -    NN     NN
237400000000F5    -                   -    NN     NN
237500000000F6    -F6=Lancio          -    SS     SS
237600000000F7    -                   -    NN     NN
237700000000F8    -                   -    NN     NN
237800000000F9    -                   -    NN     NN
237900000000F10   -                   -    NN     NN
238000000000F11   -                   -    NN     NN
238100000000F12   -F12-Ritorno        -    SS     SS
238200000000F13   -                   -    NN     NN
238300000000F14   -                   -    NN     NN
238400000000F15   -                   -    NN     NN
238500000000F16   -                   -    NN     NN
238600000000F17   -                   -    NN     NN
238700000000F18   -                   -    NN     NN
238800000000F19   -                   -    NN     NN
238900000000F20   -                   -    NN     NN
239000000000F21   -                   -    NN     NN
239100000000F22   -                   -    NN     NN
239200000000F23   -                   -    NN     NN
239300000000F24   -F24=Altri tasti    -    NN     NN
239400000000ENTER -                   -    SS     SS
239500000000ROLLUP-                   -    NN     NN
239600000000ROLLDN-                   -    NN     NN
239700000000HELP  -                   -    NN     NN
239800000000PRINT -                   -    NN     NN
239900000000BSPACE-                   -    NN     NN
240000000000CLEAR -                   -    NN     NN
240100061123**
240200061123OVRDBF FILE(TNTBE01L) TOFILE(
