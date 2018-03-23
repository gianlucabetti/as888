000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200951004     H*PARMS CVTOPT(*DATETIME)
000300940223     H DECEDIT('0,') DATEDIT(*DMY.)
000400940127      *----------*
000500950124      * ANA430R  *
000600940127      *----------*
000700940117      *
000800950228      *            BOTTONI CHIAMATA ANAGRAFICA FORNITORI
000900940223      *
001000940223      *
001100940223      * DOCUMENTI MICROANALISI :
001200940223      *
001300940223      * NOTE TECNICHE :
001400940223      *
001500940223      *
001600940223      * PERSONALIZZAZIONI
001700940223      *
001800940223      *
001900940223      *
002000940223      *               MAPPA INDICATORI STD
002100940223      *
002200940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
002300940223      *  21           GENERICO OPERAZIONI I/O
002400940223      *  22           GENERICO ERRORE OPERAZIONI I/O
002500940223      *  30           SFLDSP
002600940223      * N31           SFLCLR
002700940128      *  31           SFLDSPCTL
002800940128      *  32           SFLNXTCHG
002900940128      *  33           SFLEND
003000940128      *  39           OF PRTF
003100940315      *  40 <---> 49  DSPATR ERRORI SU SFL
003200940616      *  Specificare l'uso dei singoli indicatori
003300940315      *  50 <---> 98  ERRORI SU VIDEO
003400940616      *  Specificare l'uso dei singoli indicatori
003500940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
003600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003700940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003800940128      *  99           INDIC. GENERALE DI ERRORE
003900940128     F*----------------------------------------------------*
004000950124     FANA430D   CF   E             WORKSTN USROPN
004100950124     F                                     RENAME(A430D1:FMTD1)
004200950124     F                                     RENAME(A430Z1:FMTZ1)
004300950313     F                                     RENAME(A430D2:FMTD2)
004400940201     F                                     INFDS(DSFMT)
004500940128     D*----------------------------------------------------*
004600940506     D* Schiere per la gestione dei tasti di funzione
004700940506     D $FM             S             27    DIM(24)
004800940506     D $FX             S             50    DIM(24)
004900940506     D $FC             S              1    DIM(24)
005000940506     D $FR             S              1    DIM(24)
005100950313     D* Video D1/D2
005200940506     D $FM1            S             27    DIM(24) CTDATA PERRCD(1)
005300940506     D $FX1            S             50    DIM(24) ALT($FM1)
005400940506     D $FC1            S              1    DIM(24) CTDATA PERRCD(1)
005500940506     D $FR1            S              1    DIM(24) ALT($FC1)
005600941219     D*
005700010831G1036D* (ora è possibile gestire più bottoni)
005800010831  "  D* nr.bottoni (dimensione schiera KH)
005900010831  "  D NB1             C                   CONST(17)
006000010831  "  D* nr.bottoni (dimensione schiera KH1)
006100010831  "  D NB2             C                   CONST(5)
006200010831  "  D*
006300010831  "  D* Modificate le dimensioni delle schiere per poter gestire un
006400010830  "  D* maggior numero di bottoni
006500010831  "  D MIX             S             37    DIM(NB1)
006600010831  "  D XIM             S              7    DIM(NB1)
006700010831  "  D KH              S              2  0 DIM(NB1)
006800010830  "  D WMIX            S                   like(MIX)
006900010830  "  D* contabilità
007000010831  "  D MIX1            S             37    DIM(NB2)
007100010831  "  D XIM1            S              7    DIM(NB2)
007200010831  "  D KH1             S              2  0 DIM(NB2)
007300010830  "  D WMIX1           S                   like(MIX1)
007400010831  "  D*
007500010831  "  D D1D             S             30    DIM(NB1)
007600010831  "  D* contabilità
007700010831  "  D D2D             S             30    DIM(NB2)
007800010830  "  D*
007900010831  "  D PG              S              4    DIM(NB1) CTDATA PERRCD(1)            Programmi
008000010831  "  D PGT             S             27    DIM(NB1) ALT(PG)
008100010831  "  D DCB             S             30    DIM(NB1)                             Deco bottone
008200010831  "  D* contabilità
008300010831  "  D PG1             S              4    DIM(NB2) CTDATA PERRCD(1)            Programmi CTB
008400010831  "  D PGT1            S             27    DIM(NB2) ALT(PG1)
008500010831  "  D DCB1            S             30    DIM(NB2)                             Deco bottone CTB
008600010830  "  D*
008700010830  "  X*** MIX             S             37    DIM(14)
008800010830  "  X*** XIM             S              7    DIM(14)
008900010830  "  X*** KH              S              2  0 DIM(14)
009000010830  "  X*** WMIX            S                   like(MIX)
009100010830  "  X*** contabilità
009200010830  "  X*** MIX1            S             37    DIM(5)
009300010830  "  X*** XIM1            S              7    DIM(5)
009400010830  "  X*** KH1             S              2  0 DIM(5)
009500010830  "  X*** WMIX1           S                   like(MIX1)
009600010830  "  X***
009700010830  "  X*** D1D             S             30    DIM(14)
009800010831  "  X*** contabilità
009900010831  "  X*** D2D             S             30    DIM(5)
010000010831  "  X***
010100010830  "  X*** PG              S              4    DIM(14) CTDATA PERRCD(1)          Programmi
010200010830  "  X*** PGT             S             27    DIM(14) ALT(PG)
010300010831  "  X*** DCB             S             30    DIM(14)                           Deco bottone
010400010831  "  X*** contabilità
010500010831  "  X*** PG1             S              4    DIM(5) CTDATA PERRCD(1)           Programmi CTB
010600010831  "  X*** PGT1            S             27    DIM(5) ALT(PG1)
010700010831G1036X*** DCB1            S             30    DIM(5)                            Deco bottone CTB
010800010830     D*
010900050629B0818X*BOT             S             27    DIM(5) CTDATA PERRCD(1)
011000050629B0818D BOT             S             27    DIM(8) CTDATA PERRCD(1)
011100941228     D*-------------
011200950118     D* Bottoni a video
011300010830G1036D* (aggiunti nuovi bottoni gestiti)
011400950118     D                 DS
011500950118     D  D1B01                  1      1  0
011600950118     D  D1B02                  2      2  0
011700950118     D  D1B03                  3      3  0
011800950118     D  D1B04                  4      4  0
011900950118     D  D1B05                  5      5  0
012000950118     D  D1B06                  6      6  0
012100950118     D  D1B07                  7      7  0
012200950118     D  D1B08                  8      8  0
012300950118     D  D1B09                  9      9  0
012400950118     D  D1B10                 10     10  0
012500951010     D  D1B11                 11     11  0
012600970919     D  D1B12                 12     12  0
012700980625     D  D1B13                 13     13  0
012800980701     D  D1B14                 14     14  0
012900010830G1036D  D1B15                 15     15  0
013000010830  "  D  D1B16                 16     16  0
013100010830  "  D  D1B17                 17     17  0
013200010830  "  D  D1B                    1     17  0
013300010830  "  D                                     DIM(17)                              Bottone
013400010830  "  X***  D1B                    1     14  0
013500010830G1036X***                                  DIM(14)                              Bottone
013600021017A0973D*-------------
013700021017  "  D* Campi attributi per bottoni a video
013800021017  "  D                 DS
013900021017  "  D  D1B01A                 1      1
014000021017  "  D  D1B02A                 2      2
014100021017  "  D  D1B03A                 3      3
014200021017  "  D  D1B04A                 4      4
014300021017  "  D  D1B05A                 5      5
014400021017  "  D  D1B06A                 6      6
014500021017  "  D  D1B07A                 7      7
014600021017  "  D  D1B08A                 8      8
014700021017  "  D  D1B09A                 9      9
014800021017  "  D  D1B10A                10     10
014900021017  "  D  D1B11A                11     11
015000021017  "  D  D1B12A                12     12
015100021017  "  D  D1B13A                13     13
015200021017  "  D  D1B14A                14     14
015300021017  "  D  D1B15A                15     15
015400021017  "  D  D1B16A                16     16
015500021017  "  D  D1B17A                17     17
015600021017  "  D  D1BA                   1     17
015700021017  "  D                                     DIM(17)                              Bottone
015800021017  "  D*-------------
015900021017  "  D* Attributi video
016000021017  "  D Normale         C                   Const(X'32')
016100021017A0973D Protetto        C                   Const(X'A0')
016200950313     D*-------------
016300950313     D* Bottoni a video
016400950313     D                 DS
016500950313     D  D2B01                  1      1  0
016600950313     D  D2B02                  2      2  0
016700950313     D  D2B03                  3      3  0
016800950313     D  D2B04                  4      4  0
016900950313     D  D2B05                  5      5  0
017000950313     D  D2B                    1      5  0
017100950313     D                                     DIM(5)                               Bottone
017200021017A0973D*-------------
017300021017  "  D* Campi attributi per bottoni a video
017400021017  "  D                 DS
017500021017  "  D  D2B01A                 1      1
017600021017  "  D  D2B02A                 2      2
017700021017  "  D  D2B03A                 3      3
017800021017  "  D  D2B04A                 4      4
017900021017  "  D  D2B05A                 5      5
018000021017  "  D  D2BA                   1      5
018100021017A0973D                                     DIM(5)                               Bottone
018200950118     D*-------------
018300950118     D* Testo Bottoni a video
018400010830G1036D* (aggiunti nuovi bottoni gestiti)
018500950118     D                 DS
018600950118     D  D1T01                  1     10
018700950118     D  D1T02                 11     20
018800950118     D  D1T03                 21     30
018900950118     D  D1T04                 31     40
019000950118     D  D1T05                 41     50
019100950118     D  D1T06                 51     60
019200950118     D  D1T07                 61     70
019300950118     D  D1T08                 71     80
019400950118     D  D1T09                 81     90
019500950118     D  D1T10                 91    100
019600951010     D  D1T11                101    110
019700970919     D  D1T12                111    120
019800980624     D  D1T13                121    130
019900980624     D  D1T14                131    140
020000010830G1036D  D1T15                141    150
020100010830  "  D  D1T16                151    160
020200010830  "  D  D1T17                161    170
020300010830  "  D  D1T                    1    170
020400010830  "  D                                     DIM(17)                              Testo bottone
020500010830  "  X***  D1T                    1    140
020600010830G1036X***                                     DIM(14)                           Testo bottone
020700950313     D*-------------
020800950313     D* Testo Bottoni a video
020900950313     D                 DS
021000950313     D  D2T01                  1     10
021100950313     D  D2T02                 11     20
021200950313     D  D2T03                 21     30
021300950313     D  D2T04                 31     40
021400950313     D  D2T05                 41     50
021500950313     D  D2T                    1     50
021600950313     D                                     DIM(5)                               Testo bottone
021700951004     D*-------------
021800010830G1036D* (ridimensionate le schiere per gestire più bottoni)
021900010830  "  D WD1B            S              1  0                                      Work Bottone
022000010830  "  D                                     DIM(20)                              Testo bottone
022100010830  "  D WD1T            S             10                                         work Testo bot
022200010830  "  D                                     DIM(20)                              Testo bottone
022300010830  "  D*-------------
022400010830  "  D WD2B            S              1  0                                      Work Bottone
022500010830  "  D                                     DIM(9)                               Testo bottone
022600010830  "  D WD2T            S             10                                         work Testo bot
022700010830  "  D                                     DIM(9)                               Testo bottone
022800010830  "  X*** WD1B            S              1  0                                   Work Bottone
022900010830  "  X***                                     DIM(14)                           Testo bottone
023000010830  "  X*** WD1T            S             10                                      work Testo bot
023100010830  "  X***                                     DIM(14)                           Testo bottone
023200010830  "  X*-------------
023300010830  "  X*** WD2B            S              1  0                                   Work Bottone
023400010830  "  X***                                     DIM(5)                            Testo bottone
023500010830  "  X*** WD2T            S             10                                      work Testo bot
023600010830G1036X***                                     DIM(5)                            Testo bottone
023700010830     D*-------------
023800950118     D* Testo stato Bottoni
023900950118     D                 DS
024000950118     D  INSER                  1     10
024100950118     D  PRES                  11     20
024200950118     D  CORR                  21     30
024300950118     D  ANNUL                 31     40
024400950118     D  NOAUT                 41     50
024500050629B0818D  COMM                  51     60
024600050629  "  D  FACT                  61     70
024700050629  "  D  ASSI                  71     80
024800050629  "  x**BTD                    1     50
024900050629  "  x**                                   DIM(5) ALT(BOT)
025000050629  "  D  BTD                    1     80
025100050629B0818D                                     DIM(8) ALT(BOT)
025200950118     D*-------------
025300940127     D* Passaggio Parametri
025400940127     D KPJBA         E DS
025500941205     D*-------------
025600950103     D* Reperimento Autorizzazioni
025700950103     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
025800970217     D*-------------
025900970217     D* Parametri gestione ANFRN00F
026000970217     D P020DS        E DS                  EXTNAME(ANP020DS)
026100980710     D P033DS        E DS                  EXTNAME(ANP033DS)
026200980710     D*-------------
026300980630     D* Ventilazione commenti su società collegate al gruppo
026400980630     D X53AMCds      E DS                  inz
026500980630     D  RETamc       E                     inz(*off)
026600980630     D  OPRamc       E                     inz(*off)
026700980630     D  ERRamc       E                     inz(*off)
026800980630     D  CMTamc       E                     inz(*on)
026900980630     D  AGG                   40     64    dim(25) inz(*off)
027000970217     D*-------------
027100970217     D XPAOUT          DS          2000
027200970217     D*-------------
027300970217     D* Parametri per programma di interfaccia altri sistemi informativi
027400970217     D Ana000ds      E DS
027500950103     D*-------------
027600941205     D* Ricezione Parametri
027700010830G1036D* (la dimensione delle schiere è aumentata per poter gestire
027800010830G1036D*  più bottoni)
027900950124     D A430DS        E DS                  EXTNAME(ANA430DS)
028000010830G1036D  CHI          E                     EXTFLD(CHI430)
028100010830  "  D                                     DIM(20)
028200010830  "  D  CHM          E                     EXTFLD(CHM430)
028300010830  "  D                                     DIM(20)
028400010830  "  X***  CHI          E                     EXTFLD(CHI430)
028500010830  "  X***                                     DIM(14)
028600010830  "  X***  CHM          E                     EXTFLD(CHM430)
028700010830G1036X***                                     DIM(14)
028800950124     D  B01430       E                     EXTFLD(EXITCM430)
028900950124     D  E01430       E                     EXTFLD(ERRTCM430)
029000950124     D  B02430       E                     EXTFLD(EXIRCO430)
029100950124     D  E02430       E                     EXTFLD(ERRRCO430)
029200950124     D  B04430       E                     EXTFLD(EXIAGE430)
029300950124     D  E04430       E                     EXTFLD(ERRAGE430)
029400010830G1036D  CNI          E                     EXTFLD(CNI430)
029500010830  "  D                                     DIM(9)
029600010830  "  D  CNM          E                     EXTFLD(CNM430)
029700010830  "  D                                     DIM(9)
029800010830  "  X***  CNI          E                     EXTFLD(CNI430)
029900010830  "  X***                                     DIM(5)
030000010830  "  X***  CNM          E                     EXTFLD(CNM430)
030100010830G1036X***                                     DIM(5)
030200941205     D*-------------
030300950119     D* Richiamo pgm Appoggi bancari
030400980115     D A026DS        E DS                  EXTNAME(ANA026DS) INZ
030500950119     D  RET026       E                     INZ('0')
030600950119     D  OPR026       E                     INZ('0')
030700950119     D  ERR026       E                     INZ('0')
030800950119     D*-------------
030900950119     D* Richiamo pgm Dati anagrafici
031000950119     D A041DS        E DS                  EXTNAME(ANA041DS) INZ
031100950119     D  RET041       E                     INZ('0')
031200950119     D  OPR041       E                     INZ('0')
031300950119     D  ERR041       E                     INZ('0')
031400951010     D*-------------
031500950313     D* Richiamo pgm regole contabili
031600950313     D A204DS        E DS                  EXTNAME(ANA204DS) INZ
031700950313     D  RET204       E                     INZ('0')
031800950313     D  OPR204       E                     INZ('0')
031900950313     D  ERR204       E                     INZ('0')
032000950313     D*-------------
032100950313     D* Richiamo pgm attributi contabili
032200950313     D A205DS        E DS                  EXTNAME(ANA205DS) INZ
032300950313     D  RET205       E                     INZ('0')
032400950313     D  OPR205       E                     INZ('0')
032500950313     D  ERR205       E                     INZ('0')
032600951010     D*-------------
032700950313     D* Richiamo pgm definizione capoconto
032800950313     D A206DS        E DS                  EXTNAME(ANA206DS) INZ
032900950313     D  RET206       E                     INZ('0')
033000950313     D  OPR206       E                     INZ('0')
033100950313     D  ERR206       E                     INZ('0')
033200951010     D*-------------
033300950313     D* Richiamo pgm percipienti
033400950313     D A207DS        E DS                  EXTNAME(ANA207DS) INZ
033500950313     D  RET207       E                     INZ('0')
033600950313     D  OPR207       E                     INZ('0')
033700950313     D  ERR207       E                     INZ('0')
033800950615     D*-------------
033900950615     D* Richiamo pgm gestione voci analitica
034000950615     D A280DS        E DS                  EXTNAME(ANA280DS) INZ
034100950615     D  RET280       E                     INZ('0')
034200950615     D  OPR280       E                     INZ('0')
034300950615     D  ERR280       E                     INZ('0')
034400941219     D*-------------
034500941219     D* Richiamo pgm Dati commerciali generali
034600950124     D A401DS        E DS                  EXTNAME(ANA401DS) INZ
034700950124     D  RET401       E                     INZ('0')
034800950124     D  OPR401       E                     INZ('0')
034900950124     D  ERR401       E                     INZ('0')
035000950124     D  RIP401       E                     INZ('1')
035100941219     D*-------------
035200950104     D* Richiamo pgm Filiali
035300950124     D A405DS        E DS                  EXTNAME(ANA405DS) INZ
035400950124     D  RET405       E                     INZ('0')
035500950124     D  OPR405       E                     INZ('0')
035600950124     D  ERR405       E                     INZ('0')
035700950206     D*-------------
035800970919     D* Richiamo pgm Agenti generali
035900950206     D A441DS        E DS                  EXTNAME(ANA441DS) INZ
036000950206     D  RET441       E                     INZ('0')
036100950206     D  OPR441       E                     INZ('0')
036200950206     D  ERR441       E                     INZ('0')
036300950227     D*-------------
036400970919     D* Richiamo pgm Agenti per unità
036500970919     D A442DS        E DS                  EXTNAME(ANA442DS) INZ
036600970919     D  RET442       E                     INZ('0')
036700970919     D  OPR442       E                     INZ('0')
036800970919     D  ERR442       E                     INZ('0')
036900970924     D  RIP442       E                     INZ('1')
037000970919     D*-------------
037100950227     D* Richiamo pgm Spedizionieri
037200950227     D A048DS        E DS                  EXTNAME(ANA048DS) INZ
037300950227     D  RET048       E                     INZ('0')
037400950227     D  OPR048       E                     INZ('0')
037500950227     D  ERR048       E                     INZ('0')
037600950104     D*-------------
037700950113     D* Richiamo pgm periodi esclusione pagamenti
037800950124     D A315DS        E DS                  EXTNAME(ANA315DS) INZ
037900950113     D  RET315       E                     INZ('0')
038000950113     D  OPR315       E                     INZ('0')
038100950113     D  ERR315       E                     INZ('0')
038200950113     D*-------------
038300950113     D* Richiamo pgm Dati statistici
038400950124     D A409DS        E DS                  EXTNAME(ANA409DS) INZ
038500950124     D  RET409       E                     INZ('0')
038600950124     D  OPR409       E                     INZ('0')
038700950124     D  ERR409       E                     INZ('0')
038800951010     D*-------------
038900951010     D* Richiamo pgm Dichiarazione d'intenti
039000951010     D A311DS        E DS                  EXTNAME(ANA311DS) INZ
039100951010     D  CMT311       E                     INZ('0')
039200951010     D  RET311       E                     INZ('0')
039300951010     D  OPR311       E                     INZ('0')
039400951010     D  ERR311       E                     INZ('0')
039500010830G1036D*-------------
039600010830  "  D* Richiamo pgm linee di acquisto
039700010830  "  D ANA161DS0     E DS                  inz
039800010830  "  D  Ret1610      E                     inz('0')
039900010830  "  D  Opr1610      E                     inz('0')
040000010830  "  D  Err1610      E                     inz('0')
040100010830  "  D*-------------
040200010830  "  D* Richiamo pgm fornitori/magazzini
040300010830  "  D ANA410DS      E DS                  inz
040400010830  "  D  Ret410       E                     inz('0')
040500010830  "  D  Opr410       E                     inz('0')
040600010830  "  D  Err410       E                     inz('0')
040700010830  "  D*-------------
040800010830  "  D* Richiamo pgm assortimenti fornitori
040900010830  "  D ANA162DS      E DS                  inz
041000010830  "  D  Ret162       E                     inz('0')
041100010830  "  D  Opr162       E                     inz('0')
041200010830G1036D  Err162       E                     inz('0')
041300941227     D*-------------
041400941227     D* Bottoni
041500950124     D A431DS        E DS                  EXTNAME(ANA431DS) INZ
041600950313     D*-------------
041700970923     D* Bottoni dati agenti generali e per unità
041800970923     D A439DS        E DS                  EXTNAME(ANA439DS) INZ
041900970923     D*-------------
042000951010     D* Bottone Dichiarazione di intento
042100951010     D A331DS        E DS                  EXTNAME(ANA331DS) INZ
042200951010     D*-------------
042300950313     D* Bottoni contabilità
042400950313     D A231DS        E DS                  EXTNAME(ANA231DS) INZ
042500941219     D*-------------
042600941219     D* Parametri del PGM di test esistenza commenti ANA724R
042700941219     D A724DS        E DS                  EXTNAME(ANA724DS) INZ
042800950103     D  EXI724       E                     INZ('0')
042900941219     D  RIS724       E                     INZ('0')
043000010305a0871x**CMT724       E                     INZ('0')
043100941205     D*-------------
043200941205     D* Richiamo pgm Commenti
043300941219     D A725DS        E DS                  EXTNAME(ANA725DS) INZ
043400941205     D  RET725       E                     INZ('0')
043500941205     D  OPR725       E                     INZ('0')
043600941205     D  ERR725       E                     INZ('0')
043700980624     D*-------------
043800980624     D* Parametri per gestione FIDI
043900980624     D A360DS        E DS                  EXTNAME(ANA360DS) INZ
044000980624     D  RET360       E                     INZ('0')
044100980624     D  OPR360       E                     INZ('0')
044200980624     D  ERR360       E                     INZ('0')
044300980624     D*-------------
044400980624     D* Parametri per gestione soci
044500980624     D A600DS        E DS                  EXTNAME(ANA600DS) INZ
044600980624     D  RET600       E                     INZ('0')
044700980624     D  OPR600       E                     INZ('0')
044800980624     D  ERR600       E                     INZ('0')
044900950103     D*-------------
045000950124     D* Copia da fornitore a fornitore
045100950124     D A453DS        E DS                  EXTNAME(ANA453DS) INZ
045200950124     D  ERR453       E                     INZ('0')
045300941205     D*-------------
045400941205     D* Dati di ambiente ottenuti da XSOC
045500941205     D SOC001        E DS                  EXTNAME(XSOC001DS)
045600030514B0549D  XSCGEST                       1    OVERLAY(XSCSKP:3)
045700941205     D*-------------
045800941205     D* DS Interna per dati di output di XSOC
045900941205     D XSOCDS          DS          1000
046000940318     D*-------------
046100940318     D* DS Esterna gestione messaggi per COMIT
046200940318     D XMSGDS        E DS
046300940318     D  STS          E                     EXTFLD(MSGSTS)
046400941220     D                                     DIM(333)
046500940318     D  JBA          E                     EXTFLD(MSGJBA)
046600941220     D                                     DIM(502)
046700940201     D*-------------
046800940201     D DSFMT           DS
046900940506     D  $TASTO               369    369
047000940201     D  NRG                  370    370
047100940201     D  NCL                  371    371
047200941219     D*-------------
047300941219     D GERARK          DS
047400941220     D  CH                     1      2  0
047500950103     D  EXI                    3      3
047600941228     D  ERR                    4      4
047700941220     D  PGM                    5      7
047800950118     D  TXT                    8     37
047900970923     D*-------------
048000970923     D* campi di salvataggio schiera per agenti/unità
048100970924     D WCHI12          S              2
048200970924     D WCHM12          S              2
048300970924     D WDCB12          S             30
048400970924     D WPG12           S              4
048500970924     D WPGT12          S             27
048600970923     D*-------------
048700950313     D*Contabilta
048800950313     D GERARC          DS
048900950313     D  CHC                    1      2  0
049000950313     D  EXIC                   3      3
049100950313     D  ERRC                   4      4
049200950313     D  PGMC                   5      7
049300950313     D  TXTC                   8     37
049400940201     D*-------------
049500940201     D* posizione cursore
049600940201     D CURSOR          DS
049700940223     D  RIRI                   1      2B 0 INZ
049800940201     D  R$$                    2      2
049900940223     D  COCO                   3      4B 0 INZ
050000940201     D  C$$                    4      4
050100940207     D*-------------
050200940207     D* Reperimento nome PGM
050300940207     D STATUS         SDS           333
050400940207     D  DSPGM            *PROC
050500980115     d*-------------
050600980115
050700980115      * start definizioni precedentemente contenute nella sr. DEFCAM
050800980115     d*
050900980115     d* Variabili per gestione videate
051000980115     d*
051100980115     d*   fmt in gestione
051200980115     d $Gest           s              2
051300980115     d*   fine programma
051400980115     d $Fine           s              1
051500980115     d*   inizializzazione D1
051600980115     d $InzD1          s              1
051700980115     d*   fmt di provenienza
051800980115     d $LastG          s              2
051900980115     d*   ultima emissione
052000980115     d $LastV          s              2
052100980115     d*   stop bottoni
052200980115     d $Esci           s              1
052300980115     d*
052400980115     d* Variabili appoggio
052500980115     d*
052600980115     d wwCTB           s              2
052700980115     d CurR            s              2  0 inz(*zeros)
052800980115     d CurC            s              2  0 inz(*zeros)
052900980115     d wIN             s             99    inz(*zeros)
053000980115     d wEXF            s              1
053100980115     d wESP            s              1
053200980115     d wrk030          s              3  0 inz(*zeros)
053300980701     d wFGi            s                   like(fgiCont020)
053400980701     d wFGa            s                   like(fgiCont020)
053500980701     d $copok          s                   like(fgiCont020)
053600980115     d wDATE           s                   like(DTS724)
053700980115     d wOPZ            s                   like(OPZ430)
053800980115     d wOPR            s                   like(OPR430)
053900980115     d wKSC            s                   like(KSC430)
054000980115     d wERR            s                   like(ERR430)
054100980115     d wwwSOC          s                   like(XSCSOC)
054200980115     d wwwKCC          s                   like(KCC206)
054300980115     d wwwANL          s                   like(ANL206)
054400980115     d SAV             s                   like(GERARK)
054500980706     d EXIage          s                   like(exi439)
054600010831G1036D DrOpz           S              1A
054700010831  "  D DrNonAlloc      S              1A
054800010831G1036D DrErr           S              1A
054900980115     d*
055000980115     d* Indici
055100980115     d*
055200980115     d X               s              3  0 inz(*zeros)
055300980115     d Y               s              3  0 inz(*zeros)
055400980115     d $CTB            s              3  0 inz(*zeros)
055500980115     d*
055600000114A0735D ZDateISO        S               D
055700940225     D*-------------
055800940616     D* Costanti
055900941227     D*
056000010831G1036X*** nr.bottoni (dimensione schiera KH)
056100010831  "  X*** NB1             C                   CONST(14)
056200010831  "  X*** nr.bottoni (dimensione schiera KH1)
056300010831G1036X*** NB2             C                   CONST(5)
056400941205     D* data a 0
056500941205     D $DT0            C                   CONST('0001-01-01')
056600941219     D*
056700941228     D BLANK           C                   CONST(X'41')
056800941219     D*-------------
056900940506     D* Tasti di funzione
057000940506     D F01             C                   CONST(X'31')
057100940506     D F02             C                   CONST(X'32')
057200940506     D F03             C                   CONST(X'33')
057300940506     D F04             C                   CONST(X'34')
057400940506     D F05             C                   CONST(X'35')
057500940506     D F06             C                   CONST(X'36')
057600940506     D F07             C                   CONST(X'37')
057700940506     D F08             C                   CONST(X'38')
057800940506     D F09             C                   CONST(X'39')
057900940506     D F10             C                   CONST(X'3A')
058000940506     D F11             C                   CONST(X'3B')
058100940506     D F12             C                   CONST(X'3C')
058200940506     D F13             C                   CONST(X'B1')
058300940506     D F14             C                   CONST(X'B2')
058400940506     D F15             C                   CONST(X'B3')
058500940506     D F16             C                   CONST(X'B4')
058600940506     D F17             C                   CONST(X'B5')
058700940506     D F18             C                   CONST(X'B6')
058800940506     D F19             C                   CONST(X'B7')
058900940506     D F20             C                   CONST(X'B8')
059000940506     D F21             C                   CONST(X'B9')
059100940506     D F22             C                   CONST(X'BA')
059200940506     D F23             C                   CONST(X'BB')
059300940506     D F24             C                   CONST(X'BC')
059400940506     D ENTER           C                   CONST(X'F1')
059500940506     D ROLDWN          C                   CONST(X'F4')
059600940506     D ROLLUP          C                   CONST(X'F5')
059700941228     I*
059800941220     I*----------------------------------------------------*
059900940127     I*-------------
060000010830G1036 * aggiunti i nuovi bottoni a video
060100941228     IFMTD1
060200941228     I              D1D01                       D1D(1)
060300941228     I              D1D02                       D1D(2)
060400941228     I              D1D03                       D1D(3)
060500941228     I              D1D04                       D1D(4)
060600941228     I              D1D05                       D1D(5)
060700941228     I              D1D06                       D1D(6)
060800941228     I              D1D07                       D1D(7)
060900941228     I              D1D08                       D1D(8)
061000941228     I              D1D09                       D1D(9)
061100941228     I              D1D10                       D1D(10)
061200951010     I              D1D11                       D1D(11)
061300970919     I              D1D12                       D1D(12)
061400980624     I              D1D13                       D1D(13)
061500980624     I              D1D14                       D1D(14)
061600010830     I              D1D15                       D1D(15)
061700010830     I              D1D16                       D1D(16)
061800010830     I              D1D17                       D1D(17)
061900950313     IFMTD2
062000950313     I              D2D01                       D2D(01)
062100950313     I              D2D02                       D2D(02)
062200950313     I              D2D03                       D2D(03)
062300950313     I              D2D04                       D2D(04)
062400950313     I              D2D05                       D2D(05)
062500940506     I*-------------
062600940127     C*----------------------------------------------------*
062700940127     C*                MAIN LINE PROGRAM
062800940127     C*----------------------------------------------------*
062900940127     C*
063000940223     C* inizializzazione variabili
063100940223     C                   EXSR      INZVAR
063200940223     C*
063300940223     C     $FINE         DOWEQ     *OFF
063400940202     C     $GEST         CASEQ     'D1'          GESD1
063500950313     C     $GEST         CASEQ     'D2'          GESD2
063600940117     C                   END
063700940117     C                   END
063800940201     C*
063900940325     C* fine programma
064000940325     C                   EXSR      ENDPGM
064100940325     C/EJECT
064200940325     C************************************************************
064300951010     C* FINE PROGRAMMA
064400940325     C************************************************************
064500940325     C     ENDPGM        BEGSR
064600940325     C*
064700940325     C* passaggio dati al pgm chiamante
064800950124     C                   MOVEL     WOPR          OPR430                                     ne S1
064900951221     C*
065000951221     C* se premuto F3/F12 è inutile passare indietro gli stessi dati
065100960118     C*                  IF        Ret430 = '0'
065200950313     C     1             DO        NB1           X
065300941228     C                   MOVEL     MIX(X)        GERARK
065400941228     C     PGM           IFEQ      'TCM'
065500950124     C                   MOVE      EXI           B01430
065600950124     C                   MOVE      ERR           E01430
065700941228     C                   ENDIF
065800950206     C     PGM           IFEQ      'AGE'
065900950206     C                   MOVE      EXI           B04430
066000950206     C                   MOVE      ERR           E04430
066100960118     C                   ENDIF
066200951004     C* per la contabilità
066300951004     C     PGM           IFEQ      'RCO'
066400951004     C* considero un dato annullato (EXI=0;ERR=2)
066500951004     C     EXI           IFEQ      '0'
066600951004     C     ERR           ANDEQ     '2'
066700951004     C* come se fosse esistente
066800951004     C                   MOVE      '1'           B02430
066900951004     C                   ELSE
067000951004     C                   MOVE      EXI           B02430
067100951004     C                   ENDIF
067200951004     C                   MOVE      '0'           E02430
067300951004     C                   ENDIF
067400941228     C                   ENDDO
067500960118     C*                  ENDIF
067600950313     C*
067700950124     C                   MOVEL     A430DS        KPJBU
067800940325     C*
067900940325     C                   SETON                                            RT
068000940325     C*
068100940325     C                   ENDSR
068200940325     C/EJECT
068300940201     C************************************************************
068400940201     C* RICERCHE
068500940201     C************************************************************
068600940127     C     SEARCH        BEGSR
068700940127     C*
068800940127     C* determino Riga/Colonna del cursore
068900940127     C                   MOVE      NRG           R$$
069000940127     C                   MOVE      NCL           C$$
069100940127     C                   Z-ADD     RIRI          CURR
069200940127     C                   Z-ADD     COCO          CURC
069300941205     C*
069400940127     C                   ENDSR
069500940225     C/EJECT
069600940225     C************************************************************
069700940225     C* GESTIONE EMISSIONE TASTI FUNZIONE
069800940225     C************************************************************
069900940225     C     GESPIE        BEGSR
070000941212     C*
070100940225     C                   SELECT
070200940225     C*
070300940506     C     $GEST         WHENEQ    'D1'
070400940506     C     $ULKD1        IFEQ      0
070500940506     C* riempimento opzioni
070600940506     C                   EXSR      F24D1
070700941219     C                   ELSE
070800941219     C                   WRITE     FMTZ1
070900940506     C                   ENDIF
071000950313     C*
071100950313     C     $GEST         WHENEQ    'D2'
071200950313     C     $ULKD1        IFEQ      0
071300950313     C* riempimento opzioni
071400950313     C                   EXSR      F24D1
071500950313     C                   ELSE
071600950313     C                   WRITE     FMTZ1
071700950313     C                   ENDIF
071800940225     C*
071900940225     C                   ENDSL
072000940225     C*
072100940225     C                   ENDSR
072200940131     C/EJECT
072300940131     C************************************************************
072400940510     C* GESTIONE VIDEO RECORD D1
072500940131     C************************************************************
072600940127     C     GESD1         BEGSR
072700940225     C* inizializzazione videata
072800940223     C     $INZD1        IFEQ      *ON
072900940127     C                   EXSR      INZD1
073000940223     C                   MOVE      *OFF          $INZD1
073100940117     C                   END
073200940225     C* emissione piede videata se proveniente da altra
073300940225     C* salvataggio valore formato di provenienza
073400940207     C     $LASTV        IFNE      'D1'
073500940225     C                   EXSR      GESPIE
073600940202     C                   MOVE      $LASTV        $LASTG
073700940202     C                   MOVE      'D1'          $LASTV
073800940202     C                   END
073900941212     C*
074000941212     C* se esistono errori sulla videata
074100941212     C* emetto la write del formato a indicatori spenti per vedere
074200941212     C* le eventuali decodifiche
074300941212     C     *IN99         IFEQ      *ON
074400941212     C                   MOVEA     *IN           WIN
074500941212     C                   MOVE      *ALL'0'       IN5098           49
074600941212     C                   MOVEA     IN5098        *IN(50)
074700941212     C                   WRITE     FMTD1
074800941212     C                   MOVEA     WIN           *IN
074900941212     C                   ENDIF
075000940131     C*
075100941219     C* se è richiesta l'emissione della finestra a video
075200941219     C     WESP          IFEQ      *ON
075300941219     C*
075400940714     C                   EXFMT     FMTD1
075500940718     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
075600940718     C                   MOVEL     '1'           WEXF
075700941219     C*
075800941219     C* altrimenti simulo la digitazione dell'ENTER
075900941219     C                   ELSE
076000941219     C                   MOVEL     ENTER         $TASTO
076100941219     C                   ENDIF
076200940714     C*
076300940616     C* controllo abilitazione tasti funzione
076400940506     C                   EXSR      CTRKD1
076500940506     C*
076600941205     C     *IN99         IFEQ      *OFF
076700940131     C*
076800941205     C                   SELECT
076900950313     C* Rollup
077000950313     C     $TASTO        WHENEQ    ROLLUP
077100951004     C* solo se la contabilità è stata selezionata
077200951004     C     D1B($CTB)     IFEQ      1
077300951004     C                   MOVE      'D2'          $GEST
077400951004     C                   ELSE
077500951004     C                   SETON                                        9799
077600951004     C                   ENDIF
077700940131     C* F3=Fine
077800940506     C     $TASTO        WHENEQ    F03
077900940309     C                   EXSR      F03D1
078000940131     C* F4=Ricerca
078100940506     C     $TASTO        WHENEQ    F04
078200940127     C                   EXSR      SEARCH
078300940224     C* F5=Refresh
078400940506     C     $TASTO        WHENEQ    F05
078500940309     C                   EXSR      F05D1
078600940223     C* F8=Successivo
078700940506     C     $TASTO        WHENEQ    F08
078800940309     C                   EXSR      F08D1
078900940131     C* F12=Ritorno
079000940506     C     $TASTO        WHENEQ    F12
079100940309     C                   EXSR      F12D1
079200940315     C* F24=Altri tasti
079300940506     C     $TASTO        WHENEQ    F24
079400940315     C                   EXSR      F24D1
079500940712     C*
079600941205     C                   OTHER
079700940127     C                   EXSR      CTRD1
079800940203     C*
079900941205     C     *IN99         IFEQ      *OFF
080000950103     C* se opzione=copia chiamo solo il pgm che sarebbe obbligatorio
080100950103     C* in immissione e poi gestisco la copia dei restanti bottoni
080200950103     C* scelti
080300950124     C     OPZ430        IFEQ      '03'
080400950103     C                   EXSR      GESCOP
080500950103     C* altrimenti
080600950103     C* chiamo pgm a seconda dei bottoni selezionati
080700950103     C                   ELSE
080800950313     C                   EXSR      GESANA
080900950103     C                   ENDIF
081000950103     C                   ENDIF
081100940203     C*
081200941205     C                   ENDSL
081300940224     C*
081400941205     C                   ENDIF
081500941205     C*
081600940117     C                   ENDSR
081700950313     C/EJECT
081800950313     C************************************************************
081900950313     C* GESTIONE VIDEO RECORD D2
082000950313     C************************************************************
082100950313     C     GESD2         BEGSR
082200950313     C* emissione piede videata se proveniente da altra
082300950313     C* salvataggio valore formato di provenienza
082400950313     C     $LASTV        IFNE      'D2'
082500950313     C                   EXSR      GESPIE
082600950313     C                   MOVE      $LASTV        $LASTG
082700950313     C                   MOVE      'D2'          $LASTV
082800950313     C                   END
082900950313     C*
083000950313     C* se esistono errori sulla videata
083100950313     C* emetto la write del formato a indicatori spenti per vedere
083200950313     C* le eventuali decodifiche
083300950313     C     *IN99         IFEQ      *ON
083400950313     C                   MOVEA     *IN           WIN
083500950313     C                   MOVE      *ALL'0'       IN5098           49
083600950313     C                   MOVEA     IN5098        *IN(50)
083700950313     C                   WRITE     FMTD2
083800950313     C                   MOVEA     WIN           *IN
083900950313     C                   ENDIF
084000950313     C*
084100950313     C* se è richiesta l'emissione della finestra a video
084200950313     C* e se richiesta contabilità
084300951004     C     WESP          IFEQ      *ON
084400950313     C*
084500950313     C                   EXFMT     FMTD2
084600950313     C* memorizzo l'avvenuta EXFMT per richiamo a pgm di sola finestra
084700950313     C                   MOVEL     '1'           WEXF
084800950313     C*
084900950313     C* altrimenti simulo la digitazione dell'ENTER
085000950313     C                   ELSE
085100950313     C                   MOVEL     ENTER         $TASTO
085200950313     C                   ENDIF
085300950313     C*
085400950313     C* controllo abilitazione tasti funzione
085500950313     C                   EXSR      CTRKD1
085600950313     C*
085700950313     C     *IN99         IFEQ      *OFF
085800950313     C*
085900950313     C                   SELECT
086000950313     C* Rollup
086100950313     C     $TASTO        WHENEQ    ROLDWN
086200951004     C* solo se non è un'opzione diretta
086300951004     C     OPD430        IFEQ      *OFF
086400951004     C                   MOVE      'D1'          $GEST
086500951004     C                   ELSE
086600951004     C                   SETON                                        9799
086700951004     C                   ENDIF
086800950313     C* F3=Fine
086900950313     C     $TASTO        WHENEQ    F03
087000950313     C                   EXSR      F03D1
087100950313     C* F4=Ricerca
087200950313     C     $TASTO        WHENEQ    F04
087300950313     C                   EXSR      SEARCH
087400950313     C* F5=Refresh
087500950313     C     $TASTO        WHENEQ    F05
087600950313     C                   EXSR      F05D1
087700950313     C* F8=Successivo
087800950313     C     $TASTO        WHENEQ    F08
087900950313     C                   EXSR      F08D1
088000950313     C* F12=Ritorno
088100950313     C     $TASTO        WHENEQ    F12
088200950313     C                   EXSR      F12D1
088300950313     C* F24=Altri tasti
088400950313     C     $TASTO        WHENEQ    F24
088500950313     C                   EXSR      F24D1
088600950313     C*
088700950313     C                   OTHER
088800950313     C                   EXSR      CTRD1
088900950313     C*
089000950313     C     *IN99         IFEQ      *OFF
089100950313     C* se opzione=copia chiamo solo il pgm che sarebbe obbligatorio
089200950313     C* in immissione e poi gestisco la copia dei restanti bottoni
089300950313     C* scelti
089400950313     C     OPZ430        IFEQ      '03'
089500950313     C                   EXSR      GESCOP
089600950313     C* altrimenti
089700950313     C* chiamo pgm a seconda dei bottoni selezionati
089800950313     C                   ELSE
089900951004     C* se non è stata richiesta l'opzione diretta "Contabilità"
090000951004     C     OPD430        IFEQ      *OFF
090100951004     C* ciclo su tutti i possibili bottoni
090200951004     C                   EXSR      GESANA
090300951004     C* altrimenti
090400951004     C                   ELSE
090500951004     C* ciclo solo sui bottoni della contabilità
090600951004     C                   MOVEL     MIX(1)        GERARK
090700951004     C                   EXSR      GESCNT
090800951004     C                   MOVEL     GERARK        MIX(1)
090900951004     C                   ENDIF
091000950313     C                   ENDIF
091100950313     C                   ENDIF
091200950313     C*
091300950313     C                   ENDSL
091400950313     C*
091500950313     C                   ENDIF
091600950313     C*
091700950313     C                   ENDSR
091800940506     C/EJECT
091900940506     C************************************************************
092000940506     C* CONTROLLO ABILITAZIONE TASTI VIDEATA D1
092100940506     C************************************************************
092200940506     C     CTRKD1        BEGSR
092300941205     C*
092400940506     C                   SETOFF                                       99
092500940506     C                   MOVEA     $FC1          $FC
092600940506     C                   EXSR      CTRKEY
092700940506     C     *IN99         IFEQ      *ON
092800940506     C                   SETON                                        97
092900940506     C                   ENDIF
093000941205     C*
093100940506     C                   ENDSR
093200941212     C/EJECT
093300941205     C************************************************************
093400941205     C* INIZIALIZZAZIONE VIDEATA DATI
093500941205     C************************************************************
093600940127     C     INZD1         BEGSR
093700940131     C*
093800940131     C                   CLEAR                   FMTD1
093900950313     C                   CLEAR                   FMTD2
094000941219     C*
094100950313     C* COMMERCIALI
094200950313     C     1             DO        NB1           X
094300941220     C                   MOVEL     MIX(X)        GERARK
094400941220     C*
094500950313     C* testo esistenza/errore per il bottone del singolo pgm anagrafic
094600950313     C                   EXSR      TSTANA
094700941228     C* salvo in schiera il suo stato
094800941228     C                   MOVEL     GERARK        MIX(X)
094900941228     C*
095000950313     C* imposto il bottone e il relativo testo per anagrafiche
095100950313     C                   EXSR      IMPANA
095200950118     C*
095300950118     C* lo decodifico
095400950118     C                   MOVE      TXT           D1D(X)
095500941220     C*
095600941219     C                   ENDDO
095700951004     C* Salvataggio schiera bottoni anagrafiche
095800951004     C                   MOVEL     D1B           WD1B
095900951004     C                   MOVEL     D1T           WD1T
096000951004     C                   MOVEA     MIX           WMIX
096100950313     C* CONTABILITA'
096200950313     C     1             DO        NB2           Y
096300950313     C                   MOVEL     MIX1(Y)       GERARC
096400950313     C*
096500950313     C* testo esistenza/errore per il bottone del singolo pgm per ctb
096600950313     C                   EXSR      TSTCNT
096700950313     C* salvo in schiera il suo stato
096800950313     C                   MOVEL     GERARC        MIX1(Y)
096900950313     C*
097000950313     C* imposto il bottone e il relativo testo per ctb
097100950313     C                   EXSR      IMPCNT
097200950313     C*
097300950313     C* lo decodifico
097400950313     C                   MOVE      TXTC          D2D(Y)
097500950313     C*
097600950313     C                   ENDDO
097700951004     C* Salvataggio schiera bottoni CONTABILITA'
097800951004     C                   MOVEL     D2B           WD2B
097900951004     C                   MOVEL     D2T           WD2T
098000951004     C                   MOVEA     MIX1          WMIX1
098100950313     C*
098200940117     C                   ENDSR
098300941227     C/EJECT
098400941227     C************************************************************
098500950313     C* TEST BOTTONE DEI SINGOLI PGM ANAGRAFICHE
098600941227     C************************************************************
098700950313     C     TSTANA        BEGSR
098800941227     C*
098900941227     C                   SELECT
099000941227     C*
099100941227     C* Commerciali generali
099200950124     C     PGM           WHENEQ    'FRN'
099300950103     C     EXI           IFEQ      *BLANK
099400950124     C                   EXSR      TSTFRN
099500941228     C                   ENDIF
099600941228     C*
099700941228     C* Dati anagrafici
099800941228     C     PGM           WHENEQ    'SOG'
099900950103     C     EXI           IFEQ      *BLANK
100000941228     C                   EXSR      TSTSOG
100100941228     C                   ENDIF
100200941228     C*
100300941228     C* Commenti
100400941228     C     PGM           WHENEQ    'TCM'
100500950124     C                   MOVEL     B01430        EXI
100600950124     C                   MOVEL     E01430        ERR
100700950103     C     EXI           IFEQ      *BLANK
100800941228     C                   EXSR      TSTTCM
100900941228     C                   ENDIF
101000941228     C*
101100941228     C* Filiali
101200941228     C     PGM           WHENEQ    'FIL'
101300950103     C     EXI           IFEQ      *BLANK
101400980630     C     OPZ430        IFEQ      '03'
101500980630     c                   eval      wFGi    = fgiFIL020
101600980701     C                   exsr      tstcANP020
101700980630     C                   if        $copok  = *off
101800980630     C                   MOVEL     '2'           EXI
101900980630     C                   MOVEL     '0'           ERR
102000980630     C                   else
102100980630     C                   EXSR      TSTFIL
102200980630     C                   endif
102300980630     C                   ELSE
102400941228     C                   EXSR      TSTFIL
102500980630     C                   ENDIF
102600950206     C                   ENDIF
102700941228     C*
102800941228     C* Dati statistici
102900950124     C     PGM           WHENEQ    'FRS'
103000950103     C     EXI           IFEQ      *BLANK
103100980630     C     OPZ430        IFEQ      '03'
103200980630     c                   eval      wFGi    = fgiFRS020
103300980701     C                   exsr      tstcANP020
103400980630     C                   if        $copok  = *off
103500980630     C                   MOVEL     '2'           EXI
103600980630     C                   MOVEL     '0'           ERR
103700980630     C                   else
103800980630     C                   EXSR      TSTFRS
103900980630     C                   endif
104000980630     C                   ELSE
104100950124     C                   EXSR      TSTFRS
104200980630     C                   ENDIF
104300941228     C                   ENDIF
104400950119     C*
104500950119     C* Appoggi bancari
104600950119     C     PGM           WHENEQ    'CBA'
104700950119     C     EXI           IFEQ      *BLANK
104800980630     C     OPZ430        IFEQ      '03'
104900980630     c                   eval      wFGi    = fgiCBA020
105000980701     C                   exsr      tstcANP020
105100980630     C                   if        $copok  = *off
105200980630     C                   MOVEL     '2'           EXI
105300980630     C                   MOVEL     '0'           ERR
105400980630     C                   else
105500980630     C                   EXSR      TSTCBA
105600980630     C                   endif
105700980630     C                   ELSE
105800950119     C                   EXSR      TSTCBA
105900980630     C                   ENDIF
106000950119     C                   ENDIF
106100941228     C*
106200980630     C* Contabilità
106300941228     C     PGM           WHENEQ    'RCO'
106400951004     C* salvo la posizione della contabilità nella schiera MIX
106500951004     C* cosicchè posso testare subito se è stato impostato il suo
106600951004     C* bottone per abilitare il ROLLUP o meno
106700951004     C                   Z-ADD     X             $CTB
106800951004     C* la contabilità non è copiabile
106900951004     C* se richiesta questa opzione proteggo il bottone
107000950313     C     OPZ430        IFEQ      '03'
107100950313     C                   MOVEL     '2'           EXI
107200950313     C                   MOVEL     '0'           ERR
107300950313     C                   ELSE
107400950313     C                   CLEAR                   EXICNT
107500950313     C                   MOVEL     B02430        EXI
107600950313     C                   MOVEL     E02430        ERR
107700951004     C* TSTPDC la devo eseguire sempre perchè reperisce il codice capoconto
107800950313     C                   EXSR      TSTPDC
107900941228     C                   ENDIF
108000950125     C*
108100970922     C* Agenti generali
108200950125     C     PGM           WHENEQ    'AGE'
108300950206     C                   MOVEL     B04430        EXI
108400950206     C                   MOVEL     E04430        ERR
108500950125     C     EXI           IFEQ      *BLANK
108600980630     C     OPZ430        IFEQ      '03'
108700980630     c                   eval      wFGi    = fgiAGE020
108800980701     C                   exsr      tstcANP020
108900980630     C                   if        $copok  = *off
109000980630     C                   MOVEL     '2'           EXI
109100980630     C                   MOVEL     '0'           ERR
109200980630     C                   else
109300980630     C                   EXSR      TSTAGE
109400980630     C                   endif
109500980630     C                   ELSE
109600950206     C                   EXSR      TSTAGE
109700980630     C                   ENDIF
109800950125     C                   ENDIF
109900970922     C*
110000970922     C* Agenti per unità
110100970922     C     PGM           WHENEQ    'AGU'
110200971013     C* se siamo in immissione oppure in copia oppure non è abilitata la
110300971013     C* gestione Agenti/Unità sul parametro di base 'ANFRN00F'
110400971013     C* non deve essere possibile selezionare questa opzione
110500971013     C                   IF        GAU020   <>   *On   or
110600980824     C                             Opz430   =    '01'
110700970923     C                   Move      '0'           Err
110800970923     C                   Move      '2'           Exi
110900970923     C                   Move      Gerark        Mix(X)
111000970923     c                   else
111100970925     C*testo se il fornitore su cui sono posizionato è un agente e solo
111200970925     C*in questo caso passo a verificare che esista almeno un agente
111300970925     C* per unità
111400980630     C     OPZ430        IFEQ      '03'
111500980630     c                   eval      wFGi    = fgiAGL020
111600980701     C                   exsr      tstcANP020
111700980630     C                   if        $copok  = *off
111800980630     C                   MOVEL     '2'           EXI
111900980630     C                   MOVEL     '0'           ERR
112000980630     C                   else
112100980630     C                   EXSR      TSTAGE
112200980630     C                   endif
112300980630     C                   ELSE
112400970925     C                   EXSR      TSTAGE
112500980630     C                   ENDIF
112600980706     C                   movel     exi439        exiage
112700970925     C                   if        Exi      =    '0'
112800970925     C                   Move      '0'           Err
112900970925     C                   Move      '2'           Exi
113000970925     C                   else
113100970923     C                   EXSR      TSTAGU
113200970925     c                   endif
113300970923     c                   endif
113400950227     C*
113500950227     C* Spedizionieri
113600980702     C     PGM           WHENEQ    'SPE'
113700950103     C     EXI           IFEQ      *BLANK
113800980630     C     OPZ430        IFEQ      '03'
113900980710     c                   eval      wFGi    = imm033
114000980710     C                   exsr      tstcANP033
114100980630     C                   if        $copok  = *off
114200980630     C                   MOVEL     '2'           EXI
114300980630     C                   MOVEL     '0'           ERR
114400980630     C                   else
114500980710     C                   EXSR      TSTSPE
114600980630     C                   endif
114700980630     C                   ELSE
114800950227     C                   EXSR      TSTSPE
114900941228     C                   ENDIF
115000980630     C                   ENDIF
115100941228     C*
115200941228     C* Periodi esclusione pagamento
115300941228     C     PGM           WHENEQ    'PEP'
115400950103     C     EXI           IFEQ      *BLANK
115500980630     C     OPZ430        IFEQ      '03'
115600980630     c                   eval      wFGi    = fgiPEP020
115700980701     C                   exsr      tstcANP020
115800980630     C                   if        $copok  = *off
115900980630     C                   MOVEL     '2'           EXI
116000980630     C                   MOVEL     '0'           ERR
116100980630     C                   else
116200980630     C                   EXSR      TSTPEP
116300980630     C                   endif
116400980630     C                   ELSE
116500950113     C                   EXSR      TSTPEP
116600980630     C                   ENDIF
116700941228     C                   ENDIF
116800951010     C*
116900951010     C* Dichiarazione di intenti
117000951010     C     PGM           WHENEQ    'DIC'
117100951010     C     EXI           IFEQ      *BLANK
117200951010     C                   EXSR      TSTDIC
117300951010     C                   ENDIF
117400980624     C*
117500980624     C* Fidi
117600980624     C     PGM           WHENEQ    'FID'
117700980624     C     EXI           IFEQ      *BLANK
117800980624     C                   EXSR      TSTFID
117900980624     C                   ENDIF
118000980624     C*
118100980624     C* Soci per agente
118200980624     C     PGM           WHENEQ    'SOA'
118300980624     C     EXI           IFEQ      *BLANK
118400980630     C     OPZ430        IFEQ      '03'
118500980630     c                   eval      wFGi    = fgiSOC020
118600980701     C                   exsr      tstcANP020
118700980630     C                   if        $copok  = *off
118800980630     C                   MOVEL     '2'           EXI
118900980630     C                   MOVEL     '0'           ERR
119000980630     C                   else
119100980630     C                   EXSR      TSTSOA
119200980630     C                   endif
119300980630     C                   ELSE
119400980624     C                   EXSR      TSTSOA
119500980624     C                   ENDIF
119600980630     C                   ENDIF
119700010830G1036C*
119800010830  "  C* Linee di acquisto
119900030514G1036C     PGM           wheneq    'LDA'
120000030514B0549 * Se non è installato proj distribuzione                         a
120100030514  "   * proteggo l'opzione
120200030514  "  C                   IF        XSCGEST <> '1'
120300030514  "  C                   Move      '0'           Err
120400030514  "  C                   Move      '2'           Exi
120500030514  "  C                   Move      Gerark        Mix(X)
120600030514  "  c                   else
120700030514B0549x***  EXI           ifeq      *blank
120800030514G1036C                   exsr      TSTLDA
120900010830  "  C                   endif
121000010830  "  C*
121100010830  "  C* Abilitazioni fornitori/magazzini - Dati per magazzino
121200030514G1036C     PGM           wheneq    'FDM'
121300030514B0549 * Se non è installato proj distribuzione                         a
121400030514  "   * proteggo l'opzione
121500030514  "  C                   IF        XSCGEST <> '1'
121600030514  "  C                   Move      '0'           Err
121700030514  "  C                   Move      '2'           Exi
121800030514  "  C                   Move      Gerark        Mix(X)
121900030514  "  c                   else
122000030514B0549x***  EXI           ifeq      *blank
122100030514G1036C                   exsr      TSTFDM
122200010830  "  C                   endif
122300010830  "  C*
122400010830  "  C* Assortimenti fornitori
122500030514G1036C     PGM           wheneq    'ASF'
122600030514B0549 * Se non è installato proj distribuzione                         a
122700030514  "   * proteggo l'opzione
122800030514  "  C                   IF        XSCGEST <> '1'
122900030514  "  C                   Move      '0'           Err
123000030514  "  C                   Move      '2'           Exi
123100030514  "  C                   Move      Gerark        Mix(X)
123200030514  "  c                   else
123300030514B0549x***  EXI           ifeq      *blank
123400030514G1036C                   exsr      TSTASF
123500010830G1036C                   endif
123600010830     C*
123700941228     C                   ENDSL
123800941228     C*
123900941228     C                   ENDSR
124000950313     C/EJECT
124100950313     C************************************************************
124200950313     C* TEST BOTTONI PROGRAMMI CONTABILITA'
124300950313     C************************************************************
124400950313     C     TSTCNT        BEGSR
124500950313     C*
124600950313     C*
124700950313     C                   SELECT
124800950313     C*
124900950313     C* Capoconto
125000950313     C     PGMC          WHENEQ    'PDC'
125100950313     C     EXIC          IFEQ      *BLANK
125200950313     C                   EXSR      TSTPDC
125300950313     C                   ENDIF
125400950313     C*
125500950313     C* Regole contabili
125600950313     C     PGMC          WHENEQ    'RGC'
125700950313     C     EXIC          IFEQ      *BLANK
125800950313     C                   EXSR      TSTRGC
125900950313     C                   ENDIF
126000950313     C*
126100950313     C* Attributi contabili
126200950313     C     PGMC          WHENEQ    'ATC'
126300950313     C     EXIC          IFEQ      *BLANK
126400950313     C                   EXSR      TSTATC
126500950313     C                   ENDIF
126600950313     C*
126700950313     C* Percipienti
126800950313     C     PGMC          WHENEQ    'PER'
126900950313     C     EXIC          IFEQ      *BLANK
127000950313     C                   EXSR      TSTPER
127100950313     C                   ENDIF
127200950313     C*
127300950313     C* Voci analitica
127400950313     C     PGMC          WHENEQ    'VCA'
127500950313     C     EXIC          IFEQ      *BLANK
127600950313     C                   EXSR      TSTVCA
127700950313     C                   ENDIF
127800950313     C*
127900950313     C                   ENDSL
128000950313     C*
128100950313     C                   ENDSR
128200950103     C/EJECT
128300950103     C************************************************************
128400950313     C* IMPOSTO BOTTONE E TESTO PER ANAGRAFICHE
128500950103     C************************************************************
128600950313     C     IMPANA        BEGSR
128700950103     C*
128800950118     C*   Input
128900950118     c* - X
129000950118     c*   EXI    = Esistenza bottone
129100950118     C*   ERR    = Errore bottone
129200950118     c*
129300950118     c*   Output
129400950118     C*
129500950103     C* preparo 3 variabili generiche per la valorizzazione di quelle
129600950103     C* a video :
129700950118     C* - D1B = bottone predisposto in chiamata si/no;
129800950103     C*         lo valorizzo a 1 (pgm da richiamare) se :
129900950103     C*         1) c'è un errore da correggere
130000950103     C*         2) richiesto in gerarchia
130100950118     C* - D1T = stato del file relativo al bottone;
130200950103     C*         lo valorizzo con una costante esplicativa a seconda
130300950103     C*         dei seguenti casi :
130400950103     C*         1) 1/0 - esiste almeno un rcd e non ci sono errori di
130500950103     C*            collegamento con altri file = "Presente"
130600950103     C*         2) 0/1 - non esiste nessun rcd ma sarebbe obbligatorio
130700950103     C*            averne = "Inserire"
130800950103     C*         3) 1/1 - esiste almeno un rcd ma ci sono errori di
130900950103     C*            collegamento con altri file = "Correggere"
131000950103     C*         4) 0/0 - non esiste nessun rcd e non sono obbligatori
131100950103     C*            non emetto nessun testo
131200950103     C*         5) 0/2 - capostipite annullato (questo incrocio può
131300950103     C*            accadere solo se l'annullamento è logico e per quei
131400950103     C*            file che sono definiti obbligatori in immissione)
131500950103     C*         6) 2/0 - bottone da disabilitare : viene deciso da
131600950103     C*            questo pgm a seconda dei casi
131700950103     C*
131800950118     C                   MOVE      *ZERO         D1B(X)
131900021017A0973C                   EVAL      D1BA(X) = Normale
132000970923     C*
132100950103     C*
132200950103     C                   SELECT
132300950103     C* se siamo in copia mi comporto diversamente dagli altri casi
132400950124     C     OPZ430        WHENEQ    '03'
132500950317     C* se bottone presente (ma non contabilità)
132600950103     C     EXI           IFEQ      '1'
132700950317     C     PGM           ANDNE     'RCO'
132800950103     C* lo predispongo per la copia
132900950118     C                   MOVE      1             D1B(X)
133000950103     C* se non presente
133100950103     C                   ELSE
133200951221     C* se il pgm in questione è quello principale (FRN)
133300951221     C                   IF        Pgm = 'FRN'     and
133400951221     C* e i suoi dati sono annullati
133500951221     C                             Exi = '0'  and  Err = '2'
133600951221     C* lo predispongo lo stesso per la copia
133700951221     C                   MOVE      1             D1B(X)
133800951221     C* altrimenti
133900951221     C                   ELSE
134000950103     C* lo inibisco alla copia
134100021017A0973X***                MOVE      2             D1B(X)
134200021017A0973C                   EVAL      D1BA(X) = Protetto
134300950103     C                   ENDIF
134400951221     C                   ENDIF
134500950103     C*
134600950103     C                   OTHER
134700950103     C                   SELECT
134800950103     C* se richiesto di inibire il bottone
134900950118     C     EXI           WHENEQ    '2'
135000950103     C* lo predispongo per l'inibizione
135100021017A0973X***                MOVE      2             D1B(X)
135200021017A0973C                   EVAL      D1BA(X) = Protetto
135300950315     C* se c'è un errore e non è stata richiesta un'opzione diretta
135400950103     C     ERR           WHENEQ    '1'
135500950315     C     OPD430        ANDNE     *ON
135600950103     C* oppure il bottone è previsto dalla gerarchia
135700970923     C     CH            ORNE      98
135800970923     C     CH            ANDne     99
135900950103     C* questo è valido solo nell'inizializzazione della videata
136000950103     C* perchè nelle impostazioni successive le operazioni già eseguite
136100950103     C* non devono essere reimpostate
136200950103     C     $INZD1        ANDEQ     *ON
136300950103     C* lo predispongo per la chiamata
136400950118     C                   MOVE      1             D1B(X)
136500950103     C*
136600950103     C                   ENDSL
136700950103     C*
136800950103     C                   ENDSL
136900950103     C*
137000950103     C* predispongo i testi a secondo dell'esistenza o meno del bottone
137100950103     C* e dell'eventuale errore che ciò genera
137200950118     C                   MOVE      *BLANK        D1T(X)
137300950103     C                   SELECT
137400950103     C     EXI           WHENEQ    '0'
137500950104     C     ERR           ANDEQ     '1'
137600950118     C                   MOVE      INSER         D1T(X)
137700950103     C     EXI           WHENEQ    '1'
137800950104     C     ERR           ANDEQ     '1'
137900950118     C                   MOVE      CORR          D1T(X)
138000950103     C     EXI           WHENEQ    '1'
138100950104     C     ERR           ANDEQ     '0'
138200950118     C                   MOVE      PRES          D1T(X)
138300950103     C     EXI           WHENEQ    '0'
138400950104     C     ERR           ANDEQ     '0'
138500950118     C                   MOVE      BLANK         D1T(X)
138600950104     C     EXI           WHENEQ    '0'
138700950104     C     ERR           ANDEQ     '2'
138800950118     C                   MOVE      ANNUL         D1T(X)
138900950426     C     EXI           WHENEQ    '2'
139000950426     C     ERR           ANDEQ     '0'
139100950426     C                   MOVE      BLANK         D1T(X)
139200950103     C                   ENDSL
139300050629     C*
139400050629B0818C* se si tratta dei FIDI
139500050629  "  C     PGM           IFEQ      'FID'
139600050629  "  c                   Select
139700050629  "  c* commerciale
139800050629  "  C     EXI           WHENEQ    '1'
139900050629  "  C     ERR           ANDEQ     '0'
140000050629  "  C                   MOVE      COMM          D1T(X)
140100050629  "  c* factor
140200050629  "  C     EXI           WHENEQ    'F'
140300050629  "  C     ERR           ANDEQ     '0'
140400050629  "  C                   MOVE      FACT          D1T(X)
140500050629  "  c* assicurativo
140600050629  "  C     EXI           WHENEQ    'A'
140700050629  "  C     ERR           ANDEQ     '0'
140800050629  "  C                   MOVE      ASSI          D1T(X)
140900050629  "  c                   endsl
141000050629B0818C                   ENDIF
141100950118     C*
141200950118     C                   ENDSR
141300950313     C/EJECT
141400950313     C************************************************************
141500950313     C* IMPOSTO BOTTONE E TESTO CONTABILITA
141600950313     C************************************************************
141700950313     C     IMPCNT        BEGSR
141800950313     C*
141900950313     C*   Input
142000950313     c* - Y
142100950313     c*   EXIC   = Esistenza bottone
142200950313     C*   ERRC   = Errore bottone
142300950313     c*
142400950313     c*   Output
142500950313     C*
142600950313     C* preparo 3 variabili generiche per la valorizzazione di quelle
142700950313     C* a video :
142800950313     C* - D2B = bottone predisposto in chiamata si/no;
142900950313     C*         lo valorizzo a 1 (pgm da richiamare) se :
143000950313     C*         1) c'è un errore da correggere
143100950313     C*         2) richiesto in gerarchia
143200950313     C* - D2T = stato del file relativo al bottone;
143300950313     C*         lo valorizzo con una costante esplicativa a seconda
143400950313     C*         dei seguenti casi :
143500950313     C*         1) 1/0 - esiste almeno un rcd e non ci sono errori di
143600950313     C*            collegamento con altri file = "Presente"
143700950313     C*         2) 0/1 - non esiste nessun rcd ma sarebbe obbligatorio
143800950313     C*            averne = "Inserire"
143900950313     C*         3) 1/1 - esiste almeno un rcd ma ci sono errori di
144000950313     C*            collegamento con altri file = "Correggere"
144100950313     C*         4) 0/0 - non esiste nessun rcd e non sono obbligatori
144200950313     C*            non emetto nessun testo
144300950313     C*         5) 0/2 - capostipite annullato (questo incrocio può
144400950313     C*            accadere solo se l'annullamento è logico e per quei
144500950313     C*            file che sono definiti obbligatori in immissione)
144600950313     C*         6) 2/0 - bottone da disabilitare : viene deciso da
144700950313     C*            questo pgm a seconda dei casi
144800950313     C*
144900950313     C                   MOVE      *ZERO         D2B(Y)
145000021017A0973C                   EVAL      D2BA(Y) = Normale
145100950313     C*
145200950313     C                   SELECT
145300950313     C* se siamo in copia mi comporto diversamente dagli altri casi
145400950313     C     OPZ430        WHENEQ    '03'
145500950313     C* se bottone presente
145600950313     C     EXIC          IFEQ      '1'
145700950313     C* lo predispongo per la copia
145800950313     C                   MOVE      1             D2B(Y)
145900950313     C* se non presente
146000950313     C                   ELSE
146100950313     C* lo inibisco alla copia
146200021017A0973X***                MOVE      2             D2B(Y)
146300021017A0973C                   EVAL      D2BA(Y) = Protetto
146400950313     C                   ENDIF
146500950313     C*
146600950313     C                   OTHER
146700950313     C                   SELECT
146800950313     C* se richiesto di inibire il bottone
146900950313     C     EXIC          WHENEQ    '2'
147000950313     C* lo predispongo per l'inibizione
147100021017A0973X***                MOVE      2             D2B(Y)
147200021017A0973C                   EVAL      D2BA(Y) = Protetto
147300951004     C* se c'è un errore
147400950313     C     ERRC          WHENEQ    '1'
147500950313     C* oppure il bottone è previsto dalla gerarchia
147600970923     C     CHC           ORNE      98
147700970923     C     CHC           andne     99
147800950313     C* questo è valido solo nell'inizializzazione della videata
147900950313     C* perchè nelle impostazioni successive le operazioni già eseguite
148000950313     C* non devono essere reimpostate
148100950313     C     $INZD1        ANDEQ     *ON
148200950313     C* lo predispongo per la chiamata
148300950313     C                   MOVE      1             D2B(Y)
148400950313     C*
148500950313     C                   ENDSL
148600950313     C*
148700950313     C                   ENDSL
148800950313     C*
148900950313     C* predispongo i testi a secondo dell'esistenza o meno del bottone
149000950313     C* e dell'eventuale errore che ciò genera
149100950313     C                   MOVE      *BLANK        D2T(Y)
149200950313     C                   SELECT
149300950313     C     EXIC          WHENEQ    '0'
149400950313     C     ERRC          ANDEQ     '1'
149500950313     C                   MOVE      INSER         D2T(Y)
149600950313     C     EXIC          WHENEQ    '1'
149700950313     C     ERRC          ANDEQ     '1'
149800950313     C                   MOVE      CORR          D2T(Y)
149900950313     C     EXIC          WHENEQ    '1'
150000950313     C     ERRC          ANDEQ     '0'
150100950313     C                   MOVE      PRES          D2T(Y)
150200950313     C     EXIC          WHENEQ    '0'
150300950313     C     ERRC          ANDEQ     '0'
150400950313     C                   MOVE      BLANK         D2T(Y)
150500950313     C     EXIC          WHENEQ    '0'
150600950313     C     ERRC          ANDEQ     '2'
150700950313     C                   MOVE      ANNUL         D2T(Y)
150800950426     C     EXIC          WHENEQ    '2'
150900950426     C     ERRC          ANDEQ     '0'
151000950426     C                   MOVE      BLANK         D2T(X)
151100950313     C                   ENDSL
151200950313     C*
151300950313     C                   ENDSR
151400941228     C/EJECT
151500941228     C************************************************************
151600941228     C* TEST BOTTONE COMMERCIALI GENERALI
151700941228     C************************************************************
151800950124     C     TSTFRN        BEGSR
151900950303     C*
152000950124     C                   RESET                   A431DS
152100950124     C                   MOVEL     WWWSOC        SOC431
152200950124     C                   MOVEL     KSC430        KSC431
152300950124     C                   MOVEL     A431DS        KPJBU
152400950124     C                   CALL      'ANA436R'
152500950103     C                   PARM                    KPJBA
152600950124     C                   MOVEL     KPJBU         A431DS
152700950124     C                   MOVEL     EXI431        EXI
152800950124     C                   MOVEL     ERR431        ERR
152900950303     C*
153000950426     C* se sono in immissione del fornitore in quanto tale (ovvero
153100950426     C*  nessuna opzione diretta da parte del chiamante ma F10)
153200950426     C     OPZ430        IFEQ      '01'
153300951004     C     OPD430        ANDEQ     *OFF
153400950426     C*
153500950426     C* se il risultato del test è di non esistenza
153600950426     C* forzo la richiesta del bottone
153700950426     C     EXI           IFEQ      '0'
153800950426     C                   MOVEL     '0'           EXI
153900950426     C                   MOVEL     '1'           ERR
154000950426     C                   ENDIF
154100950426     C*
154200950426     C                   ENDIF
154300941228     C*
154400941228     C                   ENDSR
154500950206     C/EJECT
154600950206     C************************************************************
154700970923     C* TEST BOTTONE AGENTI generali
154800950206     C************************************************************
154900950206     C     TSTAGE        BEGSR
155000950206     C*
155100970923     C                   RESET                   A439DS
155200970923     C                   MOVEL     WWWSOC        SOC439
155300970923     C                   MOVEL     KSC430        KSC439
155400970923     C                   MOVEL     '1'           UNI439
155500970923     C                   MOVEL     A439DS        KPJBU
155600950206     C                   CALL      'ANA439R'
155700950206     C                   PARM                    KPJBA
155800970923     C                   MOVEL     KPJBU         A439DS
155900970923     C                   MOVEL     EXI439        EXI
156000970923     C                   MOVEL     ERR439        ERR
156100950206     C*
156200950206     C                   ENDSR
156300950227     C/EJECT
156400970923     C************************************************************
156500970923     C* TEST BOTTONE AGENTI per unità
156600970923     C************************************************************
156700970923     C     TSTAGU        BEGSR
156800970923     C*
156900970923     C                   RESET                   A439DS
157000970923     C                   MOVEL     WWWSOC        SOC439
157100970923     C                   MOVEL     KSC430        KSC439
157200970923     C                   MOVEL     '2'           UNI439
157300970923     C                   MOVEL     A439DS        KPJBU
157400970923     C                   CALL      'ANA439R'
157500970923     C                   PARM                    KPJBA
157600970923     C                   MOVEL     KPJBU         A439DS
157700970923     C                   MOVEL     EXI439        EXI
157800970923     C                   MOVEL     ERR439        ERR
157900970923     C*
158000970923     C                   ENDSR
158100970923     C/EJECT
158200950227     C************************************************************
158300950227     C* TEST SPEDIZIONIERI
158400950227     C************************************************************
158500950227     C     TSTSPE        BEGSR
158600950227     C*
158700950303     C     OPZ430        IFEQ      '03'
158800950303     C                   MOVEL     '2'           EXI
158900950303     C                   MOVEL     '0'           ERR
159000950303     C                   ELSE
159100950227     C                   RESET                   A431DS
159200950227     C                   MOVEL     WWWSOC        SOC431
159300950227     C                   MOVEL     KSC430        KSC431
159400950227     C                   MOVEL     A431DS        KPJBU
159500950227     C                   CALL      'ANA440R'
159600950227     C                   PARM                    KPJBA
159700950227     C                   MOVEL     KPJBU         A431DS
159800950227     C                   MOVEL     EXI431        EXI
159900950227     C                   MOVEL     ERR431        ERR
160000950303     C                   ENDIF
160100950227     C*
160200950227     C                   ENDSR
160300941228     C/EJECT
160400941228     C************************************************************
160500941228     C* TEST BOTTONE DATI ANAGRAFICI
160600941228     C************************************************************
160700941228     C     TSTSOG        BEGSR
160800941228     C*
160900950104     C                   SELECT
161000950124     C     OPZ430        WHENEQ    '01'
161100951004     C     OPD430        ANDEQ     *off
161200950103     C                   MOVEL     '2'           EXI
161300941227     C                   MOVEL     '0'           ERR
161400950124     C     OPZ430        WHENEQ    '03'
161500950104     C                   MOVEL     '2'           EXI
161600950104     C                   MOVEL     '0'           ERR
161700950104     C                   OTHER
161800950104     C                   MOVEL     '1'           EXI
161900950104     C                   MOVEL     '0'           ERR
162000950104     C                   ENDSL
162100941228     C*
162200941228     C                   ENDSR
162300941228     C/EJECT
162400941228     C************************************************************
162500941228     C* TEST BOTTONE COMMENTI
162600941228     C************************************************************
162700941228     C     TSTTCM        BEGSR
162800941228     C*
162900941227     C                   RESET                   A724DS
163000941227     C                   MOVEL     '01'          OPZ724
163100950110     C                   MOVEL     WWWSOC        SOC724
163200941227     C                   MOVEL     *BLANK        UNI724
163300950125     C                   MOVEL     '001'         TPA724
163400950124     C                   MOVEL     KSC430        KY1724
163500941227     C                   MOVEL     WDATE         DTS724
163600941227     C                   MOVEL     A724DS        KPJBU
163700941227     C                   CALL      'ANA724R'
163800941227     C                   PARM                    KPJBA
163900941227     C                   MOVEL     KPJBU         A724DS
164000950103     C     EXI724        IFEQ      '0'
164100950103     C                   MOVEL     '0'           EXI
164200941227     C                   ELSE
164300950103     C                   MOVEL     '1'           EXI
164400941227     C                   ENDIF
164500941227     C                   MOVEL     '0'           ERR
164600941228     C*
164700941228     C                   ENDSR
164800941228     C/EJECT
164900941228     C************************************************************
165000941228     C* TEST BOTTONE FILIALI
165100941228     C************************************************************
165200941228     C     TSTFIL        BEGSR
165300941228     C*
165400950124     C                   RESET                   A431DS
165500950124     C                   MOVEL     WWWSOC        SOC431
165600950124     C                   MOVEL     KSC430        KSC431
165700950124     C                   MOVEL     A431DS        KPJBU
165800950124     C                   CALL      'ANA432R'
165900941227     C                   PARM                    KPJBA
166000950124     C                   MOVEL     KPJBU         A431DS
166100950124     C                   MOVEL     EXI431        EXI
166200950124     C                   MOVEL     ERR431        ERR
166300941228     C*
166400941228     C                   ENDSR
166500941228     C/EJECT
166600941228     C************************************************************
166700941228     C* TEST BOTTONE DATI STATISTICI
166800941228     C************************************************************
166900950124     C     TSTFRS        BEGSR
167000941228     C*
167100950124     C                   RESET                   A431DS
167200950124     C                   MOVEL     WWWSOC        SOC431
167300950124     C                   MOVEL     KSC430        KSC431
167400950124     C                   MOVEL     A431DS        KPJBU
167500950124     C                   CALL      'ANA433R'
167600941227     C                   PARM                    KPJBA
167700950124     C                   MOVEL     KPJBU         A431DS
167800950124     C                   MOVEL     EXI431        EXI
167900950124     C                   MOVEL     ERR431        ERR
168000941228     C*
168100941228     C                   ENDSR
168200941228     C/EJECT
168300951010     C************************************************************
168400951010     C* TEST BOTTONE DICHIARAZIONE D'INTENTI
168500951010     C************************************************************
168600951010     C     TSTDIC        BEGSR
168700951010     C*
168800951010     C                   RESET                   A331DS
168900951010     C                   MOVEL     WWWSOC        SOC331
169000951010     C                   MOVEL     KSC430        KSC331
169100951011     C                   MOVEL     'F'           COF331
169200951010     C                   MOVEL     A331DS        KPJBU
169300951010     C                   CALL      'ANA334R'
169400951010     C                   PARM                    KPJBA
169500951010     C                   MOVEL     KPJBU         A331DS
169600951010     C                   MOVEL     EXI331        EXI
169700951010     C                   MOVEL     ERR331        ERR
169800951010     C*
169900951010     C                   ENDSR
170000951010     C/EJECT
170100950119     C************************************************************
170200950119     C* TEST BOTTONE APPOGGI BANCARI
170300950119     C************************************************************
170400950119     C     TSTCBA        BEGSR
170500950119     C*
170600950124     C                   RESET                   A431DS
170700950124     C                   MOVEL     WWWSOC        SOC431
170800950124     C                   MOVEL     KSC430        KSC431
170900950124     C                   MOVEL     A431DS        KPJBU
171000950124     C                   CALL      'ANA438R'
171100950119     C                   PARM                    KPJBA
171200950124     C                   MOVEL     KPJBU         A431DS
171300950124     C                   MOVEL     EXI431        EXI
171400950124     C                   MOVEL     ERR431        ERR
171500950119     C*
171600950119     C                   ENDSR
171700941228     C/EJECT
171800950113     C************************************************************
171900950113     C* TEST BOTTONE PERIODI ESCLUSIONE PAGAMENTO
172000950113     C************************************************************
172100950113     C     TSTPEP        BEGSR
172200950113     C*
172300950124     C                   RESET                   A431DS
172400950124     C                   MOVEL     WWWSOC        SOC431
172500950124     C                   MOVEL     KSC430        KSC431
172600950124     C                   MOVEL     A431DS        KPJBU
172700950124     C                   CALL      'ANA437R'
172800950113     C                   PARM                    KPJBA
172900950124     C                   MOVEL     KPJBU         A431DS
173000950124     C                   MOVEL     EXI431        EXI
173100950124     C                   MOVEL     ERR431        ERR
173200950113     C*
173300950113     C                   ENDSR
173400950313     C/EJECT
173500950313     C************************************************************
173600950313     C* TEST BOTTONE CAPOCONTO
173700950313     C************************************************************
173800950313     C     TSTPDC        BEGSR
173900950313     C*
174000950313     C                   RESET                   A231DS
174100950313     C                   MOVEL     WWWSOC        SOC231
174200950313     C                   MOVEL     WWWKCC        KCC231
174300950313     C                   MOVEL     KSC430        KSC231
174400950313     C                   MOVE      '4'           MOD231
174500950313     C                   MOVEL     A231DS        KPJBU
174600950313     C                   CALL      'ANA234R'
174700950313     C                   PARM                    KPJBA
174800950313     C                   MOVEL     KPJBU         A231DS
174900950313     C                   MOVEL     EXI231        EXIC
175000950313     C                   MOVEL     ERR231        ERRC
175100951004     C* Riporto il codice capoconto, se il record esiste o se è
175200951004     C* annullato, nel campo WWWKCC,
175300951004     C* per gli altri programmi, altrimenti lo imposta il
175400951004     C* programma di definizione capoconto (ANA206R)
175500950313     C     EXIC          IFEQ      '1'
175600950313     C     EXIC          OREQ      '0'
175700950313     C     ERRC          ANDEQ     '2'
175800950313     C                   MOVE      KCC231        WWWKCC
175900950615     C                   MOVE      ANL231        WWWANL
176000950313     C                   ENDIF
176100950313     C*
176200951004     C                   SELECT
176300951004     C* Se non esiste
176400951004     C     EXIC          WHENEQ    '0'
176500951004     C* non è annullato
176600951004     C     ERRC          ANDNE     '2'
176700951004     C* e non siamo in visualizzazione
176800951004     C     OPZ430        ANDNE     '05'
176900951004     C* forzo inserimento
177000951004     C                   MOVE      1             D2B01
177100951004     C                   MOVEL     '1'           ERRC
177200951004     C                   ENDSL
177300951004     C*
177400951004     C* Salvo esistenza record di contabilità, perchè mi serve
177500951004     C* per l'opzione da passare agli altri programmi di contabilita
177600951004     C                   MOVE      EXIC          EXICNT            1
177700951004     C* Se annullato esiste
177800951004     C     EXIC          IFEQ      '0'
177900951004     C     ERRC          ANDEQ     '2'
178000951004     C                   MOVE      '1'           EXICNT
178100951004     C                   ENDIF
178200951004     C*
178300951004     C* predispongo i flag di esistenza/errore per la globalità del bottone
178400951004     C* (mi interessa far vedere solo se almeno il capoconto è stato
178500951004     C*  impostato, anche annullato; non posso sapere se c'è un  errore
178600951004     C*  perchè dovrei testare tutti gli specifici bottoni di contabilità)
178700951004     C                   MOVE      EXICNT        EXI
178800951004     C                   MOVE      '0'           ERR
178900950313     C*
179000950313     C                   ENDSR
179100950313     C/EJECT
179200950313     C************************************************************
179300950313     C* TEST BOTTONE REGOLE CONTABILI
179400950313     C************************************************************
179500950313     C     TSTRGC        BEGSR
179600950313     C*
179700950313     C                   RESET                   A231DS
179800950313     C                   MOVEL     WWWSOC        SOC231
179900950313     C                   MOVEL     WWWKCC        KCC231
180000950313     C                   MOVEL     KSC430        KSC231
180100950313     C                   MOVEL     A231DS        KPJBU
180200950313     C                   CALL      'ANA232R'
180300950313     C                   PARM                    KPJBA
180400950313     C                   MOVEL     KPJBU         A231DS
180500950313     C                   MOVEL     EXI231        EXIC
180600950313     C                   MOVEL     ERR231        ERRC
180700950313     C*
180800950313     C                   ENDSR
180900950313     C/EJECT
181000950313     C************************************************************
181100950313     C* TEST BOTTONE ATTRIBUTI CONTABILI
181200950313     C************************************************************
181300950313     C     TSTATC        BEGSR
181400950313     C*
181500950313     C                   RESET                   A231DS
181600950313     C                   MOVEL     WWWSOC        SOC231
181700950313     C                   MOVEL     WWWKCC        KCC231
181800950313     C                   MOVEL     KSC430        KSC231
181900950313     C                   MOVEL     A231DS        KPJBU
182000950313     C                   CALL      'ANA233R'
182100950313     C                   PARM                    KPJBA
182200950313     C                   MOVEL     KPJBU         A231DS
182300950313     C                   MOVEL     EXI231        EXIC
182400950313     C                   MOVEL     ERR231        ERRC
182500950313     C*
182600950313     C                   ENDSR
182700950313     C/EJECT
182800950313     C************************************************************
182900950313     C* TEST BOTTONE PERCIPIENTE
183000950313     C************************************************************
183100950313     C     TSTPER        BEGSR
183200950313     C*
183300950313     C                   RESET                   A231DS
183400950313     C                   MOVEL     WWWSOC        SOC231
183500950313     C                   MOVEL     WWWKCC        KCC231
183600950313     C                   MOVEL     KSC430        KSC231
183700950313     C                   MOVE      '4'           MOD231
183800950313     C                   MOVEL     A231DS        KPJBU
183900950313     C                   CALL      'ANA236R'
184000950313     C                   PARM                    KPJBA
184100950313     C                   MOVEL     KPJBU         A231DS
184200950313     C                   MOVEL     EXI231        EXIC
184300950313     C                   MOVEL     ERR231        ERRC
184400950313     C*
184500950313     C                   ENDSR
184600950313     C/EJECT
184700950313     C************************************************************
184800950313     C* TEST BOTTONE VOCI ANALITICA
184900950313     C************************************************************
185000950313     C     TSTVCA        BEGSR
185100950615     C*
185200950615     C                   RESET                   A231DS
185300950615     C                   MOVEL     WWWSOC        SOC231
185400950615     C                   MOVEL     WWWKCC        KCC231
185500950615     C                   MOVEL     KSC430        KSC231
185600950615     C                   MOVE      WWWANL        ANL231
185700950615     C                   MOVE      '2'           MOD231
185800950615     C                   MOVEL     A231DS        KPJBU
185900950615     C                   CALL      'ANA237R'
186000950615     C                   PARM                    KPJBA
186100950615     C                   MOVEL     KPJBU         A231DS
186200950615     C                   MOVEL     EXI231        EXIC
186300950615     C                   MOVEL     ERR231        ERRC
186400950313     C*
186500950313     C                   ENDSR
186600980624     C/EJECT
186700980624     C************************************************************
186800980624     C* TEST BOTTONE FIDI
186900980624     C************************************************************
187000980624     C     TSTFID        BEGSR
187100980624     C*
187200980624     C                   RESET                   A360DS
187300980624     C                   MOVEL     WWWSOC        SOCIETA360
187400980629     C                   MOVEL     WWWKCC        KCC360
187500980624     C                   MOVEL     KSC430        KSC360
187600980624     C                   MOVEL     '1'           tpfido360
187700980624     C                   MOVEL     'F'           validit360
187800980625     C                   MOVEL     A360DS        KPJBU
187900980625      *
188000980629     C                   CALL      'ANA377R'
188100980624     C                   PARM                    KPJBA
188200980625      *
188300980624     C                   MOVEL     KPJBU         A360DS
188400980629     C                   MOVEL     RET360        EXI
188500980629     C                   MOVEL     ERR360        ERR
188600050629B0818C* se non trovato prova col fido '2'=factor
188700050629  "  C                   IF        Ret360 =  '0'
188800050629  "  C                   RESET                   A331DS
188900050629  "  C                   MOVEL     WWWSOC        societa360
189000050629  "  C                   MOVEL     KSC430        ksc360
189100050629  "  C                   MOVEL     wwwKCC        kcc360
189200050629  "  C                   MOVEL     '2'           tpfido360
189300050629  "  C                   MOVEL     'C'           validit360
189400050629  "  C                   MOVEL     A360DS        KPJBU
189500050629  "  C                   CALL      'ANA377R'
189600050629  "  C                   PARM                    KPJBA
189700050629  "  C                   MOVEL     KPJBU         A360DS
189800050629  "  c* se trovato
189900050629  "  c                   if        Ret360<>'0'
190000050629  "  C                   MOVEL     'F'           EXI
190100050629  "  C                   MOVEL     ERR360        ERR
190200050629  "  c                   endif
190300050629  "  C* se non trovato prova col fido '3'=assicurazione crediti
190400050629  "  C                   IF        Ret360 =  '0'
190500050629  "  C                   RESET                   A331DS
190600050629  "  C                   MOVEL     WWWSOC        societa360
190700050629  "  C                   MOVEL     KSC430        ksc360
190800050629  "  C                   MOVEL     wwwKCC        kcc360
190900050629  "  C                   MOVEL     '3'           tpfido360
191000050629  "  C                   MOVEL     'C'           validit360
191100050629  "  C                   MOVEL     A360DS        KPJBU
191200050629  "  C                   CALL      'ANA377R'
191300050629  "  C                   PARM                    KPJBA
191400050629  "  C                   MOVEL     KPJBU         A360DS
191500050629  "  c* se trovato
191600050629  "  c                   if        Ret360<>'0'
191700050629  "  C                   MOVEL     'A'           EXI
191800050629  "  C                   MOVEL     ERR360        ERR
191900050629  "  c                   endif
192000050629  "  c*
192100050629  "  c                   endif
192200050629  "  c*
192300050629B0818c                   endif
192400980624     C*
192500980624     C                   ENDSR
192600980624     C/EJECT
192700980624     C************************************************************
192800980624     C* TEST BOTTONE SOCI PER AGENTE
192900980624     C************************************************************
193000980624     C     TSTSOA        BEGSR
193100980624     C*
193200980706      * Se non sono presenti agenti non è possibile immettere un
193300980706      * socio
193400980706     C                   if        exiage = '0'
193500980706     C                   move      '2'           exi
193600980706     C                   move      '0'           err
193700980706      *
193800980706     C                   ELSE
193900980624     C                   RESET                   A600DS
194000980624     C                   MOVEL     WWWSOC        SOC600
194100980624     C                   MOVEL     KSC430        KSC600
194200980624     C                   MOVEL     A600DS        KPJBU
194300980629     C                   CALL      'ANA378R'
194400980624     C                   PARM                    KPJBA
194500980624     C                   MOVEL     KPJBU         A600DS
194600980629     C                   MOVEL     RET600        EXI
194700980629     C                   MOVEL     ERR600        ERR
194800980706     C                   endif
194900980624     C*
195000980624     C                   ENDSR
195100941220     C/EJECT
195200010830G1036C************************************************************
195300010830  "  C* Test bottone linee di acquisto
195400010830  "  C************************************************************
195500010830G1036C     TstLDA        begsr
195600010830     C*
195700010831     C                   eval      DrOpz = '3'
195800010831     C                   eval      DrErr = *Off
195900010831     C                   eval      DrNonAlloc = *On
196000010831     C*
196100010831     C                   callb     'PJDR0101R'
196200010831     C                   parm                    WWWSoc
196300010831     C                   parm                    Ksc430
196400010831     C                   parm                    *omit
196500010831     C                   parm                    *omit
196600010831     C                   parm                    DrOpz
196700010831     C                   parm                    DrNonAlloc
196800010831     C                   parm                    DrErr
196900010831     C*
197000010831     C                   if        DrErr = *Off
197100010831     C                   eval      Exi = '1'
197200010831     C                   else
197300010831     C                   eval      Exi = '0'
197400010831     C                   endif
197500010831     C                   eval      Err = '0'
197600010830     C*
197700010830G1036C                   endsr
197800010830G1036C************************************************************
197900010830  "  C* Test bottone anagrafica fornitori/magazzini
198000010830  "  C************************************************************
198100010830G1036C     TstFDM        begsr
198200010830     C*
198300010831     C                   eval      DrOpz = '3'
198400010831     C                   eval      DrErr = *Off
198500010831     C                   eval      DrNonAlloc = *On
198600010831     C*
198700010831     C                   callb     'PJDR0201R'
198800010831     C                   parm                    WWWSoc
198900010831     C                   parm                    Ksc430
199000010831     C                   parm                    *omit
199100010831     C                   parm                    *omit
199200010831     C                   parm                    *omit
199300010831     C                   parm                    *omit
199400010831     C                   parm                    *omit
199500010831     C                   parm                    *omit
199600010831     C                   parm                    DrOpz
199700010831     C                   parm                    DrNonAlloc
199800010831     C                   parm                    DrErr
199900010831     C*
200000010831     C                   if        DrErr = *Off
200100010831     C                   eval      Exi = '1'
200200010831     C                   else
200300010831     C                   eval      Exi = '0'
200400010831     C                   endif
200500010831     C                   eval      Err = '0'
200600010830     C*
200700010830G1036C                   endsr
200800010830G1036C************************************************************
200900010830  "  C* Test bottone assortimenti fornitori
201000010830  "  C************************************************************
201100010830G1036C     TstASF        begsr
201200010830     C*
201300010831     C                   eval      DrOpz = '3'
201400010831     C                   eval      DrErr = *Off
201500010831     C                   eval      DrNonAlloc = *On
201600010831     C*
201700010831     C                   callb     'PJDR0202R'
201800010831     C                   parm                    WWWSoc
201900010831     C                   parm                    Ksc430
202000010831     C                   parm                    *omit
202100010831     C                   parm                    *omit
202200010831     C                   parm                    *omit
202300010831     C                   parm                    *omit
202400010831     C                   parm                    *omit
202500010831     C                   parm                    *omit
202600010831     C                   parm                    DrOpz
202700010831     C                   parm                    DrNonAlloc
202800010831     C                   parm                    DrErr
202900010831     C*
203000010831     C                   if        DrErr = *Off
203100010831     C                   eval      Exi = '1'
203200010831     C                   else
203300010831     C                   eval      Exi = '0'
203400010831     C                   endif
203500010831     C                   eval      Err = '0'
203600010830     C*
203700010830G1036C                   endsr
203800941220     C************************************************************
203900941220     C* GESTIONE F03 VIDEO D1
204000941220     C************************************************************
204100941220     C     F03D1         BEGSR
204200941220     C*
204300940309     C                   MOVE      *ON           $FINE
204400950124     C                   MOVE      '1'           RET430
204500940325     C* fine programma
204600940325     C                   EXSR      ENDPGM
204700940325     C*
204800940325     C                   ENDSR
204900940309     C/EJECT
205000940309     C************************************************************
205100940309     C* GESTIONE F05 VIDEO D1
205200940309     C************************************************************
205300940309     C     F05D1         BEGSR
205400940309     C*
205500940309     C                   MOVE      *ON           $INZD1
205600940309     C*
205700940309     C                   ENDSR
205800940309     C/EJECT
205900940309     C************************************************************
206000940309     C* GESTIONE F08 VIDEO D1
206100940309     C************************************************************
206200940309     C     F08D1         BEGSR
206300940309     C*
206400940309     C                   MOVE      *ON           $FINE
206500950124     C                   MOVE      '0'           RET430
206600940309     C*
206700940309     C                   ENDSR
206800940309     C/EJECT
206900940309     C************************************************************
207000940309     C* GESTIONE F12 VIDEO D1
207100940309     C************************************************************
207200940309     C     F12D1         BEGSR
207300940309     C*
207400940309     C                   MOVE      *ON           $FINE
207500950124     C                   MOVE      '2'           RET430
207600940309     C*
207700940309     C                   ENDSR
207800940309     C/EJECT
207900940309     C************************************************************
208000940309     C* GESTIONE F24 VIDEO D1
208100940309     C************************************************************
208200940309     C     F24D1         BEGSR
208300940506     C*
208400940506     C                   MOVEA     $FX1          $FX
208500940506     C                   MOVEA     $FC1          $FC
208600940506     C                   MOVEA     $FR1          $FR
208700940506     C                   Z-ADD     $ULKD1        $ULK
208800940506     C                   EXSR      RIEKEY
208900940506     C                   Z-ADD     $ULK          $ULKD1            3 0
209000940720     C                   MOVEA     $FC           $FC1
209100940506     C                   MOVEL     $KEY1         Z1KE1
209200060224A1148C                   MOVEL     $ALLFUNCT     H1ALLFUNCT
209300940506     C                   MOVEL     $KEY2         Z1KE2
209400941219     C                   WRITE     FMTZ1
209500940506     C*
209600940309     C                   ENDSR
209700940207     C/EJECT
209800940131     C************************************************************
209900940207     C* CONTROLLO VIDEATA
210000940131     C************************************************************
210100940127     C     CTRD1         BEGSR
210200940131     C*
210300940127     C                   SETOFF                                       99
210400940127     C* non imposto pos. del cursore
210500940127     C                   Z-ADD     0             H1RIGA
210600940127     C                   Z-ADD     0             H1COLO
210700941212     C*
210800950426     C* FORZO L'OBBLIGATORIETA' DELLA SELEZIONE DEI BOTTONI
210900950426     C* DA "INSERIRE" NEL CASO DI IMMISSIONE
211000950426     C*
211100950426     C     OPZ430        IFEQ      '01'
211200951004     C     OPD430        ANDEQ     *off
211300950426     C*
211400950426     C                   DO        NB1           X
211500950426     C*
211600950426     C                   MOVEL     MIX(X)        GERARK
211700950426     C*
211800950426     C     D1B(X)        IFNE      1
211900950426     C     EXI           ANDEQ     '0'
212000950426     C     ERR           ANDEQ     '1'
212100950426     C                   MOVE      1             D1B(X)
212200950426     C                   ENDIF
212300950426     C*
212400950426     C                   ENDDO
212500950426     C*
212600950426     C                   ENDIF
212700950303     C*
212800940117     C                   ENDSR
212900940616     C/EJECT
213000940616     C************************************************************
213100950313     C* GESTIONE CHIAMATE PGM ANAGRAFICHE
213200940616     C************************************************************
213300950313     C     GESANA        BEGSR
213400940616     C*
213500941229     C                   MOVEL     *OFF          $ESCI
213600950124     C                   MOVEL     WEXF          EXF430
213700940718     C*
213800950313     C                   DO        NB1           X
213900941205     C*
214000950118     C     D1B(X)        IFEQ      1
214100941228     C                   MOVEL     MIX(X)        GERARK
214200950313     C                   EXSR      CALANA
214300941228     C* salvo in schiera il suo stato
214400941228     C                   MOVEL     GERARK        MIX(X)
214500951004     C*
214600951004     C* se sto visualizzando il filtro delle chiamate principali
214700951004     C     $GEST         IFEQ      'D1'
214800951004     C* oppure ho completato la richieste senza interruzzioni
214900951004     C     $ESCI         OREQ      *OFF
215000941228     C* imposto il bottone e il relativo testo
215100950313     C                   EXSR      IMPANA
215200951004     C                   ENDIF
215300951004     C*
215400941219     C     $ESCI         IFEQ      *ON
215500940712     C                   LEAVE
215600941219     C                   ENDIF
215700951004     C*
215800941219     C                   ENDIF
215900941219     C                   ENDDO
216000970217     C*
216100970217     C* GESTIONE CHIAMATA AL PGM DI INTERFACCIA ALTRI SISTEMI INFORMATIVI
216200970217     C                   SELECT
216300970217     C*
216400970217     C* se ho ultimato il ciclo senza nessuna interruzione
216500090504>>>>>C**   $ESCI         WHENEQ    *OFF
216600970217     C* sono in immissione
216700090504>>>>>C**   OPZ430        ANDEQ     '01'
216800090504>>>>>C     OPZ430        wheneq    '01'
216900970217     C* di tutto il cliente e non di una opzione diretta
217000970217     C     OPD430        ANDEQ     *OFF
217100970217     C* se richiesto un programma dai parametri moduli
217200970217     C     pgm020        ANDNE     *blanks
217300970217     c* e se la società non è fittizia
217400001009b0278c**** xscscf        andne     'S'
217500001009b0278c     xscscf        andne     *on
217600970217     c                   eval      opz000 = opz430
217700970217     c                   eval      societa000 = wwwsoc
217800970217     c                   eval      ksc000     = ksc430
217900970217     c                   eval      kpjbu = ana000ds
218000970217     c                   call      'X38CALL'
218100970217     C                   parm                    pgm020
218200970217     C                   parm                    lib020
218300970217     C                   parm                    tpa020
218400970217     C                   parm                    kpjba
218500970217     c* ritorno
218600970217     c                   eval      ana000ds = kpjbu
218700970217     c* gestione ritorno
218800970217     c                   exsr      Rtn000
218900970217     c                   endsl
219000951004     C*
219100951004     C                   SELECT
219200951004     C*
219300951004     C* se ho ultimato il ciclo senza nessuna interruzione
219400951004     C     $ESCI         WHENEQ    *OFF
219500951004     C* sono in immissione
219600951004     C     OPZ430        ANDEQ     '01'
219700951004     C* di tutto il cliente e non di una opzione diretta
219800951004     C     OPD430        ANDEQ     *OFF
219900951004     C* reimposto la gerarchia d'ingresso
220000951004     C                   MOVEL     WD1B          D1B
220100951004     C                   MOVEL     WD2B          D2B
220200951004     C                   MOVEL     WD1T          D1T
220300951004     C                   MOVEL     WD2T          D2T
220400951004     C                   MOVEL     '0'           EXICNT
220500951004     C                   MOVEA     WMIX          MIX
220600951004     C                   MOVEA     WMIX1         MIX1
220700951004     C* pulisco i campi della DS del pgm con chiamata obbligatoria
220800951004     C* che sono stati valorizzati
220900951004     C                   CLEAR                   KSC430                                     chiamant
221000951004     C                   CLEAR                   SOG430                                     chiamant
221100951004     C                   CLEAR                   TII430
221200951004     C                   CLEAR                   COI430
221300951004     C                   MOVE      *BLANK        WWWKCC
221400951004     C                   MOVE      *OFF          $FINE
221500951004     C* e riparto da capo
221600951004     C                   MOVE      'D1'          $GEST
221700951004     C*
221800951004     C* se ho ultimato il ciclo senza nessuna interruzione
221900951004     C     $ESCI         WHENEQ    *OFF
222000951004     C* e non sono in immissione
222100951004     C     OPZ430        ANDNE     '01'
222200951004     C* oppure ho ultimato il ciclo senza nessuna interruzione
222300951004     C     $ESCI         OREQ      *OFF
222400951004     C* sono in immissione
222500951004     C     OPZ430        ANDEQ     '01'
222600951004     C* ma di una opzione diretta
222700951004     C     OPD430        ANDEQ     *ON
222800951004     C* esco da pgm
222900951004     C                   MOVE      *ON           $FINE
223000951004     C                   MOVE      WERR          ERR430
223100951004     C*
223200951004     C* alla prima interruzione
223300951004     C* se non sto gestendo il video
223400951004     C     WESP          WHENEQ    *OFF
223500951004     C     $ESCI         ANDEQ     *ON
223600951004     C* esco da pgm
223700951004     C                   MOVE      *ON           $FINE
223800951004     C                   MOVE      WERR          ERR430
223900951004     C* comunicando alla guida di interrompere il suo ciclo come fosse F12
224000951004     C                   MOVE      '2'           RET430
224100951004     C*
224200951004     C                   ENDSL
224300950926     C*
224400940616     C                   ENDSR
224500941205     C/EJECT
224600941205     C************************************************************
224700950313     C* CHIAMATE VARIABILI PGM ANAGRAFICI
224800941205     C************************************************************
224900950313     C     CALANA        BEGSR
225000941205     C*
225100941219     C                   SELECT
225200941228     C*------------------------------------------*
225300941219     C* COMMERCIALI GENERALI
225400941228     C*------------------------------------------*
225500950124     C     PGM           WHENEQ    'FRN'
225600980630     c                   eval      wOPZ    = OPZ430
225700980630     c                   eval      wFGi    = fgiComG020
225800980630     c                   eval      wFGa    = fgaComG020
225900980630     c                   exsr      TestANP020
226000950124     C                   RESET                   A401DS
226100980630     c* forzo l'opzione impostata dalla subr. TestANP026
226200980630     c* (se non sono in ottica di gruppo a livello aziendale
226300980630     c*  essa corrisponde a quella impostata dalle istruzioni
226400980630     c*  soprastanti...)
226500980630     c                   movel     wopz          opz401
226600980630     C*
226700950124     C                   MOVE      FSC430        FSC401
226800980630     C     CMT430        IFEQ      '1'
226900980630     C                   MOVEL     '1'           CMT401
227000980630     C                   ELSE
227100980630     C                   MOVEL     '0'           CMT401
227200980630     C                   ENDIF
227300950124     C                   MOVEL     KSC430        KSC401
227400950306     C                   MOVEL     SOG430        SOG401
227500950427     C                   MOVEL     DF1430        DF1401
227600950912     C                   MOVEL     B02430        CTB401
227700950124     C                   MOVEL     A401DS        KPJBU
227800950124     C                   CALL      'ANA401R'
227900941219     C                   PARM                    KPJBA
228000950103     C                   PARM                    AUTDS
228100950124     C                   MOVEL     KPJBU         A401DS
228200941219     C* ritorno da PGM
228300950124     C                   EXSR      RTN401
228400950206     C*------------------------------------------*
228500970919     C* AGENTI generali
228600950206     C*------------------------------------------*
228700950206     C     PGM           WHENEQ    'AGE'
228800950206     C                   RESET                   A441DS
228900950206     C*
229000950206     C                   SELECT
229100950206     C*
229200950206     C* se richiesta immissione
229300950206     C     OPZ430        WHENEQ    '01'
229400950206     C* richiamo il pgm in immissione
229500950206     C                   MOVE      '01'          OPZ441
229600950206     C*
229700950206     C* se richiesta modifica
229800950206     C     OPZ430        WHENEQ    '02'
229900950206     C* se il bottone è presente
230000950206     C     EXI           IFEQ      '1'
230100950206     C* richiamo il pgm in manutenzione
230200950206     C                   MOVE      '02'          OPZ441
230300950206     C* se il bottone non è presente
230400950206     C                   ELSE
230500950404     C* richiamo il pgm in immissione
230600950404     C                   MOVE      '01'          OPZ441
230700950206     C                   ENDIF
230800950206     C*
230900950206     C* se richiesta visualizzazione
231000950206     C     OPZ430        WHENEQ    '05'
231100950206     C* richiamo il pgm in visualizzazione
231200950206     C                   MOVE      '05'          OPZ441
231300950206     C*
231400950206     C                   ENDSL
231500980630     c*
231600980630     c                   eval      wOPZ    = OPZ441
231700980630     c                   eval      wFGi    = fgiAGE020
231800980630     c                   eval      wFGa    = fgaAGE020
231900980630     c                   exsr      TestANP020
232000980630     c* forzo l'opzione impostata dalla subr. TestANP020
232100980630     c* (se non sono in ottica di gruppo a livello aziendale
232200980630     c*  essa corrisponde a quella impostata dalle istruzioni
232300980630     c*  soprastanti...)
232400980630     c                   movel     wopz          opz441
232500950206     C*
232600950206     C                   MOVE      FSC430        FSC441
232700980630     C     CMT430        IFEQ      '1'
232800950206     C                   MOVEL     '1'           CMT441
232900950206     C                   ELSE
233000950206     C                   MOVEL     '0'           CMT441
233100950206     C                   ENDIF
233200950206     C                   MOVE      FSC430        FSC441
233300950206     C                   MOVE      KSC430        KSC441
233400950206     C                   MOVE      SOG430        SOG441
233500950206     C                   MOVE      TII430        TII441
233600950206     C                   MOVE      COI430        COI441
233700950206     C                   MOVEL     A441DS        KPJBU
233800950206     C                   CALL      'ANA441R'
233900950206     C                   PARM                    KPJBA
234000000712b0244x*Non le passo perchè gli agenti devono reperirsi quelle di ANGAGE
234100000712b0244x***                PARM                    AUTDS
234200950206     C                   MOVEL     KPJBU         A441DS
234300950206     C* ritorno da PGM
234400950206     C                   EXSR      RTN441
234500970919     C*------------------------------------------*
234600970919     C* AGENTI per unità
234700970919     C*------------------------------------------*
234800970919     C     PGM           WHENEQ    'AGU'
234900970919     C                   RESET                   A442DS
235000970919     C*
235100970919     C                   SELECT
235200970919     C*
235300970919     C* se richiesta immissione
235400970919     C     OPZ430        WHENEQ    '01'
235500970919     C* richiamo il pgm in modifica (poichè così fa anche l'immissione)
235600970919     C                   MOVE      '02'          OPZ442
235700970919     C*
235800970919     C* se richiesta modifica
235900970919     C     OPZ430        WHENEQ    '02'
236000970919     C* se il bottone è presente
236100970919     C     EXI           IFEQ      '1'
236200970919     C* richiamo il pgm in manutenzione
236300970919     C                   MOVE      '02'          OPZ442
236400970919     C* se il bottone non è presente
236500970919     C                   ELSE
236600970919     C* richiamo il pgm in modifica (poichè così fa anche l'immissione)
236700970919     C                   MOVE      '02'          OPZ442
236800970919     C                   ENDIF
236900970919     C*
237000970919     C* se richiesta visualizzazione
237100970919     C     OPZ430        WHENEQ    '05'
237200970919     C* richiamo il pgm in visualizzazione
237300970919     C                   MOVE      '05'          OPZ442
237400970919     C*
237500970919     C                   ENDSL
237600970919     C*
237700970919     C                   MOVE      FSC430        FSC442
237800970919     C                   MOVEL     CMT430        CMT442
237900970923     C                   MOVE      *Blank        Uni442
238000970919     C                   MOVE      FSC430        FSC442
238100970919     C                   MOVE      KSC430        KSC442
238200970919     C                   MOVE      SOG430        SOG442
238300970919     C                   MOVE      TII430        TII442
238400970919     C                   MOVE      COI430        COI442
238500970919     C                   MOVEL     A442DS        KPJBU
238600970919     C                   CALL      'ANA442R'
238700970919     C                   PARM                    KPJBA
238800000712b0244x*Non le passo perchè gli agenti devono reperirsi quelle di ANGAGE
238900000724b0244x***                PARM                    AUTDS
239000970919     C                   MOVEL     KPJBU         A442DS
239100970919     C* ritorno da PGM
239200970919     C                   EXSR      RTN442
239300950227     C*------------------------------------------*
239400950227     C* SPEDIZIONIERI
239500950227     C*------------------------------------------*
239600980702     C     PGM           WHENEQ    'SPE'
239700950227     C                   RESET                   A048DS
239800950227     C*
239900950227     C                   SELECT
240000950227     C*
240100950227     C* se richiesta immissione
240200950227     C     OPZ430        WHENEQ    '01'
240300950227     C* richiamo il pgm in immissione
240400950227     C                   MOVE      '01'          OPZ048
240500950227     C*
240600950227     C* se richiesta modifica
240700950227     C     OPZ430        WHENEQ    '02'
240800950227     C* se il bottone è presente
240900950227     C     EXI           IFEQ      '1'
241000950227     C* richiamo il pgm in manutenzione
241100950227     C                   MOVE      '02'          OPZ048
241200950227     C* se il bottone non è presente
241300950227     C                   ELSE
241400950404     C* richiamo il pgm in manutenzione(dove è possibile immettere)
241500950404     C                   MOVE      '02'          OPZ048
241600950227     C                   ENDIF
241700950227     C*
241800950227     C* se richiesta visualizzazione
241900950227     C     OPZ430        WHENEQ    '05'
242000950227     C* richiamo il pgm in visualizzazione
242100950227     C                   MOVE      '05'          OPZ048
242200950227     C*
242300950227     C                   ENDSL
242400950227     C*
242500950227     C                   MOVE      FSC430        FSC048
242600950227     C     CMT430        IFEQ      '1'
242700950227     C                   MOVEL     '1'           CMT048
242800950227     C                   ELSE
242900950227     C                   MOVEL     '0'           CMT048
243000950227     C                   ENDIF
243100950227     C                   MOVE      FSC430        FSC048
243200950227     C                   MOVE      KSC430        FOR048
243300950227     C                   MOVEL     A048DS        KPJBU
243400950227     C                   CALL      'ANA048R'
243500950227     C                   PARM                    KPJBA
243600950227     C*                    PARM           AUTDS
243700950227     C                   MOVEL     KPJBU         A048DS
243800950227     C* ritorno da PGM
243900950227     C                   EXSR      RTN048
244000941228     C*------------------------------------------*
244100941219     C* ANAGRAFICI
244200941228     C*------------------------------------------*
244300941219     C     PGM           WHENEQ    'SOG'
244400941219     C                   RESET                   A041DS
244500941227     C*
244600950103     C                   SELECT
244700950103     C*
244800950103     C* se richiesta immissione
244900950124     C     OPZ430        WHENEQ    '01'
245000950404     C* richiamo il pgm in immissione
245100950404     C                   MOVE      '01'          OPZ041
245200950103     C*
245300950103     C* se richiesta modifica
245400950124     C     OPZ430        WHENEQ    '02'
245500950103     C* richiamo il pgm in manutenzione
245600950103     C                   MOVE      '02'          OPZ041
245700950103     C*
245800950103     C* se richiesta visualizzazione
245900950124     C     OPZ430        WHENEQ    '05'
246000950103     C* richiamo il pgm in visualizzazione
246100950103     C                   MOVE      '05'          OPZ041
246200950103     C*
246300950103     C                   ENDSL
246400950103     C*
246500950124     C                   MOVEL     CMT430        CMT041
246600950124     C                   MOVEL     SOG430        COD041
246700941228     C                   MOVEL     A041DS        KPJBU
246800941219     C                   CALL      'ANA041R'
246900941219     C                   PARM                    KPJBA
247000941219     C                   MOVEL     KPJBU         A041DS
247100941219     C* ritorno da PGM
247200941219     C                   EXSR      RTN041
247300950104     C*------------------------------------------*
247400950104     C* FILIALI
247500950104     C*------------------------------------------*
247600950104     C     PGM           WHENEQ    'FIL'
247700950124     C                   RESET                   A405DS
247800950104     C*
247900950104     C                   SELECT
248000950104     C*
248100950104     C* se richiesta immissione
248200950124     C     OPZ430        WHENEQ    '01'
248300950104     C* richiamo il pgm in modifica (dove è possibile anche immettere)
248400950124     C                   MOVE      '02'          OPZ405
248500950104     C*
248600950104     C* se richiesta modifica
248700950124     C     OPZ430        WHENEQ    '02'
248800950104     C* se il bottone è presente
248900950104     C     EXI           IFEQ      '1'
249000950104     C* richiamo il pgm in manutenzione
249100950124     C                   MOVE      '02'          OPZ405
249200950104     C* se il bottone non è presente
249300950104     C                   ELSE
249400950104     C* richiamo il pgm in modifica (dove è possibile anche immettere)
249500950124     C                   MOVE      '02'          OPZ405
249600950104     C                   ENDIF
249700950104     C*
249800950104     C* se richiesta visualizzazione
249900950124     C     OPZ430        WHENEQ    '05'
250000950104     C* richiamo il pgm in visualizzazione
250100950124     C                   MOVE      '05'          OPZ405
250200950104     C*
250300950104     C                   ENDSL
250400980630     C* I test per le opzioni consentite in base a ANP020DS vengono
250500980630     C* demandati al programma chiamato (subfile)
250600950104     C*
250700950124     C                   MOVE      FSC430        FSC405
250800980630     C                   MOVEL     CMT430        CMT405
250900950124     C                   MOVEL     KSC430        KSC405
251000950124     C                   MOVEL     FIL430        FIL405
251100950124     C                   MOVEL     A405DS        KPJBU
251200950124     C                   CALL      'ANA405R'
251300950104     C                   PARM                    KPJBA
251400950104     C* non passo le autorizzazioni del cliente perchè sarà lo stesso
251500950104     C* pgm a reperirsi quelle generali di linea e successivamente
251600950104     C* quelle specifiche di linea
251700950124     C                   MOVEL     KPJBU         A405DS
251800950104     C* ritorno da PGM
251900950124     C                   EXSR      RTN405
252000951010     C*------------------------------------------*
252100951010     C* DICHIARAZIONE D'INTENTI
252200951010     C*------------------------------------------*
252300951010     C     PGM           WHENEQ    'DIC'
252400951010     C                   RESET                   A311DS
252500951010     C*
252600951010     C                   SELECT
252700951010     C*
252800951010     C* se richiesta immissione
252900951010     C     OPZ430        WHENEQ    '01'
253000951010     C* richiamo il pgm in modifica (dove è possibile anche immettere)
253100951010     C                   MOVE      '02'          OPZ311
253200951010     C*
253300951010     C* se richiesta modifica
253400951010     C     OPZ430        WHENEQ    '02'
253500951010     C* se il bottone è presente
253600951010     C     EXI           IFEQ      '1'
253700951010     C* richiamo il pgm in manutenzione
253800951010     C                   MOVE      '02'          OPZ311
253900951010     C* se il bottone non è presente
254000951010     C                   ELSE
254100951010     C* richiamo il pgm in modifica (dove è possibile anche immettere)
254200951010     C                   MOVE      '02'          OPZ311
254300951010     C                   ENDIF
254400951010     C*
254500951010     C* se richiesta visualizzazione
254600951010     C     OPZ430        WHENEQ    '05'
254700951010     C* richiamo il pgm in visualizzazione
254800951010     C                   MOVE      '05'          OPZ311
254900951010     C*
255000951010     C                   ENDSL
255100951010     C*
255200951010     C                   MOVE      FSC430        FSC311
255300951010     C     CMT430        IFEQ      '1'
255400951010     C                   MOVEL     '1'           CMT311
255500951010     C                   ELSE
255600951010     C                   MOVEL     '0'           CMT311
255700951010     C                   ENDIF
255800951010     C                   MOVE      'F'           COF311
255900951010     C                   MOVE      KSC430        KSC311
256000951010     C                   MOVEL     A311DS        KPJBU
256100951010     C                   CALL      'ANA311R'
256200951010     C                   PARM                    KPJBA
256300951010     C                   PARM                    AUTDS
256400951010     C                   MOVEL     KPJBU         A311DS
256500951010     C*
256600951010     C* ritorno da PGM
256700951010     C                   EXSR      RTN311
256800950119     C*------------------------------------------*
256900950119     C* APPOGGI BANCARI
257000950119     C*------------------------------------------*
257100950119     C     PGM           WHENEQ    'CBA'
257200950119     C                   RESET                   A026DS
257300950119     C*
257400950119     C                   SELECT
257500950119     C*
257600950119     C* se richiesta immissione
257700950124     C     OPZ430        WHENEQ    '01'
257800950119     C* richiamo il pgm in modifica (dove è possibile anche immettere)
257900950119     C                   MOVE      '02'          OPZ026
258000950119     C*
258100950119     C* se richiesta modifica
258200950124     C     OPZ430        WHENEQ    '02'
258300950119     C* se il bottone è presente
258400950119     C     EXI           IFEQ      '1'
258500950119     C* richiamo il pgm in manutenzione
258600950119     C                   MOVE      '02'          OPZ026
258700950119     C* se il bottone non è presente
258800950119     C                   ELSE
258900950119     C* richiamo il pgm in modifica (dove è possibile anche immettere)
259000950119     C                   MOVE      '02'          OPZ026
259100950119     C                   ENDIF
259200950119     C*
259300950119     C* se richiesta visualizzazione
259400950124     C     OPZ430        WHENEQ    '05'
259500950119     C* richiamo il pgm in visualizzazione
259600950119     C                   MOVE      '05'          OPZ026
259700950119     C*
259800950119     C                   ENDSL
259900950119     C*
260000950124     C                   MOVE      FSC430        FSC026
260100950124     C     CMT430        IFEQ      '1'
260200950119     C                   MOVEL     '1'           CMT026
260300950119     C                   ELSE
260400950119     C                   MOVEL     '0'           CMT026
260500950119     C                   ENDIF
260600950124     C                   MOVE      KSC430        KSC026
260700950124     C                   MOVE      'F'           COF026
260800950124     C                   MOVEL     $DT0          INV026
260900950124     C                   MOVEL     $DT0          UQ6026
261000950124     C                   MOVEL     A026DS        KPJBU
261100950119     C                   CALL      'ANA026R'
261200950119     C                   PARM                    KPJBA
261300980630     C*                    PARM
261400950119     C                   MOVEL     KPJBU         A026DS
261500950119     C*
261600950119     C* ritorno da PGM
261700950119     C                   EXSR      RTN026
261800950113     C*------------------------------------------*
261900950113     C* PERIODI ESCLUSIONE PAGAMENTI
262000950113     C*------------------------------------------*
262100950113     C     PGM           WHENEQ    'PEP'
262200950113     C                   RESET                   A315DS
262300950113     C*
262400950113     C                   SELECT
262500950113     C*
262600950113     C* se richiesta immissione
262700950124     C     OPZ430        WHENEQ    '01'
262800950113     C* richiamo il pgm in modifica (dove è possibile anche immettere)
262900950113     C                   MOVE      '02'          OPZ315
263000950113     C*
263100950113     C* se richiesta modifica
263200950124     C     OPZ430        WHENEQ    '02'
263300950113     C* se il bottone è presente
263400950113     C     EXI           IFEQ      '1'
263500950113     C* richiamo il pgm in manutenzione
263600950113     C                   MOVE      '02'          OPZ315
263700950113     C* se il bottone non è presente
263800950113     C                   ELSE
263900950113     C* richiamo il pgm in modifica (dove è possibile anche immettere)
264000950113     C                   MOVE      '02'          OPZ315
264100950113     C                   ENDIF
264200950113     C*
264300950113     C* se richiesta visualizzazione
264400950124     C     OPZ430        WHENEQ    '05'
264500950113     C* richiamo il pgm in visualizzazione
264600950113     C                   MOVE      '05'          OPZ315
264700950113     C*
264800950113     C                   ENDSL
264900980630     c*
265000980630     c                   eval      wOPZ    = OPZ315
265100980630     c                   eval      wFGi    = fgiPEP020
265200980630     c                   eval      wFGa    = fgaPEP020
265300980630     c                   exsr      TestANP020
265400980630     c* forzo l'opzione impostata dalla subr. TestANP020
265500980630     c* (se non sono in ottica di gruppo a livello aziendale
265600980630     c*  essa corrisponde a quella impostata dalle istruzioni
265700980630     c*  soprastanti...)
265800980630     c                   movel     wopz          opz315
265900950113     C*
266000950124     C                   MOVE      FSC430        FSC315
266100950124     C     CMT430        IFEQ      '1'
266200950113     C                   MOVEL     '1'           CMT315
266300950113     C                   ELSE
266400950113     C                   MOVEL     '0'           CMT315
266500950113     C                   ENDIF
266600950124     C                   MOVE      KSC430        KSC315
266700950124     C                   MOVE      'F'           FLG315
266800950113     C                   MOVEL     A315DS        KPJBU
266900950113     C                   CALL      'ANA315R'
267000950113     C                   PARM                    KPJBA
267100950113     C                   PARM                    AUTDS
267200950113     C                   MOVEL     KPJBU         A315DS
267300950113     C*
267400950113     C* ritorno da PGM
267500950113     C                   EXSR      RTN315
267600941228     C*------------------------------------------*
267700941220     C* STATISTICI
267800941228     C*------------------------------------------*
267900950124     C     PGM           WHENEQ    'FRS'
268000950124     C                   RESET                   A409DS
268100941227     C*
268200941220     C                   SELECT
268300941227     C*
268400941227     C* se richiesta immissione
268500950124     C     OPZ430        WHENEQ    '01'
268600941227     C* richiamo il pgm in immissione
268700950124     C                   MOVE      '01'          OPZ409
268800941227     C*
268900941227     C* se richiesta modifica
269000950124     C     OPZ430        WHENEQ    '02'
269100941227     C* se il bottone è presente
269200950103     C     EXI           IFEQ      '1'
269300941227     C* richiamo il pgm in manutenzione
269400950124     C                   MOVE      '02'          OPZ409
269500941227     C* se il bottone non è presente
269600941227     C                   ELSE
269700941227     C* richiamo il pgm in immissione
269800950124     C                   MOVE      '01'          OPZ409
269900941227     C                   ENDIF
270000941227     C*
270100941227     C* se richiesta visualizzazione
270200950124     C     OPZ430        WHENEQ    '05'
270300941227     C* richiamo il pgm in visualizzazione
270400950124     C                   MOVE      '05'          OPZ409
270500950103     C*
270600941227     C                   ENDSL
270700941227     C*
270800980630     c                   eval      wOPZ    = OPZ409
270900980630     c                   eval      wFGi    = fgiFRS020
271000980630     c                   eval      wFGa    = fgaFRS020
271100980630     c                   exsr      TestANP020
271200980630     c* forzo l'opzione impostata dalla subr. TestANP020
271300980630     c* (se non sono in ottica di gruppo a livello aziendale
271400980630     c*  essa corrisponde a quella impostata dalle istruzioni
271500980630     c*  soprastanti...)
271600980630     c                   movel     wopz          opz409
271700950124     C                   MOVE      FSC430        FSC409
271800950124     C     CMT430        IFEQ      '1'
271900950124     C                   MOVEL     '1'           CMT409
272000941220     C                   ELSE
272100950124     C                   MOVEL     '0'           CMT409
272200941220     C                   ENDIF
272300950124     C                   MOVE      WWWSOC        SOC409
272400950124     C                   MOVE      XSCDUN        UNI409
272500950124     C                   MOVE      KSC430        KSC409
272600050920B0827C                   Z-ADD     FIL430        FIL409
272700950124     C                   MOVEL     A409DS        KPJBU
272800950124     C                   CALL      'ANA409R'
272900941220     C                   PARM                    KPJBA
273000950103     C                   PARM                    AUTDS
273100950124     C                   MOVEL     KPJBU         A409DS
273200941220     C* ritorno da PGM
273300950124     C                   EXSR      RTN409
273400941228     C*------------------------------------------*
273500941220     C* COMMENTI
273600941228     C*------------------------------------------*
273700941219     C     PGM           WHENEQ    'TCM'
273800941219     C                   RESET                   A725DS
273900950104     C*
274000941219     C                   SELECT
274100950104     C*
274200950104     C* se richiesta immissione
274300950124     C     OPZ430        WHENEQ    '01'
274400950104     C* richiamo il pgm in modifica (dove è possibile anche immettere)
274500941219     C                   MOVE      '02'          OPZ725
274600950104     C*
274700950104     C* se richiesta modifica
274800950124     C     OPZ430        WHENEQ    '02'
274900950104     C* se il bottone è presente
275000950104     C     EXI           IFEQ      '1'
275100950104     C* richiamo il pgm in manutenzione
275200950104     C                   MOVE      '02'          OPZ725
275300950104     C* se il bottone non è presente
275400950104     C                   ELSE
275500950104     C* richiamo il pgm in modifica (dove è possibile anche immettere)
275600950104     C                   MOVE      '02'          OPZ725
275700950104     C                   ENDIF
275800950104     C*
275900950104     C* se richiesta visualizzazione
276000950124     C     OPZ430        WHENEQ    '05'
276100950104     C* richiamo il pgm in visualizzazione
276200941219     C                   MOVE      '05'          OPZ725
276300950104     C*
276400941219     C                   ENDSL
276500980630     C* Se siamo in logica di GRUPPO per società:
276600980630     C* - Aggiornamento/Immissione AUTOMATICO (da capofila)
276700980630     C* - FLAG di procedure attivati
276800980630     C* E' CONSENTITA LA SOLA VISUALIZZAZIONE
276900980630     C                   if        gsp020     = 'G'    and
277000980630     C                             imm020     = 'A'    and
277100980630     C                             fgiCOMM020 = *on    and
277200980630     C                             gsp020     = 'G'    and
277300980630     C                             agg020     = 'A'    and
277400980630     C                             fgaCOMM020 = *on
277500980630     C                   eval      opz725     = '05'
277600980630     C                   endif
277700950104     C*
277800010319A0871C                   MOVE      WWWSOC        SOC725
277900950124     C                   MOVE      FSC430        FSC725
278000950124     C                   MOVE      '001'         TPA725
278100941219     C* inserire chiavi commenti
278200950124     C                   MOVEL     KSC430        KY1725
278300941219     C                   MOVE      *BLANKS       KPJBU
278400941219     C                   MOVEL     A725DS        KPJBU
278500941219     C                   CALL      'ANA725R'
278600941219     C                   PARM                    KPJBA
278700941219     C                   MOVEL     KPJBU         A725DS
278800941219     C* ritorno da PGM
278900941219     C                   EXSR      RTN725
279000980630     C* da sottomettere solo se la società in esame è definita di
279100980630     C* gruppo (GSP020=G)
279200980630     C                   If        GSP020     = 'G'
279300980630     C                   If        opr725     = *on   and
279400980630     C                             err725     = *off
279500980630     C                   exsr      VentilTCM
279600980630     C                   endIF
279700980630     C                   endIF
279800980624     C*------------------------------------------*
279900980624     C* FIDI
280000980624     C*------------------------------------------*
280100980624     C     PGM           WHENEQ    'FID'
280200980624     C                   RESET                   A360DS
280300980624     C*
280400980624     C                   SELECT
280500980624     C*
280600980624     C* se richiesta immissione
280700980624     C     OPZ430        WHENEQ    '01'
280800980624     C* richiamo il pgm in modifica (dove è possibile anche immettere)
280900980624     C                   MOVE      '02'          OPZ360
281000980624     C*
281100980624     C* se richiesta modifica
281200980624     C     OPZ430        WHENEQ    '02'
281300980624     C* se il bottone è presente
281400980624     C     EXI           IFEQ      '1'
281500980624     C* richiamo il pgm in manutenzione
281600980624     C                   MOVE      '02'          OPZ360
281700980624     C* se il bottone non è presente
281800980624     C                   ELSE
281900980624     C* richiamo il pgm in modifica (dove è possibile anche immettere)
282000980624     C                   MOVE      '02'          OPZ360
282100980624     C                   ENDIF
282200980624     C*
282300980624     C* se richiesta visualizzazione
282400980624     C     OPZ430        WHENEQ    '05'
282500980624     C* richiamo il pgm in visualizzazione
282600980624     C                   MOVE      '05'          OPZ360
282700980624     C*
282800980624     C                   ENDSL
282900980624     C*
283000980624     C     CMT430        IFEQ      '1'
283100980624     C                   MOVEL     '1'           CMT360
283200980624     C                   ELSE
283300980624     C                   MOVEL     '0'           CMT360
283400980624     C                   ENDIF
283500981103     C                   MOVE      WWWSOC        fidsoc360
283600990524B0014C***EX A0428***     movel     'F'           kcc360
283700990524B0014C                   movel     WWWKCC        kcc360
283800980624     C                   MOVE      KSC430        KSC360
283900980624     C                   MOVE      'F'           validit360
284000980624     C                   MOVE      '1'           tpfido360
284100980624     C                   MOVEL     A360DS        KPJBU
284200980624     C                   CALL      'ANA360R'
284300980624     C                   PARM                    KPJBA
284400000712b0244x*Non le passo perchè deve reperirsi quelle di ANGFID
284500000712b0244x***                PARM                    AUTDS
284600980624     C                   MOVEL     KPJBU         A360DS
284700980624     C*
284800980624     C* ritorno da PGM
284900980624     C                   EXSR      RTN360
285000980624     C*------------------------------------------*
285100980624     C* SOCI PER AGENTE
285200980624     C*------------------------------------------*
285300980624     C     PGM           WHENEQ    'SOA'
285400980624     C                   RESET                   A600DS
285500980624     C*
285600980624     C                   SELECT
285700980624     C*
285800980624     C* se richiesta immissione
285900980624     C     OPZ430        WHENEQ    '01'
286000980624     C* richiamo il pgm in modifica (dove è possibile anche immettere)
286100980624     C                   MOVE      '02'          OPZ600
286200980624     C*
286300980624     C* se richiesta modifica
286400980624     C     OPZ430        WHENEQ    '02'
286500980624     C* se il bottone è presente
286600980624     C     EXI           IFEQ      '1'
286700980624     C* richiamo il pgm in manutenzione
286800980624     C                   MOVE      '02'          OPZ600
286900980624     C* se il bottone non è presente
287000980624     C                   ELSE
287100980624     C* richiamo il pgm in modifica (dove è possibile anche immettere)
287200980624     C                   MOVE      '02'          OPZ600
287300980624     C                   ENDIF
287400980624     C*
287500980624     C* se richiesta visualizzazione
287600980624     C     OPZ430        WHENEQ    '05'
287700980624     C* richiamo il pgm in visualizzazione
287800980624     C                   MOVE      '05'          OPZ600
287900980624     C*
288000980624     C                   ENDSL
288100980630      *
288200980630     c                   eval      wOPZ    = OPZ600
288300980701     c                   eval      wFGi    = fgiSOC020
288400980701     c                   eval      wFGa    = fgaSOC020
288500980630     c                   exsr      TestANP020
288600980630     c* forzo l'opzione impostata dalla subr. TestANP020
288700980630     c* (se non sono in ottica di gruppo a livello aziendale
288800980630     c*  essa corrisponde a quella impostata dalle istruzioni
288900980630     c*  soprastanti...)
289000980630     c                   movel     wopz          opz600
289100980624     C*
289200980624     C     CMT430        IFEQ      '1'
289300980624     C                   MOVEL     '1'           CMT600
289400980624     C                   ELSE
289500980624     C                   MOVEL     '0'           CMT600
289600980624     C                   ENDIF
289700980624     C                   MOVE      WWWSOC        SOC600
289800980624     C                   MOVE      KSC430        KSC600
289900980624     C                   MOVEL     A600DS        KPJBU
290000980624     C                   CALL      'ANA600R'
290100980624     C                   PARM                    KPJBA
290200000712b0244x*Non le passo perchè deve reperirsi quelle di ANGAGE
290300000712b0244x***                PARM                    AUTDS
290400980624     C                   MOVEL     KPJBU         A600DS
290500980624     C*
290600980624     C* ritorno da PGM
290700980624     C                   EXSR      RTN600
290800010830G1036C*------------------------------------------*
290900010830  "  C* LINEE DI ACQUISTO
291000010830  "  C*------------------------------------------*
291100010830  "  C     PGM           wheneq    'LDA'
291200010830  "  C                   reset                   ANA161DS0
291300010830  "  C*
291400010830  "  C                   select
291500010830  "  C*
291600010830  "  C* se richiesta immissione
291700010830  "  C     Opz430        wheneq    '01'
291800010830  "  C* richiamo il pgm in immissione
291900010830  "  C                   move      '01'          Opz1610
292000010830  "  C*
292100010830  "  C* se richiesta modifica
292200010830  "  C     Opz430        wheneq    '02'
292300010830  "  C* se il bottone è presente
292400010830  "  C                   if        Exi = '1'
292500010830  "  C* richiamo il pgm in manutenzione
292600010830  "  C                   move      '02'          Opz1610
292700010830  "  C* se il bottone non è presente
292800010830  "  C                   else
292900010830  "  C* richiamo il pgm in immissione
293000010830  "  C                   move      '01'          Opz1610
293100010830  "  C                   endif
293200010830  "  C*
293300010830  "  C* se richiesta visualizzazione
293400010830  "  C     Opz430        wheneq    '05'
293500010830  "  C* richiamo il pgm in visualizzazione
293600010830  "  C                   move      '05'          Opz1610
293700010830  "  C*
293800010830  "  C                   endsl
293900010830  "  C*
294000010830  "  C                   eval      For1610 = Ksc430
294100010830  "  C                   eval      Filial1610 = Fil430
294200010830  "  C                   eval      Kpjbu = Ana161DS0
294300060209G1036C
294400060209G1481X*** ex-G1036       call      'ANA161R1'
294500060209G1481C                   call      'ANA161R0'
294600060209G1036C                   parm                    Kpjba
294700010830  "  C*
294800010830  "  C                   movel     kpjbu         ANA161DS0
294900010830  "  C* ritorno da PGM
295000010830  "  C                   exsr      Rtn1610
295100010830  "  C*----------------------------------------------------*
295200010830  "  C* ANAGRAFICA FORNITORI/MAGAZZINI - DATI PER MAGAZZINO
295300010830  "  C*----------------------------------------------------*
295400010830  "  C     PGM           wheneq    'FDM'
295500010830  "  C                   reset                   ANA410DS
295600010830  "  C*
295700010830  "  C                   select
295800010830  "  C*
295900010830  "  C* se richiesta immissione
296000010830  "  C     Opz430        wheneq    '01'
296100010830  "  C* richiamo il pgm in immissione
296200010830  "  C                   move      '01'          Opz410
296300010830  "  C*
296400010830  "  C* se richiesta modifica
296500010830  "  C     Opz430        wheneq    '02'
296600010830  "  C* se il bottone è presente
296700010830  "  C                   if        Exi = '1'
296800010830  "  C* richiamo il pgm in manutenzione
296900010830  "  C                   move      '02'          Opz410
297000010830  "  C* se il bottone non è presente
297100010830  "  C                   else
297200010830  "  C* richiamo il pgm in immissione
297300010830  "  C                   move      '01'          Opz410
297400010830  "  C                   endif
297500010830  "  C*
297600010830  "  C* se richiesta visualizzazione
297700010830  "  C     Opz430        wheneq    '05'
297800010830  "  C* richiamo il pgm in visualizzazione
297900010830  "  C                   move      '05'          Opz410
298000010830  "  C*
298100010830  "  C                   endsl
298200010830  "  C*
298300010830  "  C                   eval      Fornit410 = Ksc430
298400010830  "  C                   eval      ForFil410 = Fil430
298500010830  "  C                   movel     ANA410DS      kpjbu
298600010830  "  C*
298700010830  "  C                   call      'ANA410R'
298800010830  "  C                   parm                    kpjba
298900010830  "  C*
299000010830  "  C                   movel     kpjbu         ANA410DS
299100010830  "  C* ritorno da PGM
299200010830  "  C                   exsr      Rtn410
299300010830  "  C*--------------------------------------------*
299400010830  "  C* ASSORTIMENTI FORNITORI
299500010830  "  C*--------------------------------------------*
299600010830  "  C     PGM           wheneq    'ASF'
299700010830  "  C                   reset                   ANA162DS
299800010830  "  C*
299900010830  "  C                   select
300000010830  "  C*
300100010830  "  C* se richiesta immissione
300200010830  "  C     Opz430        wheneq    '01'
300300010830  "  C* richiamo il pgm in immissione
300400010830  "  C                   move      '01'          Opz162
300500010830  "  C*
300600010830  "  C* se richiesta modifica
300700010830  "  C     Opz430        wheneq    '02'
300800010830  "  C* se il bottone è presente
300900010830  "  C                   if        Exi = '1'
301000010830  "  C* richiamo il pgm in manutenzione
301100010830  "  C                   move      '02'          Opz162
301200010830  "  C* se il bottone non è presente
301300010830  "  C                   else
301400010830  "  C* richiamo il pgm in immissione
301500010830  "  C                   move      '01'          Opz162
301600010830  "  C                   endif
301700010830  "  C*
301800010830  "  C* se richiesta visualizzazione
301900010830  "  C     Opz430        wheneq    '05'
302000010830  "  C* richiamo il pgm in visualizzazione
302100010830  "  C                   move      '05'          Opz162
302200010830  "  C*
302300010830  "  C                   endsl
302400010830  "  C*
302500010830  "  C                   eval      Societa162 = WWWSoc
302600010830  "  C                   eval      For162 = Ksc430
302700010830  "  C                   movel     ANA162DS      kpjbu
302800010830  "  C*
302900010830  "  C                   call      'ANA162R'
303000010830  "  C                   parm                    kpjba
303100010830  "  C*
303200010830  "  C                   movel     kpjbu         ANA162DS
303300010830  "  C*
303400010830  "  C* ritorno da PGM
303500010830G1036C                   exsr      Rtn162
303600010830     C*------------------------------------------*
303700950313     C* CONTABILITA'
303800950313     C* Eseguo chiamate programmi di contabilità seguendo la loro
303900950313     C* sequenza di chiamata
304000950313     C*------------------------------------------*
304100950313     C     PGM           WHENEQ    'RCO'
304200951004     C*
304300950313     C                   EXSR      GESCNT
304400941219     C*
304500941219     C                   ENDSL
304600941219     C*
304700941219     C                   ENDSR
304800950313     C/EJECT
304900950313     C************************************************************
305000950313     C* GESTIONE CHIAMATE PGM CONTABILITA'
305100950313     C************************************************************
305200950313     C     GESCNT        BEGSR
305300950313     C*
305400950313     C                   MOVEL     *OFF          $ESCI
305500950313     C                   MOVEL     WEXF          EXF430
305600010209B0340C* se siamo in immissione, il primo bottone, il capoconto,
305700010209  "  C* è obbligatorio, altrimenti gli altri non hanno senso
305800010209  "  c                   if        Opz430='01'
305900010209  "  c                   eval      D2b(1) = 1
306000010209B0340c                   endif
306100950313     C*
306200950313     C                   DO        NB2           Y                 3 0
306300950313     C*
306400950313     C     D2B(Y)        IFEQ      1
306500950313     C                   MOVEL     MIX1(Y)       GERARC
306600950313     C                   EXSR      CALCNT
306700950313     C* salvo in schiera il suo stato
306800950313     C                   MOVEL     GERARC        MIX1(Y)
306900950313     C* imposto il bottone e il relativo testo
307000950313     C                   EXSR      IMPCNT
307100950313     C     $ESCI         IFEQ      *ON
307200950313     C                   LEAVE
307300950313     C                   ENDIF
307400950313     C                   ENDIF
307500950313     C                   ENDDO
307600951004     C*
307700951004     C* eseguo i test di ciclo/fine ciclo
307800951004     C* solo se è stata richiesta l'opzione diretta "Contabilità"
307900951004     C* altrimenti demando tutto alla routine GESANA (che è stata
308000951004     C* la chiamante di questa)
308100951004     C*
308200951004     C     OPD430        IFEQ      *ON
308300951004     C*
308400951004     C                   SELECT
308500951004     C*
308600951004     C* se ho ultimato il ciclo senza nessuna interruzione
308700951004     C     $ESCI         WHENEQ    *OFF
308800951004     C* esco da pgm
308900951004     C                   MOVE      *ON           $FINE
309000951004     C                   MOVE      WERR          ERR430
309100951004     C*
309200951004     C* alla prima interruzione
309300951004     C* se non sto gestendo il video
309400951004     C     WESP          WHENEQ    *OFF
309500951004     C     $ESCI         ANDEQ     *ON
309600951004     C* esco da pgm
309700951004     C                   MOVE      *ON           $FINE
309800951004     C                   MOVE      WERR          ERR430
309900951004     C* comunicando alla guida di interrompere il suo ciclo come fosse F12
310000951004     C                   MOVE      '2'           RET430
310100951004     C*
310200951004     C                   ENDSL
310300951004     C*
310400951004     C                   ENDIF
310500950313     C*
310600950313     C                   ENDSR
310700950313     C/EJECT
310800950313     C************************************************************
310900950313     C* CHIAMATE VARIABILI CONTABILITA'
311000950313     C************************************************************
311100950313     C     CALCNT        BEGSR
311200950313     C*
311300950313     C                   SELECT
311400950313     C*------------------------------------------*
311500950313     C* PIANO DEI CONTI  - ASSEGNA CAPOCONTO
311600950313     C*------------------------------------------*
311700950313     C     PGMC          WHENEQ    'PDC'
311800950313     C                   RESET                   A206DS
311900950313     C                   SELECT
312000950313     C*
312100950313     C* se richiesta immissione o modifica controllo esistenza record
312200950313     c* per modificare opzione
312300950313     C     OPZ430        WHENEQ    '01'
312400950313     C     OPZ430        OREQ      '02'
312500950313     C*
312600950313     C* se il bottone è presente
312700950313     C     EXICNT        IFEQ      '1'
312800950313     C* richiamo il pgm in manutenzione
312900950313     C                   MOVE      '02'          OPZ206
313000950313     C* se il bottone non è presente
313100950313     C                   ELSE
313200950313     C* richiamo il pgm in immissione
313300950313     C                   MOVE      '01'          OPZ206
313400950313     C                   ENDIF
313500950313     C*
313600950313     C* se richiesta visualizzazione
313700950313     C     OPZ430        WHENEQ    '05'
313800950313     C* richiamo il pgm in visualizzazione
313900950313     C                   MOVE      '05'          OPZ206
314000950313     C*
314100950313     C                   ENDSL
314200980630     c*
314300980630     c                   eval      wOPZ    = OPZ206
314400980630     c                   eval      wFGi    = fgiCont020
314500980630     c                   eval      wFGa    = fgaCont020
314600980630     c                   exsr      TestANP020
314700980630     c* forzo l'opzione impostata dalla subr. TestANP020
314800980630     c* (se non sono in ottica di gruppo a livello aziendale
314900980630     c*  essa corrisponde a quella impostata dalle istruzioni
315000980630     c*  soprastanti...)
315100980630     C                   movel     wopz          opz206
315200980701     C*
315300950313     C                   MOVEL     CMT430        CMT206
315400950313     C                   MOVE      FSC430        FSC206
315500950313     C                   MOVEL     WWWKCC        KCC206
315600950313     C                   MOVEL     KSC430        KSC206
315700950313     C                   MOVE      SOG430        SOG206
315800950313     C                   MOVEL     '4'           MOD206
315900950313     C                   MOVEL     A206DS        KPJBU
316000950313     C                   CALL      'ANA206R'
316100950313     C                   PARM                    KPJBA
316200000712b0244X*Non le passo perchè deve reperirsi quelle di ANGPDC
316300000712b0244x***                PARM                    AUTDS
316400950313     C                   MOVEL     KPJBU         A206DS
316500950313     C* ritorno da PGM
316600950313     C                   EXSR      RTN206
316700950313     C*------------------------------------------*
316800950313     C* REGOLE CONTABILI
316900950313     C*------------------------------------------*
317000950313     C     PGMC          WHENEQ    'RGC'
317100950313     C                   RESET                   A204DS
317200950313     C                   SELECT
317300950313     C*
317400950313     C* se richiesta immissione o modifica controllo esistenza record
317500950313     c* per modificare opzione
317600950313     C     OPZ430        WHENEQ    '01'
317700010209B0340C* PER LE REGOLE, IL BOTTONE È SEMPRE PRESENTE, PERCHÈ LE SCRIVE
317800010209B0340C* IL PROGRAMMA PRECEDENTE
317900010209B0340C                   MOVE      '22'          OPZ204
318000950313     C*
318100010209b0340x* se il bottone di contabilita è presente
318200010209  "  x*    EXICNT        IFEQ      '1'
318300010209b0340x* richiamo il pgm in manutenzione
318400010209b0340C     OPZ430        wheneq    '02'
318500950313     C                   MOVE      '02'          OPZ204
318600950313     C* se il bottone non è presente
318700010209B0340x*                  ELSE
318800010209B0340x* richiamo il pgm in immissione
318900010209B0340x*                  MOVE      '01'          OPZ204
319000010209B0340X*                  ENDIF
319100950313     C*
319200950313     C* se richiesta visualizzazione
319300950313     C     OPZ430        WHENEQ    '05'
319400950313     C* richiamo il pgm in visualizzazione
319500950313     C                   MOVE      '05'          OPZ204
319600950313     C*
319700950313     C                   ENDSL
319800980630     c*
319900010209B0340C* in wopz, nel caso di opzione 22, ci imposto opzione 02, per non
320000010209  "  C* modificare i suoi test
320100010209  "  c                   if        opz204 = '22'
320200010209  "  c                   eval      Wopz = '02'
320300010209B0340c                   else
320400980630     c                   eval      wOPZ    = OPZ204
320500010209B0340c                   endif
320600980630     c                   eval      wFGi    = fgiCont020
320700980630     c                   eval      wFGa    = fgaCont020
320800980630     c                   exsr      TestANP020
320900980630     c* forzo l'opzione impostata dalla subr. TestANP020
321000980630     c* (se non sono in ottica di gruppo a livello aziendale
321100980630     c*  essa corrisponde a quella impostata dalle istruzioni
321200980630     c*  soprastanti...)
321300010209     c* wopz può cambiare solo in '05', quindi se c'era opzione 22
321400010209     c* la devo lasciare altrimenti la sporca con '02'
321500010209b0340C                   IF        WOPZ = '05'
321600980630     C                   movel     wopz          opz204
321700010209B0340C                   endif
321800980630     C*
321900980630     C     CMT430        IFEQ      '1'
322000980630     C                   MOVEL     '1'           CMT204
322100980630     C                   ELSE
322200980630     C                   MOVEL     '0'           CMT204
322300980701     C                   ENDIF
322400950313     C                   MOVEL     CMT430        CMT204
322500950313     C                   MOVE      FSC430        FSC204
322600950313     C                   MOVEL     WWWKCC        KCC204
322700950313     C                   MOVEL     KSC430        KSC204
322800950313     C                   MOVEL     '4'           MOD204
322900950313     C                   MOVEL     A204DS        KPJBU
323000950313     C                   CALL      'ANA204R'
323100950313     C                   PARM                    KPJBA
323200000712b0244X*Non le passo perchè deve reperirsi quelle di ANGPDC
323300000712b0244x***                PARM                    AUTDS
323400950313     C                   MOVEL     KPJBU         A204DS
323500950313     C* ritorno da PGM
323600950313     C                   EXSR      RTN204
323700950313     C*------------------------------------------*
323800950313     C* ATTRIBUTI CONTABILI
323900950313     C*------------------------------------------*
324000980630     C     PGMC          WHENEQ    'ATC'
324100950313     C                   RESET                   A205DS
324200950313     C                   SELECT
324300950313     C* se richiesta immissione o modifica controllo esistenza record
324400950313     c* per modificare opzione
324500950313     C     OPZ430        WHENEQ    '01'
324600950313     C     OPZ430        OREQ      '02'
324700950313     C*
324800950313     C* se il bottone  di contabilità è presente
324900950313     C     EXICNT        IFEQ      '1'
325000950313     C* richiamo il pgm in manutenzione
325100950313     C                   MOVE      '02'          OPZ205
325200950313     C* se il bottone non è presente
325300950313     C                   ELSE
325400950313     C* richiamo il pgm in immissione
325500950313     C                   MOVE      '01'          OPZ205
325600950313     C                   ENDIF
325700950313     C*
325800950313     C* se richiesta visualizzazione
325900950313     C     OPZ430        WHENEQ    '05'
326000950313     C* richiamo il pgm in visualizzazione
326100950313     C                   MOVE      '05'          OPZ205
326200950313     C*
326300950313     C                   ENDSL
326400980701     c*
326500980701     c                   eval      wOPZ    = OPZ205
326600980701     c                   eval      wFGi    = fgiCont020
326700980701     c                   eval      wFGa    = fgaCont020
326800980701     c                   exsr      TestANP020
326900980701     c* forzo l'opzione impostata dalla subr. TestANP020
327000980701     c* (se non sono in ottica di gruppo a livello aziendale
327100980701     c*  essa corrisponde a quella impostata dalle istruzioni
327200980701     c*  soprastanti...)
327300980701     C                   movel     wopz          opz205
327400980701     C*
327500950313     C                   MOVEL     CMT430        CMT205
327600950313     C                   MOVE      FSC430        FSC205
327700950313     C                   MOVEL     WWWKCC        KCC205
327800950313     C                   MOVEL     KSC430        KSC205
327900950313     C                   MOVEL     '4'           MOD205
328000950313     C                   MOVEL     A205DS        KPJBU
328100950313     C                   CALL      'ANA205R'
328200950313     C                   PARM                    KPJBA
328300000712b0244X*Non le passo perchè deve reperirsi quelle di ANGPDC
328400000712b0244x***                PARM                    AUTDS
328500950313     C                   MOVEL     KPJBU         A205DS
328600950313     C* ritorno da PGM
328700950313     C                   EXSR      RTN205
328800950313     C*------------------------------------------*
328900950313     C* ANAGRAFICA PERCIPIENTI
329000950313     C*------------------------------------------*
329100950313     C     PGMC          WHENEQ    'PER'
329200950313     C                   RESET                   A207DS
329300950313     C*
329400950313     C                   SELECT
329500950313     C*
329600950313     C* se richiesta immissione o modifica controllo esistenza record
329700950313     c* per modificare opzione
329800950313     C     OPZ430        WHENEQ    '01'
329900950313     C     OPZ430        OREQ      '02'
330000950313     C*
330100950313     C* se il bottone è presente
330200950313     C     EXIC          IFEQ      '1'
330300950313     C* richiamo il pgm in manutenzione
330400950313     C                   MOVE      '02'          OPZ207
330500950313     C* se il bottone non è presente
330600950313     C                   ELSE
330700950313     C* richiamo il pgm in immissione
330800950313     C                   MOVE      '01'          OPZ207
330900950313     C                   ENDIF
331000950313     C*
331100950313     C* se richiesta visualizzazione
331200950313     C     OPZ430        WHENEQ    '05'
331300950313     C* richiamo il pgm in visualizzazione
331400950313     C                   MOVE      '05'          OPZ207
331500950313     C*
331600950313     C                   ENDSL
331700980714     c*
331800980714     c                   eval      wOPZ    = OPZ207
331900980714     c                   eval      wFGi    = fgiCont020
332000980714     c                   eval      wFGa    = fgaCont020
332100980714     c                   exsr      TestANP020
332200980714     c* forzo l'opzione impostata dalla subr. TestANP020
332300980714     c* (se non sono in ottica di gruppo a livello aziendale
332400980714     c*  essa corrisponde a quella impostata dalle istruzioni
332500980714     c*  soprastanti...)
332600980714     C                   movel     wopz          opz207
332700980714     C*
332800950313     C                   MOVEL     CMT430        CMT207
332900950313     C                   MOVE      FSC430        FSC207
333000950313     C                   MOVEL     WWWKCC        KCC207
333100950313     C                   MOVEL     KSC430        KSC207
333200950313     C                   MOVEL     '4'           MOD207
333300950313     C                   MOVEL     A207DS        KPJBU
333400950313     C                   CALL      'ANA207R'
333500950313     C                   PARM                    KPJBA
333600000712b0244X*Non le passo perchè deve reperirsi quelle di ANGPDC
333700000712b0244x***                PARM                    AUTDS
333800950313     C                   MOVEL     KPJBU         A207DS
333900950313     C* ritorno da PGM
334000950313     C                   EXSR      RTN207
334100950313     C*------------------------------------------*
334200950313     C* VOCI ANALITICA
334300950313     C*------------------------------------------*
334400950313     C     PGMC          WHENEQ    'VCA'
334500950615     C                   RESET                   A280DS
334600950615     C* Guida
334700950615     C                   SELECT
334800950615     C*
334900950615     C* se richiesta immissione
335000950615     C     OPZ430        WHENEQ    '01'
335100950615     C* richiamo il pgm in modifica (dove è possibile anche immettere)
335200950615     C                   MOVE      '02'          OPZ280
335300950615     C*
335400950615     C* se richiesta modifica
335500950615     C     OPZ430        WHENEQ    '02'
335600950615     C* se il bottone è presente
335700950615     C     EXIC          IFEQ      '1'
335800950615     C* richiamo il pgm in manutenzione
335900950615     C                   MOVE      '02'          OPZ280
336000950615     C* se il bottone non è presente
336100950615     C                   ELSE
336200950615     C* richiamo il pgm in modifica (dove è possibile anche immettere)
336300950615     C                   MOVE      '02'          OPZ280
336400950615     C                   ENDIF
336500950615     C*
336600950615     C* se richiesta visualizzazione
336700950615     C     OPZ430        WHENEQ    '05'
336800950615     C* richiamo il pgm in visualizzazione
336900950615     C                   MOVE      '05'          OPZ280
337000950615     C*
337100950615     C                   ENDSL
337200950615     C*
337300950615     C                   MOVE      FSC430        FSC280
337400950615     C* Passo capoconto e sottoconto solo se analitica 1 o 2
337500950615     C     WWWANL        IFEQ      '1'
337600950615     C     WWWANL        OREQ      '2'
337700950615     C                   MOVEL     WWWKCC        KCC280
337800950615     C                   MOVEL     KSC430        KSC280
337900950615     C                   ENDIF
338000950615     C*
338100950615     C                   MOVEL     WWWANL        ANL280
338200950615     C                   MOVE      *BLANKS       KPJBU
338300950615     C                   MOVEL     A280DS        KPJBU
338400950615     C                   CALL      'ANA280R'
338500950615     C                   PARM                    KPJBA
338600950615     C                   MOVEL     KPJBU         A280DS
338700950615     C* ritorno da PGM
338800950615     C                   EXSR      RTN280
338900950615     C*
339000950615     C                   ENDSL
339100950615     C*
339200950615     C                   ENDSR
339300950103     C/EJECT
339400950103     C************************************************************
339500950103     C* CHIAMATA PER COPIA
339600950103     C************************************************************
339700950103     C     GESCOP        BEGSR
339800950103     C*
339900950103     C*------------------------------------------*
340000950103     C* COMMERCIALI GENERALI
340100950103     C*------------------------------------------*
340200950103     C* salvo il codice che verrà copiato perchè al ritorno dal pgm
340300950124     C* dei commerciali generali il KSC430 sarà sostituito dal KSC401
340400950124     C                   MOVEL     KSC430        WKSC
340500950103     C*
340600950124     C                   RESET                   A401DS
340700980630     C                   if        gsp020     = 'G'    and
340800980630     C                             imm020     = 'A'    and
340900980630     C                             fgiComG020 = *on
341000980630     C                   eval      opz401     = '05'
341100980630     C                   else
341200950124     C                   MOVE      OPZ430        OPZ401
341300980630     C                   endif
341400950124     C                   MOVE      FSC430        FSC401
341500950124     C                   MOVEL     '1'           CMT401
341600950124     C                   MOVEL     KSC430        KSC401
341700950912     C                   MOVEL     B02430        CTB401
341800950124     C                   MOVEL     A401DS        KPJBU
341900950124     C                   CALL      'ANA401R'
342000950103     C                   PARM                    KPJBA
342100950103     C                   PARM                    AUTDS
342200950124     C                   MOVEL     KPJBU         A401DS
342300950103     C* ritorno da PGM
342400950124     C                   EXSR      RTN401
342500950103     C*
342600950103     C     $ESCI         IFEQ      *OFF
342700950124     C                   RESET                   A453DS
342800950124     C                   MOVEL     WKSC          KSV453
342900950124     C                   MOVEL     KSC430        KSN453
343000950526     C                   MOVEL     SOG430        SOG453
343100950303     C                   MOVEL     XSCSOC        SOC453
343200950303     C                   MOVEL     XSCCUT        UTE453
343300950117     C                   EXSR      PRPMIX
343400950124     C                   MOVEA     XIM           MIX453
343500950124     C                   MOVEL     A453DS        KPJBU
343600950124     C                   CALL      'ANA453R'
343700950103     C                   PARM                    KPJBA
343800950124     C                   MOVEL     KPJBU         A453DS
343900950103     C* se ha dato errore il pgm di copia
344000950124     C     ERR453        IFEQ      *ON
344100950227     C*                    ROLBK
344200950103     C* resto a pgm ed emetto msg di errore
344300950103     C                   MOVE      *ON           $ESCI                          sospese READC
344400950103     C                   SETON                                        5299                  ID
344500950103     C                   ELSE
344600950103     C* esco da pgm
344700950127     C                   MOVE      OPR453        WOPR                           avverto pgm chiamant
344800950103     C                   MOVE      *ON           $FINE
344900950103     C                   ENDIF
345000950103     C                   ENDIF
345100950103     C*
345200950103     C                   ENDSR
345300941219     C/EJECT
345400941219     C************************************************************
345500950117     C* PREPARA LA SCHIERA PER LA CHIAMATA DEL PGM DI COPIA
345600941219     C************************************************************
345700950117     C     PRPMIX        BEGSR
345800941219     C*
345900950117     C                   MOVEL     *BLANK        XIM
346000950117     C*
346100950313     C     1             DO        NB1           X
346200950118     C                   MOVEL     MIX(X)        GERARK
346300950118     C                   MOVE      D1B(X)        EXI
346400950117     C*
346500950117     C                   MOVEL     GERARK        XIM(X)
346600950117     C*
346700950117     C                   ENDDO
346800950117     C*
346900950117     C                   ENDSR
347000970217     C/EJECT
347100970217     C************************************************************
347200970217     C* GESTIONE RITORNO DA PGM DI INTERFACCIA
347300970217     C************************************************************
347400970217     C     RTN000        BEGSR
347500970217     C*
347600970217     C* modo di ritorno
347700970217     C*
347800970217     C                   SELECT
347900970217     C* questi modi di utilizzo dei valori di ritorno dal
348000970217     C* pgm di manutenzione rcd di anagrafica sono delle
348100970217     C* proposte, normalmente sempre valide, ma modificabili
348200970217     C* per situazioni particolari
348300970217     C* 1 = F3
348400970217     C     RET000        WHENEQ    '1'
348500970217     C                   MOVE      *ON           $ESCI                          sospese READC
348600970217     C                   MOVE      *ON           $FINE                          fine pgm
348700970217     C                   MOVE      '2'           RET430
348800970217     C* 2 = F12
348900970217     C     RET000        WHENEQ    '2'
349000970217     C                   MOVE      *ON           $ESCI                          sospese READC
349100970217     C*
349200970217     C                   ENDSL
349300970217     C*
349400970217     C* operazione eseguite dal pgm chiamato
349500970217     C*
349600970217     C                   SELECT
349700970217     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
349800970217     C     OPR000        WHENEQ    '1'
349900970217     C                   MOVE      OPR000        WOPR                           avverto pgm chiamant
350000970217     C*
350100970217     C                   ENDSL
350200970217     C*
350300970217     C* funzione non eseguibile per errore :
350400970217     C*
350500970217     C                   SELECT
350600970217     C* 1 = funzione richiamata chiusa in errore
350700970217     C     ERR000        WHENEQ    '1'
350800970217     C                   MOVE      *ON           $ESCI                          sospese READC
350900970217     C                   SETON                                        5299      emiss.SFLMSGID
351000970217     C* 2 = funzione richiamata senza la dovuta autorizzazione
351100970217     C     ERR000        WHENEQ    '2'
351200970217     C                   MOVE      *ON           $ESCI                                      C
351300970217     C                   SETON                                        5399                  ID
351400970217     C*
351500970217     C                   ENDSL
351600970217     C*
351700970217     C* salvo il codice d'errore da ripassare alla guida
351800970217     C                   MOVE      ERR000        WERR
351900970217     C*
352000970217     C                   ENDSR
352100950117     C/EJECT
352200950117     C************************************************************
352300950117     C* GESTIONE RITORNO DA PGM DEI DATI COMMERCIALI GENERALI
352400950117     C************************************************************
352500950124     C     RTN401        BEGSR
352600950117     C*
352700941205     C* modo di ritorno
352800941205     C*
352900941205     C                   SELECT
353000941205     C* questi modi di utilizzo dei valori di ritorno dal
353100941205     C* pgm di manutenzione rcd di anagrafica sono delle
353200941205     C* proposte, normalmente sempre valide, ma modificabili
353300941205     C* per situazioni particolari
353400941205     C* 1 = F3
353500950124     C     RET401        WHENEQ    '1'
353600941205     C                   MOVE      *ON           $ESCI                          sospese READC
353700941205     C                   MOVE      *ON           $FINE                          fine pgm
353800950124     C                   MOVE      '2'           RET430
353900941205     C* 2 = F12
354000950124     C     RET401        WHENEQ    '2'
354100941205     C                   MOVE      *ON           $ESCI                          sospese READC
354200941205     C*
354300941205     C                   ENDSL
354400941205     C*
354500941205     C* operazione eseguite dal pgm chiamato
354600941205     C*
354700941205     C                   SELECT
354800941205     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
354900950124     C     OPR401        WHENEQ    '1'
355000950124     C                   MOVE      OPR401        WOPR                           avverto pgm chiamant
355100941227     C*  passo ai successivi pgm il codice creato
355200950124     C                   MOVE      KSC401        KSC430                                     chiamant
355300950206     C                   MOVE      SOG401        SOG430
355400950206     C                   MOVE      TII401        TII430
355500950206     C                   MOVE      COI401        COI430
355600941228     C*  se è stata eseguito un aggiornamento
355700941228     C*  reperisco il nuovo stato del bottone
355800950124     C                   EXSR      TSTFRN                                                   chiamant
355900941205     C*
356000941205     C                   ENDSL
356100941205     C*
356200941205     C* funzione non eseguibile per errore :
356300941205     C*
356400941205     C                   SELECT
356500941205     C* 1 = funzione richiamata chiusa in errore
356600950124     C     ERR401        WHENEQ    '1'
356700941205     C                   MOVE      *ON           $ESCI                          sospese READC
356800941205     C                   SETON                                        5299      emiss.SFLMSGID
356900950104     C* 2 = funzione richiamata senza la dovuta autorizzazione
357000950124     C     ERR401        WHENEQ    '2'
357100950104     C                   MOVE      *ON           $ESCI                                      C
357200950104     C                   SETON                                        5399                  ID
357300941205     C*
357400941205     C                   ENDSL
357500950104     C*
357600950104     C* salvo il codice d'errore da ripassare alla guida
357700950124     C                   MOVE      ERR401        WERR
357800941205     C*
357900941205     C                   ENDSR
358000950206     C/EJECT
358100950206     C************************************************************
358200970919     C* GESTIONE RITORNO DA PGM DI GESTIONE DATI AGENTI generali
358300950206     C************************************************************
358400950206     C     RTN441        BEGSR
358500950206     C*
358600950206     C* modo di ritorno
358700950206     C*
358800950206     C                   SELECT
358900950206     C* questi modi di utilizzo dei valori di ritorno dal
359000950206     C* pgm di manutenzione rcd di anagrafica sono delle
359100950206     C* proposte, normalmente sempre valide, ma modificabili
359200950206     C* per situazioni particolari
359300950206     C* 1 = F3
359400950206     C     RET441        WHENEQ    '1'
359500950206     C                   MOVE      *ON           $ESCI                          sospese READC
359600950206     C                   MOVE      *ON           $FINE                          fine pgm
359700950206     C                   MOVE      '2'           RET430
359800950206     C* 2 = F12
359900950206     C     RET441        WHENEQ    '2'
360000950206     C                   MOVE      *ON           $ESCI                          sospese READC
360100950206     C*
360200950206     C                   ENDSL
360300950206     C*
360400950206     C* operazione eseguite dal pgm chiamato
360500950206     C*
360600950206     C                   SELECT
360700950206     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
360800950206     C     OPR441        WHENEQ    '1'
360900970630     C                   MOVE      OPR441        WOPR                           avverto pgm chiamant
361000950206     C*  se è stata eseguito un aggiornamento
361100950206     C*  reperisco il nuovo stato del bottone
361200950206     C                   EXSR      TSTAGE                                                   chiamant
361300950206     C*
361400950206     C                   ENDSL
361500950206     C*
361600950206     C* funzione non eseguibile per errore :
361700950206     C*
361800950206     C                   SELECT
361900950206     C* 1 = funzione richiamata chiusa in errore
362000950206     C     ERR441        WHENEQ    '1'
362100950206     C                   MOVE      *ON           $ESCI                          sospese READC
362200950206     C                   SETON                                        5299      emiss.SFLMSGID
362300950206     C* 2 = funzione richiamata senza la dovuta autorizzazione
362400950206     C     ERR441        WHENEQ    '2'
362500950206     C                   MOVE      *ON           $ESCI                                      C
362600950206     C                   SETON                                        5399                  ID
362700950206     C*
362800950206     C                   ENDSL
362900950206     C*
363000950206     C* salvo il codice d'errore da ripassare alla guida
363100950206     C                   MOVE      ERR441        WERR
363200950206     C*
363300950206     C                   ENDSR
363400950227     C/EJECT
363500970919     C************************************************************
363600970919     C* GESTIONE RITORNO DA PGM DI GESTIONE DATI AGENTI per unità
363700970919     C************************************************************
363800970919     C     RTN442        BEGSR
363900970919     C*
364000970919     C* modo di ritorno
364100970919     C*
364200970919     C                   SELECT
364300970919     C* questi modi di utilizzo dei valori di ritorno dal
364400970919     C* pgm di manutenzione rcd di anagrafica sono delle
364500970919     C* proposte, normalmente sempre valide, ma modificabili
364600970919     C* per situazioni particolari
364700970919     C* 1 = F3
364800970919     C     RET442        WHENEQ    '1'
364900970919     C                   MOVE      *ON           $ESCI                          sospese READC
365000970919     C                   MOVE      *ON           $FINE                          fine pgm
365100970919     C                   MOVE      '2'           RET430
365200970919     C* 2 = F12
365300970919     C     RET442        WHENEQ    '2'
365400970919     C                   MOVE      *ON           $ESCI                          sospese READC
365500970919     C*
365600970919     C                   ENDSL
365700970919     C*
365800970919     C* operazioni eseguite dal pgm chiamato
365900970919     C*
366000970919     C                   SELECT
366100970919     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
366200970919     C     OPR442        WHENEQ    '1'
366300970919     C                   MOVE      OPR442        WOPR                           avverto pgm chiamant
366400970919     C*  se è stata eseguito un aggiornamento
366500970919     C*  reperisco il nuovo stato del bottone
366600970919     C                   EXSR      TSTAGE                                                   chiamant
366700970919     C*
366800970919     C                   ENDSL
366900970919     C*
367000970919     C* funzione non eseguibile per errore :
367100970919     C*
367200970919     C                   SELECT
367300970919     C* 1 = funzione richiamata chiusa in errore
367400970919     C     ERR442        WHENEQ    '1'
367500970919     C                   MOVE      *ON           $ESCI                          sospese READC
367600970919     C                   SETON                                        5299      emiss.SFLMSGID
367700970919     C* 2 = funzione richiamata senza la dovuta autorizzazione
367800970919     C     ERR442        WHENEQ    '2'
367900970919     C                   MOVE      *ON           $ESCI                                      C
368000970919     C                   SETON                                        5399                  ID
368100970919     C*
368200970919     C                   ENDSL
368300970919     C*
368400970919     C* salvo il codice d'errore da ripassare alla guida
368500970919     C                   MOVE      ERR442        WERR
368600970919     C*
368700970919     C                   ENDSR
368800970919     C/EJECT
368900950227     C************************************************************
369000950227     C* GESTIONE RITORNO DA PGM DI GESTIONE DATI SPEDIZIONIERI
369100950227     C************************************************************
369200950227     C     RTN048        BEGSR
369300950227     C*
369400950227     C* modo di ritorno
369500950227     C*
369600950227     C                   SELECT
369700950227     C* questi modi di utilizzo dei valori di ritorno dal
369800950227     C* pgm di manutenzione rcd di anagrafica sono delle
369900950227     C* proposte, normalmente sempre valide, ma modificabili
370000950227     C* per situazioni particolari
370100950227     C* 1 = F3
370200950227     C     RET048        WHENEQ    '1'
370300950227     C                   MOVE      *ON           $ESCI                          sospese READC
370400950227     C                   MOVE      *ON           $FINE                          fine pgm
370500950227     C                   MOVE      '2'           RET430
370600950227     C* 2 = F12
370700950227     C     RET048        WHENEQ    '2'
370800950227     C                   MOVE      *ON           $ESCI                          sospese READC
370900950227     C*
371000950227     C                   ENDSL
371100950227     C*
371200950227     C* operazione eseguite dal pgm chiamato
371300950227     C*
371400950227     C                   SELECT
371500950227     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
371600950227     C     OPR048        WHENEQ    '1'
371700970630     C                   MOVE      OPR048        WOPR                           avverto pgm chiamant
371800950227     C*  se è stata eseguito un aggiornamento
371900950227     C*  reperisco il nuovo stato del bottone
372000950227     C                   EXSR      TSTSPE                                                   chiamant
372100950227     C*
372200950227     C                   ENDSL
372300950227     C*
372400950227     C* funzione non eseguibile per errore :
372500950227     C*
372600950227     C                   SELECT
372700950227     C* 1 = funzione richiamata chiusa in errore
372800950227     C     ERR048        WHENEQ    '1'
372900950227     C                   MOVE      *ON           $ESCI                          sospese READC
373000950227     C                   SETON                                        5299      emiss.SFLMSGID
373100950227     C* 2 = funzione richiamata senza la dovuta autorizzazione
373200950227     C     ERR048        WHENEQ    '2'
373300950227     C                   MOVE      *ON           $ESCI                                      C
373400950227     C                   SETON                                        5399                  ID
373500950227     C*
373600950227     C                   ENDSL
373700950227     C*
373800950227     C* salvo il codice d'errore da ripassare alla guida
373900950227     C                   MOVE      ERR048        WERR
374000950227     C*
374100950227     C                   ENDSR
374200941219     C/EJECT
374300941219     C************************************************************
374400941219     C* GESTIONE RITORNO DA PGM DEI DATI ANAGRAFICI
374500941219     C************************************************************
374600941219     C     RTN041        BEGSR
374700941219     C*
374800941219     C* modo di ritorno
374900941219     C*
375000941219     C                   SELECT
375100941219     C* questi modi di utilizzo dei valori di ritorno dal
375200941219     C* pgm di manutenzione rcd di anagrafica sono delle
375300941219     C* proposte, normalmente sempre valide, ma modificabili
375400941219     C* per situazioni particolari
375500941219     C* 1 = F3
375600941219     C     RET041        WHENEQ    '1'
375700941219     C                   MOVE      *ON           $ESCI                          sospese READC
375800941219     C                   MOVE      *ON           $FINE                          fine pgm
375900950124     C                   MOVE      '2'           RET430
376000941219     C* 2 = F12
376100941219     C     RET041        WHENEQ    '2'
376200941219     C                   MOVE      *ON           $ESCI                          sospese READC
376300941219     C*
376400941219     C                   ENDSL
376500941219     C*
376600941219     C* operazione eseguite dal pgm chiamato
376700941219     C*
376800941219     C                   SELECT
376900941219     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
377000941219     C     OPR041        WHENEQ    '1'
377100941219     C                   MOVE      OPR041        WOPR                           avverto pgm chiamant
377200941228     C*  se è stata eseguito un aggiornamento
377300941228     C*  reperisco il nuovo stato del bottone
377400941228     C                   EXSR      TSTSOG                                                   chiamant
377500941219     C*
377600941219     C                   ENDSL
377700941219     C*
377800941219     C* funzione non eseguibile per errore :
377900941219     C*
378000941219     C                   SELECT
378100941219     C* 1 = funzione richiamata chiusa in errore
378200941219     C     ERR041        WHENEQ    '1'
378300950104     C                   MOVE      *ON           $ESCI                                      C
378400950104     C                   SETON                                        5299                  ID
378500950104     C* 2 = funzione richiamata senza la dovuta autorizzazione
378600950104     C     ERR041        WHENEQ    '2'
378700950104     C                   MOVE      *ON           $ESCI                                      C
378800950104     C                   SETON                                        5399                  ID
378900941219     C*
379000941219     C                   ENDSL
379100950104     C*
379200950104     C* salvo il codice d'errore da ripassare alla guida
379300950104     C                   MOVE      ERR041        WERR
379400941219     C*
379500941219     C                   ENDSR
379600950104     C/EJECT
379700950104     C************************************************************
379800950104     C* GESTIONE RITORNO DA PGM DEI DATI FILIALI
379900950104     C************************************************************
380000950124     C     RTN405        BEGSR
380100950104     C*
380200950104     C* modo di ritorno
380300950104     C*
380400950104     C                   SELECT
380500950104     C* questi modi di utilizzo dei valori di ritorno dal
380600950104     C* pgm di manutenzione rcd di anagrafica sono delle
380700950104     C* proposte, normalmente sempre valide, ma modificabili
380800950104     C* per situazioni particolari
380900950104     C* 1 = F3
381000950124     C     RET405        WHENEQ    '1'
381100950104     C                   MOVE      *ON           $ESCI                          sospese READC
381200950104     C                   MOVE      *ON           $FINE                          fine pgm
381300950124     C                   MOVE      '2'           RET430
381400950104     C* 2 = F12
381500950124     C     RET405        WHENEQ    '2'
381600950104     C                   MOVE      *ON           $ESCI                          sospese READC
381700950104     C*
381800950104     C                   ENDSL
381900950104     C*
382000950104     C* operazione eseguite dal pgm chiamato
382100950104     C*
382200950104     C                   SELECT
382300950104     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
382400950124     C     OPR405        WHENEQ    '1'
382500950124     C                   MOVE      OPR405        WOPR                           avverto pgm chiamant
382600950104     C*  se è stata eseguito un aggiornamento
382700950104     C*  reperisco il nuovo stato del bottone
382800950104     C                   EXSR      TSTFIL                                                   chiamant
382900950104     C*
383000950104     C                   ENDSL
383100950104     C*
383200950104     C* funzione non eseguibile per errore :
383300950104     C*
383400950104     C                   SELECT
383500950104     C* 1 = funzione richiamata chiusa in errore
383600950124     C     ERR405        WHENEQ    '1'
383700950104     C                   MOVE      *ON           $ESCI                          sospese READC
383800950104     C                   SETON                                        5299      emiss.SFLMSGID
383900950104     C*
384000950104     C                   ENDSL
384100950104     C*
384200950104     C* salvo il codice d'errore da ripassare alla guida
384300950124     C                   MOVE      ERR405        WERR
384400950104     C*
384500950104     C                   ENDSR
384600941220     C/EJECT
384700941220     C************************************************************
384800941220     C* GESTIONE RITORNO DA PGM DI GESTIONE DATI STATISTICI
384900941220     C************************************************************
385000950124     C     RTN409        BEGSR
385100941220     C*
385200941220     C* modo di ritorno
385300941220     C*
385400941220     C                   SELECT
385500941220     C* questi modi di utilizzo dei valori di ritorno dal
385600941220     C* pgm di manutenzione rcd di anagrafica sono delle
385700941220     C* proposte, normalmente sempre valide, ma modificabili
385800941220     C* per situazioni particolari
385900941220     C* 1 = F3
386000950124     C     RET409        WHENEQ    '1'
386100941220     C                   MOVE      *ON           $ESCI                          sospese READC
386200941220     C                   MOVE      *ON           $FINE                          fine pgm
386300950124     C                   MOVE      '2'           RET430
386400941220     C* 2 = F12
386500950124     C     RET409        WHENEQ    '2'
386600941220     C                   MOVE      *ON           $ESCI                          sospese READC
386700941220     C*
386800941220     C                   ENDSL
386900941220     C*
387000941220     C* operazione eseguite dal pgm chiamato
387100941220     C*
387200941220     C                   SELECT
387300941220     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
387400950124     C     OPR409        WHENEQ    '1'
387500970630     C                   MOVE      OPR409        WOPR                           avverto pgm chiamant
387600941228     C*  se è stata eseguito un aggiornamento
387700941228     C*  reperisco il nuovo stato del bottone
387800950124     C                   EXSR      TSTFRS                                                   chiamant
387900941220     C*
388000941220     C                   ENDSL
388100941220     C*
388200941220     C* funzione non eseguibile per errore :
388300941220     C*
388400941220     C                   SELECT
388500941220     C* 1 = funzione richiamata chiusa in errore
388600950124     C     ERR409        WHENEQ    '1'
388700941220     C                   MOVE      *ON           $ESCI                          sospese READC
388800941220     C                   SETON                                        5299      emiss.SFLMSGID
388900950104     C* 2 = funzione richiamata senza la dovuta autorizzazione
389000950124     C     ERR409        WHENEQ    '2'
389100950104     C                   MOVE      *ON           $ESCI                                      C
389200950104     C                   SETON                                        5399                  ID
389300941220     C*
389400941220     C                   ENDSL
389500950104     C*
389600950104     C* salvo il codice d'errore da ripassare alla guida
389700950124     C                   MOVE      ERR409        WERR
389800941220     C*
389900941220     C                   ENDSR
390000950119     C/EJECT
390100950119     C************************************************************
390200950119     C* GESTIONE RITORNO DA PGM DI GESTIONE APPOGGI BANCARI
390300950119     C************************************************************
390400950119     C     RTN026        BEGSR
390500950119     C*
390600950119     C* modo di ritorno
390700950119     C*
390800950119     C                   SELECT
390900950119     C* questi modi di utilizzo dei valori di ritorno dal
391000950119     C* pgm di manutenzione rcd di anagrafica sono delle
391100950119     C* proposte, normalmente sempre valide, ma modificabili
391200950119     C* per situazioni particolari
391300950119     C* 1 = F3
391400950119     C     RET026        WHENEQ    '1'
391500950119     C                   MOVE      *ON           $ESCI                          sospese READC
391600950119     C                   MOVE      *ON           $FINE                          fine pgm
391700950124     C                   MOVE      '2'           RET430
391800950119     C* 2 = F12
391900950119     C     RET026        WHENEQ    '2'
392000950119     C                   MOVE      *ON           $ESCI                          sospese READC
392100950119     C*
392200950119     C                   ENDSL
392300950119     C*
392400950119     C* operazione eseguite dal pgm chiamato
392500950119     C*
392600950119     C                   SELECT
392700950119     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
392800950119     C     OPR026        WHENEQ    '1'
392900970630     C                   MOVE      OPR026        WOPR                           avverto pgm chiamant
393000950119     C*  se è stata eseguito un aggiornamento
393100950119     C*  reperisco il nuovo stato del bottone
393200950119     C                   EXSR      TSTCBA                                                   chiamant
393300950119     C*
393400950119     C                   ENDSL
393500950119     C*
393600950119     C* funzione non eseguibile per errore :
393700950119     C*
393800950119     C                   SELECT
393900950119     C* 1 = funzione richiamata chiusa in errore
394000950119     C     ERR026        WHENEQ    '1'
394100950119     C                   MOVE      *ON           $ESCI                          sospese READC
394200950119     C                   SETON                                        5299      emiss.SFLMSGID
394300950119     C*
394400950119     C                   ENDSL
394500950119     C*
394600950119     C* salvo il codice d'errore da ripassare alla guida
394700950119     C                   MOVE      ERR026        WERR
394800950119     C*
394900950119     C                   ENDSR
395000950113     C/EJECT
395100951010     C************************************************************
395200951010     C* GESTIONE RITORNO DA PGM DI GESTIONE
395300951010     C************************************************************
395400951010     C     RTN311        BEGSR
395500951010     C*
395600951010     C* modo di ritorno
395700951010     C*
395800951010     C                   SELECT
395900951010     C* questi modi di utilizzo dei valori di ritorno dal
396000951010     C* pgm di manutenzione rcd di anagrafica sono delle
396100951010     C* proposte, normalmente sempre valide, ma modificabili
396200951010     C* per situazioni particolari
396300951010     C* 1 = F3
396400951010     C     RET311        WHENEQ    '1'
396500951010     C                   MOVE      *ON           $ESCI                          sospese READC
396600951010     C                   MOVE      *ON           $FINE                          fine pgm
396700951010     C                   MOVE      '2'           RET430
396800951010     C* 2 = F12
396900951010     C     RET311        WHENEQ    '2'
397000951010     C                   MOVE      *ON           $ESCI                          sospese READC
397100951010     C*
397200951010     C                   ENDSL
397300951010     C*
397400951010     C* operazione eseguite dal pgm chiamato
397500951010     C*
397600951010     C                   SELECT
397700951010     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
397800951010     C     OPR311        WHENEQ    '1'
397900970630     c                   move      opr311        wopr
398000951010     C*  se è stata eseguito un aggiornamento
398100951010     C*  reperisco il nuovo stato del bottone
398200951010     C                   EXSR      TSTDIC                                                   chiamant
398300951010     C*
398400951010     C                   ENDSL
398500951010     C*
398600951010     C* funzione non eseguibile per errore :
398700951010     C*
398800951010     C                   SELECT
398900951010     C* 1 = funzione richiamata chiusa in errore
399000951010     C     ERR311        WHENEQ    '1'
399100951010     C                   MOVE      *ON           $ESCI                          sospese READC
399200951010     C                   SETON                                        5299      emiss.SFLMSGID
399300951010     C*
399400951010     C                   ENDSL
399500951010     C*
399600951010     C* salvo il codice d'errore da ripassare alla guida
399700951010     C                   MOVE      ERR311        WERR
399800951010     C*
399900951010     C                   ENDSR
400000951010     C/EJECT
400100950113     C************************************************************
400200950113     C* GESTIONE RITORNO DA PGM DI GESTIONE
400300950113     C************************************************************
400400950113     C     RTN315        BEGSR
400500950113     C*
400600950113     C* modo di ritorno
400700950113     C*
400800950113     C                   SELECT
400900950113     C* questi modi di utilizzo dei valori di ritorno dal
401000950113     C* pgm di manutenzione rcd di anagrafica sono delle
401100950113     C* proposte, normalmente sempre valide, ma modificabili
401200950113     C* per situazioni particolari
401300950113     C* 1 = F3
401400950113     C     RET315        WHENEQ    '1'
401500950113     C                   MOVE      *ON           $ESCI                          sospese READC
401600950113     C                   MOVE      *ON           $FINE                          fine pgm
401700950124     C                   MOVE      '2'           RET430
401800950113     C* 2 = F12
401900950113     C     RET315        WHENEQ    '2'
402000950113     C                   MOVE      *ON           $ESCI                          sospese READC
402100950113     C*
402200950113     C                   ENDSL
402300950113     C*
402400950113     C* operazione eseguite dal pgm chiamato
402500950113     C*
402600950113     C                   SELECT
402700950113     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
402800950113     C     OPR315        WHENEQ    '1'
402900970630     c                   move      opr315        wopr
403000950113     C*  se è stata eseguito un aggiornamento
403100950113     C*  reperisco il nuovo stato del bottone
403200950113     C                   EXSR      TSTPEP                                                   chiamant
403300950113     C*
403400950113     C                   ENDSL
403500950113     C*
403600950113     C* funzione non eseguibile per errore :
403700950113     C*
403800950113     C                   SELECT
403900950113     C* 1 = funzione richiamata chiusa in errore
404000950113     C     ERR315        WHENEQ    '1'
404100950113     C                   MOVE      *ON           $ESCI                          sospese READC
404200950113     C                   SETON                                        5299      emiss.SFLMSGID
404300950113     C*
404400950113     C                   ENDSL
404500950113     C*
404600950113     C* salvo il codice d'errore da ripassare alla guida
404700950113     C                   MOVE      ERR315        WERR
404800950113     C*
404900950113     C                   ENDSR
405000941205     C/EJECT
405100941205     C************************************************************
405200941205     C* GESTIONE RITORNO DA PGM DEI COMMENTI
405300941205     C************************************************************
405400941205     C     RTN725        BEGSR
405500941205     C*
405600941205     C* modo di ritorno
405700941205     C*
405800941205     C                   SELECT
405900941205     C* << questi modi di utilizzo dei valori di ritorno dal
406000941205     C*    pgm di manutenzione rcd di anagrafica sono delle
406100941205     C*    proposte, normalmente sempre valide, ma modificabili
406200941205     C*    per situazioni particolari >>
406300941205     C* 1 = F3
406400941205     C     RET725        WHENEQ    '1'
406500941205     C                   MOVE      *ON           $ESCI                          sospese READC
406600941205     C                   MOVE      *ON           $FINE                          fine pgm
406700950124     C                   MOVE      '2'           RET430
406800941205     C* 2 = F12
406900941205     C     RET725        WHENEQ    '2'
407000941205     C                   MOVE      *ON           $ESCI                          sospese READC
407100941205     C*
407200941205     C                   ENDSL
407300941205     C*
407400941205     C* operazione eseguite dal pgm chiamato
407500941205     C*
407600941205     C                   SELECT
407700941205     C* 1 = eseguita scrittura o modifica commento (lo segnalo sul sfl)
407800941205     C     OPR725        WHENEQ    '1'
407900941228     C*  se è stata eseguito un aggiornamento
408000941228     C*  reperisco il nuovo stato del bottone
408100941228     C                   EXSR      TSTTCM                                                   chiamant
408200941205     C*
408300941205     C                   ENDSL
408400941205     C*
408500941205     C* funzione non eseguibile per errore :
408600941205     C*
408700941205     C                   SELECT
408800941205     C* 1 = funzione richiamata chiusa in errore
408900941205     C*  eventualmente gestire altri codici di errore
409000941205     C     ERR725        WHENEQ    '1'
409100941205     C                   MOVE      *ON           $ESCI                          sospese READC
409200941205     C                   SETON                                        5299      emiss.SFLMSGID
409300941205     C*
409400941205     C                   ENDSL
409500950104     C*
409600950104     C* salvo il codice d'errore da ripassare alla guida
409700950104     C                   MOVE      ERR725        WERR
409800941205     C*
409900941205     C                   ENDSR
410000950313     C/EJECT
410100950313     C************************************************************
410200950313     C* GESTIONE RITORNO DA PGM DELLA DEFINIZIONE CAPOCONTI
410300950313     C************************************************************
410400950313     C     RTN206        BEGSR
410500950313     C*
410600950313     C* modo di ritorno
410700950313     C*
410800950313     C                   SELECT
410900950313     C* questi modi di utilizzo dei valori di ritorno dal
411000950313     C* pgm di manutenzione rcd di anagrafica sono delle
411100950313     C* proposte, normalmente sempre valide, ma modificabili
411200950313     C* per situazioni particolari
411300950313     C* 1 = F3
411400950313     C     RET206        WHENEQ    '1'
411500950313     C                   MOVE      *ON           $ESCI                          sospese READC
411600950313     C                   MOVE      *ON           $FINE                          fine pgm
411700950313     C                   MOVE      '2'           RET430
411800950313     C* 2 = F12
411900950313     C     RET206        WHENEQ    '2'
412000950313     C                   MOVE      *ON           $ESCI                          sospese READC
412100950313     C*
412200950313     C                   ENDSL
412300950313     C*
412400950313     C* operazione eseguite dal pgm chiamato
412500950313     C*
412600950313     C                   SELECT
412700950313     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
412800950313     C     OPR206        WHENEQ    '1'
412900950313     C                   MOVE      OPR206        WOPR                           avverto pgm chiamant
413000950313     C*  passo ai successivi pgm il codice capoconto creato
413100950313     C                   MOVE      KCC206        WWWKCC                                     chiamant
413200950615     C                   MOVE      ANL206        WWWANL                                     chiamant
413300950313     C*  se è stata eseguito un aggiornamento
413400950313     C*  reperisco il nuovo stato del bottone
413500950313     C                   EXSR      TSTPDC                                                   chiamant
413600950313     C*
413700950313     C                   ENDSL
413800950313     C*
413900950313     C* funzione non eseguibile per errore :
414000950313     C*
414100950313     C                   SELECT
414200950313     C* 1 = funzione richiamata chiusa in errore
414300950313     C     ERR206        WHENEQ    '1'
414400950313     C                   MOVE      *ON           $ESCI                          sospese READC
414500950313     C                   SETON                                        5299      emiss.SFLMSGID
414600950313     C* 2 = funzione richiamata senza la dovuta autorizzazione
414700950313     C     ERR206        WHENEQ    '2'
414800950313     C                   MOVE      *ON           $ESCI                                      C
414900950313     C                   SETON                                        5399                  ID
415000950313     C* 4 = conto non gestito a soggetti
415100950313     C     ERR206        WHENEQ    '4'
415200950313     C                   MOVE      *ON           $ESCI                                      C
415300950313     C                   SETON                                        5599                  ID
415400950313     C*
415500950313     C                   ENDSL
415600950313     C*
415700950313     C* salvo il codice d'errore da ripassare alla guida
415800950313     C                   MOVE      ERR206        WERR
415900950313     C*
416000950313     C                   ENDSR
416100950313     C/EJECT
416200950313     C************************************************************
416300950313     C* GESTIONE RITORNO DA PGM DELLE REGOLE CONTABILI
416400950313     C************************************************************
416500950313     C     RTN204        BEGSR
416600950313     C*
416700950313     C* modo di ritorno
416800950313     C*
416900950313     C                   SELECT
417000950313     C* questi modi di utilizzo dei valori di ritorno dal
417100950313     C* pgm di manutenzione rcd di anagrafica sono delle
417200950313     C* proposte, normalmente sempre valide, ma modificabili
417300950313     C* per situazioni particolari
417400950313     C* 1 = F3
417500950313     C     RET204        WHENEQ    '1'
417600950313     C                   MOVE      *ON           $ESCI                          sospese READC
417700950313     C                   MOVE      *ON           $FINE                          fine pgm
417800950313     C                   MOVE      '2'           RET430
417900950313     C* 2 = F12
418000950313     C     RET204        WHENEQ    '2'
418100950313     C                   MOVE      *ON           $ESCI                          sospese READC
418200950313     C*
418300950313     C                   ENDSL
418400950313     C*
418500950313     C* operazione eseguite dal pgm chiamato
418600950313     C*
418700950313     C                   SELECT
418800950313     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
418900950313     C     OPR204        WHENEQ    '1'
419000950313     C                   MOVE      OPR204        WOPR                           avverto pgm chiamant
419100951006     C                   MOVE      ANL204        WWWANL
419200950313     C*  se è stata eseguito un aggiornamento
419300950313     C*  reperisco il nuovo stato del bottone
419400950313     C                   EXSR      TSTRGC                                                   chiamant
419500950313     C*
419600950313     C                   ENDSL
419700950313     C*
419800950313     C* funzione non eseguibile per errore :
419900950313     C*
420000950313     C                   SELECT
420100950313     C* 1 = funzione richiamata chiusa in errore
420200950313     C     ERR204        WHENEQ    '1'
420300950313     C                   MOVE      *ON           $ESCI                          sospese READC
420400950313     C                   SETON                                        5299      emiss.SFLMSGID
420500950313     C* 2 = funzione richiamata senza la dovuta autorizzazione
420600950313     C     ERR204        WHENEQ    '2'
420700950313     C                   MOVE      *ON           $ESCI                                      C
420800950313     C                   SETON                                        5399                  ID
420900950313     C*
421000950313     C                   ENDSL
421100950313     C*
421200950313     C* salvo il codice d'errore da ripassare alla guida
421300950313     C                   MOVE      ERR204        WERR
421400950313     C*
421500950313     C                   ENDSR
421600950313     C/EJECT
421700950313     C************************************************************
421800950313     C* GESTIONE RITORNO DA PGM DEGLI ATTRIBUTI CONTABILI
421900950313     C************************************************************
422000950313     C     RTN205        BEGSR
422100950313     C*
422200950313     C* modo di ritorno
422300950313     C*
422400950313     C                   SELECT
422500950313     C* questi modi di utilizzo dei valori di ritorno dal
422600950313     C* pgm di manutenzione rcd di anagrafica sono delle
422700950313     C* proposte, normalmente sempre valide, ma modificabili
422800950313     C* per situazioni particolari
422900950313     C* 1 = F3
423000950313     C     RET205        WHENEQ    '1'
423100950313     C                   MOVE      *ON           $ESCI                          sospese READC
423200950313     C                   MOVE      *ON           $FINE                          fine pgm
423300950313     C                   MOVE      '2'           RET430
423400950313     C* 2 = F12
423500950313     C     RET205        WHENEQ    '2'
423600950313     C                   MOVE      *ON           $ESCI                          sospese READC
423700950313     C*
423800950313     C                   ENDSL
423900950313     C*
424000950313     C* operazione eseguite dal pgm chiamato
424100950313     C*
424200950313     C                   SELECT
424300950313     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
424400950313     C     OPR205        WHENEQ    '1'
424500950313     C                   MOVE      OPR205        WOPR                           avverto pgm chiamant
424600950313     C*  se è stata eseguito un aggiornamento
424700950313     C*  reperisco il nuovo stato del bottone
424800950313     C                   EXSR      TSTATC                                                   chiamant
424900950313     C*
425000950313     C                   ENDSL
425100950313     C*
425200950313     C* funzione non eseguibile per errore :
425300950313     C*
425400950313     C                   SELECT
425500950313     C* 1 = funzione richiamata chiusa in errore
425600950313     C     ERR205        WHENEQ    '1'
425700950313     C                   MOVE      *ON           $ESCI                          sospese READC
425800950313     C                   SETON                                        5299      emiss.SFLMSGID
425900950313     C* 2 = funzione richiamata senza la dovuta autorizzazione
426000950313     C     ERR205        WHENEQ    '2'
426100950313     C                   MOVE      *ON           $ESCI                                      C
426200950313     C                   SETON                                        5399                  ID
426300950313     C*
426400950313     C                   ENDSL
426500950313     C*
426600950313     C* salvo il codice d'errore da ripassare alla guida
426700950313     C                   MOVE      ERR205        WERR
426800950313     C*
426900950313     C                   ENDSR
427000950313     C/EJECT
427100950313     C************************************************************
427200950313     C* GESTIONE RITORNO DA PGM DELLA ANAGRAFICA CAPOCONTI
427300950313     C************************************************************
427400950313     C     RTN207        BEGSR
427500950313     C*
427600950313     C* modo di ritorno
427700950313     C*
427800950313     C                   SELECT
427900950313     C* questi modi di utilizzo dei valori di ritorno dal
428000950313     C* pgm di manutenzione rcd di anagrafica sono delle
428100950313     C* proposte, normalmente sempre valide, ma modificabili
428200950313     C* per situazioni particolari
428300950313     C* 1 = F3
428400950313     C     RET207        WHENEQ    '1'
428500950313     C                   MOVE      *ON           $ESCI                          sospese READC
428600950313     C                   MOVE      *ON           $FINE                          fine pgm
428700950313     C                   MOVE      '2'           RET430
428800950313     C* 2 = F12
428900950313     C     RET207        WHENEQ    '2'
429000950313     C                   MOVE      *ON           $ESCI                          sospese READC
429100950313     C*
429200950313     C                   ENDSL
429300950313     C*
429400950313     C* operazione eseguite dal pgm chiamato
429500950313     C*
429600950313     C                   SELECT
429700950313     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
429800950313     C     OPR207        WHENEQ    '1'
429900950313     C                   MOVE      OPR207        WOPR                           avverto pgm chiamant
430000950313     C*  se è stata eseguito un aggiornamento
430100950313     C*  reperisco il nuovo stato del bottone
430200950313     C                   EXSR      TSTPER                                                   chiamant
430300950313     C*
430400950313     C                   ENDSL
430500950313     C*
430600950313     C* funzione non eseguibile per errore :
430700950313     C*
430800950313     C                   SELECT
430900950313     C* 1 = funzione richiamata chiusa in errore
431000950313     C     ERR207        WHENEQ    '1'
431100950313     C                   MOVE      *ON           $ESCI                          sospese READC
431200950313     C                   SETON                                        5299      emiss.SFLMSGID
431300950313     C* 2 = funzione richiamata senza la dovuta autorizzazione
431400950313     C     ERR207        WHENEQ    '2'
431500950313     C                   MOVE      *ON           $ESCI                                      C
431600950313     C                   SETON                                        5399                  ID
431700950313     C* 3 = fornitore non attivo contabilmente
431800950313     C     ERR207        WHENEQ    '3'
431900950313     C                   MOVE      *ON           $ESCI                                      C
432000950313     C                   SETON                                        5499                  ID
432100950313     C* 4 = conto non gestito a soggetti
432200950313     C     ERR207        WHENEQ    '4'
432300950313     C                   MOVE      *ON           $ESCI                                      C
432400950313     C                   SETON                                        5599                  ID
432500950313     C* 5 = sottonatura non 'F'
432600950313     C     ERR207        WHENEQ    '5'
432700950313     C                   MOVE      *ON           $ESCI                                      C
432800950313     C                   SETON                                        5699                  ID
432900950313     C*
433000950313     C                   ENDSL
433100950313     C*
433200950313     C* salvo il codice d'errore da ripassare alla guida
433300950313     C                   MOVE      ERR207        WERR
433400950313     C*
433500950313     C                   ENDSR
433600950313     C/EJECT
433700950615     C************************************************************
433800950615     C* GESTIONE RITORNO DA PGM DI GESTIONE VOCI ANALITICA
433900950615     C************************************************************
434000950615     C     RTN280        BEGSR
434100950615     C*
434200950615     C* modo di ritorno
434300950615     C*
434400950615     C                   SELECT
434500950615     C* questi modi di utilizzo dei valori di ritorno dal
434600950615     C* pgm di manutenzione rcd di anagrafica sono delle
434700950615     C* proposte, normalmente sempre valide, ma modificabili
434800950615     C* per situazioni particolari
434900950615     C* 1 = F3
435000950615     C     RET280        WHENEQ    '1'
435100950615     C                   MOVE      *ON           $ESCI                          sospese READC
435200950615     C                   MOVE      *ON           $FINE                          fine pgm
435300950615     C                   MOVE      '2'           RET430
435400950615     C* 2 = F12
435500950615     C     RET280        WHENEQ    '2'
435600950615     C                   MOVE      *ON           $ESCI                          sospese READC
435700950615     C*
435800950615     C                   ENDSL
435900950615     C*
436000950615     C* operazione eseguite dal pgm chiamato
436100950615     C*
436200950615     C                   SELECT
436300950615     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
436400950615     C     OPR280        WHENEQ    '1'
436500970630     c                   move      opr280        wopr
436600950615     C*  se è stata eseguito un aggiornamento
436700950615     C*  reperisco il nuovo stato del bottone
436800950615     C                   EXSR      TSTVCA                                                   chiamant
436900950615     C*
437000950615     C                   ENDSL
437100950615     C*
437200950615     C* funzione non eseguibile per errore :
437300950615     C*
437400950615     C                   SELECT
437500950615     C* 1 = funzione richiamata chiusa in errore
437600950615     C     ERR280        WHENEQ    '1'
437700950615     C                   MOVE      *ON           $ESCI                          sospese READC
437800950615     C                   SETON                                        5299      emiss.SFLMSGID
437900950615     C* 2 = funzione richiamata senza la dovuta autorizzazione
438000950615     C     ERR280        WHENEQ    '2'
438100950615     C                   MOVE      *ON           $ESCI                                      C
438200950615     C                   SETON                                        5399                  ID
438300950615     C*
438400950615     C                   ENDSL
438500950615     C*
438600950615     C* salvo il codice d'errore da ripassare alla guida
438700950615     C                   MOVE      ERR280        WERR
438800950615     C*
438900950615     C                   ENDSR
439000980624     C/EJECT
439100980624     C************************************************************
439200980624     C* GESTIONE RITORNO DA PGM ANAGRAFICA FIDI
439300980624     C************************************************************
439400980624     C     RTN360        BEGSR
439500980624     C*
439600980624     C* modo di ritorno
439700980624     C*
439800980624     C                   SELECT
439900980624     C* questi modi di utilizzo dei valori di ritorno dal
440000980624     C* pgm di manutenzione rcd di anagrafica sono delle
440100980624     C* proposte, normalmente sempre valide, ma modificabili
440200980624     C* per situazioni particolari
440300980624     C* 1 = F3
440400980624     C     RET360        WHENEQ    '1'
440500980624     C                   MOVE      *ON           $ESCI                          sospese READC
440600980624     C                   MOVE      *ON           $FINE                          fine pgm
440700980624     C* 2 = F12
440800980624     C     RET360        WHENEQ    '2'
440900980624     C                   MOVE      *ON           $ESCI                          sospese READC
441000980624     C*
441100980624     C                   ENDSL
441200980624     C*
441300980624     C* operazione eseguite dal pgm chiamato
441400980624     C*
441500980624     C                   SELECT
441600980624     C* 1 = eseguito aggiornamento
441700980624     C     OPR360        WHENEQ    '1'
441800980624     C*
441900980624     C                   ENDSL
442000980624     C*
442100980624     C* funzione non eseguibile per errore :
442200980624     C*
442300980624     C                   SELECT
442400980624     C* 1 = funzione richiamata chiusa in errore
442500980624     C     ERR360        WHENEQ    '1'
442600980624     C                   MOVE      *ON           $ESCI                          sospese READC
442700980624     C                   SETON                                        5299      emiss.SFLMSGID
442800980624     C* 2 = funzione richiamata senza la dovuta autorizzazione
442900980624     C     ERR360        WHENEQ    '2'
443000980624     C                   MOVE      *ON           $ESCI                          sospese READC
443100980624     C                   SETON                                        5499      emiss.SFLMSGID
443200980624     C*
443300980624     C                   ENDSL
443400980624     C*
443500980624     C                   ENDSR
443600980624     C/EJECT
443700980624     C************************************************************
443800980624     C* GESTIONE RITORNO DA PGM SOCI PER AGENTE
443900980624     C************************************************************
444000980624     C     RTN600        BEGSR
444100980624     C*
444200980624     C* modo di ritorno
444300980624     C*
444400980624     C                   SELECT
444500980624     C* questi modi di utilizzo dei valori di ritorno dal
444600980624     C* pgm di manutenzione rcd di anagrafica sono delle
444700980624     C* proposte, normalmente sempre valide, ma modificabili
444800980624     C* per situazioni particolari
444900980624     C* 1 = F3
445000980624     C     RET600        WHENEQ    '1'
445100980624     C                   MOVE      *ON           $ESCI                          sospese READC
445200980624     C                   MOVE      *ON           $FINE                          fine pgm
445300980624     C* 2 = F12
445400980624     C     RET600        WHENEQ    '2'
445500980624     C                   MOVE      *ON           $ESCI                          sospese READC
445600980624     C*
445700980624     C                   ENDSL
445800980624     C*
445900980624     C* operazione eseguite dal pgm chiamato
446000980624     C*
446100980624     C                   SELECT
446200980624     C* 1 = eseguito aggiornamento
446300980624     C     OPR600        WHENEQ    '1'
446400980624     C*
446500980624     C                   ENDSL
446600980624     C*
446700980624     C* funzione non eseguibile per errore :
446800980624     C*
446900980624     C                   SELECT
447000980624     C* 1 = funzione richiamata chiusa in errore
447100980624     C     ERR600        WHENEQ    '1'
447200980624     C                   MOVE      *ON           $ESCI                          sospese READC
447300980624     C                   SETON                                        5299      emiss.SFLMSGID
447400980624     C* 2 = funzione richiamata senza la dovuta autorizzazione
447500980624     C     ERR600        WHENEQ    '2'
447600980624     C                   MOVE      *ON           $ESCI                          sospese READC
447700980624     C                   SETON                                        5499      emiss.SFLMSGID
447800980624     C*
447900980624     C                   ENDSL
448000980624     C*
448100980624     C                   ENDSR
448200940207     C/EJECT
448300010830G1036C************************************************************
448400010830  "  C* Gestione ritorno da pgm linee di acquisto
448500010830  "  C************************************************************
448600010830G1036C     Rtn1610       begsr
448700010830     C*
448800010830     C* modo di ritorno
448900010830     C                   select
449000010830     C* 1 = F3
449100010830     C     Ret1610       wheneq    '1'
449200010830     C                   move      *On           $Esci                          sospese READC
449300010830     C                   move      *On           $Fine                          fine pgm
449400010830     C                   move      '2'           Ret1610
449500010830     C* 2 = F12
449600010830     C     Ret1610       wheneq    '2'
449700010830     C                   move      *On           $Esci                          sospese READC
449800010830     C*
449900010830     C                   endsl
450000010830     C*
450100010830     C* operazione eseguite dal pgm chiamato
450200010830     C*
450300010830     C                   select
450400010830     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
450500010830     C     Opr1610       wheneq    '1'
450600010830     C                   move      Opr1610       WOpr                           avverto pgm chiamant
450700010830     C*  se è stata eseguito un aggiornamento
450800010830     C*  reperisco il nuovo stato del bottone
450900010830     C                   exsr      TstLDA                                                   chiamant
451000010830     C*
451100010830     C                   endsl
451200010830     C*
451300010830     C* funzione non eseguibile per errore :
451400010830     C*
451500010830     C                   select
451600010830     C* 1 = funzione richiamata chiusa in errore
451700010830     C     Err1610       wheneq    '1'
451800010830     C                   move      *On           $Esci                          sospese READC
451900010830     C                   seton                                        5299      emiss.SFLMSGID
452000010830     C*
452100010830     C                   endsl
452200010830     C*
452300010830     C* salvo il codice d'errore da ripassare alla guida
452400010830     C                   move      Err1610       WErr
452500010830     C*
452600010830G1036C                   endsr
452700010830G1036C************************************************************
452800010830  "  C* Gestione ritorno da pgm dati di magazzino (abilitazioni
452900010830  "  C* fornitori/magazzini)
453000010830  "  C************************************************************
453100010830G1036C     Rtn410        begsr
453200010830     C*
453300010830     C* modo di ritorno
453400010830     C                   select
453500010830     C* 1 = F3
453600010830     C     Ret410        wheneq    '1'
453700010830     C                   move      *On           $Esci                          sospese READC
453800010830     C                   move      *On           $Fine                          fine pgm
453900010830     C                   move      '2'           Ret410
454000010830     C* 2 = F12
454100010830     C     Ret410        wheneq    '2'
454200010830     C                   move      *On           $Esci                          sospese READC
454300010830     C*
454400010830     C                   endsl
454500010830     C*
454600010830     C* operazione eseguite dal pgm chiamato
454700010830     C*
454800010830     C                   select
454900010830     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
455000010830     C     Opr410        wheneq    '1'
455100010830     C                   move      Opr410        WOpr                           avverto pgm chiamant
455200010830     C*  se è stata eseguito un aggiornamento
455300010830     C*  reperisco il nuovo stato del bottone
455400010830     C                   exsr      TstFDM                                                   chiamant
455500010830     C*
455600010830     C                   endsl
455700010830     C*
455800010830     C* funzione non eseguibile per errore :
455900010830     C*
456000010830     C                   select
456100010830     C* 1 = funzione richiamata chiusa in errore
456200010830     C     Err410        wheneq    '1'
456300010830     C                   move      *On           $Esci                          sospese READC
456400010830     C                   seton                                        5299      emiss.SFLMSGID
456500010830     C*
456600010830     C                   endsl
456700010830     C*
456800010830     C* salvo il codice d'errore da ripassare alla guida
456900010830     C                   move      Err410        WErr
457000010830     C*
457100010830G1036C                   endsr
457200010830G1036C************************************************************
457300010830  "  C* Gestione ritorno da pgm assortimenti fornitori
457400010830  "  C************************************************************
457500010830G1036C     Rtn162        begsr
457600010830     C*
457700010830     C* modo di ritorno
457800010830     C                   select
457900010830     C* 1 = F3
458000010830     C     Ret162        wheneq    '1'
458100010830     C                   move      *On           $Esci                          sospese READC
458200010830     C                   move      *On           $Fine                          fine pgm
458300010830     C                   move      '2'           Ret162
458400010830     C* 2 = F12
458500010830     C     Ret162        wheneq    '2'
458600010830     C                   move      *On           $Esci                          sospese READC
458700010830     C*
458800010830     C                   endsl
458900010830     C*
459000010830     C* operazione eseguite dal pgm chiamato
459100010830     C*
459200010830     C                   select
459300010830     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
459400010830     C     Opr162        wheneq    '1'
459500010830     C                   move      Opr162        WOpr                           avverto pgm chiamant
459600010830     C*  se è stata eseguito un aggiornamento
459700010830     C*  reperisco il nuovo stato del bottone
459800010830     C                   exsr      TstASF                                                   chiamant
459900010830     C*
460000010830     C                   endsl
460100010830     C*
460200010830     C* funzione non eseguibile per errore :
460300010830     C*
460400010830     C                   select
460500010830     C* 1 = funzione richiamata chiusa in errore
460600010830     C     Err162        wheneq    '1'
460700010830     C                   move      *On           $Esci                          sospese READC
460800010830     C                   seton                                        5299      emiss.SFLMSGID
460900010830     C*
461000010830     C                   endsl
461100010830     C*
461200010830     C* salvo il codice d'errore da ripassare alla guida
461300010830     C                   move      Err162        WErr
461400010830     C*
461500010830G1036C                   endsr
461600940131     C************************************************************
461700940131     C* OPERAZIONI INIZIALI
461800940131     C************************************************************
461900940131     C     *INZSR        BEGSR
462000000114A0735C     *DMY          Move      Udate         ZDateISO
462100000114A0735C     *JobRun       Move      ZDateISO      ZDate             6 0
462200940131     C*
462300940127     C* Reperimento parametri
462400940127     C*
462500940117     C     *ENTRY        PLIST
462600940117     C                   PARM                    KPJBA
462700950103     C                   PARM                    AUTDS
462800940506     C*
462900940506     C* Reperimento tasti di funzione
463000940506     C*
463100940506     C                   EXSR      REPKEY
463200950118     C*
463300970919     C* Reperimento testi stato bottoni
463400950118     C*
463500950118     C                   EXSR      REPTBO
463600970923     C*
463700970923     C* Reperimento testi bottoni
463800970923     C*
463900970923     C                   EXSR      REPTST
464000970923     C*
464100970923     C*salvo la schiera con i valori dell'opzione agente per unità in una Ds
464200970924     C                   Eval      WPg12     =   Pg(12)
464300970924     C                   Eval      WPgt12    =   Pgt(12)
464400970924     C                   Eval      WDCb12    =   DCB(12)
464500940506     C*
464600940223     C                   ENDSR
464700940223     C/EJECT
464800940506     C************************************************************
464900940506     C* REPERIMENTO TASTI DI FUNZIONE
465000940506     C************************************************************
465100940506     C     REPKEY        BEGSR
465200940506     C                   MOVEA     $FM1          $FM
465300950906     C                   MOVEA     $FC1          $FC
465400940506     C                   EXSR      REPKKK
465500940506     C                   MOVEA     $FX           $FX1
465600940614     C*
465700940614     C                   ENDSR
465800940506     C/EJECT
465900940506     C************************************************************
466000940506     C* REPERIMENTO TASTI DI FUNZIONE
466100940506     C************************************************************
466200940506     C     REPKKK        BEGSR
466300940506     C*
466400940506     C                   CLEAR                   $FX
466500940506     C     1             DO        24            X
466600940506     C*
466700940506     C     $FM(X)        IFNE      *BLANK
466800950906     C     $FC(X)        ANDNE     '0'
466900940506     C*
467000940506     C                   MOVEL     $FM(X)        IDMSG
467100940506     C                   CALL      'XRTVM'
467200940506     C                   PARM                    IDMSG            27
467300940506     C                   PARM                    TXTMSG          100
467400940506     C                   PARM                    ERRMSG            1
467500940506     C*
467600940506     C     ERRMSG        IFNE      *ON
467700940506     C                   MOVEL     TXTMSG        $FX(X)
467800940506     C                   ELSE
467900940506     C                   MOVEL     *ALL' '       $FX(X)
468000940506     C                   END
468100940506     C*
468200940506     C                   END
468300940506     C*
468400940506     C                   END
468500940506     C*
468600940506     C                   ENDSR
468700950118     C/EJECT
468800950118     C************************************************************
468900950118     C* REPERIMENTO TESTI STATO BOTTONI
469000950118     C************************************************************
469100950118     C     REPTBO        BEGSR
469200950118     C*
469300050629B0818x**   1             DO        5             X
469400050629B0818C     1             DO        8             X
469500950118     C*
469600950118     C                   MOVEL     BOT(X)        IDMSG
469700950118     C                   CALL      'XRTVM'
469800950118     C                   PARM                    IDMSG            27
469900950118     C                   PARM                    TXTMSG          100
470000950118     C                   PARM                    ERRMSG            1
470100950118     C*
470200950118     C     ERRMSG        IFNE      *ON
470300950118     C                   MOVEL     TXTMSG        BTD(X)
470400950118     C                   ELSE
470500950118     C                   MOVEL     *ALL' '       BTD(X)
470600950118     C                   ENDIF
470700950118     C*
470800950118     C                   ENDDO
470900950118     C*
471000950118     C                   ENDSR
471100950118     C/EJECT
471200950118     C************************************************************
471300950118     C* REPERIMENTO TESTI BOTTONI
471400950118     C************************************************************
471500950118     C     REPTST        BEGSR
471600950118     C*
471700950118     C                   CLEAR                   DCB
471800970922     C*
471900950313     C     1             DO        NB1           X
472000950118     C*
472100950118     C                   MOVEL     PGT(X)        IDMSG
472200950118     C                   CALL      'XRTVM'
472300950118     C                   PARM                    IDMSG            27
472400950118     C                   PARM                    TXTMSG          100
472500950118     C                   PARM                    ERRMSG            1
472600950118     C*
472700950118     C     ERRMSG        IFNE      *ON
472800950118     C                   MOVEL     TXTMSG        DCB(X)
472900950118     C                   ELSE
473000950118     C                   MOVEL     *ALL' '       DCB(X)
473100950118     C                   ENDIF
473200950118     C*
473300950118     C                   ENDDO
473400950313     c* contabilità
473500950313     C                   CLEAR                   DCB1
473600950313     C     1             DO        NB2           X
473700950313     C*
473800950313     C                   MOVEL     PGT1(X)       IDMSG
473900950313     C                   CALL      'XRTVM'
474000950313     C                   PARM                    IDMSG            27
474100950313     C                   PARM                    TXTMSG          100
474200950313     C                   PARM                    ERRMSG            1
474300950313     C*
474400950313     C     ERRMSG        IFNE      *ON
474500950313     C                   MOVEL     TXTMSG        DCB1(X)
474600950313     C                   ELSE
474700950313     C                   MOVEL     *ALL' '       DCB1(X)
474800950313     C                   ENDIF
474900950313     C*
475000950313     C                   ENDDO
475100950118     C*
475200950118     C                   ENDSR
475300941205     C/EJECT
475400941205     C************************************************************
475500941205     C* Reperimento dati società
475600941205     C************************************************************
475700941205     C     REPSOC        BEGSR
475800941205     C*
475900941205     C                   CALL      'XSOC'
476000941205     C                   PARM                    TIPXSC            6
476100941205     C                   PARM                    SOCXSC            3
476200941205     C                   PARM                    CDSXSC            9 0
476300941205     C                   PARM                    MODXSC            3
476400941205     C                   PARM      *BLANKS       RTNXSC            1
476500941205     C                   PARM                    XSOCDS
476600941205     C                   PARM                    KPJBA
476700941205     C*
476800941205     C                   ENDSR
476900940509     C/EJECT
477000940223     C************************************************************
477100940223     C* INIZIALIZZAZIONE VARIABILI
477200940223     C************************************************************
477300940223     C     INZVAR        BEGSR
477400950112     C*
477500950112     C* Dati da PGM guida
477600950112     C*
477700950124     C                   MOVEL     KPJBU         A430DS
477800970924     C*
477900970924     C* Salvo la posizione del bottone agenti/unità ricevuta dalla Ds
478000970924     C                   Eval      WChi12    =   Chi(12)
478100970924     C                   Eval      WChm12    =   Chm(12)
478200941205     C*
478300941205     C* Reperimento dati società
478400941205     C*
478500941205     C                   MOVEL     'SOC001'      TIPXSC
478600941205     C                   MOVEL     *BLANK        SOCXSC
478700941205     C                   EXSR      REPSOC
478800941205     C     RTNXSC        IFNE      '1'
478900941205     C                   MOVEL     XSOCDS        SOC001
479000941205     C                   ELSE
479100941205     C                   RETURN
479200941205     C                   ENDIF
479300950110     C*
479400950110     C* gestione società di riferimento
479500950124     C     FSC430        IFEQ      'S'
479600950124     C     OPZ430        OREQ      *BLANK
479700950110     C                   MOVE      XSCSOC        WWWSOC
479800950110     C* gestione simulata da capofila
479900950110     C                   ELSE
480000950110     C                   MOVE      XSCCPA        WWWSOC
480100950110     C                   ENDIF
480200971013     C*
480300971013     C* Reperimento parametri di gestione dati fornitori
480400971013     C*
480500971013     C                   MOVEL     *BLANK        XPARIC
480600971013     C                   MOVEL     WWWSOC        XPASOC
480700971013     C                   MOVEL     'ANFRN00F'    XPAPAR
480800971013     C                   MOVEL     *BLANK        XPAOUT
480900971013     C                   EXSR      REPPAR
481000971013     C     XPAERR        IFEQ      '0'
481100971013     C                   MOVEL     XPAOUT        P020DS
481200971013     C                   ELSE
481300971013     C                   RETURN
481400971013     C                   ENDIF
481500980710     C*
481600980710     C* Reperimento parametri di gestione dati fornitori
481700980710     C*
481800980710     C                   MOVEL     *BLANK        XPARIC
481900980710     C                   MOVEL     WWWSOC        XPASOC
482000980710     C                   MOVEL     'ANSPE00F'    XPAPAR
482100980710     C                   MOVEL     *BLANK        XPAOUT
482200980710     C                   EXSR      REPPAR
482300980710     C     XPAERR        IFEQ      '0'
482400980710     C                   MOVEL     XPAOUT        P033DS
482500980710     C                   ELSE
482600980710     C                   RETURN
482700980710     C                   ENDIF
482800970923     C*
482900970924     C* se non siamo in immissione e nemmeno in copia
483000971013     C* e è abilitata la gestione Agenti/Unità ripristino la schiera
483100970923     C                   if        Opz430   <>   '01'  and
483200970924     C                             Opz430   <>   '03'  and
483300971013     C                             GAU020   =    *On
483400970924     C                   Eval      Pg(12)   =   WPg12
483500970924     C                   Eval      Pgt(12)  =   WPgt12
483600970924     C                   Eval      DCb(12)  =   WDCB12
483700970924     C                   Eval      Chi(12)  =   WChi12
483800970924     C                   Eval      Chm(12)  =   WChm12
483900970923     C                   endif
484000970923     C*
484100971013     C* se siamo in immissione oppure in copia oppure non è abilitata la
484200971013     C* gesttione Agenti/Unità
484300971013     C* non deve essere possibile selezionare questa opzione
484400971013     C                   IF        GAU020   <>   *On
484500970924     C                   Clear                   DCB(12)
484600970924     C                   move      '99'          CHI(12)
484700970924     C                   Move      '99'          CHM(12)
484800970924     c                   endif
484900971013     C                   IF        GAU020   <>   *On   or
485000970923     C                             Opz430   =    '01'  or
485100970923     C                             Opz430   =    '03'
485200970924     C                   Clear                   PGT(12)
485300970923     c                   endif
485400030514      *
485500030514B0549C* Se non c'è Proj distribuzione                                  a
485600030514  "  C* disattivo linee di acq, dati per magazzino, assortimenti forn.
485700030514  "  C                   IF        XSCGEST <> '1'
485800030514  "  C                   Clear                   DCB(15)
485900030514  "  C                   Clear                   PGT(15)
486000030514  "  C                   move      '99'          CHI(15)
486100030514  "  C                   Move      '99'          CHM(15)
486200030514  "  C                   Clear                   DCB(16)
486300030514  "  C                   Clear                   PGT(16)
486400030514  "  C                   move      '99'          CHI(16)
486500030514  "  C                   Move      '99'          CHM(16)
486600030514  "  C                   Clear                   DCB(17)
486700030514  "  C                   Clear                   PGT(17)
486800030514  "  C                   move      '99'          CHI(17)
486900030514  "  C                   Move      '99'          CHM(17)
487000030514B0549c                   endif
487100940718     C*
487200940718     C* Nel caso che, tra la passata chiamata a questo pgm e questa,
487300940718     C* ci sia stata una emissione a video del pgm chiamante
487400940718     C* chiudo e riapro il DSPF contenente la finestra
487500940718     C* per evitare il permanere del primo sfondo sotto la finestra.
487600940718     C* Nel caso contrario non eseguo la chiusura cosicchè rimane
487700940718     C* l'ultimo sfondo visualizzato
487800950124     C     EXF430        IFEQ      '1'
487900950124     C                   CLOSE     ANA430D
488000950124     C                   OPEN      ANA430D
488100940718     C                   END
488200940128     C*
488300940223     C* Variabili per gestione videate
488400940128     C*
488500940223     C                   MOVE      *OFF          $FINE                          fine pgm
488600940223     C                   MOVE      *ON           $INZD1                         inizializzione D1
488700941228     C                   MOVE      *OFF          $ESCI                          rich.uscita ciclo
488800941228     C                   MOVE      *HIVAL        $LASTV
488900950313     C                   MOVE      *BLANK        WWWKCC
489000940127     C*
489100940223     C* Variabili appoggio
489200940127     C*
489300940224     C                   Z-ADD     0             CURR
489400940224     C                   Z-ADD     0             CURC
489500941205     C                   CALL      'XDT4000'
489600941205     C                   PARM                    XDTAMG           10
489700000114A0735C*****              PARM      UDATE         XDTGMA            6 0
489800000114A0735C                   PARM      ZDate         XDTGMA            6 0
489900941205     C                   PARM      4             XDTSTA            1 0
490000941205     C                   MOVEL     XDTAMG        WDATE
490100940718     C                   MOVE      '0'           WEXF
490200941205     C                   MOVEL     *OFF          WOPR                                       ne S1
490300950104     C                   MOVE      '0'           WERR                                       ne S1
490400940224     C*
490500941219     C* Ordino i bottoni delle funzioni specifiche del cliente
490600941219     C* in modo gerarchico
490700940224     C*
490800941219     C                   EXSR      ORDBOT
490900951004     C*
491000951004     C                   MOVE      *off          WESP
491100951004     C* se non sono in una opzione diretta
491200951004     C     OPD430        IFEQ      *off
491300950103     C*
491400950103     C* reperisco se il video è da mostrare oppure no
491500950103     C                   SELECT
491600951004     C*
491700950124     C     OPZ430        WHENEQ    '01'
491800950124     C                   MOVE      ESI430        WESP
491900951004     C*
492000950124     C     OPZ430        WHENEQ    '02'
492100950124     C     OPZ430        OREQ      '05'
492200950103     C* se non è stata passata alcuna gerarchia in manutenzione
492300950103     C* mostro comunque il video del filtro
492400951004     C                   MOVEA     CHM           WCHM             20
492500951004     C     WCHM          IFEQ      *zeros
492600950103     C                   MOVE      *ON           WESP
492700950103     C* altrimenti mi baso sulla richiesta
492800950103     C                   ELSE
492900950124     C                   MOVE      ESM430        WESP
493000941219     C                   ENDIF
493100951004     C*
493200950103     C* se richiesta copia emetto comunque il video
493300950124     C     OPZ430        WHENEQ    '03'
493400950103     C                   MOVE      *ON           WESP
493500950317     C* se richiesto annullamento  non emetto il video
493600950317     C* e forzo opzione diretta per non emettere videate di contabilità
493700950124     C     OPZ430        WHENEQ    '04'
493800950103     C                   MOVE      *OFF          WESP
493900950317     C                   MOVE      *ON           OPD430
494000951004     C*
494100950103     C                   ENDSL
494200951004     C*
494300951004     C                   ENDIF
494400951004     C*
494500951004     C* se è richiesta espansione = NO
494600951004     C     WESP          IFEQ      '0'
494700951004     C* ma è stata richiesta solo la contabilità con opzione diretta
494800951004     C     KH(3)         IFNE      0
494900951004     C     OPD430        ANDEQ     *ON
495000951004     C* e non esiste una specifica preselezione
495100951004     C     CNM(1)        IFEQ      *BLANKS
495200951004     C     CNM(2)        ANDEQ     *BLANKS
495300951004     C     CNM(3)        ANDEQ     *BLANKS
495400951004     C     CNM(4)        ANDEQ     *BLANKS
495500951004     C     CNM(5)        ANDEQ     *BLANKS
495600951004     C     CNM(1)        OREQ      *ZEROS
495700951004     C     CNM(2)        ANDEQ     *ZEROS
495800951004     C     CNM(3)        ANDEQ     *ZEROS
495900951004     C     CNM(4)        ANDEQ     *ZEROS
496000951004     C     CNM(5)        ANDEQ     *ZEROS
496100951004     C* richiedo espansione = SI
496200951004     C                   MOVE      *ON           WESP
496300951004     C                   ENDIF
496400951004     C                   ENDIF
496500951004     C                   ENDIF
496600950313     C*
496700951004     C* se è stata richiesta solo la contabilità con opzione diretta
496800951004     C     KH(3)         IFNE      0
496900951004     C     OPD430        ANDEQ     *ON
497000951004     C* vado direttamente nel suo filtro
497100951004     C                   MOVE      'D2'          $GEST                          fmt in gestione
497200951004     C* altrimenti
497300951004     C                   ELSE
497400951004     C* parto da quello principale
497500951004     C                   MOVE      'D1'          $GEST                          fmt in gestione
497600951004     C                   ENDIF
497700941219     C*
497800941219     C* Inizializzazione/Decodifica prima videata
497900941219     C*
498000941219     C                   EXSR      INZD1
498100941219     C                   MOVE      *OFF          $INZD1
498200940127     C*
498300940117     C                   ENDSR
498400941219     C/EJECT
498500940128     C************************************************************
498600941219     C* ORDINAMENTO DEI BOTTONI
498700940223     C************************************************************
498800941219     C     ORDBOT        BEGSR
498900970923     C*-----------------------------------------------------------
499000970923     C* ATTENZIONE: per l'ordinamento dei bottoni è stabilito il valore
499100970923     C* 98 per quelli che, in ordine alfabetico, seguono quelli
499200970923     C* selezionati, e il valore 99 per quelli che devono scomparire se
499300970923     C* se la gestione societaria NON è a unità.
499400970923     C*-----------------------------------------------------------
499500940223     C*
499600941219     C* reperisco la gerarchia a seconda dell'opzione
499700950124     C     OPZ430        IFEQ      '01'
499800950302     C     OPZ430        OREQ      '03'
499900950313     C                   MOVE      CHI           KH
500000970923     C                   MOVE      CNI           KH1
500100941219     C                   ELSE
500200950313     C                   MOVE      CHM           KH
500300950313     C* Controllo se il cliente esiste in contabilità. Se non esiste
500400950313     C* prendo come schiera delle sequenze quella della immissione
500500950313     C* anche se siamo in modifica (non in visualizzazione)
500600950313     C                   EXSR      TSTPDC
500700950313     C     EXICNT        IFEQ      '0'
500800950313     C     OPZ430        ANDEQ     '02'
500900950313     C                   MOVE      CNI           KH1
501000950313     C                   ELSE
501100950313     C                   MOVE      CNM           KH1
501200950313     C                   ENDIF
501300941219     C                   ENDIF
501400941219     C*
501500941219     C* con bottoni e errori relativi creo una unica schiera correlata
501600941219     C* con quella delle gerarchie
501700950313     C                   DO        NB1           X
501800941228     C                   CLEAR                   GERARK
501900950118     C     KH(X)         IFEQ      0
502000970923     C                   Z-ADD     98            CH
502100950118     C                   ELSE
502200950118     C                   MOVEL     KH(X)         CH
502300950118     C                   ENDIF
502400950118     C                   MOVEL     PG(X)         PGM
502500950118     C                   MOVEL     DCB(X)        TXT
502600950118     C                   MOVEL     GERARK        MIX(X)
502700950313     C                   ENDDO
502800950313     C                   SORTA     MIX
502900941219     C*
503000950313     C* carico schiera programmi contabili
503100950313     C                   DO        NB2           X
503200950313     C                   CLEAR                   GERARC
503300950313     C     KH1(X)        IFEQ      0
503400970923     C                   Z-ADD     98            CHC
503500950313     C                   ELSE
503600950313     C                   MOVEL     KH1(X)        CHC
503700950313     C                   ENDIF
503800950313     C                   MOVEL     PG1(X)        PGMC
503900950313     C                   MOVEL     DCB1(X)       TXTC
504000950313     C                   MOVEL     GERARC        MIX1(X)
504100950313     C                   ENDDO
504200950313     C                   SORTA     MIX1
504300941219     C*
504400941219     C                   ENDSR
504500941219     C/EJECT
504600941219     C************************************************************
504700941219     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
504800941219     C************************************************************
504900941219     C     DEFCAM        BEGSR
505000941219     C*
505100980115     C                   ENDSR
505200940128     C************************************************************
505300940509     C/EJECT
505400940509     C************************************************************
505500940509     C* CONTROLLO ABILITAZIONE TASTI
505600940509     C************************************************************
505700940509     C     CTRKEY        BEGSR
505800940509     C                   SELECT
505900940509     C     $TASTO        WHENEQ    F01
506000940720     C     $FC(1)        IFEQ      '0'
506100940509     C                   SETON                                        99
506200940509     C                   ENDIF
506300940509     C     $TASTO        WHENEQ    F02
506400940720     C     $FC(2)        IFEQ      '0'
506500940509     C                   SETON                                        99
506600940509     C                   ENDIF
506700940509     C     $TASTO        WHENEQ    F03
506800940720     C     $FC(3)        IFEQ      '0'
506900940509     C                   SETON                                        99
507000940509     C                   ENDIF
507100940509     C     $TASTO        WHENEQ    F04
507200940720     C     $FC(4)        IFEQ      '0'
507300940509     C                   SETON                                        99
507400940509     C                   ENDIF
507500940509     C     $TASTO        WHENEQ    F05
507600940720     C     $FC(5)        IFEQ      '0'
507700940509     C                   SETON                                        99
507800940509     C                   ENDIF
507900940509     C     $TASTO        WHENEQ    F06
508000940720     C     $FC(6)        IFEQ      '0'
508100940509     C                   SETON                                        99
508200940509     C                   ENDIF
508300940509     C     $TASTO        WHENEQ    F07
508400940720     C     $FC(7)        IFEQ      '0'
508500940509     C                   SETON                                        99
508600940509     C                   ENDIF
508700940509     C     $TASTO        WHENEQ    F08
508800940720     C     $FC(8)        IFEQ      '0'
508900940509     C                   SETON                                        99
509000940509     C                   ENDIF
509100940509     C     $TASTO        WHENEQ    F09
509200940720     C     $FC(09)       IFEQ      '0'
509300940509     C                   SETON                                        99
509400940509     C                   ENDIF
509500940509     C     $TASTO        WHENEQ    F10
509600940720     C     $FC(10)       IFEQ      '0'
509700940509     C                   SETON                                        99
509800940509     C                   ENDIF
509900940509     C     $TASTO        WHENEQ    F11
510000940720     C     $FC(11)       IFEQ      '0'
510100940509     C                   SETON                                        99
510200940509     C                   ENDIF
510300940509     C     $TASTO        WHENEQ    F12
510400940720     C     $FC(12)       IFEQ      '0'
510500940509     C                   SETON                                        99
510600940509     C                   ENDIF
510700940509     C     $TASTO        WHENEQ    F13
510800940720     C     $FC(13)       IFEQ      '0'
510900940509     C                   SETON                                        99
511000940509     C                   ENDIF
511100940509     C     $TASTO        WHENEQ    F14
511200940720     C     $FC(14)       IFEQ      '0'
511300940509     C                   SETON                                        99
511400940509     C                   ENDIF
511500940509     C     $TASTO        WHENEQ    F15
511600940720     C     $FC(15)       IFEQ      '0'
511700940509     C                   SETON                                        99
511800940509     C                   ENDIF
511900940509     C     $TASTO        WHENEQ    F16
512000940720     C     $FC(16)       IFEQ      '0'
512100940509     C                   SETON                                        99
512200940509     C                   ENDIF
512300940509     C     $TASTO        WHENEQ    F17
512400940720     C     $FC(17)       IFEQ      '0'
512500940509     C                   SETON                                        99
512600940509     C                   ENDIF
512700940509     C     $TASTO        WHENEQ    F18
512800940720     C     $FC(18)       IFEQ      '0'
512900940509     C                   SETON                                        99
513000940509     C                   ENDIF
513100940509     C     $TASTO        WHENEQ    F19
513200940720     C     $FC(19)       IFEQ      '0'
513300940509     C                   SETON                                        99
513400940509     C                   ENDIF
513500940509     C     $TASTO        WHENEQ    F20
513600940720     C     $FC(20)       IFEQ      '0'
513700940509     C                   SETON                                        99
513800940509     C                   ENDIF
513900940509     C     $TASTO        WHENEQ    F21
514000940720     C     $FC(21)       IFEQ      '0'
514100940509     C                   SETON                                        99
514200940509     C                   ENDIF
514300940509     C     $TASTO        WHENEQ    F22
514400940720     C     $FC(22)       IFEQ      '0'
514500940509     C                   SETON                                        99
514600940509     C                   ENDIF
514700940509     C     $TASTO        WHENEQ    F23
514800940720     C     $FC(23)       IFEQ      '0'
514900940509     C                   SETON                                        99
515000940509     C                   ENDIF
515100940509     C     $TASTO        WHENEQ    F24
515200940720     C     $FC(24)       IFEQ      '0'
515300940509     C                   SETON                                        99
515400940509     C                   ENDIF
515500940509     C                   ENDSL
515600940509     C                   ENDSR
515700940509     C/EJECT
515800940509     C************************************************************
515900940509     C* RIEMPIMENTO TASTI FUNZIONE
516000940509     C************************************************************
516100940509     C     RIEKEY        BEGSR
516200940509     C*
516300940509     C* Input :
516400940509     C* - $FX = schiera con tutti i testi disponibili
516500940509     C* - $FC = schiere con i flag di validità associati ai testi
516600940509     C* - $FR = schiere con i flag di REVERSE IMAGE
516700940509     C* - $ULK = ultimo testo caricato
516800940509     C* Output :
516900940509     C* - $ULK
517000940609     C* - $KEY1= 1° riga di tasti funzione
517100940609     C* - $KEY2= 2° riga di tasti funzione
517200940509     C*
517300941228     C                   Z-ADD     35            $L01
517400941228     C                   Z-ADD     35            $L02
517500940616     C*
517600940509     C                   CALL      'XKEYMSG'
517700940509     C* input
517800940509     C                   PARM                    $FX
517900940509     C                   PARM                    $FC
518000950113     C                   PARM                    $FR
518100940509     C                   PARM                    $ULK              3 0
518200940509     C* output
518300940509     C                   PARM                    $KEY1            79
518400940509     C                   PARM                    $KEY2            79
518500940616     C                   PARM                    $L01              3 0
518600940616     C                   PARM                    $L02              3 0
518700940509     C*
518800060224A1148C                   PARM                    $ALLFUNCT       480
518900060224A1148C*
519000940509     C                   ENDSR
519100970217     C/EJECT
519200970217     C************************************************************
519300970217     C* REPERIMENTO PARAMETRI GESTIONE
519400970217     C************************************************************
519500970217     C     REPPAR        BEGSR
519600970217     C*
519700970217     C                   CALLb     'XPAR'
519800970217     C                   PARM                    XPASOC            3
519900970217     C                   PARM                    XPAPAR            8
520000970217     C                   PARM                    XPAOUT
520100970217     C                   PARM                    XPAERR            1
520200970217     C                   PARM                    XPARIC            1
520300970217     C*
520400970217     C                   ENDSR
520500980701      **------------------------------------------------------------**
520600980701      ** VentilTCM  ** Ventilazione dati sulle aziende collegate    **
520700980701      **------------------------------------------------------------**
520800980701     CSR   VentilTCM     BegSR
520900980701     C                   clear                   X53AMCds
521000980701     C                   reset                   X53AMCds
521100980701     C                   eval      SOCIETAamc = xscSOC
521200980701     C                   eval      CAPamc     = xscCPA
521300980701     C                   eval      KSCamc     = ksc430
521400980701     C                   eval      LDVamc     = *blanks
521500980707     C                   eval      TCMamc     = '001'
521600980703     C                   eval      FLGamc     = '3'
521700980701     C                   eval      FNZamc     = 'TCM'
521800980701     C                   clear                   kpjbu
521900980701     C                   movel     X53AMCds      kpjbu
522000980701     C                   call      'X53AMC0'
522100980701     C                   parm                    kpjba
522200980701     C                   parm                    jbq020
522300980701     C                   EndSR
522400980701     C*
522500980701     C/EJECT
522600980701      **------------------------------------------------------------**
522700980710      ** TestANP020       ** Controllo Parametri ANP020ds           **
522800980701      **------------------------------------------------------------**
522900980701     CSR   TestANP020    BegSR
523000980701     C*
523100980701     C* Se siamo in logica di GRUPPO per società:
523200980701     C* - Inserimento AUTOMATICO (da capofila)
523300980701     C* - FLAG di procedura attivato
523400980701     C* NON E' CONSENTITO: inserimento, copia
523500980701     C                   if        gsp020     = 'G'    and
523600980701     C                             imm020     = 'A'    and
523700980701     C                             wfgi       = *on
523800980701     C                   if        wopz       = '01'   or
523900980701     C                             wopz       = '03'
524000980701     C                   eval      wopz       = '05'
524100980701     C                   endif
524200980701     C                   endif
524300980701     C* Se siamo in logica di GRUPPO per società:
524400980701     C* - Aggiornamento AUTOMATICO (da capofila)
524500980701     C* - FLAG di procedura attivato
524600980701     C* NON E' CONSENTITO: variazione, annullamento
524700980701     C                   If        gsp020     = 'G'    and
524800980701     C                             agg020     = 'A'    and
524900980701     C                             wfga       = *on
525000980701     C                   if        wopz       = '02'   or
525100980701     C                             wopz       = '04'
525200980701     C                   eval      wopz       = '05'
525300980701     C                   endif
525400980701     C                   endif
525500980701     C*
525600980701     C                   EndSR
525700980701     C*
525800980710      **------------------------------------------------------------**
525900980710      ** TestANP020c      ** Controllo Parametri ANP020ds           **
526000980701      **------------------------------------------------------------**
526100980701     CSR   TstcANP020    BegSR
526200980701     C*
526300980701     C* Se siamo in logica di GRUPPO per società:
526400980701     C* - Inserimento AUTOMATICO (da capofila)
526500980701     C* - FLAG di procedura attivato
526600980701     C* NON E' CONSENTITO: inserimento, copia
526700980701     C*
526800980701     C                   eval      $copok     = *on
526900980701     C*
527000980701     C                   if        gsp020     = 'G'    and
527100980701     C                             imm020     = 'A'    and
527200980701     C                             wfgi       = *on
527300980701     C                   eval      $copok     = *off
527400980701     C                   endif
527500980701     C*
527600980701     C                   EndSR
527700980701     C*
527800980710      **------------------------------------------------------------**
527900980710      ** TestANP033c      ** Controllo Parametri ANP033ds           **
528000980710      **------------------------------------------------------------**
528100980710     CSR   TstcANP033    BegSR
528200980710     C*
528300980710     C* Se siamo in logica di GRUPPO per società:
528400980710     C* - Inserimento AUTOMATICO (da capofila)
528500980710     C* - FLAG di procedura attivato
528600980710     C* NON E' CONSENTITO: inserimento, copia
528700980710     C*
528800980710     C                   eval      $copok     = *on
528900980710     C*
529000980710     C                   if        gsp033     = 'G'    and
529100980710     C                             imm033     = '1'    and
529200980710     C                             wfgi       = *on
529300980710     C                   eval      $copok     = *off
529400980710     C                   endif
529500980710     C*
529600980710     C                   EndSR
529700980710     C*
529800980701     C/EJECT
529900950118**CTDATA $FM1
530000940509PROMSG    *LIBL     KEY0001  01
530100940509PROMSG    *LIBL     KEY0002  02
530200940509PROMSG    *LIBL     KEY0003  03
530300940509PROMSG    *LIBL     KEY0004  04
530400940509PROMSG    *LIBL     KEY0005  05
530500940509PROMSG    *LIBL     KEY0006  06
530600940509PROMSG    *LIBL     KEY0007  07
530700940509PROMSG    *LIBL     KEY0008  08
530800940509PROMSG    *LIBL     KEY0009  09
530900940509PROMSG    *LIBL     KEY0010  10
531000940509PROMSG    *LIBL     KEY0011  11
531100940509PROMSG    *LIBL     KEY0012  12
531200940509PROMSG    *LIBL     KEY0013  13
531300940509PROMSG    *LIBL     KEY0014  14
531400940509PROMSG    *LIBL     KEY0015  15
531500940509PROMSG    *LIBL     KEY0016  16
531600940509PROMSG    *LIBL     KEY0017  17
531700940509PROMSG    *LIBL     KEY0018  18
531800940509PROMSG    *LIBL     KEY0019  19
531900940509PROMSG    *LIBL     KEY0020  20
532000940509PROMSG    *LIBL     KEY0021  21
532100940509PROMSG    *LIBL     KEY0022  22
532200940509PROMSG    *LIBL     KEY0023  23
532300940509PROMSG    *LIBL     KEY0024  24
532400940509**CTDATA $FC1
5325009407200N  01
5326009407200N  02
5327009407200N  03
5328009407200N  04
5329009407200N  05
5330009407200N  06
5331009407200N  07
5332009412191N  08
5333009407200N  09
5334009407200N  10
5335009407200N  11
5336009407201N  12
5337009407200N  13
5338009407200N  14
5339009407200N  15
5340009407200N  16
5341009407200N  17
5342009407200N  18
5343009407200N  19
5344009407200N  20
5345009407200N  21
5346009407200N  22
5347009509060N  23   Impostare a '1' se si utilizzano opzioni
5348009509061N  24   Lasciare sempre impostata a '1' il valore della riga 24
534900950118**CTDATA PG
535000950124FRN PROMSG    *LIBL     COS0020
535100950118SOG PROMSG    *LIBL     COS0056
535200950313RCO PROMSG    *LIBL     COS0082
535300950118CBA PROMSG    *LIBL     COS0054
535400950118FIL PROMSG    *LIBL     COS0031
535500950124FRS PROMSG    *LIBL     COS0024
535600950118PEP PROMSG    *LIBL     COS0053
535700980702SPE PROMSG    *LIBL     COS0060
535800950124AGE PROMSG    *LIBL     COS0061
535900950118TCM PROMSG    *LIBL     COS0052
536000951010DIC PROMSG    *LIBL     COS0154
536100970919AGU PROMSG    *LIBL     COS0284
536200980624FID PROMSG    *LIBL     COS2006
536300980624SOA PROMSG    *LIBL     COS0395
536400010830LDA PROMSG    *LIBL     COS2017
536500010830FDM PROMSG    *LIBL     COS2018
536600010830ASF PROMSG    *LIBL     COS2019
536700950313**CTDATA PG1
536800950313PDC PROMSG    *LIBL     COS0077
536900950313RGC PROMSG    *LIBL     COS0078
537000950313ATC PROMSG    *LIBL     COS0079
537100950313PER PROMSG    *LIBL     COS0080
537200950313VCA PROMSG    *LIBL     COS0081
537300950118**CTDATA BOT
537400950118PROMSG    *LIBL     COS0036    inserire
537500950118PROMSG    *LIBL     COS0037    presente
537600950118PROMSG    *LIBL     COS0038    correggere
537700950118PROMSG    *LIBL     COS0039    annullato
537800950118PROMSG    *LIBL     COS0040    non autorizzato
537900050629PROMSG    *LIBL     COS2587    commerciale                       B0818
538000050629PROMSG    *LIBL     COS2588    factor                              "
538100050629PROMSG    *LIBL     COS2589    assic.crediti                     B0818
