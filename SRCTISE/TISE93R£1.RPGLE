000100000000     H*------------------------------------------------------------------------*
000200011106     H*                      - STATISTICA DELTA FILIALE -                      *
000300970820     H*------------------------------------------------------------------------*
000400970820     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800100202     FWFdea01L  IF   E           K DISK
000900100511     F*****WFDELPCF  UF A E             DISK
001000070418     FWAdelpcf  UF A E             DISK
001100100216     FWAdeapcI  UF A E             DISK
001200091124     FWAdelpcN  UF A E             DISK    usrOPN
001300130131     F*----------------------------------   x caricare la VARIA (f=FUEL) x CLIENTE a TOTALE
001400130131     FWAdelPCV  UF A E             DISK
001500130131     FWFDEAV1L  uF   E           K DISK
001600130131     F*----------------------------------
001700070420     FTFntc01L  IF   E           K DISK
001800000000     FAZORG01L  IF   E           K DISK
001900000000     FTABEL00F  IF   E           K DISK
002000100129     FTISE94P   O    E             PRINTER OFLIND(*IN01) usropn
002100030925     FWFTISE93L UF A E           K DISK
002200000000     D*------------------------------------------------------------------------*
002300000000     D* SCHIERE
002400000000     D*------------------------------------------------------------------------*
002500000000     D*------------------
002600000000     D* DECODIFICA MESI DELL'ANNO
002700000000     D*------------------
002800000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
002900000000     D*------------------
003000000000     D* DECODIFICA STATISTICHE
003100000000     D*------------------
003200000000     D DST             S             25    DIM(5) CTDATA PERRCD(1)
003300000000     D*------------------
003400000000     D* DECODIFICA RIGHE DI STAMPA
003500000000     D*------------------
003600000825     D DRI             S             24    DIM(30) CTDATA PERRCD(1)
003700020318     D*------------------
003800020318     D* DECODIFICA NETWORK
003900020318     D*------------------
004000020318     D DNTW            S              3    DIM(5) CTDATA PERRCD(1)
004100000000     D*------------------
004200000000     D* ORGANIGRAMMA
004300000000     D*------------------
004400971124     D FIL             S              3  0 DIM(500)                             *FILIALI
004500971124     D DFIL            S             20    DIM(500)
004600971124     D ARE             S              3  0 DIM(500)                             *AREE
004700971124     D DARE            S             20    DIM(500)
004800971124     D DIV             S              1    DIM(500)                             *DIVISIONI
004900971124     D DDIV            S             20    DIM(500)
005000020627     D POS             S              3    DIM(500)                             *NETWORK POSTE
005100000000     D*------------------
005200000000     D* COMMERCIALI
005300000000     D*------------------
005400100202     D AGE             S              7  0 DIM(5000)                            *CODICI
005500100202     D DAGE            S             20    DIM(5000)                            *DECODIFICHE
005600100202     D CAGE            S              3  0 DIM(5000)                            *PERCENTUALI COMPETE
005700960708     D*------------------
005800960708     D* CAUSALI BLOCCO CLIENTI
005900960708     D*------------------
006000960708     D CBL             S              3    DIM(50)                              *CODICI
006100960708     D DCBL            S             30    DIM(50)                              *DECODIFICHE
006200000000     D*------------------
006300000000     D* MESI DA ELABORARE
006400000000     D*------------------
006500000000     D MES             S              2  0 DIM(12)
006600000000     D*------------------
006700000000     D* ANNI DEI MESI DA ELABORARE
006800000000     D*------------------
006900990609     D ANN             S              4  0 DIM(12)
007000000000     D*------------------
007100000000     D* CATEGORIE CLIENTI
007200000000     D*------------------
007300020220     D ABC             S              1    DIM(5)                               QUALITA' CLIENTE
007400000000     D*------------------
007500000000     D* PERCENTUALE COMPETENZA
007600000000     D*------------------
007700000000     D PCF             S              1    DIM(3)
007800000000    ?D*-------------------
007900000000    ?D* S C H I E R E   N O N   M U L T I P L E
008000000000    ?D*-------------------
008100030924     D*------------------
008200030924     D* FLAG CONTATORE CLIENTE PER MESE
008300030924     D*------------------
008400000830     D SFCCL           S              1    DIM(12)                              FLAG CONTEGGIO
008500000000     D*------------------
008600000000     D* SCHIERE DI MEMORIZZAZIONE VALORI DI UN CLIENTE
008700000000     D*------------------
008800000000     D S1K             S             13  0 DIM(13)                              SPEDIZIONI UN CLIENT
008900081013     D KG1K            S             15  1 DIM(13)                              Pesi x UN CLIENT
009000020116     D R1K             S             19  5 DIM(13)                              RICAVI REALI UN CLIE
009100020116     D P1K             S             19  5 DIM(13)                              RICAVI PRESUNTI UN C
009200970822     D D1K             S              1    DIM(13)                              FLAG %SCOSTAMENTO DE
009300020712     D R1KP            S             19  5 DIM(12)                              RICAVI REALI UN CLIE
009400121217     D CLN1K           S              1    DIM(13)                              FLAG NUOVO ACQUISITO
009500070419     D*------------------
009600070419     D ap_S1K          S             13  0 DIM(12)                              SPEDIZIONI UN CLIENT
009700081013     D ap_KG1K         S             15  1 DIM(13)                              Pesi x UN CLIENT ANN
009800070419     D ap_R1K          S             19  5 DIM(12)                              RICAVI REALI UN CLIE
009900070419     D ap_P1K          S             19  5 DIM(12)                              RICAVI PRESUNTI UN C
010000091123     D*------------------
010100091124     D* SCHIERE PER TOTALI x NETWORK
010200091123     D*------------------
010300091124     D W1ntw           DS                  OCCURS(5) INZ
010400091124     d  c1ntw                         3
010500091124     D  S1ntw                        13  0 DIM(12)                              SPEDIZIONI UN CLIENT
010600091124     D  KG1ntw                       15  1 DIM(12)                              Pesi x UN CLIENT
010700091124     D  R1ntw                        19  5 DIM(12)                              RICAVI REALI UN CLIE
010800091124     D  P1ntw                        19  5 DIM(12)                              RICAVI PRESUNTI UN C
010900091124     D  D1ntw                         1    DIM(12)                              FLAG %SCOSTAMENTO DE
011000091124     D  R1Pntw                       19  5 DIM(12)                              RICAVI REALI UN CLIE
011100091125     D  TR1ntw                       15  0
011200091125     D  TP1ntw                       15  0
011300091123     D*------------------
011400091124     D* PER TOTALI anno Preced x NETWORK
011500091123     D*------------------
011600091124     D WAP1ntw         DS                  OCCURS(5) INZ
011700091124     d  ap_c1ntw                      3
011800091124     D  ap_S1ntw                     13  0 DIM(12)                              SPEDIZIONI UN CLIENT
011900091124     D  ap_KG1ntw                    15  1 DIM(12)                              Pesi x UN CLIENT
012000091124     D  ap_R1ntw                     19  5 DIM(12)                              RICAVI REALI UN CLIE
012100091124     D  ap_P1ntw                     19  5 DIM(12)                              RICAVI PRESUNTI UN C
012200091124     D  ap_D1ntw                      1    DIM(12)                              FLAG %SCOSTAMENTO DE
012300091124     D  ap_R1Pntw                    19  5 DIM(12)                              RICAVI REALI UN CLIE
012400091125     D  ap_TR1ntw                    15  0
012500091125     D  ap_TP1ntw                    15  0
012600091123     D*------------------
012700100202     D* SCHIERE PER TOTALI x NETWORK e TIPI SERVIZI sia Anno in corso che anno precedente
012800100202      *   legate dalla stessa chiave ossi NTW + Tipo servizio. ....quindi......
012900100129      *   le occorrenze sono date dalla moltiplica dei network x i tipi servizi possibili
013000100202      *   ossia 5(ntw) x 10(tipi serv.) = 50
013100100204     D*------------------
013200100204      * x comodità di calcolo, il codice NTW per l' ITALIA è blank e verra impostato a (ITA).
013300100129     D*------------------
013400100129     D W1ntwTS         DS                  OCCURS(50) INZ
013500100129     d  c1ntwTS                       3
013600100201     d  c1tpsTS                       1
013700100202      *-- Anno in corso
013800100202     D     S1ntwTS                   13  0 DIM(12)                              SPEDIZIONI UN CLIENT
013900100202     D     KG1ntwTS                  15  1 DIM(12)                              Pesi x UN CLIENT
014000100202     D     R1ntwTS                   19  5 DIM(12)                              RICAVI REALI UN CLIE
014100100202     D     P1ntwTS                   19  5 DIM(12)                              RICAVI PRESUNTI UN C
014200100202     D     D1ntwTS                    1    DIM(12)                              FLAG %SCOSTAMENTO DE
014300100202     D     R1PntwTS                  19  5 DIM(12)                              RICAVI REALI UN CLIE
014400100202     D     TR1ntwTS                  15  0
014500100202     D     TP1ntwTS                  15  0
014600100202      *-- Anno precedente
014700100202     D  ap_S1ntwTS                   13  0 DIM(12)                              SPEDIZIONI UN CLIENT
014800100202     D  ap_KG1ntwTS                  15  1 DIM(12)                              Pesi x UN CLIENT
014900100202     D  ap_R1ntwTS                   19  5 DIM(12)                              RICAVI REALI UN CLIE
015000100202     D  ap_P1ntwTS                   19  5 DIM(12)                              RICAVI PRESUNTI UN C
015100100202     D  ap_D1ntwTS                    1    DIM(12)                              FLAG %SCOSTAMENTO DE
015200100202     D  ap_R1PntwTS                  19  5 DIM(12)                              RICAVI REALI UN CLIE
015300100202     D  ap_TR1ntwTS                  15  0
015400100202     D  ap_TP1ntwTS                  15  0
015500100129     D*------------------
015600030924     D CNW             S             11  0 DIM(13)                              NUMERO C.NUOVI
015700030924     D SNW             S             13  0 DIM(13)                              SPEDIZIONI C.NUOVI
015800030924     D RNW             S             19  5 DIM(13)                              RICAVI REALI C.NUOVI
015900030924     D PNW             S             19  5 DIM(13)                              RICAVI PRESUNTI C.NU
016000030924     D CAW             S             11  0 DIM(13)                              NUMERO C.ACQUISITI
016100030924     D SAW             S             13  0 DIM(13)                              SPEDIZIONI C.ACQUISI
016200020116     D RAW             S             19  5 DIM(13)                              RICAVI REALI C.ACQ
016300020116     D PAW             S             19  5 DIM(13)                              RICAVI PRESUNTI C.AC
016400020221     D*------------------
016500020318     D* SCHIERE DI LAVORO PER TOTALI "D"
016600020221     D*------------------
016700020221     D CNWD            S             11  0 DIM(13)                              NUMERO C.NUOVI
016800020221     D SNWD            S             13  0 DIM(13)                              SPEDIZIONI C.NUOVI
016900020221     D RNWD            S             19  5 DIM(13)                              RICAVI REALI C.NUOVI
017000020221     D PNWD            S             19  5 DIM(13)                              RICAVI PRESUNTI C.NU
017100020221     D CAWD            S             11  0 DIM(13)                              NUMERO C.ACQUISITI
017200020221     D SAWD            S             13  0 DIM(13)                              SPEDIZIONI C.ACQUISI
017300020221     D RAWD            S             19  5 DIM(13)                              RICAVI REALI C.ACQ
017400020221     D PAWD            S             19  5 DIM(13)                              RICAVI PRESUNTI C.AC
017500000000     D*---
017600020220     D* MULTIPLA PER 13° TOT CLIENTI (TOTALI "A" + "B" + "C" + "T")
017700000000     D*---
017800951227     D NTW             S             11  0 DIM(2)                               TOTALE CLIENTI NEW
017900951227     D ATW             S             11  0 DIM(2)                               TOTALE CLIENTI ACQ
018000000000     D*------------------
018100000000     D* SCHIERE DI LAVORO PER RIGHE DETTAGLIO (CLI/SPE/RIC/DEL)
018200000000     D*------------------
018300000000     D CLW             S             13  0 DIM(13)                              NUMERO CLIENTI
018400000000     D SPW             S             13  0 DIM(13)                              SPEDIZIONI
018500020116     D RRW             S             19  5 DIM(13)                              RICAVO REALE
018600020116     D RPW             S             19  5 DIM(13)                              RICAVO PRESUNTO
018700000000     D*---
018800951227     D* NON MULTIPLA PER 13° TOT CLIENTI DETTAGLIO (CLI/SPE/RIC/DEL)
018900000000     D*---
019000951227     D CTW             S             11  0 DIM(2)                               13° TOTALE CLIENTI
019100000000     D*------------------
019200000000     D* SCHIERE DI LAVORO
019300000000     D*------------------
019400000000     D $CL             S             13  0 DIM(13)                              NUMERO CLIENTI
019500000000     D $SP             S             13  0 DIM(13)                              SPEDIZIONI
019600020116     D $RR             S             19  5 DIM(13)                              RICAVO REALE
019700020116     D $RP             S             19  5 DIM(13)                              RICAVO PRESUNTO
019800000000     D $PD             S              4  1 DIM(13)                              DELTA
019900970822     D $SD             S              1    DIM(13)                              SCOSTAMENTO DELTA
020000020116     D $VA             S             19  5 DIM(13)                              VALORE GENERICO
020100020701     D $PDF            S              4  1 DIM(13)                              SCOSTAMENTO % FATTUR
020200020319     D*------------------
020300020319     D* SCHIERA DI WRK X CONTEGGIO NETWORK SU ANNO "P" E ANNO "C" (LA DIMENSIONE=NUM. MESI x NUM. NE
020400020319     D*------------------
020500020319     D WSK1            S             12    DIM(130)
020600030925     d*---
020700030925     d* Variabili
020800030925     d*---
020900030925     d WNrr            S              7  0 INZ                                  *contatore righe
021000030926     d WTipStamp2      S              1    INZ                                  *STAMP2 chiamata da
021100000000     D*------------------------------------------------------------------------*
021200000000     D* INPUT
021300000000     D*------------------------------------------------------------------------*
021400000000     D KPJBA         E DS
021500000000     D*------------------
021600000000     D* NUMERO DEI BYTE IN PACKED:
021700000000     D*------------------
021800020116     D* CLIENTI                 (11:2) + 1 =  6,5  /  (6 X 13) =  78
021900020116     D* SPEDIZIONI              (13:2) + 1 =  7,5  /  (7 X 13) =  91
022000020116     D* RICAVI                  (19:2) + 1 =  9,5  /  (9 X 13) = 117
022100020116     D* TOTALE CLIENTI          (11:2) + 1 =  6,5  /  (6 X  2) =  12
022200000000     D*------------------
022300020220     D* DS MULTIPLA TOTALI GENERALI     - 5 CATEGORIE DI CLIENTI -
022400000000     D*------------------
022500020220     D ABCGEN          DS                  OCCURS(5) INZ
022600000000     D  CNG                    1     78P 0
022700000000     D                                     DIM(13)                              NUMERO C.NUOVI
022800000000     D  SNG                   79    169P 0
022900000000     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
023000020117     D  RNG                  170    286P 5
023100000000     D                                     DIM(13)                              RICAVI REALI C.NUOVI
023200020117     D  PNG                  287    403P 5
023300000000     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
023400020116     D  CAG                  404    481P 0
023500000000     D                                     DIM(13)                              NUMERO C.ACQUISITI
023600020116     D  SAG                  482    572P 0
023700000000     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
023800020117     D  RAG                  573    689P 5
023900000000     D                                     DIM(13)                              RICAVI REALI C.ACQ
024000020117     D  PAG                  690    806P 5
024100000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
024200000000     D*---
024300020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
024400000000     D*---
024500020220     D ABCG13          DS                  OCCURS(5) INZ
024600951227     D  NTG                    1     12P 0
024700951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
024800951227     D  ATG                   13     24P 0
024900951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
025000000000     D*------------------
025100020220     D* DS MULTIPLA TOTALI DIVISIONE    - 5 CATEGORIE DI CLIENTI -
025200000000     D*------------------
025300020220     D ABCDIV          DS                  OCCURS(5) INZ
025400020117     D  CND                    1     78P 0
025500020117     D                                     DIM(13)                              NUMERO C.NUOVI
025600020117     D  SND                   79    169P 0
025700020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
025800020117     D  RND                  170    286P 5
025900020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
026000020117     D  PND                  287    403P 5
026100020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
026200020117     D  CAD                  404    481P 0
026300020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
026400020117     D  SAD                  482    572P 0
026500020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
026600020117     D  RAD                  573    689P 5
026700020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
026800020117     D  PAD                  690    806P 5
026900000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
027000000000     D*---
027100020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
027200000000     D*---
027300020220     D ABCD13          DS                  OCCURS(5) INZ
027400951227     D  NTD                    1     12P 0
027500951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
027600951227     D  ATD                   13     24P 0
027700951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
027800000000     D*------------------
027900020220     D* DS MULTIPLA TOTALI AREA         - 5 CATEGORIE DI CLIENTI -
028000000000     D*------------------
028100020220     D ABCARE          DS                  OCCURS(5) INZ
028200020117     D  CNA                    1     78P 0
028300020117     D                                     DIM(13)                              NUMERO C.NUOVI
028400020117     D  SNA                   79    169P 0
028500020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
028600020117     D  RNA                  170    286P 5
028700020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
028800020117     D  PNA                  287    403P 5
028900020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
029000020117     D  CAA                  404    481P 0
029100020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
029200020117     D  SAA                  482    572P 0
029300020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
029400020117     D  RAA                  573    689P 5
029500020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
029600020117     D  PAA                  690    806P 5
029700000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
029800000000     D*---
029900020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
030000000000     D*---
030100020220     D ABCA13          DS                  OCCURS(5) INZ
030200951227     D  NTA                    1     12P 0
030300951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
030400951227     D  ATA                   13     24P 0
030500951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
030600000000     D*------------------
030700020220     D* DS MULTIPLA TOTALI FILIALE      - 5 CATEGORIE DI CLIENTI -
030800000000     D*------------------
030900020220     D ABCFIL          DS                  OCCURS(5) INZ
031000020117     D  CNF                    1     78P 0
031100020117     D                                     DIM(13)                              NUMERO C.NUOVI
031200020117     D  SNF                   79    169P 0
031300020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
031400020117     D  RNF                  170    286P 5
031500020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
031600020117     D  PNF                  287    403P 5
031700020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
031800020117     D  CAF                  404    481P 0
031900020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
032000020117     D  SAF                  482    572P 0
032100020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
032200020117     D  RAF                  573    689P 5
032300020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
032400020117     D  PAF                  690    806P 5
032500000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
032600000000     D*---
032700020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
032800000000     D*---
032900020220     D ABCF13          DS                  OCCURS(5) INZ
033000951227     D  NTF                    1     12P 0
033100951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
033200951227     D  ATF                   13     24P 0
033300951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
033400000000     D*------------------
033500020220     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 CATEGORIE DI CLIENTI -
033600000000     D*------------------
033700020220     D ABCCOM          DS                  OCCURS(5) INZ
033800020117     D  CNC                    1     78P 0
033900020117     D                                     DIM(13)                              NUMERO C.NUOVI
034000020117     D  SNC                   79    169P 0
034100020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
034200020117     D  RNC                  170    286P 5
034300020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
034400020117     D  PNC                  287    403P 5
034500020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
034600020117     D  CAC                  404    481P 0
034700020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
034800020117     D  SAC                  482    572P 0
034900020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
035000020117     D  RAC                  573    689P 5
035100020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
035200020117     D  PAC                  690    806P 5
035300000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
035400000000     D*---
035500020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
035600000000     D*---
035700020220     D ABCC13          DS                  OCCURS(5) INZ
035800951227     D  NTC                    1     12P 0
035900951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
036000951227     D  ATC                   13     24P 0
036100951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
036200000000     D*------------------
036300020220     D* DS MULTIPLA TOTALI STAMPA       - 5 CATEGORIE DI CLIENTI -
036400000000     D*------------------
036500020220     D ABCSTA          DS                  OCCURS(5) INZ
036600020117     D  CNS                    1     78P 0
036700020117     D                                     DIM(13)                              NUMERO C.NUOVI
036800020117     D  SNS                   79    169P 0
036900020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
037000020117     D  RNS                  170    286P 5
037100020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
037200020117     D  PNS                  287    403P 5
037300020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
037400020117     D  CAS                  404    481P 0
037500020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
037600020117     D  SAS                  482    572P 0
037700020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
037800020117     D  RAS                  573    689P 5
037900020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
038000020117     D  PAS                  690    806P 5
038100000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
038200000000     D*---
038300020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
038400000000     D*---
038500020220     D ABCS13          DS                  OCCURS(5) INZ
038600951227     D  NTS                    1     12P 0
038700951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
038800951227     D  ATS                   13     24P 0
038900951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
039000020318     D*------------------
039100020318     D* DS MULTIPLA TOTALI GENERALI     - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
039200020318     D*------------------
039300020318     D ABCGENN         DS                  OCCURS(5) INZ
039400020318     D  CNGN                   1     78P 0
039500020318     D                                     DIM(13)                              NUMERO C.NUOVI
039600020318     D  SNGN                  79    169P 0
039700020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
039800020318     D  RNGN                 170    286P 5
039900020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
040000020318     D  PNGN                 287    403P 5
040100020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
040200020318     D  CAGN                 404    481P 0
040300020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
040400020318     D  SAGN                 482    572P 0
040500020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
040600020318     D  RAGN                 573    689P 5
040700020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
040800020318     D  PAGN                 690    806P 5
040900020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
041000020318     D*---
041100020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
041200020318     D*---
041300020318     D ABCG13N         DS                  OCCURS(5) INZ
041400020318     D  NTGN                   1     12P 0
041500020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
041600020318     D  ATGN                  13     24P 0
041700020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
041800020318     D*------------------
041900020318     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
042000020318     D*------------------
042100020318     D ABCDIVN         DS                  OCCURS(5) INZ
042200020318     D  CNDN                   1     78P 0
042300020318     D                                     DIM(13)                              NUMERO C.NUOVI
042400020318     D  SNDN                  79    169P 0
042500020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
042600020318     D  RNDN                 170    286P 5
042700020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
042800020318     D  PNDN                 287    403P 5
042900020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
043000020318     D  CADN                 404    481P 0
043100020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
043200020318     D  SADN                 482    572P 0
043300020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
043400020318     D  RADN                 573    689P 5
043500020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
043600020318     D  PADN                 690    806P 5
043700020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
043800020318     D*---
043900020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
044000020318     D*---
044100020318     D ABCD13N         DS                  OCCURS(5) INZ
044200030924     D  NTDN                   1     12P 0
044300030924     D                                     DIM(2)                               TOTALE CLIENTI NEW
044400030924     D  ATDN                  13     24P 0
044500030924     D                                     DIM(2)                               TOTALE CLIENTI ACQ
044600030924     D*------------------
044700020318     D* DS MULTIPLA TOTALI AREA         - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
044800020318     D*------------------
044900020318     D ABCAREN         DS                  OCCURS(5) INZ
045000020318     D  CNAN                   1     78P 0
045100020318     D                                     DIM(13)                              NUMERO C.NUOVI
045200020318     D  SNAN                  79    169P 0
045300020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
045400020318     D  RNAN                 170    286P 5
045500020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
045600020318     D  PNAN                 287    403P 5
045700020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
045800020318     D  CAAN                 404    481P 0
045900020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
046000020318     D  SAAN                 482    572P 0
046100020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
046200020318     D  RAAN                 573    689P 5
046300020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
046400020318     D  PAAN                 690    806P 5
046500020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
046600020318     D*---
046700020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
046800020318     D*---
046900020318     D ABCA13N         DS                  OCCURS(5) INZ
047000020318     D  NTAN                   1     12P 0
047100020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
047200020318     D  ATAN                  13     24P 0
047300020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
047400020318     D*------------------
047500020318     D* DS MULTIPLA TOTALI FILIALE      - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
047600020318     D*------------------
047700020318     D ABCFILN         DS                  OCCURS(5) INZ
047800020318     D  CNFN                   1     78P 0
047900020318     D                                     DIM(13)                              NUMERO C.NUOVI
048000020318     D  SNFN                  79    169P 0
048100020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
048200020318     D  RNFN                 170    286P 5
048300020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
048400020318     D  PNFN                 287    403P 5
048500020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
048600020318     D  CAFN                 404    481P 0
048700020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
048800020318     D  SAFN                 482    572P 0
048900020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
049000020318     D  RAFN                 573    689P 5
049100020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
049200020318     D  PAFN                 690    806P 5
049300020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
049400020318     D*---
049500020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
049600020318     D*---
049700020318     D ABCF13N         DS                  OCCURS(5) INZ
049800020318     D  NTFN                   1     12P 0
049900020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
050000020318     D  ATFN                  13     24P 0
050100020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
050200020318     D*------------------
050300020318     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
050400020318     D*------------------
050500020318     D ABCCOMN         DS                  OCCURS(5) INZ
050600020318     D  CNCN                   1     78P 0
050700020318     D                                     DIM(13)                              NUMERO C.NUOVI
050800020318     D  SNCN                  79    169P 0
050900020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
051000020318     D  RNCN                 170    286P 5
051100020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
051200020318     D  PNCN                 287    403P 5
051300020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
051400020318     D  CACN                 404    481P 0
051500020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
051600020318     D  SACN                 482    572P 0
051700020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
051800020318     D  RACN                 573    689P 5
051900020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
052000020318     D  PACN                 690    806P 5
052100020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
052200020318     D*---
052300020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
052400020318     D*---
052500020318     D ABCC13N         DS                  OCCURS(5) INZ
052600020318     D  NTCN                   1     12P 0
052700020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
052800020318     D  ATCN                  13     24P 0
052900020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
053000020318     D*------------------
053100020318     D* DS MULTIPLA TOTALI STAMPA       - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
053200020318     D*------------------
053300020318     D ABCSTAN         DS                  OCCURS(5) INZ
053400020318     D  CNSN                   1     78P 0
053500020318     D                                     DIM(13)                              NUMERO C.NUOVI
053600020318     D  SNSN                  79    169P 0
053700020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
053800020318     D  RNSN                 170    286P 5
053900020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
054000020318     D  PNSN                 287    403P 5
054100020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
054200020318     D  CASN                 404    481P 0
054300020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
054400020318     D  SASN                 482    572P 0
054500020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
054600020318     D  RASN                 573    689P 5
054700020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
054800020318     D  PASN                 690    806P 5
054900020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
055000020318     D*---
055100020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
055200020318     D*---
055300020318     D ABCS13N         DS                  OCCURS(5) INZ
055400020318     D  NTSN                   1     12P 0
055500020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
055600020318     D  ATSN                  13     24P 0
055700020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
055800020318     D*------------------
055900020318     D* DS MULTIPLA TOTALI GENERALI     - 5 NETWORK X CATEGORIE CLIENTI "D" -
056000020318     D*------------------
056100020318     D ABCGENND        DS                  OCCURS(5) INZ
056200020318     D  CNGND                  1     78P 0
056300020318     D                                     DIM(13)                              NUMERO C.NUOVI
056400020318     D  SNGND                 79    169P 0
056500020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
056600020318     D  RNGND                170    286P 5
056700020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
056800020318     D  PNGND                287    403P 5
056900020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
057000020318     D  CAGND                404    481P 0
057100020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
057200020318     D  SAGND                482    572P 0
057300020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
057400020318     D  RAGND                573    689P 5
057500020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
057600020318     D  PAGND                690    806P 5
057700020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
057800020318     D*---
057900020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
058000020318     D*---
058100020318     D ABCG13ND        DS                  OCCURS(5) INZ
058200020318     D  NTGND                  1     12P 0
058300020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
058400020318     D  ATGND                 13     24P 0
058500020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
058600020318     D*------------------
058700020318     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
058800020318     D*------------------
058900020318     D ABCDIVND        DS                  OCCURS(5) INZ
059000020318     D  CNDND                  1     78P 0
059100020318     D                                     DIM(13)                              NUMERO C.NUOVI
059200020318     D  SNDND                 79    169P 0
059300020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
059400020318     D  RNDND                170    286P 5
059500020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
059600020318     D  PNDND                287    403P 5
059700020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
059800020318     D  CADND                404    481P 0
059900020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
060000020318     D  SADND                482    572P 0
060100020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
060200020318     D  RADND                573    689P 5
060300020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
060400020318     D  PADND                690    806P 5
060500020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
060600020318     D*---
060700020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
060800020318     D*---
060900020318     D ABCD13ND        DS                  OCCURS(5) INZ
061000020318     D  NTDND                  1     12P 0
061100020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
061200020318     D  ATDND                 13     24P 0
061300020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
061400020318     D*------------------
061500020318     D* DS MULTIPLA TOTALI AREA         - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
061600020318     D*------------------
061700020318     D ABCAREND        DS                  OCCURS(5) INZ
061800020318     D  CNAND                  1     78P 0
061900020318     D                                     DIM(13)                              NUMERO C.NUOVI
062000020318     D  SNAND                 79    169P 0
062100020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
062200020318     D  RNAND                170    286P 5
062300020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
062400020318     D  PNAND                287    403P 5
062500020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
062600020318     D  CAAND                404    481P 0
062700020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
062800020318     D  SAAND                482    572P 0
062900020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
063000020318     D  RAAND                573    689P 5
063100020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
063200020318     D  PAAND                690    806P 5
063300020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
063400020318     D*---
063500020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
063600020318     D*---
063700020318     D ABCA13ND        DS                  OCCURS(5) INZ
063800020318     D  NTAND                  1     12P 0
063900020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
064000020318     D  ATAND                 13     24P 0
064100020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
064200020318     D*------------------
064300020318     D* DS MULTIPLA TOTALI FILIALE      - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
064400020318     D*------------------
064500020318     D ABCFILND        DS                  OCCURS(5) INZ
064600020318     D  CNFND                  1     78P 0
064700020318     D                                     DIM(13)                              NUMERO C.NUOVI
064800020318     D  SNFND                 79    169P 0
064900020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
065000020318     D  RNFND                170    286P 5
065100020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
065200020318     D  PNFND                287    403P 5
065300020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
065400020318     D  CAFND                404    481P 0
065500020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
065600020318     D  SAFND                482    572P 0
065700020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
065800020318     D  RAFND                573    689P 5
065900020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
066000020318     D  PAFND                690    806P 5
066100020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
066200020318     D*---
066300030924     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
066400030924     D*---
066500030924     D ABCF13ND        DS                  OCCURS(5) INZ
066600030924     D  NTFND                  1     12P 0
066700020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
066800020318     D  ATFND                 13     24P 0
066900020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
067000020318     D*------------------
067100020318     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
067200020318     D*------------------
067300020318     D ABCCOMND        DS                  OCCURS(5) INZ
067400020318     D  CNCND                  1     78P 0
067500020318     D                                     DIM(13)                              NUMERO C.NUOVI
067600020318     D  SNCND                 79    169P 0
067700020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
067800020318     D  RNCND                170    286P 5
067900020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
068000020318     D  PNCND                287    403P 5
068100020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
068200020318     D  CACND                404    481P 0
068300020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
068400020318     D  SACND                482    572P 0
068500020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
068600020318     D  RACND                573    689P 5
068700020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
068800020318     D  PACND                690    806P 5
068900020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
069000020318     D*---
069100020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
069200020318     D*---
069300020318     D ABCC13ND        DS                  OCCURS(5) INZ
069400020318     D  NTCND                  1     12P 0
069500020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
069600020318     D  ATCND                 13     24P 0
069700020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
069800020318     D*------------------
069900020318     D* DS MULTIPLA TOTALI STAMPA       - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
070000020318     D*------------------
070100020318     D ABCSTAND        DS                  OCCURS(5) INZ
070200020318     D  CNSND                  1     78P 0
070300020318     D                                     DIM(13)                              NUMERO C.NUOVI
070400020318     D  SNSND                 79    169P 0
070500020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
070600020318     D  RNSND                170    286P 5
070700020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
070800020318     D  PNSND                287    403P 5
070900020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
071000020318     D  CASND                404    481P 0
071100020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
071200020318     D  SASND                482    572P 0
071300020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
071400020318     D  RASND                573    689P 5
071500020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
071600020318     D  PASND                690    806P 5
071700020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
071800020318     D*---
071900020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
072000020318     D*---
072100020318     D ABCS13ND        DS                  OCCURS(5) INZ
072200020318     D  NTSND                  1     12P 0
072300020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
072400020318     D  ATSND                 13     24P 0
072500020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
072600000828     D*------------------
072700000828     D* CLIENTI FIGLI IN STAMPA
072800000828     D*------------------
072900051031     C**** ridotta da 19 a 17 per far spazio ai flags x individuare i codici bloccati
073000051031     D***** SKSC            S              7  0 DIM(19)                              *CLIENTE
073100051031     D SKSC            S              7  0 DIM(17)                              *CLIENTE
073200051031     D SKbl            S              1    DIM(17)                              *Bloccato
073300120125      *---
073400120125     D*------------------
073500120125     D* DS per reperire la DATA del BLOCCO CONTABILE
073600120125     D*------------------
073700120125     D TRULBLCDS     E DS
073800120125      *---
073900120125     D Cod_Blocco_Ultimo...
074000120125     d                 s              3a
074100120125     D Data_Blocco_Ultimo...
074200120125     d                 s              8  0
074300120125     D Data_Prima_Fat_6...
074400120125     d                 s              6  0
074500120131     D Data_Prima_Fat_8...
074600120131     d                 s              8  0
074700120131     D CLP_Prima_Fat_8...
074800120131     d                 s              8  0
074900000000     D*------------------
075000000000     D* PARAMETRI FILTRO
075100000000     D*------------------
075200000000     D PARAM           DS
075300990609     D  PARDAT                 1      6  0
075400990609     D  PARAAI                 1      4  0
075500990609     D  PARMMI                 5      6  0
075600990609     D  PARDA2                 7     12  0
075700990609     D  PARAAF                 7     10  0
075800990609     D  PARMMF                11     12  0
075900990609     D  PARDCK                13     13
076000990609     D  PARDCC                14     14
076100990609     D  PARDCF                15     15
076200990609     D  PARFIC                16     18  0
076300990609     D  PARARC                19     21  0
076400990609     D  PARDIC                22     22
076500990609     D  PARDFF                23     23
076600990609     D  PARCOF                24     30  0
076700990609     D  PARDIF                31     31
076800990609     D  PARSTA                32     32
076900000825     D  PARSCE                33     33
077000990609     D  PARTSP                34     34
077100990609     D  PARIOE                35     35
077200990609     D  PARCLV                36     36
077300990609     D  PARDPT                37     42  0
077400990609     D  PARAPI                37     40  0
077500990609     D  PARMPI                41     42  0
077600990609     D  PARDP2                43     48  0
077700990609     D  PARAFP                43     46  0
077800990609     D  PARMFP                47     48  0
077900990609     D  PARSPD                49     52  1
078000990609     D  PARPDD                53     56  1
078100990609     D  PARPDA                57     60  1
078200000707     D  PARPOS                61     61
078300000825     D  PARCRE                62     62
078400010618     D  PARCL1                63     63
078500010618     D  PARCL2                64     64
078600010618     D  PARSIN                65     65
078700020404     D  PARCL3                66     66
078800020404     D  PARCL4                67     67
078900020404     D  PARNTW                68     68
079000020701     D  PARDFD                69     72  1
079100020701     D  PARDFA                73     76  1
079200020701     D  PARDFDP               77     80  1
079300020701     D  PARDFAP               81     84  1
079400030311     D  PARFPC                85     85
079500030925     D  PARWPC                86     86
079600030925     D  PARWCP                87     87
079700100129     D  PARWfdpv              88     88
079800100129      * Stat. Clienti Persi (Trimestrale x il momento)                          lancio trimestre(T)
079900100129     D  PARCLPtmp             89     89                                         M/B/T/Q/S
080000100129     D  PARStampa             90     90                                         *blk/N
080100100129     D  PARStaComm            91     91                                         *blk/N
080200000707     D*------------------
080300000707     D* DS LETTURA CAMPO ORGDE3 DI AZORG00F
080400000707     D*------------------
080500000707     D OG143         E DS
080600000000     D*------------------
080700000000     D* DS AGENTI COMMERCIALI
080800000000     D*------------------
080900000000     D DS01          E DS
081000000000     D*------------------
081100000000     D* DS SETTORI MERCEOLOGICI
081200000000     D*------------------
081300951006     D DS1L          E DS
081400090610     D*------------------
081500090610     D* Descrizioni tipi servizio
081600090610     D*------------------
081700090610     D DS5e          E DS
081800100129     D Quanti_Tserv    S              3s 0
081900100129     D Tserv           S              1    DIM(10)
082000100129     D DTserv          S             25    DIM(10)
082100000828     D*------------------
082200000828     D* DS DI PROCEDURA
082300000828     D*------------------
082400000828     D BS10DS        E DS                  EXTNAME(TIBS10DS)
082500000828     D  SKSC11                21   5520  0 DIM(500)                             *CLIENTI FIGLI
082600020220     D*------------------
082700020220     D* DS IMPORTANZA CLIENTI
082800020220     D*------------------
082900020220     D DSIC          E DS
083000980406     D*------------------
083100980406     D* DS REPERIMENTO DATI UTENTE
083200980406     D*-------------------
083300980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
083400980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
083500980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
083600980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
083700980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
083800951219     D*------------------
083900951219     D* COMPOSIZIONE DATA SALDI
084000951219     D*------------------
084100951219     D                 DS
084200951219     D  WFDANN                 1      4  0                                      *ANNO
084300951219     D  WFDMES                 5      6  0                                      *MESE
084400951219     D  WFDDAT                 1      6  0                                      *DATA
084500000828     D*------------------
084600000828     D* DS CONTROLLO DATA (8)
084700000828     D*------------------
084800000828     D WLBDA8          DS                  INZ
084900000828     D  G08DAT                 1      8  0
085000000828     D  G08INV                 9     16  0
085100000828     D  G08ERR                17     17
085200000828     D  G08TGI                18     22  0
085300020319     D*------------------
085400020319     D* DS DI RIEMPIMENTO SCHIERA DI WRK WSK1
085500020319     D*------------------
085600020319     D DSWSK1          DS                  INZ
085700020319     D  WFDCLI                        7  0
085800020319     D  MM                            2  0
085900020319     D  WFDNTW                        3
086000000000     C*------------------------------------------------------------------------*
086100000000     C*?MAIN LINE
086200000000     C*------------------------------------------------------------------------*
086300000000     C*
086400000000     C* STAMPA PARAMETRI FILTRO
086500000000     C                   EXSR      STAPAR
086600000000     C*
086700000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
086800000000     C                   EXSR      SETFIL
086900000000     C*
087000000000     C* CICLO FINO A FINE FILE DI LAVORO
087100000000DO  1C     $FINE         DOWEQ     '0'
087200000000     C*
087300000000     C* OPERAZIONI PER NUOVA DIVISIONE
087400000000     C                   EXSR      NEWDIV
087500000000     C*
087600000000     C* CICLO FINO A ROTTURA DI DIVISIONE
087700000000DO  2C     $FINE         DOWEQ     '0'
087800000000     C     WFDDIV        ANDEQ     DEPDIV
087900000000     C*
088000000000     C* OPERAZIONI PER NUOVA AREA
088100000000     C                   EXSR      NEWARE
088200000000     C*
088300000000     C* CICLO FINO A ROTTURA DI AREA
088400000000DO  3C     $FINE         DOWEQ     '0'
088500000000     C     WFDDIV        ANDEQ     DEPDIV
088600000000     C     WFDARE        ANDEQ     DEPARE
088700000000     C*
088800000000     C* OPERAZIONI PER NUOVA FILIALE
088900000000     C                   EXSR      NEWFIL
089000000000     C*
089100000000     C* CICLO FINO A ROTTURA DI FILIALE
089200000000DO  4C     $FINE         DOWEQ     '0'
089300000000     C     WFDDIV        ANDEQ     DEPDIV
089400000000     C     WFDARE        ANDEQ     DEPARE
089500000000     C     WFDFIL        ANDEQ     DEPFIL
089600000000     C*
089700000000     C* OPERAZIONI PER NUOVO AGENTE
089800000000     C                   EXSR      NEWAGE
089900000000     C*
090000000000     C* CICLO FINO A ROTTURA DI AGENTE
090100000000DO  5C     $FINE         DOWEQ     '0'
090200000000     C     WFDDIV        ANDEQ     DEPDIV
090300000000     C     WFDARE        ANDEQ     DEPARE
090400000000     C     WFDFIL        ANDEQ     DEPFIL
090500000000     C     WFDAGE        ANDEQ     DEPAGE
090600000000     C*
090700120131     c* OPERAZIONI PER NUOVO CLIENTE
090800000000     C                   EXSR      NEWKSC
090900000000     C*
091000000000     C* CICLO FINO A ROTTURA DI CLIENTE
091100000000DO  6C     $FINE         DOWEQ     '0'
091200000000     C     WFDDIV        ANDEQ     DEPDIV
091300000000     C     WFDARE        ANDEQ     DEPARE
091400000000     C     WFDFIL        ANDEQ     DEPFIL
091500000000     C     WFDAGE        ANDEQ     DEPAGE
091600030924     C     WFDCLI        ANDEQ     DEPCLI
091700030924     C*
091800030924     C* OPERAZIONI PER PASSAGGIO LETTURA SALDI PRECEDENTI --> SALDI CORRENTI
091900030924     C                   EXSR      NEWPPC
092000030924     C* MEMORIZZA I SALDI
092100030924     C                   EXSR      MEMSAL
092200000000     C*
092300000000     C* LEGGE IL RECORD SUCCESSIVO
092400000000     C                   EXSR      LETFIL
092500000000     C*
092600000000E   6C                   ENDDO
092700000000     C*
092800000000     C* STAMPA IL TOTALE CLIENTE
092900000000     C                   EXSR      STAKSC
093000000000E   5C                   ENDDO
093100000000     C*
093200000000     C* STAMPA IL TOTALE COMMERCIALE
093300000000     C                   EXSR      STAAGE
093400000000E   4C                   ENDDO
093500000000     C*
093600000000     C* STAMPA IL TOTALE FILIALE
093700000000     C                   EXSR      STAFIL
093800000000E   3C                   ENDDO
093900000000     C*
094000000000     C* STAMPA IL TOTALE AREA
094100000000     C                   EXSR      STAARE
094200000000E   2C                   ENDDO
094300000000     C*
094400000000     C* STAMPA IL TOTALE DIVISIONE
094500000000     C                   EXSR      STADIV
094600000000E   1C                   ENDDO
094700000000     C*
094800000000     C* STAMPA IL TOTALE GENERALE
094900000000     C                   EXSR      STAGEN
095000000000     C*
095100000000     C* STAMPA RIGA FINE STAMPA
095200100129     c                   if        parSTAMPA <> 'N'
095300030311     C                   WRITE     SE94LE
095400020116     C                   WRITE     SE94FI
095500100129     c                   end
095600980710     C* OPERAZIONI FINALI
095700980710     C                   EXSR      FINSR
095800000000     C*
095900000000     C                   SETON                                        LR
096000000000     C*------------------------------------------------------------------------*
096100000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
096200000000     C*------------------------------------------------------------------------*
096300000000     C     SETFIL        BEGSR
096400980529     C*
096500000000     C                   MOVEL     *BLANKS       KWDDIV                         *DIVISIONE
096600000000     C                   Z-ADD     *ZEROS        KWDARE                         *AREA
096700000000     C                   Z-ADD     *ZEROS        KWDFIL                         *FILIALE
096800000000     C                   Z-ADD     *ZEROS        KWDAGE                         *COMMERCIALE
096900960710     C                   MOVEL     *BLANKS       KWDCLV                         *CODICE QUALITA'
097000000825     C                   Z-ADD     *ZEROS        KWDCLI                         *CLIENTE
097100000000     C                   Z-ADD     *ZEROS        KWDANN                         *ANNO
097200000000     C                   Z-ADD     *ZEROS        KWDMES                         *MESE
097300100127     C     KEYDEL        SETLL     WFDEA01L                           99        *FINE ARCHIVIO
097400951219IF  1C     *IN99         IFEQ      *ON
097500000000     C                   MOVEL     '1'           $FINE
097600000000X   1C                   ELSE
097700000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
097800000000E   1C                   ENDIF
097900000000     C*
098000000000     C                   ENDSR
098100000000     C*------------------------------------------------------------------------*
098200000000     C*?LETFIL - LETTURA PROSSIMO RECORD DEL WORK FILE
098300000000     C*------------------------------------------------------------------------*
098400000000     C     LETFIL        BEGSR
098500000000     C*
098600000000     C* LEGGO FINO A:
098700000000     C*     - FINE FILE
098800000000     C*     - TROVATO RECORD VALIDO
098900000000     C                   MOVEL     'N'           WRECOK
099000000000DO  1C     $FINE         DOWEQ     '0'
099100000000     C     WRECOK        ANDEQ     'N'
099200000000     C*
099300000000     C* LETTURE SUCCESSIVE ALLA PRIMA
099400100127     C                   READ      WFDEA01L                               99
099500000000     C*
099600951219IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
099700000000     C                   MOVEL     '1'           $FINE
099800000000X   2C                   ELSE
099900000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
100000000000E   2C                   ENDIF
100100000000     C*
100200000000E   1C                   ENDDO
100300000000     C*
100400000000     C                   ENDSR
100500000000     C*------------------------------------------------------------------------*
100600000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD DEL WORK FILE
100700000000     C*------------------------------------------------------------------------*
100800000000     C     CHKREC        BEGSR
100900000000     C*
101000000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
101100000000     C*
101200990609IF  1C     WFDDAT        IFGE      PARDPT
101300990609     C     WFDDAT        ANDLE     PARDP2
101400951219     C                   MOVEL     'P'           TIPANN                         *PERIODO PRECEDENTE
101500071123     C*
101600071123     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
101700071123     C* a cambio di codice cliente. Essendo comunuque il periodo Precedente deve
101800071123     C* essere considerato ACQUISITO E NON NUOVO e SE LO E' RIMANE tale anche
101900071123     C* PER TUTTO IL PERIODO CORRENTE
102000071123IF  2C                   If        FlaPAS = '0'
102100071123     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
102200071123     C                   clear                   DESCLN                         *NUOVO O NON NUOVO
102300071123E   2C                   EndIF
102400071123      *
102500951219E   1C                   ENDIF
102600071123      *
102700990609IF  1C     WFDDAT        IFGE      PARDAT
102800990609     C     WFDDAT        ANDLE     PARDA2
102900951219     C                   MOVEL     'C'           TIPANN                         *PERIODO CORRENTE
103000000000     C*
103100000000     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
103200000000     C* VEDO SE IL CLIENTE E' NUOVO (PER FORZA IL PRIMO SALDO); SE LO E' RIMANE
103300000000     C* TALE PER TUTTO IL PERIODO CORRENTE
103400071123     C* Attenzione: poichè se il codice cliente precedentemente letto aveva solo
103500071123     C*  solo dati su Anno precedente e il cliente appena letto è nuovo per anno
103600071123     C*  in corso sovrappone il Nuovo/Acquisito su DESCLN poichè si ritrovava il FLAPAS = '0'
103700071123     C*  per proteggersi da tale problema occorre controllare che il test venga fatto
103800071123     C*  quando non si è alivello di rottura livello cliente.
103900000000IF  2C     FLAPAS        IFEQ      '0'
104000071123     c                   if        wfdCLI = depCLI
104100000000     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
104200000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O NON NUOVO
104300071123     c                   end
104400000000E   2C                   ENDIF
104500070822      *
104600070822      *  Può esistere il caso che il cliente sia nuovo nel mese di febbraio x ESTERO
104700070822      *  ed abbia fatturato Italia solo in giugno.
104800070822      *  Lanciando la stampa come Italia l'informazione letta, ossia il record di giugno, non
104900070822      *  avrà "Y" come NUOVO ma avrà "S".
105000070822      *  Il programma necessita della "Y" nei suoi test per considerare il cliente come NUOVO.
105100070822      *   --> Occorre memorizzarsi qui comunque cosa vale il flag del primo record letto sia
105200070822      *   che sia Italia/Estero per poi forzarlo nella routine di decodifica del cliente.
105300070822IF  2C     wfdCLI        ifne      sav_depCLI
105400070822IF  2C     wfdIOE        andne     parIOE
105500070822IF  2C     parIOE        andne     *blank
105600070822      *
105700070822      * lo salva per poterlo forzare in seguito
105800070822     c                   clear                   sav_desCLN_Y
105900070822IF  2C                   if        sav_depCLI <> wfdCLI
106000070822IF  2C                                       and wfdCLN ='Y'
106100070822     c                   move      'Y'           sav_desCLN_Y
106200070822     c                   end
106300070822      *
106400070822     c                   eval      sav_depCLI  =  wfdCLI
106500070822     c                   end
106600070822      *
106700000000E   1C                   ENDIF
106800070822      *
106900951018     C*--------------------
107000951018     C* SELEZIONI
107100951018     C*--------------------
107200951018     C* SE FILIALE CLIENTE <> FILIALE RICHIESTA, ESCLUDO
107300951018IF  1C     PARFIC        IFGT      *ZEROS                                       *DELTA FILIALE
107400951018     C     WFDFIL        ANDNE     PARFIC
107500951018     C                   MOVEL     'N'           WRECOK
107600951018E   1C                   ENDIF
107700951018     C*
107800951018     C* SE AREA CLIENTE <> AREA RICHIESTA, ESCLUDO
107900951018IF  1C     PARARC        IFGT      *ZEROS                                       *DELTA FILIALE
108000951018     C     WFDARE        ANDNE     PARARC
108100951018     C                   MOVEL     'N'           WRECOK
108200951018E   1C                   ENDIF
108300951018     C*
108400030924     C* SE DIVISIONE CLIENTE <> DIVISIONE RICHIESTA, ESCLUDO
108500951018IF  1C     PARDIC        IFNE      *BLANKS                                      *DELTA FILIALE
108600030924     C     WFDDIV        ANDNE     PARDIC
108700030924     C                   MOVEL     'N'           WRECOK
108800030924E   1C                   ENDIF
108900030924     C*
109000951018     C* SE CODICE QUALITA' CLIENTE <> CODICE QUALITA' RICHIESTO, ESCLUDO
109100010618IF  1C                   IF        (PARCLV <> *blanks and WFDCLV = PARCLV) or
109200010618     C                             (PARCL1 <> *blanks and WFDCLV = PARCL1) or
109300030924     C                             (PARCL2 <> *blanks and WFDCLV = PARCL2) or
109400030924     C                             (PARCLV = *blanks  and
109500030924     C                              PARCL1 = *blanks  and
109600030924     C                              PARCL2 = *blanks)
109700030924     C                   ELSE
109800951018     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
109900951018E   1C                   ENDIF
110000000000     C*
110100951018     C* SE TIPO DATI STATISTCA ITALIA/ESTERO <> TIPO DATI RICHIESTO, ESCLUDO
110200950210IF  1C     PARIOE        IFNE      *BLANKS
110300950210     C     PARIOE        ANDNE     WFDIOE
110400950208     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
110500950210E   1C                   ENDIF
110600000707     C*
110700000707     C* SE DATI NO POSTE ESCLUDE LE FILIALI POSTE, E VICEVERSA
110800000707IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
110900000707     C                   SETOFF                                           28
111000000707     C                   Z-ADD     1             I                 5 0
111100000707     C     WFDFIL        LOOKUP    FIL(I)                                 28
111200000707IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
111300020627IF  3C     POS(I)        IFEQ      'PPT'                                         FIL = POSTE
111400000707     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
111500000711E   2C                   ENDIF
111600000711X   2C                   ELSE                                                   *SI DATI POSTE
111700020627IF  3C     POS(I)        IFNE      'PPT'                                         FIL <> POSTE
111800000707     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
111900000711E   3C                   ENDIF
112000000711E   2C                   ENDIF
112100000707E   1C                   ENDIF
112200980528     C*
112300000000     C                   ENDSR
112400000000     C*------------------------------------------------------------------------*
112500000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
112600000000     C*------------------------------------------------------------------------*
112700000000     C     NEWDIV        BEGSR
112800020220     C*
112900020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
113000020220     C                   MOVEL     '0'           FLGSIDD
113100000000     C*
113200000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
113300000000     C                   MOVEL     '0'           PASDIV
113400000000     C*
113500000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
113600000000     C                   MOVEL     WFDDIV        DEPDIV
113700000000     C*
113800000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
113900000000     C                   EXSR      DECDIV
114000000000     C*
114100020318     C* AZZERA I TOTALI DELLA DIVISIONE (X QUALITA' CLIENTE)
114200020221DO  1C     1             DO        5             Q
114300000000     C     Q             OCCUR     ABCDIV
114400000000     C                   CLEAR                   CND
114500000000     C                   CLEAR                   SND
114600000000     C                   CLEAR                   RND
114700000000     C                   CLEAR                   PND
114800000000     C                   CLEAR                   CAD
114900000000     C                   CLEAR                   SAD
115000000000     C                   CLEAR                   RAD
115100000000     C                   CLEAR                   PAD
115200000000E   1C                   ENDDO
115300000000     C*
115400000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
115500020221DO  1C     1             DO        5             Q
115600000000     C     Q             OCCUR     ABCD13
115700000000     C                   CLEAR                   NTD
115800000000     C                   CLEAR                   ATD
115900000000E   1C                   ENDDO
116000020318     C*
116100020318     C* AZZERA I TOTALI DELLA DIVISIONE (X NETWORK) NN "D"
116200020318DO  1C     1             DO        5             K
116300020318     C     K             OCCUR     ABCDIVN
116400020318     C                   CLEAR                   CNDN
116500020318     C                   CLEAR                   SNDN
116600020318     C                   CLEAR                   RNDN
116700020318     C                   CLEAR                   PNDN
116800020318     C                   CLEAR                   CADN
116900020318     C                   CLEAR                   SADN
117000020318     C                   CLEAR                   RADN
117100020318     C                   CLEAR                   PADN
117200020318E   1C                   ENDDO
117300020318     C*
117400020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
117500020318DO  1C     1             DO        5             K
117600020318     C     K             OCCUR     ABCD13N
117700020318     C                   CLEAR                   NTDN
117800020318     C                   CLEAR                   ATDN
117900020318E   1C                   ENDDO
118000020318     C*
118100020318     C* AZZERA I TOTALI DELLA DIVISIONE (X NETWORK) "D"
118200020318DO  1C     1             DO        5             K
118300020318     C     K             OCCUR     ABCDIVND
118400020318     C                   CLEAR                   CNDND
118500020318     C                   CLEAR                   SNDND
118600020318     C                   CLEAR                   RNDND
118700020318     C                   CLEAR                   PNDND
118800020318     C                   CLEAR                   CADND
118900020318     C                   CLEAR                   SADND
119000020318     C                   CLEAR                   RADND
119100020318     C                   CLEAR                   PADND
119200020318E   1C                   ENDDO
119300020318     C*
119400020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
119500020318DO  1C     1             DO        5             K
119600020318     C     K             OCCUR     ABCD13ND
119700020318     C                   CLEAR                   NTDND
119800020318     C                   CLEAR                   ATDND
119900020318E   1C                   ENDDO
120000000000     C*
120100000000     C                   ENDSR
120200000000     C*------------------------------------------------------------------------*
120300000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
120400000000     C*------------------------------------------------------------------------*
120500000000     C     NEWARE        BEGSR
120600020220     C*
120700020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
120800020220     C                   MOVEL     '0'           FLGSIDA
120900000000     C*
121000000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
121100000000     C                   MOVEL     '0'           PASARE
121200000000     C*
121300000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
121400000000     C                   MOVEL     WFDARE        DEPARE
121500000000     C*
121600000000     C* DECODIFICA IL CODICE AREA
121700000000     C                   EXSR      DECARE
121800000000     C*
121900020318     C* AZZERA I TOTALI DELL'AREA (X QUALITA' CLIENTE)
122000020221DO  1C     1             DO        5             Q
122100000000     C     Q             OCCUR     ABCARE
122200000000     C                   CLEAR                   CNA
122300000000     C                   CLEAR                   SNA
122400000000     C                   CLEAR                   RNA
122500000000     C                   CLEAR                   PNA
122600000000     C                   CLEAR                   CAA
122700000000     C                   CLEAR                   SAA
122800000000     C                   CLEAR                   RAA
122900000000     C                   CLEAR                   PAA
123000000000E   1C                   ENDDO
123100000000     C*
123200000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
123300020221DO  1C     1             DO        5             Q
123400000000     C     Q             OCCUR     ABCA13
123500000000     C                   CLEAR                   NTA
123600000000     C                   CLEAR                   ATA
123700000000E   1C                   ENDDO
123800020318     C*
123900020318     C* AZZERA I TOTALI DELL'AREA (X NETWORK) NN "D"
124000020318DO  1C     1             DO        5             K
124100020318     C     K             OCCUR     ABCAREN
124200020318     C                   CLEAR                   CNAN
124300020318     C                   CLEAR                   SNAN
124400020318     C                   CLEAR                   RNAN
124500020318     C                   CLEAR                   PNAN
124600020318     C                   CLEAR                   CAAN
124700020318     C                   CLEAR                   SAAN
124800020318     C                   CLEAR                   RAAN
124900020318     C                   CLEAR                   PAAN
125000020318E   1C                   ENDDO
125100020318     C*
125200020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
125300020318DO  1C     1             DO        5             K
125400020318     C     K             OCCUR     ABCA13N
125500020318     C                   CLEAR                   NTAN
125600020318     C                   CLEAR                   ATAN
125700020318E   1C                   ENDDO
125800020318     C*
125900020318     C* AZZERA I TOTALI DELL'AREA (X NETWORK) "D"
126000020318DO  1C     1             DO        5             K
126100020318     C     K             OCCUR     ABCAREND
126200020318     C                   CLEAR                   CNAND
126300020318     C                   CLEAR                   SNAND
126400020318     C                   CLEAR                   RNAND
126500020318     C                   CLEAR                   PNAND
126600020318     C                   CLEAR                   CAAND
126700020318     C                   CLEAR                   SAAND
126800020318     C                   CLEAR                   RAAND
126900020318     C                   CLEAR                   PAAND
127000020318E   1C                   ENDDO
127100020318     C*
127200020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
127300020318DO  1C     1             DO        5             K
127400020318     C     K             OCCUR     ABCA13ND
127500020318     C                   CLEAR                   NTAND
127600020318     C                   CLEAR                   ATAND
127700020318E   1C                   ENDDO
127800000000     C*
127900000000     C                   ENDSR
128000000000     C*------------------------------------------------------------------------*
128100000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
128200000000     C*------------------------------------------------------------------------*
128300000000     C     NEWFIL        BEGSR
128400020220     C*
128500020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
128600020220     C                   MOVEL     '0'           FLGSIDF
128700000000     C*
128800000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
128900000000     C                   MOVEL     '0'           PASFIL
129000000000     C*
129100000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
129200000000     C                   Z-ADD     WFDFIL        DEPFIL
129300000000     C*
129400000000     C* DECODIFICA IL CODICE FILIALE
129500000000     C                   EXSR      DECFIL
129600000000     C*
129700020318     C* AZZERA I TOTALI DELLA FILIALE (X QUALITA' CLIENTE)
129800020221DO  1C     1             DO        5             Q
129900000000     C     Q             OCCUR     ABCFIL
130000000000     C                   CLEAR                   CNF
130100000000     C                   CLEAR                   SNF
130200000000     C                   CLEAR                   RNF
130300000000     C                   CLEAR                   PNF
130400000000     C                   CLEAR                   CAF
130500000000     C                   CLEAR                   SAF
130600000000     C                   CLEAR                   RAF
130700000000     C                   CLEAR                   PAF
130800000000E   1C                   ENDDO
130900000000     C*
131000000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
131100020221DO  1C     1             DO        5             Q
131200000000     C     Q             OCCUR     ABCF13
131300000000     C                   CLEAR                   NTF
131400000000     C                   CLEAR                   ATF
131500000000E   1C                   ENDDO
131600020318     C*
131700020318     C* AZZERA I TOTALI DELLA FILIALE (X NETWORK) NN "D"
131800020318DO  1C     1             DO        5             K
131900020318     C     K             OCCUR     ABCFILN
132000020318     C                   CLEAR                   CNFN
132100020318     C                   CLEAR                   SNFN
132200020318     C                   CLEAR                   RNFN
132300020318     C                   CLEAR                   PNFN
132400020318     C                   CLEAR                   CAFN
132500020318     C                   CLEAR                   SAFN
132600020318     C                   CLEAR                   RAFN
132700020318     C                   CLEAR                   PAFN
132800020318E   1C                   ENDDO
132900020318     C*
133000020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
133100020318DO  1C     1             DO        5             K
133200020318     C     K             OCCUR     ABCF13N
133300020318     C                   CLEAR                   NTFN
133400020318     C                   CLEAR                   ATFN
133500020318E   1C                   ENDDO
133600020318     C*
133700020318     C* AZZERA I TOTALI DELLA FILIALE (X NETWORK) "D"
133800020318DO  1C     1             DO        5             K
133900020318     C     K             OCCUR     ABCFILND
134000020318     C                   CLEAR                   CNFND
134100020318     C                   CLEAR                   SNFND
134200020318     C                   CLEAR                   RNFND
134300020318     C                   CLEAR                   PNFND
134400020318     C                   CLEAR                   CAFND
134500020318     C                   CLEAR                   SAFND
134600020318     C                   CLEAR                   RAFND
134700020318     C                   CLEAR                   PAFND
134800020318E   1C                   ENDDO
134900020318     C*
135000020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
135100020318DO  1C     1             DO        5             K
135200020318     C     K             OCCUR     ABCF13ND
135300020318     C                   CLEAR                   NTFND
135400020318     C                   CLEAR                   ATFND
135500020318E   1C                   ENDDO
135600000000     C*
135700000000     C                   ENDSR
135800000000     C*------------------------------------------------------------------------*
135900000000     C*?NEWAGE - OPERAZIONI PER NUOVO AGENTE COMMERCIALE
136000000000     C*------------------------------------------------------------------------*
136100000000     C     NEWAGE        BEGSR
136200020220     C*
136300030924     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
136400030924     C                   MOVEL     '0'           FLGSIDE
136500030924     C*
136600030924     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
136700030924     C                   MOVEL     '0'           PASAGE
136800030924     C*
136900030924     C* MEMORIZZO IL NUOVO AGENTE IN UN DEPOSITO
137000030924     C                   Z-ADD     WFDAGE        DEPAGE
137100081013     C*
137200081013     C* l'Agente Unificante e la sua descrizione
137300081013     C                   Z-ADD     WFDrgf        DEPrgf
137400081013     C                   movel     WFDrgd        DEPrgd
137500081013     C*
137600000000     C* DECODIFICO IL CODICE AGENTE
137700000000     C                   EXSR      DECAGE
137800000000     C*
137900020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X QUALITA' CLIENTE)
138000020221DO  1C     1             DO        5             Q
138100000000     C     Q             OCCUR     ABCCOM
138200000000     C                   CLEAR                   CNC
138300000000     C                   CLEAR                   SNC
138400000000     C                   CLEAR                   RNC
138500000000     C                   CLEAR                   PNC
138600000000     C                   CLEAR                   CAC
138700000000     C                   CLEAR                   SAC
138800000000     C                   CLEAR                   RAC
138900000000     C                   CLEAR                   PAC
139000000000E   1C                   ENDDO
139100000000     C*
139200000000     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
139300020221DO  1C     1             DO        5             Q
139400000000     C     Q             OCCUR     ABCC13
139500000000     C                   CLEAR                   NTC
139600000000     C                   CLEAR                   ATC
139700000000E   1C                   ENDDO
139800020318     C*
139900020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X NETWORK) NN "D"
140000020318DO  1C     1             DO        5             K
140100020318     C     K             OCCUR     ABCCOMN
140200020318     C                   CLEAR                   CNCN
140300020318     C                   CLEAR                   SNCN
140400020318     C                   CLEAR                   RNCN
140500020318     C                   CLEAR                   PNCN
140600020318     C                   CLEAR                   CACN
140700020318     C                   CLEAR                   SACN
140800020318     C                   CLEAR                   RACN
140900020318     C                   CLEAR                   PACN
141000020318E   1C                   ENDDO
141100020318     C*
141200020318     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
141300020318DO  1C     1             DO        5             K
141400020318     C     K             OCCUR     ABCC13N
141500020318     C                   CLEAR                   NTCN
141600020318     C                   CLEAR                   ATCN
141700020318E   1C                   ENDDO
141800020318     C*
141900020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X NETWORK) "D"
142000020318DO  1C     1             DO        5             K
142100020318     C     K             OCCUR     ABCCOMND
142200020318     C                   CLEAR                   CNCND
142300020318     C                   CLEAR                   SNCND
142400020318     C                   CLEAR                   RNCND
142500020318     C                   CLEAR                   PNCND
142600020318     C                   CLEAR                   CACND
142700020318     C                   CLEAR                   SACND
142800020318     C                   CLEAR                   RACND
142900020318     C                   CLEAR                   PACND
143000020318E   1C                   ENDDO
143100020318     C*
143200020318     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
143300020318DO  1C     1             DO        5             K
143400020318     C     K             OCCUR     ABCC13ND
143500020318     C                   CLEAR                   NTCND
143600020318     C                   CLEAR                   ATCND
143700020318E   1C                   ENDDO
143800000000     C*
143900000000     C                   ENDSR
144000000000     C*------------------------------------------------------------------------*
144100000000     C*?NEWKSC - OPERAZIONI PER NUOVO CLIENTE
144200000000     C*------------------------------------------------------------------------*
144300000000     C     NEWKSC        BEGSR
144400020221     C*
144500020221     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
144600020221     C                   MOVEL     '0'           FLGSIDK
144700000000     C*
144800000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
144900000000     C                   MOVEL     '0'           PASKSC
145000000000     C*
145100951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (13^ COLONNA) PERIODO CORRENTE
145200000000     C                   MOVEL     '0'           FLAKSC
145300951227     C*
145400951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (1^ COLONNA) PERIODO PRECEDENTE
145500951227     C                   MOVEL     '0'           FL2KSC
145600000000     C*
145700000000     C* REIMPOSTO FLAG PASSAGGIO LETTURA SALDI PRECEDENTI A SALDI CORRENTI
145800000000     C                   MOVEL     '0'           FLAPAS
145900000830     C*
146000000830     C* REIMPOSTO FLAG CONTATORE CLIENTI PER MESE
146100000830     C                   MOVEL     '0'           SFCCL
146200000000     C*
146300000000     C* MEMORIZZO IL NUOVO CLIENTE IN UN DEPOSITO
146400000825     C                   Z-ADD     WFDCLI        DEPCLI
146500020319     C*
146600020319     C* RE-INIZIALIZZO LA SCHIERA DI WRK X CLIENTE/MESE/NETWORK
146700020319     C                   Z-ADD     *ZEROS        Y
146800020319     C                   CLEAR                   WSK1
146900000000     C*
147000000000     C* DECODIFICO IL CODICE CLIENTE
147100000000     C                   EXSR      DECKSC
147200000000     C*
147300000000     C* AZZERA I TOTALI DEL CLIENTE
147400121217     C                   CLEAR                   CLN1K
147500000000     C                   CLEAR                   S1K
147600081013     C                   CLEAR                   Kg1K
147700000000     C                   CLEAR                   R1K
147800000000     C                   CLEAR                   P1K
147900970822     C                   CLEAR                   D1K
148000020712     C                   CLEAR                   R1KP
148100070419     C                   CLEAR                   ap_S1K
148200081013     C                   CLEAR                   ap_Kg1K
148300070419     C                   CLEAR                   ap_R1K
148400070419     C                   CLEAR                   ap_P1K
148500091123      * -------------
148600100204     /*?  solo x per l'Estero                                   ?
148700100204     /*?     x Network                                          ?
148800100204     /*?   Pulisce le schiere di totale                         ?
148900091123      * -------------
149000091123IF  1C     PARIOE        IFEQ      'E'
149100091123      * -------------
149200091123DO  1C     1             DO        5             K
149300100204     C* AZZERA I TOTALI x NETWORK anno in corso
149400091124     C     K             OCCUR     W1ntw
149500091124     C                   CLEAR                   C1ntw
149600091124     C                   CLEAR                   S1ntw
149700091124     C                   CLEAR                   KG1ntw
149800091124     C                   CLEAR                   R1ntw
149900091124     C                   CLEAR                   P1ntw
150000091124     C                   CLEAR                   D1ntw
150100091124     C                   CLEAR                   R1Pntw
150200091125     C                   CLEAR                   TR1ntw
150300091125     C                   CLEAR                   TP1ntw
150400091124     C* AZZERA I TOTALI x NETWORK  dell'Anno precedente
150500091124     C     K             OCCUR     Wap1ntw
150600091124     C                   CLEAR                   ap_C1ntw
150700091124     C                   CLEAR                   ap_S1ntw
150800091124     C                   CLEAR                   ap_KG1ntw
150900091124     C                   CLEAR                   ap_R1ntw
151000091124     C                   CLEAR                   ap_P1ntw
151100091124     C                   CLEAR                   ap_D1ntw
151200091124     C                   CLEAR                   ap_R1Pntw
151300091125     C                   CLEAR                   ap_TR1ntw
151400091125     C                   CLEAR                   ap_TP1ntw
151500091123E   1C                   ENDDO
151600091123      * -------------
151700091123E   1C                   END
151800091123      * -------------
151900100204     /*?  Pulizia schiere di totali                             ?
152000100204     /*?    x  ntw e tipi servizio                              ?
152100100204     /*?      sia Italia che Estero                             ?
152200100129      * -------------
152300100129DO  1C     1             DO        50            Kserv             3 0
152400100201     C* AZZERA I TOTALI x NETWORK/TIPO SERVIZIO
152500100129     C     Kserv         OCCUR     W1ntwTS
152600100202      * chiave NTW + TServ
152700100129     C                   CLEAR                   C1ntwTS
152800100201     C                   CLEAR                   C1tpsTS
152900100202     C* AZZERA I TOTALI x NETWORK  dell'Anno in Corso
153000100129     C                   CLEAR                   S1ntwTS
153100100129     C                   CLEAR                   KG1ntwTS
153200100129     C                   CLEAR                   R1ntwTS
153300100129     C                   CLEAR                   P1ntwTS
153400100129     C                   CLEAR                   D1ntwTS
153500100129     C                   CLEAR                   R1PntwTS
153600100129     C                   CLEAR                   TR1ntwTS
153700100129     C                   CLEAR                   TP1ntwTS
153800100129     C* AZZERA I TOTALI x NETWORK  dell'Anno precedente
153900100129     C                   CLEAR                   ap_S1ntwTS
154000100129     C                   CLEAR                   ap_KG1ntwTS
154100100129     C                   CLEAR                   ap_R1ntwTS
154200100129     C                   CLEAR                   ap_P1ntwTS
154300100129     C                   CLEAR                   ap_D1ntwTS
154400100129     C                   CLEAR                   ap_R1PntwTS
154500100129     C                   CLEAR                   ap_TR1ntwTS
154600100129     C                   CLEAR                   ap_TP1ntwTS
154700100202     C*
154800100129E   1C                   ENDDO
154900100129      * -------------
155000000828     C* CERCA I FIGLI DEL CLIENTE, SE RICHIESTA STAMPA PER UNFICANTE
155100000828     C                   CLEAR                   SKSC11
155200000828IF  1C     PARSCE        IFEQ      'U'                                          *SCELTA UNIFICANTE
155300000828     C                   CLEAR                   BS10DS
155400010725     C*                  Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
155500010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
155600000828     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
155700000828     C                   MOVEL     'F'           D10PAF                         *CERCA I FIGLI
155800000828     C                   MOVE      DEPCLI        D10COD
155900000828     C                   CALL      'TIBS10R'
156000000828     C                   PARM                    BS10DS
156100000828E   1C                   ENDIF
156200000000     C*
156300130131     C*  Totalizza le Varie per CLIENTE sul nuovo WADELPCV dal WFDEAV0F
156400130131     C*   man mano che le riporta sul file di TOTALE le ELIMINA dal file di WORK.
156500130131     c                   exsr      VARIE_x_Client
156600130131     C*
156700000000     C                   ENDSR
156800000000     C*------------------------------------------------------------------------*
156900000000     C*?NEWPPC - OPERAZIONI PER PASSAGGIO LETTURA SALDI PRECED-->SALDI CORRENTI
157000000000     C*------------------------------------------------------------------------*
157100000000     C     NEWPPC        BEGSR
157200000000     C*
157300000000     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
157400000000     C* PRENDO COME "CLIENTE NUOVO/ACQUISITO" QUELLO DEL PRIMO SALDO PERCHE' SE
157500000000     C* UN CLIENTE E' NUOVO PER UN MESE ("Y") LO E' PER TUTTO IL PERIODO
157600000000IF  1C     TIPANN        IFEQ      'C'
157700000000IF  2C     FLAPAS        IFEQ      '0'
157800000000     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
157900000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O NON NUOVO
158000000000E   2C                   ENDIF
158100000000E   1C                   ENDIF
158200000000     C*
158300030924     C                   ENDSR
158400030924     C*------------------------------------------------------------------------*
158500030924     C*?MEMSAL - MEMORIZZA I SALDI DEL WORK FILE IN SCHIERE
158600030924     C*------------------------------------------------------------------------*
158700030924     C     MEMSAL        BEGSR
158800030924     C*
158900030924     C* I SALDI DEL PERIODO PRECEDENTE LI MEMORIZZA IN 13^ POSIZIONE DI SCHIERA
159000030924     C* UN CLIENTE E' NUOVO NEL PERIODO SOLO SE HA UNA "Y" NEI SALDI
159100030924     C*
159200030924     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
159300030924     C                   MOVEL     '1'           PASKSC                         *CLIENTE
159400030924     C                   MOVEL     '1'           PASAGE                         *AGENTE
159500030924     C                   MOVEL     '1'           PASFIL                         *FILIALE
159600030924     C                   MOVEL     '1'           PASARE                         *AREA
159700030924     C                   MOVEL     '1'           PASDIV                         *DIVISIONE
159800030924     C                   MOVEL     '1'           PASGEN                         *GENERALE
159900000000     C*
160000951227     C* CONTA I CLIENTI DA STAMPARE ALLA 1^ O 13^ COLONNA SOLO A ROTTURA DI
160100951227     C* CLIENTE (SE STA LEGGENDO I SALDI DEL PERIODO PRECEDENTE O CORRENTE)
160200951227IF  1C     TIPANN        IFEQ      'C'                                          *CORRENTE
160300951227IF  2C     FLAKSC        IFEQ      '0'                                          *ROTTURA CLIENTE
160400000000     C                   MOVEL     '1'           FLAKSC
160500951227     C                   Z-ADD     1             CONKSC            1 0          *CLIENTE NON AGGIUNT
160600951227X   2C                   ELSE
160700951227     C                   Z-ADD     *ZEROS        CONKSC                         *CLIENTE AGGIUNTO
160800951227E   2C                   ENDIF
160900951227X   1C                   ELSE                                                   *PRECEDENTE
161000951227IF  2C     FL2KSC        IFEQ      '0'                                          *ROTTURA CLIENTE
161100951227     C                   MOVEL     '1'           FL2KSC
161200951227     C                   Z-ADD     1             CO2KSC            1 0          *CLIENTE AGGIUNTO
161300951227X   2C                   ELSE
161400951227     C                   Z-ADD     *ZEROS        CO2KSC                         *CLIENTE NON AGGIUNT
161500951227E   2C                   ENDIF
161600951227E   1C                   ENDIF
161700020318     C*
161800020318     C* Q U A L I T A'  /  N E T W O R K
161900000000     C                   Z-ADD     1             Q
162000000000     C                   SETOFF                                           28
162100000000     C     DESCLV        LOOKUP    ABC(Q)                                 28
162200020318     C                   Z-ADD     1             K
162300020318     C                   SETOFF                                           27
162400020318     C     WFDNTW        LOOKUP    DNTW(K)                                27
162500000000IF  1C     *IN28         IFEQ      *ON
162600020318IF  1C     *IN27         ANDEQ     *ON
162700000000     C* M E S E
162800990609IF  2C     WFDDAT        IFGE      PARDPT                                       *PERIODO PRECEDENTE
162900990609     C     WFDDAT        ANDLE     PARDP2
163000000000     C                   Z-ADD     13            MM
163100020712     C                   Z-ADD     1             MMP               2 0
163200020712     C                   SETOFF                                           33
163300020712     C     WFDMES        LOOKUP    MES(MMP)                               33
163400951219E   2C                   ENDIF
163500990609IF  2C     WFDDAT        IFGE      PARDAT                                       *PERIODO CORRENTE
163600990609     C     WFDDAT        ANDLE     PARDA2
163700000000     C                   Z-ADD     1             MM
163800000000     C                   SETOFF                                           28
163900000000     C     WFDMES        LOOKUP    MES(MM)                                28
164000000000E   2C                   ENDIF
164100020220     C*
164200020220     C* VERIFICO SE ALMENO 1 CLIENTE "D"
164300020318     C                   IF        DESCLV = 'D'
164400020220     C                   MOVEL     '1'           FLGSIDG           1
164500020220     C                   MOVEL     '1'           FLGSIDD           1
164600020220     C                   MOVEL     '1'           FLGSIDA           1
164700020220     C                   MOVEL     '1'           FLGSIDF           1
164800020220     C                   MOVEL     '1'           FLGSIDE           1
164900020221     C                   MOVEL     '1'           FLGSIDK           1
165000020220     C                   ENDIF
165100020319     C*
165200020319     C* VERIFICO SE X GIA' CONTEGGIATO IL CLIENTE/MESE/NETWORK
165300020319     C     DSWSK1        LOOKUP    WSK1                                   55
165400020319     C                   IF        *IN55 = *ON
165500020319     C                   Z-ADD     *ZEROS        CONNTW            1 0
165600020319     C                   ELSE
165700020319     C                   ADD       1             Y                 3 0
165800020319     C                   EVAL      WSK1(Y) = DSWSK1
165900020319     C                   Z-ADD     1             CONNTW
166000020319     C                   ENDIF
166100000000     C*--------------------
166200020220     C* C L I E N T E
166300000000     C*--------------------
166400121217     C                   move      WFDcln        cln1K(MM)                      memorizza i flag di cliente nuovo/acq
166500000000     C                   ADD       WFDNSP        S1K(MM)                        SPEDIZIONI
166600081013     C                   eval      Kg1K(MM) = wfdPKG + Kg1K(MM)                 PESI in KG
166700000000     C                   ADD       WFDRRN        R1K(MM)                        RICAVI REALI DELTA
166800000000     C                   ADD       WFDRPN        P1K(MM)                        RICAVI PRESUNTI DELT
166900091123      * -------------
167000091123      **  solo x per l'estero incasella x Network
167100091123IF  1C     PARIOE        IFEQ      'E'
167200091124IF  1C     MM            andle     12
167300091124     C     K             OCCUR     W1ntw
167400091124     c                   movel     WFDNTW        C1ntw
167500091124     C                   ADD       WFDNSP        S1ntw(MM)                        SPEDIZIONI
167600091124     C                   eval      Kg1ntw(MM) = wfdPKG + Kg1ntw(MM)                 PESI in KG
167700091124     C                   ADD       WFDRRN        R1ntw(MM)                        RICAVI REALI DELTA
167800091124     C                   ADD       WFDRPN        P1ntw(MM)                        RICAVI PRESUNTI DELT
167900091125     C                   z-add(h)  WFDRRN        R1totale         15 0
168000091125     C                   z-add(h)  WFDRPN        P1totale         15 0
168100091125     C                   ADD       R1totale      TR1ntw
168200091125     C                   ADD       P1totale      TP1ntw
168300091123     c                   END
168400091123      **
168500970822IF  2C     MM            IFNE      13                                           SOLO PERIODO CORRENT
168600020220     C                   MOVEL     WFDSPD        D1K(MM)                        FLAG % SCOSTAM.DELTA
168700070420      *
168800070420      * ?se invece è un dato dell'anno precedente
168900070420     c                   else
169000081013      *
169100070420      * ?lo devo incasellare nel mese appropriato
169200070420     C                   ADD       WFDNSP        ap_S1K(WFDmes)                 SPEDIZIONI
169300081013     C                   eval      ap_Kg1K(WFDmes) = wfdPKG + ap_Kg1K(WFDmes)   PESI in KG
169400070420     C                   ADD       WFDRRN        ap_R1K(WFDmes)                 RICAVI REALI DELTA
169500070420     C                   ADD       WFDRPN        ap_P1K(WFDmes)                 RICAVI PRESUNTI DELT
169600091123      * -------------
169700091123      **  solo x per l'estero incasella x Network
169800091123IF  1C     PARIOE        IFEQ      'E'
169900091124IF  1C     WFDmes        andle     12
170000091124     C     K             OCCUR     WAP1ntw
170100091124     c                   movel     WFDNTW        Ap_c1ntw
170200091124     C                   eval      ap_S1ntw(WFDmes) = WFDNSP + ap_S1ntw(WFDmes)    SPEDIZIONI
170300091124     C                   eval      ap_Kg1ntw(WFDmes)= wfdPKG +ap_Kg1ntw(WFDmes)      PESI in KG
170400091124     C                   eval      ap_R1ntw(WFDmes) = WFDRRN + ap_R1ntw(WFDmes)    RICAVI REALI DELT
170500091124     C                   eval      ap_P1ntw(WFDmes) = WFDRPN + ap_P1ntw(WFDmes)    RICAVI PRESUNTI D
170600091125     C                   z-add(h)  WFDRRN        R1totale         15 0
170700091125     C                   z-add(h)  WFDRPN        P1totale         15 0
170800091125     C                   ADD       R1totale      ap_TR1ntw
170900091125     C                   ADD       P1totale      ap_TP1ntw
171000091123     c                   END
171100091123      **
171200970822E   2C                   ENDIF
171300100201      * -------------
171400100204      **
171500100204     /*?  totalizza incasellando x Network e Tipo Servizio      ?
171600100204     /*?   x Italia e poi x Estero            ......e           ?
171700100204     /*?   se RICHIESTA creazione del File PC   (WFDELPCF)      ?
171800100204IF  1C                   IF        PARIOE <> *blank and PARFPC = 'S'
171900100201     c                   exsr      TOTCLI_NTW_TS
172000100201     c                   END
172100100201      * -------------
172200070420      *
172300020712     C     TIPANN        IFEQ      'P'
172400020712     C     *IN33         ANDEQ     *ON
172500020712     C                   ADD       WFDRRN        R1KP(MMP)                      RICAVI REALI DELTA A
172600020712     C                   ENDIF
172700000000     C*--------------------
172800020220     C* C O M M E R C I A L E
172900000000     C*--------------------
173000000000     C     Q             OCCUR     ABCCOM
173100020318     C     K             OCCUR     ABCCOMN
173200020318     C     K             OCCUR     ABCCOMND
173300000000     C* NUOVO
173400000000IF  2C     DESCLN        IFEQ      'Y'
173500000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
173600000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
173700020318     C                   ADD       1             CNC(MM)                        NUMERO CLIENTI
173800000830E   3C                   ENDIF
173900020319     C     MM            IFNE      13
174000020319     C                   IF        DESCLV <> 'D'
174100020319     C                   ADD       CONNTW        CNCN(MM)                       NUMERO CLIENTI
174200020318     C                   ELSE
174300020319     C                   ADD       CONNTW        CNCND(MM)                      NUMERO CLIENTI
174400020318     C                   ENDIF
174500020319     C                   ENDIF
174600000000     C                   ADD       WFDNSP        SNC(MM)                        SPEDIZIONI
174700000000     C                   ADD       WFDRRN        RNC(MM)                        RICAVI REALI DELTA
174800000000     C                   ADD       WFDRPN        PNC(MM)                        RICAVI PRESUNTI DELT
174900020318     C                   IF        DESCLV <> 'D'
175000020318     C                   ADD       WFDNSP        SNCN(MM)                       SPEDIZIONI
175100020318     C                   ADD       WFDRRN        RNCN(MM)                       RICAVI REALI DELTA
175200020318     C                   ADD       WFDRPN        PNCN(MM)                       RICAVI PRESUNTI DELT
175300020318     C                   ELSE
175400020318     C                   ADD       WFDNSP        SNCND(MM)                      SPEDIZIONI
175500020318     C                   ADD       WFDRRN        RNCND(MM)                      RICAVI REALI DELTA
175600020318     C                   ADD       WFDRPN        PNCND(MM)                      RICAVI PRESUNTI DELT
175700020318     C                   ENDIF
175800000000     C* ACQUISITO
175900000000X   2C                   ELSE
176000000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
176100000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
176200000000     C                   ADD       1             CAC(MM)                        NUMERO CLIENTI
176300000830E   3C                   ENDIF
176400020319     C     MM            IFNE      13
176500020319     C                   IF        DESCLV <> 'D'
176600020319     C                   ADD       CONNTW        CACN(MM)                       NUMERO CLIENTI
176700020318     C                   ELSE
176800020319     C                   ADD       CONNTW        CACND(MM)                      NUMERO CLIENTI
176900020318     C                   ENDIF
177000020319     C                   ENDIF
177100000000     C                   ADD       WFDNSP        SAC(MM)                        SPEDIZIONI
177200000000     C                   ADD       WFDRRN        RAC(MM)                        RICAVI REALI DELTA
177300000000     C                   ADD       WFDRPN        PAC(MM)                        RICAVI PRESUNTI DELT
177400020318     C                   IF        DESCLV <> 'D'
177500020318     C                   ADD       WFDNSP        SACN(MM)                       SPEDIZIONI
177600020318     C                   ADD       WFDRRN        RACN(MM)                       RICAVI REALI DELTA
177700020318     C                   ADD       WFDRPN        PACN(MM)                       RICAVI PRESUNTI DELT
177800020318     C                   ELSE
177900020318     C                   ADD       WFDNSP        SACND(MM)                      SPEDIZIONI
178000020318     C                   ADD       WFDRRN        RACND(MM)                      RICAVI REALI DELTA
178100020318     C                   ADD       WFDRPN        PACND(MM)                      RICAVI PRESUNTI DELT
178200020318     C                   ENDIF
178300000000E   2C                   ENDIF
178400000000     C*---
178500000000     C* TOT CLIENTI 13^ COLONNA
178600000000     C*---
178700000000     C     Q             OCCUR     ABCC13
178800020318     C     K             OCCUR     ABCC13N
178900020318     C     K             OCCUR     ABCC13ND
179000951227     C* NUOVO
179100951227IF  2C     DESCLN        IFEQ      'Y'
179200951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
179300951227     C                   ADD       CO2KSC        NTC(2)
179400020318     C                   IF        DESCLV <> 'D'
179500020318     C                   ADD       CO2KSC        NTCN(2)
179600020318     C                   ELSE
179700020318     C                   ADD       CO2KSC        NTCND(2)
179800020318     C                   ENDIF
179900951227X   3C                   ELSE                                                   *PERIODO CORRENTE
180000951227     C                   ADD       CONKSC        NTC(1)
180100020318     C                   IF        DESCLV <> 'D'
180200020318     C                   ADD       CONKSC        NTCN(1)
180300020318     C                   ELSE
180400020318     C                   ADD       CONKSC        NTCND(1)
180500020318     C                   ENDIF
180600951227E   3C                   ENDIF
180700951227     C* ACQUISITO
180800951227X   2C                   ELSE
180900951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
181000951227     C                   ADD       CO2KSC        ATC(2)
181100020318     C                   IF        DESCLV <> 'D'
181200020318     C                   ADD       CO2KSC        ATCN(2)
181300020318     C                   ELSE
181400020318     C                   ADD       CO2KSC        ATCND(2)
181500020318     C                   ENDIF
181600951227X   3C                   ELSE                                                   *PERIODO CORRENTE
181700951227     C                   ADD       CONKSC        ATC(1)
181800020318     C                   IF        DESCLV <> 'D'
181900020318     C                   ADD       CONKSC        ATCN(1)
182000020318     C                   ELSE
182100020318     C                   ADD       CONKSC        ATCND(1)
182200020318     C                   ENDIF
182300951227E   3C                   ENDIF
182400951227E   2C                   ENDIF
182500000000     C*--------------------
182600000000     C* F I L I A L E
182700000000     C*--------------------
182800000000     C     Q             OCCUR     ABCFIL
182900020318     C     K             OCCUR     ABCFILN
183000020318     C     K             OCCUR     ABCFILND
183100000000     C* NUOVO
183200000000IF  2C     DESCLN        IFEQ      'Y'
183300000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
183400000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
183500000000     C                   ADD       1             CNF(MM)                        NUMERO CLIENTI
183600000830E   3C                   ENDIF
183700020319     C     MM            IFNE      13
183800020319     C                   IF        DESCLV <> 'D'
183900020319     C                   ADD       CONNTW        CNFN(MM)                       NUMERO CLIENTI
184000020318     C                   ELSE
184100020319     C                   ADD       CONNTW        CNFND(MM)                      NUMERO CLIENTI
184200020318     C                   ENDIF
184300020319     C                   ENDIF
184400000000     C                   ADD       WFDNSP        SNF(MM)                        SPEDIZIONI
184500000000     C                   ADD       WFDRRN        RNF(MM)                        RICAVI REALI DELTA
184600000000     C                   ADD       WFDRPN        PNF(MM)                        RICAVI PRESUNTI DELT
184700020318     C                   IF        DESCLV <> 'D'
184800020318     C                   ADD       WFDNSP        SNFN(MM)                       SPEDIZIONI
184900020318     C                   ADD       WFDRRN        RNFN(MM)                       RICAVI REALI DELTA
185000020318     C                   ADD       WFDRPN        PNFN(MM)                       RICAVI PRESUNTI DELT
185100020318     C                   ELSE
185200020318     C                   ADD       WFDNSP        SNFND(MM)                      SPEDIZIONI
185300020318     C                   ADD       WFDRRN        RNFND(MM)                      RICAVI REALI DELTA
185400020318     C                   ADD       WFDRPN        PNFND(MM)                      RICAVI PRESUNTI DELT
185500020318     C                   ENDIF
185600000000     C* ACQUISITO
185700000000X   2C                   ELSE
185800000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
185900000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
186000000000     C                   ADD       1             CAF(MM)                        NUMERO CLIENTI
186100000830E   3C                   ENDIF
186200020319     C     MM            IFNE      13
186300020319     C                   IF        DESCLV <> 'D'
186400020319     C                   ADD       CONNTW        CAFN(MM)                       NUMERO CLIENTI
186500020318     C                   ELSE
186600020319     C                   ADD       CONNTW        CAFND(MM)                      NUMERO CLIENTI
186700020318     C                   ENDIF
186800020319     C                   ENDIF
186900000000     C                   ADD       WFDNSP        SAF(MM)                        SPEDIZIONI
187000000000     C                   ADD       WFDRRN        RAF(MM)                        RICAVI REALI DELTA
187100000000     C                   ADD       WFDRPN        PAF(MM)                        RICAVI PRESUNTI DELT
187200020318     C                   IF        DESCLV <> 'D'
187300020318     C                   ADD       WFDNSP        SAFN(MM)                       SPEDIZIONI
187400020318     C                   ADD       WFDRRN        RAFN(MM)                       RICAVI REALI DELTA
187500020318     C                   ADD       WFDRPN        PAFN(MM)                       RICAVI PRESUNTI DELT
187600020318     C                   ELSE
187700020318     C                   ADD       WFDNSP        SAFND(MM)                      SPEDIZIONI
187800020318     C                   ADD       WFDRRN        RAFND(MM)                      RICAVI REALI DELTA
187900020318     C                   ADD       WFDRPN        PAFND(MM)                      RICAVI PRESUNTI DELT
188000020318     C                   ENDIF
188100000000E   2C                   ENDIF
188200000000     C*---
188300000000     C* TOT CLIENTI 13^ COLONNA
188400000000     C*---
188500000000     C     Q             OCCUR     ABCF13
188600020318     C     K             OCCUR     ABCF13N
188700020318     C     K             OCCUR     ABCF13ND
188800951227IF  2C     DESCLN        IFEQ      'Y'
188900951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
189000951227     C                   ADD       CO2KSC        NTF(2)
189100020318     C                   IF        DESCLV <> 'D'
189200020319     C                   ADD       CONNTW        NTFN(2)
189300020318     C                   ELSE
189400020319     C                   ADD       CONNTW        NTFND(2)
189500020318     C                   ENDIF
189600951227X   3C                   ELSE                                                   *PERIODO CORRENTE
189700951227     C                   ADD       CONKSC        NTF(1)
189800020318     C                   IF        DESCLV <> 'D'
189900020319     C                   ADD       CONNTW        NTFN(1)
190000020318     C                   ELSE
190100020319     C                   ADD       CONNTW        NTFND(1)
190200020318     C                   ENDIF
190300951227E   3C                   ENDIF
190400030924     C* ACQUISITO
190500951227X   2C                   ELSE
190600951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
190700951227     C                   ADD       CO2KSC        ATF(2)
190800030924     C                   IF        DESCLV <> 'D'
190900030924     C                   ADD       CONNTW        ATFN(2)
191000030924     C                   ELSE
191100020319     C                   ADD       CONNTW        ATFND(2)
191200020318     C                   ENDIF
191300951227X   3C                   ELSE                                                   *PERIODO CORRENTE
191400951227     C                   ADD       CONKSC        ATF(1)
191500020318     C                   IF        DESCLV <> 'D'
191600020319     C                   ADD       CONNTW        ATFN(1)
191700020318     C                   ELSE
191800020319     C                   ADD       CONNTW        ATFND(1)
191900020318     C                   ENDIF
192000951227E   3C                   ENDIF
192100951227E   2C                   ENDIF
192200000000     C*--------------------
192300000000     C* A R E A
192400000000     C*--------------------
192500000000     C     Q             OCCUR     ABCARE
192600020318     C     K             OCCUR     ABCAREN
192700020318     C     K             OCCUR     ABCAREND
192800000000     C* NUOVO
192900000000IF  2C     DESCLN        IFEQ      'Y'
193000000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
193100000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
193200000000     C                   ADD       1             CNA(MM)                        NUMERO CLIENTI
193300000830E   3C                   ENDIF
193400020319     C     MM            IFNE      13
193500020319     C                   IF        DESCLV <> 'D'
193600020319     C                   ADD       CONNTW        CNAN(MM)                       NUMERO CLIENTI
193700020318     C                   ELSE
193800020319     C                   ADD       CONNTW        CNAND(MM)                      NUMERO CLIENTI
193900020318     C                   ENDIF
194000020319     C                   ENDIF
194100000000     C                   ADD       WFDNSP        SNA(MM)                        SPEDIZIONI
194200000000     C                   ADD       WFDRRN        RNA(MM)                        RICAVI REALI DELTA
194300000000     C                   ADD       WFDRPN        PNA(MM)                        RICAVI PRESUNTI DELT
194400020318     C                   IF        DESCLV <> 'D'
194500020318     C                   ADD       WFDNSP        SNAN(MM)                       SPEDIZIONI
194600020318     C                   ADD       WFDRRN        RNAN(MM)                       RICAVI REALI DELTA
194700020318     C                   ADD       WFDRPN        PNAN(MM)                       RICAVI PRESUNTI DELT
194800020318     C                   ELSE
194900020318     C                   ADD       WFDNSP        SNAND(MM)                      SPEDIZIONI
195000020318     C                   ADD       WFDRRN        RNAND(MM)                      RICAVI REALI DELTA
195100020318     C                   ADD       WFDRPN        PNAND(MM)                      RICAVI PRESUNTI DELT
195200020318     C                   ENDIF
195300000000     C* ACQUISITO
195400000000X   2C                   ELSE
195500000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
195600000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
195700000000     C                   ADD       1             CAA(MM)                        NUMERO CLIENTI
195800000830E   3C                   ENDIF
195900020319     C     MM            IFNE      13
196000020319     C                   IF        DESCLV <> 'D'
196100020319     C                   ADD       CONNTW        CAAN(MM)                       NUMERO CLIENTI
196200020318     C                   ELSE
196300020319     C                   ADD       CONNTW        CAAND(MM)                      NUMERO CLIENTI
196400020318     C                   ENDIF
196500020319     C                   ENDIF
196600000000     C                   ADD       WFDNSP        SAA(MM)                        SPEDIZIONI
196700000000     C                   ADD       WFDRRN        RAA(MM)                        RICAVI REALI DELTA
196800000000     C                   ADD       WFDRPN        PAA(MM)                        RICAVI PRESUNTI DELT
196900020318     C                   IF        DESCLV <> 'D'
197000020318     C                   ADD       WFDNSP        SAAN(MM)                       SPEDIZIONI
197100020318     C                   ADD       WFDRRN        RAAN(MM)                       RICAVI REALI DELTA
197200020318     C                   ADD       WFDRPN        PAAN(MM)                       RICAVI PRESUNTI DELT
197300020318     C                   ELSE
197400020318     C                   ADD       WFDNSP        SAAND(MM)                      SPEDIZIONI
197500020318     C                   ADD       WFDRRN        RAAND(MM)                      RICAVI REALI DELTA
197600020318     C                   ADD       WFDRPN        PAAND(MM)                      RICAVI PRESUNTI DELT
197700020318     C                   ENDIF
197800000000E   2C                   ENDIF
197900000000     C*---
198000000000     C* TOT CLIENTI 13^ COLONNA
198100000000     C*---
198200000000     C     Q             OCCUR     ABCA13
198300020318     C     K             OCCUR     ABCA13N
198400020318     C     K             OCCUR     ABCA13ND
198500951227IF  2C     DESCLN        IFEQ      'Y'
198600951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
198700951227     C                   ADD       CO2KSC        NTA(2)
198800020318     C                   IF        DESCLV <> 'D'
198900020319     C                   ADD       CONNTW        NTAN(2)
199000020318     C                   ELSE
199100020319     C                   ADD       CONNTW        NTAND(2)
199200020318     C                   ENDIF
199300951227X   3C                   ELSE                                                   *PERIODO CORRENTE
199400951227     C                   ADD       CONKSC        NTA(1)
199500020318     C                   IF        DESCLV <> 'D'
199600020319     C                   ADD       CONNTW        NTAN(1)
199700020318     C                   ELSE
199800020319     C                   ADD       CONNTW        NTAND(1)
199900020318     C                   ENDIF
200000951227E   3C                   ENDIF
200100951227     C* ACQUISITO
200200951227X   2C                   ELSE
200300951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
200400951227     C                   ADD       CO2KSC        ATA(2)
200500020318     C                   IF        DESCLV <> 'D'
200600020319     C                   ADD       CONNTW        ATAN(2)
200700020318     C                   ELSE
200800020319     C                   ADD       CONNTW        ATAND(2)
200900020318     C                   ENDIF
201000951227X   3C                   ELSE                                                   *PERIODO CORRENTE
201100951227     C                   ADD       CONKSC        ATA(1)
201200020318     C                   IF        DESCLV <> 'D'
201300020319     C                   ADD       CONNTW        ATAN(1)
201400020318     C                   ELSE
201500020319     C                   ADD       CONNTW        ATAND(1)
201600020318     C                   ENDIF
201700951227E   3C                   ENDIF
201800951227E   2C                   ENDIF
201900000000     C*--------------------
202000000000     C* D I V I S I O N E
202100000000     C*--------------------
202200000000     C     Q             OCCUR     ABCDIV
202300020318     C     K             OCCUR     ABCDIVN
202400020318     C     K             OCCUR     ABCDIVND
202500000000     C* NUOVO
202600000000IF  2C     DESCLN        IFEQ      'Y'
202700000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
202800000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
202900000000     C                   ADD       1             CND(MM)                        NUMERO CLIENTI
203000000830E   3C                   ENDIF
203100020319     C     MM            IFNE      13
203200020319     C                   IF        DESCLV <> 'D'
203300020319     C                   ADD       CONNTW        CNDN(MM)                       NUMERO CLIENTI
203400020318     C                   ELSE
203500020319     C                   ADD       CONNTW        CNDND(MM)                      NUMERO CLIENTI
203600020318     C                   ENDIF
203700020319     C                   ENDIF
203800000000     C                   ADD       WFDNSP        SND(MM)                        SPEDIZIONI
203900000000     C                   ADD       WFDRRN        RND(MM)                        RICAVI REALI DELTA
204000000000     C                   ADD       WFDRPN        PND(MM)                        RICAVI PRESUNTI DELT
204100020318     C                   IF        DESCLV <> 'D'
204200020318     C                   ADD       WFDNSP        SNDN(MM)                       SPEDIZIONI
204300020318     C                   ADD       WFDRRN        RNDN(MM)                       RICAVI REALI DELTA
204400020318     C                   ADD       WFDRPN        PNDN(MM)                       RICAVI PRESUNTI DELT
204500020318     C                   ELSE
204600020318     C                   ADD       WFDNSP        SNDND(MM)                      SPEDIZIONI
204700020318     C                   ADD       WFDRRN        RNDND(MM)                      RICAVI REALI DELTA
204800020318     C                   ADD       WFDRPN        PNDND(MM)                      RICAVI PRESUNTI DELT
204900020318     C                   ENDIF
205000000000     C* ACQUISITO
205100000000X   2C                   ELSE
205200000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
205300000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
205400000000     C                   ADD       1             CAD(MM)                        NUMERO CLIENTI
205500000830E   3C                   ENDIF
205600020319     C     MM            IFNE      13
205700020319     C                   IF        DESCLV <> 'D'
205800020319     C                   ADD       CONNTW        CADN(MM)                       NUMERO CLIENTI
205900020318     C                   ELSE
206000020319     C                   ADD       CONNTW        CADND(MM)                      NUMERO CLIENTI
206100020318     C                   ENDIF
206200020319     C                   ENDIF
206300000000     C                   ADD       WFDNSP        SAD(MM)                        SPEDIZIONI
206400000000     C                   ADD       WFDRRN        RAD(MM)                        RICAVI REALI DELTA
206500000000     C                   ADD       WFDRPN        PAD(MM)                        RICAVI PRESUNTI DELT
206600020318     C                   IF        DESCLV <> 'D'
206700020318     C                   ADD       WFDNSP        SADN(MM)                       SPEDIZIONI
206800020318     C                   ADD       WFDRRN        RADN(MM)                       RICAVI REALI DELTA
206900020318     C                   ADD       WFDRPN        PADN(MM)                       RICAVI PRESUNTI DELT
207000020318     C                   ELSE
207100020318     C                   ADD       WFDNSP        SADND(MM)                      SPEDIZIONI
207200020318     C                   ADD       WFDRRN        RADND(MM)                      RICAVI REALI DELTA
207300020318     C                   ADD       WFDRPN        PADND(MM)                      RICAVI PRESUNTI DELT
207400020318     C                   ENDIF
207500000000E   2C                   ENDIF
207600000000     C*---
207700000000     C* TOT CLIENTI 13^ COLONNA
207800000000     C*---
207900000000     C     Q             OCCUR     ABCD13
208000020318     C     K             OCCUR     ABCD13N
208100020318     C     K             OCCUR     ABCD13ND
208200951227     C* NUOVO
208300951227IF  2C     DESCLN        IFEQ      'Y'
208400951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
208500951227     C                   ADD       CO2KSC        NTD(2)
208600020318     C                   IF        DESCLV <> 'D'
208700020319     C                   ADD       CONNTW        NTDN(2)
208800020318     C                   ELSE
208900020319     C                   ADD       CONNTW        NTDND(2)
209000020318     C                   ENDIF
209100030924X   3C                   ELSE                                                   *PERIODO CORRENTE
209200030924     C                   ADD       CONKSC        NTD(1)
209300030924     C                   IF        DESCLV <> 'D'
209400030924     C                   ADD       CONNTW        NTDN(1)
209500030924     C                   ELSE
209600030924     C                   ADD       CONNTW        NTDND(1)
209700020318     C                   ENDIF
209800951227E   3C                   ENDIF
209900951227     C* ACQUISITO
210000951227X   2C                   ELSE
210100951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
210200951227     C                   ADD       CO2KSC        ATD(2)
210300020318     C                   IF        DESCLV <> 'D'
210400020319     C                   ADD       CONNTW        ATDN(2)
210500020318     C                   ELSE
210600020319     C                   ADD       CONNTW        ATDND(2)
210700020318     C                   ENDIF
210800951227X   3C                   ELSE                                                   *PERIODO CORRENTE
210900951227     C                   ADD       CONKSC        ATD(1)
211000020318     C                   IF        DESCLV <> 'D'
211100020319     C                   ADD       CONNTW        ATDN(1)
211200020318     C                   ELSE
211300020319     C                   ADD       CONNTW        ATDND(1)
211400020318     C                   ENDIF
211500951227E   3C                   ENDIF
211600951227E   2C                   ENDIF
211700000000     C*--------------------
211800000000     C* G E N E R A L E
211900000000     C*--------------------
212000000000     C     Q             OCCUR     ABCGEN
212100020318     C     K             OCCUR     ABCGENN
212200020318     C     K             OCCUR     ABCGENND
212300000000     C* NUOVO
212400000000IF  2C     DESCLN        IFEQ      'Y'
212500000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
212600000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
212700000830     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
212800000000     C                   ADD       1             CNG(MM)                        NUMERO CLIENTI
212900000830E   3C                   ENDIF
213000020319     C     MM            IFNE      13
213100020319     C                   IF        DESCLV <> 'D'
213200020319     C                   ADD       CONNTW        CNGN(MM)                       NUMERO CLIENTI
213300020318     C                   ELSE
213400020319     C                   ADD       CONNTW        CNGND(MM)                      NUMERO CLIENTI
213500020318     C                   ENDIF
213600020319     C                   ENDIF
213700000000     C                   ADD       WFDNSP        SNG(MM)                        SPEDIZIONI
213800000000     C                   ADD       WFDRRN        RNG(MM)                        RICAVI REALI DELTA
213900000000     C                   ADD       WFDRPN        PNG(MM)                        RICAVI PRESUNTI DELT
214000020318     C                   IF        DESCLV <> 'D'
214100020318     C                   ADD       WFDNSP        SNGN(MM)                       SPEDIZIONI
214200020318     C                   ADD       WFDRRN        RNGN(MM)                       RICAVI REALI DELTA
214300020318     C                   ADD       WFDRPN        PNGN(MM)                       RICAVI PRESUNTI DELT
214400020318     C                   ELSE
214500020318     C                   ADD       WFDNSP        SNGND(MM)                      SPEDIZIONI
214600020318     C                   ADD       WFDRRN        RNGND(MM)                      RICAVI REALI DELTA
214700020318     C                   ADD       WFDRPN        PNGND(MM)                      RICAVI PRESUNTI DELT
214800020318     C                   ENDIF
214900000000     C* ACQUISITO
215000000000X   2C                   ELSE
215100000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
215200000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
215300000830     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
215400000000     C                   ADD       1             CAG(MM)                        NUMERO CLIENTI
215500000830E   3C                   ENDIF
215600020319     C     MM            IFNE      13
215700020319     C                   IF        DESCLV <> 'D'
215800020319     C                   ADD       CONNTW        CAGN(MM)                       NUMERO CLIENTI
215900020318     C                   ELSE
216000020319     C                   ADD       CONNTW        CAGND(MM)                      NUMERO CLIENTI
216100020318     C                   ENDIF
216200020319     C                   ENDIF
216300000000     C                   ADD       WFDNSP        SAG(MM)                        SPEDIZIONI
216400000000     C                   ADD       WFDRRN        RAG(MM)                        RICAVI REALI DELTA
216500000000     C                   ADD       WFDRPN        PAG(MM)                        RICAVI PRESUNTI DELT
216600020318     C                   IF        DESCLV <> 'D'
216700020318     C                   ADD       WFDNSP        SAGN(MM)                       SPEDIZIONI
216800020318     C                   ADD       WFDRRN        RAGN(MM)                       RICAVI REALI DELTA
216900020318     C                   ADD       WFDRPN        PAGN(MM)                       RICAVI PRESUNTI DELT
217000020318     C                   ELSE
217100020318     C                   ADD       WFDNSP        SAGND(MM)                      SPEDIZIONI
217200020318     C                   ADD       WFDRRN        RAGND(MM)                      RICAVI REALI DELTA
217300020318     C                   ADD       WFDRPN        PAGND(MM)                      RICAVI PRESUNTI DELT
217400020318     C                   ENDIF
217500000000E   2C                   ENDIF
217600000000     C*---
217700000000     C* TOT CLIENTI 13^ COLONNA
217800000000     C*---
217900000000     C     Q             OCCUR     ABCG13
218000020318     C     K             OCCUR     ABCG13N
218100020318     C     K             OCCUR     ABCG13ND
218200000000     C* NUOVO
218300000000IF  2C     DESCLN        IFEQ      'Y'
218400951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
218500951227     C                   ADD       CO2KSC        NTG(2)
218600020318     C                   IF        DESCLV <> 'D'
218700020319     C                   ADD       CONNTW        NTGN(2)
218800020318     C                   ELSE
218900020319     C                   ADD       CONNTW        NTGND(2)
219000020318     C                   ENDIF
219100951227X   3C                   ELSE                                                   *PERIODO CORRENTE
219200951227     C                   ADD       CONKSC        NTG(1)
219300020318     C                   IF        DESCLV <> 'D'
219400020319     C                   ADD       CONNTW        NTGN(1)
219500020318     C                   ELSE
219600020319     C                   ADD       CONNTW        NTGND(1)
219700020318     C                   ENDIF
219800951227E   3C                   ENDIF
219900000000     C* ACQUISITO
220000000000X   2C                   ELSE
220100951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
220200951227     C                   ADD       CO2KSC        ATG(2)
220300020318     C                   IF        DESCLV <> 'D'
220400020319     C                   ADD       CONNTW        ATGN(2)
220500020318     C                   ELSE
220600020319     C                   ADD       CONNTW        ATGND(2)
220700020318     C                   ENDIF
220800951227X   3C                   ELSE                                                   *PERIODO CORRENTE
220900951227     C                   ADD       CONKSC        ATG(1)
221000020318     C                   IF        DESCLV <> 'D'
221100020319     C                   ADD       CONNTW        ATGN(1)
221200020318     C                   ELSE
221300020319     C                   ADD       CONNTW        ATGND(1)
221400020318     C                   ENDIF
221500951227E   3C                   ENDIF
221600000000E   2C                   ENDIF
221700000000     C*
221800020318E   1C                   ENDIF
221900000000     C*
222000000000     C                   ENDSR
222100000000     C*------------------------------------------------------------------------*
222200100204     /*?  TOTCLI_NTW_TS  somma sulle Ds multiple x NTW e Tipo servizio        ?
222300000000     C*------------------------------------------------------------------------*
222400100201     C     TOTCLI_NTW_TS BEGSR
222500100201      **
222600100201     c                   clear                   rec_Ntw_TSv_OK    1
222700100201      **
222800100201      **  Controlla se il record è da prendere in considerazione
222900100201IF  1C                   IF        WFDNTW = *blank
223000100202     c                   eval             rec_Ntw_TSv_OK = 'S'
223100100201     c                   movel(p)  'ITA'         cod_network       3
223200100201IF  1C                   ELSE
223300100202     c                   eval             rec_Ntw_TSv_OK = 'S'
223400100201     c                   movel(p)  WFDNTW        cod_network
223500100201     c                   END
223600100201     c**
223700100201     c                   eval      numElem = %Elem(W1ntwTS)
223800100201     c**
223900100201      **  Quindi se da totalizzare cerca su quale occurence deve metterlo.
224000100202     c                   do        numElem       k
224100100202     C     K             OCCUR     W1ntwTS
224200100202      *
224300100202     ** inserisce se non era stato ancora inserito
224400100202     c                   if         c1ntwTS = *BLANK and c1tpsTS = *BLANK
224500100202     c                   eval       c1ntwTS = cod_network
224600100202     c                   eval       c1tpsTS = wfdTSP
224700100202     c                   end
224800100202      *
224900100202     c** Solo se corrisponde la chiave NTW + TServ.
225000100202     c                   IF         c1ntwTS = cod_network and c1tpsTS = wfdTSP
225100100201     c**
225200100201IF  1C     MM            ifLE      12
225300100202     c**
225400100202     ** aggiunge x l'anno in corso
225500100201     C                   ADD       WFDNSP        S1ntwTS(MM)                        SPEDIZIONI
225600100201     C                   eval      Kg1ntwTS(MM) = wfdPKG + Kg1ntwTS(MM)             PESI in KG
225700100201     C                   ADD       WFDRRN        R1ntwTS(MM)                        RICAVI REALI DELTA
225800100201     C                   ADD       WFDRPN        P1ntwTS(MM)                        RICAVI PRESUNTI DELT
225900100201     C                   z-add(h)  WFDRRN        R1totale
226000100201     C                   z-add(h)  WFDRPN        P1totale
226100100201     C                   ADD       R1totale      TR1ntwTS
226200100201     C                   ADD       P1totale      TP1ntwTS
226300100201     c                   leave
226400100201      **
226500100201     c                   else
226600100201      **
226700100201IF  1C     WFDmes        ifLE      12
226800100202     ** aggiunge x l'anno precedente
226900100201     C                   eval      ap_S1ntwTS(WFDmes) = WFDNSP +
227000100201     C                                ap_S1ntwTS(WFDmes)                              SPEDIZIONI
227100100201     C                   eval      ap_Kg1ntwTS(WFDmes)= wfdPKG +
227200100201     C                                ap_Kg1ntwTS(WFDmes)                             PESI in KG
227300100201     C                   eval      ap_R1ntwTS(WFDmes) = WFDRRN +
227400100201     C                                ap_R1ntwTS(WFDmes)                           RICAVI REALI DELT
227500100201     C                   eval      ap_P1ntwTS(WFDmes) = WFDRPN +
227600100218     C                                ap_P1ntwTS(WFDmes)                           RICAVI PRESUNTI D
227700100201     C                   z-add(h)  WFDRRN        R1totale
227800100201     C                   z-add(h)  WFDRPN        P1totale
227900100201     C                   ADD       R1totale      ap_TR1ntwTS
228000100201     C                   ADD       P1totale      ap_TP1ntwTS
228100100201     c                   leave
228200100201     c                   end
228300100201      **
228400100201     c                   end
228500100202     c                   end
228600100202      **
228700100202     C                   ENDdo
228800100202     c**
228900100201     C                   ENDSR
229000100201     C*------------------------------------------------------------------------*
229100100201     C*?STAKSC - IMPOSTA STAMPA VALORI CLIENTE
229200100201     C*------------------------------------------------------------------------*
229300100201     C     STAKSC        BEGSR
229400100201     C*
229500020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
229600020222     C                   EVAL      DEPSTPD = '0'
229700000000     C*
229800000000     C* STAMPA I VALORI DEL CLIENTE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
229900000000     C* LA STATISTICA DELTA CON DETTAGLIO CLIENTE
230000000000IF  1C     PASKSC        IFEQ      '1'
230100000000     C     PARSTA        ANDEQ     'K'
230200000000     C                   MOVEL     '1'           ORAKSC                         *ORA STAMPA CLIENTE
230300000000     C***
230400000000     C* DESCRIZIONI INTESTAZIONE
230500000000     C***
230600020116     C                   CLEAR                   SE94IN
230700000000     C                   Z-ADD     DEPAGE        INCAGE                         *AGENTE
230800000000     C                   MOVEL     DESAGE        INDAGE                         *DESCRIZIONE AGENTE
230900000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
231000000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
231100000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
231200000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
231300000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
231400000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
231500950301     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
231600000000     C                   MOVEL     DRI(20)       PIDDES                         *RETTANGOLINO
231700020221     C                   MOVEL     *BLANKS       P1DFLD                         *RETTANGOLINO
231800030311     C*
231900030311     C* PREPARA BUFFER FILE PC (WFDELPCF)
232000030311     C                   EXSR      WRIFPC
232100000000     C*
232200000000     C* STAMPA I VALORI DEL CLIENTE
232300020221     C                   EXSR      STMKSC
232400000000     C*
232500000000E   1C                   ENDIF
232600000000     C*
232700000000     C                   ENDSR
232800030311     C*------------------------------------------------------------------------*
232900030311     C*?WRIFPC - IMPOSTA BUFFER FILE PC (WFDELPCF)
233000030311     C*------------------------------------------------------------------------*
233100030311     C     WRIFPC        BEGSR
233200030311     C*
233300100511     C**------------>    CLEAR                   WFDELPC0
233400030311     C*
233500100406     C*********>>>       Z-ADD     INCAGE        WFPAGE                         *AGENTE
233600100406     C*********>>>       MOVEL     INDAGE        WFPAGD                         *DESCRIZIONE AGENTE
233700100406     C*********>>>       Z-ADD     INCFIL        WFPFIL                         *FILIALE
233800100406     C*********>>>       MOVEL     INDFIL        WFPFID                         *DESCRIZIONE FILIALE
233900100406     C*********>>>       Z-ADD     INCARE        WFPARE                         *AREA
234000100406     C*********>>>       MOVEL     INDARE        WFPARD                         *DESCRIZIONE AREA
234100100406     C*********>>>       MOVEL     INCDIV        WFPDIV                         *DISTRETTO
234200100406     C*********>>>       MOVEL     INDDIV        WFPDID                         *DESCRIZIONE DISTRET
234300100406     C*********>>>       Z-ADD     PARAAI        WFPAAD                         *ANNO DA
234400100406     C*********>>>       Z-ADD     PARMMI        WFPMMD                         *MESE DA
234500100406     C*********>>>       Z-ADD     PARAAF        WFPAAA                         *ANNO A
234600100406     C*********>>>       Z-ADD     PARMMF        WFPMMA                         *MESE A
234700100406     C*********>>>       Z-ADD     DEPCLI        WFPCLI                         *CODICE CLIENTE
234800100406     C*********>>>       MOVEL     DESKSC        WFPCLD                         *DESCRIZIONE CLIENTE
234900100406     C*********>>>       MOVEL     DESCLV        WFPCLV                         *CODICE IMPORTANZA C
235000100406     C*********>>>       MOVEL     DESITC        WFPITC                         *CATEGORIA CLIENTE
235100100406     C                   Z-ADD     INCAGE        aaaAGE            7 0          *AGENTE
235200100406     C                   MOVEL(p)  INDAGE        aaaAGD           25            *DESCRIZIONE AGENTE
235300100406     C                   Z-ADD     INCFIL        aaaFIL            3 0          *FILIALE
235400100406     C                   MOVEL(p)  INDFIL        aaaFID           25            *DESCRIZIONE FILIALE
235500100406     C                   Z-ADD     INCARE        aaaARE            3 0          *AREA
235600100406     C                   MOVEL(p)  INDARE        aaaARD           25            *DESCRIZIONE AREA
235700100406     C                   MOVEL(p)  INCDIV        aaaDIV            1            *DISTRETTO
235800100406     C                   MOVEL(p)  INDDIV        aaaDID           25            *DESCRIZIONE DISTRET
235900050223     C*******            MOVEL     INDIOE        WFPIOE                         *DELTA ITALIA/ESTERO
236000100406     C*********>>>       MOVEL     parIOE        WFPIOE                         *DELTA ITALIA/ESTERO
236100100406     C                   MOVEL(p)  parIOE        aaaIOE            1            *DELTA ITALIA/ESTERO
236200100406     C                   Z-ADD     PARAAI        aaaAAD            4 0          *ANNO DA
236300100406     C                   Z-ADD     PARMMI        aaaMMD            2 0          *MESE DA
236400100406     C                   Z-ADD     PARAAF        aaaAAA            4 0          *ANNO A
236500100406     C                   Z-ADD     PARMMF        aaaMMA            2 0          *MESE A
236600100406     C                   Z-ADD     DEPCLI        aaaCLI            7 0          *CODICE CLIENTE
236700100406     C                   MOVEL(p)  DESKSC        aaaCLD           25            *DESCRIZIONE CLIENTE
236800100406     C                   MOVEL(p)  DESCLV        aaaCLV            1            *CODICE IMPORTANZA C
236900100406     C                   MOVEL(p)  DESITC        aaaITC           25            *CATEGORIA CLIENTE
237000070924     C*****
237100070924     C*****              MOVEL     DES09         WFPCLN                         *CLIENTE NUOVO/ACQUI
237200100406     C*********>>>       MOVEL     'A'           WFPCLN
237300100406     C                   MOVEL(p)  'A'           aaaCLN            1
237400070924IF  1C     DESCLN        IFne      *blank                                       *CL.NUOVO SE "Y" o "
237500100406     C*********>>>       MOVEL     'N'           WFPCLN
237600100406     C                   MOVEL     'N'           aaaCLN
237700070924E   1C                   ENDIF
237800030311     C*
237900030311     C                   ENDSR
238000000000     C*------------------------------------------------------------------------*
238100000000     C*?STMKSC - STAMPA I VALORI DEL CLIENTE
238200000000     C*------------------------------------------------------------------------*
238300000000     C     STMKSC        BEGSR
238400061220     C*
238500061220     C* Per controllare se il cliente da stampare è anagraficamente Bloccato
238600061220     C*   nella totalità ossia se è uniuficante di se stesso oppure se ha
238700061220     C*   tutti i codici figli oltre se stesso Bloccati.
238800061220     C                   CLEAR                   Total_Bloccato    1
238900061220     C*
239000000000     C*--------------------
239100000000     C* I N T E S T A Z I O N E
239200000000     C*--------------------
239300000000IF  1C     *IN01         IFEQ      *ON
239400000000IF  2C     PRIGIR        IFEQ      '0'
239500000000     C                   MOVEL     '1'           PRIGIR
239600000000X   2C                   ELSE
239700100129     c                   if        parSTAMPA <> 'N'
239800020116     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
239900100129     c                   end
240000000000E   2C                   ENDIF
240100020222     C*
240200020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
240300020222     C                   IF        FLGSTPLE = '1'
240400100129     c                   if        parSTAMPA <> 'N'
240500020222     C                   WRITE     SE94LE                                       *LEGENDA
240600100129     c                   end
240700020222     C                   ELSE
240800020222     C                   EVAL      FLGSTPLE = '1'
240900020222     C                   ENDIF
241000020222     C*
241100100129     c                   if        parSTAMPA <> 'N'
241200020222     C                   WRITE     SE94PT                                       *TESTATA
241300020116     C                   WRITE     SE94IN                                       *INTESTAZIONE
241400020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
241500020116     C                   WRITE     SE94PI                                       *RIGA MESI
241600020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
241700100129     c                   end
241800000000     C                   SETOFF                                       01
241900000000E   1C                   ENDIF
242000020222     C*--------------------
242100000000     C* S P E D I Z I O N I
242200000000     C*--------------------
242300000000     C                   Z-ADD     S1K           $VA
242400020116     C                   Z-ADD     *ZEROS        $VA13C           19 5          *PERIODO CORRENTE
242500000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
242600000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
242700000000E   1C                   ENDDO
242800020116     C                   Z-ADD     *ZEROS        $V213C           19 5          *PERIODO PRECEDENTE
242900951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
243000000000     C***
243100000000     C* CODICE + RAGIONE SOCIALE
243200000000     C***
243300020221     C*
243400000825     C                   MOVEL     DEPCLI        DES07             7            *CODICE CLIENTE
243500000000     C                   MOVEL     DESKSC        DES12            12            *RAGIONE SOCIALE
243600000000     C*
243700000000     C                   MOVEL     *BLANKS       DES20
243800000000     C                   MOVEL     DES07         DES20            20
243900000000     C                   MOVE      DES12         DES20
244000000000     C*
244100000000     C                   MOVEL     *BLANKS       P1DDES
244200000000     C                   MOVEL     DES20         P1DDES
244300000000     C                   MOVE      'SPE'         P1DDES
244400020222     C                   MOVEL     *BLANKS       P1DFLD
244500000000     C*
244600000000     C* SCRIVE I VALORI IN STAMPA
244700000000     C                   EXSR      SCRVAL
244800030311     C*
244900030311     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
245000100406     C*********>>>       EVAL      WFPSAP = P1CVAP
245100100406     C*********>>>       EVAL      WFPSAC = P1CVAT
245200100406     c                   z-add     P1CVAP        aaaSAP           11 0
245300100406     c                   z-add     P1CVAT        aaaSAC           11 0
245400000000     C*--------------------
245500000000     C* R I C A V I
245600000000     C*--------------------
245700020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
245800020116     C*    R1K           DIV       1000          $VA
245900020116     C                   Z-ADD     R1K           $VA
246000020116     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
246100000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
246200020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
246300000000E   1C                   ENDDO
246400020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
246500951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
246600000000     C***
246700000000     C* CLIENTE NUOVO/ACQUISITO + DI TIPO A/B/C
246800000000     C***
246900070822      *?Se il cliente ha spedizioni sia com Italia che come estero ed è NUOVO,
247000070822      *?potrebbe accadere che il mese con la "Y" abbia solo un tipo di spedizioni,
247100070822      *?quindi, se facessimo il test sulla "Y" in una stampa risulterebbe Nuovo e
247200070822      *?nell'altra invece com Acquisito. OCCORRE testare semplicemente se <> da Blk.
247300070822IF  1C**** DESCLN        IFEQ      'Y'                                          *CL.NUOVO SOLO SE "Y
247400070822IF  1C     DESCLN        IFne      *blank                                       *CL.NUOVO SE "Y" o "
247500000000     C                   MOVEL     DRI(17)       DES09             9
247600000000X   1C                   ELSE
247700000000     C                   MOVEL     DRI(18)       DES09             9
247800000000E   1C                   ENDIF
247900000829     C                   EVAL      DES20 =  DES09 +
248000000829     C                                      ' / ' +
248100000829     C                                      DESCLV
248200000000     C                   MOVEL     *BLANKS       P1DDES
248300000000     C                   MOVEL     DES20         P1DDES
248400000000     C                   MOVE      'RIC'         P1DDES
248500020222     C                   MOVEL     *BLANKS       P1DFLD
248600000000     C*
248700000000     C* SCRIVE I VALORI IN STAMPA
248800000000     C                   EXSR      SCRVAL
248900030311     C*
249000030311     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
249100100406     C*********>>>       EVAL      WFPFAP = P1CVAP
249200100406     C*********>>>       EVAL      WFPFAC = P1CVAT
249300100406     c                   z-add     P1CVAP        aaaFAP           15 3
249400100406     c                   z-add     P1CVAT        aaaFAC           15 3
249500000000     C*--------------------
249600000000     C* D E L T A
249700000000     C*--------------------
249800000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
249900000000     C                   Z-ADD     R1K           $RR                            *RICAVI REALI
250000000000     C                   Z-ADD     P1K           $RP                            *RICAVI PRESUNTI
250100000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
250200000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
250300000000     C                   EXSR      IMPCAL
250400970822     C***
250500970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
250600970822     C***
250700970822     C                   MOVEL     D1K           $SD                            *FALG SEGNALAZIONE
250800000000     C***
250900951006     C* CATEGORIA SEAT
251000000000     C***
251100000000     C                   MOVEL     *BLANKS       DES20
251200951006     C                   MOVEL     DESITC        DES20            20
251300000000     C*
251400000000     C                   MOVEL     *BLANKS       P2DDES
251500020221     C                   MOVEL     *BLANKS       P2DFLD
251600000000     C                   MOVEL     DES20         P2DDES
251700000000     C                   MOVE      'DEL'         P2DDES
251800020222     C                   MOVEL     *BLANKS       P2DFLD
251900020701     C*
252000020701     C* VERIFICO LO SCASTAMENTO % RISPETTO ALLO STESSO MESE/PERIODO DELL?ANNO PRECEDENTE
252100020701     C                   EXSR      REPDELFAT
252200000000     C*
252300000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
252400000000     C                   EXSR      SCRDEL
252500030924     C*
252600030924     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
252700100406     C*********>>>       EVAL      WFPDAP = P2CDEP
252800100406     C*********>>>       EVAL      WFPDAC = P2CDET
252900100406     c                   z-add     P2CDEP        aaaDAP            4 1
253000100406     c                   z-add     P2CDET        aaaDAC            4 1
253100030924     C*
253200030924     C* INIZIALIZZO LA SCHIERA DI WRK X SCOSTAMENTO FATTURATO ED I RELATIVI CAMPI DI STAMPA
253300030924     C                   EXSR      INZPDF
253400980406     C*--------------------
253500000828     C* DESCRIZIONE BLOCCO + CLIENTI FIGLI
253600980406     C*--------------------
253700000828     C                   MOVEL     'N'           $STRIG
253800020116     C                   CLEAR                   SE94P3
253900980406IF  1C     DESABL        IFEQ      '8'
254000980406     C     DESCBL        ORNE      *BLANKS
254100980406     C                   MOVEL     DESCBL        P3DDES
254200000828     C                   MOVEL     'S'           $STRIG                         *RIGA DA STAMPARE
254300061220     C                   movel     'S'           Total_Bloccato
254400980406E   1C                   ENDIF
254500000828     C*
254600000829     C                   Z-ADD     *ZEROS        BLOCCO            2 0          *BLOCCO CL STAMPATI
254700000828IF  1C     PARSCE        IFEQ      'U'                                          *STAT. UNIFICANTE
254800000828IF  2C     SKSC11(2)     IFGT      *ZEROS                                       *ESISTONO FIGLI
254900000828     C                   Z-ADD     2             I
255000000828DO  3C     I             DOWLE     500
255100000828     C     SKSC11(I)     ANDGT     *ZEROS
255200000828     C                   Z-ADD     *ZEROS        IS                             *N.CLIENTI X RIGA
255300000828     C                   CLEAR                   SKSC
255400051031     C                   clear                   skbl
255500051031      *
255600000828DO  4C     I             DOWLE     500
255700000828     C     SKSC11(I)     ANDGT     *ZEROS
255800051031     C**** ridotta da 19 a 17 per far spazio ai flags x individuare i codici bloccati
255900051031     C**** IS            ANDLT     19
256000051031     C     IS            ANDLT     17
256100000828     C                   ADD       1             IS                             *CLIENTI NELLA RIGA
256200000828     C                   Z-ADD     SKSC11(I)     SKSC(IS)
256300051031     C*--------------------
256400051031     C* controllo codice cliente bloccato
256500051031     C*--------------------
256600051031     C                   CLEAR                   BS69DS
256700051031     C                   CLEAR                   ACODS
256800051031     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
256900051031     C                   MOVEL     SKSC(IS)      I69KAC                         *CLIENTE X CNACO
257000120125     C                   MOVEL     SKSC(IS)      I69KCP                         *CLIENTE X CNCLP
257100051031     C                   CALL      'TIBS69R'
257200051031     C                   PARM                    BS69DS
257300051031     C                   PARM                    ACODS
257400051031     C                   PARM                    INDDS
257500051031     C                   PARM                    CLPDS
257600051031     C                   PARM                    CLSDS
257700051031IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
257800051031IF  1C     acoabl        andeq     '8'                                          *cliente bloccato
257900051031     C                   move      '+'           skbl(is)
258000120125      **
258100120125      **  Reperisce la Data di quando è stato fatto il Blocco Contabile sul cliente
258200120125     C                   clear                   TrulBLCds
258300120125     c                   eval      IblcTLA = 'L'
258400120125     c                   z-add     acoKcC        IblcKCC
258500120125     c                   z-add     acoKsC        IblcKSC
258600120125     c                   call      'TRULBLCR'
258700120125     C                   PARM                    KPJBA
258800120125     C                   PARM                    TrulBLCds
258900120125      **
259000120125      **  deve memorizzare il più recente blocco effettuato
259100120125      **   codice e data blocco dell'ultimo cliente bloccato fra quelli unificati.
259200120125     c                   if         Data_Blocco_Ultimo < OblcDAV
259300120125     c                   eval           Data_Blocco_Ultimo  = OblcDAV
259400120125     c                   eval           Cod_Blocco_Ultimo   = clpNAR
259500120125     c                   end
259600120125      **
259700051031E   1C                   ENDIF
259800120125      ***
259900120131      ***  riporta le date da 6 a 8 per effettuare il confronto
260000120131     c                   eval      Data_Prima_Fat_8 = 0
260100120131     c                   eval       CLP_Prima_Fat_8 = 0
260200120131      *
260300120131     c                   clear                   Anno2_0           2 0
260400120131     c                   IF        Data_Prima_Fat_6 > 0
260500120131     c                   eval              Anno2_0 = Data_Prima_Fat_6/10000
260600120131     c                   if           Anno2_0 < 40
260700120131     c                   eval      Data_Prima_Fat_8 = 20000000 +
260800120131     c                                            Data_Prima_Fat_6
260900120131     c                   else
261000120131     c                   eval      Data_Prima_Fat_8 = 19000000 +
261100120131     c                                            Data_Prima_Fat_6
261200120131     c                   end
261300120131     c                   endIF
261400120131      *
261500120131     c                   clear                   Anno2_0           2 0
261600120131     c                   IF        CLPdps > 0
261700120131     c                   eval              Anno2_0 = CLPdps/10000
261800120131     c                   if           Anno2_0 < 40
261900120131     c                   eval      CLP_Prima_Fat_8 = 20000000 + CLPdps
262000120131     c                   else
262100120131     c                   eval      CLP_Prima_Fat_8 = 19000000 + CLPdps
262200120131     c                   end
262300120131     c                   endIF
262400120131      *
262500120125      **  deve memorizzare, fra tutti i clienti unificati, la più vecchia data
262600120125      ***   quindi
262700120125      **  la prima spedizione fatturata.
262800120131     C*
262900120131     c                   if             Data_Prima_Fat_8 > CLP_Prima_Fat_8 or
263000120131     c                                  Data_Prima_Fat_8 = 0
263100120213     c                   if                 CLPdps > 0
263200120125     c                   eval        Data_Prima_Fat_6 = CLPdps
263300120125     c                   end
263400120213     c                   end
263500061220      ***
263600061220      *  se almeno un codice figlio non è bloccato non lo deve segnalare come
263700061220      *  totalità.
263800061220     c                   if        skbl(is) <> '+'
263900061220     C                   movel     *blank        Total_Bloccato
264000061220     c                   end
264100061220      ***
264200051031     C*--------------------
264300000828     C                   ADD       1             I                              *CLIENTI
264400000828E   4C                   ENDDO                                                  *ROTTURA BLOCCO
264500000828     C*
264600051031     C* STAMPA BLOCCO DI CLIENTI unificati
264700051031     C*
264800000828IF  4C     IS            IFGT      *ZEROS                                       *CLIENTI DA STAMPARE
264900000828     C     $STRIG        OREQ      'S'                                          *RIGA DA STAMPARE
265000000829     C                   ADD       1             BLOCCO                         INCREMENTA N.BLOCCHI
265100000828     C                   Z-ADD     SKSC(1)       P3CC01
265200000828     C                   Z-ADD     SKSC(2)       P3CC02
265300000828     C                   Z-ADD     SKSC(3)       P3CC03
265400000828     C                   Z-ADD     SKSC(4)       P3CC04
265500000828     C                   Z-ADD     SKSC(5)       P3CC05
265600000828     C                   Z-ADD     SKSC(6)       P3CC06
265700000828     C                   Z-ADD     SKSC(7)       P3CC07
265800000828     C                   Z-ADD     SKSC(8)       P3CC08
265900000828     C                   Z-ADD     SKSC(9)       P3CC09
266000000828     C                   Z-ADD     SKSC(10)      P3CC10
266100000828     C                   Z-ADD     SKSC(11)      P3CC11
266200000828     C                   Z-ADD     SKSC(12)      P3CC12
266300000828     C                   Z-ADD     SKSC(13)      P3CC13
266400000828     C                   Z-ADD     SKSC(14)      P3CC14
266500000828     C                   Z-ADD     SKSC(15)      P3CC15
266600000828     C                   Z-ADD     SKSC(16)      P3CC16
266700000828     C                   Z-ADD     SKSC(17)      P3CC17
266800051031     C******             Z-ADD     SKSC(18)      P3CC18
266900051031     C******             Z-ADD     SKSC(19)      P3CC19
267000051031      **  Aggiunti Flag x cliente bloccato sotto unificante
267100051031      **   x questo ridotta la schiera SKSC per fare spazio in stampa
267200051031     C                   move      skbl(1)       P3Cf01
267300051031     C                   move      skbl(2)       P3Cf02
267400051031     C                   move      skbl(3)       P3Cf03
267500051031     C                   move      skbl(4)       P3Cf04
267600051031     C                   move      skbl(5)       P3Cf05
267700051031     C                   move      skbl(6)       P3Cf06
267800051031     C                   move      skbl(7)       P3Cf07
267900051031     C                   move      skbl(8)       P3Cf08
268000051031     C                   move      skbl(9)       P3Cf09
268100051031     C                   move      skbl(10)      P3Cf10
268200051031     C                   move      skbl(11)      P3Cf11
268300051031     C                   move      skbl(12)      P3Cf12
268400051031     C                   move      skbl(13)      P3Cf13
268500051031     C                   move      skbl(14)      P3Cf14
268600051031     C                   move      skbl(15)      P3Cf15
268700051031     C                   move      skbl(16)      P3Cf16
268800051031     C                   move      skbl(17)      P3Cf17
268900000829IF  5C     BLOCCO        IFEQ      1                                            *1° BLOCCO DA STAMP.
269000000829     C                   MOVEL     DRI(28)       P3DCLI
269100000829E   5C                   ENDIF
269200020131     C*
269300020131     C* PRIMA DI OGNI RIGA DA STAMPARE VERIFICA IL SALTO PAGINA
269400020131IF  1C     *IN01         IFEQ      *ON
269500020131IF  2C     PRIGIR        IFEQ      '0'
269600020131     C                   MOVEL     '1'           PRIGIR
269700020131X   2C                   ELSE
269800100129     c                   if        parSTAMPA <> 'N'
269900020131     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
270000100129     c                   end
270100020131E   2C                   ENDIF
270200020222     C*
270300020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
270400020222     C                   IF        FLGSTPLE = '1'
270500100129     c                   if        parSTAMPA <> 'N'
270600020222     C                   WRITE     SE94LE                                       *LEGENDA
270700100129     c                   end
270800020222     C                   ELSE
270900020222     C                   EVAL      FLGSTPLE = '1'
271000020222     C                   ENDIF
271100020222     C*
271200100129     c                   if        parSTAMPA <> 'N'
271300020222     C                   WRITE     SE94PT                                       *TESTATA
271400020131     C                   WRITE     SE94IN                                       *INTESTAZIONE
271500020131     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
271600020131     C                   WRITE     SE94PI                                       *RIGA MESI
271700020131     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
271800100129     c                   end
271900020131     C                   SETOFF                                       01
272000020131E   1C                   ENDIF
272100020221     C*
272200100129     c                   if        parSTAMPA <> 'N'
272300020116     C                   WRITE     SE94P3
272400100129     c                   end
272500020116     C                   CLEAR                   SE94P3                         AZZERA FORMATO
272600000828     C                   MOVEL     'N'           $STRIG                         RIGA DA NON STAMPARE
272700000828E   4C                   ENDIF
272800000828E   3C                   ENDDO                                                  *FINE FIGLI
272900000828E   2C                   ENDIF
273000000828E   1C                   ENDIF
273100000828     C*
273200000828IF  1C     $STRIG        IFEQ      'S'                                          *ANCORA DA STAMPARE
273300020131     C*
273400020131     C* PRIMA DI OGNI RIGA DA STAMPARE VERIFICA IL SALTO PAGINA
273500020131IF  1C     *IN01         IFEQ      *ON
273600020131IF  2C     PRIGIR        IFEQ      '0'
273700020131     C                   MOVEL     '1'           PRIGIR
273800020131X   2C                   ELSE
273900100129     c                   if        parSTAMPA <> 'N'
274000020131     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
274100100129     c                   end
274200020131E   2C                   ENDIF
274300020222     C*
274400020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
274500020222     C                   IF        FLGSTPLE = '1'
274600100129     c                   if        parSTAMPA <> 'N'
274700020222     C                   WRITE     SE94LE                                       *LEGENDA
274800100129     c                   end
274900020222     C                   ELSE
275000020222     C                   EVAL      FLGSTPLE = '1'
275100020222     C                   ENDIF
275200020222     C*
275300100129     c                   if        parSTAMPA <> 'N'
275400020222     C                   WRITE     SE94PT                                       *TESTATA
275500020131     C                   WRITE     SE94IN                                       *INTESTAZIONE
275600020131     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
275700020131     C                   WRITE     SE94PI                                       *RIGA MESI
275800020131     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
275900100129     c                   end
276000020131     C                   SETOFF                                       01
276100020131E   1C                   ENDIF
276200020221     C*
276300100129     c                   if        parSTAMPA <> 'N'
276400020116     C                   WRITE     SE94P3
276500100129     c                   end
276600000828E   1C                   ENDIF
276700000000     C*--------------------
276800000000     C* R I G A   V U O T A
276900000000     C*--------------------
277000020116     C                   CLEAR                   SE94P1
277100100129     c                   if        parSTAMPA <> 'N'
277200020116     C                   WRITE     SE94P1
277300100129     c                   end
277400000000     C*
277500030311     C* SE RICHIESTO NEL LANBCIO INSERISCO IL RECORD NEL FILE PC (WFDELPCF)
277600030311     C                   IF        PARFPC = 'S'
277700061220     C*
277800061220     C* aggiunto campo di segnalazione cliente totalmente bloccato
277900061220     C*  come se stesso + tutti gli eventuali figli se unificante:
278000100406     c*********>>>       eval      wfpBLO = Total_Bloccato
278100100406     c                   clear                   aaaBLO            1
278200120125     c                   clear                   aaaCULTBLO        3
278300120125     c                   clear                   aaaDULTBLO        8 0
278400120125     c                   clear                   aaaDPRIFAT        8 0
278500100406     c                   eval      aaaBLO = Total_Bloccato
278600120125      *
278700120125      **  solo se il codice unificante è Totalmente bloccato deve riportare
278800120125      **   il codice di blocco e la data dell'ultimo cliente che in ordine di tempo
278900120125      *   è stato bloccato.
279000120125     c                   if        Total_Bloccato = 'S'
279100120125     c                   eval       aaaCULTBLO =  Cod_Blocco_Ultimo
279200120125     c                   eval       aaaDULTBLO = Data_Blocco_Ultimo
279300120125     c                   end
279400120125      *  Inoltre si deve memorizzare quando è stata fatturata la prima spedizione
279500120125      *   fra tutti i clienti unificati.
279600120125     c                   clear                   Anno2_0           2 0
279700120125     c                   IF        Data_Prima_Fat_6 > 0
279800120125     c                   eval      Anno2_0 = Data_Prima_Fat_6/10000
279900120125     c                   if        Anno2_0 < 40
280000120125     c                   eval       aaaDPRIFAT = 20000000 + Data_Prima_Fat_6
280100120125     c                   else
280200120125     c                   eval       aaaDPRIFAT = 19000000 + Data_Prima_Fat_6
280300120125     c                   end
280400120125     c                   endIF
280500061220     C*
280600100511     C**------------>    WRITE     WFDELPC0
280700070418     C*
280800070418      * ?- Scrive il nuovo files WAdelpcf identico al WFdelpcf con i campi in +
280900070418      *              ? *---------------------*
281000070418     c                   exsr      wri_WAdel
281100070418      *              ? *---------------------*
281200100201     C*
281300100201     C*  Solo x Estero x Network
281400100201      * -------------
281500100201IF  1C     PARIOE        IFEQ      'E'
281600100201      *              ? *---------------------*
281700100201     C                   exsr      wriWADELntw
281800100201      *              ? *---------------------*
281900100201     C                   END
282000100201      * -------------
282100070418     C*
282200100216      * ?- Scrive il nuovo files WAdeApcI identico al WFdelpcf con i campi in + x NTW e Tipo Servizio
282300100201      *              ? *---------------------*
282400100201     c                   exsr      wri_WAdeA
282500100201      *              ? *---------------------*
282600100201     C*
282700030311     C                   ENDIF
282800030311     C*
282900000000     C                   ENDSR
283000070418     C*------------------------------------------------------------------------*
283100070418      *?  Scrive il nuovo WFile identico al WFDELPCF ma con informazioni più dettagliate    ?
283200070418     C*------------------------------------------------------------------------*
283300070418     C     wri_WAdel     BEGSR
283400070418     C*
283500070418      * ?Campi uguali al precedente WFile
283600070418     c                   clear                   WAdelPC0
283700100406     c*********>>>       eval      WaPAAD   =    WFPAAD
283800100406     c*********>>>       eval      WaPMMD   =    WFPMMD
283900100406     c*********>>>       eval      WaPAAA   =    WFPAAA
284000100406     c*********>>>       eval      WaPMMA   =    WFPMMA
284100100406     c*********>>>       eval      WaPIOE   =    WFPIOE
284200100406     c*********>>>       eval      WaPDIV   =    WFPDIV
284300100406     c*********>>>       eval      WaPDID   =    WFPDID
284400100406     c*********>>>       eval      WaPARE   =    WFPARE
284500100406     c*********>>>       eval      WaPARD   =    WFPARD
284600100406     c*********>>>       eval      WaPFIL   =    WFPFIL
284700100406     c*********>>>       eval      WaPFID   =    WFPFID
284800100406     c*********>>>       eval      WaPCLV   =    WFPCLV
284900100406     c*********>>>       eval      WaPCLI   =    WFPCLI
285000100406     c*********>>>       eval      WaPCLD   =    WFPCLD
285100100406     c*********>>>       eval      WaPITC   =    WFPITC
285200100406     c*********>>>       eval      WaPCLN   =    WFPCLN
285300100406     c*********>>>       eval      WaPAGE   =    WFPAGE
285400100406     c*********>>>       eval      WaPAGD   =    WFPAGD
285500100406     c*********>>>       eval      WaPSAP   =    WFPSAP
285600100406     c*********>>>       eval      WaPFAP   =    WFPFAP
285700100406     c*********>>>       eval      WaPDAP   =    WFPDAP
285800100406     c*********>>>       eval      WaPSAC   =    WFPSAC
285900100406     c*********>>>       eval      WaPFAC   =    WFPFAC
286000100406     c*********>>>       eval      WaPDAC   =    WFPDAC
286100100406     c*********>>>       eval      WaPBLO   =    WFPBLO
286200100406     c                   eval      WaPAAD   =    aaaAAD
286300100406     c                   eval      WaPMMD   =    aaaMMD
286400100406     c                   eval      WaPAAA   =    aaaAAA
286500100406     c                   eval      WaPMMA   =    aaaMMA
286600100406     c                   eval      WaPIOE   =    aaaIOE
286700100406     c                   eval      WaPDIV   =    aaaDIV
286800100406     c                   eval      WaPDID   =    aaaDID
286900100406     c                   eval      WaPARE   =    aaaARE
287000100406     c                   eval      WaPARD   =    aaaARD
287100100406     c                   eval      WaPFIL   =    aaaFIL
287200100406     c                   eval      WaPFID   =    aaaFID
287300100406     c                   eval      WaPCLV   =    aaaCLV
287400100406     c                   eval      WaPCLI   =    aaaCLI
287500100406     c                   eval      WaPCLD   =    aaaCLD
287600100406     c                   eval      WaPITC   =    aaaITC
287700100406     c                   eval      WaPCLN   =    aaaCLN
287800100406     c                   eval      WaPAGE   =    aaaAGE
287900100406     c                   eval      WaPAGD   =    aaaAGD
288000100406     c                   eval      WaPSAP   =    aaaSAP
288100100406     c                   eval      WaPFAP   =    aaaFAP
288200100406     c                   eval      WaPDAP   =    aaaDAP
288300100406     c                   eval      WaPSAC   =    aaaSAC
288400100406     c                   eval      WaPFAC   =    aaaFAC
288500100406     c                   eval      WaPDAC   =    aaaDAC
288600100406     c                   eval      WaPBLO   =    aaaBLO
288700120125     c                   eval      WApCDULBLO =  aaaCULTBLO
288800120125     c                   eval      WApDTULBLO =  aaaDULTBLO
288900120125     c                   eval      WApDTPRIFA =  aaaDPRIFAT
289000081013      *
289100070418      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
289200070419     C*--------------------
289300070419     C* Anno Precedente 12 Mesi
289400070419     C*--------------------
289500070419     C***    Spedizioni mensili
289600070419     C                   Z-ADD     ap_S1K(1)     wapNS_01aP
289700070419     C                   Z-ADD     ap_S1K(2)     wapNS_02aP
289800070419     C                   Z-ADD     ap_S1K(3)     wapNS_03aP
289900070419     C                   Z-ADD     ap_S1K(4)     wapNS_04aP
290000070419     C                   Z-ADD     ap_S1K(5)     wapNS_05aP
290100070419     C                   Z-ADD     ap_S1K(6)     wapNS_06aP
290200070419     C                   Z-ADD     ap_S1K(7)     wapNS_07aP
290300070419     C                   Z-ADD     ap_S1K(8)     wapNS_08aP
290400070419     C                   Z-ADD     ap_S1K(9)     wapNS_09aP
290500070419     C                   Z-ADD     ap_S1K(10)    wapNS_10aP
290600070419     C                   Z-ADD     ap_S1K(11)    wapNS_11aP
290700070419     C                   Z-ADD     ap_S1K(12)    wapNS_12aP
290800081013     C***    Pesi mensili
290900081016     c                   clear                   WaPkgP
291000081016     c                   add       ap_Kg1K(1)    WaPkgP
291100081016     c                   add       ap_Kg1K(2)    WaPkgP
291200081016     c                   add       ap_Kg1K(3)    WaPkgP
291300081016     c                   add       ap_Kg1K(4)    WaPkgP
291400081016     c                   add       ap_Kg1K(5)    WaPkgP
291500081016     c                   add       ap_Kg1K(6)    WaPkgP
291600081016     c                   add       ap_Kg1K(7)    WaPkgP
291700081016     c                   add       ap_Kg1K(8)    WaPkgP
291800081016     c                   add       ap_Kg1K(9)    WaPkgP
291900081016     c                   add       ap_Kg1K(10)   WaPkgP
292000081016     c                   add       ap_Kg1K(11)   WaPkgP
292100081016     c                   add       ap_Kg1K(12)   WaPkgP
292200081013     C                   Z-ADD     ap_Kg1K(1)    wapKG_01aP
292300081013     C                   Z-ADD     ap_Kg1K(2)    wapKG_02aP
292400081013     C                   Z-ADD     ap_Kg1K(3)    wapKG_03aP
292500081013     C                   Z-ADD     ap_Kg1K(4)    wapKG_04aP
292600081013     C                   Z-ADD     ap_Kg1K(5)    wapKG_05aP
292700081013     C                   Z-ADD     ap_Kg1K(6)    wapKG_06aP
292800081013     C                   Z-ADD     ap_Kg1K(7)    wapKG_07aP
292900081013     C                   Z-ADD     ap_Kg1K(8)    wapKG_08aP
293000081013     C                   Z-ADD     ap_Kg1K(9)    wapKG_09aP
293100081013     C                   Z-ADD     ap_Kg1K(10)   wapKG_10aP
293200081013     C                   Z-ADD     ap_Kg1K(11)   wapKG_11aP
293300081013     C                   Z-ADD     ap_Kg1K(12)   wapKG_12aP
293400070419     C***  Ricavi Reali mensili
293500070419     C                   Z-ADD     ap_R1K(1)     wapRR_01aP
293600070419     C                   Z-ADD     ap_R1K(2)     wapRR_02aP
293700070419     C                   Z-ADD     ap_R1K(3)     wapRR_03aP
293800070419     C                   Z-ADD     ap_R1K(4)     wapRR_04aP
293900070419     C                   Z-ADD     ap_R1K(5)     wapRR_05aP
294000070419     C                   Z-ADD     ap_R1K(6)     wapRR_06aP
294100070419     C                   Z-ADD     ap_R1K(7)     wapRR_07aP
294200070419     C                   Z-ADD     ap_R1K(8)     wapRR_08aP
294300070419     C                   Z-ADD     ap_R1K(9)     wapRR_09aP
294400070419     C                   Z-ADD     ap_R1K(10)    wapRR_10aP
294500070419     C                   Z-ADD     ap_R1K(11)    wapRR_11aP
294600070419     C                   Z-ADD     ap_R1K(12)    wapRR_12aP
294700070419     C***  Ricavi Presunti mensili
294800070419     C                   Z-ADD     ap_P1K(1)     wapRP_01aP
294900070419     C                   Z-ADD     ap_P1K(2)     wapRP_02aP
295000070419     C                   Z-ADD     ap_P1K(3)     wapRP_03aP
295100070419     C                   Z-ADD     ap_P1K(4)     wapRP_04aP
295200070419     C                   Z-ADD     ap_P1K(5)     wapRP_05aP
295300070419     C                   Z-ADD     ap_P1K(6)     wapRP_06aP
295400070419     C                   Z-ADD     ap_P1K(7)     wapRP_07aP
295500070419     C                   Z-ADD     ap_P1K(8)     wapRP_08aP
295600070419     C                   Z-ADD     ap_P1K(9)     wapRP_09aP
295700070419     C                   Z-ADD     ap_P1K(10)    wapRP_10aP
295800070419     C                   Z-ADD     ap_P1K(11)    wapRP_11aP
295900070419     C                   Z-ADD     ap_P1K(12)    wapRP_12aP
296000070419      *
296100070419     C*--------------------
296200070419     C* Anno in Corso 12 Mesi
296300070419     C*--------------------
296400070419     C***    Spedizioni mensili
296500070419     C                   Z-ADD     S1K(1)        wapNS_01aC
296600070419     C                   Z-ADD     S1K(2)        wapNS_02aC
296700070419     C                   Z-ADD     S1K(3)        wapNS_03aC
296800070419     C                   Z-ADD     S1K(4)        wapNS_04aC
296900070419     C                   Z-ADD     S1K(5)        wapNS_05aC
297000070419     C                   Z-ADD     S1K(6)        wapNS_06aC
297100070419     C                   Z-ADD     S1K(7)        wapNS_07aC
297200070419     C                   Z-ADD     S1K(8)        wapNS_08aC
297300070419     C                   Z-ADD     S1K(9)        wapNS_09aC
297400070419     C                   Z-ADD     S1K(10)       wapNS_10aC
297500070419     C                   Z-ADD     S1K(11)       wapNS_11aC
297600070419     C                   Z-ADD     S1K(12)       wapNS_12aC
297700081013     C***    Pesi mensili
297800081016     c                   clear                   WaPkgC
297900081016     c                   add       Kg1K(1)       WaPkgC
298000081016     c                   add       Kg1K(2)       WaPkgC
298100081016     c                   add       Kg1K(3)       WaPkgC
298200081016     c                   add       Kg1K(4)       WaPkgC
298300081016     c                   add       Kg1K(5)       WaPkgC
298400081016     c                   add       Kg1K(6)       WaPkgC
298500081016     c                   add       Kg1K(7)       WaPkgC
298600081016     c                   add       Kg1K(8)       WaPkgC
298700081016     c                   add       Kg1K(9)       WaPkgC
298800081016     c                   add       Kg1K(10)      WaPkgC
298900081016     c                   add       Kg1K(11)      WaPkgC
299000081016     c                   add       Kg1K(12)      WaPkgC
299100081013     C                   Z-ADD     Kg1K(1)       wapKG_01aC
299200081013     C                   Z-ADD     Kg1K(2)       wapKG_02aC
299300081013     C                   Z-ADD     Kg1K(3)       wapKG_03aC
299400081013     C                   Z-ADD     Kg1K(4)       wapKG_04aC
299500081013     C                   Z-ADD     Kg1K(5)       wapKG_05aC
299600081013     C                   Z-ADD     Kg1K(6)       wapKG_06aC
299700081013     C                   Z-ADD     Kg1K(7)       wapKG_07aC
299800081013     C                   Z-ADD     Kg1K(8)       wapKG_08aC
299900081013     C                   Z-ADD     Kg1K(9)       wapKG_09aC
300000081013     C                   Z-ADD     Kg1K(10)      wapKG_10aC
300100081013     C                   Z-ADD     Kg1K(11)      wapKG_11aC
300200081013     C                   Z-ADD     Kg1K(12)      wapKG_12aC
300300070419     C***  Ricavi Reali mensili
300400070419     C                   Z-ADD     R1K(1)        wapRR_01aC
300500070419     C                   Z-ADD     R1K(2)        wapRR_02aC
300600070419     C                   Z-ADD     R1K(3)        wapRR_03aC
300700070419     C                   Z-ADD     R1K(4)        wapRR_04aC
300800070419     C                   Z-ADD     R1K(5)        wapRR_05aC
300900070419     C                   Z-ADD     R1K(6)        wapRR_06aC
301000070419     C                   Z-ADD     R1K(7)        wapRR_07aC
301100070419     C                   Z-ADD     R1K(8)        wapRR_08aC
301200070419     C                   Z-ADD     R1K(9)        wapRR_09aC
301300070419     C                   Z-ADD     R1K(10)       wapRR_10aC
301400070419     C                   Z-ADD     R1K(11)       wapRR_11aC
301500070419     C                   Z-ADD     R1K(12)       wapRR_12aC
301600070419     C***  Ricavi Presunti mensili
301700070419     C                   Z-ADD     P1K(1)        wapRP_01aC
301800070419     C                   Z-ADD     P1K(2)        wapRP_02aC
301900070419     C                   Z-ADD     P1K(3)        wapRP_03aC
302000070419     C                   Z-ADD     P1K(4)        wapRP_04aC
302100070419     C                   Z-ADD     P1K(5)        wapRP_05aC
302200070419     C                   Z-ADD     P1K(6)        wapRP_06aC
302300070419     C                   Z-ADD     P1K(7)        wapRP_07aC
302400070419     C                   Z-ADD     P1K(8)        wapRP_08aC
302500070419     C                   Z-ADD     P1K(9)        wapRP_09aC
302600070419     C                   Z-ADD     P1K(10)       wapRP_10aC
302700070419     C                   Z-ADD     P1K(11)       wapRP_11aC
302800070419     C                   Z-ADD     P1K(12)       wapRP_12aC
302900070419      *
303000070418      * ?Campi in più anagrafici
303100070419     C                   eval      WAPIND   =    DESvia                         *Indirizzo
303200070615     C                   eval      WAPcdf   =    DEScdf                         *cod.fisc
303300070615     C                   eval      WAPiva   =    DESiva                         *P.IVA
303400070618     C                   eval      WAPtel   =    DEStel                         *Telefono
303500070419      * ?CAP Italia
303600070419     c                   if        DESsta = *blank
303700070419     C                   movel(p)  DEScap        WAPCAP                         *CAP
303800070419      * ?CAP Estero
303900070419     c                   else
304000070419     C                   eval      WAPCAP   =    DEScae                         *CAP Europeo
304100070419     c                   end
304200070420     C                   eval      WAPLOC   =    DEScit                         *Città
304300070420     C                   eval      WAPPRV   =    DESprv                         *Provincia
304400070419     C                   eval      WAPNAZ   =    DESsta                         *Nazione
304500070420     C*
304600070420      * ?Resposnabile Trasporto
304700070420     C                   movel     'C'           NTCapl                         *CODICE APPLICAZIONE
304800070420     C                   movel     '0151'        NTCnk1                         *CHIAVE 1 NOTE
304900070420     C                   move      WaPCLI        NTCnk1                         *CHIAVE 1 NOTE
305000070420     C                   movel     *blank        NTCnk2                         *CHIAVE 2 NOTE
305100070420     C                   movel     '05'          NTCtnt                         *TIPO NOTA
305200070420     C     KeyNTC        chain     tfNTC01l
305300070420     c                   if        %Found(tfNTC01l)
305400070420     c                   movel     NTCRNT        WAPRESP
305500070420     c                   end
305600070426     C* ?E-mail Resposnabile Trasporto
305700070426     C                   movel     '06'          NTCtnt                         *TIPO NOTA
305800070426     C     KeyNTC        chain     tfNTC01l
305900070426     c                   if        %Found(tfNTC01l)
306000070426     c                   movel     NTCRNT        WAPEMAR
306100070426     c                   end
306200070426     C*
306300071212     C* ?Mese di Inizio Fatturato x Cliente Nuovo
306400071212     c                   clear                   WapNew_Mes
306500071212     c                   if        WaPCLN = 'N'
306600071212     c                   do        12            mes_k             3 0
306700071212     c                   if        s1k(mes_k) <> 0 or
306800081013     c                             kg1k(mes_k) <> 0 or
306900081013     c                             r1k(mes_k) <> 0 or
307000071212     c                             p1k(mes_k) <> 0
307100121217      *
307200121217      * Deve leggere l'ultimo a causa dei mesi antecedenti al vero e proprio inizio del cliente
307300121217      *  (questo è dovuto a storni o note di credito attribuite al cliente spalmate anche su
307400121217      *   mesi antecedenti il suo inizio di fatturazione).
307500121217     c                   if        cln1k(mes_k) = 'Y'
307600071212     c                   z-add     mes_k         WapNew_Mes
307700121217     c******                leave
307800121217     c                   else
307900121217      *   se prima aveva impostato il campo lo fa uscire immediatamente
308000121217      *   ma solo se prima era passato per gli elementi con "Y"
308100121217     c                   if          WapNew_Mes > 0
308200121217     c                   leave
308300121217     c                   end
308400121217     c                   end
308500121217      *
308600071212     c                   endIF
308700071212     c                   endDO
308800071212     c                   end
308900071212     C*
309000081013      * ?Campi aggiunti: AGE Unificante e Descrizione
309100081013     C                   eval      WAPaud   =    depRGD                         *Descr.Unificante
309200081013     C                   eval      WAPauc   =    depRGF                         *Cod.  Unificante
309300081013     C*
309400070418     C                   WRITE     WADELPC0
309500091123     C*
309600070418     C                   ENDSR
309700000000     C*------------------------------------------------------------------------*
309800091123     C*  Scrive i record del WADEL x Estero in base al Network e la qualità
309900091123     C*------------------------------------------------------------------------*
310000091123     C     wriWADELntw   Begsr
310100091123     C*
310200091124     C* riporta I TOTALI x NETWORK
310300091124DO  1C     1             DO        5             K
310400091124     C*
310500100202     C     K             OCCUR     W1ntw
310600100201     C*
310700091124     C* Se non ci sono più Network esce dal ciclo
310800091124     c                   if        c1Ntw    = *blank  and
310900091124     c                             ap_c1Ntw = *blank
311000091124     c                   iter
311100091124     c                   end
311200091124     C*
311300091124      * ?Deve scrivere tanti records quanti sono i dati dei vari Network
311400091124     c                   clear                   WAdel00N
311500091124     C*
311600091124     c                   eval      WaNAAD   =    WaPAAD
311700091124     c                   eval      WaNMMD   =    WaPMMD
311800091124     c                   eval      WaNAAA   =    WaPAAA
311900091124     c                   eval      WaNMMA   =    WaPMMA
312000091124     c                   eval      WaNIOE   =    WaPIOE
312100091124     c                   eval      WaNDIV   =    WaPDIV
312200091124     c                   eval      WaNDID   =    WaPDID
312300091124     c                   eval      WaNARE   =    WaPARE
312400091124     c                   eval      WaNARD   =    WaPARD
312500091124     c                   eval      WaNFIL   =    WaPFIL
312600091124     c                   eval      WaNFID   =    WaPFID
312700091124     c                   eval      WaNCLV   =    WaPCLV
312800091124     c                   eval      WaNCLI   =    WaPCLI
312900091124     c                   eval      WaNCLD   =    WaPCLD
313000091124     c                   eval      WaNITC   =    WaPITC
313100091124     c                   eval      WaNCLN   =    WaPCLN
313200091124     c                   eval      WaNAGE   =    WaPAGE
313300091124     c                   eval      WaNAGD   =    WaPAGD
313400091124     c                   eval      WaNBLO   =    WaPBLO
313500120125     c                   eval      WAnCDULBLO =  WaPCDULBLO
313600120125     c                   eval      WAnDTULBLO =  WaPDTULBLO
313700120125     c                   eval      WAnDTPRIFA =  WaPDTPRIFA
313800091124      * ?Campi in più anagrafici
313900091124     C                   eval      WAnIND   =    WaPIND                         *Indirizzo
314000091124     C                   eval      WAncdf   =    WaPcdf                         *cod.fisc
314100091124     C                   eval      WAniva   =    WaPiva                         *P.IVA
314200091124     C                   eval      WAntel   =    WaPtel                         *Telefono
314300091124     C                   eval      WAnCAP   =    WaPCAP                         *CAP Europeo
314400091124     C                   eval      WAnLOC   =    WaPLOC                         *Città
314500091124     C                   eval      WAnPRV   =    WaPPRV                         *Provincia
314600091124     C                   eval      WAnNAZ   =    WaPNAZ                         *Nazione
314700091124     c                   eval      WAnRESP  =    WaPRESP
314800091124     c                   eval      WAnEMAR  =    WaPEMAR
314900091124     c                   eval      WanNew_Mes =  WaPNew_Mes
315000091124     C                   eval      WAnaud   =    WaPaud                         *Descr.Unificante
315100091124     C                   eval      WAnauc   =    WaPauc                         *Cod.  Unificante
315200091123      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
315300091124     c                   eval      WaNntw   =    c1NTW
315400091124     c                   if          WaNntw = *blank
315500091124     c                   eval      WaNntw   =    ap_c1NTW
315600091124     c                   end
315700091124     C***
315800091124     c                   xfoot     ap_S1ntw      WaNSAP
315900091124     c                   xfoot     S1ntw         WaNSAC
316000091124     C***
316100091125     c**********         z-add     ap_tR1ntw     WaNFAP
316200091125     c**********         z-add     ap_tP1ntw     WaNPAP
316300091125     c**********         z-add     tR1ntw        WaNFAC
316400091125     c**********         z-add     tP1ntw        WaNPAC
316500091125     c                   xfoot     ap_R1ntw      WaNFAP
316600091125     c                   xfoot     ap_P1ntw      WaNPAP
316700091125     c                   xfoot     R1ntw         WaNFAC
316800091125     c                   xfoot     P1ntw         WaNPAC
316900091124      ***
317000091124     c***                eval      WaNDAP   =    WaPDAP
317100091124     c***                eval      WaNDAC   =    WaPDAC
317200091124      ***
317300091124     C                   Z-ADD     WaNFAP        WRRW             19 5          *RICAVO REALE
317400091124     C                   Z-ADD     WaNPAP        WRPW             19 5          *RICAVO PRESUNTO
317500091124     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
317600091124     C                   EXSR      CALDEL
317700091124     C                   Z-ADD     WDPW          WaNDAP                         *DELTA
317800091124      ***
317900091124     C                   Z-ADD     WaNFAC        WRRW             19 5          *RICAVO REALE
318000091124     C                   Z-ADD     WaNPAC        WRPW             19 5          *RICAVO PRESUNTO
318100091124     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
318200091124     C                   EXSR      CALDEL
318300091124     C                   Z-ADD     WDPW          WaNDAC                         *DELTA
318400091123     C*--------------------
318500091123     C* Anno Precedente 12 Mesi
318600091123     C*--------------------
318700091123     C***    Spedizioni mensili
318800091124     C                   Z-ADD     ap_S1ntw(1)   wanNS_01aP
318900091124     C                   Z-ADD     ap_S1ntw(2)   wanNS_02aP
319000091124     C                   Z-ADD     ap_S1ntw(3)   wanNS_03aP
319100091124     C                   Z-ADD     ap_S1ntw(4)   wanNS_04aP
319200091124     C                   Z-ADD     ap_S1ntw(5)   wanNS_05aP
319300091124     C                   Z-ADD     ap_S1ntw(6)   wanNS_06aP
319400091124     C                   Z-ADD     ap_S1ntw(7)   wanNS_07aP
319500091124     C                   Z-ADD     ap_S1ntw(8)   wanNS_08aP
319600091124     C                   Z-ADD     ap_S1ntw(9)   wanNS_09aP
319700091124     C                   Z-ADD     ap_S1ntw(10)  wanNS_10aP
319800091124     C                   Z-ADD     ap_S1ntw(11)  wanNS_11aP
319900091124     C                   Z-ADD     ap_S1ntw(12)  wanNS_12aP
320000091123     C***    Pesi mensili
320100091124     c                   clear                   WankgP
320200091124     c                   add       ap_Kg1ntw(1)  WankgP
320300091124     c                   add       ap_Kg1ntw(2)  WankgP
320400091124     c                   add       ap_Kg1ntw(3)  WankgP
320500091124     c                   add       ap_Kg1ntw(4)  WankgP
320600091124     c                   add       ap_Kg1ntw(5)  WankgP
320700091124     c                   add       ap_Kg1ntw(6)  WankgP
320800091124     c                   add       ap_Kg1ntw(7)  WankgP
320900091124     c                   add       ap_Kg1ntw(8)  WankgP
321000091124     c                   add       ap_Kg1ntw(9)  WankgP
321100091124     c                   add       ap_Kg1ntw(10) WankgP
321200091124     c                   add       ap_Kg1ntw(11) WankgP
321300091124     c                   add       ap_Kg1ntw(12) WankgP
321400091124     C                   Z-ADD     ap_Kg1ntw(1)  wanKG_01aP
321500091124     C                   Z-ADD     ap_Kg1ntw(2)  wanKG_02aP
321600091124     C                   Z-ADD     ap_Kg1ntw(3)  wanKG_03aP
321700091124     C                   Z-ADD     ap_Kg1ntw(4)  wanKG_04aP
321800091124     C                   Z-ADD     ap_Kg1ntw(5)  wanKG_05aP
321900091124     C                   Z-ADD     ap_Kg1ntw(6)  wanKG_06aP
322000091124     C                   Z-ADD     ap_Kg1ntw(7)  wanKG_07aP
322100091124     C                   Z-ADD     ap_Kg1ntw(8)  wanKG_08aP
322200091124     C                   Z-ADD     ap_Kg1ntw(9)  wanKG_09aP
322300091124     C                   Z-ADD     ap_Kg1ntw(10) wanKG_10aP
322400091124     C                   Z-ADD     ap_Kg1ntw(11) wanKG_11aP
322500091124     C                   Z-ADD     ap_Kg1ntw(12) wanKG_12aP
322600091123     C***  Ricavi Reali mensili
322700091124     C                   Z-ADD     ap_R1ntw(1)   wanRR_01aP
322800091124     C                   Z-ADD     ap_R1ntw(2)   wanRR_02aP
322900091124     C                   Z-ADD     ap_R1ntw(3)   wanRR_03aP
323000091124     C                   Z-ADD     ap_R1ntw(4)   wanRR_04aP
323100091124     C                   Z-ADD     ap_R1ntw(5)   wanRR_05aP
323200091124     C                   Z-ADD     ap_R1ntw(6)   wanRR_06aP
323300091124     C                   Z-ADD     ap_R1ntw(7)   wanRR_07aP
323400091124     C                   Z-ADD     ap_R1ntw(8)   wanRR_08aP
323500091124     C                   Z-ADD     ap_R1ntw(9)   wanRR_09aP
323600091124     C                   Z-ADD     ap_R1ntw(10)  wanRR_10aP
323700091124     C                   Z-ADD     ap_R1ntw(11)  wanRR_11aP
323800091124     C                   Z-ADD     ap_R1ntw(12)  wanRR_12aP
323900091123     C***  Ricavi Presunti mensili
324000091124     C                   Z-ADD     ap_P1ntw(1)   wanRP_01aP
324100091124     C                   Z-ADD     ap_P1ntw(2)   wanRP_02aP
324200091124     C                   Z-ADD     ap_P1ntw(3)   wanRP_03aP
324300091124     C                   Z-ADD     ap_P1ntw(4)   wanRP_04aP
324400091124     C                   Z-ADD     ap_P1ntw(5)   wanRP_05aP
324500091124     C                   Z-ADD     ap_P1ntw(6)   wanRP_06aP
324600091124     C                   Z-ADD     ap_P1ntw(7)   wanRP_07aP
324700091124     C                   Z-ADD     ap_P1ntw(8)   wanRP_08aP
324800091124     C                   Z-ADD     ap_P1ntw(9)   wanRP_09aP
324900091124     C                   Z-ADD     ap_P1ntw(10)  wanRP_10aP
325000091124     C                   Z-ADD     ap_P1ntw(11)  wanRP_11aP
325100091124     C                   Z-ADD     ap_P1ntw(12)  wanRP_12aP
325200091123     C*--------------------
325300091123     C* Anno in Corso 12 Mesi
325400091123     C*--------------------
325500091123     C***    Spedizioni mensili
325600091124     C                   Z-ADD     S1ntw(1)      wanNS_01aC
325700091124     C                   Z-ADD     S1ntw(2)      wanNS_02aC
325800091124     C                   Z-ADD     S1ntw(3)      wanNS_03aC
325900091124     C                   Z-ADD     S1ntw(4)      wanNS_04aC
326000091124     C                   Z-ADD     S1ntw(5)      wanNS_05aC
326100091124     C                   Z-ADD     S1ntw(6)      wanNS_06aC
326200091124     C                   Z-ADD     S1ntw(7)      wanNS_07aC
326300091124     C                   Z-ADD     S1ntw(8)      wanNS_08aC
326400091124     C                   Z-ADD     S1ntw(9)      wanNS_09aC
326500091124     C                   Z-ADD     S1ntw(10)     wanNS_10aC
326600091124     C                   Z-ADD     S1ntw(11)     wanNS_11aC
326700091124     C                   Z-ADD     S1ntw(12)     wanNS_12aC
326800091123     C***    Pesi mensili
326900091124     c                   clear                   WankgC
327000091124     c                   add       Kg1ntw(1)     WankgC
327100091124     c                   add       Kg1ntw(2)     WankgC
327200091124     c                   add       Kg1ntw(3)     WankgC
327300091124     c                   add       Kg1ntw(4)     WankgC
327400091124     c                   add       Kg1ntw(5)     WankgC
327500091124     c                   add       Kg1ntw(6)     WankgC
327600091124     c                   add       Kg1ntw(7)     WankgC
327700091124     c                   add       Kg1ntw(8)     WankgC
327800091124     c                   add       Kg1ntw(9)     WankgC
327900091124     c                   add       Kg1ntw(10)    WankgC
328000091124     c                   add       Kg1ntw(11)    WankgC
328100091124     c                   add       Kg1ntw(12)    WankgC
328200091124     C                   Z-ADD     Kg1ntw(1)     wanKG_01aC
328300091124     C                   Z-ADD     Kg1ntw(2)     wanKG_02aC
328400091124     C                   Z-ADD     Kg1ntw(3)     wanKG_03aC
328500091124     C                   Z-ADD     Kg1ntw(4)     wanKG_04aC
328600091124     C                   Z-ADD     Kg1ntw(5)     wanKG_05aC
328700091124     C                   Z-ADD     Kg1ntw(6)     wanKG_06aC
328800091124     C                   Z-ADD     Kg1ntw(7)     wanKG_07aC
328900091124     C                   Z-ADD     Kg1ntw(8)     wanKG_08aC
329000091124     C                   Z-ADD     Kg1ntw(9)     wanKG_09aC
329100091124     C                   Z-ADD     Kg1ntw(10)    wanKG_10aC
329200091124     C                   Z-ADD     Kg1ntw(11)    wanKG_11aC
329300091124     C                   Z-ADD     Kg1ntw(12)    wanKG_12aC
329400091123     C***  Ricavi Reali mensili
329500091124     C                   Z-ADD     R1ntw(1)      wanRR_01aC
329600091124     C                   Z-ADD     R1ntw(2)      wanRR_02aC
329700091124     C                   Z-ADD     R1ntw(3)      wanRR_03aC
329800091124     C                   Z-ADD     R1ntw(4)      wanRR_04aC
329900091124     C                   Z-ADD     R1ntw(5)      wanRR_05aC
330000091124     C                   Z-ADD     R1ntw(6)      wanRR_06aC
330100091124     C                   Z-ADD     R1ntw(7)      wanRR_07aC
330200091124     C                   Z-ADD     R1ntw(8)      wanRR_08aC
330300091124     C                   Z-ADD     R1ntw(9)      wanRR_09aC
330400091124     C                   Z-ADD     R1ntw(10)     wanRR_10aC
330500091124     C                   Z-ADD     R1ntw(11)     wanRR_11aC
330600091124     C                   Z-ADD     R1ntw(12)     wanRR_12aC
330700091123     C***  Ricavi Presunti mensili
330800091124     C                   Z-ADD     P1ntw(1)      wanRP_01aC
330900091124     C                   Z-ADD     P1ntw(2)      wanRP_02aC
331000091124     C                   Z-ADD     P1ntw(3)      wanRP_03aC
331100091124     C                   Z-ADD     P1ntw(4)      wanRP_04aC
331200091124     C                   Z-ADD     P1ntw(5)      wanRP_05aC
331300091124     C                   Z-ADD     P1ntw(6)      wanRP_06aC
331400091124     C                   Z-ADD     P1ntw(7)      wanRP_07aC
331500091124     C                   Z-ADD     P1ntw(8)      wanRP_08aC
331600091124     C                   Z-ADD     P1ntw(9)      wanRP_09aC
331700091124     C                   Z-ADD     P1ntw(10)     wanRP_10aC
331800091124     C                   Z-ADD     P1ntw(11)     wanRP_11aC
331900091124     C                   Z-ADD     P1ntw(12)     wanRP_12aC
332000091124      *
332100091124     C                   WRITE     WADEL00n
332200091124     C                   ENDdo
332300091123     C*
332400091123     C                   ENDSR
332500100201      * ?----------------------------------------------------------------------
332600100201      *?  Scrive il nuovo WFile identico al WFDELPCF ma con informazioni in più   ?
332700100201      *?    NETWORK e TIPO SERVIZIO con anche DETTAGLIO VARIE "e" / "h"           ?
332800100201      * ?----------------------------------------------------------------------
332900100201     C     wri_WAdeA     BEGSR
333000100201      *
333100100201     c                   eval      numElem = %Elem(W1ntwTS)
333200100201     c**
333300100201      **  Quindi se da totalizzare cerca su quale occurence deve metterlo.
333400100201     c                   do        NUMELEM       k
333500100201     C*
333600100202     C*  Attenzione alle schiere fra Anno in corso e Anno precedente
333700100202      * ?Deve scrivere tanti records quanti sono i dati dei vari Network/TServizi
333800100202     C     K             OCCUR     W1ntwTS
333900100201     C*
334000100204     C* Se non ci sono più Network/tipi servizio esce dal ciclo
334100100204     c                   if        c1NtwTS  = *blank  and c1tpsTS = *blank
334200100202     c                   leave
334300100201     c                   end
334400100201     C*
334500100201      * ?Deve scrivere tanti records quanti sono i dati dei vari Network
334600100201     c                   clear                   WAdeAPC0
334700100201     C*
334800100201     c                   eval      WadAAD   =    WaPAAD
334900100201     c                   eval      WadMMD   =    WaPMMD
335000100201     c                   eval      WadAAA   =    WaPAAA
335100100201     c                   eval      WadMMA   =    WaPMMA
335200100201     c                   eval      WadIOE   =    WaPIOE
335300100201     c                   eval      WadDIV   =    WaPDIV
335400100201     c                   eval      WadDID   =    WaPDID
335500100201     c                   eval      WadARE   =    WaPARE
335600100201     c                   eval      WadARD   =    WaPARD
335700100201     c                   eval      WadFIL   =    WaPFIL
335800100201     c                   eval      WadFID   =    WaPFID
335900100201     c                   eval      WadCLV   =    WaPCLV
336000100201     c                   eval      WadCLI   =    WaPCLI
336100100201     c                   eval      WadCLD   =    WaPCLD
336200100201     c                   eval      WadITC   =    WaPITC
336300100201     c                   eval      WadCLN   =    WaPCLN
336400100201     c                   eval      WadAGE   =    WaPAGE
336500100201     c                   eval      WadAGD   =    WaPAGD
336600100201     c                   eval      WadBLO   =    WaPBLO
336700120125     c                   eval      WAdCDULBLO =  WaPCDULBLO
336800120125     c                   eval      WAdDTULBLO =  WaPDTULBLO
336900120125     c                   eval      WAdDTPRIFA =  WaPDTPRIFA
337000100201      * ?Campi in più anagrafici
337100100201     C                   eval      WAdIND   =    WaPIND                         *Indirizzo
337200100201     C                   eval      WAdcdf   =    WaPcdf                         *cod.fisc
337300100201     C                   eval      WAdiva   =    WaPiva                         *P.IVA
337400100201     C                   eval      WAdtel   =    WaPtel                         *Telefono
337500100201     C                   eval      WAdCAP   =    WaPCAP                         *CAP Europeo
337600100201     C                   eval      WAdLOC   =    WaPLOC                         *Città
337700100201     C                   eval      WAdPRV   =    WaPPRV                         *Provincia
337800100201     C                   eval      WAdNAZ   =    WaPNAZ                         *Nazione
337900100201     c                   eval      WAdRESP  =    WaPRESP
338000100201     c                   eval      WAdEMAR  =    WaPEMAR
338100100201     c                   eval      WadNew_Mes =  WaPNew_Mes
338200100201     C                   eval      WAdaud   =    WaPaud                         *Descr.Unificante
338300100201     C                   eval      WAdauc   =    WaPauc                         *Cod.  Unificante
338400100201      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
338500100215     c                   if        c1ntwTS  = 'ITA'
338600100215     c                   eval      Wadntw   =    *blank
338700100215     c                   else
338800100202     c                   eval      Wadntw   =    c1ntwTS
338900100215     c                   end
339000100215     c                   eval      WadTSP   =    c1tpsTS
339100100201     C***
339200100202     c                   xfoot     ap_S1ntwTS    WadSAP
339300100202     c                   xfoot     S1ntwTS       WadSAC
339400100201     C***
339500100202     c                   xfoot     ap_R1ntwTS    WadFAP
339600100202     c                   xfoot     ap_P1ntwTS    WadPAP
339700100202     c                   xfoot     R1ntwTS       WadFAC
339800100202     c                   xfoot     P1ntwTS       WadPAC
339900100201      ***
340000100201     C                   Z-ADD     WadFAP        WRRW             19 5          *RICAVO REALE
340100100201     C                   Z-ADD     WadPAP        WRPW             19 5          *RICAVO PRESUNTO
340200100201     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
340300100201     C                   EXSR      CALDEL
340400100201     C                   Z-ADD     WDPW          WaDDAP                         *DELTA
340500100201      ***
340600100201     C                   Z-ADD     WaDFAC        WRRW             19 5          *RICAVO REALE
340700100201     C                   Z-ADD     WaDPAC        WRPW             19 5          *RICAVO PRESUNTO
340800100201     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
340900100201     C                   EXSR      CALDEL
341000100201     C                   Z-ADD     WDPW          WaDDAC                         *DELTA
341100100201     C*--------------------
341200100201     C* Anno Precedente 12 Mesi
341300100201     C*--------------------
341400100201     C***    Spedizioni mensili
341500100202     C                   Z-ADD     ap_S1ntwTS(1) wadNS_01aP
341600100202     C                   Z-ADD     ap_S1ntwTS(2) wadNS_02aP
341700100202     C                   Z-ADD     ap_S1ntwTS(3) wadNS_03aP
341800100202     C                   Z-ADD     ap_S1ntwTS(4) wadNS_04aP
341900100202     C                   Z-ADD     ap_S1ntwTS(5) wadNS_05aP
342000100202     C                   Z-ADD     ap_S1ntwTS(6) wadNS_06aP
342100100202     C                   Z-ADD     ap_S1ntwTS(7) wadNS_07aP
342200100202     C                   Z-ADD     ap_S1ntwTS(8) wadNS_08aP
342300100202     C                   Z-ADD     ap_S1ntwTS(9) wadNS_09aP
342400100202     C                   Z-ADD     ap_S1ntwTS(10)wadNS_10aP
342500100202     C                   Z-ADD     ap_S1ntwTS(11)wadNS_11aP
342600100202     C                   Z-ADD     ap_S1ntwTS(12)wadNS_12aP
342700100201     C***    Pesi mensili
342800100201     c                   clear                   WadkgP
342900100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(1)
343000100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(2)
343100100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(3)
343200100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(4)
343300100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(5)
343400100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(6)
343500100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(7)
343600100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(8)
343700100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(9)
343800100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(10)
343900100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(11)
344000100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(12)
344100100202     C                   Z-ADD     ap_Kg1ntwTS(1)wadKG_01aP
344200100202     C                   Z-ADD     ap_Kg1ntwTS(2)wadKG_02aP
344300100202     C                   Z-ADD     ap_Kg1ntwTS(3)wadKG_03aP
344400100202     C                   Z-ADD     ap_Kg1ntwTS(4)wadKG_04aP
344500100202     C                   Z-ADD     ap_Kg1ntwTS(5)wadKG_05aP
344600100202     C                   Z-ADD     ap_Kg1ntwTS(6)wadKG_06aP
344700100202     C                   Z-ADD     ap_Kg1ntwTS(7)wadKG_07aP
344800100202     C                   Z-ADD     ap_Kg1ntwTS(8)wadKG_08aP
344900100202     C                   Z-ADD     ap_Kg1ntwTS(9)wadKG_09aP
345000100202     c                   eval      wadKG_01aP = ap_Kg1ntwTS(1)
345100100202     c                   eval      wadKG_02aP = ap_Kg1ntwTS(2)
345200100202     c                   eval      wadKG_03aP = ap_Kg1ntwTS(3)
345300100202     c                   eval      wadKG_04aP = ap_Kg1ntwTS(4)
345400100202     c                   eval      wadKG_05aP = ap_Kg1ntwTS(5)
345500100202     c                   eval      wadKG_06aP = ap_Kg1ntwTS(6)
345600100202     c                   eval      wadKG_07aP = ap_Kg1ntwTS(7)
345700100202     c                   eval      wadKG_08aP = ap_Kg1ntwTS(8)
345800100202     c                   eval      wadKG_09aP = ap_Kg1ntwTS(9)
345900100202     c                   eval      wadKG_10aP = ap_Kg1ntwTS(10)
346000100202     c                   eval      wadKG_11aP = ap_Kg1ntwTS(11)
346100100202     c                   eval      wadKG_12aP = ap_Kg1ntwTS(12)
346200100201     C***  Ricavi Reali mensili
346300100202     C                   Z-ADD     ap_R1ntwTS(1) wadRR_01aP
346400100202     C                   Z-ADD     ap_R1ntwTS(2) wadRR_02aP
346500100202     C                   Z-ADD     ap_R1ntwTS(3) wadRR_03aP
346600100202     C                   Z-ADD     ap_R1ntwTS(4) wadRR_04aP
346700100202     C                   Z-ADD     ap_R1ntwTS(5) wadRR_05aP
346800100202     C                   Z-ADD     ap_R1ntwTS(6) wadRR_06aP
346900100202     C                   Z-ADD     ap_R1ntwTS(7) wadRR_07aP
347000100202     C                   Z-ADD     ap_R1ntwTS(8) wadRR_08aP
347100100202     C                   Z-ADD     ap_R1ntwTS(9) wadRR_09aP
347200100202     C                   Z-ADD     ap_R1ntwTS(10)wadRR_10aP
347300100202     C                   Z-ADD     ap_R1ntwTS(11)wadRR_11aP
347400100202     C                   Z-ADD     ap_R1ntwTS(12)wadRR_12aP
347500100201     C***  Ricavi Presunti mensili
347600100202     C                   Z-ADD     ap_P1ntwTS(1) wadRP_01aP
347700100202     C                   Z-ADD     ap_P1ntwTS(2) wadRP_02aP
347800100202     C                   Z-ADD     ap_P1ntwTS(3) wadRP_03aP
347900100202     C                   Z-ADD     ap_P1ntwTS(4) wadRP_04aP
348000100202     C                   Z-ADD     ap_P1ntwTS(5) wadRP_05aP
348100100202     C                   Z-ADD     ap_P1ntwTS(6) wadRP_06aP
348200100202     C                   Z-ADD     ap_P1ntwTS(7) wadRP_07aP
348300100202     C                   Z-ADD     ap_P1ntwTS(8) wadRP_08aP
348400100202     C                   Z-ADD     ap_P1ntwTS(9) wadRP_09aP
348500100202     C                   Z-ADD     ap_P1ntwTS(10)wadRP_10aP
348600100202     C                   Z-ADD     ap_P1ntwTS(11)wadRP_11aP
348700100202     C                   Z-ADD     ap_P1ntwTS(12)wadRP_12aP
348800100201     C*--------------------
348900100201     C* Anno in Corso 12 Mesi
349000100201     C*--------------------
349100100201     C***    Spedizioni mensili
349200100202     C                   Z-ADD     S1ntwTS(1)    wadNS_01aC
349300100202     C                   Z-ADD     S1ntwTS(2)    wadNS_02aC
349400100202     C                   Z-ADD     S1ntwTS(3)    wadNS_03aC
349500100202     C                   Z-ADD     S1ntwTS(4)    wadNS_04aC
349600100202     C                   Z-ADD     S1ntwTS(5)    wadNS_05aC
349700100202     C                   Z-ADD     S1ntwTS(6)    wadNS_06aC
349800100202     C                   Z-ADD     S1ntwTS(7)    wadNS_07aC
349900100202     C                   Z-ADD     S1ntwTS(8)    wadNS_08aC
350000100202     C                   Z-ADD     S1ntwTS(9)    wadNS_09aC
350100100202     C                   Z-ADD     S1ntwTS(10)   wadNS_10aC
350200100202     C                   Z-ADD     S1ntwTS(11)   wadNS_11aC
350300100202     C                   Z-ADD     S1ntwTS(12)   wadNS_12aC
350400100201     C***    Pesi mensili
350500100201     c                   clear                   WadkgC
350600100202     c                   add       Kg1ntwTS(1)   WadkgC
350700100202     c                   add       Kg1ntwTS(2)   WadkgC
350800100202     c                   add       Kg1ntwTS(3)   WadkgC
350900100202     c                   add       Kg1ntwTS(4)   WadkgC
351000100202     c                   add       Kg1ntwTS(5)   WadkgC
351100100202     c                   add       Kg1ntwTS(6)   WadkgC
351200100202     c                   add       Kg1ntwTS(7)   WadkgC
351300100202     c                   add       Kg1ntwTS(8)   WadkgC
351400100202     c                   add       Kg1ntwTS(9)   WadkgC
351500100202     c                   add       Kg1ntwTS(10)  WadkgC
351600100202     c                   add       Kg1ntwTS(11)  WadkgC
351700100202     c                   add       Kg1ntwTS(12)  WadkgC
351800100202     C                   Z-ADD     Kg1ntwTS(1)   wadKG_01aC
351900100202     C                   Z-ADD     Kg1ntwTS(2)   wadKG_02aC
352000100202     C                   Z-ADD     Kg1ntwTS(3)   wadKG_03aC
352100100202     C                   Z-ADD     Kg1ntwTS(4)   wadKG_04aC
352200100202     C                   Z-ADD     Kg1ntwTS(5)   wadKG_05aC
352300100202     C                   Z-ADD     Kg1ntwTS(6)   wadKG_06aC
352400100202     C                   Z-ADD     Kg1ntwTS(7)   wadKG_07aC
352500100202     C                   Z-ADD     Kg1ntwTS(8)   wadKG_08aC
352600100202     C                   Z-ADD     Kg1ntwTS(9)   wadKG_09aC
352700100202     C                   Z-ADD     Kg1ntwTS(10)  wadKG_10aC
352800100202     C                   Z-ADD     Kg1ntwTS(11)  wadKG_11aC
352900100202     C                   Z-ADD     Kg1ntwTS(12)  wadKG_12aC
353000100201     C***  Ricavi Reali mensili
353100100202     C                   Z-ADD     R1ntwTS(1)    wadRR_01aC
353200100202     C                   Z-ADD     R1ntwTS(2)    wadRR_02aC
353300100202     C                   Z-ADD     R1ntwTS(3)    wadRR_03aC
353400100202     C                   Z-ADD     R1ntwTS(4)    wadRR_04aC
353500100202     C                   Z-ADD     R1ntwTS(5)    wadRR_05aC
353600100202     C                   Z-ADD     R1ntwTS(6)    wadRR_06aC
353700100202     C                   Z-ADD     R1ntwTS(7)    wadRR_07aC
353800100202     C                   Z-ADD     R1ntwTS(8)    wadRR_08aC
353900100202     C                   Z-ADD     R1ntwTS(9)    wadRR_09aC
354000100202     C                   Z-ADD     R1ntwTS(10)   wadRR_10aC
354100100202     C                   Z-ADD     R1ntwTS(11)   wadRR_11aC
354200100202     C                   Z-ADD     R1ntwTS(12)   wadRR_12aC
354300100201     C***  Ricavi Presunti mensili
354400100202     C                   Z-ADD     P1ntwTS(1)    wadRP_01aC
354500100202     C                   Z-ADD     P1ntwTS(2)    wadRP_02aC
354600100202     C                   Z-ADD     P1ntwTS(3)    wadRP_03aC
354700100202     C                   Z-ADD     P1ntwTS(4)    wadRP_04aC
354800100202     C                   Z-ADD     P1ntwTS(5)    wadRP_05aC
354900100202     C                   Z-ADD     P1ntwTS(6)    wadRP_06aC
355000100202     C                   Z-ADD     P1ntwTS(7)    wadRP_07aC
355100100202     C                   Z-ADD     P1ntwTS(8)    wadRP_08aC
355200100202     C                   Z-ADD     P1ntwTS(9)    wadRP_09aC
355300100202     C                   Z-ADD     P1ntwTS(10)   wadRP_10aC
355400100202     C                   Z-ADD     P1ntwTS(11)   wadRP_11aC
355500100202     C                   Z-ADD     P1ntwTS(12)   wadRP_12aC
355600100204      *  i campi delle Varie verranno aggiunti in seguito solo sul file di Totale
355700100202      *
355800100202     C                   WRITE     WADEAPC0
355900100201     C                   ENDdo
356000100201     C*
356100100201     C                   ENDSR
356200091123     C*------------------------------------------------------------------------*
356300020221     C*?STAAGE - IMPOSTA STAMPA TOTALI AGENTE COMMERCIALE
356400000000     C*------------------------------------------------------------------------*
356500000000     C     STAAGE        BEGSR
356600011001     C*
356700011001     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI COMMERCIALI
356800011001 FG  C                   IF        PARSIN = 'S'
356900011001     C                   SETON                                        53
357000011105     C                   ELSE
357100011105     C                   SETOFF                                       53
357200011001     C                   ENDIF
357300000000     C*
357400000000     C* AZZERA I TOTALI NELLA FORMA CLI, SPE, RIC, DEL
357500000000     C                   CLEAR                   CLW                            *NUMERO CLIENTI
357600000000     C                   CLEAR                   SPW                            *SPEDIZIONI
357700000000     C                   CLEAR                   RRW                            *RICAVO REALE
357800000000     C                   CLEAR                   RPW                            *RICAVO PRESUNTO
357900000000     C                   CLEAR                   CTW                            *13° TOTALE CLEINTI
358000000000     C*
358100020220     C* CHIUDO IL PROSPETTO DEL DETTAGLIO CLIENTI
358200000000IF  1C     ORAKSC        IFEQ      '1'
358300100129     c                   if        parSTAMPA <> 'N'
358400020116     C                   WRITE     SE94R2
358500100129     c                   end
358600000000     C                   MOVEL     '0'           ORAKSC                         *ORA NON STAMPA CLI
358700000000E   1C                   ENDIF
358800000000     C*
358900000000     C* STAMPA I VALORI DELL'AGENTE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
359000020318     C* LA STATISTICA DELTA CON DETTAGLIO FILIALE O CON DETTAGLIO AREA
359100000000IF  1C     PASAGE        IFEQ      '1'
359200000000IF  2C     PARSTA        IFEQ      'K'
359300000000     C     PARSTA        OREQ      'C'
359400000000     C***
359500000000     C* DESCRIZIONI INTESTAZIONE
359600000000     C***
359700020116     C                   CLEAR                   SE94IN
359800000000     C                   MOVEL     DESAGE        DES25            25            *DESCRIZIONE
359900000000     C                   MOVEL     DE2PCF        DES04             4            *PERCENTUALE
360000000000IF  3C     DE2PCF        IFGT      *BLANKS
360100000000     C                   MOVE      '%'           DES04
360200000000X   3C                   ELSE
360300000000     C                   MOVE      ' '           DES04
360400000000E   3C                   ENDIF
360500000000IF  3C     PARSTA        IFEQ      'K'
360600000000     C                   Z-ADD     DEPAGE        INCAGE                         *AGENTE
360700000000     C                   MOVEL     DES25         INDAGE                         *DESCRIZ + PERCEN
360800000000     C                   MOVE      DES04         INDAGE
360900000000X   3C                   ELSE
361000000000     C                   Z-ADD     *ZEROS        INCAGE
361100000000     C                   MOVEL     DRI(1)        INDAGE
361200000000E   3C                   ENDIF
361300000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
361400000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
361500000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
361600000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
361700000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
361800000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
361900950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
362000030924     C*
362100030924     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
362200030924     C                   MOVEL     DRI(21)       PIDDES                         *RETTANGOLINO
362300030924     C*
362400030924     C* SE DELTA FILIALE STAMPA I TOTALI AGENTE NELLA FORMA: TOT A, TOT B, TOT C
362500000000IF  3C     PARSTA        IFEQ      'K'
362600020222     C*
362700020221     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
362800020221DO  4C     1             DO        4             Q
362900000000     C     Q             OCCUR     ABCCOM
363000000000     C     Q             OCCUR     ABCSTA
363100000000     C                   Z-ADD     CNC           CNS
363200000000     C                   Z-ADD     SNC           SNS
363300000000     C                   Z-ADD     RNC           RNS
363400000000     C                   Z-ADD     PNC           PNS
363500030924     C                   Z-ADD     CAC           CAS
363600030924     C                   Z-ADD     SAC           SAS
363700030924     C                   Z-ADD     RAC           RAS
363800030924     C                   Z-ADD     PAC           PAS
363900000000     C*
364000000000     C* NUMERO TOTALE CLIENTI
364100000000     C     Q             OCCUR     ABCC13
364200000000     C     Q             OCCUR     ABCS13
364300000000     C                   Z-ADD     NTC           NTS
364400000000     C                   Z-ADD     ATC           ATS
364500000000     C*
364600000000E   4C                   ENDDO
364700020220     C*
364800020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA X CLIENTI 'D'
364900020220     C     5             OCCUR     ABCCOM
365000020220     C     5             OCCUR     ABCSTA
365100020220     C                   Z-ADD     CNC           CNS
365200020220     C                   Z-ADD     SNC           SNS
365300020220     C                   Z-ADD     RNC           RNS
365400020220     C                   Z-ADD     PNC           PNS
365500020220     C                   Z-ADD     CAC           CAS
365600020220     C                   Z-ADD     SAC           SAS
365700020220     C                   Z-ADD     RAC           RAS
365800020220     C                   Z-ADD     PAC           PAS
365900020220     C*
366000020220     C* NUMERO TOTALE CLIENTI 'D'
366100020220     C     5             OCCUR     ABCC13
366200020220     C     5             OCCUR     ABCS13
366300020220     C                   Z-ADD     NTC           NTS
366400020220     C                   Z-ADD     ATC           ATS
366500020318     C*
366600020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
366700020318DO  4C     1             DO        5             K
366800020318     C     K             OCCUR     ABCCOMN
366900020318     C     K             OCCUR     ABCSTAN
367000020318     C                   Z-ADD     CNCN          CNSN
367100020318     C                   Z-ADD     SNCN          SNSN
367200020318     C                   Z-ADD     RNCN          RNSN
367300020318     C                   Z-ADD     PNCN          PNSN
367400020318     C                   Z-ADD     CACN          CASN
367500020318     C                   Z-ADD     SACN          SASN
367600020318     C                   Z-ADD     RACN          RASN
367700020318     C                   Z-ADD     PACN          PASN
367800020318     C*
367900020318     C* NUMERO TOTALE CLIENTI
368000020318     C     K             OCCUR     ABCC13N
368100020318     C     K             OCCUR     ABCS13N
368200020318     C                   Z-ADD     NTCN          NTSN
368300020318     C                   Z-ADD     ATCN          ATSN
368400020318     C*
368500020318E   4C                   ENDDO
368600020318     C*
368700020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
368800020318DO  4C     1             DO        5             K
368900020318     C     K             OCCUR     ABCCOMND
369000020318     C     K             OCCUR     ABCSTAND
369100020318     C                   Z-ADD     CNCND         CNSND
369200020318     C                   Z-ADD     SNCND         SNSND
369300020318     C                   Z-ADD     RNCND         RNSND
369400020318     C                   Z-ADD     PNCND         PNSND
369500020318     C                   Z-ADD     CACND         CASND
369600020318     C                   Z-ADD     SACND         SASND
369700020318     C                   Z-ADD     RACND         RASND
369800020318     C                   Z-ADD     PACND         PASND
369900020318     C*
370000020318     C* NUMERO TOTALE CLIENTI
370100020318     C     K             OCCUR     ABCC13ND
370200020318     C     K             OCCUR     ABCS13ND
370300020318     C                   Z-ADD     NTCND         NTSND
370400020318     C                   Z-ADD     ATCND         ATSND
370500020318     C*
370600020318E   4C                   ENDDO
370700020220     C*
370800020220     C* ROUTINE DI STAMPA TOTALI
370900020220     C                   EXSR      STAMPA
371000000000     C*
371100000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
371200000000     C                   SETON                                        01
371300000000     C                   MOVEL     '0'           PRIGIR
371400000000     C*
371500000000     C* SE DELTA AREA STAMPA I TOTALI AGENTE NELLA FORMA: CLI, SPE, RIC, DEL
371600000000X   3C                   ELSE
371700020222     C*
371800020318     C* IMPOSTO IL FLAG CHE MI IDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
371900020318     C                   EVAL      DEPSTPD = '1'
372000020318     C*
372100000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
372200020318DO  4C     1             DO        4             Q
372300020318     C     Q             OCCUR     ABCCOM
372400020318     C                   ADD       CNC           CLW                            *NUOVI A, B, C +
372500020318     C                   ADD       SNC           SPW
372600020318     C                   ADD       RNC           RRW
372700020318     C                   ADD       PNC           RPW
372800020318     C                   ADD       CAC           CLW                            *ACQUISITI A, B , C
372900020318     C                   ADD       SAC           SPW
373000020318     C                   ADD       RAC           RRW
373100020318     C                   ADD       PAC           RPW
373200000000     C*
373300020318     C* NUMERO TOTALE CLIENTI
373400020318     C     Q             OCCUR     ABCC13
373500020318     C                   ADD       NTC           CTW
373600020318     C                   ADD       ATC           CTW
373700020318     C
373800020318E   4C                   ENDDO
373900020221     C*
374000000000     C                   MOVEL     DEPAGE        DES07             7            *AGENTE
374100000000     C                   MOVEL     DESAGE        DES12            12            *DESCRIZIONE
374200000000     C*
374300000000     C                   MOVEL     *BLANKS       DES20            20
374400000000     C                   MOVEL     DES07         DES20            20
374500000000     C                   MOVE      DES12         DES20
374600010710     C*
374700010710     C* ROUTINE DI STAMPA TOTALI
374800030926     c                   MOVEL     'A'           WTipStamp2                     *da STAAGE
374900020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
375000020222     C*
375100020220     C* STAMPO I 'D' SOLO SE EFFETTIVAMENTE ESISTENTI
375200020220     C                   IF        FLGSIDE = '1'
375300020221     C*
375400020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
375500020222     C                   EVAL      DEPSTPD = '2'
375600020220     C*
375700020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
375800020318     C     5             OCCUR     ABCCOM
375900050217     C****               ADD       CNC           CLW                            *NUOVI A, B, C +
376000050217     C****               ADD       SNC           SPW
376100050217     C****               ADD       RNC           RRW
376200050217     C****               ADD       PNC           RPW
376300050217     C****               ADD       CAC           CLW                            *ACQUISITI A, B , C
376400050217     C****               ADD       SAC           SPW
376500050217     C****               ADD       RAC           RRW
376600050217     C****               ADD       PAC           RPW
376700050217     C****
376800050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
376900050217     C****  i direzionali ma riportava la somma di tutti
377000050217     C     CNC           ADD       CAC           CLW                            *NUOVI+ACQUISITI (D)
377100050217     C     SNC           ADD       SAC           SPW
377200050217     C     RNC           ADD       RAC           RRW
377300050217     C     PNC           ADD       PAC           RPW
377400050217     C*
377500020220     C* NUMERO TOTALE CLIENTI CLIENTI 'D'
377600020318     C     5             OCCUR     ABCC13
377700050217     C****               ADD       NTC           CTW
377800050217     C****               ADD       ATC           CTW
377900050217     C****
378000050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
378100050217     C****  i direzionali ma riportava la somma di tutti
378200050217     C     NTC           ADD       ATC           CTW
378300020220     C*
378400020220     C* ROUTINE DI STAMPA TOTALI
378500030926     c                   MOVEL     'A'           WTipStamp2                     *da STAAGE
378600020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
378700020318     C                   ENDIF
378800000000     C*
378900000000     C* STAMPO RIGA DI SEPARAZIONE
379000100129     c                   if        parSTAMPA <> 'N'
379100020116     C  N53              WRITE     SE94R2
379200100129     c                   end
379300011001E   3C                   ENDIF
379400000000E   2C                   ENDIF
379500000000E   1C                   ENDIF
379600011001     C*
379700000000     C                   ENDSR
379800030924     C*------------------------------------------------------------------------*
379900030924     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
380000030924     C*------------------------------------------------------------------------*
380100030924     C     STAFIL        BEGSR
380200030924     C*
380300030924     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI FILIALE
380400030924     C                   IF        PARSIN = 'S'
380500030924     C                   SETON                                        53
380600030924     C                   ELSE
380700030924     C                   SETOFF                                       53
380800030924     C                   ENDIF
380900030924     C*
381000000000     C* AZZERA I TOTALI NELLA FORMA CLI, SPE, RIC, DEL
381100000000     C                   CLEAR                   CLW                            *NUMERO CLIENTI
381200000000     C                   CLEAR                   SPW                            *SPEDIZIONI
381300000000     C                   CLEAR                   RRW                            *RICAVO REALE
381400000000     C                   CLEAR                   RPW                            *RICAVO PRESUNTO
381500000000     C                   CLEAR                   CTW                            *13° TOTALE CLEINTI
381600000000     C*
381700000000     C* STAMPA VALORI DELLA FILIALE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
381800000000     C* LA STATISTICA DELTA CON DETTAGLIO CLIENTE/COMMERCIALE/FILIALE (SEMPRE)
381900000000IF  1C     PASFIL        IFEQ      '1'
382000000000IF  2C     PARSTA        IFEQ      'K'
382100000000     C     PARSTA        OREQ      'C'
382200000000     C     PARSTA        OREQ      'F'
382300000000     C***
382400000000     C* DESCRIZIONI INTESTAZIONE
382500000000     C***
382600020116     C                   CLEAR                   SE94IN
382700000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
382800000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
382900000000IF  3C     PARSTA        IFEQ      'K'
383000020221     C     PARSTA        OREQ      'C'
383100000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
383200000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
383300000000X   3C                   ELSE
383400000000     C                   Z-ADD     *ZEROS        INCFIL
383500000000     C                   MOVEL     DRI(2)        INDFIL
383600000000E   3C                   ENDIF
383700000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
383800000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
383900000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
384000000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
384100030924     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
384200030924     C*
384300030924     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
384400030924     C                   MOVEL     DRI(22)       PIDDES                         *RETTANGOLINO
384500030924     C*
384600030924     C* SE DELTA FILIALE O DELTA AREA STAMPA I TOTALI FILIALE NELLA FORMA:
384700030924     C* TOT A, TOT B, TOT C
384800000000IF  3C     PARSTA        IFEQ      'K'
384900000000     C     PARSTA        OREQ      'C'
385000000000     C*
385100000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
385200020221DO  4C     1             DO        4             Q
385300000000     C     Q             OCCUR     ABCFIL
385400000000     C     Q             OCCUR     ABCSTA
385500000000     C                   Z-ADD     CNF           CNS
385600000000     C                   Z-ADD     SNF           SNS
385700000000     C                   Z-ADD     RNF           RNS
385800000000     C                   Z-ADD     PNF           PNS
385900000000     C                   Z-ADD     CAF           CAS
386000000000     C                   Z-ADD     SAF           SAS
386100000000     C                   Z-ADD     RAF           RAS
386200000000     C                   Z-ADD     PAF           PAS
386300000000     C*
386400000000     C* NUMERO TOTALE CLIENTI
386500000000     C     Q             OCCUR     ABCF13
386600000000     C     Q             OCCUR     ABCS13
386700000000     C                   Z-ADD     NTF           NTS
386800000000     C                   Z-ADD     ATF           ATS
386900000000     C*
387000000000E   4C                   ENDDO
387100020220     C*
387200020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
387300020220     C     5             OCCUR     ABCFIL
387400020220     C     5             OCCUR     ABCSTA
387500020220     C                   Z-ADD     CNF           CNS
387600020220     C                   Z-ADD     SNF           SNS
387700020220     C                   Z-ADD     RNF           RNS
387800020220     C                   Z-ADD     PNF           PNS
387900020220     C                   Z-ADD     CAF           CAS
388000020220     C                   Z-ADD     SAF           SAS
388100020220     C                   Z-ADD     RAF           RAS
388200020220     C                   Z-ADD     PAF           PAS
388300020220     C*
388400020220     C* NUMERO TOTALE CLIENTI 'D'
388500020220     C     5             OCCUR     ABCF13
388600020220     C     5             OCCUR     ABCS13
388700020220     C                   Z-ADD     NTF           NTS
388800020220     C                   Z-ADD     ATF           ATS
388900020318     C*
389000020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
389100020318DO  4C     1             DO        5             K
389200020318     C     K             OCCUR     ABCFILN
389300020318     C     K             OCCUR     ABCSTAN
389400020318     C                   Z-ADD     CNFN          CNSN
389500020318     C                   Z-ADD     SNFN          SNSN
389600020318     C                   Z-ADD     RNFN          RNSN
389700020318     C                   Z-ADD     PNFN          PNSN
389800020318     C                   Z-ADD     CAFN          CASN
389900020318     C                   Z-ADD     SAFN          SASN
390000020318     C                   Z-ADD     RAFN          RASN
390100020318     C                   Z-ADD     PAFN          PASN
390200020318     C*
390300020318     C* NUMERO TOTALE CLIENTI
390400020318     C     K             OCCUR     ABCF13N
390500020318     C     K             OCCUR     ABCS13N
390600020318     C                   Z-ADD     NTFN          NTSN
390700020318     C                   Z-ADD     ATFN          ATSN
390800020318     C*
390900020318E   4C                   ENDDO
391000020318     C*
391100020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
391200020318DO  4C     1             DO        5             K
391300020318     C     K             OCCUR     ABCFILND
391400020318     C     K             OCCUR     ABCSTAND
391500020318     C                   Z-ADD     CNFND         CNSND
391600020318     C                   Z-ADD     SNFND         SNSND
391700020318     C                   Z-ADD     RNFND         RNSND
391800020318     C                   Z-ADD     PNFND         PNSND
391900020318     C                   Z-ADD     CAFND         CASND
392000020318     C                   Z-ADD     SAFND         SASND
392100020318     C                   Z-ADD     RAFND         RASND
392200020318     C                   Z-ADD     PAFND         PASND
392300020318     C*
392400020318     C* NUMERO TOTALE CLIENTI
392500020318     C     K             OCCUR     ABCF13ND
392600020318     C     K             OCCUR     ABCS13ND
392700020318     C                   Z-ADD     NTFND         NTSND
392800020318     C                   Z-ADD     ATFND         ATSND
392900020318     C*
393000020318E   4C                   ENDDO
393100020220     C*
393200020220     C* ROUTINE DI STAMPA TOTALI
393300020220     C                   EXSR      STAMPA
393400000000     C*
393500000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
393600000000     C                   SETON                                        01
393700000000     C                   MOVEL     '0'           PRIGIR
393800000000     C*
393900000000     C* SE DELTA DIVISIONE STAMPA I TOTALI FILIALE NELLA FORMA: CLI,SPE,RIC,DEL
394000000000X   3C                   ELSE
394100020221     C*
394200020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
394300020222     C                   EVAL      DEPSTPD = '1'
394400000000     C*
394500000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
394600020318DO  4C     1             DO        4             Q
394700020318     C     Q             OCCUR     ABCFIL
394800050217     C                   ADD       CNF           CLW                            *NUOVI A, B, C +
394900050217     C                   ADD       SNF           SPW
395000050217     C                   ADD       RNF           RRW
395100050217     C                   ADD       PNF           RPW
395200050217     C                   ADD       CAF           CLW                            *ACQUISITI A, B , C
395300050217     C                   ADD       SAF           SPW
395400050217     C                   ADD       RAF           RRW
395500050217     C                   ADD       PAF           RPW
395600050217     C*
395700000000     C* NUMERO TOTALE CLIENTI
395800020318     C     Q             OCCUR     ABCF13
395900050217     C                   ADD       NTF           CTW
396000050217     C                   ADD       ATF           CTW
396100050217     C*
396200020318E   4C                   ENDDO
396300020221     C*
396400000000     C                   MOVEL     DEPFIL        DES03             3            *FILIALE
396500000000     C                   MOVEL     DESFIL        DES16            16            *DESCRIZIONE
396600000000     C*
396700000000     C                   MOVEL     *BLANKS       DES20            20
396800000000     C                   MOVEL     DES03         DES20            20
396900000000     C                   MOVE      DES16         DES20
397000010710     C*
397100010710     C* ROUTINE DI STAMPA TOTALI
397200030926     c                   MOVEL     'F'           WTipStamp2                     *da STAAGE
397300020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
397400020220     C*
397500020220     C* STAMPO I 'D' SOLO SE EFFETTIVAMENTE ESISTENTI
397600020220     C                   IF        FLGSIDF = '1'
397700020221     C*
397800020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
397900020222     C                   EVAL      DEPSTPD = '2'
398000020220     C*
398100020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
398200020318     C     5             OCCUR     ABCFIL
398300050217     C****               ADD       CNF           CLW                            *NUOVI A, B, C +
398400050217     C****               ADD       SNF           SPW
398500050217     C****               ADD       RNF           RRW
398600050217     C****               ADD       PNF           RPW
398700050217     C****               ADD       CAF           CLW                            *ACQUISITI A, B , C
398800050217     C****               ADD       SAF           SPW
398900050217     C****               ADD       RAF           RRW
399000050217     C****               ADD       PAF           RPW
399100050217     C****
399200050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
399300050217     C****  i direzionali ma riportava la somma di tutti
399400050217     C     CNF           ADD       CAF           CLW                            *NUOVI+ACQUISITI (D)
399500050217     C     SNF           ADD       SAF           SPW
399600050217     C     RNF           ADD       RAF           RRW
399700050217     C     PNF           ADD       PAF           RPW
399800050217     C*
399900020220     C* NUMERO TOTALE CLIENTI 'D'
400000020318     C     5             OCCUR     ABCF13
400100050217     C****               ADD       NTF           CTW
400200050217     C****               ADD       ATF           CTW
400300050217     C****
400400050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
400500050217     C****  i direzionali ma riportava la somma di tutti
400600050217     C     NTF           ADD       ATF           CTW
400700050217     C*
400800020220     C* ROUTINE DI STAMPA TOTALI
400900030926     c                   MOVEL     'F'           WTipStamp2                     *da STAAGE
401000020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
401100020318     C                   ENDIF
401200000000     C*
401300000000     C* STAMPO RIGA DI SEPARAZIONE
401400100129     c                   if        parSTAMPA <> 'N'
401500020116     C  N53              WRITE     SE94R2
401600100129     c                   end
401700000000E   3C                   ENDIF
401800000000E   2C                   ENDIF
401900000000E   1C                   ENDIF
402000010710     C*
402100000000     C                   ENDSR
402200000000     C*------------------------------------------------------------------------*
402300000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
402400000000     C*------------------------------------------------------------------------*
402500000000     C     STAARE        BEGSR
402600020215     C*
402700020215     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI AREA
402800020215 FG  C                   IF        PARSIN = 'S'
402900020215     C                   SETON                                        53
403000020215     C                   ELSE
403100020215     C                   SETOFF                                       53
403200020215     C                   ENDIF
403300000000     C*
403400000000     C* STAMPA I VALORI DELL'AREA SE HO MEMORIZZATO QUALCOSA E SE STO ELABORANDO
403500020318     C* LA STATISTICA DELTA CON DETTAGLIO AREA O CON DETTAGLIO DIVISIONE
403600000000IF  1C     PASARE        IFEQ      '1'
403700000000IF  2C     PARSTA        IFEQ      'C'
403800000000     C     PARSTA        OREQ      'F'
403900000000     C***
404000000000     C* DESCRIZIONI INTESTAZIONE
404100000000     C***
404200020116     C                   CLEAR                   SE94IN
404300000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
404400000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
404500000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
404600000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
404700000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
404800000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
404900000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
405000000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
405100950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
405200000000     C*
405300000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
405400000000     C                   MOVEL     DRI(23)       PIDDES                         *RETTANGOLINO
405500000000     C*
405600000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
405700020221DO  3C     1             DO        4             Q
405800000000     C     Q             OCCUR     ABCARE
405900000000     C     Q             OCCUR     ABCSTA
406000000000     C                   Z-ADD     CNA           CNS
406100000000     C                   Z-ADD     SNA           SNS
406200000000     C                   Z-ADD     RNA           RNS
406300000000     C                   Z-ADD     PNA           PNS
406400000000     C                   Z-ADD     CAA           CAS
406500000000     C                   Z-ADD     SAA           SAS
406600000000     C                   Z-ADD     RAA           RAS
406700000000     C                   Z-ADD     PAA           PAS
406800000000     C*
406900000000     C* NUMERO TOTALE CLIENTI
407000000000     C     Q             OCCUR     ABCA13
407100000000     C     Q             OCCUR     ABCS13
407200000000     C                   Z-ADD     NTA           NTS
407300000000     C                   Z-ADD     ATA           ATS
407400000000     C*
407500000000E   3C                   ENDDO
407600020220     C*
407700020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
407800020220     C     5             OCCUR     ABCARE
407900020220     C     5             OCCUR     ABCSTA
408000020220     C                   Z-ADD     CNA           CNS
408100020220     C                   Z-ADD     SNA           SNS
408200020220     C                   Z-ADD     RNA           RNS
408300020220     C                   Z-ADD     PNA           PNS
408400020220     C                   Z-ADD     CAA           CAS
408500020220     C                   Z-ADD     SAA           SAS
408600020220     C                   Z-ADD     RAA           RAS
408700020220     C                   Z-ADD     PAA           PAS
408800020220     C*
408900020220     C* NUMERO TOTALE CLIENTI 'D'
409000020220     C     5             OCCUR     ABCA13
409100020220     C     5             OCCUR     ABCS13
409200020220     C                   Z-ADD     NTA           NTS
409300020220     C                   Z-ADD     ATA           ATS
409400020318     C*
409500020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
409600020318DO  3C     1             DO        5             K
409700020318     C     K             OCCUR     ABCAREN
409800020318     C     K             OCCUR     ABCSTAN
409900020318     C                   Z-ADD     CNAN          CNSN
410000020318     C                   Z-ADD     SNAN          SNSN
410100020318     C                   Z-ADD     RNAN          RNSN
410200020318     C                   Z-ADD     PNAN          PNSN
410300020318     C                   Z-ADD     CAAN          CASN
410400020318     C                   Z-ADD     SAAN          SASN
410500020318     C                   Z-ADD     RAAN          RASN
410600020318     C                   Z-ADD     PAAN          PASN
410700020318     C*
410800020318     C* NUMERO TOTALE CLIENTI
410900020318     C     K             OCCUR     ABCA13N
411000020318     C     K             OCCUR     ABCS13N
411100020318     C                   Z-ADD     NTAN          NTSN
411200020318     C                   Z-ADD     ATAN          ATSN
411300020318     C*
411400020318E   3C                   ENDDO
411500020318     C*
411600020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
411700020318DO  3C     1             DO        5             K
411800020318     C     K             OCCUR     ABCAREND
411900020318     C     K             OCCUR     ABCSTAND
412000020318     C                   Z-ADD     CNAND         CNSND
412100020318     C                   Z-ADD     SNAND         SNSND
412200020318     C                   Z-ADD     RNAND         RNSND
412300020318     C                   Z-ADD     PNAND         PNSND
412400020318     C                   Z-ADD     CAAND         CASND
412500020318     C                   Z-ADD     SAAND         SASND
412600020318     C                   Z-ADD     RAAND         RASND
412700020318     C                   Z-ADD     PAAND         PASND
412800020318     C*
412900020318     C* NUMERO TOTALE CLIENTI
413000020318     C     K             OCCUR     ABCA13ND
413100020318     C     K             OCCUR     ABCS13ND
413200020318     C                   Z-ADD     NTAND         NTSND
413300020318     C                   Z-ADD     ATAND         ATSND
413400020318     C*
413500020318E   3C                   ENDDO
413600020220     C*
413700020220     C* ROUTINE DI STAMPA TOTALI
413800020220     C                   EXSR      STAMPA
413900000000     C*
414000000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
414100000000     C                   SETON                                        01
414200000000     C                   MOVEL     '0'           PRIGIR
414300000000     C*
414400000000E   2C                   ENDIF
414500000000E   1C                   ENDIF
414600000000     C*
414700000000     C                   ENDSR
414800000000     C*------------------------------------------------------------------------*
414900000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
415000000000     C*------------------------------------------------------------------------*
415100000000     C     STADIV        BEGSR
415200020215     C*
415300020215     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI DIVISIONE
415400020215 FG  C                   IF        PARSIN = 'S'
415500020215     C                   SETON                                        53
415600020215     C                   ELSE
415700020215     C                   SETOFF                                       53
415800020215     C                   ENDIF
415900000000     C*
416000020318     C* STAMPA I VALORI DELLA DIVISIONE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
416100020318     C* LA STATISTICA DELTA CON DETTAGLIO DIVISIONE
416200000000IF  1C     PASDIV        IFEQ      '1'
416300000000     C     PARSTA        ANDEQ     'F'
416400000000     C***
416500000000     C* DESCRIZIONI INTESTAZIONE
416600000000     C***
416700020116     C                   CLEAR                   SE94IN
416800000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
416900000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
417000000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
417100000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
417200000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
417300000000     C                   MOVEL     DRI(2)        INDARE                         *DESCRIZIONE AREA
417400000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
417500000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
417600950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
417700000000     C*
417800000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
417900000000     C                   MOVEL     DRI(24)       PIDDES
418000000000     C*
418100000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
418200020221DO  2C     1             DO        4             Q
418300000000     C     Q             OCCUR     ABCDIV
418400000000     C     Q             OCCUR     ABCSTA
418500000000     C                   Z-ADD     CND           CNS
418600000000     C                   Z-ADD     SND           SNS
418700000000     C                   Z-ADD     RND           RNS
418800000000     C                   Z-ADD     PND           PNS
418900000000     C                   Z-ADD     CAD           CAS
419000000000     C                   Z-ADD     SAD           SAS
419100000000     C                   Z-ADD     RAD           RAS
419200000000     C                   Z-ADD     PAD           PAS
419300000000     C*
419400000000     C* NUMERO TOTALE CLIENTI
419500000000     C     Q             OCCUR     ABCD13
419600000000     C     Q             OCCUR     ABCS13
419700000000     C                   Z-ADD     NTD           NTS
419800000000     C                   Z-ADD     ATD           ATS
419900000000     C*
420000000000E   2C                   ENDDO
420100020220     C*
420200020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
420300020220     C     5             OCCUR     ABCDIV
420400020220     C     5             OCCUR     ABCSTA
420500020220     C                   Z-ADD     CND           CNS
420600020220     C                   Z-ADD     SND           SNS
420700030924     C                   Z-ADD     RND           RNS
420800030924     C                   Z-ADD     PND           PNS
420900030924     C                   Z-ADD     CAD           CAS
421000030924     C                   Z-ADD     SAD           SAS
421100020220     C                   Z-ADD     RAD           RAS
421200030924     C                   Z-ADD     PAD           PAS
421300030924     C*
421400030924     C* NUMERO TOTALE CLIENTI
421500030924     C     5             OCCUR     ABCD13
421600030924     C     5             OCCUR     ABCS13
421700030924     C                   Z-ADD     NTD           NTS
421800030924     C                   Z-ADD     ATD           ATS
421900030924     C*
422000030924     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
422100020318DO  2C     1             DO        5             K
422200030924     C     K             OCCUR     ABCDIVN
422300030924     C     K             OCCUR     ABCSTAN
422400030924     C                   Z-ADD     CNDN          CNSN
422500030924     C                   Z-ADD     SNDN          SNSN
422600030924     C                   Z-ADD     RNDN          RNSN
422700030924     C                   Z-ADD     PNDN          PNSN
422800030924     C                   Z-ADD     CADN          CASN
422900020318     C                   Z-ADD     SADN          SASN
423000020318     C                   Z-ADD     RADN          RASN
423100020318     C                   Z-ADD     PADN          PASN
423200020318     C*
423300020318     C* NUMERO TOTALE CLIENTI
423400020318     C     K             OCCUR     ABCD13N
423500020318     C     K             OCCUR     ABCS13N
423600020318     C                   Z-ADD     NTDN          NTSN
423700020318     C                   Z-ADD     ATDN          ATSN
423800020318     C*
423900020318E   2C                   ENDDO
424000020318     C*
424100020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
424200020318DO  2C     1             DO        5             K
424300020318     C     K             OCCUR     ABCDIVND
424400020318     C     K             OCCUR     ABCSTAND
424500020318     C                   Z-ADD     CNDND         CNSND
424600020318     C                   Z-ADD     SNDND         SNSND
424700020318     C                   Z-ADD     RNDND         RNSND
424800020318     C                   Z-ADD     PNDND         PNSND
424900020318     C                   Z-ADD     CADND         CASND
425000020318     C                   Z-ADD     SADND         SASND
425100020318     C                   Z-ADD     RADND         RASND
425200020318     C                   Z-ADD     PADND         PASND
425300020318     C*
425400020318     C* NUMERO TOTALE CLIENTI
425500020318     C     K             OCCUR     ABCD13ND
425600020318     C     K             OCCUR     ABCS13ND
425700020318     C                   Z-ADD     NTDND         NTSND
425800020318     C                   Z-ADD     ATDND         ATSND
425900020318     C*
426000020318E   2C                   ENDDO
426100020220     C*
426200020220     C* ROUTINE DI STAMPA TOTALI
426300020220     C                   EXSR      STAMPA
426400000000     C*
426500000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
426600000000     C                   SETON                                        01
426700000000     C                   MOVEL     '0'           PRIGIR
426800000000     C*
426900000000E   1C                   ENDIF
427000000000     C*
427100000000     C                   ENDSR
427200000000     C*------------------------------------------------------------------------*
427300000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
427400000000     C*------------------------------------------------------------------------*
427500000000     C     STAGEN        BEGSR
427600000000     C*
427700000000     C* STAMPA VALORI GENERALI SOLO SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
427800020318     C* LA STATISTICA DELTA CON DETTAGLIO DIVISIONE
427900000000IF  1C     PASGEN        IFEQ      '1'
428000000000     C     PARSTA        ANDEQ     'F'
428100000000     C***
428200000000     C* DESCRIZIONI INTESTAZIONE
428300000000     C***
428400020116     C                   CLEAR                   SE94IN
428500000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
428600000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
428700000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
428800000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
428900000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
429000000000     C                   MOVEL     DRI(2)        INDARE                         *DESCRIZIONE AREA
429100000000     C                   MOVEL     *BLANKS       INCDIV                         *DIVISIONE
429200000000     C                   MOVEL     DRI(2)        INDDIV                         DESCRIZIONE DIVISION
429300950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
429400000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
429500000000     C                   MOVEL     DRI(25)       PIDDES
429600000000     C*
429700000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
429800020221DO  2C     1             DO        4             Q
429900000000     C     Q             OCCUR     ABCGEN
430000000000     C     Q             OCCUR     ABCSTA
430100000000     C                   Z-ADD     CNG           CNS
430200000000     C                   Z-ADD     SNG           SNS
430300000000     C                   Z-ADD     RNG           RNS
430400000000     C                   Z-ADD     PNG           PNS
430500000000     C                   Z-ADD     CAG           CAS
430600000000     C                   Z-ADD     SAG           SAS
430700000000     C                   Z-ADD     RAG           RAS
430800000000     C                   Z-ADD     PAG           PAS
430900000000     C*
431000000000     C* NUMERO TOTALE CLIENTI
431100000000     C     Q             OCCUR     ABCG13
431200000000     C     Q             OCCUR     ABCS13
431300000000     C                   Z-ADD     NTG           NTS
431400000000     C                   Z-ADD     ATG           ATS
431500000000     C*
431600000000E   2C                   ENDDO
431700020220     C*
431800020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
431900020220     C     5             OCCUR     ABCGEN
432000020220     C     5             OCCUR     ABCSTA
432100020220     C                   Z-ADD     CNG           CNS
432200020220     C                   Z-ADD     SNG           SNS
432300020220     C                   Z-ADD     RNG           RNS
432400020220     C                   Z-ADD     PNG           PNS
432500020220     C                   Z-ADD     CAG           CAS
432600020220     C                   Z-ADD     SAG           SAS
432700020220     C                   Z-ADD     RAG           RAS
432800020220     C                   Z-ADD     PAG           PAS
432900020220     C*
433000020220     C* NUMERO TOTALE CLIENTI 'D'
433100020220     C     5             OCCUR     ABCG13
433200020220     C     5             OCCUR     ABCS13
433300020220     C                   Z-ADD     NTG           NTS
433400020220     C                   Z-ADD     ATG           ATS
433500020318     C*
433600020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
433700020318DO  2C     1             DO        5             K
433800020318     C     K             OCCUR     ABCGENN
433900020318     C     K             OCCUR     ABCSTAN
434000020318     C                   Z-ADD     CNGN          CNSN
434100020318     C                   Z-ADD     SNGN          SNSN
434200020318     C                   Z-ADD     RNGN          RNSN
434300020318     C                   Z-ADD     PNGN          PNSN
434400020318     C                   Z-ADD     CAGN          CASN
434500020318     C                   Z-ADD     SAGN          SASN
434600020318     C                   Z-ADD     RAGN          RASN
434700020318     C                   Z-ADD     PAGN          PASN
434800020318     C*
434900020318     C* NUMERO TOTALE CLIENTI
435000020318     C     K             OCCUR     ABCG13N
435100020318     C     K             OCCUR     ABCS13N
435200020318     C                   Z-ADD     NTGN          NTSN
435300020318     C                   Z-ADD     ATGN          ATSN
435400020318     C*
435500020318E   2C                   ENDDO
435600020318     C*
435700020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
435800020318DO  2C     1             DO        5             K
435900020318     C     K             OCCUR     ABCGENND
436000020318     C     K             OCCUR     ABCSTAND
436100020318     C                   Z-ADD     CNGND         CNSND
436200020318     C                   Z-ADD     SNGND         SNSND
436300020318     C                   Z-ADD     RNGND         RNSND
436400020318     C                   Z-ADD     PNGND         PNSND
436500020318     C                   Z-ADD     CAGND         CASND
436600020318     C                   Z-ADD     SAGND         SASND
436700020318     C                   Z-ADD     RAGND         RASND
436800020318     C                   Z-ADD     PAGND         PASND
436900020318     C*
437000020318     C* NUMERO TOTALE CLIENTI
437100020318     C     K             OCCUR     ABCG13ND
437200020318     C     K             OCCUR     ABCS13ND
437300020318     C                   Z-ADD     NTGND         NTSND
437400020318     C                   Z-ADD     ATGND         ATSND
437500020318     C*
437600020318E   2C                   ENDDO
437700020220     C*
437800020220     C* ROUTINE DI STAMPA TOTALI
437900020220     C                   EXSR      STAMPA
438000000000     C*
438100000000E   1C                   ENDIF
438200000000     C*
438300000000     C                   ENDSR
438400000000     C*------------------------------------------------------------------------*
438500000000     C*?STAMPA - STAMPA I TOTALI NELLA FORMA: TOT "A" / TOT "B" / TOT "C"
438600000000     C*------------------------------------------------------------------------*
438700000000     C     STAMPA        BEGSR
438800020221     C*--------------------
438900020221     C* I N T E S T A Z I O N E
439000020221     C*--------------------
439100020221     C                   EXSR      STAINT
439200000000     C*
439300000000     C* PULISCO I TOTALI A+B+C
439400000000     C                   CLEAR                   CNW
439500000000     C                   CLEAR                   SNW
439600000000     C                   CLEAR                   RNW
439700000000     C                   CLEAR                   PNW
439800000000     C                   CLEAR                   CAW
439900000000     C                   CLEAR                   SAW
440000000000     C                   CLEAR                   RAW
440100000000     C                   CLEAR                   PAW
440200000000     C*
440300020318     C                   Z-ADD     *ZEROS        CNW13C           11 0          *PERIODO CORRENTE
440400000000     C                   Z-ADD     *ZEROS        CAW13C           11 0
440500951227     C                   Z-ADD     *ZEROS        CN213C           11 0          *PERIODO PRECEDENTE
440600951227     C                   Z-ADD     *ZEROS        CA213C           11 0
440700000000     C*
440800980521DO  1C     1             DO        4             Q
440900000000     C     Q             OCCUR     ABCSTA
441000000000     C***
441100020221     C* TOTALI "A" + "B" + "C"  DIVISI IN NUOVI E ACQUISITI
441200000000     C***
441300000000     C                   ADD       CNS           CNW
441400000000     C                   ADD       SNS           SNW
441500000000     C                   ADD       RNS           RNW
441600000000     C                   ADD       PNS           PNW
441700000000     C                   ADD       CAS           CAW
441800000000     C                   ADD       SAS           SAW
441900000000     C                   ADD       RAS           RAW
442000000000     C                   ADD       PAS           PAW
442100951227     C*
442200000000     C     Q             OCCUR     ABCS13
442300951227     C* CORRENTE
442400000000     C                   ADD       NTS(1)        CNW13C
442500000000     C                   ADD       ATS(1)        CAW13C
442600951227     C* PRECEDENTE
442700951227     C                   ADD       NTS(2)        CN213C
442800011105     C                   ADD       ATS(2)        CA213C
442900000000     C*
443000000000E   1C                   ENDDO
443100020221     C*
443200000000     C***
443300000000     C* CLIENTI  N U O V I
443400000000     C***
443500980521DO  1C     1             DO        4             Q
443600000000     C     Q             OCCUR     ABCSTA
443700000000     C*
443800000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
443900000000IF  2C     *IN01         IFEQ      *ON
444000020215     C                   EXSR      STAINT
444100000000E   2C                   ENDIF
444200020222     C*
444300020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
444400020222     C                   EVAL      DEPSTPD = '0'
444500000000     C*
444600000000     C                   XFOOT     CNS           COMCLI           16 0          *NUOVI DI CATEGORIA
444700000000IF  2C     COMCLI        IFGT      *ZEROS
444800000000     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
444900000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
445000020318     C                   MOVEL     '0'           FLGSTPNTW
445100000000     C                   Z-ADD     CNS           $CL                            NUMERO CLIENTI
445200000000     C                   Z-ADD     SNS           $SP                            SPEDIZIONI
445300000000     C                   Z-ADD     RNS           $RR                            RICAVI REALI
445400000000     C                   Z-ADD     PNS           $RP                            RICAVI PRESUNTI
445500000000     C*
445600000000     C     Q             OCCUR     ABCS13
445700951227     C                   Z-ADD     NTS(1)        $CL13C           11 0          N° TOT CLIENTI CORR
445800951227     C                   Z-ADD     NTS(2)        $C213C           11 0          N° TOT CLIENTI PREC
445900000000     C*
446000000000     C                   EXSR      STMDAT
446100000000E   2C                   ENDIF
446200000000     C*
446300000000E   1C                   ENDDO
446400000000     C*
446500020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
446600020318     C                   IF        PARSTA = 'K'
446700000000     C* T O T A L I   N U O V I
446800000000     C                   Z-ADD     CNW           $CL                            NUMERO CLIENTI
446900000000     C                   Z-ADD     SNW           $SP                            SPEDIZIONI
447000000000     C                   Z-ADD     RNW           $RR                            RICAVI REALI
447100000000     C                   Z-ADD     PNW           $RP                            RICAVI PRESUNTI
447200951227     C                   Z-ADD     CNW13C        $CL13C                         N° TOT CLIENTI CORR
447300951227     C                   Z-ADD     CN213C        $C213C                         N° TOT CLIENTI PREC
447400020222     C*
447500020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
447600020222IF  2C     *IN01         IFEQ      *ON
447700020222     C                   EXSR      STAINT
447800020222E   2C                   ENDIF
447900020222     C*
448000020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
448100020222     C                   EVAL      DEPSTPD = '1'
448200000000     C*
448300020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
448400000000IF  1C     COMCLI        IFGT      *ZEROS
448500000000     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
448600020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
448700020221     C                   EXSR      STMDAT
448800100129     c                   if        parSTAMPA <> 'N'
448900020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
449000100129     c                   end
449100020221E   1C                   ENDIF
449200020318     C*
449300020318     C                   ELSE
449400020318     C* T O T A L I   N U O V I
449500020318     C                   Z-ADD     CNW           $CL                            NUMERO CLIENTI
449600020318     C                   Z-ADD     SNW           $SP                            SPEDIZIONI
449700020318     C                   Z-ADD     RNW           $RR                            RICAVI REALI
449800020318     C                   Z-ADD     PNW           $RP                            RICAVI PRESUNTI
449900020318     C                   Z-ADD     CNW13C        $CL13C                         N° TOT CLIENTI CORR
450000020318     C                   Z-ADD     CN213C        $C213C                         N° TOT CLIENTI PREC
450100020318     C*
450200020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
450300020318IF  2C     *IN01         IFEQ      *ON
450400020318     C                   EXSR      STAINT
450500020318E   2C                   ENDIF
450600020318     C*
450700020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
450800020318     C                   EVAL      DEPSTPD = '1'
450900020318     C*
451000020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
451100020318IF  1C     COMCLI        IFGT      *ZEROS
451200020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
451300020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
451400020318     C                   EXSR      STMDAT
451500100129     c                   if        parSTAMPA <> 'N'
451600020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
451700100129     c                   end
451800020318E   1C                   ENDIF
451900020318     C***
452000020404     C* T O T A L I   N U O V I   X   N E T W O R K
452100020318     C***
452200020404     C*
452300020404     C* SOLO SE RICHIESTO NEL LANCIO
452400020404     C                   IF        PARNTW = 'S'
452500020404     C*
452600020319     C                   MOVEL     '0'           FLGSTPRI          1
452700020319     C*
452800020319DO  1C     2             DO        5             K
452900020318     C     K             OCCUR     ABCSTAN
453000020318     C*
453100020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
453200020318IF  2C     *IN01         IFEQ      *ON
453300020318     C                   EXSR      STAINT
453400020318E   2C                   ENDIF
453500020318     C*
453600020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
453700020318     C                   EVAL      DEPSTPD = '1'
453800020318     C*
453900020318     C                   XFOOT     CNSN          COMCLI           16 0          *NUOVI DI CATEGORIA
454000020318IF  2C     COMCLI        IFGT      *ZEROS
454100020319     C                   MOVEL     '1'           FLGSTPRI
454200020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
454300020318     C                   Z-ADD     K             $Q                             NETWORK
454400020318     C                   MOVEL     '1'           FLGSTPNTW
454500020318     C                   Z-ADD     CNSN          $CL                            NUMERO CLIENTI
454600020318     C                   Z-ADD     SNSN          $SP                            SPEDIZIONI
454700020318     C                   Z-ADD     RNSN          $RR                            RICAVI REALI
454800020318     C                   Z-ADD     PNSN          $RP                            RICAVI PRESUNTI
454900020318     C*
455000020318     C     K             OCCUR     ABCS13N
455100020318     C                   Z-ADD     NTSN(1)       $CL13C           11 0          N° TOT CLIENTI CORR
455200020318     C                   Z-ADD     NTSN(2)       $C213C           11 0          N° TOT CLIENTI PREC
455300020318     C*
455400020318     C                   EXSR      STMDAT
455500020318E   2C                   ENDIF
455600020318     C*
455700020318E   1C                   ENDDO
455800020319     C                   IF        FLGSTPRI = '1'
455900100129     c                   if        parSTAMPA <> 'N'
456000020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
456100100129     c                   end
456200020318     C                   ENDIF
456300020319     C                   ENDIF
456400020404     C*
456500020404     C                   ENDIF
456600020221     C*
456700020221     C* PULISCO I TOTALI D
456800020221     C                   CLEAR                   CNWD
456900020221     C                   CLEAR                   SNWD
457000020221     C                   CLEAR                   RNWD
457100020221     C                   CLEAR                   PNWD
457200020221     C                   CLEAR                   CAWD
457300020221     C                   CLEAR                   SAWD
457400020221     C                   CLEAR                   RAWD
457500020221     C                   CLEAR                   PAWD
457600020221     C*
457700020318     C                   Z-ADD     *ZEROS        CNW13CD          11 0          *PERIODO CORRENTE
457800020221     C                   Z-ADD     *ZEROS        CAW13CD          11 0
457900020221     C                   Z-ADD     *ZEROS        CN213CD          11 0          *PERIODO PRECEDENTE
458000020221     C                   Z-ADD     *ZEROS        CA213CD          11 0
458100020221     C*
458200020221     C     5             OCCUR     ABCSTA
458300020221     C***
458400020221     C* TOTALI "D" DIVISI IN NUOVI E ACQUISITI
458500020221     C***
458600020221     C                   ADD       CNS           CNWD
458700020221     C                   ADD       SNS           SNWD
458800020221     C                   ADD       RNS           RNWD
458900020221     C                   ADD       PNS           PNWD
459000020221     C                   ADD       CAS           CAWD
459100020221     C                   ADD       SAS           SAWD
459200020221     C                   ADD       RAS           RAWD
459300020221     C                   ADD       PAS           PAWD
459400020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
459500020221     C                   ADD       CNW           CNWD
459600020221     C                   ADD       SNW           SNWD
459700020221     C                   ADD       RNW           RNWD
459800020221     C                   ADD       PNW           PNWD
459900020221     C                   ADD       CAW           CAWD
460000020221     C                   ADD       SAW           SAWD
460100020221     C                   ADD       RAW           RAWD
460200020221     C                   ADD       PAW           PAWD
460300020221     C*
460400020221     C     5             OCCUR     ABCS13
460500020221     C* CORRENTE
460600020221     C                   ADD       NTS(1)        CNW13CD
460700020221     C                   ADD       ATS(1)        CAW13CD
460800020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
460900020221     C                   ADD       CNW13C        CNW13CD
461000020221     C                   ADD       CAW13C        CAW13CD
461100020221     C* PRECEDENTE
461200020221     C                   ADD       NTS(2)        CN213CD
461300020221     C                   ADD       ATS(2)        CA213CD
461400020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
461500020221     C                   ADD       CN213C        CN213CD
461600020221     C                   ADD       CA213C        CA213CD
461700020221     C***
461800020221     C* CLIENTI  N U O V I  'D'
461900020221     C***
462000020221     C     5             OCCUR     ABCSTA
462100020221     C*
462200020221     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
462300020221IF  2C     *IN01         IFEQ      *ON
462400020221     C                   EXSR      STAINT
462500020221E   2C                   ENDIF
462600020222     C*
462700020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
462800020222     C                   EVAL      DEPSTPD = '0'
462900020221     C*
463000020221     C                   XFOOT     CNS           COMCLI           16 0          *NUOVI DI CATEGORIA
463100020221IF  2C     COMCLI        IFGT      *ZEROS
463200020221     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
463300020221     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
463400020318     C                   MOVEL     '0'           FLGSTPNTW
463500020221     C                   Z-ADD     CNS           $CL                            NUMERO CLIENTI
463600020221     C                   Z-ADD     SNS           $SP                            SPEDIZIONI
463700020221     C                   Z-ADD     RNS           $RR                            RICAVI REALI
463800020221     C                   Z-ADD     PNS           $RP                            RICAVI PRESUNTI
463900020221     C*
464000020221     C     5             OCCUR     ABCS13
464100020221     C                   Z-ADD     NTS(1)        $CL13C           11 0          N° TOT CLIENTI CORR
464200020221     C                   Z-ADD     NTS(2)        $C213C           11 0          N° TOT CLIENTI PREC
464300020221     C*
464400020221     C                   EXSR      STMDAT
464500020221E   2C                   ENDIF
464600020318     C*
464700020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
464800020318     C                   IF        PARSTA = 'K'
464900020221     C* T O T A L I   N U O V I   'D'
465000020221     C                   Z-ADD     CNWD          $CL                            NUMERO CLIENTI
465100020221     C                   Z-ADD     SNWD          $SP                            SPEDIZIONI
465200020221     C                   Z-ADD     RNWD          $RR                            RICAVI REALI
465300020221     C                   Z-ADD     PNWD          $RP                            RICAVI PRESUNTI
465400020221     C                   Z-ADD     CNW13CD       $CL13C                         N° TOT CLIENTI CORR
465500020221     C                   Z-ADD     CN213CD       $C213C                         N° TOT CLIENTI PREC
465600020222     C*
465700020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
465800020222IF  2C     *IN01         IFEQ      *ON
465900020222     C                   EXSR      STAINT
466000020222E   2C                   ENDIF
466100030311     C*
466200030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
466300070821     C*****              EVAL      DEPSTPD = '2'
466400070821     C                   EVAL      DEPSTPD = '3'
466500020221     C*
466600020221IF  1C     COMCLI        IFGT      *ZEROS
466700020221     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
466800020221     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
466900020221     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
467000020221     C                   EXSR      STMDAT
467100100129     c                   if        parSTAMPA <> 'N'
467200020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
467300100129     c                   end
467400020221E   1C                   ENDIF
467500020318     C*
467600020318     C                   ELSE
467700020318     C* T O T A L I   N U O V I   'D'
467800020318     C                   Z-ADD     CNWD          $CL                            NUMERO CLIENTI
467900020318     C                   Z-ADD     SNWD          $SP                            SPEDIZIONI
468000020318     C                   Z-ADD     RNWD          $RR                            RICAVI REALI
468100020318     C                   Z-ADD     PNWD          $RP                            RICAVI PRESUNTI
468200020318     C                   Z-ADD     CNW13CD       $CL13C                         N° TOT CLIENTI CORR
468300020318     C                   Z-ADD     CN213CD       $C213C                         N° TOT CLIENTI PREC
468400020318     C*
468500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
468600020318IF  2C     *IN01         IFEQ      *ON
468700020318     C                   EXSR      STAINT
468800020318E   2C                   ENDIF
468900030311     C*
469000030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
469100070821     C******             EVAL      DEPSTPD = '2'
469200070821     C                   EVAL      DEPSTPD = '3'
469300020318     C*
469400020318IF  1C     COMCLI        IFGT      *ZEROS
469500020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
469600020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
469700020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
469800020318     C                   EXSR      STMDAT
469900100129     c                   if        parSTAMPA <> 'N'
470000020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
470100100129     c                   end
470200020318E   1C                   ENDIF
470300020318     C***
470400020318     C* T O T A L I   N U O V I   "D"   X   N E T W O R K
470500020318     C***
470600020404     C*
470700020404     C* SOLO SE RICHIESTO NEL LANCIO
470800020404     C                   IF        PARNTW = 'S'
470900020404     C*
471000020319     C                   MOVEL     '0'           FLGSTPRI          1
471100020319DO  1C     2             DO        5             K
471200020318     C     K             OCCUR     ABCSTAND
471300020318     C*
471400020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
471500020318IF  2C     *IN01         IFEQ      *ON
471600020318     C                   EXSR      STAINT
471700020318E   2C                   ENDIF
471800020318     C*
471900020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
472000020318     C                   EVAL      DEPSTPD = '2'
472100020318     C*
472200020318     C                   XFOOT     CNSND         COMCLI           16 0          *NUOVI DI CATEGORIA
472300020318IF  2C     COMCLI        IFGT      *ZEROS
472400020319     C                   MOVEL     '1'           FLGSTPRI
472500020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
472600020318     C                   Z-ADD     K             $Q                             NETWORK
472700020318     C                   MOVEL     '1'           FLGSTPNTW
472800020318     C                   Z-ADD     CNSND         $CL                            NUMERO CLIENTI
472900020318     C                   Z-ADD     SNSND         $SP                            SPEDIZIONI
473000020318     C                   Z-ADD     RNSND         $RR                            RICAVI REALI
473100020318     C                   Z-ADD     PNSND         $RP                            RICAVI PRESUNTI
473200020318     C*
473300020318     C     K             OCCUR     ABCS13ND
473400020318     C                   Z-ADD     NTSND(1)      $CL13C           11 0          N° TOT CLIENTI CORR
473500020318     C                   Z-ADD     NTSND(2)      $C213C           11 0          N° TOT CLIENTI PREC
473600020318     C*
473700020318     C                   EXSR      STMDAT
473800020318E   2C                   ENDIF
473900020318     C*
474000020318E   1C                   ENDDO
474100020319     C                   IF        FLGSTPRI = '1'
474200100129     c                   if        parSTAMPA <> 'N'
474300020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
474400100129     c                   end
474500020318     C                   ENDIF
474600020319     C                   ENDIF
474700020221     C*
474800020404     C                   ENDIF
474900020404     C*
475000020221     C***
475100020318     C* CLIENTI  A C Q U I S I T I
475200000000     C***
475300020221     C*
475400980521DO  1C     1             DO        4             Q
475500000000     C     Q             OCCUR     ABCSTA
475600000000     C*
475700000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
475800000000IF  2C     *IN01         IFEQ      *ON
475900020215     C                   EXSR      STAINT
476000000000E   2C                   ENDIF
476100020222     C*
476200020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
476300020222     C                   EVAL      DEPSTPD = '0'
476400000000     C*
476500020116     C                   XFOOT     CAS           COMCLI                         *NUOVI DI CATEGORIA
476600000000IF  2C     COMCLI        IFGT      *ZEROS
476700000000     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
476800000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
476900020318     C                   MOVEL     '0'           FLGSTPNTW
477000000000     C                   Z-ADD     CAS           $CL                            NUMERO CLIENTI
477100000000     C                   Z-ADD     SAS           $SP                            SPEDIZIONI
477200000000     C                   Z-ADD     RAS           $RR                            RICAVI REALI
477300000000     C                   Z-ADD     PAS           $RP                            RICAVI PRESUNTI
477400000000     C*
477500000000     C     Q             OCCUR     ABCS13
477600020116     C                   Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
477700020116     C                   Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
477800000000     C                   EXSR      STMDAT
477900000000E   2C                   ENDIF
478000000000     C*
478100000000E   1C                   ENDDO
478200020318     C*
478300020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
478400020318     C                   IF        PARSTA = 'K'
478500000000     C* T O T A L I   A C Q U I S I T I
478600000000     C                   Z-ADD     CAW           $CL                            NUMERO CLIENTI
478700000000     C                   Z-ADD     SAW           $SP                            SPEDIZIONI
478800000000     C                   Z-ADD     RAW           $RR                            RICAVI REALI
478900000000     C                   Z-ADD     PAW           $RP                            RICAVI PRESUNTI
479000951227     C                   Z-ADD     CAW13C        $CL13C                         N° TOT CLIENTI CORR
479100951227     C                   Z-ADD     CA213C        $C213C                         N° TOT CLIENTI PREC
479200020222     C*
479300020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
479400020222IF  2C     *IN01         IFEQ      *ON
479500020222     C                   EXSR      STAINT
479600020222E   2C                   ENDIF
479700020222     C*
479800020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
479900020222     C                   EVAL      DEPSTPD = '1'
480000000000     C*
480100020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
480200000000IF  1C     COMCLI        IFGT      *ZEROS
480300000000     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
480400000000     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
480500000000     C                   EXSR      STMDAT
480600100129     c                   if        parSTAMPA <> 'N'
480700020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
480800100129     c                   end
480900020221E   1C                   ENDIF
481000020318     C*
481100020318     C                   ELSE
481200020318     C* T O T A L I   A C Q U I S I T I
481300020318     C                   Z-ADD     CAW           $CL                            NUMERO CLIENTI
481400020318     C                   Z-ADD     SAW           $SP                            SPEDIZIONI
481500020318     C                   Z-ADD     RAW           $RR                            RICAVI REALI
481600020318     C                   Z-ADD     PAW           $RP                            RICAVI PRESUNTI
481700020318     C                   Z-ADD     CAW13C        $CL13C                         N° TOT CLIENTI CORR
481800020318     C                   Z-ADD     CA213C        $C213C                         N° TOT CLIENTI PREC
481900020318     C*
482000020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
482100020318IF  2C     *IN01         IFEQ      *ON
482200020318     C                   EXSR      STAINT
482300020318E   2C                   ENDIF
482400020318     C*
482500020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
482600020318     C                   EVAL      DEPSTPD = '1'
482700020318     C*
482800020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
482900020318IF  1C     COMCLI        IFGT      *ZEROS
483000050308IF  2C     $c213c        orGT      *ZEROS
483100020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
483200020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
483300020318     C                   EXSR      STMDAT
483400100129     c                   if        parSTAMPA <> 'N'
483500020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
483600100129     c                   end
483700020318E   1C                   ENDIF
483800020318     C***
483900020318     C* T O T A L I   A C Q I S I T I   X   N E T W O R K
484000020318     C***
484100020404     C*
484200020404     C* SOLO SE RICHIESTO NEL LANCIO
484300020404     C                   IF        PARNTW = 'S'
484400020404     C*
484500020319     C                   MOVEL     '0'           FLGSTPRI          1
484600020319     C*
484700020319DO  1C     2             DO        5             K
484800020318     C     K             OCCUR     ABCSTAN
484900020318     C*
485000020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
485100020318IF  2C     *IN01         IFEQ      *ON
485200020318     C                   EXSR      STAINT
485300020318E   2C                   ENDIF
485400020318     C*
485500020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
485600020318     C                   EVAL      DEPSTPD = '1'
485700050309     C     K             OCCUR     ABCS13N
485800050309     C                   Z-ADD     ATSN(1)       $CL13C                         N° TOT CLIENTI CORR
485900050309     C                   Z-ADD     ATSN(2)       $C213C                         N° TOT CLIENTI PREC
486000020318     C*
486100020318     C                   XFOOT     CASN          COMCLI                         *NUOVI DI CATEGORIA
486200020318IF  2C     COMCLI        IFGT      *ZEROS
486300050309IF  2C     $c213c        orGT      *ZEROS
486400020319     C                   MOVEL     '1'           FLGSTPRI
486500020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
486600020318     C                   Z-ADD     K             $Q                             NETWORK
486700020318     C                   MOVEL     '1'           FLGSTPNTW
486800020318     C                   Z-ADD     CASN          $CL                            NUMERO CLIENTI
486900020318     C                   Z-ADD     SASN          $SP                            SPEDIZIONI
487000020318     C                   Z-ADD     RASN          $RR                            RICAVI REALI
487100020318     C                   Z-ADD     PASN          $RP                            RICAVI PRESUNTI
487200050309     C* ???????
487300050309     C**** K             OCCUR     ABCS13N
487400050309     C*********          Z-ADD     ATSN(1)       $CL13C                         N° TOT CLIENTI CORR
487500050309     C*********          Z-ADD     ATSN(2)       $C213C                         N° TOT CLIENTI PREC
487600020318     C                   EXSR      STMDAT
487700020318E   2C                   ENDIF
487800020318     C*
487900020318E   1C                   ENDDO
488000020319     C                   IF        FLGSTPRI = '1'
488100100129     c                   if        parSTAMPA <> 'N'
488200020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
488300100129     c                   end
488400020318     C                   ENDIF
488500020319     C                   ENDIF
488600020404     C*
488700020404     C                   ENDIF
488800020221     C***
488900020221     C* CLIENTI  A C Q U I S I T I  'D'
489000020221     C***
489100020221     C*
489200020221     C     5             OCCUR     ABCSTA
489300020221     C*
489400020221     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
489500020221IF  2C     *IN01         IFEQ      *ON
489600020221     C                   EXSR      STAINT
489700020221E   2C                   ENDIF
489800020222     C*
489900020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
490000020222     C                   EVAL      DEPSTPD = '0'
490100050309     C     5             OCCUR     ABCS13
490200050309     C                   Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
490300050309     C                   Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
490400020221     C*
490500020221     C                   XFOOT     CAS           COMCLI                         *NUOVI DI CATEGORIA
490600020221IF  2C     COMCLI        IFGT      *ZEROS
490700050309IF  2C     $c213c        orGT      *ZEROS
490800020221     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
490900020221     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
491000020318     C                   MOVEL     '0'           FLGSTPNTW
491100020221     C                   Z-ADD     CAS           $CL                            NUMERO CLIENTI
491200020221     C                   Z-ADD     SAS           $SP                            SPEDIZIONI
491300020221     C                   Z-ADD     RAS           $RR                            RICAVI REALI
491400020221     C                   Z-ADD     PAS           $RP                            RICAVI PRESUNTI
491500050309     C* ???????
491600050309     C**** 5             OCCUR     ABCS13
491700050309     C**********         Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
491800050309     C**********         Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
491900020221     C                   EXSR      STMDAT
492000020221E   2C                   ENDIF
492100020318     C*
492200020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
492300020318     C                   IF        PARSTA = 'K'
492400020221     C* T O T A L I   A C Q U I S I T I  'D'
492500020221     C                   Z-ADD     CAWD          $CL                            NUMERO CLIENTI
492600020221     C                   Z-ADD     SAWD          $SP                            SPEDIZIONI
492700020221     C                   Z-ADD     RAWD          $RR                            RICAVI REALI
492800020221     C                   Z-ADD     PAWD          $RP                            RICAVI PRESUNTI
492900020221     C                   Z-ADD     CAW13CD       $CL13C                         N° TOT CLIENTI CORR
493000020221     C                   Z-ADD     CA213CD       $C213C                         N° TOT CLIENTI PREC
493100020222     C*
493200020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
493300020222IF  2C     *IN01         IFEQ      *ON
493400020222     C                   EXSR      STAINT
493500020222E   2C                   ENDIF
493600020222     C*
493700020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
493800030520     C                   EVAL      DEPSTPD = '3'
493900020221     C*
494000020221IF  1C     COMCLI        IFGT      *ZEROS
494100020221     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
494200020221     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
494300020221     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
494400020221     C                   EXSR      STMDAT
494500100129     c                   if        parSTAMPA <> 'N'
494600020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
494700100129     c                   end
494800020221E   1C                   ENDIF
494900020318     C*
495000020318     C                   ELSE
495100020318     C* T O T A L I   A C Q U I S I T I  'D'
495200020318     C                   Z-ADD     CAWD          $CL                            NUMERO CLIENTI
495300020318     C                   Z-ADD     SAWD          $SP                            SPEDIZIONI
495400020318     C                   Z-ADD     RAWD          $RR                            RICAVI REALI
495500020318     C                   Z-ADD     PAWD          $RP                            RICAVI PRESUNTI
495600020318     C                   Z-ADD     CAW13CD       $CL13C                         N° TOT CLIENTI CORR
495700020318     C                   Z-ADD     CA213CD       $C213C                         N° TOT CLIENTI PREC
495800020318     C*
495900020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
496000020318IF  2C     *IN01         IFEQ      *ON
496100020318     C                   EXSR      STAINT
496200020318E   2C                   ENDIF
496300020318     C*
496400020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
496500030520     C                   EVAL      DEPSTPD = '3'
496600020318     C*
496700050308     C                   xfoot     $cl           comcli                         *NUOVI DI CATEGORIA
496800020318IF  1C     COMCLI        IFGT      *ZEROS
496900050308IF  2C     $c213c        orGT      *ZEROS
497000050308     C********           XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
497100020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
497200020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
497300020318     C                   EXSR      STMDAT
497400100129     c                   if        parSTAMPA <> 'N'
497500020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
497600100129     c                   end
497700020318E   1C                   ENDIF
497800020318     C***
497900050308     C* T O T A L I   A C Q U I S I T I   X   N E T W O R K  "D"
498000020318     C***
498100020404     C*
498200020404     C* SOLO SE RICHIESTO NEL LANCIO
498300020404     C                   IF        PARNTW = 'S'
498400020404     C*
498500020319     C                   MOVEL     '0'           FLGSTPRI          1
498600020319     C*
498700020319DO  1C     2             DO        5             K
498800020318     C     K             OCCUR     ABCSTAND
498900020318     C*
499000020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
499100020318IF  2C     *IN01         IFEQ      *ON
499200020318     C                   EXSR      STAINT
499300020318E   2C                   ENDIF
499400020318     C*
499500020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
499600020318     C                   EVAL      DEPSTPD = '2'
499700020318     C*
499800050309     C     K             OCCUR     ABCS13ND
499900050309     C                   Z-ADD     ATSND(1)      $CL13C                         N° TOT CLIENTI CORR
500000050309     C                   Z-ADD     ATSND(2)      $C213C                         N° TOT CLIENTI PREC
500100050308     C*
500200020318     C                   XFOOT     CASND         COMCLI                         *NUOVI DI CATEGORIA
500300020318IF  2C     COMCLI        IFGT      *ZEROS
500400050309IF  2C     $c213c        orGT      *ZEROS
500500020319     C                   MOVEL     '1'           FLGSTPRI
500600020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
500700020318     C                   Z-ADD     K             $Q                             NETWORK
500800020318     C                   MOVEL     '1'           FLGSTPNTW
500900020318     C                   Z-ADD     CASND         $CL                            NUMERO CLIENTI
501000020318     C                   Z-ADD     SASND         $SP                            SPEDIZIONI
501100020318     C                   Z-ADD     RASND         $RR                            RICAVI REALI
501200020318     C                   Z-ADD     PASND         $RP                            RICAVI PRESUNTI
501300050309     C* ???????
501400050309     C**** K             OCCUR     ABCS13ND
501500050309     C********           Z-ADD     ATSND(1)      $CL13C                         N° TOT CLIENTI CORR
501600050309     C********           Z-ADD     ATSND(2)      $C213C                         N° TOT CLIENTI PREC
501700020318     C                   EXSR      STMDAT
501800020318E   2C                   ENDIF
501900020318     C*
502000020318E   1C                   ENDDO
502100020319     C                   IF        FLGSTPRI = '1'
502200100129     c                   if        parSTAMPA <> 'N'
502300020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
502400100129     c                   end
502500020318     C                   ENDIF
502600020319     C                   ENDIF
502700020404     C*
502800020404     C                   ENDIF
502900020221     C*
503000020319     C* PRIMA DEI TOTALI STAMPO SEMPRE E CMQ 1 RIGA DOPPIA X EVIDENZIARE L'INIZIO DEI TOTALI
503100100129     c                   if        parSTAMPA <> 'N'
503200020319     C  N53              WRITE     SE94R3                                       *RIGA SEPARAZIONE DO
503300100129     c                   end
503400020319     C*
503500020318     C***
503600020318     C* CLIENTI  T O T A L I
503700000000     C***
503800020221     C*
503900980521DO  1C     1             DO        4             Q
504000000000     C     Q             OCCUR     ABCSTA
504100000000     C*
504200000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
504300000000IF  2C     *IN01         IFEQ      *ON
504400020215     C                   EXSR      STAINT
504500000000E   2C                   ENDIF
504600020222     C*
504700020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
504800020222     C                   EVAL      DEPSTPD = '0'
504900000000     C*
505000000000     C     CNS           ADD       CAS           $CL                            NUMERO CLIENTI
505100000000     C     SNS           ADD       SAS           $SP                            SPEDIZIONI
505200000000     C     RNS           ADD       RAS           $RR                            RICAVI REALI
505300000000     C     PNS           ADD       PAS           $RP                            RICAVI PRESUNTI
505400000000     C*
505500000000     C     Q             OCCUR     ABCS13
505600020116     C     NTS(1)        ADD       ATS(1)        $CL13C                         N° TO CLIENTI CORR
505700020116     C     NTS(2)        ADD       ATS(2)        $C213C                         N° TO CLIENTI PREC
505800000000     C*
505900020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
506000000000IF  2C     COMCLI        IFGT      *ZEROS
506100050301IF  2C     $c213c        orGT      *ZEROS
506200000000     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
506300000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
506400020318     C                   MOVEL     '0'           FLGSTPNTW
506500000000     C                   EXSR      STMDAT
506600000000E   2C                   ENDIF
506700000000     C*
506800000000E   1C                   ENDDO
506900020318     C*
507000020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
507100020318     C                   IF        PARSTA = 'K'
507200000000     C* T O T A L I   T O T A L I
507300000000     C     CNW           ADD       CAW           $CL                            NUMERO CLIENTI
507400000000     C     SNW           ADD       SAW           $SP                            SPEDIZIONI
507500000000     C     RNW           ADD       RAW           $RR                            RICAVI REALI
507600000000     C     PNW           ADD       PAW           $RP                            RICAVI PRESUNTI
507700951227     C     CNW13C        ADD       CAW13C        $CL13C                         N° TOT CLIENTI CORR
507800951227     C     CN213C        ADD       CA213C        $C213C                         N° TOT CLIENTI CORR
507900020222     C*
508000020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
508100020222IF  2C     *IN01         IFEQ      *ON
508200020222     C                   EXSR      STAINT
508300020222E   2C                   ENDIF
508400020222     C*
508500020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
508600020222     C                   EVAL      DEPSTPD = '1'
508700000000     C*
508800020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
508900000000IF  1C     COMCLI        IFGT      *ZEROS
509000050301IF  2C     $c213c        orGT      *ZEROS
509100000000     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
509200000000     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
509300000000     C                   EXSR      STMDAT
509400100129     c                   if        parSTAMPA <> 'N'
509500020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
509600100129     c                   end
509700020221E   1C                   ENDIF
509800020318     C*
509900020318     C                   ELSE
510000020318     C* T O T A L I   T O T A L I
510100020318     C     CNW           ADD       CAW           $CL                            NUMERO CLIENTI
510200020318     C     SNW           ADD       SAW           $SP                            SPEDIZIONI
510300020318     C     RNW           ADD       RAW           $RR                            RICAVI REALI
510400020318     C     PNW           ADD       PAW           $RP                            RICAVI PRESUNTI
510500020318     C     CNW13C        ADD       CAW13C        $CL13C                         N° TOT CLIENTI CORR
510600020318     C     CN213C        ADD       CA213C        $C213C                         N° TOT CLIENTI CORR
510700020318     C*
510800020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
510900020318IF  2C     *IN01         IFEQ      *ON
511000020318     C                   EXSR      STAINT
511100020318E   2C                   ENDIF
511200020318     C*
511300020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
511400020318     C                   EVAL      DEPSTPD = '1'
511500020318     C*
511600020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
511700020318IF  1C     COMCLI        IFGT      *ZEROS
511800050301IF  2C     $c213c        orGT      *ZEROS
511900020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
512000020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
512100020318     C                   EXSR      STMDAT
512200100129     c                   if        parSTAMPA <> 'N'
512300020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
512400100129     c                   end
512500020318E   1C                   ENDIF
512600020318     C* T O T A L I   T O T A L I   X   N E T W O R K
512700020404     C*
512800020404     C* SOLO SE RICHIESTO NEL LANCIO
512900020404     C                   IF        PARNTW = 'S'
513000020404     C*
513100020319     C                   MOVEL     '0'           FLGSTPRI          1
513200020319     C*
513300020319DO  1C     2             DO        5             K
513400020318     C     K             OCCUR     ABCSTAN
513500020318     C*
513600020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
513700020318IF  2C     *IN01         IFEQ      *ON
513800020318     C                   EXSR      STAINT
513900020318E   2C                   ENDIF
514000020318     C*
514100020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
514200020318     C                   EVAL      DEPSTPD = '1'
514300020318     C*
514400020318     C     CNSN          ADD       CASN          $CL                            NUMERO CLIENTI
514500020318     C     SNSN          ADD       SASN          $SP                            SPEDIZIONI
514600020318     C     RNSN          ADD       RASN          $RR                            RICAVI REALI
514700020318     C     PNSN          ADD       PASN          $RP                            RICAVI PRESUNTI
514800020318     C*
514900020318     C     K             OCCUR     ABCS13N
515000020318     C     NTSN(1)       ADD       ATSN(1)       $CL13C                         N° TO CLIENTI CORR
515100020318     C     NTSN(2)       ADD       ATSN(2)       $C213C                         N° TO CLIENTI PREC
515200020318     C*
515300020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
515400020318IF  2C     COMCLI        IFGT      *ZEROS
515500050301IF  2C     $c213c        orGT      *ZEROS
515600020319     C                   MOVEL     '1'           FLGSTPRI
515700020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
515800020318     C                   Z-ADD     K             $Q                             NETWORK
515900020318     C                   MOVEL     '1'           FLGSTPNTW
516000020318     C                   EXSR      STMDAT
516100020318E   2C                   ENDIF
516200020318     C*
516300020318E   1C                   ENDDO
516400020319     C                   IF        FLGSTPRI = '1'
516500100129     c                   if        parSTAMPA <> 'N'
516600020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
516700100129     c                   end
516800020318     C                   ENDIF
516900020319     C                   ENDIF
517000020404     C*
517100020404     C                   ENDIF
517200020220     C***
517300020220     C* CLIENTI  T O T A L I  'D'
517400020220     C***
517500020221     C*
517600020220     C     5             OCCUR     ABCSTA
517700020220     C*
517800020220     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
517900020220IF  2C     *IN01         IFEQ      *ON
518000020220     C                   EXSR      STAINT
518100020220E   2C                   ENDIF
518200020222     C*
518300020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
518400020222     C                   EVAL      DEPSTPD = '0'
518500020220     C*
518600020220     C     CNS           ADD       CAS           $CL                            NUMERO CLIENTI
518700020220     C     SNS           ADD       SAS           $SP                            SPEDIZIONI
518800020220     C     RNS           ADD       RAS           $RR                            RICAVI REALI
518900020220     C     PNS           ADD       PAS           $RP                            RICAVI PRESUNTI
519000020220     C*
519100020220     C     5             OCCUR     ABCS13
519200020220     C     NTS(1)        ADD       ATS(1)        $CL13C                         N° TO CLIENTI CORR
519300020220     C     NTS(2)        ADD       ATS(2)        $C213C                         N° TO CLIENTI PREC
519400020220     C*
519500020220     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
519600020220IF  2C     COMCLI        IFGT      *ZEROS
519700050301IF  2C     $c213c        orGT      *ZEROS
519800020220     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
519900020220     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
520000020318     C                   MOVEL     '0'           FLGSTPNTW
520100020220     C                   EXSR      STMDAT
520200020220E   2C                   ENDIF
520300020318     C*
520400020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
520500020318     C                   IF        PARSTA = 'K'
520600020221     C* T O T A L I   T O T A L I   'D'
520700020221     C     CNWD          ADD       CAWD          $CL                            NUMERO CLIENTI
520800020221     C     SNWD          ADD       SAWD          $SP                            SPEDIZIONI
520900020221     C     RNWD          ADD       RAWD          $RR                            RICAVI REALI
521000020221     C     PNWD          ADD       PAWD          $RP                            RICAVI PRESUNTI
521100020221     C     CNW13CD       ADD       CAW13CD       $CL13C                         N° TOT CLIENTI CORR
521200020221     C     CN213CD       ADD       CA213CD       $C213C                         N° TOT CLIENTI CORR
521300020222     C*
521400020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
521500020222IF  2C     *IN01         IFEQ      *ON
521600020222     C                   EXSR      STAINT
521700020222E   2C                   ENDIF
521800030311     C*
521900030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D' + TOTALE ALT
522000030311     C                   EVAL      DEPSTPD = '3'
522100020220     C*
522200020221IF  1C     COMCLI        IFGT      *ZEROS
522300050301IF  2C     $c213c        orGT      *ZEROS
522400020220     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
522500020220     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
522600020220     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
522700020220     C                   EXSR      STMDAT
522800100129     c                   if        parSTAMPA <> 'N'
522900020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
523000100129     c                   end
523100020221E   1C                   ENDIF
523200020318     C*
523300020318     C                   ELSE
523400020318     C* T O T A L I   T O T A L I   'D'
523500020318     C     CNWD          ADD       CAWD          $CL                            NUMERO CLIENTI
523600020318     C     SNWD          ADD       SAWD          $SP                            SPEDIZIONI
523700020318     C     RNWD          ADD       RAWD          $RR                            RICAVI REALI
523800020318     C     PNWD          ADD       PAWD          $RP                            RICAVI PRESUNTI
523900020318     C     CNW13CD       ADD       CAW13CD       $CL13C                         N° TOT CLIENTI CORR
524000020318     C     CN213CD       ADD       CA213CD       $C213C                         N° TOT CLIENTI CORR
524100020318     C*
524200020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
524300020318IF  2C     *IN01         IFEQ      *ON
524400020318     C                   EXSR      STAINT
524500020318E   2C                   ENDIF
524600030311     C*
524700030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D' + TOTALE ALT
524800030311     C                   EVAL      DEPSTPD = '3'
524900020318     C*
525000020318IF  1C     COMCLI        IFGT      *ZEROS
525100050301IF  2C     $c213c        orGT      *ZEROS
525200020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
525300020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
525400020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
525500020318     C                   EXSR      STMDAT
525600100129     c                   if        parSTAMPA <> 'N'
525700020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
525800100129     c                   end
525900020318E   1C                   ENDIF
526000020318     C* T O T A L I   T O T A L I   X   N E T W O R K   "D"
526100020404     C*
526200020404     C* SOLO SE RICHIESTO NEL LANCIO
526300020404     C                   IF        PARNTW = 'S'
526400020404     C*
526500020319     C                   MOVEL     '0'           FLGSTPRI          1
526600020319     C*
526700020319DO  1C     2             DO        5             K
526800020318     C     K             OCCUR     ABCSTAND
526900020318     C*
527000020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
527100020318IF  2C     *IN01         IFEQ      *ON
527200020318     C                   EXSR      STAINT
527300020318E   2C                   ENDIF
527400020318     C*
527500020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
527600020318     C                   EVAL      DEPSTPD = '2'
527700020318     C*
527800020318     C     CNSND         ADD       CASND         $CL                            NUMERO CLIENTI
527900020318     C     SNSND         ADD       SASND         $SP                            SPEDIZIONI
528000020318     C     RNSND         ADD       RASND         $RR                            RICAVI REALI
528100020318     C     PNSND         ADD       PASND         $RP                            RICAVI PRESUNTI
528200020318     C*
528300020318     C     K             OCCUR     ABCS13ND
528400020318     C     NTSND(1)      ADD       ATSND(1)      $CL13C                         N° TO CLIENTI CORR
528500020318     C     NTSND(2)      ADD       ATSND(2)      $C213C                         N° TO CLIENTI PREC
528600020318     C*
528700020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
528800020318IF  2C     COMCLI        IFGT      *ZEROS
528900050301IF  2C     $C213C        orGT      *ZEROS
529000020319     C                   MOVEL     '1'           FLGSTPRI
529100020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
529200020318     C                   Z-ADD     K             $Q                             NETWORK
529300020318     C                   MOVEL     '1'           FLGSTPNTW
529400020318     C                   EXSR      STMDAT
529500020318E   2C                   ENDIF
529600020318     C*
529700020318E   1C                   ENDDO
529800020319     C                   IF        FLGSTPRI = '1'
529900100129     c                   if        parSTAMPA <> 'N'
530000020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
530100100129     c                   end
530200020318     C                   ENDIF
530300020319     C                   ENDIF
530400000000     C*
530500020404     C                   ENDIF
530600020404     C*
530700000000     C                   ENDSR
530800000000     C*------------------------------------------------------------------------*
530900011123     C*?STAINT - STAMPA INTESTAZIONE
531000000000     C*------------------------------------------------------------------------*
531100000000     C     STAINT        BEGSR
531200020221     C*
531300020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
531400020222     C                   IF        FLGSTPLE = '1'
531500100129     c                   if        parSTAMPA <> 'N'
531600020318     C                   WRITE     SE94R2
531700020222     C                   WRITE     SE94LE                                       *LEGENDA
531800100129     c                   end
531900020222     C                   ELSE
532000020222     C                   EVAL      FLGSTPLE = '1'
532100020222     C                   ENDIF
532200020222     C*
532300100129     c                   if        parSTAMPA <> 'N'
532400020222     C                   WRITE     SE94PT                                       *TESTATA
532500020116     C                   WRITE     SE94IN                                       *RIGA INTESTAZIONE
532600020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
532700020116     C                   WRITE     SE94PI                                       *RIGA MESI
532800020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
532900100129     c                   end
533000000000     C                   SETOFF                                       01
533100000000     C*
533200000000     C                   ENDSR
533300000000     C*------------------------------------------------------------------------*
533400000000     C*?STMDAT - STAMPA I DATI DEL CLIENTE
533500000000     C*          INPUT  --> $CATCL : CATEGORIA CLIENTI (N/A/T)
533600000000     C*                     $Q     : QUALITA' CLIENTI  (A/B/C)
533700000000     C*                     $CL    : NUMERO CLIENTI
533800000000     C*                     $SP    : SPEDIZIONI
533900000000     C*                     $RR    : RICAVI REALI
534000000000     C*                     $RP    : RICAVI PRESUNTI
534100000000     C*                     $CL13C : NUMERO TOTALE CLIENTI (13^ COLONNA)
534200951227     C*                     $C213C : NUMERO TOTALE CLIENTI (1^  COLONNA)
534300000000     C*------------------------------------------------------------------------*
534400000000     C     STMDAT        BEGSR
534500000000     C***
534600000000     C* CLIENTE NUOVI/ACQUISITI/TOTALI DI TIPO A/B/C
534700000000     C***
534800020221     C                   MOVEL     *BLANKS       DES12            12
534900000000IF  1C     $CATCL        IFEQ      'N'                                          *N U O V I
535000000000     C                   MOVEL     DRI(4)        DES12                          *PRIMA DESCRIZIONE
535100000000E   1C                   ENDIF
535200000000IF  1C     $CATCL        IFEQ      'A'                                          *A C Q U I S I T I
535300000000     C                   MOVEL     DRI(8)        DES12                          *PRIMA DESCRIZIONE
535400000000E   1C                   ENDIF
535500000000IF  1C     $CATCL        IFEQ      'T'                                          *T O T A L I
535600000000     C                   MOVEL     DRI(12)       DES12                          *PRIMA DESCRIZIONE
535700000000E   1C                   ENDIF
535800000000     C*
535900020318     C                   IF        FLGSTPNTW = '0'
536000000000IF  2C     $Q            IFGT      *ZEROS
536100000000     C                   MOVE      ABC($Q)       DES12                          *"A"  "B"  "C"  " "
536200000000E   2C                   ENDIF
536300020318     C                   ELSE
536400020318IF  2C     $Q            IFGT      *ZEROS
536500020318     C                   MOVE      DNTW($Q)      DES12                          *NETWORK
536600020318E   2C                   ENDIF
536700020318     C                   ENDIF
536800020221     C*
536900020221     C* SE STO STAMPANDO DATI DI CLIENTI 'D' => VALORIZZO IL CAMPO IN STAMPA X EVIDENZIARLI
537000020222     C                   IF        DEPSTPD = '0'
537100020222     C                   EVAL      P1DFLD = *BLANKS
537200020222     C                   ENDIF
537300020222     C                   IF        DEPSTPD = '1'
537400020222     C                   EVAL      P1DFLD = '*'
537500020222     C                   ENDIF
537600020222     C                   IF        DEPSTPD = '2'
537700020222     C                   EVAL      P1DFLD = '**'
537800020222     C                   ENDIF
537900030311     C                   IF        DEPSTPD = '3'
538000030520     C                   EVAL      P1DFLD = '§'
538100030311     C                   ENDIF
538200000000     C*--------------------
538300020221     C* C L I E N T I
538400000000     C*--------------------
538500000000     C                   MOVEL     DRI(13)       DES07             7            *SECONDA DESCRIZIONE
538600000000     C*
538700000000     C                   Z-ADD     $CL           $VA
538800020116     C                   Z-ADD     $CL13C        $VA13C                         *PERIODO CORRENTE
538900020116     C                   Z-ADD     $C213C        $V213C                         *PERIODO PRECEDENTE
539000000000     C*
539100000000     C                   MOVEL     *BLANKS       P1DDES
539200000000     C                   MOVEL     DES12         P1DDES
539300000000     C                   MOVE      DES07         P1DDES
539400000000     C*
539500000000     C* SCRIVE I VALORI IN STAMPA
539600000000     C                   EXSR      SCRVAL
539700030925     C*
539800030925     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
539900030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
540000030925     C                   MOVEL     PARIOE        SE93IOE                        *DELTA ITALIA/ESTERO
540100030925     C                   MOVEL     INCDIV        SE93DIV                        *DIVISIONE
540200030925     C                   MOVEL     INDDIV        SE93DDI                        *DESCRIZIONE DIVISIO
540300030925     C                   MOVEL     INCARE        SE93ARE                        *AREA
540400030925     C                   MOVEL     INDARE        SE93DAR                        *DESCRIZIONE AREA
540500030925     C                   MOVEL     INCFIL        SE93FIL                        *FILIALE
540600030925     C                   MOVEL     INDFIL        SE93DFI                        *DESCRIZIONE FILIALE
540700030925     C                   MOVEL     P1DFLD        SE93AST                        *CARATTERE STRANO
540800030925     C                   MOVEL     P1DDES        SE93DES                        *DESCRIZIONE
540900030925     C                   MOVEL     *BLANKS       SE93CLV
541000030926IF  2C                   IF        FLGSTPNTW = '0'                              *STAMPA NO NETWORK
541100030926IF  3C     $Q            IFGT      *ZEROS
541200030926     C                   MOVEL     ABC($Q)       SE93CLV                        *QUALITA' CLIENTE
541300030926E   3C                   ENDIF
541400030926E   2C                   ENDIF
541500030926     c*
541600030925     C                   MOVEL     '2'           SE93TIP                        *TIPO TOTALE
541700030925     C                   Z-ADD     $V213C        SE93CPR                        *VALORE PRECEDENTE
541800030925     C                   Z-ADD     $VA(1)        SE93C01                        *MESE UNO
541900030925     C                   Z-ADD     $VA(2)        SE93C02                        *MESE DUE
542000030925     C                   Z-ADD     $VA(3)        SE93C03                        *MESE TRE
542100030925     C                   Z-ADD     $VA(4)        SE93C04                        *MESE QUATTRO
542200030925     C                   Z-ADD     $VA(5)        SE93C05                        *MESE CINQUE
542300030925     C                   Z-ADD     $VA(6)        SE93C06                        *MESE SEI
542400030925     C                   Z-ADD     $VA(7)        SE93C07                        *MESE SETTE
542500030925     C                   Z-ADD     $VA(8)        SE93C08                        *MESE OTTO
542600030925     C                   Z-ADD     $VA(9)        SE93C09                        *MESE NOVE
542700030925     C                   Z-ADD     $VA(10)       SE93C10                        *MESE DIECI
542800030925     C                   Z-ADD     $VA(11)       SE93C11                        *MESE UNDICI
542900030925     C                   Z-ADD     $VA(12)       SE93C12                        *MESE DODICI
543000030925     C                   Z-ADD     $VA13C        SE93CTO                        *TOTALE MESI
543100030925E   1C                   ENDIF
543200000000     C*--------------------
543300000000     C* S P E D I Z I O N I
543400000000     C*--------------------
543500000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIIZONE
543600000000     C                   MOVEL     DRI(14)       DES07                          *SECONDA DESCRIZIONE
543700020221     C                   MOVEL     *BLANKS       P1DFLD
543800000000     C*
543900000000     C                   Z-ADD     $SP           $VA
544000000000     C                   Z-ADD     *ZEROS        $VA13C
544100000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
544200020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPEDIZ
544300000000E   1C                   ENDDO
544400020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
544500951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
544600000000     C*
544700000000     C                   MOVEL     *BLANKS       P1DDES
544800000000     C                   MOVEL     DES12         P1DDES
544900000000     C                   MOVE      DES07         P1DDES
545000000000     C*
545100000000     C* SCRIVE I VALORI IN STAMPA
545200000000     C                   EXSR      SCRVAL
545300030925     C*
545400030925     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
545500030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
545600030925     C                   Z-ADD     $V213C        SE93NPR                        *VALORE PRECEDENTE
545700030925     C                   Z-ADD     $VA(1)        SE93N01                        *MESE UNO
545800030925     C                   Z-ADD     $VA(2)        SE93N02                        *MESE DUE
545900030925     C                   Z-ADD     $VA(3)        SE93N03                        *MESE TRE
546000030925     C                   Z-ADD     $VA(4)        SE93N04                        *MESE QUATTRO
546100030925     C                   Z-ADD     $VA(5)        SE93N05                        *MESE CINQUE
546200030925     C                   Z-ADD     $VA(6)        SE93N06                        *MESE SEI
546300030925     C                   Z-ADD     $VA(7)        SE93N07                        *MESE SETTE
546400030925     C                   Z-ADD     $VA(8)        SE93N08                        *MESE OTTO
546500030925     C                   Z-ADD     $VA(9)        SE93N09                        *MESE NOVE
546600030925     C                   Z-ADD     $VA(10)       SE93N10                        *MESE DIECI
546700030925     C                   Z-ADD     $VA(11)       SE93N11                        *MESE UNDICI
546800030925     C                   Z-ADD     $VA(12)       SE93N12                        *MESE DODICI
546900030925     C                   Z-ADD     $VA13C        SE93NTO                        *TOTALE MESI
547000030925E   1C                   ENDIF
547100000000     C*--------------------
547200020220     C* R I C A V I
547300000000     C*--------------------
547400000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIIZONE
547500000000     C                   MOVEL     DRI(15)       DES07                          *SECONDA DESCRIZIONE
547600020221     C                   MOVEL     *BLANKS       P1DFLD
547700000000     C*
547800020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
547900020116     C*    $RR           DIV       1000          $VA
548000020116     C                   Z-ADD     $RR           $VA
548100951227     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
548200000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
548300020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
548400000000E   1C                   ENDDO
548500020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
548600951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
548700000000     C*
548800000000     C                   MOVEL     *BLANKS       P1DDES
548900000000     C                   MOVEL     DES12         P1DDES
549000000000     C                   MOVE      DES07         P1DDES
549100000000     C*
549200000000     C* SCRIVE I VALORI IN STAMPA
549300000000     C                   EXSR      SCRVAL
549400030925     C*
549500030925     C* IMPOSTA I CAMPI "RICAVI" DEL WFILE DI STAMPA
549600030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
549700030925     C                   Z-ADD     $V213C        SE93RPR                        *VALORE PRECEDENTE
549800030925     C                   Z-ADD     $VA(1)        SE93R01                        *MESE UNO
549900030925     C                   Z-ADD     $VA(2)        SE93R02                        *MESE DUE
550000030925     C                   Z-ADD     $VA(3)        SE93R03                        *MESE TRE
550100030925     C                   Z-ADD     $VA(4)        SE93R04                        *MESE QUATTRO
550200030925     C                   Z-ADD     $VA(5)        SE93R05                        *MESE CINQUE
550300030925     C                   Z-ADD     $VA(6)        SE93R06                        *MESE SEI
550400030925     C                   Z-ADD     $VA(7)        SE93R07                        *MESE SETTE
550500030925     C                   Z-ADD     $VA(8)        SE93R08                        *MESE OTTO
550600030925     C                   Z-ADD     $VA(9)        SE93R09                        *MESE NOVE
550700030925     C                   Z-ADD     $VA(10)       SE93R10                        *MESE DIECI
550800030925     C                   Z-ADD     $VA(11)       SE93R11                        *MESE UNDICI
550900030925     C                   Z-ADD     $VA(12)       SE93R12                        *MESE DODICI
551000030925     C                   Z-ADD     $VA13C        SE93RTO                        *TOTALE MESI
551100030925E   1C                   ENDIF
551200000000     C*--------------------
551300000000     C* D E L T A
551400000000     C*--------------------
551500000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIZIONE
551600000000     C                   MOVEL     DRI(16)       DES07                          *SECONDA DESCRIZIONE
551700020221     C                   MOVEL     *BLANKS       P1DFLD
551800000000     C*
551900000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
552000000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
552100000000     C                   Z-ADD     *ZEROS        $PD13C                         *13^PERCENTUALE DELT
552200000000     C                   EXSR      IMPCAL
552300970822     C***
552400970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
552500970822     C***
552600970822     C                   MOVEL     *BLANKS       $SD                            *FALG SEGNALAZIONE
552700970822     C*
552800000000     C                   MOVEL     *BLANKS       P2DDES
552900020221     C                   MOVEL     *BLANKS       P2DFLD
553000000000     C                   MOVEL     DES12         P2DDES
553100000000     C                   MOVE      DES07         P2DDES
553200000000     C*
553300000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
553400000000     C                   EXSR      SCRDEL
553500030925     C*
553600030925     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
553700030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
553800030925     C                   Z-ADD     $PD(13)       SE93DPR                        *VALORE PRECEDENTE
553900030925     C                   Z-ADD     $PD(1)        SE93D01                        *MESE UNO
554000030925     C                   Z-ADD     $PD(2)        SE93D02                        *MESE DUE
554100030925     C                   Z-ADD     $PD(3)        SE93D03                        *MESE TRE
554200030925     C                   Z-ADD     $PD(4)        SE93D04                        *MESE QUATTRO
554300030925     C                   Z-ADD     $PD(5)        SE93D05                        *MESE CINQUE
554400030925     C                   Z-ADD     $PD(6)        SE93D06                        *MESE SEI
554500030925     C                   Z-ADD     $PD(7)        SE93D07                        *MESE SETTE
554600030925     C                   Z-ADD     $PD(8)        SE93D08                        *MESE OTTO
554700030925     C                   Z-ADD     $PD(9)        SE93D09                        *MESE NOVE
554800030925     C                   Z-ADD     $PD(10)       SE93D10                        *MESE DIECI
554900030925     C                   Z-ADD     $PD(11)       SE93D11                        *MESE UNDICI
555000030925     C                   Z-ADD     $PD(12)       SE93D12                        *MESE DODICI
555100030925     C                   Z-ADD     $PD13C        SE93DTO                        *TOTALE MESI
555200030925     C*
555300030925     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
555400030925IF  2C     PARWCP        IFEQ      'S'
555500030925     c                   ADD       1             WNrr
555600030925     c                   Z-ADD     WNrr          SE93NRR                        *righe in stampa
555700030925     C                   WRITE     WFTISE930
555800030925E   2C                   ENDIF
555900030925E   1C                   ENDIF
556000000000     C*
556100000000     C                   ENDSR
556200020701     C*------------------------------------------------------------------------*
556300020701     C*?REPDELFAT - REPERISCE LE % SCOSTAMENTO FATTURATO TRA IL MESE/ANNO DI ELAB. RISPETTO ALL'ANNO
556400020701     C*------------------------------------------------------------------------*
556500020701     C     REPDELFAT     BEGSR
556600020701     C*
556700020712     C* INIZIALIZZO LA SCHIERA E LE VARIABILI DI WRK
556800020712     C                   CLEAR                   $PDF                           *PERCENTUALI SCOST.
556900020712     C                   Z-ADD     1             W_MM              2 0
557000020712     C                   Z-ADD     *ZEROS        W_PROG           18 3
557100020712     C                   Z-ADD     *ZEROS        W_PROGP          18 3
557200020712     C                   Z-ADD     *ZEROS        W_DELFATT        30 9
557300020712     C*
557400020712     C* SOLO X CLIENTI DI CATEGORIA IMPORTANZA DIVERSA DA 'B' E 'C'
557500020712     C                   IF        DESCLV <> 'C' AND
557600020712     C                             DESCLV <> 'B'
557700020712     C*
557800020712     C* CICLO X I 12 MESI DELLE SCHIERE DEI RICAVI REALI A.C. E A.P.
557900020712     C                   DOW       W_MM <= 12
558000020712     C*
558100020712     C* SOMMO I VALORI DEI MESI A.C. E A.P. X CALCOLARE I RELATIVI PROGRESSIVI A.C. E A.P.
558200020712     C                   ADD       R1K(W_MM)     W_PROG
558300020712     C                   ADD       R1KP(W_MM)    W_PROGP
558400020712     C*
558500020712     C* SOLO ALMENO 1 DEI 2 FATTURATI A.C. E A.P. SONO > 0 CALCOLO LA % SCOSTAMENTO FATTURATO A.C. S
558600020712     C                   IF        R1K(W_MM)  <> *zeros AND
558700020712     C                             R1KP(W_MM) <> *zeros
558800020712     C*
558900020712     C* INIZIALIZZO I LVALORE DELLA % SCOSTAMENTO
559000020712     C                   Z-ADD     *zeros        W_DELFATT
559100020712     C*
559200020712     C* QUINDI LA CALCOLO
559300020712     C                   EVAL      W_DELFATT  = (R1K(W_MM) - R1KP(W_MM)) * 100 /
559400020712     C                                           %abs(R1KP(W_MM))
559500020712     C*
559600020712     C* SE LA % OTTENUTA IN VALORE ASSOLUTO E > 999,9 => FORZO VALORE A 999,9
559700020712     C                   IF        W_DELFATT <= -1000
559800020712     C                   EVAL      W_DELFATT  = -999,9
559900020712     C                   ENDIF
560000020712     C                   IF        W_DELFATT >= +1000
560100020712     C                   EVAL      W_DELFATT  = +999,9
560200020712     C                   ENDIF
560300020712     C*
560400020712     C* SE RIENTRA NEL RANGE RICHIESTO NEL LANCIO VALORIZZO LA SCHIERA DA PORTARE IN STAMPA
560500020712     C                   IF        W_DELFATT  >= PARDFD AND
560600020712     C                             W_DELFATT  <= PARDFA
560700020712     C                   EVAL(H)   $PDF(W_MM)  = W_DELFATT
560800020712     C                   ENDIF
560900020712     C                   ENDIF
561000020712     C*
561100020712     C* INCREMENTO IL MESE
561200020712     C                   ADD       1             W_MM
561300020712     C                   ENDDO
561400020712     C*
561500020712     C* VALORIZZO I CAMPI IN STAMPA RELATIVI AI MESI
561600020712     C                   Z-ADD     $PDF(1)       P2CDE1P                        *MESE UNO
561700020712     C                   Z-ADD     $PDF(2)       P2CDE2P                        *MESE DUE
561800020712     C                   Z-ADD     $PDF(3)       P2CDE3P                        *MESE TRE
561900020712     C                   Z-ADD     $PDF(4)       P2CDE4P                        *MESE QUATTRO
562000020712     C                   Z-ADD     $PDF(5)       P2CDE5P                        *MESE CINQUE
562100020712     C                   Z-ADD     $PDF(6)       P2CDE6P                        *MESE SEI
562200020712     C                   Z-ADD     $PDF(7)       P2CDE7P                        *MESE SETTE
562300020712     C                   Z-ADD     $PDF(8)       P2CDE8P                        *MESE OTTO
562400020712     C                   Z-ADD     $PDF(9)       P2CDE9P                        *MESE NOVE
562500020712     C                   Z-ADD     $PDF(10)      P2CDE0P                        *MESE DIECI
562600020712     C                   Z-ADD     $PDF(11)      P2CDEAP                        *MESE UNDICI
562700020712     C                   Z-ADD     $PDF(12)      P2CDEBP                        *MESE DODICI
562800020712     C*
562900020712     C* DOPO LE VALORIZZAZOINI MENSILI EFFETTUO QUELLE DEL PROGRESSIVO
563000020712     C*
563100020712     C* SOLO SE ENTRAMI I FATTURATI A.C. E A.P. SONO > 0 CALCOLO LA % SCOSTAMENTO FATTURATO A.C. SU
563200020712     C                   IF        W_PROG  <> *zeros AND
563300020712     C                             W_PROGP <> *zeros
563400020712     C*
563500020712     C* INIZIALIZZO I LVALORE DELLA % SCOSTAMENTO
563600020712     C                   Z-ADD     *zeros        W_DELFATT
563700020712     C*
563800020712     C* QUINDI LA CALCOLO
563900020712     C                   EVAL      W_DELFATT  = (W_PROG - W_PROGP) * 100 /
564000020712     C                                           %abs(W_PROGP)
564100020712     C*
564200020712     C* SE LA % OTTENUTA IN VALORE ASSOLUTO E > 999,9 => FORZO VALORE A 999,9
564300020712     C                   IF        W_DELFATT <= -1000
564400020712     C                   EVAL      W_DELFATT  = -999,9
564500020712     C                   ENDIF
564600020712     C                   IF        W_DELFATT >= +1000
564700020712     C                   EVAL      W_DELFATT  = +999,9
564800020712     C                   ENDIF
564900020712     C*
565000020712     C* INIZIALIZZO SEMPRE IL CAMPO IN STAMPA RIFERITO ALLA % PROGRESSIVO
565100020712     C                   CLEAR                   P2CDETP
565200020712     C*
565300020712     C* SE RIENTRA NEL RANGE RICHIESTO NEL LANCIO VALORIZZO LA SCHIERA DA PORTARE IN STAMPA
565400020712     C                   IF        W_DELFATT  >= PARDFDP AND
565500020712     C                             W_DELFATT  <= PARDFAP
565600020712     C                   EVAL(H)   P2CDETP     = W_DELFATT
565700020712     C                   ENDIF
565800020712     C                   ENDIF
565900020712     C*
566000020712     C                   ENDIF
566100020701     C*
566200020701     C                   ENDSR
566300020701     C*------------------------------------------------------------------------*
566400020701     C*?INZPDF - INIZILIZZA I CAMPI LA SCHIERA DI WRK X LO SCOSTAMENTO FATTURATO E I RALATIVI CAMPI
566500020701     C*------------------------------------------------------------------------*
566600020701     C     INZPDF        BEGSR
566700020701     C*
566800020701     C                   CLEAR                   $PDF
566900020701     C*
567000020701     C                   Z-ADD     $PDF(1)       P2CDE1P                        *MESE UNO
567100020701     C                   Z-ADD     $PDF(2)       P2CDE2P                        *MESE DUE
567200020701     C                   Z-ADD     $PDF(3)       P2CDE3P                        *MESE TRE
567300020701     C                   Z-ADD     $PDF(4)       P2CDE4P                        *MESE QUATTRO
567400020701     C                   Z-ADD     $PDF(5)       P2CDE5P                        *MESE CINQUE
567500020701     C                   Z-ADD     $PDF(6)       P2CDE6P                        *MESE SEI
567600020701     C                   Z-ADD     $PDF(7)       P2CDE7P                        *MESE SETTE
567700020701     C                   Z-ADD     $PDF(8)       P2CDE8P                        *MESE OTTO
567800020701     C                   Z-ADD     $PDF(9)       P2CDE9P                        *MESE NOVE
567900020701     C                   Z-ADD     $PDF(10)      P2CDE0P                        *MESE DIECI
568000020701     C                   Z-ADD     $PDF(11)      P2CDEAP                        *MESE UNDICI
568100020701     C                   Z-ADD     $PDF(12)      P2CDEBP                        *MESE DODICI
568200020712     C                   Z-ADD     *ZEROS        P2CDETP                        *PROGRESSIVO
568300020701     C*
568400020701     C                   ENDSR
568500000000     C*------------------------------------------------------------------------*
568600000000     C*?STAMP2 - STAMPA I TOTALI NELLA FORMA: CLI / SPE / RIC / DEL
568700000000     C*------------------------------------------------------------------------*
568800000000     C     STAMP2        BEGSR
568900000000     C*--------------------
569000000000     C* I N T E S T A Z I O N E
569100000000     C*--------------------
569200000000IF  1C     *IN01         IFEQ      *ON
569300020215     C                   EXSR      STAINT
569400000000E   1C                   ENDIF
569500000000     C*--------------------
569600020222     C* C L I E N T I
569700000000     C*--------------------
569800000000     C                   MOVEL     DRI(13)       DES03             3
569900000000     C                   MOVEL     *BLANKS       P1DDES
570000000000     C                   MOVEL     DES20         P1DDES
570100000000     C                   MOVE      DES03         P1DDES
570200020222     C*
570300020222     C* SE STO STAMPANDO DATI DI CLIENTI 'D' => VALORIZZO IL CAMPO IN STAMPA X EVIDENZIARLI
570400020222     C                   IF        DEPSTPD = '0'
570500020222     C                   EVAL      P1DFLD = *BLANKS
570600020222     C                   ENDIF
570700020222     C                   IF        DEPSTPD = '1'
570800020222     C                   EVAL      P1DFLD = '*'
570900020222     C                   ENDIF
571000020222     C                   IF        DEPSTPD = '2'
571100020222     C                   EVAL      P1DFLD = '**'
571200020222     C                   ENDIF
571300030311     C                   IF        DEPSTPD = '3'
571400030520     C                   EVAL      P1DFLD = '§'
571500030311     C                   ENDIF
571600000000     C*
571700000000     C                   Z-ADD     CLW           $VA
571800020116     C                   Z-ADD     CTW(1)        $VA13C                         *PERIODO CORRENTE
571900020116     C                   Z-ADD     CTW(2)        $V213C                         *PERIODO PRECEDENTE
572000000000     C*
572100000000     C* SCRIVE I VALORI IN STAMPA
572200000000     C                   EXSR      SCRVAL
572300030925     C*
572400030925     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
572500030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
572600030925     C                   MOVEL     PARIOE        SE93IOE                        *DELTA ITALIA/ESTERO
572700030925     C                   MOVEL     INCDIV        SE93DIV                        *DIVISIONE
572800030925     C                   MOVEL     INDDIV        SE93DDI                        *DESCRIZIONE DIVISIO
572900030925     C                   MOVEL     INCARE        SE93ARE                        *AREA
573000030925     C                   MOVEL     INDARE        SE93DAR                        *DESCRIZIONE AREA
573100030925     C                   MOVEL     INCFIL        SE93FIL                        *FILIALE
573200030925     C                   MOVEL     INDFIL        SE93DFI                        *DESCRIZIONE FILIALE
573300030926IF  2c     WTipStamp2    IFEQ      'F'                                          *da STAFIL
573400030926     C                   MOVEL     DEPFIL        SE93FIL                        *FILIALE
573500030926     C                   MOVEL     DESFIL        SE93DFI                        *DESCRIZIONE FILIALE
573600030926E   2c                   ENDIF
573700030925     C                   MOVEL     P1DFLD        SE93AST                        *CARATTERE STRANO
573800030925     C                   MOVEL     P1DDES        SE93DES                        *DESCRIZIONE
573900030925     C                   MOVEL     *BLANKS       SE93CLV                        *QUALITA' CLIENTE
574000030925     C                   MOVEL     '1'           SE93TIP                        *TIPO TOTALE
574100030925     C                   Z-ADD     $V213C        SE93CPR                        *VALORE PRECEDENTE
574200030925     C                   Z-ADD     $VA(1)        SE93C01                        *MESE UNO
574300030925     C                   Z-ADD     $VA(2)        SE93C02                        *MESE DUE
574400030925     C                   Z-ADD     $VA(3)        SE93C03                        *MESE TRE
574500030925     C                   Z-ADD     $VA(4)        SE93C04                        *MESE QUATTRO
574600030925     C                   Z-ADD     $VA(5)        SE93C05                        *MESE CINQUE
574700030925     C                   Z-ADD     $VA(6)        SE93C06                        *MESE SEI
574800030925     C                   Z-ADD     $VA(7)        SE93C07                        *MESE SETTE
574900030925     C                   Z-ADD     $VA(8)        SE93C08                        *MESE OTTO
575000030925     C                   Z-ADD     $VA(9)        SE93C09                        *MESE NOVE
575100030925     C                   Z-ADD     $VA(10)       SE93C10                        *MESE DIECI
575200030925     C                   Z-ADD     $VA(11)       SE93C11                        *MESE UNDICI
575300030925     C                   Z-ADD     $VA(12)       SE93C12                        *MESE DODICI
575400030925     C                   Z-ADD     $VA13C        SE93CTO                        *TOTALE MESI
575500030925E   1C                   ENDIF
575600000000     C*--------------------
575700000000     C* S P E D I Z I O N I
575800000000     C*--------------------
575900000000     C                   MOVEL     DRI(14)       DES03             3
576000000000     C                   MOVEL     *BLANKS       P1DDES
576100000000     C                   MOVE      DES03         P1DDES
576200020221     C                   MOVEL     *BLANKS       P1DFLD
576300000000     C*
576400000000     C                   Z-ADD     SPW           $VA
576500020116     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
576600000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
576700000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
576800000000E   1C                   ENDDO
576900020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
577000951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
577100000000     C*
577200000000     C* SCRIVE I VALORI IN STAMPA
577300000000     C                   EXSR      SCRVAL
577400030925     C*
577500030925     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
577600030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
577700030925     C                   Z-ADD     $V213C        SE93NPR                        *VALORE PRECEDENTE
577800030925     C                   Z-ADD     $VA(1)        SE93N01                        *MESE UNO
577900030925     C                   Z-ADD     $VA(2)        SE93N02                        *MESE DUE
578000030925     C                   Z-ADD     $VA(3)        SE93N03                        *MESE TRE
578100030925     C                   Z-ADD     $VA(4)        SE93N04                        *MESE QUATTRO
578200030925     C                   Z-ADD     $VA(5)        SE93N05                        *MESE CINQUE
578300030925     C                   Z-ADD     $VA(6)        SE93N06                        *MESE SEI
578400030925     C                   Z-ADD     $VA(7)        SE93N07                        *MESE SETTE
578500030925     C                   Z-ADD     $VA(8)        SE93N08                        *MESE OTTO
578600030925     C                   Z-ADD     $VA(9)        SE93N09                        *MESE NOVE
578700030925     C                   Z-ADD     $VA(10)       SE93N10                        *MESE DIECI
578800030925     C                   Z-ADD     $VA(11)       SE93N11                        *MESE UNDICI
578900030925     C                   Z-ADD     $VA(12)       SE93N12                        *MESE DODICI
579000030925     C                   Z-ADD     $VA13C        SE93NTO                        *TOTALE MESI
579100030925E   1C                   ENDIF
579200000000     C*--------------------
579300000000     C* R I C A V I
579400000000     C*--------------------
579500000000     C                   MOVEL     DRI(15)       DES03             3
579600000000     C                   MOVEL     *BLANKS       P1DDES
579700000000     C                   MOVE      DES03         P1DDES
579800020221     C                   MOVEL     *BLANKS       P1DFLD
579900000000     C*
580000020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
580100020116     C*    RRW           DIV       1000          $VA
580200020116     C                   Z-ADD     RRW           $VA
580300020116     C                   Z-ADD     *ZEROS        $VA13C
580400000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
580500020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
580600000000E   1C                   ENDDO
580700020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
580800951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
580900000000     C*
581000000000     C* SCRIVE I VALORI IN STAMPA
581100000000     C                   EXSR      SCRVAL
581200030925     C*
581300030925     C* IMPOSTA I CAMPI "RICAVI" DEL WFILE DI STAMPA
581400030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
581500030925     C                   Z-ADD     $V213C        SE93RPR                        *VALORE PRECEDENTE
581600030925     C                   Z-ADD     $VA(1)        SE93R01                        *MESE UNO
581700030925     C                   Z-ADD     $VA(2)        SE93R02                        *MESE DUE
581800030925     C                   Z-ADD     $VA(3)        SE93R03                        *MESE TRE
581900030925     C                   Z-ADD     $VA(4)        SE93R04                        *MESE QUATTRO
582000030925     C                   Z-ADD     $VA(5)        SE93R05                        *MESE CINQUE
582100030925     C                   Z-ADD     $VA(6)        SE93R06                        *MESE SEI
582200030925     C                   Z-ADD     $VA(7)        SE93R07                        *MESE SETTE
582300030925     C                   Z-ADD     $VA(8)        SE93R08                        *MESE OTTO
582400030925     C                   Z-ADD     $VA(9)        SE93R09                        *MESE NOVE
582500030925     C                   Z-ADD     $VA(10)       SE93R10                        *MESE DIECI
582600030925     C                   Z-ADD     $VA(11)       SE93R11                        *MESE UNDICI
582700030925     C                   Z-ADD     $VA(12)       SE93R12                        *MESE DODICI
582800030925     C                   Z-ADD     $VA13C        SE93RTO                        *TOTALE MESI
582900030925E   1C                   ENDIF
583000000000     C*--------------------
583100000000     C* D E L T A
583200000000     C*--------------------
583300000000     C                   MOVEL     DRI(16)       DES03             3
583400000000     C                   MOVEL     *BLANKS       P2DDES
583500020221     C                   MOVEL     *BLANKS       P2DFLD
583600000000     C                   MOVE      DES03         P2DDES
583700000000     C*
583800000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
583900000000     C                   Z-ADD     RRW           $RR                            *RICAVI REALI
584000000000     C                   Z-ADD     RPW           $RP                            *RICAVI PRESUNTI
584100000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
584200000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
584300000000     C                   EXSR      IMPCAL
584400970822     C***
584500970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
584600970822     C***
584700970822     C                   MOVEL     *BLANKS       $SD                            *FALG SEGNALAZIONE
584800000000     C*
584900000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
585000000000     C                   EXSR      SCRDEL
585100030925     C*
585200030925     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
585300030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
585400030925     C                   Z-ADD     $PD(13)       SE93DPR                        *VALORE PRECEDENTE
585500030925     C                   Z-ADD     $PD(1)        SE93D01                        *MESE UNO
585600030925     C                   Z-ADD     $PD(2)        SE93D02                        *MESE DUE
585700030925     C                   Z-ADD     $PD(3)        SE93D03                        *MESE TRE
585800030925     C                   Z-ADD     $PD(4)        SE93D04                        *MESE QUATTRO
585900030925     C                   Z-ADD     $PD(5)        SE93D05                        *MESE CINQUE
586000030925     C                   Z-ADD     $PD(6)        SE93D06                        *MESE SEI
586100030925     C                   Z-ADD     $PD(7)        SE93D07                        *MESE SETTE
586200030925     C                   Z-ADD     $PD(8)        SE93D08                        *MESE OTTO
586300030925     C                   Z-ADD     $PD(9)        SE93D09                        *MESE NOVE
586400030925     C                   Z-ADD     $PD(10)       SE93D10                        *MESE DIECI
586500030925     C                   Z-ADD     $PD(11)       SE93D11                        *MESE UNDICI
586600030925     C                   Z-ADD     $PD(12)       SE93D12                        *MESE DODICI
586700030925     C                   Z-ADD     $PD13C        SE93DTO                        *TOTALE MESI
586800030925     C*
586900030925     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
587000030925IF  2C     PARWCP        IFEQ      'S'
587100030925     c                   ADD       1             WNrr
587200030925     c                   Z-ADD     WNrr          SE93NRR                        *righe in stampa
587300030925     C                   WRITE     WFTISE930
587400030925E   2C                   ENDIF
587500030925E   1C                   ENDIF
587600000000     C*
587700000000     C                   ENDSR
587800000000     C*------------------------------------------------------------------------*
587900000000     C*?IMPCAL - IMPOSTA IL CALCOLO DEL DESTA PER TUTTI I VALORI
588000000000     C*          INPUT  --> $RR    : SCHIERA RICAVO REALE
588100000000     C*                     $RP    : SCHIERA RICAVO PRESUNTO
588200000000     C*          OUTPUT --> $PD    : SCHIERA DELTA PERCENTUALE
588300000000     C*                     $PD13C : 13^ PERCENTUALE DELTA
588400000000     C*------------------------------------------------------------------------*
588500000000     C     IMPCAL        BEGSR
588600000000     C*--------------------
588700000000     C* DELTA PERIODO PRECEDENTE
588800000000     C*--------------------
588900020116     C                   Z-ADD     $RR(13)       WRRW             19 5          *RICAVO REALE
589000020116     C                   Z-ADD     $RP(13)       WRPW             19 5          *RICAVO PRESUNTO
589100000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
589200000000     C                   EXSR      CALDEL
589300000000     C                   Z-ADD     WDPW          $PD(13)                        *SCHIERA DELTA
589400000000     C*--------------------
589500000000     C* DELTA 12 MESI
589600000000     C*--------------------
589700000000DO  1C     1             DO        SPARM         J
589800020116     C                   Z-ADD     $RR(J)        WRRW                           *RICAVO REALE
589900020116     C                   Z-ADD     $RP(J)        WRPW                           *RICAVO PRESUNTO
590000020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
590100000000     C                   EXSR      CALDEL
590200000000     C                   Z-ADD     WDPW          $PD(J)                         *SCHIERA DELTA
590300000000E   1C                   ENDDO
590400000000     C*--------------------
590500000000     C* DELTA 13^ COLONNA
590600000000     C*--------------------
590700000000     C                   Z-ADD     *ZEROS        WRRW
590800000000     C                   Z-ADD     *ZEROS        WRPW
590900000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
591000020116     C                   ADD       $RR(J)        WRRW                           *RICAVO REALE
591100020116     C                   ADD       $RP(J)        WRPW                           *RICAVO PRESUNTO
591200000000E   1C                   ENDDO
591300020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
591400000000     C                   EXSR      CALDEL
591500000000     C                   Z-ADD     WDPW          $PD13C                         *13^ COLONNA
591600000000     C*
591700000000     C                   ENDSR
591800000000     C*------------------------------------------------------------------------*
591900000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
592000000000     C*          INPUT  --> WRRW: RICAVO REALE
592100000000     C*                     WRPW: RICAVO PRESUNTO
592200000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
592300000000     C*------------------------------------------------------------------------*
592400000000     C     CALDEL        BEGSR
592500000000     C*--------------------
592600000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
592700000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
592800000000     C*--------------------
592900000000     C*
593000000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
593100020116     C     WRRW          SUB       WRPW          GAP              19 5          (REALE - PRESUNTO)
593200000000     C     GAP           MULT      100           GAP                            (GAP x 100)
593300000000     C*
593400000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
593500000000IF  1C     WRRW          IFEQ      *ZEROS
593600000000     C                   Z-ADD     1             WRRW
593700000000E   1C                   ENDIF
593800000000     C*
593900000000     C     GAP           DIV       WRRW          GAP
594000000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
594100000000     C*
594200000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
594300000000     C     WDPW          COMP      0                                    28
594400000000     C* POSITIVO
594500000000IF  1C     *IN28         IFEQ      *OFF
594600000000IF  2C     GAP           IFGE      1000
594700000000     C                   Z-ADD     +999,9        WDPW
594800000000E   2C                   ENDIF
594900000000     C* NEGATIVO
595000000000X   1C                   ELSE
595100000000IF  2C     GAP           IFLE      -1000
595200000000     C                   Z-ADD     -999,9        WDPW
595300000000E   2C                   ENDIF
595400000000E   1C                   ENDIF
595500000000     C*
595600000000     C                   ENDSR
595700000000     C*------------------------------------------------------------------------*
595800000000     C*?SCRVAL - SCRIVE I VALORI IN STAMPA
595900000000     C*------------------------------------------------------------------------*
596000000000     C     SCRVAL        BEGSR
596100000000     C*
596200951227     C                   Z-ADD     $V213C        P1CVAP                         *VALORE PRECEDENTE
596300000000     C                   Z-ADD     $VA(1)        P1CVA1                         *MESE UNO
596400000000     C                   Z-ADD     $VA(2)        P1CVA2                         *MESE DUE
596500000000     C                   Z-ADD     $VA(3)        P1CVA3                         *MESE TRE
596600000000     C                   Z-ADD     $VA(4)        P1CVA4                         *MESE QUATTRO
596700000000     C                   Z-ADD     $VA(5)        P1CVA5                         *MESE CINQUE
596800000000     C                   Z-ADD     $VA(6)        P1CVA6                         *MESE SEI
596900000000     C                   Z-ADD     $VA(7)        P1CVA7                         *MESE SETTE
597000000000     C                   Z-ADD     $VA(8)        P1CVA8                         *MESE OTTO
597100000000     C                   Z-ADD     $VA(9)        P1CVA9                         *MESE NOVE
597200000000     C                   Z-ADD     $VA(10)       P1CVA0                         *MESE DIECI
597300000000     C                   Z-ADD     $VA(11)       P1CVAA                         *MESE UNDICI
597400000000     C                   Z-ADD     $VA(12)       P1CVAB                         *MESE DODICI
597500000000     C                   Z-ADD     $VA13C        P1CVAT                         *TOTALE MESI
597600000000     C*
597700100129     c                   if        parSTAMPA <> 'N'
597800020215     C  N53              WRITE     SE94P1                                       *STAMPA RIGA VALORE
597900100129     c                   end
598000011123     C   53              SETOFF                                       53
598100000000     C*
598200000000     C                   ENDSR
598300000000     C*------------------------------------------------------------------------*
598400000000     C*?SCRDEL - SCRIVE I VALORI DELTA IN STAMPA
598500000000     C*------------------------------------------------------------------------*
598600000000     C     SCRDEL        BEGSR
598700000000     C*
598800000000     C                   Z-ADD     $PD(13)       P2CDEP                         *VALORE PRECEDENTE
598900000000     C                   Z-ADD     $PD(1)        P2CDE1                         *MESE UNO
599000970822     C                   MOVEL     $SD(1)        P2CSP1
599100000000     C                   Z-ADD     $PD(2)        P2CDE2                         *MESE DUE
599200970822     C                   MOVEL     $SD(2)        P2CSP2
599300000000     C                   Z-ADD     $PD(3)        P2CDE3                         *MESE TRE
599400970822     C                   MOVEL     $SD(3)        P2CSP3
599500000000     C                   Z-ADD     $PD(4)        P2CDE4                         *MESE QUATTRO
599600970822     C                   MOVEL     $SD(4)        P2CSP4
599700000000     C                   Z-ADD     $PD(5)        P2CDE5                         *MESE CINQUE
599800970822     C                   MOVEL     $SD(5)        P2CSP5
599900000000     C                   Z-ADD     $PD(6)        P2CDE6                         *MESE SEI
600000970822     C                   MOVEL     $SD(6)        P2CSP6
600100000000     C                   Z-ADD     $PD(7)        P2CDE7                         *MESE SETTE
600200970822     C                   MOVEL     $SD(7)        P2CSP7
600300000000     C                   Z-ADD     $PD(8)        P2CDE8                         *MESE OTTO
600400970822     C                   MOVEL     $SD(8)        P2CSP8
600500000000     C                   Z-ADD     $PD(9)        P2CDE9                         *MESE NOVE
600600970822     C                   MOVEL     $SD(9)        P2CSP9
600700000000     C                   Z-ADD     $PD(10)       P2CDE0                         *MESE DIECI
600800970822     C                   MOVEL     $SD(10)       P2CSP0
600900000000     C                   Z-ADD     $PD(11)       P2CDEA                         *MESE UNDICI
601000970822     C                   MOVEL     $SD(11)       P2CSPA
601100000000     C                   Z-ADD     $PD(12)       P2CDEB                         *MESE DODICI
601200970822     C                   MOVEL     $SD(12)       P2CSPB
601300000000     C                   Z-ADD     $PD13C        P2CDET                         *TOTALE MESI
601400000000     C*
601500100129     c                   if        parSTAMPA <> 'N'
601600020215     C  N53              WRITE     SE94P2                                       *STAMPA RIGA VALORE
601700100129     c                   end
601800011123     C   53              SETOFF                                       53
601900000000     C*
602000000000     C                   ENDSR
602100000000     C*------------------------------------------------------------------------*
602200000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
602300000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
602400000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
602500000000     C*------------------------------------------------------------------------*
602600000000     C     DECDIV        BEGSR
602700000000     C*
602800000000     C                   Z-ADD     1             D
602900000000     C     DEPDIV        LOOKUP    DIV(D)                                 99
603000000000IF  1C     *IN99         IFEQ      *OFF
603100000000     C                   MOVEL     *ALL'*'       DESDIV           20
603200000000X   1C                   ELSE
603300000000     C                   MOVEL     DDIV(D)       DESDIV
603400000000E   1C                   ENDIF
603500000000     C*
603600000000     C                   ENDSR
603700000000     C*------------------------------------------------------------------------*
603800000000     C*? DECARE - DECODIFICA CODICE AREA
603900000000     C*          INPUT  -- DEPARE: CODICE AREA
604000000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
604100000000     C*------------------------------------------------------------------------*
604200000000     C     DECARE        BEGSR
604300000000     C*
604400000000     C                   Z-ADD     1             D
604500000000     C     DEPARE        LOOKUP    ARE(D)                                 99
604600000000IF  1C     *IN99         IFEQ      *OFF
604700000000     C                   MOVEL     *ALL'*'       DESARE           20
604800000000X   1C                   ELSE
604900000000     C                   MOVEL     DARE(D)       DESARE
605000000000E   1C                   ENDIF
605100000000     C*
605200000000     C                   ENDSR
605300000000     C*------------------------------------------------------------------------*
605400000000     C*? DECFIL - DECODIFICA CODICE FILIALE
605500000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
605600000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
605700000000     C*------------------------------------------------------------------------*
605800000000     C     DECFIL        BEGSR
605900000000     C*
606000000000     C                   Z-ADD     1             D
606100000000     C     DEPFIL        LOOKUP    FIL(D)                                 99
606200000000IF  1C     *IN99         IFEQ      *OFF
606300000000     C                   MOVEL     *ALL'*'       DESFIL           20
606400000000X   1C                   ELSE
606500000000     C                   MOVEL     DFIL(D)       DESFIL
606600000000E   1C                   ENDIF
606700000000     C*
606800000000     C                   ENDSR
606900000000     C*------------------------------------------------------------------------*
607000000000     C*?DECAGE - DECODIFICA CODICE AGENTE
607100000000     C*          INPUT  -- DEPAGE: CODICE AGENTE COMMERCIALE
607200000000     C*          OUTPUT -- DESAGE: DESCRIZIONE AGENTE
607300000000     C*                    DE2PCF: PERCENTUALE COMPETENZA FILIALE
607400000000     C*------------------------------------------------------------------------*
607500000000     C     DECAGE        BEGSR
607600000000     C*
607700980407     C                   MOVEL     *BLANKS       DESAGE           25
607800000000     C                   Z-ADD     1             D
607900000000     C     DEPAGE        LOOKUP    AGE(D)                                 99
608000000000IF  1C     *IN99         IFEQ      *OFF
608100980407     C                   MOVEL     *ALL'*'       DESAGE
608200000000     C                   Z-ADD     *ZEROS        DESPCF
608300000000     C                   MOVEL     *BLANKS       DE2PCF
608400000000X   1C                   ELSE
608500000000     C                   MOVEL     DAGE(D)       DESAGE
608600000000     C                   Z-ADD     CAGE(D)       DESPCF
608700000000     C*
608800000000     C* METTO IL CAMPO PERCENTUALE IN UNO ALFANUMERICO E LO EDITO(Z)
608900000000     C                   MOVEL     DESPCF        DE2PCF            3
609000000000     C                   Z-ADD     1             P
609100000000     C                   MOVEA     DE2PCF        PCF
609200000000     C*
609300000000DO  2C                   DO        3             P
609400000000IF  3C     PCF(P)        IFEQ      *ZEROS
609500000000     C                   MOVEL     *BLANKS       PCF(P)
609600000000X   3C                   ELSE
609700000000     C                   Z-ADD     4             P
609800000000E   3C                   ENDIF
609900000000E   2C                   ENDDO
610000000000     C                   MOVEA     PCF           DE2PCF
610100000000E   1C                   ENDIF
610200000000     C*
610300000000     C                   ENDSR
610400000000     C*------------------------------------------------------------------------*
610500000000     C*?DECKSC - DECODIFICA CODICE CLIENTE
610600000825     C*          INPUT  -- DEPCLI: CODICE CLIENTE
610700000000     C*          OUTPUT -- DESKSC: RAGIONE SOCIALE CLIENTE
610800951006     C*                    DESITC: DESCRIZIONE CATEGORIA SEAT
610900000000     C*                    DESCLV: QUALITA' CLIENTE
611000000000     C*                    DESCLN: CLIENTE NUOVO O ACQUISITO
611100000000     C*------------------------------------------------------------------------*
611200000000     C     DECKSC        BEGSR
611300120131      *--
611400120131      *-- pulisce x nuovo cliente i campi da evidenziare in seguito
611500120131     c                   eval           Cod_Blocco_Ultimo   = *blank
611600120131     c                   eval           Data_Blocco_Ultimo  = 0
611700120131     c                   eval           Data_Prima_Fat_6    = 0
611800120131      *--
611900000000     C*--------------------
612000000000     C* RAGIONE SOCIALE
612100960708     C* BLOCCO CLIENTE
612200000000     C*--------------------
612300000000     C                   MOVEL     *ALL'*'       DESKSC                         *SE NON TROVATO
612400960708     C                   MOVEL     *BLANKS       DESABL                         *SE NON TROVATO
612500980406     C                   CLEAR                   BS69DS
612600980406     C                   CLEAR                   ACODS
612700980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
612800000825     C                   MOVEL     DEPCLI        I69KAC                         *CLIENTE X CNACO
612900070419     C                   MOVEL     DEPCLI        I69KIN                         *CLIENTE X CNIND
613000980406     C                   CALL      'TIBS69R'
613100980406     C                   PARM                    BS69DS
613200980406     C                   PARM                    ACODS
613300980406     C                   PARM                    INDDS
613400980406     C                   PARM                    CLPDS
613500980406     C                   PARM                    CLSDS
613600980406IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
613700980406     C                   MOVEL     ACORAG        DESKSC                         *RAGIONE SOCIALE
613800980406     C                   MOVEL     ACOABL        DESABL                         *BLOCCO CLIENTE
613900070419      * Nuovi dati anagrafici x i nuovi WADELPCF/E
614000070419     C                   MOVEL     INDvia        DESvia                         *Indirizzo
614100070419     C                   MOVEL     INDcap        DEScap                         *CAP
614200070419     C                   MOVEL     INDcae        DEScae                         *CAP Europeo
614300070419     C                   MOVEL     INDprv        DESprv                         *Provincia
614400070419     C                   MOVEL     INDcit        DEScit                         *Città
614500070419     C                   MOVEL     INDsta        DESsta                         *Nazione
614600070615     C                   MOVEL     INDcdf        DEScdf                         *Cod.Fiscale
614700070615     C                   MOVEL     INDiva        DESiva                         *Part.IVA
614800070618     C                   MOVEL     INDtel        DEStel                         *Telefono
614900980406E   1C                   ENDIF
615000000000     C*--------------------
615100951006     C* CATEGORIA SEAT
615200000000     C*--------------------
615300951006     C                   MOVEL     *BLANKS       DESITC           35            *SE NON TROVATO
615400020906     C                   Z-ADD     1             KTBKUT
615500951006     C                   MOVEL     '1L'          KTBCOD
615600951006     C                   MOVEL     *BLANKS       KTBKEY
615700951006     C                   MOVE      ACOITC        NUM5              5 0
615800951006     C                   MOVEL     NUM5          KTBKEY
615900951006     C     KEYTAB        CHAIN     TABEL00F                           99
616000951006IF  1C     *IN99         IFEQ      *OFF
616100951006     C                   MOVEL     TBLUNI        DS1L
616200951006     C                   MOVEL     §1LDES        DESITC                         *CATEGORIA SEAT
616300951006E   1C                   ENDIF
616400951006     C*--------------------
616500951006     C* QUALITA' CLIENTE
616600951006     C*--------------------
616700960710     C                   MOVEL     WFDCLV        DESCLV
616800000000     C                   SETOFF                                           28
616900010313     C     DESCLV        LOOKUP    ABC                                    28
617000960710IF  1C     *IN28         IFEQ      *OFF
617100000000     C                   MOVEL     'C'           DESCLV
617200960710E   1C                   ENDIF
617300960708     C*--------------------
617400120131     C* CAUSALE BLOCCO e data prima spedizione Fatturata
617500960708     C*--------------------
617600960710     C                   MOVEL     *BLANKS       DESCBL           30
617700980406     C                   CLEAR                   BS69DS
617800980406     C                   CLEAR                   CLPDS
617900980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
618000000825     C                   MOVEL     DEPCLI        I69KCP                         *CLIENTE X CNCLP
618100980406     C                   CALL      'TIBS69R'
618200980406     C                   PARM                    BS69DS
618300980406     C                   PARM                    ACODS
618400980406     C                   PARM                    INDDS
618500980406     C                   PARM                    CLPDS
618600980406     C                   PARM                    CLSDS
618700980406IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
618800120131     c                   eval           Data_Prima_Fat_6 = CLPDPS
618900980406IF  2C     DESABL        IFEQ      '8'                                          *CLIENTE BLOCCATO
619000120131      **
619100120131      **  Reperisce la Data di quando è stato fatto il Blocco Contabile sul cliente
619200120131     C                   clear                   TrulBLCds
619300120131     c                   eval      IblcTLA = 'L'
619400120131     c                   z-add     clpKcC        IblcKCC
619500120131     c                   z-add     clpKsC        IblcKSC
619600120131     c                   call      'TRULBLCR'
619700120131     C                   PARM                    KPJBA
619800120131     C                   PARM                    TrulBLCds
619900120131     c                   eval           Data_Blocco_Ultimo  = OblcDAV
620000120131     c                   eval           Cod_Blocco_Ultimo   = clpNAR
620100120131      **
620200980406     C                   Z-ADD     1             C
620300980406     C                   SETOFF                                           28
620400980406     C     CLPNAR        LOOKUP    CBL(C)                                 28
620500980406IF  3C     *IN28         IFEQ      *ON
620600980406     C                   MOVEL     DCBL(C)       DESCBL
620700980406E   3C                   ENDIF
620800980406E   2C                   ENDIF
620900980406E   1C                   ENDIF
621000000000     C*--------------------
621100000000     C* CLIENTE NUOVO O ACQUISITO
621200000000     C*--------------------
621300000000     C* PER IL PERIODO PRECEDENTE ALLA STATISTICA SE UN CLIENTE HA DEI SALDI E'
621400000000     C* ACQUISITO, SE NON NE HA E' NUOVO: NON POSSO PRENDERE IL CAMPO WFDCLN PER
621500000000     C* PER L'ANNO PRECEDENTE PERCHE' IL CLIENTE POTEVA ESSERE NUOVO
621600960710     C                   MOVEL     *BLANKS       DESCLN                         *SE NON TROVATO
621700000000IF  1C     TIPANN        IFEQ      'P'                                          *PERIODO PRECEDENTE
621800000000     C                   MOVEL     *BLANKS       DESCLN                         *NON NUOVO
621900000000X   1C                   ELSE                                                   *PERIODO CORRENTE
622000070822      * forza il flag precedentemente testato nel caso particolare sopraddescritto
622100070822      * qualora si tratti di stampa o Italia o Estero.
622200070822     c                   if        desCLN <>'Y' and sav_desCLN_Y ='Y' and
622300070822     c                             wfdCLI =  sav_depCLI
622400070822     c                   movel     'Y'           wfdCLN
622500070822     c                   end
622600000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O ACQUISITO
622700000000E   1C                   ENDIF
622800000000     C*
622900000000     C                   ENDSR
623000000000     C*------------------------------------------------------------------------*
623100000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
623200000000     C*------------------------------------------------------------------------*
623300000000     C     CARMES        BEGSR
623400000000     C*
623500000000     C                   CLEAR                   ANN                            *ANNI
623600000000     C                   CLEAR                   MES                            *MESI
623700000000     C                   Z-ADD     *ZEROS        J                              *N° MESI DA ELABORAR
623800000000     C*
623900990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
624000000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
624100990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
624200000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
624300020701     C*
624400020701     C* SICCOME LA STATISTICA NON PO' + ESSERE LANCIATA A CAVALLO DI ANNI DIVERSI L'ANNO INIZIALE -1
624500020701     C* L'ANNO FINALE -1
624600020701     C     WAA           SUB       1             WAAP              4 0          *ANNO INIZIALE PRECD
624700020701     C
624800000000     C*
624900000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
625000000000IF  1C     WAA2          IFEQ      WAA
625100000000     C*
625200000000     C* CALCOLO I MESI DI DIFFERENZA
625300000000DO  2C     WMM           DOWLE     WMM2
625400000000     C                   ADD       1             J
625500000000     C                   Z-ADD     WAA           ANN(J)
625600000000     C                   Z-ADD     WMM           MES(J)
625700000000     C                   ADD       1             WMM
625800000000E   2C                   ENDDO
625900000000     C***
626000000000     C* SPARM = N° MESI SELEZIONATI
626100000000     C***
626200000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
626300000000     C*
626400000000     C* IL RESTO DELLA SCHIERA A ZERO
626500000000DO  2C     J             DOWLT     12
626600000000     C                   ADD       1             J
626700000000     C                   Z-ADD     *ZEROS        ANN(J)
626800000000     C                   Z-ADD     *ZEROS        MES(J)
626900000000E   2C                   ENDDO
627000000000     C*
627100000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
627200000000X   1C                   ELSE
627300000000     C*
627400000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
627500000000DO  2C     WMM           DOWLE     12
627600000000     C                   ADD       1             J
627700000000     C                   Z-ADD     WAA           ANN(J)
627800000000     C                   Z-ADD     WMM           MES(J)
627900000000     C                   ADD       1             WMM
628000000000E   2C                   ENDDO
628100000000     C*
628200000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
628300000000     C                   Z-ADD     01            WMM
628400000000DO  2C     WMM           DOWLE     WMM2
628500000000     C                   ADD       1             J
628600000000     C                   Z-ADD     WAA2          ANN(J)
628700000000     C                   Z-ADD     WMM           MES(J)
628800000000     C                   ADD       1             WMM
628900000000E   2C                   ENDDO
629000000000     C***
629100000000     C* SPARM = N° MESI SELEZIONATI
629200000000     C***
629300000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
629400000000     C*
629500000000     C* IL RESTO DELLA SCHIERA A ZERO
629600000000DO  2C     J             DOWLT     12
629700000000     C                   ADD       1             J
629800000000     C                   Z-ADD     *ZEROS        ANN(J)
629900000000     C                   Z-ADD     *ZEROS        MES(J)
630000000000E   2C                   ENDDO
630100000000E   1C                   ENDIF
630200000000     C*
630300000000     C                   ENDSR
630400000000     C*------------------------------------------------------------------------*
630500000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
630600000000     C*------------------------------------------------------------------------*
630700000000     C     DECTES        BEGSR
630800000000     C*--------------------
630900000000     C* DATA E ORA
631000000000     C*--------------------
631100000000     C                   Z-ADD     UDATE         DATA              6 0
631200000000     C                   TIME                    ORA               6 0
631300950602     C*--------------------
631400950602     C* DATI IN STATISTICA
631500950602     C*--------------------
631600950602IF  1C     PARIOE        IFEQ      'I'
631700950602     C                   MOVEL     DRI(5)        PTCIOE
631800950602E   1C                   ENDIF
631900950602IF  1C     PARIOE        IFEQ      'E'
632000950602     C                   MOVEL     DRI(6)        PTCIOE
632100950602E   1C                   ENDIF
632200950602IF  1C     PARIOE        IFEQ      *BLANKS
632300950602     C                   MOVEL     DRI(7)        PTCIOE
632400950602E   1C                   ENDIF
632500000829     C*--------------------
632600000829     C* DATI CLIENTE / UNIFICANTE
632700000829     C*--------------------
632800000829IF  1C     PARSCE        IFEQ      'C'
632900000829     C                   MOVEL     DRI(29)       PTDSCE
633000000829X   1C                   ELSE
633100000829     C                   MOVEL     DRI(30)       PTDSCE
633200000829E   1C                   ENDIF
633300000000     C*--------------------
633400000000     C* DECODIFICA MESI
633500000000     C*--------------------
633600000000IF  1C     MES(1)        IFGT      *ZEROS
633700000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ
633800000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
633900990609     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
634000000000E   1C                   ENDIF
634100000000     C*
634200000000IF  1C     MES(2)        IFGT      *ZEROS
634300000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ
634400000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
634500990609     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
634600000000E   1C                   ENDIF
634700000000     C*
634800000000IF  1C     MES(3)        IFGT      *ZEROS
634900000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ
635000000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
635100990609     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
635200000000E   1C                   ENDIF
635300000000     C*
635400000000IF  1C     MES(4)        IFGT      *ZEROS
635500000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ
635600000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
635700990609     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
635800000000E   1C                   ENDIF
635900000000     C*
636000000000IF  1C     MES(5)        IFGT      *ZEROS
636100000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ
636200000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
636300990609     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
636400000000E   1C                   ENDIF
636500000000     C*
636600000000IF  1C     MES(6)        IFGT      *ZEROS
636700000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ
636800000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
636900990609     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
637000000000E   1C                   ENDIF
637100000000     C*
637200000000IF  1C     MES(7)        IFGT      *ZEROS
637300000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ
637400000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
637500990609     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
637600000000E   1C                   ENDIF
637700000000     C*
637800000000IF  1C     MES(8)        IFGT      *ZEROS
637900000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ
638000000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
638100990609     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
638200000000E   1C                   ENDIF
638300000000     C*
638400000000IF  1C     MES(9)        IFGT      *ZEROS
638500000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ
638600000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
638700990609     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
638800000000E   1C                   ENDIF
638900000000     C*
639000000000IF  1C     MES(10)       IFGT      *ZEROS
639100000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ
639200000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
639300990609     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
639400000000E   1C                   ENDIF
639500000000     C*
639600000000IF  1C     MES(11)       IFGT      *ZEROS
639700000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ
639800000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
639900990609     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
640000000000E   1C                   ENDIF
640100000000     C*
640200000000IF  1C     MES(12)       IFGT      *ZEROS
640300000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ
640400000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
640500990609     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
640600000000E   1C                   ENDIF
640700000000     C*
640800000000     C                   ENDSR
640900000000     C*------------------------------------------------------------------------*
641000000000     C*?CARTAB - CARICA I DATI IN TABELLE
641100000000     C*------------------------------------------------------------------------*
641200000000     C     CARTAB        BEGSR
641300000000     C*--------------------
641400000000     C* FILIALI/AREE/DIVISIONI
641500000000     C*--------------------
641600000000     C*
641700000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
641800000000     C                   Z-ADD     *ZEROS        KORFIL
641900000000     C     KEYORG        SETLL     AZORG01L
642000000000     C                   READ      AZORG01L                               99
642100000000     C*
642200000000     C* CICLO FINO A FINE FILE
642300000000     C                   Z-ADD     *ZEROS        F
642400951219DO  1C     *IN99         DOWEQ     *OFF
642500000000     C*
642600000000     C* ELIMINA RECORD ANNULLATI
642700000000IF  2C     ORGFVA        IFEQ      *BLANKS
642800000000     C                   ADD       1             F
642900000000     C* F I L I A L I
643000000000     C                   Z-ADD     ORGFIL        FIL(F)
643100000000     C                   MOVEL     ORGDES        DFIL(F)
643200000707     C                   MOVEL     ORGDE3        OG143
643300020627     C                   MOVEL     §OGNTW        POS(F)
643400000000     C* A R E E
643500020906     C                   Z-ADD     1             KTBKUT
643600000000     C                   MOVEL     '05'          KTBCOD
643700000000     C                   MOVEL     *BLANKS       KTBKEY
643800000000     C                   MOVEL     ORGCAR        KTBKEY
643900000000     C     KEYTAB        CHAIN     TABEL00F                           99
644000000000IF  3C     *IN99         IFEQ      *OFF
644100000000     C                   Z-ADD     ORGCAR        ARE(F)
644200000000     C                   MOVEL     TBLUNI        DARE(F)
644300000000E   3C                   ENDIF
644400000000     C* D I V I S I O N I
644500020906     C                   Z-ADD     1             KTBKUT
644600000000     C                   MOVEL     '17'          KTBCOD
644700000000     C                   MOVEL     *BLANKS       KTBKEY
644800000000     C                   MOVEL     ORGFL3        KTBKEY
644900000000     C     KEYTAB        CHAIN     TABEL00F                           99
645000000000IF  3C     *IN99         IFEQ      *OFF
645100000000     C                   MOVEL     ORGFL3        DIV(F)
645200000000     C                   MOVEL     TBLUNI        DDIV(F)
645300000000E   3C                   ENDIF
645400000000E   2C                   ENDIF
645500000000     C*
645600000000     C* LEGGE IL RECORD SUCCESSIVO
645700000000     C                   READ      AZORG01L                               99
645800000000E   1C                   ENDDO
645900000000     C*--------------------
646000000000     C* COMMERCIALI
646100000000     C*--------------------
646200020906     C                   Z-ADD     1             KTBKUT
646300000000     C                   MOVEL     '01'          KTBCOD
646400981103     C     KEYTA2        SETLL     TABEL00F
646500981103     C     KEYTA2        READE     TABEL00F                               99
646600000000     C                   Z-ADD     *ZEROS        C
646700000000DO  1C     *IN99         DOWEQ     *OFF
646800960708IF  2C     TBLFLG        IFEQ      *BLANKS
646900000000     C                   ADD       1             C
647000000000     C                   MOVEL     TBLUNI        DS01
647100000000     C                   MOVEL     TBLKEY        AGE(C)
647200000000     C                   MOVEL     §01AGE        DAGE(C)
647300980527IF  3C     PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
647400000000     C                   Z-ADD     §01PCF        CAGE(C)
647500980527X   3C                   ELSE                                                   *% LAVORO ESTERO
647600980527     C                   Z-ADD     §01PCE        CAGE(C)
647700980527E   3C                   ENDIF
647800960708E   2C                   ENDIF
647900981103     C     KEYTA2        READE     TABEL00F                               99
648000000000E   1C                   ENDDO
648100960708     C*--------------------
648200960708     C* CAUSALI BLOCCO CLIENTI
648300960708     C*--------------------
648400020906     C                   Z-ADD     1             KTBKUT
648500960708     C                   MOVEL     'BI'          KTBCOD
648600960708     C     KEYTA2        SETLL     TABEL00F
648700960708     C     KEYTA2        READE     TABEL00F                               99
648800960708     C                   Z-ADD     *ZEROS        C
648900960708DO  1C     *IN99         DOWEQ     *OFF
649000960708IF  2C     TBLFLG        IFEQ      *BLANKS
649100960708     C                   ADD       1             C
649200960708     C                   MOVEL     TBLKEY        CBL(C)
649300960708     C                   MOVEL     TBLUNI        DCBL(C)
649400960708E   2C                   ENDIF
649500960708     C     KEYTA2        READE     TABEL00F                               99
649600960708E   1C                   ENDDO
649700980521     C*---
649800980521     C* QUALITA' CLIENTE
649900980521     C*---
650000020906     C                   Z-ADD     1             KTBKUT
650100980521     C                   MOVEL     'IC'          KTBCOD
650200980521     C     KEYTA2        SETLL     TABEL00F
650300980521     C     KEYTA2        READE     TABEL00F                               99
650400980521DO  1C     *IN99         DOWEQ     *OFF
650500980521IF  2C     TBLFLG        IFEQ      *BLANKS
650600020220     C                   MOVEL     TBLUNI        DSIC
650700020220     C                   MOVEL     TBLKEY        ABC(§SICOR)
650800980521E   2C                   ENDIF
650900980521     C     KEYTA2        READE     TABEL00F                               99
651000980521E   1C                   ENDDO
651100000000     C*
651200000000     C                   ENDSR
651300000000     C*------------------------------------------------------------------------*
651400000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
651500000000     C*------------------------------------------------------------------------*
651600000000     C     STAPAR        BEGSR
651700000000     C*--------------------
651800000000     C* NOME PROGRAMMA
651900000000     C*--------------------
652000020116     C                   MOVEL     'TISE93R'     PTCPGM
652100000000     C*--------------------
652200000000     C* DESCRIZIONE STATISTICA
652300000000     C*--------------------
652400000000     C                   MOVEL     DST(S)        PTCDST
652500000000     C*--------------------
652600000000     C* CRAZIONE FILE DI LAVORO
652700000000     C*--------------------
652800000000IF  1C     PARCRE        IFEQ      'S'
652900000000     C                   MOVEL     'SI'          PACCRE
653000000000X   1C                   ELSE
653100000000     C                   MOVEL     'NO'          PACCRE
653200000000E   1C                   ENDIF
653300000000     C*--------------------
653400000000     C* DATE LANCIO
653500000000     C*--------------------
653600000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
653700000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
653800000000     C*--------------------
653900000000     C* SELEZIONI PER TIPO SERVIZIO
654000000000     C*--------------------
654100000000IF  1C     PARTSP        IFEQ      *BLANKS
654200000000     C                   MOVEL     DRI(1)        PADTSP
654300100129      **
654400100129      **  si memorizza quanti tipi servizi sono attivi nella tabella
654500100129     c                   z-add     0             Quanti_Tserv
654600100129     c                   clear                   TServ
654700100129     c                   clear                   DTServ
654800100129      **  carica codice e descrizione su 2 schiere e Quanti_serv contiene il nr.dei tipi servizio abilitati
654900100129     C                   Z-ADD     1             KTBKUT
655000100129     C                   MOVEL     '5E'          KTBCOD
655100100129     C     KEYTA2        setll     TABEL00F
655200100129     C     KEYTA2        reade     TABEL00F
655300100129     c                   dow       not %EoF(TABEL00F)
655400100129     c                   add       1             Quanti_Tserv
655500100129     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
655600100129     c                   eval        tserv(Quanti_Tserv) = tblkey
655700100129     c                   eval       Dtserv(Quanti_Tserv) = §5EDES
655800100129     C     KEYTA2        reade     TABEL00F
655900100129     c                   endDO
656000100129      **
656100000000X   1C                   ELSE
656200100129      **
656300000000     C                   MOVEL     PARTSP        PACTSP
656400020906     C                   Z-ADD     1             KTBKUT
656500000000     C                   MOVEL     '5E'          KTBCOD
656600000000     C                   MOVEL     *BLANKS       KTBKEY
656700000000     C                   MOVEL     PARTSP        KTBKEY
656800980406     C     KEYTAB        CHAIN     TABEL00F                           99
656900000000IF  2C     *IN99         IFEQ      *OFF
657000090610     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
657100090610     C                   MOVEL     §5EDES        PADTSP                         *TIPO SERVIZIO
657200000000E   2C                   ENDIF
657300000000E   1C                   ENDIF
657400950208     C*--------------------
657500950208     C* SELEZIONI PER DELTA ITALIA/ESTERO
657600950208     C*--------------------
657700950208     C                   MOVEL     PARIOE        PACIOE
657800950208IF  1C     PARIOE        IFEQ      'I'
657900950208     C                   MOVEL     DRI(5)        PADIOE
658000950208E   1C                   ENDIF
658100950208IF  1C     PARIOE        IFEQ      'E'
658200950208     C                   MOVEL     DRI(6)        PADIOE
658300950208E   1C                   ENDIF
658400950208IF  1C     PARIOE        IFEQ      *BLANKS
658500950208     C                   MOVEL     DRI(7)        PADIOE
658600950208E   1C                   ENDIF
658700951018     C*--------------------
658800970822     C* SELEZIONI PER QUALITA' CLIENTE
658900951018     C*--------------------
659000951018     C                   MOVEL     PARCLV        PACCLV
659100010618     C                   MOVEL     PARCL1        PACCL1
659200010618     C                   MOVEL     PARCL2        PACCL2
659300951018     C                   MOVEL     *BLANKS       PADCLV
659400951018IF  1C     PARCLV        IFEQ      *BLANKS
659500010618     C     PARCL1        ANDEQ     *BLANKS
659600010618     C     PARCL2        ANDEQ     *BLANKS
659700951018     C                   MOVEL     DRI(1)        PADCLV
659800980528E   1C                   ENDIF
659900010618     C*--------------------
660000010618     C* TIPO STAMPA SINTETICA S/N
660100010618     C*--------------------
660200010618     C                   MOVEL     PARSIN        PACSIN
660300980528     C*--------------------
660400980528     C* % SCOSTAMENTO DELTA TOTALE
660500980528     C*--------------------
660600980528     C                   Z-ADD     PARPDD        PACPDD
660700980528     C                   Z-ADD     PARPDA        PACPDA
660800980528     C*--------------------
660900980528     C* % SCOSTAMENTO DELTA PER FASCE
661000980528     C*--------------------
661100970822     C                   Z-ADD     PARSPD        PACSPD
661200020702     C*--------------------
661300020702     C* % SCOSTAMENTO FATTURATO ANNO PRECEDENTE
661400020702     C*--------------------
661500020702     C                   Z-ADD     PARDFD        PACDFD
661600020702     C                   Z-ADD     PARDFA        PACDFA
661700020702     C                   Z-ADD     PARDFDP       PACDFDP
661800020702     C                   Z-ADD     PARDFAP       PACDFAP
661900000000     C*--------------------
662000000000     C* SELEZIONI PER DELTA FILIALE
662100000000     C*--------------------
662200000000IF  1C     PARFIC        IFGT      *ZEROS                                       *FILIALE
662300000000     C                   Z-ADD     PARFIC        DEPFIL
662400000000     C                   EXSR      DECFIL
662500000000     C                   Z-ADD     *ZEROS        DEPFIL
662600000000     C                   MOVEL     PARFIC        PACFIC
662700000000     C                   MOVEL     DESFIL        PADFIC
662800000000X   1C                   ELSE
662900000000     C                   MOVEL     *BLANKS       PACFIC
663000000000E   1C                   ENDIF
663100000000     C*
663200000000IF  1C     PARARC        IFGT      *ZEROS                                       *AREA
663300000000     C                   Z-ADD     PARARC        DEPARE
663400000000     C                   EXSR      DECARE
663500000000     C                   Z-ADD     *ZEROS        DEPARE
663600000000     C                   MOVEL     PARARC        PACARC
663700000000     C                   MOVEL     DESARE        PADARC
663800000000X   1C                   ELSE
663900000000     C                   MOVEL     *BLANKS       PACARC
664000000000E   1C                   ENDIF
664100000000     C*
664200000000IF  1C     PARDIC        IFNE      *BLANKS                                      *DIVISIONE
664300000000     C                   MOVEL     PARDIC        DEPDIV
664400000000     C                   EXSR      DECDIV
664500000000     C                   MOVEL     *BLANKS       DEPDIV
664600000000     C                   MOVEL     PARDIC        PACDIC
664700000000     C                   MOVEL     DESDIV        PADDIC
664800000000E   1C                   ENDIF
664900000000     C*--------------------
665000000000     C* SELEZIONI PER DELTA COMMERCIALE
665100000000     C*--------------------
665200000000IF  1C     PARCOF        IFGT      *ZEROS                                       *COMMERCIALE
665300000000     C                   Z-ADD     PARCOF        DEPAGE
665400000000     C                   EXSR      DECAGE
665500000000     C                   Z-ADD     *ZEROS        DEPAGE
665600000000     C                   MOVEL     PARCOF        PACCOF
665700000000     C                   MOVEL     DESAGE        PADCOF
665800000000X   1C                   ELSE
665900000000     C                   MOVEL     *BLANKS       PACCOF
666000000000E   1C                   ENDIF
666100000000     C*
666200000000IF  1C     PARDIF        IFNE      *BLANKS                                      *DIVISIONE
666300000000     C                   MOVEL     PARDIF        DEPDIV
666400000000     C                   EXSR      DECDIV
666500000000     C                   MOVEL     *BLANKS       DEPDIV
666600000000     C                   MOVEL     PARDIF        PACDIF
666700000000     C                   MOVEL     DESDIV        PADDIF
666800000000E   1C                   ENDIF
666900000707     C*--------------------
667000000707     C* DATI POSTE
667100000707     C*--------------------
667200000707IF  1C     PARPOS        IFEQ      'S'
667300000707     C                   MOVEL     'SI'          PADPOS
667400000707X   1C                   ELSE
667500000707     C                   MOVEL     'NO'          PADPOS
667600000707E   1C                   ENDIF
667700000825     C*--------------------
667800000825     C* DATI CLIENTE / UNIFICANTE
667900000825     C*--------------------
668000000825     C                   MOVEL     PARSCE        PACSCE
668100000825IF  1C     PARSCE        IFEQ      'C'
668200000825     C                   MOVEL     DRI(29)       PADSCE
668300000825X   1C                   ELSE
668400000825     C                   MOVEL     DRI(30)       PADSCE
668500000825E   1C                   ENDIF
668600000000     C*
668700000000     C* S T A M P A
668800000000     C*
668900100129     c                   if        parSTAMPA <> 'N'
669000020222     C                   WRITE     SE94PT                                       *INTESTAZIONE
669100020116     C                   WRITE     SE94PA                                       *PARAMETRI
669200100129     c                   end
669300000000     C*
669400000000     C* ACCENDO INDICATORE DI OF PER STAMPARE LA TESTATA ALLA 1^ ROTTURA CLIENTE
669500000000     C                   SETON                                        01
669600000000     C                   MOVEL     '0'           PRIGIR
669700000000     C*
669800000000     C                   ENDSR
669900980406     C*------------------------------------------------------------------------*
670000980406     C*?FINSR - OPERAZIONI FINALI
670100980406     C*------------------------------------------------------------------------*
670200980406     C     FINSR         BEGSR
670300980406     C*
670400980406     C* CHIUDE I FILE DEI PGM CHIAMATI
670500980406     C                   CLEAR                   BS69DS
670600980406     C                   CLEAR                   ACODS
670700980406     C                   CLEAR                   INDDS
670800980406     C                   CLEAR                   CLPDS
670900980406     C                   CLEAR                   CLSDS
671000980406     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
671100980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
671200980406     C                   CALL      'TIBS69R'
671300980406     C                   PARM                    BS69DS
671400980406     C                   PARM                    ACODS
671500980406     C                   PARM                    INDDS
671600980406     C                   PARM                    CLPDS
671700980406     C                   PARM                    CLSDS
671800980406     C*
671900980406     C                   ENDSR
672000030924     C*------------------------------------------------------------------------*
672100030924     C*?CANWF - CANCELLA DATI WFILE, SE RICHIESTO
672200030924     C*------------------------------------------------------------------------*
672300030924     C     CANWF         BEGSR
672400030924     C*
672500030925     C     PARSTA        IFEQ      'F'
672600030924     C     PARWCP        IFEQ      'S'                                          *SE RICHIESTO
672700030924     c                   MOVEL     PARIOE        KE93IOE                        *ITALIA/ESTERO
672800030924     C     KEYSE93       SETLL     WFTISE93L
672900030924     C     KEYSE93       READE     WFTISE93L                              99
673000030924     C                   DOW       NOT *IN99
673100030924     C                   DELETE    WFTISE930
673200030924     C     KEYSE93       READE     WFTISE93L                              99
673300030924     C                   ENDDO
673400030925     C                   ENDIF
673500030925     C                   ENDIF
673600030924     C*
673700030924     C                   ENDSR
673800000000     C*------------------------------------------------------------------------*
673900000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
674000000000     C*------------------------------------------------------------------------*
674100000000     C     *INZSR        BEGSR
674200960708     C*--------------------
674300960708     C* RICEVIMENTO PARAMETRI
674400960708     C*--------------------
674500000000     C     *ENTRY        PLIST
674600000000     C                   PARM                    KPJBA
674700000000     C                   MOVEL     KPJBU         PARAM
674800100129      *
674900100129      * se non si deve più eseguire la stampa viene passato il flag impostato a "N"
675000100129     c                   if        parSTAMPA <> 'N'
675100100129     c                   open      TISE94P
675200100129     c                   end
675300960708     C*--------------------
675400960708     C* VARIABILI RIFERITE AL DATABASE
675500960708     C*--------------------
675600000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
675700100201     C     *LIKE         DEFINE    TBLCOD        KTBCOD
675800000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
675900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
676000950213     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEL01L
676100000000     C     *LIKE         DEFINE    WFDARE        KWDARE
676200000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
676300000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
676400960710     C     *LIKE         DEFINE    WFDCLV        KWDCLV
676500000825     C     *LIKE         DEFINE    WFDCLI        KWDCLI
676600000000     C     *LIKE         DEFINE    WFDANN        KWDANN
676700000000     C     *LIKE         DEFINE    WFDMES        KWDMES
676800960708     C     *LIKE         DEFINE    WFDDIV        DEPDIV                         *DI DEPOSITO
676900000000     C     *LIKE         DEFINE    WFDARE        DEPARE
677000000000     C     *LIKE         DEFINE    WFDFIL        DEPFIL
677100000000     C     *LIKE         DEFINE    WFDAGE        DEPAGE
677200081013     C     *LIKE         DEFINE    WFDRGD        DEPRGD
677300081013     C     *LIKE         DEFINE    WFDRGF        DEPRGF
677400070822     C     *LIKE         DEFINE    WFDCLI        DEPCLI
677500070822     C     *LIKE         DEFINE    WFDCLI        sav_depCLI
677600020319     C     *LIKE         DEFINE    WFDCLI        DEPCL2
677700000000     C     *LIKE         DEFINE    WFDANN        DEPANN
677800000000     C     *LIKE         DEFINE    WFDMES        DEPMES
677900020319     C     *LIKE         DEFINE    WFDNTW        DEPNTW
678000000000     C     *LIKE         DEFINE    §01PCF        DESPCF
678100000000     C     *LIKE         DEFINE    ACORAG        DESKSC
678200070419     C     *LIKE         DEFINE    INDvia        DESvia
678300070419     C     *LIKE         DEFINE    INDcit        DEScit
678400070419     C     *LIKE         DEFINE    INDcap        DEScap
678500070419     C     *LIKE         DEFINE    INDcae        DEScae
678600070419     C     *LIKE         DEFINE    INDprv        DESprv
678700070419     C     *LIKE         DEFINE    INDsta        DESsta
678800070615     C     *LIKE         DEFINE    INDcdf        DEScdf                         *Cod.Fiscale
678900070615     C     *LIKE         DEFINE    INDiva        DESiva                         *Part.IVA
679000070618     C     *LIKE         DEFINE    INDtel        DEStel                         *Telefono
679100000000     C     *LIKE         DEFINE    CLPCLV        DESCLV
679200000000     C     *LIKE         DEFINE    WFDCLN        DESCLN
679300070822     C     *LIKE         DEFINE    WFDCLN        sav_desCLN_Y
679400960708     C     *LIKE         DEFINE    ACOABL        DESABL
679500030925     C     *LIKE         DEFINE    SE93IOE       KE93IOE
679600100201     c     *LIKE         DEFINE    k             NumElem
679700100201      **
679800960708     C*--------------------
679900960708     C* VARIABILI NON RIFERITE AL DATABASE
680000960708     C*--------------------
680100000000     C                   Z-ADD     *ZEROS        C                 4 0          *COMMERCIALI
680200000000     C                   Z-ADD     *ZEROS        D                 4 0          *PER DECODIFICHE
680300000000     C                   Z-ADD     *ZEROS        F                 4 0          *VOCI ORGANIGRAMMA
680400000000     C                   Z-ADD     *ZEROS        J                 4 0          *GENERICO
680500000000     C                   Z-ADD     *ZEROS        P                 4 0          *ELEMENTI PERCENTUAL
680600000000     C                   Z-ADD     *ZEROS        Q                 4 0          *QUALITA' CLIENTE
680700000000     C                   Z-ADD     *ZEROS        S                 4 0          *STATISTICA IN ELAB
680800000000     C                   Z-ADD     *ZEROS        X                 2 0          *PER I MESI
680900000000     C                   Z-ADD     *ZEROS        MM                2 0          *PER I MESI
681000000000     C                   Z-ADD     *ZEROS        IN                2 0          *PER I MESI
681100000000     C                   Z-ADD     *ZEROS        SPARM             4 0          *N° MESI SELEZIONATI
681200000828     C                   Z-ADD     *ZEROS        I                 5 0          *INDICE
681300000828     C                   Z-ADD     *ZEROS        IS                5 0          *INDICE
681400020318     C                   Z-ADD     *ZEROS        K                 4 0          *NETWORK
681500020701     C                   Z-ADD     *ZEROS        II                5 0          *INDICE
681600020701     C*--------------------
681700020701     C* INDICATORI DI PROCEDURA
681800020701     C*--------------------
681900020701     C                   SETOFF                                       54
682000960708     C*--------------------
682100960708     C* VARIABILI DI CONTROLLO
682200960708     C*--------------------
682300000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
682400000000     C                   MOVEL     '0'           PASGEN            1            MEMOR SALDI GENERALI
682500000000     C                   MOVEL     '0'           PASDIV            1            MEMOR SALDI DIVISION
682600000000     C                   MOVEL     '0'           PASARE            1            MEMOR SALDI AREA
682700000000     C                   MOVEL     '0'           PASFIL            1            MEMOR SALDI FILIALE
682800000000     C                   MOVEL     '0'           PASAGE            1            MEMOR SALDI COMM.LE
682900000000     C                   MOVEL     '0'           PASKSC            1            MEMOR SALDI CLIENTE
683000951227     C                   MOVEL     '0'           FLAKSC            1            FLAG CONT CLI CORR
683100951227     C                   MOVEL     '0'           FL2KSC            1            FLAG CONT CLI PREC
683200000000     C                   MOVEL     '0'           FLAPAS            1            FLAG PASSAGGIO LETTU
683300000000     C                   MOVEL     '0'           $CATCL            1            CATEGORIA CLIENTE
683400000000     C                   Z-ADD     *ZEROS        $Q                1 0          QUALITA' CLIENTE
683500000000     C                   MOVEL     ' '           TIPANN            1            *TIPO ANNO (P/C)
683600000000     C                   MOVEL     '0'           PRIGIR            1            *PRIMO GIRO
683700000000     C                   MOVEL     '0'           ORAKSC            1            *ORA STAMPA CLIENTE
683800000828     C                   MOVEL     'N'           $STRIG            1            *STAMPA RIGA
683900020221     C                   MOVEL     '0'           DEPSTPD           1            *FLAG DI STAMPA SI/N
684000020222     C                   MOVEL     '0'           FLGSTPLE          1            *FLAG DI STAMPA SI/N
684100020318     C                   MOVEL     '0'           FLGSTPNTW         1            *FLAG DI STAMPA SI/N
684200020319     C                   MOVEL     '0'           FLGSTPRI          1            *FLAG DI STAMPA SI/N
684300960708     C*--------------------
684400960708     C* CHAIVI DI LETTURA
684500960708     C*--------------------
684600960708     C* LETTURA TABEL00F - COMPLETA
684700000000     C     KEYTAB        KLIST
684800000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
684900000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
685000000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
685100960708     C* LETTURA TABEL00F - PARZIALE
685200000000     C     KEYTA2        KLIST
685300000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
685400000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
685500960708     C* LETTURA AZORG01L - COMPLETA
685600000000     C     KEYORG        KLIST
685700000000     C                   KFLD                    KORFIL                         *FILIALE
685800960708     C* LETTURA WFDEL01L - PARZIALE
685900000000     C     KEYDEL        KLIST
686000950208     C                   KFLD                    KWDDIV                         *DIVISIONE
686100000000     C                   KFLD                    KWDARE                         *AREA
686200000000     C                   KFLD                    KWDFIL                         *FILIALE
686300000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
686400960710     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
686500000825     C                   KFLD                    KWDCLI                         *CLIENTE
686600000000     C                   KFLD                    KWDANN                         *ANNO
686700000000     C                   KFLD                    KWDMES                         *MESE
686800030924     C* LETTURA WFTISE93L - PARZIALE
686900030924     C     KEYSE93       KLIST
687000030924     C                   KFLD                    KE93IOE                        *ITALIA/ESTERO
687100070420     C* LETTURA TFNTC00F - COMPLETA
687200070420     C     KeyNTC        KLIST
687300070420     C                   KFLD                    NTCapl                         *CODICE APPLICAZIONE
687400070420     C                   KFLD                    NTCnk1                         *CHIAVE 1 NOTE
687500070420     C                   KFLD                    NTCnk2                         *CHIAVE 2 NOTE
687600070420     C                   KFLD                    NTCtnt                         *TIPO NOTA
687700000828     C*
687800130131      ***
687900130131     c     k_deaV1       KLIST
688000130131     C                   KFLD                    VARIA             1
688100130131     C                   KFLD                    DEPCLI
688200130131     C*  serve x il FUEL
688300130131     C                   eval       VARIA ='f'
688400130131     C*
688500000828     C* IMNPOSTA DATA / ORA CORRENTE
688600000828     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
688700000828     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
688800000828     C                   Z-ADD     *ZEROS        G08INV
688900000828     C                   MOVEL     '0'           G08ERR
689000000828     C                   CALL      'XSRDA8'
689100000828     C                   PARM                    WLBDA8
689200000828     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
689300010725     C*
689400010725     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
689500010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
689600010725     C                   MOVE      28            WDFIN
689700081114     C*
689800081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
689900081114     C*
690000081114     C*  Ultimo giorno del mese
690100081114     C                   MOVE      WDFIN         MMGG_4            4 0
690200081114     C                   MOVEL     MMGG_4        Mm_2              2 0
690300081114     C                   MOVEl     WDFIN         Anno_4            4 0
690400081114      * se è febbraio
690500081114     c                   If        Mm_2 = 2
690600081114      *
690700081114      *  potrebbe essere bisestile 29 gg.
690800081114     c     Anno_4        div       4             risulta           3 0
690900081114     c                   mvr                     resto             3 0
691000081114      *  Se bisestile
691100081114     c                   If        resto = 0
691200081114     C                   MOVE      '29'          WDFIN
691300081114     c                   end
691400081114      *
691500081114     c                   else
691600081114      *
691700081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
691800081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
691900081114     c                                Mm_2 = 9 or Mm_2 = 11
692000081114     C                   MOVE      '30'          WDFIN
692100081114     c                   else
692200081114     C                   MOVE      '31'          WDFIN
692300081114     c                   end
692400081114      *
692500081114     c                   endIf
692600081114     C*
692700081114     C*----------- F I N E    M E S E ---------------------------*
692800081114     C*
692900000000     C*
693000000000     C* MEMORIZZA I DATI IN TABELLE
693100000000     C                   EXSR      CARTAB
693200000000     C*
693300000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
693400000000     C                   EXSR      CARMES
693500000000     C*
693600000000     C* DECODIFICA INTESTAZIONE
693700000000     C                   EXSR      DECTES
693800000000     C*
693900000000     C* VEDO QUALE STATISTICA DEVO ELABORARE
694000000000IF  1C     PARSTA        IFEQ      'K'
694100000000     C                   Z-ADD     1             S                              *DETTAGLIO CLIENTE
694200000000E   1C                   ENDIF
694300000000IF  1C     PARSTA        IFEQ      'C'
694400000000     C                   Z-ADD     2             S                              *DETTAGLIO COMM.LE
694500000000E   1C                   ENDIF
694600000000IF  1C     PARSTA        IFEQ      'F'
694700000000     C                   Z-ADD     3             S                              *DETTAGLIO FILIALE
694800000000E   1C                   ENDIF
694900051102     C*
695000051102IF  1C     PARSCE        IFEQ      'U'                                          *STAT. UNIFICANTE
695100051102IF  1C     PARSTA        andeq     'K'                                           dett.x cliente
695200051102     c                   eval      LEG040 = '(+) --> Cliente BLOCCATO'
695300051102     c                   end
695400030924     C*
695500030924     C* CANCELLA DATI WFILE, SE RICHIESTO
695600030924     C                   EXSR      CANWF
695700000000     C*
695800091124     C* Deve impostare i totali per Netowrk
695900091124IF  1C     PARIOE        IFEQ      'E'
696000091124     C                   open      waDELpcN
696100091124E   1C                   ENDIF
696200130131     C*
696300130131     C                   ENDSR
696400130131     C*------------------------------------------------------------------------*
696500130131     C*?ACCUMULA le VARIE - al momento solo x il FUEL (che si vuole scorporare)
696600130131     C*------------------------------------------------------------------------*
696700130131     C     VARIE_x_ClientBEGSR
696800130131      *
696900130131      *  Lo dovrà scrivere una sola volta anche x i giri fra Italia ed Estero
697000130131      *   è per questo che cancellando i records una volta elaborati verrà
697100130131      *   scritto correttamente la prima volta che incontra il codice CLIENTE.
697200130131     c                   move      'N'           deve_scriverlo    1
697300130131      *
697400130131      * ?Cicla sul file delle varie x il tipo servizio (f)->FUEL e x CLIENTE
697500130131      *
697600130131      * ? Man mano che legge e totalizza i records li CANCELLA dal WORKFile
697700130131      * ? per alleggerire le successive operazioni.
697800130131      *  La lettura è x VARIA/CLIENTE
697900130131     c     k_deaV1       setll     wfDEAv1l
698000130131     c     k_deaV1       reade     wfDEAv1l
698100130131     C*
698200130131     c                   DOW       not %EoF(wfDEAv1l)
698300130131     C*
698400130131     C* lo fa solo la prima, primissima volta e basta
698500130131     c                   if        deve_scriverlo = 'N'
698600130131     c                   eval         deve_scriverlo = 'S'
698700130131     c                   clear                   WADELVT0
698800130131     C                   eval      watAAD =  WAdAAD
698900130131     C                   eval      watMMD =  WAdMMD
699000130131     C                   eval      watAAA =  WAdAAA
699100130131     C                   eval      watMMA =  WAdMMA
699200130131     C                   eval      watCLV =  desCLV
699300130131     C                   eval      watCLI =  depCLI
699400130131     C                   eval      watCLD =  desKSC
699500130131     C                   eval      watSVR =  WFVSVR
699600130131     c                   end
699700130131     C*
699800130131     C*  Anno in corso
699900130131     C                   IF        WFVANN =  WAdAAD
700000130131     C*
700100130131     c                   select
700200130131     C*
700300130131     C                   when      WFVmes =  1
700400130131     c                   eval      WAtVA_01SC = wfvNSP + WAtVA_01SC
700500130131     c                   eval      WAtVA_01VC = wfvRCP + WAtVA_01VC
700600130131     C                   when      WFVmes =  2
700700130131     c                   eval      WAtVA_02SC = wfvNSP + WAtVA_02SC
700800130131     c                   eval      WAtVA_02VC = wfvRCP + WAtVA_02VC
700900130131     C                   when      WFVmes =  3
701000130131     c                   eval      WAtVA_03SC = wfvNSP + WAtVA_03SC
701100130131     c                   eval      WAtVA_03VC = wfvRCP + WAtVA_03VC
701200130131     C                   when      WFVmes =  4
701300130131     c                   eval      WAtVA_04SC = wfvNSP + WAtVA_04SC
701400130131     c                   eval      WAtVA_04VC = wfvRCP + WAtVA_04VC
701500130131     C                   when      WFVmes =  5
701600130131     c                   eval      WAtVA_05SC = wfvNSP + WAtVA_05SC
701700130131     c                   eval      WAtVA_05VC = wfvRCP + WAtVA_05VC
701800130131     C                   when      WFVmes =  6
701900130131     c                   eval      WAtVA_06SC = wfvNSP + WAtVA_06SC
702000130131     c                   eval      WAtVA_06VC = wfvRCP + WAtVA_06VC
702100130131     C                   when      WFVmes =  7
702200130131     c                   eval      WAtVA_07SC = wfvNSP + WAtVA_07SC
702300130131     c                   eval      WAtVA_07VC = wfvRCP + WAtVA_07VC
702400130131     C                   when      WFVmes =  8
702500130131     c                   eval      WAtVA_08SC = wfvNSP + WAtVA_08SC
702600130131     c                   eval      WAtVA_08VC = wfvRCP + WAtVA_08VC
702700130131     C                   when      WFVmes =  9
702800130131     c                   eval      WAtVA_09SC = wfvNSP + WAtVA_09SC
702900130131     c                   eval      WAtVA_09VC = wfvRCP + WAtVA_09VC
703000130131     C                   when      WFVmes = 10
703100130131     c                   eval      WAtVA_10SC = wfvNSP + WAtVA_10SC
703200130131     c                   eval      WAtVA_10VC = wfvRCP + WAtVA_10VC
703300130131     C                   when      WFVmes = 11
703400130131     c                   eval      WAtVA_11SC = wfvNSP + WAtVA_11SC
703500130131     c                   eval      WAtVA_11VC = wfvRCP + WAtVA_11VC
703600130131     C                   when      WFVmes = 12
703700130131     c                   eval      WAtVA_12SC = wfvNSP + WAtVA_12SC
703800130131     c                   eval      WAtVA_12VC = wfvRCP + WAtVA_12VC
703900130131     c                   endSL
704000130131     C*
704100130131     C                   eval      WAtVAT_SC  = wfvNSP  + WAtVAT_SC
704200130131     C                   eval      WAtVAT_VC  = wfvRCP  + WAtVAT_VC
704300130131     C*  Anno precedente
704400130131     C                   ELSE
704500130131     C*
704600130131     c                   select
704700130131     C*
704800130131     C                   when      WFVmes =  1
704900130131     c                   eval      WAtVA_01SP = wfvNSP + WAtVA_01SP
705000130131     c                   eval      WAtVA_01VP = wfvRCP + WAtVA_01VP
705100130131     C                   when      WFVmes =  2
705200130131     c                   eval      WAtVA_02SP = wfvNSP + WAtVA_02SP
705300130131     c                   eval      WAtVA_02VP = wfvRCP + WAtVA_02VP
705400130131     C                   when      WFVmes =  3
705500130131     c                   eval      WAtVA_03SP = wfvNSP + WAtVA_03SP
705600130131     c                   eval      WAtVA_03VP = wfvRCP + WAtVA_03VP
705700130131     C                   when      WFVmes =  4
705800130131     c                   eval      WAtVA_04SP = wfvNSP + WAtVA_04SP
705900130131     c                   eval      WAtVA_04VP = wfvRCP + WAtVA_04VP
706000130131     C                   when      WFVmes =  5
706100130131     c                   eval      WAtVA_05SP = wfvNSP + WAtVA_05SP
706200130131     c                   eval      WAtVA_05VP = wfvRCP + WAtVA_05VP
706300130131     C                   when      WFVmes =  6
706400130131     c                   eval      WAtVA_06SP = wfvNSP + WAtVA_06SP
706500130131     c                   eval      WAtVA_06VP = wfvRCP + WAtVA_06VP
706600130131     C                   when      WFVmes =  7
706700130131     c                   eval      WAtVA_07SP = wfvNSP + WAtVA_07SP
706800130131     c                   eval      WAtVA_07VP = wfvRCP + WAtVA_07VP
706900130131     C                   when      WFVmes =  8
707000130131     c                   eval      WAtVA_08SP = wfvNSP + WAtVA_08SP
707100130131     c                   eval      WAtVA_08VP = wfvRCP + WAtVA_08VP
707200130131     C                   when      WFVmes =  9
707300130131     c                   eval      WAtVA_09SP = wfvNSP + WAtVA_09SP
707400130131     c                   eval      WAtVA_09VP = wfvRCP + WAtVA_09VP
707500130131     C                   when      WFVmes = 10
707600130131     c                   eval      WAtVA_10SP = wfvNSP + WAtVA_10SP
707700130131     c                   eval      WAtVA_10VP = wfvRCP + WAtVA_10VP
707800130131     C                   when      WFVmes = 11
707900130131     c                   eval      WAtVA_11SP = wfvNSP + WAtVA_11SP
708000130131     c                   eval      WAtVA_11VP = wfvRCP + WAtVA_11VP
708100130131     C                   when      WFVmes = 12
708200130131     c                   eval      WAtVA_12SP = wfvNSP + WAtVA_12SP
708300130131     c                   eval      WAtVA_12VP = wfvRCP + WAtVA_12VP
708400130131     c                   endSL
708500130131     C*
708600130131     C                   eval      WAtVAT_SP  = wfvNSP + WAtVAT_SP
708700130131     C                   eval      WAtVAT_VP  = wfvRCP + WAtVAT_VP
708800130131     C*
708900130131     C                   END
709000130131     C*
709100130131     C*  cancella il record già incasellato in modo da non riutilizzarlo
709200130131     C*   più in seguito in sommatorie errate
709300130131     c                   delete    WFDEAV00
709400130131     C*
709500130131     c     k_deaV1       reade     wfDEAv1l
709600130131     C                   ENDDO
709700130131     C*
709800130131     C*
709900130131     C* lo fa solo la prima, primissima volta e basta
710000130131     c                   if        deve_scriverlo = 'S'
710100130131     c                   write     WADELVT0
710200130131     c                   end
710300130131     C*
710400130131     C*
710500000000     C                   ENDSR
710600130131     C*------------------------------------------------------------------------*
710700000000**   DMM - DECODIFICHE MESI DELL'ANNO
710800000000GEN  1
710900000000FEB  2
711000000000MAR  3
711100000000APR  4
711200000000MAG  5
711300000000GIU  6
711400000000LUG  7
711500000000AGO  8
711600000000SET  9
711700000000OTT  10
711800000000NOV  11
711900000000DIC  12
712000000000**   DST - DECODIFICHE STATISTICHE
712100000000PER FILIALE              1
712200000000PER AREA                 2
712300000000PER DIVISIONE            3
712400000000                         4
712500000000                         5
712600000000**   DRI - DECODIFICHE RIGA DI STAMPA
712700000000TUTTI                    1
712800000000TUTTE                    2
7129000000003
713000950208NUOVI                    4
713100950208ITALIA                   5
713200950208ESTERO                   6
713300950602ITALIA+ESTERO            7
713400020318ACQUIS.                  8
7135000000009
71360000000010
71370000000011
713800020220TOTALE                  12
713900000000CLIENTI                 13
714000000000SPEDIZ                  14
714100000000RICAVO                  15
714200000000DELTA                   16
714300000000NUOVO                   17
714400000000ACQUISITO               18
71450002022119
714600000000      C L I E N T I     20
714700000000 C O M M E R C I A L E  21
714800000000     F I L I A L E      22
714900000000       A R E A          23
715000000000   D I V I S I O N E    24
715100000000    G E N E R A L E     25
71520000082926
71530000082927
715400000829UNIFICATI:               28
715500000829PER CLIENTE              29
715600000829PER CLIENTE UNIFICANTE   30
715700020318**   DNTW - DECODIFICHE NETWORK
715800020318                          1
715900020318EEX                       2
716000020318DPD                       3
716100020318FED                       4
716200020318EUP                       5
