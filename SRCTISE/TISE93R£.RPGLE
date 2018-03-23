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
001900140612     FAZCMM01L  IF   E           K DISK    usropn
002000000000     FTABEL00F  IF   E           K DISK
002100100129     FTISE94P   O    E             PRINTER OFLIND(*IN01) usropn
002200030925     FWFTISE93L UF A E           K DISK
002300000000     D*------------------------------------------------------------------------*
002400000000     D* SCHIERE
002500000000     D*------------------------------------------------------------------------*
002600000000     D*------------------
002700000000     D* DECODIFICA MESI DELL'ANNO
002800000000     D*------------------
002900000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
003000000000     D*------------------
003100000000     D* DECODIFICA STATISTICHE
003200000000     D*------------------
003300000000     D DST             S             25    DIM(5) CTDATA PERRCD(1)
003400000000     D*------------------
003500000000     D* DECODIFICA RIGHE DI STAMPA
003600000000     D*------------------
003700000825     D DRI             S             24    DIM(30) CTDATA PERRCD(1)
003800020318     D*------------------
003900020318     D* DECODIFICA NETWORK
004000020318     D*------------------
004100020318     D DNTW            S              3    DIM(5) CTDATA PERRCD(1)
004200000000     D*------------------
004300000000     D* ORGANIGRAMMA
004400000000     D*------------------
004500971124     D FIL             S              3  0 DIM(500)                             *FILIALI
004600971124     D DFIL            S             20    DIM(500)
004700971124     D ARE             S              3  0 DIM(500)                             *AREE
004800971124     D DARE            S             20    DIM(500)
004900971124     D DIV             S              1    DIM(500)                             *DIVISIONI
005000971124     D DDIV            S             20    DIM(500)
005100020627     D POS             S              3    DIM(500)                             *NETWORK POSTE
005200000000     D*------------------
005300000000     D* COMMERCIALI
005400000000     D*------------------
005500150102     D AGE             S              7  0 DIM(9000)                            *CODICI
005600150102     D DAGE            S             20    DIM(9000)                            *DECODIFICHE
005700150102     D CAGE            S              3  0 DIM(9000)                            *PERCENTUALI COMPETE
005800960708     D*------------------
005900960708     D* CAUSALI BLOCCO CLIENTI
006000960708     D*------------------
006100960708     D CBL             S              3    DIM(50)                              *CODICI
006200960708     D DCBL            S             30    DIM(50)                              *DECODIFICHE
006300000000     D*------------------
006400000000     D* MESI DA ELABORARE
006500000000     D*------------------
006600000000     D MES             S              2  0 DIM(12)
006700000000     D*------------------
006800000000     D* ANNI DEI MESI DA ELABORARE
006900000000     D*------------------
007000990609     D ANN             S              4  0 DIM(12)
007100000000     D*------------------
007200000000     D* CATEGORIE CLIENTI
007300000000     D*------------------
007400020220     D ABC             S              1    DIM(5)                               QUALITA' CLIENTE
007500000000     D*------------------
007600000000     D* PERCENTUALE COMPETENZA
007700000000     D*------------------
007800000000     D PCF             S              1    DIM(3)
007900000000    ?D*-------------------
008000000000    ?D* S C H I E R E   N O N   M U L T I P L E
008100000000    ?D*-------------------
008200030924     D*------------------
008300030924     D* FLAG CONTATORE CLIENTE PER MESE
008400030924     D*------------------
008500000830     D SFCCL           S              1    DIM(12)                              FLAG CONTEGGIO
008600000000     D*------------------
008700000000     D* SCHIERE DI MEMORIZZAZIONE VALORI DI UN CLIENTE
008800000000     D*------------------
008900000000     D S1K             S             13  0 DIM(13)                              SPEDIZIONI UN CLIENT
009000081013     D KG1K            S             15  1 DIM(13)                              Pesi x UN CLIENT
009100020116     D R1K             S             19  5 DIM(13)                              RICAVI REALI UN CLIE
009200020116     D P1K             S             19  5 DIM(13)                              RICAVI PRESUNTI UN C
009300970822     D D1K             S              1    DIM(13)                              FLAG %SCOSTAMENTO DE
009400020712     D R1KP            S             19  5 DIM(12)                              RICAVI REALI UN CLIE
009500121217     D CLN1K           S              1    DIM(13)                              FLAG NUOVO ACQUISITO
009600070419     D*------------------
009700070419     D ap_S1K          S             13  0 DIM(12)                              SPEDIZIONI UN CLIENT
009800081013     D ap_KG1K         S             15  1 DIM(13)                              Pesi x UN CLIENT ANN
009900070419     D ap_R1K          S             19  5 DIM(12)                              RICAVI REALI UN CLIE
010000070419     D ap_P1K          S             19  5 DIM(12)                              RICAVI PRESUNTI UN C
010100091123     D*------------------
010200091124     D* SCHIERE PER TOTALI x NETWORK
010300091123     D*------------------
010400091124     D W1ntw           DS                  OCCURS(5) INZ
010500091124     d  c1ntw                         3
010600091124     D  S1ntw                        13  0 DIM(12)                              SPEDIZIONI UN CLIENT
010700091124     D  KG1ntw                       15  1 DIM(12)                              Pesi x UN CLIENT
010800091124     D  R1ntw                        19  5 DIM(12)                              RICAVI REALI UN CLIE
010900091124     D  P1ntw                        19  5 DIM(12)                              RICAVI PRESUNTI UN C
011000091124     D  D1ntw                         1    DIM(12)                              FLAG %SCOSTAMENTO DE
011100091124     D  R1Pntw                       19  5 DIM(12)                              RICAVI REALI UN CLIE
011200091125     D  TR1ntw                       15  0
011300091125     D  TP1ntw                       15  0
011400091123     D*------------------
011500091124     D* PER TOTALI anno Preced x NETWORK
011600091123     D*------------------
011700091124     D WAP1ntw         DS                  OCCURS(5) INZ
011800091124     d  ap_c1ntw                      3
011900091124     D  ap_S1ntw                     13  0 DIM(12)                              SPEDIZIONI UN CLIENT
012000091124     D  ap_KG1ntw                    15  1 DIM(12)                              Pesi x UN CLIENT
012100091124     D  ap_R1ntw                     19  5 DIM(12)                              RICAVI REALI UN CLIE
012200091124     D  ap_P1ntw                     19  5 DIM(12)                              RICAVI PRESUNTI UN C
012300091124     D  ap_D1ntw                      1    DIM(12)                              FLAG %SCOSTAMENTO DE
012400091124     D  ap_R1Pntw                    19  5 DIM(12)                              RICAVI REALI UN CLIE
012500091125     D  ap_TR1ntw                    15  0
012600091125     D  ap_TP1ntw                    15  0
012700091123     D*------------------
012800100202     D* SCHIERE PER TOTALI x NETWORK e TIPI SERVIZI sia Anno in corso che anno precedente
012900100202      *   legate dalla stessa chiave ossi NTW + Tipo servizio. ....quindi......
013000100129      *   le occorrenze sono date dalla moltiplica dei network x i tipi servizi possibili
013100100202      *   ossia 5(ntw) x 10(tipi serv.) = 50
013200100204     D*------------------
013300100204      * x comodità di calcolo, il codice NTW per l' ITALIA è blank e verra impostato a (ITA).
013400100129     D*------------------
013500100129     D W1ntwTS         DS                  OCCURS(50) INZ
013600100129     d  c1ntwTS                       3
013700100201     d  c1tpsTS                       1
013800100202      *-- Anno in corso
013900100202     D     S1ntwTS                   13  0 DIM(12)                              SPEDIZIONI UN CLIENT
014000100202     D     KG1ntwTS                  15  1 DIM(12)                              Pesi x UN CLIENT
014100100202     D     R1ntwTS                   19  5 DIM(12)                              RICAVI REALI UN CLIE
014200100202     D     P1ntwTS                   19  5 DIM(12)                              RICAVI PRESUNTI UN C
014300100202     D     D1ntwTS                    1    DIM(12)                              FLAG %SCOSTAMENTO DE
014400100202     D     R1PntwTS                  19  5 DIM(12)                              RICAVI REALI UN CLIE
014500100202     D     TR1ntwTS                  15  0
014600100202     D     TP1ntwTS                  15  0
014700100202      *-- Anno precedente
014800100202     D  ap_S1ntwTS                   13  0 DIM(12)                              SPEDIZIONI UN CLIENT
014900100202     D  ap_KG1ntwTS                  15  1 DIM(12)                              Pesi x UN CLIENT
015000100202     D  ap_R1ntwTS                   19  5 DIM(12)                              RICAVI REALI UN CLIE
015100100202     D  ap_P1ntwTS                   19  5 DIM(12)                              RICAVI PRESUNTI UN C
015200100202     D  ap_D1ntwTS                    1    DIM(12)                              FLAG %SCOSTAMENTO DE
015300100202     D  ap_R1PntwTS                  19  5 DIM(12)                              RICAVI REALI UN CLIE
015400100202     D  ap_TR1ntwTS                  15  0
015500100202     D  ap_TP1ntwTS                  15  0
015600100129     D*------------------
015700030924     D CNW             S             11  0 DIM(13)                              NUMERO C.NUOVI
015800030924     D SNW             S             13  0 DIM(13)                              SPEDIZIONI C.NUOVI
015900030924     D RNW             S             19  5 DIM(13)                              RICAVI REALI C.NUOVI
016000030924     D PNW             S             19  5 DIM(13)                              RICAVI PRESUNTI C.NU
016100030924     D CAW             S             11  0 DIM(13)                              NUMERO C.ACQUISITI
016200030924     D SAW             S             13  0 DIM(13)                              SPEDIZIONI C.ACQUISI
016300020116     D RAW             S             19  5 DIM(13)                              RICAVI REALI C.ACQ
016400020116     D PAW             S             19  5 DIM(13)                              RICAVI PRESUNTI C.AC
016500020221     D*------------------
016600020318     D* SCHIERE DI LAVORO PER TOTALI "D"
016700020221     D*------------------
016800020221     D CNWD            S             11  0 DIM(13)                              NUMERO C.NUOVI
016900020221     D SNWD            S             13  0 DIM(13)                              SPEDIZIONI C.NUOVI
017000020221     D RNWD            S             19  5 DIM(13)                              RICAVI REALI C.NUOVI
017100020221     D PNWD            S             19  5 DIM(13)                              RICAVI PRESUNTI C.NU
017200020221     D CAWD            S             11  0 DIM(13)                              NUMERO C.ACQUISITI
017300020221     D SAWD            S             13  0 DIM(13)                              SPEDIZIONI C.ACQUISI
017400020221     D RAWD            S             19  5 DIM(13)                              RICAVI REALI C.ACQ
017500020221     D PAWD            S             19  5 DIM(13)                              RICAVI PRESUNTI C.AC
017600000000     D*---
017700020220     D* MULTIPLA PER 13° TOT CLIENTI (TOTALI "A" + "B" + "C" + "T")
017800000000     D*---
017900951227     D NTW             S             11  0 DIM(2)                               TOTALE CLIENTI NEW
018000951227     D ATW             S             11  0 DIM(2)                               TOTALE CLIENTI ACQ
018100000000     D*------------------
018200000000     D* SCHIERE DI LAVORO PER RIGHE DETTAGLIO (CLI/SPE/RIC/DEL)
018300000000     D*------------------
018400000000     D CLW             S             13  0 DIM(13)                              NUMERO CLIENTI
018500000000     D SPW             S             13  0 DIM(13)                              SPEDIZIONI
018600020116     D RRW             S             19  5 DIM(13)                              RICAVO REALE
018700020116     D RPW             S             19  5 DIM(13)                              RICAVO PRESUNTO
018800000000     D*---
018900951227     D* NON MULTIPLA PER 13° TOT CLIENTI DETTAGLIO (CLI/SPE/RIC/DEL)
019000000000     D*---
019100951227     D CTW             S             11  0 DIM(2)                               13° TOTALE CLIENTI
019200000000     D*------------------
019300000000     D* SCHIERE DI LAVORO
019400000000     D*------------------
019500000000     D $CL             S             13  0 DIM(13)                              NUMERO CLIENTI
019600000000     D $SP             S             13  0 DIM(13)                              SPEDIZIONI
019700020116     D $RR             S             19  5 DIM(13)                              RICAVO REALE
019800020116     D $RP             S             19  5 DIM(13)                              RICAVO PRESUNTO
019900000000     D $PD             S              4  1 DIM(13)                              DELTA
020000970822     D $SD             S              1    DIM(13)                              SCOSTAMENTO DELTA
020100020116     D $VA             S             19  5 DIM(13)                              VALORE GENERICO
020200020701     D $PDF            S              4  1 DIM(13)                              SCOSTAMENTO % FATTUR
020300020319     D*------------------
020400020319     D* SCHIERA DI WRK X CONTEGGIO NETWORK SU ANNO "P" E ANNO "C" (LA DIMENSIONE=NUM. MESI x NUM. NE
020500020319     D*------------------
020600020319     D WSK1            S             12    DIM(130)
020700030925     d*---
020800030925     d* Variabili
020900030925     d*---
021000030925     d WNrr            S              7  0 INZ                                  *contatore righe
021100030926     d WTipStamp2      S              1    INZ                                  *STAMP2 chiamata da
021200000000     D*------------------------------------------------------------------------*
021300000000     D* INPUT
021400000000     D*------------------------------------------------------------------------*
021500000000     D KPJBA         E DS
021600000000     D*------------------
021700000000     D* NUMERO DEI BYTE IN PACKED:
021800000000     D*------------------
021900020116     D* CLIENTI                 (11:2) + 1 =  6,5  /  (6 X 13) =  78
022000020116     D* SPEDIZIONI              (13:2) + 1 =  7,5  /  (7 X 13) =  91
022100020116     D* RICAVI                  (19:2) + 1 =  9,5  /  (9 X 13) = 117
022200020116     D* TOTALE CLIENTI          (11:2) + 1 =  6,5  /  (6 X  2) =  12
022300000000     D*------------------
022400020220     D* DS MULTIPLA TOTALI GENERALI     - 5 CATEGORIE DI CLIENTI -
022500000000     D*------------------
022600020220     D ABCGEN          DS                  OCCURS(5) INZ
022700000000     D  CNG                    1     78P 0
022800000000     D                                     DIM(13)                              NUMERO C.NUOVI
022900000000     D  SNG                   79    169P 0
023000000000     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
023100020117     D  RNG                  170    286P 5
023200000000     D                                     DIM(13)                              RICAVI REALI C.NUOVI
023300020117     D  PNG                  287    403P 5
023400000000     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
023500020116     D  CAG                  404    481P 0
023600000000     D                                     DIM(13)                              NUMERO C.ACQUISITI
023700020116     D  SAG                  482    572P 0
023800000000     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
023900020117     D  RAG                  573    689P 5
024000000000     D                                     DIM(13)                              RICAVI REALI C.ACQ
024100020117     D  PAG                  690    806P 5
024200000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
024300000000     D*---
024400020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
024500000000     D*---
024600020220     D ABCG13          DS                  OCCURS(5) INZ
024700951227     D  NTG                    1     12P 0
024800951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
024900951227     D  ATG                   13     24P 0
025000951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
025100000000     D*------------------
025200020220     D* DS MULTIPLA TOTALI DIVISIONE    - 5 CATEGORIE DI CLIENTI -
025300000000     D*------------------
025400020220     D ABCDIV          DS                  OCCURS(5) INZ
025500020117     D  CND                    1     78P 0
025600020117     D                                     DIM(13)                              NUMERO C.NUOVI
025700020117     D  SND                   79    169P 0
025800020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
025900020117     D  RND                  170    286P 5
026000020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
026100020117     D  PND                  287    403P 5
026200020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
026300020117     D  CAD                  404    481P 0
026400020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
026500020117     D  SAD                  482    572P 0
026600020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
026700020117     D  RAD                  573    689P 5
026800020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
026900020117     D  PAD                  690    806P 5
027000000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
027100000000     D*---
027200020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
027300000000     D*---
027400020220     D ABCD13          DS                  OCCURS(5) INZ
027500951227     D  NTD                    1     12P 0
027600951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
027700951227     D  ATD                   13     24P 0
027800951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
027900000000     D*------------------
028000020220     D* DS MULTIPLA TOTALI AREA         - 5 CATEGORIE DI CLIENTI -
028100000000     D*------------------
028200020220     D ABCARE          DS                  OCCURS(5) INZ
028300020117     D  CNA                    1     78P 0
028400020117     D                                     DIM(13)                              NUMERO C.NUOVI
028500020117     D  SNA                   79    169P 0
028600020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
028700020117     D  RNA                  170    286P 5
028800020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
028900020117     D  PNA                  287    403P 5
029000020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
029100020117     D  CAA                  404    481P 0
029200020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
029300020117     D  SAA                  482    572P 0
029400020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
029500020117     D  RAA                  573    689P 5
029600020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
029700020117     D  PAA                  690    806P 5
029800000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
029900000000     D*---
030000020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
030100000000     D*---
030200020220     D ABCA13          DS                  OCCURS(5) INZ
030300951227     D  NTA                    1     12P 0
030400951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
030500951227     D  ATA                   13     24P 0
030600951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
030700000000     D*------------------
030800020220     D* DS MULTIPLA TOTALI FILIALE      - 5 CATEGORIE DI CLIENTI -
030900000000     D*------------------
031000020220     D ABCFIL          DS                  OCCURS(5) INZ
031100020117     D  CNF                    1     78P 0
031200020117     D                                     DIM(13)                              NUMERO C.NUOVI
031300020117     D  SNF                   79    169P 0
031400020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
031500020117     D  RNF                  170    286P 5
031600020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
031700020117     D  PNF                  287    403P 5
031800020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
031900020117     D  CAF                  404    481P 0
032000020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
032100020117     D  SAF                  482    572P 0
032200020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
032300020117     D  RAF                  573    689P 5
032400020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
032500020117     D  PAF                  690    806P 5
032600000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
032700000000     D*---
032800020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
032900000000     D*---
033000020220     D ABCF13          DS                  OCCURS(5) INZ
033100951227     D  NTF                    1     12P 0
033200951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
033300951227     D  ATF                   13     24P 0
033400951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
033500000000     D*------------------
033600020220     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 CATEGORIE DI CLIENTI -
033700000000     D*------------------
033800020220     D ABCCOM          DS                  OCCURS(5) INZ
033900020117     D  CNC                    1     78P 0
034000020117     D                                     DIM(13)                              NUMERO C.NUOVI
034100020117     D  SNC                   79    169P 0
034200020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
034300020117     D  RNC                  170    286P 5
034400020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
034500020117     D  PNC                  287    403P 5
034600020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
034700020117     D  CAC                  404    481P 0
034800020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
034900020117     D  SAC                  482    572P 0
035000020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
035100020117     D  RAC                  573    689P 5
035200020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
035300020117     D  PAC                  690    806P 5
035400000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
035500000000     D*---
035600020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
035700000000     D*---
035800020220     D ABCC13          DS                  OCCURS(5) INZ
035900951227     D  NTC                    1     12P 0
036000951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
036100951227     D  ATC                   13     24P 0
036200951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
036300000000     D*------------------
036400020220     D* DS MULTIPLA TOTALI STAMPA       - 5 CATEGORIE DI CLIENTI -
036500000000     D*------------------
036600020220     D ABCSTA          DS                  OCCURS(5) INZ
036700020117     D  CNS                    1     78P 0
036800020117     D                                     DIM(13)                              NUMERO C.NUOVI
036900020117     D  SNS                   79    169P 0
037000020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
037100020117     D  RNS                  170    286P 5
037200020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
037300020117     D  PNS                  287    403P 5
037400020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
037500020117     D  CAS                  404    481P 0
037600020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
037700020117     D  SAS                  482    572P 0
037800020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
037900020117     D  RAS                  573    689P 5
038000020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
038100020117     D  PAS                  690    806P 5
038200000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
038300000000     D*---
038400020220     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 CATEGORIE DI CLIENTI -
038500000000     D*---
038600020220     D ABCS13          DS                  OCCURS(5) INZ
038700951227     D  NTS                    1     12P 0
038800951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
038900951227     D  ATS                   13     24P 0
039000951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
039100020318     D*------------------
039200020318     D* DS MULTIPLA TOTALI GENERALI     - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
039300020318     D*------------------
039400020318     D ABCGENN         DS                  OCCURS(5) INZ
039500020318     D  CNGN                   1     78P 0
039600020318     D                                     DIM(13)                              NUMERO C.NUOVI
039700020318     D  SNGN                  79    169P 0
039800020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
039900020318     D  RNGN                 170    286P 5
040000020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
040100020318     D  PNGN                 287    403P 5
040200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
040300020318     D  CAGN                 404    481P 0
040400020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
040500020318     D  SAGN                 482    572P 0
040600020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
040700020318     D  RAGN                 573    689P 5
040800020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
040900020318     D  PAGN                 690    806P 5
041000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
041100020318     D*---
041200020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
041300020318     D*---
041400020318     D ABCG13N         DS                  OCCURS(5) INZ
041500020318     D  NTGN                   1     12P 0
041600020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
041700020318     D  ATGN                  13     24P 0
041800020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
041900020318     D*------------------
042000020318     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
042100020318     D*------------------
042200020318     D ABCDIVN         DS                  OCCURS(5) INZ
042300020318     D  CNDN                   1     78P 0
042400020318     D                                     DIM(13)                              NUMERO C.NUOVI
042500020318     D  SNDN                  79    169P 0
042600020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
042700020318     D  RNDN                 170    286P 5
042800020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
042900020318     D  PNDN                 287    403P 5
043000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
043100020318     D  CADN                 404    481P 0
043200020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
043300020318     D  SADN                 482    572P 0
043400020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
043500020318     D  RADN                 573    689P 5
043600020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
043700020318     D  PADN                 690    806P 5
043800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
043900020318     D*---
044000020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
044100020318     D*---
044200020318     D ABCD13N         DS                  OCCURS(5) INZ
044300030924     D  NTDN                   1     12P 0
044400030924     D                                     DIM(2)                               TOTALE CLIENTI NEW
044500030924     D  ATDN                  13     24P 0
044600030924     D                                     DIM(2)                               TOTALE CLIENTI ACQ
044700030924     D*------------------
044800020318     D* DS MULTIPLA TOTALI AREA         - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
044900020318     D*------------------
045000020318     D ABCAREN         DS                  OCCURS(5) INZ
045100020318     D  CNAN                   1     78P 0
045200020318     D                                     DIM(13)                              NUMERO C.NUOVI
045300020318     D  SNAN                  79    169P 0
045400020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
045500020318     D  RNAN                 170    286P 5
045600020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
045700020318     D  PNAN                 287    403P 5
045800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
045900020318     D  CAAN                 404    481P 0
046000020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
046100020318     D  SAAN                 482    572P 0
046200020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
046300020318     D  RAAN                 573    689P 5
046400020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
046500020318     D  PAAN                 690    806P 5
046600020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
046700020318     D*---
046800020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
046900020318     D*---
047000020318     D ABCA13N         DS                  OCCURS(5) INZ
047100020318     D  NTAN                   1     12P 0
047200020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
047300020318     D  ATAN                  13     24P 0
047400020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
047500020318     D*------------------
047600020318     D* DS MULTIPLA TOTALI FILIALE      - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
047700020318     D*------------------
047800020318     D ABCFILN         DS                  OCCURS(5) INZ
047900020318     D  CNFN                   1     78P 0
048000020318     D                                     DIM(13)                              NUMERO C.NUOVI
048100020318     D  SNFN                  79    169P 0
048200020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
048300020318     D  RNFN                 170    286P 5
048400020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
048500020318     D  PNFN                 287    403P 5
048600020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
048700020318     D  CAFN                 404    481P 0
048800020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
048900020318     D  SAFN                 482    572P 0
049000020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
049100020318     D  RAFN                 573    689P 5
049200020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
049300020318     D  PAFN                 690    806P 5
049400020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
049500020318     D*---
049600020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
049700020318     D*---
049800020318     D ABCF13N         DS                  OCCURS(5) INZ
049900020318     D  NTFN                   1     12P 0
050000020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
050100020318     D  ATFN                  13     24P 0
050200020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
050300020318     D*------------------
050400020318     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
050500020318     D*------------------
050600020318     D ABCCOMN         DS                  OCCURS(5) INZ
050700020318     D  CNCN                   1     78P 0
050800020318     D                                     DIM(13)                              NUMERO C.NUOVI
050900020318     D  SNCN                  79    169P 0
051000020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
051100020318     D  RNCN                 170    286P 5
051200020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
051300020318     D  PNCN                 287    403P 5
051400020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
051500020318     D  CACN                 404    481P 0
051600020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
051700020318     D  SACN                 482    572P 0
051800020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
051900020318     D  RACN                 573    689P 5
052000020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
052100020318     D  PACN                 690    806P 5
052200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
052300020318     D*---
052400020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
052500020318     D*---
052600020318     D ABCC13N         DS                  OCCURS(5) INZ
052700020318     D  NTCN                   1     12P 0
052800020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
052900020318     D  ATCN                  13     24P 0
053000020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
053100020318     D*------------------
053200020318     D* DS MULTIPLA TOTALI STAMPA       - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
053300020318     D*------------------
053400020318     D ABCSTAN         DS                  OCCURS(5) INZ
053500020318     D  CNSN                   1     78P 0
053600020318     D                                     DIM(13)                              NUMERO C.NUOVI
053700020318     D  SNSN                  79    169P 0
053800020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
053900020318     D  RNSN                 170    286P 5
054000020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
054100020318     D  PNSN                 287    403P 5
054200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
054300020318     D  CASN                 404    481P 0
054400020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
054500020318     D  SASN                 482    572P 0
054600020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
054700020318     D  RASN                 573    689P 5
054800020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
054900020318     D  PASN                 690    806P 5
055000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
055100020318     D*---
055200020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
055300020318     D*---
055400020318     D ABCS13N         DS                  OCCURS(5) INZ
055500020318     D  NTSN                   1     12P 0
055600020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
055700020318     D  ATSN                  13     24P 0
055800020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
055900020318     D*------------------
056000020318     D* DS MULTIPLA TOTALI GENERALI     - 5 NETWORK X CATEGORIE CLIENTI "D" -
056100020318     D*------------------
056200020318     D ABCGENND        DS                  OCCURS(5) INZ
056300020318     D  CNGND                  1     78P 0
056400020318     D                                     DIM(13)                              NUMERO C.NUOVI
056500020318     D  SNGND                 79    169P 0
056600020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
056700020318     D  RNGND                170    286P 5
056800020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
056900020318     D  PNGND                287    403P 5
057000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
057100020318     D  CAGND                404    481P 0
057200020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
057300020318     D  SAGND                482    572P 0
057400020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
057500020318     D  RAGND                573    689P 5
057600020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
057700020318     D  PAGND                690    806P 5
057800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
057900020318     D*---
058000020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
058100020318     D*---
058200020318     D ABCG13ND        DS                  OCCURS(5) INZ
058300020318     D  NTGND                  1     12P 0
058400020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
058500020318     D  ATGND                 13     24P 0
058600020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
058700020318     D*------------------
058800020318     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
058900020318     D*------------------
059000020318     D ABCDIVND        DS                  OCCURS(5) INZ
059100020318     D  CNDND                  1     78P 0
059200020318     D                                     DIM(13)                              NUMERO C.NUOVI
059300020318     D  SNDND                 79    169P 0
059400020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
059500020318     D  RNDND                170    286P 5
059600020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
059700020318     D  PNDND                287    403P 5
059800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
059900020318     D  CADND                404    481P 0
060000020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
060100020318     D  SADND                482    572P 0
060200020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
060300020318     D  RADND                573    689P 5
060400020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
060500020318     D  PADND                690    806P 5
060600020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
060700020318     D*---
060800020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
060900020318     D*---
061000020318     D ABCD13ND        DS                  OCCURS(5) INZ
061100020318     D  NTDND                  1     12P 0
061200020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
061300020318     D  ATDND                 13     24P 0
061400020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
061500020318     D*------------------
061600020318     D* DS MULTIPLA TOTALI AREA         - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
061700020318     D*------------------
061800020318     D ABCAREND        DS                  OCCURS(5) INZ
061900020318     D  CNAND                  1     78P 0
062000020318     D                                     DIM(13)                              NUMERO C.NUOVI
062100020318     D  SNAND                 79    169P 0
062200020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
062300020318     D  RNAND                170    286P 5
062400020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
062500020318     D  PNAND                287    403P 5
062600020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
062700020318     D  CAAND                404    481P 0
062800020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
062900020318     D  SAAND                482    572P 0
063000020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
063100020318     D  RAAND                573    689P 5
063200020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
063300020318     D  PAAND                690    806P 5
063400020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
063500020318     D*---
063600020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
063700020318     D*---
063800020318     D ABCA13ND        DS                  OCCURS(5) INZ
063900020318     D  NTAND                  1     12P 0
064000020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
064100020318     D  ATAND                 13     24P 0
064200020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
064300020318     D*------------------
064400020318     D* DS MULTIPLA TOTALI FILIALE      - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
064500020318     D*------------------
064600020318     D ABCFILND        DS                  OCCURS(5) INZ
064700020318     D  CNFND                  1     78P 0
064800020318     D                                     DIM(13)                              NUMERO C.NUOVI
064900020318     D  SNFND                 79    169P 0
065000020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
065100020318     D  RNFND                170    286P 5
065200020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
065300020318     D  PNFND                287    403P 5
065400020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
065500020318     D  CAFND                404    481P 0
065600020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
065700020318     D  SAFND                482    572P 0
065800020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
065900020318     D  RAFND                573    689P 5
066000020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
066100020318     D  PAFND                690    806P 5
066200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
066300020318     D*---
066400030924     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
066500030924     D*---
066600030924     D ABCF13ND        DS                  OCCURS(5) INZ
066700030924     D  NTFND                  1     12P 0
066800020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
066900020318     D  ATFND                 13     24P 0
067000020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
067100020318     D*------------------
067200020318     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
067300020318     D*------------------
067400020318     D ABCCOMND        DS                  OCCURS(5) INZ
067500020318     D  CNCND                  1     78P 0
067600020318     D                                     DIM(13)                              NUMERO C.NUOVI
067700020318     D  SNCND                 79    169P 0
067800020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
067900020318     D  RNCND                170    286P 5
068000020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
068100020318     D  PNCND                287    403P 5
068200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
068300020318     D  CACND                404    481P 0
068400020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
068500020318     D  SACND                482    572P 0
068600020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
068700020318     D  RACND                573    689P 5
068800020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
068900020318     D  PACND                690    806P 5
069000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
069100020318     D*---
069200020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
069300020318     D*---
069400020318     D ABCC13ND        DS                  OCCURS(5) INZ
069500020318     D  NTCND                  1     12P 0
069600020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
069700020318     D  ATCND                 13     24P 0
069800020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
069900020318     D*------------------
070000020318     D* DS MULTIPLA TOTALI STAMPA       - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
070100020318     D*------------------
070200020318     D ABCSTAND        DS                  OCCURS(5) INZ
070300020318     D  CNSND                  1     78P 0
070400020318     D                                     DIM(13)                              NUMERO C.NUOVI
070500020318     D  SNSND                 79    169P 0
070600020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
070700020318     D  RNSND                170    286P 5
070800020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
070900020318     D  PNSND                287    403P 5
071000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
071100020318     D  CASND                404    481P 0
071200020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
071300020318     D  SASND                482    572P 0
071400020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
071500020318     D  RASND                573    689P 5
071600020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
071700020318     D  PASND                690    806P 5
071800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
071900020318     D*---
072000020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
072100020318     D*---
072200020318     D ABCS13ND        DS                  OCCURS(5) INZ
072300020318     D  NTSND                  1     12P 0
072400020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
072500020318     D  ATSND                 13     24P 0
072600020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
072700000828     D*------------------
072800000828     D* CLIENTI FIGLI IN STAMPA
072900000828     D*------------------
073000051031     C**** ridotta da 19 a 17 per far spazio ai flags x individuare i codici bloccati
073100051031     D***** SKSC            S              7  0 DIM(19)                              *CLIENTE
073200051031     D SKSC            S              7  0 DIM(17)                              *CLIENTE
073300051031     D SKbl            S              1    DIM(17)                              *Bloccato
073400120125      *---
073500120125     D*------------------
073600120125     D* DS per reperire la DATA del BLOCCO CONTABILE
073700120125     D*------------------
073800120125     D TRULBLCDS     E DS
073900120125      *---
074000120125     D Cod_Blocco_Ultimo...
074100120125     d                 s              3a
074200120125     D Data_Blocco_Ultimo...
074300120125     d                 s              8  0
074400120125     D Data_Prima_Fat_6...
074500120125     d                 s              6  0
074600120131     D Data_Prima_Fat_8...
074700120131     d                 s              8  0
074800120131     D CLP_Prima_Fat_8...
074900120131     d                 s              8  0
075000000000     D*------------------
075100000000     D* PARAMETRI FILTRO
075200000000     D*------------------
075300000000     D PARAM           DS
075400990609     D  PARDAT                 1      6  0
075500990609     D  PARAAI                 1      4  0
075600990609     D  PARMMI                 5      6  0
075700990609     D  PARDA2                 7     12  0
075800990609     D  PARAAF                 7     10  0
075900990609     D  PARMMF                11     12  0
076000990609     D  PARDCK                13     13
076100990609     D  PARDCC                14     14
076200990609     D  PARDCF                15     15
076300990609     D  PARFIC                16     18  0
076400990609     D  PARARC                19     21  0
076500990609     D  PARDIC                22     22
076600990609     D  PARDFF                23     23
076700990609     D  PARCOF                24     30  0
076800990609     D  PARDIF                31     31
076900990609     D  PARSTA                32     32
077000000825     D  PARSCE                33     33
077100990609     D  PARTSP                34     34
077200990609     D  PARIOE                35     35
077300990609     D  PARCLV                36     36
077400990609     D  PARDPT                37     42  0
077500990609     D  PARAPI                37     40  0
077600990609     D  PARMPI                41     42  0
077700990609     D  PARDP2                43     48  0
077800990609     D  PARAFP                43     46  0
077900990609     D  PARMFP                47     48  0
078000990609     D  PARSPD                49     52  1
078100990609     D  PARPDD                53     56  1
078200990609     D  PARPDA                57     60  1
078300000707     D  PARPOS                61     61
078400000825     D  PARCRE                62     62
078500010618     D  PARCL1                63     63
078600010618     D  PARCL2                64     64
078700010618     D  PARSIN                65     65
078800020404     D  PARCL3                66     66
078900020404     D  PARCL4                67     67
079000020404     D  PARNTW                68     68
079100020701     D  PARDFD                69     72  1
079200020701     D  PARDFA                73     76  1
079300020701     D  PARDFDP               77     80  1
079400020701     D  PARDFAP               81     84  1
079500030311     D  PARFPC                85     85
079600030925     D  PARWPC                86     86
079700030925     D  PARWCP                87     87
079800100129     D  PARWfdpv              88     88
079900100129      * Stat. Clienti Persi (Trimestrale x il momento)                          lancio trimestre(T)
080000100129     D  PARCLPtmp             89     89                                         M/B/T/Q/S
080100100129     D  PARStampa             90     90                                         *blk/N
080200100129     D  PARStaComm            91     91                                         *blk/N
080300000707     D*------------------
080400000707     D* DS LETTURA CAMPO ORGDE3 DI AZORG00F
080500000707     D*------------------
080600000707     D OG143         E DS
080700000000     D*------------------
080800000000     D* DS AGENTI COMMERCIALI
080900000000     D*------------------
081000140612     D**** DS01          E DS
081100000000     D*------------------
081200000000     D* DS SETTORI MERCEOLOGICI
081300000000     D*------------------
081400951006     D DS1L          E DS
081500090610     D*------------------
081600090610     D* Descrizioni tipi servizio
081700090610     D*------------------
081800090610     D DS5e          E DS
081900100129     D Quanti_Tserv    S              3s 0
082000100129     D Tserv           S              1    DIM(10)
082100100129     D DTserv          S             25    DIM(10)
082200000828     D*------------------
082300000828     D* DS DI PROCEDURA
082400000828     D*------------------
082500000828     D BS10DS        E DS                  EXTNAME(TIBS10DS)
082600000828     D  SKSC11                21   5520  0 DIM(500)                             *CLIENTI FIGLI
082700020220     D*------------------
082800020220     D* DS IMPORTANZA CLIENTI
082900020220     D*------------------
083000020220     D DSIC          E DS
083100980406     D*------------------
083200980406     D* DS REPERIMENTO DATI UTENTE
083300980406     D*-------------------
083400980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
083500980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
083600980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
083700980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
083800980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
083900951219     D*------------------
084000951219     D* COMPOSIZIONE DATA SALDI
084100951219     D*------------------
084200951219     D                 DS
084300951219     D  WFDANN                 1      4  0                                      *ANNO
084400951219     D  WFDMES                 5      6  0                                      *MESE
084500951219     D  WFDDAT                 1      6  0                                      *DATA
084600000828     D*------------------
084700000828     D* DS CONTROLLO DATA (8)
084800000828     D*------------------
084900000828     D WLBDA8          DS                  INZ
085000000828     D  G08DAT                 1      8  0
085100000828     D  G08INV                 9     16  0
085200000828     D  G08ERR                17     17
085300000828     D  G08TGI                18     22  0
085400020319     D*------------------
085500020319     D* DS DI RIEMPIMENTO SCHIERA DI WRK WSK1
085600020319     D*------------------
085700020319     D DSWSK1          DS                  INZ
085800020319     D  WFDCLI                        7  0
085900020319     D  MM                            2  0
086000020319     D  WFDNTW                        3
086100160713      *--------------------------------------
086200160713     d digits          c                   '0123456789'
086300000000     C*------------------------------------------------------------------------*
086400000000     C*?MAIN LINE
086500000000     C*------------------------------------------------------------------------*
086600000000     C*
086700000000     C* STAMPA PARAMETRI FILTRO
086800000000     C                   EXSR      STAPAR
086900000000     C*
087000000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
087100000000     C                   EXSR      SETFIL
087200000000     C*
087300000000     C* CICLO FINO A FINE FILE DI LAVORO
087400000000DO  1C     $FINE         DOWEQ     '0'
087500000000     C*
087600000000     C* OPERAZIONI PER NUOVA DIVISIONE
087700000000     C                   EXSR      NEWDIV
087800000000     C*
087900000000     C* CICLO FINO A ROTTURA DI DIVISIONE
088000000000DO  2C     $FINE         DOWEQ     '0'
088100000000     C     WFDDIV        ANDEQ     DEPDIV
088200000000     C*
088300000000     C* OPERAZIONI PER NUOVA AREA
088400000000     C                   EXSR      NEWARE
088500000000     C*
088600000000     C* CICLO FINO A ROTTURA DI AREA
088700000000DO  3C     $FINE         DOWEQ     '0'
088800000000     C     WFDDIV        ANDEQ     DEPDIV
088900000000     C     WFDARE        ANDEQ     DEPARE
089000000000     C*
089100000000     C* OPERAZIONI PER NUOVA FILIALE
089200000000     C                   EXSR      NEWFIL
089300000000     C*
089400000000     C* CICLO FINO A ROTTURA DI FILIALE
089500000000DO  4C     $FINE         DOWEQ     '0'
089600000000     C     WFDDIV        ANDEQ     DEPDIV
089700000000     C     WFDARE        ANDEQ     DEPARE
089800000000     C     WFDFIL        ANDEQ     DEPFIL
089900000000     C*
090000000000     C* OPERAZIONI PER NUOVO AGENTE
090100000000     C                   EXSR      NEWAGE
090200000000     C*
090300000000     C* CICLO FINO A ROTTURA DI AGENTE
090400000000DO  5C     $FINE         DOWEQ     '0'
090500000000     C     WFDDIV        ANDEQ     DEPDIV
090600000000     C     WFDARE        ANDEQ     DEPARE
090700000000     C     WFDFIL        ANDEQ     DEPFIL
090800000000     C     WFDAGE        ANDEQ     DEPAGE
090900000000     C*
091000120131     c* OPERAZIONI PER NUOVO CLIENTE
091100000000     C                   EXSR      NEWKSC
091200000000     C*
091300000000     C* CICLO FINO A ROTTURA DI CLIENTE
091400000000DO  6C     $FINE         DOWEQ     '0'
091500000000     C     WFDDIV        ANDEQ     DEPDIV
091600000000     C     WFDARE        ANDEQ     DEPARE
091700000000     C     WFDFIL        ANDEQ     DEPFIL
091800000000     C     WFDAGE        ANDEQ     DEPAGE
091900030924     C     WFDCLI        ANDEQ     DEPCLI
092000030924     C*
092100030924     C* OPERAZIONI PER PASSAGGIO LETTURA SALDI PRECEDENTI --> SALDI CORRENTI
092200030924     C                   EXSR      NEWPPC
092300030924     C* MEMORIZZA I SALDI
092400030924     C                   EXSR      MEMSAL
092500000000     C*
092600000000     C* LEGGE IL RECORD SUCCESSIVO
092700000000     C                   EXSR      LETFIL
092800000000     C*
092900000000E   6C                   ENDDO
093000000000     C*
093100000000     C* STAMPA IL TOTALE CLIENTE
093200000000     C                   EXSR      STAKSC
093300000000E   5C                   ENDDO
093400000000     C*
093500000000     C* STAMPA IL TOTALE COMMERCIALE
093600000000     C                   EXSR      STAAGE
093700000000E   4C                   ENDDO
093800000000     C*
093900000000     C* STAMPA IL TOTALE FILIALE
094000000000     C                   EXSR      STAFIL
094100000000E   3C                   ENDDO
094200000000     C*
094300000000     C* STAMPA IL TOTALE AREA
094400000000     C                   EXSR      STAARE
094500000000E   2C                   ENDDO
094600000000     C*
094700000000     C* STAMPA IL TOTALE DIVISIONE
094800000000     C                   EXSR      STADIV
094900000000E   1C                   ENDDO
095000000000     C*
095100000000     C* STAMPA IL TOTALE GENERALE
095200000000     C                   EXSR      STAGEN
095300000000     C*
095400000000     C* STAMPA RIGA FINE STAMPA
095500100129     c                   if        parSTAMPA <> 'N'
095600030311     C                   WRITE     SE94LE
095700020116     C                   WRITE     SE94FI
095800100129     c                   end
095900980710     C* OPERAZIONI FINALI
096000980710     C                   EXSR      FINSR
096100000000     C*
096200000000     C                   SETON                                        LR
096300000000     C*------------------------------------------------------------------------*
096400000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
096500000000     C*------------------------------------------------------------------------*
096600000000     C     SETFIL        BEGSR
096700980529     C*
096800000000     C                   MOVEL     *BLANKS       KWDDIV                         *DIVISIONE
096900000000     C                   Z-ADD     *ZEROS        KWDARE                         *AREA
097000000000     C                   Z-ADD     *ZEROS        KWDFIL                         *FILIALE
097100000000     C                   Z-ADD     *ZEROS        KWDAGE                         *COMMERCIALE
097200960710     C                   MOVEL     *BLANKS       KWDCLV                         *CODICE QUALITA'
097300000825     C                   Z-ADD     *ZEROS        KWDCLI                         *CLIENTE
097400000000     C                   Z-ADD     *ZEROS        KWDANN                         *ANNO
097500000000     C                   Z-ADD     *ZEROS        KWDMES                         *MESE
097600100127     C     KEYDEL        SETLL     WFDEA01L                           99        *FINE ARCHIVIO
097700951219IF  1C     *IN99         IFEQ      *ON
097800000000     C                   MOVEL     '1'           $FINE
097900000000X   1C                   ELSE
098000000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
098100000000E   1C                   ENDIF
098200000000     C*
098300000000     C                   ENDSR
098400000000     C*------------------------------------------------------------------------*
098500000000     C*?LETFIL - LETTURA PROSSIMO RECORD DEL WORK FILE
098600000000     C*------------------------------------------------------------------------*
098700000000     C     LETFIL        BEGSR
098800000000     C*
098900000000     C* LEGGO FINO A:
099000000000     C*     - FINE FILE
099100000000     C*     - TROVATO RECORD VALIDO
099200000000     C                   MOVEL     'N'           WRECOK
099300000000DO  1C     $FINE         DOWEQ     '0'
099400000000     C     WRECOK        ANDEQ     'N'
099500000000     C*
099600000000     C* LETTURE SUCCESSIVE ALLA PRIMA
099700100127     C                   READ      WFDEA01L                               99
099800000000     C*
099900951219IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
100000000000     C                   MOVEL     '1'           $FINE
100100000000X   2C                   ELSE
100200000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
100300000000E   2C                   ENDIF
100400000000     C*
100500000000E   1C                   ENDDO
100600000000     C*
100700000000     C                   ENDSR
100800000000     C*------------------------------------------------------------------------*
100900000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD DEL WORK FILE
101000000000     C*------------------------------------------------------------------------*
101100000000     C     CHKREC        BEGSR
101200000000     C*
101300000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
101400000000     C*
101500990609IF  1C     WFDDAT        IFGE      PARDPT
101600990609     C     WFDDAT        ANDLE     PARDP2
101700951219     C                   MOVEL     'P'           TIPANN                         *PERIODO PRECEDENTE
101800071123     C*
101900071123     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
102000071123     C* a cambio di codice cliente. Essendo comunuque il periodo Precedente deve
102100071123     C* essere considerato ACQUISITO E NON NUOVO e SE LO E' RIMANE tale anche
102200071123     C* PER TUTTO IL PERIODO CORRENTE
102300071123IF  2C                   If        FlaPAS = '0'
102400071123     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
102500071123     C                   clear                   DESCLN                         *NUOVO O NON NUOVO
102600071123E   2C                   EndIF
102700071123      *
102800951219E   1C                   ENDIF
102900071123      *
103000990609IF  1C     WFDDAT        IFGE      PARDAT
103100990609     C     WFDDAT        ANDLE     PARDA2
103200951219     C                   MOVEL     'C'           TIPANN                         *PERIODO CORRENTE
103300000000     C*
103400000000     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
103500000000     C* VEDO SE IL CLIENTE E' NUOVO (PER FORZA IL PRIMO SALDO); SE LO E' RIMANE
103600000000     C* TALE PER TUTTO IL PERIODO CORRENTE
103700071123     C* Attenzione: poichè se il codice cliente precedentemente letto aveva solo
103800071123     C*  solo dati su Anno precedente e il cliente appena letto è nuovo per anno
103900071123     C*  in corso sovrappone il Nuovo/Acquisito su DESCLN poichè si ritrovava il FLAPAS = '0'
104000071123     C*  per proteggersi da tale problema occorre controllare che il test venga fatto
104100071123     C*  quando non si è alivello di rottura livello cliente.
104200000000IF  2C     FLAPAS        IFEQ      '0'
104300071123     c                   if        wfdCLI = depCLI
104400000000     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
104500000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O NON NUOVO
104600071123     c                   end
104700000000E   2C                   ENDIF
104800070822      *
104900070822      *  Può esistere il caso che il cliente sia nuovo nel mese di febbraio x ESTERO
105000070822      *  ed abbia fatturato Italia solo in giugno.
105100070822      *  Lanciando la stampa come Italia l'informazione letta, ossia il record di giugno, non
105200070822      *  avrà "Y" come NUOVO ma avrà "S".
105300070822      *  Il programma necessita della "Y" nei suoi test per considerare il cliente come NUOVO.
105400070822      *   --> Occorre memorizzarsi qui comunque cosa vale il flag del primo record letto sia
105500070822      *   che sia Italia/Estero per poi forzarlo nella routine di decodifica del cliente.
105600070822IF  2C     wfdCLI        ifne      sav_depCLI
105700070822IF  2C     wfdIOE        andne     parIOE
105800070822IF  2C     parIOE        andne     *blank
105900070822      *
106000070822      * lo salva per poterlo forzare in seguito
106100070822     c                   clear                   sav_desCLN_Y
106200070822IF  2C                   if        sav_depCLI <> wfdCLI
106300070822IF  2C                                       and wfdCLN ='Y'
106400070822     c                   move      'Y'           sav_desCLN_Y
106500070822     c                   end
106600070822      *
106700070822     c                   eval      sav_depCLI  =  wfdCLI
106800070822     c                   end
106900070822      *
107000000000E   1C                   ENDIF
107100070822      *
107200951018     C*--------------------
107300951018     C* SELEZIONI
107400951018     C*--------------------
107500951018     C* SE FILIALE CLIENTE <> FILIALE RICHIESTA, ESCLUDO
107600951018IF  1C     PARFIC        IFGT      *ZEROS                                       *DELTA FILIALE
107700951018     C     WFDFIL        ANDNE     PARFIC
107800951018     C                   MOVEL     'N'           WRECOK
107900951018E   1C                   ENDIF
108000951018     C*
108100951018     C* SE AREA CLIENTE <> AREA RICHIESTA, ESCLUDO
108200951018IF  1C     PARARC        IFGT      *ZEROS                                       *DELTA FILIALE
108300951018     C     WFDARE        ANDNE     PARARC
108400951018     C                   MOVEL     'N'           WRECOK
108500951018E   1C                   ENDIF
108600951018     C*
108700030924     C* SE DIVISIONE CLIENTE <> DIVISIONE RICHIESTA, ESCLUDO
108800951018IF  1C     PARDIC        IFNE      *BLANKS                                      *DELTA FILIALE
108900030924     C     WFDDIV        ANDNE     PARDIC
109000030924     C                   MOVEL     'N'           WRECOK
109100030924E   1C                   ENDIF
109200030924     C*
109300951018     C* SE CODICE QUALITA' CLIENTE <> CODICE QUALITA' RICHIESTO, ESCLUDO
109400010618IF  1C                   IF        (PARCLV <> *blanks and WFDCLV = PARCLV) or
109500010618     C                             (PARCL1 <> *blanks and WFDCLV = PARCL1) or
109600030924     C                             (PARCL2 <> *blanks and WFDCLV = PARCL2) or
109700030924     C                             (PARCLV = *blanks  and
109800030924     C                              PARCL1 = *blanks  and
109900030924     C                              PARCL2 = *blanks)
110000030924     C                   ELSE
110100951018     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
110200951018E   1C                   ENDIF
110300000000     C*
110400951018     C* SE TIPO DATI STATISTCA ITALIA/ESTERO <> TIPO DATI RICHIESTO, ESCLUDO
110500950210IF  1C     PARIOE        IFNE      *BLANKS
110600950210     C     PARIOE        ANDNE     WFDIOE
110700950208     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
110800950210E   1C                   ENDIF
110900000707     C*
111000000707     C* SE DATI NO POSTE ESCLUDE LE FILIALI POSTE, E VICEVERSA
111100000707IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
111200000707     C                   SETOFF                                           28
111300000707     C                   Z-ADD     1             I                 5 0
111400000707     C     WFDFIL        LOOKUP    FIL(I)                                 28
111500000707IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
111600020627IF  3C     POS(I)        IFEQ      'PPT'                                         FIL = POSTE
111700000707     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
111800000711E   2C                   ENDIF
111900000711X   2C                   ELSE                                                   *SI DATI POSTE
112000020627IF  3C     POS(I)        IFNE      'PPT'                                         FIL <> POSTE
112100000707     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
112200000711E   3C                   ENDIF
112300000711E   2C                   ENDIF
112400000707E   1C                   ENDIF
112500980528     C*
112600000000     C                   ENDSR
112700000000     C*------------------------------------------------------------------------*
112800000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
112900000000     C*------------------------------------------------------------------------*
113000000000     C     NEWDIV        BEGSR
113100020220     C*
113200020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
113300020220     C                   MOVEL     '0'           FLGSIDD
113400000000     C*
113500000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
113600000000     C                   MOVEL     '0'           PASDIV
113700000000     C*
113800000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
113900000000     C                   MOVEL     WFDDIV        DEPDIV
114000000000     C*
114100000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
114200000000     C                   EXSR      DECDIV
114300000000     C*
114400020318     C* AZZERA I TOTALI DELLA DIVISIONE (X QUALITA' CLIENTE)
114500020221DO  1C     1             DO        5             Q
114600000000     C     Q             OCCUR     ABCDIV
114700000000     C                   CLEAR                   CND
114800000000     C                   CLEAR                   SND
114900000000     C                   CLEAR                   RND
115000000000     C                   CLEAR                   PND
115100000000     C                   CLEAR                   CAD
115200000000     C                   CLEAR                   SAD
115300000000     C                   CLEAR                   RAD
115400000000     C                   CLEAR                   PAD
115500000000E   1C                   ENDDO
115600000000     C*
115700000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
115800020221DO  1C     1             DO        5             Q
115900000000     C     Q             OCCUR     ABCD13
116000000000     C                   CLEAR                   NTD
116100000000     C                   CLEAR                   ATD
116200000000E   1C                   ENDDO
116300020318     C*
116400020318     C* AZZERA I TOTALI DELLA DIVISIONE (X NETWORK) NN "D"
116500020318DO  1C     1             DO        5             K
116600020318     C     K             OCCUR     ABCDIVN
116700020318     C                   CLEAR                   CNDN
116800020318     C                   CLEAR                   SNDN
116900020318     C                   CLEAR                   RNDN
117000020318     C                   CLEAR                   PNDN
117100020318     C                   CLEAR                   CADN
117200020318     C                   CLEAR                   SADN
117300020318     C                   CLEAR                   RADN
117400020318     C                   CLEAR                   PADN
117500020318E   1C                   ENDDO
117600020318     C*
117700020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
117800020318DO  1C     1             DO        5             K
117900020318     C     K             OCCUR     ABCD13N
118000020318     C                   CLEAR                   NTDN
118100020318     C                   CLEAR                   ATDN
118200020318E   1C                   ENDDO
118300020318     C*
118400020318     C* AZZERA I TOTALI DELLA DIVISIONE (X NETWORK) "D"
118500020318DO  1C     1             DO        5             K
118600020318     C     K             OCCUR     ABCDIVND
118700020318     C                   CLEAR                   CNDND
118800020318     C                   CLEAR                   SNDND
118900020318     C                   CLEAR                   RNDND
119000020318     C                   CLEAR                   PNDND
119100020318     C                   CLEAR                   CADND
119200020318     C                   CLEAR                   SADND
119300020318     C                   CLEAR                   RADND
119400020318     C                   CLEAR                   PADND
119500020318E   1C                   ENDDO
119600020318     C*
119700020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
119800020318DO  1C     1             DO        5             K
119900020318     C     K             OCCUR     ABCD13ND
120000020318     C                   CLEAR                   NTDND
120100020318     C                   CLEAR                   ATDND
120200020318E   1C                   ENDDO
120300000000     C*
120400000000     C                   ENDSR
120500000000     C*------------------------------------------------------------------------*
120600000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
120700000000     C*------------------------------------------------------------------------*
120800000000     C     NEWARE        BEGSR
120900020220     C*
121000020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
121100020220     C                   MOVEL     '0'           FLGSIDA
121200000000     C*
121300000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
121400000000     C                   MOVEL     '0'           PASARE
121500000000     C*
121600000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
121700000000     C                   MOVEL     WFDARE        DEPARE
121800000000     C*
121900000000     C* DECODIFICA IL CODICE AREA
122000000000     C                   EXSR      DECARE
122100000000     C*
122200020318     C* AZZERA I TOTALI DELL'AREA (X QUALITA' CLIENTE)
122300020221DO  1C     1             DO        5             Q
122400000000     C     Q             OCCUR     ABCARE
122500000000     C                   CLEAR                   CNA
122600000000     C                   CLEAR                   SNA
122700000000     C                   CLEAR                   RNA
122800000000     C                   CLEAR                   PNA
122900000000     C                   CLEAR                   CAA
123000000000     C                   CLEAR                   SAA
123100000000     C                   CLEAR                   RAA
123200000000     C                   CLEAR                   PAA
123300000000E   1C                   ENDDO
123400000000     C*
123500000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
123600020221DO  1C     1             DO        5             Q
123700000000     C     Q             OCCUR     ABCA13
123800000000     C                   CLEAR                   NTA
123900000000     C                   CLEAR                   ATA
124000000000E   1C                   ENDDO
124100020318     C*
124200020318     C* AZZERA I TOTALI DELL'AREA (X NETWORK) NN "D"
124300020318DO  1C     1             DO        5             K
124400020318     C     K             OCCUR     ABCAREN
124500020318     C                   CLEAR                   CNAN
124600020318     C                   CLEAR                   SNAN
124700020318     C                   CLEAR                   RNAN
124800020318     C                   CLEAR                   PNAN
124900020318     C                   CLEAR                   CAAN
125000020318     C                   CLEAR                   SAAN
125100020318     C                   CLEAR                   RAAN
125200020318     C                   CLEAR                   PAAN
125300020318E   1C                   ENDDO
125400020318     C*
125500020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
125600020318DO  1C     1             DO        5             K
125700020318     C     K             OCCUR     ABCA13N
125800020318     C                   CLEAR                   NTAN
125900020318     C                   CLEAR                   ATAN
126000020318E   1C                   ENDDO
126100020318     C*
126200020318     C* AZZERA I TOTALI DELL'AREA (X NETWORK) "D"
126300020318DO  1C     1             DO        5             K
126400020318     C     K             OCCUR     ABCAREND
126500020318     C                   CLEAR                   CNAND
126600020318     C                   CLEAR                   SNAND
126700020318     C                   CLEAR                   RNAND
126800020318     C                   CLEAR                   PNAND
126900020318     C                   CLEAR                   CAAND
127000020318     C                   CLEAR                   SAAND
127100020318     C                   CLEAR                   RAAND
127200020318     C                   CLEAR                   PAAND
127300020318E   1C                   ENDDO
127400020318     C*
127500020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
127600020318DO  1C     1             DO        5             K
127700020318     C     K             OCCUR     ABCA13ND
127800020318     C                   CLEAR                   NTAND
127900020318     C                   CLEAR                   ATAND
128000020318E   1C                   ENDDO
128100000000     C*
128200000000     C                   ENDSR
128300000000     C*------------------------------------------------------------------------*
128400000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
128500000000     C*------------------------------------------------------------------------*
128600000000     C     NEWFIL        BEGSR
128700020220     C*
128800020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
128900020220     C                   MOVEL     '0'           FLGSIDF
129000000000     C*
129100000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
129200000000     C                   MOVEL     '0'           PASFIL
129300000000     C*
129400000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
129500000000     C                   Z-ADD     WFDFIL        DEPFIL
129600000000     C*
129700000000     C* DECODIFICA IL CODICE FILIALE
129800000000     C                   EXSR      DECFIL
129900000000     C*
130000020318     C* AZZERA I TOTALI DELLA FILIALE (X QUALITA' CLIENTE)
130100020221DO  1C     1             DO        5             Q
130200000000     C     Q             OCCUR     ABCFIL
130300000000     C                   CLEAR                   CNF
130400000000     C                   CLEAR                   SNF
130500000000     C                   CLEAR                   RNF
130600000000     C                   CLEAR                   PNF
130700000000     C                   CLEAR                   CAF
130800000000     C                   CLEAR                   SAF
130900000000     C                   CLEAR                   RAF
131000000000     C                   CLEAR                   PAF
131100000000E   1C                   ENDDO
131200000000     C*
131300000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
131400020221DO  1C     1             DO        5             Q
131500000000     C     Q             OCCUR     ABCF13
131600000000     C                   CLEAR                   NTF
131700000000     C                   CLEAR                   ATF
131800000000E   1C                   ENDDO
131900020318     C*
132000020318     C* AZZERA I TOTALI DELLA FILIALE (X NETWORK) NN "D"
132100020318DO  1C     1             DO        5             K
132200020318     C     K             OCCUR     ABCFILN
132300020318     C                   CLEAR                   CNFN
132400020318     C                   CLEAR                   SNFN
132500020318     C                   CLEAR                   RNFN
132600020318     C                   CLEAR                   PNFN
132700020318     C                   CLEAR                   CAFN
132800020318     C                   CLEAR                   SAFN
132900020318     C                   CLEAR                   RAFN
133000020318     C                   CLEAR                   PAFN
133100020318E   1C                   ENDDO
133200020318     C*
133300020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
133400020318DO  1C     1             DO        5             K
133500020318     C     K             OCCUR     ABCF13N
133600020318     C                   CLEAR                   NTFN
133700020318     C                   CLEAR                   ATFN
133800020318E   1C                   ENDDO
133900020318     C*
134000020318     C* AZZERA I TOTALI DELLA FILIALE (X NETWORK) "D"
134100020318DO  1C     1             DO        5             K
134200020318     C     K             OCCUR     ABCFILND
134300020318     C                   CLEAR                   CNFND
134400020318     C                   CLEAR                   SNFND
134500020318     C                   CLEAR                   RNFND
134600020318     C                   CLEAR                   PNFND
134700020318     C                   CLEAR                   CAFND
134800020318     C                   CLEAR                   SAFND
134900020318     C                   CLEAR                   RAFND
135000020318     C                   CLEAR                   PAFND
135100020318E   1C                   ENDDO
135200020318     C*
135300020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
135400020318DO  1C     1             DO        5             K
135500020318     C     K             OCCUR     ABCF13ND
135600020318     C                   CLEAR                   NTFND
135700020318     C                   CLEAR                   ATFND
135800020318E   1C                   ENDDO
135900000000     C*
136000000000     C                   ENDSR
136100000000     C*------------------------------------------------------------------------*
136200000000     C*?NEWAGE - OPERAZIONI PER NUOVO AGENTE COMMERCIALE
136300000000     C*------------------------------------------------------------------------*
136400000000     C     NEWAGE        BEGSR
136500020220     C*
136600030924     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
136700030924     C                   MOVEL     '0'           FLGSIDE
136800030924     C*
136900030924     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
137000030924     C                   MOVEL     '0'           PASAGE
137100030924     C*
137200030924     C* MEMORIZZO IL NUOVO AGENTE IN UN DEPOSITO
137300030924     C                   Z-ADD     WFDAGE        DEPAGE
137400081013     C*
137500081013     C* l'Agente Unificante e la sua descrizione
137600081013     C                   Z-ADD     WFDrgf        DEPrgf
137700081013     C                   movel     WFDrgd        DEPrgd
137800081013     C*
137900000000     C* DECODIFICO IL CODICE AGENTE
138000000000     C                   EXSR      DECAGE
138100000000     C*
138200020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X QUALITA' CLIENTE)
138300020221DO  1C     1             DO        5             Q
138400000000     C     Q             OCCUR     ABCCOM
138500000000     C                   CLEAR                   CNC
138600000000     C                   CLEAR                   SNC
138700000000     C                   CLEAR                   RNC
138800000000     C                   CLEAR                   PNC
138900000000     C                   CLEAR                   CAC
139000000000     C                   CLEAR                   SAC
139100000000     C                   CLEAR                   RAC
139200000000     C                   CLEAR                   PAC
139300000000E   1C                   ENDDO
139400000000     C*
139500000000     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
139600020221DO  1C     1             DO        5             Q
139700000000     C     Q             OCCUR     ABCC13
139800000000     C                   CLEAR                   NTC
139900000000     C                   CLEAR                   ATC
140000000000E   1C                   ENDDO
140100020318     C*
140200020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X NETWORK) NN "D"
140300020318DO  1C     1             DO        5             K
140400020318     C     K             OCCUR     ABCCOMN
140500020318     C                   CLEAR                   CNCN
140600020318     C                   CLEAR                   SNCN
140700020318     C                   CLEAR                   RNCN
140800020318     C                   CLEAR                   PNCN
140900020318     C                   CLEAR                   CACN
141000020318     C                   CLEAR                   SACN
141100020318     C                   CLEAR                   RACN
141200020318     C                   CLEAR                   PACN
141300020318E   1C                   ENDDO
141400020318     C*
141500020318     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
141600020318DO  1C     1             DO        5             K
141700020318     C     K             OCCUR     ABCC13N
141800020318     C                   CLEAR                   NTCN
141900020318     C                   CLEAR                   ATCN
142000020318E   1C                   ENDDO
142100020318     C*
142200020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X NETWORK) "D"
142300020318DO  1C     1             DO        5             K
142400020318     C     K             OCCUR     ABCCOMND
142500020318     C                   CLEAR                   CNCND
142600020318     C                   CLEAR                   SNCND
142700020318     C                   CLEAR                   RNCND
142800020318     C                   CLEAR                   PNCND
142900020318     C                   CLEAR                   CACND
143000020318     C                   CLEAR                   SACND
143100020318     C                   CLEAR                   RACND
143200020318     C                   CLEAR                   PACND
143300020318E   1C                   ENDDO
143400020318     C*
143500020318     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
143600020318DO  1C     1             DO        5             K
143700020318     C     K             OCCUR     ABCC13ND
143800020318     C                   CLEAR                   NTCND
143900020318     C                   CLEAR                   ATCND
144000020318E   1C                   ENDDO
144100000000     C*
144200000000     C                   ENDSR
144300000000     C*------------------------------------------------------------------------*
144400000000     C*?NEWKSC - OPERAZIONI PER NUOVO CLIENTE
144500000000     C*------------------------------------------------------------------------*
144600000000     C     NEWKSC        BEGSR
144700020221     C*
144800020221     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
144900020221     C                   MOVEL     '0'           FLGSIDK
145000000000     C*
145100000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
145200000000     C                   MOVEL     '0'           PASKSC
145300000000     C*
145400951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (13^ COLONNA) PERIODO CORRENTE
145500000000     C                   MOVEL     '0'           FLAKSC
145600951227     C*
145700951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (1^ COLONNA) PERIODO PRECEDENTE
145800951227     C                   MOVEL     '0'           FL2KSC
145900000000     C*
146000000000     C* REIMPOSTO FLAG PASSAGGIO LETTURA SALDI PRECEDENTI A SALDI CORRENTI
146100000000     C                   MOVEL     '0'           FLAPAS
146200000830     C*
146300000830     C* REIMPOSTO FLAG CONTATORE CLIENTI PER MESE
146400000830     C                   MOVEL     '0'           SFCCL
146500000000     C*
146600000000     C* MEMORIZZO IL NUOVO CLIENTE IN UN DEPOSITO
146700000825     C                   Z-ADD     WFDCLI        DEPCLI
146800020319     C*
146900020319     C* RE-INIZIALIZZO LA SCHIERA DI WRK X CLIENTE/MESE/NETWORK
147000020319     C                   Z-ADD     *ZEROS        Y
147100020319     C                   CLEAR                   WSK1
147200000000     C*
147300000000     C* DECODIFICO IL CODICE CLIENTE
147400000000     C                   EXSR      DECKSC
147500000000     C*
147600000000     C* AZZERA I TOTALI DEL CLIENTE
147700121217     C                   CLEAR                   CLN1K
147800000000     C                   CLEAR                   S1K
147900081013     C                   CLEAR                   Kg1K
148000000000     C                   CLEAR                   R1K
148100000000     C                   CLEAR                   P1K
148200970822     C                   CLEAR                   D1K
148300020712     C                   CLEAR                   R1KP
148400070419     C                   CLEAR                   ap_S1K
148500081013     C                   CLEAR                   ap_Kg1K
148600070419     C                   CLEAR                   ap_R1K
148700070419     C                   CLEAR                   ap_P1K
148800091123      * -------------
148900100204     /*?  solo x per l'Estero                                   ?
149000100204     /*?     x Network                                          ?
149100100204     /*?   Pulisce le schiere di totale                         ?
149200091123      * -------------
149300091123IF  1C     PARIOE        IFEQ      'E'
149400091123      * -------------
149500091123DO  1C     1             DO        5             K
149600100204     C* AZZERA I TOTALI x NETWORK anno in corso
149700091124     C     K             OCCUR     W1ntw
149800091124     C                   CLEAR                   C1ntw
149900091124     C                   CLEAR                   S1ntw
150000091124     C                   CLEAR                   KG1ntw
150100091124     C                   CLEAR                   R1ntw
150200091124     C                   CLEAR                   P1ntw
150300091124     C                   CLEAR                   D1ntw
150400091124     C                   CLEAR                   R1Pntw
150500091125     C                   CLEAR                   TR1ntw
150600091125     C                   CLEAR                   TP1ntw
150700091124     C* AZZERA I TOTALI x NETWORK  dell'Anno precedente
150800091124     C     K             OCCUR     Wap1ntw
150900091124     C                   CLEAR                   ap_C1ntw
151000091124     C                   CLEAR                   ap_S1ntw
151100091124     C                   CLEAR                   ap_KG1ntw
151200091124     C                   CLEAR                   ap_R1ntw
151300091124     C                   CLEAR                   ap_P1ntw
151400091124     C                   CLEAR                   ap_D1ntw
151500091124     C                   CLEAR                   ap_R1Pntw
151600091125     C                   CLEAR                   ap_TR1ntw
151700091125     C                   CLEAR                   ap_TP1ntw
151800091123E   1C                   ENDDO
151900091123      * -------------
152000091123E   1C                   END
152100091123      * -------------
152200100204     /*?  Pulizia schiere di totali                             ?
152300100204     /*?    x  ntw e tipi servizio                              ?
152400100204     /*?      sia Italia che Estero                             ?
152500100129      * -------------
152600100129DO  1C     1             DO        50            Kserv             3 0
152700100201     C* AZZERA I TOTALI x NETWORK/TIPO SERVIZIO
152800100129     C     Kserv         OCCUR     W1ntwTS
152900100202      * chiave NTW + TServ
153000100129     C                   CLEAR                   C1ntwTS
153100100201     C                   CLEAR                   C1tpsTS
153200100202     C* AZZERA I TOTALI x NETWORK  dell'Anno in Corso
153300100129     C                   CLEAR                   S1ntwTS
153400100129     C                   CLEAR                   KG1ntwTS
153500100129     C                   CLEAR                   R1ntwTS
153600100129     C                   CLEAR                   P1ntwTS
153700100129     C                   CLEAR                   D1ntwTS
153800100129     C                   CLEAR                   R1PntwTS
153900100129     C                   CLEAR                   TR1ntwTS
154000100129     C                   CLEAR                   TP1ntwTS
154100100129     C* AZZERA I TOTALI x NETWORK  dell'Anno precedente
154200100129     C                   CLEAR                   ap_S1ntwTS
154300100129     C                   CLEAR                   ap_KG1ntwTS
154400100129     C                   CLEAR                   ap_R1ntwTS
154500100129     C                   CLEAR                   ap_P1ntwTS
154600100129     C                   CLEAR                   ap_D1ntwTS
154700100129     C                   CLEAR                   ap_R1PntwTS
154800100129     C                   CLEAR                   ap_TR1ntwTS
154900100129     C                   CLEAR                   ap_TP1ntwTS
155000100202     C*
155100100129E   1C                   ENDDO
155200100129      * -------------
155300000828     C* CERCA I FIGLI DEL CLIENTE, SE RICHIESTA STAMPA PER UNFICANTE
155400000828     C                   CLEAR                   SKSC11
155500000828IF  1C     PARSCE        IFEQ      'U'                                          *SCELTA UNIFICANTE
155600000828     C                   CLEAR                   BS10DS
155700010725     C*                  Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
155800010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
155900000828     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
156000000828     C                   MOVEL     'F'           D10PAF                         *CERCA I FIGLI
156100000828     C                   MOVE      DEPCLI        D10COD
156200000828     C                   CALL      'TIBS10R'
156300000828     C                   PARM                    BS10DS
156400000828E   1C                   ENDIF
156500000000     C*
156600130131     C*  Totalizza le Varie per CLIENTE sul nuovo WADELPCV dal WFDEAV0F
156700130131     C*   man mano che le riporta sul file di TOTALE le ELIMINA dal file di WORK.
156800130131     c                   exsr      VARIE_x_Client
156900130131     C*
157000000000     C                   ENDSR
157100000000     C*------------------------------------------------------------------------*
157200000000     C*?NEWPPC - OPERAZIONI PER PASSAGGIO LETTURA SALDI PRECED-->SALDI CORRENTI
157300000000     C*------------------------------------------------------------------------*
157400000000     C     NEWPPC        BEGSR
157500000000     C*
157600000000     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
157700000000     C* PRENDO COME "CLIENTE NUOVO/ACQUISITO" QUELLO DEL PRIMO SALDO PERCHE' SE
157800000000     C* UN CLIENTE E' NUOVO PER UN MESE ("Y") LO E' PER TUTTO IL PERIODO
157900000000IF  1C     TIPANN        IFEQ      'C'
158000000000IF  2C     FLAPAS        IFEQ      '0'
158100000000     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
158200000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O NON NUOVO
158300000000E   2C                   ENDIF
158400000000E   1C                   ENDIF
158500000000     C*
158600030924     C                   ENDSR
158700030924     C*------------------------------------------------------------------------*
158800030924     C*?MEMSAL - MEMORIZZA I SALDI DEL WORK FILE IN SCHIERE
158900030924     C*------------------------------------------------------------------------*
159000030924     C     MEMSAL        BEGSR
159100030924     C*
159200030924     C* I SALDI DEL PERIODO PRECEDENTE LI MEMORIZZA IN 13^ POSIZIONE DI SCHIERA
159300030924     C* UN CLIENTE E' NUOVO NEL PERIODO SOLO SE HA UNA "Y" NEI SALDI
159400030924     C*
159500030924     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
159600030924     C                   MOVEL     '1'           PASKSC                         *CLIENTE
159700030924     C                   MOVEL     '1'           PASAGE                         *AGENTE
159800030924     C                   MOVEL     '1'           PASFIL                         *FILIALE
159900030924     C                   MOVEL     '1'           PASARE                         *AREA
160000030924     C                   MOVEL     '1'           PASDIV                         *DIVISIONE
160100030924     C                   MOVEL     '1'           PASGEN                         *GENERALE
160200000000     C*
160300951227     C* CONTA I CLIENTI DA STAMPARE ALLA 1^ O 13^ COLONNA SOLO A ROTTURA DI
160400951227     C* CLIENTE (SE STA LEGGENDO I SALDI DEL PERIODO PRECEDENTE O CORRENTE)
160500951227IF  1C     TIPANN        IFEQ      'C'                                          *CORRENTE
160600951227IF  2C     FLAKSC        IFEQ      '0'                                          *ROTTURA CLIENTE
160700000000     C                   MOVEL     '1'           FLAKSC
160800951227     C                   Z-ADD     1             CONKSC            1 0          *CLIENTE NON AGGIUNT
160900951227X   2C                   ELSE
161000951227     C                   Z-ADD     *ZEROS        CONKSC                         *CLIENTE AGGIUNTO
161100951227E   2C                   ENDIF
161200951227X   1C                   ELSE                                                   *PRECEDENTE
161300951227IF  2C     FL2KSC        IFEQ      '0'                                          *ROTTURA CLIENTE
161400951227     C                   MOVEL     '1'           FL2KSC
161500951227     C                   Z-ADD     1             CO2KSC            1 0          *CLIENTE AGGIUNTO
161600951227X   2C                   ELSE
161700951227     C                   Z-ADD     *ZEROS        CO2KSC                         *CLIENTE NON AGGIUNT
161800951227E   2C                   ENDIF
161900951227E   1C                   ENDIF
162000020318     C*
162100020318     C* Q U A L I T A'  /  N E T W O R K
162200000000     C                   Z-ADD     1             Q
162300000000     C                   SETOFF                                           28
162400000000     C     DESCLV        LOOKUP    ABC(Q)                                 28
162500020318     C                   Z-ADD     1             K
162600020318     C                   SETOFF                                           27
162700020318     C     WFDNTW        LOOKUP    DNTW(K)                                27
162800000000IF  1C     *IN28         IFEQ      *ON
162900020318IF  1C     *IN27         ANDEQ     *ON
163000000000     C* M E S E
163100990609IF  2C     WFDDAT        IFGE      PARDPT                                       *PERIODO PRECEDENTE
163200990609     C     WFDDAT        ANDLE     PARDP2
163300000000     C                   Z-ADD     13            MM
163400020712     C                   Z-ADD     1             MMP               2 0
163500020712     C                   SETOFF                                           33
163600020712     C     WFDMES        LOOKUP    MES(MMP)                               33
163700951219E   2C                   ENDIF
163800990609IF  2C     WFDDAT        IFGE      PARDAT                                       *PERIODO CORRENTE
163900990609     C     WFDDAT        ANDLE     PARDA2
164000000000     C                   Z-ADD     1             MM
164100000000     C                   SETOFF                                           28
164200000000     C     WFDMES        LOOKUP    MES(MM)                                28
164300000000E   2C                   ENDIF
164400020220     C*
164500020220     C* VERIFICO SE ALMENO 1 CLIENTE "D"
164600020318     C                   IF        DESCLV = 'D'
164700020220     C                   MOVEL     '1'           FLGSIDG           1
164800020220     C                   MOVEL     '1'           FLGSIDD           1
164900020220     C                   MOVEL     '1'           FLGSIDA           1
165000020220     C                   MOVEL     '1'           FLGSIDF           1
165100020220     C                   MOVEL     '1'           FLGSIDE           1
165200020221     C                   MOVEL     '1'           FLGSIDK           1
165300020220     C                   ENDIF
165400020319     C*
165500020319     C* VERIFICO SE X GIA' CONTEGGIATO IL CLIENTE/MESE/NETWORK
165600020319     C     DSWSK1        LOOKUP    WSK1                                   55
165700020319     C                   IF        *IN55 = *ON
165800020319     C                   Z-ADD     *ZEROS        CONNTW            1 0
165900020319     C                   ELSE
166000020319     C                   ADD       1             Y                 3 0
166100020319     C                   EVAL      WSK1(Y) = DSWSK1
166200020319     C                   Z-ADD     1             CONNTW
166300020319     C                   ENDIF
166400000000     C*--------------------
166500020220     C* C L I E N T E
166600000000     C*--------------------
166700121217     C                   move      WFDcln        cln1K(MM)                      memorizza i flag di cliente nuovo/acq
166800000000     C                   ADD       WFDNSP        S1K(MM)                        SPEDIZIONI
166900081013     C                   eval      Kg1K(MM) = wfdPKG + Kg1K(MM)                 PESI in KG
167000000000     C                   ADD       WFDRRN        R1K(MM)                        RICAVI REALI DELTA
167100000000     C                   ADD       WFDRPN        P1K(MM)                        RICAVI PRESUNTI DELT
167200091123      * -------------
167300091123      **  solo x per l'estero incasella x Network
167400091123IF  1C     PARIOE        IFEQ      'E'
167500091124IF  1C     MM            andle     12
167600091124     C     K             OCCUR     W1ntw
167700091124     c                   movel     WFDNTW        C1ntw
167800091124     C                   ADD       WFDNSP        S1ntw(MM)                        SPEDIZIONI
167900091124     C                   eval      Kg1ntw(MM) = wfdPKG + Kg1ntw(MM)                 PESI in KG
168000091124     C                   ADD       WFDRRN        R1ntw(MM)                        RICAVI REALI DELTA
168100091124     C                   ADD       WFDRPN        P1ntw(MM)                        RICAVI PRESUNTI DELT
168200091125     C                   z-add(h)  WFDRRN        R1totale         15 0
168300091125     C                   z-add(h)  WFDRPN        P1totale         15 0
168400091125     C                   ADD       R1totale      TR1ntw
168500091125     C                   ADD       P1totale      TP1ntw
168600091123     c                   END
168700091123      **
168800970822IF  2C     MM            IFNE      13                                           SOLO PERIODO CORRENT
168900020220     C                   MOVEL     WFDSPD        D1K(MM)                        FLAG % SCOSTAM.DELTA
169000070420      *
169100070420      * ?se invece è un dato dell'anno precedente
169200070420     c                   else
169300081013      *
169400070420      * ?lo devo incasellare nel mese appropriato
169500070420     C                   ADD       WFDNSP        ap_S1K(WFDmes)                 SPEDIZIONI
169600081013     C                   eval      ap_Kg1K(WFDmes) = wfdPKG + ap_Kg1K(WFDmes)   PESI in KG
169700070420     C                   ADD       WFDRRN        ap_R1K(WFDmes)                 RICAVI REALI DELTA
169800070420     C                   ADD       WFDRPN        ap_P1K(WFDmes)                 RICAVI PRESUNTI DELT
169900091123      * -------------
170000091123      **  solo x per l'estero incasella x Network
170100091123IF  1C     PARIOE        IFEQ      'E'
170200091124IF  1C     WFDmes        andle     12
170300091124     C     K             OCCUR     WAP1ntw
170400091124     c                   movel     WFDNTW        Ap_c1ntw
170500091124     C                   eval      ap_S1ntw(WFDmes) = WFDNSP + ap_S1ntw(WFDmes)    SPEDIZIONI
170600091124     C                   eval      ap_Kg1ntw(WFDmes)= wfdPKG +ap_Kg1ntw(WFDmes)      PESI in KG
170700091124     C                   eval      ap_R1ntw(WFDmes) = WFDRRN + ap_R1ntw(WFDmes)    RICAVI REALI DELT
170800091124     C                   eval      ap_P1ntw(WFDmes) = WFDRPN + ap_P1ntw(WFDmes)    RICAVI PRESUNTI D
170900091125     C                   z-add(h)  WFDRRN        R1totale         15 0
171000091125     C                   z-add(h)  WFDRPN        P1totale         15 0
171100091125     C                   ADD       R1totale      ap_TR1ntw
171200091125     C                   ADD       P1totale      ap_TP1ntw
171300091123     c                   END
171400091123      **
171500970822E   2C                   ENDIF
171600100201      * -------------
171700100204      **
171800100204     /*?  totalizza incasellando x Network e Tipo Servizio      ?
171900100204     /*?   x Italia e poi x Estero            ......e           ?
172000100204     /*?   se RICHIESTA creazione del File PC   (WFDELPCF)      ?
172100100204IF  1C                   IF        PARIOE <> *blank and PARFPC = 'S'
172200100201     c                   exsr      TOTCLI_NTW_TS
172300100201     c                   END
172400100201      * -------------
172500070420      *
172600020712     C     TIPANN        IFEQ      'P'
172700020712     C     *IN33         ANDEQ     *ON
172800020712     C                   ADD       WFDRRN        R1KP(MMP)                      RICAVI REALI DELTA A
172900020712     C                   ENDIF
173000000000     C*--------------------
173100020220     C* C O M M E R C I A L E
173200000000     C*--------------------
173300000000     C     Q             OCCUR     ABCCOM
173400020318     C     K             OCCUR     ABCCOMN
173500020318     C     K             OCCUR     ABCCOMND
173600000000     C* NUOVO
173700000000IF  2C     DESCLN        IFEQ      'Y'
173800000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
173900000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
174000020318     C                   ADD       1             CNC(MM)                        NUMERO CLIENTI
174100000830E   3C                   ENDIF
174200020319     C     MM            IFNE      13
174300020319     C                   IF        DESCLV <> 'D'
174400020319     C                   ADD       CONNTW        CNCN(MM)                       NUMERO CLIENTI
174500020318     C                   ELSE
174600020319     C                   ADD       CONNTW        CNCND(MM)                      NUMERO CLIENTI
174700020318     C                   ENDIF
174800020319     C                   ENDIF
174900000000     C                   ADD       WFDNSP        SNC(MM)                        SPEDIZIONI
175000000000     C                   ADD       WFDRRN        RNC(MM)                        RICAVI REALI DELTA
175100000000     C                   ADD       WFDRPN        PNC(MM)                        RICAVI PRESUNTI DELT
175200020318     C                   IF        DESCLV <> 'D'
175300020318     C                   ADD       WFDNSP        SNCN(MM)                       SPEDIZIONI
175400020318     C                   ADD       WFDRRN        RNCN(MM)                       RICAVI REALI DELTA
175500020318     C                   ADD       WFDRPN        PNCN(MM)                       RICAVI PRESUNTI DELT
175600020318     C                   ELSE
175700020318     C                   ADD       WFDNSP        SNCND(MM)                      SPEDIZIONI
175800020318     C                   ADD       WFDRRN        RNCND(MM)                      RICAVI REALI DELTA
175900020318     C                   ADD       WFDRPN        PNCND(MM)                      RICAVI PRESUNTI DELT
176000020318     C                   ENDIF
176100000000     C* ACQUISITO
176200000000X   2C                   ELSE
176300000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
176400000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
176500000000     C                   ADD       1             CAC(MM)                        NUMERO CLIENTI
176600000830E   3C                   ENDIF
176700020319     C     MM            IFNE      13
176800020319     C                   IF        DESCLV <> 'D'
176900020319     C                   ADD       CONNTW        CACN(MM)                       NUMERO CLIENTI
177000020318     C                   ELSE
177100020319     C                   ADD       CONNTW        CACND(MM)                      NUMERO CLIENTI
177200020318     C                   ENDIF
177300020319     C                   ENDIF
177400000000     C                   ADD       WFDNSP        SAC(MM)                        SPEDIZIONI
177500000000     C                   ADD       WFDRRN        RAC(MM)                        RICAVI REALI DELTA
177600000000     C                   ADD       WFDRPN        PAC(MM)                        RICAVI PRESUNTI DELT
177700020318     C                   IF        DESCLV <> 'D'
177800020318     C                   ADD       WFDNSP        SACN(MM)                       SPEDIZIONI
177900020318     C                   ADD       WFDRRN        RACN(MM)                       RICAVI REALI DELTA
178000020318     C                   ADD       WFDRPN        PACN(MM)                       RICAVI PRESUNTI DELT
178100020318     C                   ELSE
178200020318     C                   ADD       WFDNSP        SACND(MM)                      SPEDIZIONI
178300020318     C                   ADD       WFDRRN        RACND(MM)                      RICAVI REALI DELTA
178400020318     C                   ADD       WFDRPN        PACND(MM)                      RICAVI PRESUNTI DELT
178500020318     C                   ENDIF
178600000000E   2C                   ENDIF
178700000000     C*---
178800000000     C* TOT CLIENTI 13^ COLONNA
178900000000     C*---
179000000000     C     Q             OCCUR     ABCC13
179100020318     C     K             OCCUR     ABCC13N
179200020318     C     K             OCCUR     ABCC13ND
179300951227     C* NUOVO
179400951227IF  2C     DESCLN        IFEQ      'Y'
179500951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
179600951227     C                   ADD       CO2KSC        NTC(2)
179700020318     C                   IF        DESCLV <> 'D'
179800020318     C                   ADD       CO2KSC        NTCN(2)
179900020318     C                   ELSE
180000020318     C                   ADD       CO2KSC        NTCND(2)
180100020318     C                   ENDIF
180200951227X   3C                   ELSE                                                   *PERIODO CORRENTE
180300951227     C                   ADD       CONKSC        NTC(1)
180400020318     C                   IF        DESCLV <> 'D'
180500020318     C                   ADD       CONKSC        NTCN(1)
180600020318     C                   ELSE
180700020318     C                   ADD       CONKSC        NTCND(1)
180800020318     C                   ENDIF
180900951227E   3C                   ENDIF
181000951227     C* ACQUISITO
181100951227X   2C                   ELSE
181200951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
181300951227     C                   ADD       CO2KSC        ATC(2)
181400020318     C                   IF        DESCLV <> 'D'
181500020318     C                   ADD       CO2KSC        ATCN(2)
181600020318     C                   ELSE
181700020318     C                   ADD       CO2KSC        ATCND(2)
181800020318     C                   ENDIF
181900951227X   3C                   ELSE                                                   *PERIODO CORRENTE
182000951227     C                   ADD       CONKSC        ATC(1)
182100020318     C                   IF        DESCLV <> 'D'
182200020318     C                   ADD       CONKSC        ATCN(1)
182300020318     C                   ELSE
182400020318     C                   ADD       CONKSC        ATCND(1)
182500020318     C                   ENDIF
182600951227E   3C                   ENDIF
182700951227E   2C                   ENDIF
182800000000     C*--------------------
182900000000     C* F I L I A L E
183000000000     C*--------------------
183100000000     C     Q             OCCUR     ABCFIL
183200020318     C     K             OCCUR     ABCFILN
183300020318     C     K             OCCUR     ABCFILND
183400000000     C* NUOVO
183500000000IF  2C     DESCLN        IFEQ      'Y'
183600000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
183700000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
183800000000     C                   ADD       1             CNF(MM)                        NUMERO CLIENTI
183900000830E   3C                   ENDIF
184000020319     C     MM            IFNE      13
184100020319     C                   IF        DESCLV <> 'D'
184200020319     C                   ADD       CONNTW        CNFN(MM)                       NUMERO CLIENTI
184300020318     C                   ELSE
184400020319     C                   ADD       CONNTW        CNFND(MM)                      NUMERO CLIENTI
184500020318     C                   ENDIF
184600020319     C                   ENDIF
184700000000     C                   ADD       WFDNSP        SNF(MM)                        SPEDIZIONI
184800000000     C                   ADD       WFDRRN        RNF(MM)                        RICAVI REALI DELTA
184900000000     C                   ADD       WFDRPN        PNF(MM)                        RICAVI PRESUNTI DELT
185000020318     C                   IF        DESCLV <> 'D'
185100020318     C                   ADD       WFDNSP        SNFN(MM)                       SPEDIZIONI
185200020318     C                   ADD       WFDRRN        RNFN(MM)                       RICAVI REALI DELTA
185300020318     C                   ADD       WFDRPN        PNFN(MM)                       RICAVI PRESUNTI DELT
185400020318     C                   ELSE
185500020318     C                   ADD       WFDNSP        SNFND(MM)                      SPEDIZIONI
185600020318     C                   ADD       WFDRRN        RNFND(MM)                      RICAVI REALI DELTA
185700020318     C                   ADD       WFDRPN        PNFND(MM)                      RICAVI PRESUNTI DELT
185800020318     C                   ENDIF
185900000000     C* ACQUISITO
186000000000X   2C                   ELSE
186100000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
186200000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
186300000000     C                   ADD       1             CAF(MM)                        NUMERO CLIENTI
186400000830E   3C                   ENDIF
186500020319     C     MM            IFNE      13
186600020319     C                   IF        DESCLV <> 'D'
186700020319     C                   ADD       CONNTW        CAFN(MM)                       NUMERO CLIENTI
186800020318     C                   ELSE
186900020319     C                   ADD       CONNTW        CAFND(MM)                      NUMERO CLIENTI
187000020318     C                   ENDIF
187100020319     C                   ENDIF
187200000000     C                   ADD       WFDNSP        SAF(MM)                        SPEDIZIONI
187300000000     C                   ADD       WFDRRN        RAF(MM)                        RICAVI REALI DELTA
187400000000     C                   ADD       WFDRPN        PAF(MM)                        RICAVI PRESUNTI DELT
187500020318     C                   IF        DESCLV <> 'D'
187600020318     C                   ADD       WFDNSP        SAFN(MM)                       SPEDIZIONI
187700020318     C                   ADD       WFDRRN        RAFN(MM)                       RICAVI REALI DELTA
187800020318     C                   ADD       WFDRPN        PAFN(MM)                       RICAVI PRESUNTI DELT
187900020318     C                   ELSE
188000020318     C                   ADD       WFDNSP        SAFND(MM)                      SPEDIZIONI
188100020318     C                   ADD       WFDRRN        RAFND(MM)                      RICAVI REALI DELTA
188200020318     C                   ADD       WFDRPN        PAFND(MM)                      RICAVI PRESUNTI DELT
188300020318     C                   ENDIF
188400000000E   2C                   ENDIF
188500000000     C*---
188600000000     C* TOT CLIENTI 13^ COLONNA
188700000000     C*---
188800000000     C     Q             OCCUR     ABCF13
188900020318     C     K             OCCUR     ABCF13N
189000020318     C     K             OCCUR     ABCF13ND
189100951227IF  2C     DESCLN        IFEQ      'Y'
189200951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
189300951227     C                   ADD       CO2KSC        NTF(2)
189400020318     C                   IF        DESCLV <> 'D'
189500020319     C                   ADD       CONNTW        NTFN(2)
189600020318     C                   ELSE
189700020319     C                   ADD       CONNTW        NTFND(2)
189800020318     C                   ENDIF
189900951227X   3C                   ELSE                                                   *PERIODO CORRENTE
190000951227     C                   ADD       CONKSC        NTF(1)
190100020318     C                   IF        DESCLV <> 'D'
190200020319     C                   ADD       CONNTW        NTFN(1)
190300020318     C                   ELSE
190400020319     C                   ADD       CONNTW        NTFND(1)
190500020318     C                   ENDIF
190600951227E   3C                   ENDIF
190700030924     C* ACQUISITO
190800951227X   2C                   ELSE
190900951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
191000951227     C                   ADD       CO2KSC        ATF(2)
191100030924     C                   IF        DESCLV <> 'D'
191200030924     C                   ADD       CONNTW        ATFN(2)
191300030924     C                   ELSE
191400020319     C                   ADD       CONNTW        ATFND(2)
191500020318     C                   ENDIF
191600951227X   3C                   ELSE                                                   *PERIODO CORRENTE
191700951227     C                   ADD       CONKSC        ATF(1)
191800020318     C                   IF        DESCLV <> 'D'
191900020319     C                   ADD       CONNTW        ATFN(1)
192000020318     C                   ELSE
192100020319     C                   ADD       CONNTW        ATFND(1)
192200020318     C                   ENDIF
192300951227E   3C                   ENDIF
192400951227E   2C                   ENDIF
192500000000     C*--------------------
192600000000     C* A R E A
192700000000     C*--------------------
192800000000     C     Q             OCCUR     ABCARE
192900020318     C     K             OCCUR     ABCAREN
193000020318     C     K             OCCUR     ABCAREND
193100000000     C* NUOVO
193200000000IF  2C     DESCLN        IFEQ      'Y'
193300000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
193400000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
193500000000     C                   ADD       1             CNA(MM)                        NUMERO CLIENTI
193600000830E   3C                   ENDIF
193700020319     C     MM            IFNE      13
193800020319     C                   IF        DESCLV <> 'D'
193900020319     C                   ADD       CONNTW        CNAN(MM)                       NUMERO CLIENTI
194000020318     C                   ELSE
194100020319     C                   ADD       CONNTW        CNAND(MM)                      NUMERO CLIENTI
194200020318     C                   ENDIF
194300020319     C                   ENDIF
194400000000     C                   ADD       WFDNSP        SNA(MM)                        SPEDIZIONI
194500000000     C                   ADD       WFDRRN        RNA(MM)                        RICAVI REALI DELTA
194600000000     C                   ADD       WFDRPN        PNA(MM)                        RICAVI PRESUNTI DELT
194700020318     C                   IF        DESCLV <> 'D'
194800020318     C                   ADD       WFDNSP        SNAN(MM)                       SPEDIZIONI
194900020318     C                   ADD       WFDRRN        RNAN(MM)                       RICAVI REALI DELTA
195000020318     C                   ADD       WFDRPN        PNAN(MM)                       RICAVI PRESUNTI DELT
195100020318     C                   ELSE
195200020318     C                   ADD       WFDNSP        SNAND(MM)                      SPEDIZIONI
195300020318     C                   ADD       WFDRRN        RNAND(MM)                      RICAVI REALI DELTA
195400020318     C                   ADD       WFDRPN        PNAND(MM)                      RICAVI PRESUNTI DELT
195500020318     C                   ENDIF
195600000000     C* ACQUISITO
195700000000X   2C                   ELSE
195800000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
195900000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
196000000000     C                   ADD       1             CAA(MM)                        NUMERO CLIENTI
196100000830E   3C                   ENDIF
196200020319     C     MM            IFNE      13
196300020319     C                   IF        DESCLV <> 'D'
196400020319     C                   ADD       CONNTW        CAAN(MM)                       NUMERO CLIENTI
196500020318     C                   ELSE
196600020319     C                   ADD       CONNTW        CAAND(MM)                      NUMERO CLIENTI
196700020318     C                   ENDIF
196800020319     C                   ENDIF
196900000000     C                   ADD       WFDNSP        SAA(MM)                        SPEDIZIONI
197000000000     C                   ADD       WFDRRN        RAA(MM)                        RICAVI REALI DELTA
197100000000     C                   ADD       WFDRPN        PAA(MM)                        RICAVI PRESUNTI DELT
197200020318     C                   IF        DESCLV <> 'D'
197300020318     C                   ADD       WFDNSP        SAAN(MM)                       SPEDIZIONI
197400020318     C                   ADD       WFDRRN        RAAN(MM)                       RICAVI REALI DELTA
197500020318     C                   ADD       WFDRPN        PAAN(MM)                       RICAVI PRESUNTI DELT
197600020318     C                   ELSE
197700020318     C                   ADD       WFDNSP        SAAND(MM)                      SPEDIZIONI
197800020318     C                   ADD       WFDRRN        RAAND(MM)                      RICAVI REALI DELTA
197900020318     C                   ADD       WFDRPN        PAAND(MM)                      RICAVI PRESUNTI DELT
198000020318     C                   ENDIF
198100000000E   2C                   ENDIF
198200000000     C*---
198300000000     C* TOT CLIENTI 13^ COLONNA
198400000000     C*---
198500000000     C     Q             OCCUR     ABCA13
198600020318     C     K             OCCUR     ABCA13N
198700020318     C     K             OCCUR     ABCA13ND
198800951227IF  2C     DESCLN        IFEQ      'Y'
198900951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
199000951227     C                   ADD       CO2KSC        NTA(2)
199100020318     C                   IF        DESCLV <> 'D'
199200020319     C                   ADD       CONNTW        NTAN(2)
199300020318     C                   ELSE
199400020319     C                   ADD       CONNTW        NTAND(2)
199500020318     C                   ENDIF
199600951227X   3C                   ELSE                                                   *PERIODO CORRENTE
199700951227     C                   ADD       CONKSC        NTA(1)
199800020318     C                   IF        DESCLV <> 'D'
199900020319     C                   ADD       CONNTW        NTAN(1)
200000020318     C                   ELSE
200100020319     C                   ADD       CONNTW        NTAND(1)
200200020318     C                   ENDIF
200300951227E   3C                   ENDIF
200400951227     C* ACQUISITO
200500951227X   2C                   ELSE
200600951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
200700951227     C                   ADD       CO2KSC        ATA(2)
200800020318     C                   IF        DESCLV <> 'D'
200900020319     C                   ADD       CONNTW        ATAN(2)
201000020318     C                   ELSE
201100020319     C                   ADD       CONNTW        ATAND(2)
201200020318     C                   ENDIF
201300951227X   3C                   ELSE                                                   *PERIODO CORRENTE
201400951227     C                   ADD       CONKSC        ATA(1)
201500020318     C                   IF        DESCLV <> 'D'
201600020319     C                   ADD       CONNTW        ATAN(1)
201700020318     C                   ELSE
201800020319     C                   ADD       CONNTW        ATAND(1)
201900020318     C                   ENDIF
202000951227E   3C                   ENDIF
202100951227E   2C                   ENDIF
202200000000     C*--------------------
202300000000     C* D I V I S I O N E
202400000000     C*--------------------
202500000000     C     Q             OCCUR     ABCDIV
202600020318     C     K             OCCUR     ABCDIVN
202700020318     C     K             OCCUR     ABCDIVND
202800000000     C* NUOVO
202900000000IF  2C     DESCLN        IFEQ      'Y'
203000000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
203100000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
203200000000     C                   ADD       1             CND(MM)                        NUMERO CLIENTI
203300000830E   3C                   ENDIF
203400020319     C     MM            IFNE      13
203500020319     C                   IF        DESCLV <> 'D'
203600020319     C                   ADD       CONNTW        CNDN(MM)                       NUMERO CLIENTI
203700020318     C                   ELSE
203800020319     C                   ADD       CONNTW        CNDND(MM)                      NUMERO CLIENTI
203900020318     C                   ENDIF
204000020319     C                   ENDIF
204100000000     C                   ADD       WFDNSP        SND(MM)                        SPEDIZIONI
204200000000     C                   ADD       WFDRRN        RND(MM)                        RICAVI REALI DELTA
204300000000     C                   ADD       WFDRPN        PND(MM)                        RICAVI PRESUNTI DELT
204400020318     C                   IF        DESCLV <> 'D'
204500020318     C                   ADD       WFDNSP        SNDN(MM)                       SPEDIZIONI
204600020318     C                   ADD       WFDRRN        RNDN(MM)                       RICAVI REALI DELTA
204700020318     C                   ADD       WFDRPN        PNDN(MM)                       RICAVI PRESUNTI DELT
204800020318     C                   ELSE
204900020318     C                   ADD       WFDNSP        SNDND(MM)                      SPEDIZIONI
205000020318     C                   ADD       WFDRRN        RNDND(MM)                      RICAVI REALI DELTA
205100020318     C                   ADD       WFDRPN        PNDND(MM)                      RICAVI PRESUNTI DELT
205200020318     C                   ENDIF
205300000000     C* ACQUISITO
205400000000X   2C                   ELSE
205500000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
205600000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
205700000000     C                   ADD       1             CAD(MM)                        NUMERO CLIENTI
205800000830E   3C                   ENDIF
205900020319     C     MM            IFNE      13
206000020319     C                   IF        DESCLV <> 'D'
206100020319     C                   ADD       CONNTW        CADN(MM)                       NUMERO CLIENTI
206200020318     C                   ELSE
206300020319     C                   ADD       CONNTW        CADND(MM)                      NUMERO CLIENTI
206400020318     C                   ENDIF
206500020319     C                   ENDIF
206600000000     C                   ADD       WFDNSP        SAD(MM)                        SPEDIZIONI
206700000000     C                   ADD       WFDRRN        RAD(MM)                        RICAVI REALI DELTA
206800000000     C                   ADD       WFDRPN        PAD(MM)                        RICAVI PRESUNTI DELT
206900020318     C                   IF        DESCLV <> 'D'
207000020318     C                   ADD       WFDNSP        SADN(MM)                       SPEDIZIONI
207100020318     C                   ADD       WFDRRN        RADN(MM)                       RICAVI REALI DELTA
207200020318     C                   ADD       WFDRPN        PADN(MM)                       RICAVI PRESUNTI DELT
207300020318     C                   ELSE
207400020318     C                   ADD       WFDNSP        SADND(MM)                      SPEDIZIONI
207500020318     C                   ADD       WFDRRN        RADND(MM)                      RICAVI REALI DELTA
207600020318     C                   ADD       WFDRPN        PADND(MM)                      RICAVI PRESUNTI DELT
207700020318     C                   ENDIF
207800000000E   2C                   ENDIF
207900000000     C*---
208000000000     C* TOT CLIENTI 13^ COLONNA
208100000000     C*---
208200000000     C     Q             OCCUR     ABCD13
208300020318     C     K             OCCUR     ABCD13N
208400020318     C     K             OCCUR     ABCD13ND
208500951227     C* NUOVO
208600951227IF  2C     DESCLN        IFEQ      'Y'
208700951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
208800951227     C                   ADD       CO2KSC        NTD(2)
208900020318     C                   IF        DESCLV <> 'D'
209000020319     C                   ADD       CONNTW        NTDN(2)
209100020318     C                   ELSE
209200020319     C                   ADD       CONNTW        NTDND(2)
209300020318     C                   ENDIF
209400030924X   3C                   ELSE                                                   *PERIODO CORRENTE
209500030924     C                   ADD       CONKSC        NTD(1)
209600030924     C                   IF        DESCLV <> 'D'
209700030924     C                   ADD       CONNTW        NTDN(1)
209800030924     C                   ELSE
209900030924     C                   ADD       CONNTW        NTDND(1)
210000020318     C                   ENDIF
210100951227E   3C                   ENDIF
210200951227     C* ACQUISITO
210300951227X   2C                   ELSE
210400951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
210500951227     C                   ADD       CO2KSC        ATD(2)
210600020318     C                   IF        DESCLV <> 'D'
210700020319     C                   ADD       CONNTW        ATDN(2)
210800020318     C                   ELSE
210900020319     C                   ADD       CONNTW        ATDND(2)
211000020318     C                   ENDIF
211100951227X   3C                   ELSE                                                   *PERIODO CORRENTE
211200951227     C                   ADD       CONKSC        ATD(1)
211300020318     C                   IF        DESCLV <> 'D'
211400020319     C                   ADD       CONNTW        ATDN(1)
211500020318     C                   ELSE
211600020319     C                   ADD       CONNTW        ATDND(1)
211700020318     C                   ENDIF
211800951227E   3C                   ENDIF
211900951227E   2C                   ENDIF
212000000000     C*--------------------
212100000000     C* G E N E R A L E
212200000000     C*--------------------
212300000000     C     Q             OCCUR     ABCGEN
212400020318     C     K             OCCUR     ABCGENN
212500020318     C     K             OCCUR     ABCGENND
212600000000     C* NUOVO
212700000000IF  2C     DESCLN        IFEQ      'Y'
212800000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
212900000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
213000000830     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
213100000000     C                   ADD       1             CNG(MM)                        NUMERO CLIENTI
213200000830E   3C                   ENDIF
213300020319     C     MM            IFNE      13
213400020319     C                   IF        DESCLV <> 'D'
213500020319     C                   ADD       CONNTW        CNGN(MM)                       NUMERO CLIENTI
213600020318     C                   ELSE
213700020319     C                   ADD       CONNTW        CNGND(MM)                      NUMERO CLIENTI
213800020318     C                   ENDIF
213900020319     C                   ENDIF
214000000000     C                   ADD       WFDNSP        SNG(MM)                        SPEDIZIONI
214100000000     C                   ADD       WFDRRN        RNG(MM)                        RICAVI REALI DELTA
214200000000     C                   ADD       WFDRPN        PNG(MM)                        RICAVI PRESUNTI DELT
214300020318     C                   IF        DESCLV <> 'D'
214400020318     C                   ADD       WFDNSP        SNGN(MM)                       SPEDIZIONI
214500020318     C                   ADD       WFDRRN        RNGN(MM)                       RICAVI REALI DELTA
214600020318     C                   ADD       WFDRPN        PNGN(MM)                       RICAVI PRESUNTI DELT
214700020318     C                   ELSE
214800020318     C                   ADD       WFDNSP        SNGND(MM)                      SPEDIZIONI
214900020318     C                   ADD       WFDRRN        RNGND(MM)                      RICAVI REALI DELTA
215000020318     C                   ADD       WFDRPN        PNGND(MM)                      RICAVI PRESUNTI DELT
215100020318     C                   ENDIF
215200000000     C* ACQUISITO
215300000000X   2C                   ELSE
215400000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
215500000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
215600000830     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
215700000000     C                   ADD       1             CAG(MM)                        NUMERO CLIENTI
215800000830E   3C                   ENDIF
215900020319     C     MM            IFNE      13
216000020319     C                   IF        DESCLV <> 'D'
216100020319     C                   ADD       CONNTW        CAGN(MM)                       NUMERO CLIENTI
216200020318     C                   ELSE
216300020319     C                   ADD       CONNTW        CAGND(MM)                      NUMERO CLIENTI
216400020318     C                   ENDIF
216500020319     C                   ENDIF
216600000000     C                   ADD       WFDNSP        SAG(MM)                        SPEDIZIONI
216700000000     C                   ADD       WFDRRN        RAG(MM)                        RICAVI REALI DELTA
216800000000     C                   ADD       WFDRPN        PAG(MM)                        RICAVI PRESUNTI DELT
216900020318     C                   IF        DESCLV <> 'D'
217000020318     C                   ADD       WFDNSP        SAGN(MM)                       SPEDIZIONI
217100020318     C                   ADD       WFDRRN        RAGN(MM)                       RICAVI REALI DELTA
217200020318     C                   ADD       WFDRPN        PAGN(MM)                       RICAVI PRESUNTI DELT
217300020318     C                   ELSE
217400020318     C                   ADD       WFDNSP        SAGND(MM)                      SPEDIZIONI
217500020318     C                   ADD       WFDRRN        RAGND(MM)                      RICAVI REALI DELTA
217600020318     C                   ADD       WFDRPN        PAGND(MM)                      RICAVI PRESUNTI DELT
217700020318     C                   ENDIF
217800000000E   2C                   ENDIF
217900000000     C*---
218000000000     C* TOT CLIENTI 13^ COLONNA
218100000000     C*---
218200000000     C     Q             OCCUR     ABCG13
218300020318     C     K             OCCUR     ABCG13N
218400020318     C     K             OCCUR     ABCG13ND
218500000000     C* NUOVO
218600000000IF  2C     DESCLN        IFEQ      'Y'
218700951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
218800951227     C                   ADD       CO2KSC        NTG(2)
218900020318     C                   IF        DESCLV <> 'D'
219000020319     C                   ADD       CONNTW        NTGN(2)
219100020318     C                   ELSE
219200020319     C                   ADD       CONNTW        NTGND(2)
219300020318     C                   ENDIF
219400951227X   3C                   ELSE                                                   *PERIODO CORRENTE
219500951227     C                   ADD       CONKSC        NTG(1)
219600020318     C                   IF        DESCLV <> 'D'
219700020319     C                   ADD       CONNTW        NTGN(1)
219800020318     C                   ELSE
219900020319     C                   ADD       CONNTW        NTGND(1)
220000020318     C                   ENDIF
220100951227E   3C                   ENDIF
220200000000     C* ACQUISITO
220300000000X   2C                   ELSE
220400951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
220500951227     C                   ADD       CO2KSC        ATG(2)
220600020318     C                   IF        DESCLV <> 'D'
220700020319     C                   ADD       CONNTW        ATGN(2)
220800020318     C                   ELSE
220900020319     C                   ADD       CONNTW        ATGND(2)
221000020318     C                   ENDIF
221100951227X   3C                   ELSE                                                   *PERIODO CORRENTE
221200951227     C                   ADD       CONKSC        ATG(1)
221300020318     C                   IF        DESCLV <> 'D'
221400020319     C                   ADD       CONNTW        ATGN(1)
221500020318     C                   ELSE
221600020319     C                   ADD       CONNTW        ATGND(1)
221700020318     C                   ENDIF
221800951227E   3C                   ENDIF
221900000000E   2C                   ENDIF
222000000000     C*
222100020318E   1C                   ENDIF
222200000000     C*
222300000000     C                   ENDSR
222400000000     C*------------------------------------------------------------------------*
222500100204     /*?  TOTCLI_NTW_TS  somma sulle Ds multiple x NTW e Tipo servizio        ?
222600000000     C*------------------------------------------------------------------------*
222700100201     C     TOTCLI_NTW_TS BEGSR
222800100201      **
222900100201     c                   clear                   rec_Ntw_TSv_OK    1
223000100201      **
223100100201      **  Controlla se il record è da prendere in considerazione
223200100201IF  1C                   IF        WFDNTW = *blank
223300100202     c                   eval             rec_Ntw_TSv_OK = 'S'
223400100201     c                   movel(p)  'ITA'         cod_network       3
223500100201IF  1C                   ELSE
223600100202     c                   eval             rec_Ntw_TSv_OK = 'S'
223700100201     c                   movel(p)  WFDNTW        cod_network
223800100201     c                   END
223900100201     c**
224000100201     c                   eval      numElem = %Elem(W1ntwTS)
224100100201     c**
224200100201      **  Quindi se da totalizzare cerca su quale occurence deve metterlo.
224300160509     c                   do        numElem       kk                3 0
224400160509     C     Kk            OCCUR     W1ntwTS
224500100202      *
224600100202     ** inserisce se non era stato ancora inserito
224700100202     c                   if         c1ntwTS = *BLANK and c1tpsTS = *BLANK
224800100202     c                   eval       c1ntwTS = cod_network
224900100202     c                   eval       c1tpsTS = wfdTSP
225000100202     c                   end
225100100202      *
225200100202     c** Solo se corrisponde la chiave NTW + TServ.
225300100202     c                   IF         c1ntwTS = cod_network and c1tpsTS = wfdTSP
225400100201     c**
225500100201IF  1C     MM            ifLE      12
225600100202     c**
225700100202     ** aggiunge x l'anno in corso
225800100201     C                   ADD       WFDNSP        S1ntwTS(MM)                        SPEDIZIONI
225900100201     C                   eval      Kg1ntwTS(MM) = wfdPKG + Kg1ntwTS(MM)             PESI in KG
226000100201     C                   ADD       WFDRRN        R1ntwTS(MM)                        RICAVI REALI DELTA
226100100201     C                   ADD       WFDRPN        P1ntwTS(MM)                        RICAVI PRESUNTI DELT
226200100201     C                   z-add(h)  WFDRRN        R1totale
226300100201     C                   z-add(h)  WFDRPN        P1totale
226400100201     C                   ADD       R1totale      TR1ntwTS
226500100201     C                   ADD       P1totale      TP1ntwTS
226600100201     c                   leave
226700100201      **
226800100201     c                   else
226900100201      **
227000100201IF  1C     WFDmes        ifLE      12
227100100202     ** aggiunge x l'anno precedente
227200100201     C                   eval      ap_S1ntwTS(WFDmes) = WFDNSP +
227300100201     C                                ap_S1ntwTS(WFDmes)                              SPEDIZIONI
227400100201     C                   eval      ap_Kg1ntwTS(WFDmes)= wfdPKG +
227500100201     C                                ap_Kg1ntwTS(WFDmes)                             PESI in KG
227600100201     C                   eval      ap_R1ntwTS(WFDmes) = WFDRRN +
227700100201     C                                ap_R1ntwTS(WFDmes)                           RICAVI REALI DELT
227800100201     C                   eval      ap_P1ntwTS(WFDmes) = WFDRPN +
227900100218     C                                ap_P1ntwTS(WFDmes)                           RICAVI PRESUNTI D
228000100201     C                   z-add(h)  WFDRRN        R1totale
228100100201     C                   z-add(h)  WFDRPN        P1totale
228200100201     C                   ADD       R1totale      ap_TR1ntwTS
228300100201     C                   ADD       P1totale      ap_TP1ntwTS
228400100201     c                   leave
228500100201     c                   end
228600100201      **
228700100201     c                   end
228800100202     c                   end
228900100202      **
229000100202     C                   ENDdo
229100100202     c**
229200100201     C                   ENDSR
229300100201     C*------------------------------------------------------------------------*
229400100201     C*?STAKSC - IMPOSTA STAMPA VALORI CLIENTE
229500100201     C*------------------------------------------------------------------------*
229600100201     C     STAKSC        BEGSR
229700100201     C*
229800020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
229900020222     C                   EVAL      DEPSTPD = '0'
230000000000     C*
230100000000     C* STAMPA I VALORI DEL CLIENTE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
230200000000     C* LA STATISTICA DELTA CON DETTAGLIO CLIENTE
230300000000IF  1C     PASKSC        IFEQ      '1'
230400000000     C     PARSTA        ANDEQ     'K'
230500000000     C                   MOVEL     '1'           ORAKSC                         *ORA STAMPA CLIENTE
230600000000     C***
230700000000     C* DESCRIZIONI INTESTAZIONE
230800000000     C***
230900020116     C                   CLEAR                   SE94IN
231000000000     C                   Z-ADD     DEPAGE        INCAGE                         *AGENTE
231100000000     C                   MOVEL     DESAGE        INDAGE                         *DESCRIZIONE AGENTE
231200000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
231300000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
231400000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
231500000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
231600000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
231700000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
231800950301     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
231900000000     C                   MOVEL     DRI(20)       PIDDES                         *RETTANGOLINO
232000020221     C                   MOVEL     *BLANKS       P1DFLD                         *RETTANGOLINO
232100030311     C*
232200030311     C* PREPARA BUFFER FILE PC (WFDELPCF)
232300030311     C                   EXSR      WRIFPC
232400000000     C*
232500000000     C* STAMPA I VALORI DEL CLIENTE
232600020221     C                   EXSR      STMKSC
232700000000     C*
232800000000E   1C                   ENDIF
232900000000     C*
233000000000     C                   ENDSR
233100030311     C*------------------------------------------------------------------------*
233200030311     C*?WRIFPC - IMPOSTA BUFFER FILE PC (WFDELPCF)
233300030311     C*------------------------------------------------------------------------*
233400030311     C     WRIFPC        BEGSR
233500030311     C*
233600100511     C**------------>    CLEAR                   WFDELPC0
233700030311     C*
233800100406     C*********>>>       Z-ADD     INCAGE        WFPAGE                         *AGENTE
233900100406     C*********>>>       MOVEL     INDAGE        WFPAGD                         *DESCRIZIONE AGENTE
234000100406     C*********>>>       Z-ADD     INCFIL        WFPFIL                         *FILIALE
234100100406     C*********>>>       MOVEL     INDFIL        WFPFID                         *DESCRIZIONE FILIALE
234200100406     C*********>>>       Z-ADD     INCARE        WFPARE                         *AREA
234300100406     C*********>>>       MOVEL     INDARE        WFPARD                         *DESCRIZIONE AREA
234400100406     C*********>>>       MOVEL     INCDIV        WFPDIV                         *DISTRETTO
234500100406     C*********>>>       MOVEL     INDDIV        WFPDID                         *DESCRIZIONE DISTRET
234600100406     C*********>>>       Z-ADD     PARAAI        WFPAAD                         *ANNO DA
234700100406     C*********>>>       Z-ADD     PARMMI        WFPMMD                         *MESE DA
234800100406     C*********>>>       Z-ADD     PARAAF        WFPAAA                         *ANNO A
234900100406     C*********>>>       Z-ADD     PARMMF        WFPMMA                         *MESE A
235000100406     C*********>>>       Z-ADD     DEPCLI        WFPCLI                         *CODICE CLIENTE
235100100406     C*********>>>       MOVEL     DESKSC        WFPCLD                         *DESCRIZIONE CLIENTE
235200100406     C*********>>>       MOVEL     DESCLV        WFPCLV                         *CODICE IMPORTANZA C
235300100406     C*********>>>       MOVEL     DESITC        WFPITC                         *CATEGORIA CLIENTE
235400100406     C                   Z-ADD     INCAGE        aaaAGE            7 0          *AGENTE
235500100406     C                   MOVEL(p)  INDAGE        aaaAGD           25            *DESCRIZIONE AGENTE
235600100406     C                   Z-ADD     INCFIL        aaaFIL            3 0          *FILIALE
235700100406     C                   MOVEL(p)  INDFIL        aaaFID           25            *DESCRIZIONE FILIALE
235800100406     C                   Z-ADD     INCARE        aaaARE            3 0          *AREA
235900100406     C                   MOVEL(p)  INDARE        aaaARD           25            *DESCRIZIONE AREA
236000100406     C                   MOVEL(p)  INCDIV        aaaDIV            1            *DISTRETTO
236100100406     C                   MOVEL(p)  INDDIV        aaaDID           25            *DESCRIZIONE DISTRET
236200050223     C*******            MOVEL     INDIOE        WFPIOE                         *DELTA ITALIA/ESTERO
236300100406     C*********>>>       MOVEL     parIOE        WFPIOE                         *DELTA ITALIA/ESTERO
236400100406     C                   MOVEL(p)  parIOE        aaaIOE            1            *DELTA ITALIA/ESTERO
236500100406     C                   Z-ADD     PARAAI        aaaAAD            4 0          *ANNO DA
236600100406     C                   Z-ADD     PARMMI        aaaMMD            2 0          *MESE DA
236700100406     C                   Z-ADD     PARAAF        aaaAAA            4 0          *ANNO A
236800100406     C                   Z-ADD     PARMMF        aaaMMA            2 0          *MESE A
236900100406     C                   Z-ADD     DEPCLI        aaaCLI            7 0          *CODICE CLIENTE
237000100406     C                   MOVEL(p)  DESKSC        aaaCLD           25            *DESCRIZIONE CLIENTE
237100100406     C                   MOVEL(p)  DESCLV        aaaCLV            1            *CODICE IMPORTANZA C
237200100406     C                   MOVEL(p)  DESITC        aaaITC           25            *CATEGORIA CLIENTE
237300070924     C*****
237400070924     C*****              MOVEL     DES09         WFPCLN                         *CLIENTE NUOVO/ACQUI
237500100406     C*********>>>       MOVEL     'A'           WFPCLN
237600100406     C                   MOVEL(p)  'A'           aaaCLN            1
237700070924IF  1C     DESCLN        IFne      *blank                                       *CL.NUOVO SE "Y" o "
237800100406     C*********>>>       MOVEL     'N'           WFPCLN
237900100406     C                   MOVEL     'N'           aaaCLN
238000070924E   1C                   ENDIF
238100030311     C*
238200030311     C                   ENDSR
238300000000     C*------------------------------------------------------------------------*
238400000000     C*?STMKSC - STAMPA I VALORI DEL CLIENTE
238500000000     C*------------------------------------------------------------------------*
238600000000     C     STMKSC        BEGSR
238700061220     C*
238800061220     C* Per controllare se il cliente da stampare è anagraficamente Bloccato
238900061220     C*   nella totalità ossia se è uniuficante di se stesso oppure se ha
239000061220     C*   tutti i codici figli oltre se stesso Bloccati.
239100061220     C                   CLEAR                   Total_Bloccato    1
239200061220     C*
239300000000     C*--------------------
239400000000     C* I N T E S T A Z I O N E
239500000000     C*--------------------
239600000000IF  1C     *IN01         IFEQ      *ON
239700000000IF  2C     PRIGIR        IFEQ      '0'
239800000000     C                   MOVEL     '1'           PRIGIR
239900000000X   2C                   ELSE
240000100129     c                   if        parSTAMPA <> 'N'
240100020116     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
240200100129     c                   end
240300000000E   2C                   ENDIF
240400020222     C*
240500020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
240600020222     C                   IF        FLGSTPLE = '1'
240700100129     c                   if        parSTAMPA <> 'N'
240800020222     C                   WRITE     SE94LE                                       *LEGENDA
240900100129     c                   end
241000020222     C                   ELSE
241100020222     C                   EVAL      FLGSTPLE = '1'
241200020222     C                   ENDIF
241300020222     C*
241400100129     c                   if        parSTAMPA <> 'N'
241500020222     C                   WRITE     SE94PT                                       *TESTATA
241600020116     C                   WRITE     SE94IN                                       *INTESTAZIONE
241700020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
241800020116     C                   WRITE     SE94PI                                       *RIGA MESI
241900020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
242000100129     c                   end
242100000000     C                   SETOFF                                       01
242200000000E   1C                   ENDIF
242300020222     C*--------------------
242400000000     C* S P E D I Z I O N I
242500000000     C*--------------------
242600000000     C                   Z-ADD     S1K           $VA
242700020116     C                   Z-ADD     *ZEROS        $VA13C           19 5          *PERIODO CORRENTE
242800000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
242900000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
243000000000E   1C                   ENDDO
243100020116     C                   Z-ADD     *ZEROS        $V213C           19 5          *PERIODO PRECEDENTE
243200951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
243300000000     C***
243400000000     C* CODICE + RAGIONE SOCIALE
243500000000     C***
243600020221     C*
243700000825     C                   MOVEL     DEPCLI        DES07             7            *CODICE CLIENTE
243800000000     C                   MOVEL     DESKSC        DES12            12            *RAGIONE SOCIALE
243900000000     C*
244000000000     C                   MOVEL     *BLANKS       DES20
244100000000     C                   MOVEL     DES07         DES20            20
244200000000     C                   MOVE      DES12         DES20
244300000000     C*
244400000000     C                   MOVEL     *BLANKS       P1DDES
244500000000     C                   MOVEL     DES20         P1DDES
244600000000     C                   MOVE      'SPE'         P1DDES
244700020222     C                   MOVEL     *BLANKS       P1DFLD
244800000000     C*
244900000000     C* SCRIVE I VALORI IN STAMPA
245000000000     C                   EXSR      SCRVAL
245100030311     C*
245200030311     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
245300100406     C*********>>>       EVAL      WFPSAP = P1CVAP
245400100406     C*********>>>       EVAL      WFPSAC = P1CVAT
245500100406     c                   z-add     P1CVAP        aaaSAP           11 0
245600100406     c                   z-add     P1CVAT        aaaSAC           11 0
245700000000     C*--------------------
245800000000     C* R I C A V I
245900000000     C*--------------------
246000020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
246100020116     C*    R1K           DIV       1000          $VA
246200020116     C                   Z-ADD     R1K           $VA
246300020116     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
246400000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
246500020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
246600000000E   1C                   ENDDO
246700020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
246800951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
246900000000     C***
247000000000     C* CLIENTE NUOVO/ACQUISITO + DI TIPO A/B/C
247100000000     C***
247200070822      *?Se il cliente ha spedizioni sia com Italia che come estero ed è NUOVO,
247300070822      *?potrebbe accadere che il mese con la "Y" abbia solo un tipo di spedizioni,
247400070822      *?quindi, se facessimo il test sulla "Y" in una stampa risulterebbe Nuovo e
247500070822      *?nell'altra invece com Acquisito. OCCORRE testare semplicemente se <> da Blk.
247600070822IF  1C**** DESCLN        IFEQ      'Y'                                          *CL.NUOVO SOLO SE "Y
247700070822IF  1C     DESCLN        IFne      *blank                                       *CL.NUOVO SE "Y" o "
247800000000     C                   MOVEL     DRI(17)       DES09             9
247900000000X   1C                   ELSE
248000000000     C                   MOVEL     DRI(18)       DES09             9
248100000000E   1C                   ENDIF
248200000829     C                   EVAL      DES20 =  DES09 +
248300000829     C                                      ' / ' +
248400000829     C                                      DESCLV
248500000000     C                   MOVEL     *BLANKS       P1DDES
248600000000     C                   MOVEL     DES20         P1DDES
248700000000     C                   MOVE      'RIC'         P1DDES
248800020222     C                   MOVEL     *BLANKS       P1DFLD
248900000000     C*
249000000000     C* SCRIVE I VALORI IN STAMPA
249100000000     C                   EXSR      SCRVAL
249200030311     C*
249300030311     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
249400100406     C*********>>>       EVAL      WFPFAP = P1CVAP
249500100406     C*********>>>       EVAL      WFPFAC = P1CVAT
249600100406     c                   z-add     P1CVAP        aaaFAP           15 3
249700100406     c                   z-add     P1CVAT        aaaFAC           15 3
249800000000     C*--------------------
249900000000     C* D E L T A
250000000000     C*--------------------
250100000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
250200000000     C                   Z-ADD     R1K           $RR                            *RICAVI REALI
250300000000     C                   Z-ADD     P1K           $RP                            *RICAVI PRESUNTI
250400000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
250500000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
250600000000     C                   EXSR      IMPCAL
250700970822     C***
250800970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
250900970822     C***
251000970822     C                   MOVEL     D1K           $SD                            *FALG SEGNALAZIONE
251100000000     C***
251200951006     C* CATEGORIA SEAT
251300000000     C***
251400000000     C                   MOVEL     *BLANKS       DES20
251500951006     C                   MOVEL     DESITC        DES20            20
251600000000     C*
251700000000     C                   MOVEL     *BLANKS       P2DDES
251800020221     C                   MOVEL     *BLANKS       P2DFLD
251900000000     C                   MOVEL     DES20         P2DDES
252000000000     C                   MOVE      'DEL'         P2DDES
252100020222     C                   MOVEL     *BLANKS       P2DFLD
252200020701     C*
252300020701     C* VERIFICO LO SCASTAMENTO % RISPETTO ALLO STESSO MESE/PERIODO DELL?ANNO PRECEDENTE
252400020701     C                   EXSR      REPDELFAT
252500000000     C*
252600000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
252700000000     C                   EXSR      SCRDEL
252800030924     C*
252900030924     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
253000100406     C*********>>>       EVAL      WFPDAP = P2CDEP
253100100406     C*********>>>       EVAL      WFPDAC = P2CDET
253200100406     c                   z-add     P2CDEP        aaaDAP            4 1
253300100406     c                   z-add     P2CDET        aaaDAC            4 1
253400030924     C*
253500030924     C* INIZIALIZZO LA SCHIERA DI WRK X SCOSTAMENTO FATTURATO ED I RELATIVI CAMPI DI STAMPA
253600030924     C                   EXSR      INZPDF
253700980406     C*--------------------
253800000828     C* DESCRIZIONE BLOCCO + CLIENTI FIGLI
253900980406     C*--------------------
254000000828     C                   MOVEL     'N'           $STRIG
254100020116     C                   CLEAR                   SE94P3
254200130318IF  1C     DESABL        IFne      *blank
254300130318IF  1C     DESABL        andne     '*'
254400980406     C     DESCBL        ORNE      *BLANKS
254500980406     C                   MOVEL     DESCBL        P3DDES
254600000828     C                   MOVEL     'S'           $STRIG                         *RIGA DA STAMPARE
254700061220     C                   movel     'S'           Total_Bloccato
254800980406E   1C                   ENDIF
254900000828     C*
255000000829     C                   Z-ADD     *ZEROS        BLOCCO            2 0          *BLOCCO CL STAMPATI
255100000828IF  1C     PARSCE        IFEQ      'U'                                          *STAT. UNIFICANTE
255200000828IF  2C     SKSC11(2)     IFGT      *ZEROS                                       *ESISTONO FIGLI
255300000828     C                   Z-ADD     2             I
255400000828DO  3C     I             DOWLE     500
255500000828     C     SKSC11(I)     ANDGT     *ZEROS
255600000828     C                   Z-ADD     *ZEROS        IS                             *N.CLIENTI X RIGA
255700000828     C                   CLEAR                   SKSC
255800051031     C                   clear                   skbl
255900051031      *
256000000828DO  4C     I             DOWLE     500
256100000828     C     SKSC11(I)     ANDGT     *ZEROS
256200051031     C**** ridotta da 19 a 17 per far spazio ai flags x individuare i codici bloccati
256300051031     C**** IS            ANDLT     19
256400051031     C     IS            ANDLT     17
256500000828     C                   ADD       1             IS                             *CLIENTI NELLA RIGA
256600000828     C                   Z-ADD     SKSC11(I)     SKSC(IS)
256700051031     C*--------------------
256800051031     C* controllo codice cliente bloccato
256900051031     C*--------------------
257000051031     C                   CLEAR                   BS69DS
257100051031     C                   CLEAR                   ACODS
257200051031     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
257300051031     C                   MOVEL     SKSC(IS)      I69KAC                         *CLIENTE X CNACO
257400120125     C                   MOVEL     SKSC(IS)      I69KCP                         *CLIENTE X CNCLP
257500051031     C                   CALL      'TIBS69R'
257600051031     C                   PARM                    BS69DS
257700051031     C                   PARM                    ACODS
257800051031     C                   PARM                    INDDS
257900051031     C                   PARM                    CLPDS
258000051031     C                   PARM                    CLSDS
258100051031IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
258200130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
258300130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
258400051031     C                   move      '+'           skbl(is)
258500120125      **
258600120125      **  Reperisce la Data di quando è stato fatto il Blocco Contabile sul cliente
258700120125     C                   clear                   TrulBLCds
258800120125     c                   eval      IblcTLA = 'L'
258900120125     c                   z-add     acoKcC        IblcKCC
259000120125     c                   z-add     acoKsC        IblcKSC
259100120125     c                   call      'TRULBLCR'
259200120125     C                   PARM                    KPJBA
259300120125     C                   PARM                    TrulBLCds
259400120125      **
259500120125      **  deve memorizzare il più recente blocco effettuato
259600120125      **   codice e data blocco dell'ultimo cliente bloccato fra quelli unificati.
259700120125     c                   if         Data_Blocco_Ultimo < OblcDAV
259800120125     c                   eval           Data_Blocco_Ultimo  = OblcDAV
259900120125     c                   eval           Cod_Blocco_Ultimo   = clpNAR
260000120125     c                   end
260100120125      **
260200051031E   1C                   ENDIF
260300120125      ***
260400120131      ***  riporta le date da 6 a 8 per effettuare il confronto
260500120131     c                   eval      Data_Prima_Fat_8 = 0
260600120131     c                   eval       CLP_Prima_Fat_8 = 0
260700120131      *
260800120131     c                   clear                   Anno2_0           2 0
260900120131     c                   IF        Data_Prima_Fat_6 > 0
261000120131     c                   eval              Anno2_0 = Data_Prima_Fat_6/10000
261100120131     c                   if           Anno2_0 < 40
261200120131     c                   eval      Data_Prima_Fat_8 = 20000000 +
261300120131     c                                            Data_Prima_Fat_6
261400120131     c                   else
261500120131     c                   eval      Data_Prima_Fat_8 = 19000000 +
261600120131     c                                            Data_Prima_Fat_6
261700120131     c                   end
261800120131     c                   endIF
261900120131      *
262000120131     c                   clear                   Anno2_0           2 0
262100120131     c                   IF        CLPdps > 0
262200120131     c                   eval              Anno2_0 = CLPdps/10000
262300120131     c                   if           Anno2_0 < 40
262400120131     c                   eval      CLP_Prima_Fat_8 = 20000000 + CLPdps
262500120131     c                   else
262600120131     c                   eval      CLP_Prima_Fat_8 = 19000000 + CLPdps
262700120131     c                   end
262800120131     c                   endIF
262900120131      *
263000120125      **  deve memorizzare, fra tutti i clienti unificati, la più vecchia data
263100120125      ***   quindi
263200120125      **  la prima spedizione fatturata.
263300120131     C*
263400120131     c                   if             Data_Prima_Fat_8 > CLP_Prima_Fat_8 or
263500120131     c                                  Data_Prima_Fat_8 = 0
263600120213     c                   if                 CLPdps > 0
263700120125     c                   eval        Data_Prima_Fat_6 = CLPdps
263800120125     c                   end
263900120213     c                   end
264000061220      ***
264100061220      *  se almeno un codice figlio non è bloccato non lo deve segnalare come
264200061220      *  totalità.
264300061220     c                   if        skbl(is) <> '+'
264400061220     C                   movel     *blank        Total_Bloccato
264500061220     c                   end
264600061220      ***
264700051031     C*--------------------
264800000828     C                   ADD       1             I                              *CLIENTI
264900000828E   4C                   ENDDO                                                  *ROTTURA BLOCCO
265000000828     C*
265100051031     C* STAMPA BLOCCO DI CLIENTI unificati
265200051031     C*
265300000828IF  4C     IS            IFGT      *ZEROS                                       *CLIENTI DA STAMPARE
265400000828     C     $STRIG        OREQ      'S'                                          *RIGA DA STAMPARE
265500000829     C                   ADD       1             BLOCCO                         INCREMENTA N.BLOCCHI
265600000828     C                   Z-ADD     SKSC(1)       P3CC01
265700000828     C                   Z-ADD     SKSC(2)       P3CC02
265800000828     C                   Z-ADD     SKSC(3)       P3CC03
265900000828     C                   Z-ADD     SKSC(4)       P3CC04
266000000828     C                   Z-ADD     SKSC(5)       P3CC05
266100000828     C                   Z-ADD     SKSC(6)       P3CC06
266200000828     C                   Z-ADD     SKSC(7)       P3CC07
266300000828     C                   Z-ADD     SKSC(8)       P3CC08
266400000828     C                   Z-ADD     SKSC(9)       P3CC09
266500000828     C                   Z-ADD     SKSC(10)      P3CC10
266600000828     C                   Z-ADD     SKSC(11)      P3CC11
266700000828     C                   Z-ADD     SKSC(12)      P3CC12
266800000828     C                   Z-ADD     SKSC(13)      P3CC13
266900000828     C                   Z-ADD     SKSC(14)      P3CC14
267000000828     C                   Z-ADD     SKSC(15)      P3CC15
267100000828     C                   Z-ADD     SKSC(16)      P3CC16
267200000828     C                   Z-ADD     SKSC(17)      P3CC17
267300051031     C******             Z-ADD     SKSC(18)      P3CC18
267400051031     C******             Z-ADD     SKSC(19)      P3CC19
267500051031      **  Aggiunti Flag x cliente bloccato sotto unificante
267600051031      **   x questo ridotta la schiera SKSC per fare spazio in stampa
267700051031     C                   move      skbl(1)       P3Cf01
267800051031     C                   move      skbl(2)       P3Cf02
267900051031     C                   move      skbl(3)       P3Cf03
268000051031     C                   move      skbl(4)       P3Cf04
268100051031     C                   move      skbl(5)       P3Cf05
268200051031     C                   move      skbl(6)       P3Cf06
268300051031     C                   move      skbl(7)       P3Cf07
268400051031     C                   move      skbl(8)       P3Cf08
268500051031     C                   move      skbl(9)       P3Cf09
268600051031     C                   move      skbl(10)      P3Cf10
268700051031     C                   move      skbl(11)      P3Cf11
268800051031     C                   move      skbl(12)      P3Cf12
268900051031     C                   move      skbl(13)      P3Cf13
269000051031     C                   move      skbl(14)      P3Cf14
269100051031     C                   move      skbl(15)      P3Cf15
269200051031     C                   move      skbl(16)      P3Cf16
269300051031     C                   move      skbl(17)      P3Cf17
269400000829IF  5C     BLOCCO        IFEQ      1                                            *1° BLOCCO DA STAMP.
269500000829     C                   MOVEL     DRI(28)       P3DCLI
269600000829E   5C                   ENDIF
269700020131     C*
269800020131     C* PRIMA DI OGNI RIGA DA STAMPARE VERIFICA IL SALTO PAGINA
269900020131IF  1C     *IN01         IFEQ      *ON
270000020131IF  2C     PRIGIR        IFEQ      '0'
270100020131     C                   MOVEL     '1'           PRIGIR
270200020131X   2C                   ELSE
270300100129     c                   if        parSTAMPA <> 'N'
270400020131     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
270500100129     c                   end
270600020131E   2C                   ENDIF
270700020222     C*
270800020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
270900020222     C                   IF        FLGSTPLE = '1'
271000100129     c                   if        parSTAMPA <> 'N'
271100020222     C                   WRITE     SE94LE                                       *LEGENDA
271200100129     c                   end
271300020222     C                   ELSE
271400020222     C                   EVAL      FLGSTPLE = '1'
271500020222     C                   ENDIF
271600020222     C*
271700100129     c                   if        parSTAMPA <> 'N'
271800020222     C                   WRITE     SE94PT                                       *TESTATA
271900020131     C                   WRITE     SE94IN                                       *INTESTAZIONE
272000020131     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
272100020131     C                   WRITE     SE94PI                                       *RIGA MESI
272200020131     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
272300100129     c                   end
272400020131     C                   SETOFF                                       01
272500020131E   1C                   ENDIF
272600020221     C*
272700100129     c                   if        parSTAMPA <> 'N'
272800020116     C                   WRITE     SE94P3
272900100129     c                   end
273000020116     C                   CLEAR                   SE94P3                         AZZERA FORMATO
273100000828     C                   MOVEL     'N'           $STRIG                         RIGA DA NON STAMPARE
273200000828E   4C                   ENDIF
273300000828E   3C                   ENDDO                                                  *FINE FIGLI
273400000828E   2C                   ENDIF
273500000828E   1C                   ENDIF
273600000828     C*
273700000828IF  1C     $STRIG        IFEQ      'S'                                          *ANCORA DA STAMPARE
273800020131     C*
273900020131     C* PRIMA DI OGNI RIGA DA STAMPARE VERIFICA IL SALTO PAGINA
274000020131IF  1C     *IN01         IFEQ      *ON
274100020131IF  2C     PRIGIR        IFEQ      '0'
274200020131     C                   MOVEL     '1'           PRIGIR
274300020131X   2C                   ELSE
274400100129     c                   if        parSTAMPA <> 'N'
274500020131     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
274600100129     c                   end
274700020131E   2C                   ENDIF
274800020222     C*
274900020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
275000020222     C                   IF        FLGSTPLE = '1'
275100100129     c                   if        parSTAMPA <> 'N'
275200020222     C                   WRITE     SE94LE                                       *LEGENDA
275300100129     c                   end
275400020222     C                   ELSE
275500020222     C                   EVAL      FLGSTPLE = '1'
275600020222     C                   ENDIF
275700020222     C*
275800100129     c                   if        parSTAMPA <> 'N'
275900020222     C                   WRITE     SE94PT                                       *TESTATA
276000020131     C                   WRITE     SE94IN                                       *INTESTAZIONE
276100020131     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
276200020131     C                   WRITE     SE94PI                                       *RIGA MESI
276300020131     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
276400100129     c                   end
276500020131     C                   SETOFF                                       01
276600020131E   1C                   ENDIF
276700020221     C*
276800100129     c                   if        parSTAMPA <> 'N'
276900020116     C                   WRITE     SE94P3
277000100129     c                   end
277100000828E   1C                   ENDIF
277200000000     C*--------------------
277300000000     C* R I G A   V U O T A
277400000000     C*--------------------
277500020116     C                   CLEAR                   SE94P1
277600100129     c                   if        parSTAMPA <> 'N'
277700020116     C                   WRITE     SE94P1
277800100129     c                   end
277900000000     C*
278000030311     C* SE RICHIESTO NEL LANBCIO INSERISCO IL RECORD NEL FILE PC (WFDELPCF)
278100030311     C                   IF        PARFPC = 'S'
278200061220     C*
278300061220     C* aggiunto campo di segnalazione cliente totalmente bloccato
278400061220     C*  come se stesso + tutti gli eventuali figli se unificante:
278500100406     c*********>>>       eval      wfpBLO = Total_Bloccato
278600100406     c                   clear                   aaaBLO            1
278700120125     c                   clear                   aaaCULTBLO        3
278800120125     c                   clear                   aaaDULTBLO        8 0
278900120125     c                   clear                   aaaDPRIFAT        8 0
279000100406     c                   eval      aaaBLO = Total_Bloccato
279100120125      *
279200120125      **  solo se il codice unificante è Totalmente bloccato deve riportare
279300120125      **   il codice di blocco e la data dell'ultimo cliente che in ordine di tempo
279400120125      *   è stato bloccato.
279500120125     c                   if        Total_Bloccato = 'S'
279600120125     c                   eval       aaaCULTBLO =  Cod_Blocco_Ultimo
279700120125     c                   eval       aaaDULTBLO = Data_Blocco_Ultimo
279800120125     c                   end
279900120125      *  Inoltre si deve memorizzare quando è stata fatturata la prima spedizione
280000120125      *   fra tutti i clienti unificati.
280100120125     c                   clear                   Anno2_0           2 0
280200120125     c                   IF        Data_Prima_Fat_6 > 0
280300120125     c                   eval      Anno2_0 = Data_Prima_Fat_6/10000
280400120125     c                   if        Anno2_0 < 40
280500120125     c                   eval       aaaDPRIFAT = 20000000 + Data_Prima_Fat_6
280600120125     c                   else
280700120125     c                   eval       aaaDPRIFAT = 19000000 + Data_Prima_Fat_6
280800120125     c                   end
280900120125     c                   endIF
281000061220     C*
281100100511     C**------------>    WRITE     WFDELPC0
281200070418     C*
281300070418      * ?- Scrive il nuovo files WAdelpcf identico al WFdelpcf con i campi in +
281400070418      *              ? *---------------------*
281500070418     c                   exsr      wri_WAdel
281600070418      *              ? *---------------------*
281700100201     C*
281800100201     C*  Solo x Estero x Network
281900100201      * -------------
282000100201IF  1C     PARIOE        IFEQ      'E'
282100100201      *              ? *---------------------*
282200100201     C                   exsr      wriWADELntw
282300100201      *              ? *---------------------*
282400100201     C                   END
282500100201      * -------------
282600070418     C*
282700100216      * ?- Scrive il nuovo files WAdeApcI identico al WFdelpcf con i campi in + x NTW e Tipo Servizio
282800100201      *              ? *---------------------*
282900100201     c                   exsr      wri_WAdeA
283000100201      *              ? *---------------------*
283100100201     C*
283200030311     C                   ENDIF
283300030311     C*
283400000000     C                   ENDSR
283500070418     C*------------------------------------------------------------------------*
283600070418      *?  Scrive il nuovo WFile identico al WFDELPCF ma con informazioni più dettagliate    ?
283700070418     C*------------------------------------------------------------------------*
283800070418     C     wri_WAdel     BEGSR
283900070418     C*
284000070418      * ?Campi uguali al precedente WFile
284100070418     c                   clear                   WAdelPC0
284200100406     c*********>>>       eval      WaPAAD   =    WFPAAD
284300100406     c*********>>>       eval      WaPMMD   =    WFPMMD
284400100406     c*********>>>       eval      WaPAAA   =    WFPAAA
284500100406     c*********>>>       eval      WaPMMA   =    WFPMMA
284600100406     c*********>>>       eval      WaPIOE   =    WFPIOE
284700100406     c*********>>>       eval      WaPDIV   =    WFPDIV
284800100406     c*********>>>       eval      WaPDID   =    WFPDID
284900100406     c*********>>>       eval      WaPARE   =    WFPARE
285000100406     c*********>>>       eval      WaPARD   =    WFPARD
285100100406     c*********>>>       eval      WaPFIL   =    WFPFIL
285200100406     c*********>>>       eval      WaPFID   =    WFPFID
285300100406     c*********>>>       eval      WaPCLV   =    WFPCLV
285400100406     c*********>>>       eval      WaPCLI   =    WFPCLI
285500100406     c*********>>>       eval      WaPCLD   =    WFPCLD
285600100406     c*********>>>       eval      WaPITC   =    WFPITC
285700100406     c*********>>>       eval      WaPCLN   =    WFPCLN
285800100406     c*********>>>       eval      WaPAGE   =    WFPAGE
285900100406     c*********>>>       eval      WaPAGD   =    WFPAGD
286000100406     c*********>>>       eval      WaPSAP   =    WFPSAP
286100100406     c*********>>>       eval      WaPFAP   =    WFPFAP
286200100406     c*********>>>       eval      WaPDAP   =    WFPDAP
286300100406     c*********>>>       eval      WaPSAC   =    WFPSAC
286400100406     c*********>>>       eval      WaPFAC   =    WFPFAC
286500100406     c*********>>>       eval      WaPDAC   =    WFPDAC
286600100406     c*********>>>       eval      WaPBLO   =    WFPBLO
286700100406     c                   eval      WaPAAD   =    aaaAAD
286800100406     c                   eval      WaPMMD   =    aaaMMD
286900100406     c                   eval      WaPAAA   =    aaaAAA
287000100406     c                   eval      WaPMMA   =    aaaMMA
287100100406     c                   eval      WaPIOE   =    aaaIOE
287200100406     c                   eval      WaPDIV   =    aaaDIV
287300100406     c                   eval      WaPDID   =    aaaDID
287400100406     c                   eval      WaPARE   =    aaaARE
287500100406     c                   eval      WaPARD   =    aaaARD
287600100406     c                   eval      WaPFIL   =    aaaFIL
287700100406     c                   eval      WaPFID   =    aaaFID
287800100406     c                   eval      WaPCLV   =    aaaCLV
287900100406     c                   eval      WaPCLI   =    aaaCLI
288000100406     c                   eval      WaPCLD   =    aaaCLD
288100100406     c                   eval      WaPITC   =    aaaITC
288200100406     c                   eval      WaPCLN   =    aaaCLN
288300100406     c                   eval      WaPAGE   =    aaaAGE
288400100406     c                   eval      WaPAGD   =    aaaAGD
288500100406     c                   eval      WaPSAP   =    aaaSAP
288600100406     c                   eval      WaPFAP   =    aaaFAP
288700100406     c                   eval      WaPDAP   =    aaaDAP
288800100406     c                   eval      WaPSAC   =    aaaSAC
288900100406     c                   eval      WaPFAC   =    aaaFAC
289000100406     c                   eval      WaPDAC   =    aaaDAC
289100100406     c                   eval      WaPBLO   =    aaaBLO
289200120125     c                   eval      WApCDULBLO =  aaaCULTBLO
289300120125     c                   eval      WApDTULBLO =  aaaDULTBLO
289400120125     c                   eval      WApDTPRIFA =  aaaDPRIFAT
289500081013      *
289600070418      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
289700070419     C*--------------------
289800070419     C* Anno Precedente 12 Mesi
289900070419     C*--------------------
290000070419     C***    Spedizioni mensili
290100070419     C                   Z-ADD     ap_S1K(1)     wapNS_01aP
290200070419     C                   Z-ADD     ap_S1K(2)     wapNS_02aP
290300070419     C                   Z-ADD     ap_S1K(3)     wapNS_03aP
290400070419     C                   Z-ADD     ap_S1K(4)     wapNS_04aP
290500070419     C                   Z-ADD     ap_S1K(5)     wapNS_05aP
290600070419     C                   Z-ADD     ap_S1K(6)     wapNS_06aP
290700070419     C                   Z-ADD     ap_S1K(7)     wapNS_07aP
290800070419     C                   Z-ADD     ap_S1K(8)     wapNS_08aP
290900070419     C                   Z-ADD     ap_S1K(9)     wapNS_09aP
291000070419     C                   Z-ADD     ap_S1K(10)    wapNS_10aP
291100070419     C                   Z-ADD     ap_S1K(11)    wapNS_11aP
291200070419     C                   Z-ADD     ap_S1K(12)    wapNS_12aP
291300081013     C***    Pesi mensili
291400081016     c                   clear                   WaPkgP
291500081016     c                   add       ap_Kg1K(1)    WaPkgP
291600081016     c                   add       ap_Kg1K(2)    WaPkgP
291700081016     c                   add       ap_Kg1K(3)    WaPkgP
291800081016     c                   add       ap_Kg1K(4)    WaPkgP
291900081016     c                   add       ap_Kg1K(5)    WaPkgP
292000081016     c                   add       ap_Kg1K(6)    WaPkgP
292100081016     c                   add       ap_Kg1K(7)    WaPkgP
292200081016     c                   add       ap_Kg1K(8)    WaPkgP
292300081016     c                   add       ap_Kg1K(9)    WaPkgP
292400081016     c                   add       ap_Kg1K(10)   WaPkgP
292500081016     c                   add       ap_Kg1K(11)   WaPkgP
292600081016     c                   add       ap_Kg1K(12)   WaPkgP
292700081013     C                   Z-ADD     ap_Kg1K(1)    wapKG_01aP
292800081013     C                   Z-ADD     ap_Kg1K(2)    wapKG_02aP
292900081013     C                   Z-ADD     ap_Kg1K(3)    wapKG_03aP
293000081013     C                   Z-ADD     ap_Kg1K(4)    wapKG_04aP
293100081013     C                   Z-ADD     ap_Kg1K(5)    wapKG_05aP
293200081013     C                   Z-ADD     ap_Kg1K(6)    wapKG_06aP
293300081013     C                   Z-ADD     ap_Kg1K(7)    wapKG_07aP
293400081013     C                   Z-ADD     ap_Kg1K(8)    wapKG_08aP
293500081013     C                   Z-ADD     ap_Kg1K(9)    wapKG_09aP
293600081013     C                   Z-ADD     ap_Kg1K(10)   wapKG_10aP
293700081013     C                   Z-ADD     ap_Kg1K(11)   wapKG_11aP
293800081013     C                   Z-ADD     ap_Kg1K(12)   wapKG_12aP
293900070419     C***  Ricavi Reali mensili
294000070419     C                   Z-ADD     ap_R1K(1)     wapRR_01aP
294100070419     C                   Z-ADD     ap_R1K(2)     wapRR_02aP
294200070419     C                   Z-ADD     ap_R1K(3)     wapRR_03aP
294300070419     C                   Z-ADD     ap_R1K(4)     wapRR_04aP
294400070419     C                   Z-ADD     ap_R1K(5)     wapRR_05aP
294500070419     C                   Z-ADD     ap_R1K(6)     wapRR_06aP
294600070419     C                   Z-ADD     ap_R1K(7)     wapRR_07aP
294700070419     C                   Z-ADD     ap_R1K(8)     wapRR_08aP
294800070419     C                   Z-ADD     ap_R1K(9)     wapRR_09aP
294900070419     C                   Z-ADD     ap_R1K(10)    wapRR_10aP
295000070419     C                   Z-ADD     ap_R1K(11)    wapRR_11aP
295100070419     C                   Z-ADD     ap_R1K(12)    wapRR_12aP
295200070419     C***  Ricavi Presunti mensili
295300070419     C                   Z-ADD     ap_P1K(1)     wapRP_01aP
295400070419     C                   Z-ADD     ap_P1K(2)     wapRP_02aP
295500070419     C                   Z-ADD     ap_P1K(3)     wapRP_03aP
295600070419     C                   Z-ADD     ap_P1K(4)     wapRP_04aP
295700070419     C                   Z-ADD     ap_P1K(5)     wapRP_05aP
295800070419     C                   Z-ADD     ap_P1K(6)     wapRP_06aP
295900070419     C                   Z-ADD     ap_P1K(7)     wapRP_07aP
296000070419     C                   Z-ADD     ap_P1K(8)     wapRP_08aP
296100070419     C                   Z-ADD     ap_P1K(9)     wapRP_09aP
296200070419     C                   Z-ADD     ap_P1K(10)    wapRP_10aP
296300070419     C                   Z-ADD     ap_P1K(11)    wapRP_11aP
296400070419     C                   Z-ADD     ap_P1K(12)    wapRP_12aP
296500070419      *
296600070419     C*--------------------
296700070419     C* Anno in Corso 12 Mesi
296800070419     C*--------------------
296900070419     C***    Spedizioni mensili
297000070419     C                   Z-ADD     S1K(1)        wapNS_01aC
297100070419     C                   Z-ADD     S1K(2)        wapNS_02aC
297200070419     C                   Z-ADD     S1K(3)        wapNS_03aC
297300070419     C                   Z-ADD     S1K(4)        wapNS_04aC
297400070419     C                   Z-ADD     S1K(5)        wapNS_05aC
297500070419     C                   Z-ADD     S1K(6)        wapNS_06aC
297600070419     C                   Z-ADD     S1K(7)        wapNS_07aC
297700070419     C                   Z-ADD     S1K(8)        wapNS_08aC
297800070419     C                   Z-ADD     S1K(9)        wapNS_09aC
297900070419     C                   Z-ADD     S1K(10)       wapNS_10aC
298000070419     C                   Z-ADD     S1K(11)       wapNS_11aC
298100070419     C                   Z-ADD     S1K(12)       wapNS_12aC
298200081013     C***    Pesi mensili
298300081016     c                   clear                   WaPkgC
298400081016     c                   add       Kg1K(1)       WaPkgC
298500081016     c                   add       Kg1K(2)       WaPkgC
298600081016     c                   add       Kg1K(3)       WaPkgC
298700081016     c                   add       Kg1K(4)       WaPkgC
298800081016     c                   add       Kg1K(5)       WaPkgC
298900081016     c                   add       Kg1K(6)       WaPkgC
299000081016     c                   add       Kg1K(7)       WaPkgC
299100081016     c                   add       Kg1K(8)       WaPkgC
299200081016     c                   add       Kg1K(9)       WaPkgC
299300081016     c                   add       Kg1K(10)      WaPkgC
299400081016     c                   add       Kg1K(11)      WaPkgC
299500081016     c                   add       Kg1K(12)      WaPkgC
299600081013     C                   Z-ADD     Kg1K(1)       wapKG_01aC
299700081013     C                   Z-ADD     Kg1K(2)       wapKG_02aC
299800081013     C                   Z-ADD     Kg1K(3)       wapKG_03aC
299900081013     C                   Z-ADD     Kg1K(4)       wapKG_04aC
300000081013     C                   Z-ADD     Kg1K(5)       wapKG_05aC
300100081013     C                   Z-ADD     Kg1K(6)       wapKG_06aC
300200081013     C                   Z-ADD     Kg1K(7)       wapKG_07aC
300300081013     C                   Z-ADD     Kg1K(8)       wapKG_08aC
300400081013     C                   Z-ADD     Kg1K(9)       wapKG_09aC
300500081013     C                   Z-ADD     Kg1K(10)      wapKG_10aC
300600081013     C                   Z-ADD     Kg1K(11)      wapKG_11aC
300700081013     C                   Z-ADD     Kg1K(12)      wapKG_12aC
300800070419     C***  Ricavi Reali mensili
300900070419     C                   Z-ADD     R1K(1)        wapRR_01aC
301000070419     C                   Z-ADD     R1K(2)        wapRR_02aC
301100070419     C                   Z-ADD     R1K(3)        wapRR_03aC
301200070419     C                   Z-ADD     R1K(4)        wapRR_04aC
301300070419     C                   Z-ADD     R1K(5)        wapRR_05aC
301400070419     C                   Z-ADD     R1K(6)        wapRR_06aC
301500070419     C                   Z-ADD     R1K(7)        wapRR_07aC
301600070419     C                   Z-ADD     R1K(8)        wapRR_08aC
301700070419     C                   Z-ADD     R1K(9)        wapRR_09aC
301800070419     C                   Z-ADD     R1K(10)       wapRR_10aC
301900070419     C                   Z-ADD     R1K(11)       wapRR_11aC
302000070419     C                   Z-ADD     R1K(12)       wapRR_12aC
302100070419     C***  Ricavi Presunti mensili
302200070419     C                   Z-ADD     P1K(1)        wapRP_01aC
302300070419     C                   Z-ADD     P1K(2)        wapRP_02aC
302400070419     C                   Z-ADD     P1K(3)        wapRP_03aC
302500070419     C                   Z-ADD     P1K(4)        wapRP_04aC
302600070419     C                   Z-ADD     P1K(5)        wapRP_05aC
302700070419     C                   Z-ADD     P1K(6)        wapRP_06aC
302800070419     C                   Z-ADD     P1K(7)        wapRP_07aC
302900070419     C                   Z-ADD     P1K(8)        wapRP_08aC
303000070419     C                   Z-ADD     P1K(9)        wapRP_09aC
303100070419     C                   Z-ADD     P1K(10)       wapRP_10aC
303200070419     C                   Z-ADD     P1K(11)       wapRP_11aC
303300070419     C                   Z-ADD     P1K(12)       wapRP_12aC
303400070419      *
303500070418      * ?Campi in più anagrafici
303600070419     C                   eval      WAPIND   =    DESvia                         *Indirizzo
303700070615     C                   eval      WAPcdf   =    DEScdf                         *cod.fisc
303800070615     C                   eval      WAPiva   =    DESiva                         *P.IVA
303900070618     C                   eval      WAPtel   =    DEStel                         *Telefono
304000070419      * ?CAP Italia
304100070419     c                   if        DESsta = *blank
304200070419     C                   movel(p)  DEScap        WAPCAP                         *CAP
304300070419      * ?CAP Estero
304400070419     c                   else
304500070419     C                   eval      WAPCAP   =    DEScae                         *CAP Europeo
304600070419     c                   end
304700070420     C                   eval      WAPLOC   =    DEScit                         *Città
304800070420     C                   eval      WAPPRV   =    DESprv                         *Provincia
304900070419     C                   eval      WAPNAZ   =    DESsta                         *Nazione
305000070420     C*
305100070420      * ?Resposnabile Trasporto
305200070420     C                   movel     'C'           NTCapl                         *CODICE APPLICAZIONE
305300070420     C                   movel     '0151'        NTCnk1                         *CHIAVE 1 NOTE
305400070420     C                   move      WaPCLI        NTCnk1                         *CHIAVE 1 NOTE
305500070420     C                   movel     *blank        NTCnk2                         *CHIAVE 2 NOTE
305600070420     C                   movel     '05'          NTCtnt                         *TIPO NOTA
305700070420     C     KeyNTC        chain     tfNTC01l
305800070420     c                   if        %Found(tfNTC01l)
305900070420     c                   movel     NTCRNT        WAPRESP
306000070420     c                   end
306100070426     C* ?E-mail Resposnabile Trasporto
306200070426     C                   movel     '06'          NTCtnt                         *TIPO NOTA
306300070426     C     KeyNTC        chain     tfNTC01l
306400070426     c                   if        %Found(tfNTC01l)
306500070426     c                   movel     NTCRNT        WAPEMAR
306600070426     c                   end
306700070426     C*
306800071212     C* ?Mese di Inizio Fatturato x Cliente Nuovo
306900071212     c                   clear                   WapNew_Mes
307000071212     c                   if        WaPCLN = 'N'
307100071212     c                   do        12            mes_k             3 0
307200071212     c                   if        s1k(mes_k) <> 0 or
307300081013     c                             kg1k(mes_k) <> 0 or
307400081013     c                             r1k(mes_k) <> 0 or
307500071212     c                             p1k(mes_k) <> 0
307600121217      *
307700121217      * Deve leggere l'ultimo a causa dei mesi antecedenti al vero e proprio inizio del cliente
307800121217      *  (questo è dovuto a storni o note di credito attribuite al cliente spalmate anche su
307900121217      *   mesi antecedenti il suo inizio di fatturazione).
308000121217     c                   if        cln1k(mes_k) = 'Y'
308100071212     c                   z-add     mes_k         WapNew_Mes
308200121217     c******                leave
308300121217     c                   else
308400121217      *   se prima aveva impostato il campo lo fa uscire immediatamente
308500121217      *   ma solo se prima era passato per gli elementi con "Y"
308600121217     c                   if          WapNew_Mes > 0
308700121217     c                   leave
308800121217     c                   end
308900121217     c                   end
309000121217      *
309100071212     c                   endIF
309200071212     c                   endDO
309300071212     c                   end
309400071212     C*
309500081013      * ?Campi aggiunti: AGE Unificante e Descrizione
309600081013     C                   eval      WAPaud   =    depRGD                         *Descr.Unificante
309700081013     C                   eval      WAPauc   =    depRGF                         *Cod.  Unificante
309800081013     C*
309900070418     C                   WRITE     WADELPC0
310000091123     C*
310100070418     C                   ENDSR
310200000000     C*------------------------------------------------------------------------*
310300091123     C*  Scrive i record del WADEL x Estero in base al Network e la qualità
310400091123     C*------------------------------------------------------------------------*
310500091123     C     wriWADELntw   Begsr
310600091123     C*
310700091124     C* riporta I TOTALI x NETWORK
310800091124DO  1C     1             DO        5             K
310900091124     C*
311000100202     C     K             OCCUR     W1ntw
311100100201     C*
311200091124     C* Se non ci sono più Network esce dal ciclo
311300091124     c                   if        c1Ntw    = *blank  and
311400091124     c                             ap_c1Ntw = *blank
311500091124     c                   iter
311600091124     c                   end
311700091124     C*
311800091124      * ?Deve scrivere tanti records quanti sono i dati dei vari Network
311900091124     c                   clear                   WAdel00N
312000091124     C*
312100091124     c                   eval      WaNAAD   =    WaPAAD
312200091124     c                   eval      WaNMMD   =    WaPMMD
312300091124     c                   eval      WaNAAA   =    WaPAAA
312400091124     c                   eval      WaNMMA   =    WaPMMA
312500091124     c                   eval      WaNIOE   =    WaPIOE
312600091124     c                   eval      WaNDIV   =    WaPDIV
312700091124     c                   eval      WaNDID   =    WaPDID
312800091124     c                   eval      WaNARE   =    WaPARE
312900091124     c                   eval      WaNARD   =    WaPARD
313000091124     c                   eval      WaNFIL   =    WaPFIL
313100091124     c                   eval      WaNFID   =    WaPFID
313200091124     c                   eval      WaNCLV   =    WaPCLV
313300091124     c                   eval      WaNCLI   =    WaPCLI
313400091124     c                   eval      WaNCLD   =    WaPCLD
313500091124     c                   eval      WaNITC   =    WaPITC
313600091124     c                   eval      WaNCLN   =    WaPCLN
313700091124     c                   eval      WaNAGE   =    WaPAGE
313800091124     c                   eval      WaNAGD   =    WaPAGD
313900091124     c                   eval      WaNBLO   =    WaPBLO
314000120125     c                   eval      WAnCDULBLO =  WaPCDULBLO
314100120125     c                   eval      WAnDTULBLO =  WaPDTULBLO
314200120125     c                   eval      WAnDTPRIFA =  WaPDTPRIFA
314300091124      * ?Campi in più anagrafici
314400091124     C                   eval      WAnIND   =    WaPIND                         *Indirizzo
314500091124     C                   eval      WAncdf   =    WaPcdf                         *cod.fisc
314600091124     C                   eval      WAniva   =    WaPiva                         *P.IVA
314700091124     C                   eval      WAntel   =    WaPtel                         *Telefono
314800091124     C                   eval      WAnCAP   =    WaPCAP                         *CAP Europeo
314900091124     C                   eval      WAnLOC   =    WaPLOC                         *Città
315000091124     C                   eval      WAnPRV   =    WaPPRV                         *Provincia
315100091124     C                   eval      WAnNAZ   =    WaPNAZ                         *Nazione
315200091124     c                   eval      WAnRESP  =    WaPRESP
315300091124     c                   eval      WAnEMAR  =    WaPEMAR
315400091124     c                   eval      WanNew_Mes =  WaPNew_Mes
315500091124     C                   eval      WAnaud   =    WaPaud                         *Descr.Unificante
315600091124     C                   eval      WAnauc   =    WaPauc                         *Cod.  Unificante
315700091123      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
315800091124     c                   eval      WaNntw   =    c1NTW
315900091124     c                   if          WaNntw = *blank
316000091124     c                   eval      WaNntw   =    ap_c1NTW
316100091124     c                   end
316200091124     C***
316300091124     c                   xfoot     ap_S1ntw      WaNSAP
316400091124     c                   xfoot     S1ntw         WaNSAC
316500091124     C***
316600091125     c**********         z-add     ap_tR1ntw     WaNFAP
316700091125     c**********         z-add     ap_tP1ntw     WaNPAP
316800091125     c**********         z-add     tR1ntw        WaNFAC
316900091125     c**********         z-add     tP1ntw        WaNPAC
317000091125     c                   xfoot     ap_R1ntw      WaNFAP
317100091125     c                   xfoot     ap_P1ntw      WaNPAP
317200091125     c                   xfoot     R1ntw         WaNFAC
317300091125     c                   xfoot     P1ntw         WaNPAC
317400091124      ***
317500091124     c***                eval      WaNDAP   =    WaPDAP
317600091124     c***                eval      WaNDAC   =    WaPDAC
317700091124      ***
317800091124     C                   Z-ADD     WaNFAP        WRRW             19 5          *RICAVO REALE
317900091124     C                   Z-ADD     WaNPAP        WRPW             19 5          *RICAVO PRESUNTO
318000091124     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
318100091124     C                   EXSR      CALDEL
318200091124     C                   Z-ADD     WDPW          WaNDAP                         *DELTA
318300091124      ***
318400091124     C                   Z-ADD     WaNFAC        WRRW             19 5          *RICAVO REALE
318500091124     C                   Z-ADD     WaNPAC        WRPW             19 5          *RICAVO PRESUNTO
318600091124     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
318700091124     C                   EXSR      CALDEL
318800091124     C                   Z-ADD     WDPW          WaNDAC                         *DELTA
318900091123     C*--------------------
319000091123     C* Anno Precedente 12 Mesi
319100091123     C*--------------------
319200091123     C***    Spedizioni mensili
319300091124     C                   Z-ADD     ap_S1ntw(1)   wanNS_01aP
319400091124     C                   Z-ADD     ap_S1ntw(2)   wanNS_02aP
319500091124     C                   Z-ADD     ap_S1ntw(3)   wanNS_03aP
319600091124     C                   Z-ADD     ap_S1ntw(4)   wanNS_04aP
319700091124     C                   Z-ADD     ap_S1ntw(5)   wanNS_05aP
319800091124     C                   Z-ADD     ap_S1ntw(6)   wanNS_06aP
319900091124     C                   Z-ADD     ap_S1ntw(7)   wanNS_07aP
320000091124     C                   Z-ADD     ap_S1ntw(8)   wanNS_08aP
320100091124     C                   Z-ADD     ap_S1ntw(9)   wanNS_09aP
320200091124     C                   Z-ADD     ap_S1ntw(10)  wanNS_10aP
320300091124     C                   Z-ADD     ap_S1ntw(11)  wanNS_11aP
320400091124     C                   Z-ADD     ap_S1ntw(12)  wanNS_12aP
320500091123     C***    Pesi mensili
320600091124     c                   clear                   WankgP
320700091124     c                   add       ap_Kg1ntw(1)  WankgP
320800091124     c                   add       ap_Kg1ntw(2)  WankgP
320900091124     c                   add       ap_Kg1ntw(3)  WankgP
321000091124     c                   add       ap_Kg1ntw(4)  WankgP
321100091124     c                   add       ap_Kg1ntw(5)  WankgP
321200091124     c                   add       ap_Kg1ntw(6)  WankgP
321300091124     c                   add       ap_Kg1ntw(7)  WankgP
321400091124     c                   add       ap_Kg1ntw(8)  WankgP
321500091124     c                   add       ap_Kg1ntw(9)  WankgP
321600091124     c                   add       ap_Kg1ntw(10) WankgP
321700091124     c                   add       ap_Kg1ntw(11) WankgP
321800091124     c                   add       ap_Kg1ntw(12) WankgP
321900091124     C                   Z-ADD     ap_Kg1ntw(1)  wanKG_01aP
322000091124     C                   Z-ADD     ap_Kg1ntw(2)  wanKG_02aP
322100091124     C                   Z-ADD     ap_Kg1ntw(3)  wanKG_03aP
322200091124     C                   Z-ADD     ap_Kg1ntw(4)  wanKG_04aP
322300091124     C                   Z-ADD     ap_Kg1ntw(5)  wanKG_05aP
322400091124     C                   Z-ADD     ap_Kg1ntw(6)  wanKG_06aP
322500091124     C                   Z-ADD     ap_Kg1ntw(7)  wanKG_07aP
322600091124     C                   Z-ADD     ap_Kg1ntw(8)  wanKG_08aP
322700091124     C                   Z-ADD     ap_Kg1ntw(9)  wanKG_09aP
322800091124     C                   Z-ADD     ap_Kg1ntw(10) wanKG_10aP
322900091124     C                   Z-ADD     ap_Kg1ntw(11) wanKG_11aP
323000091124     C                   Z-ADD     ap_Kg1ntw(12) wanKG_12aP
323100091123     C***  Ricavi Reali mensili
323200091124     C                   Z-ADD     ap_R1ntw(1)   wanRR_01aP
323300091124     C                   Z-ADD     ap_R1ntw(2)   wanRR_02aP
323400091124     C                   Z-ADD     ap_R1ntw(3)   wanRR_03aP
323500091124     C                   Z-ADD     ap_R1ntw(4)   wanRR_04aP
323600091124     C                   Z-ADD     ap_R1ntw(5)   wanRR_05aP
323700091124     C                   Z-ADD     ap_R1ntw(6)   wanRR_06aP
323800091124     C                   Z-ADD     ap_R1ntw(7)   wanRR_07aP
323900091124     C                   Z-ADD     ap_R1ntw(8)   wanRR_08aP
324000091124     C                   Z-ADD     ap_R1ntw(9)   wanRR_09aP
324100091124     C                   Z-ADD     ap_R1ntw(10)  wanRR_10aP
324200091124     C                   Z-ADD     ap_R1ntw(11)  wanRR_11aP
324300091124     C                   Z-ADD     ap_R1ntw(12)  wanRR_12aP
324400091123     C***  Ricavi Presunti mensili
324500091124     C                   Z-ADD     ap_P1ntw(1)   wanRP_01aP
324600091124     C                   Z-ADD     ap_P1ntw(2)   wanRP_02aP
324700091124     C                   Z-ADD     ap_P1ntw(3)   wanRP_03aP
324800091124     C                   Z-ADD     ap_P1ntw(4)   wanRP_04aP
324900091124     C                   Z-ADD     ap_P1ntw(5)   wanRP_05aP
325000091124     C                   Z-ADD     ap_P1ntw(6)   wanRP_06aP
325100091124     C                   Z-ADD     ap_P1ntw(7)   wanRP_07aP
325200091124     C                   Z-ADD     ap_P1ntw(8)   wanRP_08aP
325300091124     C                   Z-ADD     ap_P1ntw(9)   wanRP_09aP
325400091124     C                   Z-ADD     ap_P1ntw(10)  wanRP_10aP
325500091124     C                   Z-ADD     ap_P1ntw(11)  wanRP_11aP
325600091124     C                   Z-ADD     ap_P1ntw(12)  wanRP_12aP
325700091123     C*--------------------
325800091123     C* Anno in Corso 12 Mesi
325900091123     C*--------------------
326000091123     C***    Spedizioni mensili
326100091124     C                   Z-ADD     S1ntw(1)      wanNS_01aC
326200091124     C                   Z-ADD     S1ntw(2)      wanNS_02aC
326300091124     C                   Z-ADD     S1ntw(3)      wanNS_03aC
326400091124     C                   Z-ADD     S1ntw(4)      wanNS_04aC
326500091124     C                   Z-ADD     S1ntw(5)      wanNS_05aC
326600091124     C                   Z-ADD     S1ntw(6)      wanNS_06aC
326700091124     C                   Z-ADD     S1ntw(7)      wanNS_07aC
326800091124     C                   Z-ADD     S1ntw(8)      wanNS_08aC
326900091124     C                   Z-ADD     S1ntw(9)      wanNS_09aC
327000091124     C                   Z-ADD     S1ntw(10)     wanNS_10aC
327100091124     C                   Z-ADD     S1ntw(11)     wanNS_11aC
327200091124     C                   Z-ADD     S1ntw(12)     wanNS_12aC
327300091123     C***    Pesi mensili
327400091124     c                   clear                   WankgC
327500091124     c                   add       Kg1ntw(1)     WankgC
327600091124     c                   add       Kg1ntw(2)     WankgC
327700091124     c                   add       Kg1ntw(3)     WankgC
327800091124     c                   add       Kg1ntw(4)     WankgC
327900091124     c                   add       Kg1ntw(5)     WankgC
328000091124     c                   add       Kg1ntw(6)     WankgC
328100091124     c                   add       Kg1ntw(7)     WankgC
328200091124     c                   add       Kg1ntw(8)     WankgC
328300091124     c                   add       Kg1ntw(9)     WankgC
328400091124     c                   add       Kg1ntw(10)    WankgC
328500091124     c                   add       Kg1ntw(11)    WankgC
328600091124     c                   add       Kg1ntw(12)    WankgC
328700091124     C                   Z-ADD     Kg1ntw(1)     wanKG_01aC
328800091124     C                   Z-ADD     Kg1ntw(2)     wanKG_02aC
328900091124     C                   Z-ADD     Kg1ntw(3)     wanKG_03aC
329000091124     C                   Z-ADD     Kg1ntw(4)     wanKG_04aC
329100091124     C                   Z-ADD     Kg1ntw(5)     wanKG_05aC
329200091124     C                   Z-ADD     Kg1ntw(6)     wanKG_06aC
329300091124     C                   Z-ADD     Kg1ntw(7)     wanKG_07aC
329400091124     C                   Z-ADD     Kg1ntw(8)     wanKG_08aC
329500091124     C                   Z-ADD     Kg1ntw(9)     wanKG_09aC
329600091124     C                   Z-ADD     Kg1ntw(10)    wanKG_10aC
329700091124     C                   Z-ADD     Kg1ntw(11)    wanKG_11aC
329800091124     C                   Z-ADD     Kg1ntw(12)    wanKG_12aC
329900091123     C***  Ricavi Reali mensili
330000091124     C                   Z-ADD     R1ntw(1)      wanRR_01aC
330100091124     C                   Z-ADD     R1ntw(2)      wanRR_02aC
330200091124     C                   Z-ADD     R1ntw(3)      wanRR_03aC
330300091124     C                   Z-ADD     R1ntw(4)      wanRR_04aC
330400091124     C                   Z-ADD     R1ntw(5)      wanRR_05aC
330500091124     C                   Z-ADD     R1ntw(6)      wanRR_06aC
330600091124     C                   Z-ADD     R1ntw(7)      wanRR_07aC
330700091124     C                   Z-ADD     R1ntw(8)      wanRR_08aC
330800091124     C                   Z-ADD     R1ntw(9)      wanRR_09aC
330900091124     C                   Z-ADD     R1ntw(10)     wanRR_10aC
331000091124     C                   Z-ADD     R1ntw(11)     wanRR_11aC
331100091124     C                   Z-ADD     R1ntw(12)     wanRR_12aC
331200091123     C***  Ricavi Presunti mensili
331300091124     C                   Z-ADD     P1ntw(1)      wanRP_01aC
331400091124     C                   Z-ADD     P1ntw(2)      wanRP_02aC
331500091124     C                   Z-ADD     P1ntw(3)      wanRP_03aC
331600091124     C                   Z-ADD     P1ntw(4)      wanRP_04aC
331700091124     C                   Z-ADD     P1ntw(5)      wanRP_05aC
331800091124     C                   Z-ADD     P1ntw(6)      wanRP_06aC
331900091124     C                   Z-ADD     P1ntw(7)      wanRP_07aC
332000091124     C                   Z-ADD     P1ntw(8)      wanRP_08aC
332100091124     C                   Z-ADD     P1ntw(9)      wanRP_09aC
332200091124     C                   Z-ADD     P1ntw(10)     wanRP_10aC
332300091124     C                   Z-ADD     P1ntw(11)     wanRP_11aC
332400091124     C                   Z-ADD     P1ntw(12)     wanRP_12aC
332500091124      *
332600091124     C                   WRITE     WADEL00n
332700091124     C                   ENDdo
332800091123     C*
332900091123     C                   ENDSR
333000100201      * ?----------------------------------------------------------------------
333100100201      *?  Scrive il nuovo WFile identico al WFDELPCF ma con informazioni in più   ?
333200100201      *?    NETWORK e TIPO SERVIZIO con anche DETTAGLIO VARIE "e" / "h"           ?
333300100201      * ?----------------------------------------------------------------------
333400100201     C     wri_WAdeA     BEGSR
333500100201      *
333600100201     c                   eval      numElem = %Elem(W1ntwTS)
333700100201     c**
333800100201      **  Quindi se da totalizzare cerca su quale occurence deve metterlo.
333900160509     c                   do        NUMELEM       kk
334000100201     C*
334100100202     C*  Attenzione alle schiere fra Anno in corso e Anno precedente
334200100202      * ?Deve scrivere tanti records quanti sono i dati dei vari Network/TServizi
334300160509     C     Kk            OCCUR     W1ntwTS
334400100201     C*
334500100204     C* Se non ci sono più Network/tipi servizio esce dal ciclo
334600100204     c                   if        c1NtwTS  = *blank  and c1tpsTS = *blank
334700100202     c                   leave
334800100201     c                   end
334900100201     C*
335000100201      * ?Deve scrivere tanti records quanti sono i dati dei vari Network
335100100201     c                   clear                   WAdeAPC0
335200100201     C*
335300100201     c                   eval      WadAAD   =    WaPAAD
335400100201     c                   eval      WadMMD   =    WaPMMD
335500100201     c                   eval      WadAAA   =    WaPAAA
335600100201     c                   eval      WadMMA   =    WaPMMA
335700100201     c                   eval      WadIOE   =    WaPIOE
335800100201     c                   eval      WadDIV   =    WaPDIV
335900100201     c                   eval      WadDID   =    WaPDID
336000100201     c                   eval      WadARE   =    WaPARE
336100100201     c                   eval      WadARD   =    WaPARD
336200100201     c                   eval      WadFIL   =    WaPFIL
336300100201     c                   eval      WadFID   =    WaPFID
336400100201     c                   eval      WadCLV   =    WaPCLV
336500100201     c                   eval      WadCLI   =    WaPCLI
336600100201     c                   eval      WadCLD   =    WaPCLD
336700100201     c                   eval      WadITC   =    WaPITC
336800100201     c                   eval      WadCLN   =    WaPCLN
336900100201     c                   eval      WadAGE   =    WaPAGE
337000100201     c                   eval      WadAGD   =    WaPAGD
337100100201     c                   eval      WadBLO   =    WaPBLO
337200120125     c                   eval      WAdCDULBLO =  WaPCDULBLO
337300120125     c                   eval      WAdDTULBLO =  WaPDTULBLO
337400120125     c                   eval      WAdDTPRIFA =  WaPDTPRIFA
337500100201      * ?Campi in più anagrafici
337600100201     C                   eval      WAdIND   =    WaPIND                         *Indirizzo
337700100201     C                   eval      WAdcdf   =    WaPcdf                         *cod.fisc
337800100201     C                   eval      WAdiva   =    WaPiva                         *P.IVA
337900100201     C                   eval      WAdtel   =    WaPtel                         *Telefono
338000100201     C                   eval      WAdCAP   =    WaPCAP                         *CAP Europeo
338100100201     C                   eval      WAdLOC   =    WaPLOC                         *Città
338200100201     C                   eval      WAdPRV   =    WaPPRV                         *Provincia
338300100201     C                   eval      WAdNAZ   =    WaPNAZ                         *Nazione
338400100201     c                   eval      WAdRESP  =    WaPRESP
338500100201     c                   eval      WAdEMAR  =    WaPEMAR
338600100201     c                   eval      WadNew_Mes =  WaPNew_Mes
338700100201     C                   eval      WAdaud   =    WaPaud                         *Descr.Unificante
338800100201     C                   eval      WAdauc   =    WaPauc                         *Cod.  Unificante
338900100201      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
339000100215     c                   if        c1ntwTS  = 'ITA'
339100100215     c                   eval      Wadntw   =    *blank
339200100215     c                   else
339300100202     c                   eval      Wadntw   =    c1ntwTS
339400100215     c                   end
339500100215     c                   eval      WadTSP   =    c1tpsTS
339600100201     C***
339700100202     c                   xfoot     ap_S1ntwTS    WadSAP
339800100202     c                   xfoot     S1ntwTS       WadSAC
339900100201     C***
340000100202     c                   xfoot     ap_R1ntwTS    WadFAP
340100100202     c                   xfoot     ap_P1ntwTS    WadPAP
340200100202     c                   xfoot     R1ntwTS       WadFAC
340300100202     c                   xfoot     P1ntwTS       WadPAC
340400100201      ***
340500100201     C                   Z-ADD     WadFAP        WRRW             19 5          *RICAVO REALE
340600100201     C                   Z-ADD     WadPAP        WRPW             19 5          *RICAVO PRESUNTO
340700100201     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
340800100201     C                   EXSR      CALDEL
340900100201     C                   Z-ADD     WDPW          WaDDAP                         *DELTA
341000100201      ***
341100100201     C                   Z-ADD     WaDFAC        WRRW             19 5          *RICAVO REALE
341200100201     C                   Z-ADD     WaDPAC        WRPW             19 5          *RICAVO PRESUNTO
341300100201     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
341400100201     C                   EXSR      CALDEL
341500100201     C                   Z-ADD     WDPW          WaDDAC                         *DELTA
341600100201     C*--------------------
341700100201     C* Anno Precedente 12 Mesi
341800100201     C*--------------------
341900100201     C***    Spedizioni mensili
342000100202     C                   Z-ADD     ap_S1ntwTS(1) wadNS_01aP
342100100202     C                   Z-ADD     ap_S1ntwTS(2) wadNS_02aP
342200100202     C                   Z-ADD     ap_S1ntwTS(3) wadNS_03aP
342300100202     C                   Z-ADD     ap_S1ntwTS(4) wadNS_04aP
342400100202     C                   Z-ADD     ap_S1ntwTS(5) wadNS_05aP
342500100202     C                   Z-ADD     ap_S1ntwTS(6) wadNS_06aP
342600100202     C                   Z-ADD     ap_S1ntwTS(7) wadNS_07aP
342700100202     C                   Z-ADD     ap_S1ntwTS(8) wadNS_08aP
342800100202     C                   Z-ADD     ap_S1ntwTS(9) wadNS_09aP
342900100202     C                   Z-ADD     ap_S1ntwTS(10)wadNS_10aP
343000100202     C                   Z-ADD     ap_S1ntwTS(11)wadNS_11aP
343100100202     C                   Z-ADD     ap_S1ntwTS(12)wadNS_12aP
343200100201     C***    Pesi mensili
343300100201     c                   clear                   WadkgP
343400100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(1)
343500100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(2)
343600100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(3)
343700100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(4)
343800100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(5)
343900100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(6)
344000100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(7)
344100100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(8)
344200100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(9)
344300100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(10)
344400100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(11)
344500100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(12)
344600100202     C                   Z-ADD     ap_Kg1ntwTS(1)wadKG_01aP
344700100202     C                   Z-ADD     ap_Kg1ntwTS(2)wadKG_02aP
344800100202     C                   Z-ADD     ap_Kg1ntwTS(3)wadKG_03aP
344900100202     C                   Z-ADD     ap_Kg1ntwTS(4)wadKG_04aP
345000100202     C                   Z-ADD     ap_Kg1ntwTS(5)wadKG_05aP
345100100202     C                   Z-ADD     ap_Kg1ntwTS(6)wadKG_06aP
345200100202     C                   Z-ADD     ap_Kg1ntwTS(7)wadKG_07aP
345300100202     C                   Z-ADD     ap_Kg1ntwTS(8)wadKG_08aP
345400100202     C                   Z-ADD     ap_Kg1ntwTS(9)wadKG_09aP
345500100202     c                   eval      wadKG_01aP = ap_Kg1ntwTS(1)
345600100202     c                   eval      wadKG_02aP = ap_Kg1ntwTS(2)
345700100202     c                   eval      wadKG_03aP = ap_Kg1ntwTS(3)
345800100202     c                   eval      wadKG_04aP = ap_Kg1ntwTS(4)
345900100202     c                   eval      wadKG_05aP = ap_Kg1ntwTS(5)
346000100202     c                   eval      wadKG_06aP = ap_Kg1ntwTS(6)
346100100202     c                   eval      wadKG_07aP = ap_Kg1ntwTS(7)
346200100202     c                   eval      wadKG_08aP = ap_Kg1ntwTS(8)
346300100202     c                   eval      wadKG_09aP = ap_Kg1ntwTS(9)
346400100202     c                   eval      wadKG_10aP = ap_Kg1ntwTS(10)
346500100202     c                   eval      wadKG_11aP = ap_Kg1ntwTS(11)
346600100202     c                   eval      wadKG_12aP = ap_Kg1ntwTS(12)
346700100201     C***  Ricavi Reali mensili
346800100202     C                   Z-ADD     ap_R1ntwTS(1) wadRR_01aP
346900100202     C                   Z-ADD     ap_R1ntwTS(2) wadRR_02aP
347000100202     C                   Z-ADD     ap_R1ntwTS(3) wadRR_03aP
347100100202     C                   Z-ADD     ap_R1ntwTS(4) wadRR_04aP
347200100202     C                   Z-ADD     ap_R1ntwTS(5) wadRR_05aP
347300100202     C                   Z-ADD     ap_R1ntwTS(6) wadRR_06aP
347400100202     C                   Z-ADD     ap_R1ntwTS(7) wadRR_07aP
347500100202     C                   Z-ADD     ap_R1ntwTS(8) wadRR_08aP
347600100202     C                   Z-ADD     ap_R1ntwTS(9) wadRR_09aP
347700100202     C                   Z-ADD     ap_R1ntwTS(10)wadRR_10aP
347800100202     C                   Z-ADD     ap_R1ntwTS(11)wadRR_11aP
347900100202     C                   Z-ADD     ap_R1ntwTS(12)wadRR_12aP
348000100201     C***  Ricavi Presunti mensili
348100100202     C                   Z-ADD     ap_P1ntwTS(1) wadRP_01aP
348200100202     C                   Z-ADD     ap_P1ntwTS(2) wadRP_02aP
348300100202     C                   Z-ADD     ap_P1ntwTS(3) wadRP_03aP
348400100202     C                   Z-ADD     ap_P1ntwTS(4) wadRP_04aP
348500100202     C                   Z-ADD     ap_P1ntwTS(5) wadRP_05aP
348600100202     C                   Z-ADD     ap_P1ntwTS(6) wadRP_06aP
348700100202     C                   Z-ADD     ap_P1ntwTS(7) wadRP_07aP
348800100202     C                   Z-ADD     ap_P1ntwTS(8) wadRP_08aP
348900100202     C                   Z-ADD     ap_P1ntwTS(9) wadRP_09aP
349000100202     C                   Z-ADD     ap_P1ntwTS(10)wadRP_10aP
349100100202     C                   Z-ADD     ap_P1ntwTS(11)wadRP_11aP
349200100202     C                   Z-ADD     ap_P1ntwTS(12)wadRP_12aP
349300100201     C*--------------------
349400100201     C* Anno in Corso 12 Mesi
349500100201     C*--------------------
349600100201     C***    Spedizioni mensili
349700100202     C                   Z-ADD     S1ntwTS(1)    wadNS_01aC
349800100202     C                   Z-ADD     S1ntwTS(2)    wadNS_02aC
349900100202     C                   Z-ADD     S1ntwTS(3)    wadNS_03aC
350000100202     C                   Z-ADD     S1ntwTS(4)    wadNS_04aC
350100100202     C                   Z-ADD     S1ntwTS(5)    wadNS_05aC
350200100202     C                   Z-ADD     S1ntwTS(6)    wadNS_06aC
350300100202     C                   Z-ADD     S1ntwTS(7)    wadNS_07aC
350400100202     C                   Z-ADD     S1ntwTS(8)    wadNS_08aC
350500100202     C                   Z-ADD     S1ntwTS(9)    wadNS_09aC
350600100202     C                   Z-ADD     S1ntwTS(10)   wadNS_10aC
350700100202     C                   Z-ADD     S1ntwTS(11)   wadNS_11aC
350800100202     C                   Z-ADD     S1ntwTS(12)   wadNS_12aC
350900100201     C***    Pesi mensili
351000100201     c                   clear                   WadkgC
351100100202     c                   add       Kg1ntwTS(1)   WadkgC
351200100202     c                   add       Kg1ntwTS(2)   WadkgC
351300100202     c                   add       Kg1ntwTS(3)   WadkgC
351400100202     c                   add       Kg1ntwTS(4)   WadkgC
351500100202     c                   add       Kg1ntwTS(5)   WadkgC
351600100202     c                   add       Kg1ntwTS(6)   WadkgC
351700100202     c                   add       Kg1ntwTS(7)   WadkgC
351800100202     c                   add       Kg1ntwTS(8)   WadkgC
351900100202     c                   add       Kg1ntwTS(9)   WadkgC
352000100202     c                   add       Kg1ntwTS(10)  WadkgC
352100100202     c                   add       Kg1ntwTS(11)  WadkgC
352200100202     c                   add       Kg1ntwTS(12)  WadkgC
352300100202     C                   Z-ADD     Kg1ntwTS(1)   wadKG_01aC
352400100202     C                   Z-ADD     Kg1ntwTS(2)   wadKG_02aC
352500100202     C                   Z-ADD     Kg1ntwTS(3)   wadKG_03aC
352600100202     C                   Z-ADD     Kg1ntwTS(4)   wadKG_04aC
352700100202     C                   Z-ADD     Kg1ntwTS(5)   wadKG_05aC
352800100202     C                   Z-ADD     Kg1ntwTS(6)   wadKG_06aC
352900100202     C                   Z-ADD     Kg1ntwTS(7)   wadKG_07aC
353000100202     C                   Z-ADD     Kg1ntwTS(8)   wadKG_08aC
353100100202     C                   Z-ADD     Kg1ntwTS(9)   wadKG_09aC
353200100202     C                   Z-ADD     Kg1ntwTS(10)  wadKG_10aC
353300100202     C                   Z-ADD     Kg1ntwTS(11)  wadKG_11aC
353400100202     C                   Z-ADD     Kg1ntwTS(12)  wadKG_12aC
353500100201     C***  Ricavi Reali mensili
353600100202     C                   Z-ADD     R1ntwTS(1)    wadRR_01aC
353700100202     C                   Z-ADD     R1ntwTS(2)    wadRR_02aC
353800100202     C                   Z-ADD     R1ntwTS(3)    wadRR_03aC
353900100202     C                   Z-ADD     R1ntwTS(4)    wadRR_04aC
354000100202     C                   Z-ADD     R1ntwTS(5)    wadRR_05aC
354100100202     C                   Z-ADD     R1ntwTS(6)    wadRR_06aC
354200100202     C                   Z-ADD     R1ntwTS(7)    wadRR_07aC
354300100202     C                   Z-ADD     R1ntwTS(8)    wadRR_08aC
354400100202     C                   Z-ADD     R1ntwTS(9)    wadRR_09aC
354500100202     C                   Z-ADD     R1ntwTS(10)   wadRR_10aC
354600100202     C                   Z-ADD     R1ntwTS(11)   wadRR_11aC
354700100202     C                   Z-ADD     R1ntwTS(12)   wadRR_12aC
354800100201     C***  Ricavi Presunti mensili
354900100202     C                   Z-ADD     P1ntwTS(1)    wadRP_01aC
355000100202     C                   Z-ADD     P1ntwTS(2)    wadRP_02aC
355100100202     C                   Z-ADD     P1ntwTS(3)    wadRP_03aC
355200100202     C                   Z-ADD     P1ntwTS(4)    wadRP_04aC
355300100202     C                   Z-ADD     P1ntwTS(5)    wadRP_05aC
355400100202     C                   Z-ADD     P1ntwTS(6)    wadRP_06aC
355500100202     C                   Z-ADD     P1ntwTS(7)    wadRP_07aC
355600100202     C                   Z-ADD     P1ntwTS(8)    wadRP_08aC
355700100202     C                   Z-ADD     P1ntwTS(9)    wadRP_09aC
355800100202     C                   Z-ADD     P1ntwTS(10)   wadRP_10aC
355900100202     C                   Z-ADD     P1ntwTS(11)   wadRP_11aC
356000100202     C                   Z-ADD     P1ntwTS(12)   wadRP_12aC
356100100204      *  i campi delle Varie verranno aggiunti in seguito solo sul file di Totale
356200100202      *
356300100202     C                   WRITE     WADEAPC0
356400100201     C                   ENDdo
356500100201     C*
356600100201     C                   ENDSR
356700091123     C*------------------------------------------------------------------------*
356800020221     C*?STAAGE - IMPOSTA STAMPA TOTALI AGENTE COMMERCIALE
356900000000     C*------------------------------------------------------------------------*
357000000000     C     STAAGE        BEGSR
357100011001     C*
357200011001     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI COMMERCIALI
357300011001 FG  C                   IF        PARSIN = 'S'
357400011001     C                   SETON                                        53
357500011105     C                   ELSE
357600011105     C                   SETOFF                                       53
357700011001     C                   ENDIF
357800000000     C*
357900000000     C* AZZERA I TOTALI NELLA FORMA CLI, SPE, RIC, DEL
358000000000     C                   CLEAR                   CLW                            *NUMERO CLIENTI
358100000000     C                   CLEAR                   SPW                            *SPEDIZIONI
358200000000     C                   CLEAR                   RRW                            *RICAVO REALE
358300000000     C                   CLEAR                   RPW                            *RICAVO PRESUNTO
358400000000     C                   CLEAR                   CTW                            *13° TOTALE CLEINTI
358500000000     C*
358600020220     C* CHIUDO IL PROSPETTO DEL DETTAGLIO CLIENTI
358700000000IF  1C     ORAKSC        IFEQ      '1'
358800100129     c                   if        parSTAMPA <> 'N'
358900020116     C                   WRITE     SE94R2
359000100129     c                   end
359100000000     C                   MOVEL     '0'           ORAKSC                         *ORA NON STAMPA CLI
359200000000E   1C                   ENDIF
359300000000     C*
359400000000     C* STAMPA I VALORI DELL'AGENTE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
359500020318     C* LA STATISTICA DELTA CON DETTAGLIO FILIALE O CON DETTAGLIO AREA
359600000000IF  1C     PASAGE        IFEQ      '1'
359700000000IF  2C     PARSTA        IFEQ      'K'
359800000000     C     PARSTA        OREQ      'C'
359900000000     C***
360000000000     C* DESCRIZIONI INTESTAZIONE
360100000000     C***
360200020116     C                   CLEAR                   SE94IN
360300000000     C                   MOVEL     DESAGE        DES25            25            *DESCRIZIONE
360400000000     C                   MOVEL     DE2PCF        DES04             4            *PERCENTUALE
360500000000IF  3C     DE2PCF        IFGT      *BLANKS
360600000000     C                   MOVE      '%'           DES04
360700000000X   3C                   ELSE
360800000000     C                   MOVE      ' '           DES04
360900000000E   3C                   ENDIF
361000000000IF  3C     PARSTA        IFEQ      'K'
361100000000     C                   Z-ADD     DEPAGE        INCAGE                         *AGENTE
361200000000     C                   MOVEL     DES25         INDAGE                         *DESCRIZ + PERCEN
361300000000     C                   MOVE      DES04         INDAGE
361400000000X   3C                   ELSE
361500000000     C                   Z-ADD     *ZEROS        INCAGE
361600000000     C                   MOVEL     DRI(1)        INDAGE
361700000000E   3C                   ENDIF
361800000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
361900000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
362000000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
362100000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
362200000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
362300000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
362400950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
362500030924     C*
362600030924     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
362700030924     C                   MOVEL     DRI(21)       PIDDES                         *RETTANGOLINO
362800030924     C*
362900030924     C* SE DELTA FILIALE STAMPA I TOTALI AGENTE NELLA FORMA: TOT A, TOT B, TOT C
363000000000IF  3C     PARSTA        IFEQ      'K'
363100020222     C*
363200020221     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
363300020221DO  4C     1             DO        4             Q
363400000000     C     Q             OCCUR     ABCCOM
363500000000     C     Q             OCCUR     ABCSTA
363600000000     C                   Z-ADD     CNC           CNS
363700000000     C                   Z-ADD     SNC           SNS
363800000000     C                   Z-ADD     RNC           RNS
363900000000     C                   Z-ADD     PNC           PNS
364000030924     C                   Z-ADD     CAC           CAS
364100030924     C                   Z-ADD     SAC           SAS
364200030924     C                   Z-ADD     RAC           RAS
364300030924     C                   Z-ADD     PAC           PAS
364400000000     C*
364500000000     C* NUMERO TOTALE CLIENTI
364600000000     C     Q             OCCUR     ABCC13
364700000000     C     Q             OCCUR     ABCS13
364800000000     C                   Z-ADD     NTC           NTS
364900000000     C                   Z-ADD     ATC           ATS
365000000000     C*
365100000000E   4C                   ENDDO
365200020220     C*
365300020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA X CLIENTI 'D'
365400020220     C     5             OCCUR     ABCCOM
365500020220     C     5             OCCUR     ABCSTA
365600020220     C                   Z-ADD     CNC           CNS
365700020220     C                   Z-ADD     SNC           SNS
365800020220     C                   Z-ADD     RNC           RNS
365900020220     C                   Z-ADD     PNC           PNS
366000020220     C                   Z-ADD     CAC           CAS
366100020220     C                   Z-ADD     SAC           SAS
366200020220     C                   Z-ADD     RAC           RAS
366300020220     C                   Z-ADD     PAC           PAS
366400020220     C*
366500020220     C* NUMERO TOTALE CLIENTI 'D'
366600020220     C     5             OCCUR     ABCC13
366700020220     C     5             OCCUR     ABCS13
366800020220     C                   Z-ADD     NTC           NTS
366900020220     C                   Z-ADD     ATC           ATS
367000020318     C*
367100020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
367200020318DO  4C     1             DO        5             K
367300020318     C     K             OCCUR     ABCCOMN
367400020318     C     K             OCCUR     ABCSTAN
367500020318     C                   Z-ADD     CNCN          CNSN
367600020318     C                   Z-ADD     SNCN          SNSN
367700020318     C                   Z-ADD     RNCN          RNSN
367800020318     C                   Z-ADD     PNCN          PNSN
367900020318     C                   Z-ADD     CACN          CASN
368000020318     C                   Z-ADD     SACN          SASN
368100020318     C                   Z-ADD     RACN          RASN
368200020318     C                   Z-ADD     PACN          PASN
368300020318     C*
368400020318     C* NUMERO TOTALE CLIENTI
368500020318     C     K             OCCUR     ABCC13N
368600020318     C     K             OCCUR     ABCS13N
368700020318     C                   Z-ADD     NTCN          NTSN
368800020318     C                   Z-ADD     ATCN          ATSN
368900020318     C*
369000020318E   4C                   ENDDO
369100020318     C*
369200020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
369300020318DO  4C     1             DO        5             K
369400020318     C     K             OCCUR     ABCCOMND
369500020318     C     K             OCCUR     ABCSTAND
369600020318     C                   Z-ADD     CNCND         CNSND
369700020318     C                   Z-ADD     SNCND         SNSND
369800020318     C                   Z-ADD     RNCND         RNSND
369900020318     C                   Z-ADD     PNCND         PNSND
370000020318     C                   Z-ADD     CACND         CASND
370100020318     C                   Z-ADD     SACND         SASND
370200020318     C                   Z-ADD     RACND         RASND
370300020318     C                   Z-ADD     PACND         PASND
370400020318     C*
370500020318     C* NUMERO TOTALE CLIENTI
370600020318     C     K             OCCUR     ABCC13ND
370700020318     C     K             OCCUR     ABCS13ND
370800020318     C                   Z-ADD     NTCND         NTSND
370900020318     C                   Z-ADD     ATCND         ATSND
371000020318     C*
371100020318E   4C                   ENDDO
371200020220     C*
371300020220     C* ROUTINE DI STAMPA TOTALI
371400020220     C                   EXSR      STAMPA
371500000000     C*
371600000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
371700000000     C                   SETON                                        01
371800000000     C                   MOVEL     '0'           PRIGIR
371900000000     C*
372000000000     C* SE DELTA AREA STAMPA I TOTALI AGENTE NELLA FORMA: CLI, SPE, RIC, DEL
372100000000X   3C                   ELSE
372200020222     C*
372300020318     C* IMPOSTO IL FLAG CHE MI IDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
372400020318     C                   EVAL      DEPSTPD = '1'
372500020318     C*
372600000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
372700020318DO  4C     1             DO        4             Q
372800020318     C     Q             OCCUR     ABCCOM
372900020318     C                   ADD       CNC           CLW                            *NUOVI A, B, C +
373000020318     C                   ADD       SNC           SPW
373100020318     C                   ADD       RNC           RRW
373200020318     C                   ADD       PNC           RPW
373300020318     C                   ADD       CAC           CLW                            *ACQUISITI A, B , C
373400020318     C                   ADD       SAC           SPW
373500020318     C                   ADD       RAC           RRW
373600020318     C                   ADD       PAC           RPW
373700000000     C*
373800020318     C* NUMERO TOTALE CLIENTI
373900020318     C     Q             OCCUR     ABCC13
374000020318     C                   ADD       NTC           CTW
374100020318     C                   ADD       ATC           CTW
374200020318     C
374300020318E   4C                   ENDDO
374400020221     C*
374500000000     C                   MOVEL     DEPAGE        DES07             7            *AGENTE
374600000000     C                   MOVEL     DESAGE        DES12            12            *DESCRIZIONE
374700000000     C*
374800000000     C                   MOVEL     *BLANKS       DES20            20
374900000000     C                   MOVEL     DES07         DES20            20
375000000000     C                   MOVE      DES12         DES20
375100010710     C*
375200010710     C* ROUTINE DI STAMPA TOTALI
375300030926     c                   MOVEL     'A'           WTipStamp2                     *da STAAGE
375400020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
375500020222     C*
375600020220     C* STAMPO I 'D' SOLO SE EFFETTIVAMENTE ESISTENTI
375700020220     C                   IF        FLGSIDE = '1'
375800020221     C*
375900020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
376000020222     C                   EVAL      DEPSTPD = '2'
376100020220     C*
376200020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
376300020318     C     5             OCCUR     ABCCOM
376400050217     C****               ADD       CNC           CLW                            *NUOVI A, B, C +
376500050217     C****               ADD       SNC           SPW
376600050217     C****               ADD       RNC           RRW
376700050217     C****               ADD       PNC           RPW
376800050217     C****               ADD       CAC           CLW                            *ACQUISITI A, B , C
376900050217     C****               ADD       SAC           SPW
377000050217     C****               ADD       RAC           RRW
377100050217     C****               ADD       PAC           RPW
377200050217     C****
377300050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
377400050217     C****  i direzionali ma riportava la somma di tutti
377500050217     C     CNC           ADD       CAC           CLW                            *NUOVI+ACQUISITI (D)
377600050217     C     SNC           ADD       SAC           SPW
377700050217     C     RNC           ADD       RAC           RRW
377800050217     C     PNC           ADD       PAC           RPW
377900050217     C*
378000020220     C* NUMERO TOTALE CLIENTI CLIENTI 'D'
378100020318     C     5             OCCUR     ABCC13
378200050217     C****               ADD       NTC           CTW
378300050217     C****               ADD       ATC           CTW
378400050217     C****
378500050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
378600050217     C****  i direzionali ma riportava la somma di tutti
378700050217     C     NTC           ADD       ATC           CTW
378800020220     C*
378900020220     C* ROUTINE DI STAMPA TOTALI
379000030926     c                   MOVEL     'A'           WTipStamp2                     *da STAAGE
379100020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
379200020318     C                   ENDIF
379300000000     C*
379400000000     C* STAMPO RIGA DI SEPARAZIONE
379500100129     c                   if        parSTAMPA <> 'N'
379600020116     C  N53              WRITE     SE94R2
379700100129     c                   end
379800011001E   3C                   ENDIF
379900000000E   2C                   ENDIF
380000000000E   1C                   ENDIF
380100011001     C*
380200000000     C                   ENDSR
380300030924     C*------------------------------------------------------------------------*
380400030924     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
380500030924     C*------------------------------------------------------------------------*
380600030924     C     STAFIL        BEGSR
380700030924     C*
380800030924     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI FILIALE
380900030924     C                   IF        PARSIN = 'S'
381000030924     C                   SETON                                        53
381100030924     C                   ELSE
381200030924     C                   SETOFF                                       53
381300030924     C                   ENDIF
381400030924     C*
381500000000     C* AZZERA I TOTALI NELLA FORMA CLI, SPE, RIC, DEL
381600000000     C                   CLEAR                   CLW                            *NUMERO CLIENTI
381700000000     C                   CLEAR                   SPW                            *SPEDIZIONI
381800000000     C                   CLEAR                   RRW                            *RICAVO REALE
381900000000     C                   CLEAR                   RPW                            *RICAVO PRESUNTO
382000000000     C                   CLEAR                   CTW                            *13° TOTALE CLEINTI
382100000000     C*
382200000000     C* STAMPA VALORI DELLA FILIALE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
382300000000     C* LA STATISTICA DELTA CON DETTAGLIO CLIENTE/COMMERCIALE/FILIALE (SEMPRE)
382400000000IF  1C     PASFIL        IFEQ      '1'
382500000000IF  2C     PARSTA        IFEQ      'K'
382600000000     C     PARSTA        OREQ      'C'
382700000000     C     PARSTA        OREQ      'F'
382800000000     C***
382900000000     C* DESCRIZIONI INTESTAZIONE
383000000000     C***
383100020116     C                   CLEAR                   SE94IN
383200000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
383300000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
383400000000IF  3C     PARSTA        IFEQ      'K'
383500020221     C     PARSTA        OREQ      'C'
383600000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
383700000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
383800000000X   3C                   ELSE
383900000000     C                   Z-ADD     *ZEROS        INCFIL
384000000000     C                   MOVEL     DRI(2)        INDFIL
384100000000E   3C                   ENDIF
384200000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
384300000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
384400000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
384500000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
384600030924     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
384700030924     C*
384800030924     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
384900030924     C                   MOVEL     DRI(22)       PIDDES                         *RETTANGOLINO
385000030924     C*
385100030924     C* SE DELTA FILIALE O DELTA AREA STAMPA I TOTALI FILIALE NELLA FORMA:
385200030924     C* TOT A, TOT B, TOT C
385300000000IF  3C     PARSTA        IFEQ      'K'
385400000000     C     PARSTA        OREQ      'C'
385500000000     C*
385600000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
385700020221DO  4C     1             DO        4             Q
385800000000     C     Q             OCCUR     ABCFIL
385900000000     C     Q             OCCUR     ABCSTA
386000000000     C                   Z-ADD     CNF           CNS
386100000000     C                   Z-ADD     SNF           SNS
386200000000     C                   Z-ADD     RNF           RNS
386300000000     C                   Z-ADD     PNF           PNS
386400000000     C                   Z-ADD     CAF           CAS
386500000000     C                   Z-ADD     SAF           SAS
386600000000     C                   Z-ADD     RAF           RAS
386700000000     C                   Z-ADD     PAF           PAS
386800000000     C*
386900000000     C* NUMERO TOTALE CLIENTI
387000000000     C     Q             OCCUR     ABCF13
387100000000     C     Q             OCCUR     ABCS13
387200000000     C                   Z-ADD     NTF           NTS
387300000000     C                   Z-ADD     ATF           ATS
387400000000     C*
387500000000E   4C                   ENDDO
387600020220     C*
387700020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
387800020220     C     5             OCCUR     ABCFIL
387900020220     C     5             OCCUR     ABCSTA
388000020220     C                   Z-ADD     CNF           CNS
388100020220     C                   Z-ADD     SNF           SNS
388200020220     C                   Z-ADD     RNF           RNS
388300020220     C                   Z-ADD     PNF           PNS
388400020220     C                   Z-ADD     CAF           CAS
388500020220     C                   Z-ADD     SAF           SAS
388600020220     C                   Z-ADD     RAF           RAS
388700020220     C                   Z-ADD     PAF           PAS
388800020220     C*
388900020220     C* NUMERO TOTALE CLIENTI 'D'
389000020220     C     5             OCCUR     ABCF13
389100020220     C     5             OCCUR     ABCS13
389200020220     C                   Z-ADD     NTF           NTS
389300020220     C                   Z-ADD     ATF           ATS
389400020318     C*
389500020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
389600020318DO  4C     1             DO        5             K
389700020318     C     K             OCCUR     ABCFILN
389800020318     C     K             OCCUR     ABCSTAN
389900020318     C                   Z-ADD     CNFN          CNSN
390000020318     C                   Z-ADD     SNFN          SNSN
390100020318     C                   Z-ADD     RNFN          RNSN
390200020318     C                   Z-ADD     PNFN          PNSN
390300020318     C                   Z-ADD     CAFN          CASN
390400020318     C                   Z-ADD     SAFN          SASN
390500020318     C                   Z-ADD     RAFN          RASN
390600020318     C                   Z-ADD     PAFN          PASN
390700020318     C*
390800020318     C* NUMERO TOTALE CLIENTI
390900020318     C     K             OCCUR     ABCF13N
391000020318     C     K             OCCUR     ABCS13N
391100020318     C                   Z-ADD     NTFN          NTSN
391200020318     C                   Z-ADD     ATFN          ATSN
391300020318     C*
391400020318E   4C                   ENDDO
391500020318     C*
391600020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
391700020318DO  4C     1             DO        5             K
391800020318     C     K             OCCUR     ABCFILND
391900020318     C     K             OCCUR     ABCSTAND
392000020318     C                   Z-ADD     CNFND         CNSND
392100020318     C                   Z-ADD     SNFND         SNSND
392200020318     C                   Z-ADD     RNFND         RNSND
392300020318     C                   Z-ADD     PNFND         PNSND
392400020318     C                   Z-ADD     CAFND         CASND
392500020318     C                   Z-ADD     SAFND         SASND
392600020318     C                   Z-ADD     RAFND         RASND
392700020318     C                   Z-ADD     PAFND         PASND
392800020318     C*
392900020318     C* NUMERO TOTALE CLIENTI
393000020318     C     K             OCCUR     ABCF13ND
393100020318     C     K             OCCUR     ABCS13ND
393200020318     C                   Z-ADD     NTFND         NTSND
393300020318     C                   Z-ADD     ATFND         ATSND
393400020318     C*
393500020318E   4C                   ENDDO
393600020220     C*
393700020220     C* ROUTINE DI STAMPA TOTALI
393800020220     C                   EXSR      STAMPA
393900000000     C*
394000000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
394100000000     C                   SETON                                        01
394200000000     C                   MOVEL     '0'           PRIGIR
394300000000     C*
394400000000     C* SE DELTA DIVISIONE STAMPA I TOTALI FILIALE NELLA FORMA: CLI,SPE,RIC,DEL
394500000000X   3C                   ELSE
394600020221     C*
394700020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
394800020222     C                   EVAL      DEPSTPD = '1'
394900000000     C*
395000000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
395100020318DO  4C     1             DO        4             Q
395200020318     C     Q             OCCUR     ABCFIL
395300050217     C                   ADD       CNF           CLW                            *NUOVI A, B, C +
395400050217     C                   ADD       SNF           SPW
395500050217     C                   ADD       RNF           RRW
395600050217     C                   ADD       PNF           RPW
395700050217     C                   ADD       CAF           CLW                            *ACQUISITI A, B , C
395800050217     C                   ADD       SAF           SPW
395900050217     C                   ADD       RAF           RRW
396000050217     C                   ADD       PAF           RPW
396100050217     C*
396200000000     C* NUMERO TOTALE CLIENTI
396300020318     C     Q             OCCUR     ABCF13
396400050217     C                   ADD       NTF           CTW
396500050217     C                   ADD       ATF           CTW
396600050217     C*
396700020318E   4C                   ENDDO
396800020221     C*
396900000000     C                   MOVEL     DEPFIL        DES03             3            *FILIALE
397000000000     C                   MOVEL     DESFIL        DES16            16            *DESCRIZIONE
397100000000     C*
397200000000     C                   MOVEL     *BLANKS       DES20            20
397300000000     C                   MOVEL     DES03         DES20            20
397400000000     C                   MOVE      DES16         DES20
397500010710     C*
397600010710     C* ROUTINE DI STAMPA TOTALI
397700030926     c                   MOVEL     'F'           WTipStamp2                     *da STAAGE
397800020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
397900020220     C*
398000020220     C* STAMPO I 'D' SOLO SE EFFETTIVAMENTE ESISTENTI
398100020220     C                   IF        FLGSIDF = '1'
398200020221     C*
398300020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
398400020222     C                   EVAL      DEPSTPD = '2'
398500020220     C*
398600020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
398700020318     C     5             OCCUR     ABCFIL
398800050217     C****               ADD       CNF           CLW                            *NUOVI A, B, C +
398900050217     C****               ADD       SNF           SPW
399000050217     C****               ADD       RNF           RRW
399100050217     C****               ADD       PNF           RPW
399200050217     C****               ADD       CAF           CLW                            *ACQUISITI A, B , C
399300050217     C****               ADD       SAF           SPW
399400050217     C****               ADD       RAF           RRW
399500050217     C****               ADD       PAF           RPW
399600050217     C****
399700050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
399800050217     C****  i direzionali ma riportava la somma di tutti
399900050217     C     CNF           ADD       CAF           CLW                            *NUOVI+ACQUISITI (D)
400000050217     C     SNF           ADD       SAF           SPW
400100050217     C     RNF           ADD       RAF           RRW
400200050217     C     PNF           ADD       PAF           RPW
400300050217     C*
400400020220     C* NUMERO TOTALE CLIENTI 'D'
400500020318     C     5             OCCUR     ABCF13
400600050217     C****               ADD       NTF           CTW
400700050217     C****               ADD       ATF           CTW
400800050217     C****
400900050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
401000050217     C****  i direzionali ma riportava la somma di tutti
401100050217     C     NTF           ADD       ATF           CTW
401200050217     C*
401300020220     C* ROUTINE DI STAMPA TOTALI
401400030926     c                   MOVEL     'F'           WTipStamp2                     *da STAAGE
401500020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
401600020318     C                   ENDIF
401700000000     C*
401800000000     C* STAMPO RIGA DI SEPARAZIONE
401900100129     c                   if        parSTAMPA <> 'N'
402000020116     C  N53              WRITE     SE94R2
402100100129     c                   end
402200000000E   3C                   ENDIF
402300000000E   2C                   ENDIF
402400000000E   1C                   ENDIF
402500010710     C*
402600000000     C                   ENDSR
402700000000     C*------------------------------------------------------------------------*
402800000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
402900000000     C*------------------------------------------------------------------------*
403000000000     C     STAARE        BEGSR
403100020215     C*
403200020215     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI AREA
403300020215 FG  C                   IF        PARSIN = 'S'
403400020215     C                   SETON                                        53
403500020215     C                   ELSE
403600020215     C                   SETOFF                                       53
403700020215     C                   ENDIF
403800000000     C*
403900000000     C* STAMPA I VALORI DELL'AREA SE HO MEMORIZZATO QUALCOSA E SE STO ELABORANDO
404000020318     C* LA STATISTICA DELTA CON DETTAGLIO AREA O CON DETTAGLIO DIVISIONE
404100000000IF  1C     PASARE        IFEQ      '1'
404200000000IF  2C     PARSTA        IFEQ      'C'
404300000000     C     PARSTA        OREQ      'F'
404400000000     C***
404500000000     C* DESCRIZIONI INTESTAZIONE
404600000000     C***
404700020116     C                   CLEAR                   SE94IN
404800000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
404900000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
405000000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
405100000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
405200000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
405300000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
405400000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
405500000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
405600950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
405700000000     C*
405800000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
405900000000     C                   MOVEL     DRI(23)       PIDDES                         *RETTANGOLINO
406000000000     C*
406100000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
406200020221DO  3C     1             DO        4             Q
406300000000     C     Q             OCCUR     ABCARE
406400000000     C     Q             OCCUR     ABCSTA
406500000000     C                   Z-ADD     CNA           CNS
406600000000     C                   Z-ADD     SNA           SNS
406700000000     C                   Z-ADD     RNA           RNS
406800000000     C                   Z-ADD     PNA           PNS
406900000000     C                   Z-ADD     CAA           CAS
407000000000     C                   Z-ADD     SAA           SAS
407100000000     C                   Z-ADD     RAA           RAS
407200000000     C                   Z-ADD     PAA           PAS
407300000000     C*
407400000000     C* NUMERO TOTALE CLIENTI
407500000000     C     Q             OCCUR     ABCA13
407600000000     C     Q             OCCUR     ABCS13
407700000000     C                   Z-ADD     NTA           NTS
407800000000     C                   Z-ADD     ATA           ATS
407900000000     C*
408000000000E   3C                   ENDDO
408100020220     C*
408200020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
408300020220     C     5             OCCUR     ABCARE
408400020220     C     5             OCCUR     ABCSTA
408500020220     C                   Z-ADD     CNA           CNS
408600020220     C                   Z-ADD     SNA           SNS
408700020220     C                   Z-ADD     RNA           RNS
408800020220     C                   Z-ADD     PNA           PNS
408900020220     C                   Z-ADD     CAA           CAS
409000020220     C                   Z-ADD     SAA           SAS
409100020220     C                   Z-ADD     RAA           RAS
409200020220     C                   Z-ADD     PAA           PAS
409300020220     C*
409400020220     C* NUMERO TOTALE CLIENTI 'D'
409500020220     C     5             OCCUR     ABCA13
409600020220     C     5             OCCUR     ABCS13
409700020220     C                   Z-ADD     NTA           NTS
409800020220     C                   Z-ADD     ATA           ATS
409900020318     C*
410000020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
410100020318DO  3C     1             DO        5             K
410200020318     C     K             OCCUR     ABCAREN
410300020318     C     K             OCCUR     ABCSTAN
410400020318     C                   Z-ADD     CNAN          CNSN
410500020318     C                   Z-ADD     SNAN          SNSN
410600020318     C                   Z-ADD     RNAN          RNSN
410700020318     C                   Z-ADD     PNAN          PNSN
410800020318     C                   Z-ADD     CAAN          CASN
410900020318     C                   Z-ADD     SAAN          SASN
411000020318     C                   Z-ADD     RAAN          RASN
411100020318     C                   Z-ADD     PAAN          PASN
411200020318     C*
411300020318     C* NUMERO TOTALE CLIENTI
411400020318     C     K             OCCUR     ABCA13N
411500020318     C     K             OCCUR     ABCS13N
411600020318     C                   Z-ADD     NTAN          NTSN
411700020318     C                   Z-ADD     ATAN          ATSN
411800020318     C*
411900020318E   3C                   ENDDO
412000020318     C*
412100020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
412200020318DO  3C     1             DO        5             K
412300020318     C     K             OCCUR     ABCAREND
412400020318     C     K             OCCUR     ABCSTAND
412500020318     C                   Z-ADD     CNAND         CNSND
412600020318     C                   Z-ADD     SNAND         SNSND
412700020318     C                   Z-ADD     RNAND         RNSND
412800020318     C                   Z-ADD     PNAND         PNSND
412900020318     C                   Z-ADD     CAAND         CASND
413000020318     C                   Z-ADD     SAAND         SASND
413100020318     C                   Z-ADD     RAAND         RASND
413200020318     C                   Z-ADD     PAAND         PASND
413300020318     C*
413400020318     C* NUMERO TOTALE CLIENTI
413500020318     C     K             OCCUR     ABCA13ND
413600020318     C     K             OCCUR     ABCS13ND
413700020318     C                   Z-ADD     NTAND         NTSND
413800020318     C                   Z-ADD     ATAND         ATSND
413900020318     C*
414000020318E   3C                   ENDDO
414100020220     C*
414200020220     C* ROUTINE DI STAMPA TOTALI
414300020220     C                   EXSR      STAMPA
414400000000     C*
414500000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
414600000000     C                   SETON                                        01
414700000000     C                   MOVEL     '0'           PRIGIR
414800000000     C*
414900000000E   2C                   ENDIF
415000000000E   1C                   ENDIF
415100000000     C*
415200000000     C                   ENDSR
415300000000     C*------------------------------------------------------------------------*
415400000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
415500000000     C*------------------------------------------------------------------------*
415600000000     C     STADIV        BEGSR
415700020215     C*
415800020215     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI DIVISIONE
415900020215 FG  C                   IF        PARSIN = 'S'
416000020215     C                   SETON                                        53
416100020215     C                   ELSE
416200020215     C                   SETOFF                                       53
416300020215     C                   ENDIF
416400000000     C*
416500020318     C* STAMPA I VALORI DELLA DIVISIONE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
416600020318     C* LA STATISTICA DELTA CON DETTAGLIO DIVISIONE
416700000000IF  1C     PASDIV        IFEQ      '1'
416800000000     C     PARSTA        ANDEQ     'F'
416900000000     C***
417000000000     C* DESCRIZIONI INTESTAZIONE
417100000000     C***
417200020116     C                   CLEAR                   SE94IN
417300000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
417400000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
417500000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
417600000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
417700000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
417800000000     C                   MOVEL     DRI(2)        INDARE                         *DESCRIZIONE AREA
417900000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
418000000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
418100950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
418200000000     C*
418300000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
418400000000     C                   MOVEL     DRI(24)       PIDDES
418500000000     C*
418600000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
418700020221DO  2C     1             DO        4             Q
418800000000     C     Q             OCCUR     ABCDIV
418900000000     C     Q             OCCUR     ABCSTA
419000000000     C                   Z-ADD     CND           CNS
419100000000     C                   Z-ADD     SND           SNS
419200000000     C                   Z-ADD     RND           RNS
419300000000     C                   Z-ADD     PND           PNS
419400000000     C                   Z-ADD     CAD           CAS
419500000000     C                   Z-ADD     SAD           SAS
419600000000     C                   Z-ADD     RAD           RAS
419700000000     C                   Z-ADD     PAD           PAS
419800000000     C*
419900000000     C* NUMERO TOTALE CLIENTI
420000000000     C     Q             OCCUR     ABCD13
420100000000     C     Q             OCCUR     ABCS13
420200000000     C                   Z-ADD     NTD           NTS
420300000000     C                   Z-ADD     ATD           ATS
420400000000     C*
420500000000E   2C                   ENDDO
420600020220     C*
420700020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
420800020220     C     5             OCCUR     ABCDIV
420900020220     C     5             OCCUR     ABCSTA
421000020220     C                   Z-ADD     CND           CNS
421100020220     C                   Z-ADD     SND           SNS
421200030924     C                   Z-ADD     RND           RNS
421300030924     C                   Z-ADD     PND           PNS
421400030924     C                   Z-ADD     CAD           CAS
421500030924     C                   Z-ADD     SAD           SAS
421600020220     C                   Z-ADD     RAD           RAS
421700030924     C                   Z-ADD     PAD           PAS
421800030924     C*
421900030924     C* NUMERO TOTALE CLIENTI
422000030924     C     5             OCCUR     ABCD13
422100030924     C     5             OCCUR     ABCS13
422200030924     C                   Z-ADD     NTD           NTS
422300030924     C                   Z-ADD     ATD           ATS
422400030924     C*
422500030924     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
422600020318DO  2C     1             DO        5             K
422700030924     C     K             OCCUR     ABCDIVN
422800030924     C     K             OCCUR     ABCSTAN
422900030924     C                   Z-ADD     CNDN          CNSN
423000030924     C                   Z-ADD     SNDN          SNSN
423100030924     C                   Z-ADD     RNDN          RNSN
423200030924     C                   Z-ADD     PNDN          PNSN
423300030924     C                   Z-ADD     CADN          CASN
423400020318     C                   Z-ADD     SADN          SASN
423500020318     C                   Z-ADD     RADN          RASN
423600020318     C                   Z-ADD     PADN          PASN
423700020318     C*
423800020318     C* NUMERO TOTALE CLIENTI
423900020318     C     K             OCCUR     ABCD13N
424000020318     C     K             OCCUR     ABCS13N
424100020318     C                   Z-ADD     NTDN          NTSN
424200020318     C                   Z-ADD     ATDN          ATSN
424300020318     C*
424400020318E   2C                   ENDDO
424500020318     C*
424600020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
424700020318DO  2C     1             DO        5             K
424800020318     C     K             OCCUR     ABCDIVND
424900020318     C     K             OCCUR     ABCSTAND
425000020318     C                   Z-ADD     CNDND         CNSND
425100020318     C                   Z-ADD     SNDND         SNSND
425200020318     C                   Z-ADD     RNDND         RNSND
425300020318     C                   Z-ADD     PNDND         PNSND
425400020318     C                   Z-ADD     CADND         CASND
425500020318     C                   Z-ADD     SADND         SASND
425600020318     C                   Z-ADD     RADND         RASND
425700020318     C                   Z-ADD     PADND         PASND
425800020318     C*
425900020318     C* NUMERO TOTALE CLIENTI
426000020318     C     K             OCCUR     ABCD13ND
426100020318     C     K             OCCUR     ABCS13ND
426200020318     C                   Z-ADD     NTDND         NTSND
426300020318     C                   Z-ADD     ATDND         ATSND
426400020318     C*
426500020318E   2C                   ENDDO
426600020220     C*
426700020220     C* ROUTINE DI STAMPA TOTALI
426800020220     C                   EXSR      STAMPA
426900000000     C*
427000000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
427100000000     C                   SETON                                        01
427200000000     C                   MOVEL     '0'           PRIGIR
427300000000     C*
427400000000E   1C                   ENDIF
427500000000     C*
427600000000     C                   ENDSR
427700000000     C*------------------------------------------------------------------------*
427800000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
427900000000     C*------------------------------------------------------------------------*
428000000000     C     STAGEN        BEGSR
428100000000     C*
428200000000     C* STAMPA VALORI GENERALI SOLO SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
428300020318     C* LA STATISTICA DELTA CON DETTAGLIO DIVISIONE
428400000000IF  1C     PASGEN        IFEQ      '1'
428500000000     C     PARSTA        ANDEQ     'F'
428600000000     C***
428700000000     C* DESCRIZIONI INTESTAZIONE
428800000000     C***
428900020116     C                   CLEAR                   SE94IN
429000000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
429100000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
429200000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
429300000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
429400000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
429500000000     C                   MOVEL     DRI(2)        INDARE                         *DESCRIZIONE AREA
429600000000     C                   MOVEL     *BLANKS       INCDIV                         *DIVISIONE
429700000000     C                   MOVEL     DRI(2)        INDDIV                         DESCRIZIONE DIVISION
429800950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
429900000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
430000000000     C                   MOVEL     DRI(25)       PIDDES
430100000000     C*
430200000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
430300020221DO  2C     1             DO        4             Q
430400000000     C     Q             OCCUR     ABCGEN
430500000000     C     Q             OCCUR     ABCSTA
430600000000     C                   Z-ADD     CNG           CNS
430700000000     C                   Z-ADD     SNG           SNS
430800000000     C                   Z-ADD     RNG           RNS
430900000000     C                   Z-ADD     PNG           PNS
431000000000     C                   Z-ADD     CAG           CAS
431100000000     C                   Z-ADD     SAG           SAS
431200000000     C                   Z-ADD     RAG           RAS
431300000000     C                   Z-ADD     PAG           PAS
431400000000     C*
431500000000     C* NUMERO TOTALE CLIENTI
431600000000     C     Q             OCCUR     ABCG13
431700000000     C     Q             OCCUR     ABCS13
431800000000     C                   Z-ADD     NTG           NTS
431900000000     C                   Z-ADD     ATG           ATS
432000000000     C*
432100000000E   2C                   ENDDO
432200020220     C*
432300020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
432400020220     C     5             OCCUR     ABCGEN
432500020220     C     5             OCCUR     ABCSTA
432600020220     C                   Z-ADD     CNG           CNS
432700020220     C                   Z-ADD     SNG           SNS
432800020220     C                   Z-ADD     RNG           RNS
432900020220     C                   Z-ADD     PNG           PNS
433000020220     C                   Z-ADD     CAG           CAS
433100020220     C                   Z-ADD     SAG           SAS
433200020220     C                   Z-ADD     RAG           RAS
433300020220     C                   Z-ADD     PAG           PAS
433400020220     C*
433500020220     C* NUMERO TOTALE CLIENTI 'D'
433600020220     C     5             OCCUR     ABCG13
433700020220     C     5             OCCUR     ABCS13
433800020220     C                   Z-ADD     NTG           NTS
433900020220     C                   Z-ADD     ATG           ATS
434000020318     C*
434100020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
434200020318DO  2C     1             DO        5             K
434300020318     C     K             OCCUR     ABCGENN
434400020318     C     K             OCCUR     ABCSTAN
434500020318     C                   Z-ADD     CNGN          CNSN
434600020318     C                   Z-ADD     SNGN          SNSN
434700020318     C                   Z-ADD     RNGN          RNSN
434800020318     C                   Z-ADD     PNGN          PNSN
434900020318     C                   Z-ADD     CAGN          CASN
435000020318     C                   Z-ADD     SAGN          SASN
435100020318     C                   Z-ADD     RAGN          RASN
435200020318     C                   Z-ADD     PAGN          PASN
435300020318     C*
435400020318     C* NUMERO TOTALE CLIENTI
435500020318     C     K             OCCUR     ABCG13N
435600020318     C     K             OCCUR     ABCS13N
435700020318     C                   Z-ADD     NTGN          NTSN
435800020318     C                   Z-ADD     ATGN          ATSN
435900020318     C*
436000020318E   2C                   ENDDO
436100020318     C*
436200020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
436300020318DO  2C     1             DO        5             K
436400020318     C     K             OCCUR     ABCGENND
436500020318     C     K             OCCUR     ABCSTAND
436600020318     C                   Z-ADD     CNGND         CNSND
436700020318     C                   Z-ADD     SNGND         SNSND
436800020318     C                   Z-ADD     RNGND         RNSND
436900020318     C                   Z-ADD     PNGND         PNSND
437000020318     C                   Z-ADD     CAGND         CASND
437100020318     C                   Z-ADD     SAGND         SASND
437200020318     C                   Z-ADD     RAGND         RASND
437300020318     C                   Z-ADD     PAGND         PASND
437400020318     C*
437500020318     C* NUMERO TOTALE CLIENTI
437600020318     C     K             OCCUR     ABCG13ND
437700020318     C     K             OCCUR     ABCS13ND
437800020318     C                   Z-ADD     NTGND         NTSND
437900020318     C                   Z-ADD     ATGND         ATSND
438000020318     C*
438100020318E   2C                   ENDDO
438200020220     C*
438300020220     C* ROUTINE DI STAMPA TOTALI
438400020220     C                   EXSR      STAMPA
438500000000     C*
438600000000E   1C                   ENDIF
438700000000     C*
438800000000     C                   ENDSR
438900000000     C*------------------------------------------------------------------------*
439000000000     C*?STAMPA - STAMPA I TOTALI NELLA FORMA: TOT "A" / TOT "B" / TOT "C"
439100000000     C*------------------------------------------------------------------------*
439200000000     C     STAMPA        BEGSR
439300020221     C*--------------------
439400020221     C* I N T E S T A Z I O N E
439500020221     C*--------------------
439600020221     C                   EXSR      STAINT
439700000000     C*
439800000000     C* PULISCO I TOTALI A+B+C
439900000000     C                   CLEAR                   CNW
440000000000     C                   CLEAR                   SNW
440100000000     C                   CLEAR                   RNW
440200000000     C                   CLEAR                   PNW
440300000000     C                   CLEAR                   CAW
440400000000     C                   CLEAR                   SAW
440500000000     C                   CLEAR                   RAW
440600000000     C                   CLEAR                   PAW
440700000000     C*
440800020318     C                   Z-ADD     *ZEROS        CNW13C           11 0          *PERIODO CORRENTE
440900000000     C                   Z-ADD     *ZEROS        CAW13C           11 0
441000951227     C                   Z-ADD     *ZEROS        CN213C           11 0          *PERIODO PRECEDENTE
441100951227     C                   Z-ADD     *ZEROS        CA213C           11 0
441200000000     C*
441300980521DO  1C     1             DO        4             Q
441400000000     C     Q             OCCUR     ABCSTA
441500000000     C***
441600020221     C* TOTALI "A" + "B" + "C"  DIVISI IN NUOVI E ACQUISITI
441700000000     C***
441800000000     C                   ADD       CNS           CNW
441900000000     C                   ADD       SNS           SNW
442000000000     C                   ADD       RNS           RNW
442100000000     C                   ADD       PNS           PNW
442200000000     C                   ADD       CAS           CAW
442300000000     C                   ADD       SAS           SAW
442400000000     C                   ADD       RAS           RAW
442500000000     C                   ADD       PAS           PAW
442600951227     C*
442700000000     C     Q             OCCUR     ABCS13
442800951227     C* CORRENTE
442900000000     C                   ADD       NTS(1)        CNW13C
443000000000     C                   ADD       ATS(1)        CAW13C
443100951227     C* PRECEDENTE
443200951227     C                   ADD       NTS(2)        CN213C
443300011105     C                   ADD       ATS(2)        CA213C
443400000000     C*
443500000000E   1C                   ENDDO
443600020221     C*
443700000000     C***
443800000000     C* CLIENTI  N U O V I
443900000000     C***
444000980521DO  1C     1             DO        4             Q
444100000000     C     Q             OCCUR     ABCSTA
444200000000     C*
444300000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
444400000000IF  2C     *IN01         IFEQ      *ON
444500020215     C                   EXSR      STAINT
444600000000E   2C                   ENDIF
444700020222     C*
444800020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
444900020222     C                   EVAL      DEPSTPD = '0'
445000000000     C*
445100000000     C                   XFOOT     CNS           COMCLI           16 0          *NUOVI DI CATEGORIA
445200000000IF  2C     COMCLI        IFGT      *ZEROS
445300000000     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
445400000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
445500020318     C                   MOVEL     '0'           FLGSTPNTW
445600000000     C                   Z-ADD     CNS           $CL                            NUMERO CLIENTI
445700000000     C                   Z-ADD     SNS           $SP                            SPEDIZIONI
445800000000     C                   Z-ADD     RNS           $RR                            RICAVI REALI
445900000000     C                   Z-ADD     PNS           $RP                            RICAVI PRESUNTI
446000000000     C*
446100000000     C     Q             OCCUR     ABCS13
446200951227     C                   Z-ADD     NTS(1)        $CL13C           11 0          N° TOT CLIENTI CORR
446300951227     C                   Z-ADD     NTS(2)        $C213C           11 0          N° TOT CLIENTI PREC
446400000000     C*
446500000000     C                   EXSR      STMDAT
446600000000E   2C                   ENDIF
446700000000     C*
446800000000E   1C                   ENDDO
446900000000     C*
447000020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
447100020318     C                   IF        PARSTA = 'K'
447200000000     C* T O T A L I   N U O V I
447300000000     C                   Z-ADD     CNW           $CL                            NUMERO CLIENTI
447400000000     C                   Z-ADD     SNW           $SP                            SPEDIZIONI
447500000000     C                   Z-ADD     RNW           $RR                            RICAVI REALI
447600000000     C                   Z-ADD     PNW           $RP                            RICAVI PRESUNTI
447700951227     C                   Z-ADD     CNW13C        $CL13C                         N° TOT CLIENTI CORR
447800951227     C                   Z-ADD     CN213C        $C213C                         N° TOT CLIENTI PREC
447900020222     C*
448000020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
448100020222IF  2C     *IN01         IFEQ      *ON
448200020222     C                   EXSR      STAINT
448300020222E   2C                   ENDIF
448400020222     C*
448500020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
448600020222     C                   EVAL      DEPSTPD = '1'
448700000000     C*
448800020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
448900000000IF  1C     COMCLI        IFGT      *ZEROS
449000000000     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
449100020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
449200020221     C                   EXSR      STMDAT
449300100129     c                   if        parSTAMPA <> 'N'
449400020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
449500100129     c                   end
449600020221E   1C                   ENDIF
449700020318     C*
449800020318     C                   ELSE
449900020318     C* T O T A L I   N U O V I
450000020318     C                   Z-ADD     CNW           $CL                            NUMERO CLIENTI
450100020318     C                   Z-ADD     SNW           $SP                            SPEDIZIONI
450200020318     C                   Z-ADD     RNW           $RR                            RICAVI REALI
450300020318     C                   Z-ADD     PNW           $RP                            RICAVI PRESUNTI
450400020318     C                   Z-ADD     CNW13C        $CL13C                         N° TOT CLIENTI CORR
450500020318     C                   Z-ADD     CN213C        $C213C                         N° TOT CLIENTI PREC
450600020318     C*
450700020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
450800020318IF  2C     *IN01         IFEQ      *ON
450900020318     C                   EXSR      STAINT
451000020318E   2C                   ENDIF
451100020318     C*
451200020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
451300020318     C                   EVAL      DEPSTPD = '1'
451400020318     C*
451500020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
451600020318IF  1C     COMCLI        IFGT      *ZEROS
451700020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
451800020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
451900020318     C                   EXSR      STMDAT
452000100129     c                   if        parSTAMPA <> 'N'
452100020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
452200100129     c                   end
452300020318E   1C                   ENDIF
452400020318     C***
452500020404     C* T O T A L I   N U O V I   X   N E T W O R K
452600020318     C***
452700020404     C*
452800020404     C* SOLO SE RICHIESTO NEL LANCIO
452900020404     C                   IF        PARNTW = 'S'
453000020404     C*
453100020319     C                   MOVEL     '0'           FLGSTPRI          1
453200020319     C*
453300020319DO  1C     2             DO        5             K
453400020318     C     K             OCCUR     ABCSTAN
453500020318     C*
453600020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
453700020318IF  2C     *IN01         IFEQ      *ON
453800020318     C                   EXSR      STAINT
453900020318E   2C                   ENDIF
454000020318     C*
454100020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
454200020318     C                   EVAL      DEPSTPD = '1'
454300020318     C*
454400020318     C                   XFOOT     CNSN          COMCLI           16 0          *NUOVI DI CATEGORIA
454500020318IF  2C     COMCLI        IFGT      *ZEROS
454600020319     C                   MOVEL     '1'           FLGSTPRI
454700020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
454800020318     C                   Z-ADD     K             $Q                             NETWORK
454900020318     C                   MOVEL     '1'           FLGSTPNTW
455000020318     C                   Z-ADD     CNSN          $CL                            NUMERO CLIENTI
455100020318     C                   Z-ADD     SNSN          $SP                            SPEDIZIONI
455200020318     C                   Z-ADD     RNSN          $RR                            RICAVI REALI
455300020318     C                   Z-ADD     PNSN          $RP                            RICAVI PRESUNTI
455400020318     C*
455500020318     C     K             OCCUR     ABCS13N
455600020318     C                   Z-ADD     NTSN(1)       $CL13C           11 0          N° TOT CLIENTI CORR
455700020318     C                   Z-ADD     NTSN(2)       $C213C           11 0          N° TOT CLIENTI PREC
455800020318     C*
455900020318     C                   EXSR      STMDAT
456000020318E   2C                   ENDIF
456100020318     C*
456200020318E   1C                   ENDDO
456300020319     C                   IF        FLGSTPRI = '1'
456400100129     c                   if        parSTAMPA <> 'N'
456500020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
456600100129     c                   end
456700020318     C                   ENDIF
456800020319     C                   ENDIF
456900020404     C*
457000020404     C                   ENDIF
457100020221     C*
457200020221     C* PULISCO I TOTALI D
457300020221     C                   CLEAR                   CNWD
457400020221     C                   CLEAR                   SNWD
457500020221     C                   CLEAR                   RNWD
457600020221     C                   CLEAR                   PNWD
457700020221     C                   CLEAR                   CAWD
457800020221     C                   CLEAR                   SAWD
457900020221     C                   CLEAR                   RAWD
458000020221     C                   CLEAR                   PAWD
458100020221     C*
458200020318     C                   Z-ADD     *ZEROS        CNW13CD          11 0          *PERIODO CORRENTE
458300020221     C                   Z-ADD     *ZEROS        CAW13CD          11 0
458400020221     C                   Z-ADD     *ZEROS        CN213CD          11 0          *PERIODO PRECEDENTE
458500020221     C                   Z-ADD     *ZEROS        CA213CD          11 0
458600020221     C*
458700020221     C     5             OCCUR     ABCSTA
458800020221     C***
458900020221     C* TOTALI "D" DIVISI IN NUOVI E ACQUISITI
459000020221     C***
459100020221     C                   ADD       CNS           CNWD
459200020221     C                   ADD       SNS           SNWD
459300020221     C                   ADD       RNS           RNWD
459400020221     C                   ADD       PNS           PNWD
459500020221     C                   ADD       CAS           CAWD
459600020221     C                   ADD       SAS           SAWD
459700020221     C                   ADD       RAS           RAWD
459800020221     C                   ADD       PAS           PAWD
459900020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
460000020221     C                   ADD       CNW           CNWD
460100020221     C                   ADD       SNW           SNWD
460200020221     C                   ADD       RNW           RNWD
460300020221     C                   ADD       PNW           PNWD
460400020221     C                   ADD       CAW           CAWD
460500020221     C                   ADD       SAW           SAWD
460600020221     C                   ADD       RAW           RAWD
460700020221     C                   ADD       PAW           PAWD
460800020221     C*
460900020221     C     5             OCCUR     ABCS13
461000020221     C* CORRENTE
461100020221     C                   ADD       NTS(1)        CNW13CD
461200020221     C                   ADD       ATS(1)        CAW13CD
461300020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
461400020221     C                   ADD       CNW13C        CNW13CD
461500020221     C                   ADD       CAW13C        CAW13CD
461600020221     C* PRECEDENTE
461700020221     C                   ADD       NTS(2)        CN213CD
461800020221     C                   ADD       ATS(2)        CA213CD
461900020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
462000020221     C                   ADD       CN213C        CN213CD
462100020221     C                   ADD       CA213C        CA213CD
462200020221     C***
462300020221     C* CLIENTI  N U O V I  'D'
462400020221     C***
462500020221     C     5             OCCUR     ABCSTA
462600020221     C*
462700020221     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
462800020221IF  2C     *IN01         IFEQ      *ON
462900020221     C                   EXSR      STAINT
463000020221E   2C                   ENDIF
463100020222     C*
463200020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
463300020222     C                   EVAL      DEPSTPD = '0'
463400020221     C*
463500020221     C                   XFOOT     CNS           COMCLI           16 0          *NUOVI DI CATEGORIA
463600020221IF  2C     COMCLI        IFGT      *ZEROS
463700020221     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
463800020221     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
463900020318     C                   MOVEL     '0'           FLGSTPNTW
464000020221     C                   Z-ADD     CNS           $CL                            NUMERO CLIENTI
464100020221     C                   Z-ADD     SNS           $SP                            SPEDIZIONI
464200020221     C                   Z-ADD     RNS           $RR                            RICAVI REALI
464300020221     C                   Z-ADD     PNS           $RP                            RICAVI PRESUNTI
464400020221     C*
464500020221     C     5             OCCUR     ABCS13
464600020221     C                   Z-ADD     NTS(1)        $CL13C           11 0          N° TOT CLIENTI CORR
464700020221     C                   Z-ADD     NTS(2)        $C213C           11 0          N° TOT CLIENTI PREC
464800020221     C*
464900020221     C                   EXSR      STMDAT
465000020221E   2C                   ENDIF
465100020318     C*
465200020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
465300020318     C                   IF        PARSTA = 'K'
465400020221     C* T O T A L I   N U O V I   'D'
465500020221     C                   Z-ADD     CNWD          $CL                            NUMERO CLIENTI
465600020221     C                   Z-ADD     SNWD          $SP                            SPEDIZIONI
465700020221     C                   Z-ADD     RNWD          $RR                            RICAVI REALI
465800020221     C                   Z-ADD     PNWD          $RP                            RICAVI PRESUNTI
465900020221     C                   Z-ADD     CNW13CD       $CL13C                         N° TOT CLIENTI CORR
466000020221     C                   Z-ADD     CN213CD       $C213C                         N° TOT CLIENTI PREC
466100020222     C*
466200020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
466300020222IF  2C     *IN01         IFEQ      *ON
466400020222     C                   EXSR      STAINT
466500020222E   2C                   ENDIF
466600030311     C*
466700030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
466800070821     C*****              EVAL      DEPSTPD = '2'
466900070821     C                   EVAL      DEPSTPD = '3'
467000020221     C*
467100020221IF  1C     COMCLI        IFGT      *ZEROS
467200020221     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
467300020221     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
467400020221     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
467500020221     C                   EXSR      STMDAT
467600100129     c                   if        parSTAMPA <> 'N'
467700020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
467800100129     c                   end
467900020221E   1C                   ENDIF
468000020318     C*
468100020318     C                   ELSE
468200020318     C* T O T A L I   N U O V I   'D'
468300020318     C                   Z-ADD     CNWD          $CL                            NUMERO CLIENTI
468400020318     C                   Z-ADD     SNWD          $SP                            SPEDIZIONI
468500020318     C                   Z-ADD     RNWD          $RR                            RICAVI REALI
468600020318     C                   Z-ADD     PNWD          $RP                            RICAVI PRESUNTI
468700020318     C                   Z-ADD     CNW13CD       $CL13C                         N° TOT CLIENTI CORR
468800020318     C                   Z-ADD     CN213CD       $C213C                         N° TOT CLIENTI PREC
468900020318     C*
469000020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
469100020318IF  2C     *IN01         IFEQ      *ON
469200020318     C                   EXSR      STAINT
469300020318E   2C                   ENDIF
469400030311     C*
469500030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
469600070821     C******             EVAL      DEPSTPD = '2'
469700070821     C                   EVAL      DEPSTPD = '3'
469800020318     C*
469900020318IF  1C     COMCLI        IFGT      *ZEROS
470000020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
470100020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
470200020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
470300020318     C                   EXSR      STMDAT
470400100129     c                   if        parSTAMPA <> 'N'
470500020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
470600100129     c                   end
470700020318E   1C                   ENDIF
470800020318     C***
470900020318     C* T O T A L I   N U O V I   "D"   X   N E T W O R K
471000020318     C***
471100020404     C*
471200020404     C* SOLO SE RICHIESTO NEL LANCIO
471300020404     C                   IF        PARNTW = 'S'
471400020404     C*
471500020319     C                   MOVEL     '0'           FLGSTPRI          1
471600020319DO  1C     2             DO        5             K
471700020318     C     K             OCCUR     ABCSTAND
471800020318     C*
471900020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
472000020318IF  2C     *IN01         IFEQ      *ON
472100020318     C                   EXSR      STAINT
472200020318E   2C                   ENDIF
472300020318     C*
472400020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
472500020318     C                   EVAL      DEPSTPD = '2'
472600020318     C*
472700020318     C                   XFOOT     CNSND         COMCLI           16 0          *NUOVI DI CATEGORIA
472800020318IF  2C     COMCLI        IFGT      *ZEROS
472900020319     C                   MOVEL     '1'           FLGSTPRI
473000020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
473100020318     C                   Z-ADD     K             $Q                             NETWORK
473200020318     C                   MOVEL     '1'           FLGSTPNTW
473300020318     C                   Z-ADD     CNSND         $CL                            NUMERO CLIENTI
473400020318     C                   Z-ADD     SNSND         $SP                            SPEDIZIONI
473500020318     C                   Z-ADD     RNSND         $RR                            RICAVI REALI
473600020318     C                   Z-ADD     PNSND         $RP                            RICAVI PRESUNTI
473700020318     C*
473800020318     C     K             OCCUR     ABCS13ND
473900020318     C                   Z-ADD     NTSND(1)      $CL13C           11 0          N° TOT CLIENTI CORR
474000020318     C                   Z-ADD     NTSND(2)      $C213C           11 0          N° TOT CLIENTI PREC
474100020318     C*
474200020318     C                   EXSR      STMDAT
474300020318E   2C                   ENDIF
474400020318     C*
474500020318E   1C                   ENDDO
474600020319     C                   IF        FLGSTPRI = '1'
474700100129     c                   if        parSTAMPA <> 'N'
474800020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
474900100129     c                   end
475000020318     C                   ENDIF
475100020319     C                   ENDIF
475200020221     C*
475300020404     C                   ENDIF
475400020404     C*
475500020221     C***
475600020318     C* CLIENTI  A C Q U I S I T I
475700000000     C***
475800020221     C*
475900980521DO  1C     1             DO        4             Q
476000000000     C     Q             OCCUR     ABCSTA
476100000000     C*
476200000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
476300000000IF  2C     *IN01         IFEQ      *ON
476400020215     C                   EXSR      STAINT
476500000000E   2C                   ENDIF
476600020222     C*
476700020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
476800020222     C                   EVAL      DEPSTPD = '0'
476900000000     C*
477000020116     C                   XFOOT     CAS           COMCLI                         *NUOVI DI CATEGORIA
477100000000IF  2C     COMCLI        IFGT      *ZEROS
477200000000     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
477300000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
477400020318     C                   MOVEL     '0'           FLGSTPNTW
477500000000     C                   Z-ADD     CAS           $CL                            NUMERO CLIENTI
477600000000     C                   Z-ADD     SAS           $SP                            SPEDIZIONI
477700000000     C                   Z-ADD     RAS           $RR                            RICAVI REALI
477800000000     C                   Z-ADD     PAS           $RP                            RICAVI PRESUNTI
477900000000     C*
478000000000     C     Q             OCCUR     ABCS13
478100020116     C                   Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
478200020116     C                   Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
478300000000     C                   EXSR      STMDAT
478400000000E   2C                   ENDIF
478500000000     C*
478600000000E   1C                   ENDDO
478700020318     C*
478800020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
478900020318     C                   IF        PARSTA = 'K'
479000000000     C* T O T A L I   A C Q U I S I T I
479100000000     C                   Z-ADD     CAW           $CL                            NUMERO CLIENTI
479200000000     C                   Z-ADD     SAW           $SP                            SPEDIZIONI
479300000000     C                   Z-ADD     RAW           $RR                            RICAVI REALI
479400000000     C                   Z-ADD     PAW           $RP                            RICAVI PRESUNTI
479500951227     C                   Z-ADD     CAW13C        $CL13C                         N° TOT CLIENTI CORR
479600951227     C                   Z-ADD     CA213C        $C213C                         N° TOT CLIENTI PREC
479700020222     C*
479800020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
479900020222IF  2C     *IN01         IFEQ      *ON
480000020222     C                   EXSR      STAINT
480100020222E   2C                   ENDIF
480200020222     C*
480300020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
480400020222     C                   EVAL      DEPSTPD = '1'
480500000000     C*
480600020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
480700000000IF  1C     COMCLI        IFGT      *ZEROS
480800000000     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
480900000000     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
481000000000     C                   EXSR      STMDAT
481100100129     c                   if        parSTAMPA <> 'N'
481200020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
481300100129     c                   end
481400020221E   1C                   ENDIF
481500020318     C*
481600020318     C                   ELSE
481700020318     C* T O T A L I   A C Q U I S I T I
481800020318     C                   Z-ADD     CAW           $CL                            NUMERO CLIENTI
481900020318     C                   Z-ADD     SAW           $SP                            SPEDIZIONI
482000020318     C                   Z-ADD     RAW           $RR                            RICAVI REALI
482100020318     C                   Z-ADD     PAW           $RP                            RICAVI PRESUNTI
482200020318     C                   Z-ADD     CAW13C        $CL13C                         N° TOT CLIENTI CORR
482300020318     C                   Z-ADD     CA213C        $C213C                         N° TOT CLIENTI PREC
482400020318     C*
482500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
482600020318IF  2C     *IN01         IFEQ      *ON
482700020318     C                   EXSR      STAINT
482800020318E   2C                   ENDIF
482900020318     C*
483000020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
483100020318     C                   EVAL      DEPSTPD = '1'
483200020318     C*
483300020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
483400020318IF  1C     COMCLI        IFGT      *ZEROS
483500050308IF  2C     $c213c        orGT      *ZEROS
483600020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
483700020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
483800020318     C                   EXSR      STMDAT
483900100129     c                   if        parSTAMPA <> 'N'
484000020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
484100100129     c                   end
484200020318E   1C                   ENDIF
484300020318     C***
484400020318     C* T O T A L I   A C Q I S I T I   X   N E T W O R K
484500020318     C***
484600020404     C*
484700020404     C* SOLO SE RICHIESTO NEL LANCIO
484800020404     C                   IF        PARNTW = 'S'
484900020404     C*
485000020319     C                   MOVEL     '0'           FLGSTPRI          1
485100020319     C*
485200020319DO  1C     2             DO        5             K
485300020318     C     K             OCCUR     ABCSTAN
485400020318     C*
485500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
485600020318IF  2C     *IN01         IFEQ      *ON
485700020318     C                   EXSR      STAINT
485800020318E   2C                   ENDIF
485900020318     C*
486000020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
486100020318     C                   EVAL      DEPSTPD = '1'
486200050309     C     K             OCCUR     ABCS13N
486300050309     C                   Z-ADD     ATSN(1)       $CL13C                         N° TOT CLIENTI CORR
486400050309     C                   Z-ADD     ATSN(2)       $C213C                         N° TOT CLIENTI PREC
486500020318     C*
486600020318     C                   XFOOT     CASN          COMCLI                         *NUOVI DI CATEGORIA
486700020318IF  2C     COMCLI        IFGT      *ZEROS
486800050309IF  2C     $c213c        orGT      *ZEROS
486900020319     C                   MOVEL     '1'           FLGSTPRI
487000020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
487100020318     C                   Z-ADD     K             $Q                             NETWORK
487200020318     C                   MOVEL     '1'           FLGSTPNTW
487300020318     C                   Z-ADD     CASN          $CL                            NUMERO CLIENTI
487400020318     C                   Z-ADD     SASN          $SP                            SPEDIZIONI
487500020318     C                   Z-ADD     RASN          $RR                            RICAVI REALI
487600020318     C                   Z-ADD     PASN          $RP                            RICAVI PRESUNTI
487700050309     C* ???????
487800050309     C**** K             OCCUR     ABCS13N
487900050309     C*********          Z-ADD     ATSN(1)       $CL13C                         N° TOT CLIENTI CORR
488000050309     C*********          Z-ADD     ATSN(2)       $C213C                         N° TOT CLIENTI PREC
488100020318     C                   EXSR      STMDAT
488200020318E   2C                   ENDIF
488300020318     C*
488400020318E   1C                   ENDDO
488500020319     C                   IF        FLGSTPRI = '1'
488600100129     c                   if        parSTAMPA <> 'N'
488700020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
488800100129     c                   end
488900020318     C                   ENDIF
489000020319     C                   ENDIF
489100020404     C*
489200020404     C                   ENDIF
489300020221     C***
489400020221     C* CLIENTI  A C Q U I S I T I  'D'
489500020221     C***
489600020221     C*
489700020221     C     5             OCCUR     ABCSTA
489800020221     C*
489900020221     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
490000020221IF  2C     *IN01         IFEQ      *ON
490100020221     C                   EXSR      STAINT
490200020221E   2C                   ENDIF
490300020222     C*
490400020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
490500020222     C                   EVAL      DEPSTPD = '0'
490600050309     C     5             OCCUR     ABCS13
490700050309     C                   Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
490800050309     C                   Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
490900020221     C*
491000020221     C                   XFOOT     CAS           COMCLI                         *NUOVI DI CATEGORIA
491100020221IF  2C     COMCLI        IFGT      *ZEROS
491200050309IF  2C     $c213c        orGT      *ZEROS
491300020221     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
491400020221     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
491500020318     C                   MOVEL     '0'           FLGSTPNTW
491600020221     C                   Z-ADD     CAS           $CL                            NUMERO CLIENTI
491700020221     C                   Z-ADD     SAS           $SP                            SPEDIZIONI
491800020221     C                   Z-ADD     RAS           $RR                            RICAVI REALI
491900020221     C                   Z-ADD     PAS           $RP                            RICAVI PRESUNTI
492000050309     C* ???????
492100050309     C**** 5             OCCUR     ABCS13
492200050309     C**********         Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
492300050309     C**********         Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
492400020221     C                   EXSR      STMDAT
492500020221E   2C                   ENDIF
492600020318     C*
492700020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
492800020318     C                   IF        PARSTA = 'K'
492900020221     C* T O T A L I   A C Q U I S I T I  'D'
493000020221     C                   Z-ADD     CAWD          $CL                            NUMERO CLIENTI
493100020221     C                   Z-ADD     SAWD          $SP                            SPEDIZIONI
493200020221     C                   Z-ADD     RAWD          $RR                            RICAVI REALI
493300020221     C                   Z-ADD     PAWD          $RP                            RICAVI PRESUNTI
493400020221     C                   Z-ADD     CAW13CD       $CL13C                         N° TOT CLIENTI CORR
493500020221     C                   Z-ADD     CA213CD       $C213C                         N° TOT CLIENTI PREC
493600020222     C*
493700020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
493800020222IF  2C     *IN01         IFEQ      *ON
493900020222     C                   EXSR      STAINT
494000020222E   2C                   ENDIF
494100020222     C*
494200020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
494300030520     C                   EVAL      DEPSTPD = '3'
494400020221     C*
494500020221IF  1C     COMCLI        IFGT      *ZEROS
494600020221     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
494700020221     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
494800020221     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
494900020221     C                   EXSR      STMDAT
495000100129     c                   if        parSTAMPA <> 'N'
495100020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
495200100129     c                   end
495300020221E   1C                   ENDIF
495400020318     C*
495500020318     C                   ELSE
495600020318     C* T O T A L I   A C Q U I S I T I  'D'
495700020318     C                   Z-ADD     CAWD          $CL                            NUMERO CLIENTI
495800020318     C                   Z-ADD     SAWD          $SP                            SPEDIZIONI
495900020318     C                   Z-ADD     RAWD          $RR                            RICAVI REALI
496000020318     C                   Z-ADD     PAWD          $RP                            RICAVI PRESUNTI
496100020318     C                   Z-ADD     CAW13CD       $CL13C                         N° TOT CLIENTI CORR
496200020318     C                   Z-ADD     CA213CD       $C213C                         N° TOT CLIENTI PREC
496300020318     C*
496400020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
496500020318IF  2C     *IN01         IFEQ      *ON
496600020318     C                   EXSR      STAINT
496700020318E   2C                   ENDIF
496800020318     C*
496900020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
497000030520     C                   EVAL      DEPSTPD = '3'
497100020318     C*
497200050308     C                   xfoot     $cl           comcli                         *NUOVI DI CATEGORIA
497300020318IF  1C     COMCLI        IFGT      *ZEROS
497400050308IF  2C     $c213c        orGT      *ZEROS
497500050308     C********           XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
497600020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
497700020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
497800020318     C                   EXSR      STMDAT
497900100129     c                   if        parSTAMPA <> 'N'
498000020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
498100100129     c                   end
498200020318E   1C                   ENDIF
498300020318     C***
498400050308     C* T O T A L I   A C Q U I S I T I   X   N E T W O R K  "D"
498500020318     C***
498600020404     C*
498700020404     C* SOLO SE RICHIESTO NEL LANCIO
498800020404     C                   IF        PARNTW = 'S'
498900020404     C*
499000020319     C                   MOVEL     '0'           FLGSTPRI          1
499100020319     C*
499200020319DO  1C     2             DO        5             K
499300020318     C     K             OCCUR     ABCSTAND
499400020318     C*
499500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
499600020318IF  2C     *IN01         IFEQ      *ON
499700020318     C                   EXSR      STAINT
499800020318E   2C                   ENDIF
499900020318     C*
500000020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
500100020318     C                   EVAL      DEPSTPD = '2'
500200020318     C*
500300050309     C     K             OCCUR     ABCS13ND
500400050309     C                   Z-ADD     ATSND(1)      $CL13C                         N° TOT CLIENTI CORR
500500050309     C                   Z-ADD     ATSND(2)      $C213C                         N° TOT CLIENTI PREC
500600050308     C*
500700020318     C                   XFOOT     CASND         COMCLI                         *NUOVI DI CATEGORIA
500800020318IF  2C     COMCLI        IFGT      *ZEROS
500900050309IF  2C     $c213c        orGT      *ZEROS
501000020319     C                   MOVEL     '1'           FLGSTPRI
501100020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
501200020318     C                   Z-ADD     K             $Q                             NETWORK
501300020318     C                   MOVEL     '1'           FLGSTPNTW
501400020318     C                   Z-ADD     CASND         $CL                            NUMERO CLIENTI
501500020318     C                   Z-ADD     SASND         $SP                            SPEDIZIONI
501600020318     C                   Z-ADD     RASND         $RR                            RICAVI REALI
501700020318     C                   Z-ADD     PASND         $RP                            RICAVI PRESUNTI
501800050309     C* ???????
501900050309     C**** K             OCCUR     ABCS13ND
502000050309     C********           Z-ADD     ATSND(1)      $CL13C                         N° TOT CLIENTI CORR
502100050309     C********           Z-ADD     ATSND(2)      $C213C                         N° TOT CLIENTI PREC
502200020318     C                   EXSR      STMDAT
502300020318E   2C                   ENDIF
502400020318     C*
502500020318E   1C                   ENDDO
502600020319     C                   IF        FLGSTPRI = '1'
502700100129     c                   if        parSTAMPA <> 'N'
502800020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
502900100129     c                   end
503000020318     C                   ENDIF
503100020319     C                   ENDIF
503200020404     C*
503300020404     C                   ENDIF
503400020221     C*
503500020319     C* PRIMA DEI TOTALI STAMPO SEMPRE E CMQ 1 RIGA DOPPIA X EVIDENZIARE L'INIZIO DEI TOTALI
503600100129     c                   if        parSTAMPA <> 'N'
503700020319     C  N53              WRITE     SE94R3                                       *RIGA SEPARAZIONE DO
503800100129     c                   end
503900020319     C*
504000020318     C***
504100020318     C* CLIENTI  T O T A L I
504200000000     C***
504300020221     C*
504400980521DO  1C     1             DO        4             Q
504500000000     C     Q             OCCUR     ABCSTA
504600000000     C*
504700000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
504800000000IF  2C     *IN01         IFEQ      *ON
504900020215     C                   EXSR      STAINT
505000000000E   2C                   ENDIF
505100020222     C*
505200020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
505300020222     C                   EVAL      DEPSTPD = '0'
505400000000     C*
505500000000     C     CNS           ADD       CAS           $CL                            NUMERO CLIENTI
505600000000     C     SNS           ADD       SAS           $SP                            SPEDIZIONI
505700000000     C     RNS           ADD       RAS           $RR                            RICAVI REALI
505800000000     C     PNS           ADD       PAS           $RP                            RICAVI PRESUNTI
505900000000     C*
506000000000     C     Q             OCCUR     ABCS13
506100020116     C     NTS(1)        ADD       ATS(1)        $CL13C                         N° TO CLIENTI CORR
506200020116     C     NTS(2)        ADD       ATS(2)        $C213C                         N° TO CLIENTI PREC
506300000000     C*
506400020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
506500000000IF  2C     COMCLI        IFGT      *ZEROS
506600050301IF  2C     $c213c        orGT      *ZEROS
506700000000     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
506800000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
506900020318     C                   MOVEL     '0'           FLGSTPNTW
507000000000     C                   EXSR      STMDAT
507100000000E   2C                   ENDIF
507200000000     C*
507300000000E   1C                   ENDDO
507400020318     C*
507500020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
507600020318     C                   IF        PARSTA = 'K'
507700000000     C* T O T A L I   T O T A L I
507800000000     C     CNW           ADD       CAW           $CL                            NUMERO CLIENTI
507900000000     C     SNW           ADD       SAW           $SP                            SPEDIZIONI
508000000000     C     RNW           ADD       RAW           $RR                            RICAVI REALI
508100000000     C     PNW           ADD       PAW           $RP                            RICAVI PRESUNTI
508200951227     C     CNW13C        ADD       CAW13C        $CL13C                         N° TOT CLIENTI CORR
508300951227     C     CN213C        ADD       CA213C        $C213C                         N° TOT CLIENTI CORR
508400020222     C*
508500020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
508600020222IF  2C     *IN01         IFEQ      *ON
508700020222     C                   EXSR      STAINT
508800020222E   2C                   ENDIF
508900020222     C*
509000020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
509100020222     C                   EVAL      DEPSTPD = '1'
509200000000     C*
509300020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
509400000000IF  1C     COMCLI        IFGT      *ZEROS
509500050301IF  2C     $c213c        orGT      *ZEROS
509600000000     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
509700000000     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
509800000000     C                   EXSR      STMDAT
509900100129     c                   if        parSTAMPA <> 'N'
510000020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
510100100129     c                   end
510200020221E   1C                   ENDIF
510300020318     C*
510400020318     C                   ELSE
510500020318     C* T O T A L I   T O T A L I
510600020318     C     CNW           ADD       CAW           $CL                            NUMERO CLIENTI
510700020318     C     SNW           ADD       SAW           $SP                            SPEDIZIONI
510800020318     C     RNW           ADD       RAW           $RR                            RICAVI REALI
510900020318     C     PNW           ADD       PAW           $RP                            RICAVI PRESUNTI
511000020318     C     CNW13C        ADD       CAW13C        $CL13C                         N° TOT CLIENTI CORR
511100020318     C     CN213C        ADD       CA213C        $C213C                         N° TOT CLIENTI CORR
511200020318     C*
511300020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
511400020318IF  2C     *IN01         IFEQ      *ON
511500020318     C                   EXSR      STAINT
511600020318E   2C                   ENDIF
511700020318     C*
511800020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
511900020318     C                   EVAL      DEPSTPD = '1'
512000020318     C*
512100020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
512200020318IF  1C     COMCLI        IFGT      *ZEROS
512300050301IF  2C     $c213c        orGT      *ZEROS
512400020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
512500020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
512600020318     C                   EXSR      STMDAT
512700100129     c                   if        parSTAMPA <> 'N'
512800020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
512900100129     c                   end
513000020318E   1C                   ENDIF
513100020318     C* T O T A L I   T O T A L I   X   N E T W O R K
513200020404     C*
513300020404     C* SOLO SE RICHIESTO NEL LANCIO
513400020404     C                   IF        PARNTW = 'S'
513500020404     C*
513600020319     C                   MOVEL     '0'           FLGSTPRI          1
513700020319     C*
513800020319DO  1C     2             DO        5             K
513900020318     C     K             OCCUR     ABCSTAN
514000020318     C*
514100020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
514200020318IF  2C     *IN01         IFEQ      *ON
514300020318     C                   EXSR      STAINT
514400020318E   2C                   ENDIF
514500020318     C*
514600020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
514700020318     C                   EVAL      DEPSTPD = '1'
514800020318     C*
514900020318     C     CNSN          ADD       CASN          $CL                            NUMERO CLIENTI
515000020318     C     SNSN          ADD       SASN          $SP                            SPEDIZIONI
515100020318     C     RNSN          ADD       RASN          $RR                            RICAVI REALI
515200020318     C     PNSN          ADD       PASN          $RP                            RICAVI PRESUNTI
515300020318     C*
515400020318     C     K             OCCUR     ABCS13N
515500020318     C     NTSN(1)       ADD       ATSN(1)       $CL13C                         N° TO CLIENTI CORR
515600020318     C     NTSN(2)       ADD       ATSN(2)       $C213C                         N° TO CLIENTI PREC
515700020318     C*
515800020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
515900020318IF  2C     COMCLI        IFGT      *ZEROS
516000050301IF  2C     $c213c        orGT      *ZEROS
516100020319     C                   MOVEL     '1'           FLGSTPRI
516200020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
516300020318     C                   Z-ADD     K             $Q                             NETWORK
516400020318     C                   MOVEL     '1'           FLGSTPNTW
516500020318     C                   EXSR      STMDAT
516600020318E   2C                   ENDIF
516700020318     C*
516800020318E   1C                   ENDDO
516900020319     C                   IF        FLGSTPRI = '1'
517000100129     c                   if        parSTAMPA <> 'N'
517100020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
517200100129     c                   end
517300020318     C                   ENDIF
517400020319     C                   ENDIF
517500020404     C*
517600020404     C                   ENDIF
517700020220     C***
517800020220     C* CLIENTI  T O T A L I  'D'
517900020220     C***
518000020221     C*
518100020220     C     5             OCCUR     ABCSTA
518200020220     C*
518300020220     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
518400020220IF  2C     *IN01         IFEQ      *ON
518500020220     C                   EXSR      STAINT
518600020220E   2C                   ENDIF
518700020222     C*
518800020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
518900020222     C                   EVAL      DEPSTPD = '0'
519000020220     C*
519100020220     C     CNS           ADD       CAS           $CL                            NUMERO CLIENTI
519200020220     C     SNS           ADD       SAS           $SP                            SPEDIZIONI
519300020220     C     RNS           ADD       RAS           $RR                            RICAVI REALI
519400020220     C     PNS           ADD       PAS           $RP                            RICAVI PRESUNTI
519500020220     C*
519600020220     C     5             OCCUR     ABCS13
519700020220     C     NTS(1)        ADD       ATS(1)        $CL13C                         N° TO CLIENTI CORR
519800020220     C     NTS(2)        ADD       ATS(2)        $C213C                         N° TO CLIENTI PREC
519900020220     C*
520000020220     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
520100020220IF  2C     COMCLI        IFGT      *ZEROS
520200050301IF  2C     $c213c        orGT      *ZEROS
520300020220     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
520400020220     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
520500020318     C                   MOVEL     '0'           FLGSTPNTW
520600020220     C                   EXSR      STMDAT
520700020220E   2C                   ENDIF
520800020318     C*
520900020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
521000020318     C                   IF        PARSTA = 'K'
521100020221     C* T O T A L I   T O T A L I   'D'
521200020221     C     CNWD          ADD       CAWD          $CL                            NUMERO CLIENTI
521300020221     C     SNWD          ADD       SAWD          $SP                            SPEDIZIONI
521400020221     C     RNWD          ADD       RAWD          $RR                            RICAVI REALI
521500020221     C     PNWD          ADD       PAWD          $RP                            RICAVI PRESUNTI
521600020221     C     CNW13CD       ADD       CAW13CD       $CL13C                         N° TOT CLIENTI CORR
521700020221     C     CN213CD       ADD       CA213CD       $C213C                         N° TOT CLIENTI CORR
521800020222     C*
521900020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
522000020222IF  2C     *IN01         IFEQ      *ON
522100020222     C                   EXSR      STAINT
522200020222E   2C                   ENDIF
522300030311     C*
522400030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D' + TOTALE ALT
522500030311     C                   EVAL      DEPSTPD = '3'
522600020220     C*
522700020221IF  1C     COMCLI        IFGT      *ZEROS
522800050301IF  2C     $c213c        orGT      *ZEROS
522900020220     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
523000020220     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
523100020220     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
523200020220     C                   EXSR      STMDAT
523300100129     c                   if        parSTAMPA <> 'N'
523400020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
523500100129     c                   end
523600020221E   1C                   ENDIF
523700020318     C*
523800020318     C                   ELSE
523900020318     C* T O T A L I   T O T A L I   'D'
524000020318     C     CNWD          ADD       CAWD          $CL                            NUMERO CLIENTI
524100020318     C     SNWD          ADD       SAWD          $SP                            SPEDIZIONI
524200020318     C     RNWD          ADD       RAWD          $RR                            RICAVI REALI
524300020318     C     PNWD          ADD       PAWD          $RP                            RICAVI PRESUNTI
524400020318     C     CNW13CD       ADD       CAW13CD       $CL13C                         N° TOT CLIENTI CORR
524500020318     C     CN213CD       ADD       CA213CD       $C213C                         N° TOT CLIENTI CORR
524600020318     C*
524700020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
524800020318IF  2C     *IN01         IFEQ      *ON
524900020318     C                   EXSR      STAINT
525000020318E   2C                   ENDIF
525100030311     C*
525200030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D' + TOTALE ALT
525300030311     C                   EVAL      DEPSTPD = '3'
525400020318     C*
525500020318IF  1C     COMCLI        IFGT      *ZEROS
525600050301IF  2C     $c213c        orGT      *ZEROS
525700020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
525800020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
525900020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
526000020318     C                   EXSR      STMDAT
526100100129     c                   if        parSTAMPA <> 'N'
526200020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
526300100129     c                   end
526400020318E   1C                   ENDIF
526500020318     C* T O T A L I   T O T A L I   X   N E T W O R K   "D"
526600020404     C*
526700020404     C* SOLO SE RICHIESTO NEL LANCIO
526800020404     C                   IF        PARNTW = 'S'
526900020404     C*
527000020319     C                   MOVEL     '0'           FLGSTPRI          1
527100020319     C*
527200020319DO  1C     2             DO        5             K
527300020318     C     K             OCCUR     ABCSTAND
527400020318     C*
527500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
527600020318IF  2C     *IN01         IFEQ      *ON
527700020318     C                   EXSR      STAINT
527800020318E   2C                   ENDIF
527900020318     C*
528000020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
528100020318     C                   EVAL      DEPSTPD = '2'
528200020318     C*
528300020318     C     CNSND         ADD       CASND         $CL                            NUMERO CLIENTI
528400020318     C     SNSND         ADD       SASND         $SP                            SPEDIZIONI
528500020318     C     RNSND         ADD       RASND         $RR                            RICAVI REALI
528600020318     C     PNSND         ADD       PASND         $RP                            RICAVI PRESUNTI
528700020318     C*
528800020318     C     K             OCCUR     ABCS13ND
528900020318     C     NTSND(1)      ADD       ATSND(1)      $CL13C                         N° TO CLIENTI CORR
529000020318     C     NTSND(2)      ADD       ATSND(2)      $C213C                         N° TO CLIENTI PREC
529100020318     C*
529200020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
529300020318IF  2C     COMCLI        IFGT      *ZEROS
529400050301IF  2C     $C213C        orGT      *ZEROS
529500020319     C                   MOVEL     '1'           FLGSTPRI
529600020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
529700020318     C                   Z-ADD     K             $Q                             NETWORK
529800020318     C                   MOVEL     '1'           FLGSTPNTW
529900020318     C                   EXSR      STMDAT
530000020318E   2C                   ENDIF
530100020318     C*
530200020318E   1C                   ENDDO
530300020319     C                   IF        FLGSTPRI = '1'
530400100129     c                   if        parSTAMPA <> 'N'
530500020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
530600100129     c                   end
530700020318     C                   ENDIF
530800020319     C                   ENDIF
530900000000     C*
531000020404     C                   ENDIF
531100020404     C*
531200000000     C                   ENDSR
531300000000     C*------------------------------------------------------------------------*
531400011123     C*?STAINT - STAMPA INTESTAZIONE
531500000000     C*------------------------------------------------------------------------*
531600000000     C     STAINT        BEGSR
531700020221     C*
531800020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
531900020222     C                   IF        FLGSTPLE = '1'
532000100129     c                   if        parSTAMPA <> 'N'
532100020318     C                   WRITE     SE94R2
532200020222     C                   WRITE     SE94LE                                       *LEGENDA
532300100129     c                   end
532400020222     C                   ELSE
532500020222     C                   EVAL      FLGSTPLE = '1'
532600020222     C                   ENDIF
532700020222     C*
532800100129     c                   if        parSTAMPA <> 'N'
532900020222     C                   WRITE     SE94PT                                       *TESTATA
533000020116     C                   WRITE     SE94IN                                       *RIGA INTESTAZIONE
533100020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
533200020116     C                   WRITE     SE94PI                                       *RIGA MESI
533300020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
533400100129     c                   end
533500000000     C                   SETOFF                                       01
533600000000     C*
533700000000     C                   ENDSR
533800000000     C*------------------------------------------------------------------------*
533900000000     C*?STMDAT - STAMPA I DATI DEL CLIENTE
534000000000     C*          INPUT  --> $CATCL : CATEGORIA CLIENTI (N/A/T)
534100000000     C*                     $Q     : QUALITA' CLIENTI  (A/B/C)
534200000000     C*                     $CL    : NUMERO CLIENTI
534300000000     C*                     $SP    : SPEDIZIONI
534400000000     C*                     $RR    : RICAVI REALI
534500000000     C*                     $RP    : RICAVI PRESUNTI
534600000000     C*                     $CL13C : NUMERO TOTALE CLIENTI (13^ COLONNA)
534700951227     C*                     $C213C : NUMERO TOTALE CLIENTI (1^  COLONNA)
534800000000     C*------------------------------------------------------------------------*
534900000000     C     STMDAT        BEGSR
535000000000     C***
535100000000     C* CLIENTE NUOVI/ACQUISITI/TOTALI DI TIPO A/B/C
535200000000     C***
535300020221     C                   MOVEL     *BLANKS       DES12            12
535400000000IF  1C     $CATCL        IFEQ      'N'                                          *N U O V I
535500000000     C                   MOVEL     DRI(4)        DES12                          *PRIMA DESCRIZIONE
535600000000E   1C                   ENDIF
535700000000IF  1C     $CATCL        IFEQ      'A'                                          *A C Q U I S I T I
535800000000     C                   MOVEL     DRI(8)        DES12                          *PRIMA DESCRIZIONE
535900000000E   1C                   ENDIF
536000000000IF  1C     $CATCL        IFEQ      'T'                                          *T O T A L I
536100000000     C                   MOVEL     DRI(12)       DES12                          *PRIMA DESCRIZIONE
536200000000E   1C                   ENDIF
536300000000     C*
536400020318     C                   IF        FLGSTPNTW = '0'
536500000000IF  2C     $Q            IFGT      *ZEROS
536600000000     C                   MOVE      ABC($Q)       DES12                          *"A"  "B"  "C"  " "
536700000000E   2C                   ENDIF
536800020318     C                   ELSE
536900020318IF  2C     $Q            IFGT      *ZEROS
537000020318     C                   MOVE      DNTW($Q)      DES12                          *NETWORK
537100020318E   2C                   ENDIF
537200020318     C                   ENDIF
537300020221     C*
537400020221     C* SE STO STAMPANDO DATI DI CLIENTI 'D' => VALORIZZO IL CAMPO IN STAMPA X EVIDENZIARLI
537500020222     C                   IF        DEPSTPD = '0'
537600020222     C                   EVAL      P1DFLD = *BLANKS
537700020222     C                   ENDIF
537800020222     C                   IF        DEPSTPD = '1'
537900020222     C                   EVAL      P1DFLD = '*'
538000020222     C                   ENDIF
538100020222     C                   IF        DEPSTPD = '2'
538200020222     C                   EVAL      P1DFLD = '**'
538300020222     C                   ENDIF
538400030311     C                   IF        DEPSTPD = '3'
538500030520     C                   EVAL      P1DFLD = '§'
538600030311     C                   ENDIF
538700000000     C*--------------------
538800020221     C* C L I E N T I
538900000000     C*--------------------
539000000000     C                   MOVEL     DRI(13)       DES07             7            *SECONDA DESCRIZIONE
539100000000     C*
539200000000     C                   Z-ADD     $CL           $VA
539300020116     C                   Z-ADD     $CL13C        $VA13C                         *PERIODO CORRENTE
539400020116     C                   Z-ADD     $C213C        $V213C                         *PERIODO PRECEDENTE
539500000000     C*
539600000000     C                   MOVEL     *BLANKS       P1DDES
539700000000     C                   MOVEL     DES12         P1DDES
539800000000     C                   MOVE      DES07         P1DDES
539900000000     C*
540000000000     C* SCRIVE I VALORI IN STAMPA
540100000000     C                   EXSR      SCRVAL
540200030925     C*
540300030925     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
540400030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
540500030925     C                   MOVEL     PARIOE        SE93IOE                        *DELTA ITALIA/ESTERO
540600030925     C                   MOVEL     INCDIV        SE93DIV                        *DIVISIONE
540700030925     C                   MOVEL     INDDIV        SE93DDI                        *DESCRIZIONE DIVISIO
540800030925     C                   MOVEL     INCARE        SE93ARE                        *AREA
540900030925     C                   MOVEL     INDARE        SE93DAR                        *DESCRIZIONE AREA
541000030925     C                   MOVEL     INCFIL        SE93FIL                        *FILIALE
541100030925     C                   MOVEL     INDFIL        SE93DFI                        *DESCRIZIONE FILIALE
541200030925     C                   MOVEL     P1DFLD        SE93AST                        *CARATTERE STRANO
541300030925     C                   MOVEL     P1DDES        SE93DES                        *DESCRIZIONE
541400030925     C                   MOVEL     *BLANKS       SE93CLV
541500030926IF  2C                   IF        FLGSTPNTW = '0'                              *STAMPA NO NETWORK
541600030926IF  3C     $Q            IFGT      *ZEROS
541700030926     C                   MOVEL     ABC($Q)       SE93CLV                        *QUALITA' CLIENTE
541800030926E   3C                   ENDIF
541900030926E   2C                   ENDIF
542000030926     c*
542100030925     C                   MOVEL     '2'           SE93TIP                        *TIPO TOTALE
542200030925     C                   Z-ADD     $V213C        SE93CPR                        *VALORE PRECEDENTE
542300030925     C                   Z-ADD     $VA(1)        SE93C01                        *MESE UNO
542400030925     C                   Z-ADD     $VA(2)        SE93C02                        *MESE DUE
542500030925     C                   Z-ADD     $VA(3)        SE93C03                        *MESE TRE
542600030925     C                   Z-ADD     $VA(4)        SE93C04                        *MESE QUATTRO
542700030925     C                   Z-ADD     $VA(5)        SE93C05                        *MESE CINQUE
542800030925     C                   Z-ADD     $VA(6)        SE93C06                        *MESE SEI
542900030925     C                   Z-ADD     $VA(7)        SE93C07                        *MESE SETTE
543000030925     C                   Z-ADD     $VA(8)        SE93C08                        *MESE OTTO
543100030925     C                   Z-ADD     $VA(9)        SE93C09                        *MESE NOVE
543200030925     C                   Z-ADD     $VA(10)       SE93C10                        *MESE DIECI
543300030925     C                   Z-ADD     $VA(11)       SE93C11                        *MESE UNDICI
543400030925     C                   Z-ADD     $VA(12)       SE93C12                        *MESE DODICI
543500030925     C                   Z-ADD     $VA13C        SE93CTO                        *TOTALE MESI
543600030925E   1C                   ENDIF
543700000000     C*--------------------
543800000000     C* S P E D I Z I O N I
543900000000     C*--------------------
544000000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIIZONE
544100000000     C                   MOVEL     DRI(14)       DES07                          *SECONDA DESCRIZIONE
544200020221     C                   MOVEL     *BLANKS       P1DFLD
544300000000     C*
544400000000     C                   Z-ADD     $SP           $VA
544500000000     C                   Z-ADD     *ZEROS        $VA13C
544600000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
544700020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPEDIZ
544800000000E   1C                   ENDDO
544900020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
545000951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
545100000000     C*
545200000000     C                   MOVEL     *BLANKS       P1DDES
545300000000     C                   MOVEL     DES12         P1DDES
545400000000     C                   MOVE      DES07         P1DDES
545500000000     C*
545600000000     C* SCRIVE I VALORI IN STAMPA
545700000000     C                   EXSR      SCRVAL
545800030925     C*
545900030925     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
546000030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
546100030925     C                   Z-ADD     $V213C        SE93NPR                        *VALORE PRECEDENTE
546200030925     C                   Z-ADD     $VA(1)        SE93N01                        *MESE UNO
546300030925     C                   Z-ADD     $VA(2)        SE93N02                        *MESE DUE
546400030925     C                   Z-ADD     $VA(3)        SE93N03                        *MESE TRE
546500030925     C                   Z-ADD     $VA(4)        SE93N04                        *MESE QUATTRO
546600030925     C                   Z-ADD     $VA(5)        SE93N05                        *MESE CINQUE
546700030925     C                   Z-ADD     $VA(6)        SE93N06                        *MESE SEI
546800030925     C                   Z-ADD     $VA(7)        SE93N07                        *MESE SETTE
546900030925     C                   Z-ADD     $VA(8)        SE93N08                        *MESE OTTO
547000030925     C                   Z-ADD     $VA(9)        SE93N09                        *MESE NOVE
547100030925     C                   Z-ADD     $VA(10)       SE93N10                        *MESE DIECI
547200030925     C                   Z-ADD     $VA(11)       SE93N11                        *MESE UNDICI
547300030925     C                   Z-ADD     $VA(12)       SE93N12                        *MESE DODICI
547400030925     C                   Z-ADD     $VA13C        SE93NTO                        *TOTALE MESI
547500030925E   1C                   ENDIF
547600000000     C*--------------------
547700020220     C* R I C A V I
547800000000     C*--------------------
547900000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIIZONE
548000000000     C                   MOVEL     DRI(15)       DES07                          *SECONDA DESCRIZIONE
548100020221     C                   MOVEL     *BLANKS       P1DFLD
548200000000     C*
548300020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
548400020116     C*    $RR           DIV       1000          $VA
548500020116     C                   Z-ADD     $RR           $VA
548600951227     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
548700000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
548800020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
548900000000E   1C                   ENDDO
549000020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
549100951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
549200000000     C*
549300000000     C                   MOVEL     *BLANKS       P1DDES
549400000000     C                   MOVEL     DES12         P1DDES
549500000000     C                   MOVE      DES07         P1DDES
549600000000     C*
549700000000     C* SCRIVE I VALORI IN STAMPA
549800000000     C                   EXSR      SCRVAL
549900030925     C*
550000030925     C* IMPOSTA I CAMPI "RICAVI" DEL WFILE DI STAMPA
550100030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
550200030925     C                   Z-ADD     $V213C        SE93RPR                        *VALORE PRECEDENTE
550300030925     C                   Z-ADD     $VA(1)        SE93R01                        *MESE UNO
550400030925     C                   Z-ADD     $VA(2)        SE93R02                        *MESE DUE
550500030925     C                   Z-ADD     $VA(3)        SE93R03                        *MESE TRE
550600030925     C                   Z-ADD     $VA(4)        SE93R04                        *MESE QUATTRO
550700030925     C                   Z-ADD     $VA(5)        SE93R05                        *MESE CINQUE
550800030925     C                   Z-ADD     $VA(6)        SE93R06                        *MESE SEI
550900030925     C                   Z-ADD     $VA(7)        SE93R07                        *MESE SETTE
551000030925     C                   Z-ADD     $VA(8)        SE93R08                        *MESE OTTO
551100030925     C                   Z-ADD     $VA(9)        SE93R09                        *MESE NOVE
551200030925     C                   Z-ADD     $VA(10)       SE93R10                        *MESE DIECI
551300030925     C                   Z-ADD     $VA(11)       SE93R11                        *MESE UNDICI
551400030925     C                   Z-ADD     $VA(12)       SE93R12                        *MESE DODICI
551500030925     C                   Z-ADD     $VA13C        SE93RTO                        *TOTALE MESI
551600030925E   1C                   ENDIF
551700000000     C*--------------------
551800000000     C* D E L T A
551900000000     C*--------------------
552000000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIZIONE
552100000000     C                   MOVEL     DRI(16)       DES07                          *SECONDA DESCRIZIONE
552200020221     C                   MOVEL     *BLANKS       P1DFLD
552300000000     C*
552400000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
552500000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
552600000000     C                   Z-ADD     *ZEROS        $PD13C                         *13^PERCENTUALE DELT
552700000000     C                   EXSR      IMPCAL
552800970822     C***
552900970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
553000970822     C***
553100970822     C                   MOVEL     *BLANKS       $SD                            *FALG SEGNALAZIONE
553200970822     C*
553300000000     C                   MOVEL     *BLANKS       P2DDES
553400020221     C                   MOVEL     *BLANKS       P2DFLD
553500000000     C                   MOVEL     DES12         P2DDES
553600000000     C                   MOVE      DES07         P2DDES
553700000000     C*
553800000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
553900000000     C                   EXSR      SCRDEL
554000030925     C*
554100030925     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
554200030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
554300030925     C                   Z-ADD     $PD(13)       SE93DPR                        *VALORE PRECEDENTE
554400030925     C                   Z-ADD     $PD(1)        SE93D01                        *MESE UNO
554500030925     C                   Z-ADD     $PD(2)        SE93D02                        *MESE DUE
554600030925     C                   Z-ADD     $PD(3)        SE93D03                        *MESE TRE
554700030925     C                   Z-ADD     $PD(4)        SE93D04                        *MESE QUATTRO
554800030925     C                   Z-ADD     $PD(5)        SE93D05                        *MESE CINQUE
554900030925     C                   Z-ADD     $PD(6)        SE93D06                        *MESE SEI
555000030925     C                   Z-ADD     $PD(7)        SE93D07                        *MESE SETTE
555100030925     C                   Z-ADD     $PD(8)        SE93D08                        *MESE OTTO
555200030925     C                   Z-ADD     $PD(9)        SE93D09                        *MESE NOVE
555300030925     C                   Z-ADD     $PD(10)       SE93D10                        *MESE DIECI
555400030925     C                   Z-ADD     $PD(11)       SE93D11                        *MESE UNDICI
555500030925     C                   Z-ADD     $PD(12)       SE93D12                        *MESE DODICI
555600030925     C                   Z-ADD     $PD13C        SE93DTO                        *TOTALE MESI
555700030925     C*
555800030925     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
555900030925IF  2C     PARWCP        IFEQ      'S'
556000030925     c                   ADD       1             WNrr
556100030925     c                   Z-ADD     WNrr          SE93NRR                        *righe in stampa
556200030925     C                   WRITE     WFTISE930
556300030925E   2C                   ENDIF
556400030925E   1C                   ENDIF
556500000000     C*
556600000000     C                   ENDSR
556700020701     C*------------------------------------------------------------------------*
556800020701     C*?REPDELFAT - REPERISCE LE % SCOSTAMENTO FATTURATO TRA IL MESE/ANNO DI ELAB. RISPETTO ALL'ANNO
556900020701     C*------------------------------------------------------------------------*
557000020701     C     REPDELFAT     BEGSR
557100020701     C*
557200020712     C* INIZIALIZZO LA SCHIERA E LE VARIABILI DI WRK
557300020712     C                   CLEAR                   $PDF                           *PERCENTUALI SCOST.
557400020712     C                   Z-ADD     1             W_MM              2 0
557500020712     C                   Z-ADD     *ZEROS        W_PROG           18 3
557600020712     C                   Z-ADD     *ZEROS        W_PROGP          18 3
557700020712     C                   Z-ADD     *ZEROS        W_DELFATT        30 9
557800020712     C*
557900020712     C* SOLO X CLIENTI DI CATEGORIA IMPORTANZA DIVERSA DA 'B' E 'C'
558000020712     C                   IF        DESCLV <> 'C' AND
558100020712     C                             DESCLV <> 'B'
558200020712     C*
558300020712     C* CICLO X I 12 MESI DELLE SCHIERE DEI RICAVI REALI A.C. E A.P.
558400020712     C                   DOW       W_MM <= 12
558500020712     C*
558600020712     C* SOMMO I VALORI DEI MESI A.C. E A.P. X CALCOLARE I RELATIVI PROGRESSIVI A.C. E A.P.
558700020712     C                   ADD       R1K(W_MM)     W_PROG
558800020712     C                   ADD       R1KP(W_MM)    W_PROGP
558900020712     C*
559000020712     C* SOLO ALMENO 1 DEI 2 FATTURATI A.C. E A.P. SONO > 0 CALCOLO LA % SCOSTAMENTO FATTURATO A.C. S
559100020712     C                   IF        R1K(W_MM)  <> *zeros AND
559200020712     C                             R1KP(W_MM) <> *zeros
559300020712     C*
559400020712     C* INIZIALIZZO I LVALORE DELLA % SCOSTAMENTO
559500020712     C                   Z-ADD     *zeros        W_DELFATT
559600020712     C*
559700020712     C* QUINDI LA CALCOLO
559800020712     C                   EVAL      W_DELFATT  = (R1K(W_MM) - R1KP(W_MM)) * 100 /
559900020712     C                                           %abs(R1KP(W_MM))
560000020712     C*
560100020712     C* SE LA % OTTENUTA IN VALORE ASSOLUTO E > 999,9 => FORZO VALORE A 999,9
560200020712     C                   IF        W_DELFATT <= -1000
560300020712     C                   EVAL      W_DELFATT  = -999,9
560400020712     C                   ENDIF
560500020712     C                   IF        W_DELFATT >= +1000
560600020712     C                   EVAL      W_DELFATT  = +999,9
560700020712     C                   ENDIF
560800020712     C*
560900020712     C* SE RIENTRA NEL RANGE RICHIESTO NEL LANCIO VALORIZZO LA SCHIERA DA PORTARE IN STAMPA
561000020712     C                   IF        W_DELFATT  >= PARDFD AND
561100020712     C                             W_DELFATT  <= PARDFA
561200020712     C                   EVAL(H)   $PDF(W_MM)  = W_DELFATT
561300020712     C                   ENDIF
561400020712     C                   ENDIF
561500020712     C*
561600020712     C* INCREMENTO IL MESE
561700020712     C                   ADD       1             W_MM
561800020712     C                   ENDDO
561900020712     C*
562000020712     C* VALORIZZO I CAMPI IN STAMPA RELATIVI AI MESI
562100020712     C                   Z-ADD     $PDF(1)       P2CDE1P                        *MESE UNO
562200020712     C                   Z-ADD     $PDF(2)       P2CDE2P                        *MESE DUE
562300020712     C                   Z-ADD     $PDF(3)       P2CDE3P                        *MESE TRE
562400020712     C                   Z-ADD     $PDF(4)       P2CDE4P                        *MESE QUATTRO
562500020712     C                   Z-ADD     $PDF(5)       P2CDE5P                        *MESE CINQUE
562600020712     C                   Z-ADD     $PDF(6)       P2CDE6P                        *MESE SEI
562700020712     C                   Z-ADD     $PDF(7)       P2CDE7P                        *MESE SETTE
562800020712     C                   Z-ADD     $PDF(8)       P2CDE8P                        *MESE OTTO
562900020712     C                   Z-ADD     $PDF(9)       P2CDE9P                        *MESE NOVE
563000020712     C                   Z-ADD     $PDF(10)      P2CDE0P                        *MESE DIECI
563100020712     C                   Z-ADD     $PDF(11)      P2CDEAP                        *MESE UNDICI
563200020712     C                   Z-ADD     $PDF(12)      P2CDEBP                        *MESE DODICI
563300020712     C*
563400020712     C* DOPO LE VALORIZZAZOINI MENSILI EFFETTUO QUELLE DEL PROGRESSIVO
563500020712     C*
563600020712     C* SOLO SE ENTRAMI I FATTURATI A.C. E A.P. SONO > 0 CALCOLO LA % SCOSTAMENTO FATTURATO A.C. SU
563700020712     C                   IF        W_PROG  <> *zeros AND
563800020712     C                             W_PROGP <> *zeros
563900020712     C*
564000020712     C* INIZIALIZZO I LVALORE DELLA % SCOSTAMENTO
564100020712     C                   Z-ADD     *zeros        W_DELFATT
564200020712     C*
564300020712     C* QUINDI LA CALCOLO
564400020712     C                   EVAL      W_DELFATT  = (W_PROG - W_PROGP) * 100 /
564500020712     C                                           %abs(W_PROGP)
564600020712     C*
564700020712     C* SE LA % OTTENUTA IN VALORE ASSOLUTO E > 999,9 => FORZO VALORE A 999,9
564800020712     C                   IF        W_DELFATT <= -1000
564900020712     C                   EVAL      W_DELFATT  = -999,9
565000020712     C                   ENDIF
565100020712     C                   IF        W_DELFATT >= +1000
565200020712     C                   EVAL      W_DELFATT  = +999,9
565300020712     C                   ENDIF
565400020712     C*
565500020712     C* INIZIALIZZO SEMPRE IL CAMPO IN STAMPA RIFERITO ALLA % PROGRESSIVO
565600020712     C                   CLEAR                   P2CDETP
565700020712     C*
565800020712     C* SE RIENTRA NEL RANGE RICHIESTO NEL LANCIO VALORIZZO LA SCHIERA DA PORTARE IN STAMPA
565900020712     C                   IF        W_DELFATT  >= PARDFDP AND
566000020712     C                             W_DELFATT  <= PARDFAP
566100020712     C                   EVAL(H)   P2CDETP     = W_DELFATT
566200020712     C                   ENDIF
566300020712     C                   ENDIF
566400020712     C*
566500020712     C                   ENDIF
566600020701     C*
566700020701     C                   ENDSR
566800020701     C*------------------------------------------------------------------------*
566900020701     C*?INZPDF - INIZILIZZA I CAMPI LA SCHIERA DI WRK X LO SCOSTAMENTO FATTURATO E I RALATIVI CAMPI
567000020701     C*------------------------------------------------------------------------*
567100020701     C     INZPDF        BEGSR
567200020701     C*
567300020701     C                   CLEAR                   $PDF
567400020701     C*
567500020701     C                   Z-ADD     $PDF(1)       P2CDE1P                        *MESE UNO
567600020701     C                   Z-ADD     $PDF(2)       P2CDE2P                        *MESE DUE
567700020701     C                   Z-ADD     $PDF(3)       P2CDE3P                        *MESE TRE
567800020701     C                   Z-ADD     $PDF(4)       P2CDE4P                        *MESE QUATTRO
567900020701     C                   Z-ADD     $PDF(5)       P2CDE5P                        *MESE CINQUE
568000020701     C                   Z-ADD     $PDF(6)       P2CDE6P                        *MESE SEI
568100020701     C                   Z-ADD     $PDF(7)       P2CDE7P                        *MESE SETTE
568200020701     C                   Z-ADD     $PDF(8)       P2CDE8P                        *MESE OTTO
568300020701     C                   Z-ADD     $PDF(9)       P2CDE9P                        *MESE NOVE
568400020701     C                   Z-ADD     $PDF(10)      P2CDE0P                        *MESE DIECI
568500020701     C                   Z-ADD     $PDF(11)      P2CDEAP                        *MESE UNDICI
568600020701     C                   Z-ADD     $PDF(12)      P2CDEBP                        *MESE DODICI
568700020712     C                   Z-ADD     *ZEROS        P2CDETP                        *PROGRESSIVO
568800020701     C*
568900020701     C                   ENDSR
569000000000     C*------------------------------------------------------------------------*
569100000000     C*?STAMP2 - STAMPA I TOTALI NELLA FORMA: CLI / SPE / RIC / DEL
569200000000     C*------------------------------------------------------------------------*
569300000000     C     STAMP2        BEGSR
569400000000     C*--------------------
569500000000     C* I N T E S T A Z I O N E
569600000000     C*--------------------
569700000000IF  1C     *IN01         IFEQ      *ON
569800020215     C                   EXSR      STAINT
569900000000E   1C                   ENDIF
570000000000     C*--------------------
570100020222     C* C L I E N T I
570200000000     C*--------------------
570300000000     C                   MOVEL     DRI(13)       DES03             3
570400000000     C                   MOVEL     *BLANKS       P1DDES
570500000000     C                   MOVEL     DES20         P1DDES
570600000000     C                   MOVE      DES03         P1DDES
570700020222     C*
570800020222     C* SE STO STAMPANDO DATI DI CLIENTI 'D' => VALORIZZO IL CAMPO IN STAMPA X EVIDENZIARLI
570900020222     C                   IF        DEPSTPD = '0'
571000020222     C                   EVAL      P1DFLD = *BLANKS
571100020222     C                   ENDIF
571200020222     C                   IF        DEPSTPD = '1'
571300020222     C                   EVAL      P1DFLD = '*'
571400020222     C                   ENDIF
571500020222     C                   IF        DEPSTPD = '2'
571600020222     C                   EVAL      P1DFLD = '**'
571700020222     C                   ENDIF
571800030311     C                   IF        DEPSTPD = '3'
571900030520     C                   EVAL      P1DFLD = '§'
572000030311     C                   ENDIF
572100000000     C*
572200000000     C                   Z-ADD     CLW           $VA
572300020116     C                   Z-ADD     CTW(1)        $VA13C                         *PERIODO CORRENTE
572400020116     C                   Z-ADD     CTW(2)        $V213C                         *PERIODO PRECEDENTE
572500000000     C*
572600000000     C* SCRIVE I VALORI IN STAMPA
572700000000     C                   EXSR      SCRVAL
572800030925     C*
572900030925     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
573000030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
573100030925     C                   MOVEL     PARIOE        SE93IOE                        *DELTA ITALIA/ESTERO
573200030925     C                   MOVEL     INCDIV        SE93DIV                        *DIVISIONE
573300030925     C                   MOVEL     INDDIV        SE93DDI                        *DESCRIZIONE DIVISIO
573400030925     C                   MOVEL     INCARE        SE93ARE                        *AREA
573500030925     C                   MOVEL     INDARE        SE93DAR                        *DESCRIZIONE AREA
573600030925     C                   MOVEL     INCFIL        SE93FIL                        *FILIALE
573700030925     C                   MOVEL     INDFIL        SE93DFI                        *DESCRIZIONE FILIALE
573800030926IF  2c     WTipStamp2    IFEQ      'F'                                          *da STAFIL
573900030926     C                   MOVEL     DEPFIL        SE93FIL                        *FILIALE
574000030926     C                   MOVEL     DESFIL        SE93DFI                        *DESCRIZIONE FILIALE
574100030926E   2c                   ENDIF
574200030925     C                   MOVEL     P1DFLD        SE93AST                        *CARATTERE STRANO
574300030925     C                   MOVEL     P1DDES        SE93DES                        *DESCRIZIONE
574400030925     C                   MOVEL     *BLANKS       SE93CLV                        *QUALITA' CLIENTE
574500030925     C                   MOVEL     '1'           SE93TIP                        *TIPO TOTALE
574600030925     C                   Z-ADD     $V213C        SE93CPR                        *VALORE PRECEDENTE
574700030925     C                   Z-ADD     $VA(1)        SE93C01                        *MESE UNO
574800030925     C                   Z-ADD     $VA(2)        SE93C02                        *MESE DUE
574900030925     C                   Z-ADD     $VA(3)        SE93C03                        *MESE TRE
575000030925     C                   Z-ADD     $VA(4)        SE93C04                        *MESE QUATTRO
575100030925     C                   Z-ADD     $VA(5)        SE93C05                        *MESE CINQUE
575200030925     C                   Z-ADD     $VA(6)        SE93C06                        *MESE SEI
575300030925     C                   Z-ADD     $VA(7)        SE93C07                        *MESE SETTE
575400030925     C                   Z-ADD     $VA(8)        SE93C08                        *MESE OTTO
575500030925     C                   Z-ADD     $VA(9)        SE93C09                        *MESE NOVE
575600030925     C                   Z-ADD     $VA(10)       SE93C10                        *MESE DIECI
575700030925     C                   Z-ADD     $VA(11)       SE93C11                        *MESE UNDICI
575800030925     C                   Z-ADD     $VA(12)       SE93C12                        *MESE DODICI
575900030925     C                   Z-ADD     $VA13C        SE93CTO                        *TOTALE MESI
576000030925E   1C                   ENDIF
576100000000     C*--------------------
576200000000     C* S P E D I Z I O N I
576300000000     C*--------------------
576400000000     C                   MOVEL     DRI(14)       DES03             3
576500000000     C                   MOVEL     *BLANKS       P1DDES
576600000000     C                   MOVE      DES03         P1DDES
576700020221     C                   MOVEL     *BLANKS       P1DFLD
576800000000     C*
576900000000     C                   Z-ADD     SPW           $VA
577000020116     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
577100000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
577200000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
577300000000E   1C                   ENDDO
577400020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
577500951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
577600000000     C*
577700000000     C* SCRIVE I VALORI IN STAMPA
577800000000     C                   EXSR      SCRVAL
577900030925     C*
578000030925     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
578100030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
578200030925     C                   Z-ADD     $V213C        SE93NPR                        *VALORE PRECEDENTE
578300030925     C                   Z-ADD     $VA(1)        SE93N01                        *MESE UNO
578400030925     C                   Z-ADD     $VA(2)        SE93N02                        *MESE DUE
578500030925     C                   Z-ADD     $VA(3)        SE93N03                        *MESE TRE
578600030925     C                   Z-ADD     $VA(4)        SE93N04                        *MESE QUATTRO
578700030925     C                   Z-ADD     $VA(5)        SE93N05                        *MESE CINQUE
578800030925     C                   Z-ADD     $VA(6)        SE93N06                        *MESE SEI
578900030925     C                   Z-ADD     $VA(7)        SE93N07                        *MESE SETTE
579000030925     C                   Z-ADD     $VA(8)        SE93N08                        *MESE OTTO
579100030925     C                   Z-ADD     $VA(9)        SE93N09                        *MESE NOVE
579200030925     C                   Z-ADD     $VA(10)       SE93N10                        *MESE DIECI
579300030925     C                   Z-ADD     $VA(11)       SE93N11                        *MESE UNDICI
579400030925     C                   Z-ADD     $VA(12)       SE93N12                        *MESE DODICI
579500030925     C                   Z-ADD     $VA13C        SE93NTO                        *TOTALE MESI
579600030925E   1C                   ENDIF
579700000000     C*--------------------
579800000000     C* R I C A V I
579900000000     C*--------------------
580000000000     C                   MOVEL     DRI(15)       DES03             3
580100000000     C                   MOVEL     *BLANKS       P1DDES
580200000000     C                   MOVE      DES03         P1DDES
580300020221     C                   MOVEL     *BLANKS       P1DFLD
580400000000     C*
580500020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
580600020116     C*    RRW           DIV       1000          $VA
580700020116     C                   Z-ADD     RRW           $VA
580800020116     C                   Z-ADD     *ZEROS        $VA13C
580900000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
581000020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
581100000000E   1C                   ENDDO
581200020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
581300951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
581400000000     C*
581500000000     C* SCRIVE I VALORI IN STAMPA
581600000000     C                   EXSR      SCRVAL
581700030925     C*
581800030925     C* IMPOSTA I CAMPI "RICAVI" DEL WFILE DI STAMPA
581900030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
582000030925     C                   Z-ADD     $V213C        SE93RPR                        *VALORE PRECEDENTE
582100030925     C                   Z-ADD     $VA(1)        SE93R01                        *MESE UNO
582200030925     C                   Z-ADD     $VA(2)        SE93R02                        *MESE DUE
582300030925     C                   Z-ADD     $VA(3)        SE93R03                        *MESE TRE
582400030925     C                   Z-ADD     $VA(4)        SE93R04                        *MESE QUATTRO
582500030925     C                   Z-ADD     $VA(5)        SE93R05                        *MESE CINQUE
582600030925     C                   Z-ADD     $VA(6)        SE93R06                        *MESE SEI
582700030925     C                   Z-ADD     $VA(7)        SE93R07                        *MESE SETTE
582800030925     C                   Z-ADD     $VA(8)        SE93R08                        *MESE OTTO
582900030925     C                   Z-ADD     $VA(9)        SE93R09                        *MESE NOVE
583000030925     C                   Z-ADD     $VA(10)       SE93R10                        *MESE DIECI
583100030925     C                   Z-ADD     $VA(11)       SE93R11                        *MESE UNDICI
583200030925     C                   Z-ADD     $VA(12)       SE93R12                        *MESE DODICI
583300030925     C                   Z-ADD     $VA13C        SE93RTO                        *TOTALE MESI
583400030925E   1C                   ENDIF
583500000000     C*--------------------
583600000000     C* D E L T A
583700000000     C*--------------------
583800000000     C                   MOVEL     DRI(16)       DES03             3
583900000000     C                   MOVEL     *BLANKS       P2DDES
584000020221     C                   MOVEL     *BLANKS       P2DFLD
584100000000     C                   MOVE      DES03         P2DDES
584200000000     C*
584300000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
584400000000     C                   Z-ADD     RRW           $RR                            *RICAVI REALI
584500000000     C                   Z-ADD     RPW           $RP                            *RICAVI PRESUNTI
584600000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
584700000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
584800000000     C                   EXSR      IMPCAL
584900970822     C***
585000970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
585100970822     C***
585200970822     C                   MOVEL     *BLANKS       $SD                            *FALG SEGNALAZIONE
585300000000     C*
585400000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
585500000000     C                   EXSR      SCRDEL
585600030925     C*
585700030925     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
585800030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
585900030925     C                   Z-ADD     $PD(13)       SE93DPR                        *VALORE PRECEDENTE
586000030925     C                   Z-ADD     $PD(1)        SE93D01                        *MESE UNO
586100030925     C                   Z-ADD     $PD(2)        SE93D02                        *MESE DUE
586200030925     C                   Z-ADD     $PD(3)        SE93D03                        *MESE TRE
586300030925     C                   Z-ADD     $PD(4)        SE93D04                        *MESE QUATTRO
586400030925     C                   Z-ADD     $PD(5)        SE93D05                        *MESE CINQUE
586500030925     C                   Z-ADD     $PD(6)        SE93D06                        *MESE SEI
586600030925     C                   Z-ADD     $PD(7)        SE93D07                        *MESE SETTE
586700030925     C                   Z-ADD     $PD(8)        SE93D08                        *MESE OTTO
586800030925     C                   Z-ADD     $PD(9)        SE93D09                        *MESE NOVE
586900030925     C                   Z-ADD     $PD(10)       SE93D10                        *MESE DIECI
587000030925     C                   Z-ADD     $PD(11)       SE93D11                        *MESE UNDICI
587100030925     C                   Z-ADD     $PD(12)       SE93D12                        *MESE DODICI
587200030925     C                   Z-ADD     $PD13C        SE93DTO                        *TOTALE MESI
587300030925     C*
587400030925     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
587500030925IF  2C     PARWCP        IFEQ      'S'
587600030925     c                   ADD       1             WNrr
587700030925     c                   Z-ADD     WNrr          SE93NRR                        *righe in stampa
587800030925     C                   WRITE     WFTISE930
587900030925E   2C                   ENDIF
588000030925E   1C                   ENDIF
588100000000     C*
588200000000     C                   ENDSR
588300000000     C*------------------------------------------------------------------------*
588400000000     C*?IMPCAL - IMPOSTA IL CALCOLO DEL DESTA PER TUTTI I VALORI
588500000000     C*          INPUT  --> $RR    : SCHIERA RICAVO REALE
588600000000     C*                     $RP    : SCHIERA RICAVO PRESUNTO
588700000000     C*          OUTPUT --> $PD    : SCHIERA DELTA PERCENTUALE
588800000000     C*                     $PD13C : 13^ PERCENTUALE DELTA
588900000000     C*------------------------------------------------------------------------*
589000000000     C     IMPCAL        BEGSR
589100000000     C*--------------------
589200000000     C* DELTA PERIODO PRECEDENTE
589300000000     C*--------------------
589400020116     C                   Z-ADD     $RR(13)       WRRW             19 5          *RICAVO REALE
589500020116     C                   Z-ADD     $RP(13)       WRPW             19 5          *RICAVO PRESUNTO
589600000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
589700000000     C                   EXSR      CALDEL
589800000000     C                   Z-ADD     WDPW          $PD(13)                        *SCHIERA DELTA
589900000000     C*--------------------
590000000000     C* DELTA 12 MESI
590100000000     C*--------------------
590200000000DO  1C     1             DO        SPARM         J
590300020116     C                   Z-ADD     $RR(J)        WRRW                           *RICAVO REALE
590400020116     C                   Z-ADD     $RP(J)        WRPW                           *RICAVO PRESUNTO
590500020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
590600000000     C                   EXSR      CALDEL
590700000000     C                   Z-ADD     WDPW          $PD(J)                         *SCHIERA DELTA
590800000000E   1C                   ENDDO
590900000000     C*--------------------
591000000000     C* DELTA 13^ COLONNA
591100000000     C*--------------------
591200000000     C                   Z-ADD     *ZEROS        WRRW
591300000000     C                   Z-ADD     *ZEROS        WRPW
591400000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
591500020116     C                   ADD       $RR(J)        WRRW                           *RICAVO REALE
591600020116     C                   ADD       $RP(J)        WRPW                           *RICAVO PRESUNTO
591700000000E   1C                   ENDDO
591800020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
591900000000     C                   EXSR      CALDEL
592000000000     C                   Z-ADD     WDPW          $PD13C                         *13^ COLONNA
592100000000     C*
592200000000     C                   ENDSR
592300000000     C*------------------------------------------------------------------------*
592400000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
592500000000     C*          INPUT  --> WRRW: RICAVO REALE
592600000000     C*                     WRPW: RICAVO PRESUNTO
592700000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
592800000000     C*------------------------------------------------------------------------*
592900000000     C     CALDEL        BEGSR
593000000000     C*--------------------
593100000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
593200000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
593300000000     C*--------------------
593400000000     C*
593500000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
593600020116     C     WRRW          SUB       WRPW          GAP              19 5          (REALE - PRESUNTO)
593700000000     C     GAP           MULT      100           GAP                            (GAP x 100)
593800000000     C*
593900000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
594000000000IF  1C     WRRW          IFEQ      *ZEROS
594100000000     C                   Z-ADD     1             WRRW
594200000000E   1C                   ENDIF
594300000000     C*
594400000000     C     GAP           DIV       WRRW          GAP
594500000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
594600000000     C*
594700000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
594800000000     C     WDPW          COMP      0                                    28
594900000000     C* POSITIVO
595000000000IF  1C     *IN28         IFEQ      *OFF
595100000000IF  2C     GAP           IFGE      1000
595200000000     C                   Z-ADD     +999,9        WDPW
595300000000E   2C                   ENDIF
595400000000     C* NEGATIVO
595500000000X   1C                   ELSE
595600000000IF  2C     GAP           IFLE      -1000
595700000000     C                   Z-ADD     -999,9        WDPW
595800000000E   2C                   ENDIF
595900000000E   1C                   ENDIF
596000000000     C*
596100000000     C                   ENDSR
596200000000     C*------------------------------------------------------------------------*
596300000000     C*?SCRVAL - SCRIVE I VALORI IN STAMPA
596400000000     C*------------------------------------------------------------------------*
596500000000     C     SCRVAL        BEGSR
596600000000     C*
596700951227     C                   Z-ADD     $V213C        P1CVAP                         *VALORE PRECEDENTE
596800000000     C                   Z-ADD     $VA(1)        P1CVA1                         *MESE UNO
596900000000     C                   Z-ADD     $VA(2)        P1CVA2                         *MESE DUE
597000000000     C                   Z-ADD     $VA(3)        P1CVA3                         *MESE TRE
597100000000     C                   Z-ADD     $VA(4)        P1CVA4                         *MESE QUATTRO
597200000000     C                   Z-ADD     $VA(5)        P1CVA5                         *MESE CINQUE
597300000000     C                   Z-ADD     $VA(6)        P1CVA6                         *MESE SEI
597400000000     C                   Z-ADD     $VA(7)        P1CVA7                         *MESE SETTE
597500000000     C                   Z-ADD     $VA(8)        P1CVA8                         *MESE OTTO
597600000000     C                   Z-ADD     $VA(9)        P1CVA9                         *MESE NOVE
597700000000     C                   Z-ADD     $VA(10)       P1CVA0                         *MESE DIECI
597800000000     C                   Z-ADD     $VA(11)       P1CVAA                         *MESE UNDICI
597900000000     C                   Z-ADD     $VA(12)       P1CVAB                         *MESE DODICI
598000000000     C                   Z-ADD     $VA13C        P1CVAT                         *TOTALE MESI
598100000000     C*
598200100129     c                   if        parSTAMPA <> 'N'
598300020215     C  N53              WRITE     SE94P1                                       *STAMPA RIGA VALORE
598400100129     c                   end
598500011123     C   53              SETOFF                                       53
598600000000     C*
598700000000     C                   ENDSR
598800000000     C*------------------------------------------------------------------------*
598900000000     C*?SCRDEL - SCRIVE I VALORI DELTA IN STAMPA
599000000000     C*------------------------------------------------------------------------*
599100000000     C     SCRDEL        BEGSR
599200000000     C*
599300000000     C                   Z-ADD     $PD(13)       P2CDEP                         *VALORE PRECEDENTE
599400000000     C                   Z-ADD     $PD(1)        P2CDE1                         *MESE UNO
599500970822     C                   MOVEL     $SD(1)        P2CSP1
599600000000     C                   Z-ADD     $PD(2)        P2CDE2                         *MESE DUE
599700970822     C                   MOVEL     $SD(2)        P2CSP2
599800000000     C                   Z-ADD     $PD(3)        P2CDE3                         *MESE TRE
599900970822     C                   MOVEL     $SD(3)        P2CSP3
600000000000     C                   Z-ADD     $PD(4)        P2CDE4                         *MESE QUATTRO
600100970822     C                   MOVEL     $SD(4)        P2CSP4
600200000000     C                   Z-ADD     $PD(5)        P2CDE5                         *MESE CINQUE
600300970822     C                   MOVEL     $SD(5)        P2CSP5
600400000000     C                   Z-ADD     $PD(6)        P2CDE6                         *MESE SEI
600500970822     C                   MOVEL     $SD(6)        P2CSP6
600600000000     C                   Z-ADD     $PD(7)        P2CDE7                         *MESE SETTE
600700970822     C                   MOVEL     $SD(7)        P2CSP7
600800000000     C                   Z-ADD     $PD(8)        P2CDE8                         *MESE OTTO
600900970822     C                   MOVEL     $SD(8)        P2CSP8
601000000000     C                   Z-ADD     $PD(9)        P2CDE9                         *MESE NOVE
601100970822     C                   MOVEL     $SD(9)        P2CSP9
601200000000     C                   Z-ADD     $PD(10)       P2CDE0                         *MESE DIECI
601300970822     C                   MOVEL     $SD(10)       P2CSP0
601400000000     C                   Z-ADD     $PD(11)       P2CDEA                         *MESE UNDICI
601500970822     C                   MOVEL     $SD(11)       P2CSPA
601600000000     C                   Z-ADD     $PD(12)       P2CDEB                         *MESE DODICI
601700970822     C                   MOVEL     $SD(12)       P2CSPB
601800000000     C                   Z-ADD     $PD13C        P2CDET                         *TOTALE MESI
601900000000     C*
602000100129     c                   if        parSTAMPA <> 'N'
602100020215     C  N53              WRITE     SE94P2                                       *STAMPA RIGA VALORE
602200100129     c                   end
602300011123     C   53              SETOFF                                       53
602400000000     C*
602500000000     C                   ENDSR
602600000000     C*------------------------------------------------------------------------*
602700000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
602800000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
602900000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
603000000000     C*------------------------------------------------------------------------*
603100000000     C     DECDIV        BEGSR
603200000000     C*
603300000000     C                   Z-ADD     1             D
603400000000     C     DEPDIV        LOOKUP    DIV(D)                                 99
603500000000IF  1C     *IN99         IFEQ      *OFF
603600000000     C                   MOVEL     *ALL'*'       DESDIV           20
603700000000X   1C                   ELSE
603800000000     C                   MOVEL     DDIV(D)       DESDIV
603900000000E   1C                   ENDIF
604000000000     C*
604100000000     C                   ENDSR
604200000000     C*------------------------------------------------------------------------*
604300000000     C*? DECARE - DECODIFICA CODICE AREA
604400000000     C*          INPUT  -- DEPARE: CODICE AREA
604500000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
604600000000     C*------------------------------------------------------------------------*
604700000000     C     DECARE        BEGSR
604800000000     C*
604900000000     C                   Z-ADD     1             D
605000000000     C     DEPARE        LOOKUP    ARE(D)                                 99
605100000000IF  1C     *IN99         IFEQ      *OFF
605200000000     C                   MOVEL     *ALL'*'       DESARE           20
605300000000X   1C                   ELSE
605400000000     C                   MOVEL     DARE(D)       DESARE
605500000000E   1C                   ENDIF
605600000000     C*
605700000000     C                   ENDSR
605800000000     C*------------------------------------------------------------------------*
605900000000     C*? DECFIL - DECODIFICA CODICE FILIALE
606000000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
606100000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
606200000000     C*------------------------------------------------------------------------*
606300000000     C     DECFIL        BEGSR
606400000000     C*
606500000000     C                   Z-ADD     1             D
606600000000     C     DEPFIL        LOOKUP    FIL(D)                                 99
606700000000IF  1C     *IN99         IFEQ      *OFF
606800000000     C                   MOVEL     *ALL'*'       DESFIL           20
606900000000X   1C                   ELSE
607000000000     C                   MOVEL     DFIL(D)       DESFIL
607100000000E   1C                   ENDIF
607200000000     C*
607300000000     C                   ENDSR
607400000000     C*------------------------------------------------------------------------*
607500000000     C*?DECAGE - DECODIFICA CODICE AGENTE
607600000000     C*          INPUT  -- DEPAGE: CODICE AGENTE COMMERCIALE
607700000000     C*          OUTPUT -- DESAGE: DESCRIZIONE AGENTE
607800000000     C*                    DE2PCF: PERCENTUALE COMPETENZA FILIALE
607900000000     C*------------------------------------------------------------------------*
608000000000     C     DECAGE        BEGSR
608100000000     C*
608200980407     C                   MOVEL     *BLANKS       DESAGE           25
608300000000     C                   Z-ADD     1             D
608400000000     C     DEPAGE        LOOKUP    AGE(D)                                 99
608500000000IF  1C     *IN99         IFEQ      *OFF
608600980407     C                   MOVEL     *ALL'*'       DESAGE
608700000000     C                   Z-ADD     *ZEROS        DESPCF
608800000000     C                   MOVEL     *BLANKS       DE2PCF
608900000000X   1C                   ELSE
609000000000     C                   MOVEL     DAGE(D)       DESAGE
609100000000     C                   Z-ADD     CAGE(D)       DESPCF
609200000000     C*
609300000000     C* METTO IL CAMPO PERCENTUALE IN UNO ALFANUMERICO E LO EDITO(Z)
609400000000     C                   MOVEL     DESPCF        DE2PCF            3
609500000000     C                   Z-ADD     1             P
609600000000     C                   MOVEA     DE2PCF        PCF
609700000000     C*
609800000000DO  2C                   DO        3             P
609900000000IF  3C     PCF(P)        IFEQ      *ZEROS
610000000000     C                   MOVEL     *BLANKS       PCF(P)
610100000000X   3C                   ELSE
610200000000     C                   Z-ADD     4             P
610300000000E   3C                   ENDIF
610400000000E   2C                   ENDDO
610500000000     C                   MOVEA     PCF           DE2PCF
610600000000E   1C                   ENDIF
610700000000     C*
610800000000     C                   ENDSR
610900000000     C*------------------------------------------------------------------------*
611000000000     C*?DECKSC - DECODIFICA CODICE CLIENTE
611100000825     C*          INPUT  -- DEPCLI: CODICE CLIENTE
611200000000     C*          OUTPUT -- DESKSC: RAGIONE SOCIALE CLIENTE
611300951006     C*                    DESITC: DESCRIZIONE CATEGORIA SEAT
611400000000     C*                    DESCLV: QUALITA' CLIENTE
611500000000     C*                    DESCLN: CLIENTE NUOVO O ACQUISITO
611600000000     C*------------------------------------------------------------------------*
611700000000     C     DECKSC        BEGSR
611800120131      *--
611900120131      *-- pulisce x nuovo cliente i campi da evidenziare in seguito
612000120131     c                   eval           Cod_Blocco_Ultimo   = *blank
612100120131     c                   eval           Data_Blocco_Ultimo  = 0
612200120131     c                   eval           Data_Prima_Fat_6    = 0
612300120131      *--
612400000000     C*--------------------
612500000000     C* RAGIONE SOCIALE
612600960708     C* BLOCCO CLIENTE
612700000000     C*--------------------
612800000000     C                   MOVEL     *ALL'*'       DESKSC                         *SE NON TROVATO
612900960708     C                   MOVEL     *BLANKS       DESABL                         *SE NON TROVATO
613000980406     C                   CLEAR                   BS69DS
613100980406     C                   CLEAR                   ACODS
613200980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
613300000825     C                   MOVEL     DEPCLI        I69KAC                         *CLIENTE X CNACO
613400070419     C                   MOVEL     DEPCLI        I69KIN                         *CLIENTE X CNIND
613500980406     C                   CALL      'TIBS69R'
613600980406     C                   PARM                    BS69DS
613700980406     C                   PARM                    ACODS
613800980406     C                   PARM                    INDDS
613900980406     C                   PARM                    CLPDS
614000980406     C                   PARM                    CLSDS
614100980406IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
614200980406     C                   MOVEL     ACORAG        DESKSC                         *RAGIONE SOCIALE
614300980406     C                   MOVEL     ACOABL        DESABL                         *BLOCCO CLIENTE
614400070419      * Nuovi dati anagrafici x i nuovi WADELPCF/E
614500070419     C                   MOVEL     INDvia        DESvia                         *Indirizzo
614600070419     C                   MOVEL     INDcap        DEScap                         *CAP
614700070419     C                   MOVEL     INDcae        DEScae                         *CAP Europeo
614800070419     C                   MOVEL     INDprv        DESprv                         *Provincia
614900070419     C                   MOVEL     INDcit        DEScit                         *Città
615000070419     C                   MOVEL     INDsta        DESsta                         *Nazione
615100070615     C                   MOVEL     INDcdf        DEScdf                         *Cod.Fiscale
615200070615     C                   MOVEL     INDiva        DESiva                         *Part.IVA
615300070618     C                   MOVEL     INDtel        DEStel                         *Telefono
615400980406E   1C                   ENDIF
615500000000     C*--------------------
615600951006     C* CATEGORIA SEAT
615700000000     C*--------------------
615800951006     C                   MOVEL     *BLANKS       DESITC           35            *SE NON TROVATO
615900020906     C                   Z-ADD     1             KTBKUT
616000951006     C                   MOVEL     '1L'          KTBCOD
616100951006     C                   MOVEL     *BLANKS       KTBKEY
616200951006     C                   MOVE      ACOITC        NUM5              5 0
616300951006     C                   MOVEL     NUM5          KTBKEY
616400951006     C     KEYTAB        CHAIN     TABEL00F                           99
616500951006IF  1C     *IN99         IFEQ      *OFF
616600951006     C                   MOVEL     TBLUNI        DS1L
616700951006     C                   MOVEL     §1LDES        DESITC                         *CATEGORIA SEAT
616800951006E   1C                   ENDIF
616900951006     C*--------------------
617000951006     C* QUALITA' CLIENTE
617100951006     C*--------------------
617200960710     C                   MOVEL     WFDCLV        DESCLV
617300000000     C                   SETOFF                                           28
617400010313     C     DESCLV        LOOKUP    ABC                                    28
617500960710IF  1C     *IN28         IFEQ      *OFF
617600000000     C                   MOVEL     'C'           DESCLV
617700960710E   1C                   ENDIF
617800960708     C*--------------------
617900120131     C* CAUSALE BLOCCO e data prima spedizione Fatturata
618000960708     C*--------------------
618100960710     C                   MOVEL     *BLANKS       DESCBL           30
618200980406     C                   CLEAR                   BS69DS
618300980406     C                   CLEAR                   CLPDS
618400980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
618500000825     C                   MOVEL     DEPCLI        I69KCP                         *CLIENTE X CNCLP
618600980406     C                   CALL      'TIBS69R'
618700980406     C                   PARM                    BS69DS
618800980406     C                   PARM                    ACODS
618900980406     C                   PARM                    INDDS
619000980406     C                   PARM                    CLPDS
619100980406     C                   PARM                    CLSDS
619200980406IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
619300120131     c                   eval           Data_Prima_Fat_6 = CLPDPS
619400130318IF  2C     DESABL        IFne      *blank                                       *CLIENTE BLOCCATO
619500130318IF  2C     DESABL        andne     '*'                                          *CLIENTE BLOCCATO
619600120131      **
619700120131      **  Reperisce la Data di quando è stato fatto il Blocco Contabile sul cliente
619800120131     C                   clear                   TrulBLCds
619900120131     c                   eval      IblcTLA = 'L'
620000120131     c                   z-add     clpKcC        IblcKCC
620100120131     c                   z-add     clpKsC        IblcKSC
620200120131     c                   call      'TRULBLCR'
620300120131     C                   PARM                    KPJBA
620400120131     C                   PARM                    TrulBLCds
620500120131     c                   eval           Data_Blocco_Ultimo  = OblcDAV
620600120131     c                   eval           Cod_Blocco_Ultimo   = clpNAR
620700120131      **
620800980406     C                   Z-ADD     1             C
620900980406     C                   SETOFF                                           28
621000980406     C     CLPNAR        LOOKUP    CBL(C)                                 28
621100980406IF  3C     *IN28         IFEQ      *ON
621200980406     C                   MOVEL     DCBL(C)       DESCBL
621300980406E   3C                   ENDIF
621400980406E   2C                   ENDIF
621500980406E   1C                   ENDIF
621600000000     C*--------------------
621700000000     C* CLIENTE NUOVO O ACQUISITO
621800000000     C*--------------------
621900000000     C* PER IL PERIODO PRECEDENTE ALLA STATISTICA SE UN CLIENTE HA DEI SALDI E'
622000000000     C* ACQUISITO, SE NON NE HA E' NUOVO: NON POSSO PRENDERE IL CAMPO WFDCLN PER
622100000000     C* PER L'ANNO PRECEDENTE PERCHE' IL CLIENTE POTEVA ESSERE NUOVO
622200960710     C                   MOVEL     *BLANKS       DESCLN                         *SE NON TROVATO
622300000000IF  1C     TIPANN        IFEQ      'P'                                          *PERIODO PRECEDENTE
622400000000     C                   MOVEL     *BLANKS       DESCLN                         *NON NUOVO
622500000000X   1C                   ELSE                                                   *PERIODO CORRENTE
622600070822      * forza il flag precedentemente testato nel caso particolare sopraddescritto
622700070822      * qualora si tratti di stampa o Italia o Estero.
622800070822     c                   if        desCLN <>'Y' and sav_desCLN_Y ='Y' and
622900070822     c                             wfdCLI =  sav_depCLI
623000070822     c                   movel     'Y'           wfdCLN
623100070822     c                   end
623200000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O ACQUISITO
623300000000E   1C                   ENDIF
623400000000     C*
623500000000     C                   ENDSR
623600000000     C*------------------------------------------------------------------------*
623700000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
623800000000     C*------------------------------------------------------------------------*
623900000000     C     CARMES        BEGSR
624000000000     C*
624100000000     C                   CLEAR                   ANN                            *ANNI
624200000000     C                   CLEAR                   MES                            *MESI
624300000000     C                   Z-ADD     *ZEROS        J                              *N° MESI DA ELABORAR
624400000000     C*
624500990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
624600000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
624700990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
624800000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
624900020701     C*
625000020701     C* SICCOME LA STATISTICA NON PO' + ESSERE LANCIATA A CAVALLO DI ANNI DIVERSI L'ANNO INIZIALE -1
625100020701     C* L'ANNO FINALE -1
625200020701     C     WAA           SUB       1             WAAP              4 0          *ANNO INIZIALE PRECD
625300020701     C
625400000000     C*
625500000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
625600000000IF  1C     WAA2          IFEQ      WAA
625700000000     C*
625800000000     C* CALCOLO I MESI DI DIFFERENZA
625900000000DO  2C     WMM           DOWLE     WMM2
626000000000     C                   ADD       1             J
626100000000     C                   Z-ADD     WAA           ANN(J)
626200000000     C                   Z-ADD     WMM           MES(J)
626300000000     C                   ADD       1             WMM
626400000000E   2C                   ENDDO
626500000000     C***
626600000000     C* SPARM = N° MESI SELEZIONATI
626700000000     C***
626800000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
626900000000     C*
627000000000     C* IL RESTO DELLA SCHIERA A ZERO
627100000000DO  2C     J             DOWLT     12
627200000000     C                   ADD       1             J
627300000000     C                   Z-ADD     *ZEROS        ANN(J)
627400000000     C                   Z-ADD     *ZEROS        MES(J)
627500000000E   2C                   ENDDO
627600000000     C*
627700000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
627800000000X   1C                   ELSE
627900000000     C*
628000000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
628100000000DO  2C     WMM           DOWLE     12
628200000000     C                   ADD       1             J
628300000000     C                   Z-ADD     WAA           ANN(J)
628400000000     C                   Z-ADD     WMM           MES(J)
628500000000     C                   ADD       1             WMM
628600000000E   2C                   ENDDO
628700000000     C*
628800000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
628900000000     C                   Z-ADD     01            WMM
629000000000DO  2C     WMM           DOWLE     WMM2
629100000000     C                   ADD       1             J
629200000000     C                   Z-ADD     WAA2          ANN(J)
629300000000     C                   Z-ADD     WMM           MES(J)
629400000000     C                   ADD       1             WMM
629500000000E   2C                   ENDDO
629600000000     C***
629700000000     C* SPARM = N° MESI SELEZIONATI
629800000000     C***
629900000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
630000000000     C*
630100000000     C* IL RESTO DELLA SCHIERA A ZERO
630200000000DO  2C     J             DOWLT     12
630300000000     C                   ADD       1             J
630400000000     C                   Z-ADD     *ZEROS        ANN(J)
630500000000     C                   Z-ADD     *ZEROS        MES(J)
630600000000E   2C                   ENDDO
630700000000E   1C                   ENDIF
630800000000     C*
630900000000     C                   ENDSR
631000000000     C*------------------------------------------------------------------------*
631100000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
631200000000     C*------------------------------------------------------------------------*
631300000000     C     DECTES        BEGSR
631400000000     C*--------------------
631500000000     C* DATA E ORA
631600000000     C*--------------------
631700000000     C                   Z-ADD     UDATE         DATA              6 0
631800000000     C                   TIME                    ORA               6 0
631900950602     C*--------------------
632000950602     C* DATI IN STATISTICA
632100950602     C*--------------------
632200950602IF  1C     PARIOE        IFEQ      'I'
632300950602     C                   MOVEL     DRI(5)        PTCIOE
632400950602E   1C                   ENDIF
632500950602IF  1C     PARIOE        IFEQ      'E'
632600950602     C                   MOVEL     DRI(6)        PTCIOE
632700950602E   1C                   ENDIF
632800950602IF  1C     PARIOE        IFEQ      *BLANKS
632900950602     C                   MOVEL     DRI(7)        PTCIOE
633000950602E   1C                   ENDIF
633100000829     C*--------------------
633200000829     C* DATI CLIENTE / UNIFICANTE
633300000829     C*--------------------
633400000829IF  1C     PARSCE        IFEQ      'C'
633500000829     C                   MOVEL     DRI(29)       PTDSCE
633600000829X   1C                   ELSE
633700000829     C                   MOVEL     DRI(30)       PTDSCE
633800000829E   1C                   ENDIF
633900000000     C*--------------------
634000000000     C* DECODIFICA MESI
634100000000     C*--------------------
634200000000IF  1C     MES(1)        IFGT      *ZEROS
634300000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ
634400000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
634500990609     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
634600000000E   1C                   ENDIF
634700000000     C*
634800000000IF  1C     MES(2)        IFGT      *ZEROS
634900000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ
635000000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
635100990609     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
635200000000E   1C                   ENDIF
635300000000     C*
635400000000IF  1C     MES(3)        IFGT      *ZEROS
635500000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ
635600000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
635700990609     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
635800000000E   1C                   ENDIF
635900000000     C*
636000000000IF  1C     MES(4)        IFGT      *ZEROS
636100000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ
636200000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
636300990609     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
636400000000E   1C                   ENDIF
636500000000     C*
636600000000IF  1C     MES(5)        IFGT      *ZEROS
636700000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ
636800000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
636900990609     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
637000000000E   1C                   ENDIF
637100000000     C*
637200000000IF  1C     MES(6)        IFGT      *ZEROS
637300000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ
637400000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
637500990609     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
637600000000E   1C                   ENDIF
637700000000     C*
637800000000IF  1C     MES(7)        IFGT      *ZEROS
637900000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ
638000000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
638100990609     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
638200000000E   1C                   ENDIF
638300000000     C*
638400000000IF  1C     MES(8)        IFGT      *ZEROS
638500000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ
638600000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
638700990609     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
638800000000E   1C                   ENDIF
638900000000     C*
639000000000IF  1C     MES(9)        IFGT      *ZEROS
639100000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ
639200000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
639300990609     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
639400000000E   1C                   ENDIF
639500000000     C*
639600000000IF  1C     MES(10)       IFGT      *ZEROS
639700000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ
639800000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
639900990609     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
640000000000E   1C                   ENDIF
640100000000     C*
640200000000IF  1C     MES(11)       IFGT      *ZEROS
640300000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ
640400000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
640500990609     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
640600000000E   1C                   ENDIF
640700000000     C*
640800000000IF  1C     MES(12)       IFGT      *ZEROS
640900000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ
641000000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
641100990609     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
641200000000E   1C                   ENDIF
641300000000     C*
641400000000     C                   ENDSR
641500000000     C*------------------------------------------------------------------------*
641600000000     C*?CARTAB - CARICA I DATI IN TABELLE
641700000000     C*------------------------------------------------------------------------*
641800000000     C     CARTAB        BEGSR
641900000000     C*--------------------
642000000000     C* FILIALI/AREE/DIVISIONI
642100000000     C*--------------------
642200000000     C*
642300000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
642400000000     C                   Z-ADD     *ZEROS        KORFIL
642500000000     C     KEYORG        SETLL     AZORG01L
642600000000     C                   READ      AZORG01L                               99
642700000000     C*
642800000000     C* CICLO FINO A FINE FILE
642900000000     C                   Z-ADD     *ZEROS        F
643000951219DO  1C     *IN99         DOWEQ     *OFF
643100000000     C*
643200000000     C* ELIMINA RECORD ANNULLATI
643300000000IF  2C     ORGFVA        IFEQ      *BLANKS
643400000000     C                   ADD       1             F
643500000000     C* F I L I A L I
643600000000     C                   Z-ADD     ORGFIL        FIL(F)
643700000000     C                   MOVEL     ORGDES        DFIL(F)
643800000707     C                   MOVEL     ORGDE3        OG143
643900020627     C                   MOVEL     §OGNTW        POS(F)
644000000000     C* A R E E
644100020906     C                   Z-ADD     1             KTBKUT
644200000000     C                   MOVEL     '05'          KTBCOD
644300000000     C                   MOVEL     *BLANKS       KTBKEY
644400000000     C                   MOVEL     ORGCAR        KTBKEY
644500000000     C     KEYTAB        CHAIN     TABEL00F                           99
644600000000IF  3C     *IN99         IFEQ      *OFF
644700000000     C                   Z-ADD     ORGCAR        ARE(F)
644800000000     C                   MOVEL     TBLUNI        DARE(F)
644900000000E   3C                   ENDIF
645000000000     C* D I V I S I O N I
645100020906     C                   Z-ADD     1             KTBKUT
645200000000     C                   MOVEL     '17'          KTBCOD
645300000000     C                   MOVEL     *BLANKS       KTBKEY
645400000000     C                   MOVEL     ORGFL3        KTBKEY
645500000000     C     KEYTAB        CHAIN     TABEL00F                           99
645600000000IF  3C     *IN99         IFEQ      *OFF
645700000000     C                   MOVEL     ORGFL3        DIV(F)
645800000000     C                   MOVEL     TBLUNI        DDIV(F)
645900000000E   3C                   ENDIF
646000000000E   2C                   ENDIF
646100000000     C*
646200000000     C* LEGGE IL RECORD SUCCESSIVO
646300000000     C                   READ      AZORG01L                               99
646400000000E   1C                   ENDDO
646500000000     C*--------------------
646600000000     C* COMMERCIALI
646700000000     C*--------------------
646800140612     C************       Z-ADD     1             KTBKUT
646900140612     C************       MOVEL     '01'          KTBCOD
647000140612     C***  KEYTA2        SETLL     TABEL00F
647100140612     C***  KEYTA2        READE     TABEL00F                               99
647200140612     C************       Z-ADD     *ZEROS        C
647300140612DO  1C***  *IN99         DOWEQ     *OFF
647400140612IF  2C***  TBLFLG        IFEQ      *BLANKS
647500140612     C************       ADD       1             C
647600140612     C************       MOVEL     TBLUNI        DS01
647700140612     C************       MOVEL     TBLKEY        AGE(C)
647800140612     C************       MOVEL     §01AGE        DAGE(C)
647900140612IF  3C***  PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
648000140612     C************       Z-ADD     §01PCF        CAGE(C)
648100140612X   3C************       ELSE                                                   *% LAVORO ESTERO
648200140612     C************       Z-ADD     §01PCE        CAGE(C)
648300140612E   3C************       ENDIF
648400140612E   2C************       ENDIF
648500140612     C***  KEYTA2        READE     TABEL00F                               99
648600140612E   1C************       ENDDO
648700140612      *------
648800140612     c                   open      azCMM01l
648900140612     C                   Z-ADD     *ZEROS        C
649000140612     C     *loval        SETLL     azCMM01l
649100140612     C                   read      azCMM01l
649200140612DO  1C                   dow       not %Eof(azCMM01l)
649300140612IF  2C                   IF           cmmATB = *blank
649400140612     C                   ADD       1             C
649500140612     C                   z-add     cmmCOD        AGE(C)
649600140612     C                   MOVEL     cmmDES        DAGE(C)
649700140612IF  3C     PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
649800140612     C                   Z-ADD     cmmPLI        CAGE(C)
649900140612X   3C                   ELSE                                                   *% LAVORO ESTERO
650000140612     C                   Z-ADD     cmmPLE        CAGE(C)
650100140612E   3C                   ENDIF
650200140612E   2C                   end
650300140612     C                   read      azCMM01l
650400140612E   1C                   ENDDO
650500140612     c                   close     azCMM01l
650600960708     C*--------------------
650700960708     C* CAUSALI BLOCCO CLIENTI
650800960708     C*--------------------
650900020906     C                   Z-ADD     1             KTBKUT
651000960708     C                   MOVEL     'BI'          KTBCOD
651100960708     C     KEYTA2        SETLL     TABEL00F
651200960708     C     KEYTA2        READE     TABEL00F                               99
651300960708     C                   Z-ADD     *ZEROS        C
651400960708DO  1C     *IN99         DOWEQ     *OFF
651500960708IF  2C     TBLFLG        IFEQ      *BLANKS
651600960708     C                   ADD       1             C
651700960708     C                   MOVEL     TBLKEY        CBL(C)
651800960708     C                   MOVEL     TBLUNI        DCBL(C)
651900960708E   2C                   ENDIF
652000960708     C     KEYTA2        READE     TABEL00F                               99
652100960708E   1C                   ENDDO
652200980521     C*---
652300980521     C* QUALITA' CLIENTE
652400980521     C*---
652500020906     C                   Z-ADD     1             KTBKUT
652600980521     C                   MOVEL     'IC'          KTBCOD
652700980521     C     KEYTA2        SETLL     TABEL00F
652800980521     C     KEYTA2        READE     TABEL00F                               99
652900980521DO  1C     *IN99         DOWEQ     *OFF
653000980521IF  2C     TBLFLG        IFEQ      *BLANKS
653100020220     C                   MOVEL     TBLUNI        DSIC
653200020220     C                   MOVEL     TBLKEY        ABC(§SICOR)
653300980521E   2C                   ENDIF
653400980521     C     KEYTA2        READE     TABEL00F                               99
653500980521E   1C                   ENDDO
653600000000     C*
653700000000     C                   ENDSR
653800000000     C*------------------------------------------------------------------------*
653900000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
654000000000     C*------------------------------------------------------------------------*
654100000000     C     STAPAR        BEGSR
654200000000     C*--------------------
654300000000     C* NOME PROGRAMMA
654400000000     C*--------------------
654500020116     C                   MOVEL     'TISE93R'     PTCPGM
654600000000     C*--------------------
654700000000     C* DESCRIZIONE STATISTICA
654800000000     C*--------------------
654900000000     C                   MOVEL     DST(S)        PTCDST
655000000000     C*--------------------
655100000000     C* CRAZIONE FILE DI LAVORO
655200000000     C*--------------------
655300000000IF  1C     PARCRE        IFEQ      'S'
655400000000     C                   MOVEL     'SI'          PACCRE
655500000000X   1C                   ELSE
655600000000     C                   MOVEL     'NO'          PACCRE
655700000000E   1C                   ENDIF
655800000000     C*--------------------
655900000000     C* DATE LANCIO
656000000000     C*--------------------
656100000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
656200000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
656300000000     C*--------------------
656400000000     C* SELEZIONI PER TIPO SERVIZIO
656500000000     C*--------------------
656600000000IF  1C     PARTSP        IFEQ      *BLANKS
656700000000     C                   MOVEL     DRI(1)        PADTSP
656800100129      **
656900100129      **  si memorizza quanti tipi servizi sono attivi nella tabella
657000100129     c                   z-add     0             Quanti_Tserv
657100100129     c                   clear                   TServ
657200100129     c                   clear                   DTServ
657300100129      **  carica codice e descrizione su 2 schiere e Quanti_serv contiene il nr.dei tipi servizio abilitati
657400100129     C                   Z-ADD     1             KTBKUT
657500100129     C                   MOVEL     '5E'          KTBCOD
657600100129     C     KEYTA2        setll     TABEL00F
657700100129     C     KEYTA2        reade     TABEL00F
657800100129     c                   dow       not %EoF(TABEL00F)
657900100129     c                   add       1             Quanti_Tserv
658000100129     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
658100100129     c                   eval        tserv(Quanti_Tserv) = tblkey
658200100129     c                   eval       Dtserv(Quanti_Tserv) = §5EDES
658300100129     C     KEYTA2        reade     TABEL00F
658400100129     c                   endDO
658500100129      **
658600000000X   1C                   ELSE
658700100129      **
658800000000     C                   MOVEL     PARTSP        PACTSP
658900020906     C                   Z-ADD     1             KTBKUT
659000000000     C                   MOVEL     '5E'          KTBCOD
659100000000     C                   MOVEL     *BLANKS       KTBKEY
659200000000     C                   MOVEL     PARTSP        KTBKEY
659300980406     C     KEYTAB        CHAIN     TABEL00F                           99
659400000000IF  2C     *IN99         IFEQ      *OFF
659500090610     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
659600090610     C                   MOVEL     §5EDES        PADTSP                         *TIPO SERVIZIO
659700000000E   2C                   ENDIF
659800000000E   1C                   ENDIF
659900950208     C*--------------------
660000950208     C* SELEZIONI PER DELTA ITALIA/ESTERO
660100950208     C*--------------------
660200950208     C                   MOVEL     PARIOE        PACIOE
660300950208IF  1C     PARIOE        IFEQ      'I'
660400950208     C                   MOVEL     DRI(5)        PADIOE
660500950208E   1C                   ENDIF
660600950208IF  1C     PARIOE        IFEQ      'E'
660700950208     C                   MOVEL     DRI(6)        PADIOE
660800950208E   1C                   ENDIF
660900950208IF  1C     PARIOE        IFEQ      *BLANKS
661000950208     C                   MOVEL     DRI(7)        PADIOE
661100950208E   1C                   ENDIF
661200951018     C*--------------------
661300970822     C* SELEZIONI PER QUALITA' CLIENTE
661400951018     C*--------------------
661500951018     C                   MOVEL     PARCLV        PACCLV
661600010618     C                   MOVEL     PARCL1        PACCL1
661700010618     C                   MOVEL     PARCL2        PACCL2
661800951018     C                   MOVEL     *BLANKS       PADCLV
661900951018IF  1C     PARCLV        IFEQ      *BLANKS
662000010618     C     PARCL1        ANDEQ     *BLANKS
662100010618     C     PARCL2        ANDEQ     *BLANKS
662200951018     C                   MOVEL     DRI(1)        PADCLV
662300980528E   1C                   ENDIF
662400010618     C*--------------------
662500010618     C* TIPO STAMPA SINTETICA S/N
662600010618     C*--------------------
662700010618     C                   MOVEL     PARSIN        PACSIN
662800980528     C*--------------------
662900980528     C* % SCOSTAMENTO DELTA TOTALE
663000980528     C*--------------------
663100980528     C                   Z-ADD     PARPDD        PACPDD
663200980528     C                   Z-ADD     PARPDA        PACPDA
663300980528     C*--------------------
663400980528     C* % SCOSTAMENTO DELTA PER FASCE
663500980528     C*--------------------
663600970822     C                   Z-ADD     PARSPD        PACSPD
663700020702     C*--------------------
663800020702     C* % SCOSTAMENTO FATTURATO ANNO PRECEDENTE
663900020702     C*--------------------
664000020702     C                   Z-ADD     PARDFD        PACDFD
664100020702     C                   Z-ADD     PARDFA        PACDFA
664200020702     C                   Z-ADD     PARDFDP       PACDFDP
664300020702     C                   Z-ADD     PARDFAP       PACDFAP
664400000000     C*--------------------
664500000000     C* SELEZIONI PER DELTA FILIALE
664600000000     C*--------------------
664700000000IF  1C     PARFIC        IFGT      *ZEROS                                       *FILIALE
664800000000     C                   Z-ADD     PARFIC        DEPFIL
664900000000     C                   EXSR      DECFIL
665000000000     C                   Z-ADD     *ZEROS        DEPFIL
665100000000     C                   MOVEL     PARFIC        PACFIC
665200000000     C                   MOVEL     DESFIL        PADFIC
665300000000X   1C                   ELSE
665400000000     C                   MOVEL     *BLANKS       PACFIC
665500000000E   1C                   ENDIF
665600000000     C*
665700000000IF  1C     PARARC        IFGT      *ZEROS                                       *AREA
665800000000     C                   Z-ADD     PARARC        DEPARE
665900000000     C                   EXSR      DECARE
666000000000     C                   Z-ADD     *ZEROS        DEPARE
666100000000     C                   MOVEL     PARARC        PACARC
666200000000     C                   MOVEL     DESARE        PADARC
666300000000X   1C                   ELSE
666400000000     C                   MOVEL     *BLANKS       PACARC
666500000000E   1C                   ENDIF
666600000000     C*
666700000000IF  1C     PARDIC        IFNE      *BLANKS                                      *DIVISIONE
666800000000     C                   MOVEL     PARDIC        DEPDIV
666900000000     C                   EXSR      DECDIV
667000000000     C                   MOVEL     *BLANKS       DEPDIV
667100000000     C                   MOVEL     PARDIC        PACDIC
667200000000     C                   MOVEL     DESDIV        PADDIC
667300000000E   1C                   ENDIF
667400000000     C*--------------------
667500000000     C* SELEZIONI PER DELTA COMMERCIALE
667600000000     C*--------------------
667700000000IF  1C     PARCOF        IFGT      *ZEROS                                       *COMMERCIALE
667800000000     C                   Z-ADD     PARCOF        DEPAGE
667900000000     C                   EXSR      DECAGE
668000000000     C                   Z-ADD     *ZEROS        DEPAGE
668100000000     C                   MOVEL     PARCOF        PACCOF
668200000000     C                   MOVEL     DESAGE        PADCOF
668300000000X   1C                   ELSE
668400000000     C                   MOVEL     *BLANKS       PACCOF
668500000000E   1C                   ENDIF
668600000000     C*
668700000000IF  1C     PARDIF        IFNE      *BLANKS                                      *DIVISIONE
668800000000     C                   MOVEL     PARDIF        DEPDIV
668900000000     C                   EXSR      DECDIV
669000000000     C                   MOVEL     *BLANKS       DEPDIV
669100000000     C                   MOVEL     PARDIF        PACDIF
669200000000     C                   MOVEL     DESDIV        PADDIF
669300000000E   1C                   ENDIF
669400000707     C*--------------------
669500000707     C* DATI POSTE
669600000707     C*--------------------
669700000707IF  1C     PARPOS        IFEQ      'S'
669800000707     C                   MOVEL     'SI'          PADPOS
669900000707X   1C                   ELSE
670000000707     C                   MOVEL     'NO'          PADPOS
670100000707E   1C                   ENDIF
670200000825     C*--------------------
670300000825     C* DATI CLIENTE / UNIFICANTE
670400000825     C*--------------------
670500000825     C                   MOVEL     PARSCE        PACSCE
670600000825IF  1C     PARSCE        IFEQ      'C'
670700000825     C                   MOVEL     DRI(29)       PADSCE
670800000825X   1C                   ELSE
670900000825     C                   MOVEL     DRI(30)       PADSCE
671000000825E   1C                   ENDIF
671100000000     C*
671200000000     C* S T A M P A
671300000000     C*
671400100129     c                   if        parSTAMPA <> 'N'
671500020222     C                   WRITE     SE94PT                                       *INTESTAZIONE
671600020116     C                   WRITE     SE94PA                                       *PARAMETRI
671700100129     c                   end
671800000000     C*
671900000000     C* ACCENDO INDICATORE DI OF PER STAMPARE LA TESTATA ALLA 1^ ROTTURA CLIENTE
672000000000     C                   SETON                                        01
672100000000     C                   MOVEL     '0'           PRIGIR
672200000000     C*
672300000000     C                   ENDSR
672400980406     C*------------------------------------------------------------------------*
672500980406     C*?FINSR - OPERAZIONI FINALI
672600980406     C*------------------------------------------------------------------------*
672700980406     C     FINSR         BEGSR
672800980406     C*
672900980406     C* CHIUDE I FILE DEI PGM CHIAMATI
673000980406     C                   CLEAR                   BS69DS
673100980406     C                   CLEAR                   ACODS
673200980406     C                   CLEAR                   INDDS
673300980406     C                   CLEAR                   CLPDS
673400980406     C                   CLEAR                   CLSDS
673500980406     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
673600980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
673700980406     C                   CALL      'TIBS69R'
673800980406     C                   PARM                    BS69DS
673900980406     C                   PARM                    ACODS
674000980406     C                   PARM                    INDDS
674100980406     C                   PARM                    CLPDS
674200980406     C                   PARM                    CLSDS
674300980406     C*
674400980406     C                   ENDSR
674500030924     C*------------------------------------------------------------------------*
674600030924     C*?CANWF - CANCELLA DATI WFILE, SE RICHIESTO
674700030924     C*------------------------------------------------------------------------*
674800030924     C     CANWF         BEGSR
674900030924     C*
675000030925     C     PARSTA        IFEQ      'F'
675100030924     C     PARWCP        IFEQ      'S'                                          *SE RICHIESTO
675200030924     c                   MOVEL     PARIOE        KE93IOE                        *ITALIA/ESTERO
675300030924     C     KEYSE93       SETLL     WFTISE93L
675400030924     C     KEYSE93       READE     WFTISE93L                              99
675500030924     C                   DOW       NOT *IN99
675600030924     C                   DELETE    WFTISE930
675700030924     C     KEYSE93       READE     WFTISE93L                              99
675800030924     C                   ENDDO
675900030925     C                   ENDIF
676000030925     C                   ENDIF
676100030924     C*
676200030924     C                   ENDSR
676300000000     C*------------------------------------------------------------------------*
676400000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
676500000000     C*------------------------------------------------------------------------*
676600000000     C     *INZSR        BEGSR
676700960708     C*--------------------
676800960708     C* RICEVIMENTO PARAMETRI
676900960708     C*--------------------
677000000000     C     *ENTRY        PLIST
677100000000     C                   PARM                    KPJBA
677200000000     C                   MOVEL     KPJBU         PARAM
677300160713     c* Se richiamato x BUDGET questa DATA è Valorizzata
677400160713     C                   MOVE      KPJBU         Data_8alfa        8
677500160713     c*
677600160713      *  controllo validità campo Data ricevuto se in MODALITA' BUDGET
677700160713     c                   clear                   dta_BDG_unif      8 0
677800160713     c                   if        Data_8alfa <> *blank
677900160713     c     digits        check     Data_8alfa
678000160713     c                   if        not %Found
678100160713     c                   move      Data_8alfa    dta_BDG_unif
678200160713      *** se Budget PARIOE = *Blank per sommare ITALIA +ESTERO attenzione
678300160713     c                   end
678400160713     c                   endif
678500100129      *
678600100129      * se non si deve più eseguire la stampa viene passato il flag impostato a "N"
678700100129     c                   if        parSTAMPA <> 'N'
678800100129     c                   open      TISE94P
678900100129     c                   end
679000960708     C*--------------------
679100960708     C* VARIABILI RIFERITE AL DATABASE
679200960708     C*--------------------
679300000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
679400100201     C     *LIKE         DEFINE    TBLCOD        KTBCOD
679500000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
679600000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
679700950213     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEL01L
679800000000     C     *LIKE         DEFINE    WFDARE        KWDARE
679900000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
680000000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
680100960710     C     *LIKE         DEFINE    WFDCLV        KWDCLV
680200000825     C     *LIKE         DEFINE    WFDCLI        KWDCLI
680300000000     C     *LIKE         DEFINE    WFDANN        KWDANN
680400000000     C     *LIKE         DEFINE    WFDMES        KWDMES
680500960708     C     *LIKE         DEFINE    WFDDIV        DEPDIV                         *DI DEPOSITO
680600000000     C     *LIKE         DEFINE    WFDARE        DEPARE
680700000000     C     *LIKE         DEFINE    WFDFIL        DEPFIL
680800000000     C     *LIKE         DEFINE    WFDAGE        DEPAGE
680900081013     C     *LIKE         DEFINE    WFDRGD        DEPRGD
681000081013     C     *LIKE         DEFINE    WFDRGF        DEPRGF
681100070822     C     *LIKE         DEFINE    WFDCLI        DEPCLI
681200070822     C     *LIKE         DEFINE    WFDCLI        sav_depCLI
681300020319     C     *LIKE         DEFINE    WFDCLI        DEPCL2
681400000000     C     *LIKE         DEFINE    WFDANN        DEPANN
681500000000     C     *LIKE         DEFINE    WFDMES        DEPMES
681600020319     C     *LIKE         DEFINE    WFDNTW        DEPNTW
681700140612     C**** *LIKE         DEFINE    §01PCF        DESPCF
681800140612     C     *LIKE         DEFINE    cmmPLI        DESPCF
681900000000     C     *LIKE         DEFINE    ACORAG        DESKSC
682000070419     C     *LIKE         DEFINE    INDvia        DESvia
682100070419     C     *LIKE         DEFINE    INDcit        DEScit
682200070419     C     *LIKE         DEFINE    INDcap        DEScap
682300070419     C     *LIKE         DEFINE    INDcae        DEScae
682400070419     C     *LIKE         DEFINE    INDprv        DESprv
682500070419     C     *LIKE         DEFINE    INDsta        DESsta
682600070615     C     *LIKE         DEFINE    INDcdf        DEScdf                         *Cod.Fiscale
682700070615     C     *LIKE         DEFINE    INDiva        DESiva                         *Part.IVA
682800070618     C     *LIKE         DEFINE    INDtel        DEStel                         *Telefono
682900000000     C     *LIKE         DEFINE    CLPCLV        DESCLV
683000000000     C     *LIKE         DEFINE    WFDCLN        DESCLN
683100070822     C     *LIKE         DEFINE    WFDCLN        sav_desCLN_Y
683200960708     C     *LIKE         DEFINE    ACOABL        DESABL
683300030925     C     *LIKE         DEFINE    SE93IOE       KE93IOE
683400100201     c     *LIKE         DEFINE    k             NumElem
683500100201      **
683600960708     C*--------------------
683700960708     C* VARIABILI NON RIFERITE AL DATABASE
683800960708     C*--------------------
683900000000     C                   Z-ADD     *ZEROS        C                 4 0          *COMMERCIALI
684000000000     C                   Z-ADD     *ZEROS        D                 4 0          *PER DECODIFICHE
684100000000     C                   Z-ADD     *ZEROS        F                 4 0          *VOCI ORGANIGRAMMA
684200000000     C                   Z-ADD     *ZEROS        J                 4 0          *GENERICO
684300000000     C                   Z-ADD     *ZEROS        P                 4 0          *ELEMENTI PERCENTUAL
684400000000     C                   Z-ADD     *ZEROS        Q                 4 0          *QUALITA' CLIENTE
684500000000     C                   Z-ADD     *ZEROS        S                 4 0          *STATISTICA IN ELAB
684600000000     C                   Z-ADD     *ZEROS        X                 2 0          *PER I MESI
684700000000     C                   Z-ADD     *ZEROS        MM                2 0          *PER I MESI
684800000000     C                   Z-ADD     *ZEROS        IN                2 0          *PER I MESI
684900000000     C                   Z-ADD     *ZEROS        SPARM             4 0          *N° MESI SELEZIONATI
685000000828     C                   Z-ADD     *ZEROS        I                 5 0          *INDICE
685100000828     C                   Z-ADD     *ZEROS        IS                5 0          *INDICE
685200020318     C                   Z-ADD     *ZEROS        K                 4 0          *NETWORK
685300020701     C                   Z-ADD     *ZEROS        II                5 0          *INDICE
685400020701     C*--------------------
685500020701     C* INDICATORI DI PROCEDURA
685600020701     C*--------------------
685700020701     C                   SETOFF                                       54
685800960708     C*--------------------
685900960708     C* VARIABILI DI CONTROLLO
686000960708     C*--------------------
686100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
686200000000     C                   MOVEL     '0'           PASGEN            1            MEMOR SALDI GENERALI
686300000000     C                   MOVEL     '0'           PASDIV            1            MEMOR SALDI DIVISION
686400000000     C                   MOVEL     '0'           PASARE            1            MEMOR SALDI AREA
686500000000     C                   MOVEL     '0'           PASFIL            1            MEMOR SALDI FILIALE
686600000000     C                   MOVEL     '0'           PASAGE            1            MEMOR SALDI COMM.LE
686700000000     C                   MOVEL     '0'           PASKSC            1            MEMOR SALDI CLIENTE
686800951227     C                   MOVEL     '0'           FLAKSC            1            FLAG CONT CLI CORR
686900951227     C                   MOVEL     '0'           FL2KSC            1            FLAG CONT CLI PREC
687000000000     C                   MOVEL     '0'           FLAPAS            1            FLAG PASSAGGIO LETTU
687100000000     C                   MOVEL     '0'           $CATCL            1            CATEGORIA CLIENTE
687200000000     C                   Z-ADD     *ZEROS        $Q                1 0          QUALITA' CLIENTE
687300000000     C                   MOVEL     ' '           TIPANN            1            *TIPO ANNO (P/C)
687400000000     C                   MOVEL     '0'           PRIGIR            1            *PRIMO GIRO
687500000000     C                   MOVEL     '0'           ORAKSC            1            *ORA STAMPA CLIENTE
687600000828     C                   MOVEL     'N'           $STRIG            1            *STAMPA RIGA
687700020221     C                   MOVEL     '0'           DEPSTPD           1            *FLAG DI STAMPA SI/N
687800020222     C                   MOVEL     '0'           FLGSTPLE          1            *FLAG DI STAMPA SI/N
687900020318     C                   MOVEL     '0'           FLGSTPNTW         1            *FLAG DI STAMPA SI/N
688000020319     C                   MOVEL     '0'           FLGSTPRI          1            *FLAG DI STAMPA SI/N
688100960708     C*--------------------
688200960708     C* CHAIVI DI LETTURA
688300960708     C*--------------------
688400960708     C* LETTURA TABEL00F - COMPLETA
688500000000     C     KEYTAB        KLIST
688600000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
688700000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
688800000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
688900960708     C* LETTURA TABEL00F - PARZIALE
689000000000     C     KEYTA2        KLIST
689100000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
689200000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
689300960708     C* LETTURA AZORG01L - COMPLETA
689400000000     C     KEYORG        KLIST
689500000000     C                   KFLD                    KORFIL                         *FILIALE
689600960708     C* LETTURA WFDEL01L - PARZIALE
689700000000     C     KEYDEL        KLIST
689800950208     C                   KFLD                    KWDDIV                         *DIVISIONE
689900000000     C                   KFLD                    KWDARE                         *AREA
690000000000     C                   KFLD                    KWDFIL                         *FILIALE
690100000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
690200960710     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
690300000825     C                   KFLD                    KWDCLI                         *CLIENTE
690400000000     C                   KFLD                    KWDANN                         *ANNO
690500000000     C                   KFLD                    KWDMES                         *MESE
690600030924     C* LETTURA WFTISE93L - PARZIALE
690700030924     C     KEYSE93       KLIST
690800030924     C                   KFLD                    KE93IOE                        *ITALIA/ESTERO
690900070420     C* LETTURA TFNTC00F - COMPLETA
691000070420     C     KeyNTC        KLIST
691100070420     C                   KFLD                    NTCapl                         *CODICE APPLICAZIONE
691200070420     C                   KFLD                    NTCnk1                         *CHIAVE 1 NOTE
691300070420     C                   KFLD                    NTCnk2                         *CHIAVE 2 NOTE
691400070420     C                   KFLD                    NTCtnt                         *TIPO NOTA
691500000828     C*
691600130131      ***
691700130131     c     k_deaV1       KLIST
691800130131     C                   KFLD                    VARIA             1
691900130131     C                   KFLD                    DEPCLI
692000130131     C*  serve x il FUEL
692100130131     C                   eval       VARIA ='f'
692200130131     C*
692300000828     C* IMNPOSTA DATA / ORA CORRENTE
692400000828     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
692500000828     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
692600000828     C                   Z-ADD     *ZEROS        G08INV
692700000828     C                   MOVEL     '0'           G08ERR
692800000828     C                   CALL      'XSRDA8'
692900000828     C                   PARM                    WLBDA8
693000000828     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
693100010725     C*
693200160713     C*
693300160713     C*    SE RICHIAMATO come BUDGET deve impostare la DATA di Riferimento
693400160713     C*       direttamente dal pgm chiamante
693500160713     c                   if        dta_BDG_unif   = 0
693600160713     C*
693700010725     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
693800010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
693900010725     C                   MOVE      28            WDFIN
694000081114     C*
694100081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
694200081114     C*
694300081114     C*  Ultimo giorno del mese
694400081114     C                   MOVE      WDFIN         MMGG_4            4 0
694500081114     C                   MOVEL     MMGG_4        Mm_2              2 0
694600081114     C                   MOVEl     WDFIN         Anno_4            4 0
694700081114      * se è febbraio
694800081114     c                   If        Mm_2 = 2
694900081114      *
695000081114      *  potrebbe essere bisestile 29 gg.
695100081114     c     Anno_4        div       4             risulta           3 0
695200081114     c                   mvr                     resto             3 0
695300081114      *  Se bisestile
695400081114     c                   If        resto = 0
695500081114     C                   MOVE      '29'          WDFIN
695600081114     c                   end
695700081114      *
695800081114     c                   else
695900081114      *
696000081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
696100081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
696200081114     c                                Mm_2 = 9 or Mm_2 = 11
696300081114     C                   MOVE      '30'          WDFIN
696400081114     c                   else
696500081114     C                   MOVE      '31'          WDFIN
696600081114     c                   end
696700081114      *
696800081114     c                   endIf
696900160713     C*
697000081114     C*----------- F I N E    M E S E ---------------------------*
697100160713     C*
697200160713     C*    altrimenti come BUDGET imposta la data di riferimento
697300160713     C*    dell'unificante
697400160713     c                   else
697500160713     C                   MOVE      dta_BDG_unif  WDFIN
697600160713     c                   end
697700081114     C*
697800000000     C*
697900000000     C* MEMORIZZA I DATI IN TABELLE
698000000000     C                   EXSR      CARTAB
698100000000     C*
698200000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
698300000000     C                   EXSR      CARMES
698400000000     C*
698500000000     C* DECODIFICA INTESTAZIONE
698600000000     C                   EXSR      DECTES
698700000000     C*
698800000000     C* VEDO QUALE STATISTICA DEVO ELABORARE
698900000000IF  1C     PARSTA        IFEQ      'K'
699000000000     C                   Z-ADD     1             S                              *DETTAGLIO CLIENTE
699100000000E   1C                   ENDIF
699200000000IF  1C     PARSTA        IFEQ      'C'
699300000000     C                   Z-ADD     2             S                              *DETTAGLIO COMM.LE
699400000000E   1C                   ENDIF
699500000000IF  1C     PARSTA        IFEQ      'F'
699600000000     C                   Z-ADD     3             S                              *DETTAGLIO FILIALE
699700000000E   1C                   ENDIF
699800051102     C*
699900051102IF  1C     PARSCE        IFEQ      'U'                                          *STAT. UNIFICANTE
700000051102IF  1C     PARSTA        andeq     'K'                                           dett.x cliente
700100051102     c                   eval      LEG040 = '(+) --> Cliente BLOCCATO'
700200051102     c                   end
700300030924     C*
700400030924     C* CANCELLA DATI WFILE, SE RICHIESTO
700500030924     C                   EXSR      CANWF
700600000000     C*
700700091124     C* Deve impostare i totali per Netowrk
700800091124IF  1C     PARIOE        IFEQ      'E'
700900091124     C                   open      waDELpcN
701000091124E   1C                   ENDIF
701100130131     C*
701200130131     C                   ENDSR
701300130131     C*------------------------------------------------------------------------*
701400130131     C*?ACCUMULA le VARIE - al momento solo x il FUEL (che si vuole scorporare)
701500130131     C*------------------------------------------------------------------------*
701600130131     C     VARIE_x_ClientBEGSR
701700130131      *
701800130131      *  Lo dovrà scrivere una sola volta anche x i giri fra Italia ed Estero
701900130131      *   è per questo che cancellando i records una volta elaborati verrà
702000130131      *   scritto correttamente la prima volta che incontra il codice CLIENTE.
702100130131     c                   move      'N'           deve_scriverlo    1
702200130131      *
702300130131      * ?Cicla sul file delle varie x il tipo servizio (f)->FUEL e x CLIENTE
702400130131      *
702500130131      * ? Man mano che legge e totalizza i records li CANCELLA dal WORKFile
702600130131      * ? per alleggerire le successive operazioni.
702700130131      *  La lettura è x VARIA/CLIENTE
702800130131     c     k_deaV1       setll     wfDEAv1l
702900130131     c     k_deaV1       reade     wfDEAv1l
703000130131     C*
703100130131     c                   DOW       not %EoF(wfDEAv1l)
703200130131     C*
703300130131     C* lo fa solo la prima, primissima volta e basta
703400130131     c                   if        deve_scriverlo = 'N'
703500130131     c                   eval         deve_scriverlo = 'S'
703600130131     c                   clear                   WADELVT0
703700130131     C                   eval      watAAD =  WAdAAD
703800130131     C                   eval      watMMD =  WAdMMD
703900130131     C                   eval      watAAA =  WAdAAA
704000130131     C                   eval      watMMA =  WAdMMA
704100130131     C                   eval      watCLV =  desCLV
704200130131     C                   eval      watCLI =  depCLI
704300130131     C                   eval      watCLD =  desKSC
704400130131     C                   eval      watSVR =  WFVSVR
704500130131     c                   end
704600130131     C*
704700130131     C*  Anno in corso
704800130131     C                   IF        WFVANN =  WAdAAD
704900130131     C*
705000130131     c                   select
705100130131     C*
705200130131     C                   when      WFVmes =  1
705300130131     c                   eval      WAtVA_01SC = wfvNSP + WAtVA_01SC
705400130131     c                   eval      WAtVA_01VC = wfvRCP + WAtVA_01VC
705500130131     C                   when      WFVmes =  2
705600130131     c                   eval      WAtVA_02SC = wfvNSP + WAtVA_02SC
705700130131     c                   eval      WAtVA_02VC = wfvRCP + WAtVA_02VC
705800130131     C                   when      WFVmes =  3
705900130131     c                   eval      WAtVA_03SC = wfvNSP + WAtVA_03SC
706000130131     c                   eval      WAtVA_03VC = wfvRCP + WAtVA_03VC
706100130131     C                   when      WFVmes =  4
706200130131     c                   eval      WAtVA_04SC = wfvNSP + WAtVA_04SC
706300130131     c                   eval      WAtVA_04VC = wfvRCP + WAtVA_04VC
706400130131     C                   when      WFVmes =  5
706500130131     c                   eval      WAtVA_05SC = wfvNSP + WAtVA_05SC
706600130131     c                   eval      WAtVA_05VC = wfvRCP + WAtVA_05VC
706700130131     C                   when      WFVmes =  6
706800130131     c                   eval      WAtVA_06SC = wfvNSP + WAtVA_06SC
706900130131     c                   eval      WAtVA_06VC = wfvRCP + WAtVA_06VC
707000130131     C                   when      WFVmes =  7
707100130131     c                   eval      WAtVA_07SC = wfvNSP + WAtVA_07SC
707200130131     c                   eval      WAtVA_07VC = wfvRCP + WAtVA_07VC
707300130131     C                   when      WFVmes =  8
707400130131     c                   eval      WAtVA_08SC = wfvNSP + WAtVA_08SC
707500130131     c                   eval      WAtVA_08VC = wfvRCP + WAtVA_08VC
707600130131     C                   when      WFVmes =  9
707700130131     c                   eval      WAtVA_09SC = wfvNSP + WAtVA_09SC
707800130131     c                   eval      WAtVA_09VC = wfvRCP + WAtVA_09VC
707900130131     C                   when      WFVmes = 10
708000130131     c                   eval      WAtVA_10SC = wfvNSP + WAtVA_10SC
708100130131     c                   eval      WAtVA_10VC = wfvRCP + WAtVA_10VC
708200130131     C                   when      WFVmes = 11
708300130131     c                   eval      WAtVA_11SC = wfvNSP + WAtVA_11SC
708400130131     c                   eval      WAtVA_11VC = wfvRCP + WAtVA_11VC
708500130131     C                   when      WFVmes = 12
708600130131     c                   eval      WAtVA_12SC = wfvNSP + WAtVA_12SC
708700130131     c                   eval      WAtVA_12VC = wfvRCP + WAtVA_12VC
708800130131     c                   endSL
708900130131     C*
709000130131     C                   eval      WAtVAT_SC  = wfvNSP  + WAtVAT_SC
709100130131     C                   eval      WAtVAT_VC  = wfvRCP  + WAtVAT_VC
709200130131     C*  Anno precedente
709300130131     C                   ELSE
709400130131     C*
709500130131     c                   select
709600130131     C*
709700130131     C                   when      WFVmes =  1
709800130131     c                   eval      WAtVA_01SP = wfvNSP + WAtVA_01SP
709900130131     c                   eval      WAtVA_01VP = wfvRCP + WAtVA_01VP
710000130131     C                   when      WFVmes =  2
710100130131     c                   eval      WAtVA_02SP = wfvNSP + WAtVA_02SP
710200130131     c                   eval      WAtVA_02VP = wfvRCP + WAtVA_02VP
710300130131     C                   when      WFVmes =  3
710400130131     c                   eval      WAtVA_03SP = wfvNSP + WAtVA_03SP
710500130131     c                   eval      WAtVA_03VP = wfvRCP + WAtVA_03VP
710600130131     C                   when      WFVmes =  4
710700130131     c                   eval      WAtVA_04SP = wfvNSP + WAtVA_04SP
710800130131     c                   eval      WAtVA_04VP = wfvRCP + WAtVA_04VP
710900130131     C                   when      WFVmes =  5
711000130131     c                   eval      WAtVA_05SP = wfvNSP + WAtVA_05SP
711100130131     c                   eval      WAtVA_05VP = wfvRCP + WAtVA_05VP
711200130131     C                   when      WFVmes =  6
711300130131     c                   eval      WAtVA_06SP = wfvNSP + WAtVA_06SP
711400130131     c                   eval      WAtVA_06VP = wfvRCP + WAtVA_06VP
711500130131     C                   when      WFVmes =  7
711600130131     c                   eval      WAtVA_07SP = wfvNSP + WAtVA_07SP
711700130131     c                   eval      WAtVA_07VP = wfvRCP + WAtVA_07VP
711800130131     C                   when      WFVmes =  8
711900130131     c                   eval      WAtVA_08SP = wfvNSP + WAtVA_08SP
712000130131     c                   eval      WAtVA_08VP = wfvRCP + WAtVA_08VP
712100130131     C                   when      WFVmes =  9
712200130131     c                   eval      WAtVA_09SP = wfvNSP + WAtVA_09SP
712300130131     c                   eval      WAtVA_09VP = wfvRCP + WAtVA_09VP
712400130131     C                   when      WFVmes = 10
712500130131     c                   eval      WAtVA_10SP = wfvNSP + WAtVA_10SP
712600130131     c                   eval      WAtVA_10VP = wfvRCP + WAtVA_10VP
712700130131     C                   when      WFVmes = 11
712800130131     c                   eval      WAtVA_11SP = wfvNSP + WAtVA_11SP
712900130131     c                   eval      WAtVA_11VP = wfvRCP + WAtVA_11VP
713000130131     C                   when      WFVmes = 12
713100130131     c                   eval      WAtVA_12SP = wfvNSP + WAtVA_12SP
713200130131     c                   eval      WAtVA_12VP = wfvRCP + WAtVA_12VP
713300130131     c                   endSL
713400130131     C*
713500130131     C                   eval      WAtVAT_SP  = wfvNSP + WAtVAT_SP
713600130131     C                   eval      WAtVAT_VP  = wfvRCP + WAtVAT_VP
713700130131     C*
713800130131     C                   END
713900130131     C*
714000130131     C*  cancella il record già incasellato in modo da non riutilizzarlo
714100130131     C*   più in seguito in sommatorie errate
714200130131     c                   delete    WFDEAV00
714300130131     C*
714400130131     c     k_deaV1       reade     wfDEAv1l
714500130131     C                   ENDDO
714600130131     C*
714700130131     C*
714800130131     C* lo fa solo la prima, primissima volta e basta
714900130131     c                   if        deve_scriverlo = 'S'
715000130131     c                   write     WADELVT0
715100130131     c                   end
715200130131     C*
715300130131     C*
715400000000     C                   ENDSR
715500130131     C*------------------------------------------------------------------------*
715600000000**   DMM - DECODIFICHE MESI DELL'ANNO
715700000000GEN  1
715800000000FEB  2
715900000000MAR  3
716000000000APR  4
716100000000MAG  5
716200000000GIU  6
716300000000LUG  7
716400000000AGO  8
716500000000SET  9
716600000000OTT  10
716700000000NOV  11
716800000000DIC  12
716900000000**   DST - DECODIFICHE STATISTICHE
717000000000PER FILIALE              1
717100000000PER AREA                 2
717200000000PER DIVISIONE            3
717300000000                         4
717400000000                         5
717500000000**   DRI - DECODIFICHE RIGA DI STAMPA
717600000000TUTTI                    1
717700000000TUTTE                    2
7178000000003
717900950208NUOVI                    4
718000950208ITALIA                   5
718100950208ESTERO                   6
718200950602ITALIA+ESTERO            7
718300020318ACQUIS.                  8
7184000000009
71850000000010
71860000000011
718700020220TOTALE                  12
718800000000CLIENTI                 13
718900000000SPEDIZ                  14
719000000000RICAVO                  15
719100000000DELTA                   16
719200000000NUOVO                   17
719300000000ACQUISITO               18
71940002022119
719500000000      C L I E N T I     20
719600000000 C O M M E R C I A L E  21
719700000000     F I L I A L E      22
719800000000       A R E A          23
719900000000   D I V I S I O N E    24
720000000000    G E N E R A L E     25
72010000082926
72020000082927
720300000829UNIFICATI:               28
720400000829PER CLIENTE              29
720500000829PER CLIENTE UNIFICANTE   30
720600020318**   DNTW - DECODIFICHE NETWORK
720700020318                          1
720800020318EEX                       2
720900020318DPD                       3
721000020318FED                       4
721100020318EUP                       5
