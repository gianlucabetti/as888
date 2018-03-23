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
086100000000     C*------------------------------------------------------------------------*
086200000000     C*?MAIN LINE
086300000000     C*------------------------------------------------------------------------*
086400000000     C*
086500000000     C* STAMPA PARAMETRI FILTRO
086600000000     C                   EXSR      STAPAR
086700000000     C*
086800000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
086900000000     C                   EXSR      SETFIL
087000000000     C*
087100000000     C* CICLO FINO A FINE FILE DI LAVORO
087200000000DO  1C     $FINE         DOWEQ     '0'
087300000000     C*
087400000000     C* OPERAZIONI PER NUOVA DIVISIONE
087500000000     C                   EXSR      NEWDIV
087600000000     C*
087700000000     C* CICLO FINO A ROTTURA DI DIVISIONE
087800000000DO  2C     $FINE         DOWEQ     '0'
087900000000     C     WFDDIV        ANDEQ     DEPDIV
088000000000     C*
088100000000     C* OPERAZIONI PER NUOVA AREA
088200000000     C                   EXSR      NEWARE
088300000000     C*
088400000000     C* CICLO FINO A ROTTURA DI AREA
088500000000DO  3C     $FINE         DOWEQ     '0'
088600000000     C     WFDDIV        ANDEQ     DEPDIV
088700000000     C     WFDARE        ANDEQ     DEPARE
088800000000     C*
088900000000     C* OPERAZIONI PER NUOVA FILIALE
089000000000     C                   EXSR      NEWFIL
089100000000     C*
089200000000     C* CICLO FINO A ROTTURA DI FILIALE
089300000000DO  4C     $FINE         DOWEQ     '0'
089400000000     C     WFDDIV        ANDEQ     DEPDIV
089500000000     C     WFDARE        ANDEQ     DEPARE
089600000000     C     WFDFIL        ANDEQ     DEPFIL
089700000000     C*
089800000000     C* OPERAZIONI PER NUOVO AGENTE
089900000000     C                   EXSR      NEWAGE
090000000000     C*
090100000000     C* CICLO FINO A ROTTURA DI AGENTE
090200000000DO  5C     $FINE         DOWEQ     '0'
090300000000     C     WFDDIV        ANDEQ     DEPDIV
090400000000     C     WFDARE        ANDEQ     DEPARE
090500000000     C     WFDFIL        ANDEQ     DEPFIL
090600000000     C     WFDAGE        ANDEQ     DEPAGE
090700000000     C*
090800120131     c* OPERAZIONI PER NUOVO CLIENTE
090900000000     C                   EXSR      NEWKSC
091000000000     C*
091100000000     C* CICLO FINO A ROTTURA DI CLIENTE
091200000000DO  6C     $FINE         DOWEQ     '0'
091300000000     C     WFDDIV        ANDEQ     DEPDIV
091400000000     C     WFDARE        ANDEQ     DEPARE
091500000000     C     WFDFIL        ANDEQ     DEPFIL
091600000000     C     WFDAGE        ANDEQ     DEPAGE
091700030924     C     WFDCLI        ANDEQ     DEPCLI
091800030924     C*
091900030924     C* OPERAZIONI PER PASSAGGIO LETTURA SALDI PRECEDENTI --> SALDI CORRENTI
092000030924     C                   EXSR      NEWPPC
092100030924     C* MEMORIZZA I SALDI
092200030924     C                   EXSR      MEMSAL
092300000000     C*
092400000000     C* LEGGE IL RECORD SUCCESSIVO
092500000000     C                   EXSR      LETFIL
092600000000     C*
092700000000E   6C                   ENDDO
092800000000     C*
092900000000     C* STAMPA IL TOTALE CLIENTE
093000000000     C                   EXSR      STAKSC
093100000000E   5C                   ENDDO
093200000000     C*
093300000000     C* STAMPA IL TOTALE COMMERCIALE
093400000000     C                   EXSR      STAAGE
093500000000E   4C                   ENDDO
093600000000     C*
093700000000     C* STAMPA IL TOTALE FILIALE
093800000000     C                   EXSR      STAFIL
093900000000E   3C                   ENDDO
094000000000     C*
094100000000     C* STAMPA IL TOTALE AREA
094200000000     C                   EXSR      STAARE
094300000000E   2C                   ENDDO
094400000000     C*
094500000000     C* STAMPA IL TOTALE DIVISIONE
094600000000     C                   EXSR      STADIV
094700000000E   1C                   ENDDO
094800000000     C*
094900000000     C* STAMPA IL TOTALE GENERALE
095000000000     C                   EXSR      STAGEN
095100000000     C*
095200000000     C* STAMPA RIGA FINE STAMPA
095300100129     c                   if        parSTAMPA <> 'N'
095400030311     C                   WRITE     SE94LE
095500020116     C                   WRITE     SE94FI
095600100129     c                   end
095700980710     C* OPERAZIONI FINALI
095800980710     C                   EXSR      FINSR
095900000000     C*
096000000000     C                   SETON                                        LR
096100000000     C*------------------------------------------------------------------------*
096200000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
096300000000     C*------------------------------------------------------------------------*
096400000000     C     SETFIL        BEGSR
096500980529     C*
096600000000     C                   MOVEL     *BLANKS       KWDDIV                         *DIVISIONE
096700000000     C                   Z-ADD     *ZEROS        KWDARE                         *AREA
096800000000     C                   Z-ADD     *ZEROS        KWDFIL                         *FILIALE
096900000000     C                   Z-ADD     *ZEROS        KWDAGE                         *COMMERCIALE
097000960710     C                   MOVEL     *BLANKS       KWDCLV                         *CODICE QUALITA'
097100000825     C                   Z-ADD     *ZEROS        KWDCLI                         *CLIENTE
097200000000     C                   Z-ADD     *ZEROS        KWDANN                         *ANNO
097300000000     C                   Z-ADD     *ZEROS        KWDMES                         *MESE
097400100127     C     KEYDEL        SETLL     WFDEA01L                           99        *FINE ARCHIVIO
097500951219IF  1C     *IN99         IFEQ      *ON
097600000000     C                   MOVEL     '1'           $FINE
097700000000X   1C                   ELSE
097800000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
097900000000E   1C                   ENDIF
098000000000     C*
098100000000     C                   ENDSR
098200000000     C*------------------------------------------------------------------------*
098300000000     C*?LETFIL - LETTURA PROSSIMO RECORD DEL WORK FILE
098400000000     C*------------------------------------------------------------------------*
098500000000     C     LETFIL        BEGSR
098600000000     C*
098700000000     C* LEGGO FINO A:
098800000000     C*     - FINE FILE
098900000000     C*     - TROVATO RECORD VALIDO
099000000000     C                   MOVEL     'N'           WRECOK
099100000000DO  1C     $FINE         DOWEQ     '0'
099200000000     C     WRECOK        ANDEQ     'N'
099300000000     C*
099400000000     C* LETTURE SUCCESSIVE ALLA PRIMA
099500100127     C                   READ      WFDEA01L                               99
099600000000     C*
099700951219IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
099800000000     C                   MOVEL     '1'           $FINE
099900000000X   2C                   ELSE
100000000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
100100000000E   2C                   ENDIF
100200000000     C*
100300000000E   1C                   ENDDO
100400000000     C*
100500000000     C                   ENDSR
100600000000     C*------------------------------------------------------------------------*
100700000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD DEL WORK FILE
100800000000     C*------------------------------------------------------------------------*
100900000000     C     CHKREC        BEGSR
101000000000     C*
101100000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
101200000000     C*
101300990609IF  1C     WFDDAT        IFGE      PARDPT
101400990609     C     WFDDAT        ANDLE     PARDP2
101500951219     C                   MOVEL     'P'           TIPANN                         *PERIODO PRECEDENTE
101600071123     C*
101700071123     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
101800071123     C* a cambio di codice cliente. Essendo comunuque il periodo Precedente deve
101900071123     C* essere considerato ACQUISITO E NON NUOVO e SE LO E' RIMANE tale anche
102000071123     C* PER TUTTO IL PERIODO CORRENTE
102100071123IF  2C                   If        FlaPAS = '0'
102200071123     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
102300071123     C                   clear                   DESCLN                         *NUOVO O NON NUOVO
102400071123E   2C                   EndIF
102500071123      *
102600951219E   1C                   ENDIF
102700071123      *
102800990609IF  1C     WFDDAT        IFGE      PARDAT
102900990609     C     WFDDAT        ANDLE     PARDA2
103000951219     C                   MOVEL     'C'           TIPANN                         *PERIODO CORRENTE
103100000000     C*
103200000000     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
103300000000     C* VEDO SE IL CLIENTE E' NUOVO (PER FORZA IL PRIMO SALDO); SE LO E' RIMANE
103400000000     C* TALE PER TUTTO IL PERIODO CORRENTE
103500071123     C* Attenzione: poichè se il codice cliente precedentemente letto aveva solo
103600071123     C*  solo dati su Anno precedente e il cliente appena letto è nuovo per anno
103700071123     C*  in corso sovrappone il Nuovo/Acquisito su DESCLN poichè si ritrovava il FLAPAS = '0'
103800071123     C*  per proteggersi da tale problema occorre controllare che il test venga fatto
103900071123     C*  quando non si è alivello di rottura livello cliente.
104000000000IF  2C     FLAPAS        IFEQ      '0'
104100071123     c                   if        wfdCLI = depCLI
104200000000     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
104300000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O NON NUOVO
104400071123     c                   end
104500000000E   2C                   ENDIF
104600070822      *
104700070822      *  Può esistere il caso che il cliente sia nuovo nel mese di febbraio x ESTERO
104800070822      *  ed abbia fatturato Italia solo in giugno.
104900070822      *  Lanciando la stampa come Italia l'informazione letta, ossia il record di giugno, non
105000070822      *  avrà "Y" come NUOVO ma avrà "S".
105100070822      *  Il programma necessita della "Y" nei suoi test per considerare il cliente come NUOVO.
105200070822      *   --> Occorre memorizzarsi qui comunque cosa vale il flag del primo record letto sia
105300070822      *   che sia Italia/Estero per poi forzarlo nella routine di decodifica del cliente.
105400070822IF  2C     wfdCLI        ifne      sav_depCLI
105500070822IF  2C     wfdIOE        andne     parIOE
105600070822IF  2C     parIOE        andne     *blank
105700070822      *
105800070822      * lo salva per poterlo forzare in seguito
105900070822     c                   clear                   sav_desCLN_Y
106000070822IF  2C                   if        sav_depCLI <> wfdCLI
106100070822IF  2C                                       and wfdCLN ='Y'
106200070822     c                   move      'Y'           sav_desCLN_Y
106300070822     c                   end
106400070822      *
106500070822     c                   eval      sav_depCLI  =  wfdCLI
106600070822     c                   end
106700070822      *
106800000000E   1C                   ENDIF
106900070822      *
107000951018     C*--------------------
107100951018     C* SELEZIONI
107200951018     C*--------------------
107300951018     C* SE FILIALE CLIENTE <> FILIALE RICHIESTA, ESCLUDO
107400951018IF  1C     PARFIC        IFGT      *ZEROS                                       *DELTA FILIALE
107500951018     C     WFDFIL        ANDNE     PARFIC
107600951018     C                   MOVEL     'N'           WRECOK
107700951018E   1C                   ENDIF
107800951018     C*
107900951018     C* SE AREA CLIENTE <> AREA RICHIESTA, ESCLUDO
108000951018IF  1C     PARARC        IFGT      *ZEROS                                       *DELTA FILIALE
108100951018     C     WFDARE        ANDNE     PARARC
108200951018     C                   MOVEL     'N'           WRECOK
108300951018E   1C                   ENDIF
108400951018     C*
108500030924     C* SE DIVISIONE CLIENTE <> DIVISIONE RICHIESTA, ESCLUDO
108600951018IF  1C     PARDIC        IFNE      *BLANKS                                      *DELTA FILIALE
108700030924     C     WFDDIV        ANDNE     PARDIC
108800030924     C                   MOVEL     'N'           WRECOK
108900030924E   1C                   ENDIF
109000030924     C*
109100951018     C* SE CODICE QUALITA' CLIENTE <> CODICE QUALITA' RICHIESTO, ESCLUDO
109200010618IF  1C                   IF        (PARCLV <> *blanks and WFDCLV = PARCLV) or
109300010618     C                             (PARCL1 <> *blanks and WFDCLV = PARCL1) or
109400030924     C                             (PARCL2 <> *blanks and WFDCLV = PARCL2) or
109500030924     C                             (PARCLV = *blanks  and
109600030924     C                              PARCL1 = *blanks  and
109700030924     C                              PARCL2 = *blanks)
109800030924     C                   ELSE
109900951018     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
110000951018E   1C                   ENDIF
110100000000     C*
110200951018     C* SE TIPO DATI STATISTCA ITALIA/ESTERO <> TIPO DATI RICHIESTO, ESCLUDO
110300950210IF  1C     PARIOE        IFNE      *BLANKS
110400950210     C     PARIOE        ANDNE     WFDIOE
110500950208     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
110600950210E   1C                   ENDIF
110700000707     C*
110800000707     C* SE DATI NO POSTE ESCLUDE LE FILIALI POSTE, E VICEVERSA
110900000707IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
111000000707     C                   SETOFF                                           28
111100000707     C                   Z-ADD     1             I                 5 0
111200000707     C     WFDFIL        LOOKUP    FIL(I)                                 28
111300000707IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
111400020627IF  3C     POS(I)        IFEQ      'PPT'                                         FIL = POSTE
111500000707     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
111600000711E   2C                   ENDIF
111700000711X   2C                   ELSE                                                   *SI DATI POSTE
111800020627IF  3C     POS(I)        IFNE      'PPT'                                         FIL <> POSTE
111900000707     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
112000000711E   3C                   ENDIF
112100000711E   2C                   ENDIF
112200000707E   1C                   ENDIF
112300980528     C*
112400000000     C                   ENDSR
112500000000     C*------------------------------------------------------------------------*
112600000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
112700000000     C*------------------------------------------------------------------------*
112800000000     C     NEWDIV        BEGSR
112900020220     C*
113000020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
113100020220     C                   MOVEL     '0'           FLGSIDD
113200000000     C*
113300000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
113400000000     C                   MOVEL     '0'           PASDIV
113500000000     C*
113600000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
113700000000     C                   MOVEL     WFDDIV        DEPDIV
113800000000     C*
113900000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
114000000000     C                   EXSR      DECDIV
114100000000     C*
114200020318     C* AZZERA I TOTALI DELLA DIVISIONE (X QUALITA' CLIENTE)
114300020221DO  1C     1             DO        5             Q
114400000000     C     Q             OCCUR     ABCDIV
114500000000     C                   CLEAR                   CND
114600000000     C                   CLEAR                   SND
114700000000     C                   CLEAR                   RND
114800000000     C                   CLEAR                   PND
114900000000     C                   CLEAR                   CAD
115000000000     C                   CLEAR                   SAD
115100000000     C                   CLEAR                   RAD
115200000000     C                   CLEAR                   PAD
115300000000E   1C                   ENDDO
115400000000     C*
115500000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
115600020221DO  1C     1             DO        5             Q
115700000000     C     Q             OCCUR     ABCD13
115800000000     C                   CLEAR                   NTD
115900000000     C                   CLEAR                   ATD
116000000000E   1C                   ENDDO
116100020318     C*
116200020318     C* AZZERA I TOTALI DELLA DIVISIONE (X NETWORK) NN "D"
116300020318DO  1C     1             DO        5             K
116400020318     C     K             OCCUR     ABCDIVN
116500020318     C                   CLEAR                   CNDN
116600020318     C                   CLEAR                   SNDN
116700020318     C                   CLEAR                   RNDN
116800020318     C                   CLEAR                   PNDN
116900020318     C                   CLEAR                   CADN
117000020318     C                   CLEAR                   SADN
117100020318     C                   CLEAR                   RADN
117200020318     C                   CLEAR                   PADN
117300020318E   1C                   ENDDO
117400020318     C*
117500020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
117600020318DO  1C     1             DO        5             K
117700020318     C     K             OCCUR     ABCD13N
117800020318     C                   CLEAR                   NTDN
117900020318     C                   CLEAR                   ATDN
118000020318E   1C                   ENDDO
118100020318     C*
118200020318     C* AZZERA I TOTALI DELLA DIVISIONE (X NETWORK) "D"
118300020318DO  1C     1             DO        5             K
118400020318     C     K             OCCUR     ABCDIVND
118500020318     C                   CLEAR                   CNDND
118600020318     C                   CLEAR                   SNDND
118700020318     C                   CLEAR                   RNDND
118800020318     C                   CLEAR                   PNDND
118900020318     C                   CLEAR                   CADND
119000020318     C                   CLEAR                   SADND
119100020318     C                   CLEAR                   RADND
119200020318     C                   CLEAR                   PADND
119300020318E   1C                   ENDDO
119400020318     C*
119500020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
119600020318DO  1C     1             DO        5             K
119700020318     C     K             OCCUR     ABCD13ND
119800020318     C                   CLEAR                   NTDND
119900020318     C                   CLEAR                   ATDND
120000020318E   1C                   ENDDO
120100000000     C*
120200000000     C                   ENDSR
120300000000     C*------------------------------------------------------------------------*
120400000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
120500000000     C*------------------------------------------------------------------------*
120600000000     C     NEWARE        BEGSR
120700020220     C*
120800020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
120900020220     C                   MOVEL     '0'           FLGSIDA
121000000000     C*
121100000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
121200000000     C                   MOVEL     '0'           PASARE
121300000000     C*
121400000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
121500000000     C                   MOVEL     WFDARE        DEPARE
121600000000     C*
121700000000     C* DECODIFICA IL CODICE AREA
121800000000     C                   EXSR      DECARE
121900000000     C*
122000020318     C* AZZERA I TOTALI DELL'AREA (X QUALITA' CLIENTE)
122100020221DO  1C     1             DO        5             Q
122200000000     C     Q             OCCUR     ABCARE
122300000000     C                   CLEAR                   CNA
122400000000     C                   CLEAR                   SNA
122500000000     C                   CLEAR                   RNA
122600000000     C                   CLEAR                   PNA
122700000000     C                   CLEAR                   CAA
122800000000     C                   CLEAR                   SAA
122900000000     C                   CLEAR                   RAA
123000000000     C                   CLEAR                   PAA
123100000000E   1C                   ENDDO
123200000000     C*
123300000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
123400020221DO  1C     1             DO        5             Q
123500000000     C     Q             OCCUR     ABCA13
123600000000     C                   CLEAR                   NTA
123700000000     C                   CLEAR                   ATA
123800000000E   1C                   ENDDO
123900020318     C*
124000020318     C* AZZERA I TOTALI DELL'AREA (X NETWORK) NN "D"
124100020318DO  1C     1             DO        5             K
124200020318     C     K             OCCUR     ABCAREN
124300020318     C                   CLEAR                   CNAN
124400020318     C                   CLEAR                   SNAN
124500020318     C                   CLEAR                   RNAN
124600020318     C                   CLEAR                   PNAN
124700020318     C                   CLEAR                   CAAN
124800020318     C                   CLEAR                   SAAN
124900020318     C                   CLEAR                   RAAN
125000020318     C                   CLEAR                   PAAN
125100020318E   1C                   ENDDO
125200020318     C*
125300020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
125400020318DO  1C     1             DO        5             K
125500020318     C     K             OCCUR     ABCA13N
125600020318     C                   CLEAR                   NTAN
125700020318     C                   CLEAR                   ATAN
125800020318E   1C                   ENDDO
125900020318     C*
126000020318     C* AZZERA I TOTALI DELL'AREA (X NETWORK) "D"
126100020318DO  1C     1             DO        5             K
126200020318     C     K             OCCUR     ABCAREND
126300020318     C                   CLEAR                   CNAND
126400020318     C                   CLEAR                   SNAND
126500020318     C                   CLEAR                   RNAND
126600020318     C                   CLEAR                   PNAND
126700020318     C                   CLEAR                   CAAND
126800020318     C                   CLEAR                   SAAND
126900020318     C                   CLEAR                   RAAND
127000020318     C                   CLEAR                   PAAND
127100020318E   1C                   ENDDO
127200020318     C*
127300020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
127400020318DO  1C     1             DO        5             K
127500020318     C     K             OCCUR     ABCA13ND
127600020318     C                   CLEAR                   NTAND
127700020318     C                   CLEAR                   ATAND
127800020318E   1C                   ENDDO
127900000000     C*
128000000000     C                   ENDSR
128100000000     C*------------------------------------------------------------------------*
128200000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
128300000000     C*------------------------------------------------------------------------*
128400000000     C     NEWFIL        BEGSR
128500020220     C*
128600020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
128700020220     C                   MOVEL     '0'           FLGSIDF
128800000000     C*
128900000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
129000000000     C                   MOVEL     '0'           PASFIL
129100000000     C*
129200000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
129300000000     C                   Z-ADD     WFDFIL        DEPFIL
129400000000     C*
129500000000     C* DECODIFICA IL CODICE FILIALE
129600000000     C                   EXSR      DECFIL
129700000000     C*
129800020318     C* AZZERA I TOTALI DELLA FILIALE (X QUALITA' CLIENTE)
129900020221DO  1C     1             DO        5             Q
130000000000     C     Q             OCCUR     ABCFIL
130100000000     C                   CLEAR                   CNF
130200000000     C                   CLEAR                   SNF
130300000000     C                   CLEAR                   RNF
130400000000     C                   CLEAR                   PNF
130500000000     C                   CLEAR                   CAF
130600000000     C                   CLEAR                   SAF
130700000000     C                   CLEAR                   RAF
130800000000     C                   CLEAR                   PAF
130900000000E   1C                   ENDDO
131000000000     C*
131100000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
131200020221DO  1C     1             DO        5             Q
131300000000     C     Q             OCCUR     ABCF13
131400000000     C                   CLEAR                   NTF
131500000000     C                   CLEAR                   ATF
131600000000E   1C                   ENDDO
131700020318     C*
131800020318     C* AZZERA I TOTALI DELLA FILIALE (X NETWORK) NN "D"
131900020318DO  1C     1             DO        5             K
132000020318     C     K             OCCUR     ABCFILN
132100020318     C                   CLEAR                   CNFN
132200020318     C                   CLEAR                   SNFN
132300020318     C                   CLEAR                   RNFN
132400020318     C                   CLEAR                   PNFN
132500020318     C                   CLEAR                   CAFN
132600020318     C                   CLEAR                   SAFN
132700020318     C                   CLEAR                   RAFN
132800020318     C                   CLEAR                   PAFN
132900020318E   1C                   ENDDO
133000020318     C*
133100020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
133200020318DO  1C     1             DO        5             K
133300020318     C     K             OCCUR     ABCF13N
133400020318     C                   CLEAR                   NTFN
133500020318     C                   CLEAR                   ATFN
133600020318E   1C                   ENDDO
133700020318     C*
133800020318     C* AZZERA I TOTALI DELLA FILIALE (X NETWORK) "D"
133900020318DO  1C     1             DO        5             K
134000020318     C     K             OCCUR     ABCFILND
134100020318     C                   CLEAR                   CNFND
134200020318     C                   CLEAR                   SNFND
134300020318     C                   CLEAR                   RNFND
134400020318     C                   CLEAR                   PNFND
134500020318     C                   CLEAR                   CAFND
134600020318     C                   CLEAR                   SAFND
134700020318     C                   CLEAR                   RAFND
134800020318     C                   CLEAR                   PAFND
134900020318E   1C                   ENDDO
135000020318     C*
135100020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
135200020318DO  1C     1             DO        5             K
135300020318     C     K             OCCUR     ABCF13ND
135400020318     C                   CLEAR                   NTFND
135500020318     C                   CLEAR                   ATFND
135600020318E   1C                   ENDDO
135700000000     C*
135800000000     C                   ENDSR
135900000000     C*------------------------------------------------------------------------*
136000000000     C*?NEWAGE - OPERAZIONI PER NUOVO AGENTE COMMERCIALE
136100000000     C*------------------------------------------------------------------------*
136200000000     C     NEWAGE        BEGSR
136300020220     C*
136400030924     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
136500030924     C                   MOVEL     '0'           FLGSIDE
136600030924     C*
136700030924     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
136800030924     C                   MOVEL     '0'           PASAGE
136900030924     C*
137000030924     C* MEMORIZZO IL NUOVO AGENTE IN UN DEPOSITO
137100030924     C                   Z-ADD     WFDAGE        DEPAGE
137200081013     C*
137300081013     C* l'Agente Unificante e la sua descrizione
137400081013     C                   Z-ADD     WFDrgf        DEPrgf
137500081013     C                   movel     WFDrgd        DEPrgd
137600081013     C*
137700000000     C* DECODIFICO IL CODICE AGENTE
137800000000     C                   EXSR      DECAGE
137900000000     C*
138000020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X QUALITA' CLIENTE)
138100020221DO  1C     1             DO        5             Q
138200000000     C     Q             OCCUR     ABCCOM
138300000000     C                   CLEAR                   CNC
138400000000     C                   CLEAR                   SNC
138500000000     C                   CLEAR                   RNC
138600000000     C                   CLEAR                   PNC
138700000000     C                   CLEAR                   CAC
138800000000     C                   CLEAR                   SAC
138900000000     C                   CLEAR                   RAC
139000000000     C                   CLEAR                   PAC
139100000000E   1C                   ENDDO
139200000000     C*
139300000000     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
139400020221DO  1C     1             DO        5             Q
139500000000     C     Q             OCCUR     ABCC13
139600000000     C                   CLEAR                   NTC
139700000000     C                   CLEAR                   ATC
139800000000E   1C                   ENDDO
139900020318     C*
140000020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X NETWORK) NN "D"
140100020318DO  1C     1             DO        5             K
140200020318     C     K             OCCUR     ABCCOMN
140300020318     C                   CLEAR                   CNCN
140400020318     C                   CLEAR                   SNCN
140500020318     C                   CLEAR                   RNCN
140600020318     C                   CLEAR                   PNCN
140700020318     C                   CLEAR                   CACN
140800020318     C                   CLEAR                   SACN
140900020318     C                   CLEAR                   RACN
141000020318     C                   CLEAR                   PACN
141100020318E   1C                   ENDDO
141200020318     C*
141300020318     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
141400020318DO  1C     1             DO        5             K
141500020318     C     K             OCCUR     ABCC13N
141600020318     C                   CLEAR                   NTCN
141700020318     C                   CLEAR                   ATCN
141800020318E   1C                   ENDDO
141900020318     C*
142000020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X NETWORK) "D"
142100020318DO  1C     1             DO        5             K
142200020318     C     K             OCCUR     ABCCOMND
142300020318     C                   CLEAR                   CNCND
142400020318     C                   CLEAR                   SNCND
142500020318     C                   CLEAR                   RNCND
142600020318     C                   CLEAR                   PNCND
142700020318     C                   CLEAR                   CACND
142800020318     C                   CLEAR                   SACND
142900020318     C                   CLEAR                   RACND
143000020318     C                   CLEAR                   PACND
143100020318E   1C                   ENDDO
143200020318     C*
143300020318     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
143400020318DO  1C     1             DO        5             K
143500020318     C     K             OCCUR     ABCC13ND
143600020318     C                   CLEAR                   NTCND
143700020318     C                   CLEAR                   ATCND
143800020318E   1C                   ENDDO
143900000000     C*
144000000000     C                   ENDSR
144100000000     C*------------------------------------------------------------------------*
144200000000     C*?NEWKSC - OPERAZIONI PER NUOVO CLIENTE
144300000000     C*------------------------------------------------------------------------*
144400000000     C     NEWKSC        BEGSR
144500020221     C*
144600020221     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
144700020221     C                   MOVEL     '0'           FLGSIDK
144800000000     C*
144900000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
145000000000     C                   MOVEL     '0'           PASKSC
145100000000     C*
145200951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (13^ COLONNA) PERIODO CORRENTE
145300000000     C                   MOVEL     '0'           FLAKSC
145400951227     C*
145500951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (1^ COLONNA) PERIODO PRECEDENTE
145600951227     C                   MOVEL     '0'           FL2KSC
145700000000     C*
145800000000     C* REIMPOSTO FLAG PASSAGGIO LETTURA SALDI PRECEDENTI A SALDI CORRENTI
145900000000     C                   MOVEL     '0'           FLAPAS
146000000830     C*
146100000830     C* REIMPOSTO FLAG CONTATORE CLIENTI PER MESE
146200000830     C                   MOVEL     '0'           SFCCL
146300000000     C*
146400000000     C* MEMORIZZO IL NUOVO CLIENTE IN UN DEPOSITO
146500000825     C                   Z-ADD     WFDCLI        DEPCLI
146600020319     C*
146700020319     C* RE-INIZIALIZZO LA SCHIERA DI WRK X CLIENTE/MESE/NETWORK
146800020319     C                   Z-ADD     *ZEROS        Y
146900020319     C                   CLEAR                   WSK1
147000000000     C*
147100000000     C* DECODIFICO IL CODICE CLIENTE
147200000000     C                   EXSR      DECKSC
147300000000     C*
147400000000     C* AZZERA I TOTALI DEL CLIENTE
147500121217     C                   CLEAR                   CLN1K
147600000000     C                   CLEAR                   S1K
147700081013     C                   CLEAR                   Kg1K
147800000000     C                   CLEAR                   R1K
147900000000     C                   CLEAR                   P1K
148000970822     C                   CLEAR                   D1K
148100020712     C                   CLEAR                   R1KP
148200070419     C                   CLEAR                   ap_S1K
148300081013     C                   CLEAR                   ap_Kg1K
148400070419     C                   CLEAR                   ap_R1K
148500070419     C                   CLEAR                   ap_P1K
148600091123      * -------------
148700100204     /*?  solo x per l'Estero                                   ?
148800100204     /*?     x Network                                          ?
148900100204     /*?   Pulisce le schiere di totale                         ?
149000091123      * -------------
149100091123IF  1C     PARIOE        IFEQ      'E'
149200091123      * -------------
149300091123DO  1C     1             DO        5             K
149400100204     C* AZZERA I TOTALI x NETWORK anno in corso
149500091124     C     K             OCCUR     W1ntw
149600091124     C                   CLEAR                   C1ntw
149700091124     C                   CLEAR                   S1ntw
149800091124     C                   CLEAR                   KG1ntw
149900091124     C                   CLEAR                   R1ntw
150000091124     C                   CLEAR                   P1ntw
150100091124     C                   CLEAR                   D1ntw
150200091124     C                   CLEAR                   R1Pntw
150300091125     C                   CLEAR                   TR1ntw
150400091125     C                   CLEAR                   TP1ntw
150500091124     C* AZZERA I TOTALI x NETWORK  dell'Anno precedente
150600091124     C     K             OCCUR     Wap1ntw
150700091124     C                   CLEAR                   ap_C1ntw
150800091124     C                   CLEAR                   ap_S1ntw
150900091124     C                   CLEAR                   ap_KG1ntw
151000091124     C                   CLEAR                   ap_R1ntw
151100091124     C                   CLEAR                   ap_P1ntw
151200091124     C                   CLEAR                   ap_D1ntw
151300091124     C                   CLEAR                   ap_R1Pntw
151400091125     C                   CLEAR                   ap_TR1ntw
151500091125     C                   CLEAR                   ap_TP1ntw
151600091123E   1C                   ENDDO
151700091123      * -------------
151800091123E   1C                   END
151900091123      * -------------
152000100204     /*?  Pulizia schiere di totali                             ?
152100100204     /*?    x  ntw e tipi servizio                              ?
152200100204     /*?      sia Italia che Estero                             ?
152300100129      * -------------
152400100129DO  1C     1             DO        50            Kserv             3 0
152500100201     C* AZZERA I TOTALI x NETWORK/TIPO SERVIZIO
152600100129     C     Kserv         OCCUR     W1ntwTS
152700100202      * chiave NTW + TServ
152800100129     C                   CLEAR                   C1ntwTS
152900100201     C                   CLEAR                   C1tpsTS
153000100202     C* AZZERA I TOTALI x NETWORK  dell'Anno in Corso
153100100129     C                   CLEAR                   S1ntwTS
153200100129     C                   CLEAR                   KG1ntwTS
153300100129     C                   CLEAR                   R1ntwTS
153400100129     C                   CLEAR                   P1ntwTS
153500100129     C                   CLEAR                   D1ntwTS
153600100129     C                   CLEAR                   R1PntwTS
153700100129     C                   CLEAR                   TR1ntwTS
153800100129     C                   CLEAR                   TP1ntwTS
153900100129     C* AZZERA I TOTALI x NETWORK  dell'Anno precedente
154000100129     C                   CLEAR                   ap_S1ntwTS
154100100129     C                   CLEAR                   ap_KG1ntwTS
154200100129     C                   CLEAR                   ap_R1ntwTS
154300100129     C                   CLEAR                   ap_P1ntwTS
154400100129     C                   CLEAR                   ap_D1ntwTS
154500100129     C                   CLEAR                   ap_R1PntwTS
154600100129     C                   CLEAR                   ap_TR1ntwTS
154700100129     C                   CLEAR                   ap_TP1ntwTS
154800100202     C*
154900100129E   1C                   ENDDO
155000100129      * -------------
155100000828     C* CERCA I FIGLI DEL CLIENTE, SE RICHIESTA STAMPA PER UNFICANTE
155200000828     C                   CLEAR                   SKSC11
155300000828IF  1C     PARSCE        IFEQ      'U'                                          *SCELTA UNIFICANTE
155400000828     C                   CLEAR                   BS10DS
155500010725     C*                  Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
155600010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
155700000828     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
155800000828     C                   MOVEL     'F'           D10PAF                         *CERCA I FIGLI
155900000828     C                   MOVE      DEPCLI        D10COD
156000000828     C                   CALL      'TIBS10R'
156100000828     C                   PARM                    BS10DS
156200000828E   1C                   ENDIF
156300000000     C*
156400130131     C*  Totalizza le Varie per CLIENTE sul nuovo WADELPCV dal WFDEAV0F
156500130131     C*   man mano che le riporta sul file di TOTALE le ELIMINA dal file di WORK.
156600130131     c                   exsr      VARIE_x_Client
156700130131     C*
156800000000     C                   ENDSR
156900000000     C*------------------------------------------------------------------------*
157000000000     C*?NEWPPC - OPERAZIONI PER PASSAGGIO LETTURA SALDI PRECED-->SALDI CORRENTI
157100000000     C*------------------------------------------------------------------------*
157200000000     C     NEWPPC        BEGSR
157300000000     C*
157400000000     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
157500000000     C* PRENDO COME "CLIENTE NUOVO/ACQUISITO" QUELLO DEL PRIMO SALDO PERCHE' SE
157600000000     C* UN CLIENTE E' NUOVO PER UN MESE ("Y") LO E' PER TUTTO IL PERIODO
157700000000IF  1C     TIPANN        IFEQ      'C'
157800000000IF  2C     FLAPAS        IFEQ      '0'
157900000000     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
158000000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O NON NUOVO
158100000000E   2C                   ENDIF
158200000000E   1C                   ENDIF
158300000000     C*
158400030924     C                   ENDSR
158500030924     C*------------------------------------------------------------------------*
158600030924     C*?MEMSAL - MEMORIZZA I SALDI DEL WORK FILE IN SCHIERE
158700030924     C*------------------------------------------------------------------------*
158800030924     C     MEMSAL        BEGSR
158900030924     C*
159000030924     C* I SALDI DEL PERIODO PRECEDENTE LI MEMORIZZA IN 13^ POSIZIONE DI SCHIERA
159100030924     C* UN CLIENTE E' NUOVO NEL PERIODO SOLO SE HA UNA "Y" NEI SALDI
159200030924     C*
159300030924     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
159400030924     C                   MOVEL     '1'           PASKSC                         *CLIENTE
159500030924     C                   MOVEL     '1'           PASAGE                         *AGENTE
159600030924     C                   MOVEL     '1'           PASFIL                         *FILIALE
159700030924     C                   MOVEL     '1'           PASARE                         *AREA
159800030924     C                   MOVEL     '1'           PASDIV                         *DIVISIONE
159900030924     C                   MOVEL     '1'           PASGEN                         *GENERALE
160000000000     C*
160100951227     C* CONTA I CLIENTI DA STAMPARE ALLA 1^ O 13^ COLONNA SOLO A ROTTURA DI
160200951227     C* CLIENTE (SE STA LEGGENDO I SALDI DEL PERIODO PRECEDENTE O CORRENTE)
160300951227IF  1C     TIPANN        IFEQ      'C'                                          *CORRENTE
160400951227IF  2C     FLAKSC        IFEQ      '0'                                          *ROTTURA CLIENTE
160500000000     C                   MOVEL     '1'           FLAKSC
160600951227     C                   Z-ADD     1             CONKSC            1 0          *CLIENTE NON AGGIUNT
160700951227X   2C                   ELSE
160800951227     C                   Z-ADD     *ZEROS        CONKSC                         *CLIENTE AGGIUNTO
160900951227E   2C                   ENDIF
161000951227X   1C                   ELSE                                                   *PRECEDENTE
161100951227IF  2C     FL2KSC        IFEQ      '0'                                          *ROTTURA CLIENTE
161200951227     C                   MOVEL     '1'           FL2KSC
161300951227     C                   Z-ADD     1             CO2KSC            1 0          *CLIENTE AGGIUNTO
161400951227X   2C                   ELSE
161500951227     C                   Z-ADD     *ZEROS        CO2KSC                         *CLIENTE NON AGGIUNT
161600951227E   2C                   ENDIF
161700951227E   1C                   ENDIF
161800020318     C*
161900020318     C* Q U A L I T A'  /  N E T W O R K
162000000000     C                   Z-ADD     1             Q
162100000000     C                   SETOFF                                           28
162200000000     C     DESCLV        LOOKUP    ABC(Q)                                 28
162300020318     C                   Z-ADD     1             K
162400020318     C                   SETOFF                                           27
162500020318     C     WFDNTW        LOOKUP    DNTW(K)                                27
162600000000IF  1C     *IN28         IFEQ      *ON
162700020318IF  1C     *IN27         ANDEQ     *ON
162800000000     C* M E S E
162900990609IF  2C     WFDDAT        IFGE      PARDPT                                       *PERIODO PRECEDENTE
163000990609     C     WFDDAT        ANDLE     PARDP2
163100000000     C                   Z-ADD     13            MM
163200020712     C                   Z-ADD     1             MMP               2 0
163300020712     C                   SETOFF                                           33
163400020712     C     WFDMES        LOOKUP    MES(MMP)                               33
163500951219E   2C                   ENDIF
163600990609IF  2C     WFDDAT        IFGE      PARDAT                                       *PERIODO CORRENTE
163700990609     C     WFDDAT        ANDLE     PARDA2
163800000000     C                   Z-ADD     1             MM
163900000000     C                   SETOFF                                           28
164000000000     C     WFDMES        LOOKUP    MES(MM)                                28
164100000000E   2C                   ENDIF
164200020220     C*
164300020220     C* VERIFICO SE ALMENO 1 CLIENTE "D"
164400020318     C                   IF        DESCLV = 'D'
164500020220     C                   MOVEL     '1'           FLGSIDG           1
164600020220     C                   MOVEL     '1'           FLGSIDD           1
164700020220     C                   MOVEL     '1'           FLGSIDA           1
164800020220     C                   MOVEL     '1'           FLGSIDF           1
164900020220     C                   MOVEL     '1'           FLGSIDE           1
165000020221     C                   MOVEL     '1'           FLGSIDK           1
165100020220     C                   ENDIF
165200020319     C*
165300020319     C* VERIFICO SE X GIA' CONTEGGIATO IL CLIENTE/MESE/NETWORK
165400020319     C     DSWSK1        LOOKUP    WSK1                                   55
165500020319     C                   IF        *IN55 = *ON
165600020319     C                   Z-ADD     *ZEROS        CONNTW            1 0
165700020319     C                   ELSE
165800020319     C                   ADD       1             Y                 3 0
165900020319     C                   EVAL      WSK1(Y) = DSWSK1
166000020319     C                   Z-ADD     1             CONNTW
166100020319     C                   ENDIF
166200000000     C*--------------------
166300020220     C* C L I E N T E
166400000000     C*--------------------
166500121217     C                   move      WFDcln        cln1K(MM)                      memorizza i flag di cliente nuovo/acq
166600000000     C                   ADD       WFDNSP        S1K(MM)                        SPEDIZIONI
166700081013     C                   eval      Kg1K(MM) = wfdPKG + Kg1K(MM)                 PESI in KG
166800000000     C                   ADD       WFDRRN        R1K(MM)                        RICAVI REALI DELTA
166900000000     C                   ADD       WFDRPN        P1K(MM)                        RICAVI PRESUNTI DELT
167000091123      * -------------
167100091123      **  solo x per l'estero incasella x Network
167200091123IF  1C     PARIOE        IFEQ      'E'
167300091124IF  1C     MM            andle     12
167400091124     C     K             OCCUR     W1ntw
167500091124     c                   movel     WFDNTW        C1ntw
167600091124     C                   ADD       WFDNSP        S1ntw(MM)                        SPEDIZIONI
167700091124     C                   eval      Kg1ntw(MM) = wfdPKG + Kg1ntw(MM)                 PESI in KG
167800091124     C                   ADD       WFDRRN        R1ntw(MM)                        RICAVI REALI DELTA
167900091124     C                   ADD       WFDRPN        P1ntw(MM)                        RICAVI PRESUNTI DELT
168000091125     C                   z-add(h)  WFDRRN        R1totale         15 0
168100091125     C                   z-add(h)  WFDRPN        P1totale         15 0
168200091125     C                   ADD       R1totale      TR1ntw
168300091125     C                   ADD       P1totale      TP1ntw
168400091123     c                   END
168500091123      **
168600970822IF  2C     MM            IFNE      13                                           SOLO PERIODO CORRENT
168700020220     C                   MOVEL     WFDSPD        D1K(MM)                        FLAG % SCOSTAM.DELTA
168800070420      *
168900070420      * ?se invece è un dato dell'anno precedente
169000070420     c                   else
169100081013      *
169200070420      * ?lo devo incasellare nel mese appropriato
169300070420     C                   ADD       WFDNSP        ap_S1K(WFDmes)                 SPEDIZIONI
169400081013     C                   eval      ap_Kg1K(WFDmes) = wfdPKG + ap_Kg1K(WFDmes)   PESI in KG
169500070420     C                   ADD       WFDRRN        ap_R1K(WFDmes)                 RICAVI REALI DELTA
169600070420     C                   ADD       WFDRPN        ap_P1K(WFDmes)                 RICAVI PRESUNTI DELT
169700091123      * -------------
169800091123      **  solo x per l'estero incasella x Network
169900091123IF  1C     PARIOE        IFEQ      'E'
170000091124IF  1C     WFDmes        andle     12
170100091124     C     K             OCCUR     WAP1ntw
170200091124     c                   movel     WFDNTW        Ap_c1ntw
170300091124     C                   eval      ap_S1ntw(WFDmes) = WFDNSP + ap_S1ntw(WFDmes)    SPEDIZIONI
170400091124     C                   eval      ap_Kg1ntw(WFDmes)= wfdPKG +ap_Kg1ntw(WFDmes)      PESI in KG
170500091124     C                   eval      ap_R1ntw(WFDmes) = WFDRRN + ap_R1ntw(WFDmes)    RICAVI REALI DELT
170600091124     C                   eval      ap_P1ntw(WFDmes) = WFDRPN + ap_P1ntw(WFDmes)    RICAVI PRESUNTI D
170700091125     C                   z-add(h)  WFDRRN        R1totale         15 0
170800091125     C                   z-add(h)  WFDRPN        P1totale         15 0
170900091125     C                   ADD       R1totale      ap_TR1ntw
171000091125     C                   ADD       P1totale      ap_TP1ntw
171100091123     c                   END
171200091123      **
171300970822E   2C                   ENDIF
171400100201      * -------------
171500100204      **
171600100204     /*?  totalizza incasellando x Network e Tipo Servizio      ?
171700100204     /*?   x Italia e poi x Estero            ......e           ?
171800100204     /*?   se RICHIESTA creazione del File PC   (WFDELPCF)      ?
171900100204IF  1C                   IF        PARIOE <> *blank and PARFPC = 'S'
172000100201     c                   exsr      TOTCLI_NTW_TS
172100100201     c                   END
172200100201      * -------------
172300070420      *
172400020712     C     TIPANN        IFEQ      'P'
172500020712     C     *IN33         ANDEQ     *ON
172600020712     C                   ADD       WFDRRN        R1KP(MMP)                      RICAVI REALI DELTA A
172700020712     C                   ENDIF
172800000000     C*--------------------
172900020220     C* C O M M E R C I A L E
173000000000     C*--------------------
173100000000     C     Q             OCCUR     ABCCOM
173200020318     C     K             OCCUR     ABCCOMN
173300020318     C     K             OCCUR     ABCCOMND
173400000000     C* NUOVO
173500000000IF  2C     DESCLN        IFEQ      'Y'
173600000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
173700000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
173800020318     C                   ADD       1             CNC(MM)                        NUMERO CLIENTI
173900000830E   3C                   ENDIF
174000020319     C     MM            IFNE      13
174100020319     C                   IF        DESCLV <> 'D'
174200020319     C                   ADD       CONNTW        CNCN(MM)                       NUMERO CLIENTI
174300020318     C                   ELSE
174400020319     C                   ADD       CONNTW        CNCND(MM)                      NUMERO CLIENTI
174500020318     C                   ENDIF
174600020319     C                   ENDIF
174700000000     C                   ADD       WFDNSP        SNC(MM)                        SPEDIZIONI
174800000000     C                   ADD       WFDRRN        RNC(MM)                        RICAVI REALI DELTA
174900000000     C                   ADD       WFDRPN        PNC(MM)                        RICAVI PRESUNTI DELT
175000020318     C                   IF        DESCLV <> 'D'
175100020318     C                   ADD       WFDNSP        SNCN(MM)                       SPEDIZIONI
175200020318     C                   ADD       WFDRRN        RNCN(MM)                       RICAVI REALI DELTA
175300020318     C                   ADD       WFDRPN        PNCN(MM)                       RICAVI PRESUNTI DELT
175400020318     C                   ELSE
175500020318     C                   ADD       WFDNSP        SNCND(MM)                      SPEDIZIONI
175600020318     C                   ADD       WFDRRN        RNCND(MM)                      RICAVI REALI DELTA
175700020318     C                   ADD       WFDRPN        PNCND(MM)                      RICAVI PRESUNTI DELT
175800020318     C                   ENDIF
175900000000     C* ACQUISITO
176000000000X   2C                   ELSE
176100000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
176200000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
176300000000     C                   ADD       1             CAC(MM)                        NUMERO CLIENTI
176400000830E   3C                   ENDIF
176500020319     C     MM            IFNE      13
176600020319     C                   IF        DESCLV <> 'D'
176700020319     C                   ADD       CONNTW        CACN(MM)                       NUMERO CLIENTI
176800020318     C                   ELSE
176900020319     C                   ADD       CONNTW        CACND(MM)                      NUMERO CLIENTI
177000020318     C                   ENDIF
177100020319     C                   ENDIF
177200000000     C                   ADD       WFDNSP        SAC(MM)                        SPEDIZIONI
177300000000     C                   ADD       WFDRRN        RAC(MM)                        RICAVI REALI DELTA
177400000000     C                   ADD       WFDRPN        PAC(MM)                        RICAVI PRESUNTI DELT
177500020318     C                   IF        DESCLV <> 'D'
177600020318     C                   ADD       WFDNSP        SACN(MM)                       SPEDIZIONI
177700020318     C                   ADD       WFDRRN        RACN(MM)                       RICAVI REALI DELTA
177800020318     C                   ADD       WFDRPN        PACN(MM)                       RICAVI PRESUNTI DELT
177900020318     C                   ELSE
178000020318     C                   ADD       WFDNSP        SACND(MM)                      SPEDIZIONI
178100020318     C                   ADD       WFDRRN        RACND(MM)                      RICAVI REALI DELTA
178200020318     C                   ADD       WFDRPN        PACND(MM)                      RICAVI PRESUNTI DELT
178300020318     C                   ENDIF
178400000000E   2C                   ENDIF
178500000000     C*---
178600000000     C* TOT CLIENTI 13^ COLONNA
178700000000     C*---
178800000000     C     Q             OCCUR     ABCC13
178900020318     C     K             OCCUR     ABCC13N
179000020318     C     K             OCCUR     ABCC13ND
179100951227     C* NUOVO
179200951227IF  2C     DESCLN        IFEQ      'Y'
179300951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
179400951227     C                   ADD       CO2KSC        NTC(2)
179500020318     C                   IF        DESCLV <> 'D'
179600020318     C                   ADD       CO2KSC        NTCN(2)
179700020318     C                   ELSE
179800020318     C                   ADD       CO2KSC        NTCND(2)
179900020318     C                   ENDIF
180000951227X   3C                   ELSE                                                   *PERIODO CORRENTE
180100951227     C                   ADD       CONKSC        NTC(1)
180200020318     C                   IF        DESCLV <> 'D'
180300020318     C                   ADD       CONKSC        NTCN(1)
180400020318     C                   ELSE
180500020318     C                   ADD       CONKSC        NTCND(1)
180600020318     C                   ENDIF
180700951227E   3C                   ENDIF
180800951227     C* ACQUISITO
180900951227X   2C                   ELSE
181000951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
181100951227     C                   ADD       CO2KSC        ATC(2)
181200020318     C                   IF        DESCLV <> 'D'
181300020318     C                   ADD       CO2KSC        ATCN(2)
181400020318     C                   ELSE
181500020318     C                   ADD       CO2KSC        ATCND(2)
181600020318     C                   ENDIF
181700951227X   3C                   ELSE                                                   *PERIODO CORRENTE
181800951227     C                   ADD       CONKSC        ATC(1)
181900020318     C                   IF        DESCLV <> 'D'
182000020318     C                   ADD       CONKSC        ATCN(1)
182100020318     C                   ELSE
182200020318     C                   ADD       CONKSC        ATCND(1)
182300020318     C                   ENDIF
182400951227E   3C                   ENDIF
182500951227E   2C                   ENDIF
182600000000     C*--------------------
182700000000     C* F I L I A L E
182800000000     C*--------------------
182900000000     C     Q             OCCUR     ABCFIL
183000020318     C     K             OCCUR     ABCFILN
183100020318     C     K             OCCUR     ABCFILND
183200000000     C* NUOVO
183300000000IF  2C     DESCLN        IFEQ      'Y'
183400000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
183500000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
183600000000     C                   ADD       1             CNF(MM)                        NUMERO CLIENTI
183700000830E   3C                   ENDIF
183800020319     C     MM            IFNE      13
183900020319     C                   IF        DESCLV <> 'D'
184000020319     C                   ADD       CONNTW        CNFN(MM)                       NUMERO CLIENTI
184100020318     C                   ELSE
184200020319     C                   ADD       CONNTW        CNFND(MM)                      NUMERO CLIENTI
184300020318     C                   ENDIF
184400020319     C                   ENDIF
184500000000     C                   ADD       WFDNSP        SNF(MM)                        SPEDIZIONI
184600000000     C                   ADD       WFDRRN        RNF(MM)                        RICAVI REALI DELTA
184700000000     C                   ADD       WFDRPN        PNF(MM)                        RICAVI PRESUNTI DELT
184800020318     C                   IF        DESCLV <> 'D'
184900020318     C                   ADD       WFDNSP        SNFN(MM)                       SPEDIZIONI
185000020318     C                   ADD       WFDRRN        RNFN(MM)                       RICAVI REALI DELTA
185100020318     C                   ADD       WFDRPN        PNFN(MM)                       RICAVI PRESUNTI DELT
185200020318     C                   ELSE
185300020318     C                   ADD       WFDNSP        SNFND(MM)                      SPEDIZIONI
185400020318     C                   ADD       WFDRRN        RNFND(MM)                      RICAVI REALI DELTA
185500020318     C                   ADD       WFDRPN        PNFND(MM)                      RICAVI PRESUNTI DELT
185600020318     C                   ENDIF
185700000000     C* ACQUISITO
185800000000X   2C                   ELSE
185900000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
186000000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
186100000000     C                   ADD       1             CAF(MM)                        NUMERO CLIENTI
186200000830E   3C                   ENDIF
186300020319     C     MM            IFNE      13
186400020319     C                   IF        DESCLV <> 'D'
186500020319     C                   ADD       CONNTW        CAFN(MM)                       NUMERO CLIENTI
186600020318     C                   ELSE
186700020319     C                   ADD       CONNTW        CAFND(MM)                      NUMERO CLIENTI
186800020318     C                   ENDIF
186900020319     C                   ENDIF
187000000000     C                   ADD       WFDNSP        SAF(MM)                        SPEDIZIONI
187100000000     C                   ADD       WFDRRN        RAF(MM)                        RICAVI REALI DELTA
187200000000     C                   ADD       WFDRPN        PAF(MM)                        RICAVI PRESUNTI DELT
187300020318     C                   IF        DESCLV <> 'D'
187400020318     C                   ADD       WFDNSP        SAFN(MM)                       SPEDIZIONI
187500020318     C                   ADD       WFDRRN        RAFN(MM)                       RICAVI REALI DELTA
187600020318     C                   ADD       WFDRPN        PAFN(MM)                       RICAVI PRESUNTI DELT
187700020318     C                   ELSE
187800020318     C                   ADD       WFDNSP        SAFND(MM)                      SPEDIZIONI
187900020318     C                   ADD       WFDRRN        RAFND(MM)                      RICAVI REALI DELTA
188000020318     C                   ADD       WFDRPN        PAFND(MM)                      RICAVI PRESUNTI DELT
188100020318     C                   ENDIF
188200000000E   2C                   ENDIF
188300000000     C*---
188400000000     C* TOT CLIENTI 13^ COLONNA
188500000000     C*---
188600000000     C     Q             OCCUR     ABCF13
188700020318     C     K             OCCUR     ABCF13N
188800020318     C     K             OCCUR     ABCF13ND
188900951227IF  2C     DESCLN        IFEQ      'Y'
189000951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
189100951227     C                   ADD       CO2KSC        NTF(2)
189200020318     C                   IF        DESCLV <> 'D'
189300020319     C                   ADD       CONNTW        NTFN(2)
189400020318     C                   ELSE
189500020319     C                   ADD       CONNTW        NTFND(2)
189600020318     C                   ENDIF
189700951227X   3C                   ELSE                                                   *PERIODO CORRENTE
189800951227     C                   ADD       CONKSC        NTF(1)
189900020318     C                   IF        DESCLV <> 'D'
190000020319     C                   ADD       CONNTW        NTFN(1)
190100020318     C                   ELSE
190200020319     C                   ADD       CONNTW        NTFND(1)
190300020318     C                   ENDIF
190400951227E   3C                   ENDIF
190500030924     C* ACQUISITO
190600951227X   2C                   ELSE
190700951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
190800951227     C                   ADD       CO2KSC        ATF(2)
190900030924     C                   IF        DESCLV <> 'D'
191000030924     C                   ADD       CONNTW        ATFN(2)
191100030924     C                   ELSE
191200020319     C                   ADD       CONNTW        ATFND(2)
191300020318     C                   ENDIF
191400951227X   3C                   ELSE                                                   *PERIODO CORRENTE
191500951227     C                   ADD       CONKSC        ATF(1)
191600020318     C                   IF        DESCLV <> 'D'
191700020319     C                   ADD       CONNTW        ATFN(1)
191800020318     C                   ELSE
191900020319     C                   ADD       CONNTW        ATFND(1)
192000020318     C                   ENDIF
192100951227E   3C                   ENDIF
192200951227E   2C                   ENDIF
192300000000     C*--------------------
192400000000     C* A R E A
192500000000     C*--------------------
192600000000     C     Q             OCCUR     ABCARE
192700020318     C     K             OCCUR     ABCAREN
192800020318     C     K             OCCUR     ABCAREND
192900000000     C* NUOVO
193000000000IF  2C     DESCLN        IFEQ      'Y'
193100000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
193200000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
193300000000     C                   ADD       1             CNA(MM)                        NUMERO CLIENTI
193400000830E   3C                   ENDIF
193500020319     C     MM            IFNE      13
193600020319     C                   IF        DESCLV <> 'D'
193700020319     C                   ADD       CONNTW        CNAN(MM)                       NUMERO CLIENTI
193800020318     C                   ELSE
193900020319     C                   ADD       CONNTW        CNAND(MM)                      NUMERO CLIENTI
194000020318     C                   ENDIF
194100020319     C                   ENDIF
194200000000     C                   ADD       WFDNSP        SNA(MM)                        SPEDIZIONI
194300000000     C                   ADD       WFDRRN        RNA(MM)                        RICAVI REALI DELTA
194400000000     C                   ADD       WFDRPN        PNA(MM)                        RICAVI PRESUNTI DELT
194500020318     C                   IF        DESCLV <> 'D'
194600020318     C                   ADD       WFDNSP        SNAN(MM)                       SPEDIZIONI
194700020318     C                   ADD       WFDRRN        RNAN(MM)                       RICAVI REALI DELTA
194800020318     C                   ADD       WFDRPN        PNAN(MM)                       RICAVI PRESUNTI DELT
194900020318     C                   ELSE
195000020318     C                   ADD       WFDNSP        SNAND(MM)                      SPEDIZIONI
195100020318     C                   ADD       WFDRRN        RNAND(MM)                      RICAVI REALI DELTA
195200020318     C                   ADD       WFDRPN        PNAND(MM)                      RICAVI PRESUNTI DELT
195300020318     C                   ENDIF
195400000000     C* ACQUISITO
195500000000X   2C                   ELSE
195600000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
195700000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
195800000000     C                   ADD       1             CAA(MM)                        NUMERO CLIENTI
195900000830E   3C                   ENDIF
196000020319     C     MM            IFNE      13
196100020319     C                   IF        DESCLV <> 'D'
196200020319     C                   ADD       CONNTW        CAAN(MM)                       NUMERO CLIENTI
196300020318     C                   ELSE
196400020319     C                   ADD       CONNTW        CAAND(MM)                      NUMERO CLIENTI
196500020318     C                   ENDIF
196600020319     C                   ENDIF
196700000000     C                   ADD       WFDNSP        SAA(MM)                        SPEDIZIONI
196800000000     C                   ADD       WFDRRN        RAA(MM)                        RICAVI REALI DELTA
196900000000     C                   ADD       WFDRPN        PAA(MM)                        RICAVI PRESUNTI DELT
197000020318     C                   IF        DESCLV <> 'D'
197100020318     C                   ADD       WFDNSP        SAAN(MM)                       SPEDIZIONI
197200020318     C                   ADD       WFDRRN        RAAN(MM)                       RICAVI REALI DELTA
197300020318     C                   ADD       WFDRPN        PAAN(MM)                       RICAVI PRESUNTI DELT
197400020318     C                   ELSE
197500020318     C                   ADD       WFDNSP        SAAND(MM)                      SPEDIZIONI
197600020318     C                   ADD       WFDRRN        RAAND(MM)                      RICAVI REALI DELTA
197700020318     C                   ADD       WFDRPN        PAAND(MM)                      RICAVI PRESUNTI DELT
197800020318     C                   ENDIF
197900000000E   2C                   ENDIF
198000000000     C*---
198100000000     C* TOT CLIENTI 13^ COLONNA
198200000000     C*---
198300000000     C     Q             OCCUR     ABCA13
198400020318     C     K             OCCUR     ABCA13N
198500020318     C     K             OCCUR     ABCA13ND
198600951227IF  2C     DESCLN        IFEQ      'Y'
198700951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
198800951227     C                   ADD       CO2KSC        NTA(2)
198900020318     C                   IF        DESCLV <> 'D'
199000020319     C                   ADD       CONNTW        NTAN(2)
199100020318     C                   ELSE
199200020319     C                   ADD       CONNTW        NTAND(2)
199300020318     C                   ENDIF
199400951227X   3C                   ELSE                                                   *PERIODO CORRENTE
199500951227     C                   ADD       CONKSC        NTA(1)
199600020318     C                   IF        DESCLV <> 'D'
199700020319     C                   ADD       CONNTW        NTAN(1)
199800020318     C                   ELSE
199900020319     C                   ADD       CONNTW        NTAND(1)
200000020318     C                   ENDIF
200100951227E   3C                   ENDIF
200200951227     C* ACQUISITO
200300951227X   2C                   ELSE
200400951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
200500951227     C                   ADD       CO2KSC        ATA(2)
200600020318     C                   IF        DESCLV <> 'D'
200700020319     C                   ADD       CONNTW        ATAN(2)
200800020318     C                   ELSE
200900020319     C                   ADD       CONNTW        ATAND(2)
201000020318     C                   ENDIF
201100951227X   3C                   ELSE                                                   *PERIODO CORRENTE
201200951227     C                   ADD       CONKSC        ATA(1)
201300020318     C                   IF        DESCLV <> 'D'
201400020319     C                   ADD       CONNTW        ATAN(1)
201500020318     C                   ELSE
201600020319     C                   ADD       CONNTW        ATAND(1)
201700020318     C                   ENDIF
201800951227E   3C                   ENDIF
201900951227E   2C                   ENDIF
202000000000     C*--------------------
202100000000     C* D I V I S I O N E
202200000000     C*--------------------
202300000000     C     Q             OCCUR     ABCDIV
202400020318     C     K             OCCUR     ABCDIVN
202500020318     C     K             OCCUR     ABCDIVND
202600000000     C* NUOVO
202700000000IF  2C     DESCLN        IFEQ      'Y'
202800000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
202900000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
203000000000     C                   ADD       1             CND(MM)                        NUMERO CLIENTI
203100000830E   3C                   ENDIF
203200020319     C     MM            IFNE      13
203300020319     C                   IF        DESCLV <> 'D'
203400020319     C                   ADD       CONNTW        CNDN(MM)                       NUMERO CLIENTI
203500020318     C                   ELSE
203600020319     C                   ADD       CONNTW        CNDND(MM)                      NUMERO CLIENTI
203700020318     C                   ENDIF
203800020319     C                   ENDIF
203900000000     C                   ADD       WFDNSP        SND(MM)                        SPEDIZIONI
204000000000     C                   ADD       WFDRRN        RND(MM)                        RICAVI REALI DELTA
204100000000     C                   ADD       WFDRPN        PND(MM)                        RICAVI PRESUNTI DELT
204200020318     C                   IF        DESCLV <> 'D'
204300020318     C                   ADD       WFDNSP        SNDN(MM)                       SPEDIZIONI
204400020318     C                   ADD       WFDRRN        RNDN(MM)                       RICAVI REALI DELTA
204500020318     C                   ADD       WFDRPN        PNDN(MM)                       RICAVI PRESUNTI DELT
204600020318     C                   ELSE
204700020318     C                   ADD       WFDNSP        SNDND(MM)                      SPEDIZIONI
204800020318     C                   ADD       WFDRRN        RNDND(MM)                      RICAVI REALI DELTA
204900020318     C                   ADD       WFDRPN        PNDND(MM)                      RICAVI PRESUNTI DELT
205000020318     C                   ENDIF
205100000000     C* ACQUISITO
205200000000X   2C                   ELSE
205300000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
205400000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
205500000000     C                   ADD       1             CAD(MM)                        NUMERO CLIENTI
205600000830E   3C                   ENDIF
205700020319     C     MM            IFNE      13
205800020319     C                   IF        DESCLV <> 'D'
205900020319     C                   ADD       CONNTW        CADN(MM)                       NUMERO CLIENTI
206000020318     C                   ELSE
206100020319     C                   ADD       CONNTW        CADND(MM)                      NUMERO CLIENTI
206200020318     C                   ENDIF
206300020319     C                   ENDIF
206400000000     C                   ADD       WFDNSP        SAD(MM)                        SPEDIZIONI
206500000000     C                   ADD       WFDRRN        RAD(MM)                        RICAVI REALI DELTA
206600000000     C                   ADD       WFDRPN        PAD(MM)                        RICAVI PRESUNTI DELT
206700020318     C                   IF        DESCLV <> 'D'
206800020318     C                   ADD       WFDNSP        SADN(MM)                       SPEDIZIONI
206900020318     C                   ADD       WFDRRN        RADN(MM)                       RICAVI REALI DELTA
207000020318     C                   ADD       WFDRPN        PADN(MM)                       RICAVI PRESUNTI DELT
207100020318     C                   ELSE
207200020318     C                   ADD       WFDNSP        SADND(MM)                      SPEDIZIONI
207300020318     C                   ADD       WFDRRN        RADND(MM)                      RICAVI REALI DELTA
207400020318     C                   ADD       WFDRPN        PADND(MM)                      RICAVI PRESUNTI DELT
207500020318     C                   ENDIF
207600000000E   2C                   ENDIF
207700000000     C*---
207800000000     C* TOT CLIENTI 13^ COLONNA
207900000000     C*---
208000000000     C     Q             OCCUR     ABCD13
208100020318     C     K             OCCUR     ABCD13N
208200020318     C     K             OCCUR     ABCD13ND
208300951227     C* NUOVO
208400951227IF  2C     DESCLN        IFEQ      'Y'
208500951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
208600951227     C                   ADD       CO2KSC        NTD(2)
208700020318     C                   IF        DESCLV <> 'D'
208800020319     C                   ADD       CONNTW        NTDN(2)
208900020318     C                   ELSE
209000020319     C                   ADD       CONNTW        NTDND(2)
209100020318     C                   ENDIF
209200030924X   3C                   ELSE                                                   *PERIODO CORRENTE
209300030924     C                   ADD       CONKSC        NTD(1)
209400030924     C                   IF        DESCLV <> 'D'
209500030924     C                   ADD       CONNTW        NTDN(1)
209600030924     C                   ELSE
209700030924     C                   ADD       CONNTW        NTDND(1)
209800020318     C                   ENDIF
209900951227E   3C                   ENDIF
210000951227     C* ACQUISITO
210100951227X   2C                   ELSE
210200951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
210300951227     C                   ADD       CO2KSC        ATD(2)
210400020318     C                   IF        DESCLV <> 'D'
210500020319     C                   ADD       CONNTW        ATDN(2)
210600020318     C                   ELSE
210700020319     C                   ADD       CONNTW        ATDND(2)
210800020318     C                   ENDIF
210900951227X   3C                   ELSE                                                   *PERIODO CORRENTE
211000951227     C                   ADD       CONKSC        ATD(1)
211100020318     C                   IF        DESCLV <> 'D'
211200020319     C                   ADD       CONNTW        ATDN(1)
211300020318     C                   ELSE
211400020319     C                   ADD       CONNTW        ATDND(1)
211500020318     C                   ENDIF
211600951227E   3C                   ENDIF
211700951227E   2C                   ENDIF
211800000000     C*--------------------
211900000000     C* G E N E R A L E
212000000000     C*--------------------
212100000000     C     Q             OCCUR     ABCGEN
212200020318     C     K             OCCUR     ABCGENN
212300020318     C     K             OCCUR     ABCGENND
212400000000     C* NUOVO
212500000000IF  2C     DESCLN        IFEQ      'Y'
212600000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
212700000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
212800000830     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
212900000000     C                   ADD       1             CNG(MM)                        NUMERO CLIENTI
213000000830E   3C                   ENDIF
213100020319     C     MM            IFNE      13
213200020319     C                   IF        DESCLV <> 'D'
213300020319     C                   ADD       CONNTW        CNGN(MM)                       NUMERO CLIENTI
213400020318     C                   ELSE
213500020319     C                   ADD       CONNTW        CNGND(MM)                      NUMERO CLIENTI
213600020318     C                   ENDIF
213700020319     C                   ENDIF
213800000000     C                   ADD       WFDNSP        SNG(MM)                        SPEDIZIONI
213900000000     C                   ADD       WFDRRN        RNG(MM)                        RICAVI REALI DELTA
214000000000     C                   ADD       WFDRPN        PNG(MM)                        RICAVI PRESUNTI DELT
214100020318     C                   IF        DESCLV <> 'D'
214200020318     C                   ADD       WFDNSP        SNGN(MM)                       SPEDIZIONI
214300020318     C                   ADD       WFDRRN        RNGN(MM)                       RICAVI REALI DELTA
214400020318     C                   ADD       WFDRPN        PNGN(MM)                       RICAVI PRESUNTI DELT
214500020318     C                   ELSE
214600020318     C                   ADD       WFDNSP        SNGND(MM)                      SPEDIZIONI
214700020318     C                   ADD       WFDRRN        RNGND(MM)                      RICAVI REALI DELTA
214800020318     C                   ADD       WFDRPN        PNGND(MM)                      RICAVI PRESUNTI DELT
214900020318     C                   ENDIF
215000000000     C* ACQUISITO
215100000000X   2C                   ELSE
215200000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
215300000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
215400000830     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
215500000000     C                   ADD       1             CAG(MM)                        NUMERO CLIENTI
215600000830E   3C                   ENDIF
215700020319     C     MM            IFNE      13
215800020319     C                   IF        DESCLV <> 'D'
215900020319     C                   ADD       CONNTW        CAGN(MM)                       NUMERO CLIENTI
216000020318     C                   ELSE
216100020319     C                   ADD       CONNTW        CAGND(MM)                      NUMERO CLIENTI
216200020318     C                   ENDIF
216300020319     C                   ENDIF
216400000000     C                   ADD       WFDNSP        SAG(MM)                        SPEDIZIONI
216500000000     C                   ADD       WFDRRN        RAG(MM)                        RICAVI REALI DELTA
216600000000     C                   ADD       WFDRPN        PAG(MM)                        RICAVI PRESUNTI DELT
216700020318     C                   IF        DESCLV <> 'D'
216800020318     C                   ADD       WFDNSP        SAGN(MM)                       SPEDIZIONI
216900020318     C                   ADD       WFDRRN        RAGN(MM)                       RICAVI REALI DELTA
217000020318     C                   ADD       WFDRPN        PAGN(MM)                       RICAVI PRESUNTI DELT
217100020318     C                   ELSE
217200020318     C                   ADD       WFDNSP        SAGND(MM)                      SPEDIZIONI
217300020318     C                   ADD       WFDRRN        RAGND(MM)                      RICAVI REALI DELTA
217400020318     C                   ADD       WFDRPN        PAGND(MM)                      RICAVI PRESUNTI DELT
217500020318     C                   ENDIF
217600000000E   2C                   ENDIF
217700000000     C*---
217800000000     C* TOT CLIENTI 13^ COLONNA
217900000000     C*---
218000000000     C     Q             OCCUR     ABCG13
218100020318     C     K             OCCUR     ABCG13N
218200020318     C     K             OCCUR     ABCG13ND
218300000000     C* NUOVO
218400000000IF  2C     DESCLN        IFEQ      'Y'
218500951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
218600951227     C                   ADD       CO2KSC        NTG(2)
218700020318     C                   IF        DESCLV <> 'D'
218800020319     C                   ADD       CONNTW        NTGN(2)
218900020318     C                   ELSE
219000020319     C                   ADD       CONNTW        NTGND(2)
219100020318     C                   ENDIF
219200951227X   3C                   ELSE                                                   *PERIODO CORRENTE
219300951227     C                   ADD       CONKSC        NTG(1)
219400020318     C                   IF        DESCLV <> 'D'
219500020319     C                   ADD       CONNTW        NTGN(1)
219600020318     C                   ELSE
219700020319     C                   ADD       CONNTW        NTGND(1)
219800020318     C                   ENDIF
219900951227E   3C                   ENDIF
220000000000     C* ACQUISITO
220100000000X   2C                   ELSE
220200951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
220300951227     C                   ADD       CO2KSC        ATG(2)
220400020318     C                   IF        DESCLV <> 'D'
220500020319     C                   ADD       CONNTW        ATGN(2)
220600020318     C                   ELSE
220700020319     C                   ADD       CONNTW        ATGND(2)
220800020318     C                   ENDIF
220900951227X   3C                   ELSE                                                   *PERIODO CORRENTE
221000951227     C                   ADD       CONKSC        ATG(1)
221100020318     C                   IF        DESCLV <> 'D'
221200020319     C                   ADD       CONNTW        ATGN(1)
221300020318     C                   ELSE
221400020319     C                   ADD       CONNTW        ATGND(1)
221500020318     C                   ENDIF
221600951227E   3C                   ENDIF
221700000000E   2C                   ENDIF
221800000000     C*
221900020318E   1C                   ENDIF
222000000000     C*
222100000000     C                   ENDSR
222200000000     C*------------------------------------------------------------------------*
222300100204     /*?  TOTCLI_NTW_TS  somma sulle Ds multiple x NTW e Tipo servizio        ?
222400000000     C*------------------------------------------------------------------------*
222500100201     C     TOTCLI_NTW_TS BEGSR
222600100201      **
222700100201     c                   clear                   rec_Ntw_TSv_OK    1
222800100201      **
222900100201      **  Controlla se il record è da prendere in considerazione
223000100201IF  1C                   IF        WFDNTW = *blank
223100100202     c                   eval             rec_Ntw_TSv_OK = 'S'
223200100201     c                   movel(p)  'ITA'         cod_network       3
223300100201IF  1C                   ELSE
223400100202     c                   eval             rec_Ntw_TSv_OK = 'S'
223500100201     c                   movel(p)  WFDNTW        cod_network
223600100201     c                   END
223700100201     c**
223800100201     c                   eval      numElem = %Elem(W1ntwTS)
223900100201     c**
224000100201      **  Quindi se da totalizzare cerca su quale occurence deve metterlo.
224100160509     c                   do        numElem       kk                3 0
224200160509     C     Kk            OCCUR     W1ntwTS
224300100202      *
224400100202     ** inserisce se non era stato ancora inserito
224500100202     c                   if         c1ntwTS = *BLANK and c1tpsTS = *BLANK
224600100202     c                   eval       c1ntwTS = cod_network
224700100202     c                   eval       c1tpsTS = wfdTSP
224800100202     c                   end
224900100202      *
225000100202     c** Solo se corrisponde la chiave NTW + TServ.
225100100202     c                   IF         c1ntwTS = cod_network and c1tpsTS = wfdTSP
225200100201     c**
225300100201IF  1C     MM            ifLE      12
225400100202     c**
225500100202     ** aggiunge x l'anno in corso
225600100201     C                   ADD       WFDNSP        S1ntwTS(MM)                        SPEDIZIONI
225700100201     C                   eval      Kg1ntwTS(MM) = wfdPKG + Kg1ntwTS(MM)             PESI in KG
225800100201     C                   ADD       WFDRRN        R1ntwTS(MM)                        RICAVI REALI DELTA
225900100201     C                   ADD       WFDRPN        P1ntwTS(MM)                        RICAVI PRESUNTI DELT
226000100201     C                   z-add(h)  WFDRRN        R1totale
226100100201     C                   z-add(h)  WFDRPN        P1totale
226200100201     C                   ADD       R1totale      TR1ntwTS
226300100201     C                   ADD       P1totale      TP1ntwTS
226400100201     c                   leave
226500100201      **
226600100201     c                   else
226700100201      **
226800100201IF  1C     WFDmes        ifLE      12
226900100202     ** aggiunge x l'anno precedente
227000100201     C                   eval      ap_S1ntwTS(WFDmes) = WFDNSP +
227100100201     C                                ap_S1ntwTS(WFDmes)                              SPEDIZIONI
227200100201     C                   eval      ap_Kg1ntwTS(WFDmes)= wfdPKG +
227300100201     C                                ap_Kg1ntwTS(WFDmes)                             PESI in KG
227400100201     C                   eval      ap_R1ntwTS(WFDmes) = WFDRRN +
227500100201     C                                ap_R1ntwTS(WFDmes)                           RICAVI REALI DELT
227600100201     C                   eval      ap_P1ntwTS(WFDmes) = WFDRPN +
227700100218     C                                ap_P1ntwTS(WFDmes)                           RICAVI PRESUNTI D
227800100201     C                   z-add(h)  WFDRRN        R1totale
227900100201     C                   z-add(h)  WFDRPN        P1totale
228000100201     C                   ADD       R1totale      ap_TR1ntwTS
228100100201     C                   ADD       P1totale      ap_TP1ntwTS
228200100201     c                   leave
228300100201     c                   end
228400100201      **
228500100201     c                   end
228600100202     c                   end
228700100202      **
228800100202     C                   ENDdo
228900100202     c**
229000100201     C                   ENDSR
229100100201     C*------------------------------------------------------------------------*
229200100201     C*?STAKSC - IMPOSTA STAMPA VALORI CLIENTE
229300100201     C*------------------------------------------------------------------------*
229400100201     C     STAKSC        BEGSR
229500100201     C*
229600020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
229700020222     C                   EVAL      DEPSTPD = '0'
229800000000     C*
229900000000     C* STAMPA I VALORI DEL CLIENTE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
230000000000     C* LA STATISTICA DELTA CON DETTAGLIO CLIENTE
230100000000IF  1C     PASKSC        IFEQ      '1'
230200000000     C     PARSTA        ANDEQ     'K'
230300000000     C                   MOVEL     '1'           ORAKSC                         *ORA STAMPA CLIENTE
230400000000     C***
230500000000     C* DESCRIZIONI INTESTAZIONE
230600000000     C***
230700020116     C                   CLEAR                   SE94IN
230800000000     C                   Z-ADD     DEPAGE        INCAGE                         *AGENTE
230900000000     C                   MOVEL     DESAGE        INDAGE                         *DESCRIZIONE AGENTE
231000000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
231100000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
231200000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
231300000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
231400000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
231500000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
231600950301     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
231700000000     C                   MOVEL     DRI(20)       PIDDES                         *RETTANGOLINO
231800020221     C                   MOVEL     *BLANKS       P1DFLD                         *RETTANGOLINO
231900030311     C*
232000030311     C* PREPARA BUFFER FILE PC (WFDELPCF)
232100030311     C                   EXSR      WRIFPC
232200000000     C*
232300000000     C* STAMPA I VALORI DEL CLIENTE
232400020221     C                   EXSR      STMKSC
232500000000     C*
232600000000E   1C                   ENDIF
232700000000     C*
232800000000     C                   ENDSR
232900030311     C*------------------------------------------------------------------------*
233000030311     C*?WRIFPC - IMPOSTA BUFFER FILE PC (WFDELPCF)
233100030311     C*------------------------------------------------------------------------*
233200030311     C     WRIFPC        BEGSR
233300030311     C*
233400100511     C**------------>    CLEAR                   WFDELPC0
233500030311     C*
233600100406     C*********>>>       Z-ADD     INCAGE        WFPAGE                         *AGENTE
233700100406     C*********>>>       MOVEL     INDAGE        WFPAGD                         *DESCRIZIONE AGENTE
233800100406     C*********>>>       Z-ADD     INCFIL        WFPFIL                         *FILIALE
233900100406     C*********>>>       MOVEL     INDFIL        WFPFID                         *DESCRIZIONE FILIALE
234000100406     C*********>>>       Z-ADD     INCARE        WFPARE                         *AREA
234100100406     C*********>>>       MOVEL     INDARE        WFPARD                         *DESCRIZIONE AREA
234200100406     C*********>>>       MOVEL     INCDIV        WFPDIV                         *DISTRETTO
234300100406     C*********>>>       MOVEL     INDDIV        WFPDID                         *DESCRIZIONE DISTRET
234400100406     C*********>>>       Z-ADD     PARAAI        WFPAAD                         *ANNO DA
234500100406     C*********>>>       Z-ADD     PARMMI        WFPMMD                         *MESE DA
234600100406     C*********>>>       Z-ADD     PARAAF        WFPAAA                         *ANNO A
234700100406     C*********>>>       Z-ADD     PARMMF        WFPMMA                         *MESE A
234800100406     C*********>>>       Z-ADD     DEPCLI        WFPCLI                         *CODICE CLIENTE
234900100406     C*********>>>       MOVEL     DESKSC        WFPCLD                         *DESCRIZIONE CLIENTE
235000100406     C*********>>>       MOVEL     DESCLV        WFPCLV                         *CODICE IMPORTANZA C
235100100406     C*********>>>       MOVEL     DESITC        WFPITC                         *CATEGORIA CLIENTE
235200100406     C                   Z-ADD     INCAGE        aaaAGE            7 0          *AGENTE
235300100406     C                   MOVEL(p)  INDAGE        aaaAGD           25            *DESCRIZIONE AGENTE
235400100406     C                   Z-ADD     INCFIL        aaaFIL            3 0          *FILIALE
235500100406     C                   MOVEL(p)  INDFIL        aaaFID           25            *DESCRIZIONE FILIALE
235600100406     C                   Z-ADD     INCARE        aaaARE            3 0          *AREA
235700100406     C                   MOVEL(p)  INDARE        aaaARD           25            *DESCRIZIONE AREA
235800100406     C                   MOVEL(p)  INCDIV        aaaDIV            1            *DISTRETTO
235900100406     C                   MOVEL(p)  INDDIV        aaaDID           25            *DESCRIZIONE DISTRET
236000050223     C*******            MOVEL     INDIOE        WFPIOE                         *DELTA ITALIA/ESTERO
236100100406     C*********>>>       MOVEL     parIOE        WFPIOE                         *DELTA ITALIA/ESTERO
236200100406     C                   MOVEL(p)  parIOE        aaaIOE            1            *DELTA ITALIA/ESTERO
236300100406     C                   Z-ADD     PARAAI        aaaAAD            4 0          *ANNO DA
236400100406     C                   Z-ADD     PARMMI        aaaMMD            2 0          *MESE DA
236500100406     C                   Z-ADD     PARAAF        aaaAAA            4 0          *ANNO A
236600100406     C                   Z-ADD     PARMMF        aaaMMA            2 0          *MESE A
236700100406     C                   Z-ADD     DEPCLI        aaaCLI            7 0          *CODICE CLIENTE
236800100406     C                   MOVEL(p)  DESKSC        aaaCLD           25            *DESCRIZIONE CLIENTE
236900100406     C                   MOVEL(p)  DESCLV        aaaCLV            1            *CODICE IMPORTANZA C
237000100406     C                   MOVEL(p)  DESITC        aaaITC           25            *CATEGORIA CLIENTE
237100070924     C*****
237200070924     C*****              MOVEL     DES09         WFPCLN                         *CLIENTE NUOVO/ACQUI
237300100406     C*********>>>       MOVEL     'A'           WFPCLN
237400100406     C                   MOVEL(p)  'A'           aaaCLN            1
237500070924IF  1C     DESCLN        IFne      *blank                                       *CL.NUOVO SE "Y" o "
237600100406     C*********>>>       MOVEL     'N'           WFPCLN
237700100406     C                   MOVEL     'N'           aaaCLN
237800070924E   1C                   ENDIF
237900030311     C*
238000030311     C                   ENDSR
238100000000     C*------------------------------------------------------------------------*
238200000000     C*?STMKSC - STAMPA I VALORI DEL CLIENTE
238300000000     C*------------------------------------------------------------------------*
238400000000     C     STMKSC        BEGSR
238500061220     C*
238600061220     C* Per controllare se il cliente da stampare è anagraficamente Bloccato
238700061220     C*   nella totalità ossia se è uniuficante di se stesso oppure se ha
238800061220     C*   tutti i codici figli oltre se stesso Bloccati.
238900061220     C                   CLEAR                   Total_Bloccato    1
239000061220     C*
239100000000     C*--------------------
239200000000     C* I N T E S T A Z I O N E
239300000000     C*--------------------
239400000000IF  1C     *IN01         IFEQ      *ON
239500000000IF  2C     PRIGIR        IFEQ      '0'
239600000000     C                   MOVEL     '1'           PRIGIR
239700000000X   2C                   ELSE
239800100129     c                   if        parSTAMPA <> 'N'
239900020116     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
240000100129     c                   end
240100000000E   2C                   ENDIF
240200020222     C*
240300020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
240400020222     C                   IF        FLGSTPLE = '1'
240500100129     c                   if        parSTAMPA <> 'N'
240600020222     C                   WRITE     SE94LE                                       *LEGENDA
240700100129     c                   end
240800020222     C                   ELSE
240900020222     C                   EVAL      FLGSTPLE = '1'
241000020222     C                   ENDIF
241100020222     C*
241200100129     c                   if        parSTAMPA <> 'N'
241300020222     C                   WRITE     SE94PT                                       *TESTATA
241400020116     C                   WRITE     SE94IN                                       *INTESTAZIONE
241500020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
241600020116     C                   WRITE     SE94PI                                       *RIGA MESI
241700020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
241800100129     c                   end
241900000000     C                   SETOFF                                       01
242000000000E   1C                   ENDIF
242100020222     C*--------------------
242200000000     C* S P E D I Z I O N I
242300000000     C*--------------------
242400000000     C                   Z-ADD     S1K           $VA
242500020116     C                   Z-ADD     *ZEROS        $VA13C           19 5          *PERIODO CORRENTE
242600000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
242700000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
242800000000E   1C                   ENDDO
242900020116     C                   Z-ADD     *ZEROS        $V213C           19 5          *PERIODO PRECEDENTE
243000951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
243100000000     C***
243200000000     C* CODICE + RAGIONE SOCIALE
243300000000     C***
243400020221     C*
243500000825     C                   MOVEL     DEPCLI        DES07             7            *CODICE CLIENTE
243600000000     C                   MOVEL     DESKSC        DES12            12            *RAGIONE SOCIALE
243700000000     C*
243800000000     C                   MOVEL     *BLANKS       DES20
243900000000     C                   MOVEL     DES07         DES20            20
244000000000     C                   MOVE      DES12         DES20
244100000000     C*
244200000000     C                   MOVEL     *BLANKS       P1DDES
244300000000     C                   MOVEL     DES20         P1DDES
244400000000     C                   MOVE      'SPE'         P1DDES
244500020222     C                   MOVEL     *BLANKS       P1DFLD
244600000000     C*
244700000000     C* SCRIVE I VALORI IN STAMPA
244800000000     C                   EXSR      SCRVAL
244900030311     C*
245000030311     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
245100100406     C*********>>>       EVAL      WFPSAP = P1CVAP
245200100406     C*********>>>       EVAL      WFPSAC = P1CVAT
245300100406     c                   z-add     P1CVAP        aaaSAP           11 0
245400100406     c                   z-add     P1CVAT        aaaSAC           11 0
245500000000     C*--------------------
245600000000     C* R I C A V I
245700000000     C*--------------------
245800020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
245900020116     C*    R1K           DIV       1000          $VA
246000020116     C                   Z-ADD     R1K           $VA
246100020116     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
246200000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
246300020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
246400000000E   1C                   ENDDO
246500020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
246600951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
246700000000     C***
246800000000     C* CLIENTE NUOVO/ACQUISITO + DI TIPO A/B/C
246900000000     C***
247000070822      *?Se il cliente ha spedizioni sia com Italia che come estero ed è NUOVO,
247100070822      *?potrebbe accadere che il mese con la "Y" abbia solo un tipo di spedizioni,
247200070822      *?quindi, se facessimo il test sulla "Y" in una stampa risulterebbe Nuovo e
247300070822      *?nell'altra invece com Acquisito. OCCORRE testare semplicemente se <> da Blk.
247400070822IF  1C**** DESCLN        IFEQ      'Y'                                          *CL.NUOVO SOLO SE "Y
247500070822IF  1C     DESCLN        IFne      *blank                                       *CL.NUOVO SE "Y" o "
247600000000     C                   MOVEL     DRI(17)       DES09             9
247700000000X   1C                   ELSE
247800000000     C                   MOVEL     DRI(18)       DES09             9
247900000000E   1C                   ENDIF
248000000829     C                   EVAL      DES20 =  DES09 +
248100000829     C                                      ' / ' +
248200000829     C                                      DESCLV
248300000000     C                   MOVEL     *BLANKS       P1DDES
248400000000     C                   MOVEL     DES20         P1DDES
248500000000     C                   MOVE      'RIC'         P1DDES
248600020222     C                   MOVEL     *BLANKS       P1DFLD
248700000000     C*
248800000000     C* SCRIVE I VALORI IN STAMPA
248900000000     C                   EXSR      SCRVAL
249000030311     C*
249100030311     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
249200100406     C*********>>>       EVAL      WFPFAP = P1CVAP
249300100406     C*********>>>       EVAL      WFPFAC = P1CVAT
249400100406     c                   z-add     P1CVAP        aaaFAP           15 3
249500100406     c                   z-add     P1CVAT        aaaFAC           15 3
249600000000     C*--------------------
249700000000     C* D E L T A
249800000000     C*--------------------
249900000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
250000000000     C                   Z-ADD     R1K           $RR                            *RICAVI REALI
250100000000     C                   Z-ADD     P1K           $RP                            *RICAVI PRESUNTI
250200000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
250300000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
250400000000     C                   EXSR      IMPCAL
250500970822     C***
250600970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
250700970822     C***
250800970822     C                   MOVEL     D1K           $SD                            *FALG SEGNALAZIONE
250900000000     C***
251000951006     C* CATEGORIA SEAT
251100000000     C***
251200000000     C                   MOVEL     *BLANKS       DES20
251300951006     C                   MOVEL     DESITC        DES20            20
251400000000     C*
251500000000     C                   MOVEL     *BLANKS       P2DDES
251600020221     C                   MOVEL     *BLANKS       P2DFLD
251700000000     C                   MOVEL     DES20         P2DDES
251800000000     C                   MOVE      'DEL'         P2DDES
251900020222     C                   MOVEL     *BLANKS       P2DFLD
252000020701     C*
252100020701     C* VERIFICO LO SCASTAMENTO % RISPETTO ALLO STESSO MESE/PERIODO DELL?ANNO PRECEDENTE
252200020701     C                   EXSR      REPDELFAT
252300000000     C*
252400000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
252500000000     C                   EXSR      SCRDEL
252600030924     C*
252700030924     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
252800100406     C*********>>>       EVAL      WFPDAP = P2CDEP
252900100406     C*********>>>       EVAL      WFPDAC = P2CDET
253000100406     c                   z-add     P2CDEP        aaaDAP            4 1
253100100406     c                   z-add     P2CDET        aaaDAC            4 1
253200030924     C*
253300030924     C* INIZIALIZZO LA SCHIERA DI WRK X SCOSTAMENTO FATTURATO ED I RELATIVI CAMPI DI STAMPA
253400030924     C                   EXSR      INZPDF
253500980406     C*--------------------
253600000828     C* DESCRIZIONE BLOCCO + CLIENTI FIGLI
253700980406     C*--------------------
253800000828     C                   MOVEL     'N'           $STRIG
253900020116     C                   CLEAR                   SE94P3
254000130318IF  1C     DESABL        IFne      *blank
254100130318IF  1C     DESABL        andne     '*'
254200980406     C     DESCBL        ORNE      *BLANKS
254300980406     C                   MOVEL     DESCBL        P3DDES
254400000828     C                   MOVEL     'S'           $STRIG                         *RIGA DA STAMPARE
254500061220     C                   movel     'S'           Total_Bloccato
254600980406E   1C                   ENDIF
254700000828     C*
254800000829     C                   Z-ADD     *ZEROS        BLOCCO            2 0          *BLOCCO CL STAMPATI
254900000828IF  1C     PARSCE        IFEQ      'U'                                          *STAT. UNIFICANTE
255000000828IF  2C     SKSC11(2)     IFGT      *ZEROS                                       *ESISTONO FIGLI
255100000828     C                   Z-ADD     2             I
255200000828DO  3C     I             DOWLE     500
255300000828     C     SKSC11(I)     ANDGT     *ZEROS
255400000828     C                   Z-ADD     *ZEROS        IS                             *N.CLIENTI X RIGA
255500000828     C                   CLEAR                   SKSC
255600051031     C                   clear                   skbl
255700051031      *
255800000828DO  4C     I             DOWLE     500
255900000828     C     SKSC11(I)     ANDGT     *ZEROS
256000051031     C**** ridotta da 19 a 17 per far spazio ai flags x individuare i codici bloccati
256100051031     C**** IS            ANDLT     19
256200051031     C     IS            ANDLT     17
256300000828     C                   ADD       1             IS                             *CLIENTI NELLA RIGA
256400000828     C                   Z-ADD     SKSC11(I)     SKSC(IS)
256500051031     C*--------------------
256600051031     C* controllo codice cliente bloccato
256700051031     C*--------------------
256800051031     C                   CLEAR                   BS69DS
256900051031     C                   CLEAR                   ACODS
257000051031     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
257100051031     C                   MOVEL     SKSC(IS)      I69KAC                         *CLIENTE X CNACO
257200120125     C                   MOVEL     SKSC(IS)      I69KCP                         *CLIENTE X CNCLP
257300051031     C                   CALL      'TIBS69R'
257400051031     C                   PARM                    BS69DS
257500051031     C                   PARM                    ACODS
257600051031     C                   PARM                    INDDS
257700051031     C                   PARM                    CLPDS
257800051031     C                   PARM                    CLSDS
257900051031IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
258000130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
258100130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
258200051031     C                   move      '+'           skbl(is)
258300120125      **
258400120125      **  Reperisce la Data di quando è stato fatto il Blocco Contabile sul cliente
258500120125     C                   clear                   TrulBLCds
258600120125     c                   eval      IblcTLA = 'L'
258700120125     c                   z-add     acoKcC        IblcKCC
258800120125     c                   z-add     acoKsC        IblcKSC
258900120125     c                   call      'TRULBLCR'
259000120125     C                   PARM                    KPJBA
259100120125     C                   PARM                    TrulBLCds
259200120125      **
259300120125      **  deve memorizzare il più recente blocco effettuato
259400120125      **   codice e data blocco dell'ultimo cliente bloccato fra quelli unificati.
259500120125     c                   if         Data_Blocco_Ultimo < OblcDAV
259600120125     c                   eval           Data_Blocco_Ultimo  = OblcDAV
259700120125     c                   eval           Cod_Blocco_Ultimo   = clpNAR
259800120125     c                   end
259900120125      **
260000051031E   1C                   ENDIF
260100120125      ***
260200120131      ***  riporta le date da 6 a 8 per effettuare il confronto
260300120131     c                   eval      Data_Prima_Fat_8 = 0
260400120131     c                   eval       CLP_Prima_Fat_8 = 0
260500120131      *
260600120131     c                   clear                   Anno2_0           2 0
260700120131     c                   IF        Data_Prima_Fat_6 > 0
260800120131     c                   eval              Anno2_0 = Data_Prima_Fat_6/10000
260900120131     c                   if           Anno2_0 < 40
261000120131     c                   eval      Data_Prima_Fat_8 = 20000000 +
261100120131     c                                            Data_Prima_Fat_6
261200120131     c                   else
261300120131     c                   eval      Data_Prima_Fat_8 = 19000000 +
261400120131     c                                            Data_Prima_Fat_6
261500120131     c                   end
261600120131     c                   endIF
261700120131      *
261800120131     c                   clear                   Anno2_0           2 0
261900120131     c                   IF        CLPdps > 0
262000120131     c                   eval              Anno2_0 = CLPdps/10000
262100120131     c                   if           Anno2_0 < 40
262200120131     c                   eval      CLP_Prima_Fat_8 = 20000000 + CLPdps
262300120131     c                   else
262400120131     c                   eval      CLP_Prima_Fat_8 = 19000000 + CLPdps
262500120131     c                   end
262600120131     c                   endIF
262700120131      *
262800120125      **  deve memorizzare, fra tutti i clienti unificati, la più vecchia data
262900120125      ***   quindi
263000120125      **  la prima spedizione fatturata.
263100120131     C*
263200120131     c                   if             Data_Prima_Fat_8 > CLP_Prima_Fat_8 or
263300120131     c                                  Data_Prima_Fat_8 = 0
263400120213     c                   if                 CLPdps > 0
263500120125     c                   eval        Data_Prima_Fat_6 = CLPdps
263600120125     c                   end
263700120213     c                   end
263800061220      ***
263900061220      *  se almeno un codice figlio non è bloccato non lo deve segnalare come
264000061220      *  totalità.
264100061220     c                   if        skbl(is) <> '+'
264200061220     C                   movel     *blank        Total_Bloccato
264300061220     c                   end
264400061220      ***
264500051031     C*--------------------
264600000828     C                   ADD       1             I                              *CLIENTI
264700000828E   4C                   ENDDO                                                  *ROTTURA BLOCCO
264800000828     C*
264900051031     C* STAMPA BLOCCO DI CLIENTI unificati
265000051031     C*
265100000828IF  4C     IS            IFGT      *ZEROS                                       *CLIENTI DA STAMPARE
265200000828     C     $STRIG        OREQ      'S'                                          *RIGA DA STAMPARE
265300000829     C                   ADD       1             BLOCCO                         INCREMENTA N.BLOCCHI
265400000828     C                   Z-ADD     SKSC(1)       P3CC01
265500000828     C                   Z-ADD     SKSC(2)       P3CC02
265600000828     C                   Z-ADD     SKSC(3)       P3CC03
265700000828     C                   Z-ADD     SKSC(4)       P3CC04
265800000828     C                   Z-ADD     SKSC(5)       P3CC05
265900000828     C                   Z-ADD     SKSC(6)       P3CC06
266000000828     C                   Z-ADD     SKSC(7)       P3CC07
266100000828     C                   Z-ADD     SKSC(8)       P3CC08
266200000828     C                   Z-ADD     SKSC(9)       P3CC09
266300000828     C                   Z-ADD     SKSC(10)      P3CC10
266400000828     C                   Z-ADD     SKSC(11)      P3CC11
266500000828     C                   Z-ADD     SKSC(12)      P3CC12
266600000828     C                   Z-ADD     SKSC(13)      P3CC13
266700000828     C                   Z-ADD     SKSC(14)      P3CC14
266800000828     C                   Z-ADD     SKSC(15)      P3CC15
266900000828     C                   Z-ADD     SKSC(16)      P3CC16
267000000828     C                   Z-ADD     SKSC(17)      P3CC17
267100051031     C******             Z-ADD     SKSC(18)      P3CC18
267200051031     C******             Z-ADD     SKSC(19)      P3CC19
267300051031      **  Aggiunti Flag x cliente bloccato sotto unificante
267400051031      **   x questo ridotta la schiera SKSC per fare spazio in stampa
267500051031     C                   move      skbl(1)       P3Cf01
267600051031     C                   move      skbl(2)       P3Cf02
267700051031     C                   move      skbl(3)       P3Cf03
267800051031     C                   move      skbl(4)       P3Cf04
267900051031     C                   move      skbl(5)       P3Cf05
268000051031     C                   move      skbl(6)       P3Cf06
268100051031     C                   move      skbl(7)       P3Cf07
268200051031     C                   move      skbl(8)       P3Cf08
268300051031     C                   move      skbl(9)       P3Cf09
268400051031     C                   move      skbl(10)      P3Cf10
268500051031     C                   move      skbl(11)      P3Cf11
268600051031     C                   move      skbl(12)      P3Cf12
268700051031     C                   move      skbl(13)      P3Cf13
268800051031     C                   move      skbl(14)      P3Cf14
268900051031     C                   move      skbl(15)      P3Cf15
269000051031     C                   move      skbl(16)      P3Cf16
269100051031     C                   move      skbl(17)      P3Cf17
269200000829IF  5C     BLOCCO        IFEQ      1                                            *1° BLOCCO DA STAMP.
269300000829     C                   MOVEL     DRI(28)       P3DCLI
269400000829E   5C                   ENDIF
269500020131     C*
269600020131     C* PRIMA DI OGNI RIGA DA STAMPARE VERIFICA IL SALTO PAGINA
269700020131IF  1C     *IN01         IFEQ      *ON
269800020131IF  2C     PRIGIR        IFEQ      '0'
269900020131     C                   MOVEL     '1'           PRIGIR
270000020131X   2C                   ELSE
270100100129     c                   if        parSTAMPA <> 'N'
270200020131     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
270300100129     c                   end
270400020131E   2C                   ENDIF
270500020222     C*
270600020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
270700020222     C                   IF        FLGSTPLE = '1'
270800100129     c                   if        parSTAMPA <> 'N'
270900020222     C                   WRITE     SE94LE                                       *LEGENDA
271000100129     c                   end
271100020222     C                   ELSE
271200020222     C                   EVAL      FLGSTPLE = '1'
271300020222     C                   ENDIF
271400020222     C*
271500100129     c                   if        parSTAMPA <> 'N'
271600020222     C                   WRITE     SE94PT                                       *TESTATA
271700020131     C                   WRITE     SE94IN                                       *INTESTAZIONE
271800020131     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
271900020131     C                   WRITE     SE94PI                                       *RIGA MESI
272000020131     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
272100100129     c                   end
272200020131     C                   SETOFF                                       01
272300020131E   1C                   ENDIF
272400020221     C*
272500100129     c                   if        parSTAMPA <> 'N'
272600020116     C                   WRITE     SE94P3
272700100129     c                   end
272800020116     C                   CLEAR                   SE94P3                         AZZERA FORMATO
272900000828     C                   MOVEL     'N'           $STRIG                         RIGA DA NON STAMPARE
273000000828E   4C                   ENDIF
273100000828E   3C                   ENDDO                                                  *FINE FIGLI
273200000828E   2C                   ENDIF
273300000828E   1C                   ENDIF
273400000828     C*
273500000828IF  1C     $STRIG        IFEQ      'S'                                          *ANCORA DA STAMPARE
273600020131     C*
273700020131     C* PRIMA DI OGNI RIGA DA STAMPARE VERIFICA IL SALTO PAGINA
273800020131IF  1C     *IN01         IFEQ      *ON
273900020131IF  2C     PRIGIR        IFEQ      '0'
274000020131     C                   MOVEL     '1'           PRIGIR
274100020131X   2C                   ELSE
274200100129     c                   if        parSTAMPA <> 'N'
274300020131     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
274400100129     c                   end
274500020131E   2C                   ENDIF
274600020222     C*
274700020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
274800020222     C                   IF        FLGSTPLE = '1'
274900100129     c                   if        parSTAMPA <> 'N'
275000020222     C                   WRITE     SE94LE                                       *LEGENDA
275100100129     c                   end
275200020222     C                   ELSE
275300020222     C                   EVAL      FLGSTPLE = '1'
275400020222     C                   ENDIF
275500020222     C*
275600100129     c                   if        parSTAMPA <> 'N'
275700020222     C                   WRITE     SE94PT                                       *TESTATA
275800020131     C                   WRITE     SE94IN                                       *INTESTAZIONE
275900020131     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
276000020131     C                   WRITE     SE94PI                                       *RIGA MESI
276100020131     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
276200100129     c                   end
276300020131     C                   SETOFF                                       01
276400020131E   1C                   ENDIF
276500020221     C*
276600100129     c                   if        parSTAMPA <> 'N'
276700020116     C                   WRITE     SE94P3
276800100129     c                   end
276900000828E   1C                   ENDIF
277000000000     C*--------------------
277100000000     C* R I G A   V U O T A
277200000000     C*--------------------
277300020116     C                   CLEAR                   SE94P1
277400100129     c                   if        parSTAMPA <> 'N'
277500020116     C                   WRITE     SE94P1
277600100129     c                   end
277700000000     C*
277800030311     C* SE RICHIESTO NEL LANBCIO INSERISCO IL RECORD NEL FILE PC (WFDELPCF)
277900030311     C                   IF        PARFPC = 'S'
278000061220     C*
278100061220     C* aggiunto campo di segnalazione cliente totalmente bloccato
278200061220     C*  come se stesso + tutti gli eventuali figli se unificante:
278300100406     c*********>>>       eval      wfpBLO = Total_Bloccato
278400100406     c                   clear                   aaaBLO            1
278500120125     c                   clear                   aaaCULTBLO        3
278600120125     c                   clear                   aaaDULTBLO        8 0
278700120125     c                   clear                   aaaDPRIFAT        8 0
278800100406     c                   eval      aaaBLO = Total_Bloccato
278900120125      *
279000120125      **  solo se il codice unificante è Totalmente bloccato deve riportare
279100120125      **   il codice di blocco e la data dell'ultimo cliente che in ordine di tempo
279200120125      *   è stato bloccato.
279300120125     c                   if        Total_Bloccato = 'S'
279400120125     c                   eval       aaaCULTBLO =  Cod_Blocco_Ultimo
279500120125     c                   eval       aaaDULTBLO = Data_Blocco_Ultimo
279600120125     c                   end
279700120125      *  Inoltre si deve memorizzare quando è stata fatturata la prima spedizione
279800120125      *   fra tutti i clienti unificati.
279900120125     c                   clear                   Anno2_0           2 0
280000120125     c                   IF        Data_Prima_Fat_6 > 0
280100120125     c                   eval      Anno2_0 = Data_Prima_Fat_6/10000
280200120125     c                   if        Anno2_0 < 40
280300120125     c                   eval       aaaDPRIFAT = 20000000 + Data_Prima_Fat_6
280400120125     c                   else
280500120125     c                   eval       aaaDPRIFAT = 19000000 + Data_Prima_Fat_6
280600120125     c                   end
280700120125     c                   endIF
280800061220     C*
280900100511     C**------------>    WRITE     WFDELPC0
281000070418     C*
281100070418      * ?- Scrive il nuovo files WAdelpcf identico al WFdelpcf con i campi in +
281200070418      *              ? *---------------------*
281300070418     c                   exsr      wri_WAdel
281400070418      *              ? *---------------------*
281500100201     C*
281600100201     C*  Solo x Estero x Network
281700100201      * -------------
281800100201IF  1C     PARIOE        IFEQ      'E'
281900100201      *              ? *---------------------*
282000100201     C                   exsr      wriWADELntw
282100100201      *              ? *---------------------*
282200100201     C                   END
282300100201      * -------------
282400070418     C*
282500100216      * ?- Scrive il nuovo files WAdeApcI identico al WFdelpcf con i campi in + x NTW e Tipo Servizio
282600100201      *              ? *---------------------*
282700100201     c                   exsr      wri_WAdeA
282800100201      *              ? *---------------------*
282900100201     C*
283000030311     C                   ENDIF
283100030311     C*
283200000000     C                   ENDSR
283300070418     C*------------------------------------------------------------------------*
283400070418      *?  Scrive il nuovo WFile identico al WFDELPCF ma con informazioni più dettagliate    ?
283500070418     C*------------------------------------------------------------------------*
283600070418     C     wri_WAdel     BEGSR
283700070418     C*
283800070418      * ?Campi uguali al precedente WFile
283900070418     c                   clear                   WAdelPC0
284000100406     c*********>>>       eval      WaPAAD   =    WFPAAD
284100100406     c*********>>>       eval      WaPMMD   =    WFPMMD
284200100406     c*********>>>       eval      WaPAAA   =    WFPAAA
284300100406     c*********>>>       eval      WaPMMA   =    WFPMMA
284400100406     c*********>>>       eval      WaPIOE   =    WFPIOE
284500100406     c*********>>>       eval      WaPDIV   =    WFPDIV
284600100406     c*********>>>       eval      WaPDID   =    WFPDID
284700100406     c*********>>>       eval      WaPARE   =    WFPARE
284800100406     c*********>>>       eval      WaPARD   =    WFPARD
284900100406     c*********>>>       eval      WaPFIL   =    WFPFIL
285000100406     c*********>>>       eval      WaPFID   =    WFPFID
285100100406     c*********>>>       eval      WaPCLV   =    WFPCLV
285200100406     c*********>>>       eval      WaPCLI   =    WFPCLI
285300100406     c*********>>>       eval      WaPCLD   =    WFPCLD
285400100406     c*********>>>       eval      WaPITC   =    WFPITC
285500100406     c*********>>>       eval      WaPCLN   =    WFPCLN
285600100406     c*********>>>       eval      WaPAGE   =    WFPAGE
285700100406     c*********>>>       eval      WaPAGD   =    WFPAGD
285800100406     c*********>>>       eval      WaPSAP   =    WFPSAP
285900100406     c*********>>>       eval      WaPFAP   =    WFPFAP
286000100406     c*********>>>       eval      WaPDAP   =    WFPDAP
286100100406     c*********>>>       eval      WaPSAC   =    WFPSAC
286200100406     c*********>>>       eval      WaPFAC   =    WFPFAC
286300100406     c*********>>>       eval      WaPDAC   =    WFPDAC
286400100406     c*********>>>       eval      WaPBLO   =    WFPBLO
286500100406     c                   eval      WaPAAD   =    aaaAAD
286600100406     c                   eval      WaPMMD   =    aaaMMD
286700100406     c                   eval      WaPAAA   =    aaaAAA
286800100406     c                   eval      WaPMMA   =    aaaMMA
286900100406     c                   eval      WaPIOE   =    aaaIOE
287000100406     c                   eval      WaPDIV   =    aaaDIV
287100100406     c                   eval      WaPDID   =    aaaDID
287200100406     c                   eval      WaPARE   =    aaaARE
287300100406     c                   eval      WaPARD   =    aaaARD
287400100406     c                   eval      WaPFIL   =    aaaFIL
287500100406     c                   eval      WaPFID   =    aaaFID
287600100406     c                   eval      WaPCLV   =    aaaCLV
287700100406     c                   eval      WaPCLI   =    aaaCLI
287800100406     c                   eval      WaPCLD   =    aaaCLD
287900100406     c                   eval      WaPITC   =    aaaITC
288000100406     c                   eval      WaPCLN   =    aaaCLN
288100100406     c                   eval      WaPAGE   =    aaaAGE
288200100406     c                   eval      WaPAGD   =    aaaAGD
288300100406     c                   eval      WaPSAP   =    aaaSAP
288400100406     c                   eval      WaPFAP   =    aaaFAP
288500100406     c                   eval      WaPDAP   =    aaaDAP
288600100406     c                   eval      WaPSAC   =    aaaSAC
288700100406     c                   eval      WaPFAC   =    aaaFAC
288800100406     c                   eval      WaPDAC   =    aaaDAC
288900100406     c                   eval      WaPBLO   =    aaaBLO
289000120125     c                   eval      WApCDULBLO =  aaaCULTBLO
289100120125     c                   eval      WApDTULBLO =  aaaDULTBLO
289200120125     c                   eval      WApDTPRIFA =  aaaDPRIFAT
289300081013      *
289400070418      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
289500070419     C*--------------------
289600070419     C* Anno Precedente 12 Mesi
289700070419     C*--------------------
289800070419     C***    Spedizioni mensili
289900070419     C                   Z-ADD     ap_S1K(1)     wapNS_01aP
290000070419     C                   Z-ADD     ap_S1K(2)     wapNS_02aP
290100070419     C                   Z-ADD     ap_S1K(3)     wapNS_03aP
290200070419     C                   Z-ADD     ap_S1K(4)     wapNS_04aP
290300070419     C                   Z-ADD     ap_S1K(5)     wapNS_05aP
290400070419     C                   Z-ADD     ap_S1K(6)     wapNS_06aP
290500070419     C                   Z-ADD     ap_S1K(7)     wapNS_07aP
290600070419     C                   Z-ADD     ap_S1K(8)     wapNS_08aP
290700070419     C                   Z-ADD     ap_S1K(9)     wapNS_09aP
290800070419     C                   Z-ADD     ap_S1K(10)    wapNS_10aP
290900070419     C                   Z-ADD     ap_S1K(11)    wapNS_11aP
291000070419     C                   Z-ADD     ap_S1K(12)    wapNS_12aP
291100081013     C***    Pesi mensili
291200081016     c                   clear                   WaPkgP
291300081016     c                   add       ap_Kg1K(1)    WaPkgP
291400081016     c                   add       ap_Kg1K(2)    WaPkgP
291500081016     c                   add       ap_Kg1K(3)    WaPkgP
291600081016     c                   add       ap_Kg1K(4)    WaPkgP
291700081016     c                   add       ap_Kg1K(5)    WaPkgP
291800081016     c                   add       ap_Kg1K(6)    WaPkgP
291900081016     c                   add       ap_Kg1K(7)    WaPkgP
292000081016     c                   add       ap_Kg1K(8)    WaPkgP
292100081016     c                   add       ap_Kg1K(9)    WaPkgP
292200081016     c                   add       ap_Kg1K(10)   WaPkgP
292300081016     c                   add       ap_Kg1K(11)   WaPkgP
292400081016     c                   add       ap_Kg1K(12)   WaPkgP
292500081013     C                   Z-ADD     ap_Kg1K(1)    wapKG_01aP
292600081013     C                   Z-ADD     ap_Kg1K(2)    wapKG_02aP
292700081013     C                   Z-ADD     ap_Kg1K(3)    wapKG_03aP
292800081013     C                   Z-ADD     ap_Kg1K(4)    wapKG_04aP
292900081013     C                   Z-ADD     ap_Kg1K(5)    wapKG_05aP
293000081013     C                   Z-ADD     ap_Kg1K(6)    wapKG_06aP
293100081013     C                   Z-ADD     ap_Kg1K(7)    wapKG_07aP
293200081013     C                   Z-ADD     ap_Kg1K(8)    wapKG_08aP
293300081013     C                   Z-ADD     ap_Kg1K(9)    wapKG_09aP
293400081013     C                   Z-ADD     ap_Kg1K(10)   wapKG_10aP
293500081013     C                   Z-ADD     ap_Kg1K(11)   wapKG_11aP
293600081013     C                   Z-ADD     ap_Kg1K(12)   wapKG_12aP
293700070419     C***  Ricavi Reali mensili
293800070419     C                   Z-ADD     ap_R1K(1)     wapRR_01aP
293900070419     C                   Z-ADD     ap_R1K(2)     wapRR_02aP
294000070419     C                   Z-ADD     ap_R1K(3)     wapRR_03aP
294100070419     C                   Z-ADD     ap_R1K(4)     wapRR_04aP
294200070419     C                   Z-ADD     ap_R1K(5)     wapRR_05aP
294300070419     C                   Z-ADD     ap_R1K(6)     wapRR_06aP
294400070419     C                   Z-ADD     ap_R1K(7)     wapRR_07aP
294500070419     C                   Z-ADD     ap_R1K(8)     wapRR_08aP
294600070419     C                   Z-ADD     ap_R1K(9)     wapRR_09aP
294700070419     C                   Z-ADD     ap_R1K(10)    wapRR_10aP
294800070419     C                   Z-ADD     ap_R1K(11)    wapRR_11aP
294900070419     C                   Z-ADD     ap_R1K(12)    wapRR_12aP
295000070419     C***  Ricavi Presunti mensili
295100070419     C                   Z-ADD     ap_P1K(1)     wapRP_01aP
295200070419     C                   Z-ADD     ap_P1K(2)     wapRP_02aP
295300070419     C                   Z-ADD     ap_P1K(3)     wapRP_03aP
295400070419     C                   Z-ADD     ap_P1K(4)     wapRP_04aP
295500070419     C                   Z-ADD     ap_P1K(5)     wapRP_05aP
295600070419     C                   Z-ADD     ap_P1K(6)     wapRP_06aP
295700070419     C                   Z-ADD     ap_P1K(7)     wapRP_07aP
295800070419     C                   Z-ADD     ap_P1K(8)     wapRP_08aP
295900070419     C                   Z-ADD     ap_P1K(9)     wapRP_09aP
296000070419     C                   Z-ADD     ap_P1K(10)    wapRP_10aP
296100070419     C                   Z-ADD     ap_P1K(11)    wapRP_11aP
296200070419     C                   Z-ADD     ap_P1K(12)    wapRP_12aP
296300070419      *
296400070419     C*--------------------
296500070419     C* Anno in Corso 12 Mesi
296600070419     C*--------------------
296700070419     C***    Spedizioni mensili
296800070419     C                   Z-ADD     S1K(1)        wapNS_01aC
296900070419     C                   Z-ADD     S1K(2)        wapNS_02aC
297000070419     C                   Z-ADD     S1K(3)        wapNS_03aC
297100070419     C                   Z-ADD     S1K(4)        wapNS_04aC
297200070419     C                   Z-ADD     S1K(5)        wapNS_05aC
297300070419     C                   Z-ADD     S1K(6)        wapNS_06aC
297400070419     C                   Z-ADD     S1K(7)        wapNS_07aC
297500070419     C                   Z-ADD     S1K(8)        wapNS_08aC
297600070419     C                   Z-ADD     S1K(9)        wapNS_09aC
297700070419     C                   Z-ADD     S1K(10)       wapNS_10aC
297800070419     C                   Z-ADD     S1K(11)       wapNS_11aC
297900070419     C                   Z-ADD     S1K(12)       wapNS_12aC
298000081013     C***    Pesi mensili
298100081016     c                   clear                   WaPkgC
298200081016     c                   add       Kg1K(1)       WaPkgC
298300081016     c                   add       Kg1K(2)       WaPkgC
298400081016     c                   add       Kg1K(3)       WaPkgC
298500081016     c                   add       Kg1K(4)       WaPkgC
298600081016     c                   add       Kg1K(5)       WaPkgC
298700081016     c                   add       Kg1K(6)       WaPkgC
298800081016     c                   add       Kg1K(7)       WaPkgC
298900081016     c                   add       Kg1K(8)       WaPkgC
299000081016     c                   add       Kg1K(9)       WaPkgC
299100081016     c                   add       Kg1K(10)      WaPkgC
299200081016     c                   add       Kg1K(11)      WaPkgC
299300081016     c                   add       Kg1K(12)      WaPkgC
299400081013     C                   Z-ADD     Kg1K(1)       wapKG_01aC
299500081013     C                   Z-ADD     Kg1K(2)       wapKG_02aC
299600081013     C                   Z-ADD     Kg1K(3)       wapKG_03aC
299700081013     C                   Z-ADD     Kg1K(4)       wapKG_04aC
299800081013     C                   Z-ADD     Kg1K(5)       wapKG_05aC
299900081013     C                   Z-ADD     Kg1K(6)       wapKG_06aC
300000081013     C                   Z-ADD     Kg1K(7)       wapKG_07aC
300100081013     C                   Z-ADD     Kg1K(8)       wapKG_08aC
300200081013     C                   Z-ADD     Kg1K(9)       wapKG_09aC
300300081013     C                   Z-ADD     Kg1K(10)      wapKG_10aC
300400081013     C                   Z-ADD     Kg1K(11)      wapKG_11aC
300500081013     C                   Z-ADD     Kg1K(12)      wapKG_12aC
300600070419     C***  Ricavi Reali mensili
300700070419     C                   Z-ADD     R1K(1)        wapRR_01aC
300800070419     C                   Z-ADD     R1K(2)        wapRR_02aC
300900070419     C                   Z-ADD     R1K(3)        wapRR_03aC
301000070419     C                   Z-ADD     R1K(4)        wapRR_04aC
301100070419     C                   Z-ADD     R1K(5)        wapRR_05aC
301200070419     C                   Z-ADD     R1K(6)        wapRR_06aC
301300070419     C                   Z-ADD     R1K(7)        wapRR_07aC
301400070419     C                   Z-ADD     R1K(8)        wapRR_08aC
301500070419     C                   Z-ADD     R1K(9)        wapRR_09aC
301600070419     C                   Z-ADD     R1K(10)       wapRR_10aC
301700070419     C                   Z-ADD     R1K(11)       wapRR_11aC
301800070419     C                   Z-ADD     R1K(12)       wapRR_12aC
301900070419     C***  Ricavi Presunti mensili
302000070419     C                   Z-ADD     P1K(1)        wapRP_01aC
302100070419     C                   Z-ADD     P1K(2)        wapRP_02aC
302200070419     C                   Z-ADD     P1K(3)        wapRP_03aC
302300070419     C                   Z-ADD     P1K(4)        wapRP_04aC
302400070419     C                   Z-ADD     P1K(5)        wapRP_05aC
302500070419     C                   Z-ADD     P1K(6)        wapRP_06aC
302600070419     C                   Z-ADD     P1K(7)        wapRP_07aC
302700070419     C                   Z-ADD     P1K(8)        wapRP_08aC
302800070419     C                   Z-ADD     P1K(9)        wapRP_09aC
302900070419     C                   Z-ADD     P1K(10)       wapRP_10aC
303000070419     C                   Z-ADD     P1K(11)       wapRP_11aC
303100070419     C                   Z-ADD     P1K(12)       wapRP_12aC
303200070419      *
303300070418      * ?Campi in più anagrafici
303400070419     C                   eval      WAPIND   =    DESvia                         *Indirizzo
303500070615     C                   eval      WAPcdf   =    DEScdf                         *cod.fisc
303600070615     C                   eval      WAPiva   =    DESiva                         *P.IVA
303700070618     C                   eval      WAPtel   =    DEStel                         *Telefono
303800070419      * ?CAP Italia
303900070419     c                   if        DESsta = *blank
304000070419     C                   movel(p)  DEScap        WAPCAP                         *CAP
304100070419      * ?CAP Estero
304200070419     c                   else
304300070419     C                   eval      WAPCAP   =    DEScae                         *CAP Europeo
304400070419     c                   end
304500070420     C                   eval      WAPLOC   =    DEScit                         *Città
304600070420     C                   eval      WAPPRV   =    DESprv                         *Provincia
304700070419     C                   eval      WAPNAZ   =    DESsta                         *Nazione
304800070420     C*
304900070420      * ?Resposnabile Trasporto
305000070420     C                   movel     'C'           NTCapl                         *CODICE APPLICAZIONE
305100070420     C                   movel     '0151'        NTCnk1                         *CHIAVE 1 NOTE
305200070420     C                   move      WaPCLI        NTCnk1                         *CHIAVE 1 NOTE
305300070420     C                   movel     *blank        NTCnk2                         *CHIAVE 2 NOTE
305400070420     C                   movel     '05'          NTCtnt                         *TIPO NOTA
305500070420     C     KeyNTC        chain     tfNTC01l
305600070420     c                   if        %Found(tfNTC01l)
305700070420     c                   movel     NTCRNT        WAPRESP
305800070420     c                   end
305900070426     C* ?E-mail Resposnabile Trasporto
306000070426     C                   movel     '06'          NTCtnt                         *TIPO NOTA
306100070426     C     KeyNTC        chain     tfNTC01l
306200070426     c                   if        %Found(tfNTC01l)
306300070426     c                   movel     NTCRNT        WAPEMAR
306400070426     c                   end
306500070426     C*
306600071212     C* ?Mese di Inizio Fatturato x Cliente Nuovo
306700071212     c                   clear                   WapNew_Mes
306800071212     c                   if        WaPCLN = 'N'
306900071212     c                   do        12            mes_k             3 0
307000071212     c                   if        s1k(mes_k) <> 0 or
307100081013     c                             kg1k(mes_k) <> 0 or
307200081013     c                             r1k(mes_k) <> 0 or
307300071212     c                             p1k(mes_k) <> 0
307400121217      *
307500121217      * Deve leggere l'ultimo a causa dei mesi antecedenti al vero e proprio inizio del cliente
307600121217      *  (questo è dovuto a storni o note di credito attribuite al cliente spalmate anche su
307700121217      *   mesi antecedenti il suo inizio di fatturazione).
307800121217     c                   if        cln1k(mes_k) = 'Y'
307900071212     c                   z-add     mes_k         WapNew_Mes
308000121217     c******                leave
308100121217     c                   else
308200121217      *   se prima aveva impostato il campo lo fa uscire immediatamente
308300121217      *   ma solo se prima era passato per gli elementi con "Y"
308400121217     c                   if          WapNew_Mes > 0
308500121217     c                   leave
308600121217     c                   end
308700121217     c                   end
308800121217      *
308900071212     c                   endIF
309000071212     c                   endDO
309100071212     c                   end
309200071212     C*
309300081013      * ?Campi aggiunti: AGE Unificante e Descrizione
309400081013     C                   eval      WAPaud   =    depRGD                         *Descr.Unificante
309500081013     C                   eval      WAPauc   =    depRGF                         *Cod.  Unificante
309600081013     C*
309700070418     C                   WRITE     WADELPC0
309800091123     C*
309900070418     C                   ENDSR
310000000000     C*------------------------------------------------------------------------*
310100091123     C*  Scrive i record del WADEL x Estero in base al Network e la qualità
310200091123     C*------------------------------------------------------------------------*
310300091123     C     wriWADELntw   Begsr
310400091123     C*
310500091124     C* riporta I TOTALI x NETWORK
310600091124DO  1C     1             DO        5             K
310700091124     C*
310800100202     C     K             OCCUR     W1ntw
310900100201     C*
311000091124     C* Se non ci sono più Network esce dal ciclo
311100091124     c                   if        c1Ntw    = *blank  and
311200091124     c                             ap_c1Ntw = *blank
311300091124     c                   iter
311400091124     c                   end
311500091124     C*
311600091124      * ?Deve scrivere tanti records quanti sono i dati dei vari Network
311700091124     c                   clear                   WAdel00N
311800091124     C*
311900091124     c                   eval      WaNAAD   =    WaPAAD
312000091124     c                   eval      WaNMMD   =    WaPMMD
312100091124     c                   eval      WaNAAA   =    WaPAAA
312200091124     c                   eval      WaNMMA   =    WaPMMA
312300091124     c                   eval      WaNIOE   =    WaPIOE
312400091124     c                   eval      WaNDIV   =    WaPDIV
312500091124     c                   eval      WaNDID   =    WaPDID
312600091124     c                   eval      WaNARE   =    WaPARE
312700091124     c                   eval      WaNARD   =    WaPARD
312800091124     c                   eval      WaNFIL   =    WaPFIL
312900091124     c                   eval      WaNFID   =    WaPFID
313000091124     c                   eval      WaNCLV   =    WaPCLV
313100091124     c                   eval      WaNCLI   =    WaPCLI
313200091124     c                   eval      WaNCLD   =    WaPCLD
313300091124     c                   eval      WaNITC   =    WaPITC
313400091124     c                   eval      WaNCLN   =    WaPCLN
313500091124     c                   eval      WaNAGE   =    WaPAGE
313600091124     c                   eval      WaNAGD   =    WaPAGD
313700091124     c                   eval      WaNBLO   =    WaPBLO
313800120125     c                   eval      WAnCDULBLO =  WaPCDULBLO
313900120125     c                   eval      WAnDTULBLO =  WaPDTULBLO
314000120125     c                   eval      WAnDTPRIFA =  WaPDTPRIFA
314100091124      * ?Campi in più anagrafici
314200091124     C                   eval      WAnIND   =    WaPIND                         *Indirizzo
314300091124     C                   eval      WAncdf   =    WaPcdf                         *cod.fisc
314400091124     C                   eval      WAniva   =    WaPiva                         *P.IVA
314500091124     C                   eval      WAntel   =    WaPtel                         *Telefono
314600091124     C                   eval      WAnCAP   =    WaPCAP                         *CAP Europeo
314700091124     C                   eval      WAnLOC   =    WaPLOC                         *Città
314800091124     C                   eval      WAnPRV   =    WaPPRV                         *Provincia
314900091124     C                   eval      WAnNAZ   =    WaPNAZ                         *Nazione
315000091124     c                   eval      WAnRESP  =    WaPRESP
315100091124     c                   eval      WAnEMAR  =    WaPEMAR
315200091124     c                   eval      WanNew_Mes =  WaPNew_Mes
315300091124     C                   eval      WAnaud   =    WaPaud                         *Descr.Unificante
315400091124     C                   eval      WAnauc   =    WaPauc                         *Cod.  Unificante
315500091123      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
315600091124     c                   eval      WaNntw   =    c1NTW
315700091124     c                   if          WaNntw = *blank
315800091124     c                   eval      WaNntw   =    ap_c1NTW
315900091124     c                   end
316000091124     C***
316100091124     c                   xfoot     ap_S1ntw      WaNSAP
316200091124     c                   xfoot     S1ntw         WaNSAC
316300091124     C***
316400091125     c**********         z-add     ap_tR1ntw     WaNFAP
316500091125     c**********         z-add     ap_tP1ntw     WaNPAP
316600091125     c**********         z-add     tR1ntw        WaNFAC
316700091125     c**********         z-add     tP1ntw        WaNPAC
316800091125     c                   xfoot     ap_R1ntw      WaNFAP
316900091125     c                   xfoot     ap_P1ntw      WaNPAP
317000091125     c                   xfoot     R1ntw         WaNFAC
317100091125     c                   xfoot     P1ntw         WaNPAC
317200091124      ***
317300091124     c***                eval      WaNDAP   =    WaPDAP
317400091124     c***                eval      WaNDAC   =    WaPDAC
317500091124      ***
317600091124     C                   Z-ADD     WaNFAP        WRRW             19 5          *RICAVO REALE
317700091124     C                   Z-ADD     WaNPAP        WRPW             19 5          *RICAVO PRESUNTO
317800091124     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
317900091124     C                   EXSR      CALDEL
318000091124     C                   Z-ADD     WDPW          WaNDAP                         *DELTA
318100091124      ***
318200091124     C                   Z-ADD     WaNFAC        WRRW             19 5          *RICAVO REALE
318300091124     C                   Z-ADD     WaNPAC        WRPW             19 5          *RICAVO PRESUNTO
318400091124     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
318500091124     C                   EXSR      CALDEL
318600091124     C                   Z-ADD     WDPW          WaNDAC                         *DELTA
318700091123     C*--------------------
318800091123     C* Anno Precedente 12 Mesi
318900091123     C*--------------------
319000091123     C***    Spedizioni mensili
319100091124     C                   Z-ADD     ap_S1ntw(1)   wanNS_01aP
319200091124     C                   Z-ADD     ap_S1ntw(2)   wanNS_02aP
319300091124     C                   Z-ADD     ap_S1ntw(3)   wanNS_03aP
319400091124     C                   Z-ADD     ap_S1ntw(4)   wanNS_04aP
319500091124     C                   Z-ADD     ap_S1ntw(5)   wanNS_05aP
319600091124     C                   Z-ADD     ap_S1ntw(6)   wanNS_06aP
319700091124     C                   Z-ADD     ap_S1ntw(7)   wanNS_07aP
319800091124     C                   Z-ADD     ap_S1ntw(8)   wanNS_08aP
319900091124     C                   Z-ADD     ap_S1ntw(9)   wanNS_09aP
320000091124     C                   Z-ADD     ap_S1ntw(10)  wanNS_10aP
320100091124     C                   Z-ADD     ap_S1ntw(11)  wanNS_11aP
320200091124     C                   Z-ADD     ap_S1ntw(12)  wanNS_12aP
320300091123     C***    Pesi mensili
320400091124     c                   clear                   WankgP
320500091124     c                   add       ap_Kg1ntw(1)  WankgP
320600091124     c                   add       ap_Kg1ntw(2)  WankgP
320700091124     c                   add       ap_Kg1ntw(3)  WankgP
320800091124     c                   add       ap_Kg1ntw(4)  WankgP
320900091124     c                   add       ap_Kg1ntw(5)  WankgP
321000091124     c                   add       ap_Kg1ntw(6)  WankgP
321100091124     c                   add       ap_Kg1ntw(7)  WankgP
321200091124     c                   add       ap_Kg1ntw(8)  WankgP
321300091124     c                   add       ap_Kg1ntw(9)  WankgP
321400091124     c                   add       ap_Kg1ntw(10) WankgP
321500091124     c                   add       ap_Kg1ntw(11) WankgP
321600091124     c                   add       ap_Kg1ntw(12) WankgP
321700091124     C                   Z-ADD     ap_Kg1ntw(1)  wanKG_01aP
321800091124     C                   Z-ADD     ap_Kg1ntw(2)  wanKG_02aP
321900091124     C                   Z-ADD     ap_Kg1ntw(3)  wanKG_03aP
322000091124     C                   Z-ADD     ap_Kg1ntw(4)  wanKG_04aP
322100091124     C                   Z-ADD     ap_Kg1ntw(5)  wanKG_05aP
322200091124     C                   Z-ADD     ap_Kg1ntw(6)  wanKG_06aP
322300091124     C                   Z-ADD     ap_Kg1ntw(7)  wanKG_07aP
322400091124     C                   Z-ADD     ap_Kg1ntw(8)  wanKG_08aP
322500091124     C                   Z-ADD     ap_Kg1ntw(9)  wanKG_09aP
322600091124     C                   Z-ADD     ap_Kg1ntw(10) wanKG_10aP
322700091124     C                   Z-ADD     ap_Kg1ntw(11) wanKG_11aP
322800091124     C                   Z-ADD     ap_Kg1ntw(12) wanKG_12aP
322900091123     C***  Ricavi Reali mensili
323000091124     C                   Z-ADD     ap_R1ntw(1)   wanRR_01aP
323100091124     C                   Z-ADD     ap_R1ntw(2)   wanRR_02aP
323200091124     C                   Z-ADD     ap_R1ntw(3)   wanRR_03aP
323300091124     C                   Z-ADD     ap_R1ntw(4)   wanRR_04aP
323400091124     C                   Z-ADD     ap_R1ntw(5)   wanRR_05aP
323500091124     C                   Z-ADD     ap_R1ntw(6)   wanRR_06aP
323600091124     C                   Z-ADD     ap_R1ntw(7)   wanRR_07aP
323700091124     C                   Z-ADD     ap_R1ntw(8)   wanRR_08aP
323800091124     C                   Z-ADD     ap_R1ntw(9)   wanRR_09aP
323900091124     C                   Z-ADD     ap_R1ntw(10)  wanRR_10aP
324000091124     C                   Z-ADD     ap_R1ntw(11)  wanRR_11aP
324100091124     C                   Z-ADD     ap_R1ntw(12)  wanRR_12aP
324200091123     C***  Ricavi Presunti mensili
324300091124     C                   Z-ADD     ap_P1ntw(1)   wanRP_01aP
324400091124     C                   Z-ADD     ap_P1ntw(2)   wanRP_02aP
324500091124     C                   Z-ADD     ap_P1ntw(3)   wanRP_03aP
324600091124     C                   Z-ADD     ap_P1ntw(4)   wanRP_04aP
324700091124     C                   Z-ADD     ap_P1ntw(5)   wanRP_05aP
324800091124     C                   Z-ADD     ap_P1ntw(6)   wanRP_06aP
324900091124     C                   Z-ADD     ap_P1ntw(7)   wanRP_07aP
325000091124     C                   Z-ADD     ap_P1ntw(8)   wanRP_08aP
325100091124     C                   Z-ADD     ap_P1ntw(9)   wanRP_09aP
325200091124     C                   Z-ADD     ap_P1ntw(10)  wanRP_10aP
325300091124     C                   Z-ADD     ap_P1ntw(11)  wanRP_11aP
325400091124     C                   Z-ADD     ap_P1ntw(12)  wanRP_12aP
325500091123     C*--------------------
325600091123     C* Anno in Corso 12 Mesi
325700091123     C*--------------------
325800091123     C***    Spedizioni mensili
325900091124     C                   Z-ADD     S1ntw(1)      wanNS_01aC
326000091124     C                   Z-ADD     S1ntw(2)      wanNS_02aC
326100091124     C                   Z-ADD     S1ntw(3)      wanNS_03aC
326200091124     C                   Z-ADD     S1ntw(4)      wanNS_04aC
326300091124     C                   Z-ADD     S1ntw(5)      wanNS_05aC
326400091124     C                   Z-ADD     S1ntw(6)      wanNS_06aC
326500091124     C                   Z-ADD     S1ntw(7)      wanNS_07aC
326600091124     C                   Z-ADD     S1ntw(8)      wanNS_08aC
326700091124     C                   Z-ADD     S1ntw(9)      wanNS_09aC
326800091124     C                   Z-ADD     S1ntw(10)     wanNS_10aC
326900091124     C                   Z-ADD     S1ntw(11)     wanNS_11aC
327000091124     C                   Z-ADD     S1ntw(12)     wanNS_12aC
327100091123     C***    Pesi mensili
327200091124     c                   clear                   WankgC
327300091124     c                   add       Kg1ntw(1)     WankgC
327400091124     c                   add       Kg1ntw(2)     WankgC
327500091124     c                   add       Kg1ntw(3)     WankgC
327600091124     c                   add       Kg1ntw(4)     WankgC
327700091124     c                   add       Kg1ntw(5)     WankgC
327800091124     c                   add       Kg1ntw(6)     WankgC
327900091124     c                   add       Kg1ntw(7)     WankgC
328000091124     c                   add       Kg1ntw(8)     WankgC
328100091124     c                   add       Kg1ntw(9)     WankgC
328200091124     c                   add       Kg1ntw(10)    WankgC
328300091124     c                   add       Kg1ntw(11)    WankgC
328400091124     c                   add       Kg1ntw(12)    WankgC
328500091124     C                   Z-ADD     Kg1ntw(1)     wanKG_01aC
328600091124     C                   Z-ADD     Kg1ntw(2)     wanKG_02aC
328700091124     C                   Z-ADD     Kg1ntw(3)     wanKG_03aC
328800091124     C                   Z-ADD     Kg1ntw(4)     wanKG_04aC
328900091124     C                   Z-ADD     Kg1ntw(5)     wanKG_05aC
329000091124     C                   Z-ADD     Kg1ntw(6)     wanKG_06aC
329100091124     C                   Z-ADD     Kg1ntw(7)     wanKG_07aC
329200091124     C                   Z-ADD     Kg1ntw(8)     wanKG_08aC
329300091124     C                   Z-ADD     Kg1ntw(9)     wanKG_09aC
329400091124     C                   Z-ADD     Kg1ntw(10)    wanKG_10aC
329500091124     C                   Z-ADD     Kg1ntw(11)    wanKG_11aC
329600091124     C                   Z-ADD     Kg1ntw(12)    wanKG_12aC
329700091123     C***  Ricavi Reali mensili
329800091124     C                   Z-ADD     R1ntw(1)      wanRR_01aC
329900091124     C                   Z-ADD     R1ntw(2)      wanRR_02aC
330000091124     C                   Z-ADD     R1ntw(3)      wanRR_03aC
330100091124     C                   Z-ADD     R1ntw(4)      wanRR_04aC
330200091124     C                   Z-ADD     R1ntw(5)      wanRR_05aC
330300091124     C                   Z-ADD     R1ntw(6)      wanRR_06aC
330400091124     C                   Z-ADD     R1ntw(7)      wanRR_07aC
330500091124     C                   Z-ADD     R1ntw(8)      wanRR_08aC
330600091124     C                   Z-ADD     R1ntw(9)      wanRR_09aC
330700091124     C                   Z-ADD     R1ntw(10)     wanRR_10aC
330800091124     C                   Z-ADD     R1ntw(11)     wanRR_11aC
330900091124     C                   Z-ADD     R1ntw(12)     wanRR_12aC
331000091123     C***  Ricavi Presunti mensili
331100091124     C                   Z-ADD     P1ntw(1)      wanRP_01aC
331200091124     C                   Z-ADD     P1ntw(2)      wanRP_02aC
331300091124     C                   Z-ADD     P1ntw(3)      wanRP_03aC
331400091124     C                   Z-ADD     P1ntw(4)      wanRP_04aC
331500091124     C                   Z-ADD     P1ntw(5)      wanRP_05aC
331600091124     C                   Z-ADD     P1ntw(6)      wanRP_06aC
331700091124     C                   Z-ADD     P1ntw(7)      wanRP_07aC
331800091124     C                   Z-ADD     P1ntw(8)      wanRP_08aC
331900091124     C                   Z-ADD     P1ntw(9)      wanRP_09aC
332000091124     C                   Z-ADD     P1ntw(10)     wanRP_10aC
332100091124     C                   Z-ADD     P1ntw(11)     wanRP_11aC
332200091124     C                   Z-ADD     P1ntw(12)     wanRP_12aC
332300091124      *
332400091124     C                   WRITE     WADEL00n
332500091124     C                   ENDdo
332600091123     C*
332700091123     C                   ENDSR
332800100201      * ?----------------------------------------------------------------------
332900100201      *?  Scrive il nuovo WFile identico al WFDELPCF ma con informazioni in più   ?
333000100201      *?    NETWORK e TIPO SERVIZIO con anche DETTAGLIO VARIE "e" / "h"           ?
333100100201      * ?----------------------------------------------------------------------
333200100201     C     wri_WAdeA     BEGSR
333300100201      *
333400100201     c                   eval      numElem = %Elem(W1ntwTS)
333500100201     c**
333600100201      **  Quindi se da totalizzare cerca su quale occurence deve metterlo.
333700160509     c                   do        NUMELEM       kk
333800100201     C*
333900100202     C*  Attenzione alle schiere fra Anno in corso e Anno precedente
334000100202      * ?Deve scrivere tanti records quanti sono i dati dei vari Network/TServizi
334100160509     C     Kk            OCCUR     W1ntwTS
334200100201     C*
334300100204     C* Se non ci sono più Network/tipi servizio esce dal ciclo
334400100204     c                   if        c1NtwTS  = *blank  and c1tpsTS = *blank
334500100202     c                   leave
334600100201     c                   end
334700100201     C*
334800100201      * ?Deve scrivere tanti records quanti sono i dati dei vari Network
334900100201     c                   clear                   WAdeAPC0
335000100201     C*
335100100201     c                   eval      WadAAD   =    WaPAAD
335200100201     c                   eval      WadMMD   =    WaPMMD
335300100201     c                   eval      WadAAA   =    WaPAAA
335400100201     c                   eval      WadMMA   =    WaPMMA
335500100201     c                   eval      WadIOE   =    WaPIOE
335600100201     c                   eval      WadDIV   =    WaPDIV
335700100201     c                   eval      WadDID   =    WaPDID
335800100201     c                   eval      WadARE   =    WaPARE
335900100201     c                   eval      WadARD   =    WaPARD
336000100201     c                   eval      WadFIL   =    WaPFIL
336100100201     c                   eval      WadFID   =    WaPFID
336200100201     c                   eval      WadCLV   =    WaPCLV
336300100201     c                   eval      WadCLI   =    WaPCLI
336400100201     c                   eval      WadCLD   =    WaPCLD
336500100201     c                   eval      WadITC   =    WaPITC
336600100201     c                   eval      WadCLN   =    WaPCLN
336700100201     c                   eval      WadAGE   =    WaPAGE
336800100201     c                   eval      WadAGD   =    WaPAGD
336900100201     c                   eval      WadBLO   =    WaPBLO
337000120125     c                   eval      WAdCDULBLO =  WaPCDULBLO
337100120125     c                   eval      WAdDTULBLO =  WaPDTULBLO
337200120125     c                   eval      WAdDTPRIFA =  WaPDTPRIFA
337300100201      * ?Campi in più anagrafici
337400100201     C                   eval      WAdIND   =    WaPIND                         *Indirizzo
337500100201     C                   eval      WAdcdf   =    WaPcdf                         *cod.fisc
337600100201     C                   eval      WAdiva   =    WaPiva                         *P.IVA
337700100201     C                   eval      WAdtel   =    WaPtel                         *Telefono
337800100201     C                   eval      WAdCAP   =    WaPCAP                         *CAP Europeo
337900100201     C                   eval      WAdLOC   =    WaPLOC                         *Città
338000100201     C                   eval      WAdPRV   =    WaPPRV                         *Provincia
338100100201     C                   eval      WAdNAZ   =    WaPNAZ                         *Nazione
338200100201     c                   eval      WAdRESP  =    WaPRESP
338300100201     c                   eval      WAdEMAR  =    WaPEMAR
338400100201     c                   eval      WadNew_Mes =  WaPNew_Mes
338500100201     C                   eval      WAdaud   =    WaPaud                         *Descr.Unificante
338600100201     C                   eval      WAdauc   =    WaPauc                         *Cod.  Unificante
338700100201      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
338800100215     c                   if        c1ntwTS  = 'ITA'
338900100215     c                   eval      Wadntw   =    *blank
339000100215     c                   else
339100100202     c                   eval      Wadntw   =    c1ntwTS
339200100215     c                   end
339300100215     c                   eval      WadTSP   =    c1tpsTS
339400100201     C***
339500100202     c                   xfoot     ap_S1ntwTS    WadSAP
339600100202     c                   xfoot     S1ntwTS       WadSAC
339700100201     C***
339800100202     c                   xfoot     ap_R1ntwTS    WadFAP
339900100202     c                   xfoot     ap_P1ntwTS    WadPAP
340000100202     c                   xfoot     R1ntwTS       WadFAC
340100100202     c                   xfoot     P1ntwTS       WadPAC
340200100201      ***
340300100201     C                   Z-ADD     WadFAP        WRRW             19 5          *RICAVO REALE
340400100201     C                   Z-ADD     WadPAP        WRPW             19 5          *RICAVO PRESUNTO
340500100201     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
340600100201     C                   EXSR      CALDEL
340700100201     C                   Z-ADD     WDPW          WaDDAP                         *DELTA
340800100201      ***
340900100201     C                   Z-ADD     WaDFAC        WRRW             19 5          *RICAVO REALE
341000100201     C                   Z-ADD     WaDPAC        WRPW             19 5          *RICAVO PRESUNTO
341100100201     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
341200100201     C                   EXSR      CALDEL
341300100201     C                   Z-ADD     WDPW          WaDDAC                         *DELTA
341400100201     C*--------------------
341500100201     C* Anno Precedente 12 Mesi
341600100201     C*--------------------
341700100201     C***    Spedizioni mensili
341800100202     C                   Z-ADD     ap_S1ntwTS(1) wadNS_01aP
341900100202     C                   Z-ADD     ap_S1ntwTS(2) wadNS_02aP
342000100202     C                   Z-ADD     ap_S1ntwTS(3) wadNS_03aP
342100100202     C                   Z-ADD     ap_S1ntwTS(4) wadNS_04aP
342200100202     C                   Z-ADD     ap_S1ntwTS(5) wadNS_05aP
342300100202     C                   Z-ADD     ap_S1ntwTS(6) wadNS_06aP
342400100202     C                   Z-ADD     ap_S1ntwTS(7) wadNS_07aP
342500100202     C                   Z-ADD     ap_S1ntwTS(8) wadNS_08aP
342600100202     C                   Z-ADD     ap_S1ntwTS(9) wadNS_09aP
342700100202     C                   Z-ADD     ap_S1ntwTS(10)wadNS_10aP
342800100202     C                   Z-ADD     ap_S1ntwTS(11)wadNS_11aP
342900100202     C                   Z-ADD     ap_S1ntwTS(12)wadNS_12aP
343000100201     C***    Pesi mensili
343100100201     c                   clear                   WadkgP
343200100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(1)
343300100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(2)
343400100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(3)
343500100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(4)
343600100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(5)
343700100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(6)
343800100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(7)
343900100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(8)
344000100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(9)
344100100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(10)
344200100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(11)
344300100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(12)
344400100202     C                   Z-ADD     ap_Kg1ntwTS(1)wadKG_01aP
344500100202     C                   Z-ADD     ap_Kg1ntwTS(2)wadKG_02aP
344600100202     C                   Z-ADD     ap_Kg1ntwTS(3)wadKG_03aP
344700100202     C                   Z-ADD     ap_Kg1ntwTS(4)wadKG_04aP
344800100202     C                   Z-ADD     ap_Kg1ntwTS(5)wadKG_05aP
344900100202     C                   Z-ADD     ap_Kg1ntwTS(6)wadKG_06aP
345000100202     C                   Z-ADD     ap_Kg1ntwTS(7)wadKG_07aP
345100100202     C                   Z-ADD     ap_Kg1ntwTS(8)wadKG_08aP
345200100202     C                   Z-ADD     ap_Kg1ntwTS(9)wadKG_09aP
345300100202     c                   eval      wadKG_01aP = ap_Kg1ntwTS(1)
345400100202     c                   eval      wadKG_02aP = ap_Kg1ntwTS(2)
345500100202     c                   eval      wadKG_03aP = ap_Kg1ntwTS(3)
345600100202     c                   eval      wadKG_04aP = ap_Kg1ntwTS(4)
345700100202     c                   eval      wadKG_05aP = ap_Kg1ntwTS(5)
345800100202     c                   eval      wadKG_06aP = ap_Kg1ntwTS(6)
345900100202     c                   eval      wadKG_07aP = ap_Kg1ntwTS(7)
346000100202     c                   eval      wadKG_08aP = ap_Kg1ntwTS(8)
346100100202     c                   eval      wadKG_09aP = ap_Kg1ntwTS(9)
346200100202     c                   eval      wadKG_10aP = ap_Kg1ntwTS(10)
346300100202     c                   eval      wadKG_11aP = ap_Kg1ntwTS(11)
346400100202     c                   eval      wadKG_12aP = ap_Kg1ntwTS(12)
346500100201     C***  Ricavi Reali mensili
346600100202     C                   Z-ADD     ap_R1ntwTS(1) wadRR_01aP
346700100202     C                   Z-ADD     ap_R1ntwTS(2) wadRR_02aP
346800100202     C                   Z-ADD     ap_R1ntwTS(3) wadRR_03aP
346900100202     C                   Z-ADD     ap_R1ntwTS(4) wadRR_04aP
347000100202     C                   Z-ADD     ap_R1ntwTS(5) wadRR_05aP
347100100202     C                   Z-ADD     ap_R1ntwTS(6) wadRR_06aP
347200100202     C                   Z-ADD     ap_R1ntwTS(7) wadRR_07aP
347300100202     C                   Z-ADD     ap_R1ntwTS(8) wadRR_08aP
347400100202     C                   Z-ADD     ap_R1ntwTS(9) wadRR_09aP
347500100202     C                   Z-ADD     ap_R1ntwTS(10)wadRR_10aP
347600100202     C                   Z-ADD     ap_R1ntwTS(11)wadRR_11aP
347700100202     C                   Z-ADD     ap_R1ntwTS(12)wadRR_12aP
347800100201     C***  Ricavi Presunti mensili
347900100202     C                   Z-ADD     ap_P1ntwTS(1) wadRP_01aP
348000100202     C                   Z-ADD     ap_P1ntwTS(2) wadRP_02aP
348100100202     C                   Z-ADD     ap_P1ntwTS(3) wadRP_03aP
348200100202     C                   Z-ADD     ap_P1ntwTS(4) wadRP_04aP
348300100202     C                   Z-ADD     ap_P1ntwTS(5) wadRP_05aP
348400100202     C                   Z-ADD     ap_P1ntwTS(6) wadRP_06aP
348500100202     C                   Z-ADD     ap_P1ntwTS(7) wadRP_07aP
348600100202     C                   Z-ADD     ap_P1ntwTS(8) wadRP_08aP
348700100202     C                   Z-ADD     ap_P1ntwTS(9) wadRP_09aP
348800100202     C                   Z-ADD     ap_P1ntwTS(10)wadRP_10aP
348900100202     C                   Z-ADD     ap_P1ntwTS(11)wadRP_11aP
349000100202     C                   Z-ADD     ap_P1ntwTS(12)wadRP_12aP
349100100201     C*--------------------
349200100201     C* Anno in Corso 12 Mesi
349300100201     C*--------------------
349400100201     C***    Spedizioni mensili
349500100202     C                   Z-ADD     S1ntwTS(1)    wadNS_01aC
349600100202     C                   Z-ADD     S1ntwTS(2)    wadNS_02aC
349700100202     C                   Z-ADD     S1ntwTS(3)    wadNS_03aC
349800100202     C                   Z-ADD     S1ntwTS(4)    wadNS_04aC
349900100202     C                   Z-ADD     S1ntwTS(5)    wadNS_05aC
350000100202     C                   Z-ADD     S1ntwTS(6)    wadNS_06aC
350100100202     C                   Z-ADD     S1ntwTS(7)    wadNS_07aC
350200100202     C                   Z-ADD     S1ntwTS(8)    wadNS_08aC
350300100202     C                   Z-ADD     S1ntwTS(9)    wadNS_09aC
350400100202     C                   Z-ADD     S1ntwTS(10)   wadNS_10aC
350500100202     C                   Z-ADD     S1ntwTS(11)   wadNS_11aC
350600100202     C                   Z-ADD     S1ntwTS(12)   wadNS_12aC
350700100201     C***    Pesi mensili
350800100201     c                   clear                   WadkgC
350900100202     c                   add       Kg1ntwTS(1)   WadkgC
351000100202     c                   add       Kg1ntwTS(2)   WadkgC
351100100202     c                   add       Kg1ntwTS(3)   WadkgC
351200100202     c                   add       Kg1ntwTS(4)   WadkgC
351300100202     c                   add       Kg1ntwTS(5)   WadkgC
351400100202     c                   add       Kg1ntwTS(6)   WadkgC
351500100202     c                   add       Kg1ntwTS(7)   WadkgC
351600100202     c                   add       Kg1ntwTS(8)   WadkgC
351700100202     c                   add       Kg1ntwTS(9)   WadkgC
351800100202     c                   add       Kg1ntwTS(10)  WadkgC
351900100202     c                   add       Kg1ntwTS(11)  WadkgC
352000100202     c                   add       Kg1ntwTS(12)  WadkgC
352100100202     C                   Z-ADD     Kg1ntwTS(1)   wadKG_01aC
352200100202     C                   Z-ADD     Kg1ntwTS(2)   wadKG_02aC
352300100202     C                   Z-ADD     Kg1ntwTS(3)   wadKG_03aC
352400100202     C                   Z-ADD     Kg1ntwTS(4)   wadKG_04aC
352500100202     C                   Z-ADD     Kg1ntwTS(5)   wadKG_05aC
352600100202     C                   Z-ADD     Kg1ntwTS(6)   wadKG_06aC
352700100202     C                   Z-ADD     Kg1ntwTS(7)   wadKG_07aC
352800100202     C                   Z-ADD     Kg1ntwTS(8)   wadKG_08aC
352900100202     C                   Z-ADD     Kg1ntwTS(9)   wadKG_09aC
353000100202     C                   Z-ADD     Kg1ntwTS(10)  wadKG_10aC
353100100202     C                   Z-ADD     Kg1ntwTS(11)  wadKG_11aC
353200100202     C                   Z-ADD     Kg1ntwTS(12)  wadKG_12aC
353300100201     C***  Ricavi Reali mensili
353400100202     C                   Z-ADD     R1ntwTS(1)    wadRR_01aC
353500100202     C                   Z-ADD     R1ntwTS(2)    wadRR_02aC
353600100202     C                   Z-ADD     R1ntwTS(3)    wadRR_03aC
353700100202     C                   Z-ADD     R1ntwTS(4)    wadRR_04aC
353800100202     C                   Z-ADD     R1ntwTS(5)    wadRR_05aC
353900100202     C                   Z-ADD     R1ntwTS(6)    wadRR_06aC
354000100202     C                   Z-ADD     R1ntwTS(7)    wadRR_07aC
354100100202     C                   Z-ADD     R1ntwTS(8)    wadRR_08aC
354200100202     C                   Z-ADD     R1ntwTS(9)    wadRR_09aC
354300100202     C                   Z-ADD     R1ntwTS(10)   wadRR_10aC
354400100202     C                   Z-ADD     R1ntwTS(11)   wadRR_11aC
354500100202     C                   Z-ADD     R1ntwTS(12)   wadRR_12aC
354600100201     C***  Ricavi Presunti mensili
354700100202     C                   Z-ADD     P1ntwTS(1)    wadRP_01aC
354800100202     C                   Z-ADD     P1ntwTS(2)    wadRP_02aC
354900100202     C                   Z-ADD     P1ntwTS(3)    wadRP_03aC
355000100202     C                   Z-ADD     P1ntwTS(4)    wadRP_04aC
355100100202     C                   Z-ADD     P1ntwTS(5)    wadRP_05aC
355200100202     C                   Z-ADD     P1ntwTS(6)    wadRP_06aC
355300100202     C                   Z-ADD     P1ntwTS(7)    wadRP_07aC
355400100202     C                   Z-ADD     P1ntwTS(8)    wadRP_08aC
355500100202     C                   Z-ADD     P1ntwTS(9)    wadRP_09aC
355600100202     C                   Z-ADD     P1ntwTS(10)   wadRP_10aC
355700100202     C                   Z-ADD     P1ntwTS(11)   wadRP_11aC
355800100202     C                   Z-ADD     P1ntwTS(12)   wadRP_12aC
355900100204      *  i campi delle Varie verranno aggiunti in seguito solo sul file di Totale
356000100202      *
356100100202     C                   WRITE     WADEAPC0
356200100201     C                   ENDdo
356300100201     C*
356400100201     C                   ENDSR
356500091123     C*------------------------------------------------------------------------*
356600020221     C*?STAAGE - IMPOSTA STAMPA TOTALI AGENTE COMMERCIALE
356700000000     C*------------------------------------------------------------------------*
356800000000     C     STAAGE        BEGSR
356900011001     C*
357000011001     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI COMMERCIALI
357100011001 FG  C                   IF        PARSIN = 'S'
357200011001     C                   SETON                                        53
357300011105     C                   ELSE
357400011105     C                   SETOFF                                       53
357500011001     C                   ENDIF
357600000000     C*
357700000000     C* AZZERA I TOTALI NELLA FORMA CLI, SPE, RIC, DEL
357800000000     C                   CLEAR                   CLW                            *NUMERO CLIENTI
357900000000     C                   CLEAR                   SPW                            *SPEDIZIONI
358000000000     C                   CLEAR                   RRW                            *RICAVO REALE
358100000000     C                   CLEAR                   RPW                            *RICAVO PRESUNTO
358200000000     C                   CLEAR                   CTW                            *13° TOTALE CLEINTI
358300000000     C*
358400020220     C* CHIUDO IL PROSPETTO DEL DETTAGLIO CLIENTI
358500000000IF  1C     ORAKSC        IFEQ      '1'
358600100129     c                   if        parSTAMPA <> 'N'
358700020116     C                   WRITE     SE94R2
358800100129     c                   end
358900000000     C                   MOVEL     '0'           ORAKSC                         *ORA NON STAMPA CLI
359000000000E   1C                   ENDIF
359100000000     C*
359200000000     C* STAMPA I VALORI DELL'AGENTE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
359300020318     C* LA STATISTICA DELTA CON DETTAGLIO FILIALE O CON DETTAGLIO AREA
359400000000IF  1C     PASAGE        IFEQ      '1'
359500000000IF  2C     PARSTA        IFEQ      'K'
359600000000     C     PARSTA        OREQ      'C'
359700000000     C***
359800000000     C* DESCRIZIONI INTESTAZIONE
359900000000     C***
360000020116     C                   CLEAR                   SE94IN
360100000000     C                   MOVEL     DESAGE        DES25            25            *DESCRIZIONE
360200000000     C                   MOVEL     DE2PCF        DES04             4            *PERCENTUALE
360300000000IF  3C     DE2PCF        IFGT      *BLANKS
360400000000     C                   MOVE      '%'           DES04
360500000000X   3C                   ELSE
360600000000     C                   MOVE      ' '           DES04
360700000000E   3C                   ENDIF
360800000000IF  3C     PARSTA        IFEQ      'K'
360900000000     C                   Z-ADD     DEPAGE        INCAGE                         *AGENTE
361000000000     C                   MOVEL     DES25         INDAGE                         *DESCRIZ + PERCEN
361100000000     C                   MOVE      DES04         INDAGE
361200000000X   3C                   ELSE
361300000000     C                   Z-ADD     *ZEROS        INCAGE
361400000000     C                   MOVEL     DRI(1)        INDAGE
361500000000E   3C                   ENDIF
361600000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
361700000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
361800000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
361900000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
362000000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
362100000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
362200950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
362300030924     C*
362400030924     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
362500030924     C                   MOVEL     DRI(21)       PIDDES                         *RETTANGOLINO
362600030924     C*
362700030924     C* SE DELTA FILIALE STAMPA I TOTALI AGENTE NELLA FORMA: TOT A, TOT B, TOT C
362800000000IF  3C     PARSTA        IFEQ      'K'
362900020222     C*
363000020221     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
363100020221DO  4C     1             DO        4             Q
363200000000     C     Q             OCCUR     ABCCOM
363300000000     C     Q             OCCUR     ABCSTA
363400000000     C                   Z-ADD     CNC           CNS
363500000000     C                   Z-ADD     SNC           SNS
363600000000     C                   Z-ADD     RNC           RNS
363700000000     C                   Z-ADD     PNC           PNS
363800030924     C                   Z-ADD     CAC           CAS
363900030924     C                   Z-ADD     SAC           SAS
364000030924     C                   Z-ADD     RAC           RAS
364100030924     C                   Z-ADD     PAC           PAS
364200000000     C*
364300000000     C* NUMERO TOTALE CLIENTI
364400000000     C     Q             OCCUR     ABCC13
364500000000     C     Q             OCCUR     ABCS13
364600000000     C                   Z-ADD     NTC           NTS
364700000000     C                   Z-ADD     ATC           ATS
364800000000     C*
364900000000E   4C                   ENDDO
365000020220     C*
365100020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA X CLIENTI 'D'
365200020220     C     5             OCCUR     ABCCOM
365300020220     C     5             OCCUR     ABCSTA
365400020220     C                   Z-ADD     CNC           CNS
365500020220     C                   Z-ADD     SNC           SNS
365600020220     C                   Z-ADD     RNC           RNS
365700020220     C                   Z-ADD     PNC           PNS
365800020220     C                   Z-ADD     CAC           CAS
365900020220     C                   Z-ADD     SAC           SAS
366000020220     C                   Z-ADD     RAC           RAS
366100020220     C                   Z-ADD     PAC           PAS
366200020220     C*
366300020220     C* NUMERO TOTALE CLIENTI 'D'
366400020220     C     5             OCCUR     ABCC13
366500020220     C     5             OCCUR     ABCS13
366600020220     C                   Z-ADD     NTC           NTS
366700020220     C                   Z-ADD     ATC           ATS
366800020318     C*
366900020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
367000020318DO  4C     1             DO        5             K
367100020318     C     K             OCCUR     ABCCOMN
367200020318     C     K             OCCUR     ABCSTAN
367300020318     C                   Z-ADD     CNCN          CNSN
367400020318     C                   Z-ADD     SNCN          SNSN
367500020318     C                   Z-ADD     RNCN          RNSN
367600020318     C                   Z-ADD     PNCN          PNSN
367700020318     C                   Z-ADD     CACN          CASN
367800020318     C                   Z-ADD     SACN          SASN
367900020318     C                   Z-ADD     RACN          RASN
368000020318     C                   Z-ADD     PACN          PASN
368100020318     C*
368200020318     C* NUMERO TOTALE CLIENTI
368300020318     C     K             OCCUR     ABCC13N
368400020318     C     K             OCCUR     ABCS13N
368500020318     C                   Z-ADD     NTCN          NTSN
368600020318     C                   Z-ADD     ATCN          ATSN
368700020318     C*
368800020318E   4C                   ENDDO
368900020318     C*
369000020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
369100020318DO  4C     1             DO        5             K
369200020318     C     K             OCCUR     ABCCOMND
369300020318     C     K             OCCUR     ABCSTAND
369400020318     C                   Z-ADD     CNCND         CNSND
369500020318     C                   Z-ADD     SNCND         SNSND
369600020318     C                   Z-ADD     RNCND         RNSND
369700020318     C                   Z-ADD     PNCND         PNSND
369800020318     C                   Z-ADD     CACND         CASND
369900020318     C                   Z-ADD     SACND         SASND
370000020318     C                   Z-ADD     RACND         RASND
370100020318     C                   Z-ADD     PACND         PASND
370200020318     C*
370300020318     C* NUMERO TOTALE CLIENTI
370400020318     C     K             OCCUR     ABCC13ND
370500020318     C     K             OCCUR     ABCS13ND
370600020318     C                   Z-ADD     NTCND         NTSND
370700020318     C                   Z-ADD     ATCND         ATSND
370800020318     C*
370900020318E   4C                   ENDDO
371000020220     C*
371100020220     C* ROUTINE DI STAMPA TOTALI
371200020220     C                   EXSR      STAMPA
371300000000     C*
371400000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
371500000000     C                   SETON                                        01
371600000000     C                   MOVEL     '0'           PRIGIR
371700000000     C*
371800000000     C* SE DELTA AREA STAMPA I TOTALI AGENTE NELLA FORMA: CLI, SPE, RIC, DEL
371900000000X   3C                   ELSE
372000020222     C*
372100020318     C* IMPOSTO IL FLAG CHE MI IDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
372200020318     C                   EVAL      DEPSTPD = '1'
372300020318     C*
372400000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
372500020318DO  4C     1             DO        4             Q
372600020318     C     Q             OCCUR     ABCCOM
372700020318     C                   ADD       CNC           CLW                            *NUOVI A, B, C +
372800020318     C                   ADD       SNC           SPW
372900020318     C                   ADD       RNC           RRW
373000020318     C                   ADD       PNC           RPW
373100020318     C                   ADD       CAC           CLW                            *ACQUISITI A, B , C
373200020318     C                   ADD       SAC           SPW
373300020318     C                   ADD       RAC           RRW
373400020318     C                   ADD       PAC           RPW
373500000000     C*
373600020318     C* NUMERO TOTALE CLIENTI
373700020318     C     Q             OCCUR     ABCC13
373800020318     C                   ADD       NTC           CTW
373900020318     C                   ADD       ATC           CTW
374000020318     C
374100020318E   4C                   ENDDO
374200020221     C*
374300000000     C                   MOVEL     DEPAGE        DES07             7            *AGENTE
374400000000     C                   MOVEL     DESAGE        DES12            12            *DESCRIZIONE
374500000000     C*
374600000000     C                   MOVEL     *BLANKS       DES20            20
374700000000     C                   MOVEL     DES07         DES20            20
374800000000     C                   MOVE      DES12         DES20
374900010710     C*
375000010710     C* ROUTINE DI STAMPA TOTALI
375100030926     c                   MOVEL     'A'           WTipStamp2                     *da STAAGE
375200020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
375300020222     C*
375400020220     C* STAMPO I 'D' SOLO SE EFFETTIVAMENTE ESISTENTI
375500020220     C                   IF        FLGSIDE = '1'
375600020221     C*
375700020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
375800020222     C                   EVAL      DEPSTPD = '2'
375900020220     C*
376000020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
376100020318     C     5             OCCUR     ABCCOM
376200050217     C****               ADD       CNC           CLW                            *NUOVI A, B, C +
376300050217     C****               ADD       SNC           SPW
376400050217     C****               ADD       RNC           RRW
376500050217     C****               ADD       PNC           RPW
376600050217     C****               ADD       CAC           CLW                            *ACQUISITI A, B , C
376700050217     C****               ADD       SAC           SPW
376800050217     C****               ADD       RAC           RRW
376900050217     C****               ADD       PAC           RPW
377000050217     C****
377100050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
377200050217     C****  i direzionali ma riportava la somma di tutti
377300050217     C     CNC           ADD       CAC           CLW                            *NUOVI+ACQUISITI (D)
377400050217     C     SNC           ADD       SAC           SPW
377500050217     C     RNC           ADD       RAC           RRW
377600050217     C     PNC           ADD       PAC           RPW
377700050217     C*
377800020220     C* NUMERO TOTALE CLIENTI CLIENTI 'D'
377900020318     C     5             OCCUR     ABCC13
378000050217     C****               ADD       NTC           CTW
378100050217     C****               ADD       ATC           CTW
378200050217     C****
378300050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
378400050217     C****  i direzionali ma riportava la somma di tutti
378500050217     C     NTC           ADD       ATC           CTW
378600020220     C*
378700020220     C* ROUTINE DI STAMPA TOTALI
378800030926     c                   MOVEL     'A'           WTipStamp2                     *da STAAGE
378900020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
379000020318     C                   ENDIF
379100000000     C*
379200000000     C* STAMPO RIGA DI SEPARAZIONE
379300100129     c                   if        parSTAMPA <> 'N'
379400020116     C  N53              WRITE     SE94R2
379500100129     c                   end
379600011001E   3C                   ENDIF
379700000000E   2C                   ENDIF
379800000000E   1C                   ENDIF
379900011001     C*
380000000000     C                   ENDSR
380100030924     C*------------------------------------------------------------------------*
380200030924     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
380300030924     C*------------------------------------------------------------------------*
380400030924     C     STAFIL        BEGSR
380500030924     C*
380600030924     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI FILIALE
380700030924     C                   IF        PARSIN = 'S'
380800030924     C                   SETON                                        53
380900030924     C                   ELSE
381000030924     C                   SETOFF                                       53
381100030924     C                   ENDIF
381200030924     C*
381300000000     C* AZZERA I TOTALI NELLA FORMA CLI, SPE, RIC, DEL
381400000000     C                   CLEAR                   CLW                            *NUMERO CLIENTI
381500000000     C                   CLEAR                   SPW                            *SPEDIZIONI
381600000000     C                   CLEAR                   RRW                            *RICAVO REALE
381700000000     C                   CLEAR                   RPW                            *RICAVO PRESUNTO
381800000000     C                   CLEAR                   CTW                            *13° TOTALE CLEINTI
381900000000     C*
382000000000     C* STAMPA VALORI DELLA FILIALE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
382100000000     C* LA STATISTICA DELTA CON DETTAGLIO CLIENTE/COMMERCIALE/FILIALE (SEMPRE)
382200000000IF  1C     PASFIL        IFEQ      '1'
382300000000IF  2C     PARSTA        IFEQ      'K'
382400000000     C     PARSTA        OREQ      'C'
382500000000     C     PARSTA        OREQ      'F'
382600000000     C***
382700000000     C* DESCRIZIONI INTESTAZIONE
382800000000     C***
382900020116     C                   CLEAR                   SE94IN
383000000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
383100000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
383200000000IF  3C     PARSTA        IFEQ      'K'
383300020221     C     PARSTA        OREQ      'C'
383400000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
383500000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
383600000000X   3C                   ELSE
383700000000     C                   Z-ADD     *ZEROS        INCFIL
383800000000     C                   MOVEL     DRI(2)        INDFIL
383900000000E   3C                   ENDIF
384000000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
384100000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
384200000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
384300000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
384400030924     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
384500030924     C*
384600030924     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
384700030924     C                   MOVEL     DRI(22)       PIDDES                         *RETTANGOLINO
384800030924     C*
384900030924     C* SE DELTA FILIALE O DELTA AREA STAMPA I TOTALI FILIALE NELLA FORMA:
385000030924     C* TOT A, TOT B, TOT C
385100000000IF  3C     PARSTA        IFEQ      'K'
385200000000     C     PARSTA        OREQ      'C'
385300000000     C*
385400000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
385500020221DO  4C     1             DO        4             Q
385600000000     C     Q             OCCUR     ABCFIL
385700000000     C     Q             OCCUR     ABCSTA
385800000000     C                   Z-ADD     CNF           CNS
385900000000     C                   Z-ADD     SNF           SNS
386000000000     C                   Z-ADD     RNF           RNS
386100000000     C                   Z-ADD     PNF           PNS
386200000000     C                   Z-ADD     CAF           CAS
386300000000     C                   Z-ADD     SAF           SAS
386400000000     C                   Z-ADD     RAF           RAS
386500000000     C                   Z-ADD     PAF           PAS
386600000000     C*
386700000000     C* NUMERO TOTALE CLIENTI
386800000000     C     Q             OCCUR     ABCF13
386900000000     C     Q             OCCUR     ABCS13
387000000000     C                   Z-ADD     NTF           NTS
387100000000     C                   Z-ADD     ATF           ATS
387200000000     C*
387300000000E   4C                   ENDDO
387400020220     C*
387500020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
387600020220     C     5             OCCUR     ABCFIL
387700020220     C     5             OCCUR     ABCSTA
387800020220     C                   Z-ADD     CNF           CNS
387900020220     C                   Z-ADD     SNF           SNS
388000020220     C                   Z-ADD     RNF           RNS
388100020220     C                   Z-ADD     PNF           PNS
388200020220     C                   Z-ADD     CAF           CAS
388300020220     C                   Z-ADD     SAF           SAS
388400020220     C                   Z-ADD     RAF           RAS
388500020220     C                   Z-ADD     PAF           PAS
388600020220     C*
388700020220     C* NUMERO TOTALE CLIENTI 'D'
388800020220     C     5             OCCUR     ABCF13
388900020220     C     5             OCCUR     ABCS13
389000020220     C                   Z-ADD     NTF           NTS
389100020220     C                   Z-ADD     ATF           ATS
389200020318     C*
389300020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
389400020318DO  4C     1             DO        5             K
389500020318     C     K             OCCUR     ABCFILN
389600020318     C     K             OCCUR     ABCSTAN
389700020318     C                   Z-ADD     CNFN          CNSN
389800020318     C                   Z-ADD     SNFN          SNSN
389900020318     C                   Z-ADD     RNFN          RNSN
390000020318     C                   Z-ADD     PNFN          PNSN
390100020318     C                   Z-ADD     CAFN          CASN
390200020318     C                   Z-ADD     SAFN          SASN
390300020318     C                   Z-ADD     RAFN          RASN
390400020318     C                   Z-ADD     PAFN          PASN
390500020318     C*
390600020318     C* NUMERO TOTALE CLIENTI
390700020318     C     K             OCCUR     ABCF13N
390800020318     C     K             OCCUR     ABCS13N
390900020318     C                   Z-ADD     NTFN          NTSN
391000020318     C                   Z-ADD     ATFN          ATSN
391100020318     C*
391200020318E   4C                   ENDDO
391300020318     C*
391400020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
391500020318DO  4C     1             DO        5             K
391600020318     C     K             OCCUR     ABCFILND
391700020318     C     K             OCCUR     ABCSTAND
391800020318     C                   Z-ADD     CNFND         CNSND
391900020318     C                   Z-ADD     SNFND         SNSND
392000020318     C                   Z-ADD     RNFND         RNSND
392100020318     C                   Z-ADD     PNFND         PNSND
392200020318     C                   Z-ADD     CAFND         CASND
392300020318     C                   Z-ADD     SAFND         SASND
392400020318     C                   Z-ADD     RAFND         RASND
392500020318     C                   Z-ADD     PAFND         PASND
392600020318     C*
392700020318     C* NUMERO TOTALE CLIENTI
392800020318     C     K             OCCUR     ABCF13ND
392900020318     C     K             OCCUR     ABCS13ND
393000020318     C                   Z-ADD     NTFND         NTSND
393100020318     C                   Z-ADD     ATFND         ATSND
393200020318     C*
393300020318E   4C                   ENDDO
393400020220     C*
393500020220     C* ROUTINE DI STAMPA TOTALI
393600020220     C                   EXSR      STAMPA
393700000000     C*
393800000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
393900000000     C                   SETON                                        01
394000000000     C                   MOVEL     '0'           PRIGIR
394100000000     C*
394200000000     C* SE DELTA DIVISIONE STAMPA I TOTALI FILIALE NELLA FORMA: CLI,SPE,RIC,DEL
394300000000X   3C                   ELSE
394400020221     C*
394500020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
394600020222     C                   EVAL      DEPSTPD = '1'
394700000000     C*
394800000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
394900020318DO  4C     1             DO        4             Q
395000020318     C     Q             OCCUR     ABCFIL
395100050217     C                   ADD       CNF           CLW                            *NUOVI A, B, C +
395200050217     C                   ADD       SNF           SPW
395300050217     C                   ADD       RNF           RRW
395400050217     C                   ADD       PNF           RPW
395500050217     C                   ADD       CAF           CLW                            *ACQUISITI A, B , C
395600050217     C                   ADD       SAF           SPW
395700050217     C                   ADD       RAF           RRW
395800050217     C                   ADD       PAF           RPW
395900050217     C*
396000000000     C* NUMERO TOTALE CLIENTI
396100020318     C     Q             OCCUR     ABCF13
396200050217     C                   ADD       NTF           CTW
396300050217     C                   ADD       ATF           CTW
396400050217     C*
396500020318E   4C                   ENDDO
396600020221     C*
396700000000     C                   MOVEL     DEPFIL        DES03             3            *FILIALE
396800000000     C                   MOVEL     DESFIL        DES16            16            *DESCRIZIONE
396900000000     C*
397000000000     C                   MOVEL     *BLANKS       DES20            20
397100000000     C                   MOVEL     DES03         DES20            20
397200000000     C                   MOVE      DES16         DES20
397300010710     C*
397400010710     C* ROUTINE DI STAMPA TOTALI
397500030926     c                   MOVEL     'F'           WTipStamp2                     *da STAAGE
397600020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
397700020220     C*
397800020220     C* STAMPO I 'D' SOLO SE EFFETTIVAMENTE ESISTENTI
397900020220     C                   IF        FLGSIDF = '1'
398000020221     C*
398100020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
398200020222     C                   EVAL      DEPSTPD = '2'
398300020220     C*
398400020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
398500020318     C     5             OCCUR     ABCFIL
398600050217     C****               ADD       CNF           CLW                            *NUOVI A, B, C +
398700050217     C****               ADD       SNF           SPW
398800050217     C****               ADD       RNF           RRW
398900050217     C****               ADD       PNF           RPW
399000050217     C****               ADD       CAF           CLW                            *ACQUISITI A, B , C
399100050217     C****               ADD       SAF           SPW
399200050217     C****               ADD       RAF           RRW
399300050217     C****               ADD       PAF           RPW
399400050217     C****
399500050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
399600050217     C****  i direzionali ma riportava la somma di tutti
399700050217     C     CNF           ADD       CAF           CLW                            *NUOVI+ACQUISITI (D)
399800050217     C     SNF           ADD       SAF           SPW
399900050217     C     RNF           ADD       RAF           RRW
400000050217     C     PNF           ADD       PAF           RPW
400100050217     C*
400200020220     C* NUMERO TOTALE CLIENTI 'D'
400300020318     C     5             OCCUR     ABCF13
400400050217     C****               ADD       NTF           CTW
400500050217     C****               ADD       ATF           CTW
400600050217     C****
400700050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
400800050217     C****  i direzionali ma riportava la somma di tutti
400900050217     C     NTF           ADD       ATF           CTW
401000050217     C*
401100020220     C* ROUTINE DI STAMPA TOTALI
401200030926     c                   MOVEL     'F'           WTipStamp2                     *da STAAGE
401300020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
401400020318     C                   ENDIF
401500000000     C*
401600000000     C* STAMPO RIGA DI SEPARAZIONE
401700100129     c                   if        parSTAMPA <> 'N'
401800020116     C  N53              WRITE     SE94R2
401900100129     c                   end
402000000000E   3C                   ENDIF
402100000000E   2C                   ENDIF
402200000000E   1C                   ENDIF
402300010710     C*
402400000000     C                   ENDSR
402500000000     C*------------------------------------------------------------------------*
402600000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
402700000000     C*------------------------------------------------------------------------*
402800000000     C     STAARE        BEGSR
402900020215     C*
403000020215     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI AREA
403100020215 FG  C                   IF        PARSIN = 'S'
403200020215     C                   SETON                                        53
403300020215     C                   ELSE
403400020215     C                   SETOFF                                       53
403500020215     C                   ENDIF
403600000000     C*
403700000000     C* STAMPA I VALORI DELL'AREA SE HO MEMORIZZATO QUALCOSA E SE STO ELABORANDO
403800020318     C* LA STATISTICA DELTA CON DETTAGLIO AREA O CON DETTAGLIO DIVISIONE
403900000000IF  1C     PASARE        IFEQ      '1'
404000000000IF  2C     PARSTA        IFEQ      'C'
404100000000     C     PARSTA        OREQ      'F'
404200000000     C***
404300000000     C* DESCRIZIONI INTESTAZIONE
404400000000     C***
404500020116     C                   CLEAR                   SE94IN
404600000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
404700000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
404800000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
404900000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
405000000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
405100000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
405200000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
405300000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
405400950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
405500000000     C*
405600000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
405700000000     C                   MOVEL     DRI(23)       PIDDES                         *RETTANGOLINO
405800000000     C*
405900000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
406000020221DO  3C     1             DO        4             Q
406100000000     C     Q             OCCUR     ABCARE
406200000000     C     Q             OCCUR     ABCSTA
406300000000     C                   Z-ADD     CNA           CNS
406400000000     C                   Z-ADD     SNA           SNS
406500000000     C                   Z-ADD     RNA           RNS
406600000000     C                   Z-ADD     PNA           PNS
406700000000     C                   Z-ADD     CAA           CAS
406800000000     C                   Z-ADD     SAA           SAS
406900000000     C                   Z-ADD     RAA           RAS
407000000000     C                   Z-ADD     PAA           PAS
407100000000     C*
407200000000     C* NUMERO TOTALE CLIENTI
407300000000     C     Q             OCCUR     ABCA13
407400000000     C     Q             OCCUR     ABCS13
407500000000     C                   Z-ADD     NTA           NTS
407600000000     C                   Z-ADD     ATA           ATS
407700000000     C*
407800000000E   3C                   ENDDO
407900020220     C*
408000020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
408100020220     C     5             OCCUR     ABCARE
408200020220     C     5             OCCUR     ABCSTA
408300020220     C                   Z-ADD     CNA           CNS
408400020220     C                   Z-ADD     SNA           SNS
408500020220     C                   Z-ADD     RNA           RNS
408600020220     C                   Z-ADD     PNA           PNS
408700020220     C                   Z-ADD     CAA           CAS
408800020220     C                   Z-ADD     SAA           SAS
408900020220     C                   Z-ADD     RAA           RAS
409000020220     C                   Z-ADD     PAA           PAS
409100020220     C*
409200020220     C* NUMERO TOTALE CLIENTI 'D'
409300020220     C     5             OCCUR     ABCA13
409400020220     C     5             OCCUR     ABCS13
409500020220     C                   Z-ADD     NTA           NTS
409600020220     C                   Z-ADD     ATA           ATS
409700020318     C*
409800020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
409900020318DO  3C     1             DO        5             K
410000020318     C     K             OCCUR     ABCAREN
410100020318     C     K             OCCUR     ABCSTAN
410200020318     C                   Z-ADD     CNAN          CNSN
410300020318     C                   Z-ADD     SNAN          SNSN
410400020318     C                   Z-ADD     RNAN          RNSN
410500020318     C                   Z-ADD     PNAN          PNSN
410600020318     C                   Z-ADD     CAAN          CASN
410700020318     C                   Z-ADD     SAAN          SASN
410800020318     C                   Z-ADD     RAAN          RASN
410900020318     C                   Z-ADD     PAAN          PASN
411000020318     C*
411100020318     C* NUMERO TOTALE CLIENTI
411200020318     C     K             OCCUR     ABCA13N
411300020318     C     K             OCCUR     ABCS13N
411400020318     C                   Z-ADD     NTAN          NTSN
411500020318     C                   Z-ADD     ATAN          ATSN
411600020318     C*
411700020318E   3C                   ENDDO
411800020318     C*
411900020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
412000020318DO  3C     1             DO        5             K
412100020318     C     K             OCCUR     ABCAREND
412200020318     C     K             OCCUR     ABCSTAND
412300020318     C                   Z-ADD     CNAND         CNSND
412400020318     C                   Z-ADD     SNAND         SNSND
412500020318     C                   Z-ADD     RNAND         RNSND
412600020318     C                   Z-ADD     PNAND         PNSND
412700020318     C                   Z-ADD     CAAND         CASND
412800020318     C                   Z-ADD     SAAND         SASND
412900020318     C                   Z-ADD     RAAND         RASND
413000020318     C                   Z-ADD     PAAND         PASND
413100020318     C*
413200020318     C* NUMERO TOTALE CLIENTI
413300020318     C     K             OCCUR     ABCA13ND
413400020318     C     K             OCCUR     ABCS13ND
413500020318     C                   Z-ADD     NTAND         NTSND
413600020318     C                   Z-ADD     ATAND         ATSND
413700020318     C*
413800020318E   3C                   ENDDO
413900020220     C*
414000020220     C* ROUTINE DI STAMPA TOTALI
414100020220     C                   EXSR      STAMPA
414200000000     C*
414300000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
414400000000     C                   SETON                                        01
414500000000     C                   MOVEL     '0'           PRIGIR
414600000000     C*
414700000000E   2C                   ENDIF
414800000000E   1C                   ENDIF
414900000000     C*
415000000000     C                   ENDSR
415100000000     C*------------------------------------------------------------------------*
415200000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
415300000000     C*------------------------------------------------------------------------*
415400000000     C     STADIV        BEGSR
415500020215     C*
415600020215     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI DIVISIONE
415700020215 FG  C                   IF        PARSIN = 'S'
415800020215     C                   SETON                                        53
415900020215     C                   ELSE
416000020215     C                   SETOFF                                       53
416100020215     C                   ENDIF
416200000000     C*
416300020318     C* STAMPA I VALORI DELLA DIVISIONE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
416400020318     C* LA STATISTICA DELTA CON DETTAGLIO DIVISIONE
416500000000IF  1C     PASDIV        IFEQ      '1'
416600000000     C     PARSTA        ANDEQ     'F'
416700000000     C***
416800000000     C* DESCRIZIONI INTESTAZIONE
416900000000     C***
417000020116     C                   CLEAR                   SE94IN
417100000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
417200000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
417300000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
417400000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
417500000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
417600000000     C                   MOVEL     DRI(2)        INDARE                         *DESCRIZIONE AREA
417700000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
417800000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
417900950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
418000000000     C*
418100000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
418200000000     C                   MOVEL     DRI(24)       PIDDES
418300000000     C*
418400000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
418500020221DO  2C     1             DO        4             Q
418600000000     C     Q             OCCUR     ABCDIV
418700000000     C     Q             OCCUR     ABCSTA
418800000000     C                   Z-ADD     CND           CNS
418900000000     C                   Z-ADD     SND           SNS
419000000000     C                   Z-ADD     RND           RNS
419100000000     C                   Z-ADD     PND           PNS
419200000000     C                   Z-ADD     CAD           CAS
419300000000     C                   Z-ADD     SAD           SAS
419400000000     C                   Z-ADD     RAD           RAS
419500000000     C                   Z-ADD     PAD           PAS
419600000000     C*
419700000000     C* NUMERO TOTALE CLIENTI
419800000000     C     Q             OCCUR     ABCD13
419900000000     C     Q             OCCUR     ABCS13
420000000000     C                   Z-ADD     NTD           NTS
420100000000     C                   Z-ADD     ATD           ATS
420200000000     C*
420300000000E   2C                   ENDDO
420400020220     C*
420500020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
420600020220     C     5             OCCUR     ABCDIV
420700020220     C     5             OCCUR     ABCSTA
420800020220     C                   Z-ADD     CND           CNS
420900020220     C                   Z-ADD     SND           SNS
421000030924     C                   Z-ADD     RND           RNS
421100030924     C                   Z-ADD     PND           PNS
421200030924     C                   Z-ADD     CAD           CAS
421300030924     C                   Z-ADD     SAD           SAS
421400020220     C                   Z-ADD     RAD           RAS
421500030924     C                   Z-ADD     PAD           PAS
421600030924     C*
421700030924     C* NUMERO TOTALE CLIENTI
421800030924     C     5             OCCUR     ABCD13
421900030924     C     5             OCCUR     ABCS13
422000030924     C                   Z-ADD     NTD           NTS
422100030924     C                   Z-ADD     ATD           ATS
422200030924     C*
422300030924     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
422400020318DO  2C     1             DO        5             K
422500030924     C     K             OCCUR     ABCDIVN
422600030924     C     K             OCCUR     ABCSTAN
422700030924     C                   Z-ADD     CNDN          CNSN
422800030924     C                   Z-ADD     SNDN          SNSN
422900030924     C                   Z-ADD     RNDN          RNSN
423000030924     C                   Z-ADD     PNDN          PNSN
423100030924     C                   Z-ADD     CADN          CASN
423200020318     C                   Z-ADD     SADN          SASN
423300020318     C                   Z-ADD     RADN          RASN
423400020318     C                   Z-ADD     PADN          PASN
423500020318     C*
423600020318     C* NUMERO TOTALE CLIENTI
423700020318     C     K             OCCUR     ABCD13N
423800020318     C     K             OCCUR     ABCS13N
423900020318     C                   Z-ADD     NTDN          NTSN
424000020318     C                   Z-ADD     ATDN          ATSN
424100020318     C*
424200020318E   2C                   ENDDO
424300020318     C*
424400020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
424500020318DO  2C     1             DO        5             K
424600020318     C     K             OCCUR     ABCDIVND
424700020318     C     K             OCCUR     ABCSTAND
424800020318     C                   Z-ADD     CNDND         CNSND
424900020318     C                   Z-ADD     SNDND         SNSND
425000020318     C                   Z-ADD     RNDND         RNSND
425100020318     C                   Z-ADD     PNDND         PNSND
425200020318     C                   Z-ADD     CADND         CASND
425300020318     C                   Z-ADD     SADND         SASND
425400020318     C                   Z-ADD     RADND         RASND
425500020318     C                   Z-ADD     PADND         PASND
425600020318     C*
425700020318     C* NUMERO TOTALE CLIENTI
425800020318     C     K             OCCUR     ABCD13ND
425900020318     C     K             OCCUR     ABCS13ND
426000020318     C                   Z-ADD     NTDND         NTSND
426100020318     C                   Z-ADD     ATDND         ATSND
426200020318     C*
426300020318E   2C                   ENDDO
426400020220     C*
426500020220     C* ROUTINE DI STAMPA TOTALI
426600020220     C                   EXSR      STAMPA
426700000000     C*
426800000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
426900000000     C                   SETON                                        01
427000000000     C                   MOVEL     '0'           PRIGIR
427100000000     C*
427200000000E   1C                   ENDIF
427300000000     C*
427400000000     C                   ENDSR
427500000000     C*------------------------------------------------------------------------*
427600000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
427700000000     C*------------------------------------------------------------------------*
427800000000     C     STAGEN        BEGSR
427900000000     C*
428000000000     C* STAMPA VALORI GENERALI SOLO SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
428100020318     C* LA STATISTICA DELTA CON DETTAGLIO DIVISIONE
428200000000IF  1C     PASGEN        IFEQ      '1'
428300000000     C     PARSTA        ANDEQ     'F'
428400000000     C***
428500000000     C* DESCRIZIONI INTESTAZIONE
428600000000     C***
428700020116     C                   CLEAR                   SE94IN
428800000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
428900000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
429000000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
429100000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
429200000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
429300000000     C                   MOVEL     DRI(2)        INDARE                         *DESCRIZIONE AREA
429400000000     C                   MOVEL     *BLANKS       INCDIV                         *DIVISIONE
429500000000     C                   MOVEL     DRI(2)        INDDIV                         DESCRIZIONE DIVISION
429600950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
429700000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
429800000000     C                   MOVEL     DRI(25)       PIDDES
429900000000     C*
430000000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
430100020221DO  2C     1             DO        4             Q
430200000000     C     Q             OCCUR     ABCGEN
430300000000     C     Q             OCCUR     ABCSTA
430400000000     C                   Z-ADD     CNG           CNS
430500000000     C                   Z-ADD     SNG           SNS
430600000000     C                   Z-ADD     RNG           RNS
430700000000     C                   Z-ADD     PNG           PNS
430800000000     C                   Z-ADD     CAG           CAS
430900000000     C                   Z-ADD     SAG           SAS
431000000000     C                   Z-ADD     RAG           RAS
431100000000     C                   Z-ADD     PAG           PAS
431200000000     C*
431300000000     C* NUMERO TOTALE CLIENTI
431400000000     C     Q             OCCUR     ABCG13
431500000000     C     Q             OCCUR     ABCS13
431600000000     C                   Z-ADD     NTG           NTS
431700000000     C                   Z-ADD     ATG           ATS
431800000000     C*
431900000000E   2C                   ENDDO
432000020220     C*
432100020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
432200020220     C     5             OCCUR     ABCGEN
432300020220     C     5             OCCUR     ABCSTA
432400020220     C                   Z-ADD     CNG           CNS
432500020220     C                   Z-ADD     SNG           SNS
432600020220     C                   Z-ADD     RNG           RNS
432700020220     C                   Z-ADD     PNG           PNS
432800020220     C                   Z-ADD     CAG           CAS
432900020220     C                   Z-ADD     SAG           SAS
433000020220     C                   Z-ADD     RAG           RAS
433100020220     C                   Z-ADD     PAG           PAS
433200020220     C*
433300020220     C* NUMERO TOTALE CLIENTI 'D'
433400020220     C     5             OCCUR     ABCG13
433500020220     C     5             OCCUR     ABCS13
433600020220     C                   Z-ADD     NTG           NTS
433700020220     C                   Z-ADD     ATG           ATS
433800020318     C*
433900020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
434000020318DO  2C     1             DO        5             K
434100020318     C     K             OCCUR     ABCGENN
434200020318     C     K             OCCUR     ABCSTAN
434300020318     C                   Z-ADD     CNGN          CNSN
434400020318     C                   Z-ADD     SNGN          SNSN
434500020318     C                   Z-ADD     RNGN          RNSN
434600020318     C                   Z-ADD     PNGN          PNSN
434700020318     C                   Z-ADD     CAGN          CASN
434800020318     C                   Z-ADD     SAGN          SASN
434900020318     C                   Z-ADD     RAGN          RASN
435000020318     C                   Z-ADD     PAGN          PASN
435100020318     C*
435200020318     C* NUMERO TOTALE CLIENTI
435300020318     C     K             OCCUR     ABCG13N
435400020318     C     K             OCCUR     ABCS13N
435500020318     C                   Z-ADD     NTGN          NTSN
435600020318     C                   Z-ADD     ATGN          ATSN
435700020318     C*
435800020318E   2C                   ENDDO
435900020318     C*
436000020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
436100020318DO  2C     1             DO        5             K
436200020318     C     K             OCCUR     ABCGENND
436300020318     C     K             OCCUR     ABCSTAND
436400020318     C                   Z-ADD     CNGND         CNSND
436500020318     C                   Z-ADD     SNGND         SNSND
436600020318     C                   Z-ADD     RNGND         RNSND
436700020318     C                   Z-ADD     PNGND         PNSND
436800020318     C                   Z-ADD     CAGND         CASND
436900020318     C                   Z-ADD     SAGND         SASND
437000020318     C                   Z-ADD     RAGND         RASND
437100020318     C                   Z-ADD     PAGND         PASND
437200020318     C*
437300020318     C* NUMERO TOTALE CLIENTI
437400020318     C     K             OCCUR     ABCG13ND
437500020318     C     K             OCCUR     ABCS13ND
437600020318     C                   Z-ADD     NTGND         NTSND
437700020318     C                   Z-ADD     ATGND         ATSND
437800020318     C*
437900020318E   2C                   ENDDO
438000020220     C*
438100020220     C* ROUTINE DI STAMPA TOTALI
438200020220     C                   EXSR      STAMPA
438300000000     C*
438400000000E   1C                   ENDIF
438500000000     C*
438600000000     C                   ENDSR
438700000000     C*------------------------------------------------------------------------*
438800000000     C*?STAMPA - STAMPA I TOTALI NELLA FORMA: TOT "A" / TOT "B" / TOT "C"
438900000000     C*------------------------------------------------------------------------*
439000000000     C     STAMPA        BEGSR
439100020221     C*--------------------
439200020221     C* I N T E S T A Z I O N E
439300020221     C*--------------------
439400020221     C                   EXSR      STAINT
439500000000     C*
439600000000     C* PULISCO I TOTALI A+B+C
439700000000     C                   CLEAR                   CNW
439800000000     C                   CLEAR                   SNW
439900000000     C                   CLEAR                   RNW
440000000000     C                   CLEAR                   PNW
440100000000     C                   CLEAR                   CAW
440200000000     C                   CLEAR                   SAW
440300000000     C                   CLEAR                   RAW
440400000000     C                   CLEAR                   PAW
440500000000     C*
440600020318     C                   Z-ADD     *ZEROS        CNW13C           11 0          *PERIODO CORRENTE
440700000000     C                   Z-ADD     *ZEROS        CAW13C           11 0
440800951227     C                   Z-ADD     *ZEROS        CN213C           11 0          *PERIODO PRECEDENTE
440900951227     C                   Z-ADD     *ZEROS        CA213C           11 0
441000000000     C*
441100980521DO  1C     1             DO        4             Q
441200000000     C     Q             OCCUR     ABCSTA
441300000000     C***
441400020221     C* TOTALI "A" + "B" + "C"  DIVISI IN NUOVI E ACQUISITI
441500000000     C***
441600000000     C                   ADD       CNS           CNW
441700000000     C                   ADD       SNS           SNW
441800000000     C                   ADD       RNS           RNW
441900000000     C                   ADD       PNS           PNW
442000000000     C                   ADD       CAS           CAW
442100000000     C                   ADD       SAS           SAW
442200000000     C                   ADD       RAS           RAW
442300000000     C                   ADD       PAS           PAW
442400951227     C*
442500000000     C     Q             OCCUR     ABCS13
442600951227     C* CORRENTE
442700000000     C                   ADD       NTS(1)        CNW13C
442800000000     C                   ADD       ATS(1)        CAW13C
442900951227     C* PRECEDENTE
443000951227     C                   ADD       NTS(2)        CN213C
443100011105     C                   ADD       ATS(2)        CA213C
443200000000     C*
443300000000E   1C                   ENDDO
443400020221     C*
443500000000     C***
443600000000     C* CLIENTI  N U O V I
443700000000     C***
443800980521DO  1C     1             DO        4             Q
443900000000     C     Q             OCCUR     ABCSTA
444000000000     C*
444100000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
444200000000IF  2C     *IN01         IFEQ      *ON
444300020215     C                   EXSR      STAINT
444400000000E   2C                   ENDIF
444500020222     C*
444600020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
444700020222     C                   EVAL      DEPSTPD = '0'
444800000000     C*
444900000000     C                   XFOOT     CNS           COMCLI           16 0          *NUOVI DI CATEGORIA
445000000000IF  2C     COMCLI        IFGT      *ZEROS
445100000000     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
445200000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
445300020318     C                   MOVEL     '0'           FLGSTPNTW
445400000000     C                   Z-ADD     CNS           $CL                            NUMERO CLIENTI
445500000000     C                   Z-ADD     SNS           $SP                            SPEDIZIONI
445600000000     C                   Z-ADD     RNS           $RR                            RICAVI REALI
445700000000     C                   Z-ADD     PNS           $RP                            RICAVI PRESUNTI
445800000000     C*
445900000000     C     Q             OCCUR     ABCS13
446000951227     C                   Z-ADD     NTS(1)        $CL13C           11 0          N° TOT CLIENTI CORR
446100951227     C                   Z-ADD     NTS(2)        $C213C           11 0          N° TOT CLIENTI PREC
446200000000     C*
446300000000     C                   EXSR      STMDAT
446400000000E   2C                   ENDIF
446500000000     C*
446600000000E   1C                   ENDDO
446700000000     C*
446800020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
446900020318     C                   IF        PARSTA = 'K'
447000000000     C* T O T A L I   N U O V I
447100000000     C                   Z-ADD     CNW           $CL                            NUMERO CLIENTI
447200000000     C                   Z-ADD     SNW           $SP                            SPEDIZIONI
447300000000     C                   Z-ADD     RNW           $RR                            RICAVI REALI
447400000000     C                   Z-ADD     PNW           $RP                            RICAVI PRESUNTI
447500951227     C                   Z-ADD     CNW13C        $CL13C                         N° TOT CLIENTI CORR
447600951227     C                   Z-ADD     CN213C        $C213C                         N° TOT CLIENTI PREC
447700020222     C*
447800020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
447900020222IF  2C     *IN01         IFEQ      *ON
448000020222     C                   EXSR      STAINT
448100020222E   2C                   ENDIF
448200020222     C*
448300020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
448400020222     C                   EVAL      DEPSTPD = '1'
448500000000     C*
448600020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
448700000000IF  1C     COMCLI        IFGT      *ZEROS
448800000000     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
448900020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
449000020221     C                   EXSR      STMDAT
449100100129     c                   if        parSTAMPA <> 'N'
449200020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
449300100129     c                   end
449400020221E   1C                   ENDIF
449500020318     C*
449600020318     C                   ELSE
449700020318     C* T O T A L I   N U O V I
449800020318     C                   Z-ADD     CNW           $CL                            NUMERO CLIENTI
449900020318     C                   Z-ADD     SNW           $SP                            SPEDIZIONI
450000020318     C                   Z-ADD     RNW           $RR                            RICAVI REALI
450100020318     C                   Z-ADD     PNW           $RP                            RICAVI PRESUNTI
450200020318     C                   Z-ADD     CNW13C        $CL13C                         N° TOT CLIENTI CORR
450300020318     C                   Z-ADD     CN213C        $C213C                         N° TOT CLIENTI PREC
450400020318     C*
450500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
450600020318IF  2C     *IN01         IFEQ      *ON
450700020318     C                   EXSR      STAINT
450800020318E   2C                   ENDIF
450900020318     C*
451000020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
451100020318     C                   EVAL      DEPSTPD = '1'
451200020318     C*
451300020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
451400020318IF  1C     COMCLI        IFGT      *ZEROS
451500020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
451600020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
451700020318     C                   EXSR      STMDAT
451800100129     c                   if        parSTAMPA <> 'N'
451900020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
452000100129     c                   end
452100020318E   1C                   ENDIF
452200020318     C***
452300020404     C* T O T A L I   N U O V I   X   N E T W O R K
452400020318     C***
452500020404     C*
452600020404     C* SOLO SE RICHIESTO NEL LANCIO
452700020404     C                   IF        PARNTW = 'S'
452800020404     C*
452900020319     C                   MOVEL     '0'           FLGSTPRI          1
453000020319     C*
453100020319DO  1C     2             DO        5             K
453200020318     C     K             OCCUR     ABCSTAN
453300020318     C*
453400020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
453500020318IF  2C     *IN01         IFEQ      *ON
453600020318     C                   EXSR      STAINT
453700020318E   2C                   ENDIF
453800020318     C*
453900020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
454000020318     C                   EVAL      DEPSTPD = '1'
454100020318     C*
454200020318     C                   XFOOT     CNSN          COMCLI           16 0          *NUOVI DI CATEGORIA
454300020318IF  2C     COMCLI        IFGT      *ZEROS
454400020319     C                   MOVEL     '1'           FLGSTPRI
454500020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
454600020318     C                   Z-ADD     K             $Q                             NETWORK
454700020318     C                   MOVEL     '1'           FLGSTPNTW
454800020318     C                   Z-ADD     CNSN          $CL                            NUMERO CLIENTI
454900020318     C                   Z-ADD     SNSN          $SP                            SPEDIZIONI
455000020318     C                   Z-ADD     RNSN          $RR                            RICAVI REALI
455100020318     C                   Z-ADD     PNSN          $RP                            RICAVI PRESUNTI
455200020318     C*
455300020318     C     K             OCCUR     ABCS13N
455400020318     C                   Z-ADD     NTSN(1)       $CL13C           11 0          N° TOT CLIENTI CORR
455500020318     C                   Z-ADD     NTSN(2)       $C213C           11 0          N° TOT CLIENTI PREC
455600020318     C*
455700020318     C                   EXSR      STMDAT
455800020318E   2C                   ENDIF
455900020318     C*
456000020318E   1C                   ENDDO
456100020319     C                   IF        FLGSTPRI = '1'
456200100129     c                   if        parSTAMPA <> 'N'
456300020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
456400100129     c                   end
456500020318     C                   ENDIF
456600020319     C                   ENDIF
456700020404     C*
456800020404     C                   ENDIF
456900020221     C*
457000020221     C* PULISCO I TOTALI D
457100020221     C                   CLEAR                   CNWD
457200020221     C                   CLEAR                   SNWD
457300020221     C                   CLEAR                   RNWD
457400020221     C                   CLEAR                   PNWD
457500020221     C                   CLEAR                   CAWD
457600020221     C                   CLEAR                   SAWD
457700020221     C                   CLEAR                   RAWD
457800020221     C                   CLEAR                   PAWD
457900020221     C*
458000020318     C                   Z-ADD     *ZEROS        CNW13CD          11 0          *PERIODO CORRENTE
458100020221     C                   Z-ADD     *ZEROS        CAW13CD          11 0
458200020221     C                   Z-ADD     *ZEROS        CN213CD          11 0          *PERIODO PRECEDENTE
458300020221     C                   Z-ADD     *ZEROS        CA213CD          11 0
458400020221     C*
458500020221     C     5             OCCUR     ABCSTA
458600020221     C***
458700020221     C* TOTALI "D" DIVISI IN NUOVI E ACQUISITI
458800020221     C***
458900020221     C                   ADD       CNS           CNWD
459000020221     C                   ADD       SNS           SNWD
459100020221     C                   ADD       RNS           RNWD
459200020221     C                   ADD       PNS           PNWD
459300020221     C                   ADD       CAS           CAWD
459400020221     C                   ADD       SAS           SAWD
459500020221     C                   ADD       RAS           RAWD
459600020221     C                   ADD       PAS           PAWD
459700020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
459800020221     C                   ADD       CNW           CNWD
459900020221     C                   ADD       SNW           SNWD
460000020221     C                   ADD       RNW           RNWD
460100020221     C                   ADD       PNW           PNWD
460200020221     C                   ADD       CAW           CAWD
460300020221     C                   ADD       SAW           SAWD
460400020221     C                   ADD       RAW           RAWD
460500020221     C                   ADD       PAW           PAWD
460600020221     C*
460700020221     C     5             OCCUR     ABCS13
460800020221     C* CORRENTE
460900020221     C                   ADD       NTS(1)        CNW13CD
461000020221     C                   ADD       ATS(1)        CAW13CD
461100020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
461200020221     C                   ADD       CNW13C        CNW13CD
461300020221     C                   ADD       CAW13C        CAW13CD
461400020221     C* PRECEDENTE
461500020221     C                   ADD       NTS(2)        CN213CD
461600020221     C                   ADD       ATS(2)        CA213CD
461700020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
461800020221     C                   ADD       CN213C        CN213CD
461900020221     C                   ADD       CA213C        CA213CD
462000020221     C***
462100020221     C* CLIENTI  N U O V I  'D'
462200020221     C***
462300020221     C     5             OCCUR     ABCSTA
462400020221     C*
462500020221     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
462600020221IF  2C     *IN01         IFEQ      *ON
462700020221     C                   EXSR      STAINT
462800020221E   2C                   ENDIF
462900020222     C*
463000020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
463100020222     C                   EVAL      DEPSTPD = '0'
463200020221     C*
463300020221     C                   XFOOT     CNS           COMCLI           16 0          *NUOVI DI CATEGORIA
463400020221IF  2C     COMCLI        IFGT      *ZEROS
463500020221     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
463600020221     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
463700020318     C                   MOVEL     '0'           FLGSTPNTW
463800020221     C                   Z-ADD     CNS           $CL                            NUMERO CLIENTI
463900020221     C                   Z-ADD     SNS           $SP                            SPEDIZIONI
464000020221     C                   Z-ADD     RNS           $RR                            RICAVI REALI
464100020221     C                   Z-ADD     PNS           $RP                            RICAVI PRESUNTI
464200020221     C*
464300020221     C     5             OCCUR     ABCS13
464400020221     C                   Z-ADD     NTS(1)        $CL13C           11 0          N° TOT CLIENTI CORR
464500020221     C                   Z-ADD     NTS(2)        $C213C           11 0          N° TOT CLIENTI PREC
464600020221     C*
464700020221     C                   EXSR      STMDAT
464800020221E   2C                   ENDIF
464900020318     C*
465000020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
465100020318     C                   IF        PARSTA = 'K'
465200020221     C* T O T A L I   N U O V I   'D'
465300020221     C                   Z-ADD     CNWD          $CL                            NUMERO CLIENTI
465400020221     C                   Z-ADD     SNWD          $SP                            SPEDIZIONI
465500020221     C                   Z-ADD     RNWD          $RR                            RICAVI REALI
465600020221     C                   Z-ADD     PNWD          $RP                            RICAVI PRESUNTI
465700020221     C                   Z-ADD     CNW13CD       $CL13C                         N° TOT CLIENTI CORR
465800020221     C                   Z-ADD     CN213CD       $C213C                         N° TOT CLIENTI PREC
465900020222     C*
466000020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
466100020222IF  2C     *IN01         IFEQ      *ON
466200020222     C                   EXSR      STAINT
466300020222E   2C                   ENDIF
466400030311     C*
466500030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
466600070821     C*****              EVAL      DEPSTPD = '2'
466700070821     C                   EVAL      DEPSTPD = '3'
466800020221     C*
466900020221IF  1C     COMCLI        IFGT      *ZEROS
467000020221     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
467100020221     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
467200020221     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
467300020221     C                   EXSR      STMDAT
467400100129     c                   if        parSTAMPA <> 'N'
467500020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
467600100129     c                   end
467700020221E   1C                   ENDIF
467800020318     C*
467900020318     C                   ELSE
468000020318     C* T O T A L I   N U O V I   'D'
468100020318     C                   Z-ADD     CNWD          $CL                            NUMERO CLIENTI
468200020318     C                   Z-ADD     SNWD          $SP                            SPEDIZIONI
468300020318     C                   Z-ADD     RNWD          $RR                            RICAVI REALI
468400020318     C                   Z-ADD     PNWD          $RP                            RICAVI PRESUNTI
468500020318     C                   Z-ADD     CNW13CD       $CL13C                         N° TOT CLIENTI CORR
468600020318     C                   Z-ADD     CN213CD       $C213C                         N° TOT CLIENTI PREC
468700020318     C*
468800020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
468900020318IF  2C     *IN01         IFEQ      *ON
469000020318     C                   EXSR      STAINT
469100020318E   2C                   ENDIF
469200030311     C*
469300030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
469400070821     C******             EVAL      DEPSTPD = '2'
469500070821     C                   EVAL      DEPSTPD = '3'
469600020318     C*
469700020318IF  1C     COMCLI        IFGT      *ZEROS
469800020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
469900020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
470000020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
470100020318     C                   EXSR      STMDAT
470200100129     c                   if        parSTAMPA <> 'N'
470300020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
470400100129     c                   end
470500020318E   1C                   ENDIF
470600020318     C***
470700020318     C* T O T A L I   N U O V I   "D"   X   N E T W O R K
470800020318     C***
470900020404     C*
471000020404     C* SOLO SE RICHIESTO NEL LANCIO
471100020404     C                   IF        PARNTW = 'S'
471200020404     C*
471300020319     C                   MOVEL     '0'           FLGSTPRI          1
471400020319DO  1C     2             DO        5             K
471500020318     C     K             OCCUR     ABCSTAND
471600020318     C*
471700020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
471800020318IF  2C     *IN01         IFEQ      *ON
471900020318     C                   EXSR      STAINT
472000020318E   2C                   ENDIF
472100020318     C*
472200020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
472300020318     C                   EVAL      DEPSTPD = '2'
472400020318     C*
472500020318     C                   XFOOT     CNSND         COMCLI           16 0          *NUOVI DI CATEGORIA
472600020318IF  2C     COMCLI        IFGT      *ZEROS
472700020319     C                   MOVEL     '1'           FLGSTPRI
472800020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
472900020318     C                   Z-ADD     K             $Q                             NETWORK
473000020318     C                   MOVEL     '1'           FLGSTPNTW
473100020318     C                   Z-ADD     CNSND         $CL                            NUMERO CLIENTI
473200020318     C                   Z-ADD     SNSND         $SP                            SPEDIZIONI
473300020318     C                   Z-ADD     RNSND         $RR                            RICAVI REALI
473400020318     C                   Z-ADD     PNSND         $RP                            RICAVI PRESUNTI
473500020318     C*
473600020318     C     K             OCCUR     ABCS13ND
473700020318     C                   Z-ADD     NTSND(1)      $CL13C           11 0          N° TOT CLIENTI CORR
473800020318     C                   Z-ADD     NTSND(2)      $C213C           11 0          N° TOT CLIENTI PREC
473900020318     C*
474000020318     C                   EXSR      STMDAT
474100020318E   2C                   ENDIF
474200020318     C*
474300020318E   1C                   ENDDO
474400020319     C                   IF        FLGSTPRI = '1'
474500100129     c                   if        parSTAMPA <> 'N'
474600020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
474700100129     c                   end
474800020318     C                   ENDIF
474900020319     C                   ENDIF
475000020221     C*
475100020404     C                   ENDIF
475200020404     C*
475300020221     C***
475400020318     C* CLIENTI  A C Q U I S I T I
475500000000     C***
475600020221     C*
475700980521DO  1C     1             DO        4             Q
475800000000     C     Q             OCCUR     ABCSTA
475900000000     C*
476000000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
476100000000IF  2C     *IN01         IFEQ      *ON
476200020215     C                   EXSR      STAINT
476300000000E   2C                   ENDIF
476400020222     C*
476500020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
476600020222     C                   EVAL      DEPSTPD = '0'
476700000000     C*
476800020116     C                   XFOOT     CAS           COMCLI                         *NUOVI DI CATEGORIA
476900000000IF  2C     COMCLI        IFGT      *ZEROS
477000000000     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
477100000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
477200020318     C                   MOVEL     '0'           FLGSTPNTW
477300000000     C                   Z-ADD     CAS           $CL                            NUMERO CLIENTI
477400000000     C                   Z-ADD     SAS           $SP                            SPEDIZIONI
477500000000     C                   Z-ADD     RAS           $RR                            RICAVI REALI
477600000000     C                   Z-ADD     PAS           $RP                            RICAVI PRESUNTI
477700000000     C*
477800000000     C     Q             OCCUR     ABCS13
477900020116     C                   Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
478000020116     C                   Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
478100000000     C                   EXSR      STMDAT
478200000000E   2C                   ENDIF
478300000000     C*
478400000000E   1C                   ENDDO
478500020318     C*
478600020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
478700020318     C                   IF        PARSTA = 'K'
478800000000     C* T O T A L I   A C Q U I S I T I
478900000000     C                   Z-ADD     CAW           $CL                            NUMERO CLIENTI
479000000000     C                   Z-ADD     SAW           $SP                            SPEDIZIONI
479100000000     C                   Z-ADD     RAW           $RR                            RICAVI REALI
479200000000     C                   Z-ADD     PAW           $RP                            RICAVI PRESUNTI
479300951227     C                   Z-ADD     CAW13C        $CL13C                         N° TOT CLIENTI CORR
479400951227     C                   Z-ADD     CA213C        $C213C                         N° TOT CLIENTI PREC
479500020222     C*
479600020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
479700020222IF  2C     *IN01         IFEQ      *ON
479800020222     C                   EXSR      STAINT
479900020222E   2C                   ENDIF
480000020222     C*
480100020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
480200020222     C                   EVAL      DEPSTPD = '1'
480300000000     C*
480400020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
480500000000IF  1C     COMCLI        IFGT      *ZEROS
480600000000     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
480700000000     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
480800000000     C                   EXSR      STMDAT
480900100129     c                   if        parSTAMPA <> 'N'
481000020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
481100100129     c                   end
481200020221E   1C                   ENDIF
481300020318     C*
481400020318     C                   ELSE
481500020318     C* T O T A L I   A C Q U I S I T I
481600020318     C                   Z-ADD     CAW           $CL                            NUMERO CLIENTI
481700020318     C                   Z-ADD     SAW           $SP                            SPEDIZIONI
481800020318     C                   Z-ADD     RAW           $RR                            RICAVI REALI
481900020318     C                   Z-ADD     PAW           $RP                            RICAVI PRESUNTI
482000020318     C                   Z-ADD     CAW13C        $CL13C                         N° TOT CLIENTI CORR
482100020318     C                   Z-ADD     CA213C        $C213C                         N° TOT CLIENTI PREC
482200020318     C*
482300020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
482400020318IF  2C     *IN01         IFEQ      *ON
482500020318     C                   EXSR      STAINT
482600020318E   2C                   ENDIF
482700020318     C*
482800020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
482900020318     C                   EVAL      DEPSTPD = '1'
483000020318     C*
483100020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
483200020318IF  1C     COMCLI        IFGT      *ZEROS
483300050308IF  2C     $c213c        orGT      *ZEROS
483400020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
483500020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
483600020318     C                   EXSR      STMDAT
483700100129     c                   if        parSTAMPA <> 'N'
483800020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
483900100129     c                   end
484000020318E   1C                   ENDIF
484100020318     C***
484200020318     C* T O T A L I   A C Q I S I T I   X   N E T W O R K
484300020318     C***
484400020404     C*
484500020404     C* SOLO SE RICHIESTO NEL LANCIO
484600020404     C                   IF        PARNTW = 'S'
484700020404     C*
484800020319     C                   MOVEL     '0'           FLGSTPRI          1
484900020319     C*
485000020319DO  1C     2             DO        5             K
485100020318     C     K             OCCUR     ABCSTAN
485200020318     C*
485300020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
485400020318IF  2C     *IN01         IFEQ      *ON
485500020318     C                   EXSR      STAINT
485600020318E   2C                   ENDIF
485700020318     C*
485800020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
485900020318     C                   EVAL      DEPSTPD = '1'
486000050309     C     K             OCCUR     ABCS13N
486100050309     C                   Z-ADD     ATSN(1)       $CL13C                         N° TOT CLIENTI CORR
486200050309     C                   Z-ADD     ATSN(2)       $C213C                         N° TOT CLIENTI PREC
486300020318     C*
486400020318     C                   XFOOT     CASN          COMCLI                         *NUOVI DI CATEGORIA
486500020318IF  2C     COMCLI        IFGT      *ZEROS
486600050309IF  2C     $c213c        orGT      *ZEROS
486700020319     C                   MOVEL     '1'           FLGSTPRI
486800020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
486900020318     C                   Z-ADD     K             $Q                             NETWORK
487000020318     C                   MOVEL     '1'           FLGSTPNTW
487100020318     C                   Z-ADD     CASN          $CL                            NUMERO CLIENTI
487200020318     C                   Z-ADD     SASN          $SP                            SPEDIZIONI
487300020318     C                   Z-ADD     RASN          $RR                            RICAVI REALI
487400020318     C                   Z-ADD     PASN          $RP                            RICAVI PRESUNTI
487500050309     C* ???????
487600050309     C**** K             OCCUR     ABCS13N
487700050309     C*********          Z-ADD     ATSN(1)       $CL13C                         N° TOT CLIENTI CORR
487800050309     C*********          Z-ADD     ATSN(2)       $C213C                         N° TOT CLIENTI PREC
487900020318     C                   EXSR      STMDAT
488000020318E   2C                   ENDIF
488100020318     C*
488200020318E   1C                   ENDDO
488300020319     C                   IF        FLGSTPRI = '1'
488400100129     c                   if        parSTAMPA <> 'N'
488500020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
488600100129     c                   end
488700020318     C                   ENDIF
488800020319     C                   ENDIF
488900020404     C*
489000020404     C                   ENDIF
489100020221     C***
489200020221     C* CLIENTI  A C Q U I S I T I  'D'
489300020221     C***
489400020221     C*
489500020221     C     5             OCCUR     ABCSTA
489600020221     C*
489700020221     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
489800020221IF  2C     *IN01         IFEQ      *ON
489900020221     C                   EXSR      STAINT
490000020221E   2C                   ENDIF
490100020222     C*
490200020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
490300020222     C                   EVAL      DEPSTPD = '0'
490400050309     C     5             OCCUR     ABCS13
490500050309     C                   Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
490600050309     C                   Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
490700020221     C*
490800020221     C                   XFOOT     CAS           COMCLI                         *NUOVI DI CATEGORIA
490900020221IF  2C     COMCLI        IFGT      *ZEROS
491000050309IF  2C     $c213c        orGT      *ZEROS
491100020221     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
491200020221     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
491300020318     C                   MOVEL     '0'           FLGSTPNTW
491400020221     C                   Z-ADD     CAS           $CL                            NUMERO CLIENTI
491500020221     C                   Z-ADD     SAS           $SP                            SPEDIZIONI
491600020221     C                   Z-ADD     RAS           $RR                            RICAVI REALI
491700020221     C                   Z-ADD     PAS           $RP                            RICAVI PRESUNTI
491800050309     C* ???????
491900050309     C**** 5             OCCUR     ABCS13
492000050309     C**********         Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
492100050309     C**********         Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
492200020221     C                   EXSR      STMDAT
492300020221E   2C                   ENDIF
492400020318     C*
492500020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
492600020318     C                   IF        PARSTA = 'K'
492700020221     C* T O T A L I   A C Q U I S I T I  'D'
492800020221     C                   Z-ADD     CAWD          $CL                            NUMERO CLIENTI
492900020221     C                   Z-ADD     SAWD          $SP                            SPEDIZIONI
493000020221     C                   Z-ADD     RAWD          $RR                            RICAVI REALI
493100020221     C                   Z-ADD     PAWD          $RP                            RICAVI PRESUNTI
493200020221     C                   Z-ADD     CAW13CD       $CL13C                         N° TOT CLIENTI CORR
493300020221     C                   Z-ADD     CA213CD       $C213C                         N° TOT CLIENTI PREC
493400020222     C*
493500020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
493600020222IF  2C     *IN01         IFEQ      *ON
493700020222     C                   EXSR      STAINT
493800020222E   2C                   ENDIF
493900020222     C*
494000020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
494100030520     C                   EVAL      DEPSTPD = '3'
494200020221     C*
494300020221IF  1C     COMCLI        IFGT      *ZEROS
494400020221     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
494500020221     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
494600020221     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
494700020221     C                   EXSR      STMDAT
494800100129     c                   if        parSTAMPA <> 'N'
494900020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
495000100129     c                   end
495100020221E   1C                   ENDIF
495200020318     C*
495300020318     C                   ELSE
495400020318     C* T O T A L I   A C Q U I S I T I  'D'
495500020318     C                   Z-ADD     CAWD          $CL                            NUMERO CLIENTI
495600020318     C                   Z-ADD     SAWD          $SP                            SPEDIZIONI
495700020318     C                   Z-ADD     RAWD          $RR                            RICAVI REALI
495800020318     C                   Z-ADD     PAWD          $RP                            RICAVI PRESUNTI
495900020318     C                   Z-ADD     CAW13CD       $CL13C                         N° TOT CLIENTI CORR
496000020318     C                   Z-ADD     CA213CD       $C213C                         N° TOT CLIENTI PREC
496100020318     C*
496200020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
496300020318IF  2C     *IN01         IFEQ      *ON
496400020318     C                   EXSR      STAINT
496500020318E   2C                   ENDIF
496600020318     C*
496700020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
496800030520     C                   EVAL      DEPSTPD = '3'
496900020318     C*
497000050308     C                   xfoot     $cl           comcli                         *NUOVI DI CATEGORIA
497100020318IF  1C     COMCLI        IFGT      *ZEROS
497200050308IF  2C     $c213c        orGT      *ZEROS
497300050308     C********           XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
497400020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
497500020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
497600020318     C                   EXSR      STMDAT
497700100129     c                   if        parSTAMPA <> 'N'
497800020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
497900100129     c                   end
498000020318E   1C                   ENDIF
498100020318     C***
498200050308     C* T O T A L I   A C Q U I S I T I   X   N E T W O R K  "D"
498300020318     C***
498400020404     C*
498500020404     C* SOLO SE RICHIESTO NEL LANCIO
498600020404     C                   IF        PARNTW = 'S'
498700020404     C*
498800020319     C                   MOVEL     '0'           FLGSTPRI          1
498900020319     C*
499000020319DO  1C     2             DO        5             K
499100020318     C     K             OCCUR     ABCSTAND
499200020318     C*
499300020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
499400020318IF  2C     *IN01         IFEQ      *ON
499500020318     C                   EXSR      STAINT
499600020318E   2C                   ENDIF
499700020318     C*
499800020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
499900020318     C                   EVAL      DEPSTPD = '2'
500000020318     C*
500100050309     C     K             OCCUR     ABCS13ND
500200050309     C                   Z-ADD     ATSND(1)      $CL13C                         N° TOT CLIENTI CORR
500300050309     C                   Z-ADD     ATSND(2)      $C213C                         N° TOT CLIENTI PREC
500400050308     C*
500500020318     C                   XFOOT     CASND         COMCLI                         *NUOVI DI CATEGORIA
500600020318IF  2C     COMCLI        IFGT      *ZEROS
500700050309IF  2C     $c213c        orGT      *ZEROS
500800020319     C                   MOVEL     '1'           FLGSTPRI
500900020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
501000020318     C                   Z-ADD     K             $Q                             NETWORK
501100020318     C                   MOVEL     '1'           FLGSTPNTW
501200020318     C                   Z-ADD     CASND         $CL                            NUMERO CLIENTI
501300020318     C                   Z-ADD     SASND         $SP                            SPEDIZIONI
501400020318     C                   Z-ADD     RASND         $RR                            RICAVI REALI
501500020318     C                   Z-ADD     PASND         $RP                            RICAVI PRESUNTI
501600050309     C* ???????
501700050309     C**** K             OCCUR     ABCS13ND
501800050309     C********           Z-ADD     ATSND(1)      $CL13C                         N° TOT CLIENTI CORR
501900050309     C********           Z-ADD     ATSND(2)      $C213C                         N° TOT CLIENTI PREC
502000020318     C                   EXSR      STMDAT
502100020318E   2C                   ENDIF
502200020318     C*
502300020318E   1C                   ENDDO
502400020319     C                   IF        FLGSTPRI = '1'
502500100129     c                   if        parSTAMPA <> 'N'
502600020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
502700100129     c                   end
502800020318     C                   ENDIF
502900020319     C                   ENDIF
503000020404     C*
503100020404     C                   ENDIF
503200020221     C*
503300020319     C* PRIMA DEI TOTALI STAMPO SEMPRE E CMQ 1 RIGA DOPPIA X EVIDENZIARE L'INIZIO DEI TOTALI
503400100129     c                   if        parSTAMPA <> 'N'
503500020319     C  N53              WRITE     SE94R3                                       *RIGA SEPARAZIONE DO
503600100129     c                   end
503700020319     C*
503800020318     C***
503900020318     C* CLIENTI  T O T A L I
504000000000     C***
504100020221     C*
504200980521DO  1C     1             DO        4             Q
504300000000     C     Q             OCCUR     ABCSTA
504400000000     C*
504500000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
504600000000IF  2C     *IN01         IFEQ      *ON
504700020215     C                   EXSR      STAINT
504800000000E   2C                   ENDIF
504900020222     C*
505000020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
505100020222     C                   EVAL      DEPSTPD = '0'
505200000000     C*
505300000000     C     CNS           ADD       CAS           $CL                            NUMERO CLIENTI
505400000000     C     SNS           ADD       SAS           $SP                            SPEDIZIONI
505500000000     C     RNS           ADD       RAS           $RR                            RICAVI REALI
505600000000     C     PNS           ADD       PAS           $RP                            RICAVI PRESUNTI
505700000000     C*
505800000000     C     Q             OCCUR     ABCS13
505900020116     C     NTS(1)        ADD       ATS(1)        $CL13C                         N° TO CLIENTI CORR
506000020116     C     NTS(2)        ADD       ATS(2)        $C213C                         N° TO CLIENTI PREC
506100000000     C*
506200020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
506300000000IF  2C     COMCLI        IFGT      *ZEROS
506400050301IF  2C     $c213c        orGT      *ZEROS
506500000000     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
506600000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
506700020318     C                   MOVEL     '0'           FLGSTPNTW
506800000000     C                   EXSR      STMDAT
506900000000E   2C                   ENDIF
507000000000     C*
507100000000E   1C                   ENDDO
507200020318     C*
507300020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
507400020318     C                   IF        PARSTA = 'K'
507500000000     C* T O T A L I   T O T A L I
507600000000     C     CNW           ADD       CAW           $CL                            NUMERO CLIENTI
507700000000     C     SNW           ADD       SAW           $SP                            SPEDIZIONI
507800000000     C     RNW           ADD       RAW           $RR                            RICAVI REALI
507900000000     C     PNW           ADD       PAW           $RP                            RICAVI PRESUNTI
508000951227     C     CNW13C        ADD       CAW13C        $CL13C                         N° TOT CLIENTI CORR
508100951227     C     CN213C        ADD       CA213C        $C213C                         N° TOT CLIENTI CORR
508200020222     C*
508300020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
508400020222IF  2C     *IN01         IFEQ      *ON
508500020222     C                   EXSR      STAINT
508600020222E   2C                   ENDIF
508700020222     C*
508800020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
508900020222     C                   EVAL      DEPSTPD = '1'
509000000000     C*
509100020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
509200000000IF  1C     COMCLI        IFGT      *ZEROS
509300050301IF  2C     $c213c        orGT      *ZEROS
509400000000     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
509500000000     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
509600000000     C                   EXSR      STMDAT
509700100129     c                   if        parSTAMPA <> 'N'
509800020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
509900100129     c                   end
510000020221E   1C                   ENDIF
510100020318     C*
510200020318     C                   ELSE
510300020318     C* T O T A L I   T O T A L I
510400020318     C     CNW           ADD       CAW           $CL                            NUMERO CLIENTI
510500020318     C     SNW           ADD       SAW           $SP                            SPEDIZIONI
510600020318     C     RNW           ADD       RAW           $RR                            RICAVI REALI
510700020318     C     PNW           ADD       PAW           $RP                            RICAVI PRESUNTI
510800020318     C     CNW13C        ADD       CAW13C        $CL13C                         N° TOT CLIENTI CORR
510900020318     C     CN213C        ADD       CA213C        $C213C                         N° TOT CLIENTI CORR
511000020318     C*
511100020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
511200020318IF  2C     *IN01         IFEQ      *ON
511300020318     C                   EXSR      STAINT
511400020318E   2C                   ENDIF
511500020318     C*
511600020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
511700020318     C                   EVAL      DEPSTPD = '1'
511800020318     C*
511900020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
512000020318IF  1C     COMCLI        IFGT      *ZEROS
512100050301IF  2C     $c213c        orGT      *ZEROS
512200020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
512300020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
512400020318     C                   EXSR      STMDAT
512500100129     c                   if        parSTAMPA <> 'N'
512600020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
512700100129     c                   end
512800020318E   1C                   ENDIF
512900020318     C* T O T A L I   T O T A L I   X   N E T W O R K
513000020404     C*
513100020404     C* SOLO SE RICHIESTO NEL LANCIO
513200020404     C                   IF        PARNTW = 'S'
513300020404     C*
513400020319     C                   MOVEL     '0'           FLGSTPRI          1
513500020319     C*
513600020319DO  1C     2             DO        5             K
513700020318     C     K             OCCUR     ABCSTAN
513800020318     C*
513900020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
514000020318IF  2C     *IN01         IFEQ      *ON
514100020318     C                   EXSR      STAINT
514200020318E   2C                   ENDIF
514300020318     C*
514400020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
514500020318     C                   EVAL      DEPSTPD = '1'
514600020318     C*
514700020318     C     CNSN          ADD       CASN          $CL                            NUMERO CLIENTI
514800020318     C     SNSN          ADD       SASN          $SP                            SPEDIZIONI
514900020318     C     RNSN          ADD       RASN          $RR                            RICAVI REALI
515000020318     C     PNSN          ADD       PASN          $RP                            RICAVI PRESUNTI
515100020318     C*
515200020318     C     K             OCCUR     ABCS13N
515300020318     C     NTSN(1)       ADD       ATSN(1)       $CL13C                         N° TO CLIENTI CORR
515400020318     C     NTSN(2)       ADD       ATSN(2)       $C213C                         N° TO CLIENTI PREC
515500020318     C*
515600020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
515700020318IF  2C     COMCLI        IFGT      *ZEROS
515800050301IF  2C     $c213c        orGT      *ZEROS
515900020319     C                   MOVEL     '1'           FLGSTPRI
516000020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
516100020318     C                   Z-ADD     K             $Q                             NETWORK
516200020318     C                   MOVEL     '1'           FLGSTPNTW
516300020318     C                   EXSR      STMDAT
516400020318E   2C                   ENDIF
516500020318     C*
516600020318E   1C                   ENDDO
516700020319     C                   IF        FLGSTPRI = '1'
516800100129     c                   if        parSTAMPA <> 'N'
516900020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
517000100129     c                   end
517100020318     C                   ENDIF
517200020319     C                   ENDIF
517300020404     C*
517400020404     C                   ENDIF
517500020220     C***
517600020220     C* CLIENTI  T O T A L I  'D'
517700020220     C***
517800020221     C*
517900020220     C     5             OCCUR     ABCSTA
518000020220     C*
518100020220     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
518200020220IF  2C     *IN01         IFEQ      *ON
518300020220     C                   EXSR      STAINT
518400020220E   2C                   ENDIF
518500020222     C*
518600020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
518700020222     C                   EVAL      DEPSTPD = '0'
518800020220     C*
518900020220     C     CNS           ADD       CAS           $CL                            NUMERO CLIENTI
519000020220     C     SNS           ADD       SAS           $SP                            SPEDIZIONI
519100020220     C     RNS           ADD       RAS           $RR                            RICAVI REALI
519200020220     C     PNS           ADD       PAS           $RP                            RICAVI PRESUNTI
519300020220     C*
519400020220     C     5             OCCUR     ABCS13
519500020220     C     NTS(1)        ADD       ATS(1)        $CL13C                         N° TO CLIENTI CORR
519600020220     C     NTS(2)        ADD       ATS(2)        $C213C                         N° TO CLIENTI PREC
519700020220     C*
519800020220     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
519900020220IF  2C     COMCLI        IFGT      *ZEROS
520000050301IF  2C     $c213c        orGT      *ZEROS
520100020220     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
520200020220     C                   Z-ADD     5             $Q                             QUALITA' CLIENTE
520300020318     C                   MOVEL     '0'           FLGSTPNTW
520400020220     C                   EXSR      STMDAT
520500020220E   2C                   ENDIF
520600020318     C*
520700020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
520800020318     C                   IF        PARSTA = 'K'
520900020221     C* T O T A L I   T O T A L I   'D'
521000020221     C     CNWD          ADD       CAWD          $CL                            NUMERO CLIENTI
521100020221     C     SNWD          ADD       SAWD          $SP                            SPEDIZIONI
521200020221     C     RNWD          ADD       RAWD          $RR                            RICAVI REALI
521300020221     C     PNWD          ADD       PAWD          $RP                            RICAVI PRESUNTI
521400020221     C     CNW13CD       ADD       CAW13CD       $CL13C                         N° TOT CLIENTI CORR
521500020221     C     CN213CD       ADD       CA213CD       $C213C                         N° TOT CLIENTI CORR
521600020222     C*
521700020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
521800020222IF  2C     *IN01         IFEQ      *ON
521900020222     C                   EXSR      STAINT
522000020222E   2C                   ENDIF
522100030311     C*
522200030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D' + TOTALE ALT
522300030311     C                   EVAL      DEPSTPD = '3'
522400020220     C*
522500020221IF  1C     COMCLI        IFGT      *ZEROS
522600050301IF  2C     $c213c        orGT      *ZEROS
522700020220     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
522800020220     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
522900020220     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
523000020220     C                   EXSR      STMDAT
523100100129     c                   if        parSTAMPA <> 'N'
523200020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
523300100129     c                   end
523400020221E   1C                   ENDIF
523500020318     C*
523600020318     C                   ELSE
523700020318     C* T O T A L I   T O T A L I   'D'
523800020318     C     CNWD          ADD       CAWD          $CL                            NUMERO CLIENTI
523900020318     C     SNWD          ADD       SAWD          $SP                            SPEDIZIONI
524000020318     C     RNWD          ADD       RAWD          $RR                            RICAVI REALI
524100020318     C     PNWD          ADD       PAWD          $RP                            RICAVI PRESUNTI
524200020318     C     CNW13CD       ADD       CAW13CD       $CL13C                         N° TOT CLIENTI CORR
524300020318     C     CN213CD       ADD       CA213CD       $C213C                         N° TOT CLIENTI CORR
524400020318     C*
524500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
524600020318IF  2C     *IN01         IFEQ      *ON
524700020318     C                   EXSR      STAINT
524800020318E   2C                   ENDIF
524900030311     C*
525000030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D' + TOTALE ALT
525100030311     C                   EVAL      DEPSTPD = '3'
525200020318     C*
525300020318IF  1C     COMCLI        IFGT      *ZEROS
525400050301IF  2C     $c213c        orGT      *ZEROS
525500020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
525600020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
525700020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
525800020318     C                   EXSR      STMDAT
525900100129     c                   if        parSTAMPA <> 'N'
526000020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
526100100129     c                   end
526200020318E   1C                   ENDIF
526300020318     C* T O T A L I   T O T A L I   X   N E T W O R K   "D"
526400020404     C*
526500020404     C* SOLO SE RICHIESTO NEL LANCIO
526600020404     C                   IF        PARNTW = 'S'
526700020404     C*
526800020319     C                   MOVEL     '0'           FLGSTPRI          1
526900020319     C*
527000020319DO  1C     2             DO        5             K
527100020318     C     K             OCCUR     ABCSTAND
527200020318     C*
527300020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
527400020318IF  2C     *IN01         IFEQ      *ON
527500020318     C                   EXSR      STAINT
527600020318E   2C                   ENDIF
527700020318     C*
527800020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
527900020318     C                   EVAL      DEPSTPD = '2'
528000020318     C*
528100020318     C     CNSND         ADD       CASND         $CL                            NUMERO CLIENTI
528200020318     C     SNSND         ADD       SASND         $SP                            SPEDIZIONI
528300020318     C     RNSND         ADD       RASND         $RR                            RICAVI REALI
528400020318     C     PNSND         ADD       PASND         $RP                            RICAVI PRESUNTI
528500020318     C*
528600020318     C     K             OCCUR     ABCS13ND
528700020318     C     NTSND(1)      ADD       ATSND(1)      $CL13C                         N° TO CLIENTI CORR
528800020318     C     NTSND(2)      ADD       ATSND(2)      $C213C                         N° TO CLIENTI PREC
528900020318     C*
529000020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
529100020318IF  2C     COMCLI        IFGT      *ZEROS
529200050301IF  2C     $C213C        orGT      *ZEROS
529300020319     C                   MOVEL     '1'           FLGSTPRI
529400020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
529500020318     C                   Z-ADD     K             $Q                             NETWORK
529600020318     C                   MOVEL     '1'           FLGSTPNTW
529700020318     C                   EXSR      STMDAT
529800020318E   2C                   ENDIF
529900020318     C*
530000020318E   1C                   ENDDO
530100020319     C                   IF        FLGSTPRI = '1'
530200100129     c                   if        parSTAMPA <> 'N'
530300020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
530400100129     c                   end
530500020318     C                   ENDIF
530600020319     C                   ENDIF
530700000000     C*
530800020404     C                   ENDIF
530900020404     C*
531000000000     C                   ENDSR
531100000000     C*------------------------------------------------------------------------*
531200011123     C*?STAINT - STAMPA INTESTAZIONE
531300000000     C*------------------------------------------------------------------------*
531400000000     C     STAINT        BEGSR
531500020221     C*
531600020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
531700020222     C                   IF        FLGSTPLE = '1'
531800100129     c                   if        parSTAMPA <> 'N'
531900020318     C                   WRITE     SE94R2
532000020222     C                   WRITE     SE94LE                                       *LEGENDA
532100100129     c                   end
532200020222     C                   ELSE
532300020222     C                   EVAL      FLGSTPLE = '1'
532400020222     C                   ENDIF
532500020222     C*
532600100129     c                   if        parSTAMPA <> 'N'
532700020222     C                   WRITE     SE94PT                                       *TESTATA
532800020116     C                   WRITE     SE94IN                                       *RIGA INTESTAZIONE
532900020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
533000020116     C                   WRITE     SE94PI                                       *RIGA MESI
533100020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
533200100129     c                   end
533300000000     C                   SETOFF                                       01
533400000000     C*
533500000000     C                   ENDSR
533600000000     C*------------------------------------------------------------------------*
533700000000     C*?STMDAT - STAMPA I DATI DEL CLIENTE
533800000000     C*          INPUT  --> $CATCL : CATEGORIA CLIENTI (N/A/T)
533900000000     C*                     $Q     : QUALITA' CLIENTI  (A/B/C)
534000000000     C*                     $CL    : NUMERO CLIENTI
534100000000     C*                     $SP    : SPEDIZIONI
534200000000     C*                     $RR    : RICAVI REALI
534300000000     C*                     $RP    : RICAVI PRESUNTI
534400000000     C*                     $CL13C : NUMERO TOTALE CLIENTI (13^ COLONNA)
534500951227     C*                     $C213C : NUMERO TOTALE CLIENTI (1^  COLONNA)
534600000000     C*------------------------------------------------------------------------*
534700000000     C     STMDAT        BEGSR
534800000000     C***
534900000000     C* CLIENTE NUOVI/ACQUISITI/TOTALI DI TIPO A/B/C
535000000000     C***
535100020221     C                   MOVEL     *BLANKS       DES12            12
535200000000IF  1C     $CATCL        IFEQ      'N'                                          *N U O V I
535300000000     C                   MOVEL     DRI(4)        DES12                          *PRIMA DESCRIZIONE
535400000000E   1C                   ENDIF
535500000000IF  1C     $CATCL        IFEQ      'A'                                          *A C Q U I S I T I
535600000000     C                   MOVEL     DRI(8)        DES12                          *PRIMA DESCRIZIONE
535700000000E   1C                   ENDIF
535800000000IF  1C     $CATCL        IFEQ      'T'                                          *T O T A L I
535900000000     C                   MOVEL     DRI(12)       DES12                          *PRIMA DESCRIZIONE
536000000000E   1C                   ENDIF
536100000000     C*
536200020318     C                   IF        FLGSTPNTW = '0'
536300000000IF  2C     $Q            IFGT      *ZEROS
536400000000     C                   MOVE      ABC($Q)       DES12                          *"A"  "B"  "C"  " "
536500000000E   2C                   ENDIF
536600020318     C                   ELSE
536700020318IF  2C     $Q            IFGT      *ZEROS
536800020318     C                   MOVE      DNTW($Q)      DES12                          *NETWORK
536900020318E   2C                   ENDIF
537000020318     C                   ENDIF
537100020221     C*
537200020221     C* SE STO STAMPANDO DATI DI CLIENTI 'D' => VALORIZZO IL CAMPO IN STAMPA X EVIDENZIARLI
537300020222     C                   IF        DEPSTPD = '0'
537400020222     C                   EVAL      P1DFLD = *BLANKS
537500020222     C                   ENDIF
537600020222     C                   IF        DEPSTPD = '1'
537700020222     C                   EVAL      P1DFLD = '*'
537800020222     C                   ENDIF
537900020222     C                   IF        DEPSTPD = '2'
538000020222     C                   EVAL      P1DFLD = '**'
538100020222     C                   ENDIF
538200030311     C                   IF        DEPSTPD = '3'
538300030520     C                   EVAL      P1DFLD = '§'
538400030311     C                   ENDIF
538500000000     C*--------------------
538600020221     C* C L I E N T I
538700000000     C*--------------------
538800000000     C                   MOVEL     DRI(13)       DES07             7            *SECONDA DESCRIZIONE
538900000000     C*
539000000000     C                   Z-ADD     $CL           $VA
539100020116     C                   Z-ADD     $CL13C        $VA13C                         *PERIODO CORRENTE
539200020116     C                   Z-ADD     $C213C        $V213C                         *PERIODO PRECEDENTE
539300000000     C*
539400000000     C                   MOVEL     *BLANKS       P1DDES
539500000000     C                   MOVEL     DES12         P1DDES
539600000000     C                   MOVE      DES07         P1DDES
539700000000     C*
539800000000     C* SCRIVE I VALORI IN STAMPA
539900000000     C                   EXSR      SCRVAL
540000030925     C*
540100030925     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
540200030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
540300030925     C                   MOVEL     PARIOE        SE93IOE                        *DELTA ITALIA/ESTERO
540400030925     C                   MOVEL     INCDIV        SE93DIV                        *DIVISIONE
540500030925     C                   MOVEL     INDDIV        SE93DDI                        *DESCRIZIONE DIVISIO
540600030925     C                   MOVEL     INCARE        SE93ARE                        *AREA
540700030925     C                   MOVEL     INDARE        SE93DAR                        *DESCRIZIONE AREA
540800030925     C                   MOVEL     INCFIL        SE93FIL                        *FILIALE
540900030925     C                   MOVEL     INDFIL        SE93DFI                        *DESCRIZIONE FILIALE
541000030925     C                   MOVEL     P1DFLD        SE93AST                        *CARATTERE STRANO
541100030925     C                   MOVEL     P1DDES        SE93DES                        *DESCRIZIONE
541200030925     C                   MOVEL     *BLANKS       SE93CLV
541300030926IF  2C                   IF        FLGSTPNTW = '0'                              *STAMPA NO NETWORK
541400030926IF  3C     $Q            IFGT      *ZEROS
541500030926     C                   MOVEL     ABC($Q)       SE93CLV                        *QUALITA' CLIENTE
541600030926E   3C                   ENDIF
541700030926E   2C                   ENDIF
541800030926     c*
541900030925     C                   MOVEL     '2'           SE93TIP                        *TIPO TOTALE
542000030925     C                   Z-ADD     $V213C        SE93CPR                        *VALORE PRECEDENTE
542100030925     C                   Z-ADD     $VA(1)        SE93C01                        *MESE UNO
542200030925     C                   Z-ADD     $VA(2)        SE93C02                        *MESE DUE
542300030925     C                   Z-ADD     $VA(3)        SE93C03                        *MESE TRE
542400030925     C                   Z-ADD     $VA(4)        SE93C04                        *MESE QUATTRO
542500030925     C                   Z-ADD     $VA(5)        SE93C05                        *MESE CINQUE
542600030925     C                   Z-ADD     $VA(6)        SE93C06                        *MESE SEI
542700030925     C                   Z-ADD     $VA(7)        SE93C07                        *MESE SETTE
542800030925     C                   Z-ADD     $VA(8)        SE93C08                        *MESE OTTO
542900030925     C                   Z-ADD     $VA(9)        SE93C09                        *MESE NOVE
543000030925     C                   Z-ADD     $VA(10)       SE93C10                        *MESE DIECI
543100030925     C                   Z-ADD     $VA(11)       SE93C11                        *MESE UNDICI
543200030925     C                   Z-ADD     $VA(12)       SE93C12                        *MESE DODICI
543300030925     C                   Z-ADD     $VA13C        SE93CTO                        *TOTALE MESI
543400030925E   1C                   ENDIF
543500000000     C*--------------------
543600000000     C* S P E D I Z I O N I
543700000000     C*--------------------
543800000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIIZONE
543900000000     C                   MOVEL     DRI(14)       DES07                          *SECONDA DESCRIZIONE
544000020221     C                   MOVEL     *BLANKS       P1DFLD
544100000000     C*
544200000000     C                   Z-ADD     $SP           $VA
544300000000     C                   Z-ADD     *ZEROS        $VA13C
544400000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
544500020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPEDIZ
544600000000E   1C                   ENDDO
544700020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
544800951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
544900000000     C*
545000000000     C                   MOVEL     *BLANKS       P1DDES
545100000000     C                   MOVEL     DES12         P1DDES
545200000000     C                   MOVE      DES07         P1DDES
545300000000     C*
545400000000     C* SCRIVE I VALORI IN STAMPA
545500000000     C                   EXSR      SCRVAL
545600030925     C*
545700030925     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
545800030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
545900030925     C                   Z-ADD     $V213C        SE93NPR                        *VALORE PRECEDENTE
546000030925     C                   Z-ADD     $VA(1)        SE93N01                        *MESE UNO
546100030925     C                   Z-ADD     $VA(2)        SE93N02                        *MESE DUE
546200030925     C                   Z-ADD     $VA(3)        SE93N03                        *MESE TRE
546300030925     C                   Z-ADD     $VA(4)        SE93N04                        *MESE QUATTRO
546400030925     C                   Z-ADD     $VA(5)        SE93N05                        *MESE CINQUE
546500030925     C                   Z-ADD     $VA(6)        SE93N06                        *MESE SEI
546600030925     C                   Z-ADD     $VA(7)        SE93N07                        *MESE SETTE
546700030925     C                   Z-ADD     $VA(8)        SE93N08                        *MESE OTTO
546800030925     C                   Z-ADD     $VA(9)        SE93N09                        *MESE NOVE
546900030925     C                   Z-ADD     $VA(10)       SE93N10                        *MESE DIECI
547000030925     C                   Z-ADD     $VA(11)       SE93N11                        *MESE UNDICI
547100030925     C                   Z-ADD     $VA(12)       SE93N12                        *MESE DODICI
547200030925     C                   Z-ADD     $VA13C        SE93NTO                        *TOTALE MESI
547300030925E   1C                   ENDIF
547400000000     C*--------------------
547500020220     C* R I C A V I
547600000000     C*--------------------
547700000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIIZONE
547800000000     C                   MOVEL     DRI(15)       DES07                          *SECONDA DESCRIZIONE
547900020221     C                   MOVEL     *BLANKS       P1DFLD
548000000000     C*
548100020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
548200020116     C*    $RR           DIV       1000          $VA
548300020116     C                   Z-ADD     $RR           $VA
548400951227     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
548500000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
548600020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
548700000000E   1C                   ENDDO
548800020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
548900951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
549000000000     C*
549100000000     C                   MOVEL     *BLANKS       P1DDES
549200000000     C                   MOVEL     DES12         P1DDES
549300000000     C                   MOVE      DES07         P1DDES
549400000000     C*
549500000000     C* SCRIVE I VALORI IN STAMPA
549600000000     C                   EXSR      SCRVAL
549700030925     C*
549800030925     C* IMPOSTA I CAMPI "RICAVI" DEL WFILE DI STAMPA
549900030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
550000030925     C                   Z-ADD     $V213C        SE93RPR                        *VALORE PRECEDENTE
550100030925     C                   Z-ADD     $VA(1)        SE93R01                        *MESE UNO
550200030925     C                   Z-ADD     $VA(2)        SE93R02                        *MESE DUE
550300030925     C                   Z-ADD     $VA(3)        SE93R03                        *MESE TRE
550400030925     C                   Z-ADD     $VA(4)        SE93R04                        *MESE QUATTRO
550500030925     C                   Z-ADD     $VA(5)        SE93R05                        *MESE CINQUE
550600030925     C                   Z-ADD     $VA(6)        SE93R06                        *MESE SEI
550700030925     C                   Z-ADD     $VA(7)        SE93R07                        *MESE SETTE
550800030925     C                   Z-ADD     $VA(8)        SE93R08                        *MESE OTTO
550900030925     C                   Z-ADD     $VA(9)        SE93R09                        *MESE NOVE
551000030925     C                   Z-ADD     $VA(10)       SE93R10                        *MESE DIECI
551100030925     C                   Z-ADD     $VA(11)       SE93R11                        *MESE UNDICI
551200030925     C                   Z-ADD     $VA(12)       SE93R12                        *MESE DODICI
551300030925     C                   Z-ADD     $VA13C        SE93RTO                        *TOTALE MESI
551400030925E   1C                   ENDIF
551500000000     C*--------------------
551600000000     C* D E L T A
551700000000     C*--------------------
551800000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIZIONE
551900000000     C                   MOVEL     DRI(16)       DES07                          *SECONDA DESCRIZIONE
552000020221     C                   MOVEL     *BLANKS       P1DFLD
552100000000     C*
552200000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
552300000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
552400000000     C                   Z-ADD     *ZEROS        $PD13C                         *13^PERCENTUALE DELT
552500000000     C                   EXSR      IMPCAL
552600970822     C***
552700970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
552800970822     C***
552900970822     C                   MOVEL     *BLANKS       $SD                            *FALG SEGNALAZIONE
553000970822     C*
553100000000     C                   MOVEL     *BLANKS       P2DDES
553200020221     C                   MOVEL     *BLANKS       P2DFLD
553300000000     C                   MOVEL     DES12         P2DDES
553400000000     C                   MOVE      DES07         P2DDES
553500000000     C*
553600000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
553700000000     C                   EXSR      SCRDEL
553800030925     C*
553900030925     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
554000030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
554100030925     C                   Z-ADD     $PD(13)       SE93DPR                        *VALORE PRECEDENTE
554200030925     C                   Z-ADD     $PD(1)        SE93D01                        *MESE UNO
554300030925     C                   Z-ADD     $PD(2)        SE93D02                        *MESE DUE
554400030925     C                   Z-ADD     $PD(3)        SE93D03                        *MESE TRE
554500030925     C                   Z-ADD     $PD(4)        SE93D04                        *MESE QUATTRO
554600030925     C                   Z-ADD     $PD(5)        SE93D05                        *MESE CINQUE
554700030925     C                   Z-ADD     $PD(6)        SE93D06                        *MESE SEI
554800030925     C                   Z-ADD     $PD(7)        SE93D07                        *MESE SETTE
554900030925     C                   Z-ADD     $PD(8)        SE93D08                        *MESE OTTO
555000030925     C                   Z-ADD     $PD(9)        SE93D09                        *MESE NOVE
555100030925     C                   Z-ADD     $PD(10)       SE93D10                        *MESE DIECI
555200030925     C                   Z-ADD     $PD(11)       SE93D11                        *MESE UNDICI
555300030925     C                   Z-ADD     $PD(12)       SE93D12                        *MESE DODICI
555400030925     C                   Z-ADD     $PD13C        SE93DTO                        *TOTALE MESI
555500030925     C*
555600030925     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
555700030925IF  2C     PARWCP        IFEQ      'S'
555800030925     c                   ADD       1             WNrr
555900030925     c                   Z-ADD     WNrr          SE93NRR                        *righe in stampa
556000030925     C                   WRITE     WFTISE930
556100030925E   2C                   ENDIF
556200030925E   1C                   ENDIF
556300000000     C*
556400000000     C                   ENDSR
556500020701     C*------------------------------------------------------------------------*
556600020701     C*?REPDELFAT - REPERISCE LE % SCOSTAMENTO FATTURATO TRA IL MESE/ANNO DI ELAB. RISPETTO ALL'ANNO
556700020701     C*------------------------------------------------------------------------*
556800020701     C     REPDELFAT     BEGSR
556900020701     C*
557000020712     C* INIZIALIZZO LA SCHIERA E LE VARIABILI DI WRK
557100020712     C                   CLEAR                   $PDF                           *PERCENTUALI SCOST.
557200020712     C                   Z-ADD     1             W_MM              2 0
557300020712     C                   Z-ADD     *ZEROS        W_PROG           18 3
557400020712     C                   Z-ADD     *ZEROS        W_PROGP          18 3
557500020712     C                   Z-ADD     *ZEROS        W_DELFATT        30 9
557600020712     C*
557700020712     C* SOLO X CLIENTI DI CATEGORIA IMPORTANZA DIVERSA DA 'B' E 'C'
557800020712     C                   IF        DESCLV <> 'C' AND
557900020712     C                             DESCLV <> 'B'
558000020712     C*
558100020712     C* CICLO X I 12 MESI DELLE SCHIERE DEI RICAVI REALI A.C. E A.P.
558200020712     C                   DOW       W_MM <= 12
558300020712     C*
558400020712     C* SOMMO I VALORI DEI MESI A.C. E A.P. X CALCOLARE I RELATIVI PROGRESSIVI A.C. E A.P.
558500020712     C                   ADD       R1K(W_MM)     W_PROG
558600020712     C                   ADD       R1KP(W_MM)    W_PROGP
558700020712     C*
558800020712     C* SOLO ALMENO 1 DEI 2 FATTURATI A.C. E A.P. SONO > 0 CALCOLO LA % SCOSTAMENTO FATTURATO A.C. S
558900020712     C                   IF        R1K(W_MM)  <> *zeros AND
559000020712     C                             R1KP(W_MM) <> *zeros
559100020712     C*
559200020712     C* INIZIALIZZO I LVALORE DELLA % SCOSTAMENTO
559300020712     C                   Z-ADD     *zeros        W_DELFATT
559400020712     C*
559500020712     C* QUINDI LA CALCOLO
559600020712     C                   EVAL      W_DELFATT  = (R1K(W_MM) - R1KP(W_MM)) * 100 /
559700020712     C                                           %abs(R1KP(W_MM))
559800020712     C*
559900020712     C* SE LA % OTTENUTA IN VALORE ASSOLUTO E > 999,9 => FORZO VALORE A 999,9
560000020712     C                   IF        W_DELFATT <= -1000
560100020712     C                   EVAL      W_DELFATT  = -999,9
560200020712     C                   ENDIF
560300020712     C                   IF        W_DELFATT >= +1000
560400020712     C                   EVAL      W_DELFATT  = +999,9
560500020712     C                   ENDIF
560600020712     C*
560700020712     C* SE RIENTRA NEL RANGE RICHIESTO NEL LANCIO VALORIZZO LA SCHIERA DA PORTARE IN STAMPA
560800020712     C                   IF        W_DELFATT  >= PARDFD AND
560900020712     C                             W_DELFATT  <= PARDFA
561000020712     C                   EVAL(H)   $PDF(W_MM)  = W_DELFATT
561100020712     C                   ENDIF
561200020712     C                   ENDIF
561300020712     C*
561400020712     C* INCREMENTO IL MESE
561500020712     C                   ADD       1             W_MM
561600020712     C                   ENDDO
561700020712     C*
561800020712     C* VALORIZZO I CAMPI IN STAMPA RELATIVI AI MESI
561900020712     C                   Z-ADD     $PDF(1)       P2CDE1P                        *MESE UNO
562000020712     C                   Z-ADD     $PDF(2)       P2CDE2P                        *MESE DUE
562100020712     C                   Z-ADD     $PDF(3)       P2CDE3P                        *MESE TRE
562200020712     C                   Z-ADD     $PDF(4)       P2CDE4P                        *MESE QUATTRO
562300020712     C                   Z-ADD     $PDF(5)       P2CDE5P                        *MESE CINQUE
562400020712     C                   Z-ADD     $PDF(6)       P2CDE6P                        *MESE SEI
562500020712     C                   Z-ADD     $PDF(7)       P2CDE7P                        *MESE SETTE
562600020712     C                   Z-ADD     $PDF(8)       P2CDE8P                        *MESE OTTO
562700020712     C                   Z-ADD     $PDF(9)       P2CDE9P                        *MESE NOVE
562800020712     C                   Z-ADD     $PDF(10)      P2CDE0P                        *MESE DIECI
562900020712     C                   Z-ADD     $PDF(11)      P2CDEAP                        *MESE UNDICI
563000020712     C                   Z-ADD     $PDF(12)      P2CDEBP                        *MESE DODICI
563100020712     C*
563200020712     C* DOPO LE VALORIZZAZOINI MENSILI EFFETTUO QUELLE DEL PROGRESSIVO
563300020712     C*
563400020712     C* SOLO SE ENTRAMI I FATTURATI A.C. E A.P. SONO > 0 CALCOLO LA % SCOSTAMENTO FATTURATO A.C. SU
563500020712     C                   IF        W_PROG  <> *zeros AND
563600020712     C                             W_PROGP <> *zeros
563700020712     C*
563800020712     C* INIZIALIZZO I LVALORE DELLA % SCOSTAMENTO
563900020712     C                   Z-ADD     *zeros        W_DELFATT
564000020712     C*
564100020712     C* QUINDI LA CALCOLO
564200020712     C                   EVAL      W_DELFATT  = (W_PROG - W_PROGP) * 100 /
564300020712     C                                           %abs(W_PROGP)
564400020712     C*
564500020712     C* SE LA % OTTENUTA IN VALORE ASSOLUTO E > 999,9 => FORZO VALORE A 999,9
564600020712     C                   IF        W_DELFATT <= -1000
564700020712     C                   EVAL      W_DELFATT  = -999,9
564800020712     C                   ENDIF
564900020712     C                   IF        W_DELFATT >= +1000
565000020712     C                   EVAL      W_DELFATT  = +999,9
565100020712     C                   ENDIF
565200020712     C*
565300020712     C* INIZIALIZZO SEMPRE IL CAMPO IN STAMPA RIFERITO ALLA % PROGRESSIVO
565400020712     C                   CLEAR                   P2CDETP
565500020712     C*
565600020712     C* SE RIENTRA NEL RANGE RICHIESTO NEL LANCIO VALORIZZO LA SCHIERA DA PORTARE IN STAMPA
565700020712     C                   IF        W_DELFATT  >= PARDFDP AND
565800020712     C                             W_DELFATT  <= PARDFAP
565900020712     C                   EVAL(H)   P2CDETP     = W_DELFATT
566000020712     C                   ENDIF
566100020712     C                   ENDIF
566200020712     C*
566300020712     C                   ENDIF
566400020701     C*
566500020701     C                   ENDSR
566600020701     C*------------------------------------------------------------------------*
566700020701     C*?INZPDF - INIZILIZZA I CAMPI LA SCHIERA DI WRK X LO SCOSTAMENTO FATTURATO E I RALATIVI CAMPI
566800020701     C*------------------------------------------------------------------------*
566900020701     C     INZPDF        BEGSR
567000020701     C*
567100020701     C                   CLEAR                   $PDF
567200020701     C*
567300020701     C                   Z-ADD     $PDF(1)       P2CDE1P                        *MESE UNO
567400020701     C                   Z-ADD     $PDF(2)       P2CDE2P                        *MESE DUE
567500020701     C                   Z-ADD     $PDF(3)       P2CDE3P                        *MESE TRE
567600020701     C                   Z-ADD     $PDF(4)       P2CDE4P                        *MESE QUATTRO
567700020701     C                   Z-ADD     $PDF(5)       P2CDE5P                        *MESE CINQUE
567800020701     C                   Z-ADD     $PDF(6)       P2CDE6P                        *MESE SEI
567900020701     C                   Z-ADD     $PDF(7)       P2CDE7P                        *MESE SETTE
568000020701     C                   Z-ADD     $PDF(8)       P2CDE8P                        *MESE OTTO
568100020701     C                   Z-ADD     $PDF(9)       P2CDE9P                        *MESE NOVE
568200020701     C                   Z-ADD     $PDF(10)      P2CDE0P                        *MESE DIECI
568300020701     C                   Z-ADD     $PDF(11)      P2CDEAP                        *MESE UNDICI
568400020701     C                   Z-ADD     $PDF(12)      P2CDEBP                        *MESE DODICI
568500020712     C                   Z-ADD     *ZEROS        P2CDETP                        *PROGRESSIVO
568600020701     C*
568700020701     C                   ENDSR
568800000000     C*------------------------------------------------------------------------*
568900000000     C*?STAMP2 - STAMPA I TOTALI NELLA FORMA: CLI / SPE / RIC / DEL
569000000000     C*------------------------------------------------------------------------*
569100000000     C     STAMP2        BEGSR
569200000000     C*--------------------
569300000000     C* I N T E S T A Z I O N E
569400000000     C*--------------------
569500000000IF  1C     *IN01         IFEQ      *ON
569600020215     C                   EXSR      STAINT
569700000000E   1C                   ENDIF
569800000000     C*--------------------
569900020222     C* C L I E N T I
570000000000     C*--------------------
570100000000     C                   MOVEL     DRI(13)       DES03             3
570200000000     C                   MOVEL     *BLANKS       P1DDES
570300000000     C                   MOVEL     DES20         P1DDES
570400000000     C                   MOVE      DES03         P1DDES
570500020222     C*
570600020222     C* SE STO STAMPANDO DATI DI CLIENTI 'D' => VALORIZZO IL CAMPO IN STAMPA X EVIDENZIARLI
570700020222     C                   IF        DEPSTPD = '0'
570800020222     C                   EVAL      P1DFLD = *BLANKS
570900020222     C                   ENDIF
571000020222     C                   IF        DEPSTPD = '1'
571100020222     C                   EVAL      P1DFLD = '*'
571200020222     C                   ENDIF
571300020222     C                   IF        DEPSTPD = '2'
571400020222     C                   EVAL      P1DFLD = '**'
571500020222     C                   ENDIF
571600030311     C                   IF        DEPSTPD = '3'
571700030520     C                   EVAL      P1DFLD = '§'
571800030311     C                   ENDIF
571900000000     C*
572000000000     C                   Z-ADD     CLW           $VA
572100020116     C                   Z-ADD     CTW(1)        $VA13C                         *PERIODO CORRENTE
572200020116     C                   Z-ADD     CTW(2)        $V213C                         *PERIODO PRECEDENTE
572300000000     C*
572400000000     C* SCRIVE I VALORI IN STAMPA
572500000000     C                   EXSR      SCRVAL
572600030925     C*
572700030925     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
572800030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
572900030925     C                   MOVEL     PARIOE        SE93IOE                        *DELTA ITALIA/ESTERO
573000030925     C                   MOVEL     INCDIV        SE93DIV                        *DIVISIONE
573100030925     C                   MOVEL     INDDIV        SE93DDI                        *DESCRIZIONE DIVISIO
573200030925     C                   MOVEL     INCARE        SE93ARE                        *AREA
573300030925     C                   MOVEL     INDARE        SE93DAR                        *DESCRIZIONE AREA
573400030925     C                   MOVEL     INCFIL        SE93FIL                        *FILIALE
573500030925     C                   MOVEL     INDFIL        SE93DFI                        *DESCRIZIONE FILIALE
573600030926IF  2c     WTipStamp2    IFEQ      'F'                                          *da STAFIL
573700030926     C                   MOVEL     DEPFIL        SE93FIL                        *FILIALE
573800030926     C                   MOVEL     DESFIL        SE93DFI                        *DESCRIZIONE FILIALE
573900030926E   2c                   ENDIF
574000030925     C                   MOVEL     P1DFLD        SE93AST                        *CARATTERE STRANO
574100030925     C                   MOVEL     P1DDES        SE93DES                        *DESCRIZIONE
574200030925     C                   MOVEL     *BLANKS       SE93CLV                        *QUALITA' CLIENTE
574300030925     C                   MOVEL     '1'           SE93TIP                        *TIPO TOTALE
574400030925     C                   Z-ADD     $V213C        SE93CPR                        *VALORE PRECEDENTE
574500030925     C                   Z-ADD     $VA(1)        SE93C01                        *MESE UNO
574600030925     C                   Z-ADD     $VA(2)        SE93C02                        *MESE DUE
574700030925     C                   Z-ADD     $VA(3)        SE93C03                        *MESE TRE
574800030925     C                   Z-ADD     $VA(4)        SE93C04                        *MESE QUATTRO
574900030925     C                   Z-ADD     $VA(5)        SE93C05                        *MESE CINQUE
575000030925     C                   Z-ADD     $VA(6)        SE93C06                        *MESE SEI
575100030925     C                   Z-ADD     $VA(7)        SE93C07                        *MESE SETTE
575200030925     C                   Z-ADD     $VA(8)        SE93C08                        *MESE OTTO
575300030925     C                   Z-ADD     $VA(9)        SE93C09                        *MESE NOVE
575400030925     C                   Z-ADD     $VA(10)       SE93C10                        *MESE DIECI
575500030925     C                   Z-ADD     $VA(11)       SE93C11                        *MESE UNDICI
575600030925     C                   Z-ADD     $VA(12)       SE93C12                        *MESE DODICI
575700030925     C                   Z-ADD     $VA13C        SE93CTO                        *TOTALE MESI
575800030925E   1C                   ENDIF
575900000000     C*--------------------
576000000000     C* S P E D I Z I O N I
576100000000     C*--------------------
576200000000     C                   MOVEL     DRI(14)       DES03             3
576300000000     C                   MOVEL     *BLANKS       P1DDES
576400000000     C                   MOVE      DES03         P1DDES
576500020221     C                   MOVEL     *BLANKS       P1DFLD
576600000000     C*
576700000000     C                   Z-ADD     SPW           $VA
576800020116     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
576900000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
577000000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
577100000000E   1C                   ENDDO
577200020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
577300951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
577400000000     C*
577500000000     C* SCRIVE I VALORI IN STAMPA
577600000000     C                   EXSR      SCRVAL
577700030925     C*
577800030925     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
577900030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
578000030925     C                   Z-ADD     $V213C        SE93NPR                        *VALORE PRECEDENTE
578100030925     C                   Z-ADD     $VA(1)        SE93N01                        *MESE UNO
578200030925     C                   Z-ADD     $VA(2)        SE93N02                        *MESE DUE
578300030925     C                   Z-ADD     $VA(3)        SE93N03                        *MESE TRE
578400030925     C                   Z-ADD     $VA(4)        SE93N04                        *MESE QUATTRO
578500030925     C                   Z-ADD     $VA(5)        SE93N05                        *MESE CINQUE
578600030925     C                   Z-ADD     $VA(6)        SE93N06                        *MESE SEI
578700030925     C                   Z-ADD     $VA(7)        SE93N07                        *MESE SETTE
578800030925     C                   Z-ADD     $VA(8)        SE93N08                        *MESE OTTO
578900030925     C                   Z-ADD     $VA(9)        SE93N09                        *MESE NOVE
579000030925     C                   Z-ADD     $VA(10)       SE93N10                        *MESE DIECI
579100030925     C                   Z-ADD     $VA(11)       SE93N11                        *MESE UNDICI
579200030925     C                   Z-ADD     $VA(12)       SE93N12                        *MESE DODICI
579300030925     C                   Z-ADD     $VA13C        SE93NTO                        *TOTALE MESI
579400030925E   1C                   ENDIF
579500000000     C*--------------------
579600000000     C* R I C A V I
579700000000     C*--------------------
579800000000     C                   MOVEL     DRI(15)       DES03             3
579900000000     C                   MOVEL     *BLANKS       P1DDES
580000000000     C                   MOVE      DES03         P1DDES
580100020221     C                   MOVEL     *BLANKS       P1DFLD
580200000000     C*
580300020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
580400020116     C*    RRW           DIV       1000          $VA
580500020116     C                   Z-ADD     RRW           $VA
580600020116     C                   Z-ADD     *ZEROS        $VA13C
580700000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
580800020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
580900000000E   1C                   ENDDO
581000020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
581100951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
581200000000     C*
581300000000     C* SCRIVE I VALORI IN STAMPA
581400000000     C                   EXSR      SCRVAL
581500030925     C*
581600030925     C* IMPOSTA I CAMPI "RICAVI" DEL WFILE DI STAMPA
581700030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
581800030925     C                   Z-ADD     $V213C        SE93RPR                        *VALORE PRECEDENTE
581900030925     C                   Z-ADD     $VA(1)        SE93R01                        *MESE UNO
582000030925     C                   Z-ADD     $VA(2)        SE93R02                        *MESE DUE
582100030925     C                   Z-ADD     $VA(3)        SE93R03                        *MESE TRE
582200030925     C                   Z-ADD     $VA(4)        SE93R04                        *MESE QUATTRO
582300030925     C                   Z-ADD     $VA(5)        SE93R05                        *MESE CINQUE
582400030925     C                   Z-ADD     $VA(6)        SE93R06                        *MESE SEI
582500030925     C                   Z-ADD     $VA(7)        SE93R07                        *MESE SETTE
582600030925     C                   Z-ADD     $VA(8)        SE93R08                        *MESE OTTO
582700030925     C                   Z-ADD     $VA(9)        SE93R09                        *MESE NOVE
582800030925     C                   Z-ADD     $VA(10)       SE93R10                        *MESE DIECI
582900030925     C                   Z-ADD     $VA(11)       SE93R11                        *MESE UNDICI
583000030925     C                   Z-ADD     $VA(12)       SE93R12                        *MESE DODICI
583100030925     C                   Z-ADD     $VA13C        SE93RTO                        *TOTALE MESI
583200030925E   1C                   ENDIF
583300000000     C*--------------------
583400000000     C* D E L T A
583500000000     C*--------------------
583600000000     C                   MOVEL     DRI(16)       DES03             3
583700000000     C                   MOVEL     *BLANKS       P2DDES
583800020221     C                   MOVEL     *BLANKS       P2DFLD
583900000000     C                   MOVE      DES03         P2DDES
584000000000     C*
584100000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
584200000000     C                   Z-ADD     RRW           $RR                            *RICAVI REALI
584300000000     C                   Z-ADD     RPW           $RP                            *RICAVI PRESUNTI
584400000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
584500000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
584600000000     C                   EXSR      IMPCAL
584700970822     C***
584800970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
584900970822     C***
585000970822     C                   MOVEL     *BLANKS       $SD                            *FALG SEGNALAZIONE
585100000000     C*
585200000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
585300000000     C                   EXSR      SCRDEL
585400030925     C*
585500030925     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
585600030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
585700030925     C                   Z-ADD     $PD(13)       SE93DPR                        *VALORE PRECEDENTE
585800030925     C                   Z-ADD     $PD(1)        SE93D01                        *MESE UNO
585900030925     C                   Z-ADD     $PD(2)        SE93D02                        *MESE DUE
586000030925     C                   Z-ADD     $PD(3)        SE93D03                        *MESE TRE
586100030925     C                   Z-ADD     $PD(4)        SE93D04                        *MESE QUATTRO
586200030925     C                   Z-ADD     $PD(5)        SE93D05                        *MESE CINQUE
586300030925     C                   Z-ADD     $PD(6)        SE93D06                        *MESE SEI
586400030925     C                   Z-ADD     $PD(7)        SE93D07                        *MESE SETTE
586500030925     C                   Z-ADD     $PD(8)        SE93D08                        *MESE OTTO
586600030925     C                   Z-ADD     $PD(9)        SE93D09                        *MESE NOVE
586700030925     C                   Z-ADD     $PD(10)       SE93D10                        *MESE DIECI
586800030925     C                   Z-ADD     $PD(11)       SE93D11                        *MESE UNDICI
586900030925     C                   Z-ADD     $PD(12)       SE93D12                        *MESE DODICI
587000030925     C                   Z-ADD     $PD13C        SE93DTO                        *TOTALE MESI
587100030925     C*
587200030925     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
587300030925IF  2C     PARWCP        IFEQ      'S'
587400030925     c                   ADD       1             WNrr
587500030925     c                   Z-ADD     WNrr          SE93NRR                        *righe in stampa
587600030925     C                   WRITE     WFTISE930
587700030925E   2C                   ENDIF
587800030925E   1C                   ENDIF
587900000000     C*
588000000000     C                   ENDSR
588100000000     C*------------------------------------------------------------------------*
588200000000     C*?IMPCAL - IMPOSTA IL CALCOLO DEL DESTA PER TUTTI I VALORI
588300000000     C*          INPUT  --> $RR    : SCHIERA RICAVO REALE
588400000000     C*                     $RP    : SCHIERA RICAVO PRESUNTO
588500000000     C*          OUTPUT --> $PD    : SCHIERA DELTA PERCENTUALE
588600000000     C*                     $PD13C : 13^ PERCENTUALE DELTA
588700000000     C*------------------------------------------------------------------------*
588800000000     C     IMPCAL        BEGSR
588900000000     C*--------------------
589000000000     C* DELTA PERIODO PRECEDENTE
589100000000     C*--------------------
589200020116     C                   Z-ADD     $RR(13)       WRRW             19 5          *RICAVO REALE
589300020116     C                   Z-ADD     $RP(13)       WRPW             19 5          *RICAVO PRESUNTO
589400000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
589500000000     C                   EXSR      CALDEL
589600000000     C                   Z-ADD     WDPW          $PD(13)                        *SCHIERA DELTA
589700000000     C*--------------------
589800000000     C* DELTA 12 MESI
589900000000     C*--------------------
590000000000DO  1C     1             DO        SPARM         J
590100020116     C                   Z-ADD     $RR(J)        WRRW                           *RICAVO REALE
590200020116     C                   Z-ADD     $RP(J)        WRPW                           *RICAVO PRESUNTO
590300020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
590400000000     C                   EXSR      CALDEL
590500000000     C                   Z-ADD     WDPW          $PD(J)                         *SCHIERA DELTA
590600000000E   1C                   ENDDO
590700000000     C*--------------------
590800000000     C* DELTA 13^ COLONNA
590900000000     C*--------------------
591000000000     C                   Z-ADD     *ZEROS        WRRW
591100000000     C                   Z-ADD     *ZEROS        WRPW
591200000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
591300020116     C                   ADD       $RR(J)        WRRW                           *RICAVO REALE
591400020116     C                   ADD       $RP(J)        WRPW                           *RICAVO PRESUNTO
591500000000E   1C                   ENDDO
591600020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
591700000000     C                   EXSR      CALDEL
591800000000     C                   Z-ADD     WDPW          $PD13C                         *13^ COLONNA
591900000000     C*
592000000000     C                   ENDSR
592100000000     C*------------------------------------------------------------------------*
592200000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
592300000000     C*          INPUT  --> WRRW: RICAVO REALE
592400000000     C*                     WRPW: RICAVO PRESUNTO
592500000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
592600000000     C*------------------------------------------------------------------------*
592700000000     C     CALDEL        BEGSR
592800000000     C*--------------------
592900000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
593000000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
593100000000     C*--------------------
593200000000     C*
593300000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
593400020116     C     WRRW          SUB       WRPW          GAP              19 5          (REALE - PRESUNTO)
593500000000     C     GAP           MULT      100           GAP                            (GAP x 100)
593600000000     C*
593700000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
593800000000IF  1C     WRRW          IFEQ      *ZEROS
593900000000     C                   Z-ADD     1             WRRW
594000000000E   1C                   ENDIF
594100000000     C*
594200000000     C     GAP           DIV       WRRW          GAP
594300000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
594400000000     C*
594500000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
594600000000     C     WDPW          COMP      0                                    28
594700000000     C* POSITIVO
594800000000IF  1C     *IN28         IFEQ      *OFF
594900000000IF  2C     GAP           IFGE      1000
595000000000     C                   Z-ADD     +999,9        WDPW
595100000000E   2C                   ENDIF
595200000000     C* NEGATIVO
595300000000X   1C                   ELSE
595400000000IF  2C     GAP           IFLE      -1000
595500000000     C                   Z-ADD     -999,9        WDPW
595600000000E   2C                   ENDIF
595700000000E   1C                   ENDIF
595800000000     C*
595900000000     C                   ENDSR
596000000000     C*------------------------------------------------------------------------*
596100000000     C*?SCRVAL - SCRIVE I VALORI IN STAMPA
596200000000     C*------------------------------------------------------------------------*
596300000000     C     SCRVAL        BEGSR
596400000000     C*
596500951227     C                   Z-ADD     $V213C        P1CVAP                         *VALORE PRECEDENTE
596600000000     C                   Z-ADD     $VA(1)        P1CVA1                         *MESE UNO
596700000000     C                   Z-ADD     $VA(2)        P1CVA2                         *MESE DUE
596800000000     C                   Z-ADD     $VA(3)        P1CVA3                         *MESE TRE
596900000000     C                   Z-ADD     $VA(4)        P1CVA4                         *MESE QUATTRO
597000000000     C                   Z-ADD     $VA(5)        P1CVA5                         *MESE CINQUE
597100000000     C                   Z-ADD     $VA(6)        P1CVA6                         *MESE SEI
597200000000     C                   Z-ADD     $VA(7)        P1CVA7                         *MESE SETTE
597300000000     C                   Z-ADD     $VA(8)        P1CVA8                         *MESE OTTO
597400000000     C                   Z-ADD     $VA(9)        P1CVA9                         *MESE NOVE
597500000000     C                   Z-ADD     $VA(10)       P1CVA0                         *MESE DIECI
597600000000     C                   Z-ADD     $VA(11)       P1CVAA                         *MESE UNDICI
597700000000     C                   Z-ADD     $VA(12)       P1CVAB                         *MESE DODICI
597800000000     C                   Z-ADD     $VA13C        P1CVAT                         *TOTALE MESI
597900000000     C*
598000100129     c                   if        parSTAMPA <> 'N'
598100020215     C  N53              WRITE     SE94P1                                       *STAMPA RIGA VALORE
598200100129     c                   end
598300011123     C   53              SETOFF                                       53
598400000000     C*
598500000000     C                   ENDSR
598600000000     C*------------------------------------------------------------------------*
598700000000     C*?SCRDEL - SCRIVE I VALORI DELTA IN STAMPA
598800000000     C*------------------------------------------------------------------------*
598900000000     C     SCRDEL        BEGSR
599000000000     C*
599100000000     C                   Z-ADD     $PD(13)       P2CDEP                         *VALORE PRECEDENTE
599200000000     C                   Z-ADD     $PD(1)        P2CDE1                         *MESE UNO
599300970822     C                   MOVEL     $SD(1)        P2CSP1
599400000000     C                   Z-ADD     $PD(2)        P2CDE2                         *MESE DUE
599500970822     C                   MOVEL     $SD(2)        P2CSP2
599600000000     C                   Z-ADD     $PD(3)        P2CDE3                         *MESE TRE
599700970822     C                   MOVEL     $SD(3)        P2CSP3
599800000000     C                   Z-ADD     $PD(4)        P2CDE4                         *MESE QUATTRO
599900970822     C                   MOVEL     $SD(4)        P2CSP4
600000000000     C                   Z-ADD     $PD(5)        P2CDE5                         *MESE CINQUE
600100970822     C                   MOVEL     $SD(5)        P2CSP5
600200000000     C                   Z-ADD     $PD(6)        P2CDE6                         *MESE SEI
600300970822     C                   MOVEL     $SD(6)        P2CSP6
600400000000     C                   Z-ADD     $PD(7)        P2CDE7                         *MESE SETTE
600500970822     C                   MOVEL     $SD(7)        P2CSP7
600600000000     C                   Z-ADD     $PD(8)        P2CDE8                         *MESE OTTO
600700970822     C                   MOVEL     $SD(8)        P2CSP8
600800000000     C                   Z-ADD     $PD(9)        P2CDE9                         *MESE NOVE
600900970822     C                   MOVEL     $SD(9)        P2CSP9
601000000000     C                   Z-ADD     $PD(10)       P2CDE0                         *MESE DIECI
601100970822     C                   MOVEL     $SD(10)       P2CSP0
601200000000     C                   Z-ADD     $PD(11)       P2CDEA                         *MESE UNDICI
601300970822     C                   MOVEL     $SD(11)       P2CSPA
601400000000     C                   Z-ADD     $PD(12)       P2CDEB                         *MESE DODICI
601500970822     C                   MOVEL     $SD(12)       P2CSPB
601600000000     C                   Z-ADD     $PD13C        P2CDET                         *TOTALE MESI
601700000000     C*
601800100129     c                   if        parSTAMPA <> 'N'
601900020215     C  N53              WRITE     SE94P2                                       *STAMPA RIGA VALORE
602000100129     c                   end
602100011123     C   53              SETOFF                                       53
602200000000     C*
602300000000     C                   ENDSR
602400000000     C*------------------------------------------------------------------------*
602500000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
602600000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
602700000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
602800000000     C*------------------------------------------------------------------------*
602900000000     C     DECDIV        BEGSR
603000000000     C*
603100000000     C                   Z-ADD     1             D
603200000000     C     DEPDIV        LOOKUP    DIV(D)                                 99
603300000000IF  1C     *IN99         IFEQ      *OFF
603400000000     C                   MOVEL     *ALL'*'       DESDIV           20
603500000000X   1C                   ELSE
603600000000     C                   MOVEL     DDIV(D)       DESDIV
603700000000E   1C                   ENDIF
603800000000     C*
603900000000     C                   ENDSR
604000000000     C*------------------------------------------------------------------------*
604100000000     C*? DECARE - DECODIFICA CODICE AREA
604200000000     C*          INPUT  -- DEPARE: CODICE AREA
604300000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
604400000000     C*------------------------------------------------------------------------*
604500000000     C     DECARE        BEGSR
604600000000     C*
604700000000     C                   Z-ADD     1             D
604800000000     C     DEPARE        LOOKUP    ARE(D)                                 99
604900000000IF  1C     *IN99         IFEQ      *OFF
605000000000     C                   MOVEL     *ALL'*'       DESARE           20
605100000000X   1C                   ELSE
605200000000     C                   MOVEL     DARE(D)       DESARE
605300000000E   1C                   ENDIF
605400000000     C*
605500000000     C                   ENDSR
605600000000     C*------------------------------------------------------------------------*
605700000000     C*? DECFIL - DECODIFICA CODICE FILIALE
605800000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
605900000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
606000000000     C*------------------------------------------------------------------------*
606100000000     C     DECFIL        BEGSR
606200000000     C*
606300000000     C                   Z-ADD     1             D
606400000000     C     DEPFIL        LOOKUP    FIL(D)                                 99
606500000000IF  1C     *IN99         IFEQ      *OFF
606600000000     C                   MOVEL     *ALL'*'       DESFIL           20
606700000000X   1C                   ELSE
606800000000     C                   MOVEL     DFIL(D)       DESFIL
606900000000E   1C                   ENDIF
607000000000     C*
607100000000     C                   ENDSR
607200000000     C*------------------------------------------------------------------------*
607300000000     C*?DECAGE - DECODIFICA CODICE AGENTE
607400000000     C*          INPUT  -- DEPAGE: CODICE AGENTE COMMERCIALE
607500000000     C*          OUTPUT -- DESAGE: DESCRIZIONE AGENTE
607600000000     C*                    DE2PCF: PERCENTUALE COMPETENZA FILIALE
607700000000     C*------------------------------------------------------------------------*
607800000000     C     DECAGE        BEGSR
607900000000     C*
608000980407     C                   MOVEL     *BLANKS       DESAGE           25
608100000000     C                   Z-ADD     1             D
608200000000     C     DEPAGE        LOOKUP    AGE(D)                                 99
608300000000IF  1C     *IN99         IFEQ      *OFF
608400980407     C                   MOVEL     *ALL'*'       DESAGE
608500000000     C                   Z-ADD     *ZEROS        DESPCF
608600000000     C                   MOVEL     *BLANKS       DE2PCF
608700000000X   1C                   ELSE
608800000000     C                   MOVEL     DAGE(D)       DESAGE
608900000000     C                   Z-ADD     CAGE(D)       DESPCF
609000000000     C*
609100000000     C* METTO IL CAMPO PERCENTUALE IN UNO ALFANUMERICO E LO EDITO(Z)
609200000000     C                   MOVEL     DESPCF        DE2PCF            3
609300000000     C                   Z-ADD     1             P
609400000000     C                   MOVEA     DE2PCF        PCF
609500000000     C*
609600000000DO  2C                   DO        3             P
609700000000IF  3C     PCF(P)        IFEQ      *ZEROS
609800000000     C                   MOVEL     *BLANKS       PCF(P)
609900000000X   3C                   ELSE
610000000000     C                   Z-ADD     4             P
610100000000E   3C                   ENDIF
610200000000E   2C                   ENDDO
610300000000     C                   MOVEA     PCF           DE2PCF
610400000000E   1C                   ENDIF
610500000000     C*
610600000000     C                   ENDSR
610700000000     C*------------------------------------------------------------------------*
610800000000     C*?DECKSC - DECODIFICA CODICE CLIENTE
610900000825     C*          INPUT  -- DEPCLI: CODICE CLIENTE
611000000000     C*          OUTPUT -- DESKSC: RAGIONE SOCIALE CLIENTE
611100951006     C*                    DESITC: DESCRIZIONE CATEGORIA SEAT
611200000000     C*                    DESCLV: QUALITA' CLIENTE
611300000000     C*                    DESCLN: CLIENTE NUOVO O ACQUISITO
611400000000     C*------------------------------------------------------------------------*
611500000000     C     DECKSC        BEGSR
611600120131      *--
611700120131      *-- pulisce x nuovo cliente i campi da evidenziare in seguito
611800120131     c                   eval           Cod_Blocco_Ultimo   = *blank
611900120131     c                   eval           Data_Blocco_Ultimo  = 0
612000120131     c                   eval           Data_Prima_Fat_6    = 0
612100120131      *--
612200000000     C*--------------------
612300000000     C* RAGIONE SOCIALE
612400960708     C* BLOCCO CLIENTE
612500000000     C*--------------------
612600000000     C                   MOVEL     *ALL'*'       DESKSC                         *SE NON TROVATO
612700960708     C                   MOVEL     *BLANKS       DESABL                         *SE NON TROVATO
612800980406     C                   CLEAR                   BS69DS
612900980406     C                   CLEAR                   ACODS
613000980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
613100000825     C                   MOVEL     DEPCLI        I69KAC                         *CLIENTE X CNACO
613200070419     C                   MOVEL     DEPCLI        I69KIN                         *CLIENTE X CNIND
613300980406     C                   CALL      'TIBS69R'
613400980406     C                   PARM                    BS69DS
613500980406     C                   PARM                    ACODS
613600980406     C                   PARM                    INDDS
613700980406     C                   PARM                    CLPDS
613800980406     C                   PARM                    CLSDS
613900980406IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
614000980406     C                   MOVEL     ACORAG        DESKSC                         *RAGIONE SOCIALE
614100980406     C                   MOVEL     ACOABL        DESABL                         *BLOCCO CLIENTE
614200070419      * Nuovi dati anagrafici x i nuovi WADELPCF/E
614300070419     C                   MOVEL     INDvia        DESvia                         *Indirizzo
614400070419     C                   MOVEL     INDcap        DEScap                         *CAP
614500070419     C                   MOVEL     INDcae        DEScae                         *CAP Europeo
614600070419     C                   MOVEL     INDprv        DESprv                         *Provincia
614700070419     C                   MOVEL     INDcit        DEScit                         *Città
614800070419     C                   MOVEL     INDsta        DESsta                         *Nazione
614900070615     C                   MOVEL     INDcdf        DEScdf                         *Cod.Fiscale
615000070615     C                   MOVEL     INDiva        DESiva                         *Part.IVA
615100070618     C                   MOVEL     INDtel        DEStel                         *Telefono
615200980406E   1C                   ENDIF
615300000000     C*--------------------
615400951006     C* CATEGORIA SEAT
615500000000     C*--------------------
615600951006     C                   MOVEL     *BLANKS       DESITC           35            *SE NON TROVATO
615700020906     C                   Z-ADD     1             KTBKUT
615800951006     C                   MOVEL     '1L'          KTBCOD
615900951006     C                   MOVEL     *BLANKS       KTBKEY
616000951006     C                   MOVE      ACOITC        NUM5              5 0
616100951006     C                   MOVEL     NUM5          KTBKEY
616200951006     C     KEYTAB        CHAIN     TABEL00F                           99
616300951006IF  1C     *IN99         IFEQ      *OFF
616400951006     C                   MOVEL     TBLUNI        DS1L
616500951006     C                   MOVEL     §1LDES        DESITC                         *CATEGORIA SEAT
616600951006E   1C                   ENDIF
616700951006     C*--------------------
616800951006     C* QUALITA' CLIENTE
616900951006     C*--------------------
617000960710     C                   MOVEL     WFDCLV        DESCLV
617100000000     C                   SETOFF                                           28
617200010313     C     DESCLV        LOOKUP    ABC                                    28
617300960710IF  1C     *IN28         IFEQ      *OFF
617400000000     C                   MOVEL     'C'           DESCLV
617500960710E   1C                   ENDIF
617600960708     C*--------------------
617700120131     C* CAUSALE BLOCCO e data prima spedizione Fatturata
617800960708     C*--------------------
617900960710     C                   MOVEL     *BLANKS       DESCBL           30
618000980406     C                   CLEAR                   BS69DS
618100980406     C                   CLEAR                   CLPDS
618200980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
618300000825     C                   MOVEL     DEPCLI        I69KCP                         *CLIENTE X CNCLP
618400980406     C                   CALL      'TIBS69R'
618500980406     C                   PARM                    BS69DS
618600980406     C                   PARM                    ACODS
618700980406     C                   PARM                    INDDS
618800980406     C                   PARM                    CLPDS
618900980406     C                   PARM                    CLSDS
619000980406IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
619100120131     c                   eval           Data_Prima_Fat_6 = CLPDPS
619200130318IF  2C     DESABL        IFne      *blank                                       *CLIENTE BLOCCATO
619300130318IF  2C     DESABL        andne     '*'                                          *CLIENTE BLOCCATO
619400120131      **
619500120131      **  Reperisce la Data di quando è stato fatto il Blocco Contabile sul cliente
619600120131     C                   clear                   TrulBLCds
619700120131     c                   eval      IblcTLA = 'L'
619800120131     c                   z-add     clpKcC        IblcKCC
619900120131     c                   z-add     clpKsC        IblcKSC
620000120131     c                   call      'TRULBLCR'
620100120131     C                   PARM                    KPJBA
620200120131     C                   PARM                    TrulBLCds
620300120131     c                   eval           Data_Blocco_Ultimo  = OblcDAV
620400120131     c                   eval           Cod_Blocco_Ultimo   = clpNAR
620500120131      **
620600980406     C                   Z-ADD     1             C
620700980406     C                   SETOFF                                           28
620800980406     C     CLPNAR        LOOKUP    CBL(C)                                 28
620900980406IF  3C     *IN28         IFEQ      *ON
621000980406     C                   MOVEL     DCBL(C)       DESCBL
621100980406E   3C                   ENDIF
621200980406E   2C                   ENDIF
621300980406E   1C                   ENDIF
621400000000     C*--------------------
621500000000     C* CLIENTE NUOVO O ACQUISITO
621600000000     C*--------------------
621700000000     C* PER IL PERIODO PRECEDENTE ALLA STATISTICA SE UN CLIENTE HA DEI SALDI E'
621800000000     C* ACQUISITO, SE NON NE HA E' NUOVO: NON POSSO PRENDERE IL CAMPO WFDCLN PER
621900000000     C* PER L'ANNO PRECEDENTE PERCHE' IL CLIENTE POTEVA ESSERE NUOVO
622000960710     C                   MOVEL     *BLANKS       DESCLN                         *SE NON TROVATO
622100000000IF  1C     TIPANN        IFEQ      'P'                                          *PERIODO PRECEDENTE
622200000000     C                   MOVEL     *BLANKS       DESCLN                         *NON NUOVO
622300000000X   1C                   ELSE                                                   *PERIODO CORRENTE
622400070822      * forza il flag precedentemente testato nel caso particolare sopraddescritto
622500070822      * qualora si tratti di stampa o Italia o Estero.
622600070822     c                   if        desCLN <>'Y' and sav_desCLN_Y ='Y' and
622700070822     c                             wfdCLI =  sav_depCLI
622800070822     c                   movel     'Y'           wfdCLN
622900070822     c                   end
623000000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O ACQUISITO
623100000000E   1C                   ENDIF
623200000000     C*
623300000000     C                   ENDSR
623400000000     C*------------------------------------------------------------------------*
623500000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
623600000000     C*------------------------------------------------------------------------*
623700000000     C     CARMES        BEGSR
623800000000     C*
623900000000     C                   CLEAR                   ANN                            *ANNI
624000000000     C                   CLEAR                   MES                            *MESI
624100000000     C                   Z-ADD     *ZEROS        J                              *N° MESI DA ELABORAR
624200000000     C*
624300990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
624400000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
624500990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
624600000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
624700020701     C*
624800020701     C* SICCOME LA STATISTICA NON PO' + ESSERE LANCIATA A CAVALLO DI ANNI DIVERSI L'ANNO INIZIALE -1
624900020701     C* L'ANNO FINALE -1
625000020701     C     WAA           SUB       1             WAAP              4 0          *ANNO INIZIALE PRECD
625100020701     C
625200000000     C*
625300000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
625400000000IF  1C     WAA2          IFEQ      WAA
625500000000     C*
625600000000     C* CALCOLO I MESI DI DIFFERENZA
625700000000DO  2C     WMM           DOWLE     WMM2
625800000000     C                   ADD       1             J
625900000000     C                   Z-ADD     WAA           ANN(J)
626000000000     C                   Z-ADD     WMM           MES(J)
626100000000     C                   ADD       1             WMM
626200000000E   2C                   ENDDO
626300000000     C***
626400000000     C* SPARM = N° MESI SELEZIONATI
626500000000     C***
626600000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
626700000000     C*
626800000000     C* IL RESTO DELLA SCHIERA A ZERO
626900000000DO  2C     J             DOWLT     12
627000000000     C                   ADD       1             J
627100000000     C                   Z-ADD     *ZEROS        ANN(J)
627200000000     C                   Z-ADD     *ZEROS        MES(J)
627300000000E   2C                   ENDDO
627400000000     C*
627500000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
627600000000X   1C                   ELSE
627700000000     C*
627800000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
627900000000DO  2C     WMM           DOWLE     12
628000000000     C                   ADD       1             J
628100000000     C                   Z-ADD     WAA           ANN(J)
628200000000     C                   Z-ADD     WMM           MES(J)
628300000000     C                   ADD       1             WMM
628400000000E   2C                   ENDDO
628500000000     C*
628600000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
628700000000     C                   Z-ADD     01            WMM
628800000000DO  2C     WMM           DOWLE     WMM2
628900000000     C                   ADD       1             J
629000000000     C                   Z-ADD     WAA2          ANN(J)
629100000000     C                   Z-ADD     WMM           MES(J)
629200000000     C                   ADD       1             WMM
629300000000E   2C                   ENDDO
629400000000     C***
629500000000     C* SPARM = N° MESI SELEZIONATI
629600000000     C***
629700000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
629800000000     C*
629900000000     C* IL RESTO DELLA SCHIERA A ZERO
630000000000DO  2C     J             DOWLT     12
630100000000     C                   ADD       1             J
630200000000     C                   Z-ADD     *ZEROS        ANN(J)
630300000000     C                   Z-ADD     *ZEROS        MES(J)
630400000000E   2C                   ENDDO
630500000000E   1C                   ENDIF
630600000000     C*
630700000000     C                   ENDSR
630800000000     C*------------------------------------------------------------------------*
630900000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
631000000000     C*------------------------------------------------------------------------*
631100000000     C     DECTES        BEGSR
631200000000     C*--------------------
631300000000     C* DATA E ORA
631400000000     C*--------------------
631500000000     C                   Z-ADD     UDATE         DATA              6 0
631600000000     C                   TIME                    ORA               6 0
631700950602     C*--------------------
631800950602     C* DATI IN STATISTICA
631900950602     C*--------------------
632000950602IF  1C     PARIOE        IFEQ      'I'
632100950602     C                   MOVEL     DRI(5)        PTCIOE
632200950602E   1C                   ENDIF
632300950602IF  1C     PARIOE        IFEQ      'E'
632400950602     C                   MOVEL     DRI(6)        PTCIOE
632500950602E   1C                   ENDIF
632600950602IF  1C     PARIOE        IFEQ      *BLANKS
632700950602     C                   MOVEL     DRI(7)        PTCIOE
632800950602E   1C                   ENDIF
632900000829     C*--------------------
633000000829     C* DATI CLIENTE / UNIFICANTE
633100000829     C*--------------------
633200000829IF  1C     PARSCE        IFEQ      'C'
633300000829     C                   MOVEL     DRI(29)       PTDSCE
633400000829X   1C                   ELSE
633500000829     C                   MOVEL     DRI(30)       PTDSCE
633600000829E   1C                   ENDIF
633700000000     C*--------------------
633800000000     C* DECODIFICA MESI
633900000000     C*--------------------
634000000000IF  1C     MES(1)        IFGT      *ZEROS
634100000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ
634200000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
634300990609     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
634400000000E   1C                   ENDIF
634500000000     C*
634600000000IF  1C     MES(2)        IFGT      *ZEROS
634700000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ
634800000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
634900990609     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
635000000000E   1C                   ENDIF
635100000000     C*
635200000000IF  1C     MES(3)        IFGT      *ZEROS
635300000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ
635400000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
635500990609     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
635600000000E   1C                   ENDIF
635700000000     C*
635800000000IF  1C     MES(4)        IFGT      *ZEROS
635900000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ
636000000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
636100990609     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
636200000000E   1C                   ENDIF
636300000000     C*
636400000000IF  1C     MES(5)        IFGT      *ZEROS
636500000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ
636600000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
636700990609     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
636800000000E   1C                   ENDIF
636900000000     C*
637000000000IF  1C     MES(6)        IFGT      *ZEROS
637100000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ
637200000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
637300990609     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
637400000000E   1C                   ENDIF
637500000000     C*
637600000000IF  1C     MES(7)        IFGT      *ZEROS
637700000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ
637800000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
637900990609     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
638000000000E   1C                   ENDIF
638100000000     C*
638200000000IF  1C     MES(8)        IFGT      *ZEROS
638300000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ
638400000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
638500990609     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
638600000000E   1C                   ENDIF
638700000000     C*
638800000000IF  1C     MES(9)        IFGT      *ZEROS
638900000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ
639000000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
639100990609     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
639200000000E   1C                   ENDIF
639300000000     C*
639400000000IF  1C     MES(10)       IFGT      *ZEROS
639500000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ
639600000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
639700990609     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
639800000000E   1C                   ENDIF
639900000000     C*
640000000000IF  1C     MES(11)       IFGT      *ZEROS
640100000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ
640200000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
640300990609     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
640400000000E   1C                   ENDIF
640500000000     C*
640600000000IF  1C     MES(12)       IFGT      *ZEROS
640700000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ
640800000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
640900990609     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
641000000000E   1C                   ENDIF
641100000000     C*
641200000000     C                   ENDSR
641300000000     C*------------------------------------------------------------------------*
641400000000     C*?CARTAB - CARICA I DATI IN TABELLE
641500000000     C*------------------------------------------------------------------------*
641600000000     C     CARTAB        BEGSR
641700000000     C*--------------------
641800000000     C* FILIALI/AREE/DIVISIONI
641900000000     C*--------------------
642000000000     C*
642100000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
642200000000     C                   Z-ADD     *ZEROS        KORFIL
642300000000     C     KEYORG        SETLL     AZORG01L
642400000000     C                   READ      AZORG01L                               99
642500000000     C*
642600000000     C* CICLO FINO A FINE FILE
642700000000     C                   Z-ADD     *ZEROS        F
642800951219DO  1C     *IN99         DOWEQ     *OFF
642900000000     C*
643000000000     C* ELIMINA RECORD ANNULLATI
643100000000IF  2C     ORGFVA        IFEQ      *BLANKS
643200000000     C                   ADD       1             F
643300000000     C* F I L I A L I
643400000000     C                   Z-ADD     ORGFIL        FIL(F)
643500000000     C                   MOVEL     ORGDES        DFIL(F)
643600000707     C                   MOVEL     ORGDE3        OG143
643700020627     C                   MOVEL     §OGNTW        POS(F)
643800000000     C* A R E E
643900020906     C                   Z-ADD     1             KTBKUT
644000000000     C                   MOVEL     '05'          KTBCOD
644100000000     C                   MOVEL     *BLANKS       KTBKEY
644200000000     C                   MOVEL     ORGCAR        KTBKEY
644300000000     C     KEYTAB        CHAIN     TABEL00F                           99
644400000000IF  3C     *IN99         IFEQ      *OFF
644500000000     C                   Z-ADD     ORGCAR        ARE(F)
644600000000     C                   MOVEL     TBLUNI        DARE(F)
644700000000E   3C                   ENDIF
644800000000     C* D I V I S I O N I
644900020906     C                   Z-ADD     1             KTBKUT
645000000000     C                   MOVEL     '17'          KTBCOD
645100000000     C                   MOVEL     *BLANKS       KTBKEY
645200000000     C                   MOVEL     ORGFL3        KTBKEY
645300000000     C     KEYTAB        CHAIN     TABEL00F                           99
645400000000IF  3C     *IN99         IFEQ      *OFF
645500000000     C                   MOVEL     ORGFL3        DIV(F)
645600000000     C                   MOVEL     TBLUNI        DDIV(F)
645700000000E   3C                   ENDIF
645800000000E   2C                   ENDIF
645900000000     C*
646000000000     C* LEGGE IL RECORD SUCCESSIVO
646100000000     C                   READ      AZORG01L                               99
646200000000E   1C                   ENDDO
646300000000     C*--------------------
646400000000     C* COMMERCIALI
646500000000     C*--------------------
646600140612     C************       Z-ADD     1             KTBKUT
646700140612     C************       MOVEL     '01'          KTBCOD
646800140612     C***  KEYTA2        SETLL     TABEL00F
646900140612     C***  KEYTA2        READE     TABEL00F                               99
647000140612     C************       Z-ADD     *ZEROS        C
647100140612DO  1C***  *IN99         DOWEQ     *OFF
647200140612IF  2C***  TBLFLG        IFEQ      *BLANKS
647300140612     C************       ADD       1             C
647400140612     C************       MOVEL     TBLUNI        DS01
647500140612     C************       MOVEL     TBLKEY        AGE(C)
647600140612     C************       MOVEL     §01AGE        DAGE(C)
647700140612IF  3C***  PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
647800140612     C************       Z-ADD     §01PCF        CAGE(C)
647900140612X   3C************       ELSE                                                   *% LAVORO ESTERO
648000140612     C************       Z-ADD     §01PCE        CAGE(C)
648100140612E   3C************       ENDIF
648200140612E   2C************       ENDIF
648300140612     C***  KEYTA2        READE     TABEL00F                               99
648400140612E   1C************       ENDDO
648500140612      *------
648600140612     c                   open      azCMM01l
648700140612     C                   Z-ADD     *ZEROS        C
648800140612     C     *loval        SETLL     azCMM01l
648900140612     C                   read      azCMM01l
649000140612DO  1C                   dow       not %Eof(azCMM01l)
649100140612IF  2C                   IF           cmmATB = *blank
649200140612     C                   ADD       1             C
649300140612     C                   z-add     cmmCOD        AGE(C)
649400140612     C                   MOVEL     cmmDES        DAGE(C)
649500140612IF  3C     PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
649600140612     C                   Z-ADD     cmmPLI        CAGE(C)
649700140612X   3C                   ELSE                                                   *% LAVORO ESTERO
649800140612     C                   Z-ADD     cmmPLE        CAGE(C)
649900140612E   3C                   ENDIF
650000140612E   2C                   end
650100140612     C                   read      azCMM01l
650200140612E   1C                   ENDDO
650300140612     c                   close     azCMM01l
650400960708     C*--------------------
650500960708     C* CAUSALI BLOCCO CLIENTI
650600960708     C*--------------------
650700020906     C                   Z-ADD     1             KTBKUT
650800960708     C                   MOVEL     'BI'          KTBCOD
650900960708     C     KEYTA2        SETLL     TABEL00F
651000960708     C     KEYTA2        READE     TABEL00F                               99
651100960708     C                   Z-ADD     *ZEROS        C
651200960708DO  1C     *IN99         DOWEQ     *OFF
651300960708IF  2C     TBLFLG        IFEQ      *BLANKS
651400960708     C                   ADD       1             C
651500960708     C                   MOVEL     TBLKEY        CBL(C)
651600960708     C                   MOVEL     TBLUNI        DCBL(C)
651700960708E   2C                   ENDIF
651800960708     C     KEYTA2        READE     TABEL00F                               99
651900960708E   1C                   ENDDO
652000980521     C*---
652100980521     C* QUALITA' CLIENTE
652200980521     C*---
652300020906     C                   Z-ADD     1             KTBKUT
652400980521     C                   MOVEL     'IC'          KTBCOD
652500980521     C     KEYTA2        SETLL     TABEL00F
652600980521     C     KEYTA2        READE     TABEL00F                               99
652700980521DO  1C     *IN99         DOWEQ     *OFF
652800980521IF  2C     TBLFLG        IFEQ      *BLANKS
652900020220     C                   MOVEL     TBLUNI        DSIC
653000020220     C                   MOVEL     TBLKEY        ABC(§SICOR)
653100980521E   2C                   ENDIF
653200980521     C     KEYTA2        READE     TABEL00F                               99
653300980521E   1C                   ENDDO
653400000000     C*
653500000000     C                   ENDSR
653600000000     C*------------------------------------------------------------------------*
653700000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
653800000000     C*------------------------------------------------------------------------*
653900000000     C     STAPAR        BEGSR
654000000000     C*--------------------
654100000000     C* NOME PROGRAMMA
654200000000     C*--------------------
654300020116     C                   MOVEL     'TISE93R'     PTCPGM
654400000000     C*--------------------
654500000000     C* DESCRIZIONE STATISTICA
654600000000     C*--------------------
654700000000     C                   MOVEL     DST(S)        PTCDST
654800000000     C*--------------------
654900000000     C* CRAZIONE FILE DI LAVORO
655000000000     C*--------------------
655100000000IF  1C     PARCRE        IFEQ      'S'
655200000000     C                   MOVEL     'SI'          PACCRE
655300000000X   1C                   ELSE
655400000000     C                   MOVEL     'NO'          PACCRE
655500000000E   1C                   ENDIF
655600000000     C*--------------------
655700000000     C* DATE LANCIO
655800000000     C*--------------------
655900000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
656000000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
656100000000     C*--------------------
656200000000     C* SELEZIONI PER TIPO SERVIZIO
656300000000     C*--------------------
656400000000IF  1C     PARTSP        IFEQ      *BLANKS
656500000000     C                   MOVEL     DRI(1)        PADTSP
656600100129      **
656700100129      **  si memorizza quanti tipi servizi sono attivi nella tabella
656800100129     c                   z-add     0             Quanti_Tserv
656900100129     c                   clear                   TServ
657000100129     c                   clear                   DTServ
657100100129      **  carica codice e descrizione su 2 schiere e Quanti_serv contiene il nr.dei tipi servizio abilitati
657200100129     C                   Z-ADD     1             KTBKUT
657300100129     C                   MOVEL     '5E'          KTBCOD
657400100129     C     KEYTA2        setll     TABEL00F
657500100129     C     KEYTA2        reade     TABEL00F
657600100129     c                   dow       not %EoF(TABEL00F)
657700100129     c                   add       1             Quanti_Tserv
657800100129     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
657900100129     c                   eval        tserv(Quanti_Tserv) = tblkey
658000100129     c                   eval       Dtserv(Quanti_Tserv) = §5EDES
658100100129     C     KEYTA2        reade     TABEL00F
658200100129     c                   endDO
658300100129      **
658400000000X   1C                   ELSE
658500100129      **
658600000000     C                   MOVEL     PARTSP        PACTSP
658700020906     C                   Z-ADD     1             KTBKUT
658800000000     C                   MOVEL     '5E'          KTBCOD
658900000000     C                   MOVEL     *BLANKS       KTBKEY
659000000000     C                   MOVEL     PARTSP        KTBKEY
659100980406     C     KEYTAB        CHAIN     TABEL00F                           99
659200000000IF  2C     *IN99         IFEQ      *OFF
659300090610     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
659400090610     C                   MOVEL     §5EDES        PADTSP                         *TIPO SERVIZIO
659500000000E   2C                   ENDIF
659600000000E   1C                   ENDIF
659700950208     C*--------------------
659800950208     C* SELEZIONI PER DELTA ITALIA/ESTERO
659900950208     C*--------------------
660000950208     C                   MOVEL     PARIOE        PACIOE
660100950208IF  1C     PARIOE        IFEQ      'I'
660200950208     C                   MOVEL     DRI(5)        PADIOE
660300950208E   1C                   ENDIF
660400950208IF  1C     PARIOE        IFEQ      'E'
660500950208     C                   MOVEL     DRI(6)        PADIOE
660600950208E   1C                   ENDIF
660700950208IF  1C     PARIOE        IFEQ      *BLANKS
660800950208     C                   MOVEL     DRI(7)        PADIOE
660900950208E   1C                   ENDIF
661000951018     C*--------------------
661100970822     C* SELEZIONI PER QUALITA' CLIENTE
661200951018     C*--------------------
661300951018     C                   MOVEL     PARCLV        PACCLV
661400010618     C                   MOVEL     PARCL1        PACCL1
661500010618     C                   MOVEL     PARCL2        PACCL2
661600951018     C                   MOVEL     *BLANKS       PADCLV
661700951018IF  1C     PARCLV        IFEQ      *BLANKS
661800010618     C     PARCL1        ANDEQ     *BLANKS
661900010618     C     PARCL2        ANDEQ     *BLANKS
662000951018     C                   MOVEL     DRI(1)        PADCLV
662100980528E   1C                   ENDIF
662200010618     C*--------------------
662300010618     C* TIPO STAMPA SINTETICA S/N
662400010618     C*--------------------
662500010618     C                   MOVEL     PARSIN        PACSIN
662600980528     C*--------------------
662700980528     C* % SCOSTAMENTO DELTA TOTALE
662800980528     C*--------------------
662900980528     C                   Z-ADD     PARPDD        PACPDD
663000980528     C                   Z-ADD     PARPDA        PACPDA
663100980528     C*--------------------
663200980528     C* % SCOSTAMENTO DELTA PER FASCE
663300980528     C*--------------------
663400970822     C                   Z-ADD     PARSPD        PACSPD
663500020702     C*--------------------
663600020702     C* % SCOSTAMENTO FATTURATO ANNO PRECEDENTE
663700020702     C*--------------------
663800020702     C                   Z-ADD     PARDFD        PACDFD
663900020702     C                   Z-ADD     PARDFA        PACDFA
664000020702     C                   Z-ADD     PARDFDP       PACDFDP
664100020702     C                   Z-ADD     PARDFAP       PACDFAP
664200000000     C*--------------------
664300000000     C* SELEZIONI PER DELTA FILIALE
664400000000     C*--------------------
664500000000IF  1C     PARFIC        IFGT      *ZEROS                                       *FILIALE
664600000000     C                   Z-ADD     PARFIC        DEPFIL
664700000000     C                   EXSR      DECFIL
664800000000     C                   Z-ADD     *ZEROS        DEPFIL
664900000000     C                   MOVEL     PARFIC        PACFIC
665000000000     C                   MOVEL     DESFIL        PADFIC
665100000000X   1C                   ELSE
665200000000     C                   MOVEL     *BLANKS       PACFIC
665300000000E   1C                   ENDIF
665400000000     C*
665500000000IF  1C     PARARC        IFGT      *ZEROS                                       *AREA
665600000000     C                   Z-ADD     PARARC        DEPARE
665700000000     C                   EXSR      DECARE
665800000000     C                   Z-ADD     *ZEROS        DEPARE
665900000000     C                   MOVEL     PARARC        PACARC
666000000000     C                   MOVEL     DESARE        PADARC
666100000000X   1C                   ELSE
666200000000     C                   MOVEL     *BLANKS       PACARC
666300000000E   1C                   ENDIF
666400000000     C*
666500000000IF  1C     PARDIC        IFNE      *BLANKS                                      *DIVISIONE
666600000000     C                   MOVEL     PARDIC        DEPDIV
666700000000     C                   EXSR      DECDIV
666800000000     C                   MOVEL     *BLANKS       DEPDIV
666900000000     C                   MOVEL     PARDIC        PACDIC
667000000000     C                   MOVEL     DESDIV        PADDIC
667100000000E   1C                   ENDIF
667200000000     C*--------------------
667300000000     C* SELEZIONI PER DELTA COMMERCIALE
667400000000     C*--------------------
667500000000IF  1C     PARCOF        IFGT      *ZEROS                                       *COMMERCIALE
667600000000     C                   Z-ADD     PARCOF        DEPAGE
667700000000     C                   EXSR      DECAGE
667800000000     C                   Z-ADD     *ZEROS        DEPAGE
667900000000     C                   MOVEL     PARCOF        PACCOF
668000000000     C                   MOVEL     DESAGE        PADCOF
668100000000X   1C                   ELSE
668200000000     C                   MOVEL     *BLANKS       PACCOF
668300000000E   1C                   ENDIF
668400000000     C*
668500000000IF  1C     PARDIF        IFNE      *BLANKS                                      *DIVISIONE
668600000000     C                   MOVEL     PARDIF        DEPDIV
668700000000     C                   EXSR      DECDIV
668800000000     C                   MOVEL     *BLANKS       DEPDIV
668900000000     C                   MOVEL     PARDIF        PACDIF
669000000000     C                   MOVEL     DESDIV        PADDIF
669100000000E   1C                   ENDIF
669200000707     C*--------------------
669300000707     C* DATI POSTE
669400000707     C*--------------------
669500000707IF  1C     PARPOS        IFEQ      'S'
669600000707     C                   MOVEL     'SI'          PADPOS
669700000707X   1C                   ELSE
669800000707     C                   MOVEL     'NO'          PADPOS
669900000707E   1C                   ENDIF
670000000825     C*--------------------
670100000825     C* DATI CLIENTE / UNIFICANTE
670200000825     C*--------------------
670300000825     C                   MOVEL     PARSCE        PACSCE
670400000825IF  1C     PARSCE        IFEQ      'C'
670500000825     C                   MOVEL     DRI(29)       PADSCE
670600000825X   1C                   ELSE
670700000825     C                   MOVEL     DRI(30)       PADSCE
670800000825E   1C                   ENDIF
670900000000     C*
671000000000     C* S T A M P A
671100000000     C*
671200100129     c                   if        parSTAMPA <> 'N'
671300020222     C                   WRITE     SE94PT                                       *INTESTAZIONE
671400020116     C                   WRITE     SE94PA                                       *PARAMETRI
671500100129     c                   end
671600000000     C*
671700000000     C* ACCENDO INDICATORE DI OF PER STAMPARE LA TESTATA ALLA 1^ ROTTURA CLIENTE
671800000000     C                   SETON                                        01
671900000000     C                   MOVEL     '0'           PRIGIR
672000000000     C*
672100000000     C                   ENDSR
672200980406     C*------------------------------------------------------------------------*
672300980406     C*?FINSR - OPERAZIONI FINALI
672400980406     C*------------------------------------------------------------------------*
672500980406     C     FINSR         BEGSR
672600980406     C*
672700980406     C* CHIUDE I FILE DEI PGM CHIAMATI
672800980406     C                   CLEAR                   BS69DS
672900980406     C                   CLEAR                   ACODS
673000980406     C                   CLEAR                   INDDS
673100980406     C                   CLEAR                   CLPDS
673200980406     C                   CLEAR                   CLSDS
673300980406     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
673400980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
673500980406     C                   CALL      'TIBS69R'
673600980406     C                   PARM                    BS69DS
673700980406     C                   PARM                    ACODS
673800980406     C                   PARM                    INDDS
673900980406     C                   PARM                    CLPDS
674000980406     C                   PARM                    CLSDS
674100980406     C*
674200980406     C                   ENDSR
674300030924     C*------------------------------------------------------------------------*
674400030924     C*?CANWF - CANCELLA DATI WFILE, SE RICHIESTO
674500030924     C*------------------------------------------------------------------------*
674600030924     C     CANWF         BEGSR
674700030924     C*
674800030925     C     PARSTA        IFEQ      'F'
674900030924     C     PARWCP        IFEQ      'S'                                          *SE RICHIESTO
675000030924     c                   MOVEL     PARIOE        KE93IOE                        *ITALIA/ESTERO
675100030924     C     KEYSE93       SETLL     WFTISE93L
675200030924     C     KEYSE93       READE     WFTISE93L                              99
675300030924     C                   DOW       NOT *IN99
675400030924     C                   DELETE    WFTISE930
675500030924     C     KEYSE93       READE     WFTISE93L                              99
675600030924     C                   ENDDO
675700030925     C                   ENDIF
675800030925     C                   ENDIF
675900030924     C*
676000030924     C                   ENDSR
676100000000     C*------------------------------------------------------------------------*
676200000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
676300000000     C*------------------------------------------------------------------------*
676400000000     C     *INZSR        BEGSR
676500960708     C*--------------------
676600960708     C* RICEVIMENTO PARAMETRI
676700960708     C*--------------------
676800000000     C     *ENTRY        PLIST
676900000000     C                   PARM                    KPJBA
677000000000     C                   MOVEL     KPJBU         PARAM
677100100129      *
677200100129      * se non si deve più eseguire la stampa viene passato il flag impostato a "N"
677300100129     c                   if        parSTAMPA <> 'N'
677400100129     c                   open      TISE94P
677500100129     c                   end
677600960708     C*--------------------
677700960708     C* VARIABILI RIFERITE AL DATABASE
677800960708     C*--------------------
677900000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
678000100201     C     *LIKE         DEFINE    TBLCOD        KTBCOD
678100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
678200000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
678300950213     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEL01L
678400000000     C     *LIKE         DEFINE    WFDARE        KWDARE
678500000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
678600000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
678700960710     C     *LIKE         DEFINE    WFDCLV        KWDCLV
678800000825     C     *LIKE         DEFINE    WFDCLI        KWDCLI
678900000000     C     *LIKE         DEFINE    WFDANN        KWDANN
679000000000     C     *LIKE         DEFINE    WFDMES        KWDMES
679100960708     C     *LIKE         DEFINE    WFDDIV        DEPDIV                         *DI DEPOSITO
679200000000     C     *LIKE         DEFINE    WFDARE        DEPARE
679300000000     C     *LIKE         DEFINE    WFDFIL        DEPFIL
679400000000     C     *LIKE         DEFINE    WFDAGE        DEPAGE
679500081013     C     *LIKE         DEFINE    WFDRGD        DEPRGD
679600081013     C     *LIKE         DEFINE    WFDRGF        DEPRGF
679700070822     C     *LIKE         DEFINE    WFDCLI        DEPCLI
679800070822     C     *LIKE         DEFINE    WFDCLI        sav_depCLI
679900020319     C     *LIKE         DEFINE    WFDCLI        DEPCL2
680000000000     C     *LIKE         DEFINE    WFDANN        DEPANN
680100000000     C     *LIKE         DEFINE    WFDMES        DEPMES
680200020319     C     *LIKE         DEFINE    WFDNTW        DEPNTW
680300140612     C**** *LIKE         DEFINE    §01PCF        DESPCF
680400140612     C     *LIKE         DEFINE    cmmPLI        DESPCF
680500000000     C     *LIKE         DEFINE    ACORAG        DESKSC
680600070419     C     *LIKE         DEFINE    INDvia        DESvia
680700070419     C     *LIKE         DEFINE    INDcit        DEScit
680800070419     C     *LIKE         DEFINE    INDcap        DEScap
680900070419     C     *LIKE         DEFINE    INDcae        DEScae
681000070419     C     *LIKE         DEFINE    INDprv        DESprv
681100070419     C     *LIKE         DEFINE    INDsta        DESsta
681200070615     C     *LIKE         DEFINE    INDcdf        DEScdf                         *Cod.Fiscale
681300070615     C     *LIKE         DEFINE    INDiva        DESiva                         *Part.IVA
681400070618     C     *LIKE         DEFINE    INDtel        DEStel                         *Telefono
681500000000     C     *LIKE         DEFINE    CLPCLV        DESCLV
681600000000     C     *LIKE         DEFINE    WFDCLN        DESCLN
681700070822     C     *LIKE         DEFINE    WFDCLN        sav_desCLN_Y
681800960708     C     *LIKE         DEFINE    ACOABL        DESABL
681900030925     C     *LIKE         DEFINE    SE93IOE       KE93IOE
682000100201     c     *LIKE         DEFINE    k             NumElem
682100100201      **
682200960708     C*--------------------
682300960708     C* VARIABILI NON RIFERITE AL DATABASE
682400960708     C*--------------------
682500000000     C                   Z-ADD     *ZEROS        C                 4 0          *COMMERCIALI
682600000000     C                   Z-ADD     *ZEROS        D                 4 0          *PER DECODIFICHE
682700000000     C                   Z-ADD     *ZEROS        F                 4 0          *VOCI ORGANIGRAMMA
682800000000     C                   Z-ADD     *ZEROS        J                 4 0          *GENERICO
682900000000     C                   Z-ADD     *ZEROS        P                 4 0          *ELEMENTI PERCENTUAL
683000000000     C                   Z-ADD     *ZEROS        Q                 4 0          *QUALITA' CLIENTE
683100000000     C                   Z-ADD     *ZEROS        S                 4 0          *STATISTICA IN ELAB
683200000000     C                   Z-ADD     *ZEROS        X                 2 0          *PER I MESI
683300000000     C                   Z-ADD     *ZEROS        MM                2 0          *PER I MESI
683400000000     C                   Z-ADD     *ZEROS        IN                2 0          *PER I MESI
683500000000     C                   Z-ADD     *ZEROS        SPARM             4 0          *N° MESI SELEZIONATI
683600000828     C                   Z-ADD     *ZEROS        I                 5 0          *INDICE
683700000828     C                   Z-ADD     *ZEROS        IS                5 0          *INDICE
683800020318     C                   Z-ADD     *ZEROS        K                 4 0          *NETWORK
683900020701     C                   Z-ADD     *ZEROS        II                5 0          *INDICE
684000020701     C*--------------------
684100020701     C* INDICATORI DI PROCEDURA
684200020701     C*--------------------
684300020701     C                   SETOFF                                       54
684400960708     C*--------------------
684500960708     C* VARIABILI DI CONTROLLO
684600960708     C*--------------------
684700000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
684800000000     C                   MOVEL     '0'           PASGEN            1            MEMOR SALDI GENERALI
684900000000     C                   MOVEL     '0'           PASDIV            1            MEMOR SALDI DIVISION
685000000000     C                   MOVEL     '0'           PASARE            1            MEMOR SALDI AREA
685100000000     C                   MOVEL     '0'           PASFIL            1            MEMOR SALDI FILIALE
685200000000     C                   MOVEL     '0'           PASAGE            1            MEMOR SALDI COMM.LE
685300000000     C                   MOVEL     '0'           PASKSC            1            MEMOR SALDI CLIENTE
685400951227     C                   MOVEL     '0'           FLAKSC            1            FLAG CONT CLI CORR
685500951227     C                   MOVEL     '0'           FL2KSC            1            FLAG CONT CLI PREC
685600000000     C                   MOVEL     '0'           FLAPAS            1            FLAG PASSAGGIO LETTU
685700000000     C                   MOVEL     '0'           $CATCL            1            CATEGORIA CLIENTE
685800000000     C                   Z-ADD     *ZEROS        $Q                1 0          QUALITA' CLIENTE
685900000000     C                   MOVEL     ' '           TIPANN            1            *TIPO ANNO (P/C)
686000000000     C                   MOVEL     '0'           PRIGIR            1            *PRIMO GIRO
686100000000     C                   MOVEL     '0'           ORAKSC            1            *ORA STAMPA CLIENTE
686200000828     C                   MOVEL     'N'           $STRIG            1            *STAMPA RIGA
686300020221     C                   MOVEL     '0'           DEPSTPD           1            *FLAG DI STAMPA SI/N
686400020222     C                   MOVEL     '0'           FLGSTPLE          1            *FLAG DI STAMPA SI/N
686500020318     C                   MOVEL     '0'           FLGSTPNTW         1            *FLAG DI STAMPA SI/N
686600020319     C                   MOVEL     '0'           FLGSTPRI          1            *FLAG DI STAMPA SI/N
686700960708     C*--------------------
686800960708     C* CHAIVI DI LETTURA
686900960708     C*--------------------
687000960708     C* LETTURA TABEL00F - COMPLETA
687100000000     C     KEYTAB        KLIST
687200000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
687300000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
687400000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
687500960708     C* LETTURA TABEL00F - PARZIALE
687600000000     C     KEYTA2        KLIST
687700000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
687800000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
687900960708     C* LETTURA AZORG01L - COMPLETA
688000000000     C     KEYORG        KLIST
688100000000     C                   KFLD                    KORFIL                         *FILIALE
688200960708     C* LETTURA WFDEL01L - PARZIALE
688300000000     C     KEYDEL        KLIST
688400950208     C                   KFLD                    KWDDIV                         *DIVISIONE
688500000000     C                   KFLD                    KWDARE                         *AREA
688600000000     C                   KFLD                    KWDFIL                         *FILIALE
688700000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
688800960710     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
688900000825     C                   KFLD                    KWDCLI                         *CLIENTE
689000000000     C                   KFLD                    KWDANN                         *ANNO
689100000000     C                   KFLD                    KWDMES                         *MESE
689200030924     C* LETTURA WFTISE93L - PARZIALE
689300030924     C     KEYSE93       KLIST
689400030924     C                   KFLD                    KE93IOE                        *ITALIA/ESTERO
689500070420     C* LETTURA TFNTC00F - COMPLETA
689600070420     C     KeyNTC        KLIST
689700070420     C                   KFLD                    NTCapl                         *CODICE APPLICAZIONE
689800070420     C                   KFLD                    NTCnk1                         *CHIAVE 1 NOTE
689900070420     C                   KFLD                    NTCnk2                         *CHIAVE 2 NOTE
690000070420     C                   KFLD                    NTCtnt                         *TIPO NOTA
690100000828     C*
690200130131      ***
690300130131     c     k_deaV1       KLIST
690400130131     C                   KFLD                    VARIA             1
690500130131     C                   KFLD                    DEPCLI
690600130131     C*  serve x il FUEL
690700130131     C                   eval       VARIA ='f'
690800130131     C*
690900000828     C* IMNPOSTA DATA / ORA CORRENTE
691000000828     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
691100000828     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
691200000828     C                   Z-ADD     *ZEROS        G08INV
691300000828     C                   MOVEL     '0'           G08ERR
691400000828     C                   CALL      'XSRDA8'
691500000828     C                   PARM                    WLBDA8
691600000828     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
691700010725     C*
691800010725     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
691900010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
692000010725     C                   MOVE      28            WDFIN
692100081114     C*
692200081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
692300081114     C*
692400081114     C*  Ultimo giorno del mese
692500081114     C                   MOVE      WDFIN         MMGG_4            4 0
692600081114     C                   MOVEL     MMGG_4        Mm_2              2 0
692700081114     C                   MOVEl     WDFIN         Anno_4            4 0
692800081114      * se è febbraio
692900081114     c                   If        Mm_2 = 2
693000081114      *
693100081114      *  potrebbe essere bisestile 29 gg.
693200081114     c     Anno_4        div       4             risulta           3 0
693300081114     c                   mvr                     resto             3 0
693400081114      *  Se bisestile
693500081114     c                   If        resto = 0
693600081114     C                   MOVE      '29'          WDFIN
693700081114     c                   end
693800081114      *
693900081114     c                   else
694000081114      *
694100081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
694200081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
694300081114     c                                Mm_2 = 9 or Mm_2 = 11
694400081114     C                   MOVE      '30'          WDFIN
694500081114     c                   else
694600081114     C                   MOVE      '31'          WDFIN
694700081114     c                   end
694800081114      *
694900081114     c                   endIf
695000081114     C*
695100081114     C*----------- F I N E    M E S E ---------------------------*
695200081114     C*
695300000000     C*
695400000000     C* MEMORIZZA I DATI IN TABELLE
695500000000     C                   EXSR      CARTAB
695600000000     C*
695700000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
695800000000     C                   EXSR      CARMES
695900000000     C*
696000000000     C* DECODIFICA INTESTAZIONE
696100000000     C                   EXSR      DECTES
696200000000     C*
696300000000     C* VEDO QUALE STATISTICA DEVO ELABORARE
696400000000IF  1C     PARSTA        IFEQ      'K'
696500000000     C                   Z-ADD     1             S                              *DETTAGLIO CLIENTE
696600000000E   1C                   ENDIF
696700000000IF  1C     PARSTA        IFEQ      'C'
696800000000     C                   Z-ADD     2             S                              *DETTAGLIO COMM.LE
696900000000E   1C                   ENDIF
697000000000IF  1C     PARSTA        IFEQ      'F'
697100000000     C                   Z-ADD     3             S                              *DETTAGLIO FILIALE
697200000000E   1C                   ENDIF
697300051102     C*
697400051102IF  1C     PARSCE        IFEQ      'U'                                          *STAT. UNIFICANTE
697500051102IF  1C     PARSTA        andeq     'K'                                           dett.x cliente
697600051102     c                   eval      LEG040 = '(+) --> Cliente BLOCCATO'
697700051102     c                   end
697800030924     C*
697900030924     C* CANCELLA DATI WFILE, SE RICHIESTO
698000030924     C                   EXSR      CANWF
698100000000     C*
698200091124     C* Deve impostare i totali per Netowrk
698300091124IF  1C     PARIOE        IFEQ      'E'
698400091124     C                   open      waDELpcN
698500091124E   1C                   ENDIF
698600130131     C*
698700130131     C                   ENDSR
698800130131     C*------------------------------------------------------------------------*
698900130131     C*?ACCUMULA le VARIE - al momento solo x il FUEL (che si vuole scorporare)
699000130131     C*------------------------------------------------------------------------*
699100130131     C     VARIE_x_ClientBEGSR
699200130131      *
699300130131      *  Lo dovrà scrivere una sola volta anche x i giri fra Italia ed Estero
699400130131      *   è per questo che cancellando i records una volta elaborati verrà
699500130131      *   scritto correttamente la prima volta che incontra il codice CLIENTE.
699600130131     c                   move      'N'           deve_scriverlo    1
699700130131      *
699800130131      * ?Cicla sul file delle varie x il tipo servizio (f)->FUEL e x CLIENTE
699900130131      *
700000130131      * ? Man mano che legge e totalizza i records li CANCELLA dal WORKFile
700100130131      * ? per alleggerire le successive operazioni.
700200130131      *  La lettura è x VARIA/CLIENTE
700300130131     c     k_deaV1       setll     wfDEAv1l
700400130131     c     k_deaV1       reade     wfDEAv1l
700500130131     C*
700600130131     c                   DOW       not %EoF(wfDEAv1l)
700700130131     C*
700800130131     C* lo fa solo la prima, primissima volta e basta
700900130131     c                   if        deve_scriverlo = 'N'
701000130131     c                   eval         deve_scriverlo = 'S'
701100130131     c                   clear                   WADELVT0
701200130131     C                   eval      watAAD =  WAdAAD
701300130131     C                   eval      watMMD =  WAdMMD
701400130131     C                   eval      watAAA =  WAdAAA
701500130131     C                   eval      watMMA =  WAdMMA
701600130131     C                   eval      watCLV =  desCLV
701700130131     C                   eval      watCLI =  depCLI
701800130131     C                   eval      watCLD =  desKSC
701900130131     C                   eval      watSVR =  WFVSVR
702000130131     c                   end
702100130131     C*
702200130131     C*  Anno in corso
702300130131     C                   IF        WFVANN =  WAdAAD
702400130131     C*
702500130131     c                   select
702600130131     C*
702700130131     C                   when      WFVmes =  1
702800130131     c                   eval      WAtVA_01SC = wfvNSP + WAtVA_01SC
702900130131     c                   eval      WAtVA_01VC = wfvRCP + WAtVA_01VC
703000130131     C                   when      WFVmes =  2
703100130131     c                   eval      WAtVA_02SC = wfvNSP + WAtVA_02SC
703200130131     c                   eval      WAtVA_02VC = wfvRCP + WAtVA_02VC
703300130131     C                   when      WFVmes =  3
703400130131     c                   eval      WAtVA_03SC = wfvNSP + WAtVA_03SC
703500130131     c                   eval      WAtVA_03VC = wfvRCP + WAtVA_03VC
703600130131     C                   when      WFVmes =  4
703700130131     c                   eval      WAtVA_04SC = wfvNSP + WAtVA_04SC
703800130131     c                   eval      WAtVA_04VC = wfvRCP + WAtVA_04VC
703900130131     C                   when      WFVmes =  5
704000130131     c                   eval      WAtVA_05SC = wfvNSP + WAtVA_05SC
704100130131     c                   eval      WAtVA_05VC = wfvRCP + WAtVA_05VC
704200130131     C                   when      WFVmes =  6
704300130131     c                   eval      WAtVA_06SC = wfvNSP + WAtVA_06SC
704400130131     c                   eval      WAtVA_06VC = wfvRCP + WAtVA_06VC
704500130131     C                   when      WFVmes =  7
704600130131     c                   eval      WAtVA_07SC = wfvNSP + WAtVA_07SC
704700130131     c                   eval      WAtVA_07VC = wfvRCP + WAtVA_07VC
704800130131     C                   when      WFVmes =  8
704900130131     c                   eval      WAtVA_08SC = wfvNSP + WAtVA_08SC
705000130131     c                   eval      WAtVA_08VC = wfvRCP + WAtVA_08VC
705100130131     C                   when      WFVmes =  9
705200130131     c                   eval      WAtVA_09SC = wfvNSP + WAtVA_09SC
705300130131     c                   eval      WAtVA_09VC = wfvRCP + WAtVA_09VC
705400130131     C                   when      WFVmes = 10
705500130131     c                   eval      WAtVA_10SC = wfvNSP + WAtVA_10SC
705600130131     c                   eval      WAtVA_10VC = wfvRCP + WAtVA_10VC
705700130131     C                   when      WFVmes = 11
705800130131     c                   eval      WAtVA_11SC = wfvNSP + WAtVA_11SC
705900130131     c                   eval      WAtVA_11VC = wfvRCP + WAtVA_11VC
706000130131     C                   when      WFVmes = 12
706100130131     c                   eval      WAtVA_12SC = wfvNSP + WAtVA_12SC
706200130131     c                   eval      WAtVA_12VC = wfvRCP + WAtVA_12VC
706300130131     c                   endSL
706400130131     C*
706500130131     C                   eval      WAtVAT_SC  = wfvNSP  + WAtVAT_SC
706600130131     C                   eval      WAtVAT_VC  = wfvRCP  + WAtVAT_VC
706700130131     C*  Anno precedente
706800130131     C                   ELSE
706900130131     C*
707000130131     c                   select
707100130131     C*
707200130131     C                   when      WFVmes =  1
707300130131     c                   eval      WAtVA_01SP = wfvNSP + WAtVA_01SP
707400130131     c                   eval      WAtVA_01VP = wfvRCP + WAtVA_01VP
707500130131     C                   when      WFVmes =  2
707600130131     c                   eval      WAtVA_02SP = wfvNSP + WAtVA_02SP
707700130131     c                   eval      WAtVA_02VP = wfvRCP + WAtVA_02VP
707800130131     C                   when      WFVmes =  3
707900130131     c                   eval      WAtVA_03SP = wfvNSP + WAtVA_03SP
708000130131     c                   eval      WAtVA_03VP = wfvRCP + WAtVA_03VP
708100130131     C                   when      WFVmes =  4
708200130131     c                   eval      WAtVA_04SP = wfvNSP + WAtVA_04SP
708300130131     c                   eval      WAtVA_04VP = wfvRCP + WAtVA_04VP
708400130131     C                   when      WFVmes =  5
708500130131     c                   eval      WAtVA_05SP = wfvNSP + WAtVA_05SP
708600130131     c                   eval      WAtVA_05VP = wfvRCP + WAtVA_05VP
708700130131     C                   when      WFVmes =  6
708800130131     c                   eval      WAtVA_06SP = wfvNSP + WAtVA_06SP
708900130131     c                   eval      WAtVA_06VP = wfvRCP + WAtVA_06VP
709000130131     C                   when      WFVmes =  7
709100130131     c                   eval      WAtVA_07SP = wfvNSP + WAtVA_07SP
709200130131     c                   eval      WAtVA_07VP = wfvRCP + WAtVA_07VP
709300130131     C                   when      WFVmes =  8
709400130131     c                   eval      WAtVA_08SP = wfvNSP + WAtVA_08SP
709500130131     c                   eval      WAtVA_08VP = wfvRCP + WAtVA_08VP
709600130131     C                   when      WFVmes =  9
709700130131     c                   eval      WAtVA_09SP = wfvNSP + WAtVA_09SP
709800130131     c                   eval      WAtVA_09VP = wfvRCP + WAtVA_09VP
709900130131     C                   when      WFVmes = 10
710000130131     c                   eval      WAtVA_10SP = wfvNSP + WAtVA_10SP
710100130131     c                   eval      WAtVA_10VP = wfvRCP + WAtVA_10VP
710200130131     C                   when      WFVmes = 11
710300130131     c                   eval      WAtVA_11SP = wfvNSP + WAtVA_11SP
710400130131     c                   eval      WAtVA_11VP = wfvRCP + WAtVA_11VP
710500130131     C                   when      WFVmes = 12
710600130131     c                   eval      WAtVA_12SP = wfvNSP + WAtVA_12SP
710700130131     c                   eval      WAtVA_12VP = wfvRCP + WAtVA_12VP
710800130131     c                   endSL
710900130131     C*
711000130131     C                   eval      WAtVAT_SP  = wfvNSP + WAtVAT_SP
711100130131     C                   eval      WAtVAT_VP  = wfvRCP + WAtVAT_VP
711200130131     C*
711300130131     C                   END
711400130131     C*
711500130131     C*  cancella il record già incasellato in modo da non riutilizzarlo
711600130131     C*   più in seguito in sommatorie errate
711700130131     c                   delete    WFDEAV00
711800130131     C*
711900130131     c     k_deaV1       reade     wfDEAv1l
712000130131     C                   ENDDO
712100130131     C*
712200130131     C*
712300130131     C* lo fa solo la prima, primissima volta e basta
712400130131     c                   if        deve_scriverlo = 'S'
712500130131     c                   write     WADELVT0
712600130131     c                   end
712700130131     C*
712800130131     C*
712900000000     C                   ENDSR
713000130131     C*------------------------------------------------------------------------*
713100000000**   DMM - DECODIFICHE MESI DELL'ANNO
713200000000GEN  1
713300000000FEB  2
713400000000MAR  3
713500000000APR  4
713600000000MAG  5
713700000000GIU  6
713800000000LUG  7
713900000000AGO  8
714000000000SET  9
714100000000OTT  10
714200000000NOV  11
714300000000DIC  12
714400000000**   DST - DECODIFICHE STATISTICHE
714500000000PER FILIALE              1
714600000000PER AREA                 2
714700000000PER DIVISIONE            3
714800000000                         4
714900000000                         5
715000000000**   DRI - DECODIFICHE RIGA DI STAMPA
715100000000TUTTI                    1
715200000000TUTTE                    2
7153000000003
715400950208NUOVI                    4
715500950208ITALIA                   5
715600950208ESTERO                   6
715700950602ITALIA+ESTERO            7
715800020318ACQUIS.                  8
7159000000009
71600000000010
71610000000011
716200020220TOTALE                  12
716300000000CLIENTI                 13
716400000000SPEDIZ                  14
716500000000RICAVO                  15
716600000000DELTA                   16
716700000000NUOVO                   17
716800000000ACQUISITO               18
71690002022119
717000000000      C L I E N T I     20
717100000000 C O M M E R C I A L E  21
717200000000     F I L I A L E      22
717300000000       A R E A          23
717400000000   D I V I S I O N E    24
717500000000    G E N E R A L E     25
71760000082926
71770000082927
717800000829UNIFICATI:               28
717900000829PER CLIENTE              29
718000000829PER CLIENTE UNIFICANTE   30
718100020318**   DNTW - DECODIFICHE NETWORK
718200020318                          1
718300020318EEX                       2
718400020318DPD                       3
718500020318FED                       4
718600020318EUP                       5
