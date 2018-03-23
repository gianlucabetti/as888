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
002100160914     FTntbe01l  IF   E           K DISK
002200100129     FTISE94P   O    E             PRINTER OFLIND(*IN01) usropn
002300030925     FWFTISE93L UF A E           K DISK
002400000000     D*------------------------------------------------------------------------*
002500000000     D* SCHIERE
002600000000     D*------------------------------------------------------------------------*
002700000000     D*------------------
002800000000     D* DECODIFICA MESI DELL'ANNO
002900000000     D*------------------
003000000000     D DMM             S              3    DIM(12) CTDATA PERRCD(1)
003100000000     D*------------------
003200000000     D* DECODIFICA STATISTICHE
003300000000     D*------------------
003400000000     D DST             S             25    DIM(5) CTDATA PERRCD(1)
003500000000     D*------------------
003600000000     D* DECODIFICA RIGHE DI STAMPA
003700000000     D*------------------
003800000825     D DRI             S             24    DIM(30) CTDATA PERRCD(1)
003900020318     D*------------------
004000020318     D* DECODIFICA NETWORK
004100020318     D*------------------
004200020318     D DNTW            S              3    DIM(5) CTDATA PERRCD(1)
004300000000     D*------------------
004400000000     D* ORGANIGRAMMA
004500000000     D*------------------
004600971124     D FIL             S              3  0 DIM(500)                             *FILIALI
004700971124     D DFIL            S             20    DIM(500)
004800971124     D ARE             S              3  0 DIM(500)                             *AREE
004900971124     D DARE            S             20    DIM(500)
005000971124     D DIV             S              1    DIM(500)                             *DIVISIONI
005100971124     D DDIV            S             20    DIM(500)
005200020627     D POS             S              3    DIM(500)                             *NETWORK POSTE
005300000000     D*------------------
005400000000     D* COMMERCIALI
005500000000     D*------------------
005600150102     D AGE             S              7  0 DIM(9000)                            *CODICI
005700150102     D DAGE            S             20    DIM(9000)                            *DECODIFICHE
005800150102     D CAGE            S              3  0 DIM(9000)                            *PERCENTUALI COMPETE
005900960708     D*------------------
006000960708     D* CAUSALI BLOCCO CLIENTI
006100960708     D*------------------
006200960708     D CBL             S              3    DIM(50)                              *CODICI
006300960708     D DCBL            S             30    DIM(50)                              *DECODIFICHE
006400000000     D*------------------
006500000000     D* MESI DA ELABORARE
006600000000     D*------------------
006700000000     D MES             S              2  0 DIM(12)
006800000000     D*------------------
006900000000     D* ANNI DEI MESI DA ELABORARE
007000000000     D*------------------
007100990609     D ANN             S              4  0 DIM(12)
007200000000     D*------------------
007300160909     D* CATEGORIE CLIENTI                 (aggiunta ultima "E" alle 5 esistenti)
007400000000     D*------------------
007500160916     D ABC             S              1    DIM(10)                              QUALITA' CLIENTE
007600160916     D***** ABC             S              1    DIM(6)                              QUALITA' CLIENTE
007700160916     d Altri_ECBAT...                                                            Tutti i NON DIREZIONALI
007800160916     d                 S              3  0 INZ(9)                               sono gli elementi - 1
007900160916     d*****                 S              3  0 INZ(5)                               sono gli elementi - 1
008000160916     D*------------------
008100160914     d Nr_Elem_ABC...
008200160914     d                 S              3  0 INZ(%Elem(ABC))
008300160914     d Direzionali...
008400160914     d                 S              3  0 INZ(%Elem(ABC))
008500000000     D*------------------
008600000000     D* PERCENTUALE COMPETENZA
008700000000     D*------------------
008800000000     D PCF             S              1    DIM(3)
008900000000    ?D*-------------------
009000000000    ?D* S C H I E R E   N O N   M U L T I P L E
009100000000    ?D*-------------------
009200030924     D*------------------
009300030924     D* FLAG CONTATORE CLIENTE PER MESE
009400030924     D*------------------
009500000830     D SFCCL           S              1    DIM(12)                              FLAG CONTEGGIO
009600000000     D*------------------
009700000000     D* SCHIERE DI MEMORIZZAZIONE VALORI DI UN CLIENTE
009800000000     D*------------------
009900000000     D S1K             S             13  0 DIM(13)                              SPEDIZIONI UN CLIENT
010000081013     D KG1K            S             15  1 DIM(13)                              Pesi x UN CLIENT
010100020116     D R1K             S             19  5 DIM(13)                              RICAVI REALI UN CLIE
010200020116     D P1K             S             19  5 DIM(13)                              RICAVI PRESUNTI UN C
010300970822     D D1K             S              1    DIM(13)                              FLAG %SCOSTAMENTO DE
010400020712     D R1KP            S             19  5 DIM(12)                              RICAVI REALI UN CLIE
010500121217     D CLN1K           S              1    DIM(13)                              FLAG NUOVO ACQUISITO
010600070419     D*------------------
010700070419     D ap_S1K          S             13  0 DIM(12)                              SPEDIZIONI UN CLIENT
010800081013     D ap_KG1K         S             15  1 DIM(13)                              Pesi x UN CLIENT ANN
010900070419     D ap_R1K          S             19  5 DIM(12)                              RICAVI REALI UN CLIE
011000070419     D ap_P1K          S             19  5 DIM(12)                              RICAVI PRESUNTI UN C
011100091123     D*------------------
011200091124     D* SCHIERE PER TOTALI x NETWORK
011300091123     D*------------------
011400091124     D W1ntw           DS                  OCCURS(5) INZ
011500091124     d  c1ntw                         3
011600091124     D  S1ntw                        13  0 DIM(12)                              SPEDIZIONI UN CLIENT
011700091124     D  KG1ntw                       15  1 DIM(12)                              Pesi x UN CLIENT
011800091124     D  R1ntw                        19  5 DIM(12)                              RICAVI REALI UN CLIE
011900091124     D  P1ntw                        19  5 DIM(12)                              RICAVI PRESUNTI UN C
012000091124     D  D1ntw                         1    DIM(12)                              FLAG %SCOSTAMENTO DE
012100091124     D  R1Pntw                       19  5 DIM(12)                              RICAVI REALI UN CLIE
012200091125     D  TR1ntw                       15  0
012300091125     D  TP1ntw                       15  0
012400091123     D*------------------
012500091124     D* PER TOTALI anno Preced x NETWORK
012600091123     D*------------------
012700091124     D WAP1ntw         DS                  OCCURS(5) INZ
012800091124     d  ap_c1ntw                      3
012900091124     D  ap_S1ntw                     13  0 DIM(12)                              SPEDIZIONI UN CLIENT
013000091124     D  ap_KG1ntw                    15  1 DIM(12)                              Pesi x UN CLIENT
013100091124     D  ap_R1ntw                     19  5 DIM(12)                              RICAVI REALI UN CLIE
013200091124     D  ap_P1ntw                     19  5 DIM(12)                              RICAVI PRESUNTI UN C
013300091124     D  ap_D1ntw                      1    DIM(12)                              FLAG %SCOSTAMENTO DE
013400091124     D  ap_R1Pntw                    19  5 DIM(12)                              RICAVI REALI UN CLIE
013500091125     D  ap_TR1ntw                    15  0
013600091125     D  ap_TP1ntw                    15  0
013700091123     D*------------------
013800100202     D* SCHIERE PER TOTALI x NETWORK e TIPI SERVIZI sia Anno in corso che anno precedente
013900100202      *   legate dalla stessa chiave ossi NTW + Tipo servizio. ....quindi......
014000100129      *   le occorrenze sono date dalla moltiplica dei network x i tipi servizi possibili
014100100202      *   ossia 5(ntw) x 10(tipi serv.) = 50
014200100204     D*------------------
014300100204      * x comodità di calcolo, il codice NTW per l' ITALIA è blank e verra impostato a (ITA).
014400100129     D*------------------
014500100129     D W1ntwTS         DS                  OCCURS(50) INZ
014600100129     d  c1ntwTS                       3
014700100201     d  c1tpsTS                       1
014800100202      *-- Anno in corso
014900100202     D     S1ntwTS                   13  0 DIM(12)                              SPEDIZIONI UN CLIENT
015000100202     D     KG1ntwTS                  15  1 DIM(12)                              Pesi x UN CLIENT
015100100202     D     R1ntwTS                   19  5 DIM(12)                              RICAVI REALI UN CLIE
015200100202     D     P1ntwTS                   19  5 DIM(12)                              RICAVI PRESUNTI UN C
015300100202     D     D1ntwTS                    1    DIM(12)                              FLAG %SCOSTAMENTO DE
015400100202     D     R1PntwTS                  19  5 DIM(12)                              RICAVI REALI UN CLIE
015500100202     D     TR1ntwTS                  15  0
015600100202     D     TP1ntwTS                  15  0
015700100202      *-- Anno precedente
015800100202     D  ap_S1ntwTS                   13  0 DIM(12)                              SPEDIZIONI UN CLIENT
015900100202     D  ap_KG1ntwTS                  15  1 DIM(12)                              Pesi x UN CLIENT
016000100202     D  ap_R1ntwTS                   19  5 DIM(12)                              RICAVI REALI UN CLIE
016100100202     D  ap_P1ntwTS                   19  5 DIM(12)                              RICAVI PRESUNTI UN C
016200100202     D  ap_D1ntwTS                    1    DIM(12)                              FLAG %SCOSTAMENTO DE
016300100202     D  ap_R1PntwTS                  19  5 DIM(12)                              RICAVI REALI UN CLIE
016400100202     D  ap_TR1ntwTS                  15  0
016500100202     D  ap_TP1ntwTS                  15  0
016600100129     D*------------------
016700030924     D CNW             S             11  0 DIM(13)                              NUMERO C.NUOVI
016800030924     D SNW             S             13  0 DIM(13)                              SPEDIZIONI C.NUOVI
016900030924     D RNW             S             19  5 DIM(13)                              RICAVI REALI C.NUOVI
017000030924     D PNW             S             19  5 DIM(13)                              RICAVI PRESUNTI C.NU
017100030924     D CAW             S             11  0 DIM(13)                              NUMERO C.ACQUISITI
017200030924     D SAW             S             13  0 DIM(13)                              SPEDIZIONI C.ACQUISI
017300020116     D RAW             S             19  5 DIM(13)                              RICAVI REALI C.ACQ
017400020116     D PAW             S             19  5 DIM(13)                              RICAVI PRESUNTI C.AC
017500020221     D*------------------
017600020318     D* SCHIERE DI LAVORO PER TOTALI "D"
017700020221     D*------------------
017800020221     D CNWD            S             11  0 DIM(13)                              NUMERO C.NUOVI
017900020221     D SNWD            S             13  0 DIM(13)                              SPEDIZIONI C.NUOVI
018000020221     D RNWD            S             19  5 DIM(13)                              RICAVI REALI C.NUOVI
018100020221     D PNWD            S             19  5 DIM(13)                              RICAVI PRESUNTI C.NU
018200020221     D CAWD            S             11  0 DIM(13)                              NUMERO C.ACQUISITI
018300020221     D SAWD            S             13  0 DIM(13)                              SPEDIZIONI C.ACQUISI
018400020221     D RAWD            S             19  5 DIM(13)                              RICAVI REALI C.ACQ
018500020221     D PAWD            S             19  5 DIM(13)                              RICAVI PRESUNTI C.AC
018600000000     D*---
018700160913     D* MULTIPLA PER 13° TOT CLIENTI (TOTALI "A" + "B" + "C" + "T" + "E")
018800000000     D*---
018900951227     D NTW             S             11  0 DIM(2)                               TOTALE CLIENTI NEW
019000951227     D ATW             S             11  0 DIM(2)                               TOTALE CLIENTI ACQ
019100000000     D*------------------
019200000000     D* SCHIERE DI LAVORO PER RIGHE DETTAGLIO (CLI/SPE/RIC/DEL)
019300000000     D*------------------
019400000000     D CLW             S             13  0 DIM(13)                              NUMERO CLIENTI
019500000000     D SPW             S             13  0 DIM(13)                              SPEDIZIONI
019600020116     D RRW             S             19  5 DIM(13)                              RICAVO REALE
019700020116     D RPW             S             19  5 DIM(13)                              RICAVO PRESUNTO
019800000000     D*---
019900951227     D* NON MULTIPLA PER 13° TOT CLIENTI DETTAGLIO (CLI/SPE/RIC/DEL)
020000000000     D*---
020100951227     D CTW             S             11  0 DIM(2)                               13° TOTALE CLIENTI
020200000000     D*------------------
020300000000     D* SCHIERE DI LAVORO
020400000000     D*------------------
020500000000     D $CL             S             13  0 DIM(13)                              NUMERO CLIENTI
020600000000     D $SP             S             13  0 DIM(13)                              SPEDIZIONI
020700020116     D $RR             S             19  5 DIM(13)                              RICAVO REALE
020800020116     D $RP             S             19  5 DIM(13)                              RICAVO PRESUNTO
020900000000     D $PD             S              4  1 DIM(13)                              DELTA
021000970822     D $SD             S              1    DIM(13)                              SCOSTAMENTO DELTA
021100020116     D $VA             S             19  5 DIM(13)                              VALORE GENERICO
021200020701     D $PDF            S              4  1 DIM(13)                              SCOSTAMENTO % FATTUR
021300020319     D*------------------
021400020319     D* SCHIERA DI WRK X CONTEGGIO NETWORK SU ANNO "P" E ANNO "C" (LA DIMENSIONE=NUM. MESI x NUM. NE
021500020319     D*------------------
021600020319     D WSK1            S             12    DIM(130)
021700030925     d*---
021800030925     d* Variabili
021900030925     d*---
022000030925     d WNrr            S              7  0 INZ                                  *contatore righe
022100030926     d WTipStamp2      S              1    INZ                                  *STAMP2 chiamata da
022200000000     D*------------------------------------------------------------------------*
022300000000     D* INPUT
022400000000     D*------------------------------------------------------------------------*
022500000000     D KPJBA         E DS
022600000000     D*------------------
022700000000     D* NUMERO DEI BYTE IN PACKED:
022800000000     D*------------------
022900020116     D* CLIENTI                 (11:2) + 1 =  6,5  /  (6 X 13) =  78
023000020116     D* SPEDIZIONI              (13:2) + 1 =  7,5  /  (7 X 13) =  91
023100020116     D* RICAVI                  (19:2) + 1 =  9,5  /  (9 X 13) = 117
023200020116     D* TOTALE CLIENTI          (11:2) + 1 =  6,5  /  (6 X  2) =  12
023300000000     D*------------------
023400160909     D* DS MULTIPLA TOTALI GENERALI     - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
023500000000     D*------------------
023600160913     D ABCGEN          DS                  OCCURS(%Elem(ABC)) INZ
023700000000     D  CNG                    1     78P 0
023800000000     D                                     DIM(13)                              NUMERO C.NUOVI
023900000000     D  SNG                   79    169P 0
024000000000     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
024100020117     D  RNG                  170    286P 5
024200000000     D                                     DIM(13)                              RICAVI REALI C.NUOVI
024300020117     D  PNG                  287    403P 5
024400000000     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
024500020116     D  CAG                  404    481P 0
024600000000     D                                     DIM(13)                              NUMERO C.ACQUISITI
024700020116     D  SAG                  482    572P 0
024800000000     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
024900020117     D  RAG                  573    689P 5
025000000000     D                                     DIM(13)                              RICAVI REALI C.ACQ
025100020117     D  PAG                  690    806P 5
025200000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
025300000000     D*---
025400160909     D* DS MULTIPLA PER 13° TOT CLIENTI - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
025500000000     D*---
025600160913     D ABCG13          DS                  OCCURS(%Elem(ABC)) INZ
025700951227     D  NTG                    1     12P 0
025800951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
025900951227     D  ATG                   13     24P 0
026000951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
026100000000     D*------------------
026200160909     D* DS MULTIPLA TOTALI DIVISIONE    - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
026300000000     D*------------------
026400160913     D ABCDIV          DS                  OCCURS(%Elem(ABC)) INZ
026500020117     D  CND                    1     78P 0
026600020117     D                                     DIM(13)                              NUMERO C.NUOVI
026700020117     D  SND                   79    169P 0
026800020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
026900020117     D  RND                  170    286P 5
027000020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
027100020117     D  PND                  287    403P 5
027200020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
027300020117     D  CAD                  404    481P 0
027400020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
027500020117     D  SAD                  482    572P 0
027600020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
027700020117     D  RAD                  573    689P 5
027800020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
027900020117     D  PAD                  690    806P 5
028000000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
028100000000     D*---
028200160909     D* DS MULTIPLA PER 13° TOT CLIENTI - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
028300000000     D*---
028400160913     D ABCD13          DS                  OCCURS(%Elem(ABC)) INZ
028500951227     D  NTD                    1     12P 0
028600951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
028700951227     D  ATD                   13     24P 0
028800951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
028900000000     D*------------------
029000160909     D* DS MULTIPLA TOTALI AREA         - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
029100000000     D*------------------
029200160913     D ABCARE          DS                  OCCURS(%Elem(ABC)) INZ
029300020117     D  CNA                    1     78P 0
029400020117     D                                     DIM(13)                              NUMERO C.NUOVI
029500020117     D  SNA                   79    169P 0
029600020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
029700020117     D  RNA                  170    286P 5
029800020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
029900020117     D  PNA                  287    403P 5
030000020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
030100020117     D  CAA                  404    481P 0
030200020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
030300020117     D  SAA                  482    572P 0
030400020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
030500020117     D  RAA                  573    689P 5
030600020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
030700020117     D  PAA                  690    806P 5
030800000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
030900000000     D*---
031000160909     D* DS MULTIPLA PER 13° TOT CLIENTI - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
031100000000     D*---
031200160913     D ABCA13          DS                  OCCURS(%Elem(ABC)) INZ
031300951227     D  NTA                    1     12P 0
031400951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
031500951227     D  ATA                   13     24P 0
031600951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
031700000000     D*------------------
031800160909     D* DS MULTIPLA TOTALI FILIALE      - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
031900000000     D*------------------
032000160913     D ABCFIL          DS                  OCCURS(%Elem(ABC)) INZ
032100020117     D  CNF                    1     78P 0
032200020117     D                                     DIM(13)                              NUMERO C.NUOVI
032300020117     D  SNF                   79    169P 0
032400020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
032500020117     D  RNF                  170    286P 5
032600020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
032700020117     D  PNF                  287    403P 5
032800020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
032900020117     D  CAF                  404    481P 0
033000020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
033100020117     D  SAF                  482    572P 0
033200020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
033300020117     D  RAF                  573    689P 5
033400020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
033500020117     D  PAF                  690    806P 5
033600000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
033700000000     D*---
033800160909     D* DS MULTIPLA PER 13° TOT CLIENTI - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
033900000000     D*---
034000160913     D ABCF13          DS                  OCCURS(%Elem(ABC)) INZ
034100951227     D  NTF                    1     12P 0
034200951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
034300951227     D  ATF                   13     24P 0
034400951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
034500000000     D*------------------
034600160909     D* DS MULTIPLA TOTALI COMMERCIALE  - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
034700000000     D*------------------
034800160913     D ABCCOM          DS                  OCCURS(%Elem(ABC)) INZ
034900020117     D  CNC                    1     78P 0
035000020117     D                                     DIM(13)                              NUMERO C.NUOVI
035100020117     D  SNC                   79    169P 0
035200020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
035300020117     D  RNC                  170    286P 5
035400020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
035500020117     D  PNC                  287    403P 5
035600020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
035700020117     D  CAC                  404    481P 0
035800020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
035900020117     D  SAC                  482    572P 0
036000020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
036100020117     D  RAC                  573    689P 5
036200020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
036300020117     D  PAC                  690    806P 5
036400000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
036500000000     D*---
036600160909     D* DS MULTIPLA PER 13° TOT CLIENTI - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
036700000000     D*---
036800160913     D ABCC13          DS                  OCCURS(%Elem(ABC)) INZ
036900951227     D  NTC                    1     12P 0
037000951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
037100951227     D  ATC                   13     24P 0
037200951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
037300000000     D*------------------
037400160909     D* DS MULTIPLA TOTALI STAMPA       - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
037500000000     D*------------------
037600160913     D ABCSTA          DS                  OCCURS(%Elem(ABC)) INZ
037700020117     D  CNS                    1     78P 0
037800020117     D                                     DIM(13)                              NUMERO C.NUOVI
037900020117     D  SNS                   79    169P 0
038000020117     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
038100020117     D  RNS                  170    286P 5
038200020117     D                                     DIM(13)                              RICAVI REALI C.NUOVI
038300020117     D  PNS                  287    403P 5
038400020117     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
038500020117     D  CAS                  404    481P 0
038600020117     D                                     DIM(13)                              NUMERO C.ACQUISITI
038700020117     D  SAS                  482    572P 0
038800020117     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
038900020117     D  RAS                  573    689P 5
039000020117     D                                     DIM(13)                              RICAVI REALI C.ACQ
039100020117     D  PAS                  690    806P 5
039200000000     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
039300000000     D*---
039400160909     D* DS MULTIPLA PER 13° TOT CLIENTI - 6 CATEGORIE DI CLIENTI - (aggiunta la "E")
039500000000     D*---
039600160913     D ABCS13          DS                  OCCURS(%Elem(ABC)) INZ
039700951227     D  NTS                    1     12P 0
039800951227     D                                     DIM(2)                               TOTALE CLIENTI NEW
039900951227     D  ATS                   13     24P 0
040000951227     D                                     DIM(2)                               TOTALE CLIENTI ACQ
040100020318     D*------------------
040200020318     D* DS MULTIPLA TOTALI GENERALI     - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
040300020318     D*------------------
040400020318     D ABCGENN         DS                  OCCURS(5) INZ
040500020318     D  CNGN                   1     78P 0
040600020318     D                                     DIM(13)                              NUMERO C.NUOVI
040700020318     D  SNGN                  79    169P 0
040800020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
040900020318     D  RNGN                 170    286P 5
041000020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
041100020318     D  PNGN                 287    403P 5
041200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
041300020318     D  CAGN                 404    481P 0
041400020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
041500020318     D  SAGN                 482    572P 0
041600020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
041700020318     D  RAGN                 573    689P 5
041800020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
041900020318     D  PAGN                 690    806P 5
042000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
042100020318     D*---
042200020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
042300020318     D*---
042400020318     D ABCG13N         DS                  OCCURS(5) INZ
042500020318     D  NTGN                   1     12P 0
042600020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
042700020318     D  ATGN                  13     24P 0
042800020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
042900020318     D*------------------
043000020318     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
043100020318     D*------------------
043200020318     D ABCDIVN         DS                  OCCURS(5) INZ
043300020318     D  CNDN                   1     78P 0
043400020318     D                                     DIM(13)                              NUMERO C.NUOVI
043500020318     D  SNDN                  79    169P 0
043600020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
043700020318     D  RNDN                 170    286P 5
043800020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
043900020318     D  PNDN                 287    403P 5
044000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
044100020318     D  CADN                 404    481P 0
044200020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
044300020318     D  SADN                 482    572P 0
044400020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
044500020318     D  RADN                 573    689P 5
044600020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
044700020318     D  PADN                 690    806P 5
044800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
044900020318     D*---
045000020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
045100020318     D*---
045200020318     D ABCD13N         DS                  OCCURS(5) INZ
045300030924     D  NTDN                   1     12P 0
045400030924     D                                     DIM(2)                               TOTALE CLIENTI NEW
045500030924     D  ATDN                  13     24P 0
045600030924     D                                     DIM(2)                               TOTALE CLIENTI ACQ
045700030924     D*------------------
045800020318     D* DS MULTIPLA TOTALI AREA         - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
045900020318     D*------------------
046000020318     D ABCAREN         DS                  OCCURS(5) INZ
046100020318     D  CNAN                   1     78P 0
046200020318     D                                     DIM(13)                              NUMERO C.NUOVI
046300020318     D  SNAN                  79    169P 0
046400020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
046500020318     D  RNAN                 170    286P 5
046600020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
046700020318     D  PNAN                 287    403P 5
046800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
046900020318     D  CAAN                 404    481P 0
047000020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
047100020318     D  SAAN                 482    572P 0
047200020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
047300020318     D  RAAN                 573    689P 5
047400020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
047500020318     D  PAAN                 690    806P 5
047600020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
047700020318     D*---
047800020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
047900020318     D*---
048000020318     D ABCA13N         DS                  OCCURS(5) INZ
048100020318     D  NTAN                   1     12P 0
048200020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
048300020318     D  ATAN                  13     24P 0
048400020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
048500020318     D*------------------
048600020318     D* DS MULTIPLA TOTALI FILIALE      - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
048700020318     D*------------------
048800020318     D ABCFILN         DS                  OCCURS(5) INZ
048900020318     D  CNFN                   1     78P 0
049000020318     D                                     DIM(13)                              NUMERO C.NUOVI
049100020318     D  SNFN                  79    169P 0
049200020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
049300020318     D  RNFN                 170    286P 5
049400020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
049500020318     D  PNFN                 287    403P 5
049600020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
049700020318     D  CAFN                 404    481P 0
049800020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
049900020318     D  SAFN                 482    572P 0
050000020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
050100020318     D  RAFN                 573    689P 5
050200020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
050300020318     D  PAFN                 690    806P 5
050400020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
050500020318     D*---
050600020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
050700020318     D*---
050800020318     D ABCF13N         DS                  OCCURS(5) INZ
050900020318     D  NTFN                   1     12P 0
051000020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
051100020318     D  ATFN                  13     24P 0
051200020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
051300020318     D*------------------
051400020318     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
051500020318     D*------------------
051600020318     D ABCCOMN         DS                  OCCURS(5) INZ
051700020318     D  CNCN                   1     78P 0
051800020318     D                                     DIM(13)                              NUMERO C.NUOVI
051900020318     D  SNCN                  79    169P 0
052000020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
052100020318     D  RNCN                 170    286P 5
052200020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
052300020318     D  PNCN                 287    403P 5
052400020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
052500020318     D  CACN                 404    481P 0
052600020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
052700020318     D  SACN                 482    572P 0
052800020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
052900020318     D  RACN                 573    689P 5
053000020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
053100020318     D  PACN                 690    806P 5
053200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
053300020318     D*---
053400020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
053500020318     D*---
053600020318     D ABCC13N         DS                  OCCURS(5) INZ
053700020318     D  NTCN                   1     12P 0
053800020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
053900020318     D  ATCN                  13     24P 0
054000020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
054100020318     D*------------------
054200020318     D* DS MULTIPLA TOTALI STAMPA       - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
054300020318     D*------------------
054400020318     D ABCSTAN         DS                  OCCURS(5) INZ
054500020318     D  CNSN                   1     78P 0
054600020318     D                                     DIM(13)                              NUMERO C.NUOVI
054700020318     D  SNSN                  79    169P 0
054800020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
054900020318     D  RNSN                 170    286P 5
055000020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
055100020318     D  PNSN                 287    403P 5
055200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
055300020318     D  CASN                 404    481P 0
055400020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
055500020318     D  SASN                 482    572P 0
055600020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
055700020318     D  RASN                 573    689P 5
055800020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
055900020318     D  PASN                 690    806P 5
056000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
056100020318     D*---
056200020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
056300020318     D*---
056400020318     D ABCS13N         DS                  OCCURS(5) INZ
056500020318     D  NTSN                   1     12P 0
056600020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
056700020318     D  ATSN                  13     24P 0
056800020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
056900020318     D*------------------
057000020318     D* DS MULTIPLA TOTALI GENERALI     - 5 NETWORK X CATEGORIE CLIENTI "D" -
057100020318     D*------------------
057200020318     D ABCGENND        DS                  OCCURS(5) INZ
057300020318     D  CNGND                  1     78P 0
057400020318     D                                     DIM(13)                              NUMERO C.NUOVI
057500020318     D  SNGND                 79    169P 0
057600020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
057700020318     D  RNGND                170    286P 5
057800020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
057900020318     D  PNGND                287    403P 5
058000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
058100020318     D  CAGND                404    481P 0
058200020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
058300020318     D  SAGND                482    572P 0
058400020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
058500020318     D  RAGND                573    689P 5
058600020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
058700020318     D  PAGND                690    806P 5
058800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
058900020318     D*---
059000020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
059100020318     D*---
059200020318     D ABCG13ND        DS                  OCCURS(5) INZ
059300020318     D  NTGND                  1     12P 0
059400020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
059500020318     D  ATGND                 13     24P 0
059600020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
059700020318     D*------------------
059800020318     D* DS MULTIPLA TOTALI DIVISIONE    - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
059900020318     D*------------------
060000020318     D ABCDIVND        DS                  OCCURS(5) INZ
060100020318     D  CNDND                  1     78P 0
060200020318     D                                     DIM(13)                              NUMERO C.NUOVI
060300020318     D  SNDND                 79    169P 0
060400020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
060500020318     D  RNDND                170    286P 5
060600020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
060700020318     D  PNDND                287    403P 5
060800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
060900020318     D  CADND                404    481P 0
061000020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
061100020318     D  SADND                482    572P 0
061200020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
061300020318     D  RADND                573    689P 5
061400020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
061500020318     D  PADND                690    806P 5
061600020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
061700020318     D*---
061800020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
061900020318     D*---
062000020318     D ABCD13ND        DS                  OCCURS(5) INZ
062100020318     D  NTDND                  1     12P 0
062200020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
062300020318     D  ATDND                 13     24P 0
062400020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
062500020318     D*------------------
062600020318     D* DS MULTIPLA TOTALI AREA         - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
062700020318     D*------------------
062800020318     D ABCAREND        DS                  OCCURS(5) INZ
062900020318     D  CNAND                  1     78P 0
063000020318     D                                     DIM(13)                              NUMERO C.NUOVI
063100020318     D  SNAND                 79    169P 0
063200020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
063300020318     D  RNAND                170    286P 5
063400020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
063500020318     D  PNAND                287    403P 5
063600020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
063700020318     D  CAAND                404    481P 0
063800020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
063900020318     D  SAAND                482    572P 0
064000020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
064100020318     D  RAAND                573    689P 5
064200020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
064300020318     D  PAAND                690    806P 5
064400020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
064500020318     D*---
064600020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
064700020318     D*---
064800020318     D ABCA13ND        DS                  OCCURS(5) INZ
064900020318     D  NTAND                  1     12P 0
065000020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
065100020318     D  ATAND                 13     24P 0
065200020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
065300020318     D*------------------
065400020318     D* DS MULTIPLA TOTALI FILIALE      - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
065500020318     D*------------------
065600020318     D ABCFILND        DS                  OCCURS(5) INZ
065700020318     D  CNFND                  1     78P 0
065800020318     D                                     DIM(13)                              NUMERO C.NUOVI
065900020318     D  SNFND                 79    169P 0
066000020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
066100020318     D  RNFND                170    286P 5
066200020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
066300020318     D  PNFND                287    403P 5
066400020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
066500020318     D  CAFND                404    481P 0
066600020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
066700020318     D  SAFND                482    572P 0
066800020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
066900020318     D  RAFND                573    689P 5
067000020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
067100020318     D  PAFND                690    806P 5
067200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
067300020318     D*---
067400030924     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
067500030924     D*---
067600030924     D ABCF13ND        DS                  OCCURS(5) INZ
067700030924     D  NTFND                  1     12P 0
067800020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
067900020318     D  ATFND                 13     24P 0
068000020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
068100020318     D*------------------
068200020318     D* DS MULTIPLA TOTALI COMMERCIALE  - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
068300020318     D*------------------
068400020318     D ABCCOMND        DS                  OCCURS(5) INZ
068500020318     D  CNCND                  1     78P 0
068600020318     D                                     DIM(13)                              NUMERO C.NUOVI
068700020318     D  SNCND                 79    169P 0
068800020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
068900020318     D  RNCND                170    286P 5
069000020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
069100020318     D  PNCND                287    403P 5
069200020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
069300020318     D  CACND                404    481P 0
069400020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
069500020318     D  SACND                482    572P 0
069600020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
069700020318     D  RACND                573    689P 5
069800020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
069900020318     D  PACND                690    806P 5
070000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
070100020318     D*---
070200020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
070300020318     D*---
070400020318     D ABCC13ND        DS                  OCCURS(5) INZ
070500020318     D  NTCND                  1     12P 0
070600020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
070700020318     D  ATCND                 13     24P 0
070800020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
070900020318     D*------------------
071000020318     D* DS MULTIPLA TOTALI STAMPA       - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
071100020318     D*------------------
071200020318     D ABCSTAND        DS                  OCCURS(5) INZ
071300020318     D  CNSND                  1     78P 0
071400020318     D                                     DIM(13)                              NUMERO C.NUOVI
071500020318     D  SNSND                 79    169P 0
071600020318     D                                     DIM(13)                              SPEDIZIONI C.NUOVI
071700020318     D  RNSND                170    286P 5
071800020318     D                                     DIM(13)                              RICAVI REALI C.NUOVI
071900020318     D  PNSND                287    403P 5
072000020318     D                                     DIM(13)                              RICAVI PRESUNTI C.NU
072100020318     D  CASND                404    481P 0
072200020318     D                                     DIM(13)                              NUMERO C.ACQUISITI
072300020318     D  SASND                482    572P 0
072400020318     D                                     DIM(13)                              SPEDIZIONI C.ACQUISI
072500020318     D  RASND                573    689P 5
072600020318     D                                     DIM(13)                              RICAVI REALI C.ACQ
072700020318     D  PASND                690    806P 5
072800020318     D                                     DIM(13)                              RICAVI PRESUNTI C.AC
072900020318     D*---
073000020318     D* DS MULTIPLA PER 13° TOT CLIENTI - 5 NETWORK X CATEGORIE CLIENTI NN "D" -
073100020318     D*---
073200020318     D ABCS13ND        DS                  OCCURS(5) INZ
073300020318     D  NTSND                  1     12P 0
073400020318     D                                     DIM(2)                               TOTALE CLIENTI NEW
073500020318     D  ATSND                 13     24P 0
073600020318     D                                     DIM(2)                               TOTALE CLIENTI ACQ
073700000828     D*------------------
073800000828     D* CLIENTI FIGLI IN STAMPA
073900000828     D*------------------
074000051031     C**** ridotta da 19 a 17 per far spazio ai flags x individuare i codici bloccati
074100051031     D***** SKSC            S              7  0 DIM(19)                              *CLIENTE
074200051031     D SKSC            S              7  0 DIM(17)                              *CLIENTE
074300051031     D SKbl            S              1    DIM(17)                              *Bloccato
074400120125      *---
074500120125     D*------------------
074600120125     D* DS per reperire la DATA del BLOCCO CONTABILE
074700120125     D*------------------
074800120125     D TRULBLCDS     E DS
074900120125      *---
075000120125     D Cod_Blocco_Ultimo...
075100120125     d                 s              3a
075200120125     D Data_Blocco_Ultimo...
075300120125     d                 s              8  0
075400120125     D Data_Prima_Fat_6...
075500120125     d                 s              6  0
075600120131     D Data_Prima_Fat_8...
075700120131     d                 s              8  0
075800120131     D CLP_Prima_Fat_8...
075900120131     d                 s              8  0
076000000000     D*------------------
076100000000     D* PARAMETRI FILTRO
076200000000     D*------------------
076300000000     D PARAM           DS
076400990609     D  PARDAT                 1      6  0
076500990609     D  PARAAI                 1      4  0
076600990609     D  PARMMI                 5      6  0
076700990609     D  PARDA2                 7     12  0
076800990609     D  PARAAF                 7     10  0
076900990609     D  PARMMF                11     12  0
077000990609     D  PARDCK                13     13
077100990609     D  PARDCC                14     14
077200990609     D  PARDCF                15     15
077300990609     D  PARFIC                16     18  0
077400990609     D  PARARC                19     21  0
077500990609     D  PARDIC                22     22
077600990609     D  PARDFF                23     23
077700990609     D  PARCOF                24     30  0
077800990609     D  PARDIF                31     31
077900990609     D  PARSTA                32     32
078000000825     D  PARSCE                33     33
078100990609     D  PARTSP                34     34
078200990609     D  PARIOE                35     35
078300990609     D  PARCLV                36     36
078400990609     D  PARDPT                37     42  0
078500990609     D  PARAPI                37     40  0
078600990609     D  PARMPI                41     42  0
078700990609     D  PARDP2                43     48  0
078800990609     D  PARAFP                43     46  0
078900990609     D  PARMFP                47     48  0
079000990609     D  PARSPD                49     52  1
079100990609     D  PARPDD                53     56  1
079200990609     D  PARPDA                57     60  1
079300000707     D  PARPOS                61     61
079400000825     D  PARCRE                62     62
079500010618     D  PARCL1                63     63
079600010618     D  PARCL2                64     64
079700010618     D  PARSIN                65     65
079800020404     D  PARCL3                66     66
079900020404     D  PARCL4                67     67
080000020404     D  PARNTW                68     68
080100020701     D  PARDFD                69     72  1
080200020701     D  PARDFA                73     76  1
080300020701     D  PARDFDP               77     80  1
080400020701     D  PARDFAP               81     84  1
080500030311     D  PARFPC                85     85
080600030925     D  PARWPC                86     86
080700030925     D  PARWCP                87     87
080800100129     D  PARWfdpv              88     88
080900100129      * Stat. Clienti Persi (Trimestrale x il momento)                          lancio trimestre(T)
081000100129     D  PARCLPtmp             89     89                                         M/B/T/Q/S
081100100129     D  PARStampa             90     90                                         *blk/N
081200100129     D  PARStaComm            91     91                                         *blk/N
081300000707     D*------------------
081400000707     D* DS LETTURA CAMPO ORGDE3 DI AZORG00F
081500000707     D*------------------
081600000707     D OG143         E DS
081700000000     D*------------------
081800000000     D* DS AGENTI COMMERCIALI
081900000000     D*------------------
082000140612     D**** DS01          E DS
082100000000     D*------------------
082200000000     D* DS SETTORI MERCEOLOGICI
082300000000     D*------------------
082400951006     D DS1L          E DS
082500090610     D*------------------
082600090610     D* Descrizioni tipi servizio
082700090610     D*------------------
082800090610     D DS5e          E DS
082900100129     D Quanti_Tserv    S              3s 0
083000100129     D Tserv           S              1    DIM(10)
083100100129     D DTserv          S             25    DIM(10)
083200000828     D*------------------
083300000828     D* DS DI PROCEDURA
083400000828     D*------------------
083500000828     D BS10DS        E DS                  EXTNAME(TIBS10DS)
083600000828     D  SKSC11                21   5520  0 DIM(500)                             *CLIENTI FIGLI
083700020220     D*------------------
083800020220     D* DS IMPORTANZA CLIENTI
083900020220     D*------------------
084000020220     D DSIC          E DS
084100160914     D*------------------
084200160914     D* DS per attivazioni posticipate a modifiche interne al pgm
084300160914     D*------------------
084400160914     D DVPOTISE93    E DS
084500980406     D*------------------
084600980406     D* DS REPERIMENTO DATI UTENTE
084700980406     D*-------------------
084800980406     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
084900980406     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
085000980406     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
085100980406     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
085200980406     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
085300951219     D*------------------
085400951219     D* COMPOSIZIONE DATA SALDI
085500951219     D*------------------
085600951219     D                 DS
085700951219     D  WFDANN                 1      4  0                                      *ANNO
085800951219     D  WFDMES                 5      6  0                                      *MESE
085900951219     D  WFDDAT                 1      6  0                                      *DATA
086000000828     D*------------------
086100000828     D* DS CONTROLLO DATA (8)
086200000828     D*------------------
086300000828     D WLBDA8          DS                  INZ
086400000828     D  G08DAT                 1      8  0
086500000828     D  G08INV                 9     16  0
086600000828     D  G08ERR                17     17
086700000828     D  G08TGI                18     22  0
086800020319     D*------------------
086900020319     D* DS DI RIEMPIMENTO SCHIERA DI WRK WSK1
087000020319     D*------------------
087100020319     D DSWSK1          DS                  INZ
087200020319     D  WFDCLI                        7  0
087300020319     D  MM                            2  0
087400020319     D  WFDNTW                        3
087500160713      *--------------------------------------
087600160713     d digits          c                   '0123456789'
087700000000     C*------------------------------------------------------------------------*
087800000000     C*?MAIN LINE
087900000000     C*------------------------------------------------------------------------*
088000000000     C*
088100000000     C* STAMPA PARAMETRI FILTRO
088200000000     C                   EXSR      STAPAR
088300000000     C*
088400000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
088500000000     C                   EXSR      SETFIL
088600000000     C*
088700000000     C* CICLO FINO A FINE FILE DI LAVORO
088800000000DO  1C     $FINE         DOWEQ     '0'
088900000000     C*
089000000000     C* OPERAZIONI PER NUOVA DIVISIONE
089100000000     C                   EXSR      NEWDIV
089200000000     C*
089300000000     C* CICLO FINO A ROTTURA DI DIVISIONE
089400000000DO  2C     $FINE         DOWEQ     '0'
089500000000     C     WFDDIV        ANDEQ     DEPDIV
089600000000     C*
089700000000     C* OPERAZIONI PER NUOVA AREA
089800000000     C                   EXSR      NEWARE
089900000000     C*
090000000000     C* CICLO FINO A ROTTURA DI AREA
090100000000DO  3C     $FINE         DOWEQ     '0'
090200000000     C     WFDDIV        ANDEQ     DEPDIV
090300000000     C     WFDARE        ANDEQ     DEPARE
090400000000     C*
090500000000     C* OPERAZIONI PER NUOVA FILIALE
090600000000     C                   EXSR      NEWFIL
090700000000     C*
090800000000     C* CICLO FINO A ROTTURA DI FILIALE
090900000000DO  4C     $FINE         DOWEQ     '0'
091000000000     C     WFDDIV        ANDEQ     DEPDIV
091100000000     C     WFDARE        ANDEQ     DEPARE
091200000000     C     WFDFIL        ANDEQ     DEPFIL
091300000000     C*
091400000000     C* OPERAZIONI PER NUOVO AGENTE
091500000000     C                   EXSR      NEWAGE
091600000000     C*
091700000000     C* CICLO FINO A ROTTURA DI AGENTE
091800000000DO  5C     $FINE         DOWEQ     '0'
091900000000     C     WFDDIV        ANDEQ     DEPDIV
092000000000     C     WFDARE        ANDEQ     DEPARE
092100000000     C     WFDFIL        ANDEQ     DEPFIL
092200000000     C     WFDAGE        ANDEQ     DEPAGE
092300000000     C*
092400120131     c* OPERAZIONI PER NUOVO CLIENTE
092500000000     C                   EXSR      NEWKSC
092600000000     C*
092700000000     C* CICLO FINO A ROTTURA DI CLIENTE
092800000000DO  6C     $FINE         DOWEQ     '0'
092900000000     C     WFDDIV        ANDEQ     DEPDIV
093000000000     C     WFDARE        ANDEQ     DEPARE
093100000000     C     WFDFIL        ANDEQ     DEPFIL
093200000000     C     WFDAGE        ANDEQ     DEPAGE
093300030924     C     WFDCLI        ANDEQ     DEPCLI
093400030924     C*
093500030924     C* OPERAZIONI PER PASSAGGIO LETTURA SALDI PRECEDENTI --> SALDI CORRENTI
093600030924     C                   EXSR      NEWPPC
093700030924     C* MEMORIZZA I SALDI
093800030924     C                   EXSR      MEMSAL
093900000000     C*
094000000000     C* LEGGE IL RECORD SUCCESSIVO
094100000000     C                   EXSR      LETFIL
094200000000     C*
094300000000E   6C                   ENDDO
094400000000     C*
094500000000     C* STAMPA IL TOTALE CLIENTE
094600000000     C                   EXSR      STAKSC
094700000000E   5C                   ENDDO
094800000000     C*
094900000000     C* STAMPA IL TOTALE COMMERCIALE
095000000000     C                   EXSR      STAAGE
095100000000E   4C                   ENDDO
095200000000     C*
095300000000     C* STAMPA IL TOTALE FILIALE
095400000000     C                   EXSR      STAFIL
095500000000E   3C                   ENDDO
095600000000     C*
095700000000     C* STAMPA IL TOTALE AREA
095800000000     C                   EXSR      STAARE
095900000000E   2C                   ENDDO
096000000000     C*
096100000000     C* STAMPA IL TOTALE DIVISIONE
096200000000     C                   EXSR      STADIV
096300000000E   1C                   ENDDO
096400000000     C*
096500000000     C* STAMPA IL TOTALE GENERALE
096600000000     C                   EXSR      STAGEN
096700000000     C*
096800000000     C* STAMPA RIGA FINE STAMPA
096900100129     c                   if        parSTAMPA <> 'N'
097000030311     C                   WRITE     SE94LE
097100020116     C                   WRITE     SE94FI
097200100129     c                   end
097300980710     C* OPERAZIONI FINALI
097400980710     C                   EXSR      FINSR
097500000000     C*
097600000000     C                   SETON                                        LR
097700000000     C*------------------------------------------------------------------------*
097800000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA DEL WORK FILE
097900000000     C*------------------------------------------------------------------------*
098000000000     C     SETFIL        BEGSR
098100980529     C*
098200000000     C                   MOVEL     *BLANKS       KWDDIV                         *DIVISIONE
098300000000     C                   Z-ADD     *ZEROS        KWDARE                         *AREA
098400000000     C                   Z-ADD     *ZEROS        KWDFIL                         *FILIALE
098500000000     C                   Z-ADD     *ZEROS        KWDAGE                         *COMMERCIALE
098600960710     C                   MOVEL     *BLANKS       KWDCLV                         *CODICE QUALITA'
098700000825     C                   Z-ADD     *ZEROS        KWDCLI                         *CLIENTE
098800000000     C                   Z-ADD     *ZEROS        KWDANN                         *ANNO
098900000000     C                   Z-ADD     *ZEROS        KWDMES                         *MESE
099000100127     C     KEYDEL        SETLL     WFDEA01L                           99        *FINE ARCHIVIO
099100951219IF  1C     *IN99         IFEQ      *ON
099200000000     C                   MOVEL     '1'           $FINE
099300000000X   1C                   ELSE
099400000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
099500000000E   1C                   ENDIF
099600000000     C*
099700000000     C                   ENDSR
099800000000     C*------------------------------------------------------------------------*
099900000000     C*?LETFIL - LETTURA PROSSIMO RECORD DEL WORK FILE
100000000000     C*------------------------------------------------------------------------*
100100000000     C     LETFIL        BEGSR
100200000000     C*
100300000000     C* LEGGO FINO A:
100400000000     C*     - FINE FILE
100500000000     C*     - TROVATO RECORD VALIDO
100600000000     C                   MOVEL     'N'           WRECOK
100700000000DO  1C     $FINE         DOWEQ     '0'
100800000000     C     WRECOK        ANDEQ     'N'
100900000000     C*
101000000000     C* LETTURE SUCCESSIVE ALLA PRIMA
101100100127     C                   READ      WFDEA01L                               99
101200000000     C*
101300951219IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
101400000000     C                   MOVEL     '1'           $FINE
101500000000X   2C                   ELSE
101600000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
101700000000E   2C                   ENDIF
101800000000     C*
101900000000E   1C                   ENDDO
102000000000     C*
102100000000     C                   ENDSR
102200000000     C*------------------------------------------------------------------------*
102300000000     C*?CHKREC - CONTROLLA VALIDITA' DEL RECORD DEL WORK FILE
102400000000     C*------------------------------------------------------------------------*
102500000000     C     CHKREC        BEGSR
102600000000     C*
102700000000     C                   MOVEL     'S'           WRECOK            1            *RECORD VALIDO
102800000000     C*
102900990609IF  1C     WFDDAT        IFGE      PARDPT
103000990609     C     WFDDAT        ANDLE     PARDP2
103100951219     C                   MOVEL     'P'           TIPANN                         *PERIODO PRECEDENTE
103200071123     C*
103300071123     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
103400071123     C* a cambio di codice cliente. Essendo comunuque il periodo Precedente deve
103500071123     C* essere considerato ACQUISITO E NON NUOVO e SE LO E' RIMANE tale anche
103600071123     C* PER TUTTO IL PERIODO CORRENTE
103700071123IF  2C                   If        FlaPAS = '0'
103800071123     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
103900071123     C                   clear                   DESCLN                         *NUOVO O NON NUOVO
104000071123E   2C                   EndIF
104100071123      *
104200951219E   1C                   ENDIF
104300071123      *
104400990609IF  1C     WFDDAT        IFGE      PARDAT
104500990609     C     WFDDAT        ANDLE     PARDA2
104600951219     C                   MOVEL     'C'           TIPANN                         *PERIODO CORRENTE
104700000000     C*
104800000000     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
104900000000     C* VEDO SE IL CLIENTE E' NUOVO (PER FORZA IL PRIMO SALDO); SE LO E' RIMANE
105000000000     C* TALE PER TUTTO IL PERIODO CORRENTE
105100071123     C* Attenzione: poichè se il codice cliente precedentemente letto aveva solo
105200071123     C*  solo dati su Anno precedente e il cliente appena letto è nuovo per anno
105300071123     C*  in corso sovrappone il Nuovo/Acquisito su DESCLN poichè si ritrovava il FLAPAS = '0'
105400071123     C*  per proteggersi da tale problema occorre controllare che il test venga fatto
105500071123     C*  quando non si è alivello di rottura livello cliente.
105600000000IF  2C     FLAPAS        IFEQ      '0'
105700071123     c                   if        wfdCLI = depCLI
105800000000     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
105900000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O NON NUOVO
106000071123     c                   end
106100000000E   2C                   ENDIF
106200070822      *
106300070822      *  Può esistere il caso che il cliente sia nuovo nel mese di febbraio x ESTERO
106400070822      *  ed abbia fatturato Italia solo in giugno.
106500070822      *  Lanciando la stampa come Italia l'informazione letta, ossia il record di giugno, non
106600070822      *  avrà "Y" come NUOVO ma avrà "S".
106700070822      *  Il programma necessita della "Y" nei suoi test per considerare il cliente come NUOVO.
106800070822      *   --> Occorre memorizzarsi qui comunque cosa vale il flag del primo record letto sia
106900070822      *   che sia Italia/Estero per poi forzarlo nella routine di decodifica del cliente.
107000070822IF  2C     wfdCLI        ifne      sav_depCLI
107100070822IF  2C     wfdIOE        andne     parIOE
107200070822IF  2C     parIOE        andne     *blank
107300070822      *
107400070822      * lo salva per poterlo forzare in seguito
107500070822     c                   clear                   sav_desCLN_Y
107600070822IF  2C                   if        sav_depCLI <> wfdCLI
107700070822IF  2C                                       and wfdCLN ='Y'
107800070822     c                   move      'Y'           sav_desCLN_Y
107900070822     c                   end
108000070822      *
108100070822     c                   eval      sav_depCLI  =  wfdCLI
108200070822     c                   end
108300070822      *
108400000000E   1C                   ENDIF
108500070822      *
108600951018     C*--------------------
108700951018     C* SELEZIONI
108800951018     C*--------------------
108900951018     C* SE FILIALE CLIENTE <> FILIALE RICHIESTA, ESCLUDO
109000951018IF  1C     PARFIC        IFGT      *ZEROS                                       *DELTA FILIALE
109100951018     C     WFDFIL        ANDNE     PARFIC
109200951018     C                   MOVEL     'N'           WRECOK
109300951018E   1C                   ENDIF
109400951018     C*
109500951018     C* SE AREA CLIENTE <> AREA RICHIESTA, ESCLUDO
109600951018IF  1C     PARARC        IFGT      *ZEROS                                       *DELTA FILIALE
109700951018     C     WFDARE        ANDNE     PARARC
109800951018     C                   MOVEL     'N'           WRECOK
109900951018E   1C                   ENDIF
110000951018     C*
110100030924     C* SE DIVISIONE CLIENTE <> DIVISIONE RICHIESTA, ESCLUDO
110200951018IF  1C     PARDIC        IFNE      *BLANKS                                      *DELTA FILIALE
110300030924     C     WFDDIV        ANDNE     PARDIC
110400030924     C                   MOVEL     'N'           WRECOK
110500030924E   1C                   ENDIF
110600030924     C*
110700951018     C* SE CODICE QUALITA' CLIENTE <> CODICE QUALITA' RICHIESTO, ESCLUDO
110800010618IF  1C                   IF        (PARCLV <> *blanks and WFDCLV = PARCLV) or
110900010618     C                             (PARCL1 <> *blanks and WFDCLV = PARCL1) or
111000030924     C                             (PARCL2 <> *blanks and WFDCLV = PARCL2) or
111100160909     C                             (PARCL3 <> *blanks and WFDCLV = PARCL3) or
111200160909     C                             (PARCL4 <> *blanks and WFDCLV = PARCL4) or
111300030924     C                             (PARCLV = *blanks  and
111400030924     C                              PARCL1 = *blanks  and
111500160909     C                              PARCL2 = *blanks  and
111600160909     C                              PARCL3 = *blanks  and
111700160912     C                              PARCL4 = *blanks)
111800030924     C                   ELSE
111900951018     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
112000951018E   1C                   ENDIF
112100000000     C*
112200951018     C* SE TIPO DATI STATISTCA ITALIA/ESTERO <> TIPO DATI RICHIESTO, ESCLUDO
112300950210IF  1C     PARIOE        IFNE      *BLANKS
112400950210     C     PARIOE        ANDNE     WFDIOE
112500950208     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
112600950210E   1C                   ENDIF
112700000707     C*
112800000707     C* SE DATI NO POSTE ESCLUDE LE FILIALI POSTE, E VICEVERSA
112900000707IF  1C     WRECOK        IFEQ      'S'                                          *ANCORA VALIDO
113000000707     C                   SETOFF                                           28
113100000707     C                   Z-ADD     1             I                 5 0
113200000707     C     WFDFIL        LOOKUP    FIL(I)                                 28
113300000707IF  2C     PARPOS        IFEQ      'N'                                          *NO DATI POSTE
113400020627IF  3C     POS(I)        IFEQ      'PPT'                                         FIL = POSTE
113500000707     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
113600000711E   2C                   ENDIF
113700000711X   2C                   ELSE                                                   *SI DATI POSTE
113800020627IF  3C     POS(I)        IFNE      'PPT'                                         FIL <> POSTE
113900000707     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
114000000711E   3C                   ENDIF
114100000711E   2C                   ENDIF
114200000707E   1C                   ENDIF
114300980528     C*
114400000000     C                   ENDSR
114500000000     C*------------------------------------------------------------------------*
114600000000     C*?NEWDIV - OPERAZIONI PER NUOVA DIVISIONE
114700000000     C*------------------------------------------------------------------------*
114800000000     C     NEWDIV        BEGSR
114900020220     C*
115000020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
115100020220     C                   MOVEL     '0'           FLGSIDD
115200000000     C*
115300000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
115400000000     C                   MOVEL     '0'           PASDIV
115500000000     C*
115600000000     C* MEMORIZZA LA NUOVA DIVISIONE IN UN DEPOSITO
115700000000     C                   MOVEL     WFDDIV        DEPDIV
115800000000     C*
115900000000     C* DECODIFICA IL CODICE DELLA DIVISIONE
116000000000     C                   EXSR      DECDIV
116100000000     C*
116200020318     C* AZZERA I TOTALI DELLA DIVISIONE (X QUALITA' CLIENTE)
116300160913DO  1C                   DO        NR_ELEM_ABC   Q
116400000000     C     Q             OCCUR     ABCDIV
116500000000     C                   CLEAR                   CND
116600000000     C                   CLEAR                   SND
116700000000     C                   CLEAR                   RND
116800000000     C                   CLEAR                   PND
116900000000     C                   CLEAR                   CAD
117000000000     C                   CLEAR                   SAD
117100000000     C                   CLEAR                   RAD
117200000000     C                   CLEAR                   PAD
117300000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
117400000000     C     Q             OCCUR     ABCD13
117500000000     C                   CLEAR                   NTD
117600000000     C                   CLEAR                   ATD
117700000000E   1C                   ENDDO
117800160914      **
117900160909     C*----- i NTW sono rimasti sempre (5)
118000020318     C* AZZERA I TOTALI DELLA DIVISIONE (X NETWORK) NN "D"
118100020318DO  1C     1             DO        5             K
118200020318     C     K             OCCUR     ABCDIVN
118300020318     C                   CLEAR                   CNDN
118400020318     C                   CLEAR                   SNDN
118500020318     C                   CLEAR                   RNDN
118600020318     C                   CLEAR                   PNDN
118700020318     C                   CLEAR                   CADN
118800020318     C                   CLEAR                   SADN
118900020318     C                   CLEAR                   RADN
119000020318     C                   CLEAR                   PADN
119100020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
119200020318     C     K             OCCUR     ABCD13N
119300020318     C                   CLEAR                   NTDN
119400020318     C                   CLEAR                   ATDN
119500020318     C* AZZERA I TOTALI DELLA DIVISIONE (X NETWORK) "D"
119600020318     C     K             OCCUR     ABCDIVND
119700020318     C                   CLEAR                   CNDND
119800020318     C                   CLEAR                   SNDND
119900020318     C                   CLEAR                   RNDND
120000020318     C                   CLEAR                   PNDND
120100020318     C                   CLEAR                   CADND
120200020318     C                   CLEAR                   SADND
120300020318     C                   CLEAR                   RADND
120400020318     C                   CLEAR                   PADND
120500020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
120600020318     C     K             OCCUR     ABCD13ND
120700020318     C                   CLEAR                   NTDND
120800020318     C                   CLEAR                   ATDND
120900020318E   1C                   ENDDO
121000000000     C*
121100000000     C                   ENDSR
121200000000     C*------------------------------------------------------------------------*
121300000000     C*?NEWARE - OPERAZIONI PER NUOVA AREA
121400000000     C*------------------------------------------------------------------------*
121500000000     C     NEWARE        BEGSR
121600020220     C*
121700020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
121800020220     C                   MOVEL     '0'           FLGSIDA
121900000000     C*
122000000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
122100000000     C                   MOVEL     '0'           PASARE
122200000000     C*
122300000000     C* MEMORIZZA LA NUOVA AREA IN UN DEPOSITO
122400000000     C                   MOVEL     WFDARE        DEPARE
122500000000     C*
122600000000     C* DECODIFICA IL CODICE AREA
122700000000     C                   EXSR      DECARE
122800000000     C*
122900020318     C* AZZERA I TOTALI DELL'AREA (X QUALITA' CLIENTE)
123000160913DO  1C                   DO        NR_ELEM_ABC   Q
123100000000     C     Q             OCCUR     ABCARE
123200000000     C                   CLEAR                   CNA
123300000000     C                   CLEAR                   SNA
123400000000     C                   CLEAR                   RNA
123500000000     C                   CLEAR                   PNA
123600000000     C                   CLEAR                   CAA
123700000000     C                   CLEAR                   SAA
123800000000     C                   CLEAR                   RAA
123900000000     C                   CLEAR                   PAA
124000000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
124100000000     C     Q             OCCUR     ABCA13
124200000000     C                   CLEAR                   NTA
124300000000     C                   CLEAR                   ATA
124400000000E   1C                   ENDDO
124500020318     C*
124600020318     C* AZZERA I TOTALI DELL'AREA (X NETWORK) NN "D"
124700020318DO  1C     1             DO        5             K
124800020318     C     K             OCCUR     ABCAREN
124900020318     C                   CLEAR                   CNAN
125000020318     C                   CLEAR                   SNAN
125100020318     C                   CLEAR                   RNAN
125200020318     C                   CLEAR                   PNAN
125300020318     C                   CLEAR                   CAAN
125400020318     C                   CLEAR                   SAAN
125500020318     C                   CLEAR                   RAAN
125600020318     C                   CLEAR                   PAAN
125700020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
125800020318     C     K             OCCUR     ABCA13N
125900020318     C                   CLEAR                   NTAN
126000020318     C                   CLEAR                   ATAN
126100020318     C* AZZERA I TOTALI DELL'AREA (X NETWORK) "D"
126200020318     C     K             OCCUR     ABCAREND
126300020318     C                   CLEAR                   CNAND
126400020318     C                   CLEAR                   SNAND
126500020318     C                   CLEAR                   RNAND
126600020318     C                   CLEAR                   PNAND
126700020318     C                   CLEAR                   CAAND
126800020318     C                   CLEAR                   SAAND
126900020318     C                   CLEAR                   RAAND
127000020318     C                   CLEAR                   PAAND
127100020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
127200020318     C     K             OCCUR     ABCA13ND
127300020318     C                   CLEAR                   NTAND
127400020318     C                   CLEAR                   ATAND
127500020318E   1C                   ENDDO
127600000000     C*
127700000000     C                   ENDSR
127800000000     C*------------------------------------------------------------------------*
127900000000     C*?NEWFIL - OPERAZIONI PER NUOVA FILIALE
128000000000     C*------------------------------------------------------------------------*
128100000000     C     NEWFIL        BEGSR
128200020220     C*
128300020220     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
128400020220     C                   MOVEL     '0'           FLGSIDF
128500000000     C*
128600000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
128700000000     C                   MOVEL     '0'           PASFIL
128800000000     C*
128900000000     C* MEMORIZZA LA NUOVA FILIALE IN UN DEPOSITO
129000000000     C                   Z-ADD     WFDFIL        DEPFIL
129100000000     C*
129200000000     C* DECODIFICA IL CODICE FILIALE
129300000000     C                   EXSR      DECFIL
129400000000     C*
129500020318     C* AZZERA I TOTALI DELLA FILIALE (X QUALITA' CLIENTE)
129600160913DO  1C                   DO        NR_ELEM_ABC   Q
129700000000     C     Q             OCCUR     ABCFIL
129800000000     C                   CLEAR                   CNF
129900000000     C                   CLEAR                   SNF
130000000000     C                   CLEAR                   RNF
130100000000     C                   CLEAR                   PNF
130200000000     C                   CLEAR                   CAF
130300000000     C                   CLEAR                   SAF
130400000000     C                   CLEAR                   RAF
130500000000     C                   CLEAR                   PAF
130600000000     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
130700000000     C     Q             OCCUR     ABCF13
130800000000     C                   CLEAR                   NTF
130900000000     C                   CLEAR                   ATF
131000000000E   1C                   ENDDO
131100020318     C*
131200020318     C* AZZERA I TOTALI DELLA FILIALE (X NETWORK) NN "D"
131300020318DO  1C     1             DO        5             K
131400020318     C     K             OCCUR     ABCFILN
131500020318     C                   CLEAR                   CNFN
131600020318     C                   CLEAR                   SNFN
131700020318     C                   CLEAR                   RNFN
131800020318     C                   CLEAR                   PNFN
131900020318     C                   CLEAR                   CAFN
132000020318     C                   CLEAR                   SAFN
132100020318     C                   CLEAR                   RAFN
132200020318     C                   CLEAR                   PAFN
132300020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
132400020318     C     K             OCCUR     ABCF13N
132500020318     C                   CLEAR                   NTFN
132600020318     C                   CLEAR                   ATFN
132700020318     C* AZZERA I TOTALI DELLA FILIALE (X NETWORK) "D"
132800020318     C     K             OCCUR     ABCFILND
132900020318     C                   CLEAR                   CNFND
133000020318     C                   CLEAR                   SNFND
133100020318     C                   CLEAR                   RNFND
133200020318     C                   CLEAR                   PNFND
133300020318     C                   CLEAR                   CAFND
133400020318     C                   CLEAR                   SAFND
133500020318     C                   CLEAR                   RAFND
133600020318     C                   CLEAR                   PAFND
133700020318     C* AZZERA IL NUMERO TOTALE DEI CLIENTI (TREDICESIMA COLONNA)
133800020318     C     K             OCCUR     ABCF13ND
133900020318     C                   CLEAR                   NTFND
134000020318     C                   CLEAR                   ATFND
134100020318E   1C                   ENDDO
134200000000     C*
134300000000     C                   ENDSR
134400000000     C*------------------------------------------------------------------------*
134500000000     C*?NEWAGE - OPERAZIONI PER NUOVO AGENTE COMMERCIALE
134600000000     C*------------------------------------------------------------------------*
134700000000     C     NEWAGE        BEGSR
134800020220     C*
134900030924     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
135000030924     C                   MOVEL     '0'           FLGSIDE
135100030924     C*
135200030924     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
135300030924     C                   MOVEL     '0'           PASAGE
135400030924     C*
135500030924     C* MEMORIZZO IL NUOVO AGENTE IN UN DEPOSITO
135600030924     C                   Z-ADD     WFDAGE        DEPAGE
135700081013     C*
135800081013     C* l'Agente Unificante e la sua descrizione
135900081013     C                   Z-ADD     WFDrgf        DEPrgf
136000081013     C                   movel     WFDrgd        DEPrgd
136100081013     C*
136200000000     C* DECODIFICO IL CODICE AGENTE
136300000000     C                   EXSR      DECAGE
136400000000     C*
136500020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X QUALITA' CLIENTE)
136600160913DO  1C                   DO        NR_ELEM_ABC   Q
136700000000     C     Q             OCCUR     ABCCOM
136800000000     C                   CLEAR                   CNC
136900000000     C                   CLEAR                   SNC
137000000000     C                   CLEAR                   RNC
137100000000     C                   CLEAR                   PNC
137200000000     C                   CLEAR                   CAC
137300000000     C                   CLEAR                   SAC
137400000000     C                   CLEAR                   RAC
137500000000     C                   CLEAR                   PAC
137600000000     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
137700000000     C     Q             OCCUR     ABCC13
137800000000     C                   CLEAR                   NTC
137900000000     C                   CLEAR                   ATC
138000000000E   1C                   ENDDO
138100020318     C*
138200020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X NETWORK) NN "D"
138300020318DO  1C     1             DO        5             K
138400020318     C     K             OCCUR     ABCCOMN
138500020318     C                   CLEAR                   CNCN
138600020318     C                   CLEAR                   SNCN
138700020318     C                   CLEAR                   RNCN
138800020318     C                   CLEAR                   PNCN
138900020318     C                   CLEAR                   CACN
139000020318     C                   CLEAR                   SACN
139100020318     C                   CLEAR                   RACN
139200020318     C                   CLEAR                   PACN
139300020318     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
139400020318     C     K             OCCUR     ABCC13N
139500020318     C                   CLEAR                   NTCN
139600020318     C                   CLEAR                   ATCN
139700020318     C* AZZERA I TOTALI DELL' AGENTE PER TOTALI "A", "B", "C" (X NETWORK) "D"
139800020318     C     K             OCCUR     ABCCOMND
139900020318     C                   CLEAR                   CNCND
140000020318     C                   CLEAR                   SNCND
140100020318     C                   CLEAR                   RNCND
140200020318     C                   CLEAR                   PNCND
140300020318     C                   CLEAR                   CACND
140400020318     C                   CLEAR                   SACND
140500020318     C                   CLEAR                   RACND
140600020318     C                   CLEAR                   PACND
140700020318     C* AZZERA IL N° TOTALE DEI CLIENTI (13^ COLONNA) PER TOTALI "A", "B", "C"
140800020318     C     K             OCCUR     ABCC13ND
140900020318     C                   CLEAR                   NTCND
141000020318     C                   CLEAR                   ATCND
141100020318E   1C                   ENDDO
141200000000     C*
141300000000     C                   ENDSR
141400000000     C*------------------------------------------------------------------------*
141500000000     C*?NEWKSC - OPERAZIONI PER NUOVO CLIENTE
141600000000     C*------------------------------------------------------------------------*
141700000000     C     NEWKSC        BEGSR
141800020221     C*
141900020221     C* REIMPOSTO FLAG DI LETTURA CLIENTE 'D'
142000020221     C                   MOVEL     '0'           FLGSIDK
142100000000     C*
142200000000     C* REIMPOSTO FLAG DI PASSAGGIO MEMORIZZAZIONE SALDI
142300000000     C                   MOVEL     '0'           PASKSC
142400000000     C*
142500951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (13^ COLONNA) PERIODO CORRENTE
142600000000     C                   MOVEL     '0'           FLAKSC
142700951227     C*
142800951227     C* REIMPOSTO FLAG CONTATORE CLIENTI TOTALI (1^ COLONNA) PERIODO PRECEDENTE
142900951227     C                   MOVEL     '0'           FL2KSC
143000000000     C*
143100000000     C* REIMPOSTO FLAG PASSAGGIO LETTURA SALDI PRECEDENTI A SALDI CORRENTI
143200000000     C                   MOVEL     '0'           FLAPAS
143300000830     C*
143400000830     C* REIMPOSTO FLAG CONTATORE CLIENTI PER MESE
143500000830     C                   MOVEL     '0'           SFCCL
143600000000     C*
143700000000     C* MEMORIZZO IL NUOVO CLIENTE IN UN DEPOSITO
143800000825     C                   Z-ADD     WFDCLI        DEPCLI
143900020319     C*
144000020319     C* RE-INIZIALIZZO LA SCHIERA DI WRK X CLIENTE/MESE/NETWORK
144100020319     C                   Z-ADD     *ZEROS        Y
144200020319     C                   CLEAR                   WSK1
144300000000     C*
144400000000     C* DECODIFICO IL CODICE CLIENTE
144500000000     C                   EXSR      DECKSC
144600000000     C*
144700000000     C* AZZERA I TOTALI DEL CLIENTE
144800121217     C                   CLEAR                   CLN1K
144900000000     C                   CLEAR                   S1K
145000081013     C                   CLEAR                   Kg1K
145100000000     C                   CLEAR                   R1K
145200000000     C                   CLEAR                   P1K
145300970822     C                   CLEAR                   D1K
145400020712     C                   CLEAR                   R1KP
145500070419     C                   CLEAR                   ap_S1K
145600081013     C                   CLEAR                   ap_Kg1K
145700070419     C                   CLEAR                   ap_R1K
145800070419     C                   CLEAR                   ap_P1K
145900091123      * -------------
146000100204     /*?  solo x per l'Estero                                   ?
146100100204     /*?     x Network                                          ?
146200100204     /*?   Pulisce le schiere di totale                         ?
146300091123      * -------------
146400091123IF  1C     PARIOE        IFEQ      'E'
146500091123      * -------------
146600091123DO  1C     1             DO        5             K
146700100204     C* AZZERA I TOTALI x NETWORK anno in corso
146800091124     C     K             OCCUR     W1ntw
146900091124     C                   CLEAR                   C1ntw
147000091124     C                   CLEAR                   S1ntw
147100091124     C                   CLEAR                   KG1ntw
147200091124     C                   CLEAR                   R1ntw
147300091124     C                   CLEAR                   P1ntw
147400091124     C                   CLEAR                   D1ntw
147500091124     C                   CLEAR                   R1Pntw
147600091125     C                   CLEAR                   TR1ntw
147700091125     C                   CLEAR                   TP1ntw
147800091124     C* AZZERA I TOTALI x NETWORK  dell'Anno precedente
147900091124     C     K             OCCUR     Wap1ntw
148000091124     C                   CLEAR                   ap_C1ntw
148100091124     C                   CLEAR                   ap_S1ntw
148200091124     C                   CLEAR                   ap_KG1ntw
148300091124     C                   CLEAR                   ap_R1ntw
148400091124     C                   CLEAR                   ap_P1ntw
148500091124     C                   CLEAR                   ap_D1ntw
148600091124     C                   CLEAR                   ap_R1Pntw
148700091125     C                   CLEAR                   ap_TR1ntw
148800091125     C                   CLEAR                   ap_TP1ntw
148900091123E   1C                   ENDDO
149000091123      * -------------
149100091123E   1C                   END
149200091123      * -------------
149300100204     /*?  Pulizia schiere di totali                             ?
149400100204     /*?    x  ntw e tipi servizio                              ?
149500100204     /*?      sia Italia che Estero                             ?
149600100129      * -------------
149700100129DO  1C     1             DO        50            Kserv             3 0
149800100201     C* AZZERA I TOTALI x NETWORK/TIPO SERVIZIO
149900100129     C     Kserv         OCCUR     W1ntwTS
150000100202      * chiave NTW + TServ
150100100129     C                   CLEAR                   C1ntwTS
150200100201     C                   CLEAR                   C1tpsTS
150300100202     C* AZZERA I TOTALI x NETWORK  dell'Anno in Corso
150400100129     C                   CLEAR                   S1ntwTS
150500100129     C                   CLEAR                   KG1ntwTS
150600100129     C                   CLEAR                   R1ntwTS
150700100129     C                   CLEAR                   P1ntwTS
150800100129     C                   CLEAR                   D1ntwTS
150900100129     C                   CLEAR                   R1PntwTS
151000100129     C                   CLEAR                   TR1ntwTS
151100100129     C                   CLEAR                   TP1ntwTS
151200100129     C* AZZERA I TOTALI x NETWORK  dell'Anno precedente
151300100129     C                   CLEAR                   ap_S1ntwTS
151400100129     C                   CLEAR                   ap_KG1ntwTS
151500100129     C                   CLEAR                   ap_R1ntwTS
151600100129     C                   CLEAR                   ap_P1ntwTS
151700100129     C                   CLEAR                   ap_D1ntwTS
151800100129     C                   CLEAR                   ap_R1PntwTS
151900100129     C                   CLEAR                   ap_TR1ntwTS
152000100129     C                   CLEAR                   ap_TP1ntwTS
152100100202     C*
152200100129E   1C                   ENDDO
152300100129      * -------------
152400000828     C* CERCA I FIGLI DEL CLIENTE, SE RICHIESTA STAMPA PER UNFICANTE
152500000828     C                   CLEAR                   SKSC11
152600000828IF  1C     PARSCE        IFEQ      'U'                                          *SCELTA UNIFICANTE
152700000828     C                   CLEAR                   BS10DS
152800010725     C*                  Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
152900010725     C                   Z-ADD     WDFIN         D10DRF                         *DATA FINALE LANCIO
153000000828     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
153100000828     C                   MOVEL     'F'           D10PAF                         *CERCA I FIGLI
153200000828     C                   MOVE      DEPCLI        D10COD
153300000828     C                   CALL      'TIBS10R'
153400000828     C                   PARM                    BS10DS
153500000828E   1C                   ENDIF
153600000000     C*
153700130131     C*  Totalizza le Varie per CLIENTE sul nuovo WADELPCV dal WFDEAV0F
153800130131     C*   man mano che le riporta sul file di TOTALE le ELIMINA dal file di WORK.
153900130131     c                   exsr      VARIE_x_Client
154000130131     C*
154100000000     C                   ENDSR
154200000000     C*------------------------------------------------------------------------*
154300000000     C*?NEWPPC - OPERAZIONI PER PASSAGGIO LETTURA SALDI PRECED-->SALDI CORRENTI
154400000000     C*------------------------------------------------------------------------*
154500000000     C     NEWPPC        BEGSR
154600000000     C*
154700000000     C* SE E' LA PRIMA VOLTA CHE LEGGO UN SALDO CORRENTE (MOMENTO DI PASSAGGIO)
154800000000     C* PRENDO COME "CLIENTE NUOVO/ACQUISITO" QUELLO DEL PRIMO SALDO PERCHE' SE
154900000000     C* UN CLIENTE E' NUOVO PER UN MESE ("Y") LO E' PER TUTTO IL PERIODO
155000000000IF  1C     TIPANN        IFEQ      'C'
155100000000IF  2C     FLAPAS        IFEQ      '0'
155200000000     C                   MOVEL     '1'           FLAPAS                         *PER RIMANERE TALE
155300000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O NON NUOVO
155400000000E   2C                   ENDIF
155500000000E   1C                   ENDIF
155600000000     C*
155700030924     C                   ENDSR
155800030924     C*------------------------------------------------------------------------*
155900030924     C*?MEMSAL - MEMORIZZA I SALDI DEL WORK FILE IN SCHIERE
156000030924     C*------------------------------------------------------------------------*
156100030924     C     MEMSAL        BEGSR
156200030924     C*
156300030924     C* I SALDI DEL PERIODO PRECEDENTE LI MEMORIZZA IN 13^ POSIZIONE DI SCHIERA
156400030924     C* UN CLIENTE E' NUOVO NEL PERIODO SOLO SE HA UNA "Y" NEI SALDI
156500030924     C*
156600030924     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
156700030924     C                   MOVEL     '1'           PASKSC                         *CLIENTE
156800030924     C                   MOVEL     '1'           PASAGE                         *AGENTE
156900030924     C                   MOVEL     '1'           PASFIL                         *FILIALE
157000030924     C                   MOVEL     '1'           PASARE                         *AREA
157100030924     C                   MOVEL     '1'           PASDIV                         *DIVISIONE
157200030924     C                   MOVEL     '1'           PASGEN                         *GENERALE
157300000000     C*
157400951227     C* CONTA I CLIENTI DA STAMPARE ALLA 1^ O 13^ COLONNA SOLO A ROTTURA DI
157500951227     C* CLIENTE (SE STA LEGGENDO I SALDI DEL PERIODO PRECEDENTE O CORRENTE)
157600951227IF  1C     TIPANN        IFEQ      'C'                                          *CORRENTE
157700951227IF  2C     FLAKSC        IFEQ      '0'                                          *ROTTURA CLIENTE
157800000000     C                   MOVEL     '1'           FLAKSC
157900951227     C                   Z-ADD     1             CONKSC            1 0          *CLIENTE NON AGGIUNT
158000951227X   2C                   ELSE
158100951227     C                   Z-ADD     *ZEROS        CONKSC                         *CLIENTE AGGIUNTO
158200951227E   2C                   ENDIF
158300951227X   1C                   ELSE                                                   *PRECEDENTE
158400951227IF  2C     FL2KSC        IFEQ      '0'                                          *ROTTURA CLIENTE
158500951227     C                   MOVEL     '1'           FL2KSC
158600951227     C                   Z-ADD     1             CO2KSC            1 0          *CLIENTE AGGIUNTO
158700951227X   2C                   ELSE
158800951227     C                   Z-ADD     *ZEROS        CO2KSC                         *CLIENTE NON AGGIUNT
158900951227E   2C                   ENDIF
159000951227E   1C                   ENDIF
159100020318     C*
159200020318     C* Q U A L I T A'  /  N E T W O R K
159300000000     C                   Z-ADD     1             Q
159400000000     C                   SETOFF                                           28
159500000000     C     DESCLV        LOOKUP    ABC(Q)                                 28
159600020318     C                   Z-ADD     1             K
159700020318     C                   SETOFF                                           27
159800020318     C     WFDNTW        LOOKUP    DNTW(K)                                27
159900000000IF  1C     *IN28         IFEQ      *ON
160000020318IF  1C     *IN27         ANDEQ     *ON
160100000000     C* M E S E
160200990609IF  2C     WFDDAT        IFGE      PARDPT                                       *PERIODO PRECEDENTE
160300990609     C     WFDDAT        ANDLE     PARDP2
160400000000     C                   Z-ADD     13            MM
160500020712     C                   Z-ADD     1             MMP               2 0
160600020712     C                   SETOFF                                           33
160700020712     C     WFDMES        LOOKUP    MES(MMP)                               33
160800951219E   2C                   ENDIF
160900990609IF  2C     WFDDAT        IFGE      PARDAT                                       *PERIODO CORRENTE
161000990609     C     WFDDAT        ANDLE     PARDA2
161100000000     C                   Z-ADD     1             MM
161200000000     C                   SETOFF                                           28
161300000000     C     WFDMES        LOOKUP    MES(MM)                                28
161400000000E   2C                   ENDIF
161500020220     C*
161600020220     C* VERIFICO SE ALMENO 1 CLIENTE "D"
161700020318     C                   IF        DESCLV = 'D'
161800020220     C                   MOVEL     '1'           FLGSIDG           1
161900020220     C                   MOVEL     '1'           FLGSIDD           1
162000020220     C                   MOVEL     '1'           FLGSIDA           1
162100020220     C                   MOVEL     '1'           FLGSIDF           1
162200020220     C                   MOVEL     '1'           FLGSIDE           1
162300020221     C                   MOVEL     '1'           FLGSIDK           1
162400020220     C                   ENDIF
162500020319     C*
162600020319     C* VERIFICO SE X GIA' CONTEGGIATO IL CLIENTE/MESE/NETWORK
162700020319     C     DSWSK1        LOOKUP    WSK1                                   55
162800020319     C                   IF        *IN55 = *ON
162900020319     C                   Z-ADD     *ZEROS        CONNTW            1 0
163000020319     C                   ELSE
163100020319     C                   ADD       1             Y                 3 0
163200020319     C                   EVAL      WSK1(Y) = DSWSK1
163300020319     C                   Z-ADD     1             CONNTW
163400020319     C                   ENDIF
163500000000     C*--------------------
163600020220     C* C L I E N T E
163700000000     C*--------------------
163800121217     C                   move      WFDcln        cln1K(MM)                      memorizza i flag di cliente nuovo/acq
163900000000     C                   ADD       WFDNSP        S1K(MM)                        SPEDIZIONI
164000081013     C                   eval      Kg1K(MM) = wfdPKG + Kg1K(MM)                 PESI in KG
164100000000     C                   ADD       WFDRRN        R1K(MM)                        RICAVI REALI DELTA
164200000000     C                   ADD       WFDRPN        P1K(MM)                        RICAVI PRESUNTI DELT
164300091123      * -------------
164400091123      **  solo x per l'estero incasella x Network
164500091123IF  1C     PARIOE        IFEQ      'E'
164600091124IF  1C     MM            andle     12
164700091124     C     K             OCCUR     W1ntw
164800091124     c                   movel     WFDNTW        C1ntw
164900091124     C                   ADD       WFDNSP        S1ntw(MM)                        SPEDIZIONI
165000091124     C                   eval      Kg1ntw(MM) = wfdPKG + Kg1ntw(MM)                 PESI in KG
165100091124     C                   ADD       WFDRRN        R1ntw(MM)                        RICAVI REALI DELTA
165200091124     C                   ADD       WFDRPN        P1ntw(MM)                        RICAVI PRESUNTI DELT
165300091125     C                   z-add(h)  WFDRRN        R1totale         15 0
165400091125     C                   z-add(h)  WFDRPN        P1totale         15 0
165500091125     C                   ADD       R1totale      TR1ntw
165600091125     C                   ADD       P1totale      TP1ntw
165700091123     c                   END
165800091123      **
165900970822IF  2C     MM            IFNE      13                                           SOLO PERIODO CORRENT
166000020220     C                   MOVEL     WFDSPD        D1K(MM)                        FLAG % SCOSTAM.DELTA
166100070420      *
166200070420      * ?se invece è un dato dell'anno precedente
166300070420     c                   else
166400081013      *
166500070420      * ?lo devo incasellare nel mese appropriato
166600070420     C                   ADD       WFDNSP        ap_S1K(WFDmes)                 SPEDIZIONI
166700081013     C                   eval      ap_Kg1K(WFDmes) = wfdPKG + ap_Kg1K(WFDmes)   PESI in KG
166800070420     C                   ADD       WFDRRN        ap_R1K(WFDmes)                 RICAVI REALI DELTA
166900070420     C                   ADD       WFDRPN        ap_P1K(WFDmes)                 RICAVI PRESUNTI DELT
167000091123      * -------------
167100091123      **  solo x per l'estero incasella x Network
167200091123IF  1C     PARIOE        IFEQ      'E'
167300091124IF  1C     WFDmes        andle     12
167400091124     C     K             OCCUR     WAP1ntw
167500091124     c                   movel     WFDNTW        Ap_c1ntw
167600091124     C                   eval      ap_S1ntw(WFDmes) = WFDNSP + ap_S1ntw(WFDmes)    SPEDIZIONI
167700091124     C                   eval      ap_Kg1ntw(WFDmes)= wfdPKG +ap_Kg1ntw(WFDmes)      PESI in KG
167800091124     C                   eval      ap_R1ntw(WFDmes) = WFDRRN + ap_R1ntw(WFDmes)    RICAVI REALI DELT
167900091124     C                   eval      ap_P1ntw(WFDmes) = WFDRPN + ap_P1ntw(WFDmes)    RICAVI PRESUNTI D
168000091125     C                   z-add(h)  WFDRRN        R1totale         15 0
168100091125     C                   z-add(h)  WFDRPN        P1totale         15 0
168200091125     C                   ADD       R1totale      ap_TR1ntw
168300091125     C                   ADD       P1totale      ap_TP1ntw
168400091123     c                   END
168500091123      **
168600970822E   2C                   ENDIF
168700100201      * -------------
168800100204      **
168900100204     /*?  totalizza incasellando x Network e Tipo Servizio      ?
169000100204     /*?   x Italia e poi x Estero            ......e           ?
169100100204     /*?   se RICHIESTA creazione del File PC   (WFDELPCF)      ?
169200100204IF  1C                   IF        PARIOE <> *blank and PARFPC = 'S'
169300100201     c                   exsr      TOTCLI_NTW_TS
169400100201     c                   END
169500100201      * -------------
169600070420      *
169700020712     C     TIPANN        IFEQ      'P'
169800020712     C     *IN33         ANDEQ     *ON
169900020712     C                   ADD       WFDRRN        R1KP(MMP)                      RICAVI REALI DELTA A
170000020712     C                   ENDIF
170100000000     C*--------------------
170200020220     C* C O M M E R C I A L E
170300000000     C*--------------------
170400000000     C     Q             OCCUR     ABCCOM
170500020318     C     K             OCCUR     ABCCOMN
170600020318     C     K             OCCUR     ABCCOMND
170700000000     C* NUOVO
170800000000IF  2C     DESCLN        IFEQ      'Y'
170900000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
171000000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
171100020318     C                   ADD       1             CNC(MM)                        NUMERO CLIENTI
171200000830E   3C                   ENDIF
171300020319     C     MM            IFNE      13
171400020319     C                   IF        DESCLV <> 'D'
171500020319     C                   ADD       CONNTW        CNCN(MM)                       NUMERO CLIENTI
171600020318     C                   ELSE
171700020319     C                   ADD       CONNTW        CNCND(MM)                      NUMERO CLIENTI
171800020318     C                   ENDIF
171900020319     C                   ENDIF
172000000000     C                   ADD       WFDNSP        SNC(MM)                        SPEDIZIONI
172100000000     C                   ADD       WFDRRN        RNC(MM)                        RICAVI REALI DELTA
172200000000     C                   ADD       WFDRPN        PNC(MM)                        RICAVI PRESUNTI DELT
172300020318     C                   IF        DESCLV <> 'D'
172400020318     C                   ADD       WFDNSP        SNCN(MM)                       SPEDIZIONI
172500020318     C                   ADD       WFDRRN        RNCN(MM)                       RICAVI REALI DELTA
172600020318     C                   ADD       WFDRPN        PNCN(MM)                       RICAVI PRESUNTI DELT
172700020318     C                   ELSE
172800020318     C                   ADD       WFDNSP        SNCND(MM)                      SPEDIZIONI
172900020318     C                   ADD       WFDRRN        RNCND(MM)                      RICAVI REALI DELTA
173000020318     C                   ADD       WFDRPN        PNCND(MM)                      RICAVI PRESUNTI DELT
173100020318     C                   ENDIF
173200000000     C* ACQUISITO
173300000000X   2C                   ELSE
173400000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
173500000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
173600000000     C                   ADD       1             CAC(MM)                        NUMERO CLIENTI
173700000830E   3C                   ENDIF
173800020319     C     MM            IFNE      13
173900020319     C                   IF        DESCLV <> 'D'
174000020319     C                   ADD       CONNTW        CACN(MM)                       NUMERO CLIENTI
174100020318     C                   ELSE
174200020319     C                   ADD       CONNTW        CACND(MM)                      NUMERO CLIENTI
174300020318     C                   ENDIF
174400020319     C                   ENDIF
174500000000     C                   ADD       WFDNSP        SAC(MM)                        SPEDIZIONI
174600000000     C                   ADD       WFDRRN        RAC(MM)                        RICAVI REALI DELTA
174700000000     C                   ADD       WFDRPN        PAC(MM)                        RICAVI PRESUNTI DELT
174800020318     C                   IF        DESCLV <> 'D'
174900020318     C                   ADD       WFDNSP        SACN(MM)                       SPEDIZIONI
175000020318     C                   ADD       WFDRRN        RACN(MM)                       RICAVI REALI DELTA
175100020318     C                   ADD       WFDRPN        PACN(MM)                       RICAVI PRESUNTI DELT
175200020318     C                   ELSE
175300020318     C                   ADD       WFDNSP        SACND(MM)                      SPEDIZIONI
175400020318     C                   ADD       WFDRRN        RACND(MM)                      RICAVI REALI DELTA
175500020318     C                   ADD       WFDRPN        PACND(MM)                      RICAVI PRESUNTI DELT
175600020318     C                   ENDIF
175700000000E   2C                   ENDIF
175800000000     C*---
175900000000     C* TOT CLIENTI 13^ COLONNA
176000000000     C*---
176100000000     C     Q             OCCUR     ABCC13
176200020318     C     K             OCCUR     ABCC13N
176300020318     C     K             OCCUR     ABCC13ND
176400951227     C* NUOVO
176500951227IF  2C     DESCLN        IFEQ      'Y'
176600951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
176700951227     C                   ADD       CO2KSC        NTC(2)
176800020318     C                   IF        DESCLV <> 'D'
176900020318     C                   ADD       CO2KSC        NTCN(2)
177000020318     C                   ELSE
177100020318     C                   ADD       CO2KSC        NTCND(2)
177200020318     C                   ENDIF
177300951227X   3C                   ELSE                                                   *PERIODO CORRENTE
177400951227     C                   ADD       CONKSC        NTC(1)
177500020318     C                   IF        DESCLV <> 'D'
177600020318     C                   ADD       CONKSC        NTCN(1)
177700020318     C                   ELSE
177800020318     C                   ADD       CONKSC        NTCND(1)
177900020318     C                   ENDIF
178000951227E   3C                   ENDIF
178100951227     C* ACQUISITO
178200951227X   2C                   ELSE
178300951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
178400951227     C                   ADD       CO2KSC        ATC(2)
178500020318     C                   IF        DESCLV <> 'D'
178600020318     C                   ADD       CO2KSC        ATCN(2)
178700020318     C                   ELSE
178800020318     C                   ADD       CO2KSC        ATCND(2)
178900020318     C                   ENDIF
179000951227X   3C                   ELSE                                                   *PERIODO CORRENTE
179100951227     C                   ADD       CONKSC        ATC(1)
179200020318     C                   IF        DESCLV <> 'D'
179300020318     C                   ADD       CONKSC        ATCN(1)
179400020318     C                   ELSE
179500020318     C                   ADD       CONKSC        ATCND(1)
179600020318     C                   ENDIF
179700951227E   3C                   ENDIF
179800951227E   2C                   ENDIF
179900000000     C*--------------------
180000000000     C* F I L I A L E
180100000000     C*--------------------
180200000000     C     Q             OCCUR     ABCFIL
180300020318     C     K             OCCUR     ABCFILN
180400020318     C     K             OCCUR     ABCFILND
180500000000     C* NUOVO
180600000000IF  2C     DESCLN        IFEQ      'Y'
180700000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
180800000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
180900000000     C                   ADD       1             CNF(MM)                        NUMERO CLIENTI
181000000830E   3C                   ENDIF
181100020319     C     MM            IFNE      13
181200020319     C                   IF        DESCLV <> 'D'
181300020319     C                   ADD       CONNTW        CNFN(MM)                       NUMERO CLIENTI
181400020318     C                   ELSE
181500020319     C                   ADD       CONNTW        CNFND(MM)                      NUMERO CLIENTI
181600020318     C                   ENDIF
181700020319     C                   ENDIF
181800000000     C                   ADD       WFDNSP        SNF(MM)                        SPEDIZIONI
181900000000     C                   ADD       WFDRRN        RNF(MM)                        RICAVI REALI DELTA
182000000000     C                   ADD       WFDRPN        PNF(MM)                        RICAVI PRESUNTI DELT
182100020318     C                   IF        DESCLV <> 'D'
182200020318     C                   ADD       WFDNSP        SNFN(MM)                       SPEDIZIONI
182300020318     C                   ADD       WFDRRN        RNFN(MM)                       RICAVI REALI DELTA
182400020318     C                   ADD       WFDRPN        PNFN(MM)                       RICAVI PRESUNTI DELT
182500020318     C                   ELSE
182600020318     C                   ADD       WFDNSP        SNFND(MM)                      SPEDIZIONI
182700020318     C                   ADD       WFDRRN        RNFND(MM)                      RICAVI REALI DELTA
182800020318     C                   ADD       WFDRPN        PNFND(MM)                      RICAVI PRESUNTI DELT
182900020318     C                   ENDIF
183000000000     C* ACQUISITO
183100000000X   2C                   ELSE
183200000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
183300000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
183400000000     C                   ADD       1             CAF(MM)                        NUMERO CLIENTI
183500000830E   3C                   ENDIF
183600020319     C     MM            IFNE      13
183700020319     C                   IF        DESCLV <> 'D'
183800020319     C                   ADD       CONNTW        CAFN(MM)                       NUMERO CLIENTI
183900020318     C                   ELSE
184000020319     C                   ADD       CONNTW        CAFND(MM)                      NUMERO CLIENTI
184100020318     C                   ENDIF
184200020319     C                   ENDIF
184300000000     C                   ADD       WFDNSP        SAF(MM)                        SPEDIZIONI
184400000000     C                   ADD       WFDRRN        RAF(MM)                        RICAVI REALI DELTA
184500000000     C                   ADD       WFDRPN        PAF(MM)                        RICAVI PRESUNTI DELT
184600020318     C                   IF        DESCLV <> 'D'
184700020318     C                   ADD       WFDNSP        SAFN(MM)                       SPEDIZIONI
184800020318     C                   ADD       WFDRRN        RAFN(MM)                       RICAVI REALI DELTA
184900020318     C                   ADD       WFDRPN        PAFN(MM)                       RICAVI PRESUNTI DELT
185000020318     C                   ELSE
185100020318     C                   ADD       WFDNSP        SAFND(MM)                      SPEDIZIONI
185200020318     C                   ADD       WFDRRN        RAFND(MM)                      RICAVI REALI DELTA
185300020318     C                   ADD       WFDRPN        PAFND(MM)                      RICAVI PRESUNTI DELT
185400020318     C                   ENDIF
185500000000E   2C                   ENDIF
185600000000     C*---
185700000000     C* TOT CLIENTI 13^ COLONNA
185800000000     C*---
185900000000     C     Q             OCCUR     ABCF13
186000020318     C     K             OCCUR     ABCF13N
186100020318     C     K             OCCUR     ABCF13ND
186200951227IF  2C     DESCLN        IFEQ      'Y'
186300951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
186400951227     C                   ADD       CO2KSC        NTF(2)
186500020318     C                   IF        DESCLV <> 'D'
186600020319     C                   ADD       CONNTW        NTFN(2)
186700020318     C                   ELSE
186800020319     C                   ADD       CONNTW        NTFND(2)
186900020318     C                   ENDIF
187000951227X   3C                   ELSE                                                   *PERIODO CORRENTE
187100951227     C                   ADD       CONKSC        NTF(1)
187200020318     C                   IF        DESCLV <> 'D'
187300020319     C                   ADD       CONNTW        NTFN(1)
187400020318     C                   ELSE
187500020319     C                   ADD       CONNTW        NTFND(1)
187600020318     C                   ENDIF
187700951227E   3C                   ENDIF
187800030924     C* ACQUISITO
187900951227X   2C                   ELSE
188000951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
188100951227     C                   ADD       CO2KSC        ATF(2)
188200030924     C                   IF        DESCLV <> 'D'
188300030924     C                   ADD       CONNTW        ATFN(2)
188400030924     C                   ELSE
188500020319     C                   ADD       CONNTW        ATFND(2)
188600020318     C                   ENDIF
188700951227X   3C                   ELSE                                                   *PERIODO CORRENTE
188800951227     C                   ADD       CONKSC        ATF(1)
188900020318     C                   IF        DESCLV <> 'D'
189000020319     C                   ADD       CONNTW        ATFN(1)
189100020318     C                   ELSE
189200020319     C                   ADD       CONNTW        ATFND(1)
189300020318     C                   ENDIF
189400951227E   3C                   ENDIF
189500951227E   2C                   ENDIF
189600000000     C*--------------------
189700000000     C* A R E A
189800000000     C*--------------------
189900000000     C     Q             OCCUR     ABCARE
190000020318     C     K             OCCUR     ABCAREN
190100020318     C     K             OCCUR     ABCAREND
190200000000     C* NUOVO
190300000000IF  2C     DESCLN        IFEQ      'Y'
190400000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
190500000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
190600000000     C                   ADD       1             CNA(MM)                        NUMERO CLIENTI
190700000830E   3C                   ENDIF
190800020319     C     MM            IFNE      13
190900020319     C                   IF        DESCLV <> 'D'
191000020319     C                   ADD       CONNTW        CNAN(MM)                       NUMERO CLIENTI
191100020318     C                   ELSE
191200020319     C                   ADD       CONNTW        CNAND(MM)                      NUMERO CLIENTI
191300020318     C                   ENDIF
191400020319     C                   ENDIF
191500000000     C                   ADD       WFDNSP        SNA(MM)                        SPEDIZIONI
191600000000     C                   ADD       WFDRRN        RNA(MM)                        RICAVI REALI DELTA
191700000000     C                   ADD       WFDRPN        PNA(MM)                        RICAVI PRESUNTI DELT
191800020318     C                   IF        DESCLV <> 'D'
191900020318     C                   ADD       WFDNSP        SNAN(MM)                       SPEDIZIONI
192000020318     C                   ADD       WFDRRN        RNAN(MM)                       RICAVI REALI DELTA
192100020318     C                   ADD       WFDRPN        PNAN(MM)                       RICAVI PRESUNTI DELT
192200020318     C                   ELSE
192300020318     C                   ADD       WFDNSP        SNAND(MM)                      SPEDIZIONI
192400020318     C                   ADD       WFDRRN        RNAND(MM)                      RICAVI REALI DELTA
192500020318     C                   ADD       WFDRPN        PNAND(MM)                      RICAVI PRESUNTI DELT
192600020318     C                   ENDIF
192700000000     C* ACQUISITO
192800000000X   2C                   ELSE
192900000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
193000000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
193100000000     C                   ADD       1             CAA(MM)                        NUMERO CLIENTI
193200000830E   3C                   ENDIF
193300020319     C     MM            IFNE      13
193400020319     C                   IF        DESCLV <> 'D'
193500020319     C                   ADD       CONNTW        CAAN(MM)                       NUMERO CLIENTI
193600020318     C                   ELSE
193700020319     C                   ADD       CONNTW        CAAND(MM)                      NUMERO CLIENTI
193800020318     C                   ENDIF
193900020319     C                   ENDIF
194000000000     C                   ADD       WFDNSP        SAA(MM)                        SPEDIZIONI
194100000000     C                   ADD       WFDRRN        RAA(MM)                        RICAVI REALI DELTA
194200000000     C                   ADD       WFDRPN        PAA(MM)                        RICAVI PRESUNTI DELT
194300020318     C                   IF        DESCLV <> 'D'
194400020318     C                   ADD       WFDNSP        SAAN(MM)                       SPEDIZIONI
194500020318     C                   ADD       WFDRRN        RAAN(MM)                       RICAVI REALI DELTA
194600020318     C                   ADD       WFDRPN        PAAN(MM)                       RICAVI PRESUNTI DELT
194700020318     C                   ELSE
194800020318     C                   ADD       WFDNSP        SAAND(MM)                      SPEDIZIONI
194900020318     C                   ADD       WFDRRN        RAAND(MM)                      RICAVI REALI DELTA
195000020318     C                   ADD       WFDRPN        PAAND(MM)                      RICAVI PRESUNTI DELT
195100020318     C                   ENDIF
195200000000E   2C                   ENDIF
195300000000     C*---
195400000000     C* TOT CLIENTI 13^ COLONNA
195500000000     C*---
195600000000     C     Q             OCCUR     ABCA13
195700020318     C     K             OCCUR     ABCA13N
195800020318     C     K             OCCUR     ABCA13ND
195900951227IF  2C     DESCLN        IFEQ      'Y'
196000951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
196100951227     C                   ADD       CO2KSC        NTA(2)
196200020318     C                   IF        DESCLV <> 'D'
196300020319     C                   ADD       CONNTW        NTAN(2)
196400020318     C                   ELSE
196500020319     C                   ADD       CONNTW        NTAND(2)
196600020318     C                   ENDIF
196700951227X   3C                   ELSE                                                   *PERIODO CORRENTE
196800951227     C                   ADD       CONKSC        NTA(1)
196900020318     C                   IF        DESCLV <> 'D'
197000020319     C                   ADD       CONNTW        NTAN(1)
197100020318     C                   ELSE
197200020319     C                   ADD       CONNTW        NTAND(1)
197300020318     C                   ENDIF
197400951227E   3C                   ENDIF
197500951227     C* ACQUISITO
197600951227X   2C                   ELSE
197700951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
197800951227     C                   ADD       CO2KSC        ATA(2)
197900020318     C                   IF        DESCLV <> 'D'
198000020319     C                   ADD       CONNTW        ATAN(2)
198100020318     C                   ELSE
198200020319     C                   ADD       CONNTW        ATAND(2)
198300020318     C                   ENDIF
198400951227X   3C                   ELSE                                                   *PERIODO CORRENTE
198500951227     C                   ADD       CONKSC        ATA(1)
198600020318     C                   IF        DESCLV <> 'D'
198700020319     C                   ADD       CONNTW        ATAN(1)
198800020318     C                   ELSE
198900020319     C                   ADD       CONNTW        ATAND(1)
199000020318     C                   ENDIF
199100951227E   3C                   ENDIF
199200951227E   2C                   ENDIF
199300000000     C*--------------------
199400000000     C* D I V I S I O N E
199500000000     C*--------------------
199600000000     C     Q             OCCUR     ABCDIV
199700020318     C     K             OCCUR     ABCDIVN
199800020318     C     K             OCCUR     ABCDIVND
199900000000     C* NUOVO
200000000000IF  2C     DESCLN        IFEQ      'Y'
200100000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
200200000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
200300000000     C                   ADD       1             CND(MM)                        NUMERO CLIENTI
200400000830E   3C                   ENDIF
200500020319     C     MM            IFNE      13
200600020319     C                   IF        DESCLV <> 'D'
200700020319     C                   ADD       CONNTW        CNDN(MM)                       NUMERO CLIENTI
200800020318     C                   ELSE
200900020319     C                   ADD       CONNTW        CNDND(MM)                      NUMERO CLIENTI
201000020318     C                   ENDIF
201100020319     C                   ENDIF
201200000000     C                   ADD       WFDNSP        SND(MM)                        SPEDIZIONI
201300000000     C                   ADD       WFDRRN        RND(MM)                        RICAVI REALI DELTA
201400000000     C                   ADD       WFDRPN        PND(MM)                        RICAVI PRESUNTI DELT
201500020318     C                   IF        DESCLV <> 'D'
201600020318     C                   ADD       WFDNSP        SNDN(MM)                       SPEDIZIONI
201700020318     C                   ADD       WFDRRN        RNDN(MM)                       RICAVI REALI DELTA
201800020318     C                   ADD       WFDRPN        PNDN(MM)                       RICAVI PRESUNTI DELT
201900020318     C                   ELSE
202000020318     C                   ADD       WFDNSP        SNDND(MM)                      SPEDIZIONI
202100020318     C                   ADD       WFDRRN        RNDND(MM)                      RICAVI REALI DELTA
202200020318     C                   ADD       WFDRPN        PNDND(MM)                      RICAVI PRESUNTI DELT
202300020318     C                   ENDIF
202400000000     C* ACQUISITO
202500000000X   2C                   ELSE
202600000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
202700000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
202800000000     C                   ADD       1             CAD(MM)                        NUMERO CLIENTI
202900000830E   3C                   ENDIF
203000020319     C     MM            IFNE      13
203100020319     C                   IF        DESCLV <> 'D'
203200020319     C                   ADD       CONNTW        CADN(MM)                       NUMERO CLIENTI
203300020318     C                   ELSE
203400020319     C                   ADD       CONNTW        CADND(MM)                      NUMERO CLIENTI
203500020318     C                   ENDIF
203600020319     C                   ENDIF
203700000000     C                   ADD       WFDNSP        SAD(MM)                        SPEDIZIONI
203800000000     C                   ADD       WFDRRN        RAD(MM)                        RICAVI REALI DELTA
203900000000     C                   ADD       WFDRPN        PAD(MM)                        RICAVI PRESUNTI DELT
204000020318     C                   IF        DESCLV <> 'D'
204100020318     C                   ADD       WFDNSP        SADN(MM)                       SPEDIZIONI
204200020318     C                   ADD       WFDRRN        RADN(MM)                       RICAVI REALI DELTA
204300020318     C                   ADD       WFDRPN        PADN(MM)                       RICAVI PRESUNTI DELT
204400020318     C                   ELSE
204500020318     C                   ADD       WFDNSP        SADND(MM)                      SPEDIZIONI
204600020318     C                   ADD       WFDRRN        RADND(MM)                      RICAVI REALI DELTA
204700020318     C                   ADD       WFDRPN        PADND(MM)                      RICAVI PRESUNTI DELT
204800020318     C                   ENDIF
204900000000E   2C                   ENDIF
205000000000     C*---
205100000000     C* TOT CLIENTI 13^ COLONNA
205200000000     C*---
205300000000     C     Q             OCCUR     ABCD13
205400020318     C     K             OCCUR     ABCD13N
205500020318     C     K             OCCUR     ABCD13ND
205600951227     C* NUOVO
205700951227IF  2C     DESCLN        IFEQ      'Y'
205800951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
205900951227     C                   ADD       CO2KSC        NTD(2)
206000020318     C                   IF        DESCLV <> 'D'
206100020319     C                   ADD       CONNTW        NTDN(2)
206200020318     C                   ELSE
206300020319     C                   ADD       CONNTW        NTDND(2)
206400020318     C                   ENDIF
206500030924X   3C                   ELSE                                                   *PERIODO CORRENTE
206600030924     C                   ADD       CONKSC        NTD(1)
206700030924     C                   IF        DESCLV <> 'D'
206800030924     C                   ADD       CONNTW        NTDN(1)
206900030924     C                   ELSE
207000030924     C                   ADD       CONNTW        NTDND(1)
207100020318     C                   ENDIF
207200951227E   3C                   ENDIF
207300951227     C* ACQUISITO
207400951227X   2C                   ELSE
207500951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
207600951227     C                   ADD       CO2KSC        ATD(2)
207700020318     C                   IF        DESCLV <> 'D'
207800020319     C                   ADD       CONNTW        ATDN(2)
207900020318     C                   ELSE
208000020319     C                   ADD       CONNTW        ATDND(2)
208100020318     C                   ENDIF
208200951227X   3C                   ELSE                                                   *PERIODO CORRENTE
208300951227     C                   ADD       CONKSC        ATD(1)
208400020318     C                   IF        DESCLV <> 'D'
208500020319     C                   ADD       CONNTW        ATDN(1)
208600020318     C                   ELSE
208700020319     C                   ADD       CONNTW        ATDND(1)
208800020318     C                   ENDIF
208900951227E   3C                   ENDIF
209000951227E   2C                   ENDIF
209100000000     C*--------------------
209200000000     C* G E N E R A L E
209300000000     C*--------------------
209400000000     C     Q             OCCUR     ABCGEN
209500020318     C     K             OCCUR     ABCGENN
209600020318     C     K             OCCUR     ABCGENND
209700000000     C* NUOVO
209800000000IF  2C     DESCLN        IFEQ      'Y'
209900000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
210000000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
210100000830     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
210200000000     C                   ADD       1             CNG(MM)                        NUMERO CLIENTI
210300000830E   3C                   ENDIF
210400020319     C     MM            IFNE      13
210500020319     C                   IF        DESCLV <> 'D'
210600020319     C                   ADD       CONNTW        CNGN(MM)                       NUMERO CLIENTI
210700020318     C                   ELSE
210800020319     C                   ADD       CONNTW        CNGND(MM)                      NUMERO CLIENTI
210900020318     C                   ENDIF
211000020319     C                   ENDIF
211100000000     C                   ADD       WFDNSP        SNG(MM)                        SPEDIZIONI
211200000000     C                   ADD       WFDRRN        RNG(MM)                        RICAVI REALI DELTA
211300000000     C                   ADD       WFDRPN        PNG(MM)                        RICAVI PRESUNTI DELT
211400020318     C                   IF        DESCLV <> 'D'
211500020318     C                   ADD       WFDNSP        SNGN(MM)                       SPEDIZIONI
211600020318     C                   ADD       WFDRRN        RNGN(MM)                       RICAVI REALI DELTA
211700020318     C                   ADD       WFDRPN        PNGN(MM)                       RICAVI PRESUNTI DELT
211800020318     C                   ELSE
211900020318     C                   ADD       WFDNSP        SNGND(MM)                      SPEDIZIONI
212000020318     C                   ADD       WFDRRN        RNGND(MM)                      RICAVI REALI DELTA
212100020318     C                   ADD       WFDRPN        PNGND(MM)                      RICAVI PRESUNTI DELT
212200020318     C                   ENDIF
212300000000     C* ACQUISITO
212400000000X   2C                   ELSE
212500000830IF  3C     MM            IFNE      13                                           SOLO PERIODO CORR.TE
212600000830     C     SFCCL(MM)     ANDNE     '1'                                          NO CONTATO X FASCE<>
212700000830     C                   MOVEL     '1'           SFCCL(MM)                      *GIA' CONTATTO
212800000000     C                   ADD       1             CAG(MM)                        NUMERO CLIENTI
212900000830E   3C                   ENDIF
213000020319     C     MM            IFNE      13
213100020319     C                   IF        DESCLV <> 'D'
213200020319     C                   ADD       CONNTW        CAGN(MM)                       NUMERO CLIENTI
213300020318     C                   ELSE
213400020319     C                   ADD       CONNTW        CAGND(MM)                      NUMERO CLIENTI
213500020318     C                   ENDIF
213600020319     C                   ENDIF
213700000000     C                   ADD       WFDNSP        SAG(MM)                        SPEDIZIONI
213800000000     C                   ADD       WFDRRN        RAG(MM)                        RICAVI REALI DELTA
213900000000     C                   ADD       WFDRPN        PAG(MM)                        RICAVI PRESUNTI DELT
214000020318     C                   IF        DESCLV <> 'D'
214100020318     C                   ADD       WFDNSP        SAGN(MM)                       SPEDIZIONI
214200020318     C                   ADD       WFDRRN        RAGN(MM)                       RICAVI REALI DELTA
214300020318     C                   ADD       WFDRPN        PAGN(MM)                       RICAVI PRESUNTI DELT
214400020318     C                   ELSE
214500020318     C                   ADD       WFDNSP        SAGND(MM)                      SPEDIZIONI
214600020318     C                   ADD       WFDRRN        RAGND(MM)                      RICAVI REALI DELTA
214700020318     C                   ADD       WFDRPN        PAGND(MM)                      RICAVI PRESUNTI DELT
214800020318     C                   ENDIF
214900000000E   2C                   ENDIF
215000000000     C*---
215100000000     C* TOT CLIENTI 13^ COLONNA
215200000000     C*---
215300000000     C     Q             OCCUR     ABCG13
215400020318     C     K             OCCUR     ABCG13N
215500020318     C     K             OCCUR     ABCG13ND
215600000000     C* NUOVO
215700000000IF  2C     DESCLN        IFEQ      'Y'
215800951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
215900951227     C                   ADD       CO2KSC        NTG(2)
216000020318     C                   IF        DESCLV <> 'D'
216100020319     C                   ADD       CONNTW        NTGN(2)
216200020318     C                   ELSE
216300020319     C                   ADD       CONNTW        NTGND(2)
216400020318     C                   ENDIF
216500951227X   3C                   ELSE                                                   *PERIODO CORRENTE
216600951227     C                   ADD       CONKSC        NTG(1)
216700020318     C                   IF        DESCLV <> 'D'
216800020319     C                   ADD       CONNTW        NTGN(1)
216900020318     C                   ELSE
217000020319     C                   ADD       CONNTW        NTGND(1)
217100020318     C                   ENDIF
217200951227E   3C                   ENDIF
217300000000     C* ACQUISITO
217400000000X   2C                   ELSE
217500951227IF  3C     MM            IFEQ      13                                           *PERIODO PRECEDENTE
217600951227     C                   ADD       CO2KSC        ATG(2)
217700020318     C                   IF        DESCLV <> 'D'
217800020319     C                   ADD       CONNTW        ATGN(2)
217900020318     C                   ELSE
218000020319     C                   ADD       CONNTW        ATGND(2)
218100020318     C                   ENDIF
218200951227X   3C                   ELSE                                                   *PERIODO CORRENTE
218300951227     C                   ADD       CONKSC        ATG(1)
218400020318     C                   IF        DESCLV <> 'D'
218500020319     C                   ADD       CONNTW        ATGN(1)
218600020318     C                   ELSE
218700020319     C                   ADD       CONNTW        ATGND(1)
218800020318     C                   ENDIF
218900951227E   3C                   ENDIF
219000000000E   2C                   ENDIF
219100000000     C*
219200020318E   1C                   ENDIF
219300000000     C*
219400000000     C                   ENDSR
219500000000     C*------------------------------------------------------------------------*
219600100204     /*?  TOTCLI_NTW_TS  somma sulle Ds multiple x NTW e Tipo servizio        ?
219700000000     C*------------------------------------------------------------------------*
219800100201     C     TOTCLI_NTW_TS BEGSR
219900100201      **
220000100201     c                   clear                   rec_Ntw_TSv_OK    1
220100100201      **
220200100201      **  Controlla se il record è da prendere in considerazione
220300100201IF  1C                   IF        WFDNTW = *blank
220400100202     c                   eval             rec_Ntw_TSv_OK = 'S'
220500100201     c                   movel(p)  'ITA'         cod_network       3
220600100201IF  1C                   ELSE
220700100202     c                   eval             rec_Ntw_TSv_OK = 'S'
220800100201     c                   movel(p)  WFDNTW        cod_network
220900100201     c                   END
221000100201     c**
221100100201     c                   eval      numElem = %Elem(W1ntwTS)
221200100201     c**
221300100201      **  Quindi se da totalizzare cerca su quale occurence deve metterlo.
221400160509     c                   do        numElem       kk                3 0
221500160509     C     Kk            OCCUR     W1ntwTS
221600100202      *
221700100202     ** inserisce se non era stato ancora inserito
221800100202     c                   if         c1ntwTS = *BLANK and c1tpsTS = *BLANK
221900100202     c                   eval       c1ntwTS = cod_network
222000100202     c                   eval       c1tpsTS = wfdTSP
222100100202     c                   end
222200100202      *
222300100202     c** Solo se corrisponde la chiave NTW + TServ.
222400100202     c                   IF         c1ntwTS = cod_network and c1tpsTS = wfdTSP
222500100201     c**
222600100201IF  1C     MM            ifLE      12
222700100202     c**
222800100202     ** aggiunge x l'anno in corso
222900100201     C                   ADD       WFDNSP        S1ntwTS(MM)                        SPEDIZIONI
223000100201     C                   eval      Kg1ntwTS(MM) = wfdPKG + Kg1ntwTS(MM)             PESI in KG
223100100201     C                   ADD       WFDRRN        R1ntwTS(MM)                        RICAVI REALI DELTA
223200100201     C                   ADD       WFDRPN        P1ntwTS(MM)                        RICAVI PRESUNTI DELT
223300100201     C                   z-add(h)  WFDRRN        R1totale
223400100201     C                   z-add(h)  WFDRPN        P1totale
223500100201     C                   ADD       R1totale      TR1ntwTS
223600100201     C                   ADD       P1totale      TP1ntwTS
223700100201     c                   leave
223800100201      **
223900100201     c                   else
224000100201      **
224100100201IF  1C     WFDmes        ifLE      12
224200100202     ** aggiunge x l'anno precedente
224300100201     C                   eval      ap_S1ntwTS(WFDmes) = WFDNSP +
224400100201     C                                ap_S1ntwTS(WFDmes)                              SPEDIZIONI
224500100201     C                   eval      ap_Kg1ntwTS(WFDmes)= wfdPKG +
224600100201     C                                ap_Kg1ntwTS(WFDmes)                             PESI in KG
224700100201     C                   eval      ap_R1ntwTS(WFDmes) = WFDRRN +
224800100201     C                                ap_R1ntwTS(WFDmes)                           RICAVI REALI DELT
224900100201     C                   eval      ap_P1ntwTS(WFDmes) = WFDRPN +
225000100218     C                                ap_P1ntwTS(WFDmes)                           RICAVI PRESUNTI D
225100100201     C                   z-add(h)  WFDRRN        R1totale
225200100201     C                   z-add(h)  WFDRPN        P1totale
225300100201     C                   ADD       R1totale      ap_TR1ntwTS
225400100201     C                   ADD       P1totale      ap_TP1ntwTS
225500100201     c                   leave
225600100201     c                   end
225700100201      **
225800100201     c                   end
225900100202     c                   end
226000100202      **
226100100202     C                   ENDdo
226200100202     c**
226300100201     C                   ENDSR
226400100201     C*------------------------------------------------------------------------*
226500100201     C*?STAKSC - IMPOSTA STAMPA VALORI CLIENTE
226600100201     C*------------------------------------------------------------------------*
226700100201     C     STAKSC        BEGSR
226800100201     C*
226900020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
227000020222     C                   EVAL      DEPSTPD = '0'
227100000000     C*
227200000000     C* STAMPA I VALORI DEL CLIENTE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
227300000000     C* LA STATISTICA DELTA CON DETTAGLIO CLIENTE
227400000000IF  1C     PASKSC        IFEQ      '1'
227500000000     C     PARSTA        ANDEQ     'K'
227600000000     C                   MOVEL     '1'           ORAKSC                         *ORA STAMPA CLIENTE
227700000000     C***
227800000000     C* DESCRIZIONI INTESTAZIONE
227900000000     C***
228000020116     C                   CLEAR                   SE94IN
228100000000     C                   Z-ADD     DEPAGE        INCAGE                         *AGENTE
228200000000     C                   MOVEL     DESAGE        INDAGE                         *DESCRIZIONE AGENTE
228300000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
228400000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
228500000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
228600000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
228700000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
228800000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
228900950301     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
229000000000     C                   MOVEL     DRI(20)       PIDDES                         *RETTANGOLINO
229100020221     C                   MOVEL     *BLANKS       P1DFLD                         *RETTANGOLINO
229200030311     C*
229300030311     C* PREPARA BUFFER FILE PC (WFDELPCF)
229400030311     C                   EXSR      WRIFPC
229500000000     C*
229600000000     C* STAMPA I VALORI DEL CLIENTE
229700020221     C                   EXSR      STMKSC
229800000000     C*
229900000000E   1C                   ENDIF
230000000000     C*
230100000000     C                   ENDSR
230200030311     C*------------------------------------------------------------------------*
230300030311     C*?WRIFPC - IMPOSTA BUFFER FILE PC (WFDELPCF)
230400030311     C*------------------------------------------------------------------------*
230500030311     C     WRIFPC        BEGSR
230600030311     C*
230700100406     C                   Z-ADD     INCAGE        aaaAGE            7 0          *AGENTE
230800100406     C                   MOVEL(p)  INDAGE        aaaAGD           25            *DESCRIZIONE AGENTE
230900100406     C                   Z-ADD     INCFIL        aaaFIL            3 0          *FILIALE
231000100406     C                   MOVEL(p)  INDFIL        aaaFID           25            *DESCRIZIONE FILIALE
231100100406     C                   Z-ADD     INCARE        aaaARE            3 0          *AREA
231200100406     C                   MOVEL(p)  INDARE        aaaARD           25            *DESCRIZIONE AREA
231300100406     C                   MOVEL(p)  INCDIV        aaaDIV            1            *DISTRETTO
231400100406     C                   MOVEL(p)  INDDIV        aaaDID           25            *DESCRIZIONE DISTRET
231500100406     C                   MOVEL(p)  parIOE        aaaIOE            1            *DELTA ITALIA/ESTERO
231600100406     C                   Z-ADD     PARAAI        aaaAAD            4 0          *ANNO DA
231700100406     C                   Z-ADD     PARMMI        aaaMMD            2 0          *MESE DA
231800100406     C                   Z-ADD     PARAAF        aaaAAA            4 0          *ANNO A
231900100406     C                   Z-ADD     PARMMF        aaaMMA            2 0          *MESE A
232000100406     C                   Z-ADD     DEPCLI        aaaCLI            7 0          *CODICE CLIENTE
232100100406     C                   MOVEL(p)  DESKSC        aaaCLD           25            *DESCRIZIONE CLIENTE
232200100406     C                   MOVEL(p)  DESCLV        aaaCLV            1            *CODICE IMPORTANZA C
232300100406     C                   MOVEL(p)  DESITC        aaaITC           25            *CATEGORIA CLIENTE
232400070924     C*****
232500100406     C                   MOVEL(p)  'A'           aaaCLN            1
232600070924IF  1C     DESCLN        IFne      *blank                                       *CL.NUOVO SE "Y" o "
232700100406     C                   MOVEL     'N'           aaaCLN
232800070924E   1C                   ENDIF
232900030311     C*
233000030311     C                   ENDSR
233100000000     C*------------------------------------------------------------------------*
233200000000     C*?STMKSC - STAMPA I VALORI DEL CLIENTE
233300000000     C*------------------------------------------------------------------------*
233400000000     C     STMKSC        BEGSR
233500061220     C*
233600061220     C* Per controllare se il cliente da stampare è anagraficamente Bloccato
233700061220     C*   nella totalità ossia se è uniuficante di se stesso oppure se ha
233800061220     C*   tutti i codici figli oltre se stesso Bloccati.
233900061220     C                   CLEAR                   Total_Bloccato    1
234000061220     C*
234100000000     C*--------------------
234200000000     C* I N T E S T A Z I O N E
234300000000     C*--------------------
234400000000IF  1C     *IN01         IFEQ      *ON
234500000000IF  2C     PRIGIR        IFEQ      '0'
234600000000     C                   MOVEL     '1'           PRIGIR
234700000000X   2C                   ELSE
234800100129     c                   if        parSTAMPA <> 'N'
234900020116     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
235000100129     c                   end
235100000000E   2C                   ENDIF
235200020222     C*
235300020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
235400020222     C                   IF        FLGSTPLE = '1'
235500100129     c                   if        parSTAMPA <> 'N'
235600020222     C                   WRITE     SE94LE                                       *LEGENDA
235700100129     c                   end
235800020222     C                   ELSE
235900020222     C                   EVAL      FLGSTPLE = '1'
236000020222     C                   ENDIF
236100020222     C*
236200100129     c                   if        parSTAMPA <> 'N'
236300020222     C                   WRITE     SE94PT                                       *TESTATA
236400020116     C                   WRITE     SE94IN                                       *INTESTAZIONE
236500020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
236600020116     C                   WRITE     SE94PI                                       *RIGA MESI
236700020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
236800100129     c                   end
236900000000     C                   SETOFF                                       01
237000000000E   1C                   ENDIF
237100020222     C*--------------------
237200000000     C* S P E D I Z I O N I
237300000000     C*--------------------
237400000000     C                   Z-ADD     S1K           $VA
237500020116     C                   Z-ADD     *ZEROS        $VA13C           19 5          *PERIODO CORRENTE
237600000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
237700000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
237800000000E   1C                   ENDDO
237900020116     C                   Z-ADD     *ZEROS        $V213C           19 5          *PERIODO PRECEDENTE
238000951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
238100000000     C***
238200000000     C* CODICE + RAGIONE SOCIALE
238300000000     C***
238400000825     C                   MOVEL     DEPCLI        DES07             7            *CODICE CLIENTE
238500000000     C                   MOVEL     DESKSC        DES12            12            *RAGIONE SOCIALE
238600000000     C*
238700000000     C                   MOVEL     *BLANKS       DES20
238800000000     C                   MOVEL     DES07         DES20            20
238900000000     C                   MOVE      DES12         DES20
239000000000     C*
239100000000     C                   MOVEL     *BLANKS       P1DDES
239200000000     C                   MOVEL     DES20         P1DDES
239300000000     C                   MOVE      'SPE'         P1DDES
239400020222     C                   MOVEL     *BLANKS       P1DFLD
239500000000     C*
239600000000     C* SCRIVE I VALORI IN STAMPA
239700000000     C                   EXSR      SCRVAL
239800030311     C*
239900030311     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
240000100406     c                   z-add     P1CVAP        aaaSAP           11 0
240100100406     c                   z-add     P1CVAT        aaaSAC           11 0
240200000000     C*--------------------
240300000000     C* R I C A V I
240400000000     C*--------------------
240500020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
240600020116     C*    R1K           DIV       1000          $VA
240700020116     C                   Z-ADD     R1K           $VA
240800020116     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
240900000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
241000020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
241100000000E   1C                   ENDDO
241200020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
241300951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
241400000000     C***
241500160913     C* CLIENTE NUOVO/ACQUISITO + DI TIPO A/B/C  ed /E
241600000000     C***
241700070822      *?Se il cliente ha spedizioni sia com Italia che come estero ed è NUOVO,
241800070822      *?potrebbe accadere che il mese con la "Y" abbia solo un tipo di spedizioni,
241900070822      *?quindi, se facessimo il test sulla "Y" in una stampa risulterebbe Nuovo e
242000070822      *?nell'altra invece com Acquisito. OCCORRE testare semplicemente se <> da Blk.
242100070822IF  1C     DESCLN        IFne      *blank                                       *CL.NUOVO SE "Y" o "
242200000000     C                   MOVEL     DRI(17)       DES09             9
242300000000X   1C                   ELSE
242400000000     C                   MOVEL     DRI(18)       DES09             9
242500000000E   1C                   ENDIF
242600000829     C                   EVAL      DES20 =  DES09 +
242700000829     C                                      ' / ' +
242800000829     C                                      DESCLV
242900000000     C                   MOVEL     *BLANKS       P1DDES
243000000000     C                   MOVEL     DES20         P1DDES
243100000000     C                   MOVE      'RIC'         P1DDES
243200020222     C                   MOVEL     *BLANKS       P1DFLD
243300000000     C*
243400000000     C* SCRIVE I VALORI IN STAMPA
243500000000     C                   EXSR      SCRVAL
243600030311     C*
243700030311     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
243800100406     c                   z-add     P1CVAP        aaaFAP           15 3
243900100406     c                   z-add     P1CVAT        aaaFAC           15 3
244000000000     C*--------------------
244100000000     C* D E L T A
244200000000     C*--------------------
244300000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
244400000000     C                   Z-ADD     R1K           $RR                            *RICAVI REALI
244500000000     C                   Z-ADD     P1K           $RP                            *RICAVI PRESUNTI
244600000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
244700000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
244800000000     C                   EXSR      IMPCAL
244900970822     C***
245000970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
245100970822     C***
245200970822     C                   MOVEL     D1K           $SD                            *FALG SEGNALAZIONE
245300000000     C***
245400951006     C* CATEGORIA SEAT
245500000000     C***
245600000000     C                   MOVEL     *BLANKS       DES20
245700951006     C                   MOVEL     DESITC        DES20            20
245800000000     C*
245900000000     C                   MOVEL     *BLANKS       P2DDES
246000020221     C                   MOVEL     *BLANKS       P2DFLD
246100000000     C                   MOVEL     DES20         P2DDES
246200000000     C                   MOVE      'DEL'         P2DDES
246300020222     C                   MOVEL     *BLANKS       P2DFLD
246400020701     C*
246500020701     C* VERIFICO LO SCASTAMENTO % RISPETTO ALLO STESSO MESE/PERIODO DELL?ANNO PRECEDENTE
246600020701     C                   EXSR      REPDELFAT
246700000000     C*
246800000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
246900000000     C                   EXSR      SCRDEL
247000030924     C*
247100030924     C* SALVO I DATI PER LA SCRITTURA DEL RECORD FILE PC (WFDELPCF)
247200100406     c                   z-add     P2CDEP        aaaDAP            4 1
247300100406     c                   z-add     P2CDET        aaaDAC            4 1
247400030924     C*
247500030924     C* INIZIALIZZO LA SCHIERA DI WRK X SCOSTAMENTO FATTURATO ED I RELATIVI CAMPI DI STAMPA
247600030924     C                   EXSR      INZPDF
247700980406     C*--------------------
247800000828     C* DESCRIZIONE BLOCCO + CLIENTI FIGLI
247900980406     C*--------------------
248000000828     C                   MOVEL     'N'           $STRIG
248100020116     C                   CLEAR                   SE94P3
248200130318IF  1C     DESABL        IFne      *blank
248300130318IF  1C     DESABL        andne     '*'
248400980406     C     DESCBL        ORNE      *BLANKS
248500980406     C                   MOVEL     DESCBL        P3DDES
248600000828     C                   MOVEL     'S'           $STRIG                         *RIGA DA STAMPARE
248700061220     C                   movel     'S'           Total_Bloccato
248800980406E   1C                   ENDIF
248900000828     C*
249000000829     C                   Z-ADD     *ZEROS        BLOCCO            2 0          *BLOCCO CL STAMPATI
249100000828IF  1C     PARSCE        IFEQ      'U'                                          *STAT. UNIFICANTE
249200000828IF  2C     SKSC11(2)     IFGT      *ZEROS                                       *ESISTONO FIGLI
249300000828     C                   Z-ADD     2             I
249400000828DO  3C     I             DOWLE     500
249500000828     C     SKSC11(I)     ANDGT     *ZEROS
249600000828     C                   Z-ADD     *ZEROS        IS                             *N.CLIENTI X RIGA
249700000828     C                   CLEAR                   SKSC
249800051031     C                   clear                   skbl
249900051031      *
250000000828DO  4C     I             DOWLE     500
250100000828     C     SKSC11(I)     ANDGT     *ZEROS
250200051031     C**** ridotta da 19 a 17 per far spazio ai flags x individuare i codici bloccati
250300051031     C     IS            ANDLT     17
250400000828     C                   ADD       1             IS                             *CLIENTI NELLA RIGA
250500000828     C                   Z-ADD     SKSC11(I)     SKSC(IS)
250600051031     C*--------------------
250700051031     C* controllo codice cliente bloccato
250800051031     C*--------------------
250900051031     C                   CLEAR                   BS69DS
251000051031     C                   CLEAR                   ACODS
251100051031     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
251200051031     C                   MOVEL     SKSC(IS)      I69KAC                         *CLIENTE X CNACO
251300120125     C                   MOVEL     SKSC(IS)      I69KCP                         *CLIENTE X CNCLP
251400051031     C                   CALL      'TIBS69R'
251500051031     C                   PARM                    BS69DS
251600051031     C                   PARM                    ACODS
251700051031     C                   PARM                    INDDS
251800051031     C                   PARM                    CLPDS
251900051031     C                   PARM                    CLSDS
252000051031IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
252100130315IF  1C     acoabl        andne     *blank                                       *cliente bloccato
252200130315IF  1C     acoabl        andne     '*'                                          *cliente bloccato
252300051031     C                   move      '+'           skbl(is)
252400120125      **
252500120125      **  Reperisce la Data di quando è stato fatto il Blocco Contabile sul cliente
252600120125     C                   clear                   TrulBLCds
252700120125     c                   eval      IblcTLA = 'L'
252800120125     c                   z-add     acoKcC        IblcKCC
252900120125     c                   z-add     acoKsC        IblcKSC
253000120125     c                   call      'TRULBLCR'
253100120125     C                   PARM                    KPJBA
253200120125     C                   PARM                    TrulBLCds
253300120125      **
253400120125      **  deve memorizzare il più recente blocco effettuato
253500120125      **   codice e data blocco dell'ultimo cliente bloccato fra quelli unificati.
253600120125     c                   if         Data_Blocco_Ultimo < OblcDAV
253700120125     c                   eval           Data_Blocco_Ultimo  = OblcDAV
253800120125     c                   eval           Cod_Blocco_Ultimo   = clpNAR
253900120125     c                   end
254000120125      **
254100051031E   1C                   ENDIF
254200120125      ***
254300120131      ***  riporta le date da 6 a 8 per effettuare il confronto
254400120131     c                   eval      Data_Prima_Fat_8 = 0
254500120131     c                   eval       CLP_Prima_Fat_8 = 0
254600120131      *
254700120131     c                   clear                   Anno2_0           2 0
254800120131     c                   IF        Data_Prima_Fat_6 > 0
254900120131     c                   eval              Anno2_0 = Data_Prima_Fat_6/10000
255000120131     c                   if           Anno2_0 < 40
255100120131     c                   eval      Data_Prima_Fat_8 = 20000000 +
255200120131     c                                            Data_Prima_Fat_6
255300120131     c                   else
255400120131     c                   eval      Data_Prima_Fat_8 = 19000000 +
255500120131     c                                            Data_Prima_Fat_6
255600120131     c                   end
255700120131     c                   endIF
255800120131      *
255900120131     c                   clear                   Anno2_0           2 0
256000120131     c                   IF        CLPdps > 0
256100120131     c                   eval              Anno2_0 = CLPdps/10000
256200120131     c                   if           Anno2_0 < 40
256300120131     c                   eval      CLP_Prima_Fat_8 = 20000000 + CLPdps
256400120131     c                   else
256500120131     c                   eval      CLP_Prima_Fat_8 = 19000000 + CLPdps
256600120131     c                   end
256700120131     c                   endIF
256800120131      *
256900120125      **  deve memorizzare, fra tutti i clienti unificati, la più vecchia data
257000120125      ***   quindi
257100120125      **  la prima spedizione fatturata.
257200120131     C*
257300120131     c                   if             Data_Prima_Fat_8 > CLP_Prima_Fat_8 or
257400120131     c                                  Data_Prima_Fat_8 = 0
257500120213     c                   if                 CLPdps > 0
257600120125     c                   eval        Data_Prima_Fat_6 = CLPdps
257700120125     c                   end
257800120213     c                   end
257900061220      ***
258000061220      *  se almeno un codice figlio non è bloccato non lo deve segnalare come
258100061220      *  totalità.
258200061220     c                   if        skbl(is) <> '+'
258300061220     C                   movel     *blank        Total_Bloccato
258400061220     c                   end
258500051031     C*--------------------
258600000828     C                   ADD       1             I                              *CLIENTI
258700000828E   4C                   ENDDO                                                  *ROTTURA BLOCCO
258800000828     C*
258900051031     C* STAMPA BLOCCO DI CLIENTI unificati
259000051031     C*
259100000828IF  4C     IS            IFGT      *ZEROS                                       *CLIENTI DA STAMPARE
259200000828     C     $STRIG        OREQ      'S'                                          *RIGA DA STAMPARE
259300000829     C                   ADD       1             BLOCCO                         INCREMENTA N.BLOCCHI
259400000828     C                   Z-ADD     SKSC(1)       P3CC01
259500000828     C                   Z-ADD     SKSC(2)       P3CC02
259600000828     C                   Z-ADD     SKSC(3)       P3CC03
259700000828     C                   Z-ADD     SKSC(4)       P3CC04
259800000828     C                   Z-ADD     SKSC(5)       P3CC05
259900000828     C                   Z-ADD     SKSC(6)       P3CC06
260000000828     C                   Z-ADD     SKSC(7)       P3CC07
260100000828     C                   Z-ADD     SKSC(8)       P3CC08
260200000828     C                   Z-ADD     SKSC(9)       P3CC09
260300000828     C                   Z-ADD     SKSC(10)      P3CC10
260400000828     C                   Z-ADD     SKSC(11)      P3CC11
260500000828     C                   Z-ADD     SKSC(12)      P3CC12
260600000828     C                   Z-ADD     SKSC(13)      P3CC13
260700000828     C                   Z-ADD     SKSC(14)      P3CC14
260800000828     C                   Z-ADD     SKSC(15)      P3CC15
260900000828     C                   Z-ADD     SKSC(16)      P3CC16
261000000828     C                   Z-ADD     SKSC(17)      P3CC17
261100051031      **  Aggiunti Flag x cliente bloccato sotto unificante
261200051031      **   x questo ridotta la schiera SKSC per fare spazio in stampa
261300051031     C                   move      skbl(1)       P3Cf01
261400051031     C                   move      skbl(2)       P3Cf02
261500051031     C                   move      skbl(3)       P3Cf03
261600051031     C                   move      skbl(4)       P3Cf04
261700051031     C                   move      skbl(5)       P3Cf05
261800051031     C                   move      skbl(6)       P3Cf06
261900051031     C                   move      skbl(7)       P3Cf07
262000051031     C                   move      skbl(8)       P3Cf08
262100051031     C                   move      skbl(9)       P3Cf09
262200051031     C                   move      skbl(10)      P3Cf10
262300051031     C                   move      skbl(11)      P3Cf11
262400051031     C                   move      skbl(12)      P3Cf12
262500051031     C                   move      skbl(13)      P3Cf13
262600051031     C                   move      skbl(14)      P3Cf14
262700051031     C                   move      skbl(15)      P3Cf15
262800051031     C                   move      skbl(16)      P3Cf16
262900051031     C                   move      skbl(17)      P3Cf17
263000000829IF  5C     BLOCCO        IFEQ      1                                            *1° BLOCCO DA STAMP.
263100000829     C                   MOVEL     DRI(28)       P3DCLI
263200000829E   5C                   ENDIF
263300020131     C*
263400020131     C* PRIMA DI OGNI RIGA DA STAMPARE VERIFICA IL SALTO PAGINA
263500020131IF  1C     *IN01         IFEQ      *ON
263600020131IF  2C     PRIGIR        IFEQ      '0'
263700020131     C                   MOVEL     '1'           PRIGIR
263800020131X   2C                   ELSE
263900100129     c                   if        parSTAMPA <> 'N'
264000020131     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
264100100129     c                   end
264200020131E   2C                   ENDIF
264300020222     C*
264400020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
264500020222     C                   IF        FLGSTPLE = '1'
264600100129     c                   if        parSTAMPA <> 'N'
264700020222     C                   WRITE     SE94LE                                       *LEGENDA
264800100129     c                   end
264900020222     C                   ELSE
265000020222     C                   EVAL      FLGSTPLE = '1'
265100020222     C                   ENDIF
265200020222     C*
265300100129     c                   if        parSTAMPA <> 'N'
265400020222     C                   WRITE     SE94PT                                       *TESTATA
265500020131     C                   WRITE     SE94IN                                       *INTESTAZIONE
265600020131     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
265700020131     C                   WRITE     SE94PI                                       *RIGA MESI
265800020131     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
265900100129     c                   end
266000020131     C                   SETOFF                                       01
266100020131E   1C                   ENDIF
266200020221     C*
266300100129     c                   if        parSTAMPA <> 'N'
266400020116     C                   WRITE     SE94P3
266500100129     c                   end
266600020116     C                   CLEAR                   SE94P3                         AZZERA FORMATO
266700000828     C                   MOVEL     'N'           $STRIG                         RIGA DA NON STAMPARE
266800000828E   4C                   ENDIF
266900000828E   3C                   ENDDO                                                  *FINE FIGLI
267000000828E   2C                   ENDIF
267100000828E   1C                   ENDIF
267200000828     C*
267300000828IF  1C     $STRIG        IFEQ      'S'                                          *ANCORA DA STAMPARE
267400020131     C*
267500020131     C* PRIMA DI OGNI RIGA DA STAMPARE VERIFICA IL SALTO PAGINA
267600020131IF  1C     *IN01         IFEQ      *ON
267700020131IF  2C     PRIGIR        IFEQ      '0'
267800020131     C                   MOVEL     '1'           PRIGIR
267900020131X   2C                   ELSE
268000100129     c                   if        parSTAMPA <> 'N'
268100020131     C                   WRITE     SE94R2                                       RIGA SEPARAZIONE PRE
268200100129     c                   end
268300020131E   2C                   ENDIF
268400020222     C*
268500020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
268600020222     C                   IF        FLGSTPLE = '1'
268700100129     c                   if        parSTAMPA <> 'N'
268800020222     C                   WRITE     SE94LE                                       *LEGENDA
268900100129     c                   end
269000020222     C                   ELSE
269100020222     C                   EVAL      FLGSTPLE = '1'
269200020222     C                   ENDIF
269300020222     C*
269400100129     c                   if        parSTAMPA <> 'N'
269500020222     C                   WRITE     SE94PT                                       *TESTATA
269600020131     C                   WRITE     SE94IN                                       *INTESTAZIONE
269700020131     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
269800020131     C                   WRITE     SE94PI                                       *RIGA MESI
269900020131     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
270000100129     c                   end
270100020131     C                   SETOFF                                       01
270200020131E   1C                   ENDIF
270300020221     C*
270400100129     c                   if        parSTAMPA <> 'N'
270500020116     C                   WRITE     SE94P3
270600100129     c                   end
270700000828E   1C                   ENDIF
270800000000     C*--------------------
270900000000     C* R I G A   V U O T A
271000000000     C*--------------------
271100020116     C                   CLEAR                   SE94P1
271200100129     c                   if        parSTAMPA <> 'N'
271300020116     C                   WRITE     SE94P1
271400100129     c                   end
271500000000     C*
271600030311     C* SE RICHIESTO NEL LANBCIO INSERISCO IL RECORD NEL FILE PC (WFDELPCF)
271700030311     C                   IF        PARFPC = 'S'
271800061220     C*
271900061220     C* aggiunto campo di segnalazione cliente totalmente bloccato
272000061220     C*  come se stesso + tutti gli eventuali figli se unificante:
272100100406     c                   clear                   aaaBLO            1
272200120125     c                   clear                   aaaCULTBLO        3
272300120125     c                   clear                   aaaDULTBLO        8 0
272400120125     c                   clear                   aaaDPRIFAT        8 0
272500100406     c                   eval      aaaBLO = Total_Bloccato
272600120125      *
272700120125      **  solo se il codice unificante è Totalmente bloccato deve riportare
272800120125      **   il codice di blocco e la data dell'ultimo cliente che in ordine di tempo
272900120125      *   è stato bloccato.
273000120125     c                   if        Total_Bloccato = 'S'
273100120125     c                   eval       aaaCULTBLO =  Cod_Blocco_Ultimo
273200120125     c                   eval       aaaDULTBLO = Data_Blocco_Ultimo
273300120125     c                   end
273400120125      *  Inoltre si deve memorizzare quando è stata fatturata la prima spedizione
273500120125      *   fra tutti i clienti unificati.
273600120125     c                   clear                   Anno2_0           2 0
273700120125     c                   IF        Data_Prima_Fat_6 > 0
273800120125     c                   eval      Anno2_0 = Data_Prima_Fat_6/10000
273900120125     c                   if        Anno2_0 < 40
274000120125     c                   eval       aaaDPRIFAT = 20000000 + Data_Prima_Fat_6
274100120125     c                   else
274200120125     c                   eval       aaaDPRIFAT = 19000000 + Data_Prima_Fat_6
274300120125     c                   end
274400120125     c                   endIF
274500070418     C*
274600070418      * ?- Scrive il nuovo files WAdelpcf identico al WFdelpcf con i campi in +
274700070418      *              ? *---------------------*
274800070418     c                   exsr      wri_WAdel
274900070418      *              ? *---------------------*
275000100201     C*
275100100201     C*  Solo x Estero x Network
275200100201      * -------------
275300100201IF  1C     PARIOE        IFEQ      'E'
275400100201      *              ? *---------------------*
275500100201     C                   exsr      wriWADELntw
275600100201      *              ? *---------------------*
275700100201     C                   END
275800100201      * -------------
275900100216      * ?- Scrive il nuovo files WAdeApcI identico al WFdelpcf con i campi in + x NTW e Tipo Servizio
276000100201      *              ? *---------------------*
276100100201     c                   exsr      wri_WAdeA
276200100201      *              ? *---------------------*
276300100201     C*
276400030311     C                   ENDIF
276500030311     C*
276600000000     C                   ENDSR
276700070418     C*------------------------------------------------------------------------*
276800070418      *?  Scrive il nuovo WFile identico al WFDELPCF ma con informazioni più dettagliate    ?
276900070418     C*------------------------------------------------------------------------*
277000070418     C     wri_WAdel     BEGSR
277100070418     C*
277200070418      * ?Campi uguali al precedente WFile
277300070418     c                   clear                   WAdelPC0
277400100406     c                   eval      WaPAAD   =    aaaAAD
277500100406     c                   eval      WaPMMD   =    aaaMMD
277600100406     c                   eval      WaPAAA   =    aaaAAA
277700100406     c                   eval      WaPMMA   =    aaaMMA
277800100406     c                   eval      WaPIOE   =    aaaIOE
277900100406     c                   eval      WaPDIV   =    aaaDIV
278000100406     c                   eval      WaPDID   =    aaaDID
278100100406     c                   eval      WaPARE   =    aaaARE
278200100406     c                   eval      WaPARD   =    aaaARD
278300100406     c                   eval      WaPFIL   =    aaaFIL
278400100406     c                   eval      WaPFID   =    aaaFID
278500100406     c                   eval      WaPCLV   =    aaaCLV
278600100406     c                   eval      WaPCLI   =    aaaCLI
278700100406     c                   eval      WaPCLD   =    aaaCLD
278800100406     c                   eval      WaPITC   =    aaaITC
278900100406     c                   eval      WaPCLN   =    aaaCLN
279000100406     c                   eval      WaPAGE   =    aaaAGE
279100100406     c                   eval      WaPAGD   =    aaaAGD
279200100406     c                   eval      WaPSAP   =    aaaSAP
279300100406     c                   eval      WaPFAP   =    aaaFAP
279400100406     c                   eval      WaPDAP   =    aaaDAP
279500100406     c                   eval      WaPSAC   =    aaaSAC
279600100406     c                   eval      WaPFAC   =    aaaFAC
279700100406     c                   eval      WaPDAC   =    aaaDAC
279800100406     c                   eval      WaPBLO   =    aaaBLO
279900120125     c                   eval      WApCDULBLO =  aaaCULTBLO
280000120125     c                   eval      WApDTULBLO =  aaaDULTBLO
280100120125     c                   eval      WApDTPRIFA =  aaaDPRIFAT
280200081013      *
280300070418      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
280400070419     C*--------------------
280500070419     C* Anno Precedente 12 Mesi
280600070419     C*--------------------
280700070419     C***    Spedizioni mensili
280800070419     C                   Z-ADD     ap_S1K(1)     wapNS_01aP
280900070419     C                   Z-ADD     ap_S1K(2)     wapNS_02aP
281000070419     C                   Z-ADD     ap_S1K(3)     wapNS_03aP
281100070419     C                   Z-ADD     ap_S1K(4)     wapNS_04aP
281200070419     C                   Z-ADD     ap_S1K(5)     wapNS_05aP
281300070419     C                   Z-ADD     ap_S1K(6)     wapNS_06aP
281400070419     C                   Z-ADD     ap_S1K(7)     wapNS_07aP
281500070419     C                   Z-ADD     ap_S1K(8)     wapNS_08aP
281600070419     C                   Z-ADD     ap_S1K(9)     wapNS_09aP
281700070419     C                   Z-ADD     ap_S1K(10)    wapNS_10aP
281800070419     C                   Z-ADD     ap_S1K(11)    wapNS_11aP
281900070419     C                   Z-ADD     ap_S1K(12)    wapNS_12aP
282000081013     C***    Pesi mensili
282100081016     c                   clear                   WaPkgP
282200081016     c                   add       ap_Kg1K(1)    WaPkgP
282300081016     c                   add       ap_Kg1K(2)    WaPkgP
282400081016     c                   add       ap_Kg1K(3)    WaPkgP
282500081016     c                   add       ap_Kg1K(4)    WaPkgP
282600081016     c                   add       ap_Kg1K(5)    WaPkgP
282700081016     c                   add       ap_Kg1K(6)    WaPkgP
282800081016     c                   add       ap_Kg1K(7)    WaPkgP
282900081016     c                   add       ap_Kg1K(8)    WaPkgP
283000081016     c                   add       ap_Kg1K(9)    WaPkgP
283100081016     c                   add       ap_Kg1K(10)   WaPkgP
283200081016     c                   add       ap_Kg1K(11)   WaPkgP
283300081016     c                   add       ap_Kg1K(12)   WaPkgP
283400081013     C                   Z-ADD     ap_Kg1K(1)    wapKG_01aP
283500081013     C                   Z-ADD     ap_Kg1K(2)    wapKG_02aP
283600081013     C                   Z-ADD     ap_Kg1K(3)    wapKG_03aP
283700081013     C                   Z-ADD     ap_Kg1K(4)    wapKG_04aP
283800081013     C                   Z-ADD     ap_Kg1K(5)    wapKG_05aP
283900081013     C                   Z-ADD     ap_Kg1K(6)    wapKG_06aP
284000081013     C                   Z-ADD     ap_Kg1K(7)    wapKG_07aP
284100081013     C                   Z-ADD     ap_Kg1K(8)    wapKG_08aP
284200081013     C                   Z-ADD     ap_Kg1K(9)    wapKG_09aP
284300081013     C                   Z-ADD     ap_Kg1K(10)   wapKG_10aP
284400081013     C                   Z-ADD     ap_Kg1K(11)   wapKG_11aP
284500081013     C                   Z-ADD     ap_Kg1K(12)   wapKG_12aP
284600070419     C***  Ricavi Reali mensili
284700070419     C                   Z-ADD     ap_R1K(1)     wapRR_01aP
284800070419     C                   Z-ADD     ap_R1K(2)     wapRR_02aP
284900070419     C                   Z-ADD     ap_R1K(3)     wapRR_03aP
285000070419     C                   Z-ADD     ap_R1K(4)     wapRR_04aP
285100070419     C                   Z-ADD     ap_R1K(5)     wapRR_05aP
285200070419     C                   Z-ADD     ap_R1K(6)     wapRR_06aP
285300070419     C                   Z-ADD     ap_R1K(7)     wapRR_07aP
285400070419     C                   Z-ADD     ap_R1K(8)     wapRR_08aP
285500070419     C                   Z-ADD     ap_R1K(9)     wapRR_09aP
285600070419     C                   Z-ADD     ap_R1K(10)    wapRR_10aP
285700070419     C                   Z-ADD     ap_R1K(11)    wapRR_11aP
285800070419     C                   Z-ADD     ap_R1K(12)    wapRR_12aP
285900070419     C***  Ricavi Presunti mensili
286000070419     C                   Z-ADD     ap_P1K(1)     wapRP_01aP
286100070419     C                   Z-ADD     ap_P1K(2)     wapRP_02aP
286200070419     C                   Z-ADD     ap_P1K(3)     wapRP_03aP
286300070419     C                   Z-ADD     ap_P1K(4)     wapRP_04aP
286400070419     C                   Z-ADD     ap_P1K(5)     wapRP_05aP
286500070419     C                   Z-ADD     ap_P1K(6)     wapRP_06aP
286600070419     C                   Z-ADD     ap_P1K(7)     wapRP_07aP
286700070419     C                   Z-ADD     ap_P1K(8)     wapRP_08aP
286800070419     C                   Z-ADD     ap_P1K(9)     wapRP_09aP
286900070419     C                   Z-ADD     ap_P1K(10)    wapRP_10aP
287000070419     C                   Z-ADD     ap_P1K(11)    wapRP_11aP
287100070419     C                   Z-ADD     ap_P1K(12)    wapRP_12aP
287200070419      *
287300070419     C*--------------------
287400070419     C* Anno in Corso 12 Mesi
287500070419     C*--------------------
287600070419     C***    Spedizioni mensili
287700070419     C                   Z-ADD     S1K(1)        wapNS_01aC
287800070419     C                   Z-ADD     S1K(2)        wapNS_02aC
287900070419     C                   Z-ADD     S1K(3)        wapNS_03aC
288000070419     C                   Z-ADD     S1K(4)        wapNS_04aC
288100070419     C                   Z-ADD     S1K(5)        wapNS_05aC
288200070419     C                   Z-ADD     S1K(6)        wapNS_06aC
288300070419     C                   Z-ADD     S1K(7)        wapNS_07aC
288400070419     C                   Z-ADD     S1K(8)        wapNS_08aC
288500070419     C                   Z-ADD     S1K(9)        wapNS_09aC
288600070419     C                   Z-ADD     S1K(10)       wapNS_10aC
288700070419     C                   Z-ADD     S1K(11)       wapNS_11aC
288800070419     C                   Z-ADD     S1K(12)       wapNS_12aC
288900081013     C***    Pesi mensili
289000081016     c                   clear                   WaPkgC
289100081016     c                   add       Kg1K(1)       WaPkgC
289200081016     c                   add       Kg1K(2)       WaPkgC
289300081016     c                   add       Kg1K(3)       WaPkgC
289400081016     c                   add       Kg1K(4)       WaPkgC
289500081016     c                   add       Kg1K(5)       WaPkgC
289600081016     c                   add       Kg1K(6)       WaPkgC
289700081016     c                   add       Kg1K(7)       WaPkgC
289800081016     c                   add       Kg1K(8)       WaPkgC
289900081016     c                   add       Kg1K(9)       WaPkgC
290000081016     c                   add       Kg1K(10)      WaPkgC
290100081016     c                   add       Kg1K(11)      WaPkgC
290200081016     c                   add       Kg1K(12)      WaPkgC
290300081013     C                   Z-ADD     Kg1K(1)       wapKG_01aC
290400081013     C                   Z-ADD     Kg1K(2)       wapKG_02aC
290500081013     C                   Z-ADD     Kg1K(3)       wapKG_03aC
290600081013     C                   Z-ADD     Kg1K(4)       wapKG_04aC
290700081013     C                   Z-ADD     Kg1K(5)       wapKG_05aC
290800081013     C                   Z-ADD     Kg1K(6)       wapKG_06aC
290900081013     C                   Z-ADD     Kg1K(7)       wapKG_07aC
291000081013     C                   Z-ADD     Kg1K(8)       wapKG_08aC
291100081013     C                   Z-ADD     Kg1K(9)       wapKG_09aC
291200081013     C                   Z-ADD     Kg1K(10)      wapKG_10aC
291300081013     C                   Z-ADD     Kg1K(11)      wapKG_11aC
291400081013     C                   Z-ADD     Kg1K(12)      wapKG_12aC
291500070419     C***  Ricavi Reali mensili
291600070419     C                   Z-ADD     R1K(1)        wapRR_01aC
291700070419     C                   Z-ADD     R1K(2)        wapRR_02aC
291800070419     C                   Z-ADD     R1K(3)        wapRR_03aC
291900070419     C                   Z-ADD     R1K(4)        wapRR_04aC
292000070419     C                   Z-ADD     R1K(5)        wapRR_05aC
292100070419     C                   Z-ADD     R1K(6)        wapRR_06aC
292200070419     C                   Z-ADD     R1K(7)        wapRR_07aC
292300070419     C                   Z-ADD     R1K(8)        wapRR_08aC
292400070419     C                   Z-ADD     R1K(9)        wapRR_09aC
292500070419     C                   Z-ADD     R1K(10)       wapRR_10aC
292600070419     C                   Z-ADD     R1K(11)       wapRR_11aC
292700070419     C                   Z-ADD     R1K(12)       wapRR_12aC
292800070419     C***  Ricavi Presunti mensili
292900070419     C                   Z-ADD     P1K(1)        wapRP_01aC
293000070419     C                   Z-ADD     P1K(2)        wapRP_02aC
293100070419     C                   Z-ADD     P1K(3)        wapRP_03aC
293200070419     C                   Z-ADD     P1K(4)        wapRP_04aC
293300070419     C                   Z-ADD     P1K(5)        wapRP_05aC
293400070419     C                   Z-ADD     P1K(6)        wapRP_06aC
293500070419     C                   Z-ADD     P1K(7)        wapRP_07aC
293600070419     C                   Z-ADD     P1K(8)        wapRP_08aC
293700070419     C                   Z-ADD     P1K(9)        wapRP_09aC
293800070419     C                   Z-ADD     P1K(10)       wapRP_10aC
293900070419     C                   Z-ADD     P1K(11)       wapRP_11aC
294000070419     C                   Z-ADD     P1K(12)       wapRP_12aC
294100070419      *
294200070418      * ?Campi in più anagrafici
294300070419     C                   eval      WAPIND   =    DESvia                         *Indirizzo
294400070615     C                   eval      WAPcdf   =    DEScdf                         *cod.fisc
294500070615     C                   eval      WAPiva   =    DESiva                         *P.IVA
294600070618     C                   eval      WAPtel   =    DEStel                         *Telefono
294700070419      * ?CAP Italia
294800070419     c                   if        DESsta = *blank
294900070419     C                   movel(p)  DEScap        WAPCAP                         *CAP
295000070419      * ?CAP Estero
295100070419     c                   else
295200070419     C                   eval      WAPCAP   =    DEScae                         *CAP Europeo
295300070419     c                   end
295400070420     C                   eval      WAPLOC   =    DEScit                         *Città
295500070420     C                   eval      WAPPRV   =    DESprv                         *Provincia
295600070419     C                   eval      WAPNAZ   =    DESsta                         *Nazione
295700070420     C*
295800070420      * ?Resposnabile Trasporto
295900070420     C                   movel     'C'           NTCapl                         *CODICE APPLICAZIONE
296000070420     C                   movel     '0151'        NTCnk1                         *CHIAVE 1 NOTE
296100070420     C                   move      WaPCLI        NTCnk1                         *CHIAVE 1 NOTE
296200070420     C                   movel     *blank        NTCnk2                         *CHIAVE 2 NOTE
296300070420     C                   movel     '05'          NTCtnt                         *TIPO NOTA
296400070420     C     KeyNTC        chain     tfNTC01l
296500070420     c                   if        %Found(tfNTC01l)
296600070420     c                   movel     NTCRNT        WAPRESP
296700070420     c                   end
296800070426     C* ?E-mail Resposnabile Trasporto
296900070426     C                   movel     '06'          NTCtnt                         *TIPO NOTA
297000070426     C     KeyNTC        chain     tfNTC01l
297100070426     c                   if        %Found(tfNTC01l)
297200070426     c                   movel     NTCRNT        WAPEMAR
297300070426     c                   end
297400070426     C*
297500071212     C* ?Mese di Inizio Fatturato x Cliente Nuovo
297600071212     c                   clear                   WapNew_Mes
297700071212     c                   if        WaPCLN = 'N'
297800071212     c                   do        12            mes_k             3 0
297900071212     c                   if        s1k(mes_k) <> 0 or
298000081013     c                             kg1k(mes_k) <> 0 or
298100081013     c                             r1k(mes_k) <> 0 or
298200071212     c                             p1k(mes_k) <> 0
298300121217      *
298400121217      * Deve leggere l'ultimo a causa dei mesi antecedenti al vero e proprio inizio del cliente
298500121217      *  (questo è dovuto a storni o note di credito attribuite al cliente spalmate anche su
298600121217      *   mesi antecedenti il suo inizio di fatturazione).
298700121217     c                   if        cln1k(mes_k) = 'Y'
298800071212     c                   z-add     mes_k         WapNew_Mes
298900121217     c                   else
299000121217      *   se prima aveva impostato il campo lo fa uscire immediatamente
299100121217      *   ma solo se prima era passato per gli elementi con "Y"
299200121217     c                   if          WapNew_Mes > 0
299300121217     c                   leave
299400121217     c                   end
299500121217     c                   end
299600121217      *
299700071212     c                   endIF
299800071212     c                   endDO
299900071212     c                   end
300000071212     C*
300100081013      * ?Campi aggiunti: AGE Unificante e Descrizione
300200081013     C                   eval      WAPaud   =    depRGD                         *Descr.Unificante
300300081013     C                   eval      WAPauc   =    depRGF                         *Cod.  Unificante
300400081013     C*
300500070418     C                   WRITE     WADELPC0
300600091123     C*
300700070418     C                   ENDSR
300800000000     C*------------------------------------------------------------------------*
300900091123     C*  Scrive i record del WADEL x Estero in base al Network e la qualità
301000091123     C*------------------------------------------------------------------------*
301100091123     C     wriWADELntw   Begsr
301200091123     C*
301300091124     C* riporta I TOTALI x NETWORK
301400091124DO  1C     1             DO        5             K
301500091124     C*
301600100202     C     K             OCCUR     W1ntw
301700100201     C*
301800091124     C* Se non ci sono più Network esce dal ciclo
301900091124     c                   if        c1Ntw    = *blank  and
302000091124     c                             ap_c1Ntw = *blank
302100091124     c                   iter
302200091124     c                   end
302300091124     C*
302400091124      * ?Deve scrivere tanti records quanti sono i dati dei vari Network
302500091124     c                   clear                   WAdel00N
302600091124     C*
302700091124     c                   eval      WaNAAD   =    WaPAAD
302800091124     c                   eval      WaNMMD   =    WaPMMD
302900091124     c                   eval      WaNAAA   =    WaPAAA
303000091124     c                   eval      WaNMMA   =    WaPMMA
303100091124     c                   eval      WaNIOE   =    WaPIOE
303200091124     c                   eval      WaNDIV   =    WaPDIV
303300091124     c                   eval      WaNDID   =    WaPDID
303400091124     c                   eval      WaNARE   =    WaPARE
303500091124     c                   eval      WaNARD   =    WaPARD
303600091124     c                   eval      WaNFIL   =    WaPFIL
303700091124     c                   eval      WaNFID   =    WaPFID
303800091124     c                   eval      WaNCLV   =    WaPCLV
303900091124     c                   eval      WaNCLI   =    WaPCLI
304000091124     c                   eval      WaNCLD   =    WaPCLD
304100091124     c                   eval      WaNITC   =    WaPITC
304200091124     c                   eval      WaNCLN   =    WaPCLN
304300091124     c                   eval      WaNAGE   =    WaPAGE
304400091124     c                   eval      WaNAGD   =    WaPAGD
304500091124     c                   eval      WaNBLO   =    WaPBLO
304600120125     c                   eval      WAnCDULBLO =  WaPCDULBLO
304700120125     c                   eval      WAnDTULBLO =  WaPDTULBLO
304800120125     c                   eval      WAnDTPRIFA =  WaPDTPRIFA
304900091124      * ?Campi in più anagrafici
305000091124     C                   eval      WAnIND   =    WaPIND                         *Indirizzo
305100091124     C                   eval      WAncdf   =    WaPcdf                         *cod.fisc
305200091124     C                   eval      WAniva   =    WaPiva                         *P.IVA
305300091124     C                   eval      WAntel   =    WaPtel                         *Telefono
305400091124     C                   eval      WAnCAP   =    WaPCAP                         *CAP Europeo
305500091124     C                   eval      WAnLOC   =    WaPLOC                         *Città
305600091124     C                   eval      WAnPRV   =    WaPPRV                         *Provincia
305700091124     C                   eval      WAnNAZ   =    WaPNAZ                         *Nazione
305800091124     c                   eval      WAnRESP  =    WaPRESP
305900091124     c                   eval      WAnEMAR  =    WaPEMAR
306000091124     c                   eval      WanNew_Mes =  WaPNew_Mes
306100091124     C                   eval      WAnaud   =    WaPaud                         *Descr.Unificante
306200091124     C                   eval      WAnauc   =    WaPauc                         *Cod.  Unificante
306300091123      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
306400091124     c                   eval      WaNntw   =    c1NTW
306500091124     c                   if          WaNntw = *blank
306600091124     c                   eval      WaNntw   =    ap_c1NTW
306700091124     c                   end
306800091124     C***
306900091124     c                   xfoot     ap_S1ntw      WaNSAP
307000091124     c                   xfoot     S1ntw         WaNSAC
307100091124     C***
307200091125     c                   xfoot     ap_R1ntw      WaNFAP
307300091125     c                   xfoot     ap_P1ntw      WaNPAP
307400091125     c                   xfoot     R1ntw         WaNFAC
307500091125     c                   xfoot     P1ntw         WaNPAC
307600091124      ***
307700091124     C                   Z-ADD     WaNFAP        WRRW             19 5          *RICAVO REALE
307800091124     C                   Z-ADD     WaNPAP        WRPW             19 5          *RICAVO PRESUNTO
307900091124     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
308000091124     C                   EXSR      CALDEL
308100091124     C                   Z-ADD     WDPW          WaNDAP                         *DELTA
308200091124      ***
308300091124     C                   Z-ADD     WaNFAC        WRRW             19 5          *RICAVO REALE
308400091124     C                   Z-ADD     WaNPAC        WRPW             19 5          *RICAVO PRESUNTO
308500091124     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
308600091124     C                   EXSR      CALDEL
308700091124     C                   Z-ADD     WDPW          WaNDAC                         *DELTA
308800091123     C*--------------------
308900091123     C* Anno Precedente 12 Mesi
309000091123     C*--------------------
309100091123     C***    Spedizioni mensili
309200091124     C                   Z-ADD     ap_S1ntw(1)   wanNS_01aP
309300091124     C                   Z-ADD     ap_S1ntw(2)   wanNS_02aP
309400091124     C                   Z-ADD     ap_S1ntw(3)   wanNS_03aP
309500091124     C                   Z-ADD     ap_S1ntw(4)   wanNS_04aP
309600091124     C                   Z-ADD     ap_S1ntw(5)   wanNS_05aP
309700091124     C                   Z-ADD     ap_S1ntw(6)   wanNS_06aP
309800091124     C                   Z-ADD     ap_S1ntw(7)   wanNS_07aP
309900091124     C                   Z-ADD     ap_S1ntw(8)   wanNS_08aP
310000091124     C                   Z-ADD     ap_S1ntw(9)   wanNS_09aP
310100091124     C                   Z-ADD     ap_S1ntw(10)  wanNS_10aP
310200091124     C                   Z-ADD     ap_S1ntw(11)  wanNS_11aP
310300091124     C                   Z-ADD     ap_S1ntw(12)  wanNS_12aP
310400091123     C***    Pesi mensili
310500091124     c                   clear                   WankgP
310600091124     c                   add       ap_Kg1ntw(1)  WankgP
310700091124     c                   add       ap_Kg1ntw(2)  WankgP
310800091124     c                   add       ap_Kg1ntw(3)  WankgP
310900091124     c                   add       ap_Kg1ntw(4)  WankgP
311000091124     c                   add       ap_Kg1ntw(5)  WankgP
311100091124     c                   add       ap_Kg1ntw(6)  WankgP
311200091124     c                   add       ap_Kg1ntw(7)  WankgP
311300091124     c                   add       ap_Kg1ntw(8)  WankgP
311400091124     c                   add       ap_Kg1ntw(9)  WankgP
311500091124     c                   add       ap_Kg1ntw(10) WankgP
311600091124     c                   add       ap_Kg1ntw(11) WankgP
311700091124     c                   add       ap_Kg1ntw(12) WankgP
311800091124     C                   Z-ADD     ap_Kg1ntw(1)  wanKG_01aP
311900091124     C                   Z-ADD     ap_Kg1ntw(2)  wanKG_02aP
312000091124     C                   Z-ADD     ap_Kg1ntw(3)  wanKG_03aP
312100091124     C                   Z-ADD     ap_Kg1ntw(4)  wanKG_04aP
312200091124     C                   Z-ADD     ap_Kg1ntw(5)  wanKG_05aP
312300091124     C                   Z-ADD     ap_Kg1ntw(6)  wanKG_06aP
312400091124     C                   Z-ADD     ap_Kg1ntw(7)  wanKG_07aP
312500091124     C                   Z-ADD     ap_Kg1ntw(8)  wanKG_08aP
312600091124     C                   Z-ADD     ap_Kg1ntw(9)  wanKG_09aP
312700091124     C                   Z-ADD     ap_Kg1ntw(10) wanKG_10aP
312800091124     C                   Z-ADD     ap_Kg1ntw(11) wanKG_11aP
312900091124     C                   Z-ADD     ap_Kg1ntw(12) wanKG_12aP
313000091123     C***  Ricavi Reali mensili
313100091124     C                   Z-ADD     ap_R1ntw(1)   wanRR_01aP
313200091124     C                   Z-ADD     ap_R1ntw(2)   wanRR_02aP
313300091124     C                   Z-ADD     ap_R1ntw(3)   wanRR_03aP
313400091124     C                   Z-ADD     ap_R1ntw(4)   wanRR_04aP
313500091124     C                   Z-ADD     ap_R1ntw(5)   wanRR_05aP
313600091124     C                   Z-ADD     ap_R1ntw(6)   wanRR_06aP
313700091124     C                   Z-ADD     ap_R1ntw(7)   wanRR_07aP
313800091124     C                   Z-ADD     ap_R1ntw(8)   wanRR_08aP
313900091124     C                   Z-ADD     ap_R1ntw(9)   wanRR_09aP
314000091124     C                   Z-ADD     ap_R1ntw(10)  wanRR_10aP
314100091124     C                   Z-ADD     ap_R1ntw(11)  wanRR_11aP
314200091124     C                   Z-ADD     ap_R1ntw(12)  wanRR_12aP
314300091123     C***  Ricavi Presunti mensili
314400091124     C                   Z-ADD     ap_P1ntw(1)   wanRP_01aP
314500091124     C                   Z-ADD     ap_P1ntw(2)   wanRP_02aP
314600091124     C                   Z-ADD     ap_P1ntw(3)   wanRP_03aP
314700091124     C                   Z-ADD     ap_P1ntw(4)   wanRP_04aP
314800091124     C                   Z-ADD     ap_P1ntw(5)   wanRP_05aP
314900091124     C                   Z-ADD     ap_P1ntw(6)   wanRP_06aP
315000091124     C                   Z-ADD     ap_P1ntw(7)   wanRP_07aP
315100091124     C                   Z-ADD     ap_P1ntw(8)   wanRP_08aP
315200091124     C                   Z-ADD     ap_P1ntw(9)   wanRP_09aP
315300091124     C                   Z-ADD     ap_P1ntw(10)  wanRP_10aP
315400091124     C                   Z-ADD     ap_P1ntw(11)  wanRP_11aP
315500091124     C                   Z-ADD     ap_P1ntw(12)  wanRP_12aP
315600091123     C*--------------------
315700091123     C* Anno in Corso 12 Mesi
315800091123     C*--------------------
315900091123     C***    Spedizioni mensili
316000091124     C                   Z-ADD     S1ntw(1)      wanNS_01aC
316100091124     C                   Z-ADD     S1ntw(2)      wanNS_02aC
316200091124     C                   Z-ADD     S1ntw(3)      wanNS_03aC
316300091124     C                   Z-ADD     S1ntw(4)      wanNS_04aC
316400091124     C                   Z-ADD     S1ntw(5)      wanNS_05aC
316500091124     C                   Z-ADD     S1ntw(6)      wanNS_06aC
316600091124     C                   Z-ADD     S1ntw(7)      wanNS_07aC
316700091124     C                   Z-ADD     S1ntw(8)      wanNS_08aC
316800091124     C                   Z-ADD     S1ntw(9)      wanNS_09aC
316900091124     C                   Z-ADD     S1ntw(10)     wanNS_10aC
317000091124     C                   Z-ADD     S1ntw(11)     wanNS_11aC
317100091124     C                   Z-ADD     S1ntw(12)     wanNS_12aC
317200091123     C***    Pesi mensili
317300091124     c                   clear                   WankgC
317400091124     c                   add       Kg1ntw(1)     WankgC
317500091124     c                   add       Kg1ntw(2)     WankgC
317600091124     c                   add       Kg1ntw(3)     WankgC
317700091124     c                   add       Kg1ntw(4)     WankgC
317800091124     c                   add       Kg1ntw(5)     WankgC
317900091124     c                   add       Kg1ntw(6)     WankgC
318000091124     c                   add       Kg1ntw(7)     WankgC
318100091124     c                   add       Kg1ntw(8)     WankgC
318200091124     c                   add       Kg1ntw(9)     WankgC
318300091124     c                   add       Kg1ntw(10)    WankgC
318400091124     c                   add       Kg1ntw(11)    WankgC
318500091124     c                   add       Kg1ntw(12)    WankgC
318600091124     C                   Z-ADD     Kg1ntw(1)     wanKG_01aC
318700091124     C                   Z-ADD     Kg1ntw(2)     wanKG_02aC
318800091124     C                   Z-ADD     Kg1ntw(3)     wanKG_03aC
318900091124     C                   Z-ADD     Kg1ntw(4)     wanKG_04aC
319000091124     C                   Z-ADD     Kg1ntw(5)     wanKG_05aC
319100091124     C                   Z-ADD     Kg1ntw(6)     wanKG_06aC
319200091124     C                   Z-ADD     Kg1ntw(7)     wanKG_07aC
319300091124     C                   Z-ADD     Kg1ntw(8)     wanKG_08aC
319400091124     C                   Z-ADD     Kg1ntw(9)     wanKG_09aC
319500091124     C                   Z-ADD     Kg1ntw(10)    wanKG_10aC
319600091124     C                   Z-ADD     Kg1ntw(11)    wanKG_11aC
319700091124     C                   Z-ADD     Kg1ntw(12)    wanKG_12aC
319800091123     C***  Ricavi Reali mensili
319900091124     C                   Z-ADD     R1ntw(1)      wanRR_01aC
320000091124     C                   Z-ADD     R1ntw(2)      wanRR_02aC
320100091124     C                   Z-ADD     R1ntw(3)      wanRR_03aC
320200091124     C                   Z-ADD     R1ntw(4)      wanRR_04aC
320300091124     C                   Z-ADD     R1ntw(5)      wanRR_05aC
320400091124     C                   Z-ADD     R1ntw(6)      wanRR_06aC
320500091124     C                   Z-ADD     R1ntw(7)      wanRR_07aC
320600091124     C                   Z-ADD     R1ntw(8)      wanRR_08aC
320700091124     C                   Z-ADD     R1ntw(9)      wanRR_09aC
320800091124     C                   Z-ADD     R1ntw(10)     wanRR_10aC
320900091124     C                   Z-ADD     R1ntw(11)     wanRR_11aC
321000091124     C                   Z-ADD     R1ntw(12)     wanRR_12aC
321100091123     C***  Ricavi Presunti mensili
321200091124     C                   Z-ADD     P1ntw(1)      wanRP_01aC
321300091124     C                   Z-ADD     P1ntw(2)      wanRP_02aC
321400091124     C                   Z-ADD     P1ntw(3)      wanRP_03aC
321500091124     C                   Z-ADD     P1ntw(4)      wanRP_04aC
321600091124     C                   Z-ADD     P1ntw(5)      wanRP_05aC
321700091124     C                   Z-ADD     P1ntw(6)      wanRP_06aC
321800091124     C                   Z-ADD     P1ntw(7)      wanRP_07aC
321900091124     C                   Z-ADD     P1ntw(8)      wanRP_08aC
322000091124     C                   Z-ADD     P1ntw(9)      wanRP_09aC
322100091124     C                   Z-ADD     P1ntw(10)     wanRP_10aC
322200091124     C                   Z-ADD     P1ntw(11)     wanRP_11aC
322300091124     C                   Z-ADD     P1ntw(12)     wanRP_12aC
322400091124      *
322500091124     C                   WRITE     WADEL00n
322600091124     C                   ENDdo
322700091123     C*
322800091123     C                   ENDSR
322900100201      * ?----------------------------------------------------------------------
323000100201      *?  Scrive il nuovo WFile identico al WFDELPCF ma con informazioni in più   ?
323100100201      *?    NETWORK e TIPO SERVIZIO con anche DETTAGLIO VARIE "e" / "h"           ?
323200100201      * ?----------------------------------------------------------------------
323300100201     C     wri_WAdeA     BEGSR
323400100201      *
323500100201     c                   eval      numElem = %Elem(W1ntwTS)
323600100201     c**
323700100201      **  Quindi se da totalizzare cerca su quale occurence deve metterlo.
323800160509     c                   do        NUMELEM       kk
323900100201     C*
324000100202     C*  Attenzione alle schiere fra Anno in corso e Anno precedente
324100100202      * ?Deve scrivere tanti records quanti sono i dati dei vari Network/TServizi
324200160509     C     Kk            OCCUR     W1ntwTS
324300100201     C*
324400100204     C* Se non ci sono più Network/tipi servizio esce dal ciclo
324500100204     c                   if        c1NtwTS  = *blank  and c1tpsTS = *blank
324600100202     c                   leave
324700100201     c                   end
324800100201     C*
324900100201      * ?Deve scrivere tanti records quanti sono i dati dei vari Network
325000100201     c                   clear                   WAdeAPC0
325100100201     C*
325200100201     c                   eval      WadAAD   =    WaPAAD
325300100201     c                   eval      WadMMD   =    WaPMMD
325400100201     c                   eval      WadAAA   =    WaPAAA
325500100201     c                   eval      WadMMA   =    WaPMMA
325600100201     c                   eval      WadIOE   =    WaPIOE
325700100201     c                   eval      WadDIV   =    WaPDIV
325800100201     c                   eval      WadDID   =    WaPDID
325900100201     c                   eval      WadARE   =    WaPARE
326000100201     c                   eval      WadARD   =    WaPARD
326100100201     c                   eval      WadFIL   =    WaPFIL
326200100201     c                   eval      WadFID   =    WaPFID
326300100201     c                   eval      WadCLV   =    WaPCLV
326400100201     c                   eval      WadCLI   =    WaPCLI
326500100201     c                   eval      WadCLD   =    WaPCLD
326600100201     c                   eval      WadITC   =    WaPITC
326700100201     c                   eval      WadCLN   =    WaPCLN
326800100201     c                   eval      WadAGE   =    WaPAGE
326900100201     c                   eval      WadAGD   =    WaPAGD
327000100201     c                   eval      WadBLO   =    WaPBLO
327100120125     c                   eval      WAdCDULBLO =  WaPCDULBLO
327200120125     c                   eval      WAdDTULBLO =  WaPDTULBLO
327300120125     c                   eval      WAdDTPRIFA =  WaPDTPRIFA
327400100201      * ?Campi in più anagrafici
327500100201     C                   eval      WAdIND   =    WaPIND                         *Indirizzo
327600100201     C                   eval      WAdcdf   =    WaPcdf                         *cod.fisc
327700100201     C                   eval      WAdiva   =    WaPiva                         *P.IVA
327800100201     C                   eval      WAdtel   =    WaPtel                         *Telefono
327900100201     C                   eval      WAdCAP   =    WaPCAP                         *CAP Europeo
328000100201     C                   eval      WAdLOC   =    WaPLOC                         *Città
328100100201     C                   eval      WAdPRV   =    WaPPRV                         *Provincia
328200100201     C                   eval      WAdNAZ   =    WaPNAZ                         *Nazione
328300100201     c                   eval      WAdRESP  =    WaPRESP
328400100201     c                   eval      WAdEMAR  =    WaPEMAR
328500100201     c                   eval      WadNew_Mes =  WaPNew_Mes
328600100201     C                   eval      WAdaud   =    WaPaud                         *Descr.Unificante
328700100201     C                   eval      WAdauc   =    WaPauc                         *Cod.  Unificante
328800100201      * ?Campi in più con valori sui 12 mesi anno in corso e precedente
328900100215     c                   if        c1ntwTS  = 'ITA'
329000100215     c                   eval      Wadntw   =    *blank
329100100215     c                   else
329200100202     c                   eval      Wadntw   =    c1ntwTS
329300100215     c                   end
329400100215     c                   eval      WadTSP   =    c1tpsTS
329500100201     C***
329600100202     c                   xfoot     ap_S1ntwTS    WadSAP
329700100202     c                   xfoot     S1ntwTS       WadSAC
329800100201     C***
329900100202     c                   xfoot     ap_R1ntwTS    WadFAP
330000100202     c                   xfoot     ap_P1ntwTS    WadPAP
330100100202     c                   xfoot     R1ntwTS       WadFAC
330200100202     c                   xfoot     P1ntwTS       WadPAC
330300100201      ***
330400100201     C                   Z-ADD     WadFAP        WRRW             19 5          *RICAVO REALE
330500100201     C                   Z-ADD     WadPAP        WRPW             19 5          *RICAVO PRESUNTO
330600100201     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
330700100201     C                   EXSR      CALDEL
330800100201     C                   Z-ADD     WDPW          WaDDAP                         *DELTA
330900100201      ***
331000100201     C                   Z-ADD     WaDFAC        WRRW             19 5          *RICAVO REALE
331100100201     C                   Z-ADD     WaDPAC        WRPW             19 5          *RICAVO PRESUNTO
331200100201     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
331300100201     C                   EXSR      CALDEL
331400100201     C                   Z-ADD     WDPW          WaDDAC                         *DELTA
331500100201     C*--------------------
331600100201     C* Anno Precedente 12 Mesi
331700100201     C*--------------------
331800100201     C***    Spedizioni mensili
331900100202     C                   Z-ADD     ap_S1ntwTS(1) wadNS_01aP
332000100202     C                   Z-ADD     ap_S1ntwTS(2) wadNS_02aP
332100100202     C                   Z-ADD     ap_S1ntwTS(3) wadNS_03aP
332200100202     C                   Z-ADD     ap_S1ntwTS(4) wadNS_04aP
332300100202     C                   Z-ADD     ap_S1ntwTS(5) wadNS_05aP
332400100202     C                   Z-ADD     ap_S1ntwTS(6) wadNS_06aP
332500100202     C                   Z-ADD     ap_S1ntwTS(7) wadNS_07aP
332600100202     C                   Z-ADD     ap_S1ntwTS(8) wadNS_08aP
332700100202     C                   Z-ADD     ap_S1ntwTS(9) wadNS_09aP
332800100202     C                   Z-ADD     ap_S1ntwTS(10)wadNS_10aP
332900100202     C                   Z-ADD     ap_S1ntwTS(11)wadNS_11aP
333000100202     C                   Z-ADD     ap_S1ntwTS(12)wadNS_12aP
333100100201     C***    Pesi mensili
333200100201     c                   clear                   WadkgP
333300100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(1)
333400100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(2)
333500100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(3)
333600100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(4)
333700100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(5)
333800100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(6)
333900100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(7)
334000100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(8)
334100100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(9)
334200100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(10)
334300100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(11)
334400100202     c                   EVAL      WadkgP = WadkgP + ap_Kg1ntwTS(12)
334500100202     C                   Z-ADD     ap_Kg1ntwTS(1)wadKG_01aP
334600100202     C                   Z-ADD     ap_Kg1ntwTS(2)wadKG_02aP
334700100202     C                   Z-ADD     ap_Kg1ntwTS(3)wadKG_03aP
334800100202     C                   Z-ADD     ap_Kg1ntwTS(4)wadKG_04aP
334900100202     C                   Z-ADD     ap_Kg1ntwTS(5)wadKG_05aP
335000100202     C                   Z-ADD     ap_Kg1ntwTS(6)wadKG_06aP
335100100202     C                   Z-ADD     ap_Kg1ntwTS(7)wadKG_07aP
335200100202     C                   Z-ADD     ap_Kg1ntwTS(8)wadKG_08aP
335300100202     C                   Z-ADD     ap_Kg1ntwTS(9)wadKG_09aP
335400100202     c                   eval      wadKG_01aP = ap_Kg1ntwTS(1)
335500100202     c                   eval      wadKG_02aP = ap_Kg1ntwTS(2)
335600100202     c                   eval      wadKG_03aP = ap_Kg1ntwTS(3)
335700100202     c                   eval      wadKG_04aP = ap_Kg1ntwTS(4)
335800100202     c                   eval      wadKG_05aP = ap_Kg1ntwTS(5)
335900100202     c                   eval      wadKG_06aP = ap_Kg1ntwTS(6)
336000100202     c                   eval      wadKG_07aP = ap_Kg1ntwTS(7)
336100100202     c                   eval      wadKG_08aP = ap_Kg1ntwTS(8)
336200100202     c                   eval      wadKG_09aP = ap_Kg1ntwTS(9)
336300100202     c                   eval      wadKG_10aP = ap_Kg1ntwTS(10)
336400100202     c                   eval      wadKG_11aP = ap_Kg1ntwTS(11)
336500100202     c                   eval      wadKG_12aP = ap_Kg1ntwTS(12)
336600100201     C***  Ricavi Reali mensili
336700100202     C                   Z-ADD     ap_R1ntwTS(1) wadRR_01aP
336800100202     C                   Z-ADD     ap_R1ntwTS(2) wadRR_02aP
336900100202     C                   Z-ADD     ap_R1ntwTS(3) wadRR_03aP
337000100202     C                   Z-ADD     ap_R1ntwTS(4) wadRR_04aP
337100100202     C                   Z-ADD     ap_R1ntwTS(5) wadRR_05aP
337200100202     C                   Z-ADD     ap_R1ntwTS(6) wadRR_06aP
337300100202     C                   Z-ADD     ap_R1ntwTS(7) wadRR_07aP
337400100202     C                   Z-ADD     ap_R1ntwTS(8) wadRR_08aP
337500100202     C                   Z-ADD     ap_R1ntwTS(9) wadRR_09aP
337600100202     C                   Z-ADD     ap_R1ntwTS(10)wadRR_10aP
337700100202     C                   Z-ADD     ap_R1ntwTS(11)wadRR_11aP
337800100202     C                   Z-ADD     ap_R1ntwTS(12)wadRR_12aP
337900100201     C***  Ricavi Presunti mensili
338000100202     C                   Z-ADD     ap_P1ntwTS(1) wadRP_01aP
338100100202     C                   Z-ADD     ap_P1ntwTS(2) wadRP_02aP
338200100202     C                   Z-ADD     ap_P1ntwTS(3) wadRP_03aP
338300100202     C                   Z-ADD     ap_P1ntwTS(4) wadRP_04aP
338400100202     C                   Z-ADD     ap_P1ntwTS(5) wadRP_05aP
338500100202     C                   Z-ADD     ap_P1ntwTS(6) wadRP_06aP
338600100202     C                   Z-ADD     ap_P1ntwTS(7) wadRP_07aP
338700100202     C                   Z-ADD     ap_P1ntwTS(8) wadRP_08aP
338800100202     C                   Z-ADD     ap_P1ntwTS(9) wadRP_09aP
338900100202     C                   Z-ADD     ap_P1ntwTS(10)wadRP_10aP
339000100202     C                   Z-ADD     ap_P1ntwTS(11)wadRP_11aP
339100100202     C                   Z-ADD     ap_P1ntwTS(12)wadRP_12aP
339200100201     C*--------------------
339300100201     C* Anno in Corso 12 Mesi
339400100201     C*--------------------
339500100201     C***    Spedizioni mensili
339600100202     C                   Z-ADD     S1ntwTS(1)    wadNS_01aC
339700100202     C                   Z-ADD     S1ntwTS(2)    wadNS_02aC
339800100202     C                   Z-ADD     S1ntwTS(3)    wadNS_03aC
339900100202     C                   Z-ADD     S1ntwTS(4)    wadNS_04aC
340000100202     C                   Z-ADD     S1ntwTS(5)    wadNS_05aC
340100100202     C                   Z-ADD     S1ntwTS(6)    wadNS_06aC
340200100202     C                   Z-ADD     S1ntwTS(7)    wadNS_07aC
340300100202     C                   Z-ADD     S1ntwTS(8)    wadNS_08aC
340400100202     C                   Z-ADD     S1ntwTS(9)    wadNS_09aC
340500100202     C                   Z-ADD     S1ntwTS(10)   wadNS_10aC
340600100202     C                   Z-ADD     S1ntwTS(11)   wadNS_11aC
340700100202     C                   Z-ADD     S1ntwTS(12)   wadNS_12aC
340800100201     C***    Pesi mensili
340900100201     c                   clear                   WadkgC
341000100202     c                   add       Kg1ntwTS(1)   WadkgC
341100100202     c                   add       Kg1ntwTS(2)   WadkgC
341200100202     c                   add       Kg1ntwTS(3)   WadkgC
341300100202     c                   add       Kg1ntwTS(4)   WadkgC
341400100202     c                   add       Kg1ntwTS(5)   WadkgC
341500100202     c                   add       Kg1ntwTS(6)   WadkgC
341600100202     c                   add       Kg1ntwTS(7)   WadkgC
341700100202     c                   add       Kg1ntwTS(8)   WadkgC
341800100202     c                   add       Kg1ntwTS(9)   WadkgC
341900100202     c                   add       Kg1ntwTS(10)  WadkgC
342000100202     c                   add       Kg1ntwTS(11)  WadkgC
342100100202     c                   add       Kg1ntwTS(12)  WadkgC
342200100202     C                   Z-ADD     Kg1ntwTS(1)   wadKG_01aC
342300100202     C                   Z-ADD     Kg1ntwTS(2)   wadKG_02aC
342400100202     C                   Z-ADD     Kg1ntwTS(3)   wadKG_03aC
342500100202     C                   Z-ADD     Kg1ntwTS(4)   wadKG_04aC
342600100202     C                   Z-ADD     Kg1ntwTS(5)   wadKG_05aC
342700100202     C                   Z-ADD     Kg1ntwTS(6)   wadKG_06aC
342800100202     C                   Z-ADD     Kg1ntwTS(7)   wadKG_07aC
342900100202     C                   Z-ADD     Kg1ntwTS(8)   wadKG_08aC
343000100202     C                   Z-ADD     Kg1ntwTS(9)   wadKG_09aC
343100100202     C                   Z-ADD     Kg1ntwTS(10)  wadKG_10aC
343200100202     C                   Z-ADD     Kg1ntwTS(11)  wadKG_11aC
343300100202     C                   Z-ADD     Kg1ntwTS(12)  wadKG_12aC
343400100201     C***  Ricavi Reali mensili
343500100202     C                   Z-ADD     R1ntwTS(1)    wadRR_01aC
343600100202     C                   Z-ADD     R1ntwTS(2)    wadRR_02aC
343700100202     C                   Z-ADD     R1ntwTS(3)    wadRR_03aC
343800100202     C                   Z-ADD     R1ntwTS(4)    wadRR_04aC
343900100202     C                   Z-ADD     R1ntwTS(5)    wadRR_05aC
344000100202     C                   Z-ADD     R1ntwTS(6)    wadRR_06aC
344100100202     C                   Z-ADD     R1ntwTS(7)    wadRR_07aC
344200100202     C                   Z-ADD     R1ntwTS(8)    wadRR_08aC
344300100202     C                   Z-ADD     R1ntwTS(9)    wadRR_09aC
344400100202     C                   Z-ADD     R1ntwTS(10)   wadRR_10aC
344500100202     C                   Z-ADD     R1ntwTS(11)   wadRR_11aC
344600100202     C                   Z-ADD     R1ntwTS(12)   wadRR_12aC
344700100201     C***  Ricavi Presunti mensili
344800100202     C                   Z-ADD     P1ntwTS(1)    wadRP_01aC
344900100202     C                   Z-ADD     P1ntwTS(2)    wadRP_02aC
345000100202     C                   Z-ADD     P1ntwTS(3)    wadRP_03aC
345100100202     C                   Z-ADD     P1ntwTS(4)    wadRP_04aC
345200100202     C                   Z-ADD     P1ntwTS(5)    wadRP_05aC
345300100202     C                   Z-ADD     P1ntwTS(6)    wadRP_06aC
345400100202     C                   Z-ADD     P1ntwTS(7)    wadRP_07aC
345500100202     C                   Z-ADD     P1ntwTS(8)    wadRP_08aC
345600100202     C                   Z-ADD     P1ntwTS(9)    wadRP_09aC
345700100202     C                   Z-ADD     P1ntwTS(10)   wadRP_10aC
345800100202     C                   Z-ADD     P1ntwTS(11)   wadRP_11aC
345900100202     C                   Z-ADD     P1ntwTS(12)   wadRP_12aC
346000100204      *  i campi delle Varie verranno aggiunti in seguito solo sul file di Totale
346100100202      *
346200100202     C                   WRITE     WADEAPC0
346300100201     C                   ENDdo
346400100201     C*
346500100201     C                   ENDSR
346600091123     C*------------------------------------------------------------------------*
346700020221     C*?STAAGE - IMPOSTA STAMPA TOTALI AGENTE COMMERCIALE
346800000000     C*------------------------------------------------------------------------*
346900000000     C     STAAGE        BEGSR
347000011001     C*
347100011001     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI COMMERCIALI
347200011001 FG  C                   IF        PARSIN = 'S'
347300011001     C                   SETON                                        53
347400011105     C                   ELSE
347500011105     C                   SETOFF                                       53
347600011001     C                   ENDIF
347700000000     C*
347800000000     C* AZZERA I TOTALI NELLA FORMA CLI, SPE, RIC, DEL
347900000000     C                   CLEAR                   CLW                            *NUMERO CLIENTI
348000000000     C                   CLEAR                   SPW                            *SPEDIZIONI
348100000000     C                   CLEAR                   RRW                            *RICAVO REALE
348200000000     C                   CLEAR                   RPW                            *RICAVO PRESUNTO
348300000000     C                   CLEAR                   CTW                            *13° TOTALE CLEINTI
348400000000     C*
348500020220     C* CHIUDO IL PROSPETTO DEL DETTAGLIO CLIENTI
348600000000IF  1C     ORAKSC        IFEQ      '1'
348700100129     c                   if        parSTAMPA <> 'N'
348800020116     C                   WRITE     SE94R2
348900100129     c                   end
349000000000     C                   MOVEL     '0'           ORAKSC                         *ORA NON STAMPA CLI
349100000000E   1C                   ENDIF
349200000000     C*
349300000000     C* STAMPA I VALORI DELL'AGENTE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
349400020318     C* LA STATISTICA DELTA CON DETTAGLIO FILIALE O CON DETTAGLIO AREA
349500000000IF  1C     PASAGE        IFEQ      '1'
349600000000IF  2C     PARSTA        IFEQ      'K'
349700000000     C     PARSTA        OREQ      'C'
349800000000     C***
349900000000     C* DESCRIZIONI INTESTAZIONE
350000000000     C***
350100020116     C                   CLEAR                   SE94IN
350200000000     C                   MOVEL     DESAGE        DES25            25            *DESCRIZIONE
350300000000     C                   MOVEL     DE2PCF        DES04             4            *PERCENTUALE
350400000000IF  3C     DE2PCF        IFGT      *BLANKS
350500000000     C                   MOVE      '%'           DES04
350600000000X   3C                   ELSE
350700000000     C                   MOVE      ' '           DES04
350800000000E   3C                   ENDIF
350900000000IF  3C     PARSTA        IFEQ      'K'
351000000000     C                   Z-ADD     DEPAGE        INCAGE                         *AGENTE
351100000000     C                   MOVEL     DES25         INDAGE                         *DESCRIZ + PERCEN
351200000000     C                   MOVE      DES04         INDAGE
351300000000X   3C                   ELSE
351400000000     C                   Z-ADD     *ZEROS        INCAGE
351500000000     C                   MOVEL     DRI(1)        INDAGE
351600000000E   3C                   ENDIF
351700000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
351800000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
351900000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
352000000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
352100000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
352200000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
352300950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
352400030924     C*
352500030924     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
352600030924     C                   MOVEL     DRI(21)       PIDDES                         *RETTANGOLINO
352700030924     C*
352800030924     C* SE DELTA FILIALE STAMPA I TOTALI AGENTE NELLA FORMA: TOT A, TOT B, TOT C
352900000000IF  3C     PARSTA        IFEQ      'K'
353000020222     C*
353100160909     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA       è stato aggiunto un altro codice Qualità Cliente "E"
353200160909DO  4C**** 1             DO        4             Q
353300160914DO  4C     1             DO        Altri_ECBAT   Q
353400000000     C     Q             OCCUR     ABCCOM
353500000000     C     Q             OCCUR     ABCSTA
353600000000     C                   Z-ADD     CNC           CNS
353700000000     C                   Z-ADD     SNC           SNS
353800000000     C                   Z-ADD     RNC           RNS
353900000000     C                   Z-ADD     PNC           PNS
354000030924     C                   Z-ADD     CAC           CAS
354100030924     C                   Z-ADD     SAC           SAS
354200030924     C                   Z-ADD     RAC           RAS
354300030924     C                   Z-ADD     PAC           PAS
354400000000     C*
354500000000     C* NUMERO TOTALE CLIENTI
354600000000     C     Q             OCCUR     ABCC13
354700000000     C     Q             OCCUR     ABCS13
354800000000     C                   Z-ADD     NTC           NTS
354900000000     C                   Z-ADD     ATC           ATS
355000000000     C*
355100000000E   4C                   ENDDO
355200020220     C*
355300020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA X CLIENTI 'D'
355400160913     C*     i direzionali SEMPRE sull'ultimo Elemento
355500160909     C**** 5             OCCUR     ABCCOM
355600160909     C**** 5             OCCUR     ABCSTA
355700160914     C     Direzionali   OCCUR     ABCCOM
355800160914     C     Direzionali   OCCUR     ABCSTA
355900020220     C                   Z-ADD     CNC           CNS
356000020220     C                   Z-ADD     SNC           SNS
356100020220     C                   Z-ADD     RNC           RNS
356200020220     C                   Z-ADD     PNC           PNS
356300020220     C                   Z-ADD     CAC           CAS
356400020220     C                   Z-ADD     SAC           SAS
356500020220     C                   Z-ADD     RAC           RAS
356600020220     C                   Z-ADD     PAC           PAS
356700020220     C*
356800020220     C* NUMERO TOTALE CLIENTI 'D'
356900160909     C**** 5             OCCUR     ABCC13
357000160909     C**** 5             OCCUR     ABCS13
357100160914     C     Direzionali   OCCUR     ABCC13
357200160914     C     Direzionali   OCCUR     ABCS13
357300020220     C                   Z-ADD     NTC           NTS
357400020220     C                   Z-ADD     ATC           ATS
357500020318     C*
357600020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
357700020318DO  4C     1             DO        5             K
357800020318     C     K             OCCUR     ABCCOMN
357900020318     C     K             OCCUR     ABCSTAN
358000020318     C                   Z-ADD     CNCN          CNSN
358100020318     C                   Z-ADD     SNCN          SNSN
358200020318     C                   Z-ADD     RNCN          RNSN
358300020318     C                   Z-ADD     PNCN          PNSN
358400020318     C                   Z-ADD     CACN          CASN
358500020318     C                   Z-ADD     SACN          SASN
358600020318     C                   Z-ADD     RACN          RASN
358700020318     C                   Z-ADD     PACN          PASN
358800020318     C*
358900020318     C* NUMERO TOTALE CLIENTI
359000020318     C     K             OCCUR     ABCC13N
359100020318     C     K             OCCUR     ABCS13N
359200020318     C                   Z-ADD     NTCN          NTSN
359300020318     C                   Z-ADD     ATCN          ATSN
359400020318     C*
359500020318E   4C                   ENDDO
359600020318     C*
359700020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
359800020318DO  4C     1             DO        5             K
359900020318     C     K             OCCUR     ABCCOMND
360000020318     C     K             OCCUR     ABCSTAND
360100020318     C                   Z-ADD     CNCND         CNSND
360200020318     C                   Z-ADD     SNCND         SNSND
360300020318     C                   Z-ADD     RNCND         RNSND
360400020318     C                   Z-ADD     PNCND         PNSND
360500020318     C                   Z-ADD     CACND         CASND
360600020318     C                   Z-ADD     SACND         SASND
360700020318     C                   Z-ADD     RACND         RASND
360800020318     C                   Z-ADD     PACND         PASND
360900020318     C*
361000020318     C* NUMERO TOTALE CLIENTI
361100020318     C     K             OCCUR     ABCC13ND
361200020318     C     K             OCCUR     ABCS13ND
361300020318     C                   Z-ADD     NTCND         NTSND
361400020318     C                   Z-ADD     ATCND         ATSND
361500020318     C*
361600020318E   4C                   ENDDO
361700020220     C*
361800020220     C* ROUTINE DI STAMPA TOTALI
361900020220     C                   EXSR      STAMPA
362000000000     C*
362100000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
362200000000     C                   SETON                                        01
362300000000     C                   MOVEL     '0'           PRIGIR
362400000000     C*
362500000000     C* SE DELTA AREA STAMPA I TOTALI AGENTE NELLA FORMA: CLI, SPE, RIC, DEL
362600000000X   3C                   ELSE
362700020222     C*
362800020318     C* IMPOSTO IL FLAG CHE MI IDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
362900020318     C                   EVAL      DEPSTPD = '1'
363000020318     C*
363100160909     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (aggiunto "E" qualità cliente)
363200160913     C*    Escluso l'Ultimo che ha i DIREZIONALI  gli altri
363300160909DO  4C*****1             DO        4             Q
363400160915DO  4C     1             DO        Altri_ECBAT   Q
363500020318     C     Q             OCCUR     ABCCOM
363600020318     C                   ADD       CNC           CLW                            *NUOVI A, B, C +
363700020318     C                   ADD       SNC           SPW
363800020318     C                   ADD       RNC           RRW
363900020318     C                   ADD       PNC           RPW
364000020318     C                   ADD       CAC           CLW                            *ACQUISITI A, B , C
364100020318     C                   ADD       SAC           SPW
364200020318     C                   ADD       RAC           RRW
364300020318     C                   ADD       PAC           RPW
364400000000     C*
364500020318     C* NUMERO TOTALE CLIENTI
364600020318     C     Q             OCCUR     ABCC13
364700020318     C                   ADD       NTC           CTW
364800020318     C                   ADD       ATC           CTW
364900020318     C
365000020318E   4C                   ENDDO
365100020221     C*
365200000000     C                   MOVEL     DEPAGE        DES07             7            *AGENTE
365300000000     C                   MOVEL     DESAGE        DES12            12            *DESCRIZIONE
365400000000     C*
365500000000     C                   MOVEL     *BLANKS       DES20            20
365600000000     C                   MOVEL     DES07         DES20            20
365700000000     C                   MOVE      DES12         DES20
365800010710     C*
365900010710     C* ROUTINE DI STAMPA TOTALI
366000030926     c                   MOVEL     'A'           WTipStamp2                     *da STAAGE
366100020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
366200020222     C*
366300020220     C* STAMPO I 'D' SOLO SE EFFETTIVAMENTE ESISTENTI
366400020220     C                   IF        FLGSIDE = '1'
366500020221     C*
366600020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
366700020222     C                   EVAL      DEPSTPD = '2'
366800020220     C*
366900020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
367000160913     C*  x agganciare i DIREZIONALI
367100160909     C**** 5             OCCUR     ABCCOM
367200160914     C     Direzionali   OCCUR     ABCCOM
367300050217     C****
367400050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
367500050217     C****  i direzionali ma riportava la somma di tutti
367600050217     C     CNC           ADD       CAC           CLW                            *NUOVI+ACQUISITI (D)
367700050217     C     SNC           ADD       SAC           SPW
367800050217     C     RNC           ADD       RAC           RRW
367900050217     C     PNC           ADD       PAC           RPW
368000050217     C*
368100020220     C* NUMERO TOTALE CLIENTI CLIENTI 'D'
368200160913     C*  x agganciare i DIREZIONALI
368300160909     C**** 5             OCCUR     ABCC13
368400160914     C     Direzionali   OCCUR     ABCC13
368500050217     C****
368600050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
368700050217     C****  i direzionali ma riportava la somma di tutti
368800050217     C     NTC           ADD       ATC           CTW
368900020220     C*
369000020220     C* ROUTINE DI STAMPA TOTALI
369100030926     c                   MOVEL     'A'           WTipStamp2                     *da STAAGE
369200020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
369300020318     C                   ENDIF
369400000000     C*
369500000000     C* STAMPO RIGA DI SEPARAZIONE
369600100129     c                   if        parSTAMPA <> 'N'
369700020116     C  N53              WRITE     SE94R2
369800100129     c                   end
369900011001E   3C                   ENDIF
370000000000E   2C                   ENDIF
370100000000E   1C                   ENDIF
370200011001     C*
370300000000     C                   ENDSR
370400030924     C*------------------------------------------------------------------------*
370500030924     C*?STAFIL - IMPOSTA STAMPA TOTALI FILIALE
370600030924     C*------------------------------------------------------------------------*
370700030924     C     STAFIL        BEGSR
370800030924     C*
370900030924     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI FILIALE
371000030924     C                   IF        PARSIN = 'S'
371100030924     C                   SETON                                        53
371200030924     C                   ELSE
371300030924     C                   SETOFF                                       53
371400030924     C                   ENDIF
371500030924     C*
371600000000     C* AZZERA I TOTALI NELLA FORMA CLI, SPE, RIC, DEL
371700000000     C                   CLEAR                   CLW                            *NUMERO CLIENTI
371800000000     C                   CLEAR                   SPW                            *SPEDIZIONI
371900000000     C                   CLEAR                   RRW                            *RICAVO REALE
372000000000     C                   CLEAR                   RPW                            *RICAVO PRESUNTO
372100000000     C                   CLEAR                   CTW                            *13° TOTALE CLEINTI
372200000000     C*
372300000000     C* STAMPA VALORI DELLA FILIALE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
372400000000     C* LA STATISTICA DELTA CON DETTAGLIO CLIENTE/COMMERCIALE/FILIALE (SEMPRE)
372500000000IF  1C     PASFIL        IFEQ      '1'
372600000000IF  2C     PARSTA        IFEQ      'K'
372700000000     C     PARSTA        OREQ      'C'
372800000000     C     PARSTA        OREQ      'F'
372900000000     C***
373000000000     C* DESCRIZIONI INTESTAZIONE
373100000000     C***
373200020116     C                   CLEAR                   SE94IN
373300000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
373400000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
373500000000IF  3C     PARSTA        IFEQ      'K'
373600020221     C     PARSTA        OREQ      'C'
373700000000     C                   Z-ADD     DEPFIL        INCFIL                         *FILIALE
373800000000     C                   MOVEL     DESFIL        INDFIL                         *DESCRIZIONE FILIALE
373900000000X   3C                   ELSE
374000000000     C                   Z-ADD     *ZEROS        INCFIL
374100000000     C                   MOVEL     DRI(2)        INDFIL
374200000000E   3C                   ENDIF
374300000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
374400000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
374500000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
374600000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
374700030924     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
374800030924     C*
374900030924     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
375000030924     C                   MOVEL     DRI(22)       PIDDES                         *RETTANGOLINO
375100030924     C*
375200030924     C* SE DELTA FILIALE O DELTA AREA STAMPA I TOTALI FILIALE NELLA FORMA:
375300030924     C* TOT A, TOT B, TOT C
375400000000IF  3C     PARSTA        IFEQ      'K'
375500000000     C     PARSTA        OREQ      'C'
375600000000     C*
375700000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
375800160913     **    escluso i Direzionali "D"
375900160909DO  4C**** 1             DO        4             Q
376000160914DO  4C     1             DO        Altri_ECBAT   Q
376100000000     C     Q             OCCUR     ABCFIL
376200000000     C     Q             OCCUR     ABCSTA
376300000000     C                   Z-ADD     CNF           CNS
376400000000     C                   Z-ADD     SNF           SNS
376500000000     C                   Z-ADD     RNF           RNS
376600000000     C                   Z-ADD     PNF           PNS
376700000000     C                   Z-ADD     CAF           CAS
376800000000     C                   Z-ADD     SAF           SAS
376900000000     C                   Z-ADD     RAF           RAS
377000000000     C                   Z-ADD     PAF           PAS
377100000000     C*
377200000000     C* NUMERO TOTALE CLIENTI
377300000000     C     Q             OCCUR     ABCF13
377400000000     C     Q             OCCUR     ABCS13
377500000000     C                   Z-ADD     NTF           NTS
377600000000     C                   Z-ADD     ATF           ATS
377700000000     C*
377800000000E   4C                   ENDDO
377900020220     C*
378000020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
378100160909     C**** 5             OCCUR     ABCFIL
378200160909     C**** 5             OCCUR     ABCSTA
378300160914     C     Direzionali   OCCUR     ABCFIL
378400160914     C     Direzionali   OCCUR     ABCSTA
378500020220     C                   Z-ADD     CNF           CNS
378600020220     C                   Z-ADD     SNF           SNS
378700020220     C                   Z-ADD     RNF           RNS
378800020220     C                   Z-ADD     PNF           PNS
378900020220     C                   Z-ADD     CAF           CAS
379000020220     C                   Z-ADD     SAF           SAS
379100020220     C                   Z-ADD     RAF           RAS
379200020220     C                   Z-ADD     PAF           PAS
379300020220     C*
379400020220     C* NUMERO TOTALE CLIENTI 'D'
379500160909     C**** 5             OCCUR     ABCF13
379600160909     C**** 5             OCCUR     ABCS13
379700160914     C     Direzionali   OCCUR     ABCF13
379800160914     C     Direzionali   OCCUR     ABCS13
379900020220     C                   Z-ADD     NTF           NTS
380000020220     C                   Z-ADD     ATF           ATS
380100020318     C*
380200020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
380300020318DO  4C     1             DO        5             K
380400020318     C     K             OCCUR     ABCFILN
380500020318     C     K             OCCUR     ABCSTAN
380600020318     C                   Z-ADD     CNFN          CNSN
380700020318     C                   Z-ADD     SNFN          SNSN
380800020318     C                   Z-ADD     RNFN          RNSN
380900020318     C                   Z-ADD     PNFN          PNSN
381000020318     C                   Z-ADD     CAFN          CASN
381100020318     C                   Z-ADD     SAFN          SASN
381200020318     C                   Z-ADD     RAFN          RASN
381300020318     C                   Z-ADD     PAFN          PASN
381400020318     C*
381500020318     C* NUMERO TOTALE CLIENTI
381600020318     C     K             OCCUR     ABCF13N
381700020318     C     K             OCCUR     ABCS13N
381800020318     C                   Z-ADD     NTFN          NTSN
381900020318     C                   Z-ADD     ATFN          ATSN
382000020318     C*
382100020318E   4C                   ENDDO
382200020318     C*
382300020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
382400020318DO  4C     1             DO        5             K
382500020318     C     K             OCCUR     ABCFILND
382600020318     C     K             OCCUR     ABCSTAND
382700020318     C                   Z-ADD     CNFND         CNSND
382800020318     C                   Z-ADD     SNFND         SNSND
382900020318     C                   Z-ADD     RNFND         RNSND
383000020318     C                   Z-ADD     PNFND         PNSND
383100020318     C                   Z-ADD     CAFND         CASND
383200020318     C                   Z-ADD     SAFND         SASND
383300020318     C                   Z-ADD     RAFND         RASND
383400020318     C                   Z-ADD     PAFND         PASND
383500020318     C*
383600020318     C* NUMERO TOTALE CLIENTI
383700020318     C     K             OCCUR     ABCF13ND
383800020318     C     K             OCCUR     ABCS13ND
383900020318     C                   Z-ADD     NTFND         NTSND
384000020318     C                   Z-ADD     ATFND         ATSND
384100020318     C*
384200020318E   4C                   ENDDO
384300020220     C*
384400020220     C* ROUTINE DI STAMPA TOTALI
384500020220     C                   EXSR      STAMPA
384600000000     C*
384700000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
384800000000     C                   SETON                                        01
384900000000     C                   MOVEL     '0'           PRIGIR
385000000000     C*
385100000000     C* SE DELTA DIVISIONE STAMPA I TOTALI FILIALE NELLA FORMA: CLI,SPE,RIC,DEL
385200000000X   3C                   ELSE
385300020221     C*
385400020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
385500020222     C                   EVAL      DEPSTPD = '1'
385600000000     C*
385700000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
385800160913      **  Esclusi i Direzionali "D"
385900160909DO  4C**** 1             DO        4             Q
386000160914DO  4C     1             DO        Altri_ECBAT   Q
386100020318     C     Q             OCCUR     ABCFIL
386200050217     C                   ADD       CNF           CLW                            *NUOVI A, B, C +
386300050217     C                   ADD       SNF           SPW
386400050217     C                   ADD       RNF           RRW
386500050217     C                   ADD       PNF           RPW
386600050217     C                   ADD       CAF           CLW                            *ACQUISITI A, B , C
386700050217     C                   ADD       SAF           SPW
386800050217     C                   ADD       RAF           RRW
386900050217     C                   ADD       PAF           RPW
387000050217     C*
387100000000     C* NUMERO TOTALE CLIENTI
387200020318     C     Q             OCCUR     ABCF13
387300050217     C                   ADD       NTF           CTW
387400050217     C                   ADD       ATF           CTW
387500050217     C*
387600020318E   4C                   ENDDO
387700020221     C*
387800000000     C                   MOVEL     DEPFIL        DES03             3            *FILIALE
387900000000     C                   MOVEL     DESFIL        DES16            16            *DESCRIZIONE
388000000000     C*
388100000000     C                   MOVEL     *BLANKS       DES20            20
388200000000     C                   MOVEL     DES03         DES20            20
388300000000     C                   MOVE      DES16         DES20
388400010710     C*
388500010710     C* ROUTINE DI STAMPA TOTALI
388600030926     c                   MOVEL     'F'           WTipStamp2                     *da STAAGE
388700020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
388800020220     C*
388900020220     C* STAMPO I 'D' SOLO SE EFFETTIVAMENTE ESISTENTI
389000020220     C                   IF        FLGSIDF = '1'
389100020221     C*
389200020221     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
389300020222     C                   EVAL      DEPSTPD = '2'
389400020220     C*
389500020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
389600160909     C**** 5             OCCUR     ABCFIL
389700160914     C     Direzionali   OCCUR     ABCFIL
389800050217     C****
389900050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
390000050217     C****  i direzionali ma riportava la somma di tutti
390100050217     C     CNF           ADD       CAF           CLW                            *NUOVI+ACQUISITI (D)
390200050217     C     SNF           ADD       SAF           SPW
390300050217     C     RNF           ADD       RAF           RRW
390400050217     C     PNF           ADD       PAF           RPW
390500050217     C*
390600020220     C* NUMERO TOTALE CLIENTI 'D'
390700160909     C*****5             OCCUR     ABCF13
390800160914     C     Direzionali   OCCUR     ABCF13
390900050217     C****
391000050217     C****  Prima erroneamente andava in somma e quindi non mostrava solo
391100050217     C****  i direzionali ma riportava la somma di tutti
391200050217     C     NTF           ADD       ATF           CTW
391300050217     C*
391400020220     C* ROUTINE DI STAMPA TOTALI
391500030926     c                   MOVEL     'F'           WTipStamp2                     *da STAAGE
391600020318     C                   EXSR      STAMP2                                       *NON SALTO PAGINA
391700020318     C                   ENDIF
391800000000     C*
391900000000     C* STAMPO RIGA DI SEPARAZIONE
392000100129     c                   if        parSTAMPA <> 'N'
392100020116     C  N53              WRITE     SE94R2
392200100129     c                   end
392300000000E   3C                   ENDIF
392400000000E   2C                   ENDIF
392500000000E   1C                   ENDIF
392600010710     C*
392700000000     C                   ENDSR
392800000000     C*------------------------------------------------------------------------*
392900000000     C*?STAARE - IMPOSTA STAMPA TOTALI AREA
393000000000     C*------------------------------------------------------------------------*
393100000000     C     STAARE        BEGSR
393200020215     C*
393300020215     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI AREA
393400020215 FG  C                   IF        PARSIN = 'S'
393500020215     C                   SETON                                        53
393600020215     C                   ELSE
393700020215     C                   SETOFF                                       53
393800020215     C                   ENDIF
393900000000     C*
394000000000     C* STAMPA I VALORI DELL'AREA SE HO MEMORIZZATO QUALCOSA E SE STO ELABORANDO
394100020318     C* LA STATISTICA DELTA CON DETTAGLIO AREA O CON DETTAGLIO DIVISIONE
394200000000IF  1C     PASARE        IFEQ      '1'
394300000000IF  2C     PARSTA        IFEQ      'C'
394400000000     C     PARSTA        OREQ      'F'
394500000000     C***
394600000000     C* DESCRIZIONI INTESTAZIONE
394700000000     C***
394800020116     C                   CLEAR                   SE94IN
394900000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
395000000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
395100000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
395200000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
395300000000     C                   Z-ADD     DEPARE        INCARE                         *AREA
395400000000     C                   MOVEL     DESARE        INDARE                         *DESCRIZIONE AREA
395500000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
395600000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
395700950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
395800000000     C*
395900000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
396000000000     C                   MOVEL     DRI(23)       PIDDES                         *RETTANGOLINO
396100000000     C*
396200000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
396300160913      **  esclusi i Direzionali "D"
396400160909DO  3C*****1             DO        4             Q
396500160914DO  3C     1             DO        Altri_ECBAT   Q
396600000000     C     Q             OCCUR     ABCARE
396700000000     C     Q             OCCUR     ABCSTA
396800000000     C                   Z-ADD     CNA           CNS
396900000000     C                   Z-ADD     SNA           SNS
397000000000     C                   Z-ADD     RNA           RNS
397100000000     C                   Z-ADD     PNA           PNS
397200000000     C                   Z-ADD     CAA           CAS
397300000000     C                   Z-ADD     SAA           SAS
397400000000     C                   Z-ADD     RAA           RAS
397500000000     C                   Z-ADD     PAA           PAS
397600000000     C*
397700000000     C* NUMERO TOTALE CLIENTI
397800000000     C     Q             OCCUR     ABCA13
397900000000     C     Q             OCCUR     ABCS13
398000000000     C                   Z-ADD     NTA           NTS
398100000000     C                   Z-ADD     ATA           ATS
398200000000     C*
398300000000E   3C                   ENDDO
398400020220     C*
398500020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
398600160913     **    solo i Direzionali "D"
398700160909     C**** 5             OCCUR     ABCARE
398800160909     C**** 5             OCCUR     ABCSTA
398900160914     C     Direzionali   OCCUR     ABCARE
399000160914     C     Direzionali   OCCUR     ABCSTA
399100020220     C                   Z-ADD     CNA           CNS
399200020220     C                   Z-ADD     SNA           SNS
399300020220     C                   Z-ADD     RNA           RNS
399400020220     C                   Z-ADD     PNA           PNS
399500020220     C                   Z-ADD     CAA           CAS
399600020220     C                   Z-ADD     SAA           SAS
399700020220     C                   Z-ADD     RAA           RAS
399800020220     C                   Z-ADD     PAA           PAS
399900020220     C*
400000020220     C* NUMERO TOTALE CLIENTI 'D'
400100160913     C*  solo i Direzionali "D"
400200160909     C**** 5             OCCUR     ABCA13
400300160909     C**** 5             OCCUR     ABCS13
400400160914     C     Direzionali   OCCUR     ABCA13
400500160914     C     Direzionali   OCCUR     ABCS13
400600020220     C                   Z-ADD     NTA           NTS
400700020220     C                   Z-ADD     ATA           ATS
400800020318     C*
400900020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
401000020318DO  3C     1             DO        5             K
401100020318     C     K             OCCUR     ABCAREN
401200020318     C     K             OCCUR     ABCSTAN
401300020318     C                   Z-ADD     CNAN          CNSN
401400020318     C                   Z-ADD     SNAN          SNSN
401500020318     C                   Z-ADD     RNAN          RNSN
401600020318     C                   Z-ADD     PNAN          PNSN
401700020318     C                   Z-ADD     CAAN          CASN
401800020318     C                   Z-ADD     SAAN          SASN
401900020318     C                   Z-ADD     RAAN          RASN
402000020318     C                   Z-ADD     PAAN          PASN
402100020318     C*
402200020318     C* NUMERO TOTALE CLIENTI
402300020318     C     K             OCCUR     ABCA13N
402400020318     C     K             OCCUR     ABCS13N
402500020318     C                   Z-ADD     NTAN          NTSN
402600020318     C                   Z-ADD     ATAN          ATSN
402700020318     C*
402800020318E   3C                   ENDDO
402900020318     C*
403000020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
403100020318DO  3C     1             DO        5             K
403200020318     C     K             OCCUR     ABCAREND
403300020318     C     K             OCCUR     ABCSTAND
403400020318     C                   Z-ADD     CNAND         CNSND
403500020318     C                   Z-ADD     SNAND         SNSND
403600020318     C                   Z-ADD     RNAND         RNSND
403700020318     C                   Z-ADD     PNAND         PNSND
403800020318     C                   Z-ADD     CAAND         CASND
403900020318     C                   Z-ADD     SAAND         SASND
404000020318     C                   Z-ADD     RAAND         RASND
404100020318     C                   Z-ADD     PAAND         PASND
404200020318     C*
404300020318     C* NUMERO TOTALE CLIENTI
404400020318     C     K             OCCUR     ABCA13ND
404500020318     C     K             OCCUR     ABCS13ND
404600020318     C                   Z-ADD     NTAND         NTSND
404700020318     C                   Z-ADD     ATAND         ATSND
404800020318     C*
404900020318E   3C                   ENDDO
405000020220     C*
405100020220     C* ROUTINE DI STAMPA TOTALI
405200020220     C                   EXSR      STAMPA
405300000000     C*
405400000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
405500000000     C                   SETON                                        01
405600000000     C                   MOVEL     '0'           PRIGIR
405700000000     C*
405800000000E   2C                   ENDIF
405900000000E   1C                   ENDIF
406000000000     C*
406100000000     C                   ENDSR
406200000000     C*------------------------------------------------------------------------*
406300000000     C*?STADIV - IMPOSTA STAMPA TOTALI DIVISIONE
406400000000     C*------------------------------------------------------------------------*
406500000000     C     STADIV        BEGSR
406600020215     C*
406700020215     C* SE NON RICHIESTA STAMPA SINTETICA => ESEGUO STAMPA TOTALI DIVISIONE
406800020215 FG  C                   IF        PARSIN = 'S'
406900020215     C                   SETON                                        53
407000020215     C                   ELSE
407100020215     C                   SETOFF                                       53
407200020215     C                   ENDIF
407300000000     C*
407400020318     C* STAMPA I VALORI DELLA DIVISIONE SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
407500020318     C* LA STATISTICA DELTA CON DETTAGLIO DIVISIONE
407600000000IF  1C     PASDIV        IFEQ      '1'
407700000000     C     PARSTA        ANDEQ     'F'
407800000000     C***
407900000000     C* DESCRIZIONI INTESTAZIONE
408000000000     C***
408100020116     C                   CLEAR                   SE94IN
408200000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
408300000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
408400000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
408500000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
408600000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
408700000000     C                   MOVEL     DRI(2)        INDARE                         *DESCRIZIONE AREA
408800000000     C                   MOVEL     DEPDIV        INCDIV                         *DIVISIONE
408900000000     C                   MOVEL     DESDIV        INDDIV                         DESCRIZIONE DIVISION
409000950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
409100000000     C*
409200000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
409300000000     C                   MOVEL     DRI(24)       PIDDES
409400000000     C*
409500000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
409600160913      ***  esclusi i Direzionali
409700160909DO  2C**** 1             DO        4             Q
409800160914DO  2C     1             DO        Altri_ECBAT   Q
409900000000     C     Q             OCCUR     ABCDIV
410000000000     C     Q             OCCUR     ABCSTA
410100000000     C                   Z-ADD     CND           CNS
410200000000     C                   Z-ADD     SND           SNS
410300000000     C                   Z-ADD     RND           RNS
410400000000     C                   Z-ADD     PND           PNS
410500000000     C                   Z-ADD     CAD           CAS
410600000000     C                   Z-ADD     SAD           SAS
410700000000     C                   Z-ADD     RAD           RAS
410800000000     C                   Z-ADD     PAD           PAS
410900000000     C*
411000000000     C* NUMERO TOTALE CLIENTI
411100000000     C     Q             OCCUR     ABCD13
411200000000     C     Q             OCCUR     ABCS13
411300000000     C                   Z-ADD     NTD           NTS
411400000000     C                   Z-ADD     ATD           ATS
411500000000     C*
411600000000E   2C                   ENDDO
411700020220     C*
411800020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
411900160913      **   solo i Direzionali "D"
412000160909     C**** 5             OCCUR     ABCDIV
412100160909     C**** 5             OCCUR     ABCSTA
412200160914     C     Direzionali   OCCUR     ABCDIV
412300160914     C     Direzionali   OCCUR     ABCSTA
412400020220     C                   Z-ADD     CND           CNS
412500020220     C                   Z-ADD     SND           SNS
412600030924     C                   Z-ADD     RND           RNS
412700030924     C                   Z-ADD     PND           PNS
412800030924     C                   Z-ADD     CAD           CAS
412900030924     C                   Z-ADD     SAD           SAS
413000020220     C                   Z-ADD     RAD           RAS
413100030924     C                   Z-ADD     PAD           PAS
413200030924     C*
413300030924     C* NUMERO TOTALE CLIENTI
413400160909     C**** 5             OCCUR     ABCD13
413500160909     C**** 5             OCCUR     ABCS13
413600160914     C     Direzionali   OCCUR     ABCD13
413700160914     C     Direzionali   OCCUR     ABCS13
413800030924     C                   Z-ADD     NTD           NTS
413900030924     C                   Z-ADD     ATD           ATS
414000030924     C*
414100030924     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
414200020318DO  2C     1             DO        5             K
414300030924     C     K             OCCUR     ABCDIVN
414400030924     C     K             OCCUR     ABCSTAN
414500030924     C                   Z-ADD     CNDN          CNSN
414600030924     C                   Z-ADD     SNDN          SNSN
414700030924     C                   Z-ADD     RNDN          RNSN
414800030924     C                   Z-ADD     PNDN          PNSN
414900030924     C                   Z-ADD     CADN          CASN
415000020318     C                   Z-ADD     SADN          SASN
415100020318     C                   Z-ADD     RADN          RASN
415200020318     C                   Z-ADD     PADN          PASN
415300020318     C*
415400020318     C* NUMERO TOTALE CLIENTI
415500020318     C     K             OCCUR     ABCD13N
415600020318     C     K             OCCUR     ABCS13N
415700020318     C                   Z-ADD     NTDN          NTSN
415800020318     C                   Z-ADD     ATDN          ATSN
415900020318     C*
416000020318E   2C                   ENDDO
416100020318     C*
416200020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
416300020318DO  2C     1             DO        5             K
416400020318     C     K             OCCUR     ABCDIVND
416500020318     C     K             OCCUR     ABCSTAND
416600020318     C                   Z-ADD     CNDND         CNSND
416700020318     C                   Z-ADD     SNDND         SNSND
416800020318     C                   Z-ADD     RNDND         RNSND
416900020318     C                   Z-ADD     PNDND         PNSND
417000020318     C                   Z-ADD     CADND         CASND
417100020318     C                   Z-ADD     SADND         SASND
417200020318     C                   Z-ADD     RADND         RASND
417300020318     C                   Z-ADD     PADND         PASND
417400020318     C*
417500020318     C* NUMERO TOTALE CLIENTI
417600020318     C     K             OCCUR     ABCD13ND
417700020318     C     K             OCCUR     ABCS13ND
417800020318     C                   Z-ADD     NTDND         NTSND
417900020318     C                   Z-ADD     ATDND         ATSND
418000020318     C*
418100020318E   2C                   ENDDO
418200020220     C*
418300020220     C* ROUTINE DI STAMPA TOTALI
418400020220     C                   EXSR      STAMPA
418500000000     C*
418600000000     C* SALTO PAGINA DOPO LA STAMPA DI QUESTI TOTALI
418700000000     C                   SETON                                        01
418800000000     C                   MOVEL     '0'           PRIGIR
418900000000     C*
419000000000E   1C                   ENDIF
419100000000     C*
419200000000     C                   ENDSR
419300000000     C*------------------------------------------------------------------------*
419400000000     C*?STAGEN - IMPOSTA STAMPA TOTALI GENERALI
419500000000     C*------------------------------------------------------------------------*
419600000000     C     STAGEN        BEGSR
419700000000     C*
419800000000     C* STAMPA VALORI GENERALI SOLO SE MEMORIZZATO QUALCOSA E SE STO ELABORANDO
419900020318     C* LA STATISTICA DELTA CON DETTAGLIO DIVISIONE
420000000000IF  1C     PASGEN        IFEQ      '1'
420100000000     C     PARSTA        ANDEQ     'F'
420200000000     C***
420300000000     C* DESCRIZIONI INTESTAZIONE
420400020116     C                   CLEAR                   SE94IN
420500000000     C                   Z-ADD     *ZEROS        INCAGE                         *AGENTE
420600000000     C                   MOVEL     DRI(1)        INDAGE                         *DESCRIZIONE AGENTE
420700000000     C                   Z-ADD     *ZEROS        INCFIL                         *FILIALE
420800000000     C                   MOVEL     DRI(2)        INDFIL                         *DESCRIZIONE FILIALE
420900000000     C                   Z-ADD     *ZEROS        INCARE                         *AREA
421000000000     C                   MOVEL     DRI(2)        INDARE                         *DESCRIZIONE AREA
421100000000     C                   MOVEL     *BLANKS       INCDIV                         *DIVISIONE
421200000000     C                   MOVEL     DRI(2)        INDDIV                         DESCRIZIONE DIVISION
421300950313     C                   MOVEL     PADIOE        INDIOE                         *DELTA ITALIA/ESTERO
421400000000     C                   MOVEL     *BLANKS       PIDDES                         *RETTANGOLINO
421500000000     C                   MOVEL     DRI(25)       PIDDES
421600000000     C*
421700000000     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA
421800160913      ** esclusi i direzionali "D"
421900160909DO  2C**** 1             DO        4             Q
422000160914DO  2C     1             DO        Altri_ECBAT   Q
422100000000     C     Q             OCCUR     ABCGEN
422200000000     C     Q             OCCUR     ABCSTA
422300000000     C                   Z-ADD     CNG           CNS
422400000000     C                   Z-ADD     SNG           SNS
422500000000     C                   Z-ADD     RNG           RNS
422600000000     C                   Z-ADD     PNG           PNS
422700000000     C                   Z-ADD     CAG           CAS
422800000000     C                   Z-ADD     SAG           SAS
422900000000     C                   Z-ADD     RAG           RAS
423000000000     C                   Z-ADD     PAG           PAS
423100000000     C*
423200000000     C* NUMERO TOTALE CLIENTI
423300000000     C     Q             OCCUR     ABCG13
423400000000     C     Q             OCCUR     ABCS13
423500000000     C                   Z-ADD     NTG           NTS
423600000000     C                   Z-ADD     ATG           ATS
423700000000     C*
423800000000E   2C                   ENDDO
423900020220     C*
424000020220     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA CLIENTI 'D'
424100160913      **   solo  i direzionali "D"
424200160909     C**** 5             OCCUR     ABCGEN
424300160909     C**** 5             OCCUR     ABCSTA
424400160914     C     Direzionali   OCCUR     ABCGEN
424500160914     C     Direzionali   OCCUR     ABCSTA
424600020220     C                   Z-ADD     CNG           CNS
424700020220     C                   Z-ADD     SNG           SNS
424800020220     C                   Z-ADD     RNG           RNS
424900020220     C                   Z-ADD     PNG           PNS
425000020220     C                   Z-ADD     CAG           CAS
425100020220     C                   Z-ADD     SAG           SAS
425200020220     C                   Z-ADD     RAG           RAS
425300020220     C                   Z-ADD     PAG           PAS
425400020220     C*
425500020220     C* NUMERO TOTALE CLIENTI 'D'
425600160909     C**** 5             OCCUR     ABCG13
425700160909     C**** 5             OCCUR     ABCS13
425800160914     C     Direzionali   OCCUR     ABCG13
425900160914     C     Direzionali   OCCUR     ABCS13
426000020220     C                   Z-ADD     NTG           NTS
426100020220     C                   Z-ADD     ATG           ATS
426200020318     C*
426300020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) NN "D"
426400020318DO  2C     1             DO        5             K
426500020318     C     K             OCCUR     ABCGENN
426600020318     C     K             OCCUR     ABCSTAN
426700020318     C                   Z-ADD     CNGN          CNSN
426800020318     C                   Z-ADD     SNGN          SNSN
426900020318     C                   Z-ADD     RNGN          RNSN
427000020318     C                   Z-ADD     PNGN          PNSN
427100020318     C                   Z-ADD     CAGN          CASN
427200020318     C                   Z-ADD     SAGN          SASN
427300020318     C                   Z-ADD     RAGN          RASN
427400020318     C                   Z-ADD     PAGN          PASN
427500020318     C*
427600020318     C* NUMERO TOTALE CLIENTI
427700020318     C     K             OCCUR     ABCG13N
427800020318     C     K             OCCUR     ABCS13N
427900020318     C                   Z-ADD     NTGN          NTSN
428000020318     C                   Z-ADD     ATGN          ATSN
428100020318     C*
428200020318E   2C                   ENDDO
428300020318     C*
428400020318     C* SCHIERE MEMORIZZAZIONE --> SCHIERE STAMPA (X NETWORK) "D"
428500020318DO  2C     1             DO        5             K
428600020318     C     K             OCCUR     ABCGENND
428700020318     C     K             OCCUR     ABCSTAND
428800020318     C                   Z-ADD     CNGND         CNSND
428900020318     C                   Z-ADD     SNGND         SNSND
429000020318     C                   Z-ADD     RNGND         RNSND
429100020318     C                   Z-ADD     PNGND         PNSND
429200020318     C                   Z-ADD     CAGND         CASND
429300020318     C                   Z-ADD     SAGND         SASND
429400020318     C                   Z-ADD     RAGND         RASND
429500020318     C                   Z-ADD     PAGND         PASND
429600020318     C*
429700020318     C* NUMERO TOTALE CLIENTI
429800020318     C     K             OCCUR     ABCG13ND
429900020318     C     K             OCCUR     ABCS13ND
430000020318     C                   Z-ADD     NTGND         NTSND
430100020318     C                   Z-ADD     ATGND         ATSND
430200020318     C*
430300020318E   2C                   ENDDO
430400020220     C*
430500020220     C* ROUTINE DI STAMPA TOTALI
430600020220     C                   EXSR      STAMPA
430700000000     C*
430800000000E   1C                   ENDIF
430900000000     C*
431000000000     C                   ENDSR
431100000000     C*------------------------------------------------------------------------*
431200000000     C*?STAMPA - STAMPA I TOTALI NELLA FORMA: TOT "A" / TOT "B" / TOT "C"
431300000000     C*------------------------------------------------------------------------*
431400000000     C     STAMPA        BEGSR
431500020221     C*--------------------
431600020221     C* I N T E S T A Z I O N E
431700020221     C*--------------------
431800020221     C                   EXSR      STAINT
431900000000     C*
432000000000     C* PULISCO I TOTALI A+B+C
432100000000     C                   CLEAR                   CNW
432200000000     C                   CLEAR                   SNW
432300000000     C                   CLEAR                   RNW
432400000000     C                   CLEAR                   PNW
432500000000     C                   CLEAR                   CAW
432600000000     C                   CLEAR                   SAW
432700000000     C                   CLEAR                   RAW
432800000000     C                   CLEAR                   PAW
432900000000     C*
433000020318     C                   Z-ADD     *ZEROS        CNW13C           11 0          *PERIODO CORRENTE
433100000000     C                   Z-ADD     *ZEROS        CAW13C           11 0
433200951227     C                   Z-ADD     *ZEROS        CN213C           11 0          *PERIODO PRECEDENTE
433300951227     C                   Z-ADD     *ZEROS        CA213C           11 0
433400000000     C*
433500160913      ** ESCLUSI I direzionali "D"
433600160909DO  1C*****1             DO        4             Q
433700160914DO  1C     1             DO        Altri_ECBAT   Q
433800000000     C     Q             OCCUR     ABCSTA
433900160913     C***                        ===
434000160913     C* TOTALI "A" + "B" + "C" + "E"  DIVISI IN NUOVI E ACQUISITI
434100160913     C***                        ===
434200000000     C                   ADD       CNS           CNW
434300000000     C                   ADD       SNS           SNW
434400000000     C                   ADD       RNS           RNW
434500000000     C                   ADD       PNS           PNW
434600000000     C                   ADD       CAS           CAW
434700000000     C                   ADD       SAS           SAW
434800000000     C                   ADD       RAS           RAW
434900000000     C                   ADD       PAS           PAW
435000951227     C*
435100000000     C     Q             OCCUR     ABCS13
435200951227     C* CORRENTE
435300000000     C                   ADD       NTS(1)        CNW13C
435400000000     C                   ADD       ATS(1)        CAW13C
435500951227     C* PRECEDENTE
435600951227     C                   ADD       NTS(2)        CN213C
435700011105     C                   ADD       ATS(2)        CA213C
435800000000     C*
435900000000E   1C                   ENDDO
436000020221     C*
436100000000     C***
436200000000     C* CLIENTI  N U O V I
436300000000     C***
436400160913      ** ESCLUSI I direzionali "D"
436500160909DO  1C**** 1             DO        4             Q
436600160914DO  1C     1             DO        Altri_ECBAT   Q
436700000000     C     Q             OCCUR     ABCSTA
436800000000     C*
436900000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
437000000000IF  2C     *IN01         IFEQ      *ON
437100020215     C                   EXSR      STAINT
437200000000E   2C                   ENDIF
437300020222     C*
437400020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
437500020222     C                   EVAL      DEPSTPD = '0'
437600000000     C*
437700000000     C                   XFOOT     CNS           COMCLI           16 0          *NUOVI DI CATEGORIA
437800000000IF  2C     COMCLI        IFGT      *ZEROS
437900000000     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
438000000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
438100020318     C                   MOVEL     '0'           FLGSTPNTW
438200000000     C                   Z-ADD     CNS           $CL                            NUMERO CLIENTI
438300000000     C                   Z-ADD     SNS           $SP                            SPEDIZIONI
438400000000     C                   Z-ADD     RNS           $RR                            RICAVI REALI
438500000000     C                   Z-ADD     PNS           $RP                            RICAVI PRESUNTI
438600000000     C*
438700000000     C     Q             OCCUR     ABCS13
438800951227     C                   Z-ADD     NTS(1)        $CL13C           11 0          N° TOT CLIENTI CORR
438900951227     C                   Z-ADD     NTS(2)        $C213C           11 0          N° TOT CLIENTI PREC
439000000000     C*
439100000000     C                   EXSR      STMDAT
439200000000E   2C                   ENDIF
439300000000     C*
439400000000E   1C                   ENDDO
439500000000     C*
439600020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
439700020318     C                   IF        PARSTA = 'K'
439800000000     C* T O T A L I   N U O V I
439900000000     C                   Z-ADD     CNW           $CL                            NUMERO CLIENTI
440000000000     C                   Z-ADD     SNW           $SP                            SPEDIZIONI
440100000000     C                   Z-ADD     RNW           $RR                            RICAVI REALI
440200000000     C                   Z-ADD     PNW           $RP                            RICAVI PRESUNTI
440300951227     C                   Z-ADD     CNW13C        $CL13C                         N° TOT CLIENTI CORR
440400951227     C                   Z-ADD     CN213C        $C213C                         N° TOT CLIENTI PREC
440500020222     C*
440600020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
440700020222IF  2C     *IN01         IFEQ      *ON
440800020222     C                   EXSR      STAINT
440900020222E   2C                   ENDIF
441000020222     C*
441100020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
441200020222     C                   EVAL      DEPSTPD = '1'
441300000000     C*
441400020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
441500000000IF  1C     COMCLI        IFGT      *ZEROS
441600000000     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
441700020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
441800020221     C                   EXSR      STMDAT
441900100129     c                   if        parSTAMPA <> 'N'
442000020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
442100100129     c                   end
442200020221E   1C                   ENDIF
442300020318     C*
442400020318     C                   ELSE
442500020318     C* T O T A L I   N U O V I
442600020318     C                   Z-ADD     CNW           $CL                            NUMERO CLIENTI
442700020318     C                   Z-ADD     SNW           $SP                            SPEDIZIONI
442800020318     C                   Z-ADD     RNW           $RR                            RICAVI REALI
442900020318     C                   Z-ADD     PNW           $RP                            RICAVI PRESUNTI
443000020318     C                   Z-ADD     CNW13C        $CL13C                         N° TOT CLIENTI CORR
443100020318     C                   Z-ADD     CN213C        $C213C                         N° TOT CLIENTI PREC
443200020318     C*
443300020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
443400020318IF  2C     *IN01         IFEQ      *ON
443500020318     C                   EXSR      STAINT
443600020318E   2C                   ENDIF
443700020318     C*
443800020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
443900020318     C                   EVAL      DEPSTPD = '1'
444000020318     C*
444100020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
444200020318IF  1C     COMCLI        IFGT      *ZEROS
444300020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
444400020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
444500020318     C                   EXSR      STMDAT
444600100129     c                   if        parSTAMPA <> 'N'
444700020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
444800100129     c                   end
444900020318E   1C                   ENDIF
445000020318     C***
445100020404     C* T O T A L I   N U O V I   X   N E T W O R K
445200020318     C***
445300020404     C*
445400020404     C* SOLO SE RICHIESTO NEL LANCIO
445500020404     C                   IF        PARNTW = 'S'
445600020404     C*
445700020319     C                   MOVEL     '0'           FLGSTPRI          1
445800020319     C*
445900020319DO  1C     2             DO        5             K
446000020318     C     K             OCCUR     ABCSTAN
446100020318     C*
446200020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
446300020318IF  2C     *IN01         IFEQ      *ON
446400020318     C                   EXSR      STAINT
446500020318E   2C                   ENDIF
446600020318     C*
446700020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
446800020318     C                   EVAL      DEPSTPD = '1'
446900020318     C*
447000020318     C                   XFOOT     CNSN          COMCLI           16 0          *NUOVI DI CATEGORIA
447100020318IF  2C     COMCLI        IFGT      *ZEROS
447200020319     C                   MOVEL     '1'           FLGSTPRI
447300020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
447400020318     C                   Z-ADD     K             $Q                             NETWORK
447500020318     C                   MOVEL     '1'           FLGSTPNTW
447600020318     C                   Z-ADD     CNSN          $CL                            NUMERO CLIENTI
447700020318     C                   Z-ADD     SNSN          $SP                            SPEDIZIONI
447800020318     C                   Z-ADD     RNSN          $RR                            RICAVI REALI
447900020318     C                   Z-ADD     PNSN          $RP                            RICAVI PRESUNTI
448000020318     C*
448100020318     C     K             OCCUR     ABCS13N
448200020318     C                   Z-ADD     NTSN(1)       $CL13C           11 0          N° TOT CLIENTI CORR
448300020318     C                   Z-ADD     NTSN(2)       $C213C           11 0          N° TOT CLIENTI PREC
448400020318     C*
448500020318     C                   EXSR      STMDAT
448600020318E   2C                   ENDIF
448700020318     C*
448800020318E   1C                   ENDDO
448900020319     C                   IF        FLGSTPRI = '1'
449000100129     c                   if        parSTAMPA <> 'N'
449100020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
449200100129     c                   end
449300020318     C                   ENDIF
449400020319     C                   ENDIF
449500020404     C*
449600020404     C                   ENDIF
449700020221     C*
449800020221     C* PULISCO I TOTALI D
449900020221     C                   CLEAR                   CNWD
450000020221     C                   CLEAR                   SNWD
450100020221     C                   CLEAR                   RNWD
450200020221     C                   CLEAR                   PNWD
450300020221     C                   CLEAR                   CAWD
450400020221     C                   CLEAR                   SAWD
450500020221     C                   CLEAR                   RAWD
450600020221     C                   CLEAR                   PAWD
450700020221     C*
450800020318     C                   Z-ADD     *ZEROS        CNW13CD          11 0          *PERIODO CORRENTE
450900020221     C                   Z-ADD     *ZEROS        CAW13CD          11 0
451000020221     C                   Z-ADD     *ZEROS        CN213CD          11 0          *PERIODO PRECEDENTE
451100020221     C                   Z-ADD     *ZEROS        CA213CD          11 0
451200020221     C*
451300160909     C**** 5             OCCUR     ABCSTA
451400160914     C     Direzionali   OCCUR     ABCSTA
451500020221     C***
451600020221     C* TOTALI "D" DIVISI IN NUOVI E ACQUISITI
451700020221     C***
451800020221     C                   ADD       CNS           CNWD
451900020221     C                   ADD       SNS           SNWD
452000020221     C                   ADD       RNS           RNWD
452100020221     C                   ADD       PNS           PNWD
452200020221     C                   ADD       CAS           CAWD
452300020221     C                   ADD       SAS           SAWD
452400020221     C                   ADD       RAS           RAWD
452500020221     C                   ADD       PAS           PAWD
452600020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
452700020221     C                   ADD       CNW           CNWD
452800020221     C                   ADD       SNW           SNWD
452900020221     C                   ADD       RNW           RNWD
453000020221     C                   ADD       PNW           PNWD
453100020221     C                   ADD       CAW           CAWD
453200020221     C                   ADD       SAW           SAWD
453300020221     C                   ADD       RAW           RAWD
453400020221     C                   ADD       PAW           PAWD
453500020221     C*
453600160909     C**** 5             OCCUR     ABCS13
453700160914     C     Direzionali   OCCUR     ABCS13
453800020221     C* CORRENTE
453900020221     C                   ADD       NTS(1)        CNW13CD
454000020221     C                   ADD       ATS(1)        CAW13CD
454100020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
454200020221     C                   ADD       CNW13C        CNW13CD
454300020221     C                   ADD       CAW13C        CAW13CD
454400020221     C* PRECEDENTE
454500020221     C                   ADD       NTS(2)        CN213CD
454600020221     C                   ADD       ATS(2)        CA213CD
454700020221     C* AI TOTALI DEI SOLI CLIENTI 'D' AGGIUNGO ANCHE I TOTALI DELLE ALTRE CATEGORIE X AVERE UN TOTA
454800020221     C                   ADD       CN213C        CN213CD
454900020221     C                   ADD       CA213C        CA213CD
455000020221     C***
455100020221     C* CLIENTI  N U O V I  'D'
455200020221     C***
455300160909     C*****5             OCCUR     ABCSTA
455400160914     C     Direzionali   OCCUR     ABCSTA
455500020221     C*
455600020221     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
455700020221IF  2C     *IN01         IFEQ      *ON
455800020221     C                   EXSR      STAINT
455900020221E   2C                   ENDIF
456000020222     C*
456100020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
456200020222     C                   EVAL      DEPSTPD = '0'
456300020221     C*
456400020221     C                   XFOOT     CNS           COMCLI           16 0          *NUOVI DI CATEGORIA
456500020221IF  2C     COMCLI        IFGT      *ZEROS
456600020221     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
456700160909     C**********         Z-ADD     5             $Q                             QUALITA' CLIENTE
456800160914     C                   Z-ADD     Direzionali   $Q                             QUALITA' CLIENTE
456900020318     C                   MOVEL     '0'           FLGSTPNTW
457000020221     C                   Z-ADD     CNS           $CL                            NUMERO CLIENTI
457100020221     C                   Z-ADD     SNS           $SP                            SPEDIZIONI
457200020221     C                   Z-ADD     RNS           $RR                            RICAVI REALI
457300020221     C                   Z-ADD     PNS           $RP                            RICAVI PRESUNTI
457400020221     C*
457500160909     C**** 5             OCCUR     ABCS13
457600160914     C     Direzionali   OCCUR     ABCS13
457700020221     C                   Z-ADD     NTS(1)        $CL13C           11 0          N° TOT CLIENTI CORR
457800020221     C                   Z-ADD     NTS(2)        $C213C           11 0          N° TOT CLIENTI PREC
457900020221     C*
458000020221     C                   EXSR      STMDAT
458100020221E   2C                   ENDIF
458200020318     C*
458300020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
458400020318     C                   IF        PARSTA = 'K'
458500020221     C* T O T A L I   N U O V I   'D'
458600020221     C                   Z-ADD     CNWD          $CL                            NUMERO CLIENTI
458700020221     C                   Z-ADD     SNWD          $SP                            SPEDIZIONI
458800020221     C                   Z-ADD     RNWD          $RR                            RICAVI REALI
458900020221     C                   Z-ADD     PNWD          $RP                            RICAVI PRESUNTI
459000020221     C                   Z-ADD     CNW13CD       $CL13C                         N° TOT CLIENTI CORR
459100020221     C                   Z-ADD     CN213CD       $C213C                         N° TOT CLIENTI PREC
459200020222     C*
459300020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
459400020222IF  2C     *IN01         IFEQ      *ON
459500020222     C                   EXSR      STAINT
459600020222E   2C                   ENDIF
459700030311     C*
459800030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
459900070821     C                   EVAL      DEPSTPD = '3'
460000020221     C*
460100020221IF  1C     COMCLI        IFGT      *ZEROS
460200020221     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
460300020221     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
460400020221     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
460500020221     C                   EXSR      STMDAT
460600100129     c                   if        parSTAMPA <> 'N'
460700020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
460800100129     c                   end
460900020221E   1C                   ENDIF
461000020318     C*
461100020318     C                   ELSE
461200020318     C* T O T A L I   N U O V I   'D'
461300020318     C                   Z-ADD     CNWD          $CL                            NUMERO CLIENTI
461400020318     C                   Z-ADD     SNWD          $SP                            SPEDIZIONI
461500020318     C                   Z-ADD     RNWD          $RR                            RICAVI REALI
461600020318     C                   Z-ADD     PNWD          $RP                            RICAVI PRESUNTI
461700020318     C                   Z-ADD     CNW13CD       $CL13C                         N° TOT CLIENTI CORR
461800020318     C                   Z-ADD     CN213CD       $C213C                         N° TOT CLIENTI PREC
461900020318     C*
462000020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
462100020318IF  2C     *IN01         IFEQ      *ON
462200020318     C                   EXSR      STAINT
462300020318E   2C                   ENDIF
462400030311     C*
462500030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
462600070821     C                   EVAL      DEPSTPD = '3'
462700020318     C*
462800020318IF  1C     COMCLI        IFGT      *ZEROS
462900020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
463000020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
463100020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
463200020318     C                   EXSR      STMDAT
463300100129     c                   if        parSTAMPA <> 'N'
463400020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
463500100129     c                   end
463600020318E   1C                   ENDIF
463700020318     C***
463800020318     C* T O T A L I   N U O V I   "D"   X   N E T W O R K
463900020318     C***
464000020404     C*
464100020404     C* SOLO SE RICHIESTO NEL LANCIO
464200020404     C                   IF        PARNTW = 'S'
464300020404     C*
464400020319     C                   MOVEL     '0'           FLGSTPRI          1
464500020319DO  1C     2             DO        5             K
464600020318     C     K             OCCUR     ABCSTAND
464700020318     C*
464800020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
464900020318IF  2C     *IN01         IFEQ      *ON
465000020318     C                   EXSR      STAINT
465100020318E   2C                   ENDIF
465200020318     C*
465300020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
465400020318     C                   EVAL      DEPSTPD = '2'
465500020318     C*
465600020318     C                   XFOOT     CNSND         COMCLI           16 0          *NUOVI DI CATEGORIA
465700020318IF  2C     COMCLI        IFGT      *ZEROS
465800020319     C                   MOVEL     '1'           FLGSTPRI
465900020318     C                   MOVEL     'N'           $CATCL                         CATEGORIA DA STAMPAR
466000020318     C                   Z-ADD     K             $Q                             NETWORK
466100020318     C                   MOVEL     '1'           FLGSTPNTW
466200020318     C                   Z-ADD     CNSND         $CL                            NUMERO CLIENTI
466300020318     C                   Z-ADD     SNSND         $SP                            SPEDIZIONI
466400020318     C                   Z-ADD     RNSND         $RR                            RICAVI REALI
466500020318     C                   Z-ADD     PNSND         $RP                            RICAVI PRESUNTI
466600020318     C*
466700020318     C     K             OCCUR     ABCS13ND
466800020318     C                   Z-ADD     NTSND(1)      $CL13C           11 0          N° TOT CLIENTI CORR
466900020318     C                   Z-ADD     NTSND(2)      $C213C           11 0          N° TOT CLIENTI PREC
467000020318     C*
467100020318     C                   EXSR      STMDAT
467200020318E   2C                   ENDIF
467300020318     C*
467400020318E   1C                   ENDDO
467500020319     C                   IF        FLGSTPRI = '1'
467600100129     c                   if        parSTAMPA <> 'N'
467700020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
467800100129     c                   end
467900020318     C                   ENDIF
468000020319     C                   ENDIF
468100020221     C*
468200020404     C                   ENDIF
468300020404     C*
468400020221     C***
468500020318     C* CLIENTI  A C Q U I S I T I
468600000000     C***
468700160909DO  1C**** 1             DO        4             Q
468800160914DO  1C     1             DO        Altri_ECBAT   Q
468900000000     C     Q             OCCUR     ABCSTA
469000000000     C*
469100000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
469200000000IF  2C     *IN01         IFEQ      *ON
469300020215     C                   EXSR      STAINT
469400000000E   2C                   ENDIF
469500020222     C*
469600020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
469700020222     C                   EVAL      DEPSTPD = '0'
469800000000     C*
469900020116     C                   XFOOT     CAS           COMCLI                         *NUOVI DI CATEGORIA
470000000000IF  2C     COMCLI        IFGT      *ZEROS
470100000000     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
470200000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
470300020318     C                   MOVEL     '0'           FLGSTPNTW
470400000000     C                   Z-ADD     CAS           $CL                            NUMERO CLIENTI
470500000000     C                   Z-ADD     SAS           $SP                            SPEDIZIONI
470600000000     C                   Z-ADD     RAS           $RR                            RICAVI REALI
470700000000     C                   Z-ADD     PAS           $RP                            RICAVI PRESUNTI
470800000000     C*
470900000000     C     Q             OCCUR     ABCS13
471000020116     C                   Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
471100020116     C                   Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
471200000000     C                   EXSR      STMDAT
471300000000E   2C                   ENDIF
471400000000     C*
471500000000E   1C                   ENDDO
471600020318     C*
471700020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
471800020318     C                   IF        PARSTA = 'K'
471900000000     C* T O T A L I   A C Q U I S I T I
472000000000     C                   Z-ADD     CAW           $CL                            NUMERO CLIENTI
472100000000     C                   Z-ADD     SAW           $SP                            SPEDIZIONI
472200000000     C                   Z-ADD     RAW           $RR                            RICAVI REALI
472300000000     C                   Z-ADD     PAW           $RP                            RICAVI PRESUNTI
472400951227     C                   Z-ADD     CAW13C        $CL13C                         N° TOT CLIENTI CORR
472500951227     C                   Z-ADD     CA213C        $C213C                         N° TOT CLIENTI PREC
472600020222     C*
472700020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
472800020222IF  2C     *IN01         IFEQ      *ON
472900020222     C                   EXSR      STAINT
473000020222E   2C                   ENDIF
473100020222     C*
473200020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
473300020222     C                   EVAL      DEPSTPD = '1'
473400000000     C*
473500020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
473600000000IF  1C     COMCLI        IFGT      *ZEROS
473700000000     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
473800000000     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
473900000000     C                   EXSR      STMDAT
474000100129     c                   if        parSTAMPA <> 'N'
474100020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
474200100129     c                   end
474300020221E   1C                   ENDIF
474400020318     C*
474500020318     C                   ELSE
474600020318     C* T O T A L I   A C Q U I S I T I
474700020318     C                   Z-ADD     CAW           $CL                            NUMERO CLIENTI
474800020318     C                   Z-ADD     SAW           $SP                            SPEDIZIONI
474900020318     C                   Z-ADD     RAW           $RR                            RICAVI REALI
475000020318     C                   Z-ADD     PAW           $RP                            RICAVI PRESUNTI
475100020318     C                   Z-ADD     CAW13C        $CL13C                         N° TOT CLIENTI CORR
475200020318     C                   Z-ADD     CA213C        $C213C                         N° TOT CLIENTI PREC
475300020318     C*
475400020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
475500020318IF  2C     *IN01         IFEQ      *ON
475600020318     C                   EXSR      STAINT
475700020318E   2C                   ENDIF
475800020318     C*
475900020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
476000020318     C                   EVAL      DEPSTPD = '1'
476100020318     C*
476200020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
476300020318IF  1C     COMCLI        IFGT      *ZEROS
476400050308IF  2C     $c213c        orGT      *ZEROS
476500020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
476600020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
476700020318     C                   EXSR      STMDAT
476800100129     c                   if        parSTAMPA <> 'N'
476900020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
477000100129     c                   end
477100020318E   1C                   ENDIF
477200020318     C***
477300020318     C* T O T A L I   A C Q I S I T I   X   N E T W O R K
477400020318     C***
477500020404     C*
477600020404     C* SOLO SE RICHIESTO NEL LANCIO
477700020404     C                   IF        PARNTW = 'S'
477800020404     C*
477900020319     C                   MOVEL     '0'           FLGSTPRI          1
478000020319     C*
478100020319DO  1C     2             DO        5             K
478200020318     C     K             OCCUR     ABCSTAN
478300020318     C*
478400020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
478500020318IF  2C     *IN01         IFEQ      *ON
478600020318     C                   EXSR      STAINT
478700020318E   2C                   ENDIF
478800020318     C*
478900020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
479000020318     C                   EVAL      DEPSTPD = '1'
479100050309     C     K             OCCUR     ABCS13N
479200050309     C                   Z-ADD     ATSN(1)       $CL13C                         N° TOT CLIENTI CORR
479300050309     C                   Z-ADD     ATSN(2)       $C213C                         N° TOT CLIENTI PREC
479400020318     C*
479500020318     C                   XFOOT     CASN          COMCLI                         *NUOVI DI CATEGORIA
479600020318IF  2C     COMCLI        IFGT      *ZEROS
479700050309IF  2C     $c213c        orGT      *ZEROS
479800020319     C                   MOVEL     '1'           FLGSTPRI
479900020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
480000020318     C                   Z-ADD     K             $Q                             NETWORK
480100020318     C                   MOVEL     '1'           FLGSTPNTW
480200020318     C                   Z-ADD     CASN          $CL                            NUMERO CLIENTI
480300020318     C                   Z-ADD     SASN          $SP                            SPEDIZIONI
480400020318     C                   Z-ADD     RASN          $RR                            RICAVI REALI
480500020318     C                   Z-ADD     PASN          $RP                            RICAVI PRESUNTI
480600050309     C* ???????
480700050309     C**** K             OCCUR     ABCS13N
480800050309     C*********          Z-ADD     ATSN(1)       $CL13C                         N° TOT CLIENTI CORR
480900050309     C*********          Z-ADD     ATSN(2)       $C213C                         N° TOT CLIENTI PREC
481000020318     C                   EXSR      STMDAT
481100020318E   2C                   ENDIF
481200020318     C*
481300020318E   1C                   ENDDO
481400020319     C                   IF        FLGSTPRI = '1'
481500100129     c                   if        parSTAMPA <> 'N'
481600020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
481700100129     c                   end
481800020318     C                   ENDIF
481900020319     C                   ENDIF
482000020404     C*
482100020404     C                   ENDIF
482200020221     C***
482300020221     C* CLIENTI  A C Q U I S I T I  'D'
482400020221     C***
482500160909     C*****5             OCCUR     ABCSTA
482600160914     C     Direzionali   OCCUR     ABCSTA
482700020221     C*
482800020221     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
482900020221IF  2C     *IN01         IFEQ      *ON
483000020221     C                   EXSR      STAINT
483100020221E   2C                   ENDIF
483200020222     C*
483300020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
483400020222     C                   EVAL      DEPSTPD = '0'
483500160909     C*****5             OCCUR     ABCS13
483600160914     C     Direzionali   OCCUR     ABCS13
483700050309     C                   Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
483800050309     C                   Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
483900020221     C*
484000020221     C                   XFOOT     CAS           COMCLI                         *NUOVI DI CATEGORIA
484100020221IF  2C     COMCLI        IFGT      *ZEROS
484200050309IF  2C     $c213c        orGT      *ZEROS
484300020221     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
484400160909     C*******            Z-ADD     5             $Q                             QUALITA' CLIENTE
484500160914     C                   Z-ADD     Direzionali   $Q                             QUALITA' CLIENTE
484600020318     C                   MOVEL     '0'           FLGSTPNTW
484700020221     C                   Z-ADD     CAS           $CL                            NUMERO CLIENTI
484800020221     C                   Z-ADD     SAS           $SP                            SPEDIZIONI
484900020221     C                   Z-ADD     RAS           $RR                            RICAVI REALI
485000020221     C                   Z-ADD     PAS           $RP                            RICAVI PRESUNTI
485100050309     C* ???????
485200050309     C**** 5             OCCUR     ABCS13
485300050309     C**********         Z-ADD     ATS(1)        $CL13C                         N° TOT CLIENTI CORR
485400050309     C**********         Z-ADD     ATS(2)        $C213C                         N° TOT CLIENTI PREC
485500020221     C                   EXSR      STMDAT
485600020221E   2C                   ENDIF
485700020318     C*
485800020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
485900020318     C                   IF        PARSTA = 'K'
486000020221     C* T O T A L I   A C Q U I S I T I  'D'
486100020221     C                   Z-ADD     CAWD          $CL                            NUMERO CLIENTI
486200020221     C                   Z-ADD     SAWD          $SP                            SPEDIZIONI
486300020221     C                   Z-ADD     RAWD          $RR                            RICAVI REALI
486400020221     C                   Z-ADD     PAWD          $RP                            RICAVI PRESUNTI
486500020221     C                   Z-ADD     CAW13CD       $CL13C                         N° TOT CLIENTI CORR
486600020221     C                   Z-ADD     CA213CD       $C213C                         N° TOT CLIENTI PREC
486700020222     C*
486800020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
486900020222IF  2C     *IN01         IFEQ      *ON
487000020222     C                   EXSR      STAINT
487100020222E   2C                   ENDIF
487200020222     C*
487300020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
487400030520     C                   EVAL      DEPSTPD = '3'
487500020221     C*
487600020221IF  1C     COMCLI        IFGT      *ZEROS
487700020221     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
487800020221     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
487900020221     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
488000020221     C                   EXSR      STMDAT
488100100129     c                   if        parSTAMPA <> 'N'
488200020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
488300100129     c                   end
488400020221E   1C                   ENDIF
488500020318     C*
488600020318     C                   ELSE
488700020318     C* T O T A L I   A C Q U I S I T I  'D'
488800020318     C                   Z-ADD     CAWD          $CL                            NUMERO CLIENTI
488900020318     C                   Z-ADD     SAWD          $SP                            SPEDIZIONI
489000020318     C                   Z-ADD     RAWD          $RR                            RICAVI REALI
489100020318     C                   Z-ADD     PAWD          $RP                            RICAVI PRESUNTI
489200020318     C                   Z-ADD     CAW13CD       $CL13C                         N° TOT CLIENTI CORR
489300020318     C                   Z-ADD     CA213CD       $C213C                         N° TOT CLIENTI PREC
489400020318     C*
489500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
489600020318IF  2C     *IN01         IFEQ      *ON
489700020318     C                   EXSR      STAINT
489800020318E   2C                   ENDIF
489900020318     C*
490000020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
490100030520     C                   EVAL      DEPSTPD = '3'
490200020318     C*
490300050308     C                   xfoot     $cl           comcli                         *NUOVI DI CATEGORIA
490400020318IF  1C     COMCLI        IFGT      *ZEROS
490500050308IF  2C     $c213c        orGT      *ZEROS
490600020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
490700020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
490800020318     C                   EXSR      STMDAT
490900100129     c                   if        parSTAMPA <> 'N'
491000020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
491100100129     c                   end
491200020318E   1C                   ENDIF
491300020318     C***
491400050308     C* T O T A L I   A C Q U I S I T I   X   N E T W O R K  "D"
491500020318     C***
491600020404     C*
491700020404     C* SOLO SE RICHIESTO NEL LANCIO
491800020404     C                   IF        PARNTW = 'S'
491900020404     C*
492000020319     C                   MOVEL     '0'           FLGSTPRI          1
492100020319     C*
492200020319DO  1C     2             DO        5             K
492300020318     C     K             OCCUR     ABCSTAND
492400020318     C*
492500020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
492600020318IF  2C     *IN01         IFEQ      *ON
492700020318     C                   EXSR      STAINT
492800020318E   2C                   ENDIF
492900020318     C*
493000020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
493100020318     C                   EVAL      DEPSTPD = '2'
493200020318     C*
493300050309     C     K             OCCUR     ABCS13ND
493400050309     C                   Z-ADD     ATSND(1)      $CL13C                         N° TOT CLIENTI CORR
493500050309     C                   Z-ADD     ATSND(2)      $C213C                         N° TOT CLIENTI PREC
493600050308     C*
493700020318     C                   XFOOT     CASND         COMCLI                         *NUOVI DI CATEGORIA
493800020318IF  2C     COMCLI        IFGT      *ZEROS
493900050309IF  2C     $c213c        orGT      *ZEROS
494000020319     C                   MOVEL     '1'           FLGSTPRI
494100020318     C                   MOVEL     'A'           $CATCL                         CATEGORIA DA STAMPAR
494200020318     C                   Z-ADD     K             $Q                             NETWORK
494300020318     C                   MOVEL     '1'           FLGSTPNTW
494400020318     C                   Z-ADD     CASND         $CL                            NUMERO CLIENTI
494500020318     C                   Z-ADD     SASND         $SP                            SPEDIZIONI
494600020318     C                   Z-ADD     RASND         $RR                            RICAVI REALI
494700020318     C                   Z-ADD     PASND         $RP                            RICAVI PRESUNTI
494800050309     C* ???????
494900050309     C**** K             OCCUR     ABCS13ND
495000050309     C********           Z-ADD     ATSND(1)      $CL13C                         N° TOT CLIENTI CORR
495100050309     C********           Z-ADD     ATSND(2)      $C213C                         N° TOT CLIENTI PREC
495200020318     C                   EXSR      STMDAT
495300020318E   2C                   ENDIF
495400020318     C*
495500020318E   1C                   ENDDO
495600020319     C                   IF        FLGSTPRI = '1'
495700100129     c                   if        parSTAMPA <> 'N'
495800020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
495900100129     c                   end
496000020318     C                   ENDIF
496100020319     C                   ENDIF
496200020404     C*
496300020404     C                   ENDIF
496400020221     C*
496500020319     C* PRIMA DEI TOTALI STAMPO SEMPRE E CMQ 1 RIGA DOPPIA X EVIDENZIARE L'INIZIO DEI TOTALI
496600100129     c                   if        parSTAMPA <> 'N'
496700020319     C  N53              WRITE     SE94R3                                       *RIGA SEPARAZIONE DO
496800100129     c                   end
496900020319     C*
497000020318     C***
497100020318     C* CLIENTI  T O T A L I
497200000000     C***
497300160909DO  1C*****1             DO        4             Q
497400160914DO  1C     1             DO        Altri_ECBAT   Q
497500000000     C     Q             OCCUR     ABCSTA
497600000000     C*
497700000000     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
497800000000IF  2C     *IN01         IFEQ      *ON
497900020215     C                   EXSR      STAINT
498000000000E   2C                   ENDIF
498100020222     C*
498200020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
498300020222     C                   EVAL      DEPSTPD = '0'
498400000000     C*
498500000000     C     CNS           ADD       CAS           $CL                            NUMERO CLIENTI
498600000000     C     SNS           ADD       SAS           $SP                            SPEDIZIONI
498700000000     C     RNS           ADD       RAS           $RR                            RICAVI REALI
498800000000     C     PNS           ADD       PAS           $RP                            RICAVI PRESUNTI
498900000000     C*
499000000000     C     Q             OCCUR     ABCS13
499100020116     C     NTS(1)        ADD       ATS(1)        $CL13C                         N° TO CLIENTI CORR
499200020116     C     NTS(2)        ADD       ATS(2)        $C213C                         N° TO CLIENTI PREC
499300000000     C*
499400020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
499500000000IF  2C     COMCLI        IFGT      *ZEROS
499600050301IF  2C     $c213c        orGT      *ZEROS
499700000000     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
499800000000     C                   Z-ADD     Q             $Q                             QUALITA' CLIENTE
499900020318     C                   MOVEL     '0'           FLGSTPNTW
500000000000     C                   EXSR      STMDAT
500100000000E   2C                   ENDIF
500200000000     C*
500300000000E   1C                   ENDDO
500400020318     C*
500500020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
500600020318     C                   IF        PARSTA = 'K'
500700000000     C* T O T A L I   T O T A L I
500800000000     C     CNW           ADD       CAW           $CL                            NUMERO CLIENTI
500900000000     C     SNW           ADD       SAW           $SP                            SPEDIZIONI
501000000000     C     RNW           ADD       RAW           $RR                            RICAVI REALI
501100000000     C     PNW           ADD       PAW           $RP                            RICAVI PRESUNTI
501200951227     C     CNW13C        ADD       CAW13C        $CL13C                         N° TOT CLIENTI CORR
501300951227     C     CN213C        ADD       CA213C        $C213C                         N° TOT CLIENTI CORR
501400020222     C*
501500020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
501600020222IF  2C     *IN01         IFEQ      *ON
501700020222     C                   EXSR      STAINT
501800020222E   2C                   ENDIF
501900020222     C*
502000020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
502100020222     C                   EVAL      DEPSTPD = '1'
502200000000     C*
502300020116     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
502400000000IF  1C     COMCLI        IFGT      *ZEROS
502500050301IF  2C     $c213c        orGT      *ZEROS
502600000000     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
502700000000     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
502800000000     C                   EXSR      STMDAT
502900100129     c                   if        parSTAMPA <> 'N'
503000020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
503100100129     c                   end
503200020221E   1C                   ENDIF
503300020318     C*
503400020318     C                   ELSE
503500020318     C* T O T A L I   T O T A L I
503600020318     C     CNW           ADD       CAW           $CL                            NUMERO CLIENTI
503700020318     C     SNW           ADD       SAW           $SP                            SPEDIZIONI
503800020318     C     RNW           ADD       RAW           $RR                            RICAVI REALI
503900020318     C     PNW           ADD       PAW           $RP                            RICAVI PRESUNTI
504000020318     C     CNW13C        ADD       CAW13C        $CL13C                         N° TOT CLIENTI CORR
504100020318     C     CN213C        ADD       CA213C        $C213C                         N° TOT CLIENTI CORR
504200020318     C*
504300020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
504400020318IF  2C     *IN01         IFEQ      *ON
504500020318     C                   EXSR      STAINT
504600020318E   2C                   ENDIF
504700020318     C*
504800020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
504900020318     C                   EVAL      DEPSTPD = '1'
505000020318     C*
505100020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
505200020318IF  1C     COMCLI        IFGT      *ZEROS
505300050301IF  2C     $c213c        orGT      *ZEROS
505400020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
505500020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
505600020318     C                   EXSR      STMDAT
505700100129     c                   if        parSTAMPA <> 'N'
505800020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
505900100129     c                   end
506000020318E   1C                   ENDIF
506100020318     C* T O T A L I   T O T A L I   X   N E T W O R K
506200020404     C*
506300020404     C* SOLO SE RICHIESTO NEL LANCIO
506400020404     C                   IF        PARNTW = 'S'
506500020404     C*
506600020319     C                   MOVEL     '0'           FLGSTPRI          1
506700020319     C*
506800020319DO  1C     2             DO        5             K
506900020318     C     K             OCCUR     ABCSTAN
507000020318     C*
507100020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
507200020318IF  2C     *IN01         IFEQ      *ON
507300020318     C                   EXSR      STAINT
507400020318E   2C                   ENDIF
507500020318     C*
507600020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
507700020318     C                   EVAL      DEPSTPD = '1'
507800020318     C*
507900020318     C     CNSN          ADD       CASN          $CL                            NUMERO CLIENTI
508000020318     C     SNSN          ADD       SASN          $SP                            SPEDIZIONI
508100020318     C     RNSN          ADD       RASN          $RR                            RICAVI REALI
508200020318     C     PNSN          ADD       PASN          $RP                            RICAVI PRESUNTI
508300020318     C*
508400020318     C     K             OCCUR     ABCS13N
508500020318     C     NTSN(1)       ADD       ATSN(1)       $CL13C                         N° TO CLIENTI CORR
508600020318     C     NTSN(2)       ADD       ATSN(2)       $C213C                         N° TO CLIENTI PREC
508700020318     C*
508800020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
508900020318IF  2C     COMCLI        IFGT      *ZEROS
509000050301IF  2C     $c213c        orGT      *ZEROS
509100020319     C                   MOVEL     '1'           FLGSTPRI
509200020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
509300020318     C                   Z-ADD     K             $Q                             NETWORK
509400020318     C                   MOVEL     '1'           FLGSTPNTW
509500020318     C                   EXSR      STMDAT
509600020318E   2C                   ENDIF
509700020318     C*
509800020318E   1C                   ENDDO
509900020319     C                   IF        FLGSTPRI = '1'
510000100129     c                   if        parSTAMPA <> 'N'
510100020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
510200100129     c                   end
510300020318     C                   ENDIF
510400020319     C                   ENDIF
510500020404     C*
510600020404     C                   ENDIF
510700020220     C***
510800020220     C* CLIENTI  T O T A L I  'D'
510900020220     C***
511000160909     C**** 5             OCCUR     ABCSTA
511100160914     C     Direzionali   OCCUR     ABCSTA
511200020220     C*
511300020220     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
511400020220IF  2C     *IN01         IFEQ      *ON
511500020220     C                   EXSR      STAINT
511600020220E   2C                   ENDIF
511700020222     C*
511800020222     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D'
511900020222     C                   EVAL      DEPSTPD = '0'
512000020220     C*
512100020220     C     CNS           ADD       CAS           $CL                            NUMERO CLIENTI
512200020220     C     SNS           ADD       SAS           $SP                            SPEDIZIONI
512300020220     C     RNS           ADD       RAS           $RR                            RICAVI REALI
512400020220     C     PNS           ADD       PAS           $RP                            RICAVI PRESUNTI
512500020220     C*
512600160909     C**** 5             OCCUR     ABCS13
512700160914     C     Direzionali   OCCUR     ABCS13
512800020220     C     NTS(1)        ADD       ATS(1)        $CL13C                         N° TO CLIENTI CORR
512900020220     C     NTS(2)        ADD       ATS(2)        $C213C                         N° TO CLIENTI PREC
513000020220     C*
513100020220     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
513200020220IF  2C     COMCLI        IFGT      *ZEROS
513300050301IF  2C     $c213c        orGT      *ZEROS
513400020220     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
513500160909     C*****              Z-ADD     5             $Q                             QUALITA' CLIENTE
513600160914     C                   Z-ADD     Direzionali   $Q                             QUALITA' CLIENTE
513700020318     C                   MOVEL     '0'           FLGSTPNTW
513800020220     C                   EXSR      STMDAT
513900020220E   2C                   ENDIF
514000020318     C*
514100020318     C* VERIFICO IL TIPO DI LANCIO, SE X FILIALE O DIVERSA
514200020318     C                   IF        PARSTA = 'K'
514300020221     C* T O T A L I   T O T A L I   'D'
514400020221     C     CNWD          ADD       CAWD          $CL                            NUMERO CLIENTI
514500020221     C     SNWD          ADD       SAWD          $SP                            SPEDIZIONI
514600020221     C     RNWD          ADD       RAWD          $RR                            RICAVI REALI
514700020221     C     PNWD          ADD       PAWD          $RP                            RICAVI PRESUNTI
514800020221     C     CNW13CD       ADD       CAW13CD       $CL13C                         N° TOT CLIENTI CORR
514900020221     C     CN213CD       ADD       CA213CD       $C213C                         N° TOT CLIENTI CORR
515000020222     C*
515100020222     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
515200020222IF  2C     *IN01         IFEQ      *ON
515300020222     C                   EXSR      STAINT
515400020222E   2C                   ENDIF
515500030311     C*
515600030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D' + TOTALE ALT
515700030311     C                   EVAL      DEPSTPD = '3'
515800020220     C*
515900020221IF  1C     COMCLI        IFGT      *ZEROS
516000050301IF  2C     $c213c        orGT      *ZEROS
516100020220     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
516200020220     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
516300020220     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
516400020220     C                   EXSR      STMDAT
516500100129     c                   if        parSTAMPA <> 'N'
516600020221     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
516700100129     c                   end
516800020221E   1C                   ENDIF
516900020318     C*
517000020318     C                   ELSE
517100020318     C* T O T A L I   T O T A L I   'D'
517200020318     C     CNWD          ADD       CAWD          $CL                            NUMERO CLIENTI
517300020318     C     SNWD          ADD       SAWD          $SP                            SPEDIZIONI
517400020318     C     RNWD          ADD       RAWD          $RR                            RICAVI REALI
517500020318     C     PNWD          ADD       PAWD          $RP                            RICAVI PRESUNTI
517600020318     C     CNW13CD       ADD       CAW13CD       $CL13C                         N° TOT CLIENTI CORR
517700020318     C     CN213CD       ADD       CA213CD       $C213C                         N° TOT CLIENTI CORR
517800020318     C*
517900020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
518000020318IF  2C     *IN01         IFEQ      *ON
518100020318     C                   EXSR      STAINT
518200020318E   2C                   ENDIF
518300030311     C*
518400030311     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI 'D' + TOTALE ALT
518500030311     C                   EVAL      DEPSTPD = '3'
518600020318     C*
518700020318IF  1C     COMCLI        IFGT      *ZEROS
518800050301IF  2C     $c213c        orGT      *ZEROS
518900020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
519000020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
519100020318     C                   Z-ADD     *ZEROS        $Q                             NO QUALITA PER TOTAL
519200020318     C                   EXSR      STMDAT
519300100129     c                   if        parSTAMPA <> 'N'
519400020318     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
519500100129     c                   end
519600020318E   1C                   ENDIF
519700020318     C* T O T A L I   T O T A L I   X   N E T W O R K   "D"
519800020404     C*
519900020404     C* SOLO SE RICHIESTO NEL LANCIO
520000020404     C                   IF        PARNTW = 'S'
520100020404     C*
520200020319     C                   MOVEL     '0'           FLGSTPRI          1
520300020319     C*
520400020319DO  1C     2             DO        5             K
520500020318     C     K             OCCUR     ABCSTAND
520600020318     C*
520700020318     C* SE SONO ARRIVATO A FINE PAGINA RISTAMPO LA TESTATA
520800020318IF  2C     *IN01         IFEQ      *ON
520900020318     C                   EXSR      STAINT
521000020318E   2C                   ENDIF
521100020318     C*
521200020318     C* IMPOSTO IL FLAG CHE MI INDENTIFICA CHE ANDRO' A STAMPARE I DATI DEI CLIENTI NON 'D'
521300020318     C                   EVAL      DEPSTPD = '2'
521400020318     C*
521500020318     C     CNSND         ADD       CASND         $CL                            NUMERO CLIENTI
521600020318     C     SNSND         ADD       SASND         $SP                            SPEDIZIONI
521700020318     C     RNSND         ADD       RASND         $RR                            RICAVI REALI
521800020318     C     PNSND         ADD       PASND         $RP                            RICAVI PRESUNTI
521900020318     C*
522000020318     C     K             OCCUR     ABCS13ND
522100020318     C     NTSND(1)      ADD       ATSND(1)      $CL13C                         N° TO CLIENTI CORR
522200020318     C     NTSND(2)      ADD       ATSND(2)      $C213C                         N° TO CLIENTI PREC
522300020318     C*
522400020318     C                   XFOOT     $CL           COMCLI                         *NUOVI DI CATEGORIA
522500020318IF  2C     COMCLI        IFGT      *ZEROS
522600050301IF  2C     $C213C        orGT      *ZEROS
522700020319     C                   MOVEL     '1'           FLGSTPRI
522800020318     C                   MOVEL     'T'           $CATCL                         CATEGORIA DA STAMPAR
522900020318     C                   Z-ADD     K             $Q                             NETWORK
523000020318     C                   MOVEL     '1'           FLGSTPNTW
523100020318     C                   EXSR      STMDAT
523200020318E   2C                   ENDIF
523300020318     C*
523400020318E   1C                   ENDDO
523500020319     C                   IF        FLGSTPRI = '1'
523600100129     c                   if        parSTAMPA <> 'N'
523700020319     C  N53              WRITE     SE94R2                                       *RIGA SEPARAZIONE
523800100129     c                   end
523900020318     C                   ENDIF
524000020319     C                   ENDIF
524100000000     C*
524200020404     C                   ENDIF
524300020404     C*
524400000000     C                   ENDSR
524500000000     C*------------------------------------------------------------------------*
524600011123     C*?STAINT - STAMPA INTESTAZIONE
524700000000     C*------------------------------------------------------------------------*
524800000000     C     STAINT        BEGSR
524900020221     C*
525000020222     C* SE NN E' LA 1° INTESTAZIONE => STAMPO LA LEGENDA E POI SALTO PAGINA
525100020222     C                   IF        FLGSTPLE = '1'
525200100129     c                   if        parSTAMPA <> 'N'
525300020318     C                   WRITE     SE94R2
525400020222     C                   WRITE     SE94LE                                       *LEGENDA
525500100129     c                   end
525600020222     C                   ELSE
525700020222     C                   EVAL      FLGSTPLE = '1'
525800020222     C                   ENDIF
525900020222     C*
526000100129     c                   if        parSTAMPA <> 'N'
526100020222     C                   WRITE     SE94PT                                       *TESTATA
526200020116     C                   WRITE     SE94IN                                       *RIGA INTESTAZIONE
526300020116     C                   WRITE     SE94R1                                       *RIGA SEPARAZIONE
526400020116     C                   WRITE     SE94PI                                       *RIGA MESI
526500020116     C                   WRITE     SE94R2                                       *RIGA SEPARAZIONE
526600100129     c                   end
526700000000     C                   SETOFF                                       01
526800000000     C*
526900000000     C                   ENDSR
527000000000     C*------------------------------------------------------------------------*
527100000000     C*?STMDAT - STAMPA I DATI DEL CLIENTE
527200000000     C*          INPUT  --> $CATCL : CATEGORIA CLIENTI (N/A/T)
527300000000     C*                     $Q     : QUALITA' CLIENTI  (A/B/C)
527400000000     C*                     $CL    : NUMERO CLIENTI
527500000000     C*                     $SP    : SPEDIZIONI
527600000000     C*                     $RR    : RICAVI REALI
527700000000     C*                     $RP    : RICAVI PRESUNTI
527800000000     C*                     $CL13C : NUMERO TOTALE CLIENTI (13^ COLONNA)
527900951227     C*                     $C213C : NUMERO TOTALE CLIENTI (1^  COLONNA)
528000000000     C*------------------------------------------------------------------------*
528100000000     C     STMDAT        BEGSR
528200000000     C***
528300000000     C* CLIENTE NUOVI/ACQUISITI/TOTALI DI TIPO A/B/C
528400000000     C***
528500020221     C                   MOVEL     *BLANKS       DES12            12
528600000000IF  1C     $CATCL        IFEQ      'N'                                          *N U O V I
528700000000     C                   MOVEL     DRI(4)        DES12                          *PRIMA DESCRIZIONE
528800000000E   1C                   ENDIF
528900000000IF  1C     $CATCL        IFEQ      'A'                                          *A C Q U I S I T I
529000000000     C                   MOVEL     DRI(8)        DES12                          *PRIMA DESCRIZIONE
529100000000E   1C                   ENDIF
529200000000IF  1C     $CATCL        IFEQ      'T'                                          *T O T A L I
529300000000     C                   MOVEL     DRI(12)       DES12                          *PRIMA DESCRIZIONE
529400000000E   1C                   ENDIF
529500000000     C*
529600020318     C                   IF        FLGSTPNTW = '0'
529700000000IF  2C     $Q            IFGT      *ZEROS
529800000000     C                   MOVE      ABC($Q)       DES12                          *"A"  "B"  "C"  " "
529900000000E   2C                   ENDIF
530000020318     C                   ELSE
530100020318IF  2C     $Q            IFGT      *ZEROS
530200020318     C                   MOVE      DNTW($Q)      DES12                          *NETWORK
530300020318E   2C                   ENDIF
530400020318     C                   ENDIF
530500020221     C*
530600020221     C* SE STO STAMPANDO DATI DI CLIENTI 'D' => VALORIZZO IL CAMPO IN STAMPA X EVIDENZIARLI
530700020222     C                   IF        DEPSTPD = '0'
530800020222     C                   EVAL      P1DFLD = *BLANKS
530900020222     C                   ENDIF
531000020222     C                   IF        DEPSTPD = '1'
531100020222     C                   EVAL      P1DFLD = '*'
531200020222     C                   ENDIF
531300020222     C                   IF        DEPSTPD = '2'
531400020222     C                   EVAL      P1DFLD = '**'
531500020222     C                   ENDIF
531600030311     C                   IF        DEPSTPD = '3'
531700030520     C                   EVAL      P1DFLD = '§'
531800030311     C                   ENDIF
531900000000     C*--------------------
532000020221     C* C L I E N T I
532100000000     C*--------------------
532200000000     C                   MOVEL     DRI(13)       DES07             7            *SECONDA DESCRIZIONE
532300000000     C*
532400000000     C                   Z-ADD     $CL           $VA
532500020116     C                   Z-ADD     $CL13C        $VA13C                         *PERIODO CORRENTE
532600020116     C                   Z-ADD     $C213C        $V213C                         *PERIODO PRECEDENTE
532700000000     C*
532800000000     C                   MOVEL     *BLANKS       P1DDES
532900000000     C                   MOVEL     DES12         P1DDES
533000000000     C                   MOVE      DES07         P1DDES
533100000000     C*
533200000000     C* SCRIVE I VALORI IN STAMPA
533300000000     C                   EXSR      SCRVAL
533400030925     C*
533500030925     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
533600030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
533700030925     C                   MOVEL     PARIOE        SE93IOE                        *DELTA ITALIA/ESTERO
533800030925     C                   MOVEL     INCDIV        SE93DIV                        *DIVISIONE
533900030925     C                   MOVEL     INDDIV        SE93DDI                        *DESCRIZIONE DIVISIO
534000030925     C                   MOVEL     INCARE        SE93ARE                        *AREA
534100030925     C                   MOVEL     INDARE        SE93DAR                        *DESCRIZIONE AREA
534200030925     C                   MOVEL     INCFIL        SE93FIL                        *FILIALE
534300030925     C                   MOVEL     INDFIL        SE93DFI                        *DESCRIZIONE FILIALE
534400030925     C                   MOVEL     P1DFLD        SE93AST                        *CARATTERE STRANO
534500030925     C                   MOVEL     P1DDES        SE93DES                        *DESCRIZIONE
534600030925     C                   MOVEL     *BLANKS       SE93CLV
534700030926IF  2C                   IF        FLGSTPNTW = '0'                              *STAMPA NO NETWORK
534800030926IF  3C     $Q            IFGT      *ZEROS
534900030926     C                   MOVEL     ABC($Q)       SE93CLV                        *QUALITA' CLIENTE
535000030926E   3C                   ENDIF
535100030926E   2C                   ENDIF
535200030926     c*
535300030925     C                   MOVEL     '2'           SE93TIP                        *TIPO TOTALE
535400030925     C                   Z-ADD     $V213C        SE93CPR                        *VALORE PRECEDENTE
535500030925     C                   Z-ADD     $VA(1)        SE93C01                        *MESE UNO
535600030925     C                   Z-ADD     $VA(2)        SE93C02                        *MESE DUE
535700030925     C                   Z-ADD     $VA(3)        SE93C03                        *MESE TRE
535800030925     C                   Z-ADD     $VA(4)        SE93C04                        *MESE QUATTRO
535900030925     C                   Z-ADD     $VA(5)        SE93C05                        *MESE CINQUE
536000030925     C                   Z-ADD     $VA(6)        SE93C06                        *MESE SEI
536100030925     C                   Z-ADD     $VA(7)        SE93C07                        *MESE SETTE
536200030925     C                   Z-ADD     $VA(8)        SE93C08                        *MESE OTTO
536300030925     C                   Z-ADD     $VA(9)        SE93C09                        *MESE NOVE
536400030925     C                   Z-ADD     $VA(10)       SE93C10                        *MESE DIECI
536500030925     C                   Z-ADD     $VA(11)       SE93C11                        *MESE UNDICI
536600030925     C                   Z-ADD     $VA(12)       SE93C12                        *MESE DODICI
536700030925     C                   Z-ADD     $VA13C        SE93CTO                        *TOTALE MESI
536800030925E   1C                   ENDIF
536900000000     C*--------------------
537000000000     C* S P E D I Z I O N I
537100000000     C*--------------------
537200000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIIZONE
537300000000     C                   MOVEL     DRI(14)       DES07                          *SECONDA DESCRIZIONE
537400020221     C                   MOVEL     *BLANKS       P1DFLD
537500000000     C*
537600000000     C                   Z-ADD     $SP           $VA
537700000000     C                   Z-ADD     *ZEROS        $VA13C
537800000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
537900020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPEDIZ
538000000000E   1C                   ENDDO
538100020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
538200951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
538300000000     C*
538400000000     C                   MOVEL     *BLANKS       P1DDES
538500000000     C                   MOVEL     DES12         P1DDES
538600000000     C                   MOVE      DES07         P1DDES
538700000000     C*
538800000000     C* SCRIVE I VALORI IN STAMPA
538900000000     C                   EXSR      SCRVAL
539000030925     C*
539100030925     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
539200030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
539300030925     C                   Z-ADD     $V213C        SE93NPR                        *VALORE PRECEDENTE
539400030925     C                   Z-ADD     $VA(1)        SE93N01                        *MESE UNO
539500030925     C                   Z-ADD     $VA(2)        SE93N02                        *MESE DUE
539600030925     C                   Z-ADD     $VA(3)        SE93N03                        *MESE TRE
539700030925     C                   Z-ADD     $VA(4)        SE93N04                        *MESE QUATTRO
539800030925     C                   Z-ADD     $VA(5)        SE93N05                        *MESE CINQUE
539900030925     C                   Z-ADD     $VA(6)        SE93N06                        *MESE SEI
540000030925     C                   Z-ADD     $VA(7)        SE93N07                        *MESE SETTE
540100030925     C                   Z-ADD     $VA(8)        SE93N08                        *MESE OTTO
540200030925     C                   Z-ADD     $VA(9)        SE93N09                        *MESE NOVE
540300030925     C                   Z-ADD     $VA(10)       SE93N10                        *MESE DIECI
540400030925     C                   Z-ADD     $VA(11)       SE93N11                        *MESE UNDICI
540500030925     C                   Z-ADD     $VA(12)       SE93N12                        *MESE DODICI
540600030925     C                   Z-ADD     $VA13C        SE93NTO                        *TOTALE MESI
540700030925E   1C                   ENDIF
540800000000     C*--------------------
540900020220     C* R I C A V I
541000000000     C*--------------------
541100000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIIZONE
541200000000     C                   MOVEL     DRI(15)       DES07                          *SECONDA DESCRIZIONE
541300020221     C                   MOVEL     *BLANKS       P1DFLD
541400000000     C*
541500020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
541600020116     C*    $RR           DIV       1000          $VA
541700020116     C                   Z-ADD     $RR           $VA
541800951227     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
541900000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
542000020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
542100000000E   1C                   ENDDO
542200020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
542300951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
542400000000     C*
542500000000     C                   MOVEL     *BLANKS       P1DDES
542600000000     C                   MOVEL     DES12         P1DDES
542700000000     C                   MOVE      DES07         P1DDES
542800000000     C*
542900000000     C* SCRIVE I VALORI IN STAMPA
543000000000     C                   EXSR      SCRVAL
543100030925     C*
543200030925     C* IMPOSTA I CAMPI "RICAVI" DEL WFILE DI STAMPA
543300030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
543400030925     C                   Z-ADD     $V213C        SE93RPR                        *VALORE PRECEDENTE
543500030925     C                   Z-ADD     $VA(1)        SE93R01                        *MESE UNO
543600030925     C                   Z-ADD     $VA(2)        SE93R02                        *MESE DUE
543700030925     C                   Z-ADD     $VA(3)        SE93R03                        *MESE TRE
543800030925     C                   Z-ADD     $VA(4)        SE93R04                        *MESE QUATTRO
543900030925     C                   Z-ADD     $VA(5)        SE93R05                        *MESE CINQUE
544000030925     C                   Z-ADD     $VA(6)        SE93R06                        *MESE SEI
544100030925     C                   Z-ADD     $VA(7)        SE93R07                        *MESE SETTE
544200030925     C                   Z-ADD     $VA(8)        SE93R08                        *MESE OTTO
544300030925     C                   Z-ADD     $VA(9)        SE93R09                        *MESE NOVE
544400030925     C                   Z-ADD     $VA(10)       SE93R10                        *MESE DIECI
544500030925     C                   Z-ADD     $VA(11)       SE93R11                        *MESE UNDICI
544600030925     C                   Z-ADD     $VA(12)       SE93R12                        *MESE DODICI
544700030925     C                   Z-ADD     $VA13C        SE93RTO                        *TOTALE MESI
544800030925E   1C                   ENDIF
544900000000     C*--------------------
545000000000     C* D E L T A
545100000000     C*--------------------
545200000000     C                   MOVEL     *BLANKS       DES12                          *PRIMA DESCRIZIONE
545300000000     C                   MOVEL     DRI(16)       DES07                          *SECONDA DESCRIZIONE
545400020221     C                   MOVEL     *BLANKS       P1DFLD
545500000000     C*
545600000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
545700000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
545800000000     C                   Z-ADD     *ZEROS        $PD13C                         *13^PERCENTUALE DELT
545900000000     C                   EXSR      IMPCAL
546000970822     C***
546100970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
546200970822     C***
546300970822     C                   MOVEL     *BLANKS       $SD                            *FALG SEGNALAZIONE
546400970822     C*
546500000000     C                   MOVEL     *BLANKS       P2DDES
546600020221     C                   MOVEL     *BLANKS       P2DFLD
546700000000     C                   MOVEL     DES12         P2DDES
546800000000     C                   MOVE      DES07         P2DDES
546900000000     C*
547000000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
547100000000     C                   EXSR      SCRDEL
547200030925     C*
547300030925     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
547400030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
547500030925     C                   Z-ADD     $PD(13)       SE93DPR                        *VALORE PRECEDENTE
547600030925     C                   Z-ADD     $PD(1)        SE93D01                        *MESE UNO
547700030925     C                   Z-ADD     $PD(2)        SE93D02                        *MESE DUE
547800030925     C                   Z-ADD     $PD(3)        SE93D03                        *MESE TRE
547900030925     C                   Z-ADD     $PD(4)        SE93D04                        *MESE QUATTRO
548000030925     C                   Z-ADD     $PD(5)        SE93D05                        *MESE CINQUE
548100030925     C                   Z-ADD     $PD(6)        SE93D06                        *MESE SEI
548200030925     C                   Z-ADD     $PD(7)        SE93D07                        *MESE SETTE
548300030925     C                   Z-ADD     $PD(8)        SE93D08                        *MESE OTTO
548400030925     C                   Z-ADD     $PD(9)        SE93D09                        *MESE NOVE
548500030925     C                   Z-ADD     $PD(10)       SE93D10                        *MESE DIECI
548600030925     C                   Z-ADD     $PD(11)       SE93D11                        *MESE UNDICI
548700030925     C                   Z-ADD     $PD(12)       SE93D12                        *MESE DODICI
548800030925     C                   Z-ADD     $PD13C        SE93DTO                        *TOTALE MESI
548900030925     C*
549000030925     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
549100030925IF  2C     PARWCP        IFEQ      'S'
549200030925     c                   ADD       1             WNrr
549300030925     c                   Z-ADD     WNrr          SE93NRR                        *righe in stampa
549400030925     C                   WRITE     WFTISE930
549500030925E   2C                   ENDIF
549600030925E   1C                   ENDIF
549700000000     C*
549800000000     C                   ENDSR
549900020701     C*------------------------------------------------------------------------*
550000020701     C*?REPDELFAT - REPERISCE LE % SCOSTAMENTO FATTURATO TRA IL MESE/ANNO DI ELAB. RISPETTO ALL'ANNO
550100020701     C*------------------------------------------------------------------------*
550200020701     C     REPDELFAT     BEGSR
550300020701     C*
550400020712     C* INIZIALIZZO LA SCHIERA E LE VARIABILI DI WRK
550500020712     C                   CLEAR                   $PDF                           *PERCENTUALI SCOST.
550600020712     C                   Z-ADD     1             W_MM              2 0
550700020712     C                   Z-ADD     *ZEROS        W_PROG           18 3
550800020712     C                   Z-ADD     *ZEROS        W_PROGP          18 3
550900020712     C                   Z-ADD     *ZEROS        W_DELFATT        30 9
551000020712     C*
551100020712     C* SOLO X CLIENTI DI CATEGORIA IMPORTANZA DIVERSA DA 'B' E 'C'
551200160912      **    adesso anche da "E" (sottocategoria di "C")
551300020712     C                   IF        DESCLV <> 'C' AND
551400020712     C                             DESCLV <> 'B'
551500160912     C                               and DESCLV <> 'E'
551600020712     C*
551700020712     C* CICLO X I 12 MESI DELLE SCHIERE DEI RICAVI REALI A.C. E A.P.
551800020712     C                   DOW       W_MM <= 12
551900020712     C*
552000020712     C* SOMMO I VALORI DEI MESI A.C. E A.P. X CALCOLARE I RELATIVI PROGRESSIVI A.C. E A.P.
552100020712     C                   ADD       R1K(W_MM)     W_PROG
552200020712     C                   ADD       R1KP(W_MM)    W_PROGP
552300020712     C*
552400020712     C* SOLO ALMENO 1 DEI 2 FATTURATI A.C. E A.P. SONO > 0 CALCOLO LA % SCOSTAMENTO FATTURATO A.C. S
552500020712     C                   IF        R1K(W_MM)  <> *zeros AND
552600020712     C                             R1KP(W_MM) <> *zeros
552700020712     C*
552800020712     C* INIZIALIZZO I LVALORE DELLA % SCOSTAMENTO
552900020712     C                   Z-ADD     *zeros        W_DELFATT
553000020712     C*
553100020712     C* QUINDI LA CALCOLO
553200020712     C                   EVAL      W_DELFATT  = (R1K(W_MM) - R1KP(W_MM)) * 100 /
553300020712     C                                           %abs(R1KP(W_MM))
553400020712     C*
553500020712     C* SE LA % OTTENUTA IN VALORE ASSOLUTO E > 999,9 => FORZO VALORE A 999,9
553600020712     C                   IF        W_DELFATT <= -1000
553700020712     C                   EVAL      W_DELFATT  = -999,9
553800020712     C                   ENDIF
553900020712     C                   IF        W_DELFATT >= +1000
554000020712     C                   EVAL      W_DELFATT  = +999,9
554100020712     C                   ENDIF
554200020712     C*
554300020712     C* SE RIENTRA NEL RANGE RICHIESTO NEL LANCIO VALORIZZO LA SCHIERA DA PORTARE IN STAMPA
554400020712     C                   IF        W_DELFATT  >= PARDFD AND
554500020712     C                             W_DELFATT  <= PARDFA
554600020712     C                   EVAL(H)   $PDF(W_MM)  = W_DELFATT
554700020712     C                   ENDIF
554800020712     C                   ENDIF
554900020712     C*
555000020712     C* INCREMENTO IL MESE
555100020712     C                   ADD       1             W_MM
555200020712     C                   ENDDO
555300020712     C*
555400020712     C* VALORIZZO I CAMPI IN STAMPA RELATIVI AI MESI
555500020712     C                   Z-ADD     $PDF(1)       P2CDE1P                        *MESE UNO
555600020712     C                   Z-ADD     $PDF(2)       P2CDE2P                        *MESE DUE
555700020712     C                   Z-ADD     $PDF(3)       P2CDE3P                        *MESE TRE
555800020712     C                   Z-ADD     $PDF(4)       P2CDE4P                        *MESE QUATTRO
555900020712     C                   Z-ADD     $PDF(5)       P2CDE5P                        *MESE CINQUE
556000020712     C                   Z-ADD     $PDF(6)       P2CDE6P                        *MESE SEI
556100020712     C                   Z-ADD     $PDF(7)       P2CDE7P                        *MESE SETTE
556200020712     C                   Z-ADD     $PDF(8)       P2CDE8P                        *MESE OTTO
556300020712     C                   Z-ADD     $PDF(9)       P2CDE9P                        *MESE NOVE
556400020712     C                   Z-ADD     $PDF(10)      P2CDE0P                        *MESE DIECI
556500020712     C                   Z-ADD     $PDF(11)      P2CDEAP                        *MESE UNDICI
556600020712     C                   Z-ADD     $PDF(12)      P2CDEBP                        *MESE DODICI
556700020712     C*
556800020712     C* DOPO LE VALORIZZAZOINI MENSILI EFFETTUO QUELLE DEL PROGRESSIVO
556900020712     C*
557000020712     C* SOLO SE ENTRAMI I FATTURATI A.C. E A.P. SONO > 0 CALCOLO LA % SCOSTAMENTO FATTURATO A.C. SU
557100020712     C                   IF        W_PROG  <> *zeros AND
557200020712     C                             W_PROGP <> *zeros
557300020712     C*
557400020712     C* INIZIALIZZO I LVALORE DELLA % SCOSTAMENTO
557500020712     C                   Z-ADD     *zeros        W_DELFATT
557600020712     C*
557700020712     C* QUINDI LA CALCOLO
557800020712     C                   EVAL      W_DELFATT  = (W_PROG - W_PROGP) * 100 /
557900020712     C                                           %abs(W_PROGP)
558000020712     C*
558100020712     C* SE LA % OTTENUTA IN VALORE ASSOLUTO E > 999,9 => FORZO VALORE A 999,9
558200020712     C                   IF        W_DELFATT <= -1000
558300020712     C                   EVAL      W_DELFATT  = -999,9
558400020712     C                   ENDIF
558500020712     C                   IF        W_DELFATT >= +1000
558600020712     C                   EVAL      W_DELFATT  = +999,9
558700020712     C                   ENDIF
558800020712     C*
558900020712     C* INIZIALIZZO SEMPRE IL CAMPO IN STAMPA RIFERITO ALLA % PROGRESSIVO
559000020712     C                   CLEAR                   P2CDETP
559100020712     C*
559200020712     C* SE RIENTRA NEL RANGE RICHIESTO NEL LANCIO VALORIZZO LA SCHIERA DA PORTARE IN STAMPA
559300020712     C                   IF        W_DELFATT  >= PARDFDP AND
559400020712     C                             W_DELFATT  <= PARDFAP
559500020712     C                   EVAL(H)   P2CDETP     = W_DELFATT
559600020712     C                   ENDIF
559700020712     C                   ENDIF
559800020712     C*
559900020712     C                   ENDIF
560000020701     C*
560100020701     C                   ENDSR
560200020701     C*------------------------------------------------------------------------*
560300020701     C*?INZPDF - INIZILIZZA I CAMPI LA SCHIERA DI WRK X LO SCOSTAMENTO FATTURATO E I RALATIVI CAMPI
560400020701     C*------------------------------------------------------------------------*
560500020701     C     INZPDF        BEGSR
560600020701     C*
560700020701     C                   CLEAR                   $PDF
560800020701     C*
560900020701     C                   Z-ADD     $PDF(1)       P2CDE1P                        *MESE UNO
561000020701     C                   Z-ADD     $PDF(2)       P2CDE2P                        *MESE DUE
561100020701     C                   Z-ADD     $PDF(3)       P2CDE3P                        *MESE TRE
561200020701     C                   Z-ADD     $PDF(4)       P2CDE4P                        *MESE QUATTRO
561300020701     C                   Z-ADD     $PDF(5)       P2CDE5P                        *MESE CINQUE
561400020701     C                   Z-ADD     $PDF(6)       P2CDE6P                        *MESE SEI
561500020701     C                   Z-ADD     $PDF(7)       P2CDE7P                        *MESE SETTE
561600020701     C                   Z-ADD     $PDF(8)       P2CDE8P                        *MESE OTTO
561700020701     C                   Z-ADD     $PDF(9)       P2CDE9P                        *MESE NOVE
561800020701     C                   Z-ADD     $PDF(10)      P2CDE0P                        *MESE DIECI
561900020701     C                   Z-ADD     $PDF(11)      P2CDEAP                        *MESE UNDICI
562000020701     C                   Z-ADD     $PDF(12)      P2CDEBP                        *MESE DODICI
562100020712     C                   Z-ADD     *ZEROS        P2CDETP                        *PROGRESSIVO
562200020701     C*
562300020701     C                   ENDSR
562400000000     C*------------------------------------------------------------------------*
562500000000     C*?STAMP2 - STAMPA I TOTALI NELLA FORMA: CLI / SPE / RIC / DEL
562600000000     C*------------------------------------------------------------------------*
562700000000     C     STAMP2        BEGSR
562800000000     C*--------------------
562900000000     C* I N T E S T A Z I O N E
563000000000     C*--------------------
563100000000IF  1C     *IN01         IFEQ      *ON
563200020215     C                   EXSR      STAINT
563300000000E   1C                   ENDIF
563400000000     C*--------------------
563500020222     C* C L I E N T I
563600000000     C*--------------------
563700000000     C                   MOVEL     DRI(13)       DES03             3
563800000000     C                   MOVEL     *BLANKS       P1DDES
563900000000     C                   MOVEL     DES20         P1DDES
564000000000     C                   MOVE      DES03         P1DDES
564100020222     C*
564200020222     C* SE STO STAMPANDO DATI DI CLIENTI 'D' => VALORIZZO IL CAMPO IN STAMPA X EVIDENZIARLI
564300020222     C                   IF        DEPSTPD = '0'
564400020222     C                   EVAL      P1DFLD = *BLANKS
564500020222     C                   ENDIF
564600020222     C                   IF        DEPSTPD = '1'
564700020222     C                   EVAL      P1DFLD = '*'
564800020222     C                   ENDIF
564900020222     C                   IF        DEPSTPD = '2'
565000020222     C                   EVAL      P1DFLD = '**'
565100020222     C                   ENDIF
565200030311     C                   IF        DEPSTPD = '3'
565300030520     C                   EVAL      P1DFLD = '§'
565400030311     C                   ENDIF
565500000000     C*
565600000000     C                   Z-ADD     CLW           $VA
565700020116     C                   Z-ADD     CTW(1)        $VA13C                         *PERIODO CORRENTE
565800020116     C                   Z-ADD     CTW(2)        $V213C                         *PERIODO PRECEDENTE
565900000000     C*
566000000000     C* SCRIVE I VALORI IN STAMPA
566100000000     C                   EXSR      SCRVAL
566200030925     C*
566300030925     C* IMPOSTA I CAMPI "CLIENTI" DEL WFILE DI STAMPA
566400030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
566500030925     C                   MOVEL     PARIOE        SE93IOE                        *DELTA ITALIA/ESTERO
566600030925     C                   MOVEL     INCDIV        SE93DIV                        *DIVISIONE
566700030925     C                   MOVEL     INDDIV        SE93DDI                        *DESCRIZIONE DIVISIO
566800030925     C                   MOVEL     INCARE        SE93ARE                        *AREA
566900030925     C                   MOVEL     INDARE        SE93DAR                        *DESCRIZIONE AREA
567000030925     C                   MOVEL     INCFIL        SE93FIL                        *FILIALE
567100030925     C                   MOVEL     INDFIL        SE93DFI                        *DESCRIZIONE FILIALE
567200030926IF  2c     WTipStamp2    IFEQ      'F'                                          *da STAFIL
567300030926     C                   MOVEL     DEPFIL        SE93FIL                        *FILIALE
567400030926     C                   MOVEL     DESFIL        SE93DFI                        *DESCRIZIONE FILIALE
567500030926E   2c                   ENDIF
567600030925     C                   MOVEL     P1DFLD        SE93AST                        *CARATTERE STRANO
567700030925     C                   MOVEL     P1DDES        SE93DES                        *DESCRIZIONE
567800030925     C                   MOVEL     *BLANKS       SE93CLV                        *QUALITA' CLIENTE
567900030925     C                   MOVEL     '1'           SE93TIP                        *TIPO TOTALE
568000030925     C                   Z-ADD     $V213C        SE93CPR                        *VALORE PRECEDENTE
568100030925     C                   Z-ADD     $VA(1)        SE93C01                        *MESE UNO
568200030925     C                   Z-ADD     $VA(2)        SE93C02                        *MESE DUE
568300030925     C                   Z-ADD     $VA(3)        SE93C03                        *MESE TRE
568400030925     C                   Z-ADD     $VA(4)        SE93C04                        *MESE QUATTRO
568500030925     C                   Z-ADD     $VA(5)        SE93C05                        *MESE CINQUE
568600030925     C                   Z-ADD     $VA(6)        SE93C06                        *MESE SEI
568700030925     C                   Z-ADD     $VA(7)        SE93C07                        *MESE SETTE
568800030925     C                   Z-ADD     $VA(8)        SE93C08                        *MESE OTTO
568900030925     C                   Z-ADD     $VA(9)        SE93C09                        *MESE NOVE
569000030925     C                   Z-ADD     $VA(10)       SE93C10                        *MESE DIECI
569100030925     C                   Z-ADD     $VA(11)       SE93C11                        *MESE UNDICI
569200030925     C                   Z-ADD     $VA(12)       SE93C12                        *MESE DODICI
569300030925     C                   Z-ADD     $VA13C        SE93CTO                        *TOTALE MESI
569400030925E   1C                   ENDIF
569500000000     C*--------------------
569600000000     C* S P E D I Z I O N I
569700000000     C*--------------------
569800000000     C                   MOVEL     DRI(14)       DES03             3
569900000000     C                   MOVEL     *BLANKS       P1DDES
570000000000     C                   MOVE      DES03         P1DDES
570100020221     C                   MOVEL     *BLANKS       P1DFLD
570200000000     C*
570300000000     C                   Z-ADD     SPW           $VA
570400020116     C                   Z-ADD     *ZEROS        $VA13C                         *PERIODO CORRENTE
570500000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
570600000000     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA SPED
570700000000E   1C                   ENDDO
570800020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
570900951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
571000000000     C*
571100000000     C* SCRIVE I VALORI IN STAMPA
571200000000     C                   EXSR      SCRVAL
571300030925     C*
571400030925     C* IMPOSTA I CAMPI "SPEDIZIONI" DEL WFILE DI STAMPA
571500030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
571600030925     C                   Z-ADD     $V213C        SE93NPR                        *VALORE PRECEDENTE
571700030925     C                   Z-ADD     $VA(1)        SE93N01                        *MESE UNO
571800030925     C                   Z-ADD     $VA(2)        SE93N02                        *MESE DUE
571900030925     C                   Z-ADD     $VA(3)        SE93N03                        *MESE TRE
572000030925     C                   Z-ADD     $VA(4)        SE93N04                        *MESE QUATTRO
572100030925     C                   Z-ADD     $VA(5)        SE93N05                        *MESE CINQUE
572200030925     C                   Z-ADD     $VA(6)        SE93N06                        *MESE SEI
572300030925     C                   Z-ADD     $VA(7)        SE93N07                        *MESE SETTE
572400030925     C                   Z-ADD     $VA(8)        SE93N08                        *MESE OTTO
572500030925     C                   Z-ADD     $VA(9)        SE93N09                        *MESE NOVE
572600030925     C                   Z-ADD     $VA(10)       SE93N10                        *MESE DIECI
572700030925     C                   Z-ADD     $VA(11)       SE93N11                        *MESE UNDICI
572800030925     C                   Z-ADD     $VA(12)       SE93N12                        *MESE DODICI
572900030925     C                   Z-ADD     $VA13C        SE93NTO                        *TOTALE MESI
573000030925E   1C                   ENDIF
573100000000     C*--------------------
573200000000     C* R I C A V I
573300000000     C*--------------------
573400000000     C                   MOVEL     DRI(15)       DES03             3
573500000000     C                   MOVEL     *BLANKS       P1DDES
573600000000     C                   MOVE      DES03         P1DDES
573700020221     C                   MOVEL     *BLANKS       P1DFLD
573800000000     C*
573900020116     C* === CON L'EURO NN SI "MIGLIALIZZA" +
574000020116     C*    RRW           DIV       1000          $VA
574100020116     C                   Z-ADD     RRW           $VA
574200020116     C                   Z-ADD     *ZEROS        $VA13C
574300000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
574400020116     C                   ADD       $VA(J)        $VA13C                         *13^ COLONNA RICAVI
574500000000E   1C                   ENDDO
574600020116     C                   Z-ADD     *ZEROS        $V213C                         *PERIODO PRECEDENTE
574700951227     C                   Z-ADD     $VA(13)       $V213C                         *1^ COLONNA SPED
574800000000     C*
574900000000     C* SCRIVE I VALORI IN STAMPA
575000000000     C                   EXSR      SCRVAL
575100030925     C*
575200030925     C* IMPOSTA I CAMPI "RICAVI" DEL WFILE DI STAMPA
575300030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
575400030925     C                   Z-ADD     $V213C        SE93RPR                        *VALORE PRECEDENTE
575500030925     C                   Z-ADD     $VA(1)        SE93R01                        *MESE UNO
575600030925     C                   Z-ADD     $VA(2)        SE93R02                        *MESE DUE
575700030925     C                   Z-ADD     $VA(3)        SE93R03                        *MESE TRE
575800030925     C                   Z-ADD     $VA(4)        SE93R04                        *MESE QUATTRO
575900030925     C                   Z-ADD     $VA(5)        SE93R05                        *MESE CINQUE
576000030925     C                   Z-ADD     $VA(6)        SE93R06                        *MESE SEI
576100030925     C                   Z-ADD     $VA(7)        SE93R07                        *MESE SETTE
576200030925     C                   Z-ADD     $VA(8)        SE93R08                        *MESE OTTO
576300030925     C                   Z-ADD     $VA(9)        SE93R09                        *MESE NOVE
576400030925     C                   Z-ADD     $VA(10)       SE93R10                        *MESE DIECI
576500030925     C                   Z-ADD     $VA(11)       SE93R11                        *MESE UNDICI
576600030925     C                   Z-ADD     $VA(12)       SE93R12                        *MESE DODICI
576700030925     C                   Z-ADD     $VA13C        SE93RTO                        *TOTALE MESI
576800030925E   1C                   ENDIF
576900000000     C*--------------------
577000000000     C* D E L T A
577100000000     C*--------------------
577200000000     C                   MOVEL     DRI(16)       DES03             3
577300000000     C                   MOVEL     *BLANKS       P2DDES
577400020221     C                   MOVEL     *BLANKS       P2DFLD
577500000000     C                   MOVE      DES03         P2DDES
577600000000     C*
577700000000     C* CALCOLA IL DELTA PER TUTTI I VALORI
577800000000     C                   Z-ADD     RRW           $RR                            *RICAVI REALI
577900000000     C                   Z-ADD     RPW           $RP                            *RICAVI PRESUNTI
578000000000     C                   Z-ADD     *ZEROS        $PD                            *PERCENTUALI DELTA
578100000000     C                   Z-ADD     *ZEROS        $PD13C            4 1          *13^PERCENTUALE DELT
578200000000     C                   EXSR      IMPCAL
578300970822     C***
578400970822     C* FLAG SEGNALAZIONE % SCOSTATAMENTO DELTA
578500970822     C***
578600970822     C                   MOVEL     *BLANKS       $SD                            *FALG SEGNALAZIONE
578700000000     C*
578800000000     C* SCRIVE I VALORI PERCENTUALI IN STAMPA
578900000000     C                   EXSR      SCRDEL
579000030925     C*
579100030925     C* IMPOSTA I CAMPI "DELTA" DEL WFILE DI STAMPA
579200030925IF  1C     PARSTA        IFEQ      'F'                                          *DETTAGLIO FILIALE
579300030925     C                   Z-ADD     $PD(13)       SE93DPR                        *VALORE PRECEDENTE
579400030925     C                   Z-ADD     $PD(1)        SE93D01                        *MESE UNO
579500030925     C                   Z-ADD     $PD(2)        SE93D02                        *MESE DUE
579600030925     C                   Z-ADD     $PD(3)        SE93D03                        *MESE TRE
579700030925     C                   Z-ADD     $PD(4)        SE93D04                        *MESE QUATTRO
579800030925     C                   Z-ADD     $PD(5)        SE93D05                        *MESE CINQUE
579900030925     C                   Z-ADD     $PD(6)        SE93D06                        *MESE SEI
580000030925     C                   Z-ADD     $PD(7)        SE93D07                        *MESE SETTE
580100030925     C                   Z-ADD     $PD(8)        SE93D08                        *MESE OTTO
580200030925     C                   Z-ADD     $PD(9)        SE93D09                        *MESE NOVE
580300030925     C                   Z-ADD     $PD(10)       SE93D10                        *MESE DIECI
580400030925     C                   Z-ADD     $PD(11)       SE93D11                        *MESE UNDICI
580500030925     C                   Z-ADD     $PD(12)       SE93D12                        *MESE DODICI
580600030925     C                   Z-ADD     $PD13C        SE93DTO                        *TOTALE MESI
580700030925     C*
580800030925     C* SCRIVE UN RECORD NEL WFILE, SE RICHIESTO
580900030925IF  2C     PARWCP        IFEQ      'S'
581000030925     c                   ADD       1             WNrr
581100030925     c                   Z-ADD     WNrr          SE93NRR                        *righe in stampa
581200030925     C                   WRITE     WFTISE930
581300030925E   2C                   ENDIF
581400030925E   1C                   ENDIF
581500000000     C*
581600000000     C                   ENDSR
581700000000     C*------------------------------------------------------------------------*
581800000000     C*?IMPCAL - IMPOSTA IL CALCOLO DEL DESTA PER TUTTI I VALORI
581900000000     C*          INPUT  --> $RR    : SCHIERA RICAVO REALE
582000000000     C*                     $RP    : SCHIERA RICAVO PRESUNTO
582100000000     C*          OUTPUT --> $PD    : SCHIERA DELTA PERCENTUALE
582200000000     C*                     $PD13C : 13^ PERCENTUALE DELTA
582300000000     C*------------------------------------------------------------------------*
582400000000     C     IMPCAL        BEGSR
582500000000     C*--------------------
582600000000     C* DELTA PERIODO PRECEDENTE
582700000000     C*--------------------
582800020116     C                   Z-ADD     $RR(13)       WRRW             19 5          *RICAVO REALE
582900020116     C                   Z-ADD     $RP(13)       WRPW             19 5          *RICAVO PRESUNTO
583000000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
583100000000     C                   EXSR      CALDEL
583200000000     C                   Z-ADD     WDPW          $PD(13)                        *SCHIERA DELTA
583300000000     C*--------------------
583400000000     C* DELTA 12 MESI
583500000000     C*--------------------
583600000000DO  1C     1             DO        SPARM         J
583700020116     C                   Z-ADD     $RR(J)        WRRW                           *RICAVO REALE
583800020116     C                   Z-ADD     $RP(J)        WRPW                           *RICAVO PRESUNTO
583900020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
584000000000     C                   EXSR      CALDEL
584100000000     C                   Z-ADD     WDPW          $PD(J)                         *SCHIERA DELTA
584200000000E   1C                   ENDDO
584300000000     C*--------------------
584400000000     C* DELTA 13^ COLONNA
584500000000     C*--------------------
584600000000     C                   Z-ADD     *ZEROS        WRRW
584700000000     C                   Z-ADD     *ZEROS        WRPW
584800000000DO  1C     1             DO        SPARM         J                              *SCARTO 13° ELEMENTO
584900020116     C                   ADD       $RR(J)        WRRW                           *RICAVO REALE
585000020116     C                   ADD       $RP(J)        WRPW                           *RICAVO PRESUNTO
585100000000E   1C                   ENDDO
585200020116     C                   Z-ADD     *ZEROS        WDPW                           *DELTA PERCENTUALE
585300000000     C                   EXSR      CALDEL
585400000000     C                   Z-ADD     WDPW          $PD13C                         *13^ COLONNA
585500000000     C*
585600000000     C                   ENDSR
585700000000     C*------------------------------------------------------------------------*
585800000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
585900000000     C*          INPUT  --> WRRW: RICAVO REALE
586000000000     C*                     WRPW: RICAVO PRESUNTO
586100000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
586200000000     C*------------------------------------------------------------------------*
586300000000     C     CALDEL        BEGSR
586400000000     C*--------------------
586500000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
586600000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
586700000000     C*--------------------
586800000000     C*
586900000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
587000020116     C     WRRW          SUB       WRPW          GAP              19 5          (REALE - PRESUNTO)
587100000000     C     GAP           MULT      100           GAP                            (GAP x 100)
587200000000     C*
587300000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
587400000000IF  1C     WRRW          IFEQ      *ZEROS
587500000000     C                   Z-ADD     1             WRRW
587600000000E   1C                   ENDIF
587700000000     C*
587800000000     C     GAP           DIV       WRRW          GAP
587900000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
588000000000     C*
588100000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
588200000000     C     WDPW          COMP      0                                    28
588300000000     C* POSITIVO
588400000000IF  1C     *IN28         IFEQ      *OFF
588500000000IF  2C     GAP           IFGE      1000
588600000000     C                   Z-ADD     +999,9        WDPW
588700000000E   2C                   ENDIF
588800000000     C* NEGATIVO
588900000000X   1C                   ELSE
589000000000IF  2C     GAP           IFLE      -1000
589100000000     C                   Z-ADD     -999,9        WDPW
589200000000E   2C                   ENDIF
589300000000E   1C                   ENDIF
589400000000     C*
589500000000     C                   ENDSR
589600000000     C*------------------------------------------------------------------------*
589700000000     C*?SCRVAL - SCRIVE I VALORI IN STAMPA
589800000000     C*------------------------------------------------------------------------*
589900000000     C     SCRVAL        BEGSR
590000000000     C*
590100951227     C                   Z-ADD     $V213C        P1CVAP                         *VALORE PRECEDENTE
590200000000     C                   Z-ADD     $VA(1)        P1CVA1                         *MESE UNO
590300000000     C                   Z-ADD     $VA(2)        P1CVA2                         *MESE DUE
590400000000     C                   Z-ADD     $VA(3)        P1CVA3                         *MESE TRE
590500000000     C                   Z-ADD     $VA(4)        P1CVA4                         *MESE QUATTRO
590600000000     C                   Z-ADD     $VA(5)        P1CVA5                         *MESE CINQUE
590700000000     C                   Z-ADD     $VA(6)        P1CVA6                         *MESE SEI
590800000000     C                   Z-ADD     $VA(7)        P1CVA7                         *MESE SETTE
590900000000     C                   Z-ADD     $VA(8)        P1CVA8                         *MESE OTTO
591000000000     C                   Z-ADD     $VA(9)        P1CVA9                         *MESE NOVE
591100000000     C                   Z-ADD     $VA(10)       P1CVA0                         *MESE DIECI
591200000000     C                   Z-ADD     $VA(11)       P1CVAA                         *MESE UNDICI
591300000000     C                   Z-ADD     $VA(12)       P1CVAB                         *MESE DODICI
591400000000     C                   Z-ADD     $VA13C        P1CVAT                         *TOTALE MESI
591500000000     C*
591600100129     c                   if        parSTAMPA <> 'N'
591700020215     C  N53              WRITE     SE94P1                                       *STAMPA RIGA VALORE
591800100129     c                   end
591900011123     C   53              SETOFF                                       53
592000000000     C*
592100000000     C                   ENDSR
592200000000     C*------------------------------------------------------------------------*
592300000000     C*?SCRDEL - SCRIVE I VALORI DELTA IN STAMPA
592400000000     C*------------------------------------------------------------------------*
592500000000     C     SCRDEL        BEGSR
592600000000     C*
592700000000     C                   Z-ADD     $PD(13)       P2CDEP                         *VALORE PRECEDENTE
592800000000     C                   Z-ADD     $PD(1)        P2CDE1                         *MESE UNO
592900970822     C                   MOVEL     $SD(1)        P2CSP1
593000000000     C                   Z-ADD     $PD(2)        P2CDE2                         *MESE DUE
593100970822     C                   MOVEL     $SD(2)        P2CSP2
593200000000     C                   Z-ADD     $PD(3)        P2CDE3                         *MESE TRE
593300970822     C                   MOVEL     $SD(3)        P2CSP3
593400000000     C                   Z-ADD     $PD(4)        P2CDE4                         *MESE QUATTRO
593500970822     C                   MOVEL     $SD(4)        P2CSP4
593600000000     C                   Z-ADD     $PD(5)        P2CDE5                         *MESE CINQUE
593700970822     C                   MOVEL     $SD(5)        P2CSP5
593800000000     C                   Z-ADD     $PD(6)        P2CDE6                         *MESE SEI
593900970822     C                   MOVEL     $SD(6)        P2CSP6
594000000000     C                   Z-ADD     $PD(7)        P2CDE7                         *MESE SETTE
594100970822     C                   MOVEL     $SD(7)        P2CSP7
594200000000     C                   Z-ADD     $PD(8)        P2CDE8                         *MESE OTTO
594300970822     C                   MOVEL     $SD(8)        P2CSP8
594400000000     C                   Z-ADD     $PD(9)        P2CDE9                         *MESE NOVE
594500970822     C                   MOVEL     $SD(9)        P2CSP9
594600000000     C                   Z-ADD     $PD(10)       P2CDE0                         *MESE DIECI
594700970822     C                   MOVEL     $SD(10)       P2CSP0
594800000000     C                   Z-ADD     $PD(11)       P2CDEA                         *MESE UNDICI
594900970822     C                   MOVEL     $SD(11)       P2CSPA
595000000000     C                   Z-ADD     $PD(12)       P2CDEB                         *MESE DODICI
595100970822     C                   MOVEL     $SD(12)       P2CSPB
595200000000     C                   Z-ADD     $PD13C        P2CDET                         *TOTALE MESI
595300000000     C*
595400100129     c                   if        parSTAMPA <> 'N'
595500020215     C  N53              WRITE     SE94P2                                       *STAMPA RIGA VALORE
595600100129     c                   end
595700011123     C   53              SETOFF                                       53
595800000000     C*
595900000000     C                   ENDSR
596000000000     C*------------------------------------------------------------------------*
596100000000     C*?DECDIV - DECODIFICA CODICE DIVISIONE
596200000000     C*          INPUT  -- DEPDIV: CODICE DIVISIONE
596300000000     C*          OUTPUT -- DESDIV: DESCRIZIONE DIVISIONE
596400000000     C*------------------------------------------------------------------------*
596500000000     C     DECDIV        BEGSR
596600000000     C*
596700000000     C                   Z-ADD     1             D
596800000000     C     DEPDIV        LOOKUP    DIV(D)                                 99
596900000000IF  1C     *IN99         IFEQ      *OFF
597000000000     C                   MOVEL     *ALL'*'       DESDIV           20
597100000000X   1C                   ELSE
597200000000     C                   MOVEL     DDIV(D)       DESDIV
597300000000E   1C                   ENDIF
597400000000     C*
597500000000     C                   ENDSR
597600000000     C*------------------------------------------------------------------------*
597700000000     C*? DECARE - DECODIFICA CODICE AREA
597800000000     C*          INPUT  -- DEPARE: CODICE AREA
597900000000     C*          OUTPUT -- DESARE: DESCRIZIONE AREA
598000000000     C*------------------------------------------------------------------------*
598100000000     C     DECARE        BEGSR
598200000000     C*
598300000000     C                   Z-ADD     1             D
598400000000     C     DEPARE        LOOKUP    ARE(D)                                 99
598500000000IF  1C     *IN99         IFEQ      *OFF
598600000000     C                   MOVEL     *ALL'*'       DESARE           20
598700000000X   1C                   ELSE
598800000000     C                   MOVEL     DARE(D)       DESARE
598900000000E   1C                   ENDIF
599000000000     C*
599100000000     C                   ENDSR
599200000000     C*------------------------------------------------------------------------*
599300000000     C*? DECFIL - DECODIFICA CODICE FILIALE
599400000000     C*          INPUT  -- DEPFIL: CODICE FILIALE
599500000000     C*          OUTPUT -- DESFIL: DESCRIZIONE FILIALE
599600000000     C*------------------------------------------------------------------------*
599700000000     C     DECFIL        BEGSR
599800000000     C*
599900000000     C                   Z-ADD     1             D
600000000000     C     DEPFIL        LOOKUP    FIL(D)                                 99
600100000000IF  1C     *IN99         IFEQ      *OFF
600200000000     C                   MOVEL     *ALL'*'       DESFIL           20
600300000000X   1C                   ELSE
600400000000     C                   MOVEL     DFIL(D)       DESFIL
600500000000E   1C                   ENDIF
600600000000     C*
600700000000     C                   ENDSR
600800000000     C*------------------------------------------------------------------------*
600900000000     C*?DECAGE - DECODIFICA CODICE AGENTE
601000000000     C*          INPUT  -- DEPAGE: CODICE AGENTE COMMERCIALE
601100000000     C*          OUTPUT -- DESAGE: DESCRIZIONE AGENTE
601200000000     C*                    DE2PCF: PERCENTUALE COMPETENZA FILIALE
601300000000     C*------------------------------------------------------------------------*
601400000000     C     DECAGE        BEGSR
601500000000     C*
601600980407     C                   MOVEL     *BLANKS       DESAGE           25
601700000000     C                   Z-ADD     1             D
601800000000     C     DEPAGE        LOOKUP    AGE(D)                                 99
601900000000IF  1C     *IN99         IFEQ      *OFF
602000980407     C                   MOVEL     *ALL'*'       DESAGE
602100000000     C                   Z-ADD     *ZEROS        DESPCF
602200000000     C                   MOVEL     *BLANKS       DE2PCF
602300000000X   1C                   ELSE
602400000000     C                   MOVEL     DAGE(D)       DESAGE
602500000000     C                   Z-ADD     CAGE(D)       DESPCF
602600000000     C*
602700000000     C* METTO IL CAMPO PERCENTUALE IN UNO ALFANUMERICO E LO EDITO(Z)
602800000000     C                   MOVEL     DESPCF        DE2PCF            3
602900000000     C                   Z-ADD     1             P
603000000000     C                   MOVEA     DE2PCF        PCF
603100000000     C*
603200000000DO  2C                   DO        3             P
603300000000IF  3C     PCF(P)        IFEQ      *ZEROS
603400000000     C                   MOVEL     *BLANKS       PCF(P)
603500000000X   3C                   ELSE
603600000000     C                   Z-ADD     4             P
603700000000E   3C                   ENDIF
603800000000E   2C                   ENDDO
603900000000     C                   MOVEA     PCF           DE2PCF
604000000000E   1C                   ENDIF
604100000000     C*
604200000000     C                   ENDSR
604300000000     C*------------------------------------------------------------------------*
604400000000     C*?DECKSC - DECODIFICA CODICE CLIENTE
604500000825     C*          INPUT  -- DEPCLI: CODICE CLIENTE
604600000000     C*          OUTPUT -- DESKSC: RAGIONE SOCIALE CLIENTE
604700951006     C*                    DESITC: DESCRIZIONE CATEGORIA SEAT
604800000000     C*                    DESCLV: QUALITA' CLIENTE
604900000000     C*                    DESCLN: CLIENTE NUOVO O ACQUISITO
605000000000     C*------------------------------------------------------------------------*
605100000000     C     DECKSC        BEGSR
605200120131      *--
605300120131      *-- pulisce x nuovo cliente i campi da evidenziare in seguito
605400120131     c                   eval           Cod_Blocco_Ultimo   = *blank
605500120131     c                   eval           Data_Blocco_Ultimo  = 0
605600120131     c                   eval           Data_Prima_Fat_6    = 0
605700120131      *--
605800000000     C*--------------------
605900000000     C* RAGIONE SOCIALE
606000960708     C* BLOCCO CLIENTE
606100000000     C*--------------------
606200000000     C                   MOVEL     *ALL'*'       DESKSC                         *SE NON TROVATO
606300960708     C                   MOVEL     *BLANKS       DESABL                         *SE NON TROVATO
606400980406     C                   CLEAR                   BS69DS
606500980406     C                   CLEAR                   ACODS
606600980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
606700000825     C                   MOVEL     DEPCLI        I69KAC                         *CLIENTE X CNACO
606800070419     C                   MOVEL     DEPCLI        I69KIN                         *CLIENTE X CNIND
606900980406     C                   CALL      'TIBS69R'
607000980406     C                   PARM                    BS69DS
607100980406     C                   PARM                    ACODS
607200980406     C                   PARM                    INDDS
607300980406     C                   PARM                    CLPDS
607400980406     C                   PARM                    CLSDS
607500980406IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
607600980406     C                   MOVEL     ACORAG        DESKSC                         *RAGIONE SOCIALE
607700980406     C                   MOVEL     ACOABL        DESABL                         *BLOCCO CLIENTE
607800070419      * Nuovi dati anagrafici x i nuovi WADELPCF/E
607900070419     C                   MOVEL     INDvia        DESvia                         *Indirizzo
608000070419     C                   MOVEL     INDcap        DEScap                         *CAP
608100070419     C                   MOVEL     INDcae        DEScae                         *CAP Europeo
608200070419     C                   MOVEL     INDprv        DESprv                         *Provincia
608300070419     C                   MOVEL     INDcit        DEScit                         *Città
608400070419     C                   MOVEL     INDsta        DESsta                         *Nazione
608500070615     C                   MOVEL     INDcdf        DEScdf                         *Cod.Fiscale
608600070615     C                   MOVEL     INDiva        DESiva                         *Part.IVA
608700070618     C                   MOVEL     INDtel        DEStel                         *Telefono
608800980406E   1C                   ENDIF
608900000000     C*--------------------
609000951006     C* CATEGORIA SEAT
609100000000     C*--------------------
609200951006     C                   MOVEL     *BLANKS       DESITC           35            *SE NON TROVATO
609300020906     C                   Z-ADD     1             KTBKUT
609400951006     C                   MOVEL     '1L'          KTBCOD
609500951006     C                   MOVEL     *BLANKS       KTBKEY
609600951006     C                   MOVE      ACOITC        NUM5              5 0
609700951006     C                   MOVEL     NUM5          KTBKEY
609800951006     C     KEYTAB        CHAIN     TABEL00F                           99
609900951006IF  1C     *IN99         IFEQ      *OFF
610000951006     C                   MOVEL     TBLUNI        DS1L
610100951006     C                   MOVEL     §1LDES        DESITC                         *CATEGORIA SEAT
610200951006E   1C                   ENDIF
610300951006     C*--------------------
610400951006     C* QUALITA' CLIENTE
610500951006     C*--------------------
610600960710     C                   MOVEL     WFDCLV        DESCLV
610700000000     C                   SETOFF                                           28
610800010313     C     DESCLV        LOOKUP    ABC                                    28
610900960710IF  1C     *IN28         IFEQ      *OFF
611000000000     C                   MOVEL     'C'           DESCLV
611100960710E   1C                   ENDIF
611200960708     C*--------------------
611300120131     C* CAUSALE BLOCCO e data prima spedizione Fatturata
611400960708     C*--------------------
611500960710     C                   MOVEL     *BLANKS       DESCBL           30
611600980406     C                   CLEAR                   BS69DS
611700980406     C                   CLEAR                   CLPDS
611800980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
611900000825     C                   MOVEL     DEPCLI        I69KCP                         *CLIENTE X CNCLP
612000980406     C                   CALL      'TIBS69R'
612100980406     C                   PARM                    BS69DS
612200980406     C                   PARM                    ACODS
612300980406     C                   PARM                    INDDS
612400980406     C                   PARM                    CLPDS
612500980406     C                   PARM                    CLSDS
612600980406IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
612700120131     c                   eval           Data_Prima_Fat_6 = CLPDPS
612800130318IF  2C     DESABL        IFne      *blank                                       *CLIENTE BLOCCATO
612900130318IF  2C     DESABL        andne     '*'                                          *CLIENTE BLOCCATO
613000120131      **
613100120131      **  Reperisce la Data di quando è stato fatto il Blocco Contabile sul cliente
613200120131     C                   clear                   TrulBLCds
613300120131     c                   eval      IblcTLA = 'L'
613400120131     c                   z-add     clpKcC        IblcKCC
613500120131     c                   z-add     clpKsC        IblcKSC
613600120131     c                   call      'TRULBLCR'
613700120131     C                   PARM                    KPJBA
613800120131     C                   PARM                    TrulBLCds
613900120131     c                   eval           Data_Blocco_Ultimo  = OblcDAV
614000120131     c                   eval           Cod_Blocco_Ultimo   = clpNAR
614100120131      **
614200980406     C                   Z-ADD     1             C
614300980406     C                   SETOFF                                           28
614400980406     C     CLPNAR        LOOKUP    CBL(C)                                 28
614500980406IF  3C     *IN28         IFEQ      *ON
614600980406     C                   MOVEL     DCBL(C)       DESCBL
614700980406E   3C                   ENDIF
614800980406E   2C                   ENDIF
614900980406E   1C                   ENDIF
615000000000     C*--------------------
615100000000     C* CLIENTE NUOVO O ACQUISITO
615200000000     C*--------------------
615300000000     C* PER IL PERIODO PRECEDENTE ALLA STATISTICA SE UN CLIENTE HA DEI SALDI E'
615400000000     C* ACQUISITO, SE NON NE HA E' NUOVO: NON POSSO PRENDERE IL CAMPO WFDCLN PER
615500000000     C* PER L'ANNO PRECEDENTE PERCHE' IL CLIENTE POTEVA ESSERE NUOVO
615600960710     C                   MOVEL     *BLANKS       DESCLN                         *SE NON TROVATO
615700000000IF  1C     TIPANN        IFEQ      'P'                                          *PERIODO PRECEDENTE
615800000000     C                   MOVEL     *BLANKS       DESCLN                         *NON NUOVO
615900000000X   1C                   ELSE                                                   *PERIODO CORRENTE
616000070822      * forza il flag precedentemente testato nel caso particolare sopraddescritto
616100070822      * qualora si tratti di stampa o Italia o Estero.
616200070822     c                   if        desCLN <>'Y' and sav_desCLN_Y ='Y' and
616300070822     c                             wfdCLI =  sav_depCLI
616400070822     c                   movel     'Y'           wfdCLN
616500070822     c                   end
616600000000     C                   MOVEL     WFDCLN        DESCLN                         *NUOVO O ACQUISITO
616700000000E   1C                   ENDIF
616800000000     C*
616900000000     C                   ENDSR
617000000000     C*------------------------------------------------------------------------*
617100000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
617200000000     C*------------------------------------------------------------------------*
617300000000     C     CARMES        BEGSR
617400000000     C*
617500000000     C                   CLEAR                   ANN                            *ANNI
617600000000     C                   CLEAR                   MES                            *MESI
617700000000     C                   Z-ADD     *ZEROS        J                              *N° MESI DA ELABORAR
617800000000     C*
617900990609     C                   Z-ADD     PARAAF        WAA2              4 0          *ANNO FINALE LAVORO
618000000000     C                   Z-ADD     PARMMF        WMM2              2 0          *MESE FINALE LAVORO
618100990609     C                   Z-ADD     PARAAI        WAA               4 0          *ANNO INIZIALE LAVOR
618200000000     C                   Z-ADD     PARMMI        WMM               2 0          *MESE INIZIALE LAVOR
618300020701     C*
618400020701     C* SICCOME LA STATISTICA NON PO' + ESSERE LANCIATA A CAVALLO DI ANNI DIVERSI L'ANNO INIZIALE -1
618500020701     C* L'ANNO FINALE -1
618600020701     C     WAA           SUB       1             WAAP              4 0          *ANNO INIZIALE PRECD
618700020701     C
618800000000     C*
618900000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
619000000000IF  1C     WAA2          IFEQ      WAA
619100000000     C*
619200000000     C* CALCOLO I MESI DI DIFFERENZA
619300000000DO  2C     WMM           DOWLE     WMM2
619400000000     C                   ADD       1             J
619500000000     C                   Z-ADD     WAA           ANN(J)
619600000000     C                   Z-ADD     WMM           MES(J)
619700000000     C                   ADD       1             WMM
619800000000E   2C                   ENDDO
619900000000     C***
620000000000     C* SPARM = N° MESI SELEZIONATI
620100000000     C***
620200000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
620300000000     C*
620400000000     C* IL RESTO DELLA SCHIERA A ZERO
620500000000DO  2C     J             DOWLT     12
620600000000     C                   ADD       1             J
620700000000     C                   Z-ADD     *ZEROS        ANN(J)
620800000000     C                   Z-ADD     *ZEROS        MES(J)
620900000000E   2C                   ENDDO
621000000000     C*
621100000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
621200000000X   1C                   ELSE
621300000000     C*
621400000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
621500000000DO  2C     WMM           DOWLE     12
621600000000     C                   ADD       1             J
621700000000     C                   Z-ADD     WAA           ANN(J)
621800000000     C                   Z-ADD     WMM           MES(J)
621900000000     C                   ADD       1             WMM
622000000000E   2C                   ENDDO
622100000000     C*
622200000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
622300000000     C                   Z-ADD     01            WMM
622400000000DO  2C     WMM           DOWLE     WMM2
622500000000     C                   ADD       1             J
622600000000     C                   Z-ADD     WAA2          ANN(J)
622700000000     C                   Z-ADD     WMM           MES(J)
622800000000     C                   ADD       1             WMM
622900000000E   2C                   ENDDO
623000000000     C***
623100000000     C* SPARM = N° MESI SELEZIONATI
623200000000     C***
623300000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
623400000000     C*
623500000000     C* IL RESTO DELLA SCHIERA A ZERO
623600000000DO  2C     J             DOWLT     12
623700000000     C                   ADD       1             J
623800000000     C                   Z-ADD     *ZEROS        ANN(J)
623900000000     C                   Z-ADD     *ZEROS        MES(J)
624000000000E   2C                   ENDDO
624100000000E   1C                   ENDIF
624200000000     C*
624300000000     C                   ENDSR
624400000000     C*------------------------------------------------------------------------*
624500000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
624600000000     C*------------------------------------------------------------------------*
624700000000     C     DECTES        BEGSR
624800000000     C*--------------------
624900000000     C* DATA E ORA
625000000000     C*--------------------
625100000000     C                   Z-ADD     UDATE         DATA              6 0
625200000000     C                   TIME                    ORA               6 0
625300950602     C*--------------------
625400950602     C* DATI IN STATISTICA
625500950602     C*--------------------
625600950602IF  1C     PARIOE        IFEQ      'I'
625700950602     C                   MOVEL     DRI(5)        PTCIOE
625800950602E   1C                   ENDIF
625900950602IF  1C     PARIOE        IFEQ      'E'
626000950602     C                   MOVEL     DRI(6)        PTCIOE
626100950602E   1C                   ENDIF
626200950602IF  1C     PARIOE        IFEQ      *BLANKS
626300950602     C                   MOVEL     DRI(7)        PTCIOE
626400950602E   1C                   ENDIF
626500000829     C*--------------------
626600000829     C* DATI CLIENTE / UNIFICANTE
626700000829     C*--------------------
626800000829IF  1C     PARSCE        IFEQ      'C'
626900000829     C                   MOVEL     DRI(29)       PTDSCE
627000000829X   1C                   ELSE
627100000829     C                   MOVEL     DRI(30)       PTDSCE
627200000829E   1C                   ENDIF
627300000000     C*--------------------
627400000000     C* DECODIFICA MESI
627500000000     C*--------------------
627600000000IF  1C     MES(1)        IFGT      *ZEROS
627700000000     C                   MOVEL     MES(1)        IN                2 0          *NUMERO MESE MEMORIZ
627800000000     C                   MOVEL     DMM(IN)       PIDME1                         *DESC MESE IN STAMPA
627900990609     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
628000000000E   1C                   ENDIF
628100000000     C*
628200000000IF  1C     MES(2)        IFGT      *ZEROS
628300000000     C                   MOVEL     MES(2)        IN                             *NUMERO MESE MEMORIZ
628400000000     C                   MOVEL     DMM(IN)       PIDME2                         *DESC MESE IN STAMPA
628500990609     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
628600000000E   1C                   ENDIF
628700000000     C*
628800000000IF  1C     MES(3)        IFGT      *ZEROS
628900000000     C                   MOVEL     MES(3)        IN                             *NUMERO MESE MEMORIZ
629000000000     C                   MOVEL     DMM(IN)       PIDME3                         *DESC MESE IN STAMPA
629100990609     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
629200000000E   1C                   ENDIF
629300000000     C*
629400000000IF  1C     MES(4)        IFGT      *ZEROS
629500000000     C                   MOVEL     MES(4)        IN                             *NUMERO MESE MEMORIZ
629600000000     C                   MOVEL     DMM(IN)       PIDME4                         *DESC MESE IN STAMPA
629700990609     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
629800000000E   1C                   ENDIF
629900000000     C*
630000000000IF  1C     MES(5)        IFGT      *ZEROS
630100000000     C                   MOVEL     MES(5)        IN                             *NUMERO MESE MEMORIZ
630200000000     C                   MOVEL     DMM(IN)       PIDME5                         *DESC MESE IN STAMPA
630300990609     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
630400000000E   1C                   ENDIF
630500000000     C*
630600000000IF  1C     MES(6)        IFGT      *ZEROS
630700000000     C                   MOVEL     MES(6)        IN                             *NUMERO MESE MEMORIZ
630800000000     C                   MOVEL     DMM(IN)       PIDME6                         *DESC MESE IN STAMPA
630900990609     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
631000000000E   1C                   ENDIF
631100000000     C*
631200000000IF  1C     MES(7)        IFGT      *ZEROS
631300000000     C                   MOVEL     MES(7)        IN                             *NUMERO MESE MEMORIZ
631400000000     C                   MOVEL     DMM(IN)       PIDME7                         *DESC MESE IN STAMPA
631500990609     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
631600000000E   1C                   ENDIF
631700000000     C*
631800000000IF  1C     MES(8)        IFGT      *ZEROS
631900000000     C                   MOVEL     MES(8)        IN                             *NUMERO MESE MEMORIZ
632000000000     C                   MOVEL     DMM(IN)       PIDME8                         *DESC MESE IN STAMPA
632100990609     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
632200000000E   1C                   ENDIF
632300000000     C*
632400000000IF  1C     MES(9)        IFGT      *ZEROS
632500000000     C                   MOVEL     MES(9)        IN                             *NUMERO MESE MEMORIZ
632600000000     C                   MOVEL     DMM(IN)       PIDME9                         *DESC MESE IN STAMPA
632700990609     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
632800000000E   1C                   ENDIF
632900000000     C*
633000000000IF  1C     MES(10)       IFGT      *ZEROS
633100000000     C                   MOVEL     MES(10)       IN                             *NUMERO MESE MEMORIZ
633200000000     C                   MOVEL     DMM(IN)       PIDME0                         *DESC MESE IN STAMPA
633300990609     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
633400000000E   1C                   ENDIF
633500000000     C*
633600000000IF  1C     MES(11)       IFGT      *ZEROS
633700000000     C                   MOVEL     MES(11)       IN                             *NUMERO MESE MEMORIZ
633800000000     C                   MOVEL     DMM(IN)       PIDMEA                         *DESC MESE IN STAMPA
633900990609     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
634000000000E   1C                   ENDIF
634100000000     C*
634200000000IF  1C     MES(12)       IFGT      *ZEROS
634300000000     C                   MOVEL     MES(12)       IN                             *NUMERO MESE MEMORIZ
634400000000     C                   MOVEL     DMM(IN)       PIDMEB                         *DESC MESE IN STAMPA
634500990609     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
634600000000E   1C                   ENDIF
634700000000     C*
634800000000     C                   ENDSR
634900000000     C*------------------------------------------------------------------------*
635000000000     C*?CARTAB - CARICA I DATI IN TABELLE
635100000000     C*------------------------------------------------------------------------*
635200000000     C     CARTAB        BEGSR
635300000000     C*--------------------
635400000000     C* FILIALI/AREE/DIVISIONI
635500000000     C*--------------------
635600000000     C*
635700000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
635800000000     C                   Z-ADD     *ZEROS        KORFIL
635900000000     C     KEYORG        SETLL     AZORG01L
636000000000     C                   READ      AZORG01L                               99
636100000000     C*
636200000000     C* CICLO FINO A FINE FILE
636300000000     C                   Z-ADD     *ZEROS        F
636400951219DO  1C     *IN99         DOWEQ     *OFF
636500000000     C*
636600000000     C* ELIMINA RECORD ANNULLATI
636700000000IF  2C     ORGFVA        IFEQ      *BLANKS
636800000000     C                   ADD       1             F
636900000000     C* F I L I A L I
637000000000     C                   Z-ADD     ORGFIL        FIL(F)
637100000000     C                   MOVEL     ORGDES        DFIL(F)
637200000707     C                   MOVEL     ORGDE3        OG143
637300020627     C                   MOVEL     §OGNTW        POS(F)
637400000000     C* A R E E
637500020906     C                   Z-ADD     1             KTBKUT
637600000000     C                   MOVEL     '05'          KTBCOD
637700000000     C                   MOVEL     *BLANKS       KTBKEY
637800000000     C                   MOVEL     ORGCAR        KTBKEY
637900000000     C     KEYTAB        CHAIN     TABEL00F                           99
638000000000IF  3C     *IN99         IFEQ      *OFF
638100000000     C                   Z-ADD     ORGCAR        ARE(F)
638200000000     C                   MOVEL     TBLUNI        DARE(F)
638300000000E   3C                   ENDIF
638400000000     C* D I V I S I O N I
638500020906     C                   Z-ADD     1             KTBKUT
638600000000     C                   MOVEL     '17'          KTBCOD
638700000000     C                   MOVEL     *BLANKS       KTBKEY
638800000000     C                   MOVEL     ORGFL3        KTBKEY
638900000000     C     KEYTAB        CHAIN     TABEL00F                           99
639000000000IF  3C     *IN99         IFEQ      *OFF
639100000000     C                   MOVEL     ORGFL3        DIV(F)
639200000000     C                   MOVEL     TBLUNI        DDIV(F)
639300000000E   3C                   ENDIF
639400000000E   2C                   ENDIF
639500000000     C*
639600000000     C* LEGGE IL RECORD SUCCESSIVO
639700000000     C                   READ      AZORG01L                               99
639800000000E   1C                   ENDDO
639900000000     C*--------------------
640000000000     C* COMMERCIALI
640100000000     C*--------------------
640200140612     c                   open      azCMM01l
640300140612     C                   Z-ADD     *ZEROS        C
640400140612     C     *loval        SETLL     azCMM01l
640500140612     C                   read      azCMM01l
640600140612DO  1C                   dow       not %Eof(azCMM01l)
640700140612IF  2C                   IF           cmmATB = *blank
640800140612     C                   ADD       1             C
640900140612     C                   z-add     cmmCOD        AGE(C)
641000140612     C                   MOVEL     cmmDES        DAGE(C)
641100140612IF  3C     PARIOE        IFEQ      'I'                                          *% LAVORO ITALIA
641200140612     C                   Z-ADD     cmmPLI        CAGE(C)
641300140612X   3C                   ELSE                                                   *% LAVORO ESTERO
641400140612     C                   Z-ADD     cmmPLE        CAGE(C)
641500140612E   3C                   ENDIF
641600140612E   2C                   end
641700140612     C                   read      azCMM01l
641800140612E   1C                   ENDDO
641900140612     c                   close     azCMM01l
642000960708     C*--------------------
642100960708     C* CAUSALI BLOCCO CLIENTI
642200960708     C*--------------------
642300020906     C                   Z-ADD     1             KTBKUT
642400960708     C                   MOVEL     'BI'          KTBCOD
642500960708     C     KEYTA2        SETLL     TABEL00F
642600960708     C     KEYTA2        READE     TABEL00F                               99
642700960708     C                   Z-ADD     *ZEROS        C
642800960708DO  1C     *IN99         DOWEQ     *OFF
642900960708IF  2C     TBLFLG        IFEQ      *BLANKS
643000960708     C                   ADD       1             C
643100960708     C                   MOVEL     TBLKEY        CBL(C)
643200960708     C                   MOVEL     TBLUNI        DCBL(C)
643300960708E   2C                   ENDIF
643400960708     C     KEYTA2        READE     TABEL00F                               99
643500960708E   1C                   ENDDO
643600980521     C*---
643700980521     C* QUALITA' CLIENTE
643800980521     C*---
643900020906     C                   Z-ADD     1             KTBKUT
644000980521     C                   MOVEL     'IC'          KTBCOD
644100980521     C     KEYTA2        SETLL     TABEL00F
644200980521     C     KEYTA2        READE     TABEL00F                               99
644300980521DO  1C     *IN99         DOWEQ     *OFF
644400980521IF  2C     TBLFLG        IFEQ      *BLANKS
644500160914     **
644600160914      * il codice "E" partirà da nov.2016
644700160914     c                   if        datcor >= §VPODATA and tblKEY ='E'
644800160914     c                              or tblKEY<>'E'
644900020220     C                   MOVEL     TBLUNI        DSIC
645000020220     C                   MOVEL     TBLKEY        ABC(§SICOR)
645100160914     c                   end
645200160914     **
645300980521E   2C                   ENDIF
645400980521     C     KEYTA2        READE     TABEL00F                               99
645500980521E   1C                   ENDDO
645600000000     C*
645700000000     C                   ENDSR
645800000000     C*------------------------------------------------------------------------*
645900000000     C*?STAPAR - STAMPA PAGINA PARAMETRI
646000000000     C*------------------------------------------------------------------------*
646100000000     C     STAPAR        BEGSR
646200000000     C*--------------------
646300000000     C* NOME PROGRAMMA
646400000000     C*--------------------
646500020116     C                   MOVEL     'TISE93R'     PTCPGM
646600000000     C*--------------------
646700000000     C* DESCRIZIONE STATISTICA
646800000000     C*--------------------
646900000000     C                   MOVEL     DST(S)        PTCDST
647000000000     C*--------------------
647100000000     C* CRAZIONE FILE DI LAVORO
647200000000     C*--------------------
647300000000IF  1C     PARCRE        IFEQ      'S'
647400000000     C                   MOVEL     'SI'          PACCRE
647500000000X   1C                   ELSE
647600000000     C                   MOVEL     'NO'          PACCRE
647700000000E   1C                   ENDIF
647800000000     C*--------------------
647900000000     C* DATE LANCIO
648000000000     C*--------------------
648100000000     C                   Z-ADD     PARDA2        PACDA2                         *ANNO/MESE FINALE
648200000000     C                   Z-ADD     PARDAT        PACDAT                         *ANNO/MESE INIZIALE
648300000000     C*--------------------
648400000000     C* SELEZIONI PER TIPO SERVIZIO
648500000000     C*--------------------
648600000000IF  1C     PARTSP        IFEQ      *BLANKS
648700000000     C                   MOVEL     DRI(1)        PADTSP
648800100129      **
648900100129      **  si memorizza quanti tipi servizi sono attivi nella tabella
649000100129     c                   z-add     0             Quanti_Tserv
649100100129     c                   clear                   TServ
649200100129     c                   clear                   DTServ
649300100129      **  carica codice e descrizione su 2 schiere e Quanti_serv contiene il nr.dei tipi servizio abilitati
649400100129     C                   Z-ADD     1             KTBKUT
649500100129     C                   MOVEL     '5E'          KTBCOD
649600100129     C     KEYTA2        setll     TABEL00F
649700100129     C     KEYTA2        reade     TABEL00F
649800100129     c                   dow       not %EoF(TABEL00F)
649900100129     c                   add       1             Quanti_Tserv
650000100129     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
650100100129     c                   eval        tserv(Quanti_Tserv) = tblkey
650200100129     c                   eval       Dtserv(Quanti_Tserv) = §5EDES
650300100129     C     KEYTA2        reade     TABEL00F
650400100129     c                   endDO
650500100129      **
650600000000X   1C                   ELSE
650700100129      **
650800000000     C                   MOVEL     PARTSP        PACTSP
650900020906     C                   Z-ADD     1             KTBKUT
651000000000     C                   MOVEL     '5E'          KTBCOD
651100000000     C                   MOVEL     *BLANKS       KTBKEY
651200000000     C                   MOVEL     PARTSP        KTBKEY
651300980406     C     KEYTAB        CHAIN     TABEL00F                           99
651400000000IF  2C     *IN99         IFEQ      *OFF
651500090610     C                   MOVEL     TBLUNI        ds5e                           *TIPO SERVIZIO
651600090610     C                   MOVEL     §5EDES        PADTSP                         *TIPO SERVIZIO
651700000000E   2C                   ENDIF
651800000000E   1C                   ENDIF
651900950208     C*--------------------
652000950208     C* SELEZIONI PER DELTA ITALIA/ESTERO
652100950208     C*--------------------
652200950208     C                   MOVEL     PARIOE        PACIOE
652300950208IF  1C     PARIOE        IFEQ      'I'
652400950208     C                   MOVEL     DRI(5)        PADIOE
652500950208E   1C                   ENDIF
652600950208IF  1C     PARIOE        IFEQ      'E'
652700950208     C                   MOVEL     DRI(6)        PADIOE
652800950208E   1C                   ENDIF
652900950208IF  1C     PARIOE        IFEQ      *BLANKS
653000950208     C                   MOVEL     DRI(7)        PADIOE
653100950208E   1C                   ENDIF
653200951018     C*--------------------
653300970822     C* SELEZIONI PER QUALITA' CLIENTE
653400951018     C*--------------------
653500951018     C                   MOVEL     PARCLV        PACCLV
653600010618     C                   MOVEL     PARCL1        PACCL1
653700010618     C                   MOVEL     PARCL2        PACCL2
653800160909     C                   MOVEL     PARCL3        PACCL3
653900160909     C                   MOVEL     PARCL4        PACCL4
654000951018     C                   MOVEL     *BLANKS       PADCLV
654100951018IF  1C     PARCLV        IFEQ      *BLANKS
654200010618     C     PARCL1        ANDEQ     *BLANKS
654300010618     C     PARCL2        ANDEQ     *BLANKS
654400160909     C     PARCL3        ANDEQ     *BLANKS
654500160909     C     PARCL4        ANDEQ     *BLANKS
654600951018     C                   MOVEL     DRI(1)        PADCLV
654700980528E   1C                   ENDIF
654800010618     C*--------------------
654900010618     C* TIPO STAMPA SINTETICA S/N
655000010618     C*--------------------
655100010618     C                   MOVEL     PARSIN        PACSIN
655200980528     C*--------------------
655300980528     C* % SCOSTAMENTO DELTA TOTALE
655400980528     C*--------------------
655500980528     C                   Z-ADD     PARPDD        PACPDD
655600980528     C                   Z-ADD     PARPDA        PACPDA
655700980528     C*--------------------
655800980528     C* % SCOSTAMENTO DELTA PER FASCE
655900980528     C*--------------------
656000970822     C                   Z-ADD     PARSPD        PACSPD
656100020702     C*--------------------
656200020702     C* % SCOSTAMENTO FATTURATO ANNO PRECEDENTE
656300020702     C*--------------------
656400020702     C                   Z-ADD     PARDFD        PACDFD
656500020702     C                   Z-ADD     PARDFA        PACDFA
656600020702     C                   Z-ADD     PARDFDP       PACDFDP
656700020702     C                   Z-ADD     PARDFAP       PACDFAP
656800000000     C*--------------------
656900000000     C* SELEZIONI PER DELTA FILIALE
657000000000     C*--------------------
657100000000IF  1C     PARFIC        IFGT      *ZEROS                                       *FILIALE
657200000000     C                   Z-ADD     PARFIC        DEPFIL
657300000000     C                   EXSR      DECFIL
657400000000     C                   Z-ADD     *ZEROS        DEPFIL
657500000000     C                   MOVEL     PARFIC        PACFIC
657600000000     C                   MOVEL     DESFIL        PADFIC
657700000000X   1C                   ELSE
657800000000     C                   MOVEL     *BLANKS       PACFIC
657900000000E   1C                   ENDIF
658000000000     C*
658100000000IF  1C     PARARC        IFGT      *ZEROS                                       *AREA
658200000000     C                   Z-ADD     PARARC        DEPARE
658300000000     C                   EXSR      DECARE
658400000000     C                   Z-ADD     *ZEROS        DEPARE
658500000000     C                   MOVEL     PARARC        PACARC
658600000000     C                   MOVEL     DESARE        PADARC
658700000000X   1C                   ELSE
658800000000     C                   MOVEL     *BLANKS       PACARC
658900000000E   1C                   ENDIF
659000000000     C*
659100000000IF  1C     PARDIC        IFNE      *BLANKS                                      *DIVISIONE
659200000000     C                   MOVEL     PARDIC        DEPDIV
659300000000     C                   EXSR      DECDIV
659400000000     C                   MOVEL     *BLANKS       DEPDIV
659500000000     C                   MOVEL     PARDIC        PACDIC
659600000000     C                   MOVEL     DESDIV        PADDIC
659700000000E   1C                   ENDIF
659800000000     C*--------------------
659900000000     C* SELEZIONI PER DELTA COMMERCIALE
660000000000     C*--------------------
660100000000IF  1C     PARCOF        IFGT      *ZEROS                                       *COMMERCIALE
660200000000     C                   Z-ADD     PARCOF        DEPAGE
660300000000     C                   EXSR      DECAGE
660400000000     C                   Z-ADD     *ZEROS        DEPAGE
660500000000     C                   MOVEL     PARCOF        PACCOF
660600000000     C                   MOVEL     DESAGE        PADCOF
660700000000X   1C                   ELSE
660800000000     C                   MOVEL     *BLANKS       PACCOF
660900000000E   1C                   ENDIF
661000000000     C*
661100000000IF  1C     PARDIF        IFNE      *BLANKS                                      *DIVISIONE
661200000000     C                   MOVEL     PARDIF        DEPDIV
661300000000     C                   EXSR      DECDIV
661400000000     C                   MOVEL     *BLANKS       DEPDIV
661500000000     C                   MOVEL     PARDIF        PACDIF
661600000000     C                   MOVEL     DESDIV        PADDIF
661700000000E   1C                   ENDIF
661800000707     C*--------------------
661900000707     C* DATI POSTE
662000000707     C*--------------------
662100000707IF  1C     PARPOS        IFEQ      'S'
662200000707     C                   MOVEL     'SI'          PADPOS
662300000707X   1C                   ELSE
662400000707     C                   MOVEL     'NO'          PADPOS
662500000707E   1C                   ENDIF
662600000825     C*--------------------
662700000825     C* DATI CLIENTE / UNIFICANTE
662800000825     C*--------------------
662900000825     C                   MOVEL     PARSCE        PACSCE
663000000825IF  1C     PARSCE        IFEQ      'C'
663100000825     C                   MOVEL     DRI(29)       PADSCE
663200000825X   1C                   ELSE
663300000825     C                   MOVEL     DRI(30)       PADSCE
663400000825E   1C                   ENDIF
663500000000     C*
663600000000     C* S T A M P A
663700000000     C*
663800100129     c                   if        parSTAMPA <> 'N'
663900020222     C                   WRITE     SE94PT                                       *INTESTAZIONE
664000020116     C                   WRITE     SE94PA                                       *PARAMETRI
664100100129     c                   end
664200000000     C*
664300000000     C* ACCENDO INDICATORE DI OF PER STAMPARE LA TESTATA ALLA 1^ ROTTURA CLIENTE
664400000000     C                   SETON                                        01
664500000000     C                   MOVEL     '0'           PRIGIR
664600000000     C*
664700000000     C                   ENDSR
664800980406     C*------------------------------------------------------------------------*
664900980406     C*?FINSR - OPERAZIONI FINALI
665000980406     C*------------------------------------------------------------------------*
665100980406     C     FINSR         BEGSR
665200980406     C*
665300980406     C* CHIUDE I FILE DEI PGM CHIAMATI
665400980406     C                   CLEAR                   BS69DS
665500980406     C                   CLEAR                   ACODS
665600980406     C                   CLEAR                   INDDS
665700980406     C                   CLEAR                   CLPDS
665800980406     C                   CLEAR                   CLSDS
665900980406     C                   MOVEL     'C'           I69TLA                         *TIPO LANCIO
666000980406     C                   MOVEL     KNSIF         I69SIF                         *S.INFORMATIVO
666100980406     C                   CALL      'TIBS69R'
666200980406     C                   PARM                    BS69DS
666300980406     C                   PARM                    ACODS
666400980406     C                   PARM                    INDDS
666500980406     C                   PARM                    CLPDS
666600980406     C                   PARM                    CLSDS
666700980406     C*
666800980406     C                   ENDSR
666900030924     C*------------------------------------------------------------------------*
667000030924     C*?CANWF - CANCELLA DATI WFILE, SE RICHIESTO
667100030924     C*------------------------------------------------------------------------*
667200030924     C     CANWF         BEGSR
667300030924     C*
667400030925     C     PARSTA        IFEQ      'F'
667500030924     C     PARWCP        IFEQ      'S'                                          *SE RICHIESTO
667600030924     c                   MOVEL     PARIOE        KE93IOE                        *ITALIA/ESTERO
667700030924     C     KEYSE93       SETLL     WFTISE93L
667800030924     C     KEYSE93       READE     WFTISE93L                              99
667900030924     C                   DOW       NOT *IN99
668000030924     C                   DELETE    WFTISE930
668100030924     C     KEYSE93       READE     WFTISE93L                              99
668200030924     C                   ENDDO
668300030925     C                   ENDIF
668400030925     C                   ENDIF
668500030924     C*
668600030924     C                   ENDSR
668700000000     C*------------------------------------------------------------------------*
668800000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
668900000000     C*------------------------------------------------------------------------*
669000000000     C     *INZSR        BEGSR
669100960708     C*--------------------
669200960708     C* RICEVIMENTO PARAMETRI
669300960708     C*--------------------
669400000000     C     *ENTRY        PLIST
669500000000     C                   PARM                    KPJBA
669600000000     C                   MOVEL     KPJBU         PARAM
669700160713     c* Se richiamato x BUDGET questa DATA è Valorizzata
669800160713     C                   MOVE      KPJBU         Data_8alfa        8
669900160913     c*
670000160913     c*  schiera della QUALITA' CLIENTE (aggiunto da novembre 2016 qual."E")
670100160914     c*
670200160914     C                   movel     'VPO'         TBECOD
670300160914     C                   movel(p)  'TISE93R'     TBEKE1
670400160914     C                   movel(p)  'CLV_E'       TBEKE2
670500160914     c     ktbe          chain     tnTBE01L
670600160914     c                   clear                   dvpoTISE93
670700160914     c                   if        %Found(tnTBE01L)
670800160914     c                   eval        dvpoTISE93 = tbeuni
670900160914     c                   end
671000160913     c*
671100160713      *  controllo validità campo Data ricevuto se in MODALITA' BUDGET
671200160713     c                   clear                   dta_BDG_unif      8 0
671300160713     c                   if        Data_8alfa <> *blank
671400160713     c     digits        check     Data_8alfa
671500160713     c                   if        not %Found
671600160713     c                   move      Data_8alfa    dta_BDG_unif
671700160713      *** se Budget PARIOE = *Blank per sommare ITALIA +ESTERO attenzione
671800160713     c                   end
671900160713     c                   endif
672000100129      *
672100100129      * se non si deve più eseguire la stampa viene passato il flag impostato a "N"
672200100129     c                   if        parSTAMPA <> 'N'
672300100129     c                   open      TISE94P
672400100129     c                   end
672500960708     C*--------------------
672600960708     C* VARIABILI RIFERITE AL DATABASE
672700960708     C*--------------------
672800000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
672900100201     C     *LIKE         DEFINE    TBLCOD        KTBCOD
673000000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
673100000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
673200950213     C     *LIKE         DEFINE    WFDDIV        KWDDIV                         *WFDEL01L
673300000000     C     *LIKE         DEFINE    WFDARE        KWDARE
673400000000     C     *LIKE         DEFINE    WFDFIL        KWDFIL
673500000000     C     *LIKE         DEFINE    WFDAGE        KWDAGE
673600960710     C     *LIKE         DEFINE    WFDCLV        KWDCLV
673700000825     C     *LIKE         DEFINE    WFDCLI        KWDCLI
673800000000     C     *LIKE         DEFINE    WFDANN        KWDANN
673900000000     C     *LIKE         DEFINE    WFDMES        KWDMES
674000960708     C     *LIKE         DEFINE    WFDDIV        DEPDIV                         *DI DEPOSITO
674100000000     C     *LIKE         DEFINE    WFDARE        DEPARE
674200000000     C     *LIKE         DEFINE    WFDFIL        DEPFIL
674300000000     C     *LIKE         DEFINE    WFDAGE        DEPAGE
674400081013     C     *LIKE         DEFINE    WFDRGD        DEPRGD
674500081013     C     *LIKE         DEFINE    WFDRGF        DEPRGF
674600070822     C     *LIKE         DEFINE    WFDCLI        DEPCLI
674700070822     C     *LIKE         DEFINE    WFDCLI        sav_depCLI
674800020319     C     *LIKE         DEFINE    WFDCLI        DEPCL2
674900000000     C     *LIKE         DEFINE    WFDANN        DEPANN
675000000000     C     *LIKE         DEFINE    WFDMES        DEPMES
675100020319     C     *LIKE         DEFINE    WFDNTW        DEPNTW
675200140612     C**** *LIKE         DEFINE    §01PCF        DESPCF
675300140612     C     *LIKE         DEFINE    cmmPLI        DESPCF
675400000000     C     *LIKE         DEFINE    ACORAG        DESKSC
675500070419     C     *LIKE         DEFINE    INDvia        DESvia
675600070419     C     *LIKE         DEFINE    INDcit        DEScit
675700070419     C     *LIKE         DEFINE    INDcap        DEScap
675800070419     C     *LIKE         DEFINE    INDcae        DEScae
675900070419     C     *LIKE         DEFINE    INDprv        DESprv
676000070419     C     *LIKE         DEFINE    INDsta        DESsta
676100070615     C     *LIKE         DEFINE    INDcdf        DEScdf                         *Cod.Fiscale
676200070615     C     *LIKE         DEFINE    INDiva        DESiva                         *Part.IVA
676300070618     C     *LIKE         DEFINE    INDtel        DEStel                         *Telefono
676400000000     C     *LIKE         DEFINE    CLPCLV        DESCLV
676500000000     C     *LIKE         DEFINE    WFDCLN        DESCLN
676600070822     C     *LIKE         DEFINE    WFDCLN        sav_desCLN_Y
676700960708     C     *LIKE         DEFINE    ACOABL        DESABL
676800030925     C     *LIKE         DEFINE    SE93IOE       KE93IOE
676900100201     c     *LIKE         DEFINE    k             NumElem
677000100201      **
677100960708     C*--------------------
677200960708     C* VARIABILI NON RIFERITE AL DATABASE
677300960708     C*--------------------
677400000000     C                   Z-ADD     *ZEROS        C                 4 0          *COMMERCIALI
677500000000     C                   Z-ADD     *ZEROS        D                 4 0          *PER DECODIFICHE
677600000000     C                   Z-ADD     *ZEROS        F                 4 0          *VOCI ORGANIGRAMMA
677700000000     C                   Z-ADD     *ZEROS        J                 4 0          *GENERICO
677800000000     C                   Z-ADD     *ZEROS        P                 4 0          *ELEMENTI PERCENTUAL
677900000000     C                   Z-ADD     *ZEROS        Q                 4 0          *QUALITA' CLIENTE
678000000000     C                   Z-ADD     *ZEROS        S                 4 0          *STATISTICA IN ELAB
678100000000     C                   Z-ADD     *ZEROS        X                 2 0          *PER I MESI
678200000000     C                   Z-ADD     *ZEROS        MM                2 0          *PER I MESI
678300000000     C                   Z-ADD     *ZEROS        IN                2 0          *PER I MESI
678400000000     C                   Z-ADD     *ZEROS        SPARM             4 0          *N° MESI SELEZIONATI
678500000828     C                   Z-ADD     *ZEROS        I                 5 0          *INDICE
678600000828     C                   Z-ADD     *ZEROS        IS                5 0          *INDICE
678700020318     C                   Z-ADD     *ZEROS        K                 4 0          *NETWORK
678800020701     C                   Z-ADD     *ZEROS        II                5 0          *INDICE
678900020701     C*--------------------
679000020701     C* INDICATORI DI PROCEDURA
679100020701     C*--------------------
679200020701     C                   SETOFF                                       54
679300960708     C*--------------------
679400960708     C* VARIABILI DI CONTROLLO
679500960708     C*--------------------
679600000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
679700000000     C                   MOVEL     '0'           PASGEN            1            MEMOR SALDI GENERALI
679800000000     C                   MOVEL     '0'           PASDIV            1            MEMOR SALDI DIVISION
679900000000     C                   MOVEL     '0'           PASARE            1            MEMOR SALDI AREA
680000000000     C                   MOVEL     '0'           PASFIL            1            MEMOR SALDI FILIALE
680100000000     C                   MOVEL     '0'           PASAGE            1            MEMOR SALDI COMM.LE
680200000000     C                   MOVEL     '0'           PASKSC            1            MEMOR SALDI CLIENTE
680300951227     C                   MOVEL     '0'           FLAKSC            1            FLAG CONT CLI CORR
680400951227     C                   MOVEL     '0'           FL2KSC            1            FLAG CONT CLI PREC
680500000000     C                   MOVEL     '0'           FLAPAS            1            FLAG PASSAGGIO LETTU
680600000000     C                   MOVEL     '0'           $CATCL            1            CATEGORIA CLIENTE
680700000000     C                   Z-ADD     *ZEROS        $Q                1 0          QUALITA' CLIENTE
680800000000     C                   MOVEL     ' '           TIPANN            1            *TIPO ANNO (P/C)
680900000000     C                   MOVEL     '0'           PRIGIR            1            *PRIMO GIRO
681000000000     C                   MOVEL     '0'           ORAKSC            1            *ORA STAMPA CLIENTE
681100000828     C                   MOVEL     'N'           $STRIG            1            *STAMPA RIGA
681200020221     C                   MOVEL     '0'           DEPSTPD           1            *FLAG DI STAMPA SI/N
681300020222     C                   MOVEL     '0'           FLGSTPLE          1            *FLAG DI STAMPA SI/N
681400020318     C                   MOVEL     '0'           FLGSTPNTW         1            *FLAG DI STAMPA SI/N
681500020319     C                   MOVEL     '0'           FLGSTPRI          1            *FLAG DI STAMPA SI/N
681600960708     C*--------------------
681700960708     C* CHAIVI DI LETTURA
681800960708     C*--------------------
681900960708     C* LETTURA TABEL00F - COMPLETA
682000000000     C     KEYTAB        KLIST
682100000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
682200000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
682300000000     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
682400960708     C* LETTURA TABEL00F - PARZIALE
682500000000     C     KEYTA2        KLIST
682600000000     C                   KFLD                    KTBKUT                         *CODICE UTENTE
682700000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
682800160914      * tab.VPO attivazioni modifiche
682900160914     C     KTBE          KLIST
683000160914     C                   KFLD                    TBECOD
683100160914     C                   KFLD                    TBEKE1
683200160914     C                   KFLD                    TBEKE2
683300960708     C* LETTURA AZORG01L - COMPLETA
683400000000     C     KEYORG        KLIST
683500000000     C                   KFLD                    KORFIL                         *FILIALE
683600960708     C* LETTURA WFDEL01L - PARZIALE
683700000000     C     KEYDEL        KLIST
683800950208     C                   KFLD                    KWDDIV                         *DIVISIONE
683900000000     C                   KFLD                    KWDARE                         *AREA
684000000000     C                   KFLD                    KWDFIL                         *FILIALE
684100000000     C                   KFLD                    KWDAGE                         *COMMERCIALE
684200960710     C                   KFLD                    KWDCLV                         *CODICE QUALITA'
684300000825     C                   KFLD                    KWDCLI                         *CLIENTE
684400000000     C                   KFLD                    KWDANN                         *ANNO
684500000000     C                   KFLD                    KWDMES                         *MESE
684600030924     C* LETTURA WFTISE93L - PARZIALE
684700030924     C     KEYSE93       KLIST
684800030924     C                   KFLD                    KE93IOE                        *ITALIA/ESTERO
684900070420     C* LETTURA TFNTC00F - COMPLETA
685000070420     C     KeyNTC        KLIST
685100070420     C                   KFLD                    NTCapl                         *CODICE APPLICAZIONE
685200070420     C                   KFLD                    NTCnk1                         *CHIAVE 1 NOTE
685300070420     C                   KFLD                    NTCnk2                         *CHIAVE 2 NOTE
685400070420     C                   KFLD                    NTCtnt                         *TIPO NOTA
685500000828     C*
685600130131      ***
685700130131     c     k_deaV1       KLIST
685800130131     C                   KFLD                    VARIA             1
685900130131     C                   KFLD                    DEPCLI
686000130131     C*  serve x il FUEL
686100130131     C                   eval       VARIA ='f'
686200130131     C*
686300000828     C* IMNPOSTA DATA / ORA CORRENTE
686400000828     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
686500000828     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
686600000828     C                   Z-ADD     *ZEROS        G08INV
686700000828     C                   MOVEL     '0'           G08ERR
686800000828     C                   CALL      'XSRDA8'
686900000828     C                   PARM                    WLBDA8
687000000828     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
687100010725     C*
687200160713     C*
687300160713     C*    SE RICHIAMATO come BUDGET deve impostare la DATA di Riferimento
687400160713     C*       direttamente dal pgm chiamante
687500160713     c                   if        dta_BDG_unif   = 0
687600160713     C*
687700010725     C* COMPONE IL PRIMO GIORNO DELLA DATA FINALE
687800010725     C                   MOVEL     PARDA2        WDFIN             8 0          *DATA INIZIALE
687900010725     C                   MOVE      28            WDFIN
688000081114     C*
688100081114     C*------- GIORNO   di   F I N E   M E S E ---------------------*
688200081114     C*
688300081114     C*  Ultimo giorno del mese
688400081114     C                   MOVE      WDFIN         MMGG_4            4 0
688500081114     C                   MOVEL     MMGG_4        Mm_2              2 0
688600081114     C                   MOVEl     WDFIN         Anno_4            4 0
688700081114      * se è febbraio
688800081114     c                   If        Mm_2 = 2
688900081114      *
689000081114      *  potrebbe essere bisestile 29 gg.
689100081114     c     Anno_4        div       4             risulta           3 0
689200081114     c                   mvr                     resto             3 0
689300081114      *  Se bisestile
689400081114     c                   If        resto = 0
689500081114     C                   MOVE      '29'          WDFIN
689600081114     c                   end
689700081114      *
689800081114     c                   else
689900081114      *
690000081114      *  se invece Aprile, Giugno, Settembre, Novembre con 30 gg.
690100081114     c                   If           Mm_2 = 4 or Mm_2 = 6 or
690200081114     c                                Mm_2 = 9 or Mm_2 = 11
690300081114     C                   MOVE      '30'          WDFIN
690400081114     c                   else
690500081114     C                   MOVE      '31'          WDFIN
690600081114     c                   end
690700081114      *
690800081114     c                   endIf
690900160713     C*
691000081114     C*----------- F I N E    M E S E ---------------------------*
691100160713     C*
691200160713     C*    altrimenti come BUDGET imposta la data di riferimento
691300160713     C*    dell'unificante
691400160713     c                   else
691500160713     C                   MOVE      dta_BDG_unif  WDFIN
691600160713     c                   end
691700081114     C*
691800000000     C*
691900000000     C* MEMORIZZA I DATI IN TABELLE
692000000000     C                   EXSR      CARTAB
692100000000     C*
692200000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
692300000000     C                   EXSR      CARMES
692400000000     C*
692500000000     C* DECODIFICA INTESTAZIONE
692600000000     C                   EXSR      DECTES
692700000000     C*
692800000000     C* VEDO QUALE STATISTICA DEVO ELABORARE
692900000000IF  1C     PARSTA        IFEQ      'K'
693000000000     C                   Z-ADD     1             S                              *DETTAGLIO CLIENTE
693100000000E   1C                   ENDIF
693200000000IF  1C     PARSTA        IFEQ      'C'
693300000000     C                   Z-ADD     2             S                              *DETTAGLIO COMM.LE
693400000000E   1C                   ENDIF
693500000000IF  1C     PARSTA        IFEQ      'F'
693600000000     C                   Z-ADD     3             S                              *DETTAGLIO FILIALE
693700000000E   1C                   ENDIF
693800051102     C*
693900051102IF  1C     PARSCE        IFEQ      'U'                                          *STAT. UNIFICANTE
694000051102IF  1C     PARSTA        andeq     'K'                                           dett.x cliente
694100051102     c                   eval      LEG040 = '(+) --> Cliente BLOCCATO'
694200051102     c                   end
694300030924     C*
694400030924     C* CANCELLA DATI WFILE, SE RICHIESTO
694500030924     C                   EXSR      CANWF
694600000000     C*
694700091124     C* Deve impostare i totali per Netowrk
694800091124IF  1C     PARIOE        IFEQ      'E'
694900091124     C                   open      waDELpcN
695000091124E   1C                   ENDIF
695100130131     C*
695200130131     C                   ENDSR
695300130131     C*------------------------------------------------------------------------*
695400130131     C*?ACCUMULA le VARIE - al momento solo x il FUEL (che si vuole scorporare)
695500130131     C*------------------------------------------------------------------------*
695600130131     C     VARIE_x_ClientBEGSR
695700130131      *
695800130131      *  Lo dovrà scrivere una sola volta anche x i giri fra Italia ed Estero
695900130131      *   è per questo che cancellando i records una volta elaborati verrà
696000130131      *   scritto correttamente la prima volta che incontra il codice CLIENTE.
696100130131     c                   move      'N'           deve_scriverlo    1
696200130131      *
696300130131      * ?Cicla sul file delle varie x il tipo servizio (f)->FUEL e x CLIENTE
696400130131      *
696500130131      * ? Man mano che legge e totalizza i records li CANCELLA dal WORKFile
696600130131      * ? per alleggerire le successive operazioni.
696700130131      *  La lettura è x VARIA/CLIENTE
696800130131     c     k_deaV1       setll     wfDEAv1l
696900130131     c     k_deaV1       reade     wfDEAv1l
697000130131     C*
697100130131     c                   DOW       not %EoF(wfDEAv1l)
697200130131     C*
697300130131     C* lo fa solo la prima, primissima volta e basta
697400130131     c                   if        deve_scriverlo = 'N'
697500130131     c                   eval         deve_scriverlo = 'S'
697600130131     c                   clear                   WADELVT0
697700130131     C                   eval      watAAD =  WAdAAD
697800130131     C                   eval      watMMD =  WAdMMD
697900130131     C                   eval      watAAA =  WAdAAA
698000130131     C                   eval      watMMA =  WAdMMA
698100130131     C                   eval      watCLV =  desCLV
698200130131     C                   eval      watCLI =  depCLI
698300130131     C                   eval      watCLD =  desKSC
698400130131     C                   eval      watSVR =  WFVSVR
698500130131     c                   end
698600130131     C*
698700130131     C*  Anno in corso
698800130131     C                   IF        WFVANN =  WAdAAD
698900130131     C*
699000130131     c                   select
699100130131     C*
699200130131     C                   when      WFVmes =  1
699300130131     c                   eval      WAtVA_01SC = wfvNSP + WAtVA_01SC
699400130131     c                   eval      WAtVA_01VC = wfvRCP + WAtVA_01VC
699500130131     C                   when      WFVmes =  2
699600130131     c                   eval      WAtVA_02SC = wfvNSP + WAtVA_02SC
699700130131     c                   eval      WAtVA_02VC = wfvRCP + WAtVA_02VC
699800130131     C                   when      WFVmes =  3
699900130131     c                   eval      WAtVA_03SC = wfvNSP + WAtVA_03SC
700000130131     c                   eval      WAtVA_03VC = wfvRCP + WAtVA_03VC
700100130131     C                   when      WFVmes =  4
700200130131     c                   eval      WAtVA_04SC = wfvNSP + WAtVA_04SC
700300130131     c                   eval      WAtVA_04VC = wfvRCP + WAtVA_04VC
700400130131     C                   when      WFVmes =  5
700500130131     c                   eval      WAtVA_05SC = wfvNSP + WAtVA_05SC
700600130131     c                   eval      WAtVA_05VC = wfvRCP + WAtVA_05VC
700700130131     C                   when      WFVmes =  6
700800130131     c                   eval      WAtVA_06SC = wfvNSP + WAtVA_06SC
700900130131     c                   eval      WAtVA_06VC = wfvRCP + WAtVA_06VC
701000130131     C                   when      WFVmes =  7
701100130131     c                   eval      WAtVA_07SC = wfvNSP + WAtVA_07SC
701200130131     c                   eval      WAtVA_07VC = wfvRCP + WAtVA_07VC
701300130131     C                   when      WFVmes =  8
701400130131     c                   eval      WAtVA_08SC = wfvNSP + WAtVA_08SC
701500130131     c                   eval      WAtVA_08VC = wfvRCP + WAtVA_08VC
701600130131     C                   when      WFVmes =  9
701700130131     c                   eval      WAtVA_09SC = wfvNSP + WAtVA_09SC
701800130131     c                   eval      WAtVA_09VC = wfvRCP + WAtVA_09VC
701900130131     C                   when      WFVmes = 10
702000130131     c                   eval      WAtVA_10SC = wfvNSP + WAtVA_10SC
702100130131     c                   eval      WAtVA_10VC = wfvRCP + WAtVA_10VC
702200130131     C                   when      WFVmes = 11
702300130131     c                   eval      WAtVA_11SC = wfvNSP + WAtVA_11SC
702400130131     c                   eval      WAtVA_11VC = wfvRCP + WAtVA_11VC
702500130131     C                   when      WFVmes = 12
702600130131     c                   eval      WAtVA_12SC = wfvNSP + WAtVA_12SC
702700130131     c                   eval      WAtVA_12VC = wfvRCP + WAtVA_12VC
702800130131     c                   endSL
702900130131     C*
703000130131     C                   eval      WAtVAT_SC  = wfvNSP  + WAtVAT_SC
703100130131     C                   eval      WAtVAT_VC  = wfvRCP  + WAtVAT_VC
703200130131     C*  Anno precedente
703300130131     C                   ELSE
703400130131     C*
703500130131     c                   select
703600130131     C*
703700130131     C                   when      WFVmes =  1
703800130131     c                   eval      WAtVA_01SP = wfvNSP + WAtVA_01SP
703900130131     c                   eval      WAtVA_01VP = wfvRCP + WAtVA_01VP
704000130131     C                   when      WFVmes =  2
704100130131     c                   eval      WAtVA_02SP = wfvNSP + WAtVA_02SP
704200130131     c                   eval      WAtVA_02VP = wfvRCP + WAtVA_02VP
704300130131     C                   when      WFVmes =  3
704400130131     c                   eval      WAtVA_03SP = wfvNSP + WAtVA_03SP
704500130131     c                   eval      WAtVA_03VP = wfvRCP + WAtVA_03VP
704600130131     C                   when      WFVmes =  4
704700130131     c                   eval      WAtVA_04SP = wfvNSP + WAtVA_04SP
704800130131     c                   eval      WAtVA_04VP = wfvRCP + WAtVA_04VP
704900130131     C                   when      WFVmes =  5
705000130131     c                   eval      WAtVA_05SP = wfvNSP + WAtVA_05SP
705100130131     c                   eval      WAtVA_05VP = wfvRCP + WAtVA_05VP
705200130131     C                   when      WFVmes =  6
705300130131     c                   eval      WAtVA_06SP = wfvNSP + WAtVA_06SP
705400130131     c                   eval      WAtVA_06VP = wfvRCP + WAtVA_06VP
705500130131     C                   when      WFVmes =  7
705600130131     c                   eval      WAtVA_07SP = wfvNSP + WAtVA_07SP
705700130131     c                   eval      WAtVA_07VP = wfvRCP + WAtVA_07VP
705800130131     C                   when      WFVmes =  8
705900130131     c                   eval      WAtVA_08SP = wfvNSP + WAtVA_08SP
706000130131     c                   eval      WAtVA_08VP = wfvRCP + WAtVA_08VP
706100130131     C                   when      WFVmes =  9
706200130131     c                   eval      WAtVA_09SP = wfvNSP + WAtVA_09SP
706300130131     c                   eval      WAtVA_09VP = wfvRCP + WAtVA_09VP
706400130131     C                   when      WFVmes = 10
706500130131     c                   eval      WAtVA_10SP = wfvNSP + WAtVA_10SP
706600130131     c                   eval      WAtVA_10VP = wfvRCP + WAtVA_10VP
706700130131     C                   when      WFVmes = 11
706800130131     c                   eval      WAtVA_11SP = wfvNSP + WAtVA_11SP
706900130131     c                   eval      WAtVA_11VP = wfvRCP + WAtVA_11VP
707000130131     C                   when      WFVmes = 12
707100130131     c                   eval      WAtVA_12SP = wfvNSP + WAtVA_12SP
707200130131     c                   eval      WAtVA_12VP = wfvRCP + WAtVA_12VP
707300130131     c                   endSL
707400130131     C*
707500130131     C                   eval      WAtVAT_SP  = wfvNSP + WAtVAT_SP
707600130131     C                   eval      WAtVAT_VP  = wfvRCP + WAtVAT_VP
707700130131     C*
707800130131     C                   END
707900130131     C*
708000130131     C*  cancella il record già incasellato in modo da non riutilizzarlo
708100130131     C*   più in seguito in sommatorie errate
708200130131     c                   delete    WFDEAV00
708300130131     C*
708400130131     c     k_deaV1       reade     wfDEAv1l
708500130131     C                   ENDDO
708600130131     C*
708700130131     C*
708800130131     C* lo fa solo la prima, primissima volta e basta
708900130131     c                   if        deve_scriverlo = 'S'
709000130131     c                   write     WADELVT0
709100130131     c                   end
709200130131     C*
709300000000     C                   ENDSR
709400130131     C*------------------------------------------------------------------------*
709500000000**   DMM - DECODIFICHE MESI DELL'ANNO
709600000000GEN  1
709700000000FEB  2
709800000000MAR  3
709900000000APR  4
710000000000MAG  5
710100000000GIU  6
710200000000LUG  7
710300000000AGO  8
710400000000SET  9
710500000000OTT  10
710600000000NOV  11
710700000000DIC  12
710800000000**   DST - DECODIFICHE STATISTICHE
710900000000PER FILIALE              1
711000000000PER AREA                 2
711100000000PER DIVISIONE            3
711200000000                         4
711300000000                         5
711400000000**   DRI - DECODIFICHE RIGA DI STAMPA
711500000000TUTTI                    1
711600000000TUTTE                    2
7117000000003
711800950208NUOVI                    4
711900950208ITALIA                   5
712000950208ESTERO                   6
712100950602ITALIA+ESTERO            7
712200020318ACQUIS.                  8
7123000000009
71240000000010
71250000000011
712600020220TOTALE                  12
712700000000CLIENTI                 13
712800000000SPEDIZ                  14
712900000000RICAVO                  15
713000000000DELTA                   16
713100000000NUOVO                   17
713200000000ACQUISITO               18
71330002022119
713400000000      C L I E N T I     20
713500000000 C O M M E R C I A L E  21
713600000000     F I L I A L E      22
713700000000       A R E A          23
713800000000   D I V I S I O N E    24
713900000000    G E N E R A L E     25
71400000082926
71410000082927
714200000829UNIFICATI:               28
714300000829PER CLIENTE              29
714400000829PER CLIENTE UNIFICANTE   30
714500020318**   DNTW - DECODIFICHE NETWORK
714600020318                          1
714700020318EEX                       2
714800020318DPD                       3
714900020318FED                       4
715000020318EUP                       5
