000100980821     H*------------------------------------------------------------------------*
000200970110     H* TARIFFE DA CONTO ECONOMICO - CREAZIONE DATI TIPO - SEDE                *
000300970110     H*------------------------------------------------------------------------*
000400970110     H DECEDIT('0,') DATEDIT(*DMY.)
000500970110     F*------------------------------------------------------------------------*
000600970110     F* DATA BASE
000700970110     F*------------------------------------------------------------------------*
000800970110     F*-----------
000900970110     F* BOLLE DI SEDE
001000970110     F*-----------
001100990812     FTITAS31C  IF   E           K DISK    USROPN
001200060920     F                                     RENAME(TITAS000:TAS31000)
001300990812     F                                     RENAME(TITAS010:TAS31010)
001400060920     F                                     IGNORE(TITASp00)
001500990812     FTITAS38C  IF   E           K DISK    USROPN
001600060920     F                                     RENAME(TITAS000:TAS38000)
001700060920     F                                     RENAME(TITAS010:TAS38010)
001800060920     F                                     IGNORE(TITASp00)
001900970110     F*-----------
002000060315     F* varie
002100970110     F*-----------
002200060315     Ftita730c  IF   E           K DISK
002300060315     F*-----------
002400060315     F* CONTRASSEGNI
002500060315     F*-----------
002600060315     FTNCSB03L  IF   E           K DISK
002700021122     F*-----------
002800021122     F* GIACENZE
002900021122     F*-----------
003000050309     FTIGCP51L  IF   E           K DISK
003100970110     F*-----------
003200970415     F* TARIFFE: ANAGRAFICHE CLIENTI
003300970110     F*-----------
003400970110     FCNACO00F  IF   E           K DISK
003500970110     FCNCLP00F  IF   E           K DISK
003600970110     FCNIND00F  IF   E           K DISK
003700060802     F*-----------
003800060808     FtfCLP00F  IF   E           K DISK    rename(cnclp000:tfclp000)
003900060808     FtfIND00F  IF   E           K DISK    rename(cnind000:tfind000)
004000060808     FTIOFD01L  IF   E           K DISK    rename(titad000:tiofd000)
004100970110     F*-----------
004200970110     F* DATI TIPO
004300970110     F*-----------
004400011001     FTEETC01L  UF A E           K DISK
004500011001     FTEETS01L  UF A E           K DISK
004600060830     FTEETS11L  UF A E           K DISK    rename(teets000:teets100)
004700060830     f                                     prefix(£)
004800141010     FTEETT11L  UF A E           K DISK
004900970110     F*-----------
005000970110     F* ORGANIGRAMMA
005100970110     F*-----------
005200970417     FAZORG01L  IF   E           K DISK
005300970110     F*-----------
005400970110     F* TABELLE
005500970110     F*-----------
005600970417     FTABEL00F  IF   E           K DISK
005700011011     FTNTBE01L  IF   E           K DISK
005800970110     F*-----------
005900970110     F* TARIFFE
006000970110     F*-----------
006100970417     FTNTAM01L  IF   E           K DISK
006200990820     FTITAD04L  IF   E           K DISK
006300040809     Ffiar531c  IF   E           K DISK
006400970110     F*-----------
006500970110     F* PRINTER FILE
006600970110     F*-----------
006700970110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
006800970110     D*------------------------------------------------------------------------*
006900970110     D* SCHIERE
007000970110     D*------------------------------------------------------------------------*
007100970110     D*------------------
007200970110     D* DECODIFICHE
007300970110     D*------------------
007400980821     D DDEC            S             30    DIM(10) CTDATA PERRCD(1)
007500970110     D*------------------
007600970110     D* ERRORI DI ELABORAZIONE
007700970110     D*------------------
007800990812     D MTAS            S             66    DIM(5) CTDATA PERRCD(1)              *MESSAGGI PER TITAS
007900011001     D METX            S             66    DIM(5) CTDATA PERRCD(1)              *MESSAGGI PER TEETX
008000970110     D*------------------
008100970110     D* RIGHE DI STAMPA
008200970110     D*------------------
008300970110     D STA             S             66    DIM(5) CTDATA PERRCD(1)
008400970110     D*------------------
008500970110     D* ORGANIGRAMMA
008600970110     D*------------------
008700971124     D ORG             S              3  0 DIM(500)                             *CODICE
008800971124     D DORG            S             20    DIM(500)                             *DESCRIZIONE
008900971204     D FL1             S              1    DIM(500)                             *FLAG ITALIA/ESTERO
009000970110     D*------------------
009100970110     D* SELEZIONI
009200970110     D*------------------
009300971124     D SORG            S              3  0 DIM(500)                             *FILIALI
009400971204     D SFL1            S              1    DIM(500)                             *FLAG ITALIA/ESTERO
009500970423     D SFIE            S              1    DIM(2)                               *ITALIA/ESTERO
009600140919     D Sntw            S              3    DIM(500)                             *FLAG ITALIA/ESTERO
009700970110     D*-------------------
009800970110     D* TIPI TARIFFA
009900970110     D*-------------------
010000970110     D TTR             S              1    DIM(10)                              *CODICE
010100970110     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
010200970110     D*-------------------
010300970110     D* RAGGRUPPAMENTO TIPI BOLLA
010400970110     D*-------------------
010500970110     D RBL             S              1    DIM(5)                               *RAGGRUPPAMENTO BOLL
010600970110     D FSP             S              1    DIM(5)                               *FLAG SPEDIZIONE
010700970110     D FRI             S              1    DIM(5)                               *FLAG RICAVO
010800970110     D*------------------
010900970110     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA
011000970110     D*------------------
011100970110     D TBL             S              2    DIM(20)                              *TIP0 BOLLA
011200970110     D SPD             S              1    DIM(20)                              *FLAG SPEDIZIONE
011300970110     D RIC             S              1    DIM(20)                              *FLAG RICAVO
011400970110     D*------------------
011500970110     D* TABELLE CAPICONTO
011600970110     D*------------------
011700970110     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
011800970110     D*------------------
011900970110     D* RATE DI PAGAMENTO
012000970110     D*------------------
012100970110     D RAT             S              3    DIM(10)
012200970110     D*------------------
012300970110     D* CODICI PAGAMENTO
012400970110     D*------------------
012500970110     D CDP             S              4    DIM(200)                             *CODICE PAGAMENTO
012600970110     D CRAT            S             30    DIM(200)                             *RATE DI PAGAMENTO
012700040920     D*------------------
012800040920     D* scaglioni standard
012900040920     D*------------------
013000040920     D sgl             S             13  3 DIM(10)                              *CODICI ELEMENTARI
013100970110     D*------------------
013200970110     D* CODICI TASSAZIONE
013300970110     D*------------------
013400050103     D CTS             S              2    DIM(600)                             *CODICI ELEMENTARI
013500140919     D naz             S              3    DIM(600)                             *CODICI ELEMENTARI
013600050103     D FCR             S              1    DIM(600)                             *FLAG CAPOLUOGO
013700050103     D DCTS            S             20    DIM(600)                             *DESCRIZIONE
013800050103     D CIE             S              1    DIM(600)                             *FLAG ITALIA/ESTERO
013900050103     D RAP             S              2    DIM(600)                             *CODICE REGIONE
014000050103     D NAR             S              2    DIM(600)                             *CODICE NAZIONE -> P
014100050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)   -> P
014200050808     D*------------------
014300050808     D* cod. trattamento merce con packing list
014400050808     D*------------------
014500050808     D Cpkl            S              2    DIM(600)                             *CODICI ELEMENTARI
014600970314     D*------------------
014700970314     D* DATI TARIFFE
014800970314     D*------------------
014900971124     D* MULTIPLE                                         -500 FILIALI-
015000970110     D*-------------------
015100970110     D* SCHIERE DI MEMORIZZAZIONE
015200970110     D*-------------------
015300970110     D* NON MULTIPLE
015400060315     D SVN             S              1    DIM(20)
015500060315     D VAN             S             11  3 DIM(20)
015600971124     D SPE             S              9  0 DIM(500)                             *N°SPEDIZIONI ITA/ES
015700971124     D CLA             S              9  0 DIM(500)                             *CLIENTE TIPO A
015800971124     D CLB             S              9  0 DIM(500)                             *CLIENTE TIPO B
015900971124     D CLC             S              9  0 DIM(500)                             *CLIENTE TIPO C
015901160726     D CLE             S              9  0 DIM(500)                             *CLIENTE TIPO C
016000971124     D BON             S              9  0 DIM(500)                             *BOLLETTAZIONE (' ')
016100971124     D BOA             S              9  0 DIM(500)                             *BOLLETTAZIONE (A)
016200971124     D BOB             S              9  0 DIM(500)                             *BOLLETTAZIONE (B)
016300971124     D NCL             S              9  0 DIM(500)                             *COLLI
016400971124     D PKB             S              9  1 DIM(500)                             *PESO
016500971124     D VLB             S              9  3 DIM(500)                             *VOLUME
016600060829     D NCL1            S              9  0 DIM(500)                             *COLLI
016700060829     D PKB1            S              9  1 DIM(500)                             *PESO
016800060829     D VLB1            S              9  3 DIM(500)                             *VOLUME
016900971124     D FRA             S              9  0 DIM(500)                             *FRANCHI
017000971124     D ASS             S              9  0 DIM(500)                             *ASSEGNATI
017100971124     D PCV             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
017200971124     D PCM             S              9  0 DIM(500)                             *CONSEGNE MAGAZZINO
017300971124     D PFD             S              9  0 DIM(500)                             *FERMO DEPOSITO
017400971124     D ESP             S              9  0 DIM(500)                             *ESPRESSI
017500971124     D PPV             S              9  0 DIM(500)                             *CONSEGNE PROVINCIA
017600971124     D PCA             S              9  0 DIM(500)                             *CONSEGNE APPUNTAMEN
017700971124     D PCP             S              9  0 DIM(500)                             *CONSEGNE PIANI
017800971124     D PCS             S              9  0 DIM(500)                             *CONSEGNE SUPERMERCA
017900971124     D PCF             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
018000971124     D PAB             S              9  0 DIM(500)                             *C/ASSEGNI BARTOLINI
018100971124     D PAM             S              9  0 DIM(500)                             *C/ASSEGNI MITTENTE
018200011026     D CAS             S             21  8 DIM(500)                             *C/ASSEGNO
018300971124     D PAR             S              9  0 DIM(500)                             *ASSICURAZIONE R.C.V
018400971124     D PAC             S              9  0 DIM(500)                             *ASSICURAZIONE X CON
018500060315     D xx1             S              9  0 DIM(500)                             *ASSICURAZIONE X CON
018600011026     D ASC             S             21  8 DIM(500)                             *ASSICURAZIONE
018700971124     D PGI             S              9  0 DIM(500)                             *GIACENZE
018800971124     D PCD             S              9  0 DIM(500)                             *CONSEGNE DDT
018900971124     D PCC             S              9  0 DIM(500)                             *CONSEGNE C.STORICI
019000971124     D PCL             S              9  0 DIM(500)                             *CONSEGNE L.DISAGIAT
019100980901     D CLT             S              9  0 DIM(500)                             *CLIENTE TIPO T
019200000414     D GGS             S              9  0 DIM(500)                             *N° GG SOSTA GIACENZ
019300000414     D NRG             S              9  0 DIM(500)                             *N° RIAPERTURE GIACE
019400011011     D CIS             S              9  0 DIM(500)                             *CONSEGNE ISOLE
019500011011     D RIS             S              9  0 DIM(500)                             *RITIRI ISOLE
019600020215     D CLD             S              9  0 DIM(500)                             *CLIENTE TIPO D
019700040802     D ded             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
019800040802     D val             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
019900040811     D sbnb            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020000040824     D sban            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020001160726     D sb01            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020002160726     D sb02            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020003160726     D sb03            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020100040811     D nbn             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020200040802     D nba             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020201160726     D nb01            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020202160726     D nb02            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020203160726     D nb03            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020300050808     D pks             S              9  0 DIM(500)                             *CLIENTE TIPO D
020400050808     D pkn             S              9  0 DIM(500)                             *CLIENTE TIPO D
020401160726     d detcflo       e ds
020500040809     d dar5ban       e ds
020600040811     d dar5bnb       e ds
020700060829     d teets1        e ds                  extname(teets10f) prefix(£)
020800970417     D*-------------------
020900970417     D* SCHIERE DI LAVORO
021000970417     D*-------------------
021100050103     D WCTS            S              2    DIM(600)                             *CODICI
021200050103     D WRAP            S              2    DIM(600)                             *CODICE REGIONE
021300050103     D WNAR            S              2    DIM(600)                             *CODICE NAZIONE -> P
021400050103     D WTCT            S              1    DIM(600)                             *TIPO (E/R/N)   -> P
021500970416     D*-------------------
021600970416     D* SCHIERE DI PASSAGGIO PER LA SCRITTURA
021700970416     D*-------------------
021800970416     D* NON MULTIPLE
021900970416     D §SG             S             13  3 DIM(10)                              *SCAGLIONE -KG/COLLI
022000060829     c* ricavi
022100970416     D §PS             S              7  1 DIM(10)                              *PESO
022200970416     D §CS             S              7  1 DIM(10)                              *COLLI
022300970416     D §VS             S              7  3 DIM(10)                              *VOLUME
022400970416     D §SP             S              4  1 DIM(10)                              *% INCIDENZA
022500971008     D §SD             S              1    DIM(10)                              *FLAG VALORE DEDOTTO
022600060829     c* competenze
022700060829     D §PS1            S              7  1 DIM(10)                              *PESO
022800060829     D §CS1            S              7  1 DIM(10)                              *COLLI
022900060829     D §VS1            S              7  3 DIM(10)                              *VOLUME
023000060829     D §SP1            S              4  1 DIM(10)                              *% INCIDENZA
023100060829     D §SD1            S              1    DIM(10)                              *FLAG VALORE DEDOTTO
023200970416     D*---
023300050103     D §CT             S              2    DIM(600)                             *CODICE TASSAZIONE
023400050103     D §RC             S              1    DIM(600)                             *RAGGRUPPAMENTO
023500050103     D §CP             S              4  1 DIM(600)                             *% INCIDENZA
023600050103     D §CD             S              1    DIM(600)                             *FLAG VALORE DEDOTTO
023700020227     D*-------------------
023800020227     D* SCHIERA FILIALI FEDEX
023900020227     D*-------------------
024000020227     D FILFED          S              3  0 DIM(999)
024100081105     D*-------------------
024200081105     D* SCHIERA FILIALI DPD
024300081105     D*-------------------
024400081105     D FILdpd          S              3  0 DIM(999)
024500970110     D*------------------------------------------------------------------------*
024600970110     D* INPUT
024700970110     D*------------------------------------------------------------------------*
024800970110     D*------------------
024900970110     D* DS SCHIERE MULTIPLE
025000970110     D*------------------
025100970110     D*---
025200970110     D* BYTE IN PACKED ->
025300970317     D* (3:2) +1=2 ; (2X30) =60
025400970317     D* (9:2) +1=5 ; (5X30) =150
025500970317     D* (9:2) +1=5 ; (5X999)=4995
025600970317     D* (9:2) +1=5 ; (5X10) =50
025700050103     D* (9:2) +1=5 ; (5X600)=1500
025800970317     D* (13:2)+1=7 ; (7X10) =70
025900011001     D* (18:2)+1=10; (10X10) =100
026000141002     D FPAG            DS                  OCCURS(500) INZ
026100141002     D  GGP                    1     60P 0
026200141002     D                                     DIM(30)                              *GG PAGAMENTO
026300141002     D  SGP                   61    210P 0
026400141002     D                                     DIM(30)                              *SPED. GG PAGAMENTO
026500971124     D FRIT            DS                  OCCURS(500) INZ
026600970110     D  RIT                    1   4995P 0
026700970110     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
026800971124     D FCON            DS                  OCCURS(500) INZ
026900970110     D  CON                    1   4995P 0
027000970110     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
027100971124     D FSGL            DS                  OCCURS(500) INZ
027200041118     D  SSP                    1     50P 0
027300041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
027400041118     D  PSC                   51    110P 1
027500041118     D                                     DIM(10)                              *PESO
027600041118     D  CSC                  111    160P 0
027700041118     D                                     DIM(10)                              *COLLI
027800041118     D  VSC                  161    220P 3
027900041118     D                                     DIM(10)                              *VOLUME
028000060829     D FSGL1           DS                  OCCURS(500) INZ
028100060829     D  SSP1                   1     50P 0
028200060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
028300060829     D  PSC1                  51    110P 1
028400060829     D                                     DIM(10)                              *PESO
028500060829     D  CSC1                 111    160P 0
028600060829     D                                     DIM(10)                              *COLLI
028700060829     D  VSC1                 161    220P 3
028800060829     D                                     DIM(10)                              *VOLUME
028900971124     D FCTS            DS                  OCCURS(500) INZ
029000050103     D  TSI                    1   3000P 0
029100050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
029200050103     D  TSE                 3001   6000P 0
029300050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
029400971124     D FTAR            DS                  OCCURS(500) INZ
029500970423     D  SSGL                   1     70P 3
029600970314     D                                     DIM(10)                              *SCAGLIONI
029700050103     D  CCTI                  71   1270
029800050103     D                                     DIM(600)                             *COD.TASSAZIONE ITAL
029900050103     D  CRCI                1271   1870
030000050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ITAL
030100050103     D  CCTE                1871   3070
030200050103     D                                     DIM(600)                             *COD.TASSAZIONE ESTE
030300050103     D  CRCE                3071   3670
030400050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ESTE
030500980820     D*---
030600980820     D* CARTELLO
030700980820     D*---
030800980824     D CCAR            DS                  OCCURS(2) INZ
030900980821     D  USPE                   1      5P 0
031000980821     D                                     DIM(1)                               *N°SPEDIZIONI ITA/ES
031100980821     D  UCLA                   6     10P 0
031200980821     D                                     DIM(1)                               *CLIENTE TIPO A
031300980821     D  UCLB                  11     15P 0
031400980821     D                                     DIM(1)                               *CLIENTE TIPO B
031500980821     D  UCLC                  16     20P 0
031600980821     D                                     DIM(1)                               *CLIENTE TIPO C
031700980821     D  UBON                  21     25P 0
031800980821     D                                     DIM(1)                               *BOLLETTAZIONE (' ')
031900980821     D  UBOA                  26     30P 0
032000980821     D                                     DIM(1)                               *BOLLETTAZIONE (A)
032100980821     D  UBOB                  31     35P 0
032200980821     D                                     DIM(1)                               *BOLLETTAZIONE (B)
032300980821     D  UNCL                  36     40P 0
032400980821     D                                     DIM(1)                               *COLLI
032500980821     D  UPKB                  41     45P 1
032600980821     D                                     DIM(1)                               *PESO
032700980821     D  UVLB                  46     50P 3
032800980821     D                                     DIM(1)                               *VOLUME
032900980821     D  UFRA                  51     55P 0
033000980821     D                                     DIM(1)                               *FRANCHI
033100980821     D  UASS                  56     60P 0
033200980821     D                                     DIM(1)                               *ASSEGNATI
033300980821     D  UPCV                  61     65P 0
033400980821     D                                     DIM(1)                               *CONSEGNE PRIVATI
033500980821     D  UPCM                  66     70P 0
033600980821     D                                     DIM(1)                               *CONSEGNE MAGAZZINO
033700980821     D  UPFD                  71     75P 0
033800980821     D                                     DIM(1)                               *FERMO DEPOSITO
033900980821     D  UESP                  76     80P 0
034000980821     D                                     DIM(1)                               *ESPRESSI
034100980821     D  UPPV                  81     85P 0
034200980821     D                                     DIM(1)                               *CONSEGNE PROVINCIA
034300980821     D  UPCA                  86     90P 0
034400980821     D                                     DIM(1)                               *CONSEGNE APPUNTAMEN
034500980821     D  UPCP                  91     95P 0
034600980821     D                                     DIM(1)                               *CONSEGNE PIANI
034700980821     D  UPCS                  96    100P 0
034800980821     D                                     DIM(1)                               *CONSEGNE SUPERMERCA
034900980821     D  UPCF                 101    105P 0
035000980821     D                                     DIM(1)                               *CONSEGNE FUORI MISU
035100980821     D  UPAB                 106    110P 0
035200980821     D                                     DIM(1)                               *C/ASSEGNI BARTOLINI
035300980821     D  UPAM                 111    115P 0
035400980821     D                                     DIM(1)                               *C/ASSEGNI MITTENTE
035500011002     D  UCAS                 116    125P 5
035600011002     D                                     DIM(1)                               *C/ASSEGNO
035700011002     D                                     PACKEVEN
035800011002     D  UPAR                 126    130P 0
035900980821     D                                     DIM(1)                               *ASSICURAZIONE R.C.V
036000011002     D  UPAC                 131    135P 0
036100980821     D                                     DIM(1)                               *ASSICURAZIONE X CON
036200011002     D  UASC                 136    145P 5
036300011001     D                                     DIM(1)                               *ASSICURAZIONE
036400011001     D                                     PACKEVEN
036500011002     D  UPGI                 146    150P 0
036600980821     D                                     DIM(1)                               *GIACENZE
036700011002     D  UPCD                 151    155P 0
036800980821     D                                     DIM(1)                               *CONSEGNE DDT
036900011002     D  UPCC                 156    160P 0
037000980821     D                                     DIM(1)                               *CONSEGNE C.STORICI
037100011002     D  UPCL                 161    165P 0
037200980821     D                                     DIM(1)                               *CONSEGNE L.DISAGIAT
037300011002     D  UCLT                 166    170P 0
037400980901     D                                     DIM(1)                               *CLIENTE TIPO T
037500011002     D  UGGS                 171    175P 0
037600000414     D                                     DIM(1)                               *GG SOSTA GIACENZA
037700011002     D  UNRG                 176    180P 0
037800000414     D                                     DIM(1)                               *N° RIAPERTURE GIACE
037900011011     D  UCIS                 181    185P 0
038000011011     D                                     DIM(1)                               *CONSEGNE ISOLE
038100011011     D  URIS                 186    190P 0
038200011011     D                                     DIM(1)                               *RITIRI ISOLE
038300020215     D  UCLD                 191    195P 0
038400020215     D                                     DIM(1)                               *CLIENTE TIPO D
038500040802     D  Uded                 196    200P 0
038600040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
038700040811     D  Usbnb                206    210P 0
038800040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
038900040824     D  Usban                211    215P 0
039000040824     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039100040824     D  Unbn                 216    220P 0
039200040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039300040824     D  Unba                 221    225P 0
039400040811     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039500040824     D  Uval                 226    230P 0
039600040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039700050808     D  Upks                 231    235P 0
039800050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039900050808     D  Upkn                 236    240P 0
040000050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040100060315     D  Uxx1                 241    245P 0
040200060315     D                                     DIM(1)                               *ASSICURAZIONE X CON
040300060829     D  UNCL1                246    250P 0
040400060829     D                                     DIM(1)                               *COLLI
040500060829     D  UPKB1                251    255P 1
040600060829     D                                     DIM(1)                               *PESO
040700060829     D  UVLB1                256    260P 3
040800060829     D                                     DIM(1)                               *VOLUME
040801160726     D  Usb01                261    265P 0
040802160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040803160726     D  Usb02                266    270P 0
040804160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040805160726     D  Usb03                271    275P 0
040806160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040807160726     D  Unb01                276    280P 0
040808160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040809160726     D  Unb02                281    285P 0
040810160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040811160726     D  Unb03                286    290P 0
040812160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040813160726     D  Urtm                 291    295P 0
040814160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040815160726     D  UCLE                 296    300P 0
040816160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040900980824     D CPAG            DS                  OCCURS(2) INZ
041000980820     D  UGGP                   1     60P 0
041100980819     D                                     DIM(30)                              *GG PAGAMENTO
041200980820     D  USGP                  61    210P 0
041300980819     D                                     DIM(30)                              *SPED. GG PAGAMENTO
041400980824     D CRIT            DS                  OCCURS(2) INZ
041500980820     D  URIT                   1   4995P 0
041600980819     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
041700980824     D CCON            DS                  OCCURS(2) INZ
041800980820     D  UCON                   1   4995P 0
041900980819     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
042000980824     D CSGL            DS                  OCCURS(2) INZ
042100041118     D  USSP                   1     50P 0
042200041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
042300041118     D  UPSC                  51    110P 1
042400041118     D                                     DIM(10)                              *PESO
042500041118     D  UCSC                 111    160P 0
042600041118     D                                     DIM(10)                              *COLLI
042700041118     D  UVSC                 161    220P 3
042800041118     D                                     DIM(10)                              *VOLUME
042900060829     D CSGL1           DS                  OCCURS(2) INZ
043000060829     D  USSP1                  1     50P 0
043100060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
043200060829     D  UPSC1                 51    110P 1
043300060829     D                                     DIM(10)                              *PESO
043400060829     D  UCSC1                111    160P 0
043500060829     D                                     DIM(10)                              *COLLI
043600060829     D  UVSC1                161    220P 3
043700060829     D                                     DIM(10)                              *VOLUME
043800980824     D CCTS            DS                  OCCURS(2) INZ
043900050103     D  UTSI                   1   3000P 0
044000050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
044100050103     D  UTSE                3001   6000P 0
044200050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
044300040803     D Dtasflo       E DS
044400091125     D tnte04ds      E DS
044500970110     D*-------------------
044600970110     D* DS TIPI TARIFFA
044700970110     D*-------------------
044800970110     D DSTR          E DS
044900050808     D*-------------------
045000141002     D* DS nazioni
045100050808     D*-------------------
045200141002     D DS15          E DS
045300141002     D d15             S              3    DIM(1000)
045400141002     D ecf             S              1  0 DIM(1000)
045500141002     D pcf15           S              1  0 DIM(1000)
045600141002     D wnaz            S                   like(tasnzd)
045700141002     D wcad            S                   like(tascad)
045800141002     D*-------------------
045900141002     D* DS cod. trattamento merce
046000141002     D*-------------------
046100141009     D azcpcds       E DS                  extname(azcpc00f)
046200141009     D*---------------------
046300141009     D* DS "TISI95R" - CONTROLLA CAP
046400141009     D*---------------------
046500141009     D DSSI95        E DS                  EXTNAME(TISI95DS)
046600141009     D*-------------------
046700141009     D* DS cod. trattamento merce
046800141009     D*-------------------
046900141009     D DS1b          E DS
047000021202     D*-------------------
047100021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
047200021202     D*-------------------
047300160317     D TRULC7DS      E DS
047400970110     D*-------------------
047500970110     D* DS CODICI TASSAZIONE
047600970110     D*-------------------
047700970110     D DSCT          E DS
047800970110     D*-------------------
047900970110     D* DS CAMBI E DIVISE
048000970110     D*-------------------
048100011011     D*DSCV          E DS
048200970110     D*-------------------
048300970110     D* DS RAGGRUPPAMENTI TIPI BOLLA
048400970110     D*-------------------
048500970110     D DS2W          E DS
048600970110     D*------------------
048700970110     D* DS TIPI BOLLA
048800970110     D*------------------
048900970110     D DSTB          E DS
049000970110     D*------------------
049100970110     D* DS CODICI PAGAMENTO
049200970110     D*------------------
049300970110     D DSFA          E DS
049400970110     D*------------------
049500970110     D* DS COMPOSIZIONE CODICE DI PAGAMENTO
049600970110     D*------------------
049700970110     D                 DS
049800970110     D  WCDP                   1      3
049900970110     D  WLIN                   4      4
050000970110     D  DSCDP                  1      4
050100970110     D*------------------
050200970110     D* DS INVIO DATI -BOLLETTAZIONE-
050300970110     D*------------------
050400970110     D DS3C          E DS
050500970110     D*------------------
050600020923     D* DS REPERIMENTO DATI UTENTE
050700970110     D*------------------
050800020923     D TIBS34DS      E DS                                                       *Profili utente
050900020923     D DDATIUTE      E DS                                                       *Dati utente
051000020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
051100970110     D*------------------
051200970110     D* DS "XSRDA8" - CONTROLLA DATA (8)
051300970110     D*------------------
051400970110     D WLBDA8          DS
051500970110     D  G08DAT                 1      8  0
051600970110     D  G08INV                 9     16  0
051700970110     D  G08ERR                17     17
051800970110     D  G08TGI                18     22  0
051900970110     D*-------------------
052000970110     D* DS SCOMPOSIZIONE DATA DECORRENZA(4) -TNTAM-
052100970110     D*-------------------
052200970110     D                 DS
052300970110     D  TAMDNN                 3      4  0
052400970110     D  TAMDMM                 5      6  0
052500970110     D  TAMDEC                 3      6  0
052600970110     D  TAMDDT                 1      8  0
052700970110     D*-------------------
052800970110     D* DS SCOMPOSIZIONE DATA SCADENZA(4) -TNTAM-
052900970110     D*-------------------
053000970110     D                 DS
053100970110     D  TAMSNN                 3      4  0
053200970110     D  TAMSMM                 5      6  0
053300970110     D  TAMSCA                 3      6  0
053400970110     D  TAMDST                 1      8  0
053500970110     D*-------------------
053600990812     D* DS COMPOSIZIONE DATA SPEDIZIONE -TITAS-
053700970110     D*-------------------
053800970110     D                 DS
053900970110     D  TASAAS                 1      4  0
054000970110     D  TASMGS                 5      8  0
054100970110     D  TASDSP                 1      8  0
054200970110     D*------------------
054300970110     D* DS PARAMETRI DI LANCIO
054400970110     D*------------------
054500970110     D PARAM           DS
054600980825     D  PARKSC                 1      7  0
054700980825     D  PARFPA                 8     10  0
054800980825     D  PARFAR                11     13  0
054900980825     D  PARFAP                14     16  0
055000980825     D  PARDS1                17     24  0
055100980825     D  PARAA1                17     20  0
055200980825     D  PARMG1                21     24  0
055300980825     D  PARDS2                25     32  0
055400980825     D  PARTAR                33     35
055500980825     D  PARCTO                36     36
055600980825     D  PARKOV                37     43  0
055700980825     D  PARCAR                37     41  0
055800980825     D  PARCTR                44     46  0
055900980825     D  PARTTR                44     44
056000980825     D  PARPRG                47     49  0
056100980825     D  PARLNP                50     52  0
056200980825     D  PARFIE                53     53
056300980825     D  PARSIM                54     54
056400980825     D  PARELA                55     55
056500011003     D  PARDIV                56     58
056600021031     D  PARUNI                59     59
056700040804     D  PARsta                60     60
056800040805     D  PARint                61     61
056900040805     D  PARsie                62     62
057000091130     D  PARdsf                63     63
057100970110     D*------------------
057200970110     D* ARCHITETTURA
057300970110     D*------------------
057400970110     D KPJBA         E DS
057500970110     D  PROFIL                 1     10
057600011011     D*------------------
057700011011     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
057800011011     D*------------------
057900011011     D DSGEDCE       E DS
058000011011     D*-------------------
058100011011     D* CONVERSIONI IMPORTI
058200011011     D*-------------------
058300011011     D YEUR          E DS                  EXTNAME(YEURCODS)
058400020227     D*-------------------
058500020227     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
058600020227     D*-------------------
058700020227     D OG143         E DS
058800020730     D*------------------
058900020730     D* DS PER LETTURA TABELLA "NTW"
059000020730     D*------------------
059100020730     D DNTW          E DS
059200021121     D*------------------
059300021121     D* DS RIDEFINIZIONE CAMPO TAMFLO DI TNTAM
059400021121     D*------------------
059500021121     D DSTA01        E DS
059600021031     D*-------------------
059700021031     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
059800021031     D*-------------------
059900021031     D TIBS10DS      E DS
060000021031     D  SKCLI                 21   5520  0 DIM(500)                              -cli. padri/figli
060100040811     D*-------------------
060200040811     D* CAMPI DI LAVORO
060300040811     D*-------------------
060400141010     D teett10ds     E DS                  EXTNAME(teett10f)
060500141010     d £inc            s                   like(ettinc)
060600141010     d £nsp            s                   like(ettnsp)
060700141010     d £cts            s                   like(ettcts)
060800141010     d £ntw            s                   like(ettntw)
060900140922     d colli           s                   like(tasncl)
061000060829     d colli1          s                   like(tasncl)
061100040811     d $pes            s                   like(taspkf)
061200060829     d $pes1           s                   like(taspkf)
061300060829     d etcpmc1         s                   like(etcpmc)
061400060829     d etcvmc1         s                   like(etcvmc)
061500970110     C*------------------------------------------------------------------------*
061600970110     C* MAIN LINE
061700970110     C*------------------------------------------------------------------------*
061800970313     C*
061900970313     C* SE NESSUN ERRORE IN APERTURA
062000970313IF  1C     $FINE         IFEQ      '0'
062100970313     C*
062200970314     C* ANNULLA I DATI TIPO ESISTENTI
062300970314     C                   EXSR      ANNETX
062400970110     C* APRE GLI ARCHIVI
062500970110     C                   EXSR      OPEFIL
062600970415     C*
062700970416     C* LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
062800970416     C                   EXSR      LETCLI
062900970416     C*
063000970416     C* REPERISCE LA QUALITA' DEL CLIENTE TARIFFA / OFFERTA
063100970416     C                   MOVEL     'C'           $CLV              1
063200970416IF  1C     CLPCLV        IFNE      *BLANKS
063300970416     C                   MOVEL     CLPCLV        $CLV
063400970416E   1C                   ENDIF
063500970416     C*
063600970416     C* REPERISCE I GIORNI DI PAGAMENTO DEL CLIENTE / VISITA
063700970416     C                   Z-ADD     *ZEROS        $GGP              3 0
063800970416     C                   EXSR      REPGGP
063900021129     C*
064000021129     C* CICLO SULLA SCHIERA DEI CLIENTI DA ELABORARE (DISCORSO UNIFICANTE)
064100021129     C     1             DO        500           JJ                3 0
064200021129     C                   Z-ADD     SKCLI(JJ)     KTAKSC                         *CLIENTE
064300970110     C*
064400970110     C* POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
064500970110     C                   EXSR      SETFIL
064600970110     C*
064700970110     C* CICLO FINO A FINE -BOLLE-
064800970313DO  2C     $FINE         DOWEQ     '0'
064900970110     C*
065000970110     C* MEMORIZZA I DATI
065100970110     C                   EXSR      MEMDAT
065200970110     C*
065300970110     C* LETTURA SUCESSIVA -BOLLE-
065400970110     C                   EXSR      LETFIL
065500970313E   2C                   ENDDO                                                  *FINE BOLLE
065600141002     c*
065700021129     C* DOPO L'ELABORAZIONE DI OGNI CLIENTE RE-INIZIALIZZO IL FLAG DI "FINE"
065800021129     C                   MOVEL     '0'           $FINE
065900021129     C*
066000021129     C* TESTO LE CONDIZIONI X CUI USCIRE DAL CICLO DI MEMORIZZAZIONE
066100021129     C                   IF        SKCLI(JJ) = *ZEROS
066200021129     C                   MOVEL     '1'           $FINE
066300021129     C                   LEAVE
066400021129     C                   ENDIF
066500021129     C*
066600021129     C                   ENDDO
066700970110     C*
066800970110     C* SCRIVE -SE RICHIESTO- E STAMPA I DATI TIPO
066900970110     C                   EXSR      AGGETX
067000980818     C*
067100980818     C* SCRIVE E STAMPA I DATI TIPO DI CARTELLO -SE RICHIESTE TUTTE LE LINE DI PARTENZA-
067200980818IF  2C     PARCTO        IFEQ      'T'
067300980818     C     PARCAR        ANDEQ     88888
067400980818     C     PARLNP        ANDEQ     999
067500980818     C                   EXSR      AGGET8
067600980818E   2C                   ENDIF
067700970110     C*
067800970110     C* CHIUDE GLI ARCHIVI
067900970110     C                   EXSR      CLOFIL
068000970313E   1C                   ENDIF
068100040804     c* lancio la stampa se richiesto
068200040804     c                   if        parsta = 'S'
068300040804     c                   call      'TNTE49R'
068400040804     c                   parm                    kpjba
068500040804     c                   end
068600970110     C*
068700970110     C                   SETON                                        LR
068800970415     C*------------------------------------------------------------------------*
068900970416     C* LETCLI - LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
069000970415     C*------------------------------------------------------------------------*
069100970416     C     LETCLI        BEGSR
069200970415     C*
069300060808     C                   CLEAR                   CNCLP000                       *AZZERA RECORD
069400060808     C                   CLEAR                   CNIND000                       *AZZERA RECORD
069500060808     C                   CLEAR                   tfCLP000                       *AZZERA RECORD
069600060808     C                   CLEAR                   tfIND000                       *AZZERA RECORD
069700020923     C                   Z-ADD     1             KCLKUT                         *PARTICOLARITA'
069800020923     C                   Z-ADD     DUTKCI        KCLKCC
069900970415     C                   Z-ADD     PARKOV        KCLKSC
070000020923     C                   Z-ADD     1             KINKUT                         *INDIRIZZO
070100020923     C                   Z-ADD     DUTKCI        KINKCC
070200970415     C                   Z-ADD     PARKOV        KINKSC
070300060808IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
070400060808     C     KEYCLP        CHAIN     CNCLP00F                           99
070500060808     C     KEYIND        CHAIN     CNIND00F                           99
070600060808X   1C                   ELSE                                                   *OFFERTA
070700060804     C     KEYCLP        CHAIN     TFCLP00F                           99
070800060804     C     KEYIND        CHAIN     TFIND00F                           99
070900060804IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATO
071000091125     c                   clear                   tnte04ds
071100091130     C                   movel     pardsf        d04dsf
071200091125     C                   Z-ADD     parkov        d04nrv
071300091125     c                   call      'TNTE04R'
071400091125     c                   parm                    kpjba
071500091126     c                   parm                    tnte04ds
071600091125     C                   Z-ADD     d04KSC        KCLKSC                         *CLIENTE DELLA VISIT
071700091125     C                   Z-ADD     d04KSC        KINKSC                         *CLIENTE DELLA VISIT
071800060808     C     KEYCLP        CHAIN     CNCLP00F                           99
071900060808     C     KEYIND        CHAIN     CNIND00F                           99
072000060804     c                   end
072100060808     c                   end
072200970415     C*
072300970415     C                   ENDSR
072400970415     C*------------------------------------------------------------------------*
072500970415     C* REPGGP - REPERISCE IL NUMERO DEI GIORNI DI PAGAMENTO
072600970415     C*------------------------------------------------------------------------*
072700970415     C     REPGGP        BEGSR
072800970415     C*---
072900970415     C* IL GG DI PAGAMENTO IN TABELLA POSSONO ESSERE ESPRESSI IN MESI SE <=12 O
073000970415     C* IN GIORNI SE >12;
073100970415     C* CI POSSONO ESSERE INOLTRE + RATE DI PAGAMENTO E IN QUESTO CASO PRENDE
073200970415     C* SEMPRE L'ULTIMA (ES. SE 30/60/90 PRENDE 90 COME GGP).
073300970415     C* IL PAGAMENTO A VISTA NON HA GIORNI, MA PER COMODITA' SI MEMORIZZA CON
073400970415     C* CON GIORNI DI PAGAMENTO = 999
073500970415     C*---
073600970415     C                   MOVE      INDCDP        WCDP                           *CONDIZIONE PAGAMENT
073700000323     C                   MOVEL     '1'           WLIN                           *LINGUA
073800970415     C                   SETOFF                                           98
073900970415     C                   Z-ADD     1             I
074000970415     C     DSCDP         LOOKUP    CDP(I)                                 98
074100970415IF  1C     *IN98         IFEQ      *ON                                          *TROVATO
074200970415     C                   MOVEA     CRAT(I)       RAT
074300970415     C                   Z-ADD     10            I
074400970415DO  2C     I             DOWGT     0
074500970415IF  3C     RAT(I)        IFGT      *ZEROS                                       *P I E N O
074600970415     C                   MOVEL     RAT(I)        WN3               3 0
074700970415IF  4C     WN3           IFGT      12                                           *ESPRESSO IN GIORNI
074800970415     C                   Z-ADD     WN3           $GGP
074900970415X   4C                   ELSE                                                   *ESPRESSO IN MESI
075000970415     C     WN3           MULT      30            $GGP
075100970415E   4C                   ENDIF
075200981020     C                   LEAVE                                                  *ESCE DAL CICLO
075300970415X   3C                   ELSE                                                   *V U O T O
075400970415     C     I             SUB       1             I                              *DECREMENTA INDICE
075500970415E   3C                   ENDIF
075600970415E   2C                   ENDDO
075700970415E   1C                   ENDIF
075800970415     C*
075900970415     C* PAGAMENTO A VISTA (GGP=0), IMPOSTA GGP=999
076000970415IF  1C     $GGP          IFEQ      *ZEROS
076100970415     C                   Z-ADD     999           $GGP
076200970415E   1C                   ENDIF
076300970415     C*
076400970415     C                   ENDSR
076500970110     C*------------------------------------------------------------------------*
076600970110     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
076700970110     C*------------------------------------------------------------------------*
076800970110     C     SETFIL        BEGSR
076900970110     C*
077000021105IF  1C     PARKSC        IFGT      *ZEROS
077100990812     C     K31TAS        SETLL     TITAS31C                           99        *FINE ARCHIVIO
077200970110X   1C                   ELSE
077300970110     C                   Z-ADD     PARAA1        KTAAAS                         *ANNO
077400970110     C                   Z-ADD     PARMG1        KTAMGS                         *MESE/GIORNO
077500990812     C     K38TAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
077600970110E   1C                   ENDIF
077700970110     C*
077800970110IF  1C     *IN99         IFEQ      *ON
077900970110     C                   MOVEL     '1'           $FINE                          *FINE PGM
078000970110X   1C                   ELSE
078100970110     C                   EXSR      LETFIL                                       *LETTURA RECORD
078200970110E   1C                   ENDIF
078300970110     C*
078400970110     C                   ENDSR
078500970110     C*------------------------------------------------------------------------*
078600970110     C* LETFIL - LETTURA PROSSIMO RECORD -BOLLE-
078700970110     C*------------------------------------------------------------------------*
078800970110     C     LETFIL        BEGSR
078900970110     C*
079000970110     C* LETTURA FINO A:
079100970110     C*  - FINE FILE
079200970110     C*  - TROVATO RECORD VALIDO
079300970110     C                   MOVEL     'N'           WRECOK
079400970110DO  1C     $FINE         DOWEQ     '0'
079500970110     C     WRECOK        ANDEQ     'N'
079600970110     C*
079700970110IF  2C     PARKSC        IFGT      *ZEROS
079800990812     C                   READ      TITAS31C                               99
079900970110X   2C                   ELSE
080000990812     C                   READ      TITAS38C                               99
080100970110E   2C                   ENDIF
080200970110     C*
080300970110IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
080400970110     C                   MOVEL     '1'           $FINE
080500970110X   2C                   ELSE
080600970110     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
080700970110E   2C                   ENDIF
080800970110E   1C                   ENDDO
080900970110     C*
081000970110     C                   ENDSR
081100970110     C*------------------------------------------------------------------------*
081200970110     C* CHKREC - CONTROLLA VALIDITA' RECORD -BOLLE-
081300970110     C*------------------------------------------------------------------------*
081400970110     C     CHKREC        BEGSR
081500970110     C*
081600970110     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
081700081105     C*
081800081105     C* se sto elaborando la cartello i dati tipo della cartello escludo le
081900081105     c* le spedizioni con lnp e lna DPD e FED
082000081105IF  2C     PARCTO        IFEQ      'T'
082100081105     C     PARCAR        ANDEQ     88888
082200081105     C     PARLNP        ANDEQ     999
082300081105     c* fedex
082400081105     C     taslnp        LOOKUP    FILFED                                 55
082500081105     C                   IF        *IN55 = *ON
082600081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
082700081105     C                   ENDIF
082800081105     C     taslna        LOOKUP    FILFED                                 55
082900081105     C                   IF        *IN55 = *ON
083000081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083100081105     C                   ENDIF
083200081105     c* dpd
083300081105     C     taslnp        LOOKUP    FILdpd                                 55
083400081105     C                   IF        *IN55 = *ON
083500081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083600081105     C                   ENDIF
083700081105     C     taslna        LOOKUP    FILdpd                                 55
083800081105     C                   IF        *IN55 = *ON
083900081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
084000081105     C                   ENDIF
084100081105E   1C                   ENDIF
084200970423     C*
084300970423     C* TIPO BOLLA NON VALIDO COME SPEDIZIONE O RICAVO, ESCLUDE
084400970416     C                   Z-ADD     1             I
084500970416     C                   SETOFF                                           98
084600970416     C     TASTBL        LOOKUP    TBL(I)                                 98
084700970416IF  1C     *IN98         IFEQ      *ON                                          *TIPO BOLLA TROVATO
084800970416IF  2C     SPD(I)        IFEQ      'S'
084900970416     C     RIC(I)        OREQ      'S'
085000970417     C                   MOVEL     SPD(I)        DEPSP             1            *VALIDA COME SPEDIZ
085100970417     C                   MOVEL     RIC(I)        DEPRI             1            *VALIDA COME RICAVO
085200970416X   2C                   ELSE
085300970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
085400970416E   2C                   ENDIF
085500970416X   1C                   ELSE
085600970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
085700970416E   1C                   ENDIF
085800970423     C*
085900970110     C* CLIENTE <> DA QUELLO RICHIESTO, ESCLUDE ED ESCE DAL PGM
086000970110IF  1C     PARKSC        IFGT      *ZEROS                                       *LETTURA X CLIENTE
086100021031     C* VERIFICO CHE IL CLIENTE CORRENTE SIA NELLA LISTA DI QUELLI DA CONSIDERARE
086200021031     C                   Z-ADD     TASKSC        WRKCLI           11 0
086300021031     C     WRKCLI        LOOKUP    SKCLI                                  60
086400021031IF  2C     *IN60         IFEQ      *OFF
086500021031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086600021105     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
086700021031E   2C                   ENDIF
086800970110E   1C                   ENDIF
086900970110     C*
087000970110     C* DATA SPEDIZIONE > DA DATA FINALE RICHIESTA, ESCLUDE ED ESCE DAL PGM
087100970110IF  1C     PARKSC        IFEQ      *ZEROS                                       *LETTURA X DATA
087200970110     C     PARDS1        ANDGT     *ZEROS
087300970110IF  2C     TASDSP        IFGT      PARDS2
087400970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
087500970110     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
087600970110E   2C                   ENDIF
087700970110E   1C                   ENDIF
087800970110     C*
087900970430     C* FILIALE PARTENZA <> DA QUELLA RICHIESTA, ESCLUDE
088000970430IF  1C     PARFPA        IFGT      *ZEROS
088100970430IF  2C     PARFPA        IFNE      TASLNP
088200970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
088300970110E   2C                   ENDIF
088400970110E   1C                   ENDIF
088500970430     C*
088600970430     C* FILIALE ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
088700970430IF  1C     PARFAR        IFGT      *ZEROS
088800970430IF  2C     PARFAR        IFNE      TASLNA
088900970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
089000970430E   2C                   ENDIF
089100970430E   1C                   ENDIF
089200970430     C*
089300970430     C* FILIALE PARTENZA O ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
089400970430IF  1C     PARFAP        IFGT      *ZEROS
089500970430IF  2C     PARFAP        IFEQ      TASLNP
089600970430     C     PARFAP        OREQ      TASLNA
089700970430X   2C                   ELSE
089800970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
089900970430E   2C                   ENDIF
090000970430E   1C                   ENDIF
090100020227     C*
090200020227     C* QUALUNQUE FILIALE INDICATA NON SIA FEDEX
090300020307     C     PARFPA        IFGT      *ZEROS
090400020227     C     PARFPA        LOOKUP    FILFED                                 55
090500020227     C                   IF        *IN55 = *ON
090600020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090700020227     C                   ENDIF
090800020307     C                   ENDIF
090900020307     C     PARFAR        IFGT      *ZEROS
091000020227     C     PARFAR        LOOKUP    FILFED                                 55
091100020227     C                   IF        *IN55 = *ON
091200020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
091300020227     C                   ENDIF
091400020307     C                   ENDIF
091500020307     C     PARFAP        IFGT      *ZEROS
091600020227     C     PARFAP        LOOKUP    FILFED                                 55
091700020227     C                   IF        *IN55 = *ON
091800020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
091900020227     C                   ENDIF
092000020307     C                   ENDIF
092100970110     C*
092200970110     C* DATA SPEDIZIONE <> DA QUELLE RICHIESTE, ESCLUDE
092300970110IF  1C     PARDS1        IFGT      *ZEROS
092400970110IF  2C     TASDSP        IFGE      PARDS1
092500970110     C     TASDSP        ANDLE     PARDS2
092600970110X   2C                   ELSE
092700970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
092800970110E   2C                   ENDIF
092900970110E   1C                   ENDIF
093000970313     C*
093100970313     C* CODICE TARIFFA <> DA QUELLO RICHIESTO, ESCLUDE
093200970313IF  1C     PARTAR        IFNE      *BLANKS
093300970313     C                   MOVEL     PARTAR        WN3               3 0
093400970313IF  2C     WN3           IFNE      TASCTR
093500970313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
093600970313E   2C                   ENDIF
093700970313E   1C                   ENDIF
093800970430     C*
093900970430     C* RECORD VALIDO, IMPOPSTA LA FILIALE SULLA QUALE MEMORIZZARE I DATI
094000970430IF  1C     WRECOK        IFEQ      'S'
094100970430IF  2C     PARLNP        IFNE      999                                          *SELEZIONE FILIALE
094200970430     C                   Z-ADD     PARLNP        DEPFIL                         *FILIALE=SELEZIONE
094300970430X   2C                   ELSE                                                   *SELEZIONE MULTIPLA
094400970430     C                   Z-ADD     TASLNP        DEPFIL                         *FILIALE=L.PARTENZA
094500970430E   2C                   ENDIF
094600060829     c                   z-add     tasncl        colli
094700060829     c                   z-add     tasncl        colli1
094800040811     c* reperisce i bancali. se BNB imposto nel campo di comodo COLLI
094900040811     c* i colli originali
095000040811     c                   clear                   dar5ban
095100040811     c                   clear                   dar5bnb
095200040811     c                   if        %subst(tasgva: 1: 1) = 'O' or
095300040811     c                             %subst(tasgva: 1: 1) = 'B'
095400040811     c                   if        %subst(tasgva: 1: 1) = 'O'
095500040811     c                   eval      ar5trd = 'BNB'
095600040811     c                   else
095700040811     c                   eval      ar5trd = 'BAN'
095800040811     c                   end
095900060829     c                   exsr      srfiar5
096000040811     c                   if        %subst(tasgva: 1: 1) = 'O'
096100040811     c                   eval      dar5bnb = ar5uni
096200060829     c                   z-add     §ar5bcor      colli
096300040811     c                   else
096400040811     c                   eval      dar5BAN = ar5uni
096500040811     c                   end
096600040811     c                   end
096700040811E   1C                   ENDIF
096800970416     C*
096900970110     C                   ENDSR
097000970110     C*------------------------------------------------------------------------*
097100970110     C* MEMDAT - MEMORIZZA I DATI -BOLLE-
097200970110     C*------------------------------------------------------------------------*
097300970110     C     MEMDAT        BEGSR
097400970110     C*
097500970110     C* RECUPERA L'INDICE DELLA FILIALE
097600970110     C                   Z-ADD     1             F                 4 0
097700970110     C                   SETOFF                                           98
097800970430     C     DEPFIL        LOOKUP    SORG(F)                                98
097900970110IF  1C     *IN98         IFEQ      *ON                                          *FILIALE TROVATA
098000970110     C     F             OCCUR     FPAG
098100970110     C     F             OCCUR     FCON
098200970110     C     F             OCCUR     FRIT
098300970110     C     F             OCCUR     FSGL
098400060829     C     F             OCCUR     FSGL1
098500970110     C     F             OCCUR     FCTS
098600970423     C     F             OCCUR     FTAR
098700970415     C*
098800970415     C* REPERISCE LA SOLUZIONE DEL CLIENTE
098900970415     C                   MOVEL     *BLANKS       $SOL              1
099000970415     C                   EXSR      REPSOL
099100060315     C*
099200060315     C* REPERISCE le varie di fatturazione
099300060315     C                   clear                   svn
099400060315     C                   clear                   van
099500060315     C                   EXSR      REPvrf
099600970415     C*
099700060829     C* REPERISCE IL VOLUME DELLA BOLLA
099800970415     C                   EXSR      REPVOL
099900040811     C*
100000060829     C* REPERISCE IL PESO DELLA BOLLA da
100100040811     C                   EXSR      REPpes
100200970110     C*
100300060315     C* REPERISCE LE consegne particolari
100400970110     C                   MOVEL     *BLANKS       $PCA              1
100500970110     C                   MOVEL     *BLANKS       $PCP              1
100600970110     C                   MOVEL     *BLANKS       $PCS              1
100700970110     C                   MOVEL     *BLANKS       $PCF              1
100800970415     C                   MOVEL     *BLANKS       $PCD              1
100900990812     C                   MOVEL     *BLANKS       $PIN              1            *INOLTRO
101000011026     C                   MOVEL     *ZEROS        $PINIM           21 8          *IMPORTO INOLTRO
101100011011     C                   MOVEL     *BLANKS       $CIS              1
101200011011     C                   MOVEL     *BLANKS       $RIS              1
101300040802     C                   MOVEL     *BLANKS       $ded              1
101400970110     C                   EXSR      REPVAR
101500000414     C*
101600000414     C* REPERISCE LA GIACIENZA
101700000414     C                   MOVEL     *BLANKS       $PGI              1            *ESISTE GIACENZA
101800000414     C                   Z-ADD     *ZEROS        $PGIGS            9 0          *GG SOSTA
101900000414     C                   Z-ADD     *ZEROS        $PGINR            9 0          *N° RIAPERTURE
102000021122     C                   EXSR      REPGIA
102100970110     C*
102200970110     C* REPERISCE IL C/ASSEGNO
102300011026     C                   Z-ADD     *ZEROS        $CAS             21 8
102400970110     C                   MOVEL     *BLANKS       $TPI              1
102500970110IF  2C     TASFCA        IFEQ      'S'                                          *ESISTE C/ASSEGNO
102600970110     C                   EXSR      REPCAS
102700970110E   2C                   ENDIF
102800970110     C*
102900970110     C* REPERISCE L'ASSICURAZIONE
103000011026     C                   Z-ADD     *ZEROS        $IAS             21 8
103100970110     C                   EXSR      REPASS
103200970110     C*
103300970110     C* REPERISCE IL CODICE TASSAZIONE
103400970423     C                   Z-ADD     1             J
103500970423     C                   SETOFF                                           98
103600970423     C     TASCTS        LOOKUP    CTS(J)                                 98
103700970423IF  2C     *IN98         IFEQ      *ON                                          *TROVATO
103800970416     C*---
103900970110     C*  M E M O R I Z Z A
104000970416     C*---
104100970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
104200970423     C*
104300971204IF  4C     FL1(F)        IFNE      *BLANKS                                      *CODICE ESTERO
104400970423     C     CIE(J)        OREQ      'E'
104500970423     C                   Z-ADD     1             I
104600970423     C                   SETOFF                                           98
104700970423     C     TASCTS        LOOKUP    CCTE(I)                                98
104800970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
104900140916     C                   ADD       1             TSE(I)                         *N°SPED.CTS ESTERO
105000970423E   5C                   ENDIF
105100140919     C*
105200970423X   4C                   ELSE                                                   *CODICE ITALIA
105300970423     C                   Z-ADD     1             I
105400970423     C                   SETOFF                                           98
105500970423     C     TASCTS        LOOKUP    CCTI(I)                                98
105600970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
105700970423     C                   ADD       1             TSI(I)                         *N°SPED.CTS ITALIA
105800970423E   5C                   ENDIF
105900970423E   4C                   ENDIF
106000141009     c*
106100141009     c* memorizzo nr sped. x cap e nazione
106200141009     c                   movel     sorg(f)       kttlnp
106300141009     c                   movel     sorg(f)       xorg
106400141009     c                   movel     sntw(f)       kttntw
106500141009     c                   movel     sntw(f)       xntw
106600141009     C                   MOVEL     cie(j)        KTTFIE
106700141009     C                   MOVEL     cie(j)        xfie
106800141002     c                   movel     tascts        kttcts
106900141002     c                   movel     tasnzd        kttnaz
107000141002     C                   MOVEl     tasnzd        Wnaz
107100141009     c                   eval      numspe = 1
107200141009     c                   z-add     1             i
107300141002     c     wnaz          lookup    d15(i)                                 88
107400141002     c                   if        *in88
107500141002     C                   eval      wcad = %subst(tascad: pcf15(i): ecf(i))
107600141002     c                   movel     wcad          kttcad
107700141002     c                   exsr      scrett10
107800141002     c                   end
107900970423     C*
108000970423     C                   ADD       1             SPE(F)                         *N°SPEDIZIONI TOTALI
108100020215     C*
108200020215IF  4C     $CLV          IFEQ      'D'
108300020215     C                   ADD       1             CLD(F)                         *CLIENTE TIPO D
108400020215E   4C                   ENDIF
108500980901IF  4C     $CLV          IFEQ      'T'
108600980901     C                   ADD       1             CLT(F)                         *CLIENTE TIPO T
108700980901E   4C                   ENDIF
108800970416IF  4C     $CLV          IFEQ      'A'
108900970110     C                   ADD       1             CLA(F)                         *CLIENTE TIPO A
109000970416E   4C                   ENDIF
109100970416IF  4C     $CLV          IFEQ      'B'
109200970110     C                   ADD       1             CLB(F)                         *CLIENTE TIPO B
109300970416E   4C                   ENDIF
109400970416IF  4C     $CLV          IFEQ      'C'
109500970415     C     $CLV          OREQ      *BLANKS
109600970110     C                   ADD       1             CLC(F)                         *CLIENTE TIPO C
109700970416E   4C                   ENDIF
109701160726IF  4C     $CLV          IFEQ      'E'
109702160726     C                   ADD       1             CLE(F)                         *CLIENTE TIPO B
109703160726E   4C                   ENDIF
109800970110     C*
109900970416IF  4C     $SOL          IFEQ      *BLANKS
110000970110     C                   ADD       1             BON(F)                         *BOLLETTAZIONE (' ')
110100970416E   4C                   ENDIF
110200970416IF  4C     $SOL          IFEQ      'A'
110300970110     C                   ADD       1             BOA(F)                         *BOLLETTAZIONE (A)
110400970416E   4C                   ENDIF
110500970416IF  4C     $SOL          IFEQ      'B'
110600970110     C                   ADD       1             BOB(F)                         *BOLLETTAZIONE (B)
110700970416E   4C                   ENDIF
110800050808     c* con il cod. trattamento merce aggancio tabella 1B per vedere se
110900050808     c* previsto packing list
111000050808     c                   z-add     1             xp                4 0
111100050808     c     tasctm        lookup    cpkl(xp)                               95
111200050808     c                   if        *in95
111300050808     c                   add       1             pks(f)
111400050808     c                   else
111500050808     c                   add       1             pkn(f)
111600050808     c                   end
111700040811     C* se oltre 20 colli sommo i colli originali altrimenti i colli
111800040811     c* della bolla
111900040811     C                   ADD       colli         NCL(F)                         *COLLI
112000040811     C                   ADD       $pes          PKB(F)                         *PESO
112100040811     C                   ADD       $vol          VLB(F)                         *VOLUME
112200060829     C                   ADD       colli1        NCL1(F)                        *COLLI
112300060829     C                   ADD       $pes1         PKB1(F)                        *PESO
112400060829     C                   ADD       $vol1         VLB1(F)                        *VOLUME
112500970110     C*
112600970110     C                   MOVEL     TASTBL        WA1               1
112700970416IF  4C     WA1           IFEQ      'F'
112800970110     C                   ADD       1             FRA(F)                         *FRANCHI
112900970416X   4C                   ELSE
113000970110     C                   ADD       1             ASS(F)                         *ASSEGNATI
113100970416E   4C                   ENDIF
113200970110     C*
113300970110     C                   MOVEL     TASGVA        WA1
113400970416IF  4C     WA1           IFEQ      'P'
113500970110     C                   ADD       1             PCV(F)                         *CONSEGNE PRIVATI
113600970416E   4C                   ENDIF
113700040802     C* bancali bartolini
113800040802IF  4C     WA1           IFEQ      'B'
113900040824     C                   ADD       1             sban(F)                        *CONSEGNE PRIVATI
114000040809     C                   ADD       §ar5nba       nba(F)                         *CONSEGNE PRIVATI
114100040809     C                   ADD       §ar5nb2       nba(F)                         *CONSEGNE PRIVATI
114101160726     c                   if        §ar5tba = 01 or §ar5tb2 = 01
114102160726     C                   ADD       1             sb01(F)                        *CONSEGNE PRIVATI
114103160726     C                   ADD       §ar5nba       nb01(F)                        *CONSEGNE PRIVATI
114104160726     c                   end
114105160726     c                   if        §ar5tba = 02 or §ar5tb2 = 02
114106160726     C                   ADD       1             sb02(F)                        *CONSEGNE PRIVATI
114107160726     C                   ADD       §ar5nba       nb02(F)                        *CONSEGNE PRIVATI
114108160726     c                   end
114109160726     c                   if        §ar5tba = 03 or §ar5tb2 = 03
114110160726     C                   ADD       1             sb03(F)                        *CONSEGNE PRIVATI
114111160726     C                   ADD       §ar5nba       nb03(F)                        *CONSEGNE PRIVATI
114112160726     c                   end
114200040802E   4C                   ENDIF
114300040802     C* bancali oltre 20 colli
114400040802IF  4C     WA1           IFEQ      'O'
114500040811     C                   ADD       1             sbnb(F)                        *CONSEGNE PRIVATI
114600040811     C                   ADD       §ar5bnba      nbn(F)                         *CONSEGNE PRIVATI
114700040811     C                   ADD       §ar5bnb2      nbn(F)                         *CONSEGNE PRIVATI
114800040802E   4C                   ENDIF
114900040802     C* merce di valore
115000040802     c                   movel     tasflo        dtasflo
115100040802IF  1C     §FLOTBA       IFEQ      'V'
115200040802     C                   add       1             val(F)
115300040802E   1C                   ENDIF
115400970110     C*
115500970416IF  4C     TASFDN        IFEQ      'S'
115600011011     C                   ADD       1             PCM(F)                         *MERCE A MAGAZZINO
115700970416E   4C                   ENDIF
115800970110     C*
115900970416IF  4C     TASFFD        IFEQ      'S'
116000970110     C                   ADD       1             PFD(F)                         *FERMO DEPOSITO
116100970416E   4C                   ENDIF
116200970110     C*
116300970416IF  4C     TASTSP        IFEQ      'E'
116400970110     C                   ADD       1             ESP(F)                         *ESPRESSI
116500970416E   4C                   ENDIF
116600970110     C*
116700011011IF  4C     TASFIN        IFEQ      'D'
116800011011     C                   ADD       1             PCL(F)                         *CONS. L.DISAGIATE
116900011011E   4C                   ENDIF
117000011011     C*
117100011011IF  4C     TASFIN        IFEQ      'I'
117200011011     C                   ADD       1             CIS(F)                         *CONS. ISOLE
117300011011E   4C                   ENDIF
117400011011     C*
117500011011IF  4C     TASFAP        IFEQ      'I'
117600011011     C                   ADD       1             RIS(F)                         *RITIRI ISOLE
117700011011E   4C                   ENDIF
117800011011     C*
117900011011IF  4C     TASFIN        IFEQ      'P'
118000110623IF  4C     TASFIN        oreq      'Z'
118100011011     C                   ADD       1             PPV(F)                         *CONSEGNE PROVINCIA
118200970416E   4C                   ENDIF
118300970110     C*
118400970416IF  4C     $PCA          IFEQ      'S'
118500970110     C                   ADD       1             PCA(F)                         *CONS. APPUNTAMENTO
118600970416E   4C                   ENDIF
118700970110     C*
118800970416IF  4C     $PCP          IFEQ      'S'
118900970110     C                   ADD       1             PCP(F)                         *CONS. PIANI
119000970416E   4C                   ENDIF
119100970110     C*
119200970416IF  4C     $PCS          IFEQ      'S'
119300970110     C                   ADD       1             PCS(F)                         *CONS. SUPERMERCATI
119400970416E   4C                   ENDIF
119500970110     C*
119600970416IF  4C     $PCF          IFEQ      'S'
119700970110     C                   ADD       1             PCF(F)                         *CONS. FUORI MISURA
119800970416E   4C                   ENDIF
119900040802     C* destinatario disagiato
120000040802IF  4C     $ded          IFEQ      'S'
120100040802     C                   ADD       1             ded(F)                         *CONS. FUORI MISURA
120200040802E   4C                   ENDIF
120300970415     C*
120400970416IF  4C     $PCD          IFEQ      'S'
120500970415     C                   ADD       1             PCD(F)                         *CONS. DDT
120600970416E   4C                   ENDIF
120700970110     C*
120800970416IF  4C     $GGP          IFGT      *ZEROS
120900970110     C                   Z-ADD     1             I
121000970110     C                   SETOFF                                           98
121100970110     C     $GGP          LOOKUP    GGP(I)                                 98
121200970416IF  5C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
121300970110     C                   ADD       1             SGP(I)                         *SPED. GG PAGAMENTO
121400970416X   5C                   ELSE                                                   *MAI MEMORIZZATI
121500970110     C                   Z-ADD     1             I
121600970416DO  6C     I             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
121700970416IF  7C     GGP(I)        IFEQ      *ZEROS                                       *DISPONIBILE
121800970110     C                   Z-ADD     $GGP          GGP(I)                         *GG
121900970110     C                   Z-ADD     1             SGP(I)                         *SPED. GG PAGAMENTO
122000981014     C                   LEAVE                                                  *ESCE DAL CICLO
122100970416E   7C                   ENDIF
122200970416E   6C                   ENDDO
122300970416E   5C                   ENDIF
122400970416E   4C                   ENDIF
122500000411     C*
122600000411     C                   MOVEL     TASTBL        WA1               1
122700000411IF  4C     WA1           IFEQ      'F'                                          *SOLO FRANCHI
122800000411IF  5C     TASSTR        IFGT      *ZEROS
122900990812     C     TASSTR        ANDNE     99999
123000000411IF  6C     TASSTR        IFGT      999                                          *CAMPO 5,0 -> 3,0
123100990812     C                   Z-ADD     999           TASSTR
123200000411E   6C                   ENDIF
123300970110     C                   Z-ADD     TASSTR        I                              *STOP RITIRO
123400970110     C                   ADD       1             RIT(I)                         *SPEDIZIONI STOP
123500000411E   5C                   ENDIF
123600000411     C*
123700000411X   4C                   ELSE                                                   *SOLO ASSEGNATI
123800000411     C*
123900000411IF  5C     TASSTP        IFGT      *ZEROS
124000990812     C     TASSTP        ANDNE     99999
124100000411IF  6C     TASSTP        IFGT      999                                          *CAMPO 5,0 -> 3,0
124200990812     C                   Z-ADD     999           TASSTP
124300000411E   6C                   ENDIF
124400970110     C                   Z-ADD     TASSTP        I                              *STOP CONSEGNA
124500970110     C                   ADD       1             CON(I)                         *SPEDIZIONI STOP
124600000411E   5C                   ENDIF
124700000411E   4C                   ENDIF
124800970110     C*
124900060829     c* per ricavi
125000970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
125100970110     C     PARTTR        OREQ      '3'                                          *KG
125200040811     C                   Z-ADD     $pes          TASSGL           13 3          *SCAGLIONE IN KG
125300970416X   4C                   ELSE
125400040811     C                   Z-ADD     colli         TASSGL                         *SCAGLIONE IN COLLI
125500040811E   4C                   ENDIF
125600971024IF  4C     TASSGL        IFGT      *ZEROS
125700971024DO  5C     1             DO        10            I
125800971024IF  6C     TASSGL        IFLE      SSGL(I)
125900970110     C                   ADD       1             SSP(I)                         *N°SPED SCAGLIONE
126000040811     C                   ADD       $pes          PSC(I)                         *PESO
126100040811     C                   ADD       colli         CSC(I)                         *COLLI
126200970415     C                   ADD       $VOL          VSC(I)                         *VOLUME
126300970110     C                   LEAVE
126400971024E   6C                   ENDIF
126500971024E   5C                   ENDDO
126600040920     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
126700060829IF  5C     PARCTO        IFEQ      'T'
126800040920     C     PARCAR        ANDEQ     88888
126900040920     C     PARLNP        ANDEQ     999
127000040920     C                   EXSR      SRSGL8
127100060829E   5C                   ENDIF
127200971024E   4C                   ENDIF
127300060829     c* per competenze
127400060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
127500060829     C     PARTTR        OREQ      '3'                                          *KG
127600060829     C                   Z-ADD     $pes1         TASSGL           13 3          *SCAGLIONE IN KG
127700060829X   4C                   ELSE
127800060829     C                   Z-ADD     colli1        TASSGL                         *SCAGLIONE IN COLLI
127900060829E   4C                   ENDIF
128000060829IF  4C     TASSGL        IFGT      *ZEROS
128100060829DO  5C     1             DO        10            I
128200060829IF  6C     TASSGL        IFLE      SSGL(I)
128300060829     C                   ADD       1             SSP1(I)                        *N°SPED SCAGLIONE
128400060829     C                   ADD       $pes1         PSC1(I)                        *PESO
128500060829     C                   ADD       colli1        CSC1(I)                        *COLLI
128600060829     C                   ADD       $VOL1         VSC1(I)                        *VOLUME
128700060829     C                   LEAVE
128800060829E   6C                   ENDIF
128900060829E   5C                   ENDDO
129000060829     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
129100060829IF  5C     PARCTO        IFEQ      'T'
129200060829     C     PARCAR        ANDEQ     88888
129300060829     C     PARLNP        ANDEQ     999
129400060829     C                   EXSR      SRSGL81
129500060829E   5C                   ENDIF
129600060829E   4C                   ENDIF
129700971024E   3C                   ENDIF
129800970110     C*
129900970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
130000970416     C     DEPRI         OREQ      'S'                                          *BOLLA VALIDA RICAVI
130100970416IF  4C     $CAS          IFGT      *ZEROS
130200970110     C                   ADD       $CAS          CAS(F)                         *C/ASSEGNO
130300970416IF  5C     $TPI          IFEQ      'M'
130400970110     C                   ADD       1             PAM(F)                         *MITTENTE
130500970416X   5C                   ELSE
130600970110     C                   ADD       1             PAB(F)                         *BARTOLINI
130700970416E   5C                   ENDIF
130800970416E   4C                   ENDIF
130900970110     C*
131000970416IF  4C     $IAS          IFGT      *ZEROS
131100970110     C                   ADD       $IAS          ASC(F)                         *ASSICURAZIONE
131200970110     C                   ADD       1             PAC(F)                         *ASSICURAZIONE CONTO
131300970416X   4C                   ELSE
131400060315     c                   if        flgacbase = *on
131500060315     C                   ADD       1             xx1(F)                         *ASSICURAZIONE R.C.V
131600060315     c                   else
131700970110     C                   ADD       1             PAR(F)                         *ASSICURAZIONE R.C.V
131800970416E   4C                   ENDIF
131900060315E   4C                   ENDIF
132000970110     C*
132100000414IF  4C     $PGI          IFEQ      'S'
132200970110     C                   ADD       1             PGI(F)                         *GIACENZE
132300000414     C                   ADD       $PGIGS        GGS(F)                         *GG SOSTA GIACENZA
132400000414     C                   ADD       $PGINR        NRG(F)                         *N° RIAPERTURE GIACE
132500970416E   4C                   ENDIF
132600970416E   3C                   ENDIF
132700970110     C*
132800970416X   2C                   ELSE                                                   *CON.TASS.NON TROVAT
132900970416     C                   MOVEL     MTAS(2)       DEPERR           50
133000970110     C                   MOVEL     'T'           DEPTER            1
133100970110     C                   EXSR      STAERR
133200970416E   2C                   ENDIF
133300970110     C*
133400970110X   1C                   ELSE                                                   *FILIALE NON TROVATA
133500970110     C                   MOVEL     MTAS(1)       DEPERR
133600970110     C                   MOVEL     'T'           DEPTER
133700970110     C                   EXSR      STAERR
133800970110E   1C                   ENDIF
133900970110     C*
134000970110     C                   ENDSR
134100970415     C*------------------------------------------------------------------------*
134200040809     C* fiar5 tipo record BAN
134300970415     C*------------------------------------------------------------------------*
134400040809     C     srfiar5       BEGSR
134500040809     c*
134600040809     c                   z-add     tasaas        ar5aas
134700040809     c                   z-add     taslnp        ar5lnp
134800040809     c                   z-add     tasnrs        ar5nrs
134900040809     c                   z-add     tasnsp        ar5nsp
135000040811     c                   clear                   ar5uni
135100040809     c     kar5          chain     fiar531c
135200040809     C*
135300040809     C                   ENDSR
135400040809     C*------------------------------------------------------------------------*
135500040809     C* REPSOL - REPERISCE LA SOLUZIONE DI INVIO DATI -BOLLETTAZIONE-
135600040809     C*------------------------------------------------------------------------*
135700040809     C     REPSOL        BEGSR
135800970415     C*---
135900970415     C* NON E' PRESENTE IN TABELLA -> NESSUNA PROCEDURA            ( )
136000970415     C* NON HA IL NUMERO DI SERIE  -> STAMPA LE BOLLE              (A)
136100970415     C* HA     IL NUMERO DI SERIE  -> STAMPA LE BOLLE + SEGNACOLLI (B)
136200040310     C*--- escludo bolle recapito contrassegni
136300970415IF  1C     TASNRS        IFGT      *ZEROS
136400040310IF  1C     TAScbo        andne     'FW'
136500970415     C                   MOVEL     'B'           $SOL                           *PROCEDURA 'B'
136600970415X   1C                   ELSE
136700100303     C                   Z-ADD     1             KTBKUT
136800100303     C                   MOVEL     '3C'          KTBCOD
136900100303     c                   movel(p)  tasksc        ktbkey
137000100303     c     keytab        chain     tabel00f
137100100303     c                   if        %found(tabel00f) and tblflg = ' '
137200100303     C                   MOVEL     TBLUNI        DS3C
137300100303IF  3C     §3COKD        IFEQ      '*'
137400970415     C                   MOVEL     'A'           $SOL                           *PROCEDURA 'A'
137500970415X   3C                   ELSE
137600970415     C                   MOVEL     *BLANKS       $SOL                           *PROCEDURA NO AVVIAT
137700970415E   3C                   ENDIF
137800970415E   2C                   ENDIF
137900970415E   1C                   ENDIF
138000970415     C*
138100970415     C                   ENDSR
138200970415     C*------------------------------------------------------------------------*
138300970415     C* REPVOL - REPERISCE IL VOLUME DELLA BOLLA
138400970415     C*------------------------------------------------------------------------*
138500970415     C     REPVOL        BEGSR
138600060829     C* colli x ricavi
138700060829     C                   Z-ADD     *ZEROS        $VOL              5 3
138800970415     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
138900040811    1C     TASVLC        IFGT      *ZEROS
139000040811     C     colli         ANDEQ     TASNCR
139100970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
139200970415E   1C                   ENDIF
139300970415     C*
139400970415     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
139500970415     C     TASVLC        IFGT      TASVLF
139600040811     C     colli         ANDNE     TASNCR
139700970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
139800970415E   1C                   ENDIF
139900970415     C*
140000970415     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
140100970415IF  1C     $VOL          IFEQ      *ZEROS
140200970415     C                   Z-ADD     TASVLF        $VOL                           *VOLUME NORMALE
140300060829E   1C                   ENDIF
140400060829c*   c*colli x competetnze
140500060829     C                   Z-ADD     *ZEROS        $VOL1             5 3
140600060829     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
140700060829    1C     TASVLC        IFGT      *ZEROS
140800060829     C     colli1        ANDEQ     TASNCR
140900060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
141000060829E   1C                   ENDIF
141100060829     C*
141200060829     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
141300060829     C     TASVLC        IFGT      TASVLF
141400060829     C     colli1        ANDNE     TASNCR
141500060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
141600060829E   1C                   ENDIF
141700060829     C*
141800060829     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
141900060829IF  1C     $VOL1         IFEQ      *ZEROS
142000060829     C                   Z-ADD     TASVLF        $VOL1                          *VOLUME NORMALE
142100060829E   1C                   ENDIF
142200970415     C*
142300000414     C                   ENDSR
142400040811     C*------------------------------------------------------------------------*
142500040811     C* REPpes - REPERISCE IL peso DELLA BOLLA
142600040811     C*------------------------------------------------------------------------*
142700040811     C     REPpes        BEGSR
142800060829     C* colli x ricavi
142900060829     C                   Z-ADD     *ZEROS        $pes
143000040811     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
143100040811    1C     TASpkC        IFGT      *ZEROS
143200040811     C     colli         ANDEQ     TASNCp
143300040811     C                   Z-ADD     TASpkc        $pes                           *VOLUME CML
143400040811E   1C                   ENDIF
143500040811     C*
143600040811     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
143700040811     C     TASpkc        IFGT      TASpkF
143800040811     C     colli         ANDNE     TASNCp
143900040811     C                   Z-ADD     TASpkC        $pes                           *VOLUME CML
144000040811E   1C                   ENDIF
144100040811     C*
144200040811     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
144300040811IF  1C     $pes          IFEQ      *ZEROS
144400040811     C                   Z-ADD     TASpkF        $pes                           *VOLUME NORMALE
144500040811E   1C                   ENDIF
144600060829     C* colli x competenze
144700060829     C                   Z-ADD     *ZEROS        $pes1
144800060829     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
144900060829    1C     TASpkC        IFGT      *ZEROS
145000060829     C     colli1        ANDEQ     TASNCp
145100060829     C                   Z-ADD     TASpkc        $pes1                          *VOLUME CML
145200060829E   1C                   ENDIF
145300060829     C*
145400060829     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
145500060829     C     TASpkc        IFGT      TASpkF
145600060829     C     colli1        ANDNE     TASNCp
145700060829     C                   Z-ADD     TASpkC        $pes1                          *VOLUME CML
145800060829E   1C                   ENDIF
145900060829     C*
146000060829     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
146100060829IF  1C     $pes1         IFEQ      *ZEROS
146200060829     C                   Z-ADD     TASpkF        $pes1                          *VOLUME NORMALE
146300060829E   1C                   ENDIF
146400040811     C*
146500040811     C                   ENDSR
146600970110     C*------------------------------------------------------------------------*
146700970110     C* REPCAS - REPERISCE IL CONTRASSEGNO
146800970110     C*------------------------------------------------------------------------*
146900970110     C     REPCAS        BEGSR
147000970110     C*
147100970110     C                   Z-ADD     TASAAS        KSBAAS                         *ANNO
147200970110     C                   Z-ADD     TASLNP        KSBLNP                         *LINEA PARTENZA
147300970110     C                   Z-ADD     TASNRS        KSBNRS                         *SERIE
147400970110     C                   Z-ADD     TASNSP        KSBNSP                         *SPEDIZIONE
147500970110     C     KEYCSB        CHAIN     TNCSB03L                           98
147600970110IF  1C     *IN98         IFEQ      *OFF                                         *TROVATO
147700970110IF  2C     CSBSTA        IFNE      9                                            *NO ANNULLATO
147800970110     C                   MOVEL     CSBTPI        $TPI                           *TIPO INCASSO
147900011011     C                   Z-ADD     CSBCAS        $CAS                           *C/ASSEGNO
148000011011     C     CSBVCA        IFNE      PARDIV
148100011011     C                   Z-ADD     $CAS          WIMI             30 9
148200011011     C                   MOVEL     CSBVCA        WDVI              3
148300011011     C                   EXSR      CNVEUR
148400011011     C                   Z-ADD     WIMO          $CAS
148500011011     C                   ENDIF
148600970110E   2C                   ENDIF
148700970110E   1C                   ENDIF
148800970110     C*
148900970110     C                   ENDSR
149000021122     C*------------------------------------------------------------------------*
149100021122     C* REPGIA - REPERISCE LE GIACENZE
149200021122     C*------------------------------------------------------------------------*
149300021122     C     REPGIA        BEGSR
149400021122     C*
149500021122     C* AZZERA LE VARIABILI DI LAVORO
149600021122     C                   Z-ADD     *ZEROS        §GGSOS            3 0
149700021122     C*
149800021122     C* LEGGE TUTTE LE GIACENZE DELLA BOLLA
149900021122     C                   Z-ADD     TASAAS        KGPAAS
150000021122     C                   Z-ADD     TASLNP        KGPLNP
150100021122     C                   Z-ADD     TASNRS        KGPNRS
150200021122     C                   Z-ADD     TASNSP        KGPNSP
150300050309     C     K01GCP        SETLL     TIGCP51L
150400050309     C     K01GCP        READE     TIGCP51L                               98
150500021122DO  1C     *IN98         DOWEQ     *OFF
150600021122IF  2C     GCPATB        IFEQ      *BLANKS
150700021122     C                   MOVEL     'S'           $PGI                           *ESISTE GIACENZA
150800021122     C* APERTURE
150900021122     C                   ADD       1             $PGINR                         *NUMERO RIAPERTURE
151000021122     C* GIORNI DI SOSTA
151100021122IF  3C     GCPDDM        IFGT      *ZEROS                                       *IMMESSE DISPOSIZION
151200021122IF  4C     GCPDUR        IFGT      *ZEROS                                       *ULTIMA RIAPERTURA
151300021122     C                   Z-ADD     GCPDUR        G08INV
151400021122X   4C                   ELSE                                                   *APERTURA GIACENZA
151500021122     C                   MOVEL     GCPAGC        G08INV
151600021122     C                   MOVE      GCPMGC        G08INV
151700021122E   4C                   ENDIF
151800021122     C                   Z-ADD     *ZEROS        G08DAT
151900021122     C                   MOVEL     '3'           G08ERR
152000021122     C                   CALL      'XSRDA8'
152100021122     C                   PARM                    WLBDA8
152200021122     C                   Z-ADD     G08TGI        §GGDA             5 0
152300021122IF  4C     GCPDED        IFGT      *ZEROS                                       *DISPOSIZ.GIACENZA
152400021122     C                   Z-ADD     GCPDED        G08INV
152500021122X   4C                   ELSE                                                   *DISPOSIZ.MITTENTE
152600021122     C                   Z-ADD     GCPDDM        G08INV
152700021122E   4C                   ENDIF
152800021122     C                   Z-ADD     *ZEROS        G08DAT
152900021122     C                   MOVEL     '3'           G08ERR
153000021122     C                   CALL      'XSRDA8'
153100021122     C                   PARM                    WLBDA8
153200021122     C                   Z-ADD     G08TGI        §GGA              5 0
153300021122     C* GIORNI DI SOSTA
153400021122     C     §GGA          SUB       §GGDA         §GGXX             3 0
153500021122     C                   ADD       1             §GGXX
153600021122     C                   ADD       §GGXX         §GGSOS
153700021122E   3C                   ENDIF
153800021122E   2C                   ENDIF
153900050309     C     K01GCP        READE     TIGCP51L                               98
154000021122E   1C                   ENDDO
154100021122     C*
154200021122     C                   Z-ADD     §GGSOS        $PGIGS                         *GIORNI DI SOSTA
154300021122     C*
154400021122     C                   ENDSR
154500970110     C*------------------------------------------------------------------------*
154600970110     C* REPASS - REPERISCE L'ASSICURAZIONE
154700970110     C*------------------------------------------------------------------------*
154800970110     C     REPASS        BEGSR
154900060315     c                   clear                   flgacbase         1
155000970110     C*
155100970110IF  1C     TASIAS        IFGT      *ZEROS
155200011011     C                   Z-ADD     TASIAS        $IAS
155300011011IF  2C     TASVAS        IFNE      PARDIV                                       *DIVISA ESTERA
155400011011     C                   Z-ADD     $IAS          WIMI             30 9
155500011011     C                   MOVEL     TASVAS        WDVI              3
155600011011     C                   EXSR      CNVEUR
155700011011     C                   Z-ADD     WIMO          $IAS
155800011011     C                   ENDIF
155900060315     c* verifico se esiste ac base
156000060315     c                   else
156100060315     c                   z-add     1             v
156200060315     c     'd'           lookup    svn(v)                                 99
156300060320     c                   if        van(v) <> 0 and *in99
156400060315     c                   eval      flgacbase = *on
156500970110E   1C                   ENDIF
156600060315E   1C                   ENDIF
156700970110     C*
156800970110     C                   ENDSR
156900970110     C*------------------------------------------------------------------------*
157000060315     C* REPVrf - REPERISCE le varie di fatturazione
157100990812     C* -----------------------------------------------------------------------*
157200060315     C     REPVrf        BEGSR
157300060315     C* CARICO LE VARIE BOLLA ORIGINALE DA TITAS
157400060315     C                   Z-ADD     *ZEROS        V                 6 0
157500060315     C     tasSV1        IFNE      *BLANKS
157600060315     C                   ADD       1             V
157700060315     C                   MOVEL     tasSV1        SVN(V)
157800060315     C                   Z-ADD     tasVA1        VAN(V)
157900060315     C                   ENDIF
158000060315     C     tasSV2        IFNE      *BLANKS
158100060315     C                   ADD       1             V
158200060315     C                   MOVEL     tasSV2        SVN(V)
158300060315     C                   Z-ADD     tasVA2        VAN(V)
158400060315     C                   ENDIF
158500060315     C     tasSV3        IFNE      *BLANKS
158600060315     C                   ADD       1             V
158700060315     C                   MOVEL     tasSV3        SVN(V)
158800060315     C                   Z-ADD     tasVA3        VAN(V)
158900060315     C                   ENDIF
159000060315     C* CARICO LE VARIE BOLLA DA TITA7 (SOLO SE C'E' LA VARIA 3)
159100060315     C     tasSV3        IFNE      *BLANKS
159200060315     C                   Z-ADD     tasAAS        TA7AAS
159300060315     C                   Z-ADD     tasLNP        TA7LNP
159400060315     C                   Z-ADD     tasNRS        TA7NRS
159500060315     C                   Z-ADD     tasNSP        TA7NSP
159600060315     C                   MOVEL     tasTBL        TA7TBL
159700060315     C     KEYTA7        SETLL     TITA730C
159800060315     c                   do        *hival
159900060315     C     KEYTA7        READE     TITA730C
160000060315     c                   if        %eof(tita730c)
160100060315     c                   leave
160200060315     c                   end
160300060315if  1C                   IF        (TA7VAN = 88888888,000)
160400060315     C                   Z-ADD     *zeros        TA7VAN
160500060315e   1c                   ENDIF
160600060315     C                   ADD       1             V
160700060315     C                   MOVEL     TA7SVN        SVN(V)
160800060315     C                   Z-ADD     TA7VAN        VAN(V)
160900060315     C                   ENDDO
161000060315e   1c                   ENDIF
161100060315     c                   endsr
161200060315     C*------------------------------------------------------------------------*
161300060315     C* REPVAR - REPERISCE LE consegne particolari
161400060315     C* -----------------------------------------------------------------------*
161500060315     C     REPVAR        BEGSR
161600970110     C*
161700970110     C* VARIE -BOLLA-
161800021122IF  1C     TASFTC        IFEQ      'A'
161900021122     C     TASTC2        OREQ      'A'
162000970110     C                   MOVEL     'S'           $PCA                           *CONS APPUNTAMENTO
162100970110E   1C                   ENDIF
162200021122IF  1C     TASFTC        IFEQ      'P'
162300021122     C     TASTC2        OREQ      'P'
162400970110     C                   MOVEL     'S'           $PCP                           *CONS PIANI
162500970110E   1C                   ENDIF
162600021122IF  1C     TASFTC        IFEQ      'S'
162700021122     C     TASTC2        OREQ      'S'
162800970110     C                   MOVEL     'S'           $PCS                           *CONS SUPERMERCATI
162900970110E   1C                   ENDIF
163000021122IF  1C     TASFTC        IFEQ      'F'
163100021122     C     TASTC2        OREQ      'F'
163200970110     C                   MOVEL     'S'           $PCF                           *CONS FUORI MISURA
163300970110E   1C                   ENDIF
163400040802IF  1C     TASFTC        IFEQ      'X'
163500040802     C     TASTC2        OREQ      'X'
163600040802     C                   MOVEL     'S'           $ded                           *CONS FUORI MISURA
163700040802E   1C                   ENDIF
163800021122IF  1C     TASLL1        IFEQ      'S'
163900970415     C                   MOVEL     'S'           $PCD                           *CONS DDT
164000970415E   1C                   ENDIF
164100970110     C*
164200970110     C                   ENDSR
164300980819     C*------------------------------------------------------------------------*
164400980819     C* AGGETX - AGGIORNA I DATI TIPO
164500980819     C*------------------------------------------------------------------------*
164600980819     C     AGGETX        BEGSR
164700980819     C*
164800980819     C* PER OGNI LINEA DI PARTENZA RICHIESTA
164900980819DO  1C     1             DO        500           I
165000980819IF  2C     SORG(I)       IFGT      *ZEROS
165100980819IF  3C     SPE(I)        IFGT      *ZEROS                                       *MEMORIZZATO QC.
165200980821     C                   Z-ADD     SORG(I)       XORG
165300140919     C                   movel     Sntw(I)       Xntw
165400980819     C     I             OCCUR     FPAG
165500980819     C     I             OCCUR     FCON
165600980819     C     I             OCCUR     FRIT
165700980819     C     I             OCCUR     FSGL
165800060829     C     I             OCCUR     FSGL1
165900980819     C     I             OCCUR     FCTS
166000980819     C     I             OCCUR     FTAR                                         *DATI TARIFFA
166100980819     C*---
166200980819     C* DATI TIPO -GENERALI-
166300980819     C*---
166400980821     C                   EXSR      SCRETC
166500980819     C*---
166600980819     C* DATI TIPO -SCAGLIONI-
166700980819     C*---
166800060829     c* ricavi
166900980821     C                   EXSR      SCRETS
167000060829     c* competenze
167100060829     c                   if        etcbnb <> 0
167200060829     c                   exsr      screts1
167300060829     c                   end
167400980819     C*---
167500980819     C* DATI TIPO -CODICI TASSAZIONE-
167600980819     C*---
167700980819     C* PER OGNI TIPO TARIFFA (ITALIA/ESTERO)
167800980819DO  4C     1             DO        2             K
167900980819IF  5C     SFIE(K)       IFNE      *BLANKS
168000980821     C                   MOVEL     SFIE(K)       XFIE
168100980821     C                   EXSR      SCRETT
168200981215E   5C                   ENDIF
168300980819E   4C                   ENDDO                                                  *FINE TARIFFA ITA/ES
168400980819     C*
168500980819E   3C                   ENDIF
168600980819E   2C                   ENDIF
168700980819E   1C                   ENDDO                                                  *FINE FILIALI
168800980819     C*
168900980821     C                   ENDSR
169000980819     C*------------------------------------------------------------------------*
169100980819     C* AGGET8 - AGGIORNA I DATI TIPO DI CARTELLO
169200980819     C*------------------------------------------------------------------------*
169300980819     C     AGGET8        BEGSR
169400970110     C*
169500980819     C* PER OGNI LINEA DI PARTENZA MEMORIZZATA
169600971124DO  1C     1             DO        500           I
169700980820     C     I             OCCUR     FPAG
169800970110     C     I             OCCUR     FCON
169900970110     C     I             OCCUR     FRIT
170000970110     C     I             OCCUR     FSGL
170100060829     C     I             OCCUR     FSGL1
170200970110     C     I             OCCUR     FCTS
170300980819     C     I             OCCUR     FTAR
170400980819     C*
170500980819     C* SOMMA I DATI PER LE TARIFFE DI CARTELLO -ITA/EST- DI TUTTE LE FILIALI
170600980819IF  2C     SFL1(I)       IFEQ      *BLANKS                                      *FILIALE ITALIA
170700980824     C                   Z-ADD     1             U                 4 0
170800980819X   2C                   ELSE                                                   *FILIALE ESTERO
170900980819     C                   Z-ADD     2             U
171000980819E   2C                   ENDIF
171100980824     C     U             OCCUR     CCAR
171200980824     C     U             OCCUR     CPAG
171300980824     C     U             OCCUR     CCON
171400980824     C     U             OCCUR     CRIT
171500980824     C     U             OCCUR     CSGL
171600060829     C     U             OCCUR     CSGL1
171700980824     C     U             OCCUR     CCTS
171800980819     C*
171900980819     C* DATI TIPO -GENERALI-
172000980821     C                   ADD       SPE(I)        USPE(1)
172100980821     C                   ADD       CLA(I)        UCLA(1)
172200980821     C                   ADD       CLB(I)        UCLB(1)
172300980821     C                   ADD       CLC(I)        UCLC(1)
172301160726     C                   ADD       CLE(I)        UCLE(1)
172400980821     C                   ADD       BON(I)        UBON(1)
172500980821     C                   ADD       BOA(I)        UBOA(1)
172600980821     C                   ADD       BOB(I)        UBOB(1)
172700980821     C                   ADD       NCL(I)        UNCL(1)
172800980821     C                   ADD       PKB(I)        UPKB(1)
172900980821     C                   ADD       VLB(I)        UVLB(1)
173000060829     C                   ADD       NCL1(I)       UNCL1(1)
173100060829     C                   ADD       PKB1(I)       UPKB1(1)
173200060829     C                   ADD       VLB1(I)       UVLB1(1)
173300980821     C                   ADD       FRA(I)        UFRA(1)
173400980821     C                   ADD       ASS(I)        UASS(1)
173500980821     C                   ADD       PCV(I)        UPCV(1)
173600980821     C                   ADD       PCM(I)        UPCM(1)
173700980821     C                   ADD       PFD(I)        UPFD(1)
173800980821     C                   ADD       ESP(I)        UESP(1)
173900980821     C                   ADD       PPV(I)        UPPV(1)
174000980821     C                   ADD       PCA(I)        UPCA(1)
174100980821     C                   ADD       PCP(I)        UPCP(1)
174200980821     C                   ADD       PCS(I)        UPCS(1)
174300980821     C                   ADD       PCF(I)        UPCF(1)
174400980821     C                   ADD       PCD(I)        UPCD(1)
174500980821     C                   ADD       PCC(I)        UPCC(1)
174600980821     C                   ADD       PCL(I)        UPCL(1)
174700980821     C                   ADD       PAB(I)        UPAB(1)
174800980821     C                   ADD       PAM(I)        UPAM(1)
174900980821     C                   ADD       CAS(I)        UCAS(1)
175000980821     C                   ADD       PAR(I)        UPAR(1)
175100980821     C                   ADD       PAC(I)        UPAC(1)
175200060315     C                   ADD       xx1(I)        Uxx1(1)
175300980821     C                   ADD       ASC(I)        UASC(1)
175400980821     C                   ADD       PGI(I)        UPGI(1)
175500000414     C                   ADD       GGS(I)        UGGS(1)
175600000414     C                   ADD       NRG(I)        UNRG(1)
175700980901     C                   ADD       CLT(I)        UCLT(1)
175800011011     C                   ADD       CIS(I)        UCIS(1)
175900011011     C                   ADD       RIS(I)        URIS(1)
176000020215     C                   ADD       CLD(I)        UCLD(1)
176100050808     C                   ADD       pks(I)        Upks(1)
176200050808     C                   ADD       pkn(I)        Upkn(1)
176300040802     C                   ADD       ded(I)        Uded(1)
176400040811     C                   ADD       sbnb(I)       Usbnb(1)
176500040824     C                   ADD       sban(I)       Usban(1)
176600040811     C                   ADD       nbn(I)        Unbn(1)
176700040802     C                   ADD       nba(I)        Unba(1)
176701160726     C                   ADD       sb01(I)       Usb01(1)
176702160726     C                   ADD       sb02(I)       Usb02(1)
176703160726     C                   ADD       sb03(I)       Usb03(1)
176704160726     C                   ADD       nb01(I)       Unb01(1)
176705160726     C                   ADD       nb02(I)       Unb02(1)
176706160726     C                   ADD       nb03(I)       Unb03(1)
176800040802     C                   ADD       val(I)        Uval(1)
176900980825DO  2C     1             DO        30            J
177000980825IF  3C     GGP(J)        IFGT      *ZEROS
177100980825     C                   Z-ADD     1             A                 4 0
177200980825     C                   SETOFF                                           98
177300980825     C     GGP(J)        LOOKUP    UGGP(A)                                98
177400980825IF  4C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
177500980825     C                   ADD       SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
177600980825X   4C                   ELSE                                                   *MAI MEMORIZZATI
177700980825     C                   Z-ADD     1             A
177800980825DO  5C     A             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
177900980825IF  6C     UGGP(A)       IFEQ      *ZEROS                                       *DISPONIBILE
178000980825     C                   Z-ADD     GGP(J)        UGGP(A)                        *GG
178100980825     C                   Z-ADD     SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
178200980825     C                   Z-ADD     30            A                              *ESCE DAL CICLO
178300980825E   6C                   ENDIF
178400980825     C                   ADD       1             A
178500980825E   5C                   ENDDO
178600980825E   4C                   ENDIF
178700980825E   3C                   ENDIF
178800980825E   2C                   ENDDO
178900980819DO  2C     1             DO        999           J
179000980819     C                   ADD       RIT(J)        URIT(J)
179100980819     C                   ADD       CON(J)        UCON(J)
179200980819E   2C                   ENDDO
179300980819     C*
179400040920     C* DATI TIPO -SCAGLIONI- (già caricati nella durante la lettura di
179500040920     c* titas)
179600980820     C*
179700970110     C* DATI TIPO -CODICI TASSAZIONE-
179800050103DO  2C     1             DO        600           J
179900980819     C                   ADD       TSI(J)        UTSI(J)
180000980819     C                   ADD       TSE(J)        UTSE(J)
180100980819E   2C                   ENDDO
180200970110E   1C                   ENDDO                                                  *FINE FILIALI
180300980821     C*---
180400980821     C* SCRIVE LA TARIFFA -888- ITALIA
180500980821     C*---
180600980820     C*
180700980821     C* CARICA LE SCHIERE DA ELABORARE
180800980821     C                   Z-ADD     1             U
180900980821     C                   Z-ADD     1             I
181000980821     C                   Z-ADD     888           XORG
181100140919     c                   clear                   xntw
181200980821     C                   EXSR      CARSCH
181300980821     C*
181400980821     C* DATI TIPO -GENERALI-
181500980821     C                   EXSR      SCRETC
181600980821     C*
181700980821     C* DATI TIPO -SCAGLIONI-
181800060829     c* ricavi
181900980821     C                   EXSR      SCRETS
182000060829     c* competenze
182100060829     c                   if        etcbnb <> 0
182200060829     C                   EXSR      SCRETS1
182300060829     c                   end
182400980821     C*
182500980821     C* DATI TIPO -CODICI TASSAZIONE-
182600980824     C                   MOVEL     'I'           XFIE
182700980821     C                   EXSR      SCRETT
182800980824     C*---
182900980824     C* SCRIVE LA TARIFFA -889- ESTERO
183000980824     C*---
183100980824     C*
183200980824     C* CARICA LE SCHIERE DA ELABORARE
183300980824     C                   Z-ADD     2             U
183400980824     C                   Z-ADD     1             I
183500980824     C                   Z-ADD     889           XORG
183600140919     c                   clear                   xntw
183700980824     C                   EXSR      CARSCH
183800980824     C*
183900980824     C* DATI TIPO -GENERALI-
184000980824     C                   EXSR      SCRETC
184100980824     C*
184200980824     C* DATI TIPO -SCAGLIONI-
184300060829     c* ricavi
184400980824     C                   EXSR      SCRETS
184500060829     c* competenze
184600060829     c                   if        etcbnb <> 0
184700060829     C                   EXSR      SCRETS1
184800060829     c                   end
184900980824     C*
185000980824     C* DATI TIPO -CODICI TASSAZIONE-
185100980824     C                   MOVEL     'E'           XFIE
185200980824     C                   EXSR      SCRETT
185300970110     C*
185400980819     C                   ENDSR
185500040920     C*------------------------------------------------------------------------*
185600040920     C* SRSGL8 - CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
185700040920     C*------------------------------------------------------------------------*
185800040920     C     SRSGL8        BEGSR
185900040920     C* 888 ITALIA
186000040920     C                   IF        SFL1(F) = ' '
186100040920     C                   Z-ADD     1             U
186200040920     C                   ELSE
186300040920     C                   Z-ADD     2             U
186400040920     C                   END
186500040920     C     U             OCCUR     CSGL
186600040920     C*
186700040920DO  5C     1             DO        10            I
186800040920IF  6C     TASSGL        IFLE      SGL(I)
186900040920     C                   ADD       1             USSP(I)                        *N°SPED SCAGLIONE
187000040920     C                   ADD       TASPKF        UPSC(I)                        *PESO
187100040920     C                   ADD       TASNCL        UCSC(I)                        *COLLI
187200040920     C                   ADD       $VOL          UVSC(I)                        *VOLUME
187300040920     C                   LEAVE
187400040920E   6C                   ENDIF
187500040920E   5C                   ENDDO
187600040920     C*
187700040920     C                   ENDSR
187800060829     C*------------------------------------------------------------------------*
187900060829     C* SRSGL81- CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
188000060829     C*------------------------------------------------------------------------*
188100060829     C     SRSGL81       BEGSR
188200060829     C* 888 ITALIA
188300060829     C                   IF        SFL1(F) = ' '
188400060829     C                   Z-ADD     1             U
188500060829     C                   ELSE
188600060829     C                   Z-ADD     2             U
188700060829     C                   END
188800060829     C     U             OCCUR     CSGL1
188900060829     C*
189000060829DO  5C     1             DO        10            I
189100060829IF  6C     TASSGL        IFLE      SGL(I)
189200060829     C                   ADD       1             USSP1(I)                       *N°SPED SCAGLIONE
189300060829     C                   ADD       TASPKF        UPSC1(I)                       *PESO
189400060829     C                   ADD       TASNCL        UCSC1(I)                       *COLLI
189500060829     C                   ADD       $VOL1         UVSC1(I)                       *VOLUME
189600060829     C                   LEAVE
189700060829E   6C                   ENDIF
189800060829E   5C                   ENDDO
189900060829     C*
190000060829     C                   ENDSR
190100980821     C*------------------------------------------------------------------------*
190200980821     C* CARSCH - CARICA LE SCHIERE DA ELABORARE
190300980821     C*------------------------------------------------------------------------*
190400980821     C     CARSCH        BEGSR
190500980821     C*
190600980821     C* SI POSIZIONA SULLE SCHIERE DELLA TARIFFA
190700980824     C     1             OCCUR     FPAG
190800980824     C     1             OCCUR     FCON
190900980824     C     1             OCCUR     FRIT
191000980824     C     1             OCCUR     FSGL
191100060829     C     1             OCCUR     FSGL1
191200980824     C     1             OCCUR     FCTS
191300980824     C     1             OCCUR     FTAR
191400980824     C     U             OCCUR     CCAR
191500980824     C     U             OCCUR     CPAG
191600980824     C     U             OCCUR     CCON
191700980824     C     U             OCCUR     CRIT
191800980824     C     U             OCCUR     CSGL
191900060829     C     U             OCCUR     CSGL1
192000980824     C     U             OCCUR     CCTS
192100980821     C*
192200980821     C* DATI TIPO -GENERALI-
192300980824     C                   Z-ADD     USPE(1)       SPE(1)
192400980824     C                   Z-ADD     UCLA(1)       CLA(1)
192500980824     C                   Z-ADD     UCLB(1)       CLB(1)
192600980824     C                   Z-ADD     UCLC(1)       CLC(1)
192601160726     C                   Z-ADD     UCLE(1)       CLE(1)
192700980824     C                   Z-ADD     UBON(1)       BON(1)
192800980824     C                   Z-ADD     UBOA(1)       BOA(1)
192900980824     C                   Z-ADD     UBOB(1)       BOB(1)
193000980824     C                   Z-ADD     UNCL(1)       NCL(1)
193100980824     C                   Z-ADD     UPKB(1)       PKB(1)
193200980824     C                   Z-ADD     UVLB(1)       VLB(1)
193300060829     C                   Z-ADD     UNCL1(1)      NCL1(1)
193400060829     C                   Z-ADD     UPKB1(1)      PKB1(1)
193500060829     C                   Z-ADD     UVLB1(1)      VLB1(1)
193600980824     C                   Z-ADD     UFRA(1)       FRA(1)
193700980824     C                   Z-ADD     UASS(1)       ASS(1)
193800980824     C                   Z-ADD     UPCV(1)       PCV(1)
193900980824     C                   Z-ADD     UPCM(1)       PCM(1)
194000980824     C                   Z-ADD     UPFD(1)       PFD(1)
194100980824     C                   Z-ADD     UESP(1)       ESP(1)
194200980824     C                   Z-ADD     UPPV(1)       PPV(1)
194300980824     C                   Z-ADD     UPCA(1)       PCA(1)
194400980824     C                   Z-ADD     UPCP(1)       PCP(1)
194500980824     C                   Z-ADD     UPCS(1)       PCS(1)
194600980824     C                   Z-ADD     UPCF(1)       PCF(1)
194700980824     C                   Z-ADD     UPCD(1)       PCD(1)
194800980824     C                   Z-ADD     UPCC(1)       PCC(1)
194900980824     C                   Z-ADD     UPCL(1)       PCL(1)
195000980824     C                   Z-ADD     UPAB(1)       PAB(1)
195100980824     C                   Z-ADD     UPAM(1)       PAM(1)
195200980824     C                   Z-ADD     UCAS(1)       CAS(1)
195300980824     C                   Z-ADD     UPAR(1)       PAR(1)
195400980824     C                   Z-ADD     UPAC(1)       PAC(1)
195500060315     C                   Z-ADD     Uxx1(1)       xx1(1)
195600980824     C                   Z-ADD     UASC(1)       ASC(1)
195700980824     C                   Z-ADD     UPGI(1)       PGI(1)
195800000414     C                   Z-ADD     UGGS(1)       GGS(1)
195900000414     C                   Z-ADD     UNRG(1)       NRG(1)
196000980901     C                   Z-ADD     UCLT(1)       CLT(1)
196100011011     C                   Z-ADD     UCIS(1)       CIS(1)
196200011011     C                   Z-ADD     URIS(1)       RIS(1)
196300020215     C                   Z-ADD     UCLD(1)       CLD(1)
196400050808     C                   Z-ADD     Upks(1)       pks(1)
196500050808     C                   Z-ADD     Upkn(1)       pkn(1)
196600040811     C                   Z-ADD     Usbnb(1)      sbnb(1)
196700040824     C                   Z-ADD     Usban(1)      sban(1)
196800040811     C                   Z-ADD     Unbn(1)       nbn(1)
196900040802     C                   Z-ADD     Unba(1)       nba(1)
197000040802     C                   Z-ADD     Uded(1)       ded(1)
197100040802     C                   Z-ADD     Uval(1)       val(1)
197101160726     C                   z-ADD     usb01(1)      sb01(1)
197102160726     C                   z-ADD     usb02(1)      sb02(1)
197103160726     C                   z-ADD     usb03(1)      sb03(1)
197104160726     C                   z-ADD     unb01(1)      nb01(1)
197105160726     C                   z-ADD     unb02(1)      nb02(1)
197106160726     C                   z-ADD     unb03(1)      nb03(1)
197200980821DO  1C     1             DO        30            J
197300980821     C                   Z-ADD     USGP(J)       SGP(J)
197400980821     C                   Z-ADD     UGGP(J)       GGP(J)
197500980821E   1C                   ENDDO
197600980821DO  1C     1             DO        999           J
197700980821     C                   Z-ADD     URIT(J)       RIT(J)
197800980821     C                   Z-ADD     UCON(J)       CON(J)
197900980821E   1C                   ENDDO
198000980821     C*
198100060829     C* DATI TIPO -SCAGLIONI- ricavi
198200980824DO  1C     1             DO        10            J
198300980825     C                   Z-ADD     USSP(J)       SSP(J)
198400980825     C                   Z-ADD     UPSC(J)       PSC(J)
198500980825     C                   Z-ADD     UCSC(J)       CSC(J)
198600980825     C                   Z-ADD     UVSC(J)       VSC(J)
198700980824E   1C                   ENDDO
198800060829     C* DATI TIPO -SCAGLIONI- competenze
198900060829DO  1C     1             DO        10            J
199000060829     C                   Z-ADD     USSP1(J)      SSP1(J)
199100060829     C                   Z-ADD     UPSC1(J)      PSC1(J)
199200060829     C                   Z-ADD     UCSC1(J)      CSC1(J)
199300060829     C                   Z-ADD     UVSC1(J)      VSC1(J)
199400060829E   1C                   ENDDO
199500980821     C*
199600980821     C* DATI TIPO -CODICI TASSAZIONE-
199700050103DO  1C     1             DO        600           J                 4 0
199800980825     C                   Z-ADD     UTSI(J)       TSI(J)
199900980825     C                   Z-ADD     UTSE(J)       TSE(J)
200000980824E   1C                   ENDDO
200100980821     C*
200200980821     C                   ENDSR
200300980821     C*------------------------------------------------------------------------*
200400980821     C* AZZSCH - AZZERA LE VARIABILI DI LAVORO
200500980821     C*------------------------------------------------------------------------*
200600980821     C     AZZSCH        BEGSR
200700980821     C*
200800980821     C                   CLEAR                   SPE                            -GENERALI-
200900980821     C                   CLEAR                   CLA
201000980821     C                   CLEAR                   CLB
201100980821     C                   CLEAR                   CLC
201101160726     C                   CLEAR                   CLE
201200980821     C                   CLEAR                   BON
201300980821     C                   CLEAR                   BOA
201400980821     C                   CLEAR                   BOB
201500980821     C                   CLEAR                   NCL
201600980821     C                   CLEAR                   PKB
201700980821     C                   CLEAR                   VLB
201800060829     C                   CLEAR                   NCL1
201900060829     C                   CLEAR                   PKB1
202000060829     C                   CLEAR                   VLB1
202100980821     C                   CLEAR                   FRA
202200980821     C                   CLEAR                   ASS
202300980821     C                   CLEAR                   PCV
202400980821     C                   CLEAR                   PCM
202500980821     C                   CLEAR                   PFD
202600980821     C                   CLEAR                   ESP
202700980821     C                   CLEAR                   PPV
202800980821     C                   CLEAR                   PCA
202900980821     C                   CLEAR                   PCP
203000980821     C                   CLEAR                   PCS
203100980821     C                   CLEAR                   PCF
203200980821     C                   CLEAR                   PCD
203300980821     C                   CLEAR                   PCC
203400980821     C                   CLEAR                   PCL
203500980821     C                   CLEAR                   PAB
203600980821     C                   CLEAR                   PAM
203700980821     C                   CLEAR                   CAS
203800980821     C                   CLEAR                   PAR
203900980821     C                   CLEAR                   PAC
204000060315     C                   CLEAR                   xx1
204100980821     C                   CLEAR                   ASC
204200980821     C                   CLEAR                   PGI
204300980821     C                   CLEAR                   SGP
204400980821     C                   CLEAR                   GGP
204500980821     C                   CLEAR                   RIT
204600980821     C                   CLEAR                   CON
204700980901     C                   CLEAR                   CLT
204800000414     C                   CLEAR                   GGS
204900000414     C                   CLEAR                   NRG
205000980821     C                   CLEAR                   SSP                            -SCAGLIONI-
205100980821     C                   CLEAR                   PSC
205200980821     C                   CLEAR                   CSC
205300980821     C                   CLEAR                   VSC
205400060829     C                   CLEAR                   SSP1                           -SCAGLIONI-
205500060829     C                   CLEAR                   PSC1
205600060829     C                   CLEAR                   CSC1
205700060829     C                   CLEAR                   VSC1
205800980821     C                   CLEAR                   TSI                            -CODICI TASSAZIONE-
205900980821     C                   CLEAR                   TSE
206000011011     C                   CLEAR                   CIS
206100011011     C                   CLEAR                   RIS
206200020215     C                   CLEAR                   CLD
206300050808     C                   CLEAR                   pks
206400050808     C                   CLEAR                   pkn
206500040802     C                   CLEAR                   nba
206501160726     C                   CLEAR                   nb01
206502160726     C                   CLEAR                   nb02
206503160726     C                   CLEAR                   nb03
206504160726     C                   CLEAR                   sban
206505160726     C                   CLEAR                   sb01
206506160726     C                   CLEAR                   sb02
206507160726     C                   CLEAR                   sb03
206600040824     C                   CLEAR                   sban
206700040824     C                   CLEAR                   sbnb
206800040811     C                   CLEAR                   nbn
206900040802     C                   CLEAR                   ded
207000040802     C                   CLEAR                   val
207100980821     C*
207200980821     C                   ENDSR
207300980821     C*------------------------------------------------------------------------*
207400011001     C* SCRETC - SCRIVE UN RECORD DI TEETC -GENERALI-
207500980821     C*------------------------------------------------------------------------*
207600980821     C     SCRETC        BEGSR
207700980821     C*
207800060804     C                   MOVEL     'S'           KTCDSF
207900980821     C                   MOVEL     PARCTO        KTCCTO
208000980821     C                   Z-ADD     PARKOV        KTCKSC
208100980821     C                   Z-ADD     PARCTR        KTCCTR
208200980821     C                   Z-ADD     PARPRG        KTCPRG
208300980821     C                   Z-ADD     XORG          KTCLNP
208400021122     C     KEYETC        CHAIN     TEETC01L                           9697
208500980821     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
208600021122IF  2C     *IN97         IFEQ      *OFF                                         *NON ALLOCATO
208700021122IF  3C     *IN96         IFEQ      *ON
208800980821     C                   MOVEL     KTCDSF        ETCDSF
208900980821     C                   MOVEL     KTCCTO        ETCCTO
209000980821     C                   Z-ADD     KTCKSC        ETCKSC
209100980821     C                   Z-ADD     KTCCTR        ETCCTR
209200980821     C                   Z-ADD     KTCPRG        ETCPRG
209300980821     C                   Z-ADD     KTCLNP        ETCLNP
209400980821     C                   Z-ADD     DATCOR        ETCDIR
209500980821     C                   Z-ADD     *ZEROS        ETCDUV
209600021122     C                   MOVEL     *BLANKS       ETCATB
209700011001     C                   WRITE     TEETC000                                     *INSERIMENTO
209800011001     C                   UNLOCK    TEETC01L
209900980821X   3C                   ELSE
210000980821     C                   Z-ADD     DATCOR        ETCDUV
210100021122     C                   MOVEL     *BLANKS       ETCATB
210200011001     C                   UPDATE    TEETC000                                     *VARIAZIONE
210300980821E   3C                   ENDIF
210400980821X   2C                   ELSE                                                   *ALLOCATO
210500980821     C                   MOVEL     METX(1)       DEPERR
210600980821     C                   MOVEL     'X'           DEPTER
210700980821     C                   EXSR      STAERR
210800980821E   2C                   ENDIF
210900980821     C*
211000980821     C                   ENDSR
211100980821     C*------------------------------------------------------------------------*
211200011001     C* SCRETS - SCRIVE UN RECORD DI TEETS -SCAGLIONI-
211300980821     C*------------------------------------------------------------------------*
211400980821     C     SCRETS        BEGSR
211500980821     C*
211600980821     C                   EXSR      SCRSGL                                       *SRIVE SCHIERE SGL
211700980821DO  1C     1             DO        10            J
211800980821IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
211900060804     C                   MOVEL     'S'           KTSDSF
212000980821     C                   MOVEL     PARCTO        KTSCTO
212100980821     C                   Z-ADD     PARKOV        KTSKSC
212200980821     C                   Z-ADD     PARCTR        KTSCTR
212300980821     C                   Z-ADD     PARPRG        KTSPRG
212400980821     C                   Z-ADD     XORG          KTSLNP
212500980821     C                   Z-ADD     §SG(J)        KTSSGL
212600021122     C     KEYETS        CHAIN     TEETS01L                           96
212700980821     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
212800021122IF  4C     *IN96         IFEQ      *ON
212900980821     C                   MOVEL     KTSDSF        ETSDSF
213000980821     C                   MOVEL     KTSCTO        ETSCTO
213100980821     C                   Z-ADD     KTSKSC        ETSKSC
213200980821     C                   Z-ADD     KTSCTR        ETSCTR
213300980821     C                   Z-ADD     KTSPRG        ETSPRG
213400980821     C                   Z-ADD     KTSLNP        ETSLNP
213500980821     C                   Z-ADD     KTSSGL        ETSSGL
213600980821     C                   Z-ADD     DATCOR        ETSDIR
213700980821     C                   Z-ADD     *ZEROS        ETSDUV
213800021122     C                   MOVEL     *BLANKS       ETSATB
213900011001     C                   WRITE     TEETS000                                     *INSERIMENTO
214000011001     C                   UNLOCK    TEETS01L
214100980821X   4C                   ELSE
214200980821     C                   Z-ADD     DATCOR        ETSDUV
214300021122     C                   MOVEL     *BLANKS       ETSATB
214400011001     C                   UPDATE    TEETS000                                     *VARIAZIONE
214500980821E   4C                   ENDIF
214600980821E   2C                   ENDIF
214700980821E   1C                   ENDDO                                                  *FINE SCAGLIONI
214800980821     C*
214900980821     C                   ENDSR
215000060829     C*------------------------------------------------------------------------*
215100060829     C* SCRETS1- SCRIVE UN RECORD DI TEETS10F -SCAGLIONI-
215200060829     C*------------------------------------------------------------------------*
215300060829     C     SCRETS1       BEGSR
215400060829     C*
215500060829     C                   EXSR      SCRSGL1                                      *SRIVE SCHIERE SGL
215600060829DO  1C     1             DO        10            J
215700060829IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
215800060829     C                   MOVEL     'S'           KTSDSF
215900060829     C                   MOVEL     PARCTO        KTSCTO
216000060829     C                   Z-ADD     PARKOV        KTSKSC
216100060829     C                   Z-ADD     PARCTR        KTSCTR
216200060829     C                   Z-ADD     PARPRG        KTSPRG
216300060829     C                   Z-ADD     XORG          KTSLNP
216400060829     C                   Z-ADD     §SG(J)        KTSSGL
216500060829     C     KEYETS        CHAIN     TEETS11L                           96
216600060829     C                   EXSR      IMPETS1                                      *IMPOSTA I CAMPI
216700060829IF  4C     *IN96         IFEQ      *ON
216800060829     C                   MOVEL     KTSDSF        £ETSDSF
216900060829     C                   MOVEL     KTSCTO        £ETSCTO
217000060829     C                   Z-ADD     KTSKSC        £ETSKSC
217100060829     C                   Z-ADD     KTSCTR        £ETSCTR
217200060829     C                   Z-ADD     KTSPRG        £ETSPRG
217300060829     C                   Z-ADD     KTSLNP        £ETSLNP
217400060829     C                   Z-ADD     KTSSGL        £ETSSGL
217500060829     C                   Z-ADD     DATCOR        £ETSDIR
217600060829     C                   Z-ADD     *ZEROS        £ETSDUV
217700060829     C                   MOVEL     *BLANKS       £ETSATB
217800060829     C                   WRITE     TEETS100                                     *INSERIMENTO
217900060829     C                   UNLOCK    TEETS11L
218000060829X   4C                   ELSE
218100060829     C                   Z-ADD     DATCOR        £ETSDUV
218200060829     C                   MOVEL     *BLANKS       £ETSATB
218300060829     C                   UPDATE    TEETS100                                     *VARIAZIONE
218400060829E   4C                   ENDIF
218500060829E   2C                   ENDIF
218600060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
218700060829     C*
218800060829     C                   ENDSR
218900980821     C*------------------------------------------------------------------------*
219000141009     C* scrive i record fitizi per cap
219100980821     C*------------------------------------------------------------------------*
219200141009     C     srxxx         BEGSR
219300141009     c                   clear                   numspe            1 0
219400141009     c                   clear                   numcap            4 0
219500141008     C/EXEC SQL
219600141009     C+ select count(*) into :numcap from (
219700141008     C+ select tblkey, substr(cpccap, int(da), int(per)) from (
219800141008     C+ SELECT tblkey, substr(tbluni, 27, 1) da, substr(tbluni, 26, 1) per,
219900141008     C+ AZCPC00F.CPCCAP  FROM tabel00f join azcpc00f on
220000141008     C+ tblkey = cpcnar
220100141009     C+ WHERE tblcod ='15' and cpccts = :kttcts and cpcver = :o95ver
220200141008     C+ GROUP BY tblkey, substr(tbluni,
220300141008     C+ 27, 1) , substr(tbluni, 26, 1) , AZCPC00F.CPCCAP )
220400141009     C+ a group by tblkey, substr(cpccap, int(da), int(per)) ) as conta
220500141008     C/END-EXEC
220600141009     c*
220700141009     C/EXEC SQL
220800141009     C+ DECLARE a1 CURSOR FOR
220900141009     C+ select tblkey, substr(cpccap, int(da), int(per)) from (
221000141009     C+ SELECT tblkey, substr(tbluni, 27, 1) da, substr(tbluni, 26, 1) per,
221100141009     C+ AZCPC00F.CPCCAP  FROM tabel00f join azcpc00f on
221200141009     C+ tblkey = cpcnar
221300141009     C+ WHERE tblcod ='15' and cpccts = :kttcts and cpcver = :o95ver
221400141009     C+ GROUP BY tblkey, substr(tbluni,
221500141009     C+ 27, 1) , substr(tbluni, 26, 1) , AZCPC00F.CPCCAP )
221600141009     C+ a group by tblkey, substr(cpccap, int(da), int(per))
221700141009     C/END-EXEC
221800141002     c*
221900141002     C/EXEC SQL
222000141002     C+ OPEN a1
222100141002     C/END-EXEC
222200141002
222300141002     C                   DO        *HIVAL
222400141002
222500141002     C/EXEC SQL
222600141008     C+ FETCH NEXT FROM a1 INTO :kttnaz, :kttcad
222700141002     C/END-EXEC
222800141002
222900141009     c                   IF        SQLCod = 100 or sqlcod < 0
223000141002     C                   LEAVE
223100141002     C                   ENDIF
223200141008     c*
223300141008     c                   exsr      scrett10
223400141008     C                   ENDdo
223500141002
223600141002     C/EXEC SQL
223700141002     C+ CLOSE a1
223800141002     C/END-EXEC
223900141016     c* se non ho cap per il codice tassazione forzo la scrittura di un rec.
224000141016     c                   if        numcap = 0
224100141017     C                   MOVEL     METX(4)       DEPERR
224200141017     C                   MOVEL     'C'           DEPTER
224300141017     C                   EXSR      STAERR
224400141016     C                   clear                   kttnaz
224500141016     C                   clear                   kTTcad
224600141016     c                   exsr      scrett10
224700141016     c                   end
224800141016     c                   endsr
224900140919     C*------------------------------------------------------------------------*
225000140919     C* IMPETC - IMPOSTA I CAMPI DATI TIPO -GENERALI-
225100140919     C*------------------------------------------------------------------------*
225200140919     C     IMPETC        BEGSR
225300970110     C*---
225400970110     C* CAMPI CALCOLATI
225500970110     C*---
225600970110     C                   Z-ADD     *ZEROS        WMAX              9 0
225700980901     C                   MOVEL     'C'           ETCCLV
225800020215IF  1C     CLD(I)        IFGT      WMAX
225900020215     C                   Z-ADD     CLD(I)        WMAX
226000020215     C                   MOVEL     'D'           ETCCLV
226100020215E   1C                   ENDIF
226200980901IF  1C     CLT(I)        IFGT      WMAX
226300980901     C                   Z-ADD     CLT(I)        WMAX
226400020215     C                   MOVEL     'T'           ETCCLV
226500980901E   1C                   ENDIF
226600970110IF  1C     CLA(I)        IFGT      WMAX
226700970110     C                   Z-ADD     CLA(I)        WMAX
226800970110     C                   MOVEL     'A'           ETCCLV
226900970110E   1C                   ENDIF
227000970110IF  1C     CLB(I)        IFGT      WMAX
227100970110     C                   Z-ADD     CLB(I)        WMAX
227200970110     C                   MOVEL     'B'           ETCCLV
227300970110E   1C                   ENDIF
227400970110IF  1C     CLC(I)        IFGT      WMAX
227500970110     C                   Z-ADD     CLC(I)        WMAX
227600970110     C                   MOVEL     'C'           ETCCLV
227700970110E   1C                   ENDIF
227701160726IF  1C     CLE(I)        IFGT      WMAX
227702160726     C                   Z-ADD     CLE(I)        WMAX
227703160726     C                   MOVEL     'E'           ETCCLV
227704160726E   1C                   ENDIF
227800050808     c* packing list
227900050808     C                   Z-ADD     *ZEROS        Wmax
228000050808     C                   MOVEL     'N'           ETCpkl
228100050808IF  1C     pks(I)        IFGT      wmax
228200050808     C                   Z-ADD     pks(I)        Wmax
228300050808     C                   MOVEL     'S'           ETCpkl
228400050808E   1C                   ENDIF
228500050808IF  1C     pkn(I)        IFGT      wmax
228600050808     C                   Z-ADD     pkn(I)        Wmax
228700050808     C                   MOVEL     'N'           ETCpkl
228800050808E   1C                   ENDIF
228900970110     C*
229000970110     C                   Z-ADD     *ZEROS        WMAX              9 0
229100970110     C                   MOVEL     ' '           ETCBOL
229200970110IF  1C     BON(I)        IFGT      WMAX
229300970110     C                   Z-ADD     BON(I)        WMAX
229400970110     C                   MOVEL     ' '           ETCBOL
229500970110E   1C                   ENDIF
229600970415IF  1C     BOA(I)        IFGT      WMAX                                         *"A" VINCE SU " "
229700970415     C     BOA(I)        ORGT      *ZEROS
229800970110     C                   Z-ADD     BOA(I)        WMAX
229900970110     C                   MOVEL     'A'           ETCBOL
230000970110E   1C                   ENDIF
230100970415IF  1C     BOB(I)        IFGT      WMAX                                         *"B" VINCE SU "A"
230200970415     C     BOB(I)        ORGT      *ZEROS
230300970110     C                   Z-ADD     BOB(I)        WMAX
230400970110     C                   MOVEL     'B'           ETCBOL
230500970110E   1C                   ENDIF
230600060829     C* ricavi
230700970110IF  1C     NCL(I)        IFGT      *ZEROS
230800970110     C     PKB(I)        DIV       NCL(I)        ETCPMC
230900970110     C     VLB(I)        DIV       NCL(I)        ETCVMC
231000970703E   1C                   ENDIF
231100970703IF  1C     ETCPMC        IFEQ      *ZEROS
231200970110     C                   Z-ADD     0,1           ETCPMC
231300970703E   1C                   ENDIF
231400970703IF  1C     ETCVMC        IFEQ      *ZEROS
231500970110     C                   Z-ADD     0,001         ETCVMC
231600970703E   1C                   ENDIF
231700060829     c* competenze
231800060829IF  1C     NCL1(I)       IFGT      *ZEROS
231900060829     C     PKB1(I)       DIV       NCL1(I)       ETCPMC1
232000060829     C     VLB1(I)       DIV       NCL1(I)       ETCVMC1
232100060829E   1C                   ENDIF
232200060829IF  1C     ETCPMC1       IFEQ      *ZEROS
232300060829     C                   Z-ADD     0,1           ETCPMC1
232400060829E   1C                   ENDIF
232500060829IF  1C     ETCVMC1       IFEQ      *ZEROS
232600060829     C                   Z-ADD     0,001         ETCVMC1
232700060829E   1C                   ENDIF
232800970110     C*
232900011026     C     FRA(I)        MULT      100           WN153            21 8
233000011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
233100970110     C                   Z-ADD     WPER          ETCFRA
233200011001     C     ASS(I)        MULT      100           WN153
233300011001     C     WN153         DIV(H)    SPE(I)        WPER
233400970110     C                   Z-ADD     WPER          ETCASS
233500980803IF  1C     ETCFRA        IFGE      ETCASS
233600980803     C                   Z-ADD     *ZEROS        ETCASS
233700980831     C                   Z-ADD     100           ETCFRA
233800980803X   1C                   ELSE
233900980803     C                   Z-ADD     *ZEROS        ETCFRA
234000980831     C                   Z-ADD     100           ETCASS
234100980803E   1C                   ENDIF
234200970110     C*
234300011001     C     PCV(I)        MULT      100           WN153
234400011001     C     WN153         DIV(H)    SPE(I)        WPER
234500970110     C                   Z-ADD     WPER          ETCPCV
234600040802     C*
234700011001     C     PCM(I)        MULT      100           WN153
234800011001     C     WN153         DIV(H)    SPE(I)        WPER
234900970110     C                   Z-ADD     WPER          ETCPCM
235000970110     C*
235100011001     C     PFD(I)        MULT      100           WN153
235200011001     C     WN153         DIV(H)    SPE(I)        WPER
235300970110     C                   Z-ADD     WPER          ETCPFD
235400970110     C*
235500011001     C     ESP(I)        MULT      100           WN153
235600011001     C     WN153         DIV(H)    SPE(I)        WPER
235700970110     C                   Z-ADD     WPER          ETCESP
235800970110     C*
235900011001     C     PPV(I)        MULT      100           WN153
236000011001     C     WN153         DIV(H)    SPE(I)        WPER
236100970110     C                   Z-ADD     WPER          ETCPPV
236200970110     C*
236300011001     C     PCA(I)        MULT      100           WN153
236400011001     C     WN153         DIV(H)    SPE(I)        WPER
236500970110     C                   Z-ADD     WPER          ETCPCA
236600970110     C*
236700011001     C     PCP(I)        MULT      100           WN153
236800011001     C     WN153         DIV(H)    SPE(I)        WPER
236900970110     C                   Z-ADD     WPER          ETCPCP
237000970110     C*
237100011001     C     PCS(I)        MULT      100           WN153
237200011001     C     WN153         DIV(H)    SPE(I)        WPER
237300970110     C                   Z-ADD     WPER          ETCPCS
237400970110     C*
237500011001     C     PCF(I)        MULT      100           WN153
237600011001     C     WN153         DIV(H)    SPE(I)        WPER
237700970110     C                   Z-ADD     WPER          ETCPCF
237800970415     C*
237900011001     C     PCD(I)        MULT      100           WN153
238000011001     C     WN153         DIV(H)    SPE(I)        WPER
238100970415     C                   Z-ADD     WPER          ETCPCD
238200970415     C*
238300011001     C     PCC(I)        MULT      100           WN153
238400011001     C     WN153         DIV(H)    SPE(I)        WPER
238500970415     C                   Z-ADD     WPER          ETCPCC
238600970415     C*
238700011001     C     PCL(I)        MULT      100           WN153
238800011001     C     WN153         DIV(H)    SPE(I)        WPER
238900970415     C                   Z-ADD     WPER          ETCPCL
239000970110     C*
239100011001     C     PAB(I)        MULT      100           WN153
239200011001     C     WN153         DIV(H)    SPE(I)        WPER
239300970110     C                   Z-ADD     WPER          ETCPAB
239400970110     C*
239500011001     C     PAM(I)        MULT      100           WN153
239600011001     C     WN153         DIV(H)    SPE(I)        WPER
239700970110     C                   Z-ADD     WPER          ETCPAM
239800011011     C*
239900011011     C     CIS(I)        MULT      100           WN153
240000011011     C     WN153         DIV(H)    SPE(I)        WPER
240100011011     C                   Z-ADD     WPER          ETCCIS
240200011011     C*
240300011011     C     RIS(I)        MULT      100           WN153
240400011011     C     WN153         DIV(H)    SPE(I)        WPER
240500011011     C                   Z-ADD     WPER          ETCSAM
240600970110     C*
240700011001     C     PAB(I)        ADD       PAM(I)        WN153
240800970110     C                   Z-ADD     *ZEROS        ETCCAS
240900011001IF  1C     WN153         IFGT      *ZEROS
241000011001     C     CAS(I)        DIV       WN153         ETCCAS
241100970110E   1C                   ENDIF
241200011026     C                   Z-ADD     ETCCAS        $IMP             21 8
241300970417     C                   MOVEL     'E'           $FDE              1
241400011011     C                   IF        PARDIV = 'ITL'
241500011026     C                   Z-ADD     1000          $ARR             21 8
241600011011     C                   ELSE
241700011026     C                   Z-ADD     1             $ARR             21 8
241800011011     C                   ENDIF
241900970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
242000970110     C                   Z-ADD     $IMP          ETCCAS
242100970110     C*
242200970418     C     PAR(I)        ADD       PAC(I)        WN90              9 0
242300060315     C                   ADD       xx1(I)        WN90
242400011001     C     PAR(I)        MULT      100           WN153
242500011001     C     WN153         DIV(H)    WN90          WPER
242600970110     C                   Z-ADD     WPER          ETCPAR
242700970110     C*
242800011001     C     PAC(I)        MULT      100           WN153
242900011001     C     WN153         DIV(H)    WN90          WPER
243000970110     C                   Z-ADD     WPER          ETCPAC
243100060315     C*
243200060315     C     xx1(I)        MULT      100           WN153
243300060315     C     WN153         DIV(H)    WN90          WPER
243400060315     C                   Z-ADD     WPER          ETCxx1
243500970110     C*
243600970110     C                   Z-ADD     *ZEROS        ETCASC
243700970110IF  1C     PAC(I)        IFGT      *ZEROS
243800970110     C     ASC(I)        DIV       PAC(I)        ETCASC
243900970110E   1C                   ENDIF
244000970110     C                   Z-ADD     ETCASC        $IMP
244100970110     C                   MOVEL     'E'           $FDE
244200011011     C                   IF        PARDIV = 'ITL'
244300011026     C                   Z-ADD     1000          $ARR             21 8
244400011011     C                   ELSE
244500011026     C                   Z-ADD     1             $ARR             21 8
244600011011     C                   ENDIF
244700970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
244800970110     C                   Z-ADD     $IMP          ETCASC
244900970110     C*
245000000414     C                   Z-ADD     *ZEROS        ETCGGG
245100000414     C                   Z-ADD     *ZEROS        ETCNRG
245200011001     C     PGI(I)        MULT      100           WN153
245300011001     C     WN153         DIV(H)    SPE(I)        WPER
245400970110     C                   Z-ADD     WPER          ETCPGI
245500021122     C* SE LA % GIACENZE SUL FILE VIENE A 0 => AZZERO ANCHE I GG GIACENZA ED IL NUMERO APERTURE GIAC
245600021122     C                   IF        ETCPGI = *ZEROS
245700021122     C                   Z-ADD     *ZEROS        ETCGGG
245800021122     C                   Z-ADD     *ZEROS        ETCNRG
245900021122     C                   ELSE
246000000414IF  1C     PGI(I)        IFGT      *ZEROS
246100000414     C     GGS(I)        DIV       PGI(I)        ETCGGG                         *GG SOSTA
246200000414IF  2C     ETCGGG        IFEQ      *ZEROS
246300000414     C                   Z-ADD     1             ETCGGG
246400000414E   2C                   ENDIF
246500000414     C     NRG(I)        DIV       PGI(I)        ETCNRG                         *N° RIAPERTURE
246600000414IF  2C     ETCNRG        IFEQ      *ZEROS
246700000414     C                   Z-ADD     1             ETCNRG
246800000414E   2C                   ENDIF
246900000414E   1C                   ENDIF
247000021122     C                   ENDIF
247100970110     C*
247200970110     C                   Z-ADD     *ZEROS        WFRQ              3 0
247300011001     C                   Z-ADD     *ZEROS        WN153
247400970110DO  1C     1             DO        30            J
247500011001IF  2C     SGP(J)        IFGT      WN153
247600011001     C                   Z-ADD     SGP(J)        WN153
247700970110     C                   Z-ADD     GGP(J)        WFRQ
247800970110E   2C                   ENDIF
247900970110E   1C                   ENDDO
248000970110     C                   Z-ADD     WFRQ          ETCGGP
248100970110IF  1C     ETCGGP        IFEQ      999
248200970110     C                   Z-ADD     *ZEROS        ETCGGP
248300970110E   1C                   ENDIF
248400970110     C*
248500970110     C                   Z-ADD     *ZEROS        WSPE             11 0
248600000411     C                   Z-ADD     *ZEROS        WSTP             13 0
248700970110DO  1C     1             DO        999           J
248800970110IF  2C     RIT(J)        IFGT      *ZEROS
248900000411     C     RIT(J)        DIV       J             WN13             13 0
249000000411     C                   MVR                     RESTO            15 0
249100000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
249200000411     C     J             ANDNE     999                                          *STOP <> 999
249300000411     C                   ADD       1             WN13
249400000411E   3C                   ENDIF
249500970110     C     WSPE          ADD       RIT(J)        WSPE
249600970110     C     WSTP          ADD       WN13          WSTP
249700970110E   2C                   ENDIF
249800970110E   1C                   ENDDO
249900970110     C                   Z-ADD     *ZEROS        ETCSPR
250000970110IF  1C     WSTP          IFGT      *ZEROS
250100970110     C     WSPE          DIV       WSTP          ETCSPR
250200000411     C                   MVR                     RESTO            15 0
250300000411IF  2C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
250400000411     C                   ADD       1             ETCSPR                         *ARROTONDA X ECCESSO
250500000411E   2C                   ENDIF
250600000411E   1C                   ENDIF
250700970110IF  1C     ETCSPR        IFEQ      *ZEROS
250800970110     C                   Z-ADD     1             ETCSPR
250900970110E   1C                   ENDIF
251000970110     C*
251100970110     C                   Z-ADD     *ZEROS        WSPE             11 0
251200000411     C                   Z-ADD     *ZEROS        WSTP             13 0
251300970110DO  1C     1             DO        999           J
251400970110IF  2C     CON(J)        IFGT      *ZEROS
251500000411     C     CON(J)        DIV       J             WN13             13 0
251600000411     C                   MVR                     RESTO            15 0
251700000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
251800000411     C     J             ANDNE     999                                          *STOP <> 999
251900000411     C                   ADD       1             WN13
252000000411E   3C                   ENDIF
252100970110     C     WSPE          ADD       CON(J)        WSPE
252200970110     C     WSTP          ADD       WN13          WSTP
252300970110E   2C                   ENDIF
252400970110E   1C                   ENDDO
252500001221     C                   Z-ADD     1             ETCSPC
252600970110     C*---
252700970110     C* CAMPI FISSI
252800970110     C*---
252900970110     C                   MOVEL     *BLANKS       ETCSDE
253000970110     C                   Z-ADD     *ZEROS        ETCPDE
253100970619     C                   MOVEL     'E'           ETCFAR
253200011011     C                   IF        PARDIV = 'ITL'
253300970314     C                   Z-ADD     100           ETCIAR
253400011011     C                   ELSE
253500011030     C                   Z-ADD     0,1           ETCIAR
253600011011     C                   ENDIF
253700970213     C                   MOVEL     'N'           ETCSIN
253800970110     C                   MOVEL     'N'           ETCIAP
253900970110     C                   MOVEL     'N'           ETCIPI
254000970110     C                   MOVEL     'N'           ETCISU
254100970110     C                   MOVEL     'N'           ETCIFU
254200970415     C                   MOVEL     'N'           ETCIDD
254300970415     C                   MOVEL     'N'           ETCICE
254400970415     C                   MOVEL     'N'           ETCILO
254500050810     C                   MOVEL     'N'           ETCIde
254600050810     C                   MOVEL     'N'           ETCIba
254700970110     C                   MOVEL     'N'           ETCIES
254800970110     C                   MOVEL     'N'           ETCICA
254900970110     C                   MOVEL     'N'           ETCIAS
255000970110     C                   MOVEL     'N'           ETCIGI
255100020716     C                   MOVEL     'N'           ETCISO
255200020716     C                   MOVEL     'N'           ETCINR
255300980901     C                   MOVEL     METX(3)       ETCDCV
255400970110     C*
255500970110     C                   MOVEL     *BLANKS       ETCFTR
255600970110     C                   Z-ADD     *ZEROS        ETCDTR
255700970110     C                   MOVEL     *BLANKS       ETCATB
255800021104     C*
255900021104     C* EFFETTUO ALCUNE CONSIDERAZIONI
256000021104     C                   IF        ETCBOL = 'B'
256100021104     C                   EVAL      ETCIX1 = 'K'
256200021104     C                   ELSE
256300021104     C                   EVAL      ETCIX1 = *blanks
256400021104     C                   ENDIF
256500040811     C* %sped oltre 20 colli non imbancalate
256600040811     C     sbnb(I)       MULT      100           WN153
256700040802     C     WN153         DIV(H)    SPE(I)        WPER
256800040802     C                   Z-ADD     WPER          ETCbnb
256900040811     c* numero medio bancali x spedizioni oltre 20 colli
257000061006     c                   if        sbnb(i) <> 0 and etcbnb <> 0
257100040811     c     nbn(i)        div(h)    sbnb(i)       etcnbn
257200061006     c                   if        etcnbn = 0
257300061006     c                   eval      etcnbn = 1
257400061006     c                   end
257500040812     c                   else
257600040812     c                   clear                   etcnbn
257700040811     c                   end
257800040824     C* %sped oltre 20 colli non imbancalate
257900040824     C     sban(I)       MULT      100           WN153
258000040824     C     WN153         DIV(H)    SPE(I)        WPER
258100040824     C                   Z-ADD     WPER          ETCban
258200040824     c* numero medio bancali Bartolini
258300061006     c                   if        sban(i) <> 0 and etcban <> 0
258400040824     c     nba(i)        div(h)    sban(i)       etcnba
258500061006     c                   if        etcnba = 0
258600061006     c                   eval      etcnba = 1
258700061006     c                   end
258701160726     c* come tipo bancale metto quello usato maggiormente nelle spedizoni
258702160726     c                   eval      §etctba ='01'
258703160726     c                   if        sb02(i) > sb01(i) and
258704160726     c                             sb02(i) > sb03(i)
258705160726     c                   eval      §etctba ='02'
258706160726     c                   end
258707160726     c                   if        sb03(i) > sb01(i) and
258708160726     c                             sb03(i) > sb02(i)
258709160726     c                   eval      §etctba ='03'
258710160726     c                   end
258800040824     c                   else
258900040824     c                   clear                   etcnba
259000040824     c                   end
259100040802     C*
259200040802     C     ded(I)        MULT      100           WN153
259300040802     C     WN153         DIV(H)    SPE(I)        WPER
259400040802     C                   Z-ADD     WPER          ETCdes
259500040802     C*
259600040802     C     val(I)        MULT      100           WN153
259700040802     C     WN153         DIV(H)    SPE(I)        WPER
259800040802     C                   Z-ADD     WPER          ETCval
259900970110     C*
260000970110     C                   ENDSR
260100970416     C*------------------------------------------------------------------------*
260200060829     C* SCRSGL - SCRIVE SCHIERE SCAGLIONI ricavi
260300970416     C*------------------------------------------------------------------------*
260400970416     C     SCRSGL        BEGSR
260500970416     C*
260600970416     C* AZZERA LE VARIABILI DI LAVORO
260700970416     C                   CLEAR                   §SG
260800970416     C                   CLEAR                   §PS
260900970416     C                   CLEAR                   §CS
261000970416     C                   CLEAR                   §VS
261100970416     C                   CLEAR                   §SP
261200971008     C                   CLEAR                   §SD
261300970416     C*
261400970416     C* PER CIASCUN SCAGLIONE DA SCRIVERE
261500970416DO  1C     1             DO        10            J
261600970416IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
261700970416     C                   Z-ADD     SSGL(J)       §SG(J)
261800970416IF  3C     SSP(J)        IFGT      *ZEROS
261900970416     C*
262000970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
262100971008     C                   MOVEL     *BLANKS       §SD(J)                         *VALORE NON DEDOTTO
262200970416     C     PSC(J)        DIV       SSP(J)        §PS(J)
262300970416     C     CSC(J)        DIV       SSP(J)        §CS(J)
262400970416     C     VSC(J)        DIV       SSP(J)        §VS(J)
262500011001     C     SSP(J)        MULT      100           WN153
262600011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
262700970416     C                   Z-ADD     WPER          §SP(J)
262800970703IF  4C     §SP(J)        IFEQ      *ZEROS
262900970703     C                   Z-ADD     0,1           §SP(J)
263000970703E   4C                   ENDIF
263100970416X   3C                   ELSE
263200970416     C*
263300970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
263400971008     C                   MOVEL     '*'           §SD(J)                         *VALORE DEDOTTO
263500970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
263600970416     C     PARTTR        OREQ      '3'                                          *KG
263700970416     C                   Z-ADD     §SG(J)        §PS(J)
263800970416     C     §PS(J)        DIV       ETCPMC        §CS(J)
263900970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
264000970416X   4C                   ELSE                                                   *COLLI
264100970416     C                   Z-ADD     §SG(J)        §CS(J)
264200040812     C     §CS(J)        mult      ETCPMC        §PS(J)
264300970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
264400970416E   4C                   ENDIF
264500970416     C                   Z-ADD     0,1           §SP(J)
264600970416E   3C                   ENDIF
264700970416E   2C                   ENDIF
264800970416E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
264900000707     C*
265000000707     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
265100000707     C                   EXSR      PARPSG
265200970416     C*
265300970416     C                   ENDSR
265400060829     C*------------------------------------------------------------------------*
265500060829     C* SCRSGL1- SCRIVE SCHIERE SCAGLIONI competenze
265600060829     C*------------------------------------------------------------------------*
265700060829     C     SCRSGL1       BEGSR
265800060829     C*
265900060829     C* AZZERA LE VARIABILI DI LAVORO
266000060829     C                   CLEAR                   §SG
266100060829     C                   CLEAR                   §PS1
266200060829     C                   CLEAR                   §CS1
266300060829     C                   CLEAR                   §VS1
266400060829     C                   CLEAR                   §SP1
266500060829     C                   CLEAR                   §SD1
266600060829     C*
266700060829     C* PER CIASCUN SCAGLIONE DA SCRIVERE
266800060829DO  1C     1             DO        10            J
266900060829IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
267000060829     C                   Z-ADD     SSGL(J)       §SG(J)
267100060829IF  3C     SSP1(J)       IFGT      *ZEROS
267200060829     C*
267300060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
267400060829     C                   MOVEL     *BLANKS       §SD1(J)                        *VALORE NON DEDOTTO
267500060829     C     PSC1(J)       DIV       SSP1(J)       §PS1(J)
267600060829     C     CSC1(J)       DIV       SSP1(J)       §CS1(J)
267700060829     C     VSC1(J)       DIV       SSP1(J)       §VS1(J)
267800060829     C     SSP1(J)       MULT      100           WN153
267900060829     C     WN153         DIV(H)    SPE(I)        WPER              4 1
268000060829     C                   Z-ADD     WPER          §SP1(J)
268100060829IF  4C     §SP1(J)       IFEQ      *ZEROS
268200060829     C                   Z-ADD     0,1           §SP1(J)
268300060829E   4C                   ENDIF
268400060829X   3C                   ELSE
268500060829     C*
268600060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
268700060829     C                   MOVEL     '*'           §SD1(J)                        *VALORE DEDOTTO
268800060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
268900060829     C     PARTTR        OREQ      '3'                                          *KG
269000060829     C                   Z-ADD     §SG(J)        §PS1(J)
269100060829     C     §PS1(J)       DIV       ETCPMC1       §CS1(J)
269200060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
269300060829X   4C                   ELSE                                                   *COLLI
269400060829     C                   Z-ADD     §SG(J)        §CS1(J)
269500060829     C     §CS1(J)       mult      ETCPMC1       §PS1(J)
269600060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
269700060829E   4C                   ENDIF
269800060829     C                   Z-ADD     0,1           §SP1(J)
269900060829E   3C                   ENDIF
270000060829E   2C                   ENDIF
270100060829E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
270200060829     C*
270300060829     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
270400060829     C                   EXSR      PARPSG1
270500060829     C*
270600060829     C                   ENDSR
270700000707     C*------------------------------------------------------------------------*
270800000707     C* PARPSG - PAREGGIA LA %TOTALE -SCAGLIONI- A 100
270900000707     C*------------------------------------------------------------------------*
271000000707     C     PARPSG        BEGSR
271100000707     C*
271200000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
271300000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
271400000707DO  1C     1             DO        10            J
271500000707IF  2C     §SG(J)        IFGT      *ZEROS
271600000707     C                   ADD       §SP(J)        TOTPPE                         *%INCIDENZA TOTALE
271700000707E   2C                   ENDIF
271800000707E   1C                   ENDDO
271900000707     C*
272000000707     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
272100000707     C                   Z-ADD     *ZEROS        WN41              4 1
272200000707     C     100           SUB       TOTPPE        WN41
272300000707     C*
272400000707     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
272500000707IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
272600000707DO  2C     WN41          DOWNE     *ZEROS
272700000707DO  3C     1             DO        10            J
272800000707IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
272900000707     C* POSITIVA -> AGGIUNGERE
273000000707IF  5C     WN41          IFGT      *ZEROS
273100000707     C     §SP(J)        ADD       0,1           §SP(J)                         *AGGIUNGE %
273200000707     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
273300000707E   5C                   ENDIF
273400000707     C* NEGATIVA -> TOGLIERE
273500000707IF  5C     WN41          IFLT      *ZEROS
273600000707IF  6C     §SP(J)        IFGT      0,1
273700000707     C     §SP(J)        SUB       0,1           §SP(J)                         *SOTTRAE %
273800000707     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
273900000707E   6C                   ENDIF
274000000707E   5C                   ENDIF
274100000707     C* UGUALE A ZERO -> USCIRE
274200000707IF  5C     WN41          IFEQ      *ZEROS
274300000707     C                   LEAVE
274400000707E   5C                   ENDIF
274500000707E   4C                   ENDIF
274600000707E   3C                   ENDDO                                                  *FINE SCAGLIONI
274700000707E   2C                   ENDDO                                                  *DIFFERENZA = 0
274800000707E   1C                   ENDIF
274900000707     C*
275000000707     C                   ENDSR
275100060829     C*------------------------------------------------------------------------*
275200060829     C* PARPSG1- PAREGGIA LA %TOTALE -SCAGLIONI- A 100
275300060829     C*------------------------------------------------------------------------*
275400060829     C     PARPSG1       BEGSR
275500060829     C*
275600060829     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
275700060829     C                   Z-ADD     *ZEROS        TOTPPE            4 1
275800060829DO  1C     1             DO        10            J
275900060829IF  2C     §SG(J)        IFGT      *ZEROS
276000060829     C                   ADD       §SP1(J)       TOTPPE                         *%INCIDENZA TOTALE
276100060829E   2C                   ENDIF
276200060829E   1C                   ENDDO
276300060829     C*
276400060829     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
276500060829     C                   Z-ADD     *ZEROS        WN41              4 1
276600060829     C     100           SUB       TOTPPE        WN41
276700060829     C*
276800060829     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
276900060829IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
277000060829DO  2C     WN41          DOWNE     *ZEROS
277100060829DO  3C     1             DO        10            J
277200060829IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
277300060829     C* POSITIVA -> AGGIUNGERE
277400060829IF  5C     WN41          IFGT      *ZEROS
277500060829     C     §SP1(J)       ADD       0,1           §SP1(J)                        *AGGIUNGE %
277600060829     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
277700060829E   5C                   ENDIF
277800060829     C* NEGATIVA -> TOGLIERE
277900060829IF  5C     WN41          IFLT      *ZEROS
278000060829IF  6C     §SP1(J)       IFGT      0,1
278100060829     C     §SP1(J)       SUB       0,1           §SP1(J)                        *SOTTRAE %
278200060829     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
278300060829E   6C                   ENDIF
278400060829E   5C                   ENDIF
278500060829     C* UGUALE A ZERO -> USCIRE
278600060829IF  5C     WN41          IFEQ      *ZEROS
278700060829     C                   LEAVE
278800060829E   5C                   ENDIF
278900060829E   4C                   ENDIF
279000060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
279100060829E   2C                   ENDDO                                                  *DIFFERENZA = 0
279200060829E   1C                   ENDIF
279300060829     C*
279400060829     C                   ENDSR
279500970110     C*------------------------------------------------------------------------*
279600060829     C* IMPETS - IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- ricavi
279700970110     C*------------------------------------------------------------------------*
279800970318     C     IMPETS        BEGSR
279900970110     C*---
280000970110     C* CAMPI CALCOLATI
280100970110     C*---
280200970416     C                   Z-ADD     §PS(J)        ETSPSC
280300970416     C                   Z-ADD     §CS(J)        ETSCSC
280400981020IF  1C     ETSCSC        IFLT      1
280500981020     C                   Z-ADD     1             ETSCSC
280600981020E   1C                   ENDIF
280700970416     C                   Z-ADD     §VS(J)        ETSVSC
280800970416     C                   Z-ADD     §SP(J)        ETSPPE
280900970415     C*---
281000970415     C* CAMPI FISSI
281100970415     C*---
281200980325     C                   Z-ADD     *ZEROS        ETSINL
281300970415     C                   Z-ADD     *ZEROS        ETSRPV
281400970415     C                   MOVEL     *BLANKS       ETSSDE
281500970415     C                   Z-ADD     *ZEROS        ETSPDE
281600970110     C*
281700040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
281800040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
281900040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
282000040805     C* MA È GIUSTO COSÌ
282100040805     C                   MOVEL     §SD(J)        ETSFTR
282200970110     C                   Z-ADD     *ZEROS        ETSDTR
282300970110     C                   MOVEL     *BLANKS       ETSATB
282400970110     C*
282500970110     C                   ENDSR
282600060829     C*------------------------------------------------------------------------*
282700060829     C* IMPETS1- IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- competenze
282800060829     C*------------------------------------------------------------------------*
282900060829     C     IMPETS1       BEGSR
283000060829     C*---
283100060829     C* CAMPI CALCOLATI
283200060829     C*---
283300060829     C                   Z-ADD     §PS1(J)       £ETSPSC
283400060829     C                   Z-ADD     §CS1(J)       £ETSCSC
283500060829IF  1C     £ETSCSC       IFLT      1
283600060829     C                   Z-ADD     1             £ETSCSC
283700060829E   1C                   ENDIF
283800060829     C                   Z-ADD     §VS1(J)       £ETSVSC
283900060829     C                   Z-ADD     §SP1(J)       £ETSPPE
284000060829     C*---
284100060829     C* CAMPI FISSI
284200060829     C*---
284300060829     C                   Z-ADD     *ZEROS        £ETSINL
284400060829     C                   Z-ADD     *ZEROS        £ETSRPV
284500060829     C                   MOVEL     *BLANKS       £ETSSDE
284600060829     C                   Z-ADD     *ZEROS        £ETSPDE
284700060829     C*
284800060829     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
284900060829     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
285000060829     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
285100060829     C* MA È GIUSTO COSÌ
285200060829     C                   MOVEL     §SD1(J)       £ETSFTR
285300060829     C                   Z-ADD     *ZEROS        £ETSDTR
285400060829     C                   MOVEL     *BLANKS       £ETSATB
285500060829     C*
285600060829     C                   ENDSR
285700141008     C*------------------------------------------------------------------------*
285800141008     C* SCRETT - SCRIVE UN RECORD DI TEETT -CODICI TASSAZIONE-
285900141008     C*------------------------------------------------------------------------*
286000141008     C     SCRETT        BEGSR
286100141008     C*
286200141008     c                   clear                   totinc            8 5
286300141008     C                   EXSR      SCRCTS                                       *SRIVE SCHIERE CTS
286400141008DO  1C     1             DO        600           J                 4 0
286500141008IF  2C     §CT(J)        IFNE      *BLANKS                                      *CODICE ESISTENTE
286600141009     C                   MOVEL     pardsf        KTTDSF
286700141008     C                   MOVEL     PARCTO        KTTCTO
286800141008     C                   Z-ADD     PARKOV        KTTKSC
286900141008     C                   Z-ADD     PARCTR        KTTCTR
287000141008     C                   Z-ADD     PARPRG        KTTPRG
287100141008     C                   Z-ADD     XORG          KTTLNP
287200141008     C                   MOVEL     Xntw          KTTntw
287300141008     C                   MOVEL     XFIE          KTTFIE
287400141008     C                   MOVEL     §CT(J)        KTTCTS
287500141008     C                   EXSR      IMPETT10                                     *IMPOSTA I CAMPI
287600141008     c                   end
287700141008E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
287800141009     C                   EXSR      PARPCT1
287900141008     C*
288000141008     C                   ENDSR
288100141008     C*------------------------------------------------------------------------*
288200141008     C* SCRETT10 - SCRIVE TEETT10 -CODICI TASSAZIONE- cap
288300141008     C*------------------------------------------------------------------------*
288400141008     C     SCRETT10      BEGSR
288500141008     c*
288600141009     c                   clear                   teett10ds
288700141009     C                   MOVEL     pardsf        KTTDSF
288800141009     C                   MOVEL     PARCTO        KTTCTO
288900141009     C                   Z-ADD     PARKOV        KTTKSC
289000141009     C                   Z-ADD     PARCTR        KTTCTR
289100141009     C                   Z-ADD     PARPRG        KTTPRG
289200141015     C     Kett          CHAIN     TEETT11L                           96
289300141008IF  4C     *IN96         IFEQ      *ON
289400141010     C                   MOVEL     KTTDSF        ETTDSF
289500141010     C                   MOVEL     KTTCTO        ETTCTO
289600141010     C                   Z-ADD     KTTKSC        ETTKSC
289700141010     C                   Z-ADD     KTTCTR        ETTCTR
289800141010     C                   Z-ADD     KTTPRG        ETTPRG
289900141010     C                   Z-ADD     KTTLNP        ETTLNP
290000141010     C                   MOVEL     KTTFIE        ETTFIE
290100141010     C                   MOVEL     KTTCTS        ETTCTS
290200141010     C                   MOVEL     KTTnaz        ETTnaz
290300141010     C                   MOVEL     KTTcad        ETTcad
290400141010     C                   MOVEL     KTTntw        ETTntw
290500141010     C                   Z-ADD     DATCOR        ETTDIR
290600141009     c* memcap = 0 mentre sto scrivendo i record CAP "fitizi" quindi
290700141008     c* non ho spedizioni
290800141010     C                   Z-ADD     1             ETTnsp
290900141009     c                   if        numspe = 0
291000141010     C                   Z-ADD     0             ETTnsp
291100141010     C                   z-add     §cp(j)        ETTppe
291200141016     c* se non ci sono cap per il codice tassazione, forzo la % del cod.
291300141016     c* tassazione in quella del cap
291400141016     c                   if        numcap = 0
291500141016     c                   eval(h)   ettinc = §cp(j)
291600141016     c                   else
291700141016     c                   eval(h)   ettinc = §cp(j)/numcap
291800141016     c                   end
291900141010     c                   add       ettinc        totinc
292000141010     C                   MOVEL     §RC(J)        ETTRCT
292100141010     C                   MOVEL     §CD(J)        ETTFTR
292200141008     c                   end
292300141010     C                   Z-ADD     *ZEROS        ETTDUV
292400141010     C                   MOVEL     *BLANKS       ETTATB
292500141008     C                   WRITE     TEETT100                                     *INSERIMENTO
292600141008     C                   UNLOCK    TEETT11L
292700141008X   4C                   ELSE
292800141010     c                   if        ettatb = ' '
292900141010     C                   add       1             ETTnsp
293000141008     c                   else
293100141010     c                   z-add     1             ettnsp
293200141008     c                   end
293300141009     c                   if        numspe = 0
293400141010     C                   Z-ADD     0             ETTnsp
293500141010     C                   z-add     §cp(j)        ETTppe
293600141016     c* se non ci sono cap per il codice tassazione, forzo la % del cod.
293700141016     c* tassazione in quella del cap
293800141016     c                   if        numcap = 0
293900141016     c                   eval(h)   ettinc = §cp(j)
294000141016     c                   else
294100141016     c                   eval(h)   ettinc = §cp(j)/numcap
294200141016     c                   end
294300141010     c                   add       ettinc        totinc
294400141010     C                   MOVEL     §RC(J)        ETTRCT
294500141010     C                   MOVEL     §CD(J)        ETTFTR
294600141009     c                   end
294700141010     C                   Z-ADD     DATCOR        ETTDUV
294800141010     C                   MOVEL     *BLANKS       ETTATB
294900141008     C                   UPDATE    TEETT100                                     *VARIAZIONE
295000141008E   4C                   ENDIF
295100141008     C*
295200141008     C                   ENDSR
295300970416     C*------------------------------------------------------------------------*
295400970416     C* SCRCTS - SCRIVE SCHIERE CODICI TASSAZIONE
295500970416     C*------------------------------------------------------------------------*
295600970416     C     SCRCTS        BEGSR
295700000707     C*---
295800000707     C* ITALIA
295900000707     C*---
296000000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
296100000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
296200000707     C                   CLEAR                   §RC
296300000707     C                   CLEAR                   §CP
296400000707     C                   CLEAR                   §CD
296500980821IF  1C     XFIE          IFEQ      'I'
296600970423     C                   XFOOT     TSI           TOTSPE           11 0          *TOT. SPED. ITALIA
296700050103DO  2C     1             DO        600           J
296800970423IF  3C     CCTI(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
296900000620     C     CRCI(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
297000970423     C                   MOVEL     CCTI(J)       §CT(J)
297100970423     C                   MOVEL     CRCI(J)       §RC(J)
297200970417     C*
297300970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
297400971008     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
297500970423IF  4C     TSI(J)        IFGT      *ZEROS
297600011001     C     TSI(J)        MULT      100           WN153
297700011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
297800970417     C                   Z-ADD     WPER          §CP(J)
297900970703IF  5C     §CP(J)        IFEQ      *ZEROS
298000970703     C                   Z-ADD     0,1           §CP(J)
298100970703E   5C                   ENDIF
298200970423X   4C                   ELSE
298300970417     C*
298400970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
298500971008     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
298600970417     C                   Z-ADD     0,1           §CP(J)
298700970423E   4C                   ENDIF
298800970423E   3C                   ENDIF
298900970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
299000000707     C*
299100000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
299200000707     C                   EXSR      PARPCT
299300970423     C*---
299400970423     C* ESTERO
299500970423     C*---
299600140916     C* PER CIASCUN CODICE TASSAZIONE -estero- DA SCRIVERE
299700970423X   1C                   ELSE
299800000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
299900000707     C                   CLEAR                   §RC
300000000707     C                   CLEAR                   §CP
300100000707     C                   CLEAR                   §CD
300200970423     C                   XFOOT     TSE           TOTSPE           11 0          *TOT. SPED. ITALIA
300300050103DO  2C     1             DO        600           J
300400970423IF  3C     CCTE(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
300500000620     C     CRCE(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
300600970423     C                   MOVEL     CCTE(J)       §CT(J)
300700970423     C                   MOVEL     CRCE(J)       §RC(J)
300800970423     C*
300900970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
301000990803     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
301100970423IF  4C     TSE(J)        IFGT      *ZEROS
301200011001     C     TSE(J)        MULT      100           WN153
301300011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
301400970423     C                   Z-ADD     WPER          §CP(J)
301500970703IF  5C     §CP(J)        IFEQ      *ZEROS
301600970703     C                   Z-ADD     0,1           §CP(J)
301700970703E   5C                   ENDIF
301800970423X   4C                   ELSE
301900970423     C*
302000970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
302100990803     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
302200970423     C                   Z-ADD     0,1           §CP(J)
302300970423E   4C                   ENDIF
302400970423E   3C                   ENDIF
302500970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
302600000707     C*
302700000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
302800000707     C                   EXSR      PARPCT
302900000707E   1C                   ENDIF
303000000707     C*
303100000707     C                   ENDSR
303200140919     C*------------------------------------------------------------------------*
303300140919     C* PARPCT - PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
303400140919     C*------------------------------------------------------------------------*
303500140919     C     PARPCT        BEGSR
303600140919     C*
303700140919     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
303800140919     C                   Z-ADD     *ZEROS        TOTPPE            4 1
303900140919DO  1C     1             DO        600           J
304000140919IF  2C     §CT(J)        IFNE      *BLANKS
304100140919     C                   ADD       §CP(J)        TOTPPE                         *%INCIDENZA TOTALE
304200140919E   2C                   ENDIF
304300140919E   1C                   ENDDO
304400140919     C*
304500140919     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
304600140919     C                   Z-ADD     *ZEROS        WN41              4 1
304700140919     C     100           SUB       TOTPPE        WN41
304800140919     C*
304900140919     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
305000140919IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
305100140919DO  2C     WN41          DOWNE     *ZEROS
305200140919DO  3C     1             DO        600           J
305300140919IF  4C     §CT(J)        IFNE      *BLANKS                                      *ELEMENTO PIENO
305400140919     C     §RC(J)        ANDNE     *BLANKS                                      *DA CALCOLARE
305500140919     C* POSITIVA -> AGGIUNGERE
305600140919IF  5C     WN41          IFGT      *ZEROS
305700140919     C     §CP(J)        ADD       0,1           §CP(J)                         *AGGIUNGE %
305800140919     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
305900140919E   5C                   ENDIF
306000140919     C* NEGATIVA -> TOGLIERE
306100140919IF  5C     WN41          IFLT      *ZEROS
306200140919IF  6C     §CP(J)        IFGT      0,1
306300140919     C     §CP(J)        SUB       0,1           §CP(J)                         *SOTTRAE %
306400140919     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
306500140919E   6C                   ENDIF
306600140919E   5C                   ENDIF
306700140919     C* UGUALE A ZERO -> USCIRE
306800140919IF  5C     WN41          IFEQ      *ZEROS
306900140919     C                   LEAVE
307000140919E   5C                   ENDIF
307100140919E   4C                   ENDIF
307200140919E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
307300140919E   2C                   ENDDO                                                  *DIFFERENZA = 0
307400140919E   1C                   ENDIF
307500140919     C*
307600140919     C                   ENDSR
307700141010     C*------------------------------------------------------------------------*
307800140919     C* IMPETT10 - IMPOSTA I CAMPI DATI TIPO -CODICI TASSAZIONE- CAP
307900140919     C*------------------------------------------------------------------------*
308000140919     C     IMPETT10      BEGSR
308100140922     c* calcolo il totale spedizioni x codice tassazione
308200140922     c* solo se ci sono spedizioni
308300140922     c                   clear                   totspec           8 0
308400141015     c     Kettcts       setll     teett11l
308500140922     c                   do        *hival
308600141015     c     Kettcts       reade(n)  teett11l
308700140922     c                   if        %eof(teett11l)
308800140922     c                   leave
308900140922     c                   end
309000141010     c                   add       ettnsp        totspec
309100140922     c                   enddo
309200140922     c*
309300141008     c                   if        totspec = 0
309400141008     c                   exsr      srxxx
309500140922     c                   leavesr
309600140926     c                   end
309700141015     c     kettcts       setll     teett11l
309800140922     c                   do        *hival
309900141015     c     kettcts       reade     teett11l
310000140922     c                   if        %eof(teett11l)
310100140922     c                   leave
310200140922     c                   end
310300141008     c*
310400141010     C                   MOVEL     §RC(J)        ETTRCT
310500141010     C                   MOVEL     §CD(J)        ETTFTR
310600141010     C                   MOVEL     §Cp(J)        ETTppe
310700140922     c*
310800141010IF  4C     ettnsp        IFGT      *ZEROS
310900141010     C     ettnsp        DIV(H)    TOTSPEc       ettinc
311000141010     C                   mult(h)   §cp(j)        ettinc
311100141010     c                   add       ettinc        totinc
311200141010     C                   Z-ADD     *ZEROS        ETTDTR
311300141010     C                   MOVEL     *BLANKS       ETTATB
311400140922     c                   update    teett100
311500140922     c                   end
311600140922E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
311700140919     C*
311800140919     C                   ENDSR
311900140924     C*------------------------------------------------------------------------*
312000140924     C* PARPCT1- PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
312100140924     C*------------------------------------------------------------------------*
312200140924     C     PARPCT1       BEGSR
312300140924     C*
312400141010     c                   clear                   savcts            2
312500141010     c                   clear                   totettinc        10 5
312600140924     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
312700140924     C                   Z-ADD     *ZEROS        Winc              8 5
312800140924     C     100           SUB       TOTinc        Winc
312900140924     C*
313000140924     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
313100140924IF  1C     Winc          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
313200140924DO  2C     Winc          DOWNE     *ZEROS
313300141015     c     kettntw       setll     teett11l
313400140924     c                   do        *hival
313500141015     c     kettntw       reade     teett11l
313600140924     c                   if        %eof(teett11l)
313700140924     c                   leave
313800140924     c                   end
313900141016     c*
314000141010     c                   if        ettcts <> savcts
314100141010     c                   clear                   totettinc
314200141010     c                   eval      savcts = ettcts
314300141010     c                   end
314400140924     C* POSITIVA -> AGGIUNGERE
314500140924IF  5C     Winc          IFGT      *ZEROS
314600141010     C     ettinc        ADD       0,00001       ettinc                         *AGGIUNGE %
314700140925     C     Winc          SUB       0,00001       Winc                           *DIMINUISCE DIFFER.
314800140924E   5C                   ENDIF
314900140924     C* NEGATIVA -> TOGLIERE
315000140924IF  5C     Winc          IFLT      *ZEROS
315100141010IF  6C     ettinc        IFGT      0,00001
315200141010     C     ettinc        SUB       0,00001       ettinc                         *SOTTRAE %
315300140925     C     Winc          ADD       0,00001       Winc                           *AUMENTA DIFFER.
315400140924E   6C                   ENDIF
315500140924E   5C                   ENDIF
315600140924     c                   update    teett100
315700140924     C* UGUALE A ZERO -> USCIRE
315800140924IF  5C     Winc          IFEQ      *ZEROS
315900140924     C                   LEAVEsr
316000140924E   5C                   ENDIF
316100141010     c                   add       ettinc        totettinc
316200141015     c                   if        totettinc >= ettppe
316300141010     c                   movel     ettntw        kttntw
316400141010     c                   movel     ettcts        kttcts
316500141015     c     kettcts       setgt     teett11l
316600141010     c                   end
316700141010     c*
316800140924E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
316900140924E   2C                   ENDDO                                                  *DIFFERENZA = 0
317000140924E   1C                   ENDIF
317100140924     C*
317200140924     C                   ENDSR
317300970417     C*------------------------------------------------------------------------*
317400970417     C* ARRIMP - ARROTONDA IMPORTO
317500970417     C*------------------------------------------------------------------------*
317600970417     C     ARRIMP        BEGSR
317700970417     C*---
317800970417     C* $IMP -> IMPORTO DA ARROTONDARE
317900970417     C* $FDE -> 'D'=DIFETTO / E=ECCESSO
318000970417     C* $ARR  : IMPORTO AL QUALE ARROTONDARE
318100970417     C*---
318200970417IF  1C     $IMP          IFGT      *ZEROS                                       *IMPORTO PIENO
318300970417IF  2C     $ARR          IFGT      *ZEROS                                       *SI ARROTONDAMENTO
318400970417     C     $IMP          DIV       $ARR          COMODO           15 0
318500970417     C                   MVR                     RESTO            15 0
318600970417IF  3C     $FDE          IFEQ      'E'
318700970417IF  4C     RESTO         IFGT      *ZEROS
318800970417     C                   ADD       1             COMODO
318900970417E   4C                   ENDIF
319000970417E   3C                   ENDIF
319100970417     C     COMODO        MULT      $ARR          $IMP
319200970417E   2C                   ENDIF
319300970417E   1C                   ENDIF
319400970417     C*
319500970417     C                   ENDSR
319600970110     C*------------------------------------------------------------------------*
319700970110     C* STAERR - STAMPA RIGA DI ERRORE
319800970110     C*------------------------------------------------------------------------*
319900970110     C     STAERR        BEGSR
320000970110     C*
320100970110IF  1C     *INOF         IFEQ      *ON
320200970110     C                   EXCEPT    TES
320300970110     C                   SETOFF                                       OF
320400970110E   1C                   ENDIF
320500141017     c                   select
320600141017IF  1C                   when      depter = 'T'
320700970110     C                   EXCEPT    ETAS
320800141017IF  1C                   when      depter = 'X'
320900970110     C                   EXCEPT    EETX
321000141017IF  1C                   when      depter = 'C'
321100141017     C                   EXCEPT    EETX
321200141017E   1C                   ENDsl
321300970110     C*
321400970110     C                   ENDSR
321500970313     C*------------------------------------------------------------------------*
321600970314     C* ANNETX - ANNULLA I DATI TIPO ESISTENTI
321700970313     C*------------------------------------------------------------------------*
321800970314     C     ANNETX        BEGSR
321900970313     C*
322000970314     C* PER OGNI FILIALE RICHIESTA
322100971124DO  1C     1             DO        500           I
322200970313IF  2C     SORG(I)       IFGT      *ZEROS
322300970313     C*---
322400970313     C* DATI TIPO -GENERALI-
322500970313     C*---
322600060804     C                   MOVEL     'S'           KTCDSF
322700980713     C                   MOVEL     PARCTO        KTCCTO
322800970313     C                   Z-ADD     PARKOV        KTCKSC
322900970313     C                   Z-ADD     PARCTR        KTCCTR
323000970313     C                   Z-ADD     PARPRG        KTCPRG
323100970313     C                   Z-ADD     SORG(I)       KTCLNP
323200011001     C     KEYETC        CHAIN     TEETC01L                           99
323300981221IF  3C     *IN99         IFEQ      *OFF
323400971024IF  4C     ETCATB        IFEQ      *BLANKS
323500970314     C                   MOVEL     'A'           ETCATB
323600970314     C                   MOVEL     *BLANKS       ETCFTR
323700011001     C                   UPDATE    TEETC000
323800970314E   4C                   ENDIF
323900970314E   3C                   ENDIF
324000970313     C*---
324100970313     C* DATI TIPO -SCAGLIONI-
324200970313     C*---
324300060829     c* ricavi
324400980713     C                   MOVEL     'S'           KTSDSF
324500980713     C                   MOVEL     PARCTO        KTSCTO
324600970313     C                   Z-ADD     PARKOV        KTSKSC
324700970313     C                   Z-ADD     PARCTR        KTSCTR
324800970313     C                   Z-ADD     PARPRG        KTSPRG
324900970313     C                   Z-ADD     SORG(I)       KTSLNP
325000011001     C     KE2ETS        SETLL     TEETS01L
325100011001     C     KE2ETS        READE     TEETS01L                               99
325200970314DO  3C     *IN99         DOWEQ     *OFF
325300971024IF  4C     ETSATB        IFEQ      *BLANKS
325400970314     C                   MOVEL     'A'           ETSATB
325500970314     C                   MOVEL     *BLANKS       ETSFTR
325600011001     C                   UPDATE    TEETS000
325700970314E   4C                   ENDIF
325800011001     C     KE2ETS        READE     TEETS01L                               99
325900060829E   3c                   ENDDO                                                  *FINE SCAGLIONI
326000060829     c* competetnze
326100060829     C     KE2ETS        SETLL     TEETS11L
326200060829     C     KE2ETS        READE     TEETS11L                               99
326300060829DO  3C     *IN99         DOWEQ     *OFF
326400060829IF  4C     £ETSATB       IFEQ      *BLANKS
326500060829     C                   MOVEL     'A'           £ETSATB
326600060829     C                   MOVEL     *BLANKS       £ETSFTR
326700060829     C                   UPDATE    TEETS100
326800060829E   4C                   ENDIF
326900060829     C     KE2ETS        READE     TEETS11L                               99
327000060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
327100970314     C*---
327200970314     C* DATI TIPO -CODICI TASSAZIONE-
327300970314     C*---
327400970429DO  3C     1             DO        2             K
327500970429IF  4C     SFIE(K)       IFNE      *BLANKS
327600980713     C                   MOVEL     'S'           KTTDSF
327700980713     C                   MOVEL     PARCTO        KTTCTO
327800970314     C                   Z-ADD     PARKOV        KTTKSC
327900970314     C                   Z-ADD     PARCTR        KTTCTR
328000970314     C                   Z-ADD     PARPRG        KTTPRG
328100970314     C                   Z-ADD     SORG(I)       KTTLNP
328200141010     C                   movel     Sntw(I)       KTTntw
328300970429     C                   MOVEL     SFIE(K)       KTTFIE
328400141015     C     Kettntw       SETLL     TEETT11L
328500141015     c                   do        *hival
328600141015     C     Kettntw       READE     TEETT11L                               99
328700141015     c                   if        %eof(teett11l)
328800141015     c                   leave
328900141015     c                   end
329000971024IF  6C     ETTATB        IFEQ      *BLANKS
329100970314     C                   MOVEL     'A'           ETTATB
329200970314     C                   MOVEL     *BLANKS       ETTFTR
329300141010     C                   UPDATE    TEETT100
329400970429E   6C                   ENDIF
329500970429E   5C                   ENDDO                                                  *FINE COD.TASSAZIONE
329600970429E   4C                   ENDIF
329700970429E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
329800970313     C*
329900970314E   2C                   ENDIF
330000970314E   1C                   ENDDO                                                  *FINE FILIALI
330100970314     C*
330200970313     C                   ENDSR
330300970110     C*------------------------------------------------------------------------*
330400970110     C* OPEFIL - APRE GLI ARCHIVI
330500970110     C*------------------------------------------------------------------------*
330600970110     C     OPEFIL        BEGSR
330700970110     C*
330800970110IF  1C     PARKSC        IFGT      *ZEROS
330900990812     C                   OPEN      TITAS31C                                     *LETTURA X CLIENTE
331000970110X   1C                   ELSE
331100990812     C                   OPEN      TITAS38C                                     *LETTURA X DATA
331200970110E   1C                   ENDIF
331300970110     C*
331400970110     C                   ENDSR
331500970110     C*------------------------------------------------------------------------*
331600970110     C* CLOFIL - CHIUDE GLI ARCHIVI
331700970110     C*------------------------------------------------------------------------*
331800970110     C     CLOFIL        BEGSR
331900970110     C*
332000970110IF  1C     PARKSC        IFGT      *ZEROS
332100990812     C                   CLOSE     TITAS31C                                     *LETTURA X CLIENTE
332200970110X   1C                   ELSE
332300990812     C                   CLOSE     TITAS38C                                     *LETTURA X DATA
332400970110E   1C                   ENDIF
332500970110     C*
332600970110     C                   ENDSR
332700011011     C*------------------------------------------------------------------------*
332800011011     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
332900011011     C*------------------------------------------------------------------------*
333000011011     C     CNVEUR        BEGSR
333100011011     C*
333200011011     C* AZZERA VARIABILI DI LAVORO
333300011011     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
333400011011     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
333500011026     C                   EVAL      NDEC = '9'
333600011011     C*
333700011011IF  1C     WIMI          IFNE      *ZEROS
333800011011     C                   CLEAR                   YEUR
333900011011     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
334000011011     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
334100011011     C                   MOVEL     PARDIV        YECDVO                         *DIVISA IMP CONVERTI
334200011011     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
334300011011     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
334400011011     C                   CALL      'YEURCO'
334500011011     C                   PARM                    YEUR
334600011011IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
334700011011     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
334800011011X   2C                   ELSE                                                   *SI ERRORI
334900011011     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
335000011011E   2C                   ENDIF
335100011011E   1C                   ENDIF
335200011011     C*
335300011011     C                   ENDSR
335400011011     C*------------------------------------------------------------------------*
335500011011     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
335600011011     C*------------------------------------------------------------------------*
335700011011     C     CARGEDCE      BEGSR
335800011011     C*
335900011011     C                   CLEAR                   DSGEDCE
336000011026     C                   MOVEL     *blanks       WDIVCE            3
336100011011     C*
336200011011     C                   MOVEL(P)  'GED'         KTECOD
336300011011     C                   MOVEL(P)  'CE'          KTEKE1
336400011011     C     KEYTBE        CHAIN     TNTBE01L
336500011011     C                   IF        %found(TNTBE01L)
336600011011     C                   MOVEL     TBEUNI        DSGEDCE
336700011011     C                   ENDIF
336800011011     C*
336900011011     C                   IF        §GEDCEDAT > DATCOR
337000011026     C                   EVAL      WDIVCE = §GEDCEDIP
337100011026     C                   ELSE
337200011026     C                   EVAL      WDIVCE = §GEDCEDIV
337300011011     C                   ENDIF
337400011011     C*
337500011011     C                   ENDSR
337600970110     C*------------------------------------------------------------------------*
337700970110     C* CARTAB - CARICAMENTO TABELLE
337800970110     C*------------------------------------------------------------------------*
337900970110     C     CARTAB        BEGSR
338000970110     C*--------------------
338100970110     C* ORGANIGRAMMA
338200970110     C*--------------------
338300020227     C                   Z-ADD     *ZEROS        JJ                3 0
338400970110     C                   Z-ADD     *ZEROS        I                 4 0
338500970110     C     *LOVAL        SETLL     AZORG01L
338600970110     C                   READ      AZORG01L                               99
338700970110DO  1C     *IN99         DOWEQ     *OFF
338800970110IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATE
338900970110     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
339000970110     C                   ADD       1             I
339100970110     C                   Z-ADD     ORGFIL        ORG(I)                         *FILIALE
339200970110     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
339300020730     C                   MOVEL     ORGDE3        OG143
339400020730     C*
339500020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
339600020730     C                   CLEAR                   DNTW
339700020730     C*
339800020730     C                   MOVEL(P)  'NTW'         KTECOD
339900020730     C                   MOVEL(P)  §OGNTW        KTEKE1
340000020730     C     KEYTBE        CHAIN     TNTBE01L
340100020730     C                   IF        %found(TNTBE01L)
340200020730     C                   MOVEL     TBEUNI        DNTW
340300020730     C                   ENDIF
340400020730     C*
340500020730     C                   MOVEL     §NTWFIE       FL1(I)                         *FLAG ITALIA/ESTERO
340600140919     C                   MOVEL     ORGDE3        OG143
340700970110     C* SELEZIONI
340800970430IF  3C     PARLNP        IFEQ      999                                          *TUTTE
340900970430     C                   Z-ADD     ORGFIL        SORG(I)                        *FILIALI SELEZIONATE
341000020730     C                   MOVEL     §NTWFIE       SFL1(I)                        *FLAG ITALIA/ESTERO
341100140919     C                   movel     §OGNTW        sntw(I)
341200970110X   3C                   ELSE
341300970110IF  4C     PARLNP        IFEQ      ORGFIL                                       *UNA FILIALE
341400970417     C                   Z-ADD     ORGFIL        SORG(1)                        *FILIALE SELEZIONATA
341500020730     C                   MOVEL     §NTWFIE       SFL1(1)                        *FLAG ITALIA/ESTERO
341600140919     C                   movel     §OGNTW        sntw(1)
341700970110E   4C                   ENDIF
341800970110E   3C                   ENDIF
341900081105     C* VERIFICO SE FILIALE dpd
342000140919     C                   IF        §OGNTW = 'DPD'
342100081105     C                   ADD       1             xx                3 0
342200081105     C                   MOVEL     ORGFIL        FILdpd(xx)
342300020227     C                   ENDIF
342400081105     C* VERIFICO SE FILIALE FEDEX
342500081105     C                   MOVEL     ORGDE3        OG143
342600081105     C                   IF        §OGNTW = 'FED'
342700081105     C                   ADD       1             JJ
342800081105     C                   MOVEL     ORGFIL        FILFED(JJ)
342900081105     C                   ENDIF
343000970110E   2C                   ENDIF
343100970110     C                   READ      AZORG01L                               99
343200970110E   1C                   ENDDO
343300970423     C*--------------------
343400970423     C* TARIFFA ITALIA/ESTERO
343500970423     C*--------------------
343600970423IF  1C     PARFIE        IFEQ      *BLANKS                                      *ITALIA E ESTERO
343700970423     C                   MOVEL     'I'           SFIE(1)
343800970423     C                   MOVEL     'E'           SFIE(2)
343900970423X   1C                   ELSE                                                   *ITALIA O ESTERO
344000970423     C                   MOVEL     PARFIE        SFIE(1)
344100970423E   1C                   ENDIF
344200970110     C*--------------------
344300970110     C* TIPI TARIFFA
344400970110     C*--------------------
344500970110     C                   Z-ADD     *ZEROS        I                 4 0
344600020923     C                   Z-ADD     1             KTBKUT
344700970110     C                   MOVEL     'TR'          KTBCOD
344800970110     C     KE2TAB        SETLL     TABEL00F
344900970110     C     KE2TAB        READE     TABEL00F                               99
345000970110DO  1C     *IN99         DOWEQ     *OFF
345100970110IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
345200970110     C                   MOVEL     TBLUNI        DSTR
345300970110IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
345400970110     C                   ADD       1             I
345500970110     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
345600970110     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
345700970110E   3C                   ENDIF
345800970110E   2C                   ENDIF
345900970110     C     KE2TAB        READE     TABEL00F                               99
346000970110E   1C                   ENDDO
346100141002     C*--------------------
346200141002     C* nazioni
346300141002     C*--------------------
346400141002     C                   Z-ADD     *ZEROS        I                 4 0
346500141002     C                   Z-ADD     1             KTBKUT
346600141002     C                   MOVEL     '15'          KTBCOD
346700141002     C     KE2TAB        SETLL     TABEL00F
346800141002     C     KE2TAB        READE     TABEL00F                               99
346900141002DO  1C     *IN99         DOWEQ     *OFF
347000141002IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
347100141002     C                   MOVEL     TBLUNI        DS15
347200141002     C                   ADD       1             I
347300141002     C                   MOVEL     TBLKEY        d15(I)                         *CODICE
347400141002     C                   MOVEL     §15pcf        pcf15(I)
347500141002     C                   MOVEL     §15ecf        ecf(I)
347600141002E   2C                   ENDIF
347700141002     C     KE2TAB        READE     TABEL00F                               99
347800141002E   1C                   ENDDO
347900050808     C*--------------------
348000050808     C* cod. trattamento merce con packing list
348100050808     C*--------------------
348200050808     C                   Z-ADD     *ZEROS        I                 4 0
348300050808     C                   Z-ADD     1             KTBKUT
348400050808     C                   MOVEL     '1B'          KTBCOD
348500050808     C     KE2TAB        SETLL     TABEL00F
348600050808     C     KE2TAB        READE     TABEL00F                               99
348700050808DO  1C     *IN99         DOWEQ     *OFF
348800050808IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
348900050808     C                   MOVEL     TBLUNI        DS1b
349000050808IF  3C     §1bf16        IFEQ      'S'                                          *IN GESTIONE
349100050808     C                   ADD       1             I
349200050808     C                   MOVEL     TBLKEY        cpkl(I)                        *CODICE
349300050808E   3C                   ENDIF
349400050808E   2C                   ENDIF
349500050808     C     KE2TAB        READE     TABEL00F                               99
349600050808E   1C                   ENDDO
349700970110     C*--------------------
349800970110     C* RAGGRUPPAMENTI TIPO BOLLA
349900970110     C*--------------------
350000970110     C                   Z-ADD     *ZEROS        I                 4 0
350100020923     C                   Z-ADD     1             KTBKUT
350200970110     C                   MOVEL     '2W'          KTBCOD
350300970110     C     KE2TAB        SETLL     TABEL00F
350400970110     C     KE2TAB        READE     TABEL00F                               99
350500970110DO  1C     *IN99         DOWEQ     *OFF
350600970110IF  2C     TBLFLG        IFEQ      *BLANKS
350700970110     C                   MOVEL     TBLUNI        DS2W
350800970110     C                   ADD       1             I
350900970110     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
351000970110     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
351100970110     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
351200970110E   2C                   ENDIF
351300970110     C     KE2TAB        READE     TABEL00F                               99
351400970110E   1C                   ENDDO
351500970110     C*--------------------
351600970110     C* TIPI BOLLA
351700970110     C*--------------------
351800970110     C                   Z-ADD     *ZEROS        I                 4 0
351900970110     C                   Z-ADD     *ZEROS        J                 4 0
352000020923     C                   Z-ADD     1             KTBKUT
352100970110     C                   MOVEL     'TB'          KTBCOD
352200970110     C     KE2TAB        SETLL     TABEL00F
352300970110     C     KE2TAB        READE     TABEL00F                               99
352400970110DO  1C     *IN99         DOWEQ     *OFF
352500970110IF  2C     TBLFLG        IFEQ      *BLANKS
352600970110     C                   MOVEL     TBLUNI        DSTB
352700970110     C                   ADD       1             I
352800970110     C                   MOVEL     TBLKEY        TBL(I)                         *TIPO BOLLA
352900970110     C                   Z-ADD     1             J
353000970110     C                   SETOFF                                           98
353100970110     C     §TBRBL        LOOKUP    RBL(J)                                 98
353200970110IF  3C     *IN98         IFEQ      *ON
353300970110     C                   MOVEL     FSP(J)        SPD(I)                         *FLAG SPEDIZIONE
353400970110     C                   MOVEL     FRI(J)        RIC(I)                         *FLAG RICAVO
353500970110X   3C                   ELSE
353600970110     C                   MOVEL     'N'           SPD(I)                         *FLAG SPEDIZIONE
353700970110     C                   MOVEL     'N'           RIC(I)                         *FLAG RICAVO
353800970110E   3C                   ENDIF
353900970110E   2C                   ENDIF
354000970110     C     KE2TAB        READE     TABEL00F                               99
354100970110E   1C                   ENDDO
354200970110     C*--------------------
354300970110     C* CODICI PAGAMENTO
354400970110     C*--------------------
354500970110     C                   Z-ADD     *ZEROS        I                 4 0
354600020923     C                   Z-ADD     1             KTBKUT
354700970110     C                   MOVEL     'FA'          KTBCOD
354800970110     C     KE2TAB        SETLL     TABEL00F
354900970110     C     KE2TAB        READE     TABEL00F                               99
355000970110DO  1C     *IN99         DOWEQ     *OFF
355100970110IF  2C     TBLFLG        IFEQ      *BLANKS
355200970110     C                   MOVEL     TBLUNI        DSFA
355300970110     C                   ADD       1             I
355400970110     C                   MOVEL     TBLKEY        CDP(I)                         *CODICE PAGAMENTO
355500970110     C                   MOVEL     §FARAT        CRAT(I)                        *RATE DI PAGAMENTO
355600970110E   2C                   ENDIF
355700970110     C     KE2TAB        READE     TABEL00F                               99
355800970110E   1C                   ENDDO
355900970313     C*--------------------
356000970313     C* CODICI TASSAZIONE
356100970313     C*--------------------
356200970313     C                   Z-ADD     *ZEROS        I                 4 0
356300020923     C                   Z-ADD     1             KTBKUT
356400970313     C                   MOVEL     'CT'          KTBCOD
356500970313     C     KE2TAB        SETLL     TABEL00F
356600970313     C     KE2TAB        READE     TABEL00F                               99
356700970313DO  1C     *IN99         DOWEQ     *OFF
356800970313IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
356900970313     C                   MOVEL     TBLUNI        DSCT
357000970317IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
357100970317     C                   ADD       1             I
357200970317     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
357300970317     C                   MOVEL     §CTDES        DCTS(I)                        *DESCRIZIONE
357400970317     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
357500970317     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
357600140919     C                   MOVEL     §CTnar        nar(I)                         *REGIONE
357700140919     C                   MOVEL     §CTnar        naz(I)                         *REGIONE
357800970317     C                   MOVE      §CTCOR        WA1               1            *TIPO CODICE
357900970417IF  4C     WA1           IFEQ      '9'
358000970417IF  5C     CTS(I)        IFEQ      'IT'
358100970317     C     CTS(I)        OREQ      'EE'
358200970317     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
358300970417X   5C                   ELSE
358400970317     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
358500970417E   5C                   ENDIF
358600970417X   4C                   ELSE
358700970317     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
358800970417E   4C                   ENDIF
358900970417IF  4C     §CTNAR        IFEQ      *BLANKS
359000970317     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
359100970317     C                   MOVEL     'I'           CIE(I)                         *CODICE ITALIA
359200970417X   4C                   ELSE
359300970317     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
359400970317     C                   MOVEL     'E'           CIE(I)                         *CODICE ESTERO
359500970417E   4C                   ENDIF
359600970317E   3C                   ENDIF
359700970317E   2C                   ENDIF
359800970317     C     KE2TAB        READE     TABEL00F                               99
359900970317E   1C                   ENDDO
360000040920     C* CARICO GLI SCAGLIONI DI DEFAULT
360100040920IF  1C     PARTTR        IFEQ      '0'                                          *QUINTALI
360200040920     C     PARTTR        OREQ      '3'                                          *KG
360300040920     C                   Z-ADD     2             SGL(1)
360400040920     C                   Z-ADD     5             SGL(2)
360500040920     C                   Z-ADD     10            SGL(3)
360600040920     C                   Z-ADD     25            SGL(4)
360700040920     C                   Z-ADD     50            SGL(5)
360800040920     C                   Z-ADD     75            SGL(6)
360900040920     C                   Z-ADD     100           SGL(7)
361000040920     C                   Z-ADD     99999         SGL(8)
361100040920X   1C                   ELSE                                                   *COLLI
361200040920     C                   Z-ADD     1             SGL(1)
361300040920     C                   Z-ADD     2             SGL(2)
361400040920     C                   Z-ADD     3             SGL(3)
361500040920     C                   Z-ADD     4             SGL(4)
361600040920     C                   Z-ADD     99999         SGL(5)
361700040920E   1C                   ENDIF
361800970110     C*
361900970110     C                   ENDSR
362000970313     C*------------------------------------------------------------------------*
362100970313     C* CARTAR - CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
362200970313     C*------------------------------------------------------------------------*
362300970313     C     CARTAR        BEGSR
362400970313     C*
362500970313     C* REPERISCE IL CODICE DELLA TARIFFA DI CARTELLO IN VIGORE
362600970314IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORE
362700021202     C                   Z-ADD     *ZEROS        CARKSC
362800021202     C                   Z-ADD     *ZEROS        CARCTR
362900021202     C                   MOVE(P)   *ZEROS        CARPRG
363000160317     C                   CLEAR                   TRULC7DS
363100160317     C                   EVAL      Ic7TLA = 'L'
363200160317     c                   eval      ic7tfie='I'
363300160317     C                   EVAL      IC7DTA = DATCOR
363400160317     C                   CALL      'TRULC7R'
363500160317     C                   PARM                    TRULC7DS
363600021202     C*
363700160317     C                   IF        OC7ERR <> 'E'
363800160317     C                   Z-ADD     Oc7KSCc       CARKSC
363900160317     C                   Z-ADD     Oc7CTRc       CARCTR
364000160317     C                   MOVE(P)   Oc7PRGc       CARPRG
364100021202     C                   ENDIF
364200021202E   1C                   ENDIF
364300970317     C*
364400970317     C* PER CIASCUNA FILIALE RICHIESTA
364500971124DO  1C     1             DO        500           Y                 4 0
364600970417IF  2C     SORG(Y)       IFGT      *ZEROS
364700970423     C     Y             OCCUR     FTAR
364800970317     C*---
364900970317     C* CARICA GLI SCAGLIONI DA CONSIDERARE -CLIENTE O CARTELLO-
365000970317     C*---
365100970416     C                   Z-ADD     *ZEROS        SSGL
365200970313     C                   Z-ADD     PARKOV        KTDKSC
365300970313     C                   Z-ADD     PARCTR        KTDCTR
365400970313     C                   Z-ADD     PARPRG        KTDPRG
365500970417     C                   Z-ADD     SORG(Y)       KTDLNP
365600970313     C                   EXSR      CARSGL
365700970417IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
365800970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
365900970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
366000970417     C                   Z-ADD     CARKSC        KTDKSC
366100970313     C                   Z-ADD     CARCTR        KTDCTR
366200970313     C                   Z-ADD     CARPRG        KTDPRG
366300970417     C                   Z-ADD     SORG(Y)       KTDLNP
366400970314     C                   EXSR      CARSGL
366500970703     C                   MOVEL     DEPCTO        PARCTO
366600970418E   3C                   ENDIF
366700970418IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
366800040920     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
366900040920     c                   move      sgl           ssgl
367000970418E   3C                   ENDIF
367100970317     C*---
367200970313     C* CARICA I CODICI TASSAZIONE DA CONSIDERARE -CLIENTE O CARTELLO-
367300970317     C*---
367400970423     C                   MOVEL     *BLANKS       CCTI
367500970423     C                   MOVEL     *BLANKS       CRCI
367600970423     C                   MOVEL     *BLANKS       CCTE
367700970423     C                   MOVEL     *BLANKS       CRCE
367800970423DO  3C     1             DO        2             W                 4 0
367900970423IF  4C     SFIE(W)       IFNE      *BLANKS
368000970314     C                   Z-ADD     PARKOV        KTDKSC
368100970314     C                   Z-ADD     PARCTR        KTDCTR
368200970314     C                   Z-ADD     PARPRG        KTDPRG
368300970417     C                   Z-ADD     SORG(Y)       KTDLNP
368400971204     C                   MOVEL     SFL1(Y)       DLPFL1
368500970314     C                   EXSR      CARCTS
368600970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
368700970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
368800970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
368900970314     C                   Z-ADD     CARKSC        KTDKSC
369000970314     C                   Z-ADD     CARCTR        KTDCTR
369100970314     C                   Z-ADD     CARPRG        KTDPRG
369200970417     C                   Z-ADD     SORG(Y)       KTDLNP
369300971204     C                   MOVEL     SFL1(Y)       DLPFL1
369400970314     C                   EXSR      CARCTS
369500970703     C                   MOVEL     DEPCTO        PARCTO
369600970423E   5C                   ENDIF
369700970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
369800970418     C                   EXSR      FIXCTS                                       *IMPOSTA FISSI CTS
369900970423E   5C                   ENDIF
370000970416     C                   EXSR      RAPSVI                                       *CTS->CTS ELEMENTARI
370100970423E   4C                   ENDIF
370200970423E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
370300970417     C*
370400970417E   2C                   ENDIF
370500970417E   1C                   ENDDO                                                  *FINE FILIALI
370600970313     C*
370700970313     C                   ENDSR
370800970313     C*------------------------------------------------------------------------*
370900970313     C* CARSGL - CARICA GLI SCAGLIONI
371000970313     C*------------------------------------------------------------------------*
371100970313     C     CARSGL        BEGSR
371200970313     C*
371300970313     C* AZZERA LE VARIABILI DI LAVORO
371400970313     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
371500970313     C                   Z-ADD     *ZEROS        I                 4 0
371600970313     C*
371700011128     C                   CLEAR                   DEPORP
371800060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
371900990820     C     KEYTAD        SETLL     TITAD04L                               99
372000990820     C     KEYTAD        READE     TITAD04L                               99
372100060804X   1C                   ELSE                                                   *OFFERTA
372200060804     C     KEYTAD        SETLL     TIOFD01L                               99
372300060804     C     KEYTAD        READE     TIOFD01L                               99
372400060804E   1C                   ENDIF
372500011128     C                   EVAL      DEPORP = TADORP                              *SOLO un cts
372600011010DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
372700011128     C     TADORP        ANDEQ     DEPORP                                       *ROTTURA ORDINAMENTO
372800011010IF  2C     TADATB        IFEQ      *BLANKS
372900011010     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
373000011010     C     TADSGL        LOOKUP    SSGL                                   98
373100011010IF  3C     *IN98         IFEQ      *OFF
373200030122     C                   IF        I < 10
373300011010     C                   ADD       1             I
373400011010     C                   Z-ADD     TADSGL        SSGL(I)
373500030122     C                   ELSE
373600030122     C                   LEAVE
373700030122     C                   ENDIF
373800011010E   3C                   ENDIF
373900011010E   2C                   ENDIF
374000060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
374100990820     C     KEYTAD        READE     TITAD04L                               99
374200060804X   2C                   ELSE                                                   *OFFERTA
374300060804     C     KEYTAD        READE     TIOFD01L                               99
374400060804E   2C                   ENDIF
374500970313E   1C                   ENDDO                                                  *FINE DETTAGLIO
374600970313     C*
374700970313     C                   ENDSR
374800970314     C*------------------------------------------------------------------------*
374900970314     C* CARCTS   CARICA I CODICI TASSAZIONE
375000970314     C*------------------------------------------------------------------------*
375100970314     C     CARCTS        BEGSR
375200970314     C*
375300970314     C* AZZERA LE VARIABILI DI LAVORO
375400970314     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
375500970314     C                   Z-ADD     *ZEROS        I                 4 0
375600970317     C                   CLEAR                   WCTS
375700970317     C                   CLEAR                   WRAP
375800970317     C                   CLEAR                   WNAR
375900970317     C                   CLEAR                   WTCT
376000970314     C*
376100970314     C* MEMORIZZA I CODICI DELLA LINEA DI PARTENZA RICHIESTA
376200060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
376300990820     C     KEYTAD        SETLL     TITAD04L                               99
376400990820     C     KEYTAD        READE     TITAD04L                               99
376500060804X   1C                   ELSE                                                   *OFFERTA
376600060804     C     KEYTAD        SETLL     TIOFD01L                               99
376700060804     C     KEYTAD        READE     TIOFD01L                               99
376800060804E   1C                   ENDIF
376900970314DO  1C     *IN99         DOWEQ     *OFF
377000970314IF  2C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
377100970314     C                   SETOFF                                           98
377200970314     C                   Z-ADD     1             J                 4 0
377300970314     C     TADCTS        LOOKUP    CTS(J)                                 98
377400970314IF  3C     *IN98         IFEQ      *ON                                          *TROVATO
377500970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
377600970417     C     CIE(J)        ANDEQ     'I'                                          *CODICI ITALIA
377700970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
377800971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
377900970417     C     CIE(J)        ANDEQ     'E'                                          *CODICI ESTERO
378000970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
378100971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ITALIA
378200970314     C                   SETOFF                                           98
378300970314     C                   Z-ADD     1             K                 4 0
378400970314     C     TADCTS        LOOKUP    WCTS(K)                                98
378500970417IF  5C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
378600970417     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
378700970314     C                   ADD       1             I
378800970314     C                   MOVEL     TADCTS        WCTS(I)                        *CODICE
378900970314     C                   MOVEL     RAP(J)        WRAP(I)                        *REGIONE
379000970314     C                   MOVEL     NAR(J)        WNAR(I)                        *NAZIONE
379100970314     C                   MOVEL     TCT(J)        WTCT(I)                        *TIPO
379200970417E   5C                   ENDIF
379300970417E   4C                   ENDIF
379400970314E   3C                   ENDIF
379500970314E   2C                   ENDIF
379600060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
379700990820     C     KEYTAD        READE     TITAD04L                               99
379800060804X   2C                   ELSE                                                   *OFFERTA
379900060804     C     KEYTAD        READE     TIOFD01L                               99
380000060804E   2C                   ENDIF
380100970314E   1C                   ENDDO
380200970314     C*
380300970314     C                   ENDSR
380400970421     C*------------------------------------------------------------------------*
380500970421     C* FIXCTS - IMPOSTA CODICI TASSAZIONE FISSI
380600970421     C*------------------------------------------------------------------------*
380700970421     C     FIXCTS        BEGSR
380800970421     C*
380900970421     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
381000970421     C*
381100970421     C                   Z-ADD     *ZEROS        J                 4 0
381200050103DO  1C     1             DO        600           I
381300970421IF  2C     CTS(I)        IFNE      *BLANKS
381400970421IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
381500970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
381600970421     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
381700970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
381800971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
381900970421     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
382000970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
382100971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
382200970421     C                   ADD       1             J
382300970421     C                   MOVEL     CTS(I)        WCTS(J)                        *CODICE
382400970421     C                   MOVEL     RAP(I)        WRAP(J)                        *REGIONE
382500970421     C                   MOVEL     NAR(I)        WNAR(J)                        *NAZIONE
382600970421     C                   MOVEL     TCT(I)        WTCT(J)                        *TIPO
382700970421E   4C                   ENDIF
382800970421E   3C                   ENDIF
382900970421E   2C                   ENDIF
383000970421E   1C                   ENDDO
383100970421     C*
383200970421     C                   ENDSR
383300970314     C*------------------------------------------------------------------------*
383400970314     C* RAPSVI - TRASFORMA I CODICI DELLA TARIFFA IN CODICI ELEMENTARI
383500970314     C*------------------------------------------------------------------------*
383600970314     C     RAPSVI        BEGSR
383700970314     C*
383800970314     C* 1°GIRO-> CARICA TUTTI I CODICI ELEMENTARI COME -NON CALCOLARE-
383900970314     C                   Z-ADD     *ZEROS        J                 4 0
384000050103DO  1C     1             DO        600           I
384100970416IF  2C     CTS(I)        IFNE      *BLANKS
384200970416IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
384300970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
384400970417     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
384500970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
384600971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
384700970417     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
384800970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
384900971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
385000970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
385100970314     C                   ADD       1             J
385200970423     C                   MOVEL     CTS(I)        CCTI(J)                        *CODICE
385300970423     C                   MOVEL     *BLANKS       CRCI(J)                        *RAGGRUPPAMENTO
385400970423X   5C                   ELSE                                                   *ESTERO
385500970423     C                   ADD       1             J
385600970423     C                   MOVEL     CTS(I)        CCTE(J)                        *CODICE
385700970423     C                   MOVEL     *BLANKS       CRCE(J)                        *RAGGRUPPAMENTO
385800970423E   5C                   ENDIF
385900970423E   4C                   ENDIF
386000970417E   3C                   ENDIF
386100970314E   2C                   ENDIF
386200970314E   1C                   ENDDO                                                  *FINE CODICI BASE
386300970314     C*
386400970314     C* 2°GIRO-> TRASFORMA I -NON CALCOLARE- IN -CALCOLARE IN NAZIONE-
386500050103DO  1C     1             DO        600           I
386600970314IF  2C     WCTS(I)       IFNE      *BLANKS
386700970314IF  3C     WTCT(I)       IFEQ      'N'                                          *SOLO NAZIONE
386800050103DO  4C     1             DO        600           J
386900970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
387000970423IF  6C     CCTI(J)       IFNE      *BLANKS
387100970314     C                   SETOFF                                           98
387200970314     C                   Z-ADD     1             K                 4 0
387300970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
387400970423IF  7C     WNAR(I)       IFEQ      NAR(K)
387500970423     C                   MOVEL     'N'           CRCI(J)
387600970423E   7C                   ENDIF
387700970423E   6C                   ENDIF
387800970423X   5C                   ELSE                                                   *ESTERO
387900970423IF  6C     CCTE(J)       IFNE      *BLANKS
388000970423     C                   SETOFF                                           98
388100970423     C                   Z-ADD     1             K                 4 0
388200970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
388300970423IF  7C     WNAR(I)       IFEQ      NAR(K)
388400970423     C                   MOVEL     'N'           CRCE(J)
388500970423E   7C                   ENDIF
388600970423E   6C                   ENDIF
388700970423E   5C                   ENDIF
388800970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
388900970314E   3C                   ENDIF
389000970314E   2C                   ENDIF
389100970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
389200970314     C*
389300970314     C* 3°GIRO-> TRASFORMA I -CALCOLARE IN NAZIONE- IN -CALCOLARE IN REGIONE-
389400050103DO  1C     1             DO        600           I
389500970314IF  2C     WCTS(I)       IFNE      *BLANKS
389600970314IF  3C     WTCT(I)       IFEQ      'R'                                          *SOLO REGIONE
389700050103DO  4C     1             DO        600           J
389800970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
389900970423IF  6C     CCTI(J)       IFNE      *BLANKS
390000970314     C                   SETOFF                                           98
390100970314     C                   Z-ADD     1             K                 4 0
390200970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
390300970423IF  7C     WRAP(I)       IFEQ      RAP(K)
390400970423     C                   MOVEL     'R'           CRCI(J)
390500970423E   7C                   ENDIF
390600970423E   6C                   ENDIF
390700970423X   5C                   ELSE                                                   *ESTERO
390800970423IF  6C     CCTE(J)       IFNE      *BLANKS
390900970423     C                   SETOFF                                           98
391000970423     C                   Z-ADD     1             K                 4 0
391100970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
391200970423IF  7C     WRAP(I)       IFEQ      RAP(K)
391300970423     C                   MOVEL     'R'           CRCE(J)
391400970423E   7C                   ENDIF
391500970423E   6C                   ENDIF
391600970314E   5C                   ENDIF
391700970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
391800970314E   3C                   ENDIF
391900970314E   2C                   ENDIF
392000970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
392100970314     C*
392200970314     C* 4°GIRO-> TRASFORMA I -CALCOLARE IN REGIONE- IN -CALCOLARE-
392300050103DO  1C     1             DO        600           I
392400970314IF  2C     WCTS(I)       IFNE      *BLANKS
392500970314IF  3C     WTCT(I)       IFEQ      'E'                                          *SOLO ELEMENTARI
392600050103DO  4C     1             DO        600           J
392700970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
392800970423IF  6C     CCTI(J)       IFNE      *BLANKS
392900970423IF  7C     WCTS(I)       IFEQ      CCTI(J)
393000970423     C                   MOVEL     'T'           CRCI(J)
393100970423E   7C                   ENDIF
393200970423E   6C                   ENDIF
393300970423X   5C                   ELSE                                                   *ESTERO
393400970423IF  6C     CCTE(J)       IFNE      *BLANKS
393500970423IF  7C     WCTS(I)       IFEQ      CCTE(J)
393600970423     C                   MOVEL     'T'           CRCE(J)
393700970423E   7C                   ENDIF
393800970423E   6C                   ENDIF
393900970423E   5C                   ENDIF
394000970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
394100970314E   3C                   ENDIF
394200970314E   2C                   ENDIF
394300970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
394400970314     C*
394500970314     C                   ENDSR
394600981218     C*------------------------------------------------------------------------*
394700981218     C* CANTAR - ANNULLAMENTO TARIFFA
394800981218     C*------------------------------------------------------------------------*
394900981218     C     CANTAR        BEGSR
395000981218     C*
395100981218     C* CANCELLA DATI TIPO - GENERALI
395200981218     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE
395300060804     C                   MOVEL     parcto        KTCCTO                         *DATI TARIFFA
395400981218     C                   Z-ADD     PARKOV        KTCKSC                         *CLIENTE
395500011001     C     KE3ETC        SETLL     TEETC01L
395600011001     C     KE3ETC        READE     TEETC01L                               99
395700981218DO  1C     *IN99         DOWEQ     *OFF
395800981218IF  2C     PARCTR        IFEQ      ETCCTR                                       *TARIFFA RICHIESTA
395900981218IF  3C     PARPRG        IFEQ      ETCPRG                                       *PROGRESSIVO RICHIES
396000981218IF  4C     PARLNP        IFEQ      ETCLNP                                       *LINEA RICHIESTA
396100981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
396200981218     C                   MOVEL     'A'           ETCATB                         *ANNULLAMENTO
396300981218     C                   MOVEL     *BLANKS       ETCFTR                         *FLAG TRASMISSIONE
396400011128     C*---               Z-ADD     *ZEROS        ETCDTR                         *DATA TRASMISSIONE
396500011001     C                   UPDATE    TEETC000                                     *AGGIORNA
396600981218E   4C                   ENDIF
396700981218E   3C                   ENDIF
396800981218E   2C                   ENDIF
396900011001     C     KE3ETC        READE     TEETC01L                               99
397000981218E   1C                   ENDDO                                                  *FINE DATI GENERALI
397100981218     C*
397200981218     C* CANCELLA DATI TIPO - SCAGLIONI
397300060829     c* ricavi
397400981218     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE
397500060804     C                   MOVEL     parcto        KTSCTO                         *DATI TARIFFA
397600981218     C                   Z-ADD     PARKOV        KTSKSC                         *CLIENTE
397700011001     C     KE3ETS        SETLL     TEETS01L
397800011001     C     KE3ETS        READE     TEETS01L                               99
397900981218DO  1C     *IN99         DOWEQ     *OFF
398000981218IF  2C     PARCTR        IFEQ      ETSCTR                                       *TARIFFA RICHIESTA
398100981218IF  3C     PARPRG        IFEQ      ETSPRG                                       *PROGRESSIVO RICHIES
398200981218IF  4C     PARLNP        IFEQ      ETSLNP                                       *LINEA RICHIESTA
398300981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
398400981218     C                   MOVEL     'A'           ETSATB                         *ANNULLAMENTO
398500981218     C                   MOVEL     *BLANKS       ETSFTR                         *FLAG TRASMISSIONE
398600011001     C                   UPDATE    TEETS000                                     *AGGIORNA
398700981218E   4C                   ENDIF
398800981218E   3C                   ENDIF
398900981218E   2C                   ENDIF
399000011001     C     KE3ETS        READE     TEETS01L                               99
399100981218E   1C                   ENDDO                                                  *FINE SCAGLIONI
399200060829     c* competetnze
399300060829     C     KE3ETS        SETLL     TEETS11L
399400060829     C     KE3ETS        READE     TEETS11L                               99
399500060829DO  1C     *IN99         DOWEQ     *OFF
399600060829IF  2C     PARCTR        IFEQ      £ETSCTR                                      *TARIFFA RICHIESTA
399700060829IF  3C     PARPRG        IFEQ      £ETSPRG                                      *PROGRESSIVO RICHIES
399800060829IF  4C     PARLNP        IFEQ      £ETSLNP                                      *LINEA RICHIESTA
399900060829     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
400000060829     C                   MOVEL     'A'           £ETSATB                        *ANNULLAMENTO
400100060829     C                   MOVEL     *BLANKS       £ETSFTR                        *FLAG TRASMISSIONE
400200060829     C                   UPDATE    TEETS100                                     *AGGIORNA
400300060829E   4C                   ENDIF
400400060829E   3C                   ENDIF
400500060829E   2C                   ENDIF
400600060829     C     KE3ETS        READE     TEETS11L                               99
400700060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
400800981218     C*
400900981218     C* CANCELLA DATI TIPO - CODICI TASSAZIONE
401000981218     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE
401100060804     C                   MOVEL     parcto        KTTCTO                         *DATI TARIFFA
401200981218     C                   Z-ADD     PARKOV        KTTKSC                         *CLIENTE
401300141010     C     KE3ETT        SETLL     TEETT11L
401400141010     C     KE3ETT        READE     TEETT11L                               99
401500981218DO  1C     *IN99         DOWEQ     *OFF
401600981218IF  2C     PARCTR        IFEQ      ETTCTR                                       *TARIFFA RICHIESTA
401700981218IF  3C     PARPRG        IFEQ      ETTPRG                                       *PROGRESSIVO RICHIES
401800981218IF  4C     PARLNP        IFEQ      ETTLNP                                       *LINEA RICHIESTA
401900981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
402000981218IF  5C     PARFIE        IFEQ      ETTFIE                                       *ITA O EST RICHIESTO
402100981218     C     PARFIE        OREQ      *BLANKS                                      *TUTTO ITA E EST
402200981218     C                   MOVEL     'A'           ETTATB                         *ANNULLAMENTO
402300981218     C                   MOVEL     *BLANKS       ETTFTR                         *FLAG TRASMISSIONE
402400141010     C                   UPDATE    TEETT100                                     *AGGIORNA
402500981218E   5C                   ENDIF
402600981218E   4C                   ENDIF
402700981218E   3C                   ENDIF
402800981218E   2C                   ENDIF
402900141010     C     KE3ETT        READE     TEETT11L                               99
403000981218E   1C                   ENDDO                                                  *FINE CODICI TASSAZI
403100981218     C*
403200981218     C                   ENDSR
403300020923     C*--------------------------------------------------------------------------------------------*
403400020923     C* REPERISCE I DATI UTENTE
403500020923     C*--------------------------------------------------------------------------------------------*
403600020923     C     REPDATIUTE    BEGSR
403700020923     C*
403800020923     C* INIZIALIZZA VARIABILI DI WRK
403900020923     C                   CLEAR                   TIBS34DS
404000020923     C                   CLEAR                   AZUTEDS
404100020923     C                   CLEAR                   DDATIUTE
404200020923     C*
404300020923     C     *DTAARA       DEFINE    §azute        azuteds
404400020923     C     *DTAARA       DEFINE    §datiute      ddatiute
404500020923     C                   IN(E)     *DTAARA
404600020923if  1C                   IF        %Error
404700020923     c                   EVAL      I34Tla = 'L'
404800020923     C                   CALL      'TIBS34R'
404900020923     C                   PARM                    Tibs34Ds
405000020923     C                   IN        *DTAARA
405100020923e   1C                   ENDIF
405200020923     C*
405300020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
405400020923     C                   MOVEL(P)  RSUT          DSFIRS           20
405500020923     C*
405600020923     C                   ENDSR
405700021031     C*------------------------------------------------------------------------*
405800021031     C* REPCLI - CARICAMENTO LISTA CLIENTI PADRI/FIGLI
405900021031     C*------------------------------------------------------------------------*
406000021031     C     REPCLI        BEGSR
406100021031     C*
406200021031     C* SE RICHIESTA ELABORAZIONE X CLIENTE UNIFICANTE REPERISCO LA LISTA DEI FIGLI
406300021031     C                   IF        PARUNI = 'S'
406400021031     C                   CLEAR                   TIBS10DS
406500021031     C                   EVAL      D10DRF = DATCOR                              *data riferimento
406600021031     C                   EVAL      D10TLE = 'ST'                                *tipo legame
406700021031     C                   EVAL      D10PAF = 'F'                                 *cerca il padre
406800021031     C                   Z-ADD     PARKSC        D10COD                         *cod cliente da tst
406900021031     C                   CALL      'TIBS10R'
407000021031     C                   PARM                    TIBS10DS
407100021031     C*
407200021031     C                   IF        D10ERR <> *blanks OR                         *No unif. o padre
407300021031     C                             D10COP =  *zeros
407400021031     C                   CLEAR                   SKCLI
407500021031     C                   Z-ADD     PARKSC        SKCLI(1)
407600021031     C                   ENDIF
407700021031     C*
407800021031     C                   ELSE
407900021031     C                   CLEAR                   SKCLI
408000021031     C                   Z-ADD     PARKSC        SKCLI(1)
408100021031     C                   ENDIF
408200021031     C*
408300021031     C                   ENDSR
408400141017     C*------------------------------------------------------------------------*
408500141017     C* *INZSR - OPERAZIONI INIZIALI
408600141017     C*------------------------------------------------------------------------*
408700141017     C     *INZSR        BEGSR
408800970110     C*--------------------
408900970110     C* RICEVIMENTO PARAMETRI
409000970110     C*--------------------
409100970110     C     *ENTRY        PLIST
409200970110     C                   PARM                    KPJBA
409300970110     C                   MOVEL     KPJBU         PARAM
409400141010     c*
409500141010     c                   if        pardsf = ' '
409600141010     c                   eval      pardsf = 'S'
409700141010     c                   end
409800141009     c* reperisco la versione del cappario
409900141009     C                   CLEAR                   DSSI95
410000141009     C                   MOVEL     '3'           I95TCN
410100141009     C                   MOVEL(p)  '40100'       I95CAP
410200141009     C                   CALL      'TISI95R'
410300141009     C                   PARM                    DSSI95
410400021121     C*
410500021121     C* CARICA NUOVE TABELLE OCCORRENTI
410600021121     C                   EXSR      CARGEDCE
410700021121     C*
410800021121     C* REPERISCO SUBITO LA DIVISA DELLA TARIFFA RICEVUTA IN INPUT
410900021121     C                   IF        PARDIV = *BLANKS
411000021121     C                   Z-ADD     PARKOV        KTMKSC
411100021121     C                   Z-ADD     PARCTR        KTMCTR
411200021121     C                   Z-ADD     PARPRG        KTMPRG
411300021121     C     KEYTAMUNIQ    CHAIN     TNTAM01L
411400021121     C                   IF        %FOUND
411500021121     C                   MOVEL(P)  TAMFLO        DSTA01
411600021121     C                   MOVEL(P)  §TADIV        PARDIV
411700021121     C                   ELSE
411800021121     C                   MOVEL(P)  WDIVCE        PARDIV
411900021121     C                   ENDIF
412000021121     C                   ENDIF
412100970110     C*--------------------
412200970110     C* VARIABILI RIFERITE AL DATABASE
412300970110     C*--------------------
412400970110     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
412500970110     C     *LIKE         DEFINE    TBLCOD        KTBCOD
412600970110     C     *LIKE         DEFINE    TBLKEY        KTBKEY
412700970110     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
412800970110     C     *LIKE         DEFINE    ACOKCC        KACKCC
412900970110     C     *LIKE         DEFINE    ACOKSC        KACKSC
413000970110     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
413100970110     C     *LIKE         DEFINE    CLPKCC        KCLKCC
413200970110     C     *LIKE         DEFINE    CLPKSC        KCLKSC
413300970110     C     *LIKE         DEFINE    INDKUT        KINKUT                         *CNIND00F
413400970110     C     *LIKE         DEFINE    INDKCC        KINKCC
413500970415     C     *LIKE         DEFINE    INDKSC        KINKSC
413600970110     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
413700970110     C     *LIKE         DEFINE    TAMCTR        KTMCTR
413800021121     C     *LIKE         DEFINE    TAMPRG        KTMPRG
413900990820     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
414000970110     C     *LIKE         DEFINE    TADCTR        KTDCTR
414100970110     C     *LIKE         DEFINE    TADPRG        KTDPRG
414200970317     C     *LIKE         DEFINE    TADLNP        KTDLNP
414300990812     C     *LIKE         DEFINE    TASKSC        KTAKSC                         *TITAS00F
414400970110     C     *LIKE         DEFINE    TASAAS        KTAAAS
414500970110     C     *LIKE         DEFINE    TASMGS        KTAMGS
414600970110     C     *LIKE         DEFINE    CSBAAS        KSBAAS                         *TNCSB00F
414700970110     C     *LIKE         DEFINE    CSBLNP        KSBLNP
414800970110     C     *LIKE         DEFINE    CSBNRS        KSBNRS
414900970110     C     *LIKE         DEFINE    CSBNSP        KSBNSP
415000011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
415100980713     C     *LIKE         DEFINE    ETCCTO        KTCCTO
415200970110     C     *LIKE         DEFINE    ETCKSC        KTCKSC
415300970110     C     *LIKE         DEFINE    ETCCTR        KTCCTR
415400970110     C     *LIKE         DEFINE    ETCPRG        KTCPRG
415500970110     C     *LIKE         DEFINE    ETCLNP        KTCLNP
415600011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
415700980713     C     *LIKE         DEFINE    ETSCTO        KTSCTO
415800970110     C     *LIKE         DEFINE    ETSKSC        KTSKSC
415900970110     C     *LIKE         DEFINE    ETSCTR        KTSCTR
416000970110     C     *LIKE         DEFINE    ETSPRG        KTSPRG
416100970110     C     *LIKE         DEFINE    ETSLNP        KTSLNP
416200970110     C     *LIKE         DEFINE    ETSSGL        KTSSGL
416300011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
416400980713     C     *LIKE         DEFINE    ETTCTO        KTTCTO
416500970110     C     *LIKE         DEFINE    ETTKSC        KTTKSC
416600970110     C     *LIKE         DEFINE    ETTCTR        KTTCTR
416700970110     C     *LIKE         DEFINE    ETTPRG        KTTPRG
416800970110     C     *LIKE         DEFINE    ETTLNP        KTTLNP
416900970110     C     *LIKE         DEFINE    ETTFIE        KTTFIE
417000970110     C     *LIKE         DEFINE    ETTCTS        KTTCTS
417100141010     C     *LIKE         DEFINE    ETTntw        KTTntw
417200141010     C     *LIKE         DEFINE    ETTnaz        KTTnaz
417300141010     C     *LIKE         DEFINE    ETTcad        KTTcad
417400970110     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DEPOSITI
417500970110     C     *LIKE         DEFINE    TAMCTR        CARCTR
417600970110     C     *LIKE         DEFINE    TAMPRG        CARPRG
417700970430     C     *LIKE         DEFINE    TASLNP        DEPFIL
417800980713     C     *LIKE         DEFINE    PARCTO        DEPCTO
417900020730     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
418000011128     C     *LIKE         DEFINE    TADORP        DEPORP
418100141010     C     *LIKE         DEFINE    ETtntw        Xntw                           *DI PASSAGGIO
418200140919     C     *LIKE         DEFINE    ETCLNP        XORG                           *DI PASSAGGIO
418300980821     C     *LIKE         DEFINE    ETTFIE        XFIE
418400011011     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
418500011011     C     *LIKE         DEFINE    TBEKE1        KTEKE1
418600050309     C     *LIKE         DEFINE    GCPAAS        KGPAAS                         *TIGCP00F
418700021122     C     *LIKE         DEFINE    GCPLNP        KGPLNP
418800021122     C     *LIKE         DEFINE    GCPNRS        KGPNRS
418900021122     C     *LIKE         DEFINE    GCPNSP        KGPNSP
419000980821     C*--------------------
419100970110     C* CHAIVI DI LETTURA
419200970110     C*--------------------
419300060315     C* CHIAVE LETTURA TITA730C - COMPLETA
419400060315     C     KEYTA7        KLIST
419500060315     C                   KFLD                    TA7AAS
419600060315     C                   KFLD                    TA7LNP
419700060315     C                   KFLD                    TA7NRS
419800060315     C                   KFLD                    TA7NSP
419900060315     C                   KFLD                    TA7TBL
420000970110     C* LETTURA TABEL00F - COMPLETA
420100970110     C     KEYTAB        KLIST
420200970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
420300970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
420400970110     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
420500970110     C* LETTURA TABEL00F - PARZIALE
420600970110     C     KE2TAB        KLIST
420700970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
420800970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
420900970110     C* LETTURA CNACO00F - COMPETA
421000970110     C     KEYACO        KLIST
421100970110     C                   KFLD                    KACKUT                         *CODICE UTENTE
421200970110     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
421300970110     C                   KFLD                    KACKSC                         *CODICE CLIENTE
421400970110     C* LETTURA CNCLP00F - COMPLETA
421500970110     C     KEYCLP        KLIST
421600970110     C                   KFLD                    KCLKUT                         *CODICE UTENTE
421700970110     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
421800970110     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
421900970415     C* LETTURA CNIND00F - COMPLETA
422000970415     C     KEYIND        KLIST
422100970415     C                   KFLD                    KINKUT                         *CODICE UTENTE
422200970415     C                   KFLD                    KINKCC                         *CODICE CAPOCONTO
422300970415     C                   KFLD                    KINKSC                         *CODICE CLIENTE
422400970110     C* LETTURA TNTAM01L - PARZIALE
422500970110     C     KEYTAM        KLIST
422600970110     C                   KFLD                    KTMKSC                         *CLIENTE
422700970110     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
422800021121     C* LETTURA TNTAM01L - PARZIALE
422900021121     C     KEYTAMUNIQ    KLIST
423000021121     C                   KFLD                    KTMKSC                         *CLIENTE
423100021121     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
423200021121     C                   KFLD                    KTMPRG                         *PROGRESSIVO
423300990820     C* LETTURA TITAD01L - PARZIALE
423400970110     C     KEYTAD        KLIST
423500970110     C                   KFLD                    KTDKSC                         *CLIENTE
423600970110     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
423700970110     C                   KFLD                    KTDPRG                         *PROGRESSIVO
423800970317     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
423900990812     C* LETTURA TITAS31C - PARZIALE
424000970110     C     K31TAS        KLIST
424100970110     C                   KFLD                    KTAKSC                         *CLIENTE
424200990812     C* LETTURA TITAS38C - COMPLETA
424300970110     C     K38TAS        KLIST
424400970110     C                   KFLD                    KTAAAS                         *ANNO
424500970110     C                   KFLD                    KTAMGS                         *MESE/GIORNO
424600970110     C                   KFLD                    KTAKSC                         *CLIENTE
424700970110     C* LETTURA TNCSB03L - COMPLETA
424800970110     C     KEYCSB        KLIST
424900970110     C                   KFLD                    KSBAAS                         *ANNO
425000970110     C                   KFLD                    KSBLNP                         *LINEA PARTENZA
425100970110     C                   KFLD                    KSBNRS                         *SERIE
425200970110     C                   KFLD                    KSBNSP                         *SPEDIZIONE
425300011001     C* LETTURA TEETC01L - COMPLETA
425400981218     C     KEYETC        KLIST
425500980713     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
425600980713     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
425700970110     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
425800970110     C                   KFLD                    KTCCTR                         *TARIFFA
425900970110     C                   KFLD                    KTCPRG                         *PROGRESSIVO
426000970110     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
426100011001     C* LETTURA TEETC01L - PARZIALE
426200981218     C     KE3ETC        KLIST
426300981218     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
426400981218     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
426500981218     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
426600011001     C* LETTURA TEETS01L - COMPLETA
426700970110     C     KEYETS        KLIST
426800980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
426900970110     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
427000970110     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
427100970110     C                   KFLD                    KTSCTR                         *TARIFFA
427200970110     C                   KFLD                    KTSPRG                         *PROGRESSIVO
427300970110     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
427400970417     C                   KFLD                    KTSSGL                         *SCAGLIONE
427500011001     C* LETTURA TEETS01L - PARZIALE
427600970314     C     KE2ETS        KLIST
427700980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
427800970314     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
427900970314     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
428000970314     C                   KFLD                    KTSCTR                         *TARIFFA
428100970314     C                   KFLD                    KTSPRG                         *PROGRESSIVO
428200970314     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
428300011001     C* LETTURA TEETS01L - PARZIALE
428400981218     C     KE3ETS        KLIST
428500981218     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
428600981218     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
428700981218     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
428800140919     C* LETTURA TEETT11L - COMPLETA
428900141015     C     Kett          KLIST
429000140919     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
429100140919     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
429200140919     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
429300140919     C                   KFLD                    KTTCTR                         *TARIFFA
429400140919     C                   KFLD                    KTTPRG                         *PROGRESSIVO
429500140919     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
429600140919     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
429700140919     C                   KFLD                    KTTntw                         *ITALIA/ESTERO
429800140922     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
429900140922     C                   KFLD                    KTTnaz                         *CODICE TASSAZIONE
430000140922     C                   KFLD                    KTTCad                         *CODICE TASSAZIONE
430100141015     C     Kettcts       KLIST
430200140922     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
430300140922     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
430400140922     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
430500140922     C                   KFLD                    KTTCTR                         *TARIFFA
430600140922     C                   KFLD                    KTTPRG                         *PROGRESSIVO
430700140922     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
430800140922     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
430900140922     C                   KFLD                    KTTntw                         *ITALIA/ESTERO
431000140922     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
431100140922     C* LETTURA TEETT11L - PARZIALE
431200141015     C     Kettntw       KLIST
431300140922     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
431400140922     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
431500140922     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
431600140922     C                   KFLD                    KTTCTR                         *TARIFFA
431700140922     C                   KFLD                    KTTPRG                         *PROGRESSIVO
431800140922     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
431900140922     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
432000140922     C                   KFLD                    KTTntw                         *ITALIA/ESTERO
432100140922     C* LETTURA TEETT01L - PARZIALE
432200141015     C     KEttfie       KLIST
432300980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
432400970314     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
432500970314     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
432600970314     C                   KFLD                    KTTCTR                         *TARIFFA
432700970314     C                   KFLD                    KTTPRG                         *PROGRESSIVO
432800970314     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
432900970314     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
433000040809     C* LETTURA fiar531c
433100040809     C     Kar5          KLIST
433200040809     C                   KFLD                    ar5aas                         *DATI SEDE O FILIALE
433300040809     C                   KFLD                    ar5lnp                         *TARIFFA O OFFERTA
433400040809     C                   KFLD                    ar5nrs                         *CLIENTE O VISITA
433500040809     C                   KFLD                    ar5nsp                         *CLIENTE O VISITA
433600040809     C                   KFLD                    ar5trd                         *CLIENTE O VISITA
433700040809     C* LETTURA TEETT01L - PARZIALE
433800040809     C     KE3ETT        KLIST
433900040809     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
434000040809     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
434100040809     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
434200011011     C* LETTURA TNTBE01L - PARZIALE
434300011011     C     KEYTBE        KLIST
434400011011     C                   KFLD                    KTECOD                         *UTENTE
434500011011     C                   KFLD                    KTEKE1                         *CODICE
434600050309     C* LETTURA TIGCP51L - PARZIALE
434700021122     C     K01GCP        KLIST
434800021122     C                   KFLD                    KGPAAS                         *ANNO
434900021122     C                   KFLD                    KGPLNP                         *LINEA DI PARTENZA
435000021122     C                   KFLD                    KGPNRS                         *SERIE
435100021122     C                   KFLD                    KGPNSP                         *SPEDIZIONE
435200970110     C*--------------------
435300970110     C* VARIABILI DI CONTROLLO
435400970110     C*--------------------
435500970110     C                   MOVEL     '0'           $FINE             1            *FINE PGM
435600970110     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
435700970416     C                   MOVEL     'S'           $OK               1            *RECORD OK
435800970312     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO TROVATO
435900970313     C                   MOVEL     '0'           $ERR              1            *ERRORE
436000970313     C                   MOVEL     '1'           *INOF                          *OVERFLOW QSYSPRT
436100970313     C                   MOVEL     '1'           *IN01                          *OVERFLOW TNTE41P
436200970110     C*
436300970110     C* REPERISCE I DATI DELL'UTENTE
436400020923     C                   EXSR      REPDATIUTE
436500970110     C*
436600970110     C* REPERISCE LA DATA CORRENTE
436700970110     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
436800970110     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
436900970110     C                   Z-ADD     WN8           G08DAT
437000970110     C                   Z-ADD     *ZEROS        G08INV
437100970110     C                   MOVEL     '0'           G08ERR
437200970110     C                   CALL      'XSRDA8'
437300970110     C                   PARM                    WLBDA8
437400970110     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
437500970110     C*
437600970110     C* CARICA TABELLE OCCORRENTI
437700970110     C                   EXSR      CARTAB
437800970313     C*
437900970313     C* CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
438000970313     C                   EXSR      CARTAR
438100981218     C*
438200981218     C* ANNULLAMENTO TARIFFA
438300981218     C                   EXSR      CANTAR
438400021031     C*
438500021031     C* REPERIMENTO LISTA CLIENTI PADRI/FIGLI
438600021031     C                   EXSR      REPCLI
438700970110     C*
438800970110     C                   ENDSR
438900970110     O*------------------------------------------------------------------------*
439000970110     OQSYSPRT   E            TES              02
439100970110     O                       STA(1)              66
439200970110     O                       STA(2)             132
439300970110     O                       DSFIRS              20
439400970110     O                       UDATE              117 '  /  /  '
439500970110     O                                          127 'Pag.'
439600970110     O                       PAGE          Z    132
439700970110     O*------------------------
439800970110     O          E            ETAS        1
439900970110     O                       STA(3)              66
440000970110     O                       STA(4)             132
440100970423     O                       TASAAS        Z     16
440200970423     O                       TASLNP        Z     20
440300970423     O                       TASNRS        Z     23
440400970423     O                       TASNSP        Z     31
440500970423     O                       TASTBL              34
440600970423     O                       TASCTS              43
440700970110     O                       DEPERR             124
440800970110     O*------------------------
440900970110     O          E            EETX        1
441000970110     O                       STA(3)              66
441100970110     O                       STA(5)             132
441200970110     O                       PARKOV        Z     15
441300970110     O                       PARCTR              24
441400970110     O                       PARPRG              33
441500970110     O                       PARLNP        Z     53
441600970110     O                       PARFIE              62
441700970110     O                       DEPERR             124
441800970110     O*------------------------------------------------------------------------*
441900970110** -DDEC-
442000970110NON CALCOLARE                 1
442100970110CALCOLARE                     2
442200970110CALCOLARE IN REGIONE          3
442300970110CALCOLARE IN NAZIONE          4
442400970110 **  CREAZIONE DATI  TIPO  ** 5
442500980821TUTTA L'ITALIA                6
442600980821TUTTO L'ESTERO                7
4427009808218
4428009808219
44290098082110
443000990812** MTAS - MESSAGGI PER TITAS
443100970110FILIALE BOLLA NON TROVATA                                         1
443200970423CODICE TASSAZIONE NON TROVATO O NON VALIDO                        2
4433009704163
4434009701104
4435009701105
443600011001** METX - MESSAGGI PER TEETX
443700970110DATI TIPO ALLOCATI                                                1
443800971008NESSUNA SPEDIZIONE: VALORE DEDOTTO                                2
443900980901CREAZIONE AUTOMATICA                                              3
444000141017CODICE TASSAZIONE SENZA CAP ASSOCIATI                             4
4441009701105
444200970110** STA
444300141017XXXXXXXXXXXXXXXXXXXX        TNTE45R       ** ERRORI IN CREAZIONE D1
444400970110ATI TIPO TARIFFE DA CONTO ECONOMICO **     99/99/99      PAG. 99992
444500970423SPEDIZIONE: 9999 999 99 9999999 XX  CTS: XX                       3
444600970423ERRORE: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        4
444700970110CLIENTE 9999999  TAR 999  PRG 999  LINEA PARTENZA 999   I/E  X    5
