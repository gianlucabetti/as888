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
004800011001     FTEETT01L  UF A E           K DISK
004900140922     FTEETT11L  UF A E           K DISK    prefix(£)
005000970110     F*-----------
005100970110     F* ORGANIGRAMMA
005200970110     F*-----------
005300970417     FAZORG01L  IF   E           K DISK
005400970110     F*-----------
005500970110     F* TABELLE
005600970110     F*-----------
005700970417     FTABEL00F  IF   E           K DISK
005800011011     FTNTBE01L  IF   E           K DISK
005900970110     F*-----------
006000970110     F* TARIFFE
006100970110     F*-----------
006200970417     FTNTAM01L  IF   E           K DISK
006300990820     FTITAD04L  IF   E           K DISK
006400040809     Ffiar531c  IF   E           K DISK
006500970110     F*-----------
006600970110     F* PRINTER FILE
006700970110     F*-----------
006800970110     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
006900970110     D*------------------------------------------------------------------------*
007000970110     D* SCHIERE
007100970110     D*------------------------------------------------------------------------*
007200970110     D*------------------
007300970110     D* DECODIFICHE
007400970110     D*------------------
007500980821     D DDEC            S             30    DIM(10) CTDATA PERRCD(1)
007600970110     D*------------------
007700970110     D* ERRORI DI ELABORAZIONE
007800970110     D*------------------
007900990812     D MTAS            S             66    DIM(5) CTDATA PERRCD(1)              *MESSAGGI PER TITAS
008000011001     D METX            S             66    DIM(5) CTDATA PERRCD(1)              *MESSAGGI PER TEETX
008100970110     D*------------------
008200970110     D* RIGHE DI STAMPA
008300970110     D*------------------
008400970110     D STA             S             66    DIM(5) CTDATA PERRCD(1)
008500970110     D*------------------
008600970110     D* ORGANIGRAMMA
008700970110     D*------------------
008800971124     D ORG             S              3  0 DIM(500)                             *CODICE
008900971124     D DORG            S             20    DIM(500)                             *DESCRIZIONE
009000971204     D FL1             S              1    DIM(500)                             *FLAG ITALIA/ESTERO
009100970110     D*------------------
009200970110     D* SELEZIONI
009300970110     D*------------------
009400971124     D SORG            S              3  0 DIM(500)                             *FILIALI
009500971204     D SFL1            S              1    DIM(500)                             *FLAG ITALIA/ESTERO
009600970423     D SFIE            S              1    DIM(2)                               *ITALIA/ESTERO
009700140919     D Sntw            S              3    DIM(500)                             *FLAG ITALIA/ESTERO
009800970110     D*-------------------
009900970110     D* TIPI TARIFFA
010000970110     D*-------------------
010100970110     D TTR             S              1    DIM(10)                              *CODICE
010200970110     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
010300970110     D*-------------------
010400970110     D* RAGGRUPPAMENTO TIPI BOLLA
010500970110     D*-------------------
010600970110     D RBL             S              1    DIM(5)                               *RAGGRUPPAMENTO BOLL
010700970110     D FSP             S              1    DIM(5)                               *FLAG SPEDIZIONE
010800970110     D FRI             S              1    DIM(5)                               *FLAG RICAVO
010900970110     D*------------------
011000970110     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA
011100970110     D*------------------
011200970110     D TBL             S              2    DIM(20)                              *TIP0 BOLLA
011300970110     D SPD             S              1    DIM(20)                              *FLAG SPEDIZIONE
011400970110     D RIC             S              1    DIM(20)                              *FLAG RICAVO
011500970110     D*------------------
011600970110     D* TABELLE CAPICONTO
011700970110     D*------------------
011800970110     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
011900970110     D*------------------
012000970110     D* RATE DI PAGAMENTO
012100970110     D*------------------
012200970110     D RAT             S              3    DIM(10)
012300970110     D*------------------
012400970110     D* CODICI PAGAMENTO
012500970110     D*------------------
012600970110     D CDP             S              4    DIM(200)                             *CODICE PAGAMENTO
012700970110     D CRAT            S             30    DIM(200)                             *RATE DI PAGAMENTO
012800040920     D*------------------
012900040920     D* scaglioni standard
013000040920     D*------------------
013100040920     D sgl             S             13  3 DIM(10)                              *CODICI ELEMENTARI
013200970110     D*------------------
013300970110     D* CODICI TASSAZIONE
013400970110     D*------------------
013500050103     D CTS             S              2    DIM(600)                             *CODICI ELEMENTARI
013600140919     D naz             S              3    DIM(600)                             *CODICI ELEMENTARI
013700050103     D FCR             S              1    DIM(600)                             *FLAG CAPOLUOGO
013800050103     D DCTS            S             20    DIM(600)                             *DESCRIZIONE
013900050103     D CIE             S              1    DIM(600)                             *FLAG ITALIA/ESTERO
014000050103     D RAP             S              2    DIM(600)                             *CODICE REGIONE
014100050103     D NAR             S              2    DIM(600)                             *CODICE NAZIONE -> P
014200050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)   -> P
014300050808     D*------------------
014400050808     D* cod. trattamento merce con packing list
014500050808     D*------------------
014600050808     D Cpkl            S              2    DIM(600)                             *CODICI ELEMENTARI
014700970314     D*------------------
014800970314     D* DATI TARIFFE
014900970314     D*------------------
015000971124     D* MULTIPLE                                         -500 FILIALI-
015100970110     D*-------------------
015200970110     D* SCHIERE DI MEMORIZZAZIONE
015300970110     D*-------------------
015400970110     D* NON MULTIPLE
015500060315     D SVN             S              1    DIM(20)
015600060315     D VAN             S             11  3 DIM(20)
015700971124     D SPE             S              9  0 DIM(500)                             *N°SPEDIZIONI ITA/ES
015800971124     D CLA             S              9  0 DIM(500)                             *CLIENTE TIPO A
015900971124     D CLB             S              9  0 DIM(500)                             *CLIENTE TIPO B
016000971124     D CLC             S              9  0 DIM(500)                             *CLIENTE TIPO C
016100971124     D BON             S              9  0 DIM(500)                             *BOLLETTAZIONE (' ')
016200971124     D BOA             S              9  0 DIM(500)                             *BOLLETTAZIONE (A)
016300971124     D BOB             S              9  0 DIM(500)                             *BOLLETTAZIONE (B)
016400971124     D NCL             S              9  0 DIM(500)                             *COLLI
016500971124     D PKB             S              9  1 DIM(500)                             *PESO
016600971124     D VLB             S              9  3 DIM(500)                             *VOLUME
016700060829     D NCL1            S              9  0 DIM(500)                             *COLLI
016800060829     D PKB1            S              9  1 DIM(500)                             *PESO
016900060829     D VLB1            S              9  3 DIM(500)                             *VOLUME
017000971124     D FRA             S              9  0 DIM(500)                             *FRANCHI
017100971124     D ASS             S              9  0 DIM(500)                             *ASSEGNATI
017200971124     D PCV             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
017300971124     D PCM             S              9  0 DIM(500)                             *CONSEGNE MAGAZZINO
017400971124     D PFD             S              9  0 DIM(500)                             *FERMO DEPOSITO
017500971124     D ESP             S              9  0 DIM(500)                             *ESPRESSI
017600971124     D PPV             S              9  0 DIM(500)                             *CONSEGNE PROVINCIA
017700971124     D PCA             S              9  0 DIM(500)                             *CONSEGNE APPUNTAMEN
017800971124     D PCP             S              9  0 DIM(500)                             *CONSEGNE PIANI
017900971124     D PCS             S              9  0 DIM(500)                             *CONSEGNE SUPERMERCA
018000971124     D PCF             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
018100971124     D PAB             S              9  0 DIM(500)                             *C/ASSEGNI BARTOLINI
018200971124     D PAM             S              9  0 DIM(500)                             *C/ASSEGNI MITTENTE
018300011026     D CAS             S             21  8 DIM(500)                             *C/ASSEGNO
018400971124     D PAR             S              9  0 DIM(500)                             *ASSICURAZIONE R.C.V
018500971124     D PAC             S              9  0 DIM(500)                             *ASSICURAZIONE X CON
018600060315     D xx1             S              9  0 DIM(500)                             *ASSICURAZIONE X CON
018700011026     D ASC             S             21  8 DIM(500)                             *ASSICURAZIONE
018800971124     D PGI             S              9  0 DIM(500)                             *GIACENZE
018900971124     D PCD             S              9  0 DIM(500)                             *CONSEGNE DDT
019000971124     D PCC             S              9  0 DIM(500)                             *CONSEGNE C.STORICI
019100971124     D PCL             S              9  0 DIM(500)                             *CONSEGNE L.DISAGIAT
019200980901     D CLT             S              9  0 DIM(500)                             *CLIENTE TIPO T
019300000414     D GGS             S              9  0 DIM(500)                             *N° GG SOSTA GIACENZ
019400000414     D NRG             S              9  0 DIM(500)                             *N° RIAPERTURE GIACE
019500011011     D CIS             S              9  0 DIM(500)                             *CONSEGNE ISOLE
019600011011     D RIS             S              9  0 DIM(500)                             *RITIRI ISOLE
019700020215     D CLD             S              9  0 DIM(500)                             *CLIENTE TIPO D
019800040802     D ded             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
019900040802     D val             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
020000040811     D sbnb            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020100040824     D sban            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020200040811     D nbn             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020300040802     D nba             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020400050808     D pks             S              9  0 DIM(500)                             *CLIENTE TIPO D
020500050808     D pkn             S              9  0 DIM(500)                             *CLIENTE TIPO D
020600040809     d dar5ban       e ds
020700040811     d dar5bnb       e ds
020800060829     d teets1        e ds                  extname(teets10f) prefix(£)
020900970417     D*-------------------
021000970417     D* SCHIERE DI LAVORO
021100970417     D*-------------------
021200050103     D WCTS            S              2    DIM(600)                             *CODICI
021300050103     D WRAP            S              2    DIM(600)                             *CODICE REGIONE
021400050103     D WNAR            S              2    DIM(600)                             *CODICE NAZIONE -> P
021500050103     D WTCT            S              1    DIM(600)                             *TIPO (E/R/N)   -> P
021600970416     D*-------------------
021700970416     D* SCHIERE DI PASSAGGIO PER LA SCRITTURA
021800970416     D*-------------------
021900970416     D* NON MULTIPLE
022000970416     D §SG             S             13  3 DIM(10)                              *SCAGLIONE -KG/COLLI
022100060829     c* ricavi
022200970416     D §PS             S              7  1 DIM(10)                              *PESO
022300970416     D §CS             S              7  1 DIM(10)                              *COLLI
022400970416     D §VS             S              7  3 DIM(10)                              *VOLUME
022500970416     D §SP             S              4  1 DIM(10)                              *% INCIDENZA
022600971008     D §SD             S              1    DIM(10)                              *FLAG VALORE DEDOTTO
022700060829     c* competenze
022800060829     D §PS1            S              7  1 DIM(10)                              *PESO
022900060829     D §CS1            S              7  1 DIM(10)                              *COLLI
023000060829     D §VS1            S              7  3 DIM(10)                              *VOLUME
023100060829     D §SP1            S              4  1 DIM(10)                              *% INCIDENZA
023200060829     D §SD1            S              1    DIM(10)                              *FLAG VALORE DEDOTTO
023300970416     D*---
023400050103     D §CT             S              2    DIM(600)                             *CODICE TASSAZIONE
023500050103     D §RC             S              1    DIM(600)                             *RAGGRUPPAMENTO
023600050103     D §CP             S              4  1 DIM(600)                             *% INCIDENZA
023700050103     D §CD             S              1    DIM(600)                             *FLAG VALORE DEDOTTO
023800020227     D*-------------------
023900020227     D* SCHIERA FILIALI FEDEX
024000020227     D*-------------------
024100020227     D FILFED          S              3  0 DIM(999)
024200081105     D*-------------------
024300081105     D* SCHIERA FILIALI DPD
024400081105     D*-------------------
024500081105     D FILdpd          S              3  0 DIM(999)
024600970110     D*------------------------------------------------------------------------*
024700970110     D* INPUT
024800970110     D*------------------------------------------------------------------------*
024900970110     D*------------------
025000970110     D* DS SCHIERE MULTIPLE
025100970110     D*------------------
025200970110     D*---
025300970110     D* BYTE IN PACKED ->
025400970317     D* (3:2) +1=2 ; (2X30) =60
025500970317     D* (9:2) +1=5 ; (5X30) =150
025600970317     D* (9:2) +1=5 ; (5X999)=4995
025700970317     D* (9:2) +1=5 ; (5X10) =50
025800050103     D* (9:2) +1=5 ; (5X600)=1500
025900970317     D* (13:2)+1=7 ; (7X10) =70
026000011001     D* (18:2)+1=10; (10X10) =100
026100980820     D*---
026200971124     D FPAG            DS                  OCCURS(500) INZ
026300970110     D  GGP                    1     60P 0
026400970110     D                                     DIM(30)                              *GG PAGAMENTO
026500970110     D  SGP                   61    210P 0
026600970110     D                                     DIM(30)                              *SPED. GG PAGAMENTO
026700971124     D FRIT            DS                  OCCURS(500) INZ
026800970110     D  RIT                    1   4995P 0
026900970110     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
027000971124     D FCON            DS                  OCCURS(500) INZ
027100970110     D  CON                    1   4995P 0
027200970110     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
027300971124     D FSGL            DS                  OCCURS(500) INZ
027400041118     D  SSP                    1     50P 0
027500041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
027600041118     D  PSC                   51    110P 1
027700041118     D                                     DIM(10)                              *PESO
027800041118     D  CSC                  111    160P 0
027900041118     D                                     DIM(10)                              *COLLI
028000041118     D  VSC                  161    220P 3
028100041118     D                                     DIM(10)                              *VOLUME
028200060829     D FSGL1           DS                  OCCURS(500) INZ
028300060829     D  SSP1                   1     50P 0
028400060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
028500060829     D  PSC1                  51    110P 1
028600060829     D                                     DIM(10)                              *PESO
028700060829     D  CSC1                 111    160P 0
028800060829     D                                     DIM(10)                              *COLLI
028900060829     D  VSC1                 161    220P 3
029000060829     D                                     DIM(10)                              *VOLUME
029100971124     D FCTS            DS                  OCCURS(500) INZ
029200050103     D  TSI                    1   3000P 0
029300050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
029400050103     D  TSE                 3001   6000P 0
029500050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
029600971124     D FTAR            DS                  OCCURS(500) INZ
029700970423     D  SSGL                   1     70P 3
029800970314     D                                     DIM(10)                              *SCAGLIONI
029900050103     D  CCTI                  71   1270
030000050103     D                                     DIM(600)                             *COD.TASSAZIONE ITAL
030100050103     D  CRCI                1271   1870
030200050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ITAL
030300050103     D  CCTE                1871   3070
030400050103     D                                     DIM(600)                             *COD.TASSAZIONE ESTE
030500050103     D  CRCE                3071   3670
030600050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ESTE
030700980820     D*---
030800980820     D* CARTELLO
030900980820     D*---
031000980824     D CCAR            DS                  OCCURS(2) INZ
031100980821     D  USPE                   1      5P 0
031200980821     D                                     DIM(1)                               *N°SPEDIZIONI ITA/ES
031300980821     D  UCLA                   6     10P 0
031400980821     D                                     DIM(1)                               *CLIENTE TIPO A
031500980821     D  UCLB                  11     15P 0
031600980821     D                                     DIM(1)                               *CLIENTE TIPO B
031700980821     D  UCLC                  16     20P 0
031800980821     D                                     DIM(1)                               *CLIENTE TIPO C
031900980821     D  UBON                  21     25P 0
032000980821     D                                     DIM(1)                               *BOLLETTAZIONE (' ')
032100980821     D  UBOA                  26     30P 0
032200980821     D                                     DIM(1)                               *BOLLETTAZIONE (A)
032300980821     D  UBOB                  31     35P 0
032400980821     D                                     DIM(1)                               *BOLLETTAZIONE (B)
032500980821     D  UNCL                  36     40P 0
032600980821     D                                     DIM(1)                               *COLLI
032700980821     D  UPKB                  41     45P 1
032800980821     D                                     DIM(1)                               *PESO
032900980821     D  UVLB                  46     50P 3
033000980821     D                                     DIM(1)                               *VOLUME
033100980821     D  UFRA                  51     55P 0
033200980821     D                                     DIM(1)                               *FRANCHI
033300980821     D  UASS                  56     60P 0
033400980821     D                                     DIM(1)                               *ASSEGNATI
033500980821     D  UPCV                  61     65P 0
033600980821     D                                     DIM(1)                               *CONSEGNE PRIVATI
033700980821     D  UPCM                  66     70P 0
033800980821     D                                     DIM(1)                               *CONSEGNE MAGAZZINO
033900980821     D  UPFD                  71     75P 0
034000980821     D                                     DIM(1)                               *FERMO DEPOSITO
034100980821     D  UESP                  76     80P 0
034200980821     D                                     DIM(1)                               *ESPRESSI
034300980821     D  UPPV                  81     85P 0
034400980821     D                                     DIM(1)                               *CONSEGNE PROVINCIA
034500980821     D  UPCA                  86     90P 0
034600980821     D                                     DIM(1)                               *CONSEGNE APPUNTAMEN
034700980821     D  UPCP                  91     95P 0
034800980821     D                                     DIM(1)                               *CONSEGNE PIANI
034900980821     D  UPCS                  96    100P 0
035000980821     D                                     DIM(1)                               *CONSEGNE SUPERMERCA
035100980821     D  UPCF                 101    105P 0
035200980821     D                                     DIM(1)                               *CONSEGNE FUORI MISU
035300980821     D  UPAB                 106    110P 0
035400980821     D                                     DIM(1)                               *C/ASSEGNI BARTOLINI
035500980821     D  UPAM                 111    115P 0
035600980821     D                                     DIM(1)                               *C/ASSEGNI MITTENTE
035700011002     D  UCAS                 116    125P 5
035800011002     D                                     DIM(1)                               *C/ASSEGNO
035900011002     D                                     PACKEVEN
036000011002     D  UPAR                 126    130P 0
036100980821     D                                     DIM(1)                               *ASSICURAZIONE R.C.V
036200011002     D  UPAC                 131    135P 0
036300980821     D                                     DIM(1)                               *ASSICURAZIONE X CON
036400011002     D  UASC                 136    145P 5
036500011001     D                                     DIM(1)                               *ASSICURAZIONE
036600011001     D                                     PACKEVEN
036700011002     D  UPGI                 146    150P 0
036800980821     D                                     DIM(1)                               *GIACENZE
036900011002     D  UPCD                 151    155P 0
037000980821     D                                     DIM(1)                               *CONSEGNE DDT
037100011002     D  UPCC                 156    160P 0
037200980821     D                                     DIM(1)                               *CONSEGNE C.STORICI
037300011002     D  UPCL                 161    165P 0
037400980821     D                                     DIM(1)                               *CONSEGNE L.DISAGIAT
037500011002     D  UCLT                 166    170P 0
037600980901     D                                     DIM(1)                               *CLIENTE TIPO T
037700011002     D  UGGS                 171    175P 0
037800000414     D                                     DIM(1)                               *GG SOSTA GIACENZA
037900011002     D  UNRG                 176    180P 0
038000000414     D                                     DIM(1)                               *N° RIAPERTURE GIACE
038100011011     D  UCIS                 181    185P 0
038200011011     D                                     DIM(1)                               *CONSEGNE ISOLE
038300011011     D  URIS                 186    190P 0
038400011011     D                                     DIM(1)                               *RITIRI ISOLE
038500020215     D  UCLD                 191    195P 0
038600020215     D                                     DIM(1)                               *CLIENTE TIPO D
038700040802     D  Uded                 196    200P 0
038800040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
038900040811     D  Usbnb                206    210P 0
039000040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039100040824     D  Usban                211    215P 0
039200040824     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039300040824     D  Unbn                 216    220P 0
039400040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039500040824     D  Unba                 221    225P 0
039600040811     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039700040824     D  Uval                 226    230P 0
039800040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039900050808     D  Upks                 231    235P 0
040000050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040100050808     D  Upkn                 236    240P 0
040200050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040300060315     D  Uxx1                 241    245P 0
040400060315     D                                     DIM(1)                               *ASSICURAZIONE X CON
040500060829     D  UNCL1                246    250P 0
040600060829     D                                     DIM(1)                               *COLLI
040700060829     D  UPKB1                251    255P 1
040800060829     D                                     DIM(1)                               *PESO
040900060829     D  UVLB1                256    260P 3
041000060829     D                                     DIM(1)                               *VOLUME
041100980824     D CPAG            DS                  OCCURS(2) INZ
041200980820     D  UGGP                   1     60P 0
041300980819     D                                     DIM(30)                              *GG PAGAMENTO
041400980820     D  USGP                  61    210P 0
041500980819     D                                     DIM(30)                              *SPED. GG PAGAMENTO
041600980824     D CRIT            DS                  OCCURS(2) INZ
041700980820     D  URIT                   1   4995P 0
041800980819     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
041900980824     D CCON            DS                  OCCURS(2) INZ
042000980820     D  UCON                   1   4995P 0
042100980819     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
042200980824     D CSGL            DS                  OCCURS(2) INZ
042300041118     D  USSP                   1     50P 0
042400041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
042500041118     D  UPSC                  51    110P 1
042600041118     D                                     DIM(10)                              *PESO
042700041118     D  UCSC                 111    160P 0
042800041118     D                                     DIM(10)                              *COLLI
042900041118     D  UVSC                 161    220P 3
043000041118     D                                     DIM(10)                              *VOLUME
043100060829     D CSGL1           DS                  OCCURS(2) INZ
043200060829     D  USSP1                  1     50P 0
043300060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
043400060829     D  UPSC1                 51    110P 1
043500060829     D                                     DIM(10)                              *PESO
043600060829     D  UCSC1                111    160P 0
043700060829     D                                     DIM(10)                              *COLLI
043800060829     D  UVSC1                161    220P 3
043900060829     D                                     DIM(10)                              *VOLUME
044000980824     D CCTS            DS                  OCCURS(2) INZ
044100050103     D  UTSI                   1   3000P 0
044200050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
044300050103     D  UTSE                3001   6000P 0
044400050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
044500040803     D Dtasflo       E DS
044600091125     D tnte04ds      E DS
044700970110     D*-------------------
044800970110     D* DS TIPI TARIFFA
044900970110     D*-------------------
045000970110     D DSTR          E DS
045100050808     D*-------------------
045200141002     D* DS nazioni
045300050808     D*-------------------
045400141002     D DS15          E DS
045500141002     D d15             S              3    DIM(1000)
045600141002     D ecf             S              1  0 DIM(1000)
045700141002     D pcf15           S              1  0 DIM(1000)
045800141002     D wnaz            S                   like(tasnzd)
045900141002     D wcad            S                   like(tascad)
046000141002     D*-------------------
046100141002     D* DS cod. trattamento merce
046200141002     D*-------------------
046300141002     D DS1b          E DS
046400021202     D*-------------------
046500021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
046600021202     D*-------------------
046700021202     D TRUL27DS      E DS
046800970110     D*-------------------
046900970110     D* DS CODICI TASSAZIONE
047000970110     D*-------------------
047100970110     D DSCT          E DS
047200970110     D*-------------------
047300970110     D* DS CAMBI E DIVISE
047400970110     D*-------------------
047500011011     D*DSCV          E DS
047600970110     D*-------------------
047700970110     D* DS RAGGRUPPAMENTI TIPI BOLLA
047800970110     D*-------------------
047900970110     D DS2W          E DS
048000970110     D*------------------
048100970110     D* DS TIPI BOLLA
048200970110     D*------------------
048300970110     D DSTB          E DS
048400970110     D*------------------
048500970110     D* DS CODICI PAGAMENTO
048600970110     D*------------------
048700970110     D DSFA          E DS
048800970110     D*------------------
048900970110     D* DS COMPOSIZIONE CODICE DI PAGAMENTO
049000970110     D*------------------
049100970110     D                 DS
049200970110     D  WCDP                   1      3
049300970110     D  WLIN                   4      4
049400970110     D  DSCDP                  1      4
049500970110     D*------------------
049600970110     D* DS INVIO DATI -BOLLETTAZIONE-
049700970110     D*------------------
049800970110     D DS3C          E DS
049900970110     D*------------------
050000020923     D* DS REPERIMENTO DATI UTENTE
050100970110     D*------------------
050200020923     D TIBS34DS      E DS                                                       *Profili utente
050300020923     D DDATIUTE      E DS                                                       *Dati utente
050400020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
050500970110     D*------------------
050600970110     D* DS "XSRDA8" - CONTROLLA DATA (8)
050700970110     D*------------------
050800970110     D WLBDA8          DS
050900970110     D  G08DAT                 1      8  0
051000970110     D  G08INV                 9     16  0
051100970110     D  G08ERR                17     17
051200970110     D  G08TGI                18     22  0
051300970110     D*-------------------
051400970110     D* DS SCOMPOSIZIONE DATA DECORRENZA(4) -TNTAM-
051500970110     D*-------------------
051600970110     D                 DS
051700970110     D  TAMDNN                 3      4  0
051800970110     D  TAMDMM                 5      6  0
051900970110     D  TAMDEC                 3      6  0
052000970110     D  TAMDDT                 1      8  0
052100970110     D*-------------------
052200970110     D* DS SCOMPOSIZIONE DATA SCADENZA(4) -TNTAM-
052300970110     D*-------------------
052400970110     D                 DS
052500970110     D  TAMSNN                 3      4  0
052600970110     D  TAMSMM                 5      6  0
052700970110     D  TAMSCA                 3      6  0
052800970110     D  TAMDST                 1      8  0
052900970110     D*-------------------
053000990812     D* DS COMPOSIZIONE DATA SPEDIZIONE -TITAS-
053100970110     D*-------------------
053200970110     D                 DS
053300970110     D  TASAAS                 1      4  0
053400970110     D  TASMGS                 5      8  0
053500970110     D  TASDSP                 1      8  0
053600970110     D*------------------
053700970110     D* DS PARAMETRI DI LANCIO
053800970110     D*------------------
053900970110     D PARAM           DS
054000980825     D  PARKSC                 1      7  0
054100980825     D  PARFPA                 8     10  0
054200980825     D  PARFAR                11     13  0
054300980825     D  PARFAP                14     16  0
054400980825     D  PARDS1                17     24  0
054500980825     D  PARAA1                17     20  0
054600980825     D  PARMG1                21     24  0
054700980825     D  PARDS2                25     32  0
054800980825     D  PARTAR                33     35
054900980825     D  PARCTO                36     36
055000980825     D  PARKOV                37     43  0
055100980825     D  PARCAR                37     41  0
055200980825     D  PARCTR                44     46  0
055300980825     D  PARTTR                44     44
055400980825     D  PARPRG                47     49  0
055500980825     D  PARLNP                50     52  0
055600980825     D  PARFIE                53     53
055700980825     D  PARSIM                54     54
055800980825     D  PARELA                55     55
055900011003     D  PARDIV                56     58
056000021031     D  PARUNI                59     59
056100040804     D  PARsta                60     60
056200040805     D  PARint                61     61
056300040805     D  PARsie                62     62
056400091130     D  PARdsf                63     63
056500970110     D*------------------
056600970110     D* ARCHITETTURA
056700970110     D*------------------
056800970110     D KPJBA         E DS
056900970110     D  PROFIL                 1     10
057000011011     D*------------------
057100011011     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
057200011011     D*------------------
057300011011     D DSGEDCE       E DS
057400011011     D*-------------------
057500011011     D* CONVERSIONI IMPORTI
057600011011     D*-------------------
057700011011     D YEUR          E DS                  EXTNAME(YEURCODS)
057800020227     D*-------------------
057900020227     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
058000020227     D*-------------------
058100020227     D OG143         E DS
058200020730     D*------------------
058300020730     D* DS PER LETTURA TABELLA "NTW"
058400020730     D*------------------
058500020730     D DNTW          E DS
058600021121     D*------------------
058700021121     D* DS RIDEFINIZIONE CAMPO TAMFLO DI TNTAM
058800021121     D*------------------
058900021121     D DSTA01        E DS
059000021031     D*-------------------
059100021031     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
059200021031     D*-------------------
059300021031     D TIBS10DS      E DS
059400021031     D  SKCLI                 21   5520  0 DIM(500)                              -cli. padri/figli
059500040811     D*-------------------
059600040811     D* CAMPI DI LAVORO
059700040811     D*-------------------
059800140923     D teett10ds     E DS                  EXTNAME(teett10f) prefix(£)
059900140922     d £inc            s                   like(£ettinc)
060000140922     d £nsp            s                   like(£ettnsp)
060100140922     d £cts            s                   like(£ettcts)
060200140922     d £ntw            s                   like(£ettntw)
060300140922     d colli           s                   like(tasncl)
060400060829     d colli1          s                   like(tasncl)
060500040811     d $pes            s                   like(taspkf)
060600060829     d $pes1           s                   like(taspkf)
060700060829     d etcpmc1         s                   like(etcpmc)
060800060829     d etcvmc1         s                   like(etcvmc)
060900970110     C*------------------------------------------------------------------------*
061000970110     C* MAIN LINE
061100970110     C*------------------------------------------------------------------------*
061200970313     C*
061300970313     C* SE NESSUN ERRORE IN APERTURA
061400970313IF  1C     $FINE         IFEQ      '0'
061500970313     C*
061600970314     C* ANNULLA I DATI TIPO ESISTENTI
061700970314     C                   EXSR      ANNETX
061800970110     C* APRE GLI ARCHIVI
061900970110     C                   EXSR      OPEFIL
062000970415     C*
062100970416     C* LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
062200970416     C                   EXSR      LETCLI
062300970416     C*
062400970416     C* REPERISCE LA QUALITA' DEL CLIENTE TARIFFA / OFFERTA
062500970416     C                   MOVEL     'C'           $CLV              1
062600970416IF  1C     CLPCLV        IFNE      *BLANKS
062700970416     C                   MOVEL     CLPCLV        $CLV
062800970416E   1C                   ENDIF
062900970416     C*
063000970416     C* REPERISCE I GIORNI DI PAGAMENTO DEL CLIENTE / VISITA
063100970416     C                   Z-ADD     *ZEROS        $GGP              3 0
063200970416     C                   EXSR      REPGGP
063300021129     C*
063400021129     C* CICLO SULLA SCHIERA DEI CLIENTI DA ELABORARE (DISCORSO UNIFICANTE)
063500021129     C     1             DO        500           JJ                3 0
063600021129     C                   Z-ADD     SKCLI(JJ)     KTAKSC                         *CLIENTE
063700970110     C*
063800970110     C* POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
063900970110     C                   EXSR      SETFIL
064000970110     C*
064100970110     C* CICLO FINO A FINE -BOLLE-
064200970313DO  2C     $FINE         DOWEQ     '0'
064300970110     C*
064400970110     C* MEMORIZZA I DATI
064500970110     C                   EXSR      MEMDAT
064600970110     C*
064700970110     C* LETTURA SUCESSIVA -BOLLE-
064800970110     C                   EXSR      LETFIL
064900970313E   2C                   ENDDO                                                  *FINE BOLLE
065000021129     C*
065100021129     C* DOPO L'ELABORAZIONE DI OGNI CLIENTE RE-INIZIALIZZO IL FLAG DI "FINE"
065200021129     C                   MOVEL     '0'           $FINE
065300021129     C*
065400021129     C* TESTO LE CONDIZIONI X CUI USCIRE DAL CICLO DI MEMORIZZAZIONE
065500021129     C                   IF        SKCLI(JJ) = *ZEROS
065600021129     C                   MOVEL     '1'           $FINE
065700021129     C                   LEAVE
065800021129     C                   ENDIF
065900021129     C*
066000021129     C                   ENDDO
066100970110     C*
066200970110     C* SCRIVE -SE RICHIESTO- E STAMPA I DATI TIPO
066300970110     C                   EXSR      AGGETX
066400980818     C*
066500980818     C* SCRIVE E STAMPA I DATI TIPO DI CARTELLO -SE RICHIESTE TUTTE LE LINE DI PARTENZA-
066600980818IF  2C     PARCTO        IFEQ      'T'
066700980818     C     PARCAR        ANDEQ     88888
066800980818     C     PARLNP        ANDEQ     999
066900980818     C                   EXSR      AGGET8
067000980818E   2C                   ENDIF
067100970110     C*
067200970110     C* CHIUDE GLI ARCHIVI
067300970110     C                   EXSR      CLOFIL
067400970313E   1C                   ENDIF
067500040804     c* lancio la stampa se richiesto
067600040804     c                   if        parsta = 'S'
067700040804     c                   call      'TNTE49R'
067800040804     c                   parm                    kpjba
067900040804     c                   end
068000970110     C*
068100970110     C                   SETON                                        LR
068200970415     C*------------------------------------------------------------------------*
068300970416     C* LETCLI - LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
068400970415     C*------------------------------------------------------------------------*
068500970416     C     LETCLI        BEGSR
068600970415     C*
068700060808     C                   CLEAR                   CNCLP000                       *AZZERA RECORD
068800060808     C                   CLEAR                   CNIND000                       *AZZERA RECORD
068900060808     C                   CLEAR                   tfCLP000                       *AZZERA RECORD
069000060808     C                   CLEAR                   tfIND000                       *AZZERA RECORD
069100020923     C                   Z-ADD     1             KCLKUT                         *PARTICOLARITA'
069200020923     C                   Z-ADD     DUTKCI        KCLKCC
069300970415     C                   Z-ADD     PARKOV        KCLKSC
069400020923     C                   Z-ADD     1             KINKUT                         *INDIRIZZO
069500020923     C                   Z-ADD     DUTKCI        KINKCC
069600970415     C                   Z-ADD     PARKOV        KINKSC
069700060808IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
069800060808     C     KEYCLP        CHAIN     CNCLP00F                           99
069900060808     C     KEYIND        CHAIN     CNIND00F                           99
070000060808X   1C                   ELSE                                                   *OFFERTA
070100060804     C     KEYCLP        CHAIN     TFCLP00F                           99
070200060804     C     KEYIND        CHAIN     TFIND00F                           99
070300060804IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATO
070400091125     c                   clear                   tnte04ds
070500091130     C                   movel     pardsf        d04dsf
070600091125     C                   Z-ADD     parkov        d04nrv
070700091125     c                   call      'TNTE04R'
070800091125     c                   parm                    kpjba
070900091126     c                   parm                    tnte04ds
071000091125     C                   Z-ADD     d04KSC        KCLKSC                         *CLIENTE DELLA VISIT
071100091125     C                   Z-ADD     d04KSC        KINKSC                         *CLIENTE DELLA VISIT
071200060808     C     KEYCLP        CHAIN     CNCLP00F                           99
071300060808     C     KEYIND        CHAIN     CNIND00F                           99
071400060804     c                   end
071500060808     c                   end
071600970415     C*
071700970415     C                   ENDSR
071800970415     C*------------------------------------------------------------------------*
071900970415     C* REPGGP - REPERISCE IL NUMERO DEI GIORNI DI PAGAMENTO
072000970415     C*------------------------------------------------------------------------*
072100970415     C     REPGGP        BEGSR
072200970415     C*---
072300970415     C* IL GG DI PAGAMENTO IN TABELLA POSSONO ESSERE ESPRESSI IN MESI SE <=12 O
072400970415     C* IN GIORNI SE >12;
072500970415     C* CI POSSONO ESSERE INOLTRE + RATE DI PAGAMENTO E IN QUESTO CASO PRENDE
072600970415     C* SEMPRE L'ULTIMA (ES. SE 30/60/90 PRENDE 90 COME GGP).
072700970415     C* IL PAGAMENTO A VISTA NON HA GIORNI, MA PER COMODITA' SI MEMORIZZA CON
072800970415     C* CON GIORNI DI PAGAMENTO = 999
072900970415     C*---
073000970415     C                   MOVE      INDCDP        WCDP                           *CONDIZIONE PAGAMENT
073100000323     C                   MOVEL     '1'           WLIN                           *LINGUA
073200970415     C                   SETOFF                                           98
073300970415     C                   Z-ADD     1             I
073400970415     C     DSCDP         LOOKUP    CDP(I)                                 98
073500970415IF  1C     *IN98         IFEQ      *ON                                          *TROVATO
073600970415     C                   MOVEA     CRAT(I)       RAT
073700970415     C                   Z-ADD     10            I
073800970415DO  2C     I             DOWGT     0
073900970415IF  3C     RAT(I)        IFGT      *ZEROS                                       *P I E N O
074000970415     C                   MOVEL     RAT(I)        WN3               3 0
074100970415IF  4C     WN3           IFGT      12                                           *ESPRESSO IN GIORNI
074200970415     C                   Z-ADD     WN3           $GGP
074300970415X   4C                   ELSE                                                   *ESPRESSO IN MESI
074400970415     C     WN3           MULT      30            $GGP
074500970415E   4C                   ENDIF
074600981020     C                   LEAVE                                                  *ESCE DAL CICLO
074700970415X   3C                   ELSE                                                   *V U O T O
074800970415     C     I             SUB       1             I                              *DECREMENTA INDICE
074900970415E   3C                   ENDIF
075000970415E   2C                   ENDDO
075100970415E   1C                   ENDIF
075200970415     C*
075300970415     C* PAGAMENTO A VISTA (GGP=0), IMPOSTA GGP=999
075400970415IF  1C     $GGP          IFEQ      *ZEROS
075500970415     C                   Z-ADD     999           $GGP
075600970415E   1C                   ENDIF
075700970415     C*
075800970415     C                   ENDSR
075900970110     C*------------------------------------------------------------------------*
076000970110     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
076100970110     C*------------------------------------------------------------------------*
076200970110     C     SETFIL        BEGSR
076300970110     C*
076400021105IF  1C     PARKSC        IFGT      *ZEROS
076500990812     C     K31TAS        SETLL     TITAS31C                           99        *FINE ARCHIVIO
076600970110X   1C                   ELSE
076700970110     C                   Z-ADD     PARAA1        KTAAAS                         *ANNO
076800970110     C                   Z-ADD     PARMG1        KTAMGS                         *MESE/GIORNO
076900990812     C     K38TAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
077000970110E   1C                   ENDIF
077100970110     C*
077200970110IF  1C     *IN99         IFEQ      *ON
077300970110     C                   MOVEL     '1'           $FINE                          *FINE PGM
077400970110X   1C                   ELSE
077500970110     C                   EXSR      LETFIL                                       *LETTURA RECORD
077600970110E   1C                   ENDIF
077700970110     C*
077800970110     C                   ENDSR
077900970110     C*------------------------------------------------------------------------*
078000970110     C* LETFIL - LETTURA PROSSIMO RECORD -BOLLE-
078100970110     C*------------------------------------------------------------------------*
078200970110     C     LETFIL        BEGSR
078300970110     C*
078400970110     C* LETTURA FINO A:
078500970110     C*  - FINE FILE
078600970110     C*  - TROVATO RECORD VALIDO
078700970110     C                   MOVEL     'N'           WRECOK
078800970110DO  1C     $FINE         DOWEQ     '0'
078900970110     C     WRECOK        ANDEQ     'N'
079000970110     C*
079100970110IF  2C     PARKSC        IFGT      *ZEROS
079200990812     C                   READ      TITAS31C                               99
079300970110X   2C                   ELSE
079400990812     C                   READ      TITAS38C                               99
079500970110E   2C                   ENDIF
079600970110     C*
079700970110IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
079800970110     C                   MOVEL     '1'           $FINE
079900970110X   2C                   ELSE
080000970110     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
080100970110E   2C                   ENDIF
080200970110E   1C                   ENDDO
080300970110     C*
080400970110     C                   ENDSR
080500970110     C*------------------------------------------------------------------------*
080600970110     C* CHKREC - CONTROLLA VALIDITA' RECORD -BOLLE-
080700970110     C*------------------------------------------------------------------------*
080800970110     C     CHKREC        BEGSR
080900970110     C*
081000970110     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
081100081105     C*
081200081105     C* se sto elaborando la cartello i dati tipo della cartello escludo le
081300081105     c* le spedizioni con lnp e lna DPD e FED
081400081105IF  2C     PARCTO        IFEQ      'T'
081500081105     C     PARCAR        ANDEQ     88888
081600081105     C     PARLNP        ANDEQ     999
081700081105     c* fedex
081800081105     C     taslnp        LOOKUP    FILFED                                 55
081900081105     C                   IF        *IN55 = *ON
082000081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
082100081105     C                   ENDIF
082200081105     C     taslna        LOOKUP    FILFED                                 55
082300081105     C                   IF        *IN55 = *ON
082400081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
082500081105     C                   ENDIF
082600081105     c* dpd
082700081105     C     taslnp        LOOKUP    FILdpd                                 55
082800081105     C                   IF        *IN55 = *ON
082900081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083000081105     C                   ENDIF
083100081105     C     taslna        LOOKUP    FILdpd                                 55
083200081105     C                   IF        *IN55 = *ON
083300081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083400081105     C                   ENDIF
083500081105E   1C                   ENDIF
083600970423     C*
083700970423     C* TIPO BOLLA NON VALIDO COME SPEDIZIONE O RICAVO, ESCLUDE
083800970416     C                   Z-ADD     1             I
083900970416     C                   SETOFF                                           98
084000970416     C     TASTBL        LOOKUP    TBL(I)                                 98
084100970416IF  1C     *IN98         IFEQ      *ON                                          *TIPO BOLLA TROVATO
084200970416IF  2C     SPD(I)        IFEQ      'S'
084300970416     C     RIC(I)        OREQ      'S'
084400970417     C                   MOVEL     SPD(I)        DEPSP             1            *VALIDA COME SPEDIZ
084500970417     C                   MOVEL     RIC(I)        DEPRI             1            *VALIDA COME RICAVO
084600970416X   2C                   ELSE
084700970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
084800970416E   2C                   ENDIF
084900970416X   1C                   ELSE
085000970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
085100970416E   1C                   ENDIF
085200970423     C*
085300970110     C* CLIENTE <> DA QUELLO RICHIESTO, ESCLUDE ED ESCE DAL PGM
085400970110IF  1C     PARKSC        IFGT      *ZEROS                                       *LETTURA X CLIENTE
085500021031     C* VERIFICO CHE IL CLIENTE CORRENTE SIA NELLA LISTA DI QUELLI DA CONSIDERARE
085600021031     C                   Z-ADD     TASKSC        WRKCLI           11 0
085700021031     C     WRKCLI        LOOKUP    SKCLI                                  60
085800021031IF  2C     *IN60         IFEQ      *OFF
085900021031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086000021105     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
086100021031E   2C                   ENDIF
086200970110E   1C                   ENDIF
086300970110     C*
086400970110     C* DATA SPEDIZIONE > DA DATA FINALE RICHIESTA, ESCLUDE ED ESCE DAL PGM
086500970110IF  1C     PARKSC        IFEQ      *ZEROS                                       *LETTURA X DATA
086600970110     C     PARDS1        ANDGT     *ZEROS
086700970110IF  2C     TASDSP        IFGT      PARDS2
086800970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086900970110     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
087000970110E   2C                   ENDIF
087100970110E   1C                   ENDIF
087200970110     C*
087300970430     C* FILIALE PARTENZA <> DA QUELLA RICHIESTA, ESCLUDE
087400970430IF  1C     PARFPA        IFGT      *ZEROS
087500970430IF  2C     PARFPA        IFNE      TASLNP
087600970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
087700970110E   2C                   ENDIF
087800970110E   1C                   ENDIF
087900970430     C*
088000970430     C* FILIALE ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
088100970430IF  1C     PARFAR        IFGT      *ZEROS
088200970430IF  2C     PARFAR        IFNE      TASLNA
088300970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
088400970430E   2C                   ENDIF
088500970430E   1C                   ENDIF
088600970430     C*
088700970430     C* FILIALE PARTENZA O ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
088800970430IF  1C     PARFAP        IFGT      *ZEROS
088900970430IF  2C     PARFAP        IFEQ      TASLNP
089000970430     C     PARFAP        OREQ      TASLNA
089100970430X   2C                   ELSE
089200970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
089300970430E   2C                   ENDIF
089400970430E   1C                   ENDIF
089500020227     C*
089600020227     C* QUALUNQUE FILIALE INDICATA NON SIA FEDEX
089700020307     C     PARFPA        IFGT      *ZEROS
089800020227     C     PARFPA        LOOKUP    FILFED                                 55
089900020227     C                   IF        *IN55 = *ON
090000020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090100020227     C                   ENDIF
090200020307     C                   ENDIF
090300020307     C     PARFAR        IFGT      *ZEROS
090400020227     C     PARFAR        LOOKUP    FILFED                                 55
090500020227     C                   IF        *IN55 = *ON
090600020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090700020227     C                   ENDIF
090800020307     C                   ENDIF
090900020307     C     PARFAP        IFGT      *ZEROS
091000020227     C     PARFAP        LOOKUP    FILFED                                 55
091100020227     C                   IF        *IN55 = *ON
091200020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
091300020227     C                   ENDIF
091400020307     C                   ENDIF
091500970110     C*
091600970110     C* DATA SPEDIZIONE <> DA QUELLE RICHIESTE, ESCLUDE
091700970110IF  1C     PARDS1        IFGT      *ZEROS
091800970110IF  2C     TASDSP        IFGE      PARDS1
091900970110     C     TASDSP        ANDLE     PARDS2
092000970110X   2C                   ELSE
092100970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
092200970110E   2C                   ENDIF
092300970110E   1C                   ENDIF
092400970313     C*
092500970313     C* CODICE TARIFFA <> DA QUELLO RICHIESTO, ESCLUDE
092600970313IF  1C     PARTAR        IFNE      *BLANKS
092700970313     C                   MOVEL     PARTAR        WN3               3 0
092800970313IF  2C     WN3           IFNE      TASCTR
092900970313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
093000970313E   2C                   ENDIF
093100970313E   1C                   ENDIF
093200970430     C*
093300970430     C* RECORD VALIDO, IMPOPSTA LA FILIALE SULLA QUALE MEMORIZZARE I DATI
093400970430IF  1C     WRECOK        IFEQ      'S'
093500970430IF  2C     PARLNP        IFNE      999                                          *SELEZIONE FILIALE
093600970430     C                   Z-ADD     PARLNP        DEPFIL                         *FILIALE=SELEZIONE
093700970430X   2C                   ELSE                                                   *SELEZIONE MULTIPLA
093800970430     C                   Z-ADD     TASLNP        DEPFIL                         *FILIALE=L.PARTENZA
093900970430E   2C                   ENDIF
094000060829     c                   z-add     tasncl        colli
094100060829     c                   z-add     tasncl        colli1
094200040811     c* reperisce i bancali. se BNB imposto nel campo di comodo COLLI
094300040811     c* i colli originali
094400040811     c                   clear                   dar5ban
094500040811     c                   clear                   dar5bnb
094600040811     c                   if        %subst(tasgva: 1: 1) = 'O' or
094700040811     c                             %subst(tasgva: 1: 1) = 'B'
094800040811     c                   if        %subst(tasgva: 1: 1) = 'O'
094900040811     c                   eval      ar5trd = 'BNB'
095000040811     c                   else
095100040811     c                   eval      ar5trd = 'BAN'
095200040811     c                   end
095300060829     c                   exsr      srfiar5
095400040811     c                   if        %subst(tasgva: 1: 1) = 'O'
095500040811     c                   eval      dar5bnb = ar5uni
095600060829     c                   z-add     §ar5bcor      colli
095700040811     c                   else
095800040811     c                   eval      dar5BAN = ar5uni
095900040811     c                   end
096000040811     c                   end
096100040811E   1C                   ENDIF
096200970416     C*
096300970110     C                   ENDSR
096400970110     C*------------------------------------------------------------------------*
096500970110     C* MEMDAT - MEMORIZZA I DATI -BOLLE-
096600970110     C*------------------------------------------------------------------------*
096700970110     C     MEMDAT        BEGSR
096800970110     C*
096900970110     C* RECUPERA L'INDICE DELLA FILIALE
097000970110     C                   Z-ADD     1             F                 4 0
097100970110     C                   SETOFF                                           98
097200970430     C     DEPFIL        LOOKUP    SORG(F)                                98
097300970110IF  1C     *IN98         IFEQ      *ON                                          *FILIALE TROVATA
097400970110     C     F             OCCUR     FPAG
097500970110     C     F             OCCUR     FCON
097600970110     C     F             OCCUR     FRIT
097700970110     C     F             OCCUR     FSGL
097800060829     C     F             OCCUR     FSGL1
097900970110     C     F             OCCUR     FCTS
098000970423     C     F             OCCUR     FTAR
098100970415     C*
098200970415     C* REPERISCE LA SOLUZIONE DEL CLIENTE
098300970415     C                   MOVEL     *BLANKS       $SOL              1
098400970415     C                   EXSR      REPSOL
098500060315     C*
098600060315     C* REPERISCE le varie di fatturazione
098700060315     C                   clear                   svn
098800060315     C                   clear                   van
098900060315     C                   EXSR      REPvrf
099000970415     C*
099100060829     C* REPERISCE IL VOLUME DELLA BOLLA
099200970415     C                   EXSR      REPVOL
099300040811     C*
099400060829     C* REPERISCE IL PESO DELLA BOLLA da
099500040811     C                   EXSR      REPpes
099600970110     C*
099700060315     C* REPERISCE LE consegne particolari
099800970110     C                   MOVEL     *BLANKS       $PCA              1
099900970110     C                   MOVEL     *BLANKS       $PCP              1
100000970110     C                   MOVEL     *BLANKS       $PCS              1
100100970110     C                   MOVEL     *BLANKS       $PCF              1
100200970415     C                   MOVEL     *BLANKS       $PCD              1
100300990812     C                   MOVEL     *BLANKS       $PIN              1            *INOLTRO
100400011026     C                   MOVEL     *ZEROS        $PINIM           21 8          *IMPORTO INOLTRO
100500011011     C                   MOVEL     *BLANKS       $CIS              1
100600011011     C                   MOVEL     *BLANKS       $RIS              1
100700040802     C                   MOVEL     *BLANKS       $ded              1
100800970110     C                   EXSR      REPVAR
100900000414     C*
101000000414     C* REPERISCE LA GIACIENZA
101100000414     C                   MOVEL     *BLANKS       $PGI              1            *ESISTE GIACENZA
101200000414     C                   Z-ADD     *ZEROS        $PGIGS            9 0          *GG SOSTA
101300000414     C                   Z-ADD     *ZEROS        $PGINR            9 0          *N° RIAPERTURE
101400021122     C                   EXSR      REPGIA
101500970110     C*
101600970110     C* REPERISCE IL C/ASSEGNO
101700011026     C                   Z-ADD     *ZEROS        $CAS             21 8
101800970110     C                   MOVEL     *BLANKS       $TPI              1
101900970110IF  2C     TASFCA        IFEQ      'S'                                          *ESISTE C/ASSEGNO
102000970110     C                   EXSR      REPCAS
102100970110E   2C                   ENDIF
102200970110     C*
102300970110     C* REPERISCE L'ASSICURAZIONE
102400011026     C                   Z-ADD     *ZEROS        $IAS             21 8
102500970110     C                   EXSR      REPASS
102600970110     C*
102700970110     C* REPERISCE IL CODICE TASSAZIONE
102800970423     C                   Z-ADD     1             J
102900970423     C                   SETOFF                                           98
103000970423     C     TASCTS        LOOKUP    CTS(J)                                 98
103100970423IF  2C     *IN98         IFEQ      *ON                                          *TROVATO
103200970416     C*---
103300970110     C*  M E M O R I Z Z A
103400970416     C*---
103500970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
103600970423     C*
103700971204IF  4C     FL1(F)        IFNE      *BLANKS                                      *CODICE ESTERO
103800970423     C     CIE(J)        OREQ      'E'
103900970423     C                   Z-ADD     1             I
104000970423     C                   SETOFF                                           98
104100970423     C     TASCTS        LOOKUP    CCTE(I)                                98
104200970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
104300140916     C                   ADD       1             TSE(I)                         *N°SPED.CTS ESTERO
104400970423E   5C                   ENDIF
104500140923     c* memorizzo nr sped. x cap e nazione solo estero
104600140922     c                   movel     sntw(f)       xntw
104700140923     c                   movel     taslnp        xorg
104800140923     c                   movel     tascts        kttcts
104900140923     c                   movel     tasnzd        kttnaz
105000141002     C                   MOVEl     tasnzd        Wnaz
105100141002     c     wnaz          lookup    d15(i)                                 88
105200141002     c                   if        *in88
105300141002     C                   eval      wcad = %subst(tascad: pcf15(i): ecf(i))
105400141002     c                   movel     wcad          kttcad
105500140919     c                   exsr      scrett10
105600141002     c                   end
105700140919     C*
105800970423X   4C                   ELSE                                                   *CODICE ITALIA
105900970423     C                   Z-ADD     1             I
106000970423     C                   SETOFF                                           98
106100970423     C     TASCTS        LOOKUP    CCTI(I)                                98
106200970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
106300970423     C                   ADD       1             TSI(I)                         *N°SPED.CTS ITALIA
106400970423E   5C                   ENDIF
106500970423E   4C                   ENDIF
106600970423     C*
106700970423     C                   ADD       1             SPE(F)                         *N°SPEDIZIONI TOTALI
106800020215     C*
106900020215IF  4C     $CLV          IFEQ      'D'
107000020215     C                   ADD       1             CLD(F)                         *CLIENTE TIPO D
107100020215E   4C                   ENDIF
107200980901IF  4C     $CLV          IFEQ      'T'
107300980901     C                   ADD       1             CLT(F)                         *CLIENTE TIPO T
107400980901E   4C                   ENDIF
107500970416IF  4C     $CLV          IFEQ      'A'
107600970110     C                   ADD       1             CLA(F)                         *CLIENTE TIPO A
107700970416E   4C                   ENDIF
107800970416IF  4C     $CLV          IFEQ      'B'
107900970110     C                   ADD       1             CLB(F)                         *CLIENTE TIPO B
108000970416E   4C                   ENDIF
108100970416IF  4C     $CLV          IFEQ      'C'
108200970415     C     $CLV          OREQ      *BLANKS
108300970110     C                   ADD       1             CLC(F)                         *CLIENTE TIPO C
108400970416E   4C                   ENDIF
108500970110     C*
108600970416IF  4C     $SOL          IFEQ      *BLANKS
108700970110     C                   ADD       1             BON(F)                         *BOLLETTAZIONE (' ')
108800970416E   4C                   ENDIF
108900970416IF  4C     $SOL          IFEQ      'A'
109000970110     C                   ADD       1             BOA(F)                         *BOLLETTAZIONE (A)
109100970416E   4C                   ENDIF
109200970416IF  4C     $SOL          IFEQ      'B'
109300970110     C                   ADD       1             BOB(F)                         *BOLLETTAZIONE (B)
109400970416E   4C                   ENDIF
109500050808     c* con il cod. trattamento merce aggancio tabella 1B per vedere se
109600050808     c* previsto packing list
109700050808     c                   z-add     1             xp                4 0
109800050808     c     tasctm        lookup    cpkl(xp)                               95
109900050808     c                   if        *in95
110000050808     c                   add       1             pks(f)
110100050808     c                   else
110200050808     c                   add       1             pkn(f)
110300050808     c                   end
110400040811     C* se oltre 20 colli sommo i colli originali altrimenti i colli
110500040811     c* della bolla
110600040811     C                   ADD       colli         NCL(F)                         *COLLI
110700040811     C                   ADD       $pes          PKB(F)                         *PESO
110800040811     C                   ADD       $vol          VLB(F)                         *VOLUME
110900060829     C                   ADD       colli1        NCL1(F)                        *COLLI
111000060829     C                   ADD       $pes1         PKB1(F)                        *PESO
111100060829     C                   ADD       $vol1         VLB1(F)                        *VOLUME
111200970110     C*
111300970110     C                   MOVEL     TASTBL        WA1               1
111400970416IF  4C     WA1           IFEQ      'F'
111500970110     C                   ADD       1             FRA(F)                         *FRANCHI
111600970416X   4C                   ELSE
111700970110     C                   ADD       1             ASS(F)                         *ASSEGNATI
111800970416E   4C                   ENDIF
111900970110     C*
112000970110     C                   MOVEL     TASGVA        WA1
112100970416IF  4C     WA1           IFEQ      'P'
112200970110     C                   ADD       1             PCV(F)                         *CONSEGNE PRIVATI
112300970416E   4C                   ENDIF
112400040802     C* bancali bartolini
112500040802IF  4C     WA1           IFEQ      'B'
112600040824     C                   ADD       1             sban(F)                        *CONSEGNE PRIVATI
112700040809     C                   ADD       §ar5nba       nba(F)                         *CONSEGNE PRIVATI
112800040809     C                   ADD       §ar5nb2       nba(F)                         *CONSEGNE PRIVATI
112900040802E   4C                   ENDIF
113000040802     C* bancali oltre 20 colli
113100040802IF  4C     WA1           IFEQ      'O'
113200040811     C                   ADD       1             sbnb(F)                        *CONSEGNE PRIVATI
113300040811     C                   ADD       §ar5bnba      nbn(F)                         *CONSEGNE PRIVATI
113400040811     C                   ADD       §ar5bnb2      nbn(F)                         *CONSEGNE PRIVATI
113500040802E   4C                   ENDIF
113600040802     C* merce di valore
113700040802     c                   movel     tasflo        dtasflo
113800040802IF  1C     §FLOTBA       IFEQ      'V'
113900040802     C                   add       1             val(F)
114000040802E   1C                   ENDIF
114100970110     C*
114200970416IF  4C     TASFDN        IFEQ      'S'
114300011011     C                   ADD       1             PCM(F)                         *MERCE A MAGAZZINO
114400970416E   4C                   ENDIF
114500970110     C*
114600970416IF  4C     TASFFD        IFEQ      'S'
114700970110     C                   ADD       1             PFD(F)                         *FERMO DEPOSITO
114800970416E   4C                   ENDIF
114900970110     C*
115000970416IF  4C     TASTSP        IFEQ      'E'
115100970110     C                   ADD       1             ESP(F)                         *ESPRESSI
115200970416E   4C                   ENDIF
115300970110     C*
115400011011IF  4C     TASFIN        IFEQ      'D'
115500011011     C                   ADD       1             PCL(F)                         *CONS. L.DISAGIATE
115600011011E   4C                   ENDIF
115700011011     C*
115800011011IF  4C     TASFIN        IFEQ      'I'
115900011011     C                   ADD       1             CIS(F)                         *CONS. ISOLE
116000011011E   4C                   ENDIF
116100011011     C*
116200011011IF  4C     TASFAP        IFEQ      'I'
116300011011     C                   ADD       1             RIS(F)                         *RITIRI ISOLE
116400011011E   4C                   ENDIF
116500011011     C*
116600011011IF  4C     TASFIN        IFEQ      'P'
116700110623IF  4C     TASFIN        oreq      'Z'
116800011011     C                   ADD       1             PPV(F)                         *CONSEGNE PROVINCIA
116900970416E   4C                   ENDIF
117000970110     C*
117100970416IF  4C     $PCA          IFEQ      'S'
117200970110     C                   ADD       1             PCA(F)                         *CONS. APPUNTAMENTO
117300970416E   4C                   ENDIF
117400970110     C*
117500970416IF  4C     $PCP          IFEQ      'S'
117600970110     C                   ADD       1             PCP(F)                         *CONS. PIANI
117700970416E   4C                   ENDIF
117800970110     C*
117900970416IF  4C     $PCS          IFEQ      'S'
118000970110     C                   ADD       1             PCS(F)                         *CONS. SUPERMERCATI
118100970416E   4C                   ENDIF
118200970110     C*
118300970416IF  4C     $PCF          IFEQ      'S'
118400970110     C                   ADD       1             PCF(F)                         *CONS. FUORI MISURA
118500970416E   4C                   ENDIF
118600040802     C* destinatario disagiato
118700040802IF  4C     $ded          IFEQ      'S'
118800040802     C                   ADD       1             ded(F)                         *CONS. FUORI MISURA
118900040802E   4C                   ENDIF
119000970415     C*
119100970416IF  4C     $PCD          IFEQ      'S'
119200970415     C                   ADD       1             PCD(F)                         *CONS. DDT
119300970416E   4C                   ENDIF
119400970110     C*
119500970416IF  4C     $GGP          IFGT      *ZEROS
119600970110     C                   Z-ADD     1             I
119700970110     C                   SETOFF                                           98
119800970110     C     $GGP          LOOKUP    GGP(I)                                 98
119900970416IF  5C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
120000970110     C                   ADD       1             SGP(I)                         *SPED. GG PAGAMENTO
120100970416X   5C                   ELSE                                                   *MAI MEMORIZZATI
120200970110     C                   Z-ADD     1             I
120300970416DO  6C     I             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
120400970416IF  7C     GGP(I)        IFEQ      *ZEROS                                       *DISPONIBILE
120500970110     C                   Z-ADD     $GGP          GGP(I)                         *GG
120600970110     C                   Z-ADD     1             SGP(I)                         *SPED. GG PAGAMENTO
120700981014     C                   LEAVE                                                  *ESCE DAL CICLO
120800970416E   7C                   ENDIF
120900970416E   6C                   ENDDO
121000970416E   5C                   ENDIF
121100970416E   4C                   ENDIF
121200000411     C*
121300000411     C                   MOVEL     TASTBL        WA1               1
121400000411IF  4C     WA1           IFEQ      'F'                                          *SOLO FRANCHI
121500000411IF  5C     TASSTR        IFGT      *ZEROS
121600990812     C     TASSTR        ANDNE     99999
121700000411IF  6C     TASSTR        IFGT      999                                          *CAMPO 5,0 -> 3,0
121800990812     C                   Z-ADD     999           TASSTR
121900000411E   6C                   ENDIF
122000970110     C                   Z-ADD     TASSTR        I                              *STOP RITIRO
122100970110     C                   ADD       1             RIT(I)                         *SPEDIZIONI STOP
122200000411E   5C                   ENDIF
122300000411     C*
122400000411X   4C                   ELSE                                                   *SOLO ASSEGNATI
122500000411     C*
122600000411IF  5C     TASSTP        IFGT      *ZEROS
122700990812     C     TASSTP        ANDNE     99999
122800000411IF  6C     TASSTP        IFGT      999                                          *CAMPO 5,0 -> 3,0
122900990812     C                   Z-ADD     999           TASSTP
123000000411E   6C                   ENDIF
123100970110     C                   Z-ADD     TASSTP        I                              *STOP CONSEGNA
123200970110     C                   ADD       1             CON(I)                         *SPEDIZIONI STOP
123300000411E   5C                   ENDIF
123400000411E   4C                   ENDIF
123500970110     C*
123600060829     c* per ricavi
123700970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
123800970110     C     PARTTR        OREQ      '3'                                          *KG
123900040811     C                   Z-ADD     $pes          TASSGL           13 3          *SCAGLIONE IN KG
124000970416X   4C                   ELSE
124100040811     C                   Z-ADD     colli         TASSGL                         *SCAGLIONE IN COLLI
124200040811E   4C                   ENDIF
124300971024IF  4C     TASSGL        IFGT      *ZEROS
124400971024DO  5C     1             DO        10            I
124500971024IF  6C     TASSGL        IFLE      SSGL(I)
124600970110     C                   ADD       1             SSP(I)                         *N°SPED SCAGLIONE
124700040811     C                   ADD       $pes          PSC(I)                         *PESO
124800040811     C                   ADD       colli         CSC(I)                         *COLLI
124900970415     C                   ADD       $VOL          VSC(I)                         *VOLUME
125000970110     C                   LEAVE
125100971024E   6C                   ENDIF
125200971024E   5C                   ENDDO
125300040920     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
125400060829IF  5C     PARCTO        IFEQ      'T'
125500040920     C     PARCAR        ANDEQ     88888
125600040920     C     PARLNP        ANDEQ     999
125700040920     C                   EXSR      SRSGL8
125800060829E   5C                   ENDIF
125900971024E   4C                   ENDIF
126000060829     c* per competenze
126100060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
126200060829     C     PARTTR        OREQ      '3'                                          *KG
126300060829     C                   Z-ADD     $pes1         TASSGL           13 3          *SCAGLIONE IN KG
126400060829X   4C                   ELSE
126500060829     C                   Z-ADD     colli1        TASSGL                         *SCAGLIONE IN COLLI
126600060829E   4C                   ENDIF
126700060829IF  4C     TASSGL        IFGT      *ZEROS
126800060829DO  5C     1             DO        10            I
126900060829IF  6C     TASSGL        IFLE      SSGL(I)
127000060829     C                   ADD       1             SSP1(I)                        *N°SPED SCAGLIONE
127100060829     C                   ADD       $pes1         PSC1(I)                        *PESO
127200060829     C                   ADD       colli1        CSC1(I)                        *COLLI
127300060829     C                   ADD       $VOL1         VSC1(I)                        *VOLUME
127400060829     C                   LEAVE
127500060829E   6C                   ENDIF
127600060829E   5C                   ENDDO
127700060829     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
127800060829IF  5C     PARCTO        IFEQ      'T'
127900060829     C     PARCAR        ANDEQ     88888
128000060829     C     PARLNP        ANDEQ     999
128100060829     C                   EXSR      SRSGL81
128200060829E   5C                   ENDIF
128300060829E   4C                   ENDIF
128400971024E   3C                   ENDIF
128500970110     C*
128600970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
128700970416     C     DEPRI         OREQ      'S'                                          *BOLLA VALIDA RICAVI
128800970416IF  4C     $CAS          IFGT      *ZEROS
128900970110     C                   ADD       $CAS          CAS(F)                         *C/ASSEGNO
129000970416IF  5C     $TPI          IFEQ      'M'
129100970110     C                   ADD       1             PAM(F)                         *MITTENTE
129200970416X   5C                   ELSE
129300970110     C                   ADD       1             PAB(F)                         *BARTOLINI
129400970416E   5C                   ENDIF
129500970416E   4C                   ENDIF
129600970110     C*
129700970416IF  4C     $IAS          IFGT      *ZEROS
129800970110     C                   ADD       $IAS          ASC(F)                         *ASSICURAZIONE
129900970110     C                   ADD       1             PAC(F)                         *ASSICURAZIONE CONTO
130000970416X   4C                   ELSE
130100060315     c                   if        flgacbase = *on
130200060315     C                   ADD       1             xx1(F)                         *ASSICURAZIONE R.C.V
130300060315     c                   else
130400970110     C                   ADD       1             PAR(F)                         *ASSICURAZIONE R.C.V
130500970416E   4C                   ENDIF
130600060315E   4C                   ENDIF
130700970110     C*
130800000414IF  4C     $PGI          IFEQ      'S'
130900970110     C                   ADD       1             PGI(F)                         *GIACENZE
131000000414     C                   ADD       $PGIGS        GGS(F)                         *GG SOSTA GIACENZA
131100000414     C                   ADD       $PGINR        NRG(F)                         *N° RIAPERTURE GIACE
131200970416E   4C                   ENDIF
131300970416E   3C                   ENDIF
131400970110     C*
131500970416X   2C                   ELSE                                                   *CON.TASS.NON TROVAT
131600970416     C                   MOVEL     MTAS(2)       DEPERR           50
131700970110     C                   MOVEL     'T'           DEPTER            1
131800970110     C                   EXSR      STAERR
131900970416E   2C                   ENDIF
132000970110     C*
132100970110X   1C                   ELSE                                                   *FILIALE NON TROVATA
132200970110     C                   MOVEL     MTAS(1)       DEPERR
132300970110     C                   MOVEL     'T'           DEPTER
132400970110     C                   EXSR      STAERR
132500970110E   1C                   ENDIF
132600970110     C*
132700970110     C                   ENDSR
132800970415     C*------------------------------------------------------------------------*
132900040809     C* fiar5 tipo record BAN
133000970415     C*------------------------------------------------------------------------*
133100040809     C     srfiar5       BEGSR
133200040809     c*
133300040809     c                   z-add     tasaas        ar5aas
133400040809     c                   z-add     taslnp        ar5lnp
133500040809     c                   z-add     tasnrs        ar5nrs
133600040809     c                   z-add     tasnsp        ar5nsp
133700040811     c                   clear                   ar5uni
133800040809     c     kar5          chain     fiar531c
133900040809     C*
134000040809     C                   ENDSR
134100040809     C*------------------------------------------------------------------------*
134200040809     C* REPSOL - REPERISCE LA SOLUZIONE DI INVIO DATI -BOLLETTAZIONE-
134300040809     C*------------------------------------------------------------------------*
134400040809     C     REPSOL        BEGSR
134500970415     C*---
134600970415     C* NON E' PRESENTE IN TABELLA -> NESSUNA PROCEDURA            ( )
134700970415     C* NON HA IL NUMERO DI SERIE  -> STAMPA LE BOLLE              (A)
134800970415     C* HA     IL NUMERO DI SERIE  -> STAMPA LE BOLLE + SEGNACOLLI (B)
134900040310     C*--- escludo bolle recapito contrassegni
135000970415IF  1C     TASNRS        IFGT      *ZEROS
135100040310IF  1C     TAScbo        andne     'FW'
135200970415     C                   MOVEL     'B'           $SOL                           *PROCEDURA 'B'
135300970415X   1C                   ELSE
135400100303     C                   Z-ADD     1             KTBKUT
135500100303     C                   MOVEL     '3C'          KTBCOD
135600100303     c                   movel(p)  tasksc        ktbkey
135700100303     c     keytab        chain     tabel00f
135800100303     c                   if        %found(tabel00f) and tblflg = ' '
135900100303     C                   MOVEL     TBLUNI        DS3C
136000100303IF  3C     §3COKD        IFEQ      '*'
136100970415     C                   MOVEL     'A'           $SOL                           *PROCEDURA 'A'
136200970415X   3C                   ELSE
136300970415     C                   MOVEL     *BLANKS       $SOL                           *PROCEDURA NO AVVIAT
136400970415E   3C                   ENDIF
136500970415E   2C                   ENDIF
136600970415E   1C                   ENDIF
136700970415     C*
136800970415     C                   ENDSR
136900970415     C*------------------------------------------------------------------------*
137000970415     C* REPVOL - REPERISCE IL VOLUME DELLA BOLLA
137100970415     C*------------------------------------------------------------------------*
137200970415     C     REPVOL        BEGSR
137300060829     C* colli x ricavi
137400060829     C                   Z-ADD     *ZEROS        $VOL              5 3
137500970415     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
137600040811    1C     TASVLC        IFGT      *ZEROS
137700040811     C     colli         ANDEQ     TASNCR
137800970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
137900970415E   1C                   ENDIF
138000970415     C*
138100970415     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
138200970415     C     TASVLC        IFGT      TASVLF
138300040811     C     colli         ANDNE     TASNCR
138400970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
138500970415E   1C                   ENDIF
138600970415     C*
138700970415     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
138800970415IF  1C     $VOL          IFEQ      *ZEROS
138900970415     C                   Z-ADD     TASVLF        $VOL                           *VOLUME NORMALE
139000060829E   1C                   ENDIF
139100060829c*   c*colli x competetnze
139200060829     C                   Z-ADD     *ZEROS        $VOL1             5 3
139300060829     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
139400060829    1C     TASVLC        IFGT      *ZEROS
139500060829     C     colli1        ANDEQ     TASNCR
139600060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
139700060829E   1C                   ENDIF
139800060829     C*
139900060829     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
140000060829     C     TASVLC        IFGT      TASVLF
140100060829     C     colli1        ANDNE     TASNCR
140200060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
140300060829E   1C                   ENDIF
140400060829     C*
140500060829     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
140600060829IF  1C     $VOL1         IFEQ      *ZEROS
140700060829     C                   Z-ADD     TASVLF        $VOL1                          *VOLUME NORMALE
140800060829E   1C                   ENDIF
140900970415     C*
141000000414     C                   ENDSR
141100040811     C*------------------------------------------------------------------------*
141200040811     C* REPpes - REPERISCE IL peso DELLA BOLLA
141300040811     C*------------------------------------------------------------------------*
141400040811     C     REPpes        BEGSR
141500060829     C* colli x ricavi
141600060829     C                   Z-ADD     *ZEROS        $pes
141700040811     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
141800040811    1C     TASpkC        IFGT      *ZEROS
141900040811     C     colli         ANDEQ     TASNCp
142000040811     C                   Z-ADD     TASpkc        $pes                           *VOLUME CML
142100040811E   1C                   ENDIF
142200040811     C*
142300040811     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
142400040811     C     TASpkc        IFGT      TASpkF
142500040811     C     colli         ANDNE     TASNCp
142600040811     C                   Z-ADD     TASpkC        $pes                           *VOLUME CML
142700040811E   1C                   ENDIF
142800040811     C*
142900040811     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
143000040811IF  1C     $pes          IFEQ      *ZEROS
143100040811     C                   Z-ADD     TASpkF        $pes                           *VOLUME NORMALE
143200040811E   1C                   ENDIF
143300060829     C* colli x competenze
143400060829     C                   Z-ADD     *ZEROS        $pes1
143500060829     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
143600060829    1C     TASpkC        IFGT      *ZEROS
143700060829     C     colli1        ANDEQ     TASNCp
143800060829     C                   Z-ADD     TASpkc        $pes1                          *VOLUME CML
143900060829E   1C                   ENDIF
144000060829     C*
144100060829     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
144200060829     C     TASpkc        IFGT      TASpkF
144300060829     C     colli1        ANDNE     TASNCp
144400060829     C                   Z-ADD     TASpkC        $pes1                          *VOLUME CML
144500060829E   1C                   ENDIF
144600060829     C*
144700060829     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
144800060829IF  1C     $pes1         IFEQ      *ZEROS
144900060829     C                   Z-ADD     TASpkF        $pes1                          *VOLUME NORMALE
145000060829E   1C                   ENDIF
145100040811     C*
145200040811     C                   ENDSR
145300970110     C*------------------------------------------------------------------------*
145400970110     C* REPCAS - REPERISCE IL CONTRASSEGNO
145500970110     C*------------------------------------------------------------------------*
145600970110     C     REPCAS        BEGSR
145700970110     C*
145800970110     C                   Z-ADD     TASAAS        KSBAAS                         *ANNO
145900970110     C                   Z-ADD     TASLNP        KSBLNP                         *LINEA PARTENZA
146000970110     C                   Z-ADD     TASNRS        KSBNRS                         *SERIE
146100970110     C                   Z-ADD     TASNSP        KSBNSP                         *SPEDIZIONE
146200970110     C     KEYCSB        CHAIN     TNCSB03L                           98
146300970110IF  1C     *IN98         IFEQ      *OFF                                         *TROVATO
146400970110IF  2C     CSBSTA        IFNE      9                                            *NO ANNULLATO
146500970110     C                   MOVEL     CSBTPI        $TPI                           *TIPO INCASSO
146600011011     C                   Z-ADD     CSBCAS        $CAS                           *C/ASSEGNO
146700011011     C     CSBVCA        IFNE      PARDIV
146800011011     C                   Z-ADD     $CAS          WIMI             30 9
146900011011     C                   MOVEL     CSBVCA        WDVI              3
147000011011     C                   EXSR      CNVEUR
147100011011     C                   Z-ADD     WIMO          $CAS
147200011011     C                   ENDIF
147300970110E   2C                   ENDIF
147400970110E   1C                   ENDIF
147500970110     C*
147600970110     C                   ENDSR
147700021122     C*------------------------------------------------------------------------*
147800021122     C* REPGIA - REPERISCE LE GIACENZE
147900021122     C*------------------------------------------------------------------------*
148000021122     C     REPGIA        BEGSR
148100021122     C*
148200021122     C* AZZERA LE VARIABILI DI LAVORO
148300021122     C                   Z-ADD     *ZEROS        §GGSOS            3 0
148400021122     C*
148500021122     C* LEGGE TUTTE LE GIACENZE DELLA BOLLA
148600021122     C                   Z-ADD     TASAAS        KGPAAS
148700021122     C                   Z-ADD     TASLNP        KGPLNP
148800021122     C                   Z-ADD     TASNRS        KGPNRS
148900021122     C                   Z-ADD     TASNSP        KGPNSP
149000050309     C     K01GCP        SETLL     TIGCP51L
149100050309     C     K01GCP        READE     TIGCP51L                               98
149200021122DO  1C     *IN98         DOWEQ     *OFF
149300021122IF  2C     GCPATB        IFEQ      *BLANKS
149400021122     C                   MOVEL     'S'           $PGI                           *ESISTE GIACENZA
149500021122     C* APERTURE
149600021122     C                   ADD       1             $PGINR                         *NUMERO RIAPERTURE
149700021122     C* GIORNI DI SOSTA
149800021122IF  3C     GCPDDM        IFGT      *ZEROS                                       *IMMESSE DISPOSIZION
149900021122IF  4C     GCPDUR        IFGT      *ZEROS                                       *ULTIMA RIAPERTURA
150000021122     C                   Z-ADD     GCPDUR        G08INV
150100021122X   4C                   ELSE                                                   *APERTURA GIACENZA
150200021122     C                   MOVEL     GCPAGC        G08INV
150300021122     C                   MOVE      GCPMGC        G08INV
150400021122E   4C                   ENDIF
150500021122     C                   Z-ADD     *ZEROS        G08DAT
150600021122     C                   MOVEL     '3'           G08ERR
150700021122     C                   CALL      'XSRDA8'
150800021122     C                   PARM                    WLBDA8
150900021122     C                   Z-ADD     G08TGI        §GGDA             5 0
151000021122IF  4C     GCPDED        IFGT      *ZEROS                                       *DISPOSIZ.GIACENZA
151100021122     C                   Z-ADD     GCPDED        G08INV
151200021122X   4C                   ELSE                                                   *DISPOSIZ.MITTENTE
151300021122     C                   Z-ADD     GCPDDM        G08INV
151400021122E   4C                   ENDIF
151500021122     C                   Z-ADD     *ZEROS        G08DAT
151600021122     C                   MOVEL     '3'           G08ERR
151700021122     C                   CALL      'XSRDA8'
151800021122     C                   PARM                    WLBDA8
151900021122     C                   Z-ADD     G08TGI        §GGA              5 0
152000021122     C* GIORNI DI SOSTA
152100021122     C     §GGA          SUB       §GGDA         §GGXX             3 0
152200021122     C                   ADD       1             §GGXX
152300021122     C                   ADD       §GGXX         §GGSOS
152400021122E   3C                   ENDIF
152500021122E   2C                   ENDIF
152600050309     C     K01GCP        READE     TIGCP51L                               98
152700021122E   1C                   ENDDO
152800021122     C*
152900021122     C                   Z-ADD     §GGSOS        $PGIGS                         *GIORNI DI SOSTA
153000021122     C*
153100021122     C                   ENDSR
153200970110     C*------------------------------------------------------------------------*
153300970110     C* REPASS - REPERISCE L'ASSICURAZIONE
153400970110     C*------------------------------------------------------------------------*
153500970110     C     REPASS        BEGSR
153600060315     c                   clear                   flgacbase         1
153700970110     C*
153800970110IF  1C     TASIAS        IFGT      *ZEROS
153900011011     C                   Z-ADD     TASIAS        $IAS
154000011011IF  2C     TASVAS        IFNE      PARDIV                                       *DIVISA ESTERA
154100011011     C                   Z-ADD     $IAS          WIMI             30 9
154200011011     C                   MOVEL     TASVAS        WDVI              3
154300011011     C                   EXSR      CNVEUR
154400011011     C                   Z-ADD     WIMO          $IAS
154500011011     C                   ENDIF
154600060315     c* verifico se esiste ac base
154700060315     c                   else
154800060315     c                   z-add     1             v
154900060315     c     'd'           lookup    svn(v)                                 99
155000060320     c                   if        van(v) <> 0 and *in99
155100060315     c                   eval      flgacbase = *on
155200970110E   1C                   ENDIF
155300060315E   1C                   ENDIF
155400970110     C*
155500970110     C                   ENDSR
155600970110     C*------------------------------------------------------------------------*
155700060315     C* REPVrf - REPERISCE le varie di fatturazione
155800990812     C* -----------------------------------------------------------------------*
155900060315     C     REPVrf        BEGSR
156000060315     C* CARICO LE VARIE BOLLA ORIGINALE DA TITAS
156100060315     C                   Z-ADD     *ZEROS        V                 6 0
156200060315     C     tasSV1        IFNE      *BLANKS
156300060315     C                   ADD       1             V
156400060315     C                   MOVEL     tasSV1        SVN(V)
156500060315     C                   Z-ADD     tasVA1        VAN(V)
156600060315     C                   ENDIF
156700060315     C     tasSV2        IFNE      *BLANKS
156800060315     C                   ADD       1             V
156900060315     C                   MOVEL     tasSV2        SVN(V)
157000060315     C                   Z-ADD     tasVA2        VAN(V)
157100060315     C                   ENDIF
157200060315     C     tasSV3        IFNE      *BLANKS
157300060315     C                   ADD       1             V
157400060315     C                   MOVEL     tasSV3        SVN(V)
157500060315     C                   Z-ADD     tasVA3        VAN(V)
157600060315     C                   ENDIF
157700060315     C* CARICO LE VARIE BOLLA DA TITA7 (SOLO SE C'E' LA VARIA 3)
157800060315     C     tasSV3        IFNE      *BLANKS
157900060315     C                   Z-ADD     tasAAS        TA7AAS
158000060315     C                   Z-ADD     tasLNP        TA7LNP
158100060315     C                   Z-ADD     tasNRS        TA7NRS
158200060315     C                   Z-ADD     tasNSP        TA7NSP
158300060315     C                   MOVEL     tasTBL        TA7TBL
158400060315     C     KEYTA7        SETLL     TITA730C
158500060315     c                   do        *hival
158600060315     C     KEYTA7        READE     TITA730C
158700060315     c                   if        %eof(tita730c)
158800060315     c                   leave
158900060315     c                   end
159000060315if  1C                   IF        (TA7VAN = 88888888,000)
159100060315     C                   Z-ADD     *zeros        TA7VAN
159200060315e   1c                   ENDIF
159300060315     C                   ADD       1             V
159400060315     C                   MOVEL     TA7SVN        SVN(V)
159500060315     C                   Z-ADD     TA7VAN        VAN(V)
159600060315     C                   ENDDO
159700060315e   1c                   ENDIF
159800060315     c                   endsr
159900060315     C*------------------------------------------------------------------------*
160000060315     C* REPVAR - REPERISCE LE consegne particolari
160100060315     C* -----------------------------------------------------------------------*
160200060315     C     REPVAR        BEGSR
160300970110     C*
160400970110     C* VARIE -BOLLA-
160500021122IF  1C     TASFTC        IFEQ      'A'
160600021122     C     TASTC2        OREQ      'A'
160700970110     C                   MOVEL     'S'           $PCA                           *CONS APPUNTAMENTO
160800970110E   1C                   ENDIF
160900021122IF  1C     TASFTC        IFEQ      'P'
161000021122     C     TASTC2        OREQ      'P'
161100970110     C                   MOVEL     'S'           $PCP                           *CONS PIANI
161200970110E   1C                   ENDIF
161300021122IF  1C     TASFTC        IFEQ      'S'
161400021122     C     TASTC2        OREQ      'S'
161500970110     C                   MOVEL     'S'           $PCS                           *CONS SUPERMERCATI
161600970110E   1C                   ENDIF
161700021122IF  1C     TASFTC        IFEQ      'F'
161800021122     C     TASTC2        OREQ      'F'
161900970110     C                   MOVEL     'S'           $PCF                           *CONS FUORI MISURA
162000970110E   1C                   ENDIF
162100040802IF  1C     TASFTC        IFEQ      'X'
162200040802     C     TASTC2        OREQ      'X'
162300040802     C                   MOVEL     'S'           $ded                           *CONS FUORI MISURA
162400040802E   1C                   ENDIF
162500021122IF  1C     TASLL1        IFEQ      'S'
162600970415     C                   MOVEL     'S'           $PCD                           *CONS DDT
162700970415E   1C                   ENDIF
162800970110     C*
162900970110     C                   ENDSR
163000980819     C*------------------------------------------------------------------------*
163100980819     C* AGGETX - AGGIORNA I DATI TIPO
163200980819     C*------------------------------------------------------------------------*
163300980819     C     AGGETX        BEGSR
163400980819     C*
163500980819     C* PER OGNI LINEA DI PARTENZA RICHIESTA
163600980819DO  1C     1             DO        500           I
163700980819IF  2C     SORG(I)       IFGT      *ZEROS
163800980819IF  3C     SPE(I)        IFGT      *ZEROS                                       *MEMORIZZATO QC.
163900980821     C                   Z-ADD     SORG(I)       XORG
164000140919     C                   movel     Sntw(I)       Xntw
164100980819     C     I             OCCUR     FPAG
164200980819     C     I             OCCUR     FCON
164300980819     C     I             OCCUR     FRIT
164400980819     C     I             OCCUR     FSGL
164500060829     C     I             OCCUR     FSGL1
164600980819     C     I             OCCUR     FCTS
164700980819     C     I             OCCUR     FTAR                                         *DATI TARIFFA
164800980819     C*---
164900980819     C* DATI TIPO -GENERALI-
165000980819     C*---
165100980821     C                   EXSR      SCRETC
165200980819     C*---
165300980819     C* DATI TIPO -SCAGLIONI-
165400980819     C*---
165500060829     c* ricavi
165600980821     C                   EXSR      SCRETS
165700060829     c* competenze
165800060829     c                   if        etcbnb <> 0
165900060829     c                   exsr      screts1
166000060829     c                   end
166100980819     C*---
166200980819     C* DATI TIPO -CODICI TASSAZIONE-
166300980819     C*---
166400980819     C* PER OGNI TIPO TARIFFA (ITALIA/ESTERO)
166500980819DO  4C     1             DO        2             K
166600980819IF  5C     SFIE(K)       IFNE      *BLANKS
166700980821     C                   MOVEL     SFIE(K)       XFIE
166800980821     C                   EXSR      SCRETT
166900981215E   5C                   ENDIF
167000980819E   4C                   ENDDO                                                  *FINE TARIFFA ITA/ES
167100980819     C*
167200980819E   3C                   ENDIF
167300980819E   2C                   ENDIF
167400980819E   1C                   ENDDO                                                  *FINE FILIALI
167500980819     C*
167600980821     C                   ENDSR
167700980819     C*------------------------------------------------------------------------*
167800980819     C* AGGET8 - AGGIORNA I DATI TIPO DI CARTELLO
167900980819     C*------------------------------------------------------------------------*
168000980819     C     AGGET8        BEGSR
168100970110     C*
168200980819     C* PER OGNI LINEA DI PARTENZA MEMORIZZATA
168300971124DO  1C     1             DO        500           I
168400980820     C     I             OCCUR     FPAG
168500970110     C     I             OCCUR     FCON
168600970110     C     I             OCCUR     FRIT
168700970110     C     I             OCCUR     FSGL
168800060829     C     I             OCCUR     FSGL1
168900970110     C     I             OCCUR     FCTS
169000980819     C     I             OCCUR     FTAR
169100980819     C*
169200980819     C* SOMMA I DATI PER LE TARIFFE DI CARTELLO -ITA/EST- DI TUTTE LE FILIALI
169300980819IF  2C     SFL1(I)       IFEQ      *BLANKS                                      *FILIALE ITALIA
169400980824     C                   Z-ADD     1             U                 4 0
169500980819X   2C                   ELSE                                                   *FILIALE ESTERO
169600980819     C                   Z-ADD     2             U
169700980819E   2C                   ENDIF
169800980824     C     U             OCCUR     CCAR
169900980824     C     U             OCCUR     CPAG
170000980824     C     U             OCCUR     CCON
170100980824     C     U             OCCUR     CRIT
170200980824     C     U             OCCUR     CSGL
170300060829     C     U             OCCUR     CSGL1
170400980824     C     U             OCCUR     CCTS
170500980819     C*
170600980819     C* DATI TIPO -GENERALI-
170700980821     C                   ADD       SPE(I)        USPE(1)
170800980821     C                   ADD       CLA(I)        UCLA(1)
170900980821     C                   ADD       CLB(I)        UCLB(1)
171000980821     C                   ADD       CLC(I)        UCLC(1)
171100980821     C                   ADD       BON(I)        UBON(1)
171200980821     C                   ADD       BOA(I)        UBOA(1)
171300980821     C                   ADD       BOB(I)        UBOB(1)
171400980821     C                   ADD       NCL(I)        UNCL(1)
171500980821     C                   ADD       PKB(I)        UPKB(1)
171600980821     C                   ADD       VLB(I)        UVLB(1)
171700060829     C                   ADD       NCL1(I)       UNCL1(1)
171800060829     C                   ADD       PKB1(I)       UPKB1(1)
171900060829     C                   ADD       VLB1(I)       UVLB1(1)
172000980821     C                   ADD       FRA(I)        UFRA(1)
172100980821     C                   ADD       ASS(I)        UASS(1)
172200980821     C                   ADD       PCV(I)        UPCV(1)
172300980821     C                   ADD       PCM(I)        UPCM(1)
172400980821     C                   ADD       PFD(I)        UPFD(1)
172500980821     C                   ADD       ESP(I)        UESP(1)
172600980821     C                   ADD       PPV(I)        UPPV(1)
172700980821     C                   ADD       PCA(I)        UPCA(1)
172800980821     C                   ADD       PCP(I)        UPCP(1)
172900980821     C                   ADD       PCS(I)        UPCS(1)
173000980821     C                   ADD       PCF(I)        UPCF(1)
173100980821     C                   ADD       PCD(I)        UPCD(1)
173200980821     C                   ADD       PCC(I)        UPCC(1)
173300980821     C                   ADD       PCL(I)        UPCL(1)
173400980821     C                   ADD       PAB(I)        UPAB(1)
173500980821     C                   ADD       PAM(I)        UPAM(1)
173600980821     C                   ADD       CAS(I)        UCAS(1)
173700980821     C                   ADD       PAR(I)        UPAR(1)
173800980821     C                   ADD       PAC(I)        UPAC(1)
173900060315     C                   ADD       xx1(I)        Uxx1(1)
174000980821     C                   ADD       ASC(I)        UASC(1)
174100980821     C                   ADD       PGI(I)        UPGI(1)
174200000414     C                   ADD       GGS(I)        UGGS(1)
174300000414     C                   ADD       NRG(I)        UNRG(1)
174400980901     C                   ADD       CLT(I)        UCLT(1)
174500011011     C                   ADD       CIS(I)        UCIS(1)
174600011011     C                   ADD       RIS(I)        URIS(1)
174700020215     C                   ADD       CLD(I)        UCLD(1)
174800050808     C                   ADD       pks(I)        Upks(1)
174900050808     C                   ADD       pkn(I)        Upkn(1)
175000040802     C                   ADD       ded(I)        Uded(1)
175100040811     C                   ADD       sbnb(I)       Usbnb(1)
175200040824     C                   ADD       sban(I)       Usban(1)
175300040811     C                   ADD       nbn(I)        Unbn(1)
175400040802     C                   ADD       nba(I)        Unba(1)
175500040802     C                   ADD       val(I)        Uval(1)
175600980825DO  2C     1             DO        30            J
175700980825IF  3C     GGP(J)        IFGT      *ZEROS
175800980825     C                   Z-ADD     1             A                 4 0
175900980825     C                   SETOFF                                           98
176000980825     C     GGP(J)        LOOKUP    UGGP(A)                                98
176100980825IF  4C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
176200980825     C                   ADD       SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
176300980825X   4C                   ELSE                                                   *MAI MEMORIZZATI
176400980825     C                   Z-ADD     1             A
176500980825DO  5C     A             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
176600980825IF  6C     UGGP(A)       IFEQ      *ZEROS                                       *DISPONIBILE
176700980825     C                   Z-ADD     GGP(J)        UGGP(A)                        *GG
176800980825     C                   Z-ADD     SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
176900980825     C                   Z-ADD     30            A                              *ESCE DAL CICLO
177000980825E   6C                   ENDIF
177100980825     C                   ADD       1             A
177200980825E   5C                   ENDDO
177300980825E   4C                   ENDIF
177400980825E   3C                   ENDIF
177500980825E   2C                   ENDDO
177600980819DO  2C     1             DO        999           J
177700980819     C                   ADD       RIT(J)        URIT(J)
177800980819     C                   ADD       CON(J)        UCON(J)
177900980819E   2C                   ENDDO
178000980819     C*
178100040920     C* DATI TIPO -SCAGLIONI- (già caricati nella durante la lettura di
178200040920     c* titas)
178300980820     C*
178400970110     C* DATI TIPO -CODICI TASSAZIONE-
178500050103DO  2C     1             DO        600           J
178600980819     C                   ADD       TSI(J)        UTSI(J)
178700980819     C                   ADD       TSE(J)        UTSE(J)
178800980819E   2C                   ENDDO
178900970110E   1C                   ENDDO                                                  *FINE FILIALI
179000980821     C*---
179100980821     C* SCRIVE LA TARIFFA -888- ITALIA
179200980821     C*---
179300980820     C*
179400980821     C* CARICA LE SCHIERE DA ELABORARE
179500980821     C                   Z-ADD     1             U
179600980821     C                   Z-ADD     1             I
179700980821     C                   Z-ADD     888           XORG
179800140919     c                   clear                   xntw
179900980821     C                   EXSR      CARSCH
180000980821     C*
180100980821     C* DATI TIPO -GENERALI-
180200980821     C                   EXSR      SCRETC
180300980821     C*
180400980821     C* DATI TIPO -SCAGLIONI-
180500060829     c* ricavi
180600980821     C                   EXSR      SCRETS
180700060829     c* competenze
180800060829     c                   if        etcbnb <> 0
180900060829     C                   EXSR      SCRETS1
181000060829     c                   end
181100980821     C*
181200980821     C* DATI TIPO -CODICI TASSAZIONE-
181300980824     C                   MOVEL     'I'           XFIE
181400980821     C                   EXSR      SCRETT
181500980824     C*---
181600980824     C* SCRIVE LA TARIFFA -889- ESTERO
181700980824     C*---
181800980824     C*
181900980824     C* CARICA LE SCHIERE DA ELABORARE
182000980824     C                   Z-ADD     2             U
182100980824     C                   Z-ADD     1             I
182200980824     C                   Z-ADD     889           XORG
182300140919     c                   clear                   xntw
182400980824     C                   EXSR      CARSCH
182500980824     C*
182600980824     C* DATI TIPO -GENERALI-
182700980824     C                   EXSR      SCRETC
182800980824     C*
182900980824     C* DATI TIPO -SCAGLIONI-
183000060829     c* ricavi
183100980824     C                   EXSR      SCRETS
183200060829     c* competenze
183300060829     c                   if        etcbnb <> 0
183400060829     C                   EXSR      SCRETS1
183500060829     c                   end
183600980824     C*
183700980824     C* DATI TIPO -CODICI TASSAZIONE-
183800980824     C                   MOVEL     'E'           XFIE
183900980824     C                   EXSR      SCRETT
184000970110     C*
184100980819     C                   ENDSR
184200040920     C*------------------------------------------------------------------------*
184300040920     C* SRSGL8 - CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
184400040920     C*------------------------------------------------------------------------*
184500040920     C     SRSGL8        BEGSR
184600040920     C* 888 ITALIA
184700040920     C                   IF        SFL1(F) = ' '
184800040920     C                   Z-ADD     1             U
184900040920     C                   ELSE
185000040920     C                   Z-ADD     2             U
185100040920     C                   END
185200040920     C     U             OCCUR     CSGL
185300040920     C*
185400040920DO  5C     1             DO        10            I
185500040920IF  6C     TASSGL        IFLE      SGL(I)
185600040920     C                   ADD       1             USSP(I)                        *N°SPED SCAGLIONE
185700040920     C                   ADD       TASPKF        UPSC(I)                        *PESO
185800040920     C                   ADD       TASNCL        UCSC(I)                        *COLLI
185900040920     C                   ADD       $VOL          UVSC(I)                        *VOLUME
186000040920     C                   LEAVE
186100040920E   6C                   ENDIF
186200040920E   5C                   ENDDO
186300040920     C*
186400040920     C                   ENDSR
186500060829     C*------------------------------------------------------------------------*
186600060829     C* SRSGL81- CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
186700060829     C*------------------------------------------------------------------------*
186800060829     C     SRSGL81       BEGSR
186900060829     C* 888 ITALIA
187000060829     C                   IF        SFL1(F) = ' '
187100060829     C                   Z-ADD     1             U
187200060829     C                   ELSE
187300060829     C                   Z-ADD     2             U
187400060829     C                   END
187500060829     C     U             OCCUR     CSGL1
187600060829     C*
187700060829DO  5C     1             DO        10            I
187800060829IF  6C     TASSGL        IFLE      SGL(I)
187900060829     C                   ADD       1             USSP1(I)                       *N°SPED SCAGLIONE
188000060829     C                   ADD       TASPKF        UPSC1(I)                       *PESO
188100060829     C                   ADD       TASNCL        UCSC1(I)                       *COLLI
188200060829     C                   ADD       $VOL1         UVSC1(I)                       *VOLUME
188300060829     C                   LEAVE
188400060829E   6C                   ENDIF
188500060829E   5C                   ENDDO
188600060829     C*
188700060829     C                   ENDSR
188800980821     C*------------------------------------------------------------------------*
188900980821     C* CARSCH - CARICA LE SCHIERE DA ELABORARE
189000980821     C*------------------------------------------------------------------------*
189100980821     C     CARSCH        BEGSR
189200980821     C*
189300980821     C* SI POSIZIONA SULLE SCHIERE DELLA TARIFFA
189400980824     C     1             OCCUR     FPAG
189500980824     C     1             OCCUR     FCON
189600980824     C     1             OCCUR     FRIT
189700980824     C     1             OCCUR     FSGL
189800060829     C     1             OCCUR     FSGL1
189900980824     C     1             OCCUR     FCTS
190000980824     C     1             OCCUR     FTAR
190100980824     C     U             OCCUR     CCAR
190200980824     C     U             OCCUR     CPAG
190300980824     C     U             OCCUR     CCON
190400980824     C     U             OCCUR     CRIT
190500980824     C     U             OCCUR     CSGL
190600060829     C     U             OCCUR     CSGL1
190700980824     C     U             OCCUR     CCTS
190800980821     C*
190900980821     C* DATI TIPO -GENERALI-
191000980824     C                   Z-ADD     USPE(1)       SPE(1)
191100980824     C                   Z-ADD     UCLA(1)       CLA(1)
191200980824     C                   Z-ADD     UCLB(1)       CLB(1)
191300980824     C                   Z-ADD     UCLC(1)       CLC(1)
191400980824     C                   Z-ADD     UBON(1)       BON(1)
191500980824     C                   Z-ADD     UBOA(1)       BOA(1)
191600980824     C                   Z-ADD     UBOB(1)       BOB(1)
191700980824     C                   Z-ADD     UNCL(1)       NCL(1)
191800980824     C                   Z-ADD     UPKB(1)       PKB(1)
191900980824     C                   Z-ADD     UVLB(1)       VLB(1)
192000060829     C                   Z-ADD     UNCL1(1)      NCL1(1)
192100060829     C                   Z-ADD     UPKB1(1)      PKB1(1)
192200060829     C                   Z-ADD     UVLB1(1)      VLB1(1)
192300980824     C                   Z-ADD     UFRA(1)       FRA(1)
192400980824     C                   Z-ADD     UASS(1)       ASS(1)
192500980824     C                   Z-ADD     UPCV(1)       PCV(1)
192600980824     C                   Z-ADD     UPCM(1)       PCM(1)
192700980824     C                   Z-ADD     UPFD(1)       PFD(1)
192800980824     C                   Z-ADD     UESP(1)       ESP(1)
192900980824     C                   Z-ADD     UPPV(1)       PPV(1)
193000980824     C                   Z-ADD     UPCA(1)       PCA(1)
193100980824     C                   Z-ADD     UPCP(1)       PCP(1)
193200980824     C                   Z-ADD     UPCS(1)       PCS(1)
193300980824     C                   Z-ADD     UPCF(1)       PCF(1)
193400980824     C                   Z-ADD     UPCD(1)       PCD(1)
193500980824     C                   Z-ADD     UPCC(1)       PCC(1)
193600980824     C                   Z-ADD     UPCL(1)       PCL(1)
193700980824     C                   Z-ADD     UPAB(1)       PAB(1)
193800980824     C                   Z-ADD     UPAM(1)       PAM(1)
193900980824     C                   Z-ADD     UCAS(1)       CAS(1)
194000980824     C                   Z-ADD     UPAR(1)       PAR(1)
194100980824     C                   Z-ADD     UPAC(1)       PAC(1)
194200060315     C                   Z-ADD     Uxx1(1)       xx1(1)
194300980824     C                   Z-ADD     UASC(1)       ASC(1)
194400980824     C                   Z-ADD     UPGI(1)       PGI(1)
194500000414     C                   Z-ADD     UGGS(1)       GGS(1)
194600000414     C                   Z-ADD     UNRG(1)       NRG(1)
194700980901     C                   Z-ADD     UCLT(1)       CLT(1)
194800011011     C                   Z-ADD     UCIS(1)       CIS(1)
194900011011     C                   Z-ADD     URIS(1)       RIS(1)
195000020215     C                   Z-ADD     UCLD(1)       CLD(1)
195100050808     C                   Z-ADD     Upks(1)       pks(1)
195200050808     C                   Z-ADD     Upkn(1)       pkn(1)
195300040811     C                   Z-ADD     Usbnb(1)      sbnb(1)
195400040824     C                   Z-ADD     Usban(1)      sban(1)
195500040811     C                   Z-ADD     Unbn(1)       nbn(1)
195600040802     C                   Z-ADD     Unba(1)       nba(1)
195700040802     C                   Z-ADD     Uded(1)       ded(1)
195800040802     C                   Z-ADD     Uval(1)       val(1)
195900980821DO  1C     1             DO        30            J
196000980821     C                   Z-ADD     USGP(J)       SGP(J)
196100980821     C                   Z-ADD     UGGP(J)       GGP(J)
196200980821E   1C                   ENDDO
196300980821DO  1C     1             DO        999           J
196400980821     C                   Z-ADD     URIT(J)       RIT(J)
196500980821     C                   Z-ADD     UCON(J)       CON(J)
196600980821E   1C                   ENDDO
196700980821     C*
196800060829     C* DATI TIPO -SCAGLIONI- ricavi
196900980824DO  1C     1             DO        10            J
197000980825     C                   Z-ADD     USSP(J)       SSP(J)
197100980825     C                   Z-ADD     UPSC(J)       PSC(J)
197200980825     C                   Z-ADD     UCSC(J)       CSC(J)
197300980825     C                   Z-ADD     UVSC(J)       VSC(J)
197400980824E   1C                   ENDDO
197500060829     C* DATI TIPO -SCAGLIONI- competenze
197600060829DO  1C     1             DO        10            J
197700060829     C                   Z-ADD     USSP1(J)      SSP1(J)
197800060829     C                   Z-ADD     UPSC1(J)      PSC1(J)
197900060829     C                   Z-ADD     UCSC1(J)      CSC1(J)
198000060829     C                   Z-ADD     UVSC1(J)      VSC1(J)
198100060829E   1C                   ENDDO
198200980821     C*
198300980821     C* DATI TIPO -CODICI TASSAZIONE-
198400050103DO  1C     1             DO        600           J                 4 0
198500980825     C                   Z-ADD     UTSI(J)       TSI(J)
198600980825     C                   Z-ADD     UTSE(J)       TSE(J)
198700980824E   1C                   ENDDO
198800980821     C*
198900980821     C                   ENDSR
199000980821     C*------------------------------------------------------------------------*
199100980821     C* AZZSCH - AZZERA LE VARIABILI DI LAVORO
199200980821     C*------------------------------------------------------------------------*
199300980821     C     AZZSCH        BEGSR
199400980821     C*
199500980821     C                   CLEAR                   SPE                            -GENERALI-
199600980821     C                   CLEAR                   CLA
199700980821     C                   CLEAR                   CLB
199800980821     C                   CLEAR                   CLC
199900980821     C                   CLEAR                   BON
200000980821     C                   CLEAR                   BOA
200100980821     C                   CLEAR                   BOB
200200980821     C                   CLEAR                   NCL
200300980821     C                   CLEAR                   PKB
200400980821     C                   CLEAR                   VLB
200500060829     C                   CLEAR                   NCL1
200600060829     C                   CLEAR                   PKB1
200700060829     C                   CLEAR                   VLB1
200800980821     C                   CLEAR                   FRA
200900980821     C                   CLEAR                   ASS
201000980821     C                   CLEAR                   PCV
201100980821     C                   CLEAR                   PCM
201200980821     C                   CLEAR                   PFD
201300980821     C                   CLEAR                   ESP
201400980821     C                   CLEAR                   PPV
201500980821     C                   CLEAR                   PCA
201600980821     C                   CLEAR                   PCP
201700980821     C                   CLEAR                   PCS
201800980821     C                   CLEAR                   PCF
201900980821     C                   CLEAR                   PCD
202000980821     C                   CLEAR                   PCC
202100980821     C                   CLEAR                   PCL
202200980821     C                   CLEAR                   PAB
202300980821     C                   CLEAR                   PAM
202400980821     C                   CLEAR                   CAS
202500980821     C                   CLEAR                   PAR
202600980821     C                   CLEAR                   PAC
202700060315     C                   CLEAR                   xx1
202800980821     C                   CLEAR                   ASC
202900980821     C                   CLEAR                   PGI
203000980821     C                   CLEAR                   SGP
203100980821     C                   CLEAR                   GGP
203200980821     C                   CLEAR                   RIT
203300980821     C                   CLEAR                   CON
203400980901     C                   CLEAR                   CLT
203500000414     C                   CLEAR                   GGS
203600000414     C                   CLEAR                   NRG
203700980821     C                   CLEAR                   SSP                            -SCAGLIONI-
203800980821     C                   CLEAR                   PSC
203900980821     C                   CLEAR                   CSC
204000980821     C                   CLEAR                   VSC
204100060829     C                   CLEAR                   SSP1                           -SCAGLIONI-
204200060829     C                   CLEAR                   PSC1
204300060829     C                   CLEAR                   CSC1
204400060829     C                   CLEAR                   VSC1
204500980821     C                   CLEAR                   TSI                            -CODICI TASSAZIONE-
204600980821     C                   CLEAR                   TSE
204700011011     C                   CLEAR                   CIS
204800011011     C                   CLEAR                   RIS
204900020215     C                   CLEAR                   CLD
205000050808     C                   CLEAR                   pks
205100050808     C                   CLEAR                   pkn
205200040802     C                   CLEAR                   nba
205300040824     C                   CLEAR                   sban
205400040824     C                   CLEAR                   sbnb
205500040811     C                   CLEAR                   nbn
205600040802     C                   CLEAR                   ded
205700040802     C                   CLEAR                   val
205800980821     C*
205900980821     C                   ENDSR
206000980821     C*------------------------------------------------------------------------*
206100011001     C* SCRETC - SCRIVE UN RECORD DI TEETC -GENERALI-
206200980821     C*------------------------------------------------------------------------*
206300980821     C     SCRETC        BEGSR
206400980821     C*
206500060804     C                   MOVEL     'S'           KTCDSF
206600980821     C                   MOVEL     PARCTO        KTCCTO
206700980821     C                   Z-ADD     PARKOV        KTCKSC
206800980821     C                   Z-ADD     PARCTR        KTCCTR
206900980821     C                   Z-ADD     PARPRG        KTCPRG
207000980821     C                   Z-ADD     XORG          KTCLNP
207100021122     C     KEYETC        CHAIN     TEETC01L                           9697
207200980821     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
207300021122IF  2C     *IN97         IFEQ      *OFF                                         *NON ALLOCATO
207400021122IF  3C     *IN96         IFEQ      *ON
207500980821     C                   MOVEL     KTCDSF        ETCDSF
207600980821     C                   MOVEL     KTCCTO        ETCCTO
207700980821     C                   Z-ADD     KTCKSC        ETCKSC
207800980821     C                   Z-ADD     KTCCTR        ETCCTR
207900980821     C                   Z-ADD     KTCPRG        ETCPRG
208000980821     C                   Z-ADD     KTCLNP        ETCLNP
208100980821     C                   Z-ADD     DATCOR        ETCDIR
208200980821     C                   Z-ADD     *ZEROS        ETCDUV
208300021122     C                   MOVEL     *BLANKS       ETCATB
208400011001     C                   WRITE     TEETC000                                     *INSERIMENTO
208500011001     C                   UNLOCK    TEETC01L
208600980821X   3C                   ELSE
208700980821     C                   Z-ADD     DATCOR        ETCDUV
208800021122     C                   MOVEL     *BLANKS       ETCATB
208900011001     C                   UPDATE    TEETC000                                     *VARIAZIONE
209000980821E   3C                   ENDIF
209100980821X   2C                   ELSE                                                   *ALLOCATO
209200980821     C                   MOVEL     METX(1)       DEPERR
209300980821     C                   MOVEL     'X'           DEPTER
209400980821     C                   EXSR      STAERR
209500980821E   2C                   ENDIF
209600980821     C*
209700980821     C                   ENDSR
209800980821     C*------------------------------------------------------------------------*
209900011001     C* SCRETS - SCRIVE UN RECORD DI TEETS -SCAGLIONI-
210000980821     C*------------------------------------------------------------------------*
210100980821     C     SCRETS        BEGSR
210200980821     C*
210300980821     C                   EXSR      SCRSGL                                       *SRIVE SCHIERE SGL
210400980821DO  1C     1             DO        10            J
210500980821IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
210600060804     C                   MOVEL     'S'           KTSDSF
210700980821     C                   MOVEL     PARCTO        KTSCTO
210800980821     C                   Z-ADD     PARKOV        KTSKSC
210900980821     C                   Z-ADD     PARCTR        KTSCTR
211000980821     C                   Z-ADD     PARPRG        KTSPRG
211100980821     C                   Z-ADD     XORG          KTSLNP
211200980821     C                   Z-ADD     §SG(J)        KTSSGL
211300021122     C     KEYETS        CHAIN     TEETS01L                           96
211400980821     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
211500021122IF  4C     *IN96         IFEQ      *ON
211600980821     C                   MOVEL     KTSDSF        ETSDSF
211700980821     C                   MOVEL     KTSCTO        ETSCTO
211800980821     C                   Z-ADD     KTSKSC        ETSKSC
211900980821     C                   Z-ADD     KTSCTR        ETSCTR
212000980821     C                   Z-ADD     KTSPRG        ETSPRG
212100980821     C                   Z-ADD     KTSLNP        ETSLNP
212200980821     C                   Z-ADD     KTSSGL        ETSSGL
212300980821     C                   Z-ADD     DATCOR        ETSDIR
212400980821     C                   Z-ADD     *ZEROS        ETSDUV
212500021122     C                   MOVEL     *BLANKS       ETSATB
212600011001     C                   WRITE     TEETS000                                     *INSERIMENTO
212700011001     C                   UNLOCK    TEETS01L
212800980821X   4C                   ELSE
212900980821     C                   Z-ADD     DATCOR        ETSDUV
213000021122     C                   MOVEL     *BLANKS       ETSATB
213100011001     C                   UPDATE    TEETS000                                     *VARIAZIONE
213200980821E   4C                   ENDIF
213300980821E   2C                   ENDIF
213400980821E   1C                   ENDDO                                                  *FINE SCAGLIONI
213500980821     C*
213600980821     C                   ENDSR
213700060829     C*------------------------------------------------------------------------*
213800060829     C* SCRETS1- SCRIVE UN RECORD DI TEETS10F -SCAGLIONI-
213900060829     C*------------------------------------------------------------------------*
214000060829     C     SCRETS1       BEGSR
214100060829     C*
214200060829     C                   EXSR      SCRSGL1                                      *SRIVE SCHIERE SGL
214300060829DO  1C     1             DO        10            J
214400060829IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
214500060829     C                   MOVEL     'S'           KTSDSF
214600060829     C                   MOVEL     PARCTO        KTSCTO
214700060829     C                   Z-ADD     PARKOV        KTSKSC
214800060829     C                   Z-ADD     PARCTR        KTSCTR
214900060829     C                   Z-ADD     PARPRG        KTSPRG
215000060829     C                   Z-ADD     XORG          KTSLNP
215100060829     C                   Z-ADD     §SG(J)        KTSSGL
215200060829     C     KEYETS        CHAIN     TEETS11L                           96
215300060829     C                   EXSR      IMPETS1                                      *IMPOSTA I CAMPI
215400060829IF  4C     *IN96         IFEQ      *ON
215500060829     C                   MOVEL     KTSDSF        £ETSDSF
215600060829     C                   MOVEL     KTSCTO        £ETSCTO
215700060829     C                   Z-ADD     KTSKSC        £ETSKSC
215800060829     C                   Z-ADD     KTSCTR        £ETSCTR
215900060829     C                   Z-ADD     KTSPRG        £ETSPRG
216000060829     C                   Z-ADD     KTSLNP        £ETSLNP
216100060829     C                   Z-ADD     KTSSGL        £ETSSGL
216200060829     C                   Z-ADD     DATCOR        £ETSDIR
216300060829     C                   Z-ADD     *ZEROS        £ETSDUV
216400060829     C                   MOVEL     *BLANKS       £ETSATB
216500060829     C                   WRITE     TEETS100                                     *INSERIMENTO
216600060829     C                   UNLOCK    TEETS11L
216700060829X   4C                   ELSE
216800060829     C                   Z-ADD     DATCOR        £ETSDUV
216900060829     C                   MOVEL     *BLANKS       £ETSATB
217000060829     C                   UPDATE    TEETS100                                     *VARIAZIONE
217100060829E   4C                   ENDIF
217200060829E   2C                   ENDIF
217300060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
217400060829     C*
217500060829     C                   ENDSR
217600980821     C*------------------------------------------------------------------------*
217700011001     C* SCRETT - SCRIVE UN RECORD DI TEETT -CODICI TASSAZIONE-
217800980821     C*------------------------------------------------------------------------*
217900980821     C     SCRETT        BEGSR
218000980821     C*
218100140924     c                   clear                   totinc            8 5
218200980821     C                   EXSR      SCRCTS                                       *SRIVE SCHIERE CTS
218300050103DO  1C     1             DO        600           J                 4 0
218400980821IF  2C     §CT(J)        IFNE      *BLANKS                                      *CODICE ESISTENTE
218500060804     C                   MOVEL     'S'           KTTDSF
218600980821     C                   MOVEL     PARCTO        KTTCTO
218700980821     C                   Z-ADD     PARKOV        KTTKSC
218800980821     C                   Z-ADD     PARCTR        KTTCTR
218900980821     C                   Z-ADD     PARPRG        KTTPRG
219000980821     C                   Z-ADD     XORG          KTTLNP
219100140922     C                   MOVEL     Xntw          KTTntw
219200140922     C                   MOVEL     XFIE          KTTFIE
219300980821     C                   MOVEL     §CT(J)        KTTCTS
219400021122     C     KEYETT        CHAIN     TEETT01L                           96
219500980821     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
219600021122IF  4C     *IN96         IFEQ      *ON
219700980821     C                   MOVEL     KTTDSF        ETTDSF
219800980821     C                   MOVEL     KTTCTO        ETTCTO
219900980821     C                   Z-ADD     KTTKSC        ETTKSC
220000980821     C                   Z-ADD     KTTCTR        ETTCTR
220100980821     C                   Z-ADD     KTTPRG        ETTPRG
220200980821     C                   Z-ADD     KTTLNP        ETTLNP
220300980821     C                   MOVEL     KTTFIE        ETTFIE
220400980821     C                   MOVEL     KTTCTS        ETTCTS
220500980821     C                   Z-ADD     DATCOR        ETTDIR
220600980821     C                   Z-ADD     *ZEROS        ETTDUV
220700021122     C                   MOVEL     *BLANKS       ETTATB
220800011001     C                   WRITE     TEETT000                                     *INSERIMENTO
220900011001     C                   UNLOCK    TEETT01L
221000980821X   4C                   ELSE
221100980821     C                   Z-ADD     DATCOR        ETTDUV
221200021122     C                   MOVEL     *BLANKS       ETTATB
221300011001     C                   UPDATE    TEETT000                                     *VARIAZIONE
221400980821E   4C                   ENDIF
221500140923     c* calcolo le % per codice tassazione e cap
221600140923     c                   if        ettfie ='E' and
221700140923     c                             ettlnp <> 889
221800140923     c                   if        ettftr = ' '
221900140923     c                   exsr      impett10
222000140923     c                   else
222100140923     c* scrivo il codice tassazione con cap a blanks se non sono
222200140923     c* presenti spedizioni
222300140923     c                   clear                   kttnaz
222400140923     c                   clear                   kttcad
222500140923     c                   exsr      scrett10
222600140923     c                   end
222700140923     c                   end
222800140923     c* calcolo il totale delle linee di partenza per la cartello
222900140922     c                   if        ettlnp = 889
223000140922     c                   exsr      scrett10t
223100140922     c                   end
223200980821E   2C                   ENDIF
223300980821E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
223400140924     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
223500140926     c                   if        ettfie ='E' and
223600140926     c                             ettlnp <>889
223700140924     C                   EXSR      PARPCT1
223800140924     c                   end
223900980821     C*
224000980821     C                   ENDSR
224100140919     C*------------------------------------------------------------------------*
224200140919     C* SCRETT10 - SCRIVE TEETT10 -CODICI TASSAZIONE- cap
224300140919     C*------------------------------------------------------------------------*
224400140919     C     SCRETT10      BEGSR
224500140923     c                   clear                   teett10ds
224600140919     C                   MOVEL     'S'           KTTDSF
224700140919     C                   MOVEL     PARCTO        KTTCTO
224800140919     C                   Z-ADD     PARKOV        KTTKSC
224900140919     C                   Z-ADD     PARCTR        KTTCTR
225000140919     C                   Z-ADD     PARPRG        KTTPRG
225100140923     C                   Z-ADD     xorg          KTTLNP
225200140922     c                   movel     xntw          kttntw
225300140919     C                   MOVEL     'E'           KTTFIE
225400140919     C     KEYETT1       CHAIN     TEETT11L                           96
225500140919IF  4C     *IN96         IFEQ      *ON
225600140922     C                   MOVEL     KTTDSF        £ETTDSF
225700140922     C                   MOVEL     KTTCTO        £ETTCTO
225800140922     C                   Z-ADD     KTTKSC        £ETTKSC
225900140922     C                   Z-ADD     KTTCTR        £ETTCTR
226000140922     C                   Z-ADD     KTTPRG        £ETTPRG
226100140922     C                   Z-ADD     KTTLNP        £ETTLNP
226200140922     C                   MOVEL     KTTFIE        £ETTFIE
226300140922     C                   MOVEL     KTTCTS        £ETTCTS
226400140922     C                   MOVEL     KTTnaz        £ETTnaz
226500140922     C                   MOVEL     KTTcad        £ETTcad
226600140922     C                   MOVEL     KTTntw        £ETTntw
226700140922     C                   Z-ADD     DATCOR        £ETTDIR
226800140922     C                   Z-ADD     1             £ETTnsp
226900140922     C                   Z-ADD     *ZEROS        £ETTDUV
227000140922     C                   MOVEL     *BLANKS       £ETTATB
227100140923     c* se sto scrivendo il codice tassazione senza il cap
227200140923     c* imposto i dati del TEETT00F appena scritto
227300140923     c                   if        kttcad = ' '
227400140923     c                   eval      £ettrct = ettrct
227500140923     c                   eval      £ettppe = ettppe
227600140923     c                   eval      £ettinc = ettppe
227700140923     c                   eval      £ettftr = ettftr
227800140923     C                   Z-ADD     0             £ETTnsp
227900140923     c                   end
228000140919     C                   WRITE     TEETT100                                     *INSERIMENTO
228100140919     C                   UNLOCK    TEETT11L
228200140919X   4C                   ELSE
228300140922     c                   if        £ettatb = ' '
228400140922     C                   add       1             £ETTnsp
228500140922     c                   else
228600140922     c                   z-add     1             £ettnsp
228700140922     c                   end
228800140923     c                   if        kttcad = ' '
228900140923     c                   eval      £ettrct = ettrct
229000140923     c                   eval      £ettppe = ettppe
229100140923     c                   eval      £ettinc = ettppe
229200140923     c                   eval      £ettftr = ettftr
229300140923     C                   Z-ADD     0             £ETTnsp
229400140923     c                   end
229500140922     C                   Z-ADD     DATCOR        £ETTDUV
229600140922     C                   MOVEL     *BLANKS       £ETTATB
229700140919     C                   UPDATE    TEETT100                                     *VARIAZIONE
229800140919E   4C                   ENDIF
229900140925     c                   add       £ettinc       totinc
230000140919     C*
230100140919     C                   ENDSR
230200140919     C*------------------------------------------------------------------------*
230300140919     C* IMPETC - IMPOSTA I CAMPI DATI TIPO -GENERALI-
230400140919     C*------------------------------------------------------------------------*
230500140919     C     IMPETC        BEGSR
230600970110     C*---
230700970110     C* CAMPI CALCOLATI
230800970110     C*---
230900970110     C                   Z-ADD     *ZEROS        WMAX              9 0
231000980901     C                   MOVEL     'C'           ETCCLV
231100020215IF  1C     CLD(I)        IFGT      WMAX
231200020215     C                   Z-ADD     CLD(I)        WMAX
231300020215     C                   MOVEL     'D'           ETCCLV
231400020215E   1C                   ENDIF
231500980901IF  1C     CLT(I)        IFGT      WMAX
231600980901     C                   Z-ADD     CLT(I)        WMAX
231700020215     C                   MOVEL     'T'           ETCCLV
231800980901E   1C                   ENDIF
231900970110IF  1C     CLA(I)        IFGT      WMAX
232000970110     C                   Z-ADD     CLA(I)        WMAX
232100970110     C                   MOVEL     'A'           ETCCLV
232200970110E   1C                   ENDIF
232300970110IF  1C     CLB(I)        IFGT      WMAX
232400970110     C                   Z-ADD     CLB(I)        WMAX
232500970110     C                   MOVEL     'B'           ETCCLV
232600970110E   1C                   ENDIF
232700970110IF  1C     CLC(I)        IFGT      WMAX
232800970110     C                   Z-ADD     CLC(I)        WMAX
232900970110     C                   MOVEL     'C'           ETCCLV
233000970110E   1C                   ENDIF
233100050808     c* packing list
233200050808     C                   Z-ADD     *ZEROS        Wmax
233300050808     C                   MOVEL     'N'           ETCpkl
233400050808IF  1C     pks(I)        IFGT      wmax
233500050808     C                   Z-ADD     pks(I)        Wmax
233600050808     C                   MOVEL     'S'           ETCpkl
233700050808E   1C                   ENDIF
233800050808IF  1C     pkn(I)        IFGT      wmax
233900050808     C                   Z-ADD     pkn(I)        Wmax
234000050808     C                   MOVEL     'N'           ETCpkl
234100050808E   1C                   ENDIF
234200970110     C*
234300970110     C                   Z-ADD     *ZEROS        WMAX              9 0
234400970110     C                   MOVEL     ' '           ETCBOL
234500970110IF  1C     BON(I)        IFGT      WMAX
234600970110     C                   Z-ADD     BON(I)        WMAX
234700970110     C                   MOVEL     ' '           ETCBOL
234800970110E   1C                   ENDIF
234900970415IF  1C     BOA(I)        IFGT      WMAX                                         *"A" VINCE SU " "
235000970415     C     BOA(I)        ORGT      *ZEROS
235100970110     C                   Z-ADD     BOA(I)        WMAX
235200970110     C                   MOVEL     'A'           ETCBOL
235300970110E   1C                   ENDIF
235400970415IF  1C     BOB(I)        IFGT      WMAX                                         *"B" VINCE SU "A"
235500970415     C     BOB(I)        ORGT      *ZEROS
235600970110     C                   Z-ADD     BOB(I)        WMAX
235700970110     C                   MOVEL     'B'           ETCBOL
235800970110E   1C                   ENDIF
235900060829     C* ricavi
236000970110IF  1C     NCL(I)        IFGT      *ZEROS
236100970110     C     PKB(I)        DIV       NCL(I)        ETCPMC
236200970110     C     VLB(I)        DIV       NCL(I)        ETCVMC
236300970703E   1C                   ENDIF
236400970703IF  1C     ETCPMC        IFEQ      *ZEROS
236500970110     C                   Z-ADD     0,1           ETCPMC
236600970703E   1C                   ENDIF
236700970703IF  1C     ETCVMC        IFEQ      *ZEROS
236800970110     C                   Z-ADD     0,001         ETCVMC
236900970703E   1C                   ENDIF
237000060829     c* competenze
237100060829IF  1C     NCL1(I)       IFGT      *ZEROS
237200060829     C     PKB1(I)       DIV       NCL1(I)       ETCPMC1
237300060829     C     VLB1(I)       DIV       NCL1(I)       ETCVMC1
237400060829E   1C                   ENDIF
237500060829IF  1C     ETCPMC1       IFEQ      *ZEROS
237600060829     C                   Z-ADD     0,1           ETCPMC1
237700060829E   1C                   ENDIF
237800060829IF  1C     ETCVMC1       IFEQ      *ZEROS
237900060829     C                   Z-ADD     0,001         ETCVMC1
238000060829E   1C                   ENDIF
238100970110     C*
238200011026     C     FRA(I)        MULT      100           WN153            21 8
238300011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
238400970110     C                   Z-ADD     WPER          ETCFRA
238500011001     C     ASS(I)        MULT      100           WN153
238600011001     C     WN153         DIV(H)    SPE(I)        WPER
238700970110     C                   Z-ADD     WPER          ETCASS
238800980803IF  1C     ETCFRA        IFGE      ETCASS
238900980803     C                   Z-ADD     *ZEROS        ETCASS
239000980831     C                   Z-ADD     100           ETCFRA
239100980803X   1C                   ELSE
239200980803     C                   Z-ADD     *ZEROS        ETCFRA
239300980831     C                   Z-ADD     100           ETCASS
239400980803E   1C                   ENDIF
239500970110     C*
239600011001     C     PCV(I)        MULT      100           WN153
239700011001     C     WN153         DIV(H)    SPE(I)        WPER
239800970110     C                   Z-ADD     WPER          ETCPCV
239900040802     C*
240000011001     C     PCM(I)        MULT      100           WN153
240100011001     C     WN153         DIV(H)    SPE(I)        WPER
240200970110     C                   Z-ADD     WPER          ETCPCM
240300970110     C*
240400011001     C     PFD(I)        MULT      100           WN153
240500011001     C     WN153         DIV(H)    SPE(I)        WPER
240600970110     C                   Z-ADD     WPER          ETCPFD
240700970110     C*
240800011001     C     ESP(I)        MULT      100           WN153
240900011001     C     WN153         DIV(H)    SPE(I)        WPER
241000970110     C                   Z-ADD     WPER          ETCESP
241100970110     C*
241200011001     C     PPV(I)        MULT      100           WN153
241300011001     C     WN153         DIV(H)    SPE(I)        WPER
241400970110     C                   Z-ADD     WPER          ETCPPV
241500970110     C*
241600011001     C     PCA(I)        MULT      100           WN153
241700011001     C     WN153         DIV(H)    SPE(I)        WPER
241800970110     C                   Z-ADD     WPER          ETCPCA
241900970110     C*
242000011001     C     PCP(I)        MULT      100           WN153
242100011001     C     WN153         DIV(H)    SPE(I)        WPER
242200970110     C                   Z-ADD     WPER          ETCPCP
242300970110     C*
242400011001     C     PCS(I)        MULT      100           WN153
242500011001     C     WN153         DIV(H)    SPE(I)        WPER
242600970110     C                   Z-ADD     WPER          ETCPCS
242700970110     C*
242800011001     C     PCF(I)        MULT      100           WN153
242900011001     C     WN153         DIV(H)    SPE(I)        WPER
243000970110     C                   Z-ADD     WPER          ETCPCF
243100970415     C*
243200011001     C     PCD(I)        MULT      100           WN153
243300011001     C     WN153         DIV(H)    SPE(I)        WPER
243400970415     C                   Z-ADD     WPER          ETCPCD
243500970415     C*
243600011001     C     PCC(I)        MULT      100           WN153
243700011001     C     WN153         DIV(H)    SPE(I)        WPER
243800970415     C                   Z-ADD     WPER          ETCPCC
243900970415     C*
244000011001     C     PCL(I)        MULT      100           WN153
244100011001     C     WN153         DIV(H)    SPE(I)        WPER
244200970415     C                   Z-ADD     WPER          ETCPCL
244300970110     C*
244400011001     C     PAB(I)        MULT      100           WN153
244500011001     C     WN153         DIV(H)    SPE(I)        WPER
244600970110     C                   Z-ADD     WPER          ETCPAB
244700970110     C*
244800011001     C     PAM(I)        MULT      100           WN153
244900011001     C     WN153         DIV(H)    SPE(I)        WPER
245000970110     C                   Z-ADD     WPER          ETCPAM
245100011011     C*
245200011011     C     CIS(I)        MULT      100           WN153
245300011011     C     WN153         DIV(H)    SPE(I)        WPER
245400011011     C                   Z-ADD     WPER          ETCCIS
245500011011     C*
245600011011     C     RIS(I)        MULT      100           WN153
245700011011     C     WN153         DIV(H)    SPE(I)        WPER
245800011011     C                   Z-ADD     WPER          ETCSAM
245900970110     C*
246000011001     C     PAB(I)        ADD       PAM(I)        WN153
246100970110     C                   Z-ADD     *ZEROS        ETCCAS
246200011001IF  1C     WN153         IFGT      *ZEROS
246300011001     C     CAS(I)        DIV       WN153         ETCCAS
246400970110E   1C                   ENDIF
246500011026     C                   Z-ADD     ETCCAS        $IMP             21 8
246600970417     C                   MOVEL     'E'           $FDE              1
246700011011     C                   IF        PARDIV = 'ITL'
246800011026     C                   Z-ADD     1000          $ARR             21 8
246900011011     C                   ELSE
247000011026     C                   Z-ADD     1             $ARR             21 8
247100011011     C                   ENDIF
247200970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
247300970110     C                   Z-ADD     $IMP          ETCCAS
247400970110     C*
247500970418     C     PAR(I)        ADD       PAC(I)        WN90              9 0
247600060315     C                   ADD       xx1(I)        WN90
247700011001     C     PAR(I)        MULT      100           WN153
247800011001     C     WN153         DIV(H)    WN90          WPER
247900970110     C                   Z-ADD     WPER          ETCPAR
248000970110     C*
248100011001     C     PAC(I)        MULT      100           WN153
248200011001     C     WN153         DIV(H)    WN90          WPER
248300970110     C                   Z-ADD     WPER          ETCPAC
248400060315     C*
248500060315     C     xx1(I)        MULT      100           WN153
248600060315     C     WN153         DIV(H)    WN90          WPER
248700060315     C                   Z-ADD     WPER          ETCxx1
248800970110     C*
248900970110     C                   Z-ADD     *ZEROS        ETCASC
249000970110IF  1C     PAC(I)        IFGT      *ZEROS
249100970110     C     ASC(I)        DIV       PAC(I)        ETCASC
249200970110E   1C                   ENDIF
249300970110     C                   Z-ADD     ETCASC        $IMP
249400970110     C                   MOVEL     'E'           $FDE
249500011011     C                   IF        PARDIV = 'ITL'
249600011026     C                   Z-ADD     1000          $ARR             21 8
249700011011     C                   ELSE
249800011026     C                   Z-ADD     1             $ARR             21 8
249900011011     C                   ENDIF
250000970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
250100970110     C                   Z-ADD     $IMP          ETCASC
250200970110     C*
250300000414     C                   Z-ADD     *ZEROS        ETCGGG
250400000414     C                   Z-ADD     *ZEROS        ETCNRG
250500011001     C     PGI(I)        MULT      100           WN153
250600011001     C     WN153         DIV(H)    SPE(I)        WPER
250700970110     C                   Z-ADD     WPER          ETCPGI
250800021122     C* SE LA % GIACENZE SUL FILE VIENE A 0 => AZZERO ANCHE I GG GIACENZA ED IL NUMERO APERTURE GIAC
250900021122     C                   IF        ETCPGI = *ZEROS
251000021122     C                   Z-ADD     *ZEROS        ETCGGG
251100021122     C                   Z-ADD     *ZEROS        ETCNRG
251200021122     C                   ELSE
251300000414IF  1C     PGI(I)        IFGT      *ZEROS
251400000414     C     GGS(I)        DIV       PGI(I)        ETCGGG                         *GG SOSTA
251500000414IF  2C     ETCGGG        IFEQ      *ZEROS
251600000414     C                   Z-ADD     1             ETCGGG
251700000414E   2C                   ENDIF
251800000414     C     NRG(I)        DIV       PGI(I)        ETCNRG                         *N° RIAPERTURE
251900000414IF  2C     ETCNRG        IFEQ      *ZEROS
252000000414     C                   Z-ADD     1             ETCNRG
252100000414E   2C                   ENDIF
252200000414E   1C                   ENDIF
252300021122     C                   ENDIF
252400970110     C*
252500970110     C                   Z-ADD     *ZEROS        WFRQ              3 0
252600011001     C                   Z-ADD     *ZEROS        WN153
252700970110DO  1C     1             DO        30            J
252800011001IF  2C     SGP(J)        IFGT      WN153
252900011001     C                   Z-ADD     SGP(J)        WN153
253000970110     C                   Z-ADD     GGP(J)        WFRQ
253100970110E   2C                   ENDIF
253200970110E   1C                   ENDDO
253300970110     C                   Z-ADD     WFRQ          ETCGGP
253400970110IF  1C     ETCGGP        IFEQ      999
253500970110     C                   Z-ADD     *ZEROS        ETCGGP
253600970110E   1C                   ENDIF
253700970110     C*
253800970110     C                   Z-ADD     *ZEROS        WSPE             11 0
253900000411     C                   Z-ADD     *ZEROS        WSTP             13 0
254000970110DO  1C     1             DO        999           J
254100970110IF  2C     RIT(J)        IFGT      *ZEROS
254200000411     C     RIT(J)        DIV       J             WN13             13 0
254300000411     C                   MVR                     RESTO            15 0
254400000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
254500000411     C     J             ANDNE     999                                          *STOP <> 999
254600000411     C                   ADD       1             WN13
254700000411E   3C                   ENDIF
254800970110     C     WSPE          ADD       RIT(J)        WSPE
254900970110     C     WSTP          ADD       WN13          WSTP
255000970110E   2C                   ENDIF
255100970110E   1C                   ENDDO
255200970110     C                   Z-ADD     *ZEROS        ETCSPR
255300970110IF  1C     WSTP          IFGT      *ZEROS
255400970110     C     WSPE          DIV       WSTP          ETCSPR
255500000411     C                   MVR                     RESTO            15 0
255600000411IF  2C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
255700000411     C                   ADD       1             ETCSPR                         *ARROTONDA X ECCESSO
255800000411E   2C                   ENDIF
255900000411E   1C                   ENDIF
256000970110IF  1C     ETCSPR        IFEQ      *ZEROS
256100970110     C                   Z-ADD     1             ETCSPR
256200970110E   1C                   ENDIF
256300970110     C*
256400970110     C                   Z-ADD     *ZEROS        WSPE             11 0
256500000411     C                   Z-ADD     *ZEROS        WSTP             13 0
256600970110DO  1C     1             DO        999           J
256700970110IF  2C     CON(J)        IFGT      *ZEROS
256800000411     C     CON(J)        DIV       J             WN13             13 0
256900000411     C                   MVR                     RESTO            15 0
257000000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
257100000411     C     J             ANDNE     999                                          *STOP <> 999
257200000411     C                   ADD       1             WN13
257300000411E   3C                   ENDIF
257400970110     C     WSPE          ADD       CON(J)        WSPE
257500970110     C     WSTP          ADD       WN13          WSTP
257600970110E   2C                   ENDIF
257700970110E   1C                   ENDDO
257800001221     C                   Z-ADD     1             ETCSPC
257900970110     C*---
258000970110     C* CAMPI FISSI
258100970110     C*---
258200970110     C                   MOVEL     *BLANKS       ETCSDE
258300970110     C                   Z-ADD     *ZEROS        ETCPDE
258400970619     C                   MOVEL     'E'           ETCFAR
258500011011     C                   IF        PARDIV = 'ITL'
258600970314     C                   Z-ADD     100           ETCIAR
258700011011     C                   ELSE
258800011030     C                   Z-ADD     0,1           ETCIAR
258900011011     C                   ENDIF
259000970213     C                   MOVEL     'N'           ETCSIN
259100970110     C                   MOVEL     'N'           ETCIAP
259200970110     C                   MOVEL     'N'           ETCIPI
259300970110     C                   MOVEL     'N'           ETCISU
259400970110     C                   MOVEL     'N'           ETCIFU
259500970415     C                   MOVEL     'N'           ETCIDD
259600970415     C                   MOVEL     'N'           ETCICE
259700970415     C                   MOVEL     'N'           ETCILO
259800050810     C                   MOVEL     'N'           ETCIde
259900050810     C                   MOVEL     'N'           ETCIba
260000970110     C                   MOVEL     'N'           ETCIES
260100970110     C                   MOVEL     'N'           ETCICA
260200970110     C                   MOVEL     'N'           ETCIAS
260300970110     C                   MOVEL     'N'           ETCIGI
260400020716     C                   MOVEL     'N'           ETCISO
260500020716     C                   MOVEL     'N'           ETCINR
260600980901     C                   MOVEL     METX(3)       ETCDCV
260700970110     C*
260800970110     C                   MOVEL     *BLANKS       ETCFTR
260900970110     C                   Z-ADD     *ZEROS        ETCDTR
261000970110     C                   MOVEL     *BLANKS       ETCATB
261100021104     C*
261200021104     C* EFFETTUO ALCUNE CONSIDERAZIONI
261300021104     C                   IF        ETCBOL = 'B'
261400021104     C                   EVAL      ETCIX1 = 'K'
261500021104     C                   ELSE
261600021104     C                   EVAL      ETCIX1 = *blanks
261700021104     C                   ENDIF
261800040811     C* %sped oltre 20 colli non imbancalate
261900040811     C     sbnb(I)       MULT      100           WN153
262000040802     C     WN153         DIV(H)    SPE(I)        WPER
262100040802     C                   Z-ADD     WPER          ETCbnb
262200040811     c* numero medio bancali x spedizioni oltre 20 colli
262300061006     c                   if        sbnb(i) <> 0 and etcbnb <> 0
262400040811     c     nbn(i)        div(h)    sbnb(i)       etcnbn
262500061006     c                   if        etcnbn = 0
262600061006     c                   eval      etcnbn = 1
262700061006     c                   end
262800040812     c                   else
262900040812     c                   clear                   etcnbn
263000040811     c                   end
263100040824     C* %sped oltre 20 colli non imbancalate
263200040824     C     sban(I)       MULT      100           WN153
263300040824     C     WN153         DIV(H)    SPE(I)        WPER
263400040824     C                   Z-ADD     WPER          ETCban
263500040824     c* numero medio bancali Bartolini
263600061006     c                   if        sban(i) <> 0 and etcban <> 0
263700040824     c     nba(i)        div(h)    sban(i)       etcnba
263800061006     c                   if        etcnba = 0
263900061006     c                   eval      etcnba = 1
264000061006     c                   end
264100040824     c                   else
264200040824     c                   clear                   etcnba
264300040824     c                   end
264400040802     C*
264500040802     C     ded(I)        MULT      100           WN153
264600040802     C     WN153         DIV(H)    SPE(I)        WPER
264700040802     C                   Z-ADD     WPER          ETCdes
264800040802     C*
264900040802     C     val(I)        MULT      100           WN153
265000040802     C     WN153         DIV(H)    SPE(I)        WPER
265100040802     C                   Z-ADD     WPER          ETCval
265200970110     C*
265300970110     C                   ENDSR
265400970416     C*------------------------------------------------------------------------*
265500060829     C* SCRSGL - SCRIVE SCHIERE SCAGLIONI ricavi
265600970416     C*------------------------------------------------------------------------*
265700970416     C     SCRSGL        BEGSR
265800970416     C*
265900970416     C* AZZERA LE VARIABILI DI LAVORO
266000970416     C                   CLEAR                   §SG
266100970416     C                   CLEAR                   §PS
266200970416     C                   CLEAR                   §CS
266300970416     C                   CLEAR                   §VS
266400970416     C                   CLEAR                   §SP
266500971008     C                   CLEAR                   §SD
266600970416     C*
266700970416     C* PER CIASCUN SCAGLIONE DA SCRIVERE
266800970416DO  1C     1             DO        10            J
266900970416IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
267000970416     C                   Z-ADD     SSGL(J)       §SG(J)
267100970416IF  3C     SSP(J)        IFGT      *ZEROS
267200970416     C*
267300970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
267400971008     C                   MOVEL     *BLANKS       §SD(J)                         *VALORE NON DEDOTTO
267500970416     C     PSC(J)        DIV       SSP(J)        §PS(J)
267600970416     C     CSC(J)        DIV       SSP(J)        §CS(J)
267700970416     C     VSC(J)        DIV       SSP(J)        §VS(J)
267800011001     C     SSP(J)        MULT      100           WN153
267900011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
268000970416     C                   Z-ADD     WPER          §SP(J)
268100970703IF  4C     §SP(J)        IFEQ      *ZEROS
268200970703     C                   Z-ADD     0,1           §SP(J)
268300970703E   4C                   ENDIF
268400970416X   3C                   ELSE
268500970416     C*
268600970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
268700971008     C                   MOVEL     '*'           §SD(J)                         *VALORE DEDOTTO
268800970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
268900970416     C     PARTTR        OREQ      '3'                                          *KG
269000970416     C                   Z-ADD     §SG(J)        §PS(J)
269100970416     C     §PS(J)        DIV       ETCPMC        §CS(J)
269200970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
269300970416X   4C                   ELSE                                                   *COLLI
269400970416     C                   Z-ADD     §SG(J)        §CS(J)
269500040812     C     §CS(J)        mult      ETCPMC        §PS(J)
269600970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
269700970416E   4C                   ENDIF
269800970416     C                   Z-ADD     0,1           §SP(J)
269900970416E   3C                   ENDIF
270000970416E   2C                   ENDIF
270100970416E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
270200000707     C*
270300000707     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
270400000707     C                   EXSR      PARPSG
270500970416     C*
270600970416     C                   ENDSR
270700060829     C*------------------------------------------------------------------------*
270800060829     C* SCRSGL1- SCRIVE SCHIERE SCAGLIONI competenze
270900060829     C*------------------------------------------------------------------------*
271000060829     C     SCRSGL1       BEGSR
271100060829     C*
271200060829     C* AZZERA LE VARIABILI DI LAVORO
271300060829     C                   CLEAR                   §SG
271400060829     C                   CLEAR                   §PS1
271500060829     C                   CLEAR                   §CS1
271600060829     C                   CLEAR                   §VS1
271700060829     C                   CLEAR                   §SP1
271800060829     C                   CLEAR                   §SD1
271900060829     C*
272000060829     C* PER CIASCUN SCAGLIONE DA SCRIVERE
272100060829DO  1C     1             DO        10            J
272200060829IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
272300060829     C                   Z-ADD     SSGL(J)       §SG(J)
272400060829IF  3C     SSP1(J)       IFGT      *ZEROS
272500060829     C*
272600060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
272700060829     C                   MOVEL     *BLANKS       §SD1(J)                        *VALORE NON DEDOTTO
272800060829     C     PSC1(J)       DIV       SSP1(J)       §PS1(J)
272900060829     C     CSC1(J)       DIV       SSP1(J)       §CS1(J)
273000060829     C     VSC1(J)       DIV       SSP1(J)       §VS1(J)
273100060829     C     SSP1(J)       MULT      100           WN153
273200060829     C     WN153         DIV(H)    SPE(I)        WPER              4 1
273300060829     C                   Z-ADD     WPER          §SP1(J)
273400060829IF  4C     §SP1(J)       IFEQ      *ZEROS
273500060829     C                   Z-ADD     0,1           §SP1(J)
273600060829E   4C                   ENDIF
273700060829X   3C                   ELSE
273800060829     C*
273900060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
274000060829     C                   MOVEL     '*'           §SD1(J)                        *VALORE DEDOTTO
274100060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
274200060829     C     PARTTR        OREQ      '3'                                          *KG
274300060829     C                   Z-ADD     §SG(J)        §PS1(J)
274400060829     C     §PS1(J)       DIV       ETCPMC1       §CS1(J)
274500060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
274600060829X   4C                   ELSE                                                   *COLLI
274700060829     C                   Z-ADD     §SG(J)        §CS1(J)
274800060829     C     §CS1(J)       mult      ETCPMC1       §PS1(J)
274900060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
275000060829E   4C                   ENDIF
275100060829     C                   Z-ADD     0,1           §SP1(J)
275200060829E   3C                   ENDIF
275300060829E   2C                   ENDIF
275400060829E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
275500060829     C*
275600060829     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
275700060829     C                   EXSR      PARPSG1
275800060829     C*
275900060829     C                   ENDSR
276000000707     C*------------------------------------------------------------------------*
276100000707     C* PARPSG - PAREGGIA LA %TOTALE -SCAGLIONI- A 100
276200000707     C*------------------------------------------------------------------------*
276300000707     C     PARPSG        BEGSR
276400000707     C*
276500000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
276600000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
276700000707DO  1C     1             DO        10            J
276800000707IF  2C     §SG(J)        IFGT      *ZEROS
276900000707     C                   ADD       §SP(J)        TOTPPE                         *%INCIDENZA TOTALE
277000000707E   2C                   ENDIF
277100000707E   1C                   ENDDO
277200000707     C*
277300000707     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
277400000707     C                   Z-ADD     *ZEROS        WN41              4 1
277500000707     C     100           SUB       TOTPPE        WN41
277600000707     C*
277700000707     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
277800000707IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
277900000707DO  2C     WN41          DOWNE     *ZEROS
278000000707DO  3C     1             DO        10            J
278100000707IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
278200000707     C* POSITIVA -> AGGIUNGERE
278300000707IF  5C     WN41          IFGT      *ZEROS
278400000707     C     §SP(J)        ADD       0,1           §SP(J)                         *AGGIUNGE %
278500000707     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
278600000707E   5C                   ENDIF
278700000707     C* NEGATIVA -> TOGLIERE
278800000707IF  5C     WN41          IFLT      *ZEROS
278900000707IF  6C     §SP(J)        IFGT      0,1
279000000707     C     §SP(J)        SUB       0,1           §SP(J)                         *SOTTRAE %
279100000707     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
279200000707E   6C                   ENDIF
279300000707E   5C                   ENDIF
279400000707     C* UGUALE A ZERO -> USCIRE
279500000707IF  5C     WN41          IFEQ      *ZEROS
279600000707     C                   LEAVE
279700000707E   5C                   ENDIF
279800000707E   4C                   ENDIF
279900000707E   3C                   ENDDO                                                  *FINE SCAGLIONI
280000000707E   2C                   ENDDO                                                  *DIFFERENZA = 0
280100000707E   1C                   ENDIF
280200000707     C*
280300000707     C                   ENDSR
280400060829     C*------------------------------------------------------------------------*
280500060829     C* PARPSG1- PAREGGIA LA %TOTALE -SCAGLIONI- A 100
280600060829     C*------------------------------------------------------------------------*
280700060829     C     PARPSG1       BEGSR
280800060829     C*
280900060829     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
281000060829     C                   Z-ADD     *ZEROS        TOTPPE            4 1
281100060829DO  1C     1             DO        10            J
281200060829IF  2C     §SG(J)        IFGT      *ZEROS
281300060829     C                   ADD       §SP1(J)       TOTPPE                         *%INCIDENZA TOTALE
281400060829E   2C                   ENDIF
281500060829E   1C                   ENDDO
281600060829     C*
281700060829     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
281800060829     C                   Z-ADD     *ZEROS        WN41              4 1
281900060829     C     100           SUB       TOTPPE        WN41
282000060829     C*
282100060829     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
282200060829IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
282300060829DO  2C     WN41          DOWNE     *ZEROS
282400060829DO  3C     1             DO        10            J
282500060829IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
282600060829     C* POSITIVA -> AGGIUNGERE
282700060829IF  5C     WN41          IFGT      *ZEROS
282800060829     C     §SP1(J)       ADD       0,1           §SP1(J)                        *AGGIUNGE %
282900060829     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
283000060829E   5C                   ENDIF
283100060829     C* NEGATIVA -> TOGLIERE
283200060829IF  5C     WN41          IFLT      *ZEROS
283300060829IF  6C     §SP1(J)       IFGT      0,1
283400060829     C     §SP1(J)       SUB       0,1           §SP1(J)                        *SOTTRAE %
283500060829     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
283600060829E   6C                   ENDIF
283700060829E   5C                   ENDIF
283800060829     C* UGUALE A ZERO -> USCIRE
283900060829IF  5C     WN41          IFEQ      *ZEROS
284000060829     C                   LEAVE
284100060829E   5C                   ENDIF
284200060829E   4C                   ENDIF
284300060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
284400060829E   2C                   ENDDO                                                  *DIFFERENZA = 0
284500060829E   1C                   ENDIF
284600060829     C*
284700060829     C                   ENDSR
284800970110     C*------------------------------------------------------------------------*
284900060829     C* IMPETS - IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- ricavi
285000970110     C*------------------------------------------------------------------------*
285100970318     C     IMPETS        BEGSR
285200970110     C*---
285300970110     C* CAMPI CALCOLATI
285400970110     C*---
285500970416     C                   Z-ADD     §PS(J)        ETSPSC
285600970416     C                   Z-ADD     §CS(J)        ETSCSC
285700981020IF  1C     ETSCSC        IFLT      1
285800981020     C                   Z-ADD     1             ETSCSC
285900981020E   1C                   ENDIF
286000970416     C                   Z-ADD     §VS(J)        ETSVSC
286100970416     C                   Z-ADD     §SP(J)        ETSPPE
286200970415     C*---
286300970415     C* CAMPI FISSI
286400970415     C*---
286500980325     C                   Z-ADD     *ZEROS        ETSINL
286600970415     C                   Z-ADD     *ZEROS        ETSRPV
286700970415     C                   MOVEL     *BLANKS       ETSSDE
286800970415     C                   Z-ADD     *ZEROS        ETSPDE
286900970110     C*
287000040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
287100040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
287200040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
287300040805     C* MA È GIUSTO COSÌ
287400040805     C                   MOVEL     §SD(J)        ETSFTR
287500970110     C                   Z-ADD     *ZEROS        ETSDTR
287600970110     C                   MOVEL     *BLANKS       ETSATB
287700970110     C*
287800970110     C                   ENDSR
287900060829     C*------------------------------------------------------------------------*
288000060829     C* IMPETS1- IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- competenze
288100060829     C*------------------------------------------------------------------------*
288200060829     C     IMPETS1       BEGSR
288300060829     C*---
288400060829     C* CAMPI CALCOLATI
288500060829     C*---
288600060829     C                   Z-ADD     §PS1(J)       £ETSPSC
288700060829     C                   Z-ADD     §CS1(J)       £ETSCSC
288800060829IF  1C     £ETSCSC       IFLT      1
288900060829     C                   Z-ADD     1             £ETSCSC
289000060829E   1C                   ENDIF
289100060829     C                   Z-ADD     §VS1(J)       £ETSVSC
289200060829     C                   Z-ADD     §SP1(J)       £ETSPPE
289300060829     C*---
289400060829     C* CAMPI FISSI
289500060829     C*---
289600060829     C                   Z-ADD     *ZEROS        £ETSINL
289700060829     C                   Z-ADD     *ZEROS        £ETSRPV
289800060829     C                   MOVEL     *BLANKS       £ETSSDE
289900060829     C                   Z-ADD     *ZEROS        £ETSPDE
290000060829     C*
290100060829     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
290200060829     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
290300060829     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
290400060829     C* MA È GIUSTO COSÌ
290500060829     C                   MOVEL     §SD1(J)       £ETSFTR
290600060829     C                   Z-ADD     *ZEROS        £ETSDTR
290700060829     C                   MOVEL     *BLANKS       £ETSATB
290800060829     C*
290900060829     C                   ENDSR
291000970416     C*------------------------------------------------------------------------*
291100970416     C* SCRCTS - SCRIVE SCHIERE CODICI TASSAZIONE
291200970416     C*------------------------------------------------------------------------*
291300970416     C     SCRCTS        BEGSR
291400000707     C*---
291500000707     C* ITALIA
291600000707     C*---
291700000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
291800000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
291900000707     C                   CLEAR                   §RC
292000000707     C                   CLEAR                   §CP
292100000707     C                   CLEAR                   §CD
292200980821IF  1C     XFIE          IFEQ      'I'
292300970423     C                   XFOOT     TSI           TOTSPE           11 0          *TOT. SPED. ITALIA
292400050103DO  2C     1             DO        600           J
292500970423IF  3C     CCTI(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
292600000620     C     CRCI(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
292700970423     C                   MOVEL     CCTI(J)       §CT(J)
292800970423     C                   MOVEL     CRCI(J)       §RC(J)
292900970417     C*
293000970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
293100971008     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
293200970423IF  4C     TSI(J)        IFGT      *ZEROS
293300011001     C     TSI(J)        MULT      100           WN153
293400011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
293500970417     C                   Z-ADD     WPER          §CP(J)
293600970703IF  5C     §CP(J)        IFEQ      *ZEROS
293700970703     C                   Z-ADD     0,1           §CP(J)
293800970703E   5C                   ENDIF
293900970423X   4C                   ELSE
294000970417     C*
294100970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
294200971008     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
294300970417     C                   Z-ADD     0,1           §CP(J)
294400970423E   4C                   ENDIF
294500970423E   3C                   ENDIF
294600970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
294700000707     C*
294800000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
294900000707     C                   EXSR      PARPCT
295000970423     C*---
295100970423     C* ESTERO
295200970423     C*---
295300140916     C* PER CIASCUN CODICE TASSAZIONE -estero- DA SCRIVERE
295400970423X   1C                   ELSE
295500000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
295600000707     C                   CLEAR                   §RC
295700000707     C                   CLEAR                   §CP
295800000707     C                   CLEAR                   §CD
295900970423     C                   XFOOT     TSE           TOTSPE           11 0          *TOT. SPED. ITALIA
296000050103DO  2C     1             DO        600           J
296100970423IF  3C     CCTE(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
296200000620     C     CRCE(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
296300970423     C                   MOVEL     CCTE(J)       §CT(J)
296400970423     C                   MOVEL     CRCE(J)       §RC(J)
296500970423     C*
296600970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
296700990803     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
296800970423IF  4C     TSE(J)        IFGT      *ZEROS
296900011001     C     TSE(J)        MULT      100           WN153
297000011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
297100970423     C                   Z-ADD     WPER          §CP(J)
297200970703IF  5C     §CP(J)        IFEQ      *ZEROS
297300970703     C                   Z-ADD     0,1           §CP(J)
297400970703E   5C                   ENDIF
297500970423X   4C                   ELSE
297600970423     C*
297700970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
297800990803     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
297900970423     C                   Z-ADD     0,1           §CP(J)
298000970423E   4C                   ENDIF
298100970423E   3C                   ENDIF
298200970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
298300000707     C*
298400000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
298500000707     C                   EXSR      PARPCT
298600000707E   1C                   ENDIF
298700000707     C*
298800000707     C                   ENDSR
298900140919     C*------------------------------------------------------------------------*
299000140919     C* PARPCT - PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
299100140919     C*------------------------------------------------------------------------*
299200140919     C     PARPCT        BEGSR
299300140919     C*
299400140919     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
299500140919     C                   Z-ADD     *ZEROS        TOTPPE            4 1
299600140919DO  1C     1             DO        600           J
299700140919IF  2C     §CT(J)        IFNE      *BLANKS
299800140919     C                   ADD       §CP(J)        TOTPPE                         *%INCIDENZA TOTALE
299900140919E   2C                   ENDIF
300000140919E   1C                   ENDDO
300100140919     C*
300200140919     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
300300140919     C                   Z-ADD     *ZEROS        WN41              4 1
300400140919     C     100           SUB       TOTPPE        WN41
300500140919     C*
300600140919     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
300700140919IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
300800140919DO  2C     WN41          DOWNE     *ZEROS
300900140919DO  3C     1             DO        600           J
301000140919IF  4C     §CT(J)        IFNE      *BLANKS                                      *ELEMENTO PIENO
301100140919     C     §RC(J)        ANDNE     *BLANKS                                      *DA CALCOLARE
301200140919     C* POSITIVA -> AGGIUNGERE
301300140919IF  5C     WN41          IFGT      *ZEROS
301400140919     C     §CP(J)        ADD       0,1           §CP(J)                         *AGGIUNGE %
301500140919     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
301600140919E   5C                   ENDIF
301700140919     C* NEGATIVA -> TOGLIERE
301800140919IF  5C     WN41          IFLT      *ZEROS
301900140919IF  6C     §CP(J)        IFGT      0,1
302000140919     C     §CP(J)        SUB       0,1           §CP(J)                         *SOTTRAE %
302100140919     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
302200140919E   6C                   ENDIF
302300140919E   5C                   ENDIF
302400140919     C* UGUALE A ZERO -> USCIRE
302500140919IF  5C     WN41          IFEQ      *ZEROS
302600140919     C                   LEAVE
302700140919E   5C                   ENDIF
302800140919E   4C                   ENDIF
302900140919E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
303000140919E   2C                   ENDDO                                                  *DIFFERENZA = 0
303100140919E   1C                   ENDIF
303200140919     C*
303300140919     C                   ENDSR
303400970110     C*------------------------------------------------------------------------*
303500970110     C* IMPETT - IMPOSTA I CAMPI DATI TIPO -CODICI TASSAZIONE-
303600970110     C*------------------------------------------------------------------------*
303700970110     C     IMPETT        BEGSR
303800970110     C*---
303900970110     C* CAMPI CALCOLATI
304000970110     C*---
304100970417     C                   MOVEL     §RC(J)        ETTRCT
304200970417     C                   Z-ADD     §CP(J)        ETTPPE
304300970110     C*---
304400970110     C* CAMPI FISSI
304500970110     C*---
304600040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
304700040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
304800040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
304900040805     C* MA È GIUSTO COSÌ
305000040805     C                   MOVEL     §CD(J)        ETTFTR
305100970110     C                   Z-ADD     *ZEROS        ETTDTR
305200970110     C                   MOVEL     *BLANKS       ETTATB
305300970110     C*
305400970110     C                   ENDSR
305500140919     C*------------------------------------------------------------------------*
305600140919     C* IMPETT10 - IMPOSTA I CAMPI DATI TIPO -CODICI TASSAZIONE- CAP
305700140919     C*------------------------------------------------------------------------*
305800140919     C     IMPETT10      BEGSR
305900140922     c* calcolo il totale spedizioni x codice tassazione
306000140922     c* solo se ci sono spedizioni
306100140922     c                   clear                   totspec           8 0
306200140923     c     KEyett2       setll     teett11l
306300140922     c                   do        *hival
306400140923     c     KEyett2       reade(n)  teett11l
306500140922     c                   if        %eof(teett11l)
306600140922     c                   leave
306700140922     c                   end
306800140922     c                   add       £ettnsp       totspec
306900140922     c                   enddo
307000140922     c*
307100140922     c                   if        totspec = 0
307200140922     c                   leavesr
307300140926     c                   end
307400140922     c     keyett2       setll     teett11l
307500140922     c                   do        *hival
307600140922     c     keyett2       reade     teett11l
307700140922     c                   if        %eof(teett11l)
307800140922     c                   leave
307900140922     c                   end
308000140922     c*
308100140922IF  4C     £ettnsp       IFGT      *ZEROS
308200140922     C     £ettnsp       DIV(H)    TOTSPEc       £ettinc
308300140926     C                   mult(h)   ettppe        £ettinc
308400140925     c                   add       £ettinc       totinc
308500140923     C                   Z-ADD     *ZEROS        £ETTDTR
308600140922     C                   MOVEL     *BLANKS       £ETTATB
308700140922     c                   update    teett100
308800140922     c                   end
308900140922E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
309000140924     c* scrivo un record di codice tassazione a totale
309100140924     c                   clear                   £ettnsp
309200140924     c                   clear                   £ettinc
309300140924     c                   clear                   £ettnaz
309400140924     c                   clear                   £ettcad
309500140925     c                   clear                   kttnaz
309600140925     c                   clear                   kttcad
309700140925     c     keyett1       chain     teett11l
309800140925     c                   clear                   £ettatb
309900140924     C                   movel     ettrct        £ettrct
310000140924     C                   z-add     ettppe        £ettppe
310100140925     c                   if        %found(teett11l)
310200140925     c                   update    teett100
310300140925     c                   else
310400140925     c                   write     teett100
310500140925     c                   end
310600140919     C*
310700140919     C                   ENDSR
310800140924     C*------------------------------------------------------------------------*
310900140924     C* PARPCT1- PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
311000140924     C*------------------------------------------------------------------------*
311100140924     C     PARPCT1       BEGSR
311200140924     C*
311300140924     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
311400140924     C                   Z-ADD     *ZEROS        Winc              8 5
311500140924     C     100           SUB       TOTinc        Winc
311600140924     C*
311700140924     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
311800140924IF  1C     Winc          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
311900140924DO  2C     Winc          DOWNE     *ZEROS
312000140924     c     ke2ett        setll     teett11l
312100140924     c                   do        *hival
312200140924     c     ke2ett        reade     teett11l
312300140924     c                   if        %eof(teett11l)
312400140924     c                   leave
312500140924     c                   end
312600140924     C* POSITIVA -> AGGIUNGERE
312700140924IF  5C     Winc          IFGT      *ZEROS
312800140925     C     £ettinc       ADD       0,00001       £ettinc                        *AGGIUNGE %
312900140925     C     Winc          SUB       0,00001       Winc                           *DIMINUISCE DIFFER.
313000140924E   5C                   ENDIF
313100140924     C* NEGATIVA -> TOGLIERE
313200140924IF  5C     Winc          IFLT      *ZEROS
313300140925IF  6C     £ettinc       IFGT      0,00001
313400140925     C     £ettinc       SUB       0,00001       £ettinc                        *SOTTRAE %
313500140925     C     Winc          ADD       0,00001       Winc                           *AUMENTA DIFFER.
313600140924E   6C                   ENDIF
313700140924E   5C                   ENDIF
313800140924     c                   update    teett100
313900140924     C* UGUALE A ZERO -> USCIRE
314000140924IF  5C     Winc          IFEQ      *ZEROS
314100140924     C                   LEAVEsr
314200140924E   5C                   ENDIF
314300140924E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
314400140924E   2C                   ENDDO                                                  *DIFFERENZA = 0
314500140924E   1C                   ENDIF
314600140924     C*
314700140924     C                   ENDSR
314800140922     C*------------------------------------------------------------------------*
314900140922     C* scrETT10T- DATI TIPO -CODICI TASSAZIONE- CAP - LNP 889
315000140922     C*------------------------------------------------------------------------*
315100140922     C     scrett10T     BEGSR
315200140924     C*
315300140924     c                   clear                   totinc
315400140922     c*
315500140922     C/EXEC SQL
315600140924     C+ DECLARE E1 CURSOR FOR SELECT sum(ettnsp)
315700140923     C+ FROM teett10f WHERE ETTDSF = 'S' and
315800140922     C+ ETTCTO = 'T' and ETTKSC = :parkov and ETTCTR = :parctr and ETTPRG
315900140922     C+ = :parprg and ETTFIE = 'E' and ettatb = ' ' and ettlnp <> 889
316000140924     c+ and ettcts = :kttcts
316100140922     C/END-EXEC
316200140922     c*
316300140922     C/EXEC SQL
316400140922     C+ OPEN e1
316500140922     C/END-EXEC
316600140922
316700140922     C                   DO        *HIVAL
316800140922
316900140922     C/EXEC SQL
317000140924     C+ FETCH NEXT FROM e1 INTO :£nsp
317100140922     C/END-EXEC
317200140922
317300140922     C                   IF        SQLCod = 100 or sqlcod < 0
317400140922     C                   LEAVE
317500140922     C                   ENDIF
317600140922     C                   MOVEL     'S'           KTTDSF
317700140922     C                   MOVEL     'T'           KTTCTO
317800140922     C                   Z-ADD     PARkov        KTTKSC
317900140922     C                   Z-ADD     PARCTR        KTTCTR
318000140922     C                   Z-ADD     PARPRG        KTTPRG
318100140922     C                   Z-ADD     889           KTTLNP
318200140924     c                   clear                   kttntw
318300140922     C                   MOVEL     'E'           KTTFIE
318400140924     C*                  MOVEL     kttcts        KTTCTS
318500140922     C                   clear                   KTTnaz
318600140922     c                   clear                   kttcad
318700140922     C     KEYETT1       CHAIN     TEETT11L                           96
318800140922IF  4C     *IN96         IFEQ      *ON
318900140922     C                   MOVEL     KTTDSF        £ETTDSF
319000140922     C                   MOVEL     KTTCTO        £ETTCTO
319100140922     C                   Z-ADD     KTTKSC        £ETTKSC
319200140922     C                   Z-ADD     KTTCTR        £ETTCTR
319300140922     C                   Z-ADD     KTTPRG        £ETTPRG
319400140922     C                   Z-ADD     KTTLNP        £ETTLNP
319500140922     C                   MOVEL     KTTFIE        £ETTFIE
319600140922     C                   MOVEL     KTTCTS        £ETTCTS
319700140922     C                   MOVEL     KTTnaz        £ETTnaz
319800140922     C                   MOVEL     KTTcad        £ETTcad
319900140922     C                   MOVEL     KTTntw        £ETTntw
320000140922     C                   Z-ADD     DATCOR        £ETTDIR
320100140922     C                   Z-ADD     £nsp          £ETTnsp
320200140923     c                   eval      £ettrct = ettrct
320300140923     c                   eval      £ettppe = ettppe
320400140923     c                   eval      £ettftr = ettftr
320500140926     C                   eval      £ettinc  = ettppe
320600140922     C                   Z-ADD     *ZEROS        £ETTDUV
320700140922     C                   MOVEL     *BLANKS       £ETTATB
320800140922     C                   WRITE     TEETT100                                     *INSERIMENTO
320900140922     C                   UNLOCK    TEETT11L
321000140922X   4C                   ELSE
321100140922     c                   z-add     £nsp          £ettnsp
321200140924     c                   eval      £ettrct = ettrct
321300140924     c                   eval      £ettppe = ettppe
321400140924     c                   eval      £ettftr = ettftr
321500140926     C                   eval      £ettinc  = ettppe
321600140922     C                   Z-ADD     DATCOR        £ETTDUV
321700140922     C                   MOVEL     *BLANKS       £ETTATB
321800140922     C                   UPDATE    TEETT100                                     *VARIAZIONE
321900140922E   4C                   ENDIF
322000140922     C                   ENDDO
322100140922
322200140922     C/EXEC SQL
322300140922     C+ CLOSE e1
322400140922     C/END-EXEC
322500140922     c*
322600140922     C                   ENDSR
322700970417     C*------------------------------------------------------------------------*
322800970417     C* ARRIMP - ARROTONDA IMPORTO
322900970417     C*------------------------------------------------------------------------*
323000970417     C     ARRIMP        BEGSR
323100970417     C*---
323200970417     C* $IMP -> IMPORTO DA ARROTONDARE
323300970417     C* $FDE -> 'D'=DIFETTO / E=ECCESSO
323400970417     C* $ARR  : IMPORTO AL QUALE ARROTONDARE
323500970417     C*---
323600970417IF  1C     $IMP          IFGT      *ZEROS                                       *IMPORTO PIENO
323700970417IF  2C     $ARR          IFGT      *ZEROS                                       *SI ARROTONDAMENTO
323800970417     C     $IMP          DIV       $ARR          COMODO           15 0
323900970417     C                   MVR                     RESTO            15 0
324000970417IF  3C     $FDE          IFEQ      'E'
324100970417IF  4C     RESTO         IFGT      *ZEROS
324200970417     C                   ADD       1             COMODO
324300970417E   4C                   ENDIF
324400970417E   3C                   ENDIF
324500970417     C     COMODO        MULT      $ARR          $IMP
324600970417E   2C                   ENDIF
324700970417E   1C                   ENDIF
324800970417     C*
324900970417     C                   ENDSR
325000970110     C*------------------------------------------------------------------------*
325100970110     C* STAERR - STAMPA RIGA DI ERRORE
325200970110     C*------------------------------------------------------------------------*
325300970110     C     STAERR        BEGSR
325400970110     C*
325500970110IF  1C     *INOF         IFEQ      *ON
325600970110     C                   EXCEPT    TES
325700970110     C                   SETOFF                                       OF
325800970110E   1C                   ENDIF
325900970110IF  1C     DEPTER        IFEQ      'T'
326000970110     C                   EXCEPT    ETAS
326100970110E   1C                   ENDIF
326200970110IF  1C     DEPTER        IFEQ      'X'
326300970110     C                   EXCEPT    EETX
326400970110E   1C                   ENDIF
326500970110     C*
326600970110     C                   ENDSR
326700970313     C*------------------------------------------------------------------------*
326800970314     C* ANNETX - ANNULLA I DATI TIPO ESISTENTI
326900970313     C*------------------------------------------------------------------------*
327000970314     C     ANNETX        BEGSR
327100970313     C*
327200970314     C* PER OGNI FILIALE RICHIESTA
327300971124DO  1C     1             DO        500           I
327400970313IF  2C     SORG(I)       IFGT      *ZEROS
327500970313     C*---
327600970313     C* DATI TIPO -GENERALI-
327700970313     C*---
327800060804     C                   MOVEL     'S'           KTCDSF
327900980713     C                   MOVEL     PARCTO        KTCCTO
328000970313     C                   Z-ADD     PARKOV        KTCKSC
328100970313     C                   Z-ADD     PARCTR        KTCCTR
328200970313     C                   Z-ADD     PARPRG        KTCPRG
328300970313     C                   Z-ADD     SORG(I)       KTCLNP
328400011001     C     KEYETC        CHAIN     TEETC01L                           99
328500981221IF  3C     *IN99         IFEQ      *OFF
328600971024IF  4C     ETCATB        IFEQ      *BLANKS
328700970314     C                   MOVEL     'A'           ETCATB
328800970314     C                   MOVEL     *BLANKS       ETCFTR
328900011001     C                   UPDATE    TEETC000
329000970314E   4C                   ENDIF
329100970314E   3C                   ENDIF
329200970313     C*---
329300970313     C* DATI TIPO -SCAGLIONI-
329400970313     C*---
329500060829     c* ricavi
329600980713     C                   MOVEL     'S'           KTSDSF
329700980713     C                   MOVEL     PARCTO        KTSCTO
329800970313     C                   Z-ADD     PARKOV        KTSKSC
329900970313     C                   Z-ADD     PARCTR        KTSCTR
330000970313     C                   Z-ADD     PARPRG        KTSPRG
330100970313     C                   Z-ADD     SORG(I)       KTSLNP
330200011001     C     KE2ETS        SETLL     TEETS01L
330300011001     C     KE2ETS        READE     TEETS01L                               99
330400970314DO  3C     *IN99         DOWEQ     *OFF
330500971024IF  4C     ETSATB        IFEQ      *BLANKS
330600970314     C                   MOVEL     'A'           ETSATB
330700970314     C                   MOVEL     *BLANKS       ETSFTR
330800011001     C                   UPDATE    TEETS000
330900970314E   4C                   ENDIF
331000011001     C     KE2ETS        READE     TEETS01L                               99
331100060829E   3c                   ENDDO                                                  *FINE SCAGLIONI
331200060829     c* competetnze
331300060829     C     KE2ETS        SETLL     TEETS11L
331400060829     C     KE2ETS        READE     TEETS11L                               99
331500060829DO  3C     *IN99         DOWEQ     *OFF
331600060829IF  4C     £ETSATB       IFEQ      *BLANKS
331700060829     C                   MOVEL     'A'           £ETSATB
331800060829     C                   MOVEL     *BLANKS       £ETSFTR
331900060829     C                   UPDATE    TEETS100
332000060829E   4C                   ENDIF
332100060829     C     KE2ETS        READE     TEETS11L                               99
332200060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
332300970314     C*---
332400970314     C* DATI TIPO -CODICI TASSAZIONE-
332500970314     C*---
332600970429DO  3C     1             DO        2             K
332700970429IF  4C     SFIE(K)       IFNE      *BLANKS
332800980713     C                   MOVEL     'S'           KTTDSF
332900980713     C                   MOVEL     PARCTO        KTTCTO
333000970314     C                   Z-ADD     PARKOV        KTTKSC
333100970314     C                   Z-ADD     PARCTR        KTTCTR
333200970314     C                   Z-ADD     PARPRG        KTTPRG
333300970314     C                   Z-ADD     SORG(I)       KTTLNP
333400970429     C                   MOVEL     SFIE(K)       KTTFIE
333500011001     C     KE2ETT        SETLL     TEETT01L
333600011001     C     KE2ETT        READE     TEETT01L                               99
333700970429DO  5C     *IN99         DOWEQ     *OFF
333800971024IF  6C     ETTATB        IFEQ      *BLANKS
333900970314     C                   MOVEL     'A'           ETTATB
334000970314     C                   MOVEL     *BLANKS       ETTFTR
334100011001     C                   UPDATE    TEETT000
334200970429E   6C                   ENDIF
334300011001     C     KE2ETT        READE     TEETT01L                               99
334400970429E   5C                   ENDDO                                                  *FINE COD.TASSAZIONE
334500970429E   4C                   ENDIF
334600970429E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
334700140919     C*---
334800140919     C* DATI TIPO -CODICI TASSAZIONE- CAP
334900140919     C*---
335000140919     C                   MOVEL     'S'           KTTDSF
335100140919     C                   MOVEL     PARCTO        KTTCTO
335200140919     C                   Z-ADD     PARKOV        KTTKSC
335300140919     C                   Z-ADD     PARCTR        KTTCTR
335400140919     C                   Z-ADD     PARPRG        KTTPRG
335500140919     C                   Z-ADD     SORG(I)       KTTLNP
335600140919     C                   movel     Sntw(I)       KTTntw
335700140919     C                   MOVEL     'E'           KTTFIE
335800140919     C     KE2ETT1       SETLL     TEETT11L
335900140919     C     KE2ETT1       READE     TEETT11L                               99
336000140919DO  5C     *IN99         DOWEQ     *OFF
336100140922IF  6C     £ETTATB       IFEQ      *BLANKS
336200140922     C                   MOVEL     'A'           £ETTATB
336300140922     C                   MOVEL     *BLANKS       £ETTFTR
336400140919     C                   UPDATE    TEETT100
336500140919E   6C                   ENDIF
336600140919     C     KE2ETT1       READE     TEETT11L                               99
336700140919E   5C                   ENDDO                                                  *FINE COD.TASSAZIONE
336800970313     C*
336900970314E   2C                   ENDIF
337000970314E   1C                   ENDDO                                                  *FINE FILIALI
337100970314     C*
337200970313     C                   ENDSR
337300970110     C*------------------------------------------------------------------------*
337400970110     C* OPEFIL - APRE GLI ARCHIVI
337500970110     C*------------------------------------------------------------------------*
337600970110     C     OPEFIL        BEGSR
337700970110     C*
337800970110IF  1C     PARKSC        IFGT      *ZEROS
337900990812     C                   OPEN      TITAS31C                                     *LETTURA X CLIENTE
338000970110X   1C                   ELSE
338100990812     C                   OPEN      TITAS38C                                     *LETTURA X DATA
338200970110E   1C                   ENDIF
338300970110     C*
338400970110     C                   ENDSR
338500970110     C*------------------------------------------------------------------------*
338600970110     C* CLOFIL - CHIUDE GLI ARCHIVI
338700970110     C*------------------------------------------------------------------------*
338800970110     C     CLOFIL        BEGSR
338900970110     C*
339000970110IF  1C     PARKSC        IFGT      *ZEROS
339100990812     C                   CLOSE     TITAS31C                                     *LETTURA X CLIENTE
339200970110X   1C                   ELSE
339300990812     C                   CLOSE     TITAS38C                                     *LETTURA X DATA
339400970110E   1C                   ENDIF
339500970110     C*
339600970110     C                   ENDSR
339700011011     C*------------------------------------------------------------------------*
339800011011     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
339900011011     C*------------------------------------------------------------------------*
340000011011     C     CNVEUR        BEGSR
340100011011     C*
340200011011     C* AZZERA VARIABILI DI LAVORO
340300011011     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
340400011011     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
340500011026     C                   EVAL      NDEC = '9'
340600011011     C*
340700011011IF  1C     WIMI          IFNE      *ZEROS
340800011011     C                   CLEAR                   YEUR
340900011011     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
341000011011     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
341100011011     C                   MOVEL     PARDIV        YECDVO                         *DIVISA IMP CONVERTI
341200011011     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
341300011011     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
341400011011     C                   CALL      'YEURCO'
341500011011     C                   PARM                    YEUR
341600011011IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
341700011011     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
341800011011X   2C                   ELSE                                                   *SI ERRORI
341900011011     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
342000011011E   2C                   ENDIF
342100011011E   1C                   ENDIF
342200011011     C*
342300011011     C                   ENDSR
342400011011     C*------------------------------------------------------------------------*
342500011011     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
342600011011     C*------------------------------------------------------------------------*
342700011011     C     CARGEDCE      BEGSR
342800011011     C*
342900011011     C                   CLEAR                   DSGEDCE
343000011026     C                   MOVEL     *blanks       WDIVCE            3
343100011011     C*
343200011011     C                   MOVEL(P)  'GED'         KTECOD
343300011011     C                   MOVEL(P)  'CE'          KTEKE1
343400011011     C     KEYTBE        CHAIN     TNTBE01L
343500011011     C                   IF        %found(TNTBE01L)
343600011011     C                   MOVEL     TBEUNI        DSGEDCE
343700011011     C                   ENDIF
343800011011     C*
343900011011     C                   IF        §GEDCEDAT > DATCOR
344000011026     C                   EVAL      WDIVCE = §GEDCEDIP
344100011026     C                   ELSE
344200011026     C                   EVAL      WDIVCE = §GEDCEDIV
344300011011     C                   ENDIF
344400011011     C*
344500011011     C                   ENDSR
344600970110     C*------------------------------------------------------------------------*
344700970110     C* CARTAB - CARICAMENTO TABELLE
344800970110     C*------------------------------------------------------------------------*
344900970110     C     CARTAB        BEGSR
345000970110     C*--------------------
345100970110     C* ORGANIGRAMMA
345200970110     C*--------------------
345300020227     C                   Z-ADD     *ZEROS        JJ                3 0
345400970110     C                   Z-ADD     *ZEROS        I                 4 0
345500970110     C     *LOVAL        SETLL     AZORG01L
345600970110     C                   READ      AZORG01L                               99
345700970110DO  1C     *IN99         DOWEQ     *OFF
345800970110IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATE
345900970110     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
346000970110     C                   ADD       1             I
346100970110     C                   Z-ADD     ORGFIL        ORG(I)                         *FILIALE
346200970110     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
346300020730     C                   MOVEL     ORGDE3        OG143
346400020730     C*
346500020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
346600020730     C                   CLEAR                   DNTW
346700020730     C*
346800020730     C                   MOVEL(P)  'NTW'         KTECOD
346900020730     C                   MOVEL(P)  §OGNTW        KTEKE1
347000020730     C     KEYTBE        CHAIN     TNTBE01L
347100020730     C                   IF        %found(TNTBE01L)
347200020730     C                   MOVEL     TBEUNI        DNTW
347300020730     C                   ENDIF
347400020730     C*
347500020730     C                   MOVEL     §NTWFIE       FL1(I)                         *FLAG ITALIA/ESTERO
347600140919     C                   MOVEL     ORGDE3        OG143
347700970110     C* SELEZIONI
347800970430IF  3C     PARLNP        IFEQ      999                                          *TUTTE
347900970430     C                   Z-ADD     ORGFIL        SORG(I)                        *FILIALI SELEZIONATE
348000020730     C                   MOVEL     §NTWFIE       SFL1(I)                        *FLAG ITALIA/ESTERO
348100140919     C                   movel     §OGNTW        sntw(I)
348200970110X   3C                   ELSE
348300970110IF  4C     PARLNP        IFEQ      ORGFIL                                       *UNA FILIALE
348400970417     C                   Z-ADD     ORGFIL        SORG(1)                        *FILIALE SELEZIONATA
348500020730     C                   MOVEL     §NTWFIE       SFL1(1)                        *FLAG ITALIA/ESTERO
348600140919     C                   movel     §OGNTW        sntw(1)
348700970110E   4C                   ENDIF
348800970110E   3C                   ENDIF
348900081105     C* VERIFICO SE FILIALE dpd
349000140919     C                   IF        §OGNTW = 'DPD'
349100081105     C                   ADD       1             xx                3 0
349200081105     C                   MOVEL     ORGFIL        FILdpd(xx)
349300020227     C                   ENDIF
349400081105     C* VERIFICO SE FILIALE FEDEX
349500081105     C                   MOVEL     ORGDE3        OG143
349600081105     C                   IF        §OGNTW = 'FED'
349700081105     C                   ADD       1             JJ
349800081105     C                   MOVEL     ORGFIL        FILFED(JJ)
349900081105     C                   ENDIF
350000970110E   2C                   ENDIF
350100970110     C                   READ      AZORG01L                               99
350200970110E   1C                   ENDDO
350300970423     C*--------------------
350400970423     C* TARIFFA ITALIA/ESTERO
350500970423     C*--------------------
350600970423IF  1C     PARFIE        IFEQ      *BLANKS                                      *ITALIA E ESTERO
350700970423     C                   MOVEL     'I'           SFIE(1)
350800970423     C                   MOVEL     'E'           SFIE(2)
350900970423X   1C                   ELSE                                                   *ITALIA O ESTERO
351000970423     C                   MOVEL     PARFIE        SFIE(1)
351100970423E   1C                   ENDIF
351200970110     C*--------------------
351300970110     C* TIPI TARIFFA
351400970110     C*--------------------
351500970110     C                   Z-ADD     *ZEROS        I                 4 0
351600020923     C                   Z-ADD     1             KTBKUT
351700970110     C                   MOVEL     'TR'          KTBCOD
351800970110     C     KE2TAB        SETLL     TABEL00F
351900970110     C     KE2TAB        READE     TABEL00F                               99
352000970110DO  1C     *IN99         DOWEQ     *OFF
352100970110IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
352200970110     C                   MOVEL     TBLUNI        DSTR
352300970110IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
352400970110     C                   ADD       1             I
352500970110     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
352600970110     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
352700970110E   3C                   ENDIF
352800970110E   2C                   ENDIF
352900970110     C     KE2TAB        READE     TABEL00F                               99
353000970110E   1C                   ENDDO
353100141002     C*--------------------
353200141002     C* nazioni
353300141002     C*--------------------
353400141002     C                   Z-ADD     *ZEROS        I                 4 0
353500141002     C                   Z-ADD     1             KTBKUT
353600141002     C                   MOVEL     '15'          KTBCOD
353700141002     C     KE2TAB        SETLL     TABEL00F
353800141002     C     KE2TAB        READE     TABEL00F                               99
353900141002DO  1C     *IN99         DOWEQ     *OFF
354000141002IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
354100141002     C                   MOVEL     TBLUNI        DS15
354200141002     C                   ADD       1             I
354300141002     C                   MOVEL     TBLKEY        d15(I)                         *CODICE
354400141002     C                   MOVEL     §15pcf        pcf15(I)
354500141002     C                   MOVEL     §15ecf        ecf(I)
354600141002E   2C                   ENDIF
354700141002     C     KE2TAB        READE     TABEL00F                               99
354800141002E   1C                   ENDDO
354900050808     C*--------------------
355000050808     C* cod. trattamento merce con packing list
355100050808     C*--------------------
355200050808     C                   Z-ADD     *ZEROS        I                 4 0
355300050808     C                   Z-ADD     1             KTBKUT
355400050808     C                   MOVEL     '1B'          KTBCOD
355500050808     C     KE2TAB        SETLL     TABEL00F
355600050808     C     KE2TAB        READE     TABEL00F                               99
355700050808DO  1C     *IN99         DOWEQ     *OFF
355800050808IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
355900050808     C                   MOVEL     TBLUNI        DS1b
356000050808IF  3C     §1bf16        IFEQ      'S'                                          *IN GESTIONE
356100050808     C                   ADD       1             I
356200050808     C                   MOVEL     TBLKEY        cpkl(I)                        *CODICE
356300050808E   3C                   ENDIF
356400050808E   2C                   ENDIF
356500050808     C     KE2TAB        READE     TABEL00F                               99
356600050808E   1C                   ENDDO
356700970110     C*--------------------
356800970110     C* RAGGRUPPAMENTI TIPO BOLLA
356900970110     C*--------------------
357000970110     C                   Z-ADD     *ZEROS        I                 4 0
357100020923     C                   Z-ADD     1             KTBKUT
357200970110     C                   MOVEL     '2W'          KTBCOD
357300970110     C     KE2TAB        SETLL     TABEL00F
357400970110     C     KE2TAB        READE     TABEL00F                               99
357500970110DO  1C     *IN99         DOWEQ     *OFF
357600970110IF  2C     TBLFLG        IFEQ      *BLANKS
357700970110     C                   MOVEL     TBLUNI        DS2W
357800970110     C                   ADD       1             I
357900970110     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
358000970110     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
358100970110     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
358200970110E   2C                   ENDIF
358300970110     C     KE2TAB        READE     TABEL00F                               99
358400970110E   1C                   ENDDO
358500970110     C*--------------------
358600970110     C* TIPI BOLLA
358700970110     C*--------------------
358800970110     C                   Z-ADD     *ZEROS        I                 4 0
358900970110     C                   Z-ADD     *ZEROS        J                 4 0
359000020923     C                   Z-ADD     1             KTBKUT
359100970110     C                   MOVEL     'TB'          KTBCOD
359200970110     C     KE2TAB        SETLL     TABEL00F
359300970110     C     KE2TAB        READE     TABEL00F                               99
359400970110DO  1C     *IN99         DOWEQ     *OFF
359500970110IF  2C     TBLFLG        IFEQ      *BLANKS
359600970110     C                   MOVEL     TBLUNI        DSTB
359700970110     C                   ADD       1             I
359800970110     C                   MOVEL     TBLKEY        TBL(I)                         *TIPO BOLLA
359900970110     C                   Z-ADD     1             J
360000970110     C                   SETOFF                                           98
360100970110     C     §TBRBL        LOOKUP    RBL(J)                                 98
360200970110IF  3C     *IN98         IFEQ      *ON
360300970110     C                   MOVEL     FSP(J)        SPD(I)                         *FLAG SPEDIZIONE
360400970110     C                   MOVEL     FRI(J)        RIC(I)                         *FLAG RICAVO
360500970110X   3C                   ELSE
360600970110     C                   MOVEL     'N'           SPD(I)                         *FLAG SPEDIZIONE
360700970110     C                   MOVEL     'N'           RIC(I)                         *FLAG RICAVO
360800970110E   3C                   ENDIF
360900970110E   2C                   ENDIF
361000970110     C     KE2TAB        READE     TABEL00F                               99
361100970110E   1C                   ENDDO
361200970110     C*--------------------
361300970110     C* CODICI PAGAMENTO
361400970110     C*--------------------
361500970110     C                   Z-ADD     *ZEROS        I                 4 0
361600020923     C                   Z-ADD     1             KTBKUT
361700970110     C                   MOVEL     'FA'          KTBCOD
361800970110     C     KE2TAB        SETLL     TABEL00F
361900970110     C     KE2TAB        READE     TABEL00F                               99
362000970110DO  1C     *IN99         DOWEQ     *OFF
362100970110IF  2C     TBLFLG        IFEQ      *BLANKS
362200970110     C                   MOVEL     TBLUNI        DSFA
362300970110     C                   ADD       1             I
362400970110     C                   MOVEL     TBLKEY        CDP(I)                         *CODICE PAGAMENTO
362500970110     C                   MOVEL     §FARAT        CRAT(I)                        *RATE DI PAGAMENTO
362600970110E   2C                   ENDIF
362700970110     C     KE2TAB        READE     TABEL00F                               99
362800970110E   1C                   ENDDO
362900970313     C*--------------------
363000970313     C* CODICI TASSAZIONE
363100970313     C*--------------------
363200970313     C                   Z-ADD     *ZEROS        I                 4 0
363300020923     C                   Z-ADD     1             KTBKUT
363400970313     C                   MOVEL     'CT'          KTBCOD
363500970313     C     KE2TAB        SETLL     TABEL00F
363600970313     C     KE2TAB        READE     TABEL00F                               99
363700970313DO  1C     *IN99         DOWEQ     *OFF
363800970313IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
363900970313     C                   MOVEL     TBLUNI        DSCT
364000970317IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
364100970317     C                   ADD       1             I
364200970317     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
364300970317     C                   MOVEL     §CTDES        DCTS(I)                        *DESCRIZIONE
364400970317     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
364500970317     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
364600140919     C                   MOVEL     §CTnar        nar(I)                         *REGIONE
364700140919     C                   MOVEL     §CTnar        naz(I)                         *REGIONE
364800970317     C                   MOVE      §CTCOR        WA1               1            *TIPO CODICE
364900970417IF  4C     WA1           IFEQ      '9'
365000970417IF  5C     CTS(I)        IFEQ      'IT'
365100970317     C     CTS(I)        OREQ      'EE'
365200970317     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
365300970417X   5C                   ELSE
365400970317     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
365500970417E   5C                   ENDIF
365600970417X   4C                   ELSE
365700970317     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
365800970417E   4C                   ENDIF
365900970417IF  4C     §CTNAR        IFEQ      *BLANKS
366000970317     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
366100970317     C                   MOVEL     'I'           CIE(I)                         *CODICE ITALIA
366200970417X   4C                   ELSE
366300970317     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
366400970317     C                   MOVEL     'E'           CIE(I)                         *CODICE ESTERO
366500970417E   4C                   ENDIF
366600970317E   3C                   ENDIF
366700970317E   2C                   ENDIF
366800970317     C     KE2TAB        READE     TABEL00F                               99
366900970317E   1C                   ENDDO
367000040920     C* CARICO GLI SCAGLIONI DI DEFAULT
367100040920IF  1C     PARTTR        IFEQ      '0'                                          *QUINTALI
367200040920     C     PARTTR        OREQ      '3'                                          *KG
367300040920     C                   Z-ADD     2             SGL(1)
367400040920     C                   Z-ADD     5             SGL(2)
367500040920     C                   Z-ADD     10            SGL(3)
367600040920     C                   Z-ADD     25            SGL(4)
367700040920     C                   Z-ADD     50            SGL(5)
367800040920     C                   Z-ADD     75            SGL(6)
367900040920     C                   Z-ADD     100           SGL(7)
368000040920     C                   Z-ADD     99999         SGL(8)
368100040920X   1C                   ELSE                                                   *COLLI
368200040920     C                   Z-ADD     1             SGL(1)
368300040920     C                   Z-ADD     2             SGL(2)
368400040920     C                   Z-ADD     3             SGL(3)
368500040920     C                   Z-ADD     4             SGL(4)
368600040920     C                   Z-ADD     99999         SGL(5)
368700040920E   1C                   ENDIF
368800970110     C*
368900970110     C                   ENDSR
369000970313     C*------------------------------------------------------------------------*
369100970313     C* CARTAR - CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
369200970313     C*------------------------------------------------------------------------*
369300970313     C     CARTAR        BEGSR
369400970313     C*
369500970313     C* REPERISCE IL CODICE DELLA TARIFFA DI CARTELLO IN VIGORE
369600970314IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORE
369700021202     C                   Z-ADD     *ZEROS        CARKSC
369800021202     C                   Z-ADD     *ZEROS        CARCTR
369900021202     C                   MOVE(P)   *ZEROS        CARPRG
370000021202     C                   CLEAR                   TRUL27DS
370100021202     C                   EVAL      I27TLA = 'L'
370200021202     C                   EVAL      I27DTA = DATCOR
370300021202     C                   CALL      'TRUL27R'
370400021202     C                   PARM                    TRUL27DS
370500021202     C*
370600021202     C                   IF        O27ERR <> 'E'
370700021202     C                   Z-ADD     O27KSC        CARKSC
370800021202     C                   Z-ADD     O27CTR        CARCTR
370900021202     C                   MOVE(P)   O27PRG        CARPRG
371000021202     C                   ENDIF
371100021202E   1C                   ENDIF
371200970317     C*
371300970317     C* PER CIASCUNA FILIALE RICHIESTA
371400971124DO  1C     1             DO        500           Y                 4 0
371500970417IF  2C     SORG(Y)       IFGT      *ZEROS
371600970423     C     Y             OCCUR     FTAR
371700970317     C*---
371800970317     C* CARICA GLI SCAGLIONI DA CONSIDERARE -CLIENTE O CARTELLO-
371900970317     C*---
372000970416     C                   Z-ADD     *ZEROS        SSGL
372100970313     C                   Z-ADD     PARKOV        KTDKSC
372200970313     C                   Z-ADD     PARCTR        KTDCTR
372300970313     C                   Z-ADD     PARPRG        KTDPRG
372400970417     C                   Z-ADD     SORG(Y)       KTDLNP
372500970313     C                   EXSR      CARSGL
372600970417IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
372700970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
372800970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
372900970417     C                   Z-ADD     CARKSC        KTDKSC
373000970313     C                   Z-ADD     CARCTR        KTDCTR
373100970313     C                   Z-ADD     CARPRG        KTDPRG
373200970417     C                   Z-ADD     SORG(Y)       KTDLNP
373300970314     C                   EXSR      CARSGL
373400970703     C                   MOVEL     DEPCTO        PARCTO
373500970418E   3C                   ENDIF
373600970418IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
373700040920     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
373800040920     c                   move      sgl           ssgl
373900970418E   3C                   ENDIF
374000970317     C*---
374100970313     C* CARICA I CODICI TASSAZIONE DA CONSIDERARE -CLIENTE O CARTELLO-
374200970317     C*---
374300970423     C                   MOVEL     *BLANKS       CCTI
374400970423     C                   MOVEL     *BLANKS       CRCI
374500970423     C                   MOVEL     *BLANKS       CCTE
374600970423     C                   MOVEL     *BLANKS       CRCE
374700970423DO  3C     1             DO        2             W                 4 0
374800970423IF  4C     SFIE(W)       IFNE      *BLANKS
374900970314     C                   Z-ADD     PARKOV        KTDKSC
375000970314     C                   Z-ADD     PARCTR        KTDCTR
375100970314     C                   Z-ADD     PARPRG        KTDPRG
375200970417     C                   Z-ADD     SORG(Y)       KTDLNP
375300971204     C                   MOVEL     SFL1(Y)       DLPFL1
375400970314     C                   EXSR      CARCTS
375500970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
375600970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
375700970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
375800970314     C                   Z-ADD     CARKSC        KTDKSC
375900970314     C                   Z-ADD     CARCTR        KTDCTR
376000970314     C                   Z-ADD     CARPRG        KTDPRG
376100970417     C                   Z-ADD     SORG(Y)       KTDLNP
376200971204     C                   MOVEL     SFL1(Y)       DLPFL1
376300970314     C                   EXSR      CARCTS
376400970703     C                   MOVEL     DEPCTO        PARCTO
376500970423E   5C                   ENDIF
376600970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
376700970418     C                   EXSR      FIXCTS                                       *IMPOSTA FISSI CTS
376800970423E   5C                   ENDIF
376900970416     C                   EXSR      RAPSVI                                       *CTS->CTS ELEMENTARI
377000970423E   4C                   ENDIF
377100970423E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
377200970417     C*
377300970417E   2C                   ENDIF
377400970417E   1C                   ENDDO                                                  *FINE FILIALI
377500970313     C*
377600970313     C                   ENDSR
377700970313     C*------------------------------------------------------------------------*
377800970313     C* CARSGL - CARICA GLI SCAGLIONI
377900970313     C*------------------------------------------------------------------------*
378000970313     C     CARSGL        BEGSR
378100970313     C*
378200970313     C* AZZERA LE VARIABILI DI LAVORO
378300970313     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
378400970313     C                   Z-ADD     *ZEROS        I                 4 0
378500970313     C*
378600011128     C                   CLEAR                   DEPORP
378700060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
378800990820     C     KEYTAD        SETLL     TITAD04L                               99
378900990820     C     KEYTAD        READE     TITAD04L                               99
379000060804X   1C                   ELSE                                                   *OFFERTA
379100060804     C     KEYTAD        SETLL     TIOFD01L                               99
379200060804     C     KEYTAD        READE     TIOFD01L                               99
379300060804E   1C                   ENDIF
379400011128     C                   EVAL      DEPORP = TADORP                              *SOLO un cts
379500011010DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
379600011128     C     TADORP        ANDEQ     DEPORP                                       *ROTTURA ORDINAMENTO
379700011010IF  2C     TADATB        IFEQ      *BLANKS
379800011010     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
379900011010     C     TADSGL        LOOKUP    SSGL                                   98
380000011010IF  3C     *IN98         IFEQ      *OFF
380100030122     C                   IF        I < 10
380200011010     C                   ADD       1             I
380300011010     C                   Z-ADD     TADSGL        SSGL(I)
380400030122     C                   ELSE
380500030122     C                   LEAVE
380600030122     C                   ENDIF
380700011010E   3C                   ENDIF
380800011010E   2C                   ENDIF
380900060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
381000990820     C     KEYTAD        READE     TITAD04L                               99
381100060804X   2C                   ELSE                                                   *OFFERTA
381200060804     C     KEYTAD        READE     TIOFD01L                               99
381300060804E   2C                   ENDIF
381400970313E   1C                   ENDDO                                                  *FINE DETTAGLIO
381500970313     C*
381600970313     C                   ENDSR
381700970314     C*------------------------------------------------------------------------*
381800970314     C* CARCTS   CARICA I CODICI TASSAZIONE
381900970314     C*------------------------------------------------------------------------*
382000970314     C     CARCTS        BEGSR
382100970314     C*
382200970314     C* AZZERA LE VARIABILI DI LAVORO
382300970314     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
382400970314     C                   Z-ADD     *ZEROS        I                 4 0
382500970317     C                   CLEAR                   WCTS
382600970317     C                   CLEAR                   WRAP
382700970317     C                   CLEAR                   WNAR
382800970317     C                   CLEAR                   WTCT
382900970314     C*
383000970314     C* MEMORIZZA I CODICI DELLA LINEA DI PARTENZA RICHIESTA
383100060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
383200990820     C     KEYTAD        SETLL     TITAD04L                               99
383300990820     C     KEYTAD        READE     TITAD04L                               99
383400060804X   1C                   ELSE                                                   *OFFERTA
383500060804     C     KEYTAD        SETLL     TIOFD01L                               99
383600060804     C     KEYTAD        READE     TIOFD01L                               99
383700060804E   1C                   ENDIF
383800970314DO  1C     *IN99         DOWEQ     *OFF
383900970314IF  2C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
384000970314     C                   SETOFF                                           98
384100970314     C                   Z-ADD     1             J                 4 0
384200970314     C     TADCTS        LOOKUP    CTS(J)                                 98
384300970314IF  3C     *IN98         IFEQ      *ON                                          *TROVATO
384400970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
384500970417     C     CIE(J)        ANDEQ     'I'                                          *CODICI ITALIA
384600970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
384700971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
384800970417     C     CIE(J)        ANDEQ     'E'                                          *CODICI ESTERO
384900970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
385000971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ITALIA
385100970314     C                   SETOFF                                           98
385200970314     C                   Z-ADD     1             K                 4 0
385300970314     C     TADCTS        LOOKUP    WCTS(K)                                98
385400970417IF  5C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
385500970417     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
385600970314     C                   ADD       1             I
385700970314     C                   MOVEL     TADCTS        WCTS(I)                        *CODICE
385800970314     C                   MOVEL     RAP(J)        WRAP(I)                        *REGIONE
385900970314     C                   MOVEL     NAR(J)        WNAR(I)                        *NAZIONE
386000970314     C                   MOVEL     TCT(J)        WTCT(I)                        *TIPO
386100970417E   5C                   ENDIF
386200970417E   4C                   ENDIF
386300970314E   3C                   ENDIF
386400970314E   2C                   ENDIF
386500060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
386600990820     C     KEYTAD        READE     TITAD04L                               99
386700060804X   2C                   ELSE                                                   *OFFERTA
386800060804     C     KEYTAD        READE     TIOFD01L                               99
386900060804E   2C                   ENDIF
387000970314E   1C                   ENDDO
387100970314     C*
387200970314     C                   ENDSR
387300970421     C*------------------------------------------------------------------------*
387400970421     C* FIXCTS - IMPOSTA CODICI TASSAZIONE FISSI
387500970421     C*------------------------------------------------------------------------*
387600970421     C     FIXCTS        BEGSR
387700970421     C*
387800970421     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
387900970421     C*
388000970421     C                   Z-ADD     *ZEROS        J                 4 0
388100050103DO  1C     1             DO        600           I
388200970421IF  2C     CTS(I)        IFNE      *BLANKS
388300970421IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
388400970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
388500970421     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
388600970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
388700971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
388800970421     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
388900970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
389000971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
389100970421     C                   ADD       1             J
389200970421     C                   MOVEL     CTS(I)        WCTS(J)                        *CODICE
389300970421     C                   MOVEL     RAP(I)        WRAP(J)                        *REGIONE
389400970421     C                   MOVEL     NAR(I)        WNAR(J)                        *NAZIONE
389500970421     C                   MOVEL     TCT(I)        WTCT(J)                        *TIPO
389600970421E   4C                   ENDIF
389700970421E   3C                   ENDIF
389800970421E   2C                   ENDIF
389900970421E   1C                   ENDDO
390000970421     C*
390100970421     C                   ENDSR
390200970314     C*------------------------------------------------------------------------*
390300970314     C* RAPSVI - TRASFORMA I CODICI DELLA TARIFFA IN CODICI ELEMENTARI
390400970314     C*------------------------------------------------------------------------*
390500970314     C     RAPSVI        BEGSR
390600970314     C*
390700970314     C* 1°GIRO-> CARICA TUTTI I CODICI ELEMENTARI COME -NON CALCOLARE-
390800970314     C                   Z-ADD     *ZEROS        J                 4 0
390900050103DO  1C     1             DO        600           I
391000970416IF  2C     CTS(I)        IFNE      *BLANKS
391100970416IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
391200970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
391300970417     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
391400970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
391500971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
391600970417     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
391700970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
391800971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
391900970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
392000970314     C                   ADD       1             J
392100970423     C                   MOVEL     CTS(I)        CCTI(J)                        *CODICE
392200970423     C                   MOVEL     *BLANKS       CRCI(J)                        *RAGGRUPPAMENTO
392300970423X   5C                   ELSE                                                   *ESTERO
392400970423     C                   ADD       1             J
392500970423     C                   MOVEL     CTS(I)        CCTE(J)                        *CODICE
392600970423     C                   MOVEL     *BLANKS       CRCE(J)                        *RAGGRUPPAMENTO
392700970423E   5C                   ENDIF
392800970423E   4C                   ENDIF
392900970417E   3C                   ENDIF
393000970314E   2C                   ENDIF
393100970314E   1C                   ENDDO                                                  *FINE CODICI BASE
393200970314     C*
393300970314     C* 2°GIRO-> TRASFORMA I -NON CALCOLARE- IN -CALCOLARE IN NAZIONE-
393400050103DO  1C     1             DO        600           I
393500970314IF  2C     WCTS(I)       IFNE      *BLANKS
393600970314IF  3C     WTCT(I)       IFEQ      'N'                                          *SOLO NAZIONE
393700050103DO  4C     1             DO        600           J
393800970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
393900970423IF  6C     CCTI(J)       IFNE      *BLANKS
394000970314     C                   SETOFF                                           98
394100970314     C                   Z-ADD     1             K                 4 0
394200970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
394300970423IF  7C     WNAR(I)       IFEQ      NAR(K)
394400970423     C                   MOVEL     'N'           CRCI(J)
394500970423E   7C                   ENDIF
394600970423E   6C                   ENDIF
394700970423X   5C                   ELSE                                                   *ESTERO
394800970423IF  6C     CCTE(J)       IFNE      *BLANKS
394900970423     C                   SETOFF                                           98
395000970423     C                   Z-ADD     1             K                 4 0
395100970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
395200970423IF  7C     WNAR(I)       IFEQ      NAR(K)
395300970423     C                   MOVEL     'N'           CRCE(J)
395400970423E   7C                   ENDIF
395500970423E   6C                   ENDIF
395600970423E   5C                   ENDIF
395700970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
395800970314E   3C                   ENDIF
395900970314E   2C                   ENDIF
396000970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
396100970314     C*
396200970314     C* 3°GIRO-> TRASFORMA I -CALCOLARE IN NAZIONE- IN -CALCOLARE IN REGIONE-
396300050103DO  1C     1             DO        600           I
396400970314IF  2C     WCTS(I)       IFNE      *BLANKS
396500970314IF  3C     WTCT(I)       IFEQ      'R'                                          *SOLO REGIONE
396600050103DO  4C     1             DO        600           J
396700970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
396800970423IF  6C     CCTI(J)       IFNE      *BLANKS
396900970314     C                   SETOFF                                           98
397000970314     C                   Z-ADD     1             K                 4 0
397100970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
397200970423IF  7C     WRAP(I)       IFEQ      RAP(K)
397300970423     C                   MOVEL     'R'           CRCI(J)
397400970423E   7C                   ENDIF
397500970423E   6C                   ENDIF
397600970423X   5C                   ELSE                                                   *ESTERO
397700970423IF  6C     CCTE(J)       IFNE      *BLANKS
397800970423     C                   SETOFF                                           98
397900970423     C                   Z-ADD     1             K                 4 0
398000970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
398100970423IF  7C     WRAP(I)       IFEQ      RAP(K)
398200970423     C                   MOVEL     'R'           CRCE(J)
398300970423E   7C                   ENDIF
398400970423E   6C                   ENDIF
398500970314E   5C                   ENDIF
398600970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
398700970314E   3C                   ENDIF
398800970314E   2C                   ENDIF
398900970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
399000970314     C*
399100970314     C* 4°GIRO-> TRASFORMA I -CALCOLARE IN REGIONE- IN -CALCOLARE-
399200050103DO  1C     1             DO        600           I
399300970314IF  2C     WCTS(I)       IFNE      *BLANKS
399400970314IF  3C     WTCT(I)       IFEQ      'E'                                          *SOLO ELEMENTARI
399500050103DO  4C     1             DO        600           J
399600970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
399700970423IF  6C     CCTI(J)       IFNE      *BLANKS
399800970423IF  7C     WCTS(I)       IFEQ      CCTI(J)
399900970423     C                   MOVEL     'T'           CRCI(J)
400000970423E   7C                   ENDIF
400100970423E   6C                   ENDIF
400200970423X   5C                   ELSE                                                   *ESTERO
400300970423IF  6C     CCTE(J)       IFNE      *BLANKS
400400970423IF  7C     WCTS(I)       IFEQ      CCTE(J)
400500970423     C                   MOVEL     'T'           CRCE(J)
400600970423E   7C                   ENDIF
400700970423E   6C                   ENDIF
400800970423E   5C                   ENDIF
400900970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
401000970314E   3C                   ENDIF
401100970314E   2C                   ENDIF
401200970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
401300970314     C*
401400970314     C                   ENDSR
401500981218     C*------------------------------------------------------------------------*
401600981218     C* CANTAR - ANNULLAMENTO TARIFFA
401700981218     C*------------------------------------------------------------------------*
401800981218     C     CANTAR        BEGSR
401900981218     C*
402000981218     C* CANCELLA DATI TIPO - GENERALI
402100981218     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE
402200060804     C                   MOVEL     parcto        KTCCTO                         *DATI TARIFFA
402300981218     C                   Z-ADD     PARKOV        KTCKSC                         *CLIENTE
402400011001     C     KE3ETC        SETLL     TEETC01L
402500011001     C     KE3ETC        READE     TEETC01L                               99
402600981218DO  1C     *IN99         DOWEQ     *OFF
402700981218IF  2C     PARCTR        IFEQ      ETCCTR                                       *TARIFFA RICHIESTA
402800981218IF  3C     PARPRG        IFEQ      ETCPRG                                       *PROGRESSIVO RICHIES
402900981218IF  4C     PARLNP        IFEQ      ETCLNP                                       *LINEA RICHIESTA
403000981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
403100981218     C                   MOVEL     'A'           ETCATB                         *ANNULLAMENTO
403200981218     C                   MOVEL     *BLANKS       ETCFTR                         *FLAG TRASMISSIONE
403300011128     C*---               Z-ADD     *ZEROS        ETCDTR                         *DATA TRASMISSIONE
403400011001     C                   UPDATE    TEETC000                                     *AGGIORNA
403500981218E   4C                   ENDIF
403600981218E   3C                   ENDIF
403700981218E   2C                   ENDIF
403800011001     C     KE3ETC        READE     TEETC01L                               99
403900981218E   1C                   ENDDO                                                  *FINE DATI GENERALI
404000981218     C*
404100981218     C* CANCELLA DATI TIPO - SCAGLIONI
404200060829     c* ricavi
404300981218     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE
404400060804     C                   MOVEL     parcto        KTSCTO                         *DATI TARIFFA
404500981218     C                   Z-ADD     PARKOV        KTSKSC                         *CLIENTE
404600011001     C     KE3ETS        SETLL     TEETS01L
404700011001     C     KE3ETS        READE     TEETS01L                               99
404800981218DO  1C     *IN99         DOWEQ     *OFF
404900981218IF  2C     PARCTR        IFEQ      ETSCTR                                       *TARIFFA RICHIESTA
405000981218IF  3C     PARPRG        IFEQ      ETSPRG                                       *PROGRESSIVO RICHIES
405100981218IF  4C     PARLNP        IFEQ      ETSLNP                                       *LINEA RICHIESTA
405200981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
405300981218     C                   MOVEL     'A'           ETSATB                         *ANNULLAMENTO
405400981218     C                   MOVEL     *BLANKS       ETSFTR                         *FLAG TRASMISSIONE
405500011001     C                   UPDATE    TEETS000                                     *AGGIORNA
405600981218E   4C                   ENDIF
405700981218E   3C                   ENDIF
405800981218E   2C                   ENDIF
405900011001     C     KE3ETS        READE     TEETS01L                               99
406000981218E   1C                   ENDDO                                                  *FINE SCAGLIONI
406100060829     c* competetnze
406200060829     C     KE3ETS        SETLL     TEETS11L
406300060829     C     KE3ETS        READE     TEETS11L                               99
406400060829DO  1C     *IN99         DOWEQ     *OFF
406500060829IF  2C     PARCTR        IFEQ      £ETSCTR                                      *TARIFFA RICHIESTA
406600060829IF  3C     PARPRG        IFEQ      £ETSPRG                                      *PROGRESSIVO RICHIES
406700060829IF  4C     PARLNP        IFEQ      £ETSLNP                                      *LINEA RICHIESTA
406800060829     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
406900060829     C                   MOVEL     'A'           £ETSATB                        *ANNULLAMENTO
407000060829     C                   MOVEL     *BLANKS       £ETSFTR                        *FLAG TRASMISSIONE
407100060829     C                   UPDATE    TEETS100                                     *AGGIORNA
407200060829E   4C                   ENDIF
407300060829E   3C                   ENDIF
407400060829E   2C                   ENDIF
407500060829     C     KE3ETS        READE     TEETS11L                               99
407600060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
407700981218     C*
407800981218     C* CANCELLA DATI TIPO - CODICI TASSAZIONE
407900981218     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE
408000060804     C                   MOVEL     parcto        KTTCTO                         *DATI TARIFFA
408100981218     C                   Z-ADD     PARKOV        KTTKSC                         *CLIENTE
408200011001     C     KE3ETT        SETLL     TEETT01L
408300011001     C     KE3ETT        READE     TEETT01L                               99
408400981218DO  1C     *IN99         DOWEQ     *OFF
408500981218IF  2C     PARCTR        IFEQ      ETTCTR                                       *TARIFFA RICHIESTA
408600981218IF  3C     PARPRG        IFEQ      ETTPRG                                       *PROGRESSIVO RICHIES
408700981218IF  4C     PARLNP        IFEQ      ETTLNP                                       *LINEA RICHIESTA
408800981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
408900981218IF  5C     PARFIE        IFEQ      ETTFIE                                       *ITA O EST RICHIESTO
409000981218     C     PARFIE        OREQ      *BLANKS                                      *TUTTO ITA E EST
409100981218     C                   MOVEL     'A'           ETTATB                         *ANNULLAMENTO
409200981218     C                   MOVEL     *BLANKS       ETTFTR                         *FLAG TRASMISSIONE
409300011001     C                   UPDATE    TEETT000                                     *AGGIORNA
409400981218E   5C                   ENDIF
409500981218E   4C                   ENDIF
409600981218E   3C                   ENDIF
409700981218E   2C                   ENDIF
409800011001     C     KE3ETT        READE     TEETT01L                               99
409900981218E   1C                   ENDDO                                                  *FINE CODICI TASSAZI
410000981218     C*
410100981218     C                   ENDSR
410200020923     C*--------------------------------------------------------------------------------------------*
410300020923     C* REPERISCE I DATI UTENTE
410400020923     C*--------------------------------------------------------------------------------------------*
410500020923     C     REPDATIUTE    BEGSR
410600020923     C*
410700020923     C* INIZIALIZZA VARIABILI DI WRK
410800020923     C                   CLEAR                   TIBS34DS
410900020923     C                   CLEAR                   AZUTEDS
411000020923     C                   CLEAR                   DDATIUTE
411100020923     C*
411200020923     C     *DTAARA       DEFINE    §azute        azuteds
411300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
411400020923     C                   IN(E)     *DTAARA
411500020923if  1C                   IF        %Error
411600020923     c                   EVAL      I34Tla = 'L'
411700020923     C                   CALL      'TIBS34R'
411800020923     C                   PARM                    Tibs34Ds
411900020923     C                   IN        *DTAARA
412000020923e   1C                   ENDIF
412100020923     C*
412200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
412300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
412400020923     C*
412500020923     C                   ENDSR
412600021031     C*------------------------------------------------------------------------*
412700021031     C* REPCLI - CARICAMENTO LISTA CLIENTI PADRI/FIGLI
412800021031     C*------------------------------------------------------------------------*
412900021031     C     REPCLI        BEGSR
413000021031     C*
413100021031     C* SE RICHIESTA ELABORAZIONE X CLIENTE UNIFICANTE REPERISCO LA LISTA DEI FIGLI
413200021031     C                   IF        PARUNI = 'S'
413300021031     C                   CLEAR                   TIBS10DS
413400021031     C                   EVAL      D10DRF = DATCOR                              *data riferimento
413500021031     C                   EVAL      D10TLE = 'ST'                                *tipo legame
413600021031     C                   EVAL      D10PAF = 'F'                                 *cerca il padre
413700021031     C                   Z-ADD     PARKSC        D10COD                         *cod cliente da tst
413800021031     C                   CALL      'TIBS10R'
413900021031     C                   PARM                    TIBS10DS
414000021031     C*
414100021031     C                   IF        D10ERR <> *blanks OR                         *No unif. o padre
414200021031     C                             D10COP =  *zeros
414300021031     C                   CLEAR                   SKCLI
414400021031     C                   Z-ADD     PARKSC        SKCLI(1)
414500021031     C                   ENDIF
414600021031     C*
414700021031     C                   ELSE
414800021031     C                   CLEAR                   SKCLI
414900021031     C                   Z-ADD     PARKSC        SKCLI(1)
415000021031     C                   ENDIF
415100021031     C*
415200021031     C                   ENDSR
415300970110     C*------------------------------------------------------------------------*
415400970110     C* *INZSR - OPERAZIONI INIZIALI
415500970110     C*------------------------------------------------------------------------*
415600970110     C     *INZSR        BEGSR
415700970110     C*--------------------
415800970110     C* RICEVIMENTO PARAMETRI
415900970110     C*--------------------
416000970110     C     *ENTRY        PLIST
416100970110     C                   PARM                    KPJBA
416200970110     C                   MOVEL     KPJBU         PARAM
416300021121     C*
416400021121     C* CARICA NUOVE TABELLE OCCORRENTI
416500021121     C                   EXSR      CARGEDCE
416600021121     C*
416700021121     C* REPERISCO SUBITO LA DIVISA DELLA TARIFFA RICEVUTA IN INPUT
416800021121     C                   IF        PARDIV = *BLANKS
416900021121     C                   Z-ADD     PARKOV        KTMKSC
417000021121     C                   Z-ADD     PARCTR        KTMCTR
417100021121     C                   Z-ADD     PARPRG        KTMPRG
417200021121     C     KEYTAMUNIQ    CHAIN     TNTAM01L
417300021121     C                   IF        %FOUND
417400021121     C                   MOVEL(P)  TAMFLO        DSTA01
417500021121     C                   MOVEL(P)  §TADIV        PARDIV
417600021121     C                   ELSE
417700021121     C                   MOVEL(P)  WDIVCE        PARDIV
417800021121     C                   ENDIF
417900021121     C                   ENDIF
418000970110     C*--------------------
418100970110     C* VARIABILI RIFERITE AL DATABASE
418200970110     C*--------------------
418300970110     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
418400970110     C     *LIKE         DEFINE    TBLCOD        KTBCOD
418500970110     C     *LIKE         DEFINE    TBLKEY        KTBKEY
418600970110     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
418700970110     C     *LIKE         DEFINE    ACOKCC        KACKCC
418800970110     C     *LIKE         DEFINE    ACOKSC        KACKSC
418900970110     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
419000970110     C     *LIKE         DEFINE    CLPKCC        KCLKCC
419100970110     C     *LIKE         DEFINE    CLPKSC        KCLKSC
419200970110     C     *LIKE         DEFINE    INDKUT        KINKUT                         *CNIND00F
419300970110     C     *LIKE         DEFINE    INDKCC        KINKCC
419400970415     C     *LIKE         DEFINE    INDKSC        KINKSC
419500970110     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
419600970110     C     *LIKE         DEFINE    TAMCTR        KTMCTR
419700021121     C     *LIKE         DEFINE    TAMPRG        KTMPRG
419800990820     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
419900970110     C     *LIKE         DEFINE    TADCTR        KTDCTR
420000970110     C     *LIKE         DEFINE    TADPRG        KTDPRG
420100970317     C     *LIKE         DEFINE    TADLNP        KTDLNP
420200990812     C     *LIKE         DEFINE    TASKSC        KTAKSC                         *TITAS00F
420300970110     C     *LIKE         DEFINE    TASAAS        KTAAAS
420400970110     C     *LIKE         DEFINE    TASMGS        KTAMGS
420500970110     C     *LIKE         DEFINE    CSBAAS        KSBAAS                         *TNCSB00F
420600970110     C     *LIKE         DEFINE    CSBLNP        KSBLNP
420700970110     C     *LIKE         DEFINE    CSBNRS        KSBNRS
420800970110     C     *LIKE         DEFINE    CSBNSP        KSBNSP
420900011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
421000980713     C     *LIKE         DEFINE    ETCCTO        KTCCTO
421100970110     C     *LIKE         DEFINE    ETCKSC        KTCKSC
421200970110     C     *LIKE         DEFINE    ETCCTR        KTCCTR
421300970110     C     *LIKE         DEFINE    ETCPRG        KTCPRG
421400970110     C     *LIKE         DEFINE    ETCLNP        KTCLNP
421500011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
421600980713     C     *LIKE         DEFINE    ETSCTO        KTSCTO
421700970110     C     *LIKE         DEFINE    ETSKSC        KTSKSC
421800970110     C     *LIKE         DEFINE    ETSCTR        KTSCTR
421900970110     C     *LIKE         DEFINE    ETSPRG        KTSPRG
422000970110     C     *LIKE         DEFINE    ETSLNP        KTSLNP
422100970110     C     *LIKE         DEFINE    ETSSGL        KTSSGL
422200011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
422300980713     C     *LIKE         DEFINE    ETTCTO        KTTCTO
422400970110     C     *LIKE         DEFINE    ETTKSC        KTTKSC
422500970110     C     *LIKE         DEFINE    ETTCTR        KTTCTR
422600970110     C     *LIKE         DEFINE    ETTPRG        KTTPRG
422700970110     C     *LIKE         DEFINE    ETTLNP        KTTLNP
422800970110     C     *LIKE         DEFINE    ETTFIE        KTTFIE
422900970110     C     *LIKE         DEFINE    ETTCTS        KTTCTS
423000140922     C     *LIKE         DEFINE    £ETTntw       KTTntw
423100140922     C     *LIKE         DEFINE    £ETTnaz       KTTnaz
423200140922     C     *LIKE         DEFINE    £ETTcad       KTTcad
423300970110     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DEPOSITI
423400970110     C     *LIKE         DEFINE    TAMCTR        CARCTR
423500970110     C     *LIKE         DEFINE    TAMPRG        CARPRG
423600970430     C     *LIKE         DEFINE    TASLNP        DEPFIL
423700980713     C     *LIKE         DEFINE    PARCTO        DEPCTO
423800020730     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
423900011128     C     *LIKE         DEFINE    TADORP        DEPORP
424000140922     C     *LIKE         DEFINE    £ETtntw       Xntw                           *DI PASSAGGIO
424100140919     C     *LIKE         DEFINE    ETCLNP        XORG                           *DI PASSAGGIO
424200980821     C     *LIKE         DEFINE    ETTFIE        XFIE
424300011011     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
424400011011     C     *LIKE         DEFINE    TBEKE1        KTEKE1
424500050309     C     *LIKE         DEFINE    GCPAAS        KGPAAS                         *TIGCP00F
424600021122     C     *LIKE         DEFINE    GCPLNP        KGPLNP
424700021122     C     *LIKE         DEFINE    GCPNRS        KGPNRS
424800021122     C     *LIKE         DEFINE    GCPNSP        KGPNSP
424900980821     C*--------------------
425000970110     C* CHAIVI DI LETTURA
425100970110     C*--------------------
425200060315     C* CHIAVE LETTURA TITA730C - COMPLETA
425300060315     C     KEYTA7        KLIST
425400060315     C                   KFLD                    TA7AAS
425500060315     C                   KFLD                    TA7LNP
425600060315     C                   KFLD                    TA7NRS
425700060315     C                   KFLD                    TA7NSP
425800060315     C                   KFLD                    TA7TBL
425900970110     C* LETTURA TABEL00F - COMPLETA
426000970110     C     KEYTAB        KLIST
426100970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
426200970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
426300970110     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
426400970110     C* LETTURA TABEL00F - PARZIALE
426500970110     C     KE2TAB        KLIST
426600970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
426700970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
426800970110     C* LETTURA CNACO00F - COMPETA
426900970110     C     KEYACO        KLIST
427000970110     C                   KFLD                    KACKUT                         *CODICE UTENTE
427100970110     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
427200970110     C                   KFLD                    KACKSC                         *CODICE CLIENTE
427300970110     C* LETTURA CNCLP00F - COMPLETA
427400970110     C     KEYCLP        KLIST
427500970110     C                   KFLD                    KCLKUT                         *CODICE UTENTE
427600970110     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
427700970110     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
427800970415     C* LETTURA CNIND00F - COMPLETA
427900970415     C     KEYIND        KLIST
428000970415     C                   KFLD                    KINKUT                         *CODICE UTENTE
428100970415     C                   KFLD                    KINKCC                         *CODICE CAPOCONTO
428200970415     C                   KFLD                    KINKSC                         *CODICE CLIENTE
428300970110     C* LETTURA TNTAM01L - PARZIALE
428400970110     C     KEYTAM        KLIST
428500970110     C                   KFLD                    KTMKSC                         *CLIENTE
428600970110     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
428700021121     C* LETTURA TNTAM01L - PARZIALE
428800021121     C     KEYTAMUNIQ    KLIST
428900021121     C                   KFLD                    KTMKSC                         *CLIENTE
429000021121     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
429100021121     C                   KFLD                    KTMPRG                         *PROGRESSIVO
429200990820     C* LETTURA TITAD01L - PARZIALE
429300970110     C     KEYTAD        KLIST
429400970110     C                   KFLD                    KTDKSC                         *CLIENTE
429500970110     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
429600970110     C                   KFLD                    KTDPRG                         *PROGRESSIVO
429700970317     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
429800990812     C* LETTURA TITAS31C - PARZIALE
429900970110     C     K31TAS        KLIST
430000970110     C                   KFLD                    KTAKSC                         *CLIENTE
430100990812     C* LETTURA TITAS38C - COMPLETA
430200970110     C     K38TAS        KLIST
430300970110     C                   KFLD                    KTAAAS                         *ANNO
430400970110     C                   KFLD                    KTAMGS                         *MESE/GIORNO
430500970110     C                   KFLD                    KTAKSC                         *CLIENTE
430600970110     C* LETTURA TNCSB03L - COMPLETA
430700970110     C     KEYCSB        KLIST
430800970110     C                   KFLD                    KSBAAS                         *ANNO
430900970110     C                   KFLD                    KSBLNP                         *LINEA PARTENZA
431000970110     C                   KFLD                    KSBNRS                         *SERIE
431100970110     C                   KFLD                    KSBNSP                         *SPEDIZIONE
431200011001     C* LETTURA TEETC01L - COMPLETA
431300981218     C     KEYETC        KLIST
431400980713     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
431500980713     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
431600970110     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
431700970110     C                   KFLD                    KTCCTR                         *TARIFFA
431800970110     C                   KFLD                    KTCPRG                         *PROGRESSIVO
431900970110     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
432000011001     C* LETTURA TEETC01L - PARZIALE
432100981218     C     KE3ETC        KLIST
432200981218     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
432300981218     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
432400981218     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
432500011001     C* LETTURA TEETS01L - COMPLETA
432600970110     C     KEYETS        KLIST
432700980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
432800970110     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
432900970110     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
433000970110     C                   KFLD                    KTSCTR                         *TARIFFA
433100970110     C                   KFLD                    KTSPRG                         *PROGRESSIVO
433200970110     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
433300970417     C                   KFLD                    KTSSGL                         *SCAGLIONE
433400011001     C* LETTURA TEETS01L - PARZIALE
433500970314     C     KE2ETS        KLIST
433600980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
433700970314     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
433800970314     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
433900970314     C                   KFLD                    KTSCTR                         *TARIFFA
434000970314     C                   KFLD                    KTSPRG                         *PROGRESSIVO
434100970314     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
434200011001     C* LETTURA TEETS01L - PARZIALE
434300981218     C     KE3ETS        KLIST
434400981218     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
434500981218     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
434600981218     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
434700011001     C* LETTURA TEETT01L - COMPLETA
434800970110     C     KEYETT        KLIST
434900980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
435000970110     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
435100970110     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
435200970110     C                   KFLD                    KTTCTR                         *TARIFFA
435300970110     C                   KFLD                    KTTPRG                         *PROGRESSIVO
435400970110     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
435500970110     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
435600970110     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
435700140919     C* LETTURA TEETT11L - COMPLETA
435800140919     C     KEYETT1       KLIST
435900140919     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
436000140919     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
436100140919     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
436200140919     C                   KFLD                    KTTCTR                         *TARIFFA
436300140919     C                   KFLD                    KTTPRG                         *PROGRESSIVO
436400140919     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
436500140919     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
436600140919     C                   KFLD                    KTTntw                         *ITALIA/ESTERO
436700140922     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
436800140922     C                   KFLD                    KTTnaz                         *CODICE TASSAZIONE
436900140922     C                   KFLD                    KTTCad                         *CODICE TASSAZIONE
437000140922     C     KEYETT2       KLIST
437100140922     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
437200140922     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
437300140922     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
437400140922     C                   KFLD                    KTTCTR                         *TARIFFA
437500140922     C                   KFLD                    KTTPRG                         *PROGRESSIVO
437600140922     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
437700140922     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
437800140922     C                   KFLD                    KTTntw                         *ITALIA/ESTERO
437900140922     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
438000140922     C* LETTURA TEETT11L - PARZIALE
438100140922     C     KE2ETT1       KLIST
438200140922     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
438300140922     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
438400140922     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
438500140922     C                   KFLD                    KTTCTR                         *TARIFFA
438600140922     C                   KFLD                    KTTPRG                         *PROGRESSIVO
438700140922     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
438800140922     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
438900140922     C                   KFLD                    KTTntw                         *ITALIA/ESTERO
439000140922     C* LETTURA TEETT01L - PARZIALE
439100970314     C     KE2ETT        KLIST
439200980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
439300970314     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
439400970314     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
439500970314     C                   KFLD                    KTTCTR                         *TARIFFA
439600970314     C                   KFLD                    KTTPRG                         *PROGRESSIVO
439700970314     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
439800970314     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
439900040809     C* LETTURA fiar531c
440000040809     C     Kar5          KLIST
440100040809     C                   KFLD                    ar5aas                         *DATI SEDE O FILIALE
440200040809     C                   KFLD                    ar5lnp                         *TARIFFA O OFFERTA
440300040809     C                   KFLD                    ar5nrs                         *CLIENTE O VISITA
440400040809     C                   KFLD                    ar5nsp                         *CLIENTE O VISITA
440500040809     C                   KFLD                    ar5trd                         *CLIENTE O VISITA
440600040809     C* LETTURA TEETT01L - PARZIALE
440700040809     C     KE3ETT        KLIST
440800040809     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
440900040809     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
441000040809     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
441100011011     C* LETTURA TNTBE01L - PARZIALE
441200011011     C     KEYTBE        KLIST
441300011011     C                   KFLD                    KTECOD                         *UTENTE
441400011011     C                   KFLD                    KTEKE1                         *CODICE
441500050309     C* LETTURA TIGCP51L - PARZIALE
441600021122     C     K01GCP        KLIST
441700021122     C                   KFLD                    KGPAAS                         *ANNO
441800021122     C                   KFLD                    KGPLNP                         *LINEA DI PARTENZA
441900021122     C                   KFLD                    KGPNRS                         *SERIE
442000021122     C                   KFLD                    KGPNSP                         *SPEDIZIONE
442100970110     C*--------------------
442200970110     C* VARIABILI DI CONTROLLO
442300970110     C*--------------------
442400970110     C                   MOVEL     '0'           $FINE             1            *FINE PGM
442500970110     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
442600970416     C                   MOVEL     'S'           $OK               1            *RECORD OK
442700970312     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO TROVATO
442800970313     C                   MOVEL     '0'           $ERR              1            *ERRORE
442900970313     C                   MOVEL     '1'           *INOF                          *OVERFLOW QSYSPRT
443000970313     C                   MOVEL     '1'           *IN01                          *OVERFLOW TNTE41P
443100970110     C*
443200970110     C* REPERISCE I DATI DELL'UTENTE
443300020923     C                   EXSR      REPDATIUTE
443400970110     C*
443500970110     C* REPERISCE LA DATA CORRENTE
443600970110     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
443700970110     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
443800970110     C                   Z-ADD     WN8           G08DAT
443900970110     C                   Z-ADD     *ZEROS        G08INV
444000970110     C                   MOVEL     '0'           G08ERR
444100970110     C                   CALL      'XSRDA8'
444200970110     C                   PARM                    WLBDA8
444300970110     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
444400970110     C*
444500970110     C* CARICA TABELLE OCCORRENTI
444600970110     C                   EXSR      CARTAB
444700970313     C*
444800970313     C* CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
444900970313     C                   EXSR      CARTAR
445000981218     C*
445100981218     C* ANNULLAMENTO TARIFFA
445200981218     C                   EXSR      CANTAR
445300021031     C*
445400021031     C* REPERIMENTO LISTA CLIENTI PADRI/FIGLI
445500021031     C                   EXSR      REPCLI
445600970110     C*
445700970110     C                   ENDSR
445800970110     O*------------------------------------------------------------------------*
445900970110     OQSYSPRT   E            TES              02
446000970110     O                       STA(1)              66
446100970110     O                       STA(2)             132
446200970110     O                       DSFIRS              20
446300970110     O                       UDATE              117 '  /  /  '
446400970110     O                                          127 'Pag.'
446500970110     O                       PAGE          Z    132
446600970110     O*------------------------
446700970110     O          E            ETAS        1
446800970110     O                       STA(3)              66
446900970110     O                       STA(4)             132
447000970423     O                       TASAAS        Z     16
447100970423     O                       TASLNP        Z     20
447200970423     O                       TASNRS        Z     23
447300970423     O                       TASNSP        Z     31
447400970423     O                       TASTBL              34
447500970423     O                       TASCTS              43
447600970110     O                       DEPERR             124
447700970110     O*------------------------
447800970110     O          E            EETX        1
447900970110     O                       STA(3)              66
448000970110     O                       STA(5)             132
448100970110     O                       PARKOV        Z     15
448200970110     O                       PARCTR              24
448300970110     O                       PARPRG              33
448400970110     O                       PARLNP        Z     53
448500970110     O                       PARFIE              62
448600970110     O                       DEPERR             124
448700970110     O*------------------------------------------------------------------------*
448800970110** -DDEC-
448900970110NON CALCOLARE                 1
449000970110CALCOLARE                     2
449100970110CALCOLARE IN REGIONE          3
449200970110CALCOLARE IN NAZIONE          4
449300970110 **  CREAZIONE DATI  TIPO  ** 5
449400980821TUTTA L'ITALIA                6
449500980821TUTTO L'ESTERO                7
4496009808218
4497009808219
44980098082110
449900990812** MTAS - MESSAGGI PER TITAS
450000970110FILIALE BOLLA NON TROVATA                                         1
450100970423CODICE TASSAZIONE NON TROVATO O NON VALIDO                        2
4502009704163
4503009701104
4504009701105
450500011001** METX - MESSAGGI PER TEETX
450600970110DATI TIPO ALLOCATI                                                1
450700971008NESSUNA SPEDIZIONE: VALORE DEDOTTO                                2
450800980901CREAZIONE AUTOMATICA                                              3
4509009701104
4510009701105
451100970110** STA
451200970110XXXXXXXXXXXXXXXXXXXX        TNTE41R       ** ERRORI IN CREAZIONE D1
451300970110ATI TIPO TARIFFE DA CONTO ECONOMICO **     99/99/99      PAG. 99992
451400970423SPEDIZIONE: 9999 999 99 9999999 XX  CTS: XX                       3
451500970423ERRORE: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        4
451600970110CLIENTE 9999999  TAR 999  PRG 999  LINEA PARTENZA 999   I/E  X    5
