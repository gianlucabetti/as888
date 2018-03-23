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
004100060808     FTIOFD01L  IF   E           K DISK    rename(titad000:tiofd000)
004200970110     F*-----------
004300970110     F* DATI TIPO
004400970110     F*-----------
004500011001     FTEETC01L  UF A E           K DISK
004600011001     FTEETS01L  UF A E           K DISK
004700060830     FTEETS11L  UF A E           K DISK    rename(teets000:teets100)
004800060830     f                                     prefix(£)
004900011001     FTEETT01L  UF A E           K DISK
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
012700970110     D*------------------
012800970110     D* CODICI PAGAMENTO
012900970110     D*------------------
013000970110     D CDP             S              4    DIM(200)                             *CODICE PAGAMENTO
013100970110     D CRAT            S             30    DIM(200)                             *RATE DI PAGAMENTO
013200040920     D*------------------
013300040920     D* scaglioni standard
013400040920     D*------------------
013500040920     D sgl             S             13  3 DIM(10)                              *CODICI ELEMENTARI
013600970110     D*------------------
013700970110     D* CODICI TASSAZIONE
013800970110     D*------------------
013900050103     D CTS             S              2    DIM(600)                             *CODICI ELEMENTARI
014000050103     D FCR             S              1    DIM(600)                             *FLAG CAPOLUOGO
014100050103     D DCTS            S             20    DIM(600)                             *DESCRIZIONE
014200050103     D CIE             S              1    DIM(600)                             *FLAG ITALIA/ESTERO
014300050103     D RAP             S              2    DIM(600)                             *CODICE REGIONE
014400050103     D NAR             S              2    DIM(600)                             *CODICE NAZIONE -> P
014500050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)   -> P
014600050808     D*------------------
014700050808     D* cod. trattamento merce con packing list
014800050808     D*------------------
014900050808     D Cpkl            S              2    DIM(600)                             *CODICI ELEMENTARI
015000970314     D*------------------
015100970314     D* DATI TARIFFE
015200970314     D*------------------
015300971124     D* MULTIPLE                                         -500 FILIALI-
015400970110     D*-------------------
015500970110     D* SCHIERE DI MEMORIZZAZIONE
015600970110     D*-------------------
015700970110     D* NON MULTIPLE
015800060315     D SVN             S              1    DIM(20)
015900060315     D VAN             S             11  3 DIM(20)
016000971124     D SPE             S              9  0 DIM(500)                             *N°SPEDIZIONI ITA/ES
016100971124     D CLA             S              9  0 DIM(500)                             *CLIENTE TIPO A
016200971124     D CLB             S              9  0 DIM(500)                             *CLIENTE TIPO B
016300971124     D CLC             S              9  0 DIM(500)                             *CLIENTE TIPO C
016400971124     D BON             S              9  0 DIM(500)                             *BOLLETTAZIONE (' ')
016500971124     D BOA             S              9  0 DIM(500)                             *BOLLETTAZIONE (A)
016600971124     D BOB             S              9  0 DIM(500)                             *BOLLETTAZIONE (B)
016700971124     D NCL             S              9  0 DIM(500)                             *COLLI
016800971124     D PKB             S              9  1 DIM(500)                             *PESO
016900971124     D VLB             S              9  3 DIM(500)                             *VOLUME
017000060829     D NCL1            S              9  0 DIM(500)                             *COLLI
017100060829     D PKB1            S              9  1 DIM(500)                             *PESO
017200060829     D VLB1            S              9  3 DIM(500)                             *VOLUME
017300971124     D FRA             S              9  0 DIM(500)                             *FRANCHI
017400971124     D ASS             S              9  0 DIM(500)                             *ASSEGNATI
017500971124     D PCV             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
017600971124     D PCM             S              9  0 DIM(500)                             *CONSEGNE MAGAZZINO
017700971124     D PFD             S              9  0 DIM(500)                             *FERMO DEPOSITO
017800971124     D ESP             S              9  0 DIM(500)                             *ESPRESSI
017900971124     D PPV             S              9  0 DIM(500)                             *CONSEGNE PROVINCIA
018000971124     D PCA             S              9  0 DIM(500)                             *CONSEGNE APPUNTAMEN
018100971124     D PCP             S              9  0 DIM(500)                             *CONSEGNE PIANI
018200971124     D PCS             S              9  0 DIM(500)                             *CONSEGNE SUPERMERCA
018300971124     D PCF             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
018400971124     D PAB             S              9  0 DIM(500)                             *C/ASSEGNI BARTOLINI
018500971124     D PAM             S              9  0 DIM(500)                             *C/ASSEGNI MITTENTE
018600011026     D CAS             S             21  8 DIM(500)                             *C/ASSEGNO
018700971124     D PAR             S              9  0 DIM(500)                             *ASSICURAZIONE R.C.V
018800971124     D PAC             S              9  0 DIM(500)                             *ASSICURAZIONE X CON
018900060315     D xx1             S              9  0 DIM(500)                             *ASSICURAZIONE X CON
019000011026     D ASC             S             21  8 DIM(500)                             *ASSICURAZIONE
019100971124     D PGI             S              9  0 DIM(500)                             *GIACENZE
019200971124     D PCD             S              9  0 DIM(500)                             *CONSEGNE DDT
019300971124     D PCC             S              9  0 DIM(500)                             *CONSEGNE C.STORICI
019400971124     D PCL             S              9  0 DIM(500)                             *CONSEGNE L.DISAGIAT
019500980901     D CLT             S              9  0 DIM(500)                             *CLIENTE TIPO T
019600000414     D GGS             S              9  0 DIM(500)                             *N° GG SOSTA GIACENZ
019700000414     D NRG             S              9  0 DIM(500)                             *N° RIAPERTURE GIACE
019800011011     D CIS             S              9  0 DIM(500)                             *CONSEGNE ISOLE
019900011011     D RIS             S              9  0 DIM(500)                             *RITIRI ISOLE
020000020215     D CLD             S              9  0 DIM(500)                             *CLIENTE TIPO D
020100040802     D ded             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
020200040802     D val             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
020300040811     D sbnb            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020400040824     D sban            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020500040811     D nbn             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020600040802     D nba             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020700050808     D pks             S              9  0 DIM(500)                             *CLIENTE TIPO D
020800050808     D pkn             S              9  0 DIM(500)                             *CLIENTE TIPO D
020900040809     d dar5ban       e ds
021000040811     d dar5bnb       e ds
021100060829     d teets1        e ds                  extname(teets10f) prefix(£)
021200970417     D*-------------------
021300970417     D* SCHIERE DI LAVORO
021400970417     D*-------------------
021500050103     D WCTS            S              2    DIM(600)                             *CODICI
021600050103     D WRAP            S              2    DIM(600)                             *CODICE REGIONE
021700050103     D WNAR            S              2    DIM(600)                             *CODICE NAZIONE -> P
021800050103     D WTCT            S              1    DIM(600)                             *TIPO (E/R/N)   -> P
021900970416     D*-------------------
022000970416     D* SCHIERE DI PASSAGGIO PER LA SCRITTURA
022100970416     D*-------------------
022200970416     D* NON MULTIPLE
022300970416     D §SG             S             13  3 DIM(10)                              *SCAGLIONE -KG/COLLI
022400060829     c* ricavi
022500970416     D §PS             S              7  1 DIM(10)                              *PESO
022600970416     D §CS             S              7  1 DIM(10)                              *COLLI
022700970416     D §VS             S              7  3 DIM(10)                              *VOLUME
022800970416     D §SP             S              4  1 DIM(10)                              *% INCIDENZA
022900971008     D §SD             S              1    DIM(10)                              *FLAG VALORE DEDOTTO
023000060829     c* competenze
023100060829     D §PS1            S              7  1 DIM(10)                              *PESO
023200060829     D §CS1            S              7  1 DIM(10)                              *COLLI
023300060829     D §VS1            S              7  3 DIM(10)                              *VOLUME
023400060829     D §SP1            S              4  1 DIM(10)                              *% INCIDENZA
023500060829     D §SD1            S              1    DIM(10)                              *FLAG VALORE DEDOTTO
023600970416     D*---
023700050103     D §CT             S              2    DIM(600)                             *CODICE TASSAZIONE
023800050103     D §RC             S              1    DIM(600)                             *RAGGRUPPAMENTO
023900050103     D §CP             S              4  1 DIM(600)                             *% INCIDENZA
024000050103     D §CD             S              1    DIM(600)                             *FLAG VALORE DEDOTTO
024100020227     D*-------------------
024200020227     D* SCHIERA FILIALI FEDEX
024300020227     D*-------------------
024400020227     D FILFED          S              3  0 DIM(999)
024500081105     D*-------------------
024600081105     D* SCHIERA FILIALI DPD
024700081105     D*-------------------
024800081105     D FILdpd          S              3  0 DIM(999)
024900970110     D*------------------------------------------------------------------------*
025000970110     D* INPUT
025100970110     D*------------------------------------------------------------------------*
025200970110     D*------------------
025300970110     D* DS SCHIERE MULTIPLE
025400970110     D*------------------
025500970110     D*---
025600970110     D* BYTE IN PACKED ->
025700970317     D* (3:2) +1=2 ; (2X30) =60
025800970317     D* (9:2) +1=5 ; (5X30) =150
025900970317     D* (9:2) +1=5 ; (5X999)=4995
026000970317     D* (9:2) +1=5 ; (5X10) =50
026100050103     D* (9:2) +1=5 ; (5X600)=1500
026200970317     D* (13:2)+1=7 ; (7X10) =70
026300011001     D* (18:2)+1=10; (10X10) =100
026400980820     D*---
026500971124     D FPAG            DS                  OCCURS(500) INZ
026600970110     D  GGP                    1     60P 0
026700970110     D                                     DIM(30)                              *GG PAGAMENTO
026800970110     D  SGP                   61    210P 0
026900970110     D                                     DIM(30)                              *SPED. GG PAGAMENTO
027000971124     D FRIT            DS                  OCCURS(500) INZ
027100970110     D  RIT                    1   4995P 0
027200970110     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
027300971124     D FCON            DS                  OCCURS(500) INZ
027400970110     D  CON                    1   4995P 0
027500970110     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
027600971124     D FSGL            DS                  OCCURS(500) INZ
027700041118     D  SSP                    1     50P 0
027800041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
027900041118     D  PSC                   51    110P 1
028000041118     D                                     DIM(10)                              *PESO
028100041118     D  CSC                  111    160P 0
028200041118     D                                     DIM(10)                              *COLLI
028300041118     D  VSC                  161    220P 3
028400041118     D                                     DIM(10)                              *VOLUME
028500060829     D FSGL1           DS                  OCCURS(500) INZ
028600060829     D  SSP1                   1     50P 0
028700060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
028800060829     D  PSC1                  51    110P 1
028900060829     D                                     DIM(10)                              *PESO
029000060829     D  CSC1                 111    160P 0
029100060829     D                                     DIM(10)                              *COLLI
029200060829     D  VSC1                 161    220P 3
029300060829     D                                     DIM(10)                              *VOLUME
029400971124     D FCTS            DS                  OCCURS(500) INZ
029500050103     D  TSI                    1   3000P 0
029600050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
029700050103     D  TSE                 3001   6000P 0
029800050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
029900971124     D FTAR            DS                  OCCURS(500) INZ
030000970423     D  SSGL                   1     70P 3
030100970314     D                                     DIM(10)                              *SCAGLIONI
030200050103     D  CCTI                  71   1270
030300050103     D                                     DIM(600)                             *COD.TASSAZIONE ITAL
030400050103     D  CRCI                1271   1870
030500050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ITAL
030600050103     D  CCTE                1871   3070
030700050103     D                                     DIM(600)                             *COD.TASSAZIONE ESTE
030800050103     D  CRCE                3071   3670
030900050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ESTE
031000980820     D*---
031100980820     D* CARTELLO
031200980820     D*---
031300980824     D CCAR            DS                  OCCURS(2) INZ
031400980821     D  USPE                   1      5P 0
031500980821     D                                     DIM(1)                               *N°SPEDIZIONI ITA/ES
031600980821     D  UCLA                   6     10P 0
031700980821     D                                     DIM(1)                               *CLIENTE TIPO A
031800980821     D  UCLB                  11     15P 0
031900980821     D                                     DIM(1)                               *CLIENTE TIPO B
032000980821     D  UCLC                  16     20P 0
032100980821     D                                     DIM(1)                               *CLIENTE TIPO C
032200980821     D  UBON                  21     25P 0
032300980821     D                                     DIM(1)                               *BOLLETTAZIONE (' ')
032400980821     D  UBOA                  26     30P 0
032500980821     D                                     DIM(1)                               *BOLLETTAZIONE (A)
032600980821     D  UBOB                  31     35P 0
032700980821     D                                     DIM(1)                               *BOLLETTAZIONE (B)
032800980821     D  UNCL                  36     40P 0
032900980821     D                                     DIM(1)                               *COLLI
033000980821     D  UPKB                  41     45P 1
033100980821     D                                     DIM(1)                               *PESO
033200980821     D  UVLB                  46     50P 3
033300980821     D                                     DIM(1)                               *VOLUME
033400980821     D  UFRA                  51     55P 0
033500980821     D                                     DIM(1)                               *FRANCHI
033600980821     D  UASS                  56     60P 0
033700980821     D                                     DIM(1)                               *ASSEGNATI
033800980821     D  UPCV                  61     65P 0
033900980821     D                                     DIM(1)                               *CONSEGNE PRIVATI
034000980821     D  UPCM                  66     70P 0
034100980821     D                                     DIM(1)                               *CONSEGNE MAGAZZINO
034200980821     D  UPFD                  71     75P 0
034300980821     D                                     DIM(1)                               *FERMO DEPOSITO
034400980821     D  UESP                  76     80P 0
034500980821     D                                     DIM(1)                               *ESPRESSI
034600980821     D  UPPV                  81     85P 0
034700980821     D                                     DIM(1)                               *CONSEGNE PROVINCIA
034800980821     D  UPCA                  86     90P 0
034900980821     D                                     DIM(1)                               *CONSEGNE APPUNTAMEN
035000980821     D  UPCP                  91     95P 0
035100980821     D                                     DIM(1)                               *CONSEGNE PIANI
035200980821     D  UPCS                  96    100P 0
035300980821     D                                     DIM(1)                               *CONSEGNE SUPERMERCA
035400980821     D  UPCF                 101    105P 0
035500980821     D                                     DIM(1)                               *CONSEGNE FUORI MISU
035600980821     D  UPAB                 106    110P 0
035700980821     D                                     DIM(1)                               *C/ASSEGNI BARTOLINI
035800980821     D  UPAM                 111    115P 0
035900980821     D                                     DIM(1)                               *C/ASSEGNI MITTENTE
036000011002     D  UCAS                 116    125P 5
036100011002     D                                     DIM(1)                               *C/ASSEGNO
036200011002     D                                     PACKEVEN
036300011002     D  UPAR                 126    130P 0
036400980821     D                                     DIM(1)                               *ASSICURAZIONE R.C.V
036500011002     D  UPAC                 131    135P 0
036600980821     D                                     DIM(1)                               *ASSICURAZIONE X CON
036700011002     D  UASC                 136    145P 5
036800011001     D                                     DIM(1)                               *ASSICURAZIONE
036900011001     D                                     PACKEVEN
037000011002     D  UPGI                 146    150P 0
037100980821     D                                     DIM(1)                               *GIACENZE
037200011002     D  UPCD                 151    155P 0
037300980821     D                                     DIM(1)                               *CONSEGNE DDT
037400011002     D  UPCC                 156    160P 0
037500980821     D                                     DIM(1)                               *CONSEGNE C.STORICI
037600011002     D  UPCL                 161    165P 0
037700980821     D                                     DIM(1)                               *CONSEGNE L.DISAGIAT
037800011002     D  UCLT                 166    170P 0
037900980901     D                                     DIM(1)                               *CLIENTE TIPO T
038000011002     D  UGGS                 171    175P 0
038100000414     D                                     DIM(1)                               *GG SOSTA GIACENZA
038200011002     D  UNRG                 176    180P 0
038300000414     D                                     DIM(1)                               *N° RIAPERTURE GIACE
038400011011     D  UCIS                 181    185P 0
038500011011     D                                     DIM(1)                               *CONSEGNE ISOLE
038600011011     D  URIS                 186    190P 0
038700011011     D                                     DIM(1)                               *RITIRI ISOLE
038800020215     D  UCLD                 191    195P 0
038900020215     D                                     DIM(1)                               *CLIENTE TIPO D
039000040802     D  Uded                 196    200P 0
039100040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039200040811     D  Usbnb                206    210P 0
039300040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039400040824     D  Usban                211    215P 0
039500040824     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039600040824     D  Unbn                 216    220P 0
039700040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039800040824     D  Unba                 221    225P 0
039900040811     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040000040824     D  Uval                 226    230P 0
040100040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040200050808     D  Upks                 231    235P 0
040300050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040400050808     D  Upkn                 236    240P 0
040500050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040600060315     D  Uxx1                 241    245P 0
040700060315     D                                     DIM(1)                               *ASSICURAZIONE X CON
040800060829     D  UNCL1                246    250P 0
040900060829     D                                     DIM(1)                               *COLLI
041000060829     D  UPKB1                251    255P 1
041100060829     D                                     DIM(1)                               *PESO
041200060829     D  UVLB1                256    260P 3
041300060829     D                                     DIM(1)                               *VOLUME
041400980824     D CPAG            DS                  OCCURS(2) INZ
041500980820     D  UGGP                   1     60P 0
041600980819     D                                     DIM(30)                              *GG PAGAMENTO
041700980820     D  USGP                  61    210P 0
041800980819     D                                     DIM(30)                              *SPED. GG PAGAMENTO
041900980824     D CRIT            DS                  OCCURS(2) INZ
042000980820     D  URIT                   1   4995P 0
042100980819     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
042200980824     D CCON            DS                  OCCURS(2) INZ
042300980820     D  UCON                   1   4995P 0
042400980819     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
042500980824     D CSGL            DS                  OCCURS(2) INZ
042600041118     D  USSP                   1     50P 0
042700041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
042800041118     D  UPSC                  51    110P 1
042900041118     D                                     DIM(10)                              *PESO
043000041118     D  UCSC                 111    160P 0
043100041118     D                                     DIM(10)                              *COLLI
043200041118     D  UVSC                 161    220P 3
043300041118     D                                     DIM(10)                              *VOLUME
043400060829     D CSGL1           DS                  OCCURS(2) INZ
043500060829     D  USSP1                  1     50P 0
043600060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
043700060829     D  UPSC1                 51    110P 1
043800060829     D                                     DIM(10)                              *PESO
043900060829     D  UCSC1                111    160P 0
044000060829     D                                     DIM(10)                              *COLLI
044100060829     D  UVSC1                161    220P 3
044200060829     D                                     DIM(10)                              *VOLUME
044300980824     D CCTS            DS                  OCCURS(2) INZ
044400050103     D  UTSI                   1   3000P 0
044500050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
044600050103     D  UTSE                3001   6000P 0
044700050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
044800040803     D Dtasflo       E DS
044801091125     D tnte04ds      E DS
044900970110     D*-------------------
045000970110     D* DS TIPI TARIFFA
045100970110     D*-------------------
045200970110     D DSTR          E DS
045300050808     D*-------------------
045400050808     D* DS cod. trattamento merce
045500050808     D*-------------------
045600050808     D DS1b          E DS
045700021202     D*-------------------
045800021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
045900021202     D*-------------------
046000021202     D TRUL27DS      E DS
046100970110     D*-------------------
046200970110     D* DS CODICI TASSAZIONE
046300970110     D*-------------------
046400970110     D DSCT          E DS
046500970110     D*-------------------
046600970110     D* DS CAMBI E DIVISE
046700970110     D*-------------------
046800011011     D*DSCV          E DS
046900970110     D*-------------------
047000970110     D* DS RAGGRUPPAMENTI TIPI BOLLA
047100970110     D*-------------------
047200970110     D DS2W          E DS
047300970110     D*------------------
047400970110     D* DS TIPI BOLLA
047500970110     D*------------------
047600970110     D DSTB          E DS
047700970110     D*------------------
047800970110     D* DS CODICI PAGAMENTO
047900970110     D*------------------
048000970110     D DSFA          E DS
048100970110     D*------------------
048200970110     D* DS COMPOSIZIONE CODICE DI PAGAMENTO
048300970110     D*------------------
048400970110     D                 DS
048500970110     D  WCDP                   1      3
048600970110     D  WLIN                   4      4
048700970110     D  DSCDP                  1      4
048800970110     D*------------------
048900970110     D* DS INVIO DATI -BOLLETTAZIONE-
049000970110     D*------------------
049100970110     D DS3C          E DS
049200970110     D*------------------
049300020923     D* DS REPERIMENTO DATI UTENTE
049400970110     D*------------------
049500020923     D TIBS34DS      E DS                                                       *Profili utente
049600020923     D DDATIUTE      E DS                                                       *Dati utente
049700020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
049800970110     D*------------------
049900970110     D* DS "XSRDA8" - CONTROLLA DATA (8)
050000970110     D*------------------
050100970110     D WLBDA8          DS
050200970110     D  G08DAT                 1      8  0
050300970110     D  G08INV                 9     16  0
050400970110     D  G08ERR                17     17
050500970110     D  G08TGI                18     22  0
050600970110     D*-------------------
050700970110     D* DS SCOMPOSIZIONE DATA DECORRENZA(4) -TNTAM-
050800970110     D*-------------------
050900970110     D                 DS
051000970110     D  TAMDNN                 3      4  0
051100970110     D  TAMDMM                 5      6  0
051200970110     D  TAMDEC                 3      6  0
051300970110     D  TAMDDT                 1      8  0
051400970110     D*-------------------
051500970110     D* DS SCOMPOSIZIONE DATA SCADENZA(4) -TNTAM-
051600970110     D*-------------------
051700970110     D                 DS
051800970110     D  TAMSNN                 3      4  0
051900970110     D  TAMSMM                 5      6  0
052000970110     D  TAMSCA                 3      6  0
052100970110     D  TAMDST                 1      8  0
052200970110     D*-------------------
052300990812     D* DS COMPOSIZIONE DATA SPEDIZIONE -TITAS-
052400970110     D*-------------------
052500970110     D                 DS
052600970110     D  TASAAS                 1      4  0
052700970110     D  TASMGS                 5      8  0
052800970110     D  TASDSP                 1      8  0
052900970110     D*------------------
053000970110     D* DS PARAMETRI DI LANCIO
053100970110     D*------------------
053200970110     D PARAM           DS
053300980825     D  PARKSC                 1      7  0
053400980825     D  PARFPA                 8     10  0
053500980825     D  PARFAR                11     13  0
053600980825     D  PARFAP                14     16  0
053700980825     D  PARDS1                17     24  0
053800980825     D  PARAA1                17     20  0
053900980825     D  PARMG1                21     24  0
054000980825     D  PARDS2                25     32  0
054100980825     D  PARTAR                33     35
054200980825     D  PARCTO                36     36
054300980825     D  PARKOV                37     43  0
054400980825     D  PARCAR                37     41  0
054500980825     D  PARCTR                44     46  0
054600980825     D  PARTTR                44     44
054700980825     D  PARPRG                47     49  0
054800980825     D  PARLNP                50     52  0
054900980825     D  PARFIE                53     53
055000980825     D  PARSIM                54     54
055100980825     D  PARELA                55     55
055200011003     D  PARDIV                56     58
055300021031     D  PARUNI                59     59
055400040804     D  PARsta                60     60
055500040805     D  PARint                61     61
055600040805     D  PARsie                62     62
055601091130     D  PARdsf                63     63
055700970110     D*------------------
055800970110     D* ARCHITETTURA
055900970110     D*------------------
056000970110     D KPJBA         E DS
056100970110     D  PROFIL                 1     10
056200011011     D*------------------
056300011011     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
056400011011     D*------------------
056500011011     D DSGEDCE       E DS
056600011011     D*-------------------
056700011011     D* CONVERSIONI IMPORTI
056800011011     D*-------------------
056900011011     D YEUR          E DS                  EXTNAME(YEURCODS)
057000020227     D*-------------------
057100020227     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
057200020227     D*-------------------
057300020227     D OG143         E DS
057400020730     D*------------------
057500020730     D* DS PER LETTURA TABELLA "NTW"
057600020730     D*------------------
057700020730     D DNTW          E DS
057800021121     D*------------------
057900021121     D* DS RIDEFINIZIONE CAMPO TAMFLO DI TNTAM
058000021121     D*------------------
058100021121     D DSTA01        E DS
058200021031     D*-------------------
058300021031     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
058400021031     D*-------------------
058500021031     D TIBS10DS      E DS
058600021031     D  SKCLI                 21   5520  0 DIM(500)                              -cli. padri/figli
058700040811     D*-------------------
058800040811     D* CAMPI DI LAVORO
058900040811     D*-------------------
059000060829     d colli           s                   like(tasncl)
059100060829     d colli1          s                   like(tasncl)
059200040811     d $pes            s                   like(taspkf)
059300060829     d $pes1           s                   like(taspkf)
059400060829     d etcpmc1         s                   like(etcpmc)
059500060829     d etcvmc1         s                   like(etcvmc)
059600970110     C*------------------------------------------------------------------------*
059700970110     C* MAIN LINE
059800970110     C*------------------------------------------------------------------------*
059900970313     C*
060000970313     C* SE NESSUN ERRORE IN APERTURA
060100970313IF  1C     $FINE         IFEQ      '0'
060200970313     C*
060300970314     C* ANNULLA I DATI TIPO ESISTENTI
060400970314     C                   EXSR      ANNETX
060500970110     C* APRE GLI ARCHIVI
060600970110     C                   EXSR      OPEFIL
060700970415     C*
060800970416     C* LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
060900970416     C                   EXSR      LETCLI
061000970416     C*
061100970416     C* REPERISCE LA QUALITA' DEL CLIENTE TARIFFA / OFFERTA
061200970416     C                   MOVEL     'C'           $CLV              1
061300970416IF  1C     CLPCLV        IFNE      *BLANKS
061400970416     C                   MOVEL     CLPCLV        $CLV
061500970416E   1C                   ENDIF
061600970416     C*
061700970416     C* REPERISCE I GIORNI DI PAGAMENTO DEL CLIENTE / VISITA
061800970416     C                   Z-ADD     *ZEROS        $GGP              3 0
061900970416     C                   EXSR      REPGGP
062000021129     C*
062100021129     C* CICLO SULLA SCHIERA DEI CLIENTI DA ELABORARE (DISCORSO UNIFICANTE)
062200021129     C     1             DO        500           JJ                3 0
062300021129     C                   Z-ADD     SKCLI(JJ)     KTAKSC                         *CLIENTE
062400970110     C*
062500970110     C* POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
062600970110     C                   EXSR      SETFIL
062700970110     C*
062800970110     C* CICLO FINO A FINE -BOLLE-
062900970313DO  2C     $FINE         DOWEQ     '0'
063000970110     C*
063100970110     C* MEMORIZZA I DATI
063200970110     C                   EXSR      MEMDAT
063300970110     C*
063400970110     C* LETTURA SUCESSIVA -BOLLE-
063500970110     C                   EXSR      LETFIL
063600970313E   2C                   ENDDO                                                  *FINE BOLLE
063700021129     C*
063800021129     C* DOPO L'ELABORAZIONE DI OGNI CLIENTE RE-INIZIALIZZO IL FLAG DI "FINE"
063900021129     C                   MOVEL     '0'           $FINE
064000021129     C*
064100021129     C* TESTO LE CONDIZIONI X CUI USCIRE DAL CICLO DI MEMORIZZAZIONE
064200021129     C                   IF        SKCLI(JJ) = *ZEROS
064300021129     C                   MOVEL     '1'           $FINE
064400021129     C                   LEAVE
064500021129     C                   ENDIF
064600021129     C*
064700021129     C                   ENDDO
064800970110     C*
064900970110     C* SCRIVE -SE RICHIESTO- E STAMPA I DATI TIPO
065000970110     C                   EXSR      AGGETX
065100980818     C*
065200980818     C* SCRIVE E STAMPA I DATI TIPO DI CARTELLO -SE RICHIESTE TUTTE LE LINE DI PARTENZA-
065300980818IF  2C     PARCTO        IFEQ      'T'
065400980818     C     PARCAR        ANDEQ     88888
065500980818     C     PARLNP        ANDEQ     999
065600980818     C                   EXSR      AGGET8
065700980818E   2C                   ENDIF
065800970110     C*
065900970110     C* CHIUDE GLI ARCHIVI
066000970110     C                   EXSR      CLOFIL
066100970313E   1C                   ENDIF
066200040804     c* lancio la stampa se richiesto
066300040804     c                   if        parsta = 'S'
066400040804     c                   call      'TNTE49R'
066500040804     c                   parm                    kpjba
066600040804     c                   end
066700970110     C*
066800970110     C                   SETON                                        LR
066900970415     C*------------------------------------------------------------------------*
067000970416     C* LETCLI - LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
067100970415     C*------------------------------------------------------------------------*
067200970416     C     LETCLI        BEGSR
067300970415     C*
067400060808     C                   CLEAR                   CNCLP000                       *AZZERA RECORD
067500060808     C                   CLEAR                   CNIND000                       *AZZERA RECORD
067600060808     C                   CLEAR                   tfCLP000                       *AZZERA RECORD
067700060808     C                   CLEAR                   tfIND000                       *AZZERA RECORD
067800020923     C                   Z-ADD     1             KCLKUT                         *PARTICOLARITA'
067900020923     C                   Z-ADD     DUTKCI        KCLKCC
068000970415     C                   Z-ADD     PARKOV        KCLKSC
068100020923     C                   Z-ADD     1             KINKUT                         *INDIRIZZO
068200020923     C                   Z-ADD     DUTKCI        KINKCC
068300970415     C                   Z-ADD     PARKOV        KINKSC
068400060808IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
068500060808     C     KEYCLP        CHAIN     CNCLP00F                           99
068600060808     C     KEYIND        CHAIN     CNIND00F                           99
068700060808X   1C                   ELSE                                                   *OFFERTA
068800060804     C     KEYCLP        CHAIN     TFCLP00F                           99
068900060804     C     KEYIND        CHAIN     TFIND00F                           99
069000060804IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATO
069001091125     c                   clear                   tnte04ds
069002091130     C                   movel     pardsf        d04dsf
069003091125     C                   Z-ADD     parkov        d04nrv
069005091125     c                   call      'TNTE04R'
069006091125     c                   parm                    kpjba
069007091126     c                   parm                    tnte04ds
069200091125     C                   Z-ADD     d04KSC        KCLKSC                         *CLIENTE DELLA VISIT
069300091125     C                   Z-ADD     d04KSC        KINKSC                         *CLIENTE DELLA VISIT
069400060808     C     KEYCLP        CHAIN     CNCLP00F                           99
069500060808     C     KEYIND        CHAIN     CNIND00F                           99
069600060804     c                   end
069700060808     c                   end
069800970415     C*
069900970415     C                   ENDSR
070000970415     C*------------------------------------------------------------------------*
070100970415     C* REPGGP - REPERISCE IL NUMERO DEI GIORNI DI PAGAMENTO
070200970415     C*------------------------------------------------------------------------*
070300970415     C     REPGGP        BEGSR
070400970415     C*---
070500970415     C* IL GG DI PAGAMENTO IN TABELLA POSSONO ESSERE ESPRESSI IN MESI SE <=12 O
070600970415     C* IN GIORNI SE >12;
070700970415     C* CI POSSONO ESSERE INOLTRE + RATE DI PAGAMENTO E IN QUESTO CASO PRENDE
070800970415     C* SEMPRE L'ULTIMA (ES. SE 30/60/90 PRENDE 90 COME GGP).
070900970415     C* IL PAGAMENTO A VISTA NON HA GIORNI, MA PER COMODITA' SI MEMORIZZA CON
071000970415     C* CON GIORNI DI PAGAMENTO = 999
071100970415     C*---
071200970415     C                   MOVE      INDCDP        WCDP                           *CONDIZIONE PAGAMENT
071300000323     C                   MOVEL     '1'           WLIN                           *LINGUA
071400970415     C                   SETOFF                                           98
071500970415     C                   Z-ADD     1             I
071600970415     C     DSCDP         LOOKUP    CDP(I)                                 98
071700970415IF  1C     *IN98         IFEQ      *ON                                          *TROVATO
071800970415     C                   MOVEA     CRAT(I)       RAT
071900970415     C                   Z-ADD     10            I
072000970415DO  2C     I             DOWGT     0
072100970415IF  3C     RAT(I)        IFGT      *ZEROS                                       *P I E N O
072200970415     C                   MOVEL     RAT(I)        WN3               3 0
072300970415IF  4C     WN3           IFGT      12                                           *ESPRESSO IN GIORNI
072400970415     C                   Z-ADD     WN3           $GGP
072500970415X   4C                   ELSE                                                   *ESPRESSO IN MESI
072600970415     C     WN3           MULT      30            $GGP
072700970415E   4C                   ENDIF
072800981020     C                   LEAVE                                                  *ESCE DAL CICLO
072900970415X   3C                   ELSE                                                   *V U O T O
073000970415     C     I             SUB       1             I                              *DECREMENTA INDICE
073100970415E   3C                   ENDIF
073200970415E   2C                   ENDDO
073300970415E   1C                   ENDIF
073400970415     C*
073500970415     C* PAGAMENTO A VISTA (GGP=0), IMPOSTA GGP=999
073600970415IF  1C     $GGP          IFEQ      *ZEROS
073700970415     C                   Z-ADD     999           $GGP
073800970415E   1C                   ENDIF
073900970415     C*
074000970415     C                   ENDSR
074100970110     C*------------------------------------------------------------------------*
074200970110     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
074300970110     C*------------------------------------------------------------------------*
074400970110     C     SETFIL        BEGSR
074500970110     C*
074600021105IF  1C     PARKSC        IFGT      *ZEROS
074700990812     C     K31TAS        SETLL     TITAS31C                           99        *FINE ARCHIVIO
074800970110X   1C                   ELSE
074900970110     C                   Z-ADD     PARAA1        KTAAAS                         *ANNO
075000970110     C                   Z-ADD     PARMG1        KTAMGS                         *MESE/GIORNO
075100990812     C     K38TAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
075200970110E   1C                   ENDIF
075300970110     C*
075400970110IF  1C     *IN99         IFEQ      *ON
075500970110     C                   MOVEL     '1'           $FINE                          *FINE PGM
075600970110X   1C                   ELSE
075700970110     C                   EXSR      LETFIL                                       *LETTURA RECORD
075800970110E   1C                   ENDIF
075900970110     C*
076000970110     C                   ENDSR
076100970110     C*------------------------------------------------------------------------*
076200970110     C* LETFIL - LETTURA PROSSIMO RECORD -BOLLE-
076300970110     C*------------------------------------------------------------------------*
076400970110     C     LETFIL        BEGSR
076500970110     C*
076600970110     C* LETTURA FINO A:
076700970110     C*  - FINE FILE
076800970110     C*  - TROVATO RECORD VALIDO
076900970110     C                   MOVEL     'N'           WRECOK
077000970110DO  1C     $FINE         DOWEQ     '0'
077100970110     C     WRECOK        ANDEQ     'N'
077200970110     C*
077300970110IF  2C     PARKSC        IFGT      *ZEROS
077400990812     C                   READ      TITAS31C                               99
077500970110X   2C                   ELSE
077600990812     C                   READ      TITAS38C                               99
077700970110E   2C                   ENDIF
077800970110     C*
077900970110IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
078000970110     C                   MOVEL     '1'           $FINE
078100970110X   2C                   ELSE
078200970110     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
078300970110E   2C                   ENDIF
078400970110E   1C                   ENDDO
078500970110     C*
078600970110     C                   ENDSR
078700970110     C*------------------------------------------------------------------------*
078800970110     C* CHKREC - CONTROLLA VALIDITA' RECORD -BOLLE-
078900970110     C*------------------------------------------------------------------------*
079000970110     C     CHKREC        BEGSR
079100970110     C*
079200970110     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
079300081105     C*
079400081105     C* se sto elaborando la cartello i dati tipo della cartello escludo le
079500081105     c* le spedizioni con lnp e lna DPD e FED
079600081105IF  2C     PARCTO        IFEQ      'T'
079700081105     C     PARCAR        ANDEQ     88888
079800081105     C     PARLNP        ANDEQ     999
079900081105     c* fedex
080000081105     C     taslnp        LOOKUP    FILFED                                 55
080100081105     C                   IF        *IN55 = *ON
080200081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
080300081105     C                   ENDIF
080400081105     C     taslna        LOOKUP    FILFED                                 55
080500081105     C                   IF        *IN55 = *ON
080600081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
080700081105     C                   ENDIF
080800081105     c* dpd
080900081105     C     taslnp        LOOKUP    FILdpd                                 55
081000081105     C                   IF        *IN55 = *ON
081100081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
081200081105     C                   ENDIF
081300081105     C     taslna        LOOKUP    FILdpd                                 55
081400081105     C                   IF        *IN55 = *ON
081500081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
081600081105     C                   ENDIF
081700081105E   1C                   ENDIF
081800970423     C*
081900970423     C* TIPO BOLLA NON VALIDO COME SPEDIZIONE O RICAVO, ESCLUDE
082000970416     C                   Z-ADD     1             I
082100970416     C                   SETOFF                                           98
082200970416     C     TASTBL        LOOKUP    TBL(I)                                 98
082300970416IF  1C     *IN98         IFEQ      *ON                                          *TIPO BOLLA TROVATO
082400970416IF  2C     SPD(I)        IFEQ      'S'
082500970416     C     RIC(I)        OREQ      'S'
082600970417     C                   MOVEL     SPD(I)        DEPSP             1            *VALIDA COME SPEDIZ
082700970417     C                   MOVEL     RIC(I)        DEPRI             1            *VALIDA COME RICAVO
082800970416X   2C                   ELSE
082900970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083000970416E   2C                   ENDIF
083100970416X   1C                   ELSE
083200970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083300970416E   1C                   ENDIF
083400970423     C*
083500970110     C* CLIENTE <> DA QUELLO RICHIESTO, ESCLUDE ED ESCE DAL PGM
083600970110IF  1C     PARKSC        IFGT      *ZEROS                                       *LETTURA X CLIENTE
083700021031     C* VERIFICO CHE IL CLIENTE CORRENTE SIA NELLA LISTA DI QUELLI DA CONSIDERARE
083800021031     C                   Z-ADD     TASKSC        WRKCLI           11 0
083900021031     C     WRKCLI        LOOKUP    SKCLI                                  60
084000021031IF  2C     *IN60         IFEQ      *OFF
084100021031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
084200021105     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
084300021031E   2C                   ENDIF
084400970110E   1C                   ENDIF
084500970110     C*
084600970110     C* DATA SPEDIZIONE > DA DATA FINALE RICHIESTA, ESCLUDE ED ESCE DAL PGM
084700970110IF  1C     PARKSC        IFEQ      *ZEROS                                       *LETTURA X DATA
084800970110     C     PARDS1        ANDGT     *ZEROS
084900970110IF  2C     TASDSP        IFGT      PARDS2
085000970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
085100970110     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
085200970110E   2C                   ENDIF
085300970110E   1C                   ENDIF
085400970110     C*
085500970430     C* FILIALE PARTENZA <> DA QUELLA RICHIESTA, ESCLUDE
085600970430IF  1C     PARFPA        IFGT      *ZEROS
085700970430IF  2C     PARFPA        IFNE      TASLNP
085800970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
085900970110E   2C                   ENDIF
086000970110E   1C                   ENDIF
086100970430     C*
086200970430     C* FILIALE ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
086300970430IF  1C     PARFAR        IFGT      *ZEROS
086400970430IF  2C     PARFAR        IFNE      TASLNA
086500970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086600970430E   2C                   ENDIF
086700970430E   1C                   ENDIF
086800970430     C*
086900970430     C* FILIALE PARTENZA O ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
087000970430IF  1C     PARFAP        IFGT      *ZEROS
087100970430IF  2C     PARFAP        IFEQ      TASLNP
087200970430     C     PARFAP        OREQ      TASLNA
087300970430X   2C                   ELSE
087400970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
087500970430E   2C                   ENDIF
087600970430E   1C                   ENDIF
087700020227     C*
087800020227     C* QUALUNQUE FILIALE INDICATA NON SIA FEDEX
087900020307     C     PARFPA        IFGT      *ZEROS
088000020227     C     PARFPA        LOOKUP    FILFED                                 55
088100020227     C                   IF        *IN55 = *ON
088200020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
088300020227     C                   ENDIF
088400020307     C                   ENDIF
088500020307     C     PARFAR        IFGT      *ZEROS
088600020227     C     PARFAR        LOOKUP    FILFED                                 55
088700020227     C                   IF        *IN55 = *ON
088800020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
088900020227     C                   ENDIF
089000020307     C                   ENDIF
089100020307     C     PARFAP        IFGT      *ZEROS
089200020227     C     PARFAP        LOOKUP    FILFED                                 55
089300020227     C                   IF        *IN55 = *ON
089400020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
089500020227     C                   ENDIF
089600020307     C                   ENDIF
089700970110     C*
089800970110     C* DATA SPEDIZIONE <> DA QUELLE RICHIESTE, ESCLUDE
089900970110IF  1C     PARDS1        IFGT      *ZEROS
090000970110IF  2C     TASDSP        IFGE      PARDS1
090100970110     C     TASDSP        ANDLE     PARDS2
090200970110X   2C                   ELSE
090300970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090400970110E   2C                   ENDIF
090500970110E   1C                   ENDIF
090600970313     C*
090700970313     C* CODICE TARIFFA <> DA QUELLO RICHIESTO, ESCLUDE
090800970313IF  1C     PARTAR        IFNE      *BLANKS
090900970313     C                   MOVEL     PARTAR        WN3               3 0
091000970313IF  2C     WN3           IFNE      TASCTR
091100970313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
091200970313E   2C                   ENDIF
091300970313E   1C                   ENDIF
091400970430     C*
091500970430     C* RECORD VALIDO, IMPOPSTA LA FILIALE SULLA QUALE MEMORIZZARE I DATI
091600970430IF  1C     WRECOK        IFEQ      'S'
091700970430IF  2C     PARLNP        IFNE      999                                          *SELEZIONE FILIALE
091800970430     C                   Z-ADD     PARLNP        DEPFIL                         *FILIALE=SELEZIONE
091900970430X   2C                   ELSE                                                   *SELEZIONE MULTIPLA
092000970430     C                   Z-ADD     TASLNP        DEPFIL                         *FILIALE=L.PARTENZA
092100970430E   2C                   ENDIF
092200060829     c                   z-add     tasncl        colli
092300060829     c                   z-add     tasncl        colli1
092400040811     c* reperisce i bancali. se BNB imposto nel campo di comodo COLLI
092500040811     c* i colli originali
092600040811     c                   clear                   dar5ban
092700040811     c                   clear                   dar5bnb
092800040811     c                   if        %subst(tasgva: 1: 1) = 'O' or
092900040811     c                             %subst(tasgva: 1: 1) = 'B'
093000040811     c                   if        %subst(tasgva: 1: 1) = 'O'
093100040811     c                   eval      ar5trd = 'BNB'
093200040811     c                   else
093300040811     c                   eval      ar5trd = 'BAN'
093400040811     c                   end
093500060829     c                   exsr      srfiar5
093600040811     c                   if        %subst(tasgva: 1: 1) = 'O'
093700040811     c                   eval      dar5bnb = ar5uni
093800060829     c                   z-add     §ar5bcor      colli
093900040811     c                   else
094000040811     c                   eval      dar5BAN = ar5uni
094100040811     c                   end
094200040811     c                   end
094300040811E   1C                   ENDIF
094400970416     C*
094500970110     C                   ENDSR
094600970110     C*------------------------------------------------------------------------*
094700970110     C* MEMDAT - MEMORIZZA I DATI -BOLLE-
094800970110     C*------------------------------------------------------------------------*
094900970110     C     MEMDAT        BEGSR
095000970110     C*
095100970110     C* RECUPERA L'INDICE DELLA FILIALE
095200970110     C                   Z-ADD     1             F                 4 0
095300970110     C                   SETOFF                                           98
095400970430     C     DEPFIL        LOOKUP    SORG(F)                                98
095500970110IF  1C     *IN98         IFEQ      *ON                                          *FILIALE TROVATA
095600970110     C     F             OCCUR     FPAG
095700970110     C     F             OCCUR     FCON
095800970110     C     F             OCCUR     FRIT
095900970110     C     F             OCCUR     FSGL
096000060829     C     F             OCCUR     FSGL1
096100970110     C     F             OCCUR     FCTS
096200970423     C     F             OCCUR     FTAR
096300970415     C*
096400970415     C* REPERISCE LA SOLUZIONE DEL CLIENTE
096500970415     C                   MOVEL     *BLANKS       $SOL              1
096600970415     C                   EXSR      REPSOL
096700060315     C*
096800060315     C* REPERISCE le varie di fatturazione
096900060315     C                   clear                   svn
097000060315     C                   clear                   van
097100060315     C                   EXSR      REPvrf
097200970415     C*
097300060829     C* REPERISCE IL VOLUME DELLA BOLLA
097400970415     C                   EXSR      REPVOL
097500040811     C*
097600060829     C* REPERISCE IL PESO DELLA BOLLA da
097700040811     C                   EXSR      REPpes
097800970110     C*
097900060315     C* REPERISCE LE consegne particolari
098000970110     C                   MOVEL     *BLANKS       $PCA              1
098100970110     C                   MOVEL     *BLANKS       $PCP              1
098200970110     C                   MOVEL     *BLANKS       $PCS              1
098300970110     C                   MOVEL     *BLANKS       $PCF              1
098400970415     C                   MOVEL     *BLANKS       $PCD              1
098500990812     C                   MOVEL     *BLANKS       $PIN              1            *INOLTRO
098600011026     C                   MOVEL     *ZEROS        $PINIM           21 8          *IMPORTO INOLTRO
098700011011     C                   MOVEL     *BLANKS       $CIS              1
098800011011     C                   MOVEL     *BLANKS       $RIS              1
098900040802     C                   MOVEL     *BLANKS       $ded              1
099000970110     C                   EXSR      REPVAR
099100000414     C*
099200000414     C* REPERISCE LA GIACIENZA
099300000414     C                   MOVEL     *BLANKS       $PGI              1            *ESISTE GIACENZA
099400000414     C                   Z-ADD     *ZEROS        $PGIGS            9 0          *GG SOSTA
099500000414     C                   Z-ADD     *ZEROS        $PGINR            9 0          *N° RIAPERTURE
099600021122     C                   EXSR      REPGIA
099700970110     C*
099800970110     C* REPERISCE IL C/ASSEGNO
099900011026     C                   Z-ADD     *ZEROS        $CAS             21 8
100000970110     C                   MOVEL     *BLANKS       $TPI              1
100100970110IF  2C     TASFCA        IFEQ      'S'                                          *ESISTE C/ASSEGNO
100200970110     C                   EXSR      REPCAS
100300970110E   2C                   ENDIF
100400970110     C*
100500970110     C* REPERISCE L'ASSICURAZIONE
100600011026     C                   Z-ADD     *ZEROS        $IAS             21 8
100700970110     C                   EXSR      REPASS
100800970110     C*
100900970110     C* REPERISCE IL CODICE TASSAZIONE
101000970423     C                   Z-ADD     1             J
101100970423     C                   SETOFF                                           98
101200970423     C     TASCTS        LOOKUP    CTS(J)                                 98
101300970423IF  2C     *IN98         IFEQ      *ON                                          *TROVATO
101400970416     C*---
101500970110     C*  M E M O R I Z Z A
101600970416     C*---
101700970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
101800970423     C*
101900971204IF  4C     FL1(F)        IFNE      *BLANKS                                      *CODICE ESTERO
102000970423     C     CIE(J)        OREQ      'E'
102100970423     C                   Z-ADD     1             I
102200970423     C                   SETOFF                                           98
102300970423     C     TASCTS        LOOKUP    CCTE(I)                                98
102400970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
102500970423     C                   ADD       1             TSE(I)                         *N°SPED.CTS ESTERO
102600970423E   5C                   ENDIF
102700970423X   4C                   ELSE                                                   *CODICE ITALIA
102800970423     C                   Z-ADD     1             I
102900970423     C                   SETOFF                                           98
103000970423     C     TASCTS        LOOKUP    CCTI(I)                                98
103100970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
103200970423     C                   ADD       1             TSI(I)                         *N°SPED.CTS ITALIA
103300970423E   5C                   ENDIF
103400970423E   4C                   ENDIF
103500970423     C*
103600970423     C                   ADD       1             SPE(F)                         *N°SPEDIZIONI TOTALI
103700020215     C*
103800020215IF  4C     $CLV          IFEQ      'D'
103900020215     C                   ADD       1             CLD(F)                         *CLIENTE TIPO D
104000020215E   4C                   ENDIF
104100980901IF  4C     $CLV          IFEQ      'T'
104200980901     C                   ADD       1             CLT(F)                         *CLIENTE TIPO T
104300980901E   4C                   ENDIF
104400970416IF  4C     $CLV          IFEQ      'A'
104500970110     C                   ADD       1             CLA(F)                         *CLIENTE TIPO A
104600970416E   4C                   ENDIF
104700970416IF  4C     $CLV          IFEQ      'B'
104800970110     C                   ADD       1             CLB(F)                         *CLIENTE TIPO B
104900970416E   4C                   ENDIF
105000970416IF  4C     $CLV          IFEQ      'C'
105100970415     C     $CLV          OREQ      *BLANKS
105200970110     C                   ADD       1             CLC(F)                         *CLIENTE TIPO C
105300970416E   4C                   ENDIF
105400970110     C*
105500970416IF  4C     $SOL          IFEQ      *BLANKS
105600970110     C                   ADD       1             BON(F)                         *BOLLETTAZIONE (' ')
105700970416E   4C                   ENDIF
105800970416IF  4C     $SOL          IFEQ      'A'
105900970110     C                   ADD       1             BOA(F)                         *BOLLETTAZIONE (A)
106000970416E   4C                   ENDIF
106100970416IF  4C     $SOL          IFEQ      'B'
106200970110     C                   ADD       1             BOB(F)                         *BOLLETTAZIONE (B)
106300970416E   4C                   ENDIF
106400050808     c* con il cod. trattamento merce aggancio tabella 1B per vedere se
106500050808     c* previsto packing list
106600050808     c                   z-add     1             xp                4 0
106700050808     c     tasctm        lookup    cpkl(xp)                               95
106800050808     c                   if        *in95
106900050808     c                   add       1             pks(f)
107000050808     c                   else
107100050808     c                   add       1             pkn(f)
107200050808     c                   end
107300040811     C* se oltre 20 colli sommo i colli originali altrimenti i colli
107400040811     c* della bolla
107500040811     C                   ADD       colli         NCL(F)                         *COLLI
107600040811     C                   ADD       $pes          PKB(F)                         *PESO
107700040811     C                   ADD       $vol          VLB(F)                         *VOLUME
107800060829     C                   ADD       colli1        NCL1(F)                        *COLLI
107900060829     C                   ADD       $pes1         PKB1(F)                        *PESO
108000060829     C                   ADD       $vol1         VLB1(F)                        *VOLUME
108100970110     C*
108200970110     C                   MOVEL     TASTBL        WA1               1
108300970416IF  4C     WA1           IFEQ      'F'
108400970110     C                   ADD       1             FRA(F)                         *FRANCHI
108500970416X   4C                   ELSE
108600970110     C                   ADD       1             ASS(F)                         *ASSEGNATI
108700970416E   4C                   ENDIF
108800970110     C*
108900970110     C                   MOVEL     TASGVA        WA1
109000970416IF  4C     WA1           IFEQ      'P'
109100970110     C                   ADD       1             PCV(F)                         *CONSEGNE PRIVATI
109200970416E   4C                   ENDIF
109300040802     C* bancali bartolini
109400040802IF  4C     WA1           IFEQ      'B'
109500040824     C                   ADD       1             sban(F)                        *CONSEGNE PRIVATI
109600040809     C                   ADD       §ar5nba       nba(F)                         *CONSEGNE PRIVATI
109700040809     C                   ADD       §ar5nb2       nba(F)                         *CONSEGNE PRIVATI
109800040802E   4C                   ENDIF
109900040802     C* bancali oltre 20 colli
110000040802IF  4C     WA1           IFEQ      'O'
110100040811     C                   ADD       1             sbnb(F)                        *CONSEGNE PRIVATI
110200040811     C                   ADD       §ar5bnba      nbn(F)                         *CONSEGNE PRIVATI
110300040811     C                   ADD       §ar5bnb2      nbn(F)                         *CONSEGNE PRIVATI
110400040802E   4C                   ENDIF
110500040802     C* merce di valore
110600040802     c                   movel     tasflo        dtasflo
110700040802IF  1C     §FLOTBA       IFEQ      'V'
110800040802     C                   add       1             val(F)
110900040802E   1C                   ENDIF
111000970110     C*
111100970416IF  4C     TASFDN        IFEQ      'S'
111200011011     C                   ADD       1             PCM(F)                         *MERCE A MAGAZZINO
111300970416E   4C                   ENDIF
111400970110     C*
111500970416IF  4C     TASFFD        IFEQ      'S'
111600970110     C                   ADD       1             PFD(F)                         *FERMO DEPOSITO
111700970416E   4C                   ENDIF
111800970110     C*
111900970416IF  4C     TASTSP        IFEQ      'E'
112000970110     C                   ADD       1             ESP(F)                         *ESPRESSI
112100970416E   4C                   ENDIF
112200970110     C*
112300011011IF  4C     TASFIN        IFEQ      'D'
112400011011     C                   ADD       1             PCL(F)                         *CONS. L.DISAGIATE
112500011011E   4C                   ENDIF
112600011011     C*
112700011011IF  4C     TASFIN        IFEQ      'I'
112800011011     C                   ADD       1             CIS(F)                         *CONS. ISOLE
112900011011E   4C                   ENDIF
113000011011     C*
113100011011IF  4C     TASFAP        IFEQ      'I'
113200011011     C                   ADD       1             RIS(F)                         *RITIRI ISOLE
113300011011E   4C                   ENDIF
113400011011     C*
113500011011IF  4C     TASFIN        IFEQ      'P'
113600011011     C                   ADD       1             PPV(F)                         *CONSEGNE PROVINCIA
113700970416E   4C                   ENDIF
113800970110     C*
113900970416IF  4C     $PCA          IFEQ      'S'
114000970110     C                   ADD       1             PCA(F)                         *CONS. APPUNTAMENTO
114100970416E   4C                   ENDIF
114200970110     C*
114300970416IF  4C     $PCP          IFEQ      'S'
114400970110     C                   ADD       1             PCP(F)                         *CONS. PIANI
114500970416E   4C                   ENDIF
114600970110     C*
114700970416IF  4C     $PCS          IFEQ      'S'
114800970110     C                   ADD       1             PCS(F)                         *CONS. SUPERMERCATI
114900970416E   4C                   ENDIF
115000970110     C*
115100970416IF  4C     $PCF          IFEQ      'S'
115200970110     C                   ADD       1             PCF(F)                         *CONS. FUORI MISURA
115300970416E   4C                   ENDIF
115400040802     C* destinatario disagiato
115500040802IF  4C     $ded          IFEQ      'S'
115600040802     C                   ADD       1             ded(F)                         *CONS. FUORI MISURA
115700040802E   4C                   ENDIF
115800970415     C*
115900970416IF  4C     $PCD          IFEQ      'S'
116000970415     C                   ADD       1             PCD(F)                         *CONS. DDT
116100970416E   4C                   ENDIF
116200970110     C*
116300970416IF  4C     $GGP          IFGT      *ZEROS
116400970110     C                   Z-ADD     1             I
116500970110     C                   SETOFF                                           98
116600970110     C     $GGP          LOOKUP    GGP(I)                                 98
116700970416IF  5C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
116800970110     C                   ADD       1             SGP(I)                         *SPED. GG PAGAMENTO
116900970416X   5C                   ELSE                                                   *MAI MEMORIZZATI
117000970110     C                   Z-ADD     1             I
117100970416DO  6C     I             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
117200970416IF  7C     GGP(I)        IFEQ      *ZEROS                                       *DISPONIBILE
117300970110     C                   Z-ADD     $GGP          GGP(I)                         *GG
117400970110     C                   Z-ADD     1             SGP(I)                         *SPED. GG PAGAMENTO
117500981014     C                   LEAVE                                                  *ESCE DAL CICLO
117600970416E   7C                   ENDIF
117700970416E   6C                   ENDDO
117800970416E   5C                   ENDIF
117900970416E   4C                   ENDIF
118000000411     C*
118100000411     C                   MOVEL     TASTBL        WA1               1
118200000411IF  4C     WA1           IFEQ      'F'                                          *SOLO FRANCHI
118300000411IF  5C     TASSTR        IFGT      *ZEROS
118400990812     C     TASSTR        ANDNE     99999
118500000411IF  6C     TASSTR        IFGT      999                                          *CAMPO 5,0 -> 3,0
118600990812     C                   Z-ADD     999           TASSTR
118700000411E   6C                   ENDIF
118800970110     C                   Z-ADD     TASSTR        I                              *STOP RITIRO
118900970110     C                   ADD       1             RIT(I)                         *SPEDIZIONI STOP
119000000411E   5C                   ENDIF
119100000411     C*
119200000411X   4C                   ELSE                                                   *SOLO ASSEGNATI
119300000411     C*
119400000411IF  5C     TASSTP        IFGT      *ZEROS
119500990812     C     TASSTP        ANDNE     99999
119600000411IF  6C     TASSTP        IFGT      999                                          *CAMPO 5,0 -> 3,0
119700990812     C                   Z-ADD     999           TASSTP
119800000411E   6C                   ENDIF
119900970110     C                   Z-ADD     TASSTP        I                              *STOP CONSEGNA
120000970110     C                   ADD       1             CON(I)                         *SPEDIZIONI STOP
120100000411E   5C                   ENDIF
120200000411E   4C                   ENDIF
120300970110     C*
120400060829     c* per ricavi
120500970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
120600970110     C     PARTTR        OREQ      '3'                                          *KG
120700040811     C                   Z-ADD     $pes          TASSGL           13 3          *SCAGLIONE IN KG
120800970416X   4C                   ELSE
120900040811     C                   Z-ADD     colli         TASSGL                         *SCAGLIONE IN COLLI
121000040811E   4C                   ENDIF
121100971024IF  4C     TASSGL        IFGT      *ZEROS
121200971024DO  5C     1             DO        10            I
121300971024IF  6C     TASSGL        IFLE      SSGL(I)
121400970110     C                   ADD       1             SSP(I)                         *N°SPED SCAGLIONE
121500040811     C                   ADD       $pes          PSC(I)                         *PESO
121600040811     C                   ADD       colli         CSC(I)                         *COLLI
121700970415     C                   ADD       $VOL          VSC(I)                         *VOLUME
121800970110     C                   LEAVE
121900971024E   6C                   ENDIF
122000971024E   5C                   ENDDO
122100040920     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
122200060829IF  5C     PARCTO        IFEQ      'T'
122300040920     C     PARCAR        ANDEQ     88888
122400040920     C     PARLNP        ANDEQ     999
122500040920     C                   EXSR      SRSGL8
122600060829E   5C                   ENDIF
122700971024E   4C                   ENDIF
122800060829     c* per competenze
122900060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
123000060829     C     PARTTR        OREQ      '3'                                          *KG
123100060829     C                   Z-ADD     $pes1         TASSGL           13 3          *SCAGLIONE IN KG
123200060829X   4C                   ELSE
123300060829     C                   Z-ADD     colli1        TASSGL                         *SCAGLIONE IN COLLI
123400060829E   4C                   ENDIF
123500060829IF  4C     TASSGL        IFGT      *ZEROS
123600060829DO  5C     1             DO        10            I
123700060829IF  6C     TASSGL        IFLE      SSGL(I)
123800060829     C                   ADD       1             SSP1(I)                        *N°SPED SCAGLIONE
123900060829     C                   ADD       $pes1         PSC1(I)                        *PESO
124000060829     C                   ADD       colli1        CSC1(I)                        *COLLI
124100060829     C                   ADD       $VOL1         VSC1(I)                        *VOLUME
124200060829     C                   LEAVE
124300060829E   6C                   ENDIF
124400060829E   5C                   ENDDO
124500060829     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
124600060829IF  5C     PARCTO        IFEQ      'T'
124700060829     C     PARCAR        ANDEQ     88888
124800060829     C     PARLNP        ANDEQ     999
124900060829     C                   EXSR      SRSGL81
125000060829E   5C                   ENDIF
125100060829E   4C                   ENDIF
125200971024E   3C                   ENDIF
125300970110     C*
125400970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
125500970416     C     DEPRI         OREQ      'S'                                          *BOLLA VALIDA RICAVI
125600970416IF  4C     $CAS          IFGT      *ZEROS
125700970110     C                   ADD       $CAS          CAS(F)                         *C/ASSEGNO
125800970416IF  5C     $TPI          IFEQ      'M'
125900970110     C                   ADD       1             PAM(F)                         *MITTENTE
126000970416X   5C                   ELSE
126100970110     C                   ADD       1             PAB(F)                         *BARTOLINI
126200970416E   5C                   ENDIF
126300970416E   4C                   ENDIF
126400970110     C*
126500970416IF  4C     $IAS          IFGT      *ZEROS
126600970110     C                   ADD       $IAS          ASC(F)                         *ASSICURAZIONE
126700970110     C                   ADD       1             PAC(F)                         *ASSICURAZIONE CONTO
126800970416X   4C                   ELSE
126900060315     c                   if        flgacbase = *on
127000060315     C                   ADD       1             xx1(F)                         *ASSICURAZIONE R.C.V
127100060315     c                   else
127200970110     C                   ADD       1             PAR(F)                         *ASSICURAZIONE R.C.V
127300970416E   4C                   ENDIF
127400060315E   4C                   ENDIF
127500970110     C*
127600000414IF  4C     $PGI          IFEQ      'S'
127700970110     C                   ADD       1             PGI(F)                         *GIACENZE
127800000414     C                   ADD       $PGIGS        GGS(F)                         *GG SOSTA GIACENZA
127900000414     C                   ADD       $PGINR        NRG(F)                         *N° RIAPERTURE GIACE
128000970416E   4C                   ENDIF
128100970416E   3C                   ENDIF
128200970110     C*
128300970416X   2C                   ELSE                                                   *CON.TASS.NON TROVAT
128400970416     C                   MOVEL     MTAS(2)       DEPERR           50
128500970110     C                   MOVEL     'T'           DEPTER            1
128600970110     C                   EXSR      STAERR
128700970416E   2C                   ENDIF
128800970110     C*
128900970110X   1C                   ELSE                                                   *FILIALE NON TROVATA
129000970110     C                   MOVEL     MTAS(1)       DEPERR
129100970110     C                   MOVEL     'T'           DEPTER
129200970110     C                   EXSR      STAERR
129300970110E   1C                   ENDIF
129400970110     C*
129500970110     C                   ENDSR
129600970415     C*------------------------------------------------------------------------*
129700040809     C* fiar5 tipo record BAN
129800970415     C*------------------------------------------------------------------------*
129900040809     C     srfiar5       BEGSR
130000040809     c*
130100040809     c                   z-add     tasaas        ar5aas
130200040809     c                   z-add     taslnp        ar5lnp
130300040809     c                   z-add     tasnrs        ar5nrs
130400040809     c                   z-add     tasnsp        ar5nsp
130500040811     c                   clear                   ar5uni
130600040809     c     kar5          chain     fiar531c
130700040809     C*
130800040809     C                   ENDSR
130900040809     C*------------------------------------------------------------------------*
131000040809     C* REPSOL - REPERISCE LA SOLUZIONE DI INVIO DATI -BOLLETTAZIONE-
131100040809     C*------------------------------------------------------------------------*
131200040809     C     REPSOL        BEGSR
131300970415     C*---
131400970415     C* NON E' PRESENTE IN TABELLA -> NESSUNA PROCEDURA            ( )
131500970415     C* NON HA IL NUMERO DI SERIE  -> STAMPA LE BOLLE              (A)
131600970415     C* HA     IL NUMERO DI SERIE  -> STAMPA LE BOLLE + SEGNACOLLI (B)
131700040310     C*--- escludo bolle recapito contrassegni
131800970415IF  1C     TASNRS        IFGT      *ZEROS
131900040310IF  1C     TAScbo        andne     'FW'
132000970415     C                   MOVEL     'B'           $SOL                           *PROCEDURA 'B'
132100970415X   1C                   ELSE
132303100303     C                   Z-ADD     1             KTBKUT
132304100303     C                   MOVEL     '3C'          KTBCOD
132305100303     c                   movel(p)  tasksc        ktbkey
132306100303     c     keytab        chain     tabel00f
132307100303     c                   if        %found(tabel00f) and tblflg = ' '
132308100303     C                   MOVEL     TBLUNI        DS3C
132600100303IF  3C     §3COKD        IFEQ      '*'
132700970415     C                   MOVEL     'A'           $SOL                           *PROCEDURA 'A'
132800970415X   3C                   ELSE
132900970415     C                   MOVEL     *BLANKS       $SOL                           *PROCEDURA NO AVVIAT
133000970415E   3C                   ENDIF
133100970415E   2C                   ENDIF
133200970415E   1C                   ENDIF
133300970415     C*
133400970415     C                   ENDSR
133500970415     C*------------------------------------------------------------------------*
133600970415     C* REPVOL - REPERISCE IL VOLUME DELLA BOLLA
133700970415     C*------------------------------------------------------------------------*
133800970415     C     REPVOL        BEGSR
133900060829     C* colli x ricavi
134000060829     C                   Z-ADD     *ZEROS        $VOL              5 3
134100970415     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
134200040811    1C     TASVLC        IFGT      *ZEROS
134300040811     C     colli         ANDEQ     TASNCR
134400970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
134500970415E   1C                   ENDIF
134600970415     C*
134700970415     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
134800970415     C     TASVLC        IFGT      TASVLF
134900040811     C     colli         ANDNE     TASNCR
135000970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
135100970415E   1C                   ENDIF
135200970415     C*
135300970415     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
135400970415IF  1C     $VOL          IFEQ      *ZEROS
135500970415     C                   Z-ADD     TASVLF        $VOL                           *VOLUME NORMALE
135600060829E   1C                   ENDIF
135700060829c*   c*colli x competetnze
135800060829     C                   Z-ADD     *ZEROS        $VOL1             5 3
135900060829     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
136000060829    1C     TASVLC        IFGT      *ZEROS
136100060829     C     colli1        ANDEQ     TASNCR
136200060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
136300060829E   1C                   ENDIF
136400060829     C*
136500060829     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
136600060829     C     TASVLC        IFGT      TASVLF
136700060829     C     colli1        ANDNE     TASNCR
136800060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
136900060829E   1C                   ENDIF
137000060829     C*
137100060829     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
137200060829IF  1C     $VOL1         IFEQ      *ZEROS
137300060829     C                   Z-ADD     TASVLF        $VOL1                          *VOLUME NORMALE
137400060829E   1C                   ENDIF
137500970415     C*
137600000414     C                   ENDSR
137700040811     C*------------------------------------------------------------------------*
137800040811     C* REPpes - REPERISCE IL peso DELLA BOLLA
137900040811     C*------------------------------------------------------------------------*
138000040811     C     REPpes        BEGSR
138100060829     C* colli x ricavi
138200060829     C                   Z-ADD     *ZEROS        $pes
138300040811     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
138400040811    1C     TASpkC        IFGT      *ZEROS
138500040811     C     colli         ANDEQ     TASNCp
138600040811     C                   Z-ADD     TASpkc        $pes                           *VOLUME CML
138700040811E   1C                   ENDIF
138800040811     C*
138900040811     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
139000040811     C     TASpkc        IFGT      TASpkF
139100040811     C     colli         ANDNE     TASNCp
139200040811     C                   Z-ADD     TASpkC        $pes                           *VOLUME CML
139300040811E   1C                   ENDIF
139400040811     C*
139500040811     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
139600040811IF  1C     $pes          IFEQ      *ZEROS
139700040811     C                   Z-ADD     TASpkF        $pes                           *VOLUME NORMALE
139800040811E   1C                   ENDIF
139900060829     C* colli x competenze
140000060829     C                   Z-ADD     *ZEROS        $pes1
140100060829     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
140200060829    1C     TASpkC        IFGT      *ZEROS
140300060829     C     colli1        ANDEQ     TASNCp
140400060829     C                   Z-ADD     TASpkc        $pes1                          *VOLUME CML
140500060829E   1C                   ENDIF
140600060829     C*
140700060829     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
140800060829     C     TASpkc        IFGT      TASpkF
140900060829     C     colli1        ANDNE     TASNCp
141000060829     C                   Z-ADD     TASpkC        $pes1                          *VOLUME CML
141100060829E   1C                   ENDIF
141200060829     C*
141300060829     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
141400060829IF  1C     $pes1         IFEQ      *ZEROS
141500060829     C                   Z-ADD     TASpkF        $pes1                          *VOLUME NORMALE
141600060829E   1C                   ENDIF
141700040811     C*
141800040811     C                   ENDSR
141900970110     C*------------------------------------------------------------------------*
142000970110     C* REPCAS - REPERISCE IL CONTRASSEGNO
142100970110     C*------------------------------------------------------------------------*
142200970110     C     REPCAS        BEGSR
142300970110     C*
142400970110     C                   Z-ADD     TASAAS        KSBAAS                         *ANNO
142500970110     C                   Z-ADD     TASLNP        KSBLNP                         *LINEA PARTENZA
142600970110     C                   Z-ADD     TASNRS        KSBNRS                         *SERIE
142700970110     C                   Z-ADD     TASNSP        KSBNSP                         *SPEDIZIONE
142800970110     C     KEYCSB        CHAIN     TNCSB03L                           98
142900970110IF  1C     *IN98         IFEQ      *OFF                                         *TROVATO
143000970110IF  2C     CSBSTA        IFNE      9                                            *NO ANNULLATO
143100970110     C                   MOVEL     CSBTPI        $TPI                           *TIPO INCASSO
143200011011     C                   Z-ADD     CSBCAS        $CAS                           *C/ASSEGNO
143300011011     C     CSBVCA        IFNE      PARDIV
143400011011     C                   Z-ADD     $CAS          WIMI             30 9
143500011011     C                   MOVEL     CSBVCA        WDVI              3
143600011011     C                   EXSR      CNVEUR
143700011011     C                   Z-ADD     WIMO          $CAS
143800011011     C                   ENDIF
143900970110E   2C                   ENDIF
144000970110E   1C                   ENDIF
144100970110     C*
144200970110     C                   ENDSR
144300021122     C*------------------------------------------------------------------------*
144400021122     C* REPGIA - REPERISCE LE GIACENZE
144500021122     C*------------------------------------------------------------------------*
144600021122     C     REPGIA        BEGSR
144700021122     C*
144800021122     C* AZZERA LE VARIABILI DI LAVORO
144900021122     C                   Z-ADD     *ZEROS        §GGSOS            3 0
145000021122     C*
145100021122     C* LEGGE TUTTE LE GIACENZE DELLA BOLLA
145200021122     C                   Z-ADD     TASAAS        KGPAAS
145300021122     C                   Z-ADD     TASLNP        KGPLNP
145400021122     C                   Z-ADD     TASNRS        KGPNRS
145500021122     C                   Z-ADD     TASNSP        KGPNSP
145600050309     C     K01GCP        SETLL     TIGCP51L
145700050309     C     K01GCP        READE     TIGCP51L                               98
145800021122DO  1C     *IN98         DOWEQ     *OFF
145900021122IF  2C     GCPATB        IFEQ      *BLANKS
146000021122     C                   MOVEL     'S'           $PGI                           *ESISTE GIACENZA
146100021122     C* APERTURE
146200021122     C                   ADD       1             $PGINR                         *NUMERO RIAPERTURE
146300021122     C* GIORNI DI SOSTA
146400021122IF  3C     GCPDDM        IFGT      *ZEROS                                       *IMMESSE DISPOSIZION
146500021122IF  4C     GCPDUR        IFGT      *ZEROS                                       *ULTIMA RIAPERTURA
146600021122     C                   Z-ADD     GCPDUR        G08INV
146700021122X   4C                   ELSE                                                   *APERTURA GIACENZA
146800021122     C                   MOVEL     GCPAGC        G08INV
146900021122     C                   MOVE      GCPMGC        G08INV
147000021122E   4C                   ENDIF
147100021122     C                   Z-ADD     *ZEROS        G08DAT
147200021122     C                   MOVEL     '3'           G08ERR
147300021122     C                   CALL      'XSRDA8'
147400021122     C                   PARM                    WLBDA8
147500021122     C                   Z-ADD     G08TGI        §GGDA             5 0
147600021122IF  4C     GCPDED        IFGT      *ZEROS                                       *DISPOSIZ.GIACENZA
147700021122     C                   Z-ADD     GCPDED        G08INV
147800021122X   4C                   ELSE                                                   *DISPOSIZ.MITTENTE
147900021122     C                   Z-ADD     GCPDDM        G08INV
148000021122E   4C                   ENDIF
148100021122     C                   Z-ADD     *ZEROS        G08DAT
148200021122     C                   MOVEL     '3'           G08ERR
148300021122     C                   CALL      'XSRDA8'
148400021122     C                   PARM                    WLBDA8
148500021122     C                   Z-ADD     G08TGI        §GGA              5 0
148600021122     C* GIORNI DI SOSTA
148700021122     C     §GGA          SUB       §GGDA         §GGXX             3 0
148800021122     C                   ADD       1             §GGXX
148900021122     C                   ADD       §GGXX         §GGSOS
149000021122E   3C                   ENDIF
149100021122E   2C                   ENDIF
149200050309     C     K01GCP        READE     TIGCP51L                               98
149300021122E   1C                   ENDDO
149400021122     C*
149500021122     C                   Z-ADD     §GGSOS        $PGIGS                         *GIORNI DI SOSTA
149600021122     C*
149700021122     C                   ENDSR
149800970110     C*------------------------------------------------------------------------*
149900970110     C* REPASS - REPERISCE L'ASSICURAZIONE
150000970110     C*------------------------------------------------------------------------*
150100970110     C     REPASS        BEGSR
150200060315     c                   clear                   flgacbase         1
150300970110     C*
150400970110IF  1C     TASIAS        IFGT      *ZEROS
150500011011     C                   Z-ADD     TASIAS        $IAS
150600011011IF  2C     TASVAS        IFNE      PARDIV                                       *DIVISA ESTERA
150700011011     C                   Z-ADD     $IAS          WIMI             30 9
150800011011     C                   MOVEL     TASVAS        WDVI              3
150900011011     C                   EXSR      CNVEUR
151000011011     C                   Z-ADD     WIMO          $IAS
151100011011     C                   ENDIF
151200060315     c* verifico se esiste ac base
151300060315     c                   else
151400060315     c                   z-add     1             v
151500060315     c     'd'           lookup    svn(v)                                 99
151600060320     c                   if        van(v) <> 0 and *in99
151700060315     c                   eval      flgacbase = *on
151800970110E   1C                   ENDIF
151900060315E   1C                   ENDIF
152000970110     C*
152100970110     C                   ENDSR
152200970110     C*------------------------------------------------------------------------*
152300060315     C* REPVrf - REPERISCE le varie di fatturazione
152400990812     C* -----------------------------------------------------------------------*
152500060315     C     REPVrf        BEGSR
152600060315     C* CARICO LE VARIE BOLLA ORIGINALE DA TITAS
152700060315     C                   Z-ADD     *ZEROS        V                 6 0
152800060315     C     tasSV1        IFNE      *BLANKS
152900060315     C                   ADD       1             V
153000060315     C                   MOVEL     tasSV1        SVN(V)
153100060315     C                   Z-ADD     tasVA1        VAN(V)
153200060315     C                   ENDIF
153300060315     C     tasSV2        IFNE      *BLANKS
153400060315     C                   ADD       1             V
153500060315     C                   MOVEL     tasSV2        SVN(V)
153600060315     C                   Z-ADD     tasVA2        VAN(V)
153700060315     C                   ENDIF
153800060315     C     tasSV3        IFNE      *BLANKS
153900060315     C                   ADD       1             V
154000060315     C                   MOVEL     tasSV3        SVN(V)
154100060315     C                   Z-ADD     tasVA3        VAN(V)
154200060315     C                   ENDIF
154300060315     C* CARICO LE VARIE BOLLA DA TITA7 (SOLO SE C'E' LA VARIA 3)
154400060315     C     tasSV3        IFNE      *BLANKS
154500060315     C                   Z-ADD     tasAAS        TA7AAS
154600060315     C                   Z-ADD     tasLNP        TA7LNP
154700060315     C                   Z-ADD     tasNRS        TA7NRS
154800060315     C                   Z-ADD     tasNSP        TA7NSP
154900060315     C                   MOVEL     tasTBL        TA7TBL
155000060315     C     KEYTA7        SETLL     TITA730C
155100060315     c                   do        *hival
155200060315     C     KEYTA7        READE     TITA730C
155300060315     c                   if        %eof(tita730c)
155400060315     c                   leave
155500060315     c                   end
155600060315if  1C                   IF        (TA7VAN = 88888888,000)
155700060315     C                   Z-ADD     *zeros        TA7VAN
155800060315e   1c                   ENDIF
155900060315     C                   ADD       1             V
156000060315     C                   MOVEL     TA7SVN        SVN(V)
156100060315     C                   Z-ADD     TA7VAN        VAN(V)
156200060315     C                   ENDDO
156300060315e   1c                   ENDIF
156400060315     c                   endsr
156500060315     C*------------------------------------------------------------------------*
156600060315     C* REPVAR - REPERISCE LE consegne particolari
156700060315     C* -----------------------------------------------------------------------*
156800060315     C     REPVAR        BEGSR
156900970110     C*
157000970110     C* VARIE -BOLLA-
157100021122IF  1C     TASFTC        IFEQ      'A'
157200021122     C     TASTC2        OREQ      'A'
157300970110     C                   MOVEL     'S'           $PCA                           *CONS APPUNTAMENTO
157400970110E   1C                   ENDIF
157500021122IF  1C     TASFTC        IFEQ      'P'
157600021122     C     TASTC2        OREQ      'P'
157700970110     C                   MOVEL     'S'           $PCP                           *CONS PIANI
157800970110E   1C                   ENDIF
157900021122IF  1C     TASFTC        IFEQ      'S'
158000021122     C     TASTC2        OREQ      'S'
158100970110     C                   MOVEL     'S'           $PCS                           *CONS SUPERMERCATI
158200970110E   1C                   ENDIF
158300021122IF  1C     TASFTC        IFEQ      'F'
158400021122     C     TASTC2        OREQ      'F'
158500970110     C                   MOVEL     'S'           $PCF                           *CONS FUORI MISURA
158600970110E   1C                   ENDIF
158700040802IF  1C     TASFTC        IFEQ      'X'
158800040802     C     TASTC2        OREQ      'X'
158900040802     C                   MOVEL     'S'           $ded                           *CONS FUORI MISURA
159000040802E   1C                   ENDIF
159100021122IF  1C     TASLL1        IFEQ      'S'
159200970415     C                   MOVEL     'S'           $PCD                           *CONS DDT
159300970415E   1C                   ENDIF
159400970110     C*
159500970110     C                   ENDSR
159600980819     C*------------------------------------------------------------------------*
159700980819     C* AGGETX - AGGIORNA I DATI TIPO
159800980819     C*------------------------------------------------------------------------*
159900980819     C     AGGETX        BEGSR
160000980819     C*
160100980819     C* PER OGNI LINEA DI PARTENZA RICHIESTA
160200980819DO  1C     1             DO        500           I
160300980819IF  2C     SORG(I)       IFGT      *ZEROS
160400980819IF  3C     SPE(I)        IFGT      *ZEROS                                       *MEMORIZZATO QC.
160500980821     C                   Z-ADD     SORG(I)       XORG
160600980819     C     I             OCCUR     FPAG
160700980819     C     I             OCCUR     FCON
160800980819     C     I             OCCUR     FRIT
160900980819     C     I             OCCUR     FSGL
161000060829     C     I             OCCUR     FSGL1
161100980819     C     I             OCCUR     FCTS
161200980819     C     I             OCCUR     FTAR                                         *DATI TARIFFA
161300980819     C*---
161400980819     C* DATI TIPO -GENERALI-
161500980819     C*---
161600980821     C                   EXSR      SCRETC
161700980819     C*---
161800980819     C* DATI TIPO -SCAGLIONI-
161900980819     C*---
162000060829     c* ricavi
162100980821     C                   EXSR      SCRETS
162200060829     c* competenze
162300060829     c                   if        etcbnb <> 0
162400060829     c                   exsr      screts1
162500060829     c                   end
162600980819     C*---
162700980819     C* DATI TIPO -CODICI TASSAZIONE-
162800980819     C*---
162900980819     C* PER OGNI TIPO TARIFFA (ITALIA/ESTERO)
163000980819DO  4C     1             DO        2             K
163100980819IF  5C     SFIE(K)       IFNE      *BLANKS
163200980821     C                   MOVEL     SFIE(K)       XFIE
163300980821     C                   EXSR      SCRETT
163400981215E   5C                   ENDIF
163500980819E   4C                   ENDDO                                                  *FINE TARIFFA ITA/ES
163600980819     C*
163700980819E   3C                   ENDIF
163800980819E   2C                   ENDIF
163900980819E   1C                   ENDDO                                                  *FINE FILIALI
164000980819     C*
164100980821     C                   ENDSR
164200980819     C*------------------------------------------------------------------------*
164300980819     C* AGGET8 - AGGIORNA I DATI TIPO DI CARTELLO
164400980819     C*------------------------------------------------------------------------*
164500980819     C     AGGET8        BEGSR
164600970110     C*
164700980819     C* PER OGNI LINEA DI PARTENZA MEMORIZZATA
164800971124DO  1C     1             DO        500           I
164900980820     C     I             OCCUR     FPAG
165000970110     C     I             OCCUR     FCON
165100970110     C     I             OCCUR     FRIT
165200970110     C     I             OCCUR     FSGL
165300060829     C     I             OCCUR     FSGL1
165400970110     C     I             OCCUR     FCTS
165500980819     C     I             OCCUR     FTAR
165600980819     C*
165700980819     C* SOMMA I DATI PER LE TARIFFE DI CARTELLO -ITA/EST- DI TUTTE LE FILIALI
165800980819IF  2C     SFL1(I)       IFEQ      *BLANKS                                      *FILIALE ITALIA
165900980824     C                   Z-ADD     1             U                 4 0
166000980819X   2C                   ELSE                                                   *FILIALE ESTERO
166100980819     C                   Z-ADD     2             U
166200980819E   2C                   ENDIF
166300980824     C     U             OCCUR     CCAR
166400980824     C     U             OCCUR     CPAG
166500980824     C     U             OCCUR     CCON
166600980824     C     U             OCCUR     CRIT
166700980824     C     U             OCCUR     CSGL
166800060829     C     U             OCCUR     CSGL1
166900980824     C     U             OCCUR     CCTS
167000980819     C*
167100980819     C* DATI TIPO -GENERALI-
167200980821     C                   ADD       SPE(I)        USPE(1)
167300980821     C                   ADD       CLA(I)        UCLA(1)
167400980821     C                   ADD       CLB(I)        UCLB(1)
167500980821     C                   ADD       CLC(I)        UCLC(1)
167600980821     C                   ADD       BON(I)        UBON(1)
167700980821     C                   ADD       BOA(I)        UBOA(1)
167800980821     C                   ADD       BOB(I)        UBOB(1)
167900980821     C                   ADD       NCL(I)        UNCL(1)
168000980821     C                   ADD       PKB(I)        UPKB(1)
168100980821     C                   ADD       VLB(I)        UVLB(1)
168200060829     C                   ADD       NCL1(I)       UNCL1(1)
168300060829     C                   ADD       PKB1(I)       UPKB1(1)
168400060829     C                   ADD       VLB1(I)       UVLB1(1)
168500980821     C                   ADD       FRA(I)        UFRA(1)
168600980821     C                   ADD       ASS(I)        UASS(1)
168700980821     C                   ADD       PCV(I)        UPCV(1)
168800980821     C                   ADD       PCM(I)        UPCM(1)
168900980821     C                   ADD       PFD(I)        UPFD(1)
169000980821     C                   ADD       ESP(I)        UESP(1)
169100980821     C                   ADD       PPV(I)        UPPV(1)
169200980821     C                   ADD       PCA(I)        UPCA(1)
169300980821     C                   ADD       PCP(I)        UPCP(1)
169400980821     C                   ADD       PCS(I)        UPCS(1)
169500980821     C                   ADD       PCF(I)        UPCF(1)
169600980821     C                   ADD       PCD(I)        UPCD(1)
169700980821     C                   ADD       PCC(I)        UPCC(1)
169800980821     C                   ADD       PCL(I)        UPCL(1)
169900980821     C                   ADD       PAB(I)        UPAB(1)
170000980821     C                   ADD       PAM(I)        UPAM(1)
170100980821     C                   ADD       CAS(I)        UCAS(1)
170200980821     C                   ADD       PAR(I)        UPAR(1)
170300980821     C                   ADD       PAC(I)        UPAC(1)
170400060315     C                   ADD       xx1(I)        Uxx1(1)
170500980821     C                   ADD       ASC(I)        UASC(1)
170600980821     C                   ADD       PGI(I)        UPGI(1)
170700000414     C                   ADD       GGS(I)        UGGS(1)
170800000414     C                   ADD       NRG(I)        UNRG(1)
170900980901     C                   ADD       CLT(I)        UCLT(1)
171000011011     C                   ADD       CIS(I)        UCIS(1)
171100011011     C                   ADD       RIS(I)        URIS(1)
171200020215     C                   ADD       CLD(I)        UCLD(1)
171300050808     C                   ADD       pks(I)        Upks(1)
171400050808     C                   ADD       pkn(I)        Upkn(1)
171500040802     C                   ADD       ded(I)        Uded(1)
171600040811     C                   ADD       sbnb(I)       Usbnb(1)
171700040824     C                   ADD       sban(I)       Usban(1)
171800040811     C                   ADD       nbn(I)        Unbn(1)
171900040802     C                   ADD       nba(I)        Unba(1)
172000040802     C                   ADD       val(I)        Uval(1)
172100980825DO  2C     1             DO        30            J
172200980825IF  3C     GGP(J)        IFGT      *ZEROS
172300980825     C                   Z-ADD     1             A                 4 0
172400980825     C                   SETOFF                                           98
172500980825     C     GGP(J)        LOOKUP    UGGP(A)                                98
172600980825IF  4C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
172700980825     C                   ADD       SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
172800980825X   4C                   ELSE                                                   *MAI MEMORIZZATI
172900980825     C                   Z-ADD     1             A
173000980825DO  5C     A             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
173100980825IF  6C     UGGP(A)       IFEQ      *ZEROS                                       *DISPONIBILE
173200980825     C                   Z-ADD     GGP(J)        UGGP(A)                        *GG
173300980825     C                   Z-ADD     SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
173400980825     C                   Z-ADD     30            A                              *ESCE DAL CICLO
173500980825E   6C                   ENDIF
173600980825     C                   ADD       1             A
173700980825E   5C                   ENDDO
173800980825E   4C                   ENDIF
173900980825E   3C                   ENDIF
174000980825E   2C                   ENDDO
174100980819DO  2C     1             DO        999           J
174200980819     C                   ADD       RIT(J)        URIT(J)
174300980819     C                   ADD       CON(J)        UCON(J)
174400980819E   2C                   ENDDO
174500980819     C*
174600040920     C* DATI TIPO -SCAGLIONI- (già caricati nella durante la lettura di
174700040920     c* titas)
174800980820     C*
174900970110     C* DATI TIPO -CODICI TASSAZIONE-
175000050103DO  2C     1             DO        600           J
175100980819     C                   ADD       TSI(J)        UTSI(J)
175200980819     C                   ADD       TSE(J)        UTSE(J)
175300980819E   2C                   ENDDO
175400970110E   1C                   ENDDO                                                  *FINE FILIALI
175500980821     C*---
175600980821     C* SCRIVE LA TARIFFA -888- ITALIA
175700980821     C*---
175800980820     C*
175900980821     C* CARICA LE SCHIERE DA ELABORARE
176000980821     C                   Z-ADD     1             U
176100980821     C                   Z-ADD     1             I
176200980821     C                   Z-ADD     888           XORG
176300980821     C                   EXSR      CARSCH
176400980821     C*
176500980821     C* DATI TIPO -GENERALI-
176600980821     C                   EXSR      SCRETC
176700980821     C*
176800980821     C* DATI TIPO -SCAGLIONI-
176900060829     c* ricavi
177000980821     C                   EXSR      SCRETS
177100060829     c* competenze
177200060829     c                   if        etcbnb <> 0
177300060829     C                   EXSR      SCRETS1
177400060829     c                   end
177500980821     C*
177600980821     C* DATI TIPO -CODICI TASSAZIONE-
177700980824     C                   MOVEL     'I'           XFIE
177800980821     C                   EXSR      SCRETT
177900980824     C*---
178000980824     C* SCRIVE LA TARIFFA -889- ESTERO
178100980824     C*---
178200980824     C*
178300980824     C* CARICA LE SCHIERE DA ELABORARE
178400980824     C                   Z-ADD     2             U
178500980824     C                   Z-ADD     1             I
178600980824     C                   Z-ADD     889           XORG
178700980824     C                   EXSR      CARSCH
178800980824     C*
178900980824     C* DATI TIPO -GENERALI-
179000980824     C                   EXSR      SCRETC
179100980824     C*
179200980824     C* DATI TIPO -SCAGLIONI-
179300060829     c* ricavi
179400980824     C                   EXSR      SCRETS
179500060829     c* competenze
179600060829     c                   if        etcbnb <> 0
179700060829     C                   EXSR      SCRETS1
179800060829     c                   end
179900980824     C*
180000980824     C* DATI TIPO -CODICI TASSAZIONE-
180100980824     C                   MOVEL     'E'           XFIE
180200980824     C                   EXSR      SCRETT
180300970110     C*
180400980819     C                   ENDSR
180500040920     C*------------------------------------------------------------------------*
180600040920     C* SRSGL8 - CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
180700040920     C*------------------------------------------------------------------------*
180800040920     C     SRSGL8        BEGSR
180900040920     C* 888 ITALIA
181000040920     C                   IF        SFL1(F) = ' '
181100040920     C                   Z-ADD     1             U
181200040920     C                   ELSE
181300040920     C                   Z-ADD     2             U
181400040920     C                   END
181500040920     C     U             OCCUR     CSGL
181600040920     C*
181700040920DO  5C     1             DO        10            I
181800040920IF  6C     TASSGL        IFLE      SGL(I)
181900040920     C                   ADD       1             USSP(I)                        *N°SPED SCAGLIONE
182000040920     C                   ADD       TASPKF        UPSC(I)                        *PESO
182100040920     C                   ADD       TASNCL        UCSC(I)                        *COLLI
182200040920     C                   ADD       $VOL          UVSC(I)                        *VOLUME
182300040920     C                   LEAVE
182400040920E   6C                   ENDIF
182500040920E   5C                   ENDDO
182600040920     C*
182700040920     C                   ENDSR
182800060829     C*------------------------------------------------------------------------*
182900060829     C* SRSGL81- CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
183000060829     C*------------------------------------------------------------------------*
183100060829     C     SRSGL81       BEGSR
183200060829     C* 888 ITALIA
183300060829     C                   IF        SFL1(F) = ' '
183400060829     C                   Z-ADD     1             U
183500060829     C                   ELSE
183600060829     C                   Z-ADD     2             U
183700060829     C                   END
183800060829     C     U             OCCUR     CSGL1
183900060829     C*
184000060829DO  5C     1             DO        10            I
184100060829IF  6C     TASSGL        IFLE      SGL(I)
184200060829     C                   ADD       1             USSP1(I)                       *N°SPED SCAGLIONE
184300060829     C                   ADD       TASPKF        UPSC1(I)                       *PESO
184400060829     C                   ADD       TASNCL        UCSC1(I)                       *COLLI
184500060829     C                   ADD       $VOL1         UVSC1(I)                       *VOLUME
184600060829     C                   LEAVE
184700060829E   6C                   ENDIF
184800060829E   5C                   ENDDO
184900060829     C*
185000060829     C                   ENDSR
185100980821     C*------------------------------------------------------------------------*
185200980821     C* CARSCH - CARICA LE SCHIERE DA ELABORARE
185300980821     C*------------------------------------------------------------------------*
185400980821     C     CARSCH        BEGSR
185500980821     C*
185600980821     C* SI POSIZIONA SULLE SCHIERE DELLA TARIFFA
185700980824     C     1             OCCUR     FPAG
185800980824     C     1             OCCUR     FCON
185900980824     C     1             OCCUR     FRIT
186000980824     C     1             OCCUR     FSGL
186100060829     C     1             OCCUR     FSGL1
186200980824     C     1             OCCUR     FCTS
186300980824     C     1             OCCUR     FTAR
186400980824     C     U             OCCUR     CCAR
186500980824     C     U             OCCUR     CPAG
186600980824     C     U             OCCUR     CCON
186700980824     C     U             OCCUR     CRIT
186800980824     C     U             OCCUR     CSGL
186900060829     C     U             OCCUR     CSGL1
187000980824     C     U             OCCUR     CCTS
187100980821     C*
187200980821     C* DATI TIPO -GENERALI-
187300980824     C                   Z-ADD     USPE(1)       SPE(1)
187400980824     C                   Z-ADD     UCLA(1)       CLA(1)
187500980824     C                   Z-ADD     UCLB(1)       CLB(1)
187600980824     C                   Z-ADD     UCLC(1)       CLC(1)
187700980824     C                   Z-ADD     UBON(1)       BON(1)
187800980824     C                   Z-ADD     UBOA(1)       BOA(1)
187900980824     C                   Z-ADD     UBOB(1)       BOB(1)
188000980824     C                   Z-ADD     UNCL(1)       NCL(1)
188100980824     C                   Z-ADD     UPKB(1)       PKB(1)
188200980824     C                   Z-ADD     UVLB(1)       VLB(1)
188300060829     C                   Z-ADD     UNCL1(1)      NCL1(1)
188400060829     C                   Z-ADD     UPKB1(1)      PKB1(1)
188500060829     C                   Z-ADD     UVLB1(1)      VLB1(1)
188600980824     C                   Z-ADD     UFRA(1)       FRA(1)
188700980824     C                   Z-ADD     UASS(1)       ASS(1)
188800980824     C                   Z-ADD     UPCV(1)       PCV(1)
188900980824     C                   Z-ADD     UPCM(1)       PCM(1)
189000980824     C                   Z-ADD     UPFD(1)       PFD(1)
189100980824     C                   Z-ADD     UESP(1)       ESP(1)
189200980824     C                   Z-ADD     UPPV(1)       PPV(1)
189300980824     C                   Z-ADD     UPCA(1)       PCA(1)
189400980824     C                   Z-ADD     UPCP(1)       PCP(1)
189500980824     C                   Z-ADD     UPCS(1)       PCS(1)
189600980824     C                   Z-ADD     UPCF(1)       PCF(1)
189700980824     C                   Z-ADD     UPCD(1)       PCD(1)
189800980824     C                   Z-ADD     UPCC(1)       PCC(1)
189900980824     C                   Z-ADD     UPCL(1)       PCL(1)
190000980824     C                   Z-ADD     UPAB(1)       PAB(1)
190100980824     C                   Z-ADD     UPAM(1)       PAM(1)
190200980824     C                   Z-ADD     UCAS(1)       CAS(1)
190300980824     C                   Z-ADD     UPAR(1)       PAR(1)
190400980824     C                   Z-ADD     UPAC(1)       PAC(1)
190500060315     C                   Z-ADD     Uxx1(1)       xx1(1)
190600980824     C                   Z-ADD     UASC(1)       ASC(1)
190700980824     C                   Z-ADD     UPGI(1)       PGI(1)
190800000414     C                   Z-ADD     UGGS(1)       GGS(1)
190900000414     C                   Z-ADD     UNRG(1)       NRG(1)
191000980901     C                   Z-ADD     UCLT(1)       CLT(1)
191100011011     C                   Z-ADD     UCIS(1)       CIS(1)
191200011011     C                   Z-ADD     URIS(1)       RIS(1)
191300020215     C                   Z-ADD     UCLD(1)       CLD(1)
191400050808     C                   Z-ADD     Upks(1)       pks(1)
191500050808     C                   Z-ADD     Upkn(1)       pkn(1)
191600040811     C                   Z-ADD     Usbnb(1)      sbnb(1)
191700040824     C                   Z-ADD     Usban(1)      sban(1)
191800040811     C                   Z-ADD     Unbn(1)       nbn(1)
191900040802     C                   Z-ADD     Unba(1)       nba(1)
192000040802     C                   Z-ADD     Uded(1)       ded(1)
192100040802     C                   Z-ADD     Uval(1)       val(1)
192200980821DO  1C     1             DO        30            J
192300980821     C                   Z-ADD     USGP(J)       SGP(J)
192400980821     C                   Z-ADD     UGGP(J)       GGP(J)
192500980821E   1C                   ENDDO
192600980821DO  1C     1             DO        999           J
192700980821     C                   Z-ADD     URIT(J)       RIT(J)
192800980821     C                   Z-ADD     UCON(J)       CON(J)
192900980821E   1C                   ENDDO
193000980821     C*
193100060829     C* DATI TIPO -SCAGLIONI- ricavi
193200980824DO  1C     1             DO        10            J
193300980825     C                   Z-ADD     USSP(J)       SSP(J)
193400980825     C                   Z-ADD     UPSC(J)       PSC(J)
193500980825     C                   Z-ADD     UCSC(J)       CSC(J)
193600980825     C                   Z-ADD     UVSC(J)       VSC(J)
193700980824E   1C                   ENDDO
193800060829     C* DATI TIPO -SCAGLIONI- competenze
193900060829DO  1C     1             DO        10            J
194000060829     C                   Z-ADD     USSP1(J)      SSP1(J)
194100060829     C                   Z-ADD     UPSC1(J)      PSC1(J)
194200060829     C                   Z-ADD     UCSC1(J)      CSC1(J)
194300060829     C                   Z-ADD     UVSC1(J)      VSC1(J)
194400060829E   1C                   ENDDO
194500980821     C*
194600980821     C* DATI TIPO -CODICI TASSAZIONE-
194700050103DO  1C     1             DO        600           J                 4 0
194800980825     C                   Z-ADD     UTSI(J)       TSI(J)
194900980825     C                   Z-ADD     UTSE(J)       TSE(J)
195000980824E   1C                   ENDDO
195100980821     C*
195200980821     C                   ENDSR
195300980821     C*------------------------------------------------------------------------*
195400980821     C* AZZSCH - AZZERA LE VARIABILI DI LAVORO
195500980821     C*------------------------------------------------------------------------*
195600980821     C     AZZSCH        BEGSR
195700980821     C*
195800980821     C                   CLEAR                   SPE                            -GENERALI-
195900980821     C                   CLEAR                   CLA
196000980821     C                   CLEAR                   CLB
196100980821     C                   CLEAR                   CLC
196200980821     C                   CLEAR                   BON
196300980821     C                   CLEAR                   BOA
196400980821     C                   CLEAR                   BOB
196500980821     C                   CLEAR                   NCL
196600980821     C                   CLEAR                   PKB
196700980821     C                   CLEAR                   VLB
196800060829     C                   CLEAR                   NCL1
196900060829     C                   CLEAR                   PKB1
197000060829     C                   CLEAR                   VLB1
197100980821     C                   CLEAR                   FRA
197200980821     C                   CLEAR                   ASS
197300980821     C                   CLEAR                   PCV
197400980821     C                   CLEAR                   PCM
197500980821     C                   CLEAR                   PFD
197600980821     C                   CLEAR                   ESP
197700980821     C                   CLEAR                   PPV
197800980821     C                   CLEAR                   PCA
197900980821     C                   CLEAR                   PCP
198000980821     C                   CLEAR                   PCS
198100980821     C                   CLEAR                   PCF
198200980821     C                   CLEAR                   PCD
198300980821     C                   CLEAR                   PCC
198400980821     C                   CLEAR                   PCL
198500980821     C                   CLEAR                   PAB
198600980821     C                   CLEAR                   PAM
198700980821     C                   CLEAR                   CAS
198800980821     C                   CLEAR                   PAR
198900980821     C                   CLEAR                   PAC
199000060315     C                   CLEAR                   xx1
199100980821     C                   CLEAR                   ASC
199200980821     C                   CLEAR                   PGI
199300980821     C                   CLEAR                   SGP
199400980821     C                   CLEAR                   GGP
199500980821     C                   CLEAR                   RIT
199600980821     C                   CLEAR                   CON
199700980901     C                   CLEAR                   CLT
199800000414     C                   CLEAR                   GGS
199900000414     C                   CLEAR                   NRG
200000980821     C                   CLEAR                   SSP                            -SCAGLIONI-
200100980821     C                   CLEAR                   PSC
200200980821     C                   CLEAR                   CSC
200300980821     C                   CLEAR                   VSC
200400060829     C                   CLEAR                   SSP1                           -SCAGLIONI-
200500060829     C                   CLEAR                   PSC1
200600060829     C                   CLEAR                   CSC1
200700060829     C                   CLEAR                   VSC1
200800980821     C                   CLEAR                   TSI                            -CODICI TASSAZIONE-
200900980821     C                   CLEAR                   TSE
201000011011     C                   CLEAR                   CIS
201100011011     C                   CLEAR                   RIS
201200020215     C                   CLEAR                   CLD
201300050808     C                   CLEAR                   pks
201400050808     C                   CLEAR                   pkn
201500040802     C                   CLEAR                   nba
201600040824     C                   CLEAR                   sban
201700040824     C                   CLEAR                   sbnb
201800040811     C                   CLEAR                   nbn
201900040802     C                   CLEAR                   ded
202000040802     C                   CLEAR                   val
202100980821     C*
202200980821     C                   ENDSR
202300980821     C*------------------------------------------------------------------------*
202400011001     C* SCRETC - SCRIVE UN RECORD DI TEETC -GENERALI-
202500980821     C*------------------------------------------------------------------------*
202600980821     C     SCRETC        BEGSR
202700980821     C*
202800060804     C                   MOVEL     'S'           KTCDSF
202900980821     C                   MOVEL     PARCTO        KTCCTO
203000980821     C                   Z-ADD     PARKOV        KTCKSC
203100980821     C                   Z-ADD     PARCTR        KTCCTR
203200980821     C                   Z-ADD     PARPRG        KTCPRG
203300980821     C                   Z-ADD     XORG          KTCLNP
203400021122     C     KEYETC        CHAIN     TEETC01L                           9697
203500980821     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
203600021122IF  2C     *IN97         IFEQ      *OFF                                         *NON ALLOCATO
203700021122IF  3C     *IN96         IFEQ      *ON
203800980821     C                   MOVEL     KTCDSF        ETCDSF
203900980821     C                   MOVEL     KTCCTO        ETCCTO
204000980821     C                   Z-ADD     KTCKSC        ETCKSC
204100980821     C                   Z-ADD     KTCCTR        ETCCTR
204200980821     C                   Z-ADD     KTCPRG        ETCPRG
204300980821     C                   Z-ADD     KTCLNP        ETCLNP
204400980821     C                   Z-ADD     DATCOR        ETCDIR
204500980821     C                   Z-ADD     *ZEROS        ETCDUV
204600021122     C                   MOVEL     *BLANKS       ETCATB
204700011001     C                   WRITE     TEETC000                                     *INSERIMENTO
204800011001     C                   UNLOCK    TEETC01L
204900980821X   3C                   ELSE
205000980821     C                   Z-ADD     DATCOR        ETCDUV
205100021122     C                   MOVEL     *BLANKS       ETCATB
205200011001     C                   UPDATE    TEETC000                                     *VARIAZIONE
205300980821E   3C                   ENDIF
205400980821X   2C                   ELSE                                                   *ALLOCATO
205500980821     C                   MOVEL     METX(1)       DEPERR
205600980821     C                   MOVEL     'X'           DEPTER
205700980821     C                   EXSR      STAERR
205800980821E   2C                   ENDIF
205900980821     C*
206000980821     C                   ENDSR
206100980821     C*------------------------------------------------------------------------*
206200011001     C* SCRETS - SCRIVE UN RECORD DI TEETS -SCAGLIONI-
206300980821     C*------------------------------------------------------------------------*
206400980821     C     SCRETS        BEGSR
206500980821     C*
206600980821     C                   EXSR      SCRSGL                                       *SRIVE SCHIERE SGL
206700980821DO  1C     1             DO        10            J
206800980821IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
206900060804     C                   MOVEL     'S'           KTSDSF
207000980821     C                   MOVEL     PARCTO        KTSCTO
207100980821     C                   Z-ADD     PARKOV        KTSKSC
207200980821     C                   Z-ADD     PARCTR        KTSCTR
207300980821     C                   Z-ADD     PARPRG        KTSPRG
207400980821     C                   Z-ADD     XORG          KTSLNP
207500980821     C                   Z-ADD     §SG(J)        KTSSGL
207600021122     C     KEYETS        CHAIN     TEETS01L                           96
207700980821     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
207800021122IF  4C     *IN96         IFEQ      *ON
207900980821     C                   MOVEL     KTSDSF        ETSDSF
208000980821     C                   MOVEL     KTSCTO        ETSCTO
208100980821     C                   Z-ADD     KTSKSC        ETSKSC
208200980821     C                   Z-ADD     KTSCTR        ETSCTR
208300980821     C                   Z-ADD     KTSPRG        ETSPRG
208400980821     C                   Z-ADD     KTSLNP        ETSLNP
208500980821     C                   Z-ADD     KTSSGL        ETSSGL
208600980821     C                   Z-ADD     DATCOR        ETSDIR
208700980821     C                   Z-ADD     *ZEROS        ETSDUV
208800021122     C                   MOVEL     *BLANKS       ETSATB
208900011001     C                   WRITE     TEETS000                                     *INSERIMENTO
209000011001     C                   UNLOCK    TEETS01L
209100980821X   4C                   ELSE
209200980821     C                   Z-ADD     DATCOR        ETSDUV
209300021122     C                   MOVEL     *BLANKS       ETSATB
209400011001     C                   UPDATE    TEETS000                                     *VARIAZIONE
209500980821E   4C                   ENDIF
209600980821E   2C                   ENDIF
209700980821E   1C                   ENDDO                                                  *FINE SCAGLIONI
209800980821     C*
209900980821     C                   ENDSR
210000060829     C*------------------------------------------------------------------------*
210100060829     C* SCRETS1- SCRIVE UN RECORD DI TEETS10F -SCAGLIONI-
210200060829     C*------------------------------------------------------------------------*
210300060829     C     SCRETS1       BEGSR
210400060829     C*
210500060829     C                   EXSR      SCRSGL1                                      *SRIVE SCHIERE SGL
210600060829DO  1C     1             DO        10            J
210700060829IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
210800060829     C                   MOVEL     'S'           KTSDSF
210900060829     C                   MOVEL     PARCTO        KTSCTO
211000060829     C                   Z-ADD     PARKOV        KTSKSC
211100060829     C                   Z-ADD     PARCTR        KTSCTR
211200060829     C                   Z-ADD     PARPRG        KTSPRG
211300060829     C                   Z-ADD     XORG          KTSLNP
211400060829     C                   Z-ADD     §SG(J)        KTSSGL
211500060829     C     KEYETS        CHAIN     TEETS11L                           96
211600060829     C                   EXSR      IMPETS1                                      *IMPOSTA I CAMPI
211700060829IF  4C     *IN96         IFEQ      *ON
211800060829     C                   MOVEL     KTSDSF        £ETSDSF
211900060829     C                   MOVEL     KTSCTO        £ETSCTO
212000060829     C                   Z-ADD     KTSKSC        £ETSKSC
212100060829     C                   Z-ADD     KTSCTR        £ETSCTR
212200060829     C                   Z-ADD     KTSPRG        £ETSPRG
212300060829     C                   Z-ADD     KTSLNP        £ETSLNP
212400060829     C                   Z-ADD     KTSSGL        £ETSSGL
212500060829     C                   Z-ADD     DATCOR        £ETSDIR
212600060829     C                   Z-ADD     *ZEROS        £ETSDUV
212700060829     C                   MOVEL     *BLANKS       £ETSATB
212800060829     C                   WRITE     TEETS100                                     *INSERIMENTO
212900060829     C                   UNLOCK    TEETS11L
213000060829X   4C                   ELSE
213100060829     C                   Z-ADD     DATCOR        £ETSDUV
213200060829     C                   MOVEL     *BLANKS       £ETSATB
213300060829     C                   UPDATE    TEETS100                                     *VARIAZIONE
213400060829E   4C                   ENDIF
213500060829E   2C                   ENDIF
213600060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
213700060829     C*
213800060829     C                   ENDSR
213900980821     C*------------------------------------------------------------------------*
214000011001     C* SCRETT - SCRIVE UN RECORD DI TEETT -CODICI TASSAZIONE-
214100980821     C*------------------------------------------------------------------------*
214200980821     C     SCRETT        BEGSR
214300980821     C*
214400980821     C                   EXSR      SCRCTS                                       *SRIVE SCHIERE CTS
214500050103DO  1C     1             DO        600           J                 4 0
214600980821IF  2C     §CT(J)        IFNE      *BLANKS                                      *CODICE ESISTENTE
214700060804     C                   MOVEL     'S'           KTTDSF
214800980821     C                   MOVEL     PARCTO        KTTCTO
214900980821     C                   Z-ADD     PARKOV        KTTKSC
215000980821     C                   Z-ADD     PARCTR        KTTCTR
215100980821     C                   Z-ADD     PARPRG        KTTPRG
215200980821     C                   Z-ADD     XORG          KTTLNP
215300980821     C                   MOVEL     XFIE          KTTFIE
215400980821     C                   MOVEL     §CT(J)        KTTCTS
215500021122     C     KEYETT        CHAIN     TEETT01L                           96
215600980821     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
215700021122IF  4C     *IN96         IFEQ      *ON
215800980821     C                   MOVEL     KTTDSF        ETTDSF
215900980821     C                   MOVEL     KTTCTO        ETTCTO
216000980821     C                   Z-ADD     KTTKSC        ETTKSC
216100980821     C                   Z-ADD     KTTCTR        ETTCTR
216200980821     C                   Z-ADD     KTTPRG        ETTPRG
216300980821     C                   Z-ADD     KTTLNP        ETTLNP
216400980821     C                   MOVEL     KTTFIE        ETTFIE
216500980821     C                   MOVEL     KTTCTS        ETTCTS
216600980821     C                   Z-ADD     DATCOR        ETTDIR
216700980821     C                   Z-ADD     *ZEROS        ETTDUV
216800021122     C                   MOVEL     *BLANKS       ETTATB
216900011001     C                   WRITE     TEETT000                                     *INSERIMENTO
217000011001     C                   UNLOCK    TEETT01L
217100980821X   4C                   ELSE
217200980821     C                   Z-ADD     DATCOR        ETTDUV
217300021122     C                   MOVEL     *BLANKS       ETTATB
217400011001     C                   UPDATE    TEETT000                                     *VARIAZIONE
217500980821E   4C                   ENDIF
217600980821E   2C                   ENDIF
217700980821E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
217800980821     C*
217900980821     C                   ENDSR
218000970110     C*------------------------------------------------------------------------*
218100970110     C* IMPETC - IMPOSTA I CAMPI DATI TIPO -GENERALI-
218200970110     C*------------------------------------------------------------------------*
218300970110     C     IMPETC        BEGSR
218400970110     C*---
218500970110     C* CAMPI CALCOLATI
218600970110     C*---
218700970110     C                   Z-ADD     *ZEROS        WMAX              9 0
218800980901     C                   MOVEL     'C'           ETCCLV
218900020215IF  1C     CLD(I)        IFGT      WMAX
219000020215     C                   Z-ADD     CLD(I)        WMAX
219100020215     C                   MOVEL     'D'           ETCCLV
219200020215E   1C                   ENDIF
219300980901IF  1C     CLT(I)        IFGT      WMAX
219400980901     C                   Z-ADD     CLT(I)        WMAX
219500020215     C                   MOVEL     'T'           ETCCLV
219600980901E   1C                   ENDIF
219700970110IF  1C     CLA(I)        IFGT      WMAX
219800970110     C                   Z-ADD     CLA(I)        WMAX
219900970110     C                   MOVEL     'A'           ETCCLV
220000970110E   1C                   ENDIF
220100970110IF  1C     CLB(I)        IFGT      WMAX
220200970110     C                   Z-ADD     CLB(I)        WMAX
220300970110     C                   MOVEL     'B'           ETCCLV
220400970110E   1C                   ENDIF
220500970110IF  1C     CLC(I)        IFGT      WMAX
220600970110     C                   Z-ADD     CLC(I)        WMAX
220700970110     C                   MOVEL     'C'           ETCCLV
220800970110E   1C                   ENDIF
220900050808     c* packing list
221000050808     C                   Z-ADD     *ZEROS        Wmax
221100050808     C                   MOVEL     'N'           ETCpkl
221200050808IF  1C     pks(I)        IFGT      wmax
221300050808     C                   Z-ADD     pks(I)        Wmax
221400050808     C                   MOVEL     'S'           ETCpkl
221500050808E   1C                   ENDIF
221600050808IF  1C     pkn(I)        IFGT      wmax
221700050808     C                   Z-ADD     pkn(I)        Wmax
221800050808     C                   MOVEL     'N'           ETCpkl
221900050808E   1C                   ENDIF
222000970110     C*
222100970110     C                   Z-ADD     *ZEROS        WMAX              9 0
222200970110     C                   MOVEL     ' '           ETCBOL
222300970110IF  1C     BON(I)        IFGT      WMAX
222400970110     C                   Z-ADD     BON(I)        WMAX
222500970110     C                   MOVEL     ' '           ETCBOL
222600970110E   1C                   ENDIF
222700970415IF  1C     BOA(I)        IFGT      WMAX                                         *"A" VINCE SU " "
222800970415     C     BOA(I)        ORGT      *ZEROS
222900970110     C                   Z-ADD     BOA(I)        WMAX
223000970110     C                   MOVEL     'A'           ETCBOL
223100970110E   1C                   ENDIF
223200970415IF  1C     BOB(I)        IFGT      WMAX                                         *"B" VINCE SU "A"
223300970415     C     BOB(I)        ORGT      *ZEROS
223400970110     C                   Z-ADD     BOB(I)        WMAX
223500970110     C                   MOVEL     'B'           ETCBOL
223600970110E   1C                   ENDIF
223700060829     C* ricavi
223800970110IF  1C     NCL(I)        IFGT      *ZEROS
223900970110     C     PKB(I)        DIV       NCL(I)        ETCPMC
224000970110     C     VLB(I)        DIV       NCL(I)        ETCVMC
224100970703E   1C                   ENDIF
224200970703IF  1C     ETCPMC        IFEQ      *ZEROS
224300970110     C                   Z-ADD     0,1           ETCPMC
224400970703E   1C                   ENDIF
224500970703IF  1C     ETCVMC        IFEQ      *ZEROS
224600970110     C                   Z-ADD     0,001         ETCVMC
224700970703E   1C                   ENDIF
224800060829     c* competenze
224900060829IF  1C     NCL1(I)       IFGT      *ZEROS
225000060829     C     PKB1(I)       DIV       NCL1(I)       ETCPMC1
225100060829     C     VLB1(I)       DIV       NCL1(I)       ETCVMC1
225200060829E   1C                   ENDIF
225300060829IF  1C     ETCPMC1       IFEQ      *ZEROS
225400060829     C                   Z-ADD     0,1           ETCPMC1
225500060829E   1C                   ENDIF
225600060829IF  1C     ETCVMC1       IFEQ      *ZEROS
225700060829     C                   Z-ADD     0,001         ETCVMC1
225800060829E   1C                   ENDIF
225900970110     C*
226000011026     C     FRA(I)        MULT      100           WN153            21 8
226100011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
226200970110     C                   Z-ADD     WPER          ETCFRA
226300011001     C     ASS(I)        MULT      100           WN153
226400011001     C     WN153         DIV(H)    SPE(I)        WPER
226500970110     C                   Z-ADD     WPER          ETCASS
226600980803IF  1C     ETCFRA        IFGE      ETCASS
226700980803     C                   Z-ADD     *ZEROS        ETCASS
226800980831     C                   Z-ADD     100           ETCFRA
226900980803X   1C                   ELSE
227000980803     C                   Z-ADD     *ZEROS        ETCFRA
227100980831     C                   Z-ADD     100           ETCASS
227200980803E   1C                   ENDIF
227300970110     C*
227400011001     C     PCV(I)        MULT      100           WN153
227500011001     C     WN153         DIV(H)    SPE(I)        WPER
227600970110     C                   Z-ADD     WPER          ETCPCV
227700040802     C*
227800011001     C     PCM(I)        MULT      100           WN153
227900011001     C     WN153         DIV(H)    SPE(I)        WPER
228000970110     C                   Z-ADD     WPER          ETCPCM
228100970110     C*
228200011001     C     PFD(I)        MULT      100           WN153
228300011001     C     WN153         DIV(H)    SPE(I)        WPER
228400970110     C                   Z-ADD     WPER          ETCPFD
228500970110     C*
228600011001     C     ESP(I)        MULT      100           WN153
228700011001     C     WN153         DIV(H)    SPE(I)        WPER
228800970110     C                   Z-ADD     WPER          ETCESP
228900970110     C*
229000011001     C     PPV(I)        MULT      100           WN153
229100011001     C     WN153         DIV(H)    SPE(I)        WPER
229200970110     C                   Z-ADD     WPER          ETCPPV
229300970110     C*
229400011001     C     PCA(I)        MULT      100           WN153
229500011001     C     WN153         DIV(H)    SPE(I)        WPER
229600970110     C                   Z-ADD     WPER          ETCPCA
229700970110     C*
229800011001     C     PCP(I)        MULT      100           WN153
229900011001     C     WN153         DIV(H)    SPE(I)        WPER
230000970110     C                   Z-ADD     WPER          ETCPCP
230100970110     C*
230200011001     C     PCS(I)        MULT      100           WN153
230300011001     C     WN153         DIV(H)    SPE(I)        WPER
230400970110     C                   Z-ADD     WPER          ETCPCS
230500970110     C*
230600011001     C     PCF(I)        MULT      100           WN153
230700011001     C     WN153         DIV(H)    SPE(I)        WPER
230800970110     C                   Z-ADD     WPER          ETCPCF
230900970415     C*
231000011001     C     PCD(I)        MULT      100           WN153
231100011001     C     WN153         DIV(H)    SPE(I)        WPER
231200970415     C                   Z-ADD     WPER          ETCPCD
231300970415     C*
231400011001     C     PCC(I)        MULT      100           WN153
231500011001     C     WN153         DIV(H)    SPE(I)        WPER
231600970415     C                   Z-ADD     WPER          ETCPCC
231700970415     C*
231800011001     C     PCL(I)        MULT      100           WN153
231900011001     C     WN153         DIV(H)    SPE(I)        WPER
232000970415     C                   Z-ADD     WPER          ETCPCL
232100970110     C*
232200011001     C     PAB(I)        MULT      100           WN153
232300011001     C     WN153         DIV(H)    SPE(I)        WPER
232400970110     C                   Z-ADD     WPER          ETCPAB
232500970110     C*
232600011001     C     PAM(I)        MULT      100           WN153
232700011001     C     WN153         DIV(H)    SPE(I)        WPER
232800970110     C                   Z-ADD     WPER          ETCPAM
232900011011     C*
233000011011     C     CIS(I)        MULT      100           WN153
233100011011     C     WN153         DIV(H)    SPE(I)        WPER
233200011011     C                   Z-ADD     WPER          ETCCIS
233300011011     C*
233400011011     C     RIS(I)        MULT      100           WN153
233500011011     C     WN153         DIV(H)    SPE(I)        WPER
233600011011     C                   Z-ADD     WPER          ETCSAM
233700970110     C*
233800011001     C     PAB(I)        ADD       PAM(I)        WN153
233900970110     C                   Z-ADD     *ZEROS        ETCCAS
234000011001IF  1C     WN153         IFGT      *ZEROS
234100011001     C     CAS(I)        DIV       WN153         ETCCAS
234200970110E   1C                   ENDIF
234300011026     C                   Z-ADD     ETCCAS        $IMP             21 8
234400970417     C                   MOVEL     'E'           $FDE              1
234500011011     C                   IF        PARDIV = 'ITL'
234600011026     C                   Z-ADD     1000          $ARR             21 8
234700011011     C                   ELSE
234800011026     C                   Z-ADD     1             $ARR             21 8
234900011011     C                   ENDIF
235000970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
235100970110     C                   Z-ADD     $IMP          ETCCAS
235200970110     C*
235300970418     C     PAR(I)        ADD       PAC(I)        WN90              9 0
235400060315     C                   ADD       xx1(I)        WN90
235500011001     C     PAR(I)        MULT      100           WN153
235600011001     C     WN153         DIV(H)    WN90          WPER
235700970110     C                   Z-ADD     WPER          ETCPAR
235800970110     C*
235900011001     C     PAC(I)        MULT      100           WN153
236000011001     C     WN153         DIV(H)    WN90          WPER
236100970110     C                   Z-ADD     WPER          ETCPAC
236200060315     C*
236300060315     C     xx1(I)        MULT      100           WN153
236400060315     C     WN153         DIV(H)    WN90          WPER
236500060315     C                   Z-ADD     WPER          ETCxx1
236600970110     C*
236700970110     C                   Z-ADD     *ZEROS        ETCASC
236800970110IF  1C     PAC(I)        IFGT      *ZEROS
236900970110     C     ASC(I)        DIV       PAC(I)        ETCASC
237000970110E   1C                   ENDIF
237100970110     C                   Z-ADD     ETCASC        $IMP
237200970110     C                   MOVEL     'E'           $FDE
237300011011     C                   IF        PARDIV = 'ITL'
237400011026     C                   Z-ADD     1000          $ARR             21 8
237500011011     C                   ELSE
237600011026     C                   Z-ADD     1             $ARR             21 8
237700011011     C                   ENDIF
237800970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
237900970110     C                   Z-ADD     $IMP          ETCASC
238000970110     C*
238100000414     C                   Z-ADD     *ZEROS        ETCGGG
238200000414     C                   Z-ADD     *ZEROS        ETCNRG
238300011001     C     PGI(I)        MULT      100           WN153
238400011001     C     WN153         DIV(H)    SPE(I)        WPER
238500970110     C                   Z-ADD     WPER          ETCPGI
238600021122     C* SE LA % GIACENZE SUL FILE VIENE A 0 => AZZERO ANCHE I GG GIACENZA ED IL NUMERO APERTURE GIAC
238700021122     C                   IF        ETCPGI = *ZEROS
238800021122     C                   Z-ADD     *ZEROS        ETCGGG
238900021122     C                   Z-ADD     *ZEROS        ETCNRG
239000021122     C                   ELSE
239100000414IF  1C     PGI(I)        IFGT      *ZEROS
239200000414     C     GGS(I)        DIV       PGI(I)        ETCGGG                         *GG SOSTA
239300000414IF  2C     ETCGGG        IFEQ      *ZEROS
239400000414     C                   Z-ADD     1             ETCGGG
239500000414E   2C                   ENDIF
239600000414     C     NRG(I)        DIV       PGI(I)        ETCNRG                         *N° RIAPERTURE
239700000414IF  2C     ETCNRG        IFEQ      *ZEROS
239800000414     C                   Z-ADD     1             ETCNRG
239900000414E   2C                   ENDIF
240000000414E   1C                   ENDIF
240100021122     C                   ENDIF
240200970110     C*
240300970110     C                   Z-ADD     *ZEROS        WFRQ              3 0
240400011001     C                   Z-ADD     *ZEROS        WN153
240500970110DO  1C     1             DO        30            J
240600011001IF  2C     SGP(J)        IFGT      WN153
240700011001     C                   Z-ADD     SGP(J)        WN153
240800970110     C                   Z-ADD     GGP(J)        WFRQ
240900970110E   2C                   ENDIF
241000970110E   1C                   ENDDO
241100970110     C                   Z-ADD     WFRQ          ETCGGP
241200970110IF  1C     ETCGGP        IFEQ      999
241300970110     C                   Z-ADD     *ZEROS        ETCGGP
241400970110E   1C                   ENDIF
241500970110     C*
241600970110     C                   Z-ADD     *ZEROS        WSPE             11 0
241700000411     C                   Z-ADD     *ZEROS        WSTP             13 0
241800970110DO  1C     1             DO        999           J
241900970110IF  2C     RIT(J)        IFGT      *ZEROS
242000000411     C     RIT(J)        DIV       J             WN13             13 0
242100000411     C                   MVR                     RESTO            15 0
242200000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
242300000411     C     J             ANDNE     999                                          *STOP <> 999
242400000411     C                   ADD       1             WN13
242500000411E   3C                   ENDIF
242600970110     C     WSPE          ADD       RIT(J)        WSPE
242700970110     C     WSTP          ADD       WN13          WSTP
242800970110E   2C                   ENDIF
242900970110E   1C                   ENDDO
243000970110     C                   Z-ADD     *ZEROS        ETCSPR
243100970110IF  1C     WSTP          IFGT      *ZEROS
243200970110     C     WSPE          DIV       WSTP          ETCSPR
243300000411     C                   MVR                     RESTO            15 0
243400000411IF  2C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
243500000411     C                   ADD       1             ETCSPR                         *ARROTONDA X ECCESSO
243600000411E   2C                   ENDIF
243700000411E   1C                   ENDIF
243800970110IF  1C     ETCSPR        IFEQ      *ZEROS
243900970110     C                   Z-ADD     1             ETCSPR
244000970110E   1C                   ENDIF
244100970110     C*
244200970110     C                   Z-ADD     *ZEROS        WSPE             11 0
244300000411     C                   Z-ADD     *ZEROS        WSTP             13 0
244400970110DO  1C     1             DO        999           J
244500970110IF  2C     CON(J)        IFGT      *ZEROS
244600000411     C     CON(J)        DIV       J             WN13             13 0
244700000411     C                   MVR                     RESTO            15 0
244800000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
244900000411     C     J             ANDNE     999                                          *STOP <> 999
245000000411     C                   ADD       1             WN13
245100000411E   3C                   ENDIF
245200970110     C     WSPE          ADD       CON(J)        WSPE
245300970110     C     WSTP          ADD       WN13          WSTP
245400970110E   2C                   ENDIF
245500970110E   1C                   ENDDO
245600001221     C                   Z-ADD     1             ETCSPC
245700970110     C*---
245800970110     C* CAMPI FISSI
245900970110     C*---
246000970110     C                   MOVEL     *BLANKS       ETCSDE
246100970110     C                   Z-ADD     *ZEROS        ETCPDE
246200970619     C                   MOVEL     'E'           ETCFAR
246300011011     C                   IF        PARDIV = 'ITL'
246400970314     C                   Z-ADD     100           ETCIAR
246500011011     C                   ELSE
246600011030     C                   Z-ADD     0,1           ETCIAR
246700011011     C                   ENDIF
246800970213     C                   MOVEL     'N'           ETCSIN
246900970110     C                   MOVEL     'N'           ETCIAP
247000970110     C                   MOVEL     'N'           ETCIPI
247100970110     C                   MOVEL     'N'           ETCISU
247200970110     C                   MOVEL     'N'           ETCIFU
247300970415     C                   MOVEL     'N'           ETCIDD
247400970415     C                   MOVEL     'N'           ETCICE
247500970415     C                   MOVEL     'N'           ETCILO
247600050810     C                   MOVEL     'N'           ETCIde
247700050810     C                   MOVEL     'N'           ETCIba
247800970110     C                   MOVEL     'N'           ETCIES
247900970110     C                   MOVEL     'N'           ETCICA
248000970110     C                   MOVEL     'N'           ETCIAS
248100970110     C                   MOVEL     'N'           ETCIGI
248200020716     C                   MOVEL     'N'           ETCISO
248300020716     C                   MOVEL     'N'           ETCINR
248400980901     C                   MOVEL     METX(3)       ETCDCV
248500970110     C*
248600970110     C                   MOVEL     *BLANKS       ETCFTR
248700970110     C                   Z-ADD     *ZEROS        ETCDTR
248800970110     C                   MOVEL     *BLANKS       ETCATB
248900021104     C*
249000021104     C* EFFETTUO ALCUNE CONSIDERAZIONI
249100021104     C                   IF        ETCBOL = 'B'
249200021104     C                   EVAL      ETCIX1 = 'K'
249300021104     C                   ELSE
249400021104     C                   EVAL      ETCIX1 = *blanks
249500021104     C                   ENDIF
249600040811     C* %sped oltre 20 colli non imbancalate
249700040811     C     sbnb(I)       MULT      100           WN153
249800040802     C     WN153         DIV(H)    SPE(I)        WPER
249900040802     C                   Z-ADD     WPER          ETCbnb
250000040811     c* numero medio bancali x spedizioni oltre 20 colli
250100061006     c                   if        sbnb(i) <> 0 and etcbnb <> 0
250200040811     c     nbn(i)        div(h)    sbnb(i)       etcnbn
250300061006     c                   if        etcnbn = 0
250400061006     c                   eval      etcnbn = 1
250500061006     c                   end
250600040812     c                   else
250700040812     c                   clear                   etcnbn
250800040811     c                   end
250900040824     C* %sped oltre 20 colli non imbancalate
251000040824     C     sban(I)       MULT      100           WN153
251100040824     C     WN153         DIV(H)    SPE(I)        WPER
251200040824     C                   Z-ADD     WPER          ETCban
251300040824     c* numero medio bancali Bartolini
251400061006     c                   if        sban(i) <> 0 and etcban <> 0
251500040824     c     nba(i)        div(h)    sban(i)       etcnba
251600061006     c                   if        etcnba = 0
251700061006     c                   eval      etcnba = 1
251800061006     c                   end
251900040824     c                   else
252000040824     c                   clear                   etcnba
252100040824     c                   end
252200040802     C*
252300040802     C     ded(I)        MULT      100           WN153
252400040802     C     WN153         DIV(H)    SPE(I)        WPER
252500040802     C                   Z-ADD     WPER          ETCdes
252600040802     C*
252700040802     C     val(I)        MULT      100           WN153
252800040802     C     WN153         DIV(H)    SPE(I)        WPER
252900040802     C                   Z-ADD     WPER          ETCval
253000970110     C*
253100970110     C                   ENDSR
253200970416     C*------------------------------------------------------------------------*
253300060829     C* SCRSGL - SCRIVE SCHIERE SCAGLIONI ricavi
253400970416     C*------------------------------------------------------------------------*
253500970416     C     SCRSGL        BEGSR
253600970416     C*
253700970416     C* AZZERA LE VARIABILI DI LAVORO
253800970416     C                   CLEAR                   §SG
253900970416     C                   CLEAR                   §PS
254000970416     C                   CLEAR                   §CS
254100970416     C                   CLEAR                   §VS
254200970416     C                   CLEAR                   §SP
254300971008     C                   CLEAR                   §SD
254400970416     C*
254500970416     C* PER CIASCUN SCAGLIONE DA SCRIVERE
254600970416DO  1C     1             DO        10            J
254700970416IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
254800970416     C                   Z-ADD     SSGL(J)       §SG(J)
254900970416IF  3C     SSP(J)        IFGT      *ZEROS
255000970416     C*
255100970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
255200971008     C                   MOVEL     *BLANKS       §SD(J)                         *VALORE NON DEDOTTO
255300970416     C     PSC(J)        DIV       SSP(J)        §PS(J)
255400970416     C     CSC(J)        DIV       SSP(J)        §CS(J)
255500970416     C     VSC(J)        DIV       SSP(J)        §VS(J)
255600011001     C     SSP(J)        MULT      100           WN153
255700011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
255800970416     C                   Z-ADD     WPER          §SP(J)
255900970703IF  4C     §SP(J)        IFEQ      *ZEROS
256000970703     C                   Z-ADD     0,1           §SP(J)
256100970703E   4C                   ENDIF
256200970416X   3C                   ELSE
256300970416     C*
256400970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
256500971008     C                   MOVEL     '*'           §SD(J)                         *VALORE DEDOTTO
256600970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
256700970416     C     PARTTR        OREQ      '3'                                          *KG
256800970416     C                   Z-ADD     §SG(J)        §PS(J)
256900970416     C     §PS(J)        DIV       ETCPMC        §CS(J)
257000970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
257100970416X   4C                   ELSE                                                   *COLLI
257200970416     C                   Z-ADD     §SG(J)        §CS(J)
257300040812     C     §CS(J)        mult      ETCPMC        §PS(J)
257400970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
257500970416E   4C                   ENDIF
257600970416     C                   Z-ADD     0,1           §SP(J)
257700970416E   3C                   ENDIF
257800970416E   2C                   ENDIF
257900970416E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
258000000707     C*
258100000707     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
258200000707     C                   EXSR      PARPSG
258300970416     C*
258400970416     C                   ENDSR
258500060829     C*------------------------------------------------------------------------*
258600060829     C* SCRSGL1- SCRIVE SCHIERE SCAGLIONI competenze
258700060829     C*------------------------------------------------------------------------*
258800060829     C     SCRSGL1       BEGSR
258900060829     C*
259000060829     C* AZZERA LE VARIABILI DI LAVORO
259100060829     C                   CLEAR                   §SG
259200060829     C                   CLEAR                   §PS1
259300060829     C                   CLEAR                   §CS1
259400060829     C                   CLEAR                   §VS1
259500060829     C                   CLEAR                   §SP1
259600060829     C                   CLEAR                   §SD1
259700060829     C*
259800060829     C* PER CIASCUN SCAGLIONE DA SCRIVERE
259900060829DO  1C     1             DO        10            J
260000060829IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
260100060829     C                   Z-ADD     SSGL(J)       §SG(J)
260200060829IF  3C     SSP1(J)       IFGT      *ZEROS
260300060829     C*
260400060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
260500060829     C                   MOVEL     *BLANKS       §SD1(J)                        *VALORE NON DEDOTTO
260600060829     C     PSC1(J)       DIV       SSP1(J)       §PS1(J)
260700060829     C     CSC1(J)       DIV       SSP1(J)       §CS1(J)
260800060829     C     VSC1(J)       DIV       SSP1(J)       §VS1(J)
260900060829     C     SSP1(J)       MULT      100           WN153
261000060829     C     WN153         DIV(H)    SPE(I)        WPER              4 1
261100060829     C                   Z-ADD     WPER          §SP1(J)
261200060829IF  4C     §SP1(J)       IFEQ      *ZEROS
261300060829     C                   Z-ADD     0,1           §SP1(J)
261400060829E   4C                   ENDIF
261500060829X   3C                   ELSE
261600060829     C*
261700060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
261800060829     C                   MOVEL     '*'           §SD1(J)                        *VALORE DEDOTTO
261900060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
262000060829     C     PARTTR        OREQ      '3'                                          *KG
262100060829     C                   Z-ADD     §SG(J)        §PS1(J)
262200060829     C     §PS1(J)       DIV       ETCPMC1       §CS1(J)
262300060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
262400060829X   4C                   ELSE                                                   *COLLI
262500060829     C                   Z-ADD     §SG(J)        §CS1(J)
262600060829     C     §CS1(J)       mult      ETCPMC1       §PS1(J)
262700060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
262800060829E   4C                   ENDIF
262900060829     C                   Z-ADD     0,1           §SP1(J)
263000060829E   3C                   ENDIF
263100060829E   2C                   ENDIF
263200060829E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
263300060829     C*
263400060829     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
263500060829     C                   EXSR      PARPSG1
263600060829     C*
263700060829     C                   ENDSR
263800000707     C*------------------------------------------------------------------------*
263900000707     C* PARPSG - PAREGGIA LA %TOTALE -SCAGLIONI- A 100
264000000707     C*------------------------------------------------------------------------*
264100000707     C     PARPSG        BEGSR
264200000707     C*
264300000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
264400000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
264500000707DO  1C     1             DO        10            J
264600000707IF  2C     §SG(J)        IFGT      *ZEROS
264700000707     C                   ADD       §SP(J)        TOTPPE                         *%INCIDENZA TOTALE
264800000707E   2C                   ENDIF
264900000707E   1C                   ENDDO
265000000707     C*
265100000707     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
265200000707     C                   Z-ADD     *ZEROS        WN41              4 1
265300000707     C     100           SUB       TOTPPE        WN41
265400000707     C*
265500000707     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
265600000707IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
265700000707DO  2C     WN41          DOWNE     *ZEROS
265800000707DO  3C     1             DO        10            J
265900000707IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
266000000707     C* POSITIVA -> AGGIUNGERE
266100000707IF  5C     WN41          IFGT      *ZEROS
266200000707     C     §SP(J)        ADD       0,1           §SP(J)                         *AGGIUNGE %
266300000707     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
266400000707E   5C                   ENDIF
266500000707     C* NEGATIVA -> TOGLIERE
266600000707IF  5C     WN41          IFLT      *ZEROS
266700000707IF  6C     §SP(J)        IFGT      0,1
266800000707     C     §SP(J)        SUB       0,1           §SP(J)                         *SOTTRAE %
266900000707     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
267000000707E   6C                   ENDIF
267100000707E   5C                   ENDIF
267200000707     C* UGUALE A ZERO -> USCIRE
267300000707IF  5C     WN41          IFEQ      *ZEROS
267400000707     C                   LEAVE
267500000707E   5C                   ENDIF
267600000707E   4C                   ENDIF
267700000707E   3C                   ENDDO                                                  *FINE SCAGLIONI
267800000707E   2C                   ENDDO                                                  *DIFFERENZA = 0
267900000707E   1C                   ENDIF
268000000707     C*
268100000707     C                   ENDSR
268200060829     C*------------------------------------------------------------------------*
268300060829     C* PARPSG1- PAREGGIA LA %TOTALE -SCAGLIONI- A 100
268400060829     C*------------------------------------------------------------------------*
268500060829     C     PARPSG1       BEGSR
268600060829     C*
268700060829     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
268800060829     C                   Z-ADD     *ZEROS        TOTPPE            4 1
268900060829DO  1C     1             DO        10            J
269000060829IF  2C     §SG(J)        IFGT      *ZEROS
269100060829     C                   ADD       §SP1(J)       TOTPPE                         *%INCIDENZA TOTALE
269200060829E   2C                   ENDIF
269300060829E   1C                   ENDDO
269400060829     C*
269500060829     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
269600060829     C                   Z-ADD     *ZEROS        WN41              4 1
269700060829     C     100           SUB       TOTPPE        WN41
269800060829     C*
269900060829     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
270000060829IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
270100060829DO  2C     WN41          DOWNE     *ZEROS
270200060829DO  3C     1             DO        10            J
270300060829IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
270400060829     C* POSITIVA -> AGGIUNGERE
270500060829IF  5C     WN41          IFGT      *ZEROS
270600060829     C     §SP1(J)       ADD       0,1           §SP1(J)                        *AGGIUNGE %
270700060829     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
270800060829E   5C                   ENDIF
270900060829     C* NEGATIVA -> TOGLIERE
271000060829IF  5C     WN41          IFLT      *ZEROS
271100060829IF  6C     §SP1(J)       IFGT      0,1
271200060829     C     §SP1(J)       SUB       0,1           §SP1(J)                        *SOTTRAE %
271300060829     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
271400060829E   6C                   ENDIF
271500060829E   5C                   ENDIF
271600060829     C* UGUALE A ZERO -> USCIRE
271700060829IF  5C     WN41          IFEQ      *ZEROS
271800060829     C                   LEAVE
271900060829E   5C                   ENDIF
272000060829E   4C                   ENDIF
272100060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
272200060829E   2C                   ENDDO                                                  *DIFFERENZA = 0
272300060829E   1C                   ENDIF
272400060829     C*
272500060829     C                   ENDSR
272600970110     C*------------------------------------------------------------------------*
272700060829     C* IMPETS - IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- ricavi
272800970110     C*------------------------------------------------------------------------*
272900970318     C     IMPETS        BEGSR
273000970110     C*---
273100970110     C* CAMPI CALCOLATI
273200970110     C*---
273300970416     C                   Z-ADD     §PS(J)        ETSPSC
273400970416     C                   Z-ADD     §CS(J)        ETSCSC
273500981020IF  1C     ETSCSC        IFLT      1
273600981020     C                   Z-ADD     1             ETSCSC
273700981020E   1C                   ENDIF
273800970416     C                   Z-ADD     §VS(J)        ETSVSC
273900970416     C                   Z-ADD     §SP(J)        ETSPPE
274000970415     C*---
274100970415     C* CAMPI FISSI
274200970415     C*---
274300980325     C                   Z-ADD     *ZEROS        ETSINL
274400970415     C                   Z-ADD     *ZEROS        ETSRPV
274500970415     C                   MOVEL     *BLANKS       ETSSDE
274600970415     C                   Z-ADD     *ZEROS        ETSPDE
274700970110     C*
274800040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
274900040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
275000040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
275100040805     C* MA È GIUSTO COSÌ
275200040805     C                   MOVEL     §SD(J)        ETSFTR
275300970110     C                   Z-ADD     *ZEROS        ETSDTR
275400970110     C                   MOVEL     *BLANKS       ETSATB
275500970110     C*
275600970110     C                   ENDSR
275700060829     C*------------------------------------------------------------------------*
275800060829     C* IMPETS1- IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- competenze
275900060829     C*------------------------------------------------------------------------*
276000060829     C     IMPETS1       BEGSR
276100060829     C*---
276200060829     C* CAMPI CALCOLATI
276300060829     C*---
276400060829     C                   Z-ADD     §PS1(J)       £ETSPSC
276500060829     C                   Z-ADD     §CS1(J)       £ETSCSC
276600060829IF  1C     £ETSCSC       IFLT      1
276700060829     C                   Z-ADD     1             £ETSCSC
276800060829E   1C                   ENDIF
276900060829     C                   Z-ADD     §VS1(J)       £ETSVSC
277000060829     C                   Z-ADD     §SP1(J)       £ETSPPE
277100060829     C*---
277200060829     C* CAMPI FISSI
277300060829     C*---
277400060829     C                   Z-ADD     *ZEROS        £ETSINL
277500060829     C                   Z-ADD     *ZEROS        £ETSRPV
277600060829     C                   MOVEL     *BLANKS       £ETSSDE
277700060829     C                   Z-ADD     *ZEROS        £ETSPDE
277800060829     C*
277900060829     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
278000060829     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
278100060829     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
278200060829     C* MA È GIUSTO COSÌ
278300060829     C                   MOVEL     §SD1(J)       £ETSFTR
278400060829     C                   Z-ADD     *ZEROS        £ETSDTR
278500060829     C                   MOVEL     *BLANKS       £ETSATB
278600060829     C*
278700060829     C                   ENDSR
278800970416     C*------------------------------------------------------------------------*
278900970416     C* SCRCTS - SCRIVE SCHIERE CODICI TASSAZIONE
279000970416     C*------------------------------------------------------------------------*
279100970416     C     SCRCTS        BEGSR
279200000707     C*---
279300000707     C* ITALIA
279400000707     C*---
279500000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
279600000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
279700000707     C                   CLEAR                   §RC
279800000707     C                   CLEAR                   §CP
279900000707     C                   CLEAR                   §CD
280000980821IF  1C     XFIE          IFEQ      'I'
280100970423     C                   XFOOT     TSI           TOTSPE           11 0          *TOT. SPED. ITALIA
280200050103DO  2C     1             DO        600           J
280300970423IF  3C     CCTI(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
280400000620     C     CRCI(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
280500970423     C                   MOVEL     CCTI(J)       §CT(J)
280600970423     C                   MOVEL     CRCI(J)       §RC(J)
280700970417     C*
280800970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
280900971008     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
281000970423IF  4C     TSI(J)        IFGT      *ZEROS
281100011001     C     TSI(J)        MULT      100           WN153
281200011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
281300970417     C                   Z-ADD     WPER          §CP(J)
281400970703IF  5C     §CP(J)        IFEQ      *ZEROS
281500970703     C                   Z-ADD     0,1           §CP(J)
281600970703E   5C                   ENDIF
281700970423X   4C                   ELSE
281800970417     C*
281900970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
282000971008     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
282100970417     C                   Z-ADD     0,1           §CP(J)
282200970423E   4C                   ENDIF
282300970423E   3C                   ENDIF
282400970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
282500000707     C*
282600000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
282700000707     C                   EXSR      PARPCT
282800970423     C*---
282900970423     C* ESTERO
283000970423     C*---
283100000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
283200970423X   1C                   ELSE
283300000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
283400000707     C                   CLEAR                   §RC
283500000707     C                   CLEAR                   §CP
283600000707     C                   CLEAR                   §CD
283700970423     C                   XFOOT     TSE           TOTSPE           11 0          *TOT. SPED. ITALIA
283800050103DO  2C     1             DO        600           J
283900970423IF  3C     CCTE(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
284000000620     C     CRCE(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
284100970423     C                   MOVEL     CCTE(J)       §CT(J)
284200970423     C                   MOVEL     CRCE(J)       §RC(J)
284300970423     C*
284400970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
284500990803     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
284600970423IF  4C     TSE(J)        IFGT      *ZEROS
284700011001     C     TSE(J)        MULT      100           WN153
284800011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
284900970423     C                   Z-ADD     WPER          §CP(J)
285000970703IF  5C     §CP(J)        IFEQ      *ZEROS
285100970703     C                   Z-ADD     0,1           §CP(J)
285200970703E   5C                   ENDIF
285300970423X   4C                   ELSE
285400970423     C*
285500970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
285600990803     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
285700970423     C                   Z-ADD     0,1           §CP(J)
285800970423E   4C                   ENDIF
285900970423E   3C                   ENDIF
286000970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
286100000707     C*
286200000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
286300000707     C                   EXSR      PARPCT
286400000707E   1C                   ENDIF
286500000707     C*
286600000707     C                   ENDSR
286700000707     C*------------------------------------------------------------------------*
286800000707     C* PARPCT - PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
286900000707     C*------------------------------------------------------------------------*
287000000707     C     PARPCT        BEGSR
287100000707     C*
287200000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
287300000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
287400050103DO  1C     1             DO        600           J
287500000707IF  2C     §CT(J)        IFNE      *BLANKS
287600000707     C                   ADD       §CP(J)        TOTPPE                         *%INCIDENZA TOTALE
287700000707E   2C                   ENDIF
287800000707E   1C                   ENDDO
287900970417     C*
288000970417     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
288100970417     C                   Z-ADD     *ZEROS        WN41              4 1
288200970417     C     100           SUB       TOTPPE        WN41
288300970417     C*
288400970417     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
288500970417IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
288600000707DO  2C     WN41          DOWNE     *ZEROS
288700050103DO  3C     1             DO        600           J
288800970421IF  4C     §CT(J)        IFNE      *BLANKS                                      *ELEMENTO PIENO
288900970417     C     §RC(J)        ANDNE     *BLANKS                                      *DA CALCOLARE
289000970417     C* POSITIVA -> AGGIUNGERE
289100970417IF  5C     WN41          IFGT      *ZEROS
289200970417     C     §CP(J)        ADD       0,1           §CP(J)                         *AGGIUNGE %
289300970417     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
289400970417E   5C                   ENDIF
289500970417     C* NEGATIVA -> TOGLIERE
289600970417IF  5C     WN41          IFLT      *ZEROS
289700000707IF  6C     §CP(J)        IFGT      0,1
289800970417     C     §CP(J)        SUB       0,1           §CP(J)                         *SOTTRAE %
289900970417     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
290000000707E   6C                   ENDIF
290100970417E   5C                   ENDIF
290200000707     C* UGUALE A ZERO -> USCIRE
290300000707IF  5C     WN41          IFEQ      *ZEROS
290400000707     C                   LEAVE
290500000707E   5C                   ENDIF
290600970417E   4C                   ENDIF
290700970417E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
290800970417E   2C                   ENDDO                                                  *DIFFERENZA = 0
290900970417E   1C                   ENDIF
291000970416     C*
291100970416     C                   ENDSR
291200970110     C*------------------------------------------------------------------------*
291300970110     C* IMPETT - IMPOSTA I CAMPI DATI TIPO -CODICI TASSAZIONE-
291400970110     C*------------------------------------------------------------------------*
291500970110     C     IMPETT        BEGSR
291600970110     C*---
291700970110     C* CAMPI CALCOLATI
291800970110     C*---
291900970417     C                   MOVEL     §RC(J)        ETTRCT
292000970417     C                   Z-ADD     §CP(J)        ETTPPE
292100970110     C*---
292200970110     C* CAMPI FISSI
292300970110     C*---
292400040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
292500040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
292600040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
292700040805     C* MA È GIUSTO COSÌ
292800040805     C                   MOVEL     §CD(J)        ETTFTR
292900970110     C                   Z-ADD     *ZEROS        ETTDTR
293000970110     C                   MOVEL     *BLANKS       ETTATB
293100970110     C*
293200970110     C                   ENDSR
293300970417     C*------------------------------------------------------------------------*
293400970417     C* ARRIMP - ARROTONDA IMPORTO
293500970417     C*------------------------------------------------------------------------*
293600970417     C     ARRIMP        BEGSR
293700970417     C*---
293800970417     C* $IMP -> IMPORTO DA ARROTONDARE
293900970417     C* $FDE -> 'D'=DIFETTO / E=ECCESSO
294000970417     C* $ARR  : IMPORTO AL QUALE ARROTONDARE
294100970417     C*---
294200970417IF  1C     $IMP          IFGT      *ZEROS                                       *IMPORTO PIENO
294300970417IF  2C     $ARR          IFGT      *ZEROS                                       *SI ARROTONDAMENTO
294400970417     C     $IMP          DIV       $ARR          COMODO           15 0
294500970417     C                   MVR                     RESTO            15 0
294600970417IF  3C     $FDE          IFEQ      'E'
294700970417IF  4C     RESTO         IFGT      *ZEROS
294800970417     C                   ADD       1             COMODO
294900970417E   4C                   ENDIF
295000970417E   3C                   ENDIF
295100970417     C     COMODO        MULT      $ARR          $IMP
295200970417E   2C                   ENDIF
295300970417E   1C                   ENDIF
295400970417     C*
295500970417     C                   ENDSR
295600970110     C*------------------------------------------------------------------------*
295700970110     C* STAERR - STAMPA RIGA DI ERRORE
295800970110     C*------------------------------------------------------------------------*
295900970110     C     STAERR        BEGSR
296000970110     C*
296100970110IF  1C     *INOF         IFEQ      *ON
296200970110     C                   EXCEPT    TES
296300970110     C                   SETOFF                                       OF
296400970110E   1C                   ENDIF
296500970110IF  1C     DEPTER        IFEQ      'T'
296600970110     C                   EXCEPT    ETAS
296700970110E   1C                   ENDIF
296800970110IF  1C     DEPTER        IFEQ      'X'
296900970110     C                   EXCEPT    EETX
297000970110E   1C                   ENDIF
297100970110     C*
297200970110     C                   ENDSR
297300970313     C*------------------------------------------------------------------------*
297400970314     C* ANNETX - ANNULLA I DATI TIPO ESISTENTI
297500970313     C*------------------------------------------------------------------------*
297600970314     C     ANNETX        BEGSR
297700970313     C*
297800970314     C* PER OGNI FILIALE RICHIESTA
297900971124DO  1C     1             DO        500           I
298000970313IF  2C     SORG(I)       IFGT      *ZEROS
298100970313     C*---
298200970313     C* DATI TIPO -GENERALI-
298300970313     C*---
298400060804     C                   MOVEL     'S'           KTCDSF
298500980713     C                   MOVEL     PARCTO        KTCCTO
298600970313     C                   Z-ADD     PARKOV        KTCKSC
298700970313     C                   Z-ADD     PARCTR        KTCCTR
298800970313     C                   Z-ADD     PARPRG        KTCPRG
298900970313     C                   Z-ADD     SORG(I)       KTCLNP
299000011001     C     KEYETC        CHAIN     TEETC01L                           99
299100981221IF  3C     *IN99         IFEQ      *OFF
299200971024IF  4C     ETCATB        IFEQ      *BLANKS
299300970314     C                   MOVEL     'A'           ETCATB
299400970314     C                   MOVEL     *BLANKS       ETCFTR
299500011001     C                   UPDATE    TEETC000
299600970314E   4C                   ENDIF
299700970314E   3C                   ENDIF
299800970313     C*---
299900970313     C* DATI TIPO -SCAGLIONI-
300000970313     C*---
300100060829     c* ricavi
300200980713     C                   MOVEL     'S'           KTSDSF
300300980713     C                   MOVEL     PARCTO        KTSCTO
300400970313     C                   Z-ADD     PARKOV        KTSKSC
300500970313     C                   Z-ADD     PARCTR        KTSCTR
300600970313     C                   Z-ADD     PARPRG        KTSPRG
300700970313     C                   Z-ADD     SORG(I)       KTSLNP
300800011001     C     KE2ETS        SETLL     TEETS01L
300900011001     C     KE2ETS        READE     TEETS01L                               99
301000970314DO  3C     *IN99         DOWEQ     *OFF
301100971024IF  4C     ETSATB        IFEQ      *BLANKS
301200970314     C                   MOVEL     'A'           ETSATB
301300970314     C                   MOVEL     *BLANKS       ETSFTR
301400011001     C                   UPDATE    TEETS000
301500970314E   4C                   ENDIF
301600011001     C     KE2ETS        READE     TEETS01L                               99
301700060829E   3c                   ENDDO                                                  *FINE SCAGLIONI
301800060829     c* competetnze
301900060829     C     KE2ETS        SETLL     TEETS11L
302000060829     C     KE2ETS        READE     TEETS11L                               99
302100060829DO  3C     *IN99         DOWEQ     *OFF
302200060829IF  4C     £ETSATB       IFEQ      *BLANKS
302300060829     C                   MOVEL     'A'           £ETSATB
302400060829     C                   MOVEL     *BLANKS       £ETSFTR
302500060829     C                   UPDATE    TEETS100
302600060829E   4C                   ENDIF
302700060829     C     KE2ETS        READE     TEETS11L                               99
302800060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
302900970314     C*---
303000970314     C* DATI TIPO -CODICI TASSAZIONE-
303100970314     C*---
303200970429DO  3C     1             DO        2             K
303300970429IF  4C     SFIE(K)       IFNE      *BLANKS
303400980713     C                   MOVEL     'S'           KTTDSF
303500980713     C                   MOVEL     PARCTO        KTTCTO
303600970314     C                   Z-ADD     PARKOV        KTTKSC
303700970314     C                   Z-ADD     PARCTR        KTTCTR
303800970314     C                   Z-ADD     PARPRG        KTTPRG
303900970314     C                   Z-ADD     SORG(I)       KTTLNP
304000970429     C                   MOVEL     SFIE(K)       KTTFIE
304100011001     C     KE2ETT        SETLL     TEETT01L
304200011001     C     KE2ETT        READE     TEETT01L                               99
304300970429DO  5C     *IN99         DOWEQ     *OFF
304400971024IF  6C     ETTATB        IFEQ      *BLANKS
304500970314     C                   MOVEL     'A'           ETTATB
304600970314     C                   MOVEL     *BLANKS       ETTFTR
304700011001     C                   UPDATE    TEETT000
304800970429E   6C                   ENDIF
304900011001     C     KE2ETT        READE     TEETT01L                               99
305000970429E   5C                   ENDDO                                                  *FINE COD.TASSAZIONE
305100970429E   4C                   ENDIF
305200970429E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
305300970313     C*
305400970314E   2C                   ENDIF
305500970314E   1C                   ENDDO                                                  *FINE FILIALI
305600970314     C*
305700970313     C                   ENDSR
305800970110     C*------------------------------------------------------------------------*
305900970110     C* OPEFIL - APRE GLI ARCHIVI
306000970110     C*------------------------------------------------------------------------*
306100970110     C     OPEFIL        BEGSR
306200970110     C*
306300970110IF  1C     PARKSC        IFGT      *ZEROS
306400990812     C                   OPEN      TITAS31C                                     *LETTURA X CLIENTE
306500970110X   1C                   ELSE
306600990812     C                   OPEN      TITAS38C                                     *LETTURA X DATA
306700970110E   1C                   ENDIF
306800970110     C*
306900970110     C                   ENDSR
307000970110     C*------------------------------------------------------------------------*
307100970110     C* CLOFIL - CHIUDE GLI ARCHIVI
307200970110     C*------------------------------------------------------------------------*
307300970110     C     CLOFIL        BEGSR
307400970110     C*
307500970110IF  1C     PARKSC        IFGT      *ZEROS
307600990812     C                   CLOSE     TITAS31C                                     *LETTURA X CLIENTE
307700970110X   1C                   ELSE
307800990812     C                   CLOSE     TITAS38C                                     *LETTURA X DATA
307900970110E   1C                   ENDIF
308000970110     C*
308100970110     C                   ENDSR
308200011011     C*------------------------------------------------------------------------*
308300011011     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
308400011011     C*------------------------------------------------------------------------*
308500011011     C     CNVEUR        BEGSR
308600011011     C*
308700011011     C* AZZERA VARIABILI DI LAVORO
308800011011     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
308900011011     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
309000011026     C                   EVAL      NDEC = '9'
309100011011     C*
309200011011IF  1C     WIMI          IFNE      *ZEROS
309300011011     C                   CLEAR                   YEUR
309400011011     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
309500011011     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
309600011011     C                   MOVEL     PARDIV        YECDVO                         *DIVISA IMP CONVERTI
309700011011     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
309800011011     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
309900011011     C                   CALL      'YEURCO'
310000011011     C                   PARM                    YEUR
310100011011IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
310200011011     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
310300011011X   2C                   ELSE                                                   *SI ERRORI
310400011011     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
310500011011E   2C                   ENDIF
310600011011E   1C                   ENDIF
310700011011     C*
310800011011     C                   ENDSR
310900011011     C*------------------------------------------------------------------------*
311000011011     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
311100011011     C*------------------------------------------------------------------------*
311200011011     C     CARGEDCE      BEGSR
311300011011     C*
311400011011     C                   CLEAR                   DSGEDCE
311500011026     C                   MOVEL     *blanks       WDIVCE            3
311600011011     C*
311700011011     C                   MOVEL(P)  'GED'         KTECOD
311800011011     C                   MOVEL(P)  'CE'          KTEKE1
311900011011     C     KEYTBE        CHAIN     TNTBE01L
312000011011     C                   IF        %found(TNTBE01L)
312100011011     C                   MOVEL     TBEUNI        DSGEDCE
312200011011     C                   ENDIF
312300011011     C*
312400011011     C                   IF        §GEDCEDAT > DATCOR
312500011026     C                   EVAL      WDIVCE = §GEDCEDIP
312600011026     C                   ELSE
312700011026     C                   EVAL      WDIVCE = §GEDCEDIV
312800011011     C                   ENDIF
312900011011     C*
313000011011     C                   ENDSR
313100970110     C*------------------------------------------------------------------------*
313200970110     C* CARTAB - CARICAMENTO TABELLE
313300970110     C*------------------------------------------------------------------------*
313400970110     C     CARTAB        BEGSR
313500970110     C*--------------------
313600970110     C* ORGANIGRAMMA
313700970110     C*--------------------
313800020227     C                   Z-ADD     *ZEROS        JJ                3 0
313900970110     C                   Z-ADD     *ZEROS        I                 4 0
314000970110     C     *LOVAL        SETLL     AZORG01L
314100970110     C                   READ      AZORG01L                               99
314200970110DO  1C     *IN99         DOWEQ     *OFF
314300970110IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATE
314400970110     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
314500970110     C                   ADD       1             I
314600970110     C                   Z-ADD     ORGFIL        ORG(I)                         *FILIALE
314700970110     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
314800020730     C                   MOVEL     ORGDE3        OG143
314900020730     C*
315000020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
315100020730     C                   CLEAR                   DNTW
315200020730     C*
315300020730     C                   MOVEL(P)  'NTW'         KTECOD
315400020730     C                   MOVEL(P)  §OGNTW        KTEKE1
315500020730     C     KEYTBE        CHAIN     TNTBE01L
315600020730     C                   IF        %found(TNTBE01L)
315700020730     C                   MOVEL     TBEUNI        DNTW
315800020730     C                   ENDIF
315900020730     C*
316000020730     C                   MOVEL     §NTWFIE       FL1(I)                         *FLAG ITALIA/ESTERO
316100970110     C* SELEZIONI
316200970430IF  3C     PARLNP        IFEQ      999                                          *TUTTE
316300970430     C                   Z-ADD     ORGFIL        SORG(I)                        *FILIALI SELEZIONATE
316400020730     C                   MOVEL     §NTWFIE       SFL1(I)                        *FLAG ITALIA/ESTERO
316500040920     c                   if        §OGNTW='DPD'
316600040920     c                   movel     'E'           sfl1(i)
316700040920     c                   end
316800970110X   3C                   ELSE
316900970110IF  4C     PARLNP        IFEQ      ORGFIL                                       *UNA FILIALE
317000970417     C                   Z-ADD     ORGFIL        SORG(1)                        *FILIALE SELEZIONATA
317100020730     C                   MOVEL     §NTWFIE       SFL1(1)                        *FLAG ITALIA/ESTERO
317200040920     c                   if        §OGNTW='DPD'
317300040920     c                   movel     'E'           sfl1(i)
317400040920     c                   end
317500970110E   4C                   ENDIF
317600970110E   3C                   ENDIF
317700081105     C* VERIFICO SE FILIALE dpd
317800020227     C                   MOVEL     ORGDE3        OG143
317900081105     C                   IF        §OGNTW = 'DPD'
318000081105     C                   ADD       1             xx                3 0
318100081105     C                   MOVEL     ORGFIL        FILdpd(xx)
318200020227     C                   ENDIF
318300081105     C* VERIFICO SE FILIALE FEDEX
318400081105     C                   MOVEL     ORGDE3        OG143
318500081105     C                   IF        §OGNTW = 'FED'
318600081105     C                   ADD       1             JJ
318700081105     C                   MOVEL     ORGFIL        FILFED(JJ)
318800081105     C                   ENDIF
318900970110E   2C                   ENDIF
319000970110     C                   READ      AZORG01L                               99
319100970110E   1C                   ENDDO
319200970423     C*--------------------
319300970423     C* TARIFFA ITALIA/ESTERO
319400970423     C*--------------------
319500970423IF  1C     PARFIE        IFEQ      *BLANKS                                      *ITALIA E ESTERO
319600970423     C                   MOVEL     'I'           SFIE(1)
319700970423     C                   MOVEL     'E'           SFIE(2)
319800970423X   1C                   ELSE                                                   *ITALIA O ESTERO
319900970423     C                   MOVEL     PARFIE        SFIE(1)
320000970423E   1C                   ENDIF
320100970110     C*--------------------
320200970110     C* TIPI TARIFFA
320300970110     C*--------------------
320400970110     C                   Z-ADD     *ZEROS        I                 4 0
320500020923     C                   Z-ADD     1             KTBKUT
320600970110     C                   MOVEL     'TR'          KTBCOD
320700970110     C     KE2TAB        SETLL     TABEL00F
320800970110     C     KE2TAB        READE     TABEL00F                               99
320900970110DO  1C     *IN99         DOWEQ     *OFF
321000970110IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
321100970110     C                   MOVEL     TBLUNI        DSTR
321200970110IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
321300970110     C                   ADD       1             I
321400970110     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
321500970110     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
321600970110E   3C                   ENDIF
321700970110E   2C                   ENDIF
321800970110     C     KE2TAB        READE     TABEL00F                               99
321900970110E   1C                   ENDDO
322000050808     C*--------------------
322100050808     C* cod. trattamento merce con packing list
322200050808     C*--------------------
322300050808     C                   Z-ADD     *ZEROS        I                 4 0
322400050808     C                   Z-ADD     1             KTBKUT
322500050808     C                   MOVEL     '1B'          KTBCOD
322600050808     C     KE2TAB        SETLL     TABEL00F
322700050808     C     KE2TAB        READE     TABEL00F                               99
322800050808DO  1C     *IN99         DOWEQ     *OFF
322900050808IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
323000050808     C                   MOVEL     TBLUNI        DS1b
323100050808IF  3C     §1bf16        IFEQ      'S'                                          *IN GESTIONE
323200050808     C                   ADD       1             I
323300050808     C                   MOVEL     TBLKEY        cpkl(I)                        *CODICE
323400050808E   3C                   ENDIF
323500050808E   2C                   ENDIF
323600050808     C     KE2TAB        READE     TABEL00F                               99
323700050808E   1C                   ENDDO
323800970110     C*--------------------
323900970110     C* RAGGRUPPAMENTI TIPO BOLLA
324000970110     C*--------------------
324100970110     C                   Z-ADD     *ZEROS        I                 4 0
324200020923     C                   Z-ADD     1             KTBKUT
324300970110     C                   MOVEL     '2W'          KTBCOD
324400970110     C     KE2TAB        SETLL     TABEL00F
324500970110     C     KE2TAB        READE     TABEL00F                               99
324600970110DO  1C     *IN99         DOWEQ     *OFF
324700970110IF  2C     TBLFLG        IFEQ      *BLANKS
324800970110     C                   MOVEL     TBLUNI        DS2W
324900970110     C                   ADD       1             I
325000970110     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
325100970110     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
325200970110     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
325300970110E   2C                   ENDIF
325400970110     C     KE2TAB        READE     TABEL00F                               99
325500970110E   1C                   ENDDO
325600970110     C*--------------------
325700970110     C* TIPI BOLLA
325800970110     C*--------------------
325900970110     C                   Z-ADD     *ZEROS        I                 4 0
326000970110     C                   Z-ADD     *ZEROS        J                 4 0
326100020923     C                   Z-ADD     1             KTBKUT
326200970110     C                   MOVEL     'TB'          KTBCOD
326300970110     C     KE2TAB        SETLL     TABEL00F
326400970110     C     KE2TAB        READE     TABEL00F                               99
326500970110DO  1C     *IN99         DOWEQ     *OFF
326600970110IF  2C     TBLFLG        IFEQ      *BLANKS
326700970110     C                   MOVEL     TBLUNI        DSTB
326800970110     C                   ADD       1             I
326900970110     C                   MOVEL     TBLKEY        TBL(I)                         *TIPO BOLLA
327000970110     C                   Z-ADD     1             J
327100970110     C                   SETOFF                                           98
327200970110     C     §TBRBL        LOOKUP    RBL(J)                                 98
327300970110IF  3C     *IN98         IFEQ      *ON
327400970110     C                   MOVEL     FSP(J)        SPD(I)                         *FLAG SPEDIZIONE
327500970110     C                   MOVEL     FRI(J)        RIC(I)                         *FLAG RICAVO
327600970110X   3C                   ELSE
327700970110     C                   MOVEL     'N'           SPD(I)                         *FLAG SPEDIZIONE
327800970110     C                   MOVEL     'N'           RIC(I)                         *FLAG RICAVO
327900970110E   3C                   ENDIF
328000970110E   2C                   ENDIF
328100970110     C     KE2TAB        READE     TABEL00F                               99
328200970110E   1C                   ENDDO
330000970110     C*--------------------
330100970110     C* CODICI PAGAMENTO
330200970110     C*--------------------
330300970110     C                   Z-ADD     *ZEROS        I                 4 0
330400020923     C                   Z-ADD     1             KTBKUT
330500970110     C                   MOVEL     'FA'          KTBCOD
330600970110     C     KE2TAB        SETLL     TABEL00F
330700970110     C     KE2TAB        READE     TABEL00F                               99
330800970110DO  1C     *IN99         DOWEQ     *OFF
330900970110IF  2C     TBLFLG        IFEQ      *BLANKS
331000970110     C                   MOVEL     TBLUNI        DSFA
331100970110     C                   ADD       1             I
331200970110     C                   MOVEL     TBLKEY        CDP(I)                         *CODICE PAGAMENTO
331300970110     C                   MOVEL     §FARAT        CRAT(I)                        *RATE DI PAGAMENTO
331400970110E   2C                   ENDIF
331500970110     C     KE2TAB        READE     TABEL00F                               99
331600970110E   1C                   ENDDO
331700970313     C*--------------------
331800970313     C* CODICI TASSAZIONE
331900970313     C*--------------------
332000970313     C                   Z-ADD     *ZEROS        I                 4 0
332100020923     C                   Z-ADD     1             KTBKUT
332200970313     C                   MOVEL     'CT'          KTBCOD
332300970313     C     KE2TAB        SETLL     TABEL00F
332400970313     C     KE2TAB        READE     TABEL00F                               99
332500970313DO  1C     *IN99         DOWEQ     *OFF
332600970313IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
332700970313     C                   MOVEL     TBLUNI        DSCT
332800970317IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
332900970317     C                   ADD       1             I
333000970317     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
333100970317     C                   MOVEL     §CTDES        DCTS(I)                        *DESCRIZIONE
333200970317     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
333300970317     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
333400970317     C                   MOVE      §CTCOR        WA1               1            *TIPO CODICE
333500970417IF  4C     WA1           IFEQ      '9'
333600970417IF  5C     CTS(I)        IFEQ      'IT'
333700970317     C     CTS(I)        OREQ      'EE'
333800970317     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
333900970417X   5C                   ELSE
334000970317     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
334100970417E   5C                   ENDIF
334200970417X   4C                   ELSE
334300970317     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
334400970417E   4C                   ENDIF
334500970417IF  4C     §CTNAR        IFEQ      *BLANKS
334600970317     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
334700970317     C                   MOVEL     'I'           CIE(I)                         *CODICE ITALIA
334800970417X   4C                   ELSE
334900970317     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
335000970317     C                   MOVEL     'E'           CIE(I)                         *CODICE ESTERO
335100970417E   4C                   ENDIF
335200970317E   3C                   ENDIF
335300970317E   2C                   ENDIF
335400970317     C     KE2TAB        READE     TABEL00F                               99
335500970317E   1C                   ENDDO
335600040920     C* CARICO GLI SCAGLIONI DI DEFAULT
335700040920IF  1C     PARTTR        IFEQ      '0'                                          *QUINTALI
335800040920     C     PARTTR        OREQ      '3'                                          *KG
335900040920     C                   Z-ADD     2             SGL(1)
336000040920     C                   Z-ADD     5             SGL(2)
336100040920     C                   Z-ADD     10            SGL(3)
336200040920     C                   Z-ADD     25            SGL(4)
336300040920     C                   Z-ADD     50            SGL(5)
336400040920     C                   Z-ADD     75            SGL(6)
336500040920     C                   Z-ADD     100           SGL(7)
336600040920     C                   Z-ADD     99999         SGL(8)
336700040920X   1C                   ELSE                                                   *COLLI
336800040920     C                   Z-ADD     1             SGL(1)
336900040920     C                   Z-ADD     2             SGL(2)
337000040920     C                   Z-ADD     3             SGL(3)
337100040920     C                   Z-ADD     4             SGL(4)
337200040920     C                   Z-ADD     99999         SGL(5)
337300040920E   1C                   ENDIF
337400970110     C*
337500970110     C                   ENDSR
337600970313     C*------------------------------------------------------------------------*
337700970313     C* CARTAR - CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
337800970313     C*------------------------------------------------------------------------*
337900970313     C     CARTAR        BEGSR
338000970313     C*
338100970313     C* REPERISCE IL CODICE DELLA TARIFFA DI CARTELLO IN VIGORE
338200970314IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORE
338300021202     C                   Z-ADD     *ZEROS        CARKSC
338400021202     C                   Z-ADD     *ZEROS        CARCTR
338500021202     C                   MOVE(P)   *ZEROS        CARPRG
338600021202     C                   CLEAR                   TRUL27DS
338700021202     C                   EVAL      I27TLA = 'L'
338800021202     C                   EVAL      I27DTA = DATCOR
338900021202     C                   CALL      'TRUL27R'
339000021202     C                   PARM                    TRUL27DS
339100021202     C*
339200021202     C                   IF        O27ERR <> 'E'
339300021202     C                   Z-ADD     O27KSC        CARKSC
339400021202     C                   Z-ADD     O27CTR        CARCTR
339500021202     C                   MOVE(P)   O27PRG        CARPRG
339600021202     C                   ENDIF
339700021202E   1C                   ENDIF
339800970317     C*
339900970317     C* PER CIASCUNA FILIALE RICHIESTA
340000971124DO  1C     1             DO        500           Y                 4 0
340100970417IF  2C     SORG(Y)       IFGT      *ZEROS
340200970423     C     Y             OCCUR     FTAR
340300970317     C*---
340400970317     C* CARICA GLI SCAGLIONI DA CONSIDERARE -CLIENTE O CARTELLO-
340500970317     C*---
340600970416     C                   Z-ADD     *ZEROS        SSGL
340700970313     C                   Z-ADD     PARKOV        KTDKSC
340800970313     C                   Z-ADD     PARCTR        KTDCTR
340900970313     C                   Z-ADD     PARPRG        KTDPRG
341000970417     C                   Z-ADD     SORG(Y)       KTDLNP
341100970313     C                   EXSR      CARSGL
341200970417IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
341300970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
341400970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
341500970417     C                   Z-ADD     CARKSC        KTDKSC
341600970313     C                   Z-ADD     CARCTR        KTDCTR
341700970313     C                   Z-ADD     CARPRG        KTDPRG
341800970417     C                   Z-ADD     SORG(Y)       KTDLNP
341900970314     C                   EXSR      CARSGL
342000970703     C                   MOVEL     DEPCTO        PARCTO
342100970418E   3C                   ENDIF
342200970418IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
342300040920     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
342400040920     c                   move      sgl           ssgl
342500970418E   3C                   ENDIF
342600970317     C*---
342700970313     C* CARICA I CODICI TASSAZIONE DA CONSIDERARE -CLIENTE O CARTELLO-
342800970317     C*---
342900970423     C                   MOVEL     *BLANKS       CCTI
343000970423     C                   MOVEL     *BLANKS       CRCI
343100970423     C                   MOVEL     *BLANKS       CCTE
343200970423     C                   MOVEL     *BLANKS       CRCE
343300970423DO  3C     1             DO        2             W                 4 0
343400970423IF  4C     SFIE(W)       IFNE      *BLANKS
343500970314     C                   Z-ADD     PARKOV        KTDKSC
343600970314     C                   Z-ADD     PARCTR        KTDCTR
343700970314     C                   Z-ADD     PARPRG        KTDPRG
343800970417     C                   Z-ADD     SORG(Y)       KTDLNP
343900971204     C                   MOVEL     SFL1(Y)       DLPFL1
344000970314     C                   EXSR      CARCTS
344100970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
344200970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
344300970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
344400970314     C                   Z-ADD     CARKSC        KTDKSC
344500970314     C                   Z-ADD     CARCTR        KTDCTR
344600970314     C                   Z-ADD     CARPRG        KTDPRG
344700970417     C                   Z-ADD     SORG(Y)       KTDLNP
344800971204     C                   MOVEL     SFL1(Y)       DLPFL1
344900970314     C                   EXSR      CARCTS
345000970703     C                   MOVEL     DEPCTO        PARCTO
345100970423E   5C                   ENDIF
345200970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
345300970418     C                   EXSR      FIXCTS                                       *IMPOSTA FISSI CTS
345400970423E   5C                   ENDIF
345500970416     C                   EXSR      RAPSVI                                       *CTS->CTS ELEMENTARI
345600970423E   4C                   ENDIF
345700970423E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
345800970417     C*
345900970417E   2C                   ENDIF
346000970417E   1C                   ENDDO                                                  *FINE FILIALI
346100970313     C*
346200970313     C                   ENDSR
346300970313     C*------------------------------------------------------------------------*
346400970313     C* CARSGL - CARICA GLI SCAGLIONI
346500970313     C*------------------------------------------------------------------------*
346600970313     C     CARSGL        BEGSR
346700970313     C*
346800970313     C* AZZERA LE VARIABILI DI LAVORO
346900970313     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
347000970313     C                   Z-ADD     *ZEROS        I                 4 0
347100970313     C*
347200011128     C                   CLEAR                   DEPORP
347300060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
347400990820     C     KEYTAD        SETLL     TITAD04L                               99
347500990820     C     KEYTAD        READE     TITAD04L                               99
347600060804X   1C                   ELSE                                                   *OFFERTA
347700060804     C     KEYTAD        SETLL     TIOFD01L                               99
347800060804     C     KEYTAD        READE     TIOFD01L                               99
347900060804E   1C                   ENDIF
348000011128     C                   EVAL      DEPORP = TADORP                              *SOLO un cts
348100011010DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
348200011128     C     TADORP        ANDEQ     DEPORP                                       *ROTTURA ORDINAMENTO
348300011010IF  2C     TADATB        IFEQ      *BLANKS
348400011010     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
348500011010     C     TADSGL        LOOKUP    SSGL                                   98
348600011010IF  3C     *IN98         IFEQ      *OFF
348700030122     C                   IF        I < 10
348800011010     C                   ADD       1             I
348900011010     C                   Z-ADD     TADSGL        SSGL(I)
349000030122     C                   ELSE
349100030122     C                   LEAVE
349200030122     C                   ENDIF
349300011010E   3C                   ENDIF
349400011010E   2C                   ENDIF
349500060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
349600990820     C     KEYTAD        READE     TITAD04L                               99
349700060804X   2C                   ELSE                                                   *OFFERTA
349800060804     C     KEYTAD        READE     TIOFD01L                               99
349900060804E   2C                   ENDIF
350000970313E   1C                   ENDDO                                                  *FINE DETTAGLIO
350100970313     C*
350200970313     C                   ENDSR
350300970314     C*------------------------------------------------------------------------*
350400970314     C* CARCTS   CARICA I CODICI TASSAZIONE
350500970314     C*------------------------------------------------------------------------*
350600970314     C     CARCTS        BEGSR
350700970314     C*
350800970314     C* AZZERA LE VARIABILI DI LAVORO
350900970314     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
351000970314     C                   Z-ADD     *ZEROS        I                 4 0
351100970317     C                   CLEAR                   WCTS
351200970317     C                   CLEAR                   WRAP
351300970317     C                   CLEAR                   WNAR
351400970317     C                   CLEAR                   WTCT
351500970314     C*
351600970314     C* MEMORIZZA I CODICI DELLA LINEA DI PARTENZA RICHIESTA
351700060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
351800990820     C     KEYTAD        SETLL     TITAD04L                               99
351900990820     C     KEYTAD        READE     TITAD04L                               99
352000060804X   1C                   ELSE                                                   *OFFERTA
352100060804     C     KEYTAD        SETLL     TIOFD01L                               99
352200060804     C     KEYTAD        READE     TIOFD01L                               99
352300060804E   1C                   ENDIF
352400970314DO  1C     *IN99         DOWEQ     *OFF
352500970314IF  2C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
352600970314     C                   SETOFF                                           98
352700970314     C                   Z-ADD     1             J                 4 0
352800970314     C     TADCTS        LOOKUP    CTS(J)                                 98
352900970314IF  3C     *IN98         IFEQ      *ON                                          *TROVATO
353000970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
353100970417     C     CIE(J)        ANDEQ     'I'                                          *CODICI ITALIA
353200970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
353300971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
353400970417     C     CIE(J)        ANDEQ     'E'                                          *CODICI ESTERO
353500970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
353600971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ITALIA
353700970314     C                   SETOFF                                           98
353800970314     C                   Z-ADD     1             K                 4 0
353900970314     C     TADCTS        LOOKUP    WCTS(K)                                98
354000970417IF  5C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
354100970417     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
354200970314     C                   ADD       1             I
354300970314     C                   MOVEL     TADCTS        WCTS(I)                        *CODICE
354400970314     C                   MOVEL     RAP(J)        WRAP(I)                        *REGIONE
354500970314     C                   MOVEL     NAR(J)        WNAR(I)                        *NAZIONE
354600970314     C                   MOVEL     TCT(J)        WTCT(I)                        *TIPO
354700970417E   5C                   ENDIF
354800970417E   4C                   ENDIF
354900970314E   3C                   ENDIF
355000970314E   2C                   ENDIF
355100060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
355200990820     C     KEYTAD        READE     TITAD04L                               99
355300060804X   2C                   ELSE                                                   *OFFERTA
355400060804     C     KEYTAD        READE     TIOFD01L                               99
355500060804E   2C                   ENDIF
355600970314E   1C                   ENDDO
355700970314     C*
355800970314     C                   ENDSR
355900970421     C*------------------------------------------------------------------------*
356000970421     C* FIXCTS - IMPOSTA CODICI TASSAZIONE FISSI
356100970421     C*------------------------------------------------------------------------*
356200970421     C     FIXCTS        BEGSR
356300970421     C*
356400970421     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
356500970421     C*
356600970421     C                   Z-ADD     *ZEROS        J                 4 0
356700050103DO  1C     1             DO        600           I
356800970421IF  2C     CTS(I)        IFNE      *BLANKS
356900970421IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
357000970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
357100970421     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
357200970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
357300971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
357400970421     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
357500970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
357600971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
357700970421     C                   ADD       1             J
357800970421     C                   MOVEL     CTS(I)        WCTS(J)                        *CODICE
357900970421     C                   MOVEL     RAP(I)        WRAP(J)                        *REGIONE
358000970421     C                   MOVEL     NAR(I)        WNAR(J)                        *NAZIONE
358100970421     C                   MOVEL     TCT(I)        WTCT(J)                        *TIPO
358200970421E   4C                   ENDIF
358300970421E   3C                   ENDIF
358400970421E   2C                   ENDIF
358500970421E   1C                   ENDDO
358600970421     C*
358700970421     C                   ENDSR
358800970314     C*------------------------------------------------------------------------*
358900970314     C* RAPSVI - TRASFORMA I CODICI DELLA TARIFFA IN CODICI ELEMENTARI
359000970314     C*------------------------------------------------------------------------*
359100970314     C     RAPSVI        BEGSR
359200970314     C*
359300970314     C* 1°GIRO-> CARICA TUTTI I CODICI ELEMENTARI COME -NON CALCOLARE-
359400970314     C                   Z-ADD     *ZEROS        J                 4 0
359500050103DO  1C     1             DO        600           I
359600970416IF  2C     CTS(I)        IFNE      *BLANKS
359700970416IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
359800970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
359900970417     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
360000970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
360100971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
360200970417     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
360300970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
360400971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
360500970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
360600970314     C                   ADD       1             J
360700970423     C                   MOVEL     CTS(I)        CCTI(J)                        *CODICE
360800970423     C                   MOVEL     *BLANKS       CRCI(J)                        *RAGGRUPPAMENTO
360900970423X   5C                   ELSE                                                   *ESTERO
361000970423     C                   ADD       1             J
361100970423     C                   MOVEL     CTS(I)        CCTE(J)                        *CODICE
361200970423     C                   MOVEL     *BLANKS       CRCE(J)                        *RAGGRUPPAMENTO
361300970423E   5C                   ENDIF
361400970423E   4C                   ENDIF
361500970417E   3C                   ENDIF
361600970314E   2C                   ENDIF
361700970314E   1C                   ENDDO                                                  *FINE CODICI BASE
361800970314     C*
361900970314     C* 2°GIRO-> TRASFORMA I -NON CALCOLARE- IN -CALCOLARE IN NAZIONE-
362000050103DO  1C     1             DO        600           I
362100970314IF  2C     WCTS(I)       IFNE      *BLANKS
362200970314IF  3C     WTCT(I)       IFEQ      'N'                                          *SOLO NAZIONE
362300050103DO  4C     1             DO        600           J
362400970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
362500970423IF  6C     CCTI(J)       IFNE      *BLANKS
362600970314     C                   SETOFF                                           98
362700970314     C                   Z-ADD     1             K                 4 0
362800970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
362900970423IF  7C     WNAR(I)       IFEQ      NAR(K)
363000970423     C                   MOVEL     'N'           CRCI(J)
363100970423E   7C                   ENDIF
363200970423E   6C                   ENDIF
363300970423X   5C                   ELSE                                                   *ESTERO
363400970423IF  6C     CCTE(J)       IFNE      *BLANKS
363500970423     C                   SETOFF                                           98
363600970423     C                   Z-ADD     1             K                 4 0
363700970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
363800970423IF  7C     WNAR(I)       IFEQ      NAR(K)
363900970423     C                   MOVEL     'N'           CRCE(J)
364000970423E   7C                   ENDIF
364100970423E   6C                   ENDIF
364200970423E   5C                   ENDIF
364300970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
364400970314E   3C                   ENDIF
364500970314E   2C                   ENDIF
364600970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
364700970314     C*
364800970314     C* 3°GIRO-> TRASFORMA I -CALCOLARE IN NAZIONE- IN -CALCOLARE IN REGIONE-
364900050103DO  1C     1             DO        600           I
365000970314IF  2C     WCTS(I)       IFNE      *BLANKS
365100970314IF  3C     WTCT(I)       IFEQ      'R'                                          *SOLO REGIONE
365200050103DO  4C     1             DO        600           J
365300970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
365400970423IF  6C     CCTI(J)       IFNE      *BLANKS
365500970314     C                   SETOFF                                           98
365600970314     C                   Z-ADD     1             K                 4 0
365700970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
365800970423IF  7C     WRAP(I)       IFEQ      RAP(K)
365900970423     C                   MOVEL     'R'           CRCI(J)
366000970423E   7C                   ENDIF
366100970423E   6C                   ENDIF
366200970423X   5C                   ELSE                                                   *ESTERO
366300970423IF  6C     CCTE(J)       IFNE      *BLANKS
366400970423     C                   SETOFF                                           98
366500970423     C                   Z-ADD     1             K                 4 0
366600970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
366700970423IF  7C     WRAP(I)       IFEQ      RAP(K)
366800970423     C                   MOVEL     'R'           CRCE(J)
366900970423E   7C                   ENDIF
367000970423E   6C                   ENDIF
367100970314E   5C                   ENDIF
367200970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
367300970314E   3C                   ENDIF
367400970314E   2C                   ENDIF
367500970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
367600970314     C*
367700970314     C* 4°GIRO-> TRASFORMA I -CALCOLARE IN REGIONE- IN -CALCOLARE-
367800050103DO  1C     1             DO        600           I
367900970314IF  2C     WCTS(I)       IFNE      *BLANKS
368000970314IF  3C     WTCT(I)       IFEQ      'E'                                          *SOLO ELEMENTARI
368100050103DO  4C     1             DO        600           J
368200970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
368300970423IF  6C     CCTI(J)       IFNE      *BLANKS
368400970423IF  7C     WCTS(I)       IFEQ      CCTI(J)
368500970423     C                   MOVEL     'T'           CRCI(J)
368600970423E   7C                   ENDIF
368700970423E   6C                   ENDIF
368800970423X   5C                   ELSE                                                   *ESTERO
368900970423IF  6C     CCTE(J)       IFNE      *BLANKS
369000970423IF  7C     WCTS(I)       IFEQ      CCTE(J)
369100970423     C                   MOVEL     'T'           CRCE(J)
369200970423E   7C                   ENDIF
369300970423E   6C                   ENDIF
369400970423E   5C                   ENDIF
369500970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
369600970314E   3C                   ENDIF
369700970314E   2C                   ENDIF
369800970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
369900970314     C*
370000970314     C                   ENDSR
370100981218     C*------------------------------------------------------------------------*
370200981218     C* CANTAR - ANNULLAMENTO TARIFFA
370300981218     C*------------------------------------------------------------------------*
370400981218     C     CANTAR        BEGSR
370500981218     C*
370600981218     C* CANCELLA DATI TIPO - GENERALI
370700981218     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE
370800060804     C                   MOVEL     parcto        KTCCTO                         *DATI TARIFFA
370900981218     C                   Z-ADD     PARKOV        KTCKSC                         *CLIENTE
371000011001     C     KE3ETC        SETLL     TEETC01L
371100011001     C     KE3ETC        READE     TEETC01L                               99
371200981218DO  1C     *IN99         DOWEQ     *OFF
371300981218IF  2C     PARCTR        IFEQ      ETCCTR                                       *TARIFFA RICHIESTA
371400981218IF  3C     PARPRG        IFEQ      ETCPRG                                       *PROGRESSIVO RICHIES
371500981218IF  4C     PARLNP        IFEQ      ETCLNP                                       *LINEA RICHIESTA
371600981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
371700981218     C                   MOVEL     'A'           ETCATB                         *ANNULLAMENTO
371800981218     C                   MOVEL     *BLANKS       ETCFTR                         *FLAG TRASMISSIONE
371900011128     C*---               Z-ADD     *ZEROS        ETCDTR                         *DATA TRASMISSIONE
372000011001     C                   UPDATE    TEETC000                                     *AGGIORNA
372100981218E   4C                   ENDIF
372200981218E   3C                   ENDIF
372300981218E   2C                   ENDIF
372400011001     C     KE3ETC        READE     TEETC01L                               99
372500981218E   1C                   ENDDO                                                  *FINE DATI GENERALI
372600981218     C*
372700981218     C* CANCELLA DATI TIPO - SCAGLIONI
372800060829     c* ricavi
372900981218     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE
373000060804     C                   MOVEL     parcto        KTSCTO                         *DATI TARIFFA
373100981218     C                   Z-ADD     PARKOV        KTSKSC                         *CLIENTE
373200011001     C     KE3ETS        SETLL     TEETS01L
373300011001     C     KE3ETS        READE     TEETS01L                               99
373400981218DO  1C     *IN99         DOWEQ     *OFF
373500981218IF  2C     PARCTR        IFEQ      ETSCTR                                       *TARIFFA RICHIESTA
373600981218IF  3C     PARPRG        IFEQ      ETSPRG                                       *PROGRESSIVO RICHIES
373700981218IF  4C     PARLNP        IFEQ      ETSLNP                                       *LINEA RICHIESTA
373800981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
373900981218     C                   MOVEL     'A'           ETSATB                         *ANNULLAMENTO
374000981218     C                   MOVEL     *BLANKS       ETSFTR                         *FLAG TRASMISSIONE
374100011001     C                   UPDATE    TEETS000                                     *AGGIORNA
374200981218E   4C                   ENDIF
374300981218E   3C                   ENDIF
374400981218E   2C                   ENDIF
374500011001     C     KE3ETS        READE     TEETS01L                               99
374600981218E   1C                   ENDDO                                                  *FINE SCAGLIONI
374700060829     c* competetnze
374800060829     C     KE3ETS        SETLL     TEETS11L
374900060829     C     KE3ETS        READE     TEETS11L                               99
375000060829DO  1C     *IN99         DOWEQ     *OFF
375100060829IF  2C     PARCTR        IFEQ      £ETSCTR                                      *TARIFFA RICHIESTA
375200060829IF  3C     PARPRG        IFEQ      £ETSPRG                                      *PROGRESSIVO RICHIES
375300060829IF  4C     PARLNP        IFEQ      £ETSLNP                                      *LINEA RICHIESTA
375400060829     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
375500060829     C                   MOVEL     'A'           £ETSATB                        *ANNULLAMENTO
375600060829     C                   MOVEL     *BLANKS       £ETSFTR                        *FLAG TRASMISSIONE
375700060829     C                   UPDATE    TEETS100                                     *AGGIORNA
375800060829E   4C                   ENDIF
375900060829E   3C                   ENDIF
376000060829E   2C                   ENDIF
376100060829     C     KE3ETS        READE     TEETS11L                               99
376200060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
376300981218     C*
376400981218     C* CANCELLA DATI TIPO - CODICI TASSAZIONE
376500981218     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE
376600060804     C                   MOVEL     parcto        KTTCTO                         *DATI TARIFFA
376700981218     C                   Z-ADD     PARKOV        KTTKSC                         *CLIENTE
376800011001     C     KE3ETT        SETLL     TEETT01L
376900011001     C     KE3ETT        READE     TEETT01L                               99
377000981218DO  1C     *IN99         DOWEQ     *OFF
377100981218IF  2C     PARCTR        IFEQ      ETTCTR                                       *TARIFFA RICHIESTA
377200981218IF  3C     PARPRG        IFEQ      ETTPRG                                       *PROGRESSIVO RICHIES
377300981218IF  4C     PARLNP        IFEQ      ETTLNP                                       *LINEA RICHIESTA
377400981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
377500981218IF  5C     PARFIE        IFEQ      ETTFIE                                       *ITA O EST RICHIESTO
377600981218     C     PARFIE        OREQ      *BLANKS                                      *TUTTO ITA E EST
377700981218     C                   MOVEL     'A'           ETTATB                         *ANNULLAMENTO
377800981218     C                   MOVEL     *BLANKS       ETTFTR                         *FLAG TRASMISSIONE
377900011001     C                   UPDATE    TEETT000                                     *AGGIORNA
378000981218E   5C                   ENDIF
378100981218E   4C                   ENDIF
378200981218E   3C                   ENDIF
378300981218E   2C                   ENDIF
378400011001     C     KE3ETT        READE     TEETT01L                               99
378500981218E   1C                   ENDDO                                                  *FINE CODICI TASSAZI
378600981218     C*
378700981218     C                   ENDSR
378800020923     C*--------------------------------------------------------------------------------------------*
378900020923     C* REPERISCE I DATI UTENTE
379000020923     C*--------------------------------------------------------------------------------------------*
379100020923     C     REPDATIUTE    BEGSR
379200020923     C*
379300020923     C* INIZIALIZZA VARIABILI DI WRK
379400020923     C                   CLEAR                   TIBS34DS
379500020923     C                   CLEAR                   AZUTEDS
379600020923     C                   CLEAR                   DDATIUTE
379700020923     C*
379800020923     C     *DTAARA       DEFINE    §azute        azuteds
379900020923     C     *DTAARA       DEFINE    §datiute      ddatiute
380000020923     C                   IN(E)     *DTAARA
380100020923if  1C                   IF        %Error
380200020923     c                   EVAL      I34Tla = 'L'
380300020923     C                   CALL      'TIBS34R'
380400020923     C                   PARM                    Tibs34Ds
380500020923     C                   IN        *DTAARA
380600020923e   1C                   ENDIF
380700020923     C*
380800020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
380900020923     C                   MOVEL(P)  RSUT          DSFIRS           20
381000020923     C*
381100020923     C                   ENDSR
381200021031     C*------------------------------------------------------------------------*
381300021031     C* REPCLI - CARICAMENTO LISTA CLIENTI PADRI/FIGLI
381400021031     C*------------------------------------------------------------------------*
381500021031     C     REPCLI        BEGSR
381600021031     C*
381700021031     C* SE RICHIESTA ELABORAZIONE X CLIENTE UNIFICANTE REPERISCO LA LISTA DEI FIGLI
381800021031     C                   IF        PARUNI = 'S'
381900021031     C                   CLEAR                   TIBS10DS
382000021031     C                   EVAL      D10DRF = DATCOR                              *data riferimento
382100021031     C                   EVAL      D10TLE = 'ST'                                *tipo legame
382200021031     C                   EVAL      D10PAF = 'F'                                 *cerca il padre
382300021031     C                   Z-ADD     PARKSC        D10COD                         *cod cliente da tst
382400021031     C                   CALL      'TIBS10R'
382500021031     C                   PARM                    TIBS10DS
382600021031     C*
382700021031     C                   IF        D10ERR <> *blanks OR                         *No unif. o padre
382800021031     C                             D10COP =  *zeros
382900021031     C                   CLEAR                   SKCLI
383000021031     C                   Z-ADD     PARKSC        SKCLI(1)
383100021031     C                   ENDIF
383200021031     C*
383300021031     C                   ELSE
383400021031     C                   CLEAR                   SKCLI
383500021031     C                   Z-ADD     PARKSC        SKCLI(1)
383600021031     C                   ENDIF
383700021031     C*
383800021031     C                   ENDSR
383900970110     C*------------------------------------------------------------------------*
384000970110     C* *INZSR - OPERAZIONI INIZIALI
384100970110     C*------------------------------------------------------------------------*
384200970110     C     *INZSR        BEGSR
384300970110     C*--------------------
384400970110     C* RICEVIMENTO PARAMETRI
384500970110     C*--------------------
384600970110     C     *ENTRY        PLIST
384700970110     C                   PARM                    KPJBA
384800970110     C                   MOVEL     KPJBU         PARAM
384900021121     C*
385000021121     C* CARICA NUOVE TABELLE OCCORRENTI
385100021121     C                   EXSR      CARGEDCE
385200021121     C*
385300021121     C* REPERISCO SUBITO LA DIVISA DELLA TARIFFA RICEVUTA IN INPUT
385400021121     C                   IF        PARDIV = *BLANKS
385500021121     C                   Z-ADD     PARKOV        KTMKSC
385600021121     C                   Z-ADD     PARCTR        KTMCTR
385700021121     C                   Z-ADD     PARPRG        KTMPRG
385800021121     C     KEYTAMUNIQ    CHAIN     TNTAM01L
385900021121     C                   IF        %FOUND
386000021121     C                   MOVEL(P)  TAMFLO        DSTA01
386100021121     C                   MOVEL(P)  §TADIV        PARDIV
386200021121     C                   ELSE
386300021121     C                   MOVEL(P)  WDIVCE        PARDIV
386400021121     C                   ENDIF
386500021121     C                   ENDIF
386600970110     C*--------------------
386700970110     C* VARIABILI RIFERITE AL DATABASE
386800970110     C*--------------------
386900970110     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
387000970110     C     *LIKE         DEFINE    TBLCOD        KTBCOD
387100970110     C     *LIKE         DEFINE    TBLKEY        KTBKEY
387200970110     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
387300970110     C     *LIKE         DEFINE    ACOKCC        KACKCC
387400970110     C     *LIKE         DEFINE    ACOKSC        KACKSC
387500970110     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
387600970110     C     *LIKE         DEFINE    CLPKCC        KCLKCC
387700970110     C     *LIKE         DEFINE    CLPKSC        KCLKSC
387800970110     C     *LIKE         DEFINE    INDKUT        KINKUT                         *CNIND00F
387900970110     C     *LIKE         DEFINE    INDKCC        KINKCC
388000970415     C     *LIKE         DEFINE    INDKSC        KINKSC
388100970110     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
388200970110     C     *LIKE         DEFINE    TAMCTR        KTMCTR
388300021121     C     *LIKE         DEFINE    TAMPRG        KTMPRG
388400990820     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
388500970110     C     *LIKE         DEFINE    TADCTR        KTDCTR
388600970110     C     *LIKE         DEFINE    TADPRG        KTDPRG
388700970317     C     *LIKE         DEFINE    TADLNP        KTDLNP
388800990812     C     *LIKE         DEFINE    TASKSC        KTAKSC                         *TITAS00F
388900970110     C     *LIKE         DEFINE    TASAAS        KTAAAS
389000970110     C     *LIKE         DEFINE    TASMGS        KTAMGS
389100970110     C     *LIKE         DEFINE    CSBAAS        KSBAAS                         *TNCSB00F
389200970110     C     *LIKE         DEFINE    CSBLNP        KSBLNP
389300970110     C     *LIKE         DEFINE    CSBNRS        KSBNRS
389400970110     C     *LIKE         DEFINE    CSBNSP        KSBNSP
389500011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
389600980713     C     *LIKE         DEFINE    ETCCTO        KTCCTO
389700970110     C     *LIKE         DEFINE    ETCKSC        KTCKSC
389800970110     C     *LIKE         DEFINE    ETCCTR        KTCCTR
389900970110     C     *LIKE         DEFINE    ETCPRG        KTCPRG
390000970110     C     *LIKE         DEFINE    ETCLNP        KTCLNP
390100011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
390200980713     C     *LIKE         DEFINE    ETSCTO        KTSCTO
390300970110     C     *LIKE         DEFINE    ETSKSC        KTSKSC
390400970110     C     *LIKE         DEFINE    ETSCTR        KTSCTR
390500970110     C     *LIKE         DEFINE    ETSPRG        KTSPRG
390600970110     C     *LIKE         DEFINE    ETSLNP        KTSLNP
390700970110     C     *LIKE         DEFINE    ETSSGL        KTSSGL
390800011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
390900980713     C     *LIKE         DEFINE    ETTCTO        KTTCTO
391000970110     C     *LIKE         DEFINE    ETTKSC        KTTKSC
391100970110     C     *LIKE         DEFINE    ETTCTR        KTTCTR
391200970110     C     *LIKE         DEFINE    ETTPRG        KTTPRG
391300970110     C     *LIKE         DEFINE    ETTLNP        KTTLNP
391400970110     C     *LIKE         DEFINE    ETTFIE        KTTFIE
391500970110     C     *LIKE         DEFINE    ETTCTS        KTTCTS
391600970110     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DEPOSITI
391700970110     C     *LIKE         DEFINE    TAMCTR        CARCTR
391800970110     C     *LIKE         DEFINE    TAMPRG        CARPRG
391900970430     C     *LIKE         DEFINE    TASLNP        DEPFIL
392000980713     C     *LIKE         DEFINE    PARCTO        DEPCTO
392100020730     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
392200011128     C     *LIKE         DEFINE    TADORP        DEPORP
392300980821     C     *LIKE         DEFINE    ETCLNP        XORG                           *DI PASSAGGIO
392400980821     C     *LIKE         DEFINE    ETTFIE        XFIE
392500011011     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
392600011011     C     *LIKE         DEFINE    TBEKE1        KTEKE1
392700050309     C     *LIKE         DEFINE    GCPAAS        KGPAAS                         *TIGCP00F
392800021122     C     *LIKE         DEFINE    GCPLNP        KGPLNP
392900021122     C     *LIKE         DEFINE    GCPNRS        KGPNRS
393000021122     C     *LIKE         DEFINE    GCPNSP        KGPNSP
393100980821     C*--------------------
393200970110     C* CHAIVI DI LETTURA
393300970110     C*--------------------
393400060315     C* CHIAVE LETTURA TITA730C - COMPLETA
393500060315     C     KEYTA7        KLIST
393600060315     C                   KFLD                    TA7AAS
393700060315     C                   KFLD                    TA7LNP
393800060315     C                   KFLD                    TA7NRS
393900060315     C                   KFLD                    TA7NSP
394000060315     C                   KFLD                    TA7TBL
394100970110     C* LETTURA TABEL00F - COMPLETA
394200970110     C     KEYTAB        KLIST
394300970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
394400970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
394500970110     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
394600970110     C* LETTURA TABEL00F - PARZIALE
394700970110     C     KE2TAB        KLIST
394800970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
394900970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
395000970110     C* LETTURA CNACO00F - COMPETA
395100970110     C     KEYACO        KLIST
395200970110     C                   KFLD                    KACKUT                         *CODICE UTENTE
395300970110     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
395400970110     C                   KFLD                    KACKSC                         *CODICE CLIENTE
395500970110     C* LETTURA CNCLP00F - COMPLETA
395600970110     C     KEYCLP        KLIST
395700970110     C                   KFLD                    KCLKUT                         *CODICE UTENTE
395800970110     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
395900970110     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
396000970415     C* LETTURA CNIND00F - COMPLETA
396100970415     C     KEYIND        KLIST
396200970415     C                   KFLD                    KINKUT                         *CODICE UTENTE
396300970415     C                   KFLD                    KINKCC                         *CODICE CAPOCONTO
396400970415     C                   KFLD                    KINKSC                         *CODICE CLIENTE
396500970110     C* LETTURA TNTAM01L - PARZIALE
396600970110     C     KEYTAM        KLIST
396700970110     C                   KFLD                    KTMKSC                         *CLIENTE
396800970110     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
396900021121     C* LETTURA TNTAM01L - PARZIALE
397000021121     C     KEYTAMUNIQ    KLIST
397100021121     C                   KFLD                    KTMKSC                         *CLIENTE
397200021121     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
397300021121     C                   KFLD                    KTMPRG                         *PROGRESSIVO
397400990820     C* LETTURA TITAD01L - PARZIALE
397500970110     C     KEYTAD        KLIST
397600970110     C                   KFLD                    KTDKSC                         *CLIENTE
397700970110     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
397800970110     C                   KFLD                    KTDPRG                         *PROGRESSIVO
397900970317     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
398000990812     C* LETTURA TITAS31C - PARZIALE
398100970110     C     K31TAS        KLIST
398200970110     C                   KFLD                    KTAKSC                         *CLIENTE
398300990812     C* LETTURA TITAS38C - COMPLETA
398400970110     C     K38TAS        KLIST
398500970110     C                   KFLD                    KTAAAS                         *ANNO
398600970110     C                   KFLD                    KTAMGS                         *MESE/GIORNO
398700970110     C                   KFLD                    KTAKSC                         *CLIENTE
398800970110     C* LETTURA TNCSB03L - COMPLETA
398900970110     C     KEYCSB        KLIST
399000970110     C                   KFLD                    KSBAAS                         *ANNO
399100970110     C                   KFLD                    KSBLNP                         *LINEA PARTENZA
399200970110     C                   KFLD                    KSBNRS                         *SERIE
399300970110     C                   KFLD                    KSBNSP                         *SPEDIZIONE
399400011001     C* LETTURA TEETC01L - COMPLETA
399500981218     C     KEYETC        KLIST
399600980713     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
399700980713     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
399800970110     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
399900970110     C                   KFLD                    KTCCTR                         *TARIFFA
400000970110     C                   KFLD                    KTCPRG                         *PROGRESSIVO
400100970110     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
400200011001     C* LETTURA TEETC01L - PARZIALE
400300981218     C     KE3ETC        KLIST
400400981218     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
400500981218     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
400600981218     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
400700011001     C* LETTURA TEETS01L - COMPLETA
400800970110     C     KEYETS        KLIST
400900980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
401000970110     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
401100970110     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
401200970110     C                   KFLD                    KTSCTR                         *TARIFFA
401300970110     C                   KFLD                    KTSPRG                         *PROGRESSIVO
401400970110     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
401500970417     C                   KFLD                    KTSSGL                         *SCAGLIONE
401600011001     C* LETTURA TEETS01L - PARZIALE
401700970314     C     KE2ETS        KLIST
401800980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
401900970314     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
402000970314     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
402100970314     C                   KFLD                    KTSCTR                         *TARIFFA
402200970314     C                   KFLD                    KTSPRG                         *PROGRESSIVO
402300970314     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
402400011001     C* LETTURA TEETS01L - PARZIALE
402500981218     C     KE3ETS        KLIST
402600981218     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
402700981218     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
402800981218     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
402900011001     C* LETTURA TEETT01L - COMPLETA
403000970110     C     KEYETT        KLIST
403100980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
403200970110     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
403300970110     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
403400970110     C                   KFLD                    KTTCTR                         *TARIFFA
403500970110     C                   KFLD                    KTTPRG                         *PROGRESSIVO
403600970110     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
403700970110     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
403800970110     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
403900011001     C* LETTURA TEETT01L - PARZIALE
404000970314     C     KE2ETT        KLIST
404100980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
404200970314     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
404300970314     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
404400970314     C                   KFLD                    KTTCTR                         *TARIFFA
404500970314     C                   KFLD                    KTTPRG                         *PROGRESSIVO
404600970314     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
404700970314     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
404800040809     C* LETTURA fiar531c
404900040809     C     Kar5          KLIST
405000040809     C                   KFLD                    ar5aas                         *DATI SEDE O FILIALE
405100040809     C                   KFLD                    ar5lnp                         *TARIFFA O OFFERTA
405200040809     C                   KFLD                    ar5nrs                         *CLIENTE O VISITA
405300040809     C                   KFLD                    ar5nsp                         *CLIENTE O VISITA
405400040809     C                   KFLD                    ar5trd                         *CLIENTE O VISITA
405500040809     C* LETTURA TEETT01L - PARZIALE
405600040809     C     KE3ETT        KLIST
405700040809     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
405800040809     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
405900040809     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
406000011011     C* LETTURA TNTBE01L - PARZIALE
406100011011     C     KEYTBE        KLIST
406200011011     C                   KFLD                    KTECOD                         *UTENTE
406300011011     C                   KFLD                    KTEKE1                         *CODICE
406400050309     C* LETTURA TIGCP51L - PARZIALE
406500021122     C     K01GCP        KLIST
406600021122     C                   KFLD                    KGPAAS                         *ANNO
406700021122     C                   KFLD                    KGPLNP                         *LINEA DI PARTENZA
406800021122     C                   KFLD                    KGPNRS                         *SERIE
406900021122     C                   KFLD                    KGPNSP                         *SPEDIZIONE
407000970110     C*--------------------
407100970110     C* VARIABILI DI CONTROLLO
407200970110     C*--------------------
407300970110     C                   MOVEL     '0'           $FINE             1            *FINE PGM
407400970110     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
407500970416     C                   MOVEL     'S'           $OK               1            *RECORD OK
407600970312     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO TROVATO
407700970313     C                   MOVEL     '0'           $ERR              1            *ERRORE
407800970313     C                   MOVEL     '1'           *INOF                          *OVERFLOW QSYSPRT
407900970313     C                   MOVEL     '1'           *IN01                          *OVERFLOW TNTE41P
408000970110     C*
408100970110     C* REPERISCE I DATI DELL'UTENTE
408200020923     C                   EXSR      REPDATIUTE
408300970110     C*
408400970110     C* REPERISCE LA DATA CORRENTE
408500970110     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
408600970110     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
408700970110     C                   Z-ADD     WN8           G08DAT
408800970110     C                   Z-ADD     *ZEROS        G08INV
408900970110     C                   MOVEL     '0'           G08ERR
409000970110     C                   CALL      'XSRDA8'
409100970110     C                   PARM                    WLBDA8
409200970110     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
409300970110     C*
409400970110     C* CARICA TABELLE OCCORRENTI
409500970110     C                   EXSR      CARTAB
409600970313     C*
409700970313     C* CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
409800970313     C                   EXSR      CARTAR
409900981218     C*
410000981218     C* ANNULLAMENTO TARIFFA
410100981218     C                   EXSR      CANTAR
410200021031     C*
410300021031     C* REPERIMENTO LISTA CLIENTI PADRI/FIGLI
410400021031     C                   EXSR      REPCLI
410500970110     C*
410600970110     C                   ENDSR
410700970110     O*------------------------------------------------------------------------*
410800970110     OQSYSPRT   E            TES              02
410900970110     O                       STA(1)              66
411000970110     O                       STA(2)             132
411100970110     O                       DSFIRS              20
411200970110     O                       UDATE              117 '  /  /  '
411300970110     O                                          127 'Pag.'
411400970110     O                       PAGE          Z    132
411500970110     O*------------------------
411600970110     O          E            ETAS        1
411700970110     O                       STA(3)              66
411800970110     O                       STA(4)             132
411900970423     O                       TASAAS        Z     16
412000970423     O                       TASLNP        Z     20
412100970423     O                       TASNRS        Z     23
412200970423     O                       TASNSP        Z     31
412300970423     O                       TASTBL              34
412400970423     O                       TASCTS              43
412500970110     O                       DEPERR             124
412600970110     O*------------------------
412700970110     O          E            EETX        1
412800970110     O                       STA(3)              66
412900970110     O                       STA(5)             132
413000970110     O                       PARKOV        Z     15
413100970110     O                       PARCTR              24
413200970110     O                       PARPRG              33
413300970110     O                       PARLNP        Z     53
413400970110     O                       PARFIE              62
413500970110     O                       DEPERR             124
413600970110     O*------------------------------------------------------------------------*
413700970110** -DDEC-
413800970110NON CALCOLARE                 1
413900970110CALCOLARE                     2
414000970110CALCOLARE IN REGIONE          3
414100970110CALCOLARE IN NAZIONE          4
414200970110 **  CREAZIONE DATI  TIPO  ** 5
414300980821TUTTA L'ITALIA                6
414400980821TUTTO L'ESTERO                7
4145009808218
4146009808219
41470098082110
414800990812** MTAS - MESSAGGI PER TITAS
414900970110FILIALE BOLLA NON TROVATA                                         1
415000970423CODICE TASSAZIONE NON TROVATO O NON VALIDO                        2
4151009704163
4152009701104
4153009701105
415400011001** METX - MESSAGGI PER TEETX
415500970110DATI TIPO ALLOCATI                                                1
415600971008NESSUNA SPEDIZIONE: VALORE DEDOTTO                                2
415700980901CREAZIONE AUTOMATICA                                              3
4158009701104
4159009701105
416000970110** STA
416100970110XXXXXXXXXXXXXXXXXXXX        TNTE41R       ** ERRORI IN CREAZIONE D1
416200970110ATI TIPO TARIFFE DA CONTO ECONOMICO **     99/99/99      PAG. 99992
416300970423SPEDIZIONE: 9999 999 99 9999999 XX  CTS: XX                       3
416400970423ERRORE: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        4
416500970110CLIENTE 9999999  TAR 999  PRG 999  LINEA PARTENZA 999   I/E  X    5
