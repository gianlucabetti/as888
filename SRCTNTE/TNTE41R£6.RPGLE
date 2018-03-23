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
009600970110     D*-------------------
009700970110     D* TIPI TARIFFA
009800970110     D*-------------------
009900970110     D TTR             S              1    DIM(10)                              *CODICE
010000970110     D DTTR            S             20    DIM(10)                              *DESCRIZIONE
010100970110     D*-------------------
010200970110     D* RAGGRUPPAMENTO TIPI BOLLA
010300970110     D*-------------------
010400970110     D RBL             S              1    DIM(5)                               *RAGGRUPPAMENTO BOLL
010500970110     D FSP             S              1    DIM(5)                               *FLAG SPEDIZIONE
010600970110     D FRI             S              1    DIM(5)                               *FLAG RICAVO
010700970110     D*------------------
010800970110     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA
010900970110     D*------------------
011000970110     D TBL             S              2    DIM(20)                              *TIP0 BOLLA
011100970110     D SPD             S              1    DIM(20)                              *FLAG SPEDIZIONE
011200970110     D RIC             S              1    DIM(20)                              *FLAG RICAVO
011300970110     D*------------------
011400970110     D* TABELLE CAPICONTO
011500970110     D*------------------
011600970110     D RI              S              4  0 DIM(4)                               *TAB RAG. CI
011700970110     D*------------------
011800970110     D* RATE DI PAGAMENTO
011900970110     D*------------------
012000970110     D RAT             S              3    DIM(10)
012100970110     D*------------------
012200970110     D* CODICI PAGAMENTO
012300970110     D*------------------
012400970110     D CDP             S              4    DIM(200)                             *CODICE PAGAMENTO
012500970110     D CRAT            S             30    DIM(200)                             *RATE DI PAGAMENTO
012600040920     D*------------------
012700040920     D* scaglioni standard
012800040920     D*------------------
012900040920     D sgl             S             13  3 DIM(10)                              *CODICI ELEMENTARI
013000970110     D*------------------
013100970110     D* CODICI TASSAZIONE
013200970110     D*------------------
013300050103     D CTS             S              2    DIM(600)                             *CODICI ELEMENTARI
013400050103     D FCR             S              1    DIM(600)                             *FLAG CAPOLUOGO
013500050103     D DCTS            S             20    DIM(600)                             *DESCRIZIONE
013600050103     D CIE             S              1    DIM(600)                             *FLAG ITALIA/ESTERO
013700050103     D RAP             S              2    DIM(600)                             *CODICE REGIONE
013800050103     D NAR             S              2    DIM(600)                             *CODICE NAZIONE -> P
013900050103     D TCT             S              1    DIM(600)                             *TIPO (E/R/N)   -> P
014000050808     D*------------------
014100050808     D* cod. trattamento merce con packing list
014200050808     D*------------------
014300050808     D Cpkl            S              2    DIM(600)                             *CODICI ELEMENTARI
014400970314     D*------------------
014500970314     D* DATI TARIFFE
014600970314     D*------------------
014700971124     D* MULTIPLE                                         -500 FILIALI-
014800970110     D*-------------------
014900970110     D* SCHIERE DI MEMORIZZAZIONE
015000970110     D*-------------------
015100970110     D* NON MULTIPLE
015200060315     D SVN             S              1    DIM(20)
015300060315     D VAN             S             11  3 DIM(20)
015400971124     D SPE             S              9  0 DIM(500)                             *N°SPEDIZIONI ITA/ES
015500971124     D CLA             S              9  0 DIM(500)                             *CLIENTE TIPO A
015600971124     D CLB             S              9  0 DIM(500)                             *CLIENTE TIPO B
015700971124     D CLC             S              9  0 DIM(500)                             *CLIENTE TIPO C
015800971124     D BON             S              9  0 DIM(500)                             *BOLLETTAZIONE (' ')
015900971124     D BOA             S              9  0 DIM(500)                             *BOLLETTAZIONE (A)
016000971124     D BOB             S              9  0 DIM(500)                             *BOLLETTAZIONE (B)
016100971124     D NCL             S              9  0 DIM(500)                             *COLLI
016200971124     D PKB             S              9  1 DIM(500)                             *PESO
016300971124     D VLB             S              9  3 DIM(500)                             *VOLUME
016400060829     D NCL1            S              9  0 DIM(500)                             *COLLI
016500060829     D PKB1            S              9  1 DIM(500)                             *PESO
016600060829     D VLB1            S              9  3 DIM(500)                             *VOLUME
016700971124     D FRA             S              9  0 DIM(500)                             *FRANCHI
016800971124     D ASS             S              9  0 DIM(500)                             *ASSEGNATI
016900971124     D PCV             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
017000971124     D PCM             S              9  0 DIM(500)                             *CONSEGNE MAGAZZINO
017100971124     D PFD             S              9  0 DIM(500)                             *FERMO DEPOSITO
017200971124     D ESP             S              9  0 DIM(500)                             *ESPRESSI
017300971124     D PPV             S              9  0 DIM(500)                             *CONSEGNE PROVINCIA
017400971124     D PCA             S              9  0 DIM(500)                             *CONSEGNE APPUNTAMEN
017500971124     D PCP             S              9  0 DIM(500)                             *CONSEGNE PIANI
017600971124     D PCS             S              9  0 DIM(500)                             *CONSEGNE SUPERMERCA
017700971124     D PCF             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
017800971124     D PAB             S              9  0 DIM(500)                             *C/ASSEGNI BARTOLINI
017900971124     D PAM             S              9  0 DIM(500)                             *C/ASSEGNI MITTENTE
018000011026     D CAS             S             21  8 DIM(500)                             *C/ASSEGNO
018100971124     D PAR             S              9  0 DIM(500)                             *ASSICURAZIONE R.C.V
018200971124     D PAC             S              9  0 DIM(500)                             *ASSICURAZIONE X CON
018300060315     D xx1             S              9  0 DIM(500)                             *ASSICURAZIONE X CON
018400011026     D ASC             S             21  8 DIM(500)                             *ASSICURAZIONE
018500971124     D PGI             S              9  0 DIM(500)                             *GIACENZE
018600971124     D PCD             S              9  0 DIM(500)                             *CONSEGNE DDT
018700971124     D PCC             S              9  0 DIM(500)                             *CONSEGNE C.STORICI
018800971124     D PCL             S              9  0 DIM(500)                             *CONSEGNE L.DISAGIAT
018900980901     D CLT             S              9  0 DIM(500)                             *CLIENTE TIPO T
019000000414     D GGS             S              9  0 DIM(500)                             *N° GG SOSTA GIACENZ
019100000414     D NRG             S              9  0 DIM(500)                             *N° RIAPERTURE GIACE
019200011011     D CIS             S              9  0 DIM(500)                             *CONSEGNE ISOLE
019300011011     D RIS             S              9  0 DIM(500)                             *RITIRI ISOLE
019400020215     D CLD             S              9  0 DIM(500)                             *CLIENTE TIPO D
019500040802     D ded             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
019600040802     D val             S              9  0 DIM(500)                             *CONSEGNE FUORI MISU
019700040811     D sbnb            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
019800040824     D sban            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
019801150302     D sb01            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
019802150302     D sb02            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
019803150302     D sb03            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
019900040811     D nbn             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020000040802     D nba             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020001150302     D nb01            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020002150302     D nb02            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020003150302     D nb03            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020100050808     D pks             S              9  0 DIM(500)                             *CLIENTE TIPO D
020200050808     D pkn             S              9  0 DIM(500)                             *CLIENTE TIPO D
020300150304     d detcflo       e ds
020301150304     d dar5ban       e ds
020400040811     d dar5bnb       e ds
020500060829     d teets1        e ds                  extname(teets10f) prefix(£)
020600970417     D*-------------------
020700970417     D* SCHIERE DI LAVORO
020800970417     D*-------------------
020900050103     D WCTS            S              2    DIM(600)                             *CODICI
021000050103     D WRAP            S              2    DIM(600)                             *CODICE REGIONE
021100050103     D WNAR            S              2    DIM(600)                             *CODICE NAZIONE -> P
021200050103     D WTCT            S              1    DIM(600)                             *TIPO (E/R/N)   -> P
021300970416     D*-------------------
021400970416     D* SCHIERE DI PASSAGGIO PER LA SCRITTURA
021500970416     D*-------------------
021600970416     D* NON MULTIPLE
021700970416     D §SG             S             13  3 DIM(10)                              *SCAGLIONE -KG/COLLI
021800060829     c* ricavi
021900970416     D §PS             S              7  1 DIM(10)                              *PESO
022000970416     D §CS             S              7  1 DIM(10)                              *COLLI
022100970416     D §VS             S              7  3 DIM(10)                              *VOLUME
022200970416     D §SP             S              4  1 DIM(10)                              *% INCIDENZA
022300971008     D §SD             S              1    DIM(10)                              *FLAG VALORE DEDOTTO
022400060829     c* competenze
022500060829     D §PS1            S              7  1 DIM(10)                              *PESO
022600060829     D §CS1            S              7  1 DIM(10)                              *COLLI
022700060829     D §VS1            S              7  3 DIM(10)                              *VOLUME
022800060829     D §SP1            S              4  1 DIM(10)                              *% INCIDENZA
022900060829     D §SD1            S              1    DIM(10)                              *FLAG VALORE DEDOTTO
023000970416     D*---
023100050103     D §CT             S              2    DIM(600)                             *CODICE TASSAZIONE
023200050103     D §RC             S              1    DIM(600)                             *RAGGRUPPAMENTO
023300050103     D §CP             S              4  1 DIM(600)                             *% INCIDENZA
023400050103     D §CD             S              1    DIM(600)                             *FLAG VALORE DEDOTTO
023500020227     D*-------------------
023600020227     D* SCHIERA FILIALI FEDEX
023700020227     D*-------------------
023800020227     D FILFED          S              3  0 DIM(999)
023900081105     D*-------------------
024000081105     D* SCHIERA FILIALI DPD
024100081105     D*-------------------
024200081105     D FILdpd          S              3  0 DIM(999)
024300970110     D*------------------------------------------------------------------------*
024400970110     D* INPUT
024500970110     D*------------------------------------------------------------------------*
024600970110     D*------------------
024700970110     D* DS SCHIERE MULTIPLE
024800970110     D*------------------
024900970110     D*---
025000970110     D* BYTE IN PACKED ->
025100970317     D* (3:2) +1=2 ; (2X30) =60
025200970317     D* (9:2) +1=5 ; (5X30) =150
025300970317     D* (9:2) +1=5 ; (5X999)=4995
025400970317     D* (9:2) +1=5 ; (5X10) =50
025500050103     D* (9:2) +1=5 ; (5X600)=1500
025600970317     D* (13:2)+1=7 ; (7X10) =70
025700011001     D* (18:2)+1=10; (10X10) =100
025800980820     D*---
025900971124     D FPAG            DS                  OCCURS(500) INZ
026000970110     D  GGP                    1     60P 0
026100970110     D                                     DIM(30)                              *GG PAGAMENTO
026200970110     D  SGP                   61    210P 0
026300970110     D                                     DIM(30)                              *SPED. GG PAGAMENTO
026400971124     D FRIT            DS                  OCCURS(500) INZ
026500970110     D  RIT                    1   4995P 0
026600970110     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
026700971124     D FCON            DS                  OCCURS(500) INZ
026800970110     D  CON                    1   4995P 0
026900970110     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
027000971124     D FSGL            DS                  OCCURS(500) INZ
027100041118     D  SSP                    1     50P 0
027200041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
027300041118     D  PSC                   51    110P 1
027400041118     D                                     DIM(10)                              *PESO
027500041118     D  CSC                  111    160P 0
027600041118     D                                     DIM(10)                              *COLLI
027700041118     D  VSC                  161    220P 3
027800041118     D                                     DIM(10)                              *VOLUME
027900060829     D FSGL1           DS                  OCCURS(500) INZ
028000060829     D  SSP1                   1     50P 0
028100060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
028200060829     D  PSC1                  51    110P 1
028300060829     D                                     DIM(10)                              *PESO
028400060829     D  CSC1                 111    160P 0
028500060829     D                                     DIM(10)                              *COLLI
028600060829     D  VSC1                 161    220P 3
028700060829     D                                     DIM(10)                              *VOLUME
028800971124     D FCTS            DS                  OCCURS(500) INZ
028900050103     D  TSI                    1   3000P 0
029000050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
029100050103     D  TSE                 3001   6000P 0
029200050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
029300971124     D FTAR            DS                  OCCURS(500) INZ
029400970423     D  SSGL                   1     70P 3
029500970314     D                                     DIM(10)                              *SCAGLIONI
029600050103     D  CCTI                  71   1270
029700050103     D                                     DIM(600)                             *COD.TASSAZIONE ITAL
029800050103     D  CRCI                1271   1870
029900050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ITAL
030000050103     D  CCTE                1871   3070
030100050103     D                                     DIM(600)                             *COD.TASSAZIONE ESTE
030200050103     D  CRCE                3071   3670
030300050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ESTE
030400980820     D*---
030500980820     D* CARTELLO
030600980820     D*---
030700980824     D CCAR            DS                  OCCURS(2) INZ
030800980821     D  USPE                   1      5P 0
030900980821     D                                     DIM(1)                               *N°SPEDIZIONI ITA/ES
031000980821     D  UCLA                   6     10P 0
031100980821     D                                     DIM(1)                               *CLIENTE TIPO A
031200980821     D  UCLB                  11     15P 0
031300980821     D                                     DIM(1)                               *CLIENTE TIPO B
031400980821     D  UCLC                  16     20P 0
031500980821     D                                     DIM(1)                               *CLIENTE TIPO C
031600980821     D  UBON                  21     25P 0
031700980821     D                                     DIM(1)                               *BOLLETTAZIONE (' ')
031800980821     D  UBOA                  26     30P 0
031900980821     D                                     DIM(1)                               *BOLLETTAZIONE (A)
032000980821     D  UBOB                  31     35P 0
032100980821     D                                     DIM(1)                               *BOLLETTAZIONE (B)
032200980821     D  UNCL                  36     40P 0
032300980821     D                                     DIM(1)                               *COLLI
032400980821     D  UPKB                  41     45P 1
032500980821     D                                     DIM(1)                               *PESO
032600980821     D  UVLB                  46     50P 3
032700980821     D                                     DIM(1)                               *VOLUME
032800980821     D  UFRA                  51     55P 0
032900980821     D                                     DIM(1)                               *FRANCHI
033000980821     D  UASS                  56     60P 0
033100980821     D                                     DIM(1)                               *ASSEGNATI
033200980821     D  UPCV                  61     65P 0
033300980821     D                                     DIM(1)                               *CONSEGNE PRIVATI
033400980821     D  UPCM                  66     70P 0
033500980821     D                                     DIM(1)                               *CONSEGNE MAGAZZINO
033600980821     D  UPFD                  71     75P 0
033700980821     D                                     DIM(1)                               *FERMO DEPOSITO
033800980821     D  UESP                  76     80P 0
033900980821     D                                     DIM(1)                               *ESPRESSI
034000980821     D  UPPV                  81     85P 0
034100980821     D                                     DIM(1)                               *CONSEGNE PROVINCIA
034200980821     D  UPCA                  86     90P 0
034300980821     D                                     DIM(1)                               *CONSEGNE APPUNTAMEN
034400980821     D  UPCP                  91     95P 0
034500980821     D                                     DIM(1)                               *CONSEGNE PIANI
034600980821     D  UPCS                  96    100P 0
034700980821     D                                     DIM(1)                               *CONSEGNE SUPERMERCA
034800980821     D  UPCF                 101    105P 0
034900980821     D                                     DIM(1)                               *CONSEGNE FUORI MISU
035000980821     D  UPAB                 106    110P 0
035100980821     D                                     DIM(1)                               *C/ASSEGNI BARTOLINI
035200980821     D  UPAM                 111    115P 0
035300980821     D                                     DIM(1)                               *C/ASSEGNI MITTENTE
035400011002     D  UCAS                 116    125P 5
035500011002     D                                     DIM(1)                               *C/ASSEGNO
035600011002     D                                     PACKEVEN
035700011002     D  UPAR                 126    130P 0
035800980821     D                                     DIM(1)                               *ASSICURAZIONE R.C.V
035900011002     D  UPAC                 131    135P 0
036000980821     D                                     DIM(1)                               *ASSICURAZIONE X CON
036100011002     D  UASC                 136    145P 5
036200011001     D                                     DIM(1)                               *ASSICURAZIONE
036300011001     D                                     PACKEVEN
036400011002     D  UPGI                 146    150P 0
036500980821     D                                     DIM(1)                               *GIACENZE
036600011002     D  UPCD                 151    155P 0
036700980821     D                                     DIM(1)                               *CONSEGNE DDT
036800011002     D  UPCC                 156    160P 0
036900980821     D                                     DIM(1)                               *CONSEGNE C.STORICI
037000011002     D  UPCL                 161    165P 0
037100980821     D                                     DIM(1)                               *CONSEGNE L.DISAGIAT
037200011002     D  UCLT                 166    170P 0
037300980901     D                                     DIM(1)                               *CLIENTE TIPO T
037400011002     D  UGGS                 171    175P 0
037500000414     D                                     DIM(1)                               *GG SOSTA GIACENZA
037600011002     D  UNRG                 176    180P 0
037700000414     D                                     DIM(1)                               *N° RIAPERTURE GIACE
037800011011     D  UCIS                 181    185P 0
037900011011     D                                     DIM(1)                               *CONSEGNE ISOLE
038000011011     D  URIS                 186    190P 0
038100011011     D                                     DIM(1)                               *RITIRI ISOLE
038200020215     D  UCLD                 191    195P 0
038300020215     D                                     DIM(1)                               *CLIENTE TIPO D
038400040802     D  Uded                 196    200P 0
038500040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
038600040811     D  Usbnb                206    210P 0
038700040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
038800040824     D  Usban                211    215P 0
038900040824     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039000040824     D  Unbn                 216    220P 0
039100040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039200040824     D  Unba                 221    225P 0
039300040811     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039400040824     D  Uval                 226    230P 0
039500040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039600050808     D  Upks                 231    235P 0
039700050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039800050808     D  Upkn                 236    240P 0
039900050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040000060315     D  Uxx1                 241    245P 0
040100060315     D                                     DIM(1)                               *ASSICURAZIONE X CON
040200060829     D  UNCL1                246    250P 0
040300060829     D                                     DIM(1)                               *COLLI
040400060829     D  UPKB1                251    255P 1
040500060829     D                                     DIM(1)                               *PESO
040600060829     D  UVLB1                256    260P 3
040700060829     D                                     DIM(1)                               *VOLUME
040701150302     D  Usb01                261    265P 0
040702150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040703150304     D  Usb02                266    270P 0
040704150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040705150304     D  Usb03                271    275P 0
040706150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040707150302     D  Unb01                276    280P 0
040708150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040709150302     D  Unb02                281    285P 0
040710150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040711150302     D  Unb03                286    290P 0
040712150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040800980824     D CPAG            DS                  OCCURS(2) INZ
040900980820     D  UGGP                   1     60P 0
041000980819     D                                     DIM(30)                              *GG PAGAMENTO
041100980820     D  USGP                  61    210P 0
041200980819     D                                     DIM(30)                              *SPED. GG PAGAMENTO
041300980824     D CRIT            DS                  OCCURS(2) INZ
041400980820     D  URIT                   1   4995P 0
041500980819     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
041600980824     D CCON            DS                  OCCURS(2) INZ
041700980820     D  UCON                   1   4995P 0
041800980819     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
041900980824     D CSGL            DS                  OCCURS(2) INZ
042000041118     D  USSP                   1     50P 0
042100041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
042200041118     D  UPSC                  51    110P 1
042300041118     D                                     DIM(10)                              *PESO
042400041118     D  UCSC                 111    160P 0
042500041118     D                                     DIM(10)                              *COLLI
042600041118     D  UVSC                 161    220P 3
042700041118     D                                     DIM(10)                              *VOLUME
042800060829     D CSGL1           DS                  OCCURS(2) INZ
042900060829     D  USSP1                  1     50P 0
043000060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
043100060829     D  UPSC1                 51    110P 1
043200060829     D                                     DIM(10)                              *PESO
043300060829     D  UCSC1                111    160P 0
043400060829     D                                     DIM(10)                              *COLLI
043500060829     D  UVSC1                161    220P 3
043600060829     D                                     DIM(10)                              *VOLUME
043700980824     D CCTS            DS                  OCCURS(2) INZ
043800050103     D  UTSI                   1   3000P 0
043900050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
044000050103     D  UTSE                3001   6000P 0
044100050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
044200040803     D Dtasflo       E DS
044300091125     D tnte04ds      E DS
044400970110     D*-------------------
044500970110     D* DS TIPI TARIFFA
044600970110     D*-------------------
044700970110     D DSTR          E DS
044800050808     D*-------------------
044900050808     D* DS cod. trattamento merce
045000050808     D*-------------------
045100050808     D DS1b          E DS
045200021202     D*-------------------
045300021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
045400021202     D*-------------------
045500021202     D TRUL27DS      E DS
045600970110     D*-------------------
045700970110     D* DS CODICI TASSAZIONE
045800970110     D*-------------------
045900970110     D DSCT          E DS
046000970110     D*-------------------
046100970110     D* DS CAMBI E DIVISE
046200970110     D*-------------------
046300011011     D*DSCV          E DS
046400970110     D*-------------------
046500970110     D* DS RAGGRUPPAMENTI TIPI BOLLA
046600970110     D*-------------------
046700970110     D DS2W          E DS
046800970110     D*------------------
046900970110     D* DS TIPI BOLLA
047000970110     D*------------------
047100970110     D DSTB          E DS
047200970110     D*------------------
047300970110     D* DS CODICI PAGAMENTO
047400970110     D*------------------
047500970110     D DSFA          E DS
047600970110     D*------------------
047700970110     D* DS COMPOSIZIONE CODICE DI PAGAMENTO
047800970110     D*------------------
047900970110     D                 DS
048000970110     D  WCDP                   1      3
048100970110     D  WLIN                   4      4
048200970110     D  DSCDP                  1      4
048300970110     D*------------------
048400970110     D* DS INVIO DATI -BOLLETTAZIONE-
048500970110     D*------------------
048600970110     D DS3C          E DS
048700970110     D*------------------
048800020923     D* DS REPERIMENTO DATI UTENTE
048900970110     D*------------------
049000020923     D TIBS34DS      E DS                                                       *Profili utente
049100020923     D DDATIUTE      E DS                                                       *Dati utente
049200020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
049300970110     D*------------------
049400970110     D* DS "XSRDA8" - CONTROLLA DATA (8)
049500970110     D*------------------
049600970110     D WLBDA8          DS
049700970110     D  G08DAT                 1      8  0
049800970110     D  G08INV                 9     16  0
049900970110     D  G08ERR                17     17
050000970110     D  G08TGI                18     22  0
050100970110     D*-------------------
050200970110     D* DS SCOMPOSIZIONE DATA DECORRENZA(4) -TNTAM-
050300970110     D*-------------------
050400970110     D                 DS
050500970110     D  TAMDNN                 3      4  0
050600970110     D  TAMDMM                 5      6  0
050700970110     D  TAMDEC                 3      6  0
050800970110     D  TAMDDT                 1      8  0
050900970110     D*-------------------
051000970110     D* DS SCOMPOSIZIONE DATA SCADENZA(4) -TNTAM-
051100970110     D*-------------------
051200970110     D                 DS
051300970110     D  TAMSNN                 3      4  0
051400970110     D  TAMSMM                 5      6  0
051500970110     D  TAMSCA                 3      6  0
051600970110     D  TAMDST                 1      8  0
051700970110     D*-------------------
051800990812     D* DS COMPOSIZIONE DATA SPEDIZIONE -TITAS-
051900970110     D*-------------------
052000970110     D                 DS
052100970110     D  TASAAS                 1      4  0
052200970110     D  TASMGS                 5      8  0
052300970110     D  TASDSP                 1      8  0
052400970110     D*------------------
052500970110     D* DS PARAMETRI DI LANCIO
052600970110     D*------------------
052700970110     D PARAM           DS
052800980825     D  PARKSC                 1      7  0
052900980825     D  PARFPA                 8     10  0
053000980825     D  PARFAR                11     13  0
053100980825     D  PARFAP                14     16  0
053200980825     D  PARDS1                17     24  0
053300980825     D  PARAA1                17     20  0
053400980825     D  PARMG1                21     24  0
053500980825     D  PARDS2                25     32  0
053600980825     D  PARTAR                33     35
053700980825     D  PARCTO                36     36
053800980825     D  PARKOV                37     43  0
053900980825     D  PARCAR                37     41  0
054000980825     D  PARCTR                44     46  0
054100980825     D  PARTTR                44     44
054200980825     D  PARPRG                47     49  0
054300980825     D  PARLNP                50     52  0
054400980825     D  PARFIE                53     53
054500980825     D  PARSIM                54     54
054600980825     D  PARELA                55     55
054700011003     D  PARDIV                56     58
054800021031     D  PARUNI                59     59
054900040804     D  PARsta                60     60
055000040805     D  PARint                61     61
055100040805     D  PARsie                62     62
055200091130     D  PARdsf                63     63
055300970110     D*------------------
055400970110     D* ARCHITETTURA
055500970110     D*------------------
055600970110     D KPJBA         E DS
055700970110     D  PROFIL                 1     10
055800011011     D*------------------
055900011011     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
056000011011     D*------------------
056100011011     D DSGEDCE       E DS
056200011011     D*-------------------
056300011011     D* CONVERSIONI IMPORTI
056400011011     D*-------------------
056500011011     D YEUR          E DS                  EXTNAME(YEURCODS)
056600020227     D*-------------------
056700020227     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
056800020227     D*-------------------
056900020227     D OG143         E DS
057000020730     D*------------------
057100020730     D* DS PER LETTURA TABELLA "NTW"
057200020730     D*------------------
057300020730     D DNTW          E DS
057400021121     D*------------------
057500021121     D* DS RIDEFINIZIONE CAMPO TAMFLO DI TNTAM
057600021121     D*------------------
057700021121     D DSTA01        E DS
057800021031     D*-------------------
057900021031     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
058000021031     D*-------------------
058100021031     D TIBS10DS      E DS
058200021031     D  SKCLI                 21   5520  0 DIM(500)                              -cli. padri/figli
058300040811     D*-------------------
058400040811     D* CAMPI DI LAVORO
058500040811     D*-------------------
058600060829     d colli           s                   like(tasncl)
058700060829     d colli1          s                   like(tasncl)
058800040811     d $pes            s                   like(taspkf)
058900060829     d $pes1           s                   like(taspkf)
059000060829     d etcpmc1         s                   like(etcpmc)
059100060829     d etcvmc1         s                   like(etcvmc)
059200970110     C*------------------------------------------------------------------------*
059300970110     C* MAIN LINE
059400970110     C*------------------------------------------------------------------------*
059500970313     C*
059600970313     C* SE NESSUN ERRORE IN APERTURA
059700970313IF  1C     $FINE         IFEQ      '0'
059800970313     C*
059900970314     C* ANNULLA I DATI TIPO ESISTENTI
060000970314     C                   EXSR      ANNETX
060100970110     C* APRE GLI ARCHIVI
060200970110     C                   EXSR      OPEFIL
060300970415     C*
060400970416     C* LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
060500970416     C                   EXSR      LETCLI
060600970416     C*
060700970416     C* REPERISCE LA QUALITA' DEL CLIENTE TARIFFA / OFFERTA
060800970416     C                   MOVEL     'C'           $CLV              1
060900970416IF  1C     CLPCLV        IFNE      *BLANKS
061000970416     C                   MOVEL     CLPCLV        $CLV
061100970416E   1C                   ENDIF
061200970416     C*
061300970416     C* REPERISCE I GIORNI DI PAGAMENTO DEL CLIENTE / VISITA
061400970416     C                   Z-ADD     *ZEROS        $GGP              3 0
061500970416     C                   EXSR      REPGGP
061600021129     C*
061700021129     C* CICLO SULLA SCHIERA DEI CLIENTI DA ELABORARE (DISCORSO UNIFICANTE)
061800021129     C     1             DO        500           JJ                3 0
061900021129     C                   Z-ADD     SKCLI(JJ)     KTAKSC                         *CLIENTE
062000970110     C*
062100970110     C* POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
062200970110     C                   EXSR      SETFIL
062300970110     C*
062400970110     C* CICLO FINO A FINE -BOLLE-
062500970313DO  2C     $FINE         DOWEQ     '0'
062600970110     C*
062700970110     C* MEMORIZZA I DATI
062800970110     C                   EXSR      MEMDAT
062900970110     C*
063000970110     C* LETTURA SUCESSIVA -BOLLE-
063100970110     C                   EXSR      LETFIL
063200970313E   2C                   ENDDO                                                  *FINE BOLLE
063300021129     C*
063400021129     C* DOPO L'ELABORAZIONE DI OGNI CLIENTE RE-INIZIALIZZO IL FLAG DI "FINE"
063500021129     C                   MOVEL     '0'           $FINE
063600021129     C*
063700021129     C* TESTO LE CONDIZIONI X CUI USCIRE DAL CICLO DI MEMORIZZAZIONE
063800021129     C                   IF        SKCLI(JJ) = *ZEROS
063900021129     C                   MOVEL     '1'           $FINE
064000021129     C                   LEAVE
064100021129     C                   ENDIF
064200021129     C*
064300021129     C                   ENDDO
064400970110     C*
064500970110     C* SCRIVE -SE RICHIESTO- E STAMPA I DATI TIPO
064600970110     C                   EXSR      AGGETX
064700980818     C*
064800980818     C* SCRIVE E STAMPA I DATI TIPO DI CARTELLO -SE RICHIESTE TUTTE LE LINE DI PARTENZA-
064900980818IF  2C     PARCTO        IFEQ      'T'
065000980818     C     PARCAR        ANDEQ     88888
065100980818     C     PARLNP        ANDEQ     999
065200980818     C                   EXSR      AGGET8
065300980818E   2C                   ENDIF
065400970110     C*
065500970110     C* CHIUDE GLI ARCHIVI
065600970110     C                   EXSR      CLOFIL
065700970313E   1C                   ENDIF
065800040804     c* lancio la stampa se richiesto
065900040804     c                   if        parsta = 'S'
066000040804     c                   call      'TNTE49R'
066100040804     c                   parm                    kpjba
066200040804     c                   end
066300970110     C*
066400970110     C                   SETON                                        LR
066500970415     C*------------------------------------------------------------------------*
066600970416     C* LETCLI - LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
066700970415     C*------------------------------------------------------------------------*
066800970416     C     LETCLI        BEGSR
066900970415     C*
067000060808     C                   CLEAR                   CNCLP000                       *AZZERA RECORD
067100060808     C                   CLEAR                   CNIND000                       *AZZERA RECORD
067200060808     C                   CLEAR                   tfCLP000                       *AZZERA RECORD
067300060808     C                   CLEAR                   tfIND000                       *AZZERA RECORD
067400020923     C                   Z-ADD     1             KCLKUT                         *PARTICOLARITA'
067500020923     C                   Z-ADD     DUTKCI        KCLKCC
067600970415     C                   Z-ADD     PARKOV        KCLKSC
067700020923     C                   Z-ADD     1             KINKUT                         *INDIRIZZO
067800020923     C                   Z-ADD     DUTKCI        KINKCC
067900970415     C                   Z-ADD     PARKOV        KINKSC
068000060808IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
068100060808     C     KEYCLP        CHAIN     CNCLP00F                           99
068200060808     C     KEYIND        CHAIN     CNIND00F                           99
068300060808X   1C                   ELSE                                                   *OFFERTA
068400060804     C     KEYCLP        CHAIN     TFCLP00F                           99
068500060804     C     KEYIND        CHAIN     TFIND00F                           99
068600060804IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATO
068700091125     c                   clear                   tnte04ds
068800091130     C                   movel     pardsf        d04dsf
068900091125     C                   Z-ADD     parkov        d04nrv
069000091125     c                   call      'TNTE04R'
069100091125     c                   parm                    kpjba
069200091126     c                   parm                    tnte04ds
069300091125     C                   Z-ADD     d04KSC        KCLKSC                         *CLIENTE DELLA VISIT
069400091125     C                   Z-ADD     d04KSC        KINKSC                         *CLIENTE DELLA VISIT
069500060808     C     KEYCLP        CHAIN     CNCLP00F                           99
069600060808     C     KEYIND        CHAIN     CNIND00F                           99
069700060804     c                   end
069800060808     c                   end
069900970415     C*
070000970415     C                   ENDSR
070100970415     C*------------------------------------------------------------------------*
070200970415     C* REPGGP - REPERISCE IL NUMERO DEI GIORNI DI PAGAMENTO
070300970415     C*------------------------------------------------------------------------*
070400970415     C     REPGGP        BEGSR
070500970415     C*---
070600970415     C* IL GG DI PAGAMENTO IN TABELLA POSSONO ESSERE ESPRESSI IN MESI SE <=12 O
070700970415     C* IN GIORNI SE >12;
070800970415     C* CI POSSONO ESSERE INOLTRE + RATE DI PAGAMENTO E IN QUESTO CASO PRENDE
070900970415     C* SEMPRE L'ULTIMA (ES. SE 30/60/90 PRENDE 90 COME GGP).
071000970415     C* IL PAGAMENTO A VISTA NON HA GIORNI, MA PER COMODITA' SI MEMORIZZA CON
071100970415     C* CON GIORNI DI PAGAMENTO = 999
071200970415     C*---
071300970415     C                   MOVE      INDCDP        WCDP                           *CONDIZIONE PAGAMENT
071400000323     C                   MOVEL     '1'           WLIN                           *LINGUA
071500970415     C                   SETOFF                                           98
071600970415     C                   Z-ADD     1             I
071700970415     C     DSCDP         LOOKUP    CDP(I)                                 98
071800970415IF  1C     *IN98         IFEQ      *ON                                          *TROVATO
071900970415     C                   MOVEA     CRAT(I)       RAT
072000970415     C                   Z-ADD     10            I
072100970415DO  2C     I             DOWGT     0
072200970415IF  3C     RAT(I)        IFGT      *ZEROS                                       *P I E N O
072300970415     C                   MOVEL     RAT(I)        WN3               3 0
072400970415IF  4C     WN3           IFGT      12                                           *ESPRESSO IN GIORNI
072500970415     C                   Z-ADD     WN3           $GGP
072600970415X   4C                   ELSE                                                   *ESPRESSO IN MESI
072700970415     C     WN3           MULT      30            $GGP
072800970415E   4C                   ENDIF
072900981020     C                   LEAVE                                                  *ESCE DAL CICLO
073000970415X   3C                   ELSE                                                   *V U O T O
073100970415     C     I             SUB       1             I                              *DECREMENTA INDICE
073200970415E   3C                   ENDIF
073300970415E   2C                   ENDDO
073400970415E   1C                   ENDIF
073500970415     C*
073600970415     C* PAGAMENTO A VISTA (GGP=0), IMPOSTA GGP=999
073700970415IF  1C     $GGP          IFEQ      *ZEROS
073800970415     C                   Z-ADD     999           $GGP
073900970415E   1C                   ENDIF
074000970415     C*
074100970415     C                   ENDSR
074200970110     C*------------------------------------------------------------------------*
074300970110     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
074400970110     C*------------------------------------------------------------------------*
074500970110     C     SETFIL        BEGSR
074600970110     C*
074700021105IF  1C     PARKSC        IFGT      *ZEROS
074800990812     C     K31TAS        SETLL     TITAS31C                           99        *FINE ARCHIVIO
074900970110X   1C                   ELSE
075000970110     C                   Z-ADD     PARAA1        KTAAAS                         *ANNO
075100970110     C                   Z-ADD     PARMG1        KTAMGS                         *MESE/GIORNO
075200990812     C     K38TAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
075300970110E   1C                   ENDIF
075400970110     C*
075500970110IF  1C     *IN99         IFEQ      *ON
075600970110     C                   MOVEL     '1'           $FINE                          *FINE PGM
075700970110X   1C                   ELSE
075800970110     C                   EXSR      LETFIL                                       *LETTURA RECORD
075900970110E   1C                   ENDIF
076000970110     C*
076100970110     C                   ENDSR
076200970110     C*------------------------------------------------------------------------*
076300970110     C* LETFIL - LETTURA PROSSIMO RECORD -BOLLE-
076400970110     C*------------------------------------------------------------------------*
076500970110     C     LETFIL        BEGSR
076600970110     C*
076700970110     C* LETTURA FINO A:
076800970110     C*  - FINE FILE
076900970110     C*  - TROVATO RECORD VALIDO
077000970110     C                   MOVEL     'N'           WRECOK
077100970110DO  1C     $FINE         DOWEQ     '0'
077200970110     C     WRECOK        ANDEQ     'N'
077300970110     C*
077400970110IF  2C     PARKSC        IFGT      *ZEROS
077500990812     C                   READ      TITAS31C                               99
077600970110X   2C                   ELSE
077700990812     C                   READ      TITAS38C                               99
077800970110E   2C                   ENDIF
077900970110     C*
078000970110IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
078100970110     C                   MOVEL     '1'           $FINE
078200970110X   2C                   ELSE
078300970110     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
078400970110E   2C                   ENDIF
078500970110E   1C                   ENDDO
078600970110     C*
078700970110     C                   ENDSR
078800970110     C*------------------------------------------------------------------------*
078900970110     C* CHKREC - CONTROLLA VALIDITA' RECORD -BOLLE-
079000970110     C*------------------------------------------------------------------------*
079100970110     C     CHKREC        BEGSR
079200970110     C*
079300970110     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
079400081105     C*
079500081105     C* se sto elaborando la cartello i dati tipo della cartello escludo le
079600081105     c* le spedizioni con lnp e lna DPD e FED
079700081105IF  2C     PARCTO        IFEQ      'T'
079800081105     C     PARCAR        ANDEQ     88888
079900081105     C     PARLNP        ANDEQ     999
080000081105     c* fedex
080100081105     C     taslnp        LOOKUP    FILFED                                 55
080200081105     C                   IF        *IN55 = *ON
080300081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
080400081105     C                   ENDIF
080500081105     C     taslna        LOOKUP    FILFED                                 55
080600081105     C                   IF        *IN55 = *ON
080700081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
080800081105     C                   ENDIF
080900081105     c* dpd
081000081105     C     taslnp        LOOKUP    FILdpd                                 55
081100081105     C                   IF        *IN55 = *ON
081200081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
081300081105     C                   ENDIF
081400081105     C     taslna        LOOKUP    FILdpd                                 55
081500081105     C                   IF        *IN55 = *ON
081600081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
081700081105     C                   ENDIF
081800081105E   1C                   ENDIF
081900970423     C*
082000970423     C* TIPO BOLLA NON VALIDO COME SPEDIZIONE O RICAVO, ESCLUDE
082100970416     C                   Z-ADD     1             I
082200970416     C                   SETOFF                                           98
082300970416     C     TASTBL        LOOKUP    TBL(I)                                 98
082400970416IF  1C     *IN98         IFEQ      *ON                                          *TIPO BOLLA TROVATO
082500970416IF  2C     SPD(I)        IFEQ      'S'
082600970416     C     RIC(I)        OREQ      'S'
082700970417     C                   MOVEL     SPD(I)        DEPSP             1            *VALIDA COME SPEDIZ
082800970417     C                   MOVEL     RIC(I)        DEPRI             1            *VALIDA COME RICAVO
082900970416X   2C                   ELSE
083000970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083100970416E   2C                   ENDIF
083200970416X   1C                   ELSE
083300970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083400970416E   1C                   ENDIF
083500970423     C*
083600970110     C* CLIENTE <> DA QUELLO RICHIESTO, ESCLUDE ED ESCE DAL PGM
083700970110IF  1C     PARKSC        IFGT      *ZEROS                                       *LETTURA X CLIENTE
083800021031     C* VERIFICO CHE IL CLIENTE CORRENTE SIA NELLA LISTA DI QUELLI DA CONSIDERARE
083900021031     C                   Z-ADD     TASKSC        WRKCLI           11 0
084000021031     C     WRKCLI        LOOKUP    SKCLI                                  60
084100021031IF  2C     *IN60         IFEQ      *OFF
084200021031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
084300021105     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
084400021031E   2C                   ENDIF
084500970110E   1C                   ENDIF
084600970110     C*
084700970110     C* DATA SPEDIZIONE > DA DATA FINALE RICHIESTA, ESCLUDE ED ESCE DAL PGM
084800970110IF  1C     PARKSC        IFEQ      *ZEROS                                       *LETTURA X DATA
084900970110     C     PARDS1        ANDGT     *ZEROS
085000970110IF  2C     TASDSP        IFGT      PARDS2
085100970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
085200970110     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
085300970110E   2C                   ENDIF
085400970110E   1C                   ENDIF
085500970110     C*
085600970430     C* FILIALE PARTENZA <> DA QUELLA RICHIESTA, ESCLUDE
085700970430IF  1C     PARFPA        IFGT      *ZEROS
085800970430IF  2C     PARFPA        IFNE      TASLNP
085900970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086000970110E   2C                   ENDIF
086100970110E   1C                   ENDIF
086200970430     C*
086300970430     C* FILIALE ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
086400970430IF  1C     PARFAR        IFGT      *ZEROS
086500970430IF  2C     PARFAR        IFNE      TASLNA
086600970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086700970430E   2C                   ENDIF
086800970430E   1C                   ENDIF
086900970430     C*
087000970430     C* FILIALE PARTENZA O ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
087100970430IF  1C     PARFAP        IFGT      *ZEROS
087200970430IF  2C     PARFAP        IFEQ      TASLNP
087300970430     C     PARFAP        OREQ      TASLNA
087400970430X   2C                   ELSE
087500970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
087600970430E   2C                   ENDIF
087700970430E   1C                   ENDIF
087800020227     C*
087900020227     C* QUALUNQUE FILIALE INDICATA NON SIA FEDEX
088000020307     C     PARFPA        IFGT      *ZEROS
088100020227     C     PARFPA        LOOKUP    FILFED                                 55
088200020227     C                   IF        *IN55 = *ON
088300020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
088400020227     C                   ENDIF
088500020307     C                   ENDIF
088600020307     C     PARFAR        IFGT      *ZEROS
088700020227     C     PARFAR        LOOKUP    FILFED                                 55
088800020227     C                   IF        *IN55 = *ON
088900020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
089000020227     C                   ENDIF
089100020307     C                   ENDIF
089200020307     C     PARFAP        IFGT      *ZEROS
089300020227     C     PARFAP        LOOKUP    FILFED                                 55
089400020227     C                   IF        *IN55 = *ON
089500020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
089600020227     C                   ENDIF
089700020307     C                   ENDIF
089800970110     C*
089900970110     C* DATA SPEDIZIONE <> DA QUELLE RICHIESTE, ESCLUDE
090000970110IF  1C     PARDS1        IFGT      *ZEROS
090100970110IF  2C     TASDSP        IFGE      PARDS1
090200970110     C     TASDSP        ANDLE     PARDS2
090300970110X   2C                   ELSE
090400970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090500970110E   2C                   ENDIF
090600970110E   1C                   ENDIF
090700970313     C*
090800970313     C* CODICE TARIFFA <> DA QUELLO RICHIESTO, ESCLUDE
090900970313IF  1C     PARTAR        IFNE      *BLANKS
091000970313     C                   MOVEL     PARTAR        WN3               3 0
091100970313IF  2C     WN3           IFNE      TASCTR
091200970313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
091300970313E   2C                   ENDIF
091400970313E   1C                   ENDIF
091500970430     C*
091600970430     C* RECORD VALIDO, IMPOPSTA LA FILIALE SULLA QUALE MEMORIZZARE I DATI
091700970430IF  1C     WRECOK        IFEQ      'S'
091800970430IF  2C     PARLNP        IFNE      999                                          *SELEZIONE FILIALE
091900970430     C                   Z-ADD     PARLNP        DEPFIL                         *FILIALE=SELEZIONE
092000970430X   2C                   ELSE                                                   *SELEZIONE MULTIPLA
092100970430     C                   Z-ADD     TASLNP        DEPFIL                         *FILIALE=L.PARTENZA
092200970430E   2C                   ENDIF
092300060829     c                   z-add     tasncl        colli
092400060829     c                   z-add     tasncl        colli1
092500040811     c* reperisce i bancali. se BNB imposto nel campo di comodo COLLI
092600040811     c* i colli originali
092700040811     c                   clear                   dar5ban
092800040811     c                   clear                   dar5bnb
092900040811     c                   if        %subst(tasgva: 1: 1) = 'O' or
093000040811     c                             %subst(tasgva: 1: 1) = 'B'
093100040811     c                   if        %subst(tasgva: 1: 1) = 'O'
093200040811     c                   eval      ar5trd = 'BNB'
093300040811     c                   else
093400040811     c                   eval      ar5trd = 'BAN'
093500040811     c                   end
093600060829     c                   exsr      srfiar5
093700040811     c                   if        %subst(tasgva: 1: 1) = 'O'
093800040811     c                   eval      dar5bnb = ar5uni
093900060829     c                   z-add     §ar5bcor      colli
094000040811     c                   else
094100040811     c                   eval      dar5BAN = ar5uni
094200040811     c                   end
094300040811     c                   end
094400040811E   1C                   ENDIF
094500970416     C*
094600970110     C                   ENDSR
094700970110     C*------------------------------------------------------------------------*
094800970110     C* MEMDAT - MEMORIZZA I DATI -BOLLE-
094900970110     C*------------------------------------------------------------------------*
095000970110     C     MEMDAT        BEGSR
095100970110     C*
095200970110     C* RECUPERA L'INDICE DELLA FILIALE
095300970110     C                   Z-ADD     1             F                 4 0
095400970110     C                   SETOFF                                           98
095500970430     C     DEPFIL        LOOKUP    SORG(F)                                98
095600970110IF  1C     *IN98         IFEQ      *ON                                          *FILIALE TROVATA
095700970110     C     F             OCCUR     FPAG
095800970110     C     F             OCCUR     FCON
095900970110     C     F             OCCUR     FRIT
096000970110     C     F             OCCUR     FSGL
096100060829     C     F             OCCUR     FSGL1
096200970110     C     F             OCCUR     FCTS
096300970423     C     F             OCCUR     FTAR
096400970415     C*
096500970415     C* REPERISCE LA SOLUZIONE DEL CLIENTE
096600970415     C                   MOVEL     *BLANKS       $SOL              1
096700970415     C                   EXSR      REPSOL
096800060315     C*
096900060315     C* REPERISCE le varie di fatturazione
097000060315     C                   clear                   svn
097100060315     C                   clear                   van
097200060315     C                   EXSR      REPvrf
097300970415     C*
097400060829     C* REPERISCE IL VOLUME DELLA BOLLA
097500970415     C                   EXSR      REPVOL
097600040811     C*
097700060829     C* REPERISCE IL PESO DELLA BOLLA da
097800040811     C                   EXSR      REPpes
097900970110     C*
098000060315     C* REPERISCE LE consegne particolari
098100970110     C                   MOVEL     *BLANKS       $PCA              1
098200970110     C                   MOVEL     *BLANKS       $PCP              1
098300970110     C                   MOVEL     *BLANKS       $PCS              1
098400970110     C                   MOVEL     *BLANKS       $PCF              1
098500970415     C                   MOVEL     *BLANKS       $PCD              1
098600990812     C                   MOVEL     *BLANKS       $PIN              1            *INOLTRO
098700011026     C                   MOVEL     *ZEROS        $PINIM           21 8          *IMPORTO INOLTRO
098800011011     C                   MOVEL     *BLANKS       $CIS              1
098900011011     C                   MOVEL     *BLANKS       $RIS              1
099000040802     C                   MOVEL     *BLANKS       $ded              1
099100970110     C                   EXSR      REPVAR
099200000414     C*
099300000414     C* REPERISCE LA GIACIENZA
099400000414     C                   MOVEL     *BLANKS       $PGI              1            *ESISTE GIACENZA
099500000414     C                   Z-ADD     *ZEROS        $PGIGS            9 0          *GG SOSTA
099600000414     C                   Z-ADD     *ZEROS        $PGINR            9 0          *N° RIAPERTURE
099700021122     C                   EXSR      REPGIA
099800970110     C*
099900970110     C* REPERISCE IL C/ASSEGNO
100000011026     C                   Z-ADD     *ZEROS        $CAS             21 8
100100970110     C                   MOVEL     *BLANKS       $TPI              1
100200970110IF  2C     TASFCA        IFEQ      'S'                                          *ESISTE C/ASSEGNO
100300970110     C                   EXSR      REPCAS
100400970110E   2C                   ENDIF
100500970110     C*
100600970110     C* REPERISCE L'ASSICURAZIONE
100700011026     C                   Z-ADD     *ZEROS        $IAS             21 8
100800970110     C                   EXSR      REPASS
100900970110     C*
101000970110     C* REPERISCE IL CODICE TASSAZIONE
101100970423     C                   Z-ADD     1             J
101200970423     C                   SETOFF                                           98
101300970423     C     TASCTS        LOOKUP    CTS(J)                                 98
101400970423IF  2C     *IN98         IFEQ      *ON                                          *TROVATO
101500970416     C*---
101600970110     C*  M E M O R I Z Z A
101700970416     C*---
101800970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
101900970423     C*
102000971204IF  4C     FL1(F)        IFNE      *BLANKS                                      *CODICE ESTERO
102100970423     C     CIE(J)        OREQ      'E'
102200970423     C                   Z-ADD     1             I
102300970423     C                   SETOFF                                           98
102400970423     C     TASCTS        LOOKUP    CCTE(I)                                98
102500970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
102600970423     C                   ADD       1             TSE(I)                         *N°SPED.CTS ESTERO
102700970423E   5C                   ENDIF
102800970423X   4C                   ELSE                                                   *CODICE ITALIA
102900970423     C                   Z-ADD     1             I
103000970423     C                   SETOFF                                           98
103100970423     C     TASCTS        LOOKUP    CCTI(I)                                98
103200970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
103300970423     C                   ADD       1             TSI(I)                         *N°SPED.CTS ITALIA
103400970423E   5C                   ENDIF
103500970423E   4C                   ENDIF
103600970423     C*
103700970423     C                   ADD       1             SPE(F)                         *N°SPEDIZIONI TOTALI
103800020215     C*
103900020215IF  4C     $CLV          IFEQ      'D'
104000020215     C                   ADD       1             CLD(F)                         *CLIENTE TIPO D
104100020215E   4C                   ENDIF
104200980901IF  4C     $CLV          IFEQ      'T'
104300980901     C                   ADD       1             CLT(F)                         *CLIENTE TIPO T
104400980901E   4C                   ENDIF
104500970416IF  4C     $CLV          IFEQ      'A'
104600970110     C                   ADD       1             CLA(F)                         *CLIENTE TIPO A
104700970416E   4C                   ENDIF
104800970416IF  4C     $CLV          IFEQ      'B'
104900970110     C                   ADD       1             CLB(F)                         *CLIENTE TIPO B
105000970416E   4C                   ENDIF
105100970416IF  4C     $CLV          IFEQ      'C'
105200970415     C     $CLV          OREQ      *BLANKS
105300970110     C                   ADD       1             CLC(F)                         *CLIENTE TIPO C
105400970416E   4C                   ENDIF
105500970110     C*
105600970416IF  4C     $SOL          IFEQ      *BLANKS
105700970110     C                   ADD       1             BON(F)                         *BOLLETTAZIONE (' ')
105800970416E   4C                   ENDIF
105900970416IF  4C     $SOL          IFEQ      'A'
106000970110     C                   ADD       1             BOA(F)                         *BOLLETTAZIONE (A)
106100970416E   4C                   ENDIF
106200970416IF  4C     $SOL          IFEQ      'B'
106300970110     C                   ADD       1             BOB(F)                         *BOLLETTAZIONE (B)
106400970416E   4C                   ENDIF
106500050808     c* con il cod. trattamento merce aggancio tabella 1B per vedere se
106600050808     c* previsto packing list
106700050808     c                   z-add     1             xp                4 0
106800050808     c     tasctm        lookup    cpkl(xp)                               95
106900050808     c                   if        *in95
107000050808     c                   add       1             pks(f)
107100050808     c                   else
107200050808     c                   add       1             pkn(f)
107300050808     c                   end
107400040811     C* se oltre 20 colli sommo i colli originali altrimenti i colli
107500040811     c* della bolla
107600040811     C                   ADD       colli         NCL(F)                         *COLLI
107700040811     C                   ADD       $pes          PKB(F)                         *PESO
107800040811     C                   ADD       $vol          VLB(F)                         *VOLUME
107900060829     C                   ADD       colli1        NCL1(F)                        *COLLI
108000060829     C                   ADD       $pes1         PKB1(F)                        *PESO
108100060829     C                   ADD       $vol1         VLB1(F)                        *VOLUME
108200970110     C*
108300970110     C                   MOVEL     TASTBL        WA1               1
108400970416IF  4C     WA1           IFEQ      'F'
108500970110     C                   ADD       1             FRA(F)                         *FRANCHI
108600970416X   4C                   ELSE
108700970110     C                   ADD       1             ASS(F)                         *ASSEGNATI
108800970416E   4C                   ENDIF
108900970110     C*
109000970110     C                   MOVEL     TASGVA        WA1
109100970416IF  4C     WA1           IFEQ      'P'
109200970110     C                   ADD       1             PCV(F)                         *CONSEGNE PRIVATI
109300970416E   4C                   ENDIF
109400040802     C* bancali bartolini
109500040802IF  4C     WA1           IFEQ      'B'
109600040824     C                   ADD       1             sban(F)                        *CONSEGNE PRIVATI
109700040809     C                   ADD       §ar5nba       nba(F)                         *CONSEGNE PRIVATI
109800040809     C                   ADD       §ar5nb2       nba(F)                         *CONSEGNE PRIVATI
109801150304     c                   if        §ar5tba = 01 or §ar5tb2 = 01
109802150304     C                   ADD       1             sb01(F)                        *CONSEGNE PRIVATI
109803150304     C                   ADD       §ar5nba       nb01(F)                        *CONSEGNE PRIVATI
109804150302     c                   end
109805150304     c                   if        §ar5tba = 02 or §ar5tb2 = 02
109806150304     C                   ADD       1             sb02(F)                        *CONSEGNE PRIVATI
109807150304     C                   ADD       §ar5nba       nb02(F)                        *CONSEGNE PRIVATI
109808150302     c                   end
109809150304     c                   if        §ar5tba = 03 or §ar5tb2 = 03
109810150304     C                   ADD       1             sb03(F)                        *CONSEGNE PRIVATI
109811150304     C                   ADD       §ar5nba       nb03(F)                        *CONSEGNE PRIVATI
109812150302     c                   end
109900040802E   4C                   ENDIF
110000040802     C* bancali oltre 20 colli
110100040802IF  4C     WA1           IFEQ      'O'
110200040811     C                   ADD       1             sbnb(F)                        *CONSEGNE PRIVATI
110300040811     C                   ADD       §ar5bnba      nbn(F)                         *CONSEGNE PRIVATI
110400040811     C                   ADD       §ar5bnb2      nbn(F)                         *CONSEGNE PRIVATI
110500040802E   4C                   ENDIF
110600040802     C* merce di valore
110700040802     c                   movel     tasflo        dtasflo
110800040802IF  1C     §FLOTBA       IFEQ      'V'
110900040802     C                   add       1             val(F)
111000040802E   1C                   ENDIF
111100970110     C*
111200970416IF  4C     TASFDN        IFEQ      'S'
111300011011     C                   ADD       1             PCM(F)                         *MERCE A MAGAZZINO
111400970416E   4C                   ENDIF
111500970110     C*
111600970416IF  4C     TASFFD        IFEQ      'S'
111700970110     C                   ADD       1             PFD(F)                         *FERMO DEPOSITO
111800970416E   4C                   ENDIF
111900970110     C*
112000970416IF  4C     TASTSP        IFEQ      'E'
112100970110     C                   ADD       1             ESP(F)                         *ESPRESSI
112200970416E   4C                   ENDIF
112300970110     C*
112400011011IF  4C     TASFIN        IFEQ      'D'
112500011011     C                   ADD       1             PCL(F)                         *CONS. L.DISAGIATE
112600011011E   4C                   ENDIF
112700011011     C*
112800011011IF  4C     TASFIN        IFEQ      'I'
112900011011     C                   ADD       1             CIS(F)                         *CONS. ISOLE
113000011011E   4C                   ENDIF
113100011011     C*
113200011011IF  4C     TASFAP        IFEQ      'I'
113300011011     C                   ADD       1             RIS(F)                         *RITIRI ISOLE
113400011011E   4C                   ENDIF
113500011011     C*
113600011011IF  4C     TASFIN        IFEQ      'P'
113700110623IF  4C     TASFIN        oreq      'Z'
113800011011     C                   ADD       1             PPV(F)                         *CONSEGNE PROVINCIA
113900970416E   4C                   ENDIF
114000970110     C*
114100970416IF  4C     $PCA          IFEQ      'S'
114200970110     C                   ADD       1             PCA(F)                         *CONS. APPUNTAMENTO
114300970416E   4C                   ENDIF
114400970110     C*
114500970416IF  4C     $PCP          IFEQ      'S'
114600970110     C                   ADD       1             PCP(F)                         *CONS. PIANI
114700970416E   4C                   ENDIF
114800970110     C*
114900970416IF  4C     $PCS          IFEQ      'S'
115000970110     C                   ADD       1             PCS(F)                         *CONS. SUPERMERCATI
115100970416E   4C                   ENDIF
115200970110     C*
115300970416IF  4C     $PCF          IFEQ      'S'
115400970110     C                   ADD       1             PCF(F)                         *CONS. FUORI MISURA
115500970416E   4C                   ENDIF
115600040802     C* destinatario disagiato
115700040802IF  4C     $ded          IFEQ      'S'
115800040802     C                   ADD       1             ded(F)                         *CONS. FUORI MISURA
115900040802E   4C                   ENDIF
116000970415     C*
116100970416IF  4C     $PCD          IFEQ      'S'
116200970415     C                   ADD       1             PCD(F)                         *CONS. DDT
116300970416E   4C                   ENDIF
116400970110     C*
116500970416IF  4C     $GGP          IFGT      *ZEROS
116600970110     C                   Z-ADD     1             I
116700970110     C                   SETOFF                                           98
116800970110     C     $GGP          LOOKUP    GGP(I)                                 98
116900970416IF  5C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
117000970110     C                   ADD       1             SGP(I)                         *SPED. GG PAGAMENTO
117100970416X   5C                   ELSE                                                   *MAI MEMORIZZATI
117200970110     C                   Z-ADD     1             I
117300970416DO  6C     I             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
117400970416IF  7C     GGP(I)        IFEQ      *ZEROS                                       *DISPONIBILE
117500970110     C                   Z-ADD     $GGP          GGP(I)                         *GG
117600970110     C                   Z-ADD     1             SGP(I)                         *SPED. GG PAGAMENTO
117700981014     C                   LEAVE                                                  *ESCE DAL CICLO
117800970416E   7C                   ENDIF
117900970416E   6C                   ENDDO
118000970416E   5C                   ENDIF
118100970416E   4C                   ENDIF
118200000411     C*
118300000411     C                   MOVEL     TASTBL        WA1               1
118400000411IF  4C     WA1           IFEQ      'F'                                          *SOLO FRANCHI
118500000411IF  5C     TASSTR        IFGT      *ZEROS
118600990812     C     TASSTR        ANDNE     99999
118700000411IF  6C     TASSTR        IFGT      999                                          *CAMPO 5,0 -> 3,0
118800990812     C                   Z-ADD     999           TASSTR
118900000411E   6C                   ENDIF
119000970110     C                   Z-ADD     TASSTR        I                              *STOP RITIRO
119100970110     C                   ADD       1             RIT(I)                         *SPEDIZIONI STOP
119200000411E   5C                   ENDIF
119300000411     C*
119400000411X   4C                   ELSE                                                   *SOLO ASSEGNATI
119500000411     C*
119600000411IF  5C     TASSTP        IFGT      *ZEROS
119700990812     C     TASSTP        ANDNE     99999
119800000411IF  6C     TASSTP        IFGT      999                                          *CAMPO 5,0 -> 3,0
119900990812     C                   Z-ADD     999           TASSTP
120000000411E   6C                   ENDIF
120100970110     C                   Z-ADD     TASSTP        I                              *STOP CONSEGNA
120200970110     C                   ADD       1             CON(I)                         *SPEDIZIONI STOP
120300000411E   5C                   ENDIF
120400000411E   4C                   ENDIF
120500970110     C*
120600060829     c* per ricavi
120700970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
120800970110     C     PARTTR        OREQ      '3'                                          *KG
120900040811     C                   Z-ADD     $pes          TASSGL           13 3          *SCAGLIONE IN KG
121000970416X   4C                   ELSE
121100040811     C                   Z-ADD     colli         TASSGL                         *SCAGLIONE IN COLLI
121200040811E   4C                   ENDIF
121300971024IF  4C     TASSGL        IFGT      *ZEROS
121400971024DO  5C     1             DO        10            I
121500971024IF  6C     TASSGL        IFLE      SSGL(I)
121600970110     C                   ADD       1             SSP(I)                         *N°SPED SCAGLIONE
121700040811     C                   ADD       $pes          PSC(I)                         *PESO
121800040811     C                   ADD       colli         CSC(I)                         *COLLI
121900970415     C                   ADD       $VOL          VSC(I)                         *VOLUME
122000970110     C                   LEAVE
122100971024E   6C                   ENDIF
122200971024E   5C                   ENDDO
122300040920     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
122400060829IF  5C     PARCTO        IFEQ      'T'
122500040920     C     PARCAR        ANDEQ     88888
122600040920     C     PARLNP        ANDEQ     999
122700040920     C                   EXSR      SRSGL8
122800060829E   5C                   ENDIF
122900971024E   4C                   ENDIF
123000060829     c* per competenze
123100060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
123200060829     C     PARTTR        OREQ      '3'                                          *KG
123300060829     C                   Z-ADD     $pes1         TASSGL           13 3          *SCAGLIONE IN KG
123400060829X   4C                   ELSE
123500060829     C                   Z-ADD     colli1        TASSGL                         *SCAGLIONE IN COLLI
123600060829E   4C                   ENDIF
123700060829IF  4C     TASSGL        IFGT      *ZEROS
123800060829DO  5C     1             DO        10            I
123900060829IF  6C     TASSGL        IFLE      SSGL(I)
124000060829     C                   ADD       1             SSP1(I)                        *N°SPED SCAGLIONE
124100060829     C                   ADD       $pes1         PSC1(I)                        *PESO
124200060829     C                   ADD       colli1        CSC1(I)                        *COLLI
124300060829     C                   ADD       $VOL1         VSC1(I)                        *VOLUME
124400060829     C                   LEAVE
124500060829E   6C                   ENDIF
124600060829E   5C                   ENDDO
124700060829     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
124800060829IF  5C     PARCTO        IFEQ      'T'
124900060829     C     PARCAR        ANDEQ     88888
125000060829     C     PARLNP        ANDEQ     999
125100060829     C                   EXSR      SRSGL81
125200060829E   5C                   ENDIF
125300060829E   4C                   ENDIF
125400971024E   3C                   ENDIF
125500970110     C*
125600970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
125700970416     C     DEPRI         OREQ      'S'                                          *BOLLA VALIDA RICAVI
125800970416IF  4C     $CAS          IFGT      *ZEROS
125900970110     C                   ADD       $CAS          CAS(F)                         *C/ASSEGNO
126000970416IF  5C     $TPI          IFEQ      'M'
126100970110     C                   ADD       1             PAM(F)                         *MITTENTE
126200970416X   5C                   ELSE
126300970110     C                   ADD       1             PAB(F)                         *BARTOLINI
126400970416E   5C                   ENDIF
126500970416E   4C                   ENDIF
126600970110     C*
126700970416IF  4C     $IAS          IFGT      *ZEROS
126800970110     C                   ADD       $IAS          ASC(F)                         *ASSICURAZIONE
126900970110     C                   ADD       1             PAC(F)                         *ASSICURAZIONE CONTO
127000970416X   4C                   ELSE
127100060315     c                   if        flgacbase = *on
127200060315     C                   ADD       1             xx1(F)                         *ASSICURAZIONE R.C.V
127300060315     c                   else
127400970110     C                   ADD       1             PAR(F)                         *ASSICURAZIONE R.C.V
127500970416E   4C                   ENDIF
127600060315E   4C                   ENDIF
127700970110     C*
127800000414IF  4C     $PGI          IFEQ      'S'
127900970110     C                   ADD       1             PGI(F)                         *GIACENZE
128000000414     C                   ADD       $PGIGS        GGS(F)                         *GG SOSTA GIACENZA
128100000414     C                   ADD       $PGINR        NRG(F)                         *N° RIAPERTURE GIACE
128200970416E   4C                   ENDIF
128300970416E   3C                   ENDIF
128400970110     C*
128500970416X   2C                   ELSE                                                   *CON.TASS.NON TROVAT
128600970416     C                   MOVEL     MTAS(2)       DEPERR           50
128700970110     C                   MOVEL     'T'           DEPTER            1
128800970110     C                   EXSR      STAERR
128900970416E   2C                   ENDIF
129000970110     C*
129100970110X   1C                   ELSE                                                   *FILIALE NON TROVATA
129200970110     C                   MOVEL     MTAS(1)       DEPERR
129300970110     C                   MOVEL     'T'           DEPTER
129400970110     C                   EXSR      STAERR
129500970110E   1C                   ENDIF
129600970110     C*
129700970110     C                   ENDSR
129800970415     C*------------------------------------------------------------------------*
129900040809     C* fiar5 tipo record BAN
130000970415     C*------------------------------------------------------------------------*
130100040809     C     srfiar5       BEGSR
130200040809     c*
130300040809     c                   z-add     tasaas        ar5aas
130400040809     c                   z-add     taslnp        ar5lnp
130500040809     c                   z-add     tasnrs        ar5nrs
130600040809     c                   z-add     tasnsp        ar5nsp
130700040811     c                   clear                   ar5uni
130800040809     c     kar5          chain     fiar531c
130900040809     C*
131000040809     C                   ENDSR
131100040809     C*------------------------------------------------------------------------*
131200040809     C* REPSOL - REPERISCE LA SOLUZIONE DI INVIO DATI -BOLLETTAZIONE-
131300040809     C*------------------------------------------------------------------------*
131400040809     C     REPSOL        BEGSR
131500970415     C*---
131600970415     C* NON E' PRESENTE IN TABELLA -> NESSUNA PROCEDURA            ( )
131700970415     C* NON HA IL NUMERO DI SERIE  -> STAMPA LE BOLLE              (A)
131800970415     C* HA     IL NUMERO DI SERIE  -> STAMPA LE BOLLE + SEGNACOLLI (B)
131900040310     C*--- escludo bolle recapito contrassegni
132000970415IF  1C     TASNRS        IFGT      *ZEROS
132100040310IF  1C     TAScbo        andne     'FW'
132200970415     C                   MOVEL     'B'           $SOL                           *PROCEDURA 'B'
132300970415X   1C                   ELSE
132400100303     C                   Z-ADD     1             KTBKUT
132500100303     C                   MOVEL     '3C'          KTBCOD
132600100303     c                   movel(p)  tasksc        ktbkey
132700100303     c     keytab        chain     tabel00f
132800100303     c                   if        %found(tabel00f) and tblflg = ' '
132900100303     C                   MOVEL     TBLUNI        DS3C
133000100303IF  3C     §3COKD        IFEQ      '*'
133100970415     C                   MOVEL     'A'           $SOL                           *PROCEDURA 'A'
133200970415X   3C                   ELSE
133300970415     C                   MOVEL     *BLANKS       $SOL                           *PROCEDURA NO AVVIAT
133400970415E   3C                   ENDIF
133500970415E   2C                   ENDIF
133600970415E   1C                   ENDIF
133700970415     C*
133800970415     C                   ENDSR
133900970415     C*------------------------------------------------------------------------*
134000970415     C* REPVOL - REPERISCE IL VOLUME DELLA BOLLA
134100970415     C*------------------------------------------------------------------------*
134200970415     C     REPVOL        BEGSR
134300060829     C* colli x ricavi
134400060829     C                   Z-ADD     *ZEROS        $VOL              5 3
134500970415     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
134600040811    1C     TASVLC        IFGT      *ZEROS
134700040811     C     colli         ANDEQ     TASNCR
134800970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
134900970415E   1C                   ENDIF
135000970415     C*
135100970415     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
135200970415     C     TASVLC        IFGT      TASVLF
135300040811     C     colli         ANDNE     TASNCR
135400970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
135500970415E   1C                   ENDIF
135600970415     C*
135700970415     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
135800970415IF  1C     $VOL          IFEQ      *ZEROS
135900970415     C                   Z-ADD     TASVLF        $VOL                           *VOLUME NORMALE
136000060829E   1C                   ENDIF
136100060829c*   c*colli x competetnze
136200060829     C                   Z-ADD     *ZEROS        $VOL1             5 3
136300060829     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
136400060829    1C     TASVLC        IFGT      *ZEROS
136500060829     C     colli1        ANDEQ     TASNCR
136600060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
136700060829E   1C                   ENDIF
136800060829     C*
136900060829     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
137000060829     C     TASVLC        IFGT      TASVLF
137100060829     C     colli1        ANDNE     TASNCR
137200060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
137300060829E   1C                   ENDIF
137400060829     C*
137500060829     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
137600060829IF  1C     $VOL1         IFEQ      *ZEROS
137700060829     C                   Z-ADD     TASVLF        $VOL1                          *VOLUME NORMALE
137800060829E   1C                   ENDIF
137900970415     C*
138000000414     C                   ENDSR
138100040811     C*------------------------------------------------------------------------*
138200040811     C* REPpes - REPERISCE IL peso DELLA BOLLA
138300040811     C*------------------------------------------------------------------------*
138400040811     C     REPpes        BEGSR
138500060829     C* colli x ricavi
138600060829     C                   Z-ADD     *ZEROS        $pes
138700040811     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
138800040811    1C     TASpkC        IFGT      *ZEROS
138900040811     C     colli         ANDEQ     TASNCp
139000040811     C                   Z-ADD     TASpkc        $pes                           *VOLUME CML
139100040811E   1C                   ENDIF
139200040811     C*
139300040811     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
139400040811     C     TASpkc        IFGT      TASpkF
139500040811     C     colli         ANDNE     TASNCp
139600040811     C                   Z-ADD     TASpkC        $pes                           *VOLUME CML
139700040811E   1C                   ENDIF
139800040811     C*
139900040811     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
140000040811IF  1C     $pes          IFEQ      *ZEROS
140100040811     C                   Z-ADD     TASpkF        $pes                           *VOLUME NORMALE
140200040811E   1C                   ENDIF
140300060829     C* colli x competenze
140400060829     C                   Z-ADD     *ZEROS        $pes1
140500060829     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
140600060829    1C     TASpkC        IFGT      *ZEROS
140700060829     C     colli1        ANDEQ     TASNCp
140800060829     C                   Z-ADD     TASpkc        $pes1                          *VOLUME CML
140900060829E   1C                   ENDIF
141000060829     C*
141100060829     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
141200060829     C     TASpkc        IFGT      TASpkF
141300060829     C     colli1        ANDNE     TASNCp
141400060829     C                   Z-ADD     TASpkC        $pes1                          *VOLUME CML
141500060829E   1C                   ENDIF
141600060829     C*
141700060829     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
141800060829IF  1C     $pes1         IFEQ      *ZEROS
141900060829     C                   Z-ADD     TASpkF        $pes1                          *VOLUME NORMALE
142000060829E   1C                   ENDIF
142100040811     C*
142200040811     C                   ENDSR
142300970110     C*------------------------------------------------------------------------*
142400970110     C* REPCAS - REPERISCE IL CONTRASSEGNO
142500970110     C*------------------------------------------------------------------------*
142600970110     C     REPCAS        BEGSR
142700970110     C*
142800970110     C                   Z-ADD     TASAAS        KSBAAS                         *ANNO
142900970110     C                   Z-ADD     TASLNP        KSBLNP                         *LINEA PARTENZA
143000970110     C                   Z-ADD     TASNRS        KSBNRS                         *SERIE
143100970110     C                   Z-ADD     TASNSP        KSBNSP                         *SPEDIZIONE
143200970110     C     KEYCSB        CHAIN     TNCSB03L                           98
143300970110IF  1C     *IN98         IFEQ      *OFF                                         *TROVATO
143400970110IF  2C     CSBSTA        IFNE      9                                            *NO ANNULLATO
143500970110     C                   MOVEL     CSBTPI        $TPI                           *TIPO INCASSO
143600011011     C                   Z-ADD     CSBCAS        $CAS                           *C/ASSEGNO
143700011011     C     CSBVCA        IFNE      PARDIV
143800011011     C                   Z-ADD     $CAS          WIMI             30 9
143900011011     C                   MOVEL     CSBVCA        WDVI              3
144000011011     C                   EXSR      CNVEUR
144100011011     C                   Z-ADD     WIMO          $CAS
144200011011     C                   ENDIF
144300970110E   2C                   ENDIF
144400970110E   1C                   ENDIF
144500970110     C*
144600970110     C                   ENDSR
144700021122     C*------------------------------------------------------------------------*
144800021122     C* REPGIA - REPERISCE LE GIACENZE
144900021122     C*------------------------------------------------------------------------*
145000021122     C     REPGIA        BEGSR
145100021122     C*
145200021122     C* AZZERA LE VARIABILI DI LAVORO
145300021122     C                   Z-ADD     *ZEROS        §GGSOS            3 0
145400021122     C*
145500021122     C* LEGGE TUTTE LE GIACENZE DELLA BOLLA
145600021122     C                   Z-ADD     TASAAS        KGPAAS
145700021122     C                   Z-ADD     TASLNP        KGPLNP
145800021122     C                   Z-ADD     TASNRS        KGPNRS
145900021122     C                   Z-ADD     TASNSP        KGPNSP
146000050309     C     K01GCP        SETLL     TIGCP51L
146100050309     C     K01GCP        READE     TIGCP51L                               98
146200021122DO  1C     *IN98         DOWEQ     *OFF
146300021122IF  2C     GCPATB        IFEQ      *BLANKS
146400021122     C                   MOVEL     'S'           $PGI                           *ESISTE GIACENZA
146500021122     C* APERTURE
146600021122     C                   ADD       1             $PGINR                         *NUMERO RIAPERTURE
146700021122     C* GIORNI DI SOSTA
146800021122IF  3C     GCPDDM        IFGT      *ZEROS                                       *IMMESSE DISPOSIZION
146900021122IF  4C     GCPDUR        IFGT      *ZEROS                                       *ULTIMA RIAPERTURA
147000021122     C                   Z-ADD     GCPDUR        G08INV
147100021122X   4C                   ELSE                                                   *APERTURA GIACENZA
147200021122     C                   MOVEL     GCPAGC        G08INV
147300021122     C                   MOVE      GCPMGC        G08INV
147400021122E   4C                   ENDIF
147500021122     C                   Z-ADD     *ZEROS        G08DAT
147600021122     C                   MOVEL     '3'           G08ERR
147700021122     C                   CALL      'XSRDA8'
147800021122     C                   PARM                    WLBDA8
147900021122     C                   Z-ADD     G08TGI        §GGDA             5 0
148000021122IF  4C     GCPDED        IFGT      *ZEROS                                       *DISPOSIZ.GIACENZA
148100021122     C                   Z-ADD     GCPDED        G08INV
148200021122X   4C                   ELSE                                                   *DISPOSIZ.MITTENTE
148300021122     C                   Z-ADD     GCPDDM        G08INV
148400021122E   4C                   ENDIF
148500021122     C                   Z-ADD     *ZEROS        G08DAT
148600021122     C                   MOVEL     '3'           G08ERR
148700021122     C                   CALL      'XSRDA8'
148800021122     C                   PARM                    WLBDA8
148900021122     C                   Z-ADD     G08TGI        §GGA              5 0
149000021122     C* GIORNI DI SOSTA
149100021122     C     §GGA          SUB       §GGDA         §GGXX             3 0
149200021122     C                   ADD       1             §GGXX
149300021122     C                   ADD       §GGXX         §GGSOS
149400021122E   3C                   ENDIF
149500021122E   2C                   ENDIF
149600050309     C     K01GCP        READE     TIGCP51L                               98
149700021122E   1C                   ENDDO
149800021122     C*
149900021122     C                   Z-ADD     §GGSOS        $PGIGS                         *GIORNI DI SOSTA
150000021122     C*
150100021122     C                   ENDSR
150200970110     C*------------------------------------------------------------------------*
150300970110     C* REPASS - REPERISCE L'ASSICURAZIONE
150400970110     C*------------------------------------------------------------------------*
150500970110     C     REPASS        BEGSR
150600060315     c                   clear                   flgacbase         1
150700970110     C*
150800970110IF  1C     TASIAS        IFGT      *ZEROS
150900011011     C                   Z-ADD     TASIAS        $IAS
151000011011IF  2C     TASVAS        IFNE      PARDIV                                       *DIVISA ESTERA
151100011011     C                   Z-ADD     $IAS          WIMI             30 9
151200011011     C                   MOVEL     TASVAS        WDVI              3
151300011011     C                   EXSR      CNVEUR
151400011011     C                   Z-ADD     WIMO          $IAS
151500011011     C                   ENDIF
151600060315     c* verifico se esiste ac base
151700060315     c                   else
151800060315     c                   z-add     1             v
151900060315     c     'd'           lookup    svn(v)                                 99
152000060320     c                   if        van(v) <> 0 and *in99
152100060315     c                   eval      flgacbase = *on
152200970110E   1C                   ENDIF
152300060315E   1C                   ENDIF
152400970110     C*
152500970110     C                   ENDSR
152600970110     C*------------------------------------------------------------------------*
152700060315     C* REPVrf - REPERISCE le varie di fatturazione
152800990812     C* -----------------------------------------------------------------------*
152900060315     C     REPVrf        BEGSR
153000060315     C* CARICO LE VARIE BOLLA ORIGINALE DA TITAS
153100060315     C                   Z-ADD     *ZEROS        V                 6 0
153200060315     C     tasSV1        IFNE      *BLANKS
153300060315     C                   ADD       1             V
153400060315     C                   MOVEL     tasSV1        SVN(V)
153500060315     C                   Z-ADD     tasVA1        VAN(V)
153600060315     C                   ENDIF
153700060315     C     tasSV2        IFNE      *BLANKS
153800060315     C                   ADD       1             V
153900060315     C                   MOVEL     tasSV2        SVN(V)
154000060315     C                   Z-ADD     tasVA2        VAN(V)
154100060315     C                   ENDIF
154200060315     C     tasSV3        IFNE      *BLANKS
154300060315     C                   ADD       1             V
154400060315     C                   MOVEL     tasSV3        SVN(V)
154500060315     C                   Z-ADD     tasVA3        VAN(V)
154600060315     C                   ENDIF
154700060315     C* CARICO LE VARIE BOLLA DA TITA7 (SOLO SE C'E' LA VARIA 3)
154800060315     C     tasSV3        IFNE      *BLANKS
154900060315     C                   Z-ADD     tasAAS        TA7AAS
155000060315     C                   Z-ADD     tasLNP        TA7LNP
155100060315     C                   Z-ADD     tasNRS        TA7NRS
155200060315     C                   Z-ADD     tasNSP        TA7NSP
155300060315     C                   MOVEL     tasTBL        TA7TBL
155400060315     C     KEYTA7        SETLL     TITA730C
155500060315     c                   do        *hival
155600060315     C     KEYTA7        READE     TITA730C
155700060315     c                   if        %eof(tita730c)
155800060315     c                   leave
155900060315     c                   end
156000060315if  1C                   IF        (TA7VAN = 88888888,000)
156100060315     C                   Z-ADD     *zeros        TA7VAN
156200060315e   1c                   ENDIF
156300060315     C                   ADD       1             V
156400060315     C                   MOVEL     TA7SVN        SVN(V)
156500060315     C                   Z-ADD     TA7VAN        VAN(V)
156600060315     C                   ENDDO
156700060315e   1c                   ENDIF
156800060315     c                   endsr
156900060315     C*------------------------------------------------------------------------*
157000060315     C* REPVAR - REPERISCE LE consegne particolari
157100060315     C* -----------------------------------------------------------------------*
157200060315     C     REPVAR        BEGSR
157300970110     C*
157400970110     C* VARIE -BOLLA-
157500021122IF  1C     TASFTC        IFEQ      'A'
157600021122     C     TASTC2        OREQ      'A'
157700970110     C                   MOVEL     'S'           $PCA                           *CONS APPUNTAMENTO
157800970110E   1C                   ENDIF
157900021122IF  1C     TASFTC        IFEQ      'P'
158000021122     C     TASTC2        OREQ      'P'
158100970110     C                   MOVEL     'S'           $PCP                           *CONS PIANI
158200970110E   1C                   ENDIF
158300021122IF  1C     TASFTC        IFEQ      'S'
158400021122     C     TASTC2        OREQ      'S'
158500970110     C                   MOVEL     'S'           $PCS                           *CONS SUPERMERCATI
158600970110E   1C                   ENDIF
158700021122IF  1C     TASFTC        IFEQ      'F'
158800021122     C     TASTC2        OREQ      'F'
158900970110     C                   MOVEL     'S'           $PCF                           *CONS FUORI MISURA
159000970110E   1C                   ENDIF
159100040802IF  1C     TASFTC        IFEQ      'X'
159200040802     C     TASTC2        OREQ      'X'
159300040802     C                   MOVEL     'S'           $ded                           *CONS FUORI MISURA
159400040802E   1C                   ENDIF
159500021122IF  1C     TASLL1        IFEQ      'S'
159600970415     C                   MOVEL     'S'           $PCD                           *CONS DDT
159700970415E   1C                   ENDIF
159800970110     C*
159900970110     C                   ENDSR
160000980819     C*------------------------------------------------------------------------*
160100980819     C* AGGETX - AGGIORNA I DATI TIPO
160200980819     C*------------------------------------------------------------------------*
160300980819     C     AGGETX        BEGSR
160400980819     C*
160500980819     C* PER OGNI LINEA DI PARTENZA RICHIESTA
160600980819DO  1C     1             DO        500           I
160700980819IF  2C     SORG(I)       IFGT      *ZEROS
160800980819IF  3C     SPE(I)        IFGT      *ZEROS                                       *MEMORIZZATO QC.
160900980821     C                   Z-ADD     SORG(I)       XORG
161000980819     C     I             OCCUR     FPAG
161100980819     C     I             OCCUR     FCON
161200980819     C     I             OCCUR     FRIT
161300980819     C     I             OCCUR     FSGL
161400060829     C     I             OCCUR     FSGL1
161500980819     C     I             OCCUR     FCTS
161600980819     C     I             OCCUR     FTAR                                         *DATI TARIFFA
161700980819     C*---
161800980819     C* DATI TIPO -GENERALI-
161900980819     C*---
162000980821     C                   EXSR      SCRETC
162100980819     C*---
162200980819     C* DATI TIPO -SCAGLIONI-
162300980819     C*---
162400060829     c* ricavi
162500980821     C                   EXSR      SCRETS
162600060829     c* competenze
162700060829     c                   if        etcbnb <> 0
162800060829     c                   exsr      screts1
162900060829     c                   end
163000980819     C*---
163100980819     C* DATI TIPO -CODICI TASSAZIONE-
163200980819     C*---
163300980819     C* PER OGNI TIPO TARIFFA (ITALIA/ESTERO)
163400980819DO  4C     1             DO        2             K
163500980819IF  5C     SFIE(K)       IFNE      *BLANKS
163600980821     C                   MOVEL     SFIE(K)       XFIE
163700980821     C                   EXSR      SCRETT
163800981215E   5C                   ENDIF
163900980819E   4C                   ENDDO                                                  *FINE TARIFFA ITA/ES
164000980819     C*
164100980819E   3C                   ENDIF
164200980819E   2C                   ENDIF
164300980819E   1C                   ENDDO                                                  *FINE FILIALI
164400980819     C*
164500980821     C                   ENDSR
164600980819     C*------------------------------------------------------------------------*
164700980819     C* AGGET8 - AGGIORNA I DATI TIPO DI CARTELLO
164800980819     C*------------------------------------------------------------------------*
164900980819     C     AGGET8        BEGSR
165000970110     C*
165100980819     C* PER OGNI LINEA DI PARTENZA MEMORIZZATA
165200971124DO  1C     1             DO        500           I
165300980820     C     I             OCCUR     FPAG
165400970110     C     I             OCCUR     FCON
165500970110     C     I             OCCUR     FRIT
165600970110     C     I             OCCUR     FSGL
165700060829     C     I             OCCUR     FSGL1
165800970110     C     I             OCCUR     FCTS
165900980819     C     I             OCCUR     FTAR
166000980819     C*
166100980819     C* SOMMA I DATI PER LE TARIFFE DI CARTELLO -ITA/EST- DI TUTTE LE FILIALI
166200980819IF  2C     SFL1(I)       IFEQ      *BLANKS                                      *FILIALE ITALIA
166300980824     C                   Z-ADD     1             U                 4 0
166400980819X   2C                   ELSE                                                   *FILIALE ESTERO
166500980819     C                   Z-ADD     2             U
166600980819E   2C                   ENDIF
166700980824     C     U             OCCUR     CCAR
166800980824     C     U             OCCUR     CPAG
166900980824     C     U             OCCUR     CCON
167000980824     C     U             OCCUR     CRIT
167100980824     C     U             OCCUR     CSGL
167200060829     C     U             OCCUR     CSGL1
167300980824     C     U             OCCUR     CCTS
167400980819     C*
167500980819     C* DATI TIPO -GENERALI-
167600980821     C                   ADD       SPE(I)        USPE(1)
167700980821     C                   ADD       CLA(I)        UCLA(1)
167800980821     C                   ADD       CLB(I)        UCLB(1)
167900980821     C                   ADD       CLC(I)        UCLC(1)
168000980821     C                   ADD       BON(I)        UBON(1)
168100980821     C                   ADD       BOA(I)        UBOA(1)
168200980821     C                   ADD       BOB(I)        UBOB(1)
168300980821     C                   ADD       NCL(I)        UNCL(1)
168400980821     C                   ADD       PKB(I)        UPKB(1)
168500980821     C                   ADD       VLB(I)        UVLB(1)
168600060829     C                   ADD       NCL1(I)       UNCL1(1)
168700060829     C                   ADD       PKB1(I)       UPKB1(1)
168800060829     C                   ADD       VLB1(I)       UVLB1(1)
168900980821     C                   ADD       FRA(I)        UFRA(1)
169000980821     C                   ADD       ASS(I)        UASS(1)
169100980821     C                   ADD       PCV(I)        UPCV(1)
169200980821     C                   ADD       PCM(I)        UPCM(1)
169300980821     C                   ADD       PFD(I)        UPFD(1)
169400980821     C                   ADD       ESP(I)        UESP(1)
169500980821     C                   ADD       PPV(I)        UPPV(1)
169600980821     C                   ADD       PCA(I)        UPCA(1)
169700980821     C                   ADD       PCP(I)        UPCP(1)
169800980821     C                   ADD       PCS(I)        UPCS(1)
169900980821     C                   ADD       PCF(I)        UPCF(1)
170000980821     C                   ADD       PCD(I)        UPCD(1)
170100980821     C                   ADD       PCC(I)        UPCC(1)
170200980821     C                   ADD       PCL(I)        UPCL(1)
170300980821     C                   ADD       PAB(I)        UPAB(1)
170400980821     C                   ADD       PAM(I)        UPAM(1)
170500980821     C                   ADD       CAS(I)        UCAS(1)
170600980821     C                   ADD       PAR(I)        UPAR(1)
170700980821     C                   ADD       PAC(I)        UPAC(1)
170800060315     C                   ADD       xx1(I)        Uxx1(1)
170900980821     C                   ADD       ASC(I)        UASC(1)
171000980821     C                   ADD       PGI(I)        UPGI(1)
171100000414     C                   ADD       GGS(I)        UGGS(1)
171200000414     C                   ADD       NRG(I)        UNRG(1)
171300980901     C                   ADD       CLT(I)        UCLT(1)
171400011011     C                   ADD       CIS(I)        UCIS(1)
171500011011     C                   ADD       RIS(I)        URIS(1)
171600020215     C                   ADD       CLD(I)        UCLD(1)
171700050808     C                   ADD       pks(I)        Upks(1)
171800050808     C                   ADD       pkn(I)        Upkn(1)
171900040802     C                   ADD       ded(I)        Uded(1)
172000040811     C                   ADD       sbnb(I)       Usbnb(1)
172100040824     C                   ADD       sban(I)       Usban(1)
172101150304     C                   ADD       nbn(I)        Unbn(1)
172102150304     C                   ADD       nba(I)        Unba(1)
172103150304     C                   ADD       sb01(I)       Usb01(1)
172104150304     C                   ADD       sb02(I)       Usb02(1)
172105150304     C                   ADD       sb03(I)       Usb03(1)
172301150304     C                   ADD       nb01(I)       Unb01(1)
172302150304     C                   ADD       nb02(I)       Unb02(1)
172303150304     C                   ADD       nb03(I)       Unb03(1)
172400040802     C                   ADD       val(I)        Uval(1)
172500980825DO  2C     1             DO        30            J
172600980825IF  3C     GGP(J)        IFGT      *ZEROS
172700980825     C                   Z-ADD     1             A                 4 0
172800980825     C                   SETOFF                                           98
172900980825     C     GGP(J)        LOOKUP    UGGP(A)                                98
173000980825IF  4C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
173100980825     C                   ADD       SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
173200980825X   4C                   ELSE                                                   *MAI MEMORIZZATI
173300980825     C                   Z-ADD     1             A
173400980825DO  5C     A             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
173500980825IF  6C     UGGP(A)       IFEQ      *ZEROS                                       *DISPONIBILE
173600980825     C                   Z-ADD     GGP(J)        UGGP(A)                        *GG
173700980825     C                   Z-ADD     SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
173800980825     C                   Z-ADD     30            A                              *ESCE DAL CICLO
173900980825E   6C                   ENDIF
174000980825     C                   ADD       1             A
174100980825E   5C                   ENDDO
174200980825E   4C                   ENDIF
174300980825E   3C                   ENDIF
174400980825E   2C                   ENDDO
174500980819DO  2C     1             DO        999           J
174600980819     C                   ADD       RIT(J)        URIT(J)
174700980819     C                   ADD       CON(J)        UCON(J)
174800980819E   2C                   ENDDO
174900980819     C*
175000040920     C* DATI TIPO -SCAGLIONI- (già caricati nella durante la lettura di
175100040920     c* titas)
175200980820     C*
175300970110     C* DATI TIPO -CODICI TASSAZIONE-
175400050103DO  2C     1             DO        600           J
175500980819     C                   ADD       TSI(J)        UTSI(J)
175600980819     C                   ADD       TSE(J)        UTSE(J)
175700980819E   2C                   ENDDO
175800970110E   1C                   ENDDO                                                  *FINE FILIALI
175900980821     C*---
176000980821     C* SCRIVE LA TARIFFA -888- ITALIA
176100980821     C*---
176200980820     C*
176300980821     C* CARICA LE SCHIERE DA ELABORARE
176400980821     C                   Z-ADD     1             U
176500980821     C                   Z-ADD     1             I
176600980821     C                   Z-ADD     888           XORG
176700980821     C                   EXSR      CARSCH
176800980821     C*
176900980821     C* DATI TIPO -GENERALI-
177000980821     C                   EXSR      SCRETC
177100980821     C*
177200980821     C* DATI TIPO -SCAGLIONI-
177300060829     c* ricavi
177400980821     C                   EXSR      SCRETS
177500060829     c* competenze
177600060829     c                   if        etcbnb <> 0
177700060829     C                   EXSR      SCRETS1
177800060829     c                   end
177900980821     C*
178000980821     C* DATI TIPO -CODICI TASSAZIONE-
178100980824     C                   MOVEL     'I'           XFIE
178200980821     C                   EXSR      SCRETT
178300980824     C*---
178400980824     C* SCRIVE LA TARIFFA -889- ESTERO
178500980824     C*---
178600980824     C*
178700980824     C* CARICA LE SCHIERE DA ELABORARE
178800980824     C                   Z-ADD     2             U
178900980824     C                   Z-ADD     1             I
179000980824     C                   Z-ADD     889           XORG
179100980824     C                   EXSR      CARSCH
179200980824     C*
179300980824     C* DATI TIPO -GENERALI-
179400980824     C                   EXSR      SCRETC
179500980824     C*
179600980824     C* DATI TIPO -SCAGLIONI-
179700060829     c* ricavi
179800980824     C                   EXSR      SCRETS
179900060829     c* competenze
180000060829     c                   if        etcbnb <> 0
180100060829     C                   EXSR      SCRETS1
180200060829     c                   end
180300980824     C*
180400980824     C* DATI TIPO -CODICI TASSAZIONE-
180500980824     C                   MOVEL     'E'           XFIE
180600980824     C                   EXSR      SCRETT
180700970110     C*
180800980819     C                   ENDSR
180900040920     C*------------------------------------------------------------------------*
181000040920     C* SRSGL8 - CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
181100040920     C*------------------------------------------------------------------------*
181200040920     C     SRSGL8        BEGSR
181300040920     C* 888 ITALIA
181400040920     C                   IF        SFL1(F) = ' '
181500040920     C                   Z-ADD     1             U
181600040920     C                   ELSE
181700040920     C                   Z-ADD     2             U
181800040920     C                   END
181900040920     C     U             OCCUR     CSGL
182000040920     C*
182100040920DO  5C     1             DO        10            I
182200040920IF  6C     TASSGL        IFLE      SGL(I)
182300040920     C                   ADD       1             USSP(I)                        *N°SPED SCAGLIONE
182400040920     C                   ADD       TASPKF        UPSC(I)                        *PESO
182500040920     C                   ADD       TASNCL        UCSC(I)                        *COLLI
182600040920     C                   ADD       $VOL          UVSC(I)                        *VOLUME
182700040920     C                   LEAVE
182800040920E   6C                   ENDIF
182900040920E   5C                   ENDDO
183000040920     C*
183100040920     C                   ENDSR
183200060829     C*------------------------------------------------------------------------*
183300060829     C* SRSGL81- CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
183400060829     C*------------------------------------------------------------------------*
183500060829     C     SRSGL81       BEGSR
183600060829     C* 888 ITALIA
183700060829     C                   IF        SFL1(F) = ' '
183800060829     C                   Z-ADD     1             U
183900060829     C                   ELSE
184000060829     C                   Z-ADD     2             U
184100060829     C                   END
184200060829     C     U             OCCUR     CSGL1
184300060829     C*
184400060829DO  5C     1             DO        10            I
184500060829IF  6C     TASSGL        IFLE      SGL(I)
184600060829     C                   ADD       1             USSP1(I)                       *N°SPED SCAGLIONE
184700060829     C                   ADD       TASPKF        UPSC1(I)                       *PESO
184800060829     C                   ADD       TASNCL        UCSC1(I)                       *COLLI
184900060829     C                   ADD       $VOL1         UVSC1(I)                       *VOLUME
185000060829     C                   LEAVE
185100060829E   6C                   ENDIF
185200060829E   5C                   ENDDO
185300060829     C*
185400060829     C                   ENDSR
185500980821     C*------------------------------------------------------------------------*
185600980821     C* CARSCH - CARICA LE SCHIERE DA ELABORARE
185700980821     C*------------------------------------------------------------------------*
185800980821     C     CARSCH        BEGSR
185900980821     C*
186000980821     C* SI POSIZIONA SULLE SCHIERE DELLA TARIFFA
186100980824     C     1             OCCUR     FPAG
186200980824     C     1             OCCUR     FCON
186300980824     C     1             OCCUR     FRIT
186400980824     C     1             OCCUR     FSGL
186500060829     C     1             OCCUR     FSGL1
186600980824     C     1             OCCUR     FCTS
186700980824     C     1             OCCUR     FTAR
186800980824     C     U             OCCUR     CCAR
186900980824     C     U             OCCUR     CPAG
187000980824     C     U             OCCUR     CCON
187100980824     C     U             OCCUR     CRIT
187200980824     C     U             OCCUR     CSGL
187300060829     C     U             OCCUR     CSGL1
187400980824     C     U             OCCUR     CCTS
187500980821     C*
187600980821     C* DATI TIPO -GENERALI-
187700980824     C                   Z-ADD     USPE(1)       SPE(1)
187800980824     C                   Z-ADD     UCLA(1)       CLA(1)
187900980824     C                   Z-ADD     UCLB(1)       CLB(1)
188000980824     C                   Z-ADD     UCLC(1)       CLC(1)
188100980824     C                   Z-ADD     UBON(1)       BON(1)
188200980824     C                   Z-ADD     UBOA(1)       BOA(1)
188300980824     C                   Z-ADD     UBOB(1)       BOB(1)
188400980824     C                   Z-ADD     UNCL(1)       NCL(1)
188500980824     C                   Z-ADD     UPKB(1)       PKB(1)
188600980824     C                   Z-ADD     UVLB(1)       VLB(1)
188700060829     C                   Z-ADD     UNCL1(1)      NCL1(1)
188800060829     C                   Z-ADD     UPKB1(1)      PKB1(1)
188900060829     C                   Z-ADD     UVLB1(1)      VLB1(1)
189000980824     C                   Z-ADD     UFRA(1)       FRA(1)
189100980824     C                   Z-ADD     UASS(1)       ASS(1)
189200980824     C                   Z-ADD     UPCV(1)       PCV(1)
189300980824     C                   Z-ADD     UPCM(1)       PCM(1)
189400980824     C                   Z-ADD     UPFD(1)       PFD(1)
189500980824     C                   Z-ADD     UESP(1)       ESP(1)
189600980824     C                   Z-ADD     UPPV(1)       PPV(1)
189700980824     C                   Z-ADD     UPCA(1)       PCA(1)
189800980824     C                   Z-ADD     UPCP(1)       PCP(1)
189900980824     C                   Z-ADD     UPCS(1)       PCS(1)
190000980824     C                   Z-ADD     UPCF(1)       PCF(1)
190100980824     C                   Z-ADD     UPCD(1)       PCD(1)
190200980824     C                   Z-ADD     UPCC(1)       PCC(1)
190300980824     C                   Z-ADD     UPCL(1)       PCL(1)
190400980824     C                   Z-ADD     UPAB(1)       PAB(1)
190500980824     C                   Z-ADD     UPAM(1)       PAM(1)
190600980824     C                   Z-ADD     UCAS(1)       CAS(1)
190700980824     C                   Z-ADD     UPAR(1)       PAR(1)
190800980824     C                   Z-ADD     UPAC(1)       PAC(1)
190900060315     C                   Z-ADD     Uxx1(1)       xx1(1)
191000980824     C                   Z-ADD     UASC(1)       ASC(1)
191100980824     C                   Z-ADD     UPGI(1)       PGI(1)
191200000414     C                   Z-ADD     UGGS(1)       GGS(1)
191300000414     C                   Z-ADD     UNRG(1)       NRG(1)
191400980901     C                   Z-ADD     UCLT(1)       CLT(1)
191500011011     C                   Z-ADD     UCIS(1)       CIS(1)
191600011011     C                   Z-ADD     URIS(1)       RIS(1)
191700020215     C                   Z-ADD     UCLD(1)       CLD(1)
191800050808     C                   Z-ADD     Upks(1)       pks(1)
191900050808     C                   Z-ADD     Upkn(1)       pkn(1)
192000040811     C                   Z-ADD     Usbnb(1)      sbnb(1)
192100040824     C                   Z-ADD     Usban(1)      sban(1)
192200040811     C                   Z-ADD     Unbn(1)       nbn(1)
192300040802     C                   Z-ADD     Unba(1)       nba(1)
192400040802     C                   Z-ADD     Uded(1)       ded(1)
192500040802     C                   Z-ADD     Uval(1)       val(1)
192501150304     C                   z-ADD     usb01(1)      sb01(1)
192502150304     C                   z-ADD     usb02(1)      sb02(1)
192503150304     C                   z-ADD     usb03(1)      sb03(1)
192504150304     C                   z-ADD     unb01(1)      nb01(1)
192505150304     C                   z-ADD     unb02(1)      nb02(1)
192506150304     C                   z-ADD     unb03(1)      nb03(1)
192600980821DO  1C     1             DO        30            J
192700980821     C                   Z-ADD     USGP(J)       SGP(J)
192800980821     C                   Z-ADD     UGGP(J)       GGP(J)
192900980821E   1C                   ENDDO
193000980821DO  1C     1             DO        999           J
193100980821     C                   Z-ADD     URIT(J)       RIT(J)
193200980821     C                   Z-ADD     UCON(J)       CON(J)
193300980821E   1C                   ENDDO
193400980821     C*
193500060829     C* DATI TIPO -SCAGLIONI- ricavi
193600980824DO  1C     1             DO        10            J
193700980825     C                   Z-ADD     USSP(J)       SSP(J)
193800980825     C                   Z-ADD     UPSC(J)       PSC(J)
193900980825     C                   Z-ADD     UCSC(J)       CSC(J)
194000980825     C                   Z-ADD     UVSC(J)       VSC(J)
194100980824E   1C                   ENDDO
194200060829     C* DATI TIPO -SCAGLIONI- competenze
194300060829DO  1C     1             DO        10            J
194400060829     C                   Z-ADD     USSP1(J)      SSP1(J)
194500060829     C                   Z-ADD     UPSC1(J)      PSC1(J)
194600060829     C                   Z-ADD     UCSC1(J)      CSC1(J)
194700060829     C                   Z-ADD     UVSC1(J)      VSC1(J)
194800060829E   1C                   ENDDO
194900980821     C*
195000980821     C* DATI TIPO -CODICI TASSAZIONE-
195100050103DO  1C     1             DO        600           J                 4 0
195200980825     C                   Z-ADD     UTSI(J)       TSI(J)
195300980825     C                   Z-ADD     UTSE(J)       TSE(J)
195400980824E   1C                   ENDDO
195500980821     C*
195600980821     C                   ENDSR
195700980821     C*------------------------------------------------------------------------*
195800980821     C* AZZSCH - AZZERA LE VARIABILI DI LAVORO
195900980821     C*------------------------------------------------------------------------*
196000980821     C     AZZSCH        BEGSR
196100980821     C*
196200980821     C                   CLEAR                   SPE                            -GENERALI-
196300980821     C                   CLEAR                   CLA
196400980821     C                   CLEAR                   CLB
196500980821     C                   CLEAR                   CLC
196600980821     C                   CLEAR                   BON
196700980821     C                   CLEAR                   BOA
196800980821     C                   CLEAR                   BOB
196900980821     C                   CLEAR                   NCL
197000980821     C                   CLEAR                   PKB
197100980821     C                   CLEAR                   VLB
197200060829     C                   CLEAR                   NCL1
197300060829     C                   CLEAR                   PKB1
197400060829     C                   CLEAR                   VLB1
197500980821     C                   CLEAR                   FRA
197600980821     C                   CLEAR                   ASS
197700980821     C                   CLEAR                   PCV
197800980821     C                   CLEAR                   PCM
197900980821     C                   CLEAR                   PFD
198000980821     C                   CLEAR                   ESP
198100980821     C                   CLEAR                   PPV
198200980821     C                   CLEAR                   PCA
198300980821     C                   CLEAR                   PCP
198400980821     C                   CLEAR                   PCS
198500980821     C                   CLEAR                   PCF
198600980821     C                   CLEAR                   PCD
198700980821     C                   CLEAR                   PCC
198800980821     C                   CLEAR                   PCL
198900980821     C                   CLEAR                   PAB
199000980821     C                   CLEAR                   PAM
199100980821     C                   CLEAR                   CAS
199200980821     C                   CLEAR                   PAR
199300980821     C                   CLEAR                   PAC
199400060315     C                   CLEAR                   xx1
199500980821     C                   CLEAR                   ASC
199600980821     C                   CLEAR                   PGI
199700980821     C                   CLEAR                   SGP
199800980821     C                   CLEAR                   GGP
199900980821     C                   CLEAR                   RIT
200000980821     C                   CLEAR                   CON
200100980901     C                   CLEAR                   CLT
200200000414     C                   CLEAR                   GGS
200300000414     C                   CLEAR                   NRG
200400980821     C                   CLEAR                   SSP                            -SCAGLIONI-
200500980821     C                   CLEAR                   PSC
200600980821     C                   CLEAR                   CSC
200700980821     C                   CLEAR                   VSC
200800060829     C                   CLEAR                   SSP1                           -SCAGLIONI-
200900060829     C                   CLEAR                   PSC1
201000060829     C                   CLEAR                   CSC1
201100060829     C                   CLEAR                   VSC1
201200980821     C                   CLEAR                   TSI                            -CODICI TASSAZIONE-
201300980821     C                   CLEAR                   TSE
201400011011     C                   CLEAR                   CIS
201500011011     C                   CLEAR                   RIS
201600020215     C                   CLEAR                   CLD
201700050808     C                   CLEAR                   pks
201800050808     C                   CLEAR                   pkn
201900040802     C                   CLEAR                   nba
201901150304     C                   CLEAR                   nb01
201902150304     C                   CLEAR                   nb02
201903150304     C                   CLEAR                   nb03
202000040824     C                   CLEAR                   sban
202001150304     C                   CLEAR                   sb01
202002150304     C                   CLEAR                   sb02
202003150304     C                   CLEAR                   sb03
202100040824     C                   CLEAR                   sbnb
202200040811     C                   CLEAR                   nbn
202300040802     C                   CLEAR                   ded
202400040802     C                   CLEAR                   val
202500980821     C*
202600980821     C                   ENDSR
202700980821     C*------------------------------------------------------------------------*
202800011001     C* SCRETC - SCRIVE UN RECORD DI TEETC -GENERALI-
202900980821     C*------------------------------------------------------------------------*
203000980821     C     SCRETC        BEGSR
203100980821     C*
203200060804     C                   MOVEL     'S'           KTCDSF
203300980821     C                   MOVEL     PARCTO        KTCCTO
203400980821     C                   Z-ADD     PARKOV        KTCKSC
203500980821     C                   Z-ADD     PARCTR        KTCCTR
203600980821     C                   Z-ADD     PARPRG        KTCPRG
203700980821     C                   Z-ADD     XORG          KTCLNP
203800021122     C     KEYETC        CHAIN     TEETC01L                           9697
203900980821     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
204000021122IF  2C     *IN97         IFEQ      *OFF                                         *NON ALLOCATO
204100021122IF  3C     *IN96         IFEQ      *ON
204200980821     C                   MOVEL     KTCDSF        ETCDSF
204300980821     C                   MOVEL     KTCCTO        ETCCTO
204400980821     C                   Z-ADD     KTCKSC        ETCKSC
204500980821     C                   Z-ADD     KTCCTR        ETCCTR
204600980821     C                   Z-ADD     KTCPRG        ETCPRG
204700980821     C                   Z-ADD     KTCLNP        ETCLNP
204800980821     C                   Z-ADD     DATCOR        ETCDIR
204900980821     C                   Z-ADD     *ZEROS        ETCDUV
205000021122     C                   MOVEL     *BLANKS       ETCATB
205100011001     C                   WRITE     TEETC000                                     *INSERIMENTO
205200011001     C                   UNLOCK    TEETC01L
205300980821X   3C                   ELSE
205400980821     C                   Z-ADD     DATCOR        ETCDUV
205500021122     C                   MOVEL     *BLANKS       ETCATB
205600011001     C                   UPDATE    TEETC000                                     *VARIAZIONE
205700980821E   3C                   ENDIF
205800980821X   2C                   ELSE                                                   *ALLOCATO
205900980821     C                   MOVEL     METX(1)       DEPERR
206000980821     C                   MOVEL     'X'           DEPTER
206100980821     C                   EXSR      STAERR
206200980821E   2C                   ENDIF
206300980821     C*
206400980821     C                   ENDSR
206500980821     C*------------------------------------------------------------------------*
206600011001     C* SCRETS - SCRIVE UN RECORD DI TEETS -SCAGLIONI-
206700980821     C*------------------------------------------------------------------------*
206800980821     C     SCRETS        BEGSR
206900980821     C*
207000980821     C                   EXSR      SCRSGL                                       *SRIVE SCHIERE SGL
207100980821DO  1C     1             DO        10            J
207200980821IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
207300060804     C                   MOVEL     'S'           KTSDSF
207400980821     C                   MOVEL     PARCTO        KTSCTO
207500980821     C                   Z-ADD     PARKOV        KTSKSC
207600980821     C                   Z-ADD     PARCTR        KTSCTR
207700980821     C                   Z-ADD     PARPRG        KTSPRG
207800980821     C                   Z-ADD     XORG          KTSLNP
207900980821     C                   Z-ADD     §SG(J)        KTSSGL
208000021122     C     KEYETS        CHAIN     TEETS01L                           96
208100980821     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
208200021122IF  4C     *IN96         IFEQ      *ON
208300980821     C                   MOVEL     KTSDSF        ETSDSF
208400980821     C                   MOVEL     KTSCTO        ETSCTO
208500980821     C                   Z-ADD     KTSKSC        ETSKSC
208600980821     C                   Z-ADD     KTSCTR        ETSCTR
208700980821     C                   Z-ADD     KTSPRG        ETSPRG
208800980821     C                   Z-ADD     KTSLNP        ETSLNP
208900980821     C                   Z-ADD     KTSSGL        ETSSGL
209000980821     C                   Z-ADD     DATCOR        ETSDIR
209100980821     C                   Z-ADD     *ZEROS        ETSDUV
209200021122     C                   MOVEL     *BLANKS       ETSATB
209300011001     C                   WRITE     TEETS000                                     *INSERIMENTO
209400011001     C                   UNLOCK    TEETS01L
209500980821X   4C                   ELSE
209600980821     C                   Z-ADD     DATCOR        ETSDUV
209700021122     C                   MOVEL     *BLANKS       ETSATB
209800011001     C                   UPDATE    TEETS000                                     *VARIAZIONE
209900980821E   4C                   ENDIF
210000980821E   2C                   ENDIF
210100980821E   1C                   ENDDO                                                  *FINE SCAGLIONI
210200980821     C*
210300980821     C                   ENDSR
210400060829     C*------------------------------------------------------------------------*
210500060829     C* SCRETS1- SCRIVE UN RECORD DI TEETS10F -SCAGLIONI-
210600060829     C*------------------------------------------------------------------------*
210700060829     C     SCRETS1       BEGSR
210800060829     C*
210900060829     C                   EXSR      SCRSGL1                                      *SRIVE SCHIERE SGL
211000060829DO  1C     1             DO        10            J
211100060829IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
211200060829     C                   MOVEL     'S'           KTSDSF
211300060829     C                   MOVEL     PARCTO        KTSCTO
211400060829     C                   Z-ADD     PARKOV        KTSKSC
211500060829     C                   Z-ADD     PARCTR        KTSCTR
211600060829     C                   Z-ADD     PARPRG        KTSPRG
211700060829     C                   Z-ADD     XORG          KTSLNP
211800060829     C                   Z-ADD     §SG(J)        KTSSGL
211900060829     C     KEYETS        CHAIN     TEETS11L                           96
212000060829     C                   EXSR      IMPETS1                                      *IMPOSTA I CAMPI
212100060829IF  4C     *IN96         IFEQ      *ON
212200060829     C                   MOVEL     KTSDSF        £ETSDSF
212300060829     C                   MOVEL     KTSCTO        £ETSCTO
212400060829     C                   Z-ADD     KTSKSC        £ETSKSC
212500060829     C                   Z-ADD     KTSCTR        £ETSCTR
212600060829     C                   Z-ADD     KTSPRG        £ETSPRG
212700060829     C                   Z-ADD     KTSLNP        £ETSLNP
212800060829     C                   Z-ADD     KTSSGL        £ETSSGL
212900060829     C                   Z-ADD     DATCOR        £ETSDIR
213000060829     C                   Z-ADD     *ZEROS        £ETSDUV
213100060829     C                   MOVEL     *BLANKS       £ETSATB
213200060829     C                   WRITE     TEETS100                                     *INSERIMENTO
213300060829     C                   UNLOCK    TEETS11L
213400060829X   4C                   ELSE
213500060829     C                   Z-ADD     DATCOR        £ETSDUV
213600060829     C                   MOVEL     *BLANKS       £ETSATB
213700060829     C                   UPDATE    TEETS100                                     *VARIAZIONE
213800060829E   4C                   ENDIF
213900060829E   2C                   ENDIF
214000060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
214100060829     C*
214200060829     C                   ENDSR
214300980821     C*------------------------------------------------------------------------*
214400011001     C* SCRETT - SCRIVE UN RECORD DI TEETT -CODICI TASSAZIONE-
214500980821     C*------------------------------------------------------------------------*
214600980821     C     SCRETT        BEGSR
214700980821     C*
214800980821     C                   EXSR      SCRCTS                                       *SRIVE SCHIERE CTS
214900050103DO  1C     1             DO        600           J                 4 0
215000980821IF  2C     §CT(J)        IFNE      *BLANKS                                      *CODICE ESISTENTE
215100060804     C                   MOVEL     'S'           KTTDSF
215200980821     C                   MOVEL     PARCTO        KTTCTO
215300980821     C                   Z-ADD     PARKOV        KTTKSC
215400980821     C                   Z-ADD     PARCTR        KTTCTR
215500980821     C                   Z-ADD     PARPRG        KTTPRG
215600980821     C                   Z-ADD     XORG          KTTLNP
215700980821     C                   MOVEL     XFIE          KTTFIE
215800980821     C                   MOVEL     §CT(J)        KTTCTS
215900021122     C     KEYETT        CHAIN     TEETT01L                           96
216000980821     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
216100021122IF  4C     *IN96         IFEQ      *ON
216200980821     C                   MOVEL     KTTDSF        ETTDSF
216300980821     C                   MOVEL     KTTCTO        ETTCTO
216400980821     C                   Z-ADD     KTTKSC        ETTKSC
216500980821     C                   Z-ADD     KTTCTR        ETTCTR
216600980821     C                   Z-ADD     KTTPRG        ETTPRG
216700980821     C                   Z-ADD     KTTLNP        ETTLNP
216800980821     C                   MOVEL     KTTFIE        ETTFIE
216900980821     C                   MOVEL     KTTCTS        ETTCTS
217000980821     C                   Z-ADD     DATCOR        ETTDIR
217100980821     C                   Z-ADD     *ZEROS        ETTDUV
217200021122     C                   MOVEL     *BLANKS       ETTATB
217300011001     C                   WRITE     TEETT000                                     *INSERIMENTO
217400011001     C                   UNLOCK    TEETT01L
217500980821X   4C                   ELSE
217600980821     C                   Z-ADD     DATCOR        ETTDUV
217700021122     C                   MOVEL     *BLANKS       ETTATB
217800011001     C                   UPDATE    TEETT000                                     *VARIAZIONE
217900980821E   4C                   ENDIF
218000980821E   2C                   ENDIF
218100980821E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
218200980821     C*
218300980821     C                   ENDSR
218400970110     C*------------------------------------------------------------------------*
218500970110     C* IMPETC - IMPOSTA I CAMPI DATI TIPO -GENERALI-
218600970110     C*------------------------------------------------------------------------*
218700970110     C     IMPETC        BEGSR
218800970110     C*---
218900970110     C* CAMPI CALCOLATI
219000970110     C*---
219100970110     C                   Z-ADD     *ZEROS        WMAX              9 0
219200980901     C                   MOVEL     'C'           ETCCLV
219300020215IF  1C     CLD(I)        IFGT      WMAX
219400020215     C                   Z-ADD     CLD(I)        WMAX
219500020215     C                   MOVEL     'D'           ETCCLV
219600020215E   1C                   ENDIF
219700980901IF  1C     CLT(I)        IFGT      WMAX
219800980901     C                   Z-ADD     CLT(I)        WMAX
219900020215     C                   MOVEL     'T'           ETCCLV
220000980901E   1C                   ENDIF
220100970110IF  1C     CLA(I)        IFGT      WMAX
220200970110     C                   Z-ADD     CLA(I)        WMAX
220300970110     C                   MOVEL     'A'           ETCCLV
220400970110E   1C                   ENDIF
220500970110IF  1C     CLB(I)        IFGT      WMAX
220600970110     C                   Z-ADD     CLB(I)        WMAX
220700970110     C                   MOVEL     'B'           ETCCLV
220800970110E   1C                   ENDIF
220900970110IF  1C     CLC(I)        IFGT      WMAX
221000970110     C                   Z-ADD     CLC(I)        WMAX
221100970110     C                   MOVEL     'C'           ETCCLV
221200970110E   1C                   ENDIF
221300050808     c* packing list
221400050808     C                   Z-ADD     *ZEROS        Wmax
221500050808     C                   MOVEL     'N'           ETCpkl
221600050808IF  1C     pks(I)        IFGT      wmax
221700050808     C                   Z-ADD     pks(I)        Wmax
221800050808     C                   MOVEL     'S'           ETCpkl
221900050808E   1C                   ENDIF
222000050808IF  1C     pkn(I)        IFGT      wmax
222100050808     C                   Z-ADD     pkn(I)        Wmax
222200050808     C                   MOVEL     'N'           ETCpkl
222300050808E   1C                   ENDIF
222400970110     C*
222500970110     C                   Z-ADD     *ZEROS        WMAX              9 0
222600970110     C                   MOVEL     ' '           ETCBOL
222700970110IF  1C     BON(I)        IFGT      WMAX
222800970110     C                   Z-ADD     BON(I)        WMAX
222900970110     C                   MOVEL     ' '           ETCBOL
223000970110E   1C                   ENDIF
223100970415IF  1C     BOA(I)        IFGT      WMAX                                         *"A" VINCE SU " "
223200970415     C     BOA(I)        ORGT      *ZEROS
223300970110     C                   Z-ADD     BOA(I)        WMAX
223400970110     C                   MOVEL     'A'           ETCBOL
223500970110E   1C                   ENDIF
223600970415IF  1C     BOB(I)        IFGT      WMAX                                         *"B" VINCE SU "A"
223700970415     C     BOB(I)        ORGT      *ZEROS
223800970110     C                   Z-ADD     BOB(I)        WMAX
223900970110     C                   MOVEL     'B'           ETCBOL
224000970110E   1C                   ENDIF
224100060829     C* ricavi
224200970110IF  1C     NCL(I)        IFGT      *ZEROS
224300970110     C     PKB(I)        DIV       NCL(I)        ETCPMC
224400970110     C     VLB(I)        DIV       NCL(I)        ETCVMC
224500970703E   1C                   ENDIF
224600970703IF  1C     ETCPMC        IFEQ      *ZEROS
224700970110     C                   Z-ADD     0,1           ETCPMC
224800970703E   1C                   ENDIF
224900970703IF  1C     ETCVMC        IFEQ      *ZEROS
225000970110     C                   Z-ADD     0,001         ETCVMC
225100970703E   1C                   ENDIF
225200060829     c* competenze
225300060829IF  1C     NCL1(I)       IFGT      *ZEROS
225400060829     C     PKB1(I)       DIV       NCL1(I)       ETCPMC1
225500060829     C     VLB1(I)       DIV       NCL1(I)       ETCVMC1
225600060829E   1C                   ENDIF
225700060829IF  1C     ETCPMC1       IFEQ      *ZEROS
225800060829     C                   Z-ADD     0,1           ETCPMC1
225900060829E   1C                   ENDIF
226000060829IF  1C     ETCVMC1       IFEQ      *ZEROS
226100060829     C                   Z-ADD     0,001         ETCVMC1
226200060829E   1C                   ENDIF
226300970110     C*
226400011026     C     FRA(I)        MULT      100           WN153            21 8
226500011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
226600970110     C                   Z-ADD     WPER          ETCFRA
226700011001     C     ASS(I)        MULT      100           WN153
226800011001     C     WN153         DIV(H)    SPE(I)        WPER
226900970110     C                   Z-ADD     WPER          ETCASS
227000980803IF  1C     ETCFRA        IFGE      ETCASS
227100980803     C                   Z-ADD     *ZEROS        ETCASS
227200980831     C                   Z-ADD     100           ETCFRA
227300980803X   1C                   ELSE
227400980803     C                   Z-ADD     *ZEROS        ETCFRA
227500980831     C                   Z-ADD     100           ETCASS
227600980803E   1C                   ENDIF
227700970110     C*
227800011001     C     PCV(I)        MULT      100           WN153
227900011001     C     WN153         DIV(H)    SPE(I)        WPER
228000970110     C                   Z-ADD     WPER          ETCPCV
228100040802     C*
228200011001     C     PCM(I)        MULT      100           WN153
228300011001     C     WN153         DIV(H)    SPE(I)        WPER
228400970110     C                   Z-ADD     WPER          ETCPCM
228500970110     C*
228600011001     C     PFD(I)        MULT      100           WN153
228700011001     C     WN153         DIV(H)    SPE(I)        WPER
228800970110     C                   Z-ADD     WPER          ETCPFD
228900970110     C*
229000011001     C     ESP(I)        MULT      100           WN153
229100011001     C     WN153         DIV(H)    SPE(I)        WPER
229200970110     C                   Z-ADD     WPER          ETCESP
229300970110     C*
229400011001     C     PPV(I)        MULT      100           WN153
229500011001     C     WN153         DIV(H)    SPE(I)        WPER
229600970110     C                   Z-ADD     WPER          ETCPPV
229700970110     C*
229800011001     C     PCA(I)        MULT      100           WN153
229900011001     C     WN153         DIV(H)    SPE(I)        WPER
230000970110     C                   Z-ADD     WPER          ETCPCA
230100970110     C*
230200011001     C     PCP(I)        MULT      100           WN153
230300011001     C     WN153         DIV(H)    SPE(I)        WPER
230400970110     C                   Z-ADD     WPER          ETCPCP
230500970110     C*
230600011001     C     PCS(I)        MULT      100           WN153
230700011001     C     WN153         DIV(H)    SPE(I)        WPER
230800970110     C                   Z-ADD     WPER          ETCPCS
230900970110     C*
231000011001     C     PCF(I)        MULT      100           WN153
231100011001     C     WN153         DIV(H)    SPE(I)        WPER
231200970110     C                   Z-ADD     WPER          ETCPCF
231300970415     C*
231400011001     C     PCD(I)        MULT      100           WN153
231500011001     C     WN153         DIV(H)    SPE(I)        WPER
231600970415     C                   Z-ADD     WPER          ETCPCD
231700970415     C*
231800011001     C     PCC(I)        MULT      100           WN153
231900011001     C     WN153         DIV(H)    SPE(I)        WPER
232000970415     C                   Z-ADD     WPER          ETCPCC
232100970415     C*
232200011001     C     PCL(I)        MULT      100           WN153
232300011001     C     WN153         DIV(H)    SPE(I)        WPER
232400970415     C                   Z-ADD     WPER          ETCPCL
232500970110     C*
232600011001     C     PAB(I)        MULT      100           WN153
232700011001     C     WN153         DIV(H)    SPE(I)        WPER
232800970110     C                   Z-ADD     WPER          ETCPAB
232900970110     C*
233000011001     C     PAM(I)        MULT      100           WN153
233100011001     C     WN153         DIV(H)    SPE(I)        WPER
233200970110     C                   Z-ADD     WPER          ETCPAM
233300011011     C*
233400011011     C     CIS(I)        MULT      100           WN153
233500011011     C     WN153         DIV(H)    SPE(I)        WPER
233600011011     C                   Z-ADD     WPER          ETCCIS
233700011011     C*
233800011011     C     RIS(I)        MULT      100           WN153
233900011011     C     WN153         DIV(H)    SPE(I)        WPER
234000011011     C                   Z-ADD     WPER          ETCSAM
234100970110     C*
234200011001     C     PAB(I)        ADD       PAM(I)        WN153
234300970110     C                   Z-ADD     *ZEROS        ETCCAS
234400011001IF  1C     WN153         IFGT      *ZEROS
234500011001     C     CAS(I)        DIV       WN153         ETCCAS
234600970110E   1C                   ENDIF
234700011026     C                   Z-ADD     ETCCAS        $IMP             21 8
234800970417     C                   MOVEL     'E'           $FDE              1
234900011011     C                   IF        PARDIV = 'ITL'
235000011026     C                   Z-ADD     1000          $ARR             21 8
235100011011     C                   ELSE
235200011026     C                   Z-ADD     1             $ARR             21 8
235300011011     C                   ENDIF
235400970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
235500970110     C                   Z-ADD     $IMP          ETCCAS
235600970110     C*
235700970418     C     PAR(I)        ADD       PAC(I)        WN90              9 0
235800060315     C                   ADD       xx1(I)        WN90
235900011001     C     PAR(I)        MULT      100           WN153
236000011001     C     WN153         DIV(H)    WN90          WPER
236100970110     C                   Z-ADD     WPER          ETCPAR
236200970110     C*
236300011001     C     PAC(I)        MULT      100           WN153
236400011001     C     WN153         DIV(H)    WN90          WPER
236500970110     C                   Z-ADD     WPER          ETCPAC
236600060315     C*
236700060315     C     xx1(I)        MULT      100           WN153
236800060315     C     WN153         DIV(H)    WN90          WPER
236900060315     C                   Z-ADD     WPER          ETCxx1
237000970110     C*
237100970110     C                   Z-ADD     *ZEROS        ETCASC
237200970110IF  1C     PAC(I)        IFGT      *ZEROS
237300970110     C     ASC(I)        DIV       PAC(I)        ETCASC
237400970110E   1C                   ENDIF
237500970110     C                   Z-ADD     ETCASC        $IMP
237600970110     C                   MOVEL     'E'           $FDE
237700011011     C                   IF        PARDIV = 'ITL'
237800011026     C                   Z-ADD     1000          $ARR             21 8
237900011011     C                   ELSE
238000011026     C                   Z-ADD     1             $ARR             21 8
238100011011     C                   ENDIF
238200970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
238300970110     C                   Z-ADD     $IMP          ETCASC
238400970110     C*
238500000414     C                   Z-ADD     *ZEROS        ETCGGG
238600000414     C                   Z-ADD     *ZEROS        ETCNRG
238700011001     C     PGI(I)        MULT      100           WN153
238800011001     C     WN153         DIV(H)    SPE(I)        WPER
238900970110     C                   Z-ADD     WPER          ETCPGI
239000021122     C* SE LA % GIACENZE SUL FILE VIENE A 0 => AZZERO ANCHE I GG GIACENZA ED IL NUMERO APERTURE GIAC
239100021122     C                   IF        ETCPGI = *ZEROS
239200021122     C                   Z-ADD     *ZEROS        ETCGGG
239300021122     C                   Z-ADD     *ZEROS        ETCNRG
239400021122     C                   ELSE
239500000414IF  1C     PGI(I)        IFGT      *ZEROS
239600000414     C     GGS(I)        DIV       PGI(I)        ETCGGG                         *GG SOSTA
239700000414IF  2C     ETCGGG        IFEQ      *ZEROS
239800000414     C                   Z-ADD     1             ETCGGG
239900000414E   2C                   ENDIF
240000000414     C     NRG(I)        DIV       PGI(I)        ETCNRG                         *N° RIAPERTURE
240100000414IF  2C     ETCNRG        IFEQ      *ZEROS
240200000414     C                   Z-ADD     1             ETCNRG
240300000414E   2C                   ENDIF
240400000414E   1C                   ENDIF
240500021122     C                   ENDIF
240600970110     C*
240700970110     C                   Z-ADD     *ZEROS        WFRQ              3 0
240800011001     C                   Z-ADD     *ZEROS        WN153
240900970110DO  1C     1             DO        30            J
241000011001IF  2C     SGP(J)        IFGT      WN153
241100011001     C                   Z-ADD     SGP(J)        WN153
241200970110     C                   Z-ADD     GGP(J)        WFRQ
241300970110E   2C                   ENDIF
241400970110E   1C                   ENDDO
241500970110     C                   Z-ADD     WFRQ          ETCGGP
241600970110IF  1C     ETCGGP        IFEQ      999
241700970110     C                   Z-ADD     *ZEROS        ETCGGP
241800970110E   1C                   ENDIF
241900970110     C*
242000970110     C                   Z-ADD     *ZEROS        WSPE             11 0
242100000411     C                   Z-ADD     *ZEROS        WSTP             13 0
242200970110DO  1C     1             DO        999           J
242300970110IF  2C     RIT(J)        IFGT      *ZEROS
242400000411     C     RIT(J)        DIV       J             WN13             13 0
242500000411     C                   MVR                     RESTO            15 0
242600000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
242700000411     C     J             ANDNE     999                                          *STOP <> 999
242800000411     C                   ADD       1             WN13
242900000411E   3C                   ENDIF
243000970110     C     WSPE          ADD       RIT(J)        WSPE
243100970110     C     WSTP          ADD       WN13          WSTP
243200970110E   2C                   ENDIF
243300970110E   1C                   ENDDO
243400970110     C                   Z-ADD     *ZEROS        ETCSPR
243500970110IF  1C     WSTP          IFGT      *ZEROS
243600970110     C     WSPE          DIV       WSTP          ETCSPR
243700000411     C                   MVR                     RESTO            15 0
243800000411IF  2C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
243900000411     C                   ADD       1             ETCSPR                         *ARROTONDA X ECCESSO
244000000411E   2C                   ENDIF
244100000411E   1C                   ENDIF
244200970110IF  1C     ETCSPR        IFEQ      *ZEROS
244300970110     C                   Z-ADD     1             ETCSPR
244400970110E   1C                   ENDIF
244500970110     C*
244600970110     C                   Z-ADD     *ZEROS        WSPE             11 0
244700000411     C                   Z-ADD     *ZEROS        WSTP             13 0
244800970110DO  1C     1             DO        999           J
244900970110IF  2C     CON(J)        IFGT      *ZEROS
245000000411     C     CON(J)        DIV       J             WN13             13 0
245100000411     C                   MVR                     RESTO            15 0
245200000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
245300000411     C     J             ANDNE     999                                          *STOP <> 999
245400000411     C                   ADD       1             WN13
245500000411E   3C                   ENDIF
245600970110     C     WSPE          ADD       CON(J)        WSPE
245700970110     C     WSTP          ADD       WN13          WSTP
245800970110E   2C                   ENDIF
245900970110E   1C                   ENDDO
246000001221     C                   Z-ADD     1             ETCSPC
246100970110     C*---
246200970110     C* CAMPI FISSI
246300970110     C*---
246400970110     C                   MOVEL     *BLANKS       ETCSDE
246500970110     C                   Z-ADD     *ZEROS        ETCPDE
246600970619     C                   MOVEL     'E'           ETCFAR
246700011011     C                   IF        PARDIV = 'ITL'
246800970314     C                   Z-ADD     100           ETCIAR
246900011011     C                   ELSE
247000011030     C                   Z-ADD     0,1           ETCIAR
247100011011     C                   ENDIF
247200970213     C                   MOVEL     'N'           ETCSIN
247300970110     C                   MOVEL     'N'           ETCIAP
247400970110     C                   MOVEL     'N'           ETCIPI
247500970110     C                   MOVEL     'N'           ETCISU
247600970110     C                   MOVEL     'N'           ETCIFU
247700970415     C                   MOVEL     'N'           ETCIDD
247800970415     C                   MOVEL     'N'           ETCICE
247900970415     C                   MOVEL     'N'           ETCILO
248000050810     C                   MOVEL     'N'           ETCIde
248100050810     C                   MOVEL     'N'           ETCIba
248200970110     C                   MOVEL     'N'           ETCIES
248300970110     C                   MOVEL     'N'           ETCICA
248400970110     C                   MOVEL     'N'           ETCIAS
248500970110     C                   MOVEL     'N'           ETCIGI
248600020716     C                   MOVEL     'N'           ETCISO
248700020716     C                   MOVEL     'N'           ETCINR
248800980901     C                   MOVEL     METX(3)       ETCDCV
248900970110     C*
249000970110     C                   MOVEL     *BLANKS       ETCFTR
249100970110     C                   Z-ADD     *ZEROS        ETCDTR
249200970110     C                   MOVEL     *BLANKS       ETCATB
249300021104     C*
249400021104     C* EFFETTUO ALCUNE CONSIDERAZIONI
249500021104     C                   IF        ETCBOL = 'B'
249600021104     C                   EVAL      ETCIX1 = 'K'
249700021104     C                   ELSE
249800021104     C                   EVAL      ETCIX1 = *blanks
249900021104     C                   ENDIF
250000040811     C* %sped oltre 20 colli non imbancalate
250100040811     C     sbnb(I)       MULT      100           WN153
250200040802     C     WN153         DIV(H)    SPE(I)        WPER
250300040802     C                   Z-ADD     WPER          ETCbnb
250400040811     c* numero medio bancali x spedizioni oltre 20 colli
250500061006     c                   if        sbnb(i) <> 0 and etcbnb <> 0
250600040811     c     nbn(i)        div(h)    sbnb(i)       etcnbn
250700061006     c                   if        etcnbn = 0
250800061006     c                   eval      etcnbn = 1
250900061006     c                   end
251000040812     c                   else
251100040812     c                   clear                   etcnbn
251200040811     c                   end
251300150304     C* %sped varia B
251400040824     C     sban(I)       MULT      100           WN153
251500040824     C     WN153         DIV(H)    SPE(I)        WPER
251600040824     C                   Z-ADD     WPER          ETCban
251700040824     c* numero medio bancali Bartolini
251800061006     c                   if        sban(i) <> 0 and etcban <> 0
251900040824     c     nba(i)        div(h)    sban(i)       etcnba
252000061006     c                   if        etcnba = 0
252100061006     c                   eval      etcnba = 1
252200061006     c                   end
252501150304     c* come tipo bancale metto quello usato maggiormente nelle spedizoni
252502150304     c                   eval      §etctba ='01'
252503150304     c                   if        sb02(i) > sb01(i) and
252504150304     c                             sb02(i) > sb03(i)
252505150304     c                   eval      §etctba ='02'
252506150304     c                   end
252507150304     c                   if        sb03(i) > sb01(i) and
252508150304     c                             sb03(i) > sb02(i)
252509150304     c                   eval      §etctba ='03'
252510150304     c                   end
252511150304     c                   movel(p)  detcflo       etcflo
252512150305     c                   else
252514150305     c                   clear                   etcnba
252515150305     c                   clear                   §etctba
252516150305     c                   end
252517150305     c                   movel(p)  detcflo       etcflo
252600040802     C*
252700040802     C     ded(I)        MULT      100           WN153
252800040802     C     WN153         DIV(H)    SPE(I)        WPER
252900040802     C                   Z-ADD     WPER          ETCdes
253000040802     C*
253100040802     C     val(I)        MULT      100           WN153
253200040802     C     WN153         DIV(H)    SPE(I)        WPER
253300040802     C                   Z-ADD     WPER          ETCval
253400970110     C*
253500970110     C                   ENDSR
253600970416     C*------------------------------------------------------------------------*
253700060829     C* SCRSGL - SCRIVE SCHIERE SCAGLIONI ricavi
253800970416     C*------------------------------------------------------------------------*
253900970416     C     SCRSGL        BEGSR
254000970416     C*
254100970416     C* AZZERA LE VARIABILI DI LAVORO
254200970416     C                   CLEAR                   §SG
254300970416     C                   CLEAR                   §PS
254400970416     C                   CLEAR                   §CS
254500970416     C                   CLEAR                   §VS
254600970416     C                   CLEAR                   §SP
254700971008     C                   CLEAR                   §SD
254800970416     C*
254900970416     C* PER CIASCUN SCAGLIONE DA SCRIVERE
255000970416DO  1C     1             DO        10            J
255100970416IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
255200970416     C                   Z-ADD     SSGL(J)       §SG(J)
255300970416IF  3C     SSP(J)        IFGT      *ZEROS
255400970416     C*
255500970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
255600971008     C                   MOVEL     *BLANKS       §SD(J)                         *VALORE NON DEDOTTO
255700970416     C     PSC(J)        DIV       SSP(J)        §PS(J)
255800970416     C     CSC(J)        DIV       SSP(J)        §CS(J)
255900970416     C     VSC(J)        DIV       SSP(J)        §VS(J)
256000011001     C     SSP(J)        MULT      100           WN153
256100011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
256200970416     C                   Z-ADD     WPER          §SP(J)
256300970703IF  4C     §SP(J)        IFEQ      *ZEROS
256400970703     C                   Z-ADD     0,1           §SP(J)
256500970703E   4C                   ENDIF
256600970416X   3C                   ELSE
256700970416     C*
256800970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
256900971008     C                   MOVEL     '*'           §SD(J)                         *VALORE DEDOTTO
257000970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
257100970416     C     PARTTR        OREQ      '3'                                          *KG
257200970416     C                   Z-ADD     §SG(J)        §PS(J)
257300970416     C     §PS(J)        DIV       ETCPMC        §CS(J)
257400970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
257500970416X   4C                   ELSE                                                   *COLLI
257600970416     C                   Z-ADD     §SG(J)        §CS(J)
257700040812     C     §CS(J)        mult      ETCPMC        §PS(J)
257800970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
257900970416E   4C                   ENDIF
258000970416     C                   Z-ADD     0,1           §SP(J)
258100970416E   3C                   ENDIF
258200970416E   2C                   ENDIF
258300970416E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
258400000707     C*
258500000707     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
258600000707     C                   EXSR      PARPSG
258700970416     C*
258800970416     C                   ENDSR
258900060829     C*------------------------------------------------------------------------*
259000060829     C* SCRSGL1- SCRIVE SCHIERE SCAGLIONI competenze
259100060829     C*------------------------------------------------------------------------*
259200060829     C     SCRSGL1       BEGSR
259300060829     C*
259400060829     C* AZZERA LE VARIABILI DI LAVORO
259500060829     C                   CLEAR                   §SG
259600060829     C                   CLEAR                   §PS1
259700060829     C                   CLEAR                   §CS1
259800060829     C                   CLEAR                   §VS1
259900060829     C                   CLEAR                   §SP1
260000060829     C                   CLEAR                   §SD1
260100060829     C*
260200060829     C* PER CIASCUN SCAGLIONE DA SCRIVERE
260300060829DO  1C     1             DO        10            J
260400060829IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
260500060829     C                   Z-ADD     SSGL(J)       §SG(J)
260600060829IF  3C     SSP1(J)       IFGT      *ZEROS
260700060829     C*
260800060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
260900060829     C                   MOVEL     *BLANKS       §SD1(J)                        *VALORE NON DEDOTTO
261000060829     C     PSC1(J)       DIV       SSP1(J)       §PS1(J)
261100060829     C     CSC1(J)       DIV       SSP1(J)       §CS1(J)
261200060829     C     VSC1(J)       DIV       SSP1(J)       §VS1(J)
261300060829     C     SSP1(J)       MULT      100           WN153
261400060829     C     WN153         DIV(H)    SPE(I)        WPER              4 1
261500060829     C                   Z-ADD     WPER          §SP1(J)
261600060829IF  4C     §SP1(J)       IFEQ      *ZEROS
261700060829     C                   Z-ADD     0,1           §SP1(J)
261800060829E   4C                   ENDIF
261900060829X   3C                   ELSE
262000060829     C*
262100060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
262200060829     C                   MOVEL     '*'           §SD1(J)                        *VALORE DEDOTTO
262300060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
262400060829     C     PARTTR        OREQ      '3'                                          *KG
262500060829     C                   Z-ADD     §SG(J)        §PS1(J)
262600060829     C     §PS1(J)       DIV       ETCPMC1       §CS1(J)
262700060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
262800060829X   4C                   ELSE                                                   *COLLI
262900060829     C                   Z-ADD     §SG(J)        §CS1(J)
263000060829     C     §CS1(J)       mult      ETCPMC1       §PS1(J)
263100060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
263200060829E   4C                   ENDIF
263300060829     C                   Z-ADD     0,1           §SP1(J)
263400060829E   3C                   ENDIF
263500060829E   2C                   ENDIF
263600060829E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
263700060829     C*
263800060829     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
263900060829     C                   EXSR      PARPSG1
264000060829     C*
264100060829     C                   ENDSR
264200000707     C*------------------------------------------------------------------------*
264300000707     C* PARPSG - PAREGGIA LA %TOTALE -SCAGLIONI- A 100
264400000707     C*------------------------------------------------------------------------*
264500000707     C     PARPSG        BEGSR
264600000707     C*
264700000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
264800000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
264900000707DO  1C     1             DO        10            J
265000000707IF  2C     §SG(J)        IFGT      *ZEROS
265100000707     C                   ADD       §SP(J)        TOTPPE                         *%INCIDENZA TOTALE
265200000707E   2C                   ENDIF
265300000707E   1C                   ENDDO
265400000707     C*
265500000707     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
265600000707     C                   Z-ADD     *ZEROS        WN41              4 1
265700000707     C     100           SUB       TOTPPE        WN41
265800000707     C*
265900000707     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
266000000707IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
266100000707DO  2C     WN41          DOWNE     *ZEROS
266200000707DO  3C     1             DO        10            J
266300000707IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
266400000707     C* POSITIVA -> AGGIUNGERE
266500000707IF  5C     WN41          IFGT      *ZEROS
266600000707     C     §SP(J)        ADD       0,1           §SP(J)                         *AGGIUNGE %
266700000707     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
266800000707E   5C                   ENDIF
266900000707     C* NEGATIVA -> TOGLIERE
267000000707IF  5C     WN41          IFLT      *ZEROS
267100000707IF  6C     §SP(J)        IFGT      0,1
267200000707     C     §SP(J)        SUB       0,1           §SP(J)                         *SOTTRAE %
267300000707     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
267400000707E   6C                   ENDIF
267500000707E   5C                   ENDIF
267600000707     C* UGUALE A ZERO -> USCIRE
267700000707IF  5C     WN41          IFEQ      *ZEROS
267800000707     C                   LEAVE
267900000707E   5C                   ENDIF
268000000707E   4C                   ENDIF
268100000707E   3C                   ENDDO                                                  *FINE SCAGLIONI
268200000707E   2C                   ENDDO                                                  *DIFFERENZA = 0
268300000707E   1C                   ENDIF
268400000707     C*
268500000707     C                   ENDSR
268600060829     C*------------------------------------------------------------------------*
268700060829     C* PARPSG1- PAREGGIA LA %TOTALE -SCAGLIONI- A 100
268800060829     C*------------------------------------------------------------------------*
268900060829     C     PARPSG1       BEGSR
269000060829     C*
269100060829     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
269200060829     C                   Z-ADD     *ZEROS        TOTPPE            4 1
269300060829DO  1C     1             DO        10            J
269400060829IF  2C     §SG(J)        IFGT      *ZEROS
269500060829     C                   ADD       §SP1(J)       TOTPPE                         *%INCIDENZA TOTALE
269600060829E   2C                   ENDIF
269700060829E   1C                   ENDDO
269800060829     C*
269900060829     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
270000060829     C                   Z-ADD     *ZEROS        WN41              4 1
270100060829     C     100           SUB       TOTPPE        WN41
270200060829     C*
270300060829     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
270400060829IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
270500060829DO  2C     WN41          DOWNE     *ZEROS
270600060829DO  3C     1             DO        10            J
270700060829IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
270800060829     C* POSITIVA -> AGGIUNGERE
270900060829IF  5C     WN41          IFGT      *ZEROS
271000060829     C     §SP1(J)       ADD       0,1           §SP1(J)                        *AGGIUNGE %
271100060829     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
271200060829E   5C                   ENDIF
271300060829     C* NEGATIVA -> TOGLIERE
271400060829IF  5C     WN41          IFLT      *ZEROS
271500060829IF  6C     §SP1(J)       IFGT      0,1
271600060829     C     §SP1(J)       SUB       0,1           §SP1(J)                        *SOTTRAE %
271700060829     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
271800060829E   6C                   ENDIF
271900060829E   5C                   ENDIF
272000060829     C* UGUALE A ZERO -> USCIRE
272100060829IF  5C     WN41          IFEQ      *ZEROS
272200060829     C                   LEAVE
272300060829E   5C                   ENDIF
272400060829E   4C                   ENDIF
272500060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
272600060829E   2C                   ENDDO                                                  *DIFFERENZA = 0
272700060829E   1C                   ENDIF
272800060829     C*
272900060829     C                   ENDSR
273000970110     C*------------------------------------------------------------------------*
273100060829     C* IMPETS - IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- ricavi
273200970110     C*------------------------------------------------------------------------*
273300970318     C     IMPETS        BEGSR
273400970110     C*---
273500970110     C* CAMPI CALCOLATI
273600970110     C*---
273700970416     C                   Z-ADD     §PS(J)        ETSPSC
273800970416     C                   Z-ADD     §CS(J)        ETSCSC
273900981020IF  1C     ETSCSC        IFLT      1
274000981020     C                   Z-ADD     1             ETSCSC
274100981020E   1C                   ENDIF
274200970416     C                   Z-ADD     §VS(J)        ETSVSC
274300970416     C                   Z-ADD     §SP(J)        ETSPPE
274400970415     C*---
274500970415     C* CAMPI FISSI
274600970415     C*---
274700980325     C                   Z-ADD     *ZEROS        ETSINL
274800970415     C                   Z-ADD     *ZEROS        ETSRPV
274900970415     C                   MOVEL     *BLANKS       ETSSDE
275000970415     C                   Z-ADD     *ZEROS        ETSPDE
275100970110     C*
275200040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
275300040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
275400040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
275500040805     C* MA È GIUSTO COSÌ
275600040805     C                   MOVEL     §SD(J)        ETSFTR
275700970110     C                   Z-ADD     *ZEROS        ETSDTR
275800970110     C                   MOVEL     *BLANKS       ETSATB
275900970110     C*
276000970110     C                   ENDSR
276100060829     C*------------------------------------------------------------------------*
276200060829     C* IMPETS1- IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- competenze
276300060829     C*------------------------------------------------------------------------*
276400060829     C     IMPETS1       BEGSR
276500060829     C*---
276600060829     C* CAMPI CALCOLATI
276700060829     C*---
276800060829     C                   Z-ADD     §PS1(J)       £ETSPSC
276900060829     C                   Z-ADD     §CS1(J)       £ETSCSC
277000060829IF  1C     £ETSCSC       IFLT      1
277100060829     C                   Z-ADD     1             £ETSCSC
277200060829E   1C                   ENDIF
277300060829     C                   Z-ADD     §VS1(J)       £ETSVSC
277400060829     C                   Z-ADD     §SP1(J)       £ETSPPE
277500060829     C*---
277600060829     C* CAMPI FISSI
277700060829     C*---
277800060829     C                   Z-ADD     *ZEROS        £ETSINL
277900060829     C                   Z-ADD     *ZEROS        £ETSRPV
278000060829     C                   MOVEL     *BLANKS       £ETSSDE
278100060829     C                   Z-ADD     *ZEROS        £ETSPDE
278200060829     C*
278300060829     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
278400060829     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
278500060829     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
278600060829     C* MA È GIUSTO COSÌ
278700060829     C                   MOVEL     §SD1(J)       £ETSFTR
278800060829     C                   Z-ADD     *ZEROS        £ETSDTR
278900060829     C                   MOVEL     *BLANKS       £ETSATB
279000060829     C*
279100060829     C                   ENDSR
279200970416     C*------------------------------------------------------------------------*
279300970416     C* SCRCTS - SCRIVE SCHIERE CODICI TASSAZIONE
279400970416     C*------------------------------------------------------------------------*
279500970416     C     SCRCTS        BEGSR
279600000707     C*---
279700000707     C* ITALIA
279800000707     C*---
279900000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
280000000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
280100000707     C                   CLEAR                   §RC
280200000707     C                   CLEAR                   §CP
280300000707     C                   CLEAR                   §CD
280400980821IF  1C     XFIE          IFEQ      'I'
280500970423     C                   XFOOT     TSI           TOTSPE           11 0          *TOT. SPED. ITALIA
280600050103DO  2C     1             DO        600           J
280700970423IF  3C     CCTI(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
280800000620     C     CRCI(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
280900970423     C                   MOVEL     CCTI(J)       §CT(J)
281000970423     C                   MOVEL     CRCI(J)       §RC(J)
281100970417     C*
281200970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
281300971008     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
281400970423IF  4C     TSI(J)        IFGT      *ZEROS
281500011001     C     TSI(J)        MULT      100           WN153
281600011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
281700970417     C                   Z-ADD     WPER          §CP(J)
281800970703IF  5C     §CP(J)        IFEQ      *ZEROS
281900970703     C                   Z-ADD     0,1           §CP(J)
282000970703E   5C                   ENDIF
282100970423X   4C                   ELSE
282200970417     C*
282300970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
282400971008     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
282500970417     C                   Z-ADD     0,1           §CP(J)
282600970423E   4C                   ENDIF
282700970423E   3C                   ENDIF
282800970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
282900000707     C*
283000000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
283100000707     C                   EXSR      PARPCT
283200970423     C*---
283300970423     C* ESTERO
283400970423     C*---
283500000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
283600970423X   1C                   ELSE
283700000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
283800000707     C                   CLEAR                   §RC
283900000707     C                   CLEAR                   §CP
284000000707     C                   CLEAR                   §CD
284100970423     C                   XFOOT     TSE           TOTSPE           11 0          *TOT. SPED. ITALIA
284200050103DO  2C     1             DO        600           J
284300970423IF  3C     CCTE(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
284400000620     C     CRCE(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
284500970423     C                   MOVEL     CCTE(J)       §CT(J)
284600970423     C                   MOVEL     CRCE(J)       §RC(J)
284700970423     C*
284800970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
284900990803     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
285000970423IF  4C     TSE(J)        IFGT      *ZEROS
285100011001     C     TSE(J)        MULT      100           WN153
285200011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
285300970423     C                   Z-ADD     WPER          §CP(J)
285400970703IF  5C     §CP(J)        IFEQ      *ZEROS
285500970703     C                   Z-ADD     0,1           §CP(J)
285600970703E   5C                   ENDIF
285700970423X   4C                   ELSE
285800970423     C*
285900970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
286000990803     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
286100970423     C                   Z-ADD     0,1           §CP(J)
286200970423E   4C                   ENDIF
286300970423E   3C                   ENDIF
286400970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
286500000707     C*
286600000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
286700000707     C                   EXSR      PARPCT
286800000707E   1C                   ENDIF
286900000707     C*
287000000707     C                   ENDSR
287100000707     C*------------------------------------------------------------------------*
287200000707     C* PARPCT - PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
287300000707     C*------------------------------------------------------------------------*
287400000707     C     PARPCT        BEGSR
287500000707     C*
287600000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
287700000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
287800050103DO  1C     1             DO        600           J
287900000707IF  2C     §CT(J)        IFNE      *BLANKS
288000000707     C                   ADD       §CP(J)        TOTPPE                         *%INCIDENZA TOTALE
288100000707E   2C                   ENDIF
288200000707E   1C                   ENDDO
288300970417     C*
288400970417     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
288500970417     C                   Z-ADD     *ZEROS        WN41              4 1
288600970417     C     100           SUB       TOTPPE        WN41
288700970417     C*
288800970417     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
288900970417IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
289000000707DO  2C     WN41          DOWNE     *ZEROS
289100050103DO  3C     1             DO        600           J
289200970421IF  4C     §CT(J)        IFNE      *BLANKS                                      *ELEMENTO PIENO
289300970417     C     §RC(J)        ANDNE     *BLANKS                                      *DA CALCOLARE
289400970417     C* POSITIVA -> AGGIUNGERE
289500970417IF  5C     WN41          IFGT      *ZEROS
289600970417     C     §CP(J)        ADD       0,1           §CP(J)                         *AGGIUNGE %
289700970417     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
289800970417E   5C                   ENDIF
289900970417     C* NEGATIVA -> TOGLIERE
290000970417IF  5C     WN41          IFLT      *ZEROS
290100000707IF  6C     §CP(J)        IFGT      0,1
290200970417     C     §CP(J)        SUB       0,1           §CP(J)                         *SOTTRAE %
290300970417     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
290400000707E   6C                   ENDIF
290500970417E   5C                   ENDIF
290600000707     C* UGUALE A ZERO -> USCIRE
290700000707IF  5C     WN41          IFEQ      *ZEROS
290800000707     C                   LEAVE
290900000707E   5C                   ENDIF
291000970417E   4C                   ENDIF
291100970417E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
291200970417E   2C                   ENDDO                                                  *DIFFERENZA = 0
291300970417E   1C                   ENDIF
291400970416     C*
291500970416     C                   ENDSR
291600970110     C*------------------------------------------------------------------------*
291700970110     C* IMPETT - IMPOSTA I CAMPI DATI TIPO -CODICI TASSAZIONE-
291800970110     C*------------------------------------------------------------------------*
291900970110     C     IMPETT        BEGSR
292000970110     C*---
292100970110     C* CAMPI CALCOLATI
292200970110     C*---
292300970417     C                   MOVEL     §RC(J)        ETTRCT
292400970417     C                   Z-ADD     §CP(J)        ETTPPE
292500970110     C*---
292600970110     C* CAMPI FISSI
292700970110     C*---
292800040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
292900040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
293000040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
293100040805     C* MA È GIUSTO COSÌ
293200040805     C                   MOVEL     §CD(J)        ETTFTR
293300970110     C                   Z-ADD     *ZEROS        ETTDTR
293400970110     C                   MOVEL     *BLANKS       ETTATB
293500970110     C*
293600970110     C                   ENDSR
293700970417     C*------------------------------------------------------------------------*
293800970417     C* ARRIMP - ARROTONDA IMPORTO
293900970417     C*------------------------------------------------------------------------*
294000970417     C     ARRIMP        BEGSR
294100970417     C*---
294200970417     C* $IMP -> IMPORTO DA ARROTONDARE
294300970417     C* $FDE -> 'D'=DIFETTO / E=ECCESSO
294400970417     C* $ARR  : IMPORTO AL QUALE ARROTONDARE
294500970417     C*---
294600970417IF  1C     $IMP          IFGT      *ZEROS                                       *IMPORTO PIENO
294700970417IF  2C     $ARR          IFGT      *ZEROS                                       *SI ARROTONDAMENTO
294800970417     C     $IMP          DIV       $ARR          COMODO           15 0
294900970417     C                   MVR                     RESTO            15 0
295000970417IF  3C     $FDE          IFEQ      'E'
295100970417IF  4C     RESTO         IFGT      *ZEROS
295200970417     C                   ADD       1             COMODO
295300970417E   4C                   ENDIF
295400970417E   3C                   ENDIF
295500970417     C     COMODO        MULT      $ARR          $IMP
295600970417E   2C                   ENDIF
295700970417E   1C                   ENDIF
295800970417     C*
295900970417     C                   ENDSR
296000970110     C*------------------------------------------------------------------------*
296100970110     C* STAERR - STAMPA RIGA DI ERRORE
296200970110     C*------------------------------------------------------------------------*
296300970110     C     STAERR        BEGSR
296400970110     C*
296500970110IF  1C     *INOF         IFEQ      *ON
296600970110     C                   EXCEPT    TES
296700970110     C                   SETOFF                                       OF
296800970110E   1C                   ENDIF
296900970110IF  1C     DEPTER        IFEQ      'T'
297000970110     C                   EXCEPT    ETAS
297100970110E   1C                   ENDIF
297200970110IF  1C     DEPTER        IFEQ      'X'
297300970110     C                   EXCEPT    EETX
297400970110E   1C                   ENDIF
297500970110     C*
297600970110     C                   ENDSR
297700970313     C*------------------------------------------------------------------------*
297800970314     C* ANNETX - ANNULLA I DATI TIPO ESISTENTI
297900970313     C*------------------------------------------------------------------------*
298000970314     C     ANNETX        BEGSR
298100970313     C*
298200970314     C* PER OGNI FILIALE RICHIESTA
298300971124DO  1C     1             DO        500           I
298400970313IF  2C     SORG(I)       IFGT      *ZEROS
298500970313     C*---
298600970313     C* DATI TIPO -GENERALI-
298700970313     C*---
298800060804     C                   MOVEL     'S'           KTCDSF
298900980713     C                   MOVEL     PARCTO        KTCCTO
299000970313     C                   Z-ADD     PARKOV        KTCKSC
299100970313     C                   Z-ADD     PARCTR        KTCCTR
299200970313     C                   Z-ADD     PARPRG        KTCPRG
299300970313     C                   Z-ADD     SORG(I)       KTCLNP
299400011001     C     KEYETC        CHAIN     TEETC01L                           99
299500981221IF  3C     *IN99         IFEQ      *OFF
299600971024IF  4C     ETCATB        IFEQ      *BLANKS
299700970314     C                   MOVEL     'A'           ETCATB
299800970314     C                   MOVEL     *BLANKS       ETCFTR
299900011001     C                   UPDATE    TEETC000
300000970314E   4C                   ENDIF
300100970314E   3C                   ENDIF
300200970313     C*---
300300970313     C* DATI TIPO -SCAGLIONI-
300400970313     C*---
300500060829     c* ricavi
300600980713     C                   MOVEL     'S'           KTSDSF
300700980713     C                   MOVEL     PARCTO        KTSCTO
300800970313     C                   Z-ADD     PARKOV        KTSKSC
300900970313     C                   Z-ADD     PARCTR        KTSCTR
301000970313     C                   Z-ADD     PARPRG        KTSPRG
301100970313     C                   Z-ADD     SORG(I)       KTSLNP
301200011001     C     KE2ETS        SETLL     TEETS01L
301300011001     C     KE2ETS        READE     TEETS01L                               99
301400970314DO  3C     *IN99         DOWEQ     *OFF
301500971024IF  4C     ETSATB        IFEQ      *BLANKS
301600970314     C                   MOVEL     'A'           ETSATB
301700970314     C                   MOVEL     *BLANKS       ETSFTR
301800011001     C                   UPDATE    TEETS000
301900970314E   4C                   ENDIF
302000011001     C     KE2ETS        READE     TEETS01L                               99
302100060829E   3c                   ENDDO                                                  *FINE SCAGLIONI
302200060829     c* competetnze
302300060829     C     KE2ETS        SETLL     TEETS11L
302400060829     C     KE2ETS        READE     TEETS11L                               99
302500060829DO  3C     *IN99         DOWEQ     *OFF
302600060829IF  4C     £ETSATB       IFEQ      *BLANKS
302700060829     C                   MOVEL     'A'           £ETSATB
302800060829     C                   MOVEL     *BLANKS       £ETSFTR
302900060829     C                   UPDATE    TEETS100
303000060829E   4C                   ENDIF
303100060829     C     KE2ETS        READE     TEETS11L                               99
303200060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
303300970314     C*---
303400970314     C* DATI TIPO -CODICI TASSAZIONE-
303500970314     C*---
303600970429DO  3C     1             DO        2             K
303700970429IF  4C     SFIE(K)       IFNE      *BLANKS
303800980713     C                   MOVEL     'S'           KTTDSF
303900980713     C                   MOVEL     PARCTO        KTTCTO
304000970314     C                   Z-ADD     PARKOV        KTTKSC
304100970314     C                   Z-ADD     PARCTR        KTTCTR
304200970314     C                   Z-ADD     PARPRG        KTTPRG
304300970314     C                   Z-ADD     SORG(I)       KTTLNP
304400970429     C                   MOVEL     SFIE(K)       KTTFIE
304500011001     C     KE2ETT        SETLL     TEETT01L
304600011001     C     KE2ETT        READE     TEETT01L                               99
304700970429DO  5C     *IN99         DOWEQ     *OFF
304800971024IF  6C     ETTATB        IFEQ      *BLANKS
304900970314     C                   MOVEL     'A'           ETTATB
305000970314     C                   MOVEL     *BLANKS       ETTFTR
305100011001     C                   UPDATE    TEETT000
305200970429E   6C                   ENDIF
305300011001     C     KE2ETT        READE     TEETT01L                               99
305400970429E   5C                   ENDDO                                                  *FINE COD.TASSAZIONE
305500970429E   4C                   ENDIF
305600970429E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
305700970313     C*
305800970314E   2C                   ENDIF
305900970314E   1C                   ENDDO                                                  *FINE FILIALI
306000970314     C*
306100970313     C                   ENDSR
306200970110     C*------------------------------------------------------------------------*
306300970110     C* OPEFIL - APRE GLI ARCHIVI
306400970110     C*------------------------------------------------------------------------*
306500970110     C     OPEFIL        BEGSR
306600970110     C*
306700970110IF  1C     PARKSC        IFGT      *ZEROS
306800990812     C                   OPEN      TITAS31C                                     *LETTURA X CLIENTE
306900970110X   1C                   ELSE
307000990812     C                   OPEN      TITAS38C                                     *LETTURA X DATA
307100970110E   1C                   ENDIF
307200970110     C*
307300970110     C                   ENDSR
307400970110     C*------------------------------------------------------------------------*
307500970110     C* CLOFIL - CHIUDE GLI ARCHIVI
307600970110     C*------------------------------------------------------------------------*
307700970110     C     CLOFIL        BEGSR
307800970110     C*
307900970110IF  1C     PARKSC        IFGT      *ZEROS
308000990812     C                   CLOSE     TITAS31C                                     *LETTURA X CLIENTE
308100970110X   1C                   ELSE
308200990812     C                   CLOSE     TITAS38C                                     *LETTURA X DATA
308300970110E   1C                   ENDIF
308400970110     C*
308500970110     C                   ENDSR
308600011011     C*------------------------------------------------------------------------*
308700011011     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
308800011011     C*------------------------------------------------------------------------*
308900011011     C     CNVEUR        BEGSR
309000011011     C*
309100011011     C* AZZERA VARIABILI DI LAVORO
309200011011     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
309300011011     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
309400011026     C                   EVAL      NDEC = '9'
309500011011     C*
309600011011IF  1C     WIMI          IFNE      *ZEROS
309700011011     C                   CLEAR                   YEUR
309800011011     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
309900011011     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
310000011011     C                   MOVEL     PARDIV        YECDVO                         *DIVISA IMP CONVERTI
310100011011     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
310200011011     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
310300011011     C                   CALL      'YEURCO'
310400011011     C                   PARM                    YEUR
310500011011IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
310600011011     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
310700011011X   2C                   ELSE                                                   *SI ERRORI
310800011011     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
310900011011E   2C                   ENDIF
311000011011E   1C                   ENDIF
311100011011     C*
311200011011     C                   ENDSR
311300011011     C*------------------------------------------------------------------------*
311400011011     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
311500011011     C*------------------------------------------------------------------------*
311600011011     C     CARGEDCE      BEGSR
311700011011     C*
311800011011     C                   CLEAR                   DSGEDCE
311900011026     C                   MOVEL     *blanks       WDIVCE            3
312000011011     C*
312100011011     C                   MOVEL(P)  'GED'         KTECOD
312200011011     C                   MOVEL(P)  'CE'          KTEKE1
312300011011     C     KEYTBE        CHAIN     TNTBE01L
312400011011     C                   IF        %found(TNTBE01L)
312500011011     C                   MOVEL     TBEUNI        DSGEDCE
312600011011     C                   ENDIF
312700011011     C*
312800011011     C                   IF        §GEDCEDAT > DATCOR
312900011026     C                   EVAL      WDIVCE = §GEDCEDIP
313000011026     C                   ELSE
313100011026     C                   EVAL      WDIVCE = §GEDCEDIV
313200011011     C                   ENDIF
313300011011     C*
313400011011     C                   ENDSR
313500970110     C*------------------------------------------------------------------------*
313600970110     C* CARTAB - CARICAMENTO TABELLE
313700970110     C*------------------------------------------------------------------------*
313800970110     C     CARTAB        BEGSR
313900970110     C*--------------------
314000970110     C* ORGANIGRAMMA
314100970110     C*--------------------
314200020227     C                   Z-ADD     *ZEROS        JJ                3 0
314300970110     C                   Z-ADD     *ZEROS        I                 4 0
314400970110     C     *LOVAL        SETLL     AZORG01L
314500970110     C                   READ      AZORG01L                               99
314600970110DO  1C     *IN99         DOWEQ     *OFF
314700970110IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATE
314800970110     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
314900970110     C                   ADD       1             I
315000970110     C                   Z-ADD     ORGFIL        ORG(I)                         *FILIALE
315100970110     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
315200020730     C                   MOVEL     ORGDE3        OG143
315300020730     C*
315400020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
315500020730     C                   CLEAR                   DNTW
315600020730     C*
315700020730     C                   MOVEL(P)  'NTW'         KTECOD
315800020730     C                   MOVEL(P)  §OGNTW        KTEKE1
315900020730     C     KEYTBE        CHAIN     TNTBE01L
316000020730     C                   IF        %found(TNTBE01L)
316100020730     C                   MOVEL     TBEUNI        DNTW
316200020730     C                   ENDIF
316300020730     C*
316400020730     C                   MOVEL     §NTWFIE       FL1(I)                         *FLAG ITALIA/ESTERO
316500970110     C* SELEZIONI
316600970430IF  3C     PARLNP        IFEQ      999                                          *TUTTE
316700970430     C                   Z-ADD     ORGFIL        SORG(I)                        *FILIALI SELEZIONATE
316800020730     C                   MOVEL     §NTWFIE       SFL1(I)                        *FLAG ITALIA/ESTERO
317200970110X   3C                   ELSE
317300970110IF  4C     PARLNP        IFEQ      ORGFIL                                       *UNA FILIALE
317400970417     C                   Z-ADD     ORGFIL        SORG(1)                        *FILIALE SELEZIONATA
317500020730     C                   MOVEL     §NTWFIE       SFL1(1)                        *FLAG ITALIA/ESTERO
317900970110E   4C                   ENDIF
318000970110E   3C                   ENDIF
318100081105     C* VERIFICO SE FILIALE dpd
318200020227     C                   MOVEL     ORGDE3        OG143
318300081105     C                   IF        §OGNTW = 'DPD'
318400081105     C                   ADD       1             xx                3 0
318500081105     C                   MOVEL     ORGFIL        FILdpd(xx)
318600020227     C                   ENDIF
318700081105     C* VERIFICO SE FILIALE FEDEX
318800081105     C                   MOVEL     ORGDE3        OG143
318900081105     C                   IF        §OGNTW = 'FED'
319000081105     C                   ADD       1             JJ
319100081105     C                   MOVEL     ORGFIL        FILFED(JJ)
319200081105     C                   ENDIF
319300970110E   2C                   ENDIF
319400970110     C                   READ      AZORG01L                               99
319500970110E   1C                   ENDDO
319600970423     C*--------------------
319700970423     C* TARIFFA ITALIA/ESTERO
319800970423     C*--------------------
319900970423IF  1C     PARFIE        IFEQ      *BLANKS                                      *ITALIA E ESTERO
320000970423     C                   MOVEL     'I'           SFIE(1)
320100970423     C                   MOVEL     'E'           SFIE(2)
320200970423X   1C                   ELSE                                                   *ITALIA O ESTERO
320300970423     C                   MOVEL     PARFIE        SFIE(1)
320400970423E   1C                   ENDIF
320500970110     C*--------------------
320600970110     C* TIPI TARIFFA
320700970110     C*--------------------
320800970110     C                   Z-ADD     *ZEROS        I                 4 0
320900020923     C                   Z-ADD     1             KTBKUT
321000970110     C                   MOVEL     'TR'          KTBCOD
321100970110     C     KE2TAB        SETLL     TABEL00F
321200970110     C     KE2TAB        READE     TABEL00F                               99
321300970110DO  1C     *IN99         DOWEQ     *OFF
321400970110IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
321500970110     C                   MOVEL     TBLUNI        DSTR
321600970110IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
321700970110     C                   ADD       1             I
321800970110     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
321900970110     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
322000970110E   3C                   ENDIF
322100970110E   2C                   ENDIF
322200970110     C     KE2TAB        READE     TABEL00F                               99
322300970110E   1C                   ENDDO
322400050808     C*--------------------
322500050808     C* cod. trattamento merce con packing list
322600050808     C*--------------------
322700050808     C                   Z-ADD     *ZEROS        I                 4 0
322800050808     C                   Z-ADD     1             KTBKUT
322900050808     C                   MOVEL     '1B'          KTBCOD
323000050808     C     KE2TAB        SETLL     TABEL00F
323100050808     C     KE2TAB        READE     TABEL00F                               99
323200050808DO  1C     *IN99         DOWEQ     *OFF
323300050808IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
323400050808     C                   MOVEL     TBLUNI        DS1b
323500050808IF  3C     §1bf16        IFEQ      'S'                                          *IN GESTIONE
323600050808     C                   ADD       1             I
323700050808     C                   MOVEL     TBLKEY        cpkl(I)                        *CODICE
323800050808E   3C                   ENDIF
323900050808E   2C                   ENDIF
324000050808     C     KE2TAB        READE     TABEL00F                               99
324100050808E   1C                   ENDDO
324200970110     C*--------------------
324300970110     C* RAGGRUPPAMENTI TIPO BOLLA
324400970110     C*--------------------
324500970110     C                   Z-ADD     *ZEROS        I                 4 0
324600020923     C                   Z-ADD     1             KTBKUT
324700970110     C                   MOVEL     '2W'          KTBCOD
324800970110     C     KE2TAB        SETLL     TABEL00F
324900970110     C     KE2TAB        READE     TABEL00F                               99
325000970110DO  1C     *IN99         DOWEQ     *OFF
325100970110IF  2C     TBLFLG        IFEQ      *BLANKS
325200970110     C                   MOVEL     TBLUNI        DS2W
325300970110     C                   ADD       1             I
325400970110     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
325500970110     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
325600970110     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
325700970110E   2C                   ENDIF
325800970110     C     KE2TAB        READE     TABEL00F                               99
325900970110E   1C                   ENDDO
326000970110     C*--------------------
326100970110     C* TIPI BOLLA
326200970110     C*--------------------
326300970110     C                   Z-ADD     *ZEROS        I                 4 0
326400970110     C                   Z-ADD     *ZEROS        J                 4 0
326500020923     C                   Z-ADD     1             KTBKUT
326600970110     C                   MOVEL     'TB'          KTBCOD
326700970110     C     KE2TAB        SETLL     TABEL00F
326800970110     C     KE2TAB        READE     TABEL00F                               99
326900970110DO  1C     *IN99         DOWEQ     *OFF
327000970110IF  2C     TBLFLG        IFEQ      *BLANKS
327100970110     C                   MOVEL     TBLUNI        DSTB
327200970110     C                   ADD       1             I
327300970110     C                   MOVEL     TBLKEY        TBL(I)                         *TIPO BOLLA
327400970110     C                   Z-ADD     1             J
327500970110     C                   SETOFF                                           98
327600970110     C     §TBRBL        LOOKUP    RBL(J)                                 98
327700970110IF  3C     *IN98         IFEQ      *ON
327800970110     C                   MOVEL     FSP(J)        SPD(I)                         *FLAG SPEDIZIONE
327900970110     C                   MOVEL     FRI(J)        RIC(I)                         *FLAG RICAVO
328000970110X   3C                   ELSE
328100970110     C                   MOVEL     'N'           SPD(I)                         *FLAG SPEDIZIONE
328200970110     C                   MOVEL     'N'           RIC(I)                         *FLAG RICAVO
328300970110E   3C                   ENDIF
328400970110E   2C                   ENDIF
328500970110     C     KE2TAB        READE     TABEL00F                               99
328600970110E   1C                   ENDDO
328700970110     C*--------------------
328800970110     C* CODICI PAGAMENTO
328900970110     C*--------------------
329000970110     C                   Z-ADD     *ZEROS        I                 4 0
329100020923     C                   Z-ADD     1             KTBKUT
329200970110     C                   MOVEL     'FA'          KTBCOD
329300970110     C     KE2TAB        SETLL     TABEL00F
329400970110     C     KE2TAB        READE     TABEL00F                               99
329500970110DO  1C     *IN99         DOWEQ     *OFF
329600970110IF  2C     TBLFLG        IFEQ      *BLANKS
329700970110     C                   MOVEL     TBLUNI        DSFA
329800970110     C                   ADD       1             I
329900970110     C                   MOVEL     TBLKEY        CDP(I)                         *CODICE PAGAMENTO
330000970110     C                   MOVEL     §FARAT        CRAT(I)                        *RATE DI PAGAMENTO
330100970110E   2C                   ENDIF
330200970110     C     KE2TAB        READE     TABEL00F                               99
330300970110E   1C                   ENDDO
330400970313     C*--------------------
330500970313     C* CODICI TASSAZIONE
330600970313     C*--------------------
330700970313     C                   Z-ADD     *ZEROS        I                 4 0
330800020923     C                   Z-ADD     1             KTBKUT
330900970313     C                   MOVEL     'CT'          KTBCOD
331000970313     C     KE2TAB        SETLL     TABEL00F
331100970313     C     KE2TAB        READE     TABEL00F                               99
331200970313DO  1C     *IN99         DOWEQ     *OFF
331300970313IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
331400970313     C                   MOVEL     TBLUNI        DSCT
331500970317IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
331600970317     C                   ADD       1             I
331700970317     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
331800970317     C                   MOVEL     §CTDES        DCTS(I)                        *DESCRIZIONE
331900970317     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
332000970317     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
332100970317     C                   MOVE      §CTCOR        WA1               1            *TIPO CODICE
332200970417IF  4C     WA1           IFEQ      '9'
332300970417IF  5C     CTS(I)        IFEQ      'IT'
332400970317     C     CTS(I)        OREQ      'EE'
332500970317     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
332600970417X   5C                   ELSE
332700970317     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
332800970417E   5C                   ENDIF
332900970417X   4C                   ELSE
333000970317     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
333100970417E   4C                   ENDIF
333200970417IF  4C     §CTNAR        IFEQ      *BLANKS
333300970317     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
333400970317     C                   MOVEL     'I'           CIE(I)                         *CODICE ITALIA
333500970417X   4C                   ELSE
333600970317     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
333700970317     C                   MOVEL     'E'           CIE(I)                         *CODICE ESTERO
333800970417E   4C                   ENDIF
333900970317E   3C                   ENDIF
334000970317E   2C                   ENDIF
334100970317     C     KE2TAB        READE     TABEL00F                               99
334200970317E   1C                   ENDDO
334300040920     C* CARICO GLI SCAGLIONI DI DEFAULT
334400040920IF  1C     PARTTR        IFEQ      '0'                                          *QUINTALI
334500040920     C     PARTTR        OREQ      '3'                                          *KG
334600040920     C                   Z-ADD     2             SGL(1)
334700040920     C                   Z-ADD     5             SGL(2)
334800040920     C                   Z-ADD     10            SGL(3)
334900040920     C                   Z-ADD     25            SGL(4)
335000040920     C                   Z-ADD     50            SGL(5)
335100040920     C                   Z-ADD     75            SGL(6)
335200040920     C                   Z-ADD     100           SGL(7)
335300040920     C                   Z-ADD     99999         SGL(8)
335400040920X   1C                   ELSE                                                   *COLLI
335500040920     C                   Z-ADD     1             SGL(1)
335600040920     C                   Z-ADD     2             SGL(2)
335700040920     C                   Z-ADD     3             SGL(3)
335800040920     C                   Z-ADD     4             SGL(4)
335900040920     C                   Z-ADD     99999         SGL(5)
336000040920E   1C                   ENDIF
336100970110     C*
336200970110     C                   ENDSR
336300970313     C*------------------------------------------------------------------------*
336400970313     C* CARTAR - CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
336500970313     C*------------------------------------------------------------------------*
336600970313     C     CARTAR        BEGSR
336700970313     C*
336800970313     C* REPERISCE IL CODICE DELLA TARIFFA DI CARTELLO IN VIGORE
336900970314IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORE
337000021202     C                   Z-ADD     *ZEROS        CARKSC
337100021202     C                   Z-ADD     *ZEROS        CARCTR
337200021202     C                   MOVE(P)   *ZEROS        CARPRG
337300021202     C                   CLEAR                   TRUL27DS
337400021202     C                   EVAL      I27TLA = 'L'
337500021202     C                   EVAL      I27DTA = DATCOR
337600021202     C                   CALL      'TRUL27R'
337700021202     C                   PARM                    TRUL27DS
337800021202     C*
337900021202     C                   IF        O27ERR <> 'E'
338000021202     C                   Z-ADD     O27KSC        CARKSC
338100021202     C                   Z-ADD     O27CTR        CARCTR
338200021202     C                   MOVE(P)   O27PRG        CARPRG
338300021202     C                   ENDIF
338400021202E   1C                   ENDIF
338500970317     C*
338600970317     C* PER CIASCUNA FILIALE RICHIESTA
338700971124DO  1C     1             DO        500           Y                 4 0
338800970417IF  2C     SORG(Y)       IFGT      *ZEROS
338900970423     C     Y             OCCUR     FTAR
339000970317     C*---
339100970317     C* CARICA GLI SCAGLIONI DA CONSIDERARE -CLIENTE O CARTELLO-
339200970317     C*---
339300970416     C                   Z-ADD     *ZEROS        SSGL
339400970313     C                   Z-ADD     PARKOV        KTDKSC
339500970313     C                   Z-ADD     PARCTR        KTDCTR
339600970313     C                   Z-ADD     PARPRG        KTDPRG
339700970417     C                   Z-ADD     SORG(Y)       KTDLNP
339800970313     C                   EXSR      CARSGL
339900970417IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
340000970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
340100970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
340200970417     C                   Z-ADD     CARKSC        KTDKSC
340300970313     C                   Z-ADD     CARCTR        KTDCTR
340400970313     C                   Z-ADD     CARPRG        KTDPRG
340500970417     C                   Z-ADD     SORG(Y)       KTDLNP
340600970314     C                   EXSR      CARSGL
340700970703     C                   MOVEL     DEPCTO        PARCTO
340800970418E   3C                   ENDIF
340900970418IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
341000040920     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
341100040920     c                   move      sgl           ssgl
341200970418E   3C                   ENDIF
341300970317     C*---
341400970313     C* CARICA I CODICI TASSAZIONE DA CONSIDERARE -CLIENTE O CARTELLO-
341500970317     C*---
341600970423     C                   MOVEL     *BLANKS       CCTI
341700970423     C                   MOVEL     *BLANKS       CRCI
341800970423     C                   MOVEL     *BLANKS       CCTE
341900970423     C                   MOVEL     *BLANKS       CRCE
342000970423DO  3C     1             DO        2             W                 4 0
342100970423IF  4C     SFIE(W)       IFNE      *BLANKS
342200970314     C                   Z-ADD     PARKOV        KTDKSC
342300970314     C                   Z-ADD     PARCTR        KTDCTR
342400970314     C                   Z-ADD     PARPRG        KTDPRG
342500970417     C                   Z-ADD     SORG(Y)       KTDLNP
342600971204     C                   MOVEL     SFL1(Y)       DLPFL1
342700970314     C                   EXSR      CARCTS
342800970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
342900970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
343000970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
343100970314     C                   Z-ADD     CARKSC        KTDKSC
343200970314     C                   Z-ADD     CARCTR        KTDCTR
343300970314     C                   Z-ADD     CARPRG        KTDPRG
343400970417     C                   Z-ADD     SORG(Y)       KTDLNP
343500971204     C                   MOVEL     SFL1(Y)       DLPFL1
343600970314     C                   EXSR      CARCTS
343700970703     C                   MOVEL     DEPCTO        PARCTO
343800970423E   5C                   ENDIF
343900970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
344000970418     C                   EXSR      FIXCTS                                       *IMPOSTA FISSI CTS
344100970423E   5C                   ENDIF
344200970416     C                   EXSR      RAPSVI                                       *CTS->CTS ELEMENTARI
344300970423E   4C                   ENDIF
344400970423E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
344500970417     C*
344600970417E   2C                   ENDIF
344700970417E   1C                   ENDDO                                                  *FINE FILIALI
344800970313     C*
344900970313     C                   ENDSR
345000970313     C*------------------------------------------------------------------------*
345100970313     C* CARSGL - CARICA GLI SCAGLIONI
345200970313     C*------------------------------------------------------------------------*
345300970313     C     CARSGL        BEGSR
345400970313     C*
345500970313     C* AZZERA LE VARIABILI DI LAVORO
345600970313     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
345700970313     C                   Z-ADD     *ZEROS        I                 4 0
345800970313     C*
345900011128     C                   CLEAR                   DEPORP
346000060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
346100990820     C     KEYTAD        SETLL     TITAD04L                               99
346200990820     C     KEYTAD        READE     TITAD04L                               99
346300060804X   1C                   ELSE                                                   *OFFERTA
346400060804     C     KEYTAD        SETLL     TIOFD01L                               99
346500060804     C     KEYTAD        READE     TIOFD01L                               99
346600060804E   1C                   ENDIF
346700011128     C                   EVAL      DEPORP = TADORP                              *SOLO un cts
346800011010DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
346900011128     C     TADORP        ANDEQ     DEPORP                                       *ROTTURA ORDINAMENTO
347000011010IF  2C     TADATB        IFEQ      *BLANKS
347100011010     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
347200011010     C     TADSGL        LOOKUP    SSGL                                   98
347300011010IF  3C     *IN98         IFEQ      *OFF
347400030122     C                   IF        I < 10
347500011010     C                   ADD       1             I
347600011010     C                   Z-ADD     TADSGL        SSGL(I)
347700030122     C                   ELSE
347800030122     C                   LEAVE
347900030122     C                   ENDIF
348000011010E   3C                   ENDIF
348100011010E   2C                   ENDIF
348200060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
348300990820     C     KEYTAD        READE     TITAD04L                               99
348400060804X   2C                   ELSE                                                   *OFFERTA
348500060804     C     KEYTAD        READE     TIOFD01L                               99
348600060804E   2C                   ENDIF
348700970313E   1C                   ENDDO                                                  *FINE DETTAGLIO
348800970313     C*
348900970313     C                   ENDSR
349000970314     C*------------------------------------------------------------------------*
349100970314     C* CARCTS   CARICA I CODICI TASSAZIONE
349200970314     C*------------------------------------------------------------------------*
349300970314     C     CARCTS        BEGSR
349400970314     C*
349500970314     C* AZZERA LE VARIABILI DI LAVORO
349600970314     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
349700970314     C                   Z-ADD     *ZEROS        I                 4 0
349800970317     C                   CLEAR                   WCTS
349900970317     C                   CLEAR                   WRAP
350000970317     C                   CLEAR                   WNAR
350100970317     C                   CLEAR                   WTCT
350200970314     C*
350300970314     C* MEMORIZZA I CODICI DELLA LINEA DI PARTENZA RICHIESTA
350400060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
350500990820     C     KEYTAD        SETLL     TITAD04L                               99
350600990820     C     KEYTAD        READE     TITAD04L                               99
350700060804X   1C                   ELSE                                                   *OFFERTA
350800060804     C     KEYTAD        SETLL     TIOFD01L                               99
350900060804     C     KEYTAD        READE     TIOFD01L                               99
351000060804E   1C                   ENDIF
351100970314DO  1C     *IN99         DOWEQ     *OFF
351200970314IF  2C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
351300970314     C                   SETOFF                                           98
351400970314     C                   Z-ADD     1             J                 4 0
351500970314     C     TADCTS        LOOKUP    CTS(J)                                 98
351600970314IF  3C     *IN98         IFEQ      *ON                                          *TROVATO
351700970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
351800970417     C     CIE(J)        ANDEQ     'I'                                          *CODICI ITALIA
351900970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
352000971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
352100970417     C     CIE(J)        ANDEQ     'E'                                          *CODICI ESTERO
352200970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
352300971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ITALIA
352400970314     C                   SETOFF                                           98
352500970314     C                   Z-ADD     1             K                 4 0
352600970314     C     TADCTS        LOOKUP    WCTS(K)                                98
352700970417IF  5C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
352800970417     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
352900970314     C                   ADD       1             I
353000970314     C                   MOVEL     TADCTS        WCTS(I)                        *CODICE
353100970314     C                   MOVEL     RAP(J)        WRAP(I)                        *REGIONE
353200970314     C                   MOVEL     NAR(J)        WNAR(I)                        *NAZIONE
353300970314     C                   MOVEL     TCT(J)        WTCT(I)                        *TIPO
353400970417E   5C                   ENDIF
353500970417E   4C                   ENDIF
353600970314E   3C                   ENDIF
353700970314E   2C                   ENDIF
353800060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
353900990820     C     KEYTAD        READE     TITAD04L                               99
354000060804X   2C                   ELSE                                                   *OFFERTA
354100060804     C     KEYTAD        READE     TIOFD01L                               99
354200060804E   2C                   ENDIF
354300970314E   1C                   ENDDO
354400970314     C*
354500970314     C                   ENDSR
354600970421     C*------------------------------------------------------------------------*
354700970421     C* FIXCTS - IMPOSTA CODICI TASSAZIONE FISSI
354800970421     C*------------------------------------------------------------------------*
354900970421     C     FIXCTS        BEGSR
355000970421     C*
355100970421     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
355200970421     C*
355300970421     C                   Z-ADD     *ZEROS        J                 4 0
355400050103DO  1C     1             DO        600           I
355500970421IF  2C     CTS(I)        IFNE      *BLANKS
355600970421IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
355700970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
355800970421     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
355900970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
356000971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
356100970421     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
356200970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
356300971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
356400970421     C                   ADD       1             J
356500970421     C                   MOVEL     CTS(I)        WCTS(J)                        *CODICE
356600970421     C                   MOVEL     RAP(I)        WRAP(J)                        *REGIONE
356700970421     C                   MOVEL     NAR(I)        WNAR(J)                        *NAZIONE
356800970421     C                   MOVEL     TCT(I)        WTCT(J)                        *TIPO
356900970421E   4C                   ENDIF
357000970421E   3C                   ENDIF
357100970421E   2C                   ENDIF
357200970421E   1C                   ENDDO
357300970421     C*
357400970421     C                   ENDSR
357500970314     C*------------------------------------------------------------------------*
357600970314     C* RAPSVI - TRASFORMA I CODICI DELLA TARIFFA IN CODICI ELEMENTARI
357700970314     C*------------------------------------------------------------------------*
357800970314     C     RAPSVI        BEGSR
357900970314     C*
358000970314     C* 1°GIRO-> CARICA TUTTI I CODICI ELEMENTARI COME -NON CALCOLARE-
358100970314     C                   Z-ADD     *ZEROS        J                 4 0
358200050103DO  1C     1             DO        600           I
358300970416IF  2C     CTS(I)        IFNE      *BLANKS
358400970416IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
358500970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
358600970417     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
358700970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
358800971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
358900970417     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
359000970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
359100971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
359200970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
359300970314     C                   ADD       1             J
359400970423     C                   MOVEL     CTS(I)        CCTI(J)                        *CODICE
359500970423     C                   MOVEL     *BLANKS       CRCI(J)                        *RAGGRUPPAMENTO
359600970423X   5C                   ELSE                                                   *ESTERO
359700970423     C                   ADD       1             J
359800970423     C                   MOVEL     CTS(I)        CCTE(J)                        *CODICE
359900970423     C                   MOVEL     *BLANKS       CRCE(J)                        *RAGGRUPPAMENTO
360000970423E   5C                   ENDIF
360100970423E   4C                   ENDIF
360200970417E   3C                   ENDIF
360300970314E   2C                   ENDIF
360400970314E   1C                   ENDDO                                                  *FINE CODICI BASE
360500970314     C*
360600970314     C* 2°GIRO-> TRASFORMA I -NON CALCOLARE- IN -CALCOLARE IN NAZIONE-
360700050103DO  1C     1             DO        600           I
360800970314IF  2C     WCTS(I)       IFNE      *BLANKS
360900970314IF  3C     WTCT(I)       IFEQ      'N'                                          *SOLO NAZIONE
361000050103DO  4C     1             DO        600           J
361100970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
361200970423IF  6C     CCTI(J)       IFNE      *BLANKS
361300970314     C                   SETOFF                                           98
361400970314     C                   Z-ADD     1             K                 4 0
361500970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
361600970423IF  7C     WNAR(I)       IFEQ      NAR(K)
361700970423     C                   MOVEL     'N'           CRCI(J)
361800970423E   7C                   ENDIF
361900970423E   6C                   ENDIF
362000970423X   5C                   ELSE                                                   *ESTERO
362100970423IF  6C     CCTE(J)       IFNE      *BLANKS
362200970423     C                   SETOFF                                           98
362300970423     C                   Z-ADD     1             K                 4 0
362400970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
362500970423IF  7C     WNAR(I)       IFEQ      NAR(K)
362600970423     C                   MOVEL     'N'           CRCE(J)
362700970423E   7C                   ENDIF
362800970423E   6C                   ENDIF
362900970423E   5C                   ENDIF
363000970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
363100970314E   3C                   ENDIF
363200970314E   2C                   ENDIF
363300970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
363400970314     C*
363500970314     C* 3°GIRO-> TRASFORMA I -CALCOLARE IN NAZIONE- IN -CALCOLARE IN REGIONE-
363600050103DO  1C     1             DO        600           I
363700970314IF  2C     WCTS(I)       IFNE      *BLANKS
363800970314IF  3C     WTCT(I)       IFEQ      'R'                                          *SOLO REGIONE
363900050103DO  4C     1             DO        600           J
364000970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
364100970423IF  6C     CCTI(J)       IFNE      *BLANKS
364200970314     C                   SETOFF                                           98
364300970314     C                   Z-ADD     1             K                 4 0
364400970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
364500970423IF  7C     WRAP(I)       IFEQ      RAP(K)
364600970423     C                   MOVEL     'R'           CRCI(J)
364700970423E   7C                   ENDIF
364800970423E   6C                   ENDIF
364900970423X   5C                   ELSE                                                   *ESTERO
365000970423IF  6C     CCTE(J)       IFNE      *BLANKS
365100970423     C                   SETOFF                                           98
365200970423     C                   Z-ADD     1             K                 4 0
365300970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
365400970423IF  7C     WRAP(I)       IFEQ      RAP(K)
365500970423     C                   MOVEL     'R'           CRCE(J)
365600970423E   7C                   ENDIF
365700970423E   6C                   ENDIF
365800970314E   5C                   ENDIF
365900970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
366000970314E   3C                   ENDIF
366100970314E   2C                   ENDIF
366200970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
366300970314     C*
366400970314     C* 4°GIRO-> TRASFORMA I -CALCOLARE IN REGIONE- IN -CALCOLARE-
366500050103DO  1C     1             DO        600           I
366600970314IF  2C     WCTS(I)       IFNE      *BLANKS
366700970314IF  3C     WTCT(I)       IFEQ      'E'                                          *SOLO ELEMENTARI
366800050103DO  4C     1             DO        600           J
366900970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
367000970423IF  6C     CCTI(J)       IFNE      *BLANKS
367100970423IF  7C     WCTS(I)       IFEQ      CCTI(J)
367200970423     C                   MOVEL     'T'           CRCI(J)
367300970423E   7C                   ENDIF
367400970423E   6C                   ENDIF
367500970423X   5C                   ELSE                                                   *ESTERO
367600970423IF  6C     CCTE(J)       IFNE      *BLANKS
367700970423IF  7C     WCTS(I)       IFEQ      CCTE(J)
367800970423     C                   MOVEL     'T'           CRCE(J)
367900970423E   7C                   ENDIF
368000970423E   6C                   ENDIF
368100970423E   5C                   ENDIF
368200970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
368300970314E   3C                   ENDIF
368400970314E   2C                   ENDIF
368500970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
368600970314     C*
368700970314     C                   ENDSR
368800981218     C*------------------------------------------------------------------------*
368900981218     C* CANTAR - ANNULLAMENTO TARIFFA
369000981218     C*------------------------------------------------------------------------*
369100981218     C     CANTAR        BEGSR
369200981218     C*
369300981218     C* CANCELLA DATI TIPO - GENERALI
369400981218     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE
369500060804     C                   MOVEL     parcto        KTCCTO                         *DATI TARIFFA
369600981218     C                   Z-ADD     PARKOV        KTCKSC                         *CLIENTE
369700011001     C     KE3ETC        SETLL     TEETC01L
369800011001     C     KE3ETC        READE     TEETC01L                               99
369900981218DO  1C     *IN99         DOWEQ     *OFF
370000981218IF  2C     PARCTR        IFEQ      ETCCTR                                       *TARIFFA RICHIESTA
370100981218IF  3C     PARPRG        IFEQ      ETCPRG                                       *PROGRESSIVO RICHIES
370200981218IF  4C     PARLNP        IFEQ      ETCLNP                                       *LINEA RICHIESTA
370300981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
370400981218     C                   MOVEL     'A'           ETCATB                         *ANNULLAMENTO
370500981218     C                   MOVEL     *BLANKS       ETCFTR                         *FLAG TRASMISSIONE
370600011128     C*---               Z-ADD     *ZEROS        ETCDTR                         *DATA TRASMISSIONE
370700011001     C                   UPDATE    TEETC000                                     *AGGIORNA
370800981218E   4C                   ENDIF
370900981218E   3C                   ENDIF
371000981218E   2C                   ENDIF
371100011001     C     KE3ETC        READE     TEETC01L                               99
371200981218E   1C                   ENDDO                                                  *FINE DATI GENERALI
371300981218     C*
371400981218     C* CANCELLA DATI TIPO - SCAGLIONI
371500060829     c* ricavi
371600981218     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE
371700060804     C                   MOVEL     parcto        KTSCTO                         *DATI TARIFFA
371800981218     C                   Z-ADD     PARKOV        KTSKSC                         *CLIENTE
371900011001     C     KE3ETS        SETLL     TEETS01L
372000011001     C     KE3ETS        READE     TEETS01L                               99
372100981218DO  1C     *IN99         DOWEQ     *OFF
372200981218IF  2C     PARCTR        IFEQ      ETSCTR                                       *TARIFFA RICHIESTA
372300981218IF  3C     PARPRG        IFEQ      ETSPRG                                       *PROGRESSIVO RICHIES
372400981218IF  4C     PARLNP        IFEQ      ETSLNP                                       *LINEA RICHIESTA
372500981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
372600981218     C                   MOVEL     'A'           ETSATB                         *ANNULLAMENTO
372700981218     C                   MOVEL     *BLANKS       ETSFTR                         *FLAG TRASMISSIONE
372800011001     C                   UPDATE    TEETS000                                     *AGGIORNA
372900981218E   4C                   ENDIF
373000981218E   3C                   ENDIF
373100981218E   2C                   ENDIF
373200011001     C     KE3ETS        READE     TEETS01L                               99
373300981218E   1C                   ENDDO                                                  *FINE SCAGLIONI
373400060829     c* competetnze
373500060829     C     KE3ETS        SETLL     TEETS11L
373600060829     C     KE3ETS        READE     TEETS11L                               99
373700060829DO  1C     *IN99         DOWEQ     *OFF
373800060829IF  2C     PARCTR        IFEQ      £ETSCTR                                      *TARIFFA RICHIESTA
373900060829IF  3C     PARPRG        IFEQ      £ETSPRG                                      *PROGRESSIVO RICHIES
374000060829IF  4C     PARLNP        IFEQ      £ETSLNP                                      *LINEA RICHIESTA
374100060829     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
374200060829     C                   MOVEL     'A'           £ETSATB                        *ANNULLAMENTO
374300060829     C                   MOVEL     *BLANKS       £ETSFTR                        *FLAG TRASMISSIONE
374400060829     C                   UPDATE    TEETS100                                     *AGGIORNA
374500060829E   4C                   ENDIF
374600060829E   3C                   ENDIF
374700060829E   2C                   ENDIF
374800060829     C     KE3ETS        READE     TEETS11L                               99
374900060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
375000981218     C*
375100981218     C* CANCELLA DATI TIPO - CODICI TASSAZIONE
375200981218     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE
375300060804     C                   MOVEL     parcto        KTTCTO                         *DATI TARIFFA
375400981218     C                   Z-ADD     PARKOV        KTTKSC                         *CLIENTE
375500011001     C     KE3ETT        SETLL     TEETT01L
375600011001     C     KE3ETT        READE     TEETT01L                               99
375700981218DO  1C     *IN99         DOWEQ     *OFF
375800981218IF  2C     PARCTR        IFEQ      ETTCTR                                       *TARIFFA RICHIESTA
375900981218IF  3C     PARPRG        IFEQ      ETTPRG                                       *PROGRESSIVO RICHIES
376000981218IF  4C     PARLNP        IFEQ      ETTLNP                                       *LINEA RICHIESTA
376100981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
376200981218IF  5C     PARFIE        IFEQ      ETTFIE                                       *ITA O EST RICHIESTO
376300981218     C     PARFIE        OREQ      *BLANKS                                      *TUTTO ITA E EST
376400981218     C                   MOVEL     'A'           ETTATB                         *ANNULLAMENTO
376500981218     C                   MOVEL     *BLANKS       ETTFTR                         *FLAG TRASMISSIONE
376600011001     C                   UPDATE    TEETT000                                     *AGGIORNA
376700981218E   5C                   ENDIF
376800981218E   4C                   ENDIF
376900981218E   3C                   ENDIF
377000981218E   2C                   ENDIF
377100011001     C     KE3ETT        READE     TEETT01L                               99
377200981218E   1C                   ENDDO                                                  *FINE CODICI TASSAZI
377300981218     C*
377400981218     C                   ENDSR
377500020923     C*--------------------------------------------------------------------------------------------*
377600020923     C* REPERISCE I DATI UTENTE
377700020923     C*--------------------------------------------------------------------------------------------*
377800020923     C     REPDATIUTE    BEGSR
377900020923     C*
378000020923     C* INIZIALIZZA VARIABILI DI WRK
378100020923     C                   CLEAR                   TIBS34DS
378200020923     C                   CLEAR                   AZUTEDS
378300020923     C                   CLEAR                   DDATIUTE
378400020923     C*
378500020923     C     *DTAARA       DEFINE    §azute        azuteds
378600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
378700020923     C                   IN(E)     *DTAARA
378800020923if  1C                   IF        %Error
378900020923     c                   EVAL      I34Tla = 'L'
379000020923     C                   CALL      'TIBS34R'
379100020923     C                   PARM                    Tibs34Ds
379200020923     C                   IN        *DTAARA
379300020923e   1C                   ENDIF
379400020923     C*
379500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
379600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
379700020923     C*
379800020923     C                   ENDSR
379900021031     C*------------------------------------------------------------------------*
380000021031     C* REPCLI - CARICAMENTO LISTA CLIENTI PADRI/FIGLI
380100021031     C*------------------------------------------------------------------------*
380200021031     C     REPCLI        BEGSR
380300021031     C*
380400021031     C* SE RICHIESTA ELABORAZIONE X CLIENTE UNIFICANTE REPERISCO LA LISTA DEI FIGLI
380500021031     C                   IF        PARUNI = 'S'
380600021031     C                   CLEAR                   TIBS10DS
380700021031     C                   EVAL      D10DRF = DATCOR                              *data riferimento
380800021031     C                   EVAL      D10TLE = 'ST'                                *tipo legame
380900021031     C                   EVAL      D10PAF = 'F'                                 *cerca il padre
381000021031     C                   Z-ADD     PARKSC        D10COD                         *cod cliente da tst
381100021031     C                   CALL      'TIBS10R'
381200021031     C                   PARM                    TIBS10DS
381300021031     C*
381400021031     C                   IF        D10ERR <> *blanks OR                         *No unif. o padre
381500021031     C                             D10COP =  *zeros
381600021031     C                   CLEAR                   SKCLI
381700021031     C                   Z-ADD     PARKSC        SKCLI(1)
381800021031     C                   ENDIF
381900021031     C*
382000021031     C                   ELSE
382100021031     C                   CLEAR                   SKCLI
382200021031     C                   Z-ADD     PARKSC        SKCLI(1)
382300021031     C                   ENDIF
382400021031     C*
382500021031     C                   ENDSR
382600970110     C*------------------------------------------------------------------------*
382700970110     C* *INZSR - OPERAZIONI INIZIALI
382800970110     C*------------------------------------------------------------------------*
382900970110     C     *INZSR        BEGSR
383000970110     C*--------------------
383100970110     C* RICEVIMENTO PARAMETRI
383200970110     C*--------------------
383300970110     C     *ENTRY        PLIST
383400970110     C                   PARM                    KPJBA
383500970110     C                   MOVEL     KPJBU         PARAM
383600021121     C*
383700021121     C* CARICA NUOVE TABELLE OCCORRENTI
383800021121     C                   EXSR      CARGEDCE
383900021121     C*
384000021121     C* REPERISCO SUBITO LA DIVISA DELLA TARIFFA RICEVUTA IN INPUT
384100021121     C                   IF        PARDIV = *BLANKS
384200021121     C                   Z-ADD     PARKOV        KTMKSC
384300021121     C                   Z-ADD     PARCTR        KTMCTR
384400021121     C                   Z-ADD     PARPRG        KTMPRG
384500021121     C     KEYTAMUNIQ    CHAIN     TNTAM01L
384600021121     C                   IF        %FOUND
384700021121     C                   MOVEL(P)  TAMFLO        DSTA01
384800021121     C                   MOVEL(P)  §TADIV        PARDIV
384900021121     C                   ELSE
385000021121     C                   MOVEL(P)  WDIVCE        PARDIV
385100021121     C                   ENDIF
385200021121     C                   ENDIF
385300970110     C*--------------------
385400970110     C* VARIABILI RIFERITE AL DATABASE
385500970110     C*--------------------
385600970110     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
385700970110     C     *LIKE         DEFINE    TBLCOD        KTBCOD
385800970110     C     *LIKE         DEFINE    TBLKEY        KTBKEY
385900970110     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
386000970110     C     *LIKE         DEFINE    ACOKCC        KACKCC
386100970110     C     *LIKE         DEFINE    ACOKSC        KACKSC
386200970110     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
386300970110     C     *LIKE         DEFINE    CLPKCC        KCLKCC
386400970110     C     *LIKE         DEFINE    CLPKSC        KCLKSC
386500970110     C     *LIKE         DEFINE    INDKUT        KINKUT                         *CNIND00F
386600970110     C     *LIKE         DEFINE    INDKCC        KINKCC
386700970415     C     *LIKE         DEFINE    INDKSC        KINKSC
386800970110     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
386900970110     C     *LIKE         DEFINE    TAMCTR        KTMCTR
387000021121     C     *LIKE         DEFINE    TAMPRG        KTMPRG
387100990820     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
387200970110     C     *LIKE         DEFINE    TADCTR        KTDCTR
387300970110     C     *LIKE         DEFINE    TADPRG        KTDPRG
387400970317     C     *LIKE         DEFINE    TADLNP        KTDLNP
387500990812     C     *LIKE         DEFINE    TASKSC        KTAKSC                         *TITAS00F
387600970110     C     *LIKE         DEFINE    TASAAS        KTAAAS
387700970110     C     *LIKE         DEFINE    TASMGS        KTAMGS
387800970110     C     *LIKE         DEFINE    CSBAAS        KSBAAS                         *TNCSB00F
387900970110     C     *LIKE         DEFINE    CSBLNP        KSBLNP
388000970110     C     *LIKE         DEFINE    CSBNRS        KSBNRS
388100970110     C     *LIKE         DEFINE    CSBNSP        KSBNSP
388200011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
388300980713     C     *LIKE         DEFINE    ETCCTO        KTCCTO
388400970110     C     *LIKE         DEFINE    ETCKSC        KTCKSC
388500970110     C     *LIKE         DEFINE    ETCCTR        KTCCTR
388600970110     C     *LIKE         DEFINE    ETCPRG        KTCPRG
388700970110     C     *LIKE         DEFINE    ETCLNP        KTCLNP
388800011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
388900980713     C     *LIKE         DEFINE    ETSCTO        KTSCTO
389000970110     C     *LIKE         DEFINE    ETSKSC        KTSKSC
389100970110     C     *LIKE         DEFINE    ETSCTR        KTSCTR
389200970110     C     *LIKE         DEFINE    ETSPRG        KTSPRG
389300970110     C     *LIKE         DEFINE    ETSLNP        KTSLNP
389400970110     C     *LIKE         DEFINE    ETSSGL        KTSSGL
389500011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
389600980713     C     *LIKE         DEFINE    ETTCTO        KTTCTO
389700970110     C     *LIKE         DEFINE    ETTKSC        KTTKSC
389800970110     C     *LIKE         DEFINE    ETTCTR        KTTCTR
389900970110     C     *LIKE         DEFINE    ETTPRG        KTTPRG
390000970110     C     *LIKE         DEFINE    ETTLNP        KTTLNP
390100970110     C     *LIKE         DEFINE    ETTFIE        KTTFIE
390200970110     C     *LIKE         DEFINE    ETTCTS        KTTCTS
390300970110     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DEPOSITI
390400970110     C     *LIKE         DEFINE    TAMCTR        CARCTR
390500970110     C     *LIKE         DEFINE    TAMPRG        CARPRG
390600970430     C     *LIKE         DEFINE    TASLNP        DEPFIL
390700980713     C     *LIKE         DEFINE    PARCTO        DEPCTO
390800020730     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
390900011128     C     *LIKE         DEFINE    TADORP        DEPORP
391000980821     C     *LIKE         DEFINE    ETCLNP        XORG                           *DI PASSAGGIO
391100980821     C     *LIKE         DEFINE    ETTFIE        XFIE
391200011011     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
391300011011     C     *LIKE         DEFINE    TBEKE1        KTEKE1
391400050309     C     *LIKE         DEFINE    GCPAAS        KGPAAS                         *TIGCP00F
391500021122     C     *LIKE         DEFINE    GCPLNP        KGPLNP
391600021122     C     *LIKE         DEFINE    GCPNRS        KGPNRS
391700021122     C     *LIKE         DEFINE    GCPNSP        KGPNSP
391800980821     C*--------------------
391900970110     C* CHAIVI DI LETTURA
392000970110     C*--------------------
392100060315     C* CHIAVE LETTURA TITA730C - COMPLETA
392200060315     C     KEYTA7        KLIST
392300060315     C                   KFLD                    TA7AAS
392400060315     C                   KFLD                    TA7LNP
392500060315     C                   KFLD                    TA7NRS
392600060315     C                   KFLD                    TA7NSP
392700060315     C                   KFLD                    TA7TBL
392800970110     C* LETTURA TABEL00F - COMPLETA
392900970110     C     KEYTAB        KLIST
393000970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
393100970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
393200970110     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
393300970110     C* LETTURA TABEL00F - PARZIALE
393400970110     C     KE2TAB        KLIST
393500970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
393600970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
393700970110     C* LETTURA CNACO00F - COMPETA
393800970110     C     KEYACO        KLIST
393900970110     C                   KFLD                    KACKUT                         *CODICE UTENTE
394000970110     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
394100970110     C                   KFLD                    KACKSC                         *CODICE CLIENTE
394200970110     C* LETTURA CNCLP00F - COMPLETA
394300970110     C     KEYCLP        KLIST
394400970110     C                   KFLD                    KCLKUT                         *CODICE UTENTE
394500970110     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
394600970110     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
394700970415     C* LETTURA CNIND00F - COMPLETA
394800970415     C     KEYIND        KLIST
394900970415     C                   KFLD                    KINKUT                         *CODICE UTENTE
395000970415     C                   KFLD                    KINKCC                         *CODICE CAPOCONTO
395100970415     C                   KFLD                    KINKSC                         *CODICE CLIENTE
395200970110     C* LETTURA TNTAM01L - PARZIALE
395300970110     C     KEYTAM        KLIST
395400970110     C                   KFLD                    KTMKSC                         *CLIENTE
395500970110     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
395600021121     C* LETTURA TNTAM01L - PARZIALE
395700021121     C     KEYTAMUNIQ    KLIST
395800021121     C                   KFLD                    KTMKSC                         *CLIENTE
395900021121     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
396000021121     C                   KFLD                    KTMPRG                         *PROGRESSIVO
396100990820     C* LETTURA TITAD01L - PARZIALE
396200970110     C     KEYTAD        KLIST
396300970110     C                   KFLD                    KTDKSC                         *CLIENTE
396400970110     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
396500970110     C                   KFLD                    KTDPRG                         *PROGRESSIVO
396600970317     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
396700990812     C* LETTURA TITAS31C - PARZIALE
396800970110     C     K31TAS        KLIST
396900970110     C                   KFLD                    KTAKSC                         *CLIENTE
397000990812     C* LETTURA TITAS38C - COMPLETA
397100970110     C     K38TAS        KLIST
397200970110     C                   KFLD                    KTAAAS                         *ANNO
397300970110     C                   KFLD                    KTAMGS                         *MESE/GIORNO
397400970110     C                   KFLD                    KTAKSC                         *CLIENTE
397500970110     C* LETTURA TNCSB03L - COMPLETA
397600970110     C     KEYCSB        KLIST
397700970110     C                   KFLD                    KSBAAS                         *ANNO
397800970110     C                   KFLD                    KSBLNP                         *LINEA PARTENZA
397900970110     C                   KFLD                    KSBNRS                         *SERIE
398000970110     C                   KFLD                    KSBNSP                         *SPEDIZIONE
398100011001     C* LETTURA TEETC01L - COMPLETA
398200981218     C     KEYETC        KLIST
398300980713     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
398400980713     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
398500970110     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
398600970110     C                   KFLD                    KTCCTR                         *TARIFFA
398700970110     C                   KFLD                    KTCPRG                         *PROGRESSIVO
398800970110     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
398900011001     C* LETTURA TEETC01L - PARZIALE
399000981218     C     KE3ETC        KLIST
399100981218     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
399200981218     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
399300981218     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
399400011001     C* LETTURA TEETS01L - COMPLETA
399500970110     C     KEYETS        KLIST
399600980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
399700970110     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
399800970110     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
399900970110     C                   KFLD                    KTSCTR                         *TARIFFA
400000970110     C                   KFLD                    KTSPRG                         *PROGRESSIVO
400100970110     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
400200970417     C                   KFLD                    KTSSGL                         *SCAGLIONE
400300011001     C* LETTURA TEETS01L - PARZIALE
400400970314     C     KE2ETS        KLIST
400500980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
400600970314     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
400700970314     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
400800970314     C                   KFLD                    KTSCTR                         *TARIFFA
400900970314     C                   KFLD                    KTSPRG                         *PROGRESSIVO
401000970314     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
401100011001     C* LETTURA TEETS01L - PARZIALE
401200981218     C     KE3ETS        KLIST
401300981218     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
401400981218     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
401500981218     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
401600011001     C* LETTURA TEETT01L - COMPLETA
401700970110     C     KEYETT        KLIST
401800980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
401900970110     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
402000970110     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
402100970110     C                   KFLD                    KTTCTR                         *TARIFFA
402200970110     C                   KFLD                    KTTPRG                         *PROGRESSIVO
402300970110     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
402400970110     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
402500970110     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
402600011001     C* LETTURA TEETT01L - PARZIALE
402700970314     C     KE2ETT        KLIST
402800980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
402900970314     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
403000970314     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
403100970314     C                   KFLD                    KTTCTR                         *TARIFFA
403200970314     C                   KFLD                    KTTPRG                         *PROGRESSIVO
403300970314     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
403400970314     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
403500040809     C* LETTURA fiar531c
403600040809     C     Kar5          KLIST
403700040809     C                   KFLD                    ar5aas                         *DATI SEDE O FILIALE
403800040809     C                   KFLD                    ar5lnp                         *TARIFFA O OFFERTA
403900040809     C                   KFLD                    ar5nrs                         *CLIENTE O VISITA
404000040809     C                   KFLD                    ar5nsp                         *CLIENTE O VISITA
404100040809     C                   KFLD                    ar5trd                         *CLIENTE O VISITA
404200040809     C* LETTURA TEETT01L - PARZIALE
404300040809     C     KE3ETT        KLIST
404400040809     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
404500040809     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
404600040809     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
404700011011     C* LETTURA TNTBE01L - PARZIALE
404800011011     C     KEYTBE        KLIST
404900011011     C                   KFLD                    KTECOD                         *UTENTE
405000011011     C                   KFLD                    KTEKE1                         *CODICE
405100050309     C* LETTURA TIGCP51L - PARZIALE
405200021122     C     K01GCP        KLIST
405300021122     C                   KFLD                    KGPAAS                         *ANNO
405400021122     C                   KFLD                    KGPLNP                         *LINEA DI PARTENZA
405500021122     C                   KFLD                    KGPNRS                         *SERIE
405600021122     C                   KFLD                    KGPNSP                         *SPEDIZIONE
405700970110     C*--------------------
405800970110     C* VARIABILI DI CONTROLLO
405900970110     C*--------------------
406000970110     C                   MOVEL     '0'           $FINE             1            *FINE PGM
406100970110     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
406200970416     C                   MOVEL     'S'           $OK               1            *RECORD OK
406300970312     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO TROVATO
406400970313     C                   MOVEL     '0'           $ERR              1            *ERRORE
406500970313     C                   MOVEL     '1'           *INOF                          *OVERFLOW QSYSPRT
406600970313     C                   MOVEL     '1'           *IN01                          *OVERFLOW TNTE41P
406700970110     C*
406800970110     C* REPERISCE I DATI DELL'UTENTE
406900020923     C                   EXSR      REPDATIUTE
407000970110     C*
407100970110     C* REPERISCE LA DATA CORRENTE
407200970110     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
407300970110     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
407400970110     C                   Z-ADD     WN8           G08DAT
407500970110     C                   Z-ADD     *ZEROS        G08INV
407600970110     C                   MOVEL     '0'           G08ERR
407700970110     C                   CALL      'XSRDA8'
407800970110     C                   PARM                    WLBDA8
407900970110     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
408000970110     C*
408100970110     C* CARICA TABELLE OCCORRENTI
408200970110     C                   EXSR      CARTAB
408300970313     C*
408400970313     C* CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
408500970313     C                   EXSR      CARTAR
408600981218     C*
408700981218     C* ANNULLAMENTO TARIFFA
408800981218     C                   EXSR      CANTAR
408900021031     C*
409000021031     C* REPERIMENTO LISTA CLIENTI PADRI/FIGLI
409100021031     C                   EXSR      REPCLI
409200970110     C*
409300970110     C                   ENDSR
409400970110     O*------------------------------------------------------------------------*
409500970110     OQSYSPRT   E            TES              02
409600970110     O                       STA(1)              66
409700970110     O                       STA(2)             132
409800970110     O                       DSFIRS              20
409900970110     O                       UDATE              117 '  /  /  '
410000970110     O                                          127 'Pag.'
410100970110     O                       PAGE          Z    132
410200970110     O*------------------------
410300970110     O          E            ETAS        1
410400970110     O                       STA(3)              66
410500970110     O                       STA(4)             132
410600970423     O                       TASAAS        Z     16
410700970423     O                       TASLNP        Z     20
410800970423     O                       TASNRS        Z     23
410900970423     O                       TASNSP        Z     31
411000970423     O                       TASTBL              34
411100970423     O                       TASCTS              43
411200970110     O                       DEPERR             124
411300970110     O*------------------------
411400970110     O          E            EETX        1
411500970110     O                       STA(3)              66
411600970110     O                       STA(5)             132
411700970110     O                       PARKOV        Z     15
411800970110     O                       PARCTR              24
411900970110     O                       PARPRG              33
412000970110     O                       PARLNP        Z     53
412100970110     O                       PARFIE              62
412200970110     O                       DEPERR             124
412300970110     O*------------------------------------------------------------------------*
412400970110** -DDEC-
412500970110NON CALCOLARE                 1
412600970110CALCOLARE                     2
412700970110CALCOLARE IN REGIONE          3
412800970110CALCOLARE IN NAZIONE          4
412900970110 **  CREAZIONE DATI  TIPO  ** 5
413000980821TUTTA L'ITALIA                6
413100980821TUTTO L'ESTERO                7
4132009808218
4133009808219
41340098082110
413500990812** MTAS - MESSAGGI PER TITAS
413600970110FILIALE BOLLA NON TROVATA                                         1
413700970423CODICE TASSAZIONE NON TROVATO O NON VALIDO                        2
4138009704163
4139009701104
4140009701105
414100011001** METX - MESSAGGI PER TEETX
414200970110DATI TIPO ALLOCATI                                                1
414300971008NESSUNA SPEDIZIONE: VALORE DEDOTTO                                2
414400980901CREAZIONE AUTOMATICA                                              3
4145009701104
4146009701105
414700970110** STA
414800970110XXXXXXXXXXXXXXXXXXXX        TNTE41R       ** ERRORI IN CREAZIONE D1
414900970110ATI TIPO TARIFFE DA CONTO ECONOMICO **     99/99/99      PAG. 99992
415000970423SPEDIZIONE: 9999 999 99 9999999 XX  CTS: XX                       3
415100970423ERRORE: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        4
415200970110CLIENTE 9999999  TAR 999  PRG 999  LINEA PARTENZA 999   I/E  X    5
