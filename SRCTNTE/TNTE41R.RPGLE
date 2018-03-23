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
015701160726     D CLE             S              9  0 DIM(500)                             *CLIENTE TIPO C
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
019900150302     D sb01            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020000150302     D sb02            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020100150302     D sb03            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020200040811     D nbn             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020300040802     D nba             S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020400150302     D nb01            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020500150302     D nb02            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020600150302     D nb03            S              9  0 DIM(500)                             *CONSEGNE PRIVATI
020700050808     D pks             S              9  0 DIM(500)                             *CLIENTE TIPO D
020800050808     D pkn             S              9  0 DIM(500)                             *CLIENTE TIPO D
020900151202     D rtm             S              9  0 DIM(500)                             *CLIENTE TIPO D
021000150304     d detcflo       e ds
021100150304     d dar5ban       e ds
021200040811     d dar5bnb       e ds
021300151202     d dar5gen       e ds
021400060829     d teets1        e ds                  extname(teets10f) prefix(£)
021500970417     D*-------------------
021600970417     D* SCHIERE DI LAVORO
021700970417     D*-------------------
021800050103     D WCTS            S              2    DIM(600)                             *CODICI
021900050103     D WRAP            S              2    DIM(600)                             *CODICE REGIONE
022000050103     D WNAR            S              2    DIM(600)                             *CODICE NAZIONE -> P
022100050103     D WTCT            S              1    DIM(600)                             *TIPO (E/R/N)   -> P
022200970416     D*-------------------
022300970416     D* SCHIERE DI PASSAGGIO PER LA SCRITTURA
022400970416     D*-------------------
022500970416     D* NON MULTIPLE
022600970416     D §SG             S             13  3 DIM(10)                              *SCAGLIONE -KG/COLLI
022700060829     c* ricavi
022800970416     D §PS             S              7  1 DIM(10)                              *PESO
022900970416     D §CS             S              7  1 DIM(10)                              *COLLI
023000970416     D §VS             S              7  3 DIM(10)                              *VOLUME
023100970416     D §SP             S              4  1 DIM(10)                              *% INCIDENZA
023200971008     D §SD             S              1    DIM(10)                              *FLAG VALORE DEDOTTO
023300060829     c* competenze
023400060829     D §PS1            S              7  1 DIM(10)                              *PESO
023500060829     D §CS1            S              7  1 DIM(10)                              *COLLI
023600060829     D §VS1            S              7  3 DIM(10)                              *VOLUME
023700060829     D §SP1            S              4  1 DIM(10)                              *% INCIDENZA
023800060829     D §SD1            S              1    DIM(10)                              *FLAG VALORE DEDOTTO
023900970416     D*---
024000050103     D §CT             S              2    DIM(600)                             *CODICE TASSAZIONE
024100050103     D §RC             S              1    DIM(600)                             *RAGGRUPPAMENTO
024200050103     D §CP             S              4  1 DIM(600)                             *% INCIDENZA
024300050103     D §CD             S              1    DIM(600)                             *FLAG VALORE DEDOTTO
024400020227     D*-------------------
024500020227     D* SCHIERA FILIALI FEDEX
024600020227     D*-------------------
024700020227     D FILFED          S              3  0 DIM(999)
024800081105     D*-------------------
024900081105     D* SCHIERA FILIALI DPD
025000081105     D*-------------------
025100081105     D FILdpd          S              3  0 DIM(999)
025200970110     D*------------------------------------------------------------------------*
025300970110     D* INPUT
025400970110     D*------------------------------------------------------------------------*
025500970110     D*------------------
025600970110     D* DS SCHIERE MULTIPLE
025700970110     D*------------------
025800970110     D*---
025900970110     D* BYTE IN PACKED ->
026000970317     D* (3:2) +1=2 ; (2X30) =60
026100970317     D* (9:2) +1=5 ; (5X30) =150
026200970317     D* (9:2) +1=5 ; (5X999)=4995
026300970317     D* (9:2) +1=5 ; (5X10) =50
026400050103     D* (9:2) +1=5 ; (5X600)=1500
026500970317     D* (13:2)+1=7 ; (7X10) =70
026600011001     D* (18:2)+1=10; (10X10) =100
026700980820     D*---
026800971124     D FPAG            DS                  OCCURS(500) INZ
026900970110     D  GGP                    1     60P 0
027000970110     D                                     DIM(30)                              *GG PAGAMENTO
027100970110     D  SGP                   61    210P 0
027200970110     D                                     DIM(30)                              *SPED. GG PAGAMENTO
027300971124     D FRIT            DS                  OCCURS(500) INZ
027400970110     D  RIT                    1   4995P 0
027500970110     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
027600971124     D FCON            DS                  OCCURS(500) INZ
027700970110     D  CON                    1   4995P 0
027800970110     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
027900971124     D FSGL            DS                  OCCURS(500) INZ
028000041118     D  SSP                    1     50P 0
028100041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
028200041118     D  PSC                   51    110P 1
028300041118     D                                     DIM(10)                              *PESO
028400041118     D  CSC                  111    160P 0
028500041118     D                                     DIM(10)                              *COLLI
028600041118     D  VSC                  161    220P 3
028700041118     D                                     DIM(10)                              *VOLUME
028800060829     D FSGL1           DS                  OCCURS(500) INZ
028900060829     D  SSP1                   1     50P 0
029000060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
029100060829     D  PSC1                  51    110P 1
029200060829     D                                     DIM(10)                              *PESO
029300060829     D  CSC1                 111    160P 0
029400060829     D                                     DIM(10)                              *COLLI
029500060829     D  VSC1                 161    220P 3
029600060829     D                                     DIM(10)                              *VOLUME
029700971124     D FCTS            DS                  OCCURS(500) INZ
029800050103     D  TSI                    1   3000P 0
029900050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
030000050103     D  TSE                 3001   6000P 0
030100050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
030200971124     D FTAR            DS                  OCCURS(500) INZ
030300970423     D  SSGL                   1     70P 3
030400970314     D                                     DIM(10)                              *SCAGLIONI
030500050103     D  CCTI                  71   1270
030600050103     D                                     DIM(600)                             *COD.TASSAZIONE ITAL
030700050103     D  CRCI                1271   1870
030800050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ITAL
030900050103     D  CCTE                1871   3070
031000050103     D                                     DIM(600)                             *COD.TASSAZIONE ESTE
031100050103     D  CRCE                3071   3670
031200050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ESTE
031300980820     D*---
031400980820     D* CARTELLO
031500980820     D*---
031600980824     D CCAR            DS                  OCCURS(2) INZ
031700980821     D  USPE                   1      5P 0
031800980821     D                                     DIM(1)                               *N°SPEDIZIONI ITA/ES
031900980821     D  UCLA                   6     10P 0
032000980821     D                                     DIM(1)                               *CLIENTE TIPO A
032100980821     D  UCLB                  11     15P 0
032200980821     D                                     DIM(1)                               *CLIENTE TIPO B
032300980821     D  UCLC                  16     20P 0
032400980821     D                                     DIM(1)                               *CLIENTE TIPO C
032500980821     D  UBON                  21     25P 0
032600980821     D                                     DIM(1)                               *BOLLETTAZIONE (' ')
032700980821     D  UBOA                  26     30P 0
032800980821     D                                     DIM(1)                               *BOLLETTAZIONE (A)
032900980821     D  UBOB                  31     35P 0
033000980821     D                                     DIM(1)                               *BOLLETTAZIONE (B)
033100980821     D  UNCL                  36     40P 0
033200980821     D                                     DIM(1)                               *COLLI
033300980821     D  UPKB                  41     45P 1
033400980821     D                                     DIM(1)                               *PESO
033500980821     D  UVLB                  46     50P 3
033600980821     D                                     DIM(1)                               *VOLUME
033700980821     D  UFRA                  51     55P 0
033800980821     D                                     DIM(1)                               *FRANCHI
033900980821     D  UASS                  56     60P 0
034000980821     D                                     DIM(1)                               *ASSEGNATI
034100980821     D  UPCV                  61     65P 0
034200980821     D                                     DIM(1)                               *CONSEGNE PRIVATI
034300980821     D  UPCM                  66     70P 0
034400980821     D                                     DIM(1)                               *CONSEGNE MAGAZZINO
034500980821     D  UPFD                  71     75P 0
034600980821     D                                     DIM(1)                               *FERMO DEPOSITO
034700980821     D  UESP                  76     80P 0
034800980821     D                                     DIM(1)                               *ESPRESSI
034900980821     D  UPPV                  81     85P 0
035000980821     D                                     DIM(1)                               *CONSEGNE PROVINCIA
035100980821     D  UPCA                  86     90P 0
035200980821     D                                     DIM(1)                               *CONSEGNE APPUNTAMEN
035300980821     D  UPCP                  91     95P 0
035400980821     D                                     DIM(1)                               *CONSEGNE PIANI
035500980821     D  UPCS                  96    100P 0
035600980821     D                                     DIM(1)                               *CONSEGNE SUPERMERCA
035700980821     D  UPCF                 101    105P 0
035800980821     D                                     DIM(1)                               *CONSEGNE FUORI MISU
035900980821     D  UPAB                 106    110P 0
036000980821     D                                     DIM(1)                               *C/ASSEGNI BARTOLINI
036100980821     D  UPAM                 111    115P 0
036200980821     D                                     DIM(1)                               *C/ASSEGNI MITTENTE
036300011002     D  UCAS                 116    125P 5
036400011002     D                                     DIM(1)                               *C/ASSEGNO
036500011002     D                                     PACKEVEN
036600011002     D  UPAR                 126    130P 0
036700980821     D                                     DIM(1)                               *ASSICURAZIONE R.C.V
036800011002     D  UPAC                 131    135P 0
036900980821     D                                     DIM(1)                               *ASSICURAZIONE X CON
037000011002     D  UASC                 136    145P 5
037100011001     D                                     DIM(1)                               *ASSICURAZIONE
037200011001     D                                     PACKEVEN
037300011002     D  UPGI                 146    150P 0
037400980821     D                                     DIM(1)                               *GIACENZE
037500011002     D  UPCD                 151    155P 0
037600980821     D                                     DIM(1)                               *CONSEGNE DDT
037700011002     D  UPCC                 156    160P 0
037800980821     D                                     DIM(1)                               *CONSEGNE C.STORICI
037900011002     D  UPCL                 161    165P 0
038000980821     D                                     DIM(1)                               *CONSEGNE L.DISAGIAT
038100011002     D  UCLT                 166    170P 0
038200980901     D                                     DIM(1)                               *CLIENTE TIPO T
038300011002     D  UGGS                 171    175P 0
038400000414     D                                     DIM(1)                               *GG SOSTA GIACENZA
038500011002     D  UNRG                 176    180P 0
038600000414     D                                     DIM(1)                               *N° RIAPERTURE GIACE
038700011011     D  UCIS                 181    185P 0
038800011011     D                                     DIM(1)                               *CONSEGNE ISOLE
038900011011     D  URIS                 186    190P 0
039000011011     D                                     DIM(1)                               *RITIRI ISOLE
039100020215     D  UCLD                 191    195P 0
039200020215     D                                     DIM(1)                               *CLIENTE TIPO D
039300040802     D  Uded                 196    200P 0
039400040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039500040811     D  Usbnb                206    210P 0
039600040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039700040824     D  Usban                211    215P 0
039800040824     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039900040824     D  Unbn                 216    220P 0
040000040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040100040824     D  Unba                 221    225P 0
040200040811     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040300040824     D  Uval                 226    230P 0
040400040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040500050808     D  Upks                 231    235P 0
040600050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040700050808     D  Upkn                 236    240P 0
040800050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040900060315     D  Uxx1                 241    245P 0
041000060315     D                                     DIM(1)                               *ASSICURAZIONE X CON
041100060829     D  UNCL1                246    250P 0
041200060829     D                                     DIM(1)                               *COLLI
041300060829     D  UPKB1                251    255P 1
041400060829     D                                     DIM(1)                               *PESO
041500060829     D  UVLB1                256    260P 3
041600060829     D                                     DIM(1)                               *VOLUME
041700150302     D  Usb01                261    265P 0
041800150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
041900150304     D  Usb02                266    270P 0
042000150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
042100150304     D  Usb03                271    275P 0
042200150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
042300150302     D  Unb01                276    280P 0
042400150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
042500150302     D  Unb02                281    285P 0
042600150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
042700150302     D  Unb03                286    290P 0
042800150302     D                                     DIM(1)                               *CONSEGNE FUORI MISU
042900151202     D  Urtm                 291    295P 0
043000151202     D                                     DIM(1)                               *CONSEGNE FUORI MISU
043001160726     D  UCLE                 296    300P 0
043002160726     D                                     DIM(1)                               *CONSEGNE FUORI MISU
043100980824     D CPAG            DS                  OCCURS(2) INZ
043200980820     D  UGGP                   1     60P 0
043300980819     D                                     DIM(30)                              *GG PAGAMENTO
043400980820     D  USGP                  61    210P 0
043500980819     D                                     DIM(30)                              *SPED. GG PAGAMENTO
043600980824     D CRIT            DS                  OCCURS(2) INZ
043700980820     D  URIT                   1   4995P 0
043800980819     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
043900980824     D CCON            DS                  OCCURS(2) INZ
044000980820     D  UCON                   1   4995P 0
044100980819     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
044200980824     D CSGL            DS                  OCCURS(2) INZ
044300041118     D  USSP                   1     50P 0
044400041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
044500041118     D  UPSC                  51    110P 1
044600041118     D                                     DIM(10)                              *PESO
044700041118     D  UCSC                 111    160P 0
044800041118     D                                     DIM(10)                              *COLLI
044900041118     D  UVSC                 161    220P 3
045000041118     D                                     DIM(10)                              *VOLUME
045100060829     D CSGL1           DS                  OCCURS(2) INZ
045200060829     D  USSP1                  1     50P 0
045300060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
045400060829     D  UPSC1                 51    110P 1
045500060829     D                                     DIM(10)                              *PESO
045600060829     D  UCSC1                111    160P 0
045700060829     D                                     DIM(10)                              *COLLI
045800060829     D  UVSC1                161    220P 3
045900060829     D                                     DIM(10)                              *VOLUME
046000980824     D CCTS            DS                  OCCURS(2) INZ
046100050103     D  UTSI                   1   3000P 0
046200050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
046300050103     D  UTSE                3001   6000P 0
046400050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
046500040803     D Dtasflo       E DS
046600091125     D tnte04ds      E DS
046700970110     D*-------------------
046800970110     D* DS TIPI TARIFFA
046900970110     D*-------------------
047000970110     D DSTR          E DS
047100050808     D*-------------------
047200050808     D* DS cod. trattamento merce
047300050808     D*-------------------
047400050808     D DS1b          E DS
047500021202     D*-------------------
047600021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
047700021202     D*-------------------
047800160229     D TRULC7DS      E DS
047900970110     D*-------------------
048000970110     D* DS CODICI TASSAZIONE
048100970110     D*-------------------
048200970110     D DSCT          E DS
048300970110     D*-------------------
048400970110     D* DS CAMBI E DIVISE
048500970110     D*-------------------
048600011011     D*DSCV          E DS
048700970110     D*-------------------
048800970110     D* DS RAGGRUPPAMENTI TIPI BOLLA
048900970110     D*-------------------
049000970110     D DS2W          E DS
049100970110     D*------------------
049200970110     D* DS TIPI BOLLA
049300970110     D*------------------
049400970110     D DSTB          E DS
049500970110     D*------------------
049600970110     D* DS CODICI PAGAMENTO
049700970110     D*------------------
049800970110     D DSFA          E DS
049900970110     D*------------------
050000970110     D* DS COMPOSIZIONE CODICE DI PAGAMENTO
050100970110     D*------------------
050200970110     D                 DS
050300970110     D  WCDP                   1      3
050400970110     D  WLIN                   4      4
050500970110     D  DSCDP                  1      4
050600970110     D*------------------
050700970110     D* DS INVIO DATI -BOLLETTAZIONE-
050800970110     D*------------------
050900970110     D DS3C          E DS
051000970110     D*------------------
051100020923     D* DS REPERIMENTO DATI UTENTE
051200970110     D*------------------
051300020923     D TIBS34DS      E DS                                                       *Profili utente
051400020923     D DDATIUTE      E DS                                                       *Dati utente
051500020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
051600970110     D*------------------
051700970110     D* DS "XSRDA8" - CONTROLLA DATA (8)
051800970110     D*------------------
051900970110     D WLBDA8          DS
052000970110     D  G08DAT                 1      8  0
052100970110     D  G08INV                 9     16  0
052200970110     D  G08ERR                17     17
052300970110     D  G08TGI                18     22  0
052400970110     D*-------------------
052500970110     D* DS SCOMPOSIZIONE DATA DECORRENZA(4) -TNTAM-
052600970110     D*-------------------
052700970110     D                 DS
052800970110     D  TAMDNN                 3      4  0
052900970110     D  TAMDMM                 5      6  0
053000970110     D  TAMDEC                 3      6  0
053100970110     D  TAMDDT                 1      8  0
053200970110     D*-------------------
053300970110     D* DS SCOMPOSIZIONE DATA SCADENZA(4) -TNTAM-
053400970110     D*-------------------
053500970110     D                 DS
053600970110     D  TAMSNN                 3      4  0
053700970110     D  TAMSMM                 5      6  0
053800970110     D  TAMSCA                 3      6  0
053900970110     D  TAMDST                 1      8  0
054000970110     D*-------------------
054100990812     D* DS COMPOSIZIONE DATA SPEDIZIONE -TITAS-
054200970110     D*-------------------
054300970110     D                 DS
054400970110     D  TASAAS                 1      4  0
054500970110     D  TASMGS                 5      8  0
054600970110     D  TASDSP                 1      8  0
054700970110     D*------------------
054800970110     D* DS PARAMETRI DI LANCIO
054900970110     D*------------------
055000970110     D PARAM           DS
055100980825     D  PARKSC                 1      7  0
055200980825     D  PARFPA                 8     10  0
055300980825     D  PARFAR                11     13  0
055400980825     D  PARFAP                14     16  0
055500980825     D  PARDS1                17     24  0
055600980825     D  PARAA1                17     20  0
055700980825     D  PARMG1                21     24  0
055800980825     D  PARDS2                25     32  0
055900980825     D  PARTAR                33     35
056000980825     D  PARCTO                36     36
056100980825     D  PARKOV                37     43  0
056200980825     D  PARCAR                37     41  0
056300980825     D  PARCTR                44     46  0
056400980825     D  PARTTR                44     44
056500980825     D  PARPRG                47     49  0
056600980825     D  PARLNP                50     52  0
056700980825     D  PARFIE                53     53
056800980825     D  PARSIM                54     54
056900980825     D  PARELA                55     55
057000011003     D  PARDIV                56     58
057100021031     D  PARUNI                59     59
057200040804     D  PARsta                60     60
057300040805     D  PARint                61     61
057400040805     D  PARsie                62     62
057500091130     D  PARdsf                63     63
057600970110     D*------------------
057700970110     D* ARCHITETTURA
057800970110     D*------------------
057900970110     D KPJBA         E DS
058000970110     D  PROFIL                 1     10
058100011011     D*------------------
058200011011     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
058300011011     D*------------------
058400011011     D DSGEDCE       E DS
058500011011     D*-------------------
058600011011     D* CONVERSIONI IMPORTI
058700011011     D*-------------------
058800011011     D YEUR          E DS                  EXTNAME(YEURCODS)
058900020227     D*-------------------
059000020227     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
059100020227     D*-------------------
059200020227     D OG143         E DS
059300020730     D*------------------
059400020730     D* DS PER LETTURA TABELLA "NTW"
059500020730     D*------------------
059600020730     D DNTW          E DS
059700021121     D*------------------
059800021121     D* DS RIDEFINIZIONE CAMPO TAMFLO DI TNTAM
059900021121     D*------------------
060000021121     D DSTA01        E DS
060100021031     D*-------------------
060200021031     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
060300021031     D*-------------------
060400021031     D TIBS10DS      E DS
060500021031     D  SKCLI                 21   5520  0 DIM(500)                              -cli. padri/figli
060600040811     D*-------------------
060700040811     D* CAMPI DI LAVORO
060800040811     D*-------------------
060900060829     d colli           s                   like(tasncl)
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
065600021129     C*
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
095901161010     c                   if        %found(fiar531c)
096000040811     c                   if        %subst(tasgva: 1: 1) = 'O'
096100040811     c                   eval      dar5bnb = ar5uni
096200060829     c                   z-add     §ar5bcor      colli
096300040811     c                   else
096400040811     c                   eval      dar5BAN = ar5uni
096500040811     c                   end
096501161010     c                   end
096600040811     c                   end
096700040811E   1C                   ENDIF
096800970416     C*
096900970110     C                   ENDSR
097000970110     C*------------------------------------------------------------------------*
097100970110     C* MEMDAT - MEMORIZZA I DATI -BOLLE-
097200970110     C*------------------------------------------------------------------------*
097300970110     C     MEMDAT        BEGSR
097400151123     c                   movel     tasflo        dtasflo
097500970110     C*
097600970110     C* RECUPERA L'INDICE DELLA FILIALE
097700970110     C                   Z-ADD     1             F                 4 0
097800970110     C                   SETOFF                                           98
097900970430     C     DEPFIL        LOOKUP    SORG(F)                                98
098000970110IF  1C     *IN98         IFEQ      *ON                                          *FILIALE TROVATA
098100970110     C     F             OCCUR     FPAG
098200970110     C     F             OCCUR     FCON
098300970110     C     F             OCCUR     FRIT
098400970110     C     F             OCCUR     FSGL
098500060829     C     F             OCCUR     FSGL1
098600970110     C     F             OCCUR     FCTS
098700970423     C     F             OCCUR     FTAR
098800970415     C*
098900970415     C* REPERISCE LA SOLUZIONE DEL CLIENTE
099000970415     C                   MOVEL     *BLANKS       $SOL              1
099100970415     C                   EXSR      REPSOL
099200060315     C*
099300060315     C* REPERISCE le varie di fatturazione
099400060315     C                   clear                   svn
099500060315     C                   clear                   van
099600060315     C                   EXSR      REPvrf
099700970415     C*
099800060829     C* REPERISCE IL VOLUME DELLA BOLLA
099900970415     C                   EXSR      REPVOL
100000040811     C*
100100060829     C* REPERISCE IL PESO DELLA BOLLA da
100200040811     C                   EXSR      REPpes
100300970110     C*
100400060315     C* REPERISCE LE consegne particolari
100500970110     C                   MOVEL     *BLANKS       $PCA              1
100600970110     C                   MOVEL     *BLANKS       $PCP              1
100700970110     C                   MOVEL     *BLANKS       $PCS              1
100800970110     C                   MOVEL     *BLANKS       $PCF              1
100900970415     C                   MOVEL     *BLANKS       $PCD              1
101000990812     C                   MOVEL     *BLANKS       $PIN              1            *INOLTRO
101100011026     C                   MOVEL     *ZEROS        $PINIM           21 8          *IMPORTO INOLTRO
101200011011     C                   MOVEL     *BLANKS       $CIS              1
101300011011     C                   MOVEL     *BLANKS       $RIS              1
101400040802     C                   MOVEL     *BLANKS       $ded              1
101500970110     C                   EXSR      REPVAR
101600000414     C*
101700000414     C* REPERISCE LA GIACIENZA
101800000414     C                   MOVEL     *BLANKS       $PGI              1            *ESISTE GIACENZA
101900000414     C                   Z-ADD     *ZEROS        $PGIGS            9 0          *GG SOSTA
102000000414     C                   Z-ADD     *ZEROS        $PGINR            9 0          *N° RIAPERTURE
102100021122     C                   EXSR      REPGIA
102200970110     C*
102300970110     C* REPERISCE IL C/ASSEGNO
102400011026     C                   Z-ADD     *ZEROS        $CAS             21 8
102500970110     C                   MOVEL     *BLANKS       $TPI              1
102600970110IF  2C     TASFCA        IFEQ      'S'                                          *ESISTE C/ASSEGNO
102700970110     C                   EXSR      REPCAS
102800970110E   2C                   ENDIF
102900970110     C*
103000970110     C* REPERISCE L'ASSICURAZIONE
103100011026     C                   Z-ADD     *ZEROS        $IAS             21 8
103200970110     C                   EXSR      REPASS
103300970110     C*
103400970110     C* REPERISCE IL CODICE TASSAZIONE
103500970423     C                   Z-ADD     1             J
103600970423     C                   SETOFF                                           98
103700970423     C     TASCTS        LOOKUP    CTS(J)                                 98
103800970423IF  2C     *IN98         IFEQ      *ON                                          *TROVATO
103900970416     C*---
104000970110     C*  M E M O R I Z Z A
104100970416     C*---
104200970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
104300970423     C*
104400971204IF  4C     FL1(F)        IFNE      *BLANKS                                      *CODICE ESTERO
104500970423     C     CIE(J)        OREQ      'E'
104600970423     C                   Z-ADD     1             I
104700970423     C                   SETOFF                                           98
104800970423     C     TASCTS        LOOKUP    CCTE(I)                                98
104900970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
105000970423     C                   ADD       1             TSE(I)                         *N°SPED.CTS ESTERO
105100970423E   5C                   ENDIF
105200970423X   4C                   ELSE                                                   *CODICE ITALIA
105300970423     C                   Z-ADD     1             I
105400970423     C                   SETOFF                                           98
105500970423     C     TASCTS        LOOKUP    CCTI(I)                                98
105600970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
105700970423     C                   ADD       1             TSI(I)                         *N°SPED.CTS ITALIA
105800970423E   5C                   ENDIF
105900970423E   4C                   ENDIF
106000970423     C*
106100970423     C                   ADD       1             SPE(F)                         *N°SPEDIZIONI TOTALI
106200020215     C*
106300020215IF  4C     $CLV          IFEQ      'D'
106400020215     C                   ADD       1             CLD(F)                         *CLIENTE TIPO D
106500020215E   4C                   ENDIF
106600980901IF  4C     $CLV          IFEQ      'T'
106700980901     C                   ADD       1             CLT(F)                         *CLIENTE TIPO T
106800980901E   4C                   ENDIF
106900970416IF  4C     $CLV          IFEQ      'A'
107000970110     C                   ADD       1             CLA(F)                         *CLIENTE TIPO A
107100970416E   4C                   ENDIF
107200970416IF  4C     $CLV          IFEQ      'B'
107300970110     C                   ADD       1             CLB(F)                         *CLIENTE TIPO B
107400970416E   4C                   ENDIF
107500970416IF  4C     $CLV          IFEQ      'C'
107600970415     C     $CLV          OREQ      *BLANKS
107700970110     C                   ADD       1             CLC(F)                         *CLIENTE TIPO C
107800970416E   4C                   ENDIF
107801160726IF  4C     $CLV          IFEQ      'E'
107802160726     C                   ADD       1             CLE(F)                         *CLIENTE TIPO B
107803160726E   4C                   ENDIF
107900970110     C*
108000970416IF  4C     $SOL          IFEQ      *BLANKS
108100970110     C                   ADD       1             BON(F)                         *BOLLETTAZIONE (' ')
108200970416E   4C                   ENDIF
108300970416IF  4C     $SOL          IFEQ      'A'
108400970110     C                   ADD       1             BOA(F)                         *BOLLETTAZIONE (A)
108500970416E   4C                   ENDIF
108600970416IF  4C     $SOL          IFEQ      'B'
108700970110     C                   ADD       1             BOB(F)                         *BOLLETTAZIONE (B)
108800970416E   4C                   ENDIF
108900151123     c* ritiro telefonico
109000151123     c                   if        §FLOADO <> ' '
109100151123     C                   ADD       1             rtm(F)                         *BOLLETTAZIONE (B)
109200151123     c                   end
109300050808     c* previsto packing list
109400050808     c                   z-add     1             xp                4 0
109500050808     c     tasctm        lookup    cpkl(xp)                               95
109600050808     c                   if        *in95
109700151119     c                   exsr      srpkl
109800151119     c                   else
109900151119     c                   add       1             pkn(f)
110000050808     c                   end
110100151123     C* se oltre 20 colli sommo i colli originali altrimenti i colli
110200151123     c* della bolla
110300040811     C                   ADD       colli         NCL(F)                         *COLLI
110400040811     C                   ADD       $pes          PKB(F)                         *PESO
110500040811     C                   ADD       $vol          VLB(F)                         *VOLUME
110600060829     C                   ADD       colli1        NCL1(F)                        *COLLI
110700060829     C                   ADD       $pes1         PKB1(F)                        *PESO
110800060829     C                   ADD       $vol1         VLB1(F)                        *VOLUME
110900970110     C*
111000970110     C                   MOVEL     TASTBL        WA1               1
111100970416IF  4C     WA1           IFEQ      'F'
111200970110     C                   ADD       1             FRA(F)                         *FRANCHI
111300970416X   4C                   ELSE
111400970110     C                   ADD       1             ASS(F)                         *ASSEGNATI
111500970416E   4C                   ENDIF
111600970110     C*
111700970110     C                   MOVEL     TASGVA        WA1
111800970416IF  4C     WA1           IFEQ      'P'
111900970110     C                   ADD       1             PCV(F)                         *CONSEGNE PRIVATI
112000970416E   4C                   ENDIF
112100040802     C* bancali bartolini
112200040802IF  4C     WA1           IFEQ      'B'
112300040824     C                   ADD       1             sban(F)                        *CONSEGNE PRIVATI
112400040809     C                   ADD       §ar5nba       nba(F)                         *CONSEGNE PRIVATI
112500040809     C                   ADD       §ar5nb2       nba(F)                         *CONSEGNE PRIVATI
112600150304     c                   if        §ar5tba = 01 or §ar5tb2 = 01
112700150304     C                   ADD       1             sb01(F)                        *CONSEGNE PRIVATI
112800150304     C                   ADD       §ar5nba       nb01(F)                        *CONSEGNE PRIVATI
112900150302     c                   end
113000150304     c                   if        §ar5tba = 02 or §ar5tb2 = 02
113100150304     C                   ADD       1             sb02(F)                        *CONSEGNE PRIVATI
113200150304     C                   ADD       §ar5nba       nb02(F)                        *CONSEGNE PRIVATI
113300150302     c                   end
113400150304     c                   if        §ar5tba = 03 or §ar5tb2 = 03
113500150304     C                   ADD       1             sb03(F)                        *CONSEGNE PRIVATI
113600150304     C                   ADD       §ar5nba       nb03(F)                        *CONSEGNE PRIVATI
113700150302     c                   end
113800040802E   4C                   ENDIF
113900040802     C* bancali oltre 20 colli
114000040802IF  4C     WA1           IFEQ      'O'
114100040811     C                   ADD       1             sbnb(F)                        *CONSEGNE PRIVATI
114200040811     C                   ADD       §ar5bnba      nbn(F)                         *CONSEGNE PRIVATI
114300040811     C                   ADD       §ar5bnb2      nbn(F)                         *CONSEGNE PRIVATI
114400040802E   4C                   ENDIF
114500040802     C* merce di valore
114600040802IF  1C     §FLOTBA       IFEQ      'V'
114700040802     C                   add       1             val(F)
114800040802E   1C                   ENDIF
114900970110     C*
115000970416IF  4C     TASFDN        IFEQ      'S'
115100011011     C                   ADD       1             PCM(F)                         *MERCE A MAGAZZINO
115200970416E   4C                   ENDIF
115300970110     C*
115400970416IF  4C     TASFFD        IFEQ      'S'
115500970110     C                   ADD       1             PFD(F)                         *FERMO DEPOSITO
115600970416E   4C                   ENDIF
115700970110     C*
115800970416IF  4C     TASTSP        IFEQ      'E'
115900970110     C                   ADD       1             ESP(F)                         *ESPRESSI
116000970416E   4C                   ENDIF
116100970110     C*
116200011011IF  4C     TASFIN        IFEQ      'D'
116300011011     C                   ADD       1             PCL(F)                         *CONS. L.DISAGIATE
116400011011E   4C                   ENDIF
116500011011     C*
116600011011IF  4C     TASFIN        IFEQ      'I'
116700011011     C                   ADD       1             CIS(F)                         *CONS. ISOLE
116800011011E   4C                   ENDIF
116900011011     C*
117000011011IF  4C     TASFAP        IFEQ      'I'
117100011011     C                   ADD       1             RIS(F)                         *RITIRI ISOLE
117200011011E   4C                   ENDIF
117300011011     C*
117400011011IF  4C     TASFIN        IFEQ      'P'
117500110623IF  4C     TASFIN        oreq      'Z'
117600011011     C                   ADD       1             PPV(F)                         *CONSEGNE PROVINCIA
117700970416E   4C                   ENDIF
117800970110     C*
117900970416IF  4C     $PCA          IFEQ      'S'
118000970110     C                   ADD       1             PCA(F)                         *CONS. APPUNTAMENTO
118100970416E   4C                   ENDIF
118200970110     C*
118300970416IF  4C     $PCP          IFEQ      'S'
118400970110     C                   ADD       1             PCP(F)                         *CONS. PIANI
118500970416E   4C                   ENDIF
118600970110     C*
118700970416IF  4C     $PCS          IFEQ      'S'
118800970110     C                   ADD       1             PCS(F)                         *CONS. SUPERMERCATI
118900970416E   4C                   ENDIF
119000970110     C*
119100970416IF  4C     $PCF          IFEQ      'S'
119200970110     C                   ADD       1             PCF(F)                         *CONS. FUORI MISURA
119300970416E   4C                   ENDIF
119400040802     C* destinatario disagiato
119500040802IF  4C     $ded          IFEQ      'S'
119600040802     C                   ADD       1             ded(F)                         *CONS. FUORI MISURA
119700040802E   4C                   ENDIF
119800970415     C*
119900970416IF  4C     $PCD          IFEQ      'S'
120000970415     C                   ADD       1             PCD(F)                         *CONS. DDT
120100970416E   4C                   ENDIF
120200970110     C*
120300970416IF  4C     $GGP          IFGT      *ZEROS
120400970110     C                   Z-ADD     1             I
120500970110     C                   SETOFF                                           98
120600970110     C     $GGP          LOOKUP    GGP(I)                                 98
120700970416IF  5C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
120800970110     C                   ADD       1             SGP(I)                         *SPED. GG PAGAMENTO
120900970416X   5C                   ELSE                                                   *MAI MEMORIZZATI
121000970110     C                   Z-ADD     1             I
121100970416DO  6C     I             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
121200970416IF  7C     GGP(I)        IFEQ      *ZEROS                                       *DISPONIBILE
121300970110     C                   Z-ADD     $GGP          GGP(I)                         *GG
121400970110     C                   Z-ADD     1             SGP(I)                         *SPED. GG PAGAMENTO
121500981014     C                   LEAVE                                                  *ESCE DAL CICLO
121600970416E   7C                   ENDIF
121700970416E   6C                   ENDDO
121800970416E   5C                   ENDIF
121900970416E   4C                   ENDIF
122000000411     C*
122100000411     C                   MOVEL     TASTBL        WA1               1
122200000411IF  4C     WA1           IFEQ      'F'                                          *SOLO FRANCHI
122300000411IF  5C     TASSTR        IFGT      *ZEROS
122400990812     C     TASSTR        ANDNE     99999
122500000411IF  6C     TASSTR        IFGT      999                                          *CAMPO 5,0 -> 3,0
122600990812     C                   Z-ADD     999           TASSTR
122700000411E   6C                   ENDIF
122800970110     C                   Z-ADD     TASSTR        I                              *STOP RITIRO
122900970110     C                   ADD       1             RIT(I)                         *SPEDIZIONI STOP
123000000411E   5C                   ENDIF
123100000411     C*
123200000411X   4C                   ELSE                                                   *SOLO ASSEGNATI
123300000411     C*
123400000411IF  5C     TASSTP        IFGT      *ZEROS
123500990812     C     TASSTP        ANDNE     99999
123600000411IF  6C     TASSTP        IFGT      999                                          *CAMPO 5,0 -> 3,0
123700990812     C                   Z-ADD     999           TASSTP
123800000411E   6C                   ENDIF
123900970110     C                   Z-ADD     TASSTP        I                              *STOP CONSEGNA
124000970110     C                   ADD       1             CON(I)                         *SPEDIZIONI STOP
124100000411E   5C                   ENDIF
124200000411E   4C                   ENDIF
124300970110     C*
124400060829     c* per ricavi
124500970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
124600970110     C     PARTTR        OREQ      '3'                                          *KG
124700040811     C                   Z-ADD     $pes          TASSGL           13 3          *SCAGLIONE IN KG
124800970416X   4C                   ELSE
124900040811     C                   Z-ADD     colli         TASSGL                         *SCAGLIONE IN COLLI
125000040811E   4C                   ENDIF
125100971024IF  4C     TASSGL        IFGT      *ZEROS
125200971024DO  5C     1             DO        10            I
125300971024IF  6C     TASSGL        IFLE      SSGL(I)
125400970110     C                   ADD       1             SSP(I)                         *N°SPED SCAGLIONE
125500040811     C                   ADD       $pes          PSC(I)                         *PESO
125600040811     C                   ADD       colli         CSC(I)                         *COLLI
125700970415     C                   ADD       $VOL          VSC(I)                         *VOLUME
125800970110     C                   LEAVE
125900971024E   6C                   ENDIF
126000971024E   5C                   ENDDO
126100040920     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
126200060829IF  5C     PARCTO        IFEQ      'T'
126300040920     C     PARCAR        ANDEQ     88888
126400040920     C     PARLNP        ANDEQ     999
126500040920     C                   EXSR      SRSGL8
126600060829E   5C                   ENDIF
126700971024E   4C                   ENDIF
126800060829     c* per competenze
126900060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
127000060829     C     PARTTR        OREQ      '3'                                          *KG
127100060829     C                   Z-ADD     $pes1         TASSGL           13 3          *SCAGLIONE IN KG
127200060829X   4C                   ELSE
127300060829     C                   Z-ADD     colli1        TASSGL                         *SCAGLIONE IN COLLI
127400060829E   4C                   ENDIF
127500060829IF  4C     TASSGL        IFGT      *ZEROS
127600060829DO  5C     1             DO        10            I
127700060829IF  6C     TASSGL        IFLE      SSGL(I)
127800060829     C                   ADD       1             SSP1(I)                        *N°SPED SCAGLIONE
127900060829     C                   ADD       $pes1         PSC1(I)                        *PESO
128000060829     C                   ADD       colli1        CSC1(I)                        *COLLI
128100060829     C                   ADD       $VOL1         VSC1(I)                        *VOLUME
128200060829     C                   LEAVE
128300060829E   6C                   ENDIF
128400060829E   5C                   ENDDO
128500060829     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
128600060829IF  5C     PARCTO        IFEQ      'T'
128700060829     C     PARCAR        ANDEQ     88888
128800060829     C     PARLNP        ANDEQ     999
128900060829     C                   EXSR      SRSGL81
129000060829E   5C                   ENDIF
129100060829E   4C                   ENDIF
129200971024E   3C                   ENDIF
129300970110     C*
129400970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
129500970416     C     DEPRI         OREQ      'S'                                          *BOLLA VALIDA RICAVI
129600970416IF  4C     $CAS          IFGT      *ZEROS
129700970110     C                   ADD       $CAS          CAS(F)                         *C/ASSEGNO
129800970416IF  5C     $TPI          IFEQ      'M'
129900970110     C                   ADD       1             PAM(F)                         *MITTENTE
130000970416X   5C                   ELSE
130100970110     C                   ADD       1             PAB(F)                         *BARTOLINI
130200970416E   5C                   ENDIF
130300970416E   4C                   ENDIF
130400970110     C*
130500970416IF  4C     $IAS          IFGT      *ZEROS
130600970110     C                   ADD       $IAS          ASC(F)                         *ASSICURAZIONE
130700970110     C                   ADD       1             PAC(F)                         *ASSICURAZIONE CONTO
130800970416X   4C                   ELSE
130900060315     c                   if        flgacbase = *on
131000060315     C                   ADD       1             xx1(F)                         *ASSICURAZIONE R.C.V
131100060315     c                   else
131200970110     C                   ADD       1             PAR(F)                         *ASSICURAZIONE R.C.V
131300970416E   4C                   ENDIF
131400060315E   4C                   ENDIF
131500970110     C*
131600000414IF  4C     $PGI          IFEQ      'S'
131700970110     C                   ADD       1             PGI(F)                         *GIACENZE
131800000414     C                   ADD       $PGIGS        GGS(F)                         *GG SOSTA GIACENZA
131900000414     C                   ADD       $PGINR        NRG(F)                         *N° RIAPERTURE GIACE
132000970416E   4C                   ENDIF
132100970416E   3C                   ENDIF
132200970110     C*
132300970416X   2C                   ELSE                                                   *CON.TASS.NON TROVAT
132400970416     C                   MOVEL     MTAS(2)       DEPERR           50
132500970110     C                   MOVEL     'T'           DEPTER            1
132600970110     C                   EXSR      STAERR
132700970416E   2C                   ENDIF
132800970110     C*
132900970110X   1C                   ELSE                                                   *FILIALE NON TROVATA
133000970110     C                   MOVEL     MTAS(1)       DEPERR
133100970110     C                   MOVEL     'T'           DEPTER
133200970110     C                   EXSR      STAERR
133300970110E   1C                   ENDIF
133400970110     C*
133500970110     C                   ENDSR
133600151119     C*------------------------------------------------------------------------*
133700151119     C* fiar5 tipo record GEN per packing list
133800151119     C*------------------------------------------------------------------------*
133900151119     C     srpkl         BEGSR
134000151119     c                   clear                   dar5gen
134100151209     c                   if        tasaas > 2015
134200151119     c                   eval      ar5trd = 'GEN'
134300151119     c                   exsr      srfiar5
134301161010     c                   if        %found(fiar531c)
134400151119     c                   eval      dar5gen = ar5uni
134500151119     c                   if        §AR5als ='S'
134600151119     c                             or §AR5alx  ='S'
134700151119     c                   add       1             pks(f)
134800151119     c                   else
134900151119     c                   add       1             pkn(f)
135000151119     c                   end
135001161010     c                   end
135100151119     c                   else
135200151119     c                   add       1             pks(f)
135300151119     c                   end
135400151119     C                   ENDSR
135500970415     C*------------------------------------------------------------------------*
135600040809     C* fiar5 tipo record BAN
135700970415     C*------------------------------------------------------------------------*
135800040809     C     srfiar5       BEGSR
135900040809     c*
136000040809     c                   z-add     tasaas        ar5aas
136100040809     c                   z-add     taslnp        ar5lnp
136200040809     c                   z-add     tasnrs        ar5nrs
136300040809     c                   z-add     tasnsp        ar5nsp
136400040811     c                   clear                   ar5uni
136500040809     c     kar5          chain     fiar531c
136600040809     C*
136700040809     C                   ENDSR
136800040809     C*------------------------------------------------------------------------*
136900040809     C* REPSOL - REPERISCE LA SOLUZIONE DI INVIO DATI -BOLLETTAZIONE-
137000040809     C*------------------------------------------------------------------------*
137100040809     C     REPSOL        BEGSR
137200970415     C*---
137300970415     C* NON E' PRESENTE IN TABELLA -> NESSUNA PROCEDURA            ( )
137400970415     C* NON HA IL NUMERO DI SERIE  -> STAMPA LE BOLLE              (A)
137500970415     C* HA     IL NUMERO DI SERIE  -> STAMPA LE BOLLE + SEGNACOLLI (B)
137600040310     C*--- escludo bolle recapito contrassegni
137700970415IF  1C     TASNRS        IFGT      *ZEROS
137800040310IF  1C     TAScbo        andne     'FW'
137900970415     C                   MOVEL     'B'           $SOL                           *PROCEDURA 'B'
138000970415X   1C                   ELSE
138100100303     C                   Z-ADD     1             KTBKUT
138200100303     C                   MOVEL     '3C'          KTBCOD
138300100303     c                   movel(p)  tasksc        ktbkey
138400100303     c     keytab        chain     tabel00f
138500100303     c                   if        %found(tabel00f) and tblflg = ' '
138600100303     C                   MOVEL     TBLUNI        DS3C
138700100303IF  3C     §3COKD        IFEQ      '*'
138800970415     C                   MOVEL     'A'           $SOL                           *PROCEDURA 'A'
138900970415X   3C                   ELSE
139000970415     C                   MOVEL     *BLANKS       $SOL                           *PROCEDURA NO AVVIAT
139100970415E   3C                   ENDIF
139200970415E   2C                   ENDIF
139300970415E   1C                   ENDIF
139400970415     C*
139500970415     C                   ENDSR
139600970415     C*------------------------------------------------------------------------*
139700970415     C* REPVOL - REPERISCE IL VOLUME DELLA BOLLA
139800970415     C*------------------------------------------------------------------------*
139900970415     C     REPVOL        BEGSR
140000060829     C* colli x ricavi
140100060829     C                   Z-ADD     *ZEROS        $VOL              5 3
140200970415     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
140300040811    1C     TASVLC        IFGT      *ZEROS
140400040811     C     colli         ANDEQ     TASNCR
140500970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
140600970415E   1C                   ENDIF
140700970415     C*
140800970415     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
140900970415     C     TASVLC        IFGT      TASVLF
141000040811     C     colli         ANDNE     TASNCR
141100970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
141200970415E   1C                   ENDIF
141300970415     C*
141400970415     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
141500970415IF  1C     $VOL          IFEQ      *ZEROS
141600970415     C                   Z-ADD     TASVLF        $VOL                           *VOLUME NORMALE
141700060829E   1C                   ENDIF
141800060829c*   c*colli x competetnze
141900060829     C                   Z-ADD     *ZEROS        $VOL1             5 3
142000060829     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
142100060829    1C     TASVLC        IFGT      *ZEROS
142200060829     C     colli1        ANDEQ     TASNCR
142300060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
142400060829E   1C                   ENDIF
142500060829     C*
142600060829     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
142700060829     C     TASVLC        IFGT      TASVLF
142800060829     C     colli1        ANDNE     TASNCR
142900060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
143000060829E   1C                   ENDIF
143100060829     C*
143200060829     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
143300060829IF  1C     $VOL1         IFEQ      *ZEROS
143400060829     C                   Z-ADD     TASVLF        $VOL1                          *VOLUME NORMALE
143500060829E   1C                   ENDIF
143600970415     C*
143700000414     C                   ENDSR
143800040811     C*------------------------------------------------------------------------*
143900040811     C* REPpes - REPERISCE IL peso DELLA BOLLA
144000040811     C*------------------------------------------------------------------------*
144100040811     C     REPpes        BEGSR
144200060829     C* colli x ricavi
144300060829     C                   Z-ADD     *ZEROS        $pes
144400040811     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
144500040811    1C     TASpkC        IFGT      *ZEROS
144600040811     C     colli         ANDEQ     TASNCp
144700040811     C                   Z-ADD     TASpkc        $pes                           *VOLUME CML
144800040811E   1C                   ENDIF
144900040811     C*
145000040811     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
145100040811     C     TASpkc        IFGT      TASpkF
145200040811     C     colli         ANDNE     TASNCp
145300040811     C                   Z-ADD     TASpkC        $pes                           *VOLUME CML
145400040811E   1C                   ENDIF
145500040811     C*
145600040811     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
145700040811IF  1C     $pes          IFEQ      *ZEROS
145800040811     C                   Z-ADD     TASpkF        $pes                           *VOLUME NORMALE
145900040811E   1C                   ENDIF
146000060829     C* colli x competenze
146100060829     C                   Z-ADD     *ZEROS        $pes1
146200060829     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
146300060829    1C     TASpkC        IFGT      *ZEROS
146400060829     C     colli1        ANDEQ     TASNCp
146500060829     C                   Z-ADD     TASpkc        $pes1                          *VOLUME CML
146600060829E   1C                   ENDIF
146700060829     C*
146800060829     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
146900060829     C     TASpkc        IFGT      TASpkF
147000060829     C     colli1        ANDNE     TASNCp
147100060829     C                   Z-ADD     TASpkC        $pes1                          *VOLUME CML
147200060829E   1C                   ENDIF
147300060829     C*
147400060829     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
147500060829IF  1C     $pes1         IFEQ      *ZEROS
147600060829     C                   Z-ADD     TASpkF        $pes1                          *VOLUME NORMALE
147700060829E   1C                   ENDIF
147800040811     C*
147900040811     C                   ENDSR
148000970110     C*------------------------------------------------------------------------*
148100970110     C* REPCAS - REPERISCE IL CONTRASSEGNO
148200970110     C*------------------------------------------------------------------------*
148300970110     C     REPCAS        BEGSR
148400970110     C*
148500970110     C                   Z-ADD     TASAAS        KSBAAS                         *ANNO
148600970110     C                   Z-ADD     TASLNP        KSBLNP                         *LINEA PARTENZA
148700970110     C                   Z-ADD     TASNRS        KSBNRS                         *SERIE
148800970110     C                   Z-ADD     TASNSP        KSBNSP                         *SPEDIZIONE
148900970110     C     KEYCSB        CHAIN     TNCSB03L                           98
149000970110IF  1C     *IN98         IFEQ      *OFF                                         *TROVATO
149100970110IF  2C     CSBSTA        IFNE      9                                            *NO ANNULLATO
149200970110     C                   MOVEL     CSBTPI        $TPI                           *TIPO INCASSO
149300011011     C                   Z-ADD     CSBCAS        $CAS                           *C/ASSEGNO
149400011011     C     CSBVCA        IFNE      PARDIV
149500011011     C                   Z-ADD     $CAS          WIMI             30 9
149600011011     C                   MOVEL     CSBVCA        WDVI              3
149700011011     C                   EXSR      CNVEUR
149800011011     C                   Z-ADD     WIMO          $CAS
149900011011     C                   ENDIF
150000970110E   2C                   ENDIF
150100970110E   1C                   ENDIF
150200970110     C*
150300970110     C                   ENDSR
150400021122     C*------------------------------------------------------------------------*
150500021122     C* REPGIA - REPERISCE LE GIACENZE
150600021122     C*------------------------------------------------------------------------*
150700021122     C     REPGIA        BEGSR
150800021122     C*
150900021122     C* AZZERA LE VARIABILI DI LAVORO
151000021122     C                   Z-ADD     *ZEROS        §GGSOS            3 0
151100021122     C*
151200021122     C* LEGGE TUTTE LE GIACENZE DELLA BOLLA
151300021122     C                   Z-ADD     TASAAS        KGPAAS
151400021122     C                   Z-ADD     TASLNP        KGPLNP
151500021122     C                   Z-ADD     TASNRS        KGPNRS
151600021122     C                   Z-ADD     TASNSP        KGPNSP
151700050309     C     K01GCP        SETLL     TIGCP51L
151800050309     C     K01GCP        READE     TIGCP51L                               98
151900021122DO  1C     *IN98         DOWEQ     *OFF
152000021122IF  2C     GCPATB        IFEQ      *BLANKS
152100021122     C                   MOVEL     'S'           $PGI                           *ESISTE GIACENZA
152200021122     C* APERTURE
152300021122     C                   ADD       1             $PGINR                         *NUMERO RIAPERTURE
152400021122     C* GIORNI DI SOSTA
152500021122IF  3C     GCPDDM        IFGT      *ZEROS                                       *IMMESSE DISPOSIZION
152600021122IF  4C     GCPDUR        IFGT      *ZEROS                                       *ULTIMA RIAPERTURA
152700021122     C                   Z-ADD     GCPDUR        G08INV
152800021122X   4C                   ELSE                                                   *APERTURA GIACENZA
152900021122     C                   MOVEL     GCPAGC        G08INV
153000021122     C                   MOVE      GCPMGC        G08INV
153100021122E   4C                   ENDIF
153200021122     C                   Z-ADD     *ZEROS        G08DAT
153300021122     C                   MOVEL     '3'           G08ERR
153400021122     C                   CALL      'XSRDA8'
153500021122     C                   PARM                    WLBDA8
153600021122     C                   Z-ADD     G08TGI        §GGDA             5 0
153700021122IF  4C     GCPDED        IFGT      *ZEROS                                       *DISPOSIZ.GIACENZA
153800021122     C                   Z-ADD     GCPDED        G08INV
153900021122X   4C                   ELSE                                                   *DISPOSIZ.MITTENTE
154000021122     C                   Z-ADD     GCPDDM        G08INV
154100021122E   4C                   ENDIF
154200021122     C                   Z-ADD     *ZEROS        G08DAT
154300021122     C                   MOVEL     '3'           G08ERR
154400021122     C                   CALL      'XSRDA8'
154500021122     C                   PARM                    WLBDA8
154600021122     C                   Z-ADD     G08TGI        §GGA              5 0
154700021122     C* GIORNI DI SOSTA
154800021122     C     §GGA          SUB       §GGDA         §GGXX             3 0
154900021122     C                   ADD       1             §GGXX
155000021122     C                   ADD       §GGXX         §GGSOS
155100021122E   3C                   ENDIF
155200021122E   2C                   ENDIF
155300050309     C     K01GCP        READE     TIGCP51L                               98
155400021122E   1C                   ENDDO
155500021122     C*
155600021122     C                   Z-ADD     §GGSOS        $PGIGS                         *GIORNI DI SOSTA
155700021122     C*
155800021122     C                   ENDSR
155900970110     C*------------------------------------------------------------------------*
156000970110     C* REPASS - REPERISCE L'ASSICURAZIONE
156100970110     C*------------------------------------------------------------------------*
156200970110     C     REPASS        BEGSR
156300060315     c                   clear                   flgacbase         1
156400970110     C*
156500970110IF  1C     TASIAS        IFGT      *ZEROS
156600011011     C                   Z-ADD     TASIAS        $IAS
156700011011IF  2C     TASVAS        IFNE      PARDIV                                       *DIVISA ESTERA
156800011011     C                   Z-ADD     $IAS          WIMI             30 9
156900011011     C                   MOVEL     TASVAS        WDVI              3
157000011011     C                   EXSR      CNVEUR
157100011011     C                   Z-ADD     WIMO          $IAS
157200011011     C                   ENDIF
157300060315     c* verifico se esiste ac base
157400060315     c                   else
157500060315     c                   z-add     1             v
157600060315     c     'd'           lookup    svn(v)                                 99
157700060320     c                   if        van(v) <> 0 and *in99
157800060315     c                   eval      flgacbase = *on
157900970110E   1C                   ENDIF
158000060315E   1C                   ENDIF
158100970110     C*
158200970110     C                   ENDSR
158300970110     C*------------------------------------------------------------------------*
158400060315     C* REPVrf - REPERISCE le varie di fatturazione
158500990812     C* -----------------------------------------------------------------------*
158600060315     C     REPVrf        BEGSR
158700060315     C* CARICO LE VARIE BOLLA ORIGINALE DA TITAS
158800060315     C                   Z-ADD     *ZEROS        V                 6 0
158900060315     C     tasSV1        IFNE      *BLANKS
159000060315     C                   ADD       1             V
159100060315     C                   MOVEL     tasSV1        SVN(V)
159200060315     C                   Z-ADD     tasVA1        VAN(V)
159300060315     C                   ENDIF
159400060315     C     tasSV2        IFNE      *BLANKS
159500060315     C                   ADD       1             V
159600060315     C                   MOVEL     tasSV2        SVN(V)
159700060315     C                   Z-ADD     tasVA2        VAN(V)
159800060315     C                   ENDIF
159900060315     C     tasSV3        IFNE      *BLANKS
160000060315     C                   ADD       1             V
160100060315     C                   MOVEL     tasSV3        SVN(V)
160200060315     C                   Z-ADD     tasVA3        VAN(V)
160300060315     C                   ENDIF
160400060315     C* CARICO LE VARIE BOLLA DA TITA7 (SOLO SE C'E' LA VARIA 3)
160500060315     C     tasSV3        IFNE      *BLANKS
160600060315     C                   Z-ADD     tasAAS        TA7AAS
160700060315     C                   Z-ADD     tasLNP        TA7LNP
160800060315     C                   Z-ADD     tasNRS        TA7NRS
160900060315     C                   Z-ADD     tasNSP        TA7NSP
161000060315     C                   MOVEL     tasTBL        TA7TBL
161100060315     C     KEYTA7        SETLL     TITA730C
161200060315     c                   do        *hival
161300060315     C     KEYTA7        READE     TITA730C
161400060315     c                   if        %eof(tita730c)
161500060315     c                   leave
161600060315     c                   end
161700060315if  1C                   IF        (TA7VAN = 88888888,000)
161800060315     C                   Z-ADD     *zeros        TA7VAN
161900060315e   1c                   ENDIF
162000060315     C                   ADD       1             V
162100060315     C                   MOVEL     TA7SVN        SVN(V)
162200060315     C                   Z-ADD     TA7VAN        VAN(V)
162300060315     C                   ENDDO
162400060315e   1c                   ENDIF
162500060315     c                   endsr
162600060315     C*------------------------------------------------------------------------*
162700060315     C* REPVAR - REPERISCE LE consegne particolari
162800060315     C* -----------------------------------------------------------------------*
162900060315     C     REPVAR        BEGSR
163000970110     C*
163100970110     C* VARIE -BOLLA-
163200021122IF  1C     TASFTC        IFEQ      'A'
163300021122     C     TASTC2        OREQ      'A'
163400970110     C                   MOVEL     'S'           $PCA                           *CONS APPUNTAMENTO
163500970110E   1C                   ENDIF
163600021122IF  1C     TASFTC        IFEQ      'P'
163700021122     C     TASTC2        OREQ      'P'
163800970110     C                   MOVEL     'S'           $PCP                           *CONS PIANI
163900970110E   1C                   ENDIF
164000021122IF  1C     TASFTC        IFEQ      'S'
164100021122     C     TASTC2        OREQ      'S'
164200970110     C                   MOVEL     'S'           $PCS                           *CONS SUPERMERCATI
164300970110E   1C                   ENDIF
164400021122IF  1C     TASFTC        IFEQ      'F'
164500021122     C     TASTC2        OREQ      'F'
164600970110     C                   MOVEL     'S'           $PCF                           *CONS FUORI MISURA
164700970110E   1C                   ENDIF
164800040802IF  1C     TASFTC        IFEQ      'X'
164900040802     C     TASTC2        OREQ      'X'
165000040802     C                   MOVEL     'S'           $ded                           *CONS FUORI MISURA
165100040802E   1C                   ENDIF
165200021122IF  1C     TASLL1        IFEQ      'S'
165300970415     C                   MOVEL     'S'           $PCD                           *CONS DDT
165400970415E   1C                   ENDIF
165500970110     C*
165600970110     C                   ENDSR
165700980819     C*------------------------------------------------------------------------*
165800980819     C* AGGETX - AGGIORNA I DATI TIPO
165900980819     C*------------------------------------------------------------------------*
166000980819     C     AGGETX        BEGSR
166100980819     C*
166200980819     C* PER OGNI LINEA DI PARTENZA RICHIESTA
166300980819DO  1C     1             DO        500           I
166400980819IF  2C     SORG(I)       IFGT      *ZEROS
166500980819IF  3C     SPE(I)        IFGT      *ZEROS                                       *MEMORIZZATO QC.
166600980821     C                   Z-ADD     SORG(I)       XORG
166700980819     C     I             OCCUR     FPAG
166800980819     C     I             OCCUR     FCON
166900980819     C     I             OCCUR     FRIT
167000980819     C     I             OCCUR     FSGL
167100060829     C     I             OCCUR     FSGL1
167200980819     C     I             OCCUR     FCTS
167300980819     C     I             OCCUR     FTAR                                         *DATI TARIFFA
167400980819     C*---
167500980819     C* DATI TIPO -GENERALI-
167600980819     C*---
167700980821     C                   EXSR      SCRETC
167800980819     C*---
167900980819     C* DATI TIPO -SCAGLIONI-
168000980819     C*---
168100060829     c* ricavi
168200980821     C                   EXSR      SCRETS
168300060829     c* competenze
168400060829     c                   if        etcbnb <> 0
168500060829     c                   exsr      screts1
168600060829     c                   end
168700980819     C*---
168800980819     C* DATI TIPO -CODICI TASSAZIONE-
168900980819     C*---
169000980819     C* PER OGNI TIPO TARIFFA (ITALIA/ESTERO)
169100980819DO  4C     1             DO        2             K
169200980819IF  5C     SFIE(K)       IFNE      *BLANKS
169300980821     C                   MOVEL     SFIE(K)       XFIE
169400980821     C                   EXSR      SCRETT
169500981215E   5C                   ENDIF
169600980819E   4C                   ENDDO                                                  *FINE TARIFFA ITA/ES
169700980819     C*
169800980819E   3C                   ENDIF
169900980819E   2C                   ENDIF
170000980819E   1C                   ENDDO                                                  *FINE FILIALI
170100980819     C*
170200980821     C                   ENDSR
170300980819     C*------------------------------------------------------------------------*
170400980819     C* AGGET8 - AGGIORNA I DATI TIPO DI CARTELLO
170500980819     C*------------------------------------------------------------------------*
170600980819     C     AGGET8        BEGSR
170700970110     C*
170800980819     C* PER OGNI LINEA DI PARTENZA MEMORIZZATA
170900971124DO  1C     1             DO        500           I
171000980820     C     I             OCCUR     FPAG
171100970110     C     I             OCCUR     FCON
171200970110     C     I             OCCUR     FRIT
171300970110     C     I             OCCUR     FSGL
171400060829     C     I             OCCUR     FSGL1
171500970110     C     I             OCCUR     FCTS
171600980819     C     I             OCCUR     FTAR
171700980819     C*
171800980819     C* SOMMA I DATI PER LE TARIFFE DI CARTELLO -ITA/EST- DI TUTTE LE FILIALI
171900980819IF  2C     SFL1(I)       IFEQ      *BLANKS                                      *FILIALE ITALIA
172000980824     C                   Z-ADD     1             U                 4 0
172100980819X   2C                   ELSE                                                   *FILIALE ESTERO
172200980819     C                   Z-ADD     2             U
172300980819E   2C                   ENDIF
172400980824     C     U             OCCUR     CCAR
172500980824     C     U             OCCUR     CPAG
172600980824     C     U             OCCUR     CCON
172700980824     C     U             OCCUR     CRIT
172800980824     C     U             OCCUR     CSGL
172900060829     C     U             OCCUR     CSGL1
173000980824     C     U             OCCUR     CCTS
173100980819     C*
173200980819     C* DATI TIPO -GENERALI-
173300980821     C                   ADD       SPE(I)        USPE(1)
173400980821     C                   ADD       CLA(I)        UCLA(1)
173500980821     C                   ADD       CLB(I)        UCLB(1)
173600980821     C                   ADD       CLC(I)        UCLC(1)
173601160726     C                   ADD       CLE(I)        UCLE(1)
173700980821     C                   ADD       BON(I)        UBON(1)
173800980821     C                   ADD       BOA(I)        UBOA(1)
173900980821     C                   ADD       BOB(I)        UBOB(1)
174000980821     C                   ADD       NCL(I)        UNCL(1)
174100980821     C                   ADD       PKB(I)        UPKB(1)
174200980821     C                   ADD       VLB(I)        UVLB(1)
174300060829     C                   ADD       NCL1(I)       UNCL1(1)
174400060829     C                   ADD       PKB1(I)       UPKB1(1)
174500060829     C                   ADD       VLB1(I)       UVLB1(1)
174600980821     C                   ADD       FRA(I)        UFRA(1)
174700980821     C                   ADD       ASS(I)        UASS(1)
174800980821     C                   ADD       PCV(I)        UPCV(1)
174900980821     C                   ADD       PCM(I)        UPCM(1)
175000980821     C                   ADD       PFD(I)        UPFD(1)
175100980821     C                   ADD       ESP(I)        UESP(1)
175200980821     C                   ADD       PPV(I)        UPPV(1)
175300980821     C                   ADD       PCA(I)        UPCA(1)
175400980821     C                   ADD       PCP(I)        UPCP(1)
175500980821     C                   ADD       PCS(I)        UPCS(1)
175600980821     C                   ADD       PCF(I)        UPCF(1)
175700980821     C                   ADD       PCD(I)        UPCD(1)
175800980821     C                   ADD       PCC(I)        UPCC(1)
175900980821     C                   ADD       PCL(I)        UPCL(1)
176000980821     C                   ADD       PAB(I)        UPAB(1)
176100980821     C                   ADD       PAM(I)        UPAM(1)
176200980821     C                   ADD       CAS(I)        UCAS(1)
176300980821     C                   ADD       PAR(I)        UPAR(1)
176400980821     C                   ADD       PAC(I)        UPAC(1)
176500060315     C                   ADD       xx1(I)        Uxx1(1)
176600980821     C                   ADD       ASC(I)        UASC(1)
176700980821     C                   ADD       PGI(I)        UPGI(1)
176800000414     C                   ADD       GGS(I)        UGGS(1)
176900000414     C                   ADD       NRG(I)        UNRG(1)
177000980901     C                   ADD       CLT(I)        UCLT(1)
177100011011     C                   ADD       CIS(I)        UCIS(1)
177200011011     C                   ADD       RIS(I)        URIS(1)
177300020215     C                   ADD       CLD(I)        UCLD(1)
177400050808     C                   ADD       pks(I)        Upks(1)
177500050808     C                   ADD       pkn(I)        Upkn(1)
177600151202     C                   ADD       rtm(I)        Urtm(1)
177700040802     C                   ADD       ded(I)        Uded(1)
177800040811     C                   ADD       sbnb(I)       Usbnb(1)
177900040824     C                   ADD       sban(I)       Usban(1)
178000150304     C                   ADD       nbn(I)        Unbn(1)
178100150304     C                   ADD       nba(I)        Unba(1)
178200150304     C                   ADD       sb01(I)       Usb01(1)
178300150304     C                   ADD       sb02(I)       Usb02(1)
178400150304     C                   ADD       sb03(I)       Usb03(1)
178500150304     C                   ADD       nb01(I)       Unb01(1)
178600150304     C                   ADD       nb02(I)       Unb02(1)
178700150304     C                   ADD       nb03(I)       Unb03(1)
178800040802     C                   ADD       val(I)        Uval(1)
178900980825DO  2C     1             DO        30            J
179000980825IF  3C     GGP(J)        IFGT      *ZEROS
179100980825     C                   Z-ADD     1             A                 4 0
179200980825     C                   SETOFF                                           98
179300980825     C     GGP(J)        LOOKUP    UGGP(A)                                98
179400980825IF  4C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
179500980825     C                   ADD       SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
179600980825X   4C                   ELSE                                                   *MAI MEMORIZZATI
179700980825     C                   Z-ADD     1             A
179800980825DO  5C     A             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
179900980825IF  6C     UGGP(A)       IFEQ      *ZEROS                                       *DISPONIBILE
180000980825     C                   Z-ADD     GGP(J)        UGGP(A)                        *GG
180100980825     C                   Z-ADD     SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
180200980825     C                   Z-ADD     30            A                              *ESCE DAL CICLO
180300980825E   6C                   ENDIF
180400980825     C                   ADD       1             A
180500980825E   5C                   ENDDO
180600980825E   4C                   ENDIF
180700980825E   3C                   ENDIF
180800980825E   2C                   ENDDO
180900980819DO  2C     1             DO        999           J
181000980819     C                   ADD       RIT(J)        URIT(J)
181100980819     C                   ADD       CON(J)        UCON(J)
181200980819E   2C                   ENDDO
181300980819     C*
181400040920     C* DATI TIPO -SCAGLIONI- (già caricati nella durante la lettura di
181500040920     c* titas)
181600980820     C*
181700970110     C* DATI TIPO -CODICI TASSAZIONE-
181800050103DO  2C     1             DO        600           J
181900980819     C                   ADD       TSI(J)        UTSI(J)
182000980819     C                   ADD       TSE(J)        UTSE(J)
182100980819E   2C                   ENDDO
182200970110E   1C                   ENDDO                                                  *FINE FILIALI
182300980821     C*---
182400980821     C* SCRIVE LA TARIFFA -888- ITALIA
182500980821     C*---
182600980820     C*
182700980821     C* CARICA LE SCHIERE DA ELABORARE
182800980821     C                   Z-ADD     1             U
182900980821     C                   Z-ADD     1             I
183000980821     C                   Z-ADD     888           XORG
183100980821     C                   EXSR      CARSCH
183200980821     C*
183300980821     C* DATI TIPO -GENERALI-
183400980821     C                   EXSR      SCRETC
183500980821     C*
183600980821     C* DATI TIPO -SCAGLIONI-
183700060829     c* ricavi
183800980821     C                   EXSR      SCRETS
183900060829     c* competenze
184000060829     c                   if        etcbnb <> 0
184100060829     C                   EXSR      SCRETS1
184200060829     c                   end
184300980821     C*
184400980821     C* DATI TIPO -CODICI TASSAZIONE-
184500980824     C                   MOVEL     'I'           XFIE
184600980821     C                   EXSR      SCRETT
184700980824     C*---
184800980824     C* SCRIVE LA TARIFFA -889- ESTERO
184900980824     C*---
185000980824     C*
185100980824     C* CARICA LE SCHIERE DA ELABORARE
185200980824     C                   Z-ADD     2             U
185300980824     C                   Z-ADD     1             I
185400980824     C                   Z-ADD     889           XORG
185500980824     C                   EXSR      CARSCH
185600980824     C*
185700980824     C* DATI TIPO -GENERALI-
185800980824     C                   EXSR      SCRETC
185900980824     C*
186000980824     C* DATI TIPO -SCAGLIONI-
186100060829     c* ricavi
186200980824     C                   EXSR      SCRETS
186300060829     c* competenze
186400060829     c                   if        etcbnb <> 0
186500060829     C                   EXSR      SCRETS1
186600060829     c                   end
186700980824     C*
186800980824     C* DATI TIPO -CODICI TASSAZIONE-
186900980824     C                   MOVEL     'E'           XFIE
187000980824     C                   EXSR      SCRETT
187100970110     C*
187200980819     C                   ENDSR
187300040920     C*------------------------------------------------------------------------*
187400040920     C* SRSGL8 - CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
187500040920     C*------------------------------------------------------------------------*
187600040920     C     SRSGL8        BEGSR
187700040920     C* 888 ITALIA
187800040920     C                   IF        SFL1(F) = ' '
187900040920     C                   Z-ADD     1             U
188000040920     C                   ELSE
188100040920     C                   Z-ADD     2             U
188200040920     C                   END
188300040920     C     U             OCCUR     CSGL
188400040920     C*
188500040920DO  5C     1             DO        10            I
188600040920IF  6C     TASSGL        IFLE      SGL(I)
188700040920     C                   ADD       1             USSP(I)                        *N°SPED SCAGLIONE
188800040920     C                   ADD       TASPKF        UPSC(I)                        *PESO
188900040920     C                   ADD       TASNCL        UCSC(I)                        *COLLI
189000040920     C                   ADD       $VOL          UVSC(I)                        *VOLUME
189100040920     C                   LEAVE
189200040920E   6C                   ENDIF
189300040920E   5C                   ENDDO
189400040920     C*
189500040920     C                   ENDSR
189600060829     C*------------------------------------------------------------------------*
189700060829     C* SRSGL81- CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
189800060829     C*------------------------------------------------------------------------*
189900060829     C     SRSGL81       BEGSR
190000060829     C* 888 ITALIA
190100060829     C                   IF        SFL1(F) = ' '
190200060829     C                   Z-ADD     1             U
190300060829     C                   ELSE
190400060829     C                   Z-ADD     2             U
190500060829     C                   END
190600060829     C     U             OCCUR     CSGL1
190700060829     C*
190800060829DO  5C     1             DO        10            I
190900060829IF  6C     TASSGL        IFLE      SGL(I)
191000060829     C                   ADD       1             USSP1(I)                       *N°SPED SCAGLIONE
191100060829     C                   ADD       TASPKF        UPSC1(I)                       *PESO
191200060829     C                   ADD       TASNCL        UCSC1(I)                       *COLLI
191300060829     C                   ADD       $VOL1         UVSC1(I)                       *VOLUME
191400060829     C                   LEAVE
191500060829E   6C                   ENDIF
191600060829E   5C                   ENDDO
191700060829     C*
191800060829     C                   ENDSR
191900980821     C*------------------------------------------------------------------------*
192000980821     C* CARSCH - CARICA LE SCHIERE DA ELABORARE
192100980821     C*------------------------------------------------------------------------*
192200980821     C     CARSCH        BEGSR
192300980821     C*
192400980821     C* SI POSIZIONA SULLE SCHIERE DELLA TARIFFA
192500980824     C     1             OCCUR     FPAG
192600980824     C     1             OCCUR     FCON
192700980824     C     1             OCCUR     FRIT
192800980824     C     1             OCCUR     FSGL
192900060829     C     1             OCCUR     FSGL1
193000980824     C     1             OCCUR     FCTS
193100980824     C     1             OCCUR     FTAR
193200980824     C     U             OCCUR     CCAR
193300980824     C     U             OCCUR     CPAG
193400980824     C     U             OCCUR     CCON
193500980824     C     U             OCCUR     CRIT
193600980824     C     U             OCCUR     CSGL
193700060829     C     U             OCCUR     CSGL1
193800980824     C     U             OCCUR     CCTS
193900980821     C*
194000980821     C* DATI TIPO -GENERALI-
194100980824     C                   Z-ADD     USPE(1)       SPE(1)
194200980824     C                   Z-ADD     UCLA(1)       CLA(1)
194300980824     C                   Z-ADD     UCLB(1)       CLB(1)
194400980824     C                   Z-ADD     UCLC(1)       CLC(1)
194401160726     C                   Z-ADD     UCLE(1)       CLE(1)
194500980824     C                   Z-ADD     UBON(1)       BON(1)
194600980824     C                   Z-ADD     UBOA(1)       BOA(1)
194700980824     C                   Z-ADD     UBOB(1)       BOB(1)
194800980824     C                   Z-ADD     UNCL(1)       NCL(1)
194900980824     C                   Z-ADD     UPKB(1)       PKB(1)
195000980824     C                   Z-ADD     UVLB(1)       VLB(1)
195100060829     C                   Z-ADD     UNCL1(1)      NCL1(1)
195200060829     C                   Z-ADD     UPKB1(1)      PKB1(1)
195300060829     C                   Z-ADD     UVLB1(1)      VLB1(1)
195400980824     C                   Z-ADD     UFRA(1)       FRA(1)
195500980824     C                   Z-ADD     UASS(1)       ASS(1)
195600980824     C                   Z-ADD     UPCV(1)       PCV(1)
195700980824     C                   Z-ADD     UPCM(1)       PCM(1)
195800980824     C                   Z-ADD     UPFD(1)       PFD(1)
195900980824     C                   Z-ADD     UESP(1)       ESP(1)
196000980824     C                   Z-ADD     UPPV(1)       PPV(1)
196100980824     C                   Z-ADD     UPCA(1)       PCA(1)
196200980824     C                   Z-ADD     UPCP(1)       PCP(1)
196300980824     C                   Z-ADD     UPCS(1)       PCS(1)
196400980824     C                   Z-ADD     UPCF(1)       PCF(1)
196500980824     C                   Z-ADD     UPCD(1)       PCD(1)
196600980824     C                   Z-ADD     UPCC(1)       PCC(1)
196700980824     C                   Z-ADD     UPCL(1)       PCL(1)
196800980824     C                   Z-ADD     UPAB(1)       PAB(1)
196900980824     C                   Z-ADD     UPAM(1)       PAM(1)
197000980824     C                   Z-ADD     UCAS(1)       CAS(1)
197100980824     C                   Z-ADD     UPAR(1)       PAR(1)
197200980824     C                   Z-ADD     UPAC(1)       PAC(1)
197300060315     C                   Z-ADD     Uxx1(1)       xx1(1)
197400980824     C                   Z-ADD     UASC(1)       ASC(1)
197500980824     C                   Z-ADD     UPGI(1)       PGI(1)
197600000414     C                   Z-ADD     UGGS(1)       GGS(1)
197700000414     C                   Z-ADD     UNRG(1)       NRG(1)
197800980901     C                   Z-ADD     UCLT(1)       CLT(1)
197900011011     C                   Z-ADD     UCIS(1)       CIS(1)
198000011011     C                   Z-ADD     URIS(1)       RIS(1)
198100020215     C                   Z-ADD     UCLD(1)       CLD(1)
198200050808     C                   Z-ADD     Upks(1)       pks(1)
198300050808     C                   Z-ADD     Upkn(1)       pkn(1)
198400151202     C                   Z-ADD     Urtm(1)       rtm(1)
198500040811     C                   Z-ADD     Usbnb(1)      sbnb(1)
198600040824     C                   Z-ADD     Usban(1)      sban(1)
198700040811     C                   Z-ADD     Unbn(1)       nbn(1)
198800040802     C                   Z-ADD     Unba(1)       nba(1)
198900040802     C                   Z-ADD     Uded(1)       ded(1)
199000040802     C                   Z-ADD     Uval(1)       val(1)
199100150304     C                   z-ADD     usb01(1)      sb01(1)
199200150304     C                   z-ADD     usb02(1)      sb02(1)
199300150304     C                   z-ADD     usb03(1)      sb03(1)
199400150304     C                   z-ADD     unb01(1)      nb01(1)
199500150304     C                   z-ADD     unb02(1)      nb02(1)
199600150304     C                   z-ADD     unb03(1)      nb03(1)
199700980821DO  1C     1             DO        30            J
199800980821     C                   Z-ADD     USGP(J)       SGP(J)
199900980821     C                   Z-ADD     UGGP(J)       GGP(J)
200000980821E   1C                   ENDDO
200100980821DO  1C     1             DO        999           J
200200980821     C                   Z-ADD     URIT(J)       RIT(J)
200300980821     C                   Z-ADD     UCON(J)       CON(J)
200400980821E   1C                   ENDDO
200500980821     C*
200600060829     C* DATI TIPO -SCAGLIONI- ricavi
200700980824DO  1C     1             DO        10            J
200800980825     C                   Z-ADD     USSP(J)       SSP(J)
200900980825     C                   Z-ADD     UPSC(J)       PSC(J)
201000980825     C                   Z-ADD     UCSC(J)       CSC(J)
201100980825     C                   Z-ADD     UVSC(J)       VSC(J)
201200980824E   1C                   ENDDO
201300060829     C* DATI TIPO -SCAGLIONI- competenze
201400060829DO  1C     1             DO        10            J
201500060829     C                   Z-ADD     USSP1(J)      SSP1(J)
201600060829     C                   Z-ADD     UPSC1(J)      PSC1(J)
201700060829     C                   Z-ADD     UCSC1(J)      CSC1(J)
201800060829     C                   Z-ADD     UVSC1(J)      VSC1(J)
201900060829E   1C                   ENDDO
202000980821     C*
202100980821     C* DATI TIPO -CODICI TASSAZIONE-
202200050103DO  1C     1             DO        600           J                 4 0
202300980825     C                   Z-ADD     UTSI(J)       TSI(J)
202400980825     C                   Z-ADD     UTSE(J)       TSE(J)
202500980824E   1C                   ENDDO
202600980821     C*
202700980821     C                   ENDSR
202800980821     C*------------------------------------------------------------------------*
202900980821     C* AZZSCH - AZZERA LE VARIABILI DI LAVORO
203000980821     C*------------------------------------------------------------------------*
203100980821     C     AZZSCH        BEGSR
203200980821     C*
203300980821     C                   CLEAR                   SPE                            -GENERALI-
203400980821     C                   CLEAR                   CLA
203500980821     C                   CLEAR                   CLB
203600980821     C                   CLEAR                   CLC
203601160726     C                   CLEAR                   CLE
203700980821     C                   CLEAR                   BON
203800980821     C                   CLEAR                   BOA
203900980821     C                   CLEAR                   BOB
204000980821     C                   CLEAR                   NCL
204100980821     C                   CLEAR                   PKB
204200980821     C                   CLEAR                   VLB
204300060829     C                   CLEAR                   NCL1
204400060829     C                   CLEAR                   PKB1
204500060829     C                   CLEAR                   VLB1
204600980821     C                   CLEAR                   FRA
204700980821     C                   CLEAR                   ASS
204800980821     C                   CLEAR                   PCV
204900980821     C                   CLEAR                   PCM
205000980821     C                   CLEAR                   PFD
205100980821     C                   CLEAR                   ESP
205200980821     C                   CLEAR                   PPV
205300980821     C                   CLEAR                   PCA
205400980821     C                   CLEAR                   PCP
205500980821     C                   CLEAR                   PCS
205600980821     C                   CLEAR                   PCF
205700980821     C                   CLEAR                   PCD
205800980821     C                   CLEAR                   PCC
205900980821     C                   CLEAR                   PCL
206000980821     C                   CLEAR                   PAB
206100980821     C                   CLEAR                   PAM
206200980821     C                   CLEAR                   CAS
206300980821     C                   CLEAR                   PAR
206400980821     C                   CLEAR                   PAC
206500060315     C                   CLEAR                   xx1
206600980821     C                   CLEAR                   ASC
206700980821     C                   CLEAR                   PGI
206800980821     C                   CLEAR                   SGP
206900980821     C                   CLEAR                   GGP
207000980821     C                   CLEAR                   RIT
207100980821     C                   CLEAR                   CON
207200980901     C                   CLEAR                   CLT
207300000414     C                   CLEAR                   GGS
207400000414     C                   CLEAR                   NRG
207500980821     C                   CLEAR                   SSP                            -SCAGLIONI-
207600980821     C                   CLEAR                   PSC
207700980821     C                   CLEAR                   CSC
207800980821     C                   CLEAR                   VSC
207900060829     C                   CLEAR                   SSP1                           -SCAGLIONI-
208000060829     C                   CLEAR                   PSC1
208100060829     C                   CLEAR                   CSC1
208200060829     C                   CLEAR                   VSC1
208300980821     C                   CLEAR                   TSI                            -CODICI TASSAZIONE-
208400980821     C                   CLEAR                   TSE
208500011011     C                   CLEAR                   CIS
208600011011     C                   CLEAR                   RIS
208700020215     C                   CLEAR                   CLD
208800050808     C                   CLEAR                   pks
208900050808     C                   CLEAR                   pkn
209000151202     C                   CLEAR                   rtm
209100040802     C                   CLEAR                   nba
209200150304     C                   CLEAR                   nb01
209300150304     C                   CLEAR                   nb02
209400150304     C                   CLEAR                   nb03
209500040824     C                   CLEAR                   sban
209600150304     C                   CLEAR                   sb01
209700150304     C                   CLEAR                   sb02
209800150304     C                   CLEAR                   sb03
209900040824     C                   CLEAR                   sbnb
210000040811     C                   CLEAR                   nbn
210100040802     C                   CLEAR                   ded
210200040802     C                   CLEAR                   val
210300980821     C*
210400980821     C                   ENDSR
210500980821     C*------------------------------------------------------------------------*
210600011001     C* SCRETC - SCRIVE UN RECORD DI TEETC -GENERALI-
210700980821     C*------------------------------------------------------------------------*
210800980821     C     SCRETC        BEGSR
210900980821     C*
211000060804     C                   MOVEL     'S'           KTCDSF
211100980821     C                   MOVEL     PARCTO        KTCCTO
211200980821     C                   Z-ADD     PARKOV        KTCKSC
211300980821     C                   Z-ADD     PARCTR        KTCCTR
211400980821     C                   Z-ADD     PARPRG        KTCPRG
211500980821     C                   Z-ADD     XORG          KTCLNP
211600021122     C     KEYETC        CHAIN     TEETC01L                           9697
211700980821     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
211800021122IF  2C     *IN97         IFEQ      *OFF                                         *NON ALLOCATO
211900021122IF  3C     *IN96         IFEQ      *ON
212000980821     C                   MOVEL     KTCDSF        ETCDSF
212100980821     C                   MOVEL     KTCCTO        ETCCTO
212200980821     C                   Z-ADD     KTCKSC        ETCKSC
212300980821     C                   Z-ADD     KTCCTR        ETCCTR
212400980821     C                   Z-ADD     KTCPRG        ETCPRG
212500980821     C                   Z-ADD     KTCLNP        ETCLNP
212600980821     C                   Z-ADD     DATCOR        ETCDIR
212700980821     C                   Z-ADD     *ZEROS        ETCDUV
212800021122     C                   MOVEL     *BLANKS       ETCATB
212900011001     C                   WRITE     TEETC000                                     *INSERIMENTO
213000011001     C                   UNLOCK    TEETC01L
213100980821X   3C                   ELSE
213200980821     C                   Z-ADD     DATCOR        ETCDUV
213300021122     C                   MOVEL     *BLANKS       ETCATB
213400011001     C                   UPDATE    TEETC000                                     *VARIAZIONE
213500980821E   3C                   ENDIF
213600980821X   2C                   ELSE                                                   *ALLOCATO
213700980821     C                   MOVEL     METX(1)       DEPERR
213800980821     C                   MOVEL     'X'           DEPTER
213900980821     C                   EXSR      STAERR
214000980821E   2C                   ENDIF
214100980821     C*
214200980821     C                   ENDSR
214300980821     C*------------------------------------------------------------------------*
214400011001     C* SCRETS - SCRIVE UN RECORD DI TEETS -SCAGLIONI-
214500980821     C*------------------------------------------------------------------------*
214600980821     C     SCRETS        BEGSR
214700980821     C*
214800980821     C                   EXSR      SCRSGL                                       *SRIVE SCHIERE SGL
214900980821DO  1C     1             DO        10            J
215000980821IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
215100060804     C                   MOVEL     'S'           KTSDSF
215200980821     C                   MOVEL     PARCTO        KTSCTO
215300980821     C                   Z-ADD     PARKOV        KTSKSC
215400980821     C                   Z-ADD     PARCTR        KTSCTR
215500980821     C                   Z-ADD     PARPRG        KTSPRG
215600980821     C                   Z-ADD     XORG          KTSLNP
215700980821     C                   Z-ADD     §SG(J)        KTSSGL
215800021122     C     KEYETS        CHAIN     TEETS01L                           96
215900980821     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
216000021122IF  4C     *IN96         IFEQ      *ON
216100980821     C                   MOVEL     KTSDSF        ETSDSF
216200980821     C                   MOVEL     KTSCTO        ETSCTO
216300980821     C                   Z-ADD     KTSKSC        ETSKSC
216400980821     C                   Z-ADD     KTSCTR        ETSCTR
216500980821     C                   Z-ADD     KTSPRG        ETSPRG
216600980821     C                   Z-ADD     KTSLNP        ETSLNP
216700980821     C                   Z-ADD     KTSSGL        ETSSGL
216800980821     C                   Z-ADD     DATCOR        ETSDIR
216900980821     C                   Z-ADD     *ZEROS        ETSDUV
217000021122     C                   MOVEL     *BLANKS       ETSATB
217100011001     C                   WRITE     TEETS000                                     *INSERIMENTO
217200011001     C                   UNLOCK    TEETS01L
217300980821X   4C                   ELSE
217400980821     C                   Z-ADD     DATCOR        ETSDUV
217500021122     C                   MOVEL     *BLANKS       ETSATB
217600011001     C                   UPDATE    TEETS000                                     *VARIAZIONE
217700980821E   4C                   ENDIF
217800980821E   2C                   ENDIF
217900980821E   1C                   ENDDO                                                  *FINE SCAGLIONI
218000980821     C*
218100980821     C                   ENDSR
218200060829     C*------------------------------------------------------------------------*
218300060829     C* SCRETS1- SCRIVE UN RECORD DI TEETS10F -SCAGLIONI-
218400060829     C*------------------------------------------------------------------------*
218500060829     C     SCRETS1       BEGSR
218600060829     C*
218700060829     C                   EXSR      SCRSGL1                                      *SRIVE SCHIERE SGL
218800060829DO  1C     1             DO        10            J
218900060829IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
219000060829     C                   MOVEL     'S'           KTSDSF
219100060829     C                   MOVEL     PARCTO        KTSCTO
219200060829     C                   Z-ADD     PARKOV        KTSKSC
219300060829     C                   Z-ADD     PARCTR        KTSCTR
219400060829     C                   Z-ADD     PARPRG        KTSPRG
219500060829     C                   Z-ADD     XORG          KTSLNP
219600060829     C                   Z-ADD     §SG(J)        KTSSGL
219700060829     C     KEYETS        CHAIN     TEETS11L                           96
219800060829     C                   EXSR      IMPETS1                                      *IMPOSTA I CAMPI
219900060829IF  4C     *IN96         IFEQ      *ON
220000060829     C                   MOVEL     KTSDSF        £ETSDSF
220100060829     C                   MOVEL     KTSCTO        £ETSCTO
220200060829     C                   Z-ADD     KTSKSC        £ETSKSC
220300060829     C                   Z-ADD     KTSCTR        £ETSCTR
220400060829     C                   Z-ADD     KTSPRG        £ETSPRG
220500060829     C                   Z-ADD     KTSLNP        £ETSLNP
220600060829     C                   Z-ADD     KTSSGL        £ETSSGL
220700060829     C                   Z-ADD     DATCOR        £ETSDIR
220800060829     C                   Z-ADD     *ZEROS        £ETSDUV
220900060829     C                   MOVEL     *BLANKS       £ETSATB
221000060829     C                   WRITE     TEETS100                                     *INSERIMENTO
221100060829     C                   UNLOCK    TEETS11L
221200060829X   4C                   ELSE
221300060829     C                   Z-ADD     DATCOR        £ETSDUV
221400060829     C                   MOVEL     *BLANKS       £ETSATB
221500060829     C                   UPDATE    TEETS100                                     *VARIAZIONE
221600060829E   4C                   ENDIF
221700060829E   2C                   ENDIF
221800060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
221900060829     C*
222000060829     C                   ENDSR
222100980821     C*------------------------------------------------------------------------*
222200011001     C* SCRETT - SCRIVE UN RECORD DI TEETT -CODICI TASSAZIONE-
222300980821     C*------------------------------------------------------------------------*
222400980821     C     SCRETT        BEGSR
222500980821     C*
222600980821     C                   EXSR      SCRCTS                                       *SRIVE SCHIERE CTS
222700050103DO  1C     1             DO        600           J                 4 0
222800980821IF  2C     §CT(J)        IFNE      *BLANKS                                      *CODICE ESISTENTE
222900060804     C                   MOVEL     'S'           KTTDSF
223000980821     C                   MOVEL     PARCTO        KTTCTO
223100980821     C                   Z-ADD     PARKOV        KTTKSC
223200980821     C                   Z-ADD     PARCTR        KTTCTR
223300980821     C                   Z-ADD     PARPRG        KTTPRG
223400980821     C                   Z-ADD     XORG          KTTLNP
223500980821     C                   MOVEL     XFIE          KTTFIE
223600980821     C                   MOVEL     §CT(J)        KTTCTS
223700021122     C     KEYETT        CHAIN     TEETT01L                           96
223800980821     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
223900021122IF  4C     *IN96         IFEQ      *ON
224000980821     C                   MOVEL     KTTDSF        ETTDSF
224100980821     C                   MOVEL     KTTCTO        ETTCTO
224200980821     C                   Z-ADD     KTTKSC        ETTKSC
224300980821     C                   Z-ADD     KTTCTR        ETTCTR
224400980821     C                   Z-ADD     KTTPRG        ETTPRG
224500980821     C                   Z-ADD     KTTLNP        ETTLNP
224600980821     C                   MOVEL     KTTFIE        ETTFIE
224700980821     C                   MOVEL     KTTCTS        ETTCTS
224800980821     C                   Z-ADD     DATCOR        ETTDIR
224900980821     C                   Z-ADD     *ZEROS        ETTDUV
225000021122     C                   MOVEL     *BLANKS       ETTATB
225100011001     C                   WRITE     TEETT000                                     *INSERIMENTO
225200011001     C                   UNLOCK    TEETT01L
225300980821X   4C                   ELSE
225400980821     C                   Z-ADD     DATCOR        ETTDUV
225500021122     C                   MOVEL     *BLANKS       ETTATB
225600011001     C                   UPDATE    TEETT000                                     *VARIAZIONE
225700980821E   4C                   ENDIF
225800980821E   2C                   ENDIF
225900980821E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
226000980821     C*
226100980821     C                   ENDSR
226200970110     C*------------------------------------------------------------------------*
226300970110     C* IMPETC - IMPOSTA I CAMPI DATI TIPO -GENERALI-
226400970110     C*------------------------------------------------------------------------*
226500970110     C     IMPETC        BEGSR
226600970110     C*---
226700970110     C* CAMPI CALCOLATI
226800970110     C*---
226900970110     C                   Z-ADD     *ZEROS        WMAX              9 0
227000980901     C                   MOVEL     'C'           ETCCLV
227100020215IF  1C     CLD(I)        IFGT      WMAX
227200020215     C                   Z-ADD     CLD(I)        WMAX
227300020215     C                   MOVEL     'D'           ETCCLV
227400020215E   1C                   ENDIF
227500980901IF  1C     CLT(I)        IFGT      WMAX
227600980901     C                   Z-ADD     CLT(I)        WMAX
227700020215     C                   MOVEL     'T'           ETCCLV
227800980901E   1C                   ENDIF
227900970110IF  1C     CLA(I)        IFGT      WMAX
228000970110     C                   Z-ADD     CLA(I)        WMAX
228100970110     C                   MOVEL     'A'           ETCCLV
228200970110E   1C                   ENDIF
228300970110IF  1C     CLB(I)        IFGT      WMAX
228400970110     C                   Z-ADD     CLB(I)        WMAX
228500970110     C                   MOVEL     'B'           ETCCLV
228600970110E   1C                   ENDIF
228700970110IF  1C     CLC(I)        IFGT      WMAX
228800970110     C                   Z-ADD     CLC(I)        WMAX
228900970110     C                   MOVEL     'C'           ETCCLV
229000970110E   1C                   ENDIF
229001160726IF  1C     CLE(I)        IFGT      WMAX
229002160726     C                   Z-ADD     CLE(I)        WMAX
229003160726     C                   MOVEL     'E'           ETCCLV
229004160726E   1C                   ENDIF
229100050808     c* packing list
229200050808     C                   Z-ADD     *ZEROS        Wmax
229300151210     c                   if        tasaas > 2015
229400151209     C                   clear                   ETCpkl
229500151210     c                   else
229600151210     c                   movel     'N'           etcpkl
229700151210     c                   end
229800050808IF  1C     pks(I)        IFGT      wmax
229900050808     C                   Z-ADD     pks(I)        Wmax
230000050808     C                   MOVEL     'S'           ETCpkl
230100050808E   1C                   ENDIF
230200151210IF  1C     pkn(I)        IFGT      wmax
230300151210     C                   Z-ADD     pkn(I)        Wmax
230400151210     c                   if        tasaas > 2015
230500151210     C                   clear                   ETCpkl
230600151210     c                   else
230700151210     c                   movel     'N'           etcpkl
230800151210     c                   end
230900151210E   1C                   ENDIF
231000970110     C*
231100970110     C                   Z-ADD     *ZEROS        WMAX              9 0
231200970110     C                   MOVEL     ' '           ETCBOL
231300970110IF  1C     BON(I)        IFGT      WMAX
231400970110     C                   Z-ADD     BON(I)        WMAX
231500970110     C                   MOVEL     ' '           ETCBOL
231600970110E   1C                   ENDIF
231700970415IF  1C     BOA(I)        IFGT      WMAX                                         *"A" VINCE SU " "
231800970415     C     BOA(I)        ORGT      *ZEROS
231900970110     C                   Z-ADD     BOA(I)        WMAX
232000970110     C                   MOVEL     'A'           ETCBOL
232100970110E   1C                   ENDIF
232200970415IF  1C     BOB(I)        IFGT      WMAX                                         *"B" VINCE SU "A"
232300970415     C     BOB(I)        ORGT      *ZEROS
232400970110     C                   Z-ADD     BOB(I)        WMAX
232500970110     C                   MOVEL     'B'           ETCBOL
232600970110E   1C                   ENDIF
232700060829     C* ricavi
232800970110IF  1C     NCL(I)        IFGT      *ZEROS
232900970110     C     PKB(I)        DIV       NCL(I)        ETCPMC
233000970110     C     VLB(I)        DIV       NCL(I)        ETCVMC
233100970703E   1C                   ENDIF
233200970703IF  1C     ETCPMC        IFEQ      *ZEROS
233300970110     C                   Z-ADD     0,1           ETCPMC
233400970703E   1C                   ENDIF
233500970703IF  1C     ETCVMC        IFEQ      *ZEROS
233600970110     C                   Z-ADD     0,001         ETCVMC
233700970703E   1C                   ENDIF
233800060829     c* competenze
233900060829IF  1C     NCL1(I)       IFGT      *ZEROS
234000060829     C     PKB1(I)       DIV       NCL1(I)       ETCPMC1
234100060829     C     VLB1(I)       DIV       NCL1(I)       ETCVMC1
234200060829E   1C                   ENDIF
234300060829IF  1C     ETCPMC1       IFEQ      *ZEROS
234400060829     C                   Z-ADD     0,1           ETCPMC1
234500060829E   1C                   ENDIF
234600060829IF  1C     ETCVMC1       IFEQ      *ZEROS
234700060829     C                   Z-ADD     0,001         ETCVMC1
234800060829E   1C                   ENDIF
234900970110     C*
235000011026     C     FRA(I)        MULT      100           WN153            21 8
235100011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
235200970110     C                   Z-ADD     WPER          ETCFRA
235300011001     C     ASS(I)        MULT      100           WN153
235400011001     C     WN153         DIV(H)    SPE(I)        WPER
235500970110     C                   Z-ADD     WPER          ETCASS
235600980803IF  1C     ETCFRA        IFGE      ETCASS
235700980803     C                   Z-ADD     *ZEROS        ETCASS
235800980831     C                   Z-ADD     100           ETCFRA
235900980803X   1C                   ELSE
236000980803     C                   Z-ADD     *ZEROS        ETCFRA
236100980831     C                   Z-ADD     100           ETCASS
236200980803E   1C                   ENDIF
236300970110     C*
236400011001     C     PCV(I)        MULT      100           WN153
236500011001     C     WN153         DIV(H)    SPE(I)        WPER
236600970110     C                   Z-ADD     WPER          ETCPCV
236700040802     C*
236800011001     C     PCM(I)        MULT      100           WN153
236900011001     C     WN153         DIV(H)    SPE(I)        WPER
237000970110     C                   Z-ADD     WPER          ETCPCM
237100970110     C*
237200011001     C     PFD(I)        MULT      100           WN153
237300011001     C     WN153         DIV(H)    SPE(I)        WPER
237400970110     C                   Z-ADD     WPER          ETCPFD
237500970110     C*
237600011001     C     ESP(I)        MULT      100           WN153
237700011001     C     WN153         DIV(H)    SPE(I)        WPER
237800970110     C                   Z-ADD     WPER          ETCESP
237900970110     C*
238000011001     C     PPV(I)        MULT      100           WN153
238100011001     C     WN153         DIV(H)    SPE(I)        WPER
238200970110     C                   Z-ADD     WPER          ETCPPV
238300970110     C*
238400011001     C     PCA(I)        MULT      100           WN153
238500011001     C     WN153         DIV(H)    SPE(I)        WPER
238600970110     C                   Z-ADD     WPER          ETCPCA
238700970110     C*
238800011001     C     PCP(I)        MULT      100           WN153
238900011001     C     WN153         DIV(H)    SPE(I)        WPER
239000970110     C                   Z-ADD     WPER          ETCPCP
239100970110     C*
239200011001     C     PCS(I)        MULT      100           WN153
239300011001     C     WN153         DIV(H)    SPE(I)        WPER
239400970110     C                   Z-ADD     WPER          ETCPCS
239500970110     C*
239600011001     C     PCF(I)        MULT      100           WN153
239700011001     C     WN153         DIV(H)    SPE(I)        WPER
239800970110     C                   Z-ADD     WPER          ETCPCF
239900970415     C*
240000011001     C     PCD(I)        MULT      100           WN153
240100011001     C     WN153         DIV(H)    SPE(I)        WPER
240200970415     C                   Z-ADD     WPER          ETCPCD
240300970415     C*
240400011001     C     PCC(I)        MULT      100           WN153
240500011001     C     WN153         DIV(H)    SPE(I)        WPER
240600970415     C                   Z-ADD     WPER          ETCPCC
240700970415     C*
240800011001     C     PCL(I)        MULT      100           WN153
240900011001     C     WN153         DIV(H)    SPE(I)        WPER
241000970415     C                   Z-ADD     WPER          ETCPCL
241100970110     C*
241200011001     C     PAB(I)        MULT      100           WN153
241300011001     C     WN153         DIV(H)    SPE(I)        WPER
241400970110     C                   Z-ADD     WPER          ETCPAB
241500970110     C*
241600011001     C     PAM(I)        MULT      100           WN153
241700011001     C     WN153         DIV(H)    SPE(I)        WPER
241800970110     C                   Z-ADD     WPER          ETCPAM
241900011011     C*
242000011011     C     CIS(I)        MULT      100           WN153
242100011011     C     WN153         DIV(H)    SPE(I)        WPER
242200011011     C                   Z-ADD     WPER          ETCCIS
242300011011     C*
242400011011     C     RIS(I)        MULT      100           WN153
242500011011     C     WN153         DIV(H)    SPE(I)        WPER
242600011011     C                   Z-ADD     WPER          ETCSAM
242700970110     C*
242800011001     C     PAB(I)        ADD       PAM(I)        WN153
242900970110     C                   Z-ADD     *ZEROS        ETCCAS
243000011001IF  1C     WN153         IFGT      *ZEROS
243100011001     C     CAS(I)        DIV       WN153         ETCCAS
243200970110E   1C                   ENDIF
243300011026     C                   Z-ADD     ETCCAS        $IMP             21 8
243400970417     C                   MOVEL     'E'           $FDE              1
243500011011     C                   IF        PARDIV = 'ITL'
243600011026     C                   Z-ADD     1000          $ARR             21 8
243700011011     C                   ELSE
243800011026     C                   Z-ADD     1             $ARR             21 8
243900011011     C                   ENDIF
244000970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
244100970110     C                   Z-ADD     $IMP          ETCCAS
244200970110     C*
244300970418     C     PAR(I)        ADD       PAC(I)        WN90              9 0
244400060315     C                   ADD       xx1(I)        WN90
244500011001     C     PAR(I)        MULT      100           WN153
244600011001     C     WN153         DIV(H)    WN90          WPER
244700970110     C                   Z-ADD     WPER          ETCPAR
244800970110     C*
244900011001     C     PAC(I)        MULT      100           WN153
245000011001     C     WN153         DIV(H)    WN90          WPER
245100970110     C                   Z-ADD     WPER          ETCPAC
245200060315     C*
245300060315     C     xx1(I)        MULT      100           WN153
245400060315     C     WN153         DIV(H)    WN90          WPER
245500060315     C                   Z-ADD     WPER          ETCxx1
245600970110     C*
245700970110     C                   Z-ADD     *ZEROS        ETCASC
245800970110IF  1C     PAC(I)        IFGT      *ZEROS
245900970110     C     ASC(I)        DIV       PAC(I)        ETCASC
246000970110E   1C                   ENDIF
246100970110     C                   Z-ADD     ETCASC        $IMP
246200970110     C                   MOVEL     'E'           $FDE
246300011011     C                   IF        PARDIV = 'ITL'
246400011026     C                   Z-ADD     1000          $ARR             21 8
246500011011     C                   ELSE
246600011026     C                   Z-ADD     1             $ARR             21 8
246700011011     C                   ENDIF
246800970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
246900970110     C                   Z-ADD     $IMP          ETCASC
247000970110     C*
247100000414     C                   Z-ADD     *ZEROS        ETCGGG
247200000414     C                   Z-ADD     *ZEROS        ETCNRG
247300011001     C     PGI(I)        MULT      100           WN153
247400011001     C     WN153         DIV(H)    SPE(I)        WPER
247500970110     C                   Z-ADD     WPER          ETCPGI
247600021122     C* SE LA % GIACENZE SUL FILE VIENE A 0 => AZZERO ANCHE I GG GIACENZA ED IL NUMERO APERTURE GIAC
247700021122     C                   IF        ETCPGI = *ZEROS
247800021122     C                   Z-ADD     *ZEROS        ETCGGG
247900021122     C                   Z-ADD     *ZEROS        ETCNRG
248000021122     C                   ELSE
248100000414IF  1C     PGI(I)        IFGT      *ZEROS
248200000414     C     GGS(I)        DIV       PGI(I)        ETCGGG                         *GG SOSTA
248300000414IF  2C     ETCGGG        IFEQ      *ZEROS
248400000414     C                   Z-ADD     1             ETCGGG
248500000414E   2C                   ENDIF
248600000414     C     NRG(I)        DIV       PGI(I)        ETCNRG                         *N° RIAPERTURE
248700000414IF  2C     ETCNRG        IFEQ      *ZEROS
248800000414     C                   Z-ADD     1             ETCNRG
248900000414E   2C                   ENDIF
249000000414E   1C                   ENDIF
249100021122     C                   ENDIF
249200970110     C*
249300970110     C                   Z-ADD     *ZEROS        WFRQ              3 0
249400011001     C                   Z-ADD     *ZEROS        WN153
249500970110DO  1C     1             DO        30            J
249600011001IF  2C     SGP(J)        IFGT      WN153
249700011001     C                   Z-ADD     SGP(J)        WN153
249800970110     C                   Z-ADD     GGP(J)        WFRQ
249900970110E   2C                   ENDIF
250000970110E   1C                   ENDDO
250100970110     C                   Z-ADD     WFRQ          ETCGGP
250200970110IF  1C     ETCGGP        IFEQ      999
250300970110     C                   Z-ADD     *ZEROS        ETCGGP
250400970110E   1C                   ENDIF
250500970110     C*
250600970110     C                   Z-ADD     *ZEROS        WSPE             11 0
250700000411     C                   Z-ADD     *ZEROS        WSTP             13 0
250800970110DO  1C     1             DO        999           J
250900970110IF  2C     RIT(J)        IFGT      *ZEROS
251000000411     C     RIT(J)        DIV       J             WN13             13 0
251100000411     C                   MVR                     RESTO            15 0
251200000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
251300000411     C     J             ANDNE     999                                          *STOP <> 999
251400000411     C                   ADD       1             WN13
251500000411E   3C                   ENDIF
251600970110     C     WSPE          ADD       RIT(J)        WSPE
251700970110     C     WSTP          ADD       WN13          WSTP
251800970110E   2C                   ENDIF
251900970110E   1C                   ENDDO
252000970110     C                   Z-ADD     *ZEROS        ETCSPR
252100970110IF  1C     WSTP          IFGT      *ZEROS
252200970110     C     WSPE          DIV       WSTP          ETCSPR
252300000411     C                   MVR                     RESTO            15 0
252400000411IF  2C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
252500000411     C                   ADD       1             ETCSPR                         *ARROTONDA X ECCESSO
252600000411E   2C                   ENDIF
252700000411E   1C                   ENDIF
252800970110IF  1C     ETCSPR        IFEQ      *ZEROS
252900970110     C                   Z-ADD     1             ETCSPR
253000970110E   1C                   ENDIF
253100970110     C*
253200970110     C                   Z-ADD     *ZEROS        WSPE             11 0
253300000411     C                   Z-ADD     *ZEROS        WSTP             13 0
253400970110DO  1C     1             DO        999           J
253500970110IF  2C     CON(J)        IFGT      *ZEROS
253600000411     C     CON(J)        DIV       J             WN13             13 0
253700000411     C                   MVR                     RESTO            15 0
253800000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
253900000411     C     J             ANDNE     999                                          *STOP <> 999
254000000411     C                   ADD       1             WN13
254100000411E   3C                   ENDIF
254200970110     C     WSPE          ADD       CON(J)        WSPE
254300970110     C     WSTP          ADD       WN13          WSTP
254400970110E   2C                   ENDIF
254500970110E   1C                   ENDDO
254600001221     C                   Z-ADD     1             ETCSPC
254700151202     c* ritiri telefonici: es. 100 spedizioni 18 ritiri, in media 5 sped.
254800151202     c* per ogni stop, quindi visto che
254900151202     c* il flag del ritiro telefonico viene messo solo sulla prima delle 5,
255000151202     c* metto S se prevale la maggioranza
255100151209     C                   clear                   §ETCrtm
255200151209     c                   if        etcspr > 0
255300151209     c                   eval(h)   wmax = 100/etcspr/2
255400151202IF  1C     rtm(I)        IFGT      wmax
255500151202     C                   MOVEL     'S'           §ETCrtm
255600151202E   1C                   ENDIF
255700151202E   1C                   ENDIF
255800970110     C*---
255900970110     C* CAMPI FISSI
256000970110     C*---
256100970110     C                   MOVEL     *BLANKS       ETCSDE
256200970110     C                   Z-ADD     *ZEROS        ETCPDE
256300970619     C                   MOVEL     'E'           ETCFAR
256400011011     C                   IF        PARDIV = 'ITL'
256500970314     C                   Z-ADD     100           ETCIAR
256600011011     C                   ELSE
256700011030     C                   Z-ADD     0,1           ETCIAR
256800011011     C                   ENDIF
256900970213     C                   MOVEL     'N'           ETCSIN
257000970110     C                   MOVEL     'N'           ETCIAP
257100970110     C                   MOVEL     'N'           ETCIPI
257200970110     C                   MOVEL     'N'           ETCISU
257300970110     C                   MOVEL     'N'           ETCIFU
257400970415     C                   MOVEL     'N'           ETCIDD
257500970415     C                   MOVEL     'N'           ETCICE
257600970415     C                   MOVEL     'N'           ETCILO
257700050810     C                   MOVEL     'N'           ETCIde
257800050810     C                   MOVEL     'N'           ETCIba
257900970110     C                   MOVEL     'N'           ETCIES
258000970110     C                   MOVEL     'N'           ETCICA
258100970110     C                   MOVEL     'N'           ETCIAS
258200970110     C                   MOVEL     'N'           ETCIGI
258300020716     C                   MOVEL     'N'           ETCISO
258400020716     C                   MOVEL     'N'           ETCINR
258500151209     C                   MOVEL     'N'           ETCiva
258600151209     C                   MOVEL     'N'           §ETCIrp
258700980901     C                   MOVEL     METX(3)       ETCDCV
258800970110     C*
258900970110     C                   MOVEL     *BLANKS       ETCFTR
259000970110     C                   Z-ADD     *ZEROS        ETCDTR
259100970110     C                   MOVEL     *BLANKS       ETCATB
259200021104     C*
259300021104     C* EFFETTUO ALCUNE CONSIDERAZIONI
259400021104     C                   IF        ETCBOL = 'B'
259500021104     C                   EVAL      ETCIX1 = 'K'
259600021104     C                   ELSE
259700021104     C                   EVAL      ETCIX1 = *blanks
259800021104     C                   ENDIF
259900040811     C* %sped oltre 20 colli non imbancalate
260000040811     C     sbnb(I)       MULT      100           WN153
260100040802     C     WN153         DIV(H)    SPE(I)        WPER
260200040802     C                   Z-ADD     WPER          ETCbnb
260300040811     c* numero medio bancali x spedizioni oltre 20 colli
260400061006     c                   if        sbnb(i) <> 0 and etcbnb <> 0
260500040811     c     nbn(i)        div(h)    sbnb(i)       etcnbn
260600061006     c                   if        etcnbn = 0
260700061006     c                   eval      etcnbn = 1
260800061006     c                   end
260900040812     c                   else
261000040812     c                   clear                   etcnbn
261100040811     c                   end
261200150304     C* %sped varia B
261300040824     C     sban(I)       MULT      100           WN153
261400040824     C     WN153         DIV(H)    SPE(I)        WPER
261500040824     C                   Z-ADD     WPER          ETCban
261600040824     c* numero medio bancali Bartolini
261700061006     c                   if        sban(i) <> 0 and etcban <> 0
261800040824     c     nba(i)        div(h)    sban(i)       etcnba
261900061006     c                   if        etcnba = 0
262000061006     c                   eval      etcnba = 1
262100061006     c                   end
262200150304     c* come tipo bancale metto quello usato maggiormente nelle spedizoni
262300150304     c                   eval      §etctba ='01'
262400150304     c                   if        sb02(i) > sb01(i) and
262500150304     c                             sb02(i) > sb03(i)
262600150304     c                   eval      §etctba ='02'
262700150304     c                   end
262800150304     c                   if        sb03(i) > sb01(i) and
262900150304     c                             sb03(i) > sb02(i)
263000150304     c                   eval      §etctba ='03'
263100150304     c                   end
263200150305     c                   else
263300150305     c                   clear                   etcnba
263400150305     c                   clear                   §etctba
263500150305     c                   end
263600040802     C*
263700040802     C     ded(I)        MULT      100           WN153
263800040802     C     WN153         DIV(H)    SPE(I)        WPER
263900040802     C                   Z-ADD     WPER          ETCdes
264000040802     C*
264100040802     C     val(I)        MULT      100           WN153
264200040802     C     WN153         DIV(H)    SPE(I)        WPER
264300040802     C                   Z-ADD     WPER          ETCval
264400151202     C*
264500151202     c                   movel(p)  detcflo       etcflo
264600970110     C*
264700970110     C                   ENDSR
264800970416     C*------------------------------------------------------------------------*
264900060829     C* SCRSGL - SCRIVE SCHIERE SCAGLIONI ricavi
265000970416     C*------------------------------------------------------------------------*
265100970416     C     SCRSGL        BEGSR
265200970416     C*
265300970416     C* AZZERA LE VARIABILI DI LAVORO
265400970416     C                   CLEAR                   §SG
265500970416     C                   CLEAR                   §PS
265600970416     C                   CLEAR                   §CS
265700970416     C                   CLEAR                   §VS
265800970416     C                   CLEAR                   §SP
265900971008     C                   CLEAR                   §SD
266000970416     C*
266100970416     C* PER CIASCUN SCAGLIONE DA SCRIVERE
266200970416DO  1C     1             DO        10            J
266300970416IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
266400970416     C                   Z-ADD     SSGL(J)       §SG(J)
266500970416IF  3C     SSP(J)        IFGT      *ZEROS
266600970416     C*
266700970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
266800971008     C                   MOVEL     *BLANKS       §SD(J)                         *VALORE NON DEDOTTO
266900970416     C     PSC(J)        DIV       SSP(J)        §PS(J)
267000970416     C     CSC(J)        DIV       SSP(J)        §CS(J)
267100970416     C     VSC(J)        DIV       SSP(J)        §VS(J)
267200011001     C     SSP(J)        MULT      100           WN153
267300011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
267400970416     C                   Z-ADD     WPER          §SP(J)
267500970703IF  4C     §SP(J)        IFEQ      *ZEROS
267600970703     C                   Z-ADD     0,1           §SP(J)
267700970703E   4C                   ENDIF
267800970416X   3C                   ELSE
267900970416     C*
268000970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
268100971008     C                   MOVEL     '*'           §SD(J)                         *VALORE DEDOTTO
268200970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
268300970416     C     PARTTR        OREQ      '3'                                          *KG
268400970416     C                   Z-ADD     §SG(J)        §PS(J)
268500970416     C     §PS(J)        DIV       ETCPMC        §CS(J)
268600970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
268700970416X   4C                   ELSE                                                   *COLLI
268800970416     C                   Z-ADD     §SG(J)        §CS(J)
268900040812     C     §CS(J)        mult      ETCPMC        §PS(J)
269000970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
269100970416E   4C                   ENDIF
269200970416     C                   Z-ADD     0,1           §SP(J)
269300970416E   3C                   ENDIF
269400970416E   2C                   ENDIF
269500970416E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
269600000707     C*
269700000707     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
269800000707     C                   EXSR      PARPSG
269900970416     C*
270000970416     C                   ENDSR
270100060829     C*------------------------------------------------------------------------*
270200060829     C* SCRSGL1- SCRIVE SCHIERE SCAGLIONI competenze
270300060829     C*------------------------------------------------------------------------*
270400060829     C     SCRSGL1       BEGSR
270500060829     C*
270600060829     C* AZZERA LE VARIABILI DI LAVORO
270700060829     C                   CLEAR                   §SG
270800060829     C                   CLEAR                   §PS1
270900060829     C                   CLEAR                   §CS1
271000060829     C                   CLEAR                   §VS1
271100060829     C                   CLEAR                   §SP1
271200060829     C                   CLEAR                   §SD1
271300060829     C*
271400060829     C* PER CIASCUN SCAGLIONE DA SCRIVERE
271500060829DO  1C     1             DO        10            J
271600060829IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
271700060829     C                   Z-ADD     SSGL(J)       §SG(J)
271800060829IF  3C     SSP1(J)       IFGT      *ZEROS
271900060829     C*
272000060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
272100060829     C                   MOVEL     *BLANKS       §SD1(J)                        *VALORE NON DEDOTTO
272200060829     C     PSC1(J)       DIV       SSP1(J)       §PS1(J)
272300060829     C     CSC1(J)       DIV       SSP1(J)       §CS1(J)
272400060829     C     VSC1(J)       DIV       SSP1(J)       §VS1(J)
272500060829     C     SSP1(J)       MULT      100           WN153
272600060829     C     WN153         DIV(H)    SPE(I)        WPER              4 1
272700060829     C                   Z-ADD     WPER          §SP1(J)
272800060829IF  4C     §SP1(J)       IFEQ      *ZEROS
272900060829     C                   Z-ADD     0,1           §SP1(J)
273000060829E   4C                   ENDIF
273100060829X   3C                   ELSE
273200060829     C*
273300060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
273400060829     C                   MOVEL     '*'           §SD1(J)                        *VALORE DEDOTTO
273500060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
273600060829     C     PARTTR        OREQ      '3'                                          *KG
273700060829     C                   Z-ADD     §SG(J)        §PS1(J)
273800060829     C     §PS1(J)       DIV       ETCPMC1       §CS1(J)
273900060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
274000060829X   4C                   ELSE                                                   *COLLI
274100060829     C                   Z-ADD     §SG(J)        §CS1(J)
274200060829     C     §CS1(J)       mult      ETCPMC1       §PS1(J)
274300060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
274400060829E   4C                   ENDIF
274500060829     C                   Z-ADD     0,1           §SP1(J)
274600060829E   3C                   ENDIF
274700060829E   2C                   ENDIF
274800060829E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
274900060829     C*
275000060829     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
275100060829     C                   EXSR      PARPSG1
275200060829     C*
275300060829     C                   ENDSR
275400000707     C*------------------------------------------------------------------------*
275500000707     C* PARPSG - PAREGGIA LA %TOTALE -SCAGLIONI- A 100
275600000707     C*------------------------------------------------------------------------*
275700000707     C     PARPSG        BEGSR
275800000707     C*
275900000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
276000000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
276100000707DO  1C     1             DO        10            J
276200000707IF  2C     §SG(J)        IFGT      *ZEROS
276300000707     C                   ADD       §SP(J)        TOTPPE                         *%INCIDENZA TOTALE
276400000707E   2C                   ENDIF
276500000707E   1C                   ENDDO
276600000707     C*
276700000707     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
276800000707     C                   Z-ADD     *ZEROS        WN41              4 1
276900000707     C     100           SUB       TOTPPE        WN41
277000000707     C*
277100000707     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
277200000707IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
277300000707DO  2C     WN41          DOWNE     *ZEROS
277400000707DO  3C     1             DO        10            J
277500000707IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
277600000707     C* POSITIVA -> AGGIUNGERE
277700000707IF  5C     WN41          IFGT      *ZEROS
277800000707     C     §SP(J)        ADD       0,1           §SP(J)                         *AGGIUNGE %
277900000707     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
278000000707E   5C                   ENDIF
278100000707     C* NEGATIVA -> TOGLIERE
278200000707IF  5C     WN41          IFLT      *ZEROS
278300000707IF  6C     §SP(J)        IFGT      0,1
278400000707     C     §SP(J)        SUB       0,1           §SP(J)                         *SOTTRAE %
278500000707     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
278600000707E   6C                   ENDIF
278700000707E   5C                   ENDIF
278800000707     C* UGUALE A ZERO -> USCIRE
278900000707IF  5C     WN41          IFEQ      *ZEROS
279000000707     C                   LEAVE
279100000707E   5C                   ENDIF
279200000707E   4C                   ENDIF
279300000707E   3C                   ENDDO                                                  *FINE SCAGLIONI
279400000707E   2C                   ENDDO                                                  *DIFFERENZA = 0
279500000707E   1C                   ENDIF
279600000707     C*
279700000707     C                   ENDSR
279800060829     C*------------------------------------------------------------------------*
279900060829     C* PARPSG1- PAREGGIA LA %TOTALE -SCAGLIONI- A 100
280000060829     C*------------------------------------------------------------------------*
280100060829     C     PARPSG1       BEGSR
280200060829     C*
280300060829     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
280400060829     C                   Z-ADD     *ZEROS        TOTPPE            4 1
280500060829DO  1C     1             DO        10            J
280600060829IF  2C     §SG(J)        IFGT      *ZEROS
280700060829     C                   ADD       §SP1(J)       TOTPPE                         *%INCIDENZA TOTALE
280800060829E   2C                   ENDIF
280900060829E   1C                   ENDDO
281000060829     C*
281100060829     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
281200060829     C                   Z-ADD     *ZEROS        WN41              4 1
281300060829     C     100           SUB       TOTPPE        WN41
281400060829     C*
281500060829     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
281600060829IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
281700060829DO  2C     WN41          DOWNE     *ZEROS
281800060829DO  3C     1             DO        10            J
281900060829IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
282000060829     C* POSITIVA -> AGGIUNGERE
282100060829IF  5C     WN41          IFGT      *ZEROS
282200060829     C     §SP1(J)       ADD       0,1           §SP1(J)                        *AGGIUNGE %
282300060829     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
282400060829E   5C                   ENDIF
282500060829     C* NEGATIVA -> TOGLIERE
282600060829IF  5C     WN41          IFLT      *ZEROS
282700060829IF  6C     §SP1(J)       IFGT      0,1
282800060829     C     §SP1(J)       SUB       0,1           §SP1(J)                        *SOTTRAE %
282900060829     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
283000060829E   6C                   ENDIF
283100060829E   5C                   ENDIF
283200060829     C* UGUALE A ZERO -> USCIRE
283300060829IF  5C     WN41          IFEQ      *ZEROS
283400060829     C                   LEAVE
283500060829E   5C                   ENDIF
283600060829E   4C                   ENDIF
283700060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
283800060829E   2C                   ENDDO                                                  *DIFFERENZA = 0
283900060829E   1C                   ENDIF
284000060829     C*
284100060829     C                   ENDSR
284200970110     C*------------------------------------------------------------------------*
284300060829     C* IMPETS - IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- ricavi
284400970110     C*------------------------------------------------------------------------*
284500970318     C     IMPETS        BEGSR
284600970110     C*---
284700970110     C* CAMPI CALCOLATI
284800970110     C*---
284900970416     C                   Z-ADD     §PS(J)        ETSPSC
285000970416     C                   Z-ADD     §CS(J)        ETSCSC
285100981020IF  1C     ETSCSC        IFLT      1
285200981020     C                   Z-ADD     1             ETSCSC
285300981020E   1C                   ENDIF
285400970416     C                   Z-ADD     §VS(J)        ETSVSC
285500970416     C                   Z-ADD     §SP(J)        ETSPPE
285600970415     C*---
285700970415     C* CAMPI FISSI
285800970415     C*---
285900980325     C                   Z-ADD     *ZEROS        ETSINL
286000970415     C                   Z-ADD     *ZEROS        ETSRPV
286100970415     C                   MOVEL     *BLANKS       ETSSDE
286200970415     C                   Z-ADD     *ZEROS        ETSPDE
286300970110     C*
286400040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
286500040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
286600040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
286700040805     C* MA È GIUSTO COSÌ
286800040805     C                   MOVEL     §SD(J)        ETSFTR
286900970110     C                   Z-ADD     *ZEROS        ETSDTR
287000970110     C                   MOVEL     *BLANKS       ETSATB
287100970110     C*
287200970110     C                   ENDSR
287300060829     C*------------------------------------------------------------------------*
287400060829     C* IMPETS1- IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- competenze
287500060829     C*------------------------------------------------------------------------*
287600060829     C     IMPETS1       BEGSR
287700060829     C*---
287800060829     C* CAMPI CALCOLATI
287900060829     C*---
288000060829     C                   Z-ADD     §PS1(J)       £ETSPSC
288100060829     C                   Z-ADD     §CS1(J)       £ETSCSC
288200060829IF  1C     £ETSCSC       IFLT      1
288300060829     C                   Z-ADD     1             £ETSCSC
288400060829E   1C                   ENDIF
288500060829     C                   Z-ADD     §VS1(J)       £ETSVSC
288600060829     C                   Z-ADD     §SP1(J)       £ETSPPE
288700060829     C*---
288800060829     C* CAMPI FISSI
288900060829     C*---
289000060829     C                   Z-ADD     *ZEROS        £ETSINL
289100060829     C                   Z-ADD     *ZEROS        £ETSRPV
289200060829     C                   MOVEL     *BLANKS       £ETSSDE
289300060829     C                   Z-ADD     *ZEROS        £ETSPDE
289400060829     C*
289500060829     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
289600060829     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
289700060829     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
289800060829     C* MA È GIUSTO COSÌ
289900060829     C                   MOVEL     §SD1(J)       £ETSFTR
290000060829     C                   Z-ADD     *ZEROS        £ETSDTR
290100060829     C                   MOVEL     *BLANKS       £ETSATB
290200060829     C*
290300060829     C                   ENDSR
290400970416     C*------------------------------------------------------------------------*
290500970416     C* SCRCTS - SCRIVE SCHIERE CODICI TASSAZIONE
290600970416     C*------------------------------------------------------------------------*
290700970416     C     SCRCTS        BEGSR
290800000707     C*---
290900000707     C* ITALIA
291000000707     C*---
291100000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
291200000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
291300000707     C                   CLEAR                   §RC
291400000707     C                   CLEAR                   §CP
291500000707     C                   CLEAR                   §CD
291600980821IF  1C     XFIE          IFEQ      'I'
291700970423     C                   XFOOT     TSI           TOTSPE           11 0          *TOT. SPED. ITALIA
291800050103DO  2C     1             DO        600           J
291900970423IF  3C     CCTI(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
292000000620     C     CRCI(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
292100970423     C                   MOVEL     CCTI(J)       §CT(J)
292200970423     C                   MOVEL     CRCI(J)       §RC(J)
292300970417     C*
292400970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
292500971008     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
292600970423IF  4C     TSI(J)        IFGT      *ZEROS
292700011001     C     TSI(J)        MULT      100           WN153
292800011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
292900970417     C                   Z-ADD     WPER          §CP(J)
293000970703IF  5C     §CP(J)        IFEQ      *ZEROS
293100970703     C                   Z-ADD     0,1           §CP(J)
293200970703E   5C                   ENDIF
293300970423X   4C                   ELSE
293400970417     C*
293500970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
293600971008     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
293700970417     C                   Z-ADD     0,1           §CP(J)
293800970423E   4C                   ENDIF
293900970423E   3C                   ENDIF
294000970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
294100000707     C*
294200000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
294300000707     C                   EXSR      PARPCT
294400970423     C*---
294500970423     C* ESTERO
294600970423     C*---
294700000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
294800970423X   1C                   ELSE
294900000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
295000000707     C                   CLEAR                   §RC
295100000707     C                   CLEAR                   §CP
295200000707     C                   CLEAR                   §CD
295300970423     C                   XFOOT     TSE           TOTSPE           11 0          *TOT. SPED. ITALIA
295400050103DO  2C     1             DO        600           J
295500970423IF  3C     CCTE(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
295600000620     C     CRCE(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
295700970423     C                   MOVEL     CCTE(J)       §CT(J)
295800970423     C                   MOVEL     CRCE(J)       §RC(J)
295900970423     C*
296000970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
296100990803     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
296200970423IF  4C     TSE(J)        IFGT      *ZEROS
296300011001     C     TSE(J)        MULT      100           WN153
296400011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
296500970423     C                   Z-ADD     WPER          §CP(J)
296600970703IF  5C     §CP(J)        IFEQ      *ZEROS
296700970703     C                   Z-ADD     0,1           §CP(J)
296800970703E   5C                   ENDIF
296900970423X   4C                   ELSE
297000970423     C*
297100970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
297200990803     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
297300970423     C                   Z-ADD     0,1           §CP(J)
297400970423E   4C                   ENDIF
297500970423E   3C                   ENDIF
297600970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
297700000707     C*
297800000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
297900000707     C                   EXSR      PARPCT
298000000707E   1C                   ENDIF
298100000707     C*
298200000707     C                   ENDSR
298300000707     C*------------------------------------------------------------------------*
298400000707     C* PARPCT - PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
298500000707     C*------------------------------------------------------------------------*
298600000707     C     PARPCT        BEGSR
298700000707     C*
298800000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
298900000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
299000050103DO  1C     1             DO        600           J
299100000707IF  2C     §CT(J)        IFNE      *BLANKS
299200000707     C                   ADD       §CP(J)        TOTPPE                         *%INCIDENZA TOTALE
299300000707E   2C                   ENDIF
299400000707E   1C                   ENDDO
299500970417     C*
299600970417     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
299700970417     C                   Z-ADD     *ZEROS        WN41              4 1
299800970417     C     100           SUB       TOTPPE        WN41
299900970417     C*
300000970417     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
300100970417IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
300200000707DO  2C     WN41          DOWNE     *ZEROS
300300050103DO  3C     1             DO        600           J
300400970421IF  4C     §CT(J)        IFNE      *BLANKS                                      *ELEMENTO PIENO
300500970417     C     §RC(J)        ANDNE     *BLANKS                                      *DA CALCOLARE
300600970417     C* POSITIVA -> AGGIUNGERE
300700970417IF  5C     WN41          IFGT      *ZEROS
300800970417     C     §CP(J)        ADD       0,1           §CP(J)                         *AGGIUNGE %
300900970417     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
301000970417E   5C                   ENDIF
301100970417     C* NEGATIVA -> TOGLIERE
301200970417IF  5C     WN41          IFLT      *ZEROS
301300000707IF  6C     §CP(J)        IFGT      0,1
301400970417     C     §CP(J)        SUB       0,1           §CP(J)                         *SOTTRAE %
301500970417     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
301600000707E   6C                   ENDIF
301700970417E   5C                   ENDIF
301800000707     C* UGUALE A ZERO -> USCIRE
301900000707IF  5C     WN41          IFEQ      *ZEROS
302000000707     C                   LEAVE
302100000707E   5C                   ENDIF
302200970417E   4C                   ENDIF
302300970417E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
302400970417E   2C                   ENDDO                                                  *DIFFERENZA = 0
302500970417E   1C                   ENDIF
302600970416     C*
302700970416     C                   ENDSR
302800970110     C*------------------------------------------------------------------------*
302900970110     C* IMPETT - IMPOSTA I CAMPI DATI TIPO -CODICI TASSAZIONE-
303000970110     C*------------------------------------------------------------------------*
303100970110     C     IMPETT        BEGSR
303200970110     C*---
303300970110     C* CAMPI CALCOLATI
303400970110     C*---
303500970417     C                   MOVEL     §RC(J)        ETTRCT
303600970417     C                   Z-ADD     §CP(J)        ETTPPE
303700970110     C*---
303800970110     C* CAMPI FISSI
303900970110     C*---
304000040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
304100040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
304200040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
304300040805     C* MA È GIUSTO COSÌ
304400040805     C                   MOVEL     §CD(J)        ETTFTR
304500970110     C                   Z-ADD     *ZEROS        ETTDTR
304600970110     C                   MOVEL     *BLANKS       ETTATB
304700970110     C*
304800970110     C                   ENDSR
304900970417     C*------------------------------------------------------------------------*
305000970417     C* ARRIMP - ARROTONDA IMPORTO
305100970417     C*------------------------------------------------------------------------*
305200970417     C     ARRIMP        BEGSR
305300970417     C*---
305400970417     C* $IMP -> IMPORTO DA ARROTONDARE
305500970417     C* $FDE -> 'D'=DIFETTO / E=ECCESSO
305600970417     C* $ARR  : IMPORTO AL QUALE ARROTONDARE
305700970417     C*---
305800970417IF  1C     $IMP          IFGT      *ZEROS                                       *IMPORTO PIENO
305900970417IF  2C     $ARR          IFGT      *ZEROS                                       *SI ARROTONDAMENTO
306000970417     C     $IMP          DIV       $ARR          COMODO           15 0
306100970417     C                   MVR                     RESTO            15 0
306200970417IF  3C     $FDE          IFEQ      'E'
306300970417IF  4C     RESTO         IFGT      *ZEROS
306400970417     C                   ADD       1             COMODO
306500970417E   4C                   ENDIF
306600970417E   3C                   ENDIF
306700970417     C     COMODO        MULT      $ARR          $IMP
306800970417E   2C                   ENDIF
306900970417E   1C                   ENDIF
307000970417     C*
307100970417     C                   ENDSR
307200970110     C*------------------------------------------------------------------------*
307300970110     C* STAERR - STAMPA RIGA DI ERRORE
307400970110     C*------------------------------------------------------------------------*
307500970110     C     STAERR        BEGSR
307600970110     C*
307700970110IF  1C     *INOF         IFEQ      *ON
307800970110     C                   EXCEPT    TES
307900970110     C                   SETOFF                                       OF
308000970110E   1C                   ENDIF
308100970110IF  1C     DEPTER        IFEQ      'T'
308200970110     C                   EXCEPT    ETAS
308300970110E   1C                   ENDIF
308400970110IF  1C     DEPTER        IFEQ      'X'
308500970110     C                   EXCEPT    EETX
308600970110E   1C                   ENDIF
308700970110     C*
308800970110     C                   ENDSR
308900970313     C*------------------------------------------------------------------------*
309000970314     C* ANNETX - ANNULLA I DATI TIPO ESISTENTI
309100970313     C*------------------------------------------------------------------------*
309200970314     C     ANNETX        BEGSR
309300970313     C*
309400970314     C* PER OGNI FILIALE RICHIESTA
309500971124DO  1C     1             DO        500           I
309600970313IF  2C     SORG(I)       IFGT      *ZEROS
309700970313     C*---
309800970313     C* DATI TIPO -GENERALI-
309900970313     C*---
310000060804     C                   MOVEL     'S'           KTCDSF
310100980713     C                   MOVEL     PARCTO        KTCCTO
310200970313     C                   Z-ADD     PARKOV        KTCKSC
310300970313     C                   Z-ADD     PARCTR        KTCCTR
310400970313     C                   Z-ADD     PARPRG        KTCPRG
310500970313     C                   Z-ADD     SORG(I)       KTCLNP
310600011001     C     KEYETC        CHAIN     TEETC01L                           99
310700981221IF  3C     *IN99         IFEQ      *OFF
310800971024IF  4C     ETCATB        IFEQ      *BLANKS
310900970314     C                   MOVEL     'A'           ETCATB
311000970314     C                   MOVEL     *BLANKS       ETCFTR
311100011001     C                   UPDATE    TEETC000
311200970314E   4C                   ENDIF
311300970314E   3C                   ENDIF
311400970313     C*---
311500970313     C* DATI TIPO -SCAGLIONI-
311600970313     C*---
311700060829     c* ricavi
311800980713     C                   MOVEL     'S'           KTSDSF
311900980713     C                   MOVEL     PARCTO        KTSCTO
312000970313     C                   Z-ADD     PARKOV        KTSKSC
312100970313     C                   Z-ADD     PARCTR        KTSCTR
312200970313     C                   Z-ADD     PARPRG        KTSPRG
312300970313     C                   Z-ADD     SORG(I)       KTSLNP
312400011001     C     KE2ETS        SETLL     TEETS01L
312500011001     C     KE2ETS        READE     TEETS01L                               99
312600970314DO  3C     *IN99         DOWEQ     *OFF
312700971024IF  4C     ETSATB        IFEQ      *BLANKS
312800970314     C                   MOVEL     'A'           ETSATB
312900970314     C                   MOVEL     *BLANKS       ETSFTR
313000011001     C                   UPDATE    TEETS000
313100970314E   4C                   ENDIF
313200011001     C     KE2ETS        READE     TEETS01L                               99
313300060829E   3c                   ENDDO                                                  *FINE SCAGLIONI
313400060829     c* competetnze
313500060829     C     KE2ETS        SETLL     TEETS11L
313600060829     C     KE2ETS        READE     TEETS11L                               99
313700060829DO  3C     *IN99         DOWEQ     *OFF
313800060829IF  4C     £ETSATB       IFEQ      *BLANKS
313900060829     C                   MOVEL     'A'           £ETSATB
314000060829     C                   MOVEL     *BLANKS       £ETSFTR
314100060829     C                   UPDATE    TEETS100
314200060829E   4C                   ENDIF
314300060829     C     KE2ETS        READE     TEETS11L                               99
314400060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
314500970314     C*---
314600970314     C* DATI TIPO -CODICI TASSAZIONE-
314700970314     C*---
314800970429DO  3C     1             DO        2             K
314900970429IF  4C     SFIE(K)       IFNE      *BLANKS
315000980713     C                   MOVEL     'S'           KTTDSF
315100980713     C                   MOVEL     PARCTO        KTTCTO
315200970314     C                   Z-ADD     PARKOV        KTTKSC
315300970314     C                   Z-ADD     PARCTR        KTTCTR
315400970314     C                   Z-ADD     PARPRG        KTTPRG
315500970314     C                   Z-ADD     SORG(I)       KTTLNP
315600970429     C                   MOVEL     SFIE(K)       KTTFIE
315700011001     C     KE2ETT        SETLL     TEETT01L
315800011001     C     KE2ETT        READE     TEETT01L                               99
315900970429DO  5C     *IN99         DOWEQ     *OFF
316000971024IF  6C     ETTATB        IFEQ      *BLANKS
316100970314     C                   MOVEL     'A'           ETTATB
316200970314     C                   MOVEL     *BLANKS       ETTFTR
316300011001     C                   UPDATE    TEETT000
316400970429E   6C                   ENDIF
316500011001     C     KE2ETT        READE     TEETT01L                               99
316600970429E   5C                   ENDDO                                                  *FINE COD.TASSAZIONE
316700970429E   4C                   ENDIF
316800970429E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
316900970313     C*
317000970314E   2C                   ENDIF
317100970314E   1C                   ENDDO                                                  *FINE FILIALI
317200970314     C*
317300970313     C                   ENDSR
317400970110     C*------------------------------------------------------------------------*
317500970110     C* OPEFIL - APRE GLI ARCHIVI
317600970110     C*------------------------------------------------------------------------*
317700970110     C     OPEFIL        BEGSR
317800970110     C*
317900970110IF  1C     PARKSC        IFGT      *ZEROS
318000990812     C                   OPEN      TITAS31C                                     *LETTURA X CLIENTE
318100970110X   1C                   ELSE
318200990812     C                   OPEN      TITAS38C                                     *LETTURA X DATA
318300970110E   1C                   ENDIF
318400970110     C*
318500970110     C                   ENDSR
318600970110     C*------------------------------------------------------------------------*
318700970110     C* CLOFIL - CHIUDE GLI ARCHIVI
318800970110     C*------------------------------------------------------------------------*
318900970110     C     CLOFIL        BEGSR
319000970110     C*
319100970110IF  1C     PARKSC        IFGT      *ZEROS
319200990812     C                   CLOSE     TITAS31C                                     *LETTURA X CLIENTE
319300970110X   1C                   ELSE
319400990812     C                   CLOSE     TITAS38C                                     *LETTURA X DATA
319500970110E   1C                   ENDIF
319600970110     C*
319700970110     C                   ENDSR
319800011011     C*------------------------------------------------------------------------*
319900011011     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
320000011011     C*------------------------------------------------------------------------*
320100011011     C     CNVEUR        BEGSR
320200011011     C*
320300011011     C* AZZERA VARIABILI DI LAVORO
320400011011     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
320500011011     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
320600011026     C                   EVAL      NDEC = '9'
320700011011     C*
320800011011IF  1C     WIMI          IFNE      *ZEROS
320900011011     C                   CLEAR                   YEUR
321000011011     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
321100011011     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
321200011011     C                   MOVEL     PARDIV        YECDVO                         *DIVISA IMP CONVERTI
321300011011     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
321400011011     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
321500011011     C                   CALL      'YEURCO'
321600011011     C                   PARM                    YEUR
321700011011IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
321800011011     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
321900011011X   2C                   ELSE                                                   *SI ERRORI
322000011011     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
322100011011E   2C                   ENDIF
322200011011E   1C                   ENDIF
322300011011     C*
322400011011     C                   ENDSR
322500011011     C*------------------------------------------------------------------------*
322600011011     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
322700011011     C*------------------------------------------------------------------------*
322800011011     C     CARGEDCE      BEGSR
322900011011     C*
323000011011     C                   CLEAR                   DSGEDCE
323100011026     C                   MOVEL     *blanks       WDIVCE            3
323200011011     C*
323300011011     C                   MOVEL(P)  'GED'         KTECOD
323400011011     C                   MOVEL(P)  'CE'          KTEKE1
323500011011     C     KEYTBE        CHAIN     TNTBE01L
323600011011     C                   IF        %found(TNTBE01L)
323700011011     C                   MOVEL     TBEUNI        DSGEDCE
323800011011     C                   ENDIF
323900011011     C*
324000011011     C                   IF        §GEDCEDAT > DATCOR
324100011026     C                   EVAL      WDIVCE = §GEDCEDIP
324200011026     C                   ELSE
324300011026     C                   EVAL      WDIVCE = §GEDCEDIV
324400011011     C                   ENDIF
324500011011     C*
324600011011     C                   ENDSR
324700970110     C*------------------------------------------------------------------------*
324800970110     C* CARTAB - CARICAMENTO TABELLE
324900970110     C*------------------------------------------------------------------------*
325000970110     C     CARTAB        BEGSR
325100970110     C*--------------------
325200970110     C* ORGANIGRAMMA
325300970110     C*--------------------
325400020227     C                   Z-ADD     *ZEROS        JJ                3 0
325500970110     C                   Z-ADD     *ZEROS        I                 4 0
325600970110     C     *LOVAL        SETLL     AZORG01L
325700970110     C                   READ      AZORG01L                               99
325800970110DO  1C     *IN99         DOWEQ     *OFF
325900970110IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATE
326000970110     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
326100970110     C                   ADD       1             I
326200970110     C                   Z-ADD     ORGFIL        ORG(I)                         *FILIALE
326300970110     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
326400020730     C                   MOVEL     ORGDE3        OG143
326500020730     C*
326600020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
326700020730     C                   CLEAR                   DNTW
326800020730     C*
326900020730     C                   MOVEL(P)  'NTW'         KTECOD
327000020730     C                   MOVEL(P)  §OGNTW        KTEKE1
327100020730     C     KEYTBE        CHAIN     TNTBE01L
327200020730     C                   IF        %found(TNTBE01L)
327300020730     C                   MOVEL     TBEUNI        DNTW
327400020730     C                   ENDIF
327500020730     C*
327600020730     C                   MOVEL     §NTWFIE       FL1(I)                         *FLAG ITALIA/ESTERO
327700970110     C* SELEZIONI
327800970430IF  3C     PARLNP        IFEQ      999                                          *TUTTE
327900970430     C                   Z-ADD     ORGFIL        SORG(I)                        *FILIALI SELEZIONATE
328000020730     C                   MOVEL     §NTWFIE       SFL1(I)                        *FLAG ITALIA/ESTERO
328100970110X   3C                   ELSE
328200970110IF  4C     PARLNP        IFEQ      ORGFIL                                       *UNA FILIALE
328300970417     C                   Z-ADD     ORGFIL        SORG(1)                        *FILIALE SELEZIONATA
328400020730     C                   MOVEL     §NTWFIE       SFL1(1)                        *FLAG ITALIA/ESTERO
328500970110E   4C                   ENDIF
328600970110E   3C                   ENDIF
328700081105     C* VERIFICO SE FILIALE dpd
328800020227     C                   MOVEL     ORGDE3        OG143
328900081105     C                   IF        §OGNTW = 'DPD'
329000081105     C                   ADD       1             xx                3 0
329100081105     C                   MOVEL     ORGFIL        FILdpd(xx)
329200020227     C                   ENDIF
329300081105     C* VERIFICO SE FILIALE FEDEX
329400081105     C                   MOVEL     ORGDE3        OG143
329500081105     C                   IF        §OGNTW = 'FED'
329600081105     C                   ADD       1             JJ
329700081105     C                   MOVEL     ORGFIL        FILFED(JJ)
329800081105     C                   ENDIF
329900970110E   2C                   ENDIF
330000970110     C                   READ      AZORG01L                               99
330100970110E   1C                   ENDDO
330200970423     C*--------------------
330300970423     C* TARIFFA ITALIA/ESTERO
330400970423     C*--------------------
330500970423IF  1C     PARFIE        IFEQ      *BLANKS                                      *ITALIA E ESTERO
330600970423     C                   MOVEL     'I'           SFIE(1)
330700970423     C                   MOVEL     'E'           SFIE(2)
330800970423X   1C                   ELSE                                                   *ITALIA O ESTERO
330900970423     C                   MOVEL     PARFIE        SFIE(1)
331000970423E   1C                   ENDIF
331100970110     C*--------------------
331200970110     C* TIPI TARIFFA
331300970110     C*--------------------
331400970110     C                   Z-ADD     *ZEROS        I                 4 0
331500020923     C                   Z-ADD     1             KTBKUT
331600970110     C                   MOVEL     'TR'          KTBCOD
331700970110     C     KE2TAB        SETLL     TABEL00F
331800970110     C     KE2TAB        READE     TABEL00F                               99
331900970110DO  1C     *IN99         DOWEQ     *OFF
332000970110IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
332100970110     C                   MOVEL     TBLUNI        DSTR
332200970110IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
332300970110     C                   ADD       1             I
332400970110     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
332500970110     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
332600970110E   3C                   ENDIF
332700970110E   2C                   ENDIF
332800970110     C     KE2TAB        READE     TABEL00F                               99
332900970110E   1C                   ENDDO
333000050808     C*--------------------
333100050808     C* cod. trattamento merce con packing list
333200050808     C*--------------------
333300050808     C                   Z-ADD     *ZEROS        I                 4 0
333400050808     C                   Z-ADD     1             KTBKUT
333500050808     C                   MOVEL     '1B'          KTBCOD
333600050808     C     KE2TAB        SETLL     TABEL00F
333700050808     C     KE2TAB        READE     TABEL00F                               99
333800050808DO  1C     *IN99         DOWEQ     *OFF
333900050808IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
334000050808     C                   MOVEL     TBLUNI        DS1b
334100050808IF  3C     §1bf16        IFEQ      'S'                                          *IN GESTIONE
334200050808     C                   ADD       1             I
334300050808     C                   MOVEL     TBLKEY        cpkl(I)                        *CODICE
334400050808E   3C                   ENDIF
334500050808E   2C                   ENDIF
334600050808     C     KE2TAB        READE     TABEL00F                               99
334700050808E   1C                   ENDDO
334800970110     C*--------------------
334900970110     C* RAGGRUPPAMENTI TIPO BOLLA
335000970110     C*--------------------
335100970110     C                   Z-ADD     *ZEROS        I                 4 0
335200020923     C                   Z-ADD     1             KTBKUT
335300970110     C                   MOVEL     '2W'          KTBCOD
335400970110     C     KE2TAB        SETLL     TABEL00F
335500970110     C     KE2TAB        READE     TABEL00F                               99
335600970110DO  1C     *IN99         DOWEQ     *OFF
335700970110IF  2C     TBLFLG        IFEQ      *BLANKS
335800970110     C                   MOVEL     TBLUNI        DS2W
335900970110     C                   ADD       1             I
336000970110     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
336100970110     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
336200970110     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
336300970110E   2C                   ENDIF
336400970110     C     KE2TAB        READE     TABEL00F                               99
336500970110E   1C                   ENDDO
336600970110     C*--------------------
336700970110     C* TIPI BOLLA
336800970110     C*--------------------
336900970110     C                   Z-ADD     *ZEROS        I                 4 0
337000970110     C                   Z-ADD     *ZEROS        J                 4 0
337100020923     C                   Z-ADD     1             KTBKUT
337200970110     C                   MOVEL     'TB'          KTBCOD
337300970110     C     KE2TAB        SETLL     TABEL00F
337400970110     C     KE2TAB        READE     TABEL00F                               99
337500970110DO  1C     *IN99         DOWEQ     *OFF
337600970110IF  2C     TBLFLG        IFEQ      *BLANKS
337700970110     C                   MOVEL     TBLUNI        DSTB
337800970110     C                   ADD       1             I
337900970110     C                   MOVEL     TBLKEY        TBL(I)                         *TIPO BOLLA
338000970110     C                   Z-ADD     1             J
338100970110     C                   SETOFF                                           98
338200970110     C     §TBRBL        LOOKUP    RBL(J)                                 98
338300970110IF  3C     *IN98         IFEQ      *ON
338400970110     C                   MOVEL     FSP(J)        SPD(I)                         *FLAG SPEDIZIONE
338500970110     C                   MOVEL     FRI(J)        RIC(I)                         *FLAG RICAVO
338600970110X   3C                   ELSE
338700970110     C                   MOVEL     'N'           SPD(I)                         *FLAG SPEDIZIONE
338800970110     C                   MOVEL     'N'           RIC(I)                         *FLAG RICAVO
338900970110E   3C                   ENDIF
339000970110E   2C                   ENDIF
339100970110     C     KE2TAB        READE     TABEL00F                               99
339200970110E   1C                   ENDDO
339300970110     C*--------------------
339400970110     C* CODICI PAGAMENTO
339500970110     C*--------------------
339600970110     C                   Z-ADD     *ZEROS        I                 4 0
339700020923     C                   Z-ADD     1             KTBKUT
339800970110     C                   MOVEL     'FA'          KTBCOD
339900970110     C     KE2TAB        SETLL     TABEL00F
340000970110     C     KE2TAB        READE     TABEL00F                               99
340100970110DO  1C     *IN99         DOWEQ     *OFF
340200970110IF  2C     TBLFLG        IFEQ      *BLANKS
340300970110     C                   MOVEL     TBLUNI        DSFA
340400970110     C                   ADD       1             I
340500970110     C                   MOVEL     TBLKEY        CDP(I)                         *CODICE PAGAMENTO
340600970110     C                   MOVEL     §FARAT        CRAT(I)                        *RATE DI PAGAMENTO
340700970110E   2C                   ENDIF
340800970110     C     KE2TAB        READE     TABEL00F                               99
340900970110E   1C                   ENDDO
341000970313     C*--------------------
341100970313     C* CODICI TASSAZIONE
341200970313     C*--------------------
341300970313     C                   Z-ADD     *ZEROS        I                 4 0
341400020923     C                   Z-ADD     1             KTBKUT
341500970313     C                   MOVEL     'CT'          KTBCOD
341600970313     C     KE2TAB        SETLL     TABEL00F
341700970313     C     KE2TAB        READE     TABEL00F                               99
341800970313DO  1C     *IN99         DOWEQ     *OFF
341900970313IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
342000970313     C                   MOVEL     TBLUNI        DSCT
342100970317IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
342200970317     C                   ADD       1             I
342300970317     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
342400970317     C                   MOVEL     §CTDES        DCTS(I)                        *DESCRIZIONE
342500970317     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
342600970317     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
342700970317     C                   MOVE      §CTCOR        WA1               1            *TIPO CODICE
342800970417IF  4C     WA1           IFEQ      '9'
342900970417IF  5C     CTS(I)        IFEQ      'IT'
343000970317     C     CTS(I)        OREQ      'EE'
343100970317     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
343200970417X   5C                   ELSE
343300970317     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
343400970417E   5C                   ENDIF
343500970417X   4C                   ELSE
343600970317     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
343700970417E   4C                   ENDIF
343800970417IF  4C     §CTNAR        IFEQ      *BLANKS
343900970317     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
344000970317     C                   MOVEL     'I'           CIE(I)                         *CODICE ITALIA
344100970417X   4C                   ELSE
344200970317     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
344300970317     C                   MOVEL     'E'           CIE(I)                         *CODICE ESTERO
344400970417E   4C                   ENDIF
344500970317E   3C                   ENDIF
344600970317E   2C                   ENDIF
344700970317     C     KE2TAB        READE     TABEL00F                               99
344800970317E   1C                   ENDDO
344900040920     C* CARICO GLI SCAGLIONI DI DEFAULT
345000040920IF  1C     PARTTR        IFEQ      '0'                                          *QUINTALI
345100040920     C     PARTTR        OREQ      '3'                                          *KG
345200040920     C                   Z-ADD     2             SGL(1)
345300040920     C                   Z-ADD     5             SGL(2)
345400040920     C                   Z-ADD     10            SGL(3)
345500040920     C                   Z-ADD     25            SGL(4)
345600040920     C                   Z-ADD     50            SGL(5)
345700040920     C                   Z-ADD     75            SGL(6)
345800040920     C                   Z-ADD     100           SGL(7)
345900040920     C                   Z-ADD     99999         SGL(8)
346000040920X   1C                   ELSE                                                   *COLLI
346100040920     C                   Z-ADD     1             SGL(1)
346200040920     C                   Z-ADD     2             SGL(2)
346300040920     C                   Z-ADD     3             SGL(3)
346400040920     C                   Z-ADD     4             SGL(4)
346500040920     C                   Z-ADD     99999         SGL(5)
346600040920E   1C                   ENDIF
346700970110     C*
346800970110     C                   ENDSR
346900970313     C*------------------------------------------------------------------------*
347000970313     C* CARTAR - CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
347100970313     C*------------------------------------------------------------------------*
347200970313     C     CARTAR        BEGSR
347300970313     C*
347400970313     C* REPERISCE IL CODICE DELLA TARIFFA DI CARTELLO IN VIGORE
347500970314IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORE
347600021202     C                   Z-ADD     *ZEROS        CARKSC
347700021202     C                   Z-ADD     *ZEROS        CARCTR
347800021202     C                   MOVE(P)   *ZEROS        CARPRG
347900160229     C                   CLEAR                   TRULC7DS
348000160229     C                   EVAL      Ic7TLA = 'L'
348100160229     c                   eval      ic7tfie='I'
348200160229     C                   EVAL      Ic7DTA = DATCOR
348300160229     C                   CALL      'TRULC7R'
348400160229     C                   PARM                    TRULc7DS
348500021202     C*
348600160229     C                   IF        Oc7ERR <> 'E'
348700160229     C                   Z-ADD     oc7kscc       CARKSC
348800160229     C                   Z-ADD     oc7ctrc       CARCTR
348900160229     C                   movel     oc7prgc       CARprg
349000021202     C                   ENDIF
349100021202E   1C                   ENDIF
349200970317     C*
349300970317     C* PER CIASCUNA FILIALE RICHIESTA
349400971124DO  1C     1             DO        500           Y                 4 0
349500970417IF  2C     SORG(Y)       IFGT      *ZEROS
349600970423     C     Y             OCCUR     FTAR
349700970317     C*---
349800970317     C* CARICA GLI SCAGLIONI DA CONSIDERARE -CLIENTE O CARTELLO-
349900970317     C*---
350000970416     C                   Z-ADD     *ZEROS        SSGL
350100970313     C                   Z-ADD     PARKOV        KTDKSC
350200970313     C                   Z-ADD     PARCTR        KTDCTR
350300970313     C                   Z-ADD     PARPRG        KTDPRG
350400970417     C                   Z-ADD     SORG(Y)       KTDLNP
350500970313     C                   EXSR      CARSGL
350600970417IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
350700970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
350800970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
350900970417     C                   Z-ADD     CARKSC        KTDKSC
351000970313     C                   Z-ADD     CARCTR        KTDCTR
351100970313     C                   Z-ADD     CARPRG        KTDPRG
351200970417     C                   Z-ADD     SORG(Y)       KTDLNP
351300970314     C                   EXSR      CARSGL
351400970703     C                   MOVEL     DEPCTO        PARCTO
351500970418E   3C                   ENDIF
351600970418IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
351700040920     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
351800040920     c                   move      sgl           ssgl
351900970418E   3C                   ENDIF
352000970317     C*---
352100970313     C* CARICA I CODICI TASSAZIONE DA CONSIDERARE -CLIENTE O CARTELLO-
352200970317     C*---
352300970423     C                   MOVEL     *BLANKS       CCTI
352400970423     C                   MOVEL     *BLANKS       CRCI
352500970423     C                   MOVEL     *BLANKS       CCTE
352600970423     C                   MOVEL     *BLANKS       CRCE
352700970423DO  3C     1             DO        2             W                 4 0
352800970423IF  4C     SFIE(W)       IFNE      *BLANKS
352900970314     C                   Z-ADD     PARKOV        KTDKSC
353000970314     C                   Z-ADD     PARCTR        KTDCTR
353100970314     C                   Z-ADD     PARPRG        KTDPRG
353200970417     C                   Z-ADD     SORG(Y)       KTDLNP
353300971204     C                   MOVEL     SFL1(Y)       DLPFL1
353400970314     C                   EXSR      CARCTS
353500970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
353600970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
353700970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
353800970314     C                   Z-ADD     CARKSC        KTDKSC
353900970314     C                   Z-ADD     CARCTR        KTDCTR
354000970314     C                   Z-ADD     CARPRG        KTDPRG
354100970417     C                   Z-ADD     SORG(Y)       KTDLNP
354200971204     C                   MOVEL     SFL1(Y)       DLPFL1
354300970314     C                   EXSR      CARCTS
354400970703     C                   MOVEL     DEPCTO        PARCTO
354500970423E   5C                   ENDIF
354600970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
354700970418     C                   EXSR      FIXCTS                                       *IMPOSTA FISSI CTS
354800970423E   5C                   ENDIF
354900970416     C                   EXSR      RAPSVI                                       *CTS->CTS ELEMENTARI
355000970423E   4C                   ENDIF
355100970423E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
355200970417     C*
355300970417E   2C                   ENDIF
355400970417E   1C                   ENDDO                                                  *FINE FILIALI
355500970313     C*
355600970313     C                   ENDSR
355700970313     C*------------------------------------------------------------------------*
355800970313     C* CARSGL - CARICA GLI SCAGLIONI
355900970313     C*------------------------------------------------------------------------*
356000970313     C     CARSGL        BEGSR
356100970313     C*
356200970313     C* AZZERA LE VARIABILI DI LAVORO
356300970313     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
356400970313     C                   Z-ADD     *ZEROS        I                 4 0
356500970313     C*
356600011128     C                   CLEAR                   DEPORP
356700060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
356800990820     C     KEYTAD        SETLL     TITAD04L                               99
356900990820     C     KEYTAD        READE     TITAD04L                               99
357000060804X   1C                   ELSE                                                   *OFFERTA
357100060804     C     KEYTAD        SETLL     TIOFD01L                               99
357200060804     C     KEYTAD        READE     TIOFD01L                               99
357300060804E   1C                   ENDIF
357400011128     C                   EVAL      DEPORP = TADORP                              *SOLO un cts
357500011010DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
357600011128     C     TADORP        ANDEQ     DEPORP                                       *ROTTURA ORDINAMENTO
357700011010IF  2C     TADATB        IFEQ      *BLANKS
357800011010     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
357900011010     C     TADSGL        LOOKUP    SSGL                                   98
358000011010IF  3C     *IN98         IFEQ      *OFF
358100030122     C                   IF        I < 10
358200011010     C                   ADD       1             I
358300011010     C                   Z-ADD     TADSGL        SSGL(I)
358400030122     C                   ELSE
358500030122     C                   LEAVE
358600030122     C                   ENDIF
358700011010E   3C                   ENDIF
358800011010E   2C                   ENDIF
358900060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
359000990820     C     KEYTAD        READE     TITAD04L                               99
359100060804X   2C                   ELSE                                                   *OFFERTA
359200060804     C     KEYTAD        READE     TIOFD01L                               99
359300060804E   2C                   ENDIF
359400970313E   1C                   ENDDO                                                  *FINE DETTAGLIO
359500970313     C*
359600970313     C                   ENDSR
359700970314     C*------------------------------------------------------------------------*
359800970314     C* CARCTS   CARICA I CODICI TASSAZIONE
359900970314     C*------------------------------------------------------------------------*
360000970314     C     CARCTS        BEGSR
360100970314     C*
360200970314     C* AZZERA LE VARIABILI DI LAVORO
360300970314     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
360400970314     C                   Z-ADD     *ZEROS        I                 4 0
360500970317     C                   CLEAR                   WCTS
360600970317     C                   CLEAR                   WRAP
360700970317     C                   CLEAR                   WNAR
360800970317     C                   CLEAR                   WTCT
360900970314     C*
361000970314     C* MEMORIZZA I CODICI DELLA LINEA DI PARTENZA RICHIESTA
361100060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
361200990820     C     KEYTAD        SETLL     TITAD04L                               99
361300990820     C     KEYTAD        READE     TITAD04L                               99
361400060804X   1C                   ELSE                                                   *OFFERTA
361500060804     C     KEYTAD        SETLL     TIOFD01L                               99
361600060804     C     KEYTAD        READE     TIOFD01L                               99
361700060804E   1C                   ENDIF
361800970314DO  1C     *IN99         DOWEQ     *OFF
361900970314IF  2C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
362000970314     C                   SETOFF                                           98
362100970314     C                   Z-ADD     1             J                 4 0
362200970314     C     TADCTS        LOOKUP    CTS(J)                                 98
362300970314IF  3C     *IN98         IFEQ      *ON                                          *TROVATO
362400970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
362500970417     C     CIE(J)        ANDEQ     'I'                                          *CODICI ITALIA
362600970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
362700971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
362800970417     C     CIE(J)        ANDEQ     'E'                                          *CODICI ESTERO
362900970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
363000971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ITALIA
363100970314     C                   SETOFF                                           98
363200970314     C                   Z-ADD     1             K                 4 0
363300970314     C     TADCTS        LOOKUP    WCTS(K)                                98
363400970417IF  5C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
363500970417     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
363600970314     C                   ADD       1             I
363700970314     C                   MOVEL     TADCTS        WCTS(I)                        *CODICE
363800970314     C                   MOVEL     RAP(J)        WRAP(I)                        *REGIONE
363900970314     C                   MOVEL     NAR(J)        WNAR(I)                        *NAZIONE
364000970314     C                   MOVEL     TCT(J)        WTCT(I)                        *TIPO
364100970417E   5C                   ENDIF
364200970417E   4C                   ENDIF
364300970314E   3C                   ENDIF
364400970314E   2C                   ENDIF
364500060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
364600990820     C     KEYTAD        READE     TITAD04L                               99
364700060804X   2C                   ELSE                                                   *OFFERTA
364800060804     C     KEYTAD        READE     TIOFD01L                               99
364900060804E   2C                   ENDIF
365000970314E   1C                   ENDDO
365100970314     C*
365200970314     C                   ENDSR
365300970421     C*------------------------------------------------------------------------*
365400970421     C* FIXCTS - IMPOSTA CODICI TASSAZIONE FISSI
365500970421     C*------------------------------------------------------------------------*
365600970421     C     FIXCTS        BEGSR
365700970421     C*
365800970421     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
365900970421     C*
366000970421     C                   Z-ADD     *ZEROS        J                 4 0
366100050103DO  1C     1             DO        600           I
366200970421IF  2C     CTS(I)        IFNE      *BLANKS
366300970421IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
366400970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
366500970421     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
366600970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
366700971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
366800970421     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
366900970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
367000971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
367100970421     C                   ADD       1             J
367200970421     C                   MOVEL     CTS(I)        WCTS(J)                        *CODICE
367300970421     C                   MOVEL     RAP(I)        WRAP(J)                        *REGIONE
367400970421     C                   MOVEL     NAR(I)        WNAR(J)                        *NAZIONE
367500970421     C                   MOVEL     TCT(I)        WTCT(J)                        *TIPO
367600970421E   4C                   ENDIF
367700970421E   3C                   ENDIF
367800970421E   2C                   ENDIF
367900970421E   1C                   ENDDO
368000970421     C*
368100970421     C                   ENDSR
368200970314     C*------------------------------------------------------------------------*
368300970314     C* RAPSVI - TRASFORMA I CODICI DELLA TARIFFA IN CODICI ELEMENTARI
368400970314     C*------------------------------------------------------------------------*
368500970314     C     RAPSVI        BEGSR
368600970314     C*
368700970314     C* 1°GIRO-> CARICA TUTTI I CODICI ELEMENTARI COME -NON CALCOLARE-
368800970314     C                   Z-ADD     *ZEROS        J                 4 0
368900050103DO  1C     1             DO        600           I
369000970416IF  2C     CTS(I)        IFNE      *BLANKS
369100970416IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
369200970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
369300970417     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
369400970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
369500971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
369600970417     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
369700970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
369800971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
369900970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
370000970314     C                   ADD       1             J
370100970423     C                   MOVEL     CTS(I)        CCTI(J)                        *CODICE
370200970423     C                   MOVEL     *BLANKS       CRCI(J)                        *RAGGRUPPAMENTO
370300970423X   5C                   ELSE                                                   *ESTERO
370400970423     C                   ADD       1             J
370500970423     C                   MOVEL     CTS(I)        CCTE(J)                        *CODICE
370600970423     C                   MOVEL     *BLANKS       CRCE(J)                        *RAGGRUPPAMENTO
370700970423E   5C                   ENDIF
370800970423E   4C                   ENDIF
370900970417E   3C                   ENDIF
371000970314E   2C                   ENDIF
371100970314E   1C                   ENDDO                                                  *FINE CODICI BASE
371200970314     C*
371300970314     C* 2°GIRO-> TRASFORMA I -NON CALCOLARE- IN -CALCOLARE IN NAZIONE-
371400050103DO  1C     1             DO        600           I
371500970314IF  2C     WCTS(I)       IFNE      *BLANKS
371600970314IF  3C     WTCT(I)       IFEQ      'N'                                          *SOLO NAZIONE
371700050103DO  4C     1             DO        600           J
371800970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
371900970423IF  6C     CCTI(J)       IFNE      *BLANKS
372000970314     C                   SETOFF                                           98
372100970314     C                   Z-ADD     1             K                 4 0
372200970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
372300970423IF  7C     WNAR(I)       IFEQ      NAR(K)
372400970423     C                   MOVEL     'N'           CRCI(J)
372500970423E   7C                   ENDIF
372600970423E   6C                   ENDIF
372700970423X   5C                   ELSE                                                   *ESTERO
372800970423IF  6C     CCTE(J)       IFNE      *BLANKS
372900970423     C                   SETOFF                                           98
373000970423     C                   Z-ADD     1             K                 4 0
373100970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
373200970423IF  7C     WNAR(I)       IFEQ      NAR(K)
373300970423     C                   MOVEL     'N'           CRCE(J)
373400970423E   7C                   ENDIF
373500970423E   6C                   ENDIF
373600970423E   5C                   ENDIF
373700970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
373800970314E   3C                   ENDIF
373900970314E   2C                   ENDIF
374000970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
374100970314     C*
374200970314     C* 3°GIRO-> TRASFORMA I -CALCOLARE IN NAZIONE- IN -CALCOLARE IN REGIONE-
374300050103DO  1C     1             DO        600           I
374400970314IF  2C     WCTS(I)       IFNE      *BLANKS
374500970314IF  3C     WTCT(I)       IFEQ      'R'                                          *SOLO REGIONE
374600050103DO  4C     1             DO        600           J
374700970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
374800970423IF  6C     CCTI(J)       IFNE      *BLANKS
374900970314     C                   SETOFF                                           98
375000970314     C                   Z-ADD     1             K                 4 0
375100970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
375200970423IF  7C     WRAP(I)       IFEQ      RAP(K)
375300970423     C                   MOVEL     'R'           CRCI(J)
375400970423E   7C                   ENDIF
375500970423E   6C                   ENDIF
375600970423X   5C                   ELSE                                                   *ESTERO
375700970423IF  6C     CCTE(J)       IFNE      *BLANKS
375800970423     C                   SETOFF                                           98
375900970423     C                   Z-ADD     1             K                 4 0
376000970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
376100970423IF  7C     WRAP(I)       IFEQ      RAP(K)
376200970423     C                   MOVEL     'R'           CRCE(J)
376300970423E   7C                   ENDIF
376400970423E   6C                   ENDIF
376500970314E   5C                   ENDIF
376600970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
376700970314E   3C                   ENDIF
376800970314E   2C                   ENDIF
376900970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
377000970314     C*
377100970314     C* 4°GIRO-> TRASFORMA I -CALCOLARE IN REGIONE- IN -CALCOLARE-
377200050103DO  1C     1             DO        600           I
377300970314IF  2C     WCTS(I)       IFNE      *BLANKS
377400970314IF  3C     WTCT(I)       IFEQ      'E'                                          *SOLO ELEMENTARI
377500050103DO  4C     1             DO        600           J
377600970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
377700970423IF  6C     CCTI(J)       IFNE      *BLANKS
377800970423IF  7C     WCTS(I)       IFEQ      CCTI(J)
377900970423     C                   MOVEL     'T'           CRCI(J)
378000970423E   7C                   ENDIF
378100970423E   6C                   ENDIF
378200970423X   5C                   ELSE                                                   *ESTERO
378300970423IF  6C     CCTE(J)       IFNE      *BLANKS
378400970423IF  7C     WCTS(I)       IFEQ      CCTE(J)
378500970423     C                   MOVEL     'T'           CRCE(J)
378600970423E   7C                   ENDIF
378700970423E   6C                   ENDIF
378800970423E   5C                   ENDIF
378900970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
379000970314E   3C                   ENDIF
379100970314E   2C                   ENDIF
379200970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
379300970314     C*
379400970314     C                   ENDSR
379500981218     C*------------------------------------------------------------------------*
379600981218     C* CANTAR - ANNULLAMENTO TARIFFA
379700981218     C*------------------------------------------------------------------------*
379800981218     C     CANTAR        BEGSR
379900981218     C*
380000981218     C* CANCELLA DATI TIPO - GENERALI
380100981218     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE
380200060804     C                   MOVEL     parcto        KTCCTO                         *DATI TARIFFA
380300981218     C                   Z-ADD     PARKOV        KTCKSC                         *CLIENTE
380400011001     C     KE3ETC        SETLL     TEETC01L
380500011001     C     KE3ETC        READE     TEETC01L                               99
380600981218DO  1C     *IN99         DOWEQ     *OFF
380700981218IF  2C     PARCTR        IFEQ      ETCCTR                                       *TARIFFA RICHIESTA
380800981218IF  3C     PARPRG        IFEQ      ETCPRG                                       *PROGRESSIVO RICHIES
380900981218IF  4C     PARLNP        IFEQ      ETCLNP                                       *LINEA RICHIESTA
381000981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
381100981218     C                   MOVEL     'A'           ETCATB                         *ANNULLAMENTO
381200981218     C                   MOVEL     *BLANKS       ETCFTR                         *FLAG TRASMISSIONE
381300011128     C*---               Z-ADD     *ZEROS        ETCDTR                         *DATA TRASMISSIONE
381400011001     C                   UPDATE    TEETC000                                     *AGGIORNA
381500981218E   4C                   ENDIF
381600981218E   3C                   ENDIF
381700981218E   2C                   ENDIF
381800011001     C     KE3ETC        READE     TEETC01L                               99
381900981218E   1C                   ENDDO                                                  *FINE DATI GENERALI
382000981218     C*
382100981218     C* CANCELLA DATI TIPO - SCAGLIONI
382200060829     c* ricavi
382300981218     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE
382400060804     C                   MOVEL     parcto        KTSCTO                         *DATI TARIFFA
382500981218     C                   Z-ADD     PARKOV        KTSKSC                         *CLIENTE
382600011001     C     KE3ETS        SETLL     TEETS01L
382700011001     C     KE3ETS        READE     TEETS01L                               99
382800981218DO  1C     *IN99         DOWEQ     *OFF
382900981218IF  2C     PARCTR        IFEQ      ETSCTR                                       *TARIFFA RICHIESTA
383000981218IF  3C     PARPRG        IFEQ      ETSPRG                                       *PROGRESSIVO RICHIES
383100981218IF  4C     PARLNP        IFEQ      ETSLNP                                       *LINEA RICHIESTA
383200981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
383300981218     C                   MOVEL     'A'           ETSATB                         *ANNULLAMENTO
383400981218     C                   MOVEL     *BLANKS       ETSFTR                         *FLAG TRASMISSIONE
383500011001     C                   UPDATE    TEETS000                                     *AGGIORNA
383600981218E   4C                   ENDIF
383700981218E   3C                   ENDIF
383800981218E   2C                   ENDIF
383900011001     C     KE3ETS        READE     TEETS01L                               99
384000981218E   1C                   ENDDO                                                  *FINE SCAGLIONI
384100060829     c* competetnze
384200060829     C     KE3ETS        SETLL     TEETS11L
384300060829     C     KE3ETS        READE     TEETS11L                               99
384400060829DO  1C     *IN99         DOWEQ     *OFF
384500060829IF  2C     PARCTR        IFEQ      £ETSCTR                                      *TARIFFA RICHIESTA
384600060829IF  3C     PARPRG        IFEQ      £ETSPRG                                      *PROGRESSIVO RICHIES
384700060829IF  4C     PARLNP        IFEQ      £ETSLNP                                      *LINEA RICHIESTA
384800060829     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
384900060829     C                   MOVEL     'A'           £ETSATB                        *ANNULLAMENTO
385000060829     C                   MOVEL     *BLANKS       £ETSFTR                        *FLAG TRASMISSIONE
385100060829     C                   UPDATE    TEETS100                                     *AGGIORNA
385200060829E   4C                   ENDIF
385300060829E   3C                   ENDIF
385400060829E   2C                   ENDIF
385500060829     C     KE3ETS        READE     TEETS11L                               99
385600060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
385700981218     C*
385800981218     C* CANCELLA DATI TIPO - CODICI TASSAZIONE
385900981218     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE
386000060804     C                   MOVEL     parcto        KTTCTO                         *DATI TARIFFA
386100981218     C                   Z-ADD     PARKOV        KTTKSC                         *CLIENTE
386200011001     C     KE3ETT        SETLL     TEETT01L
386300011001     C     KE3ETT        READE     TEETT01L                               99
386400981218DO  1C     *IN99         DOWEQ     *OFF
386500981218IF  2C     PARCTR        IFEQ      ETTCTR                                       *TARIFFA RICHIESTA
386600981218IF  3C     PARPRG        IFEQ      ETTPRG                                       *PROGRESSIVO RICHIES
386700981218IF  4C     PARLNP        IFEQ      ETTLNP                                       *LINEA RICHIESTA
386800981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
386900981218IF  5C     PARFIE        IFEQ      ETTFIE                                       *ITA O EST RICHIESTO
387000981218     C     PARFIE        OREQ      *BLANKS                                      *TUTTO ITA E EST
387100981218     C                   MOVEL     'A'           ETTATB                         *ANNULLAMENTO
387200981218     C                   MOVEL     *BLANKS       ETTFTR                         *FLAG TRASMISSIONE
387300011001     C                   UPDATE    TEETT000                                     *AGGIORNA
387400981218E   5C                   ENDIF
387500981218E   4C                   ENDIF
387600981218E   3C                   ENDIF
387700981218E   2C                   ENDIF
387800011001     C     KE3ETT        READE     TEETT01L                               99
387900981218E   1C                   ENDDO                                                  *FINE CODICI TASSAZI
388000981218     C*
388100981218     C                   ENDSR
388200020923     C*--------------------------------------------------------------------------------------------*
388300020923     C* REPERISCE I DATI UTENTE
388400020923     C*--------------------------------------------------------------------------------------------*
388500020923     C     REPDATIUTE    BEGSR
388600020923     C*
388700020923     C* INIZIALIZZA VARIABILI DI WRK
388800020923     C                   CLEAR                   TIBS34DS
388900020923     C                   CLEAR                   AZUTEDS
389000020923     C                   CLEAR                   DDATIUTE
389100020923     C*
389200020923     C     *DTAARA       DEFINE    §azute        azuteds
389300020923     C     *DTAARA       DEFINE    §datiute      ddatiute
389400020923     C                   IN(E)     *DTAARA
389500020923if  1C                   IF        %Error
389600020923     c                   EVAL      I34Tla = 'L'
389700020923     C                   CALL      'TIBS34R'
389800020923     C                   PARM                    Tibs34Ds
389900020923     C                   IN        *DTAARA
390000020923e   1C                   ENDIF
390100020923     C*
390200020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
390300020923     C                   MOVEL(P)  RSUT          DSFIRS           20
390400020923     C*
390500020923     C                   ENDSR
390600021031     C*------------------------------------------------------------------------*
390700021031     C* REPCLI - CARICAMENTO LISTA CLIENTI PADRI/FIGLI
390800021031     C*------------------------------------------------------------------------*
390900021031     C     REPCLI        BEGSR
391000021031     C*
391100021031     C* SE RICHIESTA ELABORAZIONE X CLIENTE UNIFICANTE REPERISCO LA LISTA DEI FIGLI
391200021031     C                   IF        PARUNI = 'S'
391300021031     C                   CLEAR                   TIBS10DS
391400021031     C                   EVAL      D10DRF = DATCOR                              *data riferimento
391500021031     C                   EVAL      D10TLE = 'ST'                                *tipo legame
391600021031     C                   EVAL      D10PAF = 'F'                                 *cerca il padre
391700021031     C                   Z-ADD     PARKSC        D10COD                         *cod cliente da tst
391800021031     C                   CALL      'TIBS10R'
391900021031     C                   PARM                    TIBS10DS
392000021031     C*
392100021031     C                   IF        D10ERR <> *blanks OR                         *No unif. o padre
392200021031     C                             D10COP =  *zeros
392300021031     C                   CLEAR                   SKCLI
392400021031     C                   Z-ADD     PARKSC        SKCLI(1)
392500021031     C                   ENDIF
392600021031     C*
392700021031     C                   ELSE
392800021031     C                   CLEAR                   SKCLI
392900021031     C                   Z-ADD     PARKSC        SKCLI(1)
393000021031     C                   ENDIF
393100021031     C*
393200021031     C                   ENDSR
393300970110     C*------------------------------------------------------------------------*
393400970110     C* *INZSR - OPERAZIONI INIZIALI
393500970110     C*------------------------------------------------------------------------*
393600970110     C     *INZSR        BEGSR
393700970110     C*--------------------
393800970110     C* RICEVIMENTO PARAMETRI
393900970110     C*--------------------
394000970110     C     *ENTRY        PLIST
394100970110     C                   PARM                    KPJBA
394200970110     C                   MOVEL     KPJBU         PARAM
394300021121     C*
394400021121     C* CARICA NUOVE TABELLE OCCORRENTI
394500021121     C                   EXSR      CARGEDCE
394600021121     C*
394700021121     C* REPERISCO SUBITO LA DIVISA DELLA TARIFFA RICEVUTA IN INPUT
394800021121     C                   IF        PARDIV = *BLANKS
394900021121     C                   Z-ADD     PARKOV        KTMKSC
395000021121     C                   Z-ADD     PARCTR        KTMCTR
395100021121     C                   Z-ADD     PARPRG        KTMPRG
395200021121     C     KEYTAMUNIQ    CHAIN     TNTAM01L
395300021121     C                   IF        %FOUND
395400021121     C                   MOVEL(P)  TAMFLO        DSTA01
395500021121     C                   MOVEL(P)  §TADIV        PARDIV
395600021121     C                   ELSE
395700021121     C                   MOVEL(P)  WDIVCE        PARDIV
395800021121     C                   ENDIF
395900021121     C                   ENDIF
396000970110     C*--------------------
396100970110     C* VARIABILI RIFERITE AL DATABASE
396200970110     C*--------------------
396300970110     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
396400970110     C     *LIKE         DEFINE    TBLCOD        KTBCOD
396500970110     C     *LIKE         DEFINE    TBLKEY        KTBKEY
396600970110     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
396700970110     C     *LIKE         DEFINE    ACOKCC        KACKCC
396800970110     C     *LIKE         DEFINE    ACOKSC        KACKSC
396900970110     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
397000970110     C     *LIKE         DEFINE    CLPKCC        KCLKCC
397100970110     C     *LIKE         DEFINE    CLPKSC        KCLKSC
397200970110     C     *LIKE         DEFINE    INDKUT        KINKUT                         *CNIND00F
397300970110     C     *LIKE         DEFINE    INDKCC        KINKCC
397400970415     C     *LIKE         DEFINE    INDKSC        KINKSC
397500970110     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
397600970110     C     *LIKE         DEFINE    TAMCTR        KTMCTR
397700021121     C     *LIKE         DEFINE    TAMPRG        KTMPRG
397800990820     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
397900970110     C     *LIKE         DEFINE    TADCTR        KTDCTR
398000970110     C     *LIKE         DEFINE    TADPRG        KTDPRG
398100970317     C     *LIKE         DEFINE    TADLNP        KTDLNP
398200990812     C     *LIKE         DEFINE    TASKSC        KTAKSC                         *TITAS00F
398300970110     C     *LIKE         DEFINE    TASAAS        KTAAAS
398400970110     C     *LIKE         DEFINE    TASMGS        KTAMGS
398500970110     C     *LIKE         DEFINE    CSBAAS        KSBAAS                         *TNCSB00F
398600970110     C     *LIKE         DEFINE    CSBLNP        KSBLNP
398700970110     C     *LIKE         DEFINE    CSBNRS        KSBNRS
398800970110     C     *LIKE         DEFINE    CSBNSP        KSBNSP
398900011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
399000980713     C     *LIKE         DEFINE    ETCCTO        KTCCTO
399100970110     C     *LIKE         DEFINE    ETCKSC        KTCKSC
399200970110     C     *LIKE         DEFINE    ETCCTR        KTCCTR
399300970110     C     *LIKE         DEFINE    ETCPRG        KTCPRG
399400970110     C     *LIKE         DEFINE    ETCLNP        KTCLNP
399500011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
399600980713     C     *LIKE         DEFINE    ETSCTO        KTSCTO
399700970110     C     *LIKE         DEFINE    ETSKSC        KTSKSC
399800970110     C     *LIKE         DEFINE    ETSCTR        KTSCTR
399900970110     C     *LIKE         DEFINE    ETSPRG        KTSPRG
400000970110     C     *LIKE         DEFINE    ETSLNP        KTSLNP
400100970110     C     *LIKE         DEFINE    ETSSGL        KTSSGL
400200011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
400300980713     C     *LIKE         DEFINE    ETTCTO        KTTCTO
400400970110     C     *LIKE         DEFINE    ETTKSC        KTTKSC
400500970110     C     *LIKE         DEFINE    ETTCTR        KTTCTR
400600970110     C     *LIKE         DEFINE    ETTPRG        KTTPRG
400700970110     C     *LIKE         DEFINE    ETTLNP        KTTLNP
400800970110     C     *LIKE         DEFINE    ETTFIE        KTTFIE
400900970110     C     *LIKE         DEFINE    ETTCTS        KTTCTS
401000970110     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DEPOSITI
401100970110     C     *LIKE         DEFINE    TAMCTR        CARCTR
401200970110     C     *LIKE         DEFINE    TAMPRG        CARPRG
401300970430     C     *LIKE         DEFINE    TASLNP        DEPFIL
401400980713     C     *LIKE         DEFINE    PARCTO        DEPCTO
401500020730     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
401600011128     C     *LIKE         DEFINE    TADORP        DEPORP
401700980821     C     *LIKE         DEFINE    ETCLNP        XORG                           *DI PASSAGGIO
401800980821     C     *LIKE         DEFINE    ETTFIE        XFIE
401900011011     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
402000011011     C     *LIKE         DEFINE    TBEKE1        KTEKE1
402100050309     C     *LIKE         DEFINE    GCPAAS        KGPAAS                         *TIGCP00F
402200021122     C     *LIKE         DEFINE    GCPLNP        KGPLNP
402300021122     C     *LIKE         DEFINE    GCPNRS        KGPNRS
402400021122     C     *LIKE         DEFINE    GCPNSP        KGPNSP
402500980821     C*--------------------
402600970110     C* CHAIVI DI LETTURA
402700970110     C*--------------------
402800060315     C* CHIAVE LETTURA TITA730C - COMPLETA
402900060315     C     KEYTA7        KLIST
403000060315     C                   KFLD                    TA7AAS
403100060315     C                   KFLD                    TA7LNP
403200060315     C                   KFLD                    TA7NRS
403300060315     C                   KFLD                    TA7NSP
403400060315     C                   KFLD                    TA7TBL
403500970110     C* LETTURA TABEL00F - COMPLETA
403600970110     C     KEYTAB        KLIST
403700970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
403800970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
403900970110     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
404000970110     C* LETTURA TABEL00F - PARZIALE
404100970110     C     KE2TAB        KLIST
404200970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
404300970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
404400970110     C* LETTURA CNACO00F - COMPETA
404500970110     C     KEYACO        KLIST
404600970110     C                   KFLD                    KACKUT                         *CODICE UTENTE
404700970110     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
404800970110     C                   KFLD                    KACKSC                         *CODICE CLIENTE
404900970110     C* LETTURA CNCLP00F - COMPLETA
405000970110     C     KEYCLP        KLIST
405100970110     C                   KFLD                    KCLKUT                         *CODICE UTENTE
405200970110     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
405300970110     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
405400970415     C* LETTURA CNIND00F - COMPLETA
405500970415     C     KEYIND        KLIST
405600970415     C                   KFLD                    KINKUT                         *CODICE UTENTE
405700970415     C                   KFLD                    KINKCC                         *CODICE CAPOCONTO
405800970415     C                   KFLD                    KINKSC                         *CODICE CLIENTE
405900970110     C* LETTURA TNTAM01L - PARZIALE
406000970110     C     KEYTAM        KLIST
406100970110     C                   KFLD                    KTMKSC                         *CLIENTE
406200970110     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
406300021121     C* LETTURA TNTAM01L - PARZIALE
406400021121     C     KEYTAMUNIQ    KLIST
406500021121     C                   KFLD                    KTMKSC                         *CLIENTE
406600021121     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
406700021121     C                   KFLD                    KTMPRG                         *PROGRESSIVO
406800990820     C* LETTURA TITAD01L - PARZIALE
406900970110     C     KEYTAD        KLIST
407000970110     C                   KFLD                    KTDKSC                         *CLIENTE
407100970110     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
407200970110     C                   KFLD                    KTDPRG                         *PROGRESSIVO
407300970317     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
407400990812     C* LETTURA TITAS31C - PARZIALE
407500970110     C     K31TAS        KLIST
407600970110     C                   KFLD                    KTAKSC                         *CLIENTE
407700990812     C* LETTURA TITAS38C - COMPLETA
407800970110     C     K38TAS        KLIST
407900970110     C                   KFLD                    KTAAAS                         *ANNO
408000970110     C                   KFLD                    KTAMGS                         *MESE/GIORNO
408100970110     C                   KFLD                    KTAKSC                         *CLIENTE
408200970110     C* LETTURA TNCSB03L - COMPLETA
408300970110     C     KEYCSB        KLIST
408400970110     C                   KFLD                    KSBAAS                         *ANNO
408500970110     C                   KFLD                    KSBLNP                         *LINEA PARTENZA
408600970110     C                   KFLD                    KSBNRS                         *SERIE
408700970110     C                   KFLD                    KSBNSP                         *SPEDIZIONE
408800011001     C* LETTURA TEETC01L - COMPLETA
408900981218     C     KEYETC        KLIST
409000980713     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
409100980713     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
409200970110     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
409300970110     C                   KFLD                    KTCCTR                         *TARIFFA
409400970110     C                   KFLD                    KTCPRG                         *PROGRESSIVO
409500970110     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
409600011001     C* LETTURA TEETC01L - PARZIALE
409700981218     C     KE3ETC        KLIST
409800981218     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
409900981218     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
410000981218     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
410100011001     C* LETTURA TEETS01L - COMPLETA
410200970110     C     KEYETS        KLIST
410300980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
410400970110     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
410500970110     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
410600970110     C                   KFLD                    KTSCTR                         *TARIFFA
410700970110     C                   KFLD                    KTSPRG                         *PROGRESSIVO
410800970110     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
410900970417     C                   KFLD                    KTSSGL                         *SCAGLIONE
411000011001     C* LETTURA TEETS01L - PARZIALE
411100970314     C     KE2ETS        KLIST
411200980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
411300970314     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
411400970314     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
411500970314     C                   KFLD                    KTSCTR                         *TARIFFA
411600970314     C                   KFLD                    KTSPRG                         *PROGRESSIVO
411700970314     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
411800011001     C* LETTURA TEETS01L - PARZIALE
411900981218     C     KE3ETS        KLIST
412000981218     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
412100981218     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
412200981218     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
412300011001     C* LETTURA TEETT01L - COMPLETA
412400970110     C     KEYETT        KLIST
412500980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
412600970110     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
412700970110     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
412800970110     C                   KFLD                    KTTCTR                         *TARIFFA
412900970110     C                   KFLD                    KTTPRG                         *PROGRESSIVO
413000970110     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
413100970110     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
413200970110     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
413300011001     C* LETTURA TEETT01L - PARZIALE
413400970314     C     KE2ETT        KLIST
413500980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
413600970314     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
413700970314     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
413800970314     C                   KFLD                    KTTCTR                         *TARIFFA
413900970314     C                   KFLD                    KTTPRG                         *PROGRESSIVO
414000970314     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
414100970314     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
414200040809     C* LETTURA fiar531c
414300040809     C     Kar5          KLIST
414400040809     C                   KFLD                    ar5aas                         *DATI SEDE O FILIALE
414500040809     C                   KFLD                    ar5lnp                         *TARIFFA O OFFERTA
414600040809     C                   KFLD                    ar5nrs                         *CLIENTE O VISITA
414700040809     C                   KFLD                    ar5nsp                         *CLIENTE O VISITA
414800040809     C                   KFLD                    ar5trd                         *CLIENTE O VISITA
414900040809     C* LETTURA TEETT01L - PARZIALE
415000040809     C     KE3ETT        KLIST
415100040809     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
415200040809     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
415300040809     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
415400011011     C* LETTURA TNTBE01L - PARZIALE
415500011011     C     KEYTBE        KLIST
415600011011     C                   KFLD                    KTECOD                         *UTENTE
415700011011     C                   KFLD                    KTEKE1                         *CODICE
415800050309     C* LETTURA TIGCP51L - PARZIALE
415900021122     C     K01GCP        KLIST
416000021122     C                   KFLD                    KGPAAS                         *ANNO
416100021122     C                   KFLD                    KGPLNP                         *LINEA DI PARTENZA
416200021122     C                   KFLD                    KGPNRS                         *SERIE
416300021122     C                   KFLD                    KGPNSP                         *SPEDIZIONE
416400970110     C*--------------------
416500970110     C* VARIABILI DI CONTROLLO
416600970110     C*--------------------
416700970110     C                   MOVEL     '0'           $FINE             1            *FINE PGM
416800970110     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
416900970416     C                   MOVEL     'S'           $OK               1            *RECORD OK
417000970312     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO TROVATO
417100970313     C                   MOVEL     '0'           $ERR              1            *ERRORE
417200970313     C                   MOVEL     '1'           *INOF                          *OVERFLOW QSYSPRT
417300970313     C                   MOVEL     '1'           *IN01                          *OVERFLOW TNTE41P
417400970110     C*
417500970110     C* REPERISCE I DATI DELL'UTENTE
417600020923     C                   EXSR      REPDATIUTE
417700970110     C*
417800970110     C* REPERISCE LA DATA CORRENTE
417900970110     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
418000970110     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
418100970110     C                   Z-ADD     WN8           G08DAT
418200970110     C                   Z-ADD     *ZEROS        G08INV
418300970110     C                   MOVEL     '0'           G08ERR
418400970110     C                   CALL      'XSRDA8'
418500970110     C                   PARM                    WLBDA8
418600970110     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
418700970110     C*
418800970110     C* CARICA TABELLE OCCORRENTI
418900970110     C                   EXSR      CARTAB
419000970313     C*
419100970313     C* CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
419200970313     C                   EXSR      CARTAR
419300981218     C*
419400981218     C* ANNULLAMENTO TARIFFA
419500981218     C                   EXSR      CANTAR
419600021031     C*
419700021031     C* REPERIMENTO LISTA CLIENTI PADRI/FIGLI
419800021031     C                   EXSR      REPCLI
419900970110     C*
420000970110     C                   ENDSR
420100970110     O*------------------------------------------------------------------------*
420200970110     OQSYSPRT   E            TES              02
420300970110     O                       STA(1)              66
420400970110     O                       STA(2)             132
420500970110     O                       DSFIRS              20
420600970110     O                       UDATE              117 '  /  /  '
420700970110     O                                          127 'Pag.'
420800970110     O                       PAGE          Z    132
420900970110     O*------------------------
421000970110     O          E            ETAS        1
421100970110     O                       STA(3)              66
421200970110     O                       STA(4)             132
421300970423     O                       TASAAS        Z     16
421400970423     O                       TASLNP        Z     20
421500970423     O                       TASNRS        Z     23
421600970423     O                       TASNSP        Z     31
421700970423     O                       TASTBL              34
421800970423     O                       TASCTS              43
421900970110     O                       DEPERR             124
422000970110     O*------------------------
422100970110     O          E            EETX        1
422200970110     O                       STA(3)              66
422300970110     O                       STA(5)             132
422400970110     O                       PARKOV        Z     15
422500970110     O                       PARCTR              24
422600970110     O                       PARPRG              33
422700970110     O                       PARLNP        Z     53
422800970110     O                       PARFIE              62
422900970110     O                       DEPERR             124
423000970110     O*------------------------------------------------------------------------*
423100970110** -DDEC-
423200970110NON CALCOLARE                 1
423300970110CALCOLARE                     2
423400970110CALCOLARE IN REGIONE          3
423500970110CALCOLARE IN NAZIONE          4
423600970110 **  CREAZIONE DATI  TIPO  ** 5
423700980821TUTTA L'ITALIA                6
423800980821TUTTO L'ESTERO                7
4239009808218
4240009808219
42410098082110
424200990812** MTAS - MESSAGGI PER TITAS
424300970110FILIALE BOLLA NON TROVATA                                         1
424400970423CODICE TASSAZIONE NON TROVATO O NON VALIDO                        2
4245009704163
4246009701104
4247009701105
424800011001** METX - MESSAGGI PER TEETX
424900970110DATI TIPO ALLOCATI                                                1
425000971008NESSUNA SPEDIZIONE: VALORE DEDOTTO                                2
425100980901CREAZIONE AUTOMATICA                                              3
4252009701104
4253009701105
425400970110** STA
425500970110XXXXXXXXXXXXXXXXXXXX        TNTE41R       ** ERRORI IN CREAZIONE D1
425600970110ATI TIPO TARIFFE DA CONTO ECONOMICO **     99/99/99      PAG. 99992
425700970423SPEDIZIONE: 9999 999 99 9999999 XX  CTS: XX                       3
425800970423ERRORE: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        4
425900970110CLIENTE 9999999  TAR 999  PRG 999  LINEA PARTENZA 999   I/E  X    5
