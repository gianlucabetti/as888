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
004900140916     FTEETT11L  UF A E           K DISK
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
023800140919     D*---
023900140919     D §CTc            S              2    DIM(600)                             *CODICE TASSAZIONE
024000140919     D §RCc            S              1    DIM(600)                             *RAGGRUPPAMENTO
024100140919     D §CPc            S              4  1 DIM(600)                             *% INCIDENZA
024200140919     D §CDc            S              1    DIM(600)                             *FLAG VALORE DEDOTTO
024300020227     D*-------------------
024400020227     D* SCHIERA FILIALI FEDEX
024500020227     D*-------------------
024600020227     D FILFED          S              3  0 DIM(999)
024700081105     D*-------------------
024800081105     D* SCHIERA FILIALI DPD
024900081105     D*-------------------
025000081105     D FILdpd          S              3  0 DIM(999)
025100970110     D*------------------------------------------------------------------------*
025200970110     D* INPUT
025300970110     D*------------------------------------------------------------------------*
025400970110     D*------------------
025500970110     D* DS SCHIERE MULTIPLE
025600970110     D*------------------
025700970110     D*---
025800970110     D* BYTE IN PACKED ->
025900970317     D* (3:2) +1=2 ; (2X30) =60
026000970317     D* (9:2) +1=5 ; (5X30) =150
026100970317     D* (9:2) +1=5 ; (5X999)=4995
026200970317     D* (9:2) +1=5 ; (5X10) =50
026300050103     D* (9:2) +1=5 ; (5X600)=1500
026400970317     D* (13:2)+1=7 ; (7X10) =70
026500011001     D* (18:2)+1=10; (10X10) =100
026600980820     D*---
026700971124     D FPAG            DS                  OCCURS(500) INZ
026800970110     D  GGP                    1     60P 0
026900970110     D                                     DIM(30)                              *GG PAGAMENTO
027000970110     D  SGP                   61    210P 0
027100970110     D                                     DIM(30)                              *SPED. GG PAGAMENTO
027200971124     D FRIT            DS                  OCCURS(500) INZ
027300970110     D  RIT                    1   4995P 0
027400970110     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
027500971124     D FCON            DS                  OCCURS(500) INZ
027600970110     D  CON                    1   4995P 0
027700970110     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
027800971124     D FSGL            DS                  OCCURS(500) INZ
027900041118     D  SSP                    1     50P 0
028000041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
028100041118     D  PSC                   51    110P 1
028200041118     D                                     DIM(10)                              *PESO
028300041118     D  CSC                  111    160P 0
028400041118     D                                     DIM(10)                              *COLLI
028500041118     D  VSC                  161    220P 3
028600041118     D                                     DIM(10)                              *VOLUME
028700060829     D FSGL1           DS                  OCCURS(500) INZ
028800060829     D  SSP1                   1     50P 0
028900060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
029000060829     D  PSC1                  51    110P 1
029100060829     D                                     DIM(10)                              *PESO
029200060829     D  CSC1                 111    160P 0
029300060829     D                                     DIM(10)                              *COLLI
029400060829     D  VSC1                 161    220P 3
029500060829     D                                     DIM(10)                              *VOLUME
029600971124     D FCTS            DS                  OCCURS(500) INZ
029700050103     D  TSI                    1   3000P 0
029800050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
029900050103     D  TSE                 3001   6000P 0
030000050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
030100971124     D FTAR            DS                  OCCURS(500) INZ
030200970423     D  SSGL                   1     70P 3
030300970314     D                                     DIM(10)                              *SCAGLIONI
030400050103     D  CCTI                  71   1270
030500050103     D                                     DIM(600)                             *COD.TASSAZIONE ITAL
030600050103     D  CRCI                1271   1870
030700050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ITAL
030800050103     D  CCTE                1871   3070
030900050103     D                                     DIM(600)                             *COD.TASSAZIONE ESTE
031000050103     D  CRCE                3071   3670
031100050103     D                                     DIM(600)                             *RAGGRUPPAMENTO ESTE
031200140916     D cccte           DS                  OCCURS(600) INZ
031300140919     D  ctse                   1   3000P 0
031400140919     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
031500140919     D  nc                  3001  10200
031600140919     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
031700980820     D*---
031800980820     D* CARTELLO
031900980820     D*---
032000980824     D CCAR            DS                  OCCURS(2) INZ
032100980821     D  USPE                   1      5P 0
032200980821     D                                     DIM(1)                               *N°SPEDIZIONI ITA/ES
032300980821     D  UCLA                   6     10P 0
032400980821     D                                     DIM(1)                               *CLIENTE TIPO A
032500980821     D  UCLB                  11     15P 0
032600980821     D                                     DIM(1)                               *CLIENTE TIPO B
032700980821     D  UCLC                  16     20P 0
032800980821     D                                     DIM(1)                               *CLIENTE TIPO C
032900980821     D  UBON                  21     25P 0
033000980821     D                                     DIM(1)                               *BOLLETTAZIONE (' ')
033100980821     D  UBOA                  26     30P 0
033200980821     D                                     DIM(1)                               *BOLLETTAZIONE (A)
033300980821     D  UBOB                  31     35P 0
033400980821     D                                     DIM(1)                               *BOLLETTAZIONE (B)
033500980821     D  UNCL                  36     40P 0
033600980821     D                                     DIM(1)                               *COLLI
033700980821     D  UPKB                  41     45P 1
033800980821     D                                     DIM(1)                               *PESO
033900980821     D  UVLB                  46     50P 3
034000980821     D                                     DIM(1)                               *VOLUME
034100980821     D  UFRA                  51     55P 0
034200980821     D                                     DIM(1)                               *FRANCHI
034300980821     D  UASS                  56     60P 0
034400980821     D                                     DIM(1)                               *ASSEGNATI
034500980821     D  UPCV                  61     65P 0
034600980821     D                                     DIM(1)                               *CONSEGNE PRIVATI
034700980821     D  UPCM                  66     70P 0
034800980821     D                                     DIM(1)                               *CONSEGNE MAGAZZINO
034900980821     D  UPFD                  71     75P 0
035000980821     D                                     DIM(1)                               *FERMO DEPOSITO
035100980821     D  UESP                  76     80P 0
035200980821     D                                     DIM(1)                               *ESPRESSI
035300980821     D  UPPV                  81     85P 0
035400980821     D                                     DIM(1)                               *CONSEGNE PROVINCIA
035500980821     D  UPCA                  86     90P 0
035600980821     D                                     DIM(1)                               *CONSEGNE APPUNTAMEN
035700980821     D  UPCP                  91     95P 0
035800980821     D                                     DIM(1)                               *CONSEGNE PIANI
035900980821     D  UPCS                  96    100P 0
036000980821     D                                     DIM(1)                               *CONSEGNE SUPERMERCA
036100980821     D  UPCF                 101    105P 0
036200980821     D                                     DIM(1)                               *CONSEGNE FUORI MISU
036300980821     D  UPAB                 106    110P 0
036400980821     D                                     DIM(1)                               *C/ASSEGNI BARTOLINI
036500980821     D  UPAM                 111    115P 0
036600980821     D                                     DIM(1)                               *C/ASSEGNI MITTENTE
036700011002     D  UCAS                 116    125P 5
036800011002     D                                     DIM(1)                               *C/ASSEGNO
036900011002     D                                     PACKEVEN
037000011002     D  UPAR                 126    130P 0
037100980821     D                                     DIM(1)                               *ASSICURAZIONE R.C.V
037200011002     D  UPAC                 131    135P 0
037300980821     D                                     DIM(1)                               *ASSICURAZIONE X CON
037400011002     D  UASC                 136    145P 5
037500011001     D                                     DIM(1)                               *ASSICURAZIONE
037600011001     D                                     PACKEVEN
037700011002     D  UPGI                 146    150P 0
037800980821     D                                     DIM(1)                               *GIACENZE
037900011002     D  UPCD                 151    155P 0
038000980821     D                                     DIM(1)                               *CONSEGNE DDT
038100011002     D  UPCC                 156    160P 0
038200980821     D                                     DIM(1)                               *CONSEGNE C.STORICI
038300011002     D  UPCL                 161    165P 0
038400980821     D                                     DIM(1)                               *CONSEGNE L.DISAGIAT
038500011002     D  UCLT                 166    170P 0
038600980901     D                                     DIM(1)                               *CLIENTE TIPO T
038700011002     D  UGGS                 171    175P 0
038800000414     D                                     DIM(1)                               *GG SOSTA GIACENZA
038900011002     D  UNRG                 176    180P 0
039000000414     D                                     DIM(1)                               *N° RIAPERTURE GIACE
039100011011     D  UCIS                 181    185P 0
039200011011     D                                     DIM(1)                               *CONSEGNE ISOLE
039300011011     D  URIS                 186    190P 0
039400011011     D                                     DIM(1)                               *RITIRI ISOLE
039500020215     D  UCLD                 191    195P 0
039600020215     D                                     DIM(1)                               *CLIENTE TIPO D
039700040802     D  Uded                 196    200P 0
039800040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
039900040811     D  Usbnb                206    210P 0
040000040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040100040824     D  Usban                211    215P 0
040200040824     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040300040824     D  Unbn                 216    220P 0
040400040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040500040824     D  Unba                 221    225P 0
040600040811     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040700040824     D  Uval                 226    230P 0
040800040802     D                                     DIM(1)                               *CONSEGNE FUORI MISU
040900050808     D  Upks                 231    235P 0
041000050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
041100050808     D  Upkn                 236    240P 0
041200050808     D                                     DIM(1)                               *CONSEGNE FUORI MISU
041300060315     D  Uxx1                 241    245P 0
041400060315     D                                     DIM(1)                               *ASSICURAZIONE X CON
041500060829     D  UNCL1                246    250P 0
041600060829     D                                     DIM(1)                               *COLLI
041700060829     D  UPKB1                251    255P 1
041800060829     D                                     DIM(1)                               *PESO
041900060829     D  UVLB1                256    260P 3
042000060829     D                                     DIM(1)                               *VOLUME
042100980824     D CPAG            DS                  OCCURS(2) INZ
042200980820     D  UGGP                   1     60P 0
042300980819     D                                     DIM(30)                              *GG PAGAMENTO
042400980820     D  USGP                  61    210P 0
042500980819     D                                     DIM(30)                              *SPED. GG PAGAMENTO
042600980824     D CRIT            DS                  OCCURS(2) INZ
042700980820     D  URIT                   1   4995P 0
042800980819     D                                     DIM(999)                             *SPEDIZIONI/RITIRO
042900980824     D CCON            DS                  OCCURS(2) INZ
043000980820     D  UCON                   1   4995P 0
043100980819     D                                     DIM(999)                             *SPEDIZIONI/CONSEGNA
043200980824     D CSGL            DS                  OCCURS(2) INZ
043300041118     D  USSP                   1     50P 0
043400041118     D                                     DIM(10)                              *N°SPED.SCAGLIONE
043500041118     D  UPSC                  51    110P 1
043600041118     D                                     DIM(10)                              *PESO
043700041118     D  UCSC                 111    160P 0
043800041118     D                                     DIM(10)                              *COLLI
043900041118     D  UVSC                 161    220P 3
044000041118     D                                     DIM(10)                              *VOLUME
044100060829     D CSGL1           DS                  OCCURS(2) INZ
044200060829     D  USSP1                  1     50P 0
044300060829     D                                     DIM(10)                              *N°SPED.SCAGLIONE
044400060829     D  UPSC1                 51    110P 1
044500060829     D                                     DIM(10)                              *PESO
044600060829     D  UCSC1                111    160P 0
044700060829     D                                     DIM(10)                              *COLLI
044800060829     D  UVSC1                161    220P 3
044900060829     D                                     DIM(10)                              *VOLUME
045000980824     D CCTS            DS                  OCCURS(2) INZ
045100050103     D  UTSI                   1   3000P 0
045200050103     D                                     DIM(600)                             *N°SPED.COD.TASS.ITA
045300050103     D  UTSE                3001   6000P 0
045400050103     D                                     DIM(600)                             *N°SPED.COD.TASS.EST
045500040803     D Dtasflo       E DS
045600091125     D tnte04ds      E DS
045700970110     D*-------------------
045800970110     D* DS TIPI TARIFFA
045900970110     D*-------------------
046000970110     D DSTR          E DS
046100050808     D*-------------------
046200050808     D* DS cod. trattamento merce
046300050808     D*-------------------
046400050808     D DS1b          E DS
046500021202     D*-------------------
046600021202     D* DS *PGM REPIMEMTO TARIFFE DI CARTELLO
046700021202     D*-------------------
046800021202     D TRUL27DS      E DS
046900970110     D*-------------------
047000970110     D* DS CODICI TASSAZIONE
047100970110     D*-------------------
047200970110     D DSCT          E DS
047300970110     D*-------------------
047400970110     D* DS CAMBI E DIVISE
047500970110     D*-------------------
047600011011     D*DSCV          E DS
047700970110     D*-------------------
047800970110     D* DS RAGGRUPPAMENTI TIPI BOLLA
047900970110     D*-------------------
048000970110     D DS2W          E DS
048100970110     D*------------------
048200970110     D* DS TIPI BOLLA
048300970110     D*------------------
048400970110     D DSTB          E DS
048500970110     D*------------------
048600970110     D* DS CODICI PAGAMENTO
048700970110     D*------------------
048800970110     D DSFA          E DS
048900970110     D*------------------
049000970110     D* DS COMPOSIZIONE CODICE DI PAGAMENTO
049100970110     D*------------------
049200970110     D                 DS
049300970110     D  WCDP                   1      3
049400970110     D  WLIN                   4      4
049500970110     D  DSCDP                  1      4
049600970110     D*------------------
049700970110     D* DS INVIO DATI -BOLLETTAZIONE-
049800970110     D*------------------
049900970110     D DS3C          E DS
050000970110     D*------------------
050100020923     D* DS REPERIMENTO DATI UTENTE
050200970110     D*------------------
050300020923     D TIBS34DS      E DS                                                       *Profili utente
050400020923     D DDATIUTE      E DS                                                       *Dati utente
050500020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
050600970110     D*------------------
050700970110     D* DS "XSRDA8" - CONTROLLA DATA (8)
050800970110     D*------------------
050900970110     D WLBDA8          DS
051000970110     D  G08DAT                 1      8  0
051100970110     D  G08INV                 9     16  0
051200970110     D  G08ERR                17     17
051300970110     D  G08TGI                18     22  0
051400970110     D*-------------------
051500970110     D* DS SCOMPOSIZIONE DATA DECORRENZA(4) -TNTAM-
051600970110     D*-------------------
051700970110     D                 DS
051800970110     D  TAMDNN                 3      4  0
051900970110     D  TAMDMM                 5      6  0
052000970110     D  TAMDEC                 3      6  0
052100970110     D  TAMDDT                 1      8  0
052200970110     D*-------------------
052300970110     D* DS SCOMPOSIZIONE DATA SCADENZA(4) -TNTAM-
052400970110     D*-------------------
052500970110     D                 DS
052600970110     D  TAMSNN                 3      4  0
052700970110     D  TAMSMM                 5      6  0
052800970110     D  TAMSCA                 3      6  0
052900970110     D  TAMDST                 1      8  0
053000970110     D*-------------------
053100990812     D* DS COMPOSIZIONE DATA SPEDIZIONE -TITAS-
053200970110     D*-------------------
053300970110     D                 DS
053400970110     D  TASAAS                 1      4  0
053500970110     D  TASMGS                 5      8  0
053600970110     D  TASDSP                 1      8  0
053700970110     D*------------------
053800970110     D* DS PARAMETRI DI LANCIO
053900970110     D*------------------
054000970110     D PARAM           DS
054100980825     D  PARKSC                 1      7  0
054200980825     D  PARFPA                 8     10  0
054300980825     D  PARFAR                11     13  0
054400980825     D  PARFAP                14     16  0
054500980825     D  PARDS1                17     24  0
054600980825     D  PARAA1                17     20  0
054700980825     D  PARMG1                21     24  0
054800980825     D  PARDS2                25     32  0
054900980825     D  PARTAR                33     35
055000980825     D  PARCTO                36     36
055100980825     D  PARKOV                37     43  0
055200980825     D  PARCAR                37     41  0
055300980825     D  PARCTR                44     46  0
055400980825     D  PARTTR                44     44
055500980825     D  PARPRG                47     49  0
055600980825     D  PARLNP                50     52  0
055700980825     D  PARFIE                53     53
055800980825     D  PARSIM                54     54
055900980825     D  PARELA                55     55
056000011003     D  PARDIV                56     58
056100021031     D  PARUNI                59     59
056200040804     D  PARsta                60     60
056300040805     D  PARint                61     61
056400040805     D  PARsie                62     62
056500091130     D  PARdsf                63     63
056600970110     D*------------------
056700970110     D* ARCHITETTURA
056800970110     D*------------------
056900970110     D KPJBA         E DS
057000970110     D  PROFIL                 1     10
057100011011     D*------------------
057200011011     D* DS LETTURA TABELLA "GED" TIPO RECORD "CE"
057300011011     D*------------------
057400011011     D DSGEDCE       E DS
057500011011     D*-------------------
057600011011     D* CONVERSIONI IMPORTI
057700011011     D*-------------------
057800011011     D YEUR          E DS                  EXTNAME(YEURCODS)
057900020227     D*-------------------
058000020227     D* DS ESTERNA X DETERMINAZIONE NETWORK FILIALE DA ORGANIGRAMMA
058100020227     D*-------------------
058200020227     D OG143         E DS
058300020730     D*------------------
058400020730     D* DS PER LETTURA TABELLA "NTW"
058500020730     D*------------------
058600020730     D DNTW          E DS
058700021121     D*------------------
058800021121     D* DS RIDEFINIZIONE CAMPO TAMFLO DI TNTAM
058900021121     D*------------------
059000021121     D DSTA01        E DS
059100021031     D*-------------------
059200021031     D* DS REPERIMENTO/CONTROLLO PADRE/FIGLI
059300021031     D*-------------------
059400021031     D TIBS10DS      E DS
059500021031     D  SKCLI                 21   5520  0 DIM(500)                              -cli. padri/figli
059600040811     D*-------------------
059700040811     D* CAMPI DI LAVORO
059800040811     D*-------------------
059900060829     d colli           s                   like(tasncl)
060000060829     d colli1          s                   like(tasncl)
060100040811     d $pes            s                   like(taspkf)
060200060829     d $pes1           s                   like(taspkf)
060300060829     d etcpmc1         s                   like(etcpmc)
060400060829     d etcvmc1         s                   like(etcvmc)
060500970110     C*------------------------------------------------------------------------*
060600970110     C* MAIN LINE
060700970110     C*------------------------------------------------------------------------*
060800970313     C*
060900970313     C* SE NESSUN ERRORE IN APERTURA
061000970313IF  1C     $FINE         IFEQ      '0'
061100970313     C*
061200970314     C* ANNULLA I DATI TIPO ESISTENTI
061300970314     C                   EXSR      ANNETX
061400970110     C* APRE GLI ARCHIVI
061500970110     C                   EXSR      OPEFIL
061600970415     C*
061700970416     C* LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
061800970416     C                   EXSR      LETCLI
061900970416     C*
062000970416     C* REPERISCE LA QUALITA' DEL CLIENTE TARIFFA / OFFERTA
062100970416     C                   MOVEL     'C'           $CLV              1
062200970416IF  1C     CLPCLV        IFNE      *BLANKS
062300970416     C                   MOVEL     CLPCLV        $CLV
062400970416E   1C                   ENDIF
062500970416     C*
062600970416     C* REPERISCE I GIORNI DI PAGAMENTO DEL CLIENTE / VISITA
062700970416     C                   Z-ADD     *ZEROS        $GGP              3 0
062800970416     C                   EXSR      REPGGP
062900021129     C*
063000021129     C* CICLO SULLA SCHIERA DEI CLIENTI DA ELABORARE (DISCORSO UNIFICANTE)
063100021129     C     1             DO        500           JJ                3 0
063200021129     C                   Z-ADD     SKCLI(JJ)     KTAKSC                         *CLIENTE
063300970110     C*
063400970110     C* POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
063500970110     C                   EXSR      SETFIL
063600970110     C*
063700970110     C* CICLO FINO A FINE -BOLLE-
063800970313DO  2C     $FINE         DOWEQ     '0'
063900970110     C*
064000970110     C* MEMORIZZA I DATI
064100970110     C                   EXSR      MEMDAT
064200970110     C*
064300970110     C* LETTURA SUCESSIVA -BOLLE-
064400970110     C                   EXSR      LETFIL
064500970313E   2C                   ENDDO                                                  *FINE BOLLE
064600021129     C*
064700021129     C* DOPO L'ELABORAZIONE DI OGNI CLIENTE RE-INIZIALIZZO IL FLAG DI "FINE"
064800021129     C                   MOVEL     '0'           $FINE
064900021129     C*
065000021129     C* TESTO LE CONDIZIONI X CUI USCIRE DAL CICLO DI MEMORIZZAZIONE
065100021129     C                   IF        SKCLI(JJ) = *ZEROS
065200021129     C                   MOVEL     '1'           $FINE
065300021129     C                   LEAVE
065400021129     C                   ENDIF
065500021129     C*
065600021129     C                   ENDDO
065700970110     C*
065800970110     C* SCRIVE -SE RICHIESTO- E STAMPA I DATI TIPO
065900970110     C                   EXSR      AGGETX
066000980818     C*
066100980818     C* SCRIVE E STAMPA I DATI TIPO DI CARTELLO -SE RICHIESTE TUTTE LE LINE DI PARTENZA-
066200980818IF  2C     PARCTO        IFEQ      'T'
066300980818     C     PARCAR        ANDEQ     88888
066400980818     C     PARLNP        ANDEQ     999
066500980818     C                   EXSR      AGGET8
066600980818E   2C                   ENDIF
066700970110     C*
066800970110     C* CHIUDE GLI ARCHIVI
066900970110     C                   EXSR      CLOFIL
067000970313E   1C                   ENDIF
067100040804     c* lancio la stampa se richiesto
067200040804     c                   if        parsta = 'S'
067300040804     c                   call      'TNTE49R'
067400040804     c                   parm                    kpjba
067500040804     c                   end
067600970110     C*
067700970110     C                   SETON                                        LR
067800970415     C*------------------------------------------------------------------------*
067900970416     C* LETCLI - LEGGE LE ANAGRAFICHE DEL CLIENTE TARIFFA / OFFERTA
068000970415     C*------------------------------------------------------------------------*
068100970416     C     LETCLI        BEGSR
068200970415     C*
068300060808     C                   CLEAR                   CNCLP000                       *AZZERA RECORD
068400060808     C                   CLEAR                   CNIND000                       *AZZERA RECORD
068500060808     C                   CLEAR                   tfCLP000                       *AZZERA RECORD
068600060808     C                   CLEAR                   tfIND000                       *AZZERA RECORD
068700020923     C                   Z-ADD     1             KCLKUT                         *PARTICOLARITA'
068800020923     C                   Z-ADD     DUTKCI        KCLKCC
068900970415     C                   Z-ADD     PARKOV        KCLKSC
069000020923     C                   Z-ADD     1             KINKUT                         *INDIRIZZO
069100020923     C                   Z-ADD     DUTKCI        KINKCC
069200970415     C                   Z-ADD     PARKOV        KINKSC
069300060808IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
069400060808     C     KEYCLP        CHAIN     CNCLP00F                           99
069500060808     C     KEYIND        CHAIN     CNIND00F                           99
069600060808X   1C                   ELSE                                                   *OFFERTA
069700060804     C     KEYCLP        CHAIN     TFCLP00F                           99
069800060804     C     KEYIND        CHAIN     TFIND00F                           99
069900060804IF  2C     *IN99         IFEQ      *ON                                          *NON TROVATO
070000091125     c                   clear                   tnte04ds
070100091130     C                   movel     pardsf        d04dsf
070200091125     C                   Z-ADD     parkov        d04nrv
070300091125     c                   call      'TNTE04R'
070400091125     c                   parm                    kpjba
070500091126     c                   parm                    tnte04ds
070600091125     C                   Z-ADD     d04KSC        KCLKSC                         *CLIENTE DELLA VISIT
070700091125     C                   Z-ADD     d04KSC        KINKSC                         *CLIENTE DELLA VISIT
070800060808     C     KEYCLP        CHAIN     CNCLP00F                           99
070900060808     C     KEYIND        CHAIN     CNIND00F                           99
071000060804     c                   end
071100060808     c                   end
071200970415     C*
071300970415     C                   ENDSR
071400970415     C*------------------------------------------------------------------------*
071500970415     C* REPGGP - REPERISCE IL NUMERO DEI GIORNI DI PAGAMENTO
071600970415     C*------------------------------------------------------------------------*
071700970415     C     REPGGP        BEGSR
071800970415     C*---
071900970415     C* IL GG DI PAGAMENTO IN TABELLA POSSONO ESSERE ESPRESSI IN MESI SE <=12 O
072000970415     C* IN GIORNI SE >12;
072100970415     C* CI POSSONO ESSERE INOLTRE + RATE DI PAGAMENTO E IN QUESTO CASO PRENDE
072200970415     C* SEMPRE L'ULTIMA (ES. SE 30/60/90 PRENDE 90 COME GGP).
072300970415     C* IL PAGAMENTO A VISTA NON HA GIORNI, MA PER COMODITA' SI MEMORIZZA CON
072400970415     C* CON GIORNI DI PAGAMENTO = 999
072500970415     C*---
072600970415     C                   MOVE      INDCDP        WCDP                           *CONDIZIONE PAGAMENT
072700000323     C                   MOVEL     '1'           WLIN                           *LINGUA
072800970415     C                   SETOFF                                           98
072900970415     C                   Z-ADD     1             I
073000970415     C     DSCDP         LOOKUP    CDP(I)                                 98
073100970415IF  1C     *IN98         IFEQ      *ON                                          *TROVATO
073200970415     C                   MOVEA     CRAT(I)       RAT
073300970415     C                   Z-ADD     10            I
073400970415DO  2C     I             DOWGT     0
073500970415IF  3C     RAT(I)        IFGT      *ZEROS                                       *P I E N O
073600970415     C                   MOVEL     RAT(I)        WN3               3 0
073700970415IF  4C     WN3           IFGT      12                                           *ESPRESSO IN GIORNI
073800970415     C                   Z-ADD     WN3           $GGP
073900970415X   4C                   ELSE                                                   *ESPRESSO IN MESI
074000970415     C     WN3           MULT      30            $GGP
074100970415E   4C                   ENDIF
074200981020     C                   LEAVE                                                  *ESCE DAL CICLO
074300970415X   3C                   ELSE                                                   *V U O T O
074400970415     C     I             SUB       1             I                              *DECREMENTA INDICE
074500970415E   3C                   ENDIF
074600970415E   2C                   ENDDO
074700970415E   1C                   ENDIF
074800970415     C*
074900970415     C* PAGAMENTO A VISTA (GGP=0), IMPOSTA GGP=999
075000970415IF  1C     $GGP          IFEQ      *ZEROS
075100970415     C                   Z-ADD     999           $GGP
075200970415E   1C                   ENDIF
075300970415     C*
075400970415     C                   ENDSR
075500970110     C*------------------------------------------------------------------------*
075600970110     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA -BOLLE-
075700970110     C*------------------------------------------------------------------------*
075800970110     C     SETFIL        BEGSR
075900970110     C*
076000021105IF  1C     PARKSC        IFGT      *ZEROS
076100990812     C     K31TAS        SETLL     TITAS31C                           99        *FINE ARCHIVIO
076200970110X   1C                   ELSE
076300970110     C                   Z-ADD     PARAA1        KTAAAS                         *ANNO
076400970110     C                   Z-ADD     PARMG1        KTAMGS                         *MESE/GIORNO
076500990812     C     K38TAS        SETLL     TITAS38C                           99        *FINE ARCHIVIO
076600970110E   1C                   ENDIF
076700970110     C*
076800970110IF  1C     *IN99         IFEQ      *ON
076900970110     C                   MOVEL     '1'           $FINE                          *FINE PGM
077000970110X   1C                   ELSE
077100970110     C                   EXSR      LETFIL                                       *LETTURA RECORD
077200970110E   1C                   ENDIF
077300970110     C*
077400970110     C                   ENDSR
077500970110     C*------------------------------------------------------------------------*
077600970110     C* LETFIL - LETTURA PROSSIMO RECORD -BOLLE-
077700970110     C*------------------------------------------------------------------------*
077800970110     C     LETFIL        BEGSR
077900970110     C*
078000970110     C* LETTURA FINO A:
078100970110     C*  - FINE FILE
078200970110     C*  - TROVATO RECORD VALIDO
078300970110     C                   MOVEL     'N'           WRECOK
078400970110DO  1C     $FINE         DOWEQ     '0'
078500970110     C     WRECOK        ANDEQ     'N'
078600970110     C*
078700970110IF  2C     PARKSC        IFGT      *ZEROS
078800990812     C                   READ      TITAS31C                               99
078900970110X   2C                   ELSE
079000990812     C                   READ      TITAS38C                               99
079100970110E   2C                   ENDIF
079200970110     C*
079300970110IF  2C     *IN99         IFEQ      *ON                                          *FINE ARCHIVIO
079400970110     C                   MOVEL     '1'           $FINE
079500970110X   2C                   ELSE
079600970110     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
079700970110E   2C                   ENDIF
079800970110E   1C                   ENDDO
079900970110     C*
080000970110     C                   ENDSR
080100970110     C*------------------------------------------------------------------------*
080200970110     C* CHKREC - CONTROLLA VALIDITA' RECORD -BOLLE-
080300970110     C*------------------------------------------------------------------------*
080400970110     C     CHKREC        BEGSR
080500970110     C*
080600970110     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
080700081105     C*
080800081105     C* se sto elaborando la cartello i dati tipo della cartello escludo le
080900081105     c* le spedizioni con lnp e lna DPD e FED
081000081105IF  2C     PARCTO        IFEQ      'T'
081100081105     C     PARCAR        ANDEQ     88888
081200081105     C     PARLNP        ANDEQ     999
081300081105     c* fedex
081400081105     C     taslnp        LOOKUP    FILFED                                 55
081500081105     C                   IF        *IN55 = *ON
081600081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
081700081105     C                   ENDIF
081800081105     C     taslna        LOOKUP    FILFED                                 55
081900081105     C                   IF        *IN55 = *ON
082000081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
082100081105     C                   ENDIF
082200081105     c* dpd
082300081105     C     taslnp        LOOKUP    FILdpd                                 55
082400081105     C                   IF        *IN55 = *ON
082500081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
082600081105     C                   ENDIF
082700081105     C     taslna        LOOKUP    FILdpd                                 55
082800081105     C                   IF        *IN55 = *ON
082900081105     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083000081105     C                   ENDIF
083100081105E   1C                   ENDIF
083200970423     C*
083300970423     C* TIPO BOLLA NON VALIDO COME SPEDIZIONE O RICAVO, ESCLUDE
083400970416     C                   Z-ADD     1             I
083500970416     C                   SETOFF                                           98
083600970416     C     TASTBL        LOOKUP    TBL(I)                                 98
083700970416IF  1C     *IN98         IFEQ      *ON                                          *TIPO BOLLA TROVATO
083800970416IF  2C     SPD(I)        IFEQ      'S'
083900970416     C     RIC(I)        OREQ      'S'
084000970417     C                   MOVEL     SPD(I)        DEPSP             1            *VALIDA COME SPEDIZ
084100970417     C                   MOVEL     RIC(I)        DEPRI             1            *VALIDA COME RICAVO
084200970416X   2C                   ELSE
084300970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
084400970416E   2C                   ENDIF
084500970416X   1C                   ELSE
084600970416     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
084700970416E   1C                   ENDIF
084800970423     C*
084900970110     C* CLIENTE <> DA QUELLO RICHIESTO, ESCLUDE ED ESCE DAL PGM
085000970110IF  1C     PARKSC        IFGT      *ZEROS                                       *LETTURA X CLIENTE
085100021031     C* VERIFICO CHE IL CLIENTE CORRENTE SIA NELLA LISTA DI QUELLI DA CONSIDERARE
085200021031     C                   Z-ADD     TASKSC        WRKCLI           11 0
085300021031     C     WRKCLI        LOOKUP    SKCLI                                  60
085400021031IF  2C     *IN60         IFEQ      *OFF
085500021031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
085600021105     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
085700021031E   2C                   ENDIF
085800970110E   1C                   ENDIF
085900970110     C*
086000970110     C* DATA SPEDIZIONE > DA DATA FINALE RICHIESTA, ESCLUDE ED ESCE DAL PGM
086100970110IF  1C     PARKSC        IFEQ      *ZEROS                                       *LETTURA X DATA
086200970110     C     PARDS1        ANDGT     *ZEROS
086300970110IF  2C     TASDSP        IFGT      PARDS2
086400970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
086500970110     C                   MOVEL     '1'           $FINE                          *FINE PROGRAMMA
086600970110E   2C                   ENDIF
086700970110E   1C                   ENDIF
086800970110     C*
086900970430     C* FILIALE PARTENZA <> DA QUELLA RICHIESTA, ESCLUDE
087000970430IF  1C     PARFPA        IFGT      *ZEROS
087100970430IF  2C     PARFPA        IFNE      TASLNP
087200970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
087300970110E   2C                   ENDIF
087400970110E   1C                   ENDIF
087500970430     C*
087600970430     C* FILIALE ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
087700970430IF  1C     PARFAR        IFGT      *ZEROS
087800970430IF  2C     PARFAR        IFNE      TASLNA
087900970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
088000970430E   2C                   ENDIF
088100970430E   1C                   ENDIF
088200970430     C*
088300970430     C* FILIALE PARTENZA O ARRIVO <> DA QUELLA RICHIESTA, ESCLUDE
088400970430IF  1C     PARFAP        IFGT      *ZEROS
088500970430IF  2C     PARFAP        IFEQ      TASLNP
088600970430     C     PARFAP        OREQ      TASLNA
088700970430X   2C                   ELSE
088800970430     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
088900970430E   2C                   ENDIF
089000970430E   1C                   ENDIF
089100020227     C*
089200020227     C* QUALUNQUE FILIALE INDICATA NON SIA FEDEX
089300020307     C     PARFPA        IFGT      *ZEROS
089400020227     C     PARFPA        LOOKUP    FILFED                                 55
089500020227     C                   IF        *IN55 = *ON
089600020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
089700020227     C                   ENDIF
089800020307     C                   ENDIF
089900020307     C     PARFAR        IFGT      *ZEROS
090000020227     C     PARFAR        LOOKUP    FILFED                                 55
090100020227     C                   IF        *IN55 = *ON
090200020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090300020227     C                   ENDIF
090400020307     C                   ENDIF
090500020307     C     PARFAP        IFGT      *ZEROS
090600020227     C     PARFAP        LOOKUP    FILFED                                 55
090700020227     C                   IF        *IN55 = *ON
090800020227     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090900020227     C                   ENDIF
091000020307     C                   ENDIF
091100970110     C*
091200970110     C* DATA SPEDIZIONE <> DA QUELLE RICHIESTE, ESCLUDE
091300970110IF  1C     PARDS1        IFGT      *ZEROS
091400970110IF  2C     TASDSP        IFGE      PARDS1
091500970110     C     TASDSP        ANDLE     PARDS2
091600970110X   2C                   ELSE
091700970110     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
091800970110E   2C                   ENDIF
091900970110E   1C                   ENDIF
092000970313     C*
092100970313     C* CODICE TARIFFA <> DA QUELLO RICHIESTO, ESCLUDE
092200970313IF  1C     PARTAR        IFNE      *BLANKS
092300970313     C                   MOVEL     PARTAR        WN3               3 0
092400970313IF  2C     WN3           IFNE      TASCTR
092500970313     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
092600970313E   2C                   ENDIF
092700970313E   1C                   ENDIF
092800970430     C*
092900970430     C* RECORD VALIDO, IMPOPSTA LA FILIALE SULLA QUALE MEMORIZZARE I DATI
093000970430IF  1C     WRECOK        IFEQ      'S'
093100970430IF  2C     PARLNP        IFNE      999                                          *SELEZIONE FILIALE
093200970430     C                   Z-ADD     PARLNP        DEPFIL                         *FILIALE=SELEZIONE
093300970430X   2C                   ELSE                                                   *SELEZIONE MULTIPLA
093400970430     C                   Z-ADD     TASLNP        DEPFIL                         *FILIALE=L.PARTENZA
093500970430E   2C                   ENDIF
093600060829     c                   z-add     tasncl        colli
093700060829     c                   z-add     tasncl        colli1
093800040811     c* reperisce i bancali. se BNB imposto nel campo di comodo COLLI
093900040811     c* i colli originali
094000040811     c                   clear                   dar5ban
094100040811     c                   clear                   dar5bnb
094200040811     c                   if        %subst(tasgva: 1: 1) = 'O' or
094300040811     c                             %subst(tasgva: 1: 1) = 'B'
094400040811     c                   if        %subst(tasgva: 1: 1) = 'O'
094500040811     c                   eval      ar5trd = 'BNB'
094600040811     c                   else
094700040811     c                   eval      ar5trd = 'BAN'
094800040811     c                   end
094900060829     c                   exsr      srfiar5
095000040811     c                   if        %subst(tasgva: 1: 1) = 'O'
095100040811     c                   eval      dar5bnb = ar5uni
095200060829     c                   z-add     §ar5bcor      colli
095300040811     c                   else
095400040811     c                   eval      dar5BAN = ar5uni
095500040811     c                   end
095600040811     c                   end
095700040811E   1C                   ENDIF
095800970416     C*
095900970110     C                   ENDSR
096000970110     C*------------------------------------------------------------------------*
096100970110     C* MEMDAT - MEMORIZZA I DATI -BOLLE-
096200970110     C*------------------------------------------------------------------------*
096300970110     C     MEMDAT        BEGSR
096400970110     C*
096500970110     C* RECUPERA L'INDICE DELLA FILIALE
096600970110     C                   Z-ADD     1             F                 4 0
096700970110     C                   SETOFF                                           98
096800970430     C     DEPFIL        LOOKUP    SORG(F)                                98
096900970110IF  1C     *IN98         IFEQ      *ON                                          *FILIALE TROVATA
097000970110     C     F             OCCUR     FPAG
097100970110     C     F             OCCUR     FCON
097200970110     C     F             OCCUR     FRIT
097300970110     C     F             OCCUR     FSGL
097400060829     C     F             OCCUR     FSGL1
097500970110     C     F             OCCUR     FCTS
097600970423     C     F             OCCUR     FTAR
097700970415     C*
097800970415     C* REPERISCE LA SOLUZIONE DEL CLIENTE
097900970415     C                   MOVEL     *BLANKS       $SOL              1
098000970415     C                   EXSR      REPSOL
098100060315     C*
098200060315     C* REPERISCE le varie di fatturazione
098300060315     C                   clear                   svn
098400060315     C                   clear                   van
098500060315     C                   EXSR      REPvrf
098600970415     C*
098700060829     C* REPERISCE IL VOLUME DELLA BOLLA
098800970415     C                   EXSR      REPVOL
098900040811     C*
099000060829     C* REPERISCE IL PESO DELLA BOLLA da
099100040811     C                   EXSR      REPpes
099200970110     C*
099300060315     C* REPERISCE LE consegne particolari
099400970110     C                   MOVEL     *BLANKS       $PCA              1
099500970110     C                   MOVEL     *BLANKS       $PCP              1
099600970110     C                   MOVEL     *BLANKS       $PCS              1
099700970110     C                   MOVEL     *BLANKS       $PCF              1
099800970415     C                   MOVEL     *BLANKS       $PCD              1
099900990812     C                   MOVEL     *BLANKS       $PIN              1            *INOLTRO
100000011026     C                   MOVEL     *ZEROS        $PINIM           21 8          *IMPORTO INOLTRO
100100011011     C                   MOVEL     *BLANKS       $CIS              1
100200011011     C                   MOVEL     *BLANKS       $RIS              1
100300040802     C                   MOVEL     *BLANKS       $ded              1
100400970110     C                   EXSR      REPVAR
100500000414     C*
100600000414     C* REPERISCE LA GIACIENZA
100700000414     C                   MOVEL     *BLANKS       $PGI              1            *ESISTE GIACENZA
100800000414     C                   Z-ADD     *ZEROS        $PGIGS            9 0          *GG SOSTA
100900000414     C                   Z-ADD     *ZEROS        $PGINR            9 0          *N° RIAPERTURE
101000021122     C                   EXSR      REPGIA
101100970110     C*
101200970110     C* REPERISCE IL C/ASSEGNO
101300011026     C                   Z-ADD     *ZEROS        $CAS             21 8
101400970110     C                   MOVEL     *BLANKS       $TPI              1
101500970110IF  2C     TASFCA        IFEQ      'S'                                          *ESISTE C/ASSEGNO
101600970110     C                   EXSR      REPCAS
101700970110E   2C                   ENDIF
101800970110     C*
101900970110     C* REPERISCE L'ASSICURAZIONE
102000011026     C                   Z-ADD     *ZEROS        $IAS             21 8
102100970110     C                   EXSR      REPASS
102200970110     C*
102300970110     C* REPERISCE IL CODICE TASSAZIONE
102400970423     C                   Z-ADD     1             J
102500970423     C                   SETOFF                                           98
102600970423     C     TASCTS        LOOKUP    CTS(J)                                 98
102700970423IF  2C     *IN98         IFEQ      *ON                                          *TROVATO
102800970416     C*---
102900970110     C*  M E M O R I Z Z A
103000970416     C*---
103100970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
103200970423     C*
103300971204IF  4C     FL1(F)        IFNE      *BLANKS                                      *CODICE ESTERO
103400970423     C     CIE(J)        OREQ      'E'
103500970423     C                   Z-ADD     1             I
103600970423     C                   SETOFF                                           98
103700970423     C     TASCTS        LOOKUP    CCTE(I)                                98
103800970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
103900140916     C                   ADD       1             TSE(I)                         *N°SPED.CTS ESTERO
104000970423E   5C                   ENDIF
104100140919     c* memorizzo nr sped. x cap e nazione
104200140916     c                   movel     tasnzd        nzdcad           12
104300140916     c                   move      tascad        nzdcad
104400140919     C                   Z-ADD     1             y
104500140919     c     i             occur     cccte
104600140919     C     nzdcad        LOOKUP    nc(y)                                  98
104700140916IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
104800140919     C                   ADD       1             cTSE(y)                        *N°SPED.CTS ESTERO
104900140919     c                   else
105000140919     C     '            'LOOKUP    nc(y)                                  98
105100140919IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
105200140919     c                   movel     nzdcad        nc(y)
105300140919     C                   z-add     1             cTSE(y)                        *N°SPED.CTS ESTERO
105400140916E   5C                   ENDIF
105500140919E   5C                   ENDIF
105600140919     C*
105700970423X   4C                   ELSE                                                   *CODICE ITALIA
105800970423     C                   Z-ADD     1             I
105900970423     C                   SETOFF                                           98
106000970423     C     TASCTS        LOOKUP    CCTI(I)                                98
106100970423IF  5C     *IN98         IFEQ      *ON                                          *TROVATO
106200970423     C                   ADD       1             TSI(I)                         *N°SPED.CTS ITALIA
106300970423E   5C                   ENDIF
106400970423E   4C                   ENDIF
106500970423     C*
106600970423     C                   ADD       1             SPE(F)                         *N°SPEDIZIONI TOTALI
106700020215     C*
106800020215IF  4C     $CLV          IFEQ      'D'
106900020215     C                   ADD       1             CLD(F)                         *CLIENTE TIPO D
107000020215E   4C                   ENDIF
107100980901IF  4C     $CLV          IFEQ      'T'
107200980901     C                   ADD       1             CLT(F)                         *CLIENTE TIPO T
107300980901E   4C                   ENDIF
107400970416IF  4C     $CLV          IFEQ      'A'
107500970110     C                   ADD       1             CLA(F)                         *CLIENTE TIPO A
107600970416E   4C                   ENDIF
107700970416IF  4C     $CLV          IFEQ      'B'
107800970110     C                   ADD       1             CLB(F)                         *CLIENTE TIPO B
107900970416E   4C                   ENDIF
108000970416IF  4C     $CLV          IFEQ      'C'
108100970415     C     $CLV          OREQ      *BLANKS
108200970110     C                   ADD       1             CLC(F)                         *CLIENTE TIPO C
108300970416E   4C                   ENDIF
108400970110     C*
108500970416IF  4C     $SOL          IFEQ      *BLANKS
108600970110     C                   ADD       1             BON(F)                         *BOLLETTAZIONE (' ')
108700970416E   4C                   ENDIF
108800970416IF  4C     $SOL          IFEQ      'A'
108900970110     C                   ADD       1             BOA(F)                         *BOLLETTAZIONE (A)
109000970416E   4C                   ENDIF
109100970416IF  4C     $SOL          IFEQ      'B'
109200970110     C                   ADD       1             BOB(F)                         *BOLLETTAZIONE (B)
109300970416E   4C                   ENDIF
109400050808     c* con il cod. trattamento merce aggancio tabella 1B per vedere se
109500050808     c* previsto packing list
109600050808     c                   z-add     1             xp                4 0
109700050808     c     tasctm        lookup    cpkl(xp)                               95
109800050808     c                   if        *in95
109900050808     c                   add       1             pks(f)
110000050808     c                   else
110100050808     c                   add       1             pkn(f)
110200050808     c                   end
110300040811     C* se oltre 20 colli sommo i colli originali altrimenti i colli
110400040811     c* della bolla
110500040811     C                   ADD       colli         NCL(F)                         *COLLI
110600040811     C                   ADD       $pes          PKB(F)                         *PESO
110700040811     C                   ADD       $vol          VLB(F)                         *VOLUME
110800060829     C                   ADD       colli1        NCL1(F)                        *COLLI
110900060829     C                   ADD       $pes1         PKB1(F)                        *PESO
111000060829     C                   ADD       $vol1         VLB1(F)                        *VOLUME
111100970110     C*
111200970110     C                   MOVEL     TASTBL        WA1               1
111300970416IF  4C     WA1           IFEQ      'F'
111400970110     C                   ADD       1             FRA(F)                         *FRANCHI
111500970416X   4C                   ELSE
111600970110     C                   ADD       1             ASS(F)                         *ASSEGNATI
111700970416E   4C                   ENDIF
111800970110     C*
111900970110     C                   MOVEL     TASGVA        WA1
112000970416IF  4C     WA1           IFEQ      'P'
112100970110     C                   ADD       1             PCV(F)                         *CONSEGNE PRIVATI
112200970416E   4C                   ENDIF
112300040802     C* bancali bartolini
112400040802IF  4C     WA1           IFEQ      'B'
112500040824     C                   ADD       1             sban(F)                        *CONSEGNE PRIVATI
112600040809     C                   ADD       §ar5nba       nba(F)                         *CONSEGNE PRIVATI
112700040809     C                   ADD       §ar5nb2       nba(F)                         *CONSEGNE PRIVATI
112800040802E   4C                   ENDIF
112900040802     C* bancali oltre 20 colli
113000040802IF  4C     WA1           IFEQ      'O'
113100040811     C                   ADD       1             sbnb(F)                        *CONSEGNE PRIVATI
113200040811     C                   ADD       §ar5bnba      nbn(F)                         *CONSEGNE PRIVATI
113300040811     C                   ADD       §ar5bnb2      nbn(F)                         *CONSEGNE PRIVATI
113400040802E   4C                   ENDIF
113500040802     C* merce di valore
113600040802     c                   movel     tasflo        dtasflo
113700040802IF  1C     §FLOTBA       IFEQ      'V'
113800040802     C                   add       1             val(F)
113900040802E   1C                   ENDIF
114000970110     C*
114100970416IF  4C     TASFDN        IFEQ      'S'
114200011011     C                   ADD       1             PCM(F)                         *MERCE A MAGAZZINO
114300970416E   4C                   ENDIF
114400970110     C*
114500970416IF  4C     TASFFD        IFEQ      'S'
114600970110     C                   ADD       1             PFD(F)                         *FERMO DEPOSITO
114700970416E   4C                   ENDIF
114800970110     C*
114900970416IF  4C     TASTSP        IFEQ      'E'
115000970110     C                   ADD       1             ESP(F)                         *ESPRESSI
115100970416E   4C                   ENDIF
115200970110     C*
115300011011IF  4C     TASFIN        IFEQ      'D'
115400011011     C                   ADD       1             PCL(F)                         *CONS. L.DISAGIATE
115500011011E   4C                   ENDIF
115600011011     C*
115700011011IF  4C     TASFIN        IFEQ      'I'
115800011011     C                   ADD       1             CIS(F)                         *CONS. ISOLE
115900011011E   4C                   ENDIF
116000011011     C*
116100011011IF  4C     TASFAP        IFEQ      'I'
116200011011     C                   ADD       1             RIS(F)                         *RITIRI ISOLE
116300011011E   4C                   ENDIF
116400011011     C*
116500011011IF  4C     TASFIN        IFEQ      'P'
116600110623IF  4C     TASFIN        oreq      'Z'
116700011011     C                   ADD       1             PPV(F)                         *CONSEGNE PROVINCIA
116800970416E   4C                   ENDIF
116900970110     C*
117000970416IF  4C     $PCA          IFEQ      'S'
117100970110     C                   ADD       1             PCA(F)                         *CONS. APPUNTAMENTO
117200970416E   4C                   ENDIF
117300970110     C*
117400970416IF  4C     $PCP          IFEQ      'S'
117500970110     C                   ADD       1             PCP(F)                         *CONS. PIANI
117600970416E   4C                   ENDIF
117700970110     C*
117800970416IF  4C     $PCS          IFEQ      'S'
117900970110     C                   ADD       1             PCS(F)                         *CONS. SUPERMERCATI
118000970416E   4C                   ENDIF
118100970110     C*
118200970416IF  4C     $PCF          IFEQ      'S'
118300970110     C                   ADD       1             PCF(F)                         *CONS. FUORI MISURA
118400970416E   4C                   ENDIF
118500040802     C* destinatario disagiato
118600040802IF  4C     $ded          IFEQ      'S'
118700040802     C                   ADD       1             ded(F)                         *CONS. FUORI MISURA
118800040802E   4C                   ENDIF
118900970415     C*
119000970416IF  4C     $PCD          IFEQ      'S'
119100970415     C                   ADD       1             PCD(F)                         *CONS. DDT
119200970416E   4C                   ENDIF
119300970110     C*
119400970416IF  4C     $GGP          IFGT      *ZEROS
119500970110     C                   Z-ADD     1             I
119600970110     C                   SETOFF                                           98
119700970110     C     $GGP          LOOKUP    GGP(I)                                 98
119800970416IF  5C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
119900970110     C                   ADD       1             SGP(I)                         *SPED. GG PAGAMENTO
120000970416X   5C                   ELSE                                                   *MAI MEMORIZZATI
120100970110     C                   Z-ADD     1             I
120200970416DO  6C     I             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
120300970416IF  7C     GGP(I)        IFEQ      *ZEROS                                       *DISPONIBILE
120400970110     C                   Z-ADD     $GGP          GGP(I)                         *GG
120500970110     C                   Z-ADD     1             SGP(I)                         *SPED. GG PAGAMENTO
120600981014     C                   LEAVE                                                  *ESCE DAL CICLO
120700970416E   7C                   ENDIF
120800970416E   6C                   ENDDO
120900970416E   5C                   ENDIF
121000970416E   4C                   ENDIF
121100000411     C*
121200000411     C                   MOVEL     TASTBL        WA1               1
121300000411IF  4C     WA1           IFEQ      'F'                                          *SOLO FRANCHI
121400000411IF  5C     TASSTR        IFGT      *ZEROS
121500990812     C     TASSTR        ANDNE     99999
121600000411IF  6C     TASSTR        IFGT      999                                          *CAMPO 5,0 -> 3,0
121700990812     C                   Z-ADD     999           TASSTR
121800000411E   6C                   ENDIF
121900970110     C                   Z-ADD     TASSTR        I                              *STOP RITIRO
122000970110     C                   ADD       1             RIT(I)                         *SPEDIZIONI STOP
122100000411E   5C                   ENDIF
122200000411     C*
122300000411X   4C                   ELSE                                                   *SOLO ASSEGNATI
122400000411     C*
122500000411IF  5C     TASSTP        IFGT      *ZEROS
122600990812     C     TASSTP        ANDNE     99999
122700000411IF  6C     TASSTP        IFGT      999                                          *CAMPO 5,0 -> 3,0
122800990812     C                   Z-ADD     999           TASSTP
122900000411E   6C                   ENDIF
123000970110     C                   Z-ADD     TASSTP        I                              *STOP CONSEGNA
123100970110     C                   ADD       1             CON(I)                         *SPEDIZIONI STOP
123200000411E   5C                   ENDIF
123300000411E   4C                   ENDIF
123400970110     C*
123500060829     c* per ricavi
123600970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
123700970110     C     PARTTR        OREQ      '3'                                          *KG
123800040811     C                   Z-ADD     $pes          TASSGL           13 3          *SCAGLIONE IN KG
123900970416X   4C                   ELSE
124000040811     C                   Z-ADD     colli         TASSGL                         *SCAGLIONE IN COLLI
124100040811E   4C                   ENDIF
124200971024IF  4C     TASSGL        IFGT      *ZEROS
124300971024DO  5C     1             DO        10            I
124400971024IF  6C     TASSGL        IFLE      SSGL(I)
124500970110     C                   ADD       1             SSP(I)                         *N°SPED SCAGLIONE
124600040811     C                   ADD       $pes          PSC(I)                         *PESO
124700040811     C                   ADD       colli         CSC(I)                         *COLLI
124800970415     C                   ADD       $VOL          VSC(I)                         *VOLUME
124900970110     C                   LEAVE
125000971024E   6C                   ENDIF
125100971024E   5C                   ENDDO
125200040920     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
125300060829IF  5C     PARCTO        IFEQ      'T'
125400040920     C     PARCAR        ANDEQ     88888
125500040920     C     PARLNP        ANDEQ     999
125600040920     C                   EXSR      SRSGL8
125700060829E   5C                   ENDIF
125800971024E   4C                   ENDIF
125900060829     c* per competenze
126000060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
126100060829     C     PARTTR        OREQ      '3'                                          *KG
126200060829     C                   Z-ADD     $pes1         TASSGL           13 3          *SCAGLIONE IN KG
126300060829X   4C                   ELSE
126400060829     C                   Z-ADD     colli1        TASSGL                         *SCAGLIONE IN COLLI
126500060829E   4C                   ENDIF
126600060829IF  4C     TASSGL        IFGT      *ZEROS
126700060829DO  5C     1             DO        10            I
126800060829IF  6C     TASSGL        IFLE      SSGL(I)
126900060829     C                   ADD       1             SSP1(I)                        *N°SPED SCAGLIONE
127000060829     C                   ADD       $pes1         PSC1(I)                        *PESO
127100060829     C                   ADD       colli1        CSC1(I)                        *COLLI
127200060829     C                   ADD       $VOL1         VSC1(I)                        *VOLUME
127300060829     C                   LEAVE
127400060829E   6C                   ENDIF
127500060829E   5C                   ENDDO
127600060829     C* CARISCO LE SCHIERE ANCHE PER CARTELLO LNP = 888 E 889
127700060829IF  5C     PARCTO        IFEQ      'T'
127800060829     C     PARCAR        ANDEQ     88888
127900060829     C     PARLNP        ANDEQ     999
128000060829     C                   EXSR      SRSGL81
128100060829E   5C                   ENDIF
128200060829E   4C                   ENDIF
128300971024E   3C                   ENDIF
128400970110     C*
128500970416IF  3C     DEPSP         IFEQ      'S'                                          *BOLLA VALIDA SPED
128600970416     C     DEPRI         OREQ      'S'                                          *BOLLA VALIDA RICAVI
128700970416IF  4C     $CAS          IFGT      *ZEROS
128800970110     C                   ADD       $CAS          CAS(F)                         *C/ASSEGNO
128900970416IF  5C     $TPI          IFEQ      'M'
129000970110     C                   ADD       1             PAM(F)                         *MITTENTE
129100970416X   5C                   ELSE
129200970110     C                   ADD       1             PAB(F)                         *BARTOLINI
129300970416E   5C                   ENDIF
129400970416E   4C                   ENDIF
129500970110     C*
129600970416IF  4C     $IAS          IFGT      *ZEROS
129700970110     C                   ADD       $IAS          ASC(F)                         *ASSICURAZIONE
129800970110     C                   ADD       1             PAC(F)                         *ASSICURAZIONE CONTO
129900970416X   4C                   ELSE
130000060315     c                   if        flgacbase = *on
130100060315     C                   ADD       1             xx1(F)                         *ASSICURAZIONE R.C.V
130200060315     c                   else
130300970110     C                   ADD       1             PAR(F)                         *ASSICURAZIONE R.C.V
130400970416E   4C                   ENDIF
130500060315E   4C                   ENDIF
130600970110     C*
130700000414IF  4C     $PGI          IFEQ      'S'
130800970110     C                   ADD       1             PGI(F)                         *GIACENZE
130900000414     C                   ADD       $PGIGS        GGS(F)                         *GG SOSTA GIACENZA
131000000414     C                   ADD       $PGINR        NRG(F)                         *N° RIAPERTURE GIACE
131100970416E   4C                   ENDIF
131200970416E   3C                   ENDIF
131300970110     C*
131400970416X   2C                   ELSE                                                   *CON.TASS.NON TROVAT
131500970416     C                   MOVEL     MTAS(2)       DEPERR           50
131600970110     C                   MOVEL     'T'           DEPTER            1
131700970110     C                   EXSR      STAERR
131800970416E   2C                   ENDIF
131900970110     C*
132000970110X   1C                   ELSE                                                   *FILIALE NON TROVATA
132100970110     C                   MOVEL     MTAS(1)       DEPERR
132200970110     C                   MOVEL     'T'           DEPTER
132300970110     C                   EXSR      STAERR
132400970110E   1C                   ENDIF
132500970110     C*
132600970110     C                   ENDSR
132700970415     C*------------------------------------------------------------------------*
132800040809     C* fiar5 tipo record BAN
132900970415     C*------------------------------------------------------------------------*
133000040809     C     srfiar5       BEGSR
133100040809     c*
133200040809     c                   z-add     tasaas        ar5aas
133300040809     c                   z-add     taslnp        ar5lnp
133400040809     c                   z-add     tasnrs        ar5nrs
133500040809     c                   z-add     tasnsp        ar5nsp
133600040811     c                   clear                   ar5uni
133700040809     c     kar5          chain     fiar531c
133800040809     C*
133900040809     C                   ENDSR
134000040809     C*------------------------------------------------------------------------*
134100040809     C* REPSOL - REPERISCE LA SOLUZIONE DI INVIO DATI -BOLLETTAZIONE-
134200040809     C*------------------------------------------------------------------------*
134300040809     C     REPSOL        BEGSR
134400970415     C*---
134500970415     C* NON E' PRESENTE IN TABELLA -> NESSUNA PROCEDURA            ( )
134600970415     C* NON HA IL NUMERO DI SERIE  -> STAMPA LE BOLLE              (A)
134700970415     C* HA     IL NUMERO DI SERIE  -> STAMPA LE BOLLE + SEGNACOLLI (B)
134800040310     C*--- escludo bolle recapito contrassegni
134900970415IF  1C     TASNRS        IFGT      *ZEROS
135000040310IF  1C     TAScbo        andne     'FW'
135100970415     C                   MOVEL     'B'           $SOL                           *PROCEDURA 'B'
135200970415X   1C                   ELSE
135300100303     C                   Z-ADD     1             KTBKUT
135400100303     C                   MOVEL     '3C'          KTBCOD
135500100303     c                   movel(p)  tasksc        ktbkey
135600100303     c     keytab        chain     tabel00f
135700100303     c                   if        %found(tabel00f) and tblflg = ' '
135800100303     C                   MOVEL     TBLUNI        DS3C
135900100303IF  3C     §3COKD        IFEQ      '*'
136000970415     C                   MOVEL     'A'           $SOL                           *PROCEDURA 'A'
136100970415X   3C                   ELSE
136200970415     C                   MOVEL     *BLANKS       $SOL                           *PROCEDURA NO AVVIAT
136300970415E   3C                   ENDIF
136400970415E   2C                   ENDIF
136500970415E   1C                   ENDIF
136600970415     C*
136700970415     C                   ENDSR
136800970415     C*------------------------------------------------------------------------*
136900970415     C* REPVOL - REPERISCE IL VOLUME DELLA BOLLA
137000970415     C*------------------------------------------------------------------------*
137100970415     C     REPVOL        BEGSR
137200060829     C* colli x ricavi
137300060829     C                   Z-ADD     *ZEROS        $VOL              5 3
137400970415     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
137500040811    1C     TASVLC        IFGT      *ZEROS
137600040811     C     colli         ANDEQ     TASNCR
137700970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
137800970415E   1C                   ENDIF
137900970415     C*
138000970415     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
138100970415     C     TASVLC        IFGT      TASVLF
138200040811     C     colli         ANDNE     TASNCR
138300970415     C                   Z-ADD     TASVLC        $VOL                           *VOLUME CML
138400970415E   1C                   ENDIF
138500970415     C*
138600970415     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
138700970415IF  1C     $VOL          IFEQ      *ZEROS
138800970415     C                   Z-ADD     TASVLF        $VOL                           *VOLUME NORMALE
138900060829E   1C                   ENDIF
139000060829c*   c*colli x competetnze
139100060829     C                   Z-ADD     *ZEROS        $VOL1             5 3
139200060829     C* SE C'E' VOLUME CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
139300060829    1C     TASVLC        IFGT      *ZEROS
139400060829     C     colli1        ANDEQ     TASNCR
139500060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
139600060829E   1C                   ENDIF
139700060829     C*
139800060829     C* SE VOLUME CML MAGGIORE VOLUME NORMALE, PRENDE CML
139900060829     C     TASVLC        IFGT      TASVLF
140000060829     C     colli1        ANDNE     TASNCR
140100060829     C                   Z-ADD     TASVLC        $VOL1                          *VOLUME CML
140200060829E   1C                   ENDIF
140300060829     C*
140400060829     C* SE VOLUME CML NON E' VALIDO PRENDO VOLUME NORMALE
140500060829IF  1C     $VOL1         IFEQ      *ZEROS
140600060829     C                   Z-ADD     TASVLF        $VOL1                          *VOLUME NORMALE
140700060829E   1C                   ENDIF
140800970415     C*
140900000414     C                   ENDSR
141000040811     C*------------------------------------------------------------------------*
141100040811     C* REPpes - REPERISCE IL peso DELLA BOLLA
141200040811     C*------------------------------------------------------------------------*
141300040811     C     REPpes        BEGSR
141400060829     C* colli x ricavi
141500060829     C                   Z-ADD     *ZEROS        $pes
141600040811     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
141700040811    1C     TASpkC        IFGT      *ZEROS
141800040811     C     colli         ANDEQ     TASNCp
141900040811     C                   Z-ADD     TASpkc        $pes                           *VOLUME CML
142000040811E   1C                   ENDIF
142100040811     C*
142200040811     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
142300040811     C     TASpkc        IFGT      TASpkF
142400040811     C     colli         ANDNE     TASNCp
142500040811     C                   Z-ADD     TASpkC        $pes                           *VOLUME CML
142600040811E   1C                   ENDIF
142700040811     C*
142800040811     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
142900040811IF  1C     $pes          IFEQ      *ZEROS
143000040811     C                   Z-ADD     TASpkF        $pes                           *VOLUME NORMALE
143100040811E   1C                   ENDIF
143200060829     C* colli x competenze
143300060829     C                   Z-ADD     *ZEROS        $pes1
143400060829     C* SE C'E' peso CML E COLLI = COLLI RILEVATI DAL MACCHINONE, PRENDE CML
143500060829    1C     TASpkC        IFGT      *ZEROS
143600060829     C     colli1        ANDEQ     TASNCp
143700060829     C                   Z-ADD     TASpkc        $pes1                          *VOLUME CML
143800060829E   1C                   ENDIF
143900060829     C*
144000060829     C* SE peso CML MAGGIORE peso NORMALE, PRENDE CML
144100060829     C     TASpkc        IFGT      TASpkF
144200060829     C     colli1        ANDNE     TASNCp
144300060829     C                   Z-ADD     TASpkC        $pes1                          *VOLUME CML
144400060829E   1C                   ENDIF
144500060829     C*
144600060829     C* SE peso CML NON E' VALIDO PRENDO peso NORMALE
144700060829IF  1C     $pes1         IFEQ      *ZEROS
144800060829     C                   Z-ADD     TASpkF        $pes1                          *VOLUME NORMALE
144900060829E   1C                   ENDIF
145000040811     C*
145100040811     C                   ENDSR
145200970110     C*------------------------------------------------------------------------*
145300970110     C* REPCAS - REPERISCE IL CONTRASSEGNO
145400970110     C*------------------------------------------------------------------------*
145500970110     C     REPCAS        BEGSR
145600970110     C*
145700970110     C                   Z-ADD     TASAAS        KSBAAS                         *ANNO
145800970110     C                   Z-ADD     TASLNP        KSBLNP                         *LINEA PARTENZA
145900970110     C                   Z-ADD     TASNRS        KSBNRS                         *SERIE
146000970110     C                   Z-ADD     TASNSP        KSBNSP                         *SPEDIZIONE
146100970110     C     KEYCSB        CHAIN     TNCSB03L                           98
146200970110IF  1C     *IN98         IFEQ      *OFF                                         *TROVATO
146300970110IF  2C     CSBSTA        IFNE      9                                            *NO ANNULLATO
146400970110     C                   MOVEL     CSBTPI        $TPI                           *TIPO INCASSO
146500011011     C                   Z-ADD     CSBCAS        $CAS                           *C/ASSEGNO
146600011011     C     CSBVCA        IFNE      PARDIV
146700011011     C                   Z-ADD     $CAS          WIMI             30 9
146800011011     C                   MOVEL     CSBVCA        WDVI              3
146900011011     C                   EXSR      CNVEUR
147000011011     C                   Z-ADD     WIMO          $CAS
147100011011     C                   ENDIF
147200970110E   2C                   ENDIF
147300970110E   1C                   ENDIF
147400970110     C*
147500970110     C                   ENDSR
147600021122     C*------------------------------------------------------------------------*
147700021122     C* REPGIA - REPERISCE LE GIACENZE
147800021122     C*------------------------------------------------------------------------*
147900021122     C     REPGIA        BEGSR
148000021122     C*
148100021122     C* AZZERA LE VARIABILI DI LAVORO
148200021122     C                   Z-ADD     *ZEROS        §GGSOS            3 0
148300021122     C*
148400021122     C* LEGGE TUTTE LE GIACENZE DELLA BOLLA
148500021122     C                   Z-ADD     TASAAS        KGPAAS
148600021122     C                   Z-ADD     TASLNP        KGPLNP
148700021122     C                   Z-ADD     TASNRS        KGPNRS
148800021122     C                   Z-ADD     TASNSP        KGPNSP
148900050309     C     K01GCP        SETLL     TIGCP51L
149000050309     C     K01GCP        READE     TIGCP51L                               98
149100021122DO  1C     *IN98         DOWEQ     *OFF
149200021122IF  2C     GCPATB        IFEQ      *BLANKS
149300021122     C                   MOVEL     'S'           $PGI                           *ESISTE GIACENZA
149400021122     C* APERTURE
149500021122     C                   ADD       1             $PGINR                         *NUMERO RIAPERTURE
149600021122     C* GIORNI DI SOSTA
149700021122IF  3C     GCPDDM        IFGT      *ZEROS                                       *IMMESSE DISPOSIZION
149800021122IF  4C     GCPDUR        IFGT      *ZEROS                                       *ULTIMA RIAPERTURA
149900021122     C                   Z-ADD     GCPDUR        G08INV
150000021122X   4C                   ELSE                                                   *APERTURA GIACENZA
150100021122     C                   MOVEL     GCPAGC        G08INV
150200021122     C                   MOVE      GCPMGC        G08INV
150300021122E   4C                   ENDIF
150400021122     C                   Z-ADD     *ZEROS        G08DAT
150500021122     C                   MOVEL     '3'           G08ERR
150600021122     C                   CALL      'XSRDA8'
150700021122     C                   PARM                    WLBDA8
150800021122     C                   Z-ADD     G08TGI        §GGDA             5 0
150900021122IF  4C     GCPDED        IFGT      *ZEROS                                       *DISPOSIZ.GIACENZA
151000021122     C                   Z-ADD     GCPDED        G08INV
151100021122X   4C                   ELSE                                                   *DISPOSIZ.MITTENTE
151200021122     C                   Z-ADD     GCPDDM        G08INV
151300021122E   4C                   ENDIF
151400021122     C                   Z-ADD     *ZEROS        G08DAT
151500021122     C                   MOVEL     '3'           G08ERR
151600021122     C                   CALL      'XSRDA8'
151700021122     C                   PARM                    WLBDA8
151800021122     C                   Z-ADD     G08TGI        §GGA              5 0
151900021122     C* GIORNI DI SOSTA
152000021122     C     §GGA          SUB       §GGDA         §GGXX             3 0
152100021122     C                   ADD       1             §GGXX
152200021122     C                   ADD       §GGXX         §GGSOS
152300021122E   3C                   ENDIF
152400021122E   2C                   ENDIF
152500050309     C     K01GCP        READE     TIGCP51L                               98
152600021122E   1C                   ENDDO
152700021122     C*
152800021122     C                   Z-ADD     §GGSOS        $PGIGS                         *GIORNI DI SOSTA
152900021122     C*
153000021122     C                   ENDSR
153100970110     C*------------------------------------------------------------------------*
153200970110     C* REPASS - REPERISCE L'ASSICURAZIONE
153300970110     C*------------------------------------------------------------------------*
153400970110     C     REPASS        BEGSR
153500060315     c                   clear                   flgacbase         1
153600970110     C*
153700970110IF  1C     TASIAS        IFGT      *ZEROS
153800011011     C                   Z-ADD     TASIAS        $IAS
153900011011IF  2C     TASVAS        IFNE      PARDIV                                       *DIVISA ESTERA
154000011011     C                   Z-ADD     $IAS          WIMI             30 9
154100011011     C                   MOVEL     TASVAS        WDVI              3
154200011011     C                   EXSR      CNVEUR
154300011011     C                   Z-ADD     WIMO          $IAS
154400011011     C                   ENDIF
154500060315     c* verifico se esiste ac base
154600060315     c                   else
154700060315     c                   z-add     1             v
154800060315     c     'd'           lookup    svn(v)                                 99
154900060320     c                   if        van(v) <> 0 and *in99
155000060315     c                   eval      flgacbase = *on
155100970110E   1C                   ENDIF
155200060315E   1C                   ENDIF
155300970110     C*
155400970110     C                   ENDSR
155500970110     C*------------------------------------------------------------------------*
155600060315     C* REPVrf - REPERISCE le varie di fatturazione
155700990812     C* -----------------------------------------------------------------------*
155800060315     C     REPVrf        BEGSR
155900060315     C* CARICO LE VARIE BOLLA ORIGINALE DA TITAS
156000060315     C                   Z-ADD     *ZEROS        V                 6 0
156100060315     C     tasSV1        IFNE      *BLANKS
156200060315     C                   ADD       1             V
156300060315     C                   MOVEL     tasSV1        SVN(V)
156400060315     C                   Z-ADD     tasVA1        VAN(V)
156500060315     C                   ENDIF
156600060315     C     tasSV2        IFNE      *BLANKS
156700060315     C                   ADD       1             V
156800060315     C                   MOVEL     tasSV2        SVN(V)
156900060315     C                   Z-ADD     tasVA2        VAN(V)
157000060315     C                   ENDIF
157100060315     C     tasSV3        IFNE      *BLANKS
157200060315     C                   ADD       1             V
157300060315     C                   MOVEL     tasSV3        SVN(V)
157400060315     C                   Z-ADD     tasVA3        VAN(V)
157500060315     C                   ENDIF
157600060315     C* CARICO LE VARIE BOLLA DA TITA7 (SOLO SE C'E' LA VARIA 3)
157700060315     C     tasSV3        IFNE      *BLANKS
157800060315     C                   Z-ADD     tasAAS        TA7AAS
157900060315     C                   Z-ADD     tasLNP        TA7LNP
158000060315     C                   Z-ADD     tasNRS        TA7NRS
158100060315     C                   Z-ADD     tasNSP        TA7NSP
158200060315     C                   MOVEL     tasTBL        TA7TBL
158300060315     C     KEYTA7        SETLL     TITA730C
158400060315     c                   do        *hival
158500060315     C     KEYTA7        READE     TITA730C
158600060315     c                   if        %eof(tita730c)
158700060315     c                   leave
158800060315     c                   end
158900060315if  1C                   IF        (TA7VAN = 88888888,000)
159000060315     C                   Z-ADD     *zeros        TA7VAN
159100060315e   1c                   ENDIF
159200060315     C                   ADD       1             V
159300060315     C                   MOVEL     TA7SVN        SVN(V)
159400060315     C                   Z-ADD     TA7VAN        VAN(V)
159500060315     C                   ENDDO
159600060315e   1c                   ENDIF
159700060315     c                   endsr
159800060315     C*------------------------------------------------------------------------*
159900060315     C* REPVAR - REPERISCE LE consegne particolari
160000060315     C* -----------------------------------------------------------------------*
160100060315     C     REPVAR        BEGSR
160200970110     C*
160300970110     C* VARIE -BOLLA-
160400021122IF  1C     TASFTC        IFEQ      'A'
160500021122     C     TASTC2        OREQ      'A'
160600970110     C                   MOVEL     'S'           $PCA                           *CONS APPUNTAMENTO
160700970110E   1C                   ENDIF
160800021122IF  1C     TASFTC        IFEQ      'P'
160900021122     C     TASTC2        OREQ      'P'
161000970110     C                   MOVEL     'S'           $PCP                           *CONS PIANI
161100970110E   1C                   ENDIF
161200021122IF  1C     TASFTC        IFEQ      'S'
161300021122     C     TASTC2        OREQ      'S'
161400970110     C                   MOVEL     'S'           $PCS                           *CONS SUPERMERCATI
161500970110E   1C                   ENDIF
161600021122IF  1C     TASFTC        IFEQ      'F'
161700021122     C     TASTC2        OREQ      'F'
161800970110     C                   MOVEL     'S'           $PCF                           *CONS FUORI MISURA
161900970110E   1C                   ENDIF
162000040802IF  1C     TASFTC        IFEQ      'X'
162100040802     C     TASTC2        OREQ      'X'
162200040802     C                   MOVEL     'S'           $ded                           *CONS FUORI MISURA
162300040802E   1C                   ENDIF
162400021122IF  1C     TASLL1        IFEQ      'S'
162500970415     C                   MOVEL     'S'           $PCD                           *CONS DDT
162600970415E   1C                   ENDIF
162700970110     C*
162800970110     C                   ENDSR
162900980819     C*------------------------------------------------------------------------*
163000980819     C* AGGETX - AGGIORNA I DATI TIPO
163100980819     C*------------------------------------------------------------------------*
163200980819     C     AGGETX        BEGSR
163300980819     C*
163400980819     C* PER OGNI LINEA DI PARTENZA RICHIESTA
163500980819DO  1C     1             DO        500           I
163600980819IF  2C     SORG(I)       IFGT      *ZEROS
163700980819IF  3C     SPE(I)        IFGT      *ZEROS                                       *MEMORIZZATO QC.
163800980821     C                   Z-ADD     SORG(I)       XORG
163900140919     C                   movel     Sntw(I)       Xntw
164000980819     C     I             OCCUR     FPAG
164100980819     C     I             OCCUR     FCON
164200980819     C     I             OCCUR     FRIT
164300980819     C     I             OCCUR     FSGL
164400060829     C     I             OCCUR     FSGL1
164500980819     C     I             OCCUR     FCTS
164600980819     C     I             OCCUR     FTAR                                         *DATI TARIFFA
164700980819     C*---
164800980819     C* DATI TIPO -GENERALI-
164900980819     C*---
165000980821     C                   EXSR      SCRETC
165100980819     C*---
165200980819     C* DATI TIPO -SCAGLIONI-
165300980819     C*---
165400060829     c* ricavi
165500980821     C                   EXSR      SCRETS
165600060829     c* competenze
165700060829     c                   if        etcbnb <> 0
165800060829     c                   exsr      screts1
165900060829     c                   end
166000980819     C*---
166100980819     C* DATI TIPO -CODICI TASSAZIONE-
166200980819     C*---
166300980819     C* PER OGNI TIPO TARIFFA (ITALIA/ESTERO)
166400980819DO  4C     1             DO        2             K
166500980819IF  5C     SFIE(K)       IFNE      *BLANKS
166600980821     C                   MOVEL     SFIE(K)       XFIE
166700980821     C                   EXSR      SCRETT
166800981215E   5C                   ENDIF
166900980819E   4C                   ENDDO                                                  *FINE TARIFFA ITA/ES
167000980819     C*
167100980819E   3C                   ENDIF
167200980819E   2C                   ENDIF
167300980819E   1C                   ENDDO                                                  *FINE FILIALI
167400980819     C*
167500980821     C                   ENDSR
167600980819     C*------------------------------------------------------------------------*
167700980819     C* AGGET8 - AGGIORNA I DATI TIPO DI CARTELLO
167800980819     C*------------------------------------------------------------------------*
167900980819     C     AGGET8        BEGSR
168000970110     C*
168100980819     C* PER OGNI LINEA DI PARTENZA MEMORIZZATA
168200971124DO  1C     1             DO        500           I
168300980820     C     I             OCCUR     FPAG
168400970110     C     I             OCCUR     FCON
168500970110     C     I             OCCUR     FRIT
168600970110     C     I             OCCUR     FSGL
168700060829     C     I             OCCUR     FSGL1
168800970110     C     I             OCCUR     FCTS
168900980819     C     I             OCCUR     FTAR
169000980819     C*
169100980819     C* SOMMA I DATI PER LE TARIFFE DI CARTELLO -ITA/EST- DI TUTTE LE FILIALI
169200980819IF  2C     SFL1(I)       IFEQ      *BLANKS                                      *FILIALE ITALIA
169300980824     C                   Z-ADD     1             U                 4 0
169400980819X   2C                   ELSE                                                   *FILIALE ESTERO
169500980819     C                   Z-ADD     2             U
169600980819E   2C                   ENDIF
169700980824     C     U             OCCUR     CCAR
169800980824     C     U             OCCUR     CPAG
169900980824     C     U             OCCUR     CCON
170000980824     C     U             OCCUR     CRIT
170100980824     C     U             OCCUR     CSGL
170200060829     C     U             OCCUR     CSGL1
170300980824     C     U             OCCUR     CCTS
170400980819     C*
170500980819     C* DATI TIPO -GENERALI-
170600980821     C                   ADD       SPE(I)        USPE(1)
170700980821     C                   ADD       CLA(I)        UCLA(1)
170800980821     C                   ADD       CLB(I)        UCLB(1)
170900980821     C                   ADD       CLC(I)        UCLC(1)
171000980821     C                   ADD       BON(I)        UBON(1)
171100980821     C                   ADD       BOA(I)        UBOA(1)
171200980821     C                   ADD       BOB(I)        UBOB(1)
171300980821     C                   ADD       NCL(I)        UNCL(1)
171400980821     C                   ADD       PKB(I)        UPKB(1)
171500980821     C                   ADD       VLB(I)        UVLB(1)
171600060829     C                   ADD       NCL1(I)       UNCL1(1)
171700060829     C                   ADD       PKB1(I)       UPKB1(1)
171800060829     C                   ADD       VLB1(I)       UVLB1(1)
171900980821     C                   ADD       FRA(I)        UFRA(1)
172000980821     C                   ADD       ASS(I)        UASS(1)
172100980821     C                   ADD       PCV(I)        UPCV(1)
172200980821     C                   ADD       PCM(I)        UPCM(1)
172300980821     C                   ADD       PFD(I)        UPFD(1)
172400980821     C                   ADD       ESP(I)        UESP(1)
172500980821     C                   ADD       PPV(I)        UPPV(1)
172600980821     C                   ADD       PCA(I)        UPCA(1)
172700980821     C                   ADD       PCP(I)        UPCP(1)
172800980821     C                   ADD       PCS(I)        UPCS(1)
172900980821     C                   ADD       PCF(I)        UPCF(1)
173000980821     C                   ADD       PCD(I)        UPCD(1)
173100980821     C                   ADD       PCC(I)        UPCC(1)
173200980821     C                   ADD       PCL(I)        UPCL(1)
173300980821     C                   ADD       PAB(I)        UPAB(1)
173400980821     C                   ADD       PAM(I)        UPAM(1)
173500980821     C                   ADD       CAS(I)        UCAS(1)
173600980821     C                   ADD       PAR(I)        UPAR(1)
173700980821     C                   ADD       PAC(I)        UPAC(1)
173800060315     C                   ADD       xx1(I)        Uxx1(1)
173900980821     C                   ADD       ASC(I)        UASC(1)
174000980821     C                   ADD       PGI(I)        UPGI(1)
174100000414     C                   ADD       GGS(I)        UGGS(1)
174200000414     C                   ADD       NRG(I)        UNRG(1)
174300980901     C                   ADD       CLT(I)        UCLT(1)
174400011011     C                   ADD       CIS(I)        UCIS(1)
174500011011     C                   ADD       RIS(I)        URIS(1)
174600020215     C                   ADD       CLD(I)        UCLD(1)
174700050808     C                   ADD       pks(I)        Upks(1)
174800050808     C                   ADD       pkn(I)        Upkn(1)
174900040802     C                   ADD       ded(I)        Uded(1)
175000040811     C                   ADD       sbnb(I)       Usbnb(1)
175100040824     C                   ADD       sban(I)       Usban(1)
175200040811     C                   ADD       nbn(I)        Unbn(1)
175300040802     C                   ADD       nba(I)        Unba(1)
175400040802     C                   ADD       val(I)        Uval(1)
175500980825DO  2C     1             DO        30            J
175600980825IF  3C     GGP(J)        IFGT      *ZEROS
175700980825     C                   Z-ADD     1             A                 4 0
175800980825     C                   SETOFF                                           98
175900980825     C     GGP(J)        LOOKUP    UGGP(A)                                98
176000980825IF  4C     *IN98         IFEQ      *ON                                          *GIA'MEMORIZZATI
176100980825     C                   ADD       SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
176200980825X   4C                   ELSE                                                   *MAI MEMORIZZATI
176300980825     C                   Z-ADD     1             A
176400980825DO  5C     A             DOWLE     30                                           *AGGIUNGE NEL 1°BUCO
176500980825IF  6C     UGGP(A)       IFEQ      *ZEROS                                       *DISPONIBILE
176600980825     C                   Z-ADD     GGP(J)        UGGP(A)                        *GG
176700980825     C                   Z-ADD     SGP(J)        USGP(A)                        *SPED. GG PAGAMENTO
176800980825     C                   Z-ADD     30            A                              *ESCE DAL CICLO
176900980825E   6C                   ENDIF
177000980825     C                   ADD       1             A
177100980825E   5C                   ENDDO
177200980825E   4C                   ENDIF
177300980825E   3C                   ENDIF
177400980825E   2C                   ENDDO
177500980819DO  2C     1             DO        999           J
177600980819     C                   ADD       RIT(J)        URIT(J)
177700980819     C                   ADD       CON(J)        UCON(J)
177800980819E   2C                   ENDDO
177900980819     C*
178000040920     C* DATI TIPO -SCAGLIONI- (già caricati nella durante la lettura di
178100040920     c* titas)
178200980820     C*
178300970110     C* DATI TIPO -CODICI TASSAZIONE-
178400050103DO  2C     1             DO        600           J
178500980819     C                   ADD       TSI(J)        UTSI(J)
178600980819     C                   ADD       TSE(J)        UTSE(J)
178700980819E   2C                   ENDDO
178800970110E   1C                   ENDDO                                                  *FINE FILIALI
178900980821     C*---
179000980821     C* SCRIVE LA TARIFFA -888- ITALIA
179100980821     C*---
179200980820     C*
179300980821     C* CARICA LE SCHIERE DA ELABORARE
179400980821     C                   Z-ADD     1             U
179500980821     C                   Z-ADD     1             I
179600980821     C                   Z-ADD     888           XORG
179700140919     c                   clear                   xntw
179800980821     C                   EXSR      CARSCH
179900980821     C*
180000980821     C* DATI TIPO -GENERALI-
180100980821     C                   EXSR      SCRETC
180200980821     C*
180300980821     C* DATI TIPO -SCAGLIONI-
180400060829     c* ricavi
180500980821     C                   EXSR      SCRETS
180600060829     c* competenze
180700060829     c                   if        etcbnb <> 0
180800060829     C                   EXSR      SCRETS1
180900060829     c                   end
181000980821     C*
181100980821     C* DATI TIPO -CODICI TASSAZIONE-
181200980824     C                   MOVEL     'I'           XFIE
181300980821     C                   EXSR      SCRETT
181400980824     C*---
181500980824     C* SCRIVE LA TARIFFA -889- ESTERO
181600980824     C*---
181700980824     C*
181800980824     C* CARICA LE SCHIERE DA ELABORARE
181900980824     C                   Z-ADD     2             U
182000980824     C                   Z-ADD     1             I
182100980824     C                   Z-ADD     889           XORG
182200140919     c                   clear                   xntw
182300980824     C                   EXSR      CARSCH
182400980824     C*
182500980824     C* DATI TIPO -GENERALI-
182600980824     C                   EXSR      SCRETC
182700980824     C*
182800980824     C* DATI TIPO -SCAGLIONI-
182900060829     c* ricavi
183000980824     C                   EXSR      SCRETS
183100060829     c* competenze
183200060829     c                   if        etcbnb <> 0
183300060829     C                   EXSR      SCRETS1
183400060829     c                   end
183500980824     C*
183600980824     C* DATI TIPO -CODICI TASSAZIONE-
183700980824     C                   MOVEL     'E'           XFIE
183800980824     C                   EXSR      SCRETT
183900970110     C*
184000980819     C                   ENDSR
184100040920     C*------------------------------------------------------------------------*
184200040920     C* SRSGL8 - CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
184300040920     C*------------------------------------------------------------------------*
184400040920     C     SRSGL8        BEGSR
184500040920     C* 888 ITALIA
184600040920     C                   IF        SFL1(F) = ' '
184700040920     C                   Z-ADD     1             U
184800040920     C                   ELSE
184900040920     C                   Z-ADD     2             U
185000040920     C                   END
185100040920     C     U             OCCUR     CSGL
185200040920     C*
185300040920DO  5C     1             DO        10            I
185400040920IF  6C     TASSGL        IFLE      SGL(I)
185500040920     C                   ADD       1             USSP(I)                        *N°SPED SCAGLIONE
185600040920     C                   ADD       TASPKF        UPSC(I)                        *PESO
185700040920     C                   ADD       TASNCL        UCSC(I)                        *COLLI
185800040920     C                   ADD       $VOL          UVSC(I)                        *VOLUME
185900040920     C                   LEAVE
186000040920E   6C                   ENDIF
186100040920E   5C                   ENDDO
186200040920     C*
186300040920     C                   ENDSR
186400060829     C*------------------------------------------------------------------------*
186500060829     C* SRSGL81- CARICA LE SCHIERE DEGLI SCAGLIONI PER CARTELLO 888 E 889
186600060829     C*------------------------------------------------------------------------*
186700060829     C     SRSGL81       BEGSR
186800060829     C* 888 ITALIA
186900060829     C                   IF        SFL1(F) = ' '
187000060829     C                   Z-ADD     1             U
187100060829     C                   ELSE
187200060829     C                   Z-ADD     2             U
187300060829     C                   END
187400060829     C     U             OCCUR     CSGL1
187500060829     C*
187600060829DO  5C     1             DO        10            I
187700060829IF  6C     TASSGL        IFLE      SGL(I)
187800060829     C                   ADD       1             USSP1(I)                       *N°SPED SCAGLIONE
187900060829     C                   ADD       TASPKF        UPSC1(I)                       *PESO
188000060829     C                   ADD       TASNCL        UCSC1(I)                       *COLLI
188100060829     C                   ADD       $VOL1         UVSC1(I)                       *VOLUME
188200060829     C                   LEAVE
188300060829E   6C                   ENDIF
188400060829E   5C                   ENDDO
188500060829     C*
188600060829     C                   ENDSR
188700980821     C*------------------------------------------------------------------------*
188800980821     C* CARSCH - CARICA LE SCHIERE DA ELABORARE
188900980821     C*------------------------------------------------------------------------*
189000980821     C     CARSCH        BEGSR
189100980821     C*
189200980821     C* SI POSIZIONA SULLE SCHIERE DELLA TARIFFA
189300980824     C     1             OCCUR     FPAG
189400980824     C     1             OCCUR     FCON
189500980824     C     1             OCCUR     FRIT
189600980824     C     1             OCCUR     FSGL
189700060829     C     1             OCCUR     FSGL1
189800980824     C     1             OCCUR     FCTS
189900980824     C     1             OCCUR     FTAR
190000980824     C     U             OCCUR     CCAR
190100980824     C     U             OCCUR     CPAG
190200980824     C     U             OCCUR     CCON
190300980824     C     U             OCCUR     CRIT
190400980824     C     U             OCCUR     CSGL
190500060829     C     U             OCCUR     CSGL1
190600980824     C     U             OCCUR     CCTS
190700980821     C*
190800980821     C* DATI TIPO -GENERALI-
190900980824     C                   Z-ADD     USPE(1)       SPE(1)
191000980824     C                   Z-ADD     UCLA(1)       CLA(1)
191100980824     C                   Z-ADD     UCLB(1)       CLB(1)
191200980824     C                   Z-ADD     UCLC(1)       CLC(1)
191300980824     C                   Z-ADD     UBON(1)       BON(1)
191400980824     C                   Z-ADD     UBOA(1)       BOA(1)
191500980824     C                   Z-ADD     UBOB(1)       BOB(1)
191600980824     C                   Z-ADD     UNCL(1)       NCL(1)
191700980824     C                   Z-ADD     UPKB(1)       PKB(1)
191800980824     C                   Z-ADD     UVLB(1)       VLB(1)
191900060829     C                   Z-ADD     UNCL1(1)      NCL1(1)
192000060829     C                   Z-ADD     UPKB1(1)      PKB1(1)
192100060829     C                   Z-ADD     UVLB1(1)      VLB1(1)
192200980824     C                   Z-ADD     UFRA(1)       FRA(1)
192300980824     C                   Z-ADD     UASS(1)       ASS(1)
192400980824     C                   Z-ADD     UPCV(1)       PCV(1)
192500980824     C                   Z-ADD     UPCM(1)       PCM(1)
192600980824     C                   Z-ADD     UPFD(1)       PFD(1)
192700980824     C                   Z-ADD     UESP(1)       ESP(1)
192800980824     C                   Z-ADD     UPPV(1)       PPV(1)
192900980824     C                   Z-ADD     UPCA(1)       PCA(1)
193000980824     C                   Z-ADD     UPCP(1)       PCP(1)
193100980824     C                   Z-ADD     UPCS(1)       PCS(1)
193200980824     C                   Z-ADD     UPCF(1)       PCF(1)
193300980824     C                   Z-ADD     UPCD(1)       PCD(1)
193400980824     C                   Z-ADD     UPCC(1)       PCC(1)
193500980824     C                   Z-ADD     UPCL(1)       PCL(1)
193600980824     C                   Z-ADD     UPAB(1)       PAB(1)
193700980824     C                   Z-ADD     UPAM(1)       PAM(1)
193800980824     C                   Z-ADD     UCAS(1)       CAS(1)
193900980824     C                   Z-ADD     UPAR(1)       PAR(1)
194000980824     C                   Z-ADD     UPAC(1)       PAC(1)
194100060315     C                   Z-ADD     Uxx1(1)       xx1(1)
194200980824     C                   Z-ADD     UASC(1)       ASC(1)
194300980824     C                   Z-ADD     UPGI(1)       PGI(1)
194400000414     C                   Z-ADD     UGGS(1)       GGS(1)
194500000414     C                   Z-ADD     UNRG(1)       NRG(1)
194600980901     C                   Z-ADD     UCLT(1)       CLT(1)
194700011011     C                   Z-ADD     UCIS(1)       CIS(1)
194800011011     C                   Z-ADD     URIS(1)       RIS(1)
194900020215     C                   Z-ADD     UCLD(1)       CLD(1)
195000050808     C                   Z-ADD     Upks(1)       pks(1)
195100050808     C                   Z-ADD     Upkn(1)       pkn(1)
195200040811     C                   Z-ADD     Usbnb(1)      sbnb(1)
195300040824     C                   Z-ADD     Usban(1)      sban(1)
195400040811     C                   Z-ADD     Unbn(1)       nbn(1)
195500040802     C                   Z-ADD     Unba(1)       nba(1)
195600040802     C                   Z-ADD     Uded(1)       ded(1)
195700040802     C                   Z-ADD     Uval(1)       val(1)
195800980821DO  1C     1             DO        30            J
195900980821     C                   Z-ADD     USGP(J)       SGP(J)
196000980821     C                   Z-ADD     UGGP(J)       GGP(J)
196100980821E   1C                   ENDDO
196200980821DO  1C     1             DO        999           J
196300980821     C                   Z-ADD     URIT(J)       RIT(J)
196400980821     C                   Z-ADD     UCON(J)       CON(J)
196500980821E   1C                   ENDDO
196600980821     C*
196700060829     C* DATI TIPO -SCAGLIONI- ricavi
196800980824DO  1C     1             DO        10            J
196900980825     C                   Z-ADD     USSP(J)       SSP(J)
197000980825     C                   Z-ADD     UPSC(J)       PSC(J)
197100980825     C                   Z-ADD     UCSC(J)       CSC(J)
197200980825     C                   Z-ADD     UVSC(J)       VSC(J)
197300980824E   1C                   ENDDO
197400060829     C* DATI TIPO -SCAGLIONI- competenze
197500060829DO  1C     1             DO        10            J
197600060829     C                   Z-ADD     USSP1(J)      SSP1(J)
197700060829     C                   Z-ADD     UPSC1(J)      PSC1(J)
197800060829     C                   Z-ADD     UCSC1(J)      CSC1(J)
197900060829     C                   Z-ADD     UVSC1(J)      VSC1(J)
198000060829E   1C                   ENDDO
198100980821     C*
198200980821     C* DATI TIPO -CODICI TASSAZIONE-
198300050103DO  1C     1             DO        600           J                 4 0
198400980825     C                   Z-ADD     UTSI(J)       TSI(J)
198500980825     C                   Z-ADD     UTSE(J)       TSE(J)
198600980824E   1C                   ENDDO
198700980821     C*
198800980821     C                   ENDSR
198900980821     C*------------------------------------------------------------------------*
199000980821     C* AZZSCH - AZZERA LE VARIABILI DI LAVORO
199100980821     C*------------------------------------------------------------------------*
199200980821     C     AZZSCH        BEGSR
199300980821     C*
199400980821     C                   CLEAR                   SPE                            -GENERALI-
199500980821     C                   CLEAR                   CLA
199600980821     C                   CLEAR                   CLB
199700980821     C                   CLEAR                   CLC
199800980821     C                   CLEAR                   BON
199900980821     C                   CLEAR                   BOA
200000980821     C                   CLEAR                   BOB
200100980821     C                   CLEAR                   NCL
200200980821     C                   CLEAR                   PKB
200300980821     C                   CLEAR                   VLB
200400060829     C                   CLEAR                   NCL1
200500060829     C                   CLEAR                   PKB1
200600060829     C                   CLEAR                   VLB1
200700980821     C                   CLEAR                   FRA
200800980821     C                   CLEAR                   ASS
200900980821     C                   CLEAR                   PCV
201000980821     C                   CLEAR                   PCM
201100980821     C                   CLEAR                   PFD
201200980821     C                   CLEAR                   ESP
201300980821     C                   CLEAR                   PPV
201400980821     C                   CLEAR                   PCA
201500980821     C                   CLEAR                   PCP
201600980821     C                   CLEAR                   PCS
201700980821     C                   CLEAR                   PCF
201800980821     C                   CLEAR                   PCD
201900980821     C                   CLEAR                   PCC
202000980821     C                   CLEAR                   PCL
202100980821     C                   CLEAR                   PAB
202200980821     C                   CLEAR                   PAM
202300980821     C                   CLEAR                   CAS
202400980821     C                   CLEAR                   PAR
202500980821     C                   CLEAR                   PAC
202600060315     C                   CLEAR                   xx1
202700980821     C                   CLEAR                   ASC
202800980821     C                   CLEAR                   PGI
202900980821     C                   CLEAR                   SGP
203000980821     C                   CLEAR                   GGP
203100980821     C                   CLEAR                   RIT
203200980821     C                   CLEAR                   CON
203300980901     C                   CLEAR                   CLT
203400000414     C                   CLEAR                   GGS
203500000414     C                   CLEAR                   NRG
203600980821     C                   CLEAR                   SSP                            -SCAGLIONI-
203700980821     C                   CLEAR                   PSC
203800980821     C                   CLEAR                   CSC
203900980821     C                   CLEAR                   VSC
204000060829     C                   CLEAR                   SSP1                           -SCAGLIONI-
204100060829     C                   CLEAR                   PSC1
204200060829     C                   CLEAR                   CSC1
204300060829     C                   CLEAR                   VSC1
204400980821     C                   CLEAR                   TSI                            -CODICI TASSAZIONE-
204500980821     C                   CLEAR                   TSE
204600011011     C                   CLEAR                   CIS
204700011011     C                   CLEAR                   RIS
204800020215     C                   CLEAR                   CLD
204900050808     C                   CLEAR                   pks
205000050808     C                   CLEAR                   pkn
205100040802     C                   CLEAR                   nba
205200040824     C                   CLEAR                   sban
205300040824     C                   CLEAR                   sbnb
205400040811     C                   CLEAR                   nbn
205500040802     C                   CLEAR                   ded
205600040802     C                   CLEAR                   val
205700980821     C*
205800980821     C                   ENDSR
205900980821     C*------------------------------------------------------------------------*
206000011001     C* SCRETC - SCRIVE UN RECORD DI TEETC -GENERALI-
206100980821     C*------------------------------------------------------------------------*
206200980821     C     SCRETC        BEGSR
206300980821     C*
206400060804     C                   MOVEL     'S'           KTCDSF
206500980821     C                   MOVEL     PARCTO        KTCCTO
206600980821     C                   Z-ADD     PARKOV        KTCKSC
206700980821     C                   Z-ADD     PARCTR        KTCCTR
206800980821     C                   Z-ADD     PARPRG        KTCPRG
206900980821     C                   Z-ADD     XORG          KTCLNP
207000021122     C     KEYETC        CHAIN     TEETC01L                           9697
207100980821     C                   EXSR      IMPETC                                       *IMPOSTA I CAMPI
207200021122IF  2C     *IN97         IFEQ      *OFF                                         *NON ALLOCATO
207300021122IF  3C     *IN96         IFEQ      *ON
207400980821     C                   MOVEL     KTCDSF        ETCDSF
207500980821     C                   MOVEL     KTCCTO        ETCCTO
207600980821     C                   Z-ADD     KTCKSC        ETCKSC
207700980821     C                   Z-ADD     KTCCTR        ETCCTR
207800980821     C                   Z-ADD     KTCPRG        ETCPRG
207900980821     C                   Z-ADD     KTCLNP        ETCLNP
208000980821     C                   Z-ADD     DATCOR        ETCDIR
208100980821     C                   Z-ADD     *ZEROS        ETCDUV
208200021122     C                   MOVEL     *BLANKS       ETCATB
208300011001     C                   WRITE     TEETC000                                     *INSERIMENTO
208400011001     C                   UNLOCK    TEETC01L
208500980821X   3C                   ELSE
208600980821     C                   Z-ADD     DATCOR        ETCDUV
208700021122     C                   MOVEL     *BLANKS       ETCATB
208800011001     C                   UPDATE    TEETC000                                     *VARIAZIONE
208900980821E   3C                   ENDIF
209000980821X   2C                   ELSE                                                   *ALLOCATO
209100980821     C                   MOVEL     METX(1)       DEPERR
209200980821     C                   MOVEL     'X'           DEPTER
209300980821     C                   EXSR      STAERR
209400980821E   2C                   ENDIF
209500980821     C*
209600980821     C                   ENDSR
209700980821     C*------------------------------------------------------------------------*
209800011001     C* SCRETS - SCRIVE UN RECORD DI TEETS -SCAGLIONI-
209900980821     C*------------------------------------------------------------------------*
210000980821     C     SCRETS        BEGSR
210100980821     C*
210200980821     C                   EXSR      SCRSGL                                       *SRIVE SCHIERE SGL
210300980821DO  1C     1             DO        10            J
210400980821IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
210500060804     C                   MOVEL     'S'           KTSDSF
210600980821     C                   MOVEL     PARCTO        KTSCTO
210700980821     C                   Z-ADD     PARKOV        KTSKSC
210800980821     C                   Z-ADD     PARCTR        KTSCTR
210900980821     C                   Z-ADD     PARPRG        KTSPRG
211000980821     C                   Z-ADD     XORG          KTSLNP
211100980821     C                   Z-ADD     §SG(J)        KTSSGL
211200021122     C     KEYETS        CHAIN     TEETS01L                           96
211300980821     C                   EXSR      IMPETS                                       *IMPOSTA I CAMPI
211400021122IF  4C     *IN96         IFEQ      *ON
211500980821     C                   MOVEL     KTSDSF        ETSDSF
211600980821     C                   MOVEL     KTSCTO        ETSCTO
211700980821     C                   Z-ADD     KTSKSC        ETSKSC
211800980821     C                   Z-ADD     KTSCTR        ETSCTR
211900980821     C                   Z-ADD     KTSPRG        ETSPRG
212000980821     C                   Z-ADD     KTSLNP        ETSLNP
212100980821     C                   Z-ADD     KTSSGL        ETSSGL
212200980821     C                   Z-ADD     DATCOR        ETSDIR
212300980821     C                   Z-ADD     *ZEROS        ETSDUV
212400021122     C                   MOVEL     *BLANKS       ETSATB
212500011001     C                   WRITE     TEETS000                                     *INSERIMENTO
212600011001     C                   UNLOCK    TEETS01L
212700980821X   4C                   ELSE
212800980821     C                   Z-ADD     DATCOR        ETSDUV
212900021122     C                   MOVEL     *BLANKS       ETSATB
213000011001     C                   UPDATE    TEETS000                                     *VARIAZIONE
213100980821E   4C                   ENDIF
213200980821E   2C                   ENDIF
213300980821E   1C                   ENDDO                                                  *FINE SCAGLIONI
213400980821     C*
213500980821     C                   ENDSR
213600060829     C*------------------------------------------------------------------------*
213700060829     C* SCRETS1- SCRIVE UN RECORD DI TEETS10F -SCAGLIONI-
213800060829     C*------------------------------------------------------------------------*
213900060829     C     SCRETS1       BEGSR
214000060829     C*
214100060829     C                   EXSR      SCRSGL1                                      *SRIVE SCHIERE SGL
214200060829DO  1C     1             DO        10            J
214300060829IF  2C     §SG(J)        IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
214400060829     C                   MOVEL     'S'           KTSDSF
214500060829     C                   MOVEL     PARCTO        KTSCTO
214600060829     C                   Z-ADD     PARKOV        KTSKSC
214700060829     C                   Z-ADD     PARCTR        KTSCTR
214800060829     C                   Z-ADD     PARPRG        KTSPRG
214900060829     C                   Z-ADD     XORG          KTSLNP
215000060829     C                   Z-ADD     §SG(J)        KTSSGL
215100060829     C     KEYETS        CHAIN     TEETS11L                           96
215200060829     C                   EXSR      IMPETS1                                      *IMPOSTA I CAMPI
215300060829IF  4C     *IN96         IFEQ      *ON
215400060829     C                   MOVEL     KTSDSF        £ETSDSF
215500060829     C                   MOVEL     KTSCTO        £ETSCTO
215600060829     C                   Z-ADD     KTSKSC        £ETSKSC
215700060829     C                   Z-ADD     KTSCTR        £ETSCTR
215800060829     C                   Z-ADD     KTSPRG        £ETSPRG
215900060829     C                   Z-ADD     KTSLNP        £ETSLNP
216000060829     C                   Z-ADD     KTSSGL        £ETSSGL
216100060829     C                   Z-ADD     DATCOR        £ETSDIR
216200060829     C                   Z-ADD     *ZEROS        £ETSDUV
216300060829     C                   MOVEL     *BLANKS       £ETSATB
216400060829     C                   WRITE     TEETS100                                     *INSERIMENTO
216500060829     C                   UNLOCK    TEETS11L
216600060829X   4C                   ELSE
216700060829     C                   Z-ADD     DATCOR        £ETSDUV
216800060829     C                   MOVEL     *BLANKS       £ETSATB
216900060829     C                   UPDATE    TEETS100                                     *VARIAZIONE
217000060829E   4C                   ENDIF
217100060829E   2C                   ENDIF
217200060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
217300060829     C*
217400060829     C                   ENDSR
217500980821     C*------------------------------------------------------------------------*
217600011001     C* SCRETT - SCRIVE UN RECORD DI TEETT -CODICI TASSAZIONE-
217700980821     C*------------------------------------------------------------------------*
217800980821     C     SCRETT        BEGSR
217900980821     C*
218000980821     C                   EXSR      SCRCTS                                       *SRIVE SCHIERE CTS
218100050103DO  1C     1             DO        600           J                 4 0
218200980821IF  2C     §CT(J)        IFNE      *BLANKS                                      *CODICE ESISTENTE
218300060804     C                   MOVEL     'S'           KTTDSF
218400980821     C                   MOVEL     PARCTO        KTTCTO
218500980821     C                   Z-ADD     PARKOV        KTTKSC
218600980821     C                   Z-ADD     PARCTR        KTTCTR
218700980821     C                   Z-ADD     PARPRG        KTTPRG
218800980821     C                   Z-ADD     XORG          KTTLNP
218900980821     C                   MOVEL     XFIE          KTTFIE
219000980821     C                   MOVEL     §CT(J)        KTTCTS
219100021122     C     KEYETT        CHAIN     TEETT01L                           96
219200980821     C                   EXSR      IMPETT                                       *IMPOSTA I CAMPI
219300021122IF  4C     *IN96         IFEQ      *ON
219400980821     C                   MOVEL     KTTDSF        ETTDSF
219500980821     C                   MOVEL     KTTCTO        ETTCTO
219600980821     C                   Z-ADD     KTTKSC        ETTKSC
219700980821     C                   Z-ADD     KTTCTR        ETTCTR
219800980821     C                   Z-ADD     KTTPRG        ETTPRG
219900980821     C                   Z-ADD     KTTLNP        ETTLNP
220000980821     C                   MOVEL     KTTFIE        ETTFIE
220100980821     C                   MOVEL     KTTCTS        ETTCTS
220200980821     C                   Z-ADD     DATCOR        ETTDIR
220300980821     C                   Z-ADD     *ZEROS        ETTDUV
220400021122     C                   MOVEL     *BLANKS       ETTATB
220500011001     C                   WRITE     TEETT000                                     *INSERIMENTO
220600011001     C                   UNLOCK    TEETT01L
220700980821X   4C                   ELSE
220800980821     C                   Z-ADD     DATCOR        ETTDUV
220900021122     C                   MOVEL     *BLANKS       ETTATB
221000011001     C                   UPDATE    TEETT000                                     *VARIAZIONE
221100980821E   4C                   ENDIF
221200980821E   2C                   ENDIF
221300980821E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
221400980821     C*
221500980821     C                   ENDSR
221600140919     C*------------------------------------------------------------------------*
221700140919     C* SCRETT10 - SCRIVE TEETT10 -CODICI TASSAZIONE- cap
221800140919     C*------------------------------------------------------------------------*
221900140919     C     SCRETT10      BEGSR
222000140919     C*
222100140919DO  1C     1             DO        600           y                 4 0
222200140919IF  2C     §CTc(y)       IFNE      *BLANKS                                      *CODICE ESISTENTE
222300140919     C                   MOVEL     'S'           KTTDSF
222400140919     C                   MOVEL     PARCTO        KTTCTO
222500140919     C                   Z-ADD     PARKOV        KTTKSC
222600140919     C                   Z-ADD     PARCTR        KTTCTR
222700140919     C                   Z-ADD     PARPRG        KTTPRG
222800140919     C                   Z-ADD     XORG          KTTLNP
222900140919     C                   movel     Xntw          kttntw
223000140919     C                   MOVEL     'E'           KTTFIE
223100140919     C                   MOVEL     §CTc(y)       KTTCTS
223200140919     C                   MOVEL     nc(y)         KTTnaz
223300140919     C                   MOVE      nc(y)         KTTcad
223400140919     C     KEYETT1       CHAIN     TEETT11L                           96
223500140919     C                   EXSR      IMPETT10                                     *IMPOSTA I CAMPI
223600140919IF  4C     *IN96         IFEQ      *ON
223700140919     C                   MOVEL     KTTDSF        ETTDSF
223800140919     C                   MOVEL     KTTCTO        ETTCTO
223900140919     C                   Z-ADD     KTTKSC        ETTKSC
224000140919     C                   Z-ADD     KTTCTR        ETTCTR
224100140919     C                   Z-ADD     KTTPRG        ETTPRG
224200140919     C                   Z-ADD     KTTLNP        ETTLNP
224300140919     C                   MOVEL     KTTFIE        ETTFIE
224400140919     C                   MOVEL     KTTCTS        ETTCTS
224500140919     C                   MOVEL     KTTnaz        ETTnaz
224600140919     C                   MOVEL     KTTcad        ETTcad
224700140919     C                   MOVEL     KTTntw        ETTntw
224800140919     C                   Z-ADD     DATCOR        ETTDIR
224900140919     C                   Z-ADD     *ZEROS        ETTDUV
225000140919     C                   MOVEL     *BLANKS       ETTATB
225100140919     C                   WRITE     TEETT100                                     *INSERIMENTO
225200140919     C                   UNLOCK    TEETT11L
225300140919X   4C                   ELSE
225400140919     C                   Z-ADD     DATCOR        ETTDUV
225500140919     C                   MOVEL     *BLANKS       ETTATB
225600140919     C                   UPDATE    TEETT100                                     *VARIAZIONE
225700140919E   4C                   ENDIF
225800140919E   2C                   ENDIF
225900140919E   1C                   ENDDO                                                  *FINE COD.TASSAZIONE
226000140919     C*
226100140919     C                   ENDSR
226200140919     C*------------------------------------------------------------------------*
226300140919     C* IMPETC - IMPOSTA I CAMPI DATI TIPO -GENERALI-
226400140919     C*------------------------------------------------------------------------*
226500140919     C     IMPETC        BEGSR
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
229100050808     c* packing list
229200050808     C                   Z-ADD     *ZEROS        Wmax
229300050808     C                   MOVEL     'N'           ETCpkl
229400050808IF  1C     pks(I)        IFGT      wmax
229500050808     C                   Z-ADD     pks(I)        Wmax
229600050808     C                   MOVEL     'S'           ETCpkl
229700050808E   1C                   ENDIF
229800050808IF  1C     pkn(I)        IFGT      wmax
229900050808     C                   Z-ADD     pkn(I)        Wmax
230000050808     C                   MOVEL     'N'           ETCpkl
230100050808E   1C                   ENDIF
230200970110     C*
230300970110     C                   Z-ADD     *ZEROS        WMAX              9 0
230400970110     C                   MOVEL     ' '           ETCBOL
230500970110IF  1C     BON(I)        IFGT      WMAX
230600970110     C                   Z-ADD     BON(I)        WMAX
230700970110     C                   MOVEL     ' '           ETCBOL
230800970110E   1C                   ENDIF
230900970415IF  1C     BOA(I)        IFGT      WMAX                                         *"A" VINCE SU " "
231000970415     C     BOA(I)        ORGT      *ZEROS
231100970110     C                   Z-ADD     BOA(I)        WMAX
231200970110     C                   MOVEL     'A'           ETCBOL
231300970110E   1C                   ENDIF
231400970415IF  1C     BOB(I)        IFGT      WMAX                                         *"B" VINCE SU "A"
231500970415     C     BOB(I)        ORGT      *ZEROS
231600970110     C                   Z-ADD     BOB(I)        WMAX
231700970110     C                   MOVEL     'B'           ETCBOL
231800970110E   1C                   ENDIF
231900060829     C* ricavi
232000970110IF  1C     NCL(I)        IFGT      *ZEROS
232100970110     C     PKB(I)        DIV       NCL(I)        ETCPMC
232200970110     C     VLB(I)        DIV       NCL(I)        ETCVMC
232300970703E   1C                   ENDIF
232400970703IF  1C     ETCPMC        IFEQ      *ZEROS
232500970110     C                   Z-ADD     0,1           ETCPMC
232600970703E   1C                   ENDIF
232700970703IF  1C     ETCVMC        IFEQ      *ZEROS
232800970110     C                   Z-ADD     0,001         ETCVMC
232900970703E   1C                   ENDIF
233000060829     c* competenze
233100060829IF  1C     NCL1(I)       IFGT      *ZEROS
233200060829     C     PKB1(I)       DIV       NCL1(I)       ETCPMC1
233300060829     C     VLB1(I)       DIV       NCL1(I)       ETCVMC1
233400060829E   1C                   ENDIF
233500060829IF  1C     ETCPMC1       IFEQ      *ZEROS
233600060829     C                   Z-ADD     0,1           ETCPMC1
233700060829E   1C                   ENDIF
233800060829IF  1C     ETCVMC1       IFEQ      *ZEROS
233900060829     C                   Z-ADD     0,001         ETCVMC1
234000060829E   1C                   ENDIF
234100970110     C*
234200011026     C     FRA(I)        MULT      100           WN153            21 8
234300011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
234400970110     C                   Z-ADD     WPER          ETCFRA
234500011001     C     ASS(I)        MULT      100           WN153
234600011001     C     WN153         DIV(H)    SPE(I)        WPER
234700970110     C                   Z-ADD     WPER          ETCASS
234800980803IF  1C     ETCFRA        IFGE      ETCASS
234900980803     C                   Z-ADD     *ZEROS        ETCASS
235000980831     C                   Z-ADD     100           ETCFRA
235100980803X   1C                   ELSE
235200980803     C                   Z-ADD     *ZEROS        ETCFRA
235300980831     C                   Z-ADD     100           ETCASS
235400980803E   1C                   ENDIF
235500970110     C*
235600011001     C     PCV(I)        MULT      100           WN153
235700011001     C     WN153         DIV(H)    SPE(I)        WPER
235800970110     C                   Z-ADD     WPER          ETCPCV
235900040802     C*
236000011001     C     PCM(I)        MULT      100           WN153
236100011001     C     WN153         DIV(H)    SPE(I)        WPER
236200970110     C                   Z-ADD     WPER          ETCPCM
236300970110     C*
236400011001     C     PFD(I)        MULT      100           WN153
236500011001     C     WN153         DIV(H)    SPE(I)        WPER
236600970110     C                   Z-ADD     WPER          ETCPFD
236700970110     C*
236800011001     C     ESP(I)        MULT      100           WN153
236900011001     C     WN153         DIV(H)    SPE(I)        WPER
237000970110     C                   Z-ADD     WPER          ETCESP
237100970110     C*
237200011001     C     PPV(I)        MULT      100           WN153
237300011001     C     WN153         DIV(H)    SPE(I)        WPER
237400970110     C                   Z-ADD     WPER          ETCPPV
237500970110     C*
237600011001     C     PCA(I)        MULT      100           WN153
237700011001     C     WN153         DIV(H)    SPE(I)        WPER
237800970110     C                   Z-ADD     WPER          ETCPCA
237900970110     C*
238000011001     C     PCP(I)        MULT      100           WN153
238100011001     C     WN153         DIV(H)    SPE(I)        WPER
238200970110     C                   Z-ADD     WPER          ETCPCP
238300970110     C*
238400011001     C     PCS(I)        MULT      100           WN153
238500011001     C     WN153         DIV(H)    SPE(I)        WPER
238600970110     C                   Z-ADD     WPER          ETCPCS
238700970110     C*
238800011001     C     PCF(I)        MULT      100           WN153
238900011001     C     WN153         DIV(H)    SPE(I)        WPER
239000970110     C                   Z-ADD     WPER          ETCPCF
239100970415     C*
239200011001     C     PCD(I)        MULT      100           WN153
239300011001     C     WN153         DIV(H)    SPE(I)        WPER
239400970415     C                   Z-ADD     WPER          ETCPCD
239500970415     C*
239600011001     C     PCC(I)        MULT      100           WN153
239700011001     C     WN153         DIV(H)    SPE(I)        WPER
239800970415     C                   Z-ADD     WPER          ETCPCC
239900970415     C*
240000011001     C     PCL(I)        MULT      100           WN153
240100011001     C     WN153         DIV(H)    SPE(I)        WPER
240200970415     C                   Z-ADD     WPER          ETCPCL
240300970110     C*
240400011001     C     PAB(I)        MULT      100           WN153
240500011001     C     WN153         DIV(H)    SPE(I)        WPER
240600970110     C                   Z-ADD     WPER          ETCPAB
240700970110     C*
240800011001     C     PAM(I)        MULT      100           WN153
240900011001     C     WN153         DIV(H)    SPE(I)        WPER
241000970110     C                   Z-ADD     WPER          ETCPAM
241100011011     C*
241200011011     C     CIS(I)        MULT      100           WN153
241300011011     C     WN153         DIV(H)    SPE(I)        WPER
241400011011     C                   Z-ADD     WPER          ETCCIS
241500011011     C*
241600011011     C     RIS(I)        MULT      100           WN153
241700011011     C     WN153         DIV(H)    SPE(I)        WPER
241800011011     C                   Z-ADD     WPER          ETCSAM
241900970110     C*
242000011001     C     PAB(I)        ADD       PAM(I)        WN153
242100970110     C                   Z-ADD     *ZEROS        ETCCAS
242200011001IF  1C     WN153         IFGT      *ZEROS
242300011001     C     CAS(I)        DIV       WN153         ETCCAS
242400970110E   1C                   ENDIF
242500011026     C                   Z-ADD     ETCCAS        $IMP             21 8
242600970417     C                   MOVEL     'E'           $FDE              1
242700011011     C                   IF        PARDIV = 'ITL'
242800011026     C                   Z-ADD     1000          $ARR             21 8
242900011011     C                   ELSE
243000011026     C                   Z-ADD     1             $ARR             21 8
243100011011     C                   ENDIF
243200970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
243300970110     C                   Z-ADD     $IMP          ETCCAS
243400970110     C*
243500970418     C     PAR(I)        ADD       PAC(I)        WN90              9 0
243600060315     C                   ADD       xx1(I)        WN90
243700011001     C     PAR(I)        MULT      100           WN153
243800011001     C     WN153         DIV(H)    WN90          WPER
243900970110     C                   Z-ADD     WPER          ETCPAR
244000970110     C*
244100011001     C     PAC(I)        MULT      100           WN153
244200011001     C     WN153         DIV(H)    WN90          WPER
244300970110     C                   Z-ADD     WPER          ETCPAC
244400060315     C*
244500060315     C     xx1(I)        MULT      100           WN153
244600060315     C     WN153         DIV(H)    WN90          WPER
244700060315     C                   Z-ADD     WPER          ETCxx1
244800970110     C*
244900970110     C                   Z-ADD     *ZEROS        ETCASC
245000970110IF  1C     PAC(I)        IFGT      *ZEROS
245100970110     C     ASC(I)        DIV       PAC(I)        ETCASC
245200970110E   1C                   ENDIF
245300970110     C                   Z-ADD     ETCASC        $IMP
245400970110     C                   MOVEL     'E'           $FDE
245500011011     C                   IF        PARDIV = 'ITL'
245600011026     C                   Z-ADD     1000          $ARR             21 8
245700011011     C                   ELSE
245800011026     C                   Z-ADD     1             $ARR             21 8
245900011011     C                   ENDIF
246000970110     C                   EXSR      ARRIMP                                       *ARROTONDA IMPORTO
246100970110     C                   Z-ADD     $IMP          ETCASC
246200970110     C*
246300000414     C                   Z-ADD     *ZEROS        ETCGGG
246400000414     C                   Z-ADD     *ZEROS        ETCNRG
246500011001     C     PGI(I)        MULT      100           WN153
246600011001     C     WN153         DIV(H)    SPE(I)        WPER
246700970110     C                   Z-ADD     WPER          ETCPGI
246800021122     C* SE LA % GIACENZE SUL FILE VIENE A 0 => AZZERO ANCHE I GG GIACENZA ED IL NUMERO APERTURE GIAC
246900021122     C                   IF        ETCPGI = *ZEROS
247000021122     C                   Z-ADD     *ZEROS        ETCGGG
247100021122     C                   Z-ADD     *ZEROS        ETCNRG
247200021122     C                   ELSE
247300000414IF  1C     PGI(I)        IFGT      *ZEROS
247400000414     C     GGS(I)        DIV       PGI(I)        ETCGGG                         *GG SOSTA
247500000414IF  2C     ETCGGG        IFEQ      *ZEROS
247600000414     C                   Z-ADD     1             ETCGGG
247700000414E   2C                   ENDIF
247800000414     C     NRG(I)        DIV       PGI(I)        ETCNRG                         *N° RIAPERTURE
247900000414IF  2C     ETCNRG        IFEQ      *ZEROS
248000000414     C                   Z-ADD     1             ETCNRG
248100000414E   2C                   ENDIF
248200000414E   1C                   ENDIF
248300021122     C                   ENDIF
248400970110     C*
248500970110     C                   Z-ADD     *ZEROS        WFRQ              3 0
248600011001     C                   Z-ADD     *ZEROS        WN153
248700970110DO  1C     1             DO        30            J
248800011001IF  2C     SGP(J)        IFGT      WN153
248900011001     C                   Z-ADD     SGP(J)        WN153
249000970110     C                   Z-ADD     GGP(J)        WFRQ
249100970110E   2C                   ENDIF
249200970110E   1C                   ENDDO
249300970110     C                   Z-ADD     WFRQ          ETCGGP
249400970110IF  1C     ETCGGP        IFEQ      999
249500970110     C                   Z-ADD     *ZEROS        ETCGGP
249600970110E   1C                   ENDIF
249700970110     C*
249800970110     C                   Z-ADD     *ZEROS        WSPE             11 0
249900000411     C                   Z-ADD     *ZEROS        WSTP             13 0
250000970110DO  1C     1             DO        999           J
250100970110IF  2C     RIT(J)        IFGT      *ZEROS
250200000411     C     RIT(J)        DIV       J             WN13             13 0
250300000411     C                   MVR                     RESTO            15 0
250400000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
250500000411     C     J             ANDNE     999                                          *STOP <> 999
250600000411     C                   ADD       1             WN13
250700000411E   3C                   ENDIF
250800970110     C     WSPE          ADD       RIT(J)        WSPE
250900970110     C     WSTP          ADD       WN13          WSTP
251000970110E   2C                   ENDIF
251100970110E   1C                   ENDDO
251200970110     C                   Z-ADD     *ZEROS        ETCSPR
251300970110IF  1C     WSTP          IFGT      *ZEROS
251400970110     C     WSPE          DIV       WSTP          ETCSPR
251500000411     C                   MVR                     RESTO            15 0
251600000411IF  2C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
251700000411     C                   ADD       1             ETCSPR                         *ARROTONDA X ECCESSO
251800000411E   2C                   ENDIF
251900000411E   1C                   ENDIF
252000970110IF  1C     ETCSPR        IFEQ      *ZEROS
252100970110     C                   Z-ADD     1             ETCSPR
252200970110E   1C                   ENDIF
252300970110     C*
252400970110     C                   Z-ADD     *ZEROS        WSPE             11 0
252500000411     C                   Z-ADD     *ZEROS        WSTP             13 0
252600970110DO  1C     1             DO        999           J
252700970110IF  2C     CON(J)        IFGT      *ZEROS
252800000411     C     CON(J)        DIV       J             WN13             13 0
252900000411     C                   MVR                     RESTO            15 0
253000000411IF  3C     RESTO         IFGT      *ZEROS                                       *ESISTONO DECIMALI
253100000411     C     J             ANDNE     999                                          *STOP <> 999
253200000411     C                   ADD       1             WN13
253300000411E   3C                   ENDIF
253400970110     C     WSPE          ADD       CON(J)        WSPE
253500970110     C     WSTP          ADD       WN13          WSTP
253600970110E   2C                   ENDIF
253700970110E   1C                   ENDDO
253800001221     C                   Z-ADD     1             ETCSPC
253900970110     C*---
254000970110     C* CAMPI FISSI
254100970110     C*---
254200970110     C                   MOVEL     *BLANKS       ETCSDE
254300970110     C                   Z-ADD     *ZEROS        ETCPDE
254400970619     C                   MOVEL     'E'           ETCFAR
254500011011     C                   IF        PARDIV = 'ITL'
254600970314     C                   Z-ADD     100           ETCIAR
254700011011     C                   ELSE
254800011030     C                   Z-ADD     0,1           ETCIAR
254900011011     C                   ENDIF
255000970213     C                   MOVEL     'N'           ETCSIN
255100970110     C                   MOVEL     'N'           ETCIAP
255200970110     C                   MOVEL     'N'           ETCIPI
255300970110     C                   MOVEL     'N'           ETCISU
255400970110     C                   MOVEL     'N'           ETCIFU
255500970415     C                   MOVEL     'N'           ETCIDD
255600970415     C                   MOVEL     'N'           ETCICE
255700970415     C                   MOVEL     'N'           ETCILO
255800050810     C                   MOVEL     'N'           ETCIde
255900050810     C                   MOVEL     'N'           ETCIba
256000970110     C                   MOVEL     'N'           ETCIES
256100970110     C                   MOVEL     'N'           ETCICA
256200970110     C                   MOVEL     'N'           ETCIAS
256300970110     C                   MOVEL     'N'           ETCIGI
256400020716     C                   MOVEL     'N'           ETCISO
256500020716     C                   MOVEL     'N'           ETCINR
256600980901     C                   MOVEL     METX(3)       ETCDCV
256700970110     C*
256800970110     C                   MOVEL     *BLANKS       ETCFTR
256900970110     C                   Z-ADD     *ZEROS        ETCDTR
257000970110     C                   MOVEL     *BLANKS       ETCATB
257100021104     C*
257200021104     C* EFFETTUO ALCUNE CONSIDERAZIONI
257300021104     C                   IF        ETCBOL = 'B'
257400021104     C                   EVAL      ETCIX1 = 'K'
257500021104     C                   ELSE
257600021104     C                   EVAL      ETCIX1 = *blanks
257700021104     C                   ENDIF
257800040811     C* %sped oltre 20 colli non imbancalate
257900040811     C     sbnb(I)       MULT      100           WN153
258000040802     C     WN153         DIV(H)    SPE(I)        WPER
258100040802     C                   Z-ADD     WPER          ETCbnb
258200040811     c* numero medio bancali x spedizioni oltre 20 colli
258300061006     c                   if        sbnb(i) <> 0 and etcbnb <> 0
258400040811     c     nbn(i)        div(h)    sbnb(i)       etcnbn
258500061006     c                   if        etcnbn = 0
258600061006     c                   eval      etcnbn = 1
258700061006     c                   end
258800040812     c                   else
258900040812     c                   clear                   etcnbn
259000040811     c                   end
259100040824     C* %sped oltre 20 colli non imbancalate
259200040824     C     sban(I)       MULT      100           WN153
259300040824     C     WN153         DIV(H)    SPE(I)        WPER
259400040824     C                   Z-ADD     WPER          ETCban
259500040824     c* numero medio bancali Bartolini
259600061006     c                   if        sban(i) <> 0 and etcban <> 0
259700040824     c     nba(i)        div(h)    sban(i)       etcnba
259800061006     c                   if        etcnba = 0
259900061006     c                   eval      etcnba = 1
260000061006     c                   end
260100040824     c                   else
260200040824     c                   clear                   etcnba
260300040824     c                   end
260400040802     C*
260500040802     C     ded(I)        MULT      100           WN153
260600040802     C     WN153         DIV(H)    SPE(I)        WPER
260700040802     C                   Z-ADD     WPER          ETCdes
260800040802     C*
260900040802     C     val(I)        MULT      100           WN153
261000040802     C     WN153         DIV(H)    SPE(I)        WPER
261100040802     C                   Z-ADD     WPER          ETCval
261200970110     C*
261300970110     C                   ENDSR
261400970416     C*------------------------------------------------------------------------*
261500060829     C* SCRSGL - SCRIVE SCHIERE SCAGLIONI ricavi
261600970416     C*------------------------------------------------------------------------*
261700970416     C     SCRSGL        BEGSR
261800970416     C*
261900970416     C* AZZERA LE VARIABILI DI LAVORO
262000970416     C                   CLEAR                   §SG
262100970416     C                   CLEAR                   §PS
262200970416     C                   CLEAR                   §CS
262300970416     C                   CLEAR                   §VS
262400970416     C                   CLEAR                   §SP
262500971008     C                   CLEAR                   §SD
262600970416     C*
262700970416     C* PER CIASCUN SCAGLIONE DA SCRIVERE
262800970416DO  1C     1             DO        10            J
262900970416IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
263000970416     C                   Z-ADD     SSGL(J)       §SG(J)
263100970416IF  3C     SSP(J)        IFGT      *ZEROS
263200970416     C*
263300970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
263400971008     C                   MOVEL     *BLANKS       §SD(J)                         *VALORE NON DEDOTTO
263500970416     C     PSC(J)        DIV       SSP(J)        §PS(J)
263600970416     C     CSC(J)        DIV       SSP(J)        §CS(J)
263700970416     C     VSC(J)        DIV       SSP(J)        §VS(J)
263800011001     C     SSP(J)        MULT      100           WN153
263900011001     C     WN153         DIV(H)    SPE(I)        WPER              4 1
264000970416     C                   Z-ADD     WPER          §SP(J)
264100970703IF  4C     §SP(J)        IFEQ      *ZEROS
264200970703     C                   Z-ADD     0,1           §SP(J)
264300970703E   4C                   ENDIF
264400970416X   3C                   ELSE
264500970416     C*
264600970416     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
264700971008     C                   MOVEL     '*'           §SD(J)                         *VALORE DEDOTTO
264800970416IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
264900970416     C     PARTTR        OREQ      '3'                                          *KG
265000970416     C                   Z-ADD     §SG(J)        §PS(J)
265100970416     C     §PS(J)        DIV       ETCPMC        §CS(J)
265200970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
265300970416X   4C                   ELSE                                                   *COLLI
265400970416     C                   Z-ADD     §SG(J)        §CS(J)
265500040812     C     §CS(J)        mult      ETCPMC        §PS(J)
265600970416     C     §CS(J)        MULT      ETCVMC        §VS(J)
265700970416E   4C                   ENDIF
265800970416     C                   Z-ADD     0,1           §SP(J)
265900970416E   3C                   ENDIF
266000970416E   2C                   ENDIF
266100970416E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
266200000707     C*
266300000707     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
266400000707     C                   EXSR      PARPSG
266500970416     C*
266600970416     C                   ENDSR
266700060829     C*------------------------------------------------------------------------*
266800060829     C* SCRSGL1- SCRIVE SCHIERE SCAGLIONI competenze
266900060829     C*------------------------------------------------------------------------*
267000060829     C     SCRSGL1       BEGSR
267100060829     C*
267200060829     C* AZZERA LE VARIABILI DI LAVORO
267300060829     C                   CLEAR                   §SG
267400060829     C                   CLEAR                   §PS1
267500060829     C                   CLEAR                   §CS1
267600060829     C                   CLEAR                   §VS1
267700060829     C                   CLEAR                   §SP1
267800060829     C                   CLEAR                   §SD1
267900060829     C*
268000060829     C* PER CIASCUN SCAGLIONE DA SCRIVERE
268100060829DO  1C     1             DO        10            J
268200060829IF  2C     SSGL(J)       IFGT      *ZEROS                                       *SCAGLIONE ESISTENTE
268300060829     C                   Z-ADD     SSGL(J)       §SG(J)
268400060829IF  3C     SSP1(J)       IFGT      *ZEROS
268500060829     C*
268600060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE CON DATI
268700060829     C                   MOVEL     *BLANKS       §SD1(J)                        *VALORE NON DEDOTTO
268800060829     C     PSC1(J)       DIV       SSP1(J)       §PS1(J)
268900060829     C     CSC1(J)       DIV       SSP1(J)       §CS1(J)
269000060829     C     VSC1(J)       DIV       SSP1(J)       §VS1(J)
269100060829     C     SSP1(J)       MULT      100           WN153
269200060829     C     WN153         DIV(H)    SPE(I)        WPER              4 1
269300060829     C                   Z-ADD     WPER          §SP1(J)
269400060829IF  4C     §SP1(J)       IFEQ      *ZEROS
269500060829     C                   Z-ADD     0,1           §SP1(J)
269600060829E   4C                   ENDIF
269700060829X   3C                   ELSE
269800060829     C*
269900060829     C* VALORIZZA SCHIERE DEGLI SCAGLIONE SENZA DATI
270000060829     C                   MOVEL     '*'           §SD1(J)                        *VALORE DEDOTTO
270100060829IF  4C     PARTTR        IFEQ      '0'                                          *QUINTALI
270200060829     C     PARTTR        OREQ      '3'                                          *KG
270300060829     C                   Z-ADD     §SG(J)        §PS1(J)
270400060829     C     §PS1(J)       DIV       ETCPMC1       §CS1(J)
270500060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
270600060829X   4C                   ELSE                                                   *COLLI
270700060829     C                   Z-ADD     §SG(J)        §CS1(J)
270800060829     C     §CS1(J)       mult      ETCPMC1       §PS1(J)
270900060829     C     §CS1(J)       MULT      ETCVMC1       §VS1(J)
271000060829E   4C                   ENDIF
271100060829     C                   Z-ADD     0,1           §SP1(J)
271200060829E   3C                   ENDIF
271300060829E   2C                   ENDIF
271400060829E   1C                   ENDDO                                                  *FINE SGL DA SCRIVER
271500060829     C*
271600060829     C* PAREGGIA LA %TOTALE -SCAGLIONI- A 100
271700060829     C                   EXSR      PARPSG1
271800060829     C*
271900060829     C                   ENDSR
272000000707     C*------------------------------------------------------------------------*
272100000707     C* PARPSG - PAREGGIA LA %TOTALE -SCAGLIONI- A 100
272200000707     C*------------------------------------------------------------------------*
272300000707     C     PARPSG        BEGSR
272400000707     C*
272500000707     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
272600000707     C                   Z-ADD     *ZEROS        TOTPPE            4 1
272700000707DO  1C     1             DO        10            J
272800000707IF  2C     §SG(J)        IFGT      *ZEROS
272900000707     C                   ADD       §SP(J)        TOTPPE                         *%INCIDENZA TOTALE
273000000707E   2C                   ENDIF
273100000707E   1C                   ENDDO
273200000707     C*
273300000707     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
273400000707     C                   Z-ADD     *ZEROS        WN41              4 1
273500000707     C     100           SUB       TOTPPE        WN41
273600000707     C*
273700000707     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
273800000707IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
273900000707DO  2C     WN41          DOWNE     *ZEROS
274000000707DO  3C     1             DO        10            J
274100000707IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
274200000707     C* POSITIVA -> AGGIUNGERE
274300000707IF  5C     WN41          IFGT      *ZEROS
274400000707     C     §SP(J)        ADD       0,1           §SP(J)                         *AGGIUNGE %
274500000707     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
274600000707E   5C                   ENDIF
274700000707     C* NEGATIVA -> TOGLIERE
274800000707IF  5C     WN41          IFLT      *ZEROS
274900000707IF  6C     §SP(J)        IFGT      0,1
275000000707     C     §SP(J)        SUB       0,1           §SP(J)                         *SOTTRAE %
275100000707     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
275200000707E   6C                   ENDIF
275300000707E   5C                   ENDIF
275400000707     C* UGUALE A ZERO -> USCIRE
275500000707IF  5C     WN41          IFEQ      *ZEROS
275600000707     C                   LEAVE
275700000707E   5C                   ENDIF
275800000707E   4C                   ENDIF
275900000707E   3C                   ENDDO                                                  *FINE SCAGLIONI
276000000707E   2C                   ENDDO                                                  *DIFFERENZA = 0
276100000707E   1C                   ENDIF
276200000707     C*
276300000707     C                   ENDSR
276400060829     C*------------------------------------------------------------------------*
276500060829     C* PARPSG1- PAREGGIA LA %TOTALE -SCAGLIONI- A 100
276600060829     C*------------------------------------------------------------------------*
276700060829     C     PARPSG1       BEGSR
276800060829     C*
276900060829     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI GLI SCAGLIONI
277000060829     C                   Z-ADD     *ZEROS        TOTPPE            4 1
277100060829DO  1C     1             DO        10            J
277200060829IF  2C     §SG(J)        IFGT      *ZEROS
277300060829     C                   ADD       §SP1(J)       TOTPPE                         *%INCIDENZA TOTALE
277400060829E   2C                   ENDIF
277500060829E   1C                   ENDDO
277600060829     C*
277700060829     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEGLI SCAGLIONI E 100
277800060829     C                   Z-ADD     *ZEROS        WN41              4 1
277900060829     C     100           SUB       TOTPPE        WN41
278000060829     C*
278100060829     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI GLI SCAGLIONI
278200060829IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
278300060829DO  2C     WN41          DOWNE     *ZEROS
278400060829DO  3C     1             DO        10            J
278500060829IF  4C     §SG(J)        IFGT      *ZEROS                                       *ELEMENTO PIENO
278600060829     C* POSITIVA -> AGGIUNGERE
278700060829IF  5C     WN41          IFGT      *ZEROS
278800060829     C     §SP1(J)       ADD       0,1           §SP1(J)                        *AGGIUNGE %
278900060829     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
279000060829E   5C                   ENDIF
279100060829     C* NEGATIVA -> TOGLIERE
279200060829IF  5C     WN41          IFLT      *ZEROS
279300060829IF  6C     §SP1(J)       IFGT      0,1
279400060829     C     §SP1(J)       SUB       0,1           §SP1(J)                        *SOTTRAE %
279500060829     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
279600060829E   6C                   ENDIF
279700060829E   5C                   ENDIF
279800060829     C* UGUALE A ZERO -> USCIRE
279900060829IF  5C     WN41          IFEQ      *ZEROS
280000060829     C                   LEAVE
280100060829E   5C                   ENDIF
280200060829E   4C                   ENDIF
280300060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
280400060829E   2C                   ENDDO                                                  *DIFFERENZA = 0
280500060829E   1C                   ENDIF
280600060829     C*
280700060829     C                   ENDSR
280800970110     C*------------------------------------------------------------------------*
280900060829     C* IMPETS - IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- ricavi
281000970110     C*------------------------------------------------------------------------*
281100970318     C     IMPETS        BEGSR
281200970110     C*---
281300970110     C* CAMPI CALCOLATI
281400970110     C*---
281500970416     C                   Z-ADD     §PS(J)        ETSPSC
281600970416     C                   Z-ADD     §CS(J)        ETSCSC
281700981020IF  1C     ETSCSC        IFLT      1
281800981020     C                   Z-ADD     1             ETSCSC
281900981020E   1C                   ENDIF
282000970416     C                   Z-ADD     §VS(J)        ETSVSC
282100970416     C                   Z-ADD     §SP(J)        ETSPPE
282200970415     C*---
282300970415     C* CAMPI FISSI
282400970415     C*---
282500980325     C                   Z-ADD     *ZEROS        ETSINL
282600970415     C                   Z-ADD     *ZEROS        ETSRPV
282700970415     C                   MOVEL     *BLANKS       ETSSDE
282800970415     C                   Z-ADD     *ZEROS        ETSPDE
282900970110     C*
283000040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
283100040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
283200040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
283300040805     C* MA È GIUSTO COSÌ
283400040805     C                   MOVEL     §SD(J)        ETSFTR
283500970110     C                   Z-ADD     *ZEROS        ETSDTR
283600970110     C                   MOVEL     *BLANKS       ETSATB
283700970110     C*
283800970110     C                   ENDSR
283900060829     C*------------------------------------------------------------------------*
284000060829     C* IMPETS1- IMPOSTA I CAMPI DATI TIPO -SCAGLIONI- competenze
284100060829     C*------------------------------------------------------------------------*
284200060829     C     IMPETS1       BEGSR
284300060829     C*---
284400060829     C* CAMPI CALCOLATI
284500060829     C*---
284600060829     C                   Z-ADD     §PS1(J)       £ETSPSC
284700060829     C                   Z-ADD     §CS1(J)       £ETSCSC
284800060829IF  1C     £ETSCSC       IFLT      1
284900060829     C                   Z-ADD     1             £ETSCSC
285000060829E   1C                   ENDIF
285100060829     C                   Z-ADD     §VS1(J)       £ETSVSC
285200060829     C                   Z-ADD     §SP1(J)       £ETSPPE
285300060829     C*---
285400060829     C* CAMPI FISSI
285500060829     C*---
285600060829     C                   Z-ADD     *ZEROS        £ETSINL
285700060829     C                   Z-ADD     *ZEROS        £ETSRPV
285800060829     C                   MOVEL     *BLANKS       £ETSSDE
285900060829     C                   Z-ADD     *ZEROS        £ETSPDE
286000060829     C*
286100060829     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
286200060829     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
286300060829     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
286400060829     C* MA È GIUSTO COSÌ
286500060829     C                   MOVEL     §SD1(J)       £ETSFTR
286600060829     C                   Z-ADD     *ZEROS        £ETSDTR
286700060829     C                   MOVEL     *BLANKS       £ETSATB
286800060829     C*
286900060829     C                   ENDSR
287000970416     C*------------------------------------------------------------------------*
287100970416     C* SCRCTS - SCRIVE SCHIERE CODICI TASSAZIONE
287200970416     C*------------------------------------------------------------------------*
287300970416     C     SCRCTS        BEGSR
287400000707     C*---
287500000707     C* ITALIA
287600000707     C*---
287700000707     C* PER CIASCUN CODICE TASSAZIONE -ITALIA- DA SCRIVERE
287800000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
287900000707     C                   CLEAR                   §RC
288000000707     C                   CLEAR                   §CP
288100000707     C                   CLEAR                   §CD
288200980821IF  1C     XFIE          IFEQ      'I'
288300970423     C                   XFOOT     TSI           TOTSPE           11 0          *TOT. SPED. ITALIA
288400050103DO  2C     1             DO        600           J
288500970423IF  3C     CCTI(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
288600000620     C     CRCI(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
288700970423     C                   MOVEL     CCTI(J)       §CT(J)
288800970423     C                   MOVEL     CRCI(J)       §RC(J)
288900970417     C*
289000970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
289100971008     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
289200970423IF  4C     TSI(J)        IFGT      *ZEROS
289300011001     C     TSI(J)        MULT      100           WN153
289400011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
289500970417     C                   Z-ADD     WPER          §CP(J)
289600970703IF  5C     §CP(J)        IFEQ      *ZEROS
289700970703     C                   Z-ADD     0,1           §CP(J)
289800970703E   5C                   ENDIF
289900970423X   4C                   ELSE
290000970417     C*
290100970417     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
290200971008     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
290300970417     C                   Z-ADD     0,1           §CP(J)
290400970423E   4C                   ENDIF
290500970423E   3C                   ENDIF
290600970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
290700000707     C*
290800000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
290900000707     C                   EXSR      PARPCT
291000970423     C*---
291100970423     C* ESTERO
291200970423     C*---
291300140916     C* PER CIASCUN CODICE TASSAZIONE -estero- DA SCRIVERE
291400970423X   1C                   ELSE
291500000707     C                   CLEAR                   §CT                            *AZZERA SCHIERE LAVO
291600000707     C                   CLEAR                   §RC
291700000707     C                   CLEAR                   §CP
291800000707     C                   CLEAR                   §CD
291900970423     C                   XFOOT     TSE           TOTSPE           11 0          *TOT. SPED. ITALIA
292000050103DO  2C     1             DO        600           J
292100970423IF  3C     CCTE(J)       IFNE      *BLANKS                                      *CODICE ESISTENTE
292200000620     C     CRCE(J)       ANDNE     *BLANKS                                      *CODICE DA CALCOLARE
292300970423     C                   MOVEL     CCTE(J)       §CT(J)
292400970423     C                   MOVEL     CRCE(J)       §RC(J)
292500970423     C*
292600970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE CON DATI
292700990803     C                   MOVEL     *BLANKS       §CD(J)                         *VALORE NON DEDOTTO
292800970423IF  4C     TSE(J)        IFGT      *ZEROS
292900011001     C     TSE(J)        MULT      100           WN153
293000011001     C     WN153         DIV(H)    TOTSPE        WPER              4 1
293100970423     C                   Z-ADD     WPER          §CP(J)
293200970703IF  5C     §CP(J)        IFEQ      *ZEROS
293300970703     C                   Z-ADD     0,1           §CP(J)
293400970703E   5C                   ENDIF
293500970423X   4C                   ELSE
293600970423     C*
293700970423     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
293800990803     C                   MOVEL     '*'           §CD(J)                         *VALORE DEDOTTO
293900970423     C                   Z-ADD     0,1           §CP(J)
294000970423E   4C                   ENDIF
294100970423E   3C                   ENDIF
294200140916     c* per ogni codice tassazione calcolo la % incidenza dei sui cap
294300140916     c                   exsr      scrcts10
294400970423E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
294500000707     C*
294600000707     C* PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
294700000707     C                   EXSR      PARPCT
294800000707E   1C                   ENDIF
294900000707     C*
295000000707     C                   ENDSR
295100140918     C*------------------------------------------------------------------------*
295200140918     C* SCRCTS10 - SCRIVE SCHIERE CODICI TASSAZIONE x cap e nazione
295300140918     C*------------------------------------------------------------------------*
295400140919     C     SCRCTS10      BEGSR
295500140918     C*---
295600140918     C* ESTERO
295700140918     C*---
295800140919IF  3C     J             occur     cccte                                        *CODICE ESISTENTE
295900140918     C* PER CIASCUN CODICE TASSAZIONE -estero- DA SCRIVERE
296000140919     C                   CLEAR                   §CTc                           *AZZERA SCHIERE LAVO
296100140919     C                   CLEAR                   §RCc
296200140919     C                   CLEAR                   §CPc
296300140919     C                   CLEAR                   §CDc
296400140919     c
296500140919     C                   xfoot     cTSE          TOTSPEc          11 0          *TOT. SPED. ITALIA
296600140919     c* se non ci sono sped forzo la nazione per caricare la percentuale 0,1
296700140919     c                   if        totspec = 0
296800140919     c                   z-add     1             y
296900140919     C     CCTE(J)       lookup    cts(y)                                 98
297000140919     c                   if        *in98
297100140919     c                   movel     naz(y)        nc(1)
297200140919     c                   end
297300140919     c                   end
297400140919     c*
297500140919DO  2C     1             DO        600           y
297600140918     C*
297700140919IF  3C     nc(y)         IFNE      *BLANKS                                      *CODICE ESISTENTE
297800140919     C                   MOVEL     CCTE(J)       §CTc(y)
297900140919     C                   MOVEL     CRCE(J)       §RCc(y)
298000140919     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE e cap CON DATI
298100140919     C                   MOVEL     *BLANKS       §CDc(y)                        *VALORE NON DEDOTTO
298200140919IF  4C     cTSE(y)       IFGT      *ZEROS
298300140919     C     cTSE(y)       MULT      100           WN153
298400140919     C     WN153         DIV(H)    TOTSPEc       WPER              4 1
298500140919     C                   Z-ADD     WPER          §CPc(y)
298600140919IF  5C     §CPc(y)       IFEQ      *ZEROS
298700140919     C                   Z-ADD     0,1           §CPc(y)
298800140918E   5C                   ENDIF
298900140918X   4C                   ELSE
299000140918     C*
299100140918     C* VALORIZZA SCHIERE DEI CODICI TASSAZIONE SENZA DATI
299200140919     C                   MOVEL     '*'           §CDc(y)                        *VALORE DEDOTTO
299300140919     C                   Z-ADD     0,1           §CPc(y)
299400140918E   4C                   ENDIF
299500140918E   3C                   ENDIF
299600140919E   2C                   ENDDO                                                  *FINE CTS DA SCRIVER
299700140918     C*
299800140919     C* PAREGGIA LA %TOTALE cap x -CODICI TASSAZIONE- A 100
299900140919     c                   if        totspec > 0
300000140919     C                   EXSR      PARPCTc
300100140919     c                   end
300200140919     c* scrivo i cap x codice tassazione
300300140919     c                   exsr      scrett10
300400140918     C*
300500140918     C                   ENDSR
300600140919     C*------------------------------------------------------------------------*
300700140919     C* PARPCT - PAREGGIA LA %TOTALE -CODICI TASSAZIONE- A 100
300800140919     C*------------------------------------------------------------------------*
300900140919     C     PARPCT        BEGSR
301000140919     C*
301100140919     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
301200140919     C                   Z-ADD     *ZEROS        TOTPPE            4 1
301300140919DO  1C     1             DO        600           J
301400140919IF  2C     §CT(J)        IFNE      *BLANKS
301500140919     C                   ADD       §CP(J)        TOTPPE                         *%INCIDENZA TOTALE
301600140919E   2C                   ENDIF
301700140919E   1C                   ENDDO
301800140919     C*
301900140919     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
302000140919     C                   Z-ADD     *ZEROS        WN41              4 1
302100140919     C     100           SUB       TOTPPE        WN41
302200140919     C*
302300140919     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
302400140919IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
302500140919DO  2C     WN41          DOWNE     *ZEROS
302600140919DO  3C     1             DO        600           J
302700140919IF  4C     §CT(J)        IFNE      *BLANKS                                      *ELEMENTO PIENO
302800140919     C     §RC(J)        ANDNE     *BLANKS                                      *DA CALCOLARE
302900140919     C* POSITIVA -> AGGIUNGERE
303000140919IF  5C     WN41          IFGT      *ZEROS
303100140919     C     §CP(J)        ADD       0,1           §CP(J)                         *AGGIUNGE %
303200140919     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
303300140919E   5C                   ENDIF
303400140919     C* NEGATIVA -> TOGLIERE
303500140919IF  5C     WN41          IFLT      *ZEROS
303600140919IF  6C     §CP(J)        IFGT      0,1
303700140919     C     §CP(J)        SUB       0,1           §CP(J)                         *SOTTRAE %
303800140919     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
303900140919E   6C                   ENDIF
304000140919E   5C                   ENDIF
304100140919     C* UGUALE A ZERO -> USCIRE
304200140919IF  5C     WN41          IFEQ      *ZEROS
304300140919     C                   LEAVE
304400140919E   5C                   ENDIF
304500140919E   4C                   ENDIF
304600140919E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
304700140919E   2C                   ENDDO                                                  *DIFFERENZA = 0
304800140919E   1C                   ENDIF
304900140919     C*
305000140919     C                   ENDSR
305100140919     C*------------------------------------------------------------------------*
305200140919     C* PARPCTc- PAREGGIA LA %TOTALE cap x CODICI TASSAZIONE- A 100
305300140919     C*------------------------------------------------------------------------*
305400140919     C     PARPCTc       BEGSR
305500140919     C*
305600140919     C* CALCOLA LA %TOTALE DI INCIDENZA DI TUTTI I CODICI TASSAZIONE
305700140919     C                   Z-ADD     *ZEROS        TOTPPEc           4 1
305800140919DO  1C     1             DO        600           y
305900140919IF  2C     §CTc(y)       IFNE      *BLANKS
306000140919     C                   ADD       §CPc(y)       TOTPPEc                        *%INCIDENZA TOTALE
306100140919E   2C                   ENDIF
306200140919E   1C                   ENDDO
306300140919     C*
306400140919     C* CALCOLA LA DIFFERENZA FRA LA %TOTALE DEI CODICI TASSAZIONE E 100
306500140919     C                   Z-ADD     *ZEROS        WN41              4 1
306600140919     C     100           SUB       TOTPPEc       WN41
306700140919     C*
306800140919     C* AGGIUNGE O TOGLIE LA DIFFERENZA % A TUTTI I CODICI TASSAZIONE
306900140919IF  1C     WN41          IFNE      *ZEROS                                       *DIFFERENZA ESISTENT
307000140919DO  2C     WN41          DOWNE     *ZEROS
307100140919DO  3C     1             DO        600           y
307200140919IF  4C     §CTc(y)       IFNE      *BLANKS                                      *ELEMENTO PIENO
307300140919     C     §RCc(y)       ANDNE     *BLANKS                                      *DA CALCOLARE
307400140919     C* POSITIVA -> AGGIUNGERE
307500140919IF  5C     WN41          IFGT      *ZEROS
307600140919     C     §CPc(y)       ADD       0,1           §CPc(y)                        *AGGIUNGE %
307700140919     C     WN41          SUB       0,1           WN41                           *DIMINUISCE DIFFER.
307800140919E   5C                   ENDIF
307900140919     C* NEGATIVA -> TOGLIERE
308000140919IF  5C     WN41          IFLT      *ZEROS
308100140919IF  6C     §CPc(y)       IFGT      0,1
308200140919     C     §CPc(y)       SUB       0,1           §CPc(y)                        *SOTTRAE %
308300140919     C     WN41          ADD       0,1           WN41                           *AUMENTA DIFFER.
308400140919E   6C                   ENDIF
308500140919E   5C                   ENDIF
308600140919     C* UGUALE A ZERO -> USCIRE
308700140919IF  5C     WN41          IFEQ      *ZEROS
308800140919     C                   LEAVE
308900140919E   5C                   ENDIF
309000140919E   4C                   ENDIF
309100140919E   3C                   ENDDO                                                  *FINE COD.TASSAZIONE
309200140919E   2C                   ENDDO                                                  *DIFFERENZA = 0
309300140919E   1C                   ENDIF
309400140919     C*
309500140919     C                   ENDSR
309600970110     C*------------------------------------------------------------------------*
309700970110     C* IMPETT - IMPOSTA I CAMPI DATI TIPO -CODICI TASSAZIONE-
309800970110     C*------------------------------------------------------------------------*
309900970110     C     IMPETT        BEGSR
310000970110     C*---
310100970110     C* CAMPI CALCOLATI
310200970110     C*---
310300970417     C                   MOVEL     §RC(J)        ETTRCT
310400970417     C                   Z-ADD     §CP(J)        ETTPPE
310500970110     C*---
310600970110     C* CAMPI FISSI
310700970110     C*---
310800040805     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
310900040805     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
311000040805     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
311100040805     C* MA È GIUSTO COSÌ
311200040805     C                   MOVEL     §CD(J)        ETTFTR
311300970110     C                   Z-ADD     *ZEROS        ETTDTR
311400970110     C                   MOVEL     *BLANKS       ETTATB
311500970110     C*
311600970110     C                   ENDSR
311700140919     C*------------------------------------------------------------------------*
311800140919     C* IMPETT10 - IMPOSTA I CAMPI DATI TIPO -CODICI TASSAZIONE- CAP
311900140919     C*------------------------------------------------------------------------*
312000140919     C     IMPETT10      BEGSR
312100140919     C*---
312200140919     C* CAMPI CALCOLATI
312300140919     C*---
312400140919     C                   MOVEL     §RCc(y)       ETTRCT
312500140919     C                   Z-ADD     §CPc(y)       ETTPPE
312600140919     C*---
312700140919     C* CAMPI FISSI
312800140919     C*---
312900140919     C* UTILIZZO IL CAMPO FTR X MEMORIZZARE SE IL VALORE È STATO DETTO
313000140919     C* OPPURE SE HO AVUTO DELLE SPEDIZIONI. AD OGNI AGGIORNAMENTO
313100140919     C* DEL RECORD VIENE PULITO E QUINDI PERDO IL CONTENUTO ORIGINALE
313200140919     C* MA È GIUSTO COSÌ
313300140919     C                   MOVEL     §CDc(y)       ETTFTR
313400140919     C                   Z-ADD     *ZEROS        ETTDTR
313500140919     C                   MOVEL     *BLANKS       ETTATB
313600140919     C*
313700140919     C                   ENDSR
313800970417     C*------------------------------------------------------------------------*
313900970417     C* ARRIMP - ARROTONDA IMPORTO
314000970417     C*------------------------------------------------------------------------*
314100970417     C     ARRIMP        BEGSR
314200970417     C*---
314300970417     C* $IMP -> IMPORTO DA ARROTONDARE
314400970417     C* $FDE -> 'D'=DIFETTO / E=ECCESSO
314500970417     C* $ARR  : IMPORTO AL QUALE ARROTONDARE
314600970417     C*---
314700970417IF  1C     $IMP          IFGT      *ZEROS                                       *IMPORTO PIENO
314800970417IF  2C     $ARR          IFGT      *ZEROS                                       *SI ARROTONDAMENTO
314900970417     C     $IMP          DIV       $ARR          COMODO           15 0
315000970417     C                   MVR                     RESTO            15 0
315100970417IF  3C     $FDE          IFEQ      'E'
315200970417IF  4C     RESTO         IFGT      *ZEROS
315300970417     C                   ADD       1             COMODO
315400970417E   4C                   ENDIF
315500970417E   3C                   ENDIF
315600970417     C     COMODO        MULT      $ARR          $IMP
315700970417E   2C                   ENDIF
315800970417E   1C                   ENDIF
315900970417     C*
316000970417     C                   ENDSR
316100970110     C*------------------------------------------------------------------------*
316200970110     C* STAERR - STAMPA RIGA DI ERRORE
316300970110     C*------------------------------------------------------------------------*
316400970110     C     STAERR        BEGSR
316500970110     C*
316600970110IF  1C     *INOF         IFEQ      *ON
316700970110     C                   EXCEPT    TES
316800970110     C                   SETOFF                                       OF
316900970110E   1C                   ENDIF
317000970110IF  1C     DEPTER        IFEQ      'T'
317100970110     C                   EXCEPT    ETAS
317200970110E   1C                   ENDIF
317300970110IF  1C     DEPTER        IFEQ      'X'
317400970110     C                   EXCEPT    EETX
317500970110E   1C                   ENDIF
317600970110     C*
317700970110     C                   ENDSR
317800970313     C*------------------------------------------------------------------------*
317900970314     C* ANNETX - ANNULLA I DATI TIPO ESISTENTI
318000970313     C*------------------------------------------------------------------------*
318100970314     C     ANNETX        BEGSR
318200970313     C*
318300970314     C* PER OGNI FILIALE RICHIESTA
318400971124DO  1C     1             DO        500           I
318500970313IF  2C     SORG(I)       IFGT      *ZEROS
318600970313     C*---
318700970313     C* DATI TIPO -GENERALI-
318800970313     C*---
318900060804     C                   MOVEL     'S'           KTCDSF
319000980713     C                   MOVEL     PARCTO        KTCCTO
319100970313     C                   Z-ADD     PARKOV        KTCKSC
319200970313     C                   Z-ADD     PARCTR        KTCCTR
319300970313     C                   Z-ADD     PARPRG        KTCPRG
319400970313     C                   Z-ADD     SORG(I)       KTCLNP
319500011001     C     KEYETC        CHAIN     TEETC01L                           99
319600981221IF  3C     *IN99         IFEQ      *OFF
319700971024IF  4C     ETCATB        IFEQ      *BLANKS
319800970314     C                   MOVEL     'A'           ETCATB
319900970314     C                   MOVEL     *BLANKS       ETCFTR
320000011001     C                   UPDATE    TEETC000
320100970314E   4C                   ENDIF
320200970314E   3C                   ENDIF
320300970313     C*---
320400970313     C* DATI TIPO -SCAGLIONI-
320500970313     C*---
320600060829     c* ricavi
320700980713     C                   MOVEL     'S'           KTSDSF
320800980713     C                   MOVEL     PARCTO        KTSCTO
320900970313     C                   Z-ADD     PARKOV        KTSKSC
321000970313     C                   Z-ADD     PARCTR        KTSCTR
321100970313     C                   Z-ADD     PARPRG        KTSPRG
321200970313     C                   Z-ADD     SORG(I)       KTSLNP
321300011001     C     KE2ETS        SETLL     TEETS01L
321400011001     C     KE2ETS        READE     TEETS01L                               99
321500970314DO  3C     *IN99         DOWEQ     *OFF
321600971024IF  4C     ETSATB        IFEQ      *BLANKS
321700970314     C                   MOVEL     'A'           ETSATB
321800970314     C                   MOVEL     *BLANKS       ETSFTR
321900011001     C                   UPDATE    TEETS000
322000970314E   4C                   ENDIF
322100011001     C     KE2ETS        READE     TEETS01L                               99
322200060829E   3c                   ENDDO                                                  *FINE SCAGLIONI
322300060829     c* competetnze
322400060829     C     KE2ETS        SETLL     TEETS11L
322500060829     C     KE2ETS        READE     TEETS11L                               99
322600060829DO  3C     *IN99         DOWEQ     *OFF
322700060829IF  4C     £ETSATB       IFEQ      *BLANKS
322800060829     C                   MOVEL     'A'           £ETSATB
322900060829     C                   MOVEL     *BLANKS       £ETSFTR
323000060829     C                   UPDATE    TEETS100
323100060829E   4C                   ENDIF
323200060829     C     KE2ETS        READE     TEETS11L                               99
323300060829E   3C                   ENDDO                                                  *FINE SCAGLIONI
323400970314     C*---
323500970314     C* DATI TIPO -CODICI TASSAZIONE-
323600970314     C*---
323700970429DO  3C     1             DO        2             K
323800970429IF  4C     SFIE(K)       IFNE      *BLANKS
323900980713     C                   MOVEL     'S'           KTTDSF
324000980713     C                   MOVEL     PARCTO        KTTCTO
324100970314     C                   Z-ADD     PARKOV        KTTKSC
324200970314     C                   Z-ADD     PARCTR        KTTCTR
324300970314     C                   Z-ADD     PARPRG        KTTPRG
324400970314     C                   Z-ADD     SORG(I)       KTTLNP
324500970429     C                   MOVEL     SFIE(K)       KTTFIE
324600011001     C     KE2ETT        SETLL     TEETT01L
324700011001     C     KE2ETT        READE     TEETT01L                               99
324800970429DO  5C     *IN99         DOWEQ     *OFF
324900971024IF  6C     ETTATB        IFEQ      *BLANKS
325000970314     C                   MOVEL     'A'           ETTATB
325100970314     C                   MOVEL     *BLANKS       ETTFTR
325200011001     C                   UPDATE    TEETT000
325300970429E   6C                   ENDIF
325400011001     C     KE2ETT        READE     TEETT01L                               99
325500970429E   5C                   ENDDO                                                  *FINE COD.TASSAZIONE
325600970429E   4C                   ENDIF
325700970429E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
325800140919     C*---
325900140919     C* DATI TIPO -CODICI TASSAZIONE- CAP
326000140919     C*---
326100140919     C                   MOVEL     'S'           KTTDSF
326200140919     C                   MOVEL     PARCTO        KTTCTO
326300140919     C                   Z-ADD     PARKOV        KTTKSC
326400140919     C                   Z-ADD     PARCTR        KTTCTR
326500140919     C                   Z-ADD     PARPRG        KTTPRG
326600140919     C                   Z-ADD     SORG(I)       KTTLNP
326700140919     C                   movel     Sntw(I)       KTTntw
326800140919     C                   MOVEL     'E'           KTTFIE
326900140919     C     KE2ETT1       SETLL     TEETT11L
327000140919     C     KE2ETT1       READE     TEETT11L                               99
327100140919DO  5C     *IN99         DOWEQ     *OFF
327200140919IF  6C     ETTATB        IFEQ      *BLANKS
327300140919     C                   MOVEL     'A'           ETTATB
327400140919     C                   MOVEL     *BLANKS       ETTFTR
327500140919     C                   UPDATE    TEETT100
327600140919E   6C                   ENDIF
327700140919     C     KE2ETT1       READE     TEETT11L                               99
327800140919E   5C                   ENDDO                                                  *FINE COD.TASSAZIONE
327900970313     C*
328000970314E   2C                   ENDIF
328100970314E   1C                   ENDDO                                                  *FINE FILIALI
328200970314     C*
328300970313     C                   ENDSR
328400970110     C*------------------------------------------------------------------------*
328500970110     C* OPEFIL - APRE GLI ARCHIVI
328600970110     C*------------------------------------------------------------------------*
328700970110     C     OPEFIL        BEGSR
328800970110     C*
328900970110IF  1C     PARKSC        IFGT      *ZEROS
329000990812     C                   OPEN      TITAS31C                                     *LETTURA X CLIENTE
329100970110X   1C                   ELSE
329200990812     C                   OPEN      TITAS38C                                     *LETTURA X DATA
329300970110E   1C                   ENDIF
329400970110     C*
329500970110     C                   ENDSR
329600970110     C*------------------------------------------------------------------------*
329700970110     C* CLOFIL - CHIUDE GLI ARCHIVI
329800970110     C*------------------------------------------------------------------------*
329900970110     C     CLOFIL        BEGSR
330000970110     C*
330100970110IF  1C     PARKSC        IFGT      *ZEROS
330200990812     C                   CLOSE     TITAS31C                                     *LETTURA X CLIENTE
330300970110X   1C                   ELSE
330400990812     C                   CLOSE     TITAS38C                                     *LETTURA X DATA
330500970110E   1C                   ENDIF
330600970110     C*
330700970110     C                   ENDSR
330800011011     C*------------------------------------------------------------------------*
330900011011     C* CNVEUR - CONVERTE I CAMPI IN DIVISA DI CALCOLO DEL CE
331000011011     C*------------------------------------------------------------------------*
331100011011     C     CNVEUR        BEGSR
331200011011     C*
331300011011     C* AZZERA VARIABILI DI LAVORO
331400011011     C                   Z-ADD     *ZEROS        WIMO             30 9          *IMPORTO CONVERTITO
331500011011     C                   MOVEL     *BLANKS       NDEC              1            *N. DEC IN OUTPUT
331600011026     C                   EVAL      NDEC = '9'
331700011011     C*
331800011011IF  1C     WIMI          IFNE      *ZEROS
331900011011     C                   CLEAR                   YEUR
332000011011     C                   MOVEL     WDVI          YECDVI                         *DIVISA IMPORTO
332100011011     C                   Z-ADD     WIMI          YECIMI                         *IMPORTO DA CONVERTI
332200011011     C                   MOVEL     PARDIV        YECDVO                         *DIVISA IMP CONVERTI
332300011011     C                   MOVEL     NDEC          YECDCO                         *N° DECIMALI
332400011011     C                   MOVEL     *BLANKS       YECTAR                         *ARROTONDAMENTO "H"
332500011011     C                   CALL      'YEURCO'
332600011011     C                   PARM                    YEUR
332700011011IF  2C     YECESI        IFEQ      ' '                                          *NO ERRORI
332800011011     C                   Z-ADD     YECIMO        WIMO                            -IMPORTO CONVERTITO
332900011011X   2C                   ELSE                                                   *SI ERRORI
333000011011     C                   Z-ADD     WIMI          WIMO                            -IMPOROTO DA CONVER
333100011011E   2C                   ENDIF
333200011011E   1C                   ENDIF
333300011011     C*
333400011011     C                   ENDSR
333500011011     C*------------------------------------------------------------------------*
333600011011     C* CARGEDCE - CARICAMENTO TABELLA "GED" TIPO RECORD "CE" E CONSIDERAZIONI SU DIVISE
333700011011     C*------------------------------------------------------------------------*
333800011011     C     CARGEDCE      BEGSR
333900011011     C*
334000011011     C                   CLEAR                   DSGEDCE
334100011026     C                   MOVEL     *blanks       WDIVCE            3
334200011011     C*
334300011011     C                   MOVEL(P)  'GED'         KTECOD
334400011011     C                   MOVEL(P)  'CE'          KTEKE1
334500011011     C     KEYTBE        CHAIN     TNTBE01L
334600011011     C                   IF        %found(TNTBE01L)
334700011011     C                   MOVEL     TBEUNI        DSGEDCE
334800011011     C                   ENDIF
334900011011     C*
335000011011     C                   IF        §GEDCEDAT > DATCOR
335100011026     C                   EVAL      WDIVCE = §GEDCEDIP
335200011026     C                   ELSE
335300011026     C                   EVAL      WDIVCE = §GEDCEDIV
335400011011     C                   ENDIF
335500011011     C*
335600011011     C                   ENDSR
335700970110     C*------------------------------------------------------------------------*
335800970110     C* CARTAB - CARICAMENTO TABELLE
335900970110     C*------------------------------------------------------------------------*
336000970110     C     CARTAB        BEGSR
336100970110     C*--------------------
336200970110     C* ORGANIGRAMMA
336300970110     C*--------------------
336400020227     C                   Z-ADD     *ZEROS        JJ                3 0
336500970110     C                   Z-ADD     *ZEROS        I                 4 0
336600970110     C     *LOVAL        SETLL     AZORG01L
336700970110     C                   READ      AZORG01L                               99
336800970110DO  1C     *IN99         DOWEQ     *OFF
336900970110IF  2C     ORGFVA        IFEQ      *BLANKS                                      *NO ANNULLATE
337000970110     C     ORGFAG        ANDNE     'V'                                          *NO VARIE
337100970110     C                   ADD       1             I
337200970110     C                   Z-ADD     ORGFIL        ORG(I)                         *FILIALE
337300970110     C                   MOVEL     ORGDES        DORG(I)                        *DESCRIZIONE
337400020730     C                   MOVEL     ORGDE3        OG143
337500020730     C*
337600020730     C* TESTO IL NETWORK X STABILIRE SE TRATTASI DI FILIALE ITALIA O ESTERO
337700020730     C                   CLEAR                   DNTW
337800020730     C*
337900020730     C                   MOVEL(P)  'NTW'         KTECOD
338000020730     C                   MOVEL(P)  §OGNTW        KTEKE1
338100020730     C     KEYTBE        CHAIN     TNTBE01L
338200020730     C                   IF        %found(TNTBE01L)
338300020730     C                   MOVEL     TBEUNI        DNTW
338400020730     C                   ENDIF
338500020730     C*
338600020730     C                   MOVEL     §NTWFIE       FL1(I)                         *FLAG ITALIA/ESTERO
338700140919     C                   MOVEL     ORGDE3        OG143
338800970110     C* SELEZIONI
338900970430IF  3C     PARLNP        IFEQ      999                                          *TUTTE
339000970430     C                   Z-ADD     ORGFIL        SORG(I)                        *FILIALI SELEZIONATE
339100020730     C                   MOVEL     §NTWFIE       SFL1(I)                        *FLAG ITALIA/ESTERO
339200140919     C                   movel     §OGNTW        sntw(I)
339300970110X   3C                   ELSE
339400970110IF  4C     PARLNP        IFEQ      ORGFIL                                       *UNA FILIALE
339500970417     C                   Z-ADD     ORGFIL        SORG(1)                        *FILIALE SELEZIONATA
339600020730     C                   MOVEL     §NTWFIE       SFL1(1)                        *FLAG ITALIA/ESTERO
339700140919     C                   movel     §OGNTW        sntw(1)
339800970110E   4C                   ENDIF
339900970110E   3C                   ENDIF
340000081105     C* VERIFICO SE FILIALE dpd
340100140919     C                   IF        §OGNTW = 'DPD'
340200081105     C                   ADD       1             xx                3 0
340300081105     C                   MOVEL     ORGFIL        FILdpd(xx)
340400020227     C                   ENDIF
340500081105     C* VERIFICO SE FILIALE FEDEX
340600081105     C                   MOVEL     ORGDE3        OG143
340700081105     C                   IF        §OGNTW = 'FED'
340800081105     C                   ADD       1             JJ
340900081105     C                   MOVEL     ORGFIL        FILFED(JJ)
341000081105     C                   ENDIF
341100970110E   2C                   ENDIF
341200970110     C                   READ      AZORG01L                               99
341300970110E   1C                   ENDDO
341400970423     C*--------------------
341500970423     C* TARIFFA ITALIA/ESTERO
341600970423     C*--------------------
341700970423IF  1C     PARFIE        IFEQ      *BLANKS                                      *ITALIA E ESTERO
341800970423     C                   MOVEL     'I'           SFIE(1)
341900970423     C                   MOVEL     'E'           SFIE(2)
342000970423X   1C                   ELSE                                                   *ITALIA O ESTERO
342100970423     C                   MOVEL     PARFIE        SFIE(1)
342200970423E   1C                   ENDIF
342300970110     C*--------------------
342400970110     C* TIPI TARIFFA
342500970110     C*--------------------
342600970110     C                   Z-ADD     *ZEROS        I                 4 0
342700020923     C                   Z-ADD     1             KTBKUT
342800970110     C                   MOVEL     'TR'          KTBCOD
342900970110     C     KE2TAB        SETLL     TABEL00F
343000970110     C     KE2TAB        READE     TABEL00F                               99
343100970110DO  1C     *IN99         DOWEQ     *OFF
343200970110IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
343300970110     C                   MOVEL     TBLUNI        DSTR
343400970110IF  3C     §TRUTE        IFEQ      'S'                                          *IN GESTIONE
343500970110     C                   ADD       1             I
343600970110     C                   MOVEL     TBLKEY        TTR(I)                         *CODICE
343700970110     C                   MOVEL     §TRDES        DTTR(I)                        *DESCRIZIONE
343800970110E   3C                   ENDIF
343900970110E   2C                   ENDIF
344000970110     C     KE2TAB        READE     TABEL00F                               99
344100970110E   1C                   ENDDO
344200050808     C*--------------------
344300050808     C* cod. trattamento merce con packing list
344400050808     C*--------------------
344500050808     C                   Z-ADD     *ZEROS        I                 4 0
344600050808     C                   Z-ADD     1             KTBKUT
344700050808     C                   MOVEL     '1B'          KTBCOD
344800050808     C     KE2TAB        SETLL     TABEL00F
344900050808     C     KE2TAB        READE     TABEL00F                               99
345000050808DO  1C     *IN99         DOWEQ     *OFF
345100050808IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NO ANNULLATI
345200050808     C                   MOVEL     TBLUNI        DS1b
345300050808IF  3C     §1bf16        IFEQ      'S'                                          *IN GESTIONE
345400050808     C                   ADD       1             I
345500050808     C                   MOVEL     TBLKEY        cpkl(I)                        *CODICE
345600050808E   3C                   ENDIF
345700050808E   2C                   ENDIF
345800050808     C     KE2TAB        READE     TABEL00F                               99
345900050808E   1C                   ENDDO
346000970110     C*--------------------
346100970110     C* RAGGRUPPAMENTI TIPO BOLLA
346200970110     C*--------------------
346300970110     C                   Z-ADD     *ZEROS        I                 4 0
346400020923     C                   Z-ADD     1             KTBKUT
346500970110     C                   MOVEL     '2W'          KTBCOD
346600970110     C     KE2TAB        SETLL     TABEL00F
346700970110     C     KE2TAB        READE     TABEL00F                               99
346800970110DO  1C     *IN99         DOWEQ     *OFF
346900970110IF  2C     TBLFLG        IFEQ      *BLANKS
347000970110     C                   MOVEL     TBLUNI        DS2W
347100970110     C                   ADD       1             I
347200970110     C                   MOVEL     TBLKEY        RBL(I)                         *RAGGRUPPAMENTO BOLL
347300970110     C                   MOVEL     §2WFSP        FSP(I)                         *FLAG SPEDIZIONE
347400970110     C                   MOVEL     §2WFRI        FRI(I)                         *FLAG RICAVO
347500970110E   2C                   ENDIF
347600970110     C     KE2TAB        READE     TABEL00F                               99
347700970110E   1C                   ENDDO
347800970110     C*--------------------
347900970110     C* TIPI BOLLA
348000970110     C*--------------------
348100970110     C                   Z-ADD     *ZEROS        I                 4 0
348200970110     C                   Z-ADD     *ZEROS        J                 4 0
348300020923     C                   Z-ADD     1             KTBKUT
348400970110     C                   MOVEL     'TB'          KTBCOD
348500970110     C     KE2TAB        SETLL     TABEL00F
348600970110     C     KE2TAB        READE     TABEL00F                               99
348700970110DO  1C     *IN99         DOWEQ     *OFF
348800970110IF  2C     TBLFLG        IFEQ      *BLANKS
348900970110     C                   MOVEL     TBLUNI        DSTB
349000970110     C                   ADD       1             I
349100970110     C                   MOVEL     TBLKEY        TBL(I)                         *TIPO BOLLA
349200970110     C                   Z-ADD     1             J
349300970110     C                   SETOFF                                           98
349400970110     C     §TBRBL        LOOKUP    RBL(J)                                 98
349500970110IF  3C     *IN98         IFEQ      *ON
349600970110     C                   MOVEL     FSP(J)        SPD(I)                         *FLAG SPEDIZIONE
349700970110     C                   MOVEL     FRI(J)        RIC(I)                         *FLAG RICAVO
349800970110X   3C                   ELSE
349900970110     C                   MOVEL     'N'           SPD(I)                         *FLAG SPEDIZIONE
350000970110     C                   MOVEL     'N'           RIC(I)                         *FLAG RICAVO
350100970110E   3C                   ENDIF
350200970110E   2C                   ENDIF
350300970110     C     KE2TAB        READE     TABEL00F                               99
350400970110E   1C                   ENDDO
350500970110     C*--------------------
350600970110     C* CODICI PAGAMENTO
350700970110     C*--------------------
350800970110     C                   Z-ADD     *ZEROS        I                 4 0
350900020923     C                   Z-ADD     1             KTBKUT
351000970110     C                   MOVEL     'FA'          KTBCOD
351100970110     C     KE2TAB        SETLL     TABEL00F
351200970110     C     KE2TAB        READE     TABEL00F                               99
351300970110DO  1C     *IN99         DOWEQ     *OFF
351400970110IF  2C     TBLFLG        IFEQ      *BLANKS
351500970110     C                   MOVEL     TBLUNI        DSFA
351600970110     C                   ADD       1             I
351700970110     C                   MOVEL     TBLKEY        CDP(I)                         *CODICE PAGAMENTO
351800970110     C                   MOVEL     §FARAT        CRAT(I)                        *RATE DI PAGAMENTO
351900970110E   2C                   ENDIF
352000970110     C     KE2TAB        READE     TABEL00F                               99
352100970110E   1C                   ENDDO
352200970313     C*--------------------
352300970313     C* CODICI TASSAZIONE
352400970313     C*--------------------
352500970313     C                   Z-ADD     *ZEROS        I                 4 0
352600020923     C                   Z-ADD     1             KTBKUT
352700970313     C                   MOVEL     'CT'          KTBCOD
352800970313     C     KE2TAB        SETLL     TABEL00F
352900970313     C     KE2TAB        READE     TABEL00F                               99
353000970313DO  1C     *IN99         DOWEQ     *OFF
353100970313IF  2C     TBLFLG        IFEQ      *BLANKS                                      *NON ANNULLATI
353200970313     C                   MOVEL     TBLUNI        DSCT
353300970317IF  3C     §CTUTC        IFEQ      'S'                                          *DA CONSIDERARE
353400970317     C                   ADD       1             I
353500970317     C                   MOVEL     TBLKEY        CTS(I)                         *CODICE
353600970317     C                   MOVEL     §CTDES        DCTS(I)                        *DESCRIZIONE
353700970317     C                   MOVEL     §CTFCR        FCR(I)                         *FLAG CAPOLUOGO
353800970317     C                   MOVEL     §CTRAP        RAP(I)                         *REGIONE
353900140919     C                   MOVEL     §CTnar        nar(I)                         *REGIONE
354000140919     C                   MOVEL     §CTnar        naz(I)                         *REGIONE
354100970317     C                   MOVE      §CTCOR        WA1               1            *TIPO CODICE
354200970417IF  4C     WA1           IFEQ      '9'
354300970417IF  5C     CTS(I)        IFEQ      'IT'
354400970317     C     CTS(I)        OREQ      'EE'
354500970317     C                   MOVEL     'N'           TCT(I)                         *TIPO "NAZIONE"
354600970417X   5C                   ELSE
354700970317     C                   MOVEL     'R'           TCT(I)                         *TIPO "REGIONE"
354800970417E   5C                   ENDIF
354900970417X   4C                   ELSE
355000970317     C                   MOVEL     'E'           TCT(I)                         *TIPO "ELEMENTARE"
355100970417E   4C                   ENDIF
355200970417IF  4C     §CTNAR        IFEQ      *BLANKS
355300970317     C                   MOVEL     'IT'          NAR(I)                         *NAZIONE ITALIA
355400970317     C                   MOVEL     'I'           CIE(I)                         *CODICE ITALIA
355500970417X   4C                   ELSE
355600970317     C                   MOVEL     'EE'          NAR(I)                         *NAZIONE ESTERO
355700970317     C                   MOVEL     'E'           CIE(I)                         *CODICE ESTERO
355800970417E   4C                   ENDIF
355900970317E   3C                   ENDIF
356000970317E   2C                   ENDIF
356100970317     C     KE2TAB        READE     TABEL00F                               99
356200970317E   1C                   ENDDO
356300040920     C* CARICO GLI SCAGLIONI DI DEFAULT
356400040920IF  1C     PARTTR        IFEQ      '0'                                          *QUINTALI
356500040920     C     PARTTR        OREQ      '3'                                          *KG
356600040920     C                   Z-ADD     2             SGL(1)
356700040920     C                   Z-ADD     5             SGL(2)
356800040920     C                   Z-ADD     10            SGL(3)
356900040920     C                   Z-ADD     25            SGL(4)
357000040920     C                   Z-ADD     50            SGL(5)
357100040920     C                   Z-ADD     75            SGL(6)
357200040920     C                   Z-ADD     100           SGL(7)
357300040920     C                   Z-ADD     99999         SGL(8)
357400040920X   1C                   ELSE                                                   *COLLI
357500040920     C                   Z-ADD     1             SGL(1)
357600040920     C                   Z-ADD     2             SGL(2)
357700040920     C                   Z-ADD     3             SGL(3)
357800040920     C                   Z-ADD     4             SGL(4)
357900040920     C                   Z-ADD     99999         SGL(5)
358000040920E   1C                   ENDIF
358100970110     C*
358200970110     C                   ENDSR
358300970313     C*------------------------------------------------------------------------*
358400970313     C* CARTAR - CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
358500970313     C*------------------------------------------------------------------------*
358600970313     C     CARTAR        BEGSR
358700970313     C*
358800970313     C* REPERISCE IL CODICE DELLA TARIFFA DI CARTELLO IN VIGORE
358900970314IF  1C     $ERR          IFEQ      '0'                                          *NO ERRORE
359000021202     C                   Z-ADD     *ZEROS        CARKSC
359100021202     C                   Z-ADD     *ZEROS        CARCTR
359200021202     C                   MOVE(P)   *ZEROS        CARPRG
359300021202     C                   CLEAR                   TRUL27DS
359400021202     C                   EVAL      I27TLA = 'L'
359500021202     C                   EVAL      I27DTA = DATCOR
359600021202     C                   CALL      'TRUL27R'
359700021202     C                   PARM                    TRUL27DS
359800021202     C*
359900021202     C                   IF        O27ERR <> 'E'
360000021202     C                   Z-ADD     O27KSC        CARKSC
360100021202     C                   Z-ADD     O27CTR        CARCTR
360200021202     C                   MOVE(P)   O27PRG        CARPRG
360300021202     C                   ENDIF
360400021202E   1C                   ENDIF
360500970317     C*
360600970317     C* PER CIASCUNA FILIALE RICHIESTA
360700971124DO  1C     1             DO        500           Y                 4 0
360800970417IF  2C     SORG(Y)       IFGT      *ZEROS
360900970423     C     Y             OCCUR     FTAR
361000970317     C*---
361100970317     C* CARICA GLI SCAGLIONI DA CONSIDERARE -CLIENTE O CARTELLO-
361200970317     C*---
361300970416     C                   Z-ADD     *ZEROS        SSGL
361400970313     C                   Z-ADD     PARKOV        KTDKSC
361500970313     C                   Z-ADD     PARCTR        KTDCTR
361600970313     C                   Z-ADD     PARPRG        KTDPRG
361700970417     C                   Z-ADD     SORG(Y)       KTDLNP
361800970313     C                   EXSR      CARSGL
361900970417IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
362000970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
362100970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
362200970417     C                   Z-ADD     CARKSC        KTDKSC
362300970313     C                   Z-ADD     CARCTR        KTDCTR
362400970313     C                   Z-ADD     CARPRG        KTDPRG
362500970417     C                   Z-ADD     SORG(Y)       KTDLNP
362600970314     C                   EXSR      CARSGL
362700970703     C                   MOVEL     DEPCTO        PARCTO
362800970418E   3C                   ENDIF
362900970418IF  3C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
363000040920     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
363100040920     c                   move      sgl           ssgl
363200970418E   3C                   ENDIF
363300970317     C*---
363400970313     C* CARICA I CODICI TASSAZIONE DA CONSIDERARE -CLIENTE O CARTELLO-
363500970317     C*---
363600970423     C                   MOVEL     *BLANKS       CCTI
363700970423     C                   MOVEL     *BLANKS       CRCI
363800970423     C                   MOVEL     *BLANKS       CCTE
363900970423     C                   MOVEL     *BLANKS       CRCE
364000970423DO  3C     1             DO        2             W                 4 0
364100970423IF  4C     SFIE(W)       IFNE      *BLANKS
364200970314     C                   Z-ADD     PARKOV        KTDKSC
364300970314     C                   Z-ADD     PARCTR        KTDCTR
364400970314     C                   Z-ADD     PARPRG        KTDPRG
364500970417     C                   Z-ADD     SORG(Y)       KTDLNP
364600971204     C                   MOVEL     SFL1(Y)       DLPFL1
364700970314     C                   EXSR      CARCTS
364800970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
364900970703     C                   MOVEL     PARCTO        DEPCTO                         *DEPOSITO OFF/TAR
365000970703     C                   MOVEL     'T'           PARCTO                         *CARTELLO
365100970314     C                   Z-ADD     CARKSC        KTDKSC
365200970314     C                   Z-ADD     CARCTR        KTDCTR
365300970314     C                   Z-ADD     CARPRG        KTDPRG
365400970417     C                   Z-ADD     SORG(Y)       KTDLNP
365500971204     C                   MOVEL     SFL1(Y)       DLPFL1
365600970314     C                   EXSR      CARCTS
365700970703     C                   MOVEL     DEPCTO        PARCTO
365800970423E   5C                   ENDIF
365900970423IF  5C     $TAD          IFEQ      '0'                                          *DETTAGLIO NO TROVAT
366000970418     C                   EXSR      FIXCTS                                       *IMPOSTA FISSI CTS
366100970423E   5C                   ENDIF
366200970416     C                   EXSR      RAPSVI                                       *CTS->CTS ELEMENTARI
366300970423E   4C                   ENDIF
366400970423E   3C                   ENDDO                                                  *FINE TARIFFA ITA/ES
366500970417     C*
366600970417E   2C                   ENDIF
366700970417E   1C                   ENDDO                                                  *FINE FILIALI
366800970313     C*
366900970313     C                   ENDSR
367000970313     C*------------------------------------------------------------------------*
367100970313     C* CARSGL - CARICA GLI SCAGLIONI
367200970313     C*------------------------------------------------------------------------*
367300970313     C     CARSGL        BEGSR
367400970313     C*
367500970313     C* AZZERA LE VARIABILI DI LAVORO
367600970313     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
367700970313     C                   Z-ADD     *ZEROS        I                 4 0
367800970313     C*
367900011128     C                   CLEAR                   DEPORP
368000060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
368100990820     C     KEYTAD        SETLL     TITAD04L                               99
368200990820     C     KEYTAD        READE     TITAD04L                               99
368300060804X   1C                   ELSE                                                   *OFFERTA
368400060804     C     KEYTAD        SETLL     TIOFD01L                               99
368500060804     C     KEYTAD        READE     TIOFD01L                               99
368600060804E   1C                   ENDIF
368700011128     C                   EVAL      DEPORP = TADORP                              *SOLO un cts
368800011010DO  1C     *IN99         DOWEQ     *OFF                                         *FINE FILE
368900011128     C     TADORP        ANDEQ     DEPORP                                       *ROTTURA ORDINAMENTO
369000011010IF  2C     TADATB        IFEQ      *BLANKS
369100011010     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
369200011010     C     TADSGL        LOOKUP    SSGL                                   98
369300011010IF  3C     *IN98         IFEQ      *OFF
369400030122     C                   IF        I < 10
369500011010     C                   ADD       1             I
369600011010     C                   Z-ADD     TADSGL        SSGL(I)
369700030122     C                   ELSE
369800030122     C                   LEAVE
369900030122     C                   ENDIF
370000011010E   3C                   ENDIF
370100011010E   2C                   ENDIF
370200060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
370300990820     C     KEYTAD        READE     TITAD04L                               99
370400060804X   2C                   ELSE                                                   *OFFERTA
370500060804     C     KEYTAD        READE     TIOFD01L                               99
370600060804E   2C                   ENDIF
370700970313E   1C                   ENDDO                                                  *FINE DETTAGLIO
370800970313     C*
370900970313     C                   ENDSR
371000970314     C*------------------------------------------------------------------------*
371100970314     C* CARCTS   CARICA I CODICI TASSAZIONE
371200970314     C*------------------------------------------------------------------------*
371300970314     C     CARCTS        BEGSR
371400970314     C*
371500970314     C* AZZERA LE VARIABILI DI LAVORO
371600970314     C                   MOVEL     '0'           $TAD                           *DETTAGLIO NO TROVAT
371700970314     C                   Z-ADD     *ZEROS        I                 4 0
371800970317     C                   CLEAR                   WCTS
371900970317     C                   CLEAR                   WRAP
372000970317     C                   CLEAR                   WNAR
372100970317     C                   CLEAR                   WTCT
372200970314     C*
372300970314     C* MEMORIZZA I CODICI DELLA LINEA DI PARTENZA RICHIESTA
372400060804IF  1C     PARCTO        IFEQ      'T'                                          *TARIFFA
372500990820     C     KEYTAD        SETLL     TITAD04L                               99
372600990820     C     KEYTAD        READE     TITAD04L                               99
372700060804X   1C                   ELSE                                                   *OFFERTA
372800060804     C     KEYTAD        SETLL     TIOFD01L                               99
372900060804     C     KEYTAD        READE     TIOFD01L                               99
373000060804E   1C                   ENDIF
373100970314DO  1C     *IN99         DOWEQ     *OFF
373200970314IF  2C     TADATB        IFEQ      *BLANKS                                      *NON ANNULLATI
373300970314     C                   SETOFF                                           98
373400970314     C                   Z-ADD     1             J                 4 0
373500970314     C     TADCTS        LOOKUP    CTS(J)                                 98
373600970314IF  3C     *IN98         IFEQ      *ON                                          *TROVATO
373700970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
373800970417     C     CIE(J)        ANDEQ     'I'                                          *CODICI ITALIA
373900970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
374000971204     C     DLPFL1        ANDEQ     *BLANKS                                      *PARTENZA ITALIA
374100970417     C     CIE(J)        ANDEQ     'E'                                          *CODICI ESTERO
374200970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
374300971204     C     DLPFL1        ANDNE     *BLANKS                                      *PARTENZA ITALIA
374400970314     C                   SETOFF                                           98
374500970314     C                   Z-ADD     1             K                 4 0
374600970314     C     TADCTS        LOOKUP    WCTS(K)                                98
374700970417IF  5C     *IN98         IFEQ      *OFF                                         *MAI MEMORIZZATO
374800970417     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
374900970314     C                   ADD       1             I
375000970314     C                   MOVEL     TADCTS        WCTS(I)                        *CODICE
375100970314     C                   MOVEL     RAP(J)        WRAP(I)                        *REGIONE
375200970314     C                   MOVEL     NAR(J)        WNAR(I)                        *NAZIONE
375300970314     C                   MOVEL     TCT(J)        WTCT(I)                        *TIPO
375400970417E   5C                   ENDIF
375500970417E   4C                   ENDIF
375600970314E   3C                   ENDIF
375700970314E   2C                   ENDIF
375800060804IF  2C     PARCTO        IFEQ      'T'                                          *TARIFFA
375900990820     C     KEYTAD        READE     TITAD04L                               99
376000060804X   2C                   ELSE                                                   *OFFERTA
376100060804     C     KEYTAD        READE     TIOFD01L                               99
376200060804E   2C                   ENDIF
376300970314E   1C                   ENDDO
376400970314     C*
376500970314     C                   ENDSR
376600970421     C*------------------------------------------------------------------------*
376700970421     C* FIXCTS - IMPOSTA CODICI TASSAZIONE FISSI
376800970421     C*------------------------------------------------------------------------*
376900970421     C     FIXCTS        BEGSR
377000970421     C*
377100970421     C                   MOVEL     '1'           $TAD                           *DETTAGLIO TROVATO
377200970421     C*
377300970421     C                   Z-ADD     *ZEROS        J                 4 0
377400050103DO  1C     1             DO        600           I
377500970421IF  2C     CTS(I)        IFNE      *BLANKS
377600970421IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
377700970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
377800970421     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
377900970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
378000971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
378100970421     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
378200970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
378300971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
378400970421     C                   ADD       1             J
378500970421     C                   MOVEL     CTS(I)        WCTS(J)                        *CODICE
378600970421     C                   MOVEL     RAP(I)        WRAP(J)                        *REGIONE
378700970421     C                   MOVEL     NAR(I)        WNAR(J)                        *NAZIONE
378800970421     C                   MOVEL     TCT(I)        WTCT(J)                        *TIPO
378900970421E   4C                   ENDIF
379000970421E   3C                   ENDIF
379100970421E   2C                   ENDIF
379200970421E   1C                   ENDDO
379300970421     C*
379400970421     C                   ENDSR
379500970314     C*------------------------------------------------------------------------*
379600970314     C* RAPSVI - TRASFORMA I CODICI DELLA TARIFFA IN CODICI ELEMENTARI
379700970314     C*------------------------------------------------------------------------*
379800970314     C     RAPSVI        BEGSR
379900970314     C*
380000970314     C* 1°GIRO-> CARICA TUTTI I CODICI ELEMENTARI COME -NON CALCOLARE-
380100970314     C                   Z-ADD     *ZEROS        J                 4 0
380200050103DO  1C     1             DO        600           I
380300970416IF  2C     CTS(I)        IFNE      *BLANKS
380400970416IF  3C     TCT(I)        IFEQ      'E'                                          *SOLO ELEMENTARI
380500970423IF  4C     SFIE(W)       IFEQ      'I'                                          *TARIFFA ITALIA
380600970417     C     CIE(I)        ANDEQ     'I'                                          *CODICI ITALIA
380700970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
380800971204     C     SFL1(Y)       ANDEQ     *BLANKS                                      *PARTENZA ITALIA
380900970417     C     CIE(I)        ANDEQ     'E'                                          *CODICI ESTERO
381000970423     C     SFIE(W)       OREQ      'E'                                          *TARIFFA ESTERO
381100971204     C     SFL1(Y)       ANDNE     *BLANKS                                      *PARTENZA ESTERO
381200970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
381300970314     C                   ADD       1             J
381400970423     C                   MOVEL     CTS(I)        CCTI(J)                        *CODICE
381500970423     C                   MOVEL     *BLANKS       CRCI(J)                        *RAGGRUPPAMENTO
381600970423X   5C                   ELSE                                                   *ESTERO
381700970423     C                   ADD       1             J
381800970423     C                   MOVEL     CTS(I)        CCTE(J)                        *CODICE
381900970423     C                   MOVEL     *BLANKS       CRCE(J)                        *RAGGRUPPAMENTO
382000970423E   5C                   ENDIF
382100970423E   4C                   ENDIF
382200970417E   3C                   ENDIF
382300970314E   2C                   ENDIF
382400970314E   1C                   ENDDO                                                  *FINE CODICI BASE
382500970314     C*
382600970314     C* 2°GIRO-> TRASFORMA I -NON CALCOLARE- IN -CALCOLARE IN NAZIONE-
382700050103DO  1C     1             DO        600           I
382800970314IF  2C     WCTS(I)       IFNE      *BLANKS
382900970314IF  3C     WTCT(I)       IFEQ      'N'                                          *SOLO NAZIONE
383000050103DO  4C     1             DO        600           J
383100970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
383200970423IF  6C     CCTI(J)       IFNE      *BLANKS
383300970314     C                   SETOFF                                           98
383400970314     C                   Z-ADD     1             K                 4 0
383500970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
383600970423IF  7C     WNAR(I)       IFEQ      NAR(K)
383700970423     C                   MOVEL     'N'           CRCI(J)
383800970423E   7C                   ENDIF
383900970423E   6C                   ENDIF
384000970423X   5C                   ELSE                                                   *ESTERO
384100970423IF  6C     CCTE(J)       IFNE      *BLANKS
384200970423     C                   SETOFF                                           98
384300970423     C                   Z-ADD     1             K                 4 0
384400970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
384500970423IF  7C     WNAR(I)       IFEQ      NAR(K)
384600970423     C                   MOVEL     'N'           CRCE(J)
384700970423E   7C                   ENDIF
384800970423E   6C                   ENDIF
384900970423E   5C                   ENDIF
385000970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
385100970314E   3C                   ENDIF
385200970314E   2C                   ENDIF
385300970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
385400970314     C*
385500970314     C* 3°GIRO-> TRASFORMA I -CALCOLARE IN NAZIONE- IN -CALCOLARE IN REGIONE-
385600050103DO  1C     1             DO        600           I
385700970314IF  2C     WCTS(I)       IFNE      *BLANKS
385800970314IF  3C     WTCT(I)       IFEQ      'R'                                          *SOLO REGIONE
385900050103DO  4C     1             DO        600           J
386000970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
386100970423IF  6C     CCTI(J)       IFNE      *BLANKS
386200970314     C                   SETOFF                                           98
386300970314     C                   Z-ADD     1             K                 4 0
386400970423     C     CCTI(J)       LOOKUP    CTS(K)                                 98
386500970423IF  7C     WRAP(I)       IFEQ      RAP(K)
386600970423     C                   MOVEL     'R'           CRCI(J)
386700970423E   7C                   ENDIF
386800970423E   6C                   ENDIF
386900970423X   5C                   ELSE                                                   *ESTERO
387000970423IF  6C     CCTE(J)       IFNE      *BLANKS
387100970423     C                   SETOFF                                           98
387200970423     C                   Z-ADD     1             K                 4 0
387300970423     C     CCTE(J)       LOOKUP    CTS(K)                                 98
387400970423IF  7C     WRAP(I)       IFEQ      RAP(K)
387500970423     C                   MOVEL     'R'           CRCE(J)
387600970423E   7C                   ENDIF
387700970423E   6C                   ENDIF
387800970314E   5C                   ENDIF
387900970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
388000970314E   3C                   ENDIF
388100970314E   2C                   ENDIF
388200970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
388300970314     C*
388400970314     C* 4°GIRO-> TRASFORMA I -CALCOLARE IN REGIONE- IN -CALCOLARE-
388500050103DO  1C     1             DO        600           I
388600970314IF  2C     WCTS(I)       IFNE      *BLANKS
388700970314IF  3C     WTCT(I)       IFEQ      'E'                                          *SOLO ELEMENTARI
388800050103DO  4C     1             DO        600           J
388900970423IF  5C     SFIE(W)       IFEQ      'I'                                          *ITALIA
389000970423IF  6C     CCTI(J)       IFNE      *BLANKS
389100970423IF  7C     WCTS(I)       IFEQ      CCTI(J)
389200970423     C                   MOVEL     'T'           CRCI(J)
389300970423E   7C                   ENDIF
389400970423E   6C                   ENDIF
389500970423X   5C                   ELSE                                                   *ESTERO
389600970423IF  6C     CCTE(J)       IFNE      *BLANKS
389700970423IF  7C     WCTS(I)       IFEQ      CCTE(J)
389800970423     C                   MOVEL     'T'           CRCE(J)
389900970423E   7C                   ENDIF
390000970423E   6C                   ENDIF
390100970423E   5C                   ENDIF
390200970314E   4C                   ENDDO                                                  *FINE COD.EL.TARIFFA
390300970314E   3C                   ENDIF
390400970314E   2C                   ENDIF
390500970314E   1C                   ENDDO                                                  *FINE CODICI TARIFFA
390600970314     C*
390700970314     C                   ENDSR
390800981218     C*------------------------------------------------------------------------*
390900981218     C* CANTAR - ANNULLAMENTO TARIFFA
391000981218     C*------------------------------------------------------------------------*
391100981218     C     CANTAR        BEGSR
391200981218     C*
391300981218     C* CANCELLA DATI TIPO - GENERALI
391400981218     C                   MOVEL     'S'           KTCDSF                         *DATI SEDE
391500060804     C                   MOVEL     parcto        KTCCTO                         *DATI TARIFFA
391600981218     C                   Z-ADD     PARKOV        KTCKSC                         *CLIENTE
391700011001     C     KE3ETC        SETLL     TEETC01L
391800011001     C     KE3ETC        READE     TEETC01L                               99
391900981218DO  1C     *IN99         DOWEQ     *OFF
392000981218IF  2C     PARCTR        IFEQ      ETCCTR                                       *TARIFFA RICHIESTA
392100981218IF  3C     PARPRG        IFEQ      ETCPRG                                       *PROGRESSIVO RICHIES
392200981218IF  4C     PARLNP        IFEQ      ETCLNP                                       *LINEA RICHIESTA
392300981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
392400981218     C                   MOVEL     'A'           ETCATB                         *ANNULLAMENTO
392500981218     C                   MOVEL     *BLANKS       ETCFTR                         *FLAG TRASMISSIONE
392600011128     C*---               Z-ADD     *ZEROS        ETCDTR                         *DATA TRASMISSIONE
392700011001     C                   UPDATE    TEETC000                                     *AGGIORNA
392800981218E   4C                   ENDIF
392900981218E   3C                   ENDIF
393000981218E   2C                   ENDIF
393100011001     C     KE3ETC        READE     TEETC01L                               99
393200981218E   1C                   ENDDO                                                  *FINE DATI GENERALI
393300981218     C*
393400981218     C* CANCELLA DATI TIPO - SCAGLIONI
393500060829     c* ricavi
393600981218     C                   MOVEL     'S'           KTSDSF                         *DATI SEDE
393700060804     C                   MOVEL     parcto        KTSCTO                         *DATI TARIFFA
393800981218     C                   Z-ADD     PARKOV        KTSKSC                         *CLIENTE
393900011001     C     KE3ETS        SETLL     TEETS01L
394000011001     C     KE3ETS        READE     TEETS01L                               99
394100981218DO  1C     *IN99         DOWEQ     *OFF
394200981218IF  2C     PARCTR        IFEQ      ETSCTR                                       *TARIFFA RICHIESTA
394300981218IF  3C     PARPRG        IFEQ      ETSPRG                                       *PROGRESSIVO RICHIES
394400981218IF  4C     PARLNP        IFEQ      ETSLNP                                       *LINEA RICHIESTA
394500981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
394600981218     C                   MOVEL     'A'           ETSATB                         *ANNULLAMENTO
394700981218     C                   MOVEL     *BLANKS       ETSFTR                         *FLAG TRASMISSIONE
394800011001     C                   UPDATE    TEETS000                                     *AGGIORNA
394900981218E   4C                   ENDIF
395000981218E   3C                   ENDIF
395100981218E   2C                   ENDIF
395200011001     C     KE3ETS        READE     TEETS01L                               99
395300981218E   1C                   ENDDO                                                  *FINE SCAGLIONI
395400060829     c* competetnze
395500060829     C     KE3ETS        SETLL     TEETS11L
395600060829     C     KE3ETS        READE     TEETS11L                               99
395700060829DO  1C     *IN99         DOWEQ     *OFF
395800060829IF  2C     PARCTR        IFEQ      £ETSCTR                                      *TARIFFA RICHIESTA
395900060829IF  3C     PARPRG        IFEQ      £ETSPRG                                      *PROGRESSIVO RICHIES
396000060829IF  4C     PARLNP        IFEQ      £ETSLNP                                      *LINEA RICHIESTA
396100060829     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
396200060829     C                   MOVEL     'A'           £ETSATB                        *ANNULLAMENTO
396300060829     C                   MOVEL     *BLANKS       £ETSFTR                        *FLAG TRASMISSIONE
396400060829     C                   UPDATE    TEETS100                                     *AGGIORNA
396500060829E   4C                   ENDIF
396600060829E   3C                   ENDIF
396700060829E   2C                   ENDIF
396800060829     C     KE3ETS        READE     TEETS11L                               99
396900060829E   1C                   ENDDO                                                  *FINE SCAGLIONI
397000981218     C*
397100981218     C* CANCELLA DATI TIPO - CODICI TASSAZIONE
397200981218     C                   MOVEL     'S'           KTTDSF                         *DATI SEDE
397300060804     C                   MOVEL     parcto        KTTCTO                         *DATI TARIFFA
397400981218     C                   Z-ADD     PARKOV        KTTKSC                         *CLIENTE
397500011001     C     KE3ETT        SETLL     TEETT01L
397600011001     C     KE3ETT        READE     TEETT01L                               99
397700981218DO  1C     *IN99         DOWEQ     *OFF
397800981218IF  2C     PARCTR        IFEQ      ETTCTR                                       *TARIFFA RICHIESTA
397900981218IF  3C     PARPRG        IFEQ      ETTPRG                                       *PROGRESSIVO RICHIES
398000981218IF  4C     PARLNP        IFEQ      ETTLNP                                       *LINEA RICHIESTA
398100981218     C     PARLNP        OREQ      999                                          *O TUTTE LE LINEE
398200981218IF  5C     PARFIE        IFEQ      ETTFIE                                       *ITA O EST RICHIESTO
398300981218     C     PARFIE        OREQ      *BLANKS                                      *TUTTO ITA E EST
398400981218     C                   MOVEL     'A'           ETTATB                         *ANNULLAMENTO
398500981218     C                   MOVEL     *BLANKS       ETTFTR                         *FLAG TRASMISSIONE
398600011001     C                   UPDATE    TEETT000                                     *AGGIORNA
398700981218E   5C                   ENDIF
398800981218E   4C                   ENDIF
398900981218E   3C                   ENDIF
399000981218E   2C                   ENDIF
399100011001     C     KE3ETT        READE     TEETT01L                               99
399200981218E   1C                   ENDDO                                                  *FINE CODICI TASSAZI
399300981218     C*
399400981218     C                   ENDSR
399500020923     C*--------------------------------------------------------------------------------------------*
399600020923     C* REPERISCE I DATI UTENTE
399700020923     C*--------------------------------------------------------------------------------------------*
399800020923     C     REPDATIUTE    BEGSR
399900020923     C*
400000020923     C* INIZIALIZZA VARIABILI DI WRK
400100020923     C                   CLEAR                   TIBS34DS
400200020923     C                   CLEAR                   AZUTEDS
400300020923     C                   CLEAR                   DDATIUTE
400400020923     C*
400500020923     C     *DTAARA       DEFINE    §azute        azuteds
400600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
400700020923     C                   IN(E)     *DTAARA
400800020923if  1C                   IF        %Error
400900020923     c                   EVAL      I34Tla = 'L'
401000020923     C                   CALL      'TIBS34R'
401100020923     C                   PARM                    Tibs34Ds
401200020923     C                   IN        *DTAARA
401300020923e   1C                   ENDIF
401400020923     C*
401500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
401600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
401700020923     C*
401800020923     C                   ENDSR
401900021031     C*------------------------------------------------------------------------*
402000021031     C* REPCLI - CARICAMENTO LISTA CLIENTI PADRI/FIGLI
402100021031     C*------------------------------------------------------------------------*
402200021031     C     REPCLI        BEGSR
402300021031     C*
402400021031     C* SE RICHIESTA ELABORAZIONE X CLIENTE UNIFICANTE REPERISCO LA LISTA DEI FIGLI
402500021031     C                   IF        PARUNI = 'S'
402600021031     C                   CLEAR                   TIBS10DS
402700021031     C                   EVAL      D10DRF = DATCOR                              *data riferimento
402800021031     C                   EVAL      D10TLE = 'ST'                                *tipo legame
402900021031     C                   EVAL      D10PAF = 'F'                                 *cerca il padre
403000021031     C                   Z-ADD     PARKSC        D10COD                         *cod cliente da tst
403100021031     C                   CALL      'TIBS10R'
403200021031     C                   PARM                    TIBS10DS
403300021031     C*
403400021031     C                   IF        D10ERR <> *blanks OR                         *No unif. o padre
403500021031     C                             D10COP =  *zeros
403600021031     C                   CLEAR                   SKCLI
403700021031     C                   Z-ADD     PARKSC        SKCLI(1)
403800021031     C                   ENDIF
403900021031     C*
404000021031     C                   ELSE
404100021031     C                   CLEAR                   SKCLI
404200021031     C                   Z-ADD     PARKSC        SKCLI(1)
404300021031     C                   ENDIF
404400021031     C*
404500021031     C                   ENDSR
404600970110     C*------------------------------------------------------------------------*
404700970110     C* *INZSR - OPERAZIONI INIZIALI
404800970110     C*------------------------------------------------------------------------*
404900970110     C     *INZSR        BEGSR
405000970110     C*--------------------
405100970110     C* RICEVIMENTO PARAMETRI
405200970110     C*--------------------
405300970110     C     *ENTRY        PLIST
405400970110     C                   PARM                    KPJBA
405500970110     C                   MOVEL     KPJBU         PARAM
405600021121     C*
405700021121     C* CARICA NUOVE TABELLE OCCORRENTI
405800021121     C                   EXSR      CARGEDCE
405900021121     C*
406000021121     C* REPERISCO SUBITO LA DIVISA DELLA TARIFFA RICEVUTA IN INPUT
406100021121     C                   IF        PARDIV = *BLANKS
406200021121     C                   Z-ADD     PARKOV        KTMKSC
406300021121     C                   Z-ADD     PARCTR        KTMCTR
406400021121     C                   Z-ADD     PARPRG        KTMPRG
406500021121     C     KEYTAMUNIQ    CHAIN     TNTAM01L
406600021121     C                   IF        %FOUND
406700021121     C                   MOVEL(P)  TAMFLO        DSTA01
406800021121     C                   MOVEL(P)  §TADIV        PARDIV
406900021121     C                   ELSE
407000021121     C                   MOVEL(P)  WDIVCE        PARDIV
407100021121     C                   ENDIF
407200021121     C                   ENDIF
407300970110     C*--------------------
407400970110     C* VARIABILI RIFERITE AL DATABASE
407500970110     C*--------------------
407600970110     C     *LIKE         DEFINE    TBLKUT        KTBKUT                         *TABEL00F
407700970110     C     *LIKE         DEFINE    TBLCOD        KTBCOD
407800970110     C     *LIKE         DEFINE    TBLKEY        KTBKEY
407900970110     C     *LIKE         DEFINE    ACOKUT        KACKUT                         *CNACO00F
408000970110     C     *LIKE         DEFINE    ACOKCC        KACKCC
408100970110     C     *LIKE         DEFINE    ACOKSC        KACKSC
408200970110     C     *LIKE         DEFINE    CLPKUT        KCLKUT                         *CNCLP00F
408300970110     C     *LIKE         DEFINE    CLPKCC        KCLKCC
408400970110     C     *LIKE         DEFINE    CLPKSC        KCLKSC
408500970110     C     *LIKE         DEFINE    INDKUT        KINKUT                         *CNIND00F
408600970110     C     *LIKE         DEFINE    INDKCC        KINKCC
408700970415     C     *LIKE         DEFINE    INDKSC        KINKSC
408800970110     C     *LIKE         DEFINE    TAMKSC        KTMKSC                         *TNTAM00F
408900970110     C     *LIKE         DEFINE    TAMCTR        KTMCTR
409000021121     C     *LIKE         DEFINE    TAMPRG        KTMPRG
409100990820     C     *LIKE         DEFINE    TADKSC        KTDKSC                         *TITAD00F
409200970110     C     *LIKE         DEFINE    TADCTR        KTDCTR
409300970110     C     *LIKE         DEFINE    TADPRG        KTDPRG
409400970317     C     *LIKE         DEFINE    TADLNP        KTDLNP
409500990812     C     *LIKE         DEFINE    TASKSC        KTAKSC                         *TITAS00F
409600970110     C     *LIKE         DEFINE    TASAAS        KTAAAS
409700970110     C     *LIKE         DEFINE    TASMGS        KTAMGS
409800970110     C     *LIKE         DEFINE    CSBAAS        KSBAAS                         *TNCSB00F
409900970110     C     *LIKE         DEFINE    CSBLNP        KSBLNP
410000970110     C     *LIKE         DEFINE    CSBNRS        KSBNRS
410100970110     C     *LIKE         DEFINE    CSBNSP        KSBNSP
410200011001     C     *LIKE         DEFINE    ETCDSF        KTCDSF                         *TEETC00F
410300980713     C     *LIKE         DEFINE    ETCCTO        KTCCTO
410400970110     C     *LIKE         DEFINE    ETCKSC        KTCKSC
410500970110     C     *LIKE         DEFINE    ETCCTR        KTCCTR
410600970110     C     *LIKE         DEFINE    ETCPRG        KTCPRG
410700970110     C     *LIKE         DEFINE    ETCLNP        KTCLNP
410800011001     C     *LIKE         DEFINE    ETSDSF        KTSDSF                         *TEETS00F
410900980713     C     *LIKE         DEFINE    ETSCTO        KTSCTO
411000970110     C     *LIKE         DEFINE    ETSKSC        KTSKSC
411100970110     C     *LIKE         DEFINE    ETSCTR        KTSCTR
411200970110     C     *LIKE         DEFINE    ETSPRG        KTSPRG
411300970110     C     *LIKE         DEFINE    ETSLNP        KTSLNP
411400970110     C     *LIKE         DEFINE    ETSSGL        KTSSGL
411500011001     C     *LIKE         DEFINE    ETTDSF        KTTDSF                         *TEETT00F
411600980713     C     *LIKE         DEFINE    ETTCTO        KTTCTO
411700970110     C     *LIKE         DEFINE    ETTKSC        KTTKSC
411800970110     C     *LIKE         DEFINE    ETTCTR        KTTCTR
411900970110     C     *LIKE         DEFINE    ETTPRG        KTTPRG
412000970110     C     *LIKE         DEFINE    ETTLNP        KTTLNP
412100970110     C     *LIKE         DEFINE    ETTFIE        KTTFIE
412200970110     C     *LIKE         DEFINE    ETTCTS        KTTCTS
412300140919     C     *LIKE         DEFINE    ETTntw        KTTntw
412400140919     C     *LIKE         DEFINE    ETTnaz        KTTnaz
412500140919     C     *LIKE         DEFINE    ETTcad        KTTcad
412600970110     C     *LIKE         DEFINE    TAMKSC        CARKSC                         *DEPOSITI
412700970110     C     *LIKE         DEFINE    TAMCTR        CARCTR
412800970110     C     *LIKE         DEFINE    TAMPRG        CARPRG
412900970430     C     *LIKE         DEFINE    TASLNP        DEPFIL
413000980713     C     *LIKE         DEFINE    PARCTO        DEPCTO
413100020730     C     *LIKE         DEFINE    §NTWFIE       DLPFL1
413200011128     C     *LIKE         DEFINE    TADORP        DEPORP
413300140919     C     *LIKE         DEFINE    ETtntw        Xntw                           *DI PASSAGGIO
413400140919     C     *LIKE         DEFINE    ETCLNP        XORG                           *DI PASSAGGIO
413500980821     C     *LIKE         DEFINE    ETTFIE        XFIE
413600011011     C     *LIKE         DEFINE    TBECOD        KTECOD                         *TNTBE01L
413700011011     C     *LIKE         DEFINE    TBEKE1        KTEKE1
413800050309     C     *LIKE         DEFINE    GCPAAS        KGPAAS                         *TIGCP00F
413900021122     C     *LIKE         DEFINE    GCPLNP        KGPLNP
414000021122     C     *LIKE         DEFINE    GCPNRS        KGPNRS
414100021122     C     *LIKE         DEFINE    GCPNSP        KGPNSP
414200980821     C*--------------------
414300970110     C* CHAIVI DI LETTURA
414400970110     C*--------------------
414500060315     C* CHIAVE LETTURA TITA730C - COMPLETA
414600060315     C     KEYTA7        KLIST
414700060315     C                   KFLD                    TA7AAS
414800060315     C                   KFLD                    TA7LNP
414900060315     C                   KFLD                    TA7NRS
415000060315     C                   KFLD                    TA7NSP
415100060315     C                   KFLD                    TA7TBL
415200970110     C* LETTURA TABEL00F - COMPLETA
415300970110     C     KEYTAB        KLIST
415400970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
415500970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
415600970110     C                   KFLD                    KTBKEY                         *CHIAVE TABELLA
415700970110     C* LETTURA TABEL00F - PARZIALE
415800970110     C     KE2TAB        KLIST
415900970110     C                   KFLD                    KTBKUT                         *CODICE UTENTE
416000970110     C                   KFLD                    KTBCOD                         *CODICE TABELLA
416100970110     C* LETTURA CNACO00F - COMPETA
416200970110     C     KEYACO        KLIST
416300970110     C                   KFLD                    KACKUT                         *CODICE UTENTE
416400970110     C                   KFLD                    KACKCC                         *CODICE CAPOCONTO
416500970110     C                   KFLD                    KACKSC                         *CODICE CLIENTE
416600970110     C* LETTURA CNCLP00F - COMPLETA
416700970110     C     KEYCLP        KLIST
416800970110     C                   KFLD                    KCLKUT                         *CODICE UTENTE
416900970110     C                   KFLD                    KCLKCC                         *CODICE CAPOCONTO
417000970110     C                   KFLD                    KCLKSC                         *CODICE CLIENTE
417100970415     C* LETTURA CNIND00F - COMPLETA
417200970415     C     KEYIND        KLIST
417300970415     C                   KFLD                    KINKUT                         *CODICE UTENTE
417400970415     C                   KFLD                    KINKCC                         *CODICE CAPOCONTO
417500970415     C                   KFLD                    KINKSC                         *CODICE CLIENTE
417600970110     C* LETTURA TNTAM01L - PARZIALE
417700970110     C     KEYTAM        KLIST
417800970110     C                   KFLD                    KTMKSC                         *CLIENTE
417900970110     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
418000021121     C* LETTURA TNTAM01L - PARZIALE
418100021121     C     KEYTAMUNIQ    KLIST
418200021121     C                   KFLD                    KTMKSC                         *CLIENTE
418300021121     C                   KFLD                    KTMCTR                         *CODICE TARIFFA
418400021121     C                   KFLD                    KTMPRG                         *PROGRESSIVO
418500990820     C* LETTURA TITAD01L - PARZIALE
418600970110     C     KEYTAD        KLIST
418700970110     C                   KFLD                    KTDKSC                         *CLIENTE
418800970110     C                   KFLD                    KTDCTR                         *CODICE TARIFFA
418900970110     C                   KFLD                    KTDPRG                         *PROGRESSIVO
419000970317     C                   KFLD                    KTDLNP                         *LINEA DI PARTENZA
419100990812     C* LETTURA TITAS31C - PARZIALE
419200970110     C     K31TAS        KLIST
419300970110     C                   KFLD                    KTAKSC                         *CLIENTE
419400990812     C* LETTURA TITAS38C - COMPLETA
419500970110     C     K38TAS        KLIST
419600970110     C                   KFLD                    KTAAAS                         *ANNO
419700970110     C                   KFLD                    KTAMGS                         *MESE/GIORNO
419800970110     C                   KFLD                    KTAKSC                         *CLIENTE
419900970110     C* LETTURA TNCSB03L - COMPLETA
420000970110     C     KEYCSB        KLIST
420100970110     C                   KFLD                    KSBAAS                         *ANNO
420200970110     C                   KFLD                    KSBLNP                         *LINEA PARTENZA
420300970110     C                   KFLD                    KSBNRS                         *SERIE
420400970110     C                   KFLD                    KSBNSP                         *SPEDIZIONE
420500011001     C* LETTURA TEETC01L - COMPLETA
420600981218     C     KEYETC        KLIST
420700980713     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
420800980713     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
420900970110     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
421000970110     C                   KFLD                    KTCCTR                         *TARIFFA
421100970110     C                   KFLD                    KTCPRG                         *PROGRESSIVO
421200970110     C                   KFLD                    KTCLNP                         *LINEA PARTENZA
421300011001     C* LETTURA TEETC01L - PARZIALE
421400981218     C     KE3ETC        KLIST
421500981218     C                   KFLD                    KTCDSF                         *DATI SEDE O FILIALE
421600981218     C                   KFLD                    KTCCTO                         *TARIFFA O OFFERTA
421700981218     C                   KFLD                    KTCKSC                         *CLIENTE O VISITA
421800011001     C* LETTURA TEETS01L - COMPLETA
421900970110     C     KEYETS        KLIST
422000980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
422100970110     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
422200970110     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
422300970110     C                   KFLD                    KTSCTR                         *TARIFFA
422400970110     C                   KFLD                    KTSPRG                         *PROGRESSIVO
422500970110     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
422600970417     C                   KFLD                    KTSSGL                         *SCAGLIONE
422700011001     C* LETTURA TEETS01L - PARZIALE
422800970314     C     KE2ETS        KLIST
422900980713     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
423000970314     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
423100970314     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
423200970314     C                   KFLD                    KTSCTR                         *TARIFFA
423300970314     C                   KFLD                    KTSPRG                         *PROGRESSIVO
423400970314     C                   KFLD                    KTSLNP                         *LINEA PARTENZA
423500011001     C* LETTURA TEETS01L - PARZIALE
423600981218     C     KE3ETS        KLIST
423700981218     C                   KFLD                    KTSDSF                         *DATI SEDE O FILIALE
423800981218     C                   KFLD                    KTSCTO                         *TARIFFA O OFFERTA
423900981218     C                   KFLD                    KTSKSC                         *CLIENTE O VISITA
424000011001     C* LETTURA TEETT01L - COMPLETA
424100970110     C     KEYETT        KLIST
424200980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
424300970110     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
424400970110     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
424500970110     C                   KFLD                    KTTCTR                         *TARIFFA
424600970110     C                   KFLD                    KTTPRG                         *PROGRESSIVO
424700970110     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
424800970110     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
424900970110     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
425000140919     C* LETTURA TEETT11L - COMPLETA
425100140919     C     KEYETT1       KLIST
425200140919     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
425300140919     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
425400140919     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
425500140919     C                   KFLD                    KTTCTR                         *TARIFFA
425600140919     C                   KFLD                    KTTPRG                         *PROGRESSIVO
425700140919     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
425800140919     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
425900140919     C                   KFLD                    KTTntw                         *ITALIA/ESTERO
426000140919     C                   KFLD                    KTTCTS                         *CODICE TASSAZIONE
426100140919     C                   KFLD                    KTTnaz                         *CODICE TASSAZIONE
426200140919     C                   KFLD                    KTTCad                         *CODICE TASSAZIONE
426300011001     C* LETTURA TEETT01L - PARZIALE
426400970314     C     KE2ETT        KLIST
426500980713     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
426600970314     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
426700970314     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
426800970314     C                   KFLD                    KTTCTR                         *TARIFFA
426900970314     C                   KFLD                    KTTPRG                         *PROGRESSIVO
427000970314     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
427100970314     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
427200140919     C* LETTURA TEETT11L - PARZIALE
427300140919     C     KE2ETT1       KLIST
427400140919     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
427500140919     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
427600140919     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
427700140919     C                   KFLD                    KTTCTR                         *TARIFFA
427800140919     C                   KFLD                    KTTPRG                         *PROGRESSIVO
427900140919     C                   KFLD                    KTTLNP                         *LINEA PARTENZA
428000140919     C                   KFLD                    KTTFIE                         *ITALIA/ESTERO
428100140919     C                   KFLD                    KTTntw                         *ITALIA/ESTERO
428200040809     C* LETTURA fiar531c
428300040809     C     Kar5          KLIST
428400040809     C                   KFLD                    ar5aas                         *DATI SEDE O FILIALE
428500040809     C                   KFLD                    ar5lnp                         *TARIFFA O OFFERTA
428600040809     C                   KFLD                    ar5nrs                         *CLIENTE O VISITA
428700040809     C                   KFLD                    ar5nsp                         *CLIENTE O VISITA
428800040809     C                   KFLD                    ar5trd                         *CLIENTE O VISITA
428900040809     C* LETTURA TEETT01L - PARZIALE
429000040809     C     KE3ETT        KLIST
429100040809     C                   KFLD                    KTTDSF                         *DATI SEDE O FILIALE
429200040809     C                   KFLD                    KTTCTO                         *TARIFFA O OFFERTA
429300040809     C                   KFLD                    KTTKSC                         *CLIENTE O VISITA
429400011011     C* LETTURA TNTBE01L - PARZIALE
429500011011     C     KEYTBE        KLIST
429600011011     C                   KFLD                    KTECOD                         *UTENTE
429700011011     C                   KFLD                    KTEKE1                         *CODICE
429800050309     C* LETTURA TIGCP51L - PARZIALE
429900021122     C     K01GCP        KLIST
430000021122     C                   KFLD                    KGPAAS                         *ANNO
430100021122     C                   KFLD                    KGPLNP                         *LINEA DI PARTENZA
430200021122     C                   KFLD                    KGPNRS                         *SERIE
430300021122     C                   KFLD                    KGPNSP                         *SPEDIZIONE
430400970110     C*--------------------
430500970110     C* VARIABILI DI CONTROLLO
430600970110     C*--------------------
430700970110     C                   MOVEL     '0'           $FINE             1            *FINE PGM
430800970110     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
430900970416     C                   MOVEL     'S'           $OK               1            *RECORD OK
431000970312     C                   MOVEL     '0'           $TAD              1            *DETTAGLIO TROVATO
431100970313     C                   MOVEL     '0'           $ERR              1            *ERRORE
431200970313     C                   MOVEL     '1'           *INOF                          *OVERFLOW QSYSPRT
431300970313     C                   MOVEL     '1'           *IN01                          *OVERFLOW TNTE41P
431400970110     C*
431500970110     C* REPERISCE I DATI DELL'UTENTE
431600020923     C                   EXSR      REPDATIUTE
431700970110     C*
431800970110     C* REPERISCE LA DATA CORRENTE
431900970110     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
432000970110     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
432100970110     C                   Z-ADD     WN8           G08DAT
432200970110     C                   Z-ADD     *ZEROS        G08INV
432300970110     C                   MOVEL     '0'           G08ERR
432400970110     C                   CALL      'XSRDA8'
432500970110     C                   PARM                    WLBDA8
432600970110     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
432700970110     C*
432800970110     C* CARICA TABELLE OCCORRENTI
432900970110     C                   EXSR      CARTAB
433000970313     C*
433100970313     C* CARICA I DATI DELLA TARIFFA -SCAGLIONI E CODICI TASSAZIONE-
433200970313     C                   EXSR      CARTAR
433300981218     C*
433400981218     C* ANNULLAMENTO TARIFFA
433500981218     C                   EXSR      CANTAR
433600021031     C*
433700021031     C* REPERIMENTO LISTA CLIENTI PADRI/FIGLI
433800021031     C                   EXSR      REPCLI
433900970110     C*
434000970110     C                   ENDSR
434100970110     O*------------------------------------------------------------------------*
434200970110     OQSYSPRT   E            TES              02
434300970110     O                       STA(1)              66
434400970110     O                       STA(2)             132
434500970110     O                       DSFIRS              20
434600970110     O                       UDATE              117 '  /  /  '
434700970110     O                                          127 'Pag.'
434800970110     O                       PAGE          Z    132
434900970110     O*------------------------
435000970110     O          E            ETAS        1
435100970110     O                       STA(3)              66
435200970110     O                       STA(4)             132
435300970423     O                       TASAAS        Z     16
435400970423     O                       TASLNP        Z     20
435500970423     O                       TASNRS        Z     23
435600970423     O                       TASNSP        Z     31
435700970423     O                       TASTBL              34
435800970423     O                       TASCTS              43
435900970110     O                       DEPERR             124
436000970110     O*------------------------
436100970110     O          E            EETX        1
436200970110     O                       STA(3)              66
436300970110     O                       STA(5)             132
436400970110     O                       PARKOV        Z     15
436500970110     O                       PARCTR              24
436600970110     O                       PARPRG              33
436700970110     O                       PARLNP        Z     53
436800970110     O                       PARFIE              62
436900970110     O                       DEPERR             124
437000970110     O*------------------------------------------------------------------------*
437100970110** -DDEC-
437200970110NON CALCOLARE                 1
437300970110CALCOLARE                     2
437400970110CALCOLARE IN REGIONE          3
437500970110CALCOLARE IN NAZIONE          4
437600970110 **  CREAZIONE DATI  TIPO  ** 5
437700980821TUTTA L'ITALIA                6
437800980821TUTTO L'ESTERO                7
4379009808218
4380009808219
43810098082110
438200990812** MTAS - MESSAGGI PER TITAS
438300970110FILIALE BOLLA NON TROVATA                                         1
438400970423CODICE TASSAZIONE NON TROVATO O NON VALIDO                        2
4385009704163
4386009701104
4387009701105
438800011001** METX - MESSAGGI PER TEETX
438900970110DATI TIPO ALLOCATI                                                1
439000971008NESSUNA SPEDIZIONE: VALORE DEDOTTO                                2
439100980901CREAZIONE AUTOMATICA                                              3
4392009701104
4393009701105
439400970110** STA
439500970110XXXXXXXXXXXXXXXXXXXX        TNTE41R       ** ERRORI IN CREAZIONE D1
439600970110ATI TIPO TARIFFE DA CONTO ECONOMICO **     99/99/99      PAG. 99992
439700970423SPEDIZIONE: 9999 999 99 9999999 XX  CTS: XX                       3
439800970423ERRORE: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX        4
439900970110CLIENTE 9999999  TAR 999  PRG 999  LINEA PARTENZA 999   I/E  X    5
