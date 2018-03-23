000100000000     H*------------------------------------------------------------------------*
000200010920     H*                   - STATISTICHE CLIENTI -          ESEGUE STAMPA       *
000300000000     H*------------------------------------------------------------------------*
000400000000     H* QUESTO PROGRAMMA PUO' PRODURRE:
000500000807     H*      - UNA O PIU' STATISTICHE PER UN SOLO CLIENTE      (DA RICH.CLIENTE)
000600000807     H*      - UNA O PIU' STATISTICHE PER PIU' CLIENTI INSIEME (DA RICH.CLIENTI)
000700000807     H*      - UNA O PIU' STATISTICHE PER CLIENTE UNIFICANTE   (DA RICH.CLIENTE)
000800000000     H*      - UNA SOLA STATISTICA PER TUTTI I CLIENTI (DA RICHIESTA STATISTICA)
000900000000     H*------------------------------------------------------------------------*
001000000000     H* INPUT  : -------- - ----------------------------------------------------
001100010920     H*          TISE60DS - PARAMETRI PASSATI DAL 'TISE61R':
001200020826     H*                     D60SCE = '1'   -> CLIENTE
001300020826     H*                              '2'   -> PIU' CLIENTI INSIEME
001400020826     H*                              '3'   -> CLIENTE UNIFICANTE
001500020826     H*                              '4'   -> CLIENTE (SOLO SE D60OP0=STA)
001600020826     H*                              '5'   -> CLIENTE UNIFICANTE (SOLO SE D60OP0=STA)
001700000817     H*                     D60OP0 = 'VIS' -> SE SCELTA INTERATTIVA
001800020522     H*                     D60OP1 = 'F21' -> SE DA STAMPA STATISTICHE
001900000000     H*
002000000817     H*                     D60OP0 = 'STA' SE SCELTA A MENU'
002100000000     H*                     D60OP1 = 'REG' SE CHIESTA STATISTICA REGIONE-REGIONE
002200050118     H*                            = 'PEV' SE CHIESTA STATISTICA PESI-VOLUMI
002300000000     H*                            = 'SER' SE CHIESTA STATISTICA SERVIZIO BOLLE
002400000000     H*                            = 'AFF' SE CHIESTA STATISTICA AFFIDABILITA'
002500000000     H*                            = 'DEL' SE CHIESTA STATISTICA DELTA X CLIENTE
002600000000     H*                            = 'AMM' SE CHIESTA STATISTICA AMMINISTRAZIONE
002700020523     H*                            = 'STT' SE TUTTE LE STATISTICHE X TUTTI I CLIENTI
002800000000     H* OUTPUT : -------- - ----------------------------------------------------
002900010920     H*          TISE62P  - STATISTICHE CLIENTI
003000950206     H*------------------------------------------------------------------------*
003100000000     H DECEDIT('0,') DATEDIT(*DMY.)
003200950206     F*------------------------------------------------------------------------*
003300000000     F* DATA BASE
003400000000     F*------------------------------------------------------------------------*
003500020523     FCNCLP00F  IF   E           K DISK
003600990609     FSISDA01L  IF   E           K DISK
003700990609     FSISDC01L  IF   E           K DISK
003800040317     FSISDE02L  IF   E           K DISK
003900990609     FSISDO01L  IF   E           K DISK
004000990609     FSISDP01L  IF   E           K DISK
004100050118     Fsipes01l  if   e           k disk
004200050118     Fsivlm01l  if   e           k disk
004300000911     FAZORG01L  IF   E           K DISK
004400000000     FTABEL00F  IF   E           K DISK
004500071210     FTISE62P   O    E             PRINTER OFLIND(*IN01) USROPN infds(CTRLPRTF)
004600000000     D*------------------------------------------------------------------------*
004700071210     D* DS di controllo del File printer
004800071210     D*------------------------------------------------------------------------*
004900071211
005000071210     D CTRLPRTF        DS
005100071211     d  Riga_di_Overflow...
005200071211     d                       188    189i 0                                      Riga di Overflow
005300071211     d  Riga_Corrente        367    368i 0                                      Riga corrente
005400071211
005500071211     D Righe_da_stampare...
005600071211     D                 S              3u 0
005700071210     D*------------------------------------------------------------------------*
005800000000     D* SCHIERE
005900000000     D*------------------------------------------------------------------------*
006000000000     D*-------------------
006100000000     D* TABELLE CAPICONTO
006200000000     D*-------------------
006300000000     D*-------------------
006400000000     D* DECODIFICA MESI DELL'ANNO
006500000000     D*-------------------
006600000000     D DMM             S              9    DIM(12) CTDATA PERRCD(1)
006700000000     D*-------------------
006800000000     D* DECODIFICA RIGHE DI STAMPA PER DESCRIZIONE STATISTICA
006900000000     D*-------------------
007000050121     D DRI             S             23    DIM(6) CTDATA PERRCD(1)
007100000000     D*-------------------
007200000000     D* DECODIFICA RIGHE DI STAMPA PER DESCRIZIONE CAMPI
007300000000     D*-------------------
007400950206     D DCA             S             11    DIM(15) CTDATA PERRCD(1)
007500000000     D*-------------------
007600000000     D* DECODIFICHE RIGHE IN STAMPA
007700000000     D*-------------------
007800051017     D DME             S             23    DIM(62) CTDATA PERRCD(1)
007900000000     D*-------------------
008000000000     D* FASCE DI PESO
008100000000     D*-------------------
008200000000     D FPE             S              2    DIM(11)                              *CODICI FASCE ESISTE
008300000000     D DFPE            S             14    DIM(11)                              *DECODIFICA FASCE
008400000000     D SFPE            S              2    DIM(11)                              *FASCE SELEZIONATE
008500000000     D*-------------------
008600000000     D* FILIALI/RAGGRUPPAMENTI REGIONE E DECODIFICHE
008700000000     D*-------------------
008800971124     D CRR             S              3  0 DIM(500)                             *RAGGRUPPAMENTO REGI
008900971124     D DCRR            S             25    DIM(500)                             *DECODIFICA RAGGRUPP
009000000000     D*-------------------
009100000000     D* COMBINAZIONE RAGGRUPPAMENTI PARTENZA/ARRIVO
009200000000     D*-------------------
009300020520     D REG             S              7    DIM(500)                             *P/A + CODICE + CODI
009400020520     D RE2             S              6    DIM(500)                             *CODICE + CODICE
009500950206     D*-------------------
009600950206     D* TIPI VALORI IN STAMPA PER REGIONE-REGIONE
009700950206     D*-------------------
009800950206     D VRR             S              1    DIM(10)                              *TIPI VALORE
009900950206     D DVRR            S             23    DIM(10)                              *DESCRIZIONE VALORI
010000000000     D*-------------------
010100000000     D* TIPI SERVIZIO
010200000000     D*-------------------
010300000000     D SER             S              1    DIM(10)                              *TIPI SERVIZIO
010400000000     D DSER            S             20    DIM(10)                              *DECODIFA SERVIZI
010500000000     D SERS            S              1    DIM(10)                              *SERVIZI SELEZIONATI
010600000000     D*-------------------
010700000000     D* ORE DI CONSEGNA
010800000000     D*-------------------
010900000000     D ORE             S              3  0 DIM(7)                               *ORE DI CONSEGNA
011000000000     D DORE            S             20    DIM(7)                               *DECODIFICA ORE
011100000000     D*------------------
011200000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
011300000000     D*------------------
011400000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
011500000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
011600000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
011700000000     D*-------------------
011800000000     D* VARIE
011900000000     D*-------------------
012000050118     D VAR             S              1    DIM(50)                              *SIGLA VARIA
012100000000     D*-------------------
012200000000     D* MESI DA ELABORARE
012300000000     D*-------------------
012400000000     D MES             S              2  0 DIM(12)
012500000000     D*-------------------
012600000000     D* ANNI DEI MESI DA ELABORARE
012700000000     D*-------------------
012800990601     D ANN             S              4  0 DIM(12)
012900950725     D*-------------------
013000950725     D* DS SCOMPATTA CLIENTI
013100950725     D*-------------------
013200050117      ** i codici clienti da passare nella DS sono diventati 28 da 30 che erano
013300050117      ** x permettere passaggio di altri parametri poichè la DS era già di 256
013400050117     D*KSA*****        S              4    DIM(30)                              *ALFANUMERICO
013500050117     D KSA             S              4    DIM(28)                              *ALFANUMERICO
013600000808     D*-------------------
013700000808     D* DS CLIENTI FIGLI
013800000808     D*-------------------
013900000808     D SKSC8           S              8  0 DIM(500)                             *clienti
014000000807     D*-------------------
014100000807     D* CLIENTI DA ELABORARE
014200000807     D*-------------------
014300000816     D CLI             S              7  0 DIM(500)
014400000816     D OCLI            S              7  0 DIM(500)                             *DA ORDINARE
014500050308     D*-------------------
014600050308     D* CLIENTI con Dati
014700050308     D*-------------------
014800050308     D CDati           S              1    DIM(500)
014900000810     D*-------------------
015000000810     D* DESCRIZIONE CLIENTI DA STAMPARE
015100000810     D*-------------------
015200050221     D blcC            S              1    DIM(8)                               *cliente Bloccato
015300050221     D totC            S              1    DIM(8)                               *totalizza DATI
015400000911     D SEDC            S              1    DIM(8)                               *ESISTENZA DATI
015500000911     D SCLI            S              7  0 DIM(8)                               *CLIENTE
015600000811     D SRAG            S             15    DIM(8)                               *RAGIONE SOCIALE
015700000810     D*------------------
015800000821     D* DS REPERIMENTO CLIENTE
015900000810     D*-------------------
016000000810     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
016100000810     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
016200000810     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
016300000810     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
016400000810     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
016500020604     D*-------------------
016600020604     D* SCHIERA X ORDINAMENTO OCCUR "REGIONE-REGIONE"
016700020604     D*-------------------
016800020604     D SKCRER          S                   DIM(500) LIKE(CRER)
016900000000    ?D*-------------------
017000000000    ?D* S C H I E R E   M U L T I P L E   D I   M E M O R I Z Z A Z I O N E
017100000000    ?D*-------------------
017200050118     D*-------------------
017300050118     D* STATISTICA PESI/VOLUMI                           - ???????? -
017400050118     D*-------------------
017500000000     D*-------------------
017600020520     D* STATISTICA REGIONE-REGIONE                       - 500 RAGGRUPPAMENTI -
017700000000     D*-------------------
017800000000     D*-------------------
017900000000     D* STATISTICA SERVIZIO BOLLE (S,C,P,V,R)            - 5 VALORI -
018000000000     D*-------------------
018100000000     D*-------------------
018200000000     D* STATISTICA AFFIDABILITA' SPEDIZIONI              - 7 ORARI -
018300000000     D*-------------------
018400000000     D*-------------------
018500000000     D* STATISTICA DELTA PER CLIENTE                     - 11 FASCE -
018600000000     D*-------------------
018700000000     D*-------------------
018800050118     D* SCHIERE STATISTICA AMMINISTRAZIONE               - 50 VARIE -
018900000000     D*-------------------
019000000000     D*---
019100000000     D* SCHIERE NON MULTIPLE
019200000000     D*---
019300010920     D AFC             S             18  3 DIM(12)                              FATTURATO
019400010920     D ANC             S             18  3 DIM(12)                              NOTE DI CREDITO
019500010920     D ARC             S             18  3 DIM(12)                              RETTIFICHE DI FATTUR
019600000000    ?D*-------------------
019700000000    ?D* S C H I E R E   D I   L A V O R O
019800000000    ?D*-------------------
019900000000     D*-------------------
020000000000     D* MULTIPLA PER LA "SERVIZIO BOLLE"                 - 5 VALORI -
020100000000     D*-------------------
020200970724     D*---
020300000000     D* PER MEDIE PER LA "SERVIZIO BOLLE"
020400970724     D*---
020500000000     D SPW             S             16  3 DIM(12)                              SPEDIZIONI
020600000000     D COW             S             16  3 DIM(12)                              COLLI
020700000000     D PEW             S             16  3 DIM(12)                              PESO
020800000000     D VOW             S             16  3 DIM(12)                              VOLUME
020900010920     D RIW             S             18  3 DIM(12)                              RICAVI
021000000000     D MEW             S             16  3 DIM(12)                              MEDIE CALCOLATE
021100000000     D*-------------------
021200000000     D* TOTALI PER LA "AFFIDABILITA'"
021300000000     D*-------------------
021400000000     D PNW             S             13  0 DIM(12)                              SPED CORRIERE DELL'O
021500000629     D PDW             S             13  0 DIM(12)                              SPED DISTRIB DELL'OR
021600000000     D PSW             S             13  0 DIM(12)                              SPED ESPRESSO DELL'O
021700000629     D PPW             S             13  0 DIM(12)                              SPED POSTE DELL'ORA
021800000000     D PTW             S             13  0 DIM(12)                              SPEDIZIONI TOTALI
021900000000     D*-------------------
022000000000     D* TOTALI A FONDO PAGINA PER LA "DELTA CLIENTE"
022100000000     D*-------------------
022200010920     D DRW             S             18  3 DIM(12)                              RICAVI REALI
022300010920     D DPW             S             18  3 DIM(12)                              RICAVI PRESUNTI
022400000000     D DSW             S             13  0 DIM(12)                              SPEDIZIONI
022500000000     D DDW             S              4  1 DIM(12)                              DELTA PERCENTUALE
022600970724     D*-------------------
022700970724     D* PERCENTUALI FATTURATO PER LA "AMMINISTRAZIONE"
022800000000     D*-------------------
022900000000     D APW             S              4  1 DIM(12)                              PERCEN SUL FATTURATO
023000000000     D*-------------------
023100000000     D* DI STAMPA
023200000000     D*-------------------
023300010920     D $VA             S             18  3 DIM(12)                              VALORE GENERICO
023400010920     D $V2             S             18  3 DIM(12)                              VALORE GENERICO 2
023500000000     D $P1             S              2  0 DIM(12)                              INCIDENZA SPED CORRI
023600000000     D $P2             S              2  0 DIM(12)                              INCIDENZA SPED DISTR
023700000000     D $P3             S              2  0 DIM(12)                              INCIDENZA SPED ESPRE
023800000629     D $P4             S              2  0 DIM(12)                              INCIDENZA SPED POSTE
023900000629     D $P5             S              2  0 DIM(12)                              INCIDENZA SPED TOT O
024000020823     D*------------------
024100020823     D* DS REPEIMENTO TERMINALs
024200020823     D*------------------
024300020823     D FNLV55DS      E DS
024400000808     D*------------------
024500000808     D* DS CONTROLLO DATA (8)
024600000808     D*------------------
024700000808     D WLBDA8          DS                  INZ
024800000808     D  G08DAT                 1      8  0
024900000808     D  G08INV                 9     16  0
025000000808     D  G08ERR                17     17
025100000808     D  G08TGI                18     22  0
025200000000     D*------------------
025300000000     D* ARCHITETTURA
025400000000     D*------------------
025500000000     D KPJBA         E DS
025600000000     D  PROFIL                 1     10                                         UTENTE
025700000000     D*------------------
025800000000     D* DS DI PROCEDURA
025900000000     D*------------------
026000010920     D DS60          E DS                  EXTNAME(TISE60DS)
026100000000     D*------------------
026200020117     D* DS LANCIO PROGRAMMA REPERIMENTO DATI CLIENTE -TISE66R-
026300000000     D*------------------
026400020117     D DSCL          E DS                  EXTNAME(TISE66DS)
026500000000     D*------------------
026600000000     D* DS FASCE DI PESO
026700000000     D*------------------
026800000000     D DS2L          E DS
026900000000     D*------------------
027000000000     D* DS REGIONI
027100000000     D*------------------
027200000000     D DS14          E DS
027300000000     D*------------------
027400000000     D* DS RAGGRUPPAMENTI REGIONI
027500000000     D*------------------
027600000000     D DS2M          E DS
027700070628      *------------------
027800070628      *? DS varie da decodificare a video (abilitate) sulla tab.:"CA"  ?
027900070628      *------------------
028000070628     D DSCA          E DS
028100070628     D*---
028200070628     D SkVar           S              1a   DIM(200)
028300070628     D SkDsc           S             23a   DIM(200)
028400070628     D SkOrd           S              6s 0 DIM(200)
028500000000     D*------------------
028600000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
028700000000     D*------------------
028800000000     D DS2W          E DS
028900091013     D*------------------
029000091013     D* DS TIPO SERVIZIO DESCRIZIONE
029100091013     D*------------------
029200091013     D DS5E          E DS
029300040317     D*------------------
029400040317     D* DS TIPO RECORD "BNB" DI SISDE
029500040317     D*------------------
029600040317     D DAR5BNBSDE    E DS
029700040318     D*------------------
029800040318     D* DS TIPO RECORD "BAN" DI SISDE
029900040318     D*------------------
030000040318     D DAR5BANSDE    E DS
030100000807     D*------------------
030200000807     D* DS DI PROCEDURA
030300000807     D*------------------
030400000807     D BS10DS        E DS                  EXTNAME(TIBS10DS)
030500000808     D  SKSC11                21   5520  0 DIM(500)                             *CLIENTI FIGLI
030600950725     D*------------------
030700950725     D* DS SCOMPATTA CLIENTI
030800950725     D*------------------
030900950725     D                 DS
031000950725     D  DSKSN                  1      4P 0
031100950725     D  DSKSA                  1      4
031200970724     D*------------------
031300970724     D* DS SUDDIVISIONE DATA INIZIALE
031400970724     D*------------------
031500970724     D                 DS
031600990601     D  ANNINI                 1      4  0
031700990601     D  MESINI                 5      6  0
031800990601     D  DATINI                 1      6  0
031900970724     D*------------------
032000970724     D* DS SUDDIVISIONE DATA FINALE
032100970724     D*------------------
032200970724     D                 DS
032300990601     D  ANNFIN                 1      4  0
032400990601     D  MESFIN                 5      6  0
032500990601     D  DATFIN                 1      6  0
032600050118     D*------------------
032700050118     D* DS UNIONE DATA SIPES01L
032800050118     D*------------------
032900050118     D                 DS
033000050118     D  pesANN                 1      4  0
033100050118     D  pesMES                 5      6  0
033200050118     D  pesDAT                 1      6  0
033300050118     D*------------------
033400050118     D* DS UNIONE DATA SIVLM01L
033500050118     D*------------------
033600050118     D                 DS
033700050118     D  vlmANN                 1      4  0
033800050118     D  vlmMES                 5      6  0
033900050118     D  vlmDAT                 1      6  0
034000970724     D*------------------
034100990609     D* DS UNIONE DATA SISDO01L
034200970724     D*------------------
034300970724     D                 DS
034400990601     D  SDOANN                 1      4  0
034500990601     D  SDOMES                 5      6  0
034600990601     D  SDODAT                 1      6  0
034700970724     D*------------------
034800990609     D* DS UNIONE DATA SISDC01L
034900970724     D*------------------
035000970724     D                 DS
035100990601     D  SDCANN                 1      4  0
035200990601     D  SDCMES                 5      6  0
035300990601     D  SDCDAT                 1      6  0
035400040317     D*------------------
035500040317     D* DS UNIONE DATA SISDE02L
035600040317     D*------------------
035700040317     D                 DS
035800040317     D  SDEANN                 1      4  0
035900040317     D  SDEMES                 5      6  0
036000040317     D  SDEDAT                 1      6  0
036100970724     D*------------------
036200990609     D* DS UNIONE DATA SISDA01L
036300970724     D*------------------
036400970724     D                 DS
036500990601     D  SDAANN                 1      4  0
036600990601     D  SDAMES                 5      6  0
036700990601     D  SDADAT                 1      6  0
036800970724     D*------------------
036900990609     D* DS UNIONE DATA SISDP01L
037000970724     D*------------------
037100970724     D                 DS
037200990601     D  SDPANN                 1      4  0
037300990601     D  SDPMES                 5      6  0
037400990601     D  SDPDAT                 1      6  0
037500000000     D*------------------
037600000000     D* DEFINIZIONE COSTANTI PER DECODIFICA ORE DI CONSEGNA (15)
037700000000     D*------------------
037800000000     D DORA1           C                   CONST('IN ANTICIPO         ')
037900000000     D DORA2           C                   CONST('IN ORARIO           ')
038000000000     D DORA3           C                   CONST('IN RITARDO DI 12 ORE')
038100000000     D DORA4           C                   CONST('IN RITARDO DI 24 ORE')
038200000000     D DORA5           C                   CONST('IN RITARDO DI 36 ORE')
038300000000     D DORA6           C                   CONST('IN RITARDO DI 48 ORE')
038400000000     D DORA7           C                   CONST('OLTRE 48H DI RITARDO')
038500000000    ?D*------------------
038600000000    ?D* S C H I E R E   M U L T I P L E   D I   M E M O R I Z Z A Z I O N E
038700000000    ?D*------------------
038800000000     D*------------------
038900000000     D* NUMERO DI BYTE IN PACKED
039000000000     D*------------------
039100000000     D* (16:2) + 1 = 9  /  (9 X 12) =  108   --> REGIONE-REGIONE/SERVIZIO/AMMNIN
039200000000     D* (11:2) + 1 = 6  /  (6 X 12) =   72   --> AFFIDABILITA/AMMINISTRAZIONE
039300000000     D* (13:2) + 1 = 7  /  (7 X 12) =   84   --> DELTA PER CLIENTE
039400000000     D*-----------------------------------
039500020520     D* "REGIONE-REGIONE"                 - 500 RAGGRUPPAMENTI X 12 MESI -
039600000000     D*-----------------------------------
039700020520     D CRER            DS                  OCCURS(500) INZ
039800020604     D  REX                    1      6                                         COMBINAZIONE REGIONE
039900020604     D  REC                    7    114P 3                                      COLLI
040000950206     D                                     DIM(12)                              COLLI
040100950206     D                                     PACKEVEN
040200020604     D  REP                  115    222P 3                                      PESO
040300950206     D                                     DIM(12)                              PESO
040400950206     D                                     PACKEVEN
040500020604     D  RER                  223    330P 3                                      RICAVI
040600950206     D                                     DIM(12)                              RICAVI
040700950206     D                                     PACKEVEN
040800020604     D  RES                  331    438P 3                                      SPEDIZIONI
040900950206     D                                     DIM(12)                              SPEDIZIONI
041000950206     D                                     PACKEVEN
041100020605     D  REV                  439    546P 3                                      VOLUME
041200950206     D                                     DIM(12)                              VOLUME
041300950206     D                                     PACKEVEN
041400000000     D*-----------------------------------
041500000000     D* "SERVIZIO BOLLE"                  - 5 VALORI X 12  MESI -
041600000000     D*-----------------------------------
041700000000     D CSER            DS                  OCCURS(5) INZ
041800000000     D  SNC                    1    108P 3                                      CORRIE
041900000000     D                                     DIM(12)                              SERVIZIO CORRIERE
042000000000     D                                     PACKEVEN
042100000629     D  SDC                  109    216P 3                                      DISTRI
042200000629     D                                     DIM(12)                              SERVIZIO DISTRIBUZIO
042300000629     D                                     PACKEVEN
042400000000     D  SEC                  217    324P 3                                      ESPRES
042500000000     D                                     DIM(12)                              SERVIZIO ESPRESSO
042600000000     D                                     PACKEVEN
042700091013     D  SHC                  325    432P 3                                      Espr.H10:30
042800091013     D                                     DIM(12)                              SERVIZIO ESPRESSO H1
042900000629     D                                     PACKEVEN
043000091013     D  SPC                  433    540P 3                                      POSTE
043100091013     D                                     DIM(12)                              SERVIZIO POSTE
043200091013     D                                     PACKEVEN
043300091013      *------
043400040317     D WSER            DS                  OCCURS(5) INZ
043500040317     D  VAW                    1    108P 3                                      VALORE
043600040317     D                                     DIM(12)                              TOTALE VALORE PERIOD
043700040317     D                                     PACKEVEN
043800000000     D*------------------
043900000000     D* "AFFIDABILITA' SPEDIZIONI"        - 7 ORARI X 12  MESI -
044000000000     D*------------------
044100000000     D CAFF            DS                  OCCURS(7) INZ
044200000000     D  CNC                    1     72P 0                                      CORRIE
044300000000     D                                     DIM(12)                              CONSEGNA CORRIERE
044400000629     D  CDC                   73    144P 0                                      DISTRI
044500000629     D                                     DIM(12)                              CONSEGNA DISTRIBUZIO
044600000000     D  CEC                  145    216P 0                                      ESPRES
044700000000     D                                     DIM(12)                              CONSEGNA ESPRESSO
044800000629     D  CPC                  217    288P 0                                      POSTE
044900000629     D                                     DIM(12)                              CONSEGNA POSTE
045000091214     D  CHC                  289    360P 0                                      ESPRES H1030
045100091214     D                                     DIM(12)                              CONSEGNA ESPRESSO H1
045200050118     D*------------------
045300050118     D* "PESI/VOLUMI"
045400050118     D*------------------
045500050118     D CPEV            DS                  OCCURS(2) INZ
045600050118     D  NCV                           7P 0                                      nr.colli VDL
045700050118     D                                     DIM(12)                              nr.colli VDL
045800050118     D  NCB                           7P 0                                      nr.colli Bollettati
045900050118     D                                     DIM(12)                              nr.colli Bollettati
046000050118     D  PVV                          12P 3                                      peso/vol VDL
046100050118     D                                     DIM(12)                              peso/vol VDL
046200050118     D  PVB                          12P 3                                      peso/vol Bollettati
046300050118     D                                     DIM(12)                              peso/vol Bollettati
046400000000     D*------------------
046500000000     D* "DELTA CLIENTE"                   - 11 FASCE X 12  MESI -
046600000000     D*------------------
046700000000     D CDEL            DS                  OCCURS(11) INZ
046800010920     D  DRC                    1     84P 3                                      R REAL
046900000000     D                                     DIM(12)                              DELTA RICAVI REALI
047000010920     D  DPC                   85    168P 3                                      R PRES
047100000000     D                                     DIM(12)                              DELTA RICAVI PRESUNT
047200000000     D  DSC                  169    252P 0                                      SPEDIZ
047300000000     D                                     DIM(12)                              DELTA SPEDIZIONI
047400000000     D*------------------
047500050118     D* "AMMINISTRAZIONE "                - 50 VARIE X 12  MESI -
047600000000     D*------------------
047700050118     D CAMM            DS                  OCCURS(50) INZ
047800000000     D  ARI                    1    108P 3                                      RICAVI
047900000000     D                                     DIM(12)                              RICAVI
048000000000     D                                     PACKEVEN
048100000000     D  ASP                  109    180P 0                                      SPEDIZ
048200000000     D                                     DIM(12)                              SPEDIZIONI
048300020905     D*------------
048400020905     D* DS REPERIMENTO DATI UTENTE
048500020905     D*------------
048600020905     D TIBS02DS      E DS
048700020905     D TIBS34DS      E DS                                                       *Profili utente
048800020905     D DDATIUTE      E DS                                                       *Dati utente
048900020905     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
049000020910     D TIBS56DS      E DS
049100020910     D  SKI                    7    756  0 dim(250)
049200040317     D*-------------------
049300040317     D* DEFINIZIONE VARIABILI
049400040317     D*-------------------
049500040318     D COR             S              9  0 DIM(12)                              VALORE GENERICO
049600040318     D BAN             S              7  0 DIM(12)                              VALORE GENERICO
049700040318     D BNB             S              7  0 DIM(12)                              VALORE GENERICO
049800000000     C*------------------------------------------------------------------------*
049900000000     C*?MAIN LINE
050000000000     C*------------------------------------------------------------------------*
050100020523     C*
050200020523     C* VERIFICO IL TIPO DI CHIAMATA X CONDIZINAR TUTTA L'ELABORAZIONE SUCCESSIVA
050300020523     C                   IF        D60SCE = '4' OR
050400020523     C                             D60SCE = '5'
050500020523     C                   OPEN      TISE62P
050600020823     C                   MOVEL     '1'           *IN01                          *OVERFLOW
050700020524     C                   Z-ADD     *ZEROS        WRKKSC                         *CLIENTE DI WRK
050800020523     C                   EXSR      SETFIL
050900020523     C                   MOVEL     WRKKSC        DEPFIL            3 0
051000020823     c*
051100020524     C* CICLO SU TUTO IL SISDO01L
051200020524     C                   DOW       $FINREC = '0'
051300020823     c*
051400020524     C* TESTO IL CAMBIO FILIALE X LA SUDDIVISIONE IN SPOOL DIVERSI X CLIENTI DIVERSI SE TIPO LANCIO
051500020524     C                   IF        D60OP1 = 'STT'
051600020523     C                   MOVEL     WRKKSC        CURFIL            3 0
051700020523     C                   IF        CURFIL <> DEPFIL
051800020523     C                   MOVEL     WRKKSC        DEPFIL
051900071210      *
052000071211     c                   eval      Righe_da_stampare = 4
052100071211     c                   exsr      solo_x_STT
052200020523     C                   WRITE     SE62FI                                       *RIGA FINE PROSPETTO
052300071210      *
052400020523     C                   CLOSE     TISE62P
052500020523     C                   OPEN      TISE62P
052600020823     C                   MOVEL     '1'           *IN01                          *OVERFLOW
052700020523     C                   ENDIF
052800020524     C                   ENDIF
052900020524     C* ESEGUO LA VERA ELABORAZIONE
053000020524     C                   EXSR      CARCLI
053100020523     C                   EXSR      SUBMAIN
053200020524     C                   EXSR      SETFIL
053300020523     C                   ENDDO
053400020524     C* ESEGUO L' ELABORAZIONE DELL'LTIMO CLIENTE LETTO
053500020524     C                   EXSR      CARCLI
053600020524     C                   EXSR      SUBMAIN
053700071210      *
053800071211     c                   eval      Righe_da_stampare = 4
053900071211     c                   exsr      solo_x_STT
054000020524     C                   WRITE     SE62FI                                       *RIGA FINE PROSPETTO
054100071210      *
054200020524     C                   CLOSE     TISE62P
054300020523     C                   ELSE
054400020523     C* LANCIO DA VIDEO
054500020523     C                   OPEN      TISE62P
054600020823     C                   MOVEL     '1'           *IN01                          *OVERFLOW
054700020523     C                   EXSR      CARCLI
054800020523     C                   EXSR      SUBMAIN
054900071210      *
055000071211     c                   eval      Righe_da_stampare = 4
055100071211     c                   exsr      solo_x_STT
055200020523     C                   WRITE     SE62FI                                       *RIGA FINE PROSPETTO
055300071210      *
055400020523     C                   CLOSE     TISE62P
055500020523     C                   ENDIF
055600020523     C*
055700020523     C* OPERAZIONI FINALI
055800020523     C                   EXSR      FINSR
055900020523     C*
056000020523     C                   SETON                                        LR
056100020523     C*------------------------------------------------------------------------*
056200020523     C*?CARCLI - CARICA I CLIENTI DA ELABORARE
056300020523     C*------------------------------------------------------------------------*
056400020523     C     CARCLI        BEGSR
056500020523     C*
056600020523     C* IMPOSTA IL CLIENTE/I DA ELABORARE
056700020523     C                   CLEAR                   CLI
056800020523     C                   CLEAR                   OCLI
056900050308     C                   CLEAR                   CDati
057000020523IF  1C     D60OP0        IFEQ      'VIS'
057100020523     C                   MOVEA     D60KSC        KSA                            *DS INPUT -> SCH ALF
057200020523IF  2C     D60SCE        IFEQ      '1'                                          *UN SOLO CLIENTE
057300020523     C                   MOVE      KSA(1)        DSKSA                          *SCH ALF -> DS ALF
057400020523     C                   Z-ADD     DSKSN         CLI(1)                         *DS NUM -> SCH NUM
057500020523E   2C                   ENDIF
057600020523IF  2C     D60SCE        IFEQ      '2'                                          * + CLIENTI INSIEME
057700050117DO  3C**** 1             DO        30            C
057800050117DO  3C     1             DO        28            C
057900020523     C                   MOVE      KSA(C)        DSKSA                          *SCH ALF -> DS ALF
058000020523     C                   Z-ADD     DSKSN         CLI(C)                         *DS NUM -> SCH NUM
058100020523E   3C                   ENDDO
058200020523E   2C                   ENDIF
058300020523IF  2C     D60SCE        IFEQ      '3'                                          *CLIENTE UNIFICANTE
058400020523     c                   CLEAR                   SKSC8
058500020523     C                   CLEAR                   BS10DS
058600020523     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
058700020523     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
058800020523     C                   MOVEL     'F'           D10PAF                         *CERCA figli
058900020523     C                   MOVE      KSA(1)        DSKSA                          *SCH ALF -> DS ALF
059000020523     C                   Z-ADD     DSKSN         N7                7 0          *DS NUM -> SCH NUM
059100020523     C                   Z-ADD     N7            D10COD                         *CODICE CLIENTE
059200020523     C                   CALL      'TIBS10R'
059300020523     C                   PARM                    BS10DS
059400020523IF  3C     D10ERR        IFEQ      *BLANKS                                      *NO ERRORE
059500020523     C                   Z-ADD     SKSC11        SKSC8                          *CLIENTI PADRE+FIGLI
059600020523DO  4C     1             DO        500           C
059700020523IF  5C     SKSC8(C)      IFGT      *ZEROS
059800020523     C                   MOVE      SKSC8(C)      CLI(C)                         *CLIENTI DETTAGLIO
059900020523E   5C                   ENDIF
060000020523E   4C                   ENDDO
060100020523X   3C                   ELSE                                                   *SI ERRORE
060200020823     C                   Z-ADD     N7            CLI(1)                         *SE STESSO
060300020523E   3C                   ENDIF
060400020523E   2C                   ENDIF
060500020523E   1C                   ENDIF
060600020523IF  1C     D60OP0        IFEQ      'STA'
060700020523IF  2C     D60SCE        IFEQ      '4'                                          *TUTTI I CLIENTI NN
060800020524     C                   Z-ADD     WRKKSC        CLI(1)                         *DS NUM -> SCH NUM
060900020523E   2C                   ENDIF
061000020523IF  2C     D60SCE        IFEQ      '5'                                          *TUTTI I CLIENTI X U
061100020523     c                   CLEAR                   SKSC8
061200020523     C                   CLEAR                   BS10DS
061300020523     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
061400020523     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
061500020523     C                   MOVEL     'F'           D10PAF                         *CERCA figli
061600020524     C                   Z-ADD     WRKKSC        D10COD                         *CODICE CLIENTE
061700020523     C                   CALL      'TIBS10R'
061800020523     C                   PARM                    BS10DS
061900020523IF  3C     D10ERR        IFEQ      *BLANKS                                      *NO ERRORE
062000020523     C                   Z-ADD     SKSC11        SKSC8                          *CLIENTI PADRE+FIGLI
062100020523DO  4C     1             DO        500           C
062200020523IF  5C     SKSC8(C)      IFGT      *ZEROS
062300020523     C                   MOVE      SKSC8(C)      CLI(C)                         *CLIENTI DETTAGLIO
062400020523E   5C                   ENDIF
062500020523E   4C                   ENDDO
062600020523X   3C                   ELSE                                                   *SI ERRORE
062700020524     C                   Z-ADD     WRKKSC        CLI(1)                         *DS NUM -> SCH NUM
062800020523E   3C                   ENDIF
062900020523E   2C                   ENDIF
063000020523E   1C                   ENDIF
063100020523     C*
063200020523     C* COMPATTA I CLIENTI TOGLIENDO I BUCHI VUOTI
063300090304     C                   clear                   OCLI
063400090304     C                   Z-ADD     0             compat            3 0
063500020523DO  2C     1             DO        500           C
063600020523IF  3C     CLI(C)        IFGT      *ZEROS
063700090304     C                   add       1             compat
063800090304     C                   Z-ADD     CLI(C)        OCLI(Compat)
063900020523E   3C                   ENDIF
064000020523E   2C                   ENDDO
064100020523     C                   Z-ADD     OCLI          CLI                            *CLIENTI COMPATTI
064200020523     C*
064300020523     C                   ENDSR
064400020523     C*------------------------------------------------------------------------*
064500020523     C*?SUBMAIN - POSIZIONAMENTO E PRIMA LETTURA SALDI CONTABILITA' CLIENTE
064600020523     C*------------------------------------------------------------------------*
064700020523     C     SUBMAIN       BEGSR
064800020523     C*
064900020523     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CONTABLILITA' CLIENTE
065000020524     C                   Z-ADD     CLI(1)        DEPKSC                         *CL SELEZIONATO
065100020524     C                   Z-ADD     CLI(1)        PRIKSC                         *1° CL SELEZIONATO
065200020823     C                   Z-ADD     1             DEPPAG                         *N.PAGINE PER CLIENT
065300020823     C                   MOVEL     '1'           *IN01                          *SI OVERFLW  (1° CLI
065400020823     C*
065500020823     C* REPERISCE IL TERMINAL PARTENZA (del cliente padre)
065600050307     C                   MOVEL     DEPKSC        WFIL              3 0
065700020823     C                   CLEAR                   FNLV55DS
065800020823     C                   MOVEL     'P'           D55TPT
065900020823     C                   MOVEL     WFIL          D55LIN
066000020823     C                   Z-ADD     DATCOR        D55DRF
066100020823     C                   CALL      'FNLV55R'
066200020823     C                   PARM                    FNLV55DS
066300020823IF  1C     D55ERR        IFNE      *BLANKS
066400020823     C                   MOVEL     WFIL          D55TFP
066500020823E   1C                   ENDIF
066600020823     C                   Z-ADD     D55TFP        DEPTFP
066700020823     C*
066800020523     C* CICLO FINO A FINE CLIENTI VALIDI CARICATI
066900020823     c                   Z-ADD     1             C
067000000808DO  2C     C             DOWLE     500
067100020524     C     CLI(C)        ANDGT     *ZEROS
067200000000     C*
067300000000     C* OPERAZIONI PER NUOVO CLIENTE
067400020523     C                   EXSR      NEWKSC
067500000000     C*
067600000000     C* LETTURA SALDI PER TUTTE LE STATISTICHE
067700950725IF  4C     SOLAFF        IFEQ      '0'                                          *CHIESTE ANCHE ALTRE
067800000000     C                   EXSR      CICSTA                                       *STATISTICHE OLTRE L
067900040318     C                   EVAL      WTRD = 'BNB'
068000040317     C                   EXSR      CICSTAE                                      *STATISTICHE OLTRE L
068100040318     C                   EVAL      WTRD = 'BAN'
068200040318     C                   EXSR      CICSTAE                                      *STATISTICHE OLTRE L
068300050118     C                   EXSR      CICSTAP                                      *STATISTICHE PESI
068400050118     C                   EXSR      CICSTAV                                      *STATISTICHE VOLUMI
068500950725E   4C                   ENDIF                                                  *AFFIDABILITA' SPEDI
068600000000     C*
068700000000     C* LETTURA SALDI PER LA STATISTICA "AFFIDABILITA' SPEDIZIONI"
068800950725IF  4C     MAIAFF        IFEQ      '0'                                          *CHIESTE ANCHE LA
068900000000     C                   EXSR      CICAFF                                       *STATISTICA AFFID.TA
069000950725E   4C                   ENDIF                                                  *OLTRE LE ALTRE
069100000000     C*
069200000000     C* LETTURA PARTICOLARITA' SALDI CLIENTE PER LA "AMMINISTRAZIONE"
069300950725IF  4C     D60ST5        IFEQ      'S'
069400000000     C                   EXSR      CICAMM
069500950725E   4C                   ENDIF
069600950725     C*
069700050308     c*  Se il Cliente non ha dati memorizzo in una schiera con (*) <- senza DATI X LA SELEZIONE
069800050308     C                   if        Con_Dati ='0'
069900050308     c                   move      '*'           CDati(C)
070000050308     c                   end
070100050308     c*
070200020523     C                   ADD       1             C                              *PROSSIMO CLIENTE
070300000808IF  4C     C             IFLE      500
070400020524     C                   IF        CLI(C) > *ZEROS
070500000810     C                   Z-ADD     CLI(C)        DEPKSC
070600020524     C                   ENDIF
070700950725X   4C                   ELSE
070800950725     C                   MOVEL     '1'           $FINO
070900950725E   4C                   ENDIF
071000950725E   2C                   ENDDO
071100020524     C*
071200020524     C* A SECONDA DEL TIPO DI ELABORAZIONE IMPOSTO IL CLIENTE X CUI REPERIRE LE INFORMAZIONI
071300020524     C     D60SCE        IFEQ      '3'
071400020524     C     D60SCE        OREQ      '5'
071500020524     C                   Z-ADD     PRIKSC        DEPKSC
071600020524     C                   ENDIF
071700000000     C*
071800000000     C* STAMPO STATISTICA/CHE CLIENTE
071900000000IF  2C     PASKSC        IFEQ      '1'
072000000000     C                   EXSR      STACLI                                       *SE MEMORIZZATO QC.
072100000000E   2C                   ENDIF
072200020523     C*
072300020523     C                   ENDSR
072400000000     C*------------------------------------------------------------------------*
072500000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CONTABILITA' CLIENTE
072600000000     C*------------------------------------------------------------------------*
072700000000     C     SETFIL        BEGSR
072800020524     C*
072900020524     C                   MOVEL     '0'           $FINO
073000000000     C*
073100020523     C                   Z-ADD     WRKKSC        KSOKSC                         *CLIENTE PIU' PICCOL
073200990602     C                   Z-ADD     9999          KSOANN                         *ANNO PIU' GRANDE
073300000000     C*
073400000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO CLIENTE VALIDO (KSC+ANN)
073500990609     C     KEYSDO        SETGT     SISDO01L                           99        *FINE ARCHIVIO
073600000000     C*
073700000000IF  1C     *IN99         IFEQ      '1'
073800000000     C                   MOVEL     '1'           $FINO
073900020524     C                   MOVEL     '1'           $FINREC
074000000000X   1C                   ELSE
074100000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
074200000000E   1C                   ENDIF
074300000000     C*
074400000000     C                   ENDSR
074500000000     C*------------------------------------------------------------------------*
074600000000     C*?LETFIL - LETTURA PROSSIMO RECORD FILE SALDI CONTABILITA' CLIENTE
074700000000     C*------------------------------------------------------------------------*
074800000000     C     LETFIL        BEGSR
074900000000     C*
075000000000     C* LEGGO FINO A:
075100000000     C*     - FINE FILE
075200000000     C*     - TROVATO RECORD VALIDO
075300000000     C                   MOVEL     'N'           WRECOK
075400020524DO  1C     $FINREC       DOWEQ     '0'
075500020524     C     WRECOK        ANDEQ     'N'
075600000000     C*
075700990609     C                   READ      SISDO01L                               99
075800000000     C*
075900000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
076000000000     C                   MOVEL     '1'           $FINO
076100020524     C                   MOVEL     '1'           $FINREC
076200000000X   2C                   ELSE
076300000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
076400000000E   2C                   ENDIF
076500000000     C*
076600000000E   1C                   ENDDO
076700000000     C*
076800020523     C                   Z-ADD     SDOKSC        WRKKSC                         *CLIENTE VALIDO
076900000000     C*
077000000000     C                   ENDSR
077100000000     C*------------------------------------------------------------------------*
077200000000     C*?CHKREC - CONTROLLA VALIDITA' RECORD SALDI CONTABILITA' CLIENTE
077300000000     C*------------------------------------------------------------------------*
077400000000     C     CHKREC        BEGSR
077500000000     C*
077600000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
077700000000     C*
077800000000     C* SE IL SALDO DEL CLIENTE LETTO HA UNA DATA SUPERIORE A QUELLA RICHIESTA
077900000000     C* A VIDEO, LEGGO DIRETTAMENTE UN ALTRO CLIENTE
078000020823IF  1C                   IF        WRECOK = 'S'
078100020823IF  2C     SDODAT        IFGE      D60DAT
078200020823     C     SDODAT        ANDLE     D60DA2
078300020823X   2C                   ELSE
078400000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
078500020823E   2C                   ENDIF
078600020823E   1C                   ENDIF
078700020523     C*
078800020524     C* X LANCIO X UNIFICANTI VERIFICO CHE NN SIA UN CLIENTE FIGLIO
078900020523     C                   IF        WRECOK = 'S'
079000020523     C     D60SCE        IFEQ      '5'
079100020524     C                   CLEAR                   BS10DS
079200020524     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
079300020524     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
079400020524     C                   MOVEL     'P'           D10PAF                         *VERIFICA SE PADRE
079500020524     C                   Z-ADD     SDOKSC        D10COD                         *CODICE CLIENTE
079600020524     C                   CALL      'TIBS10R'
079700020524     C                   PARM                    BS10DS
079800020524IF  3C     D10ERR        IFEQ      *BLANKS                                      *NO ERRORE
079900020524     C                   MOVEL     'N'           WRECOK
080000020524     C                   ENDIF
080100020523     C                   ENDIF
080200020524     C                   ENDIF
080300020523     C*
080400020524     C* SE RICHIESTA SELEZIONE VERIFICO LA QUALITA' CLIENTE RICHIESTA NEL LANCIO (SOLO X TIPO 'STA')
080500020523     C                   IF        WRECOK = 'S'
080600020524     C                   IF        D60OP0 = 'STA'
080700020523     C                   IF        D60CLA <> *BLANKS OR
080800020523     C                             D60CLB <> *BLANKS OR
080900020523     C                             D60CLC <> *BLANKS OR
081000020523     C                             D60CLT <> *BLANKS OR
081100020523     C                             D60CLD <> *BLANKS
081200020523     C                   Z-ADD     SDOKSC        CLPKSC
081300020916     C                   Z-ADD     1             CLPKUT
081400020916     C                   Z-ADD     DUTKCI        CLPKCC
081500020523     C     KEYCLP        CHAIN     CNCLP00F
081600020523     C                   IF        %FOUND(CNCLP00F) AND
081700020523     C                             (CLPCLV = D60CLA OR
081800020523     C                              CLPCLV = D60CLB OR
081900020523     C                              CLPCLV = D60CLC OR
082000020523     C                              CLPCLV = D60CLT OR
082100020523     C                              CLPCLV = D60CLD)
082200020523     C                   ELSE
082300020523     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
082400020523     C                   ENDIF
082500020524     C                   ENDIF
082600020523     C                   ENDIF
082700020523     C                   ENDIF
082800020523     C*
082900000000     C                   ENDSR
083000000000     C*------------------------------------------------------------------------*
083100000000     C*?CICSTA - CICLO MEMORIZZAZIONE SALDI PER TUTTE LE STATISTICHE
083200000000     C*------------------------------------------------------------------------*
083300000000     C     CICSTA        BEGSR
083400000000     C*
083500000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
083600000000     C                   EXSR      SETFI1
083700000000     C*
083800000000     C* CICLO FINO A FINE FILE SALDI CLIENTE
083900000000DO  1C     $FINC         DOWEQ     '0'
084000000000     C*
084100000000     C* MEMORIZZA I SALDI CLIENTE
084200000000     C                   EXSR      MEMSDC
084300000000     C*
084400000000     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
084500000000     C                   EXSR      LETFI1
084600000000     C*
084700000000E   1C                   ENDDO
084800000000     C*
084900000000     C                   ENDSR
085000000000     C*------------------------------------------------------------------------*
085100000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
085200000000     C*------------------------------------------------------------------------*
085300000000     C     SETFI1        BEGSR
085400000000     C*
085500000000     C* REIMPOSTO IL FINE FILE PER CICLO SALDI CLIENTE
085600000000     C                   MOVEL     '0'           $FINC
085700000000     C*
085800000000     C                   Z-ADD     DEPKSC        KSCKSC                         *DEPOSITO CLIENTE
085900000000     C                   Z-ADD     ANNINI        KSCANN                         *ANNO INIZIALE
086000000000     C                   Z-ADD     MESINI        KSCMES                         *MESE INIZIALE
086100000000     C*
086200000000     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
086300990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
086400000000     C*
086500000000IF  1C     *IN99         IFEQ      '1'
086600000000     C                   MOVEL     '1'           $FINC
086700000000X   1C                   ELSE
086800000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
086900000000E   1C                   ENDIF
087000000000     C*
087100000000     C                   ENDSR
087200000000     C*------------------------------------------------------------------------*
087300000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI CLIENTE
087400000000     C*------------------------------------------------------------------------*
087500000000     C     LETFI1        BEGSR
087600000000     C*
087700000000     C* LEGGO FINO A:
087800000000     C*     - FINE FILE
087900000000     C*     - TROVATO RECORD VALIDO
088000000000     C                   MOVEL     'N'           WRECOK
088100000000DO  1C     $FINC         DOWEQ     '0'
088200000000     C     WRECOK        ANDEQ     'N'
088300000000     C*
088400990609     C     KSCKSC        READE     SISDC01L                               99
088500000000     C*
088600000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
088700000000     C                   MOVEL     '1'           $FINC
088800000000X   2C                   ELSE
088900000000     C                   EXSR      VALSDC                                       *VALORIZZA CON "SDC"
089000000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
089100030520     c*
089200030520     C* CONTROLLO SE IL RECORD E' VALORIZZATO
089300030520IF  3C     SDCNSP        IFEQ      *ZEROS
089400030520     C     SDCPKG        ANDEQ     *ZEROS
089500030520     C     SDCPCC        ANDEQ     *ZEROS
089600030520     C     SDCPLS        ANDEQ     *ZEROS
089700030520     C     SDCNCO        ANDEQ     *ZEROS
089800030520     C     SDCVOL        ANDEQ     *ZEROS
089900030520     C     SDCVCC        ANDEQ     *ZEROS
090000030520     C     SDCVLS        ANDEQ     *ZEROS
090100030520     C     SDCIRB        ANDEQ     *ZEROS
090200030520     C     SDCIRR        ANDEQ     *ZEROS
090300030520     C     SDCINC        ANDEQ     *ZEROS
090400030520     C     SDCIRP        ANDEQ     *ZEROS
090500030520     C     SDCIRC        ANDEQ     *ZEROS
090600030520     C     SDCIRD        ANDEQ     *ZEROS
090700030520     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
090800030520E   3C                   ENDIF
090900000000E   2C                   ENDIF
091000000000     C*
091100000000E   1C                   ENDDO
091200000000     C*
091300000000     C                   ENDSR
091400000000     C*------------------------------------------------------------------------*
091500000000     C*?VALSDC - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDC"
091600000000     C*------------------------------------------------------------------------*
091700000000     C     VALSDC        BEGSR
091800000000     C*
091900000000     C                   MOVEL     'SDC'         $SDX                           *FILE DA CONTROLLARE
092000990601     C                   Z-ADD     SDCDAT        $DAT                           *DATA (AAAA/MM)
092100000000     C                   MOVEL     SDCTSP        $TSP                           *TIPO SERVIZIO
092200000000     C                   MOVEL     SDCTPO        $TPO                           *TIPO PORTO
092300000000     C***                  MOVELSDCTPR    $TPR             *TIPO PRODOTTO
092400000000     C                   MOVEL     SDCCTR        $CTR                           *CODICE TARIFFA
092500000000     C                   MOVEL     SDCFPE        $FPE                           *FASCIA DI PESO
092600000000     C                   MOVEL     SDCRBL        $RBL                           *RAGGRUPPAMENTO BOLL
092700000000     C*
092800000000     C                   ENDSR
092900000000     C*------------------------------------------------------------------------*
093000000000     C*?CHKRE1 - CONTROLLA VALIDITA' RECORD SALDI CLIENTE
093100000000     C*------------------------------------------------------------------------*
093200000000     C     CHKRE1        BEGSR
093300000000     C*
093400000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
093500000000     C*--------------------
093600990601     C* DATA (AAAA/MM)
093700000000     C*--------------------
093800000000     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
093900020823IF  1C     $DAT          IFGE      D60DAT
094000020823     C     $DAT          ANDLE     D60DA2
094100020823X   1C                   ELSE
094200020823     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
094300020823IF  2C     $DAT          IFGT      D60DA2
094400000000     C                   MOVEL     '1'           $FINC                          *ESCO DAL CICLO "SDC
094500000000     C                   MOVEL     '1'           $FINA                          *ESCO DAL CICLO "SDA
094600020823E   2C                   ENDIF
094700000000E   1C                   ENDIF
094800000000     C*
094900000000IF  1C     WRECOK        IFEQ      'S'
095000000000     C*
095100000000     C* CONTROLLO IL RECORD LETTO CON I PARAMETRI GENERALI RICHIESTI A VIDEO
095200010920     C* IMPORTANDO LA ROUTINE DI CONTROLLO ESTERNA ('TISE62T')
095300000000     C                   MOVEL     'S'           $ROK                           *RECORD OK X ROUTINE
095400000000     C*
095500010920    ? /COPY GAITRASRC/SRCTISE,TISE62T
095600000000     C*
095700000000     C* SE IL RECORD E' ERRATO NON FACCIO ULTERIORI CONTROLLI
095800000000     C                   MOVEL     $ROK          WRECOK
095900000000IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
096000000000     C*
096100000000     C* CONTROLLO I PARAMETRI DELLE STATISTICHE DIVERSE DALLA AFFIDABILITA' SOLO
096200990609     C* SE SONO NEL CICLO DI LETTURA DI SISDC00F
096300000000IF  3C     $SDX          IFEQ      'SDC'
096400000000     C*
096500000000     C* CONTROLLO PARAMETRI STATISTICA REGIONE-REGIONE SE SELEZIONATA
096600000000IF  4C     D60ST1        IFEQ      'S'
096700000000     C                   EXSR      CHKREG
096800000000E   4C                   ENDIF
096900050118     C*
097000050118     C* CONTROLLO PARAMETRI STATISTICA PESI-VOLUMI    SE SELEZIONATA
097100050118IF  4C     D60STp        IFEQ      'S'
097200050118     C                   EXSR      CHKpev
097300050118E   4C                   ENDIF
097400000000     C*
097500000000     C* CONTROLLO PARAMETRI STATISTICA SERVIZIO BOLLE SE SELEZIONATA
097600000000IF  4C     D60ST2        IFEQ      'S'
097700000000     C                   EXSR      CHKSER
097800000000E   4C                   ENDIF
097900000000     C*
098000000000     C* CONTROLLO PARAMETRI STATISTICA DELTA CLIENTE SE SELEZIONATA
098100000000IF  4C     D60ST4        IFEQ      'S'
098200000000     C                   EXSR      CHKDEL
098300000000E   4C                   ENDIF
098400000000     C*
098500000000     C* CONTROLLO PARAMETRI STATISTICA AMMINISTRAZIONE SE SELEZIONATA
098600000000IF  4C     D60ST5        IFEQ      'S'
098700000000     C                   EXSR      CHKAMM
098800000000E   4C                   ENDIF
098900000000     C*
099000000000     C* SE PER TUTTE LE STAT CONTROLLATE IL RECORD NON VA BENE NE LEGGO UN ALTRO
099100000000IF  4C     ROKREG        IFEQ      'N'                                          *NO X REGIONE-REGION
099200050118     C     ROKPEV        ANDEQ     'N'                                          *NO X PESI/VOLUMI
099300000000     C     ROKSER        ANDEQ     'N'                                          *NO X SERVIZIO BOLLE
099400000000     C     ROKDEL        ANDEQ     'N'                                          *NO X DELTA CLIENTE
099500000000     C     ROKAMM        ANDEQ     'N'                                          *NO X AMMINISTRAZION
099600000000     C                   MOVEL     'N'           WRECOK                         *NON VALIDO
099700000000E   4C                   ENDIF
099800000000     C*
099900000000X   3C                   ELSE
100000000000     C*
100100000000     C* CONTROLLO I PARAMETRI DELLE STATISTICA AFFIDABILITA' SOLO SE SONO NEL
100200990609     C* CICLO DI LETTURA DI SISDA00F
100300000000IF  4C     D60ST3        IFEQ      'S'
100400000000     C                   EXSR      CHKAFF
100500000000     C                   MOVEL     ROKAFF        WRECOK                         *RECORD VALIDO O NON
100600000000E   4C                   ENDIF
100700000000E   3C                   ENDIF
100800000000     C*
100900000000E   2C                   ENDIF
101000000000E   1C                   ENDIF
101100000000     C*
101200000000     C                   ENDSR
101300040317     C*------------------------------------------------------------------------*
101400040317     C*?CICSTAE - CICLO MEMORIZZAZIONE SALDI ESTENSIONE
101500040317     C*------------------------------------------------------------------------*
101600040317     C     CICSTAE       BEGSR
101700040317     C*
101800040317     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
101900040317     C                   EXSR      SETFI1E
102000040317     C*
102100040317     C* CICLO FINO A FINE FILE SALDI CLIENTE
102200040317DO  1C     $FINCE        DOWEQ     '0'
102300040317     C*
102400040317     C* MEMORIZZA I SALDI CLIENTE
102500040317     C                   EXSR      MEMSDE
102600040317     C*
102700040317     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
102800040317     C                   EXSR      LETFI1E
102900040317     C*
103000040317E   1C                   ENDDO
103100040317     C*
103200040317     C                   ENDSR
103300040317     C*------------------------------------------------------------------------*
103400040317     C*?SETFI1E - POSIZIONAMENTO E PRIMA LETTURA SALDI ESTENSIONE
103500040317     C*------------------------------------------------------------------------*
103600040317     C     SETFI1E       BEGSR
103700040317     C*
103800040317     C* REIMPOSTO IL FINE FILE PER CICLO SALDI CLIENTE
103900040317     C                   MOVEL     '0'           $FINCE
104000040317     C*
104100040317     C                   MOVEL     'SDC'         KSEFILE                        *DEPOSITO CLIENTE
104200040318     C                   MOVEL     WTRD          KSETRD                         *DEPOSITO CLIENTE
104300040317     C                   Z-ADD     DEPKSC        KSEKSC                         *DEPOSITO CLIENTE
104400040317     C                   Z-ADD     ANNINI        KSEANN                         *ANNO INIZIALE
104500040317     C                   Z-ADD     MESINI        KSEMES                         *MESE INIZIALE
104600040317     C*
104700040317     C* POSIZIONAMENTO FILE SALDI ESTENSIONE
104800040317     C     KEYSDE        SETLL     SISDE02L                           99        *FINE ARCHIVIO
104900040317     C*
105000040317IF  1C     *IN99         IFEQ      '1'
105100040317     C                   MOVEL     '1'           $FINCE
105200040317X   1C                   ELSE
105300040317     C                   EXSR      LETFI1E                                      *LETTURA RECORD
105400040317E   1C                   ENDIF
105500040317     C*
105600040317     C                   ENDSR
105700040317     C*------------------------------------------------------------------------*
105800040317     C*?LETFI1E - LETTURA PROSSIMO RECORD FILE SALDI ESTENSIONE
105900040317     C*------------------------------------------------------------------------*
106000040317     C     LETFI1E       BEGSR
106100040317     C*
106200040317     C* LEGGO FINO A:
106300040317     C*     - FINE FILE
106400040317     C*     - TROVATO RECORD VALIDO
106500040317     C                   MOVEL     'N'           WRECOK
106600040317DO  1C     $FINCE        DOWEQ     '0'
106700040317     C     WRECOK        ANDEQ     'N'
106800040317     C*
106900040317     C     KEYSDE1       READE     SISDE02L                               99
107000040317     C*
107100040317IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
107200040317     C                   MOVEL     '1'           $FINCE
107300040317X   2C                   ELSE
107400040317     C                   EXSR      VALSDE                                       *VALORIZZA CON "SDC"
107500040317     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
107600040317E   2C                   ENDIF
107700040317     C*
107800040317E   1C                   ENDDO
107900040317     C*
108000040317     C                   ENDSR
108100040317     C*------------------------------------------------------------------------*
108200040317     C*?VALSDE - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDC"
108300040317     C*------------------------------------------------------------------------*
108400040317     C     VALSDE        BEGSR
108500040317     C*
108600040317     C                   MOVEL     'SDC'         $SDX                           *FILE DA CONTROLLARE
108700040317     C                   Z-ADD     SDEDAT        $DAT                           *DATA (AAAA/MM)
108800040317     C                   MOVEL     SDETSP        $TSP                           *TIPO SERVIZIO
108900040317     C                   MOVEL     SDETPO        $TPO                           *TIPO PORTO
109000040317     C***                  MOVELSDETPR    $TPR             *TIPO PRODOTTO
109100040317     C                   MOVEL     SDECTR        $CTR                           *CODICE TARIFFA
109200040317     C                   MOVEL     SDEFPE        $FPE                           *FASCIA DI PESO
109300040317     C                   MOVEL     SDERBL        $RBL                           *RAGGRUPPAMENTO BOLL
109400040317     C*
109500040317     C                   ENDSR
109600000000     C*------------------------------------------------------------------------*
109700000000     C*?CHKREG - CONTROLL RECORD CON I PARAMETRI DELLA "REGIONE-REGIONE"
109800000000     C*------------------------------------------------------------------------*
109900000000     C     CHKREG        BEGSR
110000000000     C*
110100000000     C                   MOVEL     'S'           ROKREG                         *RECORD VALIDO
110200000000     C*
110300000000     C                   ENDSR
110400050118     C*------------------------------------------------------------------------*
110500050118     C*?CHKPEV - CONTROLL RECORD CON I PARAMETRI DELLA "PESI-VOLUMI"
110600050118     C*------------------------------------------------------------------------*
110700050118     C     CHKpev        BEGSR
110800050118     C*
110900050118     C                   MOVEL     'S'           ROKpev                         *RECORD VALIDO
111000050118     C*
111100050118     C                   ENDSR
111200000000     C*------------------------------------------------------------------------*
111300000000     C*?CHKSER - CONTROLLO RECORD CON I PARAMETRI DELLA "SERVIZIO BOLLE"
111400000000     C*------------------------------------------------------------------------*
111500000000     C     CHKSER        BEGSR
111600000000     C*
111700000000     C                   MOVEL     'S'           ROKSER                         *RECORD VALIDO
111800000000     C*
111900000000     C                   ENDSR
112000000000     C*------------------------------------------------------------------------*
112100000000     C*?CHKDEL - CONTROLLO RECORD CON I PARAMETRI DELLA "DELTA CLIENTE"
112200000000     C*------------------------------------------------------------------------*
112300000000     C     CHKDEL        BEGSR
112400000000     C*
112500000000     C                   MOVEL     'S'           ROKDEL                         *RECORD VALIDO
112600000000     C*
112700000000     C                   ENDSR
112800000000     C*------------------------------------------------------------------------*
112900000000     C*?CHKAMM - CONTROLLO RECORD CON I PARAMETRI DELLA "AMMINISTRAZIONE"
113000000000     C*------------------------------------------------------------------------*
113100000000     C     CHKAMM        BEGSR
113200000000     C*
113300000000     C                   MOVEL     'S'           ROKAMM                         *RECORD VALIDO
113400000000     C*
113500000000     C                   ENDSR
113600050118     C*------------------------------------------------------------------------*
113700050118     C*?CICSTAP - CICLO MEMORIZZAZIONE SALDI PER LE STATISTICHE PESI
113800050118     C*------------------------------------------------------------------------*
113900050118     C     CICSTAP       BEGSR
114000050118     C*
114100050118     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
114200050118     C                   EXSR      SETFIP
114300050118     C*
114400050118     C* CICLO FINO A FINE FILE SALDI CLIENTE
114500050118DO  1C     $FINPE        DOWEQ     '0'
114600050118     C*
114700050118     C* MEMORIZZA I SALDI CLIENTE
114800050118     c                   z-add     1             nrPEV
114900050118     C                   EXSR      MEMPEV
115000050118     C*
115100050118     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
115200050118     C                   EXSR      LETFIP
115300050118     C*
115400050118E   1C                   ENDDO
115500050118     C*
115600050118     C                   ENDSR
115700050118     C*------------------------------------------------------------------------*
115800050118     C*?SETFIP - POSIZIONAMENTO E PRIMA LETTURA SALDI PESI CLIENTE
115900050118     C*------------------------------------------------------------------------*
116000050118     C     SETFIP        BEGSR
116100050118     C*
116200050118     C                   MOVEL     '0'           $FINPE
116300050118     C*
116400050118     C                   Z-ADD     DEPKSC        KPEKSC                         *DEPOSITO CLIENTE
116500050118     C                   Z-ADD     ANNINI        KPEANN                         *ANNO INIZIALE
116600050118     C                   Z-ADD     MESINI        KPEMES                         *MESE INIZIALE
116700050118     C*
116800050118     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
116900050118     C     KEYPES        SETLL     SIPES01L                           99        *FINE ARCHIVIO
117000050118     C*
117100050118IF  1C     *IN99         IFEQ      '1'
117200050118     C                   MOVEL     '1'           $FINPE
117300050118X   1C                   ELSE
117400050118     C                   EXSR      LETFIP                                       *LETTURA RECORD
117500050118E   1C                   ENDIF
117600050118     C*
117700050118     C                   ENDSR
117800050118     C*------------------------------------------------------------------------*
117900050118     C*?LETFIP - LETTURA PROSSIMO RECORD FILE SALDI PESI CLIENTE
118000050118     C*------------------------------------------------------------------------*
118100050118     C     LETFIP        BEGSR
118200050118     C*
118300050118     C* LEGGO FINO A:
118400050118     C*     - FINE FILE
118500050118     C*     - TROVATO RECORD VALIDO
118600050118     C                   MOVEL     'N'           WRECOK
118700050118DO  1C     $FINPE        DOWEQ     '0'
118800050118     C     WRECOK        ANDEQ     'N'
118900050118     C*
119000050118     C     KPEKSC        READE     SIPES01L                               99
119100050118     C*
119200050118IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
119300050118     C                   MOVEL     '1'           $FINPE
119400050118X   2C                   ELSE
119500050118     C                   EXSR      CHKREP                                       *CONTROLLO RECORD
119600050118E   2C                   ENDIF
119700050118     C*
119800050118E   1C                   ENDDO
119900050118     C*
120000050118     C                   ENDSR
120100050118     C*------------------------------------------------------------------------*
120200050118     C*?CHKREP - CONTROLLA VALIDITA' RECORD SALDI PESI CLIENTE
120300050118     C*------------------------------------------------------------------------*
120400050118     C     CHKREP        BEGSR
120500050118     C*
120600050118     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
120700050118     C*--------------------
120800050118     C* DATA SALDI PESI
120900050118     C*--------------------
121000050118     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
121100050118IF  1C     pesDAT        IFGE      D60DAT
121200050118     C     pesDAT        ANDLE     D60DA2
121300050118X   1C                   ELSE
121400050118     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
121500050118IF  2C     pesDAT        IFGT      D60DA2
121600050118     C                   MOVEL     '1'           $FINPE                         *ESCO DAL CICLO "SDP
121700050118E   2C                   ENDIF
121800050118E   1C                   ENDIF
121900050118     C*--------------------
122000050118     C* CODICE TARIFFA
122100050118     C*--------------------
122200050118     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
122300050118IF  1C     SELCTR        IFNE      'T'
122400050118     C                   MOVEL     pesCTR        $CTR
122500050118IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
122600050118     C     $CTR          OREQ      D60CT2
122700050118     C     $CTR          OREQ      D60CT3
122800050118     C     $CTR          OREQ      D60CT4
122900050118     C     $CTR          OREQ      D60CT5
123000050118     C     $CTR          OREQ      D60CT6
123100050118     C     $CTR          OREQ      D60CT7
123200050118     C     $CTR          OREQ      D60CT8
123300050118     C     $CTR          OREQ      D60CT9
123400050118     C     $CTR          OREQ      D60CT0
123500050118X   2C                   ELSE
123600050118     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
123700050118E   2C                   ENDIF
123800050118E   1C                   ENDIF
123900050118     C*
124000050118     C                   ENDSR
124100050118     C*------------------------------------------------------------------------*
124200050118     C*?CICSTAV - CICLO MEMORIZZAZIONE SALDI PER LE STATISTICHE VOLUMI
124300050118     C*------------------------------------------------------------------------*
124400050118     C     CICSTAV       BEGSR
124500050118     C*
124600050118     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
124700050118     C                   EXSR      SETFIV
124800050118     C*
124900050118     C* CICLO FINO A FINE FILE SALDI CLIENTE
125000050118DO  1C     $FINVL        DOWEQ     '0'
125100050118     C*
125200050118     C* MEMORIZZA I SALDI CLIENTE
125300050118     c                   z-add     2             nrPEV
125400050118     C                   EXSR      MEMPEV
125500050118     C*
125600050118     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
125700050118     C                   EXSR      LETFIV
125800050118     C*
125900050118E   1C                   ENDDO
126000050118     C*
126100050118     C                   ENDSR
126200050118     C*------------------------------------------------------------------------*
126300050118     C*?SETFIV - POSIZIONAMENTO E PRIMA LETTURA SALDI VOLUMI CLIENTE
126400050118     C*------------------------------------------------------------------------*
126500050118     C     SETFIV        BEGSR
126600050118     C*
126700050118     C                   MOVEL     '0'           $FINVL
126800050118     C*
126900050118     C                   Z-ADD     DEPKSC        KVLKSC                         *DEPOSITO CLIENTE
127000050118     C                   Z-ADD     ANNINI        KVLANN                         *ANNO INIZIALE
127100050118     C                   Z-ADD     MESINI        KVLMES                         *MESE INIZIALE
127200050118     C*
127300050118     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
127400050118     C     KEYVLM        SETLL     SIVLM01L                           99        *FINE ARCHIVIO
127500050118     C*
127600050118IF  1C     *IN99         IFEQ      '1'
127700050118     C                   MOVEL     '1'           $FINVL
127800050118X   1C                   ELSE
127900050118     C                   EXSR      LETFIV                                       *LETTURA RECORD
128000050118E   1C                   ENDIF
128100050118     C*
128200050118     C                   ENDSR
128300050118     C*------------------------------------------------------------------------*
128400050118     C*?LETFIV - LETTURA PROSSIMO RECORD FILE SALDI PESI CLIENTE
128500050118     C*------------------------------------------------------------------------*
128600050118     C     LETFIV        BEGSR
128700050118     C*
128800050118     C* LEGGO FINO A:
128900050118     C*     - FINE FILE
129000050118     C*     - TROVATO RECORD VALIDO
129100050118     C                   MOVEL     'N'           WRECOK
129200050118DO  1C     $FINVL        DOWEQ     '0'
129300050118     C     WRECOK        ANDEQ     'N'
129400050118     C*
129500050118     C     KVLKSC        READE     SIVLM01L                               99
129600050118     C*
129700050118IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
129800050118     C                   MOVEL     '1'           $FINVL
129900050118X   2C                   ELSE
130000050118     C                   EXSR      CHKVOL                                       *CONTROLLO RECORD
130100050118E   2C                   ENDIF
130200050118     C*
130300050118E   1C                   ENDDO
130400050118     C*
130500050118     C                   ENDSR
130600050118     C*------------------------------------------------------------------------*
130700050118     C*?CHKVOL - CONTROLLA VALIDITA' RECORD SALDI VOLUMI CLIENTE
130800050118     C*------------------------------------------------------------------------*
130900050118     C     CHKVOL        BEGSR
131000050118     C*
131100050118     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
131200050118     C*--------------------
131300050118     C* DATA SALDI VOLUMI
131400050118     C*--------------------
131500050118     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
131600050118IF  1C     vlmDAT        IFGE      D60DAT
131700050118     C     vlmDAT        ANDLE     D60DA2
131800050118X   1C                   ELSE
131900050118     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
132000050118IF  2C     vlmDAT        IFGT      D60DA2
132100050118     C                   MOVEL     '1'           $FINVL                         *ESCO DAL CICLO "SDP
132200050118E   2C                   ENDIF
132300050118E   1C                   ENDIF
132400050118     C*--------------------
132500050118     C* CODICE TARIFFA
132600050118     C*--------------------
132700050118     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
132800050118IF  1C     SELCTR        IFNE      'T'
132900050118     C                   MOVEL     vlmCTR        $CTR
133000050118IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
133100050118     C     $CTR          OREQ      D60CT2
133200050118     C     $CTR          OREQ      D60CT3
133300050118     C     $CTR          OREQ      D60CT4
133400050118     C     $CTR          OREQ      D60CT5
133500050118     C     $CTR          OREQ      D60CT6
133600050118     C     $CTR          OREQ      D60CT7
133700050118     C     $CTR          OREQ      D60CT8
133800050118     C     $CTR          OREQ      D60CT9
133900050118     C     $CTR          OREQ      D60CT0
134000050118X   2C                   ELSE
134100050118     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
134200050118E   2C                   ENDIF
134300050118E   1C                   ENDIF
134400050118     C*
134500050118     C                   ENDSR
134600000000     C*------------------------------------------------------------------------*
134700000000     C*?CICAFF - CICLO MEMORIZZAZIONE SALDI SOLO PER LA "AFFIDABILITA' SPEDIZ"
134800000000     C*------------------------------------------------------------------------*
134900000000     C     CICAFF        BEGSR
135000000000     C*
135100000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE AFFIDABILITA'
135200000000     C                   EXSR      SETFI2
135300000000     C*
135400000000     C* CICLO FINO A FINE FILE SALDI AFFIDABILITA' CLIENTE
135500000000DO  1C     $FINA         DOWEQ     '0'
135600000000     C*
135700000000     C* MEMORIZZA I SALDI AFFIDABILITA' CLIENTE
135800000000     C                   EXSR      MEMSDA
135900000000     C*
136000000000     C* LETTURA SALDI AFFIDABILITA' CLIENTE SUCCESSIVO
136100000000     C                   EXSR      LETFI2
136200000000E   1C                   ENDDO
136300000000     C*
136400000000     C                   ENDSR
136500000000     C*------------------------------------------------------------------------*
136600000000     C*?SETFI2 - POSIZIONAMENTO E PRIMA LETTURA SALDI AFFIDABILITA' CLIENTE
136700000000     C*------------------------------------------------------------------------*
136800000000     C     SETFI2        BEGSR
136900000000     C*
137000000000     C* REIMPOSTO IL FINE FILE PER CICLO SALDI AFFIDABILITA' CLIENTE
137100000000     C                   MOVEL     '0'           $FINA
137200000000     C*
137300000000     C                   Z-ADD     DEPKSC        KSAKSC                         *DEPOSITO CLIENTE
137400000000     C                   Z-ADD     ANNINI        KSAANN                         *ANNO INIZIALE
137500000000     C                   Z-ADD     MESINI        KSAMES                         *MESE INIZIALE
137600000000     C*
137700000000     C* POSIZIONAMENTO FILE SALDI AFFID.TA' CLIENTE (KSC+ANN+MES)
137800990609     C     KEYSDA        SETLL     SISDA01L                           99        *FINE ARCHIVIO
137900000000     C*
138000000000IF  1C     *IN99         IFEQ      '1'
138100000000     C                   MOVEL     '1'           $FINA
138200000000X   1C                   ELSE
138300000000     C                   EXSR      LETFI2                                       *LETTURA RECORD
138400000000E   1C                   ENDIF
138500000000     C*
138600000000     C                   ENDSR
138700000000     C*------------------------------------------------------------------------*
138800000000     C*?LETFI2 - LETTURA PROSSIMO RECORD FILE SALDI AFFIDABILITA' CLIENTE
138900000000     C*------------------------------------------------------------------------*
139000000000     C     LETFI2        BEGSR
139100000000     C*
139200000000     C* LEGGO FINO A:
139300000000     C*     - FINE FILE
139400000000     C*     - TROVATO RECORD VALIDO
139500000000     C                   MOVEL     'N'           WRECOK
139600000000DO  1C     $FINA         DOWEQ     '0'
139700000000     C     WRECOK        ANDEQ     'N'
139800000000     C*
139900990609     C     KSAKSC        READE     SISDA01L                               99
140000000000     C*
140100000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
140200000000     C                   MOVEL     '1'           $FINA
140300000000X   2C                   ELSE
140400000000     C                   EXSR      VALSDA                                       *VALORIZZA CON "SDA"
140500000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
140600030520     C*
140700030520     C* CONTROLLO SE IL RECORD E' VALORIZZATO
140800030520IF  3C     SDANSP        IFEQ      *ZEROS
140900030520     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
141000030520E   3C                   ENDIF
141100030520     c*
141200000000E   2C                   ENDIF
141300000000E   1C                   ENDDO
141400000000     C*
141500000000     C                   ENDSR
141600000000     C*------------------------------------------------------------------------*
141700000000     C*?VALSDA - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDA"
141800000000     C*------------------------------------------------------------------------*
141900000000     C     VALSDA        BEGSR
142000000000     C*
142100000000     C                   MOVEL     'SDA'         $SDX                           *FILE DA CONTROLLARE
142200990601     C                   Z-ADD     SDADAT        $DAT                           *DATA (AAAA/MM)
142300000000     C                   MOVEL     SDATSP        $TSP                           *TIPO SERVIZIO
142400000000     C                   MOVEL     *BLANKS       $TPO                           *TIPO PORTO
142500000000     C***                  MOVEL*BLANKS   $TPR             *TIPO PRODOTTO
142600000000     C                   MOVEL     *BLANKS       $CTR                           *CODICE TARIFFA
142700000000     C                   MOVEL     *BLANKS       $FPE                           *FASCIA DI PESO
142800000000     C                   MOVEL     *BLANKS       $RBL                           *RAGGRUPPAMENTO BOLL
142900000000     C*
143000000000     C                   ENDSR
143100000000     C*------------------------------------------------------------------------*
143200990609     C*?CHKAFF - CONTROLLO RECORD DI SISDA00F
143300000000     C*------------------------------------------------------------------------*
143400000000     C     CHKAFF        BEGSR
143500000000     C*
143600000000     C                   MOVEL     'S'           ROKAFF                         *RECORD VALIDO
143700000000     C*
143800000000     C                   ENDSR
143900000000     C*------------------------------------------------------------------------*
144000000000     C*?NEWKSC - OPERAZIONI PER NUOVO CLIENTE
144100000000     C*------------------------------------------------------------------------*
144200000000     C     NEWKSC        BEGSR
144300050308     C*
144400050308      *  Cliente con dati
144500050308     C                   MOVEL     '0'           Con_Dati          1            Se contribuisce ai d
144600950725     C*
144700950725     C* SE VENGO DA STATISTICHE PER UN SOLO CLIENTE, LETTO QUESTO ESCO DAL CICLO
144800950725     C* PRINCIPALE DI RICERCA CLIENTI VALIDI
144900950725IF  1C     D60OP0        IFEQ      'VIS'
145000000817     C     D60SCE        ANDEQ     '1'
145100950725     C                   MOVEL     '1'           $FINO                          *FINE CICLO CLIENTI
145200950725E   1C                   ENDIF
145300950725     C*
145400950725     C* SE PROVIENE DALLA RICHIESTA STATISTICHE PER PIU' CLIENTI INSIEME, NON
145500950725     C* AZZERA LE VARIABILI DI LAVORO E LE SCHIERE
145600020826IF  1C*--- D60OP0        IFEQ      'STA'
145700020826     C*--- D60OP0        OREQ      'VIS'
145800020826     C*--- D60SCE        ANDEQ     '1'
145900020826     C*
146000020826     C* NON AZZERA LE VARIABILI DI LAVORO E LE SCHIERE SE -->
146100020826     C* RICHIESTA STATISTICA PER PIU' CLIENTI INSIEME
146200020826     C* RICHIESTA STATISTICA PER CLIENTE UNIFICANTE E NON STA "ELABORANDO" IL PADRE
146300020826IF  0C                   IF        (D60SCE ='1' OR D60SCE='4') OR
146400020826     C                             (D60SCE ='3' OR D60SCE='5') AND
146500020826     C                             (PRIKSC = DEPKSC)
146600020826     C*
146700020826     C* REIMPOSTA I PASSAGGI DI MEMORIZZAZIONE DATI
146800000000     C                   MOVEL     '0'           PASKSC                         SALDI CLIENTE
146900000000     C                   MOVEL     '0'           PASREG                         SALDI REG-REG
147000000000     C                   MOVEL     '0'           PASSER                         SALDI SERVIZIO BOLLE
147100000000     C                   MOVEL     '0'           PASAFF                         SALDI AFFIDABILITA'
147200000000     C                   MOVEL     '0'           PASDEL                         SALDI DELTA CLIENTE
147300000000     C                   MOVEL     '0'           PASAMM                         SALDI AMMINISTRAZION
147400020823     C                   MOVEL     '0'           PASDGC                         DATI GENERALI CLIENT
147500000000     C***
147600000000     C* AZZERAMENTO TOTALI CLIENTE
147700000000     C***
147800000000     C*--------------------
147900000000     C* "REGIONE-REGIONE"
148000000000     C*--------------------
148100020520DO  1C     1             DO        500           J
148200000000     C     J             OCCUR     CRER
148300020605     C                   CLEAR                   REX                            *REGIONE-REGIONE
148400950206     C                   CLEAR                   REC                            *COLLI
148500950206     C                   CLEAR                   REP                            *PESO
148600950206     C                   CLEAR                   RER                            *RICAVI
148700950206     C                   CLEAR                   RES                            *SPEDIZIONI
148800950206     C                   CLEAR                   REV                            *VOLUME
148900000000E   1C                   ENDDO
149000000000     C*--------------------
149100000000     C* "SERVIZIO BOLLE"
149200000000     C*--------------------
149300000000DO  1C     1             DO        5             J
149400000000     C     J             OCCUR     CSER
149500000000     C                   CLEAR                   SNC                            *CORRIERE
149600000629     C                   CLEAR                   SDC                            *DISTRIBUZIONE
149700000000     C                   CLEAR                   SEC                            *ESPRESSO
149800000629     C                   CLEAR                   SPC                            *POSTE
149900091013     C                   CLEAR                   SHC                            *H1030
150000000000E   1C                   ENDDO
150100040317     C*--------------------
150200040317     C* "SERVIZIO BOLLE" DA SISDE
150300040317     C*--------------------
150400040317     C                   CLEAR                   COR
150500040318     C                   CLEAR                   BAN
150600040318     C                   CLEAR                   BNB
150700000000     C*--------------------
150800000000     C* "AFFIDABILITA' SPEDIZIONI"
150900000000     C*--------------------
151000000000DO  1C     1             DO        PARO          J
151100000000     C     J             OCCUR     CAFF
151200000000     C                   CLEAR                   CNC                            *CORRIERE
151300000629     C                   CLEAR                   CDC                            *DISTRIBUZIONE
151400000000     C                   CLEAR                   CEC                            *ESPRESSO
151500091214     C                   CLEAR                   CHC                            *ESPRESSO h1030
151600000629     C                   CLEAR                   CPC                            *POSTE
151700000000E   1C                   ENDDO
151800000000     C*--------------------
151900000000     C* "DELTA CLIENTE"
152000000000     C*--------------------
152100000000DO  1C     1             DO        PARF          J
152200000000     C     J             OCCUR     CDEL
152300000000     C                   CLEAR                   DRC                            *RICAVI REALI
152400000000     C                   CLEAR                   DPC                            *RICAVI PRESUNTI
152500000000     C                   CLEAR                   DSC                            *SPEDIZIONI
152600000000E   1C                   ENDDO
152700000000     C*--------------------
152800000000     C* "AMMINISTRAZIONE"
152900000000     C*--------------------
153000050118DO  1C     1             DO        50            J
153100000000     C     J             OCCUR     CAMM
153200000000     C                   CLEAR                   ARI                            *RICAVI
153300000000     C                   CLEAR                   ASP                            *SPEDIZIONI
153400000000E   1C                   ENDDO
153500000000     C*
153600000000     C                   CLEAR                   AFC                            *FATTURATO
153700000000     C                   CLEAR                   ANC                            *NOTE DI CREDITO
153800000000     C                   CLEAR                   ARC                            *RETTIFICHE
153900000000     C*
154000000000     C* REIMPOSTO CONTATORE PARTICOLARITA' SALDI CLIENTE
154100000000     C                   Z-ADD     *ZEROS        P
154200950207     C                   MOVEL     *BLANKS       VAR
154300000000     C*--------------------
154400000000     C* RAGGRUPPAMENTI CLIENTE
154500000000     C*--------------------
154600950725     C                   CLEAR                   REG                            *P/A + CODICE+CODICE
154700950725     C                   CLEAR                   RE2                            *CODICE+CODICE
154800020823     C                   Z-ADD     *ZEROS        R
154900020826E   0C                   ENDIF
155000000000     C*
155100000000     C                   ENDSR
155200000000     C*------------------------------------------------------------------------*
155300990609     C*?MEMSDC - MEMORIZZA I SALDI DI SISDC01L
155400000000     C*          INDICI:  J(4,0) - FASCIA(2)
155500000000     C*------------------------------------------------------------------------*
155600000000     C     MEMSDC        BEGSR
155700000000     C*
155800000000     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
155900000000     C* PER IL CONTEGGIO IN STAMPA DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
156000000000     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
156100000000     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
156200000000     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
156300000000     C*
156400000000     C* MI SALVO LE NOTE E LE RETTIFICHE PER LA "DELTA" NEL CASO VENGANO AZZ.TE
156500000000     C                   Z-ADD     SDCIRR        DELIRR                         RETTIFICHE
156600000000     C                   Z-ADD     SDCINC        DELINC                         NOTE ACCREDITO
156700000000     C*
156800000000     C                   Z-ADD     1             JI
156900000000     C                   SETOFF                                           28
157000000000     C     SDCRBL        LOOKUP    FLAB(JI)                               28
157100000000     C* BOLLA NON VALIDA COME N°SPEDIZIONE
157200000000IF  1C     FSP(JI)       IFNE      'S'
157300000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
157400000000     C                   Z-ADD     *ZEROS        SDCPKG                         *PESO
157500020828     C                   Z-ADD     *ZEROS        SDCPCC                         *PESO CON CML
157600020828     C                   Z-ADD     *ZEROS        SDCPLS                         *PESO CML
157700000000     C                   Z-ADD     *ZEROS        SDCNCO                         *NUMERO COLLI
157800000000     C                   Z-ADD     *ZEROS        SDCVOL                         *VOLUME TOTALE
157900000000     C                   Z-ADD     *ZEROS        SDCVCC                         *VOLUME CON CML
158000000000     C                   Z-ADD     *ZEROS        SDCVLS                         *VOLUME CML
158100000000E   1C                   ENDIF
158200000000     C* BOLLA NON VALIDA COME RICAVO
158300000000IF  1C     FRI(JI)       IFNE      'S'
158400000000     C                   Z-ADD     *ZEROS        SDCIRB                         RICAVO REALE BOLLE
158500000000     C                   Z-ADD     *ZEROS        SDCIRR                         RETTIFICHE
158600000000     C                   Z-ADD     *ZEROS        SDCINC                         NOTE ACCREDITO
158700000000E   1C                   ENDIF
158800000000     C*
158900000000     C* RICAVO NETTO = RICAVO DA BOLLE + RETTIFICHE + NOTE DI CREDITO
159000000000     C                   Z-ADD     *ZEROS        RICTOT           16 3
159100000000     C     SDCIRB        ADD       SDCIRR        RICTOT
159200000000     C     SDCINC        ADD       RICTOT        RICTOT                         *RICAVO TOTALE
159300000000     C*
159400000000     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
159500000000     C                   Z-ADD     *ZEROS        RICDEL           16 3
159600000000     C     SDCIRD        ADD       DELIRR        RICDEL
159700000000     C     DELINC        ADD       RICDEL        RICDEL                         *RICAVO REALE DELTA
159800000000     C***
159900000000     C* V A L O R I Z Z A Z I O N I
160000000000     C***
160100000000     C* MESE
160200000000     C                   Z-ADD     1             MM
160300000000     C                   SETOFF                                           28
160400000000     C     SDCMES        LOOKUP    MES(MM)                                28
160500000000IF  1C     *IN28         IFEQ      *ON                                          *MESE TROVATO
160600000000     C*--------------------
160700000000     C* R E G I O N E - R E G I O N E
160800000000     C*--------------------
160900000000IF  2C     D60ST1        IFEQ      'S'                                          *SE STATISTICA SELEZ
161000000000     C     ROKREG        ANDEQ     'S'                                          *SE PASSATO AI CONTR
161100000000     C*
161200000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
161300050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
161400000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
161500000000     C                   MOVEL     '1'           PASREG                         *PER REGIONE
161600000000     C*
161700000000     C* RAGGRUPPAMENTO + RAGGRUPPAMENTO
161800950725     C                   MOVEL     *ZEROS        WRER              6
161900950726     C                   MOVEL     SDCREP        WRER                           *PAR+...
162000000000     C                   MOVE      SDCREA        WRER                           *PAR+ARR
162100950725     C                   MOVEL     *ZEROS        WPARER            7
162200950725     C                   MOVEL     'P'           WPARER                         *P+...+...
162300950725     C                   MOVE      WRER          WPARER                         *P+PAR+ARR
162400971128     C*
162500950725     C* CONTROLLA ESISTENZA
162600950725     C                   MOVE      REG           RE2                             *PRENDE SOLO CODICI
162700000000     C                   Z-ADD     1             J
162800000000     C                   SETOFF                                           28
162900950725     C     WRER          LOOKUP    RE2(J)                                 28
163000950725IF  3C     *IN28         IFEQ      *ON
163100000000     C                   Z-ADD     J             COMBIN                         *COMBIN GIA' PRESENT
163200950725X   3C                   ELSE
163300000000     C                   ADD       1             R                              *NON USARE ALTROVE
163400950725     C                   MOVEL     WPARER        REG(R)
163500000000     C                   Z-ADD     R             COMBIN                         *COMBIN NON PRESENTE
163600950725E   3C                   ENDIF
163700000000     C* O C U R
163800000000     C     COMBIN        OCCUR     CRER
163900020605     C                   MOVEL     WRER          REX                            *COMBINAZIONE REP+RE
164000950725     C                   ADD       SDCNCO        REC(MM)                        *COLLI
164100950725     C                   ADD       SDCPKG        REP(MM)                        *PESO
164200950725     C                   ADD       RICTOT        RER(MM)                        *RICAVI
164300950725     C                   ADD       SDCNSP        RES(MM)                        *SPEDIZIONI
164400950725     C                   ADD       SDCVOL        REV(MM)                        *VOLUME
164500950725E   2C                   ENDIF
164600000000     C*--------------------
164700000000     C* T O T A L I
164800000000     C*--------------------
164900000000IF  2C     D60ST2        IFEQ      'S'                                          *SE STATISTICA SELEZ
165000000000     C     ROKSER        ANDEQ     'S'                                          *SE PASSATO AI CONTR
165100000000     C*
165200000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
165300050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
165400000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
165500000000     C                   MOVEL     '1'           PASSER                         *PER SERVIZIO BOLLE
165600000000     C*
165700000000DO  3C     1             DO        5             J
165800000000     C*
165900000000IF  4C     J             IFEQ      1                                            *SPEDIZIONI°
166000950206     C                   Z-ADD     SDCNSP        VALORE           16 3
166100000000E   4C                   ENDIF
166200000000IF  4C     J             IFEQ      2                                            *COLLI°
166300000000     C                   Z-ADD     SDCNCO        VALORE
166400000000E   4C                   ENDIF
166500000000IF  4C     J             IFEQ      3                                            *PESO°
166600000000     C                   Z-ADD     SDCPKG        VALORE
166700000000E   5C                   ENDIF
166800000000IF  4C     J             IFEQ      4                                            *VOLUME°
166900000000     C                   Z-ADD     SDCVOL        VALORE
167000000000E   4C                   ENDIF
167100000000IF  4C     J             IFEQ      5                                            *RICAVI°
167200000000     C                   Z-ADD     RICTOT        VALORE
167300000000E   4C                   ENDIF
167400000000     C* O C U R
167500000000     C     J             OCCUR     CSER
167600000000     C*
167700000000IF  4C     SDCTSP        IFEQ      'C'
167800000000     C                   ADD       VALORE        SNC(MM)                        *CORRIERE
167900000000E   4C                   ENDIF
168000000000     C*
168100000000IF  4C     SDCTSP        IFEQ      'D'
168200000629     C                   ADD       VALORE        SDC(MM)                        *DISTRIBUZIONE
168300000000E   4C                   ENDIF
168400000000     C*
168500000000IF  4C     SDCTSP        IFEQ      'E'
168600000000     C                   ADD       VALORE        SEC(MM)                        *ESPRESSO
168700000000E   4C                   ENDIF
168800091013     C*
168900091013IF  4C     SDCTSP        IFEQ      'H'
169000091013     C                   ADD       VALORE        SHC(MM)                        *ESPR.10:30
169100091013E   4C                   ENDIF
169200000000     C*
169300000629IF  4C     SDCTSP        IFEQ      'P'
169400000629     C                   ADD       VALORE        SPC(MM)                        *POSTE
169500000629E   4C                   ENDIF
169600000629     C*
169700000000E   3C                   ENDDO
169800000000E   2C                   ENDIF
169900000000     C*--------------------
170000000000     C* D E L T A
170100000000     C*--------------------
170200000000IF  2C     D60ST4        IFEQ      'S'                                          *SE STATISTICA SELEZ
170300000000     C     ROKDEL        ANDEQ     'S'                                          *SE PASSATO AI CONTR
170400000309     C*
170500000309     C                   MOVEL     SDCANN        DATSDC            6 0
170600000309     C                   MOVE      SDCMES        DATSDC
170700000000     C*
170800000309     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
170900000309IF  3C     SDCFPE        IFEQ      '07'                                         *NO > 5000
171000000309     C     DATSDC        ANDLT     200001                                       *PRIMA DEL INIZIO GE
171100000309     C                   ELSE
171200050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
171300000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
171400000000     C                   MOVEL     '1'           PASDEL                         *PER DELTA CLIENTE
171500000000     C*
171600000000     C* ELABORO LA FASCIA SOLO SE SELEZIONATA
171700000000     C                   MOVEL     SDCFPE        FASCIA            2
171800000000     C                   Z-ADD     1             J
171900000000     C                   SETOFF                                           28
172000000000     C     FASCIA        LOOKUP    SFPE(J)                                28
172100000000IF  4C     *IN28         IFEQ      *ON
172200000000     C* O C U R
172300000000     C     J             OCCUR     CDEL                                         *F A S C I A
172400000000     C                   ADD       RICDEL        DRC(MM)                        *RICAVI REALI DELTA
172500000000     C                   ADD       SDCIRP        DPC(MM)                        *RICAVI PRESUNTI
172600000000     C                   ADD       SDCNSP        DSC(MM)                        *SPEDIZIONI°
172700000000E   4C                   ENDIF
172800000000E   3C                   ENDIF
172900000000E   2C                   ENDIF
173000000000     C*--------------------
173100000000     C* A M M I N I S T R A Z I O N E
173200000000     C*--------------------
173300990609     C* PRIMA DEL 97 -> FATTURATO = RICAVI SISDC + SISDP
173400990609     C* DOPO   IL 97 -> FATTURATO = RICAVI SISDP
173500000000IF  2C     D60ST5        IFEQ      'S'                                          *SE STATISTICA SELEZ
173600000000     C     ROKAMM        ANDEQ     'S'                                          *SE PASSATO AI CONTR
173700000000     C*
173800000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
173900050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
174000000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
174100000000     C                   MOVEL     '1'           PASAMM                         *PER AMMINISTRAZIONE
174200000000     C                   ADD       SDCINC        ANC(MM)                        *NOTE DI CREDITO
174300000000     C                   ADD       SDCIRR        ARC(MM)                        *RETTIFICHE AI RICAV
174400990602IF  3C     SDCANN        IFLT      1997
174500970724     C                   ADD       SDCIRB        AFC(MM)                        *FATTURATO LORDO
174600970724E   3C                   ENDIF
174700970724E   2C                   ENDIF
174800000000E   1C                   ENDIF
174900000000     C*
175000000000     C                   ENDSR
175100040317     C*------------------------------------------------------------------------*
175200040317     C*?MEMSDE - MEMORIZZA I SALDI DI SISDE02L
175300040317     C*------------------------------------------------------------------------*
175400040317     C     MEMSDE        BEGSR
175500040317     C*
175600040317     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
175700040317     C* PER IL CONTEGGIO IN STAMPA DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
175800040317     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
175900040317     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
176000040317     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
176100040317     C*
176200040318     C* VALORIZZA IL CAMPONE SDEUNI CON LA DS BAN o BNB
176300040318     c                   if        wtrd = 'BNB'
176400040317     C                   MOVEL     SDEUNI        DAR5BNBSDE
176500040318     C                   CLEAR                   DAR5BANSDE
176600040318     c                   else
176700040318     C                   MOVEL     SDEUNI        DAR5BANSDE
176800040318     C                   CLEAR                   DAR5BNBSDE
176900040318     c                   endif
177000040317     C*
177100040317     C                   Z-ADD     1             JI
177200040317     C                   SETOFF                                           28
177300040317     C     SDERBL        LOOKUP    FLAB(JI)                               28
177400040317     C* BOLLA NON VALIDA COME N°SPEDIZIONE
177500040317IF  1C     FSP(JI)       IFNE      'S'
177600040318     C                   Z-ADD     *ZEROS        §BNBBCOR                       *PESO
177700040318     C                   Z-ADD     *ZEROS        §BNBBNBA                       *PESO
177800040318     C                   Z-ADD     *ZEROS        §BNBBNB2                       *PESO
177900040318     C                   Z-ADD     *ZEROS        §BANNBA                        *PESO
178000040318     C                   Z-ADD     *ZEROS        §BANNB2                        *PESO
178100040317E   1C                   ENDIF
178200040317     C* BOLLA NON VALIDA COME RICAVO
178300040317IF  1C     FRI(JI)       IFNE      'S'
178400040317E   1C                   ENDIF
178500040317     C***
178600040317     C* V A L O R I Z Z A Z I O N I
178700040317     C***
178800040317     C* MESE
178900040317     C                   Z-ADD     1             MM
179000040317     C                   SETOFF                                           28
179100040317     C     SDEMES        LOOKUP    MES(MM)                                28
179200040317IF  1C     *IN28         IFEQ      *ON                                          *MESE TROVATO
179300040317     C*--------------------
179400040317     C* T O T A L I
179500040317     C*--------------------
179600040317IF  2C     D60ST2        IFEQ      'S'                                          *SE STATISTICA SELEZ
179700040317     C     ROKSER        ANDEQ     'S'                                          *SE PASSATO AI CONTR
179800040317     C*
179900040317     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
180000050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
180100040317     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
180200040317     C                   MOVEL     '1'           PASSER                         *PER SERVIZIO BOLLE
180300040317     C*
180400040318     C* MEMORIZZA COLLI ORIGINALI e BANCALI x tipo record BNB
180500040318     c                   if        wtrd = 'BNB'
180600040318     C                   ADD       §bnbBCOR      COR(MM)                        *POSTE
180700040318     C                   ADD       §bnbBnba      bnb(MM)                        *POSTE
180800040318     C                   ADD       §bnbBnb2      bnb(MM)                        *POSTE
180900040318     c                   else
181000040318     C* MEMORIZZA BANCALI x tipo record BAN
181100040318     C                   ADD       §bannba       ban(MM)                        *POSTE
181200040318     C                   ADD       §bannb2       ban(MM)                        *POSTE
181300040319     c                   endif
181400040317E   2C                   ENDIF
181500040317E   1C                   ENDIF
181600040317     C*
181700040317     C                   ENDSR
181800000000     C*------------------------------------------------------------------------*
181900990609     C*?MEMSDA - MEMORIZZA I SALDI DI SISDA01L
182000000000     C*------------------------------------------------------------------------*
182100000000     C     MEMSDA        BEGSR
182200000000     C*--------------------
182300000000     C* A F F I D A B I L I T A '
182400000000     C*--------------------
182500000000IF  1C     D60ST3        IFEQ      'S'                                          *SE STATISTICA SELEZ
182600000000     C     ROKAFF        ANDEQ     'S'                                          *SE PASSATO AI CONTR
182700000000     C*
182800000000     C* MESE
182900000000     C                   Z-ADD     1             MM
183000000000     C                   SETOFF                                           28
183100000000     C     SDAMES        LOOKUP    MES(MM)                                28
183200000000IF  2C     *IN28         IFEQ      *ON                                          *MESE TROVATO
183300000000     C*
183400000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
183500050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
183600000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
183700000000     C                   MOVEL     '1'           PASAFF                         *PER AFFIDABILITA'
183800000000     C*
183900000000IF  3C     SDADLV        IFLT      0
184000000000     C                   Z-ADD     1             J                              *IN ANTICIPO
184100000000X   3C                   ELSE
184200000000IF  4C     SDADLV        IFGT      48
184300000000     C                   Z-ADD     7             J                              *OLTRE 48HH RIT
184400000000X   4C                   ELSE
184500000000     C                   Z-ADD     2             J
184600000000     C                   SETOFF                                           28
184700000000     C     SDADLV        LOOKUP    ORE(J)                                 28
184800000000IF  5C     *IN28         IFEQ      *OFF                                         *NON TROVATO
184900000000     C                   Z-ADD     *ZEROS        J                              *PER DARE ERRORE CPF
185000000000E   5C                   ENDIF
185100000000E   4C                   ENDIF
185200000000E   3C                   ENDIF
185300000000     C* O C U R
185400000000     C     J             OCCUR     CAFF
185500000000     C*
185600000000IF  3C     SDATSP        IFEQ      'C'
185700000000     C                   ADD       SDANSP        CNC(MM)                        *CORRIERE
185800000000E   3C                   ENDIF
185900000000     C*
186000000000IF  3C     SDATSP        IFEQ      'D'
186100000629     C                   ADD       SDANSP        CDC(MM)                        *DISTRIBUZIONE
186200000000E   3C                   ENDIF
186300000000     C*
186400000000IF  3C     SDATSP        IFEQ      'E'
186500000000     C                   ADD       SDANSP        CEC(MM)                        *ESPRESSO
186600000000E   3C                   ENDIF
186700000629     C*
186800091214IF  3C     SDATSP        IFEQ      'H'
186900091214     C                   ADD       SDANSP        CHC(MM)                        *ESPRESSO H1030
187000091214E   3C                   ENDIF
187100091214     C*
187200000629IF  3C     SDATSP        IFEQ      'P'
187300000629     C                   ADD       SDANSP        CPC(MM)                        *POSTE
187400000629E   3C                   ENDIF
187500000000     C*
187600000000E   2C                   ENDIF
187700000000E   1C                   ENDIF
187800000000     C*
187900000000     C                   ENDSR
188000050118     C*------------------------------------------------------------------------*
188100050118     C*?MEMPEV - MEMORIZZA I SALDI DI SIPES01L
188200050118     C*------------------------------------------------------------------------*
188300050118     C     MEMPEV        BEGSR
188400050118     C*--------------------
188500050118     C* A F F I D A B I L I T A '
188600050118     C*--------------------
188700050118IF  1C     D60STp        IFEQ      'S'                                          *SE STATISTICA SELEZ
188800050118     C     ROKpev        ANDEQ     'S'                                          *SE PASSATO AI CONTR
188900050118     C*
189000050118     C* MESE
189100050118     C                   Z-ADD     1             MM
189200050118     C                   SETOFF                                           28
189300050118      *
189400050118     c                   Select
189500050118     c                   when      nrPEV = 1
189600050118     C                   eval      $mes = pesMES
189700050118     c                   when      nrPEV = 2
189800050118     C                   eval      $mes = vlmMES
189900050118     c                   endSL
190000050118      *
190100050118     C     $mes          LOOKUP    MES(MM)                                28
190200050118IF  2C     *IN28         IFEQ      *ON                                          *MESE TROVATO
190300050118     C*
190400050118     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
190500050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
190600050118     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
190700050118     C                   MOVEL     '1'           PASpev                         *PER Peso Volume
190800050118     C* O C U R
190900050118     C     nrPEV         OCCUR     CPEV
191000050118     C*
191100050118     c                   if        nrPEV = 1
191200050118     C                   ADD       pesNCV        NCV(MM)                        *VALORI DEL MESE
191300050118     C                   ADD       pesTCL        NCB(MM)
191400050118     C                   ADD       pesPSC        PVV(MM)
191500050118     C                   ADD       pesPSB        PVB(MM)
191600050118     c                   end
191700050118     C*
191800050118     c                   if        nrPEV = 2
191900050118     C                   ADD       vlmNCR        NCV(MM)                        *VALORI DEL MESE
192000050118     C                   ADD       vlmTCL        NCB(MM)
192100050118     C                   ADD       vlmVLC        PVV(MM)
192200050118     C                   ADD       vlmVLB        PVB(MM)
192300050118     c                   end
192400050118     C*
192500050118E   2C                   ENDIF
192600050118     C*
192700050118E   1C                   ENDIF
192800050118     C*
192900050118     C                   ENDSR
193000000000     C*------------------------------------------------------------------------*
193100000000     C*?CICAMM - CICLO MEMORIZZAZIONE PARTICOLARITA' SALDI PER LA "AMMNINSTR.NE"
193200000000     C*------------------------------------------------------------------------*
193300000000     C     CICAMM        BEGSR
193400000000     C*
193500000000     C* POSIZIONAMENTO E PRIMA LETTURA PARTICOLARITA' SALDI CLIENTE
193600000000     C                   EXSR      SETFI3
193700000000     C*
193800000000     C* CICLO FINO A FINE FILE PARTICOLARITA' SALDI CLIENTE
193900000000DO  1C     $FINP         DOWEQ     '0'
194000000000     C*
194100000000     C* MEMORIZZA LE PARTICLARITA' SALDI DEL CLIENTE
194200000000     C                   EXSR      MEMSDP
194300000000     C*
194400000000     C* LETTURA PARTICOLARITA' SALDI DEL CLIENTE SUCESSIVO
194500000000     C                   EXSR      LETFI3
194600000000     C*
194700000000E   1C                   ENDDO
194800000000     C*
194900000000     C                   ENDSR
195000000000     C*------------------------------------------------------------------------*
195100000000     C*?SETFI3 - POSIZIONAMENTO E PRIMA LETTURA PARTICOLARITA' SALDI CLIENTE
195200000000     C*------------------------------------------------------------------------*
195300000000     C     SETFI3        BEGSR
195400000000     C*
195500000000     C* REIMPOSTO IL FINE FILE PER CICLO PARTICOLARITA' SALDI CLIENTE
195600000000     C                   MOVEL     '0'           $FINP
195700000000     C*
195800000000     C                   Z-ADD     DEPKSC        KSPKSC                         *DEPOSITO CLIENTE
195900000000     C                   Z-ADD     ANNINI        KSPANN                         *ANNO INIZIALE
196000000000     C                   Z-ADD     MESINI        KSPMES                         *MESE INIZIALE
196100000000     C*
196200000000     C* POSIZIONAMENTO FILE PARTICOLARITA' SALDI CLIENTE (KSC+ANN+MES)
196300990609     C     KEYSDP        SETLL     SISDP01L                           99        *FINE ARCHIVIO
196400000000     C*
196500000000IF  1C     *IN99         IFEQ      '1'
196600000000     C                   MOVEL     '1'           $FINP
196700000000X   1C                   ELSE
196800000000     C                   EXSR      LETFI3                                       *LETTURA RECORD
196900000000E   1C                   ENDIF
197000000000     C*
197100000000     C                   ENDSR
197200000000     C*------------------------------------------------------------------------*
197300000000     C*?LETFI3 - LETTURA PROSSIMO RECORD FILE PARTICOLARITA' SALDI CLIENTE
197400000000     C*------------------------------------------------------------------------*
197500000000     C     LETFI3        BEGSR
197600000000     C*
197700000000     C* LEGGO FINO A:
197800000000     C*     - FINE FILE
197900000000     C*     - TROVATO RECORD VALIDO
198000000000     C                   MOVEL     'N'           WRECOK
198100000000DO  1C     $FINP         DOWEQ     '0'
198200000000     C     WRECOK        ANDEQ     'N'
198300000000     C*
198400990609     C     KSPKSC        READE     SISDP01L                               99
198500000000     C*
198600000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
198700000000     C                   MOVEL     '1'           $FINP
198800000000X   2C                   ELSE
198900000000     C                   EXSR      CHKRE3                                       *CONTROLLO RECORD
199000000000E   2C                   ENDIF
199100000000     C*
199200000000E   1C                   ENDDO
199300000000     C*
199400000000     C                   ENDSR
199500000000     C*------------------------------------------------------------------------*
199600000000     C*?CHKRE3 - CONTROLLA VALIDITA' RECORD PARTICOLARITA' SALDI CLIENTE
199700000000     C*------------------------------------------------------------------------*
199800000000     C     CHKRE3        BEGSR
199900000000     C*
200000000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
200100000000     C*--------------------
200200990601     C* DATA (AAAA/MM)
200300000000     C*--------------------
200400000000     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
200500020823IF  1C     SDPDAT        IFGE      D60DAT
200600020823     C     SDPDAT        ANDLE     D60DA2
200700020823X   1C                   ELSE
200800020823     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
200900020823IF  2C     SDPDAT        IFGT      D60DA2
201000020823     C                   MOVEL     '1'           $FINP                          *ESCO DAL CICLO "SDP
201100020823E   2C                   ENDIF
201200020823E   1C                   ENDIF
201300000000     C*--------------------
201400000000     C* CODICE TARIFFA
201500000000     C*--------------------
201600000000     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
201700000000IF  1C     SELCTR        IFNE      'T'
201800000000     C                   MOVEL     SDPCTR        $CTR
201900000000IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
202000000000     C     $CTR          OREQ      D60CT2
202100000000     C     $CTR          OREQ      D60CT3
202200000000     C     $CTR          OREQ      D60CT4
202300000000     C     $CTR          OREQ      D60CT5
202400000000     C     $CTR          OREQ      D60CT6
202500000000     C     $CTR          OREQ      D60CT7
202600000000     C     $CTR          OREQ      D60CT8
202700000000     C     $CTR          OREQ      D60CT9
202800000000     C     $CTR          OREQ      D60CT0
202900000000X   2C                   ELSE
203000000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
203100000000E   2C                   ENDIF
203200000000E   1C                   ENDIF
203300000000     C*
203400000000     C                   ENDSR
203500000000     C*------------------------------------------------------------------------*
203600990609     C*?MEMSDP - MEMORIZZA I SALDI DI SISDP01L
203700000000     C*------------------------------------------------------------------------*
203800000000     C     MEMSDP        BEGSR
203900000000     C* MESE
204000000000     C                   Z-ADD     1             MM
204100000000     C                   SETOFF                                           28
204200000000     C     SDPMES        LOOKUP    MES(MM)                                28
204300000000IF  1C     *IN28         IFEQ      *ON                                          *MESE TROVATO
204400000000     C*
204500000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
204600050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
204700000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
204800000000     C                   MOVEL     '1'           PASAMM                         *PER AMMINISTRAZIONE
204900970724     C*
205000970724     C* MEMORIZZA TUTTE LE PARTICOLARITA' CLIENTE
205100000000     C                   Z-ADD     1             J
205200000000     C                   SETOFF                                           28
205300000000     C     SDPSVR        LOOKUP    VAR(J)                                 28
205400000000IF  2C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATA
205500000000     C                   ADD       1             P
205600000000     C                   MOVEL     SDPSVR        VAR(P)
205700000000     C                   Z-ADD     P             J
205800000000E   2C                   ENDIF
205900000000     C* O C U R
206000000000     C     J             OCCUR     CAMM
206100000000     C                   ADD       SDPRCP        ARI(MM)                        *RICAVI
206200000000     C                   ADD       SDPNSP        ASP(MM)                        *SPEDIZIONI
206300970724     C*
206400990609     C* PRIMA DEL 97 -> FATTURATO = RICAVI SISDC + SISDP
206500990609     C* DOPO   IL 97 -> FATTURATO = RICAVI SISDP
206600990602IF  2C     SDPANN        IFGE      1997                                         *DOLO IL
206700970724     C     AFC(MM)       ADD       SDPRCP        AFC(MM)                        *FATTURATO TOTALE
206800970724X   2C                   ELSE                                                   *PRIMA DEL
206900970724IF  3C     SDPSVR        IFNE      'G'
207000970724     C     SDPSVR        ANDNE     'E'
207100970724     C     SDPSVR        ANDNE     'R'
207200970724     C     SDPSVR        ANDNE     'P'
207300970724     C     SDPSVR        ANDNE     'A'
207400970724     C     SDPSVR        ANDNE     'S'
207500970724     C     SDPSVR        ANDNE     'F'
207600010920     C     SDPSVR        ANDNE     'Q'
207700010920     C     SDPSVR        ANDNE     'K'
207800010920     C     SDPSVR        ANDNE     'B'
207900010920     C     SDPSVR        ANDNE     'U'
208000010921     C     SDPSVR        ANDNE     'J'
208100010920     C     SDPSVR        ANDNE     'X'
208200010920     C     SDPSVR        ANDNE     'Y'
208300020205     C     SDPSVR        ANDNE     'O'
208400040924     C     SDPSVR        ANDNE     'a'
208500020205     C     SDPSVR        ANDNE     '5'
208600970724     C     AFC(MM)       ADD       SDPRCP        AFC(MM)                        *FATTURATO TOTALE
208700970724E   3C                   ENDIF
208800970724E   2C                   ENDIF
208900970724E   1C                   ENDIF
209000000000     C*
209100000000     C                   ENDSR
209200000000     C*------------------------------------------------------------------------*
209300000000     C*?STACLI - IMPOSTA STAMPA STATISTICHE PER IL CLIENTE
209400000000     C*------------------------------------------------------------------------*
209500000000     C     STACLI        BEGSR
209600000000     C*
209700000000     C* STAMPO SOLO DE HO MEMORIZZATO QC PER LE SINGOLE STATISTICHE
209800000000IF  1C     PASSER        IFEQ      '1'
209900000000     C                   EXSR      STASER                                       *TOTALI
210000000000E   1C                   ENDIF
210100000000     C*
210200050118IF  1C     PASPEV        IFEQ      '1'
210300050118     C                   EXSR      STAPEV                                       *PESI-VOLUMI
210400000000E   1C                   ENDIF
210500050118     C*
210600050118IF  1C     PASREG        IFEQ      '1'
210700050118     C                   EXSR      STAREG                                       *REGIONE-REGIONE
210800050118E   1C                   ENDIF
210900000000     C*
211000000000IF  1C     PASDEL        IFEQ      '1'
211100000000     C                   EXSR      STADEL                                       *DELTA
211200000000E   1C                   ENDIF
211300000000     C*
211400000000IF  1C     PASAFF        IFEQ      '1'
211500000000     C                   EXSR      STAAFF                                       *AFFIDABILITA'
211600000000E   1C                   ENDIF
211700000000     C*
211800000000IF  1C     PASAMM        IFEQ      '1'
211900000000     C                   EXSR      STAAMM                                       *AMMINISTRAZIONE
212000000000E   1C                   ENDIF
212100000000     C*
212200000000     C                   ENDSR
212300020524     C*------------------------------------------------------------------------*
212400020524     C*?STATES - STAMPA TESTATA
212500020524     C*------------------------------------------------------------------------*
212600020524     C     STATES        BEGSR
212700020524     C*
212800020524IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
212900020823IF  2C                   IF        D60OP1 <> 'STT'                              *NO PER OTTICO
213000071210      *
213100020524     C                   WRITE     SE62PT
213200071210      *
213300020823E   2C                   ENDIF
213400020823IF  2C                   IF        D60OP1 = 'STT'                               *SI PER OTTICO
213500020823     C                   Z-ADD     DEPPAG        CL2PAG
213600071210      *
213700020823     C                   WRITE     SE62C2
213800170329     C                   z-add     wAA           AR2AAA
213900170329     C                   WRITE     SE62A2
214000071210      *
214100020823     C                   ADD       1             DEPPAG                         *PAGINE PER CLIENTE
214200020823E   2C                   ENDIF
214300020823IF  2C     WSTINT        IFEQ      '1'                                          *INTESTAZIONE DA STA
214400071210      *
214500020823     C                   WRITE     SE62PI
214600071210      *
214700020826E   2C                   ENDIF
214800020823E   1C                   ENDIF
214900020524     C*
215000020823     C* DOPO reimposta campi di lavoro
215100020823     C                   MOVEL     '0'           *IN01                            *NO OVERFLW
215200020823     c                   MOVEL     '1'           WSTINT                           *SI STAMPA INTESTA
215300020823     C*
215400020524     C                   ENDSR
215500000000     C*------------------------------------------------------------------------*
215600000000     C*?STASER - IMPOSTA STAMPA STATISTICA "TOTALI"
215700000000     C*          INDICI: J(4,0) --> TIPO VALORE IN LINEA (1÷5)
215800000000     C*                  I(4,0) --> TIPO SERVIZIO SELEZIONATO/MEDIA DA STAMPARE
215900000000     C*------------------------------------------------------------------------*
216000000000     C     STASER        BEGSR
216100071211     C*--------------------          0
216200000000     C* PARAMETRI
216300000000     C*--------------------
216400000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
216500000000     C                   EXSR      STAPAR
216600000000E   1C                   ENDIF
216700000000     C*--------------------
216800000000     C* DATI GENERALI CLIENTE
216900000000     C*--------------------
217000000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
217100000000     C                   EXSR      STADGC
217200000000E   1C                   ENDIF
217300000000     C*--------------------
217400000000     C* INTESTAZIONE
217500000000     C*--------------------
217600000000     C                   MOVEL     DRI(2)        PICSTA
217700071210      *
217800071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
217900071211     c                   eval      Righe_da_stampare = 5
218000071211     c                   exsr      solo_x_STT
218100010920     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
218200000000     C*
218300000000     C* PER OGNI VALORE DA STAMPARE
218400000000     C                   Z-ADD     1             J
218500000000DO  1C     1             DO        5             J
218600000000     C*
218700000000     C* SOMMO TUTTI I TIPI SERVIZIO NELLA SCHIERA MULTIPLA
218800000000     C     J             OCCUR     CSER
218900000000     C     J             OCCUR     WSER
219000000000     C                   Z-ADD     SNC           VAW                            CORRIERE +
219100000629     C     VAW           ADD       SDC           VAW                            DISTRIBUZIONE +
219200000629     C     VAW           ADD       SEC           VAW                            ESPRESSO +
219300091013     C     VAW           ADD       SHC           VAW                            ESPR.10:30 +
219400000629     C     VAW           ADD       SPC           VAW                            POSTE = VALORE TOTAL
219500000000     C*
219600000000     C* METTO I TOTALI DEI SINGOLI VALORI IN SCHIERA (PER LE MEDIE)
219700000000IF  2C     J             IFEQ      1
219800000000     C                   Z-ADD     VAW           SPW                            *SPEDIZIONI
219900000000E   2C                   ENDIF
220000000000IF  2C     J             IFEQ      2
220100000000     C                   Z-ADD     VAW           COW                            *COLLI
220200000000E   2C                   ENDIF
220300000000IF  2C     J             IFEQ      3
220400000000     C                   Z-ADD     VAW           PEW                            *PESO IN Q.LI
220500000000E   2C                   ENDIF
220600000000IF  2C     J             IFEQ      4
220700000000     C                   Z-ADD     VAW           VOW                            *VOLUME IN M.CUBI
220800000000E   2C                   ENDIF
220900000000IF  2C     J             IFEQ      5
221000010921     C                   Z-ADD     VAW           RIW                            *RICAVI
221100000000E   2C                   ENDIF
221200000000     C*
221300000000E   1C                   ENDDO
221400000000     C*
221500000000     C* PER OGNI TIPO VALORE DA STAMPARE (SPEDIZIONI/COLLI/PESO/VOLUME/RICAVI)
221600000000     C                   Z-ADD     1             J
221700000000DO  1C     1             DO        5             J
221800000000     C     J             OCCUR     CSER
221900000000     C*
222000000000     C                   MOVEL     '0'           NODESV            1            *SI DESCRIZ VALORE
222100000000     C                   MOVEL     DCA(J)        DPRIMO                         *DESCRIZIONE VALORE
222200000000     C                   MOVEL     *BLANKS       TIPVAL
222300000000     C*
222400000000IF  2C     J             IFEQ      4
222500000000     C                   MOVEL     'VOL'         TIPVAL            3            *STAMPO I VOLUMI
222600000000E   2C                   ENDIF
222700000000IF  2C     J             IFEQ      5
222800000000     C                   MOVEL     'RIC'         TIPVAL                         *STAMPO I RICAVI
222900000000E   2C                   ENDIF
223000000000     C*
223100000000     C* PER OGNI TIPO SERVIZIO SELEZIONATO
223200000000     C                   Z-ADD     1             I
223300000000DO  2C     1             DO        SPARS         I
223400000000     C*
223500000000IF  3C     I             IFGE      2                                            *TIPO VALORE 1 VOLTA
223600000000     C     NODESV        ANDEQ     '1'                                          *SE SCRITTO 1 VOLTA
223700000000     C                   MOVEL     *BLANKS       DPRIMO
223800000000E   3C                   ENDIF
223900000000     C*
224000000000     C* DESCRIZIONE TIPO SERVIZIO
224100000000     C                   Z-ADD     1             JJ
224200000000     C                   SETOFF                                           28
224300000000     C     SERS(I)       LOOKUP    SER(JJ)                                28
224400000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
224500000000     C                   MOVEL     *BLANKS       DSECON
224600000000X   3C                   ELSE
224700000000     C                   MOVEL     DSER(JJ)      DSECON                         *DESCRIZIONE SERVIZI
224800000000E   3C                   ENDIF
224900000000     C*
225000000000IF  3C     SERS(I)       IFEQ      'C'
225100000000     C                   Z-ADD     SNC           $VA                            *CORRIERE
225200000000X   3C                   ELSE
225300000000IF  4C     SERS(I)       IFEQ      'E'
225400000000     C                   Z-ADD     SEC           $VA                            *ESPRESSO
225500000000X   4C                   ELSE
225600000000IF  5C     SERS(I)       IFEQ      'D'
225700000629     C                   Z-ADD     SDC           $VA                            *DISTRIBUZIONE
225800000629X   5C                   ELSE
225900091013IF  5C     SERS(I)       IFEQ      'H'
226000091013     C                   Z-ADD     SHC           $VA                            *Espresso H10:30
226100091013X   5C                   ELSE
226200000629IF  6C     SERS(I)       IFEQ      'P'
226300000629     C                   Z-ADD     SPC           $VA                            *POSTE
226400000629E   6C                   ENDIF
226500091013E   6C                   ENDIF
226600000629E   5C                   ENDIF
226700000000E   4C                   ENDIF
226800000000E   3C                   ENDIF
226900000000     C*--------------------
227000000000     C* STAMPA RIGA
227100000000     C*--------------------
227200000000     C* STAMPA LA RIGA SOLO SE HO MEMORIZZATO QUALCOSA
227300000000     C                   XFOOT     $VA           COM$VA           16 3
227400000000IF  3C     COM$VA        IFNE      *ZEROS
227500000000     C                   MOVEL     '1'           NODESV            1            *NO DESCRIZ VALORE
227600000000     C     DPRIMO        CAT       DSECON        VALSER           23            *VALORE + SERVIZIO
227700000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
227800000000     C                   EXSR      STASEX
227900000000E   3C                   ENDIF
228000000000     C*
228100000000E   2C                   ENDDO
228200000000     C*--------------------
228300000629     C* STAMPA RIGA TOTALE
228400000000     C*--------------------
228500000000     C* SE HO STAPATO QC. PRIMA DEI VALORI LA RIGA DEI TOTALI NON DEVE ESSERE
228600000000     C* PRECEDUTA DALLA DESCRIZIONE VALORE, ALTRIMENTI SI
228700000000     C     J             OCCUR     WSER
228800000000IF  2C     NODESV        IFEQ      '1'
228900000000     C                   MOVEL     *BLANKS       DPRIMO
229000000000X   2C                   ELSE
229100000000     C                   MOVEL     DCA(J)        DPRIMO                         *DESCRIZIONE VALORE
229200000000E   1C                   ENDIF
229300000000     C*
229400000000     C                   MOVEL     DCA(6)        DSECON                         *VALORE "TOTALE"
229500000000     C     DPRIMO        CAT       DSECON        VALSER                         *VALORE + SERVIZIO
229600000000     C                   Z-ADD     VAW           $VA                            *TOTALE VALORE
229700000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
229800000000     C                   EXSR      STASEX
229900000000     C*
230000071211     c                   eval      Righe_da_stampare = 1
230100071211     c                   exsr      solo_x_STT
230200010920     C                   WRITE     SE62RI                                       *RIGA DI SEPARAZIONE
230300071210      *
230400020823     C                   EXSR      STATES
230500000000E   1C                   ENDDO
230600000000     C*--------------------
230700040317     C* CALCOLA MEDIE E COLLI ORIGINALI
230800000000     C*--------------------
230900000000     C                   Z-ADD     1             I
231000000000DO  1C     1             DO        9             I                              *MEDIA DA STAMPARE
231100000000     C* PER OGNI MESE
231200000000     C                   Z-ADD     1             J
231300000000DO  2C     1             DO        SPARM         J
231400000000     C                   Z-ADD     SPW(J)        WSPW             16 3          *SPEDIZIONI DEL MESE
231500000000     C                   Z-ADD     COW(J)        WCOW             16 3          *COLLI DEL MESE
231600000000     C                   Z-ADD     PEW(J)        WPEW             16 3          *PESO IN Q DEL MESE
231700000000     C                   Z-ADD     VOW(J)        WVOW             16 3          *VOLUME MC DEL MESE
231800010921     C                   Z-ADD     RIW(J)        WRIW             16 3          *RICAVI DEL MES
231900000000     C                   Z-ADD     *ZEROS        WMED             16 3          *MEDIA DA CALCOLARE
232000000000     C* CALCOLA LE MEDIE
232100000000     C                   EXSR      CALMED
232200000000     C                   Z-ADD     WMED          MEW(J)                         *MEDIA DEL MESE
232300000000E   2C                   ENDDO
232400000000     C*--------------------
232500000000     C* STAMPA MEDIE
232600000000     C*--------------------
232700000000IF  2C     I             IFLE      5
232800010921     C                   MOVEL     'MED'         TIPVAL                         *COME PER I RICAVI
232900000000X   2C                   ELSE
233000000000     C                   MOVEL     'ME1'         TIPVAL                         *COME PER I VOLUMI
233100000000E   2C                   ENDIF
233200000000     C                   Z-ADD     MEW           $VA
233300000000     C                   MOVEL     DME(I)        $DE
233400000000     C                   Z-ADD     I             $I                4 0
233500000000     C                   XFOOT     SPW           $SP              16 3          *TOTALE SPEDIZIONI
233600000000     C                   XFOOT     COW           $CO              16 3          *TOTALE COLLI
233700000000     C                   XFOOT     PEW           $PE              16 3          *TOTALE PESO IN Q.LI
233800000000     C                   XFOOT     VOW           $VO              16 3          *TOTALE VOLUME
233900010921     C                   XFOOT     RIW           $RI              16 3          *TOTALE RICAVI
234000000000     C*
234100000000     C                   EXSR      STASEX
234200000000E   1C                   ENDDO
234300040317     C*--------------------
234400040317     C* COLLI ORIGINALI (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
234500040317     C*--------------------
234600040317     C                   MOVEL     'COL'         TIPVAL                         *COLLI
234700040317     C                   MOVEL     DME(43)       $DE                            *DESCRIZIONE
234800040317     C                   Z-ADD     COR           $VA
234900040317     C                   XFOOT     $VA           COM$VA           16 3
235000040317IF  1C     COM$VA        IFNE      *ZEROS
235100040317     C                   EXSR      STASEX
235200040317E   1C                   ENDIF
235300040318     C*--------------------
235400040318     C* BANCALI BNB (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
235500040318     C*--------------------
235600040318     C                   MOVEL     DME(44)       $DE                            *DESCRIZIONE
235700040318     C                   Z-ADD     BNB           $VA
235800040318     C                   XFOOT     $VA           COM$VA           16 3
235900040318IF  1C     COM$VA        IFNE      *ZEROS
236000040318     C                   EXSR      STASEX
236100040318E   1C                   ENDIF
236200040318     C*--------------------
236300040318     C* BANCALI BAN (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
236400040318     C*--------------------
236500040318     C                   MOVEL     DME(45)       $DE                            *DESCRIZIONE
236600040318     C                   Z-ADD     BAN           $VA
236700040318     C                   XFOOT     $VA           COM$VA           16 3
236800040318IF  1C     COM$VA        IFNE      *ZEROS
236900040318     C                   EXSR      STASEX
237000040318E   1C                   ENDIF
237100040317     C*
237200071211     c                   eval      Righe_da_stampare = 1
237300071211     c                   exsr      solo_x_STT
237400040317     C                   WRITE     SE62RI                                       *RIGA CHIUSURA
237500071210      *
237600040317     C                   EXSR      STATES
237700040317     C*
237800000000     C                   ENDSR
237900000000     C*------------------------------------------------------------------------*
238000000000     C*?STASEX - IMPOSTA STAMPA RIGA XXX STATISTICA SERVIZIO BOLLE
238100000000     C*          INPUT --> $VA  : SCHIERA VALORI DI 12 MESI
238200000000     C*                    $DE  : DESCRIZIONE RIGA DA STAMPARE
238300000000     C*                    $I   : TOTALE MEDIA DA STAMPARE
238400000000     C*                    $SP  : TOTALE SPEDIZIONI
238500000000     C*                    $CO  : TOTALE COLLI
238600000000     C*                    $PE  : TOTALE PESO IN Q.LI
238700000000     C*                    $VO  : TOTALE VOLUME
238800010921     C*                    $RI  : TOTALE RICAVI
238900000000     C*------------------------------------------------------------------------*
239000000000     C     STASEX        BEGSR
239100000000     C*
239200000000     C*--------------------
239300000000     C* 13^ C O L O N N A
239400000000     C*--------------------
239500000000     C* NB: PER IL CALCOLO DELLA 13^ COL DELLE MEDIE NON POSSO FARE LA SOMMA
239600000000IF  1C     TIPVAL        IFNE      'MED'                                        N O   M E D I E
239700000000     C     TIPVAL        ANDNE     'ME1'
239800000000     C                   XFOOT     $VA           $VA13C           16 3
239900000000X   1C                   ELSE                                                   M E D I E
240000000000     C                   Z-ADD     $SP           WSPW                           *SPEDIZIONI TOTALI
240100000000     C                   Z-ADD     $CO           WCOW                           *COLLI TOTALI
240200000000     C                   Z-ADD     $PE           WPEW                           *PESO IN QLI TOTALE
240300000000     C                   Z-ADD     $VO           WVOW                           *VOLUME MC TOTALE
240400010921     C                   Z-ADD     $RI           WRIW                           *RICAVI TOTALI
240500000000     C                   Z-ADD     *ZEROS        WMED             16 3          *MEDIA DA CALCOLARE
240600000000     C                   Z-ADD     $I            I                              *MEDIA DA CALCOLARE
240700000000     C* CALCOLA LE MEDIE
240800000000     C                   EXSR      CALMED
240900000000     C                   Z-ADD     WMED          $VA13C                         *MEDIA TOTALE
241000000000E   1C                   ENDIF
241100000000     C*
241200010920IF  1C***  TIPVAL        IFEQ      'RIC'                                        R I C A V I
241300010920     C***  $VA           DIV       1000          $VA                            *RICAVI DIVISO 1000
241400010920     C***  $VA13C        DIV       1000          $VA13C                         *13^COL DIVISO 1000
241500010920E   1C***                ENDIF
241600000000     C*
241700000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
241800000000IF  1C     TIPVAL        IFEQ      'VOL'                                        V O L U M E
241900010921     C     TIPVAL        OREQ      'MED'                                        M E D I E
242000000000     C                   EXSR      STACAV                                       *RIGA VOLUMI E MEDIE
242100000000X   1C                   ELSE
242200020131IF  2C     TIPVAL        IFEQ      'ME1'
242300020131     C                   EXSR      STACAR1
242400020131     C                   ELSE
242500020131IF  3C     TIPVAL        IFEQ      'RIC'                                        R I C A V I
242600010920     C                   EXSR      STACAR
242700020131X   3C                   ELSE
242800000000     C                   EXSR      STACAN                                       *RIGA ALTRI VALORI
242900020131E   3C                   ENDIF
243000020131E   2C                   ENDIF
243100000000E   1C                   ENDIF
243200000000     C*
243300000000     C                   ENDSR
243400050118     C*------------------------------------------------------------------------*
243500050118     C*?STAPEV - IMPOSTA STAMPA STATISTICA "PESI-VOLUME"
243600050119     C*          INDICI:  J(4,0) --> MESI
243700050118     C*------------------------------------------------------------------------*
243800050118     C     STAPEV        BEGSR
243900050118     C*--------------------
244000050118     C* PARAMETRI
244100050118     C*--------------------
244200050118IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
244300050118     C                   EXSR      STAPAR
244400050118E   1C                   ENDIF
244500050118     C*--------------------
244600050118     C* DATI GENERALI CLIENTE
244700050118     C*--------------------
244800050118IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
244900050118     C                   EXSR      STADGC
245000050118E   1C                   ENDIF
245100050118     C*--------------------
245200050118     C* INTESTAZIONE PESI
245300050118     C*--------------------
245400050118     C                   MOVEL     DRI(6)        PICSTA
245500071210      *
245600071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
245700071211     c                   eval      Righe_da_stampare = 5
245800071211     c                   exsr      solo_x_STT
245900050121     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
246000050119     C*
246100050118     C*--------------------
246200050118     C* PESI             -->
246300050118     C*--------------------
246400050118     C     1             OCCUR     Cpev
246500050121     C*
246600050121     C*   Intestazione Pesi
246700050121     C                   MOVEL     DME(60)       $DE
246800050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
246900050121     C                   z-add     *ZEROS        COM$VA           16 3
247000050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
247100050121     C                   EXSR      STAAMX
247200050121     C                   EXSR      STATES
247300050118     C*
247400050119     C*--------------------
247500050119     C* PESI % Colli Ril.
247600050119     C*--------------------
247700050119     C* PER OGNI MESE CALCOLO LA PERCENTUALE
247800050119     C                   Z-ADD     1             J
247900050119DO  2C     1             DO        SPARM         J
248000050119     C                   Z-ADD     NCB(J)        WFAW             16 3          *Nr.Colli VDL
248100050119     C                   Z-ADD     NCV(J)        WNRW             16 3          *Nr.Colli Boll.
248200050119     C                   Z-ADD     *ZEROS        WPDW              4 1          *PERCENTUALE
248300050119     C                   EXSR      CALPER
248400050119     C                   Z-ADD     WPDW          APW(J)                         *SCHIERA PERCENTUALI
248500050119E   2C                   ENDDO
248600050119     C*
248700050119     C                   MOVEL     DME(54)       $DE
248800050121     c*****              move      'PES'         $DE
248900050119     C                   MOVEL     'PER'         TIPVAL                         *PERCENTUALI
249000050119     C                   Z-ADD     APW           $VA
249100050119     C*
249200050119     C* CALCOLO I TOTALI FATTURATO E NOTE PER 13^ COLONNA
249300050119     C                   XFOOT     NCB           $FA              16 3
249400050119     C                   XFOOT     NCV           $NR              16 3
249500050119     C                   EXSR      STAAMX
249600050119     C                   EXSR      STATES
249700050121     C*--------------------
249800050121     C* PESI   D E L T A   % SCOST.Ril./Boll.
249900050121     C*--------------------
250000050121     C* PER OGNI MESE CALCOLO IL DELTA
250100050121     C                   Z-ADD     1             J
250200050121DO  2C     1             DO        SPARM         J
250300050121     C                   Z-ADD     PVB(J)        WRRW             16 3          *Peso Boll.
250400050121     C                   Z-ADD     PVV(J)        WRPW             16 3          *Peso VDL
250500050121     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
250600050121     C                   EXSR      CALDEL
250700050121     C                   Z-ADD     WDPW          DDW(J)                         *SCHIERA DELTA
250800050121E   2C                   ENDDO
250900050121     C*
251000050121     C                   MOVEL     'DEL'         TIPVAL                         *VALORE DELTA
251100050121     C                   Z-ADD     DDW           $VA
251200050121     C                   MOVEL     DME(55)       $DE              23            *DESCRIZIONE RIGA
251300050121     c*****              move      'PES'         $DE
251400050121     C                   XFOOT     PVB           $RR              16 3          *Peso Boll.
251500050121     C                   XFOOT     PVV           $RP              16 3          *Peso VDL
251600050121     C                   EXSR      STADEX
251700050121     C                   EXSR      STATES
251800050121     C*
251900050121     C                   if        d60dpv = 'S'
252000050121     C*--------------------
252100050121     C* PESI Nr.Colli VDL
252200050121     C*--------------------
252300050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
252400050121     C                   ADD       NCV           $VA                            *VALORI RIGA
252500050121     C                   XFOOT     $VA           COM$VA           16 3
252600050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
252700050121IF  1C     COM$VA        IFNE      *ZEROS
252800050121     C                   MOVEL     DME(52)       $DE
252900050121     c*****              move      'PES'         $DE
253000050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
253100050121     C                   EXSR      STAAMX
253200050121     C                   EXSR      STATES
253300050121E   1C                   ENDIF
253400050121     C*--------------------
253500050121     C* PESI Nr.Colli Boll.
253600050121     C*--------------------
253700050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
253800050121     C                   ADD       NCB           $VA                            *VALORI RIGA
253900050121     C                   XFOOT     $VA           COM$VA           16 3
254000050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
254100050121IF  1C     COM$VA        IFNE      *ZEROS
254200050121     C                   MOVEL     DME(53)       $DE
254300050121     c*****              move      'PES'         $DE
254400050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
254500050121     C                   EXSR      STAAMX
254600050121     C                   EXSR      STATES
254700050121E   1C                   ENDIF
254800050121     C*--------------------
254900050121     C* PESI Pesi VDL
255000050121     C*--------------------
255100050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
255200050121     C                   ADD       PVV           $VA                            *VALORI RIGA
255300050121     C                   XFOOT     $VA           COM$VA           16 3
255400050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
255500050121IF  1C     COM$VA        IFNE      *ZEROS
255600050121     C                   MOVEL     DME(56)       $DE
255700050121     c*****              move      'PES'         $DE
255800050121     C                   MOVEL     'VOL'         TIPVAL                         *PESI/VOLUMI
255900050121     C                   EXSR      STAAMX
256000050121     C                   EXSR      STATES
256100050121E   1C                   ENDIF
256200050121     C*--------------------
256300050121     C* PESI Pesi Boll.
256400050121     C*--------------------
256500050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
256600050121     C                   ADD       PVB           $VA                            *VALORI RIGA
256700050121     C                   XFOOT     $VA           COM$VA           16 3
256800050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
256900050121IF  1C     COM$VA        IFNE      *ZEROS
257000050121     C                   MOVEL     DME(57)       $DE
257100050121     c*****              move      'PES'         $DE
257200050121     C                   MOVEL     'VOL'         TIPVAL                         *PESI/VOLUMI
257300050121     C                   EXSR      STAAMX
257400050121     C                   EXSR      STATES
257500050121E   1C                   ENDIF
257600050121      **
257700050121E   1C                   End
257800050119     C*
257900050118     C*--------------------
258000050118     C* VOLUMI           -->
258100050118     C*--------------------
258200050118     C     2             OCCUR     Cpev
258300050121     C*
258400050121     C*   Intestazione Volumi
258500050121     C                   MOVEL     DME(61)       $DE
258600050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
258700050121     C                   z-add     *ZEROS        COM$VA           16 3
258800050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
258900050121     C                   EXSR      STAAMX
259000050121     C                   EXSR      STATES
259100050121      *
259200050119     C*--------------------
259300050119     C* VOLUMI % Colli Ril.
259400050119     C*--------------------
259500050119     C* PER OGNI MESE CALCOLO LA PERCENTUALE
259600050119     C                   Z-ADD     1             J
259700050119DO  2C     1             DO        SPARM         J
259800050119     C                   Z-ADD     NCB(J)        WFAW             16 3          *Nr.Colli VDL
259900050119     C                   Z-ADD     NCV(J)        WNRW             16 3          *Nr.Colli Boll.
260000050119     C                   Z-ADD     *ZEROS        WPDW              4 1          *PERCENTUALE
260100050119     C*
260200050119     C                   EXSR      CALPER
260300050119     C                   Z-ADD     WPDW          APW(J)                         *SCHIERA PERCENTUALI
260400050119E   2C                   ENDDO
260500050119     C*
260600050119     C                   MOVEL     DME(54)       $DE
260700050121     c*****              move      'VOL'         $DE
260800050119     C                   MOVEL     'PER'         TIPVAL                         *PERCENTUALI
260900050119     C                   Z-ADD     APW           $VA
261000050119     C*
261100050119     C* CALCOLO I TOTALI FATTURATO E NOTE PER 13^ COLONNA
261200050119     C                   XFOOT     NCB           $FA              16 3
261300050119     C                   XFOOT     NCV           $NR              16 3
261400050119     C                   EXSR      STAAMX
261500050119     C                   EXSR      STATES
261600050121     C*--------------------
261700050121     C* VOLUMI   D E L T A   % SCOST.Ril./Boll.
261800050121     C*--------------------
261900050121     C* PER OGNI MESE CALCOLO IL DELTA
262000050121     C                   Z-ADD     1             J
262100050121DO  2C     1             DO        SPARM         J
262200050121     C                   Z-ADD     PVB(J)        WRRW             16 3          *Peso Boll.
262300050121     C                   Z-ADD     PVV(J)        WRPW             16 3          *Peso VDL
262400050121     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
262500050121     C                   EXSR      CALDEL
262600050121     C                   Z-ADD     WDPW          DDW(J)                         *SCHIERA DELTA
262700050121E   2C                   ENDDO
262800050121     C*
262900050121     C                   MOVEL     'DEL'         TIPVAL                         *VALORE DELTA
263000050121     C                   Z-ADD     DDW           $VA
263100050121     C                   MOVEL     DME(55)       $DE              23            *DESCRIZIONE RIGA
263200050121     c*****              move      'VOL'         $DE
263300050121     C                   XFOOT     PVB           $RR              16 3          *Peso Boll.
263400050121     C                   XFOOT     PVV           $RP              16 3          *Peso VDL
263500050121     C                   EXSR      STADEX
263600050121     C                   EXSR      STATES
263700050121     C*
263800050121     C                   if        d60dpv = 'S'
263900050121     C*--------------------
264000050121     C* VOLUMI Nr.Colli VDL
264100050121     C*--------------------
264200050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
264300050121     C                   ADD       NCV           $VA                            *VALORI RIGA
264400050121     C                   XFOOT     $VA           COM$VA           16 3
264500050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
264600050121IF  1C     COM$VA        IFNE      *ZEROS
264700050121     C                   MOVEL     DME(52)       $DE
264800050121     c*****              move      'VOL'         $DE
264900050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
265000050121     C                   EXSR      STAAMX
265100050121     C                   EXSR      STATES
265200050121E   1C                   ENDIF
265300050121     C*--------------------
265400050121     C* VOLUMI Nr.Colli Boll.
265500050121     C*--------------------
265600050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
265700050121     C                   ADD       NCB           $VA                            *VALORI RIGA
265800050121     C                   XFOOT     $VA           COM$VA           16 3
265900050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
266000050121IF  1C     COM$VA        IFNE      *ZEROS
266100050121     C                   MOVEL     DME(53)       $DE
266200050121     c*****              move      'VOL'         $DE
266300050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
266400050121     C                   EXSR      STAAMX
266500050121     C                   EXSR      STATES
266600050121E   1C                   ENDIF
266700050121     C*--------------------
266800050121     C* VOLUMI Volumi VDL
266900050121     C*--------------------
267000050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
267100050121     C                   ADD       PVV           $VA                            *VALORI RIGA
267200050121     C                   XFOOT     $VA           COM$VA           16 3
267300050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
267400050121IF  1C     COM$VA        IFNE      *ZEROS
267500050121     C                   MOVEL     DME(58)       $DE
267600050121     c*****              move      'VOL'         $DE
267700050121     C                   MOVEL     'VOL'         TIPVAL                         *PESI/VOLUMI
267800050121     C                   EXSR      STAAMX
267900050121     C                   EXSR      STATES
268000050121E   1C                   ENDIF
268100050121     C*--------------------
268200050121     C* VOLUMI Volumi Boll.
268300050121     C*--------------------
268400050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
268500050121     C                   ADD       PVB           $VA                            *VALORI RIGA
268600050121     C                   XFOOT     $VA           COM$VA           16 3
268700050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
268800050121IF  1C     COM$VA        IFNE      *ZEROS
268900050121     C                   MOVEL     DME(59)       $DE
269000050121     c*****              move      'VOL'         $DE
269100050121     C                   MOVEL     'VOL'         TIPVAL                         *PESI/VOLUMI
269200050121     C                   EXSR      STAAMX
269300050121     C                   EXSR      STATES
269400050121E   1C                   ENDIF
269500050121      *
269600050119E   1C                   End
269700050118      *
269800071211     c                   eval      Righe_da_stampare = 1
269900071211     c                   exsr      solo_x_STT
270000050118     C                   WRITE     SE62RI                                       *RIGA FINE PROSPETTO
270100071210      *
270200050118     C                   EXSR      STATES
270300050118     C*
270400050118     C                   ENDSR
270500000000     C*------------------------------------------------------------------------*
270600000000     C*?STAREG - IMPOSTA STAMPA STATISTICA "REGIONE-REGIONE"
270700000000     C*          INDICI: I(4,0) --> DECODIFICA RAGGRUPPAMENTO
270800000000     C*                  J(4,0) --> RAGGRUPPAMENTO IN LINEA
270900000000     C*                  E(4,0) --> ORDINE LETTURA RAGGRUPPAMENTI
271000000000     C*------------------------------------------------------------------------*
271100000000     C     STAREG        BEGSR
271200000000     C*--------------------
271300000000     C* PARAMETRI
271400000000     C*--------------------
271500000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
271600000000     C                   EXSR      STAPAR
271700950206E   1C                   ENDIF
271800000000     C*--------------------
271900000000     C* DATI GENERALI CLIENTE
272000000000     C*--------------------
272100000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
272200000000     C                   EXSR      STADGC
272300000000E   1C                   ENDIF
272400000000     C*--------------------
272500000000     C* INTESTAZIONE
272600000000     C*--------------------
272700000000     C                   MOVEL     DRI(1)        PICSTA
272800071210      *
272900071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
273000071211     c                   eval      Righe_da_stampare = 5
273100071211     c                   exsr      solo_x_STT
273200010920     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
273300950206     C*
273400020605     C* PER OGNI TIPO DI VALORE RICHIESTO (O UNO O TUTTI)
273500950206DO  1C     1             DO        5             JJ
273600950206     C*
273700950206IF  2C     D60RIF        IFEQ      *BLANKS                                      *TUTTI
273800950206     C     D60RIF        OREQ      VRR(JJ)                                      *VALORE = SELEZIONAT
273900950206     C*
274000010920     C                   CLEAR                   SE62P1
274100950206     C                   MOVEL     DVRR(JJ)      P1DDES
274200071210      *
274300071211     c                   eval      Righe_da_stampare = 1
274400071211     c                   exsr      solo_x_STT
274500020524     C                   WRITE     SE62P1                                       *DESCRIZIONE VALORE
274600071210      *
274700020823     C                   EXSR      STATES
274800000000     C*
274900000000     C* MANDO IN STAMPA PRIMA I RAGGRUPPAMENTI REGIONE PARTENZA E POI GLI ARRIVO
275000020605     C***                Z-ADD     *ZEROS        E                 4 0
275100020605DO  3C***  1             DO        2             E
275200000000     C*
275300020605IF  4C***  E             IFEQ      1
275400020605     C***                MOVEL     'P'           CICLO             1            *PRIMA LA PARTENZA
275500020605X   4C***                ELSE
275600020605     C***                MOVEL     'A'           CICLO                          *POI L'ARRIVO
275700020605E   4C***                ENDIF
275800020603     C*
275900020603     C* ORDINO L'OCCUR "REGIONE-REGIONE" X REGIONE P/A
276000020605     C                   EXSR      ORDCRER
276100000000     C*
276200000000     C* PER OGNI COMBINAZIONE DI REGIONI VALORIZZATA
276300000000     C                   Z-ADD     1             J
276400950206DO  4C     1             DO        R             J
276500000000     C     J             OCCUR     CRER
276600000000     C*
276700000000     C* CONTROLLO COSA ELABORARE (P/A)
276800020605     C***  1             SUBST     REG(J):1      ALF1              1
276900020605IF  5C***  CICLO         IFEQ      ALF1
277000020605     C***                MOVE      REG(J)        WRER                           *...+...
277100020605     C                   MOVE      REX           WRER                           *...+...
277200000000     C*
277300000000     C* IMPOSTA IL CAMPO DECODIFICA DEI DUE RAGGRUPPAMENTI
277400000000     C                   MOVEL     WRER          PRIMO             3 0          *1° RAGGRUPPAMENTO
277500000000     C                   Z-ADD     1             I
277600000000     C                   SETOFF                                           28
277700000000     C     PRIMO         LOOKUP    CRR(I)                                 28
277800950206IF  6C     *IN28         IFEQ      *ON
277900000000     C                   MOVEL     DCRR(I)       DPRIMO           11            *TROVATO
278000950206X   6C                   ELSE
278100000000     C                   MOVEL     *ALL'*'       DPRIMO                         *NON TROVATO
278200950206E   6C                   ENDIF
278300000000     C*
278400000000     C                   MOVE      WRER          SECON             3 0          *2° RAGGRUPPAMENTO
278500000000     C                   Z-ADD     1             I
278600000000     C                   SETOFF                                           28
278700000000     C     SECON         LOOKUP    CRR(I)                                 28
278800950206IF  6C     *IN28         IFEQ      *ON
278900000000     C                   MOVEL     DCRR(I)       DSECON           11            *TROVATO
279000950206X   6C                   ELSE
279100000000     C                   MOVEL     *ALL'*'       DSECON                         *NON TROVATO
279200950206E   6C                   ENDIF
279300000000     C*
279400950725     C     DPRIMO        CAT       '-'           RAGRAG           23
279500950725     C                   MOVE      DSECON        RAGRAG                         *REG-REG
279600000000     C*--------------------
279700000000     C* STAMPA RIGA
279800000000     C*--------------------
279900950206IF  6C     VRR(JJ)       IFEQ      'C'
280000950206     C                   Z-ADD     REC           $VA                            *SCHIERA VALORI
280100950206E   6C                   ENDIF
280200950206IF  6C     VRR(JJ)       IFEQ      'P'
280300950206     C                   Z-ADD     REP           $VA                            *SCHIERA VALORI
280400950206E   6C                   ENDIF
280500950206IF  6C     VRR(JJ)       IFEQ      'R'
280600950206     C                   Z-ADD     RER           $VA                            *SCHIERA VALORI
280700950206E   6C                   ENDIF
280800950206IF  6C     VRR(JJ)       IFEQ      'S'
280900950206     C                   Z-ADD     RES           $VA                            *SCHIERA VALORI
281000950206E   6C                   ENDIF
281100950206IF  6C     VRR(JJ)       IFEQ      'V'
281200950206     C                   Z-ADD     REV           $VA                            *SCHIERA VALORI
281300950206E   6C                   ENDIF
281400000000     C                   MOVEL     RAGRAG        $DE              23            *DESCRIZIONE RIGA
281500000000     C                   EXSR      STAREX
281600000000     C*
281700020605E   5C***                ENDIF
281800950206E   4C                   ENDDO
281900020605E   3C***                ENDDO
282000950206E   2C                   ENDIF
282100950206E   1C                   ENDDO
282200000000     C*
282300071211     c                   eval      Righe_da_stampare = 1
282400071211     c                   exsr      solo_x_STT
282500010920     C                   WRITE     SE62RI                                       *RIGA CHIUSURA
282600071210      *
282700020823     C                   EXSR      STATES
282800000000     C*
282900000000     C                   ENDSR
283000950206     C*------------------------------------------------------------------------*
283100000000     C*?STAREX - IMPOSTA STAMPA RIGA XXX STATISTICA REGIONE
283200000000     C*          INPUT --> $VA    : SCHIERA VALORI DI 12 MESI
283300000000     C*                    $DE    : DESCRIZIONE RIGA DA STAMPARE
283400000000     C*------------------------------------------------------------------------*
283500000000     C     STAREX        BEGSR
283600000000     C*
283700000000     C*--------------------
283800000000     C* 13^ C O L O N N A
283900000000     C*--------------------
284000000000     C                   XFOOT     $VA           $VA13C           16 3
284100000000     C*
284200010920IF  1C***  VRR(JJ)       IFEQ      'R'                                          R I C A V I
284300010920     C***  $VA           DIV       1000          $VA                            *RICAVI DIVISO 1000
284400010920     C***  $VA13C        DIV       1000          $VA13C                         *13^COL DIVISO 1000
284500010920E   1C***                ENDIF
284600000000     C*
284700010920     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
284800950206IF  1C     VRR(JJ)       IFEQ      'V'                                          V O L U M E
284900000000     C                   EXSR      STACAV                                       *RIGA VOLUMI
285000000000X   1C                   ELSE
285100010921IF  2C     VRR(JJ)       IFEQ      'R'                                          R I C A V I
285200010920     C                   EXSR      STACAR                                       *RIGA VOLUMI
285300010920X   2C                   ELSE
285400000000     C                   EXSR      STACAN                                       *RIGA ALTRI VALORI
285500010920E   2C                   ENDIF
285600000000E   1C                   ENDIF
285700000000     C*
285800000000     C                   ENDSR
285900000000     C*------------------------------------------------------------------------*
286000000000     C*?STADEL - IMPOSTA STAMPA STATISTICA "DELTA PER CLIENTE"
286100000000     C*          INDICI: J(4,0) - I(4,0)
286200000000     C*------------------------------------------------------------------------*
286300000000     C     STADEL        BEGSR
286400000000     C*--------------------
286500000000     C* PARAMETRI
286600000000     C*--------------------
286700000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
286800000000     C                   EXSR      STAPAR
286900000000E   1C                   ENDIF
287000000000     C*--------------------
287100000000     C* DATI GENERALI CLIENTE
287200000000     C*--------------------
287300000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
287400000000     C                   EXSR      STADGC
287500000000E   1C                   ENDIF
287600000000     C*--------------------
287700000000     C* INTESTAZIONE
287800000000     C*--------------------
287900000000     C                   MOVEL     DRI(4)        PICSTA
288000071210      *
288100071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
288200071211     c                   eval      Righe_da_stampare = 5
288300071211     c                   exsr      solo_x_STT
288400010920     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
288500000000     C*
288600000000     C* PULISCO I TOTALI A FONDO PAGINA
288700000000     C                   CLEAR                   DRW                            *RICAVI REALI
288800000000     C                   CLEAR                   DPW                            *RICAVI PRESUNTI
288900000000     C                   CLEAR                   DSW                            *SPEDIZIONI
289000000000     C*
289100000000     C* PER OGNI FASCIA DI PESO SELEZIONATA DA STAMPARE
289200000000     C                   Z-ADD     1             J
289300000000DO  1C     1             DO        SPARF         J
289400000000     C     J             OCCUR     CDEL
289500000000     C*
289600000000     C* CALCOLO I TOTALI A FONDO PAGINA
289700000000     C                   ADD       DRC           DRW                            *RICAVI REALI
289800000000     C                   ADD       DPC           DPW                            *RICAVI PRESUNTI
289900000000     C                   ADD       DSC           DSW                            *SPEDIZIONI
290000000000     C*
290100000000     C* DECODIFICO LA FASCIA SELEZIONATA
290200000000     C                   Z-ADD     1             I
290300000000     C                   SETOFF                                           28
290400000000     C     SFPE(J)       LOOKUP    FPE(I)                                 28
290500000000IF  2C     *IN28         IFEQ      *ON
290600000000     C                   MOVEL     DFPE(I)       DPRIMD           14
290700000000X   2C                   ELSE
290800000000     C                   MOVEL     *ALL'*'       DPRIMD
290900000000E   2C                   ENDIF
291000000000     C*--------------------
291100010921     C* STAMPA  R I C A V I
291200000000     C*--------------------
291300000000     C                   MOVEL     *BLANKS       DSECOD            8
291400000000     C                   MOVEL     DCA(7)        DSECOD
291500000000     C*
291600000000     C                   MOVEL     *BLANKS       VALSER           23
291700000000     C     DPRIMD        CAT       DSECOD        VALSER
291800000000     C                   MOVEL     'RIC'         TIPVAL                         *VALORE RICAVI
291900000000     C                   Z-ADD     DRC           $VA
292000000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
292100000000     C                   EXSR      STADEX
292200000000     C*--------------------
292300000000     C* STAMPA  S P E D I Z I O N I
292400000000     C*--------------------
292500000000     C                   MOVEL     *BLANKS       DPRIMD                         *DESCR FASCIA 1 VOLT
292600000000     C                   MOVEL     *BLANKS       DSECOD            8
292700000000     C                   MOVEL     DCA(8)        DSECOD
292800000000     C*
292900000000     C                   MOVEL     *BLANKS       VALSER           23
293000000000     C     DPRIMD        CAT       DSECOD        VALSER
293100000000     C                   MOVEL     'SPE'         TIPVAL                         *VALORE SPEDIZIONI
293200000000     C                   Z-ADD     DSC           $VA
293300000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
293400000000     C                   EXSR      STADEX
293500000000     C*--------------------
293600000000     C* STAMPA  D E L T A
293700000000     C*--------------------
293800000000     C* PER OGNI MESE CALCOLO IL DELTA
293900000000     C                   Z-ADD     1             I
294000000000DO  2C     1             DO        SPARM         I
294100000000     C                   Z-ADD     DRC(I)        WRRW             16 3          *RICAVO REALE
294200000000     C                   Z-ADD     DPC(I)        WRPW             16 3          *RICAVO PRESUNTO
294300000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
294400000000     C                   EXSR      CALDEL
294500000000     C                   Z-ADD     WDPW          DDW(I)                         *SCHIERA DELTA
294600000000E   2C                   ENDDO
294700000000     C*
294800000000     C                   MOVEL     *BLANKS       DSECOD            8
294900000000     C                   MOVEL     DCA(9)        DSECOD
295000000000     C*
295100000000     C                   MOVEL     *BLANKS       VALSER           23
295200000000     C     DPRIMD        CAT       DSECOD        VALSER
295300000000     C                   MOVEL     'DEL'         TIPVAL                         *VALORE DELTA
295400000000     C*
295500000000     C                   Z-ADD     DDW           $VA
295600000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
295700000000     C                   XFOOT     DRC           $RR              16 3          *TOT RICAVI REALI
295800000000     C                   XFOOT     DPC           $RP              16 3          *TOT RICAVI PRESUNTI
295900000000     C                   EXSR      STADEX
296000000000     C*
296100071211     c                   eval      Righe_da_stampare = 1
296200071211     c                   exsr      solo_x_STT
296300010920     C                   WRITE     SE62RI                                       *RIGA DI SEPARAZIONE
296400071210      *
296500020823     C                   EXSR      STATES
296600000000E   1C                   ENDDO
296700000000     C*--------------------
296800000000     C* STAMPA  T O T A L E
296900000000     C*--------------------
297000000000     C* R I C A V I
297100000000     C                   MOVEL     *BLANKS       DPRIMD           14
297200000000     C                   MOVEL     DCA(7)        DPRIMD
297300000000     C                   MOVEL     DCA(10)       DSECOD
297400000000     C*
297500000000     C                   MOVEL     *BLANKS       VALSER           23
297600000000     C     DPRIMD        CAT       DSECOD        VALSER
297700000000     C                   MOVEL     'RIC'         TIPVAL                         *VALORE RICAVI
297800000000     C                   Z-ADD     DRW           $VA
297900000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
298000000000     C                   EXSR      STADEX
298100000000     C* S P E D I Z I O N I
298200000000     C                   MOVEL     *BLANKS       DPRIMD           14
298300000000     C                   MOVEL     DCA(1)        DPRIMD
298400000000     C                   MOVEL     DCA(10)       DSECOD            8
298500000000     C*
298600000000     C                   MOVEL     *BLANKS       VALSER           23
298700000000     C     DPRIMD        CAT       DSECOD        VALSER
298800000000     C                   MOVEL     'SPE'         TIPVAL                         *VALORE SPEDIZIONI
298900000000     C                   Z-ADD     DSW           $VA
299000000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
299100000000     C                   EXSR      STADEX
299200000000     C* D E L T A
299300000000     C*
299400000000     C* PER OGNI MESE CALCOLO IL DELTA
299500000000     C                   Z-ADD     1             I
299600000000DO  1C     1             DO        SPARM         I
299700000000     C                   Z-ADD     DRW(I)        WRRW             16 3          *RICAVO REALE
299800000000     C                   Z-ADD     DPW(I)        WRPW             16 3          *RICAVO PRESUNTO
299900000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
300000000000     C                   EXSR      CALDEL
300100000000     C                   Z-ADD     WDPW          DDW(I)                         *SCHIERA DELTA
300200000000E   1C                   ENDDO
300300000000     C*
300400000000     C                   MOVEL     *BLANKS       DPRIMD           14
300500000000     C                   MOVEL     DCA(9)        DPRIMD
300600000000     C                   MOVEL     DCA(6)        DSECOD            8
300700000000     C*
300800000000     C                   MOVEL     *BLANKS       VALSER           23
300900000000     C     DPRIMD        CAT       DSECOD        VALSER
301000000000     C                   MOVEL     'DEL'         TIPVAL                         *VALORE DELTA
301100000000     C*
301200000000     C                   Z-ADD     DDW           $VA
301300000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
301400000000     C                   XFOOT     DRW           $RR              16 3          *TOT RICAVI REALI
301500000000     C                   XFOOT     DPW           $RP              16 3          *TOT RICAVI PRESUNTI
301600000000     C                   EXSR      STADEX
301700000000     C*
301800071211     c                   eval      Righe_da_stampare = 1
301900071211     c                   exsr      solo_x_STT
302000010920     C                   WRITE     SE62RI                                       *RIGA FINE PROSPETTO
302100071210      *
302200020823     C                   EXSR      STATES
302300000000     C*
302400000000     C                   ENDSR
302500000000     C*------------------------------------------------------------------------*
302600000000     C*?STADEX - IMPOSTA STAMPA RIGA XXX STATISTICA DELTA PER CLIENTE
302700000000     C*          INPUT --> $VA  : SCHIERA VALORI DI 12 MESI
302800000000     C*                    $DE  : DESCRIZIONE RIGA DA STAMPARE
302900000000     C*                    $RR  : TOTALE RICAVI REALI
303000000000     C*                    $RP  : TOTALE RICAVI PRESUNTI
303100000000     C*------------------------------------------------------------------------*
303200000000     C     STADEX        BEGSR
303300000000     C*--------------------
303400000000     C* 13^ C O L O N N A
303500000000     C*--------------------
303600000000     C* PER LA RIGA DEL DELTA NON POSSO FARE LA XFOOT
303700000000IF  1C     TIPVAL        IFNE      'DEL'                                        *N O   D E L T A
303800000000     C                   XFOOT     $VA           $VA13C           16 3
303900000000X   1C                   ELSE                                                   *S I   D E L T A
304000000000     C                   Z-ADD     $RR           WRRW             16 3          *RICAVO REALE
304100000000     C                   Z-ADD     $RP           WRPW             16 3          *RICAVO PRESUNTO
304200000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
304300000000     C                   EXSR      CALDEL
304400000000     C                   Z-ADD     WDPW          $VA13C                         *DELTA 13^ COLONNA
304500000000E   1C                   ENDIF
304600000000     C*
304700000000     C* SE RICAVI LI COMPATTA
304800010920IF  1C***  TIPVAL        IFEQ      'RIC'
304900010920     C***  $VA           DIV       1000          $VA
305000010920     C***  $VA13C        DIV       1000          $VA13C
305100010920E   1C***                ENDIF
305200000000     C*
305300000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
305400010921IF  1C     TIPVAL        IFEQ      'DEL'                                        *D E L T A
305500010921     C                   EXSR      STACAP
305600010921X   1C                   ELSE
305700010921IF  2C     TIPVAL        IFEQ      'RIC'                                        *R I C A V I
305800010920     C                   EXSR      STACAR
305900010920X   2C                   ELSE
306000010921     C                   EXSR      STACAN                                       *ALTRI VALORI
306100010920E   2C                   ENDIF
306200000000E   1C                   ENDIF
306300000000     C*
306400000000     C                   ENDSR
306500000000     C*------------------------------------------------------------------------*
306600000000     C*?STAAFF - IMPOSTA STAMPA STATISTICA "AFFIDABILITA'"
306700000000     C*          INDICI: J(4,0) --> ORA CONSEGNA (1÷7)
306800000000     C*------------------------------------------------------------------------*
306900000000     C     STAAFF        BEGSR
307000000000     C*--------------------
307100000000     C* PARAMETRI
307200000000     C*--------------------
307300000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
307400000000     C                   EXSR      STAPAR
307500000000E   1C                   ENDIF
307600000000     C*--------------------
307700000000     C* DATI GENERALI CLIENTE
307800000000     C*--------------------
307900000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
308000000000     C                   EXSR      STADGC
308100000000E   1C                   ENDIF
308200000000     C*--------------------
308300000000     C* INTESTAZIONE
308400000000     C*--------------------
308500000000     C                   MOVEL     DRI(3)        PICSTA
308600071210      *
308700071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
308800071211     c                   eval      Righe_da_stampare = 6
308900071211     c                   exsr      solo_x_STT
309000010920     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
309100010920     C                   WRITE     SE62RS                                       *ULTERIORE DESCR SER
309200071210      *
309300020823     C                   EXSR      STATES
309400071210      *
309500071211     c                   eval      Righe_da_stampare = 1
309600071211     c                   exsr      solo_x_STT
309700010920     C                   WRITE     SE62RV                                       *RIGA VUOTA
309800071210      *
309900020823     C                   EXSR      STATES
310000000000     C*
310100000000     C* AZZERO I TOTALI
310200000000     C                   Z-ADD     *ZEROS        PNW
310300000629     C                   Z-ADD     *ZEROS        PDW
310400000000     C                   Z-ADD     *ZEROS        PSW
310500000629     C                   Z-ADD     *ZEROS        PPW
310600000000     C                   Z-ADD     *ZEROS        PTW
310700000000     C                   Z-ADD     *ZEROS        PNW13C           13 0
310800000629     C                   Z-ADD     *ZEROS        PDW13C           13 0
310900000000     C                   Z-ADD     *ZEROS        PSW13C           13 0
311000000629     C                   Z-ADD     *ZEROS        PPW13C           13 0
311100000000     C                   Z-ADD     *ZEROS        PTW13C           13 0
311200000000     C*
311300000000     C* CALCOLO I TOALI MESE PER MESE DEI N/R/E E TOTALI
311400000000     C                   Z-ADD     1             J
311500000000DO  1C     1             DO        PARO          J
311600000000     C     J             OCCUR     CAFF
311700000000     C                   ADD       CNC           PNW                            *TOTALI CORRIERE
311800000629     C                   ADD       CDC           PDW                            *TOTALI DISTRIBUZION
311900000000     C                   ADD       CEC           PSW                            *TOTALI ESPRESSI
312000091214     C                   ADD       CHC           PSW                            *TOTALI ESPRESSI h10
312100000629     C                   ADD       CPC           PPW                            *TOTALI POSTE
312200000000     C     PTW           ADD       CNC           PTW
312300000629     C     PTW           ADD       CDC           PTW
312400000629     C     PTW           ADD       CEC           PTW
312500091214     C     PTW           ADD       CHC           PTW
312600000629     C     PTW           ADD       CPC           PTW                            *TOTALI C + D + E +
312700000000E   1C                   ENDDO
312800000000     C*
312900000000     C* TREDICESIMI TOTALI
313000000000     C                   XFOOT     PNW           PNW13C                         *13° TOT CORRIERE
313100000629     C                   XFOOT     PDW           PDW13C                         *13° TOT DISTRIBUZ.N
313200000000     C                   XFOOT     PSW           PSW13C                         *13° TOT ESPRESSI
313300000629     C                   XFOOT     PPW           PPW13C                         *13° TOT POSTE
313400000000     C                   XFOOT     PTW           PTW13C                         *13° TOT TOTALI
313500000000     C*
313600000000     C* PER OGNI ORA CONSEGNA DA STAMPARE
313700000000     C                   Z-ADD     1             J
313800000000DO  1C     1             DO        PARO          J
313900000000     C     J             OCCUR     CAFF
314000000000     C*--------------------
314100000000     C* STAMPA RIGA ORA DI CONSEGNA
314200000000     C*--------------------
314300000000     C                   MOVEL     *BLANKS       VALSER           23
314400000000     C                   MOVEL     DORE(J)       VALSER                         *ORA DI COMSEGNA
314500000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
314600000000     C                   EXSR      STAAFX
314700000000     C*
314800000000E   1C                   ENDDO
314900000000     C*
315000071211     c                   eval      Righe_da_stampare = 1
315100071211     c                   exsr      solo_x_STT
315200010920     C                   WRITE     SE62RI                                       *RIGA FINE PROSPETTO
315300071210      *
315400020823     C                   EXSR      STATES
315500000000     C*
315600000000     C                   ENDSR
315700000000     C*------------------------------------------------------------------------*
315800000000     C*?STAAFX - IMPOSTA STAMPA RIGA XXX STATISTICA AFFIDABILITA' SPEDIZIONI
315900000000     C*------------------------------------------------------------------------*
316000000000     C     STAAFX        BEGSR
316100000000     C*
316200000000     C* CALCOLO LE INCIDENZE PER MESE DEI SINGOLI TIPI SERVIZI SUL TOTALE DELLE
316300000000     C* SPEDIZIONI DELL'ORA DI CONSEGNA IN LINEA
316400000000     C                   Z-ADD     1             E                 4 0
316500000000DO  1C     1             DO        SPARM         E
316600000000     C*--------------------
316700000000     C* INCIDENZA CORRIERE
316800000000     C*--------------------
316900000000     C                   Z-ADD     CNC(E)        WVAW             16 3          *TOT SERVIZIO ORA
317000000000     C                   Z-ADD     PNW(E)        WTXW             16 3          *TOT SERVIZIO ORE
317100000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
317200000000     C                   EXSR      CALINC
317300000000     C                   Z-ADD     WINW          $P1(E)                         *INCIDENZA CORRIERE
317400000000     C*--------------------
317500000000     C* INCIDENZA DISTRIBUZIONE
317600000000     C*--------------------
317700000629     C                   Z-ADD     CDC(E)        WVAW             16 3          *TOT SERVIZIO ORA
317800000629     C                   Z-ADD     PDW(E)        WTXW             16 3          *TOT SERVIZIO ORE
317900000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
318000000000     C                   EXSR      CALINC
318100000000     C                   Z-ADD     WINW          $P2(E)                         *INCIDENZA DISTRIBUZ
318200000000     C*--------------------
318300000000     C* INCIDENZA ESPRESSI
318400000000     C*--------------------
318500000000     C                   Z-ADD     CEC(E)        WVAW             16 3          *TOT SERVIZIO ORA
318600091214     C                   add       ChC(E)        WVAW             16 3          *TOT SERVIZIO ORA h1
318700000000     C                   Z-ADD     PSW(E)        WTXW             16 3          *TOT SERVIZIO ORE
318800000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
318900000000     C                   EXSR      CALINC
319000000000     C                   Z-ADD     WINW          $P3(E)                         *INCIDENZA ESPRESSI
319100000629     C*--------------------
319200000629     C* INCIDENZA POSTE
319300000629     C*--------------------
319400000629     C                   Z-ADD     CPC(E)        WVAW             16 3          *TOT SERVIZIO ORA
319500000629     C                   Z-ADD     PPW(E)        WTXW             16 3          *TOT SERVIZIO ORE
319600000629     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
319700000629     C                   EXSR      CALINC
319800000629     C                   Z-ADD     WINW          $P4(E)                         *INCIDENZA ESPRESSI
319900000000     C*--------------------
320000000000     C* INCIDENZA TOTALE
320100000000     C*--------------------
320200000000     C                   Z-ADD     CNC(E)        WVAW             16 3          *TOTALE SERVIZI ORA
320300000629     C                   ADD       CDC(E)        WVAW
320400000000     C                   ADD       CEC(E)        WVAW
320500091214     C                   ADD       ChC(E)        WVAW
320600000629     C                   ADD       CPC(E)        WVAW
320700000000     C                   Z-ADD     PTW(E)        WTXW             16 3          *TOTALE SERVIZI ORE
320800000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
320900000000     C                   EXSR      CALINC
321000000629     C                   Z-ADD     WINW          $P5(E)                         *INCIDENZA ESPRESSI
321100000000E   1C                   ENDDO
321200000000     C*--------------------
321300000000     C* 13^ COLONNA
321400000000     C*--------------------
321500000000     C***
321600000000     C* INCIDENZA CORRIERE
321700000000     C***
321800000000     C                   XFOOT     CNC           WVAW             16 3          *TOT SERVIZIO ORA
321900000000     C                   Z-ADD     PNW13C        WTXW             16 3          *TOT SERVIZIO ORE
322000000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
322100000000     C                   EXSR      CALINC
322200000000     C                   Z-ADD     WINW          $P113C            2 0          *13^INCIDENZA CORRIE
322300000000     C***
322400000000     C* INCIDENZA DISTRIBUZIONE
322500000000     C***
322600000629     C                   XFOOT     CDC           WVAW             16 3          *TOT SERVIZIO ORA
322700000629     C                   Z-ADD     PDW13C        WTXW             16 3          *TOT SERVIZIO ORE
322800000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
322900000000     C                   EXSR      CALINC
323000000000     C                   Z-ADD     WINW          $P213C            2 0          *13^INCIDENZA DISTRI
323100000000     C***
323200000000     C* INCIDENZA ESPRESSI
323300000000     C***
323400000000     C                   XFOOT     CEC           WVAW             16 3          *TOT SERVIZIO ORA
323500091214     C                   XFOOT     ChC           WVAWh            16 3          *TOT SERVIZIO ORA h1
323600091214     C                   add       WVAWh         WVAW             16 3          *TOT SERVIZIO ORA
323700000000     C                   Z-ADD     PSW13C        WTXW             16 3          *TOT SERVIZIO ORE
323800000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
323900000000     C                   EXSR      CALINC
324000000000     C                   Z-ADD     WINW          $P313C            2 0          *13^ INCID ESPRESSI
324100000629     C***
324200000629     C* INCIDENZA POSTE
324300000629     C***
324400000629     C                   XFOOT     CPC           WVAW             16 3          *TOT SERVIZIO ORA
324500000629     C                   Z-ADD     PPW13C        WTXW             16 3          *TOT SERVIZIO ORE
324600000629     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
324700000629     C                   EXSR      CALINC
324800000629     C                   Z-ADD     WINW          $P413C            2 0          *13^ INCID POSTE
324900000000     C***
325000000000     C* INCIDENZA TOTALE
325100000000     C***
325200000000     C                   XFOOT     CNC           WVNW             13 0          *TOTALE CORRIERE ORA
325300000629     C                   XFOOT     CDC           WVRW             13 0          *TOTALE DISTRIBUZ OR
325400000000     C                   XFOOT     CEC           WVEW             13 0          *TOTALE ESPRESSI ORA
325500091214     C                   XFOOT     ChC           WVEWh            13 0          *TOTALE ESPRESSI ORA
325600091214     C                   add       WVEWh         WVEW             13 0          *TOTALE ESPRESSI ORA
325700000629     C                   XFOOT     CPC           WVPW             13 0          *TOTALE POSTE ORA
325800000000     C                   Z-ADD     WVNW          WVAW
325900000000     C     WVAW          ADD       WVRW          WVAW
326000000629     C     WVAW          ADD       WVEW          WVAW
326100000629     C     WVAW          ADD       WVPW          WVAW                           *TOTALE SERVIZI ORA
326200000000     C                   Z-ADD     PTW13C        WTXW             16 3          *TOTALE SERVIZI ORE
326300000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
326400000000     C                   EXSR      CALINC
326500000629     C                   Z-ADD     WINW          $P513C            2 0          *13^INCIDENZA TOTALE
326600000000     C*
326700000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
326800000000     C                   EXSR      STACA2                                       *RIGA PERCENTUALE 2
326900000000     C*
327000000000     C                   ENDSR
327100000000     C*------------------------------------------------------------------------*
327200000000     C*?STAAMM - IMPOSTA STAMPA STATISTICA "AMMINISTRAZIONE"
327300000000     C*          INDICI: J(4,0)
327400000000     C*------------------------------------------------------------------------*
327500000000     C     STAAMM        BEGSR
327600000000     C*--------------------
327700000000     C* PARAMETRI
327800000000     C*--------------------
327900000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
328000000000     C                   EXSR      STAPAR
328100000000E   1C                   ENDIF
328200000000     C*--------------------
328300000000     C* DATI GENERALI CLIENTE
328400000000     C*--------------------
328500000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
328600000000     C                   EXSR      STADGC
328700000000E   1C                   ENDIF
328800000000     C*--------------------
328900000000     C* INTESTAZIONE
329000000000     C*--------------------
329100000000     C                   MOVEL     DRI(5)        PICSTA
329200071210      *
329300071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
329400071211     c                   eval      Righe_da_stampare = 5
329500071211     c                   exsr      solo_x_STT
329600071211     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
329700071210      *
329800000000     C                   MOVEL     '0'           DICUI                          *PER STAMPARE "DI CU
329900000000     C*--------------------
330000000000     C* STAMPA  F A T T U R A T O
330100000000     C*--------------------
330200970724     C     AFC           ADD       ANC           AFC                            *AGGIUNGE NOTE
330300970724     C     AFC           ADD       ARC           AFC                            *AGGIUNGE RETTIFICHE
330400000000     C                   MOVEL     DME(10)       $DE
330500000000     C                   MOVEL     'RIC'         TIPVAL                         FATTURATO COME RICAV
330600000000     C                   Z-ADD     AFC           $VA
330700000000     C                   EXSR      STAAMX
330800000000     C*--------------------
330900000000     C* STAMPA  N O T E   A C C R E D I T O
331000000000     C*--------------------
331100000000     C                   XFOOT     ANC           COM$VA
331200000000IF  1C     COM$VA        IFNE      *ZEROS                                       *SE MEMORIZZATO QC.
331300000000IF  2C     DICUI         IFEQ      '0'
331400000000     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
331500000000E   2C                   ENDIF
331600000000     C*
331700000000     C                   MOVEL     DME(11)       $DE
331800000000     C                   MOVEL     'RIC'         TIPVAL                         *NOTE COME RICAVI
331900000000     C                   Z-ADD     ANC           $VA
332000000000     C                   EXSR      STAAMX
332100000000     C*--------------------
332200000000     C* STAMPA  P E R C E N T U A L E   N O T E
332300000000     C*--------------------
332400000000     C* PER OGNI MESE CALCOLO LA PERCENTUALE
332500000000     C                   Z-ADD     1             J
332600000000DO  2C     1             DO        SPARM         J
332700000000     C                   Z-ADD     AFC(J)        WFAW             16 3          *FATTURATO
332800000000     C                   Z-ADD     ANC(J)        WNRW             16 3          *NOTE O RETTIFICHE
332900000629     C                   Z-ADD     *ZEROS        WPDW              4 1          *PERC SUL FATTURATO
333000000000     C*
333100000000     C                   EXSR      CALPER
333200000629     C                   Z-ADD     WPDW          APW(J)                         *SCHIERA PERCENTUALI
333300000000E   2C                   ENDDO
333400000000     C*
333500000000     C                   MOVEL     DME(13)       $DE
333600000000     C                   MOVEL     'PER'         TIPVAL                         *PERCENTUALI
333700000000     C                   Z-ADD     APW           $VA
333800000000     C*
333900000000     C* CALCOLO I TOTALI FATTURATO E NOTE PER 13^ COLONNA
334000000000     C                   XFOOT     AFC           $FA              16 3
334100000000     C                   XFOOT     ANC           $NR              16 3
334200000000     C                   EXSR      STAAMX
334300000000     C*
334400071211     c                   eval      Righe_da_stampare = 1
334500071211     c                   exsr      solo_x_STT
334600010920     C                   WRITE     SE62RV                                       *RIGA VUOTA
334700071210      *
334800020823     C                   EXSR      STATES
334900000000E   1C                   ENDIF
335000000000     C*--------------------
335100000000     C* STAMPA  R E T T I F I C H E
335200000000     C*--------------------
335300000000     C                   XFOOT     ARC           COM$VA
335400000000IF  1C     COM$VA        IFNE      *ZEROS                                       *SE MEMORIZZATO QC.
335500000000IF  2C     DICUI         IFEQ      '0'
335600000000     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
335700000000E   2C                   ENDIF
335800000000     C*
335900000000     C                   MOVEL     DME(12)       $DE
336000000000     C                   MOVEL     'RIC'         TIPVAL                         *NOTE COME RICAVI
336100000000     C                   Z-ADD     ARC           $VA
336200000000     C                   EXSR      STAAMX
336300071210      *
336400071211     c                   eval      Righe_da_stampare = 1
336500071211     c                   exsr      solo_x_STT
336600010920     C                   WRITE     SE62RV
336700071210      *
336800020823     C                   EXSR      STATES
336900000000E   1C                   ENDIF
337000070628      *                                                    ?
337100070628      *  Ciclo per visualizzazione dell Varie del cliente  ?
337200070628     c                   do        sav_CA        CA
337300070628      **?  **  aggancia la chiave delle schiere tab.varie "CA"
337400070628     c                   move      SkOrd(CA)     CAkey             3 0
337500070628      **
337600070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
337700070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
337800070628     C                   Z-ADD     1             P
337900070628     C                   SETOFF                                           28
338000070628     C     SkVar(CAkey)  LOOKUP    VAR(P)                                 28
338100070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
338200070628     C     P             OCCUR     CAMM
338300070628     C                   ADD       ARI           $VA                            *RICAVI "I"
338400070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "I"
338500070628E   1C                   ENDIF
338600070628     C*
338700070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
338800070628     C                   XFOOT     $VA           COM$VA           16 3
338900070628     C                   XFOOT     $V2           COM$V2           16 3
339000070628IF  1C     COM$VA        IFNE      *ZEROS
339100070628     C     COM$V2        ORNE      *ZEROS
339200070628IF  2C     DICUI         IFEQ      '0'
339300070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
339400070628E   2C                   ENDIF
339500070628     C*--------------------
339600070628     C* STAMPA  R I C A V I  G I A C E N Z E
339700070628     C*--------------------
339800070628     C                   MOVEL     SkDsc(CAkey)  $DE
339900070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
340000070628     C                   EXSR      STAAMX
340100070628     C*--------------------
340200070628     C* STAMPA  N U M E R O  G I A C E N Z E
340300070628     C*--------------------
340400070628     C                   Z-ADD     $V2           $VA
340500070628     C                   MOVEL     DME(15)       $DE
340600070628      *?    Eccezione...(#) non è un valore di ricavo ma Numerico .  ?
340700070628     c                   if        SkVar(CAkey) = '#'
340800070628     C                   move      ' NR.'        $DE                            *DESCRIZIONE VALORE
340900070628     c                   end
341000070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
341100070628     C                   EXSR      STAAMX
341200071210      *
341300071211     c                   eval      Righe_da_stampare = 1
341400071211     c                   exsr      solo_x_STT
341500070628     C                   WRITE     SE62RV
341600071210      *
341700070628     C                   EXSR      STATES
341800070628E   1C                   ENDIF
341900070628      **?  **  ?
342000070628E   0C                   EndDO
342100070628      **?  *------------------------------------*  ?
342200070628      **?  * per provare  al momento non cancello le specifiche ma le salto *  ?
342300070628      **?  *------------------------------------*  ?
342400070628      **?  **  ?
342500070628     c                   GOTO      Salta_TUTTO
342600070628      *                                                    ?
342700070628     C*
342800070628      **?  * Le vecchie specifiche sono state parcheggiate nella Routine OLD *  ?
342900070628      **?  * a PRO-MEMORIA per verificare che tutto funzioni correttamente.  *  ?
343000070628      **?  * In un prossimo futuro POTRANNO e DOVRANNO essere cancellate !!  *  ?
343100070628     c                   Exsr      Specifiche_OLD
343200070628      **?  **  ?
343300070628      **?  *-----------------------*  ?
343400070628     c     Salta_TUTTO   tag
343500070628      **?  *-----------------------*  ?
343600070628      **?  **  ?
343700070628     C*
343800010921     C*
343900071211     c                   eval      Righe_da_stampare = 1
344000071211     c                   exsr      solo_x_STT
344100010920     C                   WRITE     SE62RI                                       *RIGA FINE PROSPETTO
344200071210      *
344300020823     C                   EXSR      STATES
344400000000     C*
344500000000     C                   ENDSR
344600070628     C*------------------------------------------------------------------------*
344700070628      *?  VECCHIE SPECIFICHE SOSTITUITE CON Tabella "CA" correlata alla "CC"...  ?
344800070628     C*------------------------------------------------------------------------*
344900070628     C     Specifiche_OLDBEGSR
345000070628     C*
345100070628     C*--------------------
345200070628     C* CALCOLO GIACENZE -->  GIACENZE = (VARIA "I")
345300070628     C*--------------------
345400070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
345500070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
345600070628     C                   Z-ADD     1             P
345700070628     C                   SETOFF                                           28
345800070628     C     'I'           LOOKUP    VAR(P)                                 28
345900070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
346000070628     C     P             OCCUR     CAMM
346100070628     C                   ADD       ARI           $VA                            *RICAVI "I"
346200070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "I"
346300070628E   1C                   ENDIF
346400070628     C*
346500070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
346600070628     C                   XFOOT     $VA           COM$VA           16 3
346700070628     C                   XFOOT     $V2           COM$V2           16 3
346800070628IF  1C     COM$VA        IFNE      *ZEROS
346900070628     C     COM$V2        ORNE      *ZEROS
347000070628IF  2C     DICUI         IFEQ      '0'
347100070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
347200070628E   2C                   ENDIF
347300070628     C*--------------------
347400070628     C* STAMPA  R I C A V I  G I A C E N Z E
347500070628     C*--------------------
347600070628     C                   MOVEL     DME(14)       $DE
347700070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
347800070628     C                   EXSR      STAAMX
347900070628     C*--------------------
348000070628     C* STAMPA  N U M E R O  G I A C E N Z E
348100070628     C*--------------------
348200070628     C                   Z-ADD     $V2           $VA
348300070628     C                   MOVEL     DME(15)       $DE
348400070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
348500070628     C                   EXSR      STAAMX
348600071210      *
348700071211     c                   eval      Righe_da_stampare = 1
348800071211     c                   exsr      solo_x_STT
348900070628     C                   WRITE     SE62RV
349000071210      *
349100070628     C                   EXSR      STATES
349200070628E   1C                   ENDIF
349300070628     C*--------------------
349400070628     C* CALCOLO ASSICURAZIONI -->  ASSICURAZIONI = (VARIA "E" + VARIA "R")
349500070628     C*--------------------
349600070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
349700070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
349800070628     C                   Z-ADD     1             P
349900070628     C                   SETOFF                                           28
350000070628     C     'E'           LOOKUP    VAR(P)                                 28
350100070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
350200070628     C     P             OCCUR     CAMM
350300070628     C                   ADD       ARI           $VA                            *RICAVI "E"
350400070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "E"
350500070628E   1C                   ENDIF
350600070628     C*
350700070628     C                   Z-ADD     1             P
350800070628     C                   SETOFF                                           28
350900070628     C     'R'           LOOKUP    VAR(P)                                 28
351000070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "R
351100070628     C     P             OCCUR     CAMM
351200070628     C     $VA           ADD       ARI           $VA                            *RICAVI "R"
351300070628     C     $V2           ADD       ASP           $V2                            *SPEDIZIONI "R"
351400070628E   1C                   ENDIF
351500070628     C*
351600070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
351700070628     C                   XFOOT     $VA           COM$VA           16 3
351800070628     C                   XFOOT     $V2           COM$V2           16 3
351900070628IF  1C     COM$VA        IFNE      *ZEROS
352000070628     C     COM$V2        ORNE      *ZEROS
352100070628IF  2C     DICUI         IFEQ      '0'
352200070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
352300070628E   2C                   ENDIF
352400070628     C*--------------------
352500070628     C* STAMPA  R I C A V I   A S S I C U R A Z I O N I
352600070628     C*--------------------
352700070628     C                   MOVEL     DME(21)       $DE
352800070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
352900070628     C                   EXSR      STAAMX
353000070628     C*--------------------
353100070628     C* STAMPA  N U M E R O   A S S I C U R A Z I O N I
353200070628     C*--------------------
353300070628     C                   Z-ADD     $V2           $VA
353400070628     C                   MOVEL     DME(15)       $DE
353500070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
353600070628     C                   EXSR      STAAMX
353700071210      *
353800071211     c                   eval      Righe_da_stampare = 1
353900071211     c                   exsr      solo_x_STT
354000070628     C                   WRITE     SE62RV
354100071210      *
354200070628     C                   EXSR      STATES
354300070628E   1C                   ENDIF
354400070628     C*--------------------
354500070628     C* CONTRASSEGNI --> CONTRASSEGNI = VARIA "G"
354600070628     C*--------------------
354700070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
354800070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
354900070628     C                   Z-ADD     1             P
355000070628     C                   SETOFF                                           28
355100070628     C     'G'           LOOKUP    VAR(P)                                 28
355200070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "G
355300070628     C     P             OCCUR     CAMM
355400070628     C                   ADD       ARI           $VA                            *RICAVI "G"
355500070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "G"
355600070628E   1C                   ENDIF
355700070628     C*
355800070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
355900070628     C                   XFOOT     $VA           COM$VA           16 3
356000070628     C                   XFOOT     $V2           COM$V2           16 3
356100070628IF  1C     COM$VA        IFNE      *ZEROS
356200070628     C     COM$V2        ORNE      *ZEROS
356300070628IF  2C     DICUI         IFEQ      '0'
356400070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
356500070628E   2C                   ENDIF
356600070628     C*--------------------
356700070628     C* STAMPA  R I C A V I   C O N T R A S S E G N O
356800070628     C*--------------------
356900070628     C                   MOVEL     DME(22)       $DE
357000070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
357100070628     C                   EXSR      STAAMX
357200070628     C*--------------------
357300070628     C* STAMPA  N U M E R O   C O N T R A S S E G N O
357400070628     C*--------------------
357500070628     C                   Z-ADD     $V2           $VA
357600070628     C                   MOVEL     DME(15)       $DE
357700070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
357800070628     C                   EXSR      STAAMX
357900071210      *
358000071211     c                   eval      Righe_da_stampare = 1
358100071211     c                   exsr      solo_x_STT
358200070628     C                   WRITE     SE62RV
358300071210      *
358400070628     C                   EXSR      STATES
358500070628E   1C                   ENDIF
358600070628     C*--------------------
358700070628     C* CONSEGNE AI PIANI --> VARIA = "P"
358800070628     C*--------------------
358900070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
359000070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
359100070628     C                   Z-ADD     1             P
359200070628     C                   SETOFF                                           28
359300070628     C     'P'           LOOKUP    VAR(P)                                 28
359400070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "P"
359500070628     C     P             OCCUR     CAMM
359600070628     C                   ADD       ARI           $VA                            *RICAVI "P"
359700070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "P"
359800070628E   1C                   ENDIF
359900070628     C*
360000070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
360100070628     C                   XFOOT     $VA           COM$VA           16 3
360200070628     C                   XFOOT     $V2           COM$V2           16 3
360300070628IF  1C     COM$VA        IFNE      *ZEROS
360400070628     C     COM$V2        ORNE      *ZEROS
360500070628IF  2C     DICUI         IFEQ      '0'
360600070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
360700070628E   2C                   ENDIF
360800070628     C*--------------------
360900070628     C* STAMPA  R I C A V I   C O N S E G N A   A I   P I A N I
361000070628     C*--------------------
361100070628     C                   MOVEL     DME(17)       $DE
361200070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
361300070628     C                   EXSR      STAAMX
361400070628     C*--------------------
361500070628     C* STAMPA  N U M E R O   C O N S E G N A   A I   P I A N I
361600070628     C*--------------------
361700070628     C                   Z-ADD     $V2           $VA
361800070628     C                   MOVEL     DME(15)       $DE
361900070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
362000070628     C                   EXSR      STAAMX
362100071210      *
362200071211     c                   eval      Righe_da_stampare = 1
362300071211     c                   exsr      solo_x_STT
362400070628     C                   WRITE     SE62RV
362500071210      *
362600070628     C                   EXSR      STATES
362700070628E   1C                   ENDIF
362800070628     C*--------------------
362900070628     C* CONSEGNE PER APPUNTAMENTO --> VARIA = "A"
363000070628     C*--------------------
363100070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
363200070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
363300070628     C                   Z-ADD     1             P
363400070628     C                   SETOFF                                           28
363500070628     C     'A'           LOOKUP    VAR(P)                                 28
363600070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "A"
363700070628     C     P             OCCUR     CAMM
363800070628     C                   ADD       ARI           $VA                            *RICAVI "A"
363900070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "A"
364000070628E   1C                   ENDIF
364100070628     C*
364200070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
364300070628     C                   XFOOT     $VA           COM$VA           16 3
364400070628     C                   XFOOT     $V2           COM$V2           16 3
364500070628IF  1C     COM$VA        IFNE      *ZEROS
364600070628     C     COM$V2        ORNE      *ZEROS
364700070628IF  2C     DICUI         IFEQ      '0'
364800070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
364900070628E   2C                   ENDIF
365000070628     C*--------------------
365100070628     C* STAMPA  R I C A V I   C O N S E G N A   P E R   A P P U N T A M E N T O
365200070628     C*--------------------
365300070628     C                   MOVEL     DME(18)       $DE
365400070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
365500070628     C                   EXSR      STAAMX
365600070628     C*--------------------
365700070628     C* STAMPA  N U M E R O   C O N S E G N A   P E R   A P P U N T A M E N T O
365800070628     C*--------------------
365900070628     C                   Z-ADD     $V2           $VA
366000070628     C                   MOVEL     DME(15)       $DE
366100070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
366200070628     C                   EXSR      STAAMX
366300071210      *
366400071211     c                   eval      Righe_da_stampare = 1
366500071211     c                   exsr      solo_x_STT
366600070628     C                   WRITE     SE62RV
366700071210      *
366800070628     C                   EXSR      STATES
366900070628E   1C                   ENDIF
367000070628     C*--------------------
367100070628     C* CONSEGNE AI SUPERMERCATI --> VARIA = "S"
367200070628     C*--------------------
367300070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
367400070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
367500070628     C                   Z-ADD     1             P
367600070628     C                   SETOFF                                           28
367700070628     C     'S'           LOOKUP    VAR(P)                                 28
367800070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "S"
367900070628     C     P             OCCUR     CAMM
368000070628     C                   ADD       ARI           $VA                            *RICAVI "S"
368100070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "S"
368200070628E   1C                   ENDIF
368300070628     C*
368400070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
368500070628     C                   XFOOT     $VA           COM$VA           16 3
368600070628     C                   XFOOT     $V2           COM$V2           16 3
368700070628IF  1C     COM$VA        IFNE      *ZEROS
368800070628     C     COM$V2        ORNE      *ZEROS
368900070628IF  2C     DICUI         IFEQ      '0'
369000070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
369100070628E   2C                   ENDIF
369200070628     C*--------------------
369300070628     C* STAMPA  R I C A V I   C O N S E G N A   A I   S U P E R M E R C A T I
369400070628     C*--------------------
369500070628     C                   MOVEL     DME(19)       $DE
369600070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
369700070628     C                   EXSR      STAAMX
369800070628     C*--------------------
369900070628     C* STAMPA  N U M E R O   C O N S E G N A   A I   S U P E R M E R C A T I
370000070628     C*--------------------
370100070628     C                   Z-ADD     $V2           $VA
370200070628     C                   MOVEL     DME(15)       $DE
370300070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
370400070628     C                   EXSR      STAAMX
370500071210      *
370600071211     c                   eval      Righe_da_stampare = 1
370700071211     c                   exsr      solo_x_STT
370800070628     C                   WRITE     SE62RV
370900071210      *
371000070628     C                   EXSR      STATES
371100070628E   1C                   ENDIF
371200070628     C*--------------------
371300070628     C* CONSEGNE FUORI MISURA --> VARIA = "F"
371400070628     C*--------------------
371500070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
371600070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
371700070628     C                   Z-ADD     1             P
371800070628     C                   SETOFF                                           28
371900070628     C     'F'           LOOKUP    VAR(P)                                 28
372000070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "F"
372100070628     C     P             OCCUR     CAMM
372200070628     C                   ADD       ARI           $VA                            *RICAVI "F"
372300070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "F"
372400070628E   1C                   ENDIF
372500070628     C*
372600070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
372700070628     C                   XFOOT     $VA           COM$VA           16 3
372800070628     C                   XFOOT     $V2           COM$V2           16 3
372900070628IF  1C     COM$VA        IFNE      *ZEROS
373000070628     C     COM$V2        ORNE      *ZEROS
373100070628IF  2C     DICUI         IFEQ      '0'
373200070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
373300070628E   2C                   ENDIF
373400070628     C*--------------------
373500070628     C* STAMPA  R I C A V I   C O N S E G N A   F U O R I   M I S U R A
373600070628     C*--------------------
373700070628     C                   MOVEL     DME(20)       $DE
373800070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
373900070628     C                   EXSR      STAAMX
374000070628     C*--------------------
374100070628     C* STAMPA  N U M E R O   C O N S E G N A   F U O R I   M I S U R A
374200070628     C*--------------------
374300070628     C                   Z-ADD     $V2           $VA
374400070628     C                   MOVEL     DME(15)       $DE
374500070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
374600070628     C                   EXSR      STAAMX
374700071210      *
374800071211     c                   eval      Righe_da_stampare = 1
374900071211     c                   exsr      solo_x_STT
375000070628     C                   WRITE     SE62RV
375100071210      *
375200070628     C                   EXSR      STATES
375300070628E   1C                   ENDIF
375400070628     C*--------------------
375500070628     C* CONSEGNE CENTRO STORICO --> VARIA = "Q"
375600070628     C*--------------------
375700070628     C*                  Z-ADD     *ZEROS        $VA                            *PER I RICAVI
375800070628     C*                  Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
375900070628     C*                  Z-ADD     1             P
376000070628     C*                  SETOFF                                           28
376100070628     C*    'Q'           LOOKUP    VAR(P)                                 28
376200070628IF  1C*    *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "Q"
376300070628     C*    P             OCCUR     CAMM
376400070628     C*                  ADD       ARI           $VA                            *RICAVI "Q"
376500070628     C*                  ADD       ASP           $V2                            *SPEDIZIONI "Q"
376600070628E   1C*                  ENDIF
376700070628     C*
376800070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
376900070628     C*                  XFOOT     $VA           COM$VA           16 3
377000070628     C*                  XFOOT     $V2           COM$V2           16 3
377100070628IF  1C*    COM$VA        IFNE      *ZEROS
377200070628     C*    COM$V2        ORNE      *ZEROS
377300070628IF  2C*    DICUI         IFEQ      '0'
377400070628     C*                  EXSR      STACUI                                       *STAMPA RIGA "DI CUI
377500070628E   2C*                  ENDIF
377600070628     C*--------------------
377700070628     C* STAMPA  R I C A V I   C O N S E G N A   C E N T R O   S T O R I C O
377800070628     C*--------------------
377900070628     C*                  MOVEL     DME(23)       $DE
378000070628     C*                  MOVEL     'RIC'         TIPVAL                         *RICAVI
378100070628     C*                  EXSR      STAAMX
378200070628     C*--------------------
378300070628     C* STAMPA  N U M E R O   C O N S E G N A   C E N T R O   S T O R I C O
378400070628     C*--------------------
378500070628     C*                  Z-ADD     $V2           $VA
378600070628     C*                  MOVEL     DME(15)       $DE
378700070628     C*                  MOVEL     'NUM'         TIPVAL                         *NUMERO
378800070628     C*                  EXSR      STAAMX
378900071210     c*
379000071211     c*                  eval      Righe_da_stampare = 1
379100071211     c*                  exsr      solo_x_STT
379200070628     C*                  WRITE     SE62RV
379300071210     c*
379400070628     C*                  EXSR      STATES
379500070628E   1C*                  ENDIF
379600070628     C*--------------------
379700070628     C* CONSEGNE LOCALITA' DISAGIATE --> VARIA = "K"
379800070628     C*--------------------
379900070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
380000070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
380100070628     C                   Z-ADD     1             P
380200070628     C                   SETOFF                                           28
380300070628     C     'K'           LOOKUP    VAR(P)                                 28
380400070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "K"
380500070628     C     P             OCCUR     CAMM
380600070628     C                   ADD       ARI           $VA                            *RICAVI "K"
380700070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "K"
380800070628E   1C                   ENDIF
380900070628     C*
381000070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
381100070628     C                   XFOOT     $VA           COM$VA           16 3
381200070628     C                   XFOOT     $V2           COM$V2           16 3
381300070628IF  1C     COM$VA        IFNE      *ZEROS
381400070628     C     COM$V2        ORNE      *ZEROS
381500070628IF  2C     DICUI         IFEQ      '0'
381600070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
381700070628E   2C                   ENDIF
381800070628     C*--------------------
381900070628     C* STAMPA  R I C A V I   C O N S E G N A   L O C A L I T A'   D I S A G I A T E
382000070628     C*--------------------
382100070628     C                   MOVEL     DME(24)       $DE
382200070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
382300070628     C                   EXSR      STAAMX
382400070628     C*--------------------
382500070628     C* STAMPA  N U M E R O   C O N S E G N A   L O C A L I T A'   D I S A G I A T E
382600070628     C*--------------------
382700070628     C                   Z-ADD     $V2           $VA
382800070628     C                   MOVEL     DME(15)       $DE
382900070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
383000070628     C                   EXSR      STAAMX
383100071210      *
383200071211     c                   eval      Righe_da_stampare = 1
383300071211     c                   exsr      solo_x_STT
383400070628     C                   WRITE     SE62RV
383500071210      *
383600070628     C                   EXSR      STATES
383700070628E   1C                   ENDIF
383800070628     C*--------------------
383900070628     C* CONSEGNE DDT --> VARIA = "B"
384000070628     C*--------------------
384100070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
384200070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
384300070628     C                   Z-ADD     1             P
384400070628     C                   SETOFF                                           28
384500070628     C     'B'           LOOKUP    VAR(P)                                 28
384600070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "B"
384700070628     C     P             OCCUR     CAMM
384800070628     C                   ADD       ARI           $VA                            *RICAVI "B"
384900070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "B"
385000070628E   1C                   ENDIF
385100070628     C*
385200070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
385300070628     C                   XFOOT     $VA           COM$VA           16 3
385400070628     C                   XFOOT     $V2           COM$V2           16 3
385500070628IF  1C     COM$VA        IFNE      *ZEROS
385600070628     C     COM$V2        ORNE      *ZEROS
385700070628IF  2C     DICUI         IFEQ      '0'
385800070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
385900070628E   2C                   ENDIF
386000070628     C*--------------------
386100070628     C* STAMPA  R I C A V I   C O N S E G N A   D D T
386200070628     C*--------------------
386300070628     C                   MOVEL     DME(25)       $DE
386400070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
386500070628     C                   EXSR      STAAMX
386600070628     C*--------------------
386700070628     C* STAMPA  N U M E R O   C O N S E G N A   D D T
386800070628     C*--------------------
386900070628     C                   Z-ADD     $V2           $VA
387000070628     C                   MOVEL     DME(15)       $DE
387100070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
387200070628     C                   EXSR      STAAMX
387300071210      *
387400071211     c                   eval      Righe_da_stampare = 1
387500071211     c                   exsr      solo_x_STT
387600070628     C                   WRITE     SE62RV
387700071210      *
387800070628     C                   EXSR      STATES
387900070628E   1C                   ENDIF
388000070628     C*--------------------
388100070628     C* RITIRI --> VARIA = "U"
388200070628     C*--------------------
388300070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
388400070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
388500070628     C                   Z-ADD     1             P
388600070628     C                   SETOFF                                           28
388700070628     C     'U'           LOOKUP    VAR(P)                                 28
388800070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "U"
388900070628     C     P             OCCUR     CAMM
389000070628     C                   ADD       ARI           $VA                            *RICAVI "U"
389100070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "U"
389200070628E   1C                   ENDIF
389300070628     C*
389400070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
389500070628     C                   XFOOT     $VA           COM$VA           16 3
389600070628     C                   XFOOT     $V2           COM$V2           16 3
389700070628IF  1C     COM$VA        IFNE      *ZEROS
389800070628     C     COM$V2        ORNE      *ZEROS
389900070628IF  2C     DICUI         IFEQ      '0'
390000070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
390100070628E   2C                   ENDIF
390200070628     C*--------------------
390300070628     C* STAMPA  R I C A V I   R I T I R I
390400070628     C*--------------------
390500070628     C                   MOVEL     DME(31)       $DE
390600070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
390700070628     C                   EXSR      STAAMX
390800070628     C*--------------------
390900070628     C* STAMPA  N U M E R O   R I T I R I
391000070628     C*--------------------
391100070628     C                   Z-ADD     $V2           $VA
391200070628     C                   MOVEL     DME(15)       $DE
391300070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
391400070628     C                   EXSR      STAAMX
391500071210      *
391600071211     c                   eval      Righe_da_stampare = 1
391700071211     c                   exsr      solo_x_STT
391800070628     C                   WRITE     SE62RV
391900071210      *
392000070628     C                   EXSR      STATES
392100070628E   1C                   ENDIF
392200070628     C*--------------------
392300070628     C* CONSEGNE ISOLA --> VARIA = "J"
392400070628     C*--------------------
392500070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
392600070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
392700070628     C                   Z-ADD     1             P
392800070628     C                   SETOFF                                           28
392900070628     C     'J'           LOOKUP    VAR(P)                                 28
393000070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "J"
393100070628     C     P             OCCUR     CAMM
393200070628     C                   ADD       ARI           $VA                            *RICAVI "J"
393300070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "J"
393400070628E   1C                   ENDIF
393500070628     C*
393600070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
393700070628     C                   XFOOT     $VA           COM$VA           16 3
393800070628     C                   XFOOT     $V2           COM$V2           16 3
393900070628IF  1C     COM$VA        IFNE      *ZEROS
394000070628     C     COM$V2        ORNE      *ZEROS
394100070628IF  2C     DICUI         IFEQ      '0'
394200070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
394300070628E   2C                   ENDIF
394400070628     C*--------------------
394500070628     C* STAMPA  R I C A V I   C O N S E G N E   I S O L A
394600070628     C*--------------------
394700070628     C                   MOVEL     DME(32)       $DE
394800070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
394900070628     C                   EXSR      STAAMX
395000070628     C*--------------------
395100070628     C* STAMPA  N U M E R O   C O N S E G N E   I S O L A
395200070628     C*--------------------
395300070628     C                   Z-ADD     $V2           $VA
395400070628     C                   MOVEL     DME(15)       $DE
395500070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
395600070628     C                   EXSR      STAAMX
395700071210      *
395800071211     c                   eval      Righe_da_stampare = 1
395900071211     c                   exsr      solo_x_STT
396000070628     C                   WRITE     SE62RV
396100071210      *
396200070628     C                   EXSR      STATES
396300070628E   1C                   ENDIF
396400070628     C*--------------------
396500070628     C* CONSEGNA DISAGIATA --> VARIA = "X"
396600070628     C*--------------------
396700070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
396800070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
396900070628     C                   Z-ADD     1             P
397000070628     C                   SETOFF                                           28
397100070628     C     'X'           LOOKUP    VAR(P)                                 28
397200070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "X"
397300070628     C     P             OCCUR     CAMM
397400070628     C                   ADD       ARI           $VA                            *RICAVI "X"
397500070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "X"
397600070628E   1C                   ENDIF
397700070628     C*
397800070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
397900070628     C                   XFOOT     $VA           COM$VA           16 3
398000070628     C                   XFOOT     $V2           COM$V2           16 3
398100070628IF  1C     COM$VA        IFNE      *ZEROS
398200070628     C     COM$V2        ORNE      *ZEROS
398300070628IF  2C     DICUI         IFEQ      '0'
398400070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
398500070628E   2C                   ENDIF
398600070628     C*--------------------
398700070628     C* STAMPA  R I C A V I   C O N S E G N E  D I S A G I A T E
398800070628     C*--------------------
398900070628     C                   MOVEL     DME(33)       $DE
399000070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
399100070628     C                   EXSR      STAAMX
399200070628     C*--------------------
399300070628     C* STAMPA  N U M E R O   C O N S E G N E  D I S A G I A T E
399400070628     C*--------------------
399500070628     C                   Z-ADD     $V2           $VA
399600070628     C                   MOVEL     DME(15)       $DE
399700070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
399800070628     C                   EXSR      STAAMX
399900071210      *
400000071211     c                   eval      Righe_da_stampare = 1
400100071211     c                   exsr      solo_x_STT
400200070628     C                   WRITE     SE62RV
400300071210      *
400400070628     C                   EXSR      STATES
400500070628E   1C                   ENDIF
400600070628     C*--------------------
400700070628     C* RITIRI ANNULLATI --> VARIA = "Y"
400800070628     C*--------------------
400900070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
401000070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
401100070628     C                   Z-ADD     1             P
401200070628     C                   SETOFF                                           28
401300070628     C     'Y'           LOOKUP    VAR(P)                                 28
401400070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "Y"
401500070628     C     P             OCCUR     CAMM
401600070628     C                   ADD       ARI           $VA                            *RICAVI "Y"
401700070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "Y"
401800070628E   1C                   ENDIF
401900070628     C*
402000070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
402100070628     C                   XFOOT     $VA           COM$VA           16 3
402200070628     C                   XFOOT     $V2           COM$V2           16 3
402300070628IF  1C     COM$VA        IFNE      *ZEROS
402400070628     C     COM$V2        ORNE      *ZEROS
402500070628IF  2C     DICUI         IFEQ      '0'
402600070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
402700070628E   2C                   ENDIF
402800070628     C*--------------------
402900070628     C* STAMPA  R I C A V I   R I T I R I  A N N U L L A T I
403000070628     C*--------------------
403100070628     C                   MOVEL     DME(34)       $DE
403200070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
403300070628     C                   EXSR      STAAMX
403400070628     C*--------------------
403500070628     C* STAMPA  N U M E R O   R I T I R I  A N N U L L A T I
403600070628     C*--------------------
403700070628     C                   Z-ADD     $V2           $VA
403800070628     C                   MOVEL     DME(15)       $DE
403900070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
404000070628     C                   EXSR      STAAMX
404100071210      *
404200071211     c                   eval      Righe_da_stampare = 1
404300071211     c                   exsr      solo_x_STT
404400070628     C                   WRITE     SE62RV
404500071210      *
404600070628     C                   EXSR      STATES
404700070628E   1C                   ENDIF
404800070628     C*--------------------
404900070628     C* RICERCA D'ARCHIVIO --> VARIA = "O"
405000070628     C*--------------------
405100070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
405200070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
405300070628     C                   Z-ADD     1             P
405400070628     C                   SETOFF                                           28
405500070628     C     'O'           LOOKUP    VAR(P)                                 28
405600070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "O"
405700070628     C     P             OCCUR     CAMM
405800070628     C                   ADD       ARI           $VA                            *RICAVI "O"
405900070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "O"
406000070628E   1C                   ENDIF
406100070628     C*
406200070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
406300070628     C                   XFOOT     $VA           COM$VA           16 3
406400070628     C                   XFOOT     $V2           COM$V2           16 3
406500070628IF  1C     COM$VA        IFNE      *ZEROS
406600070628     C     COM$V2        ORNE      *ZEROS
406700070628IF  2C     DICUI         IFEQ      '0'
406800070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
406900070628E   2C                   ENDIF
407000070628     C*--------------------
407100070628     C* STAMPA  R I C A V I   R I C E R C H E  D' A R C H I V I O
407200070628     C*--------------------
407300070628     C                   MOVEL     DME(35)       $DE
407400070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
407500070628     C                   EXSR      STAAMX
407600070628     C*--------------------
407700070628     C* STAMPA  N U M E R O   R I C E R C H E  D' A R C H I V I O
407800070628     C*--------------------
407900070628     C                   Z-ADD     $V2           $VA
408000070628     C                   MOVEL     DME(15)       $DE
408100070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
408200070628     C                   EXSR      STAAMX
408300071210      *
408400071211     c                   eval      Righe_da_stampare = 1
408500071211     c                   exsr      solo_x_STT
408600070628     C                   WRITE     SE62RV
408700071210      *
408800070628     C                   EXSR      STATES
408900070628E   1C                   ENDIF
409000070628     C*--------------------
409100070628     C* P.O.D. IMMAGE      --> VARIA = "a"
409200070628     C*--------------------
409300070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
409400070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
409500070628     C                   Z-ADD     1             P
409600070628     C                   SETOFF                                           28
409700070628     C     'a'           LOOKUP    VAR(P)                                 28
409800070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "O"
409900070628     C     P             OCCUR     CAMM
410000070628     C                   ADD       ARI           $VA                            *RICAVI "O"
410100070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "O"
410200070628E   1C                   ENDIF
410300070628     C*
410400070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
410500070628     C                   XFOOT     $VA           COM$VA           16 3
410600070628     C                   XFOOT     $V2           COM$V2           16 3
410700070628IF  1C     COM$VA        IFNE      *ZEROS
410800070628     C     COM$V2        ORNE      *ZEROS
410900070628IF  2C     DICUI         IFEQ      '0'
411000070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
411100070628E   2C                   ENDIF
411200070628     C*--------------------
411300070628     C* STAMPA  R I C A V I   P.O.D. IMMAGE
411400070628     C*--------------------
411500070628     C                   MOVEL     DME(46)       $DE
411600070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
411700070628     C                   EXSR      STAAMX
411800070628     C*--------------------
411900070628     C* STAMPA  N U M E R O   P.O.D. IMMAGE
412000070628     C*--------------------
412100070628     C                   Z-ADD     $V2           $VA
412200070628     C                   MOVEL     DME(15)       $DE
412300070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
412400070628     C                   EXSR      STAAMX
412500071210      *
412600071211     c                   eval      Righe_da_stampare = 1
412700071211     c                   exsr      solo_x_STT
412800070628     C                   WRITE     SE62RV
412900071210      *
413000070628     C                   EXSR      STATES
413100070628E   1C                   ENDIF
413200070628     C*--------------------
413300070628     C* DIRITTI DI PESATURA --> VARIA = "5"
413400070628     C*--------------------
413500070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
413600070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
413700070628     C                   Z-ADD     1             P
413800070628     C                   SETOFF                                           28
413900070628     C     '5'           LOOKUP    VAR(P)                                 28
414000070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "5"
414100070628     C     P             OCCUR     CAMM
414200070628     C                   ADD       ARI           $VA                            *RICAVI "5"
414300070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "5"
414400070628E   1C                   ENDIF
414500070628     C*
414600070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
414700070628     C                   XFOOT     $VA           COM$VA           16 3
414800070628     C                   XFOOT     $V2           COM$V2           16 3
414900070628IF  1C     COM$VA        IFNE      *ZEROS
415000070628     C     COM$V2        ORNE      *ZEROS
415100070628IF  2C     DICUI         IFEQ      '0'
415200070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
415300070628E   2C                   ENDIF
415400070628     C*--------------------
415500070628     C* STAMPA  R I C A V I   D I R I T T I  D I  P E S A T U R A
415600070628     C*--------------------
415700070628     C                   MOVEL     DME(36)       $DE
415800070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
415900070628     C                   EXSR      STAAMX
416000070628     C*--------------------
416100070628     C* STAMPA  N U M E R O   D I R I T T I  D I  P E S A T U R A
416200070628     C*--------------------
416300070628     C                   Z-ADD     $V2           $VA
416400070628     C                   MOVEL     DME(15)       $DE
416500070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
416600070628     C                   EXSR      STAAMX
416700071210      *
416800071211     c                   eval      Righe_da_stampare = 1
416900071211     c                   exsr      solo_x_STT
417000070628     C                   WRITE     SE62RV
417100071210      *
417200070628     C                   EXSR      STATES
417300070628E   1C                   ENDIF
417400070628     C*--------------------
417500070628     C* LASCIATO AVVISO --> VARIA = "#"
417600070628     C*--------------------
417700070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
417800070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
417900070628     C                   Z-ADD     1             P
418000070628     C                   SETOFF                                           28
418100070628     C     '#'           LOOKUP    VAR(P)                                 28
418200070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "B"
418300070628     C     P             OCCUR     CAMM
418400070628     C                   ADD       ARI           $VA                            *RICAVI "B"
418500070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "B"
418600070628E   1C                   ENDIF
418700070628     C*
418800070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
418900070628     C                   XFOOT     $VA           COM$VA           16 3
419000070628     C                   XFOOT     $V2           COM$V2           16 3
419100070628IF  1C     COM$VA        IFNE      *ZEROS
419200070628     C     COM$V2        ORNE      *ZEROS
419300070628IF  2C     DICUI         IFEQ      '0'
419400070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
419500070628E   2C                   ENDIF
419600070628     C*--------------------
419700070628     C* STAMPA  R I C A V I   LASCIATO AVVISO
419800070628     C*--------------------
419900070628     C                   MOVEL     DME(37)       $DE
420000070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
420100070628     C                   EXSR      STAAMX
420200070628     C*--------------------
420300070628     C* STAMPA  N U M E R O   LASCIATO AVVISO
420400070628     C*--------------------
420500070628     C                   Z-ADD     $V2           $VA
420600070628     C                   MOVEL     DME(15)       $DE
420700070628     C                   MOVE      ' NR.'        $DE
420800070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
420900070628     C                   EXSR      STAAMX
421000071210      *
421100071211     c                   eval      Righe_da_stampare = 1
421200071211     c                   exsr      solo_x_STT
421300070628     C                   WRITE     SE62RV
421400071210      *
421500070628     C                   EXSR      STATES
421600070628E   1C                   ENDIF
421700070628     C*--------------------
421800070628     C* ANN.PORTO ASSEGNATO--> VARIA = "&"
421900070628     C*--------------------
422000070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
422100070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
422200070628     C                   Z-ADD     1             P
422300070628     C                   SETOFF                                           28
422400070628     C     '&'           LOOKUP    VAR(P)                                 28
422500070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "&"
422600070628     C     P             OCCUR     CAMM
422700070628     C                   ADD       ARI           $VA                            *RICAVI "&"
422800070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "&"
422900070628E   1C                   ENDIF
423000070628     C*
423100070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
423200070628     C                   XFOOT     $VA           COM$VA           16 3
423300070628     C                   XFOOT     $V2           COM$V2           16 3
423400070628IF  1C     COM$VA        IFNE      *ZEROS
423500070628     C     COM$V2        ORNE      *ZEROS
423600070628IF  2C     DICUI         IFEQ      '0'
423700070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
423800070628E   2C                   ENDIF
423900070628     C*--------------------
424000070628     C* STAMPA  R I C A V I   ANN.PORTO ASSEGNATO
424100070628     C*--------------------
424200070628     C                   MOVEL     DME(47)       $DE
424300070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
424400070628     C                   EXSR      STAAMX
424500070628     C*--------------------
424600070628     C* STAMPA  N U M E R O   ANN.PORTO ASSEGNATO
424700070628     C*--------------------
424800070628     C                   Z-ADD     $V2           $VA
424900070628     C                   MOVEL     DME(15)       $DE
425000070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
425100070628     C                   EXSR      STAAMX
425200071210      *
425300071211     c                   eval      Righe_da_stampare = 1
425400071211     c                   exsr      solo_x_STT
425500070628     C                   WRITE     SE62RV
425600071210      *
425700070628     C                   EXSR      STATES
425800070628E   1C                   ENDIF
425900070628     C*--------------------
426000070628     C* DIROTTAMENTI       --> VARIA = "*"
426100070628     C*--------------------
426200070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
426300070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
426400070628     C                   Z-ADD     1             P
426500070628     C                   SETOFF                                           28
426600070628     C     '*'           LOOKUP    VAR(P)                                 28
426700070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "*"
426800070628     C     P             OCCUR     CAMM
426900070628     C                   ADD       ARI           $VA                            *RICAVI "*"
427000070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "*"
427100070628E   1C                   ENDIF
427200070628     C*
427300070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
427400070628     C                   XFOOT     $VA           COM$VA           16 3
427500070628     C                   XFOOT     $V2           COM$V2           16 3
427600070628IF  1C     COM$VA        IFNE      *ZEROS
427700070628     C     COM$V2        ORNE      *ZEROS
427800070628IF  2C     DICUI         IFEQ      '0'
427900070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
428000070628E   2C                   ENDIF
428100070628     C*--------------------
428200070628     C* STAMPA  R I C A V I   DIROTTAMENTI
428300070628     C*--------------------
428400070628     C                   MOVEL     DME(48)       $DE
428500070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
428600070628     C                   EXSR      STAAMX
428700070628     C*--------------------
428800070628     C* STAMPA  N U M E R O   DIROTTAMENTI
428900070628     C*--------------------
429000070628     C                   Z-ADD     $V2           $VA
429100070628     C                   MOVEL     DME(15)       $DE
429200070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
429300070628     C                   EXSR      STAAMX
429400071210      *
429500071211     c                   eval      Righe_da_stampare = 1
429600071211     c                   exsr      solo_x_STT
429700070628     C                   WRITE     SE62RV
429800071210      *
429900070628     C                   EXSR      STATES
430000070628E   1C                   ENDIF
430100070628     C*--------------------
430200070628     C* RITIRO BANCALI     --> VARIA = "="
430300070628     C*--------------------
430400070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
430500070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
430600070628     C                   Z-ADD     1             P
430700070628     C                   SETOFF                                           28
430800070628     C     '='           LOOKUP    VAR(P)                                 28
430900070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "="
431000070628     C     P             OCCUR     CAMM
431100070628     C                   ADD       ARI           $VA                            *RICAVI "="
431200070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "="
431300070628E   1C                   ENDIF
431400070628     C*
431500070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
431600070628     C                   XFOOT     $VA           COM$VA           16 3
431700070628     C                   XFOOT     $V2           COM$V2           16 3
431800070628IF  1C     COM$VA        IFNE      *ZEROS
431900070628     C     COM$V2        ORNE      *ZEROS
432000070628IF  2C     DICUI         IFEQ      '0'
432100070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
432200070628E   2C                   ENDIF
432300070628     C*--------------------
432400070628     C* STAMPA  R I C A V I   RITIRO BANCALI
432500070628     C*--------------------
432600070628     C                   MOVEL     DME(49)       $DE
432700070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
432800070628     C                   EXSR      STAAMX
432900070628     C*--------------------
433000070628     C* STAMPA  N U M E R O   RITIRO BANCALI
433100070628     C*--------------------
433200070628     C                   Z-ADD     $V2           $VA
433300070628     C                   MOVEL     DME(15)       $DE
433400070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
433500070628     C                   EXSR      STAAMX
433600071210      *
433700071211     c                   eval      Righe_da_stampare = 1
433800071211     c                   exsr      solo_x_STT
433900070628     C                   WRITE     SE62RV
434000071210      *
434100070628     C                   EXSR      STATES
434200070628E   1C                   ENDIF
434300070628     C*--------------------
434400070628     C* REC. C/ASS.        --> VARIA = "W"
434500070628     C*--------------------
434600070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
434700070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
434800070628     C                   Z-ADD     1             P
434900070628     C                   SETOFF                                           28
435000070628     C     'W'           LOOKUP    VAR(P)                                 28
435100070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "W"
435200070628     C     P             OCCUR     CAMM
435300070628     C                   ADD       ARI           $VA                            *RICAVI "W"
435400070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "W"
435500070628E   1C                   ENDIF
435600070628     C*
435700070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
435800070628     C                   XFOOT     $VA           COM$VA           16 3
435900070628     C                   XFOOT     $V2           COM$V2           16 3
436000070628IF  1C     COM$VA        IFNE      *ZEROS
436100070628     C     COM$V2        ORNE      *ZEROS
436200070628IF  2C     DICUI         IFEQ      '0'
436300070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
436400070628E   2C                   ENDIF
436500070628     C*--------------------
436600070628     C* STAMPA  R I C A V I   REC. C/ASS.
436700070628     C*--------------------
436800070628     C                   MOVEL     DME(50)       $DE
436900070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
437000070628     C                   EXSR      STAAMX
437100070628     C*--------------------
437200070628     C* STAMPA  N U M E R O   REC. C/ASS.
437300070628     C*--------------------
437400070628     C                   Z-ADD     $V2           $VA
437500070628     C                   MOVEL     DME(15)       $DE
437600070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
437700070628     C                   EXSR      STAAMX
437800071210      *
437900071211     c                   eval      Righe_da_stampare = 1
438000071211     c                   exsr      solo_x_STT
438100070628     C                   WRITE     SE62RV
438200071210      *
438300070628     C                   EXSR      STATES
438400070628E   1C                   ENDIF
438500070628     C*--------------------
438600070628     C* ORM COMMISSIONATO  --> VARIA = "T"
438700070628     C*--------------------
438800070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
438900070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
439000070628     C                   Z-ADD     1             P
439100070628     C                   SETOFF                                           28
439200070628     C     'T'           LOOKUP    VAR(P)                                 28
439300070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "T"
439400070628     C     P             OCCUR     CAMM
439500070628     C                   ADD       ARI           $VA                            *RICAVI "T"
439600070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "T"
439700070628E   1C                   ENDIF
439800070628     C*
439900070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
440000070628     C                   XFOOT     $VA           COM$VA           16 3
440100070628     C                   XFOOT     $V2           COM$V2           16 3
440200070628IF  1C     COM$VA        IFNE      *ZEROS
440300070628     C     COM$V2        ORNE      *ZEROS
440400070628IF  2C     DICUI         IFEQ      '0'
440500070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
440600070628E   2C                   ENDIF
440700070628     C*--------------------
440800070628     C* STAMPA  R I C A V I   ORM COMMISSIONATO
440900070628     C*--------------------
441000070628     C                   MOVEL     DME(51)       $DE
441100070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
441200070628     C                   EXSR      STAAMX
441300070628     C*--------------------
441400070628     C* STAMPA  N U M E R O   ORM COMMISSIONATO
441500070628     C*--------------------
441600070628     C                   Z-ADD     $V2           $VA
441700070628     C                   MOVEL     DME(15)       $DE
441800070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
441900070628     C                   EXSR      STAAMX
442000071210      *
442100071211     c                   eval      Righe_da_stampare = 1
442200071211     c                   exsr      solo_x_STT
442300070628     C                   WRITE     SE62RV
442400071210      *
442500070628     C                   EXSR      STATES
442600070628E   1C                   ENDIF
442700070628     C*--------------------
442800070628     C* L.AVVISO           --> VARIA = "c"
442900070628     C*--------------------
443000070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
443100070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
443200070628     C                   Z-ADD     1             P
443300070628     C                   SETOFF                                           28
443400070628     C     'c'           LOOKUP    VAR(P)                                 28
443500070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "T"
443600070628     C     P             OCCUR     CAMM
443700070628     C                   ADD       ARI           $VA                            *RICAVI "T"
443800070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "T"
443900070628E   1C                   ENDIF
444000070628     C*
444100070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
444200070628     C                   XFOOT     $VA           COM$VA           16 3
444300070628     C                   XFOOT     $V2           COM$V2           16 3
444400070628IF  1C     COM$VA        IFNE      *ZEROS
444500070628     C     COM$V2        ORNE      *ZEROS
444600070628IF  2C     DICUI         IFEQ      '0'
444700070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
444800070628E   2C                   ENDIF
444900070628     C*--------------------
445000070628     C* STAMPA  R I C A V I   L.AVVISO
445100070628     C*--------------------
445200070628     C                   MOVEL     DME(62)       $DE
445300070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
445400070628     C                   EXSR      STAAMX
445500070628     C*--------------------
445600070628     C* STAMPA  N U M E R O   L.AVVISO
445700070628     C*--------------------
445800070628     C                   Z-ADD     $V2           $VA
445900070628     C                   MOVEL     DME(15)       $DE
446000070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
446100070628     C                   EXSR      STAAMX
446200071210      *
446300071211     c                   eval      Righe_da_stampare = 1
446400071211     c                   exsr      solo_x_STT
446500070628     C                   WRITE     SE62RV
446600071210      *
446700070628     C                   EXSR      STATES
446800070628E   1C                   ENDIF
446900070628     C*
447000070628     C                   ENDSR
447100000000     C*------------------------------------------------------------------------*
447200000000     C*?STACUI - STAMPA STRINGA "DI CUI"
447300000000     C*------------------------------------------------------------------------*
447400000000     C     STACUI        BEGSR
447500000000     C*
447600000000     C                   MOVEL     '1'           DICUI             1
447700000000     C*
447800071211     c                   eval      Righe_da_stampare = 1
447900071211     c                   exsr      solo_x_STT
448000010920     C                   WRITE     SE62RV
448100071210      *
448200020823     C                   EXSR      STATES
448300000000     C*
448400010920     C                   CLEAR                   SE62P1
448500000000     C                   MOVEL     DME(16)       P1DDES
448600071210      *
448700071211     c                   eval      Righe_da_stampare = 1
448800071211     c                   exsr      solo_x_STT
448900020524     C                   WRITE     SE62P1                                       *"DI CUI:"
449000071210      *
449100020823     C                   EXSR      STATES
449200071210      *
449300071211     c                   eval      Righe_da_stampare = 1
449400071211     c                   exsr      solo_x_STT
449500010920     C                   WRITE     SE62RV
449600071210      *
449700020823     C                   EXSR      STATES
449800000000     C*
449900000000     C                   ENDSR
450000000000     C*------------------------------------------------------------------------*
450100000000     C*?STAAMX - IMPOSTA STAMPA RIGA XXX STATISTICA AMMNISTRAZIONE
450200000000     C*          INPUT --> $VA  : SCHIERA VALORI DI 12 MESI
450300000000     C*                    $DE  : DESCRIZIONE RIGA DA STAMPARE
450400000000     C*                    $FA  : TOTALE FATTURATO
450500000000     C*                    $NR  : TOTALE NOTE ACCREDITO O RETTIFICHE
450600000000     C*------------------------------------------------------------------------*
450700000000     C     STAAMX        BEGSR
450800000000     C*
450900000000     C*--------------------
451000000000     C* 13^ C O L O N N A
451100000000     C*--------------------
451200000000     C* PER LA RIGA DELLE PERCENTUALI NON POSSO FARE LA XFOOT
451300000000IF  1C     TIPVAL        IFNE      'PER'                                        *N O   P E R C. L E
451400000000     C                   XFOOT     $VA           $VA13C           16 3
451500000000X   1C                   ELSE                                                   *S I   D E L T A
451600000000     C                   Z-ADD     $FA           WFAW             16 3          *FATTURATO
451700000000     C                   Z-ADD     $NR           WNRW             16 3          *NOTE O RETTIFICHE
451800000629     C                   Z-ADD     *ZEROS        WPDW              4 1          *PERC FATTURATO
451900000000     C                   EXSR      CALPER
452000000629     C                   Z-ADD     WPDW          $VA13C                         *PERC 13^ COLONNA
452100000000E   1C                   ENDIF
452200000000     C*
452300000000     C* SE VALORE DI TIPO RICAVO LO COMPATTA
452400010920IF  1C***  TIPVAL        IFEQ      'RIC'
452500010920     C***  $VA           DIV       1000          $VA
452600010920     C***  $VA13C        DIV       1000          $VA13C
452700010920E   1C***                ENDIF
452800000000     C*
452900000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
453000000000IF  1C     TIPVAL        IFNE      'PER'                                        *N O   P E R C. L E
453100010920     C                   EXSR      STACAN                                       *RIGA ALTRI VALORI
453200010920X   1C                   ELSE                                                   *S I   P E R C. L E
453300010920IF  2C     TIPVAL        IFEQ      'RIC'                                        *R I C A V I
453400010920     C                   EXSR      STACAR
453500010920X   2C                   ELSE
453600000000     C                   EXSR      STACAP                                       *RIGA PERCENTUALE
453700010920E   2C                   ENDIF
453800000000E   1C                   ENDIF
453900000000     C*
454000000000     C                   ENDSR
454100000000     C*------------------------------------------------------------------------*
454200000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
454300000000     C*          INPUT  --> WRRW: RICAVO REALE
454400000000     C*                     WRPW: RICAVO PRESUNTO
454500000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
454600000000     C*------------------------------------------------------------------------*
454700000000     C     CALDEL        BEGSR
454800000000     C*
454900000000     C*--------------------
455000000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
455100000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
455200000000     C*--------------------
455300000000     C*
455400000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
455500000000     C     WRRW          SUB       WRPW          GAP              16 3          (REALE - PRESUNTO)
455600000000     C     GAP           MULT      100           GAP                            (GAP x 100)
455700000000     C*
455800000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
455900000000IF  1C     WRRW          IFEQ      *ZEROS
456000000000     C                   Z-ADD     1             WRRW
456100000000E   1C                   ENDIF
456200000000     C*
456300000000     C     GAP           DIV       WRRW          GAP
456400000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
456500000000     C*
456600000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
456700000000     C     WDPW          COMP      0                                    28
456800000000     C* POSITIVO
456900000000IF  1C     *IN28         IFEQ      *OFF
457000000000IF  2C     GAP           IFGE      1000
457100000000     C                   Z-ADD     +999,9        WDPW
457200000000E   2C                   ENDIF
457300000000     C* NEGATIVO
457400000000X   1C                   ELSE
457500000000IF  2C     GAP           IFLE      -1000
457600000000     C                   Z-ADD     -999,9        WDPW
457700000000E   2C                   ENDIF
457800000000E   1C                   ENDIF
457900000000     C*
458000000000     C                   ENDSR
458100000000     C*------------------------------------------------------------------------*
458200000000     C*?CALPER - CALCOLA LA PERCENTUALE SUL FATTURATO PER LA "AMMINISTRAZIONE"
458300000000     C*          INPUT  --> WFAW: FATTURATO
458400000000     C*                     WNRW: NOTE ACCREDITO O RETTITICHE
458500000629     C*          OUTPUT --> WPDW: PERCENTUALE SUL FATTURATO
458600000000     C*------------------------------------------------------------------------*
458700000000     C     CALPER        BEGSR
458800000000     C*
458900000000     C*               NOTE/RETTIFICHE/RICAVI GIACENZE x 100
459000000000     C* PERCENTUALE = -------------------------------------
459100000000     C*                           FATTURATO
459200000000     C*
459300000000     C* NB: SE IL FATTURATO E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
459400000000IF  1C     WFAW          IFEQ      *ZEROS
459500000000     C                   Z-ADD     1             WFAW
459600000000E   1C                   ENDIF
459700000000     C*
459800000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
459900000000     C     WNRW          MULT      100           RISUL            16 3
460000000000     C     RISUL         DIV       WFAW          RISUL
460100000629     C                   Z-ADD(H)  RISUL         WPDW
460200000000     C*
460300000000     C* SE LA PERCENTUALE E' +/-1000  METTO  +/-999,9 %
460400000629     C     WPDW          COMP      0                                    28
460500000000     C* POSITIVO
460600000000IF  1C     *IN28         IFEQ      *OFF
460700000000IF  2C     RISUL         IFGE      1000
460800000629     C                   Z-ADD     +999,9        WPDW
460900000000E   2C                   ENDIF
461000000000     C* NEGATIVO
461100000000X   1C                   ELSE
461200000000IF  2C     RISUL         IFLE      -1000
461300000629     C                   Z-ADD     -999,9        WPDW
461400000000E   2C                   ENDIF
461500000000E   1C                   ENDIF
461600000000     C*
461700000000     C                   ENDSR
461800000000     C*------------------------------------------------------------------------*
461900000000     C*?CALMED - CALCOLA LE MEDIE FINALI PER LA "SERVIZIO BOLLE"
462000000000     C*          INPUT --> WSPW: SPEDIZIONI DEL MESE
462100000000     C*                    WCOW: COLLI DEL MESE
462200000000     C*                    WPEW: PESO IN QUINTALI DEL MESE
462300000000     C*                    WVOW: VOLUME IN M.CUBI DEL MESE
462400000000     C*                    WRIW: RICAVI DEL MESE
462500000000     C*                    I   : MEDIA DA CALCOLARE (1÷9)
462600000000     C*          INPUT --> WMED: MEDIA CALCOLATA
462700000000     C*------------------------------------------------------------------------*
462800000000     C     CALMED        BEGSR
462900000000     C*
463000010920     C*    COLLI/KG /M.CUBO       KG/M.CUBO           RICAVI
463100000000     C* 1] ---------------- ;  2] --------- ;  3] -----------------------
463200000000     C*       SPEDIZIONI            COLLI          SPED/COLLI/Q.LE/M.CUBO
463300000000     C*---------------------
463400000000     C* PRIMA SERIE
463500000000     C*---------------------
463600000000     C* SE LE SPEDIZIONI SONO = 0, LE IMPOSTO = 1 PER NON DARE ERRORE
463700000000IF  1C     I             IFEQ      1
463800000000     C     I             OREQ      2
463900000000     C     I             OREQ      3
464000000000IF  2C     WSPW          IFEQ      *ZEROS
464100000000     C                   Z-ADD     1             WSPW
464200000000E   2C                   ENDIF
464300000000     C*
464400000000IF  2C     I             IFEQ      1
464500000000     C     WCOW          DIV       WSPW          WMED                           *COLLI/SPEDIZIONI
464600000000E   2C                   ENDIF
464700000000     C*
464800000000IF  2C     I             IFEQ      2
464900000000     C     WPEW          DIV       WSPW          WMED                           *KG/SPEDIZIONI
465000000000E   2C                   ENDIF
465100000000     C*
465200000000IF  2C     I             IFEQ      3
465300000000     C     WVOW          DIV       WSPW          WMED                           *VOLUME/SPEDIZIONI
465400000000E   2C                   ENDIF
465500000000E   1C                   ENDIF
465600000000     C*---------------------
465700000000     C* SECONDA SERIE
465800000000     C*---------------------
465900000000     C* SE I COLLI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
466000000000IF  1C     I             IFEQ      4
466100000000     C     I             OREQ      5
466200000000IF  2C     WCOW          IFEQ      *ZEROS
466300000000     C                   Z-ADD     1             WCOW
466400000000E   2C                   ENDIF
466500000000     C*
466600000000IF  2C     I             IFEQ      4
466700000000     C     WPEW          DIV       WCOW          WMED                           *KG/COLLI
466800000000E   2C                   ENDIF
466900000000     C*
467000000000IF  2C     I             IFEQ      5
467100000000     C     WVOW          DIV       WCOW          WMED                           *VOLUME/COLLI
467200000000E   2C                   ENDIF
467300000000E   1C                   ENDIF
467400000000     C*---------------------
467500000000     C* TERZA SERIE
467600000000     C*---------------------
467700000000IF  1C     I             IFEQ      6
467800000000     C     I             OREQ      7
467900000000     C     I             OREQ      8
468000000000     C     I             OREQ      9
468100000000     C*
468200000000     C* TRASFORMO IL PESO DA KILOGRAMMI IN QUINTALI
468300000000     C     WPEW          DIV       100           WPEW
468400000000     C*
468500000000IF  2C     I             IFEQ      6
468600000000     C* SE LE SPEDIZIONI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
468700000000IF  3C     WSPW          IFEQ      *ZEROS
468800000000     C                   Z-ADD     1             WSPW
468900000000E   3C                   ENDIF
469000000000     C     WRIW          DIV       WSPW          WMED                           *INCASSO/SPEDIZIONI
469100000000E   2C                   ENDIF
469200000000     C*
469300000000IF  2C     I             IFEQ      7
469400000000     C* SE I COLLI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
469500000000IF  3C     WCOW          IFEQ      *ZEROS
469600000000     C                   Z-ADD     1             WCOW
469700000000E   3C                   ENDIF
469800000000     C     WRIW          DIV       WCOW          WMED                           *INCASSO/COLLI
469900000000E   2C                   ENDIF
470000000000     C*
470100000000IF  2C     I             IFEQ      8
470200000000     C* SE I QUINTALI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
470300000000IF  3C     WPEW          IFEQ      *ZEROS
470400000000     C                   Z-ADD     1             WPEW
470500000000E   3C                   ENDIF
470600000000     C     WRIW          DIV       WPEW          WMED                           *INCASSO/QUINTALI
470700000000E   2C                   ENDIF
470800000000     C*
470900000000IF  2C     I             IFEQ      9
471000000000     C* SE IL VOLUME E' = 0, LO IMPOSTO = 1 PER NON DARE ERRORE
471100000000IF  3C     WVOW          IFEQ      *ZEROS
471200000000     C                   Z-ADD     1             WVOW
471300000000E   3C                   ENDIF
471400000000     C     WRIW          DIV       WVOW          WMED                           *INCASSO/VOLUME
471500000000E   2C                   ENDIF
471600000000     C*
471700000000E   1C                   ENDIF
471800000000     C*
471900000000     C                   ENDSR
472000000000     C*------------------------------------------------------------------------*
472100000000     C*?CALINC - CALCOLA L'INCIDENZA DELLE CONSEGNE DELLE SPEDIZIONI
472200020131     C*          INPUT  -->WTXW : TOTALE SERVIZIO ORE
472300000000     C*                    WVAW : TOTALE SERVIZIO ORA
472400020131     C*          OUTPUT -->WINW : PERCENTUALE DI INCIDENZA
472500000000     C*------------------------------------------------------------------------*
472600000000     C     CALINC        BEGSR
472700000000     C*
472800000000     C*                   CORRIERE/DISTRIB./ESPRESSO DI OGNI ORAIO x 100
472900000000     C*INCIDENZA= --------------------------------------------------------------
473000000000     C*           TOT CORRIER/DISTR./ESPRESSO oppure TOT CORRIER+DISTR.+ESPRESSO
473100000000     C*
473200000000     C* NB: SE IL TOTALE SPEDIZIONI E' = 0 LO IMPOSTO A 1 PER NON DARE ERRORE
473300000000     C*     <NEL CASO NON CI SIANO SPEDIZIONI PER UNA DETERMINATA ORA (0/1=0)
473400000000IF  1C     WTXW          IFEQ      *ZEROS
473500000000     C                   Z-ADD     1             WTXW
473600000000E   1C                   ENDIF
473700000000     C*
473800000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
473900000000     C     WVAW          MULT      100           RISUL            16 3
474000000000     C     RISUL         DIV       WTXW          RISUL
474100000000     C*
474200000000     C* SE LA PERCENTUALE E' 100%  METTO  99% PERCHE' IL CAMPO E' LUNGO 2
474300060525IF  1C**** RISUL         IFGE      100
474400060525IF  1C     RISUL         IFGE      99
474500000000     C                   Z-ADD     99            WINW
474600000000X   1C                   ELSE
474700000000     C                   Z-ADD(H)  RISUL         WINW              2 0
474800000000E   1C                   ENDIF
474900000000     C*
475000000000     C                   ENDSR
475100000000     C*------------------------------------------------------------------------*
475200000000     C*?STACAV - MUOVE I CAMPI "VOLUME/MEDIE" IN QUELLI DI STAMPA E STAMPA
475300000000     C*------------------------------------------------------------------------*
475400000000     C     STACAV        BEGSR
475500000000     C*
475600000000     C                   MOVEL     $DE           P2DDES                         *DESCRIZIONE VALORE
475700000000     C*
475800000000     C                   Z-ADD     $VA(1)        P2CVA1                         *MESE UNO
475900000000     C                   Z-ADD     $VA(2)        P2CVA2                         *MESE DUE
476000000000     C                   Z-ADD     $VA(3)        P2CVA3                         *MESE TRE
476100000000     C                   Z-ADD     $VA(4)        P2CVA4                         *MESE QUATTRO
476200000000     C                   Z-ADD     $VA(5)        P2CVA5                         *MESE CINQUE
476300000000     C                   Z-ADD     $VA(6)        P2CVA6                         *MESE SEI
476400000000     C                   Z-ADD     $VA(7)        P2CVA7                         *MESE SETTE
476500000000     C                   Z-ADD     $VA(8)        P2CVA8                         *MESE OTTO
476600000000     C                   Z-ADD     $VA(9)        P2CVA9                         *MESE NOVE
476700000000     C                   Z-ADD     $VA(10)       P2CVA0                         *MESE DIECI
476800000000     C                   Z-ADD     $VA(11)       P2CVAA                         *MESE UNDICI
476900000000     C                   Z-ADD     $VA(12)       P2CVAB                         *MESE DODICI
477000000000     C                   Z-ADD     $VA13C        P2CVAT                         *TOTALE VALORE
477100000000     C*
477200071211     c                   eval      Righe_da_stampare = 1
477300071211     c                   exsr      solo_x_STT
477400010920     C                   WRITE     SE62P2                                       *STAMPA RIGA VOL/MED
477500071210      *
477600020823     C                   EXSR      STATES
477700000000     C*
477800000000     C                   ENDSR
477900010920     C*------------------------------------------------------------------------*
478000010920     C*?STACAR - MUOVE I CAMPI "RICAVI" IN QUELLI DI STAMPA E STAMPA
478100010920     C*------------------------------------------------------------------------*
478200010920     C     STACAR        BEGSR
478300010920     C*
478400010920     C                   MOVEL     $DE           P5DDES                         *DESCRIZIONE VALORE
478500010920     C*
478600010920     C                   Z-ADD     $VA(1)        P5CVA1                         *MESE UNO
478700010920     C                   Z-ADD     $VA(2)        P5CVA2                         *MESE DUE
478800010920     C                   Z-ADD     $VA(3)        P5CVA3                         *MESE TRE
478900010920     C                   Z-ADD     $VA(4)        P5CVA4                         *MESE QUATTRO
479000010920     C                   Z-ADD     $VA(5)        P5CVA5                         *MESE CINQUE
479100010920     C                   Z-ADD     $VA(6)        P5CVA6                         *MESE SEI
479200010920     C                   Z-ADD     $VA(7)        P5CVA7                         *MESE SETTE
479300010920     C                   Z-ADD     $VA(8)        P5CVA8                         *MESE OTTO
479400010920     C                   Z-ADD     $VA(9)        P5CVA9                         *MESE NOVE
479500010920     C                   Z-ADD     $VA(10)       P5CVA0                         *MESE DIECI
479600010920     C                   Z-ADD     $VA(11)       P5CVAA                         *MESE UNDICI
479700010920     C                   Z-ADD     $VA(12)       P5CVAB                         *MESE DODICI
479800010920     C                   Z-ADD     $VA13C        P5CVAT                         *TOTALE VALORE
479900010920     C*
480000071211     c                   eval      Righe_da_stampare = 1
480100071211     c                   exsr      solo_x_STT
480200010920     C                   WRITE     SE62P5                                       *STAMPA RIGA RICAVI
480300071210      *
480400020823     C                   EXSR      STATES
480500010920     C*
480600010920     C                   ENDSR
480700020131     C*------------------------------------------------------------------------*
480800020131     C*?STACAR1 - MUOVE I CAMPI MEDIE "RICAVI" IN QUELLI DI STAMPA E STAMPA
480900020131     C*------------------------------------------------------------------------*
481000020131     C     STACAR1       BEGSR
481100020131     C*
481200020131     C                   MOVEL     $DE           P6DDES                         *DESCRIZIONE VALORE
481300020131     C*
481400020131     C                   Z-ADD     $VA(1)        P6CVA1                         *MESE UNO
481500020131     C                   Z-ADD     $VA(2)        P6CVA2                         *MESE DUE
481600020131     C                   Z-ADD     $VA(3)        P6CVA3                         *MESE TRE
481700020131     C                   Z-ADD     $VA(4)        P6CVA4                         *MESE QUATTRO
481800020131     C                   Z-ADD     $VA(5)        P6CVA5                         *MESE CINQUE
481900020131     C                   Z-ADD     $VA(6)        P6CVA6                         *MESE SEI
482000020131     C                   Z-ADD     $VA(7)        P6CVA7                         *MESE SETTE
482100020131     C                   Z-ADD     $VA(8)        P6CVA8                         *MESE OTTO
482200020131     C                   Z-ADD     $VA(9)        P6CVA9                         *MESE NOVE
482300020131     C                   Z-ADD     $VA(10)       P6CVA0                         *MESE DIECI
482400020131     C                   Z-ADD     $VA(11)       P6CVAA                         *MESE UNDICI
482500020131     C                   Z-ADD     $VA(12)       P6CVAB                         *MESE DODICI
482600020131     C                   Z-ADD     $VA13C        P6CVAT                         *TOTALE VALORE
482700020131     C*
482800071211     c                   eval      Righe_da_stampare = 1
482900071211     c                   exsr      solo_x_STT
483000020131     C                   WRITE     SE62P6                                       *STAMPA RIGA RICAVI
483100071210      *
483200020823     C                   EXSR      STATES
483300020131     C*
483400020131     C                   ENDSR
483500000000     C*------------------------------------------------------------------------*
483600000000     C*?STACAN - MUOVE I CAMPI "NORMALI" IN QUELLI DI STAMPA E STAMPA
483700000000     C*------------------------------------------------------------------------*
483800000000     C     STACAN        BEGSR
483900000000     C*
484000000000     C                   MOVEL     $DE           P1DDES                         *DESCRIZIONE VALORE
484100000000     C*
484200000000     C                   Z-ADD     $VA(1)        P1CVA1                         *MESE UNO
484300000000     C                   Z-ADD     $VA(2)        P1CVA2                         *MESE DUE
484400000000     C                   Z-ADD     $VA(3)        P1CVA3                         *MESE TRE
484500000000     C                   Z-ADD     $VA(4)        P1CVA4                         *MESE QUATTRO
484600000000     C                   Z-ADD     $VA(5)        P1CVA5                         *MESE CINQUE
484700000000     C                   Z-ADD     $VA(6)        P1CVA6                         *MESE SEI
484800000000     C                   Z-ADD     $VA(7)        P1CVA7                         *MESE SETTE
484900000000     C                   Z-ADD     $VA(8)        P1CVA8                         *MESE OTTO
485000000000     C                   Z-ADD     $VA(9)        P1CVA9                         *MESE NOVE
485100000000     C                   Z-ADD     $VA(10)       P1CVA0                         *MESE DIECI
485200000000     C                   Z-ADD     $VA(11)       P1CVAA                         *MESE UNDICI
485300000000     C                   Z-ADD     $VA(12)       P1CVAB                         *MESE DODICI
485400000000     C                   Z-ADD     $VA13C        P1CVAT                         *TOTALE VALORE
485500000000     C*
485600071211     c                   eval      Righe_da_stampare = 1
485700071211     c                   exsr      solo_x_STT
485800010920     C                   WRITE     SE62P1                                       *STAMPA RIGA VALORE
485900071210      *
486000020823     C                   EXSR      STATES
486100000000     C*
486200000000     C                   ENDSR
486300000000     C*------------------------------------------------------------------------*
486400000000     C*?STACAP - MUOVE I CAMPI "PERCENTUALI" IN QUELLI DI STAMPA E STAMPA
486500000000     C*------------------------------------------------------------------------*
486600000000     C     STACAP        BEGSR
486700000000     C*
486800000000     C                   MOVEL     $DE           P3DDES                         *DESCRIZIONE VALORE
486900000000     C*
487000000000     C                   Z-ADD     $VA(1)        P3CVA1                         *MESE UNO
487100000000     C                   Z-ADD     $VA(2)        P3CVA2                         *MESE DUE
487200000000     C                   Z-ADD     $VA(3)        P3CVA3                         *MESE TRE
487300000000     C                   Z-ADD     $VA(4)        P3CVA4                         *MESE QUATTRO
487400000000     C                   Z-ADD     $VA(5)        P3CVA5                         *MESE CINQUE
487500000000     C                   Z-ADD     $VA(6)        P3CVA6                         *MESE SEI
487600000000     C                   Z-ADD     $VA(7)        P3CVA7                         *MESE SETTE
487700000000     C                   Z-ADD     $VA(8)        P3CVA8                         *MESE OTTO
487800000000     C                   Z-ADD     $VA(9)        P3CVA9                         *MESE NOVE
487900000000     C                   Z-ADD     $VA(10)       P3CVA0                         *MESE DIECI
488000000000     C                   Z-ADD     $VA(11)       P3CVAA                         *MESE UNDICI
488100000000     C                   Z-ADD     $VA(12)       P3CVAB                         *MESE DODICI
488200000000     C                   Z-ADD     $VA13C        P3CVAT                         *TOTALE VALORE
488300000000     C*
488400071211     c                   eval      Righe_da_stampare = 1
488500071211     c                   exsr      solo_x_STT
488600010920     C                   WRITE     SE62P3                                       *STAMPA RIGA PERCENT
488700071210      *
488800020823     C                   EXSR      STATES
488900000000     C*
489000000000     C                   ENDSR
489100000000     C*------------------------------------------------------------------------*
489200000000     C*?STACA2 - MUOVE I CAMPI "PERCENTUALI" DI 2 IN QUELLI DI STAMPA E STAMPA
489300000000     C*------------------------------------------------------------------------*
489400000000     C     STACA2        BEGSR
489500000000     C*
489600000000     C                   MOVEL     $DE           P4DDES                         *DESCRIZIONE VALORE
489700000000     C*
489800000000     C                   Z-ADD     $P1(1)        P4CVN1                         *MESE UNO CORRIERE
489900000629     C                   Z-ADD     $P2(1)        P4CVD1                         *DISTRIBUZIONE
490000000000     C                   Z-ADD     $P3(1)        P4CVE1                         *ESPRESSO
490100000629     C                   Z-ADD     $P5(1)        P4CVT1                         *TOTALE
490200000000     C                   Z-ADD     $P1(2)        P4CVN2                         *MESE DUE CORRIERE
490300000629     C                   Z-ADD     $P2(2)        P4CVD2                         *DISTRIBUZIONE
490400000000     C                   Z-ADD     $P3(2)        P4CVE2                         *ESPRESSO
490500000629     C                   Z-ADD     $P5(2)        P4CVT2                         *TOTALE
490600000000     C                   Z-ADD     $P1(3)        P4CVN3                         *MESE TRE CORRIERE
490700000629     C                   Z-ADD     $P2(3)        P4CVD3                         *DISTRIBUZIONE
490800000000     C                   Z-ADD     $P3(3)        P4CVE3                         *ESPRESSO
490900000629     C                   Z-ADD     $P5(3)        P4CVT3                         *TOTALE
491000000000     C                   Z-ADD     $P1(4)        P4CVN4                         *MESE QUATTRO CORRIE
491100000629     C                   Z-ADD     $P2(4)        P4CVD4                         *DISTRIBUZIONE
491200000000     C                   Z-ADD     $P3(4)        P4CVE4                         *ESPRESSO
491300000629     C                   Z-ADD     $P5(4)        P4CVT4                         *TOTALE
491400000000     C                   Z-ADD     $P1(5)        P4CVN5                         *MESE CINQUE CORRIER
491500000629     C                   Z-ADD     $P2(5)        P4CVD5                         *DISTRIBUZIONE
491600000000     C                   Z-ADD     $P3(5)        P4CVE5                         *ESPRESSO
491700000629     C                   Z-ADD     $P5(5)        P4CVT5                         *TOTALE
491800000000     C                   Z-ADD     $P1(6)        P4CVN6                         *MESE SEI CORRIERE
491900000629     C                   Z-ADD     $P2(6)        P4CVD6                         *DISTRIBUZIONE
492000000000     C                   Z-ADD     $P3(6)        P4CVE6                         *ESPRESSO
492100000629     C                   Z-ADD     $P5(6)        P4CVT6                         *TOTALE
492200000000     C                   Z-ADD     $P1(7)        P4CVN7                         *MESE SETTE CORRIERE
492300000629     C                   Z-ADD     $P2(7)        P4CVD7                         *DISTRIBUZIONE
492400000000     C                   Z-ADD     $P3(7)        P4CVE7                         *ESPRESSO
492500000629     C                   Z-ADD     $P5(7)        P4CVT7                         *TOTALE
492600000000     C                   Z-ADD     $P1(8)        P4CVN8                         *MESE OTTO CORRIERE
492700000629     C                   Z-ADD     $P2(8)        P4CVD8                         *DISTRIBUZIONE
492800000000     C                   Z-ADD     $P3(8)        P4CVE8                         *ESPRESSO
492900000629     C                   Z-ADD     $P5(8)        P4CVT8                         *TOTALE
493000000000     C                   Z-ADD     $P1(9)        P4CVN9                         *MESE NOVE CORRIERE
493100000629     C                   Z-ADD     $P2(9)        P4CVD9                         *DISTRIBUZIONE
493200000000     C                   Z-ADD     $P3(9)        P4CVE9                         *ESPRESSO
493300000629     C                   Z-ADD     $P5(9)        P4CVT9                         *TOTALE
493400000000     C                   Z-ADD     $P1(10)       P4CVN0                         *MESE DIECI CORRIERE
493500000629     C                   Z-ADD     $P2(10)       P4CVD0                         *DISTRIBUZIONE
493600000000     C                   Z-ADD     $P3(10)       P4CVE0                         *ESPRESSO
493700000629     C                   Z-ADD     $P5(10)       P4CVT0                         *TOTALE
493800000000     C                   Z-ADD     $P1(11)       P4CVNA                         *MESE UNDICI CORRIER
493900000629     C                   Z-ADD     $P2(11)       P4CVDA                         *DISTRIBUZIONE
494000000000     C                   Z-ADD     $P3(11)       P4CVEA                         *ESPRESSO
494100000629     C                   Z-ADD     $P5(11)       P4CVTA                         *TOTALE
494200000000     C                   Z-ADD     $P1(12)       P4CVNB                         *MESE DODICI CORRIER
494300000629     C                   Z-ADD     $P2(12)       P4CVDB                         *DISTRIBUZIONE
494400000000     C                   Z-ADD     $P3(12)       P4CVEB                         *ESPRESSO
494500000629     C                   Z-ADD     $P5(12)       P4CVTB                         *TOTALE
494600000000     C*
494700000000     C                   Z-ADD     $P113C        P4CVNT                         TOTALE VALORE CORRIE
494800000629     C                   Z-ADD     $P213C        P4CVDT                         *DISTRIBUZIONE
494900000000     C                   Z-ADD     $P313C        P4CVET                         *ESPRESSO
495000000629     C                   Z-ADD     $P513C        P4CVTT                         *TOTALE
495100000000     C*
495200071211     c                   eval      Righe_da_stampare = 1
495300071211     c                   exsr      solo_x_STT
495400010920     C                   WRITE     SE62P4                                       *STAMPA RIGA PERCENT
495500071210      *
495600020823     C                   EXSR      STATES
495700000000     C*
495800000000     C                   ENDSR
495900000000     C*------------------------------------------------------------------------*
496000000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
496100000000     C*------------------------------------------------------------------------*
496200000000     C     DECTES        BEGSR
496300000000     C*
496400000000     C* RECUPERO LA DATA E L'ORA
496500000000     C                   Z-ADD     UDATE         DATA              6 0
496600000000     C                   TIME                    ORA               6 0
496700000000     C*
496800000000     C* DECODIFICA MESI
496900000000     C                   Z-ADD     *ZEROS        J
497000000000IF  1C     MES(1)        IFGT      *ZEROS
497100000000     C                   Z-ADD     MES(1)        J                              *NUMERO MESE MEMORIZ
497200000000     C                   MOVEL     DMM(J)        PIDME1                         *DESC MESE IN STAMPA
497300990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
497400000000     C                   MOVEL     DCA(11)       RSCDE1                         *DESCR PER STAT AFF.
497500000000E   1C                   ENDIF
497600000000     C*
497700000000IF  1C     MES(2)        IFGT      *ZEROS
497800000000     C                   Z-ADD     MES(2)        J                              *NUMERO MESE MEMORIZ
497900000000     C                   MOVEL     DMM(J)        PIDME2                         *DESC MESE IN STAMPA
498000990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
498100000000     C                   MOVEL     DCA(11)       RSCDE2                         *DESCR PER STAT AFF.
498200000000E   1C                   ENDIF
498300000000     C*
498400000000IF  1C     MES(3)        IFGT      *ZEROS
498500000000     C                   Z-ADD     MES(3)        J                              *NUMERO MESE MEMORIZ
498600000000     C                   MOVEL     DMM(J)        PIDME3                         *DESC MESE IN STAMPA
498700990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
498800000000     C                   MOVEL     DCA(11)       RSCDE3                         *DESCR PER STAT AFF.
498900000000E   1C                   ENDIF
499000000000     C*
499100000000IF  1C     MES(4)        IFGT      *ZEROS
499200000000     C                   Z-ADD     MES(4)        J                              *NUMERO MESE MEMORIZ
499300000000     C                   MOVEL     DMM(J)        PIDME4                         *DESC MESE IN STAMPA
499400990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
499500000000     C                   MOVEL     DCA(11)       RSCDE4                         *DESCR PER STAT AFF.
499600000000E   1C                   ENDIF
499700000000     C*
499800000000IF  1C     MES(5)        IFGT      *ZEROS
499900000000     C                   Z-ADD     MES(5)        J                              *NUMERO MESE MEMORIZ
500000000000     C                   MOVEL     DMM(J)        PIDME5                         *DESC MESE IN STAMPA
500100990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
500200000000     C                   MOVEL     DCA(11)       RSCDE5                         *DESCR PER STAT AFF.
500300000000E   1C                   ENDIF
500400000000     C*
500500000000IF  1C     MES(6)        IFGT      *ZEROS
500600000000     C                   Z-ADD     MES(6)        J                              *NUMERO MESE MEMORIZ
500700000000     C                   MOVEL     DMM(J)        PIDME6                         *DESC MESE IN STAMPA
500800990608     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
500900000000     C                   MOVEL     DCA(11)       RSCDE6                         *DESCR PER STAT AFF.
501000000000E   1C                   ENDIF
501100000000     C*
501200000000IF  1C     MES(7)        IFGT      *ZEROS
501300000000     C                   Z-ADD     MES(7)        J                              *NUMERO MESE MEMORIZ
501400000000     C                   MOVEL     DMM(J)        PIDME7                         *DESC MESE IN STAMPA
501500990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
501600000000     C                   MOVEL     DCA(11)       RSCDE7                         *DESCR PER STAT AFF.
501700000000E   1C                   ENDIF
501800000000     C*
501900000000IF  1C     MES(8)        IFGT      *ZEROS
502000000000     C                   Z-ADD     MES(8)        J                              *NUMERO MESE MEMORIZ
502100000000     C                   MOVEL     DMM(J)        PIDME8                         *DESC MESE IN STAMPA
502200990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
502300000000     C                   MOVEL     DCA(11)       RSCDE8                         *DESCR PER STAT AFF.
502400000000E   1C                   ENDIF
502500000000     C*
502600000000IF  1C     MES(9)        IFGT      *ZEROS
502700000000     C                   Z-ADD     MES(9)        J                              *NUMERO MESE MEMORIZ
502800000000     C                   MOVEL     DMM(J)        PIDME9                         *DESC MESE IN STAMPA
502900990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
503000000000     C                   MOVEL     DCA(11)       RSCDE9                         *DESCR PER STAT AFF.
503100000000E   1C                   ENDIF
503200000000     C*
503300000000IF  1C     MES(10)       IFGT      *ZEROS
503400000000     C                   Z-ADD     MES(10)       J                              *NUMERO MESE MEMORIZ
503500000000     C                   MOVEL     DMM(J)        PIDME0                         *DESC MESE IN STAMPA
503600990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
503700000000     C                   MOVEL     DCA(11)       RSCDE0                         *DESCR PER STAT AFF.
503800000000E   1C                   ENDIF
503900000000     C*
504000000000IF  1C     MES(11)       IFGT      *ZEROS
504100000000     C                   Z-ADD     MES(11)       J                              *NUMERO MESE MEMORIZ
504200000000     C                   MOVEL     DMM(J)        PIDMEA                         *DESC MESE IN STAMPA
504300990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
504400000000     C                   MOVEL     DCA(11)       RSCDEA                         *DESCR PER STAT AFF.
504500000000E   1C                   ENDIF
504600000000     C*
504700000000IF  1C     MES(12)       IFGT      *ZEROS
504800000000     C                   Z-ADD     MES(12)       J                              *NUMERO MESE MEMORIZ
504900000000     C                   MOVEL     DMM(J)        PIDMEB                         *DESC MESE IN STAMPA
505000990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
505100000000     C                   MOVEL     DCA(11)       RSCDEB                         *DESCR PER STAT AFF.
505200000000E   1C                   ENDIF
505300000000     C*
505400000000     C                   ENDSR
505500000000     C*------------------------------------------------------------------------*
505600000000     C*?STAPAR - STAMPA PAGINA PARAMETRI DI LANCIO
505700000000     C*------------------------------------------------------------------------*
505800000000     C     STAPAR        BEGSR
505900000000     C*
506000000000     C* PASSAGGIO STAMPA PARAMETRI LANCIO (NON PIU' REIMPOSTATO)
506100000000     C                   MOVEL     '1'           PASPAR
506200000811     C*--------------------
506300000811     C* SCELTA
506400000811     C*--------------------
506500000811IF  1C     D60SCE        IFEQ      '1'
506600000811     C                   MOVEL     DME(38)       PADSCE                         *CLIENTE
506700000811E   1C                   ENDIF
506800000811IF  1C     D60SCE        IFEQ      '2'
506900000811     C                   MOVEL     DME(39)       PADSCE                         * + CLIENTI INSIEME
507000000811E   1C                   ENDIF
507100000811IF  1C     D60SCE        IFEQ      '3'
507200000811     C                   MOVEL     DME(40)       PADSCE                         *CLIENTE UNIFICANTE
507300000811E   1C                   ENDIF
507400020524IF  1C     D60SCE        IFEQ      '4'
507500020524     C                   MOVEL     DME(41)       PADSCE                         *TUTTI I CLIENTI
507600020524E   1C                   ENDIF
507700020524IF  1C     D60SCE        IFEQ      '5'
507800020524     C                   MOVEL     DME(42)       PADSCE                         *TUTTI I CLIENTI X U
507900020524E   1C                   ENDIF
508000000000     C*--------------------
508100000000     C* PERIODO DI ELABORAZIONE
508200000000     C*--------------------
508300000000     C                   Z-ADD     D60DA2        PACDA2                         *ANNO/MESE FINALE
508400000000     C                   Z-ADD     D60DAT        PACDAT                         *ANNO/MESE INIZIALE
508500000000     C*--------------------
508600000000     C* SELEZIONI
508700000000     C*--------------------
508800000000     C                   MOVEL     D60SE1        PACSE1                         *TIPI SERVIZIO
508900000000     C                   MOVEL     D60SE2        PACSE2
509000000000     C                   MOVEL     D60SE3        PACSE3
509100000000     C                   MOVEL     D60SE4        PACSE4
509200000000     C                   MOVEL     D60SE5        PACSE5
509300000000     C                   MOVEL     D60PO1        PACPO1                         *TIPI PORTO
509400000000     C                   MOVEL     D60PO2        PACPO2
509500000000     C                   MOVEL     D60CT1        PACCT1                         *CODICI TARIFFA
509600000000     C                   MOVEL     D60CT2        PACCT2
509700000000     C                   MOVEL     D60CT3        PACCT3
509800000000     C                   MOVEL     D60CT4        PACCT4
509900000000     C                   MOVEL     D60CT5        PACCT5
510000000000     C                   MOVEL     D60CT6        PACCT6
510100000000     C                   MOVEL     D60CT7        PACCT7
510200000000     C                   MOVEL     D60CT8        PACCT8
510300000000     C                   MOVEL     D60CT9        PACCT9
510400000000     C                   MOVEL     D60CT0        PACCT0
510500000000     C                   MOVEL     D60FP1        PACFPA                         *FASCE DI PESO
510600000000     C                   MOVEL     D60FP2        PACFP0
510700000000     C                   MOVEL     D60FP3        PACFP9
510800000000     C                   MOVEL     D60FP4        PACFP8
510900000000     C                   MOVEL     D60FP5        PACFP7
511000000000     C                   MOVEL     D60FP6        PACFP6
511100000000     C                   MOVEL     D60FP7        PACFP5
511200000000     C                   MOVEL     D60FP8        PACFP4
511300000000     C                   MOVEL     D60FP9        PACFP3
511400000000     C                   MOVEL     D60FP0        PACFP2
511500000000     C                   MOVEL     D60FPA        PACFP1
511600000000     C*
511700000000     C* STAMPA PARAMETRI
511800020823IF  1C                   IF        D60OP1 <> 'STT'                              *NO SE PER OTTICO
511900020826     C                   MOVEL     '0'           WSTINT                         *NO INTESTAZIONE
512000020826     C                   EXSR      STATES                                       *TESTATA
512100071210      *
512200071211     c                   eval      Righe_da_stampare = 4
512300071211     c                   exsr      solo_x_STT
512400020826     C                   WRITE     SE62PA                                       *PARAMETRI
512500071210     c*
512600020823E   1C                   ENDIF
512700000000     C*
512800000000     C                   ENDSR
512900000000     C*------------------------------------------------------------------------*
513000000000     C*?STADGC - STAMPA PAGINA CON I DATI GENERALI DEL CLIENTE
513100000000     C*------------------------------------------------------------------------*
513200000000     C     STADGC        BEGSR
513300000000     C*
513400950725     C                   MOVEL     '1'           PASDGC                         *DATI GIA' STAMPATI
513500020826     C*
513600020826     C* STAMPA DATI CLIENTE (PADRE SE RICIESTA STATISTICA PER UNIFICANTE)
513700020826IF  1C     D60SCE        IFEQ      '2'                                          * + CLIENTI INSIEME
513800000821     C                   Z-ADD     1             C
513900020826X   1C                   ELSE                                                   *CLIENTE UNIFICANTE
514000000821     C                   MOVEL     CLI(1)        WKSC
514100000821     C                   EXSR      STADCL                                       *STAMPA DATI CLIENTE
514200000822     C                   Z-ADD     2             C
514300020826E   1C                   ENDIF
514400020826     C*
514500020826     C* STAMPA DATI CLIENTI FIGLI O PIU' CLIENTI INSIEME
514600020826DO  1C     C             DOWLE     500
514700000810     C     CLI(C)        ANDGT     *ZEROS
514800000811     C                   Z-ADD     *ZEROS        IS                5 0          *N.CLIENTI X RIGA
514900050221     C                   CLEAR                   blcC
515000050308     C                   CLEAR                   totC
515100000911     C                   CLEAR                   SEDC
515200000911     C                   CLEAR                   SCLI
515300000810     C                   CLEAR                   SRAG
515400020826DO  2C     C             DOWLE     500
515500020826     C     CLI(C)        ANDGT     *ZEROS
515600000811     C     IS            ANDLT     8
515700000811     C                   ADD       1             IS                             *CLIENTI NELLA RIGA
515800000911     C                   Z-ADD     CLI(C)        WKSC                           *REPERISCE DATI
515900000911     C                   EXSR      REPDCL
516000000911     C                   MOVEL     WEDC          SEDC(IS)
516100050221     C                   MOVEL     wblc          blcC(IS)
516200050308     C                   move      CDati(C)      Wtot                           *Cliente con dati
516300050221     C                   MOVEL     wtot          totC(IS)
516400000810     C                   Z-ADD     WKSC          SCLI(IS)
516500000811     C                   MOVEL     WDKSC         SRAG(IS)
516600000811     C                   ADD       1             C                              *CLIENTI
516700020826E   2C                   ENDDO                                                  *ROTTURA BLOCCO
516800000810     C*
516900000811     C* STAMPA BLOCCO DI 8 CLIENTI
517000010920     C                   CLEAR                   SE62C3
517100020826IF  2C     IS            IFGT      *ZEROS                                       *CLIENTI DA STAMPARE
517200000911     C                   MOVEL     SEDC(1)       CL3ED1
517300050221     c                   eval      %subst(CL3ED1:2:1) = blcc(1)
517400050221     c                   eval      %subst(CL3ED1:3:1) = totc(1)
517500000911     C                   Z-ADD     SCLI(1)       CL3KS1
517600000911     C                   MOVEL     SRAG(1)       CL3RA1
517700000911     C                   MOVEL     SEDC(2)       CL3ED2
517800050221     c                   eval      %subst(CL3ED2:2:1) = blcc(2)
517900050221     c                   eval      %subst(CL3ED2:3:1) = totc(2)
518000000911     C                   Z-ADD     SCLI(2)       CL3KS2
518100000911     C                   MOVEL     SRAG(2)       CL3RA2
518200000911     C                   MOVEL     SEDC(3)       CL3ED3
518300050221     c                   eval      %subst(CL3ED3:2:1) = blcc(3)
518400050221     c                   eval      %subst(CL3ED3:3:1) = totc(3)
518500000911     C                   Z-ADD     SCLI(3)       CL3KS3
518600000911     C                   MOVEL     SRAG(3)       CL3RA3
518700000911     C                   MOVEL     SEDC(4)       CL3ED4
518800050221     c                   eval      %subst(CL3ED4:2:1) = blcc(4)
518900050221     c                   eval      %subst(CL3ED4:3:1) = totc(4)
519000000911     C                   Z-ADD     SCLI(4)       CL3KS4
519100000911     C                   MOVEL     SRAG(4)       CL3RA4
519200000911     C                   MOVEL     SEDC(5)       CL3ED5
519300050221     c                   eval      %subst(CL3ED5:2:1) = blcc(5)
519400050221     c                   eval      %subst(CL3ED5:3:1) = totc(5)
519500000911     C                   Z-ADD     SCLI(5)       CL3KS5
519600000911     C                   MOVEL     SRAG(5)       CL3RA5
519700000911     C                   MOVEL     SEDC(6)       CL3ED6
519800050221     c                   eval      %subst(CL3ED6:2:1) = blcc(6)
519900050221     c                   eval      %subst(CL3ED6:3:1) = totc(6)
520000000911     C                   Z-ADD     SCLI(6)       CL3KS6
520100000911     C                   MOVEL     SRAG(6)       CL3RA6
520200000911     C                   MOVEL     SEDC(7)       CL3ED7
520300050221     c                   eval      %subst(CL3ED7:2:1) = blcc(7)
520400050221     c                   eval      %subst(CL3ED7:3:1) = totc(7)
520500000911     C                   Z-ADD     SCLI(7)       CL3KS7
520600000911     C                   MOVEL     SRAG(7)       CL3RA7
520700000911     C                   MOVEL     SEDC(8)       CL3ED8
520800050221     c                   eval      %subst(CL3ED8:2:1) = blcc(8)
520900050221     c                   eval      %subst(CL3ED8:3:1) = totc(8)
521000000911     C                   Z-ADD     SCLI(8)       CL3KS8
521100000911     C                   MOVEL     SRAG(8)       CL3RA8
521200020826     C                   MOVEL     '0'           WSTINT                         *NO INTESTAZIONE
521300020826     C                   EXSR      STATES
521400071210      *
521500071211     c                   eval      Righe_da_stampare = 2
521600071211     c                   exsr      solo_x_STT
521700010920     C                   WRITE     SE62C3                                       *SCRIVE RIGA
521800071210      *
521900020826E   2C                   ENDIF
522000020826E   1C                   ENDDO
522100950725     C*
522200050308     C     C             ifgt      1
522300050309     c                   eval      cl3leg = '(B) = Cliente Bloccato  -  '+
522400050309     c                             '(*) = Cliente senza dati nel Periodo ' +
522500050309     c                             'selezionato'
522600050308     C                   EXSR      STATES
522700071210      *
522800071211     c                   eval      Righe_da_stampare = 2
522900071211     c                   exsr      solo_x_STT
523000050308     C                   WRITE     SE62L3                                       *SCRIVE Legenda
523100071210      *
523200050308E   2C                   ENDIF
523300050308     C*
523400000000     C                   ENDSR
523500000821     C*------------------------------------------------------------------------*
523600000821     C*?STADCL - STAMPA DATI CLIENTE
523700000821     C*------------------------------------------------------------------------*
523800000821     C     STADCL        BEGSR
523900000821     C*
524000000821     C                   CLEAR                   DSCL
524100020916     C                   Z-ADD     1             DCLKUT
524200000821     C                   Z-ADD     WKSC          DCLKSC
524300020117     C                   CALL      'TISE66R'
524400000821     C                   PARM                    DSCL
524500000821     C                   MOVEL     DCLKSC        CLCKSC
524600000821     C                   MOVEL     DCLCLV        CLCCLV
524700020826     C                   MOVEL(P)  DCLRAG        CLCRAG
524800000821     C                   Z-ADD     DCLITC        CLCITC
524900000821     C                   MOVEL     DCLDIT        CLDITC
525000000821     C                   MOVEL     DCLVIA        CLCVIA
525100000821     C                   Z-ADD     DCLAGE        CLCAGE
525200000821     C                   MOVEL     DCLDAG        CLDAGE
525300000821     C                   MOVEL     DCLCIT        CLCCIT
525400000821     C                   Z-ADD     DCLDPS        CLCDPS
525500000821     C                   MOVEL     DCLCAP        CLCCAP
525600000821     C                   MOVEL     DCLPRV        CLCPRV
525700000821     C                   MOVEL     DCLSTA        CLCSTA
525800000821     C                   MOVEL     DCLCDP        CLCCDP
525900000821     C                   MOVEL     DCLDCD        CLDCDP
526000000821     C                   MOVEL     DCLTEL        CLCTEL
526100000821     C                   MOVEL     DCLSOL        CLCSOL
526200020826     C                   MOVEL     DCLKSC        CL2KSC                         *STAMPA X OTTICO
526300020826     C                   MOVEL(P)  DCLRAG        CL2RAG
526400020823     C                   MOVEL     DEPTFP        CL2TFP
526500020826     C                   MOVEL     '0'           WSTINT                         *NO INTESTAZIONE
526600020826     c                   EXSR      STATES                                       *STAMPA TESTATA
526700071210      *
526800071211     c                   eval      Righe_da_stampare = 3
526900071211     c                   exsr      solo_x_STT
527000020823     C                   WRITE     SE62CL                                       *DATI UN SOLO CLIENT
527100071210      *
527200020823     C                   EXSR      STATES
527300000821     C*
527400000821     C                   ENDSR
527500000810     C*------------------------------------------------------------------------*
527600000911     C*?REPDCL - REPERISCE DATI CLIENTE
527700000810     C*------------------------------------------------------------------------*
527800000911     C     REPDCL        BEGSR
527900000810     C*
528000000810     C* AZZERA VARIABILI DI LAVORO
528100000810     C                   CLEAR                   WDKSC
528200000911     C                   CLEAR                   WEDC
528300000810     C*
528400000810     C* CHIAMA IL PGM DI RICERCA CLIENTE
528500000810     C                   CLEAR                   BS69DS
528600000810     C                   CLEAR                   ACODS
528700000810     C                   MOVEL     KNSIF         I69SIF
528800000810     C                   MOVEL     WKSC          I69KAC
528900000810     C                   CALL      'TIBS69R'
529000000810     C                   PARM                    BS69DS
529100000810     C                   PARM                    ACODS
529200000810     C                   PARM                    INDDS
529300000810     C                   PARM                    CLPDS
529400000810     C                   PARM                    CLSDS
529500000810IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
529600000810     C                   MOVEL     ACORAG        WDKSC
529700050217     C                   MOVEL     ACOabl        KscABL
529800000810E   1C                   ENDIF
529900000911     C*
530000020916     C* se in filiale, CONTROLLA SE I DATI DEL CLIENTE SONO PRESENTI SUL QUEL AS/400
530100050307E   1C********
530200050307E   1C********  Su AZUTE00F il campo UTEAUT non è mai = *blank inoltre la schiera SKI contiene tutte
530300050307E   1C********  sull'AS Unico quindi questo Test non serve + molto
530400050307IF  1C**** UTEAUT        IFEQ      *blanks                                      *IN FILIALE
530500050307     C********           MOVEL     WKSC          WFIL              3 0
530600050307     C********           Z-ADD     1             J
530700050307     C********           SETOFF                                           28
530800050307     C**** WFIL          LOOKUP    SKI(J)                                 28
530900050307IF  2C**** *IN28         IFEQ      *OFF                                         *NON TROVATA
531000050307     C********           MOVEL     '*'           WEDC                           *NO DATI SULL'AS/400
531100050307E   2C********           ENDIF
531200050307E   1C********           ENDIF
531300050307E   1C********
531400050307E   1C**  Utilizzeremo il flag WEDC con (*) per segnalare che il codice cliente, quando il pgm è
531500050307E   1C**  chiamato come UNIFICANTE, non ha contribuito ai dati emessi in stampa statistica.
531600050217     C*
531700050221     C*  Attenzione: se il cliente è bloccato deve essere segnalato con (B)
531800050221     C*              e se non contribuisce alla totalizzazione (non attivato)
531900050221     C                   clear                   wtot              1
532000050221     C                   clear                   wblc              1
532100130315     c                   if        kscABL <> *blank and kscabl <> '*'
532200050307     c                   eval      wblc ='B'
532300050217     c                   end
532400000810     C*
532500000810     C                   ENDSR
532600000000     C*------------------------------------------------------------------------*
532700000000     C*?CARTAB - CARICA TABELLE (FASCE, REGIONI, SERVIZI, ORE CONSEGNE)
532800000000     C*------------------------------------------------------------------------*
532900000000     C     CARTAB        BEGSR
533000000000     C*
533100020916     C                   Z-ADD     1             KTBUTE                         *CODICE UTENTE
533200990217     C*--------------------
533300000000     C* FASCE DI PESO
533400990217     C*--------------------
533500990601IF  1C     D60DA2        IFLE      199812                                       *FASCE FINO AL '98
533600990217     C                   MOVEL     'SDC'         WSX               3
533700020325X   1C                   ELSE                                                   *FASCE DAL '99 AL 20
533800020325     C     D60DA2        IFLE      200112
533900020325     C                   MOVEL     'S9C'         WSX               3
534000020325X   1C                   ELSE                                                   *FASCE DAL 2002
534100020325     C                   MOVEL     'S2C'         WSX
534200020325     C                   ENDIF
534300020325E   1C                   ENDIF
534400000000     C                   MOVEL     '2L'          KTBCOD
534500000000     C     KEYTA2        SETLL     TABEL00F
534600000000     C     KEYTA2        READE     TABEL00F                               99
534700000000     C                   Z-ADD     *ZEROS        J                              *INDICE
534800000000DO  1C     *IN99         DOWEQ     *OFF
534900000000IF  2C     TBLFLG        IFEQ      *BLANKS
535000990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
535100000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
535200990217IF  3C     FILEST        IFEQ      WSX
535300000000     C                   ADD       1             J
535400000000     C                   MOVE      STACOD        FPE(J)                         *CODICE STATISTICA
535500000000     C                   MOVEL     TBLUNI        DS2L
535600000000     C                   MOVEL     §2LDFP        DFPE(J)                        *DECODIFICA FASCIA
535700000000E   3C                   ENDIF
535800000000E   2C                   ENDIF
535900000000     C     KEYTA2        READE     TABEL00F                               99
536000000000E   1C                   ENDDO
536100000000     C***
536200000000     C* PARF = N° FASCE ESISTENTI
536300000000     C***
536400000000     C                   Z-ADD     J             PARF                           *NON MODIFICARE
536500000000     C*
536600000000     C* NESSUNA FASCIA SELEZIONATA
536700000000IF  1C     D60FP1        IFEQ      *BLANKS
536800000000     C     D60FP2        ANDEQ     *BLANKS
536900000000     C     D60FP3        ANDEQ     *BLANKS
537000000000     C     D60FP4        ANDEQ     *BLANKS
537100000000     C     D60FP5        ANDEQ     *BLANKS
537200000000     C     D60FP6        ANDEQ     *BLANKS
537300000000     C     D60FP7        ANDEQ     *BLANKS
537400000000     C     D60FP8        ANDEQ     *BLANKS
537500000000     C     D60FP9        ANDEQ     *BLANKS
537600000000     C     D60FP0        ANDEQ     *BLANKS
537700000000     C     D60FPA        ANDEQ     *BLANKS
537800000000     C                   MOVEL     'T'           SELFPE                         *TUTTE LE FASCE
537900000000     C***
538000000000     C* PARF = N° FASCE SELEZIONATE UGUALE A QUELLE ESISTENTI
538100000000     C***
538200000000     C                   MOVEL     FPE           SFPE                           *FASCE ESIST = SELEZ
538300000000     C                   Z-ADD     PARF          SPARF                          *NON MODIFICARE
538400000000X   1C                   ELSE
538500000000     C* FASCE SELEZIONATE
538600000000     C                   Z-ADD     *ZEROS        J
538700000000IF  2C     D60FP1        IFNE      *BLANKS
538800000000     C                   SETOFF                                           28
538900000000     C     D60FP1        LOOKUP    SFPE(1)                                28
539000000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
539100000000     C                   ADD       1             J
539200000000     C                   MOVEL     D60FP1        SFPE(J)
539300000000E   3C                   ENDIF
539400000000E   2C                   ENDIF
539500000000     C*
539600000000IF  2C     D60FP2        IFNE      *BLANKS
539700000000     C                   SETOFF                                           28
539800000000     C     D60FP2        LOOKUP    SFPE(1)                                28
539900000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
540000000000     C                   ADD       1             J
540100000000     C                   MOVEL     D60FP2        SFPE(J)
540200000000E   3C                   ENDIF
540300000000E   2C                   ENDIF
540400000000     C*
540500000000IF  2C     D60FP3        IFNE      *BLANKS
540600000000     C                   SETOFF                                           28
540700000000     C     D60FP3        LOOKUP    SFPE(1)                                28
540800000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
540900000000     C                   ADD       1             J
541000000000     C                   MOVEL     D60FP3        SFPE(J)
541100000000E   3C                   ENDIF
541200000000E   2C                   ENDIF
541300000000     C*
541400000000IF  2C     D60FP4        IFNE      *BLANKS
541500000000     C                   SETOFF                                           28
541600000000     C     D60FP4        LOOKUP    SFPE(1)                                28
541700000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
541800000000     C                   ADD       1             J
541900000000     C                   MOVEL     D60FP4        SFPE(J)
542000000000E   3C                   ENDIF
542100000000E   2C                   ENDIF
542200000000     C*
542300000000IF  2C     D60FP5        IFNE      *BLANKS
542400000000     C                   SETOFF                                           28
542500000000     C     D60FP5        LOOKUP    SFPE(1)                                28
542600000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
542700000000     C                   ADD       1             J
542800000000     C                   MOVEL     D60FP5        SFPE(J)
542900000000E   3C                   ENDIF
543000000000E   2C                   ENDIF
543100000000     C*
543200000000IF  2C     D60FP6        IFNE      *BLANKS
543300000000     C                   SETOFF                                           28
543400000000     C     D60FP6        LOOKUP    SFPE(1)                                28
543500000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
543600000000     C                   ADD       1             J
543700000000     C                   MOVEL     D60FP6        SFPE(J)
543800000000E   3C                   ENDIF
543900000000E   2C                   ENDIF
544000000000     C*
544100000000IF  2C     D60FP7        IFNE      *BLANKS
544200000000     C                   SETOFF                                           28
544300000000     C     D60FP7        LOOKUP    SFPE(1)                                28
544400000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
544500000000     C                   ADD       1             J
544600000000     C                   MOVEL     D60FP7        SFPE(J)
544700000000E   3C                   ENDIF
544800000000E   2C                   ENDIF
544900000000     C*
545000000000IF  2C     D60FP8        IFNE      *BLANKS
545100000000     C                   SETOFF                                           28
545200000000     C     D60FP8        LOOKUP    SFPE(1)                                28
545300000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
545400000000     C                   ADD       1             J
545500000000     C                   MOVEL     D60FP8        SFPE(J)
545600000000E   3C                   ENDIF
545700000000E   2C                   ENDIF
545800000000     C*
545900000000IF  2C     D60FP9        IFNE      *BLANKS
546000000000     C                   SETOFF                                           28
546100000000     C     D60FP9        LOOKUP    SFPE(1)                                28
546200000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
546300000000     C                   ADD       1             J
546400000000     C                   MOVEL     D60FP9        SFPE(J)
546500000000E   3C                   ENDIF
546600000000E   2C                   ENDIF
546700000000     C*
546800000000IF  2C     D60FP0        IFNE      *BLANKS
546900000000     C                   SETOFF                                           28
547000000000     C     D60FP0        LOOKUP    SFPE(1)                                28
547100000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
547200000000     C                   ADD       1             J
547300000000     C                   MOVEL     D60FP0        SFPE(J)
547400000000E   3C                   ENDIF
547500000000E   2C                   ENDIF
547600000000     C*
547700000000IF  2C     D60FPA        IFNE      *BLANKS
547800000000     C                   SETOFF                                           28
547900000000     C     D60FPA        LOOKUP    SFPE(1)                                28
548000000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
548100000000     C                   ADD       1             J
548200000000     C                   MOVEL     D60FPA        SFPE(J)
548300000000E   3C                   ENDIF
548400000000E   2C                   ENDIF
548500000000     C***
548600000000     C* PARF = N° FASCE SELEZIONATE
548700000000     C***
548800000000     C                   Z-ADD     J             SPARF                          *NON MODIFICARE
548900000000E   1C                   ENDIF
549000000000     C*-------------------------
549100000000     C* REGIONI
549200000000     C*-------------------------
549300000000     C                   Z-ADD     *ZEROS        KORFIL                         *CODICE FILIALE
549400000000     C     KORFIL        SETLL     AZORG01L
549500000911     C                   READ      AZORG01L                               99
549600000000     C                   Z-ADD     *ZEROS        J
549700000000DO  1C     *IN99         DOWEQ     *OFF
549800000000IF  2C     ORGFVA        IFEQ      *BLANKS
549900000000     C     ORGFAG        ANDNE     'V'
550000000000     C                   ADD       1             J
550100000000     C*
550200000000     C* REPERISCO IL RAGGRUPPAMENTO REGIONE DELLA FILIALE
550300000000     C                   MOVEL     '14'          KTBCOD
550400000000     C                   MOVEL     *BLANKS       KTBKEY
550500000000     C                   MOVEL     ORGCRE        KTBKEY
550600000000     C     KEYTAB        CHAIN     TABEL00F                           99
550700000000IF  3C     *IN99         IFEQ      *OFF
550800000000     C     TBLFLG        ANDEQ     *BLANKS
550900000000     C                   MOVEL     TBLUNI        DS14
551000020325     C* VERIFICO QULI RAGGRUPPAMENTI CONSIDERARE IN RELAZIONE ALL'ANNO DEL PERIODO DA ELABORARE
551100020325     C     D60DA2        IFLE      200112
551200020325     C                   MOVEL     §14CRR        DEPCRN            3 0
551300020325     C                   ELSE
551400020325     C                   MOVEL     §14CRN        DEPCRN
551500020325     C                   ENDIF
551600020325     C*
551700020325     C                   MOVEL     DEPCRN        CRR(J)                         *CODICE RAGGRUPAM.TO
551800000000     C*
551900000000     C* DECODIFICA RAGGRUPPAMENTO
552000000000     C                   MOVEL     '2M'          KTBCOD
552100000000     C                   MOVEL     *BLANKS       KTBKEY
552200020325     C                   MOVEL     DEPCRN        KTBKEY
552300000000     C     KEYTAB        CHAIN     TABEL00F                           99
552400000000IF  4C     *IN99         IFEQ      *OFF
552500000000     C     TBLFLG        ANDEQ     *BLANKS
552600000000     C                   MOVEL     TBLUNI        DS2M
552700000000     C                   MOVEL     §2MDES        DCRR(J)                        *DECODIFICA RAGGR.TO
552800000000X   4C                   ELSE
552900000000     C                   MOVEL     *ALL'*'       DCRR(J)
553000000000E   4C                   ENDIF
553100000000X   3C                   ELSE
553200000000     C                   MOVEL     *ALL'9'       CRR(J)
553300000000E   3C                   ENDIF
553400000000E   2C                   ENDIF
553500000911     C                   READ      AZORG01L                               99
553600000000E   1C                   ENDDO
553700950206     C*
553800950206     C* CARICAMENTO TIPI VALORI POSSIBILI DA STAMPARE
553900950206     C                   MOVEL     'C'           VRR(1)                         *COLLI
554000950206     C                   MOVEL     DME(26)       DVRR(1)
554100950206     C                   MOVEL     'P'           VRR(2)                         *PESO
554200950206     C                   MOVEL     DME(27)       DVRR(2)
554300950206     C                   MOVEL     'R'           VRR(3)                         *RICAVI
554400950206     C                   MOVEL     DME(28)       DVRR(3)
554500950206     C                   MOVEL     'S'           VRR(4)                         *SPEDIZIONI
554600950206     C                   MOVEL     DME(29)       DVRR(4)
554700950206     C                   MOVEL     'V'           VRR(5)                         *VOLUME
554800950206     C                   MOVEL     DME(30)       DVRR(5)
554900000000     C*-------------------------
555000000000     C* TIPI SERVIZIO
555100000000     C*-------------------------
555200000000     C* CARICAMENTO SERVIZI ESISTENTI
555300000000     C                   MOVEL     '5E'          KTBCOD
555400000000     C     KEYTA2        SETLL     TABEL00F
555500000000     C     KEYTA2        READE     TABEL00F                               99
555600000000     C*
555700000000     C                   Z-ADD     *ZEROS        J                              *INDICE
555800000000DO  1C     *IN99         DOWEQ     *OFF
555900000000IF  2C     TBLFLG        IFEQ      *BLANKS
556000000000     C                   ADD       1             J
556100000000     C                   MOVEL     TBLKEY        SER(J)                         *CODICE SERVIZIO
556200091013     C                   MOVEL     TBLUNI        DS5e                           *DESCRIZIONE SERVIZI
556300091013     C******             MOVEL     TBLUNI        DSER(J)                        *DESCRIZIONE SERVIZI
556400091013     C                   MOVEL(p)  §5ed08        DSER(J)                        *DESCRIZIONE SERVIZI
556500000000E   2C                   ENDIF
556600000000     C*
556700000000     C     KEYTA2        READE     TABEL00F                               99
556800000000E   1C                   ENDDO
556900000000     C***
557000000000     C* PARS = N° SERVIZI ESISTENTI
557100000000     C***
557200000000     C                   Z-ADD     J             PARS                           *NON MODIFICARE
557300000000     C*
557400000000IF  1C     D60SE1        IFEQ      ' '
557500000000     C     D60SE2        ANDEQ     ' '
557600000000     C     D60SE3        ANDEQ     ' '
557700000000     C     D60SE4        ANDEQ     ' '
557800000000     C     D60SE5        ANDEQ     ' '
557900000000     C                   MOVEL     'T'           SELSER                         *TUTTI I SERVIZI
558000000000     C                   MOVEL     SER           SERS
558100000000     C***
558200000000     C* SPARS = N° SERVIZI SELEZIONATI (TUTTI)
558300000000     C***
558400000000     C                   Z-ADD     J             SPARS                          *NON MODIFICARE
558500000000     C*
558600000000X   1C                   ELSE
558700000000     C*
558800000000     C                   Z-ADD     *ZEROS        J
558900000000IF  2C     D60SE1        IFNE      ' '
559000000000     C                   SETOFF                                           28
559100000000     C     D60SE1        LOOKUP    SERS(1)                                28     *SE GIA' IMMESSO
559200000000IF  3C     *IN28         IFEQ      *OFF                                          *NON TROVATO
559300000000     C                   ADD       1             J
559400000000     C                   MOVEL     D60SE1        SERS(J)
559500000000E   3C                   ENDIF
559600000000E   2C                   ENDIF
559700000000     C*
559800000000IF  2C     D60SE2        IFNE      ' '
559900000000     C                   SETOFF                                           28
560000000000     C     D60SE2        LOOKUP    SERS(1)                                28
560100000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
560200000000     C                   ADD       1             J
560300000000     C                   MOVEL     D60SE2        SERS(J)
560400000000E   3C                   ENDIF
560500000000E   2C                   ENDIF
560600000000     C*
560700000000IF  2C     D60SE3        IFNE      ' '
560800000000     C                   SETOFF                                           28
560900000000     C     D60SE3        LOOKUP    SERS(1)                                28
561000000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
561100000000     C                   ADD       1             J
561200000000     C                   MOVEL     D60SE3        SERS(J)
561300000000E   3C                   ENDIF
561400000000E   2C                   ENDIF
561500000000     C*
561600000000IF  2C     D60SE4        IFNE      ' '
561700000000     C                   SETOFF                                           28
561800000000     C     D60SE4        LOOKUP    SERS(1)                                28
561900000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
562000000000     C                   ADD       1             J
562100000000     C                   MOVEL     D60SE4        SERS(J)
562200000000E   3C                   ENDIF
562300000000E   2C                   ENDIF
562400000000     C***
562500000000     C* SPARS = N° SERVIZI SELEZIONATI
562600000000     C***
562700000000     C                   Z-ADD     J             SPARS                          *NON MODIFICARE
562800000000     C*
562900000000E   1C                   ENDIF
563000000000     C*-------------------------
563100000000     C* ORE DI CONSEGNA
563200000000     C*-------------------------
563300000000     C                   Z-ADD     -000          ORE(1)                         *-000
563400000000     C                   MOVEL     DORA1         DORE(1)                        *IN ANTICIPO
563500000000     C                   Z-ADD     000           ORE(2)                         *000
563600000000     C                   MOVEL     DORA2         DORE(2)                        *IN ORARIO
563700000000     C                   Z-ADD     012           ORE(3)                         *012
563800000000     C                   MOVEL     DORA3         DORE(3)                        *IN RITARDO 12 H
563900000000     C                   Z-ADD     024           ORE(4)                         *024
564000000000     C                   MOVEL     DORA4         DORE(4)                        *IN RITARDO 24 H
564100000000     C                   Z-ADD     036           ORE(5)                         *036
564200000000     C                   MOVEL     DORA5         DORE(5)                        *IN RITARDO 36 H
564300000000     C                   Z-ADD     048           ORE(6)                         *048
564400000000     C                   MOVEL     DORA6         DORE(6)                        *IN RITARDO 48 H
564500000000     C                   Z-ADD     +048          ORE(7)                         *+048
564600000000     C                   MOVEL     DORA7         DORE(7)                        *IN RIT DI OLTRE 48H
564700000000     C***
564800000000     C* PARO  = N° ORE DI CONSEGNA ESISTENTI
564900000000     C***
565000000000     C                   Z-ADD     7             PARO                           *NON MODIFICARE
565100000000     C*-------------------------
565200000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
565300000000     C*-------------------------
565400000000     C                   MOVEL     '2W'          KTBCOD
565500000000     C     KEYTA2        SETLL     TABEL00F
565600000000     C     KEYTA2        READE     TABEL00F                               99
565700000000     C*
565800000000     C                   Z-ADD     *ZEROS        G                 4 0
565900000000DO  1C     *IN99         DOWEQ     *OFF
566000000000IF  2C     TBLFLG        IFEQ      *BLANKS
566100000000     C                   ADD       1             G
566200000000     C                   MOVEL     TBLKEY        FLAB(G)
566300000000     C                   MOVEL     TBLUNI        DS2W
566400000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
566500000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
566600000000E   2C                   ENDIF
566700000000     C*
566800000000     C     KEYTA2        READE     TABEL00F                               99
566900000000E   1C                   ENDDO
567000000000     C*-------------------------
567100000000     C* SELEZIONI PARAMETRI GENERALI
567200000000     C*-------------------------
567300000000IF  1C     D60PO1        IFEQ      ' '
567400000000     C     D60PO2        ANDEQ     ' '
567500000000     C                   MOVEL     'T'           SELPOR                         *TUTTI I PORTI
567600000000E   1C                   ENDIF
567700000000IF  1C     D60PR1        IFEQ      ' '
567800000000     C     D60PR2        ANDEQ     ' '
567900000000     C     D60PR3        ANDEQ     ' '
568000000000     C     D60PR4        ANDEQ     ' '
568100000000     C     D60PR5        ANDEQ     ' '
568200000000     C                   MOVEL     'T'           SELPRO                         *TUTTI I PRODOTTI
568300000000E   1C                   ENDIF
568400000000IF  1C     D60CT1        IFEQ      *BLANKS
568500000000     C     D60CT2        ANDEQ     *BLANKS
568600000000     C     D60CT3        ANDEQ     *BLANKS
568700000000     C     D60CT4        ANDEQ     *BLANKS
568800000000     C     D60CT5        ANDEQ     *BLANKS
568900000000     C     D60CT6        ANDEQ     *BLANKS
569000000000     C     D60CT7        ANDEQ     *BLANKS
569100000000     C     D60CT8        ANDEQ     *BLANKS
569200000000     C     D60CT9        ANDEQ     *BLANKS
569300000000     C     D60CT0        ANDEQ     *BLANKS
569400000000     C                   MOVEL     'T'           SELCTR                         *TUTTI LE TARIFFE
569500000000E   1C                   ENDIF
569600000000     C*
569700070628     C*-------------------------
569800070628      * Tipi Varie da tabella "CA" per visualizzazione  ?
569900070628     C                   MOVEL     'CA'          KTBCOD
570000070628     C                   z-add     9999999       SkOrd
570100070628     C     KEYTA2        SETLL     TABEL00F
570200070628     C     KEYTA2        READE     TABEL00F                               99
570300070628     C*
570400070628     C                   Z-ADD     0             CA                3 0
570500070628     C                   Z-ADD     0             sav_CA            3 0
570600070628DO  1C     *IN99         DOWEQ     *OFF
570700070628IF  2C     TBLFLG        IFEQ      *BLANKS
570800070628     C                   MOVEL     TBLUNI        DSCA
570900070628     C* se abilitato alla visualizzazione su statistica
571000070628     c                   if        §CAstc = 'S'
571100070628     C                   ADD       1             CA
571200070628     C* solo il byte + a destra del codice
571300070628     C                   MOVE      TBLKEY        SkVar(CA)
571400070628      * riga di Ricavi con intestazione da tabella
571500070628      *?    tranne .... che è una varia particolare non da ricavi .  ?
571600070628     c                   if        SkVar(CA) <> '#'
571700070628     C                   MOVE      'RIC.'        SkDsc(CA)
571800070628     c                   end
571900070628      *?    ...quindi attenzione....  ?
572000070628     C                   MOVEL     §CAdsc        SkDsc(CA)
572100070628     C                   eval      SkOrd(CA) = (§CAord * 1000) + CA
572200070628     C                   Z-ADD     CA            sav_CA
572300070628     c                   end
572400070628E   2C                   ENDIF
572500070628     C*
572600070628     C     KEYTA2        READE     TABEL00F                               99
572700070628E   1C                   ENDDO
572800070628     C*
572900070703      *? Un eccezione per la VARIA # che non è una varia di tassazione ma di solo C/E  ?
573000070703      *?  La aggiungo in fondo come ultima da visualizzare a mo' di chiodo poichè non
573100070703      *?  deve essere gestita in tabella.                                 ?
573200070703     C                   ADD       1             CA
573300070703     C* solo il byte + a destra del codice
573400070703     C                   MOVE      '#'           SkVar(CA)
573500070703      *?    .... è una varia particolare non da ricavi .  ?
573600070703     C                   eval      SkDsc(CA) = 'Nr.Lasciati Avvisi     '
573700070703      *?    ... in ordinamento la metto in fondo (888).  ?
573800070703     C                   eval      SkOrd(CA) = (888 * 1000) + CA
573900070703     C                   Z-ADD     CA            sav_CA
574000070703     C*
574100070628      *? Riordina le schiere per SkOrd ascendente dove le prime 3 cifre sono l'ordine  ?
574200070628      *?  espresso sulla tabella "CA" e le restanti 3 sono l'indice delle skiere per   ?
574300070628      *?  agganciare le decodifiche in visualizzazione                                 ?
574400070628     c                   sorta     SkOrd
574500070628     C*-------------------------
574600070628     C*
574700070628     C*
574800000000     C                   ENDSR
574900020905     C*--------------------------------------------------------------------------------------------*
575000020905     C* REPERISCE I DATI UTENTE
575100020905     C*--------------------------------------------------------------------------------------------*
575200020905     C     REPDATIUTE    BEGSR
575300020905     C*
575400020905     C* INIZIALIZZA VARIABILI DI WRK
575500020905     C                   CLEAR                   TIBS34DS
575600020905     C                   CLEAR                   AZUTEDS
575700020905     C                   CLEAR                   DDATIUTE
575800020905     C*
575900020905     C     *DTAARA       DEFINE    §azute        azuteds
576000020905     C     *DTAARA       DEFINE    §datiute      ddatiute
576100020905     C                   IN(E)     *DTAARA
576200020905if  1C                   IF        %Error
576300020905     c                   EVAL      I34Tla = 'L'
576400020905     C                   CALL      'TIBS34R'
576500020905     C                   PARM                    Tibs34Ds
576600020905     C                   IN        *DTAARA
576700020905e   1C                   ENDIF
576800020905     C*
576900020905     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
577000020916     C                   MOVEL(P)  RSUT          DSFIRS
577100020910     C*
577200020910     C* REPERISCO TUTTI I P.O. PRESENTI SUL P.O. DELL'UTENTE
577300020910     C                   CLEAR                   TIBS56DS
577400020910     C                   EVAL      I56PPO = DUTPOU
577500020910     C                   EVAL      I56MOD = 'POA'
577600020910     C                   CALL      'TIBS56R'
577700020910     C                   PARM                    TIBS56DS
577800020905     C*
577900020905     C                   ENDSR
578000000000     C*------------------------------------------------------------------------*
578100000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
578200000000     C*------------------------------------------------------------------------*
578300000000     C     CARMES        BEGSR
578400000000     C*
578500000000     C                   CLEAR                   ANN                            *ANNI
578600000000     C                   CLEAR                   MES                            *MESI
578700000000     C                   Z-ADD     *ZEROS        J                              *NUMERO MESI DA ELAB
578800000000     C*
578900990601     C                   MOVEL     D60DA2        WAA2              4 0          *ANNO FINALE LAVORO
579000000000     C                   MOVE      D60DA2        WMM2              2 0          *MESE FINALE LAVORO
579100990601     C                   MOVEL     D60DAT        WAA               4 0          *ANNO INIZIALE LAVOR
579200000000     C                   MOVE      D60DAT        WMM               2 0          *MESE INIZIALE LAVOR
579300000000     C*
579400000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
579500000000IF  1C     WAA2          IFEQ      WAA
579600000000     C*
579700000000     C* CALCOLO I MESI DI DIFFERENZA
579800000000DO  2C     WMM           DOWLE     WMM2
579900000000     C                   ADD       1             J
580000000000     C                   Z-ADD     WAA           ANN(J)
580100000000     C                   Z-ADD     WMM           MES(J)
580200000000     C                   ADD       1             WMM
580300000000E   2C                   ENDDO
580400000000     C***
580500000000     C* SPARM = N° MESI SELEZIONATI
580600000000     C***
580700000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
580800000000     C*
580900000000     C* IL RESTO DELLA SCHIERA A ZERO
581000000000DO  2C     J             DOWLT     12
581100000000     C                   ADD       1             J
581200000000     C                   Z-ADD     *ZEROS        ANN(J)
581300000000     C                   Z-ADD     *ZEROS        MES(J)
581400000000E   2C                   ENDDO
581500000000     C*
581600000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
581700000000X   1C                   ELSE
581800000000     C*
581900000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
582000000000DO  2C     WMM           DOWLE     12
582100000000     C                   ADD       1             J
582200000000     C                   Z-ADD     WAA           ANN(J)
582300000000     C                   Z-ADD     WMM           MES(J)
582400000000     C                   ADD       1             WMM
582500000000E   2C                   ENDDO
582600000000     C*
582700000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
582800000000     C                   Z-ADD     01            WMM
582900000000DO  2C     WMM           DOWLE     WMM2
583000000000     C                   ADD       1             J
583100000000     C                   Z-ADD     WAA2          ANN(J)
583200000000     C                   Z-ADD     WMM           MES(J)
583300000000     C                   ADD       1             WMM
583400000000E   2C                   ENDDO
583500000000     C***
583600000000     C* SPARM = N° MESI SELEZIONATI
583700000000     C***
583800000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
583900000000     C*
584000000000     C* IL RESTO DELLA SCHIERA A ZERO
584100000000DO  2C     J             DOWLT     12
584200000000     C                   ADD       1             J
584300000000     C                   Z-ADD     *ZEROS        ANN(J)
584400000000     C                   Z-ADD     *ZEROS        MES(J)
584500000000E   2C                   ENDDO
584600000000E   1C                   ENDIF
584700000000     C*
584800000000     C                   ENDSR
584900020603     C*------------------------------------------------------------------------*
585000020604     C*?ORDCRER - ORDINA L'OCCUR "REGIONI-REGIONI"
585100020603     C*------------------------------------------------------------------------*
585200020603     C     ORDCRER       BEGSR
585300020603     C*
585400020603     C                   CLEAR                   SKCRER
585500020603     C*
585600020603     C* PASSO L'OCCUR NELLA SCHIERA
585700020604DO  1C     1             DO        500           YY
585800020604     C     YY            OCCUR     CRER
585900020605     C                   MOVEL     CRER          SKCRER(YY)
586000020605     C***                EVAL      SKCRER(YY) = CRER
586100020603E   1C                   ENDDO
586200020603     C*
586300020603     C* ORDINO LA SCHIERA
586400020603     C                   SORTA     SKCRER
586500020605     C*
586600020605     C* SVUOTO L'OCCUR
586700020605DO  1C     1             DO        500           J
586800020605     C     J             OCCUR     CRER
586900020605     C                   CLEAR                   REX                            *REGIONE-REGIONE
587000020605     C                   CLEAR                   REC                            *COLLI
587100020605     C                   CLEAR                   REP                            *PESO
587200020605     C                   CLEAR                   RER                            *RICAVI
587300020605     C                   CLEAR                   RES                            *SPEDIZIONI
587400020605     C                   CLEAR                   REV                            *VOLUME
587500020605E   1C                   ENDDO
587600020605     C*
587700020605     C                   Z-ADD     *ZEROS        YYY               4 0
587800020603     C*
587900020603     C* PASSO LA SCHIERA NELL'OCCUR
588000020604DO  1C     1             DO        500           YY
588100020605     C                   IF        %subst(SKCRER(YY):1:6) <> *BLANKS
588200020605     C                   ADD       1             YYY
588300020605     C     YYY           OCCUR     CRER
588400020605     C                   EVAL      CRER = SKCRER(YY)
588500020605     C                   ENDIF
588600020603E   1C                   ENDDO
588700020603     C*
588800020603     C                   ENDSR
588900000810     C*------------------------------------------------------------------------*
589000000810     C*?FINSR- OPERAZIONI FINALI
589100000810     C*------------------------------------------------------------------------*
589200000810     C     FINSR         BEGSR
589300000810     C*
589400000810     C* CHIUDE I FILE DEI PGM CHIAMATI
589500000810     C                   CLEAR                   BS69DS
589600000810     C                   CLEAR                   ACODS
589700000810     C                   CLEAR                   INDDS
589800000810     C                   CLEAR                   CLPDS
589900000810     C                   CLEAR                   CLSDS
590000000810     C                   MOVEL     'C'           I69TLA
590100000810     C                   MOVEL     KNSIF         I69SIF
590200000810     C                   CALL      'TIBS69R'
590300000810     C                   PARM                    BS69DS
590400000810     C                   PARM                    ACODS
590500000810     C                   PARM                    INDDS
590600000810     C                   PARM                    CLPDS
590700000810     C                   PARM                    CLSDS
590800000810     C*
590900000810     C                   ENDSR
591000000000     C*------------------------------------------------------------------------*
591100000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
591200000000     C*------------------------------------------------------------------------*
591300000000     C     *INZSR        BEGSR
591400000000     C*
591500000000     C* PARAMETRI IN INPUT AL PGM
591600000000     C     *ENTRY        PLIST
591700000000     C                   PARM                    KPJBA
591800000000     C                   MOVEL     KPJBU         DS60
591900000000     C*
592000000000     C* DEFINIZIONE VARIABILI DELL KLIST
592100000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
592200000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
592300000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
592400990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO00F
592500000000     C     *LIKE         DEFINE    SDOANN        KSOANN
592600990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC00F
592700000000     C     *LIKE         DEFINE    SDCANN        KSCANN
592800000000     C     *LIKE         DEFINE    SDCMES        KSCMES
592900990609     C     *LIKE         DEFINE    SDAKSC        KSAKSC                         *SISDA00F
593000000000     C     *LIKE         DEFINE    SDAANN        KSAANN
593100000000     C     *LIKE         DEFINE    SDAMES        KSAMES
593200990609     C     *LIKE         DEFINE    SDPKSC        KSPKSC                         *SISDP00F
593300000000     C     *LIKE         DEFINE    SDPANN        KSPANN
593400000000     C     *LIKE         DEFINE    SDPMES        KSPMES
593500050118     C     *LIKE         DEFINE    PESKSC        KPEKSC                         *CLIENTE
593600050118     C     *LIKE         DEFINE    PESANN        KPEANN                         *ANNO
593700050118     C     *LIKE         DEFINE    PESMES        KPEMES                         *MESE
593800050118     C     *LIKE         DEFINE    PESMES        $MES                           *MESE
593900050118     C     *LIKE         DEFINE    VLMKSC        KVLKSC                         *CLIENTE
594000050118     C     *LIKE         DEFINE    VLMANN        KVLANN                         *ANNO
594100050118     C     *LIKE         DEFINE    VLMMES        KVLMES                         *MESE
594200040317     C     *LIKE         DEFINE    SDEFILE       KSEFILE                        *SISDO00F
594300040317     C     *LIKE         DEFINE    SDETRD        KSETRD                         *SISDO00F
594400040318     C     *LIKE         DEFINE    sdetrd        wtrd
594500040317     C     *LIKE         DEFINE    SDEKSC        KSEKSC                         *SISDO00F
594600040317     C     *LIKE         DEFINE    SDEANN        KSEANN
594700040317     C     *LIKE         DEFINE    SDEMES        KSEMES
594800000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
594900020823     C     *LIKE         DEFINE    D55TFP        DEPTFP
595000050217     C     *LIKE         DEFINE    ACOabl        KscABL
595100000810     C*
595200000810     C* DEFINIZIONE VARIABILI DI LAVORO
595300000810     C     *LIKE         DEFINE    ACOKSC        WKSC
595400000810     C     *LIKE         DEFINE    ACORAG        WDKSC
595500000911     C                   MOVEL     *BLANKS       WEDC              1
595600000000     C*
595700000000     C* DEFINIZIONE VARIABILI DI DEPOSITO
595800000000     C     *LIKE         DEFINE    SDCKSC        DEPKSC                         *CODICE CLIENTE
595900020523     C     *LIKE         DEFINE    SDCKSC        WRKKSC                         *CODICE CLIENTE DI W
596000020524     C     *LIKE         DEFINE    SDCKSC        PRIKSC                         *1° CODICE CLIENTE
596100000000     C*
596200000000     C* DEFINIZIONE VARIABILI DI PASSAGGIO PER IL CONTROLLO RECORD
596300000000     C                   MOVEL     *BLANKS       $SDX              3            *FILE DA CONTROLLARE
596400000000     C                   MOVEL     *BLANKS       $ROK              1            *RECORD VALIDO O NON
596500990601     C                   Z-ADD     *ZEROS        $DAT              6 0          *DATA (AAAA/MM)
596600000000     C     *LIKE         DEFINE    SDCTSP        $TSP                           *TIPO SERVIZIO
596700000000     C     *LIKE         DEFINE    SDCTPO        $TPO                           *TIPO PORTO
596800000000     C     *LIKE         DEFINE    D60CT1        $CTR                           *TARIFFA (ALFANUMER.
596900000000     C     *LIKE         DEFINE    SDCFPE        $FPE                           *FASCIA DI PESO
597000000000     C     *LIKE         DEFINE    SDCRBL        $RBL                           *RAGGRUPPAMENTO BOLL
597100000000     C     *LIKE         DEFINE    SDCIRR        DELIRR                         RETTIFICHE
597200000000     C     *LIKE         DEFINE    SDCINC        DELINC                         NOTE ACCREDITO
597300020905     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
597400000000     C*
597500000000     C* DEFINIZIONE NUMERO ELEMENTI
597600950725     C                   Z-ADD     *ZEROS        J                 4 0          *INDICE
597700000000     C                   Z-ADD     *ZEROS        JJ                4 0          *INDICE
597800000000     C                   Z-ADD     *ZEROS        JI                4 0          *INDICE
597900020604     C                   Z-ADD     *ZEROS        YY                4 0          *INDICE
598000000000     C                   Z-ADD     *ZEROS        MM                2 0          *INDICE MESE
598100020520     C                   Z-ADD     *ZEROS        R                 3 0          *INDICE COMB REGIONI
598200020520     C                   Z-ADD     *ZEROS        COMBIN            3 0          *COMBINAZIONI RAGGRU
598300000000     C                   Z-ADD     *ZEROS        I                 3 0          *DESCRIZ RAGGRUPP.TO
598400000000     C                   Z-ADD     *ZEROS        X                 2 0          *MESE DA STAMPARE
598500000000     C                   Z-ADD     *ZEROS        P                 4 0          *INDICE PARTICOLARIT
598600950725     C                   Z-ADD     *ZEROS        C                 4 0          *CLIENTI DA ELABORAR
598700050118     C                   Z-ADD     *ZEROS        nrPEV             4 0          *INDICE
598800050118      *
598900960604     C                   MOVEL     '1'           *IN01                          *OVERFLOW
599000000000     C     *LIKE         DEFINE    J             SPARM                          *N° MESI SELEZIONATI
599100000000     C     *LIKE         DEFINE    J             SPARS                          *N° SERVIZI SELEZ.TI
599200000000     C     *LIKE         DEFINE    J             SPARF                          *N° FASCE SELEZIONAT
599300000000     C     *LIKE         DEFINE    J             PARS                           *N° SERVIZI ESISTENT
599400000000     C     *LIKE         DEFINE    J             PARO                           *N° ORE ESISTENTI
599500000000     C     *LIKE         DEFINE    J             PARF                           *N° FASCE ESISTENTI
599600000000     C*
599700000000     C* DEFINIZIONE VALIDITA' RECORD
599800000000     C                   MOVEL     'N'           WRECOK            1            RECORD NO VALIDO
599900000000     C                   MOVEL     'N'           ROKREG            1            REC NON VALIDO STAT
600000050118     C                   MOVEL     'N'           ROKPEV            1            REC NON VALIDO STAT
600100000000     C                   MOVEL     'N'           ROKSER            1            REC NON VALIDO STAT
600200000000     C                   MOVEL     'N'           ROKAFF            1            REC NON VALIDO STAT
600300000000     C                   MOVEL     'N'           ROKDEL            1            REC NON VALIDO STAT
600400000000     C                   MOVEL     'N'           ROKAMM            1            REC NON VALIDO STAT
600500000000     C*
600600000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
600700000000     C     KEYTAB        KLIST
600800000000     C                   KFLD                    KTBUTE                         *UTENTE
600900000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
601000000000     C                   KFLD                    KTBKEY                         *RECORD TABELLA
601100000000     C*
601200000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
601300000000     C     KEYTA2        KLIST
601400000000     C                   KFLD                    KTBUTE
601500000000     C                   KFLD                    KTBCOD
601600000000     C*
601700990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
601800000000     C     KEYSDO        KLIST
601900000000     C                   KFLD                    KSOKSC                         *CLIENTE
602000000000     C                   KFLD                    KSOANN                         *ANNO
602100000000     C*
602200990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
602300000000     C     KEYSDC        KLIST
602400000000     C                   KFLD                    KSCKSC                         *CLIENTE
602500000000     C                   KFLD                    KSCANN                         *ANNO
602600000000     C                   KFLD                    KSCMES                         *MESE
602700050118     C*
602800050118     C* CHIAVE LETTURA SIPES01L - PARZIALE
602900050118     C     KEYPES        KLIST
603000050118     C                   KFLD                    KPEKSC                         *CLIENTE
603100050118     C                   KFLD                    KPEANN                         *ANNO
603200050118     C                   KFLD                    KPEMES                         *MESE
603300050118     C*
603400050118     C* CHIAVE LETTURA SIVLM01L - PARZIALE
603500050118     C     KEYVLM        KLIST
603600050118     C                   KFLD                    KVLKSC                         *CLIENTE
603700050118     C                   KFLD                    KVLANN                         *ANNO
603800050118     C                   KFLD                    KVLMES                         *MESE
603900040317     C*
604000040317     C* CHIAVE LETTURA SISDE02L - TOTALE
604100040317     C     KEYSDE        KLIST
604200040317     C                   KFLD                    KSEFILE                        *DEPOSITO CLIENTE
604300040317     C                   KFLD                    KSETRD                         *DEPOSITO CLIENTE
604400040317     C                   KFLD                    KSEKSC                         *CLIENTE
604500040317     C                   KFLD                    KSEANN                         *ANNO
604600040317     C                   KFLD                    KSEMES                         *MESE
604700040317     C*
604800040317     C* CHIAVE LETTURA SISDE02L - PARZIALE
604900040317     C     KEYSDE1       KLIST
605000040317     C                   KFLD                    KSEFILE                        *DEPOSITO CLIENTE
605100040317     C                   KFLD                    KSETRD                         *DEPOSITO CLIENTE
605200040317     C                   KFLD                    KSEKSC                         *CLIENTE
605300000000     C*
605400990609     C* CHIAVE LETTURA SISDA01L - PARZIALE
605500000000     C     KEYSDA        KLIST
605600000000     C                   KFLD                    KSAKSC                         *CLIENTE
605700000000     C                   KFLD                    KSAANN                         *ANNO
605800000000     C                   KFLD                    KSAMES                         *MESE
605900000000     C*
606000990609     C* CHIAVE LETTURA SISDP01L - PARZIALE
606100000000     C     KEYSDP        KLIST
606200000000     C                   KFLD                    KSPKSC                         *CLIENTE
606300000000     C                   KFLD                    KSPANN                         *ANNO
606400000000     C                   KFLD                    KSPMES                         *MESE
606500000000     C*
606600000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
606700000000     C     KEYORG        KLIST
606800000000     C                   KFLD                    KORFIL                         *FILIALE
606900020523     C*
607000020523     C* CHIAVE LETTURA CNCLP00F - COMPLETA
607100020523     C     KEYCLP        KLIST
607200020523     C                   KFLD                    CLPKUT
607300020523     C                   KFLD                    CLPKCC
607400020523     C                   KFLD                    CLPKSC                         *MESE
607500000000     C*
607600000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
607700990609     C                   MOVEL     '0'           $FINO             1            *FINE CICLO SISDO
607800020524     C                   MOVEL     '0'           $FINREC           1            *FINE CICLO SISDO
607900990609     C                   MOVEL     '0'           $FINC             1            *FINE CICLO SISDC
608000040317     C                   MOVEL     '0'           $FINCE            1            *FINE CICLO SISDC
608100990609     C                   MOVEL     '0'           $FINA             1            *FINE CICLO SISDA
608200990609     C                   MOVEL     '0'           $FINP             1            *FINE CICLO SISDP
608300050118     C                   MOVEL     '0'           $FINPE            1            *FINE CICLO SIPES
608400050118     C                   MOVEL     '0'           $FINVL            1            *FINE CICLO SIVLM
608500000000     C*
608600000000     C* IMPOSTO A ZERO I PASSAGGI DI MEMORIZZAZIONE
608700000000     C                   MOVEL     '0'           PASKSC            1            SALDI CLIENTE
608800050118     C                   MOVEL     '0'           PASPEV            1            SALDI PESI-VOLUMI
608900000000     C                   MOVEL     '0'           PASREG            1            SALDI REG-REG
609000000000     C                   MOVEL     '0'           PASSER            1            SALDI SERVIZIO BOLLE
609100000000     C                   MOVEL     '0'           PASAFF            1            SALDI AFFIDABILITA'
609200000000     C                   MOVEL     '0'           PASDEL            1            SALDI DELTA CLIENTE
609300000000     C                   MOVEL     '0'           PASAMM            1            SALDI AMMINISTRAZION
609400050308      *  Cliente con dati
609500050308     C                   MOVEL     '0'           Con_Dati          1            Se contribuisce ai d
609600000000     C*
609700000000     C* IMPOSTO A ZERO I PASSAGGI DI MEMORIZZAZIONE
609800000000     C                   MOVEL     '0'           PASDGC            1            *STAMPA DATI GENER C
609900000000     C                   MOVEL     '0'           PASPAR            1            *STAMPA I PARAMENTRI
610000000000     C*
610100000000     C* IMPOSTO A *BLANKS I CAMPI DI SELEZIONE
610200000000     C                   MOVEL     '0'           SELSER            1            SELEZ SERVIZI (' ',T
610300000000     C                   MOVEL     '0'           SELPOR            1            SELEZ PORTO   (' ',T
610400000000     C                   MOVEL     '0'           SELPRO            1            SELEZ PRODOTTI(' ',T
610500000000     C                   MOVEL     '0'           SELCTR            1            SELEZ TARIFFE (' ',T
610600000000     C                   MOVEL     '0'           SELFPE            1            SELEZ FASCE   (' ',T
610700000000     C                   MOVEL     '0'           SOLAFF            1            SOLO STAT. "AFFID.TA
610800000000     C                   MOVEL     '0'           MAIAFF            1            MAI  STAT. "AFFID.TA
610900020823     C                   MOVEL     '0'           WSTINT            1            INTESTAZIONE DA STAM
611000020823     C                   Z-ADD     *ZEROS        DEPPAG            4 0          PAGINE PER CLIENTE -
611100000808     C*
611200000808     C* IMNPOSTA DATA / ORA CORRENTE
611300000808     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
611400000808     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
611500000808     C                   Z-ADD     *ZEROS        G08INV
611600000808     C                   MOVEL     '0'           G08ERR
611700000808     C                   CALL      'XSRDA8'
611800000808     C                   PARM                    WLBDA8
611900000808     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
612000000000     C*
612100000000     C* SUDDIVISIONE DATE VIDEO IN ANNI E MESI
612200000000     C                   Z-ADD     D60DA2        DATFIN                         *DS DATA FINALE
612300000000     C                   Z-ADD     D60DAT        DATINI                         *DS DATA INIZIALE
612400000000     C*
612500000000     C* CONTROLLO SE E' STATA SCELTA SOLO LA STATISTICA "AFFIDABILITA" PER
612600000000     C* SALTARE IL CICLO DI LETTURA CHE A QUESTA STATISTICA NON INTERESSA
612700000000IF  1C     D60ST1        IFEQ      'N'                                          *NO "REGIONE-REGIONE
612800050118     C     D60STp        ANDEQ     'N'                                          *NO "PESI/VOLUMI"
612900000000     C     D60ST2        ANDEQ     'N'                                          *NO "SERVIZIO BOLLE"
613000000000     C     D60ST3        ANDEQ     'S'                                          *SI "AFFIDABILITA'"
613100000000     C     D60ST4        ANDEQ     'N'                                          *NO "DELTA CLIENTE"
613200000000     C     D60ST5        ANDEQ     'N'                                          *NO "AMMINISTRAZIONE
613300000000     C                   MOVEL     '1'           SOLAFF                         *SOLO STAT "AFFID.TA
613400000000E   1C                   ENDIF
613500000000     C*
613600000000     C* CONTROLLO SE NON E' MAI STATA SCELTA LA STATISTICA "AFFIDABILITA" PER
613700000000     C* SALTARE IL CICLO DI LETTURA CHE ALLE ALTRE STATISTICHE NON INTERESSA
613800000000IF  1C     D60ST3        IFEQ      'N'
613900000000     C                   MOVEL     '1'           MAIAFF                         *MAI LA "AFFID.TA'"
614000000000E   1C                   ENDIF
614100020916     C*
614200020916     C* REPERISCE I DATI UTENTE
614300020916     C                   EXSR      REPDATIUTE
614400000000     C*
614500000000     C* CARICA TABELLE (FASCE, REGIONI, TIPI SERVIZIO)
614600000000     C                   EXSR      CARTAB
614700000000     C*
614800000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
614900000000     C                   EXSR      CARMES
615000000000     C*
615100000000     C* DECODIFICA INTESTAZIONE
615200000000     C                   EXSR      DECTES
615300000000     C*
615400000000     C                   ENDSR
615500071211      *------------------------------------------------------------------------*
615600071211      *  Controlla se deve impostare la pagina successiva con Intestazione
615700071211      *------------------------------------------------------------------------*
615800071211     C     solo_x_STT    Begsr
615900071211      *
616000071211      *  Solo x Ottico
616100071211      *    deve controllare il salto pagina che sia corretto e stampare il
616200071211      *    numero pagina ad ogni salto pag. per poter archiviare
616300071211      *    correttamente la stampa su Ottico.
616400071211IF  1C                   IF        D60OP1 <> 'STT'                              *NO SE PER OTTICO
616500071211E   1C                   LeaveSR
616600071211E   1C                   ENDIF
616700071211      *
616800071211      * quindi controlla a che punto della pagina si trovi.... ossia se stampando
616900071211      *  il nuovo record supero l'Overflow ..... in Tal caso emette il salto
617000071211      *  pagina con intestazione e nr.Pagina -> essenziale per l'archiviazione su Ottico
617100071211     c                   if        Riga_Corrente + Righe_da_stampare >
617200071211     c                                    Riga_di_Overflow
617300071211     c                   eval      *in01 = *On
617400071211     c                   exsr      STATES
617500071211     c                   endIF
617600071211      *
617700071211      *
617800071211     C                   ENDSR
617900071211     O*------------------------------------------------------------------------*
618000950206**   DMM - DECODIFICHE MESI DELL'ANNO
618100000000GENNAIO                        1
618200000000FEBBRAIO                       2
618300000000MARZO                          3
618400000000APRILE                         4
618500000000MAGGIO                         5
618600000000GIUGNO                         6
618700000000LUGLIO                         7
618800000000AGOSTO                         8
618900000000SETTEMBRE                      9
619000000000OTTOBRE                        10
619100000000NOVEMBRE                       11
619200000000DICEMBRE                       12
619300000000**   DRI - DECODIFICHE RIGA DI STAMPA
619400000000   REGIONE - REGIONE   1
619500000000      T O T A L I      2
619600000000     AFFIDABILITA'     3
619700000000       D E L T A       4
619800000000    AMMINISTRAZIONE    5
619900050121 PESI/VOLUMI    V D L  6
620000000000**   DCA - DECODIFICHE CAMPI DA STAMPARE
620100000000SPEDIZIONI 1
620200000000COLLI      2
620300000000PESO (KG)  3
620400000000VOLUME     4
620500000000RICAVO     5
620600000000TOTALE     6
620700000000RICAVI     7
620800000000SPEDIZ     8
620900000000DELTA      9
621000000000TOTALI     10
621100091214C E+H D  T 11
62120000000012
62130000000013
62140000000014
62150000000015
621600000000**   DME - DECODIFICHE RIGHE IN STAMPA
621700000000MEDIA COLLI    X SPED  1
621800000000      KG       X SPED  2
621900000000      M.CUBI   X SPED  3
622000000000MEDIA KG       X COLLO 4
622100000000      M.CUBI   X COLLO 5
622200000000MEDIA INCASSO  X SPED  6
622300000000               X COLLO 7
622400000000               X Q.LE  8
622500000000               X M.CUBO9
622600000000FATTURATO TOTALE       10
622700000000NOTE DI ACCREDITO      11
622800000000RETTIFICHE FATTURAZIONE12
622900000000% SUL FATTURATO        13
623000000000GIACENZE           RIC.14
623100000000                   SPED15
623200000000DI CUI:                16
623300000000CONS AI PIANI      RIC.17
623400000000CONS APPUNTAMENTO  RIC.18
623500000000CONS SUPERMERCATI  RIC.19
623600000000CONS FUORI MISURA  RIC.20
623700000000ASSICURAZIONI      RIC.21
623800000000CONTRASSEGNI       RIC.22
623900010920CONS CENTRO STOR.  RIC.23
624000010920CONS LOC.DISAGIATE RIC.24
624100010920CONS DDT           RIC.25
624200950206C O L L I -------------26
624300950206P E S O ---------------27
624400950206R I C A V I -----------28
624500950206S P E D I Z I O N I ---29
624600950206V O L U M E -----------30
624700010920RITIRI             RIC.31
624800010920CONS ISOLE         RIC.32
624900021028CONS DISAGIATA     RIC.33
625000021028RITIRI ANNULLATI   RIC.34
625100021028RICERCA D'ARCHIVIO RIC.35
625200021028DIRITTO PESATURA   RIC.36
625300051018NR.LASCIATI AVVISI     37
625400000811CLIENTE                38
625500000811PIU' CLIENTI INSIEME   39
625600000811CLIENTE UNIFICANTE     40
625700020823TUTTI CLIENTI          41
625800020823TUTTI CLIENTI X UNIFIC 42
625900040318COLLI ORIG.BANCALI "O" 43
626000040318BANCALI TIPO "O"       44
626100040318BANCALI TIPO "B"       45
626200040924P.O.D. IMMAGE      RIC.46
626300051018ANN. P.ASSEGNATO   RIC.47
626400051018DIROTTAMENTI       RIC.48
626500051018RITIRO BANCALI     RIC.49
626600051018REC.C/ASS          RIC.50
626700051018ORM COMMISSIONATO  RIC.51
626800050118NR.COLLI VDL           52
626900050118NR.COLLI Boll.         53
627000050121% COLLI RILEV.         54
627100050121% SCOST RIL./BOLL.     55
627200050119   PESO VDL            56
627300050119   PESO BOLL.          57
627400050119   VOLUME VDL          58
627500050119   VOLUME BOLL.        59
627600050121------------- PESI VDL 60
627700050121----------- VOLUMI VDL 61
627800051018L.AVV.ADDEBITATI   RIC.62
