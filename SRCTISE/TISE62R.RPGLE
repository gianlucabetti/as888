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
005700170801
005800170801     d $ClienteStampato...
005900170801     d                 s               n   inz
006000170801
006100071210     D*------------------------------------------------------------------------*
006200000000     D* SCHIERE
006300000000     D*------------------------------------------------------------------------*
006400000000     D*-------------------
006500000000     D* TABELLE CAPICONTO
006600000000     D*-------------------
006700000000     D*-------------------
006800000000     D* DECODIFICA MESI DELL'ANNO
006900000000     D*-------------------
007000000000     D DMM             S              9    DIM(12) CTDATA PERRCD(1)
007100000000     D*-------------------
007200000000     D* DECODIFICA RIGHE DI STAMPA PER DESCRIZIONE STATISTICA
007300000000     D*-------------------
007400050121     D DRI             S             23    DIM(6) CTDATA PERRCD(1)
007500000000     D*-------------------
007600000000     D* DECODIFICA RIGHE DI STAMPA PER DESCRIZIONE CAMPI
007700000000     D*-------------------
007800950206     D DCA             S             11    DIM(15) CTDATA PERRCD(1)
007900000000     D*-------------------
008000000000     D* DECODIFICHE RIGHE IN STAMPA
008100000000     D*-------------------
008200051017     D DME             S             23    DIM(62) CTDATA PERRCD(1)
008300000000     D*-------------------
008400000000     D* FASCE DI PESO
008500000000     D*-------------------
008600000000     D FPE             S              2    DIM(11)                              *CODICI FASCE ESISTE
008700000000     D DFPE            S             14    DIM(11)                              *DECODIFICA FASCE
008800000000     D SFPE            S              2    DIM(11)                              *FASCE SELEZIONATE
008900000000     D*-------------------
009000000000     D* FILIALI/RAGGRUPPAMENTI REGIONE E DECODIFICHE
009100000000     D*-------------------
009200971124     D CRR             S              3  0 DIM(500)                             *RAGGRUPPAMENTO REGI
009300971124     D DCRR            S             25    DIM(500)                             *DECODIFICA RAGGRUPP
009400000000     D*-------------------
009500000000     D* COMBINAZIONE RAGGRUPPAMENTI PARTENZA/ARRIVO
009600000000     D*-------------------
009700020520     D REG             S              7    DIM(500)                             *P/A + CODICE + CODI
009800020520     D RE2             S              6    DIM(500)                             *CODICE + CODICE
009900950206     D*-------------------
010000950206     D* TIPI VALORI IN STAMPA PER REGIONE-REGIONE
010100950206     D*-------------------
010200950206     D VRR             S              1    DIM(10)                              *TIPI VALORE
010300950206     D DVRR            S             23    DIM(10)                              *DESCRIZIONE VALORI
010400000000     D*-------------------
010500000000     D* TIPI SERVIZIO
010600000000     D*-------------------
010700000000     D SER             S              1    DIM(10)                              *TIPI SERVIZIO
010800000000     D DSER            S             20    DIM(10)                              *DECODIFA SERVIZI
010900000000     D SERS            S              1    DIM(10)                              *SERVIZI SELEZIONATI
011000000000     D*-------------------
011100000000     D* ORE DI CONSEGNA
011200000000     D*-------------------
011300000000     D ORE             S              3  0 DIM(7)                               *ORE DI CONSEGNA
011400000000     D DORE            S             20    DIM(7)                               *DECODIFICA ORE
011500000000     D*------------------
011600000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
011700000000     D*------------------
011800000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
011900000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
012000000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
012100000000     D*-------------------
012200000000     D* VARIE
012300000000     D*-------------------
012400050118     D VAR             S              1    DIM(50)                              *SIGLA VARIA
012500000000     D*-------------------
012600000000     D* MESI DA ELABORARE
012700000000     D*-------------------
012800000000     D MES             S              2  0 DIM(12)
012900000000     D*-------------------
013000000000     D* ANNI DEI MESI DA ELABORARE
013100000000     D*-------------------
013200990601     D ANN             S              4  0 DIM(12)
013300950725     D*-------------------
013400950725     D* DS SCOMPATTA CLIENTI
013500950725     D*-------------------
013600050117      ** i codici clienti da passare nella DS sono diventati 28 da 30 che erano
013700050117      ** x permettere passaggio di altri parametri poichè la DS era già di 256
013800050117     D*KSA*****        S              4    DIM(30)                              *ALFANUMERICO
013900050117     D KSA             S              4    DIM(28)                              *ALFANUMERICO
014000000808     D*-------------------
014100000808     D* DS CLIENTI FIGLI
014200000808     D*-------------------
014300000808     D SKSC8           S              8  0 DIM(500)                             *clienti
014400000807     D*-------------------
014500000807     D* CLIENTI DA ELABORARE
014600000807     D*-------------------
014700000816     D CLI             S              7  0 DIM(500)
014800000816     D OCLI            S              7  0 DIM(500)                             *DA ORDINARE
014900050308     D*-------------------
015000050308     D* CLIENTI con Dati
015100050308     D*-------------------
015200050308     D CDati           S              1    DIM(500)
015300000810     D*-------------------
015400000810     D* DESCRIZIONE CLIENTI DA STAMPARE
015500000810     D*-------------------
015600050221     D blcC            S              1    DIM(8)                               *cliente Bloccato
015700050221     D totC            S              1    DIM(8)                               *totalizza DATI
015800000911     D SEDC            S              1    DIM(8)                               *ESISTENZA DATI
015900000911     D SCLI            S              7  0 DIM(8)                               *CLIENTE
016000000811     D SRAG            S             15    DIM(8)                               *RAGIONE SOCIALE
016100000810     D*------------------
016200000821     D* DS REPERIMENTO CLIENTE
016300000810     D*-------------------
016400000810     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
016500000810     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
016600000810     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
016700000810     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
016800000810     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
016900020604     D*-------------------
017000020604     D* SCHIERA X ORDINAMENTO OCCUR "REGIONE-REGIONE"
017100020604     D*-------------------
017200020604     D SKCRER          S                   DIM(500) LIKE(CRER)
017300000000    ?D*-------------------
017400000000    ?D* S C H I E R E   M U L T I P L E   D I   M E M O R I Z Z A Z I O N E
017500000000    ?D*-------------------
017600050118     D*-------------------
017700050118     D* STATISTICA PESI/VOLUMI                           - ???????? -
017800050118     D*-------------------
017900000000     D*-------------------
018000020520     D* STATISTICA REGIONE-REGIONE                       - 500 RAGGRUPPAMENTI -
018100000000     D*-------------------
018200000000     D*-------------------
018300000000     D* STATISTICA SERVIZIO BOLLE (S,C,P,V,R)            - 5 VALORI -
018400000000     D*-------------------
018500000000     D*-------------------
018600000000     D* STATISTICA AFFIDABILITA' SPEDIZIONI              - 7 ORARI -
018700000000     D*-------------------
018800000000     D*-------------------
018900000000     D* STATISTICA DELTA PER CLIENTE                     - 11 FASCE -
019000000000     D*-------------------
019100000000     D*-------------------
019200050118     D* SCHIERE STATISTICA AMMINISTRAZIONE               - 50 VARIE -
019300000000     D*-------------------
019400000000     D*---
019500000000     D* SCHIERE NON MULTIPLE
019600000000     D*---
019700010920     D AFC             S             18  3 DIM(12)                              FATTURATO
019800010920     D ANC             S             18  3 DIM(12)                              NOTE DI CREDITO
019900010920     D ARC             S             18  3 DIM(12)                              RETTIFICHE DI FATTUR
020000000000    ?D*-------------------
020100000000    ?D* S C H I E R E   D I   L A V O R O
020200000000    ?D*-------------------
020300000000     D*-------------------
020400000000     D* MULTIPLA PER LA "SERVIZIO BOLLE"                 - 5 VALORI -
020500000000     D*-------------------
020600970724     D*---
020700000000     D* PER MEDIE PER LA "SERVIZIO BOLLE"
020800970724     D*---
020900000000     D SPW             S             16  3 DIM(12)                              SPEDIZIONI
021000000000     D COW             S             16  3 DIM(12)                              COLLI
021100000000     D PEW             S             16  3 DIM(12)                              PESO
021200000000     D VOW             S             16  3 DIM(12)                              VOLUME
021300010920     D RIW             S             18  3 DIM(12)                              RICAVI
021400000000     D MEW             S             16  3 DIM(12)                              MEDIE CALCOLATE
021500000000     D*-------------------
021600000000     D* TOTALI PER LA "AFFIDABILITA'"
021700000000     D*-------------------
021800000000     D PNW             S             13  0 DIM(12)                              SPED CORRIERE DELL'O
021900000629     D PDW             S             13  0 DIM(12)                              SPED DISTRIB DELL'OR
022000000000     D PSW             S             13  0 DIM(12)                              SPED ESPRESSO DELL'O
022100000629     D PPW             S             13  0 DIM(12)                              SPED POSTE DELL'ORA
022200000000     D PTW             S             13  0 DIM(12)                              SPEDIZIONI TOTALI
022300000000     D*-------------------
022400000000     D* TOTALI A FONDO PAGINA PER LA "DELTA CLIENTE"
022500000000     D*-------------------
022600010920     D DRW             S             18  3 DIM(12)                              RICAVI REALI
022700010920     D DPW             S             18  3 DIM(12)                              RICAVI PRESUNTI
022800000000     D DSW             S             13  0 DIM(12)                              SPEDIZIONI
022900000000     D DDW             S              4  1 DIM(12)                              DELTA PERCENTUALE
023000970724     D*-------------------
023100970724     D* PERCENTUALI FATTURATO PER LA "AMMINISTRAZIONE"
023200000000     D*-------------------
023300000000     D APW             S              4  1 DIM(12)                              PERCEN SUL FATTURATO
023400000000     D*-------------------
023500000000     D* DI STAMPA
023600000000     D*-------------------
023700010920     D $VA             S             18  3 DIM(12)                              VALORE GENERICO
023800010920     D $V2             S             18  3 DIM(12)                              VALORE GENERICO 2
023900000000     D $P1             S              2  0 DIM(12)                              INCIDENZA SPED CORRI
024000000000     D $P2             S              2  0 DIM(12)                              INCIDENZA SPED DISTR
024100000000     D $P3             S              2  0 DIM(12)                              INCIDENZA SPED ESPRE
024200000629     D $P4             S              2  0 DIM(12)                              INCIDENZA SPED POSTE
024300000629     D $P5             S              2  0 DIM(12)                              INCIDENZA SPED TOT O
024400020823     D*------------------
024500020823     D* DS REPEIMENTO TERMINALs
024600020823     D*------------------
024700020823     D FNLV55DS      E DS
024800000808     D*------------------
024900000808     D* DS CONTROLLO DATA (8)
025000000808     D*------------------
025100000808     D WLBDA8          DS                  INZ
025200000808     D  G08DAT                 1      8  0
025300000808     D  G08INV                 9     16  0
025400000808     D  G08ERR                17     17
025500000808     D  G08TGI                18     22  0
025600000000     D*------------------
025700000000     D* ARCHITETTURA
025800000000     D*------------------
025900000000     D KPJBA         E DS
026000000000     D  PROFIL                 1     10                                         UTENTE
026100000000     D*------------------
026200000000     D* DS DI PROCEDURA
026300000000     D*------------------
026400010920     D DS60          E DS                  EXTNAME(TISE60DS)
026500000000     D*------------------
026600020117     D* DS LANCIO PROGRAMMA REPERIMENTO DATI CLIENTE -TISE66R-
026700000000     D*------------------
026800020117     D DSCL          E DS                  EXTNAME(TISE66DS)
026900000000     D*------------------
027000000000     D* DS FASCE DI PESO
027100000000     D*------------------
027200000000     D DS2L          E DS
027300000000     D*------------------
027400000000     D* DS REGIONI
027500000000     D*------------------
027600000000     D DS14          E DS
027700000000     D*------------------
027800000000     D* DS RAGGRUPPAMENTI REGIONI
027900000000     D*------------------
028000000000     D DS2M          E DS
028100070628      *------------------
028200070628      *? DS varie da decodificare a video (abilitate) sulla tab.:"CA"  ?
028300070628      *------------------
028400070628     D DSCA          E DS
028500070628     D*---
028600070628     D SkVar           S              1a   DIM(200)
028700070628     D SkDsc           S             23a   DIM(200)
028800070628     D SkOrd           S              6s 0 DIM(200)
028900000000     D*------------------
029000000000     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
029100000000     D*------------------
029200000000     D DS2W          E DS
029300091013     D*------------------
029400091013     D* DS TIPO SERVIZIO DESCRIZIONE
029500091013     D*------------------
029600091013     D DS5E          E DS
029700040317     D*------------------
029800040317     D* DS TIPO RECORD "BNB" DI SISDE
029900040317     D*------------------
030000040317     D DAR5BNBSDE    E DS
030100040318     D*------------------
030200040318     D* DS TIPO RECORD "BAN" DI SISDE
030300040318     D*------------------
030400040318     D DAR5BANSDE    E DS
030500000807     D*------------------
030600000807     D* DS DI PROCEDURA
030700000807     D*------------------
030800000807     D BS10DS        E DS                  EXTNAME(TIBS10DS)
030900000808     D  SKSC11                21   5520  0 DIM(500)                             *CLIENTI FIGLI
031000950725     D*------------------
031100950725     D* DS SCOMPATTA CLIENTI
031200950725     D*------------------
031300950725     D                 DS
031400950725     D  DSKSN                  1      4P 0
031500950725     D  DSKSA                  1      4
031600970724     D*------------------
031700970724     D* DS SUDDIVISIONE DATA INIZIALE
031800970724     D*------------------
031900970724     D                 DS
032000990601     D  ANNINI                 1      4  0
032100990601     D  MESINI                 5      6  0
032200990601     D  DATINI                 1      6  0
032300970724     D*------------------
032400970724     D* DS SUDDIVISIONE DATA FINALE
032500970724     D*------------------
032600970724     D                 DS
032700990601     D  ANNFIN                 1      4  0
032800990601     D  MESFIN                 5      6  0
032900990601     D  DATFIN                 1      6  0
033000050118     D*------------------
033100050118     D* DS UNIONE DATA SIPES01L
033200050118     D*------------------
033300050118     D                 DS
033400050118     D  pesANN                 1      4  0
033500050118     D  pesMES                 5      6  0
033600050118     D  pesDAT                 1      6  0
033700050118     D*------------------
033800050118     D* DS UNIONE DATA SIVLM01L
033900050118     D*------------------
034000050118     D                 DS
034100050118     D  vlmANN                 1      4  0
034200050118     D  vlmMES                 5      6  0
034300050118     D  vlmDAT                 1      6  0
034400970724     D*------------------
034500990609     D* DS UNIONE DATA SISDO01L
034600970724     D*------------------
034700970724     D                 DS
034800990601     D  SDOANN                 1      4  0
034900990601     D  SDOMES                 5      6  0
035000990601     D  SDODAT                 1      6  0
035100970724     D*------------------
035200990609     D* DS UNIONE DATA SISDC01L
035300970724     D*------------------
035400970724     D                 DS
035500990601     D  SDCANN                 1      4  0
035600990601     D  SDCMES                 5      6  0
035700990601     D  SDCDAT                 1      6  0
035800040317     D*------------------
035900040317     D* DS UNIONE DATA SISDE02L
036000040317     D*------------------
036100040317     D                 DS
036200040317     D  SDEANN                 1      4  0
036300040317     D  SDEMES                 5      6  0
036400040317     D  SDEDAT                 1      6  0
036500970724     D*------------------
036600990609     D* DS UNIONE DATA SISDA01L
036700970724     D*------------------
036800970724     D                 DS
036900990601     D  SDAANN                 1      4  0
037000990601     D  SDAMES                 5      6  0
037100990601     D  SDADAT                 1      6  0
037200970724     D*------------------
037300990609     D* DS UNIONE DATA SISDP01L
037400970724     D*------------------
037500970724     D                 DS
037600990601     D  SDPANN                 1      4  0
037700990601     D  SDPMES                 5      6  0
037800990601     D  SDPDAT                 1      6  0
037900000000     D*------------------
038000000000     D* DEFINIZIONE COSTANTI PER DECODIFICA ORE DI CONSEGNA (15)
038100000000     D*------------------
038200000000     D DORA1           C                   CONST('IN ANTICIPO         ')
038300000000     D DORA2           C                   CONST('IN ORARIO           ')
038400000000     D DORA3           C                   CONST('IN RITARDO DI 12 ORE')
038500000000     D DORA4           C                   CONST('IN RITARDO DI 24 ORE')
038600000000     D DORA5           C                   CONST('IN RITARDO DI 36 ORE')
038700000000     D DORA6           C                   CONST('IN RITARDO DI 48 ORE')
038800000000     D DORA7           C                   CONST('OLTRE 48H DI RITARDO')
038900000000    ?D*------------------
039000000000    ?D* S C H I E R E   M U L T I P L E   D I   M E M O R I Z Z A Z I O N E
039100000000    ?D*------------------
039200000000     D*------------------
039300000000     D* NUMERO DI BYTE IN PACKED
039400000000     D*------------------
039500000000     D* (16:2) + 1 = 9  /  (9 X 12) =  108   --> REGIONE-REGIONE/SERVIZIO/AMMNIN
039600000000     D* (11:2) + 1 = 6  /  (6 X 12) =   72   --> AFFIDABILITA/AMMINISTRAZIONE
039700000000     D* (13:2) + 1 = 7  /  (7 X 12) =   84   --> DELTA PER CLIENTE
039800000000     D*-----------------------------------
039900020520     D* "REGIONE-REGIONE"                 - 500 RAGGRUPPAMENTI X 12 MESI -
040000000000     D*-----------------------------------
040100020520     D CRER            DS                  OCCURS(500) INZ
040200020604     D  REX                    1      6                                         COMBINAZIONE REGIONE
040300020604     D  REC                    7    114P 3                                      COLLI
040400950206     D                                     DIM(12)                              COLLI
040500950206     D                                     PACKEVEN
040600020604     D  REP                  115    222P 3                                      PESO
040700950206     D                                     DIM(12)                              PESO
040800950206     D                                     PACKEVEN
040900020604     D  RER                  223    330P 3                                      RICAVI
041000950206     D                                     DIM(12)                              RICAVI
041100950206     D                                     PACKEVEN
041200020604     D  RES                  331    438P 3                                      SPEDIZIONI
041300950206     D                                     DIM(12)                              SPEDIZIONI
041400950206     D                                     PACKEVEN
041500020605     D  REV                  439    546P 3                                      VOLUME
041600950206     D                                     DIM(12)                              VOLUME
041700950206     D                                     PACKEVEN
041800000000     D*-----------------------------------
041900000000     D* "SERVIZIO BOLLE"                  - 5 VALORI X 12  MESI -
042000000000     D*-----------------------------------
042100000000     D CSER            DS                  OCCURS(5) INZ
042200000000     D  SNC                    1    108P 3                                      CORRIE
042300000000     D                                     DIM(12)                              SERVIZIO CORRIERE
042400000000     D                                     PACKEVEN
042500000629     D  SDC                  109    216P 3                                      DISTRI
042600000629     D                                     DIM(12)                              SERVIZIO DISTRIBUZIO
042700000629     D                                     PACKEVEN
042800000000     D  SEC                  217    324P 3                                      ESPRES
042900000000     D                                     DIM(12)                              SERVIZIO ESPRESSO
043000000000     D                                     PACKEVEN
043100091013     D  SHC                  325    432P 3                                      Espr.H10:30
043200091013     D                                     DIM(12)                              SERVIZIO ESPRESSO H1
043300000629     D                                     PACKEVEN
043400091013     D  SPC                  433    540P 3                                      POSTE
043500091013     D                                     DIM(12)                              SERVIZIO POSTE
043600091013     D                                     PACKEVEN
043700091013      *------
043800040317     D WSER            DS                  OCCURS(5) INZ
043900040317     D  VAW                    1    108P 3                                      VALORE
044000040317     D                                     DIM(12)                              TOTALE VALORE PERIOD
044100040317     D                                     PACKEVEN
044200000000     D*------------------
044300000000     D* "AFFIDABILITA' SPEDIZIONI"        - 7 ORARI X 12  MESI -
044400000000     D*------------------
044500000000     D CAFF            DS                  OCCURS(7) INZ
044600000000     D  CNC                    1     72P 0                                      CORRIE
044700000000     D                                     DIM(12)                              CONSEGNA CORRIERE
044800000629     D  CDC                   73    144P 0                                      DISTRI
044900000629     D                                     DIM(12)                              CONSEGNA DISTRIBUZIO
045000000000     D  CEC                  145    216P 0                                      ESPRES
045100000000     D                                     DIM(12)                              CONSEGNA ESPRESSO
045200000629     D  CPC                  217    288P 0                                      POSTE
045300000629     D                                     DIM(12)                              CONSEGNA POSTE
045400091214     D  CHC                  289    360P 0                                      ESPRES H1030
045500091214     D                                     DIM(12)                              CONSEGNA ESPRESSO H1
045600050118     D*------------------
045700050118     D* "PESI/VOLUMI"
045800050118     D*------------------
045900050118     D CPEV            DS                  OCCURS(2) INZ
046000050118     D  NCV                           7P 0                                      nr.colli VDL
046100050118     D                                     DIM(12)                              nr.colli VDL
046200050118     D  NCB                           7P 0                                      nr.colli Bollettati
046300050118     D                                     DIM(12)                              nr.colli Bollettati
046400050118     D  PVV                          12P 3                                      peso/vol VDL
046500050118     D                                     DIM(12)                              peso/vol VDL
046600050118     D  PVB                          12P 3                                      peso/vol Bollettati
046700050118     D                                     DIM(12)                              peso/vol Bollettati
046800000000     D*------------------
046900000000     D* "DELTA CLIENTE"                   - 11 FASCE X 12  MESI -
047000000000     D*------------------
047100000000     D CDEL            DS                  OCCURS(11) INZ
047200010920     D  DRC                    1     84P 3                                      R REAL
047300000000     D                                     DIM(12)                              DELTA RICAVI REALI
047400010920     D  DPC                   85    168P 3                                      R PRES
047500000000     D                                     DIM(12)                              DELTA RICAVI PRESUNT
047600000000     D  DSC                  169    252P 0                                      SPEDIZ
047700000000     D                                     DIM(12)                              DELTA SPEDIZIONI
047800000000     D*------------------
047900050118     D* "AMMINISTRAZIONE "                - 50 VARIE X 12  MESI -
048000000000     D*------------------
048100050118     D CAMM            DS                  OCCURS(50) INZ
048200000000     D  ARI                    1    108P 3                                      RICAVI
048300000000     D                                     DIM(12)                              RICAVI
048400000000     D                                     PACKEVEN
048500000000     D  ASP                  109    180P 0                                      SPEDIZ
048600000000     D                                     DIM(12)                              SPEDIZIONI
048700020905     D*------------
048800020905     D* DS REPERIMENTO DATI UTENTE
048900020905     D*------------
049000020905     D TIBS02DS      E DS
049100020905     D TIBS34DS      E DS                                                       *Profili utente
049200020905     D DDATIUTE      E DS                                                       *Dati utente
049300020905     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
049400020910     D TIBS56DS      E DS
049500020910     D  SKI                    7    756  0 dim(250)
049600040317     D*-------------------
049700040317     D* DEFINIZIONE VARIABILI
049800040317     D*-------------------
049900040318     D COR             S              9  0 DIM(12)                              VALORE GENERICO
050000040318     D BAN             S              7  0 DIM(12)                              VALORE GENERICO
050100040318     D BNB             S              7  0 DIM(12)                              VALORE GENERICO
050200000000     C*------------------------------------------------------------------------*
050300000000     C*?MAIN LINE
050400000000     C*------------------------------------------------------------------------*
050500020523     C*
050600020523     C* VERIFICO IL TIPO DI CHIAMATA X CONDIZINAR TUTTA L'ELABORAZIONE SUCCESSIVA
050700020523     C                   IF        D60SCE = '4' OR
050800020523     C                             D60SCE = '5'
050900020523     C                   OPEN      TISE62P
051000170801     c                   eval      $ClienteStampato  = *off
051100020823     C                   MOVEL     '1'           *IN01                          *OVERFLOW
051200020524     C                   Z-ADD     *ZEROS        WRKKSC                         *CLIENTE DI WRK
051300020523     C                   EXSR      SETFIL
051400020523     C                   MOVEL     WRKKSC        DEPFIL            3 0
051500020823     c*
051600020524     C* CICLO SU TUTO IL SISDO01L
051700020524     C                   DOW       $FINREC = '0'
051800020823     c*
051900020524     C* TESTO IL CAMBIO FILIALE X LA SUDDIVISIONE IN SPOOL DIVERSI X CLIENTI DIVERSI SE TIPO LANCIO
052000020524     C                   IF        D60OP1 = 'STT'
052100020523     C                   MOVEL     WRKKSC        CURFIL            3 0
052200020523     C                   IF        CURFIL <> DEPFIL
052300020523     C                   MOVEL     WRKKSC        DEPFIL
052400071210      *
052500071211     c                   eval      Righe_da_stampare = 4
052600071211     c                   exsr      solo_x_STT
052700170801     c                   if        $ClienteStampato  = *on
052800020523     C                   WRITE     SE62FI                                       *RIGA FINE PROSPETTO
052900170721     c                   endif
053000071210      *
053100020523     C                   CLOSE     TISE62P
053200020523     C                   OPEN      TISE62P
053300170801     c                   eval      $ClienteStampato  = *off
053400020823     C                   MOVEL     '1'           *IN01                          *OVERFLOW
053500020523     C                   ENDIF
053600020524     C                   ENDIF
053700020524     C* ESEGUO LA VERA ELABORAZIONE
053800020524     C                   EXSR      CARCLI
053900020523     C                   EXSR      SUBMAIN
054000020524     C                   EXSR      SETFIL
054100020523     C                   ENDDO
054200020524     C* ESEGUO L' ELABORAZIONE DELL'LTIMO CLIENTE LETTO
054300020524     C                   EXSR      CARCLI
054400020524     C                   EXSR      SUBMAIN
054500071210      *
054600071211     c                   eval      Righe_da_stampare = 4
054700071211     c                   exsr      solo_x_STT
054800170801     c                   if        $ClienteStampato  = *on
054900020524     C                   WRITE     SE62FI                                       *RIGA FINE PROSPETTO
055000170721     c                   endif
055100071210      *
055200020524     C                   CLOSE     TISE62P
055300020523     C                   ELSE
055400020523     C* LANCIO DA VIDEO
055500020523     C                   OPEN      TISE62P
055600170801     c                   eval      $ClienteStampato  = *off
055700020823     C                   MOVEL     '1'           *IN01                          *OVERFLOW
055800020523     C                   EXSR      CARCLI
055900020523     C                   EXSR      SUBMAIN
056000071210      *
056100071211     c                   eval      Righe_da_stampare = 4
056200071211     c                   exsr      solo_x_STT
056300170801     c                   if        $ClienteStampato  = *on
056400020523     C                   WRITE     SE62FI                                       *RIGA FINE PROSPETTO
056500170721     c                   endif
056600071210      *
056700020523     C                   CLOSE     TISE62P
056800020523     C                   ENDIF
056900020523     C*
057000020523     C* OPERAZIONI FINALI
057100020523     C                   EXSR      FINSR
057200020523     C*
057300020523     C                   SETON                                        LR
057400020523     C*------------------------------------------------------------------------*
057500020523     C*?CARCLI - CARICA I CLIENTI DA ELABORARE
057600020523     C*------------------------------------------------------------------------*
057700020523     C     CARCLI        BEGSR
057800020523     C*
057900020523     C* IMPOSTA IL CLIENTE/I DA ELABORARE
058000020523     C                   CLEAR                   CLI
058100020523     C                   CLEAR                   OCLI
058200050308     C                   CLEAR                   CDati
058300020523IF  1C     D60OP0        IFEQ      'VIS'
058400020523     C                   MOVEA     D60KSC        KSA                            *DS INPUT -> SCH ALF
058500020523IF  2C     D60SCE        IFEQ      '1'                                          *UN SOLO CLIENTE
058600020523     C                   MOVE      KSA(1)        DSKSA                          *SCH ALF -> DS ALF
058700020523     C                   Z-ADD     DSKSN         CLI(1)                         *DS NUM -> SCH NUM
058800020523E   2C                   ENDIF
058900020523IF  2C     D60SCE        IFEQ      '2'                                          * + CLIENTI INSIEME
059000050117DO  3C**** 1             DO        30            C
059100050117DO  3C     1             DO        28            C
059200020523     C                   MOVE      KSA(C)        DSKSA                          *SCH ALF -> DS ALF
059300020523     C                   Z-ADD     DSKSN         CLI(C)                         *DS NUM -> SCH NUM
059400020523E   3C                   ENDDO
059500020523E   2C                   ENDIF
059600020523IF  2C     D60SCE        IFEQ      '3'                                          *CLIENTE UNIFICANTE
059700020523     c                   CLEAR                   SKSC8
059800020523     C                   CLEAR                   BS10DS
059900020523     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
060000020523     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
060100020523     C                   MOVEL     'F'           D10PAF                         *CERCA figli
060200020523     C                   MOVE      KSA(1)        DSKSA                          *SCH ALF -> DS ALF
060300020523     C                   Z-ADD     DSKSN         N7                7 0          *DS NUM -> SCH NUM
060400020523     C                   Z-ADD     N7            D10COD                         *CODICE CLIENTE
060500020523     C                   CALL      'TIBS10R'
060600020523     C                   PARM                    BS10DS
060700020523IF  3C     D10ERR        IFEQ      *BLANKS                                      *NO ERRORE
060800020523     C                   Z-ADD     SKSC11        SKSC8                          *CLIENTI PADRE+FIGLI
060900020523DO  4C     1             DO        500           C
061000020523IF  5C     SKSC8(C)      IFGT      *ZEROS
061100020523     C                   MOVE      SKSC8(C)      CLI(C)                         *CLIENTI DETTAGLIO
061200020523E   5C                   ENDIF
061300020523E   4C                   ENDDO
061400020523X   3C                   ELSE                                                   *SI ERRORE
061500020823     C                   Z-ADD     N7            CLI(1)                         *SE STESSO
061600020523E   3C                   ENDIF
061700020523E   2C                   ENDIF
061800020523E   1C                   ENDIF
061900020523IF  1C     D60OP0        IFEQ      'STA'
062000020523IF  2C     D60SCE        IFEQ      '4'                                          *TUTTI I CLIENTI NN
062100020524     C                   Z-ADD     WRKKSC        CLI(1)                         *DS NUM -> SCH NUM
062200020523E   2C                   ENDIF
062300020523IF  2C     D60SCE        IFEQ      '5'                                          *TUTTI I CLIENTI X U
062400020523     c                   CLEAR                   SKSC8
062500020523     C                   CLEAR                   BS10DS
062600020523     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
062700020523     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
062800020523     C                   MOVEL     'F'           D10PAF                         *CERCA figli
062900020524     C                   Z-ADD     WRKKSC        D10COD                         *CODICE CLIENTE
063000020523     C                   CALL      'TIBS10R'
063100020523     C                   PARM                    BS10DS
063200020523IF  3C     D10ERR        IFEQ      *BLANKS                                      *NO ERRORE
063300020523     C                   Z-ADD     SKSC11        SKSC8                          *CLIENTI PADRE+FIGLI
063400020523DO  4C     1             DO        500           C
063500020523IF  5C     SKSC8(C)      IFGT      *ZEROS
063600020523     C                   MOVE      SKSC8(C)      CLI(C)                         *CLIENTI DETTAGLIO
063700020523E   5C                   ENDIF
063800020523E   4C                   ENDDO
063900020523X   3C                   ELSE                                                   *SI ERRORE
064000020524     C                   Z-ADD     WRKKSC        CLI(1)                         *DS NUM -> SCH NUM
064100020523E   3C                   ENDIF
064200020523E   2C                   ENDIF
064300020523E   1C                   ENDIF
064400020523     C*
064500020523     C* COMPATTA I CLIENTI TOGLIENDO I BUCHI VUOTI
064600090304     C                   clear                   OCLI
064700090304     C                   Z-ADD     0             compat            3 0
064800020523DO  2C     1             DO        500           C
064900020523IF  3C     CLI(C)        IFGT      *ZEROS
065000090304     C                   add       1             compat
065100090304     C                   Z-ADD     CLI(C)        OCLI(Compat)
065200020523E   3C                   ENDIF
065300020523E   2C                   ENDDO
065400020523     C                   Z-ADD     OCLI          CLI                            *CLIENTI COMPATTI
065500020523     C*
065600020523     C                   ENDSR
065700020523     C*------------------------------------------------------------------------*
065800020523     C*?SUBMAIN - POSIZIONAMENTO E PRIMA LETTURA SALDI CONTABILITA' CLIENTE
065900020523     C*------------------------------------------------------------------------*
066000020523     C     SUBMAIN       BEGSR
066100020523     C*
066200020523     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CONTABLILITA' CLIENTE
066300020524     C                   Z-ADD     CLI(1)        DEPKSC                         *CL SELEZIONATO
066400020524     C                   Z-ADD     CLI(1)        PRIKSC                         *1° CL SELEZIONATO
066500020823     C                   Z-ADD     1             DEPPAG                         *N.PAGINE PER CLIENT
066600020823     C                   MOVEL     '1'           *IN01                          *SI OVERFLW  (1° CLI
066700020823     C*
066800020823     C* REPERISCE IL TERMINAL PARTENZA (del cliente padre)
066900050307     C                   MOVEL     DEPKSC        WFIL              3 0
067000020823     C                   CLEAR                   FNLV55DS
067100020823     C                   MOVEL     'P'           D55TPT
067200020823     C                   MOVEL     WFIL          D55LIN
067300020823     C                   Z-ADD     DATCOR        D55DRF
067400020823     C                   CALL      'FNLV55R'
067500020823     C                   PARM                    FNLV55DS
067600020823IF  1C     D55ERR        IFNE      *BLANKS
067700020823     C                   MOVEL     WFIL          D55TFP
067800020823E   1C                   ENDIF
067900020823     C                   Z-ADD     D55TFP        DEPTFP
068000020823     C*
068100020523     C* CICLO FINO A FINE CLIENTI VALIDI CARICATI
068200020823     c                   Z-ADD     1             C
068300000808DO  2C     C             DOWLE     500
068400020524     C     CLI(C)        ANDGT     *ZEROS
068500000000     C*
068600000000     C* OPERAZIONI PER NUOVO CLIENTE
068700020523     C                   EXSR      NEWKSC
068800000000     C*
068900000000     C* LETTURA SALDI PER TUTTE LE STATISTICHE
069000950725IF  4C     SOLAFF        IFEQ      '0'                                          *CHIESTE ANCHE ALTRE
069100000000     C                   EXSR      CICSTA                                       *STATISTICHE OLTRE L
069200040318     C                   EVAL      WTRD = 'BNB'
069300040317     C                   EXSR      CICSTAE                                      *STATISTICHE OLTRE L
069400040318     C                   EVAL      WTRD = 'BAN'
069500040318     C                   EXSR      CICSTAE                                      *STATISTICHE OLTRE L
069600050118     C                   EXSR      CICSTAP                                      *STATISTICHE PESI
069700050118     C                   EXSR      CICSTAV                                      *STATISTICHE VOLUMI
069800950725E   4C                   ENDIF                                                  *AFFIDABILITA' SPEDI
069900000000     C*
070000000000     C* LETTURA SALDI PER LA STATISTICA "AFFIDABILITA' SPEDIZIONI"
070100950725IF  4C     MAIAFF        IFEQ      '0'                                          *CHIESTE ANCHE LA
070200000000     C                   EXSR      CICAFF                                       *STATISTICA AFFID.TA
070300950725E   4C                   ENDIF                                                  *OLTRE LE ALTRE
070400000000     C*
070500000000     C* LETTURA PARTICOLARITA' SALDI CLIENTE PER LA "AMMINISTRAZIONE"
070600950725IF  4C     D60ST5        IFEQ      'S'
070700000000     C                   EXSR      CICAMM
070800950725E   4C                   ENDIF
070900950725     C*
071000050308     c*  Se il Cliente non ha dati memorizzo in una schiera con (*) <- senza DATI X LA SELEZIONE
071100050308     C                   if        Con_Dati ='0'
071200050308     c                   move      '*'           CDati(C)
071300050308     c                   end
071400050308     c*
071500020523     C                   ADD       1             C                              *PROSSIMO CLIENTE
071600000808IF  4C     C             IFLE      500
071700020524     C                   IF        CLI(C) > *ZEROS
071800000810     C                   Z-ADD     CLI(C)        DEPKSC
071900020524     C                   ENDIF
072000950725X   4C                   ELSE
072100950725     C                   MOVEL     '1'           $FINO
072200950725E   4C                   ENDIF
072300950725E   2C                   ENDDO
072400020524     C*
072500020524     C* A SECONDA DEL TIPO DI ELABORAZIONE IMPOSTO IL CLIENTE X CUI REPERIRE LE INFORMAZIONI
072600020524     C     D60SCE        IFEQ      '3'
072700020524     C     D60SCE        OREQ      '5'
072800020524     C                   Z-ADD     PRIKSC        DEPKSC
072900020524     C                   ENDIF
073000000000     C*
073100000000     C* STAMPO STATISTICA/CHE CLIENTE
073200000000IF  2C     PASKSC        IFEQ      '1'
073300000000     C                   EXSR      STACLI                                       *SE MEMORIZZATO QC.
073400000000E   2C                   ENDIF
073500020523     C*
073600020523     C                   ENDSR
073700000000     C*------------------------------------------------------------------------*
073800000000     C*?SETFIL - POSIZIONAMENTO E PRIMA LETTURA SALDI CONTABILITA' CLIENTE
073900000000     C*------------------------------------------------------------------------*
074000000000     C     SETFIL        BEGSR
074100020524     C*
074200020524     C                   MOVEL     '0'           $FINO
074300000000     C*
074400020523     C                   Z-ADD     WRKKSC        KSOKSC                         *CLIENTE PIU' PICCOL
074500990602     C                   Z-ADD     9999          KSOANN                         *ANNO PIU' GRANDE
074600000000     C*
074700000000     C* POSIZIONAMENTO PER CERCARE IL PRIMO CLIENTE VALIDO (KSC+ANN)
074800990609     C     KEYSDO        SETGT     SISDO01L                           99        *FINE ARCHIVIO
074900000000     C*
075000000000IF  1C     *IN99         IFEQ      '1'
075100000000     C                   MOVEL     '1'           $FINO
075200020524     C                   MOVEL     '1'           $FINREC
075300000000X   1C                   ELSE
075400000000     C                   EXSR      LETFIL                                       *LETTURA RECORD
075500000000E   1C                   ENDIF
075600000000     C*
075700000000     C                   ENDSR
075800000000     C*------------------------------------------------------------------------*
075900000000     C*?LETFIL - LETTURA PROSSIMO RECORD FILE SALDI CONTABILITA' CLIENTE
076000000000     C*------------------------------------------------------------------------*
076100000000     C     LETFIL        BEGSR
076200000000     C*
076300000000     C* LEGGO FINO A:
076400000000     C*     - FINE FILE
076500000000     C*     - TROVATO RECORD VALIDO
076600000000     C                   MOVEL     'N'           WRECOK
076700020524DO  1C     $FINREC       DOWEQ     '0'
076800020524     C     WRECOK        ANDEQ     'N'
076900000000     C*
077000990609     C                   READ      SISDO01L                               99
077100000000     C*
077200000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
077300000000     C                   MOVEL     '1'           $FINO
077400020524     C                   MOVEL     '1'           $FINREC
077500000000X   2C                   ELSE
077600000000     C                   EXSR      CHKREC                                       *CONTROLLO RECORD
077700000000E   2C                   ENDIF
077800000000     C*
077900000000E   1C                   ENDDO
078000000000     C*
078100020523     C                   Z-ADD     SDOKSC        WRKKSC                         *CLIENTE VALIDO
078200000000     C*
078300000000     C                   ENDSR
078400000000     C*------------------------------------------------------------------------*
078500000000     C*?CHKREC - CONTROLLA VALIDITA' RECORD SALDI CONTABILITA' CLIENTE
078600000000     C*------------------------------------------------------------------------*
078700000000     C     CHKREC        BEGSR
078800000000     C*
078900000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
079000000000     C*
079100000000     C* SE IL SALDO DEL CLIENTE LETTO HA UNA DATA SUPERIORE A QUELLA RICHIESTA
079200000000     C* A VIDEO, LEGGO DIRETTAMENTE UN ALTRO CLIENTE
079300020823IF  1C                   IF        WRECOK = 'S'
079400020823IF  2C     SDODAT        IFGE      D60DAT
079500020823     C     SDODAT        ANDLE     D60DA2
079600020823X   2C                   ELSE
079700000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
079800020823E   2C                   ENDIF
079900020823E   1C                   ENDIF
080000020523     C*
080100020524     C* X LANCIO X UNIFICANTI VERIFICO CHE NN SIA UN CLIENTE FIGLIO
080200020523     C                   IF        WRECOK = 'S'
080300020523     C     D60SCE        IFEQ      '5'
080400020524     C                   CLEAR                   BS10DS
080500020524     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
080600020524     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
080700020524     C                   MOVEL     'P'           D10PAF                         *VERIFICA SE PADRE
080800020524     C                   Z-ADD     SDOKSC        D10COD                         *CODICE CLIENTE
080900020524     C                   CALL      'TIBS10R'
081000020524     C                   PARM                    BS10DS
081100020524IF  3C     D10ERR        IFEQ      *BLANKS                                      *NO ERRORE
081200020524     C                   MOVEL     'N'           WRECOK
081300020524     C                   ENDIF
081400020523     C                   ENDIF
081500020524     C                   ENDIF
081600020523     C*
081700020524     C* SE RICHIESTA SELEZIONE VERIFICO LA QUALITA' CLIENTE RICHIESTA NEL LANCIO (SOLO X TIPO 'STA')
081800020523     C                   IF        WRECOK = 'S'
081900020524     C                   IF        D60OP0 = 'STA'
082000020523     C                   IF        D60CLA <> *BLANKS OR
082100020523     C                             D60CLB <> *BLANKS OR
082200020523     C                             D60CLC <> *BLANKS OR
082300020523     C                             D60CLT <> *BLANKS OR
082400020523     C                             D60CLD <> *BLANKS
082500020523     C                   Z-ADD     SDOKSC        CLPKSC
082600020916     C                   Z-ADD     1             CLPKUT
082700020916     C                   Z-ADD     DUTKCI        CLPKCC
082800020523     C     KEYCLP        CHAIN     CNCLP00F
082900020523     C                   IF        %FOUND(CNCLP00F) AND
083000020523     C                             (CLPCLV = D60CLA OR
083100020523     C                              CLPCLV = D60CLB OR
083200020523     C                              CLPCLV = D60CLC OR
083300020523     C                              CLPCLV = D60CLT OR
083400020523     C                              CLPCLV = D60CLD)
083500020523     C                   ELSE
083600020523     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
083700020523     C                   ENDIF
083800020524     C                   ENDIF
083900020523     C                   ENDIF
084000020523     C                   ENDIF
084100020523     C*
084200000000     C                   ENDSR
084300000000     C*------------------------------------------------------------------------*
084400000000     C*?CICSTA - CICLO MEMORIZZAZIONE SALDI PER TUTTE LE STATISTICHE
084500000000     C*------------------------------------------------------------------------*
084600000000     C     CICSTA        BEGSR
084700000000     C*
084800000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
084900000000     C                   EXSR      SETFI1
085000000000     C*
085100000000     C* CICLO FINO A FINE FILE SALDI CLIENTE
085200000000DO  1C     $FINC         DOWEQ     '0'
085300000000     C*
085400000000     C* MEMORIZZA I SALDI CLIENTE
085500000000     C                   EXSR      MEMSDC
085600000000     C*
085700000000     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
085800000000     C                   EXSR      LETFI1
085900000000     C*
086000000000E   1C                   ENDDO
086100000000     C*
086200000000     C                   ENDSR
086300000000     C*------------------------------------------------------------------------*
086400000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
086500000000     C*------------------------------------------------------------------------*
086600000000     C     SETFI1        BEGSR
086700000000     C*
086800000000     C* REIMPOSTO IL FINE FILE PER CICLO SALDI CLIENTE
086900000000     C                   MOVEL     '0'           $FINC
087000000000     C*
087100000000     C                   Z-ADD     DEPKSC        KSCKSC                         *DEPOSITO CLIENTE
087200000000     C                   Z-ADD     ANNINI        KSCANN                         *ANNO INIZIALE
087300000000     C                   Z-ADD     MESINI        KSCMES                         *MESE INIZIALE
087400000000     C*
087500000000     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
087600990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
087700000000     C*
087800000000IF  1C     *IN99         IFEQ      '1'
087900000000     C                   MOVEL     '1'           $FINC
088000000000X   1C                   ELSE
088100000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
088200000000E   1C                   ENDIF
088300000000     C*
088400000000     C                   ENDSR
088500000000     C*------------------------------------------------------------------------*
088600000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI CLIENTE
088700000000     C*------------------------------------------------------------------------*
088800000000     C     LETFI1        BEGSR
088900000000     C*
089000000000     C* LEGGO FINO A:
089100000000     C*     - FINE FILE
089200000000     C*     - TROVATO RECORD VALIDO
089300000000     C                   MOVEL     'N'           WRECOK
089400000000DO  1C     $FINC         DOWEQ     '0'
089500000000     C     WRECOK        ANDEQ     'N'
089600000000     C*
089700990609     C     KSCKSC        READE     SISDC01L                               99
089800000000     C*
089900000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
090000000000     C                   MOVEL     '1'           $FINC
090100000000X   2C                   ELSE
090200000000     C                   EXSR      VALSDC                                       *VALORIZZA CON "SDC"
090300000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
090400030520     c*
090500030520     C* CONTROLLO SE IL RECORD E' VALORIZZATO
090600030520IF  3C     SDCNSP        IFEQ      *ZEROS
090700030520     C     SDCPKG        ANDEQ     *ZEROS
090800030520     C     SDCPCC        ANDEQ     *ZEROS
090900030520     C     SDCPLS        ANDEQ     *ZEROS
091000030520     C     SDCNCO        ANDEQ     *ZEROS
091100030520     C     SDCVOL        ANDEQ     *ZEROS
091200030520     C     SDCVCC        ANDEQ     *ZEROS
091300030520     C     SDCVLS        ANDEQ     *ZEROS
091400030520     C     SDCIRB        ANDEQ     *ZEROS
091500030520     C     SDCIRR        ANDEQ     *ZEROS
091600030520     C     SDCINC        ANDEQ     *ZEROS
091700030520     C     SDCIRP        ANDEQ     *ZEROS
091800030520     C     SDCIRC        ANDEQ     *ZEROS
091900030520     C     SDCIRD        ANDEQ     *ZEROS
092000030520     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
092100030520E   3C                   ENDIF
092200000000E   2C                   ENDIF
092300000000     C*
092400000000E   1C                   ENDDO
092500000000     C*
092600000000     C                   ENDSR
092700000000     C*------------------------------------------------------------------------*
092800000000     C*?VALSDC - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDC"
092900000000     C*------------------------------------------------------------------------*
093000000000     C     VALSDC        BEGSR
093100000000     C*
093200000000     C                   MOVEL     'SDC'         $SDX                           *FILE DA CONTROLLARE
093300990601     C                   Z-ADD     SDCDAT        $DAT                           *DATA (AAAA/MM)
093400000000     C                   MOVEL     SDCTSP        $TSP                           *TIPO SERVIZIO
093500000000     C                   MOVEL     SDCTPO        $TPO                           *TIPO PORTO
093600000000     C***                  MOVELSDCTPR    $TPR             *TIPO PRODOTTO
093700000000     C                   MOVEL     SDCCTR        $CTR                           *CODICE TARIFFA
093800000000     C                   MOVEL     SDCFPE        $FPE                           *FASCIA DI PESO
093900000000     C                   MOVEL     SDCRBL        $RBL                           *RAGGRUPPAMENTO BOLL
094000000000     C*
094100000000     C                   ENDSR
094200000000     C*------------------------------------------------------------------------*
094300000000     C*?CHKRE1 - CONTROLLA VALIDITA' RECORD SALDI CLIENTE
094400000000     C*------------------------------------------------------------------------*
094500000000     C     CHKRE1        BEGSR
094600000000     C*
094700000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
094800000000     C*--------------------
094900990601     C* DATA (AAAA/MM)
095000000000     C*--------------------
095100000000     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
095200020823IF  1C     $DAT          IFGE      D60DAT
095300020823     C     $DAT          ANDLE     D60DA2
095400020823X   1C                   ELSE
095500020823     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
095600020823IF  2C     $DAT          IFGT      D60DA2
095700000000     C                   MOVEL     '1'           $FINC                          *ESCO DAL CICLO "SDC
095800000000     C                   MOVEL     '1'           $FINA                          *ESCO DAL CICLO "SDA
095900020823E   2C                   ENDIF
096000000000E   1C                   ENDIF
096100000000     C*
096200000000IF  1C     WRECOK        IFEQ      'S'
096300000000     C*
096400000000     C* CONTROLLO IL RECORD LETTO CON I PARAMETRI GENERALI RICHIESTI A VIDEO
096500010920     C* IMPORTANDO LA ROUTINE DI CONTROLLO ESTERNA ('TISE62T')
096600000000     C                   MOVEL     'S'           $ROK                           *RECORD OK X ROUTINE
096700000000     C*
096800010920    ? /COPY GAITRASRC/SRCTISE,TISE62T
096900000000     C*
097000000000     C* SE IL RECORD E' ERRATO NON FACCIO ULTERIORI CONTROLLI
097100000000     C                   MOVEL     $ROK          WRECOK
097200000000IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
097300000000     C*
097400000000     C* CONTROLLO I PARAMETRI DELLE STATISTICHE DIVERSE DALLA AFFIDABILITA' SOLO
097500990609     C* SE SONO NEL CICLO DI LETTURA DI SISDC00F
097600000000IF  3C     $SDX          IFEQ      'SDC'
097700000000     C*
097800000000     C* CONTROLLO PARAMETRI STATISTICA REGIONE-REGIONE SE SELEZIONATA
097900000000IF  4C     D60ST1        IFEQ      'S'
098000000000     C                   EXSR      CHKREG
098100000000E   4C                   ENDIF
098200050118     C*
098300050118     C* CONTROLLO PARAMETRI STATISTICA PESI-VOLUMI    SE SELEZIONATA
098400050118IF  4C     D60STp        IFEQ      'S'
098500050118     C                   EXSR      CHKpev
098600050118E   4C                   ENDIF
098700000000     C*
098800000000     C* CONTROLLO PARAMETRI STATISTICA SERVIZIO BOLLE SE SELEZIONATA
098900000000IF  4C     D60ST2        IFEQ      'S'
099000000000     C                   EXSR      CHKSER
099100000000E   4C                   ENDIF
099200000000     C*
099300000000     C* CONTROLLO PARAMETRI STATISTICA DELTA CLIENTE SE SELEZIONATA
099400000000IF  4C     D60ST4        IFEQ      'S'
099500000000     C                   EXSR      CHKDEL
099600000000E   4C                   ENDIF
099700000000     C*
099800000000     C* CONTROLLO PARAMETRI STATISTICA AMMINISTRAZIONE SE SELEZIONATA
099900000000IF  4C     D60ST5        IFEQ      'S'
100000000000     C                   EXSR      CHKAMM
100100000000E   4C                   ENDIF
100200000000     C*
100300000000     C* SE PER TUTTE LE STAT CONTROLLATE IL RECORD NON VA BENE NE LEGGO UN ALTRO
100400000000IF  4C     ROKREG        IFEQ      'N'                                          *NO X REGIONE-REGION
100500050118     C     ROKPEV        ANDEQ     'N'                                          *NO X PESI/VOLUMI
100600000000     C     ROKSER        ANDEQ     'N'                                          *NO X SERVIZIO BOLLE
100700000000     C     ROKDEL        ANDEQ     'N'                                          *NO X DELTA CLIENTE
100800000000     C     ROKAMM        ANDEQ     'N'                                          *NO X AMMINISTRAZION
100900000000     C                   MOVEL     'N'           WRECOK                         *NON VALIDO
101000000000E   4C                   ENDIF
101100000000     C*
101200000000X   3C                   ELSE
101300000000     C*
101400000000     C* CONTROLLO I PARAMETRI DELLE STATISTICA AFFIDABILITA' SOLO SE SONO NEL
101500990609     C* CICLO DI LETTURA DI SISDA00F
101600000000IF  4C     D60ST3        IFEQ      'S'
101700000000     C                   EXSR      CHKAFF
101800000000     C                   MOVEL     ROKAFF        WRECOK                         *RECORD VALIDO O NON
101900000000E   4C                   ENDIF
102000000000E   3C                   ENDIF
102100000000     C*
102200000000E   2C                   ENDIF
102300000000E   1C                   ENDIF
102400000000     C*
102500000000     C                   ENDSR
102600040317     C*------------------------------------------------------------------------*
102700040317     C*?CICSTAE - CICLO MEMORIZZAZIONE SALDI ESTENSIONE
102800040317     C*------------------------------------------------------------------------*
102900040317     C     CICSTAE       BEGSR
103000040317     C*
103100040317     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
103200040317     C                   EXSR      SETFI1E
103300040317     C*
103400040317     C* CICLO FINO A FINE FILE SALDI CLIENTE
103500040317DO  1C     $FINCE        DOWEQ     '0'
103600040317     C*
103700040317     C* MEMORIZZA I SALDI CLIENTE
103800040317     C                   EXSR      MEMSDE
103900040317     C*
104000040317     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
104100040317     C                   EXSR      LETFI1E
104200040317     C*
104300040317E   1C                   ENDDO
104400040317     C*
104500040317     C                   ENDSR
104600040317     C*------------------------------------------------------------------------*
104700040317     C*?SETFI1E - POSIZIONAMENTO E PRIMA LETTURA SALDI ESTENSIONE
104800040317     C*------------------------------------------------------------------------*
104900040317     C     SETFI1E       BEGSR
105000040317     C*
105100040317     C* REIMPOSTO IL FINE FILE PER CICLO SALDI CLIENTE
105200040317     C                   MOVEL     '0'           $FINCE
105300040317     C*
105400040317     C                   MOVEL     'SDC'         KSEFILE                        *DEPOSITO CLIENTE
105500040318     C                   MOVEL     WTRD          KSETRD                         *DEPOSITO CLIENTE
105600040317     C                   Z-ADD     DEPKSC        KSEKSC                         *DEPOSITO CLIENTE
105700040317     C                   Z-ADD     ANNINI        KSEANN                         *ANNO INIZIALE
105800040317     C                   Z-ADD     MESINI        KSEMES                         *MESE INIZIALE
105900040317     C*
106000040317     C* POSIZIONAMENTO FILE SALDI ESTENSIONE
106100040317     C     KEYSDE        SETLL     SISDE02L                           99        *FINE ARCHIVIO
106200040317     C*
106300040317IF  1C     *IN99         IFEQ      '1'
106400040317     C                   MOVEL     '1'           $FINCE
106500040317X   1C                   ELSE
106600040317     C                   EXSR      LETFI1E                                      *LETTURA RECORD
106700040317E   1C                   ENDIF
106800040317     C*
106900040317     C                   ENDSR
107000040317     C*------------------------------------------------------------------------*
107100040317     C*?LETFI1E - LETTURA PROSSIMO RECORD FILE SALDI ESTENSIONE
107200040317     C*------------------------------------------------------------------------*
107300040317     C     LETFI1E       BEGSR
107400040317     C*
107500040317     C* LEGGO FINO A:
107600040317     C*     - FINE FILE
107700040317     C*     - TROVATO RECORD VALIDO
107800040317     C                   MOVEL     'N'           WRECOK
107900040317DO  1C     $FINCE        DOWEQ     '0'
108000040317     C     WRECOK        ANDEQ     'N'
108100040317     C*
108200040317     C     KEYSDE1       READE     SISDE02L                               99
108300040317     C*
108400040317IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
108500040317     C                   MOVEL     '1'           $FINCE
108600040317X   2C                   ELSE
108700040317     C                   EXSR      VALSDE                                       *VALORIZZA CON "SDC"
108800040317     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
108900040317E   2C                   ENDIF
109000040317     C*
109100040317E   1C                   ENDDO
109200040317     C*
109300040317     C                   ENDSR
109400040317     C*------------------------------------------------------------------------*
109500040317     C*?VALSDE - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDC"
109600040317     C*------------------------------------------------------------------------*
109700040317     C     VALSDE        BEGSR
109800040317     C*
109900040317     C                   MOVEL     'SDC'         $SDX                           *FILE DA CONTROLLARE
110000040317     C                   Z-ADD     SDEDAT        $DAT                           *DATA (AAAA/MM)
110100040317     C                   MOVEL     SDETSP        $TSP                           *TIPO SERVIZIO
110200040317     C                   MOVEL     SDETPO        $TPO                           *TIPO PORTO
110300040317     C***                  MOVELSDETPR    $TPR             *TIPO PRODOTTO
110400040317     C                   MOVEL     SDECTR        $CTR                           *CODICE TARIFFA
110500040317     C                   MOVEL     SDEFPE        $FPE                           *FASCIA DI PESO
110600040317     C                   MOVEL     SDERBL        $RBL                           *RAGGRUPPAMENTO BOLL
110700040317     C*
110800040317     C                   ENDSR
110900000000     C*------------------------------------------------------------------------*
111000000000     C*?CHKREG - CONTROLL RECORD CON I PARAMETRI DELLA "REGIONE-REGIONE"
111100000000     C*------------------------------------------------------------------------*
111200000000     C     CHKREG        BEGSR
111300000000     C*
111400000000     C                   MOVEL     'S'           ROKREG                         *RECORD VALIDO
111500000000     C*
111600000000     C                   ENDSR
111700050118     C*------------------------------------------------------------------------*
111800050118     C*?CHKPEV - CONTROLL RECORD CON I PARAMETRI DELLA "PESI-VOLUMI"
111900050118     C*------------------------------------------------------------------------*
112000050118     C     CHKpev        BEGSR
112100050118     C*
112200050118     C                   MOVEL     'S'           ROKpev                         *RECORD VALIDO
112300050118     C*
112400050118     C                   ENDSR
112500000000     C*------------------------------------------------------------------------*
112600000000     C*?CHKSER - CONTROLLO RECORD CON I PARAMETRI DELLA "SERVIZIO BOLLE"
112700000000     C*------------------------------------------------------------------------*
112800000000     C     CHKSER        BEGSR
112900000000     C*
113000000000     C                   MOVEL     'S'           ROKSER                         *RECORD VALIDO
113100000000     C*
113200000000     C                   ENDSR
113300000000     C*------------------------------------------------------------------------*
113400000000     C*?CHKDEL - CONTROLLO RECORD CON I PARAMETRI DELLA "DELTA CLIENTE"
113500000000     C*------------------------------------------------------------------------*
113600000000     C     CHKDEL        BEGSR
113700000000     C*
113800000000     C                   MOVEL     'S'           ROKDEL                         *RECORD VALIDO
113900000000     C*
114000000000     C                   ENDSR
114100000000     C*------------------------------------------------------------------------*
114200000000     C*?CHKAMM - CONTROLLO RECORD CON I PARAMETRI DELLA "AMMINISTRAZIONE"
114300000000     C*------------------------------------------------------------------------*
114400000000     C     CHKAMM        BEGSR
114500000000     C*
114600000000     C                   MOVEL     'S'           ROKAMM                         *RECORD VALIDO
114700000000     C*
114800000000     C                   ENDSR
114900050118     C*------------------------------------------------------------------------*
115000050118     C*?CICSTAP - CICLO MEMORIZZAZIONE SALDI PER LE STATISTICHE PESI
115100050118     C*------------------------------------------------------------------------*
115200050118     C     CICSTAP       BEGSR
115300050118     C*
115400050118     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
115500050118     C                   EXSR      SETFIP
115600050118     C*
115700050118     C* CICLO FINO A FINE FILE SALDI CLIENTE
115800050118DO  1C     $FINPE        DOWEQ     '0'
115900050118     C*
116000050118     C* MEMORIZZA I SALDI CLIENTE
116100050118     c                   z-add     1             nrPEV
116200050118     C                   EXSR      MEMPEV
116300050118     C*
116400050118     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
116500050118     C                   EXSR      LETFIP
116600050118     C*
116700050118E   1C                   ENDDO
116800050118     C*
116900050118     C                   ENDSR
117000050118     C*------------------------------------------------------------------------*
117100050118     C*?SETFIP - POSIZIONAMENTO E PRIMA LETTURA SALDI PESI CLIENTE
117200050118     C*------------------------------------------------------------------------*
117300050118     C     SETFIP        BEGSR
117400050118     C*
117500050118     C                   MOVEL     '0'           $FINPE
117600050118     C*
117700050118     C                   Z-ADD     DEPKSC        KPEKSC                         *DEPOSITO CLIENTE
117800050118     C                   Z-ADD     ANNINI        KPEANN                         *ANNO INIZIALE
117900050118     C                   Z-ADD     MESINI        KPEMES                         *MESE INIZIALE
118000050118     C*
118100050118     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
118200050118     C     KEYPES        SETLL     SIPES01L                           99        *FINE ARCHIVIO
118300050118     C*
118400050118IF  1C     *IN99         IFEQ      '1'
118500050118     C                   MOVEL     '1'           $FINPE
118600050118X   1C                   ELSE
118700050118     C                   EXSR      LETFIP                                       *LETTURA RECORD
118800050118E   1C                   ENDIF
118900050118     C*
119000050118     C                   ENDSR
119100050118     C*------------------------------------------------------------------------*
119200050118     C*?LETFIP - LETTURA PROSSIMO RECORD FILE SALDI PESI CLIENTE
119300050118     C*------------------------------------------------------------------------*
119400050118     C     LETFIP        BEGSR
119500050118     C*
119600050118     C* LEGGO FINO A:
119700050118     C*     - FINE FILE
119800050118     C*     - TROVATO RECORD VALIDO
119900050118     C                   MOVEL     'N'           WRECOK
120000050118DO  1C     $FINPE        DOWEQ     '0'
120100050118     C     WRECOK        ANDEQ     'N'
120200050118     C*
120300050118     C     KPEKSC        READE     SIPES01L                               99
120400050118     C*
120500050118IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
120600050118     C                   MOVEL     '1'           $FINPE
120700050118X   2C                   ELSE
120800050118     C                   EXSR      CHKREP                                       *CONTROLLO RECORD
120900050118E   2C                   ENDIF
121000050118     C*
121100050118E   1C                   ENDDO
121200050118     C*
121300050118     C                   ENDSR
121400050118     C*------------------------------------------------------------------------*
121500050118     C*?CHKREP - CONTROLLA VALIDITA' RECORD SALDI PESI CLIENTE
121600050118     C*------------------------------------------------------------------------*
121700050118     C     CHKREP        BEGSR
121800050118     C*
121900050118     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
122000050118     C*--------------------
122100050118     C* DATA SALDI PESI
122200050118     C*--------------------
122300050118     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
122400050118IF  1C     pesDAT        IFGE      D60DAT
122500050118     C     pesDAT        ANDLE     D60DA2
122600050118X   1C                   ELSE
122700050118     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
122800050118IF  2C     pesDAT        IFGT      D60DA2
122900050118     C                   MOVEL     '1'           $FINPE                         *ESCO DAL CICLO "SDP
123000050118E   2C                   ENDIF
123100050118E   1C                   ENDIF
123200050118     C*--------------------
123300050118     C* CODICE TARIFFA
123400050118     C*--------------------
123500050118     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
123600050118IF  1C     SELCTR        IFNE      'T'
123700050118     C                   MOVEL     pesCTR        $CTR
123800050118IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
123900050118     C     $CTR          OREQ      D60CT2
124000050118     C     $CTR          OREQ      D60CT3
124100050118     C     $CTR          OREQ      D60CT4
124200050118     C     $CTR          OREQ      D60CT5
124300050118     C     $CTR          OREQ      D60CT6
124400050118     C     $CTR          OREQ      D60CT7
124500050118     C     $CTR          OREQ      D60CT8
124600050118     C     $CTR          OREQ      D60CT9
124700050118     C     $CTR          OREQ      D60CT0
124800050118X   2C                   ELSE
124900050118     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
125000050118E   2C                   ENDIF
125100050118E   1C                   ENDIF
125200050118     C*
125300050118     C                   ENDSR
125400050118     C*------------------------------------------------------------------------*
125500050118     C*?CICSTAV - CICLO MEMORIZZAZIONE SALDI PER LE STATISTICHE VOLUMI
125600050118     C*------------------------------------------------------------------------*
125700050118     C     CICSTAV       BEGSR
125800050118     C*
125900050118     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
126000050118     C                   EXSR      SETFIV
126100050118     C*
126200050118     C* CICLO FINO A FINE FILE SALDI CLIENTE
126300050118DO  1C     $FINVL        DOWEQ     '0'
126400050118     C*
126500050118     C* MEMORIZZA I SALDI CLIENTE
126600050118     c                   z-add     2             nrPEV
126700050118     C                   EXSR      MEMPEV
126800050118     C*
126900050118     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
127000050118     C                   EXSR      LETFIV
127100050118     C*
127200050118E   1C                   ENDDO
127300050118     C*
127400050118     C                   ENDSR
127500050118     C*------------------------------------------------------------------------*
127600050118     C*?SETFIV - POSIZIONAMENTO E PRIMA LETTURA SALDI VOLUMI CLIENTE
127700050118     C*------------------------------------------------------------------------*
127800050118     C     SETFIV        BEGSR
127900050118     C*
128000050118     C                   MOVEL     '0'           $FINVL
128100050118     C*
128200050118     C                   Z-ADD     DEPKSC        KVLKSC                         *DEPOSITO CLIENTE
128300050118     C                   Z-ADD     ANNINI        KVLANN                         *ANNO INIZIALE
128400050118     C                   Z-ADD     MESINI        KVLMES                         *MESE INIZIALE
128500050118     C*
128600050118     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
128700050118     C     KEYVLM        SETLL     SIVLM01L                           99        *FINE ARCHIVIO
128800050118     C*
128900050118IF  1C     *IN99         IFEQ      '1'
129000050118     C                   MOVEL     '1'           $FINVL
129100050118X   1C                   ELSE
129200050118     C                   EXSR      LETFIV                                       *LETTURA RECORD
129300050118E   1C                   ENDIF
129400050118     C*
129500050118     C                   ENDSR
129600050118     C*------------------------------------------------------------------------*
129700050118     C*?LETFIV - LETTURA PROSSIMO RECORD FILE SALDI PESI CLIENTE
129800050118     C*------------------------------------------------------------------------*
129900050118     C     LETFIV        BEGSR
130000050118     C*
130100050118     C* LEGGO FINO A:
130200050118     C*     - FINE FILE
130300050118     C*     - TROVATO RECORD VALIDO
130400050118     C                   MOVEL     'N'           WRECOK
130500050118DO  1C     $FINVL        DOWEQ     '0'
130600050118     C     WRECOK        ANDEQ     'N'
130700050118     C*
130800050118     C     KVLKSC        READE     SIVLM01L                               99
130900050118     C*
131000050118IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
131100050118     C                   MOVEL     '1'           $FINVL
131200050118X   2C                   ELSE
131300050118     C                   EXSR      CHKVOL                                       *CONTROLLO RECORD
131400050118E   2C                   ENDIF
131500050118     C*
131600050118E   1C                   ENDDO
131700050118     C*
131800050118     C                   ENDSR
131900050118     C*------------------------------------------------------------------------*
132000050118     C*?CHKVOL - CONTROLLA VALIDITA' RECORD SALDI VOLUMI CLIENTE
132100050118     C*------------------------------------------------------------------------*
132200050118     C     CHKVOL        BEGSR
132300050118     C*
132400050118     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
132500050118     C*--------------------
132600050118     C* DATA SALDI VOLUMI
132700050118     C*--------------------
132800050118     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
132900050118IF  1C     vlmDAT        IFGE      D60DAT
133000050118     C     vlmDAT        ANDLE     D60DA2
133100050118X   1C                   ELSE
133200050118     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
133300050118IF  2C     vlmDAT        IFGT      D60DA2
133400050118     C                   MOVEL     '1'           $FINVL                         *ESCO DAL CICLO "SDP
133500050118E   2C                   ENDIF
133600050118E   1C                   ENDIF
133700050118     C*--------------------
133800050118     C* CODICE TARIFFA
133900050118     C*--------------------
134000050118     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
134100050118IF  1C     SELCTR        IFNE      'T'
134200050118     C                   MOVEL     vlmCTR        $CTR
134300050118IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
134400050118     C     $CTR          OREQ      D60CT2
134500050118     C     $CTR          OREQ      D60CT3
134600050118     C     $CTR          OREQ      D60CT4
134700050118     C     $CTR          OREQ      D60CT5
134800050118     C     $CTR          OREQ      D60CT6
134900050118     C     $CTR          OREQ      D60CT7
135000050118     C     $CTR          OREQ      D60CT8
135100050118     C     $CTR          OREQ      D60CT9
135200050118     C     $CTR          OREQ      D60CT0
135300050118X   2C                   ELSE
135400050118     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
135500050118E   2C                   ENDIF
135600050118E   1C                   ENDIF
135700050118     C*
135800050118     C                   ENDSR
135900000000     C*------------------------------------------------------------------------*
136000000000     C*?CICAFF - CICLO MEMORIZZAZIONE SALDI SOLO PER LA "AFFIDABILITA' SPEDIZ"
136100000000     C*------------------------------------------------------------------------*
136200000000     C     CICAFF        BEGSR
136300000000     C*
136400000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE AFFIDABILITA'
136500000000     C                   EXSR      SETFI2
136600000000     C*
136700000000     C* CICLO FINO A FINE FILE SALDI AFFIDABILITA' CLIENTE
136800000000DO  1C     $FINA         DOWEQ     '0'
136900000000     C*
137000000000     C* MEMORIZZA I SALDI AFFIDABILITA' CLIENTE
137100000000     C                   EXSR      MEMSDA
137200000000     C*
137300000000     C* LETTURA SALDI AFFIDABILITA' CLIENTE SUCCESSIVO
137400000000     C                   EXSR      LETFI2
137500000000E   1C                   ENDDO
137600000000     C*
137700000000     C                   ENDSR
137800000000     C*------------------------------------------------------------------------*
137900000000     C*?SETFI2 - POSIZIONAMENTO E PRIMA LETTURA SALDI AFFIDABILITA' CLIENTE
138000000000     C*------------------------------------------------------------------------*
138100000000     C     SETFI2        BEGSR
138200000000     C*
138300000000     C* REIMPOSTO IL FINE FILE PER CICLO SALDI AFFIDABILITA' CLIENTE
138400000000     C                   MOVEL     '0'           $FINA
138500000000     C*
138600000000     C                   Z-ADD     DEPKSC        KSAKSC                         *DEPOSITO CLIENTE
138700000000     C                   Z-ADD     ANNINI        KSAANN                         *ANNO INIZIALE
138800000000     C                   Z-ADD     MESINI        KSAMES                         *MESE INIZIALE
138900000000     C*
139000000000     C* POSIZIONAMENTO FILE SALDI AFFID.TA' CLIENTE (KSC+ANN+MES)
139100990609     C     KEYSDA        SETLL     SISDA01L                           99        *FINE ARCHIVIO
139200000000     C*
139300000000IF  1C     *IN99         IFEQ      '1'
139400000000     C                   MOVEL     '1'           $FINA
139500000000X   1C                   ELSE
139600000000     C                   EXSR      LETFI2                                       *LETTURA RECORD
139700000000E   1C                   ENDIF
139800000000     C*
139900000000     C                   ENDSR
140000000000     C*------------------------------------------------------------------------*
140100000000     C*?LETFI2 - LETTURA PROSSIMO RECORD FILE SALDI AFFIDABILITA' CLIENTE
140200000000     C*------------------------------------------------------------------------*
140300000000     C     LETFI2        BEGSR
140400000000     C*
140500000000     C* LEGGO FINO A:
140600000000     C*     - FINE FILE
140700000000     C*     - TROVATO RECORD VALIDO
140800000000     C                   MOVEL     'N'           WRECOK
140900000000DO  1C     $FINA         DOWEQ     '0'
141000000000     C     WRECOK        ANDEQ     'N'
141100000000     C*
141200990609     C     KSAKSC        READE     SISDA01L                               99
141300000000     C*
141400000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
141500000000     C                   MOVEL     '1'           $FINA
141600000000X   2C                   ELSE
141700000000     C                   EXSR      VALSDA                                       *VALORIZZA CON "SDA"
141800000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
141900030520     C*
142000030520     C* CONTROLLO SE IL RECORD E' VALORIZZATO
142100030520IF  3C     SDANSP        IFEQ      *ZEROS
142200030520     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
142300030520E   3C                   ENDIF
142400030520     c*
142500000000E   2C                   ENDIF
142600000000E   1C                   ENDDO
142700000000     C*
142800000000     C                   ENDSR
142900000000     C*------------------------------------------------------------------------*
143000000000     C*?VALSDA - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDA"
143100000000     C*------------------------------------------------------------------------*
143200000000     C     VALSDA        BEGSR
143300000000     C*
143400000000     C                   MOVEL     'SDA'         $SDX                           *FILE DA CONTROLLARE
143500990601     C                   Z-ADD     SDADAT        $DAT                           *DATA (AAAA/MM)
143600000000     C                   MOVEL     SDATSP        $TSP                           *TIPO SERVIZIO
143700000000     C                   MOVEL     *BLANKS       $TPO                           *TIPO PORTO
143800000000     C***                  MOVEL*BLANKS   $TPR             *TIPO PRODOTTO
143900000000     C                   MOVEL     *BLANKS       $CTR                           *CODICE TARIFFA
144000000000     C                   MOVEL     *BLANKS       $FPE                           *FASCIA DI PESO
144100000000     C                   MOVEL     *BLANKS       $RBL                           *RAGGRUPPAMENTO BOLL
144200000000     C*
144300000000     C                   ENDSR
144400000000     C*------------------------------------------------------------------------*
144500990609     C*?CHKAFF - CONTROLLO RECORD DI SISDA00F
144600000000     C*------------------------------------------------------------------------*
144700000000     C     CHKAFF        BEGSR
144800000000     C*
144900000000     C                   MOVEL     'S'           ROKAFF                         *RECORD VALIDO
145000000000     C*
145100000000     C                   ENDSR
145200000000     C*------------------------------------------------------------------------*
145300000000     C*?NEWKSC - OPERAZIONI PER NUOVO CLIENTE
145400000000     C*------------------------------------------------------------------------*
145500000000     C     NEWKSC        BEGSR
145600050308     C*
145700050308      *  Cliente con dati
145800050308     C                   MOVEL     '0'           Con_Dati          1            Se contribuisce ai d
145900950725     C*
146000950725     C* SE VENGO DA STATISTICHE PER UN SOLO CLIENTE, LETTO QUESTO ESCO DAL CICLO
146100950725     C* PRINCIPALE DI RICERCA CLIENTI VALIDI
146200950725IF  1C     D60OP0        IFEQ      'VIS'
146300000817     C     D60SCE        ANDEQ     '1'
146400950725     C                   MOVEL     '1'           $FINO                          *FINE CICLO CLIENTI
146500950725E   1C                   ENDIF
146600950725     C*
146700950725     C* SE PROVIENE DALLA RICHIESTA STATISTICHE PER PIU' CLIENTI INSIEME, NON
146800950725     C* AZZERA LE VARIABILI DI LAVORO E LE SCHIERE
146900020826IF  1C*--- D60OP0        IFEQ      'STA'
147000020826     C*--- D60OP0        OREQ      'VIS'
147100020826     C*--- D60SCE        ANDEQ     '1'
147200020826     C*
147300020826     C* NON AZZERA LE VARIABILI DI LAVORO E LE SCHIERE SE -->
147400020826     C* RICHIESTA STATISTICA PER PIU' CLIENTI INSIEME
147500020826     C* RICHIESTA STATISTICA PER CLIENTE UNIFICANTE E NON STA "ELABORANDO" IL PADRE
147600020826IF  0C                   IF        (D60SCE ='1' OR D60SCE='4') OR
147700020826     C                             (D60SCE ='3' OR D60SCE='5') AND
147800020826     C                             (PRIKSC = DEPKSC)
147900020826     C*
148000020826     C* REIMPOSTA I PASSAGGI DI MEMORIZZAZIONE DATI
148100000000     C                   MOVEL     '0'           PASKSC                         SALDI CLIENTE
148200000000     C                   MOVEL     '0'           PASREG                         SALDI REG-REG
148300000000     C                   MOVEL     '0'           PASSER                         SALDI SERVIZIO BOLLE
148400000000     C                   MOVEL     '0'           PASAFF                         SALDI AFFIDABILITA'
148500000000     C                   MOVEL     '0'           PASDEL                         SALDI DELTA CLIENTE
148600000000     C                   MOVEL     '0'           PASAMM                         SALDI AMMINISTRAZION
148700020823     C                   MOVEL     '0'           PASDGC                         DATI GENERALI CLIENT
148800000000     C***
148900000000     C* AZZERAMENTO TOTALI CLIENTE
149000000000     C***
149100000000     C*--------------------
149200000000     C* "REGIONE-REGIONE"
149300000000     C*--------------------
149400020520DO  1C     1             DO        500           J
149500000000     C     J             OCCUR     CRER
149600020605     C                   CLEAR                   REX                            *REGIONE-REGIONE
149700950206     C                   CLEAR                   REC                            *COLLI
149800950206     C                   CLEAR                   REP                            *PESO
149900950206     C                   CLEAR                   RER                            *RICAVI
150000950206     C                   CLEAR                   RES                            *SPEDIZIONI
150100950206     C                   CLEAR                   REV                            *VOLUME
150200000000E   1C                   ENDDO
150300000000     C*--------------------
150400000000     C* "SERVIZIO BOLLE"
150500000000     C*--------------------
150600000000DO  1C     1             DO        5             J
150700000000     C     J             OCCUR     CSER
150800000000     C                   CLEAR                   SNC                            *CORRIERE
150900000629     C                   CLEAR                   SDC                            *DISTRIBUZIONE
151000000000     C                   CLEAR                   SEC                            *ESPRESSO
151100000629     C                   CLEAR                   SPC                            *POSTE
151200091013     C                   CLEAR                   SHC                            *H1030
151300000000E   1C                   ENDDO
151400040317     C*--------------------
151500040317     C* "SERVIZIO BOLLE" DA SISDE
151600040317     C*--------------------
151700040317     C                   CLEAR                   COR
151800040318     C                   CLEAR                   BAN
151900040318     C                   CLEAR                   BNB
152000000000     C*--------------------
152100000000     C* "AFFIDABILITA' SPEDIZIONI"
152200000000     C*--------------------
152300000000DO  1C     1             DO        PARO          J
152400000000     C     J             OCCUR     CAFF
152500000000     C                   CLEAR                   CNC                            *CORRIERE
152600000629     C                   CLEAR                   CDC                            *DISTRIBUZIONE
152700000000     C                   CLEAR                   CEC                            *ESPRESSO
152800091214     C                   CLEAR                   CHC                            *ESPRESSO h1030
152900000629     C                   CLEAR                   CPC                            *POSTE
153000000000E   1C                   ENDDO
153100000000     C*--------------------
153200000000     C* "DELTA CLIENTE"
153300000000     C*--------------------
153400000000DO  1C     1             DO        PARF          J
153500000000     C     J             OCCUR     CDEL
153600000000     C                   CLEAR                   DRC                            *RICAVI REALI
153700000000     C                   CLEAR                   DPC                            *RICAVI PRESUNTI
153800000000     C                   CLEAR                   DSC                            *SPEDIZIONI
153900000000E   1C                   ENDDO
154000000000     C*--------------------
154100000000     C* "AMMINISTRAZIONE"
154200000000     C*--------------------
154300050118DO  1C     1             DO        50            J
154400000000     C     J             OCCUR     CAMM
154500000000     C                   CLEAR                   ARI                            *RICAVI
154600000000     C                   CLEAR                   ASP                            *SPEDIZIONI
154700000000E   1C                   ENDDO
154800000000     C*
154900000000     C                   CLEAR                   AFC                            *FATTURATO
155000000000     C                   CLEAR                   ANC                            *NOTE DI CREDITO
155100000000     C                   CLEAR                   ARC                            *RETTIFICHE
155200000000     C*
155300000000     C* REIMPOSTO CONTATORE PARTICOLARITA' SALDI CLIENTE
155400000000     C                   Z-ADD     *ZEROS        P
155500950207     C                   MOVEL     *BLANKS       VAR
155600000000     C*--------------------
155700000000     C* RAGGRUPPAMENTI CLIENTE
155800000000     C*--------------------
155900950725     C                   CLEAR                   REG                            *P/A + CODICE+CODICE
156000950725     C                   CLEAR                   RE2                            *CODICE+CODICE
156100020823     C                   Z-ADD     *ZEROS        R
156200020826E   0C                   ENDIF
156300000000     C*
156400000000     C                   ENDSR
156500000000     C*------------------------------------------------------------------------*
156600990609     C*?MEMSDC - MEMORIZZA I SALDI DI SISDC01L
156700000000     C*          INDICI:  J(4,0) - FASCIA(2)
156800000000     C*------------------------------------------------------------------------*
156900000000     C     MEMSDC        BEGSR
157000000000     C*
157100000000     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
157200000000     C* PER IL CONTEGGIO IN STAMPA DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
157300000000     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
157400000000     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
157500000000     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
157600000000     C*
157700000000     C* MI SALVO LE NOTE E LE RETTIFICHE PER LA "DELTA" NEL CASO VENGANO AZZ.TE
157800000000     C                   Z-ADD     SDCIRR        DELIRR                         RETTIFICHE
157900000000     C                   Z-ADD     SDCINC        DELINC                         NOTE ACCREDITO
158000000000     C*
158100000000     C                   Z-ADD     1             JI
158200000000     C                   SETOFF                                           28
158300000000     C     SDCRBL        LOOKUP    FLAB(JI)                               28
158400000000     C* BOLLA NON VALIDA COME N°SPEDIZIONE
158500000000IF  1C     FSP(JI)       IFNE      'S'
158600000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
158700000000     C                   Z-ADD     *ZEROS        SDCPKG                         *PESO
158800020828     C                   Z-ADD     *ZEROS        SDCPCC                         *PESO CON CML
158900020828     C                   Z-ADD     *ZEROS        SDCPLS                         *PESO CML
159000000000     C                   Z-ADD     *ZEROS        SDCNCO                         *NUMERO COLLI
159100000000     C                   Z-ADD     *ZEROS        SDCVOL                         *VOLUME TOTALE
159200000000     C                   Z-ADD     *ZEROS        SDCVCC                         *VOLUME CON CML
159300000000     C                   Z-ADD     *ZEROS        SDCVLS                         *VOLUME CML
159400000000E   1C                   ENDIF
159500000000     C* BOLLA NON VALIDA COME RICAVO
159600000000IF  1C     FRI(JI)       IFNE      'S'
159700000000     C                   Z-ADD     *ZEROS        SDCIRB                         RICAVO REALE BOLLE
159800000000     C                   Z-ADD     *ZEROS        SDCIRR                         RETTIFICHE
159900000000     C                   Z-ADD     *ZEROS        SDCINC                         NOTE ACCREDITO
160000000000E   1C                   ENDIF
160100000000     C*
160200000000     C* RICAVO NETTO = RICAVO DA BOLLE + RETTIFICHE + NOTE DI CREDITO
160300000000     C                   Z-ADD     *ZEROS        RICTOT           16 3
160400000000     C     SDCIRB        ADD       SDCIRR        RICTOT
160500000000     C     SDCINC        ADD       RICTOT        RICTOT                         *RICAVO TOTALE
160600000000     C*
160700000000     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
160800000000     C                   Z-ADD     *ZEROS        RICDEL           16 3
160900000000     C     SDCIRD        ADD       DELIRR        RICDEL
161000000000     C     DELINC        ADD       RICDEL        RICDEL                         *RICAVO REALE DELTA
161100000000     C***
161200000000     C* V A L O R I Z Z A Z I O N I
161300000000     C***
161400000000     C* MESE
161500000000     C                   Z-ADD     1             MM
161600000000     C                   SETOFF                                           28
161700000000     C     SDCMES        LOOKUP    MES(MM)                                28
161800000000IF  1C     *IN28         IFEQ      *ON                                          *MESE TROVATO
161900000000     C*--------------------
162000000000     C* R E G I O N E - R E G I O N E
162100000000     C*--------------------
162200000000IF  2C     D60ST1        IFEQ      'S'                                          *SE STATISTICA SELEZ
162300000000     C     ROKREG        ANDEQ     'S'                                          *SE PASSATO AI CONTR
162400000000     C*
162500000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
162600050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
162700000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
162800000000     C                   MOVEL     '1'           PASREG                         *PER REGIONE
162900000000     C*
163000000000     C* RAGGRUPPAMENTO + RAGGRUPPAMENTO
163100950725     C                   MOVEL     *ZEROS        WRER              6
163200950726     C                   MOVEL     SDCREP        WRER                           *PAR+...
163300000000     C                   MOVE      SDCREA        WRER                           *PAR+ARR
163400950725     C                   MOVEL     *ZEROS        WPARER            7
163500950725     C                   MOVEL     'P'           WPARER                         *P+...+...
163600950725     C                   MOVE      WRER          WPARER                         *P+PAR+ARR
163700971128     C*
163800950725     C* CONTROLLA ESISTENZA
163900950725     C                   MOVE      REG           RE2                             *PRENDE SOLO CODICI
164000000000     C                   Z-ADD     1             J
164100000000     C                   SETOFF                                           28
164200950725     C     WRER          LOOKUP    RE2(J)                                 28
164300950725IF  3C     *IN28         IFEQ      *ON
164400000000     C                   Z-ADD     J             COMBIN                         *COMBIN GIA' PRESENT
164500950725X   3C                   ELSE
164600000000     C                   ADD       1             R                              *NON USARE ALTROVE
164700950725     C                   MOVEL     WPARER        REG(R)
164800000000     C                   Z-ADD     R             COMBIN                         *COMBIN NON PRESENTE
164900950725E   3C                   ENDIF
165000000000     C* O C U R
165100000000     C     COMBIN        OCCUR     CRER
165200020605     C                   MOVEL     WRER          REX                            *COMBINAZIONE REP+RE
165300950725     C                   ADD       SDCNCO        REC(MM)                        *COLLI
165400950725     C                   ADD       SDCPKG        REP(MM)                        *PESO
165500950725     C                   ADD       RICTOT        RER(MM)                        *RICAVI
165600950725     C                   ADD       SDCNSP        RES(MM)                        *SPEDIZIONI
165700950725     C                   ADD       SDCVOL        REV(MM)                        *VOLUME
165800950725E   2C                   ENDIF
165900000000     C*--------------------
166000000000     C* T O T A L I
166100000000     C*--------------------
166200000000IF  2C     D60ST2        IFEQ      'S'                                          *SE STATISTICA SELEZ
166300000000     C     ROKSER        ANDEQ     'S'                                          *SE PASSATO AI CONTR
166400000000     C*
166500000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
166600050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
166700000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
166800000000     C                   MOVEL     '1'           PASSER                         *PER SERVIZIO BOLLE
166900000000     C*
167000000000DO  3C     1             DO        5             J
167100000000     C*
167200000000IF  4C     J             IFEQ      1                                            *SPEDIZIONI°
167300950206     C                   Z-ADD     SDCNSP        VALORE           16 3
167400000000E   4C                   ENDIF
167500000000IF  4C     J             IFEQ      2                                            *COLLI°
167600000000     C                   Z-ADD     SDCNCO        VALORE
167700000000E   4C                   ENDIF
167800000000IF  4C     J             IFEQ      3                                            *PESO°
167900000000     C                   Z-ADD     SDCPKG        VALORE
168000000000E   5C                   ENDIF
168100000000IF  4C     J             IFEQ      4                                            *VOLUME°
168200000000     C                   Z-ADD     SDCVOL        VALORE
168300000000E   4C                   ENDIF
168400000000IF  4C     J             IFEQ      5                                            *RICAVI°
168500000000     C                   Z-ADD     RICTOT        VALORE
168600000000E   4C                   ENDIF
168700000000     C* O C U R
168800000000     C     J             OCCUR     CSER
168900000000     C*
169000000000IF  4C     SDCTSP        IFEQ      'C'
169100000000     C                   ADD       VALORE        SNC(MM)                        *CORRIERE
169200000000E   4C                   ENDIF
169300000000     C*
169400000000IF  4C     SDCTSP        IFEQ      'D'
169500000629     C                   ADD       VALORE        SDC(MM)                        *DISTRIBUZIONE
169600000000E   4C                   ENDIF
169700000000     C*
169800000000IF  4C     SDCTSP        IFEQ      'E'
169900000000     C                   ADD       VALORE        SEC(MM)                        *ESPRESSO
170000000000E   4C                   ENDIF
170100091013     C*
170200091013IF  4C     SDCTSP        IFEQ      'H'
170300091013     C                   ADD       VALORE        SHC(MM)                        *ESPR.10:30
170400091013E   4C                   ENDIF
170500000000     C*
170600000629IF  4C     SDCTSP        IFEQ      'P'
170700000629     C                   ADD       VALORE        SPC(MM)                        *POSTE
170800000629E   4C                   ENDIF
170900000629     C*
171000000000E   3C                   ENDDO
171100000000E   2C                   ENDIF
171200000000     C*--------------------
171300000000     C* D E L T A
171400000000     C*--------------------
171500000000IF  2C     D60ST4        IFEQ      'S'                                          *SE STATISTICA SELEZ
171600000000     C     ROKDEL        ANDEQ     'S'                                          *SE PASSATO AI CONTR
171700000309     C*
171800000309     C                   MOVEL     SDCANN        DATSDC            6 0
171900000309     C                   MOVE      SDCMES        DATSDC
172000000000     C*
172100000309     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
172200000309IF  3C     SDCFPE        IFEQ      '07'                                         *NO > 5000
172300000309     C     DATSDC        ANDLT     200001                                       *PRIMA DEL INIZIO GE
172400000309     C                   ELSE
172500050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
172600000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
172700000000     C                   MOVEL     '1'           PASDEL                         *PER DELTA CLIENTE
172800000000     C*
172900000000     C* ELABORO LA FASCIA SOLO SE SELEZIONATA
173000000000     C                   MOVEL     SDCFPE        FASCIA            2
173100000000     C                   Z-ADD     1             J
173200000000     C                   SETOFF                                           28
173300000000     C     FASCIA        LOOKUP    SFPE(J)                                28
173400000000IF  4C     *IN28         IFEQ      *ON
173500000000     C* O C U R
173600000000     C     J             OCCUR     CDEL                                         *F A S C I A
173700000000     C                   ADD       RICDEL        DRC(MM)                        *RICAVI REALI DELTA
173800000000     C                   ADD       SDCIRP        DPC(MM)                        *RICAVI PRESUNTI
173900000000     C                   ADD       SDCNSP        DSC(MM)                        *SPEDIZIONI°
174000000000E   4C                   ENDIF
174100000000E   3C                   ENDIF
174200000000E   2C                   ENDIF
174300000000     C*--------------------
174400000000     C* A M M I N I S T R A Z I O N E
174500000000     C*--------------------
174600990609     C* PRIMA DEL 97 -> FATTURATO = RICAVI SISDC + SISDP
174700990609     C* DOPO   IL 97 -> FATTURATO = RICAVI SISDP
174800000000IF  2C     D60ST5        IFEQ      'S'                                          *SE STATISTICA SELEZ
174900000000     C     ROKAMM        ANDEQ     'S'                                          *SE PASSATO AI CONTR
175000000000     C*
175100000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
175200050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
175300000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
175400000000     C                   MOVEL     '1'           PASAMM                         *PER AMMINISTRAZIONE
175500000000     C                   ADD       SDCINC        ANC(MM)                        *NOTE DI CREDITO
175600000000     C                   ADD       SDCIRR        ARC(MM)                        *RETTIFICHE AI RICAV
175700990602IF  3C     SDCANN        IFLT      1997
175800970724     C                   ADD       SDCIRB        AFC(MM)                        *FATTURATO LORDO
175900970724E   3C                   ENDIF
176000970724E   2C                   ENDIF
176100000000E   1C                   ENDIF
176200000000     C*
176300000000     C                   ENDSR
176400040317     C*------------------------------------------------------------------------*
176500040317     C*?MEMSDE - MEMORIZZA I SALDI DI SISDE02L
176600040317     C*------------------------------------------------------------------------*
176700040317     C     MEMSDE        BEGSR
176800040317     C*
176900040317     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
177000040317     C* PER IL CONTEGGIO IN STAMPA DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
177100040317     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
177200040317     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
177300040317     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
177400040317     C*
177500040318     C* VALORIZZA IL CAMPONE SDEUNI CON LA DS BAN o BNB
177600040318     c                   if        wtrd = 'BNB'
177700040317     C                   MOVEL     SDEUNI        DAR5BNBSDE
177800040318     C                   CLEAR                   DAR5BANSDE
177900040318     c                   else
178000040318     C                   MOVEL     SDEUNI        DAR5BANSDE
178100040318     C                   CLEAR                   DAR5BNBSDE
178200040318     c                   endif
178300040317     C*
178400040317     C                   Z-ADD     1             JI
178500040317     C                   SETOFF                                           28
178600040317     C     SDERBL        LOOKUP    FLAB(JI)                               28
178700040317     C* BOLLA NON VALIDA COME N°SPEDIZIONE
178800040317IF  1C     FSP(JI)       IFNE      'S'
178900040318     C                   Z-ADD     *ZEROS        §BNBBCOR                       *PESO
179000040318     C                   Z-ADD     *ZEROS        §BNBBNBA                       *PESO
179100040318     C                   Z-ADD     *ZEROS        §BNBBNB2                       *PESO
179200040318     C                   Z-ADD     *ZEROS        §BANNBA                        *PESO
179300040318     C                   Z-ADD     *ZEROS        §BANNB2                        *PESO
179400040317E   1C                   ENDIF
179500040317     C* BOLLA NON VALIDA COME RICAVO
179600040317IF  1C     FRI(JI)       IFNE      'S'
179700040317E   1C                   ENDIF
179800040317     C***
179900040317     C* V A L O R I Z Z A Z I O N I
180000040317     C***
180100040317     C* MESE
180200040317     C                   Z-ADD     1             MM
180300040317     C                   SETOFF                                           28
180400040317     C     SDEMES        LOOKUP    MES(MM)                                28
180500040317IF  1C     *IN28         IFEQ      *ON                                          *MESE TROVATO
180600040317     C*--------------------
180700040317     C* T O T A L I
180800040317     C*--------------------
180900040317IF  2C     D60ST2        IFEQ      'S'                                          *SE STATISTICA SELEZ
181000040317     C     ROKSER        ANDEQ     'S'                                          *SE PASSATO AI CONTR
181100040317     C*
181200040317     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
181300050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
181400040317     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
181500040317     C                   MOVEL     '1'           PASSER                         *PER SERVIZIO BOLLE
181600040317     C*
181700040318     C* MEMORIZZA COLLI ORIGINALI e BANCALI x tipo record BNB
181800040318     c                   if        wtrd = 'BNB'
181900040318     C                   ADD       §bnbBCOR      COR(MM)                        *POSTE
182000040318     C                   ADD       §bnbBnba      bnb(MM)                        *POSTE
182100040318     C                   ADD       §bnbBnb2      bnb(MM)                        *POSTE
182200040318     c                   else
182300040318     C* MEMORIZZA BANCALI x tipo record BAN
182400040318     C                   ADD       §bannba       ban(MM)                        *POSTE
182500040318     C                   ADD       §bannb2       ban(MM)                        *POSTE
182600040319     c                   endif
182700040317E   2C                   ENDIF
182800040317E   1C                   ENDIF
182900040317     C*
183000040317     C                   ENDSR
183100000000     C*------------------------------------------------------------------------*
183200990609     C*?MEMSDA - MEMORIZZA I SALDI DI SISDA01L
183300000000     C*------------------------------------------------------------------------*
183400000000     C     MEMSDA        BEGSR
183500000000     C*--------------------
183600000000     C* A F F I D A B I L I T A '
183700000000     C*--------------------
183800000000IF  1C     D60ST3        IFEQ      'S'                                          *SE STATISTICA SELEZ
183900000000     C     ROKAFF        ANDEQ     'S'                                          *SE PASSATO AI CONTR
184000000000     C*
184100000000     C* MESE
184200000000     C                   Z-ADD     1             MM
184300000000     C                   SETOFF                                           28
184400000000     C     SDAMES        LOOKUP    MES(MM)                                28
184500000000IF  2C     *IN28         IFEQ      *ON                                          *MESE TROVATO
184600000000     C*
184700000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
184800050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
184900000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
185000000000     C                   MOVEL     '1'           PASAFF                         *PER AFFIDABILITA'
185100000000     C*
185200000000IF  3C     SDADLV        IFLT      0
185300000000     C                   Z-ADD     1             J                              *IN ANTICIPO
185400000000X   3C                   ELSE
185500000000IF  4C     SDADLV        IFGT      48
185600000000     C                   Z-ADD     7             J                              *OLTRE 48HH RIT
185700000000X   4C                   ELSE
185800000000     C                   Z-ADD     2             J
185900000000     C                   SETOFF                                           28
186000000000     C     SDADLV        LOOKUP    ORE(J)                                 28
186100000000IF  5C     *IN28         IFEQ      *OFF                                         *NON TROVATO
186200000000     C                   Z-ADD     *ZEROS        J                              *PER DARE ERRORE CPF
186300000000E   5C                   ENDIF
186400000000E   4C                   ENDIF
186500000000E   3C                   ENDIF
186600000000     C* O C U R
186700000000     C     J             OCCUR     CAFF
186800000000     C*
186900000000IF  3C     SDATSP        IFEQ      'C'
187000000000     C                   ADD       SDANSP        CNC(MM)                        *CORRIERE
187100000000E   3C                   ENDIF
187200000000     C*
187300000000IF  3C     SDATSP        IFEQ      'D'
187400000629     C                   ADD       SDANSP        CDC(MM)                        *DISTRIBUZIONE
187500000000E   3C                   ENDIF
187600000000     C*
187700000000IF  3C     SDATSP        IFEQ      'E'
187800000000     C                   ADD       SDANSP        CEC(MM)                        *ESPRESSO
187900000000E   3C                   ENDIF
188000000629     C*
188100091214IF  3C     SDATSP        IFEQ      'H'
188200091214     C                   ADD       SDANSP        CHC(MM)                        *ESPRESSO H1030
188300091214E   3C                   ENDIF
188400091214     C*
188500000629IF  3C     SDATSP        IFEQ      'P'
188600000629     C                   ADD       SDANSP        CPC(MM)                        *POSTE
188700000629E   3C                   ENDIF
188800000000     C*
188900000000E   2C                   ENDIF
189000000000E   1C                   ENDIF
189100000000     C*
189200000000     C                   ENDSR
189300050118     C*------------------------------------------------------------------------*
189400050118     C*?MEMPEV - MEMORIZZA I SALDI DI SIPES01L
189500050118     C*------------------------------------------------------------------------*
189600050118     C     MEMPEV        BEGSR
189700050118     C*--------------------
189800050118     C* A F F I D A B I L I T A '
189900050118     C*--------------------
190000050118IF  1C     D60STp        IFEQ      'S'                                          *SE STATISTICA SELEZ
190100050118     C     ROKpev        ANDEQ     'S'                                          *SE PASSATO AI CONTR
190200050118     C*
190300050118     C* MESE
190400050118     C                   Z-ADD     1             MM
190500050118     C                   SETOFF                                           28
190600050118      *
190700050118     c                   Select
190800050118     c                   when      nrPEV = 1
190900050118     C                   eval      $mes = pesMES
191000050118     c                   when      nrPEV = 2
191100050118     C                   eval      $mes = vlmMES
191200050118     c                   endSL
191300050118      *
191400050118     C     $mes          LOOKUP    MES(MM)                                28
191500050118IF  2C     *IN28         IFEQ      *ON                                          *MESE TROVATO
191600050118     C*
191700050118     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
191800050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
191900050118     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
192000050118     C                   MOVEL     '1'           PASpev                         *PER Peso Volume
192100050118     C* O C U R
192200050118     C     nrPEV         OCCUR     CPEV
192300050118     C*
192400050118     c                   if        nrPEV = 1
192500050118     C                   ADD       pesNCV        NCV(MM)                        *VALORI DEL MESE
192600050118     C                   ADD       pesTCL        NCB(MM)
192700050118     C                   ADD       pesPSC        PVV(MM)
192800050118     C                   ADD       pesPSB        PVB(MM)
192900050118     c                   end
193000050118     C*
193100050118     c                   if        nrPEV = 2
193200050118     C                   ADD       vlmNCR        NCV(MM)                        *VALORI DEL MESE
193300050118     C                   ADD       vlmTCL        NCB(MM)
193400050118     C                   ADD       vlmVLC        PVV(MM)
193500050118     C                   ADD       vlmVLB        PVB(MM)
193600050118     c                   end
193700050118     C*
193800050118E   2C                   ENDIF
193900050118     C*
194000050118E   1C                   ENDIF
194100050118     C*
194200050118     C                   ENDSR
194300000000     C*------------------------------------------------------------------------*
194400000000     C*?CICAMM - CICLO MEMORIZZAZIONE PARTICOLARITA' SALDI PER LA "AMMNINSTR.NE"
194500000000     C*------------------------------------------------------------------------*
194600000000     C     CICAMM        BEGSR
194700000000     C*
194800000000     C* POSIZIONAMENTO E PRIMA LETTURA PARTICOLARITA' SALDI CLIENTE
194900000000     C                   EXSR      SETFI3
195000000000     C*
195100000000     C* CICLO FINO A FINE FILE PARTICOLARITA' SALDI CLIENTE
195200000000DO  1C     $FINP         DOWEQ     '0'
195300000000     C*
195400000000     C* MEMORIZZA LE PARTICLARITA' SALDI DEL CLIENTE
195500000000     C                   EXSR      MEMSDP
195600000000     C*
195700000000     C* LETTURA PARTICOLARITA' SALDI DEL CLIENTE SUCESSIVO
195800000000     C                   EXSR      LETFI3
195900000000     C*
196000000000E   1C                   ENDDO
196100000000     C*
196200000000     C                   ENDSR
196300000000     C*------------------------------------------------------------------------*
196400000000     C*?SETFI3 - POSIZIONAMENTO E PRIMA LETTURA PARTICOLARITA' SALDI CLIENTE
196500000000     C*------------------------------------------------------------------------*
196600000000     C     SETFI3        BEGSR
196700000000     C*
196800000000     C* REIMPOSTO IL FINE FILE PER CICLO PARTICOLARITA' SALDI CLIENTE
196900000000     C                   MOVEL     '0'           $FINP
197000000000     C*
197100000000     C                   Z-ADD     DEPKSC        KSPKSC                         *DEPOSITO CLIENTE
197200000000     C                   Z-ADD     ANNINI        KSPANN                         *ANNO INIZIALE
197300000000     C                   Z-ADD     MESINI        KSPMES                         *MESE INIZIALE
197400000000     C*
197500000000     C* POSIZIONAMENTO FILE PARTICOLARITA' SALDI CLIENTE (KSC+ANN+MES)
197600990609     C     KEYSDP        SETLL     SISDP01L                           99        *FINE ARCHIVIO
197700000000     C*
197800000000IF  1C     *IN99         IFEQ      '1'
197900000000     C                   MOVEL     '1'           $FINP
198000000000X   1C                   ELSE
198100000000     C                   EXSR      LETFI3                                       *LETTURA RECORD
198200000000E   1C                   ENDIF
198300000000     C*
198400000000     C                   ENDSR
198500000000     C*------------------------------------------------------------------------*
198600000000     C*?LETFI3 - LETTURA PROSSIMO RECORD FILE PARTICOLARITA' SALDI CLIENTE
198700000000     C*------------------------------------------------------------------------*
198800000000     C     LETFI3        BEGSR
198900000000     C*
199000000000     C* LEGGO FINO A:
199100000000     C*     - FINE FILE
199200000000     C*     - TROVATO RECORD VALIDO
199300000000     C                   MOVEL     'N'           WRECOK
199400000000DO  1C     $FINP         DOWEQ     '0'
199500000000     C     WRECOK        ANDEQ     'N'
199600000000     C*
199700990609     C     KSPKSC        READE     SISDP01L                               99
199800000000     C*
199900000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
200000000000     C                   MOVEL     '1'           $FINP
200100000000X   2C                   ELSE
200200000000     C                   EXSR      CHKRE3                                       *CONTROLLO RECORD
200300000000E   2C                   ENDIF
200400000000     C*
200500000000E   1C                   ENDDO
200600000000     C*
200700000000     C                   ENDSR
200800000000     C*------------------------------------------------------------------------*
200900000000     C*?CHKRE3 - CONTROLLA VALIDITA' RECORD PARTICOLARITA' SALDI CLIENTE
201000000000     C*------------------------------------------------------------------------*
201100000000     C     CHKRE3        BEGSR
201200000000     C*
201300000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
201400000000     C*--------------------
201500990601     C* DATA (AAAA/MM)
201600000000     C*--------------------
201700000000     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
201800020823IF  1C     SDPDAT        IFGE      D60DAT
201900020823     C     SDPDAT        ANDLE     D60DA2
202000020823X   1C                   ELSE
202100020823     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
202200020823IF  2C     SDPDAT        IFGT      D60DA2
202300020823     C                   MOVEL     '1'           $FINP                          *ESCO DAL CICLO "SDP
202400020823E   2C                   ENDIF
202500020823E   1C                   ENDIF
202600000000     C*--------------------
202700000000     C* CODICE TARIFFA
202800000000     C*--------------------
202900000000     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
203000000000IF  1C     SELCTR        IFNE      'T'
203100000000     C                   MOVEL     SDPCTR        $CTR
203200000000IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
203300000000     C     $CTR          OREQ      D60CT2
203400000000     C     $CTR          OREQ      D60CT3
203500000000     C     $CTR          OREQ      D60CT4
203600000000     C     $CTR          OREQ      D60CT5
203700000000     C     $CTR          OREQ      D60CT6
203800000000     C     $CTR          OREQ      D60CT7
203900000000     C     $CTR          OREQ      D60CT8
204000000000     C     $CTR          OREQ      D60CT9
204100000000     C     $CTR          OREQ      D60CT0
204200000000X   2C                   ELSE
204300000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
204400000000E   2C                   ENDIF
204500000000E   1C                   ENDIF
204600000000     C*
204700000000     C                   ENDSR
204800000000     C*------------------------------------------------------------------------*
204900990609     C*?MEMSDP - MEMORIZZA I SALDI DI SISDP01L
205000000000     C*------------------------------------------------------------------------*
205100000000     C     MEMSDP        BEGSR
205200000000     C* MESE
205300000000     C                   Z-ADD     1             MM
205400000000     C                   SETOFF                                           28
205500000000     C     SDPMES        LOOKUP    MES(MM)                                28
205600000000IF  1C     *IN28         IFEQ      *ON                                          *MESE TROVATO
205700000000     C*
205800000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
205900050308     C                   movel     '1'           Con_Dati                       *Se ha dati x il ris
206000000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
206100000000     C                   MOVEL     '1'           PASAMM                         *PER AMMINISTRAZIONE
206200970724     C*
206300970724     C* MEMORIZZA TUTTE LE PARTICOLARITA' CLIENTE
206400000000     C                   Z-ADD     1             J
206500000000     C                   SETOFF                                           28
206600000000     C     SDPSVR        LOOKUP    VAR(J)                                 28
206700000000IF  2C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATA
206800000000     C                   ADD       1             P
206900000000     C                   MOVEL     SDPSVR        VAR(P)
207000000000     C                   Z-ADD     P             J
207100000000E   2C                   ENDIF
207200000000     C* O C U R
207300000000     C     J             OCCUR     CAMM
207400000000     C                   ADD       SDPRCP        ARI(MM)                        *RICAVI
207500000000     C                   ADD       SDPNSP        ASP(MM)                        *SPEDIZIONI
207600970724     C*
207700990609     C* PRIMA DEL 97 -> FATTURATO = RICAVI SISDC + SISDP
207800990609     C* DOPO   IL 97 -> FATTURATO = RICAVI SISDP
207900990602IF  2C     SDPANN        IFGE      1997                                         *DOLO IL
208000970724     C     AFC(MM)       ADD       SDPRCP        AFC(MM)                        *FATTURATO TOTALE
208100970724X   2C                   ELSE                                                   *PRIMA DEL
208200970724IF  3C     SDPSVR        IFNE      'G'
208300970724     C     SDPSVR        ANDNE     'E'
208400970724     C     SDPSVR        ANDNE     'R'
208500970724     C     SDPSVR        ANDNE     'P'
208600970724     C     SDPSVR        ANDNE     'A'
208700970724     C     SDPSVR        ANDNE     'S'
208800970724     C     SDPSVR        ANDNE     'F'
208900010920     C     SDPSVR        ANDNE     'Q'
209000010920     C     SDPSVR        ANDNE     'K'
209100010920     C     SDPSVR        ANDNE     'B'
209200010920     C     SDPSVR        ANDNE     'U'
209300010921     C     SDPSVR        ANDNE     'J'
209400010920     C     SDPSVR        ANDNE     'X'
209500010920     C     SDPSVR        ANDNE     'Y'
209600020205     C     SDPSVR        ANDNE     'O'
209700040924     C     SDPSVR        ANDNE     'a'
209800020205     C     SDPSVR        ANDNE     '5'
209900970724     C     AFC(MM)       ADD       SDPRCP        AFC(MM)                        *FATTURATO TOTALE
210000970724E   3C                   ENDIF
210100970724E   2C                   ENDIF
210200970724E   1C                   ENDIF
210300000000     C*
210400000000     C                   ENDSR
210500000000     C*------------------------------------------------------------------------*
210600000000     C*?STACLI - IMPOSTA STAMPA STATISTICHE PER IL CLIENTE
210700000000     C*------------------------------------------------------------------------*
210800000000     C     STACLI        BEGSR
210900000000     C*
211000000000     C* STAMPO SOLO DE HO MEMORIZZATO QC PER LE SINGOLE STATISTICHE
211100000000IF  1C     PASSER        IFEQ      '1'
211200000000     C                   EXSR      STASER                                       *TOTALI
211300000000E   1C                   ENDIF
211400000000     C*
211500050118IF  1C     PASPEV        IFEQ      '1'
211600050118     C                   EXSR      STAPEV                                       *PESI-VOLUMI
211700000000E   1C                   ENDIF
211800050118     C*
211900050118IF  1C     PASREG        IFEQ      '1'
212000050118     C                   EXSR      STAREG                                       *REGIONE-REGIONE
212100050118E   1C                   ENDIF
212200000000     C*
212300000000IF  1C     PASDEL        IFEQ      '1'
212400000000     C                   EXSR      STADEL                                       *DELTA
212500000000E   1C                   ENDIF
212600000000     C*
212700000000IF  1C     PASAFF        IFEQ      '1'
212800000000     C                   EXSR      STAAFF                                       *AFFIDABILITA'
212900000000E   1C                   ENDIF
213000000000     C*
213100000000IF  1C     PASAMM        IFEQ      '1'
213200000000     C                   EXSR      STAAMM                                       *AMMINISTRAZIONE
213300000000E   1C                   ENDIF
213400000000     C*
213500000000     C                   ENDSR
213600020524     C*------------------------------------------------------------------------*
213700020524     C*?STATES - STAMPA TESTATA
213800020524     C*------------------------------------------------------------------------*
213900020524     C     STATES        BEGSR
214000020524     C*
214100020524IF  1C     *IN01         IFEQ      *ON                                          *FINE PAGINA
214200020823IF  2C                   IF        D60OP1 <> 'STT'                              *NO PER OTTICO
214300071210      *
214400020524     C                   WRITE     SE62PT
214500071210      *
214600020823E   2C                   ENDIF
214700020823IF  2C                   IF        D60OP1 = 'STT'                               *SI PER OTTICO
214800170801     c                             and  ( CL2ksc <> *zeros
214900170801     c                              or    CL2rag <> *blanks )
215000020823     C                   Z-ADD     DEPPAG        CL2PAG
215100071210      *
215200020823     C                   WRITE     SE62C2
215300170801     c                   eval      $ClienteStampato  = *on
215400170329     C                   z-add     wAA           AR2AAA
215500170329     C                   WRITE     SE62A2
215600071210      *
215700020823     C                   ADD       1             DEPPAG                         *PAGINE PER CLIENTE
215800020823E   2C                   ENDIF
215900020823IF  2C     WSTINT        IFEQ      '1'                                          *INTESTAZIONE DA STA
216000170801     c     CL2ksc        andNE     *zeros
216100170801     c     WStInt        OReq      '1'
216200170801     c     CL2rag        andNE     *blanks
216300071210      *
216400020823     C                   WRITE     SE62PI
216500071210      *
216600020826E   2C                   ENDIF
216700020823E   1C                   ENDIF
216800020524     C*
216900020823     C* DOPO reimposta campi di lavoro
217000020823     C                   MOVEL     '0'           *IN01                            *NO OVERFLW
217100020823     c                   MOVEL     '1'           WSTINT                           *SI STAMPA INTESTA
217200020823     C*
217300020524     C                   ENDSR
217400000000     C*------------------------------------------------------------------------*
217500000000     C*?STASER - IMPOSTA STAMPA STATISTICA "TOTALI"
217600000000     C*          INDICI: J(4,0) --> TIPO VALORE IN LINEA (1÷5)
217700000000     C*                  I(4,0) --> TIPO SERVIZIO SELEZIONATO/MEDIA DA STAMPARE
217800000000     C*------------------------------------------------------------------------*
217900000000     C     STASER        BEGSR
218000071211     C*--------------------          0
218100000000     C* PARAMETRI
218200000000     C*--------------------
218300000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
218400000000     C                   EXSR      STAPAR
218500000000E   1C                   ENDIF
218600000000     C*--------------------
218700000000     C* DATI GENERALI CLIENTE
218800000000     C*--------------------
218900000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
219000000000     C                   EXSR      STADGC
219100000000E   1C                   ENDIF
219200000000     C*--------------------
219300000000     C* INTESTAZIONE
219400000000     C*--------------------
219500000000     C                   MOVEL     DRI(2)        PICSTA
219600071210      *
219700071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
219800071211     c                   eval      Righe_da_stampare = 5
219900071211     c                   exsr      solo_x_STT
220000010920     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
220100000000     C*
220200000000     C* PER OGNI VALORE DA STAMPARE
220300000000     C                   Z-ADD     1             J
220400000000DO  1C     1             DO        5             J
220500000000     C*
220600000000     C* SOMMO TUTTI I TIPI SERVIZIO NELLA SCHIERA MULTIPLA
220700000000     C     J             OCCUR     CSER
220800000000     C     J             OCCUR     WSER
220900000000     C                   Z-ADD     SNC           VAW                            CORRIERE +
221000000629     C     VAW           ADD       SDC           VAW                            DISTRIBUZIONE +
221100000629     C     VAW           ADD       SEC           VAW                            ESPRESSO +
221200091013     C     VAW           ADD       SHC           VAW                            ESPR.10:30 +
221300000629     C     VAW           ADD       SPC           VAW                            POSTE = VALORE TOTAL
221400000000     C*
221500000000     C* METTO I TOTALI DEI SINGOLI VALORI IN SCHIERA (PER LE MEDIE)
221600000000IF  2C     J             IFEQ      1
221700000000     C                   Z-ADD     VAW           SPW                            *SPEDIZIONI
221800000000E   2C                   ENDIF
221900000000IF  2C     J             IFEQ      2
222000000000     C                   Z-ADD     VAW           COW                            *COLLI
222100000000E   2C                   ENDIF
222200000000IF  2C     J             IFEQ      3
222300000000     C                   Z-ADD     VAW           PEW                            *PESO IN Q.LI
222400000000E   2C                   ENDIF
222500000000IF  2C     J             IFEQ      4
222600000000     C                   Z-ADD     VAW           VOW                            *VOLUME IN M.CUBI
222700000000E   2C                   ENDIF
222800000000IF  2C     J             IFEQ      5
222900010921     C                   Z-ADD     VAW           RIW                            *RICAVI
223000000000E   2C                   ENDIF
223100000000     C*
223200000000E   1C                   ENDDO
223300000000     C*
223400000000     C* PER OGNI TIPO VALORE DA STAMPARE (SPEDIZIONI/COLLI/PESO/VOLUME/RICAVI)
223500000000     C                   Z-ADD     1             J
223600000000DO  1C     1             DO        5             J
223700000000     C     J             OCCUR     CSER
223800000000     C*
223900000000     C                   MOVEL     '0'           NODESV            1            *SI DESCRIZ VALORE
224000000000     C                   MOVEL     DCA(J)        DPRIMO                         *DESCRIZIONE VALORE
224100000000     C                   MOVEL     *BLANKS       TIPVAL
224200000000     C*
224300000000IF  2C     J             IFEQ      4
224400000000     C                   MOVEL     'VOL'         TIPVAL            3            *STAMPO I VOLUMI
224500000000E   2C                   ENDIF
224600000000IF  2C     J             IFEQ      5
224700000000     C                   MOVEL     'RIC'         TIPVAL                         *STAMPO I RICAVI
224800000000E   2C                   ENDIF
224900000000     C*
225000000000     C* PER OGNI TIPO SERVIZIO SELEZIONATO
225100000000     C                   Z-ADD     1             I
225200000000DO  2C     1             DO        SPARS         I
225300000000     C*
225400000000IF  3C     I             IFGE      2                                            *TIPO VALORE 1 VOLTA
225500000000     C     NODESV        ANDEQ     '1'                                          *SE SCRITTO 1 VOLTA
225600000000     C                   MOVEL     *BLANKS       DPRIMO
225700000000E   3C                   ENDIF
225800000000     C*
225900000000     C* DESCRIZIONE TIPO SERVIZIO
226000000000     C                   Z-ADD     1             JJ
226100000000     C                   SETOFF                                           28
226200000000     C     SERS(I)       LOOKUP    SER(JJ)                                28
226300000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
226400000000     C                   MOVEL     *BLANKS       DSECON
226500000000X   3C                   ELSE
226600000000     C                   MOVEL     DSER(JJ)      DSECON                         *DESCRIZIONE SERVIZI
226700000000E   3C                   ENDIF
226800000000     C*
226900000000IF  3C     SERS(I)       IFEQ      'C'
227000000000     C                   Z-ADD     SNC           $VA                            *CORRIERE
227100000000X   3C                   ELSE
227200000000IF  4C     SERS(I)       IFEQ      'E'
227300000000     C                   Z-ADD     SEC           $VA                            *ESPRESSO
227400000000X   4C                   ELSE
227500000000IF  5C     SERS(I)       IFEQ      'D'
227600000629     C                   Z-ADD     SDC           $VA                            *DISTRIBUZIONE
227700000629X   5C                   ELSE
227800091013IF  5C     SERS(I)       IFEQ      'H'
227900091013     C                   Z-ADD     SHC           $VA                            *Espresso H10:30
228000091013X   5C                   ELSE
228100000629IF  6C     SERS(I)       IFEQ      'P'
228200000629     C                   Z-ADD     SPC           $VA                            *POSTE
228300000629E   6C                   ENDIF
228400091013E   6C                   ENDIF
228500000629E   5C                   ENDIF
228600000000E   4C                   ENDIF
228700000000E   3C                   ENDIF
228800000000     C*--------------------
228900000000     C* STAMPA RIGA
229000000000     C*--------------------
229100000000     C* STAMPA LA RIGA SOLO SE HO MEMORIZZATO QUALCOSA
229200000000     C                   XFOOT     $VA           COM$VA           16 3
229300000000IF  3C     COM$VA        IFNE      *ZEROS
229400000000     C                   MOVEL     '1'           NODESV            1            *NO DESCRIZ VALORE
229500000000     C     DPRIMO        CAT       DSECON        VALSER           23            *VALORE + SERVIZIO
229600000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
229700000000     C                   EXSR      STASEX
229800000000E   3C                   ENDIF
229900000000     C*
230000000000E   2C                   ENDDO
230100000000     C*--------------------
230200000629     C* STAMPA RIGA TOTALE
230300000000     C*--------------------
230400000000     C* SE HO STAPATO QC. PRIMA DEI VALORI LA RIGA DEI TOTALI NON DEVE ESSERE
230500000000     C* PRECEDUTA DALLA DESCRIZIONE VALORE, ALTRIMENTI SI
230600000000     C     J             OCCUR     WSER
230700000000IF  2C     NODESV        IFEQ      '1'
230800000000     C                   MOVEL     *BLANKS       DPRIMO
230900000000X   2C                   ELSE
231000000000     C                   MOVEL     DCA(J)        DPRIMO                         *DESCRIZIONE VALORE
231100000000E   1C                   ENDIF
231200000000     C*
231300000000     C                   MOVEL     DCA(6)        DSECON                         *VALORE "TOTALE"
231400000000     C     DPRIMO        CAT       DSECON        VALSER                         *VALORE + SERVIZIO
231500000000     C                   Z-ADD     VAW           $VA                            *TOTALE VALORE
231600000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
231700000000     C                   EXSR      STASEX
231800000000     C*
231900071211     c                   eval      Righe_da_stampare = 1
232000071211     c                   exsr      solo_x_STT
232100010920     C                   WRITE     SE62RI                                       *RIGA DI SEPARAZIONE
232200071210      *
232300020823     C                   EXSR      STATES
232400000000E   1C                   ENDDO
232500000000     C*--------------------
232600040317     C* CALCOLA MEDIE E COLLI ORIGINALI
232700000000     C*--------------------
232800000000     C                   Z-ADD     1             I
232900000000DO  1C     1             DO        9             I                              *MEDIA DA STAMPARE
233000000000     C* PER OGNI MESE
233100000000     C                   Z-ADD     1             J
233200000000DO  2C     1             DO        SPARM         J
233300000000     C                   Z-ADD     SPW(J)        WSPW             16 3          *SPEDIZIONI DEL MESE
233400000000     C                   Z-ADD     COW(J)        WCOW             16 3          *COLLI DEL MESE
233500000000     C                   Z-ADD     PEW(J)        WPEW             16 3          *PESO IN Q DEL MESE
233600000000     C                   Z-ADD     VOW(J)        WVOW             16 3          *VOLUME MC DEL MESE
233700010921     C                   Z-ADD     RIW(J)        WRIW             16 3          *RICAVI DEL MES
233800000000     C                   Z-ADD     *ZEROS        WMED             16 3          *MEDIA DA CALCOLARE
233900000000     C* CALCOLA LE MEDIE
234000000000     C                   EXSR      CALMED
234100000000     C                   Z-ADD     WMED          MEW(J)                         *MEDIA DEL MESE
234200000000E   2C                   ENDDO
234300000000     C*--------------------
234400000000     C* STAMPA MEDIE
234500000000     C*--------------------
234600000000IF  2C     I             IFLE      5
234700010921     C                   MOVEL     'MED'         TIPVAL                         *COME PER I RICAVI
234800000000X   2C                   ELSE
234900000000     C                   MOVEL     'ME1'         TIPVAL                         *COME PER I VOLUMI
235000000000E   2C                   ENDIF
235100000000     C                   Z-ADD     MEW           $VA
235200000000     C                   MOVEL     DME(I)        $DE
235300000000     C                   Z-ADD     I             $I                4 0
235400000000     C                   XFOOT     SPW           $SP              16 3          *TOTALE SPEDIZIONI
235500000000     C                   XFOOT     COW           $CO              16 3          *TOTALE COLLI
235600000000     C                   XFOOT     PEW           $PE              16 3          *TOTALE PESO IN Q.LI
235700000000     C                   XFOOT     VOW           $VO              16 3          *TOTALE VOLUME
235800010921     C                   XFOOT     RIW           $RI              16 3          *TOTALE RICAVI
235900000000     C*
236000000000     C                   EXSR      STASEX
236100000000E   1C                   ENDDO
236200040317     C*--------------------
236300040317     C* COLLI ORIGINALI (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
236400040317     C*--------------------
236500040317     C                   MOVEL     'COL'         TIPVAL                         *COLLI
236600040317     C                   MOVEL     DME(43)       $DE                            *DESCRIZIONE
236700040317     C                   Z-ADD     COR           $VA
236800040317     C                   XFOOT     $VA           COM$VA           16 3
236900040317IF  1C     COM$VA        IFNE      *ZEROS
237000040317     C                   EXSR      STASEX
237100040317E   1C                   ENDIF
237200040318     C*--------------------
237300040318     C* BANCALI BNB (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
237400040318     C*--------------------
237500040318     C                   MOVEL     DME(44)       $DE                            *DESCRIZIONE
237600040318     C                   Z-ADD     BNB           $VA
237700040318     C                   XFOOT     $VA           COM$VA           16 3
237800040318IF  1C     COM$VA        IFNE      *ZEROS
237900040318     C                   EXSR      STASEX
238000040318E   1C                   ENDIF
238100040318     C*--------------------
238200040318     C* BANCALI BAN (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
238300040318     C*--------------------
238400040318     C                   MOVEL     DME(45)       $DE                            *DESCRIZIONE
238500040318     C                   Z-ADD     BAN           $VA
238600040318     C                   XFOOT     $VA           COM$VA           16 3
238700040318IF  1C     COM$VA        IFNE      *ZEROS
238800040318     C                   EXSR      STASEX
238900040318E   1C                   ENDIF
239000040317     C*
239100071211     c                   eval      Righe_da_stampare = 1
239200071211     c                   exsr      solo_x_STT
239300040317     C                   WRITE     SE62RI                                       *RIGA CHIUSURA
239400071210      *
239500040317     C                   EXSR      STATES
239600040317     C*
239700000000     C                   ENDSR
239800000000     C*------------------------------------------------------------------------*
239900000000     C*?STASEX - IMPOSTA STAMPA RIGA XXX STATISTICA SERVIZIO BOLLE
240000000000     C*          INPUT --> $VA  : SCHIERA VALORI DI 12 MESI
240100000000     C*                    $DE  : DESCRIZIONE RIGA DA STAMPARE
240200000000     C*                    $I   : TOTALE MEDIA DA STAMPARE
240300000000     C*                    $SP  : TOTALE SPEDIZIONI
240400000000     C*                    $CO  : TOTALE COLLI
240500000000     C*                    $PE  : TOTALE PESO IN Q.LI
240600000000     C*                    $VO  : TOTALE VOLUME
240700010921     C*                    $RI  : TOTALE RICAVI
240800000000     C*------------------------------------------------------------------------*
240900000000     C     STASEX        BEGSR
241000000000     C*
241100000000     C*--------------------
241200000000     C* 13^ C O L O N N A
241300000000     C*--------------------
241400000000     C* NB: PER IL CALCOLO DELLA 13^ COL DELLE MEDIE NON POSSO FARE LA SOMMA
241500000000IF  1C     TIPVAL        IFNE      'MED'                                        N O   M E D I E
241600000000     C     TIPVAL        ANDNE     'ME1'
241700000000     C                   XFOOT     $VA           $VA13C           16 3
241800000000X   1C                   ELSE                                                   M E D I E
241900000000     C                   Z-ADD     $SP           WSPW                           *SPEDIZIONI TOTALI
242000000000     C                   Z-ADD     $CO           WCOW                           *COLLI TOTALI
242100000000     C                   Z-ADD     $PE           WPEW                           *PESO IN QLI TOTALE
242200000000     C                   Z-ADD     $VO           WVOW                           *VOLUME MC TOTALE
242300010921     C                   Z-ADD     $RI           WRIW                           *RICAVI TOTALI
242400000000     C                   Z-ADD     *ZEROS        WMED             16 3          *MEDIA DA CALCOLARE
242500000000     C                   Z-ADD     $I            I                              *MEDIA DA CALCOLARE
242600000000     C* CALCOLA LE MEDIE
242700000000     C                   EXSR      CALMED
242800000000     C                   Z-ADD     WMED          $VA13C                         *MEDIA TOTALE
242900000000E   1C                   ENDIF
243000000000     C*
243100010920IF  1C***  TIPVAL        IFEQ      'RIC'                                        R I C A V I
243200010920     C***  $VA           DIV       1000          $VA                            *RICAVI DIVISO 1000
243300010920     C***  $VA13C        DIV       1000          $VA13C                         *13^COL DIVISO 1000
243400010920E   1C***                ENDIF
243500000000     C*
243600000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
243700000000IF  1C     TIPVAL        IFEQ      'VOL'                                        V O L U M E
243800010921     C     TIPVAL        OREQ      'MED'                                        M E D I E
243900000000     C                   EXSR      STACAV                                       *RIGA VOLUMI E MEDIE
244000000000X   1C                   ELSE
244100020131IF  2C     TIPVAL        IFEQ      'ME1'
244200020131     C                   EXSR      STACAR1
244300020131     C                   ELSE
244400020131IF  3C     TIPVAL        IFEQ      'RIC'                                        R I C A V I
244500010920     C                   EXSR      STACAR
244600020131X   3C                   ELSE
244700000000     C                   EXSR      STACAN                                       *RIGA ALTRI VALORI
244800020131E   3C                   ENDIF
244900020131E   2C                   ENDIF
245000000000E   1C                   ENDIF
245100000000     C*
245200000000     C                   ENDSR
245300050118     C*------------------------------------------------------------------------*
245400050118     C*?STAPEV - IMPOSTA STAMPA STATISTICA "PESI-VOLUME"
245500050119     C*          INDICI:  J(4,0) --> MESI
245600050118     C*------------------------------------------------------------------------*
245700050118     C     STAPEV        BEGSR
245800050118     C*--------------------
245900050118     C* PARAMETRI
246000050118     C*--------------------
246100050118IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
246200050118     C                   EXSR      STAPAR
246300050118E   1C                   ENDIF
246400050118     C*--------------------
246500050118     C* DATI GENERALI CLIENTE
246600050118     C*--------------------
246700050118IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
246800050118     C                   EXSR      STADGC
246900050118E   1C                   ENDIF
247000050118     C*--------------------
247100050118     C* INTESTAZIONE PESI
247200050118     C*--------------------
247300050118     C                   MOVEL     DRI(6)        PICSTA
247400071210      *
247500071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
247600071211     c                   eval      Righe_da_stampare = 5
247700071211     c                   exsr      solo_x_STT
247800050121     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
247900050119     C*
248000050118     C*--------------------
248100050118     C* PESI             -->
248200050118     C*--------------------
248300050118     C     1             OCCUR     Cpev
248400050121     C*
248500050121     C*   Intestazione Pesi
248600050121     C                   MOVEL     DME(60)       $DE
248700050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
248800050121     C                   z-add     *ZEROS        COM$VA           16 3
248900050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
249000050121     C                   EXSR      STAAMX
249100050121     C                   EXSR      STATES
249200050118     C*
249300050119     C*--------------------
249400050119     C* PESI % Colli Ril.
249500050119     C*--------------------
249600050119     C* PER OGNI MESE CALCOLO LA PERCENTUALE
249700050119     C                   Z-ADD     1             J
249800050119DO  2C     1             DO        SPARM         J
249900050119     C                   Z-ADD     NCB(J)        WFAW             16 3          *Nr.Colli VDL
250000050119     C                   Z-ADD     NCV(J)        WNRW             16 3          *Nr.Colli Boll.
250100050119     C                   Z-ADD     *ZEROS        WPDW              4 1          *PERCENTUALE
250200050119     C                   EXSR      CALPER
250300050119     C                   Z-ADD     WPDW          APW(J)                         *SCHIERA PERCENTUALI
250400050119E   2C                   ENDDO
250500050119     C*
250600050119     C                   MOVEL     DME(54)       $DE
250700050121     c*****              move      'PES'         $DE
250800050119     C                   MOVEL     'PER'         TIPVAL                         *PERCENTUALI
250900050119     C                   Z-ADD     APW           $VA
251000050119     C*
251100050119     C* CALCOLO I TOTALI FATTURATO E NOTE PER 13^ COLONNA
251200050119     C                   XFOOT     NCB           $FA              16 3
251300050119     C                   XFOOT     NCV           $NR              16 3
251400050119     C                   EXSR      STAAMX
251500050119     C                   EXSR      STATES
251600050121     C*--------------------
251700050121     C* PESI   D E L T A   % SCOST.Ril./Boll.
251800050121     C*--------------------
251900050121     C* PER OGNI MESE CALCOLO IL DELTA
252000050121     C                   Z-ADD     1             J
252100050121DO  2C     1             DO        SPARM         J
252200050121     C                   Z-ADD     PVB(J)        WRRW             16 3          *Peso Boll.
252300050121     C                   Z-ADD     PVV(J)        WRPW             16 3          *Peso VDL
252400050121     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
252500050121     C                   EXSR      CALDEL
252600050121     C                   Z-ADD     WDPW          DDW(J)                         *SCHIERA DELTA
252700050121E   2C                   ENDDO
252800050121     C*
252900050121     C                   MOVEL     'DEL'         TIPVAL                         *VALORE DELTA
253000050121     C                   Z-ADD     DDW           $VA
253100050121     C                   MOVEL     DME(55)       $DE              23            *DESCRIZIONE RIGA
253200050121     c*****              move      'PES'         $DE
253300050121     C                   XFOOT     PVB           $RR              16 3          *Peso Boll.
253400050121     C                   XFOOT     PVV           $RP              16 3          *Peso VDL
253500050121     C                   EXSR      STADEX
253600050121     C                   EXSR      STATES
253700050121     C*
253800050121     C                   if        d60dpv = 'S'
253900050121     C*--------------------
254000050121     C* PESI Nr.Colli VDL
254100050121     C*--------------------
254200050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
254300050121     C                   ADD       NCV           $VA                            *VALORI RIGA
254400050121     C                   XFOOT     $VA           COM$VA           16 3
254500050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
254600050121IF  1C     COM$VA        IFNE      *ZEROS
254700050121     C                   MOVEL     DME(52)       $DE
254800050121     c*****              move      'PES'         $DE
254900050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
255000050121     C                   EXSR      STAAMX
255100050121     C                   EXSR      STATES
255200050121E   1C                   ENDIF
255300050121     C*--------------------
255400050121     C* PESI Nr.Colli Boll.
255500050121     C*--------------------
255600050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
255700050121     C                   ADD       NCB           $VA                            *VALORI RIGA
255800050121     C                   XFOOT     $VA           COM$VA           16 3
255900050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
256000050121IF  1C     COM$VA        IFNE      *ZEROS
256100050121     C                   MOVEL     DME(53)       $DE
256200050121     c*****              move      'PES'         $DE
256300050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
256400050121     C                   EXSR      STAAMX
256500050121     C                   EXSR      STATES
256600050121E   1C                   ENDIF
256700050121     C*--------------------
256800050121     C* PESI Pesi VDL
256900050121     C*--------------------
257000050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
257100050121     C                   ADD       PVV           $VA                            *VALORI RIGA
257200050121     C                   XFOOT     $VA           COM$VA           16 3
257300050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
257400050121IF  1C     COM$VA        IFNE      *ZEROS
257500050121     C                   MOVEL     DME(56)       $DE
257600050121     c*****              move      'PES'         $DE
257700050121     C                   MOVEL     'VOL'         TIPVAL                         *PESI/VOLUMI
257800050121     C                   EXSR      STAAMX
257900050121     C                   EXSR      STATES
258000050121E   1C                   ENDIF
258100050121     C*--------------------
258200050121     C* PESI Pesi Boll.
258300050121     C*--------------------
258400050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
258500050121     C                   ADD       PVB           $VA                            *VALORI RIGA
258600050121     C                   XFOOT     $VA           COM$VA           16 3
258700050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
258800050121IF  1C     COM$VA        IFNE      *ZEROS
258900050121     C                   MOVEL     DME(57)       $DE
259000050121     c*****              move      'PES'         $DE
259100050121     C                   MOVEL     'VOL'         TIPVAL                         *PESI/VOLUMI
259200050121     C                   EXSR      STAAMX
259300050121     C                   EXSR      STATES
259400050121E   1C                   ENDIF
259500050121      **
259600050121E   1C                   End
259700050119     C*
259800050118     C*--------------------
259900050118     C* VOLUMI           -->
260000050118     C*--------------------
260100050118     C     2             OCCUR     Cpev
260200050121     C*
260300050121     C*   Intestazione Volumi
260400050121     C                   MOVEL     DME(61)       $DE
260500050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
260600050121     C                   z-add     *ZEROS        COM$VA           16 3
260700050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
260800050121     C                   EXSR      STAAMX
260900050121     C                   EXSR      STATES
261000050121      *
261100050119     C*--------------------
261200050119     C* VOLUMI % Colli Ril.
261300050119     C*--------------------
261400050119     C* PER OGNI MESE CALCOLO LA PERCENTUALE
261500050119     C                   Z-ADD     1             J
261600050119DO  2C     1             DO        SPARM         J
261700050119     C                   Z-ADD     NCB(J)        WFAW             16 3          *Nr.Colli VDL
261800050119     C                   Z-ADD     NCV(J)        WNRW             16 3          *Nr.Colli Boll.
261900050119     C                   Z-ADD     *ZEROS        WPDW              4 1          *PERCENTUALE
262000050119     C*
262100050119     C                   EXSR      CALPER
262200050119     C                   Z-ADD     WPDW          APW(J)                         *SCHIERA PERCENTUALI
262300050119E   2C                   ENDDO
262400050119     C*
262500050119     C                   MOVEL     DME(54)       $DE
262600050121     c*****              move      'VOL'         $DE
262700050119     C                   MOVEL     'PER'         TIPVAL                         *PERCENTUALI
262800050119     C                   Z-ADD     APW           $VA
262900050119     C*
263000050119     C* CALCOLO I TOTALI FATTURATO E NOTE PER 13^ COLONNA
263100050119     C                   XFOOT     NCB           $FA              16 3
263200050119     C                   XFOOT     NCV           $NR              16 3
263300050119     C                   EXSR      STAAMX
263400050119     C                   EXSR      STATES
263500050121     C*--------------------
263600050121     C* VOLUMI   D E L T A   % SCOST.Ril./Boll.
263700050121     C*--------------------
263800050121     C* PER OGNI MESE CALCOLO IL DELTA
263900050121     C                   Z-ADD     1             J
264000050121DO  2C     1             DO        SPARM         J
264100050121     C                   Z-ADD     PVB(J)        WRRW             16 3          *Peso Boll.
264200050121     C                   Z-ADD     PVV(J)        WRPW             16 3          *Peso VDL
264300050121     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
264400050121     C                   EXSR      CALDEL
264500050121     C                   Z-ADD     WDPW          DDW(J)                         *SCHIERA DELTA
264600050121E   2C                   ENDDO
264700050121     C*
264800050121     C                   MOVEL     'DEL'         TIPVAL                         *VALORE DELTA
264900050121     C                   Z-ADD     DDW           $VA
265000050121     C                   MOVEL     DME(55)       $DE              23            *DESCRIZIONE RIGA
265100050121     c*****              move      'VOL'         $DE
265200050121     C                   XFOOT     PVB           $RR              16 3          *Peso Boll.
265300050121     C                   XFOOT     PVV           $RP              16 3          *Peso VDL
265400050121     C                   EXSR      STADEX
265500050121     C                   EXSR      STATES
265600050121     C*
265700050121     C                   if        d60dpv = 'S'
265800050121     C*--------------------
265900050121     C* VOLUMI Nr.Colli VDL
266000050121     C*--------------------
266100050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
266200050121     C                   ADD       NCV           $VA                            *VALORI RIGA
266300050121     C                   XFOOT     $VA           COM$VA           16 3
266400050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
266500050121IF  1C     COM$VA        IFNE      *ZEROS
266600050121     C                   MOVEL     DME(52)       $DE
266700050121     c*****              move      'VOL'         $DE
266800050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
266900050121     C                   EXSR      STAAMX
267000050121     C                   EXSR      STATES
267100050121E   1C                   ENDIF
267200050121     C*--------------------
267300050121     C* VOLUMI Nr.Colli Boll.
267400050121     C*--------------------
267500050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
267600050121     C                   ADD       NCB           $VA                            *VALORI RIGA
267700050121     C                   XFOOT     $VA           COM$VA           16 3
267800050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
267900050121IF  1C     COM$VA        IFNE      *ZEROS
268000050121     C                   MOVEL     DME(53)       $DE
268100050121     c*****              move      'VOL'         $DE
268200050121     C                   MOVEL     'COL'         TIPVAL                         *COLLI
268300050121     C                   EXSR      STAAMX
268400050121     C                   EXSR      STATES
268500050121E   1C                   ENDIF
268600050121     C*--------------------
268700050121     C* VOLUMI Volumi VDL
268800050121     C*--------------------
268900050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
269000050121     C                   ADD       PVV           $VA                            *VALORI RIGA
269100050121     C                   XFOOT     $VA           COM$VA           16 3
269200050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
269300050121IF  1C     COM$VA        IFNE      *ZEROS
269400050121     C                   MOVEL     DME(58)       $DE
269500050121     c*****              move      'VOL'         $DE
269600050121     C                   MOVEL     'VOL'         TIPVAL                         *PESI/VOLUMI
269700050121     C                   EXSR      STAAMX
269800050121     C                   EXSR      STATES
269900050121E   1C                   ENDIF
270000050121     C*--------------------
270100050121     C* VOLUMI Volumi Boll.
270200050121     C*--------------------
270300050121     C                   Z-ADD     *ZEROS        $VA                            *PER I VALORI
270400050121     C                   ADD       PVB           $VA                            *VALORI RIGA
270500050121     C                   XFOOT     $VA           COM$VA           16 3
270600050121     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
270700050121IF  1C     COM$VA        IFNE      *ZEROS
270800050121     C                   MOVEL     DME(59)       $DE
270900050121     c*****              move      'VOL'         $DE
271000050121     C                   MOVEL     'VOL'         TIPVAL                         *PESI/VOLUMI
271100050121     C                   EXSR      STAAMX
271200050121     C                   EXSR      STATES
271300050121E   1C                   ENDIF
271400050121      *
271500050119E   1C                   End
271600050118      *
271700071211     c                   eval      Righe_da_stampare = 1
271800071211     c                   exsr      solo_x_STT
271900050118     C                   WRITE     SE62RI                                       *RIGA FINE PROSPETTO
272000071210      *
272100050118     C                   EXSR      STATES
272200050118     C*
272300050118     C                   ENDSR
272400000000     C*------------------------------------------------------------------------*
272500000000     C*?STAREG - IMPOSTA STAMPA STATISTICA "REGIONE-REGIONE"
272600000000     C*          INDICI: I(4,0) --> DECODIFICA RAGGRUPPAMENTO
272700000000     C*                  J(4,0) --> RAGGRUPPAMENTO IN LINEA
272800000000     C*                  E(4,0) --> ORDINE LETTURA RAGGRUPPAMENTI
272900000000     C*------------------------------------------------------------------------*
273000000000     C     STAREG        BEGSR
273100000000     C*--------------------
273200000000     C* PARAMETRI
273300000000     C*--------------------
273400000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
273500000000     C                   EXSR      STAPAR
273600950206E   1C                   ENDIF
273700000000     C*--------------------
273800000000     C* DATI GENERALI CLIENTE
273900000000     C*--------------------
274000000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
274100000000     C                   EXSR      STADGC
274200000000E   1C                   ENDIF
274300000000     C*--------------------
274400000000     C* INTESTAZIONE
274500000000     C*--------------------
274600000000     C                   MOVEL     DRI(1)        PICSTA
274700071210      *
274800071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
274900071211     c                   eval      Righe_da_stampare = 5
275000071211     c                   exsr      solo_x_STT
275100010920     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
275200950206     C*
275300020605     C* PER OGNI TIPO DI VALORE RICHIESTO (O UNO O TUTTI)
275400950206DO  1C     1             DO        5             JJ
275500950206     C*
275600950206IF  2C     D60RIF        IFEQ      *BLANKS                                      *TUTTI
275700950206     C     D60RIF        OREQ      VRR(JJ)                                      *VALORE = SELEZIONAT
275800950206     C*
275900010920     C                   CLEAR                   SE62P1
276000950206     C                   MOVEL     DVRR(JJ)      P1DDES
276100071210      *
276200071211     c                   eval      Righe_da_stampare = 1
276300071211     c                   exsr      solo_x_STT
276400020524     C                   WRITE     SE62P1                                       *DESCRIZIONE VALORE
276500071210      *
276600020823     C                   EXSR      STATES
276700000000     C*
276800000000     C* MANDO IN STAMPA PRIMA I RAGGRUPPAMENTI REGIONE PARTENZA E POI GLI ARRIVO
276900020605     C***                Z-ADD     *ZEROS        E                 4 0
277000020605DO  3C***  1             DO        2             E
277100000000     C*
277200020605IF  4C***  E             IFEQ      1
277300020605     C***                MOVEL     'P'           CICLO             1            *PRIMA LA PARTENZA
277400020605X   4C***                ELSE
277500020605     C***                MOVEL     'A'           CICLO                          *POI L'ARRIVO
277600020605E   4C***                ENDIF
277700020603     C*
277800020603     C* ORDINO L'OCCUR "REGIONE-REGIONE" X REGIONE P/A
277900020605     C                   EXSR      ORDCRER
278000000000     C*
278100000000     C* PER OGNI COMBINAZIONE DI REGIONI VALORIZZATA
278200000000     C                   Z-ADD     1             J
278300950206DO  4C     1             DO        R             J
278400000000     C     J             OCCUR     CRER
278500000000     C*
278600000000     C* CONTROLLO COSA ELABORARE (P/A)
278700020605     C***  1             SUBST     REG(J):1      ALF1              1
278800020605IF  5C***  CICLO         IFEQ      ALF1
278900020605     C***                MOVE      REG(J)        WRER                           *...+...
279000020605     C                   MOVE      REX           WRER                           *...+...
279100000000     C*
279200000000     C* IMPOSTA IL CAMPO DECODIFICA DEI DUE RAGGRUPPAMENTI
279300000000     C                   MOVEL     WRER          PRIMO             3 0          *1° RAGGRUPPAMENTO
279400000000     C                   Z-ADD     1             I
279500000000     C                   SETOFF                                           28
279600000000     C     PRIMO         LOOKUP    CRR(I)                                 28
279700950206IF  6C     *IN28         IFEQ      *ON
279800000000     C                   MOVEL     DCRR(I)       DPRIMO           11            *TROVATO
279900950206X   6C                   ELSE
280000000000     C                   MOVEL     *ALL'*'       DPRIMO                         *NON TROVATO
280100950206E   6C                   ENDIF
280200000000     C*
280300000000     C                   MOVE      WRER          SECON             3 0          *2° RAGGRUPPAMENTO
280400000000     C                   Z-ADD     1             I
280500000000     C                   SETOFF                                           28
280600000000     C     SECON         LOOKUP    CRR(I)                                 28
280700950206IF  6C     *IN28         IFEQ      *ON
280800000000     C                   MOVEL     DCRR(I)       DSECON           11            *TROVATO
280900950206X   6C                   ELSE
281000000000     C                   MOVEL     *ALL'*'       DSECON                         *NON TROVATO
281100950206E   6C                   ENDIF
281200000000     C*
281300950725     C     DPRIMO        CAT       '-'           RAGRAG           23
281400950725     C                   MOVE      DSECON        RAGRAG                         *REG-REG
281500000000     C*--------------------
281600000000     C* STAMPA RIGA
281700000000     C*--------------------
281800950206IF  6C     VRR(JJ)       IFEQ      'C'
281900950206     C                   Z-ADD     REC           $VA                            *SCHIERA VALORI
282000950206E   6C                   ENDIF
282100950206IF  6C     VRR(JJ)       IFEQ      'P'
282200950206     C                   Z-ADD     REP           $VA                            *SCHIERA VALORI
282300950206E   6C                   ENDIF
282400950206IF  6C     VRR(JJ)       IFEQ      'R'
282500950206     C                   Z-ADD     RER           $VA                            *SCHIERA VALORI
282600950206E   6C                   ENDIF
282700950206IF  6C     VRR(JJ)       IFEQ      'S'
282800950206     C                   Z-ADD     RES           $VA                            *SCHIERA VALORI
282900950206E   6C                   ENDIF
283000950206IF  6C     VRR(JJ)       IFEQ      'V'
283100950206     C                   Z-ADD     REV           $VA                            *SCHIERA VALORI
283200950206E   6C                   ENDIF
283300000000     C                   MOVEL     RAGRAG        $DE              23            *DESCRIZIONE RIGA
283400000000     C                   EXSR      STAREX
283500000000     C*
283600020605E   5C***                ENDIF
283700950206E   4C                   ENDDO
283800020605E   3C***                ENDDO
283900950206E   2C                   ENDIF
284000950206E   1C                   ENDDO
284100000000     C*
284200071211     c                   eval      Righe_da_stampare = 1
284300071211     c                   exsr      solo_x_STT
284400010920     C                   WRITE     SE62RI                                       *RIGA CHIUSURA
284500071210      *
284600020823     C                   EXSR      STATES
284700000000     C*
284800000000     C                   ENDSR
284900950206     C*------------------------------------------------------------------------*
285000000000     C*?STAREX - IMPOSTA STAMPA RIGA XXX STATISTICA REGIONE
285100000000     C*          INPUT --> $VA    : SCHIERA VALORI DI 12 MESI
285200000000     C*                    $DE    : DESCRIZIONE RIGA DA STAMPARE
285300000000     C*------------------------------------------------------------------------*
285400000000     C     STAREX        BEGSR
285500000000     C*
285600000000     C*--------------------
285700000000     C* 13^ C O L O N N A
285800000000     C*--------------------
285900000000     C                   XFOOT     $VA           $VA13C           16 3
286000000000     C*
286100010920IF  1C***  VRR(JJ)       IFEQ      'R'                                          R I C A V I
286200010920     C***  $VA           DIV       1000          $VA                            *RICAVI DIVISO 1000
286300010920     C***  $VA13C        DIV       1000          $VA13C                         *13^COL DIVISO 1000
286400010920E   1C***                ENDIF
286500000000     C*
286600010920     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
286700950206IF  1C     VRR(JJ)       IFEQ      'V'                                          V O L U M E
286800000000     C                   EXSR      STACAV                                       *RIGA VOLUMI
286900000000X   1C                   ELSE
287000010921IF  2C     VRR(JJ)       IFEQ      'R'                                          R I C A V I
287100010920     C                   EXSR      STACAR                                       *RIGA VOLUMI
287200010920X   2C                   ELSE
287300000000     C                   EXSR      STACAN                                       *RIGA ALTRI VALORI
287400010920E   2C                   ENDIF
287500000000E   1C                   ENDIF
287600000000     C*
287700000000     C                   ENDSR
287800000000     C*------------------------------------------------------------------------*
287900000000     C*?STADEL - IMPOSTA STAMPA STATISTICA "DELTA PER CLIENTE"
288000000000     C*          INDICI: J(4,0) - I(4,0)
288100000000     C*------------------------------------------------------------------------*
288200000000     C     STADEL        BEGSR
288300000000     C*--------------------
288400000000     C* PARAMETRI
288500000000     C*--------------------
288600000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
288700000000     C                   EXSR      STAPAR
288800000000E   1C                   ENDIF
288900000000     C*--------------------
289000000000     C* DATI GENERALI CLIENTE
289100000000     C*--------------------
289200000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
289300000000     C                   EXSR      STADGC
289400000000E   1C                   ENDIF
289500000000     C*--------------------
289600000000     C* INTESTAZIONE
289700000000     C*--------------------
289800000000     C                   MOVEL     DRI(4)        PICSTA
289900071210      *
290000071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
290100071211     c                   eval      Righe_da_stampare = 5
290200071211     c                   exsr      solo_x_STT
290300010920     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
290400000000     C*
290500000000     C* PULISCO I TOTALI A FONDO PAGINA
290600000000     C                   CLEAR                   DRW                            *RICAVI REALI
290700000000     C                   CLEAR                   DPW                            *RICAVI PRESUNTI
290800000000     C                   CLEAR                   DSW                            *SPEDIZIONI
290900000000     C*
291000000000     C* PER OGNI FASCIA DI PESO SELEZIONATA DA STAMPARE
291100000000     C                   Z-ADD     1             J
291200000000DO  1C     1             DO        SPARF         J
291300000000     C     J             OCCUR     CDEL
291400000000     C*
291500000000     C* CALCOLO I TOTALI A FONDO PAGINA
291600000000     C                   ADD       DRC           DRW                            *RICAVI REALI
291700000000     C                   ADD       DPC           DPW                            *RICAVI PRESUNTI
291800000000     C                   ADD       DSC           DSW                            *SPEDIZIONI
291900000000     C*
292000000000     C* DECODIFICO LA FASCIA SELEZIONATA
292100000000     C                   Z-ADD     1             I
292200000000     C                   SETOFF                                           28
292300000000     C     SFPE(J)       LOOKUP    FPE(I)                                 28
292400000000IF  2C     *IN28         IFEQ      *ON
292500000000     C                   MOVEL     DFPE(I)       DPRIMD           14
292600000000X   2C                   ELSE
292700000000     C                   MOVEL     *ALL'*'       DPRIMD
292800000000E   2C                   ENDIF
292900000000     C*--------------------
293000010921     C* STAMPA  R I C A V I
293100000000     C*--------------------
293200000000     C                   MOVEL     *BLANKS       DSECOD            8
293300000000     C                   MOVEL     DCA(7)        DSECOD
293400000000     C*
293500000000     C                   MOVEL     *BLANKS       VALSER           23
293600000000     C     DPRIMD        CAT       DSECOD        VALSER
293700000000     C                   MOVEL     'RIC'         TIPVAL                         *VALORE RICAVI
293800000000     C                   Z-ADD     DRC           $VA
293900000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
294000000000     C                   EXSR      STADEX
294100000000     C*--------------------
294200000000     C* STAMPA  S P E D I Z I O N I
294300000000     C*--------------------
294400000000     C                   MOVEL     *BLANKS       DPRIMD                         *DESCR FASCIA 1 VOLT
294500000000     C                   MOVEL     *BLANKS       DSECOD            8
294600000000     C                   MOVEL     DCA(8)        DSECOD
294700000000     C*
294800000000     C                   MOVEL     *BLANKS       VALSER           23
294900000000     C     DPRIMD        CAT       DSECOD        VALSER
295000000000     C                   MOVEL     'SPE'         TIPVAL                         *VALORE SPEDIZIONI
295100000000     C                   Z-ADD     DSC           $VA
295200000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
295300000000     C                   EXSR      STADEX
295400000000     C*--------------------
295500000000     C* STAMPA  D E L T A
295600000000     C*--------------------
295700000000     C* PER OGNI MESE CALCOLO IL DELTA
295800000000     C                   Z-ADD     1             I
295900000000DO  2C     1             DO        SPARM         I
296000000000     C                   Z-ADD     DRC(I)        WRRW             16 3          *RICAVO REALE
296100000000     C                   Z-ADD     DPC(I)        WRPW             16 3          *RICAVO PRESUNTO
296200000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
296300000000     C                   EXSR      CALDEL
296400000000     C                   Z-ADD     WDPW          DDW(I)                         *SCHIERA DELTA
296500000000E   2C                   ENDDO
296600000000     C*
296700000000     C                   MOVEL     *BLANKS       DSECOD            8
296800000000     C                   MOVEL     DCA(9)        DSECOD
296900000000     C*
297000000000     C                   MOVEL     *BLANKS       VALSER           23
297100000000     C     DPRIMD        CAT       DSECOD        VALSER
297200000000     C                   MOVEL     'DEL'         TIPVAL                         *VALORE DELTA
297300000000     C*
297400000000     C                   Z-ADD     DDW           $VA
297500000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
297600000000     C                   XFOOT     DRC           $RR              16 3          *TOT RICAVI REALI
297700000000     C                   XFOOT     DPC           $RP              16 3          *TOT RICAVI PRESUNTI
297800000000     C                   EXSR      STADEX
297900000000     C*
298000071211     c                   eval      Righe_da_stampare = 1
298100071211     c                   exsr      solo_x_STT
298200010920     C                   WRITE     SE62RI                                       *RIGA DI SEPARAZIONE
298300071210      *
298400020823     C                   EXSR      STATES
298500000000E   1C                   ENDDO
298600000000     C*--------------------
298700000000     C* STAMPA  T O T A L E
298800000000     C*--------------------
298900000000     C* R I C A V I
299000000000     C                   MOVEL     *BLANKS       DPRIMD           14
299100000000     C                   MOVEL     DCA(7)        DPRIMD
299200000000     C                   MOVEL     DCA(10)       DSECOD
299300000000     C*
299400000000     C                   MOVEL     *BLANKS       VALSER           23
299500000000     C     DPRIMD        CAT       DSECOD        VALSER
299600000000     C                   MOVEL     'RIC'         TIPVAL                         *VALORE RICAVI
299700000000     C                   Z-ADD     DRW           $VA
299800000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
299900000000     C                   EXSR      STADEX
300000000000     C* S P E D I Z I O N I
300100000000     C                   MOVEL     *BLANKS       DPRIMD           14
300200000000     C                   MOVEL     DCA(1)        DPRIMD
300300000000     C                   MOVEL     DCA(10)       DSECOD            8
300400000000     C*
300500000000     C                   MOVEL     *BLANKS       VALSER           23
300600000000     C     DPRIMD        CAT       DSECOD        VALSER
300700000000     C                   MOVEL     'SPE'         TIPVAL                         *VALORE SPEDIZIONI
300800000000     C                   Z-ADD     DSW           $VA
300900000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
301000000000     C                   EXSR      STADEX
301100000000     C* D E L T A
301200000000     C*
301300000000     C* PER OGNI MESE CALCOLO IL DELTA
301400000000     C                   Z-ADD     1             I
301500000000DO  1C     1             DO        SPARM         I
301600000000     C                   Z-ADD     DRW(I)        WRRW             16 3          *RICAVO REALE
301700000000     C                   Z-ADD     DPW(I)        WRPW             16 3          *RICAVO PRESUNTO
301800000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
301900000000     C                   EXSR      CALDEL
302000000000     C                   Z-ADD     WDPW          DDW(I)                         *SCHIERA DELTA
302100000000E   1C                   ENDDO
302200000000     C*
302300000000     C                   MOVEL     *BLANKS       DPRIMD           14
302400000000     C                   MOVEL     DCA(9)        DPRIMD
302500000000     C                   MOVEL     DCA(6)        DSECOD            8
302600000000     C*
302700000000     C                   MOVEL     *BLANKS       VALSER           23
302800000000     C     DPRIMD        CAT       DSECOD        VALSER
302900000000     C                   MOVEL     'DEL'         TIPVAL                         *VALORE DELTA
303000000000     C*
303100000000     C                   Z-ADD     DDW           $VA
303200000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
303300000000     C                   XFOOT     DRW           $RR              16 3          *TOT RICAVI REALI
303400000000     C                   XFOOT     DPW           $RP              16 3          *TOT RICAVI PRESUNTI
303500000000     C                   EXSR      STADEX
303600000000     C*
303700071211     c                   eval      Righe_da_stampare = 1
303800071211     c                   exsr      solo_x_STT
303900010920     C                   WRITE     SE62RI                                       *RIGA FINE PROSPETTO
304000071210      *
304100020823     C                   EXSR      STATES
304200000000     C*
304300000000     C                   ENDSR
304400000000     C*------------------------------------------------------------------------*
304500000000     C*?STADEX - IMPOSTA STAMPA RIGA XXX STATISTICA DELTA PER CLIENTE
304600000000     C*          INPUT --> $VA  : SCHIERA VALORI DI 12 MESI
304700000000     C*                    $DE  : DESCRIZIONE RIGA DA STAMPARE
304800000000     C*                    $RR  : TOTALE RICAVI REALI
304900000000     C*                    $RP  : TOTALE RICAVI PRESUNTI
305000000000     C*------------------------------------------------------------------------*
305100000000     C     STADEX        BEGSR
305200000000     C*--------------------
305300000000     C* 13^ C O L O N N A
305400000000     C*--------------------
305500000000     C* PER LA RIGA DEL DELTA NON POSSO FARE LA XFOOT
305600000000IF  1C     TIPVAL        IFNE      'DEL'                                        *N O   D E L T A
305700000000     C                   XFOOT     $VA           $VA13C           16 3
305800000000X   1C                   ELSE                                                   *S I   D E L T A
305900000000     C                   Z-ADD     $RR           WRRW             16 3          *RICAVO REALE
306000000000     C                   Z-ADD     $RP           WRPW             16 3          *RICAVO PRESUNTO
306100000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
306200000000     C                   EXSR      CALDEL
306300000000     C                   Z-ADD     WDPW          $VA13C                         *DELTA 13^ COLONNA
306400000000E   1C                   ENDIF
306500000000     C*
306600000000     C* SE RICAVI LI COMPATTA
306700010920IF  1C***  TIPVAL        IFEQ      'RIC'
306800010920     C***  $VA           DIV       1000          $VA
306900010920     C***  $VA13C        DIV       1000          $VA13C
307000010920E   1C***                ENDIF
307100000000     C*
307200000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
307300010921IF  1C     TIPVAL        IFEQ      'DEL'                                        *D E L T A
307400010921     C                   EXSR      STACAP
307500010921X   1C                   ELSE
307600010921IF  2C     TIPVAL        IFEQ      'RIC'                                        *R I C A V I
307700010920     C                   EXSR      STACAR
307800010920X   2C                   ELSE
307900010921     C                   EXSR      STACAN                                       *ALTRI VALORI
308000010920E   2C                   ENDIF
308100000000E   1C                   ENDIF
308200000000     C*
308300000000     C                   ENDSR
308400000000     C*------------------------------------------------------------------------*
308500000000     C*?STAAFF - IMPOSTA STAMPA STATISTICA "AFFIDABILITA'"
308600000000     C*          INDICI: J(4,0) --> ORA CONSEGNA (1÷7)
308700000000     C*------------------------------------------------------------------------*
308800000000     C     STAAFF        BEGSR
308900000000     C*--------------------
309000000000     C* PARAMETRI
309100000000     C*--------------------
309200000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
309300000000     C                   EXSR      STAPAR
309400000000E   1C                   ENDIF
309500000000     C*--------------------
309600000000     C* DATI GENERALI CLIENTE
309700000000     C*--------------------
309800000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
309900000000     C                   EXSR      STADGC
310000000000E   1C                   ENDIF
310100000000     C*--------------------
310200000000     C* INTESTAZIONE
310300000000     C*--------------------
310400000000     C                   MOVEL     DRI(3)        PICSTA
310500071210      *
310600071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
310700071211     c                   eval      Righe_da_stampare = 6
310800071211     c                   exsr      solo_x_STT
310900010920     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
311000010920     C                   WRITE     SE62RS                                       *ULTERIORE DESCR SER
311100071210      *
311200020823     C                   EXSR      STATES
311300071210      *
311400071211     c                   eval      Righe_da_stampare = 1
311500071211     c                   exsr      solo_x_STT
311600010920     C                   WRITE     SE62RV                                       *RIGA VUOTA
311700071210      *
311800020823     C                   EXSR      STATES
311900000000     C*
312000000000     C* AZZERO I TOTALI
312100000000     C                   Z-ADD     *ZEROS        PNW
312200000629     C                   Z-ADD     *ZEROS        PDW
312300000000     C                   Z-ADD     *ZEROS        PSW
312400000629     C                   Z-ADD     *ZEROS        PPW
312500000000     C                   Z-ADD     *ZEROS        PTW
312600000000     C                   Z-ADD     *ZEROS        PNW13C           13 0
312700000629     C                   Z-ADD     *ZEROS        PDW13C           13 0
312800000000     C                   Z-ADD     *ZEROS        PSW13C           13 0
312900000629     C                   Z-ADD     *ZEROS        PPW13C           13 0
313000000000     C                   Z-ADD     *ZEROS        PTW13C           13 0
313100000000     C*
313200000000     C* CALCOLO I TOALI MESE PER MESE DEI N/R/E E TOTALI
313300000000     C                   Z-ADD     1             J
313400000000DO  1C     1             DO        PARO          J
313500000000     C     J             OCCUR     CAFF
313600000000     C                   ADD       CNC           PNW                            *TOTALI CORRIERE
313700000629     C                   ADD       CDC           PDW                            *TOTALI DISTRIBUZION
313800000000     C                   ADD       CEC           PSW                            *TOTALI ESPRESSI
313900091214     C                   ADD       CHC           PSW                            *TOTALI ESPRESSI h10
314000000629     C                   ADD       CPC           PPW                            *TOTALI POSTE
314100000000     C     PTW           ADD       CNC           PTW
314200000629     C     PTW           ADD       CDC           PTW
314300000629     C     PTW           ADD       CEC           PTW
314400091214     C     PTW           ADD       CHC           PTW
314500000629     C     PTW           ADD       CPC           PTW                            *TOTALI C + D + E +
314600000000E   1C                   ENDDO
314700000000     C*
314800000000     C* TREDICESIMI TOTALI
314900000000     C                   XFOOT     PNW           PNW13C                         *13° TOT CORRIERE
315000000629     C                   XFOOT     PDW           PDW13C                         *13° TOT DISTRIBUZ.N
315100000000     C                   XFOOT     PSW           PSW13C                         *13° TOT ESPRESSI
315200000629     C                   XFOOT     PPW           PPW13C                         *13° TOT POSTE
315300000000     C                   XFOOT     PTW           PTW13C                         *13° TOT TOTALI
315400000000     C*
315500000000     C* PER OGNI ORA CONSEGNA DA STAMPARE
315600000000     C                   Z-ADD     1             J
315700000000DO  1C     1             DO        PARO          J
315800000000     C     J             OCCUR     CAFF
315900000000     C*--------------------
316000000000     C* STAMPA RIGA ORA DI CONSEGNA
316100000000     C*--------------------
316200000000     C                   MOVEL     *BLANKS       VALSER           23
316300000000     C                   MOVEL     DORE(J)       VALSER                         *ORA DI COMSEGNA
316400000000     C                   MOVEL     VALSER        $DE              23            *DESCRIZIONE RIGA
316500000000     C                   EXSR      STAAFX
316600000000     C*
316700000000E   1C                   ENDDO
316800000000     C*
316900071211     c                   eval      Righe_da_stampare = 1
317000071211     c                   exsr      solo_x_STT
317100010920     C                   WRITE     SE62RI                                       *RIGA FINE PROSPETTO
317200071210      *
317300020823     C                   EXSR      STATES
317400000000     C*
317500000000     C                   ENDSR
317600000000     C*------------------------------------------------------------------------*
317700000000     C*?STAAFX - IMPOSTA STAMPA RIGA XXX STATISTICA AFFIDABILITA' SPEDIZIONI
317800000000     C*------------------------------------------------------------------------*
317900000000     C     STAAFX        BEGSR
318000000000     C*
318100000000     C* CALCOLO LE INCIDENZE PER MESE DEI SINGOLI TIPI SERVIZI SUL TOTALE DELLE
318200000000     C* SPEDIZIONI DELL'ORA DI CONSEGNA IN LINEA
318300000000     C                   Z-ADD     1             E                 4 0
318400000000DO  1C     1             DO        SPARM         E
318500000000     C*--------------------
318600000000     C* INCIDENZA CORRIERE
318700000000     C*--------------------
318800000000     C                   Z-ADD     CNC(E)        WVAW             16 3          *TOT SERVIZIO ORA
318900000000     C                   Z-ADD     PNW(E)        WTXW             16 3          *TOT SERVIZIO ORE
319000000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
319100000000     C                   EXSR      CALINC
319200000000     C                   Z-ADD     WINW          $P1(E)                         *INCIDENZA CORRIERE
319300000000     C*--------------------
319400000000     C* INCIDENZA DISTRIBUZIONE
319500000000     C*--------------------
319600000629     C                   Z-ADD     CDC(E)        WVAW             16 3          *TOT SERVIZIO ORA
319700000629     C                   Z-ADD     PDW(E)        WTXW             16 3          *TOT SERVIZIO ORE
319800000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
319900000000     C                   EXSR      CALINC
320000000000     C                   Z-ADD     WINW          $P2(E)                         *INCIDENZA DISTRIBUZ
320100000000     C*--------------------
320200000000     C* INCIDENZA ESPRESSI
320300000000     C*--------------------
320400000000     C                   Z-ADD     CEC(E)        WVAW             16 3          *TOT SERVIZIO ORA
320500091214     C                   add       ChC(E)        WVAW             16 3          *TOT SERVIZIO ORA h1
320600000000     C                   Z-ADD     PSW(E)        WTXW             16 3          *TOT SERVIZIO ORE
320700000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
320800000000     C                   EXSR      CALINC
320900000000     C                   Z-ADD     WINW          $P3(E)                         *INCIDENZA ESPRESSI
321000000629     C*--------------------
321100000629     C* INCIDENZA POSTE
321200000629     C*--------------------
321300000629     C                   Z-ADD     CPC(E)        WVAW             16 3          *TOT SERVIZIO ORA
321400000629     C                   Z-ADD     PPW(E)        WTXW             16 3          *TOT SERVIZIO ORE
321500000629     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
321600000629     C                   EXSR      CALINC
321700000629     C                   Z-ADD     WINW          $P4(E)                         *INCIDENZA ESPRESSI
321800000000     C*--------------------
321900000000     C* INCIDENZA TOTALE
322000000000     C*--------------------
322100000000     C                   Z-ADD     CNC(E)        WVAW             16 3          *TOTALE SERVIZI ORA
322200000629     C                   ADD       CDC(E)        WVAW
322300000000     C                   ADD       CEC(E)        WVAW
322400091214     C                   ADD       ChC(E)        WVAW
322500000629     C                   ADD       CPC(E)        WVAW
322600000000     C                   Z-ADD     PTW(E)        WTXW             16 3          *TOTALE SERVIZI ORE
322700000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
322800000000     C                   EXSR      CALINC
322900000629     C                   Z-ADD     WINW          $P5(E)                         *INCIDENZA ESPRESSI
323000000000E   1C                   ENDDO
323100000000     C*--------------------
323200000000     C* 13^ COLONNA
323300000000     C*--------------------
323400000000     C***
323500000000     C* INCIDENZA CORRIERE
323600000000     C***
323700000000     C                   XFOOT     CNC           WVAW             16 3          *TOT SERVIZIO ORA
323800000000     C                   Z-ADD     PNW13C        WTXW             16 3          *TOT SERVIZIO ORE
323900000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
324000000000     C                   EXSR      CALINC
324100000000     C                   Z-ADD     WINW          $P113C            2 0          *13^INCIDENZA CORRIE
324200000000     C***
324300000000     C* INCIDENZA DISTRIBUZIONE
324400000000     C***
324500000629     C                   XFOOT     CDC           WVAW             16 3          *TOT SERVIZIO ORA
324600000629     C                   Z-ADD     PDW13C        WTXW             16 3          *TOT SERVIZIO ORE
324700000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
324800000000     C                   EXSR      CALINC
324900000000     C                   Z-ADD     WINW          $P213C            2 0          *13^INCIDENZA DISTRI
325000000000     C***
325100000000     C* INCIDENZA ESPRESSI
325200000000     C***
325300000000     C                   XFOOT     CEC           WVAW             16 3          *TOT SERVIZIO ORA
325400091214     C                   XFOOT     ChC           WVAWh            16 3          *TOT SERVIZIO ORA h1
325500091214     C                   add       WVAWh         WVAW             16 3          *TOT SERVIZIO ORA
325600000000     C                   Z-ADD     PSW13C        WTXW             16 3          *TOT SERVIZIO ORE
325700000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
325800000000     C                   EXSR      CALINC
325900000000     C                   Z-ADD     WINW          $P313C            2 0          *13^ INCID ESPRESSI
326000000629     C***
326100000629     C* INCIDENZA POSTE
326200000629     C***
326300000629     C                   XFOOT     CPC           WVAW             16 3          *TOT SERVIZIO ORA
326400000629     C                   Z-ADD     PPW13C        WTXW             16 3          *TOT SERVIZIO ORE
326500000629     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
326600000629     C                   EXSR      CALINC
326700000629     C                   Z-ADD     WINW          $P413C            2 0          *13^ INCID POSTE
326800000000     C***
326900000000     C* INCIDENZA TOTALE
327000000000     C***
327100000000     C                   XFOOT     CNC           WVNW             13 0          *TOTALE CORRIERE ORA
327200000629     C                   XFOOT     CDC           WVRW             13 0          *TOTALE DISTRIBUZ OR
327300000000     C                   XFOOT     CEC           WVEW             13 0          *TOTALE ESPRESSI ORA
327400091214     C                   XFOOT     ChC           WVEWh            13 0          *TOTALE ESPRESSI ORA
327500091214     C                   add       WVEWh         WVEW             13 0          *TOTALE ESPRESSI ORA
327600000629     C                   XFOOT     CPC           WVPW             13 0          *TOTALE POSTE ORA
327700000000     C                   Z-ADD     WVNW          WVAW
327800000000     C     WVAW          ADD       WVRW          WVAW
327900000629     C     WVAW          ADD       WVEW          WVAW
328000000629     C     WVAW          ADD       WVPW          WVAW                           *TOTALE SERVIZI ORA
328100000000     C                   Z-ADD     PTW13C        WTXW             16 3          *TOTALE SERVIZI ORE
328200000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
328300000000     C                   EXSR      CALINC
328400000629     C                   Z-ADD     WINW          $P513C            2 0          *13^INCIDENZA TOTALE
328500000000     C*
328600000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
328700000000     C                   EXSR      STACA2                                       *RIGA PERCENTUALE 2
328800000000     C*
328900000000     C                   ENDSR
329000000000     C*------------------------------------------------------------------------*
329100000000     C*?STAAMM - IMPOSTA STAMPA STATISTICA "AMMINISTRAZIONE"
329200000000     C*          INDICI: J(4,0)
329300000000     C*------------------------------------------------------------------------*
329400000000     C     STAAMM        BEGSR
329500000000     C*--------------------
329600000000     C* PARAMETRI
329700000000     C*--------------------
329800000000IF  1C     PASPAR        IFEQ      '0'                                          *NON HO ANCORA STAMP
329900000000     C                   EXSR      STAPAR
330000000000E   1C                   ENDIF
330100000000     C*--------------------
330200000000     C* DATI GENERALI CLIENTE
330300000000     C*--------------------
330400000000IF  1C     PASDGC        IFEQ      '0'                                          *NON HO ANCORA STAMP
330500000000     C                   EXSR      STADGC
330600000000E   1C                   ENDIF
330700000000     C*--------------------
330800000000     C* INTESTAZIONE
330900000000     C*--------------------
331000000000     C                   MOVEL     DRI(5)        PICSTA
331100071210      *
331200071211     C                   eval      wstINT = '0'                                 *NO INTESTAZIONE
331300071211     c                   eval      Righe_da_stampare = 5
331400071211     c                   exsr      solo_x_STT
331500071211     C                   WRITE     SE62PI                                       *TIPO STATISTICA+MES
331600071210      *
331700000000     C                   MOVEL     '0'           DICUI                          *PER STAMPARE "DI CU
331800000000     C*--------------------
331900000000     C* STAMPA  F A T T U R A T O
332000000000     C*--------------------
332100970724     C     AFC           ADD       ANC           AFC                            *AGGIUNGE NOTE
332200970724     C     AFC           ADD       ARC           AFC                            *AGGIUNGE RETTIFICHE
332300000000     C                   MOVEL     DME(10)       $DE
332400000000     C                   MOVEL     'RIC'         TIPVAL                         FATTURATO COME RICAV
332500000000     C                   Z-ADD     AFC           $VA
332600000000     C                   EXSR      STAAMX
332700000000     C*--------------------
332800000000     C* STAMPA  N O T E   A C C R E D I T O
332900000000     C*--------------------
333000000000     C                   XFOOT     ANC           COM$VA
333100000000IF  1C     COM$VA        IFNE      *ZEROS                                       *SE MEMORIZZATO QC.
333200000000IF  2C     DICUI         IFEQ      '0'
333300000000     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
333400000000E   2C                   ENDIF
333500000000     C*
333600000000     C                   MOVEL     DME(11)       $DE
333700000000     C                   MOVEL     'RIC'         TIPVAL                         *NOTE COME RICAVI
333800000000     C                   Z-ADD     ANC           $VA
333900000000     C                   EXSR      STAAMX
334000000000     C*--------------------
334100000000     C* STAMPA  P E R C E N T U A L E   N O T E
334200000000     C*--------------------
334300000000     C* PER OGNI MESE CALCOLO LA PERCENTUALE
334400000000     C                   Z-ADD     1             J
334500000000DO  2C     1             DO        SPARM         J
334600000000     C                   Z-ADD     AFC(J)        WFAW             16 3          *FATTURATO
334700000000     C                   Z-ADD     ANC(J)        WNRW             16 3          *NOTE O RETTIFICHE
334800000629     C                   Z-ADD     *ZEROS        WPDW              4 1          *PERC SUL FATTURATO
334900000000     C*
335000000000     C                   EXSR      CALPER
335100000629     C                   Z-ADD     WPDW          APW(J)                         *SCHIERA PERCENTUALI
335200000000E   2C                   ENDDO
335300000000     C*
335400000000     C                   MOVEL     DME(13)       $DE
335500000000     C                   MOVEL     'PER'         TIPVAL                         *PERCENTUALI
335600000000     C                   Z-ADD     APW           $VA
335700000000     C*
335800000000     C* CALCOLO I TOTALI FATTURATO E NOTE PER 13^ COLONNA
335900000000     C                   XFOOT     AFC           $FA              16 3
336000000000     C                   XFOOT     ANC           $NR              16 3
336100000000     C                   EXSR      STAAMX
336200000000     C*
336300071211     c                   eval      Righe_da_stampare = 1
336400071211     c                   exsr      solo_x_STT
336500010920     C                   WRITE     SE62RV                                       *RIGA VUOTA
336600071210      *
336700020823     C                   EXSR      STATES
336800000000E   1C                   ENDIF
336900000000     C*--------------------
337000000000     C* STAMPA  R E T T I F I C H E
337100000000     C*--------------------
337200000000     C                   XFOOT     ARC           COM$VA
337300000000IF  1C     COM$VA        IFNE      *ZEROS                                       *SE MEMORIZZATO QC.
337400000000IF  2C     DICUI         IFEQ      '0'
337500000000     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
337600000000E   2C                   ENDIF
337700000000     C*
337800000000     C                   MOVEL     DME(12)       $DE
337900000000     C                   MOVEL     'RIC'         TIPVAL                         *NOTE COME RICAVI
338000000000     C                   Z-ADD     ARC           $VA
338100000000     C                   EXSR      STAAMX
338200071210      *
338300071211     c                   eval      Righe_da_stampare = 1
338400071211     c                   exsr      solo_x_STT
338500010920     C                   WRITE     SE62RV
338600071210      *
338700020823     C                   EXSR      STATES
338800000000E   1C                   ENDIF
338900070628      *                                                    ?
339000070628      *  Ciclo per visualizzazione dell Varie del cliente  ?
339100070628     c                   do        sav_CA        CA
339200070628      **?  **  aggancia la chiave delle schiere tab.varie "CA"
339300070628     c                   move      SkOrd(CA)     CAkey             3 0
339400070628      **
339500070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
339600070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
339700070628     C                   Z-ADD     1             P
339800070628     C                   SETOFF                                           28
339900070628     C     SkVar(CAkey)  LOOKUP    VAR(P)                                 28
340000070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
340100070628     C     P             OCCUR     CAMM
340200070628     C                   ADD       ARI           $VA                            *RICAVI "I"
340300070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "I"
340400070628E   1C                   ENDIF
340500070628     C*
340600070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
340700070628     C                   XFOOT     $VA           COM$VA           16 3
340800070628     C                   XFOOT     $V2           COM$V2           16 3
340900070628IF  1C     COM$VA        IFNE      *ZEROS
341000070628     C     COM$V2        ORNE      *ZEROS
341100070628IF  2C     DICUI         IFEQ      '0'
341200070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
341300070628E   2C                   ENDIF
341400070628     C*--------------------
341500070628     C* STAMPA  R I C A V I  G I A C E N Z E
341600070628     C*--------------------
341700070628     C                   MOVEL     SkDsc(CAkey)  $DE
341800070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
341900070628     C                   EXSR      STAAMX
342000070628     C*--------------------
342100070628     C* STAMPA  N U M E R O  G I A C E N Z E
342200070628     C*--------------------
342300070628     C                   Z-ADD     $V2           $VA
342400070628     C                   MOVEL     DME(15)       $DE
342500070628      *?    Eccezione...(#) non è un valore di ricavo ma Numerico .  ?
342600070628     c                   if        SkVar(CAkey) = '#'
342700070628     C                   move      ' NR.'        $DE                            *DESCRIZIONE VALORE
342800070628     c                   end
342900070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
343000070628     C                   EXSR      STAAMX
343100071210      *
343200071211     c                   eval      Righe_da_stampare = 1
343300071211     c                   exsr      solo_x_STT
343400070628     C                   WRITE     SE62RV
343500071210      *
343600070628     C                   EXSR      STATES
343700070628E   1C                   ENDIF
343800070628      **?  **  ?
343900070628E   0C                   EndDO
344000070628      **?  *------------------------------------*  ?
344100070628      **?  * per provare  al momento non cancello le specifiche ma le salto *  ?
344200070628      **?  *------------------------------------*  ?
344300070628      **?  **  ?
344400070628     c                   GOTO      Salta_TUTTO
344500070628      *                                                    ?
344600070628     C*
344700070628      **?  * Le vecchie specifiche sono state parcheggiate nella Routine OLD *  ?
344800070628      **?  * a PRO-MEMORIA per verificare che tutto funzioni correttamente.  *  ?
344900070628      **?  * In un prossimo futuro POTRANNO e DOVRANNO essere cancellate !!  *  ?
345000070628     c                   Exsr      Specifiche_OLD
345100070628      **?  **  ?
345200070628      **?  *-----------------------*  ?
345300070628     c     Salta_TUTTO   tag
345400070628      **?  *-----------------------*  ?
345500070628      **?  **  ?
345600070628     C*
345700010921     C*
345800071211     c                   eval      Righe_da_stampare = 1
345900071211     c                   exsr      solo_x_STT
346000010920     C                   WRITE     SE62RI                                       *RIGA FINE PROSPETTO
346100071210      *
346200020823     C                   EXSR      STATES
346300000000     C*
346400000000     C                   ENDSR
346500070628     C*------------------------------------------------------------------------*
346600070628      *?  VECCHIE SPECIFICHE SOSTITUITE CON Tabella "CA" correlata alla "CC"...  ?
346700070628     C*------------------------------------------------------------------------*
346800070628     C     Specifiche_OLDBEGSR
346900070628     C*
347000070628     C*--------------------
347100070628     C* CALCOLO GIACENZE -->  GIACENZE = (VARIA "I")
347200070628     C*--------------------
347300070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
347400070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
347500070628     C                   Z-ADD     1             P
347600070628     C                   SETOFF                                           28
347700070628     C     'I'           LOOKUP    VAR(P)                                 28
347800070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
347900070628     C     P             OCCUR     CAMM
348000070628     C                   ADD       ARI           $VA                            *RICAVI "I"
348100070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "I"
348200070628E   1C                   ENDIF
348300070628     C*
348400070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
348500070628     C                   XFOOT     $VA           COM$VA           16 3
348600070628     C                   XFOOT     $V2           COM$V2           16 3
348700070628IF  1C     COM$VA        IFNE      *ZEROS
348800070628     C     COM$V2        ORNE      *ZEROS
348900070628IF  2C     DICUI         IFEQ      '0'
349000070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
349100070628E   2C                   ENDIF
349200070628     C*--------------------
349300070628     C* STAMPA  R I C A V I  G I A C E N Z E
349400070628     C*--------------------
349500070628     C                   MOVEL     DME(14)       $DE
349600070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
349700070628     C                   EXSR      STAAMX
349800070628     C*--------------------
349900070628     C* STAMPA  N U M E R O  G I A C E N Z E
350000070628     C*--------------------
350100070628     C                   Z-ADD     $V2           $VA
350200070628     C                   MOVEL     DME(15)       $DE
350300070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
350400070628     C                   EXSR      STAAMX
350500071210      *
350600071211     c                   eval      Righe_da_stampare = 1
350700071211     c                   exsr      solo_x_STT
350800070628     C                   WRITE     SE62RV
350900071210      *
351000070628     C                   EXSR      STATES
351100070628E   1C                   ENDIF
351200070628     C*--------------------
351300070628     C* CALCOLO ASSICURAZIONI -->  ASSICURAZIONI = (VARIA "E" + VARIA "R")
351400070628     C*--------------------
351500070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
351600070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
351700070628     C                   Z-ADD     1             P
351800070628     C                   SETOFF                                           28
351900070628     C     'E'           LOOKUP    VAR(P)                                 28
352000070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
352100070628     C     P             OCCUR     CAMM
352200070628     C                   ADD       ARI           $VA                            *RICAVI "E"
352300070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "E"
352400070628E   1C                   ENDIF
352500070628     C*
352600070628     C                   Z-ADD     1             P
352700070628     C                   SETOFF                                           28
352800070628     C     'R'           LOOKUP    VAR(P)                                 28
352900070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "R
353000070628     C     P             OCCUR     CAMM
353100070628     C     $VA           ADD       ARI           $VA                            *RICAVI "R"
353200070628     C     $V2           ADD       ASP           $V2                            *SPEDIZIONI "R"
353300070628E   1C                   ENDIF
353400070628     C*
353500070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
353600070628     C                   XFOOT     $VA           COM$VA           16 3
353700070628     C                   XFOOT     $V2           COM$V2           16 3
353800070628IF  1C     COM$VA        IFNE      *ZEROS
353900070628     C     COM$V2        ORNE      *ZEROS
354000070628IF  2C     DICUI         IFEQ      '0'
354100070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
354200070628E   2C                   ENDIF
354300070628     C*--------------------
354400070628     C* STAMPA  R I C A V I   A S S I C U R A Z I O N I
354500070628     C*--------------------
354600070628     C                   MOVEL     DME(21)       $DE
354700070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
354800070628     C                   EXSR      STAAMX
354900070628     C*--------------------
355000070628     C* STAMPA  N U M E R O   A S S I C U R A Z I O N I
355100070628     C*--------------------
355200070628     C                   Z-ADD     $V2           $VA
355300070628     C                   MOVEL     DME(15)       $DE
355400070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
355500070628     C                   EXSR      STAAMX
355600071210      *
355700071211     c                   eval      Righe_da_stampare = 1
355800071211     c                   exsr      solo_x_STT
355900070628     C                   WRITE     SE62RV
356000071210      *
356100070628     C                   EXSR      STATES
356200070628E   1C                   ENDIF
356300070628     C*--------------------
356400070628     C* CONTRASSEGNI --> CONTRASSEGNI = VARIA "G"
356500070628     C*--------------------
356600070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
356700070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
356800070628     C                   Z-ADD     1             P
356900070628     C                   SETOFF                                           28
357000070628     C     'G'           LOOKUP    VAR(P)                                 28
357100070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "G
357200070628     C     P             OCCUR     CAMM
357300070628     C                   ADD       ARI           $VA                            *RICAVI "G"
357400070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "G"
357500070628E   1C                   ENDIF
357600070628     C*
357700070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
357800070628     C                   XFOOT     $VA           COM$VA           16 3
357900070628     C                   XFOOT     $V2           COM$V2           16 3
358000070628IF  1C     COM$VA        IFNE      *ZEROS
358100070628     C     COM$V2        ORNE      *ZEROS
358200070628IF  2C     DICUI         IFEQ      '0'
358300070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
358400070628E   2C                   ENDIF
358500070628     C*--------------------
358600070628     C* STAMPA  R I C A V I   C O N T R A S S E G N O
358700070628     C*--------------------
358800070628     C                   MOVEL     DME(22)       $DE
358900070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
359000070628     C                   EXSR      STAAMX
359100070628     C*--------------------
359200070628     C* STAMPA  N U M E R O   C O N T R A S S E G N O
359300070628     C*--------------------
359400070628     C                   Z-ADD     $V2           $VA
359500070628     C                   MOVEL     DME(15)       $DE
359600070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
359700070628     C                   EXSR      STAAMX
359800071210      *
359900071211     c                   eval      Righe_da_stampare = 1
360000071211     c                   exsr      solo_x_STT
360100070628     C                   WRITE     SE62RV
360200071210      *
360300070628     C                   EXSR      STATES
360400070628E   1C                   ENDIF
360500070628     C*--------------------
360600070628     C* CONSEGNE AI PIANI --> VARIA = "P"
360700070628     C*--------------------
360800070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
360900070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
361000070628     C                   Z-ADD     1             P
361100070628     C                   SETOFF                                           28
361200070628     C     'P'           LOOKUP    VAR(P)                                 28
361300070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "P"
361400070628     C     P             OCCUR     CAMM
361500070628     C                   ADD       ARI           $VA                            *RICAVI "P"
361600070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "P"
361700070628E   1C                   ENDIF
361800070628     C*
361900070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
362000070628     C                   XFOOT     $VA           COM$VA           16 3
362100070628     C                   XFOOT     $V2           COM$V2           16 3
362200070628IF  1C     COM$VA        IFNE      *ZEROS
362300070628     C     COM$V2        ORNE      *ZEROS
362400070628IF  2C     DICUI         IFEQ      '0'
362500070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
362600070628E   2C                   ENDIF
362700070628     C*--------------------
362800070628     C* STAMPA  R I C A V I   C O N S E G N A   A I   P I A N I
362900070628     C*--------------------
363000070628     C                   MOVEL     DME(17)       $DE
363100070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
363200070628     C                   EXSR      STAAMX
363300070628     C*--------------------
363400070628     C* STAMPA  N U M E R O   C O N S E G N A   A I   P I A N I
363500070628     C*--------------------
363600070628     C                   Z-ADD     $V2           $VA
363700070628     C                   MOVEL     DME(15)       $DE
363800070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
363900070628     C                   EXSR      STAAMX
364000071210      *
364100071211     c                   eval      Righe_da_stampare = 1
364200071211     c                   exsr      solo_x_STT
364300070628     C                   WRITE     SE62RV
364400071210      *
364500070628     C                   EXSR      STATES
364600070628E   1C                   ENDIF
364700070628     C*--------------------
364800070628     C* CONSEGNE PER APPUNTAMENTO --> VARIA = "A"
364900070628     C*--------------------
365000070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
365100070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
365200070628     C                   Z-ADD     1             P
365300070628     C                   SETOFF                                           28
365400070628     C     'A'           LOOKUP    VAR(P)                                 28
365500070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "A"
365600070628     C     P             OCCUR     CAMM
365700070628     C                   ADD       ARI           $VA                            *RICAVI "A"
365800070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "A"
365900070628E   1C                   ENDIF
366000070628     C*
366100070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
366200070628     C                   XFOOT     $VA           COM$VA           16 3
366300070628     C                   XFOOT     $V2           COM$V2           16 3
366400070628IF  1C     COM$VA        IFNE      *ZEROS
366500070628     C     COM$V2        ORNE      *ZEROS
366600070628IF  2C     DICUI         IFEQ      '0'
366700070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
366800070628E   2C                   ENDIF
366900070628     C*--------------------
367000070628     C* STAMPA  R I C A V I   C O N S E G N A   P E R   A P P U N T A M E N T O
367100070628     C*--------------------
367200070628     C                   MOVEL     DME(18)       $DE
367300070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
367400070628     C                   EXSR      STAAMX
367500070628     C*--------------------
367600070628     C* STAMPA  N U M E R O   C O N S E G N A   P E R   A P P U N T A M E N T O
367700070628     C*--------------------
367800070628     C                   Z-ADD     $V2           $VA
367900070628     C                   MOVEL     DME(15)       $DE
368000070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
368100070628     C                   EXSR      STAAMX
368200071210      *
368300071211     c                   eval      Righe_da_stampare = 1
368400071211     c                   exsr      solo_x_STT
368500070628     C                   WRITE     SE62RV
368600071210      *
368700070628     C                   EXSR      STATES
368800070628E   1C                   ENDIF
368900070628     C*--------------------
369000070628     C* CONSEGNE AI SUPERMERCATI --> VARIA = "S"
369100070628     C*--------------------
369200070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
369300070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
369400070628     C                   Z-ADD     1             P
369500070628     C                   SETOFF                                           28
369600070628     C     'S'           LOOKUP    VAR(P)                                 28
369700070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "S"
369800070628     C     P             OCCUR     CAMM
369900070628     C                   ADD       ARI           $VA                            *RICAVI "S"
370000070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "S"
370100070628E   1C                   ENDIF
370200070628     C*
370300070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
370400070628     C                   XFOOT     $VA           COM$VA           16 3
370500070628     C                   XFOOT     $V2           COM$V2           16 3
370600070628IF  1C     COM$VA        IFNE      *ZEROS
370700070628     C     COM$V2        ORNE      *ZEROS
370800070628IF  2C     DICUI         IFEQ      '0'
370900070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
371000070628E   2C                   ENDIF
371100070628     C*--------------------
371200070628     C* STAMPA  R I C A V I   C O N S E G N A   A I   S U P E R M E R C A T I
371300070628     C*--------------------
371400070628     C                   MOVEL     DME(19)       $DE
371500070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
371600070628     C                   EXSR      STAAMX
371700070628     C*--------------------
371800070628     C* STAMPA  N U M E R O   C O N S E G N A   A I   S U P E R M E R C A T I
371900070628     C*--------------------
372000070628     C                   Z-ADD     $V2           $VA
372100070628     C                   MOVEL     DME(15)       $DE
372200070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
372300070628     C                   EXSR      STAAMX
372400071210      *
372500071211     c                   eval      Righe_da_stampare = 1
372600071211     c                   exsr      solo_x_STT
372700070628     C                   WRITE     SE62RV
372800071210      *
372900070628     C                   EXSR      STATES
373000070628E   1C                   ENDIF
373100070628     C*--------------------
373200070628     C* CONSEGNE FUORI MISURA --> VARIA = "F"
373300070628     C*--------------------
373400070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
373500070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
373600070628     C                   Z-ADD     1             P
373700070628     C                   SETOFF                                           28
373800070628     C     'F'           LOOKUP    VAR(P)                                 28
373900070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "F"
374000070628     C     P             OCCUR     CAMM
374100070628     C                   ADD       ARI           $VA                            *RICAVI "F"
374200070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "F"
374300070628E   1C                   ENDIF
374400070628     C*
374500070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
374600070628     C                   XFOOT     $VA           COM$VA           16 3
374700070628     C                   XFOOT     $V2           COM$V2           16 3
374800070628IF  1C     COM$VA        IFNE      *ZEROS
374900070628     C     COM$V2        ORNE      *ZEROS
375000070628IF  2C     DICUI         IFEQ      '0'
375100070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
375200070628E   2C                   ENDIF
375300070628     C*--------------------
375400070628     C* STAMPA  R I C A V I   C O N S E G N A   F U O R I   M I S U R A
375500070628     C*--------------------
375600070628     C                   MOVEL     DME(20)       $DE
375700070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
375800070628     C                   EXSR      STAAMX
375900070628     C*--------------------
376000070628     C* STAMPA  N U M E R O   C O N S E G N A   F U O R I   M I S U R A
376100070628     C*--------------------
376200070628     C                   Z-ADD     $V2           $VA
376300070628     C                   MOVEL     DME(15)       $DE
376400070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
376500070628     C                   EXSR      STAAMX
376600071210      *
376700071211     c                   eval      Righe_da_stampare = 1
376800071211     c                   exsr      solo_x_STT
376900070628     C                   WRITE     SE62RV
377000071210      *
377100070628     C                   EXSR      STATES
377200070628E   1C                   ENDIF
377300070628     C*--------------------
377400070628     C* CONSEGNE CENTRO STORICO --> VARIA = "Q"
377500070628     C*--------------------
377600070628     C*                  Z-ADD     *ZEROS        $VA                            *PER I RICAVI
377700070628     C*                  Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
377800070628     C*                  Z-ADD     1             P
377900070628     C*                  SETOFF                                           28
378000070628     C*    'Q'           LOOKUP    VAR(P)                                 28
378100070628IF  1C*    *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "Q"
378200070628     C*    P             OCCUR     CAMM
378300070628     C*                  ADD       ARI           $VA                            *RICAVI "Q"
378400070628     C*                  ADD       ASP           $V2                            *SPEDIZIONI "Q"
378500070628E   1C*                  ENDIF
378600070628     C*
378700070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
378800070628     C*                  XFOOT     $VA           COM$VA           16 3
378900070628     C*                  XFOOT     $V2           COM$V2           16 3
379000070628IF  1C*    COM$VA        IFNE      *ZEROS
379100070628     C*    COM$V2        ORNE      *ZEROS
379200070628IF  2C*    DICUI         IFEQ      '0'
379300070628     C*                  EXSR      STACUI                                       *STAMPA RIGA "DI CUI
379400070628E   2C*                  ENDIF
379500070628     C*--------------------
379600070628     C* STAMPA  R I C A V I   C O N S E G N A   C E N T R O   S T O R I C O
379700070628     C*--------------------
379800070628     C*                  MOVEL     DME(23)       $DE
379900070628     C*                  MOVEL     'RIC'         TIPVAL                         *RICAVI
380000070628     C*                  EXSR      STAAMX
380100070628     C*--------------------
380200070628     C* STAMPA  N U M E R O   C O N S E G N A   C E N T R O   S T O R I C O
380300070628     C*--------------------
380400070628     C*                  Z-ADD     $V2           $VA
380500070628     C*                  MOVEL     DME(15)       $DE
380600070628     C*                  MOVEL     'NUM'         TIPVAL                         *NUMERO
380700070628     C*                  EXSR      STAAMX
380800071210     c*
380900071211     c*                  eval      Righe_da_stampare = 1
381000071211     c*                  exsr      solo_x_STT
381100070628     C*                  WRITE     SE62RV
381200071210     c*
381300070628     C*                  EXSR      STATES
381400070628E   1C*                  ENDIF
381500070628     C*--------------------
381600070628     C* CONSEGNE LOCALITA' DISAGIATE --> VARIA = "K"
381700070628     C*--------------------
381800070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
381900070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
382000070628     C                   Z-ADD     1             P
382100070628     C                   SETOFF                                           28
382200070628     C     'K'           LOOKUP    VAR(P)                                 28
382300070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "K"
382400070628     C     P             OCCUR     CAMM
382500070628     C                   ADD       ARI           $VA                            *RICAVI "K"
382600070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "K"
382700070628E   1C                   ENDIF
382800070628     C*
382900070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
383000070628     C                   XFOOT     $VA           COM$VA           16 3
383100070628     C                   XFOOT     $V2           COM$V2           16 3
383200070628IF  1C     COM$VA        IFNE      *ZEROS
383300070628     C     COM$V2        ORNE      *ZEROS
383400070628IF  2C     DICUI         IFEQ      '0'
383500070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
383600070628E   2C                   ENDIF
383700070628     C*--------------------
383800070628     C* STAMPA  R I C A V I   C O N S E G N A   L O C A L I T A'   D I S A G I A T E
383900070628     C*--------------------
384000070628     C                   MOVEL     DME(24)       $DE
384100070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
384200070628     C                   EXSR      STAAMX
384300070628     C*--------------------
384400070628     C* STAMPA  N U M E R O   C O N S E G N A   L O C A L I T A'   D I S A G I A T E
384500070628     C*--------------------
384600070628     C                   Z-ADD     $V2           $VA
384700070628     C                   MOVEL     DME(15)       $DE
384800070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
384900070628     C                   EXSR      STAAMX
385000071210      *
385100071211     c                   eval      Righe_da_stampare = 1
385200071211     c                   exsr      solo_x_STT
385300070628     C                   WRITE     SE62RV
385400071210      *
385500070628     C                   EXSR      STATES
385600070628E   1C                   ENDIF
385700070628     C*--------------------
385800070628     C* CONSEGNE DDT --> VARIA = "B"
385900070628     C*--------------------
386000070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
386100070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
386200070628     C                   Z-ADD     1             P
386300070628     C                   SETOFF                                           28
386400070628     C     'B'           LOOKUP    VAR(P)                                 28
386500070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "B"
386600070628     C     P             OCCUR     CAMM
386700070628     C                   ADD       ARI           $VA                            *RICAVI "B"
386800070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "B"
386900070628E   1C                   ENDIF
387000070628     C*
387100070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
387200070628     C                   XFOOT     $VA           COM$VA           16 3
387300070628     C                   XFOOT     $V2           COM$V2           16 3
387400070628IF  1C     COM$VA        IFNE      *ZEROS
387500070628     C     COM$V2        ORNE      *ZEROS
387600070628IF  2C     DICUI         IFEQ      '0'
387700070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
387800070628E   2C                   ENDIF
387900070628     C*--------------------
388000070628     C* STAMPA  R I C A V I   C O N S E G N A   D D T
388100070628     C*--------------------
388200070628     C                   MOVEL     DME(25)       $DE
388300070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
388400070628     C                   EXSR      STAAMX
388500070628     C*--------------------
388600070628     C* STAMPA  N U M E R O   C O N S E G N A   D D T
388700070628     C*--------------------
388800070628     C                   Z-ADD     $V2           $VA
388900070628     C                   MOVEL     DME(15)       $DE
389000070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
389100070628     C                   EXSR      STAAMX
389200071210      *
389300071211     c                   eval      Righe_da_stampare = 1
389400071211     c                   exsr      solo_x_STT
389500070628     C                   WRITE     SE62RV
389600071210      *
389700070628     C                   EXSR      STATES
389800070628E   1C                   ENDIF
389900070628     C*--------------------
390000070628     C* RITIRI --> VARIA = "U"
390100070628     C*--------------------
390200070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
390300070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
390400070628     C                   Z-ADD     1             P
390500070628     C                   SETOFF                                           28
390600070628     C     'U'           LOOKUP    VAR(P)                                 28
390700070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "U"
390800070628     C     P             OCCUR     CAMM
390900070628     C                   ADD       ARI           $VA                            *RICAVI "U"
391000070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "U"
391100070628E   1C                   ENDIF
391200070628     C*
391300070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
391400070628     C                   XFOOT     $VA           COM$VA           16 3
391500070628     C                   XFOOT     $V2           COM$V2           16 3
391600070628IF  1C     COM$VA        IFNE      *ZEROS
391700070628     C     COM$V2        ORNE      *ZEROS
391800070628IF  2C     DICUI         IFEQ      '0'
391900070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
392000070628E   2C                   ENDIF
392100070628     C*--------------------
392200070628     C* STAMPA  R I C A V I   R I T I R I
392300070628     C*--------------------
392400070628     C                   MOVEL     DME(31)       $DE
392500070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
392600070628     C                   EXSR      STAAMX
392700070628     C*--------------------
392800070628     C* STAMPA  N U M E R O   R I T I R I
392900070628     C*--------------------
393000070628     C                   Z-ADD     $V2           $VA
393100070628     C                   MOVEL     DME(15)       $DE
393200070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
393300070628     C                   EXSR      STAAMX
393400071210      *
393500071211     c                   eval      Righe_da_stampare = 1
393600071211     c                   exsr      solo_x_STT
393700070628     C                   WRITE     SE62RV
393800071210      *
393900070628     C                   EXSR      STATES
394000070628E   1C                   ENDIF
394100070628     C*--------------------
394200070628     C* CONSEGNE ISOLA --> VARIA = "J"
394300070628     C*--------------------
394400070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
394500070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
394600070628     C                   Z-ADD     1             P
394700070628     C                   SETOFF                                           28
394800070628     C     'J'           LOOKUP    VAR(P)                                 28
394900070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "J"
395000070628     C     P             OCCUR     CAMM
395100070628     C                   ADD       ARI           $VA                            *RICAVI "J"
395200070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "J"
395300070628E   1C                   ENDIF
395400070628     C*
395500070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
395600070628     C                   XFOOT     $VA           COM$VA           16 3
395700070628     C                   XFOOT     $V2           COM$V2           16 3
395800070628IF  1C     COM$VA        IFNE      *ZEROS
395900070628     C     COM$V2        ORNE      *ZEROS
396000070628IF  2C     DICUI         IFEQ      '0'
396100070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
396200070628E   2C                   ENDIF
396300070628     C*--------------------
396400070628     C* STAMPA  R I C A V I   C O N S E G N E   I S O L A
396500070628     C*--------------------
396600070628     C                   MOVEL     DME(32)       $DE
396700070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
396800070628     C                   EXSR      STAAMX
396900070628     C*--------------------
397000070628     C* STAMPA  N U M E R O   C O N S E G N E   I S O L A
397100070628     C*--------------------
397200070628     C                   Z-ADD     $V2           $VA
397300070628     C                   MOVEL     DME(15)       $DE
397400070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
397500070628     C                   EXSR      STAAMX
397600071210      *
397700071211     c                   eval      Righe_da_stampare = 1
397800071211     c                   exsr      solo_x_STT
397900070628     C                   WRITE     SE62RV
398000071210      *
398100070628     C                   EXSR      STATES
398200070628E   1C                   ENDIF
398300070628     C*--------------------
398400070628     C* CONSEGNA DISAGIATA --> VARIA = "X"
398500070628     C*--------------------
398600070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
398700070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
398800070628     C                   Z-ADD     1             P
398900070628     C                   SETOFF                                           28
399000070628     C     'X'           LOOKUP    VAR(P)                                 28
399100070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "X"
399200070628     C     P             OCCUR     CAMM
399300070628     C                   ADD       ARI           $VA                            *RICAVI "X"
399400070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "X"
399500070628E   1C                   ENDIF
399600070628     C*
399700070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
399800070628     C                   XFOOT     $VA           COM$VA           16 3
399900070628     C                   XFOOT     $V2           COM$V2           16 3
400000070628IF  1C     COM$VA        IFNE      *ZEROS
400100070628     C     COM$V2        ORNE      *ZEROS
400200070628IF  2C     DICUI         IFEQ      '0'
400300070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
400400070628E   2C                   ENDIF
400500070628     C*--------------------
400600070628     C* STAMPA  R I C A V I   C O N S E G N E  D I S A G I A T E
400700070628     C*--------------------
400800070628     C                   MOVEL     DME(33)       $DE
400900070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
401000070628     C                   EXSR      STAAMX
401100070628     C*--------------------
401200070628     C* STAMPA  N U M E R O   C O N S E G N E  D I S A G I A T E
401300070628     C*--------------------
401400070628     C                   Z-ADD     $V2           $VA
401500070628     C                   MOVEL     DME(15)       $DE
401600070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
401700070628     C                   EXSR      STAAMX
401800071210      *
401900071211     c                   eval      Righe_da_stampare = 1
402000071211     c                   exsr      solo_x_STT
402100070628     C                   WRITE     SE62RV
402200071210      *
402300070628     C                   EXSR      STATES
402400070628E   1C                   ENDIF
402500070628     C*--------------------
402600070628     C* RITIRI ANNULLATI --> VARIA = "Y"
402700070628     C*--------------------
402800070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
402900070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
403000070628     C                   Z-ADD     1             P
403100070628     C                   SETOFF                                           28
403200070628     C     'Y'           LOOKUP    VAR(P)                                 28
403300070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "Y"
403400070628     C     P             OCCUR     CAMM
403500070628     C                   ADD       ARI           $VA                            *RICAVI "Y"
403600070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "Y"
403700070628E   1C                   ENDIF
403800070628     C*
403900070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
404000070628     C                   XFOOT     $VA           COM$VA           16 3
404100070628     C                   XFOOT     $V2           COM$V2           16 3
404200070628IF  1C     COM$VA        IFNE      *ZEROS
404300070628     C     COM$V2        ORNE      *ZEROS
404400070628IF  2C     DICUI         IFEQ      '0'
404500070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
404600070628E   2C                   ENDIF
404700070628     C*--------------------
404800070628     C* STAMPA  R I C A V I   R I T I R I  A N N U L L A T I
404900070628     C*--------------------
405000070628     C                   MOVEL     DME(34)       $DE
405100070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
405200070628     C                   EXSR      STAAMX
405300070628     C*--------------------
405400070628     C* STAMPA  N U M E R O   R I T I R I  A N N U L L A T I
405500070628     C*--------------------
405600070628     C                   Z-ADD     $V2           $VA
405700070628     C                   MOVEL     DME(15)       $DE
405800070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
405900070628     C                   EXSR      STAAMX
406000071210      *
406100071211     c                   eval      Righe_da_stampare = 1
406200071211     c                   exsr      solo_x_STT
406300070628     C                   WRITE     SE62RV
406400071210      *
406500070628     C                   EXSR      STATES
406600070628E   1C                   ENDIF
406700070628     C*--------------------
406800070628     C* RICERCA D'ARCHIVIO --> VARIA = "O"
406900070628     C*--------------------
407000070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
407100070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
407200070628     C                   Z-ADD     1             P
407300070628     C                   SETOFF                                           28
407400070628     C     'O'           LOOKUP    VAR(P)                                 28
407500070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "O"
407600070628     C     P             OCCUR     CAMM
407700070628     C                   ADD       ARI           $VA                            *RICAVI "O"
407800070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "O"
407900070628E   1C                   ENDIF
408000070628     C*
408100070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
408200070628     C                   XFOOT     $VA           COM$VA           16 3
408300070628     C                   XFOOT     $V2           COM$V2           16 3
408400070628IF  1C     COM$VA        IFNE      *ZEROS
408500070628     C     COM$V2        ORNE      *ZEROS
408600070628IF  2C     DICUI         IFEQ      '0'
408700070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
408800070628E   2C                   ENDIF
408900070628     C*--------------------
409000070628     C* STAMPA  R I C A V I   R I C E R C H E  D' A R C H I V I O
409100070628     C*--------------------
409200070628     C                   MOVEL     DME(35)       $DE
409300070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
409400070628     C                   EXSR      STAAMX
409500070628     C*--------------------
409600070628     C* STAMPA  N U M E R O   R I C E R C H E  D' A R C H I V I O
409700070628     C*--------------------
409800070628     C                   Z-ADD     $V2           $VA
409900070628     C                   MOVEL     DME(15)       $DE
410000070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
410100070628     C                   EXSR      STAAMX
410200071210      *
410300071211     c                   eval      Righe_da_stampare = 1
410400071211     c                   exsr      solo_x_STT
410500070628     C                   WRITE     SE62RV
410600071210      *
410700070628     C                   EXSR      STATES
410800070628E   1C                   ENDIF
410900070628     C*--------------------
411000070628     C* P.O.D. IMMAGE      --> VARIA = "a"
411100070628     C*--------------------
411200070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
411300070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
411400070628     C                   Z-ADD     1             P
411500070628     C                   SETOFF                                           28
411600070628     C     'a'           LOOKUP    VAR(P)                                 28
411700070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "O"
411800070628     C     P             OCCUR     CAMM
411900070628     C                   ADD       ARI           $VA                            *RICAVI "O"
412000070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "O"
412100070628E   1C                   ENDIF
412200070628     C*
412300070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
412400070628     C                   XFOOT     $VA           COM$VA           16 3
412500070628     C                   XFOOT     $V2           COM$V2           16 3
412600070628IF  1C     COM$VA        IFNE      *ZEROS
412700070628     C     COM$V2        ORNE      *ZEROS
412800070628IF  2C     DICUI         IFEQ      '0'
412900070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
413000070628E   2C                   ENDIF
413100070628     C*--------------------
413200070628     C* STAMPA  R I C A V I   P.O.D. IMMAGE
413300070628     C*--------------------
413400070628     C                   MOVEL     DME(46)       $DE
413500070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
413600070628     C                   EXSR      STAAMX
413700070628     C*--------------------
413800070628     C* STAMPA  N U M E R O   P.O.D. IMMAGE
413900070628     C*--------------------
414000070628     C                   Z-ADD     $V2           $VA
414100070628     C                   MOVEL     DME(15)       $DE
414200070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
414300070628     C                   EXSR      STAAMX
414400071210      *
414500071211     c                   eval      Righe_da_stampare = 1
414600071211     c                   exsr      solo_x_STT
414700070628     C                   WRITE     SE62RV
414800071210      *
414900070628     C                   EXSR      STATES
415000070628E   1C                   ENDIF
415100070628     C*--------------------
415200070628     C* DIRITTI DI PESATURA --> VARIA = "5"
415300070628     C*--------------------
415400070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
415500070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
415600070628     C                   Z-ADD     1             P
415700070628     C                   SETOFF                                           28
415800070628     C     '5'           LOOKUP    VAR(P)                                 28
415900070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "5"
416000070628     C     P             OCCUR     CAMM
416100070628     C                   ADD       ARI           $VA                            *RICAVI "5"
416200070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "5"
416300070628E   1C                   ENDIF
416400070628     C*
416500070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
416600070628     C                   XFOOT     $VA           COM$VA           16 3
416700070628     C                   XFOOT     $V2           COM$V2           16 3
416800070628IF  1C     COM$VA        IFNE      *ZEROS
416900070628     C     COM$V2        ORNE      *ZEROS
417000070628IF  2C     DICUI         IFEQ      '0'
417100070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
417200070628E   2C                   ENDIF
417300070628     C*--------------------
417400070628     C* STAMPA  R I C A V I   D I R I T T I  D I  P E S A T U R A
417500070628     C*--------------------
417600070628     C                   MOVEL     DME(36)       $DE
417700070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
417800070628     C                   EXSR      STAAMX
417900070628     C*--------------------
418000070628     C* STAMPA  N U M E R O   D I R I T T I  D I  P E S A T U R A
418100070628     C*--------------------
418200070628     C                   Z-ADD     $V2           $VA
418300070628     C                   MOVEL     DME(15)       $DE
418400070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
418500070628     C                   EXSR      STAAMX
418600071210      *
418700071211     c                   eval      Righe_da_stampare = 1
418800071211     c                   exsr      solo_x_STT
418900070628     C                   WRITE     SE62RV
419000071210      *
419100070628     C                   EXSR      STATES
419200070628E   1C                   ENDIF
419300070628     C*--------------------
419400070628     C* LASCIATO AVVISO --> VARIA = "#"
419500070628     C*--------------------
419600070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
419700070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
419800070628     C                   Z-ADD     1             P
419900070628     C                   SETOFF                                           28
420000070628     C     '#'           LOOKUP    VAR(P)                                 28
420100070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "B"
420200070628     C     P             OCCUR     CAMM
420300070628     C                   ADD       ARI           $VA                            *RICAVI "B"
420400070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "B"
420500070628E   1C                   ENDIF
420600070628     C*
420700070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
420800070628     C                   XFOOT     $VA           COM$VA           16 3
420900070628     C                   XFOOT     $V2           COM$V2           16 3
421000070628IF  1C     COM$VA        IFNE      *ZEROS
421100070628     C     COM$V2        ORNE      *ZEROS
421200070628IF  2C     DICUI         IFEQ      '0'
421300070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
421400070628E   2C                   ENDIF
421500070628     C*--------------------
421600070628     C* STAMPA  R I C A V I   LASCIATO AVVISO
421700070628     C*--------------------
421800070628     C                   MOVEL     DME(37)       $DE
421900070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
422000070628     C                   EXSR      STAAMX
422100070628     C*--------------------
422200070628     C* STAMPA  N U M E R O   LASCIATO AVVISO
422300070628     C*--------------------
422400070628     C                   Z-ADD     $V2           $VA
422500070628     C                   MOVEL     DME(15)       $DE
422600070628     C                   MOVE      ' NR.'        $DE
422700070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
422800070628     C                   EXSR      STAAMX
422900071210      *
423000071211     c                   eval      Righe_da_stampare = 1
423100071211     c                   exsr      solo_x_STT
423200070628     C                   WRITE     SE62RV
423300071210      *
423400070628     C                   EXSR      STATES
423500070628E   1C                   ENDIF
423600070628     C*--------------------
423700070628     C* ANN.PORTO ASSEGNATO--> VARIA = "&"
423800070628     C*--------------------
423900070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
424000070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
424100070628     C                   Z-ADD     1             P
424200070628     C                   SETOFF                                           28
424300070628     C     '&'           LOOKUP    VAR(P)                                 28
424400070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "&"
424500070628     C     P             OCCUR     CAMM
424600070628     C                   ADD       ARI           $VA                            *RICAVI "&"
424700070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "&"
424800070628E   1C                   ENDIF
424900070628     C*
425000070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
425100070628     C                   XFOOT     $VA           COM$VA           16 3
425200070628     C                   XFOOT     $V2           COM$V2           16 3
425300070628IF  1C     COM$VA        IFNE      *ZEROS
425400070628     C     COM$V2        ORNE      *ZEROS
425500070628IF  2C     DICUI         IFEQ      '0'
425600070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
425700070628E   2C                   ENDIF
425800070628     C*--------------------
425900070628     C* STAMPA  R I C A V I   ANN.PORTO ASSEGNATO
426000070628     C*--------------------
426100070628     C                   MOVEL     DME(47)       $DE
426200070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
426300070628     C                   EXSR      STAAMX
426400070628     C*--------------------
426500070628     C* STAMPA  N U M E R O   ANN.PORTO ASSEGNATO
426600070628     C*--------------------
426700070628     C                   Z-ADD     $V2           $VA
426800070628     C                   MOVEL     DME(15)       $DE
426900070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
427000070628     C                   EXSR      STAAMX
427100071210      *
427200071211     c                   eval      Righe_da_stampare = 1
427300071211     c                   exsr      solo_x_STT
427400070628     C                   WRITE     SE62RV
427500071210      *
427600070628     C                   EXSR      STATES
427700070628E   1C                   ENDIF
427800070628     C*--------------------
427900070628     C* DIROTTAMENTI       --> VARIA = "*"
428000070628     C*--------------------
428100070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
428200070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
428300070628     C                   Z-ADD     1             P
428400070628     C                   SETOFF                                           28
428500070628     C     '*'           LOOKUP    VAR(P)                                 28
428600070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "*"
428700070628     C     P             OCCUR     CAMM
428800070628     C                   ADD       ARI           $VA                            *RICAVI "*"
428900070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "*"
429000070628E   1C                   ENDIF
429100070628     C*
429200070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
429300070628     C                   XFOOT     $VA           COM$VA           16 3
429400070628     C                   XFOOT     $V2           COM$V2           16 3
429500070628IF  1C     COM$VA        IFNE      *ZEROS
429600070628     C     COM$V2        ORNE      *ZEROS
429700070628IF  2C     DICUI         IFEQ      '0'
429800070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
429900070628E   2C                   ENDIF
430000070628     C*--------------------
430100070628     C* STAMPA  R I C A V I   DIROTTAMENTI
430200070628     C*--------------------
430300070628     C                   MOVEL     DME(48)       $DE
430400070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
430500070628     C                   EXSR      STAAMX
430600070628     C*--------------------
430700070628     C* STAMPA  N U M E R O   DIROTTAMENTI
430800070628     C*--------------------
430900070628     C                   Z-ADD     $V2           $VA
431000070628     C                   MOVEL     DME(15)       $DE
431100070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
431200070628     C                   EXSR      STAAMX
431300071210      *
431400071211     c                   eval      Righe_da_stampare = 1
431500071211     c                   exsr      solo_x_STT
431600070628     C                   WRITE     SE62RV
431700071210      *
431800070628     C                   EXSR      STATES
431900070628E   1C                   ENDIF
432000070628     C*--------------------
432100070628     C* RITIRO BANCALI     --> VARIA = "="
432200070628     C*--------------------
432300070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
432400070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
432500070628     C                   Z-ADD     1             P
432600070628     C                   SETOFF                                           28
432700070628     C     '='           LOOKUP    VAR(P)                                 28
432800070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "="
432900070628     C     P             OCCUR     CAMM
433000070628     C                   ADD       ARI           $VA                            *RICAVI "="
433100070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "="
433200070628E   1C                   ENDIF
433300070628     C*
433400070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
433500070628     C                   XFOOT     $VA           COM$VA           16 3
433600070628     C                   XFOOT     $V2           COM$V2           16 3
433700070628IF  1C     COM$VA        IFNE      *ZEROS
433800070628     C     COM$V2        ORNE      *ZEROS
433900070628IF  2C     DICUI         IFEQ      '0'
434000070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
434100070628E   2C                   ENDIF
434200070628     C*--------------------
434300070628     C* STAMPA  R I C A V I   RITIRO BANCALI
434400070628     C*--------------------
434500070628     C                   MOVEL     DME(49)       $DE
434600070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
434700070628     C                   EXSR      STAAMX
434800070628     C*--------------------
434900070628     C* STAMPA  N U M E R O   RITIRO BANCALI
435000070628     C*--------------------
435100070628     C                   Z-ADD     $V2           $VA
435200070628     C                   MOVEL     DME(15)       $DE
435300070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
435400070628     C                   EXSR      STAAMX
435500071210      *
435600071211     c                   eval      Righe_da_stampare = 1
435700071211     c                   exsr      solo_x_STT
435800070628     C                   WRITE     SE62RV
435900071210      *
436000070628     C                   EXSR      STATES
436100070628E   1C                   ENDIF
436200070628     C*--------------------
436300070628     C* REC. C/ASS.        --> VARIA = "W"
436400070628     C*--------------------
436500070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
436600070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
436700070628     C                   Z-ADD     1             P
436800070628     C                   SETOFF                                           28
436900070628     C     'W'           LOOKUP    VAR(P)                                 28
437000070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "W"
437100070628     C     P             OCCUR     CAMM
437200070628     C                   ADD       ARI           $VA                            *RICAVI "W"
437300070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "W"
437400070628E   1C                   ENDIF
437500070628     C*
437600070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
437700070628     C                   XFOOT     $VA           COM$VA           16 3
437800070628     C                   XFOOT     $V2           COM$V2           16 3
437900070628IF  1C     COM$VA        IFNE      *ZEROS
438000070628     C     COM$V2        ORNE      *ZEROS
438100070628IF  2C     DICUI         IFEQ      '0'
438200070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
438300070628E   2C                   ENDIF
438400070628     C*--------------------
438500070628     C* STAMPA  R I C A V I   REC. C/ASS.
438600070628     C*--------------------
438700070628     C                   MOVEL     DME(50)       $DE
438800070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
438900070628     C                   EXSR      STAAMX
439000070628     C*--------------------
439100070628     C* STAMPA  N U M E R O   REC. C/ASS.
439200070628     C*--------------------
439300070628     C                   Z-ADD     $V2           $VA
439400070628     C                   MOVEL     DME(15)       $DE
439500070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
439600070628     C                   EXSR      STAAMX
439700071210      *
439800071211     c                   eval      Righe_da_stampare = 1
439900071211     c                   exsr      solo_x_STT
440000070628     C                   WRITE     SE62RV
440100071210      *
440200070628     C                   EXSR      STATES
440300070628E   1C                   ENDIF
440400070628     C*--------------------
440500070628     C* ORM COMMISSIONATO  --> VARIA = "T"
440600070628     C*--------------------
440700070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
440800070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
440900070628     C                   Z-ADD     1             P
441000070628     C                   SETOFF                                           28
441100070628     C     'T'           LOOKUP    VAR(P)                                 28
441200070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "T"
441300070628     C     P             OCCUR     CAMM
441400070628     C                   ADD       ARI           $VA                            *RICAVI "T"
441500070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "T"
441600070628E   1C                   ENDIF
441700070628     C*
441800070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
441900070628     C                   XFOOT     $VA           COM$VA           16 3
442000070628     C                   XFOOT     $V2           COM$V2           16 3
442100070628IF  1C     COM$VA        IFNE      *ZEROS
442200070628     C     COM$V2        ORNE      *ZEROS
442300070628IF  2C     DICUI         IFEQ      '0'
442400070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
442500070628E   2C                   ENDIF
442600070628     C*--------------------
442700070628     C* STAMPA  R I C A V I   ORM COMMISSIONATO
442800070628     C*--------------------
442900070628     C                   MOVEL     DME(51)       $DE
443000070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
443100070628     C                   EXSR      STAAMX
443200070628     C*--------------------
443300070628     C* STAMPA  N U M E R O   ORM COMMISSIONATO
443400070628     C*--------------------
443500070628     C                   Z-ADD     $V2           $VA
443600070628     C                   MOVEL     DME(15)       $DE
443700070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
443800070628     C                   EXSR      STAAMX
443900071210      *
444000071211     c                   eval      Righe_da_stampare = 1
444100071211     c                   exsr      solo_x_STT
444200070628     C                   WRITE     SE62RV
444300071210      *
444400070628     C                   EXSR      STATES
444500070628E   1C                   ENDIF
444600070628     C*--------------------
444700070628     C* L.AVVISO           --> VARIA = "c"
444800070628     C*--------------------
444900070628     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
445000070628     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
445100070628     C                   Z-ADD     1             P
445200070628     C                   SETOFF                                           28
445300070628     C     'c'           LOOKUP    VAR(P)                                 28
445400070628IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "T"
445500070628     C     P             OCCUR     CAMM
445600070628     C                   ADD       ARI           $VA                            *RICAVI "T"
445700070628     C                   ADD       ASP           $V2                            *SPEDIZIONI "T"
445800070628E   1C                   ENDIF
445900070628     C*
446000070628     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
446100070628     C                   XFOOT     $VA           COM$VA           16 3
446200070628     C                   XFOOT     $V2           COM$V2           16 3
446300070628IF  1C     COM$VA        IFNE      *ZEROS
446400070628     C     COM$V2        ORNE      *ZEROS
446500070628IF  2C     DICUI         IFEQ      '0'
446600070628     C                   EXSR      STACUI                                       *STAMPA RIGA "DI CUI
446700070628E   2C                   ENDIF
446800070628     C*--------------------
446900070628     C* STAMPA  R I C A V I   L.AVVISO
447000070628     C*--------------------
447100070628     C                   MOVEL     DME(62)       $DE
447200070628     C                   MOVEL     'RIC'         TIPVAL                         *RICAVI
447300070628     C                   EXSR      STAAMX
447400070628     C*--------------------
447500070628     C* STAMPA  N U M E R O   L.AVVISO
447600070628     C*--------------------
447700070628     C                   Z-ADD     $V2           $VA
447800070628     C                   MOVEL     DME(15)       $DE
447900070628     C                   MOVEL     'NUM'         TIPVAL                         *NUMERO
448000070628     C                   EXSR      STAAMX
448100071210      *
448200071211     c                   eval      Righe_da_stampare = 1
448300071211     c                   exsr      solo_x_STT
448400070628     C                   WRITE     SE62RV
448500071210      *
448600070628     C                   EXSR      STATES
448700070628E   1C                   ENDIF
448800070628     C*
448900070628     C                   ENDSR
449000000000     C*------------------------------------------------------------------------*
449100000000     C*?STACUI - STAMPA STRINGA "DI CUI"
449200000000     C*------------------------------------------------------------------------*
449300000000     C     STACUI        BEGSR
449400000000     C*
449500000000     C                   MOVEL     '1'           DICUI             1
449600000000     C*
449700071211     c                   eval      Righe_da_stampare = 1
449800071211     c                   exsr      solo_x_STT
449900010920     C                   WRITE     SE62RV
450000071210      *
450100020823     C                   EXSR      STATES
450200000000     C*
450300010920     C                   CLEAR                   SE62P1
450400000000     C                   MOVEL     DME(16)       P1DDES
450500071210      *
450600071211     c                   eval      Righe_da_stampare = 1
450700071211     c                   exsr      solo_x_STT
450800020524     C                   WRITE     SE62P1                                       *"DI CUI:"
450900071210      *
451000020823     C                   EXSR      STATES
451100071210      *
451200071211     c                   eval      Righe_da_stampare = 1
451300071211     c                   exsr      solo_x_STT
451400010920     C                   WRITE     SE62RV
451500071210      *
451600020823     C                   EXSR      STATES
451700000000     C*
451800000000     C                   ENDSR
451900000000     C*------------------------------------------------------------------------*
452000000000     C*?STAAMX - IMPOSTA STAMPA RIGA XXX STATISTICA AMMNISTRAZIONE
452100000000     C*          INPUT --> $VA  : SCHIERA VALORI DI 12 MESI
452200000000     C*                    $DE  : DESCRIZIONE RIGA DA STAMPARE
452300000000     C*                    $FA  : TOTALE FATTURATO
452400000000     C*                    $NR  : TOTALE NOTE ACCREDITO O RETTIFICHE
452500000000     C*------------------------------------------------------------------------*
452600000000     C     STAAMX        BEGSR
452700000000     C*
452800000000     C*--------------------
452900000000     C* 13^ C O L O N N A
453000000000     C*--------------------
453100000000     C* PER LA RIGA DELLE PERCENTUALI NON POSSO FARE LA XFOOT
453200000000IF  1C     TIPVAL        IFNE      'PER'                                        *N O   P E R C. L E
453300000000     C                   XFOOT     $VA           $VA13C           16 3
453400000000X   1C                   ELSE                                                   *S I   D E L T A
453500000000     C                   Z-ADD     $FA           WFAW             16 3          *FATTURATO
453600000000     C                   Z-ADD     $NR           WNRW             16 3          *NOTE O RETTIFICHE
453700000629     C                   Z-ADD     *ZEROS        WPDW              4 1          *PERC FATTURATO
453800000000     C                   EXSR      CALPER
453900000629     C                   Z-ADD     WPDW          $VA13C                         *PERC 13^ COLONNA
454000000000E   1C                   ENDIF
454100000000     C*
454200000000     C* SE VALORE DI TIPO RICAVO LO COMPATTA
454300010920IF  1C***  TIPVAL        IFEQ      'RIC'
454400010920     C***  $VA           DIV       1000          $VA
454500010920     C***  $VA13C        DIV       1000          $VA13C
454600010920E   1C***                ENDIF
454700000000     C*
454800000000     C* MUOVE I CAMPI CALCOLATI IN QUELLI DI STAMPA E STAMPA
454900000000IF  1C     TIPVAL        IFNE      'PER'                                        *N O   P E R C. L E
455000010920     C                   EXSR      STACAN                                       *RIGA ALTRI VALORI
455100010920X   1C                   ELSE                                                   *S I   P E R C. L E
455200010920IF  2C     TIPVAL        IFEQ      'RIC'                                        *R I C A V I
455300010920     C                   EXSR      STACAR
455400010920X   2C                   ELSE
455500000000     C                   EXSR      STACAP                                       *RIGA PERCENTUALE
455600010920E   2C                   ENDIF
455700000000E   1C                   ENDIF
455800000000     C*
455900000000     C                   ENDSR
456000000000     C*------------------------------------------------------------------------*
456100000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
456200000000     C*          INPUT  --> WRRW: RICAVO REALE
456300000000     C*                     WRPW: RICAVO PRESUNTO
456400000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
456500000000     C*------------------------------------------------------------------------*
456600000000     C     CALDEL        BEGSR
456700000000     C*
456800000000     C*--------------------
456900000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
457000000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
457100000000     C*--------------------
457200000000     C*
457300000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
457400000000     C     WRRW          SUB       WRPW          GAP              16 3          (REALE - PRESUNTO)
457500000000     C     GAP           MULT      100           GAP                            (GAP x 100)
457600000000     C*
457700000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
457800000000IF  1C     WRRW          IFEQ      *ZEROS
457900000000     C                   Z-ADD     1             WRRW
458000000000E   1C                   ENDIF
458100000000     C*
458200000000     C     GAP           DIV       WRRW          GAP
458300000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
458400000000     C*
458500000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
458600000000     C     WDPW          COMP      0                                    28
458700000000     C* POSITIVO
458800000000IF  1C     *IN28         IFEQ      *OFF
458900000000IF  2C     GAP           IFGE      1000
459000000000     C                   Z-ADD     +999,9        WDPW
459100000000E   2C                   ENDIF
459200000000     C* NEGATIVO
459300000000X   1C                   ELSE
459400000000IF  2C     GAP           IFLE      -1000
459500000000     C                   Z-ADD     -999,9        WDPW
459600000000E   2C                   ENDIF
459700000000E   1C                   ENDIF
459800000000     C*
459900000000     C                   ENDSR
460000000000     C*------------------------------------------------------------------------*
460100000000     C*?CALPER - CALCOLA LA PERCENTUALE SUL FATTURATO PER LA "AMMINISTRAZIONE"
460200000000     C*          INPUT  --> WFAW: FATTURATO
460300000000     C*                     WNRW: NOTE ACCREDITO O RETTITICHE
460400000629     C*          OUTPUT --> WPDW: PERCENTUALE SUL FATTURATO
460500000000     C*------------------------------------------------------------------------*
460600000000     C     CALPER        BEGSR
460700000000     C*
460800000000     C*               NOTE/RETTIFICHE/RICAVI GIACENZE x 100
460900000000     C* PERCENTUALE = -------------------------------------
461000000000     C*                           FATTURATO
461100000000     C*
461200000000     C* NB: SE IL FATTURATO E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
461300000000IF  1C     WFAW          IFEQ      *ZEROS
461400000000     C                   Z-ADD     1             WFAW
461500000000E   1C                   ENDIF
461600000000     C*
461700000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
461800000000     C     WNRW          MULT      100           RISUL            16 3
461900000000     C     RISUL         DIV       WFAW          RISUL
462000000629     C                   Z-ADD(H)  RISUL         WPDW
462100000000     C*
462200000000     C* SE LA PERCENTUALE E' +/-1000  METTO  +/-999,9 %
462300000629     C     WPDW          COMP      0                                    28
462400000000     C* POSITIVO
462500000000IF  1C     *IN28         IFEQ      *OFF
462600000000IF  2C     RISUL         IFGE      1000
462700000629     C                   Z-ADD     +999,9        WPDW
462800000000E   2C                   ENDIF
462900000000     C* NEGATIVO
463000000000X   1C                   ELSE
463100000000IF  2C     RISUL         IFLE      -1000
463200000629     C                   Z-ADD     -999,9        WPDW
463300000000E   2C                   ENDIF
463400000000E   1C                   ENDIF
463500000000     C*
463600000000     C                   ENDSR
463700000000     C*------------------------------------------------------------------------*
463800000000     C*?CALMED - CALCOLA LE MEDIE FINALI PER LA "SERVIZIO BOLLE"
463900000000     C*          INPUT --> WSPW: SPEDIZIONI DEL MESE
464000000000     C*                    WCOW: COLLI DEL MESE
464100000000     C*                    WPEW: PESO IN QUINTALI DEL MESE
464200000000     C*                    WVOW: VOLUME IN M.CUBI DEL MESE
464300000000     C*                    WRIW: RICAVI DEL MESE
464400000000     C*                    I   : MEDIA DA CALCOLARE (1÷9)
464500000000     C*          INPUT --> WMED: MEDIA CALCOLATA
464600000000     C*------------------------------------------------------------------------*
464700000000     C     CALMED        BEGSR
464800000000     C*
464900010920     C*    COLLI/KG /M.CUBO       KG/M.CUBO           RICAVI
465000000000     C* 1] ---------------- ;  2] --------- ;  3] -----------------------
465100000000     C*       SPEDIZIONI            COLLI          SPED/COLLI/Q.LE/M.CUBO
465200000000     C*---------------------
465300000000     C* PRIMA SERIE
465400000000     C*---------------------
465500000000     C* SE LE SPEDIZIONI SONO = 0, LE IMPOSTO = 1 PER NON DARE ERRORE
465600000000IF  1C     I             IFEQ      1
465700000000     C     I             OREQ      2
465800000000     C     I             OREQ      3
465900000000IF  2C     WSPW          IFEQ      *ZEROS
466000000000     C                   Z-ADD     1             WSPW
466100000000E   2C                   ENDIF
466200000000     C*
466300000000IF  2C     I             IFEQ      1
466400000000     C     WCOW          DIV       WSPW          WMED                           *COLLI/SPEDIZIONI
466500000000E   2C                   ENDIF
466600000000     C*
466700000000IF  2C     I             IFEQ      2
466800000000     C     WPEW          DIV       WSPW          WMED                           *KG/SPEDIZIONI
466900000000E   2C                   ENDIF
467000000000     C*
467100000000IF  2C     I             IFEQ      3
467200000000     C     WVOW          DIV       WSPW          WMED                           *VOLUME/SPEDIZIONI
467300000000E   2C                   ENDIF
467400000000E   1C                   ENDIF
467500000000     C*---------------------
467600000000     C* SECONDA SERIE
467700000000     C*---------------------
467800000000     C* SE I COLLI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
467900000000IF  1C     I             IFEQ      4
468000000000     C     I             OREQ      5
468100000000IF  2C     WCOW          IFEQ      *ZEROS
468200000000     C                   Z-ADD     1             WCOW
468300000000E   2C                   ENDIF
468400000000     C*
468500000000IF  2C     I             IFEQ      4
468600000000     C     WPEW          DIV       WCOW          WMED                           *KG/COLLI
468700000000E   2C                   ENDIF
468800000000     C*
468900000000IF  2C     I             IFEQ      5
469000000000     C     WVOW          DIV       WCOW          WMED                           *VOLUME/COLLI
469100000000E   2C                   ENDIF
469200000000E   1C                   ENDIF
469300000000     C*---------------------
469400000000     C* TERZA SERIE
469500000000     C*---------------------
469600000000IF  1C     I             IFEQ      6
469700000000     C     I             OREQ      7
469800000000     C     I             OREQ      8
469900000000     C     I             OREQ      9
470000000000     C*
470100000000     C* TRASFORMO IL PESO DA KILOGRAMMI IN QUINTALI
470200000000     C     WPEW          DIV       100           WPEW
470300000000     C*
470400000000IF  2C     I             IFEQ      6
470500000000     C* SE LE SPEDIZIONI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
470600000000IF  3C     WSPW          IFEQ      *ZEROS
470700000000     C                   Z-ADD     1             WSPW
470800000000E   3C                   ENDIF
470900000000     C     WRIW          DIV       WSPW          WMED                           *INCASSO/SPEDIZIONI
471000000000E   2C                   ENDIF
471100000000     C*
471200000000IF  2C     I             IFEQ      7
471300000000     C* SE I COLLI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
471400000000IF  3C     WCOW          IFEQ      *ZEROS
471500000000     C                   Z-ADD     1             WCOW
471600000000E   3C                   ENDIF
471700000000     C     WRIW          DIV       WCOW          WMED                           *INCASSO/COLLI
471800000000E   2C                   ENDIF
471900000000     C*
472000000000IF  2C     I             IFEQ      8
472100000000     C* SE I QUINTALI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
472200000000IF  3C     WPEW          IFEQ      *ZEROS
472300000000     C                   Z-ADD     1             WPEW
472400000000E   3C                   ENDIF
472500000000     C     WRIW          DIV       WPEW          WMED                           *INCASSO/QUINTALI
472600000000E   2C                   ENDIF
472700000000     C*
472800000000IF  2C     I             IFEQ      9
472900000000     C* SE IL VOLUME E' = 0, LO IMPOSTO = 1 PER NON DARE ERRORE
473000000000IF  3C     WVOW          IFEQ      *ZEROS
473100000000     C                   Z-ADD     1             WVOW
473200000000E   3C                   ENDIF
473300000000     C     WRIW          DIV       WVOW          WMED                           *INCASSO/VOLUME
473400000000E   2C                   ENDIF
473500000000     C*
473600000000E   1C                   ENDIF
473700000000     C*
473800000000     C                   ENDSR
473900000000     C*------------------------------------------------------------------------*
474000000000     C*?CALINC - CALCOLA L'INCIDENZA DELLE CONSEGNE DELLE SPEDIZIONI
474100020131     C*          INPUT  -->WTXW : TOTALE SERVIZIO ORE
474200000000     C*                    WVAW : TOTALE SERVIZIO ORA
474300020131     C*          OUTPUT -->WINW : PERCENTUALE DI INCIDENZA
474400000000     C*------------------------------------------------------------------------*
474500000000     C     CALINC        BEGSR
474600000000     C*
474700000000     C*                   CORRIERE/DISTRIB./ESPRESSO DI OGNI ORAIO x 100
474800000000     C*INCIDENZA= --------------------------------------------------------------
474900000000     C*           TOT CORRIER/DISTR./ESPRESSO oppure TOT CORRIER+DISTR.+ESPRESSO
475000000000     C*
475100000000     C* NB: SE IL TOTALE SPEDIZIONI E' = 0 LO IMPOSTO A 1 PER NON DARE ERRORE
475200000000     C*     <NEL CASO NON CI SIANO SPEDIZIONI PER UNA DETERMINATA ORA (0/1=0)
475300000000IF  1C     WTXW          IFEQ      *ZEROS
475400000000     C                   Z-ADD     1             WTXW
475500000000E   1C                   ENDIF
475600000000     C*
475700000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
475800000000     C     WVAW          MULT      100           RISUL            16 3
475900000000     C     RISUL         DIV       WTXW          RISUL
476000000000     C*
476100000000     C* SE LA PERCENTUALE E' 100%  METTO  99% PERCHE' IL CAMPO E' LUNGO 2
476200060525IF  1C**** RISUL         IFGE      100
476300060525IF  1C     RISUL         IFGE      99
476400000000     C                   Z-ADD     99            WINW
476500000000X   1C                   ELSE
476600000000     C                   Z-ADD(H)  RISUL         WINW              2 0
476700000000E   1C                   ENDIF
476800000000     C*
476900000000     C                   ENDSR
477000000000     C*------------------------------------------------------------------------*
477100000000     C*?STACAV - MUOVE I CAMPI "VOLUME/MEDIE" IN QUELLI DI STAMPA E STAMPA
477200000000     C*------------------------------------------------------------------------*
477300000000     C     STACAV        BEGSR
477400000000     C*
477500000000     C                   MOVEL     $DE           P2DDES                         *DESCRIZIONE VALORE
477600000000     C*
477700000000     C                   Z-ADD     $VA(1)        P2CVA1                         *MESE UNO
477800000000     C                   Z-ADD     $VA(2)        P2CVA2                         *MESE DUE
477900000000     C                   Z-ADD     $VA(3)        P2CVA3                         *MESE TRE
478000000000     C                   Z-ADD     $VA(4)        P2CVA4                         *MESE QUATTRO
478100000000     C                   Z-ADD     $VA(5)        P2CVA5                         *MESE CINQUE
478200000000     C                   Z-ADD     $VA(6)        P2CVA6                         *MESE SEI
478300000000     C                   Z-ADD     $VA(7)        P2CVA7                         *MESE SETTE
478400000000     C                   Z-ADD     $VA(8)        P2CVA8                         *MESE OTTO
478500000000     C                   Z-ADD     $VA(9)        P2CVA9                         *MESE NOVE
478600000000     C                   Z-ADD     $VA(10)       P2CVA0                         *MESE DIECI
478700000000     C                   Z-ADD     $VA(11)       P2CVAA                         *MESE UNDICI
478800000000     C                   Z-ADD     $VA(12)       P2CVAB                         *MESE DODICI
478900000000     C                   Z-ADD     $VA13C        P2CVAT                         *TOTALE VALORE
479000000000     C*
479100071211     c                   eval      Righe_da_stampare = 1
479200071211     c                   exsr      solo_x_STT
479300010920     C                   WRITE     SE62P2                                       *STAMPA RIGA VOL/MED
479400071210      *
479500020823     C                   EXSR      STATES
479600000000     C*
479700000000     C                   ENDSR
479800010920     C*------------------------------------------------------------------------*
479900010920     C*?STACAR - MUOVE I CAMPI "RICAVI" IN QUELLI DI STAMPA E STAMPA
480000010920     C*------------------------------------------------------------------------*
480100010920     C     STACAR        BEGSR
480200010920     C*
480300010920     C                   MOVEL     $DE           P5DDES                         *DESCRIZIONE VALORE
480400010920     C*
480500010920     C                   Z-ADD     $VA(1)        P5CVA1                         *MESE UNO
480600010920     C                   Z-ADD     $VA(2)        P5CVA2                         *MESE DUE
480700010920     C                   Z-ADD     $VA(3)        P5CVA3                         *MESE TRE
480800010920     C                   Z-ADD     $VA(4)        P5CVA4                         *MESE QUATTRO
480900010920     C                   Z-ADD     $VA(5)        P5CVA5                         *MESE CINQUE
481000010920     C                   Z-ADD     $VA(6)        P5CVA6                         *MESE SEI
481100010920     C                   Z-ADD     $VA(7)        P5CVA7                         *MESE SETTE
481200010920     C                   Z-ADD     $VA(8)        P5CVA8                         *MESE OTTO
481300010920     C                   Z-ADD     $VA(9)        P5CVA9                         *MESE NOVE
481400010920     C                   Z-ADD     $VA(10)       P5CVA0                         *MESE DIECI
481500010920     C                   Z-ADD     $VA(11)       P5CVAA                         *MESE UNDICI
481600010920     C                   Z-ADD     $VA(12)       P5CVAB                         *MESE DODICI
481700010920     C                   Z-ADD     $VA13C        P5CVAT                         *TOTALE VALORE
481800010920     C*
481900071211     c                   eval      Righe_da_stampare = 1
482000071211     c                   exsr      solo_x_STT
482100010920     C                   WRITE     SE62P5                                       *STAMPA RIGA RICAVI
482200071210      *
482300020823     C                   EXSR      STATES
482400010920     C*
482500010920     C                   ENDSR
482600020131     C*------------------------------------------------------------------------*
482700020131     C*?STACAR1 - MUOVE I CAMPI MEDIE "RICAVI" IN QUELLI DI STAMPA E STAMPA
482800020131     C*------------------------------------------------------------------------*
482900020131     C     STACAR1       BEGSR
483000020131     C*
483100020131     C                   MOVEL     $DE           P6DDES                         *DESCRIZIONE VALORE
483200020131     C*
483300020131     C                   Z-ADD     $VA(1)        P6CVA1                         *MESE UNO
483400020131     C                   Z-ADD     $VA(2)        P6CVA2                         *MESE DUE
483500020131     C                   Z-ADD     $VA(3)        P6CVA3                         *MESE TRE
483600020131     C                   Z-ADD     $VA(4)        P6CVA4                         *MESE QUATTRO
483700020131     C                   Z-ADD     $VA(5)        P6CVA5                         *MESE CINQUE
483800020131     C                   Z-ADD     $VA(6)        P6CVA6                         *MESE SEI
483900020131     C                   Z-ADD     $VA(7)        P6CVA7                         *MESE SETTE
484000020131     C                   Z-ADD     $VA(8)        P6CVA8                         *MESE OTTO
484100020131     C                   Z-ADD     $VA(9)        P6CVA9                         *MESE NOVE
484200020131     C                   Z-ADD     $VA(10)       P6CVA0                         *MESE DIECI
484300020131     C                   Z-ADD     $VA(11)       P6CVAA                         *MESE UNDICI
484400020131     C                   Z-ADD     $VA(12)       P6CVAB                         *MESE DODICI
484500020131     C                   Z-ADD     $VA13C        P6CVAT                         *TOTALE VALORE
484600020131     C*
484700071211     c                   eval      Righe_da_stampare = 1
484800071211     c                   exsr      solo_x_STT
484900020131     C                   WRITE     SE62P6                                       *STAMPA RIGA RICAVI
485000071210      *
485100020823     C                   EXSR      STATES
485200020131     C*
485300020131     C                   ENDSR
485400000000     C*------------------------------------------------------------------------*
485500000000     C*?STACAN - MUOVE I CAMPI "NORMALI" IN QUELLI DI STAMPA E STAMPA
485600000000     C*------------------------------------------------------------------------*
485700000000     C     STACAN        BEGSR
485800000000     C*
485900000000     C                   MOVEL     $DE           P1DDES                         *DESCRIZIONE VALORE
486000000000     C*
486100000000     C                   Z-ADD     $VA(1)        P1CVA1                         *MESE UNO
486200000000     C                   Z-ADD     $VA(2)        P1CVA2                         *MESE DUE
486300000000     C                   Z-ADD     $VA(3)        P1CVA3                         *MESE TRE
486400000000     C                   Z-ADD     $VA(4)        P1CVA4                         *MESE QUATTRO
486500000000     C                   Z-ADD     $VA(5)        P1CVA5                         *MESE CINQUE
486600000000     C                   Z-ADD     $VA(6)        P1CVA6                         *MESE SEI
486700000000     C                   Z-ADD     $VA(7)        P1CVA7                         *MESE SETTE
486800000000     C                   Z-ADD     $VA(8)        P1CVA8                         *MESE OTTO
486900000000     C                   Z-ADD     $VA(9)        P1CVA9                         *MESE NOVE
487000000000     C                   Z-ADD     $VA(10)       P1CVA0                         *MESE DIECI
487100000000     C                   Z-ADD     $VA(11)       P1CVAA                         *MESE UNDICI
487200000000     C                   Z-ADD     $VA(12)       P1CVAB                         *MESE DODICI
487300000000     C                   Z-ADD     $VA13C        P1CVAT                         *TOTALE VALORE
487400000000     C*
487500071211     c                   eval      Righe_da_stampare = 1
487600071211     c                   exsr      solo_x_STT
487700010920     C                   WRITE     SE62P1                                       *STAMPA RIGA VALORE
487800071210      *
487900020823     C                   EXSR      STATES
488000000000     C*
488100000000     C                   ENDSR
488200000000     C*------------------------------------------------------------------------*
488300000000     C*?STACAP - MUOVE I CAMPI "PERCENTUALI" IN QUELLI DI STAMPA E STAMPA
488400000000     C*------------------------------------------------------------------------*
488500000000     C     STACAP        BEGSR
488600000000     C*
488700000000     C                   MOVEL     $DE           P3DDES                         *DESCRIZIONE VALORE
488800000000     C*
488900000000     C                   Z-ADD     $VA(1)        P3CVA1                         *MESE UNO
489000000000     C                   Z-ADD     $VA(2)        P3CVA2                         *MESE DUE
489100000000     C                   Z-ADD     $VA(3)        P3CVA3                         *MESE TRE
489200000000     C                   Z-ADD     $VA(4)        P3CVA4                         *MESE QUATTRO
489300000000     C                   Z-ADD     $VA(5)        P3CVA5                         *MESE CINQUE
489400000000     C                   Z-ADD     $VA(6)        P3CVA6                         *MESE SEI
489500000000     C                   Z-ADD     $VA(7)        P3CVA7                         *MESE SETTE
489600000000     C                   Z-ADD     $VA(8)        P3CVA8                         *MESE OTTO
489700000000     C                   Z-ADD     $VA(9)        P3CVA9                         *MESE NOVE
489800000000     C                   Z-ADD     $VA(10)       P3CVA0                         *MESE DIECI
489900000000     C                   Z-ADD     $VA(11)       P3CVAA                         *MESE UNDICI
490000000000     C                   Z-ADD     $VA(12)       P3CVAB                         *MESE DODICI
490100000000     C                   Z-ADD     $VA13C        P3CVAT                         *TOTALE VALORE
490200000000     C*
490300071211     c                   eval      Righe_da_stampare = 1
490400071211     c                   exsr      solo_x_STT
490500010920     C                   WRITE     SE62P3                                       *STAMPA RIGA PERCENT
490600071210      *
490700020823     C                   EXSR      STATES
490800000000     C*
490900000000     C                   ENDSR
491000000000     C*------------------------------------------------------------------------*
491100000000     C*?STACA2 - MUOVE I CAMPI "PERCENTUALI" DI 2 IN QUELLI DI STAMPA E STAMPA
491200000000     C*------------------------------------------------------------------------*
491300000000     C     STACA2        BEGSR
491400000000     C*
491500000000     C                   MOVEL     $DE           P4DDES                         *DESCRIZIONE VALORE
491600000000     C*
491700000000     C                   Z-ADD     $P1(1)        P4CVN1                         *MESE UNO CORRIERE
491800000629     C                   Z-ADD     $P2(1)        P4CVD1                         *DISTRIBUZIONE
491900000000     C                   Z-ADD     $P3(1)        P4CVE1                         *ESPRESSO
492000000629     C                   Z-ADD     $P5(1)        P4CVT1                         *TOTALE
492100000000     C                   Z-ADD     $P1(2)        P4CVN2                         *MESE DUE CORRIERE
492200000629     C                   Z-ADD     $P2(2)        P4CVD2                         *DISTRIBUZIONE
492300000000     C                   Z-ADD     $P3(2)        P4CVE2                         *ESPRESSO
492400000629     C                   Z-ADD     $P5(2)        P4CVT2                         *TOTALE
492500000000     C                   Z-ADD     $P1(3)        P4CVN3                         *MESE TRE CORRIERE
492600000629     C                   Z-ADD     $P2(3)        P4CVD3                         *DISTRIBUZIONE
492700000000     C                   Z-ADD     $P3(3)        P4CVE3                         *ESPRESSO
492800000629     C                   Z-ADD     $P5(3)        P4CVT3                         *TOTALE
492900000000     C                   Z-ADD     $P1(4)        P4CVN4                         *MESE QUATTRO CORRIE
493000000629     C                   Z-ADD     $P2(4)        P4CVD4                         *DISTRIBUZIONE
493100000000     C                   Z-ADD     $P3(4)        P4CVE4                         *ESPRESSO
493200000629     C                   Z-ADD     $P5(4)        P4CVT4                         *TOTALE
493300000000     C                   Z-ADD     $P1(5)        P4CVN5                         *MESE CINQUE CORRIER
493400000629     C                   Z-ADD     $P2(5)        P4CVD5                         *DISTRIBUZIONE
493500000000     C                   Z-ADD     $P3(5)        P4CVE5                         *ESPRESSO
493600000629     C                   Z-ADD     $P5(5)        P4CVT5                         *TOTALE
493700000000     C                   Z-ADD     $P1(6)        P4CVN6                         *MESE SEI CORRIERE
493800000629     C                   Z-ADD     $P2(6)        P4CVD6                         *DISTRIBUZIONE
493900000000     C                   Z-ADD     $P3(6)        P4CVE6                         *ESPRESSO
494000000629     C                   Z-ADD     $P5(6)        P4CVT6                         *TOTALE
494100000000     C                   Z-ADD     $P1(7)        P4CVN7                         *MESE SETTE CORRIERE
494200000629     C                   Z-ADD     $P2(7)        P4CVD7                         *DISTRIBUZIONE
494300000000     C                   Z-ADD     $P3(7)        P4CVE7                         *ESPRESSO
494400000629     C                   Z-ADD     $P5(7)        P4CVT7                         *TOTALE
494500000000     C                   Z-ADD     $P1(8)        P4CVN8                         *MESE OTTO CORRIERE
494600000629     C                   Z-ADD     $P2(8)        P4CVD8                         *DISTRIBUZIONE
494700000000     C                   Z-ADD     $P3(8)        P4CVE8                         *ESPRESSO
494800000629     C                   Z-ADD     $P5(8)        P4CVT8                         *TOTALE
494900000000     C                   Z-ADD     $P1(9)        P4CVN9                         *MESE NOVE CORRIERE
495000000629     C                   Z-ADD     $P2(9)        P4CVD9                         *DISTRIBUZIONE
495100000000     C                   Z-ADD     $P3(9)        P4CVE9                         *ESPRESSO
495200000629     C                   Z-ADD     $P5(9)        P4CVT9                         *TOTALE
495300000000     C                   Z-ADD     $P1(10)       P4CVN0                         *MESE DIECI CORRIERE
495400000629     C                   Z-ADD     $P2(10)       P4CVD0                         *DISTRIBUZIONE
495500000000     C                   Z-ADD     $P3(10)       P4CVE0                         *ESPRESSO
495600000629     C                   Z-ADD     $P5(10)       P4CVT0                         *TOTALE
495700000000     C                   Z-ADD     $P1(11)       P4CVNA                         *MESE UNDICI CORRIER
495800000629     C                   Z-ADD     $P2(11)       P4CVDA                         *DISTRIBUZIONE
495900000000     C                   Z-ADD     $P3(11)       P4CVEA                         *ESPRESSO
496000000629     C                   Z-ADD     $P5(11)       P4CVTA                         *TOTALE
496100000000     C                   Z-ADD     $P1(12)       P4CVNB                         *MESE DODICI CORRIER
496200000629     C                   Z-ADD     $P2(12)       P4CVDB                         *DISTRIBUZIONE
496300000000     C                   Z-ADD     $P3(12)       P4CVEB                         *ESPRESSO
496400000629     C                   Z-ADD     $P5(12)       P4CVTB                         *TOTALE
496500000000     C*
496600000000     C                   Z-ADD     $P113C        P4CVNT                         TOTALE VALORE CORRIE
496700000629     C                   Z-ADD     $P213C        P4CVDT                         *DISTRIBUZIONE
496800000000     C                   Z-ADD     $P313C        P4CVET                         *ESPRESSO
496900000629     C                   Z-ADD     $P513C        P4CVTT                         *TOTALE
497000000000     C*
497100071211     c                   eval      Righe_da_stampare = 1
497200071211     c                   exsr      solo_x_STT
497300010920     C                   WRITE     SE62P4                                       *STAMPA RIGA PERCENT
497400071210      *
497500020823     C                   EXSR      STATES
497600000000     C*
497700000000     C                   ENDSR
497800000000     C*------------------------------------------------------------------------*
497900000000     C*?DECTES - DECODIFICA CAMPI INTESTAZIONE
498000000000     C*------------------------------------------------------------------------*
498100000000     C     DECTES        BEGSR
498200000000     C*
498300000000     C* RECUPERO LA DATA E L'ORA
498400000000     C                   Z-ADD     UDATE         DATA              6 0
498500000000     C                   TIME                    ORA               6 0
498600000000     C*
498700000000     C* DECODIFICA MESI
498800000000     C                   Z-ADD     *ZEROS        J
498900000000IF  1C     MES(1)        IFGT      *ZEROS
499000000000     C                   Z-ADD     MES(1)        J                              *NUMERO MESE MEMORIZ
499100000000     C                   MOVEL     DMM(J)        PIDME1                         *DESC MESE IN STAMPA
499200990608     C                   MOVE      ANN(1)        PICAN1                         *NUM. ANNO IN STAMPA
499300000000     C                   MOVEL     DCA(11)       RSCDE1                         *DESCR PER STAT AFF.
499400000000E   1C                   ENDIF
499500000000     C*
499600000000IF  1C     MES(2)        IFGT      *ZEROS
499700000000     C                   Z-ADD     MES(2)        J                              *NUMERO MESE MEMORIZ
499800000000     C                   MOVEL     DMM(J)        PIDME2                         *DESC MESE IN STAMPA
499900990608     C                   MOVE      ANN(2)        PICAN2                         *NUM. ANNO IN STAMPA
500000000000     C                   MOVEL     DCA(11)       RSCDE2                         *DESCR PER STAT AFF.
500100000000E   1C                   ENDIF
500200000000     C*
500300000000IF  1C     MES(3)        IFGT      *ZEROS
500400000000     C                   Z-ADD     MES(3)        J                              *NUMERO MESE MEMORIZ
500500000000     C                   MOVEL     DMM(J)        PIDME3                         *DESC MESE IN STAMPA
500600990608     C                   MOVE      ANN(3)        PICAN3                         *NUM. ANNO IN STAMPA
500700000000     C                   MOVEL     DCA(11)       RSCDE3                         *DESCR PER STAT AFF.
500800000000E   1C                   ENDIF
500900000000     C*
501000000000IF  1C     MES(4)        IFGT      *ZEROS
501100000000     C                   Z-ADD     MES(4)        J                              *NUMERO MESE MEMORIZ
501200000000     C                   MOVEL     DMM(J)        PIDME4                         *DESC MESE IN STAMPA
501300990608     C                   MOVE      ANN(4)        PICAN4                         *NUM. ANNO IN STAMPA
501400000000     C                   MOVEL     DCA(11)       RSCDE4                         *DESCR PER STAT AFF.
501500000000E   1C                   ENDIF
501600000000     C*
501700000000IF  1C     MES(5)        IFGT      *ZEROS
501800000000     C                   Z-ADD     MES(5)        J                              *NUMERO MESE MEMORIZ
501900000000     C                   MOVEL     DMM(J)        PIDME5                         *DESC MESE IN STAMPA
502000990608     C                   MOVE      ANN(5)        PICAN5                         *NUM. ANNO IN STAMPA
502100000000     C                   MOVEL     DCA(11)       RSCDE5                         *DESCR PER STAT AFF.
502200000000E   1C                   ENDIF
502300000000     C*
502400000000IF  1C     MES(6)        IFGT      *ZEROS
502500000000     C                   Z-ADD     MES(6)        J                              *NUMERO MESE MEMORIZ
502600000000     C                   MOVEL     DMM(J)        PIDME6                         *DESC MESE IN STAMPA
502700990608     C                   MOVE      ANN(6)        PICAN6                         *NUM. ANNO IN STAMPA
502800000000     C                   MOVEL     DCA(11)       RSCDE6                         *DESCR PER STAT AFF.
502900000000E   1C                   ENDIF
503000000000     C*
503100000000IF  1C     MES(7)        IFGT      *ZEROS
503200000000     C                   Z-ADD     MES(7)        J                              *NUMERO MESE MEMORIZ
503300000000     C                   MOVEL     DMM(J)        PIDME7                         *DESC MESE IN STAMPA
503400990608     C                   MOVE      ANN(7)        PICAN7                         *NUM. ANNO IN STAMPA
503500000000     C                   MOVEL     DCA(11)       RSCDE7                         *DESCR PER STAT AFF.
503600000000E   1C                   ENDIF
503700000000     C*
503800000000IF  1C     MES(8)        IFGT      *ZEROS
503900000000     C                   Z-ADD     MES(8)        J                              *NUMERO MESE MEMORIZ
504000000000     C                   MOVEL     DMM(J)        PIDME8                         *DESC MESE IN STAMPA
504100990608     C                   MOVE      ANN(8)        PICAN8                         *NUM. ANNO IN STAMPA
504200000000     C                   MOVEL     DCA(11)       RSCDE8                         *DESCR PER STAT AFF.
504300000000E   1C                   ENDIF
504400000000     C*
504500000000IF  1C     MES(9)        IFGT      *ZEROS
504600000000     C                   Z-ADD     MES(9)        J                              *NUMERO MESE MEMORIZ
504700000000     C                   MOVEL     DMM(J)        PIDME9                         *DESC MESE IN STAMPA
504800990608     C                   MOVE      ANN(9)        PICAN9                         *NUM. ANNO IN STAMPA
504900000000     C                   MOVEL     DCA(11)       RSCDE9                         *DESCR PER STAT AFF.
505000000000E   1C                   ENDIF
505100000000     C*
505200000000IF  1C     MES(10)       IFGT      *ZEROS
505300000000     C                   Z-ADD     MES(10)       J                              *NUMERO MESE MEMORIZ
505400000000     C                   MOVEL     DMM(J)        PIDME0                         *DESC MESE IN STAMPA
505500990608     C                   MOVE      ANN(10)       PICAN0                         *NUM. ANNO IN STAMPA
505600000000     C                   MOVEL     DCA(11)       RSCDE0                         *DESCR PER STAT AFF.
505700000000E   1C                   ENDIF
505800000000     C*
505900000000IF  1C     MES(11)       IFGT      *ZEROS
506000000000     C                   Z-ADD     MES(11)       J                              *NUMERO MESE MEMORIZ
506100000000     C                   MOVEL     DMM(J)        PIDMEA                         *DESC MESE IN STAMPA
506200990608     C                   MOVE      ANN(11)       PICANA                         *NUM. ANNO IN STAMPA
506300000000     C                   MOVEL     DCA(11)       RSCDEA                         *DESCR PER STAT AFF.
506400000000E   1C                   ENDIF
506500000000     C*
506600000000IF  1C     MES(12)       IFGT      *ZEROS
506700000000     C                   Z-ADD     MES(12)       J                              *NUMERO MESE MEMORIZ
506800000000     C                   MOVEL     DMM(J)        PIDMEB                         *DESC MESE IN STAMPA
506900990608     C                   MOVE      ANN(12)       PICANB                         *NUM. ANNO IN STAMPA
507000000000     C                   MOVEL     DCA(11)       RSCDEB                         *DESCR PER STAT AFF.
507100000000E   1C                   ENDIF
507200000000     C*
507300000000     C                   ENDSR
507400000000     C*------------------------------------------------------------------------*
507500000000     C*?STAPAR - STAMPA PAGINA PARAMETRI DI LANCIO
507600000000     C*------------------------------------------------------------------------*
507700000000     C     STAPAR        BEGSR
507800000000     C*
507900000000     C* PASSAGGIO STAMPA PARAMETRI LANCIO (NON PIU' REIMPOSTATO)
508000000000     C                   MOVEL     '1'           PASPAR
508100000811     C*--------------------
508200000811     C* SCELTA
508300000811     C*--------------------
508400000811IF  1C     D60SCE        IFEQ      '1'
508500000811     C                   MOVEL     DME(38)       PADSCE                         *CLIENTE
508600000811E   1C                   ENDIF
508700000811IF  1C     D60SCE        IFEQ      '2'
508800000811     C                   MOVEL     DME(39)       PADSCE                         * + CLIENTI INSIEME
508900000811E   1C                   ENDIF
509000000811IF  1C     D60SCE        IFEQ      '3'
509100000811     C                   MOVEL     DME(40)       PADSCE                         *CLIENTE UNIFICANTE
509200000811E   1C                   ENDIF
509300020524IF  1C     D60SCE        IFEQ      '4'
509400020524     C                   MOVEL     DME(41)       PADSCE                         *TUTTI I CLIENTI
509500020524E   1C                   ENDIF
509600020524IF  1C     D60SCE        IFEQ      '5'
509700020524     C                   MOVEL     DME(42)       PADSCE                         *TUTTI I CLIENTI X U
509800020524E   1C                   ENDIF
509900000000     C*--------------------
510000000000     C* PERIODO DI ELABORAZIONE
510100000000     C*--------------------
510200000000     C                   Z-ADD     D60DA2        PACDA2                         *ANNO/MESE FINALE
510300000000     C                   Z-ADD     D60DAT        PACDAT                         *ANNO/MESE INIZIALE
510400000000     C*--------------------
510500000000     C* SELEZIONI
510600000000     C*--------------------
510700000000     C                   MOVEL     D60SE1        PACSE1                         *TIPI SERVIZIO
510800000000     C                   MOVEL     D60SE2        PACSE2
510900000000     C                   MOVEL     D60SE3        PACSE3
511000000000     C                   MOVEL     D60SE4        PACSE4
511100000000     C                   MOVEL     D60SE5        PACSE5
511200000000     C                   MOVEL     D60PO1        PACPO1                         *TIPI PORTO
511300000000     C                   MOVEL     D60PO2        PACPO2
511400000000     C                   MOVEL     D60CT1        PACCT1                         *CODICI TARIFFA
511500000000     C                   MOVEL     D60CT2        PACCT2
511600000000     C                   MOVEL     D60CT3        PACCT3
511700000000     C                   MOVEL     D60CT4        PACCT4
511800000000     C                   MOVEL     D60CT5        PACCT5
511900000000     C                   MOVEL     D60CT6        PACCT6
512000000000     C                   MOVEL     D60CT7        PACCT7
512100000000     C                   MOVEL     D60CT8        PACCT8
512200000000     C                   MOVEL     D60CT9        PACCT9
512300000000     C                   MOVEL     D60CT0        PACCT0
512400000000     C                   MOVEL     D60FP1        PACFPA                         *FASCE DI PESO
512500000000     C                   MOVEL     D60FP2        PACFP0
512600000000     C                   MOVEL     D60FP3        PACFP9
512700000000     C                   MOVEL     D60FP4        PACFP8
512800000000     C                   MOVEL     D60FP5        PACFP7
512900000000     C                   MOVEL     D60FP6        PACFP6
513000000000     C                   MOVEL     D60FP7        PACFP5
513100000000     C                   MOVEL     D60FP8        PACFP4
513200000000     C                   MOVEL     D60FP9        PACFP3
513300000000     C                   MOVEL     D60FP0        PACFP2
513400000000     C                   MOVEL     D60FPA        PACFP1
513500000000     C*
513600000000     C* STAMPA PARAMETRI
513700020823IF  1C                   IF        D60OP1 <> 'STT'                              *NO SE PER OTTICO
513800020826     C                   MOVEL     '0'           WSTINT                         *NO INTESTAZIONE
513900020826     C                   EXSR      STATES                                       *TESTATA
514000071210      *
514100071211     c                   eval      Righe_da_stampare = 4
514200071211     c                   exsr      solo_x_STT
514300020826     C                   WRITE     SE62PA                                       *PARAMETRI
514400071210     c*
514500020823E   1C                   ENDIF
514600000000     C*
514700000000     C                   ENDSR
514800000000     C*------------------------------------------------------------------------*
514900000000     C*?STADGC - STAMPA PAGINA CON I DATI GENERALI DEL CLIENTE
515000000000     C*------------------------------------------------------------------------*
515100000000     C     STADGC        BEGSR
515200000000     C*
515300950725     C                   MOVEL     '1'           PASDGC                         *DATI GIA' STAMPATI
515400020826     C*
515500020826     C* STAMPA DATI CLIENTE (PADRE SE RICIESTA STATISTICA PER UNIFICANTE)
515600020826IF  1C     D60SCE        IFEQ      '2'                                          * + CLIENTI INSIEME
515700000821     C                   Z-ADD     1             C
515800020826X   1C                   ELSE                                                   *CLIENTE UNIFICANTE
515900000821     C                   MOVEL     CLI(1)        WKSC
516000000821     C                   EXSR      STADCL                                       *STAMPA DATI CLIENTE
516100000822     C                   Z-ADD     2             C
516200020826E   1C                   ENDIF
516300020826     C*
516400020826     C* STAMPA DATI CLIENTI FIGLI O PIU' CLIENTI INSIEME
516500020826DO  1C     C             DOWLE     500
516600000810     C     CLI(C)        ANDGT     *ZEROS
516700000811     C                   Z-ADD     *ZEROS        IS                5 0          *N.CLIENTI X RIGA
516800050221     C                   CLEAR                   blcC
516900050308     C                   CLEAR                   totC
517000000911     C                   CLEAR                   SEDC
517100000911     C                   CLEAR                   SCLI
517200000810     C                   CLEAR                   SRAG
517300020826DO  2C     C             DOWLE     500
517400020826     C     CLI(C)        ANDGT     *ZEROS
517500000811     C     IS            ANDLT     8
517600000811     C                   ADD       1             IS                             *CLIENTI NELLA RIGA
517700000911     C                   Z-ADD     CLI(C)        WKSC                           *REPERISCE DATI
517800000911     C                   EXSR      REPDCL
517900000911     C                   MOVEL     WEDC          SEDC(IS)
518000050221     C                   MOVEL     wblc          blcC(IS)
518100050308     C                   move      CDati(C)      Wtot                           *Cliente con dati
518200050221     C                   MOVEL     wtot          totC(IS)
518300000810     C                   Z-ADD     WKSC          SCLI(IS)
518400000811     C                   MOVEL     WDKSC         SRAG(IS)
518500000811     C                   ADD       1             C                              *CLIENTI
518600020826E   2C                   ENDDO                                                  *ROTTURA BLOCCO
518700000810     C*
518800000811     C* STAMPA BLOCCO DI 8 CLIENTI
518900010920     C                   CLEAR                   SE62C3
519000020826IF  2C     IS            IFGT      *ZEROS                                       *CLIENTI DA STAMPARE
519100000911     C                   MOVEL     SEDC(1)       CL3ED1
519200050221     c                   eval      %subst(CL3ED1:2:1) = blcc(1)
519300050221     c                   eval      %subst(CL3ED1:3:1) = totc(1)
519400000911     C                   Z-ADD     SCLI(1)       CL3KS1
519500000911     C                   MOVEL     SRAG(1)       CL3RA1
519600000911     C                   MOVEL     SEDC(2)       CL3ED2
519700050221     c                   eval      %subst(CL3ED2:2:1) = blcc(2)
519800050221     c                   eval      %subst(CL3ED2:3:1) = totc(2)
519900000911     C                   Z-ADD     SCLI(2)       CL3KS2
520000000911     C                   MOVEL     SRAG(2)       CL3RA2
520100000911     C                   MOVEL     SEDC(3)       CL3ED3
520200050221     c                   eval      %subst(CL3ED3:2:1) = blcc(3)
520300050221     c                   eval      %subst(CL3ED3:3:1) = totc(3)
520400000911     C                   Z-ADD     SCLI(3)       CL3KS3
520500000911     C                   MOVEL     SRAG(3)       CL3RA3
520600000911     C                   MOVEL     SEDC(4)       CL3ED4
520700050221     c                   eval      %subst(CL3ED4:2:1) = blcc(4)
520800050221     c                   eval      %subst(CL3ED4:3:1) = totc(4)
520900000911     C                   Z-ADD     SCLI(4)       CL3KS4
521000000911     C                   MOVEL     SRAG(4)       CL3RA4
521100000911     C                   MOVEL     SEDC(5)       CL3ED5
521200050221     c                   eval      %subst(CL3ED5:2:1) = blcc(5)
521300050221     c                   eval      %subst(CL3ED5:3:1) = totc(5)
521400000911     C                   Z-ADD     SCLI(5)       CL3KS5
521500000911     C                   MOVEL     SRAG(5)       CL3RA5
521600000911     C                   MOVEL     SEDC(6)       CL3ED6
521700050221     c                   eval      %subst(CL3ED6:2:1) = blcc(6)
521800050221     c                   eval      %subst(CL3ED6:3:1) = totc(6)
521900000911     C                   Z-ADD     SCLI(6)       CL3KS6
522000000911     C                   MOVEL     SRAG(6)       CL3RA6
522100000911     C                   MOVEL     SEDC(7)       CL3ED7
522200050221     c                   eval      %subst(CL3ED7:2:1) = blcc(7)
522300050221     c                   eval      %subst(CL3ED7:3:1) = totc(7)
522400000911     C                   Z-ADD     SCLI(7)       CL3KS7
522500000911     C                   MOVEL     SRAG(7)       CL3RA7
522600000911     C                   MOVEL     SEDC(8)       CL3ED8
522700050221     c                   eval      %subst(CL3ED8:2:1) = blcc(8)
522800050221     c                   eval      %subst(CL3ED8:3:1) = totc(8)
522900000911     C                   Z-ADD     SCLI(8)       CL3KS8
523000000911     C                   MOVEL     SRAG(8)       CL3RA8
523100020826     C                   MOVEL     '0'           WSTINT                         *NO INTESTAZIONE
523200020826     C                   EXSR      STATES
523300071210      *
523400071211     c                   eval      Righe_da_stampare = 2
523500071211     c                   exsr      solo_x_STT
523600010920     C                   WRITE     SE62C3                                       *SCRIVE RIGA
523700170801     c                   eval      $ClienteStampato  = *on
523800071210      *
523900020826E   2C                   ENDIF
524000020826E   1C                   ENDDO
524100950725     C*
524200050308     C     C             ifgt      1
524300050309     c                   eval      cl3leg = '(B) = Cliente Bloccato  -  '+
524400050309     c                             '(*) = Cliente senza dati nel Periodo ' +
524500050309     c                             'selezionato'
524600050308     C                   EXSR      STATES
524700071210      *
524800071211     c                   eval      Righe_da_stampare = 2
524900071211     c                   exsr      solo_x_STT
525000050308     C                   WRITE     SE62L3                                       *SCRIVE Legenda
525100071210      *
525200050308E   2C                   ENDIF
525300050308     C*
525400000000     C                   ENDSR
525500000821     C*------------------------------------------------------------------------*
525600000821     C*?STADCL - STAMPA DATI CLIENTE
525700000821     C*------------------------------------------------------------------------*
525800000821     C     STADCL        BEGSR
525900000821     C*
526000000821     C                   CLEAR                   DSCL
526100020916     C                   Z-ADD     1             DCLKUT
526200000821     C                   Z-ADD     WKSC          DCLKSC
526300020117     C                   CALL      'TISE66R'
526400000821     C                   PARM                    DSCL
526500000821     C                   MOVEL     DCLKSC        CLCKSC
526600000821     C                   MOVEL     DCLCLV        CLCCLV
526700020826     C                   MOVEL(P)  DCLRAG        CLCRAG
526800000821     C                   Z-ADD     DCLITC        CLCITC
526900000821     C                   MOVEL     DCLDIT        CLDITC
527000000821     C                   MOVEL     DCLVIA        CLCVIA
527100000821     C                   Z-ADD     DCLAGE        CLCAGE
527200000821     C                   MOVEL     DCLDAG        CLDAGE
527300000821     C                   MOVEL     DCLCIT        CLCCIT
527400000821     C                   Z-ADD     DCLDPS        CLCDPS
527500000821     C                   MOVEL     DCLCAP        CLCCAP
527600000821     C                   MOVEL     DCLPRV        CLCPRV
527700000821     C                   MOVEL     DCLSTA        CLCSTA
527800000821     C                   MOVEL     DCLCDP        CLCCDP
527900000821     C                   MOVEL     DCLDCD        CLDCDP
528000000821     C                   MOVEL     DCLTEL        CLCTEL
528100000821     C                   MOVEL     DCLSOL        CLCSOL
528200020826     C                   MOVEL     DCLKSC        CL2KSC                         *STAMPA X OTTICO
528300020826     C                   MOVEL(P)  DCLRAG        CL2RAG
528400020823     C                   MOVEL     DEPTFP        CL2TFP
528500020826     C                   MOVEL     '0'           WSTINT                         *NO INTESTAZIONE
528600020826     c                   EXSR      STATES                                       *STAMPA TESTATA
528700071210      *
528800071211     c                   eval      Righe_da_stampare = 3
528900071211     c                   exsr      solo_x_STT
529000020823     C                   WRITE     SE62CL                                       *DATI UN SOLO CLIENT
529100170801     c                   eval      $ClienteStampato  = *on
529200071210      *
529300020823     C                   EXSR      STATES
529400000821     C*
529500000821     C                   ENDSR
529600000810     C*------------------------------------------------------------------------*
529700000911     C*?REPDCL - REPERISCE DATI CLIENTE
529800000810     C*------------------------------------------------------------------------*
529900000911     C     REPDCL        BEGSR
530000000810     C*
530100000810     C* AZZERA VARIABILI DI LAVORO
530200000810     C                   CLEAR                   WDKSC
530300000911     C                   CLEAR                   WEDC
530400000810     C*
530500000810     C* CHIAMA IL PGM DI RICERCA CLIENTE
530600000810     C                   CLEAR                   BS69DS
530700000810     C                   CLEAR                   ACODS
530800000810     C                   MOVEL     KNSIF         I69SIF
530900000810     C                   MOVEL     WKSC          I69KAC
531000000810     C                   CALL      'TIBS69R'
531100000810     C                   PARM                    BS69DS
531200000810     C                   PARM                    ACODS
531300000810     C                   PARM                    INDDS
531400000810     C                   PARM                    CLPDS
531500000810     C                   PARM                    CLSDS
531600000810IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
531700000810     C                   MOVEL     ACORAG        WDKSC
531800050217     C                   MOVEL     ACOabl        KscABL
531900000810E   1C                   ENDIF
532000000911     C*
532100020916     C* se in filiale, CONTROLLA SE I DATI DEL CLIENTE SONO PRESENTI SUL QUEL AS/400
532200050307E   1C********
532300050307E   1C********  Su AZUTE00F il campo UTEAUT non è mai = *blank inoltre la schiera SKI contiene tutte
532400050307E   1C********  sull'AS Unico quindi questo Test non serve + molto
532500050307IF  1C**** UTEAUT        IFEQ      *blanks                                      *IN FILIALE
532600050307     C********           MOVEL     WKSC          WFIL              3 0
532700050307     C********           Z-ADD     1             J
532800050307     C********           SETOFF                                           28
532900050307     C**** WFIL          LOOKUP    SKI(J)                                 28
533000050307IF  2C**** *IN28         IFEQ      *OFF                                         *NON TROVATA
533100050307     C********           MOVEL     '*'           WEDC                           *NO DATI SULL'AS/400
533200050307E   2C********           ENDIF
533300050307E   1C********           ENDIF
533400050307E   1C********
533500050307E   1C**  Utilizzeremo il flag WEDC con (*) per segnalare che il codice cliente, quando il pgm è
533600050307E   1C**  chiamato come UNIFICANTE, non ha contribuito ai dati emessi in stampa statistica.
533700050217     C*
533800050221     C*  Attenzione: se il cliente è bloccato deve essere segnalato con (B)
533900050221     C*              e se non contribuisce alla totalizzazione (non attivato)
534000050221     C                   clear                   wtot              1
534100050221     C                   clear                   wblc              1
534200130315     c                   if        kscABL <> *blank and kscabl <> '*'
534300050307     c                   eval      wblc ='B'
534400050217     c                   end
534500000810     C*
534600000810     C                   ENDSR
534700000000     C*------------------------------------------------------------------------*
534800000000     C*?CARTAB - CARICA TABELLE (FASCE, REGIONI, SERVIZI, ORE CONSEGNE)
534900000000     C*------------------------------------------------------------------------*
535000000000     C     CARTAB        BEGSR
535100000000     C*
535200020916     C                   Z-ADD     1             KTBUTE                         *CODICE UTENTE
535300990217     C*--------------------
535400000000     C* FASCE DI PESO
535500990217     C*--------------------
535600990601IF  1C     D60DA2        IFLE      199812                                       *FASCE FINO AL '98
535700990217     C                   MOVEL     'SDC'         WSX               3
535800020325X   1C                   ELSE                                                   *FASCE DAL '99 AL 20
535900020325     C     D60DA2        IFLE      200112
536000020325     C                   MOVEL     'S9C'         WSX               3
536100020325X   1C                   ELSE                                                   *FASCE DAL 2002
536200020325     C                   MOVEL     'S2C'         WSX
536300020325     C                   ENDIF
536400020325E   1C                   ENDIF
536500000000     C                   MOVEL     '2L'          KTBCOD
536600000000     C     KEYTA2        SETLL     TABEL00F
536700000000     C     KEYTA2        READE     TABEL00F                               99
536800000000     C                   Z-ADD     *ZEROS        J                              *INDICE
536900000000DO  1C     *IN99         DOWEQ     *OFF
537000000000IF  2C     TBLFLG        IFEQ      *BLANKS
537100990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
537200000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
537300990217IF  3C     FILEST        IFEQ      WSX
537400000000     C                   ADD       1             J
537500000000     C                   MOVE      STACOD        FPE(J)                         *CODICE STATISTICA
537600000000     C                   MOVEL     TBLUNI        DS2L
537700000000     C                   MOVEL     §2LDFP        DFPE(J)                        *DECODIFICA FASCIA
537800000000E   3C                   ENDIF
537900000000E   2C                   ENDIF
538000000000     C     KEYTA2        READE     TABEL00F                               99
538100000000E   1C                   ENDDO
538200000000     C***
538300000000     C* PARF = N° FASCE ESISTENTI
538400000000     C***
538500000000     C                   Z-ADD     J             PARF                           *NON MODIFICARE
538600000000     C*
538700000000     C* NESSUNA FASCIA SELEZIONATA
538800000000IF  1C     D60FP1        IFEQ      *BLANKS
538900000000     C     D60FP2        ANDEQ     *BLANKS
539000000000     C     D60FP3        ANDEQ     *BLANKS
539100000000     C     D60FP4        ANDEQ     *BLANKS
539200000000     C     D60FP5        ANDEQ     *BLANKS
539300000000     C     D60FP6        ANDEQ     *BLANKS
539400000000     C     D60FP7        ANDEQ     *BLANKS
539500000000     C     D60FP8        ANDEQ     *BLANKS
539600000000     C     D60FP9        ANDEQ     *BLANKS
539700000000     C     D60FP0        ANDEQ     *BLANKS
539800000000     C     D60FPA        ANDEQ     *BLANKS
539900000000     C                   MOVEL     'T'           SELFPE                         *TUTTE LE FASCE
540000000000     C***
540100000000     C* PARF = N° FASCE SELEZIONATE UGUALE A QUELLE ESISTENTI
540200000000     C***
540300000000     C                   MOVEL     FPE           SFPE                           *FASCE ESIST = SELEZ
540400000000     C                   Z-ADD     PARF          SPARF                          *NON MODIFICARE
540500000000X   1C                   ELSE
540600000000     C* FASCE SELEZIONATE
540700000000     C                   Z-ADD     *ZEROS        J
540800000000IF  2C     D60FP1        IFNE      *BLANKS
540900000000     C                   SETOFF                                           28
541000000000     C     D60FP1        LOOKUP    SFPE(1)                                28
541100000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
541200000000     C                   ADD       1             J
541300000000     C                   MOVEL     D60FP1        SFPE(J)
541400000000E   3C                   ENDIF
541500000000E   2C                   ENDIF
541600000000     C*
541700000000IF  2C     D60FP2        IFNE      *BLANKS
541800000000     C                   SETOFF                                           28
541900000000     C     D60FP2        LOOKUP    SFPE(1)                                28
542000000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
542100000000     C                   ADD       1             J
542200000000     C                   MOVEL     D60FP2        SFPE(J)
542300000000E   3C                   ENDIF
542400000000E   2C                   ENDIF
542500000000     C*
542600000000IF  2C     D60FP3        IFNE      *BLANKS
542700000000     C                   SETOFF                                           28
542800000000     C     D60FP3        LOOKUP    SFPE(1)                                28
542900000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
543000000000     C                   ADD       1             J
543100000000     C                   MOVEL     D60FP3        SFPE(J)
543200000000E   3C                   ENDIF
543300000000E   2C                   ENDIF
543400000000     C*
543500000000IF  2C     D60FP4        IFNE      *BLANKS
543600000000     C                   SETOFF                                           28
543700000000     C     D60FP4        LOOKUP    SFPE(1)                                28
543800000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
543900000000     C                   ADD       1             J
544000000000     C                   MOVEL     D60FP4        SFPE(J)
544100000000E   3C                   ENDIF
544200000000E   2C                   ENDIF
544300000000     C*
544400000000IF  2C     D60FP5        IFNE      *BLANKS
544500000000     C                   SETOFF                                           28
544600000000     C     D60FP5        LOOKUP    SFPE(1)                                28
544700000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
544800000000     C                   ADD       1             J
544900000000     C                   MOVEL     D60FP5        SFPE(J)
545000000000E   3C                   ENDIF
545100000000E   2C                   ENDIF
545200000000     C*
545300000000IF  2C     D60FP6        IFNE      *BLANKS
545400000000     C                   SETOFF                                           28
545500000000     C     D60FP6        LOOKUP    SFPE(1)                                28
545600000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
545700000000     C                   ADD       1             J
545800000000     C                   MOVEL     D60FP6        SFPE(J)
545900000000E   3C                   ENDIF
546000000000E   2C                   ENDIF
546100000000     C*
546200000000IF  2C     D60FP7        IFNE      *BLANKS
546300000000     C                   SETOFF                                           28
546400000000     C     D60FP7        LOOKUP    SFPE(1)                                28
546500000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
546600000000     C                   ADD       1             J
546700000000     C                   MOVEL     D60FP7        SFPE(J)
546800000000E   3C                   ENDIF
546900000000E   2C                   ENDIF
547000000000     C*
547100000000IF  2C     D60FP8        IFNE      *BLANKS
547200000000     C                   SETOFF                                           28
547300000000     C     D60FP8        LOOKUP    SFPE(1)                                28
547400000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
547500000000     C                   ADD       1             J
547600000000     C                   MOVEL     D60FP8        SFPE(J)
547700000000E   3C                   ENDIF
547800000000E   2C                   ENDIF
547900000000     C*
548000000000IF  2C     D60FP9        IFNE      *BLANKS
548100000000     C                   SETOFF                                           28
548200000000     C     D60FP9        LOOKUP    SFPE(1)                                28
548300000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
548400000000     C                   ADD       1             J
548500000000     C                   MOVEL     D60FP9        SFPE(J)
548600000000E   3C                   ENDIF
548700000000E   2C                   ENDIF
548800000000     C*
548900000000IF  2C     D60FP0        IFNE      *BLANKS
549000000000     C                   SETOFF                                           28
549100000000     C     D60FP0        LOOKUP    SFPE(1)                                28
549200000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
549300000000     C                   ADD       1             J
549400000000     C                   MOVEL     D60FP0        SFPE(J)
549500000000E   3C                   ENDIF
549600000000E   2C                   ENDIF
549700000000     C*
549800000000IF  2C     D60FPA        IFNE      *BLANKS
549900000000     C                   SETOFF                                           28
550000000000     C     D60FPA        LOOKUP    SFPE(1)                                28
550100000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
550200000000     C                   ADD       1             J
550300000000     C                   MOVEL     D60FPA        SFPE(J)
550400000000E   3C                   ENDIF
550500000000E   2C                   ENDIF
550600000000     C***
550700000000     C* PARF = N° FASCE SELEZIONATE
550800000000     C***
550900000000     C                   Z-ADD     J             SPARF                          *NON MODIFICARE
551000000000E   1C                   ENDIF
551100000000     C*-------------------------
551200000000     C* REGIONI
551300000000     C*-------------------------
551400000000     C                   Z-ADD     *ZEROS        KORFIL                         *CODICE FILIALE
551500000000     C     KORFIL        SETLL     AZORG01L
551600000911     C                   READ      AZORG01L                               99
551700000000     C                   Z-ADD     *ZEROS        J
551800000000DO  1C     *IN99         DOWEQ     *OFF
551900000000IF  2C     ORGFVA        IFEQ      *BLANKS
552000000000     C     ORGFAG        ANDNE     'V'
552100000000     C                   ADD       1             J
552200000000     C*
552300000000     C* REPERISCO IL RAGGRUPPAMENTO REGIONE DELLA FILIALE
552400000000     C                   MOVEL     '14'          KTBCOD
552500000000     C                   MOVEL     *BLANKS       KTBKEY
552600000000     C                   MOVEL     ORGCRE        KTBKEY
552700000000     C     KEYTAB        CHAIN     TABEL00F                           99
552800000000IF  3C     *IN99         IFEQ      *OFF
552900000000     C     TBLFLG        ANDEQ     *BLANKS
553000000000     C                   MOVEL     TBLUNI        DS14
553100020325     C* VERIFICO QULI RAGGRUPPAMENTI CONSIDERARE IN RELAZIONE ALL'ANNO DEL PERIODO DA ELABORARE
553200020325     C     D60DA2        IFLE      200112
553300020325     C                   MOVEL     §14CRR        DEPCRN            3 0
553400020325     C                   ELSE
553500020325     C                   MOVEL     §14CRN        DEPCRN
553600020325     C                   ENDIF
553700020325     C*
553800020325     C                   MOVEL     DEPCRN        CRR(J)                         *CODICE RAGGRUPAM.TO
553900000000     C*
554000000000     C* DECODIFICA RAGGRUPPAMENTO
554100000000     C                   MOVEL     '2M'          KTBCOD
554200000000     C                   MOVEL     *BLANKS       KTBKEY
554300020325     C                   MOVEL     DEPCRN        KTBKEY
554400000000     C     KEYTAB        CHAIN     TABEL00F                           99
554500000000IF  4C     *IN99         IFEQ      *OFF
554600000000     C     TBLFLG        ANDEQ     *BLANKS
554700000000     C                   MOVEL     TBLUNI        DS2M
554800000000     C                   MOVEL     §2MDES        DCRR(J)                        *DECODIFICA RAGGR.TO
554900000000X   4C                   ELSE
555000000000     C                   MOVEL     *ALL'*'       DCRR(J)
555100000000E   4C                   ENDIF
555200000000X   3C                   ELSE
555300000000     C                   MOVEL     *ALL'9'       CRR(J)
555400000000E   3C                   ENDIF
555500000000E   2C                   ENDIF
555600000911     C                   READ      AZORG01L                               99
555700000000E   1C                   ENDDO
555800950206     C*
555900950206     C* CARICAMENTO TIPI VALORI POSSIBILI DA STAMPARE
556000950206     C                   MOVEL     'C'           VRR(1)                         *COLLI
556100950206     C                   MOVEL     DME(26)       DVRR(1)
556200950206     C                   MOVEL     'P'           VRR(2)                         *PESO
556300950206     C                   MOVEL     DME(27)       DVRR(2)
556400950206     C                   MOVEL     'R'           VRR(3)                         *RICAVI
556500950206     C                   MOVEL     DME(28)       DVRR(3)
556600950206     C                   MOVEL     'S'           VRR(4)                         *SPEDIZIONI
556700950206     C                   MOVEL     DME(29)       DVRR(4)
556800950206     C                   MOVEL     'V'           VRR(5)                         *VOLUME
556900950206     C                   MOVEL     DME(30)       DVRR(5)
557000000000     C*-------------------------
557100000000     C* TIPI SERVIZIO
557200000000     C*-------------------------
557300000000     C* CARICAMENTO SERVIZI ESISTENTI
557400000000     C                   MOVEL     '5E'          KTBCOD
557500000000     C     KEYTA2        SETLL     TABEL00F
557600000000     C     KEYTA2        READE     TABEL00F                               99
557700000000     C*
557800000000     C                   Z-ADD     *ZEROS        J                              *INDICE
557900000000DO  1C     *IN99         DOWEQ     *OFF
558000000000IF  2C     TBLFLG        IFEQ      *BLANKS
558100000000     C                   ADD       1             J
558200000000     C                   MOVEL     TBLKEY        SER(J)                         *CODICE SERVIZIO
558300091013     C                   MOVEL     TBLUNI        DS5e                           *DESCRIZIONE SERVIZI
558400091013     C******             MOVEL     TBLUNI        DSER(J)                        *DESCRIZIONE SERVIZI
558500091013     C                   MOVEL(p)  §5ed08        DSER(J)                        *DESCRIZIONE SERVIZI
558600000000E   2C                   ENDIF
558700000000     C*
558800000000     C     KEYTA2        READE     TABEL00F                               99
558900000000E   1C                   ENDDO
559000000000     C***
559100000000     C* PARS = N° SERVIZI ESISTENTI
559200000000     C***
559300000000     C                   Z-ADD     J             PARS                           *NON MODIFICARE
559400000000     C*
559500000000IF  1C     D60SE1        IFEQ      ' '
559600000000     C     D60SE2        ANDEQ     ' '
559700000000     C     D60SE3        ANDEQ     ' '
559800000000     C     D60SE4        ANDEQ     ' '
559900000000     C     D60SE5        ANDEQ     ' '
560000000000     C                   MOVEL     'T'           SELSER                         *TUTTI I SERVIZI
560100000000     C                   MOVEL     SER           SERS
560200000000     C***
560300000000     C* SPARS = N° SERVIZI SELEZIONATI (TUTTI)
560400000000     C***
560500000000     C                   Z-ADD     J             SPARS                          *NON MODIFICARE
560600000000     C*
560700000000X   1C                   ELSE
560800000000     C*
560900000000     C                   Z-ADD     *ZEROS        J
561000000000IF  2C     D60SE1        IFNE      ' '
561100000000     C                   SETOFF                                           28
561200000000     C     D60SE1        LOOKUP    SERS(1)                                28     *SE GIA' IMMESSO
561300000000IF  3C     *IN28         IFEQ      *OFF                                          *NON TROVATO
561400000000     C                   ADD       1             J
561500000000     C                   MOVEL     D60SE1        SERS(J)
561600000000E   3C                   ENDIF
561700000000E   2C                   ENDIF
561800000000     C*
561900000000IF  2C     D60SE2        IFNE      ' '
562000000000     C                   SETOFF                                           28
562100000000     C     D60SE2        LOOKUP    SERS(1)                                28
562200000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
562300000000     C                   ADD       1             J
562400000000     C                   MOVEL     D60SE2        SERS(J)
562500000000E   3C                   ENDIF
562600000000E   2C                   ENDIF
562700000000     C*
562800000000IF  2C     D60SE3        IFNE      ' '
562900000000     C                   SETOFF                                           28
563000000000     C     D60SE3        LOOKUP    SERS(1)                                28
563100000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
563200000000     C                   ADD       1             J
563300000000     C                   MOVEL     D60SE3        SERS(J)
563400000000E   3C                   ENDIF
563500000000E   2C                   ENDIF
563600000000     C*
563700000000IF  2C     D60SE4        IFNE      ' '
563800000000     C                   SETOFF                                           28
563900000000     C     D60SE4        LOOKUP    SERS(1)                                28
564000000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
564100000000     C                   ADD       1             J
564200000000     C                   MOVEL     D60SE4        SERS(J)
564300000000E   3C                   ENDIF
564400000000E   2C                   ENDIF
564500000000     C***
564600000000     C* SPARS = N° SERVIZI SELEZIONATI
564700000000     C***
564800000000     C                   Z-ADD     J             SPARS                          *NON MODIFICARE
564900000000     C*
565000000000E   1C                   ENDIF
565100000000     C*-------------------------
565200000000     C* ORE DI CONSEGNA
565300000000     C*-------------------------
565400000000     C                   Z-ADD     -000          ORE(1)                         *-000
565500000000     C                   MOVEL     DORA1         DORE(1)                        *IN ANTICIPO
565600000000     C                   Z-ADD     000           ORE(2)                         *000
565700000000     C                   MOVEL     DORA2         DORE(2)                        *IN ORARIO
565800000000     C                   Z-ADD     012           ORE(3)                         *012
565900000000     C                   MOVEL     DORA3         DORE(3)                        *IN RITARDO 12 H
566000000000     C                   Z-ADD     024           ORE(4)                         *024
566100000000     C                   MOVEL     DORA4         DORE(4)                        *IN RITARDO 24 H
566200000000     C                   Z-ADD     036           ORE(5)                         *036
566300000000     C                   MOVEL     DORA5         DORE(5)                        *IN RITARDO 36 H
566400000000     C                   Z-ADD     048           ORE(6)                         *048
566500000000     C                   MOVEL     DORA6         DORE(6)                        *IN RITARDO 48 H
566600000000     C                   Z-ADD     +048          ORE(7)                         *+048
566700000000     C                   MOVEL     DORA7         DORE(7)                        *IN RIT DI OLTRE 48H
566800000000     C***
566900000000     C* PARO  = N° ORE DI CONSEGNA ESISTENTI
567000000000     C***
567100000000     C                   Z-ADD     7             PARO                           *NON MODIFICARE
567200000000     C*-------------------------
567300000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
567400000000     C*-------------------------
567500000000     C                   MOVEL     '2W'          KTBCOD
567600000000     C     KEYTA2        SETLL     TABEL00F
567700000000     C     KEYTA2        READE     TABEL00F                               99
567800000000     C*
567900000000     C                   Z-ADD     *ZEROS        G                 4 0
568000000000DO  1C     *IN99         DOWEQ     *OFF
568100000000IF  2C     TBLFLG        IFEQ      *BLANKS
568200000000     C                   ADD       1             G
568300000000     C                   MOVEL     TBLKEY        FLAB(G)
568400000000     C                   MOVEL     TBLUNI        DS2W
568500000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
568600000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
568700000000E   2C                   ENDIF
568800000000     C*
568900000000     C     KEYTA2        READE     TABEL00F                               99
569000000000E   1C                   ENDDO
569100000000     C*-------------------------
569200000000     C* SELEZIONI PARAMETRI GENERALI
569300000000     C*-------------------------
569400000000IF  1C     D60PO1        IFEQ      ' '
569500000000     C     D60PO2        ANDEQ     ' '
569600000000     C                   MOVEL     'T'           SELPOR                         *TUTTI I PORTI
569700000000E   1C                   ENDIF
569800000000IF  1C     D60PR1        IFEQ      ' '
569900000000     C     D60PR2        ANDEQ     ' '
570000000000     C     D60PR3        ANDEQ     ' '
570100000000     C     D60PR4        ANDEQ     ' '
570200000000     C     D60PR5        ANDEQ     ' '
570300000000     C                   MOVEL     'T'           SELPRO                         *TUTTI I PRODOTTI
570400000000E   1C                   ENDIF
570500000000IF  1C     D60CT1        IFEQ      *BLANKS
570600000000     C     D60CT2        ANDEQ     *BLANKS
570700000000     C     D60CT3        ANDEQ     *BLANKS
570800000000     C     D60CT4        ANDEQ     *BLANKS
570900000000     C     D60CT5        ANDEQ     *BLANKS
571000000000     C     D60CT6        ANDEQ     *BLANKS
571100000000     C     D60CT7        ANDEQ     *BLANKS
571200000000     C     D60CT8        ANDEQ     *BLANKS
571300000000     C     D60CT9        ANDEQ     *BLANKS
571400000000     C     D60CT0        ANDEQ     *BLANKS
571500000000     C                   MOVEL     'T'           SELCTR                         *TUTTI LE TARIFFE
571600000000E   1C                   ENDIF
571700000000     C*
571800070628     C*-------------------------
571900070628      * Tipi Varie da tabella "CA" per visualizzazione  ?
572000070628     C                   MOVEL     'CA'          KTBCOD
572100070628     C                   z-add     9999999       SkOrd
572200070628     C     KEYTA2        SETLL     TABEL00F
572300070628     C     KEYTA2        READE     TABEL00F                               99
572400070628     C*
572500070628     C                   Z-ADD     0             CA                3 0
572600070628     C                   Z-ADD     0             sav_CA            3 0
572700070628DO  1C     *IN99         DOWEQ     *OFF
572800070628IF  2C     TBLFLG        IFEQ      *BLANKS
572900070628     C                   MOVEL     TBLUNI        DSCA
573000070628     C* se abilitato alla visualizzazione su statistica
573100070628     c                   if        §CAstc = 'S'
573200070628     C                   ADD       1             CA
573300070628     C* solo il byte + a destra del codice
573400070628     C                   MOVE      TBLKEY        SkVar(CA)
573500070628      * riga di Ricavi con intestazione da tabella
573600070628      *?    tranne .... che è una varia particolare non da ricavi .  ?
573700070628     c                   if        SkVar(CA) <> '#'
573800070628     C                   MOVE      'RIC.'        SkDsc(CA)
573900070628     c                   end
574000070628      *?    ...quindi attenzione....  ?
574100070628     C                   MOVEL     §CAdsc        SkDsc(CA)
574200070628     C                   eval      SkOrd(CA) = (§CAord * 1000) + CA
574300070628     C                   Z-ADD     CA            sav_CA
574400070628     c                   end
574500070628E   2C                   ENDIF
574600070628     C*
574700070628     C     KEYTA2        READE     TABEL00F                               99
574800070628E   1C                   ENDDO
574900070628     C*
575000070703      *? Un eccezione per la VARIA # che non è una varia di tassazione ma di solo C/E  ?
575100070703      *?  La aggiungo in fondo come ultima da visualizzare a mo' di chiodo poichè non
575200070703      *?  deve essere gestita in tabella.                                 ?
575300070703     C                   ADD       1             CA
575400070703     C* solo il byte + a destra del codice
575500070703     C                   MOVE      '#'           SkVar(CA)
575600070703      *?    .... è una varia particolare non da ricavi .  ?
575700070703     C                   eval      SkDsc(CA) = 'Nr.Lasciati Avvisi     '
575800070703      *?    ... in ordinamento la metto in fondo (888).  ?
575900070703     C                   eval      SkOrd(CA) = (888 * 1000) + CA
576000070703     C                   Z-ADD     CA            sav_CA
576100070703     C*
576200070628      *? Riordina le schiere per SkOrd ascendente dove le prime 3 cifre sono l'ordine  ?
576300070628      *?  espresso sulla tabella "CA" e le restanti 3 sono l'indice delle skiere per   ?
576400070628      *?  agganciare le decodifiche in visualizzazione                                 ?
576500070628     c                   sorta     SkOrd
576600070628     C*-------------------------
576700070628     C*
576800070628     C*
576900000000     C                   ENDSR
577000020905     C*--------------------------------------------------------------------------------------------*
577100020905     C* REPERISCE I DATI UTENTE
577200020905     C*--------------------------------------------------------------------------------------------*
577300020905     C     REPDATIUTE    BEGSR
577400020905     C*
577500020905     C* INIZIALIZZA VARIABILI DI WRK
577600020905     C                   CLEAR                   TIBS34DS
577700020905     C                   CLEAR                   AZUTEDS
577800020905     C                   CLEAR                   DDATIUTE
577900020905     C*
578000020905     C     *DTAARA       DEFINE    §azute        azuteds
578100020905     C     *DTAARA       DEFINE    §datiute      ddatiute
578200020905     C                   IN(E)     *DTAARA
578300020905if  1C                   IF        %Error
578400020905     c                   EVAL      I34Tla = 'L'
578500020905     C                   CALL      'TIBS34R'
578600020905     C                   PARM                    Tibs34Ds
578700020905     C                   IN        *DTAARA
578800020905e   1C                   ENDIF
578900020905     C*
579000020905     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
579100020916     C                   MOVEL(P)  RSUT          DSFIRS
579200020910     C*
579300020910     C* REPERISCO TUTTI I P.O. PRESENTI SUL P.O. DELL'UTENTE
579400020910     C                   CLEAR                   TIBS56DS
579500020910     C                   EVAL      I56PPO = DUTPOU
579600020910     C                   EVAL      I56MOD = 'POA'
579700020910     C                   CALL      'TIBS56R'
579800020910     C                   PARM                    TIBS56DS
579900020905     C*
580000020905     C                   ENDSR
580100000000     C*------------------------------------------------------------------------*
580200000000     C*?CARMES - CARICA I MESI/ANNI DA ELEBORARE
580300000000     C*------------------------------------------------------------------------*
580400000000     C     CARMES        BEGSR
580500000000     C*
580600000000     C                   CLEAR                   ANN                            *ANNI
580700000000     C                   CLEAR                   MES                            *MESI
580800000000     C                   Z-ADD     *ZEROS        J                              *NUMERO MESI DA ELAB
580900000000     C*
581000990601     C                   MOVEL     D60DA2        WAA2              4 0          *ANNO FINALE LAVORO
581100000000     C                   MOVE      D60DA2        WMM2              2 0          *MESE FINALE LAVORO
581200990601     C                   MOVEL     D60DAT        WAA               4 0          *ANNO INIZIALE LAVOR
581300000000     C                   MOVE      D60DAT        WMM               2 0          *MESE INIZIALE LAVOR
581400000000     C*
581500000000     C* ANNO INIZIALE UGUALE AD ANNO FINALE
581600000000IF  1C     WAA2          IFEQ      WAA
581700000000     C*
581800000000     C* CALCOLO I MESI DI DIFFERENZA
581900000000DO  2C     WMM           DOWLE     WMM2
582000000000     C                   ADD       1             J
582100000000     C                   Z-ADD     WAA           ANN(J)
582200000000     C                   Z-ADD     WMM           MES(J)
582300000000     C                   ADD       1             WMM
582400000000E   2C                   ENDDO
582500000000     C***
582600000000     C* SPARM = N° MESI SELEZIONATI
582700000000     C***
582800000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
582900000000     C*
583000000000     C* IL RESTO DELLA SCHIERA A ZERO
583100000000DO  2C     J             DOWLT     12
583200000000     C                   ADD       1             J
583300000000     C                   Z-ADD     *ZEROS        ANN(J)
583400000000     C                   Z-ADD     *ZEROS        MES(J)
583500000000E   2C                   ENDDO
583600000000     C*
583700000000     C* ANNO FINALE DIVERSO DA ANNO INIZIALE
583800000000X   1C                   ELSE
583900000000     C*
584000000000     C* MEMORIZZO I MESI DELL'ANNO INIZIALE
584100000000DO  2C     WMM           DOWLE     12
584200000000     C                   ADD       1             J
584300000000     C                   Z-ADD     WAA           ANN(J)
584400000000     C                   Z-ADD     WMM           MES(J)
584500000000     C                   ADD       1             WMM
584600000000E   2C                   ENDDO
584700000000     C*
584800000000     C* MEMORIZZO I MESI DELL'ANNO FINALE
584900000000     C                   Z-ADD     01            WMM
585000000000DO  2C     WMM           DOWLE     WMM2
585100000000     C                   ADD       1             J
585200000000     C                   Z-ADD     WAA2          ANN(J)
585300000000     C                   Z-ADD     WMM           MES(J)
585400000000     C                   ADD       1             WMM
585500000000E   2C                   ENDDO
585600000000     C***
585700000000     C* SPARM = N° MESI SELEZIONATI
585800000000     C***
585900000000     C                   Z-ADD     J             SPARM                          *NON MODIFICARE
586000000000     C*
586100000000     C* IL RESTO DELLA SCHIERA A ZERO
586200000000DO  2C     J             DOWLT     12
586300000000     C                   ADD       1             J
586400000000     C                   Z-ADD     *ZEROS        ANN(J)
586500000000     C                   Z-ADD     *ZEROS        MES(J)
586600000000E   2C                   ENDDO
586700000000E   1C                   ENDIF
586800000000     C*
586900000000     C                   ENDSR
587000020603     C*------------------------------------------------------------------------*
587100020604     C*?ORDCRER - ORDINA L'OCCUR "REGIONI-REGIONI"
587200020603     C*------------------------------------------------------------------------*
587300020603     C     ORDCRER       BEGSR
587400020603     C*
587500020603     C                   CLEAR                   SKCRER
587600020603     C*
587700020603     C* PASSO L'OCCUR NELLA SCHIERA
587800020604DO  1C     1             DO        500           YY
587900020604     C     YY            OCCUR     CRER
588000020605     C                   MOVEL     CRER          SKCRER(YY)
588100020605     C***                EVAL      SKCRER(YY) = CRER
588200020603E   1C                   ENDDO
588300020603     C*
588400020603     C* ORDINO LA SCHIERA
588500020603     C                   SORTA     SKCRER
588600020605     C*
588700020605     C* SVUOTO L'OCCUR
588800020605DO  1C     1             DO        500           J
588900020605     C     J             OCCUR     CRER
589000020605     C                   CLEAR                   REX                            *REGIONE-REGIONE
589100020605     C                   CLEAR                   REC                            *COLLI
589200020605     C                   CLEAR                   REP                            *PESO
589300020605     C                   CLEAR                   RER                            *RICAVI
589400020605     C                   CLEAR                   RES                            *SPEDIZIONI
589500020605     C                   CLEAR                   REV                            *VOLUME
589600020605E   1C                   ENDDO
589700020605     C*
589800020605     C                   Z-ADD     *ZEROS        YYY               4 0
589900020603     C*
590000020603     C* PASSO LA SCHIERA NELL'OCCUR
590100020604DO  1C     1             DO        500           YY
590200020605     C                   IF        %subst(SKCRER(YY):1:6) <> *BLANKS
590300020605     C                   ADD       1             YYY
590400020605     C     YYY           OCCUR     CRER
590500020605     C                   EVAL      CRER = SKCRER(YY)
590600020605     C                   ENDIF
590700020603E   1C                   ENDDO
590800020603     C*
590900020603     C                   ENDSR
591000000810     C*------------------------------------------------------------------------*
591100000810     C*?FINSR- OPERAZIONI FINALI
591200000810     C*------------------------------------------------------------------------*
591300000810     C     FINSR         BEGSR
591400000810     C*
591500000810     C* CHIUDE I FILE DEI PGM CHIAMATI
591600000810     C                   CLEAR                   BS69DS
591700000810     C                   CLEAR                   ACODS
591800000810     C                   CLEAR                   INDDS
591900000810     C                   CLEAR                   CLPDS
592000000810     C                   CLEAR                   CLSDS
592100000810     C                   MOVEL     'C'           I69TLA
592200000810     C                   MOVEL     KNSIF         I69SIF
592300000810     C                   CALL      'TIBS69R'
592400000810     C                   PARM                    BS69DS
592500000810     C                   PARM                    ACODS
592600000810     C                   PARM                    INDDS
592700000810     C                   PARM                    CLPDS
592800000810     C                   PARM                    CLSDS
592900000810     C*
593000000810     C                   ENDSR
593100000000     C*------------------------------------------------------------------------*
593200000000     C*?*INZSR - IMPOSTAZIONI INIZIALI
593300000000     C*------------------------------------------------------------------------*
593400000000     C     *INZSR        BEGSR
593500000000     C*
593600000000     C* PARAMETRI IN INPUT AL PGM
593700000000     C     *ENTRY        PLIST
593800000000     C                   PARM                    KPJBA
593900000000     C                   MOVEL     KPJBU         DS60
594000000000     C*
594100000000     C* DEFINIZIONE VARIABILI DELL KLIST
594200000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
594300000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
594400000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
594500990609     C     *LIKE         DEFINE    SDOKSC        KSOKSC                         *SISDO00F
594600000000     C     *LIKE         DEFINE    SDOANN        KSOANN
594700990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC00F
594800000000     C     *LIKE         DEFINE    SDCANN        KSCANN
594900000000     C     *LIKE         DEFINE    SDCMES        KSCMES
595000990609     C     *LIKE         DEFINE    SDAKSC        KSAKSC                         *SISDA00F
595100000000     C     *LIKE         DEFINE    SDAANN        KSAANN
595200000000     C     *LIKE         DEFINE    SDAMES        KSAMES
595300990609     C     *LIKE         DEFINE    SDPKSC        KSPKSC                         *SISDP00F
595400000000     C     *LIKE         DEFINE    SDPANN        KSPANN
595500000000     C     *LIKE         DEFINE    SDPMES        KSPMES
595600050118     C     *LIKE         DEFINE    PESKSC        KPEKSC                         *CLIENTE
595700050118     C     *LIKE         DEFINE    PESANN        KPEANN                         *ANNO
595800050118     C     *LIKE         DEFINE    PESMES        KPEMES                         *MESE
595900050118     C     *LIKE         DEFINE    PESMES        $MES                           *MESE
596000050118     C     *LIKE         DEFINE    VLMKSC        KVLKSC                         *CLIENTE
596100050118     C     *LIKE         DEFINE    VLMANN        KVLANN                         *ANNO
596200050118     C     *LIKE         DEFINE    VLMMES        KVLMES                         *MESE
596300040317     C     *LIKE         DEFINE    SDEFILE       KSEFILE                        *SISDO00F
596400040317     C     *LIKE         DEFINE    SDETRD        KSETRD                         *SISDO00F
596500040318     C     *LIKE         DEFINE    sdetrd        wtrd
596600040317     C     *LIKE         DEFINE    SDEKSC        KSEKSC                         *SISDO00F
596700040317     C     *LIKE         DEFINE    SDEANN        KSEANN
596800040317     C     *LIKE         DEFINE    SDEMES        KSEMES
596900000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
597000020823     C     *LIKE         DEFINE    D55TFP        DEPTFP
597100050217     C     *LIKE         DEFINE    ACOabl        KscABL
597200000810     C*
597300000810     C* DEFINIZIONE VARIABILI DI LAVORO
597400000810     C     *LIKE         DEFINE    ACOKSC        WKSC
597500000810     C     *LIKE         DEFINE    ACORAG        WDKSC
597600000911     C                   MOVEL     *BLANKS       WEDC              1
597700000000     C*
597800000000     C* DEFINIZIONE VARIABILI DI DEPOSITO
597900000000     C     *LIKE         DEFINE    SDCKSC        DEPKSC                         *CODICE CLIENTE
598000020523     C     *LIKE         DEFINE    SDCKSC        WRKKSC                         *CODICE CLIENTE DI W
598100020524     C     *LIKE         DEFINE    SDCKSC        PRIKSC                         *1° CODICE CLIENTE
598200000000     C*
598300000000     C* DEFINIZIONE VARIABILI DI PASSAGGIO PER IL CONTROLLO RECORD
598400000000     C                   MOVEL     *BLANKS       $SDX              3            *FILE DA CONTROLLARE
598500000000     C                   MOVEL     *BLANKS       $ROK              1            *RECORD VALIDO O NON
598600990601     C                   Z-ADD     *ZEROS        $DAT              6 0          *DATA (AAAA/MM)
598700000000     C     *LIKE         DEFINE    SDCTSP        $TSP                           *TIPO SERVIZIO
598800000000     C     *LIKE         DEFINE    SDCTPO        $TPO                           *TIPO PORTO
598900000000     C     *LIKE         DEFINE    D60CT1        $CTR                           *TARIFFA (ALFANUMER.
599000000000     C     *LIKE         DEFINE    SDCFPE        $FPE                           *FASCIA DI PESO
599100000000     C     *LIKE         DEFINE    SDCRBL        $RBL                           *RAGGRUPPAMENTO BOLL
599200000000     C     *LIKE         DEFINE    SDCIRR        DELIRR                         RETTIFICHE
599300000000     C     *LIKE         DEFINE    SDCINC        DELINC                         NOTE ACCREDITO
599400020905     C     *LIKE         DEFINE    Kpjbu         DEPKPJBU                       *depositi
599500000000     C*
599600000000     C* DEFINIZIONE NUMERO ELEMENTI
599700950725     C                   Z-ADD     *ZEROS        J                 4 0          *INDICE
599800000000     C                   Z-ADD     *ZEROS        JJ                4 0          *INDICE
599900000000     C                   Z-ADD     *ZEROS        JI                4 0          *INDICE
600000020604     C                   Z-ADD     *ZEROS        YY                4 0          *INDICE
600100000000     C                   Z-ADD     *ZEROS        MM                2 0          *INDICE MESE
600200020520     C                   Z-ADD     *ZEROS        R                 3 0          *INDICE COMB REGIONI
600300020520     C                   Z-ADD     *ZEROS        COMBIN            3 0          *COMBINAZIONI RAGGRU
600400000000     C                   Z-ADD     *ZEROS        I                 3 0          *DESCRIZ RAGGRUPP.TO
600500000000     C                   Z-ADD     *ZEROS        X                 2 0          *MESE DA STAMPARE
600600000000     C                   Z-ADD     *ZEROS        P                 4 0          *INDICE PARTICOLARIT
600700950725     C                   Z-ADD     *ZEROS        C                 4 0          *CLIENTI DA ELABORAR
600800050118     C                   Z-ADD     *ZEROS        nrPEV             4 0          *INDICE
600900050118      *
601000960604     C                   MOVEL     '1'           *IN01                          *OVERFLOW
601100000000     C     *LIKE         DEFINE    J             SPARM                          *N° MESI SELEZIONATI
601200000000     C     *LIKE         DEFINE    J             SPARS                          *N° SERVIZI SELEZ.TI
601300000000     C     *LIKE         DEFINE    J             SPARF                          *N° FASCE SELEZIONAT
601400000000     C     *LIKE         DEFINE    J             PARS                           *N° SERVIZI ESISTENT
601500000000     C     *LIKE         DEFINE    J             PARO                           *N° ORE ESISTENTI
601600000000     C     *LIKE         DEFINE    J             PARF                           *N° FASCE ESISTENTI
601700000000     C*
601800000000     C* DEFINIZIONE VALIDITA' RECORD
601900000000     C                   MOVEL     'N'           WRECOK            1            RECORD NO VALIDO
602000000000     C                   MOVEL     'N'           ROKREG            1            REC NON VALIDO STAT
602100050118     C                   MOVEL     'N'           ROKPEV            1            REC NON VALIDO STAT
602200000000     C                   MOVEL     'N'           ROKSER            1            REC NON VALIDO STAT
602300000000     C                   MOVEL     'N'           ROKAFF            1            REC NON VALIDO STAT
602400000000     C                   MOVEL     'N'           ROKDEL            1            REC NON VALIDO STAT
602500000000     C                   MOVEL     'N'           ROKAMM            1            REC NON VALIDO STAT
602600000000     C*
602700000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
602800000000     C     KEYTAB        KLIST
602900000000     C                   KFLD                    KTBUTE                         *UTENTE
603000000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
603100000000     C                   KFLD                    KTBKEY                         *RECORD TABELLA
603200000000     C*
603300000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
603400000000     C     KEYTA2        KLIST
603500000000     C                   KFLD                    KTBUTE
603600000000     C                   KFLD                    KTBCOD
603700000000     C*
603800990609     C* CHIAVE LETTURA SISDO01L - PARZIALE
603900000000     C     KEYSDO        KLIST
604000000000     C                   KFLD                    KSOKSC                         *CLIENTE
604100000000     C                   KFLD                    KSOANN                         *ANNO
604200000000     C*
604300990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
604400000000     C     KEYSDC        KLIST
604500000000     C                   KFLD                    KSCKSC                         *CLIENTE
604600000000     C                   KFLD                    KSCANN                         *ANNO
604700000000     C                   KFLD                    KSCMES                         *MESE
604800050118     C*
604900050118     C* CHIAVE LETTURA SIPES01L - PARZIALE
605000050118     C     KEYPES        KLIST
605100050118     C                   KFLD                    KPEKSC                         *CLIENTE
605200050118     C                   KFLD                    KPEANN                         *ANNO
605300050118     C                   KFLD                    KPEMES                         *MESE
605400050118     C*
605500050118     C* CHIAVE LETTURA SIVLM01L - PARZIALE
605600050118     C     KEYVLM        KLIST
605700050118     C                   KFLD                    KVLKSC                         *CLIENTE
605800050118     C                   KFLD                    KVLANN                         *ANNO
605900050118     C                   KFLD                    KVLMES                         *MESE
606000040317     C*
606100040317     C* CHIAVE LETTURA SISDE02L - TOTALE
606200040317     C     KEYSDE        KLIST
606300040317     C                   KFLD                    KSEFILE                        *DEPOSITO CLIENTE
606400040317     C                   KFLD                    KSETRD                         *DEPOSITO CLIENTE
606500040317     C                   KFLD                    KSEKSC                         *CLIENTE
606600040317     C                   KFLD                    KSEANN                         *ANNO
606700040317     C                   KFLD                    KSEMES                         *MESE
606800040317     C*
606900040317     C* CHIAVE LETTURA SISDE02L - PARZIALE
607000040317     C     KEYSDE1       KLIST
607100040317     C                   KFLD                    KSEFILE                        *DEPOSITO CLIENTE
607200040317     C                   KFLD                    KSETRD                         *DEPOSITO CLIENTE
607300040317     C                   KFLD                    KSEKSC                         *CLIENTE
607400000000     C*
607500990609     C* CHIAVE LETTURA SISDA01L - PARZIALE
607600000000     C     KEYSDA        KLIST
607700000000     C                   KFLD                    KSAKSC                         *CLIENTE
607800000000     C                   KFLD                    KSAANN                         *ANNO
607900000000     C                   KFLD                    KSAMES                         *MESE
608000000000     C*
608100990609     C* CHIAVE LETTURA SISDP01L - PARZIALE
608200000000     C     KEYSDP        KLIST
608300000000     C                   KFLD                    KSPKSC                         *CLIENTE
608400000000     C                   KFLD                    KSPANN                         *ANNO
608500000000     C                   KFLD                    KSPMES                         *MESE
608600000000     C*
608700000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
608800000000     C     KEYORG        KLIST
608900000000     C                   KFLD                    KORFIL                         *FILIALE
609000020523     C*
609100020523     C* CHIAVE LETTURA CNCLP00F - COMPLETA
609200020523     C     KEYCLP        KLIST
609300020523     C                   KFLD                    CLPKUT
609400020523     C                   KFLD                    CLPKCC
609500020523     C                   KFLD                    CLPKSC                         *MESE
609600000000     C*
609700000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
609800990609     C                   MOVEL     '0'           $FINO             1            *FINE CICLO SISDO
609900020524     C                   MOVEL     '0'           $FINREC           1            *FINE CICLO SISDO
610000990609     C                   MOVEL     '0'           $FINC             1            *FINE CICLO SISDC
610100040317     C                   MOVEL     '0'           $FINCE            1            *FINE CICLO SISDC
610200990609     C                   MOVEL     '0'           $FINA             1            *FINE CICLO SISDA
610300990609     C                   MOVEL     '0'           $FINP             1            *FINE CICLO SISDP
610400050118     C                   MOVEL     '0'           $FINPE            1            *FINE CICLO SIPES
610500050118     C                   MOVEL     '0'           $FINVL            1            *FINE CICLO SIVLM
610600000000     C*
610700000000     C* IMPOSTO A ZERO I PASSAGGI DI MEMORIZZAZIONE
610800000000     C                   MOVEL     '0'           PASKSC            1            SALDI CLIENTE
610900050118     C                   MOVEL     '0'           PASPEV            1            SALDI PESI-VOLUMI
611000000000     C                   MOVEL     '0'           PASREG            1            SALDI REG-REG
611100000000     C                   MOVEL     '0'           PASSER            1            SALDI SERVIZIO BOLLE
611200000000     C                   MOVEL     '0'           PASAFF            1            SALDI AFFIDABILITA'
611300000000     C                   MOVEL     '0'           PASDEL            1            SALDI DELTA CLIENTE
611400000000     C                   MOVEL     '0'           PASAMM            1            SALDI AMMINISTRAZION
611500050308      *  Cliente con dati
611600050308     C                   MOVEL     '0'           Con_Dati          1            Se contribuisce ai d
611700000000     C*
611800000000     C* IMPOSTO A ZERO I PASSAGGI DI MEMORIZZAZIONE
611900000000     C                   MOVEL     '0'           PASDGC            1            *STAMPA DATI GENER C
612000000000     C                   MOVEL     '0'           PASPAR            1            *STAMPA I PARAMENTRI
612100000000     C*
612200000000     C* IMPOSTO A *BLANKS I CAMPI DI SELEZIONE
612300000000     C                   MOVEL     '0'           SELSER            1            SELEZ SERVIZI (' ',T
612400000000     C                   MOVEL     '0'           SELPOR            1            SELEZ PORTO   (' ',T
612500000000     C                   MOVEL     '0'           SELPRO            1            SELEZ PRODOTTI(' ',T
612600000000     C                   MOVEL     '0'           SELCTR            1            SELEZ TARIFFE (' ',T
612700000000     C                   MOVEL     '0'           SELFPE            1            SELEZ FASCE   (' ',T
612800000000     C                   MOVEL     '0'           SOLAFF            1            SOLO STAT. "AFFID.TA
612900000000     C                   MOVEL     '0'           MAIAFF            1            MAI  STAT. "AFFID.TA
613000020823     C                   MOVEL     '0'           WSTINT            1            INTESTAZIONE DA STAM
613100020823     C                   Z-ADD     *ZEROS        DEPPAG            4 0          PAGINE PER CLIENTE -
613200000808     C*
613300000808     C* IMNPOSTA DATA / ORA CORRENTE
613400000808     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
613500000808     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
613600000808     C                   Z-ADD     *ZEROS        G08INV
613700000808     C                   MOVEL     '0'           G08ERR
613800000808     C                   CALL      'XSRDA8'
613900000808     C                   PARM                    WLBDA8
614000000808     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
614100000000     C*
614200000000     C* SUDDIVISIONE DATE VIDEO IN ANNI E MESI
614300000000     C                   Z-ADD     D60DA2        DATFIN                         *DS DATA FINALE
614400000000     C                   Z-ADD     D60DAT        DATINI                         *DS DATA INIZIALE
614500000000     C*
614600000000     C* CONTROLLO SE E' STATA SCELTA SOLO LA STATISTICA "AFFIDABILITA" PER
614700000000     C* SALTARE IL CICLO DI LETTURA CHE A QUESTA STATISTICA NON INTERESSA
614800000000IF  1C     D60ST1        IFEQ      'N'                                          *NO "REGIONE-REGIONE
614900050118     C     D60STp        ANDEQ     'N'                                          *NO "PESI/VOLUMI"
615000000000     C     D60ST2        ANDEQ     'N'                                          *NO "SERVIZIO BOLLE"
615100000000     C     D60ST3        ANDEQ     'S'                                          *SI "AFFIDABILITA'"
615200000000     C     D60ST4        ANDEQ     'N'                                          *NO "DELTA CLIENTE"
615300000000     C     D60ST5        ANDEQ     'N'                                          *NO "AMMINISTRAZIONE
615400000000     C                   MOVEL     '1'           SOLAFF                         *SOLO STAT "AFFID.TA
615500000000E   1C                   ENDIF
615600000000     C*
615700000000     C* CONTROLLO SE NON E' MAI STATA SCELTA LA STATISTICA "AFFIDABILITA" PER
615800000000     C* SALTARE IL CICLO DI LETTURA CHE ALLE ALTRE STATISTICHE NON INTERESSA
615900000000IF  1C     D60ST3        IFEQ      'N'
616000000000     C                   MOVEL     '1'           MAIAFF                         *MAI LA "AFFID.TA'"
616100000000E   1C                   ENDIF
616200020916     C*
616300020916     C* REPERISCE I DATI UTENTE
616400020916     C                   EXSR      REPDATIUTE
616500000000     C*
616600000000     C* CARICA TABELLE (FASCE, REGIONI, TIPI SERVIZIO)
616700000000     C                   EXSR      CARTAB
616800000000     C*
616900000000     C* CALCOLA I MESI/ANNI DA ELABORARE DALLE DUE DATE PASSATE
617000000000     C                   EXSR      CARMES
617100000000     C*
617200000000     C* DECODIFICA INTESTAZIONE
617300000000     C                   EXSR      DECTES
617400000000     C*
617500000000     C                   ENDSR
617600071211      *------------------------------------------------------------------------*
617700071211      *  Controlla se deve impostare la pagina successiva con Intestazione
617800071211      *------------------------------------------------------------------------*
617900071211     C     solo_x_STT    Begsr
618000071211      *
618100071211      *  Solo x Ottico
618200071211      *    deve controllare il salto pagina che sia corretto e stampare il
618300071211      *    numero pagina ad ogni salto pag. per poter archiviare
618400071211      *    correttamente la stampa su Ottico.
618500071211IF  1C                   IF        D60OP1 <> 'STT'                              *NO SE PER OTTICO
618600071211E   1C                   LeaveSR
618700071211E   1C                   ENDIF
618800071211      *
618900071211      * quindi controlla a che punto della pagina si trovi.... ossia se stampando
619000071211      *  il nuovo record supero l'Overflow ..... in Tal caso emette il salto
619100071211      *  pagina con intestazione e nr.Pagina -> essenziale per l'archiviazione su Ottico
619200071211     c                   if        Riga_Corrente + Righe_da_stampare >
619300071211     c                                    Riga_di_Overflow
619400071211     c                   eval      *in01 = *On
619500071211     c                   exsr      STATES
619600071211     c                   endIF
619700071211      *
619800071211      *
619900071211     C                   ENDSR
620000071211     O*------------------------------------------------------------------------*
620100950206**   DMM - DECODIFICHE MESI DELL'ANNO
620200000000GENNAIO                        1
620300000000FEBBRAIO                       2
620400000000MARZO                          3
620500000000APRILE                         4
620600000000MAGGIO                         5
620700000000GIUGNO                         6
620800000000LUGLIO                         7
620900000000AGOSTO                         8
621000000000SETTEMBRE                      9
621100000000OTTOBRE                        10
621200000000NOVEMBRE                       11
621300000000DICEMBRE                       12
621400000000**   DRI - DECODIFICHE RIGA DI STAMPA
621500000000   REGIONE - REGIONE   1
621600000000      T O T A L I      2
621700000000     AFFIDABILITA'     3
621800000000       D E L T A       4
621900000000    AMMINISTRAZIONE    5
622000050121 PESI/VOLUMI    V D L  6
622100000000**   DCA - DECODIFICHE CAMPI DA STAMPARE
622200000000SPEDIZIONI 1
622300000000COLLI      2
622400000000PESO (KG)  3
622500000000VOLUME     4
622600000000RICAVO     5
622700000000TOTALE     6
622800000000RICAVI     7
622900000000SPEDIZ     8
623000000000DELTA      9
623100000000TOTALI     10
623200091214C E+H D  T 11
62330000000012
62340000000013
62350000000014
62360000000015
623700000000**   DME - DECODIFICHE RIGHE IN STAMPA
623800000000MEDIA COLLI    X SPED  1
623900000000      KG       X SPED  2
624000000000      M.CUBI   X SPED  3
624100000000MEDIA KG       X COLLO 4
624200000000      M.CUBI   X COLLO 5
624300000000MEDIA INCASSO  X SPED  6
624400000000               X COLLO 7
624500000000               X Q.LE  8
624600000000               X M.CUBO9
624700000000FATTURATO TOTALE       10
624800000000NOTE DI ACCREDITO      11
624900000000RETTIFICHE FATTURAZIONE12
625000000000% SUL FATTURATO        13
625100000000GIACENZE           RIC.14
625200000000                   SPED15
625300000000DI CUI:                16
625400000000CONS AI PIANI      RIC.17
625500000000CONS APPUNTAMENTO  RIC.18
625600000000CONS SUPERMERCATI  RIC.19
625700000000CONS FUORI MISURA  RIC.20
625800000000ASSICURAZIONI      RIC.21
625900000000CONTRASSEGNI       RIC.22
626000010920CONS CENTRO STOR.  RIC.23
626100010920CONS LOC.DISAGIATE RIC.24
626200010920CONS DDT           RIC.25
626300950206C O L L I -------------26
626400950206P E S O ---------------27
626500950206R I C A V I -----------28
626600950206S P E D I Z I O N I ---29
626700950206V O L U M E -----------30
626800010920RITIRI             RIC.31
626900010920CONS ISOLE         RIC.32
627000021028CONS DISAGIATA     RIC.33
627100021028RITIRI ANNULLATI   RIC.34
627200021028RICERCA D'ARCHIVIO RIC.35
627300021028DIRITTO PESATURA   RIC.36
627400051018NR.LASCIATI AVVISI     37
627500000811CLIENTE                38
627600000811PIU' CLIENTI INSIEME   39
627700000811CLIENTE UNIFICANTE     40
627800020823TUTTI CLIENTI          41
627900020823TUTTI CLIENTI X UNIFIC 42
628000040318COLLI ORIG.BANCALI "O" 43
628100040318BANCALI TIPO "O"       44
628200040318BANCALI TIPO "B"       45
628300040924P.O.D. IMMAGE      RIC.46
628400051018ANN. P.ASSEGNATO   RIC.47
628500051018DIROTTAMENTI       RIC.48
628600051018RITIRO BANCALI     RIC.49
628700051018REC.C/ASS          RIC.50
628800051018ORM COMMISSIONATO  RIC.51
628900050118NR.COLLI VDL           52
629000050118NR.COLLI Boll.         53
629100050121% COLLI RILEV.         54
629200050121% SCOST RIL./BOLL.     55
629300050119   PESO VDL            56
629400050119   PESO BOLL.          57
629500050119   VOLUME VDL          58
629600050119   VOLUME BOLL.        59
629700050121------------- PESI VDL 60
629800050121----------- VOLUMI VDL 61
629900051018L.AVV.ADDEBITATI   RIC.62
