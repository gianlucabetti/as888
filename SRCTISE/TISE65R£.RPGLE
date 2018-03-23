000100000000     H*------------------------------------------------------------------------*
000200020520     H*                VISUALIZZAZIONE STATISTICHE CLIENTI                     *
000300000000     H*------------------------------------------------------------------------*
000400000000     H* INPUT  : -------- - ----------------------------------------------------
000500010920     H*          TISE60DS - D60OP0 = 'VIS' - LIVELLO PROCEDURA
000600000818     H*                     D60OP1 = 'F06' - LIVELLO PROGRAMMA
000700000000     H* OUTPUT : -------- - ----------------------------------------------------
000800010920     H*          TISE65D  - SFILE VIDEO DELLE STATISTICHE RICHIESTE
000900000000     H*------------------------------------------------------------------------*
001000000000     H DECEDIT('0,') DATEDIT(*DMY.)
001100000000     F*------------------------------------------------------------------------*
001200000000     F* DATA BASE
001300000000     F*------------------------------------------------------------------------*
001400010920     FTISE65D   CF   E             WORKSTN INFDS(DEVDS1)
001500000000     F                                     SFILE(VSFL:SFLULT)
001600990609     FSISDC01L  IF   E           K DISK
001700990609     FSISDA01L  IF   E           K DISK
001800990609     FSISDP01L  IF   E           K DISK
001900040318     FSISDE02L  IF   E           K DISK
002000050110     Fsipes01l  if   e           k disk
002100050110     Fsivlm01l  if   e           k disk
002200000000     FAZORG01L  IF   F 5000     2PIDISK    KEYLOC(4)
002300000000     FTABEL00F  IF   E           K DISK
002400000000     D*------------------------------------------------------------------------*
002500000000     D* SCHIERE
002600000000     D*------------------------------------------------------------------------*
002700000000     D*-------------------
002800000000     D* SCHIERE PER MESSAGGI, OPZIONI E FUNZIONI
002900000000     D*-------------------
003000000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)             *MESSAGGI
003100000000     D*                   CMD     1   5 80               *COMANDI
003200000000     D FUN1            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ.VIDEO PARAMETR
003300000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
003400000000     D*-------------------
003500000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
003600000000     D*-------------------
003700000000     D FFU             S              6    DIM(31)                              *FUNZIONE
003800000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
003900000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
004000000000     D*-------------------
004100000000     D* TABELLA DESCRIZIONI A VIDEO
004200000000     D*-------------------
004300000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
004400000000     D*-------------------
004500000000     D* TABELLA TASTI FUNZIONALI PREMUTI
004600000000     D*-------------------
004700000000     D KEYS            S              1    DIM(40)
004800950726     D*-------------------
004900950726     D* TABELLE CAPICONTO
005000950726     D*-------------------
005100000000     D*-------------------
005200000000     D* DECODIFICHE MESI
005300000000     D*-------------------
005400000000     D DMM             S              9    DIM(12) CTDATA PERRCD(1)
005500000000     D*-------------------
005600000000     D* DESCRIZIONE STATISTICHE
005700000000     D*-------------------
005800050110     D DRI             S             23    DIM(6) CTDATA PERRCD(1)
005900000000     D*-------------------
006000000000     D* DECODIFICHE RIGHE IN STAMPA
006100000000     D*-------------------
006200000000     D DCA             S             11    DIM(15) CTDATA PERRCD(1)
006300050118     D DME             S             23    DIM(50) CTDATA PERRCD(1)
006400950726     D DSTA            S             28    DIM(10) CTDATA PERRCD(1)
006500050119     D DPEV            S             23    DIM(8) CTDATA PERRCD(1)
006600000000     D*-------------------
006700000000     D* FASCE DI PESO
006800000000     D*-------------------
006900000000     D FPE             S              2    DIM(11)                              *CODICI FASCE ESISTE
007000000000     D DFPE            S             14    DIM(11)                              *DECODIFICA FASCE
007100000000     D SFPE            S              2    DIM(11)                              *FASCE SELEZIONATE
007200000000     D*-------------------
007300000000     D* FILIALI/RAGGRUPPAMENTI REGIONE E DECODIFICHE
007400000000     D*-------------------
007500971124     D FIL             S              3  0 DIM(500)                             *CODICE FILIALE
007600971124     D CRR             S              3  0 DIM(500)                             *RAGGRUPPAMENTO REGI
007700971124     D DCRR            S             25    DIM(500)                             *DECODIFICA RAGGRUPP
007800000000     D*-------------------
007900000000     D* COMBINAZIONE RAGGRUPPAMENTI PARTENZA/ARRIVO
008000000000     D*-------------------
008100020520     D REG             S              7    DIM(500)                             *P/A + CODICE + CODI
008200020520     D RE2             S              6    DIM(500)                             *CODICE + CODICE
008300950206     D*-------------------
008400950206     D* TIPI VALORI IN STAMPA PER REGIONE-REGIONE
008500950206     D*-------------------
008600950206     D VRR             S              1    DIM(10)                              *TIPI VALORE
008700950206     D DVRR            S             23    DIM(10)                              *DESCRIZIONE VALORI
008800000000     D*-------------------
008900000000     D* TIPI SERVIZIO
009000000000     D*-------------------
009100000000     D SER             S              1    DIM(10)                              *TIPI SERVIZIO
009200000000     D DSER            S             20    DIM(10)                              *DECODIFA SERVIZI
009300000000     D SERS            S              1    DIM(10)                              *SERVIZI SELEZIONATI
009400000000     D*------------------
009500000000     D* FLAG SPEDIZIONI/RICAVI PER TIPO BOLLA (C/N/R/S)
009600000000     D*------------------
009700000000     D FLAB            S              1    DIM(10)                              *FLAG TIPO BOLLA
009800000000     D FSP             S              1    DIM(10)                              *FLAG SPEDIZIONI (S/
009900000000     D FRI             S              1    DIM(10)                              *FLAG RICAVI (S/N)
010000000000     D*-------------------
010100000000     D* ORE DI CONSEGNA
010200000000     D*-------------------
010300000000     D ORE             S              3  0 DIM(7)                               *ORE DI CONSEGNA
010400000000     D DORE            S             20    DIM(7)                               *DECODIFICA ORE
010500000000     D*-------------------
010600000000     D* VARIE
010700000000     D*-------------------
010800050118     D*VAR****         S              1    DIM(40)                              *SIGLA VARIA
010900050118     D VAR             S              1    DIM(50)                              *SIGLA VARIA
011000950726     D*-------------------
011100950726     D* DS SCOMPATTA CLIENTI
011200950726     D*-------------------
011300050117      ** i codici clienti da passare nella DS sono diventati 28 da 30 che erano
011400050117      ** x permettere passaggio di altri parametri poichè la DS era già di 256
011500050117     D*KSA*******      S              4    DIM(30)                              *ALFANUMERICO
011600050117     D KSA             S              4    DIM(28)                              *ALFANUMERICO
011700000821     D*-------------------
011800000821     D* DS CLIENTI FIGLI
011900000821     D*-------------------
012000000821     D SKSC8           S              8  0 DIM(500)                             *clienti
012100000821     D*-------------------
012200000821     D* CLIENTI DA ELABORARE
012300000821     D*-------------------
012400000821     D CLI             S              7  0 DIM(500)
012500000821     D OCLI            S              7  0 DIM(500)                             *DA ORDINARE
012600000821     D*------------------
012700000821     D* DS REPERIMENTO CLIENTE
012800000821     D*-------------------
012900000821     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
013000000821     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
013100000821     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
013200000821     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
013300000821     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
013400020605     D*-------------------
013500020605     D* SCHIERA X ORDINAMENTO OCCUR "REGIONE-REGIONE"
013600020605     D*-------------------
013700020605     D SKCRER          S                   DIM(500) LIKE(CRER)
013800000000    ?D*-------------------
013900000000    ?D* S C H I E R E   M U L T I P L E   D I   M E M O R I Z Z A Z I O N E
014000000000    ?D*-------------------
014100000000     D*-------------------
014200020520     D* STATISTICA REGIONE-REGIONE                       - 500 RAGGRUPPAMENTI -
014300000000     D*-------------------
014400050110     D*-------------------
014500050110     D* STATISTICA PESI/VOLUMI                           - ???????? -
014600050110     D*-------------------
014700000000     D*-------------------
014800000000     D* STATISTICA SERVIZIO BOLLE (S,C,P,V,R)            - 5 VALORI -
014900000000     D*-------------------
015000000000     D*-------------------
015100000000     D* STATISTICA AFFIDABILITA' SPEDIZIONI              - 7 ORARI -
015200000000     D*-------------------
015300000000     D*-------------------
015400000000     D* STATISTICA DELTA PER CLIENTE                     - 11 FASCE -
015500000000     D*-------------------
015600000000     D*-------------------
015700050118     D* SCHIERE STATISTICA AMMINISTRAZIONE               - 50 VARIE -
015800000000     D*-------------------
015900000000     D*---
016000000000     D* SCHIERE NON MULTIPLE
016100000000     D*---
016200010920     D AFC             S             18  3 DIM(2)                               FATTURATO
016300010920     D ANC             S             18  3 DIM(2)                               NOTE DI CREDITO
016400010920     D ARC             S             18  3 DIM(2)                               RETTIFICHE DI FATTUR
016500000000    ?D*-------------------
016600000000    ?D* S C H I E R E   D I   L A V O R O
016700000000    ?D*-------------------
016800000000     D*-------------------
016900000000     D* MULTIPLA PER LA "SERVIZIO BOLLE"                 - 5 VALORI -
017000000000     D*-------------------
017100000000     D*-------------------
017200000000     D* PER MEDIE PER LA "SERVIZIO BOLLE"
017300000000     D*-------------------
017400000000     D SPW             S             16  3 DIM(2)                               SPEDIZIONI
017500000000     D COW             S             16  3 DIM(2)                               COLLI
017600000000     D PEW             S             16  3 DIM(2)                               PESO
017700000000     D VOW             S             16  3 DIM(2)                               VOLUME
017800000000     D RIW             S             16  3 DIM(2)                               RICAVI
017900000000     D MEW             S             16  3 DIM(2)                               MEDIE CALCOLATE
018000000000     D*-------------------
018100000000     D* TOTALI PER LA "AFFIDABILITA'"
018200000000     D*-------------------
018300000000     D PNW             S             13  0 DIM(2)                               SPED CORRIERE DELL'O
018400000000     D PRW             S             13  0 DIM(2)                               SPED DISTRIBUZ DELL'
018500000000     D PSW             S             13  0 DIM(2)                               SPED ESPRESSO DELL'O
018600000629     D PPW             S             13  0 DIM(2)                               SPED POSTE DELL'ORA
018700000000     D PTW             S             13  0 DIM(2)                               SPEDIZIONI TOTALI
018800000000     D*-------------------
018900000000     D* TOTALI A FONDO PAGINA PER LA "DELTA CLIENTE"
019000000000     D*-------------------
019100010920     D DRW             S             18  3 DIM(2)                               RICAVI REALI
019200010920     D DPW             S             18  3 DIM(2)                               RICAVI PRESUNTI
019300000000     D DSW             S             13  0 DIM(2)                               SPEDIZIONI
019400000000     D DDW             S              4  1 DIM(2)                               DELTA PERCENTUALE
019500000000     D*-------------------
019600000000     D* PERCENTULAI FATTURATO PER LA "AMMINISTRAZIONE"
019700000000     D*-------------------
019800000000     D APW             S              4  1 DIM(2)                               PERCEN SUL FATTURATO
019900010920     D $VA             S             18  3 DIM(2)                               VALORE GENERICO
020000010920     D $V2             S             18  3 DIM(2)                               VALORE GENERICO 2
020100000000     D $P1             S              2  0 DIM(2)                               INCIDENZA SPED CORRI
020200000000     D $P2             S              2  0 DIM(2)                               INCIDENZA SPED DISTR
020300000000     D $P3             S              2  0 DIM(2)                               INCIDENZA SPED ESPRE
020400000629     D $P4             S              2  0 DIM(2)                               INCIDENZA SPED POSTE
020500000629     D $P5             S              2  0 DIM(2)                               INCIDENZA SPED TOT O
020600000000    ?D*-------------------
020700000000    ?D* S C H I E R E   V I D E O
020800000000    ?D*-------------------
020900020605     D $DS             S             23    DIM(1500)                             *TOTALI
021000020605     D $VS             S             18  3 DIM(1500)
021100020605     D $TS             S             18  3 DIM(1500)
021200020605     D $ES             S              1  0 DIM(1500)
021300000000     D*---
021400050113     D $Dp             S             23    DIM(1500)                             *PESI VOLUMI
021500050113     D $Vp             S             18  3 DIM(1500)
021600050113     D $Tp             S             18  3 DIM(1500)
021700050113     D $Ep             S              1  0 DIM(1500)
021800050111     D*---
021900020605     D $DR             S             23    DIM(1500)                             *REGIONE-REGIONE
022000020605     D $VR             S             18  3 DIM(1500)
022100020605     D $TR             S             18  3 DIM(1500)
022200020605     D $ER             S              1  0 DIM(1500)
022300000000     D*---
022400020605     D $DD             S             23    DIM(100)                             *DELTA
022500020605     D $VD             S             18  3 DIM(100)
022600020520     D $TD             S             18  3 DIM(100)
022700020520     D $ED             S              1  0 DIM(100)
022800000000     D*---
022900950726     D $DM             S             23    DIM(100)                             *AMMINISTRAZIONE
023000010920     D $VM             S             18  3 DIM(100)
023100010920     D $TM             S             18  3 DIM(100)
023200950726     D $EM             S              1  0 DIM(100)
023300000000     D*---
023400000000     D* GESTIONE PARTICOLARE
023500000000     D*---
023600950726     D $DA             S             23    DIM(100)                             *AFFIDABILITA'
023700950726     D $PA             S             22    DIM(100)
023800950726     D $TA             S             22    DIM(100)
023900000000     D*---
024000000000     D* GENERICHE DI LAVORO
024100000000     D*---
024200020605     D $DW             S             23    DIM(1500)                             *X TUTTE LE STATISTI
024300020605     D $EW             S              1  0 DIM(1500)
024400020605     D $VW             S             18  3 DIM(1500)                             STATISTICHE <> AFFID
024500020605     D $TW             S             18  3 DIM(1500)
024600000000     D*-------------------
024700000000     D* EDITAZIONE CAMPI ALFANUMERICI
024800000000     D*-------------------
024900000000     D $N$             S              1    DIM(16)                              *NUMERO DA EDITARE
025000000000     D $A$             S              1    DIM(22)                              *NUMERO EDITATO
025100000000     D UNIORG        E DS          5000    EXTNAME(AZORG00F)
025200000000     D*-------------------
025300000000     D* RIDEFINIZIONE SCHIERA -FUN- (FUNZIONI)
025400000000     D*-------------------
025500000000     D DSFUN         E DS
025600000000     D  FUNRIG                 1     40
025700000000     D*-------------------
025800000000     D* DS PER GESTIONE TASTI FUNZIONALI
025900000000     D*-------------------
026000000000     D STDKDS        E DS
026100000000     D  KEYRIG                 1     40
026200000000     D*-------------------
026300000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
026400000000     D*-------------------
026500000000     D                SDS
026600000000     D*-------------------
026700000000     D* NOME UTENTE
026800000000     D*-------------------
026900000000     D  $PGM                   1     10
027000000000     D  $USER                254    263
027100000000     D*-------------------
027200000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
027300000000     D*-------------------
027400000000     D DEVDS1          DS
027500000000     D  KEY                  369    369
027600000000     D  FRTSFL               378    379B 0
027700000821     D*------------------
027800000821     D* DS CONTROLLO DATA (8)
027900000821     D*------------------
028000000821     D WLBDA8          DS                  INZ
028100000821     D  G08DAT                 1      8  0
028200000821     D  G08INV                 9     16  0
028300000821     D  G08ERR                17     17
028400000821     D  G08TGI                18     22  0
028500000000     D*-------------------
028600000000     D* ARCHITETTURA
028700000000     D*-------------------
028800000000     D KPJBA         E DS
028900000000     D*-------------------
029000000000     D* DS DI PROCEDURA
029100000000     D*-------------------
029200010920     D DS60          E DS                  EXTNAME(TISE60DS)
029300020916     D*------------
029400020916     D* DS REPERIMENTO DATI UTENTE
029500020916     D*------------
029600020916     D TIBS34DS      E DS                                                       *Profili utente
029700020916     D DDATIUTE      E DS                                                       *Dati utente
029800020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
029900000000     D*------------------
030000000000     D* DS SUDDIVISIONE DATA INIZIALE
030100000000     D*------------------
030200000000     D                 DS
030300990601     D  ANNINI                 1      4  0
030400990601     D  MESINI                 5      6  0
030500990601     D  DATINI                 1      6  0
030600000000     D*------------------
030700000000     D* DS SUDDIVISIONE DATA FINALE
030800000000     D*------------------
030900000000     D                 DS
031000990601     D  ANNFIN                 1      4  0
031100990601     D  MESFIN                 5      6  0
031200990601     D  DATFIN                 1      6  0
031300000000     D*------------------
031400990609     D* DS UNIONE DATA SISDC01L
031500000000     D*------------------
031600000000     D                 DS
031700990601     D  SDCANN                 1      4  0
031800990601     D  SDCMES                 5      6  0
031900990601     D  SDCDAT                 1      6  0
032000040318     D*------------------
032100040318     D* DS UNIONE DATA SISDE02L
032200040318     D*------------------
032300040318     D                 DS
032400040318     D  SDEANN                 1      4  0
032500040318     D  SDEMES                 5      6  0
032600040318     D  SDEDAT                 1      6  0
032700000000     D*------------------
032800990609     D* DS UNIONE DATA SISDA01L
032900000000     D*------------------
033000000000     D                 DS
033100990601     D  SDAANN                 1      4  0
033200990601     D  SDAMES                 5      6  0
033300990601     D  SDADAT                 1      6  0
033400000000     D*------------------
033500990609     D* DS UNIONE DATA SISDP01L
033600000000     D*------------------
033700000000     D                 DS
033800990601     D  SDPANN                 1      4  0
033900990601     D  SDPMES                 5      6  0
034000990601     D  SDPDAT                 1      6  0
034100050111     D*------------------
034200050111     D* DS UNIONE DATA SIPES01L
034300050111     D*------------------
034400050111     D                 DS
034500050111     D  pesANN                 1      4  0
034600050111     D  pesMES                 5      6  0
034700050111     D  pesDAT                 1      6  0
034800050111     D*------------------
034900050111     D* DS UNIONE DATA SIVLM01L
035000050111     D*------------------
035100050111     D                 DS
035200050111     D  vlmANN                 1      4  0
035300050111     D  vlmMES                 5      6  0
035400050111     D  vlmDAT                 1      6  0
035500000000     D*------------------
035600000000     D* DS SUDDIVISIONE PRIMA DESCRIZIONE CONTROL
035700000000     D*------------------
035800000000     D                 DS
035900000000     D  VCCSTA                 1     23
036000000000     D  VCCVAF                29     38
036100990602     D  VCCMMF                39     46
036200990602     D  VCCAAF                47     50
036300000000     D  VCCVAI                56     64
036400990602     D  VCCMMI                66     73
036500990602     D  VCCAAI                74     77
036600000000     D  VCCDES                 1     77
036700000000     D*------------------
036800000000     D* DS SUDDIVISIONE SECONDA DESCRIZIONE CONTROL
036900000000     D*------------------
037000000000     D                 DS
037100000000     D  VCCTVA                 1     23
037200000000     D  VCCDE2                 1     77
037300000000     D*------------------
037400000000     D* DEFINIZIONE COSTANTI CONTROL
037500000000     D*------------------
037600000000     D COSVAF          C                   CONST('Valori di')
037700000000     D COSVAI          C                   CONST('Totali da')
037800000000     D*------------------
037900000000     D* DS FASCE DI PESO
038000000000     D*------------------
038100000000     D DS2L          E DS
038200090610     D*------------------
038300090610     D* DS descrizioni tipi servizio
038400090610     D*------------------
038500090610     D DS5e          E DS
038600000000     D*------------------
038700000000     D* DS REGIONI
038800000000     D*------------------
038900000000     D DS14          E DS
039000000000     D*------------------
039100000000     D* DS RAGGRUPPAMENTI REGIONI
039200000000     D*------------------
039300000000     D DS2M          E DS
039400070627      *------------------
039500070627      *? DS varie da decodificare a video (abilitate) sulla tab.:"CA"  ?
039600070627      *------------------
039700070627     D DSCA          E DS
039800070627     D*---
039900070627     D SkVar           S              1a   DIM(200)
040000070627     D SkDsc           S             23a   DIM(200)
040100070627     D SkOrd           S              6s 0 DIM(200)
040200070627     D*------------------
040300070627     D* DS FLAG RICAVI/SPEDIZIONI TIPI BOLLA
040400070627     D*------------------
040500070627     D DS2W          E DS
040600040318     D*------------------
040700040318     D* DS TIPO RECORD "BNB" DI SISDE
040800040318     D*------------------
040900040318     D DAR5BNBSDE    E DS
041000040319     D*------------------
041100040319     D* DS TIPO RECORD "BAN" DI SISDE
041200040319     D*------------------
041300040319     D DAR5BANSDE    E DS
041400000821     D*------------------
041500000821     D* DS DI PROCEDURA
041600000821     D*------------------
041700000821     D BS10DS        E DS                  EXTNAME(TIBS10DS)
041800000821     D  SKSC11                21   5520  0 DIM(500)                             *CLIENTI FIGLI
041900950726     D*------------------
042000950726     D* DS SCOMPATTA CLIENTI
042100950726     D*------------------
042200950726     D                 DS
042300950726     D  DSKSN                  1      4P 0
042400950726     D  DSKSA                  1      4
042500000000     D*------------------
042600000000     D* DEFINIZIONE COSTANTI PER DECODIFICA ORE DI CONSEGNA (15)
042700000000     D*------------------
042800000000     D DORA1           C                   CONST('In Anticipo         ')
042900000000     D DORA2           C                   CONST('In Orario           ')
043000000000     D DORA3           C                   CONST('In Ritardo di 12 ore')
043100000000     D DORA4           C                   CONST('In Ritardo di 24 ore')
043200000000     D DORA5           C                   CONST('In Ritardo di 36 ore')
043300000000     D DORA6           C                   CONST('In Ritardo di 48 ore')
043400000000     D DORA7           C                   CONST('Oltre 48h di Ritardo')
043500000000    ?D*------------------
043600000000    ?D* S C H I E R E   M U L T I P L E   D I   M E M O R I Z Z A Z I O N E
043700000000    ?D*------------------
043800000000     D*------------------
043900000000     D* NUMERO DI BYTE IN PACKED
044000000000     D*------------------
044100000000     D* (16:2) + 1 = 9  /  (9 X 2) =   18   --> REGIONE-REGIONE/SERVIZIO/AMM.NE
044200000000     D* (11:2) + 1 = 6  /  (6 X 2) =   12   --> AFFIDABILITA' SPEDIZIONI/AMM.NE
044300000000     D* (13:2) + 1 = 7  /  (7 X 2) =   14   --> DELTA PER CLIENTE
044400000000     D*-----------------------------------
044500020520     D* "REGIONE-REGIONE"                 - 500 RAGGRUPPAMENTI X 2 (MESE+TOTALE)
044600000000     D*-----------------------------------
044700020520     D CRER            DS                  OCCURS(500) INZ
044800020605     D  REX                    1      6                                         COMBINAZIONE REGIONE P.- REGIONE A.
044900020605     D  REC                    7     24P 3                                      COLLI
045000950206     D                                     DIM(2)                               COLLI
045100950206     D                                     PACKEVEN
045200020605     D  REP                   25     42P 3                                      PESO
045300950206     D                                     DIM(2)                               PESO
045400950206     D                                     PACKEVEN
045500020605     D  RER                   43     60P 3                                      RICAVI
045600950206     D                                     DIM(2)                               RICAVI
045700950206     D                                     PACKEVEN
045800020605     D  RES                   61     78P 3                                      SPEDIZ
045900950206     D                                     DIM(2)                               SPEDIZIONI
046000950206     D                                     PACKEVEN
046100020605     D  REV                   79     96P 3                                      VOLUME
046200950206     D                                     DIM(2)                               VOLUME
046300950206     D                                     PACKEVEN
046400000000     D*-----------------------------------
046500000000     D* "TOTALI"                          - 5 VALORI X 2 (MESE+TOTALE)
046600000000     D*-----------------------------------
046700000000     D CSER            DS                  OCCURS(5) INZ
046800000000     D  SNC                    1     18P 3                                      CORRIE
046900000000     D                                     DIM(2)                               SERVIZIO CORRIERE
047000000000     D                                     PACKEVEN
047100000629     D  SDC                   19     36P 3                                      DISTRI
047200000629     D                                     DIM(2)                               SERVIZIO DISTRIBUZIO
047300000629     D                                     PACKEVEN
047400000000     D  SEC                   37     54P 3                                      ESPRES
047500000000     D                                     DIM(2)                               SERVIZIO ESPRESSO
047600000000     D                                     PACKEVEN
047700091013     D  SHC                   55     72P 3                                      H10:30
047800091214     D                                     DIM(2)                               SERVIZIO espresso H1030
047900000629     D                                     PACKEVEN
048000091013     D  SPC                   73     90P 3                                      POSTE
048100091013     D                                     DIM(2)                               SERVIZIO POSTE
048200091013     D                                     PACKEVEN
048300000000     D WSER            DS                  OCCURS(5) INZ
048400010920     D  VAW                          18P 3                                      VALORE
048500000000     D                                     DIM(2)                               TOTALE VALORE PERIOD
048600040318     D*-------------------
048700040318     D* DEFINIZIONE VARIABILI
048800040318     D*-------------------
048900040318     D COR             S              9  0 DIM(2)                               VALORE GENERICO
049000040319     D BAN             S              7  0 DIM(2)                               VALORE GENERICO
049100040319     D BNB             S              7  0 DIM(2)                               VALORE GENERICO
049200000000     D*------------------
049300000000     D* "AFFIDABILITA'"                   - 7 ORARI X 2 (MESE+TOTALE)
049400000000     D*------------------
049500000000     D CAFF            DS                  OCCURS(7) INZ
049600010920     D  CNC                          14P 0                                      CORRIE
049700000000     D                                     DIM(2)                               CONSEGNA CORRIERE
049800010920     D  CDC                          14P 0                                      DISTRI
049900000629     D                                     DIM(2)                               CONSEGNA DISTRIBUZIO
050000010920     D  CEC                          14P 0                                      ESPRES
050100000000     D                                     DIM(2)                               CONSEGNA ESPRESSO
050200010920     D  CPC                          14P 0                                      POSTE
050300000629     D                                     DIM(2)                               CONSEGNA POSTE
050400091214     D  CHC                          14P 0                                      ESPRES H1030
050500091214     D                                     DIM(2)                               CONSEGNA ESPRESSO 1030
050600050111     D*------------------
050700050111     D* "PESI/VOLUMI"
050800050111     D*------------------
050900050111     D CPEV            DS                  OCCURS(2) INZ
051000050111     D  NCV                           7P 0                                      nr.colli VDL
051100050111     D                                     DIM(2)                               nr.colli VDL
051200050112     D  NCB                           7P 0                                      nr.colli Bollettati
051300050111     D                                     DIM(2)                               nr.colli Bollettati
051400050111     D  PVV                          12P 3                                      peso/vol VDL
051500050111     D                                     DIM(2)                               peso/vol VDL
051600050111     D  PVB                          12P 3                                      peso/vol Bollettati
051700050111     D                                     DIM(2)                               peso/vol Bollettati
051800000000     D*------------------
051900000000     D* "DELTA"                           - 11 FASCE X 2 (MESE+TOALE)
052000000000     D*------------------
052100000000     D CDEL            DS                  OCCURS(11) INZ
052200010920     D  DRC                          18P 3                                      R REAL
052300000000     D                                     DIM(2)                               DELTA RICAVI REALI
052400010920     D  DPC                          18P 3                                      R PRES
052500000000     D                                     DIM(2)                               DELTA RICAVI PRESUNT
052600010920     D  DSC                          14P 0                                      SPEDIZ
052700000000     D                                     DIM(2)                               DELTA SPEDIZIONI
052800000000     D*------------------
052900050118     D* "AMMINISTRAZIONE "                - 50 VARIE X 2 (MESE+TOTALE)
053000000000     D*------------------
053100050118     D CAMM            DS                  OCCURS(50) INZ
053200010920     D  ARI                          18P 3                                      RICAVI
053300000000     D                                     DIM(2)                               RICAVI
053400010920     D  ASP                          14P 0                                      SPEDIZ
053500000000     D                                     DIM(2)                               SPEDIZIONI
053600970724     I*------------------------------------------------------------------------*
053700000000     I* INPUT
053800000000     I*------------------------------------------------------------------------*
053900000000     I*------------------
054000000000     I* RIDEFINIZIONE AZORG X FARE UNA SOLA MOVE E NON TANTE QUANTI SONO I CAMPI
054100000000     I*------------------
054200000000     IAZORG01L  AA
054300000000     C*------------------------------------------------------------------------*
054400000000     C*?MAIN LINE
054500000000     C*------------------------------------------------------------------------*
054600000000     C*
054700000000     C* CICLO FINO A CHE:
054800000000DO  1C     $FINE         DOUEQ     '1'
054900000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
055000000000     C*--------------------
055100000000     C* GESTIONE VIDEO "TOTALI"
055200000000     C*--------------------
055300000000IF  2C     $GES          IFEQ      'VS'
055400000000     C                   EXSR      GESVS
055500000000E   2C                   ENDIF
055600050110     C*--------------------
055700050110     C* GESTIONE VIDEO "PESI/VOLUMI"
055800050110     C*--------------------
055900050110IF  2C     $ERR          IFEQ      '0'
056000050110IF  3C     $GES          IFEQ      'PV'
056100050110     C                   EXSR      GESPV
056200050110E   3C                   ENDIF
056300050110E   2C                   ENDIF
056400000000     C*--------------------
056500000000     C* GESTIONE VIDEO "REGIONE-REGIONE"
056600000000     C*--------------------
056700000000IF  2C     $ERR          IFEQ      '0'
056800000000IF  3C     $GES          IFEQ      'VR'
056900000000     C                   EXSR      GESVR
057000000000E   3C                   ENDIF
057100000000E   2C                   ENDIF
057200000000     C*--------------------
057300000000     C* GESTIONE VIDEO "DELTA"
057400000000     C*--------------------
057500000000IF  2C     $ERR          IFEQ      '0'
057600000000IF  3C     $GES          IFEQ      'VD'
057700000000     C                   EXSR      GESVD
057800000000E   3C                   ENDIF
057900000000E   2C                   ENDIF
058000000000     C*--------------------
058100000000     C* GESTIONE VIDEO "AFFIDABILITA'"
058200000000     C*--------------------
058300000000IF  2C     $ERR          IFEQ      '0'
058400000000IF  3C     $GES          IFEQ      'VA'
058500000000     C                   EXSR      GESVA
058600000000E   3C                   ENDIF
058700000000E   2C                   ENDIF
058800000000     C*--------------------
058900000000     C* GESTIONE VIDEO "AMMINISTRAZIONE"
059000000000     C*--------------------
059100000000IF  2C     $ERR          IFEQ      '0'
059200000000IF  3C     $GES          IFEQ      'VM'
059300000000     C                   EXSR      GESVM
059400000000E   3C                   ENDIF
059500000000E   2C                   ENDIF
059600000000     C*
059700000000E   1C                   ENDDO
059800000000     C*
059900950726IF  1C     $F03          IFEQ      '1'                                          *SE PREMUTO F03
060000000000     C                   MOVEL     '1'           D60F03                         *LO IMPOSTO NELLA DS
060100000000E   1C                   ENDIF                                                  *DI PROCEDURA
060200000821     C*
060300000821     C* OPERAZIONI FINALI
060400000821     C                   EXSR      FINSR
060500000000     C*
060600000000     C                   MOVEL     DS60          KPJBU
060700000000     C                   SETON                                        LR
060800000000     C*------------------------------------------------------------------------*
060900000000     C*?CARVS - CARICA VIDEO STATISTICA TOTALI
061000000000     C*------------------------------------------------------------------------*
061100000000     C     CARVS         BEGSR
061200000000     C*
061300000000     C* SE LA STATISTICA NON E' STATA RICHIESTA
061400000000IF  1C     D60ST2        IFEQ      'N'
061500000000     C*
061600000000IF  2C     KEY           IFEQ      F12
061700000000     C                   MOVEL     '1'           $FINE                          *ESCE
061800000000E   2C                   ENDIF
061900000000IF  2C     KEY           IFEQ      ENTER
062000050110     C                   EXSR      CARPV                                        *CARICA VIDEO DOPO
062100000000E   2C                   ENDIF
062200000000     C*
062300000000     C* SE LA STATISTICA E' STATA RICHIESTA: GESTIONE NORMALE
062400000000X   1C                   ELSE
062500000000     C                   MOVEL     'VS'          $GES                           *GESTIONE ATTUALE
062600000000     C                   MOVEL     DRI(2)        VCCSTA                         *DECOD STATISTICA
062700000000     C                   MOVEL     *BLANKS       VCCTVA                         *NO DECOD VALORE
062800000000     C*
062900000000     C* PORTA SCHIERE MEMORIZZAZIONE IN SCHIERE LAVORO
063000000000     C                   EXSR      VSLAV                                        *TOTALI
063100000000     C*
063200000000     C                   MOVEA     FUN1          FUN                            *SCHIERA FUNZIONI
063300000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
063400000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
063500000000     C*
063600000000     C                   WRITE     VTESTA                                       *TESTATA
063700000000     C                   WRITE     VFUNZ                                        *FUNZIONI
063800000000     C*
063900000000     C                   EXSR      POSSFL                                       *CARICA SFL
064000000000     C*
064100000000E   1C                   ENDIF
064200000000     C*
064300000000     C                   ENDSR
064400000000     C*------------------------------------------------------------------------*
064500000000     C*?VSLAV - PORTA SCHIERE MEMORIZZAZIONE "TOTALI" IN SCHIERE LAVORO
064600000000     C*------------------------------------------------------------------------*
064700000000     C     VSLAV         BEGSR
064800000000     C*
064900000000     C                   MOVEL     $DS           $DW                            *DESCRIZIONE VALORE
065000000000     C                   Z-ADD     $VS           $VW                            *VALORE DEL MESE
065100000000     C                   Z-ADD     $TS           $TW                            *VALORE TOTALE
065200000000     C                   Z-ADD     $ES           $EW                            *NUMERO DECIMALI
065300000000     C*
065400000000     C                   ENDSR
065500050111     C*------------------------------------------------------------------------*
065600050111     C*?GESVS - GESTIONE VIDEO STATISTICA TOTALI
065700050111     C*------------------------------------------------------------------------*
065800050111     C     GESVS         BEGSR
065900050111     C*
066000050111IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
066100050111     C                   WRITE     VTESTA                                       *TESTATA
066200050111     C                   WRITE     VFUNZ                                        *FUNZIONI
066300050111E   1C                   ENDIF
066400050111     C*
066500050111     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
066600050111     C*
066700050111     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
066800050111     C*
066900050111IF  1C     $ERR          IFEQ      '0'
067000050111SL  2C                   SELECT
067100050111     C     KEY           WHENEQ    ENTER                                        *CARICA VIDEO DOPO
067200050111     C                   EXSR      CARPV
067300050111     C     KEY           WHENEQ    F12                                          *ESCE
067400050111     C                   MOVEL     '1'           $FINE
067500050111     C                   OTHER
067600050111     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
067700050111E   2C                   ENDSL
067800050111E   1C                   ENDIF
067900050111     C*
068000050111     C                   ENDSR
068100050110     C*------------------------------------------------------------------------*
068200050110     C*?CARPV - CARICA VIDEO STATISTICA PESI VOLUMI
068300050110     C*------------------------------------------------------------------------*
068400050110     C     CARPV         BEGSR
068500050110     C*
068600050110     C* SE LA STATISTICA NON E' STATA RICHIESTA
068700050110IF  1C     D60STp        IFEQ      'N'
068800050110     C*
068900050110IF  2C     KEY           IFEQ      F12
069000050110     C                   EXSR      CARVS                                        *CARICA VIDEO PRIMA
069100050110E   2C                   ENDIF
069200050110IF  2C     KEY           IFEQ      ENTER
069300050110     C                   EXSR      CARVR                                        *CARICA VIDEO DOPO
069400050110E   2C                   ENDIF
069500050110     C*
069600050110     C* SE LA STATISTICA E' STATA RICHIESTA: GESTIONE NORMALE
069700050110X   1C                   ELSE
069800050110     C                   MOVEL     'PV'          $GES                           *GESTIONE ATTUALE
069900050110     C                   MOVEL     DRI(6)        VCCSTA                         *DECOD STATISTICA
070000050110     C                   MOVEL     *BLANKS       VCCTVA                         *NO DECOD VALORE
070100050110     C*
070200050110     C* PORTA SCHIERE MEMORIZZAZIONE IN SCHIERE LAVORO
070300050110     C                   EXSR      PVLAV                                        *TOTALI
070400050110     C*
070500050110     C                   MOVEA     FUN1          FUN                            *SCHIERA FUNZIONI
070600050110     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
070700050110     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
070800050110     C*
070900050110     C                   WRITE     VTESTA                                       *TESTATA
071000050110     C                   WRITE     VFUNZ                                        *FUNZIONI
071100050110     C*
071200050110     C                   EXSR      POSSFL                                       *CARICA SFL
071300050110     C*
071400050110E   1C                   ENDIF
071500050110     C*
071600050110     C                   ENDSR
071700050110     C*------------------------------------------------------------------------*
071800050110     C*?PVLAV - PORTA SCHIERE MEMORIZZAZIONE "TOTALI" IN SCHIERE LAVORO
071900050110     C*------------------------------------------------------------------------*
072000050110     C     PVLAV         BEGSR
072100050110     C*
072200050111     C                   MOVEL     $Dp           $DW                            *DESCRIZIONE VALORE
072300050111     C                   Z-ADD     $Vp           $VW                            *VALORE DEL MESE
072400050111     C                   Z-ADD     $Tp           $TW                            *VALORE TOTALE
072500050111     C                   Z-ADD     $Ep           $EW                            *NUMERO DECIMALI
072600050110     C*
072700050110     C                   ENDSR
072800000000     C*------------------------------------------------------------------------*
072900050110     C*?GESPV - GESTIONE VIDEO STATISTICA PESI VOLUMI
073000000000     C*------------------------------------------------------------------------*
073100050110     C     GESPV         BEGSR
073200000000     C*
073300000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
073400000000     C                   WRITE     VTESTA                                       *TESTATA
073500000000     C                   WRITE     VFUNZ                                        *FUNZIONI
073600000000E   1C                   ENDIF
073700000000     C*
073800000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
073900000000     C*
074000000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
074100000000     C*
074200000000IF  1C     $ERR          IFEQ      '0'
074300000000SL  2C                   SELECT
074400000000     C     KEY           WHENEQ    ENTER                                        *CARICA VIDEO DOPO
074500000000     C                   EXSR      CARVR
074600050111     C     KEY           WHENEQ    F12                                          *CARICA VIDEO PRIMA
074700050111     C                   EXSR      CARVS
074800000000     C                   OTHER
074900000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
075000000000E   2C                   ENDSL
075100000000E   1C                   ENDIF
075200000000     C*
075300000000     C                   ENDSR
075400000000     C*------------------------------------------------------------------------*
075500000000     C*?CARVR - CARICA VIDEO STATISTICA REGIONE-REGIONE
075600000000     C*------------------------------------------------------------------------*
075700000000     C     CARVR         BEGSR
075800000000     C*
075900000000     C* SE LA STATISTICA NON E' STATA RICHIESTA
076000000000IF  1C     D60ST1        IFEQ      'N'
076100000000     C*
076200000000IF  2C     KEY           IFEQ      F12
076300050111     C                   EXSR      CARPV                                        *CARICA VIDEO PRIMA
076400000000E   2C                   ENDIF
076500000000IF  2C     KEY           IFEQ      ENTER
076600000000     C                   EXSR      CARVD                                        *CARICA VIDEO DOPO
076700000000E   2C                   ENDIF
076800000000     C*
076900000000     C* SE LA STATISTICA E' STATA RICHIESTA: GESTIONE NORMALE
077000000000X   1C                   ELSE
077100000000     C                   MOVEL     'VR'          $GES                           *GESTIONE ATTUALE
077200000000     C                   MOVEL     DRI(1)        VCCSTA                         *DECOD STATISTICA
077300000000     C*
077400000000     C* PORTA SCHIERE MEMORIZZAZIONE IN SCHIERE LAVORO
077500000000     C                   EXSR      VRLAV                                        *REGIONE-REGIONE
077600000000     C*
077700000000     C                   MOVEA     FUN1          FUN                            *SCHIERA FUNZIONI
077800000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
077900000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
078000000000     C*
078100000000     C                   WRITE     VTESTA                                       *TESTATA
078200000000     C                   WRITE     VFUNZ                                        *FUNZIONI
078300000000     C*
078400000000     C                   EXSR      POSSFL                                       *CARICA SFL
078500000000     C*
078600000000E   1C                   ENDIF
078700000000     C*
078800000000     C                   ENDSR
078900000000     C*------------------------------------------------------------------------*
079000000000     C*?VRLAV - PORTA SCHIERE MEMORIZZAZIONE "REGIONE-REGIONE" IN SCHIERE LAVORO
079100000000     C*------------------------------------------------------------------------*
079200000000     C     VRLAV         BEGSR
079300000000     C*
079400000000     C                   MOVEL     $DR           $DW                            *DESCRIZIONE VALORE
079500000000     C                   Z-ADD     $VR           $VW                            *VALORE DEL MESE
079600000000     C                   Z-ADD     $TR           $TW                            *VALORE TOTALE
079700000000     C                   Z-ADD     $ER           $EW                            *NUMERO DECIMALI
079800000000     C*
079900000000     C                   ENDSR
080000000000     C*------------------------------------------------------------------------*
080100000000     C*?GESVR - GESTIONE VIDEO STATISTICA REGIONE-REGIONE
080200000000     C*------------------------------------------------------------------------*
080300000000     C     GESVR         BEGSR
080400000000     C*
080500000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
080600000000     C                   WRITE     VTESTA                                       *TESTATA
080700000000     C                   WRITE     VFUNZ                                        *FUNZIONI
080800000000E   1C                   ENDIF
080900000000     C*
081000000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
081100000000     C*
081200000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
081300000000     C*
081400000000IF  1C     $ERR          IFEQ      '0'
081500000000SL  2C                   SELECT
081600000000     C     KEY           WHENEQ    ENTER                                        *CARICA VIDEO DOPO
081700000000     C                   EXSR      CARVD
081800000000     C     KEY           WHENEQ    F12                                          *CARICA VIDEO PRIMA
081900050111     C                   EXSR      CARPV
082000000000     C                   OTHER
082100000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
082200000000E   2C                   ENDSL
082300000000E   1C                   ENDIF
082400000000     C*
082500000000     C                   ENDSR
082600000000     C*------------------------------------------------------------------------*
082700000000     C*?CARVD - CARICA VIDEO STATISTICA DELTA CLIENTE
082800000000     C*------------------------------------------------------------------------*
082900000000     C     CARVD         BEGSR
083000000000     C*
083100000000     C* SE LA STATISTICA NON E' STATA RICHIESTA
083200000000IF  1C     D60ST4        IFEQ      'N'
083300000000     C*
083400000000IF  2C     KEY           IFEQ      F12
083500000000     C                   EXSR      CARVR                                        *CARICA VIDEO DOPO
083600000000E   2C                   ENDIF
083700000000IF  2C     KEY           IFEQ      ENTER
083800000000     C                   EXSR      CARVA                                        *CARICA VIDEO DOPO
083900000000E   2C                   ENDIF
084000000000     C*
084100000000     C* SE LA STATISTICA E' STATA RICHIESTA: GESTIONE NORMALE
084200000000X   1C                   ELSE
084300000000     C                   MOVEL     'VD'          $GES                           *GESTIONE ATTUALE
084400000000     C                   MOVEL     DRI(4)        VCCSTA                         *DECOD STATISTICA
084500000000     C                   MOVEL     *BLANKS       VCCTVA                         *NO DECOD VALORE
084600000000     C*
084700000000     C* PORTA SCHIERE MEMORIZZAZIONE IN SCHIERE LAVORO
084800000000     C                   EXSR      VDLAV                                        *DELTA
084900000000     C*
085000000000     C                   MOVEA     FUN1          FUN                            *SCHIERA FUNZIONI
085100000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
085200000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
085300000000     C*
085400000000     C                   WRITE     VTESTA                                       *TESTATA
085500000000     C                   WRITE     VFUNZ                                        *FUNZIONI
085600000000     C*
085700000000     C                   EXSR      POSSFL                                       *CARICA SFL
085800000000     C*
085900000000E   1C                   ENDIF
086000000000     C*
086100000000     C                   ENDSR
086200000000     C*------------------------------------------------------------------------*
086300000000     C*?VDLAV - PORTA SCHIERE MEMORIZZAZIONE "DELTA CLIENTE" IN SCHIERE LAVORO
086400000000     C*------------------------------------------------------------------------*
086500000000     C     VDLAV         BEGSR
086600000000     C*
086700000000     C                   MOVEL     $DD           $DW                            *DESCRIZIONE VALORE
086800000000     C                   Z-ADD     $VD           $VW                            *VALORE DEL MESE
086900000000     C                   Z-ADD     $TD           $TW                            *VALORE TOTALE
087000000000     C                   Z-ADD     $ED           $EW                            *NUMERO DECIMALI
087100000000     C*
087200000000     C                   ENDSR
087300000000     C*------------------------------------------------------------------------*
087400000000     C*?GESVD - GESTIONE VIDEO STATISTICA DELTA CLIENTE
087500000000     C*------------------------------------------------------------------------*
087600000000     C     GESVD         BEGSR
087700000000     C*
087800000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
087900000000     C                   WRITE     VTESTA                                       *TESTATA
088000000000     C                   WRITE     VFUNZ                                        *FUNZIONI
088100000000E   1C                   ENDIF
088200000000     C*
088300000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
088400000000     C*
088500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
088600000000     C*
088700000000IF  1C     $ERR          IFEQ      '0'
088800000000SL  2C                   SELECT
088900000000     C     KEY           WHENEQ    ENTER                                        *CARICA VIDEO DOPO
089000000000     C                   EXSR      CARVA
089100000000     C     KEY           WHENEQ    F12                                          *CARICA VIDEO PRIMA
089200000000     C                   EXSR      CARVR
089300000000     C                   OTHER
089400000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
089500000000E   2C                   ENDSL
089600000000E   1C                   ENDIF
089700000000     C*
089800000000     C                   ENDSR
089900000000     C*------------------------------------------------------------------------*
090000000000     C*?CARVA - CARICA VIDEO STATISTICA AFFIDABILITA'
090100000000     C*------------------------------------------------------------------------*
090200000000     C     CARVA         BEGSR
090300000000     C*
090400000000     C* SE LA STATISTICA NON E' STATA RICHIESTA
090500000000IF  1C     D60ST3        IFEQ      'N'
090600000000     C*
090700000000IF  2C     KEY           IFEQ      F12
090800000000     C                   EXSR      CARVD                                        *CARICA VIDEO PRIMA
090900000000E   2C                   ENDIF
091000000000IF  2C     KEY           IFEQ      ENTER
091100000000     C                   EXSR      CARVM                                        *CARICA VIDEO DOPO
091200000000E   2C                   ENDIF
091300000000     C*
091400000000     C* SE LA STATISTICA E' STATA RICHIESTA: GESTIONE NORMALE
091500000000X   1C                   ELSE
091600000000     C                   MOVEL     'VA'          $GES                           *GESTIONE ATTUALE
091700000000     C                   MOVEL     DRI(3)        VCCSTA                         *DECOD STATISTICA
091800000000     C                   MOVEL     *BLANKS       VCCTVA                         *NO DECOD VALORE
091900000000     C*
092000000000     C* PORTA SCHIERE MEMORIZZAZIONE IN SCHIERE LAVORO
092100000000     C                   EXSR      VALAV                                        *AFFIDABILITA'
092200000000     C*
092300000000     C                   MOVEA     FUN1          FUN                            *SCHIERA FUNZIONI
092400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
092500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
092600000000     C*
092700000000     C                   WRITE     VTESTA                                       *TESTATA
092800000000     C                   WRITE     VFUNZ                                        *FUNZIONI
092900000000     C*
093000000000     C                   EXSR      POSSFL                                       *CARICA SFL
093100000000     C*
093200000000E   1C                   ENDIF
093300000000     C*
093400000000     C                   ENDSR
093500000000     C*------------------------------------------------------------------------*
093600000000     C*?VALAV - PORTA SCHIERE MEMORIZZAZIONE "AFFIDABILITA'" IN SCHIERE LAVORO
093700000000     C*------------------------------------------------------------------------*
093800000000     C     VALAV         BEGSR
093900000000     C*
094000000000     C                   MOVEL     $DA           $DW                            *DESCRIZIONE VALORE
094100000000     C                   Z-ADD     *ZEROS        $VW                            *NON UTILIZZATO
094200000000     C                   Z-ADD     *ZEROS        $TW                            *NON UTILIZZATO
094300000000     C                   Z-ADD     *ZEROS        $EW                            *NON UTILIZZATO
094400000000     C*
094500000000     C                   ENDSR
094600000000     C*------------------------------------------------------------------------*
094700000000     C*?GESVA - GESTIONE VIDEO STATISTICA AFFIDABILITA'
094800000000     C*------------------------------------------------------------------------*
094900000000     C     GESVA         BEGSR
095000000000     C*
095100000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
095200000000     C                   WRITE     VTESTA                                       *TESTATA
095300000000     C                   WRITE     VFUNZ                                        *FUNZIONI
095400000000E   1C                   ENDIF
095500000000     C*
095600000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
095700000000     C*
095800000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
095900000000     C*
096000000000IF  1C     $ERR          IFEQ      '0'
096100000000SL  2C                   SELECT
096200000000     C     KEY           WHENEQ    ENTER                                        *CARICA VIDEO DOPO
096300000000     C                   EXSR      CARVM
096400000000     C     KEY           WHENEQ    F12                                          *CARICA VIDEO PRIMA
096500000000     C                   EXSR      CARVD
096600000000     C                   OTHER
096700000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
096800000000E   2C                   ENDSL
096900000000E   1C                   ENDIF
097000000000     C*
097100000000     C                   ENDSR
097200000000     C*------------------------------------------------------------------------*
097300000000     C*?CARVM - CARICA VIDEO STATISTICA AMMINISTRAZIONE
097400000000     C*------------------------------------------------------------------------*
097500000000     C     CARVM         BEGSR
097600000000     C*
097700000000     C* SE LA STATISTICA NON E' STATA RICHIESTA
097800000000IF  1C     D60ST5        IFEQ      'N'
097900000000     C*
098000000000IF  2C     KEY           IFEQ      F12
098100000000     C                   EXSR      CARVA                                        *CARICA VIDEO PRIMA
098200000000E   2C                   ENDIF
098300000000IF  2C     KEY           IFEQ      ENTER
098400000000     C                   EXSR      CARVS                                        *PRIMO VIDEO
098500000000E   2C                   ENDIF
098600000000     C*
098700000000     C* SE LA STATISTICA E' STATA RICHIESTA: GESTIONE NORMALE
098800000000X   1C                   ELSE
098900000000     C                   MOVEL     'VM'          $GES                           *GESTIONE ATTUALE
099000000000     C                   MOVEL     DRI(5)        VCCSTA                         *DECOD STATISTICA
099100000000     C                   MOVEL     *BLANKS       VCCTVA                         *NO DECOD VALORE
099200000000     C*
099300000000     C* PORTA SCHIERE MEMORIZZAZIONE IN SCHIERE LAVORO
099400000000     C                   EXSR      VMLAV                                        *AMMINISTRAZIONE
099500000000     C*
099600000000     C                   MOVEA     FUN1          FUN                            *SCHIERA FUNZIONI
099700000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
099800000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
099900000000     C*
100000000000     C                   WRITE     VTESTA                                       *TESTATA
100100000000     C                   WRITE     VFUNZ                                        *FUNZIONI
100200000000     C*
100300000000     C                   EXSR      POSSFL                                       *CARICA SFL
100400000000     C*
100500000000E   1C                   ENDIF
100600000000     C*
100700000000     C                   ENDSR
100800000000     C*------------------------------------------------------------------------*
100900000000     C*?VMLAV - PORTA SCHIERE MEMORIZZAZIONE "AMMINISTRAZIONE" IN SCHIERE LAVORO
101000000000     C*------------------------------------------------------------------------*
101100000000     C     VMLAV         BEGSR
101200000000     C*
101300000000     C                   MOVEL     $DM           $DW                            *DESCRIZIONE VALORE
101400000000     C                   Z-ADD     $VM           $VW                            *VALORE DEL MESE
101500000000     C                   Z-ADD     $TM           $TW                            *VALORE TOTALE
101600000000     C                   Z-ADD     $EM           $EW                            *NUMERO DECIMALI
101700000000     C*
101800000000     C                   ENDSR
101900000000     C*------------------------------------------------------------------------*
102000000000     C*?GESVM - GESTIONE VIDEO STATISTICA AMMINISTRAZIONE
102100000000     C*------------------------------------------------------------------------*
102200000000     C     GESVM         BEGSR
102300000000     C*
102400000000IF  1C     $TESVD        IFEQ      '1'                                          *TESTATA DA RIEMETTE
102500000000     C                   WRITE     VTESTA                                       *TESTATA
102600000000     C                   WRITE     VFUNZ                                        *FUNZIONI
102700000000E   1C                   ENDIF
102800000000     C*
102900000000     C                   EXSR      OUTSFL                                       *EMETTE SUBFILE
103000000000     C*
103100000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
103200000000     C*
103300000000IF  1C     $ERR          IFEQ      '0'
103400000000SL  2C                   SELECT
103500000000     C     KEY           WHENEQ    ENTER                                        *PRIMO VIDEO
103600000000     C                   EXSR      CARVS
103700000000     C     KEY           WHENEQ    F12                                          *CARICA VIDEO PRIMA
103800000000     C                   EXSR      CARVA
103900000000     C                   OTHER
104000000000     C                   EXSR      FUNSFL                                       *FUNZIONI STND SUBFI
104100000000E   2C                   ENDSL
104200000000E   1C                   ENDIF
104300000000     C*
104400000000     C                   ENDSR
104500000000     C*------------------------------------------------------------------------*
104600000000     C*?SETFIL - POSIZIONAMENTO SCHIERE DA CARICARE A VIDEO
104700000000     C*------------------------------------------------------------------------*
104800000000     C     SETFIL        BEGSR
104900000000     C*
105000000000     C                   Z-ADD     1             IS                             *INDICE SCHIERE VIDE
105100000000     C*
105200000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
105300000000     C     $DW(IS)       IFEQ      *BLANKS                                      *SCHIERA VUOTA -->
105400000000     C                   Z-ADD     1             SFLEOF                         *F I N E   F I L E
105500000000E   1C                   ENDIF
105600000000     C*
105700000000     C                   ENDSR
105800000000     C*------------------------------------------------------------------------*
105900000000     C*?LETFIL - LETTURA SCHIERE DA CARICARE A VIDEO
106000000000     C*------------------------------------------------------------------------*
106100000000     C     LETFIL        BEGSR
106200000000     C*
106300000000     C                   ADD       1             IS                             *INDICE SCHIERE VIDE
106400000000     C*
106500000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
106600020605IF  1C     IS            IFGT      1500                                         LUNGHEZZA MAX SCHIER
106700000000     C     $DW(IS)       OREQ      *BLANKS                                      *SCHIERA VUOTA
106800000000     C                   Z-ADD     1             SFLEOF
106900000000E   1C                   ENDIF
107000000000     C*
107100000000     C                   ENDSR
107200000000     C*------------------------------------------------------------------------*
107300000000     C*?IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DALLE SCHIERE MEMORIZZATE
107400000000     C*------------------------------------------------------------------------*
107500000000     C     IRESFL        BEGSR
107600000000     C*--------------------
107700000000     C* DESCRIZIONE VALORE
107800000000     C*--------------------
107900000000     C                   MOVEL     $DW(IS)       VSCDES                         *DESCRIZIONE VALORE
108000000000     C*
108100000000     C* N O  AFFIDABILITA'
108200000000IF  1C     $GES          IFNE      'VA'
108300000000     C*--------------------
108400000000     C* VALORE DEL MESE
108500000000     C*--------------------
108600000000IF  2C     $VW(IS)       IFNE      *ZEROS
108700000000     C                   Z-ADD     $VW(IS)       $NUM             16 3
108800000000     C                   Z-ADD     $EW(IS)       $DEC              1 0
108900010920    ? /COPY GAITRASRC/SRCTISE,TISE65T
109000000000     C                   MOVE      $ALF          VSCVAM                         *ALLINEAMENTO A Dx
109100000000X   2C                   ELSE
109200000000     C                   MOVEL     *BLANKS       VSCVAM
109300000000E   2C                   ENDIF
109400000000     C*--------------------
109500000000     C* TOTALI AL MESE
109600000000     C*--------------------
109700000000IF  2C     $TW(IS)       IFNE      *ZEROS
109800000000     C                   Z-ADD     $TW(IS)       $NUM             16 3
109900000000     C                   Z-ADD     $EW(IS)       $DEC              1 0
110000010920    ? /COPY GAITRASRC/SRCTISE,TISE65T
110100000000     C                   MOVE      $ALF          VSCVAT                         *ALLINEAMENTO A Dx
110200000000X   2C                   ELSE
110300000000     C                   MOVEL     *BLANKS       VSCVAT
110400000000E   2C                   ENDIF
110500000000     C*
110600000000     C* S I  AFFIDABILITA'
110700000000X   1C                   ELSE
110800000000     C                   MOVE      $PA(IS)       VSCVAM
110900000000     C                   MOVE      $TA(IS)       VSCVAT
111000000000E   1C                   ENDIF
111100000000     C*
111200000000     C                   ENDSR
111300000000     C*------------------------------------------------------------------------*
111400020826     C*?
111500000000     C*------------------------------------------------------------------------*
111600950726     C     CICSAL        BEGSR
111700950726     C*
111800950726     C* LETTURA PRIMO CLIENTE
111900000821     C                   Z-ADD     CLI(1)        DEPKSC                         *CLIENTE/I SELEZ.TO
112000000821IF  1C     D60SCE        IFEQ      '1'
112100000821     C                   Z-ADD     500           C                              *UN SOLO CLIENTE
112200950726X   1C                   ELSE
112300000821     C                   Z-ADD     1             C                              *PIU' CLIENTI
112400950726E   1C                   ENDIF
112500950726     C*
112600000821DO  1C     C             DOWLE     500
112700950726IF  2C     DEPKSC        IFGT      *ZEROS
112800000000     C*
112900000000     C* LETTURA SALDI PER TUTTE LE STATISTICHE
113000950726IF  3C     SOLAFF        IFEQ      '0'                                          *CHIESTE ANCHE ALTRE
113100000000     C                   EXSR      CICSTA                                       *STATISTICHE OLTRE L
113200040319     C                   EVAL      WTRD = 'BNB'
113300040319     C                   EXSR      CICSTAE                                      *STATISTICHE OLTRE L
113400040319     C                   EVAL      WTRD = 'BAN'
113500040318     C                   EXSR      CICSTAE                                      *STATISTICHE OLTRE L
113600050111     C                   EXSR      CICSTAP                                      *STATISTICHE PESI
113700050111     C                   EXSR      CICSTAV                                      *STATISTICHE VOLUMI
113800950726E   3C                   ENDIF                                                  *AFFIDABILITA' SPEDI
113900000000     C*
114000000000     C* LETTURA SALDI PER LA STATISTICA "AFFIDABILITA' SPEDIZIONI"
114100950726IF  3C     MAIAFF        IFEQ      '0'                                          *CHIESTE ANCHE LA
114200000000     C                   EXSR      CICAFF                                       *STATISTICA AFFID.TA
114300950726E   3C                   ENDIF                                                  *OLTRE LE ALTRE
114400000000     C*
114500000000     C* LETTURA PARTICOLARITA' SALDI CLIENTE PER LA "AMMINISTRAZIONE"
114600950726IF  3C     D60ST5        IFEQ      'S'
114700000000     C                   EXSR      CICAMM
114800950726E   3C                   ENDIF
114900950726E   2C                   ENDIF
115000950726     C*
115100950726     C                   ADD       1             C                              *PROSSIMO CLIENTE
115200000821IF  2C     C             IFLE      500
115300000821     C                   Z-ADD     CLI(C)        DEPKSC
115400950726E   2C                   ENDIF
115500950726E   1C                   ENDDO
115600000000     C*
115700000000     C* VALORIZZO LE SCHIERE DA PORTARE A VIDEO CON I SALDI MEMORIZZATI
115800000000     C                   EXSR      VIDCLI
115900000000     C*
116000000000     C                   ENDSR
116100000000     C*------------------------------------------------------------------------*
116200000000     C*?CICSTA - CICLO MEMORIZZAZIONE SALDI PER TUTTE LE STATISTICHE
116300000000     C*------------------------------------------------------------------------*
116400000000     C     CICSTA        BEGSR
116500000000     C*
116600000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
116700000000     C                   EXSR      SETFI1
116800000000     C*
116900000000     C* CICLO FINO A FINE FILE SALDI CLIENTE
117000000000DO  1C     $FINC         DOWEQ     '0'
117100000000     C*
117200000000     C* MEMORIZZA I SALDI CLIENTE
117300000000     C                   EXSR      MEMSDC
117400000000     C*
117500000000     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
117600000000     C                   EXSR      LETFI1
117700000000     C*
117800000000E   1C                   ENDDO
117900000000     C*
118000000000     C                   ENDSR
118100000000     C*------------------------------------------------------------------------*
118200000000     C*?SETFI1 - POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
118300000000     C*------------------------------------------------------------------------*
118400000000     C     SETFI1        BEGSR
118500000000     C*
118600000000     C                   MOVEL     '0'           $FINC
118700000000     C*
118800000000     C                   Z-ADD     DEPKSC        KSCKSC                         *DEPOSITO CLIENTE
118900000000     C                   Z-ADD     ANNINI        KSCANN                         *ANNO INIZIALE
119000000000     C                   Z-ADD     MESINI        KSCMES                         *MESE INIZIALE
119100000000     C*
119200000000     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
119300990609     C     KEYSDC        SETLL     SISDC01L                           99        *FINE ARCHIVIO
119400000000     C*
119500000000IF  1C     *IN99         IFEQ      '1'
119600000000     C                   MOVEL     '1'           $FINC
119700000000X   1C                   ELSE
119800000000     C                   EXSR      LETFI1                                       *LETTURA RECORD
119900000000E   1C                   ENDIF
120000000000     C*
120100000000     C                   ENDSR
120200000000     C*------------------------------------------------------------------------*
120300000000     C*?LETFI1 - LETTURA PROSSIMO RECORD FILE SALDI CLIENTE
120400000000     C*------------------------------------------------------------------------*
120500000000     C     LETFI1        BEGSR
120600000000     C*
120700000000     C* LEGGO FINO A:
120800000000     C*     - FINE FILE
120900000000     C*     - TROVATO RECORD VALIDO
121000000000     C                   MOVEL     'N'           WRECOK
121100000000DO  1C     $FINC         DOWEQ     '0'
121200000000     C     WRECOK        ANDEQ     'N'
121300000000     C*
121400990609     C     KSCKSC        READE     SISDC01L                               99
121500000000     C*
121600000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
121700000000     C                   MOVEL     '1'           $FINC
121800000000X   2C                   ELSE
121900000000     C                   EXSR      VALSDC                                       *VALORIZZA CON "SDC"
122000000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
122100030520     C*
122200030520     C* CONTROLLO SE IL RECORD E' VALORIZZATO
122300030520IF  3C     SDCNSP        IFEQ      *ZEROS
122400030520     C     SDCPKG        ANDEQ     *ZEROS
122500030520     C     SDCPCC        ANDEQ     *ZEROS
122600030520     C     SDCPLS        ANDEQ     *ZEROS
122700030520     C     SDCNCO        ANDEQ     *ZEROS
122800030520     C     SDCVOL        ANDEQ     *ZEROS
122900030520     C     SDCVCC        ANDEQ     *ZEROS
123000030520     C     SDCVLS        ANDEQ     *ZEROS
123100030520     C     SDCIRB        ANDEQ     *ZEROS
123200030520     C     SDCIRR        ANDEQ     *ZEROS
123300030520     C     SDCINC        ANDEQ     *ZEROS
123400030520     C     SDCIRP        ANDEQ     *ZEROS
123500030520     C     SDCIRC        ANDEQ     *ZEROS
123600030520     C     SDCIRD        ANDEQ     *ZEROS
123700030520     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
123800030520E   3C                   ENDIF
123900000000E   2C                   ENDIF
124000000000     C*
124100000000E   1C                   ENDDO
124200000000     C*
124300000000     C                   ENDSR
124400000000     C*------------------------------------------------------------------------*
124500000000     C*?VALSDC - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDC"
124600000000     C*------------------------------------------------------------------------*
124700000000     C     VALSDC        BEGSR
124800000000     C*
124900000000     C                   MOVEL     'SDC'         $SDX                           *FILE DA CONTROLLARE
125000000000     C                   Z-ADD     SDCDAT        $DAT                           *DATA (AA/MM)
125100000000     C                   MOVEL     SDCTSP        $TSP                           *TIPO SERVIZIO
125200000000     C                   MOVEL     SDCTPO        $TPO                           *TIPO PORTO
125300000000     C***                  MOVELSDCTPR    $TPR             *TIPO PRODOTTO
125400000000     C                   MOVEL     SDCCTR        $CTR                           *CODICE TARIFFA
125500000000     C                   MOVEL     SDCFPE        $FPE                           *FASCIA DI PESO
125600000000     C                   MOVEL     SDCRBL        $RBL                           *RAGGRUPPAMENTO BOLL
125700000000     C*
125800000000     C                   ENDSR
125900000000     C*------------------------------------------------------------------------*
126000000000     C*?CHKRE1 - CONTROLLA VALIDITA' RECORD SALDI CLIENTE
126100000000     C*------------------------------------------------------------------------*
126200000000     C     CHKRE1        BEGSR
126300000000     C*
126400000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
126500000000     C*--------------------
126600000000     C* DATA SALDI
126700000000     C*--------------------
126800000000     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
126900020826IF  1C     $DAT          IFGE      D60DAT
127000020826     C     $DAT          ANDLE     D60DA2
127100020826X   1C                   ELSE
127200020826     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
127300020826IF  2C     $DAT          IFGT      D60DA2
127400020826     C                   MOVEL     '1'           $FINC                          *ESCO DAL CICLO "SDC
127500020826     C                   MOVEL     '1'           $FINA                          *ESCO DAL CICLO "SDA
127600020826E   2C                   ENDIF
127700020826E   1C                   ENDIF
127800000000     C*
127900000000IF  1C     WRECOK        IFEQ      'S'
128000000000     C*
128100000000     C* CONTROLLO IL RECORD LETTO CON I PARAMETRI GENERALI RICHIESTI A VIDEO
128200010920     C* IMPORTANDO LA ROUTINE DI CONTROLLO ESTERNA ('TISE62T')
128300000000     C                   MOVEL     'S'           $ROK                           *RECORD OK X ROUTINE
128400000000     C*
128500010920    ? /COPY GAITRASRC/SRCTISE,TISE62T
128600000000     C*
128700000000     C* SE IL RECORD E' ERRATO NON FACCIO ULTERIORI CONTROLLI
128800000000     C                   MOVEL     $ROK          WRECOK
128900000000IF  2C     WRECOK        IFEQ      'S'                                          *RECORD VALIDO
129000000000     C*
129100000000     C* CONTROLLO I PARAMETRI DELLE STATISTICHE DIVERSE DALLA AFFIDABILITA' SOLO
129200990609     C* SE SONO NEL CICLO DI LETTURA DI SISDC00F
129300000000IF  3C     $SDX          IFEQ      'SDC'
129400000000     C*
129500000000     C* CONTROLLO PARAMETRI STATISTICA REGIONE-REGIONE SE SELEZIONATA
129600000000IF  4C     D60ST1        IFEQ      'S'
129700000000     C                   EXSR      CHKREG
129800000000E   4C                   ENDIF
129900000000     C*
130000050113     C* CONTROLLO PARAMETRI STATISTICA PESI-VOLUMI     SE SELEZIONATA
130100050113IF  4C     D60STp        IFEQ      'S'
130200050113     C                   EXSR      CHKPEV
130300050113E   4C                   ENDIF
130400050113     C*
130500000000     C* CONTROLLO PARAMETRI STATISTICA SERVIZIO BOLLE SE SELEZIONATA
130600000000IF  4C     D60ST2        IFEQ      'S'
130700000000     C                   EXSR      CHKSER
130800000000E   4C                   ENDIF
130900000000     C*
131000000000     C* CONTROLLO PARAMETRI STATISTICA DELTA CLIENTE SE SELEZIONATA
131100000000IF  4C     D60ST4        IFEQ      'S'
131200000000     C                   EXSR      CHKDEL
131300000000E   4C                   ENDIF
131400000000     C*
131500000000     C* CONTROLLO PARAMETRI STATISTICA AMMINISTRAZIONE SE SELEZIONATA
131600000000IF  4C     D60ST5        IFEQ      'S'
131700000000     C                   EXSR      CHKAMM
131800000000E   4C                   ENDIF
131900000000     C*
132000000000     C* SE PER TUTTE LE STAT CONTROLLATE IL RECORD NON VA BENE NE LEGGO UN ALTRO
132100000000IF  4C     ROKREG        IFEQ      'N'                                          *NO X REGIONE-REGION
132200050110     C     ROKPEV        ANDEQ     'N'                                          *NO X PESI/VOLUMI
132300000000     C     ROKSER        ANDEQ     'N'                                          *NO X SERVIZIO BOLLE
132400000000     C     ROKDEL        ANDEQ     'N'                                          *NO X DELTA CLIENTE
132500000000     C     ROKAMM        ANDEQ     'N'                                          *NO X AMMINISTRAZION
132600000000     C                   MOVEL     'N'           WRECOK                         *NON VALIDO
132700000000E   4C                   ENDIF
132800000000     C*
132900000000X   3C                   ELSE
133000000000     C*
133100000000     C* CONTROLLO I PARAMETRI DELLE STATISTICA AFFIDABILITA' SOLO SE SONO NEL
133200990609     C* CICLO DI LETTURA DI SISDA00F
133300000000IF  4C     D60ST3        IFEQ      'S'
133400000000     C                   EXSR      CHKAFF
133500000000     C                   MOVEL     ROKAFF        WRECOK                         *RECORD VALIDO O NON
133600000000E   4C                   ENDIF
133700000000E   3C                   ENDIF
133800000000     C*
133900000000E   2C                   ENDIF
134000000000E   1C                   ENDIF
134100000000     C*
134200000000     C                   ENDSR
134300040318     C*------------------------------------------------------------------------*
134400040318     C*?CICSTAE - CICLO MEMORIZZAZIONE SALDI ESTENSIONE
134500040318     C*------------------------------------------------------------------------*
134600040318     C     CICSTAE       BEGSR
134700040318     C*
134800040318     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
134900040318     C                   EXSR      SETFI1E
135000040318     C*
135100040318     C* CICLO FINO A FINE FILE SALDI CLIENTE
135200040318DO  1C     $FINCE        DOWEQ     '0'
135300040318     C*
135400040318     C* MEMORIZZA I SALDI CLIENTE
135500040318     C                   EXSR      MEMSDE
135600040318     C*
135700040318     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
135800040318     C                   EXSR      LETFI1E
135900040318     C*
136000040318E   1C                   ENDDO
136100040318     C*
136200040318     C                   ENDSR
136300040318     C*------------------------------------------------------------------------*
136400040318     C*?SETFI1E - POSIZIONAMENTO E PRIMA LETTURA SALDI ESTENSIONE
136500040318     C*------------------------------------------------------------------------*
136600040318     C     SETFI1E       BEGSR
136700040318     C*
136800040318     C* REIMPOSTO IL FINE FILE PER CICLO SALDI CLIENTE
136900040318     C                   MOVEL     '0'           $FINCE
137000040318     C*
137100040318     C                   MOVEL     'SDC'         KSEFILE                        *DEPOSITO CLIENTE
137200040319     C                   MOVEL     WTRD          KSETRD                         *DEPOSITO CLIENTE
137300040318     C                   Z-ADD     DEPKSC        KSEKSC                         *DEPOSITO CLIENTE
137400040318     C                   Z-ADD     ANNINI        KSEANN                         *ANNO INIZIALE
137500040318     C                   Z-ADD     MESINI        KSEMES                         *MESE INIZIALE
137600040318     C*
137700040318     C* POSIZIONAMENTO FILE SALDI ESTENSIONE
137800040318     C     KEYSDE        SETLL     SISDE02L                           99        *FINE ARCHIVIO
137900040318     C*
138000040318IF  1C     *IN99         IFEQ      '1'
138100040318     C                   MOVEL     '1'           $FINCE
138200040318X   1C                   ELSE
138300040318     C                   EXSR      LETFI1E                                      *LETTURA RECORD
138400040318E   1C                   ENDIF
138500040318     C*
138600040318     C                   ENDSR
138700040318     C*------------------------------------------------------------------------*
138800040318     C*?LETFI1E - LETTURA PROSSIMO RECORD FILE SALDI ESTENSIONE
138900040318     C*------------------------------------------------------------------------*
139000040318     C     LETFI1E       BEGSR
139100040318     C*
139200040318     C* LEGGO FINO A:
139300040318     C*     - FINE FILE
139400040318     C*     - TROVATO RECORD VALIDO
139500040318     C                   MOVEL     'N'           WRECOK
139600040318DO  1C     $FINCE        DOWEQ     '0'
139700040318     C     WRECOK        ANDEQ     'N'
139800040318     C*
139900040318     C     KEYSDE1       READE     SISDE02L                               99
140000040318     C*
140100040318IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
140200040318     C                   MOVEL     '1'           $FINCE
140300040318X   2C                   ELSE
140400040318     C                   EXSR      VALSDE                                       *VALORIZZA CON "SDC"
140500040318     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
140600040318E   2C                   ENDIF
140700040318     C*
140800040318E   1C                   ENDDO
140900040318     C*
141000040318     C                   ENDSR
141100040318     C*------------------------------------------------------------------------*
141200040318     C*?VALSDE - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDC"
141300040318     C*------------------------------------------------------------------------*
141400040318     C     VALSDE        BEGSR
141500040318     C*
141600040318     C                   MOVEL     'SDC'         $SDX                           *FILE DA CONTROLLARE
141700040318     C                   Z-ADD     SDEDAT        $DAT                           *DATA (AAAA/MM)
141800040318     C                   MOVEL     SDETSP        $TSP                           *TIPO SERVIZIO
141900040318     C                   MOVEL     SDETPO        $TPO                           *TIPO PORTO
142000040318     C***                  MOVELSDETPR    $TPR             *TIPO PRODOTTO
142100040318     C                   MOVEL     SDECTR        $CTR                           *CODICE TARIFFA
142200040318     C                   MOVEL     SDEFPE        $FPE                           *FASCIA DI PESO
142300040318     C                   MOVEL     SDERBL        $RBL                           *RAGGRUPPAMENTO BOLL
142400040318     C*
142500040318     C                   ENDSR
142600000000     C*------------------------------------------------------------------------*
142700000000     C*?CHKREG - CONTROLL RECORD CON I PARAMETRI DELLA "REGIONE-REGIONE"
142800000000     C*------------------------------------------------------------------------*
142900000000     C     CHKREG        BEGSR
143000000000     C*
143100000000     C                   MOVEL     'S'           ROKREG                         *RECORD VALIDO
143200970724     C*
143300000000     C                   ENDSR
143400050110     C*------------------------------------------------------------------------*
143500050110     C*?CHKPEV - CONTROLL RECORD CON I PARAMETRI DELLA "PESI-VOLUMI"
143600050110     C*------------------------------------------------------------------------*
143700050110     C     CHKPEV        BEGSR
143800050110     C*
143900050110     C                   MOVEL     'S'           ROKPEV                         *RECORD VALIDO
144000050110     C*
144100050110     C                   ENDSR
144200000000     C*------------------------------------------------------------------------*
144300000000     C*?CHKSER - CONTROLLO RECORD CON I PARAMETRI DELLA "SERVIZIO BOLLE"
144400000000     C*------------------------------------------------------------------------*
144500000000     C     CHKSER        BEGSR
144600000000     C*
144700000000     C                   MOVEL     'S'           ROKSER                         *RECORD VALIDO
144800000000     C*
144900000000     C                   ENDSR
145000000000     C*------------------------------------------------------------------------*
145100000000     C*?CHKDEL - CONTROLLO RECORD CON I PARAMETRI DELLA "DELTA CLIENTE"
145200000000     C*------------------------------------------------------------------------*
145300000000     C     CHKDEL        BEGSR
145400000000     C*
145500000000     C                   MOVEL     'S'           ROKDEL                         *RECORD VALIDO
145600000000     C*
145700000000     C                   ENDSR
145800000000     C*------------------------------------------------------------------------*
145900000000     C*?CHKAMM - CONTROLLO RECORD CON I PARAMETRI DELLA "AMMINISTRAZIONE"
146000000000     C*------------------------------------------------------------------------*
146100000000     C     CHKAMM        BEGSR
146200000000     C*
146300000000     C                   MOVEL     'S'           ROKAMM                         *RECORD VALIDO
146400000000     C*
146500000000     C                   ENDSR
146600050111     C*------------------------------------------------------------------------*
146700050111     C*?CICSTAP - CICLO MEMORIZZAZIONE SALDI PER LE STATISTICHE PESI
146800050111     C*------------------------------------------------------------------------*
146900050111     C     CICSTAP       BEGSR
147000050111     C*
147100050111     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
147200050111     C                   EXSR      SETFIP
147300050111     C*
147400050111     C* CICLO FINO A FINE FILE SALDI CLIENTE
147500050111DO  1C     $FINPE        DOWEQ     '0'
147600050111     C*
147700050111     C* MEMORIZZA I SALDI CLIENTE
147800050112     c                   z-add     1             nrPEV
147900050112     C                   EXSR      MEMPEV
148000050111     C*
148100050111     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
148200050111     C                   EXSR      LETFIP
148300050111     C*
148400050111E   1C                   ENDDO
148500050111     C*
148600050111     C                   ENDSR
148700050111     C*------------------------------------------------------------------------*
148800050111     C*?SETFIP - POSIZIONAMENTO E PRIMA LETTURA SALDI PESI CLIENTE
148900050111     C*------------------------------------------------------------------------*
149000050111     C     SETFIP        BEGSR
149100050111     C*
149200050111     C                   MOVEL     '0'           $FINPE
149300050111     C*
149400050111     C                   Z-ADD     DEPKSC        KPEKSC                         *DEPOSITO CLIENTE
149500050111     C                   Z-ADD     ANNINI        KPEANN                         *ANNO INIZIALE
149600050111     C                   Z-ADD     MESINI        KPEMES                         *MESE INIZIALE
149700050111     C*
149800050111     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
149900050111     C     KEYPES        SETLL     SIPES01L                           99        *FINE ARCHIVIO
150000050111     C*
150100050111IF  1C     *IN99         IFEQ      '1'
150200050111     C                   MOVEL     '1'           $FINPE
150300050111X   1C                   ELSE
150400050111     C                   EXSR      LETFIP                                       *LETTURA RECORD
150500050111E   1C                   ENDIF
150600050111     C*
150700050111     C                   ENDSR
150800050111     C*------------------------------------------------------------------------*
150900050111     C*?LETFIP - LETTURA PROSSIMO RECORD FILE SALDI PESI CLIENTE
151000050111     C*------------------------------------------------------------------------*
151100050111     C     LETFIP        BEGSR
151200050111     C*
151300050111     C* LEGGO FINO A:
151400050111     C*     - FINE FILE
151500050111     C*     - TROVATO RECORD VALIDO
151600050111     C                   MOVEL     'N'           WRECOK
151700050111DO  1C     $FINPE        DOWEQ     '0'
151800050111     C     WRECOK        ANDEQ     'N'
151900050111     C*
152000050111     C     KPEKSC        READE     SIPES01L                               99
152100050111     C*
152200050111IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
152300050111     C                   MOVEL     '1'           $FINPE
152400050111X   2C                   ELSE
152500050111     C                   EXSR      CHKREP                                       *CONTROLLO RECORD
152600050111E   2C                   ENDIF
152700050111     C*
152800050111E   1C                   ENDDO
152900050111     C*
153000050111     C                   ENDSR
153100050111     C*------------------------------------------------------------------------*
153200050111     C*?CHKREP - CONTROLLA VALIDITA' RECORD SALDI PESI CLIENTE
153300050111     C*------------------------------------------------------------------------*
153400050111     C     CHKREP        BEGSR
153500050111     C*
153600050111     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
153700050111     C*--------------------
153800050111     C* DATA SALDI PESI
153900050111     C*--------------------
154000050111     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
154100050111IF  1C     pesDAT        IFGE      D60DAT
154200050111     C     pesDAT        ANDLE     D60DA2
154300050111X   1C                   ELSE
154400050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
154500050111IF  2C     pesDAT        IFGT      D60DA2
154600050111     C                   MOVEL     '1'           $FINPE                         *ESCO DAL CICLO "SDP
154700050111E   2C                   ENDIF
154800050111E   1C                   ENDIF
154900050111     C*--------------------
155000050111     C* CODICE TARIFFA
155100050111     C*--------------------
155200050111     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
155300050111IF  1C     SELCTR        IFNE      'T'
155400050111     C                   MOVEL     pesCTR        $CTR
155500050111IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
155600050111     C     $CTR          OREQ      D60CT2
155700050111     C     $CTR          OREQ      D60CT3
155800050111     C     $CTR          OREQ      D60CT4
155900050111     C     $CTR          OREQ      D60CT5
156000050111     C     $CTR          OREQ      D60CT6
156100050111     C     $CTR          OREQ      D60CT7
156200050111     C     $CTR          OREQ      D60CT8
156300050111     C     $CTR          OREQ      D60CT9
156400050111     C     $CTR          OREQ      D60CT0
156500050111X   2C                   ELSE
156600050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
156700050111E   2C                   ENDIF
156800050111E   1C                   ENDIF
156900050111     C*
157000050111     C                   ENDSR
157100050111     C*------------------------------------------------------------------------*
157200050111     C*?CICSTAV - CICLO MEMORIZZAZIONE SALDI PER LE STATISTICHE VOLUMI
157300050111     C*------------------------------------------------------------------------*
157400050111     C     CICSTAV       BEGSR
157500050111     C*
157600050111     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE
157700050111     C                   EXSR      SETFIV
157800050111     C*
157900050111     C* CICLO FINO A FINE FILE SALDI CLIENTE
158000050111DO  1C     $FINVL        DOWEQ     '0'
158100050111     C*
158200050111     C* MEMORIZZA I SALDI CLIENTE
158300050112     c                   z-add     2             nrPEV
158400050112     C                   EXSR      MEMPEV
158500050111     C*
158600050111     C* LETTURA RECORD SALDI CLIENTE SUCCESSIVO
158700050111     C                   EXSR      LETFIV
158800050111     C*
158900050111E   1C                   ENDDO
159000050111     C*
159100050111     C                   ENDSR
159200050111     C*------------------------------------------------------------------------*
159300050111     C*?SETFIV - POSIZIONAMENTO E PRIMA LETTURA SALDI VOLUMI CLIENTE
159400050111     C*------------------------------------------------------------------------*
159500050111     C     SETFIV        BEGSR
159600050111     C*
159700050111     C                   MOVEL     '0'           $FINVL
159800050111     C*
159900050111     C                   Z-ADD     DEPKSC        KVLKSC                         *DEPOSITO CLIENTE
160000050111     C                   Z-ADD     ANNINI        KVLANN                         *ANNO INIZIALE
160100050111     C                   Z-ADD     MESINI        KVLMES                         *MESE INIZIALE
160200050111     C*
160300050111     C* POSIZIONAMENTO FILE SALDI CLIENTE (KSC+ANN+MES)
160400050111     C     KEYVLM        SETLL     SIVLM01L                           99        *FINE ARCHIVIO
160500050111     C*
160600050111IF  1C     *IN99         IFEQ      '1'
160700050111     C                   MOVEL     '1'           $FINVL
160800050111X   1C                   ELSE
160900050111     C                   EXSR      LETFIV                                       *LETTURA RECORD
161000050111E   1C                   ENDIF
161100050111     C*
161200050111     C                   ENDSR
161300050111     C*------------------------------------------------------------------------*
161400050111     C*?LETFIV - LETTURA PROSSIMO RECORD FILE SALDI PESI CLIENTE
161500050111     C*------------------------------------------------------------------------*
161600050111     C     LETFIV        BEGSR
161700050111     C*
161800050111     C* LEGGO FINO A:
161900050111     C*     - FINE FILE
162000050111     C*     - TROVATO RECORD VALIDO
162100050111     C                   MOVEL     'N'           WRECOK
162200050111DO  1C     $FINVL        DOWEQ     '0'
162300050111     C     WRECOK        ANDEQ     'N'
162400050111     C*
162500050111     C     KVLKSC        READE     SIVLM01L                               99
162600050111     C*
162700050111IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
162800050111     C                   MOVEL     '1'           $FINVL
162900050111X   2C                   ELSE
163000050111     C                   EXSR      CHKVOL                                       *CONTROLLO RECORD
163100050111E   2C                   ENDIF
163200050111     C*
163300050111E   1C                   ENDDO
163400050111     C*
163500050111     C                   ENDSR
163600050111     C*------------------------------------------------------------------------*
163700050111     C*?CHKVOL - CONTROLLA VALIDITA' RECORD SALDI VOLUMI CLIENTE
163800050111     C*------------------------------------------------------------------------*
163900050111     C     CHKVOL        BEGSR
164000050111     C*
164100050111     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
164200050111     C*--------------------
164300050111     C* DATA SALDI VOLUMI
164400050111     C*--------------------
164500050111     C* SE E' MAGGIORE DELLA DATA FINALE RICHIESTA A VIDEO, ESCO DAL CICLO
164600050111IF  1C     vlmDAT        IFGE      D60DAT
164700050111     C     vlmDAT        ANDLE     D60DA2
164800050111X   1C                   ELSE
164900050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
165000050111IF  2C     vlmDAT        IFGT      D60DA2
165100050111     C                   MOVEL     '1'           $FINVL                         *ESCO DAL CICLO "SDP
165200050111E   2C                   ENDIF
165300050111E   1C                   ENDIF
165400050111     C*--------------------
165500050111     C* CODICE TARIFFA
165600050111     C*--------------------
165700050111     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
165800050111IF  1C     SELCTR        IFNE      'T'
165900050111     C                   MOVEL     vlmCTR        $CTR
166000050111IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
166100050111     C     $CTR          OREQ      D60CT2
166200050111     C     $CTR          OREQ      D60CT3
166300050111     C     $CTR          OREQ      D60CT4
166400050111     C     $CTR          OREQ      D60CT5
166500050111     C     $CTR          OREQ      D60CT6
166600050111     C     $CTR          OREQ      D60CT7
166700050111     C     $CTR          OREQ      D60CT8
166800050111     C     $CTR          OREQ      D60CT9
166900050111     C     $CTR          OREQ      D60CT0
167000050111X   2C                   ELSE
167100050111     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
167200050111E   2C                   ENDIF
167300050111E   1C                   ENDIF
167400050111     C*
167500050111     C                   ENDSR
167600000000     C*------------------------------------------------------------------------*
167700000000     C*?CICAFF - CICLO MEMORIZZAZIONE SALDI SOLO PER LA "AFFIDABILITA' SPEDIZ"
167800000000     C*------------------------------------------------------------------------*
167900000000     C     CICAFF        BEGSR
168000000000     C*
168100000000     C* POSIZIONAMENTO E PRIMA LETTURA SALDI CLIENTE AFFIDABILITA'
168200000000     C                   EXSR      SETFI2
168300000000     C*
168400000000     C* CICLO FINO A FINE FILE SALDI AFFIDABILITA' CLIENTE
168500000000DO  1C     $FINA         DOWEQ     '0'
168600000000     C*
168700000000     C* MEMORIZZA I SALDI AFFIDABILITA' CLIENTE
168800000000     C                   EXSR      MEMSDA
168900000000     C*
169000000000     C* LETTURA SALDI AFFIDABILITA' CLIENTE SUCCESSIVO
169100000000     C                   EXSR      LETFI2
169200000000     C*
169300000000E   1C                   ENDDO
169400000000     C*
169500000000     C                   ENDSR
169600000000     C*------------------------------------------------------------------------*
169700000000     C*?SETFI2 - POSIZIONAMENTO E PRIMA LETTURA SALDI AFFIDABILITA' CLIENTE
169800000000     C*------------------------------------------------------------------------*
169900000000     C     SETFI2        BEGSR
170000000000     C*
170100000000     C                   MOVEL     '0'           $FINA
170200000000     C*
170300000000     C                   Z-ADD     DEPKSC        KSAKSC                         *DEPOSITO CLIENTE
170400000000     C                   Z-ADD     ANNINI        KSAANN                         *ANNO INIZIALE
170500000000     C                   Z-ADD     MESINI        KSAMES                         *MESE INIZIALE
170600000000     C*
170700000000     C* POSIZIONAMENTO FILE SALDI AFFID.TA' CLIENTE (KSC+ANN+MES)
170800990609     C     KEYSDA        SETLL     SISDA01L                           99        *FINE ARCHIVIO
170900000000     C*
171000000000IF  1C     *IN99         IFEQ      '1'
171100000000     C                   MOVEL     '1'           $FINA
171200000000X   1C                   ELSE
171300000000     C                   EXSR      LETFI2                                       *LETTURA RECORD
171400000000E   1C                   ENDIF
171500000000     C*
171600000000     C                   ENDSR
171700000000     C*------------------------------------------------------------------------*
171800000000     C*?LETFI2 - LETTURA PROSSIMO RECORD FILE SALDI AFFIDABILITA' CLIENTE
171900000000     C*------------------------------------------------------------------------*
172000000000     C     LETFI2        BEGSR
172100000000     C*
172200000000     C* LEGGO FINO A:
172300000000     C*     - FINE FILE
172400000000     C*     - TROVATO RECORD VALIDO
172500000000     C                   MOVEL     'N'           WRECOK
172600000000DO  1C     $FINA         DOWEQ     '0'
172700000000     C     WRECOK        ANDEQ     'N'
172800000000     C*
172900990609     C     KSAKSC        READE     SISDA01L                               99
173000000000     C*
173100000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
173200000000     C                   MOVEL     '1'           $FINA
173300000000X   2C                   ELSE
173400000000     C                   EXSR      VALSDA                                       *VALORIZZA CON "SDA"
173500000000     C                   EXSR      CHKRE1                                       *CONTROLLO RECORD
173600030520     C*
173700030520     C* CONTROLLO SE IL RECORD E' VALORIZZATO
173800030520IF  3C     SDANSP        IFEQ      *ZEROS
173900030520     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
174000030520E   3C                   ENDIF
174100000000E   2C                   ENDIF
174200000000     C*
174300000000E   1C                   ENDDO
174400000000     C*
174500000000     C                   ENDSR
174600000000     C*------------------------------------------------------------------------*
174700000000     C*?VALSDA - VALORIZZA I CAMPI DI PASSAGGIO ALLA ROUT DI CONTROLLO PER "SDA"
174800000000     C*------------------------------------------------------------------------*
174900000000     C     VALSDA        BEGSR
175000000000     C*
175100000000     C                   MOVEL     'SDA'         $SDX                           *FILE DA CONTROLLARE
175200000000     C                   Z-ADD     SDADAT        $DAT                           *DATA (AA/MM)
175300000000     C                   MOVEL     SDATSP        $TSP                           *TIPO SERVIZIO
175400000000     C                   MOVEL     *BLANKS       $TPO                           *TIPO PORTO
175500000000     C***                  MOVEL*BLANKS   $TPR             *TIPO PRODOTTO
175600000000     C                   MOVEL     *BLANKS       $CTR                           *CODICE TARIFFA
175700000000     C                   MOVEL     *BLANKS       $FPE                           *FASCIA DI PESO
175800000000     C                   MOVEL     *BLANKS       $RBL                           *RAGGRUPPAMENTO BOLL
175900000000     C*
176000000000     C                   ENDSR
176100000000     C*------------------------------------------------------------------------*
176200990609     C*?CHKAFF - CONTROLLO RECORD DI SISDA00F
176300000000     C*------------------------------------------------------------------------*
176400000000     C     CHKAFF        BEGSR
176500000000     C*
176600000000     C                   MOVEL     'S'           ROKAFF                         *RECORD VALIDO
176700000000     C*
176800000000     C                   ENDSR
176900000000     C*------------------------------------------------------------------------*
177000000000     C*?CICAMM - CICLO MEMORIZZAZIONE PARTICOLARITA' SALDI PER LA "AMMNINSTR.NE"
177100000000     C*------------------------------------------------------------------------*
177200000000     C     CICAMM        BEGSR
177300000000     C*
177400000000     C* POSIZIONAMENTO E PRIMA LETTURA PARTICOLARITA' SALDI CLIENTE
177500000000     C                   EXSR      SETFI3
177600000000     C*
177700000000     C* CICLO FINO A FINE FILE PARTICOLARITA' SALDI CLIENTE
177800000000DO  1C     $FINP         DOWEQ     '0'
177900000000     C*
178000000000     C* MEMORIZZA LE PARTICLARITA' SALDI DEL CLIENTE
178100000000     C                   EXSR      MEMSDP
178200000000     C*
178300000000     C* LETTURA PARTICOLARITA' SALDI DEL CLIENTE SUCESSIVO
178400000000     C                   EXSR      LETFI3
178500000000     C*
178600000000E   1C                   ENDDO
178700000000     C*
178800000000     C                   ENDSR
178900000000     C*------------------------------------------------------------------------*
179000000000     C*?SETFI3 - POSIZIONAMENTO E PRIMA LETTURA PARTICOLARITA' SALDI CLIENTE
179100000000     C*------------------------------------------------------------------------*
179200000000     C     SETFI3        BEGSR
179300000000     C*
179400000000     C* REIMPOSTO IL FINE FILE PER CICLO PARTICOLARITA' SALDI CLIENTE
179500000000     C                   MOVEL     '0'           $FINP
179600000000     C*
179700000000     C                   Z-ADD     DEPKSC        KSPKSC                         *DEPOSITO CLIENTE
179800000000     C                   Z-ADD     ANNINI        KSPANN                         *ANNO INIZIALE
179900000000     C                   Z-ADD     MESINI        KSPMES                         *MESE INIZIALE
180000000000     C*
180100000000     C* POSIZIONAMENTO FILE PARTICOLARITA' SALDI CLIENTE (KSC+ANN+MES)
180200990609     C     KEYSDP        SETLL     SISDP01L                           99        *FINE ARCHIVIO
180300000000     C*
180400000000IF  1C     *IN99         IFEQ      '1'
180500000000     C                   MOVEL     '1'           $FINP
180600000000X   1C                   ELSE
180700000000     C                   EXSR      LETFI3                                       *LETTURA RECORD
180800000000E   1C                   ENDIF
180900000000     C*
181000000000     C                   ENDSR
181100000000     C*------------------------------------------------------------------------*
181200000000     C*?LETFI3 - LETTURA PROSSIMO RECORD FILE PARTICOLARITA' SALDI CLIENTE
181300000000     C*------------------------------------------------------------------------*
181400000000     C     LETFI3        BEGSR
181500000000     C*
181600000000     C* LEGGO FINO A:
181700000000     C*     - FINE FILE
181800000000     C*     - TROVATO RECORD VALIDO
181900000000     C                   MOVEL     'N'           WRECOK
182000000000DO  1C     $FINP         DOWEQ     '0'
182100000000     C     WRECOK        ANDEQ     'N'
182200000000     C*
182300990609     C     KSPKSC        READE     SISDP01L                               99
182400000000     C*
182500000000IF  2C     *IN99         IFEQ      '1'                                          *FINE ARCHIVIO
182600000000     C                   MOVEL     '1'           $FINP
182700000000X   2C                   ELSE
182800000000     C                   EXSR      CHKRE3                                       *CONTROLLO RECORD
182900000000E   2C                   ENDIF
183000000000     C*
183100000000E   1C                   ENDDO
183200000000     C*
183300000000     C                   ENDSR
183400000000     C*------------------------------------------------------------------------*
183500000000     C*?CHKRE3 - CONTROLLA VALIDITA' RECORD PARTICOLARITA' SALDI CLIENTE
183600000000     C*------------------------------------------------------------------------*
183700000000     C     CHKRE3        BEGSR
183800000000     C*
183900000000     C                   MOVEL     'S'           WRECOK                         RECORD OK IN GENERAL
184000000000     C*--------------------
184100000000     C* DATA (AA/MM)
184200000000     C*--------------------
184300020826IF  1C     SDPDAT        IFGE      D60DAT
184400020826     C     SDPDAT        ANDLE     D60DA2
184500020826X   1C                   ELSE
184600020826     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
184700020826IF  2C     SDPDAT        IFGT      D60DA2
184800020826     C                   MOVEL     '1'           $FINP                          *ESCO DAL CICLO "SDP
184900020826E   2C                   ENDIF
185000020826E   1C                   ENDIF
185100000000     C*--------------------
185200000000     C* CODICE TARIFFA
185300000000     C*--------------------
185400000000     C* SE NON HO SELEZIONATO TARIFFE, NON FACCIO IL CONTROLLO DI ESCLUSIONE
185500000000IF  1C     SELCTR        IFNE      'T'
185600000000     C                   MOVEL     SDPCTR        $CTR
185700000000IF  3C     $CTR          IFEQ      D60CT1                                       *FA PARTE DELLA SELE
185800000000     C     $CTR          OREQ      D60CT2
185900000000     C     $CTR          OREQ      D60CT3
186000000000     C     $CTR          OREQ      D60CT4
186100000000     C     $CTR          OREQ      D60CT5
186200000000     C     $CTR          OREQ      D60CT6
186300000000     C     $CTR          OREQ      D60CT7
186400000000     C     $CTR          OREQ      D60CT8
186500000000     C     $CTR          OREQ      D60CT9
186600000000     C     $CTR          OREQ      D60CT0
186700000000X   2C                   ELSE
186800000000     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
186900000000E   2C                   ENDIF
187000000000E   1C                   ENDIF
187100000000     C*
187200000000     C                   ENDSR
187300000000     C*------------------------------------------------------------------------*
187400990609     C*?MEMSDC - MEMORIZZA I SALDI DI SISDC01L
187500000000     C*          INDICI:  J(4,0) - FASCIA(2)
187600000000     C*------------------------------------------------------------------------*
187700000000     C     MEMSDC        BEGSR
187800000000     C*
187900000000     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
188000000000     C* PER IL CONTEGGIO IN STAMPA DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
188100000000     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
188200000000     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
188300000000     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
188400000000     C*
188500000000     C* MI SALVO LE NOTE E LE RETTIFICHE PER LA "DELTA" NEL CASO VENGANO AZZ.TE
188600000000     C                   Z-ADD     SDCIRR        DELIRR                         RETTIFICHE
188700000000     C                   Z-ADD     SDCINC        DELINC                         NOTE ACCREDITO
188800000000     C*
188900000000     C                   Z-ADD     1             JI
189000000000     C                   SETOFF                                           28
189100000000     C     SDCRBL        LOOKUP    FLAB(JI)                               28
189200000000     C* BOLLA NON VALIDA COME N°SPEDIZIONE
189300000000IF  1C     FSP(JI)       IFNE      'S'
189400000000     C                   Z-ADD     *ZEROS        SDCNSP                         *NUMERO SPEDIZIONI
189500000000     C                   Z-ADD     *ZEROS        SDCPKG                         *PESO
189600020828     C                   Z-ADD     *ZEROS        SDCPCC                         *PESO CON CML
189700020828     C                   Z-ADD     *ZEROS        SDCPLS                         *PESO CML
189800000000     C                   Z-ADD     *ZEROS        SDCNCO                         *NUMERO COLLI
189900000000     C                   Z-ADD     *ZEROS        SDCVOL                         *VOLUME TOTALE
190000000000     C                   Z-ADD     *ZEROS        SDCVCC                         *VOLUME CON CML
190100000000     C                   Z-ADD     *ZEROS        SDCVLS                         *VOLUME CML
190200000000E   1C                   ENDIF
190300000000     C* BOLLA NON VALIDA COME RICAVO
190400000000IF  1C     FRI(JI)       IFNE      'S'
190500000000     C                   Z-ADD     *ZEROS        SDCIRB                         RICAVO REALE BOLLE
190600000000     C                   Z-ADD     *ZEROS        SDCIRR                         RETTIFICHE
190700000000     C                   Z-ADD     *ZEROS        SDCINC                         NOTE ACCREDITO
190800000000E   1C                   ENDIF
190900000000     C*
191000000000     C* RICAVO NETTO = RICAVO DA BOLLE + RETTIFICHE + NOTE DI CREDITO
191100000000     C                   Z-ADD     *ZEROS        RICTOT           16 3
191200000000     C     SDCIRB        ADD       SDCIRR        RICTOT
191300000000     C     SDCINC        ADD       RICTOT        RICTOT                         *RICAVO TOTALE
191400000000     C*
191500000000     C* RICAVO REALE DELTA = RICAVO REALE DELTA + NOTE + RETTIFICHE
191600000000     C                   Z-ADD     *ZEROS        RICDEL           16 3
191700000000     C     SDCIRD        ADD       DELIRR        RICDEL
191800000000     C     DELINC        ADD       RICDEL        RICDEL                         *RICAVO REALE DELTA
191900000000     C*--------------------
192000000000     C* R E G I O N E - R E G I O N E
192100000000     C*--------------------
192200000000IF  1C     D60ST1        IFEQ      'S'                                          *SE STATISTICA SELEZ
192300000000     C     ROKREG        ANDEQ     'S'                                          *SE PASSATO AI CONTR
192400000000     C*
192500000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
192600000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
192700000000     C                   MOVEL     '1'           PASREG                         *PER REGIONE
192800950726     C*
192900950726     C* RAGGRUPPAMENTO + RAGGRUPPAMENTO
193000950726     C                   MOVEL     *ZEROS        WRER              6
193100950726     C                   MOVEL     SDCREP        WRER                           *PAR+...
193200950726     C                   MOVE      SDCREA        WRER                           *PAR+ARR
193300950726     C                   MOVEL     *ZEROS        WPARER            7
193400950726     C                   MOVEL     'P'           WPARER                         *P+...+...
193500950726     C                   MOVE      WRER          WPARER                         *P+PAR+ARR
193600971128     C*
193700950726     C* CONTROLLA ESISTENZA
193800950726     C                   MOVE      REG           RE2                             *PRENDE SOLO CODICI
193900950726     C                   Z-ADD     1             J
194000950726     C                   SETOFF                                           28
194100950726     C     WRER          LOOKUP    RE2(J)                                 28
194200950726IF  2C     *IN28         IFEQ      *ON
194300950726     C                   Z-ADD     J             COMBIN                         *COMBIN GIA' PRESENT
194400950726X   2C                   ELSE
194500950726     C                   ADD       1             R                              *NON USARE ALTROVE
194600950726     C                   MOVEL     WPARER        REG(R)
194700950726     C                   Z-ADD     R             COMBIN                         *COMBIN NON PRESENTE
194800950726E   2C                   ENDIF
194900000000     C* O C U R
195000000000     C     COMBIN        OCCUR     CRER
195100950726IF  2C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
195200020605     C                   MOVEL     WRER          REX                            *COMBINAZIONE REP+REA
195300950206     C                   ADD       SDCNCO        REC(01)                        *COLLI DEL MESE
195400950206     C                   ADD       SDCPKG        REP(01)                        *PESO DEL MESE
195500950206     C                   ADD       RICTOT        RER(01)                        *RICAVI DEL MESE
195600950206     C                   ADD       SDCNSP        RES(01)                        *SPEDIZIONI DEL MESE
195700950206     C                   ADD       SDCVOL        REV(01)                        *VOLUME DEL MESE
195800950726E   2C                   ENDIF
195900950206     C                   ADD       SDCNCO        REC(02)                        *COLLI TOTALI
196000950206     C                   ADD       SDCPKG        REP(02)                        *PESO TOTALI
196100950206     C                   ADD       RICTOT        RER(02)                        *RICAVI TOTALI
196200950206     C                   ADD       SDCNSP        RES(02)                        *SPEDIZIONI TOTALI
196300950206     C                   ADD       SDCVOL        REV(02)                        *VOLUME TOTALI
196400000000E   1C                   ENDIF
196500000000     C*--------------------
196600000000     C* T O T A L I
196700000000     C*--------------------
196800000000IF  1C     D60ST2        IFEQ      'S'                                          *SE STATISTICA SELEZ
196900000000     C     ROKSER        ANDEQ     'S'                                          *SE PASSATO AI CONTR
197000000000     C*
197100000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
197200000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
197300000000     C                   MOVEL     '1'           PASSER                         *PER SERVIZIO BOLLE
197400000000     C*
197500000000DO  2C     1             DO        5             J
197600000000     C*
197700000000IF  3C     J             IFEQ      1                                            *SPEDIZIONI°
197800950206     C                   Z-ADD     SDCNSP        VALORE           16 3
197900000000E   3C                   ENDIF
198000000000IF  3C     J             IFEQ      2                                            *COLLI°
198100000000     C                   Z-ADD     SDCNCO        VALORE
198200000000E   3C                   ENDIF
198300000000IF  3C     J             IFEQ      3                                            *PESO°
198400000000     C                   Z-ADD     SDCPKG        VALORE
198500000000E   3C                   ENDIF
198600000000IF  3C     J             IFEQ      4                                            *VOLUME°
198700000000     C                   Z-ADD     SDCVOL        VALORE
198800000000E   3C                   ENDIF
198900000000IF  3C     J             IFEQ      5                                            *RICAVI°
199000000000     C                   Z-ADD     RICTOT        VALORE
199100000000E   3C                   ENDIF
199200000000     C* O C U R
199300000000     C     J             OCCUR     CSER
199400000000     C*
199500000000IF  3C     SDCTSP        IFEQ      'C'                                          *CORRIERE
199600000000IF  4C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
199700000000     C                   ADD       VALORE        SNC(01)                        *VALORI DEL MESE
199800000000E   4C                   ENDIF
199900000000     C                   ADD       VALORE        SNC(02)                        *VALORI TOTALI
200000000000E   3C                   ENDIF
200100000000     C*
200200000000IF  3C     SDCTSP        IFEQ      'D'                                          *DISTRIBUZIONE
200300000000IF  4C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
200400000629     C                   ADD       VALORE        SDC(01)                        *VALORI DEL MESE
200500000000E   4C                   ENDIF
200600000629     C                   ADD       VALORE        SDC(02)                        *VALORI TOTALI
200700000000E   3C                   ENDIF
200800000000     C*
200900000000IF  3C     SDCTSP        IFEQ      'E'                                          *ESPRESSO
201000000000IF  4C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
201100000000     C                   ADD       VALORE        SEC(01)                        *VALORI DEL MESE
201200000000E   4C                   ENDIF
201300000000     C                   ADD       VALORE        SEC(02)                        *VALORI TOTALI
201400000000E   3C                   ENDIF
201500000629     C*
201600000629IF  3C     SDCTSP        IFEQ      'P'                                          *POSTE
201700000629IF  4C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
201800000629     C                   ADD       VALORE        SPC(01)                        *VALORI DEL MESE
201900000629E   4C                   ENDIF
202000000629     C                   ADD       VALORE        SPC(02)                        *VALORI TOTALI
202100000629E   3C                   ENDIF
202200091013     C*
202300091013IF  3C     SDCTSP        IFEQ      'H'                                          *H 10:30
202400091013IF  4C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
202500091013     C                   ADD       VALORE        SHC(01)                        *VALORI DEL MESE
202600091013E   4C                   ENDIF
202700091013     C                   ADD       VALORE        SHC(02)                        *VALORI TOTALI
202800091013E   3C                   ENDIF
202900000000     C*
203000000000E   2C                   ENDDO
203100000000E   1C                   ENDIF
203200000000     C*--------------------
203300000000     C* D E L T A
203400000000     C*--------------------
203500000000IF  1C     D60ST4        IFEQ      'S'                                          *SE STATISTICA SELEZ
203600000000     C     ROKDEL        ANDEQ     'S'                                          *SE PASSATO AI CONTR
203700000000     C*
203800000309     C                   MOVEL     SDCANN        DATSDC            6 0
203900000309     C                   MOVE      SDCMES        DATSDC
204000000309     C*
204100000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
204200000309IF  3C     SDCFPE        IFEQ      '07'                                         *NO > 5000
204300000309     C     DATSDC        ANDLT     200001                                       *PRIMA DEL INIZIO GE
204400000309     C                   ELSE
204500000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
204600000000     C                   MOVEL     '1'           PASDEL                         *PER DELTA CLIENTE
204700000000     C*
204800000000     C* ELABORO LA FASCIA SOLO SE SELEZIONATA
204900000000     C                   MOVEL     SDCFPE        FASCIA            2
205000000000     C                   Z-ADD     1             J
205100000000     C                   SETOFF                                           28
205200000000     C     FASCIA        LOOKUP    SFPE(J)                                28
205300000000IF  3C     *IN28         IFEQ      *ON
205400000000     C* O C U R
205500000000     C     J             OCCUR     CDEL                                         *F A S C I A
205600000000IF  4C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
205700000000     C                   ADD       RICDEL        DRC(01)                        *RICAVI REALI
205800000000     C                   ADD       SDCIRP        DPC(01)                        *RICAVI PRESUNTI
205900000000E   4C                   ENDIF
206000000000     C                   ADD       RICDEL        DRC(02)                        *RICAVI REALI
206100000000     C                   ADD       SDCIRP        DPC(02)                        *RICAVI PRESUNTI
206200000000     C*
206300000000IF  4C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
206400000000     C                   ADD       SDCNSP        DSC(01)                        *SPEDIZIONI°
206500000000E   4C                   ENDIF
206600000000     C                   ADD       SDCNSP        DSC(02)                        *SPEDIZIONI°
206700000000     C*
206800000000E   3C                   ENDIF
206900000000E   2C                   ENDIF
207000000000E   1C                   ENDIF
207100000000     C*--------------------
207200000000     C* A M M I N I S T R A Z I O N E
207300000000     C*--------------------
207400990609     C* PRIMA DEL 97 -> FATTURATO = RICAVI SISDC + SISDP
207500990609     C* DOPO   IL 97 -> FATTURATO = RICAVI SISDP
207600000000IF  1C     D60ST5        IFEQ      'S'                                          *SE STATISTICA SELEZ
207700000000     C     ROKAMM        ANDEQ     'S'                                          *SE PASSATO AI CONTR
207800000000     C*
207900000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
208000000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
208100000000     C                   MOVEL     '1'           PASAMM                         *PER AMMINISTRAZIONE
208200000000IF  2C     SDCMES        IFEQ      MESFIN                                       *MESE RICHIESTO
208300000000     C                   ADD       SDCINC        ANC(01)                        *NOTE DI CREDITO
208400000000     C                   ADD       SDCIRR        ARC(01)                        *RETTIFICHE AI RICAV
208500990601IF  3C     SDCANN        IFLT      1997
208600970724     C                   ADD       SDCIRB        AFC(01)                        *FATTURATO LORDO
208700970724E   3C                   ENDIF
208800000000E   2C                   ENDIF
208900000000     C                   ADD       SDCINC        ANC(02)                        *NOTE DI CREDITO
209000000000     C                   ADD       SDCIRR        ARC(02)                        *RETTIFICHE AI RICAV
209100990601IF  3C     SDCANN        IFLT      1997
209200970724     C                   ADD       SDCIRB        AFC(02)                        *FATTURATO LORDO
209300970724E   3C                   ENDIF
209400000000E   1C                   ENDIF
209500000000     C*
209600000000     C                   ENDSR
209700040318     C*------------------------------------------------------------------------*
209800040318     C*?MEMSDE - MEMORIZZA I SALDI DI SISDE02L
209900040318     C*------------------------------------------------------------------------*
210000040318     C     MEMSDE        BEGSR
210100040318     C*
210200040318     C* CONTROLLO SE LA BOLLA E' DA CONSIDERARE COME N°SPEDIZIONE E COME RICAVO:
210300040318     C* PER IL CONTEGGIO IN STAMPA DEL N°SPED/COLLI/PESO/VOLUME PRENDO SOLO LE
210400040318     C* BOLLE CON FLAG FSP='S'; PER IL CONTEGGIO DEI RICAVI SOLAMENTE LE BOLLE
210500040318     C* CON FLAG FRI='S'; AZZERO PERCIO' I CAMPI DELLE BOLLE INTERESSATE
210600040318     C* TRANNE CHE PER LA "DELTA" CHE VANNO BENE TUTTI I TIPI SPEDIZIONE
210700040319     C*
210800040319     C* VALORIZZA IL CAMPONE SDEUNI CON LA DS BAN o BNB
210900040319     c                   if        wtrd = 'BNB'
211000040319     C                   MOVEL     SDEUNI        DAR5BNBSDE
211100040319     C                   CLEAR                   DAR5BANSDE
211200040319     c                   else
211300040319     C                   MOVEL     SDEUNI        DAR5BANSDE
211400040319     C                   CLEAR                   DAR5BNBSDE
211500040319     c                   endif
211600040318     C*
211700040318     C                   Z-ADD     1             JI
211800040318     C                   SETOFF                                           28
211900040318     C     SDERBL        LOOKUP    FLAB(JI)                               28
212000040318     C* BOLLA NON VALIDA COME N°SPEDIZIONE
212100040318IF  1C     FSP(JI)       IFNE      'S'
212200040319     C                   Z-ADD     *ZEROS        §BNBBCOR                       *PESO
212300040319     C                   Z-ADD     *ZEROS        §BNBBNBA                       *PESO
212400040319     C                   Z-ADD     *ZEROS        §BNBBNB2                       *PESO
212500040319     C                   Z-ADD     *ZEROS        §BANNBA                        *PESO
212600040319     C                   Z-ADD     *ZEROS        §BANNB2                        *PESO
212700040318E   1C                   ENDIF
212800040318     C* BOLLA NON VALIDA COME RICAVO
212900040318IF  1C     FRI(JI)       IFNE      'S'
213000040318E   1C                   ENDIF
213100040318     C*--------------------
213200040318     C* T O T A L I
213300040318     C*--------------------
213400040318IF  2C     D60ST2        IFEQ      'S'                                          *SE STATISTICA SELEZ
213500040318     C     ROKSER        ANDEQ     'S'                                          *SE PASSATO AI CONTR
213600040318     C*
213700040318     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
213800040318     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
213900040318     C                   MOVEL     '1'           PASSER                         *PER SERVIZIO BOLLE
214000040318     C*
214100040318     C* MEMORIZZA COLLI ORIGINALI
214200040319if  4c                   if        wtrd = 'BNB'
214300040319IF  5C     SDeMES        IFEQ      MESFIN                                       *MESE RICHIESTO
214400040319     C                   ADD       §bnbBCOR      COR(01)                        *POSTE
214500040319     C                   ADD       §bnbBnba      bnb(01)                        *POSTE
214600040319     C                   ADD       §bnbBnb2      bnb(01)                        *POSTE
214700040319x   5c                   endif
214800040319     C                   ADD       §bnbBCOR      COR(02)                        *POSTE
214900040319     C                   ADD       §bnbBnba      bnb(02)                        *POSTE
215000040319     C                   ADD       §bnbBnb2      bnb(02)                        *POSTE
215100040319x   4c                   else
215200040319     C* MEMORIZZA BANCALI x tipo record BAN
215300040319IF  5C     SDeMES        IFEQ      MESFIN                                       *MESE RICHIESTO
215400040319     C                   ADD       §bannba       ban(01)                        *POSTE
215500040319     C                   ADD       §bannb2       ban(01)                        *POSTE
215600040319e   5c                   endif
215700040319     C                   ADD       §bannba       ban(02)                        *POSTE
215800040319     C                   ADD       §bannb2       ban(02)                        *POSTE
215900040318E   4C                   ENDIF
216000040318E   3C                   ENDIF
216100040318     C*
216200040318     C                   ENDSR
216300000000     C*------------------------------------------------------------------------*
216400990609     C*?MEMSDA - MEMORIZZA I SALDI DI SISDA01L
216500000000     C*------------------------------------------------------------------------*
216600000000     C     MEMSDA        BEGSR
216700000000     C*--------------------
216800000000     C* A F F I D A B I L I T A '
216900000000     C*--------------------
217000000000IF  1C     D60ST3        IFEQ      'S'                                          *SE STATISTICA SELEZ
217100000000     C     ROKAFF        ANDEQ     'S'                                          *SE PASSATO AI CONTR
217200000000     C*
217300000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
217400000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
217500000000     C                   MOVEL     '1'           PASAFF                         *PER AFFIDABILITA'
217600000000     C*
217700000000IF  2C     SDADLV        IFLT      0
217800000000     C                   Z-ADD     1             J                              *IN ANTICIPO
217900000000X   2C                   ELSE
218000000000IF  3C     SDADLV        IFGT      48
218100000000     C                   Z-ADD     7             J                              *OLTRE 48HH RIT
218200000000X   3C                   ELSE
218300000000     C                   Z-ADD     2             J
218400000000     C                   SETOFF                                           28
218500000000     C     SDADLV        LOOKUP    ORE(J)                                 28
218600000000IF  4C     *IN28         IFEQ      *OFF                                         *NON TROVATO
218700000000     C                   Z-ADD     *ZEROS        J                              *PER DARE ERRORE CPF
218800000000E   4C                   ENDIF
218900000000E   3C                   ENDIF
219000000000E   2C                   ENDIF
219100000000     C* O C U R
219200000000     C     J             OCCUR     CAFF
219300000000     C*
219400000000IF  2C     SDATSP        IFEQ      'C'                                          *CORRIERE
219500000000IF  3C     SDAMES        IFEQ      MESFIN                                       *MESE RICHIESTO
219600000000     C                   ADD       SDANSP        CNC(01)                        *VALORI DEL MESE
219700000000E   3C                   ENDIF
219800000000     C                   ADD       SDANSP        CNC(02)                        *VALORI TOTALI
219900000000E   2C                   ENDIF
220000000000     C*
220100000000IF  2C     SDATSP        IFEQ      'D'                                          *DISTRIBUZIONE
220200000000IF  3C     SDAMES        IFEQ      MESFIN                                       *MESE RICHIESTO
220300000629     C                   ADD       SDANSP        CDC(01)                        *VALORI DEL MESE
220400000000E   3C                   ENDIF
220500000629     C                   ADD       SDANSP        CDC(02)                        *VALORI TOTALI
220600000000E   2C                   ENDIF
220700000000     C*
220800000000IF  2C     SDATSP        IFEQ      'E'                                          *ESPRESSO
220900000000IF  3C     SDAMES        IFEQ      MESFIN                                       *MESE RICHIESTO
221000000000     C                   ADD       SDANSP        CEC(01)                        *VALORI DEL MESE
221100000000E   3C                   ENDIF
221200000000     C                   ADD       SDANSP        CEC(02)                        *VALORI TOTALI
221300000000E   2C                   ENDIF
221400091214     C*
221500091214IF  2C     SDATSP        IFEQ      'H'                                          *ESPRESSO 1030
221600091214IF  3C     SDAMES        IFEQ      MESFIN                                       *MESE RICHIESTO
221700091214     C                   ADD       SDANSP        CHC(01)                        *VALORI DEL MESE
221800091214E   3C                   ENDIF
221900091214     C                   ADD       SDANSP        CHC(02)                        *VALORI TOTALI
222000091214E   2C                   ENDIF
222100000629     C*
222200000629IF  2C     SDATSP        IFEQ      'P'                                          *POSTE
222300000629IF  3C     SDAMES        IFEQ      MESFIN                                       *MESE RICHIESTO
222400000629     C                   ADD       SDANSP        CPC(01)                        *VALORI DEL MESE
222500000629E   3C                   ENDIF
222600000629     C                   ADD       SDANSP        CPC(02)                        *VALORI TOTALI
222700000629E   2C                   ENDIF
222800000000     C*
222900000000E   1C                   ENDIF
223000000000     C*
223100000000     C                   ENDSR
223200000000     C*------------------------------------------------------------------------*
223300990609     C*?MEMSDP - MEMORIZZA I SALDI DI SISDP01L
223400000000     C*------------------------------------------------------------------------*
223500000000     C     MEMSDP        BEGSR
223600000000     C*
223700000000     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
223800000000     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
223900000000     C                   MOVEL     '1'           PASAMM                         *PER AMMINISTRAZIONE
224000000000     C*
224100000000     C                   Z-ADD     1             J
224200000000     C                   SETOFF                                           28
224300000000     C     SDPSVR        LOOKUP    VAR(J)                                 28
224400000000IF  1C     *IN28         IFEQ      *OFF                                         *MAI MEMORIZZATA
224500000000     C                   ADD       1             P
224600000000     C                   MOVEL     SDPSVR        VAR(P)
224700000000     C                   Z-ADD     P             J
224800000000E   1C                   ENDIF
224900000000     C* O C U R
225000990609     C* PRIMA DEL 97 -> FATTURATO = RICAVI SISDC + SISDP
225100990609     C* DOPO   IL 97 -> FATTURATO = RICAVI SISDP
225200000000     C     J             OCCUR     CAMM
225300000000IF  1C     SDPMES        IFEQ      MESFIN                                       *MESE RICHIESTO
225400000000     C                   ADD       SDPRCP        ARI(01)                        *RICAVI DEL MESE
225500000000     C                   ADD       SDPNSP        ASP(01)                        *SPEDIZIONI DEL MESE
225600990601IF  2C     SDPANN        IFGE      1997                                           *PRIMA DEL
225700970722     C     AFC(01)       ADD       SDPRCP        AFC(01)                        *FATTURATO DEL MESE
225800970724X   2C                   ELSE
225900970724IF  3C     SDPSVR        IFNE      'G'
226000970724     C     SDPSVR        ANDNE     'E'
226100970724     C     SDPSVR        ANDNE     'R'
226200970724     C     SDPSVR        ANDNE     'P'
226300970724     C     SDPSVR        ANDNE     'A'
226400970724     C     SDPSVR        ANDNE     'S'
226500970724     C     SDPSVR        ANDNE     'F'
226600970724     C     AFC(01)       ADD       SDPRCP        AFC(01)                        *FATTURATO DEL MESE
226700970724E   3C                   ENDIF
226800970724E   2C                   ENDIF
226900970724E   1C                   ENDIF
227000970724     C*
227100000000     C                   ADD       SDPRCP        ARI(02)                        *RICAVI TOTALI
227200000000     C                   ADD       SDPNSP        ASP(02)                        *SPEDIZIONI TOTALI
227300990601IF  1C     SDPANN        IFGE      1997                                           *PRIMA DEL
227400970722     C     AFC(02)       ADD       SDPRCP        AFC(02)                        *FATTURATO TOTALE
227500970724X   1C                   ELSE
227600970724IF  2C     SDPSVR        IFNE      'G'
227700970724     C     SDPSVR        ANDNE     'E'
227800970724     C     SDPSVR        ANDNE     'R'
227900970724     C     SDPSVR        ANDNE     'P'
228000970724     C     SDPSVR        ANDNE     'A'
228100970724     C     SDPSVR        ANDNE     'S'
228200970724     C     SDPSVR        ANDNE     'F'
228300970724     C     AFC(02)       ADD       SDPRCP        AFC(02)                        *FATTURATO TOTALE
228400970724E   2C                   ENDIF
228500970724E   1C                   ENDIF
228600000000     C*
228700000000     C                   ENDSR
228800050111     C*------------------------------------------------------------------------*
228900050112     C*?MEMPEV - MEMORIZZA I SALDI DI SIPES01L
229000050111     C*------------------------------------------------------------------------*
229100050112     C     MEMPEV        BEGSR
229200050121     C*
229300050111     C*--------------------
229400050121     C* PESI/VOLUMI
229500050111     C*--------------------
229600050111IF  1C     D60STp        IFEQ      'S'                                          *SE STATISTICA SELEZ
229700050111     C     ROKpev        ANDEQ     'S'                                          *SE PASSATO AI CONTR
229800050111     C*
229900050111     C* PASSATO DALLA MEMORIZZAZIONE DEI SALDI
230000050111     C                   MOVEL     '1'           PASKSC                         *PER CLIENTE
230100050111     C                   MOVEL     '1'           PASpev                         *PER Peso Volume
230200050111     C* O C U R
230300050112     C     nrPEV         OCCUR     CPEV
230400050111     C*
230500050112     c                   if        nrPEV = 1
230600050112IF  3C     pesMES        IFEQ      MESFIN                                       *MESE RICHIESTO
230700050112     C                   ADD       pesNCV        NCV(01)                        *VALORI DEL MESE
230800050112     C                   ADD       pesTCL        NCB(01)
230900050112     C                   ADD       pesPSC        PVV(01)
231000050113     C                   ADD       pesPSB        PVB(01)
231100050119E   3C                   End
231200050112     C                   ADD       pesNCV        NCV(02)                        *VALORI TOTALI
231300050112     C                   ADD       pesTCL        NCB(02)
231400050112     C                   ADD       pesPSC        PVV(02)
231500050113     C                   ADD       pesPSB        PVB(02)
231600050112     c                   end
231700050112     C*
231800050112     c                   if        nrPEV = 2
231900050112IF  3C     vlmMES        IFEQ      MESFIN                                       *MESE RICHIESTO
232000050113     C                   ADD       vlmNCR        NCV(01)                        *VALORI DEL MESE
232100050112     C                   ADD       vlmTCL        NCB(01)
232200050113     C                   ADD       vlmVLC        PVV(01)
232300050113     C                   ADD       vlmVLB        PVB(01)
232400050119E   3C                   End
232500050113     C                   ADD       vlmNCR        NCV(02)                        *VALORI TOTALI
232600050112     C                   ADD       vlmTCL        NCB(02)
232700050113     C                   ADD       vlmVLC        PVV(02)
232800050113     C                   ADD       vlmVLB        PVB(02)
232900050112     c                   end
233000050111     C*
233100050111E   1C                   ENDIF
233200050111     C*
233300050111     C                   ENDSR
233400000000     C*------------------------------------------------------------------------*
233500000000     C*?VIDCLI - VALORIZZO LE SCHIERE DA PORTARE A VIDEO CON I SALDI MEMORIZZATI
233600000000     C*------------------------------------------------------------------------*
233700000000     C     VIDCLI        BEGSR
233800000000     C*
233900000000IF  1C     PASSER        IFEQ      '1'                                          *SE MEMORIZZATO QC
234000000000     C     D60ST2        ANDEQ     'S'                                          *SE SELEZIONATA
234100000000     C                   EXSR      VIDSER                                       *TOTALI
234200000000E   1C                   ENDIF
234300050110     C*
234400050110IF  1C     PASPEV        IFEQ      '1'                                          *SE MEMORIZZATO QC
234500050110     C     D60STp        ANDEQ     'S'                                          *SE SELEZIONATA
234600050110     C                   EXSR      VIDpev                                       *PESI VOLUMI
234700050110E   1C                   ENDIF
234800000000     C*
234900000000IF  1C     PASREG        IFEQ      '1'                                          *SE MEMORIZZATO QC
235000000000     C     D60ST1        ANDEQ     'S'                                          *SE SELEZIONATA
235100000000     C                   EXSR      VIDREG                                       *REGIONE-REGIONE
235200000000E   1C                   ENDIF
235300000000     C*
235400000000IF  1C     PASDEL        IFEQ      '1'                                          *SE MEMORIZZATO QC
235500000000     C     D60ST4        ANDEQ     'S'                                          *SE SELEZIONATA
235600000000     C                   EXSR      VIDDEL                                       *DELTA
235700000000E   1C                   ENDIF
235800000000     C*
235900000000IF  1C     PASAFF        IFEQ      '1'                                          *SE MEMORIZZATO QC
236000000000     C     D60ST3        ANDEQ     'S'                                          *SE SELEZIONATA
236100000000     C                   EXSR      VIDAFF                                       *AFFIDABILITA'
236200000000E   1C                   ENDIF
236300000000     C*
236400000000IF  1C     PASAMM        IFEQ      '1'                                          *SE MEMORIZZATO QC
236500000000     C     D60ST5        ANDEQ     'S'                                          *SE SELEZIONATA
236600000000     C                   EXSR      VIDAMM                                       *AMMINISTRAZIONE
236700000000E   1C                   ENDIF
236800000000     C*
236900000000     C                   ENDSR
237000000000     C*------------------------------------------------------------------------*
237100000000     C*?VIDDEL - IMPOSTA VIDEO STATISTICA "DELTA CLIENTE"
237200000000     C*------------------------------------------------------------------------*
237300000000     C     VIDDEL        BEGSR
237400000000     C*
237500000000     C                   Z-ADD     *ZEROS        IN                5 0          *INDICE DI SCHIERA
237600000000     C*
237700000000     C* PER OGNI FASCIA DI PESO SELEZIONATA DA VISUALIZZARE
237800000000     C                   Z-ADD     1             J
237900000000DO  1C     1             DO        SPARF         J
238000000000     C     J             OCCUR     CDEL
238100000000     C*
238200000000     C* CALCOLO I TOTALI A FONDO PAGINA
238300000000     C                   ADD       DRC           DRW                            *RICAVI REALI
238400000000     C                   ADD       DPC           DPW                            *RICAVI PRESUNTI
238500000000     C                   ADD       DSC           DSW                            *SPEDIZIONI
238600000000     C*
238700000000     C* DECODIFICO LA FASCIA SELEZIONATA
238800000000     C                   Z-ADD     1             I
238900000000     C                   SETOFF                                           28
239000000000     C     SFPE(J)       LOOKUP    FPE(I)                                 28
239100000000IF  2C     *IN28         IFEQ      *ON
239200000000     C                   MOVEL     DFPE(I)       DPRIMD           14
239300000000X   2C                   ELSE
239400000000     C                   MOVEL     *ALL'-'       DPRIMD
239500000000E   2C                   ENDIF
239600000000     C*--------------------
239700000000     C* VALORIZZA  R I C A V I
239800000000     C*--------------------
239900000000     C                   MOVEL     DCA(7)        DSECOD
240000000000     C                   MOVEL     *BLANKS       VALSER           23
240100000000     C     DPRIMD        CAT       DSECOD        VALSER
240200000000     C*
240300000000     C                   ADD       1             IN                             *INDICE
240400000000     C                   MOVEL     VALSER        $DD(IN)                        *DESCRIZIONE VALORE
240500000000     C                   Z-ADD     DRC(1)        $VD(IN)                        *VALORE MESE
240600000000     C                   Z-ADD     DRC(2)        $TD(IN)                        *VALORE TOTALE
240700010920     C                   Z-ADD     2             $ED(IN)                        *NUMERO DI DECIMALI
240800000000     C*--------------------
240900000000     C* VALORIZZA  S P E D I Z I O N I
241000000000     C*--------------------
241100000000     C                   MOVEL     *BLANKS       DPRIMD                         *DESCR FASCIA 1 VOLT
241200000000     C                   MOVEL     DCA(8)        DSECOD
241300000000     C                   MOVEL     *BLANKS       VALSER           23
241400000000     C     DPRIMD        CAT       DSECOD        VALSER
241500000000     C*
241600000000     C                   ADD       1             IN                             *INDICE
241700000000     C                   MOVEL     VALSER        $DD(IN)                        *DESCRIZIONE VALORE
241800000000     C                   Z-ADD     DSC(1)        $VD(IN)                        *VALORE MESE
241900000000     C                   Z-ADD     DSC(2)        $TD(IN)                        *VALORE TOTALE
242000000000     C                   Z-ADD     *ZEROS        $ED(IN)                        *NUMERO DI DECIMALI
242100000000     C*--------------------
242200000000     C* VALORIZZA  D E L T A
242300000000     C*--------------------
242400000000     C                   Z-ADD     DRC(01)       WRRW             16 3          *RICAVO REALE
242500000000     C                   Z-ADD     DPC(01)       WRPW             16 3          *RICAVO PRESUNTO
242600000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
242700000000     C                   EXSR      CALDEL
242800000000     C                   Z-ADD     WDPW          DDW(01)                        *DELTA DEL MESE
242900000000     C*
243000000000     C                   Z-ADD     DRC(02)       WRRW             16 3          *RICAVO REALE
243100000000     C                   Z-ADD     DPC(02)       WRPW             16 3          *RICAVO PRESUNTO
243200000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
243300000000     C                   EXSR      CALDEL
243400000000     C                   Z-ADD     WDPW          DDW(02)                        *DELTA TOTALE
243500000000     C*
243600000000     C                   MOVEL     DCA(9)        DSECOD
243700000000     C                   MOVEL     *BLANKS       VALSER           23
243800000000     C     DPRIMD        CAT       DSECOD        VALSER
243900000000     C*
244000000000     C                   ADD       1             IN                             *INDICE
244100000000     C                   MOVEL     VALSER        $DD(IN)                        *DESCRIZIONE VALORE
244200000000     C                   Z-ADD     DDW(1)        $VD(IN)                        *VALORE MESE
244300000000     C                   Z-ADD     DDW(2)        $TD(IN)                        *VALORE TOTALE
244400000000     C                   Z-ADD     1             $ED(IN)                        *NUMERO DI DECIMALI
244500000000     C*
244600000000     C                   ADD       1             IN                             *INDICE
244700000000     C                   MOVEL     *ALL'-'       $DD(IN)                        *RIGA DI SEPARAZIONE
244800000000     C                   Z-ADD     *ZEROS        $ED(IN)                        *NUMERO DI DECIMALI
244900000000     C*
245000000000E   1C                   ENDDO
245100000000     C*--------------------
245200000000     C* VALORIZZA  T O T A L I   R I C A V I
245300000000     C*--------------------
245400000000     C                   MOVEL     DCA(7)        DPRIMD
245500000000     C                   MOVEL     DCA(10)       DSECOD
245600000000     C                   MOVEL     *BLANKS       VALSER           23
245700000000     C     DPRIMD        CAT       DSECOD        VALSER
245800000000     C*
245900000000     C                   ADD       1             IN                             *INDICE
246000000000     C                   MOVEL     VALSER        $DD(IN)                        *DESCRIZIONE VALORE
246100000000     C                   Z-ADD     DRW(1)        $VD(IN)                        *VALORE MESE
246200000000     C                   Z-ADD     DRW(2)        $TD(IN)                        *VALORE TOTALE
246300010920     C                   Z-ADD     2             $ED(IN)                        *NUMERO DI DECIMALI
246400000000     C*--------------------
246500000000     C* VALORIZZA  T O T A L I   S P E D I Z I O N I
246600000000     C*--------------------
246700000000     C                   MOVEL     DCA(1)        DPRIMD
246800000000     C                   MOVEL     DCA(10)       DSECOD            8
246900000000     C                   MOVEL     *BLANKS       VALSER           23
247000000000     C     DPRIMD        CAT       DSECOD        VALSER
247100000000     C*
247200000000     C                   ADD       1             IN                             *INDICE
247300000000     C                   MOVEL     VALSER        $DD(IN)                        *DESCRIZIONE VALORE
247400000000     C                   Z-ADD     DSW(1)        $VD(IN)                        *VALORE MESE
247500000000     C                   Z-ADD     DSW(2)        $TD(IN)                        *VALORE TOTALE
247600000000     C                   Z-ADD     *ZEROS        $ED(IN)                        *NUMERO DI DECIMALI
247700000000     C*--------------------
247800000000     C* VALORIZZA  T O T A L I   D E L T A
247900000000     C*--------------------
248000000000     C                   Z-ADD     DRW(01)       WRRW             16 3          *RICAVO REALE
248100000000     C                   Z-ADD     DPW(01)       WRPW             16 3          *RICAVO PRESUNTO
248200000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
248300000000     C                   EXSR      CALDEL
248400000000     C                   Z-ADD     WDPW          DDW(01)                        *DELTA DEL MESE
248500000000     C*
248600000000     C                   Z-ADD     DRW(02)       WRRW             16 3          *RICAVO REALE
248700000000     C                   Z-ADD     DPW(02)       WRPW             16 3          *RICAVO PRESUNTO
248800000000     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
248900000000     C                   EXSR      CALDEL
249000000000     C                   Z-ADD     WDPW          DDW(02)                        *DELTA TOTALE
249100000000     C*
249200000000     C                   MOVEL     DCA(9)        DPRIMD
249300000000     C                   MOVEL     DCA(6)        DSECOD            8
249400000000     C                   MOVEL     *BLANKS       VALSER           23
249500000000     C     DPRIMD        CAT       DSECOD        VALSER
249600000000     C*
249700000000     C                   ADD       1             IN                             *INDICE
249800000000     C                   MOVEL     VALSER        $DD(IN)                        *DESCRIZIONE VALORE
249900000000     C                   Z-ADD     DDW(1)        $VD(IN)                        *VALORE MESE
250000000000     C                   Z-ADD     DDW(2)        $TD(IN)                        *VALORE TOTALE
250100000000     C                   Z-ADD     1             $ED(IN)                        *NUMERO DI DECIMALI
250200000000     C*
250300000000     C                   ADD       1             IN                             *INDICE
250400000000     C                   MOVEL     *ALL'-'       $DD(IN)                        *RIGA DI SEPARAZIONE
250500000000     C                   Z-ADD     *ZEROS        $ED(IN)                        *NUMERO DI DECIMALI
250600000000     C*
250700000000     C                   ENDSR
250800000000     C*------------------------------------------------------------------------*
250900000000     C*?VIDAMM - IMPOSTA VIDEO STATISTICA "AMMINISTRAZIONE"
251000000000     C*------------------------------------------------------------------------*
251100000000     C     VIDAMM        BEGSR
251200000000     C*
251300950726     C                   Z-ADD     *ZEROS        IN                             *INDICE DI SCHIERA
251400000000     C                   MOVEL     '0'           DICUI
251500000000     C*--------------------
251600000000     C* VALORIZZA  F A T T U R A T O
251700000000     C*--------------------
251800970722     C     AFC           ADD       ANC           AFC                            *AGGIUNGE NOTE
251900970722     C     AFC           ADD       ARC           AFC                            *AGGIUNGE RETTIFICHE
252000000000     C                   ADD       1             IN                             *INDICE
252100000000     C                   MOVEL     DME(10)       $DM(IN)                        *DESCRIZIONE VALORE
252200000000     C                   Z-ADD     AFC(01)       $VM(IN)                        *VALORI MESE
252300000000     C                   Z-ADD     AFC(02)       $TM(IN)                        *VALORE TOTALE
252400010920     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
252500000000     C*--------------------
252600000000     C* VALORIZZA  N O T E   A C C R E D I T O
252700000000     C*--------------------
252800000000     C                   XFOOT     ANC           COM$VA
252900000000IF  1C     COM$VA        IFNE      *ZEROS                                       *SE MEMORIZZATO QC.
253000000000IF  2C     DICUI         IFEQ      '0'
253100000000     C                   MOVEL     '1'           DICUI             1
253200000000     C                   ADD       1             IN                             *INDICE
253300000000     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
253400010920     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
253500000000E   2C                   ENDIF
253600000000     C*
253700000000     C                   ADD       1             IN                             *INDICE
253800000000     C                   MOVEL     DME(11)       $DM(IN)                        *DESCRIZIONE VALORE
253900000000     C                   Z-ADD     ANC(01)       $VM(IN)                        *VALORI MESE
254000000000     C                   Z-ADD     ANC(02)       $TM(IN)                        *VALORE TOTALE
254100010920     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
254200000000     C*--------------------
254300000000     C* VALORIZZA  P E R C E N T U A L E   N O T E
254400000000     C*--------------------
254500000000     C                   Z-ADD     AFC(01)       WFAW             16 3          *FATTURATO
254600000000     C                   Z-ADD     ANC(01)       WNRW             16 3          *NOTE O RETTIFICHE
254700000000     C                   Z-ADD     *ZEROS        WPRW              4 1          PERCENTUALE FATTURAT
254800000000     C                   EXSR      CALPER
254900000000     C                   Z-ADD     WPRW          APW(01)                        *PERCENTUALE MESE
255000000000     C*
255100000000     C                   Z-ADD     AFC(02)       WFAW             16 3          *FATTURATO
255200000000     C                   Z-ADD     ANC(02)       WNRW             16 3          *NOTE O RETTIFICHE
255300000000     C                   Z-ADD     *ZEROS        WPRW              4 1          PERCENTUALE FATTURAT
255400000000     C                   EXSR      CALPER
255500000000     C                   Z-ADD     WPRW          APW(02)                        *PERCENTUALE TOTALE
255600000000     C*
255700000000     C                   ADD       1             IN                             *INDICE
255800000000     C                   MOVEL     DME(13)       $DM(IN)                        *DESCRIZIONE VALORE
255900000000     C                   Z-ADD     APW(01)       $VM(IN)                        *VALORI MESE
256000000000     C                   Z-ADD     APW(02)       $TM(IN)                        *VALORE TOTALE
256100000000     C                   Z-ADD     1             $EM(IN)                        *NUMERO DI DECIMALI
256200000000E   1C                   ENDIF
256300000000     C*--------------------
256400000000     C* VALORIZZA  R E T T I F I C H E
256500000000     C*--------------------
256600000000     C                   XFOOT     ARC           COM$VA
256700000000IF  1C     COM$VA        IFNE      *ZEROS                                       *SE MEMORIZZATO QC.
256800000000IF  2C     DICUI         IFEQ      '0'
256900000000     C                   MOVEL     '1'           DICUI             1
257000000000     C                   ADD       1             IN                             *INDICE
257100000000     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
257200010920     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
257300000000E   2C                   ENDIF
257400000000     C*
257500000000     C                   ADD       1             IN                             *INDICE
257600000000     C                   MOVEL     DME(12)       $DM(IN)                        *DESCRIZIONE VALORE
257700000000     C                   Z-ADD     ARC(01)       $VM(IN)                        *VALORI MESE
257800000000     C                   Z-ADD     ARC(02)       $TM(IN)                        *VALORE TOTALE
257900010920     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
258000000000E   1C                   ENDIF
258100070627      *                                                    ?
258200070627      *  Ciclo per visualizzazione dell Varie del cliente  ?
258300070627     c                   do        sav_CA        CA
258400070627      **?  **  aggancia la chiave delle schiere tab.varie "CA"
258500070627     c                   move      SkOrd(CA)     CAkey             3 0
258600070627      **
258700070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
258800070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
258900070627     C                   Z-ADD     1             P
259000070627     C                   SETOFF                                           28
259100070627     C     SkVar(CAkey)  LOOKUP    VAR(P)                                 28
259200070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
259300070627     C     P             OCCUR     CAMM
259400070627     C                   ADD       ARI           $VA                            *RICAVI "I"
259500070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "I"
259600070627E   1C                   ENDIF
259700070627     C*
259800070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
259900070627     C                   XFOOT     $VA           COM$VA           16 3
260000070627     C                   XFOOT     $V2           COM$V2           16 3
260100070627IF  1C     COM$VA        IFNE      *ZEROS
260200070627     C     COM$V2        ORNE      *ZEROS
260300070627IF  2C     DICUI         IFEQ      '0'
260400070627     C                   MOVEL     '1'           DICUI             1
260500070627     C                   ADD       1             IN                             *INDICE
260600070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
260700070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
260800070627E   2C                   ENDIF
260900070627     C*--------------------
261000070627     C* VALORIZZA  R I C A V O  G I A C E N Z E
261100070627     C*--------------------
261200070627     C                   ADD       1             IN                             *INDICE
261300070627     C                   MOVEL     SkDsc(CAkey)  $DM(IN)                        *DESCRIZIONE VALORE
261400070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
261500070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
261600070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
261700070627     C*--------------------
261800070627     C* VALORIZZA  N U M E R O  G I A C E N Z E
261900070627     C*--------------------
262000070627     C                   ADD       1             IN                             *INDICE
262100070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
262200070627      *?    Eccezione...(#) non è un valore di ricavo ma Numerico .  ?
262300070627     c                   if        SkVar(CAkey) = '#'
262400070627     C                   move      ' Nr.'        $DM(IN)                        *DESCRIZIONE VALORE
262500070627     c                   end
262600070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
262700070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
262800070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
262900070627E   1C                   ENDIF
263000070627      **?  **  ?
263100070627E   0C                   EndDO
263200070627      **?  *------------------------------------*  ?
263300070627      **?  * per provare  al momento non cancello le specifiche ma le salto *  ?
263400070627      **?  *------------------------------------*  ?
263500070627      **?  **  ?
263600070627     c                   GOTO      Salta_TUTTO
263700070627      *                                                    ?
263800051017     C*
263900070627      **?  * Le vecchie specifiche sono state parcheggiate nella Routine OLD *  ?
264000070627      **?  * a PRO-MEMORIA per verificare che tutto funzioni correttamente.  *  ?
264100070627      **?  * In un prossimo futuro POTRANNO e DOVRANNO essere cancellate !!  *  ?
264200070627     c                   Exsr      Specifiche_OLD
264300070627      **?  **  ?
264400070627      **?  *-----------------------*  ?
264500070627     c     Salta_TUTTO   tag
264600070627      **?  *-----------------------*  ?
264700070627      **?  **  ?
264800070627     C*
264900020201     C                   ADD       1             IN                             *INDICE
265000020201     C                   MOVEL     *ALL'-'       $DM(IN)                        *RIGA DI SEPARAZIONE
265100020201     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
265200000000     C*
265300000000     C                   ENDSR
265400050113     C*------------------------------------------------------------------------*
265500070627      *?  VECCHIE SPECIFICHE SOSTITUITE CON Tabella "CA" correlata alla "CC"...  ?
265600050113     C*------------------------------------------------------------------------*
265700070627     C     Specifiche_OLDBEGSR
265800070627     C*
265900070627     C*--------------------
266000070627     C* CALCOLO GIACENZE -->  GIACENZE = (VARIA "I")
266100070627     C*--------------------
266200070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
266300070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
266400070627     C                   Z-ADD     1             P
266500070627     C                   SETOFF                                           28
266600070627     C     'I'           LOOKUP    VAR(P)                                 28
266700070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
266800070627     C     P             OCCUR     CAMM
266900070627     C                   ADD       ARI           $VA                            *RICAVI "I"
267000070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "I"
267100070627E   1C                   ENDIF
267200070627     C*
267300070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
267400070627     C                   XFOOT     $VA           COM$VA           16 3
267500070627     C                   XFOOT     $V2           COM$V2           16 3
267600070627IF  1C     COM$VA        IFNE      *ZEROS
267700070627     C     COM$V2        ORNE      *ZEROS
267800070627IF  2C     DICUI         IFEQ      '0'
267900070627     C                   MOVEL     '1'           DICUI             1
268000070627     C                   ADD       1             IN                             *INDICE
268100070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
268200070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
268300070627E   2C                   ENDIF
268400070627     C*--------------------
268500070627     C* VALORIZZA  R I C A V O  G I A C E N Z E
268600070627     C*--------------------
268700070627     C                   ADD       1             IN                             *INDICE
268800070627     C                   MOVEL     DME(14)       $DM(IN)                        *DESCRIZIONE VALORE
268900070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
269000070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
269100070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
269200070627     C*--------------------
269300070627     C* VALORIZZA  N U M E R O  G I A C E N Z E
269400070627     C*--------------------
269500070627     C                   ADD       1             IN                             *INDICE
269600070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
269700070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
269800070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
269900070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
270000070627E   1C                   ENDIF
270100070627     C*--------------------
270200070627     C* CALCOLO ASSICURAZIONI -->  ASSICURAZIONI = (VARIA "E" + VARIA "R")
270300070627     C*--------------------
270400070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
270500070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
270600070627     C                   Z-ADD     1             P
270700070627     C                   SETOFF                                           28
270800070627     C     'E'           LOOKUP    VAR(P)                                 28
270900070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "I
271000070627     C     P             OCCUR     CAMM
271100070627     C                   ADD       ARI           $VA                            *RICAVI "E"
271200070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "E"
271300070627E   1C                   ENDIF
271400070627     C                   Z-ADD     1             P
271500070627     C                   SETOFF                                           28
271600070627     C     'R'           LOOKUP    VAR(P)                                 28
271700070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "R
271800070627     C     P             OCCUR     CAMM
271900070627     C     $VA           ADD       ARI           $VA                            *RICAVI "R"
272000070627     C     $V2           ADD       ASP           $V2                            *SPEDIZIONI "R"
272100070627E   1C                   ENDIF
272200070627     C*
272300070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
272400070627     C                   XFOOT     $VA           COM$VA           16 3
272500070627     C                   XFOOT     $V2           COM$V2           16 3
272600070627IF  1C     COM$VA        IFNE      *ZEROS
272700070627     C     COM$V2        ORNE      *ZEROS
272800070627IF  2C     DICUI         IFEQ      '0'
272900070627     C                   MOVEL     '1'           DICUI             1
273000070627     C                   ADD       1             IN                             *INDICE
273100070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
273200070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
273300070627E   2C                   ENDIF
273400070627     C*--------------------
273500070627     C* STAMPA  R I C A V I   A S S I C U R A Z I O N I
273600070627     C*--------------------
273700070627     C                   ADD       1             IN                             *INDICE
273800070627     C                   MOVEL     DME(21)       $DM(IN)                        *DESCRIZIONE VALORE
273900070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
274000070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
274100070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
274200070627     C*--------------------
274300070627     C* STAMPA  N U M E R O   A S S I C U R A Z I O N I
274400070627     C*--------------------
274500070627     C                   ADD       1             IN                             *INDICE
274600070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
274700070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
274800070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
274900070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
275000070627E   1C                   ENDIF
275100070627     C*--------------------
275200070627     C* CONTRASSEGNI --> CONTRASSEGNI = VARIA "G"
275300070627     C*--------------------
275400070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
275500070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
275600070627     C                   Z-ADD     1             P
275700070627     C                   SETOFF                                           28
275800070627     C     'G'           LOOKUP    VAR(P)                                 28
275900070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA VARIA "G
276000070627     C     P             OCCUR     CAMM
276100070627     C                   ADD       ARI           $VA                            *RICAVI "G"
276200070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "G"
276300070627E   1C                   ENDIF
276400070627     C*
276500070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
276600070627     C                   XFOOT     $VA           COM$VA           16 3
276700070627     C                   XFOOT     $V2           COM$V2           16 3
276800070627IF  1C     COM$VA        IFNE      *ZEROS
276900070627     C     COM$V2        ORNE      *ZEROS
277000070627IF  2C     DICUI         IFEQ      '0'
277100070627     C                   MOVEL     '1'           DICUI             1
277200070627     C                   ADD       1             IN                             *INDICE
277300070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
277400070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
277500070627E   2C                   ENDIF
277600070627     C*--------------------
277700070627     C* STAMPA  R I C A V I   C O N T R A S S E G N O
277800070627     C*--------------------
277900070627     C                   ADD       1             IN                             *INDICE
278000070627     C                   MOVEL     DME(22)       $DM(IN)                        *DESCRIZIONE VALORE
278100070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
278200070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
278300070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
278400070627     C*--------------------
278500070627     C* STAMPA  N U M E R O   C O N T R A S S E G N O
278600070627     C*--------------------
278700070627     C                   ADD       1             IN                             *INDICE
278800070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
278900070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
279000070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
279100070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
279200070627E   1C                   ENDIF
279300070627     C*--------------------
279400070627     C* CONSEGNE AI PIANI --> VARIA = "P"
279500070627     C*--------------------
279600070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
279700070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
279800070627     C                   Z-ADD     1             P
279900070627     C                   SETOFF                                           28
280000070627     C     'P'           LOOKUP    VAR(P)                                 28
280100070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "P"
280200070627     C     P             OCCUR     CAMM
280300070627     C                   ADD       ARI           $VA                            *RICAVI "P"
280400070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "P"
280500070627E   1C                   ENDIF
280600070627     C*
280700070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
280800070627     C                   XFOOT     $VA           COM$VA           16 3
280900070627     C                   XFOOT     $V2           COM$V2           16 3
281000070627IF  1C     COM$VA        IFNE      *ZEROS
281100070627     C     COM$V2        ORNE      *ZEROS
281200070627IF  2C     DICUI         IFEQ      '0'
281300070627     C                   MOVEL     '1'           DICUI             1
281400070627     C                   ADD       1             IN                             *INDICE
281500070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
281600070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
281700070627E   2C                   ENDIF
281800070627     C*--------------------
281900070627     C* STAMPA  R I C A V I   C O N S E G N A   A I   P I A N I
282000070627     C*--------------------
282100070627     C                   ADD       1             IN                             *INDICE
282200070627     C                   MOVEL     DME(17)       $DM(IN)                        *DESCRIZIONE VALORE
282300070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
282400070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
282500070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
282600070627     C*--------------------
282700070627     C* STAMPA  N U M E R O   C O N S E G N A   A I   P I A N I
282800070627     C*--------------------
282900070627     C                   ADD       1             IN                             *INDICE
283000070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
283100070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
283200070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
283300070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
283400070627E   1C                   ENDIF
283500070627     C*--------------------
283600070627     C* CONSEGNE PER APPUNTAMENTO --> VARIA = "A"
283700070627     C*--------------------
283800070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
283900070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
284000070627     C                   Z-ADD     1             P
284100070627     C                   SETOFF                                           28
284200070627     C     'A'           LOOKUP    VAR(P)                                 28
284300070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "A"
284400070627     C     P             OCCUR     CAMM
284500070627     C                   ADD       ARI           $VA                            *RICAVI "A"
284600070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "A"
284700070627E   1C                   ENDIF
284800070627     C*
284900070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
285000070627     C                   XFOOT     $VA           COM$VA           16 3
285100070627     C                   XFOOT     $V2           COM$V2           16 3
285200070627IF  1C     COM$VA        IFNE      *ZEROS
285300070627     C     COM$V2        ORNE      *ZEROS
285400070627IF  2C     DICUI         IFEQ      '0'
285500070627     C                   MOVEL     '1'           DICUI             1
285600070627     C                   ADD       1             IN                             *INDICE
285700070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
285800070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
285900070627E   2C                   ENDIF
286000070627     C*--------------------
286100070627     C* STAMPA  R I C A V I   C O N S E G N A   P E R   A P P U N T A M E N T O
286200070627     C*--------------------
286300070627     C                   ADD       1             IN                             *INDICE
286400070627     C                   MOVEL     DME(18)       $DM(IN)                        *DESCRIZIONE VALORE
286500070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
286600070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
286700070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
286800070627     C*--------------------
286900070627     C* STAMPA  N U M E R O   C O N S E G N A   P E R   A P P U N T A M E N T O
287000070627     C*--------------------
287100070627     C                   ADD       1             IN                             *INDICE
287200070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
287300070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
287400070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
287500070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
287600070627E   1C                   ENDIF
287700070627     C*--------------------
287800070627     C* CONSEGNE AI SUPERMERCATI --> VARIA = "S"
287900070627     C*--------------------
288000070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
288100070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
288200070627     C                   Z-ADD     1             P
288300070627     C                   SETOFF                                           28
288400070627     C     'S'           LOOKUP    VAR(P)                                 28
288500070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "S"
288600070627     C     P             OCCUR     CAMM
288700070627     C                   ADD       ARI           $VA                            *RICAVI "S"
288800070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "S"
288900070627E   1C                   ENDIF
289000070627     C*
289100070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
289200070627     C                   XFOOT     $VA           COM$VA           16 3
289300070627     C                   XFOOT     $V2           COM$V2           16 3
289400070627IF  1C     COM$VA        IFNE      *ZEROS
289500070627     C     COM$V2        ORNE      *ZEROS
289600070627IF  2C     DICUI         IFEQ      '0'
289700070627     C                   MOVEL     '1'           DICUI             1
289800070627     C                   ADD       1             IN                             *INDICE
289900070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
290000070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
290100070627E   2C                   ENDIF
290200070627     C*--------------------
290300070627     C* STAMPA  R I C A V I   C O N S E G N A   A I   S U P E R M E R C A T I
290400070627     C*--------------------
290500070627     C                   ADD       1             IN                             *INDICE
290600070627     C                   MOVEL     DME(19)       $DM(IN)                        *DESCRIZIONE VALORE
290700070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
290800070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
290900070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
291000070627     C*--------------------
291100070627     C* STAMPA  N U M E R O   C O N S E G N A   A I   S U P E R M E R C A T I
291200070627     C*--------------------
291300070627     C                   ADD       1             IN                             *INDICE
291400070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
291500070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
291600070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
291700070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
291800070627E   1C                   ENDIF
291900070627     C*--------------------
292000070627     C* CONSEGNE FUORI MISURA --> VARIA = "F"
292100070627     C*--------------------
292200070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
292300070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
292400070627     C                   Z-ADD     1             P
292500070627     C                   SETOFF                                           28
292600070627     C     'F'           LOOKUP    VAR(P)                                 28
292700070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "F"
292800070627     C     P             OCCUR     CAMM
292900070627     C                   ADD       ARI           $VA                            *RICAVI "F"
293000070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "F"
293100070627E   1C                   ENDIF
293200070627     C*
293300070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
293400070627     C                   XFOOT     $VA           COM$VA           16 3
293500070627     C                   XFOOT     $V2           COM$V2           16 3
293600070627IF  1C     COM$VA        IFNE      *ZEROS
293700070627     C     COM$V2        ORNE      *ZEROS
293800070627IF  2C     DICUI         IFEQ      '0'
293900070627     C                   MOVEL     '1'           DICUI             1
294000070627     C                   ADD       1             IN                             *INDICE
294100070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
294200070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
294300070627E   2C                   ENDIF
294400070627     C*--------------------
294500070627     C* STAMPA  R I C A V I   C O N S E G N A   F U O R I   M I S U R A
294600070627     C*--------------------
294700070627     C                   ADD       1             IN                             *INDICE
294800070627     C                   MOVEL     DME(20)       $DM(IN)                        *DESCRIZIONE VALORE
294900070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
295000070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
295100070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
295200070627     C*--------------------
295300070627     C* STAMPA  N U M E R O   C O N S E G N A   F U O R I   M I S U R A
295400070627     C*--------------------
295500070627     C                   ADD       1             IN                             *INDICE
295600070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
295700070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
295800070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
295900070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
296000070627E   1C                   ENDIF
296100070627     C*--------------------
296200070627     C* CONSEGNE CENTRO STORICO --> VARIA = "Q"
296300070627     C*--------------------
296400070627     C*                  Z-ADD     *ZEROS        $VA                            *PER I RICAVI
296500070627     C*                  Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
296600070627     C*                  Z-ADD     1             P
296700070627     C*                  SETOFF                                           28
296800070627     C*    'Q'           LOOKUP    VAR(P)                                 28
296900070627IF  1C*    *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "Q"
297000070627     C*    P             OCCUR     CAMM
297100070627     C*                  ADD       ARI           $VA                            *RICAVI "Q"
297200070627     C*                  ADD       ASP           $V2                            *SPEDIZIONI "Q"
297300070627E   1C*                  ENDIF
297400070627     C*
297500070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
297600070627     C*                  XFOOT     $VA           COM$VA           16 3
297700070627     C*                  XFOOT     $V2           COM$V2           16 3
297800070627IF  1C*    COM$VA        IFNE      *ZEROS
297900070627     C*    COM$V2        ORNE      *ZEROS
298000070627IF  2C*    DICUI         IFEQ      '0'
298100070627     C*                  MOVEL     '1'           DICUI             1
298200070627     C*                  ADD       1             IN                             *INDICE
298300070627     C*                  MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
298400070627     C*                  Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
298500070627E   2C*                  ENDIF
298600070627     C*--------------------
298700070627     C* STAMPA  R I C A V I   C O N S E G N A   C E N T R O  S T O R I C O
298800070627     C*--------------------
298900070627     C*                  ADD       1             IN                             *INDICE
299000070627     C*                  MOVEL     DME(31)       $DM(IN)                        *DESCRIZIONE VALORE
299100070627     C*                  Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
299200070627     C*                  Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
299300070627     C*                  Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
299400070627     C*--------------------
299500070627     C* STAMPA  N U M E R O   C O N S E G N A   C E N T R O  S T O R I C O
299600070627     C*--------------------
299700070627     C*                  ADD       1             IN                             *INDICE
299800070627     C*                  MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
299900070627     C*                  Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
300000070627     C*                  Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
300100070627     C*                  Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
300200070627E   1C*                  ENDIF
300300070627     C*--------------------
300400070627     C* CONSEGNE LOCALITA' DISAGIATE --> VARIA = "K"
300500070627     C*--------------------
300600070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
300700070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
300800070627     C                   Z-ADD     1             P
300900070627     C                   SETOFF                                           28
301000070627     C     'K'           LOOKUP    VAR(P)                                 28
301100070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "K"
301200070627     C     P             OCCUR     CAMM
301300070627     C                   ADD       ARI           $VA                            *RICAVI "K"
301400070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "K"
301500070627E   1C                   ENDIF
301600070627     C*
301700070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
301800070627     C                   XFOOT     $VA           COM$VA           16 3
301900070627     C                   XFOOT     $V2           COM$V2           16 3
302000070627IF  1C     COM$VA        IFNE      *ZEROS
302100070627     C     COM$V2        ORNE      *ZEROS
302200070627IF  2C     DICUI         IFEQ      '0'
302300070627     C                   MOVEL     '1'           DICUI             1
302400070627     C                   ADD       1             IN                             *INDICE
302500070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
302600070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
302700070627E   2C                   ENDIF
302800070627     C*--------------------
302900070627     C* STAMPA  R I C A V I   C O N S E G N A   L O C A L I T A'  D I S A G I A T A
303000070627     C*--------------------
303100070627     C                   ADD       1             IN                             *INDICE
303200070627     C                   MOVEL     DME(32)       $DM(IN)                        *DESCRIZIONE VALORE
303300070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
303400070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
303500070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
303600070627     C*--------------------
303700070627     C* STAMPA  N U M E R O   C O N S E G N A   L O C A L I T A'  D I S A G I A T A
303800070627     C*--------------------
303900070627     C                   ADD       1             IN                             *INDICE
304000070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
304100070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
304200070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
304300070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
304400070627E   1C                   ENDIF
304500070627     C*--------------------
304600070627     C* CONSEGNE DDT --> VARIA = "B"
304700070627     C*--------------------
304800070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
304900070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
305000070627     C                   Z-ADD     1             P
305100070627     C                   SETOFF                                           28
305200070627     C     'B'           LOOKUP    VAR(P)                                 28
305300070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "B"
305400070627     C     P             OCCUR     CAMM
305500070627     C                   ADD       ARI           $VA                            *RICAVI "B"
305600070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "B"
305700070627E   1C                   ENDIF
305800070627     C*
305900070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
306000070627     C                   XFOOT     $VA           COM$VA           16 3
306100070627     C                   XFOOT     $V2           COM$V2           16 3
306200070627IF  1C     COM$VA        IFNE      *ZEROS
306300070627     C     COM$V2        ORNE      *ZEROS
306400070627IF  2C     DICUI         IFEQ      '0'
306500070627     C                   MOVEL     '1'           DICUI             1
306600070627     C                   ADD       1             IN                             *INDICE
306700070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
306800070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
306900070627E   2C                   ENDIF
307000070627     C*--------------------
307100070627     C* STAMPA  R I C A V I   C O N S E G N A   D D T
307200070627     C*--------------------
307300070627     C                   ADD       1             IN                             *INDICE
307400070627     C                   MOVEL     DME(33)       $DM(IN)                        *DESCRIZIONE VALORE
307500070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
307600070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
307700070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
307800070627     C*--------------------
307900070627     C* STAMPA  N U M E R O   C O N S E G N A   D D T
308000070627     C*--------------------
308100070627     C                   ADD       1             IN                             *INDICE
308200070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
308300070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
308400070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
308500070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
308600070627E   1C                   ENDIF
308700070627     C*--------------------
308800070627     C* RITIRO --> VARIA = "U"
308900070627     C*--------------------
309000070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
309100070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
309200070627     C                   Z-ADD     1             P
309300070627     C                   SETOFF                                           28
309400070627     C     'U'           LOOKUP    VAR(P)                                 28
309500070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "U"
309600070627     C     P             OCCUR     CAMM
309700070627     C                   ADD       ARI           $VA                            *RICAVI "U"
309800070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "U"
309900070627E   1C                   ENDIF
310000070627     C*
310100070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
310200070627     C                   XFOOT     $VA           COM$VA           16 3
310300070627     C                   XFOOT     $V2           COM$V2           16 3
310400070627IF  1C     COM$VA        IFNE      *ZEROS
310500070627     C     COM$V2        ORNE      *ZEROS
310600070627IF  2C     DICUI         IFEQ      '0'
310700070627     C                   MOVEL     '1'           DICUI             1
310800070627     C                   ADD       1             IN                             *INDICE
310900070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
311000070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
311100070627E   2C                   ENDIF
311200070627     C*--------------------
311300070627     C* STAMPA  R I C A V I   R I T I R O
311400070627     C*--------------------
311500070627     C                   ADD       1             IN                             *INDICE
311600070627     C                   MOVEL     DME(34)       $DM(IN)                        *DESCRIZIONE VALORE
311700070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
311800070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
311900070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
312000070627     C*--------------------
312100070627     C* STAMPA  N U M E R O   R I T I R O
312200070627     C*--------------------
312300070627     C                   ADD       1             IN                             *INDICE
312400070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
312500070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
312600070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
312700070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
312800070627E   1C                   ENDIF
312900070627     C*--------------------
313000070627     C* CONSEGNE ISOLA --> VARIA = "J"
313100070627     C*--------------------
313200070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
313300070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
313400070627     C                   Z-ADD     1             P
313500070627     C                   SETOFF                                           28
313600070627     C     'J'           LOOKUP    VAR(P)                                 28
313700070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "J"
313800070627     C     P             OCCUR     CAMM
313900070627     C                   ADD       ARI           $VA                            *RICAVI "J"
314000070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "J"
314100070627E   1C                   ENDIF
314200070627     C*
314300070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
314400070627     C                   XFOOT     $VA           COM$VA           16 3
314500070627     C                   XFOOT     $V2           COM$V2           16 3
314600070627IF  1C     COM$VA        IFNE      *ZEROS
314700070627     C     COM$V2        ORNE      *ZEROS
314800070627IF  2C     DICUI         IFEQ      '0'
314900070627     C                   MOVEL     '1'           DICUI             1
315000070627     C                   ADD       1             IN                             *INDICE
315100070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
315200070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
315300070627E   2C                   ENDIF
315400070627     C*--------------------
315500070627     C* STAMPA  R I C A V I   C O N S E G N E   I S O L A
315600070627     C*--------------------
315700070627     C                   ADD       1             IN                             *INDICE
315800070627     C                   MOVEL     DME(35)       $DM(IN)                        *DESCRIZIONE VALORE
315900070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
316000070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
316100070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
316200070627     C*--------------------
316300070627     C* STAMPA  N U M E R O   C O N S E G N E   I S O L A
316400070627     C*--------------------
316500070627     C                   ADD       1             IN                             *INDICE
316600070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
316700070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
316800070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
316900070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
317000070627E   1C                   ENDIF
317100070627     C*--------------------
317200070627     C* CONSEGNA DISAGIATA --> VARIA = "X"
317300070627     C*--------------------
317400070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
317500070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
317600070627     C                   Z-ADD     1             P
317700070627     C                   SETOFF                                           28
317800070627     C     'X'           LOOKUP    VAR(P)                                 28
317900070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "X"
318000070627     C     P             OCCUR     CAMM
318100070627     C                   ADD       ARI           $VA                            *RICAVI "X"
318200070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "X"
318300070627E   1C                   ENDIF
318400070627     C*
318500070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
318600070627     C                   XFOOT     $VA           COM$VA           16 3
318700070627     C                   XFOOT     $V2           COM$V2           16 3
318800070627IF  1C     COM$VA        IFNE      *ZEROS
318900070627     C     COM$V2        ORNE      *ZEROS
319000070627IF  2C     DICUI         IFEQ      '0'
319100070627     C                   MOVEL     '1'           DICUI             1
319200070627     C                   ADD       1             IN                             *INDICE
319300070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
319400070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
319500070627E   2C                   ENDIF
319600070627     C*--------------------
319700070627     C* STAMPA  R I C A V I   C O N S E G N E   D I S A G I A T E
319800070627     C*--------------------
319900070627     C                   ADD       1             IN                             *INDICE
320000070627     C                   MOVEL     DME(36)       $DM(IN)                        *DESCRIZIONE VALORE
320100070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
320200070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
320300070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
320400070627     C*--------------------
320500070627     C* STAMPA  N U M E R O   C O N S E G N E  D I S A G I A T E
320600070627     C*--------------------
320700070627     C                   ADD       1             IN                             *INDICE
320800070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
320900070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
321000070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
321100070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
321200070627E   1C                   ENDIF
321300070627     C*--------------------
321400070627     C* RITIRI ANNULLATI --> VARIA = "Y"
321500070627     C*--------------------
321600070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
321700070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
321800070627     C                   Z-ADD     1             P
321900070627     C                   SETOFF                                           28
322000070627     C     'Y'           LOOKUP    VAR(P)                                 28
322100070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "Y"
322200070627     C     P             OCCUR     CAMM
322300070627     C                   ADD       ARI           $VA                            *RICAVI "Y"
322400070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "Y"
322500070627E   1C                   ENDIF
322600070627     C*
322700070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
322800070627     C                   XFOOT     $VA           COM$VA           16 3
322900070627     C                   XFOOT     $V2           COM$V2           16 3
323000070627IF  1C     COM$VA        IFNE      *ZEROS
323100070627     C     COM$V2        ORNE      *ZEROS
323200070627IF  2C     DICUI         IFEQ      '0'
323300070627     C                   MOVEL     '1'           DICUI             1
323400070627     C                   ADD       1             IN                             *INDICE
323500070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
323600070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
323700070627E   2C                   ENDIF
323800070627     C*--------------------
323900070627     C* STAMPA  R I C A V I   R I T I R I  A N N U L L A T I
324000070627     C*--------------------
324100070627     C                   ADD       1             IN                             *INDICE
324200070627     C                   MOVEL     DME(37)       $DM(IN)                        *DESCRIZIONE VALORE
324300070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
324400070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
324500070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
324600070627     C*--------------------
324700070627     C* STAMPA  N U M E R O   R I T I R I  A N N U L L A T I
324800070627     C*--------------------
324900070627     C                   ADD       1             IN                             *INDICE
325000070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
325100070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
325200070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
325300070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
325400070627E   1C                   ENDIF
325500070627     C*--------------------
325600070627     C* RICERCA D'ARCHIVIO --> VARIA = "O"
325700070627     C*--------------------
325800070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
325900070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
326000070627     C                   Z-ADD     1             P
326100070627     C                   SETOFF                                           28
326200070627     C     'O'           LOOKUP    VAR(P)                                 28
326300070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "O"
326400070627     C     P             OCCUR     CAMM
326500070627     C                   ADD       ARI           $VA                            *RICAVI "O"
326600070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "O"
326700070627E   1C                   ENDIF
326800070627     C*
326900070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
327000070627     C                   XFOOT     $VA           COM$VA           16 3
327100070627     C                   XFOOT     $V2           COM$V2           16 3
327200070627IF  1C     COM$VA        IFNE      *ZEROS
327300070627     C     COM$V2        ORNE      *ZEROS
327400070627IF  2C     DICUI         IFEQ      '0'
327500070627     C                   MOVEL     '1'           DICUI             1
327600070627     C                   ADD       1             IN                             *INDICE
327700070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
327800070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
327900070627E   2C                   ENDIF
328000070627     C*--------------------
328100070627     C* STAMPA  R I C A V I   R I C E R C H E  D' A R C H I V I O
328200070627     C*--------------------
328300070627     C                   ADD       1             IN                             *INDICE
328400070627     C                   MOVEL     DME(38)       $DM(IN)                        *DESCRIZIONE VALORE
328500070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
328600070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
328700070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
328800070627     C*--------------------
328900070627     C* STAMPA  N U M E R O   R I C E R C H E  D' A R C H I V I O
329000070627     C*--------------------
329100070627     C                   ADD       1             IN                             *INDICE
329200070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
329300070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
329400070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
329500070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
329600070627E   1C                   ENDIF
329700070627     C*--------------------
329800070627     C* P.O.D. IMAGE       --> VARIA = "a"
329900070627     C*--------------------
330000070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
330100070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
330200070627     C                   Z-ADD     1             P
330300070627     C                   SETOFF                                           28
330400070627     C     'a'           LOOKUP    VAR(P)                                 28
330500070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "O"
330600070627     C     P             OCCUR     CAMM
330700070627     C                   ADD       ARI           $VA                            *RICAVI "O"
330800070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "O"
330900070627E   1C                   ENDIF
331000070627     C*
331100070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
331200070627     C                   XFOOT     $VA           COM$VA           16 3
331300070627     C                   XFOOT     $V2           COM$V2           16 3
331400070627IF  1C     COM$VA        IFNE      *ZEROS
331500070627     C     COM$V2        ORNE      *ZEROS
331600070627IF  2C     DICUI         IFEQ      '0'
331700070627     C                   MOVEL     '1'           DICUI             1
331800070627     C                   ADD       1             IN                             *INDICE
331900070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
332000070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
332100070627E   2C                   ENDIF
332200070627     C*--------------------
332300070627     C* STAMPA  R I C A V I   P.O.D. IMMAGE
332400070627     C*--------------------
332500070627     C                   ADD       1             IN                             *INDICE
332600070627     C                   MOVEL     DME(44)       $DM(IN)                        *DESCRIZIONE VALORE
332700070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
332800070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
332900070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
333000070627     C*--------------------
333100070627     C* STAMPA  N U M E R O   P.O.D. IMMAGE
333200070627     C*--------------------
333300070627     C                   ADD       1             IN                             *INDICE
333400070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
333500070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
333600070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
333700070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
333800070627E   1C                   ENDIF
333900070627     C*--------------------
334000070627     C* DIRITTI DI PESATURA --> VARIA = "5"
334100070627     C*--------------------
334200070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
334300070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
334400070627     C                   Z-ADD     1             P
334500070627     C                   SETOFF                                           28
334600070627     C     '5'           LOOKUP    VAR(P)                                 28
334700070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "5"
334800070627     C     P             OCCUR     CAMM
334900070627     C                   ADD       ARI           $VA                            *RICAVI "5"
335000070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "5"
335100070627E   1C                   ENDIF
335200070627     C*
335300070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
335400070627     C                   XFOOT     $VA           COM$VA           16 3
335500070627     C                   XFOOT     $V2           COM$V2           16 3
335600070627IF  1C     COM$VA        IFNE      *ZEROS
335700070627     C     COM$V2        ORNE      *ZEROS
335800070627IF  2C     DICUI         IFEQ      '0'
335900070627     C                   MOVEL     '1'           DICUI             1
336000070627     C                   ADD       1             IN                             *INDICE
336100070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
336200070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
336300070627E   2C                   ENDIF
336400070627     C*--------------------
336500070627     C* STAMPA  R I C A V I   D I R I T T I  D I  P E S A T U R A
336600070627     C*--------------------
336700070627     C                   ADD       1             IN                             *INDICE
336800070627     C                   MOVEL     DME(39)       $DM(IN)                        *DESCRIZIONE VALORE
336900070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
337000070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
337100070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
337200070627     C*--------------------
337300070627     C* STAMPA  N U M E R O   D I R I T T I  D I  P E S A T U R A
337400070627     C*--------------------
337500070627     C                   ADD       1             IN                             *INDICE
337600070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
337700070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
337800070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
337900070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
338000070627E   1C                   ENDIF
338100070627     C*--------------------
338200070627     C* LASCIATO AVVISO --> VARIA = "#"
338300070627     C*--------------------
338400070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
338500070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
338600070627     C                   Z-ADD     1             P
338700070627     C                   SETOFF                                           28
338800070627     C     '#'           LOOKUP    VAR(P)                                 28
338900070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "B"
339000070627     C     P             OCCUR     CAMM
339100070627     C                   ADD       ARI           $VA                            *RICAVI "B"
339200070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "B"
339300070627E   1C                   ENDIF
339400070627     C*
339500070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
339600070627     C                   XFOOT     $VA           COM$VA           16 3
339700070627     C                   XFOOT     $V2           COM$V2           16 3
339800070627IF  1C     COM$VA        IFNE      *ZEROS
339900070627     C     COM$V2        ORNE      *ZEROS
340000070627IF  2C     DICUI         IFEQ      '0'
340100070627     C                   MOVEL     '1'           DICUI             1
340200070627     C                   ADD       1             IN                             *INDICE
340300070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
340400070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
340500070627E   2C                   ENDIF
340600070627     C*--------------------
340700070627     C* STAMPA  R I C A V I   LASCIATO AVVISO
340800070627     C*--------------------
340900070627     C                   ADD       1             IN                             *INDICE
341000070627     C                   MOVEL     DME(43)       $DM(IN)                        *DESCRIZIONE VALORE
341100070627     C                   MOVE      '    '        $DM(IN)                        *DESCRIZIONE VALORE
341200070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
341300070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
341400070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
341500070627     C*--------------------
341600070627     C* STAMPA  N U M E R O   LASCIATO AVVISO
341700070627     C*--------------------
341800070627     C                   ADD       1             IN                             *INDICE
341900070627     C******             MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
342000070627     C                   move      ' Nr.'        $DM(IN)                        *DESCRIZIONE VALORE
342100070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
342200070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
342300070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
342400070627E   1C                   ENDIF
342500070627     C*--------------------
342600070627     C* ANN.PORTO ASSEGNATO --> VARIA = "&"
342700070627     C*--------------------
342800070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
342900070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
343000070627     C                   Z-ADD     1             P
343100070627     C                   SETOFF                                           28
343200070627     C     '&'           LOOKUP    VAR(P)                                 28
343300070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "&"
343400070627     C     P             OCCUR     CAMM
343500070627     C                   ADD       ARI           $VA                            *RICAVI "&"
343600070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "&"
343700070627E   1C                   ENDIF
343800070627     C*
343900070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
344000070627     C                   XFOOT     $VA           COM$VA           16 3
344100070627     C                   XFOOT     $V2           COM$V2           16 3
344200070627IF  1C     COM$VA        IFNE      *ZEROS
344300070627     C     COM$V2        ORNE      *ZEROS
344400070627IF  2C     DICUI         IFEQ      '0'
344500070627     C                   MOVEL     '1'           DICUI             1
344600070627     C                   ADD       1             IN                             *INDICE
344700070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
344800070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
344900070627E   2C                   ENDIF
345000070627     C*--------------------
345100070627     C* STAMPA  R I C A V I   ANN.PORTO ASSEGNATO
345200070627     C*--------------------
345300070627     C                   ADD       1             IN                             *INDICE
345400070627     C                   MOVEL     DME(45)       $DM(IN)                        *DESCRIZIONE VALORE
345500070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
345600070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
345700070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
345800070627     C*--------------------
345900070627     C* STAMPA  N U M E R O   ANN.PORTO ASSEGNATO
346000070627     C*--------------------
346100070627     C                   ADD       1             IN                             *INDICE
346200070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
346300070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
346400070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
346500070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
346600070627E   1C                   ENDIF
346700070627     C*--------------------
346800070627     C* DIROTTAMENTI        --> VARIA = "*"
346900070627     C*--------------------
347000070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
347100070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
347200070627     C                   Z-ADD     1             P
347300070627     C                   SETOFF                                           28
347400070627     C     '*'           LOOKUP    VAR(P)                                 28
347500070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "*"
347600070627     C     P             OCCUR     CAMM
347700070627     C                   ADD       ARI           $VA                            *RICAVI "*"
347800070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "*"
347900070627E   1C                   ENDIF
348000070627     C*
348100070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
348200070627     C                   XFOOT     $VA           COM$VA           16 3
348300070627     C                   XFOOT     $V2           COM$V2           16 3
348400070627IF  1C     COM$VA        IFNE      *ZEROS
348500070627     C     COM$V2        ORNE      *ZEROS
348600070627IF  2C     DICUI         IFEQ      '0'
348700070627     C                   MOVEL     '1'           DICUI             1
348800070627     C                   ADD       1             IN                             *INDICE
348900070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
349000070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
349100070627E   2C                   ENDIF
349200070627     C*--------------------
349300070627     C* STAMPA  R I C A V I    DIROTTAMENTI
349400070627     C*--------------------
349500070627     C                   ADD       1             IN                             *INDICE
349600070627     C                   MOVEL     DME(46)       $DM(IN)                        *DESCRIZIONE VALORE
349700070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
349800070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
349900070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
350000070627     C*--------------------
350100070627     C* STAMPA  N U M E R O    DIROTTAMENTI
350200070627     C*--------------------
350300070627     C                   ADD       1             IN                             *INDICE
350400070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
350500070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
350600070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
350700070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
350800070627E   1C                   ENDIF
350900070627     C*--------------------
351000070627     C* RITIRO BANCALI      --> VARIA = "="
351100070627     C*--------------------
351200070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
351300070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
351400070627     C                   Z-ADD     1             P
351500070627     C                   SETOFF                                           28
351600070627     C     '='           LOOKUP    VAR(P)                                 28
351700070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "="
351800070627     C     P             OCCUR     CAMM
351900070627     C                   ADD       ARI           $VA                            *RICAVI "="
352000070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "="
352100070627E   1C                   ENDIF
352200070627     C*
352300070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
352400070627     C                   XFOOT     $VA           COM$VA           16 3
352500070627     C                   XFOOT     $V2           COM$V2           16 3
352600070627IF  1C     COM$VA        IFNE      *ZEROS
352700070627     C     COM$V2        ORNE      *ZEROS
352800070627IF  2C     DICUI         IFEQ      '0'
352900070627     C                   MOVEL     '1'           DICUI             1
353000070627     C                   ADD       1             IN                             *INDICE
353100070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
353200070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
353300070627E   2C                   ENDIF
353400070627     C*--------------------
353500070627     C* STAMPA  R I C A V I    RITIRO BANCALI
353600070627     C*--------------------
353700070627     C                   ADD       1             IN                             *INDICE
353800070627     C                   MOVEL     DME(47)       $DM(IN)                        *DESCRIZIONE VALORE
353900070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
354000070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
354100070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
354200070627     C*--------------------
354300070627     C* STAMPA  N U M E R O    RITIRO BANCALI
354400070627     C*--------------------
354500070627     C                   ADD       1             IN                             *INDICE
354600070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
354700070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
354800070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
354900070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
355000070627E   1C                   ENDIF
355100070627     C*--------------------
355200070627     C* REC.C/ASS.          --> VARIA = "W"
355300070627     C*--------------------
355400070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
355500070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
355600070627     C                   Z-ADD     1             P
355700070627     C                   SETOFF                                           28
355800070627     C     'W'           LOOKUP    VAR(P)                                 28
355900070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "W"
356000070627     C     P             OCCUR     CAMM
356100070627     C                   ADD       ARI           $VA                            *RICAVI "W"
356200070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "W"
356300070627E   1C                   ENDIF
356400070627     C*
356500070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
356600070627     C                   XFOOT     $VA           COM$VA           16 3
356700070627     C                   XFOOT     $V2           COM$V2           16 3
356800070627IF  1C     COM$VA        IFNE      *ZEROS
356900070627     C     COM$V2        ORNE      *ZEROS
357000070627IF  2C     DICUI         IFEQ      '0'
357100070627     C                   MOVEL     '1'           DICUI             1
357200070627     C                   ADD       1             IN                             *INDICE
357300070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
357400070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
357500070627E   2C                   ENDIF
357600070627     C*--------------------
357700070627     C* STAMPA  R I C A V I    REC.C/ASS.
357800070627     C*--------------------
357900070627     C                   ADD       1             IN                             *INDICE
358000070627     C                   MOVEL     DME(48)       $DM(IN)                        *DESCRIZIONE VALORE
358100070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
358200070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
358300070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
358400070627     C*--------------------
358500070627     C* STAMPA  N U M E R O    REC.C/ASS.
358600070627     C*--------------------
358700070627     C                   ADD       1             IN                             *INDICE
358800070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
358900070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
359000070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
359100070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
359200070627E   1C                   ENDIF
359300070627     C*--------------------
359400070627     C* ORM COMMISSIONATO   --> VARIA = "T"
359500070627     C*--------------------
359600070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
359700070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
359800070627     C                   Z-ADD     1             P
359900070627     C                   SETOFF                                           28
360000070627     C     'T'           LOOKUP    VAR(P)                                 28
360100070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "W"
360200070627     C     P             OCCUR     CAMM
360300070627     C                   ADD       ARI           $VA                            *RICAVI "W"
360400070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "W"
360500070627E   1C                   ENDIF
360600070627     C*
360700070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
360800070627     C                   XFOOT     $VA           COM$VA           16 3
360900070627     C                   XFOOT     $V2           COM$V2           16 3
361000070627IF  1C     COM$VA        IFNE      *ZEROS
361100070627     C     COM$V2        ORNE      *ZEROS
361200070627IF  2C     DICUI         IFEQ      '0'
361300070627     C                   MOVEL     '1'           DICUI             1
361400070627     C                   ADD       1             IN                             *INDICE
361500070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
361600070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
361700070627E   2C                   ENDIF
361800070627     C*--------------------
361900070627     C* STAMPA  R I C A V I    ORM COMMISSIONATO
362000070627     C*--------------------
362100070627     C                   ADD       1             IN                             *INDICE
362200070627     C                   MOVEL     DME(49)       $DM(IN)                        *DESCRIZIONE VALORE
362300070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
362400070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
362500070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
362600070627     C*--------------------
362700070627     C* STAMPA  N U M E R O    ORM COMMISSIONATO
362800070627     C*--------------------
362900070627     C                   ADD       1             IN                             *INDICE
363000070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
363100070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
363200070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
363300070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
363400070627E   1C                   ENDIF
363500070627     C*
363600070627     C*--------------------
363700070627     C* L.AVVISO            --> VARIA = "c"
363800070627     C*--------------------
363900070627     C                   Z-ADD     *ZEROS        $VA                            *PER I RICAVI
364000070627     C                   Z-ADD     *ZEROS        $V2                            *PER LE SPEDIZIONI
364100070627     C                   Z-ADD     1             P
364200070627     C                   SETOFF                                           28
364300070627     C     'c'           LOOKUP    VAR(P)                                 28
364400070627IF  1C     *IN28         IFEQ      *ON                                          *MEMORIZ.TA CONS "c"
364500070627     C     P             OCCUR     CAMM
364600070627     C                   ADD       ARI           $VA                            *RICAVI "c"
364700070627     C                   ADD       ASP           $V2                            *SPEDIZIONI "c"
364800070627E   1C                   ENDIF
364900070627     C*
365000070627     C* STAMPO RIGA SOLO SE HO MEMORIZZATO QC.
365100070627     C                   XFOOT     $VA           COM$VA           16 3
365200070627     C                   XFOOT     $V2           COM$V2           16 3
365300070627IF  1C     COM$VA        IFNE      *ZEROS
365400070627     C     COM$V2        ORNE      *ZEROS
365500070627IF  2C     DICUI         IFEQ      '0'
365600070627     C                   MOVEL     '1'           DICUI             1
365700070627     C                   ADD       1             IN                             *INDICE
365800070627     C                   MOVEL     DME(16)       $DM(IN)                        *DESCRIZIONE VALORE
365900070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
366000070627E   2C                   ENDIF
366100070627     C*--------------------
366200070627     C* STAMPA  R I C A V I    L.AVVISO
366300070627     C*--------------------
366400070627     C                   ADD       1             IN                             *INDICE
366500070627     C                   MOVEL     DME(50)       $DM(IN)                        *DESCRIZIONE VALORE
366600070627     C                   Z-ADD     $VA(01)       $VM(IN)                        *VALORI MESE
366700070627     C                   Z-ADD     $VA(02)       $TM(IN)                        *VALORE TOTALE
366800070627     C                   Z-ADD     2             $EM(IN)                        *NUMERO DI DECIMALI
366900070627     C*--------------------
367000070627     C* STAMPA  N U M E R O    L.AVVISO
367100070627     C*--------------------
367200070627     C                   ADD       1             IN                             *INDICE
367300070627     C                   MOVEL     DME(15)       $DM(IN)                        *DESCRIZIONE VALORE
367400070627     C                   Z-ADD     $V2(01)       $VM(IN)                        *VALORI MESE
367500070627     C                   Z-ADD     $V2(02)       $TM(IN)                        *VALORE TOTALE
367600070627     C                   Z-ADD     *ZEROS        $EM(IN)                        *NUMERO DI DECIMALI
367700070627E   1C                   ENDIF
367800070627     C*
367900070627     C                   ENDSR
368000070627     C*------------------------------------------------------------------------*
368100070627     C*?VIDPEV - IMPOSTA VIDEO STATISTICA "PESI VOLUMI"
368200070627     C*------------------------------------------------------------------------*
368300050113     C     VIDPEV        BEGSR
368400050113     C*
368500050113     C                   Z-ADD     *ZEROS        IN                5 0          *INDICE DI SCHIERA
368600050113     C*
368700050113     C* Pesi (1) e Volumi (2)
368800050113DO  1C     1             DO        2             J
368900050113     C*
369000050113     C     J             OCCUR     Cpev
369100050113     C*
369200050113     C                   ADD       1             IN                             *INDICE
369300050119     C                   MOVEL     DPEV(J)       $Dp(IN)                        *DESCRIZIONE VALORE
369400050113     C                   Z-ADD     *zeros        $Ep(IN)                        *NUMERO DI DECIMALI
369500050121     C*
369600050121     c                   if        D60DPV = 'S'
369700050121     C*  Nr.Colli VDL
369800050121     C                   ADD       1             IN                             *INDICE
369900050121     C                   MOVEL     DPEV(3)       $Dp(IN)                        *DESCRIZIONE VALORE
370000050121     C                   Z-ADD     NCV(1)        $Vp(IN)                        *VALORE MESE
370100050121     C                   Z-ADD     NCV(2)        $Tp(IN)                        *VALORE TOTALE
370200050121     C                   Z-ADD     *zeros        $Ep(IN)                        *NUMERO DI DECIMALI
370300050121     C*  Nr.Colli Boll.
370400050121     C                   ADD       1             IN                             *INDICE
370500050121     C                   MOVEL     DPEV(4)       $Dp(IN)                        *DESCRIZIONE VALORE
370600050121     C                   Z-ADD     NCB(1)        $Vp(IN)                        *VALORE MESE
370700050121     C                   Z-ADD     NCB(2)        $Tp(IN)                        *VALORE TOTALE
370800050121     C                   Z-ADD     *zeros        $Ep(IN)                        *NUMERO DI DECIMALI
370900050121     C*
371000050121     c                   end
371100050113     C* % Colli Rilev.
371200050113     C                   ADD       1             IN                             *INDICE
371300050119     C                   MOVEL     DPEV(5)       $Dp(IN)                        *DESCRIZIONE VALORE
371400050113     C                   Z-ADD     NCB(01)       WFAW             16 3          *Nr.Colli VDL
371500050113     C                   Z-ADD     NCV(01)       WNRW             16 3          *Nr.Colli Boll.
371600050113     C                   Z-ADD     *ZEROS        WPRW              4 1          *PERCENTUALE
371700050113     C                   EXSR      CALPER
371800050113     C                   Z-ADD     WPRW          $Vp(IN)                        *PERCENTUALE MESE
371900050113      *
372000050113     C                   Z-ADD     NCB(02)       WFAW             16 3          *Nr.Colli VDL
372100050113     C                   Z-ADD     NCV(02)       WNRW             16 3          *Nr.Colli Boll.
372200050113     C                   Z-ADD     *ZEROS        WPRW              4 1          *PERCENTUALE
372300050113     C                   EXSR      CALPER
372400050113     C                   Z-ADD     WPRW          $Tp(IN)                        *PERCENTUALE MESE
372500050113     C                   Z-ADD     1             $Ep(IN)                        *NUMERO DI DECIMALI
372600050119     C************
372700050119     c                   if        D60DPV = 'S'
372800050121     C*       VDL
372900050121     C                   ADD       1             IN                             *INDICE
373000050121     C                   MOVEL     DPEV(7)       $Dp(IN)                        *DESCRIZIONE VALORE
373100050121     C                   Z-ADD     PVV(1)        $Vp(IN)                        *VALORE MESE
373200050121     C                   Z-ADD     PVV(2)        $Tp(IN)                        *VALORE TOTALE
373300050121     C                   Z-ADD     1             $Ep(IN)                        *NUMERO DI DECIMALI
373400050121     c                   if        j =2
373500050121     C                   Z-ADD     3             $Ep(IN)                        *NUMERO DI DECIMALI
373600050121     c                   end
373700050121     C*       Boll.
373800050121     C                   ADD       1             IN                             *INDICE
373900050121     C                   MOVEL     DPEV(8)       $Dp(IN)                        *DESCRIZIONE VALORE
374000050121     c                   Z-ADD     PVB(1)        $Vp(IN)                        *VALORE MESE
374100050121     C                   Z-ADD     PVB(2)        $Tp(IN)                        *VALORE TOTALE
374200050121     C                   Z-ADD     1             $Ep(IN)                        *NUMERO DI DECIMALI
374300050121     c                   if        j =2
374400050121     C                   Z-ADD     3             $Ep(IN)                        *NUMERO DI DECIMALI
374500050121     c                   end
374600050119     c                   endIF
374700050119     C************
374800050113     C* % Scost Ril./Boll.
374900050113     C                   ADD       1             IN                             *INDICE
375000050119     C                   MOVEL     DPEV(6)       $Dp(IN)                        *DESCRIZIONE VALORE
375100050113     C                   Z-ADD     PVB(01)       WRRW             16 3          *Peso Boll.
375200050113     C                   Z-ADD     PVV(01)       WRPW             16 3          *Peso VDL
375300050113     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
375400050113     C                   EXSR      CALDEL
375500050113     C                   Z-ADD     WDPW          $Vp(IN)                        *DELTA DEL MESE
375600050113      *
375700050113     C                   Z-ADD     PVB(02)       WRRW             16 3          *Peso Boll.
375800050113     C                   Z-ADD     PVV(02)       WRPW             16 3          *Peso VDL
375900050113     C                   Z-ADD     *ZEROS        WDPW              4 1          *DELTA PERCENTUALE
376000050113     C                   EXSR      CALDEL
376100050113     C                   Z-ADD     WDPW          $Tp(IN)                        *DELTA DEL TOTALE
376200050113     C                   Z-ADD     1             $Ep(IN)                        *NUMERO DI DECIMALI
376300050113     C*
376400050113     C                   ENDDO
376500050113     C*
376600050113     C                   ENDSR
376700000000     C*------------------------------------------------------------------------*
376800000000     C*?VIDREG - IMPOSTA VIDEO STATISTICA "REGIONE-REGIONE"
376900000000     C*------------------------------------------------------------------------*
377000000000     C     VIDREG        BEGSR
377100000000     C*
377200950726     C                   Z-ADD     *ZEROS        IN                             *INDICE DI SCHIERA
377300950206     C*
377400950206     C* PER OGNI TIPO DI VALORE RICHIESTO ( O UNO O TUTTI)
377500950206DO  1C     1             DO        5             JJ
377600950206     C*
377700950206IF  2C     D60RIF        IFEQ      *BLANKS                                      *TUTTI
377800950206     C     D60RIF        OREQ      VRR(JJ)                                      *VALORE = SELEZIONAT
377900950206     C*
378000950206     C                   ADD       1             IN                             *INDICE
378100950206     C                   MOVEL     DVRR(JJ)      $DR(IN)                        *RIGA DI SEPARAZIONE
378200950206     C                   Z-ADD     *ZEROS        $ER(IN)                        *NUMERO DI DECIMALI
378300000000     C*
378400000000     C* VISUALIZZO PRIMA I RAGGRUPPAMENTI REGIONE PARTENZA E POI GLI ARRIVO
378500020605     C***                Z-ADD     *ZEROS        E                 4 0
378600020605DO  3C***  1             DO        2             E
378700000000     C*
378800020605IF  4C***  E             IFEQ      1
378900020605     C***                MOVEL     'P'           CICLO             1            *PRIMA LA PARTENZA
379000020605X   4C***                ELSE
379100020605     C***                MOVEL     'A'           CICLO                          *POI L'ARRIVO
379200020605E   4C***                ENDIF
379300020605     C*
379400020605     C* ORDINO L'OCCUR "REGIONE-REGIONE" X REGIONE P/A
379500020605     C                   EXSR      ORDCRER
379600000000     C*
379700000000     C* PER OGNI COMBINAZIONE DI REGIONI VALORIZZATA
379800000000     C                   Z-ADD     1             J
379900950206DO  4C     1             DO        R             J
380000000000     C     J             OCCUR     CRER
380100020605     C*
380200020605     C                   IF        REX <> *BLANKS
380300000000     C*
380400000000     C* CONTROLLO COSA ELABORARE (P/A)
380500020605     C***  1             SUBST     REG(J):1      ALF1              1
380600020605IF  5C***  CICLO         IFEQ      ALF1
380700020605     C***                MOVE      REG(J)        WRER                           *...+...
380800020605     C                   MOVE      REX           WRER                           *...+...
380900000000     C*
381000000000     C* IMPOSTA IL CAMPO DECODIFICA DEI DUE RAGGRUPPAMENTI
381100000000     C                   MOVEL     WRER          PRIMO             3 0          *1° RAGGRUPPAMENTO
381200000000     C                   Z-ADD     1             I
381300000000     C                   SETOFF                                           28
381400000000     C     PRIMO         LOOKUP    CRR(I)                                 28
381500950206IF  6C     *IN28         IFEQ      *ON
381600000000     C                   MOVEL     DCRR(I)       DPRIMO           11            *TROVATO
381700950206X   6C                   ELSE
381800000000     C                   MOVEL     *ALL'-'       DPRIMO                         *NON TROVATO
381900950206E   6C                   ENDIF
382000000000     C*
382100000000     C                   MOVE      WRER          SECON             3 0          *2° RAGGRUPPAMENTO
382200000000     C                   Z-ADD     1             I
382300000000     C                   SETOFF                                           28
382400000000     C     SECON         LOOKUP    CRR(I)                                 28
382500950206IF  6C     *IN28         IFEQ      *ON
382600000000     C                   MOVEL     DCRR(I)       DSECON           11            *TROVATO
382700950206X   6C                   ELSE
382800000000     C                   MOVEL     *ALL'-'       DSECON                         *NON TROVATO
382900950206E   6C                   ENDIF
383000950726     C*
383100950726     C     DPRIMO        CAT       '-'           RAGRAG           23
383200950726     C                   MOVE      DSECON        RAGRAG                         *REG-REG
383300000000     C*--------------------
383400000000     C* VALORIZZA  R A G G R U P P A M E N T O
383500000000     C*--------------------
383600950206     C                   ADD       1             IN                             *INDICE
383700950206     C                   MOVEL     RAGRAG        $DR(IN)                        *DESCRIZIONE VALORE
383800950206     C*
383900950206IF  6C     VRR(JJ)       IFEQ      'C'
384000950206     C                   Z-ADD     REC(01)       $VR(IN)                        *COLLI MESE
384100950206     C                   Z-ADD     REC(02)       $TR(IN)                        *COLLI TOTALI
384200950206E   6C                   ENDIF
384300950206IF  6C     VRR(JJ)       IFEQ      'P'
384400950206     C                   Z-ADD     REP(01)       $VR(IN)                        *PESO MESE
384500950206     C                   Z-ADD     REP(02)       $TR(IN)                        *PESO TOTALE
384600950206E   6C                   ENDIF
384700950206IF  6C     VRR(JJ)       IFEQ      'R'
384800950206     C                   Z-ADD     RER(01)       $VR(IN)                        *RICAVO MESE
384900950206     C                   Z-ADD     RER(02)       $TR(IN)                        *RICAVO TOTALE
385000950206E   6C                   ENDIF
385100950206IF  6C     VRR(JJ)       IFEQ      'S'
385200950206     C                   Z-ADD     RES(01)       $VR(IN)                        *SPEDIZIONI MESE
385300950206     C                   Z-ADD     RES(02)       $TR(IN)                        *SPEDIZIONI TOTALI
385400950206E   6C                   ENDIF
385500950206IF  6C     VRR(JJ)       IFEQ      'V'
385600950206     C                   Z-ADD     REV(01)       $VR(IN)                        *VOLUME MESE
385700950206     C                   Z-ADD     REV(02)       $TR(IN)                        *VOLUME TOTALI
385800950206E   6C                   ENDIF
385900000000     C*
386000950206IF  6C     VRR(JJ)       IFEQ      'V'                                          *VALORE = VOLUME
386100000000     C                   Z-ADD     3             $ER(IN)                        *NUMERO DI DECIMALI
386200010920X   6C                   ELSE
386300010920IF  7C     VRR(JJ)       IFEQ      'R'                                          *VALORE = RICAVI
386400010920     C                   Z-ADD     2             $ER(IN)                        *NUMERO DI DECIMALI
386500010920X   7C                   ELSE                                                   *VALORE NO VOL E NO RIC
386600000000     C                   Z-ADD     *ZEROS        $ER(IN)                        *NUMERO DI DECIMALI
386700010920E   7C                   ENDIF
386800010920E   6C                   ENDIF
386900000000     C*
387000020605E   5C***                ENDIF
387100020605     C                   ENDIF
387200950206E   4C                   ENDDO
387300000000     C*
387400020605E   3C***                ENDDO
387500950206     C*
387600950206E   2C                   ENDIF
387700950206E   1C                   ENDDO
387800000000     C*
387900950206     C                   ADD       1             IN                             *INDICE
388000000000     C                   MOVEL     *ALL'-'       $DR(IN)                        *RIGA DI SEPARAZIONE
388100000000     C                   Z-ADD     *ZEROS        $ER(IN)                        *NUMERO DI DECIMALI
388200000000     C*
388300000000     C                   ENDSR
388400000000     C*------------------------------------------------------------------------*
388500000000     C*?VIDSER - IMPOSTA VIDEO STATISTICA "TOTALI"
388600000000     C*------------------------------------------------------------------------*
388700000000     C     VIDSER        BEGSR
388800000000     C*
388900950726     C                   Z-ADD     *ZEROS        IN                             *INDICE DI SCHIERA
389000000000     C*
389100000000     C* PER OGNI VALORE DA VISUALIZZARE
389200000000     C                   Z-ADD     1             J
389300000000DO  1C     1             DO        5             J
389400000000     C*
389500000000     C* SOMMO TUTTI I TIPI SERVIZIO NELLA SCHIERA MULTIPLA
389600000000     C     J             OCCUR     CSER
389700000000     C     J             OCCUR     WSER
389800000000     C                   Z-ADD     SNC           VAW                            CORRIERE +
389900000629     C     VAW           ADD       SDC           VAW                            DISTRIBUZIONE +
390000000629     C     VAW           ADD       SEC           VAW                            ESPRESSO +
390100091013     C     VAW           ADD       SHC           VAW                            H10:30   +
390200000629     C     VAW           ADD       SPC           VAW                            POSTE = VALORE TOTAL
390300000000     C*
390400000000     C* METTO I TOTALI DEI SINGOLI VALORI IN SCHIERA (PER LE MEDIE)
390500000000IF  2C     J             IFEQ      1
390600000000     C                   Z-ADD     VAW           SPW                            *SPEDIZIONI
390700000000E   2C                   ENDIF
390800000000IF  2C     J             IFEQ      2
390900000000     C                   Z-ADD     VAW           COW                            *COLLI
391000000000E   2C                   ENDIF
391100000000IF  2C     J             IFEQ      3
391200000000     C                   Z-ADD     VAW           PEW                            *PESO IN Q.LI
391300000000E   2C                   ENDIF
391400000000IF  2C     J             IFEQ      4
391500000000     C                   Z-ADD     VAW           VOW                            *VOLUME IN M.CUBI
391600000000E   2C                   ENDIF
391700000000IF  2C     J             IFEQ      5
391800000000     C                   Z-ADD     VAW           RIW                            *RICAVI DIVISO 1000
391900000000E   2C                   ENDIF
392000000000     C*
392100000000E   1C                   ENDDO
392200000000     C*
392300000000     C* PER OGNI TIPO VALORE DA VISUAL.RE (SPEDIZIONI/COLLI/PESO/VOLUME/RICAVI)
392400000000     C                   Z-ADD     1             J
392500000000DO  1C     1             DO        5             J
392600000000     C     J             OCCUR     CSER
392700000000     C*
392800000000     C                   MOVEL     '0'           NODESV            1            *SI DESCRIZ VALORE
392900000000     C                   MOVEL     DCA(J)        DPRIMO                         *DESCRIZIONE VALORE
393000000000     C*
393100000000     C* PER OGNI TIPO SERVIZIO SELEZIONATO
393200000000     C                   Z-ADD     1             I
393300000000DO  2C     1             DO        SPARS         I
393400000000     C*
393500000000IF  3C     I             IFGE      2                                            *TIPO VALORE 1 VOLTA
393600000000     C     NODESV        ANDEQ     '1'                                          *SE SCRITTO 1 VOLTA
393700000000     C                   MOVEL     *BLANKS       DPRIMO
393800000000E   3C                   ENDIF
393900000000     C*
394000000000     C* DESCRIZIONE TIPO SERVIZIO
394100000000     C                   Z-ADD     1             JJ
394200000000     C                   SETOFF                                           28
394300000000     C     SERS(I)       LOOKUP    SER(JJ)                                28
394400000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
394500000000     C                   MOVEL     *BLANKS       DSECON
394600000000X   3C                   ELSE
394700000000     C                   MOVEL     DSER(JJ)      DSECON                         *DESCRIZIONE SERVIZI
394800000000E   3C                   ENDIF
394900000000     C*
395000000000IF  3C     SERS(I)       IFEQ      'C'
395100000000     C                   Z-ADD     SNC           $VA                            *CORRIERE
395200000000X   3C                   ELSE
395300000000IF  4C     SERS(I)       IFEQ      'E'
395400000000     C                   Z-ADD     SEC           $VA                            *ESPRESSO
395500000000X   4C                   ELSE
395600000629IF  5C     SERS(I)       IFEQ      'D'
395700000629     C                   Z-ADD     SDC           $VA                            *DISRIBUZIONE
395800000629X   5C                   ELSE
395900000629IF  6C     SERS(I)       IFEQ      'P'
396000000629     C                   Z-ADD     SPC           $VA                            *POSTE
396100000629E   6C                   ENDIF
396200091013IF  6C     SERS(I)       IFEQ      'H'
396300091214     C                   Z-ADD     SHC           $VA                            *ESPRESSO 1030
396400091013E   6C                   ENDIF
396500000629E   5C                   ENDIF
396600000000E   4C                   ENDIF
396700000000E   3C                   ENDIF
396800000000     C*--------------------
396900000000     C* VALORIZZA  S E R V I Z I O
397000000000     C*--------------------
397100000000     C                   XFOOT     $VA           COM$VA           16 3
397200000000IF  3C     COM$VA        IFNE      *ZEROS                                       *SOLO SE PIENA
397300000000     C                   MOVEL     '1'           NODESV            1            *NO DESCRIZ VALORE
397400000000     C     DPRIMO        CAT       DSECON        VALSER           23            *VALORE + SERVIZIO
397500000000     C*
397600000000     C                   ADD       1             IN                             *INDICE
397700000000     C                   MOVEL     VALSER        $DS(IN)                        *DESCRIZIONE VALORE
397800000000     C                   Z-ADD     $VA(01)       $VS(IN)                        *VALORI MESE
397900000000     C                   Z-ADD     $VA(02)       $TS(IN)                        *VALORE TOTALE
398000000000     C*
398100000000IF  4C     J             IFEQ      4                                            *VALORE = VOLUME
398200000000     C                   Z-ADD     3             $ES(IN)                        *NUMERO DI DECIMALI
398300010920X   4C                   ELSE
398400010920IF  5C     J             IFEQ      5                                            *VALORE = IMPORTO
398500010920     C                   Z-ADD     2             $ES(IN)                        *NUMERO DI DECIMALI
398600010920X   5C                   ELSE                                                   *VALORE NO VOL E NO IMP
398700000000     C                   Z-ADD     *ZEROS        $ES(IN)                        *NUMERO DI DECIMALI
398800010920E   5C                   ENDIF
398900000000E   4C                   ENDIF
399000000000     C*
399100000000E   3C                   ENDIF
399200000000     C*
399300000000E   2C                   ENDDO
399400000000     C*--------------------
399500000000     C* VISUALIZZA RIGA TOTALE
399600000000     C*--------------------
399700000000     C* SE HO VISUAL.TO QC. PRIMA DEI VALORI LA RIGA DEI TOTALI NON DEVE ESSERE
399800000000     C* PRECEDUTA DALLA DESCRIZIONE VALORE, ALTRIMENTI SI
399900000000     C     J             OCCUR     WSER
400000000000IF  2C     NODESV        IFEQ      '1'
400100000000     C                   MOVEL     *BLANKS       DPRIMO
400200000000X   2C                   ELSE
400300000000     C                   MOVEL     DCA(J)        DPRIMO                         *DESCRIZIONE VALORE
400400000000E   1C                   ENDIF
400500000000     C*
400600000000     C                   MOVEL     DCA(6)        DSECON                         *VALORE "TOTALE"
400700000000     C     DPRIMO        CAT       DSECON        VALSER                         *VALORE + SERVIZIO
400800000000     C*
400900000000     C                   ADD       1             IN                             *INDICE
401000000000     C                   MOVEL     VALSER        $DS(IN)                        *DESCRIZIONE VALORE
401100000000     C                   Z-ADD     VAW(01)       $VS(IN)                        *VALORI MESE
401200000000     C                   Z-ADD     VAW(02)       $TS(IN)                        *VALORE TOTALE
401300000000     C*
401400000000IF  2C     J             IFEQ      4                                            *VALORE = VOLUME
401500000000     C                   Z-ADD     3             $ES(IN)                        *NUMERO DI DECIMALI
401600010920X   2C                   ELSE
401700010920IF  3C     J             IFEQ      5                                            *VALORE = IMPORTO
401800010920     C                   Z-ADD     2             $ES(IN)                        *NUMERO DI DECIMALI
401900010920X   3C                   ELSE                                                   *VALORE NO VOL E NO IMP
402000000000     C                   Z-ADD     *ZEROS        $ES(IN)                        *NUMERO DI DECIMALI
402100010920E   3C                   ENDIF
402200000000E   2C                   ENDIF
402300000000     C*
402400000000     C                   ADD       1             IN                             *INDICE
402500000000     C                   MOVEL     *ALL'-'       $DS(IN)                        *RIGA DI SEPARAZIONE
402600000000     C                   Z-ADD     *ZEROS        $ES(IN)                        *NUMERO DI DECIMALI
402700000000E   1C                   ENDDO
402800000000     C*--------------------
402900000000     C* CALCOLA MEDIE
403000000000     C*--------------------
403100000000     C                   Z-ADD     1             I
403200000000DO  1C     1             DO        9             I                              *MEDIA DA STAMPARE
403300000000     C*
403400000000     C                   Z-ADD     SPW(01)       WSPW             16 3          *SPEDIZIONI DEL MESE
403500000000     C                   Z-ADD     COW(01)       WCOW             16 3          *COLLI DEL MESE
403600000000     C                   Z-ADD     PEW(01)       WPEW             16 3          *PESO IN QL DEL MESE
403700000000     C                   Z-ADD     VOW(01)       WVOW             16 3          *VOLUME MC DEL MESE
403800000000     C                   Z-ADD     RIW(01)       WRIW             16 3          *RICAVI/1000 DEL MES
403900000000     C                   Z-ADD     *ZEROS        WMED             16 3          *MEDIA DA CALCOLARE
404000000000     C                   EXSR      CALMED
404100000000     C                   Z-ADD     WMED          MEW(01)                        *MEDIA DEL MESE
404200000000     C*
404300000000     C                   Z-ADD     SPW(02)       WSPW             16 3          *SPEDIZIONI TOTALI
404400000000     C                   Z-ADD     COW(02)       WCOW             16 3          *COLLI TOTALI
404500000000     C                   Z-ADD     PEW(02)       WPEW             16 3          *PESO IN QL TOTALI
404600000000     C                   Z-ADD     VOW(02)       WVOW             16 3          *VOLUME IN MC TOTALI
404700000000     C                   Z-ADD     RIW(02)       WRIW             16 3          *RICAVI/1000 TOTALI
404800000000     C                   Z-ADD     *ZEROS        WMED             16 3          *MEDIA DA CALCOLARE
404900000000     C                   EXSR      CALMED
405000000000     C                   Z-ADD     WMED          MEW(02)                        *MEDIA TOTALE
405100000000     C*--------------------
405200000000     C* VALORIZZA  M E D I E
405300000000     C*--------------------
405400000000     C                   ADD       1             IN                             *INDICE
405500000000     C                   MOVEL     DME(I)        $DS(IN)                        *DESCRIZIONE VALORE
405600000000     C                   Z-ADD     MEW(01)       $VS(IN)                        *VALORI MESE
405700000000     C                   Z-ADD     MEW(02)       $TS(IN)                        *VALORE TOTALE
405800000000     C*
405900000000IF  2C     I             IFLE      5                                            *PRIME CINQUE MEDIE
406000000000     C                   Z-ADD     3             $ES(IN)                        *NUMERO DI DECIMALI
406100000000X   2C                   ELSE                                                   *ALTRE QUATTRO MEDIE
406200010921     C                   Z-ADD     2             $ES(IN)                        *NUMERO DI DECIMALI
406300000000E   2C                   ENDIF
406400000000     C*
406500000000E   1C                   ENDDO
406600040318     C*--------------------
406700040318     C* COLLI ORIGINALI (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
406800040318     C*--------------------
406900040318     C                   XFOOT     cor           COM$VA
407000040318IF  1C     COM$VA        IFNE      *ZEROS
407100040318     C                   Z-ADD     *ZEROS        $ES(IN)                        *NUMERO DI DECIMALI
407200040318     C                   ADD       1             IN                             *INDICE
407300040318     C                   MOVEL     DME(40)       $DS(IN)                        *DESCRIZIONE VALORE
407400040318     C                   Z-ADD     cor(01)       $VS(IN)                        *VALORI MESE
407500040318     C                   Z-ADD     cor(02)       $TS(IN)                        *VALORE TOTALE
407600040318E   1C                   ENDIF
407700040319     C*--------------------
407800040319     C* BANCALI BNB (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
407900040319     C*--------------------
408000040319     C                   XFOOT     bnb           COM$VA
408100040319IF  1C     COM$VA        IFNE      *ZEROS
408200040319     C                   Z-ADD     *ZEROS        $ES(IN)                        *NUMERO DI DECIMALI
408300040319     C                   ADD       1             IN                             *INDICE
408400040319     C                   MOVEL     DME(41)       $DS(IN)                        *DESCRIZIONE VALORE
408500040319     C                   Z-ADD     bnb(01)       $VS(IN)                        *VALORI MESE
408600040319     C                   Z-ADD     bnb(02)       $TS(IN)                        *VALORE TOTALE
408700040319E   1C                   ENDIF
408800040319     C*--------------------
408900040319     C* BANCALI BAN (SOLO TOTALE: NO DETTAGLIO PER TIPO SERVIZIO)
409000040319     C*--------------------
409100040319     C                   XFOOT     ban           COM$VA
409200040319IF  1C     COM$VA        IFNE      *ZEROS
409300040319     C                   Z-ADD     *ZEROS        $ES(IN)                        *NUMERO DI DECIMALI
409400040319     C                   ADD       1             IN                             *INDICE
409500040319     C                   MOVEL     DME(42)       $DS(IN)                        *DESCRIZIONE VALORE
409600040319     C                   Z-ADD     ban(01)       $VS(IN)                        *VALORI MESE
409700040319     C                   Z-ADD     ban(02)       $TS(IN)                        *VALORE TOTALE
409800040319E   1C                   ENDIF
409900000000     C*
410000000000     C                   ADD       1             IN                             *INDICE
410100000000     C                   MOVEL     *ALL'-'       $DS(IN)                        *RIGA DI SEPARAZIONE
410200000000     C                   Z-ADD     *ZEROS        $ES(IN)                        *NUMERO DI DECIMALI
410300000000     C*
410400000000     C                   ENDSR
410500000000     C*------------------------------------------------------------------------*
410600000000     C*?VIDAFF - IMPOSTA VIDEO STATISTICA "AFFIDABILITA' SPEDIZIONI"
410700000000     C*------------------------------------------------------------------------*
410800000000     C     VIDAFF        BEGSR
410900000000     C*
411000950726     C                   Z-ADD     *ZEROS        IN                             *INDICE DI SCHIERA
411100000000     C*
411200000000     C* AZZERO I TOTALI
411300000000     C                   Z-ADD     *ZEROS        PNW
411400000000     C                   Z-ADD     *ZEROS        PRW
411500000000     C                   Z-ADD     *ZEROS        PSW
411600000629     C                   Z-ADD     *ZEROS        PPW
411700000000     C                   Z-ADD     *ZEROS        PTW
411800000000     C*
411900091214     C* CALCOLO I TOTALI DI CORRIERE/DITRIBUZIONE/ESPRESSO/Espresso1030
412000000000     C                   Z-ADD     1             J
412100000000DO  1C     1             DO        PARO          J
412200000000     C     J             OCCUR     CAFF
412300000000     C                   ADD       CNC           PNW                            *TOTALI CORRIERE
412400000629     C                   ADD       CDC           PRW                            *TOTALI DISTRIBUZION
412500000000     C                   ADD       CEC           PSW                            *TOTALI ESPRESSI
412600091214     C                   ADD       CHC           PSW                            *TOTALI ESPRESSI
412700000629     C                   ADD       CPC           PPW                            *TOTALI POSTE
412800000000     C     PTW           ADD       CNC           PTW
412900000629     C     PTW           ADD       CDC           PTW
413000000629     C     PTW           ADD       CEC           PTW
413100091214     C     PTW           ADD       CHC           PTW
413200091214     C     PTW           ADD       CPC           PTW                            *TOTALI C + D + E + H
413300000000E   1C                   ENDDO
413400000000     C*
413500000000     C                   ADD       1             IN                             *INDICE
413600000000     C                   MOVEL     DME(24)       $DA(IN)                        *DESCRIZIONE INFORMA
413700000000     C                   MOVEL     DME(25)       $PA(IN)                        *DESCRIZIONE SERVIZI
413800000000     C                   MOVEL     DME(25)       $TA(IN)                        *DESCRIZIONE SERVIZI
413900000000     C*
414000000000     C* PER OGNI ORA CONSEGNA DA STAMPARE
414100000000     C                   Z-ADD     1             J
414200000000DO  1C     1             DO        PARO          J
414300000000     C     J             OCCUR     CAFF
414400000000     C*--------------------
414500000000     C* INCIDENZA CORRIERE
414600000000     C*--------------------
414700000000     C                   Z-ADD     CNC(01)       WVAW             16 3          *TOT SERVIZIO ORA
414800000000     C                   Z-ADD     PNW(01)       WTXW             16 3          *TOT SERVIZIO ORE
414900000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
415000000000     C                   EXSR      CALINC
415100000000     C                   Z-ADD     WINW          $P1(01)                        *INCID CORRIERE MESE
415200000000     C*
415300000000     C                   Z-ADD     CNC(02)       WVAW             16 3          *TOT SERVIZIO ORA
415400000000     C                   Z-ADD     PNW(02)       WTXW             16 3          *TOT SERVIZIO ORE
415500000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
415600000000     C                   EXSR      CALINC
415700000000     C                   Z-ADD     WINW          $P1(02)                        *INCID CORRIER TOTAL
415800000000     C*--------------------
415900000000     C* INCIDENZA ESPRESSI
416000000000     C*--------------------
416100000000     C                   Z-ADD     CEC(01)       WVAW             16 3          *TOT SERVIZIO ORA
416200091214     C                   add       CHC(01)       WVAW                           *TOT SERVIZIO ORA H1030
416300000000     C                   Z-ADD     PSW(01)       WTXW             16 3          *TOT SERVIZIO ORE
416400000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
416500000000     C                   EXSR      CALINC
416600000000     C                   Z-ADD     WINW          $P2(01)                        *INCIDENZA ESPRESSI
416700000000     C*
416800000000     C                   Z-ADD     CEC(02)       WVAW             16 3          *TOT SERVIZIO ORA
416900091214     C                   add       CHC(02)       WVAW                           *TOT SERVIZIO ORA H1030
417000000000     C                   Z-ADD     PSW(02)       WTXW             16 3          *TOT SERVIZIO ORE
417100000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
417200000000     C                   EXSR      CALINC
417300000000     C                   Z-ADD     WINW          $P2(02)                        *INCIDENZA ESPRESSI
417400000000     C*--------------------
417500000000     C* INCIDENZA DISTRIBUZIONE
417600000000     C*--------------------
417700000629     C                   Z-ADD     CDC(01)       WVAW             16 3          *TOT SERVIZIO ORA
417800000000     C                   Z-ADD     PRW(01)       WTXW             16 3          *TOT SERVIZIO ORE
417900000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
418000000000     C                   EXSR      CALINC
418100000000     C                   Z-ADD     WINW          $P3(01)                        *INCID DISTRIBUZ MES
418200000000     C*
418300000629     C                   Z-ADD     CDC(02)       WVAW             16 3          *TOT SERVIZIO ORA
418400000000     C                   Z-ADD     PRW(02)       WTXW             16 3          *TOT SERVIZIO ORE
418500000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
418600000000     C                   EXSR      CALINC
418700000000     C                   Z-ADD     WINW          $P3(02)                        *INCID DISTRIBUZ MES
418800000629     C*--------------------
418900000629     C* INCIDENZA POSTE
419000000629     C*--------------------
419100000629     C                   Z-ADD     CPC(01)       WVAW             16 3          *TOT SERVIZIO ORA
419200000629     C                   Z-ADD     PPW(01)       WTXW             16 3          *TOT SERVIZIO ORE
419300000629     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
419400000629     C                   EXSR      CALINC
419500000629     C                   Z-ADD     WINW          $P4(01)                        *INCIDENZA POSTE
419600000629     C*
419700000629     C                   Z-ADD     CPC(02)       WVAW             16 3          *TOT SERVIZIO ORA
419800000629     C                   Z-ADD     PPW(02)       WTXW             16 3          *TOT SERVIZIO ORE
419900000629     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
420000000629     C                   EXSR      CALINC
420100000629     C                   Z-ADD     WINW          $P4(02)                        *INCIDENZA POSTE
420200000000     C*--------------------
420300000000     C* INCIDENZA TOTALE
420400000000     C*--------------------
420500000000     C                   Z-ADD     CNC(01)       WVAW             16 3          *TOTALE SERVIZI ORA
420600000629     C                   ADD       CDC(01)       WVAW
420700000000     C                   ADD       CEC(01)       WVAW
420800091214     C                   add       CHC(01)       WVAW
420900000629     C                   ADD       CPC(01)       WVAW
421000000000     C                   Z-ADD     PTW(01)       WTXW             16 3          *TOTALE SERVIZI ORE
421100000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
421200000000     C                   EXSR      CALINC
421300000629     C                   Z-ADD     WINW          $P5(01)                        *INCIDENZA ESPRESSI
421400000000     C*
421500000000     C                   Z-ADD     CNC(02)       WVAW             16 3          *TOTALE SERVIZI ORA
421600000629     C                   ADD       CDC(02)       WVAW
421700000000     C                   ADD       CEC(02)       WVAW
421800091214     C                   add       CHC(02)       WVAW
421900000629     C                   ADD       CPC(02)       WVAW
422000000000     C                   Z-ADD     PTW(02)       WTXW             16 3          *TOTALE SERVIZI ORE
422100000000     C                   Z-ADD     *ZEROS        WINW              2 0          *INCIDENZA SPEDIZION
422200000000     C                   EXSR      CALINC
422300000629     C                   Z-ADD     WINW          $P5(02)                        *INCIDENZA ESPRESSI
422400000000     C***
422500000000     C* VISUALIZZA RIGA
422600000000     C***
422700000000     C                   ADD       1             IN                             *INDICE
422800000000     C                   MOVEL     DORE(J)       $DA(IN)                        *DESCRIZIONE VALORE
422900000000     C* VALORE  M E S E
423000000000     C                   MOVEL     $P1(01)       COM$PX            2
423100000000IF  2C     COM$PX        IFEQ      *ZEROS                                       *TOLGO ZERI NO SIGNI
423200000000     C                   MOVEL     *BLANKS       COM$PX            2
423300000000E   2C                   ENDIF
423400000000     C                   MOVEL     COM$PX        $DAPSX            8            *PARTE DI SINISTRA
423500000000     C*
423600000000     C                   MOVEL     $P2(01)       COM$PX            2
423700000000IF  2C     COM$PX        IFEQ      *ZEROS                                       *TOLGO ZERI NO SIGNI
423800000000     C                   MOVEL     *BLANKS       COM$PX            2
423900000000E   2C                   ENDIF
424000000000     C                   MOVE      COM$PX        $DAPSX            8            *PARTE DI SINISTRA
424100000000     C*
424200000000     C                   MOVEL     $P3(01)       COM$PX            2
424300000000IF  2C     COM$PX        IFEQ      *ZEROS                                       *TOLGO ZERI NO SIGNI
424400000000     C                   MOVEL     *BLANKS       COM$PX            2
424500000000E   2C                   ENDIF
424600000000     C                   MOVEL     COM$PX        $DAPDX            8            *PARTE DI DESTRA
424700000000     C*
424800000629     C                   MOVEL     $P5(01)       COM$PX            2
424900000000IF  2C     COM$PX        IFEQ      *ZEROS                                       *TOLGO ZERI NO SIGNI
425000000000     C                   MOVEL     *BLANKS       COM$PX            2
425100000000E   2C                   ENDIF
425200000000     C                   MOVE      COM$PX        $DAPDX            8
425300000000     C*
425400000000     C                   MOVEL     *BLANKS       $PA(IN)
425500000000     C     $DAPSX        CAT       '    '        DAPSXB           12            *PARTE Sx PIU' SPAZI
425600000000     C     DAPSXB        CAT       $DAPDX        $PA(IN)
425700000000     C*
425800000000     C* VALORE  T O T A L E
425900000000     C                   MOVEL     $P1(02)       COM$PX            2
426000000000IF  2C     COM$PX        IFEQ      *ZEROS                                       *TOLGO ZERI NO SIGNI
426100000000     C                   MOVEL     *BLANKS       COM$PX            2
426200000000E   2C                   ENDIF
426300000000     C                   MOVEL     COM$PX        $DAPSX            8            *PARTE DI SINISTRA
426400000000     C*
426500000000     C                   MOVEL     $P2(02)       COM$PX            2
426600000000IF  2C     COM$PX        IFEQ      *ZEROS                                       *TOLGO ZERI NO SIGNI
426700000000     C                   MOVEL     *BLANKS       COM$PX            2
426800000000E   2C                   ENDIF
426900000000     C                   MOVE      COM$PX        $DAPSX            8            *PARTE DI SINISTRA
427000000000     C*
427100000000     C                   MOVEL     $P3(02)       COM$PX            2
427200000000IF  2C     COM$PX        IFEQ      *ZEROS                                       *TOLGO ZERI NO SIGNI
427300000000     C                   MOVEL     *BLANKS       COM$PX            2
427400000000E   2C                   ENDIF
427500000000     C                   MOVEL     COM$PX        $DAPDX            8            *PARTE DI DESTRA
427600000000     C*
427700000629     C                   MOVEL     $P5(02)       COM$PX            2
427800000000IF  2C     COM$PX        IFEQ      *ZEROS                                       *TOLGO ZERI NO SIGNI
427900000000     C                   MOVEL     *BLANKS       COM$PX            2
428000000000E   2C                   ENDIF
428100000000     C                   MOVE      COM$PX        $DAPDX            8
428200000000     C*
428300000000     C                   MOVEL     *BLANKS       $TA(IN)
428400000000     C     $DAPSX        CAT       '    '        DAPSXB           12            *PARTE Sx PIU' SPAZI
428500000000     C     DAPSXB        CAT       $DAPDX        $TA(IN)
428600000000     C*
428700000000E   1C                   ENDDO
428800000000     C*
428900000000     C                   ADD       1             IN                             *INDICE
429000000000     C                   MOVEL     *ALL'-'       $DA(IN)                        *RIGA DI SEPARAZIONE
429100000000     C*
429200000000     C                   ENDSR
429300000000     C*------------------------------------------------------------------------*
429400000000     C*?CALINC - CALCOLA L'INCIDENZA DELLE CONSEGNE DELLE SPEDIZIONI
429500000000     C*          INPUT --> WTXW : TOTALE SERVIZIO ORE
429600000000     C*                    WVAW : TOTALE SERVIZIO ORA
429700000000     C*          OUTPUT -->WINW: PERCENTUALE DI INCIDENZA
429800000000     C*------------------------------------------------------------------------*
429900000000     C     CALINC        BEGSR
430000000000     C*
430100000000     C*             CORRIERE/DISTRIBUZIONE/ESPRESSO x 100
430200000000     C* INCIDENZA = --------------------------------------
430300000000     C*             TOTALE CORRIERE/DISTRIBUZIONE/ESPRESSO
430400000000     C*
430500000000     C* NB: SE IL TOTALE SPEDIZIONI E' = 0 LO IMPOSTO A 1 PER NON DARE ERRORE
430600000000     C*     <NEL CASO NON CI SIANO SPEDIZIONI PER UNA DETERMINATA ORA (0/1=0)
430700000000IF  1C     WTXW          IFEQ      *ZEROS
430800000000     C                   Z-ADD     1             WTXW
430900000000E   1C                   ENDIF
431000000000     C*
431100000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
431200000000     C     WVAW          MULT      100           RISUL            16 3
431300000000     C     RISUL         DIV       WTXW          RISUL
431400000000     C*
431500000000     C* SE LA PERCENTUALE E' 100%  METTO  99% PERCHE' IL CAMPO E' LUNGO 2
431600060525IF  1C**** RISUL         IFGE      100
431700060525IF  1C     RISUL         IFGE      99
431800000000     C                   Z-ADD     99            WINW
431900000000X   1C                   ELSE
432000000000     C                   Z-ADD(H)  RISUL         WINW              2 0
432100000000E   1C                   ENDIF
432200000000     C*
432300000000     C                   ENDSR
432400000000     C*------------------------------------------------------------------------*
432500000000     C*?CALDEL - CALCOLA IL DELTA PER LA STATISTICA "DELTA CLIENTE"
432600000000     C*          INPUT  --> WRRW: RICAVO REALE
432700000000     C*                     WRPW: RICAVO PRESUNTO
432800000000     C*          OUTPUT --> WDPW: DELTA PERCENTUALE
432900000000     C*------------------------------------------------------------------------*
433000000000     C     CALDEL        BEGSR
433100000000     C*--------------------
433200000000     C* (RICAVO REALE - RICAVO PRESUNTO) = GAP
433300000000     C* (GAP x 100) : RICAVO REALE = DELTA PERCENTUALE
433400000000     C*--------------------
433500000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
433600000000     C     WRRW          SUB       WRPW          GAP              16 3          (REALE - PRESUNTO)
433700000000     C     GAP           MULT      100           GAP                            (GAP x 100)
433800000000     C*
433900000000     C* NB: SE IL RICAVO REALE E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
434000000000IF  1C     WRRW          IFEQ      *ZEROS
434100000000     C                   Z-ADD     1             WRRW
434200000000E   1C                   ENDIF
434300000000     C*
434400000000     C     GAP           DIV       WRRW          GAP
434500000000     C                   Z-ADD(H)  GAP           WDPW                           *DELTA PERCENTUALE
434600000000     C*
434700000000     C* SE IL DELTA E' +/-1000  METTO  +/-999,9 %
434800000000     C     WDPW          COMP      0                                    28
434900000000     C* POSITIVO
435000000000IF  1C     *IN28         IFEQ      *OFF
435100000000IF  2C     GAP           IFGE      1000
435200000000     C                   Z-ADD     +999,9        WDPW
435300000000E   2C                   ENDIF
435400000000     C* NEGATIVO
435500000000X   1C                   ELSE
435600000000IF  2C     GAP           IFLE      -1000
435700000000     C                   Z-ADD     -999,9        WDPW
435800000000E   2C                   ENDIF
435900000000E   1C                   ENDIF
436000000000     C*
436100000000     C                   ENDSR
436200000000     C*------------------------------------------------------------------------*
436300000000     C*?CALPER - CALCOLA LA PERCENTUALE SUL FATTURATO PER LA "AMMINISTRAZIONE"
436400000000     C*          INPUT  --> WFAW: FATTURATO
436500000000     C*                     WNRW: NOTE ACCREDITO O RETTITICHE
436600000000     C*          OUTPUT --> WPRW: PERCENTUALE SUL FATTURATO
436700000000     C*------------------------------------------------------------------------*
436800000000     C     CALPER        BEGSR
436900000000     C*
437000000000     C*               NOTE/RETTIFICHE/RICAVI GIACENZE x 100
437100000000     C* PERCENTUALE = -------------------------------------
437200000000     C*                           FATTURATO
437300000000     C*
437400000000     C* NB: SE IL FATTURATO E' A ZERO LO IMPOSTO A UNO PER NON DARE ERRORE
437500000000IF  1C     WFAW          IFEQ      *ZEROS
437600000000     C                   Z-ADD     1             WFAW
437700000000E   1C                   ENDIF
437800000000     C*
437900000000     C* NB: LA PERCENTUALE CALCOLATA DEVE ESSERE SEMPRE ARROTONDATA
438000000000     C     WNRW          MULT      100           RISUL            16 3
438100000000     C     RISUL         DIV       WFAW          RISUL
438200000000     C                   Z-ADD(H)  RISUL         WPRW
438300000000     C*
438400000000     C* SE LA PERCENTUALE E' +/-1000  METTO  +/-999,9 %
438500000000     C     WPRW          COMP      0                                    28
438600000000     C* POSITIVO
438700000000IF  1C     *IN28         IFEQ      *OFF
438800000000IF  2C     RISUL         IFGE      1000
438900000000     C                   Z-ADD     +999,9        WPRW
439000000000E   2C                   ENDIF
439100000000     C* NEGATIVO
439200000000X   1C                   ELSE
439300000000IF  2C     RISUL         IFLE      -1000
439400000000     C                   Z-ADD     -999,9        WPRW
439500000000E   2C                   ENDIF
439600000000E   1C                   ENDIF
439700000000     C*
439800000000     C                   ENDSR
439900000000     C*------------------------------------------------------------------------*
440000000000     C*?CALMED - CALCOLA LE MEDIE FINALI PER LA "SERVIZIO BOLLE"
440100000000     C*          INPUT --> WSPW: SPEDIZIONI DEL MESE
440200000000     C*                    WCOW: COLLI DEL MESE
440300000000     C*                    WPEW: PESO IN QUINTALI DEL MESE
440400000000     C*                    WVOW: VOLUME IN M.CUBI DEL MESE
440500000000     C*                    WRIW: RICAVI DEL MESE
440600000000     C*                    I   : MEDIA DA CALCOLARE (1÷9)
440700000000     C*          INPUT --> WMED: MEDIA CALCOLATA
440800000000     C*------------------------------------------------------------------------*
440900000000     C     CALMED        BEGSR
441000000000     C*
441100000000     C*    COLLI/KG /M.CUBO       KG/M.CUBO           RICAVI x 1000
441200000000     C* 1] ---------------- ;  2] --------- ;  3] -----------------------
441300000000     C*       SPEDIZIONI            COLLI          SPED/COLLI/Q.LE/M.CUBO
441400000000     C*---------------------
441500000000     C* PRIMA SERIE
441600000000     C*---------------------
441700000000     C* SE LE SPEDIZIONI SONO = 0, LE IMPOSTO = 1 PER NON DARE ERRORE
441800000000IF  1C     I             IFEQ      1
441900000000     C     I             OREQ      2
442000000000     C     I             OREQ      3
442100000000IF  2C     WSPW          IFEQ      *ZEROS
442200000000     C                   Z-ADD     1             WSPW
442300000000E   2C                   ENDIF
442400000000     C*
442500000000IF  2C     I             IFEQ      1
442600000000     C     WCOW          DIV       WSPW          WMED                           *COLLI/SPEDIZIONI
442700000000E   2C                   ENDIF
442800000000     C*
442900000000IF  2C     I             IFEQ      2
443000000000     C     WPEW          DIV       WSPW          WMED                           *KG/SPEDIZIONI
443100000000E   2C                   ENDIF
443200000000     C*
443300000000IF  2C     I             IFEQ      3
443400000000     C     WVOW          DIV       WSPW          WMED                           *VOLUME/SPEDIZIONI
443500000000E   2C                   ENDIF
443600000000E   1C                   ENDIF
443700000000     C*---------------------
443800000000     C* SECONDA SERIE
443900000000     C*---------------------
444000000000     C* SE I COLLI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
444100000000IF  1C     I             IFEQ      4
444200000000     C     I             OREQ      5
444300000000IF  2C     WCOW          IFEQ      *ZEROS
444400000000     C                   Z-ADD     1             WCOW
444500000000E   2C                   ENDIF
444600000000     C*
444700000000IF  2C     I             IFEQ      4
444800000000     C     WPEW          DIV       WCOW          WMED                           *KG/COLLI
444900000000E   2C                   ENDIF
445000000000     C*
445100000000IF  2C     I             IFEQ      5
445200000000     C     WVOW          DIV       WCOW          WMED                           *VOLUME/COLLI
445300000000E   2C                   ENDIF
445400000000E   1C                   ENDIF
445500000000     C*---------------------
445600000000     C* TERZA SERIE
445700000000     C*---------------------
445800000000IF  1C     I             IFEQ      6
445900000000     C     I             OREQ      7
446000000000     C     I             OREQ      8
446100000000     C     I             OREQ      9
446200000000     C*
446300000000     C* TRASFORMO IL PESO DA KILOGRAMMI IN QUINTALI
446400000000     C     WPEW          DIV       100           WPEW
446500000000     C*
446600000000IF  2C     I             IFEQ      6
446700000000     C* SE LE SPEDIZIONI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
446800000000IF  3C     WSPW          IFEQ      *ZEROS
446900000000     C                   Z-ADD     1             WSPW
447000000000E   3C                   ENDIF
447100000000     C     WRIW          DIV       WSPW          WMED                           *INCASSO/SPEDIZIONI
447200000000E   2C                   ENDIF
447300000000     C*
447400000000IF  2C     I             IFEQ      7
447500000000     C* SE I COLLI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
447600000000IF  3C     WCOW          IFEQ      *ZEROS
447700000000     C                   Z-ADD     1             WCOW
447800000000E   3C                   ENDIF
447900000000     C     WRIW          DIV       WCOW          WMED                           *INCASSO/COLLI
448000000000E   2C                   ENDIF
448100000000     C*
448200000000IF  2C     I             IFEQ      8
448300000000     C* SE I QUINTALI SONO = 0, LI IMPOSTO = 1 PER NON DARE ERRORE
448400000000IF  3C     WPEW          IFEQ      *ZEROS
448500000000     C                   Z-ADD     1             WPEW
448600000000E   3C                   ENDIF
448700000000     C     WRIW          DIV       WPEW          WMED                           *INCASSO/QUINTALI
448800000000E   2C                   ENDIF
448900000000     C*
449000000000IF  2C     I             IFEQ      9
449100000000     C* SE IL VOLUME E' = 0, LO IMPOSTO = 1 PER NON DARE ERRORE
449200000000IF  3C     WVOW          IFEQ      *ZEROS
449300000000     C                   Z-ADD     1             WVOW
449400000000E   3C                   ENDIF
449500000000     C     WRIW          DIV       WVOW          WMED                           *INCASSO/VOLUME
449600000000E   2C                   ENDIF
449700000000     C*
449800000000E   1C                   ENDIF
449900000000     C*
450000000000     C                   ENDSR
450100000821     C*------------------------------------------------------------------------*
450200000821     C*?REPRAG - REPERISCE LA RAGIONE SOCIALE DEL CLIENTE
450300000821     C*------------------------------------------------------------------------*
450400000821     C     REPRAG        BEGSR
450500000821     C*
450600000821     C* AZZERA VARIABILI DI LAVORO
450700000821     C                   CLEAR                   WDKSC
450800000821     C*
450900000821     C* CHIAMA IL PGM DI RICERCA CLIENTE
451000000821     C                   CLEAR                   BS69DS
451100000821     C                   CLEAR                   ACODS
451200000821     C                   MOVEL     KNSIF         I69SIF
451300000821     C                   MOVEL     WKSC          I69KAC
451400000821     C                   CALL      'TIBS69R'
451500000821     C                   PARM                    BS69DS
451600000821     C                   PARM                    ACODS
451700000821     C                   PARM                    INDDS
451800000821     C                   PARM                    CLPDS
451900000821     C                   PARM                    CLSDS
452000000821IF  1C     O69ERR        IFNE      '1'                                          *NO ERRORE
452100000821     C                   MOVEL     ACORAG        WDKSC
452200000821E   1C                   ENDIF
452300000821     C*
452400000821     C                   ENDSR
452500000000     C*------------------------------------------------------------------------*
452600000000     C*?CARTAB - CARICA TABELLE (FASCE, REGIONI, SERVIZI, ORE CONSEGNE)
452700000000     C*------------------------------------------------------------------------*
452800000000     C     CARTAB        BEGSR
452900000000     C*
453000000000     C                   Z-ADD     1             KTBUTE                         *CODICE UTENTE
453100990217     C*--------------------
453200000000     C* FASCE DI PESO
453300990217     C*--------------------
453400990601IF  1C     D60DA2        IFLE      199812                                       *FASCE FINO AL '98
453500990217     C                   MOVEL     'SDC'         WSX               3
453600990217X   1C                   ELSE                                                   *FASCE DAL '99
453700020325     C     D60DA2        IFLE      200112
453800020325     C                   MOVEL     'S9C'         WSX               3
453900020325     C                   ELSE                                                   *FASCE DAL 2002
454000020325     C                   MOVEL     'S2C'         WSX
454100020325     C                   ENDIF
454200020325E   1C                   ENDIF
454300000000     C                   MOVEL     '2L'          KTBCOD
454400000000     C     KEYTA2        SETLL     TABEL00F
454500000000     C     KEYTA2        READE     TABEL00F                               99
454600000000     C                   Z-ADD     *ZEROS        J                              *INDICE
454700000000DO  1C     *IN99         DOWEQ     *OFF
454800000000IF  2C     TBLFLG        IFEQ      *BLANKS
454900990217     C                   MOVEL     TBLKEY        STACOD            5            *SxC+XX
455000000000     C                   MOVEL     TBLKEY        FILEST            3            *TIPO STATISTICA
455100990217IF  3C     FILEST        IFEQ      WSX
455200000000     C                   ADD       1             J
455300000000     C                   MOVE      STACOD        FPE(J)                         *CODICE STATISTICA
455400000000     C                   MOVEL     TBLUNI        DS2L
455500000000     C                   MOVEL     §2LDFP        DFPE(J)                        *DECODIFICA FASCIA
455600000000E   3C                   ENDIF
455700000000E   2C                   ENDIF
455800000000     C     KEYTA2        READE     TABEL00F                               99
455900000000E   1C                   ENDDO
456000000000     C***
456100000000     C* PARF = N° FASCE ESISTENTI
456200000000     C***
456300000000     C                   Z-ADD     J             PARF                           *NON MODIFICARE
456400000000     C*
456500000000     C* NESSUNA FASCIA SELEZIONATA
456600000000IF  1C     D60FP1        IFEQ      *BLANKS
456700000000     C     D60FP2        ANDEQ     *BLANKS
456800000000     C     D60FP3        ANDEQ     *BLANKS
456900000000     C     D60FP4        ANDEQ     *BLANKS
457000000000     C     D60FP5        ANDEQ     *BLANKS
457100000000     C     D60FP6        ANDEQ     *BLANKS
457200000000     C     D60FP7        ANDEQ     *BLANKS
457300000000     C     D60FP8        ANDEQ     *BLANKS
457400000000     C     D60FP9        ANDEQ     *BLANKS
457500000000     C     D60FP0        ANDEQ     *BLANKS
457600000000     C     D60FPA        ANDEQ     *BLANKS
457700000000     C                   MOVEL     'T'           SELFPE                         *TUTTE LE FASCE
457800000000     C***
457900000000     C* PARF = N° FASCE SELEZIONATE UGUALE A QUELLE ESISTENTI
458000000000     C***
458100000000     C                   MOVEL     FPE           SFPE                           *FASCE ESIST = SELEZ
458200000000     C                   Z-ADD     PARF          SPARF                          *NON MODIFICARE
458300000000X   1C                   ELSE
458400000000     C* FASCE SELEZIONATE
458500000000     C                   Z-ADD     *ZEROS        J
458600000000IF  2C     D60FP1        IFNE      *BLANKS
458700000000     C                   SETOFF                                           28
458800000000     C     D60FP1        LOOKUP    SFPE(1)                                28
458900000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
459000000000     C                   ADD       1             J
459100000000     C                   MOVEL     D60FP1        SFPE(J)
459200000000E   3C                   ENDIF
459300000000E   2C                   ENDIF
459400000000     C*
459500000000IF  2C     D60FP2        IFNE      *BLANKS
459600000000     C                   SETOFF                                           28
459700000000     C     D60FP2        LOOKUP    SFPE(1)                                28
459800000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
459900000000     C                   ADD       1             J
460000000000     C                   MOVEL     D60FP2        SFPE(J)
460100000000E   3C                   ENDIF
460200000000E   2C                   ENDIF
460300000000     C*
460400000000IF  2C     D60FP3        IFNE      *BLANKS
460500000000     C                   SETOFF                                           28
460600000000     C     D60FP3        LOOKUP    SFPE(1)                                28
460700000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
460800000000     C                   ADD       1             J
460900000000     C                   MOVEL     D60FP3        SFPE(J)
461000000000E   3C                   ENDIF
461100000000E   2C                   ENDIF
461200000000     C*
461300000000IF  2C     D60FP4        IFNE      *BLANKS
461400000000     C                   SETOFF                                           28
461500000000     C     D60FP4        LOOKUP    SFPE(1)                                28
461600000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
461700000000     C                   ADD       1             J
461800000000     C                   MOVEL     D60FP4        SFPE(J)
461900000000E   3C                   ENDIF
462000000000E   2C                   ENDIF
462100000000     C*
462200000000IF  2C     D60FP5        IFNE      *BLANKS
462300000000     C                   SETOFF                                           28
462400000000     C     D60FP5        LOOKUP    SFPE(1)                                28
462500000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
462600000000     C                   ADD       1             J
462700000000     C                   MOVEL     D60FP5        SFPE(J)
462800000000E   3C                   ENDIF
462900000000E   2C                   ENDIF
463000000000     C*
463100000000IF  2C     D60FP6        IFNE      *BLANKS
463200000000     C                   SETOFF                                           28
463300000000     C     D60FP6        LOOKUP    SFPE(1)                                28
463400000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
463500000000     C                   ADD       1             J
463600000000     C                   MOVEL     D60FP6        SFPE(J)
463700000000E   3C                   ENDIF
463800000000E   2C                   ENDIF
463900000000     C*
464000000000IF  2C     D60FP7        IFNE      *BLANKS
464100000000     C                   SETOFF                                           28
464200000000     C     D60FP7        LOOKUP    SFPE(1)                                28
464300000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
464400000000     C                   ADD       1             J
464500000000     C                   MOVEL     D60FP7        SFPE(J)
464600000000E   3C                   ENDIF
464700000000E   2C                   ENDIF
464800000000     C*
464900000000IF  2C     D60FP8        IFNE      *BLANKS
465000000000     C                   SETOFF                                           28
465100000000     C     D60FP8        LOOKUP    SFPE(1)                                28
465200000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
465300000000     C                   ADD       1             J
465400000000     C                   MOVEL     D60FP8        SFPE(J)
465500000000E   3C                   ENDIF
465600000000E   2C                   ENDIF
465700000000     C*
465800000000IF  2C     D60FP9        IFNE      *BLANKS
465900000000     C                   SETOFF                                           28
466000000000     C     D60FP9        LOOKUP    SFPE(1)                                28
466100000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
466200000000     C                   ADD       1             J
466300000000     C                   MOVEL     D60FP9        SFPE(J)
466400000000E   3C                   ENDIF
466500000000E   2C                   ENDIF
466600000000     C*
466700000000IF  2C     D60FP0        IFNE      *BLANKS
466800000000     C                   SETOFF                                           28
466900000000     C     D60FP0        LOOKUP    SFPE(1)                                28
467000000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
467100000000     C                   ADD       1             J
467200000000     C                   MOVEL     D60FP0        SFPE(J)
467300000000E   3C                   ENDIF
467400000000E   2C                   ENDIF
467500000000     C*
467600000000IF  2C     D60FPA        IFNE      *BLANKS
467700000000     C                   SETOFF                                           28
467800000000     C     D60FPA        LOOKUP    SFPE(1)                                28
467900000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATA
468000000000     C                   ADD       1             J
468100000000     C                   MOVEL     D60FPA        SFPE(J)
468200000000E   3C                   ENDIF
468300000000E   2C                   ENDIF
468400000000     C***
468500000000     C* PARF = N° FASCE SELEZIONATE
468600000000     C***
468700000000     C                   Z-ADD     J             SPARF                          *NON MODIFICARE
468800000000E   1C                   ENDIF
468900000000     C*-------------------------
469000000000     C* REGIONI
469100000000     C*-------------------------
469200000000     C*
469300000000     C* POSIZIONAMENTO SUL FILE ORGANIGRAMMA E PRIMA LETTURA
469400000000     C                   Z-ADD     *ZEROS        KORFIL                         *CODICE FILIALE
469500000000     C     KORFIL        SETLL     AZORG01L
469600000000     C                   READ      AZORG01L      UNIORG                   99
469700000000     C*
469800000000     C* CICLO FINO A FINE FILE
469900000000     C                   Z-ADD     *ZEROS        J
470000000000DO  1C     *IN99         DOWEQ     *OFF
470100000000     C*
470200000000     C* ELIMINA FILIALI ANNULLATE E VOCI VARIE
470300000000IF  2C     ORGFVA        IFEQ      *BLANKS
470400000000     C     ORGFAG        ANDNE     'V'
470500000000     C                   ADD       1             J
470600000000     C                   Z-ADD     ORGFIL        FIL(J)
470700000000     C*
470800000000     C* REPERISCO IL RAGGRUPPAMENTO REGIONE DELLA FILIALE
470900000000     C                   MOVEL     '14'          KTBCOD
471000000000     C                   MOVEL     *BLANKS       KTBKEY
471100000000     C                   MOVEL     ORGCRE        KTBKEY
471200000000     C     KEYTAB        CHAIN     TABEL00F                           99
471300000000IF  3C     *IN99         IFEQ      *OFF
471400000000     C     TBLFLG        ANDEQ     *BLANKS
471500000000     C                   MOVEL     TBLUNI        DS14
471600020325     C* VERIFICO QULI RAGGRUPPAMENTI CONSIDERARE IN RELAZIONE ALL'ANNO DEL PERIODO DA ELABORARE
471700020325     C     D60DA2        IFLE      200112
471800020325     C                   MOVEL     §14CRR        DEPCRN            3 0
471900020325     C                   ELSE
472000020325     C                   MOVEL     §14CRN        DEPCRN
472100020325     C                   ENDIF
472200020325     C*
472300020325     C                   MOVEL     DEPCRN        CRR(J)                         *CODICE RAGGRUPAM.TO
472400000000     C*
472500000000     C* DECODIFICA RAGGRUPPAMENTO
472600000000     C                   MOVEL     '2M'          KTBCOD
472700000000     C                   MOVEL     *BLANKS       KTBKEY
472800020325     C                   MOVEL     DEPCRN        KTBKEY
472900000000     C     KEYTAB        CHAIN     TABEL00F                           99
473000000000IF  4C     *IN99         IFEQ      *OFF
473100000000     C     TBLFLG        ANDEQ     *BLANKS
473200000000     C                   MOVEL     TBLUNI        DS2M
473300000000     C                   MOVEL     §2MDES        DCRR(J)                        *DECODIFICA RAGGR.TO
473400000000X   4C                   ELSE
473500000000     C                   MOVEL     *ALL'*'       DCRR(J)
473600000000E   4C                   ENDIF
473700000000X   3C                   ELSE
473800000000     C                   MOVEL     *ALL'9'       CRR(J)
473900000000E   3C                   ENDIF
474000000000     C*
474100000000E   2C                   ENDIF
474200000000     C*
474300000000     C                   READ      AZORG01L      UNIORG                   99
474400000000E   1C                   ENDDO
474500950206     C*
474600950206     C* CARICAMENTO TIPI VALORI POSSIBILI DA STAMPARE
474700950206     C                   MOVEL     'C'           VRR(1)                         *COLLI
474800950206     C                   MOVEL     DME(26)       DVRR(1)
474900950206     C                   MOVEL     'P'           VRR(2)                         *PESO
475000950206     C                   MOVEL     DME(27)       DVRR(2)
475100950206     C                   MOVEL     'R'           VRR(3)                         *RICAVI
475200950206     C                   MOVEL     DME(28)       DVRR(3)
475300950206     C                   MOVEL     'S'           VRR(4)                         *SPEDIZIONI
475400950206     C                   MOVEL     DME(29)       DVRR(4)
475500950206     C                   MOVEL     'V'           VRR(5)                         *VOLUME
475600950206     C                   MOVEL     DME(30)       DVRR(5)
475700000000     C*-------------------------
475800000000     C* TIPI SERVIZIO
475900000000     C*-------------------------
476000000000     C* CARICAMENTO SERVIZI ESISTENTI
476100000000     C                   MOVEL     '5E'          KTBCOD
476200000000     C     KEYTA2        SETLL     TABEL00F
476300000000     C     KEYTA2        READE     TABEL00F                               99
476400000000     C*
476500000000     C                   Z-ADD     *ZEROS        J                              *INDICE
476600000000DO  1C     *IN99         DOWEQ     *OFF
476700000000IF  2C     TBLFLG        IFEQ      *BLANKS
476800000000     C                   ADD       1             J
476900000000     C                   MOVEL     TBLKEY        SER(J)                         *CODICE SERVIZIO
477000090610     C                   MOVEL     TBLUNI        DS5e                           *DESCRIZIONE SERVIZI
477100090610     C                   MOVEL     §5ED08        DSER(J)                        *DESCRIZIONE SERVIZI
477200000000E   2C                   ENDIF
477300000000     C*
477400000000     C     KEYTA2        READE     TABEL00F                               99
477500000000E   1C                   ENDDO
477600000000     C***
477700000000     C* PARS = N° SERVIZI ESISTENTI
477800000000     C***
477900000000     C                   Z-ADD     J             PARS                           *NON MODIFICARE
478000000000     C*
478100000000IF  1C     D60SE1        IFEQ      ' '
478200000000     C     D60SE2        ANDEQ     ' '
478300000000     C     D60SE3        ANDEQ     ' '
478400000000     C     D60SE4        ANDEQ     ' '
478500000000     C     D60SE5        ANDEQ     ' '
478600000000     C                   MOVEL     'T'           SELSER                         *TUTTI I SERVIZI
478700000000     C                   MOVEL     SER           SERS
478800000000     C***
478900000000     C* SPARS = N° SERVIZI SELEZIONATI (TUTTI)
479000000000     C***
479100000000     C                   Z-ADD     J             SPARS                          *NON MODIFICARE
479200000000     C*
479300000000X   1C                   ELSE
479400000000     C*
479500000000     C                   Z-ADD     *ZEROS        J
479600000000IF  2C     D60SE1        IFNE      ' '
479700000000     C                   SETOFF                                           28
479800000000     C     D60SE1        LOOKUP    SERS(1)                                28
479900000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
480000000000     C                   ADD       1             J
480100000000     C                   MOVEL     D60SE1        SERS(J)
480200000000E   3C                   ENDIF
480300000000E   2C                   ENDIF
480400000000     C*
480500000000IF  2C     D60SE2        IFNE      ' '
480600000000     C                   SETOFF                                           28
480700000000     C     D60SE2        LOOKUP    SERS(1)                                28
480800000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
480900000000     C                   ADD       1             J
481000000000     C                   MOVEL     D60SE2        SERS(J)
481100000000E   3C                   ENDIF
481200000000E   2C                   ENDIF
481300000000     C*
481400000000IF  2C     D60SE3        IFNE      ' '
481500000000     C                   SETOFF                                           28
481600000000     C     D60SE3        LOOKUP    SERS(1)                                28
481700000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
481800000000     C                   ADD       1             J
481900000000     C                   MOVEL     D60SE3        SERS(J)
482000000000E   3C                   ENDIF
482100000000E   2C                   ENDIF
482200000000     C*
482300000000IF  2C     D60SE4        IFNE      ' '
482400000000     C                   SETOFF                                           28
482500000000     C     D60SE4        LOOKUP    SERS(1)                                28
482600000000IF  3C     *IN28         IFEQ      *OFF                                         *NON TROVATO
482700000000     C                   ADD       1             J
482800000000     C                   MOVEL     D60SE4        SERS(J)
482900000000E   3C                   ENDIF
483000000000E   2C                   ENDIF
483100000000     C***
483200000000     C* SPARS = N° SERVIZI SELEZIONATI
483300000000     C***
483400000000     C                   Z-ADD     J             SPARS                          *NON MODIFICARE
483500000000     C*
483600000000E   1C                   ENDIF
483700000000     C*-------------------------
483800000000     C* ORE DI CONSEGNA
483900000000     C*-------------------------
484000000000     C                   Z-ADD     -000          ORE(1)                         *-000
484100000000     C                   MOVEL     DORA1         DORE(1)                        *IN ANTICIPO
484200000000     C                   Z-ADD     000           ORE(2)                         *000
484300000000     C                   MOVEL     DORA2         DORE(2)                        *IN ORARIO
484400000000     C                   Z-ADD     012           ORE(3)                         *012
484500000000     C                   MOVEL     DORA3         DORE(3)                        *IN RITARDO 12 H
484600000000     C                   Z-ADD     024           ORE(4)                         *024
484700000000     C                   MOVEL     DORA4         DORE(4)                        *IN RITARDO 24 H
484800000000     C                   Z-ADD     036           ORE(5)                         *036
484900000000     C                   MOVEL     DORA5         DORE(5)                        *IN RITARDO 36 H
485000000000     C                   Z-ADD     048           ORE(6)                         *048
485100000000     C                   MOVEL     DORA6         DORE(6)                        *IN RITARDO 48 H
485200000000     C                   Z-ADD     +048          ORE(7)                         *+048
485300000000     C                   MOVEL     DORA7         DORE(7)                        *IN RIT DI OLTRE 48H
485400000000     C***
485500000000     C* PARO  = N° ORE DI CONSEGNA ESISTENTI
485600000000     C***
485700000000     C                   Z-ADD     7             PARO                           *NON MODIFICARE
485800000000     C*-------------------------
485900000000     C* FLAG RICAVI/SPEDIZIONI TIPI BOLLA
486000000000     C*-------------------------
486100000000     C                   MOVEL     '2W'          KTBCOD
486200000000     C     KEYTA2        SETLL     TABEL00F
486300000000     C     KEYTA2        READE     TABEL00F                               99
486400000000     C*
486500000000     C                   Z-ADD     *ZEROS        G                 4 0
486600000000DO  1C     *IN99         DOWEQ     *OFF
486700000000IF  2C     TBLFLG        IFEQ      *BLANKS
486800000000     C                   ADD       1             G
486900000000     C                   MOVEL     TBLKEY        FLAB(G)
487000000000     C                   MOVEL     TBLUNI        DS2W
487100000000     C                   MOVEL     §2WFSP        FSP(G)                         *FLAG SPEDIZIONI
487200000000     C                   MOVEL     §2WFRI        FRI(G)                         *FLAG RICAVI
487300000000E   2C                   ENDIF
487400000000     C*
487500000000     C     KEYTA2        READE     TABEL00F                               99
487600000000E   1C                   ENDDO
487700000000     C*-------------------------
487800000000     C* SELEZIONI PARAMETRI GENERALI
487900000000     C*-------------------------
488000000000IF  1C     D60PO1        IFEQ      ' '
488100000000     C     D60PO2        ANDEQ     ' '
488200000000     C                   MOVEL     'T'           SELPOR                         *TUTTI I PORTI
488300000000E   1C                   ENDIF
488400000000IF  1C     D60PR1        IFEQ      ' '
488500000000     C     D60PR2        ANDEQ     ' '
488600000000     C     D60PR3        ANDEQ     ' '
488700000000     C     D60PR4        ANDEQ     ' '
488800000000     C     D60PR5        ANDEQ     ' '
488900000000     C                   MOVEL     'T'           SELPRO                         *TUTTI I PRODOTTI
489000000000E   1C                   ENDIF
489100000000IF  1C     D60CT1        IFEQ      *BLANKS
489200000000     C     D60CT2        ANDEQ     *BLANKS
489300000000     C     D60CT3        ANDEQ     *BLANKS
489400000000     C     D60CT4        ANDEQ     *BLANKS
489500000000     C     D60CT5        ANDEQ     *BLANKS
489600000000     C     D60CT6        ANDEQ     *BLANKS
489700000000     C     D60CT7        ANDEQ     *BLANKS
489800000000     C     D60CT8        ANDEQ     *BLANKS
489900000000     C     D60CT9        ANDEQ     *BLANKS
490000000000     C     D60CT0        ANDEQ     *BLANKS
490100000000     C                   MOVEL     'T'           SELCTR                         *TUTTI LE TARIFFE
490200000000E   1C                   ENDIF
490300000000     C*
490400070627     C*-------------------------
490500070627      * Tipi Varie da tabella "CA" per visualizzazione  ?
490600070627     C                   MOVEL     'CA'          KTBCOD
490700070627     C                   z-add     9999999       SkOrd
490800070627     C     KEYTA2        SETLL     TABEL00F
490900070627     C     KEYTA2        READE     TABEL00F                               99
491000070627     C*
491100070627     C                   Z-ADD     0             CA                3 0
491200070627     C                   Z-ADD     0             sav_CA            3 0
491300070627DO  1C     *IN99         DOWEQ     *OFF
491400070627IF  2C     TBLFLG        IFEQ      *BLANKS
491500070627     C                   MOVEL     TBLUNI        DSCA
491600070627     C* se abilitato alla visualizzazione su statistica
491700070627     c                   if        §CAstc = 'S'
491800070627     C                   ADD       1             CA
491900070627     C* solo il byte + a destra del codice
492000070627     C                   MOVE      TBLKEY        SkVar(CA)
492100070627      * riga di Ricavi con intestazione da tabella
492200070627      *?    tranne .... che è una varia particolare non da ricavi .  ?
492300070627     c                   if        SkVar(CA) <> '#'
492400070627     C                   MOVE      'Ric.'        SkDsc(CA)
492500070627     c                   end
492600070627      *?    ...quindi attenzione....  ?
492700070627     C                   MOVEL     §CAdsc        SkDsc(CA)
492800070627     C                   eval      SkOrd(CA) = (§CAord * 1000) + CA
492900070627     C                   Z-ADD     CA            sav_CA
493000070627     c                   end
493100070627E   2C                   ENDIF
493200070627     C*
493300070627     C     KEYTA2        READE     TABEL00F                               99
493400070627E   1C                   ENDDO
493500070627     C*
493600070703      *? Un eccezione per la VARIA # che non è una varia di tassazione ma di solo C/E  ?
493700070703      *?  La aggiungo in fondo come ultima da visualizzare a mo' di chiodo poichè non                                 ?
493800070703      *?  deve essere gestita in tabella.                                 ?
493900070703     C                   ADD       1             CA
494000070703     C* solo il byte + a destra del codice
494100070703     C                   MOVE      '#'           SkVar(CA)
494200070703      *?    .... è una varia particolare non da ricavi .  ?
494300070703     C                   eval      SkDsc(CA) = 'Nr.Lasciati Avvisi     '
494400070703      *?    ... in ordinamento la metto in fondo (888).  ?
494500070703     C                   eval      SkOrd(CA) = (888 * 1000) + CA
494600070703     C                   Z-ADD     CA            sav_CA
494700070703     C*
494800070703      *? Riordina le schiere per SkOrd ascendente dove le prime 3 cifre sono l'ordine  ?
494900070627      *?  espresso sulla tabella "CA" e le restanti 3 sono l'indice delle skiere per   ?
495000070627      *?  agganciare le decodifiche in visualizzazione                                 ?
495100070627     c                   sorta     SkOrd
495200070627     C*-------------------------
495300070627     C*
495400000000     C                   ENDSR
495500020605     C*------------------------------------------------------------------------*
495600020605     C*?ORDCRER - ORDINA L'OCCUR "REGIONI-REGIONI"
495700020605     C*------------------------------------------------------------------------*
495800020605     C     ORDCRER       BEGSR
495900020605     C*
496000020605     C                   CLEAR                   SKCRER
496100020605     C*
496200020605     C* PASSO L'OCCUR NELLA SCHIERA
496300020605DO  1C     1             DO        500           YY
496400020605     C     YY            OCCUR     CRER
496500020605     C                   MOVEL     CRER          SKCRER(YY)
496600020605     C***                EVAL      SKCRER(YY) = CRER
496700020605E   1C                   ENDDO
496800020605     C*
496900020605     C* ORDINO LA SCHIERA
497000020605     C                   SORTA     SKCRER
497100020605     C*
497200020605     C* SVUOTO L'OCCUR
497300020605DO  1C     1             DO        500           J
497400020605     C     J             OCCUR     CRER
497500020605     C                   CLEAR                   REX                            *REGIONE-REGIONE
497600020605     C                   CLEAR                   REC                            *COLLI
497700020605     C                   CLEAR                   REP                            *PESO
497800020605     C                   CLEAR                   RER                            *RICAVI
497900020605     C                   CLEAR                   RES                            *SPEDIZIONI
498000020605     C                   CLEAR                   REV                            *VOLUME
498100020605E   1C                   ENDDO
498200020605     C*
498300020605     C                   Z-ADD     *ZEROS        YYY               4 0
498400020605     C*
498500020605     C* PASSO LA SCHIERA NELL'OCCUR
498600020605DO  1C     1             DO        500           YY
498700020605     C                   IF        %subst(SKCRER(YY):1:6) <> *BLANKS
498800020605     C                   ADD       1             YYY
498900020605     C     YYY           OCCUR     CRER
499000020605     C                   EVAL      CRER = SKCRER(YY)
499100020605     C                   ENDIF
499200020605E   1C                   ENDDO
499300020605     C*
499400020605     C                   ENDSR
499500000821     C*------------------------------------------------------------------------*
499600000821     C*?FINSR- OPERAZIONI FINALI
499700000821     C*------------------------------------------------------------------------*
499800000821     C     FINSR         BEGSR
499900000821     C*
500000000821     C* CHIUDE I FILE DEI PGM CHIAMATI
500100000821     C                   CLEAR                   BS69DS
500200000821     C                   CLEAR                   ACODS
500300000821     C                   CLEAR                   INDDS
500400000821     C                   CLEAR                   CLPDS
500500000821     C                   CLEAR                   CLSDS
500600000821     C                   MOVEL     'C'           I69TLA
500700000821     C                   MOVEL     KNSIF         I69SIF
500800000821     C                   CALL      'TIBS69R'
500900000821     C                   PARM                    BS69DS
501000000821     C                   PARM                    ACODS
501100000821     C                   PARM                    INDDS
501200000821     C                   PARM                    CLPDS
501300000821     C                   PARM                    CLSDS
501400000821     C*
501500000821     C                   ENDSR
501600000000     C*------------------------------------------------------------------------*
501700000000     C*?*INZSR - ROUTINE INIZIALE DI IMPOSTAZIONI E DICHIARAZIONI
501800000000     C*------------------------------------------------------------------------*
501900000000     C     *INZSR        BEGSR
502000000000     C*
502100000000     C* RICEVIMENTO PARAMETRI
502200000000     C     *ENTRY        PLIST
502300000000     C                   PARM                    KPJBA
502400000000     C                   MOVEL     KPJBU         DS60
502500000000     C*
502600000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
502700000000     C                   EXSR      INZSTD
502800020916     C*
502900020916     C* REPERISCE I DATI UTENTE
503000020916     C                   EXSR      REPDATIUTE
503100020916     C*
503200020916     C* ASSEGNA LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO VIDEO
503300020916     C                   MOVEL(P)  RSUT          $RSU
503400000000     C*
503500000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD PER SUBFILE
503600000000     C                   EXSR      INZSFL
503700000000     C*
503800000000     C* DEFINIZIONE VARIABILI DELL KLIST
503900000000     C     *LIKE         DEFINE    TBLKUT        KTBUTE                         *TABEL00F
504000000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
504100000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
504200990609     C     *LIKE         DEFINE    SDCKSC        KSCKSC                         *SISDC00F
504300000000     C     *LIKE         DEFINE    SDCANN        KSCANN
504400000000     C     *LIKE         DEFINE    SDCMES        KSCMES
504500990609     C     *LIKE         DEFINE    SDAKSC        KSAKSC                         *SISDA00F
504600000000     C     *LIKE         DEFINE    SDAANN        KSAANN
504700000000     C     *LIKE         DEFINE    SDAMES        KSAMES
504800990609     C     *LIKE         DEFINE    SDPKSC        KSPKSC                         *SISDP00F
504900000000     C     *LIKE         DEFINE    SDPANN        KSPANN
505000000000     C     *LIKE         DEFINE    SDPMES        KSPMES
505100040318     C     *LIKE         DEFINE    SDEFILE       KSEFILE                        *SISDO00F
505200040318     C     *LIKE         DEFINE    SDETRD        KSETRD                         *SISDO00F
505300040319     C     *LIKE         DEFINE    sdetrd        wtrd
505400040318     C     *LIKE         DEFINE    SDEKSC        KSEKSC                         *SISDO00F
505500040318     C     *LIKE         DEFINE    SDEANN        KSEANN
505600040318     C     *LIKE         DEFINE    SDEMES        KSEMES
505700000000     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG00F
505800000821     C     *LIKE         DEFINE    ACOKSC        WKSC                           *DI LAVORO
505900000821     C     *LIKE         DEFINE    ACORAG        WDKSC
506000050110     C     *LIKE         DEFINE    PESKSC        KPEKSC                         *CLIENTE
506100050110     C     *LIKE         DEFINE    PESANN        KPEANN                         *ANNO
506200050110     C     *LIKE         DEFINE    PESMES        KPEMES                         *MESE
506300050110     C     *LIKE         DEFINE    VLMKSC        KVLKSC                         *CLIENTE
506400050110     C     *LIKE         DEFINE    VLMANN        KVLANN                         *ANNO
506500050110     C     *LIKE         DEFINE    VLMMES        KVLMES                         *MESE
506600000000     C*
506700000000     C* DEFINIZIONE VARIABILI DI DEPOSITO
506800000000     C     *LIKE         DEFINE    SDCKSC        DEPKSC                         *CODICE CLIENTE
506900000000     C*
507000000000     C* DEFINIZIONE VARIABILI DI PASSAGGIO PER IL CONTROLLO RECORD
507100000000     C                   MOVEL     *BLANKS       $SDX              3            *FILE DA CONTROLLARE
507200000000     C                   MOVEL     *BLANKS       $ROK              1            *RECORD VALIDO O NON
507300990601     C                   Z-ADD     *ZEROS        $DAT              6 0          *DATA (AA/MM)
507400000000     C     *LIKE         DEFINE    SDCTSP        $TSP                           *TIPO SERRVIZIO
507500000000     C     *LIKE         DEFINE    SDCTPO        $TPO                           *TIPO PORTO
507600000000     C***        *LIKE     DEFN SDCTPR    $TPR             *TIPO PRODOTTO
507700000000     C     *LIKE         DEFINE    D60CT1        $CTR                           *TARIFFA (ALFANUMER.
507800000000     C     *LIKE         DEFINE    SDCFPE        $FPE                           *FASCIA DI PESO
507900000000     C     *LIKE         DEFINE    SDCRBL        $RBL                           *RAGGRUPPAMENTO BOLL
508000000000     C* PER STATISTICA DELTA
508100000000     C     *LIKE         DEFINE    SDCIRR        DELIRR                         RETTIFICHE
508200000000     C     *LIKE         DEFINE    SDCINC        DELINC                         NOTE ACCREDITO
508300000000     C*
508400000000     C* DEFINIZIONE NUMERO ELEMENTI
508500000000     C                   Z-ADD     *ZEROS        J                 4 0          *INDICE
508600000000     C                   Z-ADD     *ZEROS        JJ                4 0          *INDICE
508700000000     C                   Z-ADD     *ZEROS        JI                4 0          *INDICE
508800020605     C                   Z-ADD     *ZEROS        YY                4 0          *INDICE
508900000000     C                   Z-ADD     *ZEROS        IS                4 0          *INDICE SCHIERE VIDE
509000020520     C                   Z-ADD     *ZEROS        R                 3 0          *INDICE COMB REGIONI
509100020520     C                   Z-ADD     *ZEROS        COMBIN            3 0          *COMBINAZIONI RAGGRU
509200000000     C                   Z-ADD     *ZEROS        I                 5 0          *DESCRIZ RAGGRUPP.TO
509300000000     C                   Z-ADD     *ZEROS        X                 2 0          *MESE DA STAMPARE
509400950726     C                   Z-ADD     *ZEROS        P                 4 0          *INDICE PARTICOLARIT
509500950726     C                   Z-ADD     *ZEROS        C                 4 0          *CLIENTI SELEZIONATI
509600050112     C                   Z-ADD     *ZEROS        nrPEV             4 0          *INDICE
509700000000     C*
509800000000     C     *LIKE         DEFINE    J             SPARM                          *N° MESI SELEZIONATI
509900000000     C     *LIKE         DEFINE    J             SPARS                          *N° SERVIZI SELEZ.TI
510000000000     C     *LIKE         DEFINE    J             SPARF                          *N° FASCE SELEZIONAT
510100000000     C     *LIKE         DEFINE    J             PARS                           *N° SERVIZI ESISTENT
510200000000     C     *LIKE         DEFINE    J             PARO                           *N° ORE ESISTENTI
510300000000     C     *LIKE         DEFINE    J             PARF                           *N° FASCE ESISTENTI
510400000000     C*
510500000000     C* DEFINIZIONE VALIDITA' RECORD
510600000000     C                   MOVEL     'N'           WRECOK            1            RECORD NO VALIDO
510700000000     C                   MOVEL     'N'           ROKREG            1            REC NON VALIDO STAT
510800050110     C                   MOVEL     'N'           ROKPEV            1            REC NON VALIDO STAT
510900000000     C                   MOVEL     'N'           ROKSER            1            REC NON VALIDO STAT
511000000000     C                   MOVEL     'N'           ROKAFF            1            REC NON VALIDO STAT
511100000000     C                   MOVEL     'N'           ROKDEL            1            REC NON VALIDO STAT
511200000000     C                   MOVEL     'N'           ROKAMM            1            REC NON VALIDO STAT
511300000000     C*
511400000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
511500000000     C     KEYTAB        KLIST
511600000000     C                   KFLD                    KTBUTE                         *UTENTE
511700000000     C                   KFLD                    KTBCOD                         *CODICE TABELLA
511800000000     C                   KFLD                    KTBKEY                         *RECORD TABELLA
511900000000     C*
512000000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
512100000000     C     KEYTA2        KLIST
512200000000     C                   KFLD                    KTBUTE
512300000000     C                   KFLD                    KTBCOD
512400000000     C*
512500990609     C* CHIAVE LETTURA SISDC01L - PARZIALE
512600000000     C     KEYSDC        KLIST
512700000000     C                   KFLD                    KSCKSC                         *CLIENTE
512800000000     C                   KFLD                    KSCANN                         *ANNO
512900000000     C                   KFLD                    KSCMES                         *MESE
513000000000     C*
513100050110     C* CHIAVE LETTURA SIPES01L - PARZIALE
513200050110     C     KEYPES        KLIST
513300050110     C                   KFLD                    KPEKSC                         *CLIENTE
513400050110     C                   KFLD                    KPEANN                         *ANNO
513500050110     C                   KFLD                    KPEMES                         *MESE
513600050110     C*
513700050110     C* CHIAVE LETTURA SIVLM01L - PARZIALE
513800050110     C     KEYVLM        KLIST
513900050110     C                   KFLD                    KVLKSC                         *CLIENTE
514000050110     C                   KFLD                    KVLANN                         *ANNO
514100050110     C                   KFLD                    KVLMES                         *MESE
514200040318     C*
514300040318     C* CHIAVE LETTURA SISDE02L - TOTALE
514400040318     C     KEYSDE        KLIST
514500040318     C                   KFLD                    KSEFILE                        *DEPOSITO CLIENTE
514600040318     C                   KFLD                    KSETRD                         *DEPOSITO CLIENTE
514700040318     C                   KFLD                    KSEKSC                         *CLIENTE
514800040318     C                   KFLD                    KSEANN                         *ANNO
514900040318     C                   KFLD                    KSEMES                         *MESE
515000040318     C*
515100040318     C* CHIAVE LETTURA SISDE02L - PARZIALE
515200040318     C     KEYSDE1       KLIST
515300040318     C                   KFLD                    KSEFILE                        *DEPOSITO CLIENTE
515400040318     C                   KFLD                    KSETRD                         *DEPOSITO CLIENTE
515500040318     C                   KFLD                    KSEKSC                         *CLIENTE
515600990609     C* CHIAVE LETTURA SISDA01L - PARZIALE
515700000000     C     KEYSDA        KLIST
515800000000     C                   KFLD                    KSAKSC                         *CLIENTE
515900000000     C                   KFLD                    KSAANN                         *ANNO
516000000000     C                   KFLD                    KSAMES                         *MESE
516100000000     C*
516200990609     C* CHIAVE LETTURA SISDP01L - PARZIALE
516300000000     C     KEYSDP        KLIST
516400000000     C                   KFLD                    KSPKSC                         *CLIENTE
516500000000     C                   KFLD                    KSPANN                         *ANNO
516600000000     C                   KFLD                    KSPMES                         *MESE
516700000000     C*
516800000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
516900000000     C     KEYORG        KLIST
517000000000     C                   KFLD                    KORFIL                         *FILIALE
517100000000     C*
517200000000     C* IMPOSTO A ZERO I FINE CICLI DI LETTURA
517300990609     C                   MOVEL     '0'           $FINC             1            *FINE CICLO SISDC
517400040318     C                   MOVEL     '0'           $FINCE            1            *FINE CICLO SISDC
517500990609     C                   MOVEL     '0'           $FINA             1            *FINE CICLO SISDA
517600990609     C                   MOVEL     '0'           $FINP             1            *FINE CICLO SISDP
517700050110     C                   MOVEL     '0'           $FINPE            1            *FINE CICLO SIPES
517800050110     C                   MOVEL     '0'           $FINVL            1            *FINE CICLO SIVLM
517900000000     C*
518000000000     C* IMPOSTO A ZERO I PASSAGGI DI MEMORIZZAZIONE
518100000000     C                   MOVEL     '0'           PASKSC            1            SALDI CLIENTE
518200050110     C                   MOVEL     '0'           PASPEV            1            SALDI PESI-VOLUMI
518300000000     C                   MOVEL     '0'           PASREG            1            SALDI REG-REG
518400000000     C                   MOVEL     '0'           PASSER            1            SALDI SERVIZIO BOLLE
518500000000     C                   MOVEL     '0'           PASAFF            1            SALDI AFFIDABILITA'
518600000000     C                   MOVEL     '0'           PASDEL            1            SALDI DELTA CLIENTE
518700000000     C                   MOVEL     '0'           PASAMM            1            SALDI AMMINISTRAZION
518800000000     C*
518900000000     C* IMPOSTO A *BLANKS I CAMPI DI SELEZIONE
519000000000     C                   MOVEL     '0'           SELSER            1            SELEZ SERVIZI (' ',T
519100000000     C                   MOVEL     '0'           SELPOR            1            SELEZ PORTO   (' ',T
519200000000     C                   MOVEL     '0'           SELPRO            1            SELEZ PRODOTTI(' ',T
519300000000     C                   MOVEL     '0'           SELCTR            1            SELEZ TARIFFE (' ',T
519400000000     C                   MOVEL     '0'           SELFPE            1            SELEZ FASCE   (' ',T
519500000000     C                   MOVEL     '0'           SOLAFF            1            SOLO STAT. "AFFID.TA
519600000000     C                   MOVEL     '0'           MAIAFF            1            MAI  STAT. "AFFID.TA
519700000000     C*
519800000000     C* CONRIG VIENE AZZERATO SUBITO PRIMA DI OGNI STAMPA DI TESTATA; DOPO OGNI
519900000000     C* ALTRA WRITE (TESTA COMPRESA) GLI VIENE AGGIUNTO IL N° DI RIGHE STAMPATE
520000000000     C                   Z-ADD     *ZEROS        CONRIG            2 0          *CONTATORE RIGHE PAG
520100000000     C*
520200000000     C* SUDDIVISIONE DATE VIDEO IN ANNI E MESI
520300000000     C                   Z-ADD     D60DA2        DATFIN                         *DS DATA FINALE
520400000000     C                   Z-ADD     D60DAT        DATINI                         *DS DATA INIZIALE
520500000821     C*
520600000821     C* IMNPOSTA DATA / ORA CORRENTE
520700000821     C                   TIME                    N14              14 0          *ORA (6)+ DATA (8)
520800000821     C                   MOVE      N14           G08DAT            8 0          *DATA (8) IN G/M/AA
520900000821     C                   Z-ADD     *ZEROS        G08INV
521000000821     C                   MOVEL     '0'           G08ERR
521100000821     C                   CALL      'XSRDA8'
521200000821     C                   PARM                    WLBDA8
521300000821     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA (8) IN AA/M/G
521400000000     C*
521500000000     C* CONTROLLO SE E' STATA SCELTA SOLO LA STATISTICA "AFFIDABILITA" PER
521600000000     C* SALTARE IL CICLO DI LETTURA CHE A QUESTA STATISTICA NON INTERESSA
521700000000IF  1C     D60ST1        IFEQ      'N'                                          *NO "REGIONE-REGIONE
521800050110     C     D60STp        ANDEQ     'N'                                          *NO "PESI/VOLUMI"
521900000000     C     D60ST2        ANDEQ     'N'                                          *NO "SERVIZIO BOLLE"
522000000000     C     D60ST3        ANDEQ     'S'                                          *SI "AFFIDABILITA'"
522100000000     C     D60ST4        ANDEQ     'N'                                          *NO "DELTA CLIENTE"
522200000000     C     D60ST5        ANDEQ     'N'                                          *NO "AMMINISTRAZIONE
522300000000     C                   MOVEL     '1'           SOLAFF                         *SOLO STAT "AFFID.TA
522400000000E   1C                   ENDIF
522500000000     C*
522600000000     C* CONTROLLO SE NON E' MAI STATA SCELTA LA STATISTICA "AFFIDABILITA" PER
522700000000     C* SALTARE IL CICLO DI LETTURA CHE ALLE ALTRE STATISTICHE NON INTERESSA
522800000000IF  1C     D60ST3        IFEQ      'N'
522900000000     C                   MOVEL     '1'           MAIAFF                         *MAI LA "AFFID.TA'"
523000000000E   1C                   ENDIF
523100000000     C*
523200000000     C* CARICA TABELLE (FASCE, REGIONI, TIPI SERVIZIO)
523300000000     C                   EXSR      CARTAB
523400000000     C*
523500000000     C* IMPOSTO CAMPI TESTATA VIDEO E CONTROL FISSI
523600000000     C                   Z-ADD     MESFIN        J
523700000000     C                   MOVEL     DMM(J)        VCCMMF                         *MESE FINALE
523800000000     C                   MOVEL     ANNFIN        VCCAAF                         *ANNO FINALE
523900000000     C                   Z-ADD     MESINI        J
524000000000     C                   MOVEL     DMM(J)        VCCMMI                         *MESE INIZIALE
524100000000     C                   MOVEL     ANNINI        VCCAAI                         *ANNO INIZIALE
524200000000     C                   MOVEL     COSVAF        VCCVAF                         *"VALORI DI "
524300000000     C                   MOVEL     COSVAI        VCCVAI                         *"TOTALI DAL"
524400000821     C*
524500000821     C* IMPOSTA IL CLIENTE/I DA ELABORARE
524600000821     C                   CLEAR                   CLI
524700000821     C                   CLEAR                   OCLI
524800000821IF  1C     D60OP0        IFEQ      'VIS'
524900000821     C                   MOVEA     D60KSC        KSA                            *DS INPUT -> SCH ALF
525000000821IF  2C     D60SCE        IFEQ      '1'                                          *UN SOLO CLIENTE
525100000821     C                   MOVE      KSA(1)        DSKSA                          *SCH ALF -> DS ALF
525200000821     C                   Z-ADD     DSKSN         CLI(1)                         *DS NUM -> SCH NUM
525300000821     C                   Z-ADD     CLI(1)        VCCKSC
525400000821     C                   Z-ADD     VCCKSC        WKSC                           *REPERISCE R.SOCIALE
525500000821     C                   EXSR      REPRAG
525600000821     C                   MOVEL     WDKSC         VCCRAG
525700000821E   2C                   ENDIF
525800000821IF  2C     D60SCE        IFEQ      '2'                                          * + CLIENTI INSIEME
525900050117DO  3C**** 1             DO        30            C
526000050117DO  3C     1             DO        28            C
526100000821     C                   MOVE      KSA(C)        DSKSA                          *SCH ALF -> DS ALF
526200000821     C                   Z-ADD     DSKSN         CLI(C)                         *DS NUM -> SCH NUM
526300000821E   3C                   ENDDO
526400000821     C                   Z-ADD     *ZEROS        VCCKSC
526500000821     C                   MOVEL     DSTA(10)      VCCRAG
526600000821E   2C                   ENDIF
526700000821IF  2C     D60SCE        IFEQ      '3'                                          *CLIENTE UNIFICANTE
526800000821     c                   CLEAR                   SKSC8
526900000821     C                   CLEAR                   BS10DS
527000000821     C                   Z-ADD     DATCOR        D10DRF                         *DATA RIFERIMENTO
527100000821     C                   MOVEL     'ST'          D10TLE                         *TIPO LEGAME
527200000821     C                   MOVEL     'F'           D10PAF                         *CERCA figli
527300000821     C                   MOVE      KSA(1)        DSKSA                          *SCH ALF -> DS ALF
527400000821     C                   Z-ADD     DSKSN         N7                7 0          *DS NUM -> SCH NUM
527500000821     C                   Z-ADD     N7            D10COD                         *CODICE CLIENTE
527600000821     C                   CALL      'TIBS10R'
527700000821     C                   PARM                    BS10DS
527800000821IF  3C     D10ERR        IFEQ      *BLANKS                                      *NO ERRORE
527900000821     C                   Z-ADD     SKSC11        SKSC8                          *CLIENTI PADRE+FIGLI
528000000821DO  4C     1             DO        500           C
528100000821IF  5C     SKSC8(C)      IFGT      *ZEROS
528200000821     C                   MOVE      SKSC8(C)      CLI(C)                         *CLIENTI DETTAGLIO
528300000821E   5C                   ENDIF
528400000821E   4C                   ENDDO
528500000821X   3C                   ELSE                                                   *SI ERRORE
528600020826     C                   Z-ADD     N7            CLI(1)                         *SE STESSO
528700000821E   3C                   ENDIF
528800000821     C                   Z-ADD     CLI(1)        VCCKSC
528900000821     C                   MOVEL     DSTA(9)       VCCRAG
529000000821E   2C                   ENDIF
529100000821     C*
529200000821     C* LANCIA ROUTINE PER REPERIRE I DATI UTENTE
529300000821     C                   Z-ADD     CLI           OCLI
529400000821DO  2C     1             DO        500           C
529500000821IF  3C     CLI(C)        IFGT      *ZEROS
529600000821     C                   Z-ADD     CLI(C)        OCLI(C)
529700000821E   3C                   ENDIF
529800000821E   2C                   ENDDO
529900000821     C                   Z-ADD     OCLI          CLI                            *CLIENTI COMPATTI
530000000821E   1C                   ENDIF
530100000000     C*
530200000000     C* MEMORIZZA I SALDI CLIENTE IN SCHIERE
530300000000     C                   EXSR      CICSAL
530400000000     C*
530500000000     C* CARICA PRIMO VIDEO (IN ORDINE QUELLO RICHIESTO)
530600000000SL  1C                   SELECT
530700000000     C     D60ST2        WHENEQ    'S'                                          *TOTALI
530800000000     C                   EXSR      CARVS
530900050110     C     D60STp        WHENEQ    'S'                                          *PESI/VOLUMI
531000050110     C                   EXSR      CARPV
531100000000     C     D60ST1        WHENEQ    'S'                                          *REGIONE-REGIONE
531200000000     C                   EXSR      CARVR
531300000000     C     D60ST4        WHENEQ    'S'                                          *DELTA
531400000000     C                   EXSR      CARVD
531500000000     C     D60ST3        WHENEQ    'S'                                          *AFFIDABILITA'
531600000000     C                   EXSR      CARVA
531700000000     C     D60ST5        WHENEQ    'S'                                          *AMMINISTRAZIONE
531800000000     C                   EXSR      CARVM
531900000000E   1C                   ENDSL
532000000000     C*
532100000000     C                   ENDSR
532200020916     C*--------------------------------------------------------------------------------------------*
532300020916     C* REPERISCE DATI UTENTE
532400020916     C*--------------------------------------------------------------------------------------------*
532500020916     C     REPDATIUTE    BEGSR
532600020916     C*
532700020916     C* REimposta le variabili di lavoro
532800020916     C                   CLEAR                   TIBS34DS
532900020916     C                   CLEAR                   AZUTEDS
533000020916     C                   CLEAR                   DDATIUTE
533100020916     C*
533200020916     C* Reperisce i dati del profilo in ingresso
533300020916     C     *DTAARA       DEFINE    §azute        azuteds
533400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
533500020916     C                   IN(E)     *DTAARA
533600020916if  1C                   IF        %Error                                       *NO errori
533700020916     c                   EVAL      I34Tla = 'L'
533800020916     C                   CALL      'TIBS34R'
533900020916     C                   PARM                    Tibs34Ds
534000020916     C                   IN        *DTAARA
534100020916e   1C                   ENDIF
534200020916     C*
534300020916     C                   ENDSR
534400000000     C*------------------------------------------------------------------------*
534500000000     C*?§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
534600000000     C*?§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
534700000000     C* LA PARTE DI SPECIFICHE "C" SOTTOSTANTE SI RIFA' A PARTICOLARI STANDARDS
534800000000     C* DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI NON E' NECESSARIO
534900000000     C* MODIFICARLA; NEL CASO FARE ATTENZIONE.
535000000000     C*?§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
535100000000     C*?§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
535200000000     C*------------------------------------------------------------------------*
535300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
535400000000     C*------------------------------------------------------------------------*
535500000000     C     INZSTD        BEGSR
535600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS STDKDS (TASTI PREMUTI)
535700000000     C                   CALL      'STDKEY'
535800000000     C                   PARM                    STDKDS
535900000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
536000000000     C* INIZIALIZZO FLAG CONTROLLO PGM
536100000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
536200000000     C                   MOVEL     '0'           $F03              1            *PREMUTO TASTO F03
536300000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE VIDEO
536400000000     C                   MOVEL     *BLANKS       $MSG                           *MESSAGGI
536500000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
536600000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
536700000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROG,G=GESTIO
536800000000     C                   ENDSR
536900000000     C*------------------------------------------------------------------------*
537000000000     C* INZSFL - ROUTINE INIZIALE STANDARD PER SUBFILE
537100000000     C*------------------------------------------------------------------------*
537200000000     C     INZSFL        BEGSR
537300000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA PER IL SUBFILE
537400000000     C     *LIKE         DEFINE    VCCNBR        SFLPAG
537500000000     C     *LIKE         DEFINE    VCCNBR        SFLEOF
537600000000     C     *LIKE         DEFINE    VCCNBR        SFLERR
537700000000     C     *LIKE         DEFINE    VCCNBR        SFLULT
537800000000     C     *LIKE         DEFINE    VCCNBR        WFLULT
537900000000     C     *LIKE         DEFINE    VCCNBR        SFLRIG
538000000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
538100000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
538200000000     C                   Z-ADD     16            SFLPAG
538300000000     C                   ENDSR
538400000000     C*------------------------------------------------------------------------*
538500000000     C* FUNSFL - LANCIA SUBROUTINE STANDARD SUBFILE
538600000000     C*------------------------------------------------------------------------*
538700000000     C     FUNSFL        BEGSR
538800000000     C                   SELECT
538900000000     C     KEY           WHENEQ    F3
539000000000     C                   MOVEL     '1'           $F03                           *FLAG F03 PREMUTO
539100000000     C                   MOVEL     '1'           $FINE                          *FLAG FINE PGM
539200000000     C     KEY           WHENEQ    F5
539300000000     C                   EXSR      POSSFL                                       *CARICA SUBFILE
539400000000     C     KEY           WHENEQ    F12
539500000000     C                   MOVEL     '1'           $FINE                          *FLAG FINE PGM
539600000000     C     KEY           WHENEQ    F17
539700000000     C                   Z-ADD     1             VCCNBR                         *SFLRCDNBR 1° PAGINA
539800000000     C     KEY           WHENEQ    F18
539900000000     C                   Z-ADD     SFLULT        VCCNBR                         *SFLRCDNBR ULT.PAGIN
540000000000     C     KEY           WHENEQ    F24
540100000000     C                   EXSR      OUTFUN                                       *VISUALIZZA FUNZIONI
540200000000     C     KEY           WHENEQ    ROLLUP
540300000000     C                   EXSR      CARSFL                                       *CARICA SFL DAL FILE
540400000000     C                   OTHER
540500000000     C                   MOVEL     MSG(2)        $MSG
540600000000     C                   ENDSL
540700000000     C                   ENDSR
540800000000     C*------------------------------------------------------------------------*
540900000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
541000000000     C*------------------------------------------------------------------------*
541100000000     C     POSSFL        BEGSR
541200000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
541300000000     C                   EXSR      CLRSFL
541400000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
541500000000     C                   EXSR      SETFIL
541600000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
541700000000     C     SFLEOF        IFEQ      0
541800000000     C                   EXSR      CARSFL
541900000000     C                   ENDIF
542000000000     C                   ENDSR
542100000000     C*------------------------------------------------------------------------*
542200000000     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
542300000000     C*------------------------------------------------------------------------*
542400000000     C     CLRSFL        BEGSR
542500000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
542600000000     C* (0=NON FINE FILE  1=FINE FILE)
542700000000     C                   Z-ADD     0             SFLEOF
542800000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
542900000000     C                   Z-ADD     0             SFLULT
543000000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
543100000000     C                   MOVEL     '0'           *IN31
543200000000     C                   WRITE     VCTL
543300000000     C                   MOVEL     '1'           *IN31
543400000000     C                   ENDSR
543500000000     C*------------------------------------------------------------------------*
543600000000     C* CARSFL - CARICAMENTO SFL
543700000000     C*------------------------------------------------------------------------*
543800000000     C     CARSFL        BEGSR
543900000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
544000000000     C     SFLULT        IFEQ      0
544100000000     C                   EXSR      SETFIL
544200000000     C                   ENDIF
544300000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
544400000000     C                   Z-ADD     SFLULT        WFLULT
544500000000     C* CICLO:
544600000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
544700000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
544800000000     C                   Z-ADD     1             SFLRIG
544900000000     C     SFLRIG        DOWLE     SFLPAG                                       --- 1 -->
545000000000     C     SFLEOF        ANDEQ     0
545100000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
545200000000     C                   EXSR      IRESFL
545300000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
545400000000     C                   ADD       1             SFLULT
545500000000     C* SCRITTURA RECORD SFL
545600000000     C                   WRITE     VSFL
545700000000     C                   ADD       1             SFLRIG
545800000000     C* LETTURA FILE DA CARICARE
545900000000     C                   EXSR      LETFIL
546000000000     C                   ENDDO                                                  <-- 1 ---
546100000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
546200000000     C     WFLULT        IFNE      SFLULT
546300000000     C                   Z-ADD     WFLULT        VCCNBR
546400000000     C                   ADD       1             VCCNBR
546500000000     C                   ENDIF
546600000000     C* SE USCITO DAL LOOP CON FINE FILE MANDO MESSAGGIO DI FINE ELENCO
546700000000     C     SFLEOF        IFEQ      1                                            --- 1 -->
546800000000     C     SFLULT        ANDGT     *ZEROS
546900000000     C*--------------------MOVELMSG,5     $MSG
547000000000     C* AGGIORNO L'ULTIMO RECORD DEL SFL PER POSIZIONARE IL CURSORE
547100000000     C     SFLULT        CHAIN     VSFL                               99
547200000000     C     *IN99         IFEQ      '0'
547300000000     C                   SETON                                        37
547400000000     C                   UPDATE    VSFL
547500000000     C                   SETOFF                                       37
547600000000     C                   ENDIF
547700000000     C                   ENDIF                                                  <-- 1 ---
547800000000     C                   ENDSR
547900000000     C*------------------------------------------------------------------------*
548000000000     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
548100000000     C*------------------------------------------------------------------------*
548200000000     C     OUTSFL        BEGSR
548300000000     C* SE C'E' UN MESSAGGIO ACCENDO INDICATORE DI EMISSIONE MESSAGGIO A VIDEO
548400000000     C     $MSG          IFNE      *BLANKS
548500000000     C                   SETON                                        38
548600000000     C                   END
548700000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
548800000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
548900000000     C     SFLEOF        IFEQ      0
549000000000     C                   MOVEL     '0'           *IN33
549100000000     C                   ELSE
549200000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
549300000000     C                   MOVEL     '1'           *IN33
549400000000     C                   ENDIF
549500000000     C* EMISSIONE SUBFILE
549600000000     C     SFLULT        IFLE      0
549700000000     C*  --> CASO DI SFL VUOTO
549800000000     C                   MOVEL     '0'           *IN30                          *NON VISUALIZZA SFL
549900000000     C                   WRITE     VVUOTO                                       *'NON VI SONO DATI'
550000000000     C                   WRITE     VCTL                                         *SFL CONTROL
550100000000     C                   READ      VCTL                                   99    *SFL CONTROL
550200000000     C                   ELSE
550300000000     C*  --> CASO DI SFL CON RECORDS
550400000000     C                   MOVEL     '1'           *IN30                          *NON VISUALIZZA SFL
550500000000     C                   WRITE     VCTL                                         *SFL CONTROL
550600000000     C                   READ      VCTL                                   99    *SFL CONTROL
550700000000     C                   ENDIF
550800000000     C* MEMORIZZA IL N.RECORD DELLA PRIMA RIGA PAG.CORRENTE SFL
550900000000     C* (NEL CASO VENGANO UTILIZZATI ROLLUP E ROLLDOWN PERDEREI IL REC.CORRENTE)
551000000000     C                   Z-ADD     FRTSFL        VCCNBR
551100000000     C* PULISCE CAMPO MESSAGGIO E RELATIVO INDICATORE
551200000000     C                   MOVEL     *BLANKS       $MSG
551300000000     C                   SETOFF                                       38
551400000000     C* AZZERA TUTTI GLI INDICATORI DEL VIDEO
551500000000     C                   EXSR      $AZIND
551600000000     C                   ENDSR
551700000000     C*------------------------------------------------------------------------*
551800000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
551900000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
552000000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
552100000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
552200000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
552300000000     C*------------------------------------------------------------------------*
552400000000     C     CARFUN        BEGSR
552500000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
552600000000     C     1             DO        31            I                 5 0          --- 1 -->
552700000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
552800000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
552900000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
553000020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
553100000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
553200000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
553300000000     C                   ELSE                                                   *INTERROG.
553400000000     C                   MOVEL     FUNASI        FAT(I)
553500000000     C                   ENDIF
553600000000     C                   ELSE                                                   <-- 2 --> *FILIALE
553700000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
553800000000     C                   MOVEL     FUNAFG        FAT(I)
553900000000     C                   ELSE                                                   *INTERROG.
554000000000     C                   MOVEL     FUNAFI        FAT(I)
554100000000     C                   ENDIF
554200000000     C                   ENDIF                                                  <-- 2 ---
554300000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
554400000000     C                   MOVEL     $USER         $EDP              3            *PROFILO
554500000000     C     $EDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
554600000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
554700000000     C                   ELSE
554800000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
554900000000     C                   ENDIF
555000000000     C                   ENDIF
555100000000     C                   ENDDO                                                  <-- 1 ---
555200000000     C                   ENDSR
555300000000     C*------------------------------------------------------------------------*
555400000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
555500000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
555600000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
555700000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
555800000000     C*------------------------------------------------------------------------*
555900000000     C     OUTFUN        BEGSR
556000000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
556100000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
556200000000     C                   ENDDO
556300000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
556400000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
556500000000     C     1             DO        31            I
556600000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
556700000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
556800000000     C                   ADD       1             FUNNAT
556900000000     C                   ENDIF
557000000000     C                   ENDDO
557100000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
557200000000     C     FUNNAT        IFGT      8
557300000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
557400000000     C                   ENDIF
557500000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
557600000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
557700000000     C                   Z-ADD     8             FUNVIS            5 0
557800000000     C     FAT(23)       IFEQ      'S'
557900000000     C                   SUB       1             FUNVIS
558000000000     C                   ENDIF
558100000000     C     FAT(24)       IFEQ      'S'
558200000000     C                   SUB       1             FUNVIS
558300000000     C                   ENDIF
558400000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
558500000000     C                   Z-ADD     *ZEROS        W                 5 0
558600000000     C                   ADD       1             FUNULT
558700000000     C     FUNULT        IFGT      31
558800000000     C                   Z-ADD     1             FUNULT
558900000000     C                   ENDIF
559000000000     C* RICERCO LE FUNZIONI ATTIVE E LE EMETTO A VIDEO
559100000000     C     FUNULT        DO        31            I                              --- 1 -->
559200000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
559300000000     C     W             IFLT      FUNVIS                                       --- 2 -->
559400000000     C* SE FUNZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
559500000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
559600000000     C     FDE(I)        ANDNE     *BLANKS
559700000000     C     I             IFEQ      23                                           *TOLGO F23
559800000000     C     I             OREQ      24                                           *E F24
559900000000     C                   ELSE
560000000000     C                   ADD       1             W
560100000000     C                   MOVEL     FDE(I)        FUV(W)
560200000000     C                   ENDIF
560300000000     C                   ENDIF                                                  <-- 3 ---
560400000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
560500000000     C                   Z-ADD     I             FUNULT
560600000000     C                   ENDIF                                                  <-- 2 ---
560700000000     C                   ENDDO                                                  <-- 1 ---
560800000000     C* TRASFERISCO LE FUNZIONI DALLA SCHIERA AI CAMPI VIDEO
560900000000     C                   MOVEL     FUV(1)        VFD001
561000000000     C                   MOVEL     FUV(2)        VFD002
561100000000     C                   MOVEL     FUV(3)        VFD003
561200000000     C                   MOVEL     FUV(4)        VFD004
561300000000     C                   MOVEL     FUV(5)        VFD005
561400000000     C                   MOVEL     FUV(6)        VFD006
561500000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
561600000000     C     FAT(24)       ANDEQ     'N'
561700000000     C                   MOVEL     FUV(7)        VFD007
561800000000     C                   MOVEL     FUV(8)        VFD008
561900000000     C                   ELSE                                                   <-- 1 -->
562000000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
562100000000     C     FAT(24)       ANDEQ     'S'
562200000000     C                   MOVEL     FDE(23)       VFD007
562300000000     C                   MOVEL     FDE(24)       VFD008
562400000000     C                   ELSE                                                   <-- 2 -->
562500000000     C                   MOVEL     FUV(7)        VFD007
562600000000     C     FAT(23)       IFEQ      'S'
562700000000     C                   MOVEL     FDE(23)       VFD008
562800000000     C                   ELSE
562900000000     C                   MOVEL     FDE(24)       VFD008
563000000000     C                   ENDIF
563100000000     C                   ENDIF                                                  <-- 2 ---
563200000000     C                   ENDIF                                                  <-- 1 ---
563300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
563400000000     C                   ENDSR
563500000000     C*------------------------------------------------------------------------*
563600000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
563700000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
563800000000     C*           OUTPUT: - FUNOK  (FUNZIONE OK S/N)
563900000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
564000000000     C*------------------------------------------------------------------------*
564100000000     C     CONFUN        BEGSR
564200000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
564300000000     C     1             DO        31            I                              --- 1 -->
564400000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
564500000000     C* CONTROLLO SE IL TASTO E' ATTIVO
564600000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
564700000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
564800000000     C                   MOVEL     '1'           $ERR
564900000000     C                   ENDIF                                                  <-- 3 ---
565000000000     C                   ENDIF                                                  <-- 2 ---
565100000000     C                   ENDDO                                                  <-- 1 ---
565200000000     C                   ENDSR
565300000000     C*------------------------------------------------------------------------*
565400000000     C*  $AZIND - AZZERA INDICATORI VIDEO
565500000000     C*------------------------------------------------------------------------*
565600000000     C     $AZIND        BEGSR
565700000000     C     40            DO        50            I
565800000000     C                   MOVE      '0'           *IN(I)
565900000000     C                   ENDDO
566000000000     C                   ENDSR
566100000000     O*------------------------------------------------------------------------*
566200950726** SCHIERA: MSG  (MESSAGGI)
566300010920TISE65R- Tasto funzionale non valido                                           1
566400010920TISE65R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
566500010920TISE65R- 3
566600010920TISE65R- 4
566700010920TISE65R- Fine elenco.                                                          5
566800010920TISE65R- 6
566900010920TISE65R- 7
567000010920TISE65R- 8
567100010920TISE65R- 9
567200010920TISE65R- 10
567300010920TISE65R- 11
567400010920TISE65R- 12
567500010920TISE65R- 13
567600010920TISE65R- 14
567700010920TISE65R- 15
567800010920TISE65R- 16
567900010920TISE65R- 17
568000010920TISE65R- 18
568100010920TISE65R- 19
568200010920TISE65R- 20
568300000000** FUN1 (PARAMETRI)       SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
568400000000F1    -                   -    NN     NN
568500000000F2    -                   -    NN     NN
568600000000F3    -F3=Fine            -    SS     SS
568700000000F4    -                   -    NN     NN
568800000000F5    -                   -    NN     NN
568900000000F6    -                   -    NN     NN
569000000000F7    -                   -    NN     NN
569100000000F8    -                   -    NN     NN
569200000000F9    -                   -    NN     NN
569300000000F10   -                   -    NN     NN
569400000000F11   -                   -    NN     NN
569500000000F12   -F12=Ritorno        -    SS     SS
569600000000F13   -                   -    NN     NN
569700000000F14   -                   -    NN     NN
569800000000F15   -                   -    NN     NN
569900000000F16   -                   -    NN     NN
570000000000F17   -F17=Inizio dati    -    SS     SS
570100000000F18   -F18=Fine dati      -    SS     SS
570200000000F19   -                   -    NN     NN
570300000000F20   -                   -    NN     NN
570400000000F21   -                   -    NN     NN
570500000000F22   -                   -    NN     NN
570600000000F23   -                   -    NN     NN
570700000000F24   -F24=Altri tasti    -    NN     NN
570800000000ENTER -                   -    SS     SS
570900000000ROLLUP-                   -    SS     SS
571000000000ROLLDN-                   -    SS     SS
571100000000HELP  -                   -    NN     NN
571200000000PRINT -                   -    NN     NN
571300000000BSPACE-                   -    NN     NN
571400000000CLEAR -                   -    NN     NN
571500000000**   DMM - DECODIFICHE MESI DELL'ANNO
571600990602GEN      1
571700990602FEB      2
571800990602MAR      3
571900990602APR      4
572000990602MAG      5
572100990602GIU      6
572200990602LUG      7
572300990602AGO      8
572400990602SET      9
572500990602OTT      10
572600990602NOV      11
572700990602DIC      12
572800000000**   DRI - DECODIFICHE RIGA DI STAMPA
572900000000   REGIONE - REGIONE   1
573000000000      T O T A L I      2
573100000000     AFFIDABILITA'     3
573200000000       D E L T A       4
573300000000    AMMINISTRAZIONE    5
573400050110     PESI / VOLUMI     6
573500000000**   DCA - DECODIFICHE CAMPI DA STAMPARE
573600000000Spedizioni 1
573700000000Colli      2
573800000000Peso (Kg)  3
573900000000Volume     4
574000000000Ricavo     5
574100000000TOTALE     6
574200000000Ricavi     7
574300000000Spediz     8
574400000000Delta      9
574500000000TOTALI     10
57460000000011
57470000000012
57480000000013
57490000000014
57500000000015
575100000000**   DME - DECODIFICHE RIGHE IN STAMPA
575200000000Media Colli    X Sped  1
575300000000      Kg       X Sped  2
575400000000      M.Cubi   X Sped  3
575500000000Media Kg       X Collo 4
575600000000      M.Cubi   X Collo 5
575700000000Media Incasso  X Sped  6
575800000000               X Collo 7
575900000000               X Q.Le  8
576000000000               X M.Cubo9
576100000000Fatturato Totale       10
576200000000Note di Accredito      11
576300000000Rettifiche Fatturazione12
576400000000% sul Fatturato        13
576500000000Giacenze           Ric.14
576600000000                   Sped15
576700000000Di Cui:                16
576800000000Cons ai Piani      Ric.17
576900000000Cons Appuntamento  Ric.18
577000000000Cons Supermercati  Ric.19
577100000000Cons Fuori Misura  Ric.20
577200000000Assicurazioni      Ric.21
577300000000Contrassegni       Ric.22
57740000000023
577500000000Spediz consegnate in % 24
577600091214 C   E+H     D     T   25
577700950206C O L L I -------------26
577800950206P E S O ---------------27
577900950206R I C A V I -----------28
578000950206S P E D I Z I O N I ---29
578100950206V O L U M E -----------30
578200010920Cons Centro stor.  Ric.31
578300010920Cons Loc. disag.   Ric.32
578400010920Cons DDT           Ric.33
578500010920Ritiri             Ric.34
578600010920Cons Isole         Ric.35
578700020205Cons Disagiata     Ric.36
578800020205Ritiri Annullati   Ric.37
578900020205Ricerca d'archivio Ric.38
579000020205Diritti Pesatura   Ric.39
579100040319Colli orig.bancali "O" 40
579200040319Bancali tipo "O"       41
579300040319Bancali tipo "B"       42
579400051018Nr.Lasciati Avvisi Ric.43
579500040924P.O.D. Immage      Ric.44
579600050118Ann. P.Assegnato   Ric.45
579700050118Dirottamenti       Ric.46
579800050118Ritiro Bancali     Ric.47
579900050118Rec.C/Ass          Ric.48
580000050118ORM Commissionato  Ric.49
580100051018L.Avviso AddebitatiRic.50
580200950726**   DSTA - DESCRIZIONE STATISTICA SELEZIONATA
5803009507261
5804009507262
5805009507263
5806009507264
5807009507265
5808009507266
5809000008217
581000000821CLIENTE                                                                        8
581100000821CLIENTE UNIFICANTE                                                             9
581200950726STAT. PIU' CLIENTI INSIEME                                                    10
581300050113**   DPEV - DESCRIZIONE STATISTICA PESI/VOLUMI
581400050113------------- PESI VDL 1
581500050119----------- VOLUMI VDL 2
581600050119Nr.Colli VDL           3
581700050119Nr.Colli Boll.         4
581800050119 % Colli Rilev.        5
581900050119 % Scost Ril./Boll.    6
582000050119   VDL                 7
582100050119   Boll.               8
