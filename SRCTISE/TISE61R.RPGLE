000100000000     H*------------------------------------------------------------------------*
000200000000     H*                    - STATISTICA CLIENTI -      RICHIESTA PARAMETRI     *
000300000000     H*------------------------------------------------------------------------*
000400000000     H* VALORI DELLA DS DI PROCEDURA (DS60) IN INPUT:
000500000000     H*
000600000817     H* - LIV.PROCEDURA = 'VIS' -> SE SCELTA INTERATTIVA
000700000000     H* - LIV.PROGRAMMA = 'F21' -> SE CHIESTA STAMPA STATISTICA CLIENTE
000800951005     H*                   'F06' -> SE CHIESTA VISUALIZZAZIONE STATISTICA CLIENTE
000900000817     H* - LIV.PROCEDURA = 'STA' -> SE SCELTA A MENU'
001000000000     H* - LIV.PROGRAMMA = 'REG' -> SE CHIESTA STAT REGIONE-REGIONE DA MENU'
001100050113     H*                   'PEV' -> SE CHIESTA STAT PESI/VOLUMI DA MENU'
001200000000     H*                   'SER' -> SE CHIESTA STAT SERVIZIO BOLLE DA MENU'
001300000000     H*                   'AFF' -> SE CHIESTA STAT AFFIDABILITA' DA MENU'
001400000000     H*                   'DEL' -> SE CHIESTA STAT DELTA CLIENTE DA MENU'
001500000000     H*                   'AMM' -> SE CHIESTA STAT AMMINISTRAZIONE DA MENU'
001600000000     H*                   'DEF' -> SE CHIESTA STAT DELTA FILIALE DA MENU'
001700000000     H*                   'DEC' -> SE CHIESTA STAT DELTA COMMERCIALE DA MENU'
001800020523     H* - LIV.PROCEDURA = 'STA' -> SE SCELTA A MENU'
001900020523     H* - LIV.PROGRAMMA = 'STT' -> SE SCELTA A MENU E STAMPA TUTTE STATISTICHE X TUTTI I CLIENTI'
002000000000     H*------------------------------------------------------------------------*
002100000000     H DECEDIT('0,') DATEDIT(*DMY.)
002200000000     H*------------------------------------------------------------------------*
002300000000     H* INDICATORI USATI
002400000000     H*------------------------------------------------------------------------*
002500000000     H* 01    - PER RICERCHE (SCAN,...)
002600000000     H* 02    - PROTEZIONE RICH STAT, SE VOGLIO LA STAMPA ('STA' O 'VIS+'F21')
002700960604     H* DAL 04/06 ANCHE IN STAMPA (DA CLIENTE) POSSO CHIEDERE LE <> STATISTICHE
002800000000     H* 40    - SFLCLR PER SUBFILE MESSAGGI
002900000000     H* 44    - SFLEND PER SUBFILE MESSAGGI
003000000000     H* 50/98 - ERRORI SUI CAMPI (TRANNE: 52,53,54,55,56,57,59)
003100000000     H* 99    - PER LETTURE
003200000000     F*------------------------------------------------------------------------*
003300000000     F* DATA BASE
003400000000     F*------------------------------------------------------------------------*
003500950724     FTABEL00F  IF   E           K DISK
003600000000     FTNTAM01L  IF   E           K DISK
003700010920     FTISE61D   CF   E             WORKSTN INFDS(DEVDS1)
003800010920     F                                     SFILE(SE61DVM:MSGULT)
003900000000     D*------------------------------------------------------------------------*
004000000000     D* SCHIERE
004100000000     D*------------------------------------------------------------------------*
004200000817     D DSTA            S             28    DIM(15) CTDATA PERRCD(1)             *DESCRIZIONI A VIDEO
004300000000     D MSG             S             78    DIM(30) CTDATA PERRCD(1)             *MESSAGGI
004400000000     D FUN1            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ VIDEO PARAMETR
004500000000     D FUN2            S             40    DIM(31) CTDATA PERRCD(1)             *FUNZ VIDEO STATISTI
004600000000     D* FUNZIONI A VIDEO
004700000000     D FUN             S             40    DIM(31)                              *RIGA DESCR. E ABIL.
004800000000     D* SINGOLE SCHIERE ALL'INTERNO DELLA RIGA FUNZIONI
004900000000     D FFU             S              6    DIM(31)                              *FUNZIONE
005000000000     D FDE             S             19    DIM(31)                              *DESCRIZIONE FUNZION
005100000000     D FAT             S              1    DIM(31)                              *FUNZIONE ATTIVA S/N
005200000000     D* TABELLA DESCRIZIONI A VIDEO
005300000000     D FUV             S             19    DIM(8)                               *FUNZIONI A VIDEO
005400000000     D* TABELLA TASTI FUNZIONALI PREMUTI
005500000000     D KEYS            S              1    DIM(40)
005600000000     D*------------------
005700000000     D* TABELLE CAPICONTO
005800000000     D*------------------
005900000000     D*------------------
006000000000     D* RICHIESTE STATISTICHE
006100000000     D*------------------
006200000000     D RIC             S              2    DIM(15)                              *RICH STATISTICA = '
006300000000     D*------------------
006400000000     D* PER ORDINAMENTO FASCE SELEZIONATE
006500000000     D*------------------
006600950724     D*-------------------
006700960227     D* CAPOCONTO DI RITORNO
006800950724     D*-------------------
006900050117      ** i codici clienti da passare nella DS sono diventati 28 da 30 che erano
007000050117      ** x permettere passaggio di altri parametri poichè la DS era già di 256
007100050117     D*KSC*****        S              7  0 DIM(30)                              *NUMERICO
007200050117     D*KSA*****        S              4    DIM(30)                              *ALFANUMERICO
007300050117     D KSC             S              7  0 DIM(28)                              *NUMERICO
007400050117     D KSA             S              4    DIM(28)                              *ALFANUMERICO
007500020523     D*-------------------
007600020523     D* CODICI QUALITA' CLIENTE
007700020523     D*-------------------
007800020523     D CODQUA          S              1    DIM(20)
007900950724     D*------------------------------------------------------------------------*
008000000000     D* INPUT
008100000000     D*------------------------------------------------------------------------*
008200000000     D*------------------
008300000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
008400000000     D*------------------
008500000000     D DSMSG         E DS
008600000000     D*------------------
008700000000     D* DS PER GESTIONE TASTI FUNZIONALI
008800000000     D*------------------
008900000000     D DSKEYS        E DS
009000000000     D  KEYRIG                 1     40
009100000000     D*------------------
009200000000     D* RIDEFINIZIONE INFDS STATO DEL PROGRAMMA
009300000000     D*------------------
009400000000     D                SDS
009500000000     D* NOME UTENTE
009600000000     D  USER                 254    263
009700000000     D*------------------
009800000000     D* RIDEFINIZIONE INFDS DEL FILE VIDEO
009900000000     D*------------------
010000000000     D DEVDS1          DS
010100000000     D  KEY                  369    369
010200000000     D  CURSOR               370    371
010300000000     D*------------------
010400000000     D* RIDEFINIZIONE SCHIERA -FUD- (FUNZIONI)
010500000000     D*------------------
010600000000     D DSFUN         E DS
010700000000     D  FUNRIG                 1     40
010800000000     D*------------------
010900000000     D* ARCHITETTURA
011000000000     D*------------------
011100000000     D KPJBA         E DS
011200000000     D*------------------
011300010410     D* DS DATI PER STATISTICHE -DI GRUPPO-
011400980129     D*------------------
011500010410     D DS5S          E DS
011600000817     D*------------------
011700000817     D* DS REPERIMENTO DATI UTENTE
011800000817     D*-------------------
011900000817     D BS69DS        E DS                  EXTNAME(TIBS69DS) INZ
012000000817     D ACODS         E DS                  EXTNAME(CNACO00F) INZ
012100000817     D INDDS         E DS                  EXTNAME(CNIND00F) INZ
012200000817     D CLPDS         E DS                  EXTNAME(CNCLP00F) INZ
012300000817     D CLSDS         E DS                  EXTNAME(FNCLS00F) INZ
012400990601     D*------------------
012500990601     D* DS CONTROLLO DATA (8)
012600990601     D*------------------
012700990601     D WLBDA8          DS                  INZ
012800990601     D  G08DAT                 1      8  0
012900990601     D  G08INV                 9     16  0
013000990601     D  G08ERR                17     17
013100990601     D  G08TGI                18     22  0
013200000000     D*------------------
013300000000     D* PARAMETRI CHIAMATA 'TNTA36R'
013400000000     D*------------------
013500000000     D PARAM4          DS                  INZ
013600000000     D  PARCOD                 6     12  0
013700000000     D  PARCTR                13     15
013800000000     D  PARFLG                16     16
013900000000     D  PARID                 17     17  0
014000000000     D  PARCCC                18     21  0
014100000000     D  PARDCV                22     36
014200000000     D*------------------
014300000000     D* DS PROCEDURA STATISTICHE CLIENTI
014400000000     D*------------------
014500010920     D DS60          E DS                  EXTNAME(TISE60DS)
014600020916     D*------------
014700020916     D* DS REPERIMENTO DATI UTENTE
014800020916     D*------------
014900020916     D TIBS34DS      E DS                                                       *Profili utente
015000020916     D DDATIUTE      E DS                                                       *Dati utente
015100020916     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
015200000000     D*------------------
015300000000     D* DS UNIONE CODICI TARIFFA
015400000000     D*------------------
015500000000     D                 DS
015600000000     D  V0CCT1                 1      3
015700000000     D  V0CCT2                 4      6
015800000000     D  V0CCT3                 7      9
015900000000     D  V0CCT4                10     12
016000000000     D  V0CCT5                13     15
016100000000     D  V0CCT6                16     18
016200000000     D  V0CCT7                19     21
016300000000     D  V0CCT8                22     24
016400000000     D  V0CCT9                25     27
016500000000     D  V0CCT0                28     30
016600000000     D  V0CCTR                 1     30
016700000000     D*------------------
016800000000     D* DS UNIONE FASCE DI PESO
016900000000     D*------------------
017000000000     D                 DS
017100000000     D  V0CFP1                 1      2
017200000000     D  V0CFP2                 3      4
017300000000     D  V0CFP3                 5      6
017400000000     D  V0CFP4                 7      8
017500000000     D  V0CFP5                 9     10
017600000000     D  V0CFP6                11     12
017700000000     D  V0CFP7                13     14
017800000000     D  V0CFP8                15     16
017900000000     D  V0CFP9                17     18
018000000000     D  V0CFP0                19     20
018100000000     D  V0CFPA                21     22
018200000000     D  V0CFPE                 1     22
018300000000     D                                     DIM(11) ASCEND                       *FASCE IN ORDINE ASC
018400000000     D*------------------
018500000000     D* DS UNIONE RICHIESTE STATISTICA
018600000000     D*------------------
018700000000     D                 DS
018800000000     D  V0CST1                 1      1
018900000000     D  V0CST2                 2      2
019000000000     D  V0CST3                 3      3
019100000000     D  V0CST4                 4      4
019200000000     D  V0CST5                 5      5
019300050110     D  V0CSTp                 6      6
019400050110     D  V0CSTA                 1      6
019500000000     D*------------------
019600000000     D* DS DATA ULTIMO AGGIORNAMENTO SALDI
019700000000     D*------------------
019800000000     D                 DS
019900990601     D  AGGAA                  1      4  0
020000981117     D  AGGMM                  5      6  0
020100990601     D  AGGAM                  1      6  0
020200981117     D  AGGGG                  7      8  0
020300980408     D  WDAC                   1      8  0
020400000000     D*------------------
020500000000     D* DS DATA ULTIMA CANCELLAZIONE SALDI
020600000000     D*------------------
020700000000     D                 DS
020800990601     D  CANAA                  1      4  0
020900981117     D  CANMM                  5      6  0
021000990601     D  CANAM                  1      6  0
021100981117     D  CANGG                  7      8  0
021200980408     D  WDCC                   1      8  0
021300950724     D*------------------
021400950724     D* DS PER PASSAGGIO CAMPI PACKED
021500950724     D*------------------
021600950724     D                 DS
021700950724     D  DSKSN                  1      4P 0
021800950724     D  DSKSA                  1      4
021900000000     C*------------------------------------------------------------------------*
022000000000     C*?MAIN DEL PGM
022100000000     C*------------------------------------------------------------------------*
022200020523     C*
022300000000     C* CICLO FINO A CHE:
022400000000DO  1C     $FINE         DOUEQ     '1'
022500000000     C                   MOVEL     '0'           $ERR                           *FLAG ERRORE
022600000000     C*
022700000000IF  2C     $GES          IFEQ      'V0'
022800000000     C                   EXSR      GESV0                                        *PARAMETRI GENERALI
022900000000E   2C                   ENDIF
023000000000     C*
023100000000E   1C                   ENDDO
023200000000     C*
023300000000     C* SE PREMUTO F3 LO IMPOSTO ANCHE NELL' F3 DELLA DS ESTERNA
023400000000IF  1C     $F03          IFEQ      '1'
023500000000     C                   MOVEL     '1'           D60F03
023600000000E   1C                   ENDIF
023700000817     C*
023800000817     C* OPERAZIONI FINALI
023900000817     C                   EXSR      FINSR
024000000817     C*
024100950724     C                   MOVEL     DS60          KPJBU
024200000000     C                   SETON                                        LR
024300020523     C*------------------------------------------------------------------------*
024400020523     C*?INZVID - INIZIALIZZAZIONI INIZIALI
024500020523     C*------------------------------------------------------------------------*
024600020523     C     INZVID        BEGSR
024700020523     C*
024800020523     C* QUALITA' CLIENTE
024900160801     C*m                 MOVEL     'A'           V0CCLA
025000160801     C*m                 MOVEL     'B'           V0CCLB
025100160801     C*m                 MOVEL     'C'           V0CCLC
025200160801     C*m                 MOVEL     'T'           V0CCLT
025300160801     C*m                 MOVEL     'D'           V0CCLD
025400020523     C*
025500020523     C* UNIFICANTE
025600020523     C                   MOVEL     *blanks       V0CUNI
025700020523     C*
025800020523     C                   ENDSR
025900000000     C*------------------------------------------------------------------------*
026000000000     C*?CARV0 - CARICA RICHIESTA PARAMETRI GENERALI STATISTICHE
026100000000     C*------------------------------------------------------------------------*
026200000000     C     CARV0         BEGSR
026300000000     C*
026400000000     C                   MOVEL     'V0'          $GES                           *GESTIONE ATTUALE
026500000000     C*
026600000000     C                   MOVEA     FUN1          FUN                            *SCHIERA FUNZ.VIDEO
026700000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
026800000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
026900000000     C*
027000010920     C                   WRITE     SE61DVT                                      *TESTATA
027100010920     C                   WRITE     SE61DVF                                      *FUNZIONI
027200000000     C*
027300000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
027400000000     C*
027500000000     C                   ENDSR
027600000000     C*------------------------------------------------------------------------*
027700000000     C*?GESV0 - GESTIONE RICHIESTA PARAMETRI GENERALI STATISTICHE
027800000000     C*------------------------------------------------------------------------*
027900000000     C     GESV0         BEGSR
028000000000     C*
028100000000IF  1C     $TESVD        IFEQ      '1'                                          *RIEMETTERE TESTATA
028200000000     C                   EXSR      CARV0                                        *CARICA VIDEO
028300000000E   1C                   ENDIF
028400000000     C*
028500000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
028600000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
028700000000E   1C                   ENDIF
028800000000     C*
028900010920     C                   EXFMT     SE61DV0                                      *PARAMETRI GENERALI
029000000000     C*
029100000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTI FUN
029200000000     C*
029300000000IF  1C     $ERR          IFEQ      '0'
029400000000SL  2C                   SELECT
029500000000     C     KEY           WHENEQ    ENTER
029600000000     C                   EXSR      ENTV0                                        ENTER-CONTROLLI
029700000000     C     KEY           WHENEQ    F6
029800000000     C                   EXSR      F06GES                                       F06-LANCIO STATISTIC
029900000000     C     KEY           WHENEQ    F10
030000000000     C                   EXSR      F10GES                                       F10-ALTRI PARAMETRI
030100000000     C                   OTHER
030200000000     C                   EXSR      FUNSTD                                       TASTI STANDARD
030300000000E   2C                   ENDSL
030400000000E   1C                   ENDIF
030500000000     C*
030600000000     C                   ENDSR
030700000000     C*------------------------------------------------------------------------*
030800000000     C*?ENTV0 - GESIONE TASTO ENTER ==> CONTROLLA TUTTI I PARAMETRI
030900000000     C*------------------------------------------------------------------------*
031000000000     C     ENTV0         BEGSR
031100000000     C*
031200000000     C                   MOVEL     '0'           GESPAR                         *GESTIONE DI PARTENZ
031300000000     C*
031400000000     C                   MOVEL     'V0'          $GES                           *PARAMETRI GENERALI
031500000000     C                   EXSR      DECOD                                        *CONTROLLO
031600000000     C*
031700000000IF  1C     $ERR          IFEQ      '0'
031800000000     C     V0CST1        ANDEQ     'S'                                          *STATIST SELEZIONATA
031900000000     C                   MOVEL     'VR'          $GES                           *PARAMETRI REG-REG
032000000000     C                   EXSR      DECOD                                        *CONTROLLO
032100000000     C*
032200000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
032300000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
032400000000E   2C                   ENDIF
032500000000E   1C                   ENDIF
032600050110     C*
032700050110IF  1C     $ERR          IFEQ      '0'
032800050110     C     V0CSTp        ANDEQ     'S'                                          *STATIST SELEZIONATA
032900050110     C                   MOVEL     'PV'          $GES                           *PARAMETRI PESO-VOLUME
033000050110     C                   EXSR      DECOD                                        *CONTROLLO
033100050110     C*
033200050110IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
033300050110     C                   EXSR      GESVX                                        *GESTIONE VIDEO
033400050110E   2C                   ENDIF
033500050110E   1C                   ENDIF
033600000000     C*
033700000000IF  1C     $ERR          IFEQ      '0'
033800000000     C     V0CST2        ANDEQ     'S'                                          *STATIST SELEZIONATA
033900000000     C                   MOVEL     'VS'          $GES                           *PARAMETRI SERV BOLL
034000000000     C                   EXSR      DECOD                                        *CONTROLLO
034100000000     C*
034200000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
034300000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
034400000000E   2C                   ENDIF
034500000000E   1C                   ENDIF
034600000000     C*
034700000000IF  1C     $ERR          IFEQ      '0'
034800000000     C     V0CST3        ANDEQ     'S'                                          *STATIST SELEZIONATA
034900000000     C                   MOVEL     'VA'          $GES                           *PARAMETRI AFFID.TA'
035000000000     C                   EXSR      DECOD                                        *CONTROLLO
035100000000     C*
035200000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
035300000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
035400000000E   2C                   ENDIF
035500000000E   1C                   ENDIF
035600000000     C*
035700000000IF  1C     $ERR          IFEQ      '0'
035800000000     C     V0CST4        ANDEQ     'S'                                          *STATIST SELEZIONATA
035900000000     C                   MOVEL     'VD'          $GES                           *PARAMETRI DELTA CLI
036000000000     C                   EXSR      DECOD                                        *CONTROLLO
036100000000     C*
036200000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
036300000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
036400000000E   2C                   ENDIF
036500000000E   1C                   ENDIF
036600000000     C*
036700000000IF  1C     $ERR          IFEQ      '0'
036800000000     C     V0CST5        ANDEQ     'S'                                          *STATIST SELEZIONATA
036900000000     C                   MOVEL     'VM'          $GES                           *PARAMETRI AMMIN.NE
037000000000     C                   EXSR      DECOD                                        *CONTROLLO
037100000000     C*
037200000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
037300000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
037400000000E   2C                   ENDIF
037500000000E   1C                   ENDIF
037600000000     C*
037700000000IF  1C     $ERR          IFEQ      '0'
037800000000     C     V0CST6        ANDEQ     'S'                                          *STATIST SELEZIONATA
037900000000     C                   MOVEL     'VF'          $GES                           *PARAMETRI DELTA FIL
038000000000     C                   EXSR      DECOD                                        *CONTROLLO
038100000000     C*
038200000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
038300000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
038400000000E   2C                   ENDIF
038500000000E   1C                   ENDIF
038600000000     C*
038700000000IF  1C     $ERR          IFEQ      '0'
038800000000     C     V0CST7        ANDEQ     'S'                                          *STATIST SELEZIONATA
038900000000     C                   MOVEL     'VC'          $GES                           *PARAMETRI DELTA COM
039000000000     C                   EXSR      DECOD                                        *CONTROLLO
039100000000     C*
039200000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
039300000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
039400000000E   2C                   ENDIF
039500000000E   1C                   ENDIF
039600000000     C*
039700000000     C                   MOVEL     'V0'          $GES                           *GESTIONE PARAM GENE
039800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
039900000000     C*
040000000000     C                   ENDSR
040100000000     C*------------------------------------------------------------------------*
040200000000     C*?F06GES -GEST.TASTO FUNZ. F06 ==> LANCIO STATISTICHE
040300000000     C*------------------------------------------------------------------------*
040400000000     C     F06GES        BEGSR
040500000000     C*
040600000000     C                   MOVEL     '0'           GESPAR                         *GESTIONE DI PARTENZ
040700000000     C*
040800000000     C                   MOVEL     'V0'          $GES                           *PARAMETRI GENERALI
040900000000     C                   EXSR      DECOD                                        *CONTROLLO
041000000000     C*
041100000000IF  1C     $ERR          IFEQ      '0'
041200000000     C     V0CST1        ANDEQ     'S'                                          *STATIST SELEZIONATA
041300000000     C                   MOVEL     'VR'          $GES                           *PARAMETRI REG-REG
041400000000     C                   EXSR      DECOD                                        *CONTROLLO
041500000000     C*
041600000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
041700000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
041800000000E   2C                   ENDIF
041900000000E   1C                   ENDIF
042000050110     C*
042100050110IF  1C     $ERR          IFEQ      '0'
042200050110     C     V0CSTp        ANDEQ     'S'                                          *STATIST SELEZIONATA
042300050110     C                   MOVEL     'PV'          $GES                           *PARAMETRI PESO-VOLUME
042400050110     C                   EXSR      DECOD                                        *CONTROLLO
042500050110     C*
042600050110IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
042700050110     C                   EXSR      GESVX                                        *GESTIONE VIDEO
042800050110E   2C                   ENDIF
042900050110E   1C                   ENDIF
043000000000     C*
043100000000IF  1C     $ERR          IFEQ      '0'
043200000000     C     V0CST2        ANDEQ     'S'                                          *STATIST SELEZIONATA
043300000000     C                   MOVEL     'VS'          $GES                           *PARAMETRI SERV BOLL
043400000000     C                   EXSR      DECOD                                        *CONTROLLO
043500000000     C*
043600000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
043700000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
043800000000E   2C                   ENDIF
043900000000E   1C                   ENDIF
044000000000     C*
044100000000IF  1C     $ERR          IFEQ      '0'
044200000000     C     V0CST3        ANDEQ     'S'                                          *STATIST SELEZIONATA
044300000000     C                   MOVEL     'VA'          $GES                           *PARAMETRI AFFID.TA'
044400000000     C                   EXSR      DECOD                                        *CONTROLLO
044500000000     C*
044600000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
044700000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
044800000000E   2C                   ENDIF
044900000000E   1C                   ENDIF
045000000000     C*
045100000000IF  1C     $ERR          IFEQ      '0'
045200000000     C     V0CST4        ANDEQ     'S'                                          *STATIST SELEZIONATA
045300000000     C                   MOVEL     'VD'          $GES                           *PARAMETRI DELTA CLI
045400000000     C                   EXSR      DECOD                                        *CONTROLLO
045500000000     C*
045600000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
045700000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
045800000000E   2C                   ENDIF
045900000000E   1C                   ENDIF
046000000000     C*
046100000000IF  1C     $ERR          IFEQ      '0'
046200000000     C     V0CST5        ANDEQ     'S'                                          *STATIST SELEZIONATA
046300000000     C                   MOVEL     'VM'          $GES                           *PARAMETRI AMMIN.NE
046400000000     C                   EXSR      DECOD                                        *CONTROLLO
046500000000     C*
046600000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
046700000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
046800000000E   2C                   ENDIF
046900000000E   1C                   ENDIF
047000000000     C*
047100000000IF  1C     $ERR          IFEQ      '0'
047200000000     C     V0CST6        ANDEQ     'S'                                          *STATIST SELEZIONATA
047300000000     C                   MOVEL     'VF'          $GES                           *PARAMETRI DELTA FIL
047400000000     C                   EXSR      DECOD                                        *CONTROLLO
047500000000     C*
047600000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
047700000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
047800000000E   2C                   ENDIF
047900000000E   1C                   ENDIF
048000000000     C*
048100000000IF  1C     $ERR          IFEQ      '0'
048200000000     C     V0CST7        ANDEQ     'S'                                          *STATIST SELEZIONATA
048300000000     C                   MOVEL     'VC'          $GES                           *PARAMETRI DELTA COM
048400000000     C                   EXSR      DECOD                                        *CONTROLLO
048500000000     C*
048600000000IF  2C     $ERR          IFEQ      '1'                                          *SE ERRORE
048700000000     C                   EXSR      GESVX                                        *GESTIONE VIDEO
048800000000E   2C                   ENDIF
048900000000E   1C                   ENDIF
049000000000     C*
049100000000     C                   MOVEL     'V0'          $GES                           *GESTIONE PARAM GENE
049200000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
049300000000     C*
049400000000     C* SE NON CI SONO ERRORI E NON HO PREMUTO F3 PER USCIRE
049500000000IF  1C     $ERR          IFEQ      '0'
049600000000     C     $FINE         ANDEQ     '0'
049700000000     C                   EXSR      LANPGM
049800000000     C                   MOVEL     '1'           $FINE                          *FINE PGM MA NON PRO
049900000000E   1C                   ENDIF
050000000000     C*
050100000000     C                   ENDSR
050200000000     C*------------------------------------------------------------------------*
050300000000     C*?LANPGM - LANCIO PROGRAMMA BATCH DI STAMPA STATISTICHE
050400000000     C*------------------------------------------------------------------------*
050500000000     C     LANPGM        BEGSR
050600000000     C*
050700000000     C* MUOVO I PARAMETRI SELEZIONATI IN QUELLI DELLA DS
050800000000     C*
050900000000     C*--------------------
051000000000     C* PARAMETRI INFORMATIVI
051100000000     C*--------------------
051200000000     C                   MOVEL     '0'           D60F03
051300000000     C                   MOVEL     '0'           D60ERR
051400000000     C                   MOVEL     *BLANKS       D60MSG
051500000000     C*--------------------
051600000000     C* PARAMETRI GENERALI
051700000000     C*--------------------
051800000000     C                   Z-ADD     V0CDA2        D60DA2                         *AA/MM FINALE
051900000000     C                   Z-ADD     V0CDAT        D60DAT                         *AA/MM INIZIALE
052000000000     C                   MOVEL     V0CSE1        D60SE1                         *TIPO SERVIZIO
052100000000     C                   MOVEL     V0CSE2        D60SE2
052200000000     C                   MOVEL     V0CSE3        D60SE3
052300000000     C                   MOVEL     V0CSE4        D60SE4
052400000000     C                   MOVEL     V0CSE5        D60SE5
052500000000     C                   MOVEL     V0CPO1        D60PO1                         *TIPO PORTO
052600000000     C                   MOVEL     V0CPO2        D60PO2
052700000000     C                   MOVEL     V0CPR1        D60PR1                         *TIPO PRODOTTO
052800000000     C                   MOVEL     V0CPR2        D60PR2
052900000000     C                   MOVEL     V0CPR3        D60PR3
053000000000     C                   MOVEL     V0CPR4        D60PR4
053100000000     C                   MOVEL     V0CPR5        D60PR5
053200000000     C                   MOVEL     V0CCT1        D60CT1                         *CODICE TARIFFA
053300000000     C                   MOVEL     V0CCT2        D60CT2
053400000000     C                   MOVEL     V0CCT3        D60CT3
053500000000     C                   MOVEL     V0CCT4        D60CT4
053600000000     C                   MOVEL     V0CCT5        D60CT5
053700000000     C                   MOVEL     V0CCT6        D60CT6
053800000000     C                   MOVEL     V0CCT7        D60CT7
053900000000     C                   MOVEL     V0CCT8        D60CT8
054000000000     C                   MOVEL     V0CCT9        D60CT9
054100000000     C                   MOVEL     V0CCT0        D60CT0
054200000000     C*
054300000000     C* ORDINAMENTO FASCE DI PESO RICHIESTE A VIDEO
054400000000     C                   SORTA     V0CFPE
054500000000     C                   MOVEL     V0CFP1        D60FP1                         *FASCIA DI PESO
054600000000     C                   MOVEL     V0CFP2        D60FP2
054700000000     C                   MOVEL     V0CFP3        D60FP3
054800000000     C                   MOVEL     V0CFP4        D60FP4
054900000000     C                   MOVEL     V0CFP5        D60FP5
055000000000     C                   MOVEL     V0CFP6        D60FP6
055100000000     C                   MOVEL     V0CFP7        D60FP7
055200000000     C                   MOVEL     V0CFP8        D60FP8
055300000000     C                   MOVEL     V0CFP9        D60FP9
055400000000     C                   MOVEL     V0CFP0        D60FP0
055500000000     C                   MOVEL     V0CFPA        D60FPA
055600000000     C                   MOVEL     V0CST1        D60ST1                         *REGIONE-REGIONE
055700000000     C                   MOVEL     V0CST2        D60ST2                         *SERVIZIO BOLLE
055800000000     C                   MOVEL     V0CST3        D60ST3                         *AFFIDABILITA'
055900000000     C                   MOVEL     V0CST4        D60ST4                         *DELTA CLIENTE
056000000000     C                   MOVEL     V0CST5        D60ST5                         *AMMINISTRAZIONE
056100000000     C                   MOVEL     V0CST6        D60ST6                         *DELTA
056200000000     C                   MOVEL     V0CST7        D60ST7                         *DELTA COMMERCIALE
056300050117     C                   movel     V0CSTp        D60STp                         *PESI-VOLUMI
056400020523     C*
056500020523     C* QUALITA' CLIENTE
056600020523     C                   MOVEL     V0CCLA        D60CLA
056700020523     C                   MOVEL     V0CCLB        D60CLB
056800020523     C                   MOVEL     V0CCLC        D60CLC
056900020523     C                   MOVEL     V0CCLT        D60CLT
057000020523     C                   MOVEL     V0CCLD        D60CLD
057100000000     C*--------------------
057200000000     C* PARAMETRI REGIONE-REGIONE
057300000000     C*--------------------
057400000000     C                   MOVEL     V1CRIF        D60RIF                         *VALORE RIFERIMENTO
057500050119     C*--------------------
057600050119     C* PARAMETRI PESI-VOLUMI
057700050119     C*--------------------
057800050119     C                   MOVEL     VPVDET        D60DPV                         *VALORE RIFERIMENTO
057900971128     C*---                 MOVELV1CREG    D60REG           *REGIONE
058000000000     C***
058100000000     C* L A N C I O : PROGRAMMA DI STAMPA O DI VISUALIZZAZIONE
058200000000     C***
058300000000     C                   MOVEL     DS60          KPJBU
058400000000     C*
058500000000IF  1C     D60OP0        IFEQ      'STA'                                        *S T A M P A
058600000000     C     D60OP1        OREQ      'F21'
058700010920     C                   MOVEL     'SE62'        KCOAZ                          *PGM DI STAMPA
058800020606     C                   CALL      'BCH10'                                      *STAMPA STATISTICHE/
058900020610 MOM C***                CALL      'TISE62C'                                    *STAMPA STATISTICHE/
059000000000     C                   PARM                    KPJBA
059100950724     C                   MOVEL(P)  MSG(25)       D60MSG                         *STAMPA LANCIATA ...
059200000000X   1C                   ELSE                                                   *V I S U A L I Z Z A
059300010920     C                   CALL      'TISE65R'                                    *STAMPA STATISTICHE/
059400000000     C                   PARM                    KPJBA
059500000000E   1C                   ENDIF
059600950726     C                   MOVEL     KPJBU         DS60
059700000000     C*
059800000000     C                   ENDSR
059900000000     C*------------------------------------------------------------------------*
060000000000     C*?F10GES -GEST.TASTO FUNZ. F10 ==> ALTRI PARAMETRI
060100000000     C*------------------------------------------------------------------------*
060200000000     C     F10GES        BEGSR
060300000000     C*
060400000000     C                   MOVEL     '1'           GESPAR                         *GESTIONE DI PARTENZ
060500000000     C*
060600000000     C                   MOVEL     'V0'          $GES                           *PARAMETRI GENERALI
060700000000     C                   EXSR      DECOD                                        *CONTROLLO
060800000000     C*
060900000000IF  1C     $ERR          IFEQ      '0'
061000000000     C                   EXSR      CARRIC                                       CARICA GESTIONI SELE
061100000000     C*
061200000000     C                   Z-ADD     2             IN
061300000000     C                   MOVEL     RIC(IN)       $GES                           *1^ GESTIONE <> DA V
061400000000DO  2C     $FINE         DOWEQ     '0'
061500000000     C     $GES          ANDNE     'V0'
061600000000     C                   EXSR      GESVX
061700000000E   2C                   ENDDO
061800000000     C*
061900000000     C                   MOVEL     'V0'          $GES                           *RITORNO PARAM GENER
062000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
062100000000E   1C                   ENDIF
062200000000     C*
062300000000     C                   ENDSR
062400000000     C*------------------------------------------------------------------------*
062500000000     C*?GESVX - GESTIONE VIDEO SINGOLE STATISTICHE
062600000000     C*------------------------------------------------------------------------*
062700000000     C     GESVX         BEGSR
062800000000     C*
062900000000     C                   MOVEL     '1'           $ERR2                          *PER ENTRARE DA F10
063000000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
063100000000     C*
063200000000     C* FINO A QUANDO:
063300000000     C*              - CI SONO DEGLI ERRORI
063400000000     C*              - NON VOGLIO USCIRE
063500000000DO  1C     $FINE         DOWEQ     '0'
063600000000     C     $ERR2         ANDEQ     '1'
063700000000     C*
063800000000     C* EMISSIONE VIDEO PARAMETRI IN GESTIONE
063900000000SL  2C                   SELECT
064000000000     C     $GES          WHENEQ    'VR'
064100000000     C                   EXSR      OUTVR                                        *PARAMETRI REGIONE
064200050110     C*
064300050110     C     $GES          WHENEQ    'PV'
064400050110     C                   EXSR      OUTPV                                        *PARAMETRI PESI-VOLUMI
064500000000     C*
064600000000     C     $GES          WHENEQ    'VS'
064700000000     C                   EXSR      OUTVS                                        *PARAMETRI SERV BOLL
064800000000     C*
064900000000     C     $GES          WHENEQ    'VA'
065000000000     C                   EXSR      OUTVA                                        *PARAMETRI AFFID.TA'
065100000000     C*
065200000000     C     $GES          WHENEQ    'VD'
065300000000     C                   EXSR      OUTVD                                        *PARAMETRI DELTA CLI
065400000000     C*
065500000000     C     $GES          WHENEQ    'VM'
065600000000     C                   EXSR      OUTVM                                        *PARAMETRI AMM.ZIONE
065700000000     C*
065800000000     C     $GES          WHENEQ    'VF'
065900000000     C                   EXSR      OUTVF                                        *PARAMETRI DELTA FIL
066000000000     C*
066100000000     C     $GES          WHENEQ    'VC'
066200000000     C                   EXSR      OUTVC                                        *PARAMETRI DELTA COM
066300000000E   2C                   ENDSL
066400000000     C*
066500000000     C                   EXSR      CONFUN                                       *CONTROLLA TASTO
066600000000     C*
066700000000SL  2C                   SELECT
066800000000     C     KEY           WHENEQ    ENTER
066900000000     C                   EXSR      ENTVX                                        *ENTER-VIDEO DOPO
067000000000     C     KEY           WHENEQ    F12
067100000000     C                   EXSR      F12VX                                        F12-VIDEO INDIETRO
067200000000     C                   OTHER
067300000000     C                   EXSR      FUNSTD                                       *FUNZIONI STANDARD
067400000000E   2C                   ENDSL
067500000000     C*
067600000000E   1C                   ENDDO
067700000000     C*
067800000000     C                   ENDSR
067900000000     C*------------------------------------------------------------------------*
068000000000     C*?ENTVX - GESTIONE TASTO ENTER SINGOLE STATISTICHE ==> PORTA AL VIDEO DOPO
068100000000     C*------------------------------------------------------------------------*
068200000000     C     ENTVX         BEGSR
068300000000     C*
068400000000     C* GESPAR='0' --> VENGO DA F06/ENTER: NON VEDO TUTTA LA SEQUENZA DEI VIDEO
068500000000     C*                PARAMETRI, MA SOLO QUELLI IN ERRORE (ENTER INIBITO)
068600000000     C* GESPAR='1' --> VENGO DA F10-ALTRI PARAMETRI: VEDO TUTTI I VIDEO-PARAME-
068700000000     C*                TRI (ENTER-VIDEO DOPO)
068800000000     C*
068900000000     C                   EXSR      DECOD                                        *CONTROLLO VIDEO
069000000000     C*
069100000000     C* SE NO ERRORI E VENGO DA F10-ALTRI PARAMETRI, VADO AL VIDEO SUCESSIVO
069200000000IF  1C     $ERR          IFEQ      '0'
069300000000IF  2C     GESPAR        IFEQ      '1'                                          *F10
069400000000     C                   ADD       1             IN                             *VIDEO SUCESSIVO
069500000000     C                   MOVEL     RIC(IN)       $GES
069600000000X   2C                   ELSE
069700000000     C                   MOVEL     'V0'          $GES                           *X TORNARE AI PAR GE
069800000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
069900000000E   2C                   ENDIF
070000000000E   1C                   ENDIF
070100000000     C*
070200000000     C                   ENDSR
070300000000     C*------------------------------------------------------------------------*
070400000000     C*?F12VX - GESTIONE F12 SINGOLE STATISTICHE CHE PORTA AL VIDEO PRECEDENTE
070500000000     C*------------------------------------------------------------------------*
070600000000     C     F12VX         BEGSR
070700000000     C*
070800000000     C* GESPAR='0' --> VENGO DA F06/ENTER: NON VEDO TUTTA LA SEQUENZA DEI VIDEO
070900000000     C*                PARAMETRI, MA SOLO QUELLI IN ERRORE (F12-PARAMETRI GENER)
071000000000     C* GESPAR='1' --> VENGO DA F10-ALTRI PARAMETRI: VEDO TUTTI I VIDEO-PARAME-
071100000000     C*                TRI (F12-VIDEO PRIMA)
071200000000     C*
071300000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTERE TESTATA
071400000000     C                   MOVEL     '0'           $ERR2                          *PER USCIRE
071500000000     C                   Z-ADD     *ZEROS        MSGULT                         PULISCE ERROR MESSAG
071600000000     C*
071700000000IF  1C     GESPAR        IFEQ      '0'                                          *DA F06 O ENTER
071800000000     C                   MOVEL     'V0'          $GES                           *PARAMETRI GENERALI
071900000000X   1C                   ELSE                                                   *DA F10
072000000000     C                   SUB       1             IN                             *VIDEO PRECEDENTE
072100000000     C                   MOVEL     RIC(IN)       $GES                           *GESTIONE
072200000000E   1C                   ENDIF
072300000000     C*
072400000000     C                   ENDSR
072500000000     C*------------------------------------------------------------------------*
072600000000     C*?DECOD - CONTROLLA I PARAMETRI DELLA STATISTICA IN GESTIONE
072700000000     C*------------------------------------------------------------------------*
072800000000     C     DECOD         BEGSR
072900000000     C*
073000000000SL  1C                   SELECT
073100000000     C     $GES          WHENEQ    'V0'
073200000000     C                   EXSR      DECOD0                                       *PARAMETRI GENERALI
073300000000     C*
073400000000     C     $GES          WHENEQ    'VR'
073500000000     C                   EXSR      DECOD1                                       *PARAMETRI REGIONE
073600050110     C*
073700050110     C     $GES          WHENEQ    'PV'
073800050110     C                   EXSR      DECODP                                       *PARAMETRI PESO/VOLUME
073900000000     C*
074000000000     C     $GES          WHENEQ    'VS'
074100000000     C                   EXSR      DECOD2                                       *PARAMETRI SERV BOLL
074200000000     C*
074300000000     C     $GES          WHENEQ    'VA'
074400000000     C                   EXSR      DECOD3                                       *PARAMETRI AFFID.TA'
074500000000     C*
074600000000     C     $GES          WHENEQ    'VD'
074700000000     C                   EXSR      DECOD4                                       *PARAMETRI DELTA CLI
074800000000     C*
074900000000     C     $GES          WHENEQ    'VM'
075000000000     C                   EXSR      DECOD5                                       *PARAMETRI AMM.ZIONE
075100000000     C*
075200000000     C     $GES          WHENEQ    'VF'
075300000000     C                   EXSR      DECOD6                                       *PARAMETRI DELTA FIL
075400000000     C*
075500000000     C     $GES          WHENEQ    'VC'
075600000000     C                   EXSR      DECOD7                                       *PARAMETRI DELTA COM
075700000000E   1C                   ENDSL
075800000000     C*
075900000000     C                   ENDSR
076000000000     C*------------------------------------------------------------------------*
076100000000     C*?DECOD0 - CONTROLLA I PARAMETRI GENERALI
076200000000     C*------------------------------------------------------------------------*
076300000000     C     DECOD0        BEGSR
076400000000     C*
076500000000     C* REIMPOSTO INDICATORI DI ERRORE SUI CAMPI
076600000000     C                   SETOFF                                       606162
076700000000     C                   SETOFF                                       636465
076800000000     C                   SETOFF                                       666768
076900000000     C                   SETOFF                                       697071
077000000000     C                   SETOFF                                       727374
077100000000     C                   SETOFF                                       757677
077200000000     C                   SETOFF                                       787980
077300000000     C                   SETOFF                                       818283
077400000000     C                   SETOFF                                       848586
077500000000     C                   SETOFF                                       878889
077600000000     C                   SETOFF                                       909192
077700000000     C                   SETOFF                                       939495
077800000000     C                   SETOFF                                       969798
077900000000     C                   SETOFF                                       58
078000020523     C                   SETOFF                                       323334
078100020523     C                   SETOFF                                       3536
078200000000     C*
078300000000     C* E VARIABILI DI LAVORO
078400000000     C                   Z-ADD     *ZEROS        RISUL             4 0
078500020523     C*--------------------
078600020524     C* CONTROLLO DEL TIPO DI LANCIO IN FUNZIONE DELLE RICHIESTE A VIDEO
078700020523     C*--------------------
078800020523     C     $ERR          IFEQ      '0'
078900020524     C     D60OP0        IFEQ      'STA'
079000020523     C     V0CUNI        IFEQ      *blanks
079100020523     C                   MOVEL     '4'           D60SCE
079200020523     C                   ELSE
079300020523     C                   MOVEL     '5'           D60SCE
079400020523     C                   ENDIF
079500020524     C                   ENDIF
079600020523     C                   ENDIF
079700020523     C*--------------------
079800020523     C* QUALITA' CLIENTE
079900020523     C*--------------------
080000020523IF  1C     $ERR          IFEQ      '0'
080100020523     C     V0CCLA        LOOKUP    CODQUA                                 55
080200020523     C                   IF        *IN55 = *OFF
080300020523     C                   MOVEL     '1'           $ERR
080400020523     C                   MOVEL     MSG(26)       $MSG
080500020523     C                   EXSR      CARMSG
080600020523     C                   SETON                                        32
080700020523     C                   ENDIF
080800020523E   1C                   ENDIF
080900020523     C*
081000020523IF  1C     $ERR          IFEQ      '0'
081100020523     C     V0CCLB        LOOKUP    CODQUA                                 55
081200020523     C                   IF        *IN55 = *OFF
081300020523     C                   MOVEL     '1'           $ERR
081400020523     C                   MOVEL     MSG(26)       $MSG
081500020523     C                   EXSR      CARMSG
081600020523     C                   SETON                                        33
081700020523     C                   ENDIF
081800020523E   1C                   ENDIF
081900020523     C*
082000020523IF  1C     $ERR          IFEQ      '0'
082100020523     C     V0CCLC        LOOKUP    CODQUA                                 55
082200020523     C                   IF        *IN55 = *OFF
082300020523     C                   MOVEL     '1'           $ERR
082400020523     C                   MOVEL     MSG(26)       $MSG
082500020523     C                   EXSR      CARMSG
082600020523     C                   SETON                                        34
082700020523     C                   ENDIF
082800020523E   1C                   ENDIF
082900020523     C*
083000020523IF  1C     $ERR          IFEQ      '0'
083100020523     C     V0CCLT        LOOKUP    CODQUA                                 55
083200020523     C                   IF        *IN55 = *OFF
083300020523     C                   MOVEL     '1'           $ERR
083400020523     C                   MOVEL     MSG(26)       $MSG
083500020523     C                   EXSR      CARMSG
083600020523     C                   SETON                                        35
083700020523     C                   ENDIF
083800020523E   1C                   ENDIF
083900020523     C*
084000020523IF  1C     $ERR          IFEQ      '0'
084100020523     C     V0CCLD        LOOKUP    CODQUA                                 55
084200020523     C                   IF        *IN55 = *OFF
084300020523     C                   MOVEL     '1'           $ERR
084400020523     C                   MOVEL     MSG(26)       $MSG
084500020523     C                   EXSR      CARMSG
084600020523     C                   SETON                                        36
084700020523     C                   ENDIF
084800020523E   1C                   ENDIF
084900020523     C*--------------------
085000000000     C* FLAG SALDI AGGIORNATI
085100000000     C*--------------------
085200000000     C* NON POSSO LANCIARE LE STATISTICHE PER TUTTI I CLIENTI CHE RIPORTANO IN
085300000000     C* STAMPA I RICAVI SE I SALDI NON SONO AGGIORNATI CON RETTIFICHE E NOTE !!!
085400000000IF  1C     D60OP1        IFEQ      'DEL'                                        *DELTA
085500000000     C     D60OP1        OREQ      'AMM'                                        *AMMINISTRAZIONE
085600000000     C     D60OP1        OREQ      'SER'                                        *SERVIZIO BOLLE
085700020523     C     D60OP1        OREQ      'STT'                                        *TUTTE LE STATISTICHE
085800980408IF  2C     WFSC          IFEQ      'N'                                          *SALDI NON AGGIORNAT
085900000000     C                   MOVEL     '1'           $ERR
086000000000     C                   MOVEL     MSG(21)       $MSG
086100000000     C                   EXSR      CARMSG
086200000000E   2C                   ENDIF
086300000000E   1C                   ENDIF
086400000000     C*--------------------
086500000000     C* ANNO/MESE INIZIALE
086600000000     C*--------------------
086700000000     C* OBBLIGATORIO
086800000000IF  1C     $ERR          IFEQ      '0'
086900000000IF  1C     V0CDAT        IFEQ      *ZEROS
087000000000     C                   MOVEL     '1'           $ERR
087100000000     C                   MOVEL     MSG(7)        $MSG
087200000000     C                   EXSR      CARMSG
087300000000     C                   SETON                                        61
087400000000E   1C                   ENDIF
087500000000E   1C                   ENDIF
087600000000     C* CORRETTO
087700000000IF  1C     $ERR          IFEQ      '0'
087800990601     C                   MOVEL     V0CDAT        G08INV
087900990601     C                   MOVE      01            G08INV
088000990601     C                   Z-ADD     *ZEROS        G08DAT
088100990601     C                   MOVEL     '3'           G08ERR
088200990601     C                   CALL      'XSRDA8'
088300990601     C                   PARM                    WLBDA8
088400000000     C*
088500990601IF  2C     G08ERR        IFEQ      '1'
088600000000     C                   MOVEL     '1'           $ERR
088700000000     C                   MOVEL     MSG(8)        $MSG
088800000000     C                   EXSR      CARMSG
088900000000     C                   SETON                                        61
089000000000E   2C                   ENDIF
089100000000E   1C                   ENDIF
089200000000     C*--------------------
089300000000     C* ANNO/MESE FINALE
089400000000     C*--------------------
089500000000     C* OBBLIGATORIO
089600000000IF  1C     V0CDA2        IFEQ      *ZEROS
089700000000     C                   MOVEL     '1'           $ERR
089800000000     C                   MOVEL     MSG(7)        $MSG
089900000000     C                   EXSR      CARMSG
090000000000     C                   SETON                                        60
090100000000E   1C                   ENDIF
090200000000     C* CORRETTO
090300000000IF  1C     $ERR          IFEQ      '0'
090400990601     C                   MOVEL     V0CDA2        G08INV
090500990601     C                   MOVE      01            G08INV
090600990601     C                   Z-ADD     *ZEROS        G08DAT
090700990601     C                   MOVEL     '3'           G08ERR
090800990601     C                   CALL      'XSRDA8'
090900990601     C                   PARM                    WLBDA8
091000000000     C*
091100990601IF  2C     G08ERR        IFEQ      '1'
091200000000     C                   MOVEL     '1'           $ERR
091300000000     C                   MOVEL     MSG(8)        $MSG
091400000000     C                   EXSR      CARMSG
091500000000     C                   SETON                                        60
091600000000E   2C                   ENDIF
091700000000E   1C                   ENDIF
091800000000     C*
091900000000     C* ANNO/MESE FINALE SUPERIORE/UGUALE A QUELLO INIZIALE
092000000000IF  2C     $ERR          IFEQ      '0'
092100000000IF  3C     V0CDA2        IFLT      V0CDAT
092200000000     C                   MOVEL     '1'           $ERR
092300000000     C                   MOVEL     MSG(10)       $MSG
092400000000     C                   EXSR      CARMSG
092500000000     C                   SETON                                        60
092600000000E   3C                   ENDIF
092700000000E   2C                   ENDIF
092800000000     C*
092900000000     C* ANNO/MESE INIZIALE SUPERIORE/UGUALE A QUELLO CANCELLAZIONE + 1
093000000000IF  1C     $ERR          IFEQ      '0'
093100990601IF  2C     V0CDAT        IFLT      CANAM
093200000000     C                   MOVEL     '1'           $ERR
093300000000     C                   MOVEL     MSG(20)       $MSG
093400000000     C                   EXSR      CARMSG
093500000000     C                   SETON                                        61
093600000000E   2C                   ENDIF
093700000000E   1C                   ENDIF
093800000000     C*
093900000000     C* ANNO/MESE FINALE INFERIORE/UGUALE A QUELLO AGGIORNAMENTO - 1
094000000000IF  1C     $ERR          IFEQ      '0'
094100990601IF  2C     V0CDA2        IFGT      AGGAM
094200000000     C                   MOVEL     '1'           $ERR
094300000000     C                   MOVEL     MSG(9)        $MSG
094400000000     C                   EXSR      CARMSG
094500000000     C                   SETON                                        60
094600000000E   2C                   ENDIF
094700000000E   1C                   ENDIF
094800000000     C*
094900000000     C* RICHIEDERE NON PIU' DI DODICI MESI
095000000000IF  1C     $ERR          IFEQ      '0'
095100000000     C     V0CDA2        SUB       V0CDAT        RISUL
095200000000IF  2C     RISUL         IFGE      100
095300000000     C                   MOVEL     '1'           $ERR
095400000000     C                   MOVEL     MSG(11)       $MSG
095500000000     C                   EXSR      CARMSG
095600000000     C                   SETON                                        60
095700000000E   2C                   ENDIF
095800000000E   1C                   ENDIF
095900020325     C*
096000020325     C* ANNO INIZIALE DIVERSO DA ANNO FINALE (NN E' + POSSIBILE LANCIARE LA STATISTICA A CAVALLO D'ANNO)
096100020325IF  1C     $ERR          IFEQ      '0'
096200020325     C                   MOVEL     V0CDAT        ANNODA            4 0
096300020325     C                   MOVEL     V0CDA2        ANNOA             4 0
096400020325IF  2C     ANNODA        IFNE      ANNOA
096500020325     C                   MOVEL     '1'           $ERR
096600020325     C                   MOVEL     MSG(24)       $MSG
096700020325     C                   EXSR      CARMSG
096800020325     C                   SETON                                        6061
096900020325E   2C                   ENDIF
097000020325E   1C                   ENDIF
097100000000     C*--------------------
097200000000     C* TARIFFE CLIENTI
097300000000     C*--------------------
097400000000IF  1C     $ERR          IFEQ      '0'
097500950724     C*
097600000817     C* PER CLIENTI DIVERSI, IMPOSSIBILE SELEZIONARE CODICI TARIFFA
097700000000IF  2C     D60OP0        IFEQ      'STA'
097800000817     C     D60OP0        OREQ      'VIS'
097900000817     C     D60SCE        ANDNE     '1'
098000000000IF  3C     V0CCTR        IFNE      *BLANKS
098100000000     C                   MOVEL     '1'           $ERR
098200000000     C                   MOVEL     MSG(23)       $MSG
098300000000     C                   EXSR      CARMSG
098400000000     C                   SETON                                        626364
098500000000     C                   SETON                                        656667
098600000000     C                   SETON                                        686970
098700000000     C                   SETON                                        71
098800000000E   3C                   ENDIF
098900000000     C*
099000000000     C* PER UN SOLO CLIENTE
099100000000X   2C                   ELSE
099200000000     C                   MOVEL     V0CCT1        WCTRA             3            *TARIFFA LAVORO ALFA
099300000000     C                   EXSR      CHKCTR
099400000000     C                   MOVEL     WCTRA         V0CCT1
099500000000IF  3C     $ERR          IFEQ      '1'
099600000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
099700000000     C                   MOVEL     MSG(12)       $MSG
099800000000     C                   EXSR      CARMSG
099900000000     C                   SETON                                        62
100000000000E   3C                   ENDIF
100100000000     C*
100200000000IF  3C     $ERR          IFEQ      '0'
100300000000     C                   MOVEL     V0CCT2        WCTRA             3            *TARIFFA LAVORO ALFA
100400000000     C                   EXSR      CHKCTR
100500000000     C                   MOVEL     WCTRA         V0CCT2
100600000000IF  4C     $ERR          IFEQ      '1'
100700000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
100800000000     C                   MOVEL     MSG(12)       $MSG
100900000000     C                   EXSR      CARMSG
101000000000     C                   SETON                                        63
101100000000E   4C                   ENDIF
101200000000E   3C                   ENDIF
101300000000     C*
101400000000IF  3C     $ERR          IFEQ      '0'
101500000000     C                   MOVEL     V0CCT3        WCTRA             3            *TARIFFA LAVORO ALFA
101600000000     C                   EXSR      CHKCTR
101700000000     C                   MOVEL     WCTRA         V0CCT3
101800000000IF  4C     $ERR          IFEQ      '1'
101900000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
102000000000     C                   MOVEL     MSG(12)       $MSG
102100000000     C                   EXSR      CARMSG
102200000000     C                   SETON                                        64
102300000000E   4C                   ENDIF
102400000000E   3C                   ENDIF
102500000000     C*
102600000000IF  3C     $ERR          IFEQ      '0'
102700000000     C                   MOVEL     V0CCT4        WCTRA             3            *TARIFFA LAVORO ALFA
102800000000     C                   EXSR      CHKCTR
102900000000     C                   MOVEL     WCTRA         V0CCT4
103000000000IF  4C     $ERR          IFEQ      '1'
103100000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
103200000000     C                   MOVEL     MSG(12)       $MSG
103300000000     C                   EXSR      CARMSG
103400000000     C                   SETON                                        65
103500000000E   3C                   ENDIF
103600000000E   4C                   ENDIF
103700000000     C*
103800000000IF  3C     $ERR          IFEQ      '0'
103900000000     C                   MOVEL     V0CCT5        WCTRA             3            *TARIFFA LAVORO ALFA
104000000000     C                   EXSR      CHKCTR
104100000000     C                   MOVEL     WCTRA         V0CCT5
104200000000IF  4C     $ERR          IFEQ      '1'
104300000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
104400000000     C                   MOVEL     MSG(12)       $MSG
104500000000     C                   EXSR      CARMSG
104600000000     C                   SETON                                        66
104700000000E   4C                   ENDIF
104800000000E   3C                   ENDIF
104900000000     C*
105000000000IF  3C     $ERR          IFEQ      '0'
105100000000     C                   MOVEL     V0CCT6        WCTRA             3            *TARIFFA LAVORO ALFA
105200000000     C                   EXSR      CHKCTR
105300000000     C                   MOVEL     WCTRA         V0CCT6
105400000000IF  3C     $ERR          IFEQ      '1'
105500000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
105600000000     C                   MOVEL     MSG(12)       $MSG
105700000000     C                   EXSR      CARMSG
105800000000     C                   SETON                                        67
105900000000E   4C                   ENDIF
106000000000E   3C                   ENDIF
106100000000     C*
106200000000IF  3C     $ERR          IFEQ      '0'
106300000000     C                   MOVEL     V0CCT7        WCTRA             3            *TARIFFA LAVORO ALFA
106400000000     C                   EXSR      CHKCTR
106500000000     C                   MOVEL     WCTRA         V0CCT7
106600000000IF  4C     $ERR          IFEQ      '1'
106700000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
106800000000     C                   MOVEL     MSG(12)       $MSG
106900000000     C                   EXSR      CARMSG
107000000000     C                   SETON                                        68
107100000000E   4C                   ENDIF
107200000000E   3C                   ENDIF
107300000000     C*
107400000000IF  3C     $ERR          IFEQ      '0'
107500000000     C                   MOVEL     V0CCT8        WCTRA             3            *TARIFFA LAVORO ALFA
107600000000     C                   EXSR      CHKCTR
107700000000     C                   MOVEL     WCTRA         V0CCT8
107800000000IF  4C     $ERR          IFEQ      '1'
107900000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
108000000000     C                   MOVEL     MSG(12)       $MSG
108100000000     C                   EXSR      CARMSG
108200000000     C                   SETON                                        69
108300000000E   4C                   ENDIF
108400000000E   3C                   ENDIF
108500000000     C*
108600000000IF  3C     $ERR          IFEQ      '0'
108700000000     C                   MOVEL     V0CCT9        WCTRA             3            *TARIFFA LAVORO ALFA
108800000000     C                   EXSR      CHKCTR
108900000000     C                   MOVEL     WCTRA         V0CCT9
109000000000IF  4C     $ERR          IFEQ      '1'
109100000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
109200000000     C                   MOVEL     MSG(12)       $MSG
109300000000     C                   EXSR      CARMSG
109400000000     C                   SETON                                        70
109500000000E   4C                   ENDIF
109600000000E   3C                   ENDIF
109700000000     C*
109800000000IF  3C     $ERR          IFEQ      '0'
109900000000     C                   MOVEL     V0CCT0        WCTRA             3            *TARIFFA LAVORO ALFA
110000000000     C                   EXSR      CHKCTR
110100000000     C                   MOVEL     WCTRA         V0CCT0
110200000000IF  4C     $ERR          IFEQ      '1'
110300000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
110400000000     C                   MOVEL     MSG(12)       $MSG
110500000000     C                   EXSR      CARMSG
110600000000     C                   SETON                                        71
110700000000E   4C                   ENDIF
110800000000E   3C                   ENDIF
110900000000     C*
111000000000E   2C                   ENDIF
111100000000E   1C                   ENDIF
111200000000     C*--------------------
111300000000     C* FASCE DI PESO
111400000000     C*--------------------
111500000000IF  1C     $ERR          IFEQ      '0'
111600000000     C                   MOVEL     V0CFP1        WFPE              2
111700000000     C                   EXSR      CHKFPE
111800000000     C                   MOVEL     WFPE          V0CFP1
111900000000IF  2C     $ERR          IFEQ      '1'
112000000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
112100000000     C                   MOVEL     MSG(13)       $MSG
112200000000     C                   EXSR      CARMSG
112300000000     C                   SETON                                        72
112400000000E   2C                   ENDIF
112500000000E   1C                   ENDIF
112600000000     C*
112700000000IF  1C     $ERR          IFEQ      '0'
112800000000     C                   MOVEL     V0CFP2        WFPE              2
112900000000     C                   EXSR      CHKFPE
113000000000     C                   MOVEL     WFPE          V0CFP2
113100000000IF  2C     $ERR          IFEQ      '1'
113200000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
113300000000     C                   MOVEL     MSG(13)       $MSG
113400000000     C                   EXSR      CARMSG
113500000000     C                   SETON                                        73
113600000000E   2C                   ENDIF
113700000000E   1C                   ENDIF
113800000000     C*
113900000000IF  1C     $ERR          IFEQ      '0'
114000000000     C                   MOVEL     V0CFP3        WFPE              2
114100000000     C                   EXSR      CHKFPE
114200000000     C                   MOVEL     WFPE          V0CFP3
114300000000IF  2C     $ERR          IFEQ      '1'
114400000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
114500000000     C                   MOVEL     MSG(13)       $MSG
114600000000     C                   EXSR      CARMSG
114700000000     C                   SETON                                        74
114800000000E   2C                   ENDIF
114900000000E   1C                   ENDIF
115000000000     C*
115100000000IF  1C     $ERR          IFEQ      '0'
115200000000     C                   MOVEL     V0CFP4        WFPE              2
115300000000     C                   EXSR      CHKFPE
115400000000     C                   MOVEL     WFPE          V0CFP4
115500000000IF  2C     $ERR          IFEQ      '1'
115600000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
115700000000     C                   MOVEL     MSG(13)       $MSG
115800000000     C                   EXSR      CARMSG
115900000000     C                   SETON                                        75
116000000000E   2C                   ENDIF
116100000000E   1C                   ENDIF
116200000000     C*
116300000000IF  1C     $ERR          IFEQ      '0'
116400000000     C                   MOVEL     V0CFP5        WFPE              2
116500000000     C                   EXSR      CHKFPE
116600000000     C                   MOVEL     WFPE          V0CFP5
116700000000IF  2C     $ERR          IFEQ      '1'
116800000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
116900000000     C                   MOVEL     MSG(13)       $MSG
117000000000     C                   EXSR      CARMSG
117100000000     C                   SETON                                        76
117200000000E   2C                   ENDIF
117300000000E   1C                   ENDIF
117400000000     C*
117500000000IF  1C     $ERR          IFEQ      '0'
117600000000     C                   MOVEL     V0CFP6        WFPE              2
117700000000     C                   EXSR      CHKFPE
117800000000     C                   MOVEL     WFPE          V0CFP6
117900000000IF  2C     $ERR          IFEQ      '1'
118000000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
118100000000     C                   MOVEL     MSG(13)       $MSG
118200000000     C                   EXSR      CARMSG
118300000000     C                   SETON                                        77
118400000000E   2C                   ENDIF
118500000000E   1C                   ENDIF
118600000000     C*
118700000000IF  1C     $ERR          IFEQ      '0'
118800000000     C                   MOVEL     V0CFP7        WFPE              2
118900000000     C                   EXSR      CHKFPE
119000000000     C                   MOVEL     WFPE          V0CFP7
119100000000IF  2C     $ERR          IFEQ      '1'
119200000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
119300000000     C                   MOVEL     MSG(13)       $MSG
119400000000     C                   EXSR      CARMSG
119500000000     C                   SETON                                        78
119600000000E   2C                   ENDIF
119700000000E   1C                   ENDIF
119800000000     C*
119900000000IF  1C     $ERR          IFEQ      '0'
120000000000     C                   MOVEL     V0CFP8        WFPE              2
120100000000     C                   EXSR      CHKFPE
120200000000     C                   MOVEL     WFPE          V0CFP8
120300000000IF  2C     $ERR          IFEQ      '1'
120400000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
120500000000     C                   MOVEL     MSG(13)       $MSG
120600000000     C                   EXSR      CARMSG
120700000000     C                   SETON                                        79
120800000000E   2C                   ENDIF
120900000000E   1C                   ENDIF
121000000000     C*
121100000000IF  1C     $ERR          IFEQ      '0'
121200000000     C                   MOVEL     V0CFP9        WFPE              2
121300000000     C                   EXSR      CHKFPE
121400000000     C                   MOVEL     WFPE          V0CFP9
121500000000IF  2C     $ERR          IFEQ      '1'
121600000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
121700000000     C                   MOVEL     MSG(13)       $MSG
121800000000     C                   EXSR      CARMSG
121900000000     C                   SETON                                        80
122000000000E   2C                   ENDIF
122100000000E   1C                   ENDIF
122200000000     C*
122300000000IF  1C     $ERR          IFEQ      '0'
122400000000     C                   MOVEL     V0CFP0        WFPE              2
122500000000     C                   EXSR      CHKFPE
122600000000     C                   MOVEL     WFPE          V0CFP0
122700000000IF  2C     $ERR          IFEQ      '1'
122800000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
122900000000     C                   MOVEL     MSG(13)       $MSG
123000000000     C                   EXSR      CARMSG
123100000000     C                   SETON                                        81
123200000000E   2C                   ENDIF
123300000000E   1C                   ENDIF
123400000000     C*
123500000000IF  1C     $ERR          IFEQ      '0'
123600000000     C                   MOVEL     V0CFPA        WFPE              2
123700000000     C                   EXSR      CHKFPE
123800000000     C                   MOVEL     WFPE          V0CFPA
123900000000IF  2C     $ERR          IFEQ      '1'
124000000000     C     PUNTO         ANDEQ     '0'                                          *NO CHIESTO '?'
124100000000     C                   MOVEL     MSG(13)       $MSG
124200000000     C                   EXSR      CARMSG
124300000000     C                   SETON                                        58
124400000000E   2C                   ENDIF
124500000000E   1C                   ENDIF
124600000000     C*--------------------
124700000000     C* TIPO SERVIZIO
124800000000     C*--------------------
124900000000IF  1C     $ERR          IFEQ      '0'
125000000000IF  2C     V0CSE1        IFNE      *BLANKS
125100000000     C                   MOVEL     V0CSE1        WSER
125200000000     C                   EXSR      CHKSER
125300000000     C                   MOVEL     WSER          V0CSE1
125400000000IF  3C     $ERR          IFEQ      '1'
125500000000     C                   MOVEL     '1'           $ERR
125600000000     C                   MOVEL     MSG(14)       $MSG
125700000000     C                   EXSR      CARMSG
125800000000     C                   SETON                                        82
125900000000E   3C                   ENDIF
126000000000E   2C                   ENDIF
126100000000E   1C                   ENDIF
126200000000     C*
126300000000IF  1C     $ERR          IFEQ      '0'
126400000000IF  2C     V0CSE2        IFNE      *BLANKS
126500000000     C                   MOVEL     V0CSE2        WSER
126600000000     C                   EXSR      CHKSER
126700000000     C                   MOVEL     WSER          V0CSE2
126800000000IF  3C     $ERR          IFEQ      '1'
126900000000     C                   MOVEL     '1'           $ERR
127000000000     C                   MOVEL     MSG(14)       $MSG
127100000000     C                   EXSR      CARMSG
127200000000     C                   SETON                                        83
127300000000E   3C                   ENDIF
127400000000E   2C                   ENDIF
127500000000E   1C                   ENDIF
127600000000     C*
127700000000IF  1C     $ERR          IFEQ      '0'
127800000000IF  2C     V0CSE3        IFNE      *BLANKS
127900000000     C                   MOVEL     V0CSE3        WSER
128000000000     C                   EXSR      CHKSER
128100000000     C                   MOVEL     WSER          V0CSE3
128200000000IF  3C     $ERR          IFEQ      '1'
128300000000     C                   MOVEL     '1'           $ERR
128400000000     C                   MOVEL     MSG(14)       $MSG
128500000000     C                   EXSR      CARMSG
128600000000     C                   SETON                                        84
128700000000E   3C                   ENDIF
128800000000E   2C                   ENDIF
128900000000E   1C                   ENDIF
129000000000     C*
129100000000IF  1C     $ERR          IFEQ      '0'
129200000000IF  2C     V0CSE4        IFNE      *BLANKS
129300000000     C                   MOVEL     V0CSE4        WSER
129400000000     C                   EXSR      CHKSER
129500000000     C                   MOVEL     WSER          V0CSE4
129600000000IF  3C     $ERR          IFEQ      '1'
129700000000     C                   MOVEL     '1'           $ERR
129800000000     C                   MOVEL     MSG(14)       $MSG
129900000000     C                   EXSR      CARMSG
130000000000     C                   SETON                                        85
130100000000E   3C                   ENDIF
130200000000E   2C                   ENDIF
130300000000E   1C                   ENDIF
130400000000     C*
130500000000IF  1C     $ERR          IFEQ      '0'
130600000000IF  2C     V0CSE5        IFNE      *BLANKS
130700000000     C                   MOVEL     V0CSE5        WSER
130800000000     C                   EXSR      CHKSER
130900000000     C                   MOVEL     WSER          V0CSE5
131000000000IF  3C     $ERR          IFEQ      '1'
131100000000     C                   MOVEL     '1'           $ERR
131200000000     C                   MOVEL     MSG(14)       $MSG
131300000000     C                   EXSR      CARMSG
131400000000     C                   SETON                                        86
131500000000E   3C                   ENDIF
131600000000E   2C                   ENDIF
131700000000E   1C                   ENDIF
131800000000     C*--------------------
131900000000     C* TIPO PORTO
132000000000     C*--------------------
132100000000IF  1C     $ERR          IFEQ      '0'
132200000000IF  2C     V0CPO1        IFEQ      'F'
132300000000     C     V0CPO1        OREQ      'A'
132400000000     C     V0CPO1        OREQ      ' '
132500000000X   2C                   ELSE
132600000000     C                   MOVEL     '1'           $ERR
132700000000     C                   MOVEL     MSG(16)       $MSG
132800000000     C                   EXSR      CARMSG
132900000000     C                   SETON                                        87
133000000000E   2C                   ENDIF
133100000000E   1C                   ENDIF
133200000000     C*
133300000000IF  1C     $ERR          IFEQ      '0'
133400000000IF  2C     V0CPO2        IFEQ      'F'
133500000000     C     V0CPO2        OREQ      'A'
133600000000     C     V0CPO2        OREQ      ' '
133700000000X   2C                   ELSE
133800000000     C                   MOVEL     '1'           $ERR
133900000000     C                   MOVEL     MSG(16)       $MSG
134000000000     C                   EXSR      CARMSG
134100000000     C                   SETON                                        88
134200000000E   2C                   ENDIF
134300000000E   1C                   ENDIF
134400000000     C*--------------------
134500000000     C* TIPO PRODOTTO
134600000000     C*--------------------
134700000000IF  1C***        $ERR      IFEQ '0'
134800000000IF  2C***        V0CPR1    IFEQ 'B'
134900000000     C***        V0CPR1    OREQ 'P'
135000000000     C***        V0CPR1    OREQ 'M'
135100000000     C***        V0CPR1    OREQ ' '
135200000000X   2C***                  ELSE
135300000000     C***                  MOVEL'1'       $ERR
135400000000     C***                  MOVELMSG,16    $MSG
135500000000     C***                  EXSR CARMSG
135600000000     C***                  SETON                     89
135700000000E   2C***                  ENDIF
135800000000E   1C***                  ENDIF
135900000000     C*
136000000000IF  1C***        $ERR      IFEQ '0'
136100000000IF  2C***        V0CPR2    IFEQ 'B'
136200000000     C***        V0CPR2    OREQ 'P'
136300000000     C***        V0CPR2    OREQ 'M'
136400000000     C***        V0CPR2    OREQ ' '
136500000000X   2C***                  ELSE
136600000000     C***                  MOVEL'1'       $ERR
136700000000     C***                  MOVELMSG,16    $MSG
136800000000     C***                  EXSR CARMSG
136900000000     C***                  SETON                     90
137000000000E   2C***                  ENDIF
137100000000E   1C***                  ENDIF
137200000000     C*
137300000000IF  1C***        $ERR      IFEQ '0'
137400000000IF  2C***        V0CPR3    IFEQ 'B'
137500000000     C***        V0CPR3    OREQ 'P'
137600000000     C***        V0CPR3    OREQ 'M'
137700000000     C***        V0CPR3    OREQ ' '
137800000000X   2C***                  ELSE
137900000000     C***                  MOVEL'1'       $ERR
138000000000     C***                  MOVELMSG,16    $MSG
138100000000     C***                  EXSR CARMSG
138200000000     C***                  SETON                     91
138300000000E   2C***                  ENDIF
138400000000E   1C***                  ENDIF
138500000000     C*
138600000000IF  1C***        $ERR      IFEQ '0'
138700000000IF  2C***        V0CPR4    IFEQ 'B'
138800000000     C***        V0CPR4    OREQ 'P'
138900000000     C***        V0CPR4    OREQ 'M'
139000000000     C***        V0CPR4    OREQ ' '
139100000000X   2C***                  ELSE
139200000000     C***                  MOVEL'1'       $ERR
139300000000     C***                  MOVELMSG,16    $MSG
139400000000     C***                  EXSR CARMSG
139500000000     C***                  SETON                     92
139600000000E   2C***                  ENDIF
139700000000E   1C***                  ENDIF
139800000000     C*
139900050110      ****** attenzione indicatore 93 è stato riutilizzato per
140000050110      ******   msg statistica pesi/volumi 10/01/2005
140100000000IF  1C***        $ERR      IFEQ '0'
140200000000IF  2C***        V0CPR5    IFEQ 'B'
140300000000     C***        V0CPR5    OREQ 'P'
140400000000     C***        V0CPR5    OREQ 'M'
140500000000     C***        V0CPR5    OREQ ' '
140600000000X   2C***                  ELSE
140700000000     C***                  MOVEL'1'       $ERR
140800000000     C***                  MOVELMSG,16    $MSG
140900000000     C***                  EXSR CARMSG
141000000000     C***                  SETON                     93
141100000000E   2C***                  ENDIF
141200000000E   1C***                  ENDIF
141300000000     C*--------------------
141400000000     C* STATISTICHE
141500000000     C*--------------------
141600960604IF  1C     D60OP0        IFEQ      'VIS'
141700020523     C     D60OP1        OREQ      'STT'
141800050110      *
141900000000IF  2C     $ERR          IFEQ      '0'
142000000000IF  3C     V0CST1        IFEQ      'S'
142100000000     C     V0CST1        OREQ      'N'
142200000000X   3C                   ELSE
142300000000     C                   MOVEL     '1'           $ERR
142400000000     C                   MOVEL     MSG(17)       $MSG
142500000000     C                   EXSR      CARMSG
142600000000     C                   SETON                                        94
142700000000E   3C                   ENDIF
142800000000E   2C                   ENDIF
142900050110      *
143000050110IF  2C     $ERR          IFEQ      '0'
143100050110IF  3C     V0CSTp        IFEQ      'S'
143200050110     C     V0CSTp        OREQ      'N'
143300050110X   3C                   ELSE
143400050110     C                   MOVEL     '1'           $ERR
143500050110     C                   MOVEL     MSG(17)       $MSG
143600050110     C                   EXSR      CARMSG
143700050110     C                   SETON                                        93
143800050110E   3C                   ENDIF
143900050110E   2C                   ENDIF
144000000000     C*
144100000000IF  2C     $ERR          IFEQ      '0'
144200000000IF  3C     V0CST2        IFEQ      'S'
144300000000     C     V0CST2        OREQ      'N'
144400000000X   3C                   ELSE
144500000000     C                   MOVEL     '1'           $ERR
144600000000     C                   MOVEL     MSG(17)       $MSG
144700000000     C                   EXSR      CARMSG
144800000000     C                   SETON                                        95
144900000000E   3C                   ENDIF
145000000000E   2C                   ENDIF
145100000000     C*
145200000000IF  2C     $ERR          IFEQ      '0'
145300000000IF  3C     V0CST3        IFEQ      'S'
145400000000     C     V0CST3        OREQ      'N'
145500000000X   3C                   ELSE
145600000000     C                   MOVEL     '1'           $ERR
145700000000     C                   MOVEL     MSG(17)       $MSG
145800000000     C                   EXSR      CARMSG
145900000000     C                   SETON                                        96
146000000000E   3C                   ENDIF
146100000000E   2C                   ENDIF
146200000000     C*
146300000000IF  2C     $ERR          IFEQ      '0'
146400000000IF  3C     V0CST4        IFEQ      'S'
146500000000     C     V0CST4        OREQ      'N'
146600000000X   3C                   ELSE
146700000000     C                   MOVEL     '1'           $ERR
146800000000     C                   MOVEL     MSG(17)       $MSG
146900000000     C                   EXSR      CARMSG
147000000000     C                   SETON                                        97
147100000000E   3C                   ENDIF
147200000000E   2C                   ENDIF
147300000000     C*
147400000000IF  2C     $ERR          IFEQ      '0'
147500000000IF  3C     V0CST5        IFEQ      'S'
147600000000     C     V0CST5        OREQ      'N'
147700000000X   3C                   ELSE
147800000000     C                   MOVEL     '1'           $ERR
147900000000     C                   MOVEL     MSG(17)       $MSG
148000000000     C                   EXSR      CARMSG
148100000000     C                   SETON                                        98
148200000000E   3C                   ENDIF
148300000000E   2C                   ENDIF
148400000000     C*
148500000000     C* RICHIEDERE ALMENO UNA STATISTICA FRA LE 5
148600000000IF  2C     $ERR          IFEQ      '0'
148700000000     C     'S'           SCAN      V0CSTA                                 01
148800000000IF  3C     *IN01         IFEQ      *OFF
148900000000     C                   MOVEL     '1'           $ERR
149000000000     C                   MOVEL     MSG(22)       $MSG
149100000000     C                   EXSR      CARMSG
149200000000     C                   SETON                                        949596
149300000000     C                   SETON                                        9798
149400000000E   3C                   ENDIF
149500000000E   2C                   ENDIF
149600000000     C*
149700000000E   1C                   ENDIF
149800000000     C*
149900000000     C                   ENDSR
150000000000     C*------------------------------------------------------------------------*
150100000000     C*?DECOD1 - CONTROLLA I PARAMETRI DELLA STATISTICA REGIONE-REGIONE
150200000000     C*------------------------------------------------------------------------*
150300000000     C     DECOD1        BEGSR
150400000000     C*
150500000000     C* REIMPOSTO INDICATORI DI ERRORE SUI CAMPI
150600000000     C                   SETOFF                                       5051
150700000000     C                   MOVEL     '0'           $ERR                           *ERRORE GENERALE
150800000000     C                   MOVEL     '0'           $ERR2                          *ERRORE SPECIF VIDEO
150900000000     C*--------------------
151000000000     C* VALORE DI RIFERIMENTO
151100000000     C*--------------------
151200000000IF  1C     V1CRIF        IFEQ      'C'
151300000000     C     V1CRIF        OREQ      'P'
151400000000     C     V1CRIF        OREQ      'R'
151500000000     C     V1CRIF        OREQ      'S'
151600000000     C     V1CRIF        OREQ      'V'
151700950206     C     V1CRIF        OREQ      ' '
151800000000X   1C                   ELSE
151900000000     C                   MOVEL     '1'           $ERR
152000000000     C                   MOVEL     '1'           $ERR2                          *ERRORE GEST PARTIC
152100000000     C                   MOVEL     MSG(18)       $MSG
152200000000     C                   EXSR      CARMSG
152300000000     C                   SETON                                        50
152400000000E   1C                   ENDIF
152500000000     C*--------------------
152600000000     C* REGIONE
152700000000     C*--------------------
152800971128IF  1C*---       $ERR      IFEQ '0'
152900971128IF  2C*---       V1CREG    IFEQ 'P'
153000971128     C*---       V1CREG    OREQ 'A'
153100971128     C*---       V1CREG    OREQ 'E'
153200971128X   2C*---                 ELSE
153300971128     C*---                 MOVEL'1'       $ERR
153400971128     C*---                 MOVEL'1'       $ERR2            *ERRORE GEST PARTIC
153500971128     C*---                 MOVELMSG,19    $MSG
153600971128     C*---                 EXSR CARMSG
153700971128     C*---                 SETON                     51
153800971128E   2C*---                 ENDIF
153900971128E   1C*---                 ENDIF
154000000000     C*
154100000000     C* NON POSSO LANCIARE LA STATISTICA REGIONE PER TUTTI I CLIENTI SE VOGLIO
154200000000     C* STAMPARE I RICAVI E I SALDI NON SONO AGGIORNATI
154300000000IF  1C     $ERR          IFEQ      '0'
154400020523IF  2C                   IF        (D60OP1 = 'REG' OR D60OP1 = 'STT') AND
154500020523     C                             V1CRIF = 'R' AND WFSC = 'N'
154600000000     C                   MOVEL     '1'           $ERR
154700000000     C                   MOVEL     MSG(21)       $MSG
154800000000     C                   EXSR      CARMSG
154900000000E   2C                   ENDIF
155000000000E   1C                   ENDIF
155100000000     C*
155200000000     C                   ENDSR
155300050110     C*------------------------------------------------------------------------*
155400050110     C*?DECODP - CONTROLLA I PARAMETRI DELLA STATISTICA PESO/VOLUME
155500050110     C*------------------------------------------------------------------------*
155600050110     C     DECODP        BEGSR
155700050110     C*
155800050110     C                   MOVEL     '0'           $ERR                           *ERRORE GENERALE
155900050110     C                   MOVEL     '0'           $ERR2                          *ERRORE SPECIF VIDEO
156000050110     C*
156100050110     C                   ENDSR
156200000000     C*------------------------------------------------------------------------*
156300000000     C*?DECOD2 - CONTROLLA I PARAMETRI DELLA STATISTICA TIPO SERVIZIO
156400000000     C*------------------------------------------------------------------------*
156500000000     C     DECOD2        BEGSR
156600000000     C*
156700000000     C                   MOVEL     '0'           $ERR                           *ERRORE GENERALE
156800000000     C                   MOVEL     '0'           $ERR2                          *ERRORE SPECIF VIDEO
156900000000     C*
157000000000     C                   ENDSR
157100000000     C*------------------------------------------------------------------------*
157200000000     C*?DECOD3 - CONTROLLA I PARAMETRI DELLA STATISTICA AFFIDABILITA'
157300000000     C*------------------------------------------------------------------------*
157400000000     C     DECOD3        BEGSR
157500000000     C*
157600000000     C                   MOVEL     '0'           $ERR                           *ERRORE GENERALE
157700000000     C                   MOVEL     '0'           $ERR2                          *ERRORE SPECIF VIDEO
157800000000     C*
157900000000     C                   ENDSR
158000000000     C*------------------------------------------------------------------------*
158100000000     C*?DECOD4 - CONTROLLA I PARAMETRI DELLA STATISTICA DELTA CLIENTE
158200000000     C*------------------------------------------------------------------------*
158300000000     C     DECOD4        BEGSR
158400000000     C*
158500000000     C                   MOVEL     '0'           $ERR                           *ERRORE GENERALE
158600000000     C                   MOVEL     '0'           $ERR2                          *ERRORE SPECIF VIDEO
158700000000     C*
158800000000     C                   ENDSR
158900000000     C*------------------------------------------------------------------------*
159000000000     C*?DECOD5 - CONTROLLA I PARAMETRI DELLA STATISTICA AMMINISTRAZIONE
159100000000     C*------------------------------------------------------------------------*
159200000000     C     DECOD5        BEGSR
159300000000     C*
159400000000     C                   MOVEL     '0'           $ERR                           *ERRORE GENERALE
159500000000     C                   MOVEL     '0'           $ERR2                          *ERRORE SPECIF VIDEO
159600000000     C*
159700000000     C                   ENDSR
159800000000     C*------------------------------------------------------------------------*
159900000000     C*?DECOD6 - CONTROLLA I PARAMETRI DELLA STATISTICA DELTA FILIALE
160000000000     C*------------------------------------------------------------------------*
160100000000     C     DECOD6        BEGSR
160200000000     C*
160300000000     C                   MOVEL     '0'           $ERR                           *ERRORE GENERALE
160400000000     C                   MOVEL     '0'           $ERR2                          *ERRORE SPECIF VIDEO
160500000000     C*
160600000000     C                   ENDSR
160700000000     C*------------------------------------------------------------------------*
160800000000     C*?DECOD7 - CONTROLLA I PARAMETRI DELLA STATISTICA DELTA COMMERCIALE
160900000000     C*------------------------------------------------------------------------*
161000000000     C     DECOD7        BEGSR
161100000000     C*
161200000000     C                   MOVEL     '0'           $ERR                           *ERRORE GENERALE
161300000000     C                   MOVEL     '0'           $ERR2                          *ERRORE SPECIF VIDEO
161400000000     C*
161500000000     C                   ENDSR
161600000000     C*------------------------------------------------------------------------*
161700000000     C*?CHKCTR - CONTROLLA CODICE TARIFFA
161800000000     C*          INPUT --> WCTRA : CODICE TARIFFA ALFABETICO
161900000000     C*          OUTPUT--> $ERR  : 1-ERRORE, 0-NO ERRORE
162000000000     C*------------------------------------------------------------------------*
162100000000     C     CHKCTR        BEGSR
162200000000     C*
162300000000     C* SE C'E' IL ? DI RICERCA DEVO RIEMETTERE LA VIDEATA SENZA MESS DI ERRORE
162400000000     C                   MOVEL     '0'           PUNTO             1
162500000000     C*
162600000000     C* SE UGUALE A BLANKS NON CONTROLLO
162700000000IF  1C     WCTRA         IFNE      *BLANKS
162800000000     C*
162900000000     C     '?'           SCAN      WCTRA                                  01
163000000000IF  2C     *IN01         IFEQ      *ON                                          *RICERCA TARIFFE
163100000000     C                   CLEAR                   PARAM4
163200000000     C                   MOVEL     V0CKSC        PARCOD                         *CODICE CLIENTE
163300000000     C                   MOVEL     '1'           PARFLG                         *1-TARIFFE/2-OFFERTE
163400020916     C                   Z-ADD     DUTKCI        PARCCC                         *SOTTOCONTO
163500000000     C                   MOVEL     PARAM4        KPJBU
163600000000     C                   CALL      'TNTA36R'
163700000000     C                   PARM                    KPJBA
163800000000     C                   MOVEL     KPJBU         PARAM4
163900000000     C*
164000000000     C                   MOVEL     PARCTR        WCTRA                          *TARIFFA SELEZIONATA
164100000000     C                   MOVEL     '1'           $ERR
164200000000     C                   MOVEL     '1'           $TESVD
164300000000     C                   MOVEL     '1'           PUNTO                          *CHIESTO PUNTO '?'
164400000000     C*
164500000000     C* INSERITA TARIFFA CONTROLLO ESISTENZA
164600000000X   2C                   ELSE
164700000000IF  3C     WCTRA         IFGE      *ZEROS
164800000000     C                   MOVEL     V0CKSC        KTAKSC                         *CODICE CLIENTE
164900000000     C                   MOVEL     WCTRA         KTACTR                         *CODICE TARIFFA NUME
165000000000     C*
165100000000     C     KEYTAM        CHAIN     TNTAM01L                           99
165200000000IF  4C     *IN99         IFEQ      *ON
165300000000     C                   MOVEL     '1'           $ERR
165400000000E   4C                   ENDIF
165500000000     C*
165600000000     C* INSERITI CARATTERI ALFABETICI, ERRORE
165700000000X   3C                   ELSE
165800000000     C                   MOVEL     '1'           $ERR
165900000000E   3C                   ENDIF
166000000000     C*
166100000000E   2C                   ENDIF
166200000000E   1C                   ENDIF
166300000000     C*
166400000000     C                   ENDSR
166500000000     C*------------------------------------------------------------------------*
166600000000     C*?CHKFPE - CONTROLLA FASCIA DI PESO
166700000000     C*          INPUT --> WFPE : FASCIA DI PESO
166800000000     C*          OUTPUT--> $ERR : 1-ERRORE, 0-NO ERRORE
166900000000     C*------------------------------------------------------------------------*
167000000000     C     CHKFPE        BEGSR
167100000000     C*
167200000000     C* SE C'E' IL ? DI RICERCA DEVO RIEMETTERE LA VIDEATA SENZA MESS DI ERRORE
167300000000     C                   MOVEL     '0'           PUNTO             1
167400000000     C*
167500000000     C* SE UGUALE A BLANKS NON CONTROLLO
167600000000IF  1C     WFPE          IFNE      *BLANKS
167700000000     C     '?'           SCAN      WFPE                                   01
167800000000IF  2C     *IN01         IFEQ      *ON                                          *RICERCA FASCE
167900990601IF  3C     V0CDA2        IFLE      199812                                       *FASCE FINO AL '98
168000990217     C                   MOVEL(P)  'SDC'         §KEY
168100030320X   3C                   ELSE                                                   *FASCE DAL '99 AL 2001
168200030320IF  4C     V0CDA2        IFLE      200112
168300030320     C                   MOVEL     'S9C'         §KEY
168400030320X   4C                   ELSE                                                   *FASCE DAL 2002
168500030320     C                   MOVEL     'S2C'         §KEY
168600030320E   4C                   ENDIF
168700030320E   3C                   ENDIF
168800020916     C                   Z-ADD     1             §KUT
168900000000     C                   MOVEL     '2L'          §COD
169000000000     C                   MOVEL     *BLANKS       §DES             25
169100020117     C                   CALL      'TISE67R'
169200000000     C                   PARM                    §KUT
169300000000     C                   PARM                    §COD
169400000000     C                   PARM                    §KEY
169500000000     C                   PARM                    §DES
169600000000     C                   MOVEL     §KEY          WFPE
169700000000     C                   MOVEL     '1'           $ERR
169800000000     C                   MOVEL     '1'           $TESVD
169900000000     C                   MOVEL     '1'           PUNTO             1            *CHIESTO PUNTO '?'
170000000000X   2C                   ELSE
170100000000     C*
170200000000     C* INSERITA FASCIA CONTROLLO ESISTENZA
170300990601IF  3C     V0CDA2        IFLE      199812                                       *FASCE FINO AL '98
170400990217     C                   MOVEL(P)  'SDC'         FASCIA            5
170500990217X   3C                   ELSE                                                   *FASCE DAL '99
170600041129    4C     V0CDA2        IFLE      200112
170700990217     C                   MOVEL(P)  'S9C'         FASCIA            5
170800041129x   4c                   else
170900041129     C                   MOVEL(P)  'S2C'         FASCIA            5
171000041129E   4C                   ENDIF
171100041129E   3C                   ENDIF
171200000000     C                   MOVE      WFPE          FASCIA
171300020916     C                   Z-ADD     1             KTBKUT
171400000000     C                   MOVEL     '2L'          KTBCOD
171500000000     C                   MOVEL     *BLANKS       KTBKEY
171600000000     C                   MOVEL     FASCIA        KTBKEY
171700000000     C     KEYTAB        CHAIN     TABEL00F                           99
171800000000IF  3C     *IN99         IFEQ      *ON
171900000000     C                   MOVEL     '1'           $ERR                           *FASCIA INESISTENTE
172000000000E   3C                   ENDIF
172100990217E   2C                   ENDIF
172200990217E   1C                   ENDIF
172300000000     C*
172400000000     C                   ENDSR
172500000000     C*------------------------------------------------------------------------*
172600000000     C*?CHKSER - CONTROLLOA IL TIPO SERVIZIO
172700000000     C*------------------------------------------------------------------------*
172800000000     C     CHKSER        BEGSR
172900000000     C*
173000000000     C     *LIKE         DEFINE    V0CSE1        WSER
173100000000     C*
173200000000     C* E' STATO INSERITO '?', LANCIO IL PGM DI INTERROGAZIONE TABELLE
173300000000IF  1C     WSER          IFEQ      '?'
173400020916     C                   MOVEL     1             §KUT
173500000000     C                   MOVEL     '5E'          §COD
173600000000     C                   MOVEL     *BLANKS       §KEY
173700000000     C                   MOVEL     *BLANKS       §DES
173800000000     C                   CALL      'X§TABER'
173900000000     C                   PARM                    §KUT
174000000000     C                   PARM                    §COD
174100000000     C                   PARM                    §KEY
174200000000     C                   PARM                    §DES
174300000000     C                   MOVEL     §KEY          WSER
174400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
174500000000E   1C                   ENDIF
174600000000     C*
174700000000     C* NON E' STATO INSERITO '?', CONTROLLO CHE ESISTA IN TABELLA
174800020916     C                   MOVEL     1             KTBKUT
174900000000     C                   MOVEL     '5E'          KTBCOD
175000000000     C                   MOVEL     *BLANKS       KTBKEY
175100000000     C                   MOVEL     WSER          KTBKEY
175200000000     C     KEYTAB        CHAIN     TABEL00F                           99
175300000000IF  1C     *IN99         IFEQ      *ON
175400000000     C     TBLFLG        OREQ      '*'
175500000000     C                   MOVEL     '1'           $ERR
175600000000E   1C                   ENDIF
175700000000     C*
175800000000     C                   ENDSR
175900000000     C*------------------------------------------------------------------------*
176000000000     C*?CARRIC - CARICA LE STATISTICHE SELEZIONATE
176100000000     C*------------------------------------------------------------------------*
176200000000     C     CARRIC        BEGSR
176300000000     C*
176400000000     C                   Z-ADD     1             IN                2 0          *INIZIO SCHIERA
176500000000     C                   MOVEL     'V0'          RIC(IN)                        *RICHIESTA PARAMETRI
176600000000     C*
176700000000IF  1C     V0CST1        IFEQ      'S'
176800000000     C     PARREG        ANDEQ     'S'                                          *CI SONO PARAMETRI
176900000000     C                   ADD       1             IN
177000000000     C                   MOVEL     'VR'          RIC(IN)                        *REGIONE-REGIONE
177100000000E   1C                   ENDIF
177200050110     C*
177300050110IF  1C     V0CSTp        IFEQ      'S'
177400050110     C     PARPEV        ANDEQ     'S'                                          *CI SONO PARAMETRI
177500050110     C                   ADD       1             IN
177600050110     C                   MOVEL     'PV'          RIC(IN)                        *PESI-VOLUMI
177700050110E   1C                   ENDIF
177800000000     C*
177900000000IF  1C     V0CST2        IFEQ      'S'
178000000000     C     PARSER        ANDEQ     'S'                                          *CI SONO PARAMETRI
178100000000     C                   ADD       1             IN
178200000000     C                   MOVEL     'VS'          RIC(IN)                        *SERVIZIO BOLLE
178300000000E   1C                   ENDIF
178400000000     C*
178500000000IF  1C     V0CST3        IFEQ      'S'
178600000000     C     PARAFF        ANDEQ     'S'                                          *CI SONO PARAMETRI
178700000000     C                   ADD       1             IN
178800000000     C                   MOVEL     'VA'          RIC(IN)                        *AFFIDABILITA'
178900000000E   1C                   ENDIF
179000000000     C*
179100000000IF  1C     V0CST4        IFEQ      'S'
179200000000     C     PARDEL        ANDEQ     'S'                                          *CI SONO PARAMETRI
179300000000     C                   ADD       1             IN
179400000000     C                   MOVEL     'VD'          RIC(IN)                        *DELTA PER CLIENTE
179500000000E   1C                   ENDIF
179600000000     C*
179700000000IF  1C     V0CST5        IFEQ      'S'
179800000000     C     PARAMM        ANDEQ     'S'                                          *CI SONO PARAMETRI
179900000000     C                   ADD       1             IN
180000000000     C                   MOVEL     'VM'          RIC(IN)                        *AMMINISTRAZIONE
180100000000E   1C                   ENDIF
180200000000     C*
180300000000IF  1C     V0CST6        IFEQ      'S'
180400000000     C     PARDEF        ANDEQ     'S'                                          *CI SONO PARAMETRI
180500000000     C                   ADD       1             IN
180600000000     C                   MOVEL     'VF'          RIC(IN)                        *DELTA PER FILIALE
180700000000E   1C                   ENDIF
180800000000     C*
180900000000IF  1C     V0CST7        IFEQ      'S'
181000000000     C     PARDEC        ANDEQ     'S'                                          *CI SONO PARAMETRI
181100000000     C                   ADD       1             IN
181200000000     C                   MOVEL     'VC'          RIC(IN)                        *DELTA PER COMM.LE
181300000000E   1C                   ENDIF
181400000000     C*
181500000000     C                   ADD       1             IN                             *FINE SCHIERA
181600000000     C                   MOVEL     'V0'          RIC(IN)                        *RICHIESTA PARAMETRI
181700000000     C*
181800000000     C                   ENDSR
181900000000     C*------------------------------------------------------------------------*
182000000000     C*?OUTVR - EMETTE I PARAMETRI DELLA STATISTICA REGIONE-REGIONE
182100000000     C*------------------------------------------------------------------------*
182200000000     C     OUTVR         BEGSR
182300000000     C*
182400000000IF  1C     $TESVD        IFEQ      '1'
182500000000     C                   MOVEL     DSTA(1)       VICINT                         *DESCRIZ STATISTICA
182600000000     C*
182700000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
182800000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
182900000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
183000000000     C*
183100010920     C                   WRITE     SE61DVI                                      *INTESTAZIONE
183200010920     C                   WRITE     SE61DVF                                      *FUNZIONI
183300000000     C*
183400000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
183500000000E   1C                   ENDIF
183600000000     C*
183700000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
183800000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
183900000000E   1C                   ENDIF
184000000000     C*
184100010920     C                   EXFMT     SE61DV1                                      *PARAMETRI REG-REG
184200000000     C*
184300000000     C* CONTROLLO FROMATO VIDEO
184400000000     C                   EXSR      DECOD1
184500000000     C*
184600000000     C                   ENDSR
184700050110     C*------------------------------------------------------------------------*
184800050110     C*?OUTPV - EMETTE I PARAMETRI DELLA STATISTICA PESI VOLUMI
184900050110     C*------------------------------------------------------------------------*
185000050110     C     OUTPV         BEGSR
185100050110     C*
185200050110IF  1C     $TESVD        IFEQ      '1'
185300050110     C                   MOVEL     DSTA(14)      VICINT                         *DESCRIZ STATISTICA
185400050110     C*
185500050110     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
185600050110     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
185700050110     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
185800050110     C*
185900050110     C                   WRITE     SE61DVI                                      *INTESTAZIONE
186000050110     C                   WRITE     SE61DVF                                      *FUNZIONI
186100050110     C*
186200050110     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
186300050110E   1C                   ENDIF
186400050110     C*
186500050110IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
186600050110     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
186700050110E   1C                   ENDIF
186800050110     C*
186900050110     C                   EXFMT     SE61DVP                                      *PARAMETRI PESI VOLUMI
187000050110     C*
187100050110     C* CONTROLLO FROMATO VIDEO
187200050110     C                   EXSR      DECODP
187300050110     C*
187400050110     C                   ENDSR
187500000000     C*------------------------------------------------------------------------*
187600000000     C*?OUTVS - EMETTE I PARAMETRI DELLA STATISTICA SERVIZIO BOLLE
187700000000     C*------------------------------------------------------------------------*
187800000000     C     OUTVS         BEGSR
187900000000     C*
188000000000IF  1C     $TESVD        IFEQ      '1'
188100000000     C                   MOVEL     DSTA(2)       VICINT                         *DESCRIZ STATISTICA
188200000000     C*
188300000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
188400000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
188500000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
188600000000     C*
188700010920     C                   WRITE     SE61DVI                                      *INTESTAZIONE
188800010920     C                   WRITE     SE61DVF                                      *FUNZIONI
188900000000     C*
189000000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
189100000000E   1C                   ENDIF
189200000000     C*
189300000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
189400000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
189500000000E   1C                   ENDIF
189600000000     C*
189700010920     C                   EXFMT     SE61DV2                                      *PARAMETRI SERV BOLL
189800000000     C*
189900000000     C* CONTROLLO FROMATO VIDEO
190000000000     C                   EXSR      DECOD2
190100000000     C*
190200000000     C                   ENDSR
190300000000     C*------------------------------------------------------------------------*
190400000000     C*?OUTVA - EMETTE I PARAMETRI DELLA STATISTICA AFFIDABILITA' SPEDIZIONI
190500000000     C*------------------------------------------------------------------------*
190600000000     C     OUTVA         BEGSR
190700000000     C*
190800000000IF  1C     $TESVD        IFEQ      '1'
190900000000     C                   MOVEL     DSTA(3)       VICINT                         *DESCRIZ STATISTICA
191000000000     C*
191100000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
191200000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
191300000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
191400000000     C*
191500010920     C                   WRITE     SE61DVI                                      *INTESTAZIONE
191600010920     C                   WRITE     SE61DVF                                      *FUNZIONI
191700000000     C*
191800000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
191900000000E   1C                   ENDIF
192000000000     C*
192100000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
192200000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
192300000000E   1C                   ENDIF
192400000000     C*
192500010920     C                   EXFMT     SE61DV3                                      *PARAMETRI AFF.TA' S
192600000000     C*
192700000000     C* CONTROLLO FROMATO VIDEO
192800000000     C                   EXSR      DECOD3
192900000000     C*
193000000000     C                   ENDSR
193100000000     C*------------------------------------------------------------------------*
193200000000     C*?OUTVD - EMETTE I PARAMETRI DELLA STATISTICA DELTA PER CLIENTE
193300000000     C*------------------------------------------------------------------------*
193400000000     C     OUTVD         BEGSR
193500000000     C*
193600000000IF  1C     $TESVD        IFEQ      '1'
193700000000     C                   MOVEL     DSTA(4)       VICINT                         *DESCRIZ STATISTICA
193800000000     C*
193900000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
194000000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
194100000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
194200000000     C*
194300010920     C                   WRITE     SE61DVI                                      *INTESTAZIONE
194400010920     C                   WRITE     SE61DVF                                      *FUNZIONI
194500000000     C*
194600000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
194700000000E   1C                   ENDIF
194800000000     C*
194900000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
195000000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
195100000000E   1C                   ENDIF
195200000000     C*
195300010920     C                   EXFMT     SE61DV4                                      *PARAMETRI DELTA CLI
195400000000     C*
195500000000     C* CONTROLLO FROMATO VIDEO
195600000000     C                   EXSR      DECOD4
195700000000     C*
195800000000     C                   ENDSR
195900000000     C*------------------------------------------------------------------------*
196000000000     C*?OUTVM - EMETTE I PARAMETRI DELLA STATISTICA AMMINISTRAZIONE
196100000000     C*------------------------------------------------------------------------*
196200000000     C     OUTVM         BEGSR
196300000000     C*
196400000000IF  1C     $TESVD        IFEQ      '1'
196500000000     C                   MOVEL     DSTA(5)       VICINT                         *DESCRIZ STATISTICA
196600000000     C*
196700000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
196800000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
196900000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
197000000000     C*
197100010920     C                   WRITE     SE61DVI                                      *INTESTAZIONE
197200010920     C                   WRITE     SE61DVF                                      *FUNZIONI
197300000000     C*
197400000000     C                   MOVEL     '0'           $TESVD                         *TESTATA RIEMESSA
197500000000E   1C                   ENDIF
197600000000     C*
197700000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
197800000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
197900000000E   1C                   ENDIF
198000000000     C*
198100010920     C                   EXFMT     SE61DV5                                      *PARAMETRI AMMIN.NE
198200000000     C*
198300000000     C* CONTROLLO FROMATO VIDEO
198400000000     C                   EXSR      DECOD5
198500000000     C*
198600000000     C                   ENDSR
198700000000     C*------------------------------------------------------------------------*
198800000000     C*?OUTVF - EMETTE I PARAMETRI DELLA STATISTICA DELTA FILIALE
198900000000     C*------------------------------------------------------------------------*
199000000000     C     OUTVF         BEGSR
199100000000     C*
199200000000     C                   MOVEL     DSTA(6)       VICINT                         *DESCRIZ STATISTICA
199300000000     C*
199400000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
199500000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
199600000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
199700000000     C*
199800010920     C                   WRITE     SE61DVI                                      *INTESTAZIONE
199900010920     C                   WRITE     SE61DVF                                      *FUNZIONI
200000000000     C*
200100000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
200200000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
200300000000E   1C                   ENDIF
200400000000     C*
200500010920     C                   EXFMT     SE61DV6                                      *PARAMETRI DELTA FIL
200600000000     C*
200700000000     C* CONTROLLO FROMATO VIDEO
200800000000     C                   EXSR      DECOD6
200900000000     C*
201000000000     C                   ENDSR
201100000000     C*------------------------------------------------------------------------*
201200000000     C*?OUTVC - EMETTE I PARAMETRI DELLA STATISTICA DELTA COMMERCIALE
201300000000     C*------------------------------------------------------------------------*
201400000000     C     OUTVC         BEGSR
201500000000     C*
201600000000     C                   MOVEL     DSTA(7)       VICINT                         *DESCRIZ STATISTICA
201700000000     C*
201800000000     C                   MOVEA     FUN2          FUN                            *SCHIERA FUNZ.VIDEO
201900000000     C                   EXSR      CARFUN                                       *CARICA FUNZIONI
202000000000     C                   EXSR      OUTFUN                                       *EMETTE FUNZIONI
202100000000     C*
202200010920     C                   WRITE     SE61DVI                                      *INTESTAZIONE
202300010920     C                   WRITE     SE61DVF                                      *FUNZIONI
202400000000     C*
202500000000IF  1C     MSGULT        IFNE      *ZEROS                                       *SE CI SONO MESSAGGG
202600000000     C                   EXSR      OUTMSG                                       *EMETTE MESSAGGI
202700000000E   1C                   ENDIF
202800000000     C*
202900010920     C                   EXFMT     SE61DV7                                      *PARAMETRI DELTA COM
203000000000     C*
203100000000     C* CONTROLLO FROMATO VIDEO
203200000000     C                   EXSR      DECOD7
203300000000     C*
203400000000     C                   ENDSR
203500000817     C*------------------------------------------------------------------------*
203600000817     C*?FINSR- OPERAZIONI FINALI
203700000817     C*------------------------------------------------------------------------*
203800000817     C     FINSR         BEGSR
203900000817     C*
204000000817     C* CHIUDE I FILE DEI PGM CHIAMATI
204100000817     C                   CLEAR                   BS69DS
204200000817     C                   CLEAR                   ACODS
204300000817     C                   CLEAR                   INDDS
204400000817     C                   CLEAR                   CLPDS
204500000817     C                   CLEAR                   CLSDS
204600000817     C                   MOVEL     'C'           I69TLA
204700000817     C                   MOVEL     KNSIF         I69SIF
204800000817     C                   CALL      'TIBS69R'
204900000817     C                   PARM                    BS69DS
205000000817     C                   PARM                    ACODS
205100000817     C                   PARM                    INDDS
205200000817     C                   PARM                    CLPDS
205300000817     C                   PARM                    CLSDS
205400000817     C*
205500000817     C                   ENDSR
205600020523     C*--------------------------------------------------------------------------------------------*
205700020523     C* CARTAB - CARICA LE TABELLE OCCORRENTI
205800020523     C*--------------------------------------------------------------------------------------------*
205900020523     C     CARTAB        BEGSR
206000020523     C*
206100020523     C* CODICI QUALITA' CLIENTE
206200020523     C                   Z-ADD     *ZEROS        X                 2 0
206300020523     C                   EVAL      KTBKUT = 1
206400020523     C                   EVAL      KTBCOD = 'IC'
206500020523     C     KEYTA2        SETLL     TABEL00F
206600020523     C     KEYTA2        READE     TABEL00F
206700020523     C                   DOW       NOT %EOF(TABEL00F)
206800020523     C                   ADD       1             X
206900020523     C                   MOVEL     TBLKEY        CODQUA(X)
207000020523     C     KEYTA2        READE     TABEL00F
207100020523     C                   ENDDO
207200020523     C*
207300020523     C                   ENDSR
207400000000     C*------------------------------------------------------------------------*
207500000000     C*?*INZSR - ROUTINE INIZIALE
207600000000     C*------------------------------------------------------------------------*
207700000000     C     *INZSR        BEGSR
207800000000     C*
207900000000     C* RICEVIMENTO PARAMETRI
208000000000     C     *ENTRY        PLIST
208100000000     C                   PARM                    KPJBA
208200000000     C                   MOVEL     KPJBU         DS60                           *DS PROC STATISTICHE
208300020523     C*
208400020523     C* ESEGUO SUBITO ALCUNE CONSIDERAZIONI SUI PARAMETRI IN INGRESSO
208500020523     C                   IF        D60OP0 = 'VIS'
208600020523     C                   SETOFF                                       30
208700020523     C                   ELSE
208800020523     C                   SETON                                        30
208900020523     C                   ENDIF
209000000000     C*
209100000000     C* DEFINIZIONI VARIABILI CHIAVI DI LETTURA
209200000000     C     *LIKE         DEFINE    TAMKSC        KTAKSC
209300000000     C     *LIKE         DEFINE    TAMCTR        KTACTR
209400000000     C     *LIKE         DEFINE    TBLKUT        KTBKUT
209500000000     C     *LIKE         DEFINE    TBLCOD        KTBCOD
209600000000     C     *LIKE         DEFINE    TBLKEY        KTBKEY
209700950724     C     *LIKE         DEFINE    ACOKUT        KACKUT
209800950724     C     *LIKE         DEFINE    ACOKCC        KACKCC
209900950724     C     *LIKE         DEFINE    ACOKSC        KACKSC
210000000000     C*
210100000000     C* DEFINIZIONI VARIABILI DI LAVORO
210200000000     C     *LIKE         DEFINE    TAMCTR        WCTR
210300000000     C     *LIKE         DEFINE    TBLKUT        §KUT
210400000000     C     *LIKE         DEFINE    TBLCOD        §COD
210500000000     C     *LIKE         DEFINE    TBLKEY        §KEY
210600000000     C*
210700000000     C* INIZIALIZZO FLAG CONTROLLO PGM
210800000000     C                   MOVEL     '0'           $ERR              1            *ERRORE
210900000000     C                   MOVEL     *BLANKS       $GES              2            *GESTIONE ATTUALE VI
211000000000     C                   MOVEL     *BLANKS       $MSG             80            *MESSAGGI
211100000000     C                   MOVEL     '0'           $FINE             1            *FINE PGM
211200000000     C                   MOVEL     '0'           $TESVD            1            *1=RIEMETT.TEST.VIDE
211300000000     C                   MOVEL     'I'           $TGES             1            *I=INTERROG,G=GESTIO
211400000000     C*
211500000000     C                   MOVEL     '0'           $F03              1            *FINE PROCEDURA
211600000000     C                   MOVEL     '0'           GESPAR            1            *GESTIONE DI PARTENZ
211700000000     C                   MOVEL     '0'           $ERR2             1            *ERRORE DI COMODO
211800000000     C*
211900000000     C* CHIAVE LETTURA TABEL00F - COMPLETA
212000000000     C     KEYTAB        KLIST
212100000000     C                   KFLD                    KTBKUT
212200000000     C                   KFLD                    KTBCOD
212300000000     C                   KFLD                    KTBKEY
212400000000     C*
212500000000     C* CHIAVE LETTURA TABEL00F - PARZIALE
212600000000     C     KEYTA2        KLIST
212700000000     C                   KFLD                    KTBKUT
212800000000     C                   KFLD                    KTBCOD
212900000000     C*
213000000000     C* CHIAVE LETTURA TNTAM01L - PARZIALE
213100000000     C     KEYTAM        KLIST
213200000000     C                   KFLD                    KTAKSC                         *CODICE CLIENTE
213300000000     C                   KFLD                    KTACTR                         *CODICE TARIFFA
213400020523     C*
213500020523     C* CARICA LE TABELLE OCCORRENTI
213600020523     C                   EXSR      CARTAB
213700000000     C*
213800000000     C* ESEGUE OPERAZIONI INIZIALI STANDARD
213900000000     C                   EXSR      INZSTD
214000020916     C*
214100020916     C* REPERISCE I DATI UTENTE
214200020916     C                   EXSR      REPDATIUTE
214300020916     C*
214400020916     C* ASSEGNA LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO VIDEO
214500020916     C                   MOVEL(P)  RSUT          DSFIRS
214600000000     C*
214700000000     C* IMPOSTAZIONE PARAMETRI
214800000000     C                   EXSR      IMPVIX
214900020523     C*
215000020523     C* ESEGUO LA ROUTINE DI INIZIALIZZAZIONE INIZIALE
215100020523     C                   EXSR      INZVID
215200000000     C*
215300000000     C* EMETTO VIDEO PARAMETRI
215400000000     C                   EXSR      CARV0                                        *EMISSIONE VIDEO
215500000000     C*------------------------------------------------------------------------*
215600000000     C* A T T E N Z I O N E:
215700000000     C* PER IL MOMENTO QUASI TUTTE LE VIDEATE DI RICHIESTA PARAMETRI SONO VUOTE:
215800000000     C* CONDIZIONARNE L'EMISSIONE IMPOSTANDO I CORRISPONDENTI FLAG
215900000000     C*------------------------------------------------------------------------*
216000000000     C                   MOVEL     'S'           PARREG            1            *PARAMETRI REGIONE
216100050126     C*** Pesi/Volumi disttivati i parametri di richiesta Dettaglio S/N valori che generano le percentuali
216200050126     C*********          MOVEL     'S'           PARPEV            1            *PARAMETRI PESI/VOLUMI
216300050126     C                   MOVEL     'N'           PARPEV            1            *PARAMETRI PESI/VOLUMI
216400000000     C                   MOVEL     'N'           PARSER            1            *PARAMETRI SERVIZIO
216500000000     C                   MOVEL     'N'           PARAFF            1            *PARAMETRI AFFID.TA'
216600000000     C                   MOVEL     'N'           PARDEL            1            *PARAMETRI DELTA CL
216700000000     C                   MOVEL     'N'           PARAMM            1            *PARAMETRI AMMIN.NE
216800000000     C                   MOVEL     'N'           PARDEF            1            *PARAMETRI DELTA FIL
216900000000     C                   MOVEL     'N'           PARDEC            1            *PARAMETRI DELTA COM
217000000000     C*
217100000000     C                   ENDSR
217200020916     C*--------------------------------------------------------------------------------------------*
217300020916     C* REPERISCE DATI UTENTE
217400020916     C*--------------------------------------------------------------------------------------------*
217500020916     C     REPDATIUTE    BEGSR
217600020916     C*
217700020916     C* REimposta le variabili di lavoro
217800020916     C                   CLEAR                   TIBS34DS
217900020916     C                   CLEAR                   AZUTEDS
218000020916     C                   CLEAR                   DDATIUTE
218100020916     C*
218200020916     C* Reperisce i dati del profilo in ingresso
218300020916     C     *DTAARA       DEFINE    §azute        azuteds
218400020916     C     *DTAARA       DEFINE    §datiute      ddatiute
218500020916     C                   IN(E)     *DTAARA
218600020916if  1C                   IF        %Error                                       *NO errori
218700020916     c                   EVAL      I34Tla = 'L'
218800020916     C                   CALL      'TIBS34R'
218900020916     C                   PARM                    Tibs34Ds
219000020916     C                   IN        *DTAARA
219100020916e   1C                   ENDIF
219200020916     C*
219300020916     C                   ENDSR
219400000000     C*------------------------------------------------------------------------*
219500000000     C*?IMPVIX - IMPOSTAZIONE INIZIALE PARAMETRI VIDEO
219600000000     C*------------------------------------------------------------------------*
219700000000     C     IMPVIX        BEGSR
219800980408     C*
219900010410     C     *LIKE         DEFINE    §5SFSC        WFSC
220000000000     C*--------------------
220100000000     C* VISUALIZZA O STAMPA
220200000000     C*--------------------
220300000817     C                   MOVEL     DSTA(9)       $ING                           *STAMPA
220400000817IF  1C     D60OP0        IFEQ      'VIS'
220500951005     C     D60OP1        ANDEQ     'F06'
220600000000     C                   MOVEL     DSTA(8)       $ING                           *VISUALIZZA
220700000000E   1C                   ENDIF
220800980408     C*--------------------
220900980408     C* FLAG E DATE DI LAVORO
221000980408     C*--------------------
221100020916     C                   MOVEL     1             KTBKUT
221200010410     C                   MOVEL     '5S'          KTBCOD
221300980129     C     KEYTA2        CHAIN     TABEL00F                           99
221400000000IF  1C     *IN99         IFEQ      *ON                                          *TABELLA NON TROVATA
221500980408     C                   MOVEL     'N'           WFSC                           *NON PERMETTO LANCIO
221600020528     C                   Z-ADD     99999999      WDCC
221700000000X   1C                   ELSE
221800010410     C                   MOVEL     TBLUNI        DS5S
221900010410     C                   Z-ADD     §5SDAC        WDAC                           *ULTIMO AGGIORNAMENT
222000010410     C                   MOVEL     §5SFSC        WFSC                           *FLAG AGGIORNAMENTO
222100020528     C                   Z-ADD     §5SDCC        WDCC                           *ULTIMA CANCELLAZION
222200000000E   1C                   ENDIF
222300950210     C*
222400950210     C* DATA ULTIMO AGGIORNAMENTO - 1 MESE
222500950210     C                   SUB       1             AGGMM
222600950210IF  1C     AGGMM         IFEQ      *ZEROS                                       *SE GENNAIO
222700950210     C                   Z-ADD     12            AGGMM                          *IMPOSTO DICEMBRE
222800950210     C                   SUB       1             AGGAA                          *DELL'ANNO PRECEDENT
222900950210E   1C                   ENDIF
223000950210     C*
223100950210     C* DATA ULTIMA CANCELLAZIONE + 1 MESE
223200950210     C                   ADD       1             CANMM
223300950210IF  1C     CANMM         IFEQ      13                                           *SE DICEMBRE
223400950210     C                   Z-ADD     01            CANMM                          *IMPOSTO GENNAIO
223500950210     C                   ADD       1             CANAA                          *DELL'ANNO SUCESSIVO
223600950210E   1C                   ENDIF
223700000000     C*--------------------
223800000000     C* PARAMETRI GENERALI
223900000000     C*--------------------
224000000000     C***
224100000000     C* AA/MM FINALE   --> ULTIMO AGGIORNAMENTO - 1 MESE
224200950210     C* AA/MM INIZIALE --> INIZIO ANNO oppure DATA ULTIMA CANCELLAZIONE + 1 MESE
224300000000     C***
224400950210     C*--------------------
224500950210     C* D A T A   F I N E
224600950210     C*--------------------
224700990601     C                   Z-ADD     AGGAM         V0CDA2
224800950210     C*--------------------
224900950210     C* D A T A   I N I Z I O
225000950210     C*--------------------
225100950210     C* CONFRONTO FRA INIZIO ANNO E ULTIMA CANCELLAZIONE
225200990601     C                   Z-ADD     AGGAM         AAMM1             6 0          *INIZIO ANNO
225300950210     C                   MOVE      01            AAMM1
225400990601     C                   Z-ADD     CANAM         AAMM2             6 0          *ULTIMA CANCELLAZION
225500000000IF  1C     AAMM2         IFGT      AAMM1
225600990601     C                   Z-ADD     CANAM         V0CDAT
225700950210     C                   ELSE
225800990601     C                   Z-ADD     AGGAM         V0CDAT
225900950210     C                   MOVE      01            V0CDAT
226000000000E   1C                   ENDIF
226100000000     C***
226200000000     C* STATISTICHE
226300000000     C***
226400950724     C*--------------------
226500950724     C* "S T A"
226600950724     C*--------------------
226700960604     C* STAMPA DA STATISTICA -> NON PERMETTE LA RICHIESTA DELLE <> STATISTICHE
226800000000IF  1C     D60OP0        IFEQ      'STA'                                        *S T A M P A  TUTTI
226900960604     C                   SETON                                        02        *NON VISUALIZZA RICH
227000020523     C     D60OP1        IFEQ      'STT'                                        *S T A M P A  TUTTI
227100020523     C                   SETOFF                                       02        *NON VISUALIZZA RICH
227200020523     C                   ENDIF
227300000000IF  2C     D60OP1        IFEQ      'REG'
227400020523     C     D60OP1        OREQ      'STT'
227500000000     C                   MOVEL     'S'           V0CST1                         *REGIONE-REGIONE
227600000000     C                   MOVEL     DSTA(1)       V0CRAG                         *DESCRIZ STATISTICA
227700000000X   2C                   ELSE
227800000000     C                   MOVEL     'N'           V0CST1
227900000000E   2C                   ENDIF
228000050110     C*
228100050110IF  2C     D60OP1        IFEQ      'PEV'
228200050110     C     D60OP1        OREQ      'STT'
228300050110     C                   MOVEL     'S'           V0CSTp                         *PESI-VOLUMI
228400050110     C                   MOVEL     DSTA(14)      V0CRAG                         *DESCRIZ STATISTICA
228500050110X   2C                   ELSE
228600050110     C                   MOVEL     'N'           V0CSTp
228700050110E   2C                   ENDIF
228800000000     C*
228900000000IF  2C     D60OP1        IFEQ      'SER'
229000020523     C     D60OP1        OREQ      'STT'
229100000000     C                   MOVEL     'S'           V0CST2                         *SERVIZIO BOLLE
229200000000     C                   MOVEL     DSTA(2)       V0CRAG                         *DESCRIZ STATISTICA
229300000000X   2C                   ELSE
229400000000     C                   MOVEL     'N'           V0CST2
229500000000E   2C                   ENDIF
229600000000     C*
229700000000IF  2C     D60OP1        IFEQ      'AFF'
229800020523     C     D60OP1        OREQ      'STT'
229900000000     C                   MOVEL     'S'           V0CST3                         *AFFIDABILITA'
230000000000     C                   MOVEL     DSTA(3)       V0CRAG                         *DESCRIZ STATISTICA
230100000000X   2C                   ELSE
230200000000     C                   MOVEL     'N'           V0CST3
230300000000E   2C                   ENDIF
230400000000     C*
230500000000IF  2C     D60OP1        IFEQ      'DEL'
230600020523     C     D60OP1        OREQ      'STT'
230700000000     C                   MOVEL     'S'           V0CST4                         *DELTA CLIENTE
230800000000     C                   MOVEL     DSTA(4)       V0CRAG                         *DESCRIZ STATISTICA
230900000000X   2C                   ELSE
231000000000     C                   MOVEL     'N'           V0CST4
231100000000E   2C                   ENDIF
231200000000     C*
231300000000IF  2C     D60OP1        IFEQ      'AMM'
231400020523     C     D60OP1        OREQ      'STT'
231500000000     C                   MOVEL     'S'           V0CST5                         *AMMINISTRAZIONE
231600000000     C                   MOVEL     DSTA(5)       V0CRAG                         *DESCRIZ STATISTICA
231700000000X   2C                   ELSE
231800000000     C                   MOVEL     'N'           V0CST5
231900000000E   2C                   ENDIF
232000000000     C*
232100000000IF  2C     D60OP1        IFEQ      'DEF'
232200020523     C     D60OP1        OREQ      'STT'
232300000000     C                   MOVEL     'S'           V0CST6                         *DELTA FILIALE
232400000000     C                   MOVEL     DSTA(6)       V0CRAG                         *DESCRIZ STATISTICA
232500000000X   2C                   ELSE
232600000000     C                   MOVEL     'N'           V0CST6
232700000000E   2C                   ENDIF
232800000000     C*
232900000000IF  2C     D60OP1        IFEQ      'DEC'
233000020523     C     D60OP1        OREQ      'STT'
233100000000     C                   MOVEL     'S'           V0CST7                         *DELTA COMMERCIALE
233200000000     C                   MOVEL     DSTA(7)       V0CRAG                         *DESCRIZ STATISTICA
233300000000X   2C                   ELSE
233400000000     C                   MOVEL     'N'           V0CST7
233500000000E   2C                   ENDIF
233600950724     C*--------------------
233700000817     C* "V I S"
233800950724     C*--------------------
233900960604     C* VISUALIZZAZIONE DA CLIENTE -> VISUALIZZA E CHIEDE LE DIVERSE STATISTICHE
234000000000X   1C                   ELSE
234100000817IF  2C     D60OP1        IFEQ      'F06'
234200000000     C                   MOVEL     'S'           V0CST1                         *REGIONE-REGIONE
234300050110     C                   MOVEL     'S'           V0CSTp                         *pesi/volumi
234400000000     C                   MOVEL     'S'           V0CST2                         *SERVIZIO BOLLE
234500000000     C                   MOVEL     'S'           V0CST3                         *AFFID.TA' SPEDIZION
234600000000     C                   MOVEL     'S'           V0CST4                         *DELTA CLIENTE
234700960604     C                   MOVEL     'S'           V0CST5                         *AMMINISTRAZIONE
234800000000     C                   MOVEL     'N'           V0CST6                         *DELTA
234900000000     C                   MOVEL     'N'           V0CST7                         *DELTA COMMERCIALE
235000000817E   2C                   ENDIF
235100000817     C* STAMPA DA CLIENTE -> VISUALIZZA E CHIEDE LE DIVERSE STATISTICHE
235200000817IF  2C     D60OP1        IFEQ      'F21'
235300000817     C*---               SETON                     02    *NON VISUL.STAT.
235400000817     C                   MOVEL     'S'           V0CST1                         *REGIONE-REGIONE
235500050110     C                   MOVEL     'S'           V0CSTp                         *pesi/volumi
235600000817     C                   MOVEL     'S'           V0CST2                         *SERVIZIO BOLLE
235700000817     C                   MOVEL     'S'           V0CST3                         *AFFID.TA' SPEDIZION
235800000817     C                   MOVEL     'N'           V0CST4                         *NO DELTA CLIENTE
235900000817     C                   MOVEL     'N'           V0CST5                         *NO AMMINISTRAZIONE
236000000817     C                   MOVEL     'N'           V0CST6                         *DELTA
236100000817     C                   MOVEL     'N'           V0CST7                         *DELTA COMMERCIALE
236200000817E   2C                   ENDIF
236300000817     C* IMPOSTA TESTATA
236400950724IF  2C     D60OP0        IFEQ      'VIS'
236500000817     C                   Z-ADD     *ZEROS        V0CKSC
236600000817IF  3C     D60SCE        IFEQ      '1'
236700000817     C     D60SCE        OREQ      '3'
236800950724     C                   MOVEA     D60KSC        KSA                            *DS INPUT -> SCH ALF
236900950724     C                   MOVE      KSA(1)        DSKSA                          *SCH ALF -> DS ALF
237000950724     C                   Z-ADD     DSKSN         KSC(1)                         *DS NUM -> SCH NUM
237100950724     C                   Z-ADD     KSC(1)        V0CKSC                         *CODICE CLIENTE
237200000817     C                   CLEAR                   BS69DS
237300000817     C                   CLEAR                   ACODS
237400000817     C                   MOVEL     KNSIF         I69SIF
237500020916     C                   Z-ADD     DUTKCI        I69KCC
237600000817     C                   MOVEL     V0CKSC        I69KAC
237700000817     C                   CALL      'TIBS69R'
237800000817     C                   PARM                    BS69DS
237900000817     C                   PARM                    ACODS
238000000817     C                   PARM                    INDDS
238100000817     C                   PARM                    CLPDS
238200000817     C                   PARM                    CLSDS
238300000817E   3C                   ENDIF
238400000817IF  3C     D60SCE        IFEQ      '1'
238500000817     C                   MOVEL     ACORAG        V0CRAG
238600000817E   3C                   ENDIF
238700000817IF  3C     D60SCE        IFEQ      '2'
238800000817     C                   MOVEL     DSTA(10)      V0CRAG
238900000817E   3C                   ENDIF
239000000817IF  3C     D60SCE        IFEQ      '3'
239100000817     C                   MOVEL     DSTA(11)      V0CRAG
239200000817E   3C                   ENDIF
239300020523IF  3C     D60SCE        IFEQ      '4'
239400020523     C                   MOVEL     DSTA(12)      V0CRAG
239500020523E   3C                   ENDIF
239600020523IF  3C     D60SCE        IFEQ      '5'
239700020523     C                   MOVEL     DSTA(13)      V0CRAG
239800020523E   3C                   ENDIF
239900000817E   2C                   ENDIF
240000000000E   1C                   ENDIF
240100000000     C*--------------------
240200000000     C* PARAMETRI REGIONE-REGIONE
240300000000     C*--------------------
240400950206     C                   MOVEL     *BLANKS       V1CRIF                         *TUTTI I VALORI
240500971128     C*---                 MOVEL'E'       V1CREG           *REGIONE
240600050119     C*--------------------
240700050119     C* PARAMETRI PESI VOLUMI
240800050119     C*--------------------
240900050119     C                   MOVEL     'N'           VPVDET                         *TUTTI I VALORI
241000000000     C*--------------------
241100000000     C* PARAMETRI SERVIZIO BOLLE
241200000000     C*--------------------
241300000000     C*--------------------
241400000000     C* PARAMETRI AFFIDABILITA'
241500000000     C*--------------------
241600000000     C*--------------------
241700000000     C* PARAMETRI DELTA CLIENTE
241800000000     C*--------------------
241900000000     C*--------------------
242000000000     C* PARAMETRI DELTA COMMERCIALE
242100000000     C*--------------------
242200000000     C*
242300000000     C                   ENDSR
242400000000     C*------------------------------------------------------------------------*
242500000000     C*?   §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
242600000000     C*?   §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
242700000000     C*     LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
242800000000     C*     STANDARDS  DI  PROGRAMMAZIONE  E NELLA MAGGIOR PARTE DEI CASI
242900000000     C*     NON  E'  NECESSARIO  MODIFICARLA, NEL CASO  FARE  ATTENZIONE.
243000000000     C*?   §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
243100000000     C*?   §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
243200000000     C*------------------------------------------------------------------------*
243300000000     C* INZSTD - ROUTINE INIZIALE STANDARD
243400000000     C*------------------------------------------------------------------------*
243500000000     C     INZSTD        BEGSR
243600000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
243700000000     C                   CALL      'XKEYS'
243800000000     C                   PARM                    DSKEYS
243900000000     C                   MOVEA     KEYRIG        KEYS                           *CARICA SCHIERA FUNZ
244000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
244100000000     C     XMSGPA        PLIST
244200000000     C                   PARM                    DSMSMS
244300000000     C                   PARM                    DSMSPG
244400000000     C                   PARM                    DSMSKE
244500000000     C                   PARM                    DSMSER
244600000000     C* IMPOSTA PER CL=XMSG (MESSAGGI ALLA CODA PGM), FISSI IN TUTTO IL PGM
244700010920     C                   MOVEL     'TISE61R'     DSMSPG                         *NOME PGM CHIAMANTE
244800010920     C                   MOVEL     'TISE61R'     VMCQUE                         *CODA MESSAGGI DESTI
244900000000     C                   MOVEL     *BLANKS       VMCKEY                         *CHIAVE MESSAGGIO
245000000000     C                   MOVEL     *BLANKS       DSMSMS                         *MESSAGGIO
245100000000     C* POSIZIONE MINIMA E MASSIMA DEL CURSORE PER GESTIONE MESSAGGI
245200000000     C                   BITOFF    '01234567'    POSMI1            1
245300000000     C                   BITOFF    '01234567'    POSMI2            1
245400000000     C                   BITOFF    '01234567'    POSMA1            1
245500000000     C                   BITOFF    '01234567'    POSMA2            1
245600000000     C                   BITON     '34'          POSMI1            1            *18
245700000000     C                   BITON     '7'           POSMI2            1            *01
245800000000     C                   BITON     '34'          POSMA1            1            *18
245900000000     C                   BITON     '0'           POSMA2            1            *80
246000000000     C                   MOVE      POSMI2        POSMIN            2
246100000000     C                   MOVEL     POSMI1        POSMIN
246200000000     C                   MOVE      POSMA2        POSMAX            2
246300000000     C                   MOVEL     POSMA1        POSMAX
246400000000     C                   ENDSR
246500000000     C*------------------------------------------------------------------------*
246600000000     C* FUNSTD - LANCIA SUBROUTINE STANDARD
246700000000     C*------------------------------------------------------------------------*
246800000000     C     FUNSTD        BEGSR
246900000000     C                   SELECT
247000000000     C     KEY           WHENEQ    F3
247100000000     C                   EXSR      F03STD
247200000000     C     KEY           WHENEQ    F12
247300000000     C                   EXSR      F12STD
247400000000     C     KEY           WHENEQ    F24
247500000000     C                   EXSR      F24STD
247600000000     C                   OTHER
247700000000     C                   MOVEL     MSG(2)        $MSG
247800000000     C                   EXSR      CARMSG
247900000000     C                   ENDSL
248000000000     C                   ENDSR
248100000000     C*------------------------------------------------------------------------*
248200000000     C* F03STD -GEST.TASTO FUNZ. F03 STANDARD ===> FINE
248300000000     C*------------------------------------------------------------------------*
248400000000     C     F03STD        BEGSR
248500000000     C* IMPOSTO FLAG DI FINE PROCEDURA
248600000000     C                   MOVEL     '1'           $F03
248700000000     C* IMPOSTO FLAG DI FINE PROGRAMMA
248800000000     C                   MOVEL     '1'           $FINE
248900000000     C                   ENDSR
249000000000     C*------------------------------------------------------------------------*
249100000000     C* F12STD -GEST.TASTO FUNZ. F12 STANDARD ===> RITORNO
249200000000     C*------------------------------------------------------------------------*
249300000000     C     F12STD        BEGSR
249400000000     C* IMPOSTO FLAG DI FINE PGM
249500000000     C                   MOVEL     '1'           $FINE
249600000000     C                   ENDSR
249700000000     C*------------------------------------------------------------------------*
249800000000     C* F24STD -GEST.TASTO FUNZ. F24 STANDARD ===> ALTRI TASTI
249900000000     C*------------------------------------------------------------------------*
250000000000     C     F24STD        BEGSR
250100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
250200000000     C                   EXSR      OUTFUN
250300000000     C                   ENDSR
250400000000     C*------------------------------------------------------------------------*
250500000000     C* CARMSG - CARICA MESSAGGIO NEL SUBFILE MESSAGGI E LO INVIA ALLA
250600000000     C*          CODA MESSAGGI DEL PGM
250700000000     C*------------------------------------------------------------------------*
250800000000     C     CARMSG        BEGSR
250900000000     C* SE QUELLO CHE STO PER CARICARE E' IL PRIMO MESSAGGIO
251000000000     C* PULISCO SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
251100000000     C     MSGULT        IFEQ      0
251200000000     C                   MOVEL     '1'           *IN40
251300010920     C                   WRITE     SE61DVK
251400000000     C                   MOVEL     '0'           *IN40
251500000000     C                   ENDIF
251600000000     C* INVIA MESSAGGIO ALLA CODA DEL PGM
251700000000     C                   MOVEL     $MSG          DSMSMS
251800000000     C                   CALL      'XMSG'        XMSGPA
251900000000     C     DSMSER        IFEQ      0
252000000000     C                   MOVEL     DSMSKE        VMCKEY                         *CHIAVE DEL MESSAGGI
252100000000     C                   ENDIF
252200000000     C* CARICA IL MESSAGGIO NEL SUBFILE MESSAGGI
252300000000     C                   ADD       1             MSGULT            4 0          *CHIAVE SFL MESS
252400010920     C                   WRITE     SE61DVM                                      *SFL MESS
252500000000     C* PULISCO CAMPO MESSAGGIO APPENA CARICATO
252600000000     C                   MOVEL     *BLANKS       $MSG
252700000000     C                   ENDSR
252800000000     C*------------------------------------------------------------------------*
252900000000     C* OUTMSG - EMETTE A VIDEO IL SUBFILE MESSAGGI
253000000000     C*------------------------------------------------------------------------*
253100000000     C     OUTMSG        BEGSR
253200000000     C* VISUALIZZAZIONE MESSAGGIO
253300000000     C                   MOVEL     '1'           *IN44                          *SFLEND (+)
253400010920     C                   WRITE     SE61DVK                                      *CONTR MESS
253500000000     C* AZZERO LA CHIAVE DEL SUBFILE MESSAGGI
253600000000     C                   Z-ADD     *ZEROS        MSGULT
253700000000     C                   ENDSR
253800000000     C*------------------------------------------------------------------------*
253900000000     C* CARFUN - CARICA LE SCHIERE PER LA GESTIONE DEI TASTI FUNZIONALI
254000000000     C*          INPUT:  - FUN    (SCHIERA TASTI ABILITATI PER IL VIDEO ATTUALE)
254100000000     C*          OUTPUT: - FFU    (SCHIERA FUNZIONI)
254200000000     C*                  - FDE    (SCHIERA DESCRIZIONI FUNZIONI)
254300000000     C*                  - FAT    (SCHIERA FLAG DI FUNZIONE ATTIVA S/N)
254400000000     C*------------------------------------------------------------------------*
254500000000     C     CARFUN        BEGSR
254600000000     C                   Z-ADD     *ZEROS        FUNULT            5 0          *ULTIMA FUNZ. VISUAL
254700000000     C     1             DO        31            I                 5 0          --- 1 -->
254800000000     C                   MOVEL     FUN(I)        FUNRIG                         *DS RIGA FUNZIONI
254900000000     C                   MOVEL     FUNFUN        FFU(I)                         *FUNZIONE
255000000000     C                   MOVEL     FUNDES        FDE(I)                         *DESCRIZIONE
255100020916     C     DUTLPO        IFEQ      'S'                                          --- 2 --> *SEDE
255200000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
255300000000     C                   MOVEL     FUNASG        FAT(I)                         *FUNZIONE ATTIVA S/N
255400000000     C                   ELSE                                                   *INTERROG.
255500000000     C                   MOVEL     FUNASI        FAT(I)
255600000000     C                   ENDIF
255700000000     C                   ELSE                                                   <-- 2 --> *FILIALE
255800000000     C     $TGES         IFEQ      'G'                                          *GESTIONE
255900000000     C                   MOVEL     FUNAFG        FAT(I)
256000000000     C                   ELSE                                                   *INTERROG.
256100000000     C                   MOVEL     FUNAFI        FAT(I)
256200000000     C                   ENDIF
256300000000     C                   ENDIF                                                  <-- 2 ---
256400000000     C     FAT(I)        IFEQ      'E'                                          *FLAG 'E'->SOLO EDP
256500000000     C                   MOVEL     USER          WEDP              3            *PROFILO
256600000000     C     WEDP          IFEQ      'EDP'                                        *SE PROFILO EDPXX
256700000000     C                   MOVEL     'S'           FAT(I)                         *ABILITO
256800000000     C                   ELSE
256900000000     C                   MOVEL     'N'           FAT(I)                         *NON ABILITO
257000000000     C                   ENDIF
257100000000     C                   ENDIF
257200000000     C                   ENDDO                                                  <-- 1 ---
257300000000     C                   ENDSR
257400000000     C*------------------------------------------------------------------------*
257500000000     C* OUTFUN - EMETTE A VIDEO LE FUNZIONI ABILITATE
257600000000     C*          INPUT:  - FUNULT (ULTIMA FUNZIONE EMESSA NEL VIDEO PRECEDENTE)
257700000000     C*          OUTPUT: - VFD001-008 (CAMPI FUNZIONI A VIDEO)
257800000000     C*                  - FUNULT (ULTIMA FUNZIONE EMESSA A VIDEO)
257900000000     C*------------------------------------------------------------------------*
258000000000     C     OUTFUN        BEGSR
258100000000     C     1             DO        8             I                              *PULIZIA FUNZIONI A
258200000000     C                   MOVEL     *BLANKS       FUV(I)                         *VIDEO
258300000000     C                   ENDDO
258400000000     C* CONTROLLO QUANTE FUNZIONI ATTIVE DA EMETTERE A VIDEO CI SONO
258500000000     C                   Z-ADD     *ZEROS        FUNNAT            2 0          *N.FUNZIONI ATTIVE
258600000000     C     1             DO        31            I
258700000000     C     FAT(I)        IFEQ      'S'                                          *FUNZIONE ATTIVA
258800000000     C     FDE(I)        ANDNE     *BLANKS                                      *DESCRIZIONE FUNZION
258900000000     C                   ADD       1             FUNNAT
259000000000     C                   ENDIF
259100000000     C                   ENDDO
259200000000     C* SE VI SONO PIU' DI 8 FUNZIONI ATTIVO IL TASTO F24
259300000000     C     FUNNAT        IFGT      8
259400000000     C                   MOVEL     'S'           FAT(24)                        *FLAG ATTIVITA' FUNZ
259500000000     C                   ENDIF
259600000000     C* IMPOSTO IL N.DI FUNZIONI VISUALIZZABILI A 8, E SE SONO ATTIVE F23 E F24
259700000000     C* (SE ATTIVE DA VISUALIZZARE SEMPRE) DIVENTANO 6
259800000000     C                   Z-ADD     8             FUNVIS            5 0
259900000000     C     FAT(23)       IFEQ      'S'
260000000000     C                   SUB       1             FUNVIS
260100000000     C                   ENDIF
260200000000     C     FAT(24)       IFEQ      'S'
260300000000     C                   SUB       1             FUNVIS
260400000000     C                   ENDIF
260500000000     C* AGGIUNGO UNO ALL'ULTIMA FUNZIONE VISUALIZZATA PER AVERE LA 1A DA VISUALI
260600000000     C                   Z-ADD     *ZEROS        W                 5 0
260700000000     C                   ADD       1             FUNULT
260800000000     C     FUNULT        IFGT      31
260900000000     C                   Z-ADD     1             FUNULT
261000000000     C                   ENDIF
261100000000     C* RICERCO LE OPZIONI ATTIVE E LE EMETTO A VIDEO
261200000000     C     FUNULT        DO        31            I                              --- 1 -->
261300000000     C* SE HO GIA' RAGGIUNTO IL NUMERO DI FUNZIONI VISUALIZZABILI VADO A FINE
261400000000     C     W             IFLT      FUNVIS                                       --- 2 -->
261500000000     C* SE OPZIONE ATTIVA ED ESISTE LA DESCRIZIONE EMETTO A VIDEO
261600000000     C     FAT(I)        IFEQ      'S'                                          --- 3 -->
261700000000     C     FDE(I)        ANDNE     *BLANKS
261800000000     C     I             IFEQ      23                                           *TOLGO F23
261900000000     C     I             OREQ      24                                           *E F24
262000000000     C                   ELSE
262100000000     C                   ADD       1             W
262200000000     C                   MOVEL     FDE(I)        FUV(W)
262300000000     C                   ENDIF
262400000000     C                   ENDIF                                                  <-- 3 ---
262500000000     C* MEMORIZZO L'ULTIMO INDICE TRATTATO
262600000000     C                   Z-ADD     I             FUNULT
262700000000     C                   ENDIF                                                  <-- 2 ---
262800000000     C                   ENDDO                                                  <-- 1 ---
262900000000     C* TRASFERISCO LE OPZIONI DALLA SCHIERA AI CAMPI VIDEO
263000000000     C                   MOVEL     FUV(1)        VFD001
263100000000     C                   MOVEL     FUV(2)        VFD002
263200000000     C                   MOVEL     FUV(3)        VFD003
263300000000     C                   MOVEL     FUV(4)        VFD004
263400000000     C                   MOVEL     FUV(5)        VFD005
263500000000     C                   MOVEL     FUV(6)        VFD006
263600000000     C     FAT(23)       IFEQ      'N'                                          --- 1 -->
263700000000     C     FAT(24)       ANDEQ     'N'
263800000000     C                   MOVEL     FUV(7)        VFD007
263900000000     C                   MOVEL     FUV(8)        VFD008
264000000000     C                   ELSE                                                   <-- 1 -->
264100000000     C     FAT(23)       IFEQ      'S'                                          --- 2 -->
264200000000     C     FAT(24)       ANDEQ     'S'
264300000000     C                   MOVEL     FDE(23)       VFD007
264400000000     C                   MOVEL     FDE(24)       VFD008
264500000000     C                   ELSE                                                   <-- 2 -->
264600000000     C                   MOVEL     FUV(7)        VFD007
264700000000     C     FAT(23)       IFEQ      'S'
264800000000     C                   MOVEL     FDE(23)       VFD008
264900000000     C                   ELSE
265000000000     C                   MOVEL     FDE(24)       VFD008
265100000000     C                   ENDIF
265200000000     C                   ENDIF                                                  <-- 2 ---
265300000000     C                   ENDIF                                                  <-- 1 ---
265400000000     C                   MOVEL     '1'           $TESVD                         *RIEMETTE TESTATA VI
265500000000     C                   ENDSR
265600000000     C*------------------------------------------------------------------------*
265700000000     C*  CONFUN - CONTROLLO SE LA FUNZIONE SCELTA E' ATTIVA
265800000000     C*           INPUT:  - KEY    (FUNZIONE PREMUTA - DA INFDS)
265900000000     C*           OUTPUT: - FUNOK  (OPZIONE OK S/N)
266000000000     C*                   - ERRORE SE FUNZIONE NON ATTIVA
266100000000     C*------------------------------------------------------------------------*
266200000000     C     CONFUN        BEGSR
266300000000     C* RICERCA FUNZIONE SCELTA IN TABELLA FUNZIONI
266400000000     C     1             DO        31            I                              --- 1 -->
266500000000     C     KEY           IFEQ      KEYS(I)                                      --- 2 -->
266600000000     C* CONTROLLO SE IL TASTO E' ATTIVO
266700000000     C     FAT(I)        IFNE      'S'                                          --- 3 -->
266800000000     C                   MOVEL     MSG(1)        $MSG                           *FUNZ.NON ACCETTABIL
266900000000     C                   EXSR      CARMSG
267000000000     C                   MOVEL     '1'           $ERR
267100000000     C                   ENDIF                                                  <-- 3 ---
267200000000     C                   ENDIF                                                  <-- 2 ---
267300000000     C                   ENDDO                                                  <-- 1 ---
267400000000     C                   ENDSR
267500000000     O*------------------------------------------------------------------------*
267600000000     O* SCHIERE
267700000000     O*------------------------------------------------------------------------*
267800000000**   DSTA - DESCRIZIONE STATISTICA SELEZIONATA
267900000000STATISTICA REGIONE - REGIONE                                                  1
268000000000STATISTICA TOTALI                                                             2
268100000000STATISTICA AFFIDABILITA'                                                      3
268200000000STATISTICA DELTA CLIENTE                                                      4
268300000000STATISTICA AMMINISTRAZIONE                                                    5
268400020823TUTTI I CLIENTI                                                               6
268500020823TUTTI I CLIENTI                                                               7
268600950724* VISUALIZZA *                                                                8
268700950724*** STAMPA ***                                                                9
268800950724STAT. PIU' CLIENTI INSIEME                                                    10
268900000817STAT. CLIENTE UNIFICANTE                                                      11
269000020523STAT. TUTTI CLIENTI                                                           12
269100020523STAT. TUTTI CLIENTI UNIF.                                                     13
269200050110STATISTICA PESI/VOLUMI                                                        14
26930000081715
269400000000**   MSG - MESSAGGI
269500010920TISE61R- Tasto funzionale non valido.                                          1
269600010920TISE61R- Tasto funzionale abilitato ma non gestito. Avvertire CED.             2
269700010920TISE61R- Opzione non valida                                                    3
269800010920TISE61R- Opzione abilitata ma non gestita. Avvertire CED                       4
269900010920TISE61R- Inizio elenco                                                         5
270000010920TISE61R- Fine elenco                                                           6
270100010920TISE61R- Anno/Mese obbligatorio                                                7
270200010920TISE61R- Anno/Mese errato                                                      8
270300010920TISE61R- Data superiore all'ultimo agg.to: lanciare l'aggior.to da contabilità 9
270400010920TISE61R- Anno/Mese finale inferiore ad Anno/Mese iniziale                     10
270500010920TISE61R- Richiedere non più di dodici mesi                                    11
270600010920TISE61R- Codice tariffa errato o inesistente                                  12
270700010920TISE61R- Fascia di peso inesistente                                           13
270800010920TISE61R- Tipo servizio non accettabile                                        14
270900010920TISE61R- Tipo porto non accettabile                                           15
271000010920TISE61R- Tipo prodotto non accettabile                                        16
271100010920TISE61R- Inserire 'S' per richiedere la statistica, 'N' per non richiederla   17
271200010920TISE61R- Inserire il tipo di dato che si vuole elaborare in statistica        18
271300010920TISE61R- 'P' per avere i dati del cliente in partenza, 'A' in arrivo          19
271400010920TISE61R- Saldi non presenti: data inferiore all' ultima cancellazione         20
271500010920TISE61R- Saldi cliente non aggiornati: impossibile lanciare questa statistica 21
271600010920TISE61R- Selezionare almeno una statistica                                    22
271700010920TISE61R- Impossibile selezionare codici tariffa per clienti diversi           23
271800020325TISE61R- Impossibile lanciare la statistica su anni differenti                24
271900010920TISE61R- Stampa lanciata ...                                                  25
272000020523TISE61R- Codice qualità cliente inesistente                                   26
272100010920TISE61R- 27
272200010920TISE61R- 28
272300010920TISE61R- 29
272400010920TISE61R- 30
272500000000**   FUN1 - PARAMETRI     SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
272600000000F1    -                   -    NN     NN
272700000000F2    -                   -    NN     NN
272800000000F3    -F3=Fine            -    SS     SS
272900000000F4    -                   -    NN     NN
273000000000F5    -                   -    NN     NN
273100000000F6    -F6=Lancio          -    SS     SS
273200000000F7    -                   -    NN     NN
273300000000F8    -                   -    NN     NN
273400000000F9    -                   -    NN     NN
273500951005F10   -F10=Altri parametri-    SS     SS
273600000000F11   -                   -    NN     NN
273700000000F12   -F12=Ritorno        -    SS     SS
273800000000F13   -                   -    NN     NN
273900000000F14   -                   -    NN     NN
274000000000F15   -                   -    NN     NN
274100000000F16   -                   -    NN     NN
274200000000F17   -                   -    NN     NN
274300000000F18   -                   -    NN     NN
274400000000F19   -                   -    NN     NN
274500000000F20   -                   -    NN     NN
274600000000F21   -                   -    NN     NN
274700000000F22   -                   -    NN     NN
274800000000F23   -                   -    NN     NN
274900000000F24   -F24=Altri tasti    -    NN     NN
275000000000ENTER -                   -    SS     SS
275100000000ROLLUP-                   -    NN     NN
275200000000ROLLDN-                   -    NN     NN
275300000000HELP  -                   -    NN     NN
275400000000PRINT -                   -    NN     NN
275500000000BSPACE-                   -    NN     NN
275600000000CLEAR -                   -    NN     NN
275700000000**   FUN2 - STATISTICHE   SEDE:GI FIL:GI   (G=GESTIONE,I=INTERROG.)
275800000000F1    -                   -    NN     NN
275900000000F2    -                   -    NN     NN
276000000000F3    -F3=Fine            -    SS     SS
276100000000F4    -                   -    NN     NN
276200000000F5    -                   -    NN     NN
276300000000F6    -                   -    NN     NN
276400000000F7    -                   -    NN     NN
276500000000F8    -                   -    NN     NN
276600000000F9    -                   -    NN     NN
276700000000F10   -                   -    NN     NN
276800000000F11   -                   -    NN     NN
276900000000F12   -F12=Ritorno        -    SS     SS
277000000000F13   -                   -    NN     NN
277100000000F14   -                   -    NN     NN
277200000000F15   -                   -    NN     NN
277300000000F16   -                   -    NN     NN
277400000000F17   -                   -    NN     NN
277500000000F18   -                   -    NN     NN
277600000000F19   -                   -    NN     NN
277700000000F20   -                   -    NN     NN
277800000000F21   -                   -    NN     NN
277900000000F22   -                   -    NN     NN
278000000000F23   -                   -    NN     NN
278100000000F24   -                   -    NN     NN
278200000000ENTER -                   -    SS     SS
278300000000ROLLUP-                   -    NN     NN
278400000000ROLLDN-                   -    NN     NN
278500000000HELP  -                   -    NN     NN
278600000000PRINT -                   -    NN     NN
278700000000BSPACE-                   -    NN     NN
278800000000CLEAR -                   -    NN     NN
