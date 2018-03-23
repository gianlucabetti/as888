000100970526     H*------------------------------------------------------------------------*
000200021218     H* "DRIVER" MOVIMENTI GABBIE E BANCALI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800021218     FAZMGB00F  O  A E             DISK
000900021218     FTNTBE01L  IF   E           K DISK
001000021218     FCNACO00F  IF   E           K DISK
001100021218     FAZDST04L  IF   E           K DISK
001200021218     FAZORG01L  IF   E           K DISK
001300021218     FAZAGB01L  IF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* INPUT
001600970526     D*------------------------------------------------------------------------*
001700021218     D* DS "XSRDA8" - CONTROLLA DATA (8)
001800021218     D*------------
001900021218     D WLBDA8          DS                  INZ
002000021218     D  G08DAT                 1      8  0
002100021218     D  G08INV                 9     16  0
002200021218     D  G08ERR                17     17
002300021218     D  G08TGI                18     22  0
002400021218     D*------------
002500021218     D* DS DI MEMORIZZAZIONE MOVIMENTI DA INSERIRE
002600021218     D*------------
002700021219     D AZMGBXXX      E DS                  EXTNAME(AZMGB00F)
002800021218     D AZMGB001      E DS                  EXTNAME(AZMGB00F) PREFIX(W1_)
002900021218     D AZMGB002      E DS                  EXTNAME(AZMGB00F) PREFIX(W2_)
003000021218     D*------------
003100020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
003200021218     D*------------
003300021218     D TISIG0DS      E DS
003400021218     D*------------
003500021218     D* DS CHIAMAMTA *PGM REPERIMENTO DATI ALTRO UTENTE (TIBS36R)
003600021218     D*------------
003700021218     D TIBS36DS      E DS
003800021218     D*------------
003900021218     D* DS RIDEFINIZIONE CAMPO DATI UTENTE DELLA DS TIBS36DS RELATIVO ALLA STRUTTIRA DI AZUTE00F
004000021218     D*------------
004100021218     D AZUTE00F      E DS
004200021218     D*------------
004300021218     D* DS RIDEFINIZIONE CAMPO AUTORIZZAZIONI SPECIALI (UTEFAF) DI AZUTE00F
004400021218     D*------------
004500021218     D DUTE01        E DS
004600021218     D*------------
004700021218     D* DS X LETTURA NUOVA TABELLA GGB
004800021218     D*------------
004900021218     D DGGB          E DS
005000030122     D*------------------
005100030122     D* DS X REPERIMENTO NUMERATORE
005200030122     D*------------------
005300030122     D TRUL33DS      E DS                  INZ
005400030122     D*------------------
005500030122     D* ARCHITETTURA
005600030122     D*------------------
005700030122     D KPJBA         E DS
005800030122     D*------------------
005900021218     D* SCHIERE
006000030122     D*------------------
006100021218     D KCAU            S              4    DIM(100)                             *CHIAVE CAUSALE
006200021218     D DCAU            S                   LIKE(DGGB) DIM(100)                  *DATI CAUSALE
006300020415     D*------------------
006400020415     D* VARIABILI DI WRK
006500020415     D*------------------
006600021218     D X               S              3  0 INZ(*ZEROS)
006700021218     D ERRL00          S              1    INZ('0')
006800030122     D ERRNUM          S              1    INZ('0')
006900021218     D ERR_1GIRO       S              1    INZ('0')
007000021218     D ERR_2GIRO       S              1    INZ('0')
007100021218     D W_CAU           S                   LIKE(IG0CAU)
007200021218     D W_SOG           S                   LIKE(IG0SOG)
007300970526     C*------------------------------------------------------------------------*
007400970526     C* MAIN LINES
007500970526     C*------------------------------------------------------------------------*
007600970526     C*
007700021218     C                   IF        IG0TLA <> 'C'
007800020415     C                   EXSR      PROCEDI
007900021218     C                   EXSR      WRIMOV
008000020415     C                   ENDIF
008100020415     C*
008200970526     C* FINE PGM
008300021218     C                   IF        IG0TLA = 'E'
008400020415     C                   SETON                                        RT
008500020415     C                   ELSE
008600020415     C                   SETON                                        LR
008700020415     C                   ENDIF
008800970526     C*------------------------------------------------------------------------*
008900021218     C* PROCEDI - ELABORA E INSERISCE I MOVIMENTI GABBIE/BANCALI
009000970526     C*------------------------------------------------------------------------*
009100020415     C     PROCEDI       BEGSR
009200021218     C*
009300021218     C* INIZIALIZZO I CAMPI DI OUT DELLA DS DI PROCEDURA (TISIG0DS)
009400021218     C                   EVAL      OG0ERR = *blanks
009500021218     C                   EVAL      OG0MSG = *blanks
009600021218     C*--------------
009700021218     C* 1° GIRO: INSERIMENTO MOVIMENTO RICHIESTO
009800021218     C*--------------
009900021218     C                   MOVEL     '0'           ERR_1GIRO
010000021218     C*
010100021218     C* - EFFETTUO CONTROLLI FORMALI
010200021218     C                   EVAL      W_CAU = IG0CAU
010300021218     C                   EVAL      W_SOG = IG0SOG
010400021218     C                   EXSR      DECODI
010500021218     C*
010600021218     C* - SE TUTTO OK RIEMPIO IL BUFFER X IL MOVIMENTO RICHIESTO
010700021218     C                   IF        ERRL00 = '0'
010800021218     C                   CLEAR                   AZMGB001
010900021218     C                   EVAL      W1_mgbTIP = §GGBTIP
011000021218     C                   EVAL      W1_mgbSOG = W_SOG
011100021218     C                   EVAL      W1_mgbCAU = W_CAU
011200021218     C                   EVAL      W1_mgbTIM = §GGBTIM
011300021218     C                   EVAL      W1_mgbCOD = IG0COD
011400021230     C* A seconda del segno della causale gestisco a programma il segno della quantità
011500021230     C                   IF        §GGBSEG = 'E'
011600021230     C                   EVAL      W1_mgbQTA = %abs(IG0QTA) * +1
011700021230     C                   ENDIF
011800021230     C                   IF        §GGBSEG = 'U'
011900021230     C                   EVAL      W1_mgbQTA = %abs(IG0QTA) * -1
012000021230     C                   ENDIF
012100030122     C                   IF        §GGBSEG = ' '
012200030122     C                   EVAL      W1_mgbQTA = IG0QTA
012300030122     C                   ENDIF
012400021230     C*
012500021218     C                   EVAL      W1_mgbRDO = IG0RDO
012600021218     C                   EVAL      W1_mgbDDO = IG0DDO
012700021218     C                   EVAL      W1_mgbUTE = IG0UTE
012800021218     C                   EVAL      W1_mgbPOU = IG0POU
012900021218     C                   EVAL      W1_mgbDAT = IG0DAT
013000021218     C                   EVAL      W1_mgbORA = IG0ORA
013100021218     C                   ELSE
013200021218     C                   MOVEL     '1'           ERR_1GIRO
013300021218     C                   ENDIF
013400021218     C*--------------
013500021218     C* 2° GIRO: SE NO ERRORI GIRO 1 E PREVISTO MOVIMENTO CORRELATO (SU CAUSALE 1° GIRO)
013600021218     C*--------------
013700021218     C                   MOVEL     '0'           ERR_2GIRO
013800021218     C*
013900021218     C                   IF        ERRL00 = '0' AND
014000021218     C                             §GGBCAC <> *blanks
014100021218     C*
014200021218     C* - EFFETTUO CONTROLLI FORMALI
014300021218     C                   EVAL      W_CAU = §GGBCAC
014400021218     C                   EVAL      W_SOG = IG0SMC
014500021218     C                   EXSR      DECODI
014600021218     C*
014700021218     C* - SE TUTTO OK RIEMPIO IL BUFFER X IL MOVIMENTO CORRELATO
014800021218     C                   IF        ERRL00 = '0'
014900021218     C                   CLEAR                   AZMGB002
015000021218     C                   EVAL      W2_mgbTIP = §GGBTIP
015100021218     C                   EVAL      W2_mgbSOG = W_SOG
015200021218     C                   EVAL      W2_mgbCAU = W_CAU
015300021218     C                   EVAL      W2_mgbTIM = §GGBTIM
015400021218     C                   EVAL      W2_mgbCOD = IG0COD
015500021230     C                   EVAL      W2_mgbQTA = W1_mgbQTA * -1
015600021218     C                   EVAL      W2_mgbRDO = IG0RDO
015700021218     C                   EVAL      W2_mgbDDO = IG0DDO
015800021218     C                   EVAL      W2_mgbUTE = IG0UTE
015900021218     C                   EVAL      W2_mgbPOU = IG0POU
016000021230     C                   EVAL      W2_mgbDAT = IG0DAT
016100021218     C                   EVAL      W2_mgbORA = IG0ORA
016200021218     C                   ELSE
016300021218     C                   MOVEL     '1'           ERR_2GIRO
016400021218     C                   ENDIF
016500021218     C                   ENDIF
016600020415     C*
016700970526     C                   ENDSR
016800021218     C*------------------------------------------------------------------------*
016900021218     C* DECODI - VERIFICA CORRETTEZZA DATI DI PASSAGGIO X GENERAZIONI MOVIMENTI
017000021218     C*------------------------------------------------------------------------*
017100021218     C     DECODI        BEGSR
017200021218     C*
017300021218     C* INIALIAZIZZO FLAG ERRORE
017400021218     C                   MOVEL     '0'           ERRL00
017500021218     C*
017600021218     C* CONTROLLO ESISTENZA CODICE CAUSALE
017700021218     C                   IF        ERRL00 = '0'
017800021230     C                   Z-ADD     1             X
017900021218     C     W_CAU         LOOKUP    KCAU(X)                                55
018000021218     C                   IF        *IN55 = *OFF
018100021218     C                   MOVEL     '1'           ERRL00
018200021218     C                   EVAL      OG0ERR = 'E'
018300021218     C                   EVAL      OG0MSG = 'CAUSALE INESISTENTE'
018400021218     C                   ELSE
018500021218     C                   MOVEL(P)  DCAU(X)       DGGB
018600021218     C                   ENDIF
018700021218     C                   ENDIF
018800021218     C*
018900021218     C* CONTROLLO ESISTENZA SOGGETTO MOVIMENTO
019000021218     C                   IF        ERRL00 = '0'
019100021218     C                   IF        §GGBTIP = 'C'                                *CLIENTE
019200021218     C                   MOVE(P)   W_SOG         ACOKSC
019300021218     C     KEYACO        CHAIN     CNACO00F
019400021218     C                   IF        not %found(CNACO00F)
019500021218     C                   MOVEL     '1'           ERRL00
019600021218     C                   EVAL      OG0ERR = 'E'
019700021218     C                   EVAL      OG0MSG = 'CLIENTE INESISTENTE'
019800021218     C                   ENDIF
019900021218     C                   ENDIF
020000021218     C*
020100021218     C                   IF        §GGBTIP = 'D'                                *DESTINATARIO
020200021218     C                   MOVE(P)   W_SOG         DSTCOD
020300021218     C     DSTCOD        CHAIN     AZDST04L
020400021218     C                   IF        not %found(AZDST04L)
020500021218     C                   MOVEL     '1'           ERRL00
020600021218     C                   EVAL      OG0ERR = 'E'
020700021218     C                   EVAL      OG0MSG = 'DESTINATARIO INESISTENTE'
020800021218     C                   ENDIF
020900021218     C                   ENDIF
021000021218     C*
021100021218     C                   IF        §GGBTIP = 'F'                                *P.O.
021200021218     C                   MOVE(P)   W_SOG         ORGFIL
021300021218     C     ORGFIL        CHAIN     AZORG01L
021400021218     C                   IF        not %found(AZORG01L)
021500021218     C                   MOVEL     '1'           ERRL00
021600021218     C                   EVAL      OG0ERR = 'E'
021700021218     C                   EVAL      OG0MSG = 'P.O. INESISTENTE'
021800021218     C                   ENDIF
021900021218     C                   ENDIF
022000021218     C                   ENDIF
022100021218     C*
022200021218     C* CONTROLLO ESISTENZA CODICE ARTICOLO
022300021218     C                   IF        ERRL00 = '0'
022400021218     C     IG0COD        CHAIN     AZAGB01L
022500021218     C                   IF        not %found(AZAGB01L)
022600021218     C                   MOVEL     '1'           ERRL00
022700021218     C                   EVAL      OG0ERR = 'E'
022800021218     C                   EVAL      OG0MSG = 'ARTICOLO INESISTENTE'
022900021218     C                   ENDIF
023000021218     C                   ENDIF
023100021218     C*
023200021218     C* CONTROLLO AUTORIZZAZIONE SPECIALE SU CAUSALE (SOLO X CAUSALI DI TIPO "M"=MANUALI)
023300021218     C                   IF        ERRL00 = '0'
023400021218     C                   IF        §GGBTIM = 'M'
023500021218     C                   IF        §GGBLIV = 'S'
023600021218     C                   CLEAR                   TIBS36DS
023700021218     C                   MOVEL     'L'           I36TLA
023800021218     C                   MOVEL     IG0UTE        I36UTE
023900021218     C                   CALL      'TIBS36R'
024000021218     C                   PARM                    TIBS36DS
024100021218     C                   IF        O36ERR <> *blanks
024200021218     C                   MOVEL     '1'           ERRL00
024300021218     C                   EVAL      OG0ERR = 'E'
024400021218     C                   EVAL      OG0MSG = 'IMPOSSIBILE REPERIRE DATI UTENTE'
024500021218     C                   ELSE
024600021218     C                   MOVEL     O36RUT        AZUTE00F
024700021218     C                   MOVEL     UTEFAF        DUTE01
024800021218     C                   IF        §UTEGB1 = 'S'
024900021218     C                   ELSE
025000021218     C                   MOVEL     '1'           ERRL00
025100021218     C                   EVAL      OG0ERR = 'E'
025200021218     C                   EVAL      OG0MSG = 'UTENTE NON AUTORIZZATO ' +
025300021218     C                                      'AD USO CAUSALE'
025400021218     C                   ENDIF
025500021218     C                   ENDIF
025600021218     C                   ENDIF
025700021218     C                   ENDIF
025800021218     C                   ENDIF
025900021230     C*
026000021230     C* CONTROLLO CHE I SOGGETTI DEI DUE MOVIMENTI CORRELATI NN SIANO MAI UGUALI
026100021230     C                   IF        ERRL00 = '0'
026200021230     C                   IF        IG0SOG <> IG0SMC
026300021230     C                   ELSE
026400021230     C                   MOVEL     '1'           ERRL00
026500021230     C                   EVAL      OG0ERR = 'E'
026600021230     C                   EVAL      OG0MSG = 'I SOGGETTI NON POSSONO COINCIDERE'
026700021230     C                   ENDIF
026800021230     C                   ENDIF
026900021218     C*
027000021218     C                   ENDSR
027100021218     C*------------------------------------------------------------------------*
027200021218     C* WRIMOV - SCRIVE I MOVIMENTI SUL FILE (AZMGB00F)
027300021218     C*------------------------------------------------------------------------*
027400021218     C     WRIMOV        BEGSR
027500021218     C*
027600021218     C* SOLO SE TUTTO OK PROCEDO CON LA SCRITTIRA EFFETTIVA DEI MOVIMENTI
027700021218     C                   IF        ERR_1GIRO = '0' AND
027800021218     C                             ERR_2GIRO = '0'
027900030122     C* REPERISCO UN NUMERATORE
028000030122     C                   EXSR      REPNUM
028100030122     C*
028200030122     C* SE ANCHE REPERIMENTO NUMERATORE E' OK => PROCEDO
028300030122     C                   IF        ERRNUM = '0'
028400021219     C                   CLEAR                   AZMGBXXX
028500021219     C                   EVAL      AZMGBXXX = AZMGB001
028600030122     C                   EVAL      mgbPRG = O33NRF
028700021218     C                   WRITE     AZMGB000
028800021218     C* VERIFICO SE E' PREVISTO UN MOVIMENTO CORRELATO
028900021219     C                   IF        AZMGB002 > *blanks
029000021219     C                   CLEAR                   AZMGBXXX
029100021219     C                   EVAL      AZMGBXXX = AZMGB002
029200030122     C                   EVAL      mgbPRG = O33NRF
029300021218     C                   WRITE     AZMGB000
029400021218     C                   ENDIF
029500021218     C                   ENDIF
029600030206     C                   ENDIF
029700021218     C*
029800021218     C                   ENDSR
029900030122     C*--------------------------------------------------------------------------------------------*
030000030122     C* REPNUM - REPERISCE NUMERATORE
030100030122     C*--------------------------------------------------------------------------------------------*
030200030122     C     REPNUM        BEGSR
030300030122     C*
030400030122     C* INIALIAZIZZO FLAG ERRORE
030500030122     C                   MOVEL     '0'           ERRNUM
030600030122     C*
030700030122     C                   CLEAR                   TRUL33DS
030800030122     C                   EVAL      I33TLA = 'L'
030900030122     C                   EVAL      I33OPE = *zeros
031000030122     C                   EVAL      I33CNU = 031
031100030122     C                   EVAL      I33NUM = 1
031200030122     C                   MOVEL     TRUL33DS      KPJBU
031300030122     C                   CALL      'TRUL33R'
031400030122     C                   PARM                    KPJBA
031500030122     C                   MOVEL     KPJBU         TRUL33DS
031600030122     C                   IF        O33ERR <> *zeros
031700030122     C                   MOVEL     '1'           ERRNUM
031800030122     C                   MOVEL     O33MSG        OG0MSG
031900030122     C                   ENDIF
032000030122     C*
032100030122     C                   ENDSR
032200021218     C*--------------------------------------------------------------------------------------------*
032300021218     C* CARTAB - CARICA LE TABELLE OCCORRENTI
032400021218     C*--------------------------------------------------------------------------------------------*
032500021218     C     CARTAB        BEGSR
032600021218     C*
032700021218     C* NUOVA TABELLA CODICI CAUSALI MOVIMENTI GABBIE/BANCALI (GGB)
032800021230     C                   Z-ADD     *zeros        x
032900021218     C                   EVAL      tbeCOD = 'GGB'
033000021218     C     KEYTBE        SETLL     tntbe01l
033100021218     C     KEYTBE        READE     tntbe01l
033200021218     C                   DOW       not %eof(tntbe01l)
033300030122     C                   IF        tbeatb = *blanks
033400021218     C                   MOVEL     tbeuni        DGGB
033500021230     C                   ADD       1             x
033600021218     C                   MOVEL     tbeke1        KCAU(x)
033700021218     C                   MOVEL     DGGB          DCAU(x)
033800030122     C                   ENDIF
033900021218     C     KEYTBE        READE     tntbe01l
034000021218     C                   ENDDO
034100021218     C*
034200021218     C                   ENDSR
034300970526     C*------------------------------------------------------------------------*
034400970526     C* *INZSR - OPERAZIONI INIZIALI
034500970526     C*------------------------------------------------------------------------*
034600970526     C     *INZSR        BEGSR
034700970526     C*---
034800970526     C* RICEVIMENTO PARAMETRI
034900970526     C*---
035000970526     C     *ENTRY        PLIST
035100021218     C                   PARM                    TISIG0DS
035200021218     C*---
035300021218     C* INIZIALIZZAZIONE CAMPI DI WRK
035400021218     C*---
035500021218     C                   Z-ADD     1             ACOKUT
035600021218     C                   Z-ADD     151           ACOKCC
035700021218     C*---
035800021218     C* CHIAVI DI LETTURA
035900021218     C*---
036000021218     C* LETTURA TNTBE01L - PARZIALE
036100021218     C     KEYTBE        KLIST
036200021218     C                   KFLD                    tbeCOD                         *CODICE TABELLA
036300021218     C* LETTURA CNACO00F - COMPLETA
036400021218     C     KEYACO        KLIST
036500021218     C                   KFLD                    acoKUT                         *CODICE UTENTE
036600021218     C                   KFLD                    acoKCC                         *CODICE CAPOCONTO
036700021218     C                   KFLD                    acoKSC                         *CODICE SOTTOCONTO
036800021218     C*
036900021218     C* CARICA LE TABELLE OCCORRENTI
037000021218     C                   EXSR      CARTAB
037100020415     C*
037200020415     C* CALCOLA LA DATA CORRENTE
037300020415     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
037400020415     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
037500020415     C                   Z-ADD     WN8           G08DAT
037600020415     C                   Z-ADD     *ZEROS        G08INV
037700020415     C                   MOVEL     '0'           G08ERR
037800020415     C                   CALL      'XSRDA8'
037900020415     C                   PARM                    WLBDA8
038000020415     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
038100970526     C*
038200970526     C                   ENDSR
