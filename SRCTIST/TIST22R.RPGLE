000100000000     H*------------------------------------------------------------------------*
000200050706     H* STATISTICA SERVIZI STRATEGI - GENERA FILE SALDI LOG VAS
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500991118     FTIVLR03L  IF   E           K DISK
000600991118     FTIVLT03L  IF   E           K DISK
000700041207     FTIVTL11L  IF   E           K DISK
000800001115     FTIANV01L  IF   E           K DISK
000900000000     FAZORG01L  IF   E           K DISK
001000041209     FTITAS30C  IF   E           K DISK
001100050706     FSISTL02L  UF A E           K DISK
001200000000     D*------------------------------------------------------------------------*
001300000000     D* SCHIERE
001400000000     D*------------------------------------------------------------------------*
001500000000     D*-------------------
001600000000     D* FILIALI CON RELATIVA DIVISIONE E AREA
001700000000     D*-------------------
001800000000     D FIL             S              3  0 DIM(500)                             *FILIALE
001900000000     D ARE             S              3  0 DIM(500)                             *AREA
002000000000     D DIV             S              1    DIM(500)                             *DIVISIONE
002100000000     D DIT             S              3    DIM(500)                             *RAMO AZIENDALE
002200000000     D*------------------------------------------------------------------------*
002300000000     D* INPUT
002400000000     D*------------------------------------------------------------------------*
002500000000     D*-------------------
002600000000     D* ARCHITETTURA
002700000000     D*-------------------
002800000000     D KPJBA         E DS
002900041209     D*------------------
003000041209     D* DS RIDEFINIZIONE REQUEST-DATA SERVIZI T&T
003100041209     D*-------------------
003200041209     D TIS174DSI     E DS
003300041210     D TIS183DSI     E DS
003400041210     D TIS166IDS     E DS
003500041210     D TIO107IDS     E DS
003600991122     D*------------------
003700991122     D* DS REPERIMENTO DATI UTENTE
003800991122     D*-------------------
003900991122     DBS69DS         E DS                  extname(TIBS69DS)
004000991122     DACODS          E DS                  extname(CNACO00F)
004100991122     DINDDS          E DS                  extname(CNIND00F)
004200991122     DCLPDS          E DS                  extname(CNCLP00F)
004300991122     DCLSDS          E DS                  extname(FNCLS00F)
004400000000     D*-------------------
004500000000     D* DS SCOMPOSIZIONE CODICE CLIENTE
004600000000     D*-------------------
004700000000     D                 DS
004800000000     D  DSCLI                  1      7  0                                      *CLIEN
004900000000     D  DSFIL                  1      3  0                                      *FIL
005000000000     D  DSCOD                  4      7  0                                      *COD
005100041209     D*-------------------
005200041210     D* DS SCOMPOSIZIONE CHIAVE VARI FORMATI NUMERI SPEDIZIONE
005300041209     D*-------------------
005400041210     D                 DS
005500041210     D  DSAAS                  1      2  0
005600041210     D  DSLNP                  3      5  0
005700041210     D  DSNRS                  6      7  0
005800041210     D  DSNSP                  8     14  0
005900041210     D  DSNSPEDIZ              1     14
006000000000     D*-------------------
006100000000     D* DS PARAMETRI DI LANCIO PGM
006200000000     D*-------------------
006300000000     D PARAM           DS
006400991122     D  PARDAI                 1      8  0
006500991122     D  PARDAF                 9     16  0
006600991122     D  PARSNR                17     17
006700991122     D  PARDIV                18     18
006800991122     D  PARARE                19     21  0
006900991122     D  PARFIL                22     24  0
007000991122     D  PARKSC                25     31  0
007100991122     D  PARDET                32     32
007200011228     D  PARGIO                33     35  0
007300060216     D  PARDEL                36     43  0
007400041207     D*-------------------
007500041207     D* VARIABILI D WRK
007600041207     D*-------------------
007700041207     D wDATAlf1        S             26    INZ(*blanks)
007800041207     D wDATAlf2        S              8    INZ(*blanks)
007900091027
008000091027     D/COPY GAITRASRC/SRCPROTOPR,TIBS10R1
008100091027     D/COPY GAITRASRC/SRCPROTOPI,TIBS10R1
008200091027
008300000000     C*------------------------------------------------------------------------*
008400000000     C* MAIN LINE
008500000000     C*------------------------------------------------------------------------*
008600000000     C*
008700000000     C* SE NON DEVE ESSERE ESEGUITO IL PGM VADO A FINE
008800000000     C     FINL00        IFEQ      '0'                                          --- 1 -->
008900060216     C*
009000060216     C* CANCELLA TUTTI I RECORD FINO ALLA DATA MANTENIMENTO CALCOLATA IN BASE
009100060216     C* AI GIORNI INDICATI IN TABELLA 'SDD'
009200060216     C                   EXSR      CANSTLOLD
009300991122     C*
009400050706     C* CANCELLA TUTTI I RECORD DEL PERIODO RICHIESTO DA SISTL00F
009500060216     C                   EXSR      CANSTLCUR
009600000000     C*
009700991118     C                   EXSR      OPEVTL
009800991118     C                   EXSR      OPEVLR
009900991118     C                   EXSR      OPEVLT
010000000000     C*
010100000000     C                   ENDIF                                                  <-- 1 ---
010200991122     C*
010300991122     C                   EXSR      FINSR
010400000000     C*
010500000000     C                   SETON                                        LR
010600991118     C*------------------------------------------------------------------------*
010700041207     C* OPEVTL - POSIZIONAMENTO SUL FILE TIVTL11L
010800991118     C*------------------------------------------------------------------------*
010900991118     C     OPEVTL        BEGSR
011000991118     C*
011100991122     C                   EXSR      INZWRK
011200991122     C*
011300991118     C* IMPOSTO FLAG PER SAPERE SU QUALE ARCHIVIO LEGGERE
011400991118     C                   MOVEL     1             TIPFIL            1
011500991118     C*
011600991118     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
011700991118     C                   EXSR      SETFIL
011800991118     C*
011900991118     C* CICLO FINO A FINE FILE
012000991118     C     FINL00        DOWEQ     '0'                                          --- 2 -->
012100991118     C*
012200991118     C* CREA IL RECORD SUI FILE DI LAVORO
012300991118     C                   EXSR      CREARE
012400991118     C*
012500991118     C* LEGGE IL RECORD SUCCESSIVO
012600991118     C                   EXSR      LETFIL
012700991118     C                   ENDDO                                                  <-- 2 ---
012800991118     C*
012900991118     C                   ENDSR
013000991118     C*------------------------------------------------------------------------*
013100991118     C* OPEVLR - POSIZIONAMENTO SUL FILE TIVLR03L
013200991118     C*------------------------------------------------------------------------*
013300991118     C     OPEVLR        BEGSR
013400991122     C*
013500991122     C                   EXSR      INZWRK
013600991118     C*
013700991118     C* IMPOSTO FLAG PER SAPERE SU QUALE ARCHIVIO LEGGERE
013800991118     C                   MOVEL     2             TIPFIL            1
013900991118     C*
014000991118     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
014100991118     C                   EXSR      SETFIL
014200991118     C*
014300991118     C* CICLO FINO A FINE FILE
014400991118     C     FINL00        DOWEQ     '0'                                          --- 2 -->
014500991118     C*
014600991118     C* CREA IL RECORD SUI FILE DI LAVORO
014700991118     C                   EXSR      CREARE
014800991118     C*
014900991118     C* LEGGE IL RECORD SUCCESSIVO
015000991118     C                   EXSR      LETFIL
015100991118     C                   ENDDO                                                  <-- 2 ---
015200991118     C*
015300991118     C                   ENDSR
015400991118     C*------------------------------------------------------------------------*
015500991118     C* OPEVLT - POSIZIONAMENTO SUL FILE TIVLT03L
015600991118     C*------------------------------------------------------------------------*
015700991118     C     OPEVLT        BEGSR
015800991122     C*
015900991122     C                   EXSR      INZWRK
016000991118     C*
016100991118     C* IMPOSTO FLAG PER SAPERE SU QUALE ARCHIVIO LEGGERE
016200991118     C                   MOVEL     3             TIPFIL            1
016300991118     C*
016400991118     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
016500991118     C                   EXSR      SETFIL
016600991118     C*
016700991118     C* CICLO FINO A FINE FILE
016800991118     C     FINL00        DOWEQ     '0'                                          --- 2 -->
016900991118     C*
017000991118     C* CREA IL RECORD SUI FILE DI LAVORO
017100991118     C                   EXSR      CREARE
017200991118     C*
017300991118     C* LEGGE IL RECORD SUCCESSIVO
017400991118     C                   EXSR      LETFIL
017500991118     C                   ENDDO                                                  <-- 2 ---
017600991118     C*
017700991118     C                   ENDSR
017800000000     C*------------------------------------------------------------------------*
017900000000     C* SETFIL - POSIZIONAMENTO SUL FILE
018000000000     C*------------------------------------------------------------------------*
018100000000     C     SETFIL        BEGSR
018200000000     C*
018300000000     C* POSIZIONAMENTO E PRIMA LETTURA
018400041207     C                   Z-ADD     PARDAF        KXXDAT
018500041207     C                   MOVEL     PARDAF        wDATAlf2
018600041207     C                   EXSR      MKDATISO
018700041207     C                   MOVEL     wDATAlf1      KXXDATISO
018800000000     C*
018900991118     C     TIPFIL        IFEQ      '1'
019000041207     C     KEYVTL        SETLL     TIVTL11L                           99        *FINE FILE
019100991118     C                   ENDIF
019200991118     C*
019300991118     C     TIPFIL        IFEQ      '2'
019400041207     C     KEYVLX        SETLL     TIVLR03L                           99        *FINE FILE
019500991118     C                   ENDIF
019600991118     C*
019700991118     C     TIPFIL        IFEQ      '3'
019800041207     C     KEYVLX        SETLL     TIVLT03L                           99        *FINE FILE
019900991118     C                   ENDIF
020000000000     C*
020100000000     C     *IN99         IFEQ      '1'
020200000000     C                   MOVEL     '1'           FINL00
020300000000     C                   ELSE
020400000000     C                   EXSR      LETFIL
020500000000     C                   END
020600000000     C*
020700000000     C                   ENDSR
020800000000     C*------------------------------------------------------------------------*
020900000000     C* LETFIL - LETTURA PROSSIMO RECORD
021000000000     C*------------------------------------------------------------------------*
021100000000     C     LETFIL        BEGSR
021200000000     C*
021300000000     C* LEGGO FINO A:
021400000000     C*     - FINE FILE O SUPERATO LIMITE FINALE
021500000000     C*     - TROVATO RECORD VALIDO
021600000000     C                   MOVEL     'N'           WRECOK
021700000000     C     FINL00        DOWEQ     '0'                                          --- 1 -->
021800000000     C     WRECOK        ANDEQ     'N'
021900000000     C*
022000000000     C* LETTURE SUCCESSIVE ALLA PRIMA
022100991118     C*
022200991118     C     TIPFIL        IFEQ      '1'
022300041207     C                   READ      TIVTL11L                               99
022400041209     C     *IN99         IFNE      '1'                                          *FINE FILE
022500041209     C                   EVAL      wDATAlf1 = %char(VTLRPYMMT)
022600041209     C                   EXSR      MKISODAT
022700041209     C                   MOVEL     wDATAlf2      SAVDAT
022800041209     C* Effettuo alcune considerazioni x cercare d reperire il + possibile il cliente a cui
022900041209     C* attribuire la richiesta loggata sul record corrente del T&T
023000041209     C                   CLEAR                   wVTLKSU
023100070430     C*
023200070430     C* Verifico se il codice cliente, se passato, è numerico valido
023300070430     C                   EVAL      PiInt = *on
023400140428     C                   EVAL      PiVal = *zeros
023500070430     C                   IF        VTLKSU <> *blanks
023600070430     C                   EVAL      PiStr=VTLKSU
023700070430     C                   EXSR      CHKNUM
023800070430     C                   ENDIF
023900070430     C*
024000041209     C* Innanzitutto considero solo i record con esito risposta 'DONE' (ergo OK)
024100140428     C                   IF        VTLRPYOPC = 'DONE' AND PiInt=*on
024200041209     C* Se cliente nn già espresso nel record d log
024300041209     C                   IF        VTLKSU = *blanks
024400041209     C* Eseguo routine d reperimento cliente unificante a cui attribuire il record d log corrente
024500041209     C                   EXSR      REPKSU
024600041209     C                   ELSE
024700140428     C                   MOVEL     VTLKSU        wVTLKSU
024800041209     C                   ENDIF
024900140428     C***                MOVE      wVTLKSU       SAVKSC
025000140428     C                   Z-ADD     PiVal         SAVKSC
025100130830     C                   ENDIF
025200041209     C                   ENDIF
025300991118     C                   ENDIF
025400991118     C*
025500991118     C     TIPFIL        IFEQ      '2'
025600991118     C                   READ      TIVLR03L                               99
025700041209     C     *IN99         IFNE      '1'                                          *FINE FILE
025800130903     C*
025900130903     C* Verifico se il codice cliente, se passato, è numerico valido
026000130903     C                   EVAL      PiInt = *on
026100130903     C                   IF        VLRKSC <> *blanks
026200130903     C                   EVAL      PiStr=VLRKSC
026300130903     C                   EXSR      CHKNUM
026400130903     C                   ENDIF
026500130903     C                   IF        PiInt=*on
026600041209     C                   Z-ADD     VLRDAT        SAVDAT
026700041209     C                   MOVE      VLRKSC        SAVKSC
026800130903     C                   ENDIF
026900041209     C                   ENDIF
027000991118     C                   ENDIF
027100991118     C*
027200991118     C     TIPFIL        IFEQ      '3'
027300991118     C                   READ      TIVLT03L                               99
027400041209     C     *IN99         IFNE      '1'                                          *FINE FILE
027500041209     C                   Z-ADD     VLTDAT        SAVDAT
027600041209     C                   MOVE      VLTKSC        SAVKSC
027700041209     C                   ENDIF
027800991118     C                   ENDIF
027900000000     C*
028000041209     C* SE NO FINE FILE E PERIODO ANCORA OK
028100001114     C     *IN99         IFEQ      '1'                                          *FINE FILE
028200001114     C     SAVDAT        ORLT      PARDAI                                       *SUPERATO RANGE PERI
028300001114     C                   MOVEL     '1'           FINL00
028400001114     C                   ELSE
028500041209     C* VERIFICO CHE IL CODICE CLIENTE SIA UN VALORE NUMERICO
028600070430     C                   EXSR      chkcod
028700041209     C                   IF        PiInt=*on OR
028800041209     C                             PiStr=*blanks
028900001114     C                   EXSR      CHKREC                                       *CONTR.VALIDITA REC.
029000001114     C                   ENDIF
029100001114     C                   ENDIF
029200001114     C*
029300001114     C                   ENDDO                                                  <-- 1 ---
029400000000     C*
029500000000     C                   ENDSR
029600000000     C*------------------------------------------------------------------------*
029700000000     C* CHKREC - CONTROLLA VALIDITA' DEL RECORD
029800000000     C*------------------------------------------------------------------------*
029900000000     C     CHKREC        BEGSR
030000000000     C*
030100001114     C                   MOVEL     'S'           WRECOK            1
030200000000     C*
030300000000     C* RECUPERA FILIALE,DIVISIONE ED AREA IN BASE AL CODICE CLIENTE
030400000000     C     WRECOK        IFEQ      'S'
030500991122     C                   MOVE      SAVKSC        DSCLI
030600000000     C                   Z-ADD     DSFIL         WFIL              3 0
030700000000     C                   EXSR      DIVARE
030800000000     C                   MOVEL     WDIV          WDIV              1
030900000000     C                   Z-ADD     WARE          WARE              3 0
031000000000     C                   ENDIF
031100000000     C*
031200000000     C     WRECOK        IFEQ      'S'
031300000000     C*
031400000000     C* SE DIVISIONE DEL RECORD <> DIVISIONE RICHIESTA, ESCLUDO
031500000000     C*
031600000000     C     PARDIV        IFNE      *BLANKS
031700000000     C     WDIV          IFEQ      PARDIV
031800000000     C                   ELSE
031900000000     C                   MOVEL     'N'           WRECOK
032000000000     C                   ENDIF
032100000000     C                   ENDIF
032200000000     C*
032300000000     C* SE AREA DEL RECORD <> AREA RICHIESTA, ESCLUDO
032400000000     C     PARARE        IFNE      *ZEROS
032500000000     C     WARE          IFEQ      PARARE
032600000000     C                   ELSE
032700000000     C                   MOVEL     'N'           WRECOK
032800000000     C                   ENDIF
032900000000     C                   ENDIF
033000991122     C*
033100991122     C* SE FILIALE PARTENZA DEL RECORD <> FILIALE RICHIESTA, ESCLUDO
033200991122     C     PARFIL        IFNE      *ZEROS
033300991122     C     WFIL          IFNE      PARFIL
033400991122     C                   MOVEL     'N'           WRECOK
033500991122     C                   ENDIF
033600991122     C                   ENDIF
033700991118     C*
033800991118     C* SE CLIENTE CORRENTE <> CLIENTE RICHIESTO, ESCLUDO
033900991122     C     PARKSC        IFNE      *ZEROS
034000991122     C     SAVKSC        IFNE      PARKSC
034100991122     C                   MOVEL     'N'           WRECOK
034200991122     C                   ENDIF
034300991122     C                   ENDIF
034400041209     C*
034500041209     C* IN CASO D T&T CONSIDERO SOLO I RECORD D TIPO DB/T2/T3/TO/TR/LV
034600041209     C* ADESSO SONO GIA' LOGGATI NEL T&T
034700041209     C                   IF        TIPFIL = '1'
034800041209     C                   IF        VTLTIP = 'DB' OR
034900041209     C                             VTLTIP = 'T2' OR
035000041209     C                             VTLTIP = 'T3' OR
035100041209     C                             VTLTIP = 'TO' OR
035200041209     C                             VTLTIP = 'TR' OR
035300041209     C                             VTLTIP = 'LV'
035400041209     C                   ELSE
035500041209     C                   MOVEL     'N'           WRECOK
035600041209     C                   ENDIF
035700041209     C                   ENDIF
035800041209     C*
035900041209     C* IN CASO D UPLOAD NN CONSIDERO I RECORD DEL SERVIZIO 'TT' IN QUANTO
036000041209     C* ADESSO SONO GIA' LOGGATI NEL T&T
036100041209     C                   IF        TIPFIL = '2'
036200041209     C                   IF        VLRISV = 'TT'
036300041209     C                   MOVEL     'N'           WRECOK
036400041209     C                   ENDIF
036500041209     C                   ENDIF
036600041209     C*
036700041209     C* IN OGNI CASO SE FILIALE PARTENZA DEL RECORD = *ZEROS => NN CONSIDERO
036800041209     C     WFIL          IFEQ      *ZEROS
036900041209     C                   MOVEL     'N'           WRECOK
037000041209     C                   ENDIF
037100041209     C*
037200041207     C                   ENDIF
037300991118     C*
037400000000     C                   ENDSR
037500000000     C*------------------------------------------------------------------------*
037600000000     C* CREARE - CREA IL RECORD SUL FILE DI LAVORO
037700000000     C*------------------------------------------------------------------------*
037800000000     C     CREARE        BEGSR
037900991025     C*
038000991118     C                   Z-ADD     SAVDAT        KLODAT
038100991122     C                   MOVE      SAVKSC        KLOKSC
038200000000     C*
038300041209     C* SCRIVO O RISCRIVO FILE DI LAVORO
038400000000     C*
038500050706     C     KEYLOG        CHAIN     SISTL02L                           98
038600000000     C*
038700000000     C* RIEMPIO BUFFER RECORD
038800000000     C*
038900050706     C                   MOVEL     WDIV          STLDIV
039000050706     C                   Z-ADD     WARE          STLARE
039100050706     C                   Z-ADD     WFIL          STLFIL
039200050706     C                   Z-ADD     KLODAT        STLDAT
039300050706     C                   MOVE      KLOKSC        STLKSC
039400991122     C   98              EXSR      AZZSTL                                       *RE-INIZIALIZZA BUFF
039500991122     C                   EXSR      ADDSTL                                       *AGGIUNGE SPED.AL RE
039600050706     C   98              WRITE     SISTL000                                     *RECORD NON ESISTE,
039700050706     C  N98              UPDATE    SISTL000                                     *RECORD ESISTE, AGGI
039800991122     C*
039900000000     C                   ENDSR
040000000000     C*------------------------------------------------------------------------*
040100050706     C* ADDSTL - AGGIUNGE IL RECORD DI LOG AL RECORD SISTL00F
040200000000     C*------------------------------------------------------------------------*
040300991118     C     ADDSTL        BEGSR
040400041207     C*
040500041207     C     TIPFIL        IFEQ      '1'                                          T&T
040600050706     C                   Z-ADD     1             STLTTT
040700041207     C     VTLTIP        IFEQ      'DB'
040800050706     C                   ADD       1             STLTDB
040900050706     C                   Z-ADD     1             STLCDB
041000041214     C* Evidenzio un "d cui" x il cliente d Roma che effettua richieste dettagli bolle
041100041214     C* in modo automatico
041200041214     C                   IF        VTLCLTIPAD = '212.141.48.2'    OR
041300041214     C                             VTLCLTIPAD = '195.110.104.134'
041400050706     C                   ADD       1             STLDBL
041500041214     C                   ENDIF
041600041207     C                   ENDIF
041700041207     C     VTLTIP        IFEQ      'TO'
041800050706     C                   ADD       1             STLORM
041900050706     C                   Z-ADD     1             STLCOR
042000041207     C                   ENDIF
042100041207     C     VTLTIP        IFEQ      'TR'
042200041207     C                   IF        VTLSUN > 0
042300050706     C                   ADD       1             STLVAG
042400050706     C                   Z-ADD     1             STLCVG
042500041207     C                   ELSE
042600050706     C                   ADD       1             STLGML
042700050706     C                   Z-ADD     1             STLCGL
042800041207     C                   ENDIF
042900041207     C                   ENDIF
043000041207     C     VTLTIP        IFEQ      'LV'
043100050706     C                   ADD       1             STLPOD
043200050706     C                   Z-ADD     1             STLCPO
043300041207     C                   ENDIF
043400041207     C     VTLTIP        IFEQ      'T2'
043500050706     C                   ADD       1             STLTT2
043600050706     C                   Z-ADD     1             STLCT2
043700041207     C                   ENDIF
043800041207     C     VTLTIP        IFEQ      'T3'
043900050706     C                   ADD       1             STLTT3
044000050706     C                   Z-ADD     1             STLCT3
044100041207     C                   ENDIF
044200041207     C                   ENDIF
044300041207     C*
044400041207     C     TIPFIL        IFEQ      '2'                                          UPLOAD
044500050706     C                   Z-ADD     1             STLCUL
044600050706     C                   Z-ADD     1             STLURC
044700050706     C                   ADD       1             STLUTN
044800041207     C     VLRFLG        IFEQ      '1'
044900041207     C     VLRFLG        OREQ      '3'
045000050706     C                   ADD       1             STLUFS
045100041207     C                   ENDIF
045200041207     C                   ENDIF
045300041207     C*
045400041207     C     TIPFIL        IFEQ      '3'                                          DOWNLOAD
045500050706     C                   Z-ADD     1             STLCDL
045600050706     C                   Z-ADD     1             STLDRC
045700050706     C                   ADD       1             STLDTN
045800041207     C                   IF        (VLTSTS = '1' OR VLTSTS = '3') AND
045900041207     C                              VLTTIA = 'F'
046000050706     C                   ADD       1             STLDFS
046100041207     C                   ENDIF
046200041207     C                   IF        (VLTSTS = '5' OR VLTSTS = '3') AND
046300041207     C                              VLTTIA = 'S'
046400050706     C                   ADD       1             STLDFS
046500041207     C                   ENDIF
046600041207     C                   ENDIF
046700041207     C*
046800041207     C* Reperimento "Ragione Sociale" del Cliente Unificante
046900991122     C                   CLEAR                   BS69DS
047000991122     C                   CLEAR                   ACODS
047100991122     C                   MOVEL     KNSIF         I69SIF
047200001114     C                   MOVE      SAVKSC        I69KAC
047300991122     C                   CALL      'TIBS69R'
047400991122     C                   PARM                    BS69DS
047500991122     C                   PARM                    ACODS
047600991122     C                   PARM                    INDDS
047700991122     C                   PARM                    CLPDS
047800991122     C                   PARM                    CLSDS
047900991122     C     O69ERR        IFNE      '1'
048000050706     C                   MOVEL     ACORAG        STLRAG
048100001115     C                   ELSE
048200001115     C                   MOVEL     *ALL'0'       DEPKSC            8
048300001115     C                   MOVE      SAVKSC        DEPKSC
048400001115     C     DEPKSC        CHAIN     TIANV01L
048500001115     C                   IF        %FOUND(TIANV01L)
048600050706     C                   MOVEL(P)  ANVDES        STLRAG
048700001115     C                   ELSE
048800050706     C                   MOVEL     *ALL'*'       STLRAG
048900001115     C                   ENDIF
049000991122     C                   ENDIF
049100991122     C*
049200000000     C                   ENDSR
049300001114     C*----------------------------------------------------*
049400001114     C*  CONTROLLO NUMERICITA' CAMPI
049500001114     C*----------------------------------------------------*
049600070430     C     CHKCOD        BEGSR
049700001114     C*
049800001114     C                   IF        TIPFIL = '1'
049900041209     C                   EVAL      PiStr = wVTLKSU
050000001114     C                   ENDIF
050100001114     C                   IF        TIPFIL = '2'
050200001114     C                   EVAL      PiStr = VLRKSC
050300001114     C                   ENDIF
050400001114     C                   IF        TIPFIL = '3'
050500001114     C                   EVAL      PiStr = VLTKSC
050600001114     C                   ENDIF
050700001114     C*
050800070430     C                   EXSR      CHKNUM
050900001114     C*
051000001114     C                   ENDSR
051100000000     C*------------------------------------------------------------------------*
051200050706     C* AZZSTL - AZZERA CAMPI IN SISTL00F
051300000000     C*------------------------------------------------------------------------*
051400991122     C     AZZSTL        BEGSR
051500000000     C*
051600050706     C                   Z-ADD     0             STLDRC
051700050706     C                   Z-ADD     0             STLDTN
051800050706     C                   Z-ADD     0             STLDFS
051900050706     C                   Z-ADD     0             STLURC
052000050706     C                   Z-ADD     0             STLUTN
052100050706     C                   Z-ADD     0             STLUFS
052200050706     C                   Z-ADD     0             STLTTT
052300050706     C                   Z-ADD     0             STLTDB
052400050706     C                   Z-ADD     0             STLORM
052500050706     C                   Z-ADD     0             STLVAG
052600050706     C                   Z-ADD     0             STLCDL
052700050706     C                   Z-ADD     0             STLCUL
052800050706     C                   Z-ADD     0             STLCDB
052900050706     C                   Z-ADD     0             STLTT2
053000050706     C                   Z-ADD     0             STLTT3
053100050706     C                   Z-ADD     0             STLGML
053200050706     C                   Z-ADD     0             STLPOD
053300050706     C                   Z-ADD     0             STLDBL
053400050706     C                   Z-ADD     0             STLCT2
053500050706     C                   Z-ADD     0             STLCT3
053600050706     C                   Z-ADD     0             STLCOR
053700050706     C                   Z-ADD     0             STLCVG
053800050706     C                   Z-ADD     0             STLCGL
053900050706     C                   Z-ADD     0             STLCPO
054000000000     C*
054100000000     C                   ENDSR
054200000000     C*------------------------------------------------------------------------*
054300000000     C* DIVARE - RECUPERA AREA E DIVISIONE
054400000000     C*------------------------------------------------------------------------*
054500000000     C     DIVARE        BEGSR
054600000000     C*
054700030528     C***                IF        WFIL = 199
054800030528     C***                MOVEL     '0'           WDIV
054900030528     C***                Z-ADD     199           WARE
055000030528     C***                ELSE
055100000000     C                   Z-ADD     1             I
055200000000     C     WFIL          LOOKUP    FIL(I)                                 99
055300001115     C     *IN99         IFEQ      '0'
055400000000     C                   MOVEL     '*'           WDIV              1
055500000000     C                   Z-ADD     999           WARE              3 0
055600001115     C                   ELSE
055700000000     C                   MOVEL     DIV(I)        WDIV
055800000000     C                   Z-ADD     ARE(I)        WARE
055900001115     C                   ENDIF
056000030528     C***                ENDIF
056100000000     C*
056200000000     C                   ENDSR
056300000000     C*------------------------------------------------------------------------*
056400000000     C* CARORG - CARICA TUTTE LE FILIALI DALL'ORGANIGRAMMA
056500000000     C*------------------------------------------------------------------------*
056600000000     C     CARORG        BEGSR
056700000000     C*
056800000000     C                   Z-ADD     *ZEROS        KORFIL
056900000000     C     KEYORG        SETLL     AZORG01L                           99        *FINE-ARCHIV
057000000000     C  N99              READ      AZORG01L                               99
057100000000     C                   Z-ADD     0             I                 3 0          *INDICE SCHIERA
057200000000     C     *IN99         DOWEQ     '0'                                          --- 1 -->
057300000000     C     ORGFVA        IFEQ      *BLANKS                                      --- 2 -->
057400030528     C***  ORGFAG        IFNE      'V'
057500000000     C                   ADD       1             I
057600000000     C                   Z-ADD     ORGFIL        FIL(I)
057700000000     C                   Z-ADD     ORGCAR        ARE(I)
057800000000     C                   MOVEL     ORGFL3        DIV(I)
057900000000     C                   MOVEL     ORGDIT        DIT(I)
058000030528     C***                ENDIF
058100030528     C                   ENDIF                                                  <-- 2 ---
058200000000     C                   READ      AZORG01L                               99
058300030528     C                   ENDDO                                                  <-- 1 ---
058400000000     C*
058500000000     C                   ENDSR
058600060216     C*------------------------------------------------------------------------*
058700060217     C* CANSTLOLD - CANCELLA I RECORD DEL SISTL00F CONSIDERATI VECCHI
058800060216     C*------------------------------------------------------------------------*
058900060216     C     CANSTLOLD     BEGSR
059000060216     C*
059100060216     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
059200060217     C     *LOVAL        SETLL     SISTL02L                           97
059300060216     C  N97              READ      SISTL02L                               97
059400060216     C*
059500060216     C* CICLO FINO A FINE FILE OPPURE A DATA MAGGIORE DATA FINE
059600060216     C     *IN97         DOWEQ     '0'                                          --- 1 -->
059700060217     C     STLDAT        ANDLE     PARDEL
059800060216     C*
059900060216     C* DELETO SE LA DATA DEL WRKFILE E' COMPRESA NEL RANGE DEL PERIODO DI LANCIO
060000060216     C                   DELETE    SISTL000
060100060216     C*
060200060216     C* LEGGE IL RECORD SUCCESSIVO
060300060216     C                   READ      SISTL02L                               97
060400060216     C*
060500060216     C                   ENDDO                                                  <-- 1 ---
060600060216     C*
060700060216     C                   ENDSR
060800000000     C*------------------------------------------------------------------------*
060900060216     C* CANSTLCUR - CANCELLA IL SISTL00F TUTTI I RECORD DEI MESI RICHIESTI
061000000000     C*------------------------------------------------------------------------*
061100060216     C     CANSTLCUR     BEGSR
061200000000     C*
061300000000     C* POSIZIONAMENTO SUL FILE E PRIMA LETTURA
061400050706     C     PARDAI        SETLL     SISTL02L                           97
061500050706     C  N97              READ      SISTL02L                               97
061600000000     C*
061700991122     C* CICLO FINO A FINE FILE OPPURE A DATA MAGGIORE DATA FINE
061800991122     C     *IN97         DOWEQ     '0'                                          --- 1 -->
061900050706     C     STLDAT        ANDGE     PARDAI
062000050706     C     STLDAT        ANDLE     PARDAF
062100991122     C*
062200991122     C* DELETO SE LA DATA DEL WRKFILE E' COMPRESA NEL RANGE DEL PERIODO DI LANCIO
062300050706     C                   DELETE    SISTL000
062400000000     C*
062500000000     C* LEGGE IL RECORD SUCCESSIVO
062600050706     C                   READ      SISTL02L                               97
062700000000     C*
062800991122     C                   ENDDO                                                  <-- 1 ---
062900000000     C*
063000000000     C                   ENDSR
063100991122     C*------------------------------------------------------------------------*
063200991122     C* FINSR - OPERAZIONI FINALI
063300991122     C*------------------------------------------------------------------------*
063400991122     C     FINSR         BEGSR
063500991122     C*
063600991122     C* CHIUDE I FILE DEI PGM CHIAMATI
063700991122     C                   CLEAR                   BS69DS
063800991122     C                   CLEAR                   ACODS
063900991122     C                   CLEAR                   INDDS
064000991122     C                   CLEAR                   CLPDS
064100991122     C                   CLEAR                   CLSDS
064200991122     C                   MOVEL     'C'           I69TLA
064300991122     C                   MOVEL     KNSIF         I69SIF
064400991122     C                   CALL      'TIBS69R'
064500991122     C                   PARM                    BS69DS
064600991122     C                   PARM                    ACODS
064700991122     C                   PARM                    INDDS
064800991122     C                   PARM                    CLPDS
064900991122     C                   PARM                    CLSDS
065000991122     C*
065100991122     C                   ENDSR
065200991122     C*------------------------------------------------------------------------*
065300991122     C* INZWRK - INIZIALIZA VARIABILI DI WRK RICORRENTI
065400991122     C*------------------------------------------------------------------------*
065500991122     C     INZWRK        BEGSR
065600991122     C*--------------------
065700991122     C* CAMPI DI CONTROLLO
065800991122     C*--------------------
065900991122     C                   MOVEL     '0'           FINL00            1
066000991122     C                   Z-ADD     1             CODUT             1 0
066100000710     C                   Z-ADD     *HIVAL        SAVDAT            8 0
066200001114     C                   MOVE      *LOVAL        SAVKSC            7 0
066300991122     C*
066400991122     C                   ENDSR
066500041207     C*------------------------------------------------------------------------*
066600041207     C* MKDATISO - TRASFORMAZIONE DATA NUMERICA 8 IN FORMATO TIMESTAMP
066700041207     C*------------------------------------------------------------------------*
066800041207     C     MKDATISO      BEGSR
066900041207     C*
067000041207     C                   EVAL      wDATAlf1 = %subst(wDATAlf2:1:4) + '-' +
067100041207     C                                        %subst(wDATAlf2:5:2) + '-' +
067200041207     C                                        %subst(wDATAlf2:7:2) + '-' +
067300041207     C                                        '24.00.00.000000'
067400041207     C*
067500041207     C                   ENDSR
067600041207     C*------------------------------------------------------------------------*
067700041207     C* MKISODAT - TRASFORMAZIONE TIMESTAMP IN NUMERICA 8
067800041207     C*------------------------------------------------------------------------*
067900041207     C     MKISODAT      BEGSR
068000041207     C*
068100041207     C                   EVAL      wDATAlf2 = %subst(wDATAlf1:1:4)+
068200041207     C                                        %subst(wDATAlf1:6:2)+
068300041207     C                                        %subst(wDATAlf1:9:2)
068400041207     C*
068500041207     C                   ENDSR
068600041209     C*------------------------------------------------------------------------*
068700041209     C* REPKSU - REPERIMENTO CLIENTE UNIFICANTE DA REQUEST-DATA LOG T&T
068800041209     C*------------------------------------------------------------------------*
068900041209     C     REPKSU        BEGSR
069000041209     C*
069100041210     C* Effettuo verifiche a seconda del servizio
069200041210     C*
069300041210     C* Inizializzo variabili d wrk
069400041210     C                   CLEAR                   DSNSPEDIZ
069500041210     C                   Z-ADD     *zeros        wKSC             10 0
069600041209     C*
069700041209     C*******
069800041209     C* - servizio 'DB'
069900041209     C*******
070000041209     C                   IF        VTLTIP = 'DB'
070100041209     C                   EVAL      TIS174DSI = VTLRQSDTA
070200041209     C*
070300041209     C* Se già espresso "in chiaro" codice cliente unificante utilizzo quello
070400041209     C                   IF        KSCI74 <> *blanks
070500041209     C                   MOVEL     KSCI74        wVTLKSU
070600041209     C                   ELSE
070700041210     C*
070800041210     C* Solo se numero spedizione valorizzato
070900041210     C                   MOVE(P)   NSPEDIZI74    DSNSPEDIZ
071000041210     C                   IF        DSNSPEDIZ <> *blanks
071100041209     C*
071200041209     C* 1° tentativo su anno della richiesta
071300041209     C                   MOVEL     SAVDAT        TASAAS
071400041209     C                   Z-ADD     DSLNP         TASLNP
071500041209     C                   Z-ADD     DSNRS         TASNRS
071600041209     C                   Z-ADD     DSNSP         TASNSP
071700041209     C     KEYTAS        CHAIN     TITAS30C
071800041209     C                   IF        %found(titas30c)
071900041210     C                   Z-ADD     TASKSC        wKSC
072000041209     C                   ELSE
072100041209     C*
072200041209     C* 2° tentativo su anno della richiesta - 1 (anno precedente)
072300041209     C                   EVAL      TASAAS = TASAAS - 1
072400041209     C     KEYTAS        CHAIN     TITAS30C
072500041209     C                   IF        %found(titas30c)
072600041210     C                   Z-ADD     TASKSC        wKSC
072700041209     C                   ENDIF
072800041209     C                   ENDIF
072900041210     C                   ENDIF
073000041210     C*
073100041210     C                   ENDIF
073200041210     C                   ENDIF
073300041210     C*******
073400041210     C* - servizio 'LV'
073500041210     C*******
073600041210     C                   IF        VTLTIP = 'LV'
073700041210     C                   EVAL      TIS183DSI = VTLRQSDTA
073800041210     C*
073900041210     C* Solo se numero spedizione valorizzato
074000041210     C                   MOVE(P)   IN_CONS       DSNSPEDIZ
074100041210     C                   IF        DSNSPEDIZ <> *blanks
074200041210     C*
074300041210     C* Effettuo unico tentativo in quanto il numero spedizione x questo servizio comprende già
074400041210     C* anche l'anno (anche se solo d 2 cifre)
074500041210     C                   EVAL      TASAAS = 2000 + DSAAS
074600041210     C                   Z-ADD     DSLNP         TASLNP
074700041210     C                   Z-ADD     DSNRS         TASNRS
074800041210     C                   Z-ADD     DSNSP         TASNSP
074900041210     C     KEYTAS        CHAIN     TITAS30C
075000041210     C                   IF        %found(titas30c)
075100041210     C                   Z-ADD     TASKSC        wKSC
075200041210     C                   ENDIF
075300041210     C                   ENDIF
075400041210     C*
075500041210     C                   ENDIF
075600041210     C*******
075700041210     C* - servizio 'TR'
075800041210     C*******
075900041210     C                   IF        VTLTIP = 'TR'
076000041210     C                   EVAL      TIS166IDS = VTLRQSDTA
076100041210     C*
076200041210     C* Solo se numero spedizione valorizzato
076300041210     C                   EVAL      DSNSPEDIZ = %subst(TIS166IDS:32:2)+
076400041210     C                                         %subst(TIS166IDS:18:12)
076500041210     C                   IF        DSNSPEDIZ <> *blanks
076600041210     C*
076700041210     C* Effettuo unico tentativo in quanto il numero spedizione x questo servizio comprende già
076800041210     C* anche l'anno (anche se solo d 2 cifre)
076900041210     C                   EVAL      TASAAS = 2000 + DSAAS
077000041210     C                   Z-ADD     DSLNP         TASLNP
077100041210     C                   Z-ADD     DSNRS         TASNRS
077200041210     C                   Z-ADD     DSNSP         TASNSP
077300041210     C     KEYTAS        CHAIN     TITAS30C
077400041210     C                   IF        %found(titas30c)
077500041210     C                   Z-ADD     TASKSC        wKSC
077600041210     C                   ENDIF
077700041210     C                   ENDIF
077800041210     C*
077900041210     C                   ENDIF
078000041210     C*******
078100041210     C* - servizio 'TO'
078200041210     C*******
078300140714     C                   IF        VTLTIP = 'TO' AND VTLRQSOPC = 'CONFERMA'
078400041210     C                   EVAL      TIO107IDS = VTLRQSDTA
078500041210     C*
078600041210     C* X l'inserimento ORM nn "codificati" attribuisco l'operazione al generico cliente '8888'
078700041210     C* del P.O. Ritiro
078800041210     C                   IF        O107POR <> *blanks
078900041210     C                   MOVEL(P)  O107POR       wKSC
079000041210     C                   MOVE      '8888'        wKSC
079100041210     C                   ENDIF
079200041210     C*
079300041210     C                   ENDIF
079400041210     C*
079500041210     C*******
079600041210     C* Al termine del cliente mittente eventualmente reperito considero il relativo codice padre
079700041210     C*******
079800041209     C*
079900091027     C* Se reperito Codice Cliente a questo punto reperisco l'eventuale padre legame tipo internet
080000041210     C                   IF        wKSC > *zeros
080100041209     C                   CLEAR                   TIBS10DS
080200041209     C                   EVAL      D10PAF = 'P'
080300041210     C                   EVAL      D10COD = wKSC                                * uso cli. mitt.
080400091027     C*
080500091027     C* Preparazione chiamata al TIBS10R1
080600091027     C                   clear                   tibs10r1ds
080700091027     C                   eval      tibs10r1ds.tibs10ds = tibs10ds
080800091027     C                   eval      tibs10r1ds.tibs10ds.d10tle = *blanks
080900091027     C                   eval      tibs10r1ds.d10igrptle = 'W'
081000091027     C*
081100091027     C                   call      'TIBS10R1'
081200091027     C                   parm                    tibs10r1ds
081300091027     C*
081400091027     C                   eval      tibs10ds = tibs10r1ds.tibs10ds
081500091027     C*
081600041209     C                   IF        D10ERR = '1'
081700041209     C                   MOVE      D10COD        wVTLKSU
081800041209     C                   ELSE
081900041209     C                   MOVE      D10COP        wVTLKSU
082000041209     C                   ENDIF
082100041209     C                   ENDIF
082200041209     C*
082300041209     C                   ENDSR
082400070430     C*----------------------------------------------------*
082500070430     C*  CONTROLLO NUMERICITA' CAMPI
082600070430     C*----------------------------------------------------*
082700070430     C     CHKNUM        BEGSR
082800070430     C*
082900070430     C                   IF        PiDecChr = *blanks
083000070430     C                   EVAL      PiDecChr = ','
083100070430     C                   ENDIF
083200070430     C*
083300070430     C                   CALL(e)   'ISNUMERIC'
083400070430     C                   PARM                    PiStr            30
083500070430     C                   PARM                    PiDecChr          1
083600070430     C                   PARM      *ZEROS        PiVal            30 9
083700070430     C                   PARM      '0'           PiInt             1
083800070430     C                   PARM      '0'           PiNum             1
083900070430     C                   IF        %error
084000070430     C                   EVAL      PiInt=*off
084100070430     C                   ENDIF
084200070430     C*
084300070430     C                   ENDSR
084400000000     C*------------------------------------------------------------------------*
084500000000     C* *INZSR - ROUTINE INIZIALE
084600000000     C*------------------------------------------------------------------------*
084700000000     C     *INZSR        BEGSR
084800000000     C*--------------------
084900000000     C* RICEVIMENTO PARAMETRI
085000000000     C*--------------------
085100000000     C     *ENTRY        PLIST
085200000000     C                   PARM                    KPJBA
085300000000     C                   MOVEL     KPJBU         PARAM
085400000000     C*--------------------
085500000000     C* CAMPI RIFERITI AL DATA BASE
085600000000     C*--------------------
085700000000     C     *LIKE         DEFINE    ORGFIL        KORFIL
085800050706     C     *LIKE         DEFINE    STLDAT        KLODAT
085900050706     C     *LIKE         DEFINE    STLDAT        KXXDAT
086000041207     C     *LIKE         DEFINE    VTLRPYMMT     KXXDATISO
086100050706     C     *LIKE         DEFINE    STLKSC        KLOKSC
086200041209     C     *LIKE         DEFINE    VTLKSU        wVTLKSU
086300991122     C                   MOVEL     '0'           FINL00            1
086400000000     C*--------------------
086500000000     C* CHIAVI DI LETTURA
086600000000     C*--------------------
086700000000     C* CHIAVE LETTURA AZORG01L - COMPLETA
086800000000     C     KEYORG        KLIST
086900000000     C                   KFLD                    KORFIL
087000000000     C*
087100050706     C* CHIAVE LETTURA SISTL02L - COMPLETA
087200991122     C     KEYLOG        KLIST
087300991122     C                   KFLD                    KLODAT
087400991122     C                   KFLD                    KLOKSC
087500991122     C*
087600041207     C* CHIAVE LETTURA GENERICA X I FILE DI LOG UPLOAD E DOWNLOAD
087700041207     C     KEYVLX        KLIST
087800991122     C                   KFLD                    KXXDAT
087900041207     C     KEYVTL        KLIST
088000041207     C                   KFLD                    KXXDATISO
088100041209     C*
088200041209     C* CHIAVE LETTURA TOTAS30C - COMPLETA
088300041209     C     KEYTAS        KLIST
088400041209     C                   KFLD                    TASAAS
088500041209     C                   KFLD                    TASLNP
088600041209     C                   KFLD                    TASNRS
088700041209     C                   KFLD                    TASNSP
088800000000     C*
088900000000     C* CARICA ORGANIGRAMMA
089000000000     C                   EXSR      CARORG
089100000000     C*
089200000000     C                   ENDSR
