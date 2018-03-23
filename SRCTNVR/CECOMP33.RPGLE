000100000000     H*------------------------------------------------------------------------*
000200991117     H* SIMULAZIONE CONTO ECONOMICO - SITEMA VOCI E TARIFFE
000300000000     H*------------------------------------------------------------------------*
000400000000     H DECEDIT('0,') DATEDIT(*DMY.)
000500000000     F*------------------------------------------------------------------------*
000600000000     F* DATA BASE
000700000000     F*------------------------------------------------------------------------*
000800991117     F*---
000900010807     F* Voci
001000991117     F*---
001100010808     FECEVT01L  UF A E           K DISK    USROPN
001200010808     FECEVT0XL  IF   E           K DISK    USROPN PREFIX(x_) RENAME(ECEVT:EVT)
001300010807     F*---
001400010807     F* Tariffe
001500010807     F*---
001600010807     FECEVD01L  UF A E           K DISK
001700010808     F*---
001800010808     F* Maggiorazioni
001900010808     F*---
002000010808     FECEVM01L  UF A E           K DISK
002100010808     D*------------------------------------------------------------------------*
002200010808     D* SCHIERE
002300010808     D*------------------------------------------------------------------------*
002400010808     D*------------
002500010808     D* COMANDI
002600010808     D*------------
002700010808     D cmd             S             80    DIM(1) CTDATA PERRCD(1)
002800991117     D*------------------------------------------------------------------------*
002900991117     D* DATA STRUCTURE
003000991117     D*------------------------------------------------------------------------*
003100991117     D*-------------------
003200991117     D* CONTROLLO DATA (8)
003300991117     D*-------------------
003400991117     D WLBDA8          DS                  INZ
003500991117     D  G08DAT                 1      8  0
003600991117     D  G08INV                 9     16  0
003700991117     D  G08ERR                17     17
003800991117     D  G08TGI                18     22  0
003900000000     C*------------------------------------------------------------------------*
004000000000     C* MAIN LINE
004100000000     C*------------------------------------------------------------------------*
004200991117     C*---
004300010807     C* VOCI E TARIFFE
004400991117     C*---
004500010807     C* -FA SCADERE AL 31/12/2001 LE TARIFFE DI TUTTE LE VOCI ATTIVE ALLA DATA
004600010807     C*  CORRENTE
004700010807     C* -CREA NUOVE TARIFFE CON DECORRENZA 01/01/2002 E SCADENZA 31/12/2009
004800010807     C*  CON NUOVO PROGRESSIVO E
004900010807     C*  CONVERTENDO IN EURO (/1936,27) TUTTI GLI IMPORTI (ITR, IMI, IMA)
005000010807     C*  E GLI SCAGLIONI (SGI, SGF) DELLE VOCI CON EVTTIV=21/22/23/25
005100010808     C*
005200010808     C                   EXSR      EXEVOC
005300010808     C*
005400010808     C* INFINE CHIUDO I FILE APERTI IN PRECEDENZA
005500010808     C                   CLOSE     ECEVT01L
005600010808     C                   CLOSE     ECEVT0XL
005700010808     C*
005800010808     C                   SETON                                            LR
005900010808     C*------------------------------------------------------------------------*
006000010808     C* EXEVOC - OPERAZIONI SU VOCI
006100010808     C*------------------------------------------------------------------------*
006200010808     C     EXEVOC        BEGSR
006300010808     C*
006400010808     C*
006500010808     C*  V O C I
006600010808     C*
006700010808     C*
006800010808     C     *loval        SETLL     ECEVT01L
006900010808     C                   READ      ECEVT01L
007000010808     C*
007100010808     C                   DOW       not %eof(ECEVT01L)
007200010808     C*
007300010808     C                   IF        EVTATB = *BLANKS and
007400010808     C                             EVTDDV <= DATCOR and
007500010808     C                             EVTDSV >= DATCOR
007600010808     C* MODIFICA SCADENZA VOCE CORRENTE
007700010808     C                   Z-ADD     20011231      EVTDSV
007800010808     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
007900010810     C                   MOVEL     *BLANKS       EVTFTR
008000010808     C* ESEGUO AGGIONAMENTO VOCE CORRENTE
008100010808     C                   UPDATE    ECEVT
008200010808     C* DOPO AVER AGIORNATO LA VOCE CORRENTE ANALOGAMENTE EFFETTUO TALI
008300010808     C* OPERAZIONI ANCHE PER GLI ARCHIVI CORRELATI, OVVERO...
008400010808     C*
008500010808     C* ESEGUO MEDESIME OPERAZIONI PER TUTTE TARIFFE CORRELATE
008600010808     C                   EXSR      EXETAR
008700010808     C                   IF        EVTTVO = 'M'
008800010808     C* X VOCE TIPO 'M' ESEGUO ESEGUO ANCHE PER TUTTE MAGGIORAZIONI CORRELATE
008900010808     C                   EXSR      EXEMAG
009000010808     C                   ENDIF
009100010808     C*
009200010808     C* DOPODICHE' INSERISCO NUOVA VOCE COM:
009300010808     C* NUOVO PROGRESSIVO
009400010808     C                   EVAL      EVTPRG = EVTPRG + 1
009500010808     C* NUOVA DECORRENZA VOCE
009600010808     C                   Z-ADD     20020101      EVTDDV
009700010808     C* NUOVA SCADENZA VOCE
009800010808     C                   Z-ADD     20091231      EVTDSV
009900010808     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
010000010810     C                   Z-ADD     datcor        EVTDTR
010100010810     C                   MOVEL     'T'           EVTFTR
010200010808     C* ESEGUO INSERIMENTO NUOVA TARIFFA
010300010808     C                   WRITE     ECEVT
010400010808     C                   ENDIF
010500010808     C*
010600010808     C                   READ      ECEVT01L
010700010808     C*
010800010808     C                   ENDDO
010900010808     C*
011000010808     C                   ENDSR
011100010808     C*------------------------------------------------------------------------*
011200010808     C* EXETAR - OPERAZIONI SU TARIFFE
011300010808     C*------------------------------------------------------------------------*
011400010808     C     EXETAR        BEGSR
011500010808     C*
011600010807     C*
011700010807     C*  T A R I F F E
011800010807     C*
011900010807     C*
012000010808     C     KEYEVD        SETLL     ECEVD01L
012100010808     C     KEYEVD        READE     ECEVD01L
012200010808     C*
012300010807     C                   DOW       not %eof(ECEVD01L)
012400010808     C*
012500010807     C                   IF        EVDATB = *BLANKS and
012600010807     C                             EVDDDT <= DATCOR and
012700010807     C                             EVDDST >= DATCOR
012800010807     C* MODIFICA SCADENZA TARIFFA CORRENTE
012900010807     C                   Z-ADD     20011231      EVDDST
013000010807     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
013100010810     C                   MOVEL     *BLANKS       EVDFTR
013200010808     C* VALORIZZO LA DATA DI ULTIMA MODIFICA
013300010808     C                   Z-ADD     DATCOR        EVDDUV
013400010807     C* ESEGUO AGGIONAMENTO TARIFFA CORRENTE
013500010807     C                   UPDATE    ECEVD000
013600010807     C*
013700010807     C* DOPODICHE' INSERISCO NUOVA TARIFFA IN EURO
013800010807     C* "EURIZZAZIONE" DEGLI IMPORTI
013900010807     C                   EVAL      EVDITR = EVDITR / 1936,27
014000010807     C                   EVAL      EVDIMI = EVDIMI / 1936,27
014100010807     C                   EVAL      EVDIMA = EVDIMA / 1936,27
014200010809     C* PER TUTTE LE TARIFFE PER MASSIMO SCAGLIONE SUPERIORE INDICO TUTTI 9
014300010809     C                   IF        EVDSGF > 9999999998
014400010809     C                   EVAL      EVDSGF = 9999999999,999
014500010809     C                   ENDIF
014600010807     C* PER DETERMINATE TARIFFE "EURIZZO" ANCHE GLI SCAGLIONI
014700010807     C                   IF        EVTTIV = '21' or
014800010807     C                             EVTTIV = '22' or
014900010807     C                             EVTTIV = '23' or
015000010807     C                             EVTTIV = '25'
015100010809     C                   IF        EVDSGI > 0
015200010807     C                   EVAL      EVDSGI = EVDSGI / 1936,27
015300010809     C                   ENDIF
015400010809     C                   IF        EVDSGF > 9999999998
015500010809     C                   EVAL      EVDSGF = 9999999999,999
015600010809     C                   ELSE
015700010807     C                   EVAL      EVDSGF = EVDSGF / 1936,27
015800010807     C                   ENDIF
015900010809     C                   ENDIF
016000010807     C* NUOVO PROGRESSIVO
016100010807     C                   EVAL      EVDPRG = EVDPRG + 1
016200010807     C* DECORRENZA TARIFFA
016300010807     C                   Z-ADD     20020101      EVDDDT
016400010807     C* SCADENZA TARIFFA
016500010807     C                   Z-ADD     20091231      EVDDST
016600010807     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
016700010810     C                   Z-ADD     datcor        EVDDTR
016800010810     C                   MOVEL     'T'           EVDFTR
016900010808     C* VALORIZZO LA DATA DI INSERIMENTO
017000010810     C                   Z-ADD     *zeros        EVDDUV
017100010808     C                   Z-ADD     DATCOR        EVDDIR
017200010807     C* ESEGUO INSERIMENTO NUOVA TARIFFA
017300010807     C                   WRITE     ECEVD000
017400010807     C                   ENDIF
017500010808     C*
017600010808     C     KEYEVD        READE     ECEVD01L
017700010808     C*
017800010807     C                   ENDDO
017900010808     C*
018000010808     C                   ENDSR
018100010808     C*------------------------------------------------------------------------*
018200010808     C* EXEMAG - OPERAZIONI SU TARIFFE
018300010808     C*------------------------------------------------------------------------*
018400010808     C     EXEMAG        BEGSR
018500010808     C*
018600010808     C*
018700010808     C*  M A G G I O R A Z I O N I
018800010808     C*
018900010808     C*
019000010808     C     KEYEVM        SETLL     ECEVM01L
019100010808     C     KEYEVM        READE     ECEVM01L
019200010808     C*
019300010808     C                   DOW       not %eof(ECEVM01L)
019400010808     C*
019500010808     C* VERIFICO CHE LA VOCE IN MAGGIORAZIONE SIA ANCH'ESSA VALIDA
019600010808     C* DOPODICHE' INSERISCO NUOVA MAGGIORAZIONE CON NUOVO PROGRESSIVO
019700010808     C* CORRELATO ALLE VOCI/PROGRESSIVO APPENA INSERITE
019800010808     C     KEYEVT        CHAIN     ECEVT0XL
019900010808     C                   IF        %found(ECEVT0XL)
020000010808     C                   IF        x_EVTATB = *BLANKS and
020100010808     C                             x_EVTDDV <= DATCOR and
020200010808     C                             x_EVTDSV >= DATCOR
020300010808     C* NUOVO PROGRESSIVI
020400010808     C                   EVAL      EVMPRM = EVMPRM + 1
020500010808     C                   EVAL      EVMPRG = EVMPRG + 1
020600010808     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
020700010810     C                   Z-ADD     datcor        EVMDTR
020800010810     C                   MOVEL     'T'           EVMFTR
020900010808     C* ESEGUO INSERIMENTO NUOVA MAGGIORAZIONE
021000010808     C                   WRITE     ECEVM
021100010808     C                   ENDIF
021200010808     C                   ENDIF
021300010808     C*
021400010808     C     KEYEVM        READE     ECEVM01L
021500010808     C*
021600010808     C                   ENDDO
021700010808     C*
021800010808     C                   ENDSR
021900000000     C*------------------------------------------------------------------------*
022000000000     C* *INZSR - OPERAZIONI INIZIALI
022100000000     C*------------------------------------------------------------------------*
022200000000     C     *INZSR        BEGSR
022300991117     C*
022400010808     C* DEFINIZIONE CHIAVI - ECEVD01L
022500010808     C     KEYEVD        KLIST
022600010808     C                   KFLD                    EVTVOC
022700010808     C                   KFLD                    EVTPRG
022800010808     C* DEFINIZIONE CHIAVI - ECEVM01L
022900010808     C     KEYEVM        KLIST
023000010808     C                   KFLD                    EVTVOC
023100010808     C                   KFLD                    EVTPRG
023200010808     C* DEFINIZIONE CHIAVI - ECEVT0XL
023300010808     C     KEYEVT        KLIST
023400010808     C                   KFLD                    EVMVOC
023500010808     C                   KFLD                    EVMPRG
023600991117     C*
023700991117     C* IMPOSTA DATA E ORA CORRENTE
023800991117     C                   TIME                    N14              14 0
023900991117     C                   MOVE      N14           N8                8 0
024000991117     C                   Z-ADD     N8            G08DAT
024100991117     C                   Z-ADD     *ZEROS        G08INV
024200991117     C                   MOVEL     '0'           G08ERR
024300991117     C                   CALL      'XSRDA8'
024400991117     C                   PARM                    WLBDA8
024500991117     C                   Z-ADD     G08INV        DATCOR            8 0
024600991119     C                   MOVE      00            DATCO0            8 0
024700010808     C*
024800010808     C* OVRDBF PER LETTURA SECONDA CHIAVE DI INPUT
024900010808     C                   Z-ADD     80            LENGH            15 5
025000010808     C                   CALL      'QCMDEXC'
025100010808     C                   PARM                    CMD(1)
025200010808     C                   PARM                    LENGH
025300010808     C*
025400010808     C* DOPO DI CHE' APRO I FILE VOCI
025500010808     C                   OPEN      ECEVT01L
025600010808     C                   OPEN      ECEVT0XL
025700991117     C*
025800000000     C                   ENDSR
025900010808** CMD - COMANDI CL
026000010808OVRDBF FILE(ECEVT0XL) TOFILE(ECEVT01L)
