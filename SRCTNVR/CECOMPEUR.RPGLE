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
004000011123     C
004100000000     C*------------------------------------------------------------------------*
004200991117     C*---
004300010807     C* VOCI E TARIFFE
004400991117     C*---
004500011121     C*
004600011121     C* - CANCELLO INNANZITUTTO LE MAGGIORAZOINI LEGATE A TARIFFE CON DECORRENZA 2002
004700011121     C                   EXSR      DLTMAG
004800011121     C* - CANCELLO QUINDI LE VOCI CON DECORRENZA 2002
004900011121     C                   EXSR      DLTVOC
005000011121     C* - CANCELLO QUINDI LE TARIFFE CON DECORRENZA 2002
005100011121     C                   EXSR      DLTTAR
005200011121     C*
005300010807     C* -FA SCADERE AL 31/12/2001 LE TARIFFE DI TUTTE LE VOCI ATTIVE ALLA DATA
005400010807     C*  CORRENTE
005500010807     C* -CREA NUOVE TARIFFE CON DECORRENZA 01/01/2002 E SCADENZA 31/12/2009
005600011121     C*  CON NUOVO PROGRESSIVO
005700010808     C*
005800011123     C                   EXSR      EXEVOC
005900010808     C*
006000010808     C* INFINE CHIUDO I FILE APERTI IN PRECEDENZA
006100010808     C                   CLOSE     ECEVT01L
006200010808     C                   CLOSE     ECEVT0XL
006300010808     C*
006400010808     C                   SETON                                            LR
006500011121     C*------------------------------------------------------------------------*
006600011121     C* DLTMAG - OPERAZIONI SU MAGGIORAZIONI
006700011121     C*------------------------------------------------------------------------*
006800011121     C     DLTMAG        BEGSR
006900011121     C*
007000011121     C*
007100011121     C*  M A G G I O R A Z I O N I
007200011121     C*
007300011121     C*
007400011121     C     *LOVAL        SETLL     ECEVM01L
007500011121     C                   READ      ECEVM01L
007600011121     C*
007700011121     C                   DOW       not %eof(ECEVM01L)
007800011121     C*
007900011121     C* VERIFICO CHE LA MAGGIORAZIONE SIA LEGATA A UNA TARIFFA AVENTE DECORRENZA 2002
008000011121     C* DOPODICHE' SE SI => LA CANCELLO
008100011121     C     KEYEVT        CHAIN     ECEVT0XL
008200011121     C                   IF        %found(ECEVT0XL)
008300011121     C                   IF        x_EVTDDV >= 20020101
008400011121     C* DELETO MAGGIORAZIONE
008500011121     C                   DELETE    ECEVM
008600011121     C                   ENDIF
008700011121     C                   ENDIF
008800011121     C*
008900011121     C                   READ      ECEVM01L
009000011121     C*
009100011121     C                   ENDDO
009200011121     C*
009300011121     C                   ENDSR
009400011121     C*------------------------------------------------------------------------*
009500011121     C* DLTVOC - OPERAZIONI SU VOCI
009600011121     C*------------------------------------------------------------------------*
009700011121     C     DLTVOC        BEGSR
009800011121     C*
009900011121     C*
010000011121     C*  V O C I
010100011121     C*
010200011121     C*
010300011121     C     *LOVAL        SETLL     ECEVD01L
010400011121     C                   READ      ECEVD01L
010500011121     C*
010600011121     C                   DOW       not %eof(ECEVD01L)
010700011121     C*
010800011121     C* VERIFICO CHE LA VOCE ABBIA DECORRENZA 2002
010900011121     C* DOPODICHE' SE SI => LA CANCELLO
011000011121     C                   IF        EVDDDT >= 20020101
011100011121     C* DELETO VOCE
011200011121     C                   DELETE    ECEVD000
011300011121     C                   ENDIF
011400011121     C*
011500011121     C                   READ      ECEVD01L
011600011121     C*
011700011121     C                   ENDDO
011800011121     C*
011900011121     C                   ENDSR
012000011121     C*------------------------------------------------------------------------*
012100011121     C* DLTTAR - OPERAZIONI SU TARIFFE
012200011121     C*------------------------------------------------------------------------*
012300011121     C     DLTTAR        BEGSR
012400011121     C*
012500011121     C*
012600011121     C*  T A R I F F E
012700011121     C*
012800011121     C*
012900011121     C     *LOVAL        SETLL     ECEVT01L
013000011121     C                   READ      ECEVT01L
013100011121     C*
013200011121     C                   DOW       not %eof(ECEVT01L)
013300011121     C*
013400011121     C* VERIFICO CHE LA TARIFFA ABBIA DECORRENZA 2002
013500011121     C* DOPODICHE' SE SI => LA CANCELLO
013600011121     C                   IF        EVTDDV >= 20020101
013700011121     C* DELETO TARIFFA
013800011121     C                   DELETE    ECEVT
013900011121     C                   ENDIF
014000011121     C*
014100011121     C                   READ      ECEVT01L
014200011121     C*
014300011121     C                   ENDDO
014400011121     C*
014500011121     C                   ENDSR
014600010808     C*------------------------------------------------------------------------*
014700010808     C* EXEVOC - OPERAZIONI SU VOCI
014800010808     C*------------------------------------------------------------------------*
014900010808     C     EXEVOC        BEGSR
015000010808     C*
015100010808     C*
015200010808     C*  V O C I
015300010808     C*
015400010808     C*
015500010808     C     *loval        SETLL     ECEVT01L
015600010808     C                   READ      ECEVT01L
015700010808     C*
015800010808     C                   DOW       not %eof(ECEVT01L)
015900010808     C*
016000010808     C                   IF        EVTATB = *BLANKS and
016100010808     C                             EVTDDV <= DATCOR and
016200010808     C                             EVTDSV >= DATCOR
016300010808     C* MODIFICA SCADENZA VOCE CORRENTE
016400011122     C                   IF        EVTDSV > 20011231
016500010808     C                   Z-ADD     20011231      EVTDSV
016600010808     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
016700010810     C                   MOVEL     *BLANKS       EVTFTR
016800010808     C* ESEGUO AGGIONAMENTO VOCE CORRENTE
016900010808     C                   UPDATE    ECEVT
017000011122     C                   ENDIF
017100010808     C* DOPO AVER AGIORNATO LA VOCE CORRENTE ANALOGAMENTE EFFETTUO TALI
017200010808     C* OPERAZIONI ANCHE PER GLI ARCHIVI CORRELATI, OVVERO...
017300010808     C*
017400010808     C* ESEGUO MEDESIME OPERAZIONI PER TUTTE TARIFFE CORRELATE
017500010808     C                   EXSR      EXETAR
017600010808     C                   IF        EVTTVO = 'M'
017700010808     C* X VOCE TIPO 'M' ESEGUO ESEGUO ANCHE PER TUTTE MAGGIORAZIONI CORRELATE
017800010808     C                   EXSR      EXEMAG
017900010808     C                   ENDIF
018000010808     C*
018100010808     C* DOPODICHE' INSERISCO NUOVA VOCE COM:
018200010808     C* NUOVO PROGRESSIVO
018300010808     C                   EVAL      EVTPRG = EVTPRG + 1
018400010808     C* NUOVA DECORRENZA VOCE
018500010808     C                   Z-ADD     20020101      EVTDDV
018600010808     C* NUOVA SCADENZA VOCE
018700010808     C                   Z-ADD     20091231      EVTDSV
018800010808     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
018900010810     C                   Z-ADD     datcor        EVTDTR
019000010810     C                   MOVEL     'T'           EVTFTR
019100010808     C* ESEGUO INSERIMENTO NUOVA TARIFFA
019200010808     C                   WRITE     ECEVT
019300010808     C                   ENDIF
019400010808     C*
019500010808     C                   READ      ECEVT01L
019600010808     C*
019700010808     C                   ENDDO
019800010808     C*
019900010808     C                   ENDSR
020000010808     C*------------------------------------------------------------------------*
020100010808     C* EXETAR - OPERAZIONI SU TARIFFE
020200010808     C*------------------------------------------------------------------------*
020300010808     C     EXETAR        BEGSR
020400010808     C*
020500010807     C*
020600010807     C*  T A R I F F E
020700010807     C*
020800010807     C*
020900010808     C     KEYEVD        SETLL     ECEVD01L
021000010808     C     KEYEVD        READE     ECEVD01L
021100010808     C*
021200010807     C                   DOW       not %eof(ECEVD01L)
021300010808     C*
021400010807     C                   IF        EVDATB = *BLANKS and
021500010807     C                             EVDDDT <= DATCOR and
021600010807     C                             EVDDST >= DATCOR
021700010807     C* MODIFICA SCADENZA TARIFFA CORRENTE
021800011122     C                   IF        EVDDST > 20011231
021900010807     C                   Z-ADD     20011231      EVDDST
022000010807     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
022100010810     C                   MOVEL     *BLANKS       EVDFTR
022200010808     C* VALORIZZO LA DATA DI ULTIMA MODIFICA
022300010808     C                   Z-ADD     DATCOR        EVDDUV
022400010807     C* ESEGUO AGGIONAMENTO TARIFFA CORRENTE
022500010807     C                   UPDATE    ECEVD000
022600011122     C                   ENDIF
022700010807     C* NUOVO PROGRESSIVO
022800010807     C                   EVAL      EVDPRG = EVDPRG + 1
022900010807     C* DECORRENZA TARIFFA
023000010807     C                   Z-ADD     20020101      EVDDDT
023100010807     C* SCADENZA TARIFFA
023200010807     C                   Z-ADD     20091231      EVDDST
023300010807     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
023400010810     C                   Z-ADD     datcor        EVDDTR
023500010810     C                   MOVEL     'T'           EVDFTR
023600010808     C* VALORIZZO LA DATA DI INSERIMENTO
023700010810     C                   Z-ADD     *zeros        EVDDUV
023800010808     C                   Z-ADD     DATCOR        EVDDIR
023900010807     C* ESEGUO INSERIMENTO NUOVA TARIFFA
024000010807     C                   WRITE     ECEVD000
024100010807     C                   ENDIF
024200010808     C*
024300010808     C     KEYEVD        READE     ECEVD01L
024400010808     C*
024500010807     C                   ENDDO
024600010808     C*
024700010808     C                   ENDSR
024800010808     C*------------------------------------------------------------------------*
024900010808     C* EXEMAG - OPERAZIONI SU TARIFFE
025000010808     C*------------------------------------------------------------------------*
025100010808     C     EXEMAG        BEGSR
025200010808     C*
025300010808     C*
025400010808     C*  M A G G I O R A Z I O N I
025500010808     C*
025600010808     C*
025700010808     C     KEYEVM        SETLL     ECEVM01L
025800010808     C     KEYEVM        READE     ECEVM01L
025900010808     C*
026000010808     C                   DOW       not %eof(ECEVM01L)
026100010808     C*
026200010808     C* VERIFICO CHE LA VOCE IN MAGGIORAZIONE SIA ANCH'ESSA VALIDA
026300010808     C* DOPODICHE' INSERISCO NUOVA MAGGIORAZIONE CON NUOVO PROGRESSIVO
026400010808     C* CORRELATO ALLE VOCI/PROGRESSIVO APPENA INSERITE
026500010808     C     KEYEVT        CHAIN     ECEVT0XL
026600010808     C                   IF        %found(ECEVT0XL)
026700010808     C                   IF        x_EVTATB = *BLANKS and
026800010808     C                             x_EVTDDV <= DATCOR and
026900010808     C                             x_EVTDSV >= DATCOR
027000010808     C* NUOVO PROGRESSIVI
027100010808     C                   EVAL      EVMPRM = EVMPRM + 1
027200010808     C                   EVAL      EVMPRG = EVMPRG + 1
027300010808     C* SFLEGGAMENTO CAMPI X TRASMISSIONE
027400010810     C                   Z-ADD     datcor        EVMDTR
027500010810     C                   MOVEL     'T'           EVMFTR
027600010808     C* ESEGUO INSERIMENTO NUOVA MAGGIORAZIONE
027700010808     C                   WRITE     ECEVM
027800010808     C                   ENDIF
027900010808     C                   ENDIF
028000010808     C*
028100010808     C     KEYEVM        READE     ECEVM01L
028200010808     C*
028300010808     C                   ENDDO
028400010808     C*
028500010808     C                   ENDSR
028600000000     C*------------------------------------------------------------------------*
028700000000     C* *INZSR - OPERAZIONI INIZIALI
028800000000     C*------------------------------------------------------------------------*
028900000000     C     *INZSR        BEGSR
029000991117     C*
029100010808     C* DEFINIZIONE CHIAVI - ECEVD01L
029200010808     C     KEYEVD        KLIST
029300010808     C                   KFLD                    EVTVOC
029400010808     C                   KFLD                    EVTPRG
029500010808     C* DEFINIZIONE CHIAVI - ECEVM01L
029600010808     C     KEYEVM        KLIST
029700010808     C                   KFLD                    EVTVOC
029800010808     C                   KFLD                    EVTPRG
029900010808     C* DEFINIZIONE CHIAVI - ECEVT0XL
030000010808     C     KEYEVT        KLIST
030100010808     C                   KFLD                    EVMVOC
030200010808     C                   KFLD                    EVMPRG
030300991117     C*
030400991117     C* IMPOSTA DATA E ORA CORRENTE
030500991117     C                   TIME                    N14              14 0
030600991117     C                   MOVE      N14           N8                8 0
030700991117     C                   Z-ADD     N8            G08DAT
030800991117     C                   Z-ADD     *ZEROS        G08INV
030900991117     C                   MOVEL     '0'           G08ERR
031000991117     C                   CALL      'XSRDA8'
031100991117     C                   PARM                    WLBDA8
031200991117     C                   Z-ADD     G08INV        DATCOR            8 0
031300991119     C                   MOVE      00            DATCO0            8 0
031400010808     C*
031500010808     C* OVRDBF PER LETTURA SECONDA CHIAVE DI INPUT
031600010808     C                   Z-ADD     80            LENGH            15 5
031700010808     C                   CALL      'QCMDEXC'
031800010808     C                   PARM                    CMD(1)
031900010808     C                   PARM                    LENGH
032000010808     C*
032100010808     C* DOPO DI CHE' APRO I FILE VOCI
032200010808     C                   OPEN      ECEVT01L
032300010808     C                   OPEN      ECEVT0XL
032400991117     C*
032500000000     C                   ENDSR
032600010808** CMD - COMANDI CL
032700010808OVRDBF FILE(ECEVT0XL) TOFILE(ECEVT01L)
