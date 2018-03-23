000100020411     H*--------------------------------------------------------------*
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400011026     FTEETC01L  UF   E           K DISK
000500011026     FTEETT01L  UF   E           K DISK
000600011026     FTEETS01L  UF   E           K DISK
000700020723     FTNTAM01L  IF   E           K DISK
000800020723     FTNOFM01L  IF   E           K DISK    rename(tntam000:tnofm000)
000900040728     FPRTF198   O    F  198        PRINTER
001000020411     D*-------------
001100020411     D* DS DI WRK X LISTING DELLE CANCELAZIONI
001200020411     D*-------------
001300020411     D prtriga         DS
001400020411     D FILE                           3
001500020411     D XXXSP1                         1
001600020411     D WRKDSF                         1
001700020411     D XXXSP2                         1
001800020411     D WRKCTO                         1
001900020411     D XXXSP3                         1
002000020411     D TAMKSC                         7  0
002100020411     D XXXSP4                         1
002200020411     D TAMCTR                         3  0
002300020411     D XXXSP5                         1
002400020411     D TAMPRG                         3  0
002500020411     D XXXSP6                         1
002600020411     C*
002700020411     D prttot          DS
002800020411     D CONSTTOT                       3    INZ('TOT')
002900020411     D XXXSP7                         1
003000020411     D FILETOT                        3
003100020411     D XXXSP8                         1
003200040728     D CONTAREC                       9  0
003300000000     C*---------------------------------------------------------------*
003400970114     C*
003500020411     C* ELABORA TEETC
003600040728     C                   Z-ADD     *zeros        CONTAREC
003700020724     C     *loval        SETLL     TEETC01L
003800020724     C                   READ      TEETC01L
003900020411     C                   DOW       not %eof(TEETC01L)
004000020723     C*
004100020723     C* SE IL RECORD DATI TIPO DI TIPO 'O'
004200020723     C                   IF        ETCCTO = 'O'
004300020411     C* VERIFICO CONDIZIONE DI CANCELLAZIONE
004400020411     C                   EVAL      TAMKSC = ETCKSC
004500020411     C                   EVAL      TAMCTR = ETCCTR
004600020411     C                   EVAL      TAMPRG = ETCPRG
004700020723     C                   EXSR      CHKRECOFF
004800020411     C                   IF        WRECDEL = 'S'
004900020411     C* LISTO L'OPERAZIONE DI CANCELLAZIONE
005000020411     C                   EVAL      FILE    = 'ETC'
005100040728     C*                  EXCEPT    listato
005200020514     C                   DELETE    TEETC01L
005300020411     C                   ADD       1             CONTAREC
005400020411     C                   ENDIF
005500020411     C                   ENDIF
005600020723     C*
005700020723     C* SE IL RECORD DATI TIPO DI TIPO 'T'
005800020723     C                   IF        ETCCTO = 'T'
005900020723     C* VERIFICO CONDIZIONE DI CANCELLAZIONE
006000020723     C                   EVAL      TAMKSC = ETCKSC
006100020723     C                   EVAL      TAMCTR = ETCCTR
006200020723     C                   EVAL      TAMPRG = ETCPRG
006300020723     C                   EXSR      CHKRECTAR
006400020723     C                   IF        WRECDEL = 'S'
006500020723     C* LISTO L'OPERAZIONE DI CANCELLAZIONE
006600020723     C                   EVAL      FILE    = 'ETC'
006700040728     C*                  EXCEPT    listato
006800020723     C                   DELETE    TEETC01L
006900020723     C                   ADD       1             CONTAREC
007000020723     C                   ENDIF
007100020723     C                   ENDIF
007200020724     C                   READ      TEETC01L
007300020411     C                   ENDDO
007400020411     C* LISTO IL TOTALE RECORD DI TEETC CANCELLATI
007500020723     C                   EVAL      FILETOT = 'ETC'
007600040728     C                   EXCEPT    listatoT
007700020411     C*
007800020411     C* ELABORA TEETS
007900020411     C                   Z-ADD     *zeros        CONTAREC
008000020724     C     *loval        SETLL     TEETS01L
008100020724     C                   READ      TEETS01L
008200020411     C                   DOW       not %eof(TEETS01L)
008300020723     C*
008400020723     C* SE IL RECORD DATI TIPO DI TIPO 'O'
008500020723     C                   IF        ETSCTO = 'O'
008600020411     C* VERIFICO CONDIZIONE DI CANCELLAZIONE
008700020411     C                   EVAL      TAMKSC = ETSKSC
008800020411     C                   EVAL      TAMCTR = ETSCTR
008900020411     C                   EVAL      TAMPRG = ETSPRG
009000020723     C                   EXSR      CHKRECOFF
009100020411     C                   IF        WRECDEL = 'S'
009200020411     C* LISTO L'OPERAZIONE DI CANCELLAZIONE
009300020411     C                   EVAL      FILE    = 'ETS'
009400040728     C*                  EXCEPT    listato
009500020514     C                   DELETE    TEETS01L
009600020411     C                   ADD       1             CONTAREC
009700020411     C                   ENDIF
009800020411     C                   ENDIF
009900020723     C*
010000020723     C* SE IL RECORD DATI TIPO DI TIPO 'T'
010100020723     C                   IF        ETSCTO = 'T'
010200020723     C* VERIFICO CONDIZIONE DI CANCELLAZIONE
010300020723     C                   EVAL      TAMKSC = ETSKSC
010400020723     C                   EVAL      TAMCTR = ETSCTR
010500020723     C                   EVAL      TAMPRG = ETSPRG
010600020723     C                   EXSR      CHKRECTAR
010700020723     C                   IF        WRECDEL = 'S'
010800020723     C* LISTO L'OPERAZIONE DI CANCELLAZIONE
010900020723     C                   EVAL      FILE    = 'ETS'
011000040728     C*                  EXCEPT    listato
011100020723     C                   DELETE    TEETS01L
011200020723     C                   ADD       1             CONTAREC
011300020723     C                   ENDIF
011400020723     C                   ENDIF
011500020724     C                   READ      TEETS01L
011600020411     C                   ENDDO
011700020411     C* LISTO IL TOTALE RECORD DI TEETS CANCELLATI
011800020723     C                   EVAL      FILETOT = 'ETS'
011900020411     C                   EXCEPT    listatoT
012000020411     C*
012100020411     C* ELABORA TEETT
012200020411     C                   Z-ADD     *zeros        CONTAREC
012300020724     C     *loval        SETLL     TEETT01L
012400020724     C                   READ      TEETT01L
012500020411     C                   DOW       not %eof(TEETT01L)
012600020723     C*
012700020723     C* SE IL RECORD DATI TIPO DI TIPO 'O'
012800020723     C                   IF        ETTCTO = 'O'
012900020411     C* VERIFICO CONDIZIONE DI CANCELLAZIONE
013000020411     C                   EVAL      TAMKSC = ETTKSC
013100020411     C                   EVAL      TAMCTR = ETTCTR
013200020411     C                   EVAL      TAMPRG = ETTPRG
013300020723     C                   EXSR      CHKRECOFF
013400020411     C                   IF        WRECDEL = 'S'
013500020411     C* LISTO L'OPERAZIONE DI CANCELLAZIONE
013600020411     C                   EVAL      FILE    = 'ETT'
013700040728     C*                  EXCEPT    listato
013800020514     C                   DELETE    TEETT01L
013900020411     C                   ADD       1             CONTAREC
014000020411     C                   ENDIF
014100020411     C                   ENDIF
014200020723     C*
014300020723     C* SE IL RECORD DATI TIPO DI TIPO 'T'
014400020723     C                   IF        ETTCTO = 'T'
014500020723     C* VERIFICO CONDIZIONE DI CANCELLAZIONE
014600020723     C                   EVAL      TAMKSC = ETTKSC
014700020723     C                   EVAL      TAMCTR = ETTCTR
014800020723     C                   EVAL      TAMPRG = ETTPRG
014900020723     C                   EXSR      CHKRECTAR
015000020723     C                   IF        WRECDEL = 'S'
015100020723     C* LISTO L'OPERAZIONE DI CANCELLAZIONE
015200020723     C                   EVAL      FILE    = 'ETT'
015300040728     C*                  EXCEPT    listato
015400020723     C                   DELETE    TEETT01L
015500020723     C                   ADD       1             CONTAREC
015600020723     C                   ENDIF
015700020723     C                   ENDIF
015800020724     C                   READ      TEETT01L
015900020411     C                   ENDDO
016000020411     C* LISTO IL TOTALE RECORD DI TEETS CANCELLATI
016100020723     C                   EVAL      FILETOT = 'ETT'
016200020411     C                   EXCEPT    listatoT
016300020411     C*
016400020411     C                   SETON                                        LR
016500011026     C*------------------------------------------------------------------------*
016600020723     C* CHKRECOFF - VERIFICA SE IL RECORD E' DA CANCELLARE RISPETTO ALLE OFFERTE
016700011026     C*------------------------------------------------------------------------*
016800020723     C     CHKRECOFF     BEGSR
016900011026     C*
017000020411     C* INIZIALIZZO A "RECORD DA NN CANCELLARE" IL FLAG
017100020411     C                   MOVEL     'N'           WRECDEL           1
017200020411     C*
017300020411     C* VERIFICO MANCANZA OFFERTA SUL FLE OFFERTE
017400020723     C     KEYT_O        CHAIN     TNOFM01L
017500020411     C                   IF        not %found(TNOFM01L)
017600020411     C                   MOVEL     'S'           WRECDEL
017700020411     C                   ENDIF
017800011026     C*
017900011026     C                   ENDSR
018000020723     C*------------------------------------------------------------------------*
018100020723     C* CHKRECTAR - VERIFICA SE IL RECORD E' DA CANCELLARE RISPETTO ALLE TARIFFE
018200020723     C*------------------------------------------------------------------------*
018300020723     C     CHKRECTAR     BEGSR
018400020723     C*
018500020723     C* INIZIALIZZO A "RECORD DA NN CANCELLARE" IL FLAG
018600020723     C                   MOVEL     'N'           WRECDEL           1
018700020723     C*
018800020723     C* VERIFICO MANCANZA OFFERTA SUL FLE TARIFFE
018900020723     C     KEYT_O        CHAIN     TNTAM01L
019000020723     C                   IF        not %found(TNTAM01L)
019100020723     C                   MOVEL     'S'           WRECDEL
019200020723     C                   ENDIF
019300020723     C*
019400020723     C                   ENDSR
019500020411     C*------------------------------------------------------------------------*
019600020411     C* *INZSR - ROUNTINE DI *INIT DEL *PGM
019700020411     C*------------------------------------------------------------------------*
019800970115     C     *INZSR        BEGSR
019900020411     C*
020000020723     C* ACCESSO TNTAM01L/TNOFM01L
020100020723     C     KEYT_O        KLIST
020200020411     C                   KFLD                    TAMKSC
020300020411     C                   KFLD                    TAMCTR
020400020411     C                   KFLD                    TAMPRG
020500970115     C*
020600970115     C                   ENDSR
020700020411
020800020411     OPRTF198   E            listato     2
020900020411     O                       prtriga
021000020411     OPRTF198   E            listatoT    2
021100020411     O                       prttot
