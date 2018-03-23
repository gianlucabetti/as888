000100000000     H*------------------------------------------------------------------------*
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*------------------------------------------------------------------------*
000400000000     F* DATA BASE
000500000000     F*------------------------------------------------------------------------*
000600020620     FECERR01L  IF   E           K DISK
000700020610     FTREC87P   O    E             PRINTER OFLIND(*IN01)
000800000000     D*------------------------------------------------------------------------*
000900000000     D* SCHIERE
001000000000     D*------------------------------------------------------------------------*
001100000000     D*------------------------------------------------------------------------*
001200000000     D* INPUT
001300000000     D*------------------------------------------------------------------------*
001400000000     D*-------------------
001500000000     D* ARCHITETTURA
001600000000     D*-------------------
001700000000     D KPJBA         E DS
001800000000     D*---------------------
001900000000     D* DS DI PROCEDURA
002000000000     D*---------------------
002100950323     D DS87          E DS                  EXTNAME(TREC87DS)
002200020610     D*---------------------
002300020610     D* DS RIDEFINIZIONE IMMAGINI "PRIMA" E "DOPO"
002400020610     D*---------------------
002500020610     D PTASDS        E DS                  EXTNAME(TITAS00F)
002600020610     D                                     PREFIX(P)
002700020610     D DTASDS        E DS                  EXTNAME(TITAS00F)
002800020610     D                                     PREFIX(D)
002900020619     D PCSBDS        E DS                  EXTNAME(TNCSB00F)
003000020619     D                                     PREFIX(P)
003100020619     D DCSBDS        E DS                  EXTNAME(TNCSB00F)
003200020619     D                                     PREFIX(D)
003300030123     D PAR5DS        E DS                  EXTNAME(FIAR500F)
003400030123     D                                     PREFIX(P)
003500030123     D DAR5DS        E DS                  EXTNAME(FIAR500F)
003600030123     D                                     PREFIX(D)
003700000000     C*------------------------------------------------------------------------*
003800000000     C* MAIN LINE
003900000000     C*------------------------------------------------------------------------*
004000000000     C*
004100020610     C* Innanzitutto reperisco il record dal file ECERR00F
004200020610     C                   EXSR      REPERR
004300020610     C*
004400020610     C* Effettuo la stampa
004500020610     C                   EXSR      STAERR
004600020610     C*
004700000000     C                   SETON                                        LR
004800020610     C*------------------------------------------------------------------------*
004900020610     C*  REPERR - REPERISCO IL RECORD DAL FILE ECERR00F
005000020610     C*------------------------------------------------------------------------*
005100020610     C     REPERR        BEGSR
005200020610     C*
005300020610     C                   MOVEL     'S'           WRECOK            1
005400020610     C*
005500020610     C     KEYERR        CHAIN     ECERR01L
005600020610     C                   IF        NOT %found(ECERR01L)
005700020610     C                   MOVEL     'N'           WRECOK
005800020610     C                   ENDIF
005900020610     C*
006000020610     C                   ENDSR
006100020610     C*------------------------------------------------------------------------*
006200020610     C*  STAERR - STAMPA L'ERRORE
006300020610     C*------------------------------------------------------------------------*
006400020610     C     STAERR        BEGSR
006500020610     C*
006600020620     C* Imposto l'indicatore di stampa dati di testata del file ECERR
006700020620     C                   SETON                                        55
006800020620     C*
006900020610     C                   IF        WRECOK = 'S'
007000020610     C*
007100020610     C* Gestisco errore su record di TITAS00F/TITAS10F
007200020610     C                   IF        ERRFIL = 'TITAS00F' OR
007300020610     C                             ERRFIL = 'TITAS10F'
007400020611     C*
007500020611     C* Inizializzo DS, variabili di WRK
007600020611     C                   CLEAR                   PTASDS
007700020611     C                   CLEAR                   DTASDS
007800020611     C                   MOVEL     *BLANKS       wrkPRIMA        500
007900020611     C                   MOVEL     *BLANKS       wrkDOPO         500
008000020611     C*
008100020611     C* Compongo le immagini "PRIMA" e "DOPO"
008200020611     C                   EVAL      wrkPRIMA = ERRRP1 + ERRRP2
008300020611     C                   EVAL      wrkDOPO  = ERRRD1 + ERRRD2
008400020611     C*
008500020611     C* Verifico presenza di valorizzazioni a *null
008600020611     C                   Z-ADD     *zeros        jPRIMA            3 0
008700020611     C                   Z-ADD     *zeros        lenPRIMA          3 0
008800020611     C                   EVAL      lenPRIMA = %len(%trim(wrkPRIMA))
008900020611     C     1             DO        lenPRIMA      j                 3 0
009000020611     C                   IF        %subst(wrkPRIMA:j:1) = X'00'
009100020611     C                   ADD       1             jPRIMA
009200020611     C                   ENDIF
009300020611     C                   ENDDO
009400020611     C*
009500020611     C                   Z-ADD     *zeros        jDOPO             3 0
009600020611     C                   Z-ADD     *zeros        lenDOPO           3 0
009700020611     C                   EVAL      lenDOPO = %len(%trim(wrkDOPO))
009800020611     C     1             DO        lenDOPO       j                 3 0
009900020611     C                   IF        %subst(wrkDOPO:j:1) = X'00'
010000020611     C                   ADD       1             jDOPO
010100020611     C                   ENDIF
010200020611     C                   ENDDO
010300020611     C*
010400020611     C* Muovo le immagini "PRIMA" e "DOPO" in stampa
010500020611     C                   IF        jPRIMA <> lenPRIMA
010600020611     C                   MOVEL     wrkPRIMA      PTASDS
010700020611     C                   ENDIF
010800020611     C                   IF        jDOPO  <> lenDOPO
010900020611     C                   MOVEL     wrkDOPO       DTASDS
011000020611     C                   ENDIF
011100020611     C*
011200020611     C* Stampo
011300020611     C                   WRITE     EC87TES
011400020611     C                   WRITE     EC87TASP1
011500020618     C*
011600020610     C                   ENDIF
011700020619     C*
011800020619     C* Gestisco errore su record di TNCSB00F
011900020619     C                   IF        ERRFIL = 'TNCSB00F'
012000020619     C*
012100020619     C* Inizializzo DS, variabili di WRK
012200020619     C                   CLEAR                   PCSBDS
012300020619     C                   CLEAR                   DCSBDS
012400020619     C                   MOVEL     *BLANKS       wrkPRIMA        500
012500020619     C                   MOVEL     *BLANKS       wrkDOPO         500
012600020619     C*
012700020619     C* Compongo le immagini "PRIMA" e "DOPO"
012800020619     C                   EVAL      wrkPRIMA = ERRRP1 + ERRRP2
012900020619     C                   EVAL      wrkDOPO  = ERRRD1 + ERRRD2
013000020619     C*
013100020619     C* Verifico presenza di valorizzazioni a *null
013200020619     C                   Z-ADD     *zeros        jPRIMA            3 0
013300020619     C                   Z-ADD     *zeros        lenPRIMA          3 0
013400020619     C                   EVAL      lenPRIMA = %len(%trim(wrkPRIMA))
013500020619     C     1             DO        lenPRIMA      j                 3 0
013600020619     C                   IF        %subst(wrkPRIMA:j:1) = X'00'
013700020619     C                   ADD       1             jPRIMA
013800020619     C                   ENDIF
013900020619     C                   ENDDO
014000020619     C*
014100020619     C                   Z-ADD     *zeros        jDOPO             3 0
014200020619     C                   Z-ADD     *zeros        lenDOPO           3 0
014300020619     C                   EVAL      lenDOPO = %len(%trim(wrkDOPO))
014400020619     C     1             DO        lenDOPO       j                 3 0
014500020619     C                   IF        %subst(wrkDOPO:j:1) = X'00'
014600020619     C                   ADD       1             jDOPO
014700020619     C                   ENDIF
014800020619     C                   ENDDO
014900030123     C*
015000020619     C* Muovo le immagini "PRIMA" e "DOPO" in stampa
015100020619     C                   IF        jPRIMA <> lenPRIMA
015200020619     C                   MOVEL     wrkPRIMA      PCSBDS
015300020619     C                   ENDIF
015400020619     C                   IF        jDOPO  <> lenDOPO
015500020619     C                   MOVEL     wrkDOPO       DCSBDS
015600020619     C                   ENDIF
015700020619     C*
015800020619     C* Stampo
015900020619     C                   WRITE     EC87TES
016000020619     C                   WRITE     EC87CSBP1
016100020619     C*
016200020619     C                   ENDIF
016300030123     C*
016400030123     C* Gestisco errore su record di FIAR500F
016500030123     C                   IF        ERRFIL = 'FIAR500F'
016600030123     C*
016700030123     C* Inizializzo DS, variabili di WRK
016800030123     C                   CLEAR                   PAR5DS
016900030123     C                   CLEAR                   DAR5DS
017000030123     C                   MOVEL     *BLANKS       wrkPRIMA        500
017100030123     C                   MOVEL     *BLANKS       wrkDOPO         500
017200030123     C*
017300030123     C* Compongo le immagini "PRIMA" e "DOPO"
017400030123     C                   EVAL      wrkPRIMA = ERRRP1 + ERRRP2
017500030123     C                   EVAL      wrkDOPO  = ERRRD1 + ERRRD2
017600030123     C*
017700030123     C* Verifico presenza di valorizzazioni a *null
017800030123     C                   Z-ADD     *zeros        jPRIMA            3 0
017900030123     C                   Z-ADD     *zeros        lenPRIMA          3 0
018000030123     C                   EVAL      lenPRIMA = %len(%trim(wrkPRIMA))
018100030123     C     1             DO        lenPRIMA      j                 3 0
018200030123     C                   IF        %subst(wrkPRIMA:j:1) = X'00'
018300030123     C                   ADD       1             jPRIMA
018400030123     C                   ENDIF
018500030123     C                   ENDDO
018600030123     C*
018700030123     C                   Z-ADD     *zeros        jDOPO             3 0
018800030123     C                   Z-ADD     *zeros        lenDOPO           3 0
018900030123     C                   EVAL      lenDOPO = %len(%trim(wrkDOPO))
019000030123     C     1             DO        lenDOPO       j                 3 0
019100030123     C                   IF        %subst(wrkDOPO:j:1) = X'00'
019200030123     C                   ADD       1             jDOPO
019300030123     C                   ENDIF
019400030123     C                   ENDDO
019500030123     C*
019600030123     C                   EVAL      PAR5UNI1 = %subst(par5uni:1:100)
019700030123     C                   EVAL      PAR5UNI2 = %subst(par5uni:100:100)
019800030123     C                   EVAL      DAR5UNI1 = %subst(dar5uni:1:100)
019900030123     C                   EVAL      DAR5UNI2 = %subst(dar5uni:100:100)
020000030123     C*
020100030123     C* Muovo le immagini "PRIMA" e "DOPO" in stampa
020200030123     C                   IF        jPRIMA <> lenPRIMA
020300030123     C                   MOVEL     wrkPRIMA      PAR5DS
020400030123     C                   ENDIF
020500030123     C                   IF        jDOPO  <> lenDOPO
020600030123     C                   MOVEL     wrkDOPO       DAR5DS
020700030123     C                   ENDIF
020800030123     C*
020900030123     C* Stampo
021000030123     C                   WRITE     EC87TES
021100030123     C                   WRITE     EC87AR5P1
021200030123     C*
021300030123     C                   ENDIF
021400030123     C*
021500020619     C                   ENDIF
021600020619     C*
021700020610     C                   ENDSR
021800000000     C*------------------------------------------------------------------------*
021900000000     C*  *INZSR - ROUTINE INIZIALE
022000000000     C*------------------------------------------------------------------------*
022100000000     C     *INZSR        BEGSR
022200000000     C*--------------------
022300000000     C* RICEVIMENTO PARAMETRI
022400000000     C*--------------------
022500000000     C     *ENTRY        PLIST
022600000000     C                   PARM                    KPJBA
022700950323     C                   MOVEL     KPJBU         DS87
022800000000     C*--------------------
022900000000     C* CHIAVI DI LETTURA
023000000000     C*--------------------
023100950323     C* CHIAVE LETTURA ECERR01L - COMPLETA
023200950323     C     KEYERR        KLIST
023300020610     C                   KFLD                    D87DAT
023400020610     C                   KFLD                    D87TIM
023500020610     C                   KFLD                    D87PRG
023600020610     C*
023700020620     C* Inizializzo a *OFF gli indicatori condizionanti la stampa
023800020620     C                   SETOFF                                       55
023900020620     C*
024000020610     C                   ENDSR
