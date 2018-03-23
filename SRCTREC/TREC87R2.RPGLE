000100000000     H*------------------------------------------------------------------------*
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     F*------------------------------------------------------------------------*
000400000000     F* DATA BASE
000500000000     F*------------------------------------------------------------------------*
000600020620     FTITASQUE  IF   E             DISK
000700020610     FTREC87P   O    E             PRINTER OFLIND(*IN01)
000800000000     D*------------------------------------------------------------------------*
000900000000     D* SCHIERE
001000000000     D*------------------------------------------------------------------------*
001100000000     D*------------------------------------------------------------------------*
001200000000     D* INPUT
001300000000     D*------------------------------------------------------------------------*
001400020620     D*---------------------
001500020620     D* DS RIDEFINIZIONE RECORD TITASQUE
001600020620     D*---------------------
001700020620     D DS£ECO        E DS
001800020610     D*---------------------
001900020610     D* DS RIDEFINIZIONE IMMAGINI "PRIMA" E "DOPO"
002000020610     D*---------------------
002100020610     D PTASDS        E DS                  EXTNAME(TITAS00F)
002200020610     D                                     PREFIX(P)
002300020610     D DTASDS        E DS                  EXTNAME(TITAS00F)
002400020610     D                                     PREFIX(D)
002500020619     D PCSBDS        E DS                  EXTNAME(TNCSB00F)
002600020619     D                                     PREFIX(P)
002700020619     D DCSBDS        E DS                  EXTNAME(TNCSB00F)
002800020619     D                                     PREFIX(D)
002900030123     D PAR5DS        E DS                  EXTNAME(FIAR500F)
003000030123     D                                     PREFIX(P)
003100030123     D DAR5DS        E DS                  EXTNAME(FIAR500F)
003200030123     D                                     PREFIX(D)
003300020620     D*---------------------
003400020620     D* VARIABILI DI WRK
003500020620     D*---------------------
003600020620     D PARRRNALFA      S             10
003700020620     D PARRRNALFAWRK   S             10    varying
003800020620     D PARRRNNUM       S             10S 0
003900000000     C*------------------------------------------------------------------------*
004000000000     C* MAIN LINE
004100000000     C*------------------------------------------------------------------------*
004200000000     C*
004300020620     C* Innanzitutto reperisco il record dal file TITASQUE
004400020610     C                   EXSR      REPERR
004500020610     C*
004600020610     C* Effettuo la stampa
004700020610     C                   EXSR      STAERR
004800020610     C*
004900000000     C                   SETON                                        LR
005000020610     C*------------------------------------------------------------------------*
005100020620     C*  REPERR - REPERISCO IL RECORD DAL FILE TITASQUE
005200020610     C*------------------------------------------------------------------------*
005300020610     C     REPERR        BEGSR
005400020610     C*
005500020610     C                   MOVEL     'S'           WRECOK            1
005600020610     C*
005700020620     C     PARRRNNUM     CHAIN     TITASQUE
005800020620     C                   IF        NOT %found(TITASQUE)
005900020610     C                   MOVEL     'N'           WRECOK
006000020610     C                   ENDIF
006100020610     C*
006200020610     C                   ENDSR
006300020610     C*------------------------------------------------------------------------*
006400020610     C*  STAERR - STAMPA L'ERRORE
006500020610     C*------------------------------------------------------------------------*
006600020610     C     STAERR        BEGSR
006700020620     C*
006800020610     C                   IF        WRECOK = 'S'
006900020620     C*
007000020620     C* Ridefinisco il record di TITASQUE
007100020620     C                   MOVEL(P)  QUEDTA        DS£ECO
007200020620     C*
007300020620     C* Valorizzo i campi di testata in stampa
007400020620     C                   MOVEL(P)  D£GDAT        ERRDAM
007500020620     C                   MOVEL(P)  D£GTIM        ERRORM
007600020620     C                   MOVEL(P)  D£GSTS        ERRSTS
007700020620     C                   MOVEL(P)  D£GUSR        ERRUSR
007800020620     C                   MOVEL(P)  D£GPGF        ERRPGF
007900020620     C                   MOVEL(P)  D£GLIB        ERRLIB
008000020620     C                   MOVEL(P)  D£GFIL        ERRFIL
008100020620     C                   MOVEL(P)  D£GMBR        ERRMBR
008200020610     C*
008300020610     C* Gestisco errore su record di TITAS00F/TITAS10F
008400020620     C                   IF        D£GFIL = 'TITAS00F' OR
008500020620     C                             D£GFIL = 'TITAS10F'
008600020611     C*
008700020611     C* Inizializzo DS, variabili di WRK
008800020611     C                   CLEAR                   PTASDS
008900020611     C                   CLEAR                   DTASDS
009000020611     C                   MOVEL     *BLANKS       wrkPRIMA        500
009100020611     C                   MOVEL     *BLANKS       wrkDOPO         500
009200020611     C*
009300020611     C* Compongo le immagini "PRIMA" e "DOPO"
009400020620     C                   EVAL      wrkPRIMA = D£PRE1 + D£PRE2
009500020620     C                   EVAL      wrkDOPO  = D£DRE1 + D£DRE2
009600020611     C*
009700020611     C* Verifico presenza di valorizzazioni a *null
009800020611     C                   Z-ADD     *zeros        jPRIMA            3 0
009900020611     C                   Z-ADD     *zeros        lenPRIMA          3 0
010000020611     C                   EVAL      lenPRIMA = %len(%trim(wrkPRIMA))
010100020611     C     1             DO        lenPRIMA      j                 3 0
010200020611     C                   IF        %subst(wrkPRIMA:j:1) = X'00'
010300020611     C                   ADD       1             jPRIMA
010400020611     C                   ENDIF
010500020611     C                   ENDDO
010600020611     C*
010700020611     C                   Z-ADD     *zeros        jDOPO             3 0
010800020611     C                   Z-ADD     *zeros        lenDOPO           3 0
010900020611     C                   EVAL      lenDOPO = %len(%trim(wrkDOPO))
011000020611     C     1             DO        lenDOPO       j                 3 0
011100020611     C                   IF        %subst(wrkDOPO:j:1) = X'00'
011200020611     C                   ADD       1             jDOPO
011300020611     C                   ENDIF
011400020611     C                   ENDDO
011500020611     C*
011600020611     C* Muovo le immagini "PRIMA" e "DOPO" in stampa
011700020611     C                   IF        jPRIMA <> lenPRIMA
011800020611     C                   MOVEL     wrkPRIMA      PTASDS
011900020611     C                   ENDIF
012000020611     C                   IF        jDOPO  <> lenDOPO
012100020611     C                   MOVEL     wrkDOPO       DTASDS
012200020611     C                   ENDIF
012300020611     C*
012400020611     C* Stampo
012500020611     C                   WRITE     EC87TES
012600020611     C                   WRITE     EC87TASP1
012700020618     C*
012800020610     C                   ENDIF
012900020619     C*
013000020619     C* Gestisco errore su record di TNCSB00F
013100020620     C                   IF        D£GFIL = 'TNCSB00F'
013200020619     C*
013300020619     C* Inizializzo DS, variabili di WRK
013400020619     C                   CLEAR                   PCSBDS
013500020619     C                   CLEAR                   DCSBDS
013600020619     C                   MOVEL     *BLANKS       wrkPRIMA        500
013700020619     C                   MOVEL     *BLANKS       wrkDOPO         500
013800020619     C*
013900020619     C* Compongo le immagini "PRIMA" e "DOPO"
014000020620     C                   EVAL      wrkPRIMA = D£PRE1 + D£PRE2
014100020620     C                   EVAL      wrkDOPO  = D£DRE1 + D£DRE2
014200020619     C*
014300020619     C* Verifico presenza di valorizzazioni a *null
014400020619     C                   Z-ADD     *zeros        jPRIMA            3 0
014500020619     C                   Z-ADD     *zeros        lenPRIMA          3 0
014600020619     C                   EVAL      lenPRIMA = %len(%trim(wrkPRIMA))
014700020619     C     1             DO        lenPRIMA      j                 3 0
014800020619     C                   IF        %subst(wrkPRIMA:j:1) = X'00'
014900020619     C                   ADD       1             jPRIMA
015000020619     C                   ENDIF
015100020619     C                   ENDDO
015200020619     C*
015300020619     C                   Z-ADD     *zeros        jDOPO             3 0
015400020619     C                   Z-ADD     *zeros        lenDOPO           3 0
015500020619     C                   EVAL      lenDOPO = %len(%trim(wrkDOPO))
015600020619     C     1             DO        lenDOPO       j                 3 0
015700020619     C                   IF        %subst(wrkDOPO:j:1) = X'00'
015800020619     C                   ADD       1             jDOPO
015900020619     C                   ENDIF
016000020619     C                   ENDDO
016100020619     C*
016200020619     C* Muovo le immagini "PRIMA" e "DOPO" in stampa
016300020619     C                   IF        jPRIMA <> lenPRIMA
016400020619     C                   MOVEL     wrkPRIMA      PCSBDS
016500020619     C                   ENDIF
016600020619     C                   IF        jDOPO  <> lenDOPO
016700020619     C                   MOVEL     wrkDOPO       DCSBDS
016800020619     C                   ENDIF
016900020619     C*
017000020619     C* Stampo
017100020619     C                   WRITE     EC87TES
017200020619     C                   WRITE     EC87CSBP1
017300020619     C*
017400020619     C                   ENDIF
017500030123     C*
017600030123     C* Gestisco errore su record di FIAR500F
017700030123     C                   IF        D£GFIL = 'FIAR500F'
017800030123     C*
017900030123     C* Inizializzo DS, variabili di WRK
018000030123     C                   CLEAR                   PAR5DS
018100030123     C                   CLEAR                   DAR5DS
018200030123     C                   MOVEL     *BLANKS       wrkPRIMA        500
018300030123     C                   MOVEL     *BLANKS       wrkDOPO         500
018400030123     C*
018500030123     C* Compongo le immagini "PRIMA" e "DOPO"
018600030123     C                   EVAL      wrkPRIMA = D£PRE1 + D£PRE2
018700030123     C                   EVAL      wrkDOPO  = D£DRE1 + D£DRE2
018800030123     C*
018900030123     C* Verifico presenza di valorizzazioni a *null
019000030123     C                   Z-ADD     *zeros        jPRIMA            3 0
019100030123     C                   Z-ADD     *zeros        lenPRIMA          3 0
019200030123     C                   EVAL      lenPRIMA = %len(%trim(wrkPRIMA))
019300030123     C     1             DO        lenPRIMA      j                 3 0
019400030123     C                   IF        %subst(wrkPRIMA:j:1) = X'00'
019500030123     C                   ADD       1             jPRIMA
019600030123     C                   ENDIF
019700030123     C                   ENDDO
019800030123     C*
019900030123     C                   Z-ADD     *zeros        jDOPO             3 0
020000030123     C                   Z-ADD     *zeros        lenDOPO           3 0
020100030123     C                   EVAL      lenDOPO = %len(%trim(wrkDOPO))
020200030123     C     1             DO        lenDOPO       j                 3 0
020300030123     C                   IF        %subst(wrkDOPO:j:1) = X'00'
020400030123     C                   ADD       1             jDOPO
020500030123     C                   ENDIF
020600030123     C                   ENDDO
020700030123     C*
020800030123     C                   EVAL      PAR5UNI1 = %subst(par5uni:1:100)
020900030123     C                   EVAL      PAR5UNI2 = %subst(par5uni:100:100)
021000030123     C                   EVAL      DAR5UNI1 = %subst(dar5uni:1:100)
021100030123     C                   EVAL      DAR5UNI2 = %subst(dar5uni:100:100)
021200030123     C*
021300030123     C* Muovo le immagini "PRIMA" e "DOPO" in stampa
021400030123     C                   IF        jPRIMA <> lenPRIMA
021500030123     C                   MOVEL     wrkPRIMA      PAR5DS
021600030123     C                   ENDIF
021700030123     C                   IF        jDOPO  <> lenDOPO
021800030123     C                   MOVEL     wrkDOPO       DAR5DS
021900030123     C                   ENDIF
022000030123     C*
022100030123     C* Stampo
022200030123     C                   WRITE     EC87TES
022300030123     C                   WRITE     EC87AR5P1
022400030123     C*
022500030123     C                   ENDIF
022600020610     C*
022700020619     C                   ENDIF
022800020619     C*
022900020610     C                   ENDSR
023000000000     C*------------------------------------------------------------------------*
023100000000     C*  *INZSR - ROUTINE INIZIALE
023200000000     C*------------------------------------------------------------------------*
023300000000     C     *INZSR        BEGSR
023400000000     C*--------------------
023500000000     C* RICEVIMENTO PARAMETRI
023600000000     C*--------------------
023700000000     C     *ENTRY        PLIST
023800020620     C                   PARM                    PARRRNALFA
023900020620     C                   EVAL      PARRRNALFAWRK = %trim(PARRRNALFA)
024000020620     C                   MOVE(P)   PARRRNALFAWRK PARRRNNUM
024100020610     C*
024200020620     C* Inizializzo a *OFF gli indicatori condizionanti la stampa
024300020620     C                   SETOFF                                       55
024400020620     C*
024500020610     C                   ENDSR
