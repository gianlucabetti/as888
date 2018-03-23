000100921021     H DECEDIT('0,') DATEDIT(*DMY.)
000200941027     H* FNLV50R  *---------------------------------------------------*
000300941027     H*     CONTROLLO FILIALE GESTIONE                               *
000400921021     H*--------------------------------------------------------------*
000500941027     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
000600941027     D*
000700941027     D L1              S              3  0 DIM(30)
000800941027     D L6S             S              3  0 DIM(30)
000900940218      *
001000941027     D* DS PER FNLV50R - DS DEI CAMPI PER CONTROLLO FILIALE GESTIONE
001100941027     D DSLV50        E DS                  EXTNAME(FNLV50DS)
001200941027     D* INIZIO DEL PROFILO UTENTE
001300020430     D*                                       2   4 REM
001400020430     D*                                       5   7 REMFIL
001500941027     D*
001600941027     D* DS PER TRUL06R - CARICAMENTO £X
001700941027     D DSUL06        E DS                  EXTNAME(TRUL06DS)
001800941027     D  LIN                    1     90  0
001900941027     D                                     DIM(30)
002000941027     D CNCR80        E DS
002100941027     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002200941027     D KPJBA         E DS
002300921021     C*---------------------------------------------------------------*
002400921021     C     *ENTRY        PLIST
002500941027     C                   PARM                    DSLV50
002600000000     C*---------------------------------------------------------------*
002700941027     C* TIPO LANCIO, D50TLA "C" -           CHIUSO CON LR
002800941027     C* TIPO LANCIO, D50TLA "L" - ELABORO E CHIUDO CON LR
002900941027     C* TIPO LANCIO, D50TLA " " - ELABORO E CHIUDO IN RETRN
003000941027    1C     D50TLA        IFNE      'C'
003100941027     C*
003200941027     C                   CLEAR                   D50ERR
003300941027     C                   CLEAR                   D50MSG
003400941027     C                   SETOFF                                       90
003500941027     C*
003600941027     C* OPERAZIONI INIZIALI
003700941027    2C     WFLG1         IFEQ      ' '
003800941027     C                   EXSR      INIZIO
003900941027     C                   MOVEL     '1'           WFLG1             1
004000941027    2C                   ENDIF
004100941027     C*
004200941027     C* CONTROLLO SE SONO SIMFEL O REMOTO
004300020430     C*                    EXSR CTRREM
004400020430     C                   Z-ADD     SIMPOU        WFGS              3 0
004500941027     C*
004600941027     C* SE SONO REM POSSO ESSERE SOLO ME STESSO
004700941027     C* OSE E' A 0 ERRATA
004800941027    2C     WFGS          IFNE      SIMFEL
004900941027    2C     D50FGS        ANDNE     WFGS
005000941027     C     D50FGS        OREQ      0
005100941027     C                   MOVEL     '1'           D50ERR
005200941027     C                   MOVEL     MSG(1)        D50MSG
005300941027     C                   SETON                                        90
005400941027    2C                   ENDIF
005500941027     C*
005600941027     C* SE SONO SIMFEL NON PUO' ESSERE FIL GESTITA DA ALTRE
005700941027    2C  N90WFGS          IFEQ      SIMFEL
005800941027     C     D50FGS        ANDNE     SIMFEL
005900941027     C* CARICO LE TABELLE
006000941027    3C     WFLG2         IFEQ      ' '
006100941027     C                   EXSR      CARTAB
006200941027     C                   MOVEL     '1'           WFLG2             1
006300941027    3C                   ENDIF
006400941027     C*
006500941027     C*
006600941027     C* DEVE APPARTENERE ALLA L1
006700941027     C     D50FGS        LOOKUP    L1                                     30
006800941027    3C     *IN30         IFEQ      *OFF
006900941027     C                   MOVEL     '1'           D50ERR
007000941027     C                   MOVEL     MSG(1)        D50MSG
007100941027     C                   SETON                                        90
007200941027    3C                   ENDIF
007300941027     C*
007400941027     C* NON DEVE ESSERE GESTITA DA ALTRE FILIALI
007500941027    3C     *IN90         IFEQ      *OFF
007600941027     C     D50FGS        LOOKUP    L6S                                    30
007700941027    4C     *IN30         IFEQ      *ON
007800941027     C                   MOVEL     '2'           D50ERR
007900941027     C                   MOVEL     MSG(2)        D50MSG
008000941027     C                   SETON                                        90
008100941027    4C                   ENDIF
008200941027    3C                   ENDIF
008300941027    2C                   ENDIF
008400941027    1C                   ENDIF
008500941025     C*
008600941025     C* CHIUSURA PGM
008700941027    1C     D50TLA        IFEQ      ' '
008800941025     C                   RETURN
008900941025     C                   ELSE
009000941025     C                   SETON                                        LR
009100941025    1C                   ENDIF
009200941024     C*
009300941027     C* OPERAZIONI CHE ESEGUO AD APERTURA PGM ------------------------*
009400941027     C     INIZIO        BEGSR
009500941027     C                   Z-ADD     1             CODUT
009600020430     C                   MOVEL     D50PRU        SIMUTE
009700020430     C                   MOVEL     CNCR80        REC80
009800941027     C                   CALL      'X§PARUT'
009900941027     C                   PARM                    UT§DSE
010000941027     C                   MOVEL     REC80         CNCR80
010100941027     C                   ENDSR
010200941027     C*
010300941027     C* CARICO LE TABELLE --------------------------------------------*
010400941027     C* OPERAZIONI CHE ESEGUO SOLO SE FIL GESTIONE NON E' SIMFEL
010500941027     C     CARTAB        BEGSR
010600941027     C***
010700941027     C* CARICO TUTTE LE  FILIALI GESTITE DA ALTRE (DA £6)
010800941027     C***
010900941027     C                   CLEAR                   DSUL06
011000941027     C                   MOVE      '£6'          D06COD
011100020509     C**!!!              MOVEL     'S'           D06ESC
011200020509     c                   eval      d06esc = 'G'
011300020509     c                   movel     simfel        d06key
011400941027     C                   MOVEL     DSUL06        KPJBU
011500941027     C                   CALL      'TRUL06R'
011600941027     C                   PARM                    KPJBA
011700941027     C                   MOVEL     KPJBU         DSUL06
011800941027     C                   MOVEA     LIN           L6S
011900941027     C***
012000941027     C* CARICO TABELLA FILIALI GESTITE £1
012100941027     C***
012200941027     C                   CLEAR                   DSUL06
012300941027     C                   MOVE      '£1'          D06COD
012400941027     C                   MOVEL     SIMFEL        D06KEY
012500941027     C                   MOVEL     'L'           D06TLA
012600941027     C                   MOVEL     DSUL06        KPJBU
012700941027     C*
012800941027     C                   CALL      'TRUL06R'
012900941027     C                   PARM                    KPJBA
013000941027     C                   MOVEL     KPJBU         DSUL06
013100941027     C                   MOVEA     LIN           L1
013200941027     C*
013300941027     C                   ENDSR
013400941027     C*
013500941027     C* VEDO SE SONO SIMFEL O REMOTO ---------------------------------*
013600020430     C*          CTRREM    BEGSR
013700020430     C*          REM       IFEQ 'REM'
013800020430     C*          REMFIL    ANDGT*ZEROS
013900020430     C*                    MOVELREMFIL    WFGS
014000020430     C*                    ELSE
014100020430     C*                    MOVELSIMFEL    WFGS    30
014200020430     C*                    END
014300020430     C*                    ENDSR
014400941027**
014500941027Filiale in Gestione errata
014600941027Filiale in Gestione ad altra filiale: cambio non effettuabile
