000100000000      ****************************************************************
000200941219      *  PRIMO PROGRAMMA DI PROCEDURA RICHIAMATO IN TUTTE LE AZIONI  *
000300940105      *   PERMETTE IL LANCIO DEI PGM DI GESTIONE UNITA'.             *
000400930204      *                                                              *
000500930310      *   HA IN INPUT I DATI DI LANCIO NELLA KPJBU  E                *
000600940105      *   LANCIA IL PGM DELLA PROCEDURA CON DSUNIT NELLA KPJBU.      *
000700930204      *                                                              *
000800940105      *   PARAMETRI UTILIZZATI DAL PGM (IN KPJBU):                   *
000900940105      *   INPUT: - TIPO DI LANCIO  ( 2 ALFA )                        *
001000941115      *            -A2- GESTIONE       ANAGRAFICA  (TNSDU1R)         *
001100941115      *            -A5- INTERROGAZIONE ANAGRAFICA  (TNSDU1R)         *
001200941115      *            -A6- STAMPA         ANAGRAFICA  (TNSDU6R)         *
001300941115      *            -M2- GESTIONE       MOVIMENTI   (TNSDU4R)         *
001400941115      *            -M5- INTERROGAZIONE MOVIMENTI   (TNSDU4R)         *
001500941115      *            -M6- STAMPA         MOVIMENTI   (TNSDU8R)         *
001600941219      *            -D2- GESTIONE       DOTAZIONI   (TNSDUCR)         *
001700941219      *            -D5- INTERROGAZIONE DOTAZIONI   (TNSDUCR)         *
001800941219      *            -D6- STAMPA         DOTAZIONI   (TNSDUER)         *
001900940105      *   OUTPUT:- DSUNIT                                            *
002000930312      *                                                              *
002100930204      ****************************************************************
002200930714     D DSUNIT        E DS
002300930310     D KPJBA         E DS
002400000000      *
002500930204     C* PARAMETRI IN INPUT ALLA ROUTINE
002600000000     C     *ENTRY        PLIST
002700930310     C                   PARM                    KPJBA
002800940105     C                   MOVEL     KPJBU         WBU               2
002900940105     C                   MOVEL     WBU           WBU1              1
003000940105     C                   MOVE      WBU           WBU2              1
003100930312     C*
003200100305     c                   clear                   dsunit
003300100305     c
003400100305     c*
003500100305     c                   eval      dsutpmv=%subst(kpjbu: 106: 3)
003600940105     C                   MOVE      WBU1          DSUPRO
003700940105     C                   MOVE      WBU2          DSUOP0
003800930714     C                   MOVE      '0'           DSUF03
003900930714     C                   MOVE      '0'           DSUERR
004000930312     C*
004100940105     C     WBU           IFEQ      'A2'                                         *GESTIONE ANAGR.
004200940105     C     WBU           OREQ      'A5'                                         *INTERR.  ANAGR.
004300941115     C                   MOVEL     'TNSDU1R'     WPGM              8
004400940105     C                   ENDIF
004500940105     C     WBU           IFEQ      'A6'                                         *STAMPA   ANAGR.
004600941115     C                   MOVEL     'TNSDU6R'     WPGM
004700940105     C                   ENDIF
004800091111
004900940105     C     WBU           IFEQ      'M2'                                         *GESTIONE MOVIM.
005000940105     C     WBU           OREQ      'M5'                                         *INTERR.  MOVIM.
005100941115     C                   MOVEL     'TNSDU4R'     WPGM
005200940105     C                   ENDIF
005300940105     C     WBU           IFEQ      'M6'                                         *STAMPA   MOVIM.
005400941115     C                   MOVEL     'TNSDU8R'     WPGM
005500940105     C                   ENDIF
005600941219     C*
005700091111     C     WBU           IFEQ      'G2'                                         *GESTIONE MOVIM.G
005800091111     C     WBU           OREQ      'G5'                                         *INTERR.  MOVIM.G
005900091111     C                   MOVEL     'TNSDULR'     WPGM
006000091111     C                   ENDIF
006100091111     C     WBU           IFEQ      'G6'                                         *STAMPA   MOVIM.G
006200091111     C                   MOVEL     'TNSDU8R'     WPGM
006300091111     C                   ENDIF
006400091111     C*
006500941219     C     WBU           IFEQ      'D2'                                         *GESTIONE DOTAZIONI
006600941219     C     WBU           OREQ      'D5'                                         *INTERR.  DOTAZIONI
006700941219     C                   MOVEL     'TNSDUCR'     WPGM              8
006800941219     C                   ENDIF
006900941219     C     WBU           IFEQ      'D6'                                         *STAMPA   DOTAZIONI
007000941219     C                   MOVEL     'TNSDUER'     WPGM
007100941219     C                   ENDIF
007200940105     C*
007300940105     C                   MOVEL     DSUNIT        KPJBU
007400940105     C                   CALL      WPGM
007500940105     C                   PARM                    KPJBA
007600000000     C*
007700000000     C                   RETURN
