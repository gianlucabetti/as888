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
001200160923      *            -AS- storicizzazioneANAGRAFICA  (TNSDU1R)         *
001300941115      *            -A6- STAMPA         ANAGRAFICA  (TNSDU6R)         *
001400941115      *            -M2- GESTIONE       MOVIMENTI   (TNSDU4R)         *
001500941115      *            -M5- INTERROGAZIONE MOVIMENTI   (TNSDU4R)         *
001600941115      *            -M6- STAMPA         MOVIMENTI   (TNSDU8R)         *
001700941219      *            -D2- GESTIONE       DOTAZIONI   (TNSDUCR)         *
001800941219      *            -D5- INTERROGAZIONE DOTAZIONI   (TNSDUCR)         *
001900941219      *            -D6- STAMPA         DOTAZIONI   (TNSDUER)         *
002000940105      *   OUTPUT:- DSUNIT                                            *
002100930312      *                                                              *
002200930204      ****************************************************************
002300930714     D DSUNIT        E DS
002400930310     D KPJBA         E DS
002500000000      *
002600930204     C* PARAMETRI IN INPUT ALLA ROUTINE
002700000000     C     *ENTRY        PLIST
002800930310     C                   PARM                    KPJBA
002900940105     C                   MOVEL     KPJBU         WBU               2
003000940105     C                   MOVEL     WBU           WBU1              1
003100940105     C                   MOVE      WBU           WBU2              1
003200930312     C*
003300100305     c                   clear                   dsunit
003400100305     c
003500100305     c*
003600160923     c********           eval      dsutpmv=%subst(kpjbu: 106: 3)
003700940105     C                   MOVE      WBU1          DSUPRO
003800940105     C                   MOVE      WBU2          DSUOP0
003900930714     C                   MOVE      '0'           DSUF03
004000930714     C                   MOVE      '0'           DSUERR
004100930312     C*
004200940105     C     WBU           IFEQ      'A2'                                         *GESTIONE ANAGR.
004300940105     C     WBU           OREQ      'A5'                                         *INTERR.  ANAGR.
004400160923     C     WBU           OREQ      'AS'                                         *storicizzazione
004500941115     C                   MOVEL     'TNSDU1R'     WPGM              8
004600940105     C                   ENDIF
004700940105     C     WBU           IFEQ      'A6'                                         *STAMPA   ANAGR.
004800941115     C                   MOVEL     'TNSDU6R'     WPGM
004900940105     C                   ENDIF
005000091111
005100940105     C     WBU           IFEQ      'M2'                                         *GESTIONE MOVIM.
005200940105     C     WBU           OREQ      'M5'                                         *INTERR.  MOVIM.
005300941115     C                   MOVEL     'TNSDU4R'     WPGM
005400940105     C                   ENDIF
005500940105     C     WBU           IFEQ      'M6'                                         *STAMPA   MOVIM.
005600941115     C                   MOVEL     'TNSDU8R'     WPGM
005700940105     C                   ENDIF
005800941219     C*
005900091111     C     WBU           IFEQ      'G2'                                         *GESTIONE MOVIM.G
006000091111     C     WBU           OREQ      'G5'                                         *INTERR.  MOVIM.G
006100091111     C                   MOVEL     'TNSDULR'     WPGM
006200091111     C                   ENDIF
006300091111     C     WBU           IFEQ      'G6'                                         *STAMPA   MOVIM.G
006400091111     C                   MOVEL     'TNSDU8R'     WPGM
006500091111     C                   ENDIF
006600091111     C*
006700941219     C     WBU           IFEQ      'D2'                                         *GESTIONE DOTAZIONI
006800941219     C     WBU           OREQ      'D5'                                         *INTERR.  DOTAZIONI
006900941219     C                   MOVEL     'TNSDUCR'     WPGM              8
007000941219     C                   ENDIF
007100941219     C     WBU           IFEQ      'D6'                                         *STAMPA   DOTAZIONI
007200941219     C                   MOVEL     'TNSDUER'     WPGM
007300941219     C                   ENDIF
007400940105     C*
007500940105     C                   MOVEL     DSUNIT        KPJBU
007600940105     C                   CALL      WPGM
007700940105     C                   PARM                    KPJBA
007800000000     C*
007900000000     C                   RETURN
