000100000000     H*------------------------------------------------------------------------*
000200000000     H*                  - STATISTICA CLIENTI -         LANCIO DA MENU'        *
000300000000     H*------------------------------------------------------------------------*
000400000000     H* INPUT  - DATI DI LANCIO NELLA KPJBU:
000500000000     H*    LIVELLO PROCEDURA --> 'STA'
000600000000     H*    LIVELLO PROGRAMMA --> 'REG'/'SER'/'AFF'/'DEL'/'AMM'/'DEF'/'DEL'/'DEC'
000700000000     H*------------------------------------------------------------------------*
000800000000     H* PERMETTE IL LANCIO DEL PROGRAMMA DI STAMPA DELLE STATISTICHE  DA  MENU'.
000900000000     H* HA NELLA KPJBU IL TIPO DI STATISTICA CHE SI VUOLE LANCIARE, E  VALORIZZA
001000000000     H* LA DS DI PROCEDURA IMPOSTANDO IL LIVELLO PROGRAMMA DEL MENU' DI ENTRATA
001100000000     D*------------------------------------------------------------------------*
001200000000     D* INPUT
001300000000     D*------------------------------------------------------------------------*
001400000000     D*--------------------
001500000000     D* DS ESTERNA DI PROCEDURA
001600000000     D*--------------------
001700010921     D DS60          E DS                  EXTNAME(TISE60DS)
001800000000     D*--------------------
001900000000     D* ARCHITETTURA
002000000000     D*--------------------
002100000000     D KPJBA         E DS
002200000000     C*------------------------------------------------------------------------*
002300000000     C*?MAIN LINE
002400000000     C*------------------------------------------------------------------------*
002500000000     C*
002600000000     C* PARAMETRI IN INPUT ALLA ROUTINE
002700000000     C     *ENTRY        PLIST
002800000000     C                   PARM                    KPJBA
002900000000     C*
003000000000     C                   MOVEL     KPJBU         WOP               6            *LIV PROCED + LIV PG
003100000000     C                   MOVEL     WOP           WOP0              3            *LIVELLO PROCEDURA
003200000000     C                   MOVE      WOP           WOP1              3            *LIVELLO PROGRAMMA
003300000818     c                   CLEAR                   DS60
003400000000     C*--------------------
003500000000     C* PARAMETRI INFORMATIVI
003600000000     C*--------------------
003700000000     C                   MOVEL     WOP0          D60OP0                         *LIVELLO PROCEDURA
003800000000     C                   MOVEL     WOP1          D60OP1                         *LIVELLO PROGRAMMA
003900000818     c                   MOVEL     '2'           D60SCE                         *SCELTA
004000000000     C                   MOVEL     '0'           D60F03                         *FINE PROGRAMMA
004100000000     C                   MOVEL     '0'           D60ERR                         *ERRORE
004200000000     C                   MOVEL     *BLANKS       D60MSG                         *MESSAGGIO DI ERRORE
004300000000     C*
004400000000     C                   MOVEL     DS60          KPJBU
004500010921     C                   CALL      'TISE61R'
004600000000     C                   PARM                    KPJBA
004700000000     C*
004800000000     C                   SETON                                        LR
