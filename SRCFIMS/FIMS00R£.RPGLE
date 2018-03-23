000100000000     h*--------------------------------------------------------------------------------------------*
000200000000     h* Saldi VanDerLande - Guida                                                                  *
000300000000     h*--------------------------------------------------------------------------------------------*
000400000000     h DECEDIT('0,') DATEDIT(*DMY.)
000500000000     d*--------------------------------------------------------------------------------------------*
000600000000     d* data structure
000700000000     d*--------------------------------------------------------------------------------------------*
000800000000     d*---
000900000000     d* Variabili
001000000000     d*---
001100000000     d wges            S              3                                         *gestione (arg+ope)
001200000000     d wpro            S              1                                         *procedura
001300000000     d wope            S              2                                         *operazione
001400000000     d wpgm            S              8                                         *programma
001500000000     d*---
001600000000     d* Ds
001700000000     d*---
001800000000     d dsms00        E DS                  EXTNAME(fims00ds)                    *ds di procedura
001900000000     d kpjba         E DS                                                       *architettura
002000000000     c*--------------------------------------------------------------------------------------------*
002100000000     c* Main lines
002200000000     c*--------------------------------------------------------------------------------------------*
002300000000     C*
002400000000     C* reperisce la procedura di entrata
002500000000     c                   MOVEL     kpjbu         wges                           *gestione
002600000000     c                   MOVEL     wges          wpro                           *procedura
002700000000     c                   MOVE      wges          wope                           operazione (05,03..)
002800000000     c*
002900000000     c* imposta la ds di procedura
003000000000     c                   CLEAR                   dsms00
003100000000     c                   MOVEL     wges          d00op0                         *livello procedura
003200000000     c                   MOVEL     *blanks       d00op1                         *livello programma
003300000000     c                   MOVEL     '0'           d00f03                         *premuto F03
003400000000     c                   MOVEL     '0'           d00f12                         *premuto F12
003500000000     c                   MOVEL     '0'           d00err                         *errore
003600000000     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
003700000000     c*
003800000000     c* imposta pgm da chiamare
003900000000if  1c                   IF        wpro = 'S'                                   *LETTURE
004000000000     c                   MOVEL     'FIMS01R'     wpgm
004100000000e   1c                   ENDIF
004200000000     c*
004300000000if  1c                   IF        wpro = 'C'                                   *LETTURE TEST
004400000000     c                   MOVEL     'FIMS11R'     wpgm
004500000000e   1c                   ENDIF
004600000000     c*
004700000000     c* lancia il programma
004800000000     c                   CALL      wpgm
004900000000     c                   PARM                    kpjba
005000000000     c                   PARM                    dsms00
005100000000     c*
005200000000     c                   SETON                                            lr
005300000000     c*--------------------------------------------------------------------------------------------*
005400000000     C* *inzsr - operazioni iniziali
005500000000     c*--------------------------------------------------------------------------------------------*
005600000000     C     *inzsr        BEGSR
005700000000     c*
005800000000     c* ricevimento parametri
005900000000     c     *ENTRY        PLIST
006000000000     c                   PARM                    kpjba
006100000000     C*
006200000000     C                   ENDSR
