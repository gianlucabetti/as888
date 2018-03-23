000100970528     h*--------------------------------------------------------------------------------------------*
000200970528     h* Gestione cappario - Guida
000300970528     h*--------------------------------------------------------------------------------------------*
000400970528     h DECEDIT('0,') DATEDIT(*DMY.)
000500970528     d*--------------------------------------------------------------------------------------------*
000600970528     d* Data structure
000700970528     d*--------------------------------------------------------------------------------------------*
000800970528     d*----------------
000900970528     d* Ds
001000970528     d*----------------
001100970528     d dssi00        E DS                  EXTNAME(tisi00ds)                    *ds di procedura
001200970528     d kpjba         E DS                                                       *architettura
001300970528     d*----------------
001400970528     d* Variabili a programma
001500970528     d*----------------
001600970528     d wges            S              1                                         *procedura
001700970528     d wope            S              2                                         *funzione
001800970528     d wpgm            S              8                                         *nome programma
001900970528     c*--------------------------------------------------------------------------------------------*
002000970528     c* Main lines
002100970528     c*--------------------------------------------------------------------------------------------*
002200970528     c*
002300970528     c* imposta la ds di procedura
002400970528     c                   CLEAR                   dssi00
002500970528     c                   MOVEL     kpjbu         d00op0                         *livello procedura
002600970528     c                   MOVEL     *blanks       d00op1                         *livello programma
002700970528     c                   MOVEL     '0'           d00f03                         *premuto F03
002800970528     c                   MOVEL     '0'           d00f12                         *premuto F12
002900970528     c                   MOVEL     '0'           d00err                         *errore
003000970528     c                   MOVEL     *blanks       d00msg                         *messaggio ritorno
003100970528     C*
003200970528     C* reperisce la procedura e la funzione di entrata
003300970528     c                   MOVEL     d00op0        wges                           *procedura
003400970528     c                   MOVE      d00op0        wope                           *funzione
003500970528     c*
003600970528     c* gestione versioni cappario
003700970528if  1c                   IF        wges='R'
003800970528     c                   MOVEL     'TISI46R'     wpgm
003900970528e   1c                   ENDIF
004000970528     c*
004100970528     c* gestione eccezioni terminal
004200970528if  1c                   IF        wges='P'
004300970528if  2c                   IF        wope='01' OR wope='02' OR wope='05'          *gestione
004400970528     c                   MOVEL     'TISI17R'     wpgm
004500970528e   2c                   ENDIF
004600980225if  2c                   IF        wope='03'                                    *stampa
004700980225     c                   MOVEL     'TISI19R'     wpgm
004800980225e   2c                   ENDIF
004900970619e   1c                   ENDIF
005000970528     c*
005100970528     c* gestione tempi di trazione
005200970528if  1c                   IF        wges='Z'
005300970528if  2c                   IF        wope='01' OR wope='02' OR wope='05'          *gestione
005400970528     c                   MOVEL     'TISI23R'     wpgm
005500970528e   2c                   ENDIF
005600970528if  2c                   IF        wope='03'                                    *stampa
005700970528     c                   MOVEL     'TISI25R'     wpgm
005800970528e   2c                   ENDIF
005900970528e   1c                   ENDIF
006000970528     c*
006100970528     c* lancia il programma
006200970528     c                   CALL      wpgm
006300970612     c                   PARM                    kpjba
006400970612     c                   PARM                    dssi00
006500970528     c*
006600970528     c                   SETON                                        lr
006700970528     c*--------------------------------------------------------------------------------------------*
006800970528     C* *inzsr - operazioni iniziali
006900970528     c*--------------------------------------------------------------------------------------------*
007000970528     C     *inzsr        BEGSR
007100970528     c*
007200970528     c* ricevimento parametri
007300970528     c     *ENTRY        PLIST
007400970528     c                   PARM                    kpjba
007500970528     c*
007600970528     C                   ENDSR
