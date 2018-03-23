000100940224     H*--------------------------------------------------------------*
000200030926     D KPJBA         E DS
000300030926     D Tibs36ds      E DS
000400030926     D Tibs56ds      E DS
000500030926     D                                     INZ
000600030926     D  Ski                           3  0
000700030926     D                                     DIM(250)
000800030926     D                                     OVERLAY(O56Ski)
000900030926
001000030926     D  Hub            s              3  0 INZ
001100030926     D                                     DIM(250)
001200030926     D  Fil            s              3  0 INZ
001300030926     D                                     DIM(250)
001400030926
001500030926      * DS PER trul06r - caricamento £1
001600030926     D TR06DS        E DS                  EXTNAME(TRUL06DS)
001700030926     D  £1                     1     90  0
001800030926     D                                     DIM(30)                              Fil. gestite
001900030926     D TIBS55        E DS                  EXTNAME(TIBS55DS)
002000030926     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
002100030926     D CNCR80        E DS
002200030205     C*---------------------------------------------------------------*
002300030926     C     *ENTRY        PLIST
002400030926     C                   PARM                    KPJBA
002500030926      *
002600030926      * Recupero codice AS
002700030926     C                   Z-ADD     1             CODUT             1 0
002800030926     C                   CALL      'X§PARUT'
002900030926     C                   PARM                    UTEDSE
003000030926     C                   MOVEL     REC80         CNCR80
003100030926      *
003200030926      * CARICAMENTO SCHIERE
003300030926     C                   EXSR      CARTAB
003400030926      *
003500030926      *  In base all'utente prende il P.O.
003600030926     C                   CLEAR                   Tibs36Ds
003700030926     c                   EVAL      I36ute = Knmus
003800030926     c                   EVAL      I36Tla = 'L'
003900030926     C                   CALL      'TIBS36R'
004000030926     C                   PARM                    Tibs36Ds
004100030926      *
004200030926      * Caricamento Tabella £1
004300030926     C     o36pou        IFNE      *ZEROS
004400030926     C                   CLEAR                   TR06DS
004500030926     C                   CLEAR                   KPJBU
004600030926     C                   MOVEL     '£1'          D06COD
004700030926     C                   MOVEL     o36pou        D06KEY
004800030926     C                   MOVEL     TR06DS        KPJBU
004900030926     C                   CALL      'TRUL06R'
005000030926     C                   PARM                    KPJBA
005100030926     C                   MOVEL     KPJBU         TR06DS
005200030926     C                   ENDIF
005300030926      *
005400030926      * RICHIAMO PGM X REPERIMENTO LIBRERIA DATI SISTEMA INFORMATIVO
005500030926     C                   CLEAR                   TIBS55
005600030926     C                   MOVEL     o36pou        I50ppo
005700030926     C                   MOVEL     '046'         I50apo
005800030926     C                   CALL      'TIBS55R'
005900030926     C                   PARM                    TIBS55
006000030926      *
006100000000     C                   SETON                                        LR
006200030926      **-------------------------------------------------------------**
006300030926      ** CARICAMENTO SCHIERE                                         **
006400030926      ***------------------------------------------------------------**
006500030926     C     CARTAB        BEGSR
006600030926
006700030926     ** carico la schiera dei PO del terminal usando il PO utente.
006800030926     c                   clear                   tibs56ds
006900030926     c                   eval      i56ppo=simfel
007000030926     c                   eval      i56mod='TPS'
007100030926     c                   call      'TIBS56R'
007200030926     c                   parm                    tibs56ds
007300030926     C                   MOVEA     SKI           HUB
007400030926
007500030926     ** carico la schiera dei PO del terminal usando il PO utente.
007600030926     c                   clear                   tibs56ds
007700030926     c                   eval      i56ppo=simfel
007800030926     c                   eval      i56mod='POS'
007900030926     c                   call      'TIBS56R'
008000030926     c                   parm                    tibs56ds
008100030926     C                   MOVEA     SKI           FIL
008200030926      *
008300030926     C                   ENDSR
008400030926      **-------------------------------------------------------------**
