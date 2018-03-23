000100020911      *--------------------------------------------------------------*
000200020911      *    crea numeratori nuova gestione prendendo l'ultimo         *
000300020911      *    numero gestito su flnuf
000400020912      *    deve essere passato il codice
000500020912      *    numeratore  3
000600020911      *
000700020911      *    PER TRASFERIMENTO FILIALE DA AS/400 AD AS UNICO           *
000800020911      *--------------------------------------------------------------*
000900020911     Fflnuf01l  if   e           k disk
001000020911     fqsysprt   o    f  132        printer infds(stamp)
001100020911
001200020911     D kpjba         e ds
001300020911     D* DS PER TRUL33R - REPERIMENTO/AGGIORNAMENTO NUMERATORI SU AZNUM
001400020911     D trul33ds      e ds
001500020911     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001600020911     D CNCR80        E DS
001700020911
001800020911     D trul06ds      e ds
001900020911     d  lin                    1     90  0 dim(30)
002000020911
002100020912     D numeratore      s              3
002200020911
002300020801     d stamp           ds
002400020801     d  line                 367    368b 0
002500020702      *---------------------------------------------------------------*
002600020911     c                   do        30            i                 3 0
002700020911      *loop con le chiavi delle tabelle personalizzate per filiale
002800020911     c                   if        lin(i) = 0
002900020911     c                   leave
003000020911     c                   endif
003100020911     C     knuf          chain     flnuf01l                           99
003200020911     C   99knufnopo      chain     flnuf01l                           99
003300020911     c                   if        not *in99
003400020911     c                   clear                   trul33ds
003500020911     c                   move      'L'           i33tla
003600020911     c                   z-add     2             i33ope
003700020912     c                   z-add     numerat       i33cnu
003800020911     c                   z-add     lin(i)        i33po1
003900020911     c                   z-add     nufnum        i33unp
004000020911     c                   movel     trul33ds      kpjbu
004100020911     c                   call      'TRUL33R'
004200020911     c                   PARM                    kpjba
004300020911     c                   movel     kpjbu         trul33ds
004400020911      *stampo numeratore creato
004500020911     c                   if        line > 60
004600020911     c                   except    testa
004700020911     c                   endif
004800020911     c                   except    nume
004900020911     C*
005000020911     c                   endif
005100020911     c                   ENDDO
005200020911
005300020911     C                   eval      *inlr = *on
005400020911      *-----------------------------------------
005500020911     c     *inzsr        begsr
005600020911      *---------------------------------------------------------------*
005700020911      *
005800020911     C     *ENTRY        PLIST
005900020912     C                   PARM                    numeratore
006000020912     C                   move      numeratore    numerat
006100020911     C                   Z-ADD     1             CODUT
006200020911     C                   CALL      'X§PARUT'
006300020911     C                   PARM                    UTEDSE
006400020911     C                   MOVEL     REC80         CNCR80
006500020911      *
006600020911     C     Knuf          klist
006700020911     C                   kfld                    anno
006800020912     C                   kfld                    numerat
006900020911     C                   kfld                    lin(i)
007000020911     c                   z-add     uyear         anno
007100020911     C     Knufnopo      klist
007200020911     C                   kfld                    anno              2 0
007300020912     C                   kfld                    numerat           3 0
007400020911     C* CARICO I P.O. DI AREA
007500020911     C                   CLEAR                   trul06ds
007600020911     C                   MOVE      '£1'          D06COD
007700020911     C                   MOVEL     'L'           D06TLA
007800020911     C                   MOVEL     simfel        D06KEY
007900020911     C                   MOVEL     trul06ds      KPJBU
008000020911     C                   CALL      'TRUL06R'
008100020911     C                   PARM                    KPJBA
008200020911     C                   MOVEL     KPJBU         trul06ds
008300020911     C                   Z-ADD     99            LINE
008400020911      *
008500020911     c                   endsr
008600020911     O*------------------------------------------------------*
008700020911     OQSYSPRT   e            TESTA             2
008800020911     O                                           70 'Numeratori creati in fase -
008900020911     O                                              di conversione '
009000020911     o                                          + 1 'Terminal'
009100020911     o                       simfel             + 1
009200020911
009300020911     O          e            nume        1
009400020911     o                                           17 'Numeratore x  P.O'
009500020911     o                       lin(i)             + 1
009600020911     o                       o33aaa             + 1
009700020911     o                       i33cnu             + 1
009800020911     o                       o33po1             + 1
009900020911     o                       o33nri        z    + 1
010000020911     o                       o33err             + 1
010100020911     o                       o33msg             + 1
010200020702      *
