000100020911      *--------------------------------------------------------------*
000200040227      *    crea numeratore 10 dal num. 17 per tutti i p.o.           *
000300020911      *--------------------------------------------------------------*
000400040227     Fazorg01l  if   e           k disk
000500020911
000600040227     D kpjba         e ds
000700020911     D trul33ds      e ds
000800040227     D FNLV55DS      e ds
000900040227     D WNRI            s                   LIKE(O33NRI)
001000040227     D WLBDAT          DS
001100040227     D  G02DAT                 1      8  0
001200040227     D  G02INV                 9     16  0
001300040227     D  G02ERR                17     17
001400040227     D  G02TGI                18     22  0
001500020702      *---------------------------------------------------------------*
001600040227     C                   TIME                    UTIME            14 0          ORA E DATA
001700040227     C                   MOVE      UTIME         UDATE8            8 0          GG/MM/AAAA
001800040227     C                   Z-ADD     UDATE8        G02DAT
001900040227     C                   MOVEL     *BLANK        G02ERR
002000040227     C                   CALL      'XSRDA8'
002100040227     C                   PARM                    WLBDAT
002200040227     C                   Z-ADD     G02INV        DATEU             8 0          AAAA/MM/GG
002300040227     C*
002400040227     c     *loval        setll     azorg01l
002500040227     c                   read      azorg01l                               30
002600040227     c                   dow       not *in30
002700040227     C* ELABORO SOLO F A NON ANNULLATI
002800040227     C                   IF        ORGFAG='A' OR ORGFAG='F'
002900040227     C                   IF        ORGFVA=' '
003000040227     C* NON ELABORO SE TERMINAL DI PARTENZA
003100040227     c                   clear                   FNLV55DS
003200040227     C                   MOVEL     'P'           D55TPT
003300040227     C                   MOVEL     ORGFIL        D55LIN
003400040227     C                   MOVEL     DATEU         D55DRF
003500040227     C                   CALL      'FNLV55R'
003600040227     C                   PARM                    FNLV55DS
003700040227     C                   IF        D55TFP<>ORGFIL
003800040227     c                   clear                   trul33ds
003900040227     c                   z-add     1             i33ope
004000040227     c                   z-add     17            i33cnu
004100040227     c                   z-add     ORGFIL        i33po1
004200040227     c                   movel     trul33ds      kpjbu
004300040227     c                   call      'TRUL33R'
004400040227     c                   PARM                    kpjba
004500040227     c                   movel     kpjbu         trul33ds
004600040227     C                   IF        O33ERR=00
004700040227     C* PRENDO L'ULTIMO NUMERO E CREO IL RECORD NUMERATORE 10
004800040227     C                   EVAL      WNRI=O33NRI
004900040227     c                   clear                   trul33ds
005000020911     c                   z-add     2             i33ope
005100040227     c                   z-add     10            i33cnu
005200040227     c                   z-add     ORGFIL        i33po1
005300040227     c                   z-add     WNRI          i33unp
005400020911     c                   movel     trul33ds      kpjbu
005500020911     c                   call      'TRUL33R'
005600020911     c                   PARM                    kpjba
005700020911     c                   movel     kpjbu         trul33ds
005800020911     C*
005900020911     c                   endif
006000040227     c                   endif
006100040227     c                   endif
006200040227     c                   endif
006300040227     C
006400040227     c                   read      azorg01l                               30
006500020911     c                   ENDDO
006600020911
006700020911     C                   eval      *inlr = *on
