000100021016     h Decedit('0,') Datedit(*YMD.) option(*nodebugio)
000200021016
000300021016     fTntbe01l  if a e           k Disk
000400021016
000500021016      * DS per scomporre la data area FNLSC1
000600021016      *    C1DAPE  data prima elaborazione statistica
000700021016      *    C1DAUE  data ultima elaborazione statistica
000800021016     d                 DS
000900021016     d  FNLSC1                 1     16
001000021016     d  C1DAPE                 1      8  0
001100021016     d  C1DAUE                 9     16  0
001200021016      * DS per scomporre la data area FNLSC2
001300021016      *    C2DAPE  data prima elaborazione statistica
001400021016      *    C2DAUE  data ultima elaborazione statistica
001500021016     d                 DS
001600021016     d  FNLSC2                 1     16
001700021016     d  C2DAPE                 1      8  0
001800021016     d  C2DAUE                 9     16  0
001900021016      * DS per scomporre la data area FNLSC3
002000021016      *    C3DAPE  data prima elaborazione statistica
002100021016      *    C3DAUE  data ultima elaborazione statistica
002200021016     d                 DS
002300021016     d  FNLSC3                 1     16
002400021016     d  C3DAPE                 1      8  0
002500021016     d  C3DAUE                 9     16  0
002600021016      * DS per scomporre la data area FNLSC5
002700021016      *    C5DAPE  data prima elaborazione statistica
002800021016      *    C5DAUE  data ultima elaborazione statistica
002900021016     d                 DS
003000021016     d  FNLSC5                 1     16
003100021016     d  C5DAPE                 1      8  0
003200021016     d  C5DAUE                 9     16  0
003300021016      * DS per scomporre la data area FNLRE6
003400021016      *    E6DAPE  data prima elaborazione statistica
003500021016      *    E6DAUE  data ultima elaborazione statistica
003600021016     d                 DS
003700021016     d  FNLRE6                 1     16
003800021016     d  E6DAPE                 1      8  0
003900021016     d  E6DAUE                 9     16  0
004000021016
004100021016      * Ds date statistiche
004200021016     d dSdf          e ds                  inz
004300021016
004400021016     d Ut§dse0f      e ds
004500021016     d Cncr80        e ds
004600021016     d Kpjba         e ds
004700021016
004800021016     c     Ktbe          Klist
004900021016     c                   Kfld                    TbeCod
005000021016     c                   Kfld                    TbeKe1
005100021016
005200021016     c     *DTAARA       DEFINE                  FNLSC1
005300021016     c                   IN        FNLSC1
005400021016     c     *DTAARA       DEFINE                  FNLSC2
005500021016     c                   IN        FNLSC2
005600021016     c     *DTAARA       DEFINE                  FNLSC3
005700021016     c                   IN        FNLSC3
005800021016     c     *DTAARA       DEFINE                  FNLSC5
005900021016     c                   IN        FNLSC5
006000021016     c     *DTAARA       DEFINE                  FNLRE6
006100021016     c                   IN        FNLRE6
006200021016
006300021016      * Reperimento parametri aziendali
006400021016     c                   Z-Add     1             CODUT
006500021016     c                   CALL      'X§PARUT'
006600021016     c                   PARM                    Ut§dse0f
006700021016     c                   MOVEL     REC80         CNCR80
006800021016
006900021016     c                   Eval      TbeCod = 'SDF'
007000021016     c                   Movel(p)  Simfel        TbeKe1
007100021016     c     Ktbe          Chain     Tntbe01l
007200021016     c                   If        Not %Found(Tntbe01l)
007300021016     c                   Z-Add     E6daPe        §SdfArrP
007400021016     c                   Z-Add     E6daUe        §SdfArrU
007500021016     c                   Z-Add     C1daPe        §SdfParP
007600021016     c                   Z-Add     C1daUe        §SdfParU
007700021016     c                   Z-Add     C2daPe        §SdfTerP
007800021016     c                   Z-Add     C2daUe        §SdfTerU
007900021016     c                   Z-Add     C3daPe        §SdfEntP
008000021016     c                   Z-Add     C3daUe        §SdfEntU
008100021016     c                   Z-Add     C5daPe        §SdfBolP
008200021016     c                   Z-Add     C5daUe        §SdfBolU
008300021016     c                   Movel     dSdf          TbeUni
008400021016     c                   Write     Tntbe000
008500021016     c                   EndIf
008600021016
008700021016     c                   Eval      *InLr = *On
