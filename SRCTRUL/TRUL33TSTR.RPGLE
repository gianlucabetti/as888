000100010911     H DECEDIT('0,') DATEDIT(*DMY.)
000200010905
000300020226     FTRUL33TSTDCF   E             WORKSTN
000400010905     F                                     INFDS(DSFMT)
000500010905
000600020226     D*-------------
000700020226     D KPJBA         E DS                  inz
000800011011     D*-------------
000900020226     D* Parametri per pgm. TRUL33R
001000020226     D TRUL33DS      E DS                  inz
001100020313     D  I33ope       e                     inz(1)
001200011011     D*-------------
001300010905     D DSFMT           DS           512
001400010905     D  $TASTO               369    369
001500010905     D  NRG                  370    370
001600010905     D  NCL                  371    371
001700010905     D  SFLNRR               378    379B 0
001800010905     D*-------------
001900010905     D* Reperimento nome PGM
002000010905     D STATUS         SDS           333
002100010905     D  DSPGM            *PROC
002200010905     D  PARMS            *PARMS
002300020226     D  USER                  81     90
002400020226     D*-------------
002500020226     D WKPJBA          s                   like(KPJBA)
002600010905     D*-------------
002700010905     D* Tasti di funzione
002800010905     D F01             C                   CONST(X'31')
002900010905     D F02             C                   CONST(X'32')
003000010905     D F03             C                   CONST(X'33')
003100010905     D F04             C                   CONST(X'34')
003200010905     D F05             C                   CONST(X'35')
003300010905     D F06             C                   CONST(X'36')
003400010905     D F07             C                   CONST(X'37')
003500010905     D F08             C                   CONST(X'38')
003600010905     D F09             C                   CONST(X'39')
003700010905     D F10             C                   CONST(X'3A')
003800010905     D F11             C                   CONST(X'3B')
003900010905     D F12             C                   CONST(X'3C')
004000010905     D F13             C                   CONST(X'B1')
004100010905     D F14             C                   CONST(X'B2')
004200010905     D F15             C                   CONST(X'B3')
004300010905     D F16             C                   CONST(X'B4')
004400010905     D F17             C                   CONST(X'B5')
004500010905     D F18             C                   CONST(X'B6')
004600010905     D F19             C                   CONST(X'B7')
004700010905     D F20             C                   CONST(X'B8')
004800010905     D F21             C                   CONST(X'B9')
004900010905     D F22             C                   CONST(X'BA')
005000010905     D F23             C                   CONST(X'BB')
005100010905     D F24             C                   CONST(X'BC')
005200010905     D ENTER           C                   CONST(X'F1')
005300010905     D ROLDWN          C                   CONST(X'F4')
005400010905     D ROLLUP          C                   CONST(X'F5')
005500010905
005600020226     C     *ENTRY        plist
005700020226     C                   parm                    wKPJBA
005800020226     C*
005900020226     C                   if        PARMS = 0
006000020226     C                   clear                   KPJBA
006100020226     C                   eval      KNMUS = USER
006200020226     C***                eval      KNSIF = ???
006300020226     C                   else
006400020226     C                   movel     wKPJBA        KPJBA
006500020226     C                   endif
006600020226     C                   reset                   TRUL33DS
006700020226     C*
006800020226     C                   doU       $TASTO = F03
006900010905     C*
007000020226     C                   write     TRUL33T1
007100020226     C                   write     TRUL33Z1
007200020226     C                   exfmt     TRUL33D1
007300020226     C*
007400020306     C                   if        $TASTO = F03   or
007500020306     C                             $TASTO = F12
007600020226     C                   leave
007700020226     C                   endif
007800021206     C                   if        $TASTO = F23
007900021206     C                   eval      *in33 = *on
008000021206     C                   iter
008100021206     C                   endif
008200010905     C*
008300020226     C                   exsr      CallPGM
008400020226     C                   write     TRUL33D2
008500020226     C*
008600020226     C                   doU       $TASTO = F03   or
008700020226     C                             $TASTO = F12
008800010907     C                   exfmt     PROTECT
008900020226     C                   endDO
009000010905     C*
009100021206     C                   eval      *in33 = *off
009200021206     C                   reset                   TRUL33DS
009300010905     C                   endDO
009400010905     C*
009500020313     C                   reset                   TRUL33DS
009600020313     C                   eval      I33tla = 'C'
009700020313     C                   exsr      CallPGM
009800020313     C                   eval      *inLR  = *on
009900020313
010000010905      ************************************************************
010100020226      * Richiamo pgm. TRUL33R
010200010905      ************************************************************
010300020226     C     CallPGM       Begsr
010400011011     C*
010500020226     C                   movel(p)  TRUL33DS      KPJBU
010600020226     C*
010700020226     C                   call      'TRUL33R'
010800020226     C                   parm                    KPJBA
010900020226     C*
011000020226     C                   movel     KPJBU         TRUL33DS
011100010910     C*
011200011011     C                   ENDSR
