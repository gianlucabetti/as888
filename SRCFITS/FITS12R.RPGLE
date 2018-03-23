000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040805      * FITS12R  *----------------------------------------------------*
000400040805      *  BOLLE TRANSITO
000500021010      *---------------------------------------------------------------* c
000600040805     fFnBtpNEW  Uf A e           k disk    rename(FNBTP000:BTPNEW)
000700040805     fFnBtp11l  If   e           k disk
000800021007      *
000900040805     fFnBttNEW  Uf A e           k disk    rename(FNBTT000:BTTNEW)
001000040805     fFnBtt11l  If   e           k disk
001100040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001200021008      *
001300021010     d KPJBA         E DS
001400040805     D* Schiera p.o. da trasferire
001500040805     D  SKPO           s              3    dim(500)
001600040805      *
001700021008      *****************************************************************
001800021010     c     *ENTRY        PLIST
001900021010     c                   PARM                    KPJBA
002000040805     C                   parm                    PARAM          1500
002100040805     c
002200040805     c     ' ':'0'       xlate     param         param
002300040805     c                   movea     param         skpo
002400040805     C*
002500021011     c     KBtp          KLIST
002600040805     c                   KFLD                    btpFlp
002700040805     c                   KFLD                    btpAAS
002800040805     c                   KFLD                    btpLNP
002900040805     c                   KFLD                    btpNRS
003000040805     c                   KFLD                    btpNSP
003100021009      *
003200021010      *---------------------------------------------------------------*
003300021008      ****
003400021010      ** LETTURA FILE BOLLE TRANSITI:TESTATA
003500021008      ****
003600021008      *
003700040805     c                   READ      FnBtp11l                               22
003800021010     c     *IN22         DOWEQ     *OFF
003900040805     c
004000040805     C* LEGGO TUTTE LE BOLLE TRANSITO E TRASFERISCO QUelle con:
004100040805     c*   LNP LNA o FLP = alle linne da trasferire
004200040805     c                   movel     btplnp        wlinea            3
004300040805     c     wlinea        lookup    skpo                                   35
004400040805     c                   if        not *in35
004500040805     c                   movel     btplna        wlinea            3
004600040805     c     wlinea        lookup    skpo                                   35
004700040805     c                   if        not *in35
004800040805     c                   movel     btpflp        wlinea            3
004900040805     c     wlinea        lookup    skpo                                   35
005000040805     c                   endif
005100040805     c                   endif
005200021008      *
005300021010      * Verifico se su file nuovo esistono records testata e dettaglio
005400040805     c                   if        *in35
005500040805     c     kbtp          setll     fnbtpnew                               31
005600040805     c                   if        not *in31
005700040805     c                   write     BTPNEW                               61
005800040805     c                   add       1             $TotBtp           7 0
005900040805      *
006000040805      * Bolle transito:dettaglio
006100040805     C     Kbtp          setll     FnBtt11l
006200040805     C     Kbtp          reade     FnBtt11l                               36
006300040805     c     *in36         doweq     *off
006400040805     c                   write     BTTNEW                               61
006500040805     c                   add       1             $TotBtt           7 0
006600040805     C     Kbtp          reade     FnBtt11l                               36
006700040805     c                   enddo
006800040805     c                   endif
006900040805     c                   endif
007000021008      *
007100040805     c                   READ      FnBtp11l                               22
007200021010     c                   ENDDO
007300021010      *
007400040805     c                   EXCEPT    RINUM1
007500021011     c                   seton                                        LR
007600021009      *
007700021010      *
007800021010      *---------------------------------------------------------------*
007900040805     oQSYSPRT   E            RINUM1         2 02
008000040805     o                                         +  5 'BOLLE TRANSITO'
008100040805     o                                         +  5 'Fits12R'
008200021010     o                       UDATE             +  2 '  /  /  '
008300021010     o          E            RINUM1      1
008400021011     o                                         +  3 'TOT.Testata bolle scritte:'
008500030521     o                       $TOTBTP       2   +  2
008600021011     o                                         +  3 'TOT. Detta.  bolle scitte:'
008700030521     o                       $TOTBTT       2   +  2
