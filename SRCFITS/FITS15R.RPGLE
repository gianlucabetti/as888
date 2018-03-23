000100040805     h decedit('0,') datedit(*ymd.)
000200021010      *
000300040910      * FITS15R  *----------------------------------------------------*
000400040910      *  contrassegni: FNCSB
000500021010      *---------------------------------------------------------------* c
000600040910     fFnCSBNEW  Uf A e           k disk    rename(FNCSB000:CSBNEW)
000700040910     fFnCSB00F  If   e             disk
000800040805     FqSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000900021008      *
001000021010     d KPJBA         E DS
001100040805     D* Schiera p.o. da trasferire
001200040805     D  SKPO           s              3    dim(500)
001300040805      *
001400021008      *****************************************************************
001500021010     c     *ENTRY        PLIST
001600021010     c                   PARM                    KPJBA
001700040805     C                   parm                    PARAM          1500
001800040805     c
001900040805     c     ' ':'0'       xlate     param         param
002000040805     c                   movea     param         skpo
002100040805     C*
002200040910     c     KCSB          KLIST
002300040910     c                   KFLD                    csbAAS
002400040910     c                   KFLD                    csbLNP
002500040910     c                   KFLD                    csbNRS
002600040910     c                   KFLD                    csbNSP
002700021010      *---------------------------------------------------------------*
002800021008      ****
002900040910      ** LETTURA
003000021008      ****
003100040910     c                   READ      Fncsb00f                               22
003200040910    1c     *IN22         DOWEQ     *OFF
003300040805     c
003400040910     C* TRASFERISCO QUelli con:
003500040910     c*   LNP o LNP CLIENTE = a una delle linee da trasferire
003600040910     c                   movel     csblnp        wlinea            3
003700040805     c     wlinea        lookup    skpo                                   35
003800040910    2c                   if        not *in35
003900040910     C                   MOVE      CSBCDI        TIP               4 0
004000040910     C                   MOVEL     CSBCDI        APP               3
004100040910     c     app           lookup    skpo                                   35
004200040910    3c                   if        *in35   and tip=9999
004300040910     c                   setoff                                       35
004400040910    3c                   endif
004500040910    3c                   if        *in35   and tip=8888
004600040910     c                   setoff                                       35
004700040910    3c                   endif
004800040910    2c                   endif
004900040910     c* 35 on trasferisco
005000040910     c
005100040910      * Verifico se su file nuovo esiste già il record
005200040910    2c                   if        *in35
005300040910     c     kcsb          setll     fncsbnew                               31
005400040910    3c                   if        not *in31
005500040910     c                   write     CSBNEW                               61
005600040910     c                   add       1             $Totcsb           7 0
005700040910    3c                   endif
005800040910    2c                   endif
005900021008      *
006000040910     c                   READ      Fncsb00f                               22
006100040910    1c                   ENDDO
006200021010      *
006300040805     c                   EXCEPT    RINUM1
006400021011     c                   seton                                        LR
006500021009      *
006600021010      *
006700021010      *---------------------------------------------------------------*
006800040805     oQSYSPRT   E            RINUM1         2 02
006900040910     o                                         +  5 'CONTRASSEGNI'
007000040910     o                                         +  5 'Fits15R'
007100021010     o                       UDATE             +  2 '  /  /  '
007200021010     o          E            RINUM1      1
007300040910     o                                         +  3 'TOTALE FNCSB00F:'
007400040910     o                       $TOTCSB       2   +  2
