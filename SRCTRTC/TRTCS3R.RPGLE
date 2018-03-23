000100010221     H DECEDIT('0,') DATEDIT(*DMY.)
000200020808
000300070221     Ffibar00f  O  A E           K DISK
000400070123     Ffnbrv02l  IF   E           K DISK
000500070123     Ffnbrv01l  IF   E           K DISK
000600070123     F                                     RENAME(Fnbrv000:Fnbrv001)
000700010221     Ffnfgv01l  IF   E           K DISK
000800020808
000900010221     D KPJBA         E DS
001000010221     D trtcs2ds      E DS
001100020808     D***
001200070221     D wtimez          S               z
001300070221     D dataiso         S               d   datfmt(*iso)
001400070221     D oraiso          S               t   timfmt(*iso)
001500070221
001600070221     D                SDS
001700070221     D  NOMPGM                 1     10
001800010221     c*
001900010221     c* per sicurezza controllo che il foglio di defluenza sia presente
002000010221     c* e sia aperto
002100020808     ***c                   z-add     ds2nfd        w0050             5 0
002200020808     ***c     w0050         chain     fnfgv01l                           30
002300020808     c                   z-add     DS2nfd        FGVnfv
002400020808     c                   z-add     DS2fgd        FGVlnp
002500020808     c     K02FGV01      chain     FNFGV01L                           30
002600010221     c     *in30         ifeq      *on
002700010221     c     fgvfcf        orne      *blanks
002800010221     c                   goto      fine
002900010221     c                   endif
003000010221     c
003100070123     C     kbrv          setll     fnbrv02l
003200070123     C     kbrv          reade     fnbrv02l                               30
003300010221     C     *in30         doweq     *off
003400070123     c     kbrv1         setll     fnbrv01l                               31
003500010221     c     *in31         ifeq      *off
003600010221     c* autogenero spunta solo se non esiste
003700010221     c                   z-add     brvlnp        barlnp
003800010221     c                   z-add     brvlna        barlna
003900010221     c                   z-add     brvnrs        barnrs
004000010221     c                   z-add     brvnsc        barnsc
004100010221     c                   z-add     brvznc        barznc
004200010221     c                   z-add     ds2nfd        barnfv
004300010222     c                   z-add     88            barnps
004400010221     c                   z-add     ds2npd        barnpg
004500070221     c                   time                    wtimez
004600070221     c* millisecondi
004700070221     c                   extrct    wtimez:*ms    millisec          6 0
004800070221     c                   movel     millisec      barmis
004900070221     c* data immissione
005000070221     c                   movel     wtimez        dataiso
005100070221     c                   movel     dataiso       bardfs
005200070221     c* ora immissione
005300070221     c                   move      wtimez        oraiso
005400070221     c                   movel     oraiso        barhms
005500070221     c*
005600070221     c                   movel     'A'           bartap
005700070221     c                   eval      barpru='§'+ nompgm
005800010221     c                   write     barco
005900010221     c                   endif
006000070123     C     kbrv          reade     fnbrv02l                               30
006100010221     C                   enddo
006200070221     c                   feod      fibar00f
006300010221     c* richiamo pgm di caricamento spunte
006400070221     c                   move      ds2fgd        parfil
006500070221     c                   call      'FNLS45C5'
006600070221     c                   parm                    parfil            3
006700070221     c                   parm      'FIBAR00F'    parfile          10
006800010221     c
006900010221     c     fine          tag
007000020808     c                   eval      *inlr = *on
007100010221     c*
007200010221     c     *inzsr        begsr
007300010221     c     kbrv          klist
007400010221     c                   kfld                    ds2npg
007500010221     c                   kfld                    ds2nfv
007600010221     c                   kfld                    ds2fgs
007700010221     c                   kfld                    ds2lna
007800010221     c     kbrv1         klist
007900010221     c                   kfld                    ds2npd
008000010221     c                   kfld                    ds2nfd
008100010221     c                   kfld                    ds2fgd
008200010221     c                   kfld                    brvlnp
008300010221     c                   kfld                    brvlna
008400010221     c                   kfld                    brvnrs
008500010221     c                   kfld                    brvnsc
008600020808     c     K02FGV01      klist
008700020808     c                   kfld                    FGVnfv
008800020808     c                   kfld                    FGVlnp
008900010221     c*
009000010221     C     *ENTRY        PLIST
009100010221     C                   PARM                    KPJBA
009200010221     C                   MOVEL     KPJBU         trtcs2ds
009300010221     c                   endsr
