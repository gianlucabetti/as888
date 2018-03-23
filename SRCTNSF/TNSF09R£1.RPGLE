000100110926     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) ACTGRP(QILE)
000200110926     H*PARMS BNDDIR(PJXBND PJCBND)
000300110926     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000400120215     H/TITLE    -- FATTURAZIONE AUTOMATICA --  STAMPA MANCA TARIFFA  *
000500110926     H*--------------------------------------------------------------*
000600110926     H**
000700110926     D PARAM           DS
000800110926     D  VIDDFI                        8  0
000900110926     D  VIDDFF                        8  0
001000110926     D  VIDDSP                        8  0
001100110926     D  VIDTFT                        1  0
001200110926     D  VIDTCL                        1  0
001300110926     D  VIDTSC                        1
001400110926     D  FIL                           3  0 dim(40)
001500110926     D  VIDRB                         2
001600110926     D  VIDDFT                        8  0
001700110926     D  VIDKSC                        7  0
001800110926     D  wsocieta                      3
001900110926     D  VIDDIV                        3
002000110926     D  VIDAGG                        1
002100110926     D  VIDSTA                        1
002200110926     D
002300120215     d dataiso         s               d   datfmt(*iso)
002400120215     d dataeur         s               d   datfmt(*eur)
002500120215     D Dataspe         s              8  0
002600120215     D
002700110926     D KPJBA         E DS
002800120215     D TNSB44DS      E DS
002900110926      *****************************************************************
003000110926     c     *ENTRY        PLIST
003100110926     c                   PARM                    KPJBA
003200110926     c                   MOVEL     *ZEROS        PARAM
003300110926     c                   MOVEL     KPJBU         PARAM
003400120215
003500120215      * lancio manca tariffa solo se fatturazione mensile
003600120224     C                   If        vidtft = 4
003700110926     C*
003800120215     C* stampa manca tariffa se fattura mensile
003900120215     c                   clear                   tnsb44ds
004000120215
004100120215     c                   move      viddsp        dataeur
004200120215     c                   move      dataeur       dataiso
004300120215     c                   move      dataiso       dataspe
004400120215     c                   move      00010101      D44dsd
004500120215     c                   move      dataspe       D44dsa
004600120215      * creazione file
004700120215     c                   eval      d44crf = 'S'
004800120215      *
004900120215     c                   movel     d44dsa        com8a             8
005000120215     c                   eval      D44mbr = 'F'+ com8a
005100120215      * stampa imponibile
005200120215     c                   eval      d44fsi = 'N'
005300120215      * stampa separata per P.O.
005400120215     c                   eval      d44spp = 'S'
005500120215      * stampa unica
005600120215     c                   eval      d44sun = 'N'
005700120215      * coda di stampa
005800120215     c                   eval      d44out = 'O'
005900120215      * totali per area
006000120215     c                   eval      d44tot = 'S'
006100120215      * stampante totali per area
006200120215     c                   eval      d44Tou = 'T'
006300120215
006400120215      * LANCIO
006500120215     c                   clear                   kpjbu
006600120215     c                   movel     tnsb44ds      Kpjbu
006700120215
006800120215     c                   call      'TNSB46R'
006900120215     c                   parm                    kpjba
007000120215
007100120215     c                   endif
007200110926     C*
007300110926     c                   seton                                        lr
