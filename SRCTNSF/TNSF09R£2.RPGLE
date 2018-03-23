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
002601120416     d comman1         s             91
002602120416     d lun1            s             15  5 inz(91)
002603120416
002700110926     D KPJBA         E DS
002701120416      *------------------------------------------------------------------------*
002702120416      *   S C H I E R E
002703120416      *------------------------------------------------------------------------*
002704120416
002705120416      * Ovrprtf
002707120416     d cmdc            s             91    dim(1) ctdata perrcd(1)
002900110926      *****************************************************************
003000110926     c     *ENTRY        PLIST
003100110926     c                   PARM                    KPJBA
003200110926     c                   MOVEL     *ZEROS        PARAM
003300110926     c                   MOVEL     KPJBU         PARAM
003400120215
003500120215      * lancio manca tariffa solo se fatturazione mensile
003600120224     C                   If        vidtft = 4
003601120416
003602120416     c                   move      viddsp        dataeur
003603120416     c                   move      dataeur       dataiso
003604120416     c                   move      dataiso       dataspe
003605120416     c                   movel     dataspe       com8a             8
003606120416
003607120416     c                   clear                   comman1
003608120416     c                   movea     cmdc(1)       comman1
003609120416     c                   eval      %subst(comman1:64:10) = 'F' + com8a
003610120416     c                   Call      'QCMDEXC'                            99
003611120416     c                   Parm                    Comman1
003612120416     c                   Parm                    Lun1
003613120416
007100120215     c                   endif
007200110926     C*
007300110926     c                   seton                                        lr
007400120416** cmdc
007500120416CPYF FROMFILE(QTEMP/WFMTC00F) TOFILE(GAITRAAZM/WFMTC00F) TOMBR(          ) MBROPT(*REPLACE)
