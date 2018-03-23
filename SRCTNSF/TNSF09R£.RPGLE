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
002700120416     d comman1         s             91
002800120416     d lun1            s             15  5 inz(91)
002900120416
003000110926     D KPJBA         E DS
003100120416      *------------------------------------------------------------------------*
003200120416      *   S C H I E R E
003300120416      *------------------------------------------------------------------------*
003400120416
003500120416      * Ovrprtf
003600150909     d cmdc            s             91    dim(2) ctdata perrcd(1)
003700110926      *****************************************************************
003800110926     c     *ENTRY        PLIST
003900110926     c                   PARM                    KPJBA
004000110926     c                   MOVEL     *ZEROS        PARAM
004100110926     c                   MOVEL     KPJBU         PARAM
004200151013
004300151013     c                   move      viddsp        dataeur
004400151013     c                   move      dataeur       dataiso
004500151013     c                   move      dataiso       dataspe
004600151013     c                   movel     dataspe       com8a             8
004700120215
004800120215      * lancio manca tariffa solo se fatturazione mensile
004900120224     C                   If        vidtft = 4
005000120416
005100120416     c                   clear                   comman1
005200120416     c                   movea     cmdc(1)       comman1
005300120416     c                   eval      %subst(comman1:64:10) = 'F' + com8a
005400120416     c                   Call      'QCMDEXC'                            99
005500120416     c                   Parm                    Comman1
005600120416     c                   Parm                    Lun1
005700150917     c                   endif
005800150909
005900150917      * COPIO il file delle variazioni peso desunto sempre anche nelle settimanali
006000150909     c                   clear                   comman1
006100150909     c                   movea     cmdc(2)       comman1
006200150917      * fatturazione mensile
006300150917     C                   If        vidtft = 4
006400150909     c                   eval      %subst(comman1:64:10) = 'F' + com8a
006500150917     c                   endif
006600150917      * fatturazione settimanale
006700150917     C                   If        vidtft = 1
006800150917     c                   eval      %subst(comman1:64:10) = 'S' + com8a
006900150917     c                   endif
007000150917
007100150909     c                   Call      'QCMDEXC'                            99
007200150909     c                   Parm                    Comman1
007300150909     c                   Parm                    Lun1
007400120416
007500110926     C*
007600110926     c                   seton                                        lr
007700120416** cmdc
007800120416CPYF FROMFILE(QTEMP/WFMTC00F) TOFILE(GAITRAAZM/WFMTC00F) TOMBR(          ) MBROPT(*REPLACE)
007900160428CPYF FROMFILE(QTEMP/WFVPD10F) TOFILE(GAITRAAZM/WFVPD10F) TOMBR(          ) MBROPT(*REPLACE)
