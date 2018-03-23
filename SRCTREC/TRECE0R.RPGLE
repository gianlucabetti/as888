000100991227     H*------------------------------------------------------------------------*
000200991227     H* CHIUDE I SIOTTOSISTEMI IN MODO ORDINATO                                *
000300991227     H*------------------------------------------------------------------------*
000400991227     H DATEDIT(*YMD)
000500991227     D*------------------------------------------------------------------------*
000600991227     D* INPUT                                                                  *
000700991227     D*------------------------------------------------------------------------*
000800991227     D*---
000900991227     D* DS LANCIO
001000991227     D*---
001100991227     D DS£ECO        E DS
001200991227     D*---
001300991227     D* DS INFORMAZIONI JOB
001400991227     D*---
001500991227     D                SDS
001600991227     D  JOB                  244    253
001700991227     D  USER                 254    263
001800991227     D*---
001900991227     D* CONTROLLO DATA (8)
002000991227     D*---
002100991227     D WLBDA8          DS                  INZ
002200991227     D  G08DAT                 1      8  0
002300991227     D  G08INV                 9     16  0
002400991227     D  G08ERR                17     17
002500991227     D  G08TGI                18     22  0
002600991227     D*---
002700991227     D* COSTANTI
002800991227     D*---
002900991227     D CPGF            C                   CONST('TRECE0R  ')
003000991227     D CFIL            C                   CONST('TITAS10F ')
003100991227     D CMBR            C                   CONST('TITAS10F ')
003200991227     D CLIB            C                   CONST('GAITRAGRU')
003300991227     I*------------------------------------------------------------------------*
003400991227     I* INPUT                                                                  *
003500991227     I*------------------------------------------------------------------------*
003600991227     C*
003700991227     C* REPERISCE DATA E ORA CORRENTE
003800991227     C                   TIME                    N14              14 0
003900991227     C                   MOVEL     N14           ORACOR            6 0
004000991227     C                   MOVE      N14           N8                8 0
004100991227     C                   Z-ADD     N8            G08DAT
004200991227     C                   Z-ADD     *ZEROS        G08INV
004300991227     C                   MOVEL     '0'           G08ERR
004400991227     C                   CALL      'XSRDA8'
004500991227     C                   PARM                    WLBDA8
004600991227     C                   Z-ADD     G08INV        DATCOR            6 0
004700991227     C*
004800991227     C* IMPOSTA DS DI LANCIO
004900991227     C                   CLEAR                   DS£ECO
005000991227     C                   MOVEL     'S'           D£GSTS
005100991227     C                   Z-ADD     DATCOR        D£GDAT
005200991227     C                   Z-ADD     ORACOR        D£GTIM
005300991227     C                   MOVEL     USER          D£GUSR
005400991227     C                   MOVEL     CPGF          D£GPGF
005500991227     C                   MOVEL     CFIL          D£GFIL
005600991227     C                   MOVEL     CMBR          D£GMBR
005700991227     C                   MOVEL     CLIB          D£GLIB
005800991227     C                   MOVEL     'N'           D£GPDE
005900991227     C                   MOVEL     *BLANKS       D£GXXX
006000991227     C                   MOVEL     *BLANKS       D£PRE1
006100991227     C                   MOVEL     *BLANKS       D£PRE2
006200991227     C                   MOVEL     *BLANKS       D£DRE1
006300991227     C                   MOVEL     *BLANKS       D£DRE2
006400991227     C*
006500991227     C* CHIUDE IL SOTTOSISTEMA BARTOLINI
006600991227     c                   EVAL      %SUBST(ds£eco:570:1)  = 'B'
006700991227     c                   EVAL      %SUBST(ds£eco:1070:1) = 'B'
006800020619     C                   CALL      'TRECD2R'
006900991227     C                   PARM                    DS£ECO
007000991227     C*
007100991227     C                   SETON                                            LR
