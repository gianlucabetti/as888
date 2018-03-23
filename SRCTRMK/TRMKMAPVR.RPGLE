000100030723     H DECEDIT('0,') DATEDIT(*DMY/)
000200081121     H* trmkmapvr *---------------------------------------------------*
000300081121     H*   lancio pgm di controllo mappatura
000400930126     H*---------------------------------------------------------------*
000500081121     Ftrmkmapvd CF   E             WORKSTN
000600080319     d
000700040604     D CNCR80        E DS
000800040604     D KPJBA         E DS
000900030722     D UT§DSE0F      E DS
001000090114     D*
001100090114     D PARAM           DS
001200090114     D* CODICE FILIALE
001300090114     D  prmFIL                 1      3  0 inz
001400090114     D* CODICE COMMERCIALE unificante (0 = TUTTI)
001500090114     D  prmagu                 4     10  0 inz
001600090114     D  prmARE                11     13  0 inz
001700090114     d*
001800090114     d* prmela='T'  --> rielabora da WADEL tutto
001900090114     D  prmela                14     14    inz('T')
002000090114     d*
002100090114     d* prmerr='E'  --> elaborazione non effettuabile
002200090114     d*                 per mancanza di dati
002300090114     D  prmerr                15     15    inz
002400080319     d
002500000000     C*---------------------------------------------------------------*
002600000000     C     *ENTRY        PLIST
002700000000     C                   PARM                    KPJBA
002800941221     C*
002900000000     C                   Z-ADD     1             CODUT
003000911010     C                   CALL      'X§PARUT'
003100030722     C                   PARM                    UT§DSE0F
003200920901     C                   MOVEL     REC80         CNCR80
003300941221     C                   MOVEL     RAGUT         RSUT
003400081121     C*---------------------------------------------------------------*
003500081121     c                   eval      v1cuno='S'
003600081121     c                   eval      v1cdue='S'
003700081121     c                   eval      v1ctre='S'
003800081124     c                   eval      v1cqua='S'
003900971223     C*
004000921123     C     FOR01         TAG
004100911008     C*
004200081121     C                   EXFMT     mkmapd1
004300040607     C*
004400030722     C** F3 - FINE LAVORO
004500040604     C   KC              GOTO      FINE
004600921117     C*
004700081121     C*
004800081121     C  nkf              GOTO      FOR01
004900040528     c
005000030722     C* F6  - ELABORAZIONE
005100040607     c                   if        *inkf
005200081121     c                   if        v1cuno='S'
005300090114     c                   movel     param         kpjbu
005400081121     C                   CALL      'TRMKMAP0R'
005500040607     C                   PARM                    KPJBA
005600040604     c                   endif
005700081121     c                   if        v1cdue='S'
005800081121     C                   CALL      'TRMKMAP1R'
005900081121     C                   PARM                    KPJBA
006000081121     c                   endif
006100081121     c                   if        v1ctre='S'
006200081121     C                   CALL      'TRMKMAP2R'
006300081121     C                   PARM                    KPJBA
006400081121     c                   endif
006500081124     c                   if        v1cqua='S'
006600081124     C                   CALL      'TRMKMAP3R'
006700081124     C                   PARM                    KPJBA
006800081124     c                   endif
006900081121
007000081121     c                   endif
007100040604     c*
007200000000     C     FINE          TAG
007300000000     C                   SETON                                        LR
