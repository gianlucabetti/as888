000100960124     H DECEDIT('0,') DATEDIT(*DMY.)
000200960124      * FNLSA2R2*----------------------------------------------------*
000300960124      *         - LEGGO IL FILE DEI PADRONCINI
000400960124      *--------------------------------------------------------------*
000500021203     FFIAPD01L  IF   E           K DISK
000600031027     D L1              S              3  0 DIM(30)
000700070824     D
000800031027     D* DS per TRUL06R - Caricamento £X
000900031027     D TRUL06DS      E DS
001000031027     D  LIN                    1     90  0
001100031027     D                                     DIM(30)
001200031027     d kpjba         e ds
001300070824     d wCarica                        1    inz
001400070827     d Simfel          s              3s 0
001500070824     c*--------------------------------------------------------------------
001600960124     C     *ENTRY        PLIST
001700960124     C                   PARM                    APDPDR
001800960124     C                   PARM                    APDETM
001900960124     C                   PARM                    APDETP
002000970929     C                   PARM                    APDPDD
002100030107     c                   Parm                    ApdScm
002200030107     c                   Parm                    ApdScp
002300960124     C                   PARM                    WFINE             1
002400070827     C                   PARM                    simfel
002500070824     C                   PARM                    apddfv            8 0
002600070824     c*
002700070824     c                   if        wcarica=' '
002800070824     c                   exsr      INIZIO
002900070824     c                   eval      wcarica='1'
003000070824     c                   endif
003100960124     C*
003200960124     C                   CLEAR                   WFINE
003300021203     C*                  READ      FNAPD000                               30
003400031027     c     leggi         tag
003500021203     C     kpdtip        READE     FIAPD000                               30
003600960124     C*
003700031027     c     *in30         ifeq      *off
003800031027     c                   movel     apdpdr        wpdr              3 0
003900031027     c     wpdr          lookup    L1                                     31
004000031027     c     *in31         ifeq      *off
004100031027     c                   goto      leggi
004200031027     c                   endif
004300031027     c                   endif
004400070824     c
004500960124    1C     *IN30         DOWEQ     *OFF
004600960124     C*
004700960124     C* TROVATO RECORD BUONO --> PASSO AL CHIAMANTE
004800960124     C                   SETON                                        30
004900960124     C                   MOVEL     'N'           WFINE
005000960124    1C                   ENDDO
005100960124     C*
005200960124    1C     WFINE         IFEQ      ' '
005300960124     C                   MOVEL     'S'           WFINE
005400960124     C                   SETON                                        LR
005500960124   X1C                   ELSE
005600960124     C                   SETON                                        RT
005700960124    1C                   ENDIF
005800960124     C*
005900960124     C* POSIZIONAMENTO INZIALE --------------------------------------*
006000070824     C     inizio        BEGSR
006100031027     C* CARICO TABELLA FILIALI GESTITE £1
006200031027     C                   CLEAR                   TRUL06DS
006300031027     C                   MOVE      '£1'          D06COD
006400031027     C                   MOVEL     'L'           D06TLA
006500031027     C                   MOVEL     SIMFEL        D06KEY
006600070824     c                   if        apddfv>0
006700070824     C                   MOVE      'S'           D06KEY
006800070824     c                   movel     apddfv        d06drf
006900070824     c                   endif
007000031027     C                   MOVEL     TRUL06DS      KPJBU
007100031027     C*
007200031027     C                   CALL      'TRUL06R'
007300031027     C                   PARM                    KPJBA
007400031027     C                   MOVEL     KPJBU         TRUL06DS
007500031027     C                   MOVEA     LIN           L1
007600070824     c
007700021203     C*    *LOVAL        SETLL     FNAPD000
007800021203     C                   move      'A'           kpdtip            1
007900031027     C     kpdtip        SETLL     FIAPD000
008000031027     c*
008100031027     c                   ENDSR
