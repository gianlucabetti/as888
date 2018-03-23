000100951009     H DECEDIT('0,') DATEDIT(*DMY.)
000200980625      * ---------------------------------------------------------
000300990409     fFNDCT00F  UP A E           K DISK
000400990409     fFNLBL02L  IF   E           K DISK
000500990414     fTNTAS30C  IF   E           K DISK
000600990409     fTABEL00F  IF   E           K DISK
000700990412     fPRTF198   O    F  132        PRINTER  oflind(*INOF)
000800980611      * ---------------------------------------------------------
000900990217     D R7O             S              1    DIM(30)                              Con. Anom. Reso
001000980717
001100981020     D KPJBA         E DS
001200981020     D DS7O          E DS
001300990414     D DSTB          E DS
001400981228     D DDCT01        E DS
001500980615
001600990414     D codut           S                   LIKE(TBLkut)
001700990414     D Kcod            S                   LIKE(TBLcod)
001800990414     D Kkey            S                   LIKE(TBLkey)
001900980708     D JJ              s              3  0
002000980625      * ---------------------------------------------------------
002100990414     c     kbol01        chain     tntas30c                           30
002200990414      *  se tipo bolla non è valido cerco altro rcd
002300990414     C                   movel     'TB'          kCOD
002400990414     C                   movel(P)  TAStbl        kKEY
002500990414     C     ktab          chain(N)  TABEL                              31
002600990414     C                   movel     TBLUNI        DSTB
002700990414     c                   if        §TBrbl = 'R'
002800990414     c     kbol01        reade     tntas30c                               30
002900990414     c                   endif
003000990409      *
003100990409      * se flag legami non impostato e se spedizione con consegna anomala diversa da RESO
003200990409      * verifico se bolla legata
003300990409     c                   movel     DCTflo        DDCT01
003400990414     c                   IF        §dctlega = *blanks  and  TAScca <> *blanks
003500990414     c     TAScca        lookup    R7O                                    30
003600990409     c                   IF        *IN30 = *off
003700990409     c     Kbol01        chain(N)  fnlbl000                           30
003800990409     c                   IF        *IN30 = *off
003900990409     c                   eval      §DCTlega = 'S'
004000990409     c                   movel     DDCT01        DCTflo
004100990409     c                   update    FNDCT000
004200990409     c                   ENDIF
004300990409     c                   ENDIF
004400990409     c                   ENDIF
004500990409      *
004600990412     c   OF              except    TESTA
004700990412     c                   eval      *inof = *off
004800990412     c                   except    DETTA
004900980722      *****************************************************************
005000980623      *  ROUTINE INIZIALE
005100980722      *****************************************************************
005200951009     C     *INZSR        BEGSR
005300980722
005400990412     c                   eval      *inof = *on
005500990409
005600990409      *     Causali consegna anomala Reso
005700990217     C                   clear                   JJ
005800981009     C                   MOVE      '7O'          KCOD
005900990312     C     ktabe         SETLL     TABEL00F
006000981009     C                   DO        *HIVAL
006100990312     C     ktabe         READE     TABEL00F                               31
006200981029     C   31              leave
006300981009     c                   movel     tbluni        DS7O
006400990217     c                   IF        §7Ores =  'S'
006500990217     C                   ADD       1             jj
006600990217     C                   MOVEL     TBLKEY        R7O(jj)
006700990217     c                   ENDIF
006800990409      *
006900981009     C                   ENDDO
007000980625
007100990312     C     KTABE         KLIST
007200990312     C                   KFLD                    CODUT
007300990312     C                   KFLD                    KCOD
007400990414
007500990414     C     KTAB          KLIST
007600990414     C                   KFLD                    CODUT
007700990414     C                   KFLD                    KCOD
007800990414     C                   KFLD                    KKEY
007900980625      *
008000980508     c     kbol01        klist
008100990409     c                   kfld                    dctaas
008200990409     c                   kfld                    dctlnp
008300990409     c                   kfld                    dctnrs
008400990409     c                   kfld                    dctnsp
008500980721      *
008600951009     C                   ENDSR
008700980611      *------------------------------------------------------------
008800990412     OPRTF198   E            TESTA             2
008900990414     o                                           74 'Calcolo legami bolla in se-
009000990414     o                                              de'
009100990412     o          E            DETTA       2
009200990412     O                       dctaac               4
009300990412     O                       dctfil           +   1
009400990412     O                       dctnca        z  +   1
009500990412     O                       §dctlega         +   2
