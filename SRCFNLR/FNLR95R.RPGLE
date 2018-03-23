000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011126      *---------------------------------------------------------------*
000300030108      *  Pulizia di FIAR5, FIAR6 e FIAR7 senza testata bolla          *
000400011126      *---------------------------------------------------------------*
000500971002     FFNBLP01L  IF   E           K DISK
000600011126     FFNARB01L  IF   E           K DISK
000700030108     FFIAR501L  UF   E           K DISK
000800991104     FFIAR601L  UF   E           K DISK
000900991104     FFIAR701L  UF   E           K DISK
001000011126
001100011126     D KPJBA         E DS
001200011126      *---------------------------------------------------------------*
001300030108      *
001400030108     C     *loval        SETLL     Fiar5000
001500030108     C                   READ      Fiar5000
001600030108      *
001700030108    1C                   DOW       not %EOF(FIAR501L)
001800030108     C     KAR5          chain(E)  FNARB01L
001900030108    2C                   if        Not %FOUND(fnarb01L)
002000030108     C     KAR5          chain(E)  FNBLP01L
002100030108    3C                   if        Not %FOUND(fnblp01L)
002200030108     C                   DELETE    fiar5000
002300030108   -3C                   endif
002400030108   -2C                   endif
002500030108     C                   READ      Fiar5000
002600030108   -1C                   ENDDO
002700011126      *
002800011126     C     *loval        SETLL     Fiar6000
002900011126     C                   READ      Fiar6000
003000011126      *
003100030108     C                   DOW       not %EOF(FIAR601L)
003200011130     C     KAR6          chain(E)  FNARB01L
003300011126     C                   if        Not %FOUND(fnarb01L)
003400011130     C     KAR6          chain(E)  FNBLP01L
003500011126     C                   if        Not %FOUND(fnblp01L)
003600011126     C                   delete    fiar6000
003700011126     C                   endif
003800011126     C                   ENDIF
003900011126     C                   READ      Fiar6000
004000011126     C                   ENDDO
004100011126      *
004200011126     C     *loval        SETLL     Fiar7000
004300011126     C                   READ      Fiar7000
004400011126      *
004500030108     C                   DOW       not %EOF(FIAR701L)
004600011130     C     KAR7          chain(E)  FNARB01L
004700011126     C                   if        Not %FOUND(fnarb01L)
004800011130     C     KAR7          chain(E)  FNBLP01L
004900011126     C                   if        Not %FOUND(fnblp01L)
005000011126     C                   delete    fiar7000
005100011126     C                   endif
005200011126     C                   ENDIF
005300011126     C                   READ      Fiar7000
005400011126     C                   ENDDO
005500011126      *
005600011126     c                   eval      *inlr = *on
005700011126      *---------------------------------------------------------------*
005800011126     c     *inzsr        BEGSR
005900011126      *
006000011126     C     *entry        plist
006100011126     C                   PARM                    kpjba
006200030108      *
006300030108     C     KAR5          KLIST
006400030108     C                   KFLD                    AR5AAS
006500030108     C                   KFLD                    AR5LNP
006600030108     C                   KFLD                    AR5NRS
006700030108     C                   KFLD                    AR5NSP
006800011126      *
006900011126     C     KAR6          KLIST
007000011126     C                   KFLD                    AR6AAS
007100011126     C                   KFLD                    AR6LNP
007200011126     C                   KFLD                    AR6NRS
007300011126     C                   KFLD                    AR6NSP
007400011126      *
007500011126     C     KAR7          KLIST
007600011126     C                   KFLD                    AR7AAS
007700011126     C                   KFLD                    AR7LNP
007800011126     C                   KFLD                    AR7NRS
007900011126     C                   KFLD                    AR7NSP
008000011126      *
008100011126     c                   ENDSR
