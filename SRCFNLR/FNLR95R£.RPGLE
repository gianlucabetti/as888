000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011126      *---------------------------------------------------------------*
000300011126      *  Pulizia di FIAR6 e FIAR7 senza testata bolla                 *
000400011126      *---------------------------------------------------------------*
000500971002     FFNBLP01L  IF   E           K DISK
000600011126     FFNARB01L  IF   E           K DISK
000700991104     FFIAR601L  UF   E           K DISK
000800991104     FFIAR701L  UF   E           K DISK
000900011126
001000011126     D KPJBA         E DS
001100011126      *---------------------------------------------------------------*
001200011126      *
001300011126     C     *loval        SETLL     Fiar6000
001400011126     C                   READ      Fiar6000
001500011126      *
001600011126    3C                   DOW       not %EOF(FIAR601L)
001700011130     C     KAR6          chain(E)  FNARB01L
001800011126     C                   if        Not %FOUND(fnarb01L)
001900011130     C     KAR6          chain(E)  FNBLP01L
002000011126     C                   if        Not %FOUND(fnblp01L)
002100011126     C                   delete    fiar6000
002200011126     C                   endif
002300011126     C                   ENDIF
002400011126     C                   READ      Fiar6000
002500011126     C                   ENDDO
002600011126      *
002700011126     C     *loval        SETLL     Fiar7000
002800011126     C                   READ      Fiar7000
002900011126      *
003000011126    3C                   DOW       not %EOF(FIAR701L)
003100011130     C     KAR7          chain(E)  FNARB01L
003200011126     C                   if        Not %FOUND(fnarb01L)
003300011130     C     KAR7          chain(E)  FNBLP01L
003400011126     C                   if        Not %FOUND(fnblp01L)
003500011126     C                   delete    fiar7000
003600011126     C                   endif
003700011126     C                   ENDIF
003800011126     C                   READ      Fiar7000
003900011126     C                   ENDDO
004000011126      *
004100011126     c                   eval      *inlr = *on
004200011126      *---------------------------------------------------------------*
004300011126     c     *inzsr        BEGSR
004400011126      *
004500011126     C     *entry        plist
004600011126     C                   PARM                    kpjba
004700011126      *
004800011126     C     KAR6          KLIST
004900011126     C                   KFLD                    AR6AAS
005000011126     C                   KFLD                    AR6LNP
005100011126     C                   KFLD                    AR6NRS
005200011126     C                   KFLD                    AR6NSP
005300011126      *
005400011126     C     KAR7          KLIST
005500011126     C                   KFLD                    AR7AAS
005600011126     C                   KFLD                    AR7LNP
005700011126     C                   KFLD                    AR7NRS
005800011126     C                   KFLD                    AR7NSP
005900011126      *
006000011126     c                   ENDSR
