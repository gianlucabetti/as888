000100930210     H DECEDIT('0,') DATEDIT(*DMY.)
000200941107      * FNLR50R1 *---------------------------------------------------*
000300930210      *----------*                                                   *
000400940209      *     - GESTIONE SBLOCCO DISTINTE IN PREPARAZIONE -            *
000500930210      *--------------------------------------------------------------*
000600940221      * 01 - ERRMSG: DISTINTA INESISTENTE                            *
000700940221      * 02 - ERRMSG: DISTINTA NON SBLOCCABILE                        *
000800940221      * 03 - ERRMSG: DISTINTA DI ALTRA FILIALE                       *
000900940221      *--------------------------------------------------------------*
001000941107     FFNLR50D1  CF   E             WORKSTN
001100020906     FFNFVV01L  UF   E           K DISK
001200941107      *--------------------------------------------------------------*
001300941229     D DS§FIL        E DS
001400950224     D KPJBA         E DS
001500950224      * DS per richiamo pgm FNLR50R1
001600950224     D FNLR50        E DS                  EXTNAME(FNLR50DS)
001601020906     D ddatiute      e ds
001602020906     D azuteds       e ds                  extname(AZUTE00F)
001603020906     D tibs34ds      E DS                  inz
001700911028      *--------------------------------------------------------------*
001800941107     C*  Loop Princiaple
001900930210     C     *INKL         DOWEQ     '0'
002000930210     C     *INKF         ANDEQ     '0'
002100930211     C     *IN02         OREQ      '1'
002200930211     C     *IN03         OREQ      '1'
002300941107     C                   EXFMT     LR50D01
002400941107     C*  Ritorno
002500941107     C     *INKL         IFEQ      '1'
002600941107     C                   SETOFF                                       0302
002700941107     C                   ELSE
002800941107     C                   Z-ADD     V1CNDT        KNFV
002900020906     C     KFVV          CHAIN     FNFVV01L                           01
003000941107     C     *IN01         IFEQ      '0'
003200941107     C*
003300941107     C* Conferma Sblocco
003400941107     C     *INKF         IFEQ      '0'
003500941107     C                   EXCEPT    RILFVV
003600941107     C                   ELSE
003700941107     C  N03FVVFCF        IFEQ      'S'
003800940209     C     FVVFCF        OREQ      'F'
003900950613     C     FVVFCF        OREQ      '§'
004000940209     C     FVVFCF        OREQ      *BLANKS
004100930210     C                   SETON                                        02
004200930520     C                   EXCEPT    RILFVV
004300930210     C                   ELSE
004400930210     C                   SETOFF                                       02
004500930210     C                   MOVEL     *BLANKS       FVVFCF
004600930210     C                   EXCEPT    AGGI
004700930210     C                   END
004800941107     C                   END
004900941107     C*
005000941107     C                   END
005100941107     C                   END
005200911028     C*
005300941107     C                   END
005400941107     C*
005500930210     C                   SETON                                        LR
005600941107     C*----------------------------------------------------*
005700941107     C*  OPERAZIONI INIZIALI
005800941107     C*----------------------------------------------------*
005900941107     C     *INZSR        BEGSR
006000941107     C*
006100950224     C     *ENTRY        PLIST
006200950224     C                   PARM                    KPJBA
006300950224     C                   MOVEL     KPJBU         FNLR50
006301020906     c*
006302020906     c     *dtaara       define    §azute        azuteds
006303020906     c     *dtaara       define    §datiute      ddatiute
006304020906     C                   in(E)     *dtaara
006305020906     C                   IF        %Error  or  RSUT = *blanks
006306020906     C                   call      'TIBS34R'
006307020906     C                   parm                    Tibs34Ds
006308020906     C                   in        *dtaara
006309020906     c                   ENDIF
006800941107     C*
006900941107     C     KFVV          KLIST
007000941107     C                   KFLD                    KNPG
007100941107     C                   KFLD                    KNFV
007101020906     C                   KFLD                    d50fil
007300941107     C*
007400941107     C*  Definizione variabili
007500941107     C     *LIKE         DEFINE    FVVNPG        KNPG
007600941107     C     *LIKE         DEFINE    FVVNFV        KNFV
007700941107     C                   Z-ADD     4             KNPG
007800950224     C                   Z-ADD     D50NDT        V1CNDT
007900941107     C*
008000941107     C                   ENDSR
008100930210     C*------------------------------------------------------------*
008200941107     OFNFVV000  E            RILFVV
008300941107     OFNFVV000  E            AGGI
008400930210     O                       FVVFCF
