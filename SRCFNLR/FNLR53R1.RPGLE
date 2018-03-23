000100930210     H DECEDIT('0,') DATEDIT(*DMY.)
000200941111      * FNLR53R1 *---------------------------------------------------*
000300930210      *----------*                                                   *
000400930210      *     - GESTIONE SBLOCCO DISTINTE -                            *
000500930210      *--------------------------------------------------------------*
000600940221      * 01 - ERRMSG: DISTINTA INESISTENTE                            *
000700940221      * 02 - ERRMSG: DISTINTA NON SBLOCCABILE                        *
000800940221      * 03 - ERRMSG: DISTINTA DI ALTRA FILIALE                       *
000900940221      *--------------------------------------------------------------*
001000941111     FFNLR53D1  CF   E             WORKSTN
001100020902     FFNFVV01L  UF   E           K DISK
001200911028      *
001201020822     D ddatiute      e ds
001202020822     D azuteds       e ds                  extname(AZUTE00F)
001203020822     D tibs34ds      E DS                  inz
001400950224     D KPJBA         E DS
001500941229      *
001600911028      *--------------------------------------------------------------*
001700950224     C     *ENTRY        PLIST
001800950224     C                   PARM                    KPJBA
001900950224     C                   MOVE      KPJBU         P53FGS            3 0
001901020822     C* Decodifica Elaboratore
001902020822     c     *dtaara       define    §azute        azuteds
001903020822     c     *dtaara       define    §datiute      ddatiute
001904020822     C                   in(E)     *dtaara
001905020822     C                   IF        %Error  or  RSUT = *blanks
001906020822     C                   call      'TIBS34R'
001907020822     C                   parm                    Tibs34Ds
001908020822     C                   in        *dtaara
001909020822     c                   ENDIF
002400941229     C*
002500911108     C     KFVV          KLIST
002600911108     C                   KFLD                    FVVNPG
002700930211     C                   KFLD                    VNDT
002800020822     C                   KFLD                    p53fgs
002900941229     C*
003000911108     C                   Z-ADD     4             FVVNPG
003100911112     C*
003200930210     C     *INKL         DOWEQ     '0'
003300930210     C     *INKF         ANDEQ     '0'
003400930211     C     *IN02         OREQ      '1'
003600941111     C                   EXFMT     LR53D01
003700020822     C   KL              SETOFF                                       02
003800930211     C  NKL              DO
003900020902     C     KFVV          CHAIN     FNFVV01L                           01
004000930210     C  NKF
004100930210     CANN01              EXCEPT    RILFVV
004300930211     C   KF
004500020822     CANN01FVVFCF        IFEQ      'S'
004600940209     C     FVVFCF        OREQ      'A'
004700950613     C     FVVFCF        OREQ      '§'
004800940209     C     FVVFCF        OREQ      *BLANKS
004900930210     C                   SETON                                        02
005000930520     C                   EXCEPT    RILFVV
005100930210     C                   ELSE
005200930210     C                   SETOFF                                       02
005300930210     C                   MOVEL     *BLANKS       FVVFCF
005400930210     C                   EXCEPT    AGGI
005500930210     C                   END
005600930210     C                   END
005700930211     C                   END
005800911028     C*
005900930210     C                   SETON                                        LR
006000930210     C*------------------------------------------------------------*
006100941111     OFNFVV000  E            RILFVV
006200941111     OFNFVV000  E            AGGI
006300930210     O                       FVVFCF
