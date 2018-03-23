000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300930510      * CNC0V4RD                                                     *
000400930510      *        DISALLOCO FILE TRASMISSIONE                           *
000500911030      *        CHIUSURA PER OPERATORE                                *
000600900606?     *--------------------------------------------------------------*
000700940224      * 01 -ON--> PRIMO LIVELLO/OFF --> SECONDO LIVELLO              *
000800940224      * 02 -ON--> SIMFEL UGUALE A 0                                  *
000900940224      * 91 -CALL A QCMDEXEC                                          *
001000940224?     *--------------------------------------------------------------*
001100900515      *
001200120702     D CMDD            S             44    DIM(3) CTDATA PERRCD(1)
001300900515      *
001400910708     D CNCR80        E DS
001500900514     D KPJBA         E DS
001600900514     D  LIBSYS                92    100
001700911030      *
001800930510     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
001900911104      *  DISALLOCO I FILE
002000911104     C                   EXSR      DLCOBJ
002100921204     C*-                                                   *
002200930510     C     FINE          TAG
002300910521     C                   SETON                                        LR
002400910521     C*
002500911104      *-----------------------------------------------------***********
002600911104      * DISSALLOC.  FILE                                    *  DLCOBJ *
002700911104      *-----------------------------------------------------***********
002800911104     C     DLCOBJ        BEGSR
002900020926     C*
003000020926     C                   Z-ADD     44            LUNCMD           15 5
003100020926     C                   MOVE      ')'           VAR7              7
003200020926     c                   eval      %subst(var7:1:3) = $fle
003300020926     c                   eval      %subst(var7:4:3) = '046'
003400020926     C                   MOVEL     VAR7          VAR8              8
003500020926     C                   MOVE      ')'           VAR8
003600020926      * FNMOV00T
003700020926     C                   MOVE      VAR8          CMDD(1)
003800020806     C                   MOVEA     CMDD(1)       CMDX             44
003900911104     C                   CALL      'QCMDEXC'                            91
004000911104     C                   PARM                    CMDX
004100911104     C                   PARM                    LUNCMD
004200941018      * FNCAS00T
004300020926     C                   MOVE      VAR8          CMDD(2)
004400020926     C                   MOVEA     CMDD(2)       CMDX
004500911104     C                   CALL      'QCMDEXC'                            91
004600911104     C                   PARM                    CMDX
004700911104     C                   PARM                    LUNCMD
004800991018     C*
004900020805     c                   eval      %subst(var7:1:3) = $simfel
005000020926     C                   MOVEL     VAR7          VAR8
005100950125     C* FNCAF00T
005200020902     c                   clear                   cmdx
005300120702     C                   MOVEA     CMDD(3)       CMDX
005400040407     c                   eval      %subst(cmdx:37:3) = $simfel
005500040407     c                   eval      %subst(cmdx:40:3) = $simfel
005600950125     C                   CALL      'QCMDEXC'                            91
005700950125     C                   PARM                    CMDX
005800950125     C                   PARM                    LUNCMD
005900911104     C                   ENDSR
006000941018      *
006100941018     C     *INZSR        BEGSR
006200941018     C     *ENTRY        PLIST
006300941018     C                   PARM                    KPJBA
006400941018      *
006500941018     C                   Z-ADD     1             CODUT             1 0
006600941018     C                   CALL      'X§PARUT'
006700941018     C                   PARM                    UT§DSE
006800941018     C                   MOVEL     REC80         CNCR80
006900020719     C*
007000020719     C     SIMTPP        COMP      '2'                                    01
007100020719     C   01              MOVE      SIMPOU        §FLE              3 0
007200941018     C  N01              DO
007300941018     C     SIMFEL        COMP      0                                      02
007400941018     C  N02              MOVE      SIMFEL        §FLE
007500941018     C                   END
007600020805
007700020805     c                   movel     simfel        $simfel           3
007800020926     C                   MOVE      §FLE          $fle              3
007900941018     C                   ENDSR
008000991122**         CMDD
008100020926DLCOBJ OBJ((FNMOV00T *FILE *EXCL   M000000))
008200020926DLCOBJ OBJ((FNCAS00T *FILE *EXCL   M000000))
008300040407DLCOBJ OBJ((FNCAF00T *FILE *EXCL   M000000))
