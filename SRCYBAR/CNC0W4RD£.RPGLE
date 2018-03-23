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
001200020806     D CMDD            S             44    DIM(10) CTDATA PERRCD(1)             QCAEXEC DL
001300900515      *
001400910708     D CNCR80        E DS
001500900514     D KPJBA         E DS
001600900514     D  LIBSYS                92    100
001700900514     D  MSGERR               453    502
001800911030      *
001900930510     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
002000911104      *  DISALLOCO I FILE
002100911104     C                   EXSR      DLCOBJ
002200921204     C*-                                                   *
002300930510     C     FINE          TAG
002400910521     C                   SETON                                        LR
002500910521     C*
002600911104      *-----------------------------------------------------***********
002700911104      * DISSALLOC.  FILE                                    *  DLCOBJ *
002800911104      *-----------------------------------------------------***********
002900911104     C     DLCOBJ        BEGSR
003000020926     C*
003100020926     C                   Z-ADD     44            LUNCMD           15 5
003200020926     C                   MOVE      ')'           VAR7              7
003300020926     c                   eval      %subst(var7:1:3) = $fle
003400020926     c                   eval      %subst(var7:4:3) = '046'
003500020926     C                   MOVEL     VAR7          VAR8              8
003600020926     C                   MOVE      ')'           VAR8
003700020926      * FNMOV00T
003800020926     C                   MOVE      VAR8          CMDD(1)
003900020806     C                   MOVEA     CMDD(1)       CMDX             44
004000911104     C                   CALL      'QCMDEXC'                            91
004100911104     C                   PARM                    CMDX
004200911104     C                   PARM                    LUNCMD
004300941018      * FNCAS00T
004400020926     C                   MOVE      VAR8          CMDD(2)
004500020926     C                   MOVEA     CMDD(2)       CMDX
004600911104     C                   CALL      'QCMDEXC'                            91
004700911104     C                   PARM                    CMDX
004800911104     C                   PARM                    LUNCMD
004900991018     C*
005000020805     c                   eval      %subst(var7:1:3) = $simfel
005100020926     C                   MOVEL     VAR7          VAR8
005200941018     C* FNARBD0F
005300020805     C                   MOVE      VAR8          CMDD(3)
005400020926     C                   MOVEA     CMDD(3)       CMDX
005500920213     C                   CALL      'QCMDEXC'                            91
005600920213     C                   PARM                    CMDX
005700920213     C                   PARM                    LUNCMD
005800991011     C* FIARBT0F
005900020805     C                   MOVE      VAR8          CMDD(4)
006000020926     C                   MOVEA     CMDD(4)       CMDX
006100920213     C                   CALL      'QCMDEXC'                            91
006200920213     C                   PARM                    CMDX
006300920213     C                   PARM                    LUNCMD
006400991206     C* FNARBK0F
006500020805     C                   MOVE      VAR8          CMDD(5)
006600020926     C                   MOVEA     CMDD(5)       CMDX
006700920213     C                   CALL      'QCMDEXC'                            91
006800920213     C                   PARM                    CMDX
006900920213     C                   PARM                    LUNCMD
007000991206     C* FNARBM0F
007100020805     C                   MOVE      VAR8          CMDD(6)
007200020926     C                   MOVEA     CMDD(6)       CMDX
007300920213     C                   CALL      'QCMDEXC'                            91
007400920213     C                   PARM                    CMDX
007500920213     C                   PARM                    LUNCMD
007600991206     C* FNARBG0F
007700020805     C                   MOVE      VAR8          CMDD(7)
007800020926     C                   MOVEA     CMDD(7)       CMDX
007900920213     C                   CALL      'QCMDEXC'                            91
008000920213     C                   PARM                    CMDX
008100920213     C                   PARM                    LUNCMD
008200991206     C* FNARBU0F
008300020805     C                   MOVE      VAR8          CMDD(8)
008400020926     C                   MOVEA     CMDD(8)       CMDX
008500930512     C                   CALL      'QCMDEXC'                            91
008600930512     C                   PARM                    CMDX
008700930512     C                   PARM                    LUNCMD
008800950125     C* FNCAF00T
008900020902     c                   clear                   cmdx
009000020926     C                   MOVEA     CMDD(9)       CMDX
009100040407     c                   eval      %subst(cmdx:37:3) = $simfel
009200040407     c                   eval      %subst(cmdx:40:3) = $simfel
009300950125     C                   CALL      'QCMDEXC'                            91
009400950125     C                   PARM                    CMDX
009500950125     C                   PARM                    LUNCMD
009600911104     C                   ENDSR
009700941018      *
009800941018     C     *INZSR        BEGSR
009900941018     C     *ENTRY        PLIST
010000941018     C                   PARM                    KPJBA
010100941018      *
010200941018     C                   Z-ADD     1             CODUT             1 0
010300941018     C                   CALL      'X§PARUT'
010400941018     C                   PARM                    UT§DSE
010500941018     C     CODUT         CABEQ     -1            FINE
010600941018     C                   MOVEL     REC80         CNCR80
010700020719     C*
010800020719     C     SIMTPP        COMP      '2'                                    01
010900020719     C   01              MOVE      SIMPOU        §FLE              3 0
011000941018     C  N01              DO
011100941018     C     SIMFEL        COMP      0                                      02
011200941018     C  N02              MOVE      SIMFEL        §FLE
011300941018     C                   END
011400020805
011500020805     c                   movel     simfel        $simfel           3
011600020926     C                   MOVE      §FLE          $fle              3
011700941018     C                   ENDSR
011800991122**         CMDD
011900020926DLCOBJ OBJ((FNMOV00T *FILE *EXCL   M000000))
012000020926DLCOBJ OBJ((FNCAS00T *FILE *EXCL   M000000))
012100020805DLCOBJ OBJ((FNARBD0T *FILE *EXCL   M000000))
012200020805DLCOBJ OBJ((FIARBT0T *FILE *EXCL   M000000))
012300020805DLCOBJ OBJ((FNARBK0T *FILE *EXCL   M000000))
012400020805DLCOBJ OBJ((FNARBM0T *FILE *EXCL   M000000))
012500020805DLCOBJ OBJ((FNARBG0T *FILE *EXCL   M000000))
012600020805DLCOBJ OBJ((FIARBU0T *FILE *EXCL   M000000))
012700040407DLCOBJ OBJ((FNCAF00T *FILE *EXCL   M000000))
