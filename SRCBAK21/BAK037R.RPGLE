000100000000     H*PARMS OPTION(*NOXREF) DFTACTGRP(*YES) CVTOPT(*NONE)
000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300000000     D*-----------------------------------------
000400000000     D RCVVAR          DS
000500000000     D  OFFSET                 1      4B 0
000600000000     D  NOENTR                 9     12B 0
000700000000     D  LSTSIZ                13     16B 0
000800000000     D*-----------------------------------------
000900000000     D LIST            DS
001000000000     D  JOBNAM                 1     10
001100000000     D  USRNAM                11     20
001200000000     D  JOBNBR                21     26
001300000000     D*-----------------------------------------
001400000000     D                 DS
001500000000     D  LENDTA                 1      4B 0
001600000000     D  STRPOS                 5      8B 0
001700000000     D  RCVLEN                 9     12B 0
001800000000     D  SPLF£                 13     16B 0
001900000000     D*-----------------------------------------
002000000000     D SPACE           C                   CONST('AA        QTEMP     ')
002100000000     C*------------------------------------------
002200000000     C     *ENTRY        PLIST
002300000000     C                   PARM                    JOB              10
002400000000     C                   PARM                    USR              10
002500000000     C                   PARM                    NBR               6
002600000000     C                   MOVE      JOB           JOBNAM
002700000000     C                   MOVE      USR           USRNAM
002800000000     C                   MOVEL     NBR           JOBNBR
002900000000     C*-----------------------------------------------------
003000000000     C*  CARICA LO USR SPACE CON LISTA JOB
003100000000     C*-----------------------------------------------------
003200000000     C                   CALL      'QUSLJOB'
003300000000     C                   PARM      SPACE         USRSPC
003400000000     C                   PARM      'JOBL0100'    FMTNME            8
003500000000     C                   PARM                    LIST
003600000000     C                   PARM      '*ALL '       STATUS           10
003700000000     C*----------------------------------------------------
003800000000     C* REPERISCE IL NUMERO DI ELEMENTI DI LISTA
003900000000     C*----------------------------------------------------
004000000000     C                   Z-ADD     16            LENDTA
004100000000     C                   Z-ADD     125           STRPOS
004200000000     C                   CALL      'QUSRTVUS'
004300000000     C                   PARM      SPACE         USRSPC
004400000000     C                   PARM                    STRPOS
004500000000     C                   PARM                    LENDTA
004600000000     C                   PARM                    RCVVAR
004700000000     C*--------------------------------------
004800000000     C* REPERISCE ELEMENTO DI LISTA
004900000000     C*--------------------------------------
005000000000     C                   Z-ADD     OFFSET        STRPOS
005100000000     C                   ADD       1             STRPOS
005200000000     C                   Z-ADD     LSTSIZ        LENDTA
005300000000     C                   Z-ADD     26            RCVLEN
005400000000     C                   Z-ADD     1             COUNT            15 0
005500000000     C     NOENTR        IFGT      0
005600000000     C                   CALL      'QUSRTVUS'
005700000000     C                   PARM      SPACE         USRSPC
005800000000     C                   PARM                    STRPOS
005900000000     C                   PARM                    LENDTA
006000000000     C                   PARM                    LIST
006100000000     C                   MOVE      JOBNAM        JOB
006200000000     C                   MOVE      USRNAM        USR
006300000000     C                   MOVE      JOBNBR        NBR
006400000000     C                   ENDIF
006500000000     C                   SETON                                          RT
006600000000     C*------------------------------------------
006700000000     C* CREA USER SPACE
006800000000     C*------------------------------------------
006900000000     C     *INZSR        BEGSR
007000000000     C                   CALL      'QUSCRTUS'                           21
007100000000     C                   PARM      SPACE         USRSPC           20
007200000000     C                   PARM                    EXTATR           10
007300000000     C                   PARM      100           USSIZE           13 5
007400000000     C                   PARM      ' '           USINIT            1
007500000000     C                   PARM      '*CHANGE'     USAUTH           10
007600000000     C                   PARM                    USTEXT           50
007700000000     C                   ENDSR
