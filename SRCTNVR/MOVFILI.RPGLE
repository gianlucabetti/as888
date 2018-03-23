000100980529     F*-----------------------------------------------------------*
000200980529     F*  DATA BASE                                                *
000300980529     F*-----------------------------------------------------------*
000400980529     FCNMOV18L  IP   E           K DISK
000500980529     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
000600980529     F*-----------------------------------------------------------*
000700980529     ICNMOV018
000800980529     I                                          MOVKUT        L1
000900980529     I                                          MOVDRG        L1
001000980529     I                                          MOVNRG        L1
001100980529     I                                          MOVNRJ        L1
001200980529     I                                          MOVNU2        L1
001300980529     ICNMOVH18
001400980529     I                                          MOVKUT        L1
001500980529     I                                          MOVDRG        L1
001600980529     I                                          MOVNRG        L1
001700980529     I                                          MOVNRJ        L1
001800980529     I                                          MOVNU2        L1
001900980529     C*--------------------------------------------------§
002000980529     C   L1              EXSR      CLRMOV
002100980529     C                   EXSR      CTRFIL
002200980529     CL1                 EXSR      TSTSTP
002300980529     C*--------------------------------------------------§
002400980529     C     CLRMOV        BEGSR
002500980529     C*
002600980529     C                   MOVE      *ZEROS        WFILRG            3 0
002700980529     C                   MOVE      *ZEROS        WFIL              3 0
002800980529     C                   MOVEL     'N'           W2FIL             1
002900980529     C*
003000980529     C                   ENDSR
003100980529     C*--------------------------------------------------§
003200980529     C     CTRFIL        BEGSR
003300980529     C*
003400980529     C                   MOVEL     MOVKSC        WFIL              3 0
003500980529     C     WFIL          IFNE      *ZEROS
003600980529     C     WFIL          ANDLT     500
003700980529     C     WFIL          ANDNE     46
003800980529     C     MOVKCC        ANDNE     161
003900980529     C     MOVKCC        ANDNE     51
004000980603     C     MOVKCC        ANDNE     175
004100980603     C     MOVKCC        ANDNE     41
004200980603     C     MOVKCC        ANDNE     42
004300980603     C     MOVKCC        ANDNE     43
004400980603     C     MOVKCC        ANDNE     44
004500980603     C     MOVKCC        ANDNE     45
004600980529     C     WFILRG        IFEQ      *ZEROS
004700980529     C                   MOVEL     MOVKSC        WFILRG            3 0
004800980529     C                   ELSE
004900980529     C     WFILRG        IFNE      WFIL
005000980529     C                   MOVEL     'S'           W2FIL             1
005100980529     C                   END
005200980529     C                   END
005300980529     C                   END
005400980529     C*
005500980529     C                   ENDSR
005600980529     C*--------------------------------------------------§
005700980529     C     TSTSTP        BEGSR
005800980529     C*
005900980529     C     W2FIL         IFEQ      'S'
006000980529     C   OF              EXCEPT    TESTA
006100980529     C                   EXCEPT    DETREG
006200980529     C                   END
006300980529     C*
006400980529     C                   ENDSR
006500980529     C*--------------------------------------------------§
006600980529     OQSYSPRT   E            TESTA          1 01
006700980529     O                                           63 'REGISTRZIONI CON'
006800980529     O                                              'PIU'' UNITA'''
006900980529     O          E            DETREG         1
007000980529     O                       MOVKCC        Z      4
007100980529     O                       MOVKSC        Z     12
007200980529     O                       MOVDRG        Y     21
007300980529     O                       MOVNRG        Z     28
007400980529     O                       MOVNU2        Z     36
007500980529     O                       MOVNRJ        Z     46
007600980529     O                       WFILRG        Z     55
007700980529     O                       WFIL          Z     70
