000100990429     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990429     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H* OCO306R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500000000     H*        - ANNULLAMENTO ASSEGNAZIONE SIMULATA -                *
000600000000     H*        --------------------------------------                *
000700000000     H*                                                      AZ.O306 *
000800000000     H*         FILE : OCMOC00F -          - MPSMP00F -              *
000900000000     H*                                                              *
001000000000     H*--------------------------------------------------------------*
001100990429     FtmMOC00F  UF   E           K DISK
001200990429     FtmSMP01L  UF   E           K DISK
001300871222     D MPHDT1          DS
001400871222     D*--- DATA AREA N.MOVIMENTO MAGAZZINO :
001500871222     D  NUMMOV                 1      5  0
001600871222     D  FILL1                  6      8
001700871222     D  DATDTA                 9     14  0
001800871222     D  FILL2                 15     17
001900871222     D  ANNDTA                18     19  0
002000871222     D  FIL03                 20     50
002100871222     D  DGIO                  51     56  0
002200871222     D  FIL04                 57    100
002300871222     D  FILL5                101    300
002400871222     D  GIOTOL               301    302  0
002500871222     D  FILL6                303    512
002600000000     D KPJBA         E DS
002700990429     D soc001        E DS                  EXTNAME(xsoc001ds)
002800990429     D xsocds          DS          1000
002900000000     I*--- PASSAGGIO PARAMETRI
003000000000     C*----------------------------------------------------*
003100000000     C     *ENTRY        PLIST                                                  *
003200000000     C                   PARM                    KPJBA                          *
003300990429     C*---------- RICERCA DITTA :
003400990429     C                   MOVEL     'SOC001'      TIPXSC
003500990429     C                   MOVEL     *blank        SOCXSC
003600990429     C                   EXSR      REPSOC
003700990429     C     RTNXSC        IFNE      '1'
003800990429     C                   MOVEL     XSOCDS        SOC001
003900990429     C                   MOVEL     xscrgs        desdit           20
004000990429     c                   end
004100000000     C******************************************************
004200871222     C     *DTAARA       DEFINE                  MPHDT1                         *
004300871222     C                   IN        MPHDT1
004400990429     c                   z-add     anndta        anndta4
004500990429     c                   if        anndta > 40
004600990429     c                   add       1900          anndta4
004700990429     c                   else
004800990429     c                   add       2000          anndta4
004900990429     c                   end
005000000000     C                   EXSR      ANORD
005100000000     C                   EXSR      ANSAL
005200000000     C     FINE          TAG
005300000000     C                   SETON                                        LR
005400000000     C******************************************************
005500000000     C*ANNULLAMENTO DI TUTTE LE RIGHE OR ASSEGNATE
005600000000     C     ANORD         BEGSR
005700000000     C     STAR          TAG
005800990429     C                   READ      tmMOC000                               01
005900000000     C   01              GOTO      END1
006000000000     C     MOCFAS        CABNE     'OR'          STAR
006100870630     C     MOCTRG        CABNE     '1'           STAR
006200000000     C     MOCASS        IFGT      0
006300000000     C                   Z-ADD     0             MOCASS
006400000000     C                   MOVEL     *BLANKS       MOCFLS
006500990429     C                   UPDATE    tmMOC000
006600000000     C                   END
006700000000     C                   GOTO      STAR
006800000000     C     END1          ENDSR
006900000000     C******************************************************
007000000000     C*ANNULLAMENTO DI TUTTI GLI ARTICOLI CON QUANTITA' ASSE-
007100000000     C*GNATA IN PROVA MAGGIORE DI 0
007200000000     C     ANSAL         BEGSR
007300871222     C     KSAL          KLIST
007400990429     C                   KFLD                    ANNDTA4           4 0
007500990429     C     KSAL          SETLL     tmSMP000                               02
007600000000     C     SU            TAG
007700990429     C     KSAL          READE     tmSMP000                               02
007800000000     C   02              GOTO      END2
007900000000     C     SMPQIP        IFGT      0
008000000000     C                   Z-ADD     0             SMPQIP
008100990429     C                   UPDATE    tmSMP000
008200000000     C                   END
008300000000     C                   GOTO      SU
008400000000     C     END2          ENDSR
008500990429     C*----------------------------------------------------*
008600990429     C* Reperimento dati società
008700990429     C*----------------------------------------------------*
008800990429     C     REPSOC        BEGSR
008900990429     C*
009000990429     C                   CALLB     'XSOC'
009100990429     C                   PARM                    TIPXSC            6
009200990429     C                   PARM      '201'         SOCXSC            3
009300990429     C                   PARM                    CDSXSC            9 0
009400990429     C                   PARM                    MODXSC            3
009500990429     C                   PARM      *blanks       RTNXSC            1
009600990429     C                   PARM                    XSOCDS
009700990429     C                   PARM                    KPJBA
009800990429     C*
009900990429     C                   ENDSR
