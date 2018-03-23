000100990415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300910920     H* OFN363R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500880517     H*             - PULIZIA ARCHIVIO ORDINI CLIENTI -              *
000600880517     H*             -----------------------------------              *
000700000000     H*                                                      AZ.O351 *
000800000000     H*--------------------------------------------------------------*
000900990415     FtmSOF00F  UF   E           K DISK
001000000000     D PARAM2          DS
001100990415     D  vIDFAT                        8  0
001200990415     D  socpas                        3
001300000000     D KPJBA         E DS
001400990415     D soc001        E DS                  EXTNAME(xsoc001ds)
001500990415     D xsocds          DS          1000
001600990415     d dataiso         s               d   datfmt(*iso)
001700990415     d dataeur         s               d   datfmt(*eur)
001800000000     I*---- PASSAGGIO PARAMETRI
001900000000     C*----------------------------------------------------*
002000000000     C     *ENTRY        PLIST                                                  *
002100000000     C                   PARM                    KPJBA                          *
002200000000     C                   MOVEL     KPJBU         PARAM2                         *
002300990415     C*---------- RICERCA DITTA :
002400990415     C                   MOVEL     'SOC001'      TIPXSC
002500990415     C                   MOVEL     *blank        SOCXSC
002600990415     C                   EXSR      REPSOC
002700990415     C     RTNXSC        IFNE      '1'
002800990415     C                   MOVEL     XSOCDS        SOC001
002900990415     c                   end
003000990415     C******************************************************
003100990415     C*ROVESCIO IN AAMMGG LA DATA RICHIESTA A VIDEO
003200990415     C                   move      VIDFAT        DATAeur
003300990415     C                   move      dataeur       dataiso
003400990415     C                   move      dataiso       DATFAT            8 0
003500880518     C*ELABORA SOLO LE RIGHE FA
003600000000     C     SU            TAG
003700990415     C                   READ      tmSOF000                               06
003800880622     C   06              GOTO      FINE
003900910920     C     MOFFAS        CABNE     'FA'          SU
004000910920     C     MOFDFT        CABGT     DATFAT        SU
004100990415     C                   DELETE    tmSOF000
004200880516     C                   GOTO      SU
004300880516     C     FINE          TAG
004400880516     C                   SETON                                        LR
004500990415     C*----------------------------------------------------*
004600990415     C* Reperimento dati società
004700990415     C*----------------------------------------------------*
004800990415     C     REPSOC        BEGSR
004900990415     C*
005000990415     C                   CALLB     'XSOC'
005100990415     C                   PARM                    TIPXSC            6
005200990415     C                   PARM      socpas        SOCXSC            3
005300990415     C                   PARM                    CDSXSC            9 0
005400990415     C                   PARM                    MODXSC            3
005500990415     C                   PARM      *blanks       RTNXSC            1
005600990415     C                   PARM                    XSOCDS
005700990415     C                   PARM                    KPJBA
005800990415     C*
005900990415     C                   ENDSR
