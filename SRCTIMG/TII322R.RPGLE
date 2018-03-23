000100990415     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990415     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H* OCO351R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500880517     H*             - PULIZIA ARCHIVIO ORDINI CLIENTI -              *
000600880517     H*             -----------------------------------              *
000700000000     H*                                                      AZ.O351 *
000800000000     H*--------------------------------------------------------------*
000900990415     FtmMOC00F  UF   E           K DISK
001000990415     FtmSOC00F  O  A E           K DISK
001100880518     FQSYSPRT   O    F  132        PRINTER
001200000000     D PARAM2          DS
001300990415     D  vIDFAT                        8  0
001400990415     D  socpas                        3
001500000000     D KPJBA         E DS
001600990415     D soc001        E DS                  EXTNAME(xsoc001ds)
001700990415     D xsocds          DS          1000
001800990415     d dataiso         s               d   datfmt(*iso)
001900990415     d dataeur         s               d   datfmt(*eur)
002000000000     I*---- PASSAGGIO PARAMETRI
002100000000     C*----------------------------------------------------*
002200000000     C     *ENTRY        PLIST                                                  *
002300000000     C                   PARM                    KPJBA                          *
002400000000     C                   MOVEL     KPJBU         PARAM2                         *
002500990415     C*---------- RICERCA DITTA :
002600990415     C                   MOVEL     'SOC001'      TIPXSC
002700990415     C                   MOVEL     *blank        SOCXSC
002800990415     C                   EXSR      REPSOC
002900990415     C     RTNXSC        IFNE      '1'
003000990415     C                   MOVEL     XSOCDS        SOC001
003100990415     c                   end
003200000000     C******************************************************
003300880518     C*ROVESCIO IN AAMMGG LA DATA RICHIESTA A VIDEO
003400990415     C                   move      VIDFAT        DATAeur
003500990415     C                   move      dataeur       dataiso
003600990415     C                   move      dataiso       DATFAT            8 0
003700880518     C*ELABORA SOLO LE RIGHE FA
003800000000     C     SU            TAG
003900990415     C                   READ      tmMOC000                               06
004000880622     C   06              GOTO      FINE
004100000000     C     MOCFAS        CABNE     'FA'          SU
004200880518     C     MOCDFT        CABGT     DATFAT        SU
004300990415     C                   WRITE     tmSOC000                             40
004400880517     C   40              EXCEPT    ERRBOL
004500880517     C   40              GOTO      SU
004600990415     C  N40              DELETE    tmMOC000
004700880516     C                   GOTO      SU
004800880516     C     FINE          TAG
004900880516     C                   SETON                                        LR
005000990415     C*----------------------------------------------------*
005100990415     C* Reperimento dati società
005200990415     C*----------------------------------------------------*
005300990415     C     REPSOC        BEGSR
005400990415     C*
005500990415     C                   CALLB     'XSOC'
005600990415     C                   PARM                    TIPXSC            6
005700990415     C                   PARM      socpas        SOCXSC            3
005800990415     C                   PARM                    CDSXSC            9 0
005900990415     C                   PARM                    MODXSC            3
006000990415     C                   PARM      *blanks       RTNXSC            1
006100990415     C                   PARM                    XSOCDS
006200990415     C                   PARM                    KPJBA
006300990415     C*
006400990415     C                   ENDSR
006500880518     C***------------------------------------------------***
006600880518     OQSYSPRT   E            ERRBOL      2
006700880518     O                       MOCCLI               8
006800880518     O                       MOCMAT            +  2
006900880518     O                       MOCNOR        Z   +  2
007000990415     O                       MOCDOR            +  2 '  /  /    '
007100880518     O                       MOCQTA        2   +  2
007200990415     O                                         +  2 'GIA ESISTENTE SU TMSOC00'
