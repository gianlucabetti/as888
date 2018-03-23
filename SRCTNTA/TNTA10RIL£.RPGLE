000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940713     H* TNTA10R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400990716     H* ELIMINAZIONE FISICA TNTAM/TITAD/TITPT/TITPD/TITGC ANNULLATI  *
000500970122     H* E ELIMINAZIONE FISICA TNETC/TNETT/TNETS (CALL A PGM TNTA09R) *
000600000000     H*--------------------------------------------------------------*
000700940713     FTNTAM00F  UF   E             DISK
000800940713     FTNTAM01L  IF   E           K DISK
000900940713     F                                     RENAME(TNTAM000:TNTAM001)
001000921016     F                                     INFDS(TAR1)
001100990716     FTITAD00F  UF   E             DISK
001200990716     FTITAD04L  IF   E           K DISK
001300990716     F                                     RENAME(TITAD000:TITAD004)
001400921016     F                                     INFDS(TAR2)
001500990716     FTITPT00F  UF   E             DISK
001600990716     FTITPT01L  IF   E           K DISK
001700990716     F                                     RENAME(TITPT000:TITPT001)
001800921016     F                                     INFDS(TAR3)
001900990716     FTITPD00F  UF   E             DISK
002000990716     FTITPD01L  IF   E           K DISK
002100990716     F                                     RENAME(TITPD000:TITPD001)
002200921016     F                                     INFDS(TAR4)
002300990716     FTITGC00F  UF   E             DISK
002400990716     FTITGC01L  IF   E           K DISK
002500990716     F                                     RENAME(TITGC000:TITGC001)
002600950105     F                                     INFDS(TAR5)
002700030613     FTITAV00F  UF   E             DISK
002800030613     FTITAV01L  IF   E           K DISK
002900030613     F                                     RENAME(TITAV000:TITAV001)
003000030613     F                                     INFDS(TAR6)
003100921016     D TAR1            DS
003200921016     D  TAMNRR               397    400B 0
003300921016     D TAR2            DS
003400921016     D  TADNRR               397    400B 0
003500921016     D TAR3            DS
003600921016     D  TPTNRR               397    400B 0
003700921016     D TAR4            DS
003800921016     D  TPDNRR               397    400B 0
003900950105     D TAR5            DS
004000950105     D  TGCNRR               397    400B 0
004100030613     D TAR6            DS
004200030613     D  TAVNRR               397    400B 0
004300921013     D CNCR80        E DS
004400921013     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004500000000     D KPJBA         E DS
004600921015     C*****************************************************************
004700921015     C* RIEPILOGO INDICATORI
004800921015     C*****************************************************************
004900921015     C* 10    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
005000921015     C*         IL RECORD E' ALLOCATO (CIOE' *IN10 ACCESO) SALTO LA
005100921015     C*         TARIFFA (CHE VERRA' ANNULLATA LA PROSSIMA VOLTA)
005200940713     C* 30/32 - DI COMODO
005300921015     C*****************************************************************
005400000000     C     *ENTRY        PLIST
005500000000     C                   PARM                    KPJBA
005600950207     C                   Z-ADD     1             CODUT
005700921013     C*
005800921013     C                   CALL      'X§PARUT'
005900921013     C                   PARM                    UT§DSE
006000921013     C                   MOVEL     REC80         CNCR80
006100000000     C*---------------------------------------------------------------*
006200940713     C****  PULIZIA  TNTAM  ****
006300940713     C     *LOVAL        SETLL     TNTAM001
006400940713     C                   READ      TNTAM001                               32
006500940713     C     *IN32         DOWEQ     *OFF
006600921016     C*
006700950105     C     TAMATB        IFNE      ' '
006800921015     C** SE LA FILIALE E' MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
006900940713     C     TAMFTR        IFNE      *BLANKS
007000940713     C     TAMNRR        CHAIN     TNTAM000                           3010
007100940713     C  N30
007200940713     CANN10              DELETE    TNTAM000
007300940713     C                   ENDIF
007400940713     C                   ENDIF
007500921028     C*
007600940713     C                   READ      TNTAM001                               32
007700940713     C                   ENDDO
007800921016     C*
007900990716     C****  PULIZIA  TITAD  ****
008000990716     C     *LOVAL        SETLL     TITAD004
008100990716     C                   READ      TITAD004                               32
008200940713     C     *IN32         DOWEQ     *OFF
008300921015     C*
008400950105     C     TADATB        IFNE      ' '
008500921015     C** SE SONO IN SEDE   E FIL. MECC. ANNULLO SE TRASMESSO/RICEVUTO
008600921015     C** SE SONO IN FILIALE                 "   "     "
008700940713     C     TADFTR        IFNE      *BLANKS
008800990716     C     TADNRR        CHAIN     TITAD000                           3010
008900990716     C  N30
009000990716     CANN10              DELETE    TITAD000
009100940713     C                   ENDIF
009200940713     C                   ENDIF
009300921028     C*
009400990716     C                   READ      TITAD004                               32
009500940713     C                   ENDDO
009600921012     C*
009700990716     C****  PULIZIA  TITPT  ****
009800990716     C     *LOVAL        SETLL     TITPT001
009900990716     C                   READ      TITPT001                               32
010000940713     C     *IN32         DOWEQ     *OFF
010100921016     C*
010200950105     C     TPTATB        IFNE      ' '
010300921013     C** SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
010400940713     C     TPTFTR        IFNE      *BLANKS
010500990716     C     TPTNRR        CHAIN     TITPT000                           3010
010600990716     C  N30
010700990716     CANN10              DELETE    TITPT000
010800940713     C                   ENDIF
010900940713     C                   ENDIF
011000921028     C*
011100990716     C                   READ      TITPT001                               32
011200940713     C                   ENDDO
011300921016     C*
011400990716     C****  PULIZIA  TITPD  ****
011500990716     C     *LOVAL        SETLL     TITPD001
011600990716     C                   READ      TITPD001                               32
011700940713     C     *IN32         DOWEQ     *OFF
011800921015     C*
011900950105     C     TPDATB        IFNE      ' '
012000921013     C** SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
012100921119     C     TPDFTR        IFNE      *BLANKS
012200990716     C     TPDNRR        CHAIN     TITPD000                           3010
012300990716     C  N30
012400990716     CANN10              DELETE    TITPD000
012500940713     C                   ENDIF
012600940713     C                   ENDIF
012700921028     C*
012800990716     C                   READ      TITPD001                               32
012900940713     C                   ENDDO
013000950105     C*
013100990716     C****  PULIZIA  TITGC  ****
013200990716     C     *LOVAL        SETLL     TITGC001
013300990716     C                   READ      TITGC001                               32
013400950105     C     *IN32         DOWEQ     *OFF
013500950105     C*
013600950105     C     TGCATB        IFNE      ' '
013700950105     C** SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
013800950105     C     TGCFTR        IFNE      *BLANKS
013900990716     C     TGCNRR        CHAIN     TITGC000                           3010
014000990716     C  N30
014100990716     CANN10              DELETE    TITGC000
014200950105     C                   ENDIF
014300950105     C                   ENDIF
014400950105     C*
014500990716     C                   READ      TITGC001                               32
014600950105     C                   ENDDO
014700030613     C*
014800030613     C****  PULIZIA  TITAV  ****
014900030613     C     *LOVAL        SETLL     TITAV001
015000030613     C                   READ      TITAV001                               32
015100030613     C     *IN32         DOWEQ     *OFF
015200030613     C*
015300030613     C     TAVATB        IFNE      ' '
015400030613     C** SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
015500030613     C     TAVFTR        IFNE      *BLANKS
015600030613     C     TAVNRR        CHAIN     TITAV000                           3010
015700030613     C  N30
015800030613     CANN10              DELETE    TITAV000
015900030613     C                   ENDIF
016000030613     C                   ENDIF
016100030613     C*
016200030613     C                   READ      TITAV001                               32
016300030613     C                   ENDDO
016400921012     C*
016500970115     C****  PULIZIA FILE DATI TIPO: TNETC/TNETT/TNETS  ****
016600970122     C                   CALL      'TNTA09R'
016700970115     C                   PARM                    KPJBA
016800970115     C*
016900000000     C                   SETON                                        LR
