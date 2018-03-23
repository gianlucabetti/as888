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
003100040915     fTabel00f  if   e           k disk
003200040915
003300921016     D TAR1            DS
003400921016     D  TAMNRR               397    400B 0
003500921016     D TAR2            DS
003600921016     D  TADNRR               397    400B 0
003700921016     D TAR3            DS
003800921016     D  TPTNRR               397    400B 0
003900921016     D TAR4            DS
004000921016     D  TPDNRR               397    400B 0
004100950105     D TAR5            DS
004200950105     D  TGCNRR               397    400B 0
004300030613     D TAR6            DS
004400030613     D  TAVNRR               397    400B 0
004500921013     D CNCR80        E DS
004600921013     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004700000000     D KPJBA         E DS
004800040915      ** GIORNI DI PULIZIA TARIFFE ANNULLATE
004900040915     d ds5as1        e ds
005000040915
005100040915     d kcod            s                   like(tblcod) inz('5A')
005200050127     d kkey            s                   like(tblkey) inz('SEDE1')
005300040915
005400040915     d datamax         s               d   datfmt(*iso)
005500040915     d dataoggi        s               d   datfmt(*iso)
005600040915     d wdata           s              8  0
005700040915
005800921015     C*****************************************************************
005900921015     C* RIEPILOGO INDICATORI
006000921015     C*****************************************************************
006100921015     C* 10    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
006200921015     C*         IL RECORD E' ALLOCATO (CIOE' *IN10 ACCESO) SALTO LA
006300921015     C*         TARIFFA (CHE VERRA' ANNULLATA LA PROSSIMA VOLTA)
006400940713     C* 30/32 - DI COMODO
006500921015     C*****************************************************************
006600000000     C     *ENTRY        PLIST
006700000000     C                   PARM                    KPJBA
006800950207     C                   Z-ADD     1             CODUT
006900921013     C*
007000921013     C                   CALL      'X§PARUT'
007100921013     C                   PARM                    UT§DSE
007200921013     C                   MOVEL     REC80         CNCR80
007300040915
007400040915     c     ktabel        klist
007500040915     c                   kfld                    codut
007600040915     c                   kfld                    kcod
007700040915     c                   kfld                    kkey
007800040915
007900040915     c                   Clear                   ds5as1
008000040915     c     ktabel        chain     Tabel00f
008100040915     c                   If        %Found(Tabel00f)
008200040915     c                   Eval      ds5as1 = Tbluni
008300040915     c                   EndIf
008400040915
008500040915     c                   Move      *date         dataoggi
008600040915     c     dataoggi      subdur    §5atar:*d     datamax
008700040915     c                   move      datamax       wdata
008800040915
008900000000     C*---------------------------------------------------------------*
009000940713     C****  PULIZIA  TNTAM  ****
009100940713     C     *LOVAL        SETLL     TNTAM001
009200940713     C                   READ      TNTAM001                               32
009300940713     C     *IN32         DOWEQ     *OFF
009400921016     C*
009500950105     C     TAMATB        IFNE      ' '
009600921015     C** SE LA FILIALE E' MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
009700040915     C**** TAMFTR        IFNE      *BLANKS
009800040915      * se data trasmissione è minore dei giorni indicati in tabella
009900040915     c     tamdtr        iflt      wdata
010000040915      *
010100940713     C     TAMNRR        CHAIN     TNTAM000                           3010
010200940713     C  N30
010300940713     CANN10              DELETE    TNTAM000
010400940713     C                   ENDIF
010500940713     C                   ENDIF
010600921028     C*
010700940713     C                   READ      TNTAM001                               32
010800940713     C                   ENDDO
010900921016     C*
011000990716     C****  PULIZIA  TITAD  ****
011100990716     C     *LOVAL        SETLL     TITAD004
011200990716     C                   READ      TITAD004                               32
011300940713     C     *IN32         DOWEQ     *OFF
011400921015     C*
011500950105     C     TADATB        IFNE      ' '
011600921015     C** SE SONO IN SEDE   E FIL. MECC. ANNULLO SE TRASMESSO/RICEVUTO
011700921015     C** SE SONO IN FILIALE                 "   "     "
011800040915     C*****TADFTR        IFNE      *BLANKS
011900040915      * se data trasmissione è minore dei giorni indicati in tabella
012000040915     c     taddtr        iflt      wdata
012100040915      *
012200990716     C     TADNRR        CHAIN     TITAD000                           3010
012300990716     C  N30
012400990716     CANN10              DELETE    TITAD000
012500940713     C                   ENDIF
012600940713     C                   ENDIF
012700921028     C*
012800990716     C                   READ      TITAD004                               32
012900940713     C                   ENDDO
013000921012     C*
013100990716     C****  PULIZIA  TITPT  ****
013200990716     C     *LOVAL        SETLL     TITPT001
013300990716     C                   READ      TITPT001                               32
013400940713     C     *IN32         DOWEQ     *OFF
013500921016     C*
013600950105     C     TPTATB        IFNE      ' '
013700921013     C** SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
013800040915     C***  TPTFTR        IFNE      *BLANKS
013900040915      * se data trasmissione è minore dei giorni indicati in tabella
014000040915     c     tptdtr        iflt      wdata
014100040915      *
014200990716     C     TPTNRR        CHAIN     TITPT000                           3010
014300990716     C  N30
014400990716     CANN10              DELETE    TITPT000
014500940713     C                   ENDIF
014600940713     C                   ENDIF
014700921028     C*
014800990716     C                   READ      TITPT001                               32
014900940713     C                   ENDDO
015000921016     C*
015100990716     C****  PULIZIA  TITPD  ****
015200990716     C     *LOVAL        SETLL     TITPD001
015300990716     C                   READ      TITPD001                               32
015400940713     C     *IN32         DOWEQ     *OFF
015500921015     C*
015600950105     C     TPDATB        IFNE      ' '
015700921013     C** SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
015800040915     C***  TPDFTR        IFNE      *BLANKS
015900040915      * se data trasmissione è minore dei giorni indicati in tabella
016000040915     c     tpddtr        iflt      wdata
016100040915      *
016200990716     C     TPDNRR        CHAIN     TITPD000                           3010
016300990716     C  N30
016400990716     CANN10              DELETE    TITPD000
016500940713     C                   ENDIF
016600940713     C                   ENDIF
016700921028     C*
016800990716     C                   READ      TITPD001                               32
016900940713     C                   ENDDO
017000950105     C*
017100990716     C****  PULIZIA  TITGC  ****
017200990716     C     *LOVAL        SETLL     TITGC001
017300990716     C                   READ      TITGC001                               32
017400950105     C     *IN32         DOWEQ     *OFF
017500950105     C*
017600950105     C     TGCATB        IFNE      ' '
017700950105     C** SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
017800040915     C**   TGCFTR        IFNE      *BLANKS
017900040915      * se data trasmissione è minore dei giorni indicati in tabella
018000040915     c     tgcdtr        iflt      wdata
018100040915      *
018200990716     C     TGCNRR        CHAIN     TITGC000                           3010
018300990716     C  N30
018400990716     CANN10              DELETE    TITGC000
018500950105     C                   ENDIF
018600950105     C                   ENDIF
018700950105     C*
018800990716     C                   READ      TITGC001                               32
018900950105     C                   ENDDO
019000030613     C*
019100030613     C****  PULIZIA  TITAV  ****
019200030613     C     *LOVAL        SETLL     TITAV001
019300030613     C                   READ      TITAV001                               32
019400030613     C     *IN32         DOWEQ     *OFF
019500030613     C*
019600030613     C     TAVATB        IFNE      ' '
019700030613     C** SE LA FILIALE E MECC. ANNULLO SOLO SE TRASMESSO O RICEVUTO
019800040915     C**** TAVFTR        IFNE      *BLANKS
019900040915      * se data trasmissione è minore dei giorni indicati in tabella
020000040915     c     tavdtr        iflt      wdata
020100040915      *
020200030613     C     TAVNRR        CHAIN     TITAV000                           3010
020300030613     C  N30
020400030613     CANN10              DELETE    TITAV000
020500030613     C                   ENDIF
020600030613     C                   ENDIF
020700030613     C*
020800030613     C                   READ      TITAV001                               32
020900030613     C                   ENDDO
021000921012     C*
021100970115     C****  PULIZIA FILE DATI TIPO: TNETC/TNETT/TNETS  ****
021200970122     C                   CALL      'TNTA09R'
021300970115     C                   PARM                    KPJBA
021400970115     C*
021500000000     C                   SETON                                        LR
