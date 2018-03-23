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
003100160209     fTICFT01L  uf   e           k disk
003200040915     fTabel00f  if   e           k disk
003300040915
003400921016     D TAR1            DS
003500921016     D  TAMNRR               397    400B 0
003600921016     D TAR2            DS
003700921016     D  TADNRR               397    400B 0
003800921016     D TAR3            DS
003900921016     D  TPTNRR               397    400B 0
004000921016     D TAR4            DS
004100921016     D  TPDNRR               397    400B 0
004200950105     D TAR5            DS
004300950105     D  TGCNRR               397    400B 0
004400030613     D TAR6            DS
004500030613     D  TAVNRR               397    400B 0
004600921013     D CNCR80        E DS
004700921013     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004800000000     D KPJBA         E DS
004900040915      ** GIORNI DI PULIZIA TARIFFE ANNULLATE
005000040915     d ds5as1        e ds
005100040915
005200040915     d kcod            s                   like(tblcod) inz('5A')
005300050127     d kkey            s                   like(tblkey) inz('SEDE1')
005400040915
005500040915     d datamax         s               d   datfmt(*iso)
005600040915     d dataoggi        s               d   datfmt(*iso)
005700040915     d wdata           s              8  0
005800160209
005900160209     d wEoF            s               n   inz
006000040915
006100921015     C*****************************************************************
006200921015     C* RIEPILOGO INDICATORI
006300921015     C*****************************************************************
006400921015     C* 10    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
006500921015     C*         IL RECORD E' ALLOCATO (CIOE' *IN10 ACCESO) SALTO LA
006600921015     C*         TARIFFA (CHE VERRA' ANNULLATA LA PROSSIMA VOLTA)
006700940713     C* 30/32 - DI COMODO
006800921015     C*****************************************************************
006900000000     C     *ENTRY        PLIST
007000000000     C                   PARM                    KPJBA
007100950207     C                   Z-ADD     1             CODUT
007200921013     C*
007300921013     C                   CALL      'X§PARUT'
007400921013     C                   PARM                    UT§DSE
007500921013     C                   MOVEL     REC80         CNCR80
007600040915
007700040915     c     ktabel        klist
007800040915     c                   kfld                    codut
007900040915     c                   kfld                    kcod
008000040915     c                   kfld                    kkey
008100040915
008200040915     c                   Clear                   ds5as1
008300040915     c     ktabel        chain     Tabel00f
008400040915     c                   If        %Found(Tabel00f)
008500040915     c                   Eval      ds5as1 = Tbluni
008600040915     c                   EndIf
008700040915
008800040915     c                   Move      *date         dataoggi
008900121220     c     dataoggi      ADDdur    1:*d          datamax
009000121220     c                   move      dataMAX       wdata
009100040915
009200000000     C*---------------------------------------------------------------*
009300940713     C****  PULIZIA  TNTAM  ****
009400940713     C     *LOVAL        SETLL     TNTAM001
009500940713     C                   READ      TNTAM001                               32
009600940713     C     *IN32         DOWEQ     *OFF
009700921016     C*
009800950105     C     TAMATB        IFNE      ' '
009900040915      * se data trasmissione è minore dei giorni indicati in tabella
010000040915     c     tamdtr        iflt      wdata
010100040915      *
010200940713     C     TAMNRR        CHAIN     TNTAM000                           3010
010300940713     C  N30
010400940713     CANN10              DELETE    TNTAM000
010500160209      /free
010600160209       //?cancello anche il file TICFT
010700160209         IF  not *in10 and not *in30;
010800160209           wEoF = *off;
010900160209           setll (TAMksc:TAMctr) TICFT01L;
011000160209           reade (TAMksc:TAMctr) TICFT01L;
011100160209           DOW not wEoF;
011200160209             IF  %eof(TICFT01L);
011300160209               leave;
011400160209             ENDIF;
011500160209             delete TICFT000;
011600160209             reade (TAMksc:TAMctr) TICFT01L;
011700160209           ENDDO;
011800160209         ENDIF;
011900160209      /end-free
012000940713     C                   ENDIF
012100940713     C                   ENDIF
012200921028     C*
012300940713     C                   READ      TNTAM001                               32
012400940713     C                   ENDDO
012500921016     C*
012600990716     C****  PULIZIA  TITAD  ****
012700990716     C     *LOVAL        SETLL     TITAD004
012800990716     C                   READ      TITAD004                               32
012900940713     C     *IN32         DOWEQ     *OFF
013000921015     C*
013100950105     C     TADATB        IFNE      ' '
013200040915      * se data trasmissione è minore dei giorni indicati in tabella
013300040915     c     taddtr        iflt      wdata
013400040915      *
013500990716     C     TADNRR        CHAIN     TITAD000                           3010
013600990716     C  N30
013700990716     CANN10              DELETE    TITAD000
013800940713     C                   ENDIF
013900940713     C                   ENDIF
014000921028     C*
014100990716     C                   READ      TITAD004                               32
014200940713     C                   ENDDO
014300921012     C*
014400990716     C****  PULIZIA  TITPT  ****
014500990716     C     *LOVAL        SETLL     TITPT001
014600990716     C                   READ      TITPT001                               32
014700940713     C     *IN32         DOWEQ     *OFF
014800921016     C*
014900950105     C     TPTATB        IFNE      ' '
015000040915      * se data trasmissione è minore dei giorni indicati in tabella
015100040915     c     tptdtr        iflt      wdata
015200040915      *
015300990716     C     TPTNRR        CHAIN     TITPT000                           3010
015400990716     C  N30
015500990716     CANN10              DELETE    TITPT000
015600940713     C                   ENDIF
015700940713     C                   ENDIF
015800921028     C*
015900990716     C                   READ      TITPT001                               32
016000940713     C                   ENDDO
016100921016     C*
016200990716     C****  PULIZIA  TITPD  ****
016300990716     C     *LOVAL        SETLL     TITPD001
016400990716     C                   READ      TITPD001                               32
016500940713     C     *IN32         DOWEQ     *OFF
016600921015     C*
016700950105     C     TPDATB        IFNE      ' '
016800040915      * se data trasmissione è minore dei giorni indicati in tabella
016900040915     c     tpddtr        iflt      wdata
017000040915      *
017100990716     C     TPDNRR        CHAIN     TITPD000                           3010
017200990716     C  N30
017300990716     CANN10              DELETE    TITPD000
017400940713     C                   ENDIF
017500940713     C                   ENDIF
017600921028     C*
017700990716     C                   READ      TITPD001                               32
017800940713     C                   ENDDO
017900950105     C*
018000990716     C****  PULIZIA  TITGC  ****
018100990716     C     *LOVAL        SETLL     TITGC001
018200990716     C                   READ      TITGC001                               32
018300950105     C     *IN32         DOWEQ     *OFF
018400950105     C*
018500950105     C     TGCATB        IFNE      ' '
018600040915      * se data trasmissione è minore dei giorni indicati in tabella
018700040915     c     tgcdtr        iflt      wdata
018800040915      *
018900990716     C     TGCNRR        CHAIN     TITGC000                           3010
019000990716     C  N30
019100990716     CANN10              DELETE    TITGC000
019200950105     C                   ENDIF
019300950105     C                   ENDIF
019400950105     C*
019500990716     C                   READ      TITGC001                               32
019600950105     C                   ENDDO
019700030613     C*
019800030613     C****  PULIZIA  TITAV  ****
019900030613     C     *LOVAL        SETLL     TITAV001
020000030613     C                   READ      TITAV001                               32
020100030613     C     *IN32         DOWEQ     *OFF
020200030613     C*
020300030613     C     TAVATB        IFNE      ' '
020400040915      * se data trasmissione è minore dei giorni indicati in tabella
020500040915     c     tavdtr        iflt      wdata
020600040915      *
020700030613     C     TAVNRR        CHAIN     TITAV000                           3010
020800030613     C  N30
020900030613     CANN10              DELETE    TITAV000
021000030613     C                   ENDIF
021100030613     C                   ENDIF
021200030613     C*
021300030613     C                   READ      TITAV001                               32
021400030613     C                   ENDDO
021500121220
021600000000     C                   SETON                                        LR
