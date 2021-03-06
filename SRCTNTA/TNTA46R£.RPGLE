000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940722     H* TNTA46R *----------------------------------------------------*
000300940722     H*          - ANNULLAMENTO TARIFFE SCADUTE  -    ESEGUE         *
000400000000     H*--------------------------------------------------------------*
000500940722     FTNTAM01L  UF   E           K DISK
000600940722     FTNTAM00L  IF   E           K DISK
000700940726     F                                     RENAME(TNTAM000:TNTAML)
000800990716     FTITAD04L  UF   E           K DISK
000900990716     FTITPT01L  UF   E           K DISK
001000990716     FTITPD01L  UF   E           K DISK
001100990716     FTITGC01L  UF   E           K DISK
001200030613     FTITAV01L  UF   E           K DISK
001300961106     D***
001400961106     D* PASSAGGIO PARAMETRI DAL FILTRO              - TNTA45R -
001500961106     D***
001600000000     D PARAM           DS
001700940722     D* DATA IN AAAA/MM/GG
001800940722     D  AMGDTL                 1      8  0
001900940722     D* DATA IN GG/MM/AAAAA
002000940722     D  VIDDTL                 9     16  0
002100940722     D* STAMPA PROGRESSIVI (S/ )
002200940722     D  VIDSTA                17     17
002300940722     D*
002400921015     D                 DS
002500900409     D  PREKSC                 1      7  0
002600900409     D  PRECTR                 8     10  0
002700900409     D  PRECON                 1     10  0
002800900409     D  PREPRG                11     13  0
002900921015     D  PREKEY                 1     13  0
003000900409     D TAMKEY          DS
003100900409     D  TAMKSC                 1      7  0
003200900409     D  TAMCTR                 8     10  0
003300900409     D  TAMCON                 1     10  0
003400900409     D  TAMPRG                11     13  0
003500940726     D                 DS
003600021104     D BS56DS        E DS                  EXTNAME(TIBS56DS)
003700021104     d  FIL                    7    756  0 DIM(250)
003800021104      *
003900970115     D*
004000970122     D* DS PER TNTA47R - ANNULLAMENTO DATI TIPO
004100970122     D DSTA47        E DS                  EXTNAME(TNTA47DS)
004200961106     D*
004300000000     D KPJBA         E DS
004400021104     D CNCR80        E DS
004500021104     D UT�DSE        E DS                  EXTNAME(UT�DSE0F)
004600040914
004700040914     D WLBDAT          DS
004800040914     D  G02DAT                 1      8  0
004900040914     D  G02INV                 9     16  0
005000040914     D  G02ERR                17     17
005100040914     D  G02TGI                18     22  0
005200921014     C*****************************************************************
005300921014     C* RIEPILOGO INDICATORI
005400921014     C*****************************************************************
005500921014     C* 40    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
005600921014     C*         IL RECORD E' ALLOCATO (CIOE' *IN40 ACCESO) SALTO LA
005700921014     C*         TARIFFA (CHE VERRA' ANNULLATA LA PROSSIMA VOLTA)
005800921014     C* 30/34 - DI COMODO
005900961106     C* 35    - USATO PER LOKUP
006000921014     C*****************************************************************
006100921118     C                   SETON                                        OF
006200921118     C                   MOVE      *ALL'9'       PREKEY
006300921118     C*
006400940722     C     KTAM          SETLL     TNTAML
006500900607     C     LEGGI         TAG
006600940722     C                   READP     TNTAML                                 30
006700900409     C*
006800900409     C* 30 ON - FINE LETTURA
006900921026     C  N30              DO
007000921014     C*
007100961106     C                   MOVEL     TAMKSC        FILF              3 0
007200961106     C* SE SONO IN SEDE ELABORO SOLO LE TARIFFE DI CARTELLO
007300021104     C     SIMFEL        IFEQ      0
007400961106     C     FILF          IFNE      888
007500921119     C                   GOTO      LEGGI
007600921119     C                   END
007700921119     C*
007800900607     C                   ELSE
007900961106     C* SE SONO IN FILIALE ELABORO SOLO LE TARIFFE DELLE FILIALI DELLA
008000021113     C* FIL  (ESCLUDENDO ANCHE LE TARIFFE DI CARTELLO)
008100021113     C     FILF          LOOKUP    FIL                                    35
008200961106     C  N35              GOTO      LEGGI
008300921119     C                   END
008400921119     C*
008500900409     C* CONTROLLO SE E' L'ULTIMO PROGRESSIVO
008600900409     C     TAMCON        IFNE      PRECON
008700001206     C     TAMDDT        IFLT      AMGDTL
008800900409     C                   MOVE      TAMKEY        PREKEY
008900001206     C                   ENDIF
009000900409     C                   ELSE
009100900822     C                   MOVE      TAMKEY        PREKEY
009200900409     C*
009300940726     C* CONTROLLO SE DATA SCADENZA E' NEL LIMITE
009400900409     C     TAMDST        IFLE      AMGDTL
009500921014     C*
009600940722     C     KTAM          CHAIN     TNTAM000                           3140
009700921026     C* 40 ON - RECORD ALLOCATO PER CUI SALTO LA TARIFFA
009800921026     C  N31
009900921026     CANN40              DO
010000921014     C* ANNULLO DETTAGLIO TARIFFA
010100921014     C                   EXSR      ANNTAD
010200970115     C*
010300950106     C* ANNULLO TARIFFE PARTICOLARI E TARIFFE GIACENZA
010400921014     C                   EXSR      ANNULL
010500030627     C*
010600030627     C* ANNULLO TARIFFE STORICO VARIAZIONI
010700030627     C                   EXSR      ANNTAV
010800970115     C*
010900061009     C* ANNULLO DATI TIPO RELATIVI ALLA TARIFFA
011000040303     C                   CLEAR                   DSTA47
011100040303     C                   MOVEL     '1'           WTA47             1
011200040303     C                   MOVEL     '4'           D47TUP
011300040303     C                   MOVEL     'S'           D47DSF
011400040303     C                   MOVEL     'T'           D47CTO
011500040303     C                   Z-ADD     PREKSC        D47KSC
011600040303     C                   Z-ADD     PRECTR        D47CTR
011700040303     C                   Z-ADD     PREPRG        D47PRG
011800040303     C                   MOVEL     DSTA47        KPJBU
011900040303     C                   CALL      'TNTA47R'
012000040303     C                   PARM                    KPJBA
012100970115     C*
012200921014     C* ANNULLO TESTATA TARIFFA
012300900409     C                   MOVE      'A'           TAMATB
012400040914     C***                Z-ADD     0             TAMDTR
012500040914     C                   Z-ADD     DATEU8        TAMDTR
012600940726     C                   MOVE      *BLANKS       TAMFTR
012700940722     C                   UPDATE    TNTAM000
012800921026     C                   END
012900900409     C                   END
013000900409     C                   END
013100921026     C*
013200900409     C                   END
013300921015     C*
013400900607     C  N30              GOTO      LEGGI
013500921118     C*
013600970122     C* CALL A VUOTO AL PGM TNTA47R PER CHIUDERE I FILES
013700970122     C     WTA47         IFEQ      '1'
013800970122     C                   CLEAR                   DSTA47
013900970122     C                   MOVEL     'C'           D47TLA
014000970122     C                   MOVEL     DSTA47        KPJBU
014100970122     C                   CALL      'TNTA47R'
014200970115     C                   PARM                    KPJBA
014300970115     C                   ENDIF
014400970115     C*
014500000000     C                   SETON                                        LR
014600900409     C*
014700921014     C*--- ANNULLO DETTAGLIO TARIFFA ---------------------------------*
014800921014     C*
014900900409     C     ANNTAD        BEGSR
015000990716     C     KTAM          SETLL     TITAD000
015100990716     C     KTAM          READE     TITAD000                               32
015200921014     C     *IN32         DOWEQ     '0'
015300921014     C                   MOVE      'A'           TADATB
015400040914     C****               Z-ADD     0             TADDTR
015500040914     C                   Z-ADD     DATEU8        TADDTR
015600940726     C                   MOVE      *BLANKS       TADFTR
015700990716     C                   UPDATE    TITAD000
015800921014     C*
015900990716     C     KTAM          READE     TITAD000                               32
016000921014     C                   END
016100900409     C                   ENDSR
016200900409     C*
016300950106     C*--- ANNULLO TARIFFE PARTICOLARI E TARIFFE GIACENZA -----------*
016400921014     C*
016500921014     C     ANNULL        BEGSR
016600950106     C* ANNULLO DETTAGLIO TARIFFE PARTICOLARI
016700990716     C     KTAM          SETLL     TITPD000
016800990716     C     KTAM          READE     TITPD000                               33
016900921014     C     *IN33         DOWEQ     '0'
017000921014     C                   MOVE      'A'           TPDATB
017100040914     C***                Z-ADD     0             TPDDTR
017200040914     C                   Z-ADD     DATEU8        TPDDTR
017300921014     C                   MOVE      *BLANKS       TPDFTR
017400990716     C                   UPDATE    TITPD000
017500921014     C*
017600990716     C     KTAM          READE     TITPD000                               33
017700921014     C                   END
017800950106     C*
017900950106     C* ANNULLO TESTATA   TARIFFE PARTICOLARI
018000990716     C     KTAM          SETLL     TITPT000
018100990716     C     KTAM          READE     TITPT000                               34
018200921014     C     *IN34         DOWEQ     '0'
018300921014     C                   MOVE      'A'           TPTATB
018400040914     C*****              Z-ADD     0             TPTDTR
018500040914     C                   Z-ADD     DATEU8        TPTDTR
018600921014     C                   MOVE      *BLANKS       TPTFTR
018700990716     C                   UPDATE    TITPT000
018800921014     C*
018900990716     C     KTAM          READE     TITPT000                               34
019000921014     C                   END
019100950106     C*
019200950106     C* ANNULLO TARIFFE GIACENZA
019300990716     C     KTAM          SETLL     TITGC000
019400990716     C     KTAM          READE     TITGC000                               33
019500950106     C     *IN33         DOWEQ     '0'
019600950106     C                   MOVE      'A'           TGCATB
019700040914     C***                Z-ADD     0             TGCDTR
019800040914     C                   Z-ADD     DATEU8        TGCDTR
019900950106     C                   MOVE      *BLANKS       TGCFTR
020000990716     C                   UPDATE    TITGC000
020100950106     C*
020200990716     C     KTAM          READE     TITGC000                               33
020300950106     C                   END
020400950106     C*
020500921014     C                   ENDSR
020600921014     C*
020700030627     C*
020800030627     C*--- ANNULLO TARIFFE STORICO VARIAZIONI -----------------------*
020900030627     C*
021000030627     C     ANNTAV        BEGSR
021100030627     C*
021200030627     C* ANNULLO TARIFFE STORICO VARIAZIONI
021300030627     C     KTAM          SETLL     TITAV000
021400030627     C     KTAM          READE     TITAV000                               33
021500030627     C     *IN33         DOWEQ     '0'
021600030627     C                   MOVE      'A'           TAVATB
021700040914     C****               Z-ADD     0             TAVDTR
021800040914     C                   Z-ADD     DATEU8        TAVDTR
021900030627     C                   MOVE      *BLANKS       TAVFTR
022000030627     C                   UPDATE    TITAV000
022100030627     C*
022200030627     C     KTAM          READE     TITAV000                               33
022300030627     C                   END
022400030627     C*
022500030627     C                   ENDSR
022600961106     C*--- SR INIZIALE -----------------------------------------------*
022700961106     C     *INZSR        BEGSR
022800961106     C****
022900961106     C** KLIST
023000961106     C****
023100961106     C     KTAM          KLIST
023200961106     C                   KFLD                    PREKSC
023300961106     C                   KFLD                    PRECTR
023400961106     C                   KFLD                    PREPRG
023500961106     C*
023600961106     C*
023700961106     C****  PARAMETRI  ****
023800961106     C     *ENTRY        PLIST
023900961106     C                   PARM                    KPJBA
024000961106     C                   MOVEL     KPJBU         PARAM
024100021104      *
024200021104     C                   Z-ADD     1             CODUT
024300021104     C                   CALL      'X�PARUT'
024400021104     C                   PARM                    UT�DSE
024500021104     C                   MOVEL     RAGUT         RSUT             20
024600021104     C                   MOVEL     REC80         CNCR80
024700021104      *
024800021104     C* CHIAMO TIBS56R IN MODALITA' "POS"
024900021104     C                   CLEAR                   BS56DS
025000021104     C                   CLEAR                   BS56DS
025100021105     C                   MOVE      Simfel        I56PPO
025200021104     C                   MOVEL     'POS'         I56MOD
025300021104     C                   CALL      'TIBS56R'
025400021104     C                   PARM                    BS56DS
025500021104     C**
025600040914     C***
025700040914     C                   TIME                    W0140            14 0
025800040914     C                   MOVE      W0140         dateu8            8 0
025900040914     C*
026000040914     C                   Z-ADD     DATEU8        G02DAT
026100040914     C                   MOVEL     *BLANK        G02ERR
026200040914     C                   CALL      'XSRDA8'
026300040914     C                   PARM                    WLBDAT
026400040914     C* UDATE A 8 IN AAAA/MM/GG
026500040914     C                   Z-ADD     G02INV        dateu8
026600961106     C*
026700961106     C                   ENDSR
