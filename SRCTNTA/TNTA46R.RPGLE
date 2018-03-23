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
004500021104     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004600160209
004700160209       // -?ds dati passati al TRSM90R - Cancella file Tariffe Inserite
004800160209     d TRSM90DS      e ds
004900040914
005000040914     D WLBDAT          DS
005100040914     D  G02DAT                 1      8  0
005200040914     D  G02INV                 9     16  0
005300040914     D  G02ERR                17     17
005400040914     D  G02TGI                18     22  0
005500160209
005600160209      //---------------------------------------------------------------
005700160209      //?Definizione procedure esterne.
005800160209      //---------------------------------------------------------------
005900160209      // - Pgm Cancella file Tariffe Inserite
006000160209     d trsm90r         pr                  extpgm('TRSM90R')
006100160209     d  kpjba                              likeds(KPJBA)
006200160209     d  trsm90ds                           likeds(TRSM90DS)
006300160209
006400921014     C*****************************************************************
006500921014     C* RIEPILOGO INDICATORI
006600921014     C*****************************************************************
006700921014     C* 40    - UTILIZZATO PER GESTIRE L'ALLOCAZIONE DEL RECORD : SE
006800921014     C*         IL RECORD E' ALLOCATO (CIOE' *IN40 ACCESO) SALTO LA
006900921014     C*         TARIFFA (CHE VERRA' ANNULLATA LA PROSSIMA VOLTA)
007000921014     C* 30/34 - DI COMODO
007100961106     C* 35    - USATO PER LOKUP
007200921014     C*****************************************************************
007300921118     C                   SETON                                        OF
007400921118     C                   MOVE      *ALL'9'       PREKEY
007500921118     C*
007600940722     C     KTAM          SETLL     TNTAML
007700900607     C     LEGGI         TAG
007800940722     C                   READP     TNTAML                                 30
007900900409     C*
008000900409     C* 30 ON - FINE LETTURA
008100921026     C  N30              DO
008200921014     C*
008300961106     C                   MOVEL     TAMKSC        FILF              3 0
008400961106     C* SE SONO IN SEDE ELABORO SOLO LE TARIFFE DI CARTELLO
008500021104     C     SIMFEL        IFEQ      0
008600961106     C     FILF          IFNE      888
008700921119     C                   GOTO      LEGGI
008800921119     C                   END
008900921119     C*
009000900607     C                   ELSE
009100961106     C* SE SONO IN FILIALE ELABORO SOLO LE TARIFFE DELLE FILIALI DELLA
009200021113     C* FIL  (ESCLUDENDO ANCHE LE TARIFFE DI CARTELLO)
009300021113     C     FILF          LOOKUP    FIL                                    35
009400961106     C  N35              GOTO      LEGGI
009500921119     C                   END
009600921119     C*
009700900409     C* CONTROLLO SE E' L'ULTIMO PROGRESSIVO
009800900409     C     TAMCON        IFNE      PRECON
009900001206     C     TAMDDT        IFLT      AMGDTL
010000900409     C                   MOVE      TAMKEY        PREKEY
010100001206     C                   ENDIF
010200900409     C                   ELSE
010300900822     C                   MOVE      TAMKEY        PREKEY
010400900409     C*
010500940726     C* CONTROLLO SE DATA SCADENZA E' NEL LIMITE
010600900409     C     TAMDST        IFLE      AMGDTL
010700921014     C*
010800940722     C     KTAM          CHAIN     TNTAM000                           3140
010900921026     C* 40 ON - RECORD ALLOCATO PER CUI SALTO LA TARIFFA
011000921026     C  N31
011100921026     CANN40              DO
011200921014     C* ANNULLO DETTAGLIO TARIFFA
011300921014     C                   EXSR      ANNTAD
011400970115     C*
011500950106     C* ANNULLO TARIFFE PARTICOLARI E TARIFFE GIACENZA
011600921014     C                   EXSR      ANNULL
011700030627     C*
011800030627     C* ANNULLO TARIFFE STORICO VARIAZIONI
011900030627     C                   EXSR      ANNTAV
012000970115     C*
012100061009     C* ANNULLO DATI TIPO RELATIVI ALLA TARIFFA
012200040303     C                   CLEAR                   DSTA47
012300040303     C                   MOVEL     '1'           WTA47             1
012400040303     C                   MOVEL     '4'           D47TUP
012500040303     C                   MOVEL     'S'           D47DSF
012600040303     C                   MOVEL     'T'           D47CTO
012700040303     C                   Z-ADD     PREKSC        D47KSC
012800040303     C                   Z-ADD     PRECTR        D47CTR
012900040303     C                   Z-ADD     PREPRG        D47PRG
013000040303     C                   MOVEL     DSTA47        KPJBU
013100040303     C                   CALL      'TNTA47R'
013200040303     C                   PARM                    KPJBA
013300970115     C*
013400921014     C* ANNULLO TESTATA TARIFFA
013500900409     C                   MOVE      'A'           TAMATB
013600040914     C                   Z-ADD     DATEU8        TAMDTR
013700940726     C                   MOVE      *BLANKS       TAMFTR
013800940722     C                   UPDATE    TNTAM000
013900160209      /free
014000160209       //?richiamo pgm per cancellare:
014100160209       //?file Tariffe Inserite
014200160209         clear TRSM90DS;
014300160209         ISM90ksc = TAMksc;
014400160209         ISM90ctr = TAMctr;
014500160209         ISM90prg = TAMprg;
014600160209         trsm90r (kpjba:TRSM90DS);
014700160209      /end-free
014800921026     C                   END
014900900409     C                   END
015000900409     C                   END
015100921026     C*
015200900409     C                   END
015300921015     C*
015400900607     C  N30              GOTO      LEGGI
015500921118     C*
015600970122     C* CALL A VUOTO AL PGM TNTA47R PER CHIUDERE I FILES
015700970122     C     WTA47         IFEQ      '1'
015800970122     C                   CLEAR                   DSTA47
015900970122     C                   MOVEL     'C'           D47TLA
016000970122     C                   MOVEL     DSTA47        KPJBU
016100970122     C                   CALL      'TNTA47R'
016200970115     C                   PARM                    KPJBA
016300970115     C                   ENDIF
016400970115     C*
016500000000     C                   SETON                                        LR
016600900409     C*
016700921014     C*--- ANNULLO DETTAGLIO TARIFFA ---------------------------------*
016800921014     C*
016900900409     C     ANNTAD        BEGSR
017000990716     C     KTAM          SETLL     TITAD000
017100990716     C     KTAM          READE     TITAD000                               32
017200921014     C     *IN32         DOWEQ     '0'
017300921014     C                   MOVE      'A'           TADATB
017400040914     C                   Z-ADD     DATEU8        TADDTR
017500940726     C                   MOVE      *BLANKS       TADFTR
017600990716     C                   UPDATE    TITAD000
017700921014     C*
017800990716     C     KTAM          READE     TITAD000                               32
017900921014     C                   END
018000900409     C                   ENDSR
018100900409     C*
018200950106     C*--- ANNULLO TARIFFE PARTICOLARI E TARIFFE GIACENZA -----------*
018300921014     C*
018400921014     C     ANNULL        BEGSR
018500950106     C* ANNULLO DETTAGLIO TARIFFE PARTICOLARI
018600990716     C     KTAM          SETLL     TITPD000
018700990716     C     KTAM          READE     TITPD000                               33
018800921014     C     *IN33         DOWEQ     '0'
018900921014     C                   MOVE      'A'           TPDATB
019000040914     C                   Z-ADD     DATEU8        TPDDTR
019100921014     C                   MOVE      *BLANKS       TPDFTR
019200990716     C                   UPDATE    TITPD000
019300921014     C*
019400990716     C     KTAM          READE     TITPD000                               33
019500921014     C                   END
019600950106     C*
019700950106     C* ANNULLO TESTATA   TARIFFE PARTICOLARI
019800990716     C     KTAM          SETLL     TITPT000
019900990716     C     KTAM          READE     TITPT000                               34
020000921014     C     *IN34         DOWEQ     '0'
020100921014     C                   MOVE      'A'           TPTATB
020200040914     C                   Z-ADD     DATEU8        TPTDTR
020300921014     C                   MOVE      *BLANKS       TPTFTR
020400990716     C                   UPDATE    TITPT000
020500921014     C*
020600990716     C     KTAM          READE     TITPT000                               34
020700921014     C                   END
020800950106     C*
020900950106     C* ANNULLO TARIFFE GIACENZA
021000990716     C     KTAM          SETLL     TITGC000
021100990716     C     KTAM          READE     TITGC000                               33
021200950106     C     *IN33         DOWEQ     '0'
021300950106     C                   MOVE      'A'           TGCATB
021400040914     C                   Z-ADD     DATEU8        TGCDTR
021500950106     C                   MOVE      *BLANKS       TGCFTR
021600990716     C                   UPDATE    TITGC000
021700950106     C*
021800990716     C     KTAM          READE     TITGC000                               33
021900950106     C                   END
022000950106     C*
022100921014     C                   ENDSR
022200921014     C*
022300030627     C*
022400030627     C*--- ANNULLO TARIFFE STORICO VARIAZIONI -----------------------*
022500030627     C*
022600030627     C     ANNTAV        BEGSR
022700030627     C*
022800030627     C* ANNULLO TARIFFE STORICO VARIAZIONI
022900030627     C     KTAM          SETLL     TITAV000
023000030627     C     KTAM          READE     TITAV000                               33
023100030627     C     *IN33         DOWEQ     '0'
023200030627     C                   MOVE      'A'           TAVATB
023300040914     C                   Z-ADD     DATEU8        TAVDTR
023400030627     C                   MOVE      *BLANKS       TAVFTR
023500030627     C                   UPDATE    TITAV000
023600030627     C*
023700030627     C     KTAM          READE     TITAV000                               33
023800030627     C                   END
023900030627     C*
024000030627     C                   ENDSR
024100961106     C*--- SR INIZIALE -----------------------------------------------*
024200961106     C     *INZSR        BEGSR
024300961106     C****
024400961106     C** KLIST
024500961106     C****
024600961106     C     KTAM          KLIST
024700961106     C                   KFLD                    PREKSC
024800961106     C                   KFLD                    PRECTR
024900961106     C                   KFLD                    PREPRG
025000961106     C*
025100961106     C*
025200961106     C****  PARAMETRI  ****
025300961106     C     *ENTRY        PLIST
025400961106     C                   PARM                    KPJBA
025500961106     C                   MOVEL     KPJBU         PARAM
025600021104      *
025700021104     C                   Z-ADD     1             CODUT
025800021104     C                   CALL      'X§PARUT'
025900021104     C                   PARM                    UT§DSE
026000021104     C                   MOVEL     RAGUT         RSUT             20
026100021104     C                   MOVEL     REC80         CNCR80
026200021104      *
026300021104     C* CHIAMO TIBS56R IN MODALITA' "POS"
026400021104     C                   CLEAR                   BS56DS
026500021104     C                   CLEAR                   BS56DS
026600021105     C                   MOVE      Simfel        I56PPO
026700021104     C                   MOVEL     'POS'         I56MOD
026800021104     C                   CALL      'TIBS56R'
026900021104     C                   PARM                    BS56DS
027000021104     C**
027100040914     C***
027200040914     C                   TIME                    W0140            14 0
027300040914     C                   MOVE      W0140         dateu8            8 0
027400040914     C*
027500040914     C                   Z-ADD     DATEU8        G02DAT
027600040914     C                   MOVEL     *BLANK        G02ERR
027700040914     C                   CALL      'XSRDA8'
027800040914     C                   PARM                    WLBDAT
027900040914     C* UDATE A 8 IN AAAA/MM/GG
028000040914     C                   Z-ADD     G02INV        dateu8
028100961106     C*
028200961106     C                   ENDSR
