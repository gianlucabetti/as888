000100980323     H DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300140509     H* CREAZIONE TARIFFA PARTICOLARE 'm' AVVISO AFFIDAMENTO SPEDIZIONE        *
000400140509     H* CHE NON L'HANNO E PER TUTTI QUELLI CHE L'HANNO ANNULLATA               *
000500980323     H*------------------------------------------------------------------------*
000600980323     F*
000700990923     FTNTAM01L  IF   E           K DISK
000800990928     FTITPT01L  UF A E           K DISK
000900990928     FTITPD01L  UF A E           K DISK
001000980323     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001100980323     F*
001200140509     D* SCHIERE
001300980323     D ST              S             66    DIM(3) CTDATA PERRCD(1)
001400980323      *
001500990611     D KPJBA         E DS
001600980323     I*
001700980323      *
001800990923     C                   Z-ADD     1             CODUT             1 0
001900980324     C*
002000990611     C* ACCESSO TITPT01L
002100980324     C     KTPT          KLIST
002200980324     C                   KFLD                    TAMKSC
002300980324     C                   KFLD                    TAMCTR
002400980324     C                   KFLD                    TAMPRG
002500990923     C                   KFLD                    RITIRO
002600990924     C*
002700990924     C* ACCESSO TITPD01L
002800990924     C     KTPD          KLIST
002900990924     C                   KFLD                    TPTKSC
003000990924     C                   KFLD                    TPTCTR
003100990924     C                   KFLD                    TPTPRG
003200990924     C                   KFLD                    TPTFTC
003300990923     C*
003400990923     C***
003500990923     C** DEFINIZIONE CAMPI
003600990923     C***
003700140509     C     *LIKE         DEFINE    TPDORP        WORPE
003800140509     C     *LIKE         DEFINE    TPDORP        WORPI
003900140509     C     *LIKE         DEFINE    TPDCTS        WCTSE
004000140509     C     *LIKE         DEFINE    TPDCTS        WCTSI
004100990924     C     *LIKE         DEFINE    TPDSGL        WSGL
004200140509     C     *LIKE         DEFINE    TPTFTC        WFTC
004300140509     C     *LIKE         DEFINE    TPDITR        WITR
004400140509     C                   MOVEL     'm '          WFTC
004500140509     C                   Z-ADD     1             WSGL
004600140509     C                   Z-ADD     0,3           WITR
004700140509     C                   MOVEL     'IT'          WCTSI
004800140509     C                   MOVEL     'A 9'         WORPI
004900140509     C                   MOVEL     'EE'          WCTSE
005000140509     C                   MOVEL     '999'         WORPE
005100980323     C*
005200980323     C                   EXCEPT    TESTA
005300980323     C*
005400980323     C     *LOVAL        SETLL     TNTAM01L
005500980323     C                   READ      TNTAM01L                               30
005600980323     C*
005700990923    0C     *IN30         DOWEQ     *OFF
005800990924     C*
005900990924     C* TARIFFA NON ANNULLATA
006000990924     C     TAMATB        IFEQ      ' '
006100990924     C* E NON E' TARIFFA DI CARTELLO
006200990924     C     W0030         ANDNE     888
006300980323     C*
006400990924     C* CONTROLLO : SE IL CLIENTE HA GIA' LA TARIFFA PARTICOLARE E NON ANNULLATA
006500990923     C     KTPT          CHAIN     TITPT01L                           30
006600990923    1C     *IN30         IFEQ      *OFF
006700990923     C     TPTATB        ANDEQ     ' '
006800990825     C                   GOTO      LEGGI
006900990923    1C                   ENDIF
007000990924     C* SE E' ANNULLATA RIPRISTINO LA TESTATA
007100990924     C     TPTATB        IFEQ      'A'
007200990924     C* SE TPD ANNULLATO CANCELLO TUTTO E RISCRIVO
007300990924     C                   EXSR      DELTPD
007400990924     C                   ENDIF
007500990924     C* TARIFFA A SPEDIZIONE
007600990924     C                   MOVEL     '2'           TPTTPG
007700990924     C* CREAZIONE TARIFFA PARTICOLARE
007800990924     C* SCRIVO LA TESTATA DELLA TARIFFA PARTICOLARE  RITIRO
007900140509     C                   MOVEL     'm '          TPTFTC
008000990924     C                   EXSR      VALTPT
008100990611     C*
008200990611     C* CREAZIONE DETTAGLIO TARIFFA PARTICOLARE  1° SCAGLIONE
008300990611     C*
008400140509     C                   Z-ADD     WSGL          TPDSGL
008500140509     C                   Z-ADD     WITR          TPDITR
008600990923     C                   Z-ADD     *ZEROS        TPDMIN
008700980323     C                   Z-ADD     *ZEROS        TPDMAX
008800980323     C*
008900980323     C                   EXSR      VALTPD
009000990611     C*
009100980323     C  N90
009200980323     CAN OF              EXCEPT    TESTA
009300980323     C  N90              EXCEPT    RIGA
009400980323     C  N90              SETON                                        90
009500980323     C*
009600990924     C                   ENDIF
009700990924     C*
009800990825     C     LEGGI         TAG
009900980323     C*
010000990923     C                   READ      TNTAM01L                               30
010100980323     C*
010200990923    0C                   ENDDO
010300980323     C*
010400980323     C                   SETON                                        LR
010500980323     C*-----------------------------------------------------------------------**
010600980323     C* VALORIZZO I CAMPI DELLE TARIFFE PARTICOLARE
010700980323     C*-----------------------------------------------------------------------**
010800980323     C     VALTPT        BEGSR
010900980323     C*
011000140509     C                   CLEAR                   TITP000
011100990820     C* METTO IL FLAG DI TRASMISSIONE DELLA TESTATA
011200990820     C  N35              MOVEL     TAMFTR        TPTFTR
011300990820     C  N35              Z-ADD     TAMDTR        TPTDTR
011400980507     C*
011500980323     C                   MOVE      TAMKSC        TPTKSC
011600980323     C                   MOVE      TAMCTR        TPTCTR
011700980323     C                   MOVE      TAMPRG        TPTPRG
011800980323     C*
011900990928     C                   WRITE     TITPT000
012000980323     C*
012100980323     C                   ENDSR
012200980323     C*-----------------------------------------------------------------------**
012300980323     C* VALORIZZO I CAMPI DELLE TARIFFE PARTICOLARE DETTAGLIO
012400980323     C*-----------------------------------------------------------------------**
012500980323     C     VALTPD        BEGSR
012600980323     C*
012700990923     C                   MOVEL     TPTATB        TPDATB
012800980323     C                   MOVE      TAMKSC        TPDKSC
012900980323     C                   MOVE      TAMCTR        TPDCTR
013000980323     C                   MOVE      TAMPRG        TPDPRG
013100980323     C                   MOVE      TPTFTC        TPDFTC
013200990611     C                   CLEAR                   TPDFTR
013300990611     C                   CLEAR                   TPDDTR
013400990820     C* METTO IL FLAG DI TRASMISSIONE DELLA TESTATA
013500990611     C  N35              MOVEL     TAMFTR        TPDFTR
013600990611     C  N35              Z-ADD     TAMDTR        TPDDTR
013700990923     C*  TARIFFA ITALIA
013800980323     C                   MOVEL     'IT'          TPDCTS
013900980323     C* SCRIVO IL DETTAGLIO
014000990924     C                   MOVE      WORP          TPDORP
014100990928     C                   WRITE     TITPD000
014200980323     C*
014300980323     C                   ENDSR
014400990923     C*------------------------------------------------------------------------*
014500990923     C* PULIZIA DEL DETTAGLIO TARIFFE
014600990923     C*------------------------------------------------------------------------*
014700990923     C     DELTPD        BEGSR
014800990924     C*
014900990928     C                   DELETE    TITPT000
015000990923     C*
015100140509     C* VERIFICO SE ESISTE IL DETTAGLIO CHE VOGLIO SCRIVERE E LO DELETO
015200140509     C     KTPD          SETLL     TITPD01L
015300140509     C     KTPD          READE     TITPD01L                               46
015400140509     C*
015500140509    0C     *IN46         DOWEQ     *OFF
015600990928     C  N46              DELETE    TITPD000
015700140509     C     KTPD          READE     TITPD01L                               46
015800140509     C                   ENDDO
015900990923     C*
016000990923     C                   ENDSR
016100990611     C*
016200980323     OQSYSPRT   E            TESTA            02
016300980323     O                       ST(1)               66
016400980323     O                       ST(1)              132
016500980323     O          E            TESTA            03
016600980323     O                       ST(2)               66
016700980323     O                       ST(3)              132
016800980323     O          E            TESTA            04
016900980323     O                       ST(1)               66
017000980323     O                       ST(1)              132
017100980323     O          E            RIGA        2  0
017200980323     O                       TAMKSC        Z      7
017300980323     O                       TAMCTR        Z     12
017400980323     O                                           13 '/'
017500990611     O                       TAMPRG        Z     18
017600990611     O                       TAMFIE              25
017700990611**
017800980323------------------------------------------------------------------
017900140509Cliente  Tariffa    I/E
018000990611
