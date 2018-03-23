000100980323     H DECEDIT('0,') DATEDIT(*DMY.)
000200980323     H*------------------------------------------------------------------------*
000300000731     H* CREAZIONE TARIFFA PARTICOLARE                                          *
000400000731     H*                                                                        *
000500000731     H* VENGONO PASSATI A PGM I SEGUENTI PARAMETRI :                           *
000600000731     H*                                                                        *
000700000731     H* -- Codice cliente (di solito è la tariffa di cartello)                 *
000800000731     H* -- Codice Tariffa                                                      *
000900000731     H* -- Progressivo tariffa                                                 *
001000000731     H* -- Sigla della tariffa particolare da inglobare                        *
001100000731     H* -- Flag tariffa Italia / Estero (che può essere anche blanks nel caso  *
001200000731     H*    si inglobino entrambe)                                              *
001300000901     H* -- Flag Tariffa o Offerta (' '=Tariffa , 'O'=Offerta)
001400000919     H* -- Azienda
001500000731     H*                                                                        *
001600000731     H* Per tariffe DPD prendo la tariffa italia                               *
001700000731     H* Per tariffe Poste controllo se tariffa particolare abilitata           *
001800000804     H*                                                                        *
001900000804     H*                                                                        *
002000000804     H* In caso di tariffe in EURO devo fare la conversione                    *
002100000731     H*                                                                        *
002200000731     H* Se non esiste il record in tpt creo la tariffa particolare             *
002300000731     H* Se esiste ma annullata mi comporto come se non ci fosse                *
002400000731     H* Elaboro tutto il file a parte le 8888 e sflaggo solo i clienti della   *
002500000731     H* £1                                                                     *
002600000731     H*                                                                        *
002700980323     H*------------------------------------------------------------------------*
002800980323     F*
002900990923     FTNTAM01L  IF   E           K DISK
003000000804     FTITPT01L  UF A E           K DISK
003100000804     FTITPD01L  UF A E           K DISK
003200000731     FTITPTCAR  IF   E           K DISK    rename(titpt000:tptcar)
003300000731     FTITPDCAR  IF   E           K DISK    rename(titpd000:tpdcar)
003400000901     FTNTAMCAR  IF   E           K DISK    rename(tntam000:tamcar)
003500990923     FTABEL00F  IF   E           K DISK
003600980323     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
003700980323     F*
003800980323     D* SCHIERE PER CARICAMENTO L1
003900980323     D L1              S              3  0 DIM(30)                              FIL.GESTITE
004000980323     D ST              S             66    DIM(3) CTDATA PERRCD(1)
004100980323      *
004200990611     D KPJBA         E DS
004300990923     D DSCT          E DS
004400980323     D CNCR80        E DS
004500980323     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
004600980323     D* DS PER TRUL06R - CARICAMENTO £1
004700980323     D DSUL06        E DS                  EXTNAME(TRUL06DS)
004800980323     D  LIN                    1     90  0
004900980323     D                                     DIM(30)                              SKI COMODO
005000000804     D* DS per controllo tariffa particolare posta
005100000804     D DS1P          E DS
005200000804     D* DS per controllo tariffa con tipo servizio posta
005300000804     D DS5E          E DS
005400000804     D* ds flag operativi file tntam
005500000804     D DSTA01        E DS
005600000804     D** CAMBIO DIVISA
005700000804     D YEUR          E DS                  EXTNAME(YEURCODS)
005800000804     D* ds DIVISA
005900000804     D DSCV          E DS
006000000731      *
006100000731     D cod             S                   LIKE(TBLcod)
006200000731     D key             S                   LIKE(TBLkey)
006300000731      *
006400000803     D parksc          s              7
006500000803     D parctr          s              3
006600000803     D parprg          s              3
006700000803     D parftc          s              2
006800000803     D parfie          s              1
006900000901     D paroff          s              1
007000000919     D pardit          s              3
007100000803     D keyksc          s                   like(tamksc)
007200000803     D keyctr          s                   like(tamctr)
007300000803     D keyprg          s                   like(tamprg)
007400000803     D keyftc          s                   like(tptftc)
007500000803     D keyfie          s                   like(tamfie)
007600000804     D valcar          s                   like(§tadiv)
007700000731
007800000803     D prima           s              3
007900000802     D wtrtpd          s              1
008000980323      *
008100990923     C*
008200980323     C*
008300980323     C*
008400980323     C     *LOVAL        SETLL     TNTAM01L
008500980323     C*
008600000801    0C                   DO        *hival
008700000801     C                   READ      TNTAM01L                               30
008800000801     c* fine file
008900000801     c                   if        %eof(tntam01l)
009000000801     c                   leave
009100000801     c                   endif
009200000801     c* annullata
009300000801     c                   if        tamatb <> ' '
009400000801     c                   iter
009500000801     c                   endif
009600000828     c* se richiesta tariffa con tipo flag i/e diverso da quello da cambiare passo oltre
009700000801     c                   if        parfie <> ' '  and parfie <> tamfie
009800000801     c                   iter
009900000801     c                   endif
010000990924     C*
010100000828     C* CONTROLLO SE E' UNA tariffa di cartello
010200990924     C                   MOVEL     TAMKSC        W0030             3 0
010300000901     c* se si tratta doi offerta non faccio il controllo
010400000901     c                   if        paroff = 'O'
010500000901     c                   clear                   w0030
010600000901     c                   endif
010700000828     C* se è una tariffa  di cartello leggo la successiva
010800000801     C                   if        w0030 = 888
010900000801     c                   iter
011000000801     c                   endif
011100000919     c*
011200000919     c* verifico se si può uitlizzare sia da tariffe italia che da tariffe
011300000919     c* estere
011400000920     c                   if        §1pfie <> ' ' and §1pfie <> tamfie
011500000919     c                   iter
011600000919     c                   endif
011700000919     c* se è la tariffa particolare 'E' espresso verifico il tipo servizio della tariffa
011800000919     c* se stiamo convertendo la SDI non deve essere 'C' altrimenti non deve essere 'E'
011900000919     c                   if        parftc = 'E ' and ((pardit = 'SDI' and
012000000919     c                             tamtsp = 'C') or (pardit <> 'SDI' and
012100000919     c                             tamtsp = 'E'))
012200000919     c                   iter
012300000919     c                   endif
012400000920     c* se è una tariffa DPD non inglobo l'espresso
012500000920     c                   if        parftc = 'E'
012600000920     c                   movel     tamflo        dsta01
012700000920     c                   if        §tadpd = 'S'
012800000920     c                   iter
012900000920     c                   endif
013000000920     c                   endif
013100000804     c* se è una tariffa particolare utilizzabile da tutti va bene
013200000804     c* altrimenti
013300000804     c                   if        §1pfpt <> ' '
013400000804     C*
013500000804     C* CONTROLLO SE E' STO LEGGENDO UNA TARIFFA CON TIPO SERVIZIO POSTE
013600000804     C                   MOVEL     '5E'          COD
013700000804     C                   MOVEL     *BLANKS       KEY
013800000804     C                   MOVEL     TAMTSP        KEY
013900000804     C                   CLEAR                   DS5E
014000000804     C     KTAB          CHAIN     TABEL00F
014100000804     C                   IF        %FOUND(TABEL00F)
014200000804     C                   MOVEL     TBLUNI        DS5E
014300000804     C                   ENDIF
014400000828     C* se è una tariffa particolare  solo poste e la tariffa non lo è leggo la successiva
014500000804     C                   if        §1pfpt = 'P' and §5euso <> 'P'
014600000804     c                   iter
014700000804     c                   endif
014800000828     C* se è una tariffa particolare non  poste e la tariffa è Posta  leggo la successiva
014900000804     C                   if        §1pfpt = 'N' and §5euso  = 'P'
015000000804     c                   iter
015100000804     c                   endif
015200000804     c*
015300000804     c                   endif
015400000804     C**
015500000828     C* Controllo se è un cliente gestito dalla filiale
015600980323     C*
015700980323     C* 15 ON  - SONO IN FILIALE
015800000801    1C                   IF        *in15
015900980323     C*
016000990611     C* MODIFICO LA TARIFFA MA NON LA SFLAGGO SE NON E' UN CLIENTE DELLA FILIALE
016100980323     C     W0030         LOOKUP    L1                                     35
016200980323     C                   SETOFF                                           90
016300980323     C*
016400980323   X1C                   ELSE
016500980323     C* 15 OFF - SONO IN SEDE
016600980323     C*
016700990611     C* MODIFICO LA TARIFFA MA NON LA SFLAGGO
016800000802     c                   SETOFF                                           35
016900000802     c                   SETOFF                                           90
017000000802    1c                   ENDIF
017100000804     c* verifco divisa tariffa
017200000804     c                   movel     tamflo        dsta01
017300000804     c* se divisa = a blank metto ITL
017400000804     c                   if        §tadiv  = *blanks
017500000804     c                   eval      §tadiv = 'ITL'
017600000804     c                   endif
017700000804     C*
017800000804     C* CONTROLLO SE E' DIVISA CON DECIMALI O SENZA
017900000804     C                   MOVEL     'CV'          COD
018000000804     C                   MOVEL     *BLANKS       KEY
018100000804     C                   MOVEL     §TADIV        KEY
018200000804     C                   CLEAR                   DSCV
018300000804     C     KTAB          CHAIN     TABEL00F
018400000804     C                   IF        %FOUND(TABEL00F)
018500000804     C                   MOVEL     TBLUNI        DSCV
018600000804     C                   ENDIF
018700000804     c* se tariffa con decimali accendo il 12
018800000804     c                   eval      *in12=(§cvfdc = 'S')
018900000804     c*
019000000803     c                   clear                   prima
019100000802     c* CONTROLLO : SE IL CLIENTE HA GIA' LA TARIFFA PARTICOLARE E NON ANNULLATA
019200000802     c     KTPT          CHAIN     TITPT01L                           30
019300000801    1C* se esiste e non è annullata leggo la tariffa successiva
019400000801     C                   if        %found(titpt01l) and tptatb = ' '
019500000801     C                   iter
019600000801    1C                   endif
019700000801     C* SE E' ANNULLATA deleto tpt e tpd e riscrivo
019800000920     C                   if        %found(titpt01l) and tptatb = 'A'
019900000803     c                   eval      prima = 'ANN'
020000000802     c* SE TPD ANNULLATO CANCELLO TUTTO E RISCRIVO
020100000802     c                   EXSR      DELTPD
020200000802     c                   ENDIF
020300000801     c* aggancio la tariffa di cartello e scrivo
020400000802     C                   EXSR      VALTPD
020500000802     c*
020600000802     c  N90
020700000802     cAN OF              EXCEPT    TESTA
020800000901     c   35
020900000901     canN90              EXCEPT    RIGA
021000000901     c   35
021100000901     canN90              SETON                                        90
021200000802     c*
021300000802    0c                   ENDDO
021400000802     c*
021500000804     C     FINE          TAG
021600000802     c                   SETON                                        LR
021700000802     c*-----------------------------------------------------------------------**
021800000802     c* VALORIZZO I CAMPI DELLE TARIFFE PARTICOLARE
021900000802     c*-----------------------------------------------------------------------**
022000000802     C     VALTPD        BEGSR
022100000802     c*
022200000802     c                   clear                   wtrtpd
022300000919     c* recupero il progressivo da utilizzare in bese alle date decorrenza
022400000919     c* se data decorrenza minore o uguale a 20001031 = 005
022500000919     c* se data decorrenza maggiore di 20001031 e minore e uguale 20001130 = 006
022600000920     c* se data decorrenza maggiore o uguale a  20001201 = 007  per la BARTOLINI
022700000920     c* se data decorrenza maggiore o uguale a  20001201 = 006  per la SDI
022800000919     c*
022900000919     c                   if        tamddt <= 20001031
023000000919     c                   z-add     5             keyprg
023100000919     c                   endif
023200000919     c                   if        tamddt > 20001031 and tamddt <= 20001130
023300000919     c                   z-add     6             keyprg
023400000919     c                   endif
023500000920     c                   if        tamddt >= 20001201  and pardit = 'BAR'
023600000919     c                   z-add     7             keyprg
023700000919     c                   endif
023800000920     c                   if        tamddt >= 20001201  and pardit = 'SDI'
023900000920     c                   z-add     6             keyprg
024000000920     c                   endif
024100000801     c*
024200000802     c* aggancio la cartello e verifico se esiste un dettaglio tariffario che fa al mio caso
024300000802     c*
024400000802     c     ktptca        setll     titpdcar
024500000920    1c                   do        *hival
024600000802     c*
024700000802     c     ktptca        reade     titpdcar
024800000802     c*
024900000920    2c                   if        %eof(titpdcar)
025000000802     c                   leave
025100000920    2c                   endif
025200000802     c*
025300000802     c* verifico se tipo tariffa cliente corrisponde a codice tassazione tarifa
025400000802     c*
025500000802     c                   exsr      decorp
025600000802     c* se tariffa italia inserisco solo cod.tassazione italia e viceversa
025700000802     c*
025800000920    2c                   if        tamfie = 'E' and §ctest <> 'E'  or
025900000802     c                             tamfie = 'I' and §ctest = 'E'
026000000802     c                   iter
026100000920    2c                   endif
026200000802     c*
026300000802     c* valorizzo la chiave
026400000802     c                   eval      tpdksc = tamksc
026500000802     c                   eval      tpdctr = tamctr
026600000802     c                   eval      tpdprg = tamprg
026700000802     c*
026800000802     C* METTO IL FLAG DI TRASMISSIONE DELLA TESTATA
026900000920    2c                   if        not *in35
027000000802     c                   eval      tpdftr = tamftr
027100000802     c                   eval      tpddtr = tamdtr
027200000802     c                   else
027300000802     c                   clear                   tpdftr
027400000802     c                   clear                   tpddtr
027500000920    2c                   endif
027600000804     c* verifico se le  valute delle due tariffe sono diverse
027700000920    2c                   if        valcar <> §tadiv
027800000804     C* devo convertire in moneta della tariffa che sto modificando
027900000804     c* aggancio la testata di cartello per recupero del tipo pagamento
028000000804     c     ktptca        chain     titptcar
028100000804     C* TARIFFA A VALORE
028200000920    3C     TPTTPG        IFEQ      '4'
028300000804     C     TPTTPG        OREQ      'T'
028400000804     C     TPTTPG        OREQ      'V'
028500000804     C* SE TARIFFA A VALORE CONVERTO SOLO LO SCAGLIONE
028600000804     C**  SCAGLIONE
028700000804     C** SE E' SCAGLIONE 99999 NON CONVERTO
028800000804     C                   Z-ADD     TPDSGL        W0050             5 0
028900000920    4C     W0050         IFNE      99999
029000000804     C                   Z-ADD     TPDSGL        COMODO
029100000804     C                   EXSR      CNVEUR
029200000804     C                   Z-ADD     COMODO        TPDSGL
029300000920    4C                   ENDIF
029400000804     C* SE TARIFFA NON A VALORE CONVERTO LA TARIFFA
029500000920   X3C                   ELSE
029600000804     C* TARIFFA
029700000804     C                   Z-ADD     TPDITR        COMODO
029800000804     C                   EXSR      CNVEUR
029900000804     C                   Z-ADD     COMODO        TPDITR
030000000919     C*
030100000920    3C                   ENDIF
030200000828     C* MINIMO
030300000920    3C                   IF        TPDMIN > 0
030400000828     C                   Z-ADD     TPDMIN        COMODO
030500000828     C                   EXSR      CNVEUR
030600000828     C                   Z-ADD     COMODO        TPDMIN
030700000920    3C                   ENDIF
030800000828     C* MASSIMO
030900000920    3C                   IF        TPDMAX > 0
031000000828     C                   Z-ADD     TPDMAX        COMODO
031100000828     C                   EXSR      CNVEUR
031200000828     C                   Z-ADD     COMODO        TPDMAX
031300000920    3C                   ENDIF
031400000804     C*
031500000920    2C                   ENDIF
031600000802     c*
031700000804     c                   write     titpd000
031800000802     c*
031900000802     c* se scrivo il dettaglio segnalo per scrivere la testata
032000000802     c                   eval      wtrtpd = 'S'
032100000802     c*
032200000920    1c                   enddo
032300000802     c*
032400000802     c* se ho scritto il dettaglio aggancio la testat di cartello e la scrivo
032500000920     c* NON VA BENE IN QUANTO SE CI SONO NELLA CARTELLO DELLE TARIFFE NEGATE
032600000920     C* CIOE' SENZA DETTAGLIO DEVO NEGARLE ANCHE QUI
032700000920    1c*******            if        wtrtpd = 'S'
032800000802     c     ktptca        chain     titptcar
032900000802     c*
033000000920    2c                   if        %found(titptcar)
033100000802     c*
033200000802     c                   MOVE      TAMKSC        TPTKSC
033300000802     c                   MOVE      TAMCTR        TPTCTR
033400000802     c                   MOVE      TAMPRG        TPTPRG
033500000802     C* METTO IL FLAG DI TRASMISSIONE DELLA TESTATA
033600000920    3c                   if        not *in35
033700000802     c                   eval      tptftr = tamftr
033800000802     c                   eval      tptdtr = tamdtr
033900000802     c                   else
034000000802     c                   clear                   tptftr
034100000802     c                   clear                   tptdtr
034200000920    3c                   endif
034300000920    3c                   if        valcar <> §tadiv
034400000804     C* devo convertire in moneta della tariffa che sto modificando
034500000804    4C     TPTTPG        IFEQ      '4'
034600000804     C     TPTTPG        OREQ      'T'
034700000804     C     TPTTPG        OREQ      'V'
034800000804     C* AZZERO GLI ARROTONDAMENTI
034900000804     C                   Z-ADD     0             TPTARL
035000000804     C                   Z-ADD     0             TPTARF
035100000804     C                   Z-ADD     0             TPTARO
035200000804     C*
035300000804    4C                   ENDIF
035400000804     C* CONVERTO NELLA NUOVA MONETA
035500000804     C**  IMPORTO VALORE MERCE
035600000804     C                   Z-ADD     TPTVLM        COMODO
035700000804     C                   EXSR      CNVEUR
035800000804     C                   Z-ADD     COMODO        TPTVLM
035900000804     C*
036000000920    3C                   ENDIF
036100000901     c* valorizzo come data ultima variazione quella testata
036200000901     c                   eval      tptduv = tamduv
036300000802     c*
036400000828     c                   write     titpt000
036500000802     c*
036600000920    2c                   endif
036700000802     c*
036800000920    1c****               endif
036900000802     c*
037000000802     c                   ENDSR
037100980323     C*------------------------------------------------------------------------*
037200980323     C* RECUPERO ORDINAMENTO PROVINCIA DALLA TABELLA CODICE TASSAZIONE
037300980323     C*------------------------------------------------------------------------*
037400980323     C     DECORP        BEGSR
037500980323     C*
037600980323     C                   MOVEL     'CT'          COD
037700980323     C                   MOVEL     *BLANKS       KEY
037800000731     C                   MOVEL     tpdcts        KEY
037900000804     C                   CLEAR                   DSCT
038000000804     C     KTAB          CHAIN     TABEL00F
038100000804     C                   IF        %FOUND(TABEL00F)
038200980323     C                   MOVEL     TBLUNI        DSCT
038300000804     C                   ENDIF
038400980323     C*
038500980323     C                   ENDSR
038600990923     C*------------------------------------------------------------------------*
038700990923     C* PULIZIA DEL DETTAGLIO TARIFFE
038800990923     C*------------------------------------------------------------------------*
038900990923     C     DELTPD        BEGSR
039000990924     C*
039100000828     C                   DELETE    TITPT000
039200990923     C*
039300990924     C* VERIFICO SE ESISTE IL DETTAGLIO CHE VOGLIO SCRIVERE
039400000802     c     ktpt          setll     titpd01l
039500000802     c                   do        *hival
039600000802     C     KTPT          reade     TITPD01L
039700000802     c                   if        %eof(titpd01l)
039800000802     c                   leave
039900000802     c                   endif
040000000802     c*
040100000804     C                   DELETE    TITPD000
040200000802     c*
040300000802     c                   enddo
040400990923     C*
040500990923     C                   ENDSR
040600990611     C*
040700980323     C*--- CARICAMENTO TABELLE ---------------------------------------*
040800980323     C     CARTAB        BEGSR
040900980323     C*
041000980323     C****  CARICO TABELLA FILIALI GESTITE £1  ****
041100980323     C                   CLEAR                   DSUL06
041200980323     C                   MOVE      '£1'          D06COD
041300980323     C                   MOVEL     SIMFEL        D06KEY
041400980323     C                   MOVEL     DSUL06        KPJBU
041500980323     C                   CALL      'TRUL06R'
041600980323     C                   PARM                    KPJBA
041700980323     C                   MOVEL     KPJBU         DSUL06
041800980323     C                   MOVEA     LIN           L1
041900980323     C*
042000980323     C                   ENDSR
042100000804     C*-------------------------------------------------------------------*
042200000804     C* CONVERSIONE IN MONETA ED EVENTUALE ARROTONDAMENTO
042300000804     C*-------------------------------------------------------------------*
042400000804     C*
042500000804     C     CNVEUR        BEGSR
042600000804     C*
042700000804     c                   if        comodo > 0
042800000804     C                   CLEAR                   YEUR
042900000828     C                   MOVEL     VALCAR        YECDVI
043000000828     C                   MOVEL     §TADIV        YECDVO
043100000804     C                   Z-ADD     COMODO        YECIMI
043200000804     C   12              MOVE      '3'           YECDCO
043300000804     C  N12              MOVE      '0'           YECDCO
043400000804     C*
043500000804     C                   CALL      'YEURCO'
043600000804     C                   PARM                    YEUR
043700000804     C*
043800000804    5C     YECESI        IFEQ      ' '
043900000804     C                   Z-ADD     YECIMO        COMODO           15 3
044000000804     C                   ELSE
044100000804     C                   Z-ADD     *ZEROS        COMODO
044200000804    5C                   END
044300000804     C*
044400000804    4C                   ENDIF
044500000804     C*
044600000804     C                   ENDSR
044700000731      *---------------------------------------------------------------------------------------------
044800000731      *        routine iniziale
044900000731      *---------------------------------------------------------------------------------------------
045000000731     c     *inzsr        begsr
045100000731      *
045200000731     c     *entry        plist
045300000803     c                   parm                    parksc                         * codice cliente
045400000731     c                   parm                    parctr                         * codice tariffa
045500000731     c                   parm                    parprg                         * progr. tariffa
045600000731     c                   parm                    parftc                         * cod.tar.particolar
045700000731     c                   parm                    parfie                         * flag Italia/Estero
045800000901     c                   parm                    paroff                         * flag tariffa/Offer
045900000919     c                   parm                    pardit                         * ditta di elaboraz
046000000731      *
046100000803     c                   move      parksc        keyksc
046200000803     c                   move      parctr        keyctr
046300000803     c                   move      parprg        keyprg
046400000803     c                   move      parftc        keyftc
046500000803     c                   move      parfie        keyfie
046600000803      *
046700000731     C                   Z-ADD     1             CODUT             1 0
046800000731     C                   CALL      'X§PARUT'
046900000731     C                   PARM                    UT§DSE
047000000731     C                   MOVEL     REC80         CNCR80
047100000731     C* ACCESSO TABEL00F
047200000731     C     KTAB          KLIST
047300000731     C                   KFLD                    CODUT
047400000731     C                   KFLD                    COD
047500000731     C                   KFLD                    KEY
047600000731     C*
047700000801     C* ACCESSO TITPTCAR    CARTELLO
047800000801     C     KTPTCA        KLIST
047900000803     C                   KFLD                    keyKSC
048000000803     C                   KFLD                    keyCTR
048100000803     C                   KFLD                    keyPRG
048200000803     C                   KFLD                    keyFTC
048300000804     C* ACCESSO TNTAM01l    CARTELLO
048400000804     C     KTAMCA        KLIST
048500000804     C                   KFLD                    keyKSC
048600000804     C                   KFLD                    keyCTR
048700000804     C                   KFLD                    keyPRG
048800000801     C*
048900000801     C* ACCESSO TITPT01l    Cliente
049000000801     C     KTPT          KLIST
049100000801     C                   KFLD                    tamKSC
049200000801     C                   KFLD                    tamCTR
049300000801     C                   KFLD                    tamPRG
049400000801     C                   KFLD                    parFTC
049500000801     c*
049600000801     C* SOLO SE SONO IN FILIALE CARICO TABELLA L1
049700000801    1C     SIMFEL        IFGT      0
049800000801     C                   SETON                                        15
049900000801     C                   EXSR      CARTAB
050000000801    1C                   END
050100000804     c*
050200000804     C* VERIFICO SE TARIFFA PARTICOLARE UTILIZZATA ANCHE DALLE POSTE
050300000804     C                   MOVEL     '1P'          COD
050400000804     C                   MOVEL     *BLANKS       KEY
050500000804     C                   MOVEL     PARFTC        KEY
050600000804     C     KTAB          CHAIN     TABEL00F
050700000804     C                   IF        %FOUND(TABEL00F)
050800000804     C                   MOVEL     TBLUNI        DS1P
050900000804     C                   ELSE
051000000804     C                   GOTO      FINE
051100000804     C                   ENDIF
051200000804     C* IL FLAG §1PFPT SE UGUALE A "P" SOLO PER POSTE
051300000804     C*                            "N" NON E' ABILITATA PER TARIFFE POSTE
051400000804     C*                            " " PER TUTTE LE TARIFFE
051500000804     c* aggancio la tariffa di cartello per recuperare la moneta
051600000901     c     ktamca        chain     tntamcar
051700000901     c                   if        %found(tntamcar)
051800000804     c                   movel     tamflo        dsta01
051900000804     c                   eval      valcar =  §tadiv
052000000804     c                   endif
052100000804     c* se divisa = a blank metto ITL
052200000804     c                   if        valcar  = *blanks
052300000804     c                   eval      valcar = 'ITL'
052400000804     c                   endif
052500000801     C*
052600000801     C                   EXCEPT    TESTA
052700000731     c*
052800000731     c                   endsr
052900000731
053000980323     OQSYSPRT   E            TESTA            02
053100980323     O                       ST(1)               66
053200980323     O                       ST(1)              132
053300980323     O          E            TESTA            03
053400980323     O                       ST(2)               66
053500980323     O                       ST(3)              132
053600000920     O                       PARFTC              50
053700980323     O          E            TESTA            04
053800980323     O                       ST(1)               66
053900980323     O                       ST(1)              132
054000980323     O          E            RIGA        2  0
054100980323     O                       TAMKSC        Z      7
054200980323     O                       TAMCTR        Z     12
054300980323     O                                           13 '/'
054400990611     O                       TAMPRG        Z     18
054500990611     O                       TAMFIE              25
054600990928     O                       PRIMA               30
054700990928     O                       TPTFTR              35
054800990611**
054900980323------------------------------------------------------------------
055000990928Cliente  Tariffa    I/E  ANNU  TRASMISSIONE
055100990611
