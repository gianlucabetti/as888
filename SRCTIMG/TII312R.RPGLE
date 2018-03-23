000100990202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990202     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300871217     H* OCO322R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500890927     H*          - STAMPA BOLLA MANUALE
000600871217     H*                                                      AZ.O322 *
000700990202     H*         FILE : ancln01l - anind01l - FNSPE01L -              *
000800021029     H*                FIVABWKF - tmclp01L - ansog01l                *
000900000000     H*--------------------------------------------------------------*
001000990202     Fancln01l  IF   E           K DISK
001100990202     Fanind01l  IF   E           K DISK
001200990202     Fansog01l  IF   E           K DISK
001300990202     Ftmclp01L  IF   E           K DISK
001400890711     FANTAB00F  IF   E           K DISK
001500980112     FAZORG01L  IF   E           K DISK
001600990317     FTII312D   CF   E             WORKSTN
001700871217     F                                     SFILE(OCO322S1:NRR)
001800021029     FFIVABWKF  O  A E           K DISK    USROPN
001900871224     FBOLLEPRT  O    F  132        PRINTER OFLIND(*INOF)
002000871224     F                                     USROPN
002100920911     D TAB$            S              1    DIM(10) CTDATA PERRCD(1)
002200920911     D TAB£            S              1    DIM(10) ALT(TAB$)
002300920911     D $I              S              1    DIM(15)
002400920911     D $O              S              1    DIM(17)
002500920925     D ST              S             25    DIM(2) CTDATA PERRCD(1)
002600921005     D*--------------------------------------------------------------*
002700020925     D GAI             S             10    DIM(2) CTDATA PERRCD(1)
002800921005     D*--------------------------------------------------------------*
002900871217     D OCLBL1          DS
003000871217     D*--- DATA AREA N.BOLLA /DATA BOLLA :
003100871217     D  NUMBOL                 1      6  0
003200910102     D  DATABL                 7     12  0
003300890711     D STMDS           DS
003400890711     D  STMFLG                41     41
003500101014     d
003600101014     D og148         E DS
003601150213     D og143         E DS
003700101014     d
003800101014     D KPJBA         E DS
003900990202     D soc001        E DS                  EXTNAME(xsoc001ds)
004000990202     D xsocds          DS          1000
004100990202     d dataiso         s               d   datfmt(*iso)
004200990202     d dataeur         s               d   datfmt(*eur)
004300871217     I*---- PASSAGGIO PARAMETRI
004400871217     C*----------------------------------------------------*
004500871217     C     *ENTRY        PLIST                                                  *
004600871217     C                   PARM                    KPJBA                          *
004700990202     C*---------- RICERCA DITTA :
004800990202     C                   MOVEL     'SOC001'      TIPXSC
004900990202     C                   MOVEL     *blank        SOCXSC
005000990202     C                   EXSR      REPSOC
005100990202     C     RTNXSC        IFNE      '1'
005200990202     C                   MOVEL     XSOCDS        SOC001
005300990202     C                   MOVEL     xscrgs        rsut
005400990202     C                   MOVEL     xscsoc        wsoc              3
005500990202     c                   movel     xscvia        simvia           30
005600990202     c                   movel     xsccit        simcit           30
005700990202     c                   end
005800990202     c                   time                    como14           14 0
005900990202     c                   move      como14        woggi             8 0
005901101021     c     *eur          movel     woggi         dataiso
005902101021     c                   movel     dataiso       DATEU             8 0
006000920127     C*
006100921005     C* RICERCO SULLA TABELLA DELLA STAMPA PARTITA IVA
006200921005     C* E DELLA STAMPA LEGENDA CONVERSOINE
006300921005     C*
006400921005     C                   MOVE      '900'         TABGRU
006500921005     C                   MOVE      '9999999'     TABCOD
006600921005     C     KEYANT        CHAIN     ANTAB00F                           41
006700921005     C  N41              MOVEL     TABDES        STSINO            2
006800921005     C  N41              MOVEL     STSINO        STPIVA            1
006900921005     C  N41              MOVE      STSINO        STLEGE            1
007000921005     C   41              MOVEL     *BLANKS       STSINO
007100921005     C     *IN41         IFEQ      '1'
007200921005     C                   SETOFF                                       9394
007300921005     C                   END
007400921005     C     STPIVA        COMP      'S'                                    93
007500921005     C     STLEGE        COMP      'S'                                    94
007600921005     C*----------------------------------------------------*
007700921005     C* CONTROLLO SE IL SISTEMA INFORMATIVO  E' RIFERITO A *
007800921005     C* GAITRA201 (BARTOLINI)                              *
007900921005     C     KNSIF         IFEQ      GAI(1)                                       *
008000021029     C                   OPEN      FIVABWKF                                     *
008100921005     C                   Z-ADD     1             FLGAI             1 0
008200921005     C                   END                                                    *
008300871217     C*--- RICERCA ESISTENZA CONTI SUL P.D.C.              *
008400990202     C     KEYcln        KLIST                                                  *
008500990202     C                   KFLD                    wsoc                           *
008600990202     C                   KFLD                    vidscc                         *
008700890711     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
008800890711     C     KEYANT        KLIST                                                  *
008900890711     C                   KFLD                    TABGRU                         *
009000890711     C                   KFLD                    TABCOD                         *
009100871217     C*----------------------------------------------------*
009200871217     C     INIZIO        TAG
009300871217     C*          -------------
009400990202     C                   movel     '000151'      VIDCCC
009500990202     C                   movel     *blank        VIDSCC
009600990202     C                   Z-ADD     woggi         VIDDEM
009700871221     C                   EXFMT     OCO322D1
009800990412     C   Kc              GOTO      FINE
009900990202     C     VIDCCC        CABEQ     *blank        INIZIO
010000990202     C     VIDSCC        CABEQ     *blank        INIZIO
010100871218     C     VIDDEM        IFEQ      0
010200871218     C                   SETON                                        50
010300871218     C                   GOTO      INIZIO
010400871218     C                   END
010500890711     C                   SETOFF                                       72
010600990202     C     vidscc        CHAIN     tmclp000                           71
010700890711     C                   MOVE      '160'         TABGRU                         *
010800890711     C                   MOVE      *ZEROS        TABCOD                         *
010900890711     C                   MOVE      CLPSTM        TABCOD                         *
011000890711     C     KEYANT        CHAIN     ANTAB00F                           74        *
011100890711     C                   MOVEL     *BLANKS       STMDS
011200890711     C  N74              MOVEL     TABDES        STMDS
011300890711     C** NON E' POSSIBILE STAMPA BOLLA MANUALE SE SETTORE MERC.
011400980113     C** CLIENTE <> BLANKS
011500980113     C     STMFLG        COMP      ' '                                7373
011600890711     C   73              GOTO      INIZIO
011700871218     C*PRELEVO IL NUMERO BOLLA DALLA DATA AREA OCLBL1
011800871218     C     *DTAARA       DEFINE                  OCLBL1                         *
011900871218     C     SUDT1         TAG
012000871218     C     *LOCK         IN        OCLBL1                               46
012100871218     C   46              GOTO      SUDT1                                        *
012200990202     C                   move      woggi         DATAeur
012300990202     C                   move      dataeur       dataiso
012400990202     C                   move      DATAIso       RUDATE            8 0
012500990202     C                   move      VIDDEM        DATAeur
012600990202     C                   move      dataeur       dataiso
012700990202     C                   move      DATAIso       datain            8 0
012800990202     c     *ymd          move      databl        dataiso
012900990202     c                   move      dataiso       databl8           8 0
013000990202     C     DATAIN        IFLT      DATABL8
013100871218     C                   SETON                                        51
013200871218     C                   GOTO      INIZIO
013300871218     C                   END
013400871222     C** DATA EMISSIONE DEVE SSERE MAG.UG DATA ULTIMA BOLLA
013500871222     C     DATAIN        IFGT      RUDATE
013600871222     C                   SETON                                        51
013700871222     C   51              GOTO      INIZIO
013800871222     C                   END
013900871222     C** DATA EMISSIONE NON PUO' ESSERE MAGGIORE UDATE
014000871217     C*DECODIFICA DEL CLIENTE O DEL FORNITORE
014100990202     C     KEYcln        CHAIN     ancln000                           02
014200990202     C     clnsogg       CHAIN     ansog000                           02
014300990202     C     clnsogg       CHAIN     aNIND000                           03
014400871217     C                   MOVEL     *BLANKS       VIDRAG
014500871217     C                   MOVEL     *BLANKS       VIDRA1
014600871217     C                   MOVEL     *BLANKS       VIDIND
014700871217     C                   Z-ADD     0             VIDCAP
014800871217     C                   MOVEL     *BLANKS       VIDCIT
014900871217     C                   MOVEL     *BLANKS       VIDPRV
015000871217     C                   MOVEL     *BLANKS       RAGLUO
015100871217     C                   MOVEL     *BLANKS       RAGLU1
015200871217     C                   MOVEL     *BLANKS       INDLUO
015300871217     C                   MOVEL     *BLANKS       CITLUO
015400871217     C                   Z-ADD     0             CAPLUO
015500871217     C                   MOVEL     *BLANKS       PRVLUO
015600871217     C  N02
015700990202     CANN03              MOVEL     sogdes        VIDRAG
015800990202     C                   MOVE      sogdes        VIDRA1
015900990202     C                   MOVEL     sogdes        RAGLUO
016000990202     C                   MOVE      sogdes        RAGLU1
016100990202     C                   MOVEL     INDindriz     VIDIND
016200990202     C                   MOVEL     INDindriz     INDLUO
016300990202     C                   MOVEL     INDlocalit    VIDCIT
016400990202     C                   MOVEL     INDlocalit    CITLUO
016500990202     C                   movel     INDCAP        VIDCAP
016600990202     C                   movel     INDCAP        CAPLUO
016700990202     C                   MOVEL     INDPRov       VIDPRV
016800990202     C                   MOVEL     INDPRov       PRVLUO
016900871217     C*PULIZIA SUBFILE
017000871217     C                   EXSR      PULIZ
017100871217     C*CARICAMENTO SUBFILE
017200871217     C                   EXSR      CARIC
017300871217     C                   Z-ADD     NUMBOL        BOLNUO            6 0
017400871217     C                   ADD       1             BOLNUO
017500871217     C                   Z-ADD     BOLNUO        VIDBOL
017600921103     C                   WRITE     OCO322D3
017700871217     C     STAR          TAG
017800871217     C                   EXFMT     OCO322SC
017900990202     C  NKl              DO
018000921103     C     RAGLUO        IFEQ      *BLANKS
018100921103     C                   SETON                                        64
018200921103     C                   GOTO      STAR
018300921103     C                   END
018400921103     C     INDLUO        IFEQ      *BLANKS
018500921103     C                   SETON                                        65
018600921103     C                   GOTO      STAR
018700921103     C                   END
018800921103     C     CITLUO        IFEQ      *BLANKS
018900921103     C                   SETON                                        66
019000921103     C                   GOTO      STAR
019100921103     C                   END
019200921103     C     CAPLUO        IFEQ      *ZEROS
019300921103     C                   SETON                                        67
019400921103     C                   GOTO      STAR
019500921103     C                   END
019600921103     C     PRVLUO        IFEQ      *BLANKS
019700921103     C                   SETON                                        68
019800921103     C                   GOTO      STAR
019900921103     C                   END
020000921103     C                   END
020100990202     C   Kl              GOTO      INIZIO
020200921005     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
020300921005     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
020400921005     C*
020500921005     C*
020600921102     C   KA              DO
020700921102     C                   EXSR      STAMPA
020800990317     C                   CALL      'TII200R'
020900910102     C                   PARM                    KPJBA
021000921102     C     FLGAI         IFEQ      1
021100921102     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
021200921102     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
021300921102     C*
021400020111     C*    CAPLUO        ANDNE     40138
021500921102     C                   EXSR      SPEBOL
021600990202     C  NKlVD4SEG        IFEQ      'S'
021700941125     C                   CALL      'TNSD50C'
021800921102     C                   PARM                    KPJBA
021900921102     C                   END
022000921102     C                   END
022100921102     C                   GOTO      FINE
022200921102     C                   END
022300871217     C  NKA              GOTO      STAR
022400871217     C     FINE          TAG
022500871217     C                   SETON                                        LR
022600921005     C******************************************************
022700921005     C     SPEBOL        BEGSR
022800921005     C**----------------------
022900921005     C** RICHIESTA DI SPEDIZIONE TRAMITE LA FILIALE DI BOLOGNA
023000921103     C                   SETOFF                                       606163
023100921103     C                   MOVEL     'S'           VD4BOL
023200921103     C                   MOVEL     'N'           VD4SEG
023300921005     C*
023400921005     C     SU            TAG
023500921005     C                   EXFMT     OCO322D4
023600921005     C*
023700990202     C  NKl              DO
023800921005     C*
023900921005     C  N60VD4BOL        IFEQ      'S'
024000921005     C                   SETON                                        60
024100921005     C                   GOTO      SU
024200921005     C*
024300921005     C                   END
024400921005     C*
024500921102     C   60
024600921102     CANN61VD4SEG        IFEQ      'S'
024700921005     C                   SETON                                        61
024800921102     C                   GOTO      SU
024900921005     C                   END
025000921005     C*
025100921005     C*
025200921102     C   60              EXSR      CARFBL
025300960118     C   60
025400960118     CAN 61
025500960118     CANN63              Z-ADD     VIDBOL        COM15            15 0
025600960118     C   60
025700960118     CAN 61
025800960118     CANN63              MOVEL     COM15         KPJBU
025900921005     C*
026000921005     C                   END
026100921005     C*
026200921005     C                   ENDSR
026300871217     C******************************************************
026400871217     C     PULIZ         BEGSR
026500871217     C**----------------------
026600871217     C** PULIZIA DEL SUBFILE
026700871217     C**----------------------
026800871217     C                   SETON                                        70
026900871217     C                   WRITE     OCO322SC
027000871217     C                   SETOFF                                       70
027100871217     C                   ENDSR
027200871217     C******************************************************
027300871217     C     CARIC         BEGSR
027400871217     C**----------------------
027500871217     C** CARICAMENTO SUBFILE
027600871217     C** CARICO RECORDS VUOTI
027700871217     C**----------------------
027800871217     C                   Z-ADD     0             NRR               2 0
027900871217     C     LET           TAG
028000871224     C                   DO        32
028100871217     C                   MOVEL     *BLANKS       VIDMAT
028200871217     C                   MOVEL     *BLANKS       VIDDMT
028300871217     C                   Z-ADD     0             VIDQTA
028400871217     C                   ADD       1             NRR
028500871217     C                   WRITE     OCO322S1
028600871217     C                   END
028700871217     C                   ENDSR
028800871217     C******************************************************
028900871217     C     STAMPA        BEGSR
029000871217     C**----------------------
029100871217     C** STAMPA BOLLA
029200871217     C**----------------------
029300871217     C                   Z-ADD     BOLNUO        NUMBOL
029400990202     C                   move      VIDDEM        DATAeur
029500990202     C                   move      dataeur       dataiso
029600990202     C                   move      DATAIso       DATABL8
029700871217     C                   OUT       OCLBL1
029800871228     C                   OPEN      BOLLEPRT
029900871217     C                   EXCEPT    TESTA
030000871217     C   OF              DO
030100871217     C                   EXCEPT    FONDO
030200871217     C                   EXCEPT    TESTA
030300871217     C                   END
030400871217     C                   EXCEPT    DETT
030500871224     C                   DO        32            A                 2 0
030600871217     C     A             CHAIN     OCO322S1                           05
030700871217     C   05              GOTO      SOTTO
030800920911     C*
030900920911     C* PRIMA DI STAMPARE LA QTA FACCIO LA CONVERSIONE IN LETTERE
031000920911     C* A.G. 11-9-92
031100920916     C                   MOVE      *BLANKS       STQTAL
031200920916     C     VIDQTA        IFNE      *ZEROS
031300920911     C                   MOVE      0             £CI              15 0
031400920911     C                   MOVE      VIDQTA        £CI
031500920911     C                   Z-ADD     2             $DC               2 0
031600920911     C***
031700920911     C                   EXSR      NUMALF
031800920911     C***
031900920911     C                   MOVE      $OUT17        STQTAL           11
032000920916     C                   END
032100920911     C** FINE VARAIAZIONE
032200871217     C                   EXCEPT    DETAIL
032300871217     C                   END
032400871217     C     SOTTO         TAG
032500871217     C                   EXCEPT    FONDO
032600871217     C     END2          ENDSR
032700871217     C***------------------------------------------------***
032800920911     C* ROUTINE CONVERSIONE NUMERI IN LETTERE ALFABETICHE
032900920911     C     NUMALF        BEGSR
033000920911     C*          ---------------
033100920911     C* PULISCO IL CAMPO FINALE
033200920911     C                   MOVE      *BLANKS       $O
033300920911     C* SE NUMERO NEGATIVO METTO SEGNO NELL'ULTIMO POSTO
033400920911     C     £CI           IFLT      0
033500920911     C                   MOVE      '-'           $O(17)
033600920911     C* LO RENDO POSISTIVO SE NO NON STAMPA L'ULTIMA LETTERA
033700920911     C     £CI           MULT      -1            £CI
033800920911     C                   ELSE
033900920911     C                   MOVE      ' '           $O(17)
034000920911     C                   END
034100920911     C* SI METTE IL CAMPO £CI NELLA SCHIERA DI INPUT $I
034200920911     C                   MOVE      £CI           £CI2             15
034300920911     C                   MOVEA     £CI2          $I
034400920911     C*
034500920911     C                   MOVE      0             £VIRG             1 0
034600920911     C                   MOVE      0             £VALID            1 0
034700920911     C* DETERMINO IN QUALE POSTO DEVO METTERE LA VIRGOLA
034800920911     C     15            SUB       $DC           £DE               2 0
034900920911     C     £DE           COMP      15                                     OD
035000920911     C* SE OD ACCESO NON DEVO STAMPARE LA VIRGOLA NELLA SCHIERA
035100920911     C* DI OUTPUT
035200920911     C  NOD              Z-ADD     1             J£                2 0
035300920911     C   OD              Z-ADD     2             J£
035400920911     C*
035500920911     C     1             DO        15            I£                2 0
035600920911     C     $I(I£)        COMP      '0'                                    OC
035700920911     C   OC£VALID        COMP      0                                      OC
035800920911     C   OC£VIRG         COMP      0                                      OC
035900920911     C   OC              MOVE      ' '           $O(J£)
036000920911     C*
036100920911     C  NOC              MOVE      1             £VALID
036200920911     C  NOC$I(I£)        LOOKUP    TAB$          TAB£                     OB
036300920911     C  NOC
036400920911     CAN OB              MOVE      TAB£          $O(J£)
036500920911     C  NOC
036600920911     CANNOB              MOVE      ' '           $O(J£)
036700920911     C* CONTROLLO SE DEVO STAMPARE LA VIRGOLA
036800920911     C  NODJ£            IFEQ      £DE
036900920911     C* CONTROLLO SE DEVO METTERE UNO ZERO PRIME DELLA VIRGOLA
037000920911     C     £VALID        IFEQ      0
037100920911     C     '0'           LOOKUP    TAB$          TAB£                     OB
037200920911     C   OB              MOVE      TAB£          $O(J£)
037300920911     C                   END
037400920911     C*
037500920911     C                   ADD       1             J£
037600920911     C                   MOVE      ','           $O(J£)
037700920911     C                   MOVE      1             £VIRG
037800920911     C                   END
037900920911     C*
038000920911     C                   ADD       1             J£
038100920911     C*
038200920911     C                   END
038300920911     C* ESEGUITA LA ROUTINE IL CAMPO $O CONTIENE LE LETTERE DA
038400920911     C* STAMPARE CON LA VIRGOLA O MENO. LO METTO IN UN CAMPO DI 17
038500920911     C                   MOVEA     $O            $OUT17           17
038600920911     C                   ENDSR
038700920911     C*-------------------------------------------------------------------
038800921005     C*-------------------------------------------------------------------
038900921005     C     CARFBL        BEGSR
039000021029     C                   CLEAR                   FIVAB000
039001101021     c
039002101021     c                   movel     dateu         VABAAS
039003101021     c                   move      dateu         VABmgs
039100921005     C*
039200921005     C* CODICE BOLLA PARTENZA
039300960108     C                   MOVE      'A '          VABCBO
039400921005     C* RAGIONE SOCIALE DESTINATARIO
039500960108     C                   MOVEL     RAGLUO        VABRSD
039600921005     C* INDIRIZZO DESTINATARIO
039700960108     C                   MOVEL     INDLUO        VABIND
039800921005     C* CAP DESTINATARIO
039900960108     C                   MOVEL     CAPLUO        VABCAD
040000921005     C* LOCALITA' DESTINATARIO
040100960108     C                   MOVEL     CITLUO        VABLOD
040200921005     C* PROVINCIA DESTINATARIO
040300960108     C                   MOVEL     PRVLUO        VABPRD
040400921005     C* NATURA MERCE
040500921102     C     VD4NTM        IFNE      *BLANK
040600960108     C                   MOVEL     VD4NTM        VABNAS
040700921102     C                   ELSE
040800960108     C                   MOVE      GAI(2)        VABNAS
040900921102     C                   END
041000921005     C* NUMERO COLLI
041100960108     C                   Z-ADD     VD4NCO        VABNCL
041200921005     C* PESO IN KG
041300960108     C                   Z-ADD     VD4PKG        VABPKB
041400921005     C* RIFERIMENTO MITTENTE NUMERICO
041500960108     C                   Z-ADD     BOLNUO        VABRMN
041600980519     C* FLAG FERMO DEPOSITO E TIPO SPEDIZIONE
041700990202     C     VIDCCC        IFEQ      '000151'
041800960108     C                   MOVE      'S'           VABFFD
041900980519     C                   MOVE      'C'           VABTSP
042000921103     C                   ELSE
042100960108     C                   MOVE      ' '           VABFFD
042200980519     C                   MOVE      'E'           VABTSP
042300921103     C                   END
042400980519     C* FORZO LNA E ZONA 00 SIA PER DESTINATARIO SDI CHE PER DESTIN.BAR
042500980112     C* (NON DOVREBBE VERIFICARSI QUESTO CASO PERCHE' PER SDI (COME PER
042600980112     C* BARTOLINI) VIENE STAMPATA SOLO UNA BOLLA INTERNA --> TRATTAMENT
042700980112     C* MERCEOLOGICO CHE NON PREVEDE LA BOLLA FISCALE)
042800990202     C                   MOVE      VIDSCC        W0030             3 0
042900990202     C                   MOVEL     VIDSCC        W0040             5
043000990202     C     W0040         IFEQ      '00465'
043100980112     C     W0030         CHAIN     AZORG01L                           30
043200980112     C     *IN30         IFEQ      *OFF
043300980112     C                   MOVE      ORGFIL        VABLNA
043400101014     c* Se piena la filiale invio materialesede, imposto quella linea di arrivo
043500101014     c                   movel     orgde8        og148
043501150213     c                   movel     orgde3        og143
043600101014     c                   if        §ogfiue>*zeros
043700101014     c                   movel     §ogfiue       vablna
043701150213     c
043702150213     c* se si tratta di logistica tolgo il fermo deposito
043703150213     c                   if        §ogntw='LOG'
043704150213     C                   MOVE      ' '           VABFFD
043705150213     C                   MOVE      'E'           VABTSP
043706150213     c                   endif
043800101014     c                   endif
043900101014     c
044000980112     C                   MOVE      00            VABZNC
044100980519     C     ORGDIT        IFEQ      'SDI'
044200980519     C     VABFFD        IFEQ      'S'
044300980519     C                   MOVE      'D'           VABTSP
044400980519     C                   ELSE
044500980519     C                   MOVE      'C'           VABTSP
044600980519     C                   END
044700980112     C                   END
044800980519     C                   END
044900980112     C                   END
045000921005     C* SCRIVO IL RECORD
045001101021     c                   if        vablna<>046
045100021029     C                   WRITE     FIVAB000
045101101021     c                   endif
045200921005     C*
045300921005     C                   ENDSR
045400990202     C*----------------------------------------------------*
045500990202     C* Reperimento dati società
045600990202     C*----------------------------------------------------*
045700990202     C     REPSOC        BEGSR
045800990202     C*
045900990202     C                   CALLB     'XSOC'
046000990202     C                   PARM                    TIPXSC            6
046100990202     C                   PARM                    SOCXSC            3
046200990202     C                   PARM                    CDSXSC            9 0
046300990202     C                   PARM                    MODXSC            3
046400990202     C                   PARM      *blanks       RTNXSC            1
046500990202     C                   PARM                    XSOCDS
046600990202     C                   PARM                    KPJBA
046700990202     C*
046800990202     C                   ENDSR
046900921005     C*-------------------------------------------------------------------
047000871217     O*--------------------------------------------------------------*
047100871217     O*     TESTATA                                                  *
047200871217     O*--------------------------------------------------------------*
047300871224     OBOLLEPRT  E            TESTA            01
047400940722     O               93      SIMVIA              31
047500940722     O               93                          37 'Tel.:'
047600990202     O               93      xsctel              58
047700871224     O                       NUMBOL        Z     70
047800990202     O                       VIDDEM              84 '0  /  /    '
047900940722     O          E            TESTA            02
048000940722     O               93      SIMCIT              31
048100990202     O               93      xsccap              41
048200990202     O               93      xscPRV              44
048300990202     O               93                          49 'Fax:'
048400990202     O               93      xscfax              71
048500871228     O          E            TESTA            05
048600871217     O                       VIDSCC               8
048700871217     O                       RAGLUO              33
048800871217     O                       VIDRAG              69
048900871228     O          E            TESTA            06
049000871217     O                       RAGLU1              33
049100871217     O                       VIDRA1              69
049200871228     O          E            TESTA            07
049300871217     O                       INDLUO              39
049400871217     O                       VIDIND              75
049500871228     O          E            TESTA            08
049600880111     O                       CAPLUO              14
049700871217     O                       CITLUO              40
049800871217     O                       PRVLUO              43
049900880111     O                       VIDCAP              50
050000871224     O                       VIDCIT              76
050100871224     O                       VIDPRV              79
050200871228     O          E            TESTA            11
050300871217     O                       DESVET              24
050400871217     O                       INDVET              45
050500871217     O                       CITVET              60
050600871217     O*--------------------------------------------------------------*
050700871217     O*     DETTAGLIO                                                *
050800871217     O*--------------------------------------------------------------*
050900871228     O          E            DETT             14
051000871217     O          E            DETAIL      1
051100871217     O                       VIDMAT              13
051200871217     O                       VIDDMT              48
051300871217     O                       VIDQTA        2     63
051400920916     O                       STQTAL              78
051500871217     O*--------------------------------------------------------------*
051600871217     O*     FONDO                                                    *
051700871217     O*--------------------------------------------------------------*
051800871228     O          E            FONDO            48
051900871228     O                       DESTRO              45
052000871228     O          E            FONDO            50
052100871217     O                       DESIMB              27
052200921103     O                       VD4NTM              50
052300921103     O                       VD4NCO        Z     57
052400921103     O                       VD4PKG        4     64
052500871228     O          E            FONDO            54
052600871217     O                       DESPOR              27
052700871224     O                       T1                  30
052800871224     O                       T2                  34
052900871224     O                       T3                  38
053000921019     O          E            FONDO            58
053100921019     O               94      ST(1)               26
053200921019     O               94      ST(2)               50
053300971210** TABELLA CORRELATA PER IL CAMBIO NUMERO LETTERA
0534009209110Z
0535009209111A
0536009209112E
0537009209113G
0538009209114H
0539009209115M
0540009209116P
0541009209117S
0542009209118T
0543009209119K
054400920925** ST TABELLA LEGENDA LEGGE MINISTERIALE
0545009209251=A,2=E,3=G,4=H,5=M,6=P,     ST,1
0546009209257=S,8=T,9=K,0=Z,VIRGOLA=,    ST,2
054700921005** GAI TABELLA CONTROLLO SISTEMA INFORMATIVO
054800921102GAITRA201
054900921005VEDI BAM
