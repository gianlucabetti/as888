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
003500871217     D KPJBA         E DS
003600990202     D soc001        E DS                  EXTNAME(xsoc001ds)
003700990202     D xsocds          DS          1000
003800990202     d dataiso         s               d   datfmt(*iso)
003900990202     d dataeur         s               d   datfmt(*eur)
004000871217     I*---- PASSAGGIO PARAMETRI
004100871217     C*----------------------------------------------------*
004200871217     C     *ENTRY        PLIST                                                  *
004300871217     C                   PARM                    KPJBA                          *
004400990202     C*---------- RICERCA DITTA :
004500990202     C                   MOVEL     'SOC001'      TIPXSC
004600990202     C                   MOVEL     *blank        SOCXSC
004700990202     C                   EXSR      REPSOC
004800990202     C     RTNXSC        IFNE      '1'
004900990202     C                   MOVEL     XSOCDS        SOC001
005000990202     C                   MOVEL     xscrgs        rsut
005100990202     C                   MOVEL     xscsoc        wsoc              3
005200990202     c                   movel     xscvia        simvia           30
005300990202     c                   movel     xsccit        simcit           30
005400990202     c                   end
005500990202     c                   time                    como14           14 0
005600990202     c                   move      como14        woggi             8 0
005700920127     C*
005800921005     C* RICERCO SULLA TABELLA DELLA STAMPA PARTITA IVA
005900921005     C* E DELLA STAMPA LEGENDA CONVERSOINE
006000921005     C*
006100921005     C                   MOVE      '900'         TABGRU
006200921005     C                   MOVE      '9999999'     TABCOD
006300921005     C     KEYANT        CHAIN     ANTAB00F                           41
006400921005     C  N41              MOVEL     TABDES        STSINO            2
006500921005     C  N41              MOVEL     STSINO        STPIVA            1
006600921005     C  N41              MOVE      STSINO        STLEGE            1
006700921005     C   41              MOVEL     *BLANKS       STSINO
006800921005     C     *IN41         IFEQ      '1'
006900921005     C                   SETOFF                                       9394
007000921005     C                   END
007100921005     C     STPIVA        COMP      'S'                                    93
007200921005     C     STLEGE        COMP      'S'                                    94
007300921005     C*----------------------------------------------------*
007400921005     C* CONTROLLO SE IL SISTEMA INFORMATIVO  E' RIFERITO A *
007500921005     C* GAITRA201 (BARTOLINI)                              *
007600921005     C     KNSIF         IFEQ      GAI(1)                                       *
007700021029     C                   OPEN      FIVABWKF                                     *
007800921005     C                   Z-ADD     1             FLGAI             1 0
007900921005     C                   END                                                    *
008000871217     C*--- RICERCA ESISTENZA CONTI SUL P.D.C.              *
008100990202     C     KEYcln        KLIST                                                  *
008200990202     C                   KFLD                    wsoc                           *
008300990202     C                   KFLD                    vidscc                         *
008400890711     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
008500890711     C     KEYANT        KLIST                                                  *
008600890711     C                   KFLD                    TABGRU                         *
008700890711     C                   KFLD                    TABCOD                         *
008800871217     C*----------------------------------------------------*
008900871217     C     INIZIO        TAG
009000871217     C*          -------------
009100990202     C                   movel     '000151'      VIDCCC
009200990202     C                   movel     *blank        VIDSCC
009300990202     C                   Z-ADD     woggi         VIDDEM
009400871221     C                   EXFMT     OCO322D1
009500990412     C   Kc              GOTO      FINE
009600990202     C     VIDCCC        CABEQ     *blank        INIZIO
009700990202     C     VIDSCC        CABEQ     *blank        INIZIO
009800871218     C     VIDDEM        IFEQ      0
009900871218     C                   SETON                                        50
010000871218     C                   GOTO      INIZIO
010100871218     C                   END
010200890711     C                   SETOFF                                       72
010300990202     C     vidscc        CHAIN     tmclp000                           71
010400890711     C                   MOVE      '160'         TABGRU                         *
010500890711     C                   MOVE      *ZEROS        TABCOD                         *
010600890711     C                   MOVE      CLPSTM        TABCOD                         *
010700890711     C     KEYANT        CHAIN     ANTAB00F                           74        *
010800890711     C                   MOVEL     *BLANKS       STMDS
010900890711     C  N74              MOVEL     TABDES        STMDS
011000890711     C** NON E' POSSIBILE STAMPA BOLLA MANUALE SE SETTORE MERC.
011100980113     C** CLIENTE <> BLANKS
011200980113     C     STMFLG        COMP      ' '                                7373
011300890711     C   73              GOTO      INIZIO
011400871218     C*PRELEVO IL NUMERO BOLLA DALLA DATA AREA OCLBL1
011500871218     C     *DTAARA       DEFINE                  OCLBL1                         *
011600871218     C     SUDT1         TAG
011700871218     C     *LOCK         IN        OCLBL1                               46
011800871218     C   46              GOTO      SUDT1                                        *
011900990202     C                   move      woggi         DATAeur
012000990202     C                   move      dataeur       dataiso
012100990202     C                   move      DATAIso       RUDATE            8 0
012200990202     C                   move      VIDDEM        DATAeur
012300990202     C                   move      dataeur       dataiso
012400990202     C                   move      DATAIso       datain            8 0
012500990202     c     *ymd          move      databl        dataiso
012600990202     c                   move      dataiso       databl8           8 0
012700990202     C     DATAIN        IFLT      DATABL8
012800871218     C                   SETON                                        51
012900871218     C                   GOTO      INIZIO
013000871218     C                   END
013100871222     C** DATA EMISSIONE DEVE SSERE MAG.UG DATA ULTIMA BOLLA
013200871222     C     DATAIN        IFGT      RUDATE
013300871222     C                   SETON                                        51
013400871222     C   51              GOTO      INIZIO
013500871222     C                   END
013600871222     C** DATA EMISSIONE NON PUO' ESSERE MAGGIORE UDATE
013700871217     C*DECODIFICA DEL CLIENTE O DEL FORNITORE
013800990202     C     KEYcln        CHAIN     ancln000                           02
013900990202     C     clnsogg       CHAIN     ansog000                           02
014000990202     C     clnsogg       CHAIN     aNIND000                           03
014100871217     C                   MOVEL     *BLANKS       VIDRAG
014200871217     C                   MOVEL     *BLANKS       VIDRA1
014300871217     C                   MOVEL     *BLANKS       VIDIND
014400871217     C                   Z-ADD     0             VIDCAP
014500871217     C                   MOVEL     *BLANKS       VIDCIT
014600871217     C                   MOVEL     *BLANKS       VIDPRV
014700871217     C                   MOVEL     *BLANKS       RAGLUO
014800871217     C                   MOVEL     *BLANKS       RAGLU1
014900871217     C                   MOVEL     *BLANKS       INDLUO
015000871217     C                   MOVEL     *BLANKS       CITLUO
015100871217     C                   Z-ADD     0             CAPLUO
015200871217     C                   MOVEL     *BLANKS       PRVLUO
015300871217     C  N02
015400990202     CANN03              MOVEL     sogdes        VIDRAG
015500990202     C                   MOVE      sogdes        VIDRA1
015600990202     C                   MOVEL     sogdes        RAGLUO
015700990202     C                   MOVE      sogdes        RAGLU1
015800990202     C                   MOVEL     INDindriz     VIDIND
015900990202     C                   MOVEL     INDindriz     INDLUO
016000990202     C                   MOVEL     INDlocalit    VIDCIT
016100990202     C                   MOVEL     INDlocalit    CITLUO
016200990202     C                   movel     INDCAP        VIDCAP
016300990202     C                   movel     INDCAP        CAPLUO
016400990202     C                   MOVEL     INDPRov       VIDPRV
016500990202     C                   MOVEL     INDPRov       PRVLUO
016600871217     C*PULIZIA SUBFILE
016700871217     C                   EXSR      PULIZ
016800871217     C*CARICAMENTO SUBFILE
016900871217     C                   EXSR      CARIC
017000871217     C                   Z-ADD     NUMBOL        BOLNUO            6 0
017100871217     C                   ADD       1             BOLNUO
017200871217     C                   Z-ADD     BOLNUO        VIDBOL
017300921103     C                   WRITE     OCO322D3
017400871217     C     STAR          TAG
017500871217     C                   EXFMT     OCO322SC
017600990202     C  NKl              DO
017700921103     C     RAGLUO        IFEQ      *BLANKS
017800921103     C                   SETON                                        64
017900921103     C                   GOTO      STAR
018000921103     C                   END
018100921103     C     INDLUO        IFEQ      *BLANKS
018200921103     C                   SETON                                        65
018300921103     C                   GOTO      STAR
018400921103     C                   END
018500921103     C     CITLUO        IFEQ      *BLANKS
018600921103     C                   SETON                                        66
018700921103     C                   GOTO      STAR
018800921103     C                   END
018900921103     C     CAPLUO        IFEQ      *ZEROS
019000921103     C                   SETON                                        67
019100921103     C                   GOTO      STAR
019200921103     C                   END
019300921103     C     PRVLUO        IFEQ      *BLANKS
019400921103     C                   SETON                                        68
019500921103     C                   GOTO      STAR
019600921103     C                   END
019700921103     C                   END
019800990202     C   Kl              GOTO      INIZIO
019900921005     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
020000921005     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
020100921005     C*
020200921005     C*
020300921102     C   KA              DO
020400921102     C                   EXSR      STAMPA
020500990317     C                   CALL      'TII200R'
020600910102     C                   PARM                    KPJBA
020700921102     C     FLGAI         IFEQ      1
020800921102     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
020900921102     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
021000921102     C*
021100020111     C*    CAPLUO        ANDNE     40138
021200921102     C                   EXSR      SPEBOL
021300990202     C  NKlVD4SEG        IFEQ      'S'
021400941125     C                   CALL      'TNSD50C'
021500921102     C                   PARM                    KPJBA
021600921102     C                   END
021700921102     C                   END
021800921102     C                   GOTO      FINE
021900921102     C                   END
022000871217     C  NKA              GOTO      STAR
022100871217     C     FINE          TAG
022200871217     C                   SETON                                        LR
022300921005     C******************************************************
022400921005     C     SPEBOL        BEGSR
022500921005     C**----------------------
022600921005     C** RICHIESTA DI SPEDIZIONE TRAMITE LA FILIALE DI BOLOGNA
022700921103     C                   SETOFF                                       606163
022800921103     C                   MOVEL     'S'           VD4BOL
022900921103     C                   MOVEL     'N'           VD4SEG
023000921005     C*
023100921005     C     SU            TAG
023200921005     C                   EXFMT     OCO322D4
023300921005     C*
023400990202     C  NKl              DO
023500921005     C*
023600921005     C  N60VD4BOL        IFEQ      'S'
023700921005     C                   SETON                                        60
023800921005     C                   GOTO      SU
023900921005     C*
024000921005     C                   END
024100921005     C*
024200921102     C   60
024300921102     CANN61VD4SEG        IFEQ      'S'
024400921005     C                   SETON                                        61
024500921102     C                   GOTO      SU
024600921005     C                   END
024700921005     C*
024800921005     C*
024900921102     C   60              EXSR      CARFBL
025000960118     C   60
025100960118     CAN 61
025200960118     CANN63              Z-ADD     VIDBOL        COM15            15 0
025300960118     C   60
025400960118     CAN 61
025500960118     CANN63              MOVEL     COM15         KPJBU
025600921005     C*
025700921005     C                   END
025800921005     C*
025900921005     C                   ENDSR
026000871217     C******************************************************
026100871217     C     PULIZ         BEGSR
026200871217     C**----------------------
026300871217     C** PULIZIA DEL SUBFILE
026400871217     C**----------------------
026500871217     C                   SETON                                        70
026600871217     C                   WRITE     OCO322SC
026700871217     C                   SETOFF                                       70
026800871217     C                   ENDSR
026900871217     C******************************************************
027000871217     C     CARIC         BEGSR
027100871217     C**----------------------
027200871217     C** CARICAMENTO SUBFILE
027300871217     C** CARICO RECORDS VUOTI
027400871217     C**----------------------
027500871217     C                   Z-ADD     0             NRR               2 0
027600871217     C     LET           TAG
027700871224     C                   DO        32
027800871217     C                   MOVEL     *BLANKS       VIDMAT
027900871217     C                   MOVEL     *BLANKS       VIDDMT
028000871217     C                   Z-ADD     0             VIDQTA
028100871217     C                   ADD       1             NRR
028200871217     C                   WRITE     OCO322S1
028300871217     C                   END
028400871217     C                   ENDSR
028500871217     C******************************************************
028600871217     C     STAMPA        BEGSR
028700871217     C**----------------------
028800871217     C** STAMPA BOLLA
028900871217     C**----------------------
029000871217     C                   Z-ADD     BOLNUO        NUMBOL
029100990202     C                   move      VIDDEM        DATAeur
029200990202     C                   move      dataeur       dataiso
029300990202     C                   move      DATAIso       DATABL8
029400871217     C                   OUT       OCLBL1
029500871228     C                   OPEN      BOLLEPRT
029600871217     C                   EXCEPT    TESTA
029700871217     C   OF              DO
029800871217     C                   EXCEPT    FONDO
029900871217     C                   EXCEPT    TESTA
030000871217     C                   END
030100871217     C                   EXCEPT    DETT
030200871224     C                   DO        32            A                 2 0
030300871217     C     A             CHAIN     OCO322S1                           05
030400871217     C   05              GOTO      SOTTO
030500920911     C*
030600920911     C* PRIMA DI STAMPARE LA QTA FACCIO LA CONVERSIONE IN LETTERE
030700920911     C* A.G. 11-9-92
030800920916     C                   MOVE      *BLANKS       STQTAL
030900920916     C     VIDQTA        IFNE      *ZEROS
031000920911     C                   MOVE      0             £CI              15 0
031100920911     C                   MOVE      VIDQTA        £CI
031200920911     C                   Z-ADD     2             $DC               2 0
031300920911     C***
031400920911     C                   EXSR      NUMALF
031500920911     C***
031600920911     C                   MOVE      $OUT17        STQTAL           11
031700920916     C                   END
031800920911     C** FINE VARAIAZIONE
031900871217     C                   EXCEPT    DETAIL
032000871217     C                   END
032100871217     C     SOTTO         TAG
032200871217     C                   EXCEPT    FONDO
032300871217     C     END2          ENDSR
032400871217     C***------------------------------------------------***
032500920911     C* ROUTINE CONVERSIONE NUMERI IN LETTERE ALFABETICHE
032600920911     C     NUMALF        BEGSR
032700920911     C*          ---------------
032800920911     C* PULISCO IL CAMPO FINALE
032900920911     C                   MOVE      *BLANKS       $O
033000920911     C* SE NUMERO NEGATIVO METTO SEGNO NELL'ULTIMO POSTO
033100920911     C     £CI           IFLT      0
033200920911     C                   MOVE      '-'           $O(17)
033300920911     C* LO RENDO POSISTIVO SE NO NON STAMPA L'ULTIMA LETTERA
033400920911     C     £CI           MULT      -1            £CI
033500920911     C                   ELSE
033600920911     C                   MOVE      ' '           $O(17)
033700920911     C                   END
033800920911     C* SI METTE IL CAMPO £CI NELLA SCHIERA DI INPUT $I
033900920911     C                   MOVE      £CI           £CI2             15
034000920911     C                   MOVEA     £CI2          $I
034100920911     C*
034200920911     C                   MOVE      0             £VIRG             1 0
034300920911     C                   MOVE      0             £VALID            1 0
034400920911     C* DETERMINO IN QUALE POSTO DEVO METTERE LA VIRGOLA
034500920911     C     15            SUB       $DC           £DE               2 0
034600920911     C     £DE           COMP      15                                     OD
034700920911     C* SE OD ACCESO NON DEVO STAMPARE LA VIRGOLA NELLA SCHIERA
034800920911     C* DI OUTPUT
034900920911     C  NOD              Z-ADD     1             J£                2 0
035000920911     C   OD              Z-ADD     2             J£
035100920911     C*
035200920911     C     1             DO        15            I£                2 0
035300920911     C     $I(I£)        COMP      '0'                                    OC
035400920911     C   OC£VALID        COMP      0                                      OC
035500920911     C   OC£VIRG         COMP      0                                      OC
035600920911     C   OC              MOVE      ' '           $O(J£)
035700920911     C*
035800920911     C  NOC              MOVE      1             £VALID
035900920911     C  NOC$I(I£)        LOOKUP    TAB$          TAB£                     OB
036000920911     C  NOC
036100920911     CAN OB              MOVE      TAB£          $O(J£)
036200920911     C  NOC
036300920911     CANNOB              MOVE      ' '           $O(J£)
036400920911     C* CONTROLLO SE DEVO STAMPARE LA VIRGOLA
036500920911     C  NODJ£            IFEQ      £DE
036600920911     C* CONTROLLO SE DEVO METTERE UNO ZERO PRIME DELLA VIRGOLA
036700920911     C     £VALID        IFEQ      0
036800920911     C     '0'           LOOKUP    TAB$          TAB£                     OB
036900920911     C   OB              MOVE      TAB£          $O(J£)
037000920911     C                   END
037100920911     C*
037200920911     C                   ADD       1             J£
037300920911     C                   MOVE      ','           $O(J£)
037400920911     C                   MOVE      1             £VIRG
037500920911     C                   END
037600920911     C*
037700920911     C                   ADD       1             J£
037800920911     C*
037900920911     C                   END
038000920911     C* ESEGUITA LA ROUTINE IL CAMPO $O CONTIENE LE LETTERE DA
038100920911     C* STAMPARE CON LA VIRGOLA O MENO. LO METTO IN UN CAMPO DI 17
038200920911     C                   MOVEA     $O            $OUT17           17
038300920911     C                   ENDSR
038400920911     C*-------------------------------------------------------------------
038500921005     C*-------------------------------------------------------------------
038600921005     C     CARFBL        BEGSR
038700021029     C                   CLEAR                   FIVAB000
038800921005     C*
038900921005     C* CODICE BOLLA PARTENZA
039000960108     C                   MOVE      'A '          VABCBO
039100921005     C* RAGIONE SOCIALE DESTINATARIO
039200960108     C                   MOVEL     RAGLUO        VABRSD
039300921005     C* INDIRIZZO DESTINATARIO
039400960108     C                   MOVEL     INDLUO        VABIND
039500921005     C* CAP DESTINATARIO
039600960108     C                   MOVEL     CAPLUO        VABCAD
039700921005     C* LOCALITA' DESTINATARIO
039800960108     C                   MOVEL     CITLUO        VABLOD
039900921005     C* PROVINCIA DESTINATARIO
040000960108     C                   MOVEL     PRVLUO        VABPRD
040100921005     C* NATURA MERCE
040200921102     C     VD4NTM        IFNE      *BLANK
040300960108     C                   MOVEL     VD4NTM        VABNAS
040400921102     C                   ELSE
040500960108     C                   MOVE      GAI(2)        VABNAS
040600921102     C                   END
040700921005     C* NUMERO COLLI
040800960108     C                   Z-ADD     VD4NCO        VABNCL
040900921005     C* PESO IN KG
041000960108     C                   Z-ADD     VD4PKG        VABPKB
041100921005     C* RIFERIMENTO MITTENTE NUMERICO
041200960108     C                   Z-ADD     BOLNUO        VABRMN
041300980519     C* FLAG FERMO DEPOSITO E TIPO SPEDIZIONE
041400990202     C     VIDCCC        IFEQ      '000151'
041500960108     C                   MOVE      'S'           VABFFD
041600980519     C                   MOVE      'C'           VABTSP
041700921103     C                   ELSE
041800960108     C                   MOVE      ' '           VABFFD
041900980519     C                   MOVE      'E'           VABTSP
042000921103     C                   END
042100980519     C* FORZO LNA E ZONA 00 SIA PER DESTINATARIO SDI CHE PER DESTIN.BAR
042200980112     C* (NON DOVREBBE VERIFICARSI QUESTO CASO PERCHE' PER SDI (COME PER
042300980112     C* BARTOLINI) VIENE STAMPATA SOLO UNA BOLLA INTERNA --> TRATTAMENT
042400980112     C* MERCEOLOGICO CHE NON PREVEDE LA BOLLA FISCALE)
042500990202     C                   MOVE      VIDSCC        W0030             3 0
042600990202     C                   MOVEL     VIDSCC        W0040             5
042700990202     C     W0040         IFEQ      '00465'
042800980112     C     W0030         CHAIN     AZORG01L                           30
042900980112     C     *IN30         IFEQ      *OFF
043000980112     C                   MOVE      ORGFIL        VABLNA
043100980112     C                   MOVE      00            VABZNC
043200980519     C     ORGDIT        IFEQ      'SDI'
043300980519     C     VABFFD        IFEQ      'S'
043400980519     C                   MOVE      'D'           VABTSP
043500980519     C                   ELSE
043600980519     C                   MOVE      'C'           VABTSP
043700980519     C                   END
043800980112     C                   END
043900980519     C                   END
044000980112     C                   END
044100921005     C* SCRIVO IL RECORD
044200021029     C                   WRITE     FIVAB000
044300921005     C*
044400921005     C                   ENDSR
044500990202     C*----------------------------------------------------*
044600990202     C* Reperimento dati società
044700990202     C*----------------------------------------------------*
044800990202     C     REPSOC        BEGSR
044900990202     C*
045000990202     C                   CALLB     'XSOC'
045100990202     C                   PARM                    TIPXSC            6
045200990202     C                   PARM                    SOCXSC            3
045300990202     C                   PARM                    CDSXSC            9 0
045400990202     C                   PARM                    MODXSC            3
045500990202     C                   PARM      *blanks       RTNXSC            1
045600990202     C                   PARM                    XSOCDS
045700990202     C                   PARM                    KPJBA
045800990202     C*
045900990202     C                   ENDSR
046000921005     C*-------------------------------------------------------------------
046100871217     O*--------------------------------------------------------------*
046200871217     O*     TESTATA                                                  *
046300871217     O*--------------------------------------------------------------*
046400871224     OBOLLEPRT  E            TESTA            01
046500940722     O               93      SIMVIA              31
046600940722     O               93                          37 'Tel.:'
046700990202     O               93      xsctel              58
046800871224     O                       NUMBOL        Z     70
046900990202     O                       VIDDEM              84 '0  /  /    '
047000940722     O          E            TESTA            02
047100940722     O               93      SIMCIT              31
047200990202     O               93      xsccap              41
047300990202     O               93      xscPRV              44
047400990202     O               93                          49 'Fax:'
047500990202     O               93      xscfax              71
047600871228     O          E            TESTA            05
047700871217     O                       VIDSCC               8
047800871217     O                       RAGLUO              33
047900871217     O                       VIDRAG              69
048000871228     O          E            TESTA            06
048100871217     O                       RAGLU1              33
048200871217     O                       VIDRA1              69
048300871228     O          E            TESTA            07
048400871217     O                       INDLUO              39
048500871217     O                       VIDIND              75
048600871228     O          E            TESTA            08
048700880111     O                       CAPLUO              14
048800871217     O                       CITLUO              40
048900871217     O                       PRVLUO              43
049000880111     O                       VIDCAP              50
049100871224     O                       VIDCIT              76
049200871224     O                       VIDPRV              79
049300871228     O          E            TESTA            11
049400871217     O                       DESVET              24
049500871217     O                       INDVET              45
049600871217     O                       CITVET              60
049700871217     O*--------------------------------------------------------------*
049800871217     O*     DETTAGLIO                                                *
049900871217     O*--------------------------------------------------------------*
050000871228     O          E            DETT             14
050100871217     O          E            DETAIL      1
050200871217     O                       VIDMAT              13
050300871217     O                       VIDDMT              48
050400871217     O                       VIDQTA        2     63
050500920916     O                       STQTAL              78
050600871217     O*--------------------------------------------------------------*
050700871217     O*     FONDO                                                    *
050800871217     O*--------------------------------------------------------------*
050900871228     O          E            FONDO            48
051000871228     O                       DESTRO              45
051100871228     O          E            FONDO            50
051200871217     O                       DESIMB              27
051300921103     O                       VD4NTM              50
051400921103     O                       VD4NCO        Z     57
051500921103     O                       VD4PKG        4     64
051600871228     O          E            FONDO            54
051700871217     O                       DESPOR              27
051800871224     O                       T1                  30
051900871224     O                       T2                  34
052000871224     O                       T3                  38
052100921019     O          E            FONDO            58
052200921019     O               94      ST(1)               26
052300921019     O               94      ST(2)               50
052400971210** TABELLA CORRELATA PER IL CAMBIO NUMERO LETTERA
0525009209110Z
0526009209111A
0527009209112E
0528009209113G
0529009209114H
0530009209115M
0531009209116P
0532009209117S
0533009209118T
0534009209119K
053500920925** ST TABELLA LEGENDA LEGGE MINISTERIALE
0536009209251=A,2=E,3=G,4=H,5=M,6=P,     ST,1
0537009209257=S,8=T,9=K,0=Z,VIRGOLA=,    ST,2
053800921005** GAI TABELLA CONTROLLO SISTEMA INFORMATIVO
053900921102GAITRA201
054000921005VEDI BAM
