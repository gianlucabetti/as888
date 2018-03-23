000100990203     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200990203     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300890710     H* OCO401R *-OC/38-*--------------------------------------------*
000400000000     H*-----------------*                                            *
000500890710     H*          - STAMPA BOLLA MANUALE INTERNA -                    *
000600000000     H*          ------------------------------------                *
000700990203     H*         FILE : ancln01l - anind01l - ANSPE01L -              *
000800021029     H*                FIVABWKF - ansog01l                           *
000900890711     H*--------------------------------------------------------------*
001000990203     Fancln01l  IF   E           K DISK
001100990203     Fansog01l  IF   E           K DISK
001200990203     Fanind01l  IF   E           K DISK
001300990203     Ftmclp01L  IF   E           K DISK
001400890711     FANTAB00F  IF   E           K DISK
001500980112     FAZORG01L  IF   E           K DISK
001600990317     FTII314D   CF   E             WORKSTN
001700890710     F                                     SFILE(OCO401S1:NRR)
001800021029     FFIVABWKF  O  A E           K DISK    USROPN
001900890710     FBOLLEINT  O    F  132        PRINTER OFLIND(*INOF)
002000890710     F                                     USROPN
002100890710     D*--------------------------------------------------------------*
002200890710     D SK1             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 1
002300890710     D SK2             S             66    DIM(2) CTDATA PERRCD(1)              *TESTATA 2
002400890710     D SK3             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 1
002500890710     D SK4             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 2
002600890720     D SK5             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 3
002700890720     D SK6             S             66    DIM(2) CTDATA PERRCD(1)              *NOTE 4
002800921102     D*--------------------------------------------------------------*
002900101014     D GAI             S             10    DIM(3) CTDATA PERRCD(1)
003000921102     D*--------------------------------------------------------------*
003100101013     d og148         e ds
003200101013     d
003300890710     D OCLBL2          DS
003400000000     D*--- DATA AREA N.BOLLA /DATA BOLLA :
003500000000     D  NUMBOL                 1      6  0
003600000000     D  DATABL                 7     12  0
003700890711     D STMDS           DS
003800890711     D  STMFLG                41     41
003900891128     D KPJBA         E DS
004000990203     D soc001        E DS                  EXTNAME(xsoc001ds)
004100990203     D xsocds          DS          1000
004200990203     d dataiso         s               d   datfmt(*iso)
004300990203     d dataeur         s               d   datfmt(*eur)
004400000000     C*----------------------------------------------------*
004500000000     C     *ENTRY        PLIST                                                  *
004600000000     C                   PARM                    KPJBA                          *
004700990203     C*---------- RICERCA DITTA :
004800990203     C                   MOVEL     'SOC001'      TIPXSC
004900990203     C                   MOVEL     *blank        SOCXSC
005000990203     C                   EXSR      REPSOC
005100990203     C     RTNXSC        IFNE      '1'
005200990203     C                   MOVEL     XSOCDS        SOC001
005300990203     C                   MOVEL     xscsoc        wsoc              3
005400990203     C                   MOVEL     xscrgs        desdit           20
005500990203     C                   MOVEL     xscrgs        DITRSC           35
005600990203     C                   MOVEL     xscvia        DITIND           35
005700990203     C                   movel     xsccap        DITCAP            5 0
005800990203     C                   MOVEL     xsccit        DITLOC           20
005900990203     C                   MOVEL     xscprv        DITPRV            2
006000990203     c                   end
006100990203     c                   time                    como14           14 0
006200990203     c                   move      como14        woggi             8 0
006300990203     c                   move      woggi         dataeur
006400990203     c                   move      dataeur       dataiso
006500990203     c                   move      dataiso       woggig            8 0
006600921102     C*----------------------------------------------------*
006700921102     C* CONTROLLO SE IL SISTEMA INFORMATIVO  E' RIFERITO A *
006800921102     C* GAITRA201 (BARTOLINI)                              *
006900971210     C     KNSIF         IFEQ      GAI(1)                                       *
007000101014     C     KNSIF         oreq      GAI(3)                                       *
007100021029     C                   OPEN      FIVABWKF                                     *
007200921102     C                   Z-ADD     1             FLGAI             1 0
007300921102     C                   END                                                    *
007400000000     C*--- RICERCA ESISTENZA CONTI SUL P.D.C.              *
007500990203     C     KEYcln        KLIST                                                  *
007600990203     C                   KFLD                    wsoc                           *
007700990203     C                   KFLD                    vidscc                         *
007800890711     C*--- FILE ANAGRAFICO TABELLE "ANTAB00F" -------------*
007900890711     C     KEYANT        KLIST                                                  *
008000890711     C                   KFLD                    TABGRU                         *
008100890711     C                   KFLD                    TABCOD                         *
008200000000     C*----------------------------------------------------*
008300000000     C     INIZIO        TAG
008400000000     C*          -------------
008500140414     C                   move      '000151'      VIDCCC
008600990203     C                   move      *blank        VIDSCC
008700990203     C                   Z-ADD     woggi         VIDDEM
008800890710     C                   EXFMT     OCO401D1
008900990412     C   Kc              GOTO      FINE
009000990203     C     VIDCCC        CABEQ     *blank        INIZIO
009100990203     C     VIDSCC        CABEQ     *blank        INIZIO
009200000000     C     VIDDEM        IFEQ      0
009300000000     C                   SETON                                        50
009400000000     C                   GOTO      INIZIO
009500000000     C                   END
009600961029     C                   SETOFF                                         7576
009700990203     C     vidscc        CHAIN     tmclp000                           71
009800140414     c   71              goto      inizio
009900140414     c
010000890711     C  N71              DO
010100890718     C*AGENZIA
010200890711     C     CLPSTM        IFEQ      '52'
010300980112     C     CLPSTM        OREQ      '57'
010400961029     C                   SETON                                        75
010500890711     C                   END
010600890718     C*FILIALE
010700890718     C     CLPSTM        IFEQ      '53'
010800980112     C     CLPSTM        OREQ      '56'
010900890718     C                   SETON                                        75
011000890718     C                   END
011100890718     C*AZIENDA
011200890718     C     CLPSTM        IFEQ      '54'
011300890718     C                   SETON                                        76
011400890718     C                   END
011500890711     C                   END
011600890711     C                   MOVE      '160'         TABGRU                         *
011700890711     C                   MOVE      *ZEROS        TABCOD                         *
011800890711     C                   MOVE      CLPSTM        TABCOD                         *
011900890711     C     KEYANT        CHAIN     ANTAB00F                           74        *
012000890711     C                   MOVEL     *BLANKS       STMDS
012100890711     C  N74              MOVEL     TABDES        STMDS
012200890711     C** NON E' POSSIBILE STAMPA BOLLA MANUALE INTERNA SE SETT.MERC.
012300890711     C** CLIENTE = BLANKS
012400890711     C     STMFLG        COMP      ' '                                    73
012500890711     C   73              GOTO      INIZIO
012600890711     C*PRELEVO IL NUMERO BOLLA DALLA DATA AREA OCLBL2
012700890711     C     *DTAARA       DEFINE                  OCLBL2                         *
012800000000     C     SUDT1         TAG
012900890710     C     *LOCK         IN        OCLBL2                               46
013000000000     C   46              GOTO      SUDT1                                        *
013100990203     C                   move      woggig        RUDATE            8 0
013200990203     C                   move      VIDDEM        DATAeur
013300990203     C                   move      dataeur       DATAiso
013400990203     c                   move      dataiso       datain            8 0
013500990203     C     *ymd          move      databl        dataiso
013600990203     C                   move      dataiso       databl8           8 0
013700990203     C     DATAIN        IFLT      DATABL8
013800000000     C                   SETON                                        51
013900000000     C                   GOTO      INIZIO
014000000000     C                   END
014100000000     C** DATA EMISSIONE DEVE SSERE MAG.UG DATA ULTIMA BOLLA
014200000000     C     DATAIN        IFGT      RUDATE
014300000000     C                   SETON                                        51
014400000000     C   51              GOTO      INIZIO
014500000000     C                   END
014600000000     C** DATA EMISSIONE NON PUO' ESSERE MAGGIORE UDATE
014700000000     C*DECODIFICA DEL CLIENTE O DEL FORNITORE
014800990203     C     KEYcln        CHAIN     ancln01l                           02
014900990203     c  n02clnsogg       chain     ansog01l                           02
015000990203     C     clnsogg       CHAIN     anind01l                           03
015100000000     C                   MOVEL     *BLANKS       VIDRAG
015200000000     C                   MOVEL     *BLANKS       VIDRA1
015300000000     C                   MOVEL     *BLANKS       VIDIND
015400000000     C                   Z-ADD     0             VIDCAP
015500000000     C                   MOVEL     *BLANKS       VIDCIT
015600000000     C                   MOVEL     *BLANKS       VIDPRV
015700000000     C                   MOVEL     *BLANKS       RAGLUO
015800000000     C                   MOVEL     *BLANKS       RAGLU1
015900000000     C                   MOVEL     *BLANKS       INDLUO
016000000000     C                   MOVEL     *BLANKS       CITLUO
016100000000     C                   Z-ADD     0             CAPLUO
016200000000     C                   MOVEL     *BLANKS       PRVLUO
016300990203     C                   if        not *in02 and not *in03
016400990203     C                   MOVEL     sogdes        VIDRAG
016500990203     C                   MOVE      sogdes        VIDRA1
016600990203     C                   MOVEL     sogdes        RAGLUO
016700990203     C                   MOVE      sogdes        RAGLU1
016800990203     C                   MOVEL     INDindriz     VIDIND
016900990203     C                   MOVEL     INDindriz     INDLUO
017000990203     C                   MOVEL     INDlocalit    VIDCIT
017100990203     C                   MOVEL     INDlocalit    CITLUO
017200990203     C                   movel     INDCAP        VIDCAP
017300990203     C                   movel     INDCAP        CAPLUO
017400990203     C                   MOVEL     INDPRov       VIDPRV
017500990203     C                   MOVEL     INDPRov       PRVLUO
017600990203     c                   end
017700000000     C*PULIZIA SUBFILE
017800000000     C                   EXSR      PULIZ
017900000000     C*CARICAMENTO SUBFILE
018000000000     C                   EXSR      CARIC
018100000000     C                   Z-ADD     NUMBOL        BOLNUO            6 0
018200000000     C                   ADD       1             BOLNUO
018300000000     C                   Z-ADD     BOLNUO        VIDBOL
018400921103     C                   WRITE     OCO401D3
018500000000     C     STAR          TAG
018600890710     C                   EXFMT     OCO401SC
018700990203     C  NKl              DO
018800921103     C     RAGLUO        IFEQ      *BLANKS
018900921103     C                   SETON                                        64
019000921103     C                   GOTO      STAR
019100921103     C                   END
019200921103     C     INDLUO        IFEQ      *BLANKS
019300921103     C                   SETON                                        65
019400921103     C                   GOTO      STAR
019500921103     C                   END
019600921103     C     CITLUO        IFEQ      *BLANKS
019700921103     C                   SETON                                        66
019800921103     C                   GOTO      STAR
019900921103     C                   END
020000921103     C     CAPLUO        IFEQ      *ZEROS
020100921103     C                   SETON                                        67
020200921103     C                   GOTO      STAR
020300921103     C                   END
020400921103     C     PRVLUO        IFEQ      *BLANKS
020500921103     C                   SETON                                        68
020600921103     C                   GOTO      STAR
020700921103     C                   END
020800921103     C                   END
020900990203     C   Kl              GOTO      INIZIO
021000921102     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
021100921102     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
021200921102     C*
021300921102     C*
021400921102     C   KA              DO
021500921102     C                   EXSR      STAMPA
021600921102     C     FLGAI         IFEQ      1
021700921102     C* SE FLGAI UGUALE A 1 (QUINDI SETRAS) E CAP DIVERSO DA 40138
021800921102     C* SCRIVO IL RECORD DELL'ARCHIVIO SPEDIZIONE
021900921102     C*
022000981222     C********** CAPLUO    ANDNE40138
022100921102     C                   EXSR      SPEBOL
022200990203     C  NKlVD4SEG        IFEQ      'S'
022300140414     c                   eval      %subst(kpjbu:20:3)='ACQ'
022400941125     C                   CALL      'TNSD50C'
022500921102     C                   PARM                    KPJBA
022600921102     C                   END
022700921102     C                   END
022800921102     C                   GOTO      FINE
022900921102     C                   END
023000000000     C  NKA              GOTO      STAR
023100000000     C     FINE          TAG
023200000000     C                   SETON                                        LR
023300921102     C******************************************************
023400921102     C     SPEBOL        BEGSR
023500921102     C**----------------------
023600921102     C** RICHIESTA DI SPEDIZIONE TRAMITE LA FILIALE DI BOLOGNA
023700921103     C                   SETOFF                                       606163
023800921103     C                   MOVEL     'S'           VD4BOL
023900921103     C                   MOVEL     'N'           VD4SEG
024000921102     C*
024100921102     C     SU            TAG
024200921102     C                   EXFMT     OCO401D4
024300921102     C*
024400990203     C  NKl              DO
024500921102     C*
024600921102     C  N60VD4BOL        IFEQ      'S'
024700921102     C                   SETON                                        60
024800921102     C                   GOTO      SU
024900921102     C*
025000921102     C                   END
025100921102     C*
025200921102     C   60
025300921102     CANN61VD4SEG        IFEQ      'S'
025400921102     C                   SETON                                        61
025500921102     C                   GOTO      SU
025600921102     C                   END
025700921102     C   60              EXSR      CARFBL
025800960118     C   60
025900960118     CAN 61
026000960118     CANN63              Z-ADD     VIDBOL        COM15            15 0
026100960118     C   60
026200960118     CAN 61
026300960118     CANN63              MOVEL     COM15         KPJBU
026400921102     C*
026500921102     C                   END
026600921102     C*
026700921102     C                   ENDSR
026800000000     C******************************************************
026900000000     C     PULIZ         BEGSR
027000000000     C**----------------------
027100000000     C** PULIZIA DEL SUBFILE
027200000000     C**----------------------
027300000000     C                   SETON                                        70
027400890710     C                   WRITE     OCO401SC
027500000000     C                   SETOFF                                       70
027600000000     C                   ENDSR
027700000000     C******************************************************
027800000000     C     CARIC         BEGSR
027900000000     C**----------------------
028000000000     C** CARICAMENTO SUBFILE
028100000000     C** CARICO RECORDS VUOTI
028200000000     C**----------------------
028300000000     C                   Z-ADD     0             NRR               2 0
028400000000     C     LET           TAG
028500000000     C                   DO        32
028600000000     C                   MOVEL     *BLANKS       VIDMAT
028700000000     C                   MOVEL     *BLANKS       VIDDMT
028800000000     C                   Z-ADD     0             VIDQTA
028900000000     C                   ADD       1             NRR
029000890710     C                   WRITE     OCO401S1
029100000000     C                   END
029200000000     C                   ENDSR
029300000000     C******************************************************
029400000000     C     STAMPA        BEGSR
029500000000     C**----------------------
029600000000     C** STAMPA BOLLA
029700000000     C**----------------------
029800000000     C                   Z-ADD     BOLNUO        NUMBOL
029900990203     C                   move      DATAIN        DATABL
030000890711     C                   OUT       OCLBL2
030100890710     C                   OPEN      BOLLEINT
030200000000     C                   EXCEPT    TESTA
030300000000     C   OF              DO
030400890711     C                   EXCEPT    SEGUE
030500000000     C                   EXCEPT    FONDO
030600000000     C                   EXCEPT    TESTA
030700000000     C                   END
030800000000     C                   DO        32            A                 2 0
030900890710     C     A             CHAIN     OCO401S1                           05
031000000000     C   05              GOTO      SOTTO
031100000000     C                   EXCEPT    DETAIL
031200000000     C                   END
031300000000     C     SOTTO         TAG
031400000000     C                   EXCEPT    FONDO
031500000000     C     END2          ENDSR
031600890710     C*
031700921102     C*-------------------------------------------------------------------
031800921102     C     CARFBL        BEGSR
031900021029     C                   CLEAR                   FIVAB000
032000101021     c                   movel     woggig        VABAAS
032100101021     c                   move      woggig        VABmgs
032200921102     C*
032300921102     C* CODICE BOLLA PARTENZA
032400960108     C                   MOVE      'A '          VABCBO
032500921102     C* RAGIONE SOCIALE DESTINATARIO
032600131211     c                   clear                   vabrd2
032700131213     C                   MOVEL(P)  RAGLUO        VABRSD
032800131213     C                   MOVEL(P)  RAGLU1        VABRd2
032900131213     c**                 eval      vabrsd=ragluo+raglu1
033000131213     c**                 if        %subst(raglu1:12:13)<>*blanks
033100131213     c**                 eval      vabrd2=%subst(raglu1:12:13)
033200131213     c**                 endif
033300921102     C* INDIRIZZO DESTINATARIO
033400960108     C                   MOVEL     INDLUO        VABIND
033500921102     C* CAP DESTINATARIO
033600960108     C                   MOVEL     CAPLUO        VABCAD
033700921102     C* LOCALITA' DESTINATARIO
033800960108     C                   MOVEL     CITLUO        VABLOD
033900921102     C* PROVINCIA DESTINATARIO
034000960108     C                   MOVEL     PRVLUO        VABPRD
034100921102     C* NATURA MERCE
034200921102     C     VD4NTM        IFNE      *BLANK
034300960108     C                   MOVEL     VD4NTM        VABNAS
034400921102     C                   ELSE
034500960108     C                   MOVE      GAI(2)        VABNAS
034600921102     C                   END
034700921102     C* NUMERO COLLI
034800960108     C                   Z-ADD     VD4NCO        VABNCL
034900921102     C* PESO IN KG
035000960108     C                   Z-ADD     VD4PKG        VABPKB
035100921102     C* RIFERIMENTO MITTENTE NUMERICO
035200960108     C                   Z-ADD     BOLNUO        VABRMN
035300980519     C* FLAG FERMO DEPOSITO E TIPO SPEDIZIONE
035400140414     C**   VIDCCC        IFEQ      '000151'
035500140414     c* imposrto sempre fermo deposito tranne che per codice 0461509
035600140414     c                   if        vidscc<>'00461509'
035700960108     C                   MOVE      'S'           VABFFD
035800980519     C                   MOVE      'C'           VABTSP
035900921103     C                   ELSE
036000960108     C                   MOVE      ' '           VABFFD
036100980519     C                   MOVE      'E'           VABTSP
036200921103     C                   END
036300980519     C* FORZO LNA E ZONA 00 SIA PER DESTINATARIO SDI CHE PER DESTIN.BAR
036400980112     C                   MOVE      VIDSCC        W0030             3 0
036500990203     C                   MOVEL     VIDSCC        W0040             5 0
036600990203     C     W0040         IFEQ      00465
036700980112     C     W0030         CHAIN     AZORG01L                           30
036800980112     C     *IN30         IFEQ      *OFF
036900101013     C                   MOVE      ORGFIL        VABLNA
037000101013     c* Se piena la linea per invio materiale SEDE imposto quella
037100101013     c                   movel     orgde8        og148
037200101013     c                   if        §ogfiue>*zeros
037300101013     c                   movel     §ogfiue       vablna
037400101013     c                   endif
037500101013     c
037600980112     C                   MOVE      00            VABZNC
037700980519     C     ORGDIT        IFEQ      'SDI'
037800980519     C     VABFFD        IFEQ      'S'
037900980519     C                   MOVE      'D'           VABTSP
038000980519     C                   ELSE
038100980519     C                   MOVE      'C'           VABTSP
038200980519     C                   END
038300980112     C                   END
038400980519     C                   END
038500980112     C                   END
038600921102     C* SCRIVO IL RECORD
038700101021     c                   if        vablna<>046
038800021029     C                   WRITE     FIVAB000
038900101021     c                   endif
039000921102     C*
039100921102     C                   ENDSR
039200990203     C*----------------------------------------------------*
039300990203     C* Reperimento dati società
039400990203     C*----------------------------------------------------*
039500990203     C     REPSOC        BEGSR
039600990203     C*
039700990203     C                   CALLB     'XSOC'
039800990203     C                   PARM                    TIPXSC            6
039900990203     C                   PARM                    SOCXSC            3
040000990203     C                   PARM                    CDSXSC            9 0
040100990203     C                   PARM                    MODXSC            3
040200990203     C                   PARM      *blanks       RTNXSC            1
040300990203     C                   PARM                    XSOCDS
040400990203     C                   PARM                    KPJBA
040500990203     C*
040600990203     C                   ENDSR
040700000000     O*--------------------------------------------------------------*
040800000000     O*     TESTATA                                                  *
040900000000     O*--------------------------------------------------------------*
041000890710     OBOLLEINT  E            TESTA            01
041100890710     O                       SK1                132
041200890710     O          E            TESTA            02
041300890710     O                       SK1                132
041400890710     O                       DITRSC              37
041500890710     O          E            TESTA            03
041600890710     O                       SK1                132
041700890710     O                       DITIND              37
041800890710     O                                           51 'BOLLA N.'
041900890710     O                       NUMBOL        Z     58
042000890710     O                                           63 'del'
042100990203     O                       VIDDEM              74 '0  /  /    '
042200890710     O          E            TESTA            04
042300890710     O                       SK1                132
042400890710     O                       DITCAP               7
042500890710     O                       DITLOC              28
042600890710     O                       DITPRV              31
042700890710     O          E            TESTA            05
042800890710     O                       SK1                132
042900890710     O          E            TESTA            06
043000890710     O                       SK2                132
043100890710     O          E            TESTA            07
043200890710     O                                           24 'Destinatario e luogo di'
043300890710     O                                           38 'destinazione'
043400890710     O          E            TESTA            08
043500890710     O                       SK2                132
043600890710     O          E            TESTA            09
043700890710     O                       VIDSCC               8
043800961029     O               75                          28 'Spett.le P.O. di'
043900961029     O               76                          28 'Spett.le        '
044000890710     O                       VIDRAG              53
044100890710     O                       VIDRA1              77
044200921112     O*
044300921112     O          E            TESTA       1
044400921112     O                       INDLUO              59
044500921112     O          E            TESTA       1
044600921112     O                       CAPLUO              34
044700981222     O                       CITLUO              60
044800981222     O                       PRVLUO              63
044900921112     O*
045000890710     O          E            TESTA            14
045100890710     O                       SK2                132
045200890710     O          E            TESTA            15
045300890710     O                                            8 'Vettore'
045400890710     O          E            TESTA            16
045500890710     O                       DESVET              21
045600890710     O                       INDVET              42
045700890710     O                       CITVET              58
045800890710     O          E            TESTA            17
045900890710     O                       SK2                132
046000890710     O          E            TESTA            18
046100890710     O                                           10 'Materiale'
046200890710     O                                           41 'Descrizione'
046300890710     O                                           61 'Quantità'
046400890710     O                                           81 'RIF. VS. ORDINE'
046500890710     O          E            TESTA            19
046600890710     O                       SK2                132
046700000000     O*--------------------------------------------------------------*
046800000000     O*     DETTAGLIO                                                *
046900000000     O*--------------------------------------------------------------*
047000000000     O          E            DETAIL      1
047100890711     O                       VIDMAT              14
047200000000     O                       VIDDMT              48
047300000000     O                       VIDQTA        2     63
047400890711     O*----------------------------------------------------------------
047500890711     O          E            SEGUE
047600890711     O                                           83 '%'
047700000000     O*--------------------------------------------------------------*
047800000000     O*     FONDO                                                    *
047900000000     O*--------------------------------------------------------------*
048000890711     O          E            FONDO            56
048100890711     O                       SK2                132
048200890711     O          E            FONDO            58
048300890720     O                       SK5                132
048400890711     O                                            6 'COLLI'
048500921103     O                       VD4NCO        Z     13
048600890711     O                                           27 'PESO'
048700921103     O                       VD4PKG        4     35
048800890720     O          E            FONDO            59
048900890720     O                       SK6                132
049000890711     O          E            FONDO            60
049100890711     O                       SK3                132
049200890711     O          E            FONDO            62
049300890711     O                       SK4                132
049400971210**   SK1
049500890711                                         !                        SK01
049600890711                                                                  SK01
049700890711**   SK2
049800890711------------------------------------------------------------------SK02
049900890711-----------------                                                 SK02
050000890711**   SK3
050100890711  NOTE -----------------------------------------------------------SK03
050200890711-----------------                                                 SK03
050300890711**   SK4
050400890711       -----------------------------------------------------------SK04
050500890711-----------------                                                 SK04
050600890720**   SK5
050700890720                                       Merci Esenti Emissione BollSK05
050800890720a Accompagnamento                                                 SK05
050900890720**   SK6
051000890720                                       Merci ai Sensi D.P.R. 6/10/SK06
05110089072078 N.627 Art.4/8                                                  SK06
051200921102** GAI TABELLA CONTROLLO SISTEMA INFORMATIVO
051300921102GAITRA201
051400921102VEDI BAM
051500101014GAITRAPSP
