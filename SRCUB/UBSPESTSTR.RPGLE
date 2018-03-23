000100080612     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('TEST_FG')
000200980304     H*------------------------------------------------------------------------*
000300120117     H* UBSPESTS - TEST
000400980304     H*------------------------------------------------------------------------*
000500980304     H DECEDIT('0,') DATEDIT(*DMY.)
000600980304     F*------------------------------------------------------------------------*
000700980304     F* DATA BASE
000800980304     F*------------------------------------------------------------------------*
000900120117     FUBSPESTSTDCF   E             WORKSTN INFDS(DEVDS1)
001000120117     F                                     SFILE(UBSSDMS:MSGULT)
001100120117     D* PARAMETRI
001200180109     D pInAnno         S              4s 0
001300180109     D pInLineaPar     S              3s 0
001400180109     D pInSerie        S              2s 0
001500180109     D pInNumSped      S              7s 0
001600120117     D pInLingua       S              2
001700120117     D pInMaiuscolo    S              1
001800120117     D pOutLblTyp      S              1
001900120117     D pOutRblBO       S              1
002000120117     D pOutStsSped     S              2
002100141106     D pOutDtConsRich  S              8S 0
002200120117     D pOutDtApGiac    s              8s 0
002300120117     D pOutDtLAvviso   s              8s 0
002400120117     D pOutDescSts     S            128
002500120117     D pOutDtSts       S              8S 0
002600120117     D pOutErrore      S              1
002700141106     D pOutCA          S              1
002800141106     D pOutDtPrGiac    S              8s 0
002900141106     D pOutDtChGiac    S              8s 0
003000980304     D*------------------------------------------------------------------------*
003100980304     D* SCHIERE
003200980304     D*------------------------------------------------------------------------*
003300980304     d MSG             S             80    DIM(10) CTDATA PERRCD(1)
003400980304     D*------------
003500980304     D* DESCRIZIONI TASTI FUNZIONALI
003600980304     D*------------
003700980304     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003800980304     D*------------
003900980304     D* FUNZIONI DA CARICARE A VIDEO
004000980304     D*------------
004100980304     D FUV             S             19    DIM(24)
004200980304     D*------------------------------------------------------------------------*
004300980304     D* INPUT
004400980304     D*------------------------------------------------------------------------*
004500980304     D*------------
004600980304     D* RIDEFINIZIONE FUNZIONI VIDEO
004700980304     D*------------
004800980304     D FUDDS           DS
004900980304     D  FUDDE1                 1      1
005000980304     D  FUDFUN                 2      3
005100980304     D  FUDDE2                 4     19
005200980304     D  FUDFI0                20     34
005300980304     D  FUDATT                35     35
005400980304     D  FUDFI1                36     80
005500980304     D  FUDDES                 1     19
005600980304     D  FUDRIG                 1     80
005700980304     D*------------
005800980304     D* DS GESTIONE TASTI FUNZIONALI
005900980304     D*------------
006000980304     D DSKEY         E DS
006100980304     D DEVDS1          DS
006200980304     D  KEY                  369    369
006300980304     D*------------
006400980304     D* DS "XMSG" - GESTIONE MESSAGGI
006500980304     D*------------
006600980304     D DSMSG         E DS
006700980304     D*------------------
006800980304     D* ARCHITETTURA
006900980304     D*------------------
007000980304     D KPJBA         E DS
007100080612     D*------------------
007200080612     D* LINKING A DEFINIZIONI ESTERNE
007300080612     D*------------------
007400120117     D*/COPY GAITRASRC/SRCPROTOPR,UBSPESTS
007500120117     D*/COPY GAITRASRC/SRCPROTOPI,UBSPESTS
007600980304     C*------------------------------------------------------------------------*
007700980304     C* MAIN LINES
007800980304     C*------------------------------------------------------------------------*
007900980304     C*
008000980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008100980304DO  1C     F03L00        DOUEQ     '1'
008200980304     C     F12L00        OREQ      '1'
008300980304     C     FINL00        OREQ      '1'
008400980304     C*
008500980304     C* EMETTE FORMATO VIDEO
008600980304     C                   EXSR      BUTFMR
008700980304     C*
008800980304     C* GESTIONE TASTI FUNZIONALI
008900980304     C                   EXSR      FUNCON
009000980304E   1C                   ENDDO
009100980304     C*
009200980304     C                   SETON                                        LR
009300980304     C*------------------------------------------------------------------------*
009400980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
009500980304     C*------------------------------------------------------------------------*
009600980304     C     BUTFMR        BEGSR
009700980304     C*
009800980304     C* EMETTE LA TESTATA
009900120117     C                   WRITE     UBSSD01
010000980304     C*
010100980304     C* SPEDISCE EVENTUALE MESSAGGIO
010200980304     C                   EXSR      SNDMSG
010300980304     C*
010400980304     C* EMETTE LE FUNZIONI
010500120117     C                   WRITE     UBSSD02
010600980304     C*
010700980304     C* EMETTE IL VIDEO
010800120117     C                   EXFMT     UBSSD03
010900980304     C*
011000980304     C* REIMPOSTA FLAG DI ERRORE
011100980304     C                   MOVEL     '0'           ERRL00
011200980304     C*
011300980304     C                   ENDSR
011400980304     C*------------------------------------------------------------------------*
011500980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
011600980304     C*------------------------------------------------------------------------*
011700980304     C     SNDMSG        BEGSR
011800980304     C*
011900980304     C* PULISCE IL SFILE MESSAGGI
012000980304     C                   MOVEL     '1'           *IN40
012100120117     C                   WRITE     UBSSDMC
012200980304     C                   MOVEL     '0'           *IN40
012300980304     C*
012400980304     C* VISUALIZZA IL MESSAGGIO
012500980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
012600980304     C                   CALL      'XMSG'        XMSGPA
012700980304IF  1C     DSMSER        IFEQ      *ZEROS
012800980304     C                   MOVEL     DSMSKE        MSGKEY
012900980304E   1C                   ENDIF
013000980304     C                   Z-ADD     1             MSGULT
013100120117     C                   WRITE     UBSSDMS
013200120117     C                   WRITE     UBSSDMC
013300980304     C*
013400980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
013500980304     C                   MOVEL     *BLANKS       DSMSMS
013600980304     C*
013700980304     C                   ENDSR
013800980304     C*------------------------------------------------------------------------*
013900980304     C* ENTGES - GESTIONE ENTER -> INVIO
014000980304     C*------------------------------------------------------------------------*
014100980304     C     ENTGES        BEGSR
014200080612     C*
014300120117     C                   eval      pInAnno     = VIDAASC
014400120117     C                   eval      pInLineaPar = VIDLNPC
014500120117     C                   eval      pInSerie    = VIDNRSC
014600120117     C                   eval      pInNumSped  = VIDNSPC
014700120117     C                   eval      pInLingua   = VIDLingua
014800120117     C                   eval      pInMaiuscolo= VIDMaius
014900080612     C*
015000120117     C                   CALL      'UBSPESTS'
015100120117     C                   PARM                    pInAnno
015200120117     C                   PARM                    pInLineaPar
015300120117     C                   PARM                    pInSerie
015400120117     C                   PARM                    pInNumSped
015500120117     C                   PARM                    pInLingua
015600120117     C                   PARM                    pInMaiuscolo
015700120117     C     vidlbltyp     PARM                    pOutLblTyp
015800120117     C     vidrblbo      PARM                    pOutRblBO
015900120117     C     vidstssped    PARM                    pOutStsSped
016000141106     C     viddtcons     PARM                    pOutDtConsRich
016100120117     C     vidapgiac     PARM                    pOutDtApGiac
016200120117     C     vidlavv       PARM                    pOutDtLAvviso
016300120117     C     viddescsts    PARM                    pOutDescSts
016400120117     C     viddtsts      PARM                    pOutDtSts
016500120117     C     viderrore     PARM                    pOutErrore
016600141106     C     vidca         PARM                    pOutCA
016700141106     C     vidprgiac     PARM                    pOutDtPrGiac
016800141106     C     vidchgiac     PARM                    pOutDtChGiac
016900080612     C*
017000120117     C*                  eval      VIDTIPL  = pOutLblTyp
017100120117     C*                  eval      VIDAASO  = pOutAnnoBO
017200120117     C*                  eval      VIDLNPO  = pOutLineaParBO
017300120117     C*                  eval      VIDNRSO  = pOutSerieBO
017400120117     C*                  eval      VIDNSPO  = pOutNumSpedBO
017500120117     C*                  eval      VIDDCMO  = pOutDcmBO
017600120117     C*                  eval      VIDCCAO  = pOutCcaBO
017700120117     C*                  eval      VIDRBLO  = pOutRblBO
017800120117     C*                  eval      VIDAASF  = pOutAnnoFI
017900120117     C*                  eval      VIDLNPF  = pOutLineaParFI
018000120117     C*                  eval      VIDNRSF  = pOutSerieFI
018100120117     C*                  eval      VIDNSPF  = pOutNumSpedFI
018200120117     C*                  eval      VIDDCMF  = pOutDcmFI
018300120117     C*                  eval      VIDCCAF  = pOutCcaFI
018400120117     C*                  eval      VIDAASFF = pOutAnnoFFI
018500120117     C*                  eval      VIDLNPFF = pOutLineaPaFFI
018600120117     C*                  eval      VIDNRSFF = pOutSerieFFI
018700120117     C*                  eval      VIDNSPFF = pOutNumSpedFFI
018800120117     C*                  eval      VIDDCMFF = pOutDcmFFI
018900120117     C*                  eval      VIDCCAFF = pOutCcaFFI
019000080612     C*
019100980304     C                   ENDSR
019200980304     C*------------------------------------------------------------------------*
019300980304     C* F03GES - GESTIONE F03 -> FINE
019400980304     C*------------------------------------------------------------------------*
019500980304     C     F03GES        BEGSR
019600980304     C*
019700980304     C* IMPOSTA "PREMUTO F03"
019800980304     C                   MOVEL     '1'           F03L00
019900980304     C*
020000980304     C                   ENDSR
020100980304     C*------------------------------------------------------------------------*
020200980304     C* F12GES - GESTIONE F12 -> RITORNO
020300980304     C*------------------------------------------------------------------------*
020400980304     C     F12GES        BEGSR
020500980304     C*
020600980304     C* IMPOSTA "PREMUTO F12"
020700980304     C                   MOVEL     '1'           F12L00
020800980304     C*
020900980304     C                   ENDSR
021000980304     C*------------------------------------------------------------------------*
021100980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
021200980304     C*------------------------------------------------------------------------*
021300980304     C     F24GES        BEGSR
021400980304     C*
021500980304     C                   EXSR      FUNGES
021600980304     C*
021700980304     C                   ENDSR
021800980304     C*------------------------------------------------------------------------*
021900980304     C* *INZSR - OPERAZIONI INIZIALI
022000980304     C*------------------------------------------------------------------------*
022100980304     C     *INZSR        BEGSR
022200980304     C*---
022300980304     C* RICEVIMENTO PARAMETRI
022400980304     C*---
022500980304     C     *ENTRY        PLIST
022600980304     C                   PARM                    KPJBA
022700980304     C*
022800980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
022900980304     C     XMSGPA        PLIST
023000980304     C                   PARM                    DSMSMS
023100980304     C                   PARM                    DSMSPG
023200980304     C                   PARM                    DSMSKE
023300980304     C                   PARM                    DSMSER
023400980304     C*
023500980304     C* IMPOSTA LA DS -TASTI PREMUTI-
023600980304     C                   CALL      'XKEY'
023700980304     C                   PARM                    DSKEY
023800980304     C*---
023900980304     C* VARIABILI NON RIFERITE AL DATA BASE
024000980304     C*---
024100980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
024200980304     C*---
024300980304     C* VARIABILI DI CONTROLLO
024400980304     C*---
024500980304     C                   MOVEL     '0'           FINL00            1
024600980304     C                   MOVEL     '0'           F03L00            1
024700980304     C                   MOVEL     '0'           F12L00            1
024800980304     C                   MOVEL     '0'           ERRL00            1
024900980304     C*
025000980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
025100120117     C                   MOVEL     'UBSPESTS'    DSMSPG
025200120117     C                   MOVEL     'UBSPESTS'    MSGQUE
025300980304     C                   MOVEL     *BLANKS       MSGKEY
025400980304     C                   MOVEL     *BLANKS       DSMSMS
025500980304     C*
025600980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
025700980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
025800980304     C                   EXSR      FUNGES
025900980304     C*
026000980304     C                   ENDSR
026100980304     C*------------------------------------------------------------------------*
026200980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
026300980304     C*------------------------------------------------------------------------*
026400980304     C     FUNGES        BEGSR
026500980304     C*
026600980304     C* PULISCO CAMPI DI LAVORO
026700980304     C                   Z-ADD     0             W                 5 0
026800980304     C*
026900980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
027000980304     C* E CARICO TABELLA FUNZIONI ATTIVE
027100980304     C     1             DO        24            I
027200980304     C                   MOVEL     FUD(I)        FUDRIG
027300980304     C*
027400980304     C* PULIZIA FUNZIONI PRECEDENTI
027500980304     C                   MOVEL     *BLANKS       FUV(I)
027600980304     C*
027700980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
027800980304     C* (SALTA F24)
027900980304     C     FUDATT        IFEQ      'S'
028000980304     C     FUDFUN        ANDNE     '24'
028100980304     C                   ADD       1             W
028200980304     C                   MOVEL     FUDDES        FUV(W)
028300980304     C                   END
028400980304     C                   END
028500980304     C*
028600980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
028700980304     C* ALTRIMENTI DISATTIVO F24
028800980304     C     1             DO        24            I
028900980304     C                   MOVEL     FUD(I)        FUDRIG
029000980304     C*
029100980304     C     FUDFUN        IFEQ      '24'
029200980304     C     W             IFGT      6
029300980304     C                   MOVEL     'S'           FUDATT
029400980304     C                   ELSE
029500980304     C                   MOVEL     *BLANKS       FUDATT
029600980304     C                   END
029700980304     C                   MOVEL     FUDRIG        FUD(I)
029800980304     C                   END
029900980304     C                   END
030000980304     C*
030100980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
030200980304     C     W             DIV       6             I
030300980304     C                   MVR                     Z
030400980304     C*
030500980304     C     Z             IFNE      0
030600980304     C     I             ADD       1             Z
030700980304     C                   ELSE
030800980304     C                   Z-ADD     I             Z                 5 0
030900980304     C                   END
031000980304     C*
031100980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
031200980304     C                   ADD       1             FUNGRU
031300980304     C     FUNGRU        IFGT      Z
031400980304     C     FUNGRU        ORLT      1
031500980304     C                   Z-ADD     1             FUNGRU
031600980304     C                   END
031700980304     C*
031800980304     C* PRIMO GRUPPO DI 6 FUNZIONI
031900980304     C     FUNGRU        IFEQ      1
032000980304     C                   MOVEL     FUV(1)        FUN001
032100980304     C                   MOVEL     FUV(2)        FUN002
032200980304     C                   MOVEL     FUV(3)        FUN003
032300980304     C                   MOVEL     FUV(4)        FUN004
032400980304     C                   MOVEL     FUV(5)        FUN005
032500980304     C                   MOVEL     FUV(6)        FUN006
032600980304     C                   END
032700980304     C*
032800980304     C* SECONDO GRUPPO DI 6 FUNZIONI
032900980304     C     FUNGRU        IFEQ      2
033000980304     C                   MOVEL     FUV(7)        FUN001
033100980304     C                   MOVEL     FUV(8)        FUN002
033200980304     C                   MOVEL     FUV(9)        FUN003
033300980304     C                   MOVEL     FUV(10)       FUN004
033400980304     C                   MOVEL     FUV(11)       FUN005
033500980304     C                   MOVEL     FUV(12)       FUN006
033600980304     C                   END
033700980304     C*
033800980304     C* TERZO GRUPPO DI 6 FUNZIONI
033900980304     C     FUNGRU        IFEQ      3
034000980304     C                   MOVEL     FUV(13)       FUN001
034100980304     C                   MOVEL     FUV(14)       FUN002
034200980304     C                   MOVEL     FUV(15)       FUN003
034300980304     C                   MOVEL     FUV(16)       FUN004
034400980304     C                   MOVEL     FUV(17)       FUN005
034500980304     C                   MOVEL     FUV(18)       FUN006
034600980304     C                   END
034700980304     C*
034800980304     C* QUARTO GRUPPO DI 6 FUNZIONI
034900980304     C     FUNGRU        IFEQ      4
035000980304     C                   MOVEL     FUV(19)       FUN001
035100980304     C                   MOVEL     FUV(20)       FUN002
035200980304     C                   MOVEL     FUV(21)       FUN003
035300980304     C                   MOVEL     FUV(22)       FUN004
035400980304     C                   MOVEL     *BLANKS       FUN005
035500980304     C                   MOVEL     *BLANKS       FUN006
035600980304     C                   END
035700980304     C*
035800980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
035900980304     C                   MOVEL     *BLANKS       FUN007
036000980304     C                   MOVEL     *BLANKS       FUN008
036100980304     C*
036200980304     C     1             DO        24            I
036300980304     C                   MOVEL     FUD(I)        FUDRIG
036400980304     C     FUDFUN        IFEQ      '24'
036500980304     C     FUDATT        ANDEQ     'S'
036600980304     C                   MOVEL     FUDDES        FUN008
036700980304     C                   END
036800980304     C                   END
036900980304     C*
037000980304     C                   ENDSR
037100980304     C*------------------------------------------------------------------------*
037200980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
037300980304     C*------------------------------------------------------------------------*
037400980304     C     FUNCON        BEGSR
037500980304     C*
037600980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
037700980304     C                   EXSR      FUNVAR
037800980304     C*
037900980304     C* PULISCO CAMPI DI LAVORO
038000980304     C                   Z-ADD     0             I                 5 0
038100980304     C                   Z-ADD     0             W                 5 0
038200980304     C*
038300980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
038400980304     C     FUNKEY        IFNE      *BLANKS
038500980304     C     1             DO        24            I
038600980304     C                   MOVEL     FUD(I)        FUDRIG
038700980304     C*
038800980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
038900980304     C     FUNKEY        IFEQ      FUDFUN
039000980304     C     FUDATT        ANDEQ     'S'
039100980304     C                   Z-ADD     I             W
039200980304     C                   END
039300980304     C                   END
039400980304     C*
039500980304     C* FUNZIONE NON ATTIVA (ERRORE)
039600980304     C     W             IFEQ      0
039700980304     C*
039800980304     C* EMISSIONE MESSAGGIO DI ERRORE
039900980304     C                   MOVEL     MSG(1)        DSMSMS
040000980304     C*
040100980304     C                   END
040200980304     C                   END
040300980304     C*
040400980304     C* FUNZIONE ATTIVA (PROCEDI ...)
040500980304     C     W             IFNE      0
040600980304     C     FUNKEY        OREQ      *BLANKS
040700980304     C     FUNKEY        IFEQ      *BLANKS
040800980304     C                   EXSR      ENTGES
040900980304     C                   END
041000980304     C     FUNKEY        IFEQ      '03'
041100980304     C                   EXSR      F03GES
041200980304     C                   END
041300980304     C     FUNKEY        IFEQ      '12'
041400980304     C                   EXSR      F12GES
041500980304     C                   END
041600980304     C                   END
041700980304     C*
041800980304     C                   ENDSR
041900980304     C*------------------------------------------------------------------------*
042000980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
042100980304     C*------------------------------------------------------------------------*
042200980304     C     AZZVID        BEGSR
042300980304     C*
042400980304     C                   ENDSR
042500980304     C*------------------------------------------------------------------------*
042600980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
042700980304     C*------------------------------------------------------------------------*
042800980304     C     FUNVAR        BEGSR
042900980304     C*
043000980304     C* INIZIALIZZA VARIABILE -FUNKEY-
043100980304     C*
043200980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
043300980304     C     KEY           IFEQ      ENTER
043400980304     C                   MOVEL     *BLANKS       FUNKEY
043500980304     C                   END
043600980304     C     KEY           IFEQ      F1
043700980304     C                   MOVEL     '01'          FUNKEY
043800980304     C                   END
043900980304     C     KEY           IFEQ      F2
044000980304     C                   MOVEL     '02'          FUNKEY
044100980304     C                   END
044200980304     C     KEY           IFEQ      F3
044300980304     C                   MOVEL     '03'          FUNKEY
044400980304     C                   END
044500980304     C     KEY           IFEQ      F4
044600980304     C                   MOVEL     '04'          FUNKEY
044700980304     C                   END
044800980304     C     KEY           IFEQ      F5
044900980304     C                   MOVEL     '05'          FUNKEY
045000980304     C                   END
045100980304     C     KEY           IFEQ      F6
045200980304     C                   MOVEL     '06'          FUNKEY
045300980304     C                   END
045400980304     C     KEY           IFEQ      F7
045500980304     C                   MOVEL     '07'          FUNKEY
045600980304     C                   END
045700980304     C     KEY           IFEQ      F8
045800980304     C                   MOVEL     '08'          FUNKEY
045900980304     C                   END
046000980304     C     KEY           IFEQ      F9
046100980304     C                   MOVEL     '09'          FUNKEY
046200980304     C                   END
046300980304     C     KEY           IFEQ      F10
046400980304     C                   MOVEL     '10'          FUNKEY
046500980304     C                   END
046600980304     C     KEY           IFEQ      F11
046700980304     C                   MOVEL     '11'          FUNKEY
046800980304     C                   END
046900980304     C     KEY           IFEQ      F12
047000980304     C                   MOVEL     '12'          FUNKEY
047100980304     C                   END
047200980304     C     KEY           IFEQ      F13
047300980304     C                   MOVEL     '13'          FUNKEY
047400980304     C                   END
047500980304     C     KEY           IFEQ      F14
047600980304     C                   MOVEL     '14'          FUNKEY
047700980304     C                   END
047800980304     C     KEY           IFEQ      F15
047900980304     C                   MOVEL     '15'          FUNKEY
048000980304     C                   END
048100980304     C     KEY           IFEQ      F16
048200980304     C                   MOVEL     '16'          FUNKEY
048300980304     C                   END
048400980304     C     KEY           IFEQ      F17
048500980304     C                   MOVEL     '17'          FUNKEY
048600980304     C                   END
048700980304     C     KEY           IFEQ      F18
048800980304     C                   MOVEL     '18'          FUNKEY
048900980304     C                   END
049000980304     C     KEY           IFEQ      F19
049100980304     C                   MOVEL     '19'          FUNKEY
049200980304     C                   END
049300980304     C     KEY           IFEQ      F20
049400980304     C                   MOVEL     '20'          FUNKEY
049500980304     C                   END
049600980304     C     KEY           IFEQ      F21
049700980304     C                   MOVEL     '21'          FUNKEY
049800980304     C                   END
049900980304     C     KEY           IFEQ      F22
050000980304     C                   MOVEL     '22'          FUNKEY
050100980304     C                   END
050200980304     C     KEY           IFEQ      F23
050300980304     C                   MOVEL     '23'          FUNKEY
050400980304     C                   END
050500980304     C     KEY           IFEQ      F24
050600980304     C                   MOVEL     '24'          FUNKEY
050700980304     C                   END
050800980304     C     KEY           IFEQ      ROLLUP
050900980304     C                   MOVEL     'RU'          FUNKEY
051000980304     C                   END
051100980304     C     KEY           IFEQ      HELP
051200980304     C                   MOVEL     'HP'          FUNKEY
051300980304     C                   END
051400980304     C                   ENDSR
051500980304     C*------------------------------------------------------------------------*
051600980304     C* NOTE COSTRUZIONE SCHIERE
051700980304     C*------------------------------------------------------------------------*
051800980304     C*
051900980304     C* -MSG-MESSAGGI
052000980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
052100980304     C* (NON TOCCARE MSG,1 E MSG,2)
052200980304     C*
052300980304     C* -CMD-COMANDI
052400980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
052500980304     C*
052600980304     C* -OPD-OPZIONI
052700980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
052800980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
052900980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
053000980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
053100980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
053200980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
053300980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
053400980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
053500980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
053600980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
053700980304     C*
053800980304     C* -FUD-FUNZIONI
053900980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
054000980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
054100980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
054200980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
054300980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
054400980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
054500980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
054600980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
054700980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
054800980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
054900980304     o*-------------------------------------------------------------------------
055000980304** msg - messaggi
055100080612UBLBLSPE Funzione non attiva
055200080612UBLBLSPE 2
055300080612UBLBLSPE 3
055400080612UBLBLSPE 4
055500080612UBLBLSPE 5
055600080612UBLBLSPE 6
055700080612UBLBLSPE 7
055800080612UBLBLSPE 8
055900080612UBLBLSPE 9
056000080612UBLBLSPE 10
056100980304** FUD - FUNZIONI
056200980304                   -        (S/N)=N                        funz.01¬
056300980304                   -        (S/N)=N                        funz.02¬
056400980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
056500980304                   -        (S/N)=N                        funz.04¬
056600980304                   -        (S/N)=N                        funz.05¬
056700980304                   -        (S/N)=N                        funz.06¬
056800980304                   -        (S/N)=N                        funz.07¬
056900980304                   -        (S/N)=N                        funz.08¬
057000980304                   -        (S/N)=N                        funz.09¬
057100980304                   -        (S/N)=N                        funz.10¬
057200980304                   -        (S/N)=N                        funz.11¬
057300980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
057400980304                   -        (S/N)=N                        funz.13¬
057500980304                   -        (S/N)=N                        funz.14¬
057600980304                   -        (S/N)=N                        funz.15¬
057700980304                   -        (S/N)=N                        funz.16¬
057800980304                   -        (S/N)=N                        funz.17¬
057900980304                   -        (S/N)=N                        funz.18¬
058000980304                   -        (S/N)=N                        funz.19¬
058100980304                   -        (S/N)=N                        funz.20¬
058200980304                   -        (S/N)=N                        funz.21¬
058300980304                   -        (S/N)=N                        funz.22¬
058400980304                   -        (S/N)=N                        funz.23¬
058500980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
