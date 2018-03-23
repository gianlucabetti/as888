000100080612     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('TEST_FG')
000200980304     H*------------------------------------------------------------------------*
000300080612     H* UBLBLSTS - TEST
000400980304     H*------------------------------------------------------------------------*
000500980304     H DECEDIT('0,') DATEDIT(*DMY.)
000600980304     F*------------------------------------------------------------------------*
000700980304     F* DATA BASE
000800980304     F*------------------------------------------------------------------------*
000900080612     FUBLBLSPETDCF   E             WORKSTN INFDS(DEVDS1)
001000080612     F                                     SFILE(UBLSDMS:MSGULT)
001100980304     D*------------------------------------------------------------------------*
001200980304     D* SCHIERE
001300980304     D*------------------------------------------------------------------------*
001400980304     d MSG             S             80    DIM(10) CTDATA PERRCD(1)
001500980304     D*------------
001600980304     D* DESCRIZIONI TASTI FUNZIONALI
001700980304     D*------------
001800980304     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001900980304     D*------------
002000980304     D* FUNZIONI DA CARICARE A VIDEO
002100980304     D*------------
002200980304     D FUV             S             19    DIM(24)
002300980304     D*------------------------------------------------------------------------*
002400980304     D* INPUT
002500980304     D*------------------------------------------------------------------------*
002600980304     D*------------
002700980304     D* RIDEFINIZIONE FUNZIONI VIDEO
002800980304     D*------------
002900980304     D FUDDS           DS
003000980304     D  FUDDE1                 1      1
003100980304     D  FUDFUN                 2      3
003200980304     D  FUDDE2                 4     19
003300980304     D  FUDFI0                20     34
003400980304     D  FUDATT                35     35
003500980304     D  FUDFI1                36     80
003600980304     D  FUDDES                 1     19
003700980304     D  FUDRIG                 1     80
003800980304     D*------------
003900980304     D* DS GESTIONE TASTI FUNZIONALI
004000980304     D*------------
004100980304     D DSKEY         E DS
004200980304     D DEVDS1          DS
004300980304     D  KEY                  369    369
004400980304     D*------------
004500980304     D* DS "XMSG" - GESTIONE MESSAGGI
004600980304     D*------------
004700980304     D DSMSG         E DS
004800980304     D*------------------
004900980304     D* ARCHITETTURA
005000980304     D*------------------
005100980304     D KPJBA         E DS
005200080612     D*------------------
005300080612     D* LINKING A DEFINIZIONI ESTERNE
005400080612     D*------------------
005500080612     D/COPY GAITRASRC/SRCPROTOPR,UBLBLSPE
005600100107     D/COPY GAITRASRC/SRCPROTOPI,UBLBLSPE
005700980304     C*------------------------------------------------------------------------*
005800980304     C* MAIN LINES
005900980304     C*------------------------------------------------------------------------*
006000980304     C*
006100980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
006200980304DO  1C     F03L00        DOUEQ     '1'
006300980304     C     F12L00        OREQ      '1'
006400980304     C     FINL00        OREQ      '1'
006500980304     C*
006600980304     C* EMETTE FORMATO VIDEO
006700980304     C                   EXSR      BUTFMR
006800980304     C*
006900980304     C* GESTIONE TASTI FUNZIONALI
007000980304     C                   EXSR      FUNCON
007100980304E   1C                   ENDDO
007200980304     C*
007300980304     C                   SETON                                        LR
007400980304     C*------------------------------------------------------------------------*
007500980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007600980304     C*------------------------------------------------------------------------*
007700980304     C     BUTFMR        BEGSR
007800980304     C*
007900980304     C* EMETTE LA TESTATA
008000080612     C                   WRITE     UBLSD01
008100980304     C*
008200980304     C* SPEDISCE EVENTUALE MESSAGGIO
008300980304     C                   EXSR      SNDMSG
008400980304     C*
008500980304     C* EMETTE LE FUNZIONI
008600080612     C                   WRITE     UBLSD02
008700980304     C*
008800980304     C* EMETTE IL VIDEO
008900080612     C                   EXFMT     UBLSD03
009000980304     C*
009100980304     C* REIMPOSTA FLAG DI ERRORE
009200980304     C                   MOVEL     '0'           ERRL00
009300980304     C*
009400980304     C                   ENDSR
009500980304     C*------------------------------------------------------------------------*
009600980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
009700980304     C*------------------------------------------------------------------------*
009800980304     C     SNDMSG        BEGSR
009900980304     C*
010000980304     C* PULISCE IL SFILE MESSAGGI
010100980304     C                   MOVEL     '1'           *IN40
010200080612     C                   WRITE     UBLSDMC
010300980304     C                   MOVEL     '0'           *IN40
010400980304     C*
010500980304     C* VISUALIZZA IL MESSAGGIO
010600980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
010700980304     C                   CALL      'XMSG'        XMSGPA
010800980304IF  1C     DSMSER        IFEQ      *ZEROS
010900980304     C                   MOVEL     DSMSKE        MSGKEY
011000980304E   1C                   ENDIF
011100980304     C                   Z-ADD     1             MSGULT
011200080612     C                   WRITE     UBLSDMS
011300080612     C                   WRITE     UBLSDMC
011400980304     C*
011500980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
011600980304     C                   MOVEL     *BLANKS       DSMSMS
011700980304     C*
011800980304     C                   ENDSR
011900980304     C*------------------------------------------------------------------------*
012000980304     C* ENTGES - GESTIONE ENTER -> INVIO
012100980304     C*------------------------------------------------------------------------*
012200980304     C     ENTGES        BEGSR
012300080612     C*
012400100107     C                   eval      PInAnno     = VIDAASC
012500100107     C                   eval      PInLineaPar = VIDLNPC
012600100107     C                   eval      PInSerie    = VIDNRSC
012700100107     C                   eval      PInNumSped  = VIDNSPC
012800080612     C*
012900080612     C* Chiamata metodo GetLblTyp
013000100107     C                   eval      VIDESITO1=%char(UBLBLSPE_GetLblTyp(
013100100107     C                                                pInAnno
013200100107     C                                               :pInLineaPar
013300100107     C                                               :pInSerie
013400100107     C                                               :pInNumSped
013500100107     C                                               :pOutLblTyp
013600100107     C                                               :pOutAnnoBO
013700100107     C                                               :pOutLineaParBO
013800100107     C                                               :pOutSerieBO
013900100107     C                                               :pOutNumSpedBO
014000100107     C                                               :pOutDcmBO
014100100107     C                                               :pOutCcaBO
014200100107     C                                               :pOutRblBO))
014300080612     C*
014400080612     C* Chiamata metodo GetLastChild
014500100107     C                   eval      VIDESITO2=%char(UBLBLSPE_GetLastChild(
014600100107     C                                                   pInAnno
014700100107     C                                                  :pInLineaPar
014800100107     C                                                  :pInSerie
014900100107     C                                                  :pInNumSped
015000100107     C                                                  :pOutAnnoFI
015100100107     C                                                  :pOutLineaParFI
015200100107     C                                                  :pOutSerieFI
015300100107     C                                                  :pOutNumSpedFI
015400100107     C                                                  :pOutDcmFI
015500100107     C                                                  :pOutCcaFI))
015600100107     C*
015700100107     C* Chiamata metodo GetFirstChild
015800100107     C                   eval      VIDESITO3=%char(UBLBLSPE_GetFirstChild(
015900100107     C                                                   pInAnno
016000100107     C                                                  :pInLineaPar
016100100107     C                                                  :pInSerie
016200100107     C                                                  :pInNumSped
016300100107     C                                                  :pOutAnnoFFI
016400100107     C                                                  :pOutLineaPaFFI
016500100107     C                                                  :pOutSerieFFI
016600100107     C                                                  :pOutNumSpedFFI
016700100107     C                                                  :pOutDcmFFI
016800100107     C                                                  :pOutCcaFFI))
016900130919     C*
017000130919     C* Chiamata metodo GetMother
017100130919     C                   eval      VIDESITO4=%char(UBLBLSPE_GetMother(
017200130919     C                                                   pInAnno
017300130919     C                                                  :pInLineaPar
017400130919     C                                                  :pInSerie
017500130919     C                                                  :pInNumSped
017600130919     C                                                  :pOutAnnoMAM
017700130919     C                                                  :pOutLineaPaMAM
017800130919     C                                                  :pOutSerieMAM
017900130919     C                                                  :pOutNumSpedMAM
018000130919     C                                                  :pOutDcmMAM
018100130919     C                                                  :pOutCcaMAM))
018200080612     C*
018300100107     C                   eval      VIDTIPL  = pOutLblTyp
018400100107     C                   eval      VIDAASO  = pOutAnnoBO
018500100107     C                   eval      VIDLNPO  = pOutLineaParBO
018600100107     C                   eval      VIDNRSO  = pOutSerieBO
018700100107     C                   eval      VIDNSPO  = pOutNumSpedBO
018800100107     C                   eval      VIDDCMO  = pOutDcmBO
018900100107     C                   eval      VIDCCAO  = pOutCcaBO
019000100107     C                   eval      VIDRBLO  = pOutRblBO
019100100107     C                   eval      VIDAASF  = pOutAnnoFI
019200100107     C                   eval      VIDLNPF  = pOutLineaParFI
019300100107     C                   eval      VIDNRSF  = pOutSerieFI
019400100107     C                   eval      VIDNSPF  = pOutNumSpedFI
019500100107     C                   eval      VIDDCMF  = pOutDcmFI
019600100107     C                   eval      VIDCCAF  = pOutCcaFI
019700100107     C                   eval      VIDAASFF = pOutAnnoFFI
019800100107     C                   eval      VIDLNPFF = pOutLineaPaFFI
019900100107     C                   eval      VIDNRSFF = pOutSerieFFI
020000100107     C                   eval      VIDNSPFF = pOutNumSpedFFI
020100100107     C                   eval      VIDDCMFF = pOutDcmFFI
020200100107     C                   eval      VIDCCAFF = pOutCcaFFI
020300130919     C                   eval      VIDAASM  = pOutAnnoMAM
020400130919     C                   eval      VIDLNPM  = pOutLineaPaMAM
020500130919     C                   eval      VIDNRSM  = pOutSerieMAM
020600130919     C                   eval      VIDNSPM  = pOutNumSpedMAM
020700130919     C                   eval      VIDDCMM  = pOutDcmMAM
020800130919     C                   eval      VIDCCAM  = pOutCcaMAM
020900080612     C*
021000980304     C                   ENDSR
021100980304     C*------------------------------------------------------------------------*
021200980304     C* F03GES - GESTIONE F03 -> FINE
021300980304     C*------------------------------------------------------------------------*
021400980304     C     F03GES        BEGSR
021500980304     C*
021600980304     C* IMPOSTA "PREMUTO F03"
021700980304     C                   MOVEL     '1'           F03L00
021800980304     C*
021900980304     C                   ENDSR
022000980304     C*------------------------------------------------------------------------*
022100980304     C* F12GES - GESTIONE F12 -> RITORNO
022200980304     C*------------------------------------------------------------------------*
022300980304     C     F12GES        BEGSR
022400980304     C*
022500980304     C* IMPOSTA "PREMUTO F12"
022600980304     C                   MOVEL     '1'           F12L00
022700980304     C*
022800980304     C                   ENDSR
022900980304     C*------------------------------------------------------------------------*
023000980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023100980304     C*------------------------------------------------------------------------*
023200980304     C     F24GES        BEGSR
023300980304     C*
023400980304     C                   EXSR      FUNGES
023500980304     C*
023600980304     C                   ENDSR
023700980304     C*------------------------------------------------------------------------*
023800980304     C* *INZSR - OPERAZIONI INIZIALI
023900980304     C*------------------------------------------------------------------------*
024000980304     C     *INZSR        BEGSR
024100980304     C*---
024200980304     C* RICEVIMENTO PARAMETRI
024300980304     C*---
024400980304     C     *ENTRY        PLIST
024500980304     C                   PARM                    KPJBA
024600980304     C*
024700980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
024800980304     C     XMSGPA        PLIST
024900980304     C                   PARM                    DSMSMS
025000980304     C                   PARM                    DSMSPG
025100980304     C                   PARM                    DSMSKE
025200980304     C                   PARM                    DSMSER
025300980304     C*
025400980304     C* IMPOSTA LA DS -TASTI PREMUTI-
025500980304     C                   CALL      'XKEY'
025600980304     C                   PARM                    DSKEY
025700980304     C*---
025800980304     C* VARIABILI NON RIFERITE AL DATA BASE
025900980304     C*---
026000980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
026100980304     C*---
026200980304     C* VARIABILI DI CONTROLLO
026300980304     C*---
026400980304     C                   MOVEL     '0'           FINL00            1
026500980304     C                   MOVEL     '0'           F03L00            1
026600980304     C                   MOVEL     '0'           F12L00            1
026700980304     C                   MOVEL     '0'           ERRL00            1
026800980304     C*
026900980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
027000080612     C                   MOVEL     'UBLBLSPE'    DSMSPG
027100080612     C                   MOVEL     'UBLBLSPE'    MSGQUE
027200980304     C                   MOVEL     *BLANKS       MSGKEY
027300980304     C                   MOVEL     *BLANKS       DSMSMS
027400980304     C*
027500980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
027600980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
027700980304     C                   EXSR      FUNGES
027800980304     C*
027900980304     C                   ENDSR
028000980304     C*------------------------------------------------------------------------*
028100980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
028200980304     C*------------------------------------------------------------------------*
028300980304     C     FUNGES        BEGSR
028400980304     C*
028500980304     C* PULISCO CAMPI DI LAVORO
028600980304     C                   Z-ADD     0             W                 5 0
028700980304     C*
028800980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
028900980304     C* E CARICO TABELLA FUNZIONI ATTIVE
029000980304     C     1             DO        24            I
029100980304     C                   MOVEL     FUD(I)        FUDRIG
029200980304     C*
029300980304     C* PULIZIA FUNZIONI PRECEDENTI
029400980304     C                   MOVEL     *BLANKS       FUV(I)
029500980304     C*
029600980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
029700980304     C* (SALTA F24)
029800980304     C     FUDATT        IFEQ      'S'
029900980304     C     FUDFUN        ANDNE     '24'
030000980304     C                   ADD       1             W
030100980304     C                   MOVEL     FUDDES        FUV(W)
030200980304     C                   END
030300980304     C                   END
030400980304     C*
030500980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
030600980304     C* ALTRIMENTI DISATTIVO F24
030700980304     C     1             DO        24            I
030800980304     C                   MOVEL     FUD(I)        FUDRIG
030900980304     C*
031000980304     C     FUDFUN        IFEQ      '24'
031100980304     C     W             IFGT      6
031200980304     C                   MOVEL     'S'           FUDATT
031300980304     C                   ELSE
031400980304     C                   MOVEL     *BLANKS       FUDATT
031500980304     C                   END
031600980304     C                   MOVEL     FUDRIG        FUD(I)
031700980304     C                   END
031800980304     C                   END
031900980304     C*
032000980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
032100980304     C     W             DIV       6             I
032200980304     C                   MVR                     Z
032300980304     C*
032400980304     C     Z             IFNE      0
032500980304     C     I             ADD       1             Z
032600980304     C                   ELSE
032700980304     C                   Z-ADD     I             Z                 5 0
032800980304     C                   END
032900980304     C*
033000980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
033100980304     C                   ADD       1             FUNGRU
033200980304     C     FUNGRU        IFGT      Z
033300980304     C     FUNGRU        ORLT      1
033400980304     C                   Z-ADD     1             FUNGRU
033500980304     C                   END
033600980304     C*
033700980304     C* PRIMO GRUPPO DI 6 FUNZIONI
033800980304     C     FUNGRU        IFEQ      1
033900980304     C                   MOVEL     FUV(1)        FUN001
034000980304     C                   MOVEL     FUV(2)        FUN002
034100980304     C                   MOVEL     FUV(3)        FUN003
034200980304     C                   MOVEL     FUV(4)        FUN004
034300980304     C                   MOVEL     FUV(5)        FUN005
034400980304     C                   MOVEL     FUV(6)        FUN006
034500980304     C                   END
034600980304     C*
034700980304     C* SECONDO GRUPPO DI 6 FUNZIONI
034800980304     C     FUNGRU        IFEQ      2
034900980304     C                   MOVEL     FUV(7)        FUN001
035000980304     C                   MOVEL     FUV(8)        FUN002
035100980304     C                   MOVEL     FUV(9)        FUN003
035200980304     C                   MOVEL     FUV(10)       FUN004
035300980304     C                   MOVEL     FUV(11)       FUN005
035400980304     C                   MOVEL     FUV(12)       FUN006
035500980304     C                   END
035600980304     C*
035700980304     C* TERZO GRUPPO DI 6 FUNZIONI
035800980304     C     FUNGRU        IFEQ      3
035900980304     C                   MOVEL     FUV(13)       FUN001
036000980304     C                   MOVEL     FUV(14)       FUN002
036100980304     C                   MOVEL     FUV(15)       FUN003
036200980304     C                   MOVEL     FUV(16)       FUN004
036300980304     C                   MOVEL     FUV(17)       FUN005
036400980304     C                   MOVEL     FUV(18)       FUN006
036500980304     C                   END
036600980304     C*
036700980304     C* QUARTO GRUPPO DI 6 FUNZIONI
036800980304     C     FUNGRU        IFEQ      4
036900980304     C                   MOVEL     FUV(19)       FUN001
037000980304     C                   MOVEL     FUV(20)       FUN002
037100980304     C                   MOVEL     FUV(21)       FUN003
037200980304     C                   MOVEL     FUV(22)       FUN004
037300980304     C                   MOVEL     *BLANKS       FUN005
037400980304     C                   MOVEL     *BLANKS       FUN006
037500980304     C                   END
037600980304     C*
037700980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
037800980304     C                   MOVEL     *BLANKS       FUN007
037900980304     C                   MOVEL     *BLANKS       FUN008
038000980304     C*
038100980304     C     1             DO        24            I
038200980304     C                   MOVEL     FUD(I)        FUDRIG
038300980304     C     FUDFUN        IFEQ      '24'
038400980304     C     FUDATT        ANDEQ     'S'
038500980304     C                   MOVEL     FUDDES        FUN008
038600980304     C                   END
038700980304     C                   END
038800980304     C*
038900980304     C                   ENDSR
039000980304     C*------------------------------------------------------------------------*
039100980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
039200980304     C*------------------------------------------------------------------------*
039300980304     C     FUNCON        BEGSR
039400980304     C*
039500980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
039600980304     C                   EXSR      FUNVAR
039700980304     C*
039800980304     C* PULISCO CAMPI DI LAVORO
039900980304     C                   Z-ADD     0             I                 5 0
040000980304     C                   Z-ADD     0             W                 5 0
040100980304     C*
040200980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
040300980304     C     FUNKEY        IFNE      *BLANKS
040400980304     C     1             DO        24            I
040500980304     C                   MOVEL     FUD(I)        FUDRIG
040600980304     C*
040700980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
040800980304     C     FUNKEY        IFEQ      FUDFUN
040900980304     C     FUDATT        ANDEQ     'S'
041000980304     C                   Z-ADD     I             W
041100980304     C                   END
041200980304     C                   END
041300980304     C*
041400980304     C* FUNZIONE NON ATTIVA (ERRORE)
041500980304     C     W             IFEQ      0
041600980304     C*
041700980304     C* EMISSIONE MESSAGGIO DI ERRORE
041800980304     C                   MOVEL     MSG(1)        DSMSMS
041900980304     C*
042000980304     C                   END
042100980304     C                   END
042200980304     C*
042300980304     C* FUNZIONE ATTIVA (PROCEDI ...)
042400980304     C     W             IFNE      0
042500980304     C     FUNKEY        OREQ      *BLANKS
042600980304     C     FUNKEY        IFEQ      *BLANKS
042700980304     C                   EXSR      ENTGES
042800980304     C                   END
042900980304     C     FUNKEY        IFEQ      '03'
043000980304     C                   EXSR      F03GES
043100980304     C                   END
043200980304     C     FUNKEY        IFEQ      '12'
043300980304     C                   EXSR      F12GES
043400980304     C                   END
043500980304     C                   END
043600980304     C*
043700980304     C                   ENDSR
043800980304     C*------------------------------------------------------------------------*
043900980304     C* AZZVID - AZZERA I CAMPI DEL VIDEO
044000980304     C*------------------------------------------------------------------------*
044100980304     C     AZZVID        BEGSR
044200980304     C*
044300980304     C                   ENDSR
044400980304     C*------------------------------------------------------------------------*
044500980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
044600980304     C*------------------------------------------------------------------------*
044700980304     C     FUNVAR        BEGSR
044800980304     C*
044900980304     C* INIZIALIZZA VARIABILE -FUNKEY-
045000980304     C*
045100980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
045200980304     C     KEY           IFEQ      ENTER
045300980304     C                   MOVEL     *BLANKS       FUNKEY
045400980304     C                   END
045500980304     C     KEY           IFEQ      F1
045600980304     C                   MOVEL     '01'          FUNKEY
045700980304     C                   END
045800980304     C     KEY           IFEQ      F2
045900980304     C                   MOVEL     '02'          FUNKEY
046000980304     C                   END
046100980304     C     KEY           IFEQ      F3
046200980304     C                   MOVEL     '03'          FUNKEY
046300980304     C                   END
046400980304     C     KEY           IFEQ      F4
046500980304     C                   MOVEL     '04'          FUNKEY
046600980304     C                   END
046700980304     C     KEY           IFEQ      F5
046800980304     C                   MOVEL     '05'          FUNKEY
046900980304     C                   END
047000980304     C     KEY           IFEQ      F6
047100980304     C                   MOVEL     '06'          FUNKEY
047200980304     C                   END
047300980304     C     KEY           IFEQ      F7
047400980304     C                   MOVEL     '07'          FUNKEY
047500980304     C                   END
047600980304     C     KEY           IFEQ      F8
047700980304     C                   MOVEL     '08'          FUNKEY
047800980304     C                   END
047900980304     C     KEY           IFEQ      F9
048000980304     C                   MOVEL     '09'          FUNKEY
048100980304     C                   END
048200980304     C     KEY           IFEQ      F10
048300980304     C                   MOVEL     '10'          FUNKEY
048400980304     C                   END
048500980304     C     KEY           IFEQ      F11
048600980304     C                   MOVEL     '11'          FUNKEY
048700980304     C                   END
048800980304     C     KEY           IFEQ      F12
048900980304     C                   MOVEL     '12'          FUNKEY
049000980304     C                   END
049100980304     C     KEY           IFEQ      F13
049200980304     C                   MOVEL     '13'          FUNKEY
049300980304     C                   END
049400980304     C     KEY           IFEQ      F14
049500980304     C                   MOVEL     '14'          FUNKEY
049600980304     C                   END
049700980304     C     KEY           IFEQ      F15
049800980304     C                   MOVEL     '15'          FUNKEY
049900980304     C                   END
050000980304     C     KEY           IFEQ      F16
050100980304     C                   MOVEL     '16'          FUNKEY
050200980304     C                   END
050300980304     C     KEY           IFEQ      F17
050400980304     C                   MOVEL     '17'          FUNKEY
050500980304     C                   END
050600980304     C     KEY           IFEQ      F18
050700980304     C                   MOVEL     '18'          FUNKEY
050800980304     C                   END
050900980304     C     KEY           IFEQ      F19
051000980304     C                   MOVEL     '19'          FUNKEY
051100980304     C                   END
051200980304     C     KEY           IFEQ      F20
051300980304     C                   MOVEL     '20'          FUNKEY
051400980304     C                   END
051500980304     C     KEY           IFEQ      F21
051600980304     C                   MOVEL     '21'          FUNKEY
051700980304     C                   END
051800980304     C     KEY           IFEQ      F22
051900980304     C                   MOVEL     '22'          FUNKEY
052000980304     C                   END
052100980304     C     KEY           IFEQ      F23
052200980304     C                   MOVEL     '23'          FUNKEY
052300980304     C                   END
052400980304     C     KEY           IFEQ      F24
052500980304     C                   MOVEL     '24'          FUNKEY
052600980304     C                   END
052700980304     C     KEY           IFEQ      ROLLUP
052800980304     C                   MOVEL     'RU'          FUNKEY
052900980304     C                   END
053000980304     C     KEY           IFEQ      HELP
053100980304     C                   MOVEL     'HP'          FUNKEY
053200980304     C                   END
053300980304     C                   ENDSR
053400980304     C*------------------------------------------------------------------------*
053500980304     C* NOTE COSTRUZIONE SCHIERE
053600980304     C*------------------------------------------------------------------------*
053700980304     C*
053800980304     C* -MSG-MESSAGGI
053900980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
054000980304     C* (NON TOCCARE MSG,1 E MSG,2)
054100980304     C*
054200980304     C* -CMD-COMANDI
054300980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
054400980304     C*
054500980304     C* -OPD-OPZIONI
054600980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
054700980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
054800980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
054900980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
055000980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
055100980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
055200980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
055300980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
055400980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
055500980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
055600980304     C*
055700980304     C* -FUD-FUNZIONI
055800980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
055900980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
056000980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
056100980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
056200980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
056300980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
056400980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
056500980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
056600980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
056700980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
056800980304     o*-------------------------------------------------------------------------
056900980304** msg - messaggi
057000080612UBLBLSPE Funzione non attiva
057100080612UBLBLSPE 2
057200080612UBLBLSPE 3
057300080612UBLBLSPE 4
057400080612UBLBLSPE 5
057500080612UBLBLSPE 6
057600080612UBLBLSPE 7
057700080612UBLBLSPE 8
057800080612UBLBLSPE 9
057900080612UBLBLSPE 10
058000980304** FUD - FUNZIONI
058100980304                   -        (S/N)=N                        funz.01¬
058200980304                   -        (S/N)=N                        funz.02¬
058300980304F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
058400980304                   -        (S/N)=N                        funz.04¬
058500980304                   -        (S/N)=N                        funz.05¬
058600980304                   -        (S/N)=N                        funz.06¬
058700980304                   -        (S/N)=N                        funz.07¬
058800980304                   -        (S/N)=N                        funz.08¬
058900980304                   -        (S/N)=N                        funz.09¬
059000980304                   -        (S/N)=N                        funz.10¬
059100980304                   -        (S/N)=N                        funz.11¬
059200980304F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
059300980304                   -        (S/N)=N                        funz.13¬
059400980304                   -        (S/N)=N                        funz.14¬
059500980304                   -        (S/N)=N                        funz.15¬
059600980304                   -        (S/N)=N                        funz.16¬
059700980304                   -        (S/N)=N                        funz.17¬
059800980304                   -        (S/N)=N                        funz.18¬
059900980304                   -        (S/N)=N                        funz.19¬
060000980304                   -        (S/N)=N                        funz.20¬
060100980304                   -        (S/N)=N                        funz.21¬
060200980304                   -        (S/N)=N                        funz.22¬
060300980304                   -        (S/N)=N                        funz.23¬
060400980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
