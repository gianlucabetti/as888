000100980304     H*------------------------------------------------------------------------*
000200041220     H* REPERIMENTO SALDI CLIENTE - TEST
000300980304     H*------------------------------------------------------------------------*
000400980304     H DECEDIT('0,') DATEDIT(*DMY.)
000500980304     F*------------------------------------------------------------------------*
000600980304     F* DATA BASE
000700980304     F*------------------------------------------------------------------------*
000800080718     FTRULKPJD  CF   E             WORKSTN INFDS(DEVDS1)
000900080718     F                                     SFILE(TRKUDMS:MSGULT)
001000980304     D*------------------------------------------------------------------------*
001100980304     D* SCHIERE
001200980304     D*------------------------------------------------------------------------*
001300041220     D MSG             S             80    DIM(10) CTDATA PERRCD(1)
001400980304     D*------------
001500980304     D* DESCRIZIONI TASTI FUNZIONALI
001600980304     D*------------
001700980304     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001800980304     D*------------
001900980304     D* FUNZIONI DA CARICARE A VIDEO
002000980304     D*------------
002100980304     D FUV             S             19    DIM(24)
002200980304     D*------------------------------------------------------------------------*
002300980304     D* INPUT
002400980304     D*------------------------------------------------------------------------*
002500980304     D*------------
002600980304     D* RIDEFINIZIONE FUNZIONI VIDEO
002700980304     D*------------
002800980304     D FUDDS           DS
002900980304     D  FUDDE1                 1      1
003000980304     D  FUDFUN                 2      3
003100980304     D  FUDDE2                 4     19
003200980304     D  FUDFI0                20     34
003300980304     D  FUDATT                35     35
003400980304     D  FUDFI1                36     80
003500980304     D  FUDDES                 1     19
003600980304     D  FUDRIG                 1     80
003700980304     D*------------
003800980304     D* DS GESTIONE TASTI FUNZIONALI
003900980304     D*------------
004000980304     D DSKEY         E DS
004100980304     D DEVDS1          DS
004200980304     D  KEY                  369    369
004300980304     D*------------
004400980304     D* DS "XMSG" - GESTIONE MESSAGGI
004500980304     D*------------
004600980304     D DSMSG         E DS
004700980304     D*------------------
004800980304     D* DS di procedura
004900980304     D*------------------
005000080718     D TRULKPJDS     E DS
005100980304     C*------------------------------------------------------------------------*
005200980304     C* MAIN LINES
005300980304     C*------------------------------------------------------------------------*
005400041220     C*
005500041220     C* INIZIALIZZO LA DS D PROCEDURA
005600080718     C                   CLEAR                   TRULKPJDS
005700980304     C*
005800980304     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
005900980304DO  1C     F03L00        DOUEQ     '1'
006000980304     C     F12L00        OREQ      '1'
006100980304     C     FINL00        OREQ      '1'
006200980304     C*
006300980304     C* EMETTE FORMATO VIDEO
006400980304     C                   EXSR      BUTFMR
006500980304     C*
006600980304     C* GESTIONE TASTI FUNZIONALI
006700980304     C                   EXSR      FUNCON
006800980304E   1C                   ENDDO
006900980304     C*
007000980304     C                   SETON                                        LR
007100980304     C*------------------------------------------------------------------------*
007200980304     C* BUTFMR - EMISSIONE FORMATO A VIDEO
007300980304     C*------------------------------------------------------------------------*
007400980304     C     BUTFMR        BEGSR
007500980304     C*
007600980304     C* EMETTE LA TESTATA
007700080718     C                   WRITE     TRKUD01
007800980304     C*
007900980304     C* SPEDISCE EVENTUALE MESSAGGIO
008000980304     C                   EXSR      SNDMSG
008100980304     C*
008200980304     C* EMETTE LE FUNZIONI
008300080718     C                   WRITE     TRKUD02
008400980304     C*
008500980304     C* EMETTE IL VIDEO
008600080718     C                   EXFMT     TRKUD03
008700980304     C*
008800980304     C* REIMPOSTA FLAG DI ERRORE
008900980304     C                   MOVEL     '0'           ERRL00
009000980304     C*
009100980304     C                   ENDSR
009200980304     C*------------------------------------------------------------------------*
009300980304     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
009400980304     C*------------------------------------------------------------------------*
009500980304     C     SNDMSG        BEGSR
009600980304     C*
009700980304     C* PULISCE IL SFILE MESSAGGI
009800980304     C                   MOVEL     '1'           *IN40
009900080718     C                   WRITE     TRKUDMC
010000980304     C                   MOVEL     '0'           *IN40
010100980304     C*
010200980304     C* VISUALIZZA IL MESSAGGIO
010300980304     C                   Z-ADD     *ZEROS        MSGULT            4 0
010400980304     C                   CALL      'XMSG'        XMSGPA
010500041220     C     DSMSER        IFEQ      *ZEROS
010600980304     C                   MOVEL     DSMSKE        MSGKEY
010700041220     C                   ENDIF
010800980304     C                   Z-ADD     1             MSGULT
010900080718     C                   WRITE     TRKUDMS
011000080718     C                   WRITE     TRKUDMC
011100980304     C*
011200980304     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
011300980304     C                   MOVEL     *BLANKS       DSMSMS
011400980304     C*
011500980304     C                   ENDSR
011600980304     C*------------------------------------------------------------------------*
011700980304     C* ENTGES - GESTIONE ENTER -> INVIO
011800980304     C*------------------------------------------------------------------------*
011900980304     C     ENTGES        BEGSR
012000980304     C*
012100041220     C*
012200980304     C                   ENDSR
012300980304     C*------------------------------------------------------------------------*
012400980304     C* F03GES - GESTIONE F03 -> FINE
012500980304     C*------------------------------------------------------------------------*
012600980304     C     F03GES        BEGSR
012700980304     C*
012800080718     C* IMPOSTA "PREMUTO F03"
012900080718     C                   MOVEL     '1'           F03L00
013000080718     C                   MOVEL     'F03'         OKPJOPZ
013100980304     C*
013200980304     C                   ENDSR
013300080718     C*------------------------------------------------------------------------*
013400080718     C* F06GES - GESTIONE F06 -> CONFERMA
013500080718     C*------------------------------------------------------------------------*
013600080718     C     F06GES        BEGSR
013700080718     C*
013800080718     C* IMPOSTA "PREMUTO F06"
013900080718     C                   EVAL      OKPJKPJBU = IKPJBU01 + IKPJBU02 + IKPJBU03 +
014000080718     C                                         IKPJBU04 + IKPJBU05 + IKPJBU06
014100080718     C                   MOVEL     'F06'         OKPJOPZ
014200080718     C                   MOVEL     '1'           F03L00
014300080718     C*
014400080718     C                   ENDSR
014500980304     C*------------------------------------------------------------------------*
014600980304     C* F12GES - GESTIONE F12 -> RITORNO
014700980304     C*------------------------------------------------------------------------*
014800980304     C     F12GES        BEGSR
014900980304     C*
015000980304     C*
015100980304     C                   ENDSR
015200980304     C*------------------------------------------------------------------------*
015300980304     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
015400980304     C*------------------------------------------------------------------------*
015500980304     C     F24GES        BEGSR
015600980304     C*
015700980304     C                   EXSR      FUNGES
015800980304     C*
015900980304     C                   ENDSR
016000980304     C*------------------------------------------------------------------------*
016100980304     C* *INZSR - OPERAZIONI INIZIALI
016200980304     C*------------------------------------------------------------------------*
016300980304     C     *INZSR        BEGSR
016400980304     C*---
016500980304     C* RICEVIMENTO PARAMETRI
016600980304     C*---
016700980304     C     *ENTRY        PLIST
016800080721     C                   PARM                    TRULKPJDS
016900080721     C*
017000080721     C                   EVAL      IKPJBU01 = %subst(OKPJKPJBU:1:50)
017100080721     C                   EVAL      IKPJBU02 = %subst(OKPJKPJBU:51:50)
017200080721     C                   EVAL      IKPJBU03 = %subst(OKPJKPJBU:101:50)
017300080721     C                   EVAL      IKPJBU04 = %subst(OKPJKPJBU:151:50)
017400080721     C                   EVAL      IKPJBU05 = %subst(OKPJKPJBU:201:50)
017500080721     C                   EVAL      IKPJBU06 = %subst(OKPJKPJBU:251:6)
017600980304     C*
017700980304     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
017800980304     C     XMSGPA        PLIST
017900980304     C                   PARM                    DSMSMS
018000980304     C                   PARM                    DSMSPG
018100980304     C                   PARM                    DSMSKE
018200980304     C                   PARM                    DSMSER
018300980304     C*
018400980304     C* IMPOSTA LA DS -TASTI PREMUTI-
018500980304     C                   CALL      'XKEY'
018600980304     C                   PARM                    DSKEY
018700980304     C*---
018800980304     C* VARIABILI NON RIFERITE AL DATA BASE
018900980304     C*---
019000980304     C     *LIKE         DEFINE    FUDFUN        FUNKEY
019100980304     C*---
019200980304     C* VARIABILI DI CONTROLLO
019300980304     C*---
019400980304     C                   MOVEL     '0'           FINL00            1
019500980304     C                   MOVEL     '0'           F03L00            1
019600980304     C                   MOVEL     '0'           F12L00            1
019700980304     C                   MOVEL     '0'           ERRL00            1
019800980304     C*
019900980304     C* IMPOSTA LA DS -INVIO MESSAGGI-
020000080718     C                   MOVEL     'TRULKPJR'    DSMSPG
020100080718     C                   MOVEL     'TRULKPJR'    MSGQUE
020200980304     C                   MOVEL     *BLANKS       MSGKEY
020300980304     C                   MOVEL     *BLANKS       DSMSMS
020400980304     C*
020500980304     C* GESTIONE TASTI FUNZIONALI A VIDEO
020600980304     C                   Z-ADD     *ZEROS        FUNGRU            5 0
020700980304     C                   EXSR      FUNGES
020800980304     C*
020900980304     C                   ENDSR
021000980304     C*------------------------------------------------------------------------*
021100980304     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
021200980304     C*------------------------------------------------------------------------*
021300980304     C     FUNGES        BEGSR
021400980304     C*
021500980304     C* PULISCO CAMPI DI LAVORO
021600980304     C                   Z-ADD     0             W                 5 0
021700980304     C*
021800980304     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
021900980304     C* E CARICO TABELLA FUNZIONI ATTIVE
022000980304     C     1             DO        24            I
022100980304     C                   MOVEL     FUD(I)        FUDRIG
022200980304     C*
022300980304     C* PULIZIA FUNZIONI PRECEDENTI
022400980304     C                   MOVEL     *BLANKS       FUV(I)
022500980304     C*
022600980304     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
022700980304     C* (SALTA F24)
022800980304     C     FUDATT        IFEQ      'S'
022900980304     C     FUDFUN        ANDNE     '24'
023000980304     C                   ADD       1             W
023100980304     C                   MOVEL     FUDDES        FUV(W)
023200980304     C                   END
023300980304     C                   END
023400980304     C*
023500980304     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
023600980304     C* ALTRIMENTI DISATTIVO F24
023700980304     C     1             DO        24            I
023800980304     C                   MOVEL     FUD(I)        FUDRIG
023900980304     C*
024000980304     C     FUDFUN        IFEQ      '24'
024100980304     C     W             IFGT      6
024200980304     C                   MOVEL     'S'           FUDATT
024300980304     C                   ELSE
024400980304     C                   MOVEL     *BLANKS       FUDATT
024500980304     C                   END
024600980304     C                   MOVEL     FUDRIG        FUD(I)
024700980304     C                   END
024800980304     C                   END
024900980304     C*
025000980304     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
025100980304     C     W             DIV       6             I
025200980304     C                   MVR                     Z
025300980304     C*
025400980304     C     Z             IFNE      0
025500980304     C     I             ADD       1             Z
025600980304     C                   ELSE
025700980304     C                   Z-ADD     I             Z                 5 0
025800980304     C                   END
025900980304     C*
026000980304     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
026100980304     C                   ADD       1             FUNGRU
026200980304     C     FUNGRU        IFGT      Z
026300980304     C     FUNGRU        ORLT      1
026400980304     C                   Z-ADD     1             FUNGRU
026500980304     C                   END
026600980304     C*
026700980304     C* PRIMO GRUPPO DI 6 FUNZIONI
026800980304     C     FUNGRU        IFEQ      1
026900980304     C                   MOVEL     FUV(1)        FUN001
027000980304     C                   MOVEL     FUV(2)        FUN002
027100980304     C                   MOVEL     FUV(3)        FUN003
027200980304     C                   MOVEL     FUV(4)        FUN004
027300980304     C                   MOVEL     FUV(5)        FUN005
027400980304     C                   MOVEL     FUV(6)        FUN006
027500980304     C                   END
027600980304     C*
027700980304     C* SECONDO GRUPPO DI 6 FUNZIONI
027800980304     C     FUNGRU        IFEQ      2
027900980304     C                   MOVEL     FUV(7)        FUN001
028000980304     C                   MOVEL     FUV(8)        FUN002
028100980304     C                   MOVEL     FUV(9)        FUN003
028200980304     C                   MOVEL     FUV(10)       FUN004
028300980304     C                   MOVEL     FUV(11)       FUN005
028400980304     C                   MOVEL     FUV(12)       FUN006
028500980304     C                   END
028600980304     C*
028700980304     C* TERZO GRUPPO DI 6 FUNZIONI
028800980304     C     FUNGRU        IFEQ      3
028900980304     C                   MOVEL     FUV(13)       FUN001
029000980304     C                   MOVEL     FUV(14)       FUN002
029100980304     C                   MOVEL     FUV(15)       FUN003
029200980304     C                   MOVEL     FUV(16)       FUN004
029300980304     C                   MOVEL     FUV(17)       FUN005
029400980304     C                   MOVEL     FUV(18)       FUN006
029500980304     C                   END
029600980304     C*
029700980304     C* QUARTO GRUPPO DI 6 FUNZIONI
029800980304     C     FUNGRU        IFEQ      4
029900980304     C                   MOVEL     FUV(19)       FUN001
030000980304     C                   MOVEL     FUV(20)       FUN002
030100980304     C                   MOVEL     FUV(21)       FUN003
030200980304     C                   MOVEL     FUV(22)       FUN004
030300980304     C                   MOVEL     *BLANKS       FUN005
030400980304     C                   MOVEL     *BLANKS       FUN006
030500980304     C                   END
030600980304     C*
030700980304     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
030800980304     C                   MOVEL     *BLANKS       FUN007
030900980304     C                   MOVEL     *BLANKS       FUN008
031000980304     C*
031100980304     C     1             DO        24            I
031200980304     C                   MOVEL     FUD(I)        FUDRIG
031300980304     C     FUDFUN        IFEQ      '24'
031400980304     C     FUDATT        ANDEQ     'S'
031500980304     C                   MOVEL     FUDDES        FUN008
031600980304     C                   END
031700980304     C                   END
031800980304     C*
031900980304     C                   ENDSR
032000980304     C*------------------------------------------------------------------------*
032100980304     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
032200980304     C*------------------------------------------------------------------------*
032300980304     C     FUNCON        BEGSR
032400980304     C*
032500980304     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
032600980304     C                   EXSR      FUNVAR
032700980304     C*
032800980304     C* PULISCO CAMPI DI LAVORO
032900980304     C                   Z-ADD     0             I                 5 0
033000980304     C                   Z-ADD     0             W                 5 0
033100980304     C*
033200980304     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
033300980304     C     FUNKEY        IFNE      *BLANKS
033400980304     C     1             DO        24            I
033500980304     C                   MOVEL     FUD(I)        FUDRIG
033600980304     C*
033700980304     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
033800980304     C     FUNKEY        IFEQ      FUDFUN
033900980304     C     FUDATT        ANDEQ     'S'
034000980304     C                   Z-ADD     I             W
034100980304     C                   END
034200980304     C                   END
034300980304     C*
034400980304     C* FUNZIONE NON ATTIVA (ERRORE)
034500980304     C     W             IFEQ      0
034600980304     C*
034700980304     C* EMISSIONE MESSAGGIO DI ERRORE
034800980304     C                   MOVEL     MSG(1)        DSMSMS
034900980304     C*
035000980304     C                   END
035100980304     C                   END
035200980304     C*
035300980304     C* FUNZIONE ATTIVA (PROCEDI ...)
035400980304     C     W             IFNE      0
035500980304     C     FUNKEY        OREQ      *BLANKS
035600980304     C     FUNKEY        IFEQ      *BLANKS
035700980304     C                   EXSR      ENTGES
035800980304     C                   END
035900980304     C     FUNKEY        IFEQ      '03'
036000980304     C                   EXSR      F03GES
036100980304     C                   END
036200080721     C     FUNKEY        IFEQ      '06'
036300080721     C                   EXSR      F06GES
036400080721     C                   END
036500980304     C     FUNKEY        IFEQ      '12'
036600980304     C                   EXSR      F12GES
036700980304     C                   END
036800980304     C                   END
036900980304     C*
037000980304     C                   ENDSR
037100980304     C*------------------------------------------------------------------------*
037200980304     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
037300980304     C*------------------------------------------------------------------------*
037400980304     C     FUNVAR        BEGSR
037500980304     C*
037600980304     C* INIZIALIZZA VARIABILE -FUNKEY-
037700980304     C*
037800980304     C* FUNKEY=*BLANKS SIGNIFICA ENTER
037900980304     C     KEY           IFEQ      ENTER
038000980304     C                   MOVEL     *BLANKS       FUNKEY
038100980304     C                   END
038200980304     C     KEY           IFEQ      F1
038300980304     C                   MOVEL     '01'          FUNKEY
038400980304     C                   END
038500980304     C     KEY           IFEQ      F2
038600980304     C                   MOVEL     '02'          FUNKEY
038700980304     C                   END
038800980304     C     KEY           IFEQ      F3
038900980304     C                   MOVEL     '03'          FUNKEY
039000980304     C                   END
039100980304     C     KEY           IFEQ      F4
039200980304     C                   MOVEL     '04'          FUNKEY
039300980304     C                   END
039400980304     C     KEY           IFEQ      F5
039500980304     C                   MOVEL     '05'          FUNKEY
039600980304     C                   END
039700980304     C     KEY           IFEQ      F6
039800980304     C                   MOVEL     '06'          FUNKEY
039900980304     C                   END
040000980304     C     KEY           IFEQ      F7
040100980304     C                   MOVEL     '07'          FUNKEY
040200980304     C                   END
040300980304     C     KEY           IFEQ      F8
040400980304     C                   MOVEL     '08'          FUNKEY
040500980304     C                   END
040600980304     C     KEY           IFEQ      F9
040700980304     C                   MOVEL     '09'          FUNKEY
040800980304     C                   END
040900980304     C     KEY           IFEQ      F10
041000980304     C                   MOVEL     '10'          FUNKEY
041100980304     C                   END
041200980304     C     KEY           IFEQ      F11
041300980304     C                   MOVEL     '11'          FUNKEY
041400980304     C                   END
041500980304     C     KEY           IFEQ      F12
041600980304     C                   MOVEL     '12'          FUNKEY
041700980304     C                   END
041800980304     C     KEY           IFEQ      F13
041900980304     C                   MOVEL     '13'          FUNKEY
042000980304     C                   END
042100980304     C     KEY           IFEQ      F14
042200980304     C                   MOVEL     '14'          FUNKEY
042300980304     C                   END
042400980304     C     KEY           IFEQ      F15
042500980304     C                   MOVEL     '15'          FUNKEY
042600980304     C                   END
042700980304     C     KEY           IFEQ      F16
042800980304     C                   MOVEL     '16'          FUNKEY
042900980304     C                   END
043000980304     C     KEY           IFEQ      F17
043100980304     C                   MOVEL     '17'          FUNKEY
043200980304     C                   END
043300980304     C     KEY           IFEQ      F18
043400980304     C                   MOVEL     '18'          FUNKEY
043500980304     C                   END
043600980304     C     KEY           IFEQ      F19
043700980304     C                   MOVEL     '19'          FUNKEY
043800980304     C                   END
043900980304     C     KEY           IFEQ      F20
044000980304     C                   MOVEL     '20'          FUNKEY
044100980304     C                   END
044200980304     C     KEY           IFEQ      F21
044300980304     C                   MOVEL     '21'          FUNKEY
044400980304     C                   END
044500980304     C     KEY           IFEQ      F22
044600980304     C                   MOVEL     '22'          FUNKEY
044700980304     C                   END
044800980304     C     KEY           IFEQ      F23
044900980304     C                   MOVEL     '23'          FUNKEY
045000980304     C                   END
045100980304     C     KEY           IFEQ      F24
045200980304     C                   MOVEL     '24'          FUNKEY
045300980304     C                   END
045400980304     C     KEY           IFEQ      ROLLUP
045500980304     C                   MOVEL     'RU'          FUNKEY
045600980304     C                   END
045700980304     C     KEY           IFEQ      HELP
045800980304     C                   MOVEL     'HP'          FUNKEY
045900980304     C                   END
046000980304     C                   ENDSR
046100980304     C*------------------------------------------------------------------------*
046200980304     C* NOTE COSTRUZIONE SCHIERE
046300980304     C*------------------------------------------------------------------------*
046400980304     C*
046500980304     C* -MSG-MESSAGGI
046600980304     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
046700980304     C* (NON TOCCARE MSG,1 E MSG,2)
046800980304     C*
046900980304     C* -CMD-COMANDI
047000980304     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
047100980304     C*
047200980304     C* -OPD-OPZIONI
047300980304     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
047400980304     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
047500980304     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
047600980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
047700980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
047800980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
047900980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
048000980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
048100980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
048200980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
048300980304     C*
048400980304     C* -FUD-FUNZIONI
048500980304     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
048600980304     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
048700980304     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
048800980304     C*  . POSSONO ESSERE IN ORDINE SPARSO
048900980304     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
049000980304     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
049100980304     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
049200980304     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
049300980304     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
049400980304     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
049500980304     o*-------------------------------------------------------------------------
049600980304** msg - messaggi
049700080718TRULKPJR - Funzione non attiva
049800080718TRULKPJR - 2
049900080718TRULKPJR - 3
050000080718TRULKPJR - 4
050100080718TRULKPJR - 5
050200080718TRULKPJR - 6
050300080718TRULKPJR - 7
050400080718TRULKPJR - 8
050500080718TRULKPJR - 9
050600080718TRULKPJR - 10
050700980304** FUD - FUNZIONI
050800980304                   -        (S/N)=N                        funz.01¬
050900980304                   -        (S/N)=N                        funz.02¬
051000080718F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
051100980304                   -        (S/N)=N                        funz.04¬
051200980304                   -        (S/N)=N                        funz.05¬
051300080718F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
051400980304                   -        (S/N)=N                        funz.07¬
051500980304                   -        (S/N)=N                        funz.08¬
051600980304                   -        (S/N)=N                        funz.09¬
051700980304                   -        (S/N)=N                        funz.10¬
051800980304                   -        (S/N)=N                        funz.11¬
051900080718F12=Ritorno        -        (S/N)=N  <= S-FISSO            funz.12¬
052000980304                   -        (S/N)=N                        funz.13¬
052100980304                   -        (S/N)=N                        funz.14¬
052200980304                   -        (S/N)=N                        funz.15¬
052300980304                   -        (S/N)=N                        funz.16¬
052400980304                   -        (S/N)=N                        funz.17¬
052500980304                   -        (S/N)=N                        funz.18¬
052600980304                   -        (S/N)=N                        funz.19¬
052700980304                   -        (S/N)=N                        funz.20¬
052800980304                   -        (S/N)=N                        funz.21¬
052900980304                   -        (S/N)=N                        funz.22¬
053000980304                   -        (S/N)=N                        funz.23¬
053100980304F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
