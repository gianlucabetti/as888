000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*------------------------------------------------------------------------*
000300000000     F* DATA BASE
000400000000     F*------------------------------------------------------------------------*
000500030430     Fkpjbq00f  UF A E             DISK    USROPN
000600030430     Ftnsyjqd   CF   E             WORKSTN INFDS(DEVDS1)
000700030430     F                                     SFILE(TNSYJQS:MSGULT)
000800000000     D*------------------------------------------------------------------------*
000900000000     D* SCHIERE
001000000000     D*------------------------------------------------------------------------*
001100000000     D*-------------------
001200000000     D* MESSAGGI
001300000000     D*-------------------
001400000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
001500000000     D*-------------------
001600000000     D* DESCRIZIONI TASTI FUNZIONALI
001700000000     D*-------------------
001800000000     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
001900000000     D*-------------------
002000000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
002100000000     D*-------------------
002200000000     D FUV             S             19    DIM(24)
002300020514     D*-------------------
002400030430     D* SCHIERA AS/400 REMOTI/LOGICI
002500020514     D*-------------------
002600040330     D skFEL           S              3  0 DIM(600)
002700030505     D*-------------------
002800030505     D* SCHIERA WRK X CHK CODE GIA' ESISTENTI
002900030505     D*-------------------
003000040330     D skJOBQ          S             10    DIM(600)
003100030908     D*-------------------
003200030908     D* DS X REPERIMENTO/CONTROLLO DATA CORRENTE
003300030908     D*-------------------
003400030908     D WLBDA8          DS                  INZ
003500030908     D  G08DAT                        8  0
003600030908     D  G08INV                        8  0
003700030908     D  G08ERR                        1
003800030908     D  G08TGI                        5  0
003900030908     D*-------------------
004000061211     D* VARIABILI DI WRK
004100030908     D*-------------------
004200030908     D WN14            S             14  0
004300030908     D WN8             S              8  0
004400030908     D DATCOR          S              8  0
004500000000     D*------------------------------------------------------------------------*
004600000000     D* INPUT
004700000000     D*------------------------------------------------------------------------*
004800000000     D*-------------------
004900000000     D* RIDEFINIZIONE SCHIERA -FUD-
005000000000     D*-------------------
005100000000     D FUDDS           DS
005200000000     D*                -DESCRIZIONE PARZIALE
005300000000     D  FUDDE1                 1      1
005400000000     D*                -FUNZIONE
005500000000     D  FUDFUN                 2      3
005600000000     D*                -DESCRIZIONE PARZIALE
005700000000     D  FUDDE2                 4     19
005800000000     D*                -FILLER
005900000000     D  FUDFI0                20     34
006000000000     D*                -FLAG FUNZ.ATTIVA (S/N)
006100000000     D  FUDATT                35     35
006200000000     D*                -FILLER
006300000000     D  FUDFI1                36     80
006400000000     D*                -DESCRIZIONE COMPLETA
006500000000     D  FUDDES                 1     19
006600000000     D*                -RIDEFINIZIONE RIGA
006700000000     D  FUDRIG                 1     80
006800000000     D*-------------------
006900000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
007000000000     D*-------------------
007100000000     D DSMSG         E DS
007200000000     D*-------------------
007300000000     D* DS PER GESTIONE TASTI FUNZIONALI
007400000000     D*-------------------
007500000000     D DSKEY         E DS
007600000000     D DEVDS1          DS
007700000000     D  KEY                  369    369
007800030908     D*-------------------
007900030908     D* DS X RICHIAMO *PGM FNLV55R
008000030908     D*-------------------
008100030908     D FNLV55DS      E DS
008200000000     C*------------------------------------------------------------------------*
008300000000     C* MAIN LINE
008400000000     C*------------------------------------------------------------------------*
008500000000     C*
008600000000     C* SE CONTROLLI NON OK VADO A FINE PGM (ERRL00: 1=ERRORE)
008700000000     C     ERRL00        IFEQ      '0'
008800000000     C*
008900000000     C* CICLO FINO A CHE:
009000000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009100000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
009200000000     C*  -E' STATA ESEGUITA LA FUNZIONE RICHIESTA
009300000000     C     F03L00        DOUEQ     '1'
009400000000     C     F12L00        OREQ      '1'
009500000000     C*
009600000000     C* EMETTE FORMATO VIDEO
009700000000     C                   EXSR      BUTFMR
009800000000     C*
009900000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI
010000000000     C                   EXSR      FUNCON
010100000000     C*
010200010905     C                   ENDDO
010300000000     C*
010400010905     C                   ENDIF
010500991118     C*
010600000000     C                   SETON                                        LR
010700000000     C*------------------------------------------------------------------------*
010800000000     C* EMISSIONE FORMATO A VIDEO
010900000000     C*------------------------------------------------------------------------*
011000000000     C     BUTFMR        BEGSR
011100000000     C*
011200000000     C* WRITE RECORD TESTATA
011300030430     C                   WRITE     TNSYJQ1
011400000000     C*
011500000000     C* VISUALIZZAZIONE MESSAGGIO
011600000000     C                   EXSR      SNDMSG
011700000000     C*
011800000000     C* WRITE RECORD FUNZIONI
011900030430     C                   WRITE     TNSYJQ2
012000991019     C*
012100991019     C* EXFMT RECORD DETTAGLIO
012200030430     C                   EXFMT     TNSYJQ3
012300991019     C*
012400000000     C* AZZERO FLAG DI ERRORE LIVELLO
012500000000     C                   MOVEL     '0'           ERRL00
012600000000     C*
012700000505     C* AZZERO FLAG DI ERRORE
012800010905     C                   SETOFF                                       7072
012900000000     C*
013000000000     C                   ENDSR
013100000000     C*------------------------------------------------------------------------*
013200000000     C* SNDMSG - MANDA MESSAGGIO
013300000000     C*------------------------------------------------------------------------*
013400000000     C     SNDMSG        BEGSR
013500000000     C*
013600000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
013700000000     C                   MOVEL     '1'           *IN40
013800030430     C                   WRITE     TNSYJQC
013900000000     C                   MOVEL     '0'           *IN40
014000000000     C*
014100000000     C* VISUALIZZAZIONE MESSAGGIO
014200000000     C                   Z-ADD     0             MSGULT            4 0
014300000000     C                   CALL      'XMSG'        XMSGPA
014400000000     C     DSMSER        IFEQ      0
014500000000     C                   MOVEL     DSMSKE        MSGKEY
014600000000     C                   END
014700000000     C                   Z-ADD     1             MSGULT
014800030430     C                   WRITE     TNSYJQS
014900030430     C                   WRITE     TNSYJQC
015000000000     C*
015100000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
015200000000     C                   MOVEL     *BLANKS       DSMSMS
015300000000     C*
015400000000     C                   ENDSR
015500000000     C*------------------------------------------------------------------------*
015600000000     C* ENTGES - GESTIONE DEL TASTO FUNZIONALE ENTER
015700000000     C*------------------------------------------------------------------------*
015800000000     C     ENTGES        BEGSR
015900030430     C*
016000030430     C* Verifico che l'AS/400 inserito a video sia un elaboratore valido
016100030430     C                   EXSR      DECODI
016200000000     C*
016300000000     C                   ENDSR
016400000000     C*------------------------------------------------------------------------*
016500000000     C* F03GES -GESTIONE TASTO FUNZIONALE F03 - FINE
016600000000     C*------------------------------------------------------------------------*
016700000000     C     F03GES        BEGSR
016800000000     C*
016900000000     C* IMPOSTO FLAG F03 DI LIVELLO PGM
017000000000     C                   MOVEL     '1'           F03L00
017100000000     C*
017200000000     C                   ENDSR
017300000000     C*------------------------------------------------------------------------*
017400011120     C* F06GES -GESTIONE TASTO FUNZIONALE F06 - CONFERMA STEP1
017500000000     C*------------------------------------------------------------------------*
017600991020     C     F06GES        BEGSR
017700030430     C*
017800030430     C* Verifico che l'AS/400 inserito a video sia un elaboratore valido
017900030430     C                   EXSR      DECODI
018000030430     C*
018100030430     C* Se tutto ok procedo con l'elaborazione vera e propria
018200030505     C                   IF        ERRL00 = '0'
018300030430     C*
018400030430     C* Come prima cosa apro il file delle JOBQ
018500030430     C                   OPEN      KPJBQ00F
018600011119     C*
018700030430     C* Scorro il file delle JOBQ x "costruire" la coda da aagiungere
018800030430     C                   MOVEL     '0'           CHKERR            1
018900030430     C                   Z-ADD     1             Wconta            2 0
019000030430     C                   READ      KPJBQ00F
019100030430     C                   DOW       not %eof(KPJBQ00F)
019200030430     C                   IF        %subst(JBQNOM:2:3) = 'XXX'
019300030430     C                   MOVEL(P)  JBQNOM        Wjobq            10
019400030505     C                   MOVEL(P)  VIDSEQ        Wseq              4
019500030430     C                   MOVE      Wconta        Wseq
019600030430     C                   EVAL      %subst(Wjobq:2:3) = VIDAS4
019700030430     C                   CALL(e)   'TNSYJQC'
019800030430     C                   PARM                    Wjobq
019900030430     C                   PARM                    Wseq
020000030430     C                   PARM      '0'           Wesito            1
020100030505     C                   PARM      VIDOPZ        Wopzione          1
020200030430     C                   IF        not %error
020300030505     C*
020400030505     C* Solo in caso di opzione A=Aggiunta
020500030505     C                   IF        VIDOPZ = 'A'
020600030430     C                   EVAL      JBQNOM = Wjobq
020700030505     C*
020800030505     C* Inserisco nel file la JOBQ solo se nn già presente
020900030505     C     JBQNOM        LOOKUP    skJOBQ                                 55
021000030505     C                   IF        *IN55 = *off
021100030505     C                   WRITE     KPJBQ000
021200030505     C                   ADD       1             Y
021300030505     C                   EVAL      skJOBQ(Y) = JBQNOM
021400030430     C                   ENDIF
021500030505     C                   ENDIF
021600030505     C                   ENDIF
021700030430     C                   IF        Wesito <> '0'
021800030430     C                   MOVEL     '1'           CHKERR
021900030430     C                   ENDIF
022000030430     C                   ADD       1             Wconta
022100030430     C                   ENDIF
022200030430     C                   READ      KPJBQ00F
022300030430     C                   ENDDO
022400991124     C*
022500030430     C* Al termine mando a video un messaggio relativo all'esito dell'elaboraz.
022600030430     C                   IF        CHKERR = '0'
022700030430     C                   MOVEL     MSG(3)        DSMSMS
022800030430     C                   ELSE
022900030430     C                   MOVEL     MSG(4)        DSMSMS
023000030430     C                   ENDIF
023100030430     C*
023200030430     C* Al termine dell'elaborazione chiudo il file delle JOBQ
023300030430     C                   CLOSE     KPJBQ00F
023400030430     C                   ENDIF
023500991105     C*
023600000000     C                   ENDSR
023700000000     C*------------------------------------------------------------------------*
023800000000     C* F12GES -GESTIONE TASTO FUNZIONALE F12 - RITORNO
023900000000     C*------------------------------------------------------------------------*
024000000000     C     F12GES        BEGSR
024100000000     C*
024200000000     C* IMPOSTO FLAG DI LIVELLO
024300000000     C                   MOVEL     '1'           F12L00
024400000000     C*
024500000000     C                   ENDSR
024600000000     C*------------------------------------------------------------------------*
024700991020     C* F24GES -GESTIONE TASTO FUNZIONALE F24 - ALTRE FUNZIONI
024800000000     C*------------------------------------------------------------------------*
024900000000     C     F24GES        BEGSR
025000000000     C*
025100000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
025200000000     C                   EXSR      FUNGES
025300000000     C*
025400000000     C                   ENDSR
025500030430     C*------------------------------------------------------------------------*
025600030430     C* DECODI - CONTROLLA I DATI IN INPUT DEL VIDEO                           *
025700030430     C*------------------------------------------------------------------------*
025800030430     C     DECODI        BEGSR
025900030505     C*
026000030505     C* Carico la lista delle JOBQ
026100030505     C                   EXSR      CARJOBQ
026200030430     C*
026300030430     C* Inizializzo le variabili di wrk
026400030430     C                   MOVEL     '0'           ERRL00            1
026500030908     C                   MOVEL(P)  VIDAS4        WVIDAS4num        3 0
026600030430     C*
026700061211     C* Controllo che AS400 immesso a video sia 1 terminal di partenza
026800030908     C                   CLEAR                   FNLV55DS
026900030908     C                   EVAL      D55TLA = 'L'
027000030908     C                   EVAL      D55TPT = 'P'
027100030908     C                   EVAL      D55LIN = WVIDAS4num
027200061211     C                   EVAL      D55DRF = VIDDAT
027300030908     C                   CALL      'FNLV55R'
027400030908     C                   PARM                    FNLV55DS
027500030908     C*
027600030908     C                   IF        D55ERR <> *blanks
027700030908     C                   MOVEL     '1'           ERRL00
027800030908     C                   MOVEL     MSG(7)        DSMSMS
027900030908     C                   ELSE
028000030908     C                   IF        WVIDAS4num <> D55TFP
028100030430     C                   MOVEL     '1'           ERRL00
028200030505     C                   MOVEL     MSG(2)        DSMSMS
028300030505     C                   ELSE
028400030505     C*
028500030505     C* Se nn impostato valorizzo la sequenza con il codice AS/400 indicato
028600030505     C                   IF        VIDSEQ = *zeros
028700030505     C                   MOVE(p)   VIDAS4        VIDSEQ
028800030505     C                   ENDIF
028900030505     C*
029000030505     C* Se nn impostata valorizzo l'opzione
029100030505     C                   IF        VIDOPZ = *blanks
029200030505     C                   MOVEL     'A'           VIDOPZ
029300030505     C                   ENDIF
029400030505     C*
029500030505     C* Verifico correttezza ordine sequenza (solo se opzione = 'A')
029600030505     C                   IF        VIDOPZ = 'A'
029700030505     C                   EXSR      CHKSEQ
029800030505     C                   ENDIF
029900030430     C                   ENDIF
030000030908     C                   ENDIF
030100030430     C*
030200030430     C                   ENDSR
030300030505     C*------------------------------------------------------------------------*
030400030505     C* CARJOBQ - REPERISCE TUTTE LE CODE LAVORI GIA' PRESENTI SU KPJBQ00F     *
030500030505     C*------------------------------------------------------------------------*
030600030505     C     CARJOBQ       BEGSR
030700030505     C*
030800030505     C                   Z-ADD     *ZEROS        Y                 3 0
030900030505     C*
031000030505     C* Come prima cosa apro il file delle JOBQ
031100030505     C                   OPEN      KPJBQ00F
031200030505     C*
031300030505     C* Scorro tutto il file delle JOBQ
031400030505     C                   READ      KPJBQ00F
031500030505     C                   DOW       not %eof(KPJBQ00F)
031600030505     C                   ADD       1             Y
031700030505     C                   EVAL      skJOBQ(Y) = JBQNOM
031800030505     C                   READ      KPJBQ00F
031900030505     C                   ENDDO
032000030505     C*
032100030505     C* Al termine dell'elaborazione chiudo il file delle JOBQ
032200030505     C                   CLOSE     KPJBQ00F
032300030505     C*
032400030505     C                   ENDSR
032500030505     C*------------------------------------------------------------------------*
032600030505     C* CHKSEQ - CONTROLLA CORRETTEZZA ORDINE DI SEQUENZA                      *
032700030505     C*------------------------------------------------------------------------*
032800030505     C     CHKSEQ        BEGSR
032900030505     C*
033000030505     C                   MOVEL(p)  'KXXXSDIFFE'  Wjobq
033100030505     C                   EVAL      %subst(Wjobq:2:3) = VIDAS4
033200030505     C                   MOVEL(p)  VIDSEQ        Wseq
033300030505     C                   MOVE      '01'          Wseq
033400030505     C*
033500030505     C                   CALL(e)   'TNSYJQC1'
033600030505     C                   PARM                    Wjobq
033700030505     C                   PARM                    Wseq
033800030505     C                   PARM      '0'           Wesito
033900030505     C                   IF        Wesito <> '0'
034000030505     C                   MOVEL     '1'           ERRL00
034100030505     C                   IF        Wesito = '1'
034200030505     C                   MOVEL     MSG(5)        DSMSMS
034300030505     C                   ENDIF
034400030505     C                   IF        Wesito = '2'
034500030505     C                   MOVEL     MSG(6)        DSMSMS
034600030505     C                   ENDIF
034700030505     C                   ENDIF
034800030505     C*
034900030505     C                   ENDSR
035000000000     C*------------------------------------------------------------------------*
035100980403     C* *INZSR - OPERAZIONI INIZIALI
035200000000     C*------------------------------------------------------------------------*
035300980403     C     *INZSR        BEGSR
035400000000     C*
035500000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
035600000000     C     XMSGPA        PLIST
035700000000     C                   PARM                    DSMSMS
035800000000     C                   PARM                    DSMSPG
035900000000     C                   PARM                    DSMSKE
036000000000     C                   PARM                    DSMSER
036100030908     C*
036200030908     C* REPERISCE LA DATA CORRENTE
036300030908     C                   TIME                    WN14
036400030908     C                   MOVE      WN14          WN8
036500030908     C                   Z-ADD     WN8           G08DAT
036600030908     C                   Z-ADD     *zeros        G08INV
036700030908     C                   MOVEL     '0'           G08ERR
036800030908     C                   CALL      'XSRDA8'
036900030908     C                   PARM                    WLBDA8
037000030908     C                   Z-ADD     G08INV        DATCOR
037100000000     C*
037200000000     C* CHIAMA XKEY PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
037300000000     C                   CALL      'XKEY'
037400000000     C                   PARM                    DSKEY
037500000000     C*
037600000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
037700000225     C     *LIKE         DEFINE    FUDFUN        FUNKEY
037800000000     C*
037900000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
038000000000     C*   -NOME PGM CHIAMANTE
038100000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
038200030430     C                   MOVEL     'TNSYJQR'     DSMSPG
038300030430     C                   MOVEL     'TNSYJQR'     MSGQUE
038400000000     C                   MOVEL     *BLANKS       MSGKEY
038500000000     C                   MOVEL     *BLANKS       DSMSMS
038600000000     C*
038700000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
038800000000     C                   Z-ADD     0             FUNGRU            5 0
038900000000     C*
039000000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
039100000000     C                   EXSR      FUNGES
039200000000     C*
039300000000     C* INIZIALIZZO SWITCH DI LIVELLO DI F12 E F03
039400000000     C                   MOVEL     '0'           F12L00            1
039500000000     C                   MOVEL     '0'           F03L00            1
039600000000     C                   MOVEL     '0'           FINL00            1
039700000000     C                   MOVEL     '0'           ERRL00            1
039800000000     C*
039900000000     C                   ENDSR
040000000000     C*------------------------------------------------------------------------*
040100000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
040200000000     C*------------------------------------------------------------------------*
040300000000     C     FUNGES        BEGSR
040400000000     C*
040500000000     C* PULISCO CAMPI DI LAVORO
040600000000     C                   Z-ADD     0             W                 5 0
040700000000     C*
040800000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
040900000000     C* E CARICO TABELLA FUNZIONI ATTIVE
041000000000     C     1             DO        24            I
041100000000     C                   MOVEL     FUD(I)        FUDRIG
041200000000     C*
041300000000     C* PULIZIA FUNZIONI PRECEDENTI
041400000000     C                   MOVEL     *BLANKS       FUV(I)
041500000000     C*
041600000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
041700000000     C* (SALTA F24)
041800000000     C     FUDATT        IFEQ      'S'
041900000000     C     FUDFUN        ANDNE     '24'
042000000000     C                   ADD       1             W
042100000000     C                   MOVEL     FUDDES        FUV(W)
042200000000     C                   END
042300000000     C                   END
042400000000     C*
042500000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
042600000000     C* ALTRIMENTI DISATTIVO F24
042700000000     C     1             DO        24            I
042800000000     C                   MOVEL     FUD(I)        FUDRIG
042900000000     C*
043000000000     C     FUDFUN        IFEQ      '24'
043100000000     C     W             IFGT      6
043200000000     C                   MOVEL     'S'           FUDATT
043300000000     C                   ELSE
043400000000     C                   MOVEL     *BLANKS       FUDATT
043500000000     C                   END
043600000000     C                   MOVEL     FUDRIG        FUD(I)
043700000000     C                   END
043800000000     C                   END
043900000000     C*
044000000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
044100000000     C     W             DIV       6             I
044200000000     C                   MVR                     Z
044300000000     C*
044400000000     C     Z             IFNE      0
044500000000     C     I             ADD       1             Z
044600000000     C                   ELSE
044700000000     C                   Z-ADD     I             Z                 5 0
044800000000     C                   END
044900000000     C*
045000000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
045100000000     C                   ADD       1             FUNGRU
045200000000     C     FUNGRU        IFGT      Z
045300000000     C     FUNGRU        ORLT      1
045400000000     C                   Z-ADD     1             FUNGRU
045500000000     C                   END
045600000000     C*
045700000000     C* PRIMO GRUPPO DI 6 FUNZIONI
045800000000     C     FUNGRU        IFEQ      1
045900000000     C                   MOVEL     FUV(1)        FUN001
046000000000     C                   MOVEL     FUV(2)        FUN002
046100000000     C                   MOVEL     FUV(3)        FUN003
046200000000     C                   MOVEL     FUV(4)        FUN004
046300000000     C                   MOVEL     FUV(5)        FUN005
046400000000     C                   MOVEL     FUV(6)        FUN006
046500000000     C                   END
046600000000     C*
046700000000     C* SECONDO GRUPPO DI 6 FUNZIONI
046800000000     C     FUNGRU        IFEQ      2
046900000000     C                   MOVEL     FUV(7)        FUN001
047000000000     C                   MOVEL     FUV(8)        FUN002
047100000000     C                   MOVEL     FUV(9)        FUN003
047200000000     C                   MOVEL     FUV(10)       FUN004
047300000000     C                   MOVEL     FUV(11)       FUN005
047400000000     C                   MOVEL     FUV(12)       FUN006
047500000000     C                   END
047600000000     C*
047700000000     C* TERZO GRUPPO DI 6 FUNZIONI
047800000000     C     FUNGRU        IFEQ      3
047900000000     C                   MOVEL     FUV(13)       FUN001
048000000000     C                   MOVEL     FUV(14)       FUN002
048100000000     C                   MOVEL     FUV(15)       FUN003
048200000000     C                   MOVEL     FUV(16)       FUN004
048300000000     C                   MOVEL     FUV(17)       FUN005
048400000000     C                   MOVEL     FUV(18)       FUN006
048500000000     C                   END
048600000000     C*
048700000000     C* QUARTO GRUPPO DI 6 FUNZIONI
048800000000     C     FUNGRU        IFEQ      4
048900000000     C                   MOVEL     FUV(19)       FUN001
049000000000     C                   MOVEL     FUV(20)       FUN002
049100000000     C                   MOVEL     FUV(21)       FUN003
049200000000     C                   MOVEL     FUV(22)       FUN004
049300000000     C                   MOVEL     *BLANKS       FUN005
049400000000     C                   MOVEL     *BLANKS       FUN006
049500000000     C                   END
049600000000     C*
049700000000     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
049800000000     C                   MOVEL     *BLANKS       FUN007
049900000000     C                   MOVEL     *BLANKS       FUN008
050000000000     C*
050100000000     C     1             DO        24            I
050200000000     C                   MOVEL     FUD(I)        FUDRIG
050300000000     C     FUDFUN        IFEQ      '24'
050400000000     C     FUDATT        ANDEQ     'S'
050500000000     C                   MOVEL     FUDDES        FUN008
050600000000     C                   END
050700000000     C                   END
050800000000     C                   ENDSR
050900000000     C*------------------------------------------------------------------------*
051000000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
051100000000     C*------------------------------------------------------------------------*
051200000000     C     FUNCON        BEGSR
051300000000     C*
051400000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
051500000000     C                   EXSR      FUNVAR
051600000000     C*
051700000000     C* PULISCO CAMPI DI LAVORO
051800000000     C                   Z-ADD     0             I                 5 0
051900000000     C                   Z-ADD     0             W                 5 0
052000000000     C*
052100000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
052200000000     C     FUNKEY        IFNE      *BLANKS
052300000000     C     1             DO        24            I
052400000000     C                   MOVEL     FUD(I)        FUDRIG
052500000000     C*
052600000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
052700000000     C     FUNKEY        IFEQ      FUDFUN
052800000000     C     FUDATT        ANDEQ     'S'
052900000000     C                   Z-ADD     I             W
053000000000     C                   END
053100000000     C                   END
053200000000     C*
053300000000     C* FUNZIONE NON ATTIVA (ERRORE)
053400000000     C     W             IFEQ      0
053500000000     C*
053600000000     C* EMISSIONE MESSAGGIO DI ERRORE
053700000000     C                   MOVEL     MSG(1)        DSMSMS
053800000000     C*
053900000000     C                   END
054000000000     C                   END
054100000000     C*
054200000000     C* FUNZIONE ATTIVA (PROCEDI ...)
054300000000     C     W             IFNE      0
054400000000     C     FUNKEY        OREQ      *BLANKS
054500000000     C*
054600000000     C     FUNKEY        IFEQ      *BLANKS
054700000000     C                   EXSR      ENTGES
054800000000     C                   END
054900000000     C     FUNKEY        IFEQ      '03'
055000000000     C                   EXSR      F03GES
055100000000     C                   END
055200000000     C     FUNKEY        IFEQ      '06'
055300000000     C                   EXSR      F06GES
055400000000     C                   END
055500000000     C     FUNKEY        IFEQ      '12'
055600000000     C                   EXSR      F12GES
055700000000     C                   END
055800000000     C     FUNKEY        IFEQ      '24'
055900000000     C                   EXSR      F24GES
056000000000     C                   END
056100000000     C*
056200000000     C                   END
056300000000     C*
056400000000     C                   ENDSR
056500000000     C*------------------------------------------------------------------------*
056600000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
056700000000     C*------------------------------------------------------------------------*
056800000000     C     FUNVAR        BEGSR
056900000000     C*
057000000000     C* INIZIALIZZA VARIABILE -FUNKEY-
057100000000     C*
057200000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
057300000000     C     KEY           IFEQ      ENTER
057400000000     C                   MOVEL     *BLANKS       FUNKEY
057500000000     C                   END
057600000000     C     KEY           IFEQ      F1
057700000000     C                   MOVEL     '01'          FUNKEY
057800000000     C                   END
057900000000     C     KEY           IFEQ      F2
058000000000     C                   MOVEL     '02'          FUNKEY
058100000000     C                   END
058200000000     C     KEY           IFEQ      F3
058300000000     C                   MOVEL     '03'          FUNKEY
058400000000     C                   END
058500000000     C     KEY           IFEQ      F4
058600000000     C                   MOVEL     '04'          FUNKEY
058700000000     C                   END
058800000000     C     KEY           IFEQ      F5
058900000000     C                   MOVEL     '05'          FUNKEY
059000000000     C                   END
059100000000     C     KEY           IFEQ      F6
059200000000     C                   MOVEL     '06'          FUNKEY
059300000000     C                   END
059400000000     C     KEY           IFEQ      F7
059500000000     C                   MOVEL     '07'          FUNKEY
059600000000     C                   END
059700000000     C     KEY           IFEQ      F8
059800000000     C                   MOVEL     '08'          FUNKEY
059900000000     C                   END
060000000000     C     KEY           IFEQ      F9
060100000000     C                   MOVEL     '09'          FUNKEY
060200000000     C                   END
060300000000     C     KEY           IFEQ      F10
060400000000     C                   MOVEL     '10'          FUNKEY
060500000000     C                   END
060600000000     C     KEY           IFEQ      F11
060700000000     C                   MOVEL     '11'          FUNKEY
060800000000     C                   END
060900000000     C     KEY           IFEQ      F12
061000000000     C                   MOVEL     '12'          FUNKEY
061100000000     C                   END
061200000000     C     KEY           IFEQ      F13
061300000000     C                   MOVEL     '13'          FUNKEY
061400000000     C                   END
061500000000     C     KEY           IFEQ      F14
061600000000     C                   MOVEL     '14'          FUNKEY
061700000000     C                   END
061800000000     C     KEY           IFEQ      F15
061900000000     C                   MOVEL     '15'          FUNKEY
062000000000     C                   END
062100000000     C     KEY           IFEQ      F16
062200000000     C                   MOVEL     '16'          FUNKEY
062300000000     C                   END
062400000000     C     KEY           IFEQ      F17
062500000000     C                   MOVEL     '17'          FUNKEY
062600000000     C                   END
062700000000     C     KEY           IFEQ      F18
062800000000     C                   MOVEL     '18'          FUNKEY
062900000000     C                   END
063000000000     C     KEY           IFEQ      F19
063100000000     C                   MOVEL     '19'          FUNKEY
063200000000     C                   END
063300000000     C     KEY           IFEQ      F20
063400000000     C                   MOVEL     '20'          FUNKEY
063500000000     C                   END
063600000000     C     KEY           IFEQ      F21
063700000000     C                   MOVEL     '21'          FUNKEY
063800000000     C                   END
063900000000     C     KEY           IFEQ      F22
064000000000     C                   MOVEL     '22'          FUNKEY
064100000000     C                   END
064200000000     C     KEY           IFEQ      F23
064300000000     C                   MOVEL     '23'          FUNKEY
064400000000     C                   END
064500000000     C     KEY           IFEQ      F24
064600000000     C                   MOVEL     '24'          FUNKEY
064700000000     C                   END
064800000000     C     KEY           IFEQ      ROLLUP
064900000000     C                   MOVEL     'RU'          FUNKEY
065000000000     C                   END
065100000000     C     KEY           IFEQ      HELP
065200000000     C                   MOVEL     'HP'          FUNKEY
065300000000     C                   END
065400000000     C                   ENDSR
065500000000     C*------------------------------------------------------------------------*
065600000000     C* NOTE PER COMPILAZIONE SCHIERE
065700000000     C*------------------------------------------------------------------------*
065800000000     C* -MSG-
065900000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
066000000000     C* (NON TOCCARE MSG,1)
066100000000     C*
066200000000     C* -FUD-
066300000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
066400000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
066500000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
066600000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
066700000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
066800000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
066900000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
067000000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
067100000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
067200000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
067300000000     C*
067400000000     C*------------------------------------------------------------------------*
067500000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
067600030430TNSYJQR- Tasto funzionale non gestito.                                         1
067700030908TNSYJQR- Il valore immesso non è un terminal partenza valido, correggere.      2
067800030430TNSYJQR- Elaborazione conclusa correttamente.                                  3
067900030430TNSYJQR- Elaborazione conclusa ma si sono verificati errori, verificare.       4
068000030505TNSYJQR- JOBQ già esistenti per AS/400 indicato.                               5
068100030505TNSYJQR- Numero di sequenza già utilizzato, correggere.                        6
068200030908TNSYJQR- Errore in chiamata FNLV55R, controllare l'AS/400 indicato a video.    7
068300030430TNSYJQR-                                                                       8
068400030430TNSYJQR-                                                                       9
068500030430TNSYJQR-                                                                      10
068600030430TNSYJQR-                                                                      11
068700030430TNSYJQR-                                                                      12
068800030430TNSYJQR-                                                                      13
068900030430TNSYJQR-                                                                      14
069000030430TNSYJQR-                                                                      15
069100030430TNSYJQR-                                                                      16
069200030430TNSYJQR-                                                                      17
069300030430TNSYJQR-                                                                      18
069400030430TNSYJQR-                                                                      19
069500030430TNSYJQR-                                                                      20
069600000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
069700000000                   -        (S/N)=N                        funz.01¬
069800000000                   -        (S/N)=N                        funz.02¬
069900000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
070000000000                   -        (S/N)=N                        funz.04¬
070100000000                   -        (S/N)=N                        funz.05¬
070200020416F06=Conferma       -        (S/N)=S                        funz.06¬
070300020416                   -        (S/N)=N                        funz.07¬
070400000000                   -        (S/N)=N                        funz.08¬
070500000000                   -        (S/N)=N                        funz.09¬
070600000000                   -        (S/N)=N                        funz.10¬
070700000000                   -        (S/N)=N                        funz.11¬
070800000000F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
070900000000                   -        (S/N)=N                        funz.13¬
071000000000                   -        (S/N)=N                        funz.14¬
071100000000                   -        (S/N)=N                        funz.15¬
071200000000                   -        (S/N)=N                        funz.16¬
071300000000                   -        (S/N)=N                        funz.17¬
071400000000                   -        (S/N)=N                        funz.18¬
071500000000                   -        (S/N)=N                        funz.19¬
071600000000                   -        (S/N)=N                        funz.20¬
071700000000                   -        (S/N)=N                        funz.21¬
071800000000F22=Gestione stampe-        (S/N)=N                        funz.22¬
071900000000                   -        (S/N)=N                        funz.23¬
072000000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
