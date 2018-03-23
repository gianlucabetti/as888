000100970526     H*------------------------------------------------------------------------*
000200991025     H* Gestione tipi file internet
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800000705     Ftis728d   cf   e             WORKSTN INFDS(DEVDS1)
000900000705     F                                     SFILE(s728DMS:MSGULT)
001000970527     F*------------
001100000705     Ftivre02l  UF A E           K DISK
001200000707     Ftivir02l  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900991014     D DEC             S             66    DIM(6) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970527     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* DESCRIZIONI TASTI FUNZIONALI
002600970526     D*------------
002700970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* FUNZIONI DA CARICARE A VIDEO
003000970526     D*------------
003100970526     D FUV             S             19    DIM(24)
003200970526     D*------------------------------------------------------------------------*
003300970526     D* INPUT
003400970526     D*------------------------------------------------------------------------*
003500970526     D*------------
003600970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003700970526     D*------------
003800970526     D FUDDS           DS
003900970526     D  FUDDE1                 1      1
004000970526     D  FUDFUN                 2      3
004100970526     D  FUDDE2                 4     19
004200970526     D  FUDFI0                20     34
004300970526     D  FUDATT                35     35
004400970526     D  FUDFI1                36     80
004500970526     D  FUDDES                 1     19
004600970526     D  FUDRIG                 1     80
004700970526     D*------------
004800970526     D* DS GESTIONE TASTI FUNZIONALI
004900970526     D*------------
005000970526     D DSKEY         E DS
005100970526     D DEVDS1          DS
005200970526     D  KEY                  369    369
005300970526     D*------------
005400970526     D* DS "XMSG" - GESTIONE MESSAGGI
005500970526     D*------------
005600970526     D DSMSG         E DS
005700020218     D*------------
005800020218     D* DS "ITF" - TABELLA SCAMBIO TIPI FILE
005900020218     D*------------
006000020218     D DITF          E DS
006100000705     D*------------
006200000705     D* TABELLE
006300000705     D*------------
006400991108     D TIBS02DS      E DS
006500970528     D*------------------
006600970528     D* DS "XSRDA8" - CONTROLLA DATA (8)
006700970528     D*------------------
006800970528     D WLBDA8          DS                  INZ
006900970528     D  G08DAT                 1      8  0
007000970528     D  G08INV                 9     16  0
007100970528     D  G08ERR                17     17
007200970528     D  G08TGI                18     22  0
007300970526     D*------------
007400970526     D* DS DI PROCEDURA
007500970526     D*------------
007600991025     D tis720        E DS                  EXTNAME(tis720ds)
007700970526     D*------------
007800970526     D* ARCHITETTURA
007900970526     D*------------
008000970526     D KPJBA         E DS
008100000705     D*------------------
008200000707     D BS69DS        e ds                  extname(TIBS69DS)
008300000705     D ACODS         e ds                  extname(cnaco00f)
008400000705     D INDDS         e ds                  extname(cnind00f)
008500000705     D CLPDS         e ds                  extname(cnclp00f)
008600000705     D CLSDS         e ds                  extname(fncls00f)
008700970526     C*------------------------------------------------------------------------*
008800970526     C* MAIN LINES
008900970526     C*------------------------------------------------------------------------*
009000970526     C*
009100970526     C* NON CI SONO ERRORI IN ENTRATA
009200970526IF  1C     ERRL00        IFEQ      '0'
009300970526     C*
009400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009800970526     C*
009900970526     C* EMETTE FORMATO VIDEO
010000970526     C                   EXSR      BUTFMR
010100970526     C*
010200970526     C* GESTIONE TASTI FUNZIONALI
010300970526     C                   EXSR      FUNCON
010400970526E   2C                   ENDDO
010500970526     C*
010600970526     C* CI SONO ERRORI IN ENTRATA
010700970527X   1C                   ELSE
010800991025     C                   MOVEL     '1'           S20err
010900991025     C                   MOVEL     DSMSMS        S20msg
011000970526E   1C                   ENDIF
011100970526     C* FINE PGM
011200000705     c                   EXSR      finsr
011300970526     C                   SETON                                        LR
011400970526     C*------------------------------------------------------------------------*
011500970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011600970526     C*------------------------------------------------------------------------*
011700970526     C     BUTFMR        BEGSR
011800970526     C*
011900970526     C* EMETTE LA TESTATA
012000000705     C                   WRITE     s728D01
012100970526     C*
012200970526     C* SPEDISCE EVENTUALE MESSAGGIO
012300970526     C                   EXSR      SNDMSG
012400970526     C*
012500970526     C* EMETTE LE FUNZIONI
012600000705     C                   WRITE     s728D02
012700970526     C*
012800970526     C* EMETTE IL VIDEO
012900000705     C                   EXFMT     s728D03
013000970526     C*
013100970526     C* REIMPOSTA FLAG DI ERRORE
013200970526     C                   MOVEL     '0'           ERRL00
013300970526     C*
013400970526     C                   ENDSR
013500970526     C*------------------------------------------------------------------------*
013600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013700970526     C*------------------------------------------------------------------------*
013800970526     C     SNDMSG        BEGSR
013900970526     C*
014000970526     C* PULISCE IL SFILE MESSAGGI
014100970526     C                   MOVEL     '1'           *IN40
014200000705     C                   WRITE     s728DMC
014300970526     C                   MOVEL     '0'           *IN40
014400970526     C*
014500970526     C* VISUALIZZA IL MESSAGGIO
014600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014700970526     C                   CALL      'XMSG'        XMSGPA
014800970526IF  1C     DSMSER        IFEQ      *ZEROS
014900970526     C                   MOVEL     DSMSKE        MSGKEY
015000970526E   1C                   ENDIF
015100970526     C                   Z-ADD     1             MSGULT
015200000705     C                   WRITE     s728DMS
015300000705     C                   WRITE     s728DMC
015400970526     C*
015500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015600970526     C                   MOVEL     *BLANKS       DSMSMS
015700970526     C*
015800970526     C                   ENDSR
015900970526     C*------------------------------------------------------------------------*
016000970526     C* ENTGES - GESTIONE ENTER -> INVIO
016100970526     C*------------------------------------------------------------------------*
016200970526     C     ENTGES        BEGSR
016300970526     C*
016400970526     C* CONTROLLA I CAMPI IMMESSI
016500970526     C                   EXSR      DECODI
016600970526     C*
016700970526     C                   ENDSR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* F03GES - GESTIONE F03 -> FINE
017000970526     C*------------------------------------------------------------------------*
017100970526     C     F03GES        BEGSR
017200970526     C*
017300970526     C* IMPOSTA "PREMUTO F03"
017400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017500991025     C                   MOVEL     '1'           S20F03                         *DI PROCEDURA
017600970526     C*
017700970526     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
018000970526     C*------------------------------------------------------------------------*
018100970526     C     F06GES        BEGSR
018200970526     C*
018300970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
018400970526IF  1C     DEPOP1        IFEQ      'F06'
018500970526     C                   EXSR      INSERI
018600970526E   1C                   ENDIF
018700970526IF  1C     DEPOP1        IFEQ      'O02'
018800970526     C                   EXSR      MODIFI
018900970526E   1C                   ENDIF
019000000707IF  1C     DEPOP1        IFEQ      'O04'
019100000707     C                   EXSR      DELETA
019200000707E   1C                   ENDIF
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* F12GES - GESTIONE F12 -> RITORNO
019700970526     C*------------------------------------------------------------------------*
019800970526     C     F12GES        BEGSR
019900970526     C*
020000970526     C* IMPOSTA "PREMUTO F12"
020100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020200991025     C                   MOVEL     '1'           S20F12                         *DI PROCEDURA
020300970526     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F24GES        BEGSR
020900970526     C*
021000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021100970526     C*
021200970526     C                   ENDSR
021300970526     C*------------------------------------------------------------------------*
021400970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
021500970526     C*------------------------------------------------------------------------*
021600970526     C     INSERI        BEGSR
021700970526     C*
021800970526     C* CONTROLLA I CAMPI IMMESSI
021900970526     C                   EXSR      DECODI
022000970526     C*
022100970526     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
022200000705IF  1C                   IF        ERRL00 = '0'                                 *NO ERRORI
022300000705     C                   EXSR      impvre
022400000705     C                   WRITE     tivre000
022500000705     C                   MOVEL     MSG(2)        S20MSG                         *MESSAGGIO OK
022600000706     C                   EXSR      SNDMSG
022700000706     C* PULISCE LA VIDEATA PER UN ALTRO INSERIMENTO
022800000706     C                   EXSR      AZZVID
022900000705     C                   ENDIF
023000970526     C*
023100970526     C                   ENDSR
023200970526     C*------------------------------------------------------------------------*
023300970526     C* MODIFI - OPERAZIONI DI MODIFICA
023400970526     C*------------------------------------------------------------------------*
023500970526     C     MODIFI        BEGSR
023600970526     C*
023700970526     C* CONTROLLA I CAMPI IMMESSI
023800970526     C                   EXSR      DECODI
023900000705     C*
024000000705     C* IMPOSTA I CAMPI DEL RECORD DA MODIFICARE
024100000705IF  1C                   IF        ERRL00 = '0'                                 *NO ERRORI
024200000705     C                   EXSR      impvre
024300000705     C                   UPDATE    tivre000
024400000705     C                   MOVEL     MSG(3)        S20MSG                         *MESSAGGIO OK
024500000706     C                   EXSR      SNDMSG
024600000706     C* DOPO MNODIFICA OK ==> ESCO
024700000706     C                   EXSR      F12GES
024800000705     C                   ENDIF
024900970526     C*
025000970526     C                   ENDSR
025100000707     C*------------------------------------------------------------------------*
025200000707     C* DELETA - OPERAZIONI DI CANCELLAZIONE
025300000707     C*------------------------------------------------------------------------*
025400000707     C     DELETA        BEGSR
025500000707     C*
025600000707     C     ERRL00        IFEQ      '0'
025700000707     C                   DELETE    tivre000
025800000707     C                   MOVEL     MSG(4)        S20MSG                         *MESSAGGIO OK
025900000707     C                   EXSR      SNDMSG
026000000707     C* DOPO MNODIFICA OK ==> ESCO
026100000707     C                   EXSR      F12GES
026200000707     C                   ENDIF
026300000707     C*
026400000707     C                   ENDSR
026500970526     C*------------------------------------------------------------------------*
026600970526     C* DECODI - CONTROLLA CAMPI E DECODIFICA
026700970526     C*------------------------------------------------------------------------*
026800970526     C     DECODI        BEGSR
026900000626     C*
027000000710     C*
027100000710     C* REIMPOSTA INDICATORI E FLAG DI ERRORE
027200000710     C                   SETOFF                                       6566
027300000706     C                   MOVEL     '0'           ERRL00
027400000706     C*
027500000705     C* CONTROLLO TIPO FILE
027600000706     C     ERRL00        IFEQ      '0'
027700000706     C     VIDtfl        IFNE      *blank
027800000705     C                   MOVEL     VIDtfl        deptip
027900000705     C                   EXSR      cnttip
028000000706     C     ERRL00        IFEQ      '0'
028100000705     C                   MOVEL     deptip        VIDtfl
028200000705     C                   MOVEL     depdes        DEHtfl
028300000705     C                   ENDIF
028400000706     C                   ELSE
028500991008     C                   MOVEL     '1'           ERRL00
028600000706     C                   SETON                                        65
028700991027     C                   MOVEL     MSG(12)       DSMSMS
028800000706     C                   ENDIF
028900000706     C                   ENDIF
029000000707     C*
029100000707     C* CONTROLLO DIVERSITA' TIPO FILE "MASTER" E TIPO FILE "LEGATO"
029200000707     C     ERRL00        IFEQ      '0'
029300000707     C     VIDtip        IFEQ      VIDtfl
029400000707     C                   MOVEL     '1'           ERRL00
029500000707     C                   SETON                                        65
029600000707     C                   MOVEL     MSG(13)       DSMSMS
029700000707     C                   ENDIF
029800000707     C                   ENDIF
029900991026     C*
030000000706     C* CONTROLLO INESISTENZA RECORD X INSERIMENTO
030100000706     C     ERRL00        IFEQ      '0'
030200000706     C     DEPOP1        IFEQ      'F06'
030300000706     C* ESGUO LETTURA CON CHIAVE DA INSERIRE
030400000706     C                   MOVEL     VIDtfl        KREtfl
030500000706     C     KEYvre        CHAIN     tivre02l
030600000706     C                   IF        %found(tivre02l)
030700000706     C                   MOVEL     '1'           ERRL00
030800000706     C                   SETON                                        65
030900000706     C                   MOVEL     MSG(8)        DSMSMS                         *MESSAGGIO
031000000706     C                   ENDIF
031100000706     C                   ENDIF
031200000706     C                   ENDIF
031300000707     C*
031400000711     C* CONTROLLO ESISTENZA RECORD X CANCELLAZIONE O X MODIFICA
031500000707     C     ERRL00        IFEQ      '0'
031600000707     C     DEPOP1        IFEQ      'O04'
031700000711     C     DEPOP1        OREQ      'O02'
031800000711     C* ESGUO LETTURA CON CHIAVE DA CANCELLARE o MODIFICARE
031900000707     C                   MOVEL     VIDtfl        KREtfl
032000000707     C     KEYvre        CHAIN     tivre02l
032100000707     C                   IF        not %found(tivre02l)
032200000707     C                   MOVEL     '1'           ERRL00
032300000707     C                   SETON                                        65
032400000707     C                   MOVEL     MSG(16)       DSMSMS                         *MESSAGGIO
032500000707     C                   ENDIF
032600000707     C                   ENDIF
032700000707     C                   ENDIF
032800000707     C*
032900000707     C* CONTROLLO ESISTENZA ABILITAZIONE AL TIPO FILE IN REGOLE PER CLIENTE
033000000707     C     ERRL00        IFEQ      '0'
033100000710     C                   SETOFF                                       35
033200000710     C     KEYvir        SETLL     tivir02l
033300000710     C                   IF        %found(tivir02l)
033400000710     C     KEYvir        READE     tivir02l
033500000710     C                   DOW       (not %eof) and (*in40=*off)
033600000710     C                   IF        virdti <= datcor and
033700000710     C                             virdtf >= datcor
033800000710     C                   Z-ADD     virdti        depdti
033900000710     C                   SETON                                        35
034000000710     C                   ENDIF
034100000710     C     KEYvir        READE     tivir02l
034200000710     C                   ENDDO
034300000710     C                   ENDIF
034400000710     C* alla fine del ciclo testo se ho trovato un record valido
034500000710     C                   IF        *in35 = *off
034600000707     C                   MOVEL     '1'           ERRL00
034700000707     C                   SETON                                        65
034800000710     C                   MOVEL     MSG(9)        DSMSMS                         *MESSAGGIO
034900000707     C                   ENDIF
035000000707     C                   ENDIF
035100000707     C*
035200000707     C* CONTROLLO VALORIZZAZIONE NUMERO DI SEQUENZA
035300000707     C     ERRL00        IFEQ      '0'
035400000707     C     VIDord        IFEQ      *zeros
035500000707     C                   MOVEL     '1'           ERRL00
035600000707     C                   SETON                                        66
035700000707     C                   MOVEL     MSG(15)       DSMSMS                         *MESSAGGIO
035800000707     C                   ENDIF
035900000707     C                   ENDIF
036000000706     C*
036100000706     C                   ENDSR
036200991027     C*------------------------------------------------------------------------*
036300991027     C* cnttip - Controlla esistenza tipo file su tivt00f
036400991027     C*------------------------------------------------------------------------*
036500000705     C     CNTTIP        BEGSR
036600000705     C*
036700000705     C                   CLEAR                   TIBS02DS
036800000626     C*
036900000705     C     '?'           SCAN      deptip                                 30
037000000705     C*  Ricerca
037100000705     C                   EVAL      t02tla = 'L'
037200991108    1C                   IF        *IN30 = *ON
037300000705     C                   MOVEL     'R'           t02mod
037400000705     C                   ELSE
037500000705     C                   MOVEL     'C'           t02mod
037600000705     C                   MOVEL(p)  deptip        t02ke1
037700000705     C                   ENDIF
037800000705     C                   MOVEL     'ITF'         t02cod
037900000705     C                   MOVEL     knsif         t02sif
038000991108     C                   CALL      'TIBS02R'
038100000705     C                   PARM                    KPJBA
038200000705     C                   PARM                    TIBS02DS
038300000705     C
038400000705     C*
038500991108    2C                   IF        T02err = *blanks
038600000705     C                   MOVEL     T02ke1        deptip
038700020218     C                   MOVEL     T02uni        DITF
038800020218     C                   MOVEL     §ITFDESC      depdes
038900000705     C                   ELSE
039000000705     C                   MOVEL     '1'           ERRL00
039100000705     C                   MOVEL     *blank        depdes
039200000710     C                   SETON                                        65
039300000706     C                   MOVEL     MSG(11)       DSMSMS
039400991108     C                   ENDIF
039500000705     C*
039600000705     C                   ENDSR
039700991210     C*------------------------------------------------------------------------*
039800000705     C* FINSR - OPERAZIONI FINALI
039900991210     C*------------------------------------------------------------------------*
040000991210     C     FINSR         BEGSR
040100991210     C*
040200991210     C* CHIUDE I FILE DEI PGM CHIAMATI
040300991210     C                   CLEAR                   bs69ds
040400991210     C                   CLEAR                   acods
040500991210     C                   CLEAR                   indds
040600991210     C                   CLEAR                   clpds
040700991210     C                   CLEAR                   clsds
040800991210     C                   MOVEL     'C'           i69tla
040900991210     C                   MOVEL     knsif         i69sif
041000991210     C                   CALL      'TIBS69R'
041100991210     C                   PARM                    bs69ds
041200991210     C                   PARM                    acods
041300991210     C                   PARM                    indds
041400991210     C                   PARM                    clpds
041500991210     C                   PARM                    clsds
041600991210     C*
041700991210     C                   ENDSR
041800991026     C*------------------------------------------------------------------------*
041900991026     C* CHKDA8 - CONTROLLA LA DATA (8)
042000991026     C*------------------------------------------------------------------------*
042100991026     C     CHKDA8        BEGSR
042200991026     C*
042300000705     C     *LIKE         DEFINE    VIDdat        WDA8
042400000705     C     *LIKE         DEFINE    VIDdat        DA8W
042500991026     C*
042600000706     C                   Z-ADD     *zeros        G08DAT
042700000706     C                   Z-ADD     WDA8          G08INV
042800000706     C                   MOVEL     '3'           G08ERR
042900991026     C                   CALL      'XSRDA8'
043000991026     C                   PARM                    WLBDA8
043100000706     C                   Z-ADD     G08INV        WDA8
043200000706     C                   Z-ADD     G08DAT        DA8W
043300991026IF  1C     G08ERR        IFEQ      '1'
043400991026     C                   MOVEL     '1'           ERRL00
043500991026E   1C                   ENDIF
043600991026     C*
043700991026     C                   ENDSR
043800970526     C*------------------------------------------------------------------------*
043900970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
044000970526     C*------------------------------------------------------------------------*
044100970526     C     IREVID        BEGSR
044200970526     C* CAMPI
044300000705     C                   MOVEL     S20ksc        VIDksc
044400000705     C                   MOVEL     S20tip        VIDtip
044500000706     C                   MOVEL     VIDtip        deptip
044600000706     C                   EXSR      cnttip
044700000706IF  1C     ERRL00        IFNE      '1'
044800000706     C                   MOVEL     deptip        VIDtip
044900000706     C                   MOVEL     depdes        DEHtip
045000000706     C                   ENDIF
045100000706     C                   Z-ADD     S20dti        WDA8
045200000706     C                   EXSR      CHKDA8
045300000706     C                   IF        ERRL00<>'1'
045400000706     C                   Z-ADD     DA8W          VIDdat
045500000706     C                   ENDIF
045600000705     C                   MOVEL     Ptfl          VIDtfl
045700000705     C                   MOVEL     Prag          VIDrag
045800000707     C                   Z-ADD     Pord          VIDord
045900000705     C*
046000970526     C                   ENDSR
046100970526     C*------------------------------------------------------------------------*
046200970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
046300970526     C*------------------------------------------------------------------------*
046400970526     C     AZZVID        BEGSR
046500000705     C*
046600000705     C                   MOVEL     *blanks       VIDtfl
046700000705     C                   MOVEL     *blanks       DEHtfl
046800000707     C                   Z-ADD     *zeros        VIDord
046900970526     C*
047000970526     C                   ENDSR
047100970526     C*------------------------------------------------------------------------*
047200000705     C* IMPVRE - IMPOSTA IL RECORD DA SCRIVERE SUL FILE
047300970526     C*------------------------------------------------------------------------*
047400000705     C     IMPVRE        BEGSR
047500970526     C*
047600000705     C                   MOVEL     VIDksc        vreksc
047700000705     C                   MOVEL     VIDtip        vretip
047800000705     C                   MOVEL     VIDtfl        vretfl
047900000706     C                   Z-ADD     s20dti        vredti
048000000710     C                   Z-ADD     depdti        vredil
048100000707     C                   Z-ADD     VIDord        vreord
048200970526     C*
048300970526     C                   ENDSR
048400970526     C*------------------------------------------------------------------------*
048500970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
048600970526     C*------------------------------------------------------------------------*
048700970526     C     IMPFUN        BEGSR
048800970526     C*
048900970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
049000970526IF  1C     DEPOP1        IFEQ      'O05'
049100970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
049200970526     C                   MOVEL     'N'           FUDATT
049300970526     C                   MOVEL     FUDRIG        FUD(6)
049400970526E   1C                   ENDIF
049500970526     C*
049600970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
049700970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
049800970526     C                   EXSR      FUNGES
049900970526     C*
050000970526     C* IMPOSTA LA PROTEZIONE CAMPI IN BASE ALLA FUNZIONE DI ENTRATA:
050100970526     C*---
050200970526     C* 90 -> PROTEZIONE CHIAVE
050300970526     C* 91 -> PROTEZIONE TOTALE
050400970526     C*---
050500970528IF  1C     DEPOP0        IFEQ      'P02'                                        G E S T I O N E
050600970526IF  2C     DEPOP1        IFEQ      'F06'                                        *INSERIMENTO
050700970526     C                   SETOFF                                       90        *NESSUNA PROTEZIONE
050800970526     C                   SETOFF                                       91
050900970526E   2C                   ENDIF
051000970526IF  2C     DEPOP1        IFEQ      'O02'                                        *MODIFICA
051100991014IF  2C     DEPOP1        oreq      'O01'                                        *MODIFICA
051200970526     C                   SETON                                        90        *PROTEZIONE CHIAVE
051300970526     C                   SETOFF                                       91        *NO PROTEZIONE CAMPI
051400970526E   2C                   ENDIF
051500970526IF  2C     DEPOP1        IFEQ      'O04'                                        *ANNULLAMENTO
051600970526     C     DEPOP1        OREQ      'O05'                                        *VISUALIZZAZIONE
051700970526     C     DEPOP1        OREQ      'O07'                                        *RIPRISTINO
051800970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
051900970526     C                   SETON                                        91
052000970526E   2C                   ENDIF
052100970526X   1C                   ELSE                                                   *I N T E R R O G A
052200970526     C                   SETON                                        90        *TUTTE LE PROTEZIONI
052300970526     C                   SETON                                        91
052400970526E   1C                   ENDIF
052500970526     C*
052600970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
052700970526IF  1C     DEPOP1        IFEQ      'O02'
052800970526     C                   MOVEL     DEC(1)        VIDFUN
052900970526E   1C                   ENDIF
053000991014IF  1C     DEPOP1        IFEQ      'O03'
053100991014     C                   MOVEL     DEC(6)        VIDFUN
053200991014E   1C                   ENDIF
053300970526IF  1C     DEPOP1        IFEQ      'O04'
053400970526     C                   MOVEL     DEC(2)        VIDFUN
053500970526E   1C                   ENDIF
053600970526IF  1C     DEPOP1        IFEQ      'O05'
053700970526     C                   MOVEL     DEC(3)        VIDFUN
053800970526E   1C                   ENDIF
053900970526IF  1C     DEPOP1        IFEQ      'O07'
054000970526     C                   MOVEL     DEC(4)        VIDFUN
054100970526E   1C                   ENDIF
054200970526IF  1C     DEPOP1        IFEQ      'F06'
054300970526     C                   MOVEL     DEC(5)        VIDFUN
054400970526E   1C                   ENDIF
054500970526     C*
054600970526     C                   ENDSR
054700970526     C*------------------------------------------------------------------------*
054800970526     C* *INZSR - OPERAZIONI INIZIALI
054900970526     C*------------------------------------------------------------------------*
055000970526     C     *INZSR        BEGSR
055100970526     C*---
055200970526     C* RICEVIMENTO PARAMETRI
055300970526     C*---
055400970526     C     *ENTRY        PLIST
055500000705     C                   PARM                    KPJBA
055600000705     C                   PARM                    Prag             25
055700000705     C                   PARM                    Ptfl              2
055800000707     C                   PARM                    Pord              3 0
055900000705     C*
056000000705     C                   MOVEL     KPJBU         tis720
056100000706     C*
056200000706     C* IMPOSTA I CAMPI A VIDEO
056300000706     C                   EXSR      IREVID
056400970526     C*
056500970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
056600970526     C     XMSGPA        PLIST
056700970526     C                   PARM                    DSMSMS
056800970526     C                   PARM                    DSMSPG
056900970526     C                   PARM                    DSMSKE
057000970526     C                   PARM                    DSMSER
057100970526     C*
057200970526     C* IMPOSTA LA DS -TASTI PREMUTI-
057300970526     C                   CALL      'XKEY'
057400970526     C                   PARM                    DSKEY
057500970526     C*---
057600970526     C* VARIABILI NON RIFERITE AL DATA BASE
057700970526     C*---
057800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
057900991025     C     *LIKE         DEFINE    S20OP0        DEPOP0
058000991025     C     *LIKE         DEFINE    S20OP1        DEPOP1
058100991025     C     *LIKE         DEFINE    S20OP0        WOP0
058200991025     C     *LIKE         DEFINE    S20OP1        WOP1
058300970526     C*---
058400000705     C* VARIABILI DI CONTROLLO E DI WRK
058500970526     C*---
058600970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
058700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
058800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
058900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
059000000705     C                   MOVEL     *blanks       DEPDES           30
059100000705     C                   MOVEL     *blanks       DEPTIP            2
059200970526     C*
059300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
059400000705     C                   MOVEL     'TIS728R'     DSMSPG
059500000705     C                   MOVEL     'TIS728R'     MSGQUE
059600970526     C                   MOVEL     *BLANKS       MSGKEY
059700970526     C                   MOVEL     *BLANKS       DSMSMS
059800970526     C*---
059900970526     C* VARIABILI RIFERITE AL DATA BASE
060000970526     C*---
060100000707     C     *LIKE         DEFINE    vretfl        KREtfl
060200000710     C     *LIKE         DEFINE    virdti        depdti
060300970526     C*---
060400970526     C* CHIAVI DI LETTURA
060500970526     C*---
060600000705     C* LETTURA tivre02L - COMPLETA
060700000705     C     KEYvre        KLIST
060800000705     C                   KFLD                    VIDksc
060900000705     C                   KFLD                    VIDtip
061000000706     C                   KFLD                    s20dti
061100000707     C                   KFLD                    KREtfl
061200000707     C* LETTURA tivir02L - COMPLETA
061300000707     C     KEYvir        KLIST
061400000707     C                   KFLD                    VIDksc
061500000707     C                   KFLD                    VIDtfl
061600970528     C*
061700970528     C* CALCOLA LA DATA CORRENTE
061800970528     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
061900000710     C                   MOVE      WN14          WN8               8 0          *DATA (8)  G/M/AAAA
062000970528     C                   Z-ADD     WN8           G08DAT
062100970528     C                   Z-ADD     *ZEROS        G08INV
062200970528     C                   MOVEL     '0'           G08ERR
062300970528     C                   CALL      'XSRDA8'
062400970528     C                   PARM                    WLBDA8
062500000710     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORR AAAA/M/G
062600970526     C*
062700970526     C* DEPOSITA LE OPZIONI DI ENTRATA
062800991025     C                   MOVEL     S20OP0        DEPOP0                         *LIVELLO PROCEDURA
062900991025     C                   MOVEL     S20OP1        DEPOP1                         *LIVELLO PROGRAMMA
063000000707     C*
063100000707     C* PRIME CONSIDERAZIONI SUI PARAMETRI DI INPUT (OPZIONI)
063200000707     C                   IF        DEPOP1 = 'O02'
063300000707     C                   SETON                                        67
063400000707     C                   ENDIF
063500970526     C*
063600970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
063700970526     C                   EXSR      IMPFUN
063800970526     C*
063900970526     C                   ENDSR
064000970526     C*------------------------------------------------------------------------*
064100970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
064200970526     C*------------------------------------------------------------------------*
064300970526     C     FUNGES        BEGSR
064400970526     C*
064500970526     C* PULISCO CAMPI DI LAVORO
064600970526     C                   Z-ADD     0             W                 5 0
064700970526     C*
064800970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
064900970526     C* E CARICO TABELLA FUNZIONI ATTIVE
065000970526     C     1             DO        24            I
065100970526     C                   MOVEL     FUD(I)        FUDRIG
065200970526     C*
065300970526     C* PULIZIA FUNZIONI PRECEDENTI
065400970526     C                   MOVEL     *BLANKS       FUV(I)
065500970526     C*
065600970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
065700970526     C* (SALTA F24)
065800970526     C     FUDATT        IFEQ      'S'
065900970526     C     FUDFUN        ANDNE     '24'
066000970526     C                   ADD       1             W
066100970526     C                   MOVEL     FUDDES        FUV(W)
066200970526     C                   END
066300970526     C                   END                                                    DO WHILE
066400970526     C*
066500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
066600970526     C* ALTRIMENTI DISATTIVO F24
066700970526     C     1             DO        24            I
066800970526     C                   MOVEL     FUD(I)        FUDRIG
066900970526     C*
067000970526     C     FUDFUN        IFEQ      '24'
067100970526     C     W             IFGT      6
067200970526     C                   MOVEL     'S'           FUDATT
067300970526     C                   ELSE
067400970526     C                   MOVEL     *BLANKS       FUDATT
067500970526     C                   END                                                    W GT 8
067600970526     C                   MOVEL     FUDRIG        FUD(I)
067700970526     C                   END
067800970526     C                   END                                                    DO WHILE
067900970526     C*
068000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
068100970526     C     W             DIV       6             I
068200970526     C                   MVR                     Z
068300970526     C*
068400970526     C     Z             IFNE      0
068500970526     C     I             ADD       1             Z
068600970526     C                   ELSE
068700970526     C                   Z-ADD     I             Z                 5 0
068800970526     C                   END
068900970526     C*
069000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
069100970526     C                   ADD       1             FUNGRU
069200970526     C     FUNGRU        IFGT      Z
069300970526     C     FUNGRU        ORLT      1
069400970526     C                   Z-ADD     1             FUNGRU
069500970526     C                   END
069600970526     C*
069700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
069800970526     C     FUNGRU        IFEQ      1
069900970526     C                   MOVEL     FUV(1)        FUN001
070000970526     C                   MOVEL     FUV(2)        FUN002
070100970526     C                   MOVEL     FUV(3)        FUN003
070200970526     C                   MOVEL     FUV(4)        FUN004
070300970526     C                   MOVEL     FUV(5)        FUN005
070400970526     C                   MOVEL     FUV(6)        FUN006
070500970526     C                   END
070600970526     C*
070700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
070800970526     C     FUNGRU        IFEQ      2
070900970526     C                   MOVEL     FUV(7)        FUN001
071000970526     C                   MOVEL     FUV(8)        FUN002
071100970526     C                   MOVEL     FUV(9)        FUN003
071200970526     C                   MOVEL     FUV(10)       FUN004
071300970526     C                   MOVEL     FUV(11)       FUN005
071400970526     C                   MOVEL     FUV(12)       FUN006
071500970526     C                   END
071600970526     C*
071700970526     C* TERZO GRUPPO DI 6 FUNZIONI
071800970526     C     FUNGRU        IFEQ      3
071900970526     C                   MOVEL     FUV(13)       FUN001
072000970526     C                   MOVEL     FUV(14)       FUN002
072100970526     C                   MOVEL     FUV(15)       FUN003
072200970526     C                   MOVEL     FUV(16)       FUN004
072300970526     C                   MOVEL     FUV(17)       FUN005
072400970526     C                   MOVEL     FUV(18)       FUN006
072500970526     C                   END
072600970526     C*
072700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
072800970526     C     FUNGRU        IFEQ      4
072900970526     C                   MOVEL     FUV(19)       FUN001
073000970526     C                   MOVEL     FUV(20)       FUN002
073100970526     C                   MOVEL     FUV(21)       FUN003
073200970526     C                   MOVEL     FUV(22)       FUN004
073300970526     C                   MOVEL     *BLANKS       FUN005
073400970526     C                   MOVEL     *BLANKS       FUN006
073500970526     C                   END
073600970526     C*
073700970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
073800970526     C                   MOVEL     *BLANKS       FUN007
073900970526     C                   MOVEL     *BLANKS       FUN008
074000970526     C*
074100970526     C     1             DO        24            I
074200970526     C                   MOVEL     FUD(I)        FUDRIG
074300970526     C     FUDFUN        IFEQ      '24'
074400970526     C     FUDATT        ANDEQ     'S'
074500970526     C                   MOVEL     FUDDES        FUN008
074600970526     C                   END
074700970526     C                   END                                                    DO
074800970526     C*
074900970526     C                   ENDSR
075000970526     C*------------------------------------------------------------------------*
075100970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
075200970526     C*------------------------------------------------------------------------*
075300970526     C     FUNCON        BEGSR
075400970526     C*
075500970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
075600970526     C                   EXSR      FUNVAR
075700970526     C*
075800970526     C* PULISCO CAMPI DI LAVORO
075900970526     C                   Z-ADD     0             I                 5 0
076000970526     C                   Z-ADD     0             W                 5 0
076100970526     C*
076200970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
076300970526     C     FUNKEY        IFNE      *BLANKS
076400970526     C     1             DO        24            I
076500970526     C                   MOVEL     FUD(I)        FUDRIG
076600970526     C*
076700970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
076800970526     C     FUNKEY        IFEQ      FUDFUN
076900970526     C     FUDATT        ANDEQ     'S'
077000970526     C                   Z-ADD     I             W
077100970526     C                   END
077200970526     C                   END                                                    DO WHILE
077300970526     C*
077400970526     C* FUNZIONE NON ATTIVA (ERRORE)
077500970526     C     W             IFEQ      0
077600970526     C*
077700970526     C* EMISSIONE MESSAGGIO DI ERRORE
077800970526     C                   MOVEL     MSG(1)        DSMSMS
077900970526     C*
078000970526     C                   END                                                    W=0
078100970526     C                   END                                                    FUNKEY NE *BLANKS
078200970526     C*
078300970526     C* FUNZIONE ATTIVA (PROCEDI ...)
078400970526     C     W             IFNE      0
078500970526     C     FUNKEY        OREQ      *BLANKS
078600970526     C     FUNKEY        IFEQ      *BLANKS
078700970526     C                   EXSR      ENTGES
078800970526     C                   END
078900970526     C     FUNKEY        IFEQ      '03'
079000970526     C                   EXSR      F03GES
079100970526     C                   END
079200970526     C     FUNKEY        IFEQ      '06'
079300970526     C                   EXSR      F06GES
079400970526     C                   END
079500970526     C     FUNKEY        IFEQ      '12'
079600970526     C                   EXSR      F12GES
079700970526     C                   END
079800970526     C     FUNKEY        IFEQ      '24'
079900970526     C                   EXSR      F24GES
080000970526     C                   END
080100970526     C                   END
080200970526     C*
080300970526     C                   ENDSR
080400970526     C*------------------------------------------------------------------------*
080500970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
080600970526     C*------------------------------------------------------------------------*
080700970526     C     FUNVAR        BEGSR
080800970526     C*
080900970526     C* INIZIALIZZA VARIABILE -FUNKEY-
081000970526     C*
081100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
081200970526     C     KEY           IFEQ      ENTER
081300970526     C                   MOVEL     *BLANKS       FUNKEY
081400970526     C                   END
081500970526     C     KEY           IFEQ      F1
081600970526     C                   MOVEL     '01'          FUNKEY
081700970526     C                   END
081800970526     C     KEY           IFEQ      F2
081900970526     C                   MOVEL     '02'          FUNKEY
082000970526     C                   END
082100970526     C     KEY           IFEQ      F3
082200970526     C                   MOVEL     '03'          FUNKEY
082300970526     C                   END
082400970526     C     KEY           IFEQ      F4
082500970526     C                   MOVEL     '04'          FUNKEY
082600970526     C                   END
082700970526     C     KEY           IFEQ      F5
082800970526     C                   MOVEL     '05'          FUNKEY
082900970526     C                   END
083000970526     C     KEY           IFEQ      F6
083100970526     C                   MOVEL     '06'          FUNKEY
083200970526     C                   END
083300970526     C     KEY           IFEQ      F7
083400970526     C                   MOVEL     '07'          FUNKEY
083500970526     C                   END
083600970526     C     KEY           IFEQ      F8
083700970526     C                   MOVEL     '08'          FUNKEY
083800970526     C                   END
083900970526     C     KEY           IFEQ      F9
084000970526     C                   MOVEL     '09'          FUNKEY
084100970526     C                   END
084200970526     C     KEY           IFEQ      F10
084300970526     C                   MOVEL     '10'          FUNKEY
084400970526     C                   END
084500970526     C     KEY           IFEQ      F11
084600970526     C                   MOVEL     '11'          FUNKEY
084700970526     C                   END
084800970526     C     KEY           IFEQ      F12
084900970526     C                   MOVEL     '12'          FUNKEY
085000970526     C                   END
085100970526     C     KEY           IFEQ      F13
085200970526     C                   MOVEL     '13'          FUNKEY
085300970526     C                   END
085400970526     C     KEY           IFEQ      F14
085500970526     C                   MOVEL     '14'          FUNKEY
085600970526     C                   END
085700970526     C     KEY           IFEQ      F15
085800970526     C                   MOVEL     '15'          FUNKEY
085900970526     C                   END
086000970526     C     KEY           IFEQ      F16
086100970526     C                   MOVEL     '16'          FUNKEY
086200970526     C                   END
086300970526     C     KEY           IFEQ      F17
086400970526     C                   MOVEL     '17'          FUNKEY
086500970526     C                   END
086600970526     C     KEY           IFEQ      F18
086700970526     C                   MOVEL     '18'          FUNKEY
086800970526     C                   END
086900970526     C     KEY           IFEQ      F19
087000970526     C                   MOVEL     '19'          FUNKEY
087100970526     C                   END
087200970526     C     KEY           IFEQ      F20
087300970526     C                   MOVEL     '20'          FUNKEY
087400970526     C                   END
087500970526     C     KEY           IFEQ      F21
087600970526     C                   MOVEL     '21'          FUNKEY
087700970526     C                   END
087800970526     C     KEY           IFEQ      F22
087900970526     C                   MOVEL     '22'          FUNKEY
088000970526     C                   END
088100970526     C     KEY           IFEQ      F23
088200970526     C                   MOVEL     '23'          FUNKEY
088300970526     C                   END
088400970526     C     KEY           IFEQ      F24
088500970526     C                   MOVEL     '24'          FUNKEY
088600970526     C                   END
088700970526     C     KEY           IFEQ      ROLLUP
088800970526     C                   MOVEL     'RU'          FUNKEY
088900970526     C                   END
089000970526     C     KEY           IFEQ      HELP
089100970526     C                   MOVEL     'HP'          FUNKEY
089200970526     C                   END
089300970526     C                   ENDSR
089400970526     C*------------------------------------------------------------------------*
089500970526     C* NOTE COSTRUZIONE SCHIERE
089600970526     C*------------------------------------------------------------------------*
089700970526     C*
089800970526     C* -MSG-MESSAGGI
089900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
090000970526     C* (NON TOCCARE MSG,1 E MSG,2)
090100970526     C*
090200970526     C* -CMD-COMANDI
090300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
090400970526     C*
090500970526     C* -OPD-OPZIONI
090600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
090700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
090800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
090900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
091000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
091100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
091200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
091300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
091400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
091500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
091600970526     C*
091700970526     C* -FUD-FUNZIONI
091800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
091900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
092000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
092100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
092200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
092300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
092400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
092500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
092600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
092700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
092800970526     O*------------------------------------------------------------------------*
092900970526** DEC - DECODIFICHE
093000970526    MODIFICA                                                      1
093100970526  ANNULLAMENTO                                                    2
093200970526VISUALIZZAZIONE                                                   3
093300970526   RIPRISTINO                                                     4
093400970526  INSERIMENTO                                                     5
093500991014     COPIA                                                        6
093600970526** MSG - MESSAGGI
093700000705TIS728R- Tasto funzionale non gestito                                          1
093800000705TIS728R- Inserimento effettuato                                                2
093900000705TIS728R- Modifica effettuata                                                   3
094000000705TIS728R- Annullamento effettuato                                               4
094100000705TIS728R- Ripristino effettuato                                                 5
094200000705TIS728R- Richiesta modifica per record inesistente                             6
094300000705TIS728R- Legame già presente, impossibile apportare la modifica                7
094400000705TIS728R- Richiesto inserimento per record già esistente                        8
094500000707TIS728R- Errore, il cliente non è abilitato il tipo file da "legare"           9
094600000705TIS728R- Il codice tipo file non è fra quelli presenti in anagrafico           10
094700000706TIS728R- Tipo file errato o non presente in tabella                            11
094800000705TIS728R- Campo obbligatorio                                                    12
094900000707TIS728R- I tipi file coincidono, impossibile effettuare richiesta              13
095000000705TIS728R- Data errata                                                           14
095100000707TIS728R- Campo obbligatorio, inserire un valore progressivo                    15
095200000711TIS728R- Richiesto operazioine per record inesistente                          16
095300000710TIS728R- 17
095400000706TIS728R- 18
095500000706TIS728R- 19                                                                    19
095600000706TIS728R- 20                                                                    20
095700970526** FUD - FUNZIONI
095800970526                   -        (S/N)=N                        funz.01¬
095900970526                   -        (S/N)=N                        funz.02¬
096000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
096100970526                   -        (S/N)=N                        funz.04¬
096200970526                   -        (S/N)=N                        funz.05¬
096300970526F06=Aggiornamento  -        (S/N)=S                        funz.06¬
096400970527                   -        (S/N)=N                        funz.07¬
096500970527                   -        (S/N)=N                        funz.08¬
096600970527                   -        (S/N)=N                        funz.09¬
096700970527                   -        (S/N)=N                        funz.10¬
096800970526                   -        (S/N)=N                        funz.11¬
096900970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
097000970526                   -        (S/N)=N                        funz.13¬
097100970526                   -        (S/N)=N                        funz.14¬
097200970526                   -        (S/N)=N                        funz.15¬
097300970526                   -        (S/N)=N                        funz.16¬
097400970526                   -        (S/N)=N                        funz.17¬
097500970526                   -        (S/N)=N                        funz.18¬
097600970526                   -        (S/N)=N                        funz.19¬
097700970526                   -        (S/N)=N                        funz.20¬
097800970526                   -        (S/N)=N                        funz.21¬
097900970526                   -        (S/N)=N                        funz.22¬
098000970526                   -        (S/N)=N                        funz.23¬
098100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
