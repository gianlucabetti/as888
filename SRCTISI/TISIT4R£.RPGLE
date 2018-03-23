000100970526     H*------------------------------------------------------------------------*
000200040621     H* Gestione sinonimi DESTINATARI DISAGIATI/SUPERMERCATI/APPUNTAMENTI
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800020415     FTISIT4D   CF   E             WORKSTN INFDS(DEVDS1)
000900020415     F                                     SFILE(SIT4DMS:MSGULT)
001000020415     Fazdss01l  UF A E           K DISK
001100040116     Fazdst01l  IF   E           K DISK
001200040621     Fazdstp1l  UF   E           K DISK    PREFIX(w_)
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900020416     D DEC             S             66    DIM(7) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300040319     D MSG             S             78    DIM(21) CTDATA PERRCD(1)
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
005700970526     D*------------
005800000407     D* DS PROVINCIE
005900000407     D*------------------
006000000407     D DSPR          E DS
006100970528     D*------------------
006200970526     D* ARCHITETTURA
006300970526     D*------------
006400970526     D KPJBA         E DS
006500000407     D*------------------
006600000407     D* DS "XSRDA8" - INVERSIONE DATA (8)
006700000407     D*------------------
006800000407     D WLBDA8          DS                  INZ
006900000407     D  G08DAT                 1      8  0
007000000407     D  G08INV                 9     16  0
007100000407     D  G08ERR                17     17
007200000407     D  G08TGI                18     22  0
007300020412     D*------------------
007400020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
007500020412     D*------------------
007600020412     D TISIT0DS      E DS
007700020424     D WRKT0DS       E DS                  EXTNAME(TISIT0DS)
007800020424     D                                     PREFIX(w_)
007900040625     D*------------------
008000040625     D* DS DI RIDEFINIZIONE PARAMETRI IN INPUT
008100040625     D*------------------
008200040625     D PARAM           DS
008300040625     D  DEPOP0                        3
008400040625     D  DEPPRO                        1
008500040625     D  DEPLNA                        3  0
008600020412     D*------------------
008700020412     D* DS X CONTROLLO DATI DI "INSTRADAMENTO"
008800020412     D*------------------
008900020412     D TISI95DS      E DS                  INZ
009000970526     C*------------------------------------------------------------------------*
009100970526     C* MAIN LINES
009200970526     C*------------------------------------------------------------------------*
009300970526     C*
009400970526     C* NON CI SONO ERRORI IN ENTRATA
009500020426     C     ERRL00        IFEQ      '0'
009600970526     C*
009700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009800020426     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010100970526     C*
010200970526     C* EMETTE FORMATO VIDEO
010300970526     C                   EXSR      BUTFMR
010400970526     C*
010500970526     C* GESTIONE TASTI FUNZIONALI
010600970526     C                   EXSR      FUNCON
010700020426     C                   ENDDO
010800970526     C*
010900020426     C                   ENDIF
011000020412     C*
011100020412     C* OPERAZIONI FINALI
011200020412     C                   EXSR      FINSR
011300970526     C* FINE PGM
011400970526     C                   SETON                                        LR
011500970526     C*------------------------------------------------------------------------*
011600970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011700970526     C*------------------------------------------------------------------------*
011800970526     C     BUTFMR        BEGSR
011900970526     C*
012000970526     C* EMETTE LA TESTATA
012100020415     C                   WRITE     SIT4D01
012200970526     C*
012300970526     C* SPEDISCE EVENTUALE MESSAGGIO
012400970526     C                   EXSR      SNDMSG
012500970526     C*
012600970526     C* EMETTE LE FUNZIONI
012700020415     C                   WRITE     SIT4D02
012800970526     C*
012900970526     C* EMETTE IL VIDEO
013000020415     C                   EXFMT     SIT4D03
013100970526     C*
013200970526     C* REIMPOSTA FLAG DI ERRORE
013300970526     C                   MOVEL     '0'           ERRL00
013400970526     C*
013500970526     C                   ENDSR
013600970526     C*------------------------------------------------------------------------*
013700970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013800970526     C*------------------------------------------------------------------------*
013900970526     C     SNDMSG        BEGSR
014000970526     C*
014100970526     C* PULISCE IL SFILE MESSAGGI
014200970526     C                   MOVEL     '1'           *IN40
014300020415     C                   WRITE     SIT4DMC
014400970526     C                   MOVEL     '0'           *IN40
014500970526     C*
014600970526     C* VISUALIZZA IL MESSAGGIO
014700970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014800970526     C                   CALL      'XMSG'        XMSGPA
014900020426     C     DSMSER        IFEQ      *ZEROS
015000970526     C                   MOVEL     DSMSKE        MSGKEY
015100020426     C                   ENDIF
015200970526     C                   Z-ADD     1             MSGULT
015300020415     C                   WRITE     SIT4DMS
015400020415     C                   WRITE     SIT4DMC
015500970526     C*
015600970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
015700970526     C                   MOVEL     *BLANKS       DSMSMS
015800970526     C*
015900970526     C                   ENDSR
016000970526     C*------------------------------------------------------------------------*
016100970526     C* ENTGES - GESTIONE ENTER -> INVIO
016200970526     C*------------------------------------------------------------------------*
016300970526     C     ENTGES        BEGSR
016400020424     C*
016500040322     C* SOLO SE OPZIONE DIVERSA DA ANNULLAMENTO E/O RIPRISTINO
016600020424     C                   IF        IT0OP1 = 'O04' OR
016700020424     C                             IT0OP1 = 'O07'
016800020424     C                   ELSE
016900970526     C*
017000970526     C* CONTROLLA I CAMPI IMMESSI
017100970526     C                   EXSR      DECODI
017200970526     C*
017300970526     C* SE DA VISUALIZZAZIONE CON ENTER RITORNA AL PGM CHIAMANTE
017400020424     C     IT0OP1        IFEQ      'O05'
017500970526     C                   MOVEL     '1'           FINL00
017600020424     C                   ENDIF
017700970526     C*
017800020424     C                   ENDIF
017900020424     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F03GES - GESTIONE F03 -> FINE
018300970526     C*------------------------------------------------------------------------*
018400970526     C     F03GES        BEGSR
018500970526     C*
018600970526     C* IMPOSTA "PREMUTO F03"
018700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
018800970526     C*
018900970526     C                   ENDSR
019000970526     C*------------------------------------------------------------------------*
019100970526     C* F06GES - GESTIONE F06 -> AGGIORNAMENTO
019200970526     C*------------------------------------------------------------------------*
019300970526     C     F06GES        BEGSR
019400970526     C*
019500970527     C* LANCIA LA ROUTINE DI CONFERMA INTERESSATA
019600020412     C     IT0OP1        IFEQ      'F06'
019700970526     C                   EXSR      INSERI
019800020412     C                   ENDIF
019900020412     C     IT0OP1        IFEQ      'O02'
020000020412     C                   EXSR      MODIFI
020100020412     C                   ENDIF
020200020412     C     IT0OP1        IFEQ      'O03'
020300020412     C                   EXSR      COPIA
020400020412     C                   ENDIF
020500020412     C     IT0OP1        IFEQ      'O04'
020600020412     C                   EXSR      ANNULL
020700020412     C                   ENDIF
020800020412     C     IT0OP1        IFEQ      'O07'
020900020412     C                   EXSR      RIPRIS
021000020412     C                   ENDIF
021100020426     C     IT0OP1        IFEQ      'O13'
021200020426     C     IT0OP1        OREQ      'P13'
021300020416     C                   EXSR      INSERI
021400020416     C                   ENDIF
021500020412     C*
021600020412     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800970526     C* F12GES - GESTIONE F12 -> RITORNO
021900970526     C*------------------------------------------------------------------------*
022000970526     C     F12GES        BEGSR
022100970526     C*
022200970526     C* IMPOSTA "PREMUTO F12"
022300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022400970526     C*
022500970526     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022800970526     C*------------------------------------------------------------------------*
022900970526     C     F24GES        BEGSR
023000970526     C*
023100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* INSERI - OPERAZIONI DI INSERIMENTO RECORD
023600970526     C*------------------------------------------------------------------------*
023700970526     C     INSERI        BEGSR
023800970526     C*
023900970526     C* CONTROLLA I CAMPI IMMESSI
024000970526     C                   EXSR      DECODI
024100020415     C*
024200020415     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
024300020415     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024400020415     C                   EXSR      LETCAS
024500970526     C*
024600020412     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
024700020412     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024800020415     C                   CLEAR                   AZDSS000
024900020415     C                   EXSR      IMPCAM
025000020415     C                   MOVEL     KNMUS         DSSUTE
025100020415     C                   Z-ADD     DATCOR        DSSDIV                         *DATA INS/VAR
025200020415     C                   Z-ADD     *ZEROS        DSSDTR                         *DATA TRASMISSIONE
025300020415     C                   MOVEL     *BLANKS       DSSFTR                         *FLAG TRASMISSIONE
025400020415     C                   MOVEL     *BLANKS       DSSATB                         *FLAG ANNULLAMENTO
025500020415     C                   WRITE     AZDSS000
025600020412     C                   MOVEL     MSG(2)        OT0MSG                         *MESSAGGIO
025700020426     C*
025800020426     C* SE PROVENIENTE DA PROPOSTE IN AUTOMATICO => CANCELLO LA PROPOSTA ED ESCO
025900020426     C                   IF        IT0OP1 = 'P13'
026000040621     C     KEYWDST1C     CHAIN     AZDSTP1L
026100040621     C                   IF        %found(AZDSTP1L)
026200040621     C                   DELETE    AZDSTP1L
026300020426     C                   ENDIF
026400020426     C                   MOVEL     '1'           F03L00
026500020426     C                   ELSE
026600970602     C*
026700020426     C* DOPO L'INSERIMENTO LASCIA IMPOSTATA A VIDEO SOLO LA CHIAVE
026800020415     C                   MOVEL     *blanks       VIDSIN
026900020415     C                   ENDIF
027000020426     C                   ENDIF
027100020415     C                   ENDIF
027200020412     C*
027300020412     C                   ENDSR
027400020412     C*------------------------------------------------------------------------*
027500020412     C* MODIFI - OPERAZIONI DI MODIFICA
027600970526     C*------------------------------------------------------------------------*
027700970526     C     MODIFI        BEGSR
027800970526     C*
027900020412     C* CONTROLLA I CAMPI IMMESSI
028000020412     C                   EXSR      DECODI
028100020412     C*
028200020412     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
028300020412     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028400020412     C                   EXSR      LETCAS
028500970526     C*
028600020412     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
028700020412     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028800020412     C                   EXSR      IMPCAM
028900020415     C                   MOVEL     *BLANKS       DSSFTR                         *FLAG TRASMISSIONE
029000020415     C                   MOVEL     KNMUS         DSSUTE                         *UTENTE INS/VAR
029100020415     C                   Z-ADD     DATCOR        DSSDIV                         *DATA INS/VAR
029200020415     C                   UPDATE    AZDSS000
029300020412     C                   MOVEL     MSG(3)        OT0MSG
029400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
029500020412     C                   ENDIF
029600020412     C                   ENDIF
029700020412     C*
029800020412     C                   ENDSR
029900020412     C*------------------------------------------------------------------------*
030000020412     C* COPIA - OPERAZIONI DI COPIA
030100020412     C*------------------------------------------------------------------------*
030200020412     C     COPIA         BEGSR
030300020412     C*
030400020412     C* CONTROLLA I CAMPI IMMESSI
030500020412     C                   EXSR      DECODI
030600020412     C*
030700020412     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
030800020412     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030900020412     C                   EXSR      LETCAS
031000020412     C*
031100020412     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
031200020412     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
031300020412     C                   EXSR      IMPCAM
031400020415     C                   MOVEL     *BLANKS       DSSFTR                         *FLAG TRASMISSIONE
031500020415     C                   MOVEL     KNMUS         DSSUTE                         *UTENTE INS/VAR
031600020415     C                   Z-ADD     DATCOR        DSSDIV                         *DATA INS/VAR
031700020415     C                   WRITE     AZDSS000
031800020412     C                   MOVEL     MSG(15)       OT0MSG
031900020412     C                   MOVEL     '1'           FINL00                         *FINE PGM
032000020412     C                   ENDIF
032100020412     C                   ENDIF
032200020412     C*
032300020412     C                   ENDSR
032400020412     C*------------------------------------------------------------------------*
032500020412     C* ANNULL - OPERAZIONI DI ANNULLAMENTO
032600970526     C*------------------------------------------------------------------------*
032700970526     C     ANNULL        BEGSR
032800000411     C*
032900000411     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
033000000411     C                   EXSR      LETCAS
033100000411     C*
033200000411     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
033300000411     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033400040601     C* ESSENDO ORMAI SU "AS UNICO" ED ESSENDO IL FILE IN UNITRAGRU
033500020424     C                   DELETE    AZDSS000
033600020424     C                   MOVEL     MSG(4)        OT0MSG
033700000411     C                   MOVEL     '1'           FINL00                         *FINE PGM
033800000411     C                   ENDIF
033900000411     C*
034000970526     C                   ENDSR
034100000411     C*------------------------------------------------------------------------*
034200000411     C* RIPRIS - OPERAZIONI DI RIPRISTINO
034300000411     C*------------------------------------------------------------------------*
034400000411     C     RIPRIS        BEGSR
034500000411     C*
034600000411     C* LEGGE IL RECORD PERL'AGGIORNAMENTO
034700000411     C                   EXSR      LETCAS
034800000411     C*
034900000411     C* IMPOSTA I CAMPI DEL RECORD DA SCRIVERE
035000000411     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035100020415     C                   MOVEL     *BLANKS       DSSATB
035200020415     C                   MOVEL     *BLANKS       DSSFTR                         *FLAG TRASMISSIONE
035300020415     C                   MOVEL     KNMUS         DSSUTE                         *UTENTE INS/VAR
035400020415     C                   Z-ADD     DATCOR        DSSDIV                         *DATA INS/VAR
035500020415     C                   UPDATE    AZDSS000
035600020412     C                   MOVEL     MSG(5)        OT0MSG
035700000411     C                   MOVEL     '1'           FINL00                         *FINE PGM
035800000411     C                   ENDIF
035900000411     C*
036000000411     C                   ENDSR
036100970526     C*------------------------------------------------------------------------*
036200020415     C* LETCAS - LEGGE IL RECORD PER L'AGGIORNAMENTO
036300970526     C*------------------------------------------------------------------------*
036400970619     C     LETCAS        BEGSR
036500020415     C*
036600020426     C     KEYDSS1C      CHAIN     AZDSS01L                           99
036700020426     C     IT0OP1        IFEQ      'F06'
036800020426     C     IT0OP1        OREQ      'O13'
036900020426     C     IT0OP1        OREQ      'P13'
037000020621     C     *IN99         IFEQ      *OFF                                         *INESISTENTE
037100020415     C                   MOVEL     '1'           ERRL00                         *ERRORE
037200020415     C                   MOVEL     MSG(7)        OT0MSG                         *MESSAGGIO
037300020415     C                   MOVEL     '1'           FINL00                         *FINE PGM
037400020415     C                   ENDIF
037500020426     C                   ENDIF
037600970526     C*
037700970526     C                   ENDSR
037800970526     C*------------------------------------------------------------------------*
037900020415     C* DECODI - CONTROLLA CAMPI E DECODIFICA
038000970526     C*------------------------------------------------------------------------*
038100970526     C     DECODI        BEGSR
038200000407     C*
038300000407     C                   MOVEL     '0'           ERRL00
038400000407     C*
038500000407     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE
038600020412     C                   SETOFF                                       818283
038700020412     C                   SETOFF                                       848586
038800020412     C                   SETOFF                                       8788
038900020412     C*--------------------
039000020412     C* CONTROLLO CORRETTEZZA DATI DI INSTRADAMENTO
039100020412     C*--------------------
039200020412     C     ERRL00        IFEQ      '0'
039300020412     C                   EXSR      CHKINSTR
039400020412     C                   ENDIF
039500020415     C*--------------------
039600020415     C* CONTROLLO CHE NON SIA STATO INSERITO IL SINONIMO UGUALE ALLA RAGIONE SOCIALE
039700020415     C*--------------------
039800020415     C     ERRL00        IFEQ      '0'
039900020415     C                   IF         %trim(VIDSIN) = %trim(VIDRAG)
040000020415     C                   MOVEL     '1'           ERRL00
040100020415     C                   MOVEL     MSG(10)       DSMSMS
040200020415     C                   SETON                                        8187
040300020415     C                   ENDIF
040400020415     C                   ENDIF
040500020412     C*--------------------
040600040319     C* CONTROLLO CHE NON SIA STATO INSERITO IL CARATTERE JOLLY SUL SINONIMO
040700040319     C* COME CARATTERE SINGOLO O IN PRIMA POSIZIOINE
040800020412     C*--------------------
040900020412     C     ERRL00        IFEQ      '0'
041000040609     C                   IF        VIDSIN <> *blanks
041100040319     C                   IF        %subst(%trim(VIDSIN):1:1) = '*'
041200020412     C                   MOVEL     '1'           ERRL00
041300040319     C                   MOVEL     MSG(21)       DSMSMS
041400020415     C                   SETON                                        87
041500020412     C                   ENDIF
041600040609     C                   ENDIF
041700020412     C                   ENDIF
041800020415     C*--------------------
041900020415     C* CONTROLLO CHE NON SIA STATO INSERITO IL CARATTERE JOLLY SULLA RAGIONE SOCIALE
042000020415     C*--------------------
042100020415     C     ERRL00        IFEQ      '0'
042200020415     C     '*'           SCAN      VIDRAG
042300020415     C                   IF        %found
042400020415     C                   MOVEL     '1'           ERRL00
042500020415     C                   MOVEL     MSG(9)        DSMSMS
042600020415     C                   SETON                                        81
042700020415     C                   ENDIF
042800020415     C                   ENDIF
042900000407     C*--------------------
043000000407     C* CONTROLLO OBBLIGATORIETA' TUTTI CAMPI
043100000407     C*--------------------
043200000407     C     ERRL00        IFEQ      '0'
043300020412     C     VIDRAG        IFEQ      *blanks
043400000407     C                   MOVEL     '1'           ERRL00
043500020412     C                   SETON                                        81
043600000407     C                   ENDIF
043700020415     C     VIDSIN        IFEQ      *blanks
043800020415     C                   MOVEL     '1'           ERRL00
043900020415     C                   SETON                                        87
044000020415     C                   ENDIF
044100020412     C     VIDLOC        IFEQ      *blanks
044200000407     C                   MOVEL     '1'           ERRL00
044300020412     C                   SETON                                        83
044400000407     C                   ENDIF
044500020412     C     VIDCAP        IFEQ      *blanks
044600000407     C                   MOVEL     '1'           ERRL00
044700020412     C                   SETON                                        84
044800000407     C                   ENDIF
044900020412     C     VIDPRV        IFEQ      *blanks
045000000407     C                   MOVEL     '1'           ERRL00
045100020412     C                   SETON                                        85
045200000407     C                   ENDIF
045300020412     C     VIDKWI        IFEQ      *blanks
045400000407     C                   MOVEL     '1'           ERRL00
045500020412     C                   SETON                                        86
045600000407     C                   ENDIF
045700000407     C     ERRL00        IFEQ      '1'
045800000407     C                   MOVEL     MSG(12)       DSMSMS
045900000407     C                   ENDIF
046000000407     C                   ENDIF
046100020424     C*--------------------
046200040421     C* PRIMA DI PROCEDERE CONTROLLO TRAMITE TISIT5R => POSSO INSERIRE SOLO SE RESTITUITO "ERRORE"
046300020424     C*--------------------
046400020424     C     ERRL00        IFEQ      '0'
046500020424     C     IT0OP1        IFEQ      'F06'
046600020426     C     IT0OP1        OREQ      'O13'
046700020426     C     IT0OP1        OREQ      'P13'
046800040322     C     IT0OP1        OREQ      'O03'
046900040322     C     IT0OP1        OREQ      'O02'
047000040421     C     IT0OP1        OREQ      'O07'
047100020424     C*
047200020424     C* IMPOSTO I CAMPI DELLA DS DI PASSAGGIO PROCEDURA DI CONTROLLO/REPERIMENTO
047300020424     C                   EXSR      WIMPCAM
047400020424     C*
047500020424     C* CHIAMO IL PROGRAMMA DI REPERIMENTO
047600020424     C                   CALL      'TISIT5R'
047700020424     C                   PARM                    WRKT0DS
047800040116     C*
047900040116     C* SE TRATTASI DI INSERIMENTO SINONIMO DA PROPOSTE E SE VALIDATO IL SINONIMO DAL DRIVER
048000040116     C* REIMPOSTO IL CAP DEL SINONIMO A VIDEO CON QUELLO DELL'ANAGRAFICA DESTINATARIO RELATIVA
048100040116     C     IT0OP1        IFEQ      'P13'
048200040116     C                   IF        W_OT0ERR <> '1' OR
048300040116     C                             W_OT0LIV =  '9'
048400040116     C                   EVAL      VIDCAP = W_OT0CAP
048500040625     C***                MOVEL     MSG(16)       DSMSMS
048600040116     C                   ENDIF
048700040116     C                   ENDIF
048800020424     C*
048900020708     C     w_OT0LIV      IFEQ      '1'
049000020708     C     w_OT0LIV      OREQ      '3'
049100020708     C     w_OT0LIV      OREQ      '3'
049200020708     C                   MOVEL     '1'           ERRL00
049300020708     C                   MOVEL     MSG(19)       DSMSMS
049400020708     C                   ELSE
049500020708     C     w_OT0LIV      IFEQ      '2'
049600020708     C     w_OT0LIV      OREQ      '4'
049700020708     C                   MOVEL     '1'           ERRL00
049800020708     C                   MOVEL     MSG(20)       DSMSMS
049900020708     C                   ELSE
050000020424     C     w_OT0LIV      IFNE      '9'
050100020424     C                   MOVEL     '1'           ERRL00
050200020708     C                   MOVEL     MSG(18)       DSMSMS
050300020424     C                   ENDIF
050400020708     C                   ENDIF
050500020708     C                   ENDIF
050600020424     C*
050700020424     C                   ENDIF
050800020424     C                   ENDIF
050900040625     C*--------------------
051000040625     C* A QUESTO PUNTO => RE-IMPOSTO LA CHIAVE COI CAMPI DEL VIDEO IN BASE AL TIPO DI LANCIO
051100040625     C*--------------------
051200040625     C                   IF        IT0OP1  = 'O13'   OR
051300040625     C                             IT0OP1  = 'P13'
051400040625     C                   EVAL      KDSRAG = VIDRAG
051500040625     C                   EVAL      KDSSIN = VIDSIN
051600040625     C                   EVAL      KDSNAZ = VIDNAZ
051700040625     C                   EVAL      KDSPRV = VIDPRV
051800040625     C                   EVAL      KDSCAP = VIDCAP
051900040625     C                   EVAL      KDSLOC = VIDLOC
052000040625     C                   EVAL      KDSKWI = VIDKWI
052100040625     C                   ELSE
052200040625     C                   EVAL      KDSRAG = IT0RAG
052300040625     C                   EVAL      KDSSIN = IT0SIN
052400040625     C                   EVAL      KDSNAZ = IT0NAZ
052500040625     C                   EVAL      KDSPRV = IT0PRV
052600040625     C                   EVAL      KDSCAP = IT0CAP
052700040625     C                   EVAL      KDSLOC = IT0LOC
052800040625     C                   EVAL      KDSKWI = IT0KWI
052900040625     C                   ENDIF
053000040426     C*--------------------
053100040426     C* CONTROLLO CHE X IL SINONIMO ESISTA VERAMENTE IL RECORD "PRINCIPALE" RELATIVO
053200040426     C*--------------------
053300040426     C     ERRL00        IFEQ      '0'
053400040426     C*    IT0OP1        IFNE      'F06'
053500040426     C*    IT0OP1        ANDNE     'O13'
053600040426     C*    IT0OP1        ANDNE     'P13'
053700040426     C     KEYDST1C      CHAIN     AZDST01L
053800040426     C                   IF        not %found(AZDST01L)
053900040426     C                   MOVEL     '1'           ERRL00
054000040426     C                   MOVEL     MSG(13)       DSMSMS
054100040426     C                   SETON                                        818283
054200040426     C                   SETON                                        848586
054300040426     C                   SETON                                        8788
054400040426     C                   ENDIF
054500040426     C*                  ENDIF
054600040426     C                   ENDIF
054700000407     C*
054800970528     C                   ENDSR
054900020424     C*------------------------------------------------------------------------*
055000020424     C* WIMPCAM - IMPOSTA I CAMPI X LA CHIAMATA AL TISIT5R
055100020424     C*------------------------------------------------------------------------*
055200020424     C     WIMPCAM       BEGSR
055300020424     C*
055400040625     C                   EVAL      w_IT0TLA = 'P'
055500040625     C*
055600020424     C                   MOVEL     VIDSIN        w_IT0RAG
055700020424     C                   MOVEL     VIDNAZ        w_IT0NAZ
055800020424     C                   MOVEL     VIDPRV        w_IT0PRV
055900020424     C                   MOVEL     VIDCAP        w_IT0CAP
056000020424     C                   MOVEL     VIDLOC        w_IT0LOC
056100020424     C                   MOVEL     VIDKWI        w_IT0IND
056200020424     C*
056300020424     C                   ENDSR
056400020412     C*------------------------------------------------------------------------*
056500020412     C* CHKINSTR - CONTROLLA TUTTI I DATI DI "INSTRADAMENTO"
056600020412     C*------------------------------------------------------------------------*
056700020412     C     CHKINSTR      BEGSR
056800020412     C*
056900020412     C                   CLEAR                   TISI95DS                       *azzera i parametri
057000020412     C                   MOVEL     *blanks       I95TLA                         *tipo lancio
057100020412     C                   MOVEL     '7'           I95TCN                         *tipo controllo
057200020412     C                   MOVEL     VIDNAZ        I95NAR                         *nazione
057300020412     C                   MOVEL     VIDCAP        I95CAP                         *cap
057400020412     C                   MOVEL     VIDLOC        I95LOC                         *località
057500020412     C                   MOVEL     VIDPRV        I95PRV                         *provincia
057600020412     C                   Z-ADD     DATCOR        I95DAT                         *data di riferimento
057700020412     C*
057800020412     C                   CALL      'TISI95R'
057900020412     C                   PARM                    TISI95DS
058000020412     C*
058100020412     C* SE LIVELLO DATI 3 O 4 "NORMALIZZO" I DATI DI "INSTRADAMENTO" INSERITO A VIDEO
058200020412     C                   IF        O95LID = '3' OR
058300020412     C                             O95LID = '4'
058400020412     C*
058500020412     C                   IF        VIDNAZ <> O95NAR OR
058600020412     C                             VIDCAP <> O95CAP OR
058700020412     C                             VIDPRV <> O95PRV OR
058800020412     C                             VIDLOC <> O95LOC
058900020412     C                   EVAL      VIDNAZ = O95NAR
059000020412     C                   EVAL      VIDCAP = O95CAP
059100020412     C                   EVAL      VIDPRV = O95PRV
059200020412     C                   EVAL      VIDLOC = O95LOC
059300020412     C                   MOVEL     MSG(16)       DSMSMS
059400020412     C                   ENDIF
059500020412     C*
059600020412     C                   ELSE
059700020412     C                   MOVEL     '1'           ERRL00
059800020412     C                   SETON                                        838485
059900020412     C                   SETON                                        88
060000020412     C                   MOVEL     MSG(17)       DSMSMS
060100020412     C                   ENDIF
060200020412     C*
060300020412     C                   ENDSR
060400970526     C*------------------------------------------------------------------------*
060500970526     C* IREVID - IMPOSTA I CAMPI DEL VIDEO DAL FILE DI DATA BASE
060600970526     C*------------------------------------------------------------------------*
060700970526     C     IREVID        BEGSR
060800000407     C*
060900020415     C                   MOVEL     DSSRAG        VIDRAG
061000020415     C                   MOVEL     DSSSIN        VIDSIN
061100020415     C                   MOVEL     DSSNAZ        VIDNAZ
061200020415     C                   MOVEL     DSSPRV        VIDPRV
061300020415     C                   MOVEL     DSSCAP        VIDCAP
061400020415     C                   MOVEL     DSSLOC        VIDLOC
061500020415     C                   MOVEL     DSSKWI        VIDKWI
061600040614     C                   MOVEL     DSSUTE        VIDUTE
061700040614     C                   Z-ADD     DSSDIV        VIDDIV
061800040614     C                   MOVEL     DSSFTR        VIDFTR
061900040614     C                   Z-ADD     DSSDTR        VIDDTR
062000970527     C*
062100970526     C                   ENDSR
062200970526     C*------------------------------------------------------------------------*
062300970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
062400970526     C*------------------------------------------------------------------------*
062500970526     C     AZZVID        BEGSR
062600020412     C*
062700020412     C* INIZIALIZZO CAMPI
062800020412     C                   MOVEL     *blanks       VIDRAG
062900020415     C                   MOVEL     *blanks       VIDSIN
063000020412     C                   MOVEL     *blanks       VIDNAZ
063100020412     C                   MOVEL     *blanks       VIDPRV
063200020412     C                   MOVEL     *blanks       VIDCAP
063300020412     C                   MOVEL     *blanks       VIDLOC
063400020412     C                   MOVEL     *blanks       VIDKWI
063500970526     C*
063600020412     C                   ENDSR
063700000407     C*------------------------------------------------------------------------*
063800020415     C* IMPCAM - IMPOSTA I CAMPI DEL FILE DAL VIDEO
063900000407     C*------------------------------------------------------------------------*
064000000407     C     IMPCAM        BEGSR
064100000407     C*
064200020415     C                   MOVEL     VIDRAG        DSSRAG
064300020415     C                   MOVEL     VIDSIN        DSSSIN
064400020415     C                   MOVEL     VIDNAZ        DSSNAZ
064500020415     C                   MOVEL     VIDPRV        DSSPRV
064600020415     C                   MOVEL     VIDCAP        DSSCAP
064700020415     C                   MOVEL     VIDLOC        DSSLOC
064800020415     C                   MOVEL     VIDKWI        DSSKWI
064900000407     C*
065000000407     C                   ENDSR
065100970526     C*------------------------------------------------------------------------*
065200970526     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
065300970526     C*------------------------------------------------------------------------*
065400970526     C     IMPFUN        BEGSR
065500970526     C*
065600970526     C* SE VISUALIZZAZIONE TOGLIE LA FUNZIONE DI AGGIORNAMENTO
065700020426     C     IT0OP1        IFEQ      'O05'
065800970526     C                   MOVEL     FUD(6)        FUDRIG                         *AGGIORNAMENTO
065900970526     C                   MOVEL     'N'           FUDATT
066000970526     C                   MOVEL     FUDRIG        FUD(6)
066100020426     C                   ENDIF
066200970526     C*
066300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
066400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
066500970526     C                   EXSR      FUNGES
066600970526     C*
066700970526     C*
066800000407     C* LETTURA DEL RECORD DA AGGIORNARE -NO IN INSERIMENTO-
066900020412     C     IT0OP1        IFNE      'F06'                                        *NO INSERIMENTO
067000020426     C     IT0OP1        ANDNE     'O13'                                        *NO INSERIMENTO
067100020426     C     IT0OP1        ANDNE     'P13'                                        *NO INSERIMENTO
067200000407     C                   EXSR      LETCAS
067300980528     C                   EXSR      IREVID                                       *IMPOSTA CAMPI
067400020412     C***                EXSR      DECODI                                       *DECODIFICA CAMPI
067500020426     C***                EXSR      AZZVID                                       *AZZERA CAMPI
067600020415     C                   ENDIF
067700020426     C                   MOVEL     '0'           ERRL00                         *TOGLIE L'ERRORE
067800970526     C*
067900970526     C* IMPOSTA LA DESCRIZIONE DELLA FUNZIONE
068000020426     C     IT0OP1        IFEQ      'O02'
068100970526     C                   MOVEL     DEC(1)        VIDFUN
068200020426     C                   ENDIF
068300020426     C     IT0OP1        IFEQ      'O03'
068400020415     C                   MOVEL     DEC(6)        VIDFUN
068500020426     C                   ENDIF
068600020426     C     IT0OP1        IFEQ      'O04'
068700970526     C                   MOVEL     DEC(2)        VIDFUN
068800020426     C                   ENDIF
068900020426     C     IT0OP1        IFEQ      'O05'
069000970526     C                   MOVEL     DEC(3)        VIDFUN
069100020426     C                   ENDIF
069200020426     C     IT0OP1        IFEQ      'O07'
069300970526     C                   MOVEL     DEC(4)        VIDFUN
069400020426     C                   ENDIF
069500020426     C     IT0OP1        IFEQ      'F06'
069600020426     C                   MOVEL     DEC(5)        VIDFUN
069700020426     C                   ENDIF
069800020426     C     IT0OP1        IFEQ      'O13'
069900020426     C                   MOVEL     DEC(5)        VIDFUN
070000020426     C                   ENDIF
070100020426     C     IT0OP1        IFEQ      'P13'
070200020426     C                   MOVEL     DEC(7)        VIDFUN
070300020426     C                   ENDIF
070400970526     C*
070500970526     C                   ENDSR
070600020412     C*------------------------------------------------------------------------*
070700020412     C* FINSR - OPERAZIONI FINALI
070800020412     C*------------------------------------------------------------------------*
070900020412     C     FINSR         BEGSR
071000020412     C*
071100020412     C                   CLEAR                   TISI95DS                       *azzera i parametri
071200020412     C                   MOVEL     'C'           I95TLA                         *tipo lancio
071300020412     C*
071400020412     C                   CALL      'TISI95R'
071500020412     C                   PARM                    TISI95DS
071600020412     C*
071700020412     C                   ENDSR
071800970526     C*------------------------------------------------------------------------*
071900970526     C* *INZSR - OPERAZIONI INIZIALI
072000970526     C*------------------------------------------------------------------------*
072100970526     C     *INZSR        BEGSR
072200970526     C*---
072300970526     C* RICEVIMENTO PARAMETRI
072400970526     C*---
072500970526     C     *ENTRY        PLIST
072600970612     C                   PARM                    KPJBA
072700020412     C                   PARM                    TISIT0DS
072800040625     C*
072900040625     C                   MOVEL     KPJBU         PARAM
073000020415     C*
073100020415     C* TESTO SUBITO IL TIPO DI LANCIO
073200040421     C                   IF        IT0OP1  = 'O13'   OR
073300020426     C                             IT0OP1  = 'P13'
073400020426     C                   EVAL      VIDRAG = IT0RAG
073500020426     C                   EVAL      VIDSIN = IT0SIN
073600020426     C                   EVAL      VIDNAZ = IT0NAZ
073700020426     C                   EVAL      VIDPRV = IT0PRV
073800020426     C                   EVAL      VIDCAP = IT0CAP
073900020426     C                   EVAL      VIDLOC = IT0LOC
074000020426     C                   EVAL      VIDKWI = IT0KWI
074100020426     C                   ELSE
074200020426     C                   EVAL      KDSRAG = IT0RAG
074300020426     C                   EVAL      KDSSIN = IT0SIN
074400020426     C                   EVAL      KDSNAZ = IT0NAZ
074500020426     C                   EVAL      KDSPRV = IT0PRV
074600020426     C                   EVAL      KDSCAP = IT0CAP
074700020426     C                   EVAL      KDSLOC = IT0LOC
074800020426     C                   EVAL      KDSKWI = IT0KWI
074900020415     C                   ENDIF
075000030129     C*
075100030129     C* INIZIALIZZO TUTTI GLI INDICATORI DI ERRORE E DI OPZIONE
075200030129     C                   SETOFF                                       818283
075300030129     C                   SETOFF                                       848586
075400030129     C                   SETOFF                                       8788
075500030129     C                   SETOFF                                       9091
075600000407     C*
075700000407     C* EFFETTUO PRIME VERIFICHE SUL TIPO DI OPERAZIONI DA EFFETTUARE
075800020412     C     IT0OP1        IFEQ      'O04'
075900000407     C                   SETON                                        90
076000000407     C                   ENDIF
076100020412     C     IT0OP1        IFEQ      'O05'
076200020412     C                   SETON                                        90
076300020412     C                   ENDIF
076400020412     C     IT0OP1        IFEQ      'O07'
076500020412     C                   SETON                                        90
076600020412     C                   ENDIF
076700020412     C     IT0OP1        IFEQ      'O02'
076800000407     C                   SETON                                        91
076900000407     C                   ENDIF
077000020415     C     IT0OP1        IFEQ      'O03'
077100020415     C                   SETON                                        91
077200020415     C                   ENDIF
077300970526     C*
077400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
077500970526     C     XMSGPA        PLIST
077600970526     C                   PARM                    DSMSMS
077700970526     C                   PARM                    DSMSPG
077800970526     C                   PARM                    DSMSKE
077900970526     C                   PARM                    DSMSER
078000970526     C*
078100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
078200970526     C                   CALL      'XKEY'
078300970526     C                   PARM                    DSKEY
078400020415     C*---
078500020415     C* VARIABILI RIFERITE AL DATA BASE
078600020415     C*---
078700020415     C     *LIKE         DEFINE    DSSNAZ        KDSNAZ
078800020415     C     *LIKE         DEFINE    DSSPRV        KDSPRV
078900020415     C     *LIKE         DEFINE    DSSCAP        KDSCAP
079000020415     C     *LIKE         DEFINE    DSSLOC        KDSLOC
079100020415     C     *LIKE         DEFINE    DSSKWI        KDSKWI
079200020415     C     *LIKE         DEFINE    DSSRAG        KDSRAG
079300020415     C     *LIKE         DEFINE    DSSSIN        KDSSIN
079400020426     C     *LIKE         DEFINE    DSTTIP        KKCTIP
079500020426     C     *LIKE         DEFINE    DSTTIP        KKPTIP
079600970526     C*---
079700970526     C* VARIABILI NON RIFERITE AL DATA BASE
079800970526     C*---
079900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
080000970526     C*---
080100970526     C* VARIABILI DI CONTROLLO
080200970526     C*---
080300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
080400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
080500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
080600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
080700020426     C*---
080800020426     C* INIZIALIZZAZIONE VARIABILI
080900020426     C*---
081000020426     C                   EVAL      KKCTIP = 'C'
081100020426     C                   EVAL      KKPTIP = 'P'
081200000407     C*---
081300000407     C* CHIAVI DI LETTURA
081400000407     C*---
081500020415     C* LETTURA AZDSS01L - COMPLETA
081600020415     C     KEYDSS1C      KLIST
081700020415     C                   KFLD                    KDSCAP                         *CAP
081800020415     C                   KFLD                    KDSRAG                         *RAGIONE SOCIALE
081900020415     C                   KFLD                    KDSSIN                         *SINONIMO
082000020415     C                   KFLD                    KDSNAZ                         *NAZIONE
082100020415     C                   KFLD                    KDSPRV                         *PROVINCIA
082200020415     C                   KFLD                    KDSLOC                         *LOCALITA
082300020415     C                   KFLD                    KDSKWI                         *KEYWORD INDIRIZZO
082400020415     C* LETTURA AZDST01L - COMPLETA
082500020415     C     KEYDST1C      KLIST
082600020426     C                   KFLD                    KKCTIP                         *TIPO
082700020621     C                   KFLD                    KDSCAP                         *CAP
082800020621     C                   KFLD                    KDSRAG                         *RAGIONE SOCIALE
082900020621     C                   KFLD                    KDSNAZ                         *NAZIONE
083000020621     C                   KFLD                    KDSPRV                         *PROVINCIA
083100020621     C                   KFLD                    KDSLOC                         *LOCALITA
083200020621     C                   KFLD                    KDSKWI                         *KEYWORD INDIRIZZO
083300040621     C* LETTURA AZDSTP1L - COMPLETA (CON CHIAVE DA DS INPUT)
083400020426     C     KEYWDST1C     KLIST
083500020426     C                   KFLD                    KKPTIP
083600020426     C                   KFLD                    IT0NAZ
083700020426     C                   KFLD                    IT0PRV
083800020426     C                   KFLD                    IT0LOC
083900020426     C                   KFLD                    IT0CAP
084000020426     C                   KFLD                    IT0SIN
084100020426     C                   KFLD                    IT0IND
084200040625     C                   KFLD                    DEPLNA
084300970526     C*
084400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
084500020415     C                   MOVEL     'TISIT4R'     DSMSPG
084600020415     C                   MOVEL     'TISIT4R'     MSGQUE
084700970526     C                   MOVEL     *BLANKS       MSGKEY
084800970526     C                   MOVEL     *BLANKS       DSMSMS
084900000407     C*
085000000407     C* CALCOLA LA DATA CORRENTE
085100000407     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
085200000407     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
085300000407     C                   Z-ADD     WN8           G08DAT
085400000407     C                   Z-ADD     *ZEROS        G08INV
085500000407     C                   MOVEL     '0'           G08ERR
085600000407     C                   CALL      'XSRDA8'
085700000407     C                   PARM                    WLBDA8
085800000407     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
085900970526     C*
086000970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
086100970526     C                   EXSR      IMPFUN
086200970526     C*
086300970526     C                   ENDSR
086400970526     C*------------------------------------------------------------------------*
086500970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
086600970526     C*------------------------------------------------------------------------*
086700970526     C     FUNGES        BEGSR
086800970526     C*
086900970526     C* PULISCO CAMPI DI LAVORO
087000970526     C                   Z-ADD     0             W                 5 0
087100970526     C*
087200970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
087300970526     C* E CARICO TABELLA FUNZIONI ATTIVE
087400970526     C     1             DO        24            I
087500970526     C                   MOVEL     FUD(I)        FUDRIG
087600970526     C*
087700970526     C* PULIZIA FUNZIONI PRECEDENTI
087800970526     C                   MOVEL     *BLANKS       FUV(I)
087900970526     C*
088000970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
088100970526     C* (SALTA F24)
088200970526     C     FUDATT        IFEQ      'S'
088300970526     C     FUDFUN        ANDNE     '24'
088400970526     C                   ADD       1             W
088500970526     C                   MOVEL     FUDDES        FUV(W)
088600970526     C                   END
088700970526     C                   END                                                    DO WHILE
088800970526     C*
088900970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
089000970526     C* ALTRIMENTI DISATTIVO F24
089100970526     C     1             DO        24            I
089200970526     C                   MOVEL     FUD(I)        FUDRIG
089300970526     C*
089400970526     C     FUDFUN        IFEQ      '24'
089500970526     C     W             IFGT      6
089600970526     C                   MOVEL     'S'           FUDATT
089700970526     C                   ELSE
089800970526     C                   MOVEL     *BLANKS       FUDATT
089900970526     C                   END                                                    W GT 8
090000970526     C                   MOVEL     FUDRIG        FUD(I)
090100970526     C                   END
090200970526     C                   END                                                    DO WHILE
090300970526     C*
090400970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
090500970526     C     W             DIV       6             I
090600970526     C                   MVR                     Z
090700970526     C*
090800970526     C     Z             IFNE      0
090900970526     C     I             ADD       1             Z
091000970526     C                   ELSE
091100970526     C                   Z-ADD     I             Z                 5 0
091200970526     C                   END
091300970526     C*
091400970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
091500970526     C                   ADD       1             FUNGRU
091600970526     C     FUNGRU        IFGT      Z
091700970526     C     FUNGRU        ORLT      1
091800970526     C                   Z-ADD     1             FUNGRU
091900970526     C                   END
092000970526     C*
092100970526     C* PRIMO GRUPPO DI 6 FUNZIONI
092200970526     C     FUNGRU        IFEQ      1
092300970526     C                   MOVEL     FUV(1)        FUN001
092400970526     C                   MOVEL     FUV(2)        FUN002
092500970526     C                   MOVEL     FUV(3)        FUN003
092600970526     C                   MOVEL     FUV(4)        FUN004
092700970526     C                   MOVEL     FUV(5)        FUN005
092800970526     C                   MOVEL     FUV(6)        FUN006
092900970526     C                   END
093000970526     C*
093100970526     C* SECONDO GRUPPO DI 6 FUNZIONI
093200970526     C     FUNGRU        IFEQ      2
093300970526     C                   MOVEL     FUV(7)        FUN001
093400970526     C                   MOVEL     FUV(8)        FUN002
093500970526     C                   MOVEL     FUV(9)        FUN003
093600970526     C                   MOVEL     FUV(10)       FUN004
093700970526     C                   MOVEL     FUV(11)       FUN005
093800970526     C                   MOVEL     FUV(12)       FUN006
093900970526     C                   END
094000970526     C*
094100970526     C* TERZO GRUPPO DI 6 FUNZIONI
094200970526     C     FUNGRU        IFEQ      3
094300970526     C                   MOVEL     FUV(13)       FUN001
094400970526     C                   MOVEL     FUV(14)       FUN002
094500970526     C                   MOVEL     FUV(15)       FUN003
094600970526     C                   MOVEL     FUV(16)       FUN004
094700970526     C                   MOVEL     FUV(17)       FUN005
094800970526     C                   MOVEL     FUV(18)       FUN006
094900970526     C                   END
095000970526     C*
095100970526     C* QUARTO GRUPPO DI 6 FUNZIONI
095200970526     C     FUNGRU        IFEQ      4
095300970526     C                   MOVEL     FUV(19)       FUN001
095400970526     C                   MOVEL     FUV(20)       FUN002
095500970526     C                   MOVEL     FUV(21)       FUN003
095600970526     C                   MOVEL     FUV(22)       FUN004
095700970526     C                   MOVEL     *BLANKS       FUN005
095800970526     C                   MOVEL     *BLANKS       FUN006
095900970526     C                   END
096000970526     C*
096100970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
096200970526     C                   MOVEL     *BLANKS       FUN007
096300970526     C                   MOVEL     *BLANKS       FUN008
096400970526     C*
096500970526     C     1             DO        24            I
096600970526     C                   MOVEL     FUD(I)        FUDRIG
096700970526     C     FUDFUN        IFEQ      '24'
096800970526     C     FUDATT        ANDEQ     'S'
096900970526     C                   MOVEL     FUDDES        FUN008
097000970526     C                   END
097100970526     C                   END                                                    DO
097200970526     C*
097300970526     C                   ENDSR
097400970526     C*------------------------------------------------------------------------*
097500970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
097600970526     C*------------------------------------------------------------------------*
097700970526     C     FUNCON        BEGSR
097800970526     C*
097900970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
098000970526     C                   EXSR      FUNVAR
098100970526     C*
098200970526     C* PULISCO CAMPI DI LAVORO
098300970526     C                   Z-ADD     0             I                 5 0
098400970526     C                   Z-ADD     0             W                 5 0
098500970526     C*
098600970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
098700970526     C     FUNKEY        IFNE      *BLANKS
098800970526     C     1             DO        24            I
098900970526     C                   MOVEL     FUD(I)        FUDRIG
099000970526     C*
099100970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
099200970526     C     FUNKEY        IFEQ      FUDFUN
099300970526     C     FUDATT        ANDEQ     'S'
099400970526     C                   Z-ADD     I             W
099500970526     C                   END
099600970526     C                   END                                                    DO WHILE
099700970526     C*
099800970526     C* FUNZIONE NON ATTIVA (ERRORE)
099900970526     C     W             IFEQ      0
100000970526     C*
100100970526     C* EMISSIONE MESSAGGIO DI ERRORE
100200970526     C                   MOVEL     MSG(1)        DSMSMS
100300970526     C*
100400970526     C                   END                                                    W=0
100500970526     C                   END                                                    FUNKEY NE *BLANKS
100600970526     C*
100700970526     C* FUNZIONE ATTIVA (PROCEDI ...)
100800970526     C     W             IFNE      0
100900970526     C     FUNKEY        OREQ      *BLANKS
101000970526     C     FUNKEY        IFEQ      *BLANKS
101100970526     C                   EXSR      ENTGES
101200970526     C                   END
101300970526     C     FUNKEY        IFEQ      '03'
101400970526     C                   EXSR      F03GES
101500970526     C                   END
101600970526     C     FUNKEY        IFEQ      '06'
101700970526     C                   EXSR      F06GES
101800970526     C                   END
101900970526     C     FUNKEY        IFEQ      '12'
102000970526     C                   EXSR      F12GES
102100970526     C                   END
102200970526     C     FUNKEY        IFEQ      '24'
102300970526     C                   EXSR      F24GES
102400970526     C                   END
102500970526     C                   END
102600970526     C*
102700970526     C                   ENDSR
102800970526     C*------------------------------------------------------------------------*
102900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
103000970526     C*------------------------------------------------------------------------*
103100970526     C     FUNVAR        BEGSR
103200970526     C*
103300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
103400970526     C*
103500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
103600970526     C     KEY           IFEQ      ENTER
103700970526     C                   MOVEL     *BLANKS       FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F1
104000970526     C                   MOVEL     '01'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F2
104300970526     C                   MOVEL     '02'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F3
104600970526     C                   MOVEL     '03'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F4
104900970526     C                   MOVEL     '04'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      F5
105200970526     C                   MOVEL     '05'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      F6
105500970526     C                   MOVEL     '06'          FUNKEY
105600970526     C                   END
105700970526     C     KEY           IFEQ      F7
105800970526     C                   MOVEL     '07'          FUNKEY
105900970526     C                   END
106000970526     C     KEY           IFEQ      F8
106100970526     C                   MOVEL     '08'          FUNKEY
106200970526     C                   END
106300970526     C     KEY           IFEQ      F9
106400970526     C                   MOVEL     '09'          FUNKEY
106500970526     C                   END
106600970526     C     KEY           IFEQ      F10
106700970526     C                   MOVEL     '10'          FUNKEY
106800970526     C                   END
106900970526     C     KEY           IFEQ      F11
107000970526     C                   MOVEL     '11'          FUNKEY
107100970526     C                   END
107200970526     C     KEY           IFEQ      F12
107300970526     C                   MOVEL     '12'          FUNKEY
107400970526     C                   END
107500970526     C     KEY           IFEQ      F13
107600970526     C                   MOVEL     '13'          FUNKEY
107700970526     C                   END
107800970526     C     KEY           IFEQ      F14
107900970526     C                   MOVEL     '14'          FUNKEY
108000970526     C                   END
108100970526     C     KEY           IFEQ      F15
108200970526     C                   MOVEL     '15'          FUNKEY
108300970526     C                   END
108400970526     C     KEY           IFEQ      F16
108500970526     C                   MOVEL     '16'          FUNKEY
108600970526     C                   END
108700970526     C     KEY           IFEQ      F17
108800970526     C                   MOVEL     '17'          FUNKEY
108900970526     C                   END
109000970526     C     KEY           IFEQ      F18
109100970526     C                   MOVEL     '18'          FUNKEY
109200970526     C                   END
109300970526     C     KEY           IFEQ      F19
109400970526     C                   MOVEL     '19'          FUNKEY
109500970526     C                   END
109600970526     C     KEY           IFEQ      F20
109700970526     C                   MOVEL     '20'          FUNKEY
109800970526     C                   END
109900970526     C     KEY           IFEQ      F21
110000970526     C                   MOVEL     '21'          FUNKEY
110100970526     C                   END
110200970526     C     KEY           IFEQ      F22
110300970526     C                   MOVEL     '22'          FUNKEY
110400970526     C                   END
110500970526     C     KEY           IFEQ      F23
110600970526     C                   MOVEL     '23'          FUNKEY
110700970526     C                   END
110800970526     C     KEY           IFEQ      F24
110900970526     C                   MOVEL     '24'          FUNKEY
111000970526     C                   END
111100970526     C     KEY           IFEQ      ROLLUP
111200970526     C                   MOVEL     'RU'          FUNKEY
111300970526     C                   END
111400970526     C     KEY           IFEQ      HELP
111500970526     C                   MOVEL     'HP'          FUNKEY
111600970526     C                   END
111700970526     C                   ENDSR
111800970526     C*------------------------------------------------------------------------*
111900970526     C* NOTE COSTRUZIONE SCHIERE
112000970526     C*------------------------------------------------------------------------*
112100970526     C*
112200970526     C* -MSG-MESSAGGI
112300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
112400970526     C* (NON TOCCARE MSG,1 E MSG,2)
112500970526     C*
112600970526     C* -CMD-COMANDI
112700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
112800970526     C*
112900970526     C* -OPD-OPZIONI
113000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
113100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
113200970529     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
113300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
113400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
113500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
113600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
113700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
113800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
113900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
114000970526     C*
114100970526     C* -FUD-FUNZIONI
114200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
114300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
114400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
114500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
114600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
114700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
114800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
114900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
115000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
115100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
115200970526     O*------------------------------------------------------------------------*
115300970526** DEC - DECODIFICHE
115400970526    MODIFICA                                                      1
115500970526  ANNULLAMENTO                                                    2
115600970526VISUALIZZAZIONE                                                   3
115700970526   RIPRISTINO                                                     4
115800970526  INSERIMENTO                                                     5
115900020415     COPIA                                                        6
116000020416  CONF. PROP.                                                     7
116100020416** MSG - MESSAGGI
116200020415TISIT4R- Tasto funzionale non gestito                                          1
116300020415TISIT4R- Inserimento effettuato                                                2
116400020415TISIT4R- Modifica effettuata                                                   3
116500020415TISIT4R- Annullamento effettuato                                               4
116600020415TISIT4R- Ripristino effettuato                                                 5
116700020415TISIT4R- Richiesta funzione per record inesistente.                            6
116800020415TISIT4R- Impossibile inserire, record già esistente.                           7
116900020415TISIT4R- Richiesta funzione per record esistente                               8
117000020415TISIT4R- Impossibile indicare '*' nella Ragione Sociale, correggere.           9
117100020415TISIT4R- Impossibile indicare il sinonimo uguale alla Ragione Sociale.         10
117200020415TISIT4R- Per indicare qualsiasi valore inserire solo '*', correggere.          11
117300020415TISIT4R- Tutti i campi sono obbligatori                                        12
117400020415TISIT4R- Il sinonimo non si riferisce a nessun destinatario dis./sup.          13
117500020415TISIT4R- Impossibile indicare '*' nel sinonimo, correggere.                    14
117600020415TISIT4R- Copia effettuata                                                      15
117700020415TISIT4R- ATTENZIONE... dati a video sostituiti !                               16
117800020415TISIT4R- NAZ/PRV/CAP/LOC incongruenti, verificare e correggere.                17
117900020424TISIT4R- Errore, per il sinonimo non esiste nessun possibile destinatario.     18
118000020708TISIT4R- Errore, il sinonimo già presente come anagrafica dis./sup.            19
118100020708TISIT4R- Errore, il sinonimo già presente come sinonimo dis./sup.              20
118200040319TISIT4R- La Ragione Sociale non può essere o iniziare con '*'.                 21
118300970526** FUD - FUNZIONI
118400970526                   -        (S/N)=N                        funz.01¬
118500970526                   -        (S/N)=N                        funz.02¬
118600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
118700970526                   -        (S/N)=N                        funz.04¬
118800970526                   -        (S/N)=N                        funz.05¬
118900000407F06=Conferma       -        (S/N)=S                        funz.06¬
119000970527                   -        (S/N)=N                        funz.07¬
119100970527                   -        (S/N)=N                        funz.08¬
119200970527                   -        (S/N)=N                        funz.09¬
119300970527                   -        (S/N)=N                        funz.10¬
119400970526                   -        (S/N)=N                        funz.11¬
119500970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
119600970526                   -        (S/N)=N                        funz.13¬
119700970526                   -        (S/N)=N                        funz.14¬
119800970526                   -        (S/N)=N                        funz.15¬
119900970526                   -        (S/N)=N                        funz.16¬
120000970526                   -        (S/N)=N                        funz.17¬
120100970526                   -        (S/N)=N                        funz.18¬
120200970526                   -        (S/N)=N                        funz.19¬
120300970526                   -        (S/N)=N                        funz.20¬
120400970526                   -        (S/N)=N                        funz.21¬
120500970526                   -        (S/N)=N                        funz.22¬
120600970526                   -        (S/N)=N                        funz.23¬
120700970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
