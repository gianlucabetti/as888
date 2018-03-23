000100970526     H*------------------------------------------------------------------------*
000200060811     H* CONVERSIONE FILE DB2/400 IN EXCEL
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800060811     Ftrtcm8d   cf   e             WORKSTN INFDS(DEVDS1)
000900060811     F                                     SFILE(TCM8DMS:MSGULT)
001000110616     Ftntbe01l  IF   E           K DISK
001100110616     Ftimar01l  IF   E           K DISK
001200970526     D*------------------------------------------------------------------------*
001300970526     D* SCHIERE
001400970526     D*------------------------------------------------------------------------*
001500970526     D* MESSAGGI
001600970526     D*------------
001700000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001800970526     D*------------
001900970526     D* DESCRIZIONI TASTI FUNZIONALI
002000970526     D*------------
002100970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* FUNZIONI DA CARICARE A VIDEO
002400970526     D*------------
002500970526     D FUV             S             19    DIM(24)
002600970526     D*------------------------------------------------------------------------*
002700970526     D* INPUT
002800970526     D*------------------------------------------------------------------------*
002900970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003000970526     D*------------
003100970526     D FUDDS           DS
003200970526     D  FUDDE1                 1      1
003300970526     D  FUDFUN                 2      3
003400970526     D  FUDDE2                 4     19
003500970526     D  FUDFI0                20     34
003600970526     D  FUDATT                35     35
003700970526     D  FUDFI1                36     80
003800970526     D  FUDDES                 1     19
003900970526     D  FUDRIG                 1     80
004000970526     D*------------
004100970526     D* DS GESTIONE TASTI FUNZIONALI
004200970526     D*------------
004300970526     D DSKEY         E DS
004400970526     D DEVDS1          DS
004500970526     D  KEY                  369    369
004600970526     D*------------
004700970526     D* DS "XMSG" - GESTIONE MESSAGGI
004800970526     D*------------
004900970526     D DSMSG         E DS
005000110616     D*------------------
005100110616     D* TABELLEA 'MRA'
005200110616     D*------------------
005300110616     D DMRAXLS       E DS
005400970526     D*------------
005500970526     D* ARCHITETTURA
005600970526     D*------------
005700970526     D KPJBA         E DS
005800040628     D*------------
005900020923     D*-------------------
006000020923     D* DS REPERIMENTO DATI UTENTE
006100020923     D*-------------------
006200020923     D TIBS34DS      E DS                                                       *Profili utente
006300020923     D DDATIUTE      E DS                                                       *Dati utente
006400020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006500110616     D*------------------
006600110616     D* VARIABILI D WRK
006700110616     D*------------------
006800110616     D wNomeFileXLS    s             50    inz
006900970526     C*------------------------------------------------------------------------*
007000000303     C* MAIN LINES
007100970526     C*------------------------------------------------------------------------*
007200970526     C*
007300970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
007400991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
007500970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
007600970526     C     FINL00        OREQ      '1'                                          *FINE PGM
007700970526     C*
007800970526     C* EMETTE FORMATO VIDEO
007900970526     C                   EXSR      BUTFMR
008000970526     C*
008100970526     C* GESTIONE TASTI FUNZIONALI
008200970526     C                   EXSR      FUNCON
008300970526E   2C                   ENDDO
008400970526     C*
008500970526     C* FINE PGM
008600970526     C                   SETON                                        LR
008700970526     C*------------------------------------------------------------------------*
008800970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
008900970526     C*------------------------------------------------------------------------*
009000970526     C     BUTFMR        BEGSR
009100970526     C*
009200970526     C* EMETTE LA TESTATA
009300060811     C                   WRITE     TCM8D01
009400970526     C*
009500970526     C* SPEDISCE EVENTUALE MESSAGGIO
009600970526     C                   EXSR      SNDMSG
009700970526     C*
009800970526     C* EMETTE LE FUNZIONI
009900060811     C                   WRITE     TCM8D02
010000970526     C*
010100970526     C* EMETTE IL VIDEO
010200060811     C                   EXFMT     TCM8D03
010300970526     C*
010400991214     C* REIMPOSTA FLAG DI ERRORE
010500991214     C                   MOVEL     '0'           ERRL00
010600970526     C*
010700970526     C* REIMPOSTA INDICATORI DI ERRORE
010800060811     C                   SETOFF                                       55
010900970526     C*
011000970526     C                   ENDSR
011100970526     C*------------------------------------------------------------------------*
011200970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
011300970526     C*------------------------------------------------------------------------*
011400970526     C     SNDMSG        BEGSR
011500970526     C*
011600970526     C* PULISCE IL SFILE MESSAGGI
011700970526     C                   MOVEL     '1'           *IN40
011800060811     C                   WRITE     TCM8DMC
011900970526     C                   MOVEL     '0'           *IN40
012000970526     C*
012100970526     C* VISUALIZZA IL MESSAGGIO
012200970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
012300970526     C                   CALL      'XMSG'        XMSGPA
012400020611     C     DSMSER        IFEQ      *ZEROS
012500970526     C                   MOVEL     DSMSKE        MSGKEY
012600020611     C                   ENDIF
012700970526     C                   Z-ADD     1             MSGULT
012800060811     C                   WRITE     TCM8DMS
012900060811     C                   WRITE     TCM8DMC
013000970526     C*
013100970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
013200970526     C                   MOVEL     *BLANKS       DSMSMS
013300970526     C*
013400970526     C                   ENDSR
013500970526     C*------------------------------------------------------------------------*
013600970526     C* ENTGES - GESTIONE ENTER -> INVIO
013700970526     C*------------------------------------------------------------------------*
013800970526     C     ENTGES        BEGSR
013900970526     C*
014000970526     C* CONTROLLA I CAMPI IMMESSI
014100970526     C                   EXSR      DECODI
014200970526     C*
014300970526     C                   ENDSR
014400970526     C*------------------------------------------------------------------------*
014500970526     C* F03GES - GESTIONE F03 -> FINE
014600970526     C*------------------------------------------------------------------------*
014700970526     C     F03GES        BEGSR
014800970526     C*
014900970526     C* IMPOSTA "PREMUTO F03"
015000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015100970526     C*
015200970526     C                   ENDSR
015300970526     C*------------------------------------------------------------------------*
015400991214     C* F06GES - GESTIONE F06 -> CONFERMA
015500970526     C*------------------------------------------------------------------------*
015600970526     C     F06GES        BEGSR
015700970526     C*
015800991214     C* CONTROLLA I CAMPI IMMESSI
015900060811     C                   EXSR      DECODI
016000020611     C*
016100991214     C* ESEGUE ROUTINE DI CONFERMA
016200020611     C     ERRL00        IFEQ      '0'
016300060811     C                   EXSR      ESEGUI
016400030603     C                   MOVEL     '1'           F03L00                         *FINE PROGRAMMA
016500991214     C                   ENDIF
016600970526     C*
016700991214     C                   ENDSR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* F12GES - GESTIONE F12 -> RITORNO
017000970526     C*------------------------------------------------------------------------*
017100970526     C     F12GES        BEGSR
017200970526     C*
017300970526     C* IMPOSTA "PREMUTO F12"
017400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
017500970526     C*
017600970526     C                   ENDSR
017700970526     C*------------------------------------------------------------------------*
017800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
017900970526     C*------------------------------------------------------------------------*
018000970526     C     F24GES        BEGSR
018100970526     C*
018200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
018300970526     C*
018400970526     C                   ENDSR
018500110616     C*------------------------------------------------------------------------*
018600110616     C* REPERIMENTO REGOLA DA TABELLA "MRA"
018700110616     C*------------------------------------------------------------------------*
018800110616     C     REPMRA        BEGSR
018900110616     C*
019000110616     C* Reperimento tabella MRA
019100110616     C                   CLEAR                   DMRAXLS
019200110616     C                   MOVE(P)   'MRA'         tbeCOD
019300110616     C                   MOVEL(p)  VIDFILE       tbeKE1
019400110616     C                   MOVEL     'XLS'         tbeKE2
019500110616     C     KEYtbe1_P     CHAIN     TNTBE01L
019600110616     C                   IF        %found(TNTBE01L) AND
019700110616     C                             tbeATB <> 'A'
019800110616     C                   MOVEL     tbeUNI        DMRAXLS
019900110616     C                   IF        §MRAFLDID = *blanks
020000110616     C                   EVAL      ERRL00 = '1'
020100110616     C                   SETON                                        54
020200110616     C                   EVAL      DSMSMS    = 'Record x file ' + VIDFILE      +
020300110616     C                                         ' in tabella MRA incompleto, '  +
020400110616     C                                         'contattare il CED.'
020500110616     C                   ENDIF
020600110616     C*
020700110616     C                   ELSE
020800110616     C                   EVAL      ERRL00 = '1'
020900110616     C                   SETON                                        54
021000110616     C                   EVAL      DSMSMS    = 'Record x file ' + VIDFILE      +
021100110616     C                                         ' non trovato in tabella MRA, ' +
021200110616     C                                         'contattare il CED.'
021300110616     C                   ENDIF
021400110616     C*
021500110616     C* Se tutto ok reperisco gli attributi regola dal file regole BartMailing
021600110616     C                   IF        ERRL00 <> '1'
021700110616     C     §MRAREG       CHAIN     timar01l
021800110616     C                   IF        %found(timar01l)
021900110616     C*
022000110616     C* Se presente su regola nome file lo imposto come default a video
022100110616     C                   IF        MARFOUT <> *blanks
022200110616     C                   EVAL      wNomeFileXLS = MARFOUT
022300110616     C                   ENDIF
022400110616     C                   ELSE
022500110616     C                   EVAL      ERRL00 = '1'
022600110616     C                   SETON                                        54
022700110616     C                   EVAL      DSMSMS    = 'Regola ' + §MRAREG             +
022800110616     C                                         ' in tabella MRA inesistente, ' +
022900110616     C                                         'contattare il CED.'
023000110616     C                   ENDIF
023100110616     C                   ENDIF
023200110616     C*
023300110616     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
023600970526     C*------------------------------------------------------------------------*
023700991214     C     ESEGUI        BEGSR
023800110616     C*
023900110616     C* Chiamo *pgm esecutore
024000110616     C                   CALL      'TRTCM8R1'
024100110616     C                   PARM                    VIDFILE
024200110616     C                   PARM                    VIDIDJOB
024300110616     C                   PARM                    VIDPWDXLS
024400110616     C                   PARM                    VIDNOMXLS
024500110616     C                   PARM      *blanks       EMLDEST         121
024600111202     C                   PARM      *blanks       EMLVAR           80
024700120509     C                   PARM      *blanks       MRAKE2           15
024800110616     C                   PARM      '0'           ESITO             1
024900991214     C*
025000970526     C                   ENDSR
025100970526     C*------------------------------------------------------------------------*
025200991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
025300970526     C*------------------------------------------------------------------------*
025400970526     C     DECODI        BEGSR
025500060905     C*
025600060905     C* INDICATORI DI VISUALIZZAZIONE
025700060905     C                   SETON                                        70
025800060905     C                   SETOFF                                       71
025900000306     C*
026000060811     C                   EVAL      ERRL00 = '0'
026100060811     C                   SETOFF                                       5455
026200060811     C*--------------------
026300060811     C* OBBLIGATORIO FILE
026400060811     C*--------------------
026500060811     C     ERRL00        IFEQ      '0'
026600060811     C                   IF        VIDFILE = *blanks
026700060811     C                   EVAL      ERRL00 = '1'
026800060811     C                   SETON                                        54
026900060811     C                   MOVEL     MSG(2)        DSMSMS
027000060811     C                   ELSE
027100060905     C                   EXSR      REPMRA
027200060811     C                   ENDIF
027300060811     C                   ENDIF
027400991015     C*--------------------
027500060811     C* OBBLIGATORIO ID ELABORAZIONE (JOB)
027600991015     C*--------------------
027700060811     C     ERRL00        IFEQ      '0'
027800060905     C                   IF        §MRAFLDID <> *all'*'
027900060905     C                   SETOFF                                       70
028000060811     C                   IF        VIDIDJOB = *blanks
028100060811     C                   EVAL      ERRL00 = '1'
028200060811     C                   SETON                                        55
028300060811     C                   MOVEL     MSG(2)        DSMSMS
028400020611     C                   ENDIF
028500060905     C                   ENDIF
028600020611     C                   ENDIF
028700060905     C*--------------------
028800110616     C* COMPONGO IL PERCORSO \ NOME FILE COMPLETO A VIDEO
028900060905     C*--------------------
029000060905     C     ERRL00        IFEQ      '0'
029100060905     C                   SETON                                        71
029200060913     C                   IF        %trim(wNomeFileXLS) = '*AUTO*'
029300060913     C                   EVAL      wNomeFileXLS = %trim(VIDFILE)
029400060913     C                   ENDIF
029500060913     C*
029600060905     C                   IF        VIDIDJOB <> *blanks
029700080331     C                   EVAL      wNomeFileXLS = %trim(wNomeFileXLS) + '_' +
029800080331     C                                            %trim(VIDIDJOB)
029900060905     C                   ENDIF
030000060905     C*
030100060905     C                   EVAL      VIDNOMXLS = '(File: N:\BartMail\'          +
030200060905     C                             %trim(%editw(DUTPOU:'0   '))+'\'           +
030300110616     C                             %trim(MARPOUT)+'\'                         +
030400060905     C                             %trim(wNomeFileXLS)+'.XLS)'
030500060905     C                   ENDIF
030600000306     C*
030700970528     C                   ENDSR
030800970526     C*------------------------------------------------------------------------*
030900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
031000970526     C*------------------------------------------------------------------------*
031100970526     C     AZZVID        BEGSR
031200060905     C*
031300060905     C* INDICATORI DI VISUALIZZAZIONE
031400060905     C                   SETON                                        70
031500060905     C                   SETOFF                                       71
031600060811     C*
031700970526     C* CAMPI
031800060811     C                   MOVEL     *blanks       VIDFILE
031900060811     C                   MOVEL     *blanks       VIDIDJOB
032000060905     C                   MOVEL     *blanks       VIDPWDXLS
032100970526     C* DECODIFICHE
032200060905     C                   MOVEL     *blanks       VIDNOMXLS
032300970526     C*
032400970526     C                   ENDSR
032500970526     C*------------------------------------------------------------------------*
032600020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
032700970526     C*------------------------------------------------------------------------*
032800970526     C     IMPFUN        BEGSR
032900970526     C*
033000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
033100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
033200970526     C                   EXSR      FUNGES
033300970526     C*
033400970526     C                   ENDSR
033500020923     C*--------------------------------------------------------------------------------------------*
033600020923     C* REPERISCE I DATI UTENTE
033700020923     C*--------------------------------------------------------------------------------------------*
033800020923     C     REPDATIUTE    BEGSR
033900020923     C*
034000020923     C* INIZIALIZZA VARIABILI DI WRK
034100020923     C                   CLEAR                   TIBS34DS
034200020923     C                   CLEAR                   AZUTEDS
034300020923     C                   CLEAR                   DDATIUTE
034400020923     C*
034500020923     C     *DTAARA       DEFINE    §azute        azuteds
034600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
034700020923     C                   IN(E)     *DTAARA
034800020923if  1C                   IF        %Error
034900020923     c                   EVAL      I34Tla = 'L'
035000020923     C                   CALL      'TIBS34R'
035100020923     C                   PARM                    Tibs34Ds
035200020923     C                   IN        *DTAARA
035300020923e   1C                   ENDIF
035400020923     C*
035500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
035600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
035700020923     C*
035800020923     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* *INZSR - OPERAZIONI INIZIALI
036100970526     C*------------------------------------------------------------------------*
036200970526     C     *INZSR        BEGSR
036300991217     C*
036400991217     C     *ENTRY        PLIST
036500991217     C                   PARM                    KPJBA
036600020923     C*
036700020923     C* REPERISCE I DATI DELL'UTENTE
036800020923     C                   EXSR      REPDATIUTE
036900060811     C*
037000050617     C* Definizione chiavi - TNTBE01L
037100060811     C     KEYtbe1_P     KLIST
037200050617     C                   KFLD                    tbeCOD
037300050617     C                   KFLD                    tbeKE1
037400060811     C                   KFLD                    tbeKE2
037500970526     C*
037600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
037700970526     C     XMSGPA        PLIST
037800970526     C                   PARM                    DSMSMS
037900970526     C                   PARM                    DSMSPG
038000970526     C                   PARM                    DSMSKE
038100970526     C                   PARM                    DSMSER
038200970526     C*
038300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
038400970526     C                   CALL      'XKEY'
038500970526     C                   PARM                    DSKEY
038600970526     C*---
038700970526     C* VARIABILI NON RIFERITE AL DATA BASE
038800970526     C*---
038900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
039000970526     C*---
039100970526     C* VARIABILI DI CONTROLLO
039200970526     C*---
039300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
039400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
039500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
039600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
039700970526     C*
039800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
039900060811     C                   MOVEL     'TRTCM8R'     DSMSPG
040000060811     C                   MOVEL     'TRTCM8R'     MSGQUE
040100970526     C                   MOVEL     *BLANKS       MSGKEY
040200970526     C                   MOVEL     *BLANKS       DSMSMS
040300000310     C*
040400000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
040500000310     C                   EXSR      AZZVID
040600970526     C*
040700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
040800970526     C                   EXSR      IMPFUN
040900970526     C*
041000970526     C                   ENDSR
041100970526     C*------------------------------------------------------------------------*
041200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
041300970526     C*------------------------------------------------------------------------*
041400970526     C     FUNGES        BEGSR
041500970526     C*
041600970526     C* PULISCO CAMPI DI LAVORO
041700970526     C                   Z-ADD     0             W                 5 0
041800970526     C*
041900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
042000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
042100970526     C     1             DO        24            I
042200970526     C                   MOVEL     FUD(I)        FUDRIG
042300970526     C*
042400970526     C* PULIZIA FUNZIONI PRECEDENTI
042500970526     C                   MOVEL     *BLANKS       FUV(I)
042600970526     C*
042700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
042800970526     C* (SALTA F24)
042900970526     C     FUDATT        IFEQ      'S'
043000970526     C     FUDFUN        ANDNE     '24'
043100970526     C                   ADD       1             W
043200970526     C                   MOVEL     FUDDES        FUV(W)
043300970526     C                   END
043400970526     C                   END                                                    DO WHILE
043500970526     C*
043600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
043700970526     C* ALTRIMENTI DISATTIVO F24
043800970526     C     1             DO        24            I
043900970526     C                   MOVEL     FUD(I)        FUDRIG
044000970526     C*
044100970526     C     FUDFUN        IFEQ      '24'
044200970526     C     W             IFGT      6
044300970526     C                   MOVEL     'S'           FUDATT
044400970526     C                   ELSE
044500970526     C                   MOVEL     *BLANKS       FUDATT
044600970526     C                   END                                                    W GT 8
044700970526     C                   MOVEL     FUDRIG        FUD(I)
044800970526     C                   END
044900970526     C                   END                                                    DO WHILE
045000970526     C*
045100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
045200970526     C     W             DIV       6             I
045300970526     C                   MVR                     Z
045400970526     C*
045500970526     C     Z             IFNE      0
045600970526     C     I             ADD       1             Z
045700970526     C                   ELSE
045800970526     C                   Z-ADD     I             Z                 5 0
045900970526     C                   END
046000970526     C*
046100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
046200970526     C                   ADD       1             FUNGRU
046300970526     C     FUNGRU        IFGT      Z
046400970526     C     FUNGRU        ORLT      1
046500970526     C                   Z-ADD     1             FUNGRU
046600970526     C                   END
046700970526     C*
046800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
046900970526     C     FUNGRU        IFEQ      1
047000970526     C                   MOVEL     FUV(1)        FUN001
047100970526     C                   MOVEL     FUV(2)        FUN002
047200970526     C                   MOVEL     FUV(3)        FUN003
047300970526     C                   MOVEL     FUV(4)        FUN004
047400970526     C                   MOVEL     FUV(5)        FUN005
047500970526     C                   MOVEL     FUV(6)        FUN006
047600970526     C                   END
047700970526     C*
047800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
047900970526     C     FUNGRU        IFEQ      2
048000970526     C                   MOVEL     FUV(7)        FUN001
048100970526     C                   MOVEL     FUV(8)        FUN002
048200970526     C                   MOVEL     FUV(9)        FUN003
048300970526     C                   MOVEL     FUV(10)       FUN004
048400970526     C                   MOVEL     FUV(11)       FUN005
048500970526     C                   MOVEL     FUV(12)       FUN006
048600970526     C                   END
048700970526     C*
048800970526     C* TERZO GRUPPO DI 6 FUNZIONI
048900970526     C     FUNGRU        IFEQ      3
049000970526     C                   MOVEL     FUV(13)       FUN001
049100970526     C                   MOVEL     FUV(14)       FUN002
049200970526     C                   MOVEL     FUV(15)       FUN003
049300970526     C                   MOVEL     FUV(16)       FUN004
049400970526     C                   MOVEL     FUV(17)       FUN005
049500970526     C                   MOVEL     FUV(18)       FUN006
049600970526     C                   END
049700970526     C*
049800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
049900970526     C     FUNGRU        IFEQ      4
050000970526     C                   MOVEL     FUV(19)       FUN001
050100970526     C                   MOVEL     FUV(20)       FUN002
050200970526     C                   MOVEL     FUV(21)       FUN003
050300970526     C                   MOVEL     FUV(22)       FUN004
050400970526     C                   MOVEL     *BLANKS       FUN005
050500970526     C                   MOVEL     *BLANKS       FUN006
050600970526     C                   END
050700970526     C*
050800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
050900970526     C                   MOVEL     *BLANKS       FUN007
051000970526     C                   MOVEL     *BLANKS       FUN008
051100970526     C*
051200970526     C     1             DO        24            I
051300970526     C                   MOVEL     FUD(I)        FUDRIG
051400970526     C     FUDFUN        IFEQ      '24'
051500970526     C     FUDATT        ANDEQ     'S'
051600970526     C                   MOVEL     FUDDES        FUN008
051700970526     C                   END
051800970526     C                   END                                                    DO
051900970526     C*
052000970526     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
052300970526     C*------------------------------------------------------------------------*
052400970526     C     FUNCON        BEGSR
052500970526     C*
052600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
052700970526     C                   EXSR      FUNVAR
052800970526     C*
052900970526     C* PULISCO CAMPI DI LAVORO
053000970526     C                   Z-ADD     0             I                 5 0
053100970526     C                   Z-ADD     0             W                 5 0
053200970526     C*
053300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
053400970526     C     FUNKEY        IFNE      *BLANKS
053500970526     C     1             DO        24            I
053600970526     C                   MOVEL     FUD(I)        FUDRIG
053700970526     C*
053800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
053900970526     C     FUNKEY        IFEQ      FUDFUN
054000970526     C     FUDATT        ANDEQ     'S'
054100970526     C                   Z-ADD     I             W
054200970526     C                   END
054300970526     C                   END                                                    DO WHILE
054400970526     C*
054500970526     C* FUNZIONE NON ATTIVA (ERRORE)
054600970526     C     W             IFEQ      0
054700970526     C*
054800970526     C* EMISSIONE MESSAGGIO DI ERRORE
054900970526     C                   MOVEL     MSG(1)        DSMSMS
055000970526     C*
055100970526     C                   END                                                    W=0
055200970526     C                   END                                                    FUNKEY NE *BLANKS
055300970526     C*
055400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
055500970526     C     W             IFNE      0
055600970526     C     FUNKEY        OREQ      *BLANKS
055700970526     C     FUNKEY        IFEQ      *BLANKS
055800970526     C                   EXSR      ENTGES
055900970526     C                   END
056000970526     C     FUNKEY        IFEQ      '03'
056100970526     C                   EXSR      F03GES
056200970526     C                   END
056300970526     C     FUNKEY        IFEQ      '06'
056400970526     C                   EXSR      F06GES
056500970526     C                   END
056600970526     C     FUNKEY        IFEQ      '12'
056700970526     C                   EXSR      F12GES
056800970526     C                   END
056900970526     C     FUNKEY        IFEQ      '24'
057000970526     C                   EXSR      F24GES
057100970526     C                   END
057200970526     C                   END
057300970526     C*
057400970526     C                   ENDSR
057500970526     C*------------------------------------------------------------------------*
057600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
057700970526     C*------------------------------------------------------------------------*
057800970526     C     FUNVAR        BEGSR
057900970526     C*
058000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
058100970526     C*
058200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
058300970526     C     KEY           IFEQ      ENTER
058400970526     C                   MOVEL     *BLANKS       FUNKEY
058500970526     C                   END
058600970526     C     KEY           IFEQ      F1
058700970526     C                   MOVEL     '01'          FUNKEY
058800970526     C                   END
058900970526     C     KEY           IFEQ      F2
059000970526     C                   MOVEL     '02'          FUNKEY
059100970526     C                   END
059200970526     C     KEY           IFEQ      F3
059300970526     C                   MOVEL     '03'          FUNKEY
059400970526     C                   END
059500970526     C     KEY           IFEQ      F4
059600970526     C                   MOVEL     '04'          FUNKEY
059700970526     C                   END
059800970526     C     KEY           IFEQ      F5
059900970526     C                   MOVEL     '05'          FUNKEY
060000970526     C                   END
060100970526     C     KEY           IFEQ      F6
060200970526     C                   MOVEL     '06'          FUNKEY
060300970526     C                   END
060400970526     C     KEY           IFEQ      F7
060500970526     C                   MOVEL     '07'          FUNKEY
060600970526     C                   END
060700970526     C     KEY           IFEQ      F8
060800970526     C                   MOVEL     '08'          FUNKEY
060900970526     C                   END
061000970526     C     KEY           IFEQ      F9
061100970526     C                   MOVEL     '09'          FUNKEY
061200970526     C                   END
061300970526     C     KEY           IFEQ      F10
061400970526     C                   MOVEL     '10'          FUNKEY
061500970526     C                   END
061600970526     C     KEY           IFEQ      F11
061700970526     C                   MOVEL     '11'          FUNKEY
061800970526     C                   END
061900970526     C     KEY           IFEQ      F12
062000970526     C                   MOVEL     '12'          FUNKEY
062100970526     C                   END
062200970526     C     KEY           IFEQ      F13
062300970526     C                   MOVEL     '13'          FUNKEY
062400970526     C                   END
062500970526     C     KEY           IFEQ      F14
062600970526     C                   MOVEL     '14'          FUNKEY
062700970526     C                   END
062800970526     C     KEY           IFEQ      F15
062900970526     C                   MOVEL     '15'          FUNKEY
063000970526     C                   END
063100970526     C     KEY           IFEQ      F16
063200970526     C                   MOVEL     '16'          FUNKEY
063300970526     C                   END
063400970526     C     KEY           IFEQ      F17
063500970526     C                   MOVEL     '17'          FUNKEY
063600970526     C                   END
063700970526     C     KEY           IFEQ      F18
063800970526     C                   MOVEL     '18'          FUNKEY
063900970526     C                   END
064000970526     C     KEY           IFEQ      F19
064100970526     C                   MOVEL     '19'          FUNKEY
064200970526     C                   END
064300970526     C     KEY           IFEQ      F20
064400970526     C                   MOVEL     '20'          FUNKEY
064500970526     C                   END
064600970526     C     KEY           IFEQ      F21
064700970526     C                   MOVEL     '21'          FUNKEY
064800970526     C                   END
064900970526     C     KEY           IFEQ      F22
065000970526     C                   MOVEL     '22'          FUNKEY
065100970526     C                   END
065200970526     C     KEY           IFEQ      F23
065300970526     C                   MOVEL     '23'          FUNKEY
065400970526     C                   END
065500970526     C     KEY           IFEQ      F24
065600970526     C                   MOVEL     '24'          FUNKEY
065700970526     C                   END
065800970526     C     KEY           IFEQ      ROLLUP
065900970526     C                   MOVEL     'RU'          FUNKEY
066000970526     C                   END
066100970526     C     KEY           IFEQ      HELP
066200970526     C                   MOVEL     'HP'          FUNKEY
066300970526     C                   END
066400970526     C                   ENDSR
066500970526     C*------------------------------------------------------------------------*
066600970526     C* NOTE COSTRUZIONE SCHIERE
066700970526     C*------------------------------------------------------------------------*
066800970526     C*
066900970526     C* -MSG-MESSAGGI
067000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
067100970526     C* (NON TOCCARE MSG,1 E MSG,2)
067200970526     C*
067300970526     C* -CMD-COMANDI
067400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
067500970526     C*
067600970526     C* -OPD-OPZIONI
067700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
067800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
067900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
068000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
068100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
068200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
068300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
068400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
068500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
068600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
068700970526     C*
068800970526     C* -FUD-FUNZIONI
068900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
069000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
069100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
069200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
069300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
069400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
069500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
069600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
069700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
069800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
069900970526     O*------------------------------------------------------------------------*
070000970526** MSG - MESSAGGI
070100060811TRTCM8R- Tasto funzionale non gestito.                                         1
070200060811TRTCM8R- Scelta obbligatoria.                                                  2
070300060811TRTCM8R- ID elaborazinoe indicato inesistente.                                 3
070400060811TRTCM8R-                                                                       4
070500060811TRTCM8R-                                                                       5
070600060811TRTCM8R-                                                                       6
070700060811TRTCM8R-                                                                       7
070800060811TRTCM8R-                                                                       8
070900060811TRTCM8R-                                                                       9
071000060811TRTCM8R-                                                                       10
071100060811TRTCM8R-                                                                       11
071200060811TRTCM8R-                                                                       12
071300060811TRTCM8R-                                                                       13
071400060811TRTCM8R-                                                                       14
071500060811TRTCM8R-                                                                       15
071600060811TRTCM8R-                                                                       16
071700060811TRTCM8R-                                                                       17
071800060811TRTCM8R-                                                                       18
071900060811TRTCM8R-                                                                       19
072000060811TRTCM8R-                                                                       20
072100060811TRTCM8R-                                                                       21
072200060811TRTCM8R-                                                                       22
072300060811TRTCM8R-                                                                       23
072400060811TRTCM8R-                                                                       24
072500970526** FUD - FUNZIONI
072600970526                   -        (S/N)=N                        funz.01¬
072700970526                   -        (S/N)=N                        funz.02¬
072800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
072900060811                   -        (S/N)=N                        funz.04¬
073000970526                   -        (S/N)=N                        funz.05¬
073100060811F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
073200970527                   -        (S/N)=N                        funz.07¬
073300970527                   -        (S/N)=N                        funz.08¬
073400970527                   -        (S/N)=N                        funz.09¬
073500970527                   -        (S/N)=N                        funz.10¬
073600970526                   -        (S/N)=N                        funz.11¬
073700970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
073800970526                   -        (S/N)=N                        funz.13¬
073900970526                   -        (S/N)=N                        funz.14¬
074000970526                   -        (S/N)=N                        funz.15¬
074100970526                   -        (S/N)=N                        funz.16¬
074200970526                   -        (S/N)=N                        funz.17¬
074300970526                   -        (S/N)=N                        funz.18¬
074400970526                   -        (S/N)=N                        funz.19¬
074500970526                   -        (S/N)=N                        funz.20¬
074600970526                   -        (S/N)=N                        funz.21¬
074700970526                   -        (S/N)=N                        funz.22¬
074800970526                   -        (S/N)=N                        funz.23¬
074900060811F24=Altri tasti    -        (S/N)=N                        funz.24¬
