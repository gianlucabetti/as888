000100970526     H*------------------------------------------------------------------------*
000200060811     H* CONVERSIONE FILE DB2/400 IN EXCEL
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800100219     FTRBMm8d   cf   e             WORKSTN INFDS(DEVDS1)
000900060811     F                                     SFILE(TCM8DMS:MSGULT)
001000050617     Ftntbe01l  IF   E           K DISK
001100060905     Ftimar01l  IF   E           K DISK
001200060811     FPRTEXCEL  O    F  378        PRINTER OFLIND(*INOF) USROPN
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D* MESSAGGI
001700970526     D*------------
001800000807     D MSG             S             78    DIM(24) CTDATA PERRCD(1)
001900970526     D*------------
002000970526     D* DESCRIZIONI TASTI FUNZIONALI
002100970526     D*------------
002200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
002300970526     D*------------
002400970526     D* FUNZIONI DA CARICARE A VIDEO
002500970526     D*------------
002600970526     D FUV             S             19    DIM(24)
002700970526     D*------------------------------------------------------------------------*
002800970526     D* INPUT
002900970526     D*------------------------------------------------------------------------*
003000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
003100970526     D*------------
003200970526     D FUDDS           DS
003300970526     D  FUDDE1                 1      1
003400970526     D  FUDFUN                 2      3
003500970526     D  FUDDE2                 4     19
003600970526     D  FUDFI0                20     34
003700970526     D  FUDATT                35     35
003800970526     D  FUDFI1                36     80
003900970526     D  FUDDES                 1     19
004000970526     D  FUDRIG                 1     80
004100970526     D*------------
004200970526     D* DS GESTIONE TASTI FUNZIONALI
004300970526     D*------------
004400970526     D DSKEY         E DS
004500970526     D DEVDS1          DS
004600970526     D  KEY                  369    369
004700970526     D*------------
004800970526     D* DS "XMSG" - GESTIONE MESSAGGI
004900970526     D*------------
005000970526     D DSMSG         E DS
005100970526     D*------------
005200970526     D* ARCHITETTURA
005300970526     D*------------
005400970526     D KPJBA         E DS
005500040628     D*------------
005600060811     D*------------------
005700060811     D* TABELLEA 'MRA'
005800060811     D*------------------
005900060811     D DMRAXLS       E DS
006000060811     D*------------------
006100060811     D* DS ridefinizione dati utente estesi spool x mailing
006200060811     D*------------------
006300100219     D TRBMM1DS      E DS
006400020923     D*-------------------
006500020923     D* DS REPERIMENTO DATI UTENTE
006600020923     D*-------------------
006700020923     D TIBS34DS      E DS                                                       *Profili utente
006800020923     D DDATIUTE      E DS                                                       *Dati utente
006900020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
007000060811     D*------------------
007100060811     D* VARIABILI D WRK
007200060811     D*------------------
007300060811     D RigaStampa      s            378    inz
007400060811     D Esito           s              1    inz
007500060811     D wLibreria       s             10    inz
007600060811     D wFile           s             10    inz
007700060811     D wMembro         s             10    inz
007800060811     D wSelect         s            378    inz
007900060811     D wWhere          s            378    inz
008000060811     D wOrderBy        s            378    inz
008100060811     D wNomeFileXLS    s             50    inz
008200060811     D wPwdFileXLS     s             15    inz
008300970526     C*------------------------------------------------------------------------*
008400000303     C* MAIN LINES
008500970526     C*------------------------------------------------------------------------*
008600970526     C*
008700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
008800991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
008900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
009000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
009100970526     C*
009200970526     C* EMETTE FORMATO VIDEO
009300970526     C                   EXSR      BUTFMR
009400970526     C*
009500970526     C* GESTIONE TASTI FUNZIONALI
009600970526     C                   EXSR      FUNCON
009700970526E   2C                   ENDDO
009800970526     C*
009900970526     C* FINE PGM
010000970526     C                   SETON                                        LR
010100970526     C*------------------------------------------------------------------------*
010200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
010300970526     C*------------------------------------------------------------------------*
010400970526     C     BUTFMR        BEGSR
010500970526     C*
010600970526     C* EMETTE LA TESTATA
010700060811     C                   WRITE     TCM8D01
010800970526     C*
010900970526     C* SPEDISCE EVENTUALE MESSAGGIO
011000970526     C                   EXSR      SNDMSG
011100970526     C*
011200970526     C* EMETTE LE FUNZIONI
011300060811     C                   WRITE     TCM8D02
011400970526     C*
011500970526     C* EMETTE IL VIDEO
011600060811     C                   EXFMT     TCM8D03
011700970526     C*
011800991214     C* REIMPOSTA FLAG DI ERRORE
011900991214     C                   MOVEL     '0'           ERRL00
012000970526     C*
012100970526     C* REIMPOSTA INDICATORI DI ERRORE
012200060811     C                   SETOFF                                       55
012300970526     C*
012400970526     C                   ENDSR
012500970526     C*------------------------------------------------------------------------*
012600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
012700970526     C*------------------------------------------------------------------------*
012800970526     C     SNDMSG        BEGSR
012900970526     C*
013000970526     C* PULISCE IL SFILE MESSAGGI
013100970526     C                   MOVEL     '1'           *IN40
013200060811     C                   WRITE     TCM8DMC
013300970526     C                   MOVEL     '0'           *IN40
013400970526     C*
013500970526     C* VISUALIZZA IL MESSAGGIO
013600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
013700970526     C                   CALL      'XMSG'        XMSGPA
013800020611     C     DSMSER        IFEQ      *ZEROS
013900970526     C                   MOVEL     DSMSKE        MSGKEY
014000020611     C                   ENDIF
014100970526     C                   Z-ADD     1             MSGULT
014200060811     C                   WRITE     TCM8DMS
014300060811     C                   WRITE     TCM8DMC
014400970526     C*
014500970526     C* MANDA MESSAGGIO A *BLANKS PER PULIRLO
014600970526     C                   MOVEL     *BLANKS       DSMSMS
014700970526     C*
014800970526     C                   ENDSR
014900970526     C*------------------------------------------------------------------------*
015000970526     C* ENTGES - GESTIONE ENTER -> INVIO
015100970526     C*------------------------------------------------------------------------*
015200970526     C     ENTGES        BEGSR
015300970526     C*
015400970526     C* CONTROLLA I CAMPI IMMESSI
015500970526     C                   EXSR      DECODI
015600970526     C*
015700970526     C                   ENDSR
015800970526     C*------------------------------------------------------------------------*
015900970526     C* F03GES - GESTIONE F03 -> FINE
016000970526     C*------------------------------------------------------------------------*
016100970526     C     F03GES        BEGSR
016200970526     C*
016300970526     C* IMPOSTA "PREMUTO F03"
016400970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016500970526     C*
016600970526     C                   ENDSR
016700970526     C*------------------------------------------------------------------------*
016800991214     C* F06GES - GESTIONE F06 -> CONFERMA
016900970526     C*------------------------------------------------------------------------*
017000970526     C     F06GES        BEGSR
017100970526     C*
017200991214     C* CONTROLLA I CAMPI IMMESSI
017300060811     C                   EXSR      DECODI
017400020611     C*
017500991214     C* ESEGUE ROUTINE DI CONFERMA
017600020611     C     ERRL00        IFEQ      '0'
017700060811     C                   EXSR      ESEGUI
017800030603     C                   MOVEL     '1'           F03L00                         *FINE PROGRAMMA
017900991214     C                   ENDIF
018000970526     C*
018100991214     C                   ENDSR
018200970526     C*------------------------------------------------------------------------*
018300970526     C* F12GES - GESTIONE F12 -> RITORNO
018400970526     C*------------------------------------------------------------------------*
018500970526     C     F12GES        BEGSR
018600970526     C*
018700970526     C* IMPOSTA "PREMUTO F12"
018800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018900970526     C*
019000970526     C                   ENDSR
019100970526     C*------------------------------------------------------------------------*
019200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
019300970526     C*------------------------------------------------------------------------*
019400970526     C     F24GES        BEGSR
019500970526     C*
019600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
019700970526     C*
019800970526     C                   ENDSR
019900991214     C*------------------------------------------------------------------------*
020000060811     C* REPERIMENTO REGOLA DA TABELLA "MRA"
020100060811     C*------------------------------------------------------------------------*
020200060811     C     REPMRA        BEGSR
020300060811     C*
020400060811     C* Reperimento tabella MRA
020500060811     C                   CLEAR                   DMRAXLS
020600060811     C                   MOVE(P)   'MRA'         tbeCOD
020700060811     C                   MOVEL(p)  VIDFILE       tbeKE1
020800060913     C                   MOVEL     'XLS'         tbeKE2
020900060811     C     KEYtbe1_P     CHAIN     TNTBE01L
021000060811     C                   IF        %found(TNTBE01L) AND
021100060811     C                             tbeATB <> 'A'
021200060811     C                   MOVEL     tbeUNI        DMRAXLS
021300060905     C                   IF        §MRAFLDID = *blanks
021400060905     C                   EVAL      ERRL00 = '1'
021500060905     C                   SETON                                        54
021600060905     C                   EVAL      DSMSMS    = 'Record x file ' + VIDFILE      +
021700060905     C                                         ' in tabella MRA incompleto, '  +
021800060905     C                                         'contattare il CED.'
021900060905     C                   ENDIF
022000060905     C*
022100060811     C                   ELSE
022200060811     C                   EVAL      ERRL00 = '1'
022300060811     C                   SETON                                        54
022400060811     C                   EVAL      DSMSMS    = 'Record x file ' + VIDFILE      +
022500060811     C                                         ' non trovato in tabella MRA, ' +
022600060811     C                                         'contattare il CED.'
022700060811     C                   ENDIF
022800060905     C*
022900060905     C* Se tutto ok reperisco gli attributi regola dal file regole BartMailing
023000060905     C                   IF        ERRL00 <> '1'
023100060905     C     §MRAREG       CHAIN     timar01l
023200060905     C                   IF        %found(timar01l)
023300060905     C*
023400060905     C* Se presente su regola nome file lo imposto come default a video
023500060905     C                   IF        MARFOUT <> *blanks
023600060905     C                   EVAL      wNomeFileXLS = MARFOUT
023700060905     C                   ENDIF
023800060905     C                   ELSE
023900060905     C                   EVAL      ERRL00 = '1'
024000060905     C                   SETON                                        54
024100060905     C                   EVAL      DSMSMS    = 'Regola ' + §MRAREG             +
024200060905     C                                         ' in tabella MRA inesistente, ' +
024300060905     C                                         'contattare il CED.'
024400060905     C                   ENDIF
024500060905     C                   ENDIF
024600060811     C*
024700060811     C                   ENDSR
024800970526     C*------------------------------------------------------------------------*
024900991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
025000970526     C*------------------------------------------------------------------------*
025100991214     C     ESEGUI        BEGSR
025200060811     C*
025300060811     C* Reperisco la libreria in cui risiede il file richiesto
025400100219     C                   CALL      'TRBMM8C'
025500060811     C                   PARM      *blanks       wLibreria
025600060811     C                   PARM      VIDFILE       wFile
025700060811     C*
025800060811     C* Imposto quindi i dati utenti estesi
025900100219     C                   CLEAR                   TRBMM1DS
026000060811     C                   EVAL      §CM1TIPS = §MRAREG
026100060811     C                   EVAL      §CM1IDP  = §MRAIDPRO
026200060811     C                   MOVEL(P)  DUTPOU        §CM1PO
026300060811     C*
026400060811     C* Eseguo override x generare uno spool ad "hoc"
026500060811     C                   CALL      'TRUL83C1'
026600060811     C                   PARM      'P'           OPZIONE           1
026700060811     C                   PARM      'PRTEXCEL'    PRTFILE          10
026800100219     C                   PARM                    TRBMM1DS
026900060811     C                   PARM                    §MRAOUTQI
027000060811     C                   PARM                    Esito             1
027100060811     C*
027200060811     C* Valorizzo le variabili d definizione stringa SQL
027300060811     C                   eval      wSelect = '*'
027400060811     C                   EVAL      wWhere = %trim(§MRAFLDID) + ' = ' + '''' +
027500060811     C                                      %trim(VIDIDJOB)  + ''''
027600060811     C*
027700060811     C* Creo lo spool
027800060811     C                   OPEN      PRTEXCEL
027900060811     C*
028000060811     C* - Prima riga: Libreria AS400
028100060822     C                   EVAL      RigaStampa = '1'+wLibreria
028200060811     C                   EXCEPT    STA_RIGA
028300060811     C*
028400060811     C* - Seconda riga: File AS400
028500060822     C                   EVAL      RigaStampa = '2'+wFile
028600060811     C                   EXCEPT    STA_RIGA
028700060811     C*
028800060811     C* - Terza riga: Membro AS400
028900060822     C                   EVAL      RigaStampa = '3'+wMembro
029000060811     C                   EXCEPT    STA_RIGA
029100060811     C*
029200060811     C* - Quarta riga: SELECT
029300060822     C                   EVAL      RigaStampa = '4'+wSelect
029400060811     C                   EXCEPT    STA_RIGA
029500060811     C*
029600060811     C* - Quinta riga: WHERE
029700060822     C                   EVAL      RigaStampa = '5'+wWhere
029800060811     C                   EXCEPT    STA_RIGA
029900060811     C*
030000060811     C* - Sesta riga: ORDER BY
030100060822     C                   EVAL      RigaStampa = '6'+wOrderBy
030200060811     C                   EXCEPT    STA_RIGA
030300060811     C*
030400060811     C* - Settima riga: Nome file Excel
030500060822     C                   IF        wNomeFileXLS = *blanks
030600060822     C                   EVAL      wNomeFileXLS = %trim(wFile) + '_' +
030700060822     C                                            %trim(VIDIDJOB)
030800060822     C                   ENDIF
030900060822     C                   EVAL      RigaStampa = '7'+wNomeFileXLS
031000060811     C                   EXCEPT    STA_RIGA
031100060811     C*
031200060811     C* - Ottava riga: Password file Excel
031300060905     C                   IF        wPwdFileXLS = *blanks
031400060905     C                   EVAL      wPwdFileXLS = %trim(VIDPWDXLS)
031500060905     C                   ENDIF
031600060822     C                   EVAL      RigaStampa = '8'+wPwdFileXLS
031700060811     C                   EXCEPT    STA_RIGA
031800060811     C*
031900060811     C* Chiudo lo spool
032000060811     C                   CLOSE     PRTEXCEL
032100060811     C*
032200060811     C* Eseguo delete override precedente
032300060811     C                   CALL      'TRUL83C1'
032400060811     C                   PARM      'D'           OPZIONE
032500060811     C                   PARM                    PRTFILE
032600100219     C                   PARM                    TRBMM1DS
032700060811     C                   PARM                    §MRAOUTQI
032800060811     C                   PARM                    Esito
032900991214     C*
033000970526     C                   ENDSR
033100970526     C*------------------------------------------------------------------------*
033200991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
033300970526     C*------------------------------------------------------------------------*
033400970526     C     DECODI        BEGSR
033500060905     C*
033600060905     C* INDICATORI DI VISUALIZZAZIONE
033700060905     C                   SETON                                        70
033800060905     C                   SETOFF                                       71
033900000306     C*
034000060811     C                   EVAL      ERRL00 = '0'
034100060811     C                   SETOFF                                       5455
034200060811     C*--------------------
034300060811     C* OBBLIGATORIO FILE
034400060811     C*--------------------
034500060811     C     ERRL00        IFEQ      '0'
034600060811     C                   IF        VIDFILE = *blanks
034700060811     C                   EVAL      ERRL00 = '1'
034800060811     C                   SETON                                        54
034900060811     C                   MOVEL     MSG(2)        DSMSMS
035000060811     C                   ELSE
035100060905     C                   EXSR      REPMRA
035200060811     C                   ENDIF
035300060811     C                   ENDIF
035400991015     C*--------------------
035500060811     C* OBBLIGATORIO ID ELABORAZIONE (JOB)
035600991015     C*--------------------
035700060811     C     ERRL00        IFEQ      '0'
035800060905     C                   IF        §MRAFLDID <> *all'*'
035900060905     C                   SETOFF                                       70
036000060811     C                   IF        VIDIDJOB = *blanks
036100060811     C                   EVAL      ERRL00 = '1'
036200060811     C                   SETON                                        55
036300060811     C                   MOVEL     MSG(2)        DSMSMS
036400020611     C                   ENDIF
036500060905     C                   ENDIF
036600020611     C                   ENDIF
036700060905     C*--------------------
036800060905     C* COMPONGO IL PERCORSO \ NOME FILE COMPLETO A VIDEO
036900060905     C*--------------------
037000060905     C     ERRL00        IFEQ      '0'
037100060905     C                   SETON                                        71
037200060913     C                   IF        %trim(wNomeFileXLS) = '*AUTO*'
037300060913     C                   EVAL      wNomeFileXLS = %trim(VIDFILE)
037400060913     C                   ENDIF
037500060913     C*
037600060905     C                   IF        VIDIDJOB <> *blanks
037700080331     C                   EVAL      wNomeFileXLS = %trim(wNomeFileXLS) + '_' +
037800080331     C                                            %trim(VIDIDJOB)
037900060905     C                   ENDIF
038000060905     C*
038100060905     C                   EVAL      VIDNOMXLS = '(File: N:\BartMail\'          +
038200060905     C                             %trim(%editw(DUTPOU:'0   '))+'\'           +
038300060905     C                             %trim(MARPOUT)+'\'                         +
038400060905     C                             %trim(wNomeFileXLS)+'.XLS)'
038500060905     C                   ENDIF
038600000306     C*
038700970528     C                   ENDSR
038800970526     C*------------------------------------------------------------------------*
038900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
039000970526     C*------------------------------------------------------------------------*
039100970526     C     AZZVID        BEGSR
039200060905     C*
039300060905     C* INDICATORI DI VISUALIZZAZIONE
039400060905     C                   SETON                                        70
039500060905     C                   SETOFF                                       71
039600060811     C*
039700970526     C* CAMPI
039800060811     C                   MOVEL     *blanks       VIDFILE
039900060811     C                   MOVEL     *blanks       VIDIDJOB
040000060905     C                   MOVEL     *blanks       VIDPWDXLS
040100970526     C* DECODIFICHE
040200060905     C                   MOVEL     *blanks       VIDNOMXLS
040300970526     C*
040400970526     C                   ENDSR
040500970526     C*------------------------------------------------------------------------*
040600020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
040700970526     C*------------------------------------------------------------------------*
040800970526     C     IMPFUN        BEGSR
040900970526     C*
041000970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
041100970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
041200970526     C                   EXSR      FUNGES
041300970526     C*
041400970526     C                   ENDSR
041500020923     C*--------------------------------------------------------------------------------------------*
041600020923     C* REPERISCE I DATI UTENTE
041700020923     C*--------------------------------------------------------------------------------------------*
041800020923     C     REPDATIUTE    BEGSR
041900020923     C*
042000020923     C* INIZIALIZZA VARIABILI DI WRK
042100020923     C                   CLEAR                   TIBS34DS
042200020923     C                   CLEAR                   AZUTEDS
042300020923     C                   CLEAR                   DDATIUTE
042400020923     C*
042500020923     C     *DTAARA       DEFINE    §azute        azuteds
042600020923     C     *DTAARA       DEFINE    §datiute      ddatiute
042700020923     C                   IN(E)     *DTAARA
042800020923if  1C                   IF        %Error
042900020923     c                   EVAL      I34Tla = 'L'
043000020923     C                   CALL      'TIBS34R'
043100020923     C                   PARM                    Tibs34Ds
043200020923     C                   IN        *DTAARA
043300020923e   1C                   ENDIF
043400020923     C*
043500020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
043600020923     C                   MOVEL(P)  RSUT          DSFIRS           20
043700020923     C*
043800020923     C                   ENDSR
043900970526     C*------------------------------------------------------------------------*
044000970526     C* *INZSR - OPERAZIONI INIZIALI
044100970526     C*------------------------------------------------------------------------*
044200970526     C     *INZSR        BEGSR
044300991217     C*
044400991217     C     *ENTRY        PLIST
044500991217     C                   PARM                    KPJBA
044600020923     C*
044700020923     C* REPERISCE I DATI DELL'UTENTE
044800020923     C                   EXSR      REPDATIUTE
044900060811     C*
045000050617     C* Definizione chiavi - TNTBE01L
045100060811     C     KEYtbe1_P     KLIST
045200050617     C                   KFLD                    tbeCOD
045300050617     C                   KFLD                    tbeKE1
045400060811     C                   KFLD                    tbeKE2
045500970526     C*
045600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
045700970526     C     XMSGPA        PLIST
045800970526     C                   PARM                    DSMSMS
045900970526     C                   PARM                    DSMSPG
046000970526     C                   PARM                    DSMSKE
046100970526     C                   PARM                    DSMSER
046200970526     C*
046300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
046400970526     C                   CALL      'XKEY'
046500970526     C                   PARM                    DSKEY
046600970526     C*---
046700970526     C* VARIABILI NON RIFERITE AL DATA BASE
046800970526     C*---
046900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
047000970526     C*---
047100970526     C* VARIABILI DI CONTROLLO
047200970526     C*---
047300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
047400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
047500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
047600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
047700970526     C*
047800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
047900100219     C                   MOVEL     'TRBMM8R'     DSMSPG
048000100219     C                   MOVEL     'TRBMM8R'     MSGQUE
048100970526     C                   MOVEL     *BLANKS       MSGKEY
048200970526     C                   MOVEL     *BLANKS       DSMSMS
048300000310     C*
048400000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
048500000310     C                   EXSR      AZZVID
048600970526     C*
048700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
048800970526     C                   EXSR      IMPFUN
048900970526     C*
049000970526     C                   ENDSR
049100970526     C*------------------------------------------------------------------------*
049200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
049300970526     C*------------------------------------------------------------------------*
049400970526     C     FUNGES        BEGSR
049500970526     C*
049600970526     C* PULISCO CAMPI DI LAVORO
049700970526     C                   Z-ADD     0             W                 5 0
049800970526     C*
049900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
050000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
050100970526     C     1             DO        24            I
050200970526     C                   MOVEL     FUD(I)        FUDRIG
050300970526     C*
050400970526     C* PULIZIA FUNZIONI PRECEDENTI
050500970526     C                   MOVEL     *BLANKS       FUV(I)
050600970526     C*
050700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
050800970526     C* (SALTA F24)
050900970526     C     FUDATT        IFEQ      'S'
051000970526     C     FUDFUN        ANDNE     '24'
051100970526     C                   ADD       1             W
051200970526     C                   MOVEL     FUDDES        FUV(W)
051300970526     C                   END
051400970526     C                   END                                                    DO WHILE
051500970526     C*
051600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
051700970526     C* ALTRIMENTI DISATTIVO F24
051800970526     C     1             DO        24            I
051900970526     C                   MOVEL     FUD(I)        FUDRIG
052000970526     C*
052100970526     C     FUDFUN        IFEQ      '24'
052200970526     C     W             IFGT      6
052300970526     C                   MOVEL     'S'           FUDATT
052400970526     C                   ELSE
052500970526     C                   MOVEL     *BLANKS       FUDATT
052600970526     C                   END                                                    W GT 8
052700970526     C                   MOVEL     FUDRIG        FUD(I)
052800970526     C                   END
052900970526     C                   END                                                    DO WHILE
053000970526     C*
053100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
053200970526     C     W             DIV       6             I
053300970526     C                   MVR                     Z
053400970526     C*
053500970526     C     Z             IFNE      0
053600970526     C     I             ADD       1             Z
053700970526     C                   ELSE
053800970526     C                   Z-ADD     I             Z                 5 0
053900970526     C                   END
054000970526     C*
054100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
054200970526     C                   ADD       1             FUNGRU
054300970526     C     FUNGRU        IFGT      Z
054400970526     C     FUNGRU        ORLT      1
054500970526     C                   Z-ADD     1             FUNGRU
054600970526     C                   END
054700970526     C*
054800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
054900970526     C     FUNGRU        IFEQ      1
055000970526     C                   MOVEL     FUV(1)        FUN001
055100970526     C                   MOVEL     FUV(2)        FUN002
055200970526     C                   MOVEL     FUV(3)        FUN003
055300970526     C                   MOVEL     FUV(4)        FUN004
055400970526     C                   MOVEL     FUV(5)        FUN005
055500970526     C                   MOVEL     FUV(6)        FUN006
055600970526     C                   END
055700970526     C*
055800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
055900970526     C     FUNGRU        IFEQ      2
056000970526     C                   MOVEL     FUV(7)        FUN001
056100970526     C                   MOVEL     FUV(8)        FUN002
056200970526     C                   MOVEL     FUV(9)        FUN003
056300970526     C                   MOVEL     FUV(10)       FUN004
056400970526     C                   MOVEL     FUV(11)       FUN005
056500970526     C                   MOVEL     FUV(12)       FUN006
056600970526     C                   END
056700970526     C*
056800970526     C* TERZO GRUPPO DI 6 FUNZIONI
056900970526     C     FUNGRU        IFEQ      3
057000970526     C                   MOVEL     FUV(13)       FUN001
057100970526     C                   MOVEL     FUV(14)       FUN002
057200970526     C                   MOVEL     FUV(15)       FUN003
057300970526     C                   MOVEL     FUV(16)       FUN004
057400970526     C                   MOVEL     FUV(17)       FUN005
057500970526     C                   MOVEL     FUV(18)       FUN006
057600970526     C                   END
057700970526     C*
057800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
057900970526     C     FUNGRU        IFEQ      4
058000970526     C                   MOVEL     FUV(19)       FUN001
058100970526     C                   MOVEL     FUV(20)       FUN002
058200970526     C                   MOVEL     FUV(21)       FUN003
058300970526     C                   MOVEL     FUV(22)       FUN004
058400970526     C                   MOVEL     *BLANKS       FUN005
058500970526     C                   MOVEL     *BLANKS       FUN006
058600970526     C                   END
058700970526     C*
058800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
058900970526     C                   MOVEL     *BLANKS       FUN007
059000970526     C                   MOVEL     *BLANKS       FUN008
059100970526     C*
059200970526     C     1             DO        24            I
059300970526     C                   MOVEL     FUD(I)        FUDRIG
059400970526     C     FUDFUN        IFEQ      '24'
059500970526     C     FUDATT        ANDEQ     'S'
059600970526     C                   MOVEL     FUDDES        FUN008
059700970526     C                   END
059800970526     C                   END                                                    DO
059900970526     C*
060000970526     C                   ENDSR
060100970526     C*------------------------------------------------------------------------*
060200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
060300970526     C*------------------------------------------------------------------------*
060400970526     C     FUNCON        BEGSR
060500970526     C*
060600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
060700970526     C                   EXSR      FUNVAR
060800970526     C*
060900970526     C* PULISCO CAMPI DI LAVORO
061000970526     C                   Z-ADD     0             I                 5 0
061100970526     C                   Z-ADD     0             W                 5 0
061200970526     C*
061300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
061400970526     C     FUNKEY        IFNE      *BLANKS
061500970526     C     1             DO        24            I
061600970526     C                   MOVEL     FUD(I)        FUDRIG
061700970526     C*
061800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
061900970526     C     FUNKEY        IFEQ      FUDFUN
062000970526     C     FUDATT        ANDEQ     'S'
062100970526     C                   Z-ADD     I             W
062200970526     C                   END
062300970526     C                   END                                                    DO WHILE
062400970526     C*
062500970526     C* FUNZIONE NON ATTIVA (ERRORE)
062600970526     C     W             IFEQ      0
062700970526     C*
062800970526     C* EMISSIONE MESSAGGIO DI ERRORE
062900970526     C                   MOVEL     MSG(1)        DSMSMS
063000970526     C*
063100970526     C                   END                                                    W=0
063200970526     C                   END                                                    FUNKEY NE *BLANKS
063300970526     C*
063400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
063500970526     C     W             IFNE      0
063600970526     C     FUNKEY        OREQ      *BLANKS
063700970526     C     FUNKEY        IFEQ      *BLANKS
063800970526     C                   EXSR      ENTGES
063900970526     C                   END
064000970526     C     FUNKEY        IFEQ      '03'
064100970526     C                   EXSR      F03GES
064200970526     C                   END
064300970526     C     FUNKEY        IFEQ      '06'
064400970526     C                   EXSR      F06GES
064500970526     C                   END
064600970526     C     FUNKEY        IFEQ      '12'
064700970526     C                   EXSR      F12GES
064800970526     C                   END
064900970526     C     FUNKEY        IFEQ      '24'
065000970526     C                   EXSR      F24GES
065100970526     C                   END
065200970526     C                   END
065300970526     C*
065400970526     C                   ENDSR
065500970526     C*------------------------------------------------------------------------*
065600970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
065700970526     C*------------------------------------------------------------------------*
065800970526     C     FUNVAR        BEGSR
065900970526     C*
066000970526     C* INIZIALIZZA VARIABILE -FUNKEY-
066100970526     C*
066200970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
066300970526     C     KEY           IFEQ      ENTER
066400970526     C                   MOVEL     *BLANKS       FUNKEY
066500970526     C                   END
066600970526     C     KEY           IFEQ      F1
066700970526     C                   MOVEL     '01'          FUNKEY
066800970526     C                   END
066900970526     C     KEY           IFEQ      F2
067000970526     C                   MOVEL     '02'          FUNKEY
067100970526     C                   END
067200970526     C     KEY           IFEQ      F3
067300970526     C                   MOVEL     '03'          FUNKEY
067400970526     C                   END
067500970526     C     KEY           IFEQ      F4
067600970526     C                   MOVEL     '04'          FUNKEY
067700970526     C                   END
067800970526     C     KEY           IFEQ      F5
067900970526     C                   MOVEL     '05'          FUNKEY
068000970526     C                   END
068100970526     C     KEY           IFEQ      F6
068200970526     C                   MOVEL     '06'          FUNKEY
068300970526     C                   END
068400970526     C     KEY           IFEQ      F7
068500970526     C                   MOVEL     '07'          FUNKEY
068600970526     C                   END
068700970526     C     KEY           IFEQ      F8
068800970526     C                   MOVEL     '08'          FUNKEY
068900970526     C                   END
069000970526     C     KEY           IFEQ      F9
069100970526     C                   MOVEL     '09'          FUNKEY
069200970526     C                   END
069300970526     C     KEY           IFEQ      F10
069400970526     C                   MOVEL     '10'          FUNKEY
069500970526     C                   END
069600970526     C     KEY           IFEQ      F11
069700970526     C                   MOVEL     '11'          FUNKEY
069800970526     C                   END
069900970526     C     KEY           IFEQ      F12
070000970526     C                   MOVEL     '12'          FUNKEY
070100970526     C                   END
070200970526     C     KEY           IFEQ      F13
070300970526     C                   MOVEL     '13'          FUNKEY
070400970526     C                   END
070500970526     C     KEY           IFEQ      F14
070600970526     C                   MOVEL     '14'          FUNKEY
070700970526     C                   END
070800970526     C     KEY           IFEQ      F15
070900970526     C                   MOVEL     '15'          FUNKEY
071000970526     C                   END
071100970526     C     KEY           IFEQ      F16
071200970526     C                   MOVEL     '16'          FUNKEY
071300970526     C                   END
071400970526     C     KEY           IFEQ      F17
071500970526     C                   MOVEL     '17'          FUNKEY
071600970526     C                   END
071700970526     C     KEY           IFEQ      F18
071800970526     C                   MOVEL     '18'          FUNKEY
071900970526     C                   END
072000970526     C     KEY           IFEQ      F19
072100970526     C                   MOVEL     '19'          FUNKEY
072200970526     C                   END
072300970526     C     KEY           IFEQ      F20
072400970526     C                   MOVEL     '20'          FUNKEY
072500970526     C                   END
072600970526     C     KEY           IFEQ      F21
072700970526     C                   MOVEL     '21'          FUNKEY
072800970526     C                   END
072900970526     C     KEY           IFEQ      F22
073000970526     C                   MOVEL     '22'          FUNKEY
073100970526     C                   END
073200970526     C     KEY           IFEQ      F23
073300970526     C                   MOVEL     '23'          FUNKEY
073400970526     C                   END
073500970526     C     KEY           IFEQ      F24
073600970526     C                   MOVEL     '24'          FUNKEY
073700970526     C                   END
073800970526     C     KEY           IFEQ      ROLLUP
073900970526     C                   MOVEL     'RU'          FUNKEY
074000970526     C                   END
074100970526     C     KEY           IFEQ      HELP
074200970526     C                   MOVEL     'HP'          FUNKEY
074300970526     C                   END
074400970526     C                   ENDSR
074500970526     C*------------------------------------------------------------------------*
074600970526     C* NOTE COSTRUZIONE SCHIERE
074700970526     C*------------------------------------------------------------------------*
074800970526     C*
074900970526     C* -MSG-MESSAGGI
075000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
075100970526     C* (NON TOCCARE MSG,1 E MSG,2)
075200970526     C*
075300970526     C* -CMD-COMANDI
075400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
075500970526     C*
075600970526     C* -OPD-OPZIONI
075700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
075800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
075900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
076000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
076100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
076200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
076300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
076400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
076500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
076600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
076700970526     C*
076800970526     C* -FUD-FUNZIONI
076900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
077000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
077100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
077200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
077300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
077400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
077500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
077600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
077700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
077800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
077900970526     O*------------------------------------------------------------------------*
078000060811     O*------------------------------------------------------------------------*
078100060811     O* STAMPA
078200060811     O*------------------------------------------------------------------------*
078300060811     OPRTEXCEL  E            STA_RIGA    1
078400060811     O                       RigaStampa
078500060811     O*------------------------------------------------------------------------*
078600970526** MSG - MESSAGGI
078700100219TRBMM8R- Tasto funzionale non gestito.                                         1
078800100219TRBMM8R- Scelta obbligatoria.                                                  2
078900100219TRBMM8R- ID elaborazinoe indicato inesistente.                                 3
079000100219TRBMM8R-                                                                       4
079100100219TRBMM8R-                                                                       5
079200100219TRBMM8R-                                                                       6
079300100219TRBMM8R-                                                                       7
079400100219TRBMM8R-                                                                       8
079500100219TRBMM8R-                                                                       9
079600100219TRBMM8R-                                                                       10
079700100219TRBMM8R-                                                                       11
079800100219TRBMM8R-                                                                       12
079900100219TRBMM8R-                                                                       13
080000100219TRBMM8R-                                                                       14
080100100219TRBMM8R-                                                                       15
080200100219TRBMM8R-                                                                       16
080300100219TRBMM8R-                                                                       17
080400100219TRBMM8R-                                                                       18
080500100219TRBMM8R-                                                                       19
080600100219TRBMM8R-                                                                       20
080700100219TRBMM8R-                                                                       21
080800100219TRBMM8R-                                                                       22
080900100219TRBMM8R-                                                                       23
081000100219TRBMM8R-                                                                       24
081100970526** FUD - FUNZIONI
081200970526                   -        (S/N)=N                        funz.01¬
081300970526                   -        (S/N)=N                        funz.02¬
081400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
081500060811                   -        (S/N)=N                        funz.04¬
081600970526                   -        (S/N)=N                        funz.05¬
081700060811F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
081800970527                   -        (S/N)=N                        funz.07¬
081900970527                   -        (S/N)=N                        funz.08¬
082000970527                   -        (S/N)=N                        funz.09¬
082100970527                   -        (S/N)=N                        funz.10¬
082200970526                   -        (S/N)=N                        funz.11¬
082300970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
082400970526                   -        (S/N)=N                        funz.13¬
082500970526                   -        (S/N)=N                        funz.14¬
082600970526                   -        (S/N)=N                        funz.15¬
082700970526                   -        (S/N)=N                        funz.16¬
082800970526                   -        (S/N)=N                        funz.17¬
082900970526                   -        (S/N)=N                        funz.18¬
083000970526                   -        (S/N)=N                        funz.19¬
083100970526                   -        (S/N)=N                        funz.20¬
083200970526                   -        (S/N)=N                        funz.21¬
083300970526                   -        (S/N)=N                        funz.22¬
083400970526                   -        (S/N)=N                        funz.23¬
083500060811F24=Altri tasti    -        (S/N)=N                        funz.24¬
