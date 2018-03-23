000100970526     H*------------------------------------------------------------------------*
000200060811     H* CONVERSIONE FILE DB2/400 IN EXCEL
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800060811     Ftrtcm8d   cf   e             WORKSTN INFDS(DEVDS1)
000900060811     F                                     SFILE(TCM8DMS:MSGULT)
001000050617     Ftntbe01l  IF   E           K DISK
001100060811     FPRTEXCEL  O    F  378        PRINTER OFLIND(*INOF) USROPN
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
005000970526     D*------------
005100970526     D* ARCHITETTURA
005200970526     D*------------
005300970526     D KPJBA         E DS
005400040628     D*------------
005500060811     D*------------------
005600060811     D* TABELLEA 'MRA'
005700060811     D*------------------
005800060811     D DMRAXLS       E DS
005900060811     D*------------------
006000060811     D* DS ridefinizione dati utente estesi spool x mailing
006100060811     D*------------------
006200060811     D TRTCM1DS      E DS
006300020923     D*-------------------
006400020923     D* DS REPERIMENTO DATI UTENTE
006500020923     D*-------------------
006600020923     D TIBS34DS      E DS                                                       *Profili utente
006700020923     D DDATIUTE      E DS                                                       *Dati utente
006800020923     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006900060811     D*------------------
007000060811     D* COSTANTI D PROCEDURA
007100060811     D*------------------
007200060811     D cTIPOMODULO     s             10    inz('BOLLE043A4')
007300060811     D cDATIUTENTE     s             10    inz('BBraunCoUs')
007400060811     D*------------------
007500060811     D* DS RIDEFINIZIONE RECORDSET
007600060811     D*------------------
007700060811     D  dsFields       ds
007800060811     D  wColumnName                  10
007900060811     D  wColumnText                  50
008000060811     D*------------------
008100060811     D* VARIABILI D WRK
008200060811     D*------------------
008300060811     D RigaStampa      s            378    inz
008400060811     D Esito           s              1    inz
008500060811     D wLibreria       s             10    inz
008600060811     D wFile           s             10    inz
008700060811     D wMembro         s             10    inz
008800060811     D wSelect         s            378    inz
008900060811     D wWhere          s            378    inz
009000060811     D wOrderBy        s            378    inz
009100060811     D wNomeFileXLS    s             50    inz
009200060811     D wPwdFileXLS     s             15    inz
009300970526     C*------------------------------------------------------------------------*
009400000303     C* MAIN LINES
009500970526     C*------------------------------------------------------------------------*
009600970526     C*
009700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
009800991214     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
009900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
010000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
010100970526     C*
010200970526     C* EMETTE FORMATO VIDEO
010300970526     C                   EXSR      BUTFMR
010400970526     C*
010500970526     C* GESTIONE TASTI FUNZIONALI
010600970526     C                   EXSR      FUNCON
010700970526E   2C                   ENDDO
010800970526     C*
010900970526     C* FINE PGM
011000970526     C                   SETON                                        LR
011100970526     C*------------------------------------------------------------------------*
011200970526     C* BUTFMR - EMISSIONE FORMATO A VIDEO
011300970526     C*------------------------------------------------------------------------*
011400970526     C     BUTFMR        BEGSR
011500970526     C*
011600970526     C* EMETTE LA TESTATA
011700060811     C                   WRITE     TCM8D01
011800970526     C*
011900970526     C* SPEDISCE EVENTUALE MESSAGGIO
012000970526     C                   EXSR      SNDMSG
012100970526     C*
012200970526     C* EMETTE LE FUNZIONI
012300060811     C                   WRITE     TCM8D02
012400970526     C*
012500970526     C* EMETTE IL VIDEO
012600060811     C                   EXFMT     TCM8D03
012700970526     C*
012800991214     C* REIMPOSTA FLAG DI ERRORE
012900991214     C                   MOVEL     '0'           ERRL00
013000970526     C*
013100970526     C* REIMPOSTA INDICATORI DI ERRORE
013200060811     C                   SETOFF                                       55
013300970526     C*
013400970526     C                   ENDSR
013500970526     C*------------------------------------------------------------------------*
013600970526     C* SNDMSG - SPEDISCE EVENTUALE MESSAGGIO
013700970526     C*------------------------------------------------------------------------*
013800970526     C     SNDMSG        BEGSR
013900970526     C*
014000970526     C* PULISCE IL SFILE MESSAGGI
014100970526     C                   MOVEL     '1'           *IN40
014200060811     C                   WRITE     TCM8DMC
014300970526     C                   MOVEL     '0'           *IN40
014400970526     C*
014500970526     C* VISUALIZZA IL MESSAGGIO
014600970526     C                   Z-ADD     *ZEROS        MSGULT            4 0
014700970526     C                   CALL      'XMSG'        XMSGPA
014800020611     C     DSMSER        IFEQ      *ZEROS
014900970526     C                   MOVEL     DSMSKE        MSGKEY
015000020611     C                   ENDIF
015100970526     C                   Z-ADD     1             MSGULT
015200060811     C                   WRITE     TCM8DMS
015300060811     C                   WRITE     TCM8DMC
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
017500970526     C*
017600970526     C                   ENDSR
017700060811     C*------------------------------------------------------------------------*
017800060811     C* F04GES - GESTIONE F04 -> SCELTA ELABORAZIONE
017900060811     C*------------------------------------------------------------------------*
018000060811     C     F04GES        BEGSR
018100060811     C*
018200060811     C* Emetto subito il listing degli spool che rispondono ai requisiti richiesti
018300060811     C                   CALL      'TRUL83R'
018400060811     C                   PARM      *blanks       p_USRDFT         10
018500060811     C                   PARM      *blanks       p_FILLIB         20
018600060811     C                   PARM      cTIPOMODULO   p_FORMTYP        10
018700060811     C                   PARM      cDATIUTENTE   p_USRDATA        10
018800060811     C                   PARM      *blanks       p_USRABIL         1
018900060811     C                   PARM      'S'           p_BYPASS          1
019000060811     C                   PARM      'S'           p_GESLIV          1
019100060811     C                   PARM      *blanks       p_USREMAIL       30
019200060811     C                   PARM      *blanks       o_VARFIL         10
019300060811     C                   PARM      *blanks       o_SPLNUM          6
019400060811     C                   PARM      *blanks       o_VARUSN         10
019500060811     C                   PARM      *blanks       o_VARJNM         10
019600060811     C                   PARM      *blanks       o_JOBNUM          6
019700060811     C                   PARM      *blanks       o_VARTYP         10
019800060811     C                   PARM      *blanks       o_VARDTA         10
019900060811     C
020000060811     C*
020100060811     C* Imposto a video l'ID del job scelto
020200060811     C                   EVAL      VIDIDJOB = %trim(o_VARJNM) + '/' +
020300060811     C                                        %trim(o_VARUSN) + '/' +
020400060811     C                                        %trim(o_JOBNUM)
020500060811     C*
020600060811     C                   ENDSR
020700970526     C*------------------------------------------------------------------------*
020800991214     C* F06GES - GESTIONE F06 -> CONFERMA
020900970526     C*------------------------------------------------------------------------*
021000970526     C     F06GES        BEGSR
021100970526     C*
021200991214     C* CONTROLLA I CAMPI IMMESSI
021300060811     C                   EXSR      DECODI
021400020611     C*
021500991214     C* ESEGUE ROUTINE DI CONFERMA
021600020611     C     ERRL00        IFEQ      '0'
021700060811     C                   EXSR      ESEGUI
021800030603     C                   MOVEL     '1'           F03L00                         *FINE PROGRAMMA
021900991214     C                   ENDIF
022000970526     C*
022100991214     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300970526     C* F12GES - GESTIONE F12 -> RITORNO
022400970526     C*------------------------------------------------------------------------*
022500970526     C     F12GES        BEGSR
022600970526     C*
022700970526     C* IMPOSTA "PREMUTO F12"
022800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023300970526     C*------------------------------------------------------------------------*
023400970526     C     F24GES        BEGSR
023500970526     C*
023600970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023700970526     C*
023800970526     C                   ENDSR
023900991214     C*------------------------------------------------------------------------*
024000060811     C* REPERIMENTO REGOLA DA TABELLA "MRA"
024100060811     C*------------------------------------------------------------------------*
024200060811     C     REPMRA        BEGSR
024300060811     C*
024400060811     C* Reperimento tabella MRA
024500060811     C                   CLEAR                   DMRAXLS
024600060811     C                   MOVE(P)   'MRA'         tbeCOD
024700060811     C                   MOVEL(p)  wFile         tbeKE1
024800060811     C                   MOVEL     *blanks       tbeKE2
024900060811     C     KEYtbe1_P     CHAIN     TNTBE01L
025000060811     C                   IF        %found(TNTBE01L) AND
025100060811     C                             tbeATB <> 'A'
025200060811     C                   MOVEL     tbeUNI        DMRAXLS
025300060811     C                   ELSE
025400060811     C                   EVAL      ERRL00 = '1'
025500060811     C                   EVAL      DSMSMS    = 'Record x file ' + wFile        +
025600060811     C                                         ' non trovato in tabella MRA, ' +
025700060811     C                                         'contattare il CED.'
025800060811     C                   ENDIF
025900060811     C*
026000060811     C                   ENDSR
026100970526     C*------------------------------------------------------------------------*
026200991214     C* ESEGUI - OPERAZIONI DI LANCIO PGM BATCH
026300970526     C*------------------------------------------------------------------------*
026400991214     C     ESEGUI        BEGSR
026500060811     C*
026600060811     C* Reperisco la libreria in cui risiede il file richiesto
026700060811     C                   CALL      'TRTCM8C'
026800060811     C                   PARM      *blanks       wLibreria
026900060811     C                   PARM      o_VARTYP      wFile
027000060811     C*
027100060811     C/EXEC SQL
027200060811     C+ declare C1 cursor for
027300060811     C+ SELECT COLUMN_TEXT
027400060811     C+ FROM SYSCOLUMNS
027500060811     C+ WHERE TABLE_SCHEMA = :wLibreria AND
027600060811     C+ TABLE_NAME = :wFile
027700060811     C+ for read only
027800060811     C/END-EXEC
027900060811     C
028000060811     C/EXEC SQL
028100060811     C+ open C1
028200060811     C/END-EXEC
028300060811     C
028400060811     C/EXEC SQL
028500060811     C+ Fetch next from C1 into :dsFields
028600060811     C/END-EXEC
028700060811     C*
028800060811     C* Se reperito qualcosa => analizzo
028900060811     C                   dow       sqlcod <> *zeros
029000060811     C*
029100060811     C* Nn considero nell'elenco campi il campo contenente l'ID del job stesso
029200060811     C                   if        wColumnName <> §MRAFLDID
029300060811     C                   if        wSelect = *blanks
029400060811     C                   eval      wSelect = %trim(wSelect) + wColumnText
029500060811     C                   else
029600060811     C                   eval      wSelect = %trim(wSelect) + ', ' + wColumnText
029700060811     C                   endif
029800060811     C                   endif
029900060811     C*
030000060811     C* Proseguo con la lettura del recordset
030100060811     C/EXEC SQL
030200060811     C+ Fetch next from C1 into :wColumnText
030300060811     C/END-EXEC
030400060811     C*
030500060811     C                   enddo
030600060811     C*
030700060811     C/EXEC SQL
030800060811     C+ close C1
030900060811     C/END-EXEC
031000060811     C*
031100060811     C* Eseguo override x generare uno spool ad "hoc"
031200060811     C                   CALL      'TRUL83C1'
031300060811     C                   PARM      'P'           OPZIONE           1
031400060811     C                   PARM      'PRTEXCEL'    PRTFILE          10
031500060811     C                   PARM                    TRTCM1DS
031600060811     C                   PARM                    §MRAOUTQI
031700060811     C                   PARM                    Esito             1
031800060811     C*
031900060811     C* Valorizzo le variabili d definizione stringa SQL
032000060811     C                   EVAL      wWhere = §MRAFLDID + ' = ' +
032100060811     C                                      %trim(o_VARJNM)   +
032200060811     C                                      %trim(o_VARUSN)   +
032300060811     C                                      %trim(o_JOBNUM)
032400060811     C*
032500060811     C* Creo lo spool
032600060811     C                   OPEN      PRTEXCEL
032700060811     C*
032800060811     C* - Prima riga: Libreria AS400
032900060811     C                   EVAL      RigaStampa = wLibreria
033000060811     C                   EXCEPT    STA_RIGA
033100060811     C*
033200060811     C* - Seconda riga: File AS400
033300060811     C                   EVAL      RigaStampa = wFile
033400060811     C                   EXCEPT    STA_RIGA
033500060811     C*
033600060811     C* - Terza riga: Membro AS400
033700060811     C                   EVAL      RigaStampa = wMembro
033800060811     C                   EXCEPT    STA_RIGA
033900060811     C*
034000060811     C* - Quarta riga: SELECT
034100060811     C                   EVAL      RigaStampa = wSelect
034200060811     C                   EXCEPT    STA_RIGA
034300060811     C*
034400060811     C* - Quinta riga: WHERE
034500060811     C                   EVAL      RigaStampa = wWhere
034600060811     C                   EXCEPT    STA_RIGA
034700060811     C*
034800060811     C* - Sesta riga: ORDER BY
034900060811     C                   EVAL      RigaStampa = wOrderBy
035000060811     C                   EXCEPT    STA_RIGA
035100060811     C*
035200060811     C* - Settima riga: Nome file Excel
035300060811     C                   EVAL      RigaStampa = wNomeFileXLS
035400060811     C                   EXCEPT    STA_RIGA
035500060811     C*
035600060811     C* - Ottava riga: Password file Excel
035700060811     C                   EVAL      RigaStampa = wPwdFileXLS
035800060811     C                   EXCEPT    STA_RIGA
035900060811     C*
036000060811     C* Chiudo lo spool
036100060811     C                   CLOSE     PRTEXCEL
036200060811     C*
036300060811     C* Eseguo delete override precedente
036400060811     C                   CALL      'TRUL83C1'
036500060811     C                   PARM      'D'           OPZIONE
036600060811     C                   PARM                    PRTFILE
036700060811     C                   PARM                    TRTCM1DS
036800060811     C                   PARM                    §MRAOUTQI
036900060811     C                   PARM                    Esito
037000991214     C*
037100970526     C                   ENDSR
037200970526     C*------------------------------------------------------------------------*
037300991214     C* DECODI - CONTROLLA CAMPI E DECODIFICA
037400970526     C*------------------------------------------------------------------------*
037500970526     C     DECODI        BEGSR
037600000306     C*
037700060811     C                   EVAL      ERRL00 = '0'
037800060811     C                   SETOFF                                       55
037900991015     C*--------------------
038000060811     C* OBBLIGATORIO ID ELABORAZIONE (JOB)
038100991015     C*--------------------
038200060811     C     ERRL00        IFEQ      '0'
038300060811     C                   IF        VIDIDJOB = *blanks
038400060811     C                   EVAL      ERRL00 = '1'
038500060811     C                   SETON                                        55
038600060811     C                   MOVEL     MSG(2)        DSMSMS
038700060811     C                   ELSE
038800060811     C                   EVAL      VIDDESC = '(File da elaborare: '+ wFile +')'
038900020611     C                   ENDIF
039000020611     C                   ENDIF
039100060811     C*
039200060811     C* Reperisco dati tabellati (Tabella 'MRA') x lo spool corrente
039300060811     C     ERRL00        IFEQ      '0'
039400060811     C                   EXSR      REPMRA
039500060811     C                   ENDIF
039600000306     C*
039700970528     C                   ENDSR
039800970526     C*------------------------------------------------------------------------*
039900970526     C* AZZVID - AZZERA I CAMPI DEL VIDEO
040000970526     C*------------------------------------------------------------------------*
040100970526     C     AZZVID        BEGSR
040200060811     C*
040300970526     C* CAMPI
040400060811     C                   MOVEL     *blanks       VIDIDJOB
040500970526     C* DECODIFICHE
040600060811     C                   MOVEL     *blanks       VIDDESC
040700970526     C*
040800970526     C                   ENDSR
040900970526     C*------------------------------------------------------------------------*
041000020611     C* IMPFUN - IMPOSTA LE FUNZIONI DI INGRESSO
041100970526     C*------------------------------------------------------------------------*
041200970526     C     IMPFUN        BEGSR
041300970526     C*
041400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
041500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0
041600970526     C                   EXSR      FUNGES
041700970526     C*
041800970526     C                   ENDSR
041900020923     C*--------------------------------------------------------------------------------------------*
042000020923     C* REPERISCE I DATI UTENTE
042100020923     C*--------------------------------------------------------------------------------------------*
042200020923     C     REPDATIUTE    BEGSR
042300020923     C*
042400020923     C* INIZIALIZZA VARIABILI DI WRK
042500020923     C                   CLEAR                   TIBS34DS
042600020923     C                   CLEAR                   AZUTEDS
042700020923     C                   CLEAR                   DDATIUTE
042800020923     C*
042900020923     C     *DTAARA       DEFINE    §azute        azuteds
043000020923     C     *DTAARA       DEFINE    §datiute      ddatiute
043100020923     C                   IN(E)     *DTAARA
043200020923if  1C                   IF        %Error
043300020923     c                   EVAL      I34Tla = 'L'
043400020923     C                   CALL      'TIBS34R'
043500020923     C                   PARM                    Tibs34Ds
043600020923     C                   IN        *DTAARA
043700020923e   1C                   ENDIF
043800020923     C*
043900020923     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
044000020923     C                   MOVEL(P)  RSUT          DSFIRS           20
044100020923     C*
044200020923     C                   ENDSR
044300970526     C*------------------------------------------------------------------------*
044400970526     C* *INZSR - OPERAZIONI INIZIALI
044500970526     C*------------------------------------------------------------------------*
044600970526     C     *INZSR        BEGSR
044700991217     C*
044800991217     C     *ENTRY        PLIST
044900991217     C                   PARM                    KPJBA
045000020923     C*
045100020923     C* REPERISCE I DATI DELL'UTENTE
045200020923     C                   EXSR      REPDATIUTE
045300060811     C*
045400060811     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
045500060811     C
045600060811     C/EXEC SQL
045700060811     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
045800060811     C/END-EXEC
045900060811     C*
046000050617     C* Definizione chiavi - TNTBE01L
046100060811     C     KEYtbe1_P     KLIST
046200050617     C                   KFLD                    tbeCOD
046300050617     C                   KFLD                    tbeKE1
046400060811     C                   KFLD                    tbeKE1
046500970526     C*
046600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
046700970526     C     XMSGPA        PLIST
046800970526     C                   PARM                    DSMSMS
046900970526     C                   PARM                    DSMSPG
047000970526     C                   PARM                    DSMSKE
047100970526     C                   PARM                    DSMSER
047200970526     C*
047300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
047400970526     C                   CALL      'XKEY'
047500970526     C                   PARM                    DSKEY
047600970526     C*---
047700970526     C* VARIABILI NON RIFERITE AL DATA BASE
047800970526     C*---
047900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
048000970526     C*---
048100970526     C* VARIABILI DI CONTROLLO
048200970526     C*---
048300970526     C                   MOVEL     '0'           FINL00            1            *FINE PROGRAMMA
048400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
048500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
048600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
048700970526     C*
048800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
048900060811     C                   MOVEL     'TRTCM8R'     DSMSPG
049000060811     C                   MOVEL     'TRTCM8R'     MSGQUE
049100970526     C                   MOVEL     *BLANKS       MSGKEY
049200970526     C                   MOVEL     *BLANKS       DSMSMS
049300000310     C*
049400000310     C* ESEGUO OPERAZIONI PRELIMINARI X VIDEO
049500000310     C                   EXSR      AZZVID
049600970526     C*
049700970526     C* IMPOSTA LE FUNZIONI DI INGRESSO
049800970526     C                   EXSR      IMPFUN
049900970526     C*
050000970526     C                   ENDSR
050100970526     C*------------------------------------------------------------------------*
050200970526     C* FUNGES - GESTIONE TASTI FUNZIONALI DEL PROGRAMMA
050300970526     C*------------------------------------------------------------------------*
050400970526     C     FUNGES        BEGSR
050500970526     C*
050600970526     C* PULISCO CAMPI DI LAVORO
050700970526     C                   Z-ADD     0             W                 5 0
050800970526     C*
050900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
051000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
051100970526     C     1             DO        24            I
051200970526     C                   MOVEL     FUD(I)        FUDRIG
051300970526     C*
051400970526     C* PULIZIA FUNZIONI PRECEDENTI
051500970526     C                   MOVEL     *BLANKS       FUV(I)
051600970526     C*
051700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
051800970526     C* (SALTA F24)
051900970526     C     FUDATT        IFEQ      'S'
052000970526     C     FUDFUN        ANDNE     '24'
052100970526     C                   ADD       1             W
052200970526     C                   MOVEL     FUDDES        FUV(W)
052300970526     C                   END
052400970526     C                   END                                                    DO WHILE
052500970526     C*
052600970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
052700970526     C* ALTRIMENTI DISATTIVO F24
052800970526     C     1             DO        24            I
052900970526     C                   MOVEL     FUD(I)        FUDRIG
053000970526     C*
053100970526     C     FUDFUN        IFEQ      '24'
053200970526     C     W             IFGT      6
053300970526     C                   MOVEL     'S'           FUDATT
053400970526     C                   ELSE
053500970526     C                   MOVEL     *BLANKS       FUDATT
053600970526     C                   END                                                    W GT 8
053700970526     C                   MOVEL     FUDRIG        FUD(I)
053800970526     C                   END
053900970526     C                   END                                                    DO WHILE
054000970526     C*
054100970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
054200970526     C     W             DIV       6             I
054300970526     C                   MVR                     Z
054400970526     C*
054500970526     C     Z             IFNE      0
054600970526     C     I             ADD       1             Z
054700970526     C                   ELSE
054800970526     C                   Z-ADD     I             Z                 5 0
054900970526     C                   END
055000970526     C*
055100970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
055200970526     C                   ADD       1             FUNGRU
055300970526     C     FUNGRU        IFGT      Z
055400970526     C     FUNGRU        ORLT      1
055500970526     C                   Z-ADD     1             FUNGRU
055600970526     C                   END
055700970526     C*
055800970526     C* PRIMO GRUPPO DI 6 FUNZIONI
055900970526     C     FUNGRU        IFEQ      1
056000970526     C                   MOVEL     FUV(1)        FUN001
056100970526     C                   MOVEL     FUV(2)        FUN002
056200970526     C                   MOVEL     FUV(3)        FUN003
056300970526     C                   MOVEL     FUV(4)        FUN004
056400970526     C                   MOVEL     FUV(5)        FUN005
056500970526     C                   MOVEL     FUV(6)        FUN006
056600970526     C                   END
056700970526     C*
056800970526     C* SECONDO GRUPPO DI 6 FUNZIONI
056900970526     C     FUNGRU        IFEQ      2
057000970526     C                   MOVEL     FUV(7)        FUN001
057100970526     C                   MOVEL     FUV(8)        FUN002
057200970526     C                   MOVEL     FUV(9)        FUN003
057300970526     C                   MOVEL     FUV(10)       FUN004
057400970526     C                   MOVEL     FUV(11)       FUN005
057500970526     C                   MOVEL     FUV(12)       FUN006
057600970526     C                   END
057700970526     C*
057800970526     C* TERZO GRUPPO DI 6 FUNZIONI
057900970526     C     FUNGRU        IFEQ      3
058000970526     C                   MOVEL     FUV(13)       FUN001
058100970526     C                   MOVEL     FUV(14)       FUN002
058200970526     C                   MOVEL     FUV(15)       FUN003
058300970526     C                   MOVEL     FUV(16)       FUN004
058400970526     C                   MOVEL     FUV(17)       FUN005
058500970526     C                   MOVEL     FUV(18)       FUN006
058600970526     C                   END
058700970526     C*
058800970526     C* QUARTO GRUPPO DI 6 FUNZIONI
058900970526     C     FUNGRU        IFEQ      4
059000970526     C                   MOVEL     FUV(19)       FUN001
059100970526     C                   MOVEL     FUV(20)       FUN002
059200970526     C                   MOVEL     FUV(21)       FUN003
059300970526     C                   MOVEL     FUV(22)       FUN004
059400970526     C                   MOVEL     *BLANKS       FUN005
059500970526     C                   MOVEL     *BLANKS       FUN006
059600970526     C                   END
059700970526     C*
059800970526     C* F24 SE C'E' VIENE SEMPRE VISUALIZZATO
059900970526     C                   MOVEL     *BLANKS       FUN007
060000970526     C                   MOVEL     *BLANKS       FUN008
060100970526     C*
060200970526     C     1             DO        24            I
060300970526     C                   MOVEL     FUD(I)        FUDRIG
060400970526     C     FUDFUN        IFEQ      '24'
060500970526     C     FUDATT        ANDEQ     'S'
060600970526     C                   MOVEL     FUDDES        FUN008
060700970526     C                   END
060800970526     C                   END                                                    DO
060900970526     C*
061000970526     C                   ENDSR
061100970526     C*------------------------------------------------------------------------*
061200970526     C* FUNCON - GESTIONE TASTI FUNZIONALI DELL'UTENTE
061300970526     C*------------------------------------------------------------------------*
061400970526     C     FUNCON        BEGSR
061500970526     C*
061600970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
061700970526     C                   EXSR      FUNVAR
061800970526     C*
061900970526     C* PULISCO CAMPI DI LAVORO
062000970526     C                   Z-ADD     0             I                 5 0
062100970526     C                   Z-ADD     0             W                 5 0
062200970526     C*
062300970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
062400970526     C     FUNKEY        IFNE      *BLANKS
062500970526     C     1             DO        24            I
062600970526     C                   MOVEL     FUD(I)        FUDRIG
062700970526     C*
062800970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
062900970526     C     FUNKEY        IFEQ      FUDFUN
063000970526     C     FUDATT        ANDEQ     'S'
063100970526     C                   Z-ADD     I             W
063200970526     C                   END
063300970526     C                   END                                                    DO WHILE
063400970526     C*
063500970526     C* FUNZIONE NON ATTIVA (ERRORE)
063600970526     C     W             IFEQ      0
063700970526     C*
063800970526     C* EMISSIONE MESSAGGIO DI ERRORE
063900970526     C                   MOVEL     MSG(1)        DSMSMS
064000970526     C*
064100970526     C                   END                                                    W=0
064200970526     C                   END                                                    FUNKEY NE *BLANKS
064300970526     C*
064400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
064500970526     C     W             IFNE      0
064600970526     C     FUNKEY        OREQ      *BLANKS
064700970526     C     FUNKEY        IFEQ      *BLANKS
064800970526     C                   EXSR      ENTGES
064900970526     C                   END
065000970526     C     FUNKEY        IFEQ      '03'
065100970526     C                   EXSR      F03GES
065200970526     C                   END
065300060811     C     FUNKEY        IFEQ      '04'
065400060811     C                   EXSR      F04GES
065500060811     C                   END
065600970526     C     FUNKEY        IFEQ      '06'
065700970526     C                   EXSR      F06GES
065800970526     C                   END
065900970526     C     FUNKEY        IFEQ      '12'
066000970526     C                   EXSR      F12GES
066100970526     C                   END
066200970526     C     FUNKEY        IFEQ      '24'
066300970526     C                   EXSR      F24GES
066400970526     C                   END
066500970526     C                   END
066600970526     C*
066700970526     C                   ENDSR
066800970526     C*------------------------------------------------------------------------*
066900970526     C* FUNVAR -TRADUCE GLI INDICATORI DI TASTO FUNZIONALE IN VARIABILE
067000970526     C*------------------------------------------------------------------------*
067100970526     C     FUNVAR        BEGSR
067200970526     C*
067300970526     C* INIZIALIZZA VARIABILE -FUNKEY-
067400970526     C*
067500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
067600970526     C     KEY           IFEQ      ENTER
067700970526     C                   MOVEL     *BLANKS       FUNKEY
067800970526     C                   END
067900970526     C     KEY           IFEQ      F1
068000970526     C                   MOVEL     '01'          FUNKEY
068100970526     C                   END
068200970526     C     KEY           IFEQ      F2
068300970526     C                   MOVEL     '02'          FUNKEY
068400970526     C                   END
068500970526     C     KEY           IFEQ      F3
068600970526     C                   MOVEL     '03'          FUNKEY
068700970526     C                   END
068800970526     C     KEY           IFEQ      F4
068900970526     C                   MOVEL     '04'          FUNKEY
069000970526     C                   END
069100970526     C     KEY           IFEQ      F5
069200970526     C                   MOVEL     '05'          FUNKEY
069300970526     C                   END
069400970526     C     KEY           IFEQ      F6
069500970526     C                   MOVEL     '06'          FUNKEY
069600970526     C                   END
069700970526     C     KEY           IFEQ      F7
069800970526     C                   MOVEL     '07'          FUNKEY
069900970526     C                   END
070000970526     C     KEY           IFEQ      F8
070100970526     C                   MOVEL     '08'          FUNKEY
070200970526     C                   END
070300970526     C     KEY           IFEQ      F9
070400970526     C                   MOVEL     '09'          FUNKEY
070500970526     C                   END
070600970526     C     KEY           IFEQ      F10
070700970526     C                   MOVEL     '10'          FUNKEY
070800970526     C                   END
070900970526     C     KEY           IFEQ      F11
071000970526     C                   MOVEL     '11'          FUNKEY
071100970526     C                   END
071200970526     C     KEY           IFEQ      F12
071300970526     C                   MOVEL     '12'          FUNKEY
071400970526     C                   END
071500970526     C     KEY           IFEQ      F13
071600970526     C                   MOVEL     '13'          FUNKEY
071700970526     C                   END
071800970526     C     KEY           IFEQ      F14
071900970526     C                   MOVEL     '14'          FUNKEY
072000970526     C                   END
072100970526     C     KEY           IFEQ      F15
072200970526     C                   MOVEL     '15'          FUNKEY
072300970526     C                   END
072400970526     C     KEY           IFEQ      F16
072500970526     C                   MOVEL     '16'          FUNKEY
072600970526     C                   END
072700970526     C     KEY           IFEQ      F17
072800970526     C                   MOVEL     '17'          FUNKEY
072900970526     C                   END
073000970526     C     KEY           IFEQ      F18
073100970526     C                   MOVEL     '18'          FUNKEY
073200970526     C                   END
073300970526     C     KEY           IFEQ      F19
073400970526     C                   MOVEL     '19'          FUNKEY
073500970526     C                   END
073600970526     C     KEY           IFEQ      F20
073700970526     C                   MOVEL     '20'          FUNKEY
073800970526     C                   END
073900970526     C     KEY           IFEQ      F21
074000970526     C                   MOVEL     '21'          FUNKEY
074100970526     C                   END
074200970526     C     KEY           IFEQ      F22
074300970526     C                   MOVEL     '22'          FUNKEY
074400970526     C                   END
074500970526     C     KEY           IFEQ      F23
074600970526     C                   MOVEL     '23'          FUNKEY
074700970526     C                   END
074800970526     C     KEY           IFEQ      F24
074900970526     C                   MOVEL     '24'          FUNKEY
075000970526     C                   END
075100970526     C     KEY           IFEQ      ROLLUP
075200970526     C                   MOVEL     'RU'          FUNKEY
075300970526     C                   END
075400970526     C     KEY           IFEQ      HELP
075500970526     C                   MOVEL     'HP'          FUNKEY
075600970526     C                   END
075700970526     C                   ENDSR
075800970526     C*------------------------------------------------------------------------*
075900970526     C* NOTE COSTRUZIONE SCHIERE
076000970526     C*------------------------------------------------------------------------*
076100970526     C*
076200970526     C* -MSG-MESSAGGI
076300970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
076400970526     C* (NON TOCCARE MSG,1 E MSG,2)
076500970526     C*
076600970526     C* -CMD-COMANDI
076700970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
076800970526     C*
076900970526     C* -OPD-OPZIONI
077000970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
077100970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
077200970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
077300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
077400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
077500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
077600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
077700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
077800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
077900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
078000970526     C*
078100970526     C* -FUD-FUNZIONI
078200970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
078300970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
078400970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
078500970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
078600970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
078700970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
078800970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
078900970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
079000970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
079100970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
079200970526     O*------------------------------------------------------------------------*
079300060811     O*------------------------------------------------------------------------*
079400060811     O* STAMPA
079500060811     O*------------------------------------------------------------------------*
079600060811     OPRTEXCEL  E            STA_RIGA    1
079700060811     O                       RigaStampa
079800060811     O*------------------------------------------------------------------------*
079900970526** MSG - MESSAGGI
080000060811TRTCM8R- Tasto funzionale non gestito.                                         1
080100060811TRTCM8R- Scelta obbligatoria.                                                  2
080200060811TRTCM8R- ID elaborazinoe indicato inesistente.                                 3
080300060811TRTCM8R-                                                                       4
080400060811TRTCM8R-                                                                       5
080500060811TRTCM8R-                                                                       6
080600060811TRTCM8R-                                                                       7
080700060811TRTCM8R-                                                                       8
080800060811TRTCM8R-                                                                       9
080900060811TRTCM8R-                                                                       10
081000060811TRTCM8R-                                                                       11
081100060811TRTCM8R-                                                                       12
081200060811TRTCM8R-                                                                       13
081300060811TRTCM8R-                                                                       14
081400060811TRTCM8R-                                                                       15
081500060811TRTCM8R-                                                                       16
081600060811TRTCM8R-                                                                       17
081700060811TRTCM8R-                                                                       18
081800060811TRTCM8R-                                                                       19
081900060811TRTCM8R-                                                                       20
082000060811TRTCM8R-                                                                       21
082100060811TRTCM8R-                                                                       22
082200060811TRTCM8R-                                                                       23
082300060811TRTCM8R-                                                                       24
082400970526** FUD - FUNZIONI
082500970526                   -        (S/N)=N                        funz.01¬
082600970526                   -        (S/N)=N                        funz.02¬
082700970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
082800060811F04=Scelta elab.   -        (S/N)=S  <== S-FISSO           funz.04¬
082900970526                   -        (S/N)=N                        funz.05¬
083000060811F06=Conferma       -        (S/N)=S  <== S-FISSO           funz.06¬
083100970527                   -        (S/N)=N                        funz.07¬
083200970527                   -        (S/N)=N                        funz.08¬
083300970527                   -        (S/N)=N                        funz.09¬
083400970527                   -        (S/N)=N                        funz.10¬
083500970526                   -        (S/N)=N                        funz.11¬
083600970526F12=Ritorno        -        (S/N)=S  <= S-FISSO            funz.12¬
083700970526                   -        (S/N)=N                        funz.13¬
083800970526                   -        (S/N)=N                        funz.14¬
083900970526                   -        (S/N)=N                        funz.15¬
084000970526                   -        (S/N)=N                        funz.16¬
084100970526                   -        (S/N)=N                        funz.17¬
084200970526                   -        (S/N)=N                        funz.18¬
084300970526                   -        (S/N)=N                        funz.19¬
084400970526                   -        (S/N)=N                        funz.20¬
084500970526                   -        (S/N)=N                        funz.21¬
084600970526                   -        (S/N)=N                        funz.22¬
084700970526                   -        (S/N)=N                        funz.23¬
084800060811F24=Altri tasti    -        (S/N)=N                        funz.24¬
