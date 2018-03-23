000100940519     H DATEDIT(*YMD)
000200930915     F* TRTB44R *-----------------------------------------------------*
000300930915     F*       GESTIONE TABELLA 4C
000400930913     F*---------------------------------------------------------------*
000500110811      *?  ATTENZIONE!!  ?
000600110811      *?    Questa tabella è utilizzata anche dal pgm TNTA61R  ?
000700110811      *?    'Interrogazione abilitazioni clienti'              ?
000800110811      *?    In caso di aggiunta/modifica campi alla tabella    ?
000900110811      *?    verificare anche relativo pgm di interrogazione    ?
001000110811      *?    per le abilitazioni clienti --> TRTB441R/TRTB442R. ?
001100110811
001200930915     FTRTB44D   CF   E             WORKSTN
001300891030     FTABEL00F  UF A E           K DISK
001400981130     FAZORG01L  IF   E           K DISK
001500130521
001600930917     D*DS CHE CONTIENE I TRE FLAG DI SCARICO A VIDEO
001700930917     D DSFLG           DS
001800930917     D  VI2FSC                 1      1
001900930917     D  VI2FAN                 2      2
002000930917     D  VI2FLG                 3      3
002100130521      *
002200971210     D TRUL12        E DS                  EXTNAME(TRUL12DS)
002300130521      *
002400981130     D DSBS69        E DS                  EXTNAME(TIBS69DS)
002500981130     D DSACO         E DS                  EXTNAME(CNACO00F)
002600981130     D DSIND         E DS                  EXTNAME(CNIND00F)
002700981130     D DSCLP         E DS                  EXTNAME(CNCLP00F)
002800981130     D DSCLS         E DS                  EXTNAME(FNCLS00F)
002900130521      *
003000930915     D DS4C          E DS
003100130521      *
003200891030     D KPJBA         E DS
003300130521      *
003400130521      * -?Parametri x Controllo profilo utenti?
003500130521     d TIBS34ds      e ds
003600130521      * -?Ds di riferimento al file esterno AZUTE00F?
003700130521     d AZUTEds       e ds                  extname(AZUTE00F)
003800130521      * -?Ds per dati organigramma?
003900130521     d dDATIUTE      e ds
004000130521      *
004100130521      * -?Parametri per pgm ricerca data blocco cliente?
004200130521     d TRULBLCds     e ds                  inz
004300130521
004400130521     d wDate_DMY       s               d   datfmt(*dmy)  inz
004500130521     d wDate6          s              6  0 inz
004600130521
004700930913     F*--------------------------------------------------------------*
004800930917     F* TABELLA GESTIONE SCAMBIO DATI CONTRASSEGNI E FATTURA *
004900930913     F*----------------------* USO DEGLI INDICATORI *----------------*
005000930913     F*  80        INSERIMENTO                                       *
005100930913     F*  03        ANNULLAMENTO                                      *
005200930913     F*  05        PROTEGGO CAMPI 2 VIDEATA                          *
005300930913     F*  10        CODICE BLOCCATO O ANNULLATO IN VARIAZION: PROTETTO*
005400930913     F*  30        COMODO
005500930917     F*  37        DECODIFICA CLIENTE CONTRASSEGNO E CLIENTE FATTURA
005600930913     F*  40/51     ERRORI
005700930920     F********************
005800930917     F*  ERRORI 1°VID
005900930917     F*  40        CLIENTE ERRATO
006000930917     F*  41        CLIENTE ERRATO
006100930920     F********************
006200930917     F*  ERRORI 2°VID
006300930920     F***DATI CONTRASSEGNO
006400930917     F*  43        GG.CADENZA INSERIB.ED OBBLIG SOLO SE ALMENO UNO DEI 3 FLG='S'
006500930917     F*  44        TP.SCAR.DT.INSERIB.E OBBLIG. SOLO SE ALMENO UNO DEI 3 FLG='S'
006600930917     F*  45        TP.SCAR.DATI INESISTENTE
006700930917     F*  47        TP.SCAR.DATI ANNULLATO
006800930917     F*  50        CLI.A CUI SCAR,INSERIB.E OBBLIG.SE ALMENO 1 FLAG DI SCARICO=S
006900930920     F*  51        CLI.A CUI SCAR,INESISTENTE
007000930920     F*  52        CLI.A CUI SCAR,ANNULLATO
007100930920     F***DATI FATTURA
007200930920     F*  53  NOTE INSERIBILI SOLO SE INSER.ALMENO UN ALTRO DATO FATTURA
007300930920     F*  54  CADENZA INSERIB.SOLO SE INSER.ALMENO UN ALTRO DATO FATTURA
007400930920     F*  55  TP.SC.DT:OBBLIG. SOLO SE INSER.ALMENO UN ALTRO DATO FATTURA
007500930920     F*  57  CLIENTE :OBBLIGATORIO SOLO SE INSER.ALMENO UN ALTRO DATO FATTURA
007600050125      *  58  errore tra RFO e MBX
007700050125      *  59  membro obbligatorio
007800930920     F*  60  TP.SCAR.DATI INESISTENTE
007900930920     F*  61  TP.SCAR.DATI ANNULLATO
008000930920     F*  62  CLI.A CUI SCAR,INESISTENTE
008100930920     F*  63  CLI.A CUI SCAR,ANNULLATO
008200930920     F*  65  NON E'POSSIB.CMD6, SE NON INSER.ALMENO UN DATO C/A O FATT.
008300930920     F********************
008400930920     F*  79        UTENTE EDP
008500930913     F*  80        IMMISSIONE VARIAIZONE CODICE
008600930920     F********************
008700930913     F*  90        IMDICATORE GENERICO DI ERRORE
008800930914     C*  98        SE CHIAMATO CON CLIENTE VALORIZZATO
008900930914     C*  99        SE CHIAMATO (DISABILITA I CMD USATI PER CALL)
009000930913     F*--------------------------------------------------------------*
009100891030     C     *ENTRY        PLIST
009200891030     C                   PARM                    KPJBA
009300971210     C******************************************************
009400971210     C* Richiamo pgm per controllo modificabilità tabella
009500971210     C                   CLEAR                   TRUL12
009600971210     C                   MOVEL     'L'           I12TLA
009700971210     C                   MOVEL     KNSIF         I12NSI
009800971210     C                   MOVEL     '4C'          I12COD
009900971210     C                   CALL      'TRUL12R'
010000971210     C                   PARM                    TRUL12
010100971210     C     O12ERR        IFEQ      '1'
010200971210     C                   GOTO      FINE
010300971210     C                   END
010400971210     C******************************************************
010500930914     C*
010600930914     C* TESTO LE PRIME SETTE POSIZIONI DELLA KPJBA PER SAPERE SE IL PROGRAMMA
010700930914     C* E' STATO RICHIAMATO O SE SONO ENTRATO DA MENU' TABELLE, PER SAPERE
010800930914     C* SE ABILITARE O MENO I TASTI COMANDO 7-14-21, CHE NON VANNO ABILITATI
010900930914     C* SE E' STATO RICHIAMATO.
011000930914     C*
011100981130     C                   CLEAR                   VIDFIL
011200930914     C                   MOVEL     KPJBU         $TEST             7
011300930914     C*SE E'STATO RICHIAMATO,$TEST E' DIVERSO DA BLANK E NON DEVONO APPARIRE CMD
011400930914     C     $TEST         IFNE      *BLANK
011500930914     C                   SETON                                        99
011600930914     C*SE E'STATO RICHIAMATO,CON UN CODICE CLIENTE VALORIZZATO(MAGGIORE DI 0)
011700930914     C*MUOVO $TEST NEL CAMPO CODICE CLIENTE A VIDEO,E ACCENDO 98
011800930914     C                   TESTN                   $TEST                98
011900930914     C     *IN98         IFEQ      *ON
012000930914     C     $TEST         ANDGT     *ALL'0'
012100930914     C                   SETON                                        98
012200930914     C                   MOVEL     $TEST         VIDCLI
012300930914     C                   ELSE
012400930914     C                   SETOFF                                       98
012500930914     C                   ENDIF
012600930914     C                   ENDIF
012700930916     C* IL CAMPO CODUT DEVE ESSERE SEMPRE A 1.
012800130521     C                   Z-ADD     1             CODUT             1 0
012900130521     C* REPERIMENTO DATI UTENTE
013000130521     c     *dtaara       define    §azute        AZUTEds
013100130521     c     *dtaara       define    §datiute      dDATIUTE
013200130521     c                   in(E)     *dtaara
013300130521     c                   if        %ERROR or RSUT = *blanks
013400130521     c                   clear                   Tibs34Ds
013500130521     c                   call      'TIBS34R'
013600130521     c                   parm                    Tibs34Ds
013700130521     c                   in        *dtaara
013800130521     c                   endif
013900910315     C*
014000891030     C******************************************************
014100891030     C* POSIZIONAMENTO TABELLE                             *
014200891115     C     KTAB          KLIST
014300930916     C                   KFLD                    CODUT
014400930916     C                   KFLD                    COD
014500930916     C                   KFLD                    KEY
014600910315     C******************************************************
014700930913     C     *LIKE         DEFINE    TBLKUT        KUT
014800930913     C     *LIKE         DEFINE    TBLKEY        KEY
014900930913     C     *LIKE         DEFINE    TBLCOD        COD
015000930916     C*
015100930916     C* CONTROLLO CHE UTENTE PROGRAMMA SIA EDP.
015200930920     C* SE L'UTENTE NON E' EDP, NON ABILITA CMD16:ANNULL E CMD5:RIPRISTINO
015300891102     C                   MOVEL     KNMUS         PROF              3
015400891102     C     PROF          IFNE      'EDP'
015500891102     C                   SETON                                        79
015600891102     C                   END
015700921103     C*
015800930920     C* CICLO DI EMISSIONE 1° VIDEO
015900930913     C     START         TAG
016000930915     C  N98              MOVEL     *BLANKS       VIDCLI
016100930915     C  N98              MOVEL     '?'           VIDCLI
016200921103     C*
016300930913     C     FOR01         TAG
016400930914     C                   SETOFF                                       100503
016500930914     C*NON DEVE EMETTERE TESTATA E 1°VIDEO SE E'STATO CHIAMATO ED HA RICEVUTO UN
016600930914     C*CODICE CLIENTE BUONO. (PER N98)
016700930921     C  N98              EXFMT     TB44D1
016800930913     C* CMD3  - FINE LAVORO
016900910318     C   KC              GOTO      FINE
017000931124      *
017200931124      * CMD22 - GESTIONE SCAMBIO DATI CONS.GIAC.DISP.CON CLIENTE
017300931124      * O CMD21 - GESTIONE DATI RICEZ BOLLE DA CLIENTI
017500140424     C   KS
017600000731     COR KT
017700931124     COR KV
017800931124     COR KW              DO
017900930914     C                   EXSR      KOMAND
018000930914     C*PRIMA DI FARE I CONTROLLI, DEVO RIEMETTERE LA VIDEATA SE EFFETTUATA CALL
018100930914     C                   GOTO      FOR01
018200930914     C                   ENDDO
018300930913     C* CONTROLLI FORMATO 1
018400930913     C                   EXSR      CONTR1
018500930914     C   90              SETOFF                                       98
018600930914     C   90              GOTO      FOR01
018700921103     C*
018800921103     C* CARICO TABELLA A VIDEO
018900921103     C                   EXSR      CARVID
019000921103     C*
019100930920     C* CICLO DI EMISSIONE 2° VIDEO
019200930913     C     FOR02         TAG
019300930921     C                   EXFMT     TB44D2
019400921103     C* CMD3 - FINE LAVORO
019500910226     C   KC              GOTO      FINE
019600910515     C*
019700930917     C* CMD12 - RITORNO (SOLO SE N98,CIOE' SE E' STATO CHIAMATO,ED HA RICEVUTO
019800930917     C*         UN CODICE CLIENTE BUONO).
019900930920     C   KL
020000930920     CANN98              GOTO      FOR01
020100921103     C* CMD16 - ANNULLAMENTO
020200930913     C     *INKQ         IFEQ      *ON
020300930913     C                   EXSR      ANNULL
020400930920     C* TORNA ALL'EMISSIONE DEL 1° VIDEO, CLIENTE CORRETTO)
020500930920     C* SOLO SE N98 (RICHIAMATO E RICEVUTO CLIENTE CORRETTO)
020600930914     C  N98              GOTO      START
020700930917     C   98              GOTO      FINE
020800930913     C                   ENDIF
020900921103     C* CMD5 - RIPRISTINO
021000930920     C     *INKE         IFEQ      *ON
021100930913     C                   EXSR      RIPRIS
021200930914     C* SOLO SE N98 (RICHIAMATO E RICEVUTO CLIENTE MAGGIORE DI 0 E NON ERRATO)
021300930914     C  N98              GOTO      FOR01
021400930917     C   98              GOTO      FINE
021500930913     C                   ENDIF
021600931124      *
021700931124      * SE SELEZIONATO CMD21- GESTIONE DATI PER RICEZIONE BOLLE DA CLIENTI
021800931124      * O SELEZIONATO  CMD22 -GESTIONE SCAMBIO DATI C/A E FATTURA CON CLIENTI
022100140424     C   KS
022200000803     COR KT
022300931124     COR KV
022400931124     COR KW              DO
022500930914     C                   EXSR      KOMAND
022600930914     C*PRIMA DI FARE I CONTROLLI, DEVO RIEMETTERE LA VIDEATA SE EFFETTUATA CALL
022700930917     C*(PER NON USARE IL RESTORE DISPLAY).
022800930917     C                   GOTO      FOR02
022900930914     C                   ENDDO
023000921103     C* CONTROLLI FORMATO 2
023100921103     C  N05              EXSR      CONTR2
023200930920     C* SE ERRORE O NON SPINTO CMD CHE VIENE DOPO (CMD6), TORNA ALL'INIZIO 1°VID
023300921103     C  NKF
023400930913     COR 90              GOTO      FOR02
023500921103     C*
023600921103     C*  CMD6 - AGGIORNAMENTO
023700930917     C   KF              DO
023800930920     C                   EXSR      SCRIVI
023900930914     C* SOLO SE N98 (RICHIAMATO E RICEVUTO CLIENTE MAGGIORE DI 0 E NON ERRATO)
024000930920     C  N98              GOTO      START
024100930917     C   98              GOTO      FINE
024200930917     C                   ENDDO
024300910514     C*
024400930913     C     FINE          TAG
024500981130     C                   CLEAR                   DSBS69
024600981130     C                   MOVEL     'C'           I69TLA
024700981130     C                   CALL      'TIBS69R'
024800981130     C                   PARM                    DSBS69
024900981130     C                   PARM                    DSACO
025000981130     C                   PARM                    DSIND
025100981130     C                   PARM                    DSCLP
025200981130     C                   PARM                    DSCLS
025300930915     C                   MOVE      *BLANKS       KPJBU
025400910514     C                   SETON                                        LR
025500930913     C*
025600930920     C**------------------------------------------
025700930920     C* SUBROUTINE DI CONTROLLO FORMATO 1
025800930920     C**------------------------------------------
025900930913     C     CONTR1        BEGSR
026000930916     C* SPEGNIMENTO INDICATORE GENERICO DI ERRORE
026100930913     C                   SETOFF                                       90
026200981130     C     VIDFIL        IFGT      0
026300981130     C     VIDFIL        CHAIN     AZORG01L                           67
026400981130     C   67              SETON                                        90
026500981130     C   67              GOTO      ENDCT1
026600981130     C                   ENDIF
026700981130     C**
026800930916     C*§  RICERCA CLIENTE IN TABEL00F
026900930913     C     '?'           SCAN      VIDCLI                                 90
027000930913     C*
027100930913     C     *IN90         IFEQ      *ON
027200930913     C                   MOVEL     CODUT         KUT
027300930915     C                   MOVEL     '4C'          COD
027400930913     C                   MOVE      *BLANK        KEY
027500930921     C                   MOVE      *BLANK        §DES
027600981130     C                   MOVEL     VIDFIL        KEY
027700930913     C                   CALL      'X§TABER'
027800930916     C                   PARM                    KUT
027900930916     C                   PARM                    COD
028000930916     C                   PARM                    KEY
028100930913     C                   PARM                    §DES             30
028200981130     C     KEY           IFNE      *BLANKS
028300930913     C                   MOVEL     KEY           VIDCLI
028400981130     C                   ELSE
028500981130     C                   MOVEL     '?'           VIDCLI
028600981130     C                   ENDIF
028700930913     C                   GOTO      ENDCT1
028800930913     C                   ENDIF
028900930916     C*
029000930916     C* RICERCA ALFABETICA CLIENTE
029100930913     C     RICCLI        IFNE      *BLANKS
029200930913     C     VIDCLI        IFEQ      *BLANKS
029300930913     C     VIDCLI        OREQ      *ZEROS
029400000710     C                   MOVEL     RSUT          PAXDUT           30
029500000710     C                   MOVEL     *BLANKS       PAXDMT
029600000710     C                   MOVEL     RICCLI        PAXDMT
029700001003     C                   Z-ADD     9             PAXSTA
029800130521     C                   Z-ADD     dutKCI        PAXCCC            4 0
029900000710     C                   Z-ADD     1             PAXNUM            2 0
030000000710     C                   CLEAR                   PAXDIT
030100930913     C*
030200000710     C                   CALL      'XALFA3BR'
030300000710     C                   PARM                    PAXDUT
030400130521     C                   PARM                    CODUT
030500000710     C                   PARM                    PAXDMT           48
030600000710     C                   PARM                    PAXCCC
030700000710     C                   PARM                    PAXSTA            1 0
030800000710     C                   PARM                    PAXFLR           90
030900000710     C                   PARM                    PAXDIT            3
031000000710     C                   PARM                    PAXNUM            2 0
031100000710     C                   PARM                    PAXKCM           80
031200000710     C                   PARM                    PAXKSM          140
031300000710     C                   PARM                    PAXKDM           60
031400000710     C* (SE PAXSTA = -1, SIGNIFICA CHE NON E' STATO SELEZIONATO NIENTE)
031500000710     C     PAXSTA        IFNE      -1
031600000710     C                   MOVEL     PAXKSM        VIDCLI
031700930921     C                   ENDIF
031800930913     C                   SETON                                          90
031900930913     C                   GOTO      ENDCT1
032000930913     C                   ENDIF
032100930913     C                   ENDIF
032200930913     C*
032300930913     C* CONTROLLO CODICE CLIENTE
032400930916     C*(SE A QUESTO PUNTO IL CODICE CLIENTE NON C'E' ANCORA --> ERRORE).
032500930913     C     VIDCLI        IFEQ      *BLANKS
032600930913     C     VIDCLI        OREQ      *ZEROS
032700930913     C                   SETON                                        4090
032800930913     C                   GOTO      ENDCT1
032900930913     C                   ENDIF
033000930916     C*
033100930916     C* SE IMMESSO, CONTROLLO ESISTENZA CLIENTE IN ANAGRAFICA.
033200981130     C                   CLEAR                   DSBS69
033300130521     C                   MOVEL     dutKCI        I69KCC
033400981130     C                   MOVEL     VIDCLI        I69KAC
033500981130     C                   MOVEL     KNSIF         I69SIF
033600981130     C                   CALL      'TIBS69R'
033700981130     C                   PARM                    DSBS69
033800981130     C                   PARM                    DSACO
033900981130     C                   PARM                    DSIND
034000981130     C                   PARM                    DSCLP
034100981130     C                   PARM                    DSCLS
034200981130     C* ERRORE
034300981130     C     O69ERR        IFNE      ' '
034400981130     C                   SETON                                        9040
034500930913     C                   GOTO      ENDCT1
034600930913     C                   ENDIF
034700930916     C*
034800930916     C* SE CLIENTE IMMESSO E TROVATO IN ANAGRAFICA, CONTROLLO ESISTENZA IN TABEL
034900930913     C                   MOVEL     CODUT         KUT
035000930915     C                   MOVEL     '4C'          COD
035100930913     C                   MOVE      *BLANKS       KEY
035200930913     C                   MOVEL     VIDCLI        KEY
035300930913     C*
035400930913     C     KTAB          CHAIN     TABEL                              80
035500930916     C* ACOFLG E' IL CAMPO DI CNACO (ANAGR.CLI.) CHE MI DICE SE OK/ANNULL/BLOCC.
035600930916     C***
035700930916     C* (SE ACOFLG <> BLANK, IL RECORD E' ANNULLATO O BLOCCATO IN ANAGRAFICA).
035800930913     C     ACOFLG        IFNE      ' '
035900930916     C*SE NON TROVATO IN TABELLA
036000930920     C* NON ESISTE IN TABEL(80),QUINDI DOVREI DARE 2°VID PER IMMISSIONE,MA SIC-
036100930920     C* COME E' ANNULLATO IN ANAGRAFICA, DO ERRORE AL 1°VIDEO.
036200930916     C*NON PRESENTO 2° VIDEO E DO ERRORE SUL 1°.(CLIENTE ANNULL O BLOCC)
036300930913     C   80              SETON                                        4190
036400930916     C*SE TROVATO IN TABELLA
036500930920     C*SICCOME E' ANNULLATO IN ANAGRAFICA, ED ESISTE IN TABELLA, DEVO PROPORRE
036600930920     C*LA 2° VIDEATA PER L'ANNULLAMENTO IN TABELLA.
036700930916     C*PRESENTO 2°VIDEO CON LA SCRITTA COD.ANNULL.ED ABILITO SOLO CMD PER ANNULL
036800130521     ***C  N80              MOVEL     'CODICE A'    VI2AOB
036900130521     ***C  N80              MOVE      'NNULLATO'    VI2AOB
037000130521     c  n80              eval      VI2aob = 'CODICE ANNULLATO'
037100930916     C*10= DISABILITA CMD5 RIPRISTINO;VISUALIZZA VI2AOB;
037200930916     C*05= DISABILITA CMD6 AGGIORN;PROTECT CAMPI 2°VIDEO;
037300930913     C  N80              SETON                                        1005
037400930913     C                   END
037500930916     C***
037600930916     C* CLIENTE BLOCCATO IN ANAGRAFICA
037700130315     C  N41ACOABL        IFne      *blanks
037800930916     C*SE NON TROVATO IN TABELLA
037900930920     C* NON ESISTE IN TABEL(80),QUINDI DOVREI DARE 2°VID PER IMMISSIONE,MA SIC-
038000930920     C* COME E' BLOCCATO IN ANAGRAFICA, DO ERRORE AL 1°VIDEO.
038100930913     C   80              SETON                                        4190
038200930916     C*SE TROVATO IN TABELLA
038300930920     C*SICCOME E' BLOCCATO IN ANAGRAFICA, ED ESISTE IN TABELLA, DEVO PROPORRE
038400930920     C*LA 2° VIDEATA PER L'ANNULLAMENTO IN TABELLA.
038500130521     ***C  N80              MOVEL     'CODICE  '    VI2AOB
038600130521     ***C  N80              MOVE      'BLOCCATO'    VI2AOB
038700130521     c  n80              eval      VI2aob = 'CODICE BLOCCATO'
038800130521     c                   if        Not *in80
038900130521     c                   clear                   TRULBLCds
039000130521     c                   eval      iBLCkcc = DUTkci
039100130521     c                   eval      iBLCksc = %int(VIDcli)
039200130521     c                   call      'TRULBLCR'
039300130521     c                   parm                    kpjba
039400130521     c                   parm                    trulBLCds
039500130521     c                   if        oBLCdav > 00010101
039600130521     c                   eval      wDate_Dmy = %date( oBLCdav : *iso )
039700130521     c                   eval      wDate6 = %dec( wDate_Dmy )
039800130521     c                   eval      VI2aob = 'BLOCCATO IL ' +
039900130521     c                                       %editw ( wDate6 : '  /  /  ')
040000130521     c                   endif
040100130521     c                   endif
040200940519SC   C  N80              SETON                                        10
040300930913     C                   END
040400930913     C   41              GOTO      ENDCT1
040500930913     C*
040600930913     C     ENDCT1        ENDSR
040700930913     C*
040800930920     C**----------------------------------------------------
040900930920     C* SUBROUTINE DI CARICAMENTO O PULIZIA CAMPI 2° VIDEATA
041000930920     C**----------------------------------------------------
041100921103     C     CARVID        BEGSR
041200950927     C                   CLEAR                   VI2RIC
041300950927     C                   CLEAR                   VI2RI2
041400930913     C*
041500930916     C* RECORD TABELLA ANNULLATO:03 ABILITO RIPRISTINO;DISABILITO ANNULL;
041600930916     C* 05 DISABILITO AGGIORNAMENTO;PROTEZIONE CAMPI 2°VIDEO;
041700930913     C  N80TBLFLG        IFEQ      '*'
041800930913     C                   SETON                                        0305
041900930913     C                   END
042000930916     C*VALORIZZO IL CAMPO COD. CLI DA1°VID E DESCR  DA ANAGRAFICO
042100930916     C                   MOVEL     VIDCLI        VI2COD
042200930916     C                   MOVEL     ACORAG        VI2RAG
042300930916     C*
042400930916     C* VARIAZIONE (N80=TROVATO IN TABELLA, QUINDI VARIAZIONE)
042500930913    1C     *IN80         IFEQ      *OFF
042600930916     C*TBLUNI E' IL MAXI CAMPO DI TABEL CON CUI SI VALORIZZA LA DS
042700930915     C                   MOVEL     TBLUNI        DS4C
042800930917     C* VALORIZZO I DATI CONTRASSEGNO
042900930916     C* ROVESCIAMENTO DATA DA PORTARE A VIDEO
043000930921     C                   Z-ADD     §4CFDU        $DATA
043100930916     C                   EXSR      ROVDA
043200930921     C                   Z-ADD     $DATA         VI2FDU
043300930916     C*
043400930921     C                   MOVEL     §4CFSC        VI2FSC
043500930916     C                   MOVEL     §4CFAN        VI2FAN
043600930916     C                   MOVEL     §4CFLG        VI2FLG
043700930916     C                   MOVEL     §4CDTS        VI2DTS
043800930916     C                   Z-ADD     §4CCGC        VI2CGC
043900050125     c                   Eval      vi2rfo = §4crfo
044000930916     C                   MOVEL     §4CTSD        VI2TSD
044100930917     C*CLIENTE A CUI SCARICARE
044200930916     C                   Z-ADD     §4CCKS        VI2CKS
044300930917IF  1C     §4CCKS        IFNE      *ZEROS
044400981130     C                   CLEAR                   DSBS69
044500130521     C                   MOVEL     dutKCI        I69KCC
044600981130     C                   MOVEL     §4CCKS        I69KAC
044700981130     C                   MOVEL     KNSIF         I69SIF
044800981130     C                   CALL      'TIBS69R'
044900981130     C                   PARM                    DSBS69
045000981130     C                   PARM                    DSACO
045100981130     C                   PARM                    DSIND
045200981130     C                   PARM                    DSCLP
045300981130     C                   PARM                    DSCLS
045400981130     C     O69ERR        IFEQ      ' '
045500981130     C                   MOVEL     ACORAG        VI2RIC
045600981130     C                   ELSE
045700981130     C                   MOVEL     *BLANKS       VI2RIC
045800981130     C                   ENDIF
045900930917E   1C                   ENDIF
046000930917     C*
046100930917     C* VALORIZZO I DATI FATTURA
046200930917     C                   MOVEL     §4CFNT        VI2FNT
046300930917     C                   Z-ADD     §4CFGC        VI2FGC
046400930917     C                   MOVEL     §4CFSD        VI2FSD
046500050125     c                   Eval      vi2mbx = §4cmbx
046600930917     C*CLIENTE A CUI SCARICARE
046700930917     C                   Z-ADD     §4CFKS        VI2FKS
046800930917IF  1C     §4CFKS        IFNE      *ZEROS
046900981130     C                   CLEAR                   DSBS69
047000130521     C                   MOVEL     dutKCI        I69KCC
047100981130     C                   MOVEL     §4CFKS        I69KAC
047200981130     C                   MOVEL     KNSIF         I69SIF
047300981130     C                   CALL      'TIBS69R'
047400981130     C                   PARM                    DSBS69
047500981130     C                   PARM                    DSACO
047600981130     C                   PARM                    DSIND
047700981130     C                   PARM                    DSCLP
047800981130     C                   PARM                    DSCLS
047900981130     C     O69ERR        IFEQ      ' '
048000981130     C                   MOVEL     ACORAG        VI2RI2
048100981130     C                   ELSE
048200981130     C                   MOVEL     *BLANKS       VI2RI2
048300981130     C                   ENDIF
048400930917E   1C                   ENDIF
048500921103     C*
048600930913   X1C                   ELSE
048700930913     C* IMMISSIONE
048800930917     C*DATI CONTRASSEGNO
048900930917     C                   MOVE      *BLANKS       DSFLG
049000930917     C                   MOVE      *BLANKS       VI2DTS
049100930917     C                   Z-ADD     0             VI2CGC
049200050125     c                   Clear                   vi2rfo
049300930917     C                   MOVEL     *BLANKS       VI2TSD
049400930917     C                   Z-ADD     0             VI2CKS
049500930917     C                   MOVEL     *BLANKS       VI2RIC
049600930917     C*DATI FATTURA
049700930917     C                   MOVEL     *BLANKS       VI2FNT
049800930917     C                   Z-ADD     0             VI2FGC
049900930917     C                   MOVEL     *BLANKS       VI2FSD
050000050125     c                   Clear                   vi2mbx
050100930917     C                   Z-ADD     0             VI2FKS
050200930917     C                   MOVEL     *BLANKS       VI2RI2
050300930913   E1C                   ENDIF
050400891030     C                   ENDSR
050500930913     C*
050600930920     C**----------------------------------------------------
050700930920     C* SUBROUTINE DI ANNULLAMENTO RECORD TABELLA
050800930920     C**----------------------------------------------------
050900930913     C     ANNULL        BEGSR
051000930915     C                   MOVEL     '4C'          COD
051100930913     C                   MOVEL     *BLANKS       KEY
051200930913     C                   MOVEL     VIDCLI        KEY
051300930920     C     KTAB          CHAIN     TABEL                              80
051400930913     C                   MOVEL     '*'           TBLFLG
051500930923     C                   MOVE      *BLANK        TBLFTR
051600930923     C                   MOVE      *ZERO         TBLDTR
051700930913     C  N80              UPDATE    TABEL
051800941117     C                   ENDSR
051900941117     C*
052000930920     C**----------------------------------------------------
052100930920     C* SUBROUTINE DI RIPRISTINO RECORD ANNULLATO
052200930920     C**----------------------------------------------------
052300930913     C     RIPRIS        BEGSR
052400930915     C                   MOVEL     '4C'          COD
052500930913     C                   MOVEL     *BLANKS       KEY
052600930913     C                   MOVEL     VIDCLI        KEY
052700930913     C     KTAB          CHAIN     TABEL                              80
052800930913     C                   MOVEL     *BLANKS       TBLFLG
052900930923     C                   MOVE      *BLANK        TBLFTR
053000930923     C                   MOVE      *ZERO         TBLDTR
053100930913     C  N80              UPDATE    TABEL
053200930913     C                   ENDSR
053300930913     C*
053400930920     C**----------------------------------------------------
053500930920     C* SUBROUTINE DI CONTROLLI 2° VIDEATA
053600930920     C**----------------------------------------------------
053700910514     C     CONTR2        BEGSR
053800930917     C*SPEGNIMENTO INDICATORE GENERICO DI ERRORE
053900921103     C                   SETOFF                                       90
054000930920     C**** *** *** *** *** *** *** ***
054100930920     C*DATI CONTRASSEGNO
054200930920     C**** *** *** *** *** *** *** ***
054300921103     C*
054400930917     C*CONTR.CADENZA: INSERIB.E OBBLIG. SOLO SE ALMENO 1 DEI 3 FLG SCARICO ='S'.
054500091215     C     VI2CGC        ifeq      0
054600930917     C     DSFLG         ANDNE     *BLANKS
054700930917     C                   SETON                                        4390
054800930917     C                   GOTO      ENDCT2
054900930917     C                   ENDIF
055000930917     C*CONTROLLO
055100930917     C*TP SCAR.DATI: INSERIB.E OBBLIG. SOLO SE ALMENO 1 DEI 3 FLG SCARICO ='S'.
055200091215     C     VI2TSD        ifeq      *BLANKS
055300930917     C     DSFLG         ANDNE     *BLANKS
055400930917     C                   SETON                                        4490
055500930917     C                   GOTO      ENDCT2
055600930917     C                   ENDIF
055700930917     C*
055800930917     C*RICERCA TIPO SARICO DATI
055900930917IF  2C     VI2TSD        IFEQ      ' ?'
056000930917     C     VI2TSD        OREQ      '? '
056100930917     C                   MOVEL     CODUT         KUT
056200930917     C                   MOVEL     '3T'          COD
056300930917     C                   MOVEL     *BLANKS       KEY
056400930917     C                   MOVEL     *BLANKS       §DES
056500930917     C                   CALL      'X§TABER'
056600930917     C                   PARM                    KUT
056700930917     C                   PARM                    COD
056800930917     C                   PARM                    KEY
056900930917     C                   PARM                    §DES
057000930917     C* METTO CODICE TP SCARIC DATI SELEZIONATO IN CAMPO VIDEO ED EMETTO.
057100930917     C                   MOVEL     KEY           VI2TSD
057200930917     C                   SETON                                        90
057300930917     C                   GOTO      ENDCT2
057400930917     C                   ENDIF
057500930917     C*
057600930917     C*CONTROLLO TIPO SCARICO DATI
057700930921IF  2C     VI2TSD        IFNE      *BLANKS
057800930917     C                   MOVEL     '3T'          COD
057900930917     C                   MOVEL     *BLANKS       KEY
058000930917     C                   MOVEL     VI2TSD        KEY
058100930917     C     KTAB          CHAIN     TABEL00F                           45
058200930917     C* TIPO SCARICO DATI INESISTENTE
058300930917IF  2C     *IN45         IFEQ      '1'
058400930920     C                   SETON                                        90
058500930917     C                   GOTO      ENDCT2
058600930917X   2C                   ELSE
058700930917     C* TIPO SCARICO DATI ANNULLATO
058800930917IF  3C     TBLFLG        IFNE      *BLANKS
058900930917     C                   SETON                                        4790
059000930917     C                   GOTO      ENDCT2
059100930917E   3C                   ENDIF
059200930917E   2C                   ENDIF
059300930921     C                   ENDIF
059400930917     C*
059500930917     C* RICERCA ALFABETICA CLIENTE
059600930917     C     VI2RIC        IFNE      *BLANKS
059700930917     C     VI2CKS        ANDEQ     0
059800000710     C                   MOVEL     RSUT          PAXDUT           30
059900000710     C                   MOVEL     *BLANKS       PAXDMT
060000000710     C                   MOVEL     VI2RIC        PAXDMT
060100000710     C                   Z-ADD     0             PAXSTA
060200130521     C                   Z-ADD     dutKCI        PAXCCC
060300000710     C                   Z-ADD     1             PAXNUM
060400000710     C                   CLEAR                   PAXDIT
060500921103     C*
060600000710     C                   CALL      'XALFA3BR'
060700000710     C                   PARM                    PAXDUT
060800130521     C                   PARM                    CODUT
060900000710     C                   PARM                    PAXDMT           48
061000000710     C                   PARM                    PAXCCC
061100000710     C                   PARM                    PAXSTA            1 0
061200000710     C                   PARM                    PAXFLR
061300000710     C                   PARM                    PAXDIT
061400000710     C                   PARM                    PAXNUM            2 0
061500000710     C                   PARM                    PAXKCM           80
061600000710     C                   PARM                    PAXKSM          140
061700000710     C                   PARM                    PAXKDM           60
061800930920     C*
061900000710     C*PAXSTA NON =(-1),SE HO SELEZIONATO UN CODICE IN RICERCA.
062000000710     C     PAXSTA        IFNE      -1
062100000710     C                   MOVEL     PAXKSM        VI2CKS
062200981130     C                   CLEAR                   DSBS69
062300130521     C                   MOVEL     dutKCI        I69KCC
062400000710     C                   MOVEL     PAXKSM        I69KAC
062500981130     C                   MOVEL     KNSIF         I69SIF
062600981130     C                   CALL      'TIBS69R'
062700981130     C                   PARM                    DSBS69
062800981130     C                   PARM                    DSACO
062900981130     C                   PARM                    DSIND
063000981130     C                   PARM                    DSCLP
063100981130     C                   PARM                    DSCLS
063200981130     C*
063300981130     C     O69ERR        IFEQ      ' '
063400981130     C                   MOVEL     ACORAG        VI2RIC
063500981130     C                   ELSE
063600981130     C                   MOVEL     *BLANKS       VI2RIC
063700981130     C                   ENDIF
063800930920     C                   SETON                                        90
063900930917     C                   GOTO      ENDCT2
064000000710     C*SE INVECE PAXSTA=(-1) NON HO SELEZIONATO NIENTE.
064100930917X   3C                   ELSE
064200930917     C                   MOVEL     *ZEROS        VI2CKS
064300930920     C                   SETON                                        90
064400930917     C                   GOTO      ENDCT2
064500930917E   3C                   ENDIF
064600930920X   2C                   ENDIF
064700930920     C*
064800930920     C*CLIENTE:INSERIB. ED OBBLIG. SE ALMENO UN FLAG DI SCARICO='S'
064900091215     C     VI2CKS        ifeq      0
065000930920     C     DSFLG         ANDNE     *BLANKS
065100930920     C                   SETON                                        5090
065200930920     C                   GOTO      ENDCT2
065300930920     C                   ENDIF
065400930920     C*
065500930920     C* SE CLIENTE VALORIZZATO LO CONTROLLO
065600930920X   2C     VI2CKS        IFNE      0
065700981130     C                   CLEAR                   DSBS69
065800130521     C                   MOVEL     dutKCI        I69KCC
065900981130     C                   MOVEL     VI2CKS        I69KAC
066000981130     C                   MOVEL     KNSIF         I69SIF
066100981130     C                   CALL      'TIBS69R'
066200981130     C                   PARM                    DSBS69
066300981130     C                   PARM                    DSACO
066400981130     C                   PARM                    DSIND
066500981130     C                   PARM                    DSCLP
066600981130     C                   PARM                    DSCLS
066700981130     C* ERRORE
066800981130     C     O69ERR        IFNE      ' '
066900981130     C                   SETON                                        9051
067000930917     C                   GOTO      ENDCT2
067100930917X   3C                   ELSE
067200930917     C* CODICE CLIENTE ANNULLATO
067300930917IF  4C     ACOFLG        IFNE      *BLANKS
067400930920     C                   SETON                                        5290
067500930917     C                   GOTO      ENDCT2
067600930917E   4C                   ENDIF
067700930917     C*
067800930917     C                   MOVEL     ACORAG        VI2RIC
067900930917E   3C                   ENDIF
068000930917     C*
068100930917E   2C                   ENDIF
068200930920     C**** *** *** *** *** *** *** ***
068300930920     C*DATI FATTURA
068400930920     C**** *** *** *** *** *** *** ***
068500930920     C*
068600930920     C* RICERCA ALFABETICA CLIENTE
068700930920     C*VA FATTA PRIMA DI TUTTI GLI ALTRI CONTROLLI, PERCHE' SE RICHIESTA E VA
068800930920     C*BENE,VALORIZZA IL CAMPO DEL CODICE CLIENTE,CHE RIENTRA NEL TEST SEGUENTE
068900930920     C     VI2RI2        IFNE      *BLANKS
069000930920     C     VI2FKS        ANDEQ     0
069100000710     C                   MOVEL     RSUT          PAXDUT           30
069200000710     C                   MOVEL     *BLANKS       PAXDMT
069300000710     C                   MOVEL     VI2RI2        PAXDMT
069400000710     C                   Z-ADD     0             PAXSTA
069500130521     C                   Z-ADD     dutKCI        PAXCCC
069600000710     C                   Z-ADD     1             PAXNUM
069700000710     C                   CLEAR                   PAXDIT
069800930920     C*
069900000710     C                   CALL      'XALFA3BR'
070000000710     C                   PARM                    PAXDUT
070100130521     C                   PARM                    CODUT
070200000710     C                   PARM                    PAXDMT           48
070300000710     C                   PARM                    PAXCCC
070400000710     C                   PARM                    PAXSTA            1 0
070500000710     C                   PARM                    PAXFLR
070600000710     C                   PARM                    PAXDIT
070700000710     C                   PARM                    PAXNUM            2 0
070800000710     C                   PARM                    PAXKCM           80
070900000710     C                   PARM                    PAXKSM          140
071000000710     C                   PARM                    PAXKDM           60
071100930920     C*
071200000710     C*PAXSTA NON =(-1),SE HO SELEZIONATO UN CODICE IN RICERCA.
071300000710     C     PAXSTA        IFNE      -1
071400000710     C                   MOVEL     PAXKSM        VI2FKS
071500981130     C                   CLEAR                   DSBS69
071600130521     C                   MOVEL     dutKCI        I69KCC
071700000710     C                   MOVEL     PAXKSM        I69KAC
071800981130     C                   MOVEL     KNSIF         I69SIF
071900981130     C                   CALL      'TIBS69R'
072000981130     C                   PARM                    DSBS69
072100981130     C                   PARM                    DSACO
072200981130     C                   PARM                    DSIND
072300981130     C                   PARM                    DSCLP
072400981130     C                   PARM                    DSCLS
072500981130     C*
072600981130     C     O69ERR        IFEQ      ' '
072700981130     C                   MOVEL     ACORAG        VI2RI2
072800981130     C                   ELSE
072900981130     C                   MOVEL     *BLANKS       VI2RI2
073000981130     C                   ENDIF
073100930920     C                   SETON                                        90
073200930920     C                   GOTO      ENDCT2
073300000710     C*SE INVECE PAXSTA=(-1) NON HO SELEZIONATO NIENTE.
073400930920X   3C                   ELSE
073500930920     C                   MOVEL     *ZEROS        VI2FKS
073600930920     C                   SETON                                        90
073700930920     C                   GOTO      ENDCT2
073800930920E   3C                   ENDIF
073900930920X   2C                   ENDIF
074000930920     C*
074100111019     C*TEST DEI 4 CAMPI (CADENZA,TIPO SCAR.DATI,CLI A CUI SCARIC)
074200930920     C*SE SONO TUTTI BLANK, ESEGUO SOLO IL CONTROLLO CHE ANCHE LE NOTE SIANO
074300930920     C*BLANK E SALTO GLI ALTRI CONTROLLI, ALTRIMENTI..PASSO AGLI ALTRI CONTROLLI
074400930920     C     VI2FGC        IFEQ      0
074500930920     C     VI2FSD        ANDEQ     *BLANKS
074600930920     C     VI2FKS        ANDEQ     0
074700930920     C*CONTROLLO NOTE:INSERIBILI SE VALORIZZATO ALMENO UN ALTRO DATO FATTURA
074800930920     C     VI2FNT        IFNE      *BLANKS
074900930920     C                   SETON                                        5390
075000930920     C                   GOTO      ENDCT2
075100930920     C                   ENDIF
075200930920     C*ALTRIMENTI,SE UNO DEI 4 CAMPI E' VALORIZZATO,  PROCEDO AI CONTROLLI
075300930920     C                   ELSE
075400930920     C*
075500930920     C*CONTROLLO GG CADENZA
075600930920     C*OBBLIGATORIO SOLO SE INSERITO ALMENO UNO DEGLI ALTRI DATI FATTURA
075700930920     C     VI2FGC        IFEQ      0
075800930920     C                   SETON                                        5490
075900930920     C                   GOTO      ENDCT2
076000930920     C                   ENDIF
076100930920     C*
076200930920     C*CONTROLLO TIPO SCARICO DATI
076300930920     C*OBBLIGATORIO SOLO SE INSERITO ALMENO UNO DEGLI ALTRI DATI FATTURA
076400930920     C     VI2FSD        IFEQ      *BLANKS
076500930920     C                   SETON                                        5590
076600930920     C                   GOTO      ENDCT2
076700930920     C                   ENDIF
076800930920     C*
076900930920     C*RICERCA TIPO SARICO DATI
077000930920IF  2C     VI2FSD        IFEQ      ' ?'
077100930920     C     VI2FSD        OREQ      '? '
077200930920     C                   MOVEL     CODUT         KUT
077300930920     C                   MOVEL     '3T'          COD
077400930920     C                   MOVEL     *BLANKS       KEY
077500930920     C                   MOVEL     *BLANKS       §DES
077600930920     C                   CALL      'X§TABER'
077700930920     C                   PARM                    KUT
077800930920     C                   PARM                    COD
077900930920     C                   PARM                    KEY
078000930920     C                   PARM                    §DES
078100930920     C* METTO CODICE TP SCARIC DATI SELEZIONATO IN CAMPO VIDEO ED EMETTO.
078200930920     C                   MOVEL     KEY           VI2FSD
078300930920     C                   SETON                                        90
078400930920     C                   GOTO      ENDCT2
078500930920     C                   ENDIF
078600930920     C*
078700930920     C*CONTROLLO TIPO SCARICO DATI
078800930920     C                   MOVEL     '3T'          COD
078900930920     C                   MOVEL     *BLANKS       KEY
079000930920     C                   MOVEL     VI2FSD        KEY
079100930920     C     KTAB          CHAIN     TABEL00F                           60
079200930920     C* TIPO SCARICO DATI INESISTENTE
079300930920IF  2C     *IN60         IFEQ      '1'
079400930920     C                   SETON                                        90
079500930920     C                   GOTO      ENDCT2
079600930920X   2C                   ELSE
079700930920     C* TIPO SCARICO DATI ANNULLATO
079800930920IF  3C     TBLFLG        IFNE      *BLANKS
079900930920     C                   SETON                                        6190
080000930920     C                   GOTO      ENDCT2
080100930920E   3C                   ENDIF
080200930920E   2C                   ENDIF
080300050125      * Controllo membro per XCO con campo RFO
080400050125     c                   If        vi2mbx <> *Blanks and vi2rfo <> 'X'
080500050125     c                   Eval      *In58 = *On
080600050125     c                   Eval      *In90 = *On
080700050125     c                   Goto      endct2
080800050125     c                   EndIf
080900050125     c                   If        vi2mbx = *Blanks and vi2rfo = 'X'
081000050125     c                   Eval      *In59 = *On
081100050125     c                   Eval      *In90 = *On
081200050125     c                   Goto      endct2
081300050125     c                   EndIf
081400930920     C*
081500930920     C*CLIENTE OBBLIGATORIO SE INSERITO ALMENO UN ALTRO DATO FATTURA
081600930921     C     VI2FKS        IFEQ      0
081700930920     C                   SETON                                        5790
081800930920     C                   GOTO      ENDCT2
081900930920     C                   ENDIF
082000930920     C*
082100930920     C* SE CLIENTE VALORIZZATO LO CONTROLLO
082200930920X   2C     VI2FKS        IFNE      0
082300981130     C                   CLEAR                   DSBS69
082400130521     C                   MOVEL     dutKCI        I69KCC
082500981130     C                   MOVEL     VI2FKS        I69KAC
082600981130     C                   MOVEL     KNSIF         I69SIF
082700981130     C                   CALL      'TIBS69R'
082800981130     C                   PARM                    DSBS69
082900981130     C                   PARM                    DSACO
083000981130     C                   PARM                    DSIND
083100981130     C                   PARM                    DSCLP
083200981130     C                   PARM                    DSCLS
083300981130     C* ERRORE
083400981130     C     O69ERR        IFNE      ' '
083500981130     C                   SETON                                        9062
083600930920     C                   GOTO      ENDCT2
083700930920X   3C                   ELSE
083800930920     C* CODICE CLIENTE ANNULLATO
083900930920IF  4C     ACOFLG        IFNE      *BLANKS
084000930920     C                   SETON                                        6390
084100930920     C                   GOTO      ENDCT2
084200930920E   4C                   ENDIF
084300930920     C                   MOVEL     ACORAG        VI2RI2
084400930920E   3C                   ENDIF
084500930920E   2C                   ENDIF
084600930920     C*
084700930920     C*
084800930920     C                   ENDIF
084900930920     C*
085000930920     C*CONTROLLO CMD6:DA ACCETTARE SOLO SE IMMESSI ALMENO DATI C/A O DATI FATT.
085100921103     C*
085200930920     C     *INKF         IFEQ      *ON
085300930920     C     DSFLG         ANDEQ     *BLANKS
085400930920     C     VI2FGC        ANDEQ     0
085500930920     C     VI2FSD        ANDEQ     *BLANKS
085600930920     C     VI2FKS        ANDEQ     0
085700930920     C                   SETON                                        6590
085800930920     C                   SETOFF                                       KF
085900930920     C                   GOTO      ENDCT2
086000930920     C                   ENDIF
086100921103     C     ENDCT2        ENDSR
086200930920     C**------------------------------------------------------------
086300930920     C** SUBROUTINE DI AGGIORNAMENTO / INSERIMENTO REC DI TABELLA
086400930920     C**------------------------------------------------------------
086500910423     C     SCRIVI        BEGSR
086600930915     C                   MOVEL     '4C'          COD
086700930913     C                   MOVEL     *BLANK        KEY
086800930913     C                   MOVEL     VIDCLI        KEY
086900891115     C     KTAB          CHAIN     TABEL                              80
087000930920     C*
087100930913     C                   MOVEL     CODUT         TBLKUT
087200930913     C                   MOVEL     COD           TBLCOD
087300930913     C                   MOVEL     KEY           TBLKEY
087400930921     C*§ DATI DA VIDEO A DS4C, PER RIEMPIRE IL CAMPO TBLUNI DI TABEL.
087500930921     C                   MOVEL     VI2FSC        §4CFSC
087600930921     C                   MOVEL     VI2FAN        §4CFAN
087700930921     C                   MOVEL     VI2FLG        §4CFLG
087800930921     C                   MOVEL     VI2DTS        §4CDTS
087900930921     C                   Z-ADD     VI2CGC        §4CCGC
088000050125     c                   Eval      §4crfo = vi2rfo
088100930921     C                   MOVEL     VI2TSD        §4CTSD
088200930921     C                   Z-ADD     VI2CKS        §4CCKS
088300930921     C                   MOVEL     VI2FNT        §4CFNT
088400930921     C                   Z-ADD     VI2FGC        §4CFGC
088500930921     C                   MOVEL     VI2FSD        §4CFSD
088600050125     c                   Eval      §4cmbx = vi2mbx
088700930921     C                   Z-ADD     VI2FKS        §4CFKS
088800930921     C*IL CAMPO DATA ULTIMA ESTRAZIONE E' UN CAMPO DI OUTPUT CHE NON VA GESTITO
088900930921     C*IN VARIAZIONE E VA MESSO A ZERO IN IMMISSIONE.
089000930921     C   80              Z-ADD     0             §4CFDU                 98
089100930920     C                   MOVEL     DS4C          TBLUNI
089200930920     C*§ DATI DA FISSARE
089300021111     C                   CLEAR                   TBLFTT
089400981201     C                   CLEAR                   TBLFLT
089500900420     C                   MOVE      *BLANK        TBLFTR
089600900420     C                   MOVE      *ZERO         TBLDTR
089700900420     C*§
089800930921     C   80              WRITE     TABEL
089900930921     C  N80              UPDATE    TABEL
090000891030     C                   ENDSR
090100930920     C**----------------------------------------------------
090200930916     **   ROUTINE DI ROVESCIAMENTO DATE AAMMGG  IN  GGMMAA           **
090300930920     C**----------------------------------------------------
090400930916     C     ROVDA         BEGSR
090500930916     C                   MOVEL     $DATA         AA                2 0
090600930916     C                   MOVE      $DATA         GG                2 0
090700930916     C                   MOVEL     GG            $DATA             6 0
090800930916     C                   MOVE      AA            $DATA
090900930916     C                   ENDSR
091000930914     C*----------------------------------------------------------------
091100930916     **   KOMAND
091200930914     **   ROUTINE DI ESECUZIONE CALL (CMD7 O CMD14)
091300930914     C*----------------------------------------------------------------
091400930914     C     KOMAND        BEGSR
091500931124      *
091600930914     C*NON DEVO MANDARGLIELO BLANK, ALTRIMENTI NEL CHIAMATO,QUANDO FA IL TEST
091700930914     C*PER SAPERE SE E' STATO CHIAMATO, RISULTA DI NO.
091800930914     C     VIDCLI        IFEQ      *BLANKS
091900930914     C                   MOVE      *ZEROS        KPJBU
092000930914     C                   ELSE
092100930914     C                   MOVEL     VIDCLI        KPJBU
092200930914     C                   ENDIF
092700931124      *
092800931124      * CMD22- GESTIONE SCAMBIO DATI CONS.GIAC.DISP.CON CLIENTI
092900931124     C   KW              CALL      'TRTB40R'
093000930914     C                   PARM                    KPJBA
093100931124      *
093200931124      * CMD21 - GESTIONE DATI RICEZ BOLLE DA CLIENTI
093300931124     C   KV              CALL      'TRTB28R'
093400930914     C                   PARM                    KPJBA
093500000731      *
093600930914     C                   ENDSR
