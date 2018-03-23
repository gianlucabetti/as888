000100020418     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020418      *------------------------------------------------------------------------*
000300020418      * CAPPARIO - GESTIONE TERMINAL - SCELTA
000400020418      *------------------------------------------------------------------------*
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800970528     FTISI17D   CF   E             WORKSTN INFDS(DEVDS1)
000900970528     F                                     SFILE(SI17D05:SFLULT)
001000970528     F                                     SFILE(SI17DMS:MSGULT)
001100970528     FAZORG01L  IF   E           K DISK
001200970526     F*------------
001300970528     F* ECCEZIONI TERMINAL
001400970526     F*------------
001500970618     FAZCAE01L  IF   E           K DISK
001600970618     FAZCAE02L  IF   E           K DISK
001700970618     F                                     RENAME(AZCAE000:AZCAE2)
001800980107     FAZCAE05L  IF   E           K DISK
001900980107     F                                     RENAME(AZCAE000:AZCAE5)
002000970526     D*------------------------------------------------------------------------*
002100970526     D* SCHIERE
002200970526     D*------------------------------------------------------------------------*
002300970526     D*------------
002400970526     D* DECODIFICHE
002500970526     D*------------
002600970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002700970526     D*------------
002800970526     D* MESSAGGI
002900970526     D*------------
003000970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
003100970528     D*------------
003200970528     D* COMANDI
003300970528     D*------------
003400970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003500970526     D*------------
003600970526     D* DESCRIZIONI OPZIONI
003700970526     D*------------
003800970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003900970526     D*------------
004000970526     D* DESCRIZIONI TASTI FUNZIONALI
004100970526     D*------------
004200970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004300970526     D*------------
004400970526     D* OPZIONI DA CARICARE A VIDEO
004500970526     D*------------
004600970526     D OPV             S             19    DIM(24)
004700970526     D*------------
004800970526     D* FUNZIONI DA CARICARE A VIDEO
004900970526     D*------------
005000970527     D FUV             S             19    DIM(24)
005100970528     D*------------
005200970528     D* ORGANIGRAMMA
005300970528     D*------------
005400971118     D ORG             S              3  0 DIM(999)                             *CODICE
005500971118     D DORG            S             30    DIM(999)                             *DESCRIZIONE
005600970526     D*------------------------------------------------------------------------*
005700970526     D* INPUT
005800970526     D*------------------------------------------------------------------------*
005900970526     D*------------
006000970526     D* RIDEFINIZIONE OPZIONI VIDEO
006100970526     D*------------
006200970526     D OPDDS           DS
006300970526     D  OPDOPZ                 1      2
006400970526     D  OPDDE1                 3     19
006500970526     D  OPDOP1                21     22
006600970526     D  OPDFI0                23     34
006700970526     D  OPDATT                35     35
006800970526     D  OPDFI1                36     80
006900970526     D  OPDDES                 1     19
007000970526     D  OPDRIG                 1     80
007100970526     D*------------
007200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
007300970526     D*------------
007400970526     D FUDDS           DS
007500970526     D  FUDDE1                 1      1
007600970526     D  FUDFUN                 2      3
007700970526     D  FUDDE2                 4     19
007800970526     D  FUDFI0                20     34
007900970526     D  FUDATT                35     35
008000970526     D  FUDFI1                36     80
008100970526     D  FUDDES                 1     19
008200970526     D  FUDRIG                 1     80
008300970526     D*------------
008400970526     D* DS GESTIONE TASTI FUNZIONALI
008500970526     D*------------
008600970526     D DSKEY         E DS
008700970526     D DEVDS1          DS
008800970526     D  KEY                  369    369
008900970526     D*------------
009000970526     D* DS "XMSG" - GESTIONE MESSAGGI
009100970526     D*------------
009200970526     D DSMSG         E DS
009300970527     D*------------------
009400970527     D* DS "XSRDA8" - CONTROLLA DATA (8)
009500970527     D*------------------
009600970527     D WLBDA8          DS                  INZ
009700970527     D  G08DAT                 1      8  0
009800970527     D  G08INV                 9     16  0
009900970527     D  G08ERR                17     17
010000970527     D  G08TGI                18     22  0
010100970526     D*------------
010200970526     D* DS DI PROCEDURA
010300970526     D*------------
010400970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
010500970526     D*------------
010600970526     D* ARCHITETTURA
010700970526     D*------------
010800970526     D KPJBA         E DS
010900970526     C*------------------------------------------------------------------------*
011000970526     C* MAIN LINES
011100970526     C*------------------------------------------------------------------------*
011200970526     C*
011300970526     C* SE NON CI SONO ERRORI, PROSEGUE
011400970526IF  1C     ERRL00        IFNE      '1'
011500970526     C*
011600970526     C* CARICA IL SFILE
011700970526     C                   EXSR      POSSFL
011800970526     C*
011900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
012100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
012200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
012300970526     C*
012400970526     C* REIMPOSTA ERRORE DI PROCERURA
012500970526     C                   MOVEL     '0'           ERRL00
012600970526     C*
012700970526     C* EMETTE IL SFILE
012800970526     C                   EXSR      OUTSFL
012900970526     C*
013000970526     C* GESTIONE TASTI FUNZIONALI
013100970526     C                   EXSR      FUNCON
013200970526     C*
013300970526E   2C                   ENDDO
013400970527     C*
013500970527     C* CI SONO ERRORI IN ENTRATA
013600970527X   1C                   ELSE
013700970527     C                   MOVEL     '1'           D00ERR
013800970527     C                   MOVEL     DSMSMS        D00MSG
013900970526E   1C                   ENDIF
014000970526     C* FINE PGM
014100970526     C                   SETON                                        LR
014200970526     C*------------------------------------------------------------------------*
014300970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
014400970526     C*------------------------------------------------------------------------*
014500970526     C     F03GES        BEGSR
014600970526     C*
014700970526     C* IMPOSTA "PREMUTO F03"
014800970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014900970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
015000970526     C*
015100970526     C                   ENDSR
015200970526     C*------------------------------------------------------------------------*
015300970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
015400970526     C*------------------------------------------------------------------------*
015500970526     C     F05GES        BEGSR
015600970526     C*
015700970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015800970526     C*
015900970526     C                   ENDSR
016000970526     C*------------------------------------------------------------------------*
016100970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
016200970526     C*------------------------------------------------------------------------*
016300970526     C     F06GES        BEGSR
016400970526     C*
016500970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016600970602     C                   MOVEL     *BLANKS       DEPA
016700970602     C                   Z-ADD     *ZEROS        DETP
016800970602     C                   Z-ADD     *ZEROS        DETA
016900970602     C                   Z-ADD     *ZEROS        DEDD
017000970602     C                   MOVEL     DEPOP0        WOP0
017100970529     C                   MOVEL     'F06'         WOP1
017200970528     C                   MOVEL     'TISI18R'     WPGM              8
017300970526     C                   EXSR      LANPGM                                       *LANCIA PGM
017400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017500970526E   1C                   ENDIF
017600970526     C*
017700970526     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900970526     C* F12GES - GESTIONE F12 -> RITORNO
018000970526     C*------------------------------------------------------------------------*
018100970526     C     F12GES        BEGSR
018200970526     C*
018300970526     C* IMPOSTA "PREMUTO F12"
018400970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018500970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
018600970526     C*
018700970526     C                   ENDSR
018800970528     C*------------------------------------------------------------------------*
018900970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
019000970528     C*------------------------------------------------------------------------*
019100970528     C     F21GES        BEGSR
019200970612     C*
019300980206     C                   MOVEL     *BLANKS       DEPA
019400980206     C                   Z-ADD     *ZEROS        DETP
019500980206     C                   Z-ADD     *ZEROS        DETA
019600980206     C                   Z-ADD     *ZEROS        DEDD
019700970612     C                   MOVEL     DEPOP0        WOP0
019800970612     C                   MOVE      '03'          WOP0
019900970612     C                   MOVEL     'F21'         WOP1
020000970612     C                   MOVEL     'TISI19R'     WPGM              8
020100970612     C                   EXSR      LANPGM
020200970528     C*
020300970528     C                   ENDSR
020400970528     C*------------------------------------------------------------------------*
020500970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
020600970528     C*------------------------------------------------------------------------*
020700970528     C     F22GES        BEGSR
020800970528     C*
020900970528     C                   Z-ADD     80            LENGH            15 5
021000970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
021100970528     C                   PARM                    CMD(1)
021200970528     C                   PARM                    LENGH
021300970528     C*
021400970528     C                   ENDSR
021500970526     C*------------------------------------------------------------------------*
021600970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021700970526     C*------------------------------------------------------------------------*
021800970526     C     F23GES        BEGSR
021900970526     C*
022000970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022100970526     C*
022200970526     C                   ENDSR
022300970526     C*------------------------------------------------------------------------*
022400970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022500970526     C*------------------------------------------------------------------------*
022600970526     C     F24GES        BEGSR
022700970526     C*
022800970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
022900970526     C*
023000970526     C                   ENDSR
023100970526     C*------------------------------------------------------------------------*
023200970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023300970526     C*------------------------------------------------------------------------*
023400970526     C     RUPGES        BEGSR
023500970526     C*
023600970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023700970526     C*
023800970526     C                   ENDSR
023900970526     C*------------------------------------------------------------------------*
024000970526     C* ENTGES - GESTIONE ENTER -> INVIO
024100970526     C*------------------------------------------------------------------------*
024200970526     C     ENTGES        BEGSR
024300970526     C*
024400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024500970526     C                   EXSR      NEWPOS
024600970526     C*
024700970526     C* SWPOSI-> 0=NO NUOVO OSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
024800970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
024900970526IF  2C     SWPOSI        IFNE      0
025000970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025100970526X   2C                   ELSE
025200970526IF  3C     SFLULT        IFNE      0
025300970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025400970526     C*
025500970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
025600970526IF  4C     SWMODI        IFNE      0
025700970526     C                   EXSR      POSSFL
025800970526E   4C                   ENDIF
025900970526E   3C                   ENDIF
026000970526E   2C                   ENDIF
026100970526E   1C                   ENDIF
026200970526     C*
026300970526     C                   ENDSR
026400970526     C*------------------------------------------------------------------------*
026500970526     C* O01GES - GESTIONE OPZIONE 1=SCELTA
026600970526     C*------------------------------------------------------------------------*
026700970526     C     O01GES        BEGSR
026800970526     C*
026900970527     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027000970528     C                   MOVEL     VIDEPA        D00EPA
027100970602     C                   Z-ADD     VIDTFP        D00ETP
027200970528     C                   Z-ADD     VIDTFA        D00ETA
027300970528     C                   Z-ADD     VIHDDE        D00EDD
027400970526     C                   MOVEL     '1'           FINL00                         *FINE PGM
027500970526     C*
027600970526     C                   ENDSR
027700970526     C*------------------------------------------------------------------------*
027800970526     C* O02GES - GESTIONE OPZIONE 2=MODIFICA
027900970526     C*------------------------------------------------------------------------*
028000970526     C     O02GES        BEGSR
028100970526     C*
028200970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
028300970526IF  1C     VIDATB        IFEQ      'A'
028400970526     C                   MOVEL     '1'           ERRL00
028500970526     C                   MOVEL     MSG(4)        DSMSMS
028600970526E   1C                   ENDIF
028700970527     C* MODIFICA
028800970526IF  1C     ERRL00        IFEQ      '0'
028900970602     C                   MOVEL     VIDEPA        DEPA
029000970602     C                   Z-ADD     VIDTFP        DETP
029100970602     C                   Z-ADD     VIDTFA        DETA
029200970602     C                   Z-ADD     VIHDDE        DEDD
029300970602     C                   MOVEL     DEPOP0        WOP0
029400970526     C                   MOVEL     'O02'         WOP1
029500970528     C                   MOVEL     'TISI18R'     WPGM
029600970526     C                   EXSR      LANPGM
029700970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
029800970526E   1C                   ENDIF
029900970526     C*
030000970526     C                   ENDSR
030100970526     C*------------------------------------------------------------------------*
030200970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
030300970526     C*------------------------------------------------------------------------*
030400970526     C     O04GES        BEGSR
030500970526     C*
030600970526     C* OPERAZIONE NON VALIDA PER RECORD ANNULLATO
030700970526IF  1C     VIDATB        IFEQ      'A'
030800970526     C                   MOVEL     '1'           ERRL00
030900970526     C                   MOVEL     MSG(4)        DSMSMS
031000970526E   1C                   ENDIF
031100970527     C* ANNULLAMENTO
031200970526IF  1C     ERRL00        IFEQ      '0'
031300970602     C                   MOVEL     VIDEPA        DEPA
031400970602     C                   Z-ADD     VIDTFP        DETP
031500970602     C                   Z-ADD     VIDTFA        DETA
031600970602     C                   Z-ADD     VIHDDE        DEDD
031700970602     C                   MOVEL     DEPOP0        WOP0
031800970526     C                   MOVEL     'O04'         WOP1
031900970528     C                   MOVEL     'TISI18R'     WPGM
032000970526     C                   EXSR      LANPGM
032100970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
032200970526E   1C                   ENDIF
032300970526     C*
032400970526     C                   ENDSR
032500970526     C*------------------------------------------------------------------------*
032600970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
032700970526     C*------------------------------------------------------------------------*
032800970526     C     O05GES        BEGSR
032900970526     C*
033000970602     C                   MOVEL     VIDEPA        DEPA
033100970602     C                   Z-ADD     VIDTFP        DETP
033200970602     C                   Z-ADD     VIDTFA        DETA
033300970602     C                   Z-ADD     VIHDDE        DEDD
033400970602     C                   MOVEL     DEPOP0        WOP0
033500970526     C                   MOVEL     'O05'         WOP1
033600970528     C                   MOVEL     'TISI18R'     WPGM
033700970526     C                   EXSR      LANPGM
033800970526     C*
033900970526     C                   ENDSR
034000970526     C*------------------------------------------------------------------------*
034100970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
034200970526     C*------------------------------------------------------------------------*
034300970526     C     O07GES        BEGSR
034400970526     C*
034500970526     C* OPERAZIONE NON VALIDA PER RECORD NON ANNULLATO
034600970526IF  1C     VIDATB        IFNE      'A'
034700970526     C                   MOVEL     '1'           ERRL00
034800970526     C                   MOVEL     MSG(5)        DSMSMS
034900970526E   1C                   ENDIF
035000970526     C* RIPRISITINO
035100970527IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035200970602     C                   MOVEL     VIDEPA        DEPA
035300970602     C                   Z-ADD     VIDTFP        DETP
035400970602     C                   Z-ADD     VIDTFA        DETA
035500970602     C                   Z-ADD     VIHDDE        DEDD
035600970602     C                   MOVEL     DEPOP0        WOP0
035700970526     C                   MOVEL     'O07'         WOP1
035800970528     C                   MOVEL     'TISI18R'     WPGM
035900970526     C                   EXSR      LANPGM
036000970528     C*---                 Z-ADD1         SWMODI           *FLAG RICARICA SFL
036100970526E   1C                   ENDIF
036200970526     C*
036300970526     C                   ENDSR
036400970526     C*------------------------------------------------------------------------*
036500970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
036600970526     C*------------------------------------------------------------------------*
036700970526     C     NEWPOS        BEGSR
036800970526     C*
036900970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
037000970526     C*         1= RICHIESTO POSIZIONAMENTO
037100970526     C                   Z-ADD     0             SWPOSI            7 0
037200970526     C*
037300970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
037400970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037500970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037600970526     C                   Z-ADD     0             SWMODI            1 0
037700970530     C*
037800970530     C* REIMPOSTA INDICATORI DI ERRORE
037900970530     C                   SETOFF                                       808182
038000970530     C                   SETOFF                                       83
038100970526     C*
038200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
038300970528IF  1C     VIDPPA        IFNE      DEPEPA
038400970528     C     VIDPTP        ORNE      DEPTFP
038500970528     C     VIDPTA        ORNE      DEPTFA
038600980107     C     VIDPTE        ORNE      DEPTFE
038700970528     C     VIDPDD        ORNE      DEPDDE
038800970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
038900970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
039000970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
039100970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
039200970528     C                   Z-ADD     VIDPTP        DEPTFP
039300970528     C                   Z-ADD     VIDPTA        DEPTFA
039400980107     C                   Z-ADD     VIDPTE        DEPTFE
039500970528     C                   Z-ADD     VIDPDD        DEPDDE
039600970526E   2C                   ENDIF
039700970526E   1C                   ENDIF
039800970526     C*
039900970526     C                   ENDSR
040000970526     C*------------------------------------------------------------------------*
040100970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
040200970526     C*------------------------------------------------------------------------*
040300970528     C     SETFIL        BEGSR
040400970526     C*
040500970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
040600970528IF  1C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
040700970618     C                   MOVEL     VIDPPA        KCEEPA
040800970618     C                   Z-ADD     VIDPTP        KCETFP
040900980108     C                   Z-ADD     VIDPTA        KCETFA
041000970618     C     KEYCAE        SETLL     AZCAE01L                           99
041100970528E   1C                   ENDIF
041200970528     C*
041300970528IF  1C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
041400970618     C                   MOVEL     VIDPPA        KCEEPA
041500970618     C                   Z-ADD     VIDPTA        KCETFA
041600980108     C                   Z-ADD     VIDPTP        KCETFP
041700970618     C     KE2CAE        SETLL     AZCAE02L                           99
041800970528E   1C                   ENDIF
041900980107     C*
042000980107IF  1C     WTIPRI        IFEQ      'ECC'                                        *PER ARRIVO
042100980107     C                   MOVEL     VIDPPA        KCEEPA
042200980107     C                   Z-ADD     VIDPTE        KCETFE
042300980108     C                   Z-ADD     VIDPTA        KCETFA
042400980108     C                   Z-ADD     VIDPTP        KCETFP
042500980107     C     KE5CAE        SETLL     AZCAE05L                           99
042600980107E   1C                   ENDIF
042700970526     C* LETTURA RECORD
042800970526IF  1C     *IN99         IFEQ      *OFF
042900970526     C                   EXSR      LETFIL
043000970526E   1C                   ENDIF
043100970526     C*
043200970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043300970526IF  1C     *IN99         IFEQ      *ON
043400970526     C                   Z-ADD     1             SFLEOF
043500970526E   1C                   ENDIF
043600970526     C*
043700970526     C                   ENDSR
043800970526     C*------------------------------------------------------------------------*
043900970526     C* LETFIL - LETTURA RECORD
044000970526     C*------------------------------------------------------------------------*
044100970526     C     LETFIL        BEGSR
044200970526     C*
044300970526     C* LEGGE FINO A:
044400970526     C*     - FINE FILE
044500970526     C*     - TROVATO RECORD VALIDO
044600970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
044700970526DO  1C     *IN99         DOWEQ     *OFF
044800970526     C     WRECOK        ANDEQ     'N'
044900970526     C*
045000970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
045100970528IF  2C     WTIPRI        IFEQ      'PAR'                                        *PER PARTENZA
045200970618     C                   READ      AZCAE01L                               99
045300970528E   2C                   ENDIF
045400970526     C*
045500970528IF  2C     WTIPRI        IFEQ      'ARR'                                        *PER ARRIVO
045600970618     C                   READ      AZCAE02L                               99
045700970528E   2C                   ENDIF
045800980107     C*
045900980107IF  2C     WTIPRI        IFEQ      'ECC'                                        *PER ECCEZIONE
046000980107     C                   READ      AZCAE05L                               99
046100980107E   2C                   ENDIF
046200970528     C*
046300970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
046400970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
046500970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
046600970530X   2C                   ELSE
046700970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
046800970530E   2C                   ENDIF
046900970526E   1C                   ENDDO
047000970526     C*
047100970526     C                   ENDSR
047200970526     C*------------------------------------------------------------------------*
047300970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
047400970526     C*------------------------------------------------------------------------*
047500970526     C     CHKREC        BEGSR
047600970526     C*
047700970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
047800970530     C*
047900970530     C* RECORD DIVERSO DALLE SELEZIONI RICHIESTE, ESCLUDE
048000970530IF  1C     VIDPDD        IFGT      *ZEROS
048100970618IF  2C     PDDVID        IFGE      CAEDDE                                       *IN DECORRENZA
048200970618     C     PDDVID        ANDLE     CAEDSC                                       *ALLA DATA
048300970530X   2C                   ELSE
048400970530     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048500970530E   2C                   ENDIF
048600970530E   1C                   ENDIF
048700970526     C*
048800970526     C                   ENDSR
048900970526     C*------------------------------------------------------------------------*
049000970526     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
049100970526     C*------------------------------------------------------------------------*
049200970526     C     IRESFL        BEGSR
049300970526     C*
049400970526     C                   MOVEL     *BLANKS       OPZKEY
049500970618     C                   Z-ADD     CAEDDE        VIHDDE
049600970618     C                   MOVEL     CAEATB        VIDATB
049700970618     C                   MOVEL     CAEEPA        VIDEPA
049800970618     C                   Z-ADD     CAETFP        VIDTFP
049900970618     C                   Z-ADD     CAETFA        VIDTFA
050000970618     C                   Z-ADD     CAETFE        VIDTFE
050100970618     C                   Z-ADD     CAEDDE        G08INV
050200970618     C                   Z-ADD     *ZEROS        G08DAT
050300970618     C                   MOVEL     '3'           G08ERR
050400970618     C                   CALL      'XSRDA8'
050500970618     C                   PARM                    WLBDA8
050600970618     C                   Z-ADD     G08DAT        VIDDDE
050700970618     C                   Z-ADD     CAEDSC        G08INV
050800970618     C                   Z-ADD     *ZEROS        G08DAT
050900970618     C                   MOVEL     '3'           G08ERR
051000970618     C                   CALL      'XSRDA8'
051100970618     C                   PARM                    WLBDA8
051200970618     C                   Z-ADD     G08DAT        VIDDSC
051300970528     C* DECODIFICHE
051400970618     C                   MOVEL     *BLANKS       VIDDTP
051500970618IF  1C     CAETFP        IFGT      *ZEROS
051600970528     C                   SETOFF                                           28
051700970528     C                   Z-ADD     1             I
051800970618     C     CAETFP        LOOKUP    ORG(I)                                 28
051900970618IF  2C     *IN28         IFEQ      *ON                                          *TROVATO
052000970528     C                   MOVEL     DORG(I)       VIDDTP
052100970618E   2C                   ENDIF
052200970618E   1C                   ENDIF
052300970526     C*
052400970618     C                   MOVEL     *BLANKS       VIDDTA
052500970618IF  1C     CAETFA        IFGT      *ZEROS
052600970528     C                   SETOFF                                           28
052700970528     C                   Z-ADD     1             I
052800970618     C     CAETFA        LOOKUP    ORG(I)                                 28
052900970618IF  2C     *IN28         IFEQ      *ON                                          *TROVATO
053000970528     C                   MOVEL     DORG(I)       VIDDTA
053100970618E   2C                   ENDIF
053200970618E   1C                   ENDIF
053300970528     C*
053400970618     C                   MOVEL     *BLANKS       VIDDTE
053500970528     C                   SETOFF                                           28
053600970528     C                   Z-ADD     1             I
053700970618     C     CAETFE        LOOKUP    ORG(I)                                 28
053800970528IF  1C     *IN28         IFEQ      *ON                                          *TROVATO
053900970528     C                   MOVEL     DORG(I)       VIDDTE
054000970528E   1C                   ENDIF
054100970528     C*
054200970526     C                   ENDSR
054300970526     C*------------------------------------------------------------------------*
054400970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
054500970526     C*------------------------------------------------------------------------*
054600970526     C     CHKPAR        BEGSR
054700970528     C*
054800970528     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER PARTENZA
054900970529IF  1C     VIDPPA        IFEQ      *BLANKS
055000970529     C     VIDPTP        ANDEQ     *ZEROS
055100970528     C     VIDPTA        ANDEQ     *ZEROS
055200980107     C     VIDPTE        ANDEQ     *ZEROS
055300970528     C                   MOVEL     'PAR'         WTIPRI            3
055400970528E   1C                   ENDIF                                                  *ORDINAMENTO RICHIES
055500970528     C*
055600970528     C* CONTROLLA LA DATA DECORRENZA
055700970528     C     *LIKE         DEFINE    VIDPDD        PDDVID                         *DATA IN A/M/G
055800970528     C                   Z-ADD     *ZEROS        PDDVID
055900970528IF  1C     ERRL00        IFEQ      '0'
056000970528IF  2C     VIDPDD        IFGT      *ZEROS
056100970528     C                   Z-ADD     VIDPDD        G08DAT
056200970528     C                   Z-ADD     *ZEROS        G08INV
056300970528     C                   MOVEL     '0'           G08ERR
056400970528     C                   CALL      'XSRDA8'
056500970528     C                   PARM                    WLBDA8
056600970528     C                   Z-ADD     G08DAT        VIDPDD
056700970528     C                   Z-ADD     G08INV        PDDVID
056800970528IF  3C     G08ERR        IFEQ      '1'
056900970528     C                   MOVEL     '1'           ERRL00
057000970528     C                   MOVEL     MSG(12)       DSMSMS
057100970528     C                   SETON                                        83
057200970528E   3C                   ENDIF
057300970528E   2C                   ENDIF
057400970528E   1C                   ENDIF
057500970528     C*
057600970528     C* CONTROLLA IL TIPO RICERCA
057700970528IF  1C     ERRL00        IFEQ      '0'
057800970528IF  2C     VIDPTP        IFNE      *ZEROS
057900980107     C                   MOVEL     'PAR'         WTIPRI
058000970528E   2C                   ENDIF
058100970528E   1C                   ENDIF
058200980107     C*
058300980107IF  1C     ERRL00        IFEQ      '0'
058400980107IF  2C     VIDPTA        IFNE      *ZEROS
058500970528     C                   MOVEL     'ARR'         WTIPRI
058600980107E   2C                   ENDIF
058700980107E   1C                   ENDIF
058800980107     C*
058900980107IF  1C     ERRL00        IFEQ      '0'
059000980107IF  2C     VIDPTE        IFNE      *ZEROS
059100980107     C                   MOVEL     'ECC'         WTIPRI
059200980107E   2C                   ENDIF
059300980107E   1C                   ENDIF
059400970526     C*
059500970526     C                   ENDSR
059600970526     C*------------------------------------------------------------------------*
059700970526     C* LANPGM - LANCIA UN PROGRAMMA
059800970526     C*------------------------------------------------------------------------*
059900970526     C     LANPGM        BEGSR
060000970526     C*
060100970526     C* IMPOSTA LA DS DI PROCEDURA
060200970602     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
060300970602     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
060400970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
060500970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
060600970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
060700970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
060800970602     C                   MOVEL     DEPA          D00EPA                         *PARTENZA/ARRIVO
060900970602     C                   Z-ADD     DETP          D00ETP                         *TERMINAL PARTENZA
061000970602     C                   Z-ADD     DETA          D00ETA                         *TERMINAL ARRIVO
061100970602     C                   Z-ADD     DEDD          D00EDD                         *DECORRENZA
061200970602     C                   Z-ADD     *ZEROS        D00TTC                         *ALTRI DATI
061300970530     C                   MOVEL     *BLANKS       D00TSP
061400970530     C                   Z-ADD     *ZEROS        D00TFP
061500970530     C                   MOVEL     *BLANKS       D00LOC
061600970603     C                   MOVEL     *BLANKS       D00SIN
061700970530     C                   MOVEL     *BLANKS       D00SPT
061800970530     C                   Z-ADD     *ZEROS        D00SLP
061900970530     C                   Z-ADD     *ZEROS        D00SLA
062000970530     C                   MOVEL     *BLANKS       D00STS
062100970530     C                   Z-ADD     *ZEROS        D00SDD
062200970526     C*
062300970526     C                   CALL      WPGM
062400970612     C                   PARM                    KPJBA
062500970612     C                   PARM                    DSSI00
062600970526     C*
062700970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
062800970526IF  1C     D00F03        IFEQ      '1'                                          *PREMUTO F03
062900970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
063000970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
063100970526X   1C                   ELSE                                                   *NON PREMUTO F03
063200970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
063300970526E   1C                   ENDIF
063400970526     C*
063500970526     C                   ENDSR
063600970527     C*------------------------------------------------------------------------*
063700970527     C* CARTAB - CARICA LE TABELLE OCCORRENTI
063800970527     C*------------------------------------------------------------------------*
063900970527     C     CARTAB        BEGSR
064000970528     C*---
064100970528     C* ORGANIGRAMMA
064200970528     C*---
064300970528     C                   Z-ADD     *ZEROS        I                 5 0
064400970528     C                   Z-ADD     *ZEROS        ORG
064500970528     C                   MOVEL     *BLANKS       DORG
064600970528     C     *LOVAL        SETLL     AZORG01L
064700970528     C                   READ      AZORG01L                               99
064800970528DO  1C     *IN99         DOWEQ     *OFF
064900970528IF  2C     ORGFVA        IFEQ      *BLANKS
065000970528     C     ORGFAG        ANDNE     'V'
065100970528     C                   ADD       1             I
065200970528     C                   Z-ADD     ORGFIL        ORG(I)
065300970528     C                   MOVEL     ORGDES        DORG(I)
065400970528E   2C                   ENDIF
065500970528     C                   READ      AZORG01L                               99
065600970528E   1C                   ENDDO                                                  *FINE ORGANIGRAMMA
065700970527     C*
065800970527     C                   ENDSR
065900970526     C*------------------------------------------------------------------------*
066000970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
066100970526     C*------------------------------------------------------------------------*
066200970526     C     CHKFOP        BEGSR
066300970626     C*
066400970626     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
066500970626IF  1C     DEPOP0        IFEQ      'P01'
066600970626     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
066700970626     C                   MOVEL     'S'           OPDATT
066800970626     C                   MOVEL     OPDRIG        OPD(1)
066900970626X   1C                   ELSE
067000970626     C* OPZIONI
067100970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
067200970626     C                   MOVEL     'S'           OPDATT
067300970626     C                   MOVEL     OPDRIG        OPD(5)
067400970626     C* FUNZIONI
067500970626E   1C                   ENDIF
067600970626     C*
067700970626     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI MODIFICA
067800970626IF  1C     DEPOP0        IFEQ      'P02'
067900970626     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
068000970626     C                   MOVEL     'S'           OPDATT
068100970626     C                   MOVEL     OPDRIG        OPD(2)
068200970626     C                   MOVEL     OPD(4)        OPDRIG                         *ANNULLAMENTO
068300970626     C                   MOVEL     'S'           OPDATT
068400970626     C                   MOVEL     OPDRIG        OPD(4)
068500970626     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
068600970626     C                   MOVEL     'S'           OPDATT
068700970626     C                   MOVEL     OPDRIG        OPD(7)
068800970626     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
068900970626     C                   MOVEL     'S'           FUDATT
069000970626     C                   MOVEL     FUDRIG        FUD(6)
069100970626E   1C                   ENDIF
069200970526     C*
069300970526     C* GESTIONE OPZIONI A VIDEO
069400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
069500970526     C                   EXSR      OPZGES
069600970526     C*
069700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
069800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
069900970526     C                   EXSR      FUNGES
070000970526     C*
070100970526     C                   ENDSR
070200970526     C*------------------------------------------------------------------------*
070300970526     C* *INZSR - OPERAZIONI INIZIALI
070400970526     C*------------------------------------------------------------------------*
070500970526     C     *INZSR        BEGSR
070600970526     C*---
070700970526     C* RICEVIMENTO PARAMETRI
070800970526     C*---
070900970526     C     *ENTRY        PLIST
071000970612     C                   PARM                    KPJBA
071100970612     C                   PARM                    DSSI00
071200970526     C*
071300970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
071400970526     C     XMSGPA        PLIST
071500970526     C                   PARM                    DSMSMS
071600970526     C                   PARM                    DSMSPG
071700970526     C                   PARM                    DSMSKE
071800970526     C                   PARM                    DSMSER
071900970526     C*
072000970526     C* IMPOSTA LA DS -TASTI PREMUTI-
072100970526     C                   CALL      'XKEY'
072200970526     C                   PARM                    DSKEY
072300970526     C*---
072400970526     C* VARIABILI NON RIFERITE AL DATA BASE
072500970526     C*---
072600970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
072700970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
072800970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
072900970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
073000970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
073100970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
073200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
073300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
073400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
073500970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
073600970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
073700970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
073800970529     C     *LIKE         DEFINE    D00OP1        WOP1
073900970602     C     *LIKE         DEFINE    D00EPA        DEPA
074000970602     C     *LIKE         DEFINE    D00ETP        DETP
074100970602     C     *LIKE         DEFINE    D00ETA        DETA
074200970602     C     *LIKE         DEFINE    D00EDD        DEDD
074300970526     C*---
074400970526     C* VARIABILI DI CONTROLLO
074500970526     C*---
074600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
074700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
074800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
074900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
075000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
075100970528     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
075200970526     C*
075300970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
075400970528     C                   MOVEL     'TISI17R'     DSMSPG
075500970528     C                   MOVEL     'TISI17R'     MSGQUE
075600970526     C                   MOVEL     *BLANKS       MSGKEY
075700970526     C                   MOVEL     *BLANKS       DSMSMS
075800970526     C                   MOVEL     *BLANKS       DSMSKE
075900970526     C                   Z-ADD     *ZEROS        DSMSER
076000970526     C*---
076100970526     C* VARIABILI RIFERITE AL DATA BASE
076200970526     C*---
076300970528     C     *LIKE         DEFINE    ORGFIL        KORFIL                         *AZORG01L
076400970618     C     *LIKE         DEFINE    CAEEPA        KCEEPA                         *AZCAE00F
076500970618     C     *LIKE         DEFINE    CAETFP        KCETFP
076600970618     C     *LIKE         DEFINE    CAETFA        KCETFA
076700980107     C     *LIKE         DEFINE    CAETFE        KCETFE
076800970618     C     *LIKE         DEFINE    CAEEPA        DEPEPA                         *DEPOSITI
076900970618     C     *LIKE         DEFINE    CAETFP        DEPTFP
077000970618     C     *LIKE         DEFINE    CAETFA        DEPTFA
077100980107     C     *LIKE         DEFINE    CAETFE        DEPTFE
077200970618     C     *LIKE         DEFINE    CAEDDE        DEPDDE
077300970526     C*---
077400970526     C* CHIAVI DI LETTURA
077500970526     C*---
077600970528     C* LETTURA AZORG01L - COMPLETA
077700970528     C     KEYORG        KLIST
077800970528     C                   KFLD                    KORFIL                         *FILIALE
077900970618     C* LETTURA AZCAE01L - PARZIALE
078000970618     C     KEYCAE        KLIST
078100970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
078200970618     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
078300980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
078400970618     C* LETTURA AZCAE02L - PARZIALE
078500970618     C     KE2CAE        KLIST
078600970618     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
078700970618     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
078800980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
078900980107     C* LETTURA AZCAE05L - PARZIALE
079000980107     C     KE5CAE        KLIST
079100980107     C                   KFLD                    KCEEPA                         *PARTENZA/ARRIVO
079200980107     C                   KFLD                    KCETFE                         *TERMINAL ECCEZIONE
079300980108     C                   KFLD                    KCETFA                         *TERMINAL ARRIVO
079400980108     C                   KFLD                    KCETFP                         *TERMINAL PARTENZA
079500970527     C*
079600970527     C* CARICA LE TABELLE OCCORRENTI
079700970527     C                   EXSR      CARTAB
079800970526     C*
079900970526     C* DEPOSITA LE OPZIONI DI ENTRATA
080000970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
080100970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
080200970526     C*
080300970526     C* CALCOLA LA DATA CORRENTE
080400970526     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
080500970526     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
080600970526     C                   Z-ADD     WN8           G08DAT
080700970526     C                   Z-ADD     *ZEROS        G08INV
080800970526     C                   MOVEL     '0'           G08ERR
080900970526     C                   CALL      'XSRDA8'
081000970526     C                   PARM                    WLBDA8
081100970526     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
081200970526     C*
081300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
081400970528     C                   MOVEL     D00EPA        VIDPPA                         *PARTENZA/ARRIVO
081500970528     C                   Z-ADD     D00ETP        VIDPTP                         *TERMINAL PARTENZA
081600970528     C                   Z-ADD     D00ETA        VIDPTA                         *TERMINAL ARRIVO
081700980107     C                   Z-ADD     *ZEROS        VIDPTE                         *TERMINAL ECCEZIONE
081800970528     C                   Z-ADD     *ZEROS        VIDPDD                         *DATA DECORRENZA
081900970528IF  1C     D00EDD        IFGT      *ZEROS
082000970528     C                   Z-ADD     D00EDD        G08INV
082100970528     C                   Z-ADD     *ZEROS        G08DAT
082200970528     C                   MOVEL     '3'           G08ERR
082300970528     C                   CALL      'XSRDA8'
082400970528     C                   PARM                    WLBDA8
082500970528     C                   Z-ADD     G08DAT        VIDPDD
082600970528E   1C                   ENDIF
082700970526     C                   EXSR      CHKPAR
082800970526IF  1C     ERRL00        IFEQ      '1'
082900970526     C                   MOVEL     '1'           D00ERR
083000970526     C                   MOVEL     DSMSMS        D00MSG
083100970526X   1C                   ELSE
083200970528     C                   MOVEL     VIDPPA        DEPEPA                         *DEPOSITI
083300970528     C                   Z-ADD     VIDPTP        DEPTFP
083400970528     C                   Z-ADD     VIDPTA        DEPTFA
083500980107     C                   Z-ADD     *ZEROS        DEPTFE
083600970528     C                   Z-ADD     VIDPDD        DEPDDE
083700970526E   1C                   ENDIF
083800970526     C*
083900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
084000970526     C                   EXSR      CHKFOP
084100970526     C*
084200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
084300970530IF  1C     DEPOP0        IFEQ      'P01'
084400970530     C                   MOVEL     DEC(3)        VIDING
084500970530E   1C                   ENDIF
084600970530IF  1C     DEPOP0        IFEQ      'P02'
084700970526     C                   MOVEL     DEC(1)        VIDING
084800970530E   1C                   ENDIF
084900970530IF  1C     DEPOP0        IFEQ      'P05'
085000970526     C                   MOVEL     DEC(2)        VIDING
085100970526E   1C                   ENDIF
085200970526     C*
085300970526     C                   ENDSR
085400970526     C*------------------------------------------------------------------------*
085500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
085600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
085700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
085800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
085900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
086000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
086100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
086200970526     C*------------------------------------------------------------------------*
086300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
086400970526     C*------------------------------------------------------------------------*
086500970526     C     POSSFL        BEGSR
086600970526     C*
086700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
086800970526     C                   EXSR      CLRSFL
086900970526     C*
087000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
087100970526     C                   EXSR      SETFIL
087200970526     C*
087300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
087400970526     C     SFLEOF        IFEQ      0
087500970526     C                   EXSR      CARSFL
087600970526     C                   END
087700970526     C*
087800970526     C                   ENDSR
087900970526     C*------------------------------------------------------------------------*
088000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
088100970526     C*------------------------------------------------------------------------*
088200970526     C     ENTATT        BEGSR
088300970526     C*
088400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
088500970526     C                   Z-ADD     0             OPZOK             1 0
088600970526     C*
088700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
088800970526     C                   Z-ADD     0             SFLVAR            5 0
088900970526     C*
089000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
089100970526     C                   Z-ADD     0             SFLERR
089200970526     C*
089300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
089400970526     C                   Z-ADD     SFLULT        WFLULT
089500970526     C*
089600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
089700970528     C                   READC     SI17D05                                98
089800970526     C     *IN98         DOWEQ     '0'
089900970526     C*
090000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
090100970526     C                   ADD       1             SFLVAR
090200970526     C*
090300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
090400970526     C     OPZOK         IFEQ      0
090500970526     C*
090600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
090700970526     C     OPZKEY        IFNE      *BLANKS
090800970526     C                   EXSR      OPZCON
090900970526     C                   END
091000970526     C*
091100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
091200970526     C*            2. PULIZIA SCELTA IN SFL
091300970526     C     OPZOK         IFEQ      0
091400970526     C                   MOVEL     '0'           *IN41
091500970526     C                   MOVEL     *BLANKS       OPZKEY
091600970526     C                   ELSE
091700970526     C*
091800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
091900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
092000970526     C                   MOVEL     '1'           *IN41
092100970526     C                   Z-ADD     SFLULT        SFLERR
092200970526     C                   END
092300970526     C                   ELSE
092400970526     C*
092500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
092600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
092700970526     C                   MOVEL     '1'           *IN41
092800970526     C                   END                                                    OPZOK=0
092900970526     C*
093000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
093100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
093200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
093300970526     C     OPZOK         IFNE      0
093400970526     C                   Z-ADD     SFLERR        SFLNBR
093500970526     C                   ELSE
093600970526     C                   Z-ADD     SFLULT        SFLNBR
093700970526     C                   END
093800970526     C*
093900970526     C* AGGIORNO RECORD SFL
094000970528     C                   UPDATE    SI17D05
094100970526     C*
094200970528     C                   READC     SI17D05                                98
094300970526     C                   END                                                    DO WHILE
094400970526     C*
094500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
094600970526     C                   Z-ADD     WFLULT        SFLULT
094700970526     C*
094800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
094900970526     C     SFLVAR        IFEQ      0
095000970526     C*---                 MOVEL'1'       FINL00
095100970526     C                   END
095200970526     C*
095300970526     C                   ENDSR
095400970526     C*------------------------------------------------------------------------*
095500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
095600970526     C*------------------------------------------------------------------------*
095700970526     C     CLRSFL        BEGSR
095800970526     C*
095900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
096000970526     C* (0=NON FINE FILE  1=FINE FILE)
096100970526     C                   Z-ADD     0             SFLEOF
096200970526     C*
096300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
096400970526     C                   Z-ADD     0             SFLULT
096500970526     C*
096600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
096700970526     C                   MOVEL     '1'           *IN40
096800970528     C                   WRITE     SI17D06
096900970526     C                   MOVEL     '0'           *IN40
097000970526     C*
097100970526     C                   ENDSR
097200970526     C*------------------------------------------------------------------------*
097300970526     C* CARSFL - CARICAMENTO SFL
097400970526     C*------------------------------------------------------------------------*
097500970526     C     CARSFL        BEGSR
097600970526     C*
097700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
097800970526     C     SFLULT        IFEQ      0
097900970526     C                   EXSR      SETFIL
098000970526     C                   END
098100970526     C*
098200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
098300970526     C                   Z-ADD     SFLULT        WFLULT
098400970526     C*
098500970526     C* CICLO:
098600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
098700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
098800970526     C                   Z-ADD     1             SFLRIG
098900970526     C     SFLRIG        DOWLE     SFLPAG
099000970526     C     SFLEOF        ANDEQ     0
099100970526     C*
099200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
099300970526     C                   MOVEL     *BLANKS       OPZKEY
099400970526     C*
099500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
099600970526     C                   EXSR      IRESFL
099700970526     C*
099800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
099900970526     C                   ADD       1             SFLULT
100000970526     C*
100100970526     C* SCRITTURA RECORD SFL
100200970528     C                   WRITE     SI17D05
100300970526     C                   ADD       1             SFLRIG
100400970526     C*
100500970526     C* LETTURA FILE DA CARICARE
100600970526     C                   EXSR      LETFIL
100601060706     c* se cambiato tipo, lascio una riga vuota se non ho finito la
100602060706     c* pagina
100603060706     c                   if        caeepa<>videpa     and sfleof=0 and
100604060706     C                             sflrig<=sflpag
100605060706     c                   seton                                        01
100606060706     C                   MOVEL     *BLANKS       OPZKEY
100607060706     c                   clear                   vidatb
100608060706     c                   clear                   videpa
100609060706     c                   clear                   vidtfp
100610060706     c                   clear                   viddtp
100611060706     c                   clear                   vidtfa
100612060706     c                   clear                   viddta
100613060706     c                   clear                   vidtfe
100614060706     c                   clear                   viddte
100615060706     c                   clear                   viddde
100616060706     c                   clear                   viddsc
100621060706     C* SCRITTURA RECORD SFL
100622060706     C                   ADD       1             SFLULT
100623060706     C                   WRITE     SI17D05
100624060706     C                   ADD       1             SFLRIG
100625060706     c                   setoff                                       01
100700060706     C                   ENDif                                                  DO WHILE
100701060706     C                   END                                                    DO WHILE
100800970526     C*
100900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
101000970526     C     WFLULT        IFNE      SFLULT
101100970526     C                   Z-ADD     WFLULT        SFLNBR
101200970526     C                   ADD       1             SFLNBR
101300970526     C                   END
101400970526     C*
101500970526     C                   ENDSR
101600970526     C*------------------------------------------------------------------------*
101700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
101800970526     C*------------------------------------------------------------------------*
101900970526     C     OUTSFL        BEGSR
102000970526     C*
102100970526     C* WRITE RECORD TESTATA VIDEO
102200970528     C                   WRITE     SI17D01
102300970526     C*
102400970526     C* VISUALIZZAZIONE MESSAGGIO
102500970526     C                   EXSR      SNDMSG
102600970526     C*
102700970526     C* WRITE RECORD OPZIONI VIDEO
102800970528     C                   WRITE     SI17D02
102900970526     C*
103000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
103100970528     C                   WRITE     SI17D03
103200970526     C*
103300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
103400970528     C                   WRITE     SI17D04
103500970526     C*
103600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
103700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
103800970526     C     SFLEOF        IFEQ      0
103900970526     C                   MOVEL     '0'           *IN42
104000970526     C                   ELSE
104100970526     C*
104200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
104300970526     C                   MOVEL     '1'           *IN42
104400970526     C                   END
104500970526     C*
104600970526     C* WRITE RECORD TASTI FUNZIONALI
104700970528     C                   WRITE     SI17D09
104800970526     C*
104900970526     C* CASO DI SFL VUOTO
105000970526     C     SFLULT        IFLE      0
105100970526     C*
105200970526     C*    - WRITE RECORD VUOTO
105300970528     C                   WRITE     SI17D0A
105400970528     C                   READ      SI17D0A                                99
105500970526     C*
105600970526     C                   ELSE
105700970526     C*
105800970526     C* CASO DI SFL CON RECORDS
105900970526     C*
106000970526     C*    - WRITE RECORD CONTROLLO SFL
106100970528     C                   WRITE     SI17D06
106200970528     C                   READ      SI17D06                                99
106300970526     C                   END
106400970526     C*
106500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
106600970528     C                   READ      SI17D03                                99
106700970526     C*
106800970526     C                   ENDSR
106900970526     C*------------------------------------------------------------------------*
107000970526     C* SNDMSG - MANDA MESSAGGIO
107100970526     C*------------------------------------------------------------------------*
107200970526     C     SNDMSG        BEGSR
107300970526     C*
107400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
107500970526     C                   MOVEL     '1'           *IN40
107600970528     C                   WRITE     SI17DMC
107700970526     C                   MOVEL     '0'           *IN40
107800970526     C*
107900970526     C* VISUALIZZAZIONE MESSAGGIO
108000970526     C                   Z-ADD     0             MSGULT
108100970526     C                   CALL      'XMSG'        XMSGPA
108200970526     C     DSMSER        IFEQ      0
108300970526     C                   MOVEL     DSMSKE        MSGKEY
108400970526     C                   END
108500970526     C                   Z-ADD     1             MSGULT
108600970528     C                   WRITE     SI17DMS                                      SFL MESS
108700970528     C                   WRITE     SI17DMC                                      CONTR MESS
108800970526     C*
108900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
109000970526     C                   MOVEL     *BLANKS       DSMSMS
109100970526     C*
109200970526     C                   ENDSR
109300970526     C*------------------------------------------------------------------------*
109400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
109500970526     C*------------------------------------------------------------------------*
109600970526     C     FUNCON        BEGSR
109700970526     C*
109800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
109900970526     C                   EXSR      FUNVAR
110000970526     C*
110100970526     C* PULISCO CAMPI DI LAVORO
110200970526     C                   Z-ADD     0             I                 5 0
110300970526     C                   Z-ADD     0             W                 5 0
110400970526     C*
110500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
110600970526     C     FUNKEY        IFNE      *BLANKS
110700970526     C     1             DO        24            I
110800970526     C                   MOVEL     FUD(I)        FUDRIG
110900970526     C*
111000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
111100970526     C     FUNKEY        IFEQ      FUDFUN
111200970526     C     FUDATT        ANDEQ     'S'
111300970526     C                   Z-ADD     I             W
111400970526     C                   END
111500970526     C                   END                                                    DO WHILE
111600970526     C*
111700970526     C* FUNZIONE NON ATTIVA (ERRORE)
111800970526     C     W             IFEQ      0
111900970526     C*
112000970526     C* EMISSIONE MESSAGGIO DI ERRORE
112100970526     C                   MOVEL     MSG(2)        DSMSMS
112200970526     C                   END                                                    W=0
112300970526     C                   END                                                    FUNKEY NE *BLANKS
112400970526     C*
112500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
112600970526     C     W             IFNE      0
112700970526     C     FUNKEY        OREQ      *BLANKS
112800970526     C*
112900970526     C     FUNKEY        IFEQ      *BLANKS
113000970526     C                   EXSR      ENTGES
113100970526     C                   END
113200970526     C     FUNKEY        IFEQ      '03'
113300970526     C                   EXSR      F03GES
113400970526     C                   END
113500970526     C     FUNKEY        IFEQ      '05'
113600970526     C                   EXSR      F05GES
113700970526     C                   END
113800970526     C     FUNKEY        IFEQ      '06'
113900970526     C                   EXSR      F06GES
114000970526     C                   END
114100970526     C     FUNKEY        IFEQ      '12'
114200970526     C                   EXSR      F12GES
114300970526     C                   END
114400970528     C     FUNKEY        IFEQ      '21'
114500970528     C                   EXSR      F21GES
114600970528     C                   END
114700970528     C     FUNKEY        IFEQ      '22'
114800970528     C                   EXSR      F22GES
114900970528     C                   END
115000970526     C     FUNKEY        IFEQ      '23'
115100970526     C                   EXSR      F23GES
115200970526     C                   END
115300970526     C     FUNKEY        IFEQ      '24'
115400970526     C                   EXSR      F24GES
115500970526     C                   END
115600970526     C     FUNKEY        IFEQ      'RU'
115700970526     C                   EXSR      RUPGES
115800970526     C                   END
115900970526     C*
116000970526     C                   END
116100970526     C*
116200970526     C                   ENDSR
116300970526     C*------------------------------------------------------------------------*
116400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
116500970526     C*------------------------------------------------------------------------*
116600970526     C     FUNVAR        BEGSR
116700970526     C*
116800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
116900970526     C     KEY           IFEQ      ENTER
117000970526     C                   MOVEL     *BLANKS       FUNKEY
117100970526     C                   END
117200970526     C     KEY           IFEQ      F1
117300970526     C                   MOVEL     '01'          FUNKEY
117400970526     C                   END
117500970526     C     KEY           IFEQ      F2
117600970526     C                   MOVEL     '02'          FUNKEY
117700970526     C                   END
117800970526     C     KEY           IFEQ      F3
117900970526     C                   MOVEL     '03'          FUNKEY
118000970526     C                   END
118100970526     C     KEY           IFEQ      F4
118200970526     C                   MOVEL     '04'          FUNKEY
118300970526     C                   END
118400970526     C     KEY           IFEQ      F5
118500970526     C                   MOVEL     '05'          FUNKEY
118600970526     C                   END
118700970526     C     KEY           IFEQ      F6
118800970526     C                   MOVEL     '06'          FUNKEY
118900970526     C                   END
119000970526     C     KEY           IFEQ      F7
119100970526     C                   MOVEL     '07'          FUNKEY
119200970526     C                   END
119300970526     C     KEY           IFEQ      F8
119400970526     C                   MOVEL     '08'          FUNKEY
119500970526     C                   END
119600970526     C     KEY           IFEQ      F9
119700970526     C                   MOVEL     '09'          FUNKEY
119800970526     C                   END
119900970526     C     KEY           IFEQ      F10
120000970526     C                   MOVEL     '10'          FUNKEY
120100970526     C                   END
120200970526     C     KEY           IFEQ      F11
120300970526     C                   MOVEL     '11'          FUNKEY
120400970526     C                   END
120500970526     C     KEY           IFEQ      F12
120600970526     C                   MOVEL     '12'          FUNKEY
120700970526     C                   END
120800970526     C     KEY           IFEQ      F13
120900970526     C                   MOVEL     '13'          FUNKEY
121000970526     C                   END
121100970526     C     KEY           IFEQ      F14
121200970526     C                   MOVEL     '14'          FUNKEY
121300970526     C                   END
121400970526     C     KEY           IFEQ      F15
121500970526     C                   MOVEL     '15'          FUNKEY
121600970526     C                   END
121700970526     C     KEY           IFEQ      F16
121800970526     C                   MOVEL     '16'          FUNKEY
121900970526     C                   END
122000970526     C     KEY           IFEQ      F17
122100970526     C                   MOVEL     '17'          FUNKEY
122200970526     C                   END
122300970526     C     KEY           IFEQ      F18
122400970526     C                   MOVEL     '18'          FUNKEY
122500970526     C                   END
122600970526     C     KEY           IFEQ      F19
122700970526     C                   MOVEL     '19'          FUNKEY
122800970526     C                   END
122900970526     C     KEY           IFEQ      F20
123000970526     C                   MOVEL     '20'          FUNKEY
123100970526     C                   END
123200970526     C     KEY           IFEQ      F21
123300970526     C                   MOVEL     '21'          FUNKEY
123400970526     C                   END
123500970526     C     KEY           IFEQ      F22
123600970526     C                   MOVEL     '22'          FUNKEY
123700970526     C                   END
123800970526     C     KEY           IFEQ      F23
123900970526     C                   MOVEL     '23'          FUNKEY
124000970526     C                   END
124100970526     C     KEY           IFEQ      F24
124200970526     C                   MOVEL     '24'          FUNKEY
124300970526     C                   END
124400970526     C     KEY           IFEQ      ROLLUP
124500970526     C                   MOVEL     'RU'          FUNKEY
124600970526     C                   END
124700970526     C     KEY           IFEQ      HELP
124800970526     C                   MOVEL     'HP'          FUNKEY
124900970526     C                   END
125000970526     C*
125100970526     C                   ENDSR
125200970526     C*------------------------------------------------------------------------*
125300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
125400970526     C*------------------------------------------------------------------------*
125500970526     C     FUNGES        BEGSR
125600970526     C*
125700970526     C* PULISCO CAMPI DI LAVORO
125800970526     C                   Z-ADD     0             W                 5 0
125900970526     C*
126000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
126100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
126200970526     C     1             DO        24            I
126300970526     C                   MOVEL     FUD(I)        FUDRIG
126400970526     C*
126500970526     C* PULIZIA FUNZIONI PRECEDENTI
126600970526     C                   MOVEL     *BLANKS       FUV(I)
126700970526     C*
126800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
126900970526     C* (SALTA F23 E F24)
127000970526     C     FUDATT        IFEQ      'S'
127100970526     C     FUDFUN        ANDNE     '23'
127200970526     C     FUDFUN        ANDNE     '24'
127300970526     C                   ADD       1             W
127400970526     C                   MOVEL     FUDDES        FUV(W)
127500970526     C                   END
127600970526     C                   END                                                    DO WHILE
127700970526     C*
127800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
127900970526     C* ALTRIMENTI DISATTIVO F24
128000970526     C     1             DO        24            I
128100970526     C                   MOVEL     FUD(I)        FUDRIG
128200970526     C*
128300970526     C     FUDFUN        IFEQ      '24'
128400970526     C     W             IFGT      6
128500970526     C                   MOVEL     'S'           FUDATT
128600970526     C                   ELSE
128700970526     C                   MOVEL     *BLANKS       FUDATT
128800970526     C                   END                                                    W GT 8
128900970526     C                   MOVEL     FUDRIG        FUD(I)
129000970526     C                   END
129100970526     C                   END                                                    DO WHILE
129200970526     C*
129300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
129400970526     C     W             DIV       6             I
129500970526     C                   MVR                     Z
129600970526     C*
129700970526     C     Z             IFNE      0
129800970526     C     I             ADD       1             Z
129900970526     C                   ELSE
130000970526     C                   Z-ADD     I             Z
130100970526     C                   END
130200970526     C*
130300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
130400970526     C                   ADD       1             FUNGRU
130500970526     C     FUNGRU        IFGT      Z
130600970526     C     FUNGRU        ORLT      1
130700970526     C                   Z-ADD     1             FUNGRU
130800970526     C                   END
130900970526     C*
131000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
131100970526     C     FUNGRU        IFEQ      1
131200970526     C                   MOVEL     FUV(1)        FUN001
131300970526     C                   MOVEL     FUV(2)        FUN002
131400970526     C                   MOVEL     FUV(3)        FUN003
131500970526     C                   MOVEL     FUV(4)        FUN004
131600970526     C                   MOVEL     FUV(5)        FUN005
131700970526     C                   MOVEL     FUV(6)        FUN006
131800970526     C                   END
131900970526     C*
132000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
132100970526     C     FUNGRU        IFEQ      2
132200970526     C                   MOVEL     FUV(7)        FUN001
132300970526     C                   MOVEL     FUV(8)        FUN002
132400970526     C                   MOVEL     FUV(9)        FUN003
132500970526     C                   MOVEL     FUV(10)       FUN004
132600970526     C                   MOVEL     FUV(11)       FUN005
132700970526     C                   MOVEL     FUV(12)       FUN006
132800970526     C                   END
132900970526     C*
133000970526     C* TERZO GRUPPO DI 6 FUNZIONI
133100970526     C     FUNGRU        IFEQ      3
133200970526     C                   MOVEL     FUV(13)       FUN001
133300970526     C                   MOVEL     FUV(14)       FUN002
133400970526     C                   MOVEL     FUV(15)       FUN003
133500970526     C                   MOVEL     FUV(16)       FUN004
133600970526     C                   MOVEL     FUV(17)       FUN005
133700970526     C                   MOVEL     FUV(18)       FUN006
133800970526     C                   END
133900970526     C*
134000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
134100970526     C     FUNGRU        IFEQ      4
134200970526     C                   MOVEL     FUV(19)       FUN001
134300970526     C                   MOVEL     FUV(20)       FUN002
134400970526     C                   MOVEL     FUV(21)       FUN003
134500970526     C                   MOVEL     FUV(22)       FUN004
134600970526     C                   MOVEL     *BLANKS       FUN005
134700970526     C                   MOVEL     *BLANKS       FUN006
134800970526     C                   END
134900970526     C*
135000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
135100970526     C                   MOVEL     *BLANKS       FUN007
135200970526     C                   MOVEL     *BLANKS       FUN008
135300970526     C*
135400970526     C     1             DO        24            I
135500970526     C                   MOVEL     FUD(I)        FUDRIG
135600970526     C     FUDFUN        IFEQ      '23'
135700970526     C     FUDATT        ANDEQ     'S'
135800970526     C                   MOVEL     FUDDES        FUN007
135900970526     C                   END
136000970526     C     FUDFUN        IFEQ      '24'
136100970526     C     FUDATT        ANDEQ     'S'
136200970526     C                   MOVEL     FUDDES        FUN008
136300970526     C                   END
136400970526     C                   END                                                    DO
136500970526     C*
136600970526     C                   ENDSR
136700970526     C*------------------------------------------------------------------------*
136800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
136900970526     C*------------------------------------------------------------------------*
137000970526     C     OPZGES        BEGSR
137100970526     C*
137200970526     C* PULISCO CAMPI DI LAVORO
137300970526     C                   Z-ADD     0             W                 5 0
137400970526     C*
137500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
137600970526     C* E CARICO TABELLA OPZIONI ATTIVE
137700970526     C     1             DO        24            I
137800970526     C                   MOVEL     OPD(I)        OPDRIG
137900970526     C*
138000970526     C* PULIZIA OPZIONI PRECEDENTI
138100970526     C                   MOVEL     *BLANKS       OPV(I)
138200970526     C*
138300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
138400970526     C     OPDATT        IFEQ      'S'
138500970526     C                   ADD       1             W
138600970526     C                   MOVEL     OPDDES        OPV(W)
138700970526     C                   END
138800970526     C                   END                                                    DO WHILE
138900970526     C*
139000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
139100970526     C* ALTRIMENTI DISATTIVO F23
139200970526     C     1             DO        24            I
139300970526     C                   MOVEL     FUD(I)        FUDRIG
139400970526     C*
139500970526     C     FUDFUN        IFEQ      '23'
139600970526     C     W             IFGT      8
139700970526     C                   MOVEL     'S'           FUDATT
139800970526     C                   ELSE
139900970526     C                   MOVEL     *BLANKS       FUDATT
140000970526     C                   END                                                    W GT 8
140100970526     C                   MOVEL     FUDRIG        FUD(I)
140200970526     C                   END
140300970526     C                   END                                                    DO WHILE
140400970526     C*
140500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
140600970526     C     W             DIV       8             I
140700970526     C                   MVR                     Z                 5 0
140800970526     C*
140900970526     C     Z             IFNE      0
141000970526     C     I             ADD       1             Z
141100970526     C                   ELSE
141200970526     C                   Z-ADD     I             Z
141300970526     C                   END
141400970526     C*
141500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
141600970526     C                   ADD       1             OPZGRU
141700970526     C     OPZGRU        IFGT      Z
141800970526     C     OPZGRU        ORLT      1
141900970526     C                   Z-ADD     1             OPZGRU
142000970526     C                   END
142100970526     C*
142200970526     C* PRIMO GRUPPO DI 8 OPZIONI
142300970526     C     OPZGRU        IFEQ      1
142400970526     C                   MOVEL     OPV(1)        OPZ001
142500970526     C                   MOVEL     OPV(2)        OPZ002
142600970526     C                   MOVEL     OPV(3)        OPZ003
142700970526     C                   MOVEL     OPV(4)        OPZ004
142800970526     C                   MOVEL     OPV(5)        OPZ005
142900970526     C                   MOVEL     OPV(6)        OPZ006
143000970526     C                   MOVEL     OPV(7)        OPZ007
143100970526     C                   MOVEL     OPV(8)        OPZ008
143200970526     C                   END
143300970526     C*
143400970526     C* SECONDO GRUPPO DI 8 OPZIONI
143500970526     C     OPZGRU        IFEQ      2
143600970526     C                   MOVEL     OPV(9)        OPZ001
143700970526     C                   MOVEL     OPV(10)       OPZ002
143800970526     C                   MOVEL     OPV(11)       OPZ003
143900970526     C                   MOVEL     OPV(12)       OPZ004
144000970526     C                   MOVEL     OPV(13)       OPZ005
144100970526     C                   MOVEL     OPV(14)       OPZ006
144200970526     C                   MOVEL     OPV(15)       OPZ007
144300970526     C                   MOVEL     OPV(16)       OPZ008
144400970526     C                   END
144500970526     C*
144600970526     C* TERZO GRUPPO DI 8 OPZIONI
144700970526     C     OPZGRU        IFEQ      3
144800970526     C                   MOVEL     OPV(17)       OPZ001
144900970526     C                   MOVEL     OPV(18)       OPZ002
145000970526     C                   MOVEL     OPV(19)       OPZ003
145100970526     C                   MOVEL     OPV(20)       OPZ004
145200970526     C                   MOVEL     OPV(21)       OPZ005
145300970526     C                   MOVEL     OPV(22)       OPZ006
145400970526     C                   MOVEL     OPV(23)       OPZ007
145500970526     C                   MOVEL     OPV(24)       OPZ008
145600970526     C                   END
145700970526     C*
145800970526     C                   ENDSR
145900970526     C*------------------------------------------------------------------------*
146000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
146100970526     C*------------------------------------------------------------------------*
146200970526     C     OPZCON        BEGSR
146300970526     C*
146400970526     C* PULISCO CAMPI DI LAVORO
146500970526     C                   Z-ADD     0             I                 5 0
146600970526     C                   Z-ADD     0             W                 5 0
146700970526     C*
146800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
146900970526     C     1             DO        24            I
147000970526     C                   MOVEL     OPD(I)        OPDRIG
147100970526     C*
147200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
147300970526     C     OPZKEY        IFEQ      OPDOPZ
147400970526     C     OPZKEY        OREQ      OPDOP1
147500970526     C*
147600970526     C     OPDATT        IFEQ      'S'
147700970526     C                   MOVEL     OPDOPZ        OPZKEY
147800970526     C                   Z-ADD     I             W
147900970526     C                   END
148000970526     C                   END
148100970526     C*
148200970526     C                   END                                                    DO WHILE
148300970526     C*
148400970526     C* OPZIONE NON ATTIVA (ERRORE)
148500970526     C     W             IFEQ      0
148600970526     C*
148700970526     C* OPZIONE NON OK
148800970526     C                   Z-ADD     1             OPZOK
148900970526     C*
149000970526     C* EMISSIONE MESSAGGIO DI ERRORE
149100970526     C                   MOVEL     MSG(1)        DSMSMS
149200970526     C                   ELSE
149300970526     C*
149400970526     C* OPZIONE ATTIVA (PROCEDI ...)
149500970526     C*
149600970526     C     OPZKEY        IFEQ      ' 1'
149700970526     C                   EXSR      O01GES
149800970526     C                   END
149900970526     C     OPZKEY        IFEQ      ' 2'
150000970526     C                   EXSR      O02GES
150100970526     C                   END
150200970526     C     OPZKEY        IFEQ      ' 4'
150300970526     C                   EXSR      O04GES
150400970526     C                   END
150500970526     C     OPZKEY        IFEQ      ' 5'
150600970526     C                   EXSR      O05GES
150700970526     C                   END
150800970526     C     OPZKEY        IFEQ      ' 7'
150900970526     C                   EXSR      O07GES
151000970526     C                   END
151100970526     C                   END                                                    OPZ.NON ATTIVA
151200970526     C*
151300970526     C                   ENDSR
151400970526     C*------------------------------------------------------------------------*
151500970526     C* NOTE COSTRUZIONE SCHIERE
151600970526     C*------------------------------------------------------------------------*
151700970526     C*
151800970526     C* -MSG-MESSAGGI
151900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
152000970526     C* (NON TOCCARE MSG,1 E MSG,2)
152100970526     C*
152200970526     C* -CMD-COMANDI
152300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
152400970526     C*
152500970526     C* -OPD-OPZIONI
152600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
152700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
152800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
152900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
153000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
153100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
153200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
153300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
153400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
153500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
153600970526     C*
153700970526     C* -FUD-FUNZIONI
153800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
153900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
154000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
154100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
154200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
154300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
154400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
154500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
154600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
154700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
154800970526     O*------------------------------------------------------------------------*
154900970526** DEC - DECODIFICHE
155000970526    GESTIONE                                                     1
155100970526INTERROGAZIONE                                                   2
155200970530     SCELTA                                                      3
1553009705264
1554009705265
155500970526** MSG - MESSAGGI
155600970528TISI17R- Opzione non gestita                                                   1
155700970528TISI17R- Tasto funzionale non gestito                                          2
155800970528TISI17R- Non abilitato alla gestione di questo CAP                             3
155900970528TISI17R- Record annullato. Consentito solo il ripristino                       4
156000970528TISI17R- Record non annullato. Impossibile ripristinare                        5
156100970528TISI17R- Accettato solo un parametro di inizio lista                           6
156200970528TISI17R- 7                                                                     7
156300970528TISI17R- 8                                                                     8
156400970528TISI17R- 9                                                                     9
156500970528TISI17R- 10                                                                   10
156600970530TISI17R- 11
156700970528TISI17R- Data errata                                                          12
156800970528TISI17R- 13                                                                   13
156900970528TISI17R- 14                                                                   14
157000970528TISI17R- 15                                                                   15
157100970528TISI17R- 16                                                                   16
157200970528TISI17R- 17                                                                   17
157300970528TISI17R- 18                                                                   18
157400970528TISI17R- 19                                                                   19
157500970528TISI17R- 20                                                                   20
157600970528** CMD - COMANDI CL
157700970528WRKSPLF
157800970528
157900970528
158000970528
158100970528
158200970526** OPD - OPZIONI
158300970526 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
158400970526 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
158500970526                   -3 -     (S/N)=N                        opz. 03¬
158600970526 4=Annullamento    -4 -     (S/N)=N                        opz. 04¬
158700970526 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
158800970526                   -6 -     (S/N)=N                        opz. 06¬
158900970526 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
159000970527                   -8 -     (S/N)=N                        opz. 08¬
159100970527                   -9 -     (S/N)=N                        opz. 09¬
159200970527                   -10-     (S/N)=N                        opz. 10¬
159300970526                   -  -     (S/N)=N                        opz. 11¬
159400970526                   -  -     (S/N)=N                        opz. 12¬
159500970526                   -  -     (S/N)=N                        opz. 13¬
159600970526                   -  -     (S/N)=N                        opz. 14¬
159700970526                   -  -     (S/N)=N                        opz. 15¬
159800970526                   -  -     (S/N)=N                        opz. 16¬
159900970526                   -  -     (S/N)=N                        opz. 17¬
160000970526                   -  -     (S/N)=N                        opz. 18¬
160100970526                   -  -     (S/N)=N                        opz. 19¬
160200970526                   -  -     (S/N)=N                        opz. 20¬
160300970526                   -  -     (S/N)=N                        opz. 21¬
160400970526                   -  -     (S/N)=N                        opz. 22¬
160500970526                   -  -     (S/N)=N                        opz. 23¬
160600970526                   -  -     (S/N)=N                        opz. 24¬
160700970526** FUD - FUNZIONI
160800970526                   -        (S/N)=N                        funz.01¬
160900970526                   -        (S/N)=N                        funz.02¬
161000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
161100970526                   -        (S/N)=N                        funz.04¬
161200970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
161300020506F06=Inserimento    -        (S/N)=S                        funz.06¬
161400970526                   -        (S/N)=N                        funz.07¬
161500970527                   -        (S/N)=N                        funz.08¬
161600970527                   -        (S/N)=N                        funz.09¬
161700970527                   -        (S/N)=N                        funz.10¬
161800970526                   -        (S/N)=N                        funz.11¬
161900970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
162000970526                   -        (S/N)=N                        funz.13¬
162100970526                   -        (S/N)=N                        funz.14¬
162200970526                   -        (S/N)=N                        funz.15¬
162300970526                   -        (S/N)=N                        funz.16¬
162400970526                   -        (S/N)=N                        funz.17¬
162500970526                   -        (S/N)=N                        funz.18¬
162600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
162700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
162800980512F21=Stampa elenco  -        (S/N)=S                        funz.21¬
162900970626F22=Gestione stampe-        (S/N)=S                        funz.22¬
163000970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
163100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
