000100001123      *PARMS COMMIT(*NONE) DATFMT(*ISO)
000200001123      *PARMS CLOSQLCSR(*ENDMOD)
000300010925      *PARMS DYNUSRPRF(*OWNER)
000400970526     H*------------------------------------------------------------------------*
000500001030     H* VERIFICA LOCALITA' INESISTENTI (Da WFLOC00F)
000600970526     H*------------------------------------------------------------------------*
000700970526     H DECEDIT('0,') DATEDIT(*DMY.)
000800970526     F*------------------------------------------------------------------------*
000900970526     F* DATA BASE
001000970526     F*------------------------------------------------------------------------*
001100001116     FAZCPR01L  IF   E           K DISK
001200120523     FAZCPL01L  IF   E           K DISK
001300120523     FAZCPC01L  IF   E           K DISK
001400001030     FTISI77D   CF   E             WORKSTN INFDS(DEVDS1)
001500001030     F                                     SFILE(SI77D05:SFLULT)
001600001030     F                                     SFILE(SI77DMS:MSGULT)
001700970526     D*------------------------------------------------------------------------*
001800970526     D* SCHIERE
001900970526     D*------------------------------------------------------------------------*
002000970526     D*------------
002100970526     D* DECODIFICHE
002200970526     D*------------
002300970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002400970526     D*------------
002500970526     D* MESSAGGI
002600970526     D*------------
002700970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002800970526     D*------------
002900970526     D* DESCRIZIONI OPZIONI
003000970526     D*------------
003100970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI TASTI FUNZIONALI
003400970526     D*------------
003500970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* OPZIONI DA CARICARE A VIDEO
003800970526     D*------------
003900970526     D OPV             S             19    DIM(24)
004000970526     D*------------
004100970526     D* FUNZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970527     D FUV             S             19    DIM(24)
004400970526     D*------------------------------------------------------------------------*
004500970526     D* INPUT
004600970526     D*------------------------------------------------------------------------*
004700970526     D*------------
004800970526     D* RIDEFINIZIONE OPZIONI VIDEO
004900970526     D*------------
005000970526     D OPDDS           DS
005100970526     D  OPDOPZ                 1      2
005200970526     D  OPDDE1                 3     19
005300970526     D  OPDOP1                21     22
005400970526     D  OPDFI0                23     34
005500970526     D  OPDATT                35     35
005600970526     D  OPDFI1                36     80
005700970526     D  OPDDES                 1     19
005800970526     D  OPDRIG                 1     80
005900970526     D*------------
006000970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006100970526     D*------------
006200970526     D FUDDS           DS
006300970526     D  FUDDE1                 1      1
006400970526     D  FUDFUN                 2      3
006500970526     D  FUDDE2                 4     19
006600970526     D  FUDFI0                20     34
006700970526     D  FUDATT                35     35
006800970526     D  FUDFI1                36     80
006900970526     D  FUDDES                 1     19
007000970526     D  FUDRIG                 1     80
007100970526     D*------------
007200970526     D* DS GESTIONE TASTI FUNZIONALI
007300970526     D*------------
007400970526     D DSKEY         E DS
007500970526     D DEVDS1          DS
007600970526     D  KEY                  369    369
007700970526     D*------------
007800970526     D* DS "XMSG" - GESTIONE MESSAGGI
007900970526     D*------------
008000970526     D DSMSG         E DS
008100970527     D*------------------
008200001030     D* DS DI PROCEDURA
008300970526     D*------------
008400970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
008500970526     D*------------
008600970526     D* ARCHITETTURA
008700970526     D*------------
008800970526     D KPJBA         E DS
008900001030     D*------------
009000001030     D* DEFINIZIONE DS PER LETTURA VIA SQL
009100001030     D*------------
009200001030     D DSWFLOC         DS
009300001030     D  WFLNAR                 1      3
009400001030     D  WFLPRV                 4      5
009500001030     D  WFLCAP                 6     14
009600001030     D  WFLLOC                15     49
009700001106     D  WFLSPE                50     58  0
009800001030     D*-------------------
009900001030     D* DS "XSRDA8" - CONTROLLO DATA(8)
010000001030     D*-------------------
010100001030     D WLBDA8          DS
010200001030     D  G08DAT                 1      8  0
010300001030     D  G08INV                 9     16  0
010400001030     D  G08ERR                17     17
010500001030     D  G08TGI                18     22  0
010600120523     D*-------------------
010700120523     D* variabili di appoggio
010800120523     D Versione        s              5s 0
010900970526     C*------------------------------------------------------------------------*
011000970526     C* MAIN LINES
011100970526     C*------------------------------------------------------------------------*
011200970526     C*
011300970526     C* SE NON CI SONO ERRORI, PROSEGUE
011400001030     C     ERRL00        IFNE      '1'
011500970526     C*
011600970526     C* CARICA IL SFILE
011700970526     C                   EXSR      POSSFL
011800970526     C*
011900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
012000001030     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
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
013300001030     C                   ENDDO
013400970527     C*
013500970527     C* CI SONO ERRORI IN ENTRATA
013600001030     C                   ELSE
013700970527     C                   MOVEL     '1'           D00ERR
013800970527     C                   MOVEL     DSMSMS        D00MSG
013900001030     C                   ENDIF
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
016500970526     C*
016600970526     C                   ENDSR
016700970526     C*------------------------------------------------------------------------*
016800970526     C* F12GES - GESTIONE F12 -> RITORNO
016900970526     C*------------------------------------------------------------------------*
017000970526     C     F12GES        BEGSR
017100970526     C*
017200970526     C* IMPOSTA "PREMUTO F12"
017300970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
017400970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
017500970526     C*
017600970526     C                   ENDSR
017700970529     C*------------------------------------------------------------------------*
017800970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
017900970529     C*------------------------------------------------------------------------*
018000970529     C     F13GES        BEGSR
018100970529     C*
018200970529     C*
018300970529     C                   ENDSR
018400970526     C*------------------------------------------------------------------------*
018500970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
018600970526     C*------------------------------------------------------------------------*
018700970526     C     F23GES        BEGSR
018800970526     C*
018900970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
019000970526     C*
019100970526     C                   ENDSR
019200970526     C*------------------------------------------------------------------------*
019300970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
019400970526     C*------------------------------------------------------------------------*
019500970526     C     F24GES        BEGSR
019600970526     C*
019700970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
019800970526     C*
019900970526     C                   ENDSR
020000970526     C*------------------------------------------------------------------------*
020100970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
020200970526     C*------------------------------------------------------------------------*
020300970526     C     RUPGES        BEGSR
020400970526     C*
020500970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
020600970526     C*
020700970526     C                   ENDSR
020800970526     C*------------------------------------------------------------------------*
020900970526     C* ENTGES - GESTIONE ENTER -> INVIO
021000970526     C*------------------------------------------------------------------------*
021100970526     C     ENTGES        BEGSR
021200970526     C*
021300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
021400970526     C                   EXSR      NEWPOS
021500970526     C*
021600001106     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
021700001030     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021800001030     C     SWPOSI        IFNE      0
021900970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
022000001030     C                   ELSE
022100001030     C     SFLULT        IFNE      0
022200970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
022300970526     C*
022400970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
022500001030     C     SWMODI        IFNE      0
022600970526     C                   EXSR      POSSFL
022700001030     C                   ENDIF
022800001030     C                   ENDIF
022900001030     C                   ENDIF
023000001030     C                   ENDIF
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400001030     C* O01GES - GESTIONE OPZIONE 1=SCELTA
023500970526     C*------------------------------------------------------------------------*
023600970526     C     O01GES        BEGSR
023700970526     C*
023800970526     C*
023900970526     C                   ENDSR
024000001106     C*------------------------------------------------------------------------*
024100001106     C* O08GES - GESTIONE OPZIONE 8=VERIFICA X LOCALITA'
024200001106     C*------------------------------------------------------------------------*
024300001106     C     O08GES        BEGSR
024400001106     C*
024500001106     C* VERIFICA
024600001106     C                   CLEAR                   DSSI00
024700001106     C                   MOVEL     VIDNAR        DNAR
024800001106     C                   MOVEL     *BLANKS       DCAP
024900001106     C                   MOVEL     VIDLOC        DLOC
025000001106     C                   MOVEL     VIDLOC        DSIN
025100001106     C                   MOVEL     DEPOP0        WOP0
025200001106     C                   MOVEL     'S'           WOP0
025300001106     C                   MOVEL     'O08'         WOP1
025400001106     C                   MOVEL     'TISI35R'     WPGM
025500001106     C                   EXSR      LANPGM
025600001106     C*
025700001106     C                   ENDSR
025800970526     C*------------------------------------------------------------------------*
025900001106     C* O09GES - GESTIONE OPZIONE 9=VERIFICA X CAP
026000970526     C*------------------------------------------------------------------------*
026100001031     C     O09GES        BEGSR
026200970526     C*
026300001030     C* VERIFICA
026400001030     C                   CLEAR                   DSSI00
026500001030     C                   MOVEL     VIDNAR        DNAR
026600001106     C                   MOVEL     VIDCAP        DCAP
026700001106     C                   MOVEL     *BLANKS       DLOC
026800001106     C                   MOVEL     VIDLOC        DSIN
026900001030     C                   MOVEL     DEPOP0        WOP0
027000001030     C                   MOVEL     'S'           WOP0
027100001031     C                   MOVEL     'O09'         WOP1
027200001030     C                   MOVEL     'TISI35R'     WPGM
027300001030     C                   EXSR      LANPGM
027400970526     C*
027500970526     C                   ENDSR
027600970611     C*------------------------------------------------------------------------*
027700970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
027800970611     C*------------------------------------------------------------------------*
027900970611     C     O03GES        BEGSR
028000970611     C*
028100970611     C*
028200970611     C                   ENDSR
028300970526     C*------------------------------------------------------------------------*
028400970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
028500970526     C*------------------------------------------------------------------------*
028600970526     C     O04GES        BEGSR
028700970526     C*
028800970526     C*
028900970526     C                   ENDSR
029000970526     C*------------------------------------------------------------------------*
029100970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
029200970526     C*------------------------------------------------------------------------*
029300970526     C     O05GES        BEGSR
029400970526     C*
029500970526     C*
029600970526     C                   ENDSR
029700970526     C*------------------------------------------------------------------------*
029800970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
029900970526     C*------------------------------------------------------------------------*
030000970526     C     O07GES        BEGSR
030100970526     C*
030200001030     C*
030300970526     C                   ENDSR
030400970529     C*------------------------------------------------------------------------*
030500970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
030600970529     C*------------------------------------------------------------------------*
030700970529     C     O13GES        BEGSR
030800970529     C*
030900970529     C*
031000970529     C                   ENDSR
031100970526     C*------------------------------------------------------------------------*
031200001030     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
031300970526     C*------------------------------------------------------------------------*
031400970526     C     NEWPOS        BEGSR
031500970526     C*
031600970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
031700970526     C*         1= RICHIESTO POSIZIONAMENTO
031800970526     C                   Z-ADD     0             SWPOSI            7 0
031900970526     C*
032000970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
032100970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
032200970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
032300970526     C                   Z-ADD     0             SWMODI            1 0
032400970530     C*
032500970530     C* REIMPOSTA INDICATORI DI ERRORE
032600970530     C                   SETOFF                                       808182
032700970526     C*
032800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
032900001030     C     VIDPNA        IFNE      DEPNAR
033000970527     C     VIDPCA        ORNE      DEPCAP
033100970529     C     VIDPLO        ORNE      DEPLOC
033200001106     C     VIDRIC        ORNE      DEPRIC
033300970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
033400001030     C     ERRL00        IFEQ      '0'                                          *NO ERORI
033500970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
033600970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
033700970527     C                   MOVEL     VIDPCA        DEPCAP
033800970529     C                   MOVEL     VIDPLO        DEPLOC
033900001106     C                   MOVEL     VIDRIC        DEPRIC
034000001030     C                   ENDIF
034100001030     C                   ENDIF
034200970526     C*
034300970526     C                   ENDSR
034400970526     C*------------------------------------------------------------------------*
034500970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
034600970526     C*------------------------------------------------------------------------*
034700970526     C     SETFIL        BEGSR
034800001030     C*
034900001030     C                   SETOFF                                       99
035000970529     C*
035100970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
035200001030     C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
035300001030     C/EXEC SQL
035400010713     C+ DECLARE C1 SCROLL CURSOR FOR
035500010713     C+ SELECT DISTINCT WFLNAR, WFLPRV, WFLCAP, WFLLOC, SUM(WFLSPE)
035600010713     C+ FROM WFLOC00F WHERE WFLNAR='   ' AND WFLLIA<'3'
035700010713     C+ GROUP BY WFLNAR, WFLPRV, WFLCAP, WFLLOC
035800010713     C+ ORDER BY WFLLOC
035900001030     C/END-EXEC
036000001030     C
036100001030     C                   IF        CHKCURSOR = '0'
036200001030     C/EXEC SQL
036300001030     C+ Open C1
036400001030     C/END-EXEC
036500001030     C                   MOVEL     '1'           CHKCURSOR
036600001030     C                   ELSE
036700001030     C/EXEC SQL
036800001030     C+ Fetch before from C1
036900001030     C/END-EXEC
037000001030     C                   ENDIF
037100001030     C                   ENDIF
037200001030     C*
037300001030     C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
037400001030     C/EXEC SQL
037500010713     C+ DECLARE C2 SCROLL CURSOR FOR
037600010713     C+ SELECT DISTINCT WFLNAR, WFLPRV, WFLCAP, WFLLOC, SUM(WFLSPE)
037700010713     C+ FROM WFLOC00F WHERE WFLNAR='   ' AND WFLLIA<'3'
037800010713     C+ GROUP BY WFLNAR, WFLPRV, WFLCAP, WFLLOC
037900010713     C+ ORDER BY WFLLOC
038000001030     C/END-EXEC
038100001030     C
038200001030     C                   IF        CHKCURSOR = '0'
038300001030     C/EXEC SQL
038400001030     C+ Open C2
038500001030     C/END-EXEC
038600001030     C                   MOVEL     '1'           CHKCURSOR
038700001030     C                   ELSE
038800001030     C/EXEC SQL
038900001030     C+ Fetch before from C2
039000001030     C/END-EXEC
039100001030     C                   ENDIF
039200001030     C                   ENDIF
039300001030     C*
039400970526     C* LETTURA RECORD
039500001030     C     SQLCOD        IFEQ      *ZEROS
039600970526     C                   EXSR      LETFIL
039700001030     C                   ENDIF
039800970526     C*
039900970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
040000001030     C     *IN99         IFEQ      *ON
040100970526     C                   Z-ADD     1             SFLEOF
040200001030     C                   ENDIF
040300970526     C*
040400970526     C                   ENDSR
040500970526     C*------------------------------------------------------------------------*
040600001030     C* LETFIL - LETTURA RECORD
040700970526     C*------------------------------------------------------------------------*
040800970526     C     LETFIL        BEGSR
040900970526     C*
041000970526     C* LEGGE FINO A:
041100970526     C*     - FINE FILE
041200970526     C*     - TROVATO RECORD VALIDO
041300970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
041400001030     C     *IN99         DOWEQ     *OFF
041500001030     C     WRECOK        ANDEQ     'N'
041600970529     C*
041700970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
041800001030     C     SQLCOD        IFEQ      *ZEROS
041900001030     C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
042000001030     C/EXEC SQL
042100001030     C+ Fetch C1 into :dswfloc
042200001030     C/END-EXEC
042300001030     C                   ENDIF
042400001030     C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
042500001030     C/EXEC SQL
042600001030     C+ Fetch C2 into :dswfloc
042700001030     C/END-EXEC
042800001030     C                   ENDIF
042900001030     C                   ELSE
043000001030     C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
043100001030     C/EXEC SQL
043200001030     C+ Close C1
043300001030     C/END-EXEC
043400001030     C                   MOVEL     '0'           CHKCURSOR
043500001030     C                   ENDIF
043600001030     C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
043700001030     C/EXEC SQL
043800001030     C+ Close C2
043900001030     C/END-EXEC
044000001030     C                   MOVEL     '0'           CHKCURSOR
044100001030     C                   ENDIF
044200001030     C                   ENDIF
044300001030     C*
044400001030     C     SQLCOD        IFNE      *ZEROS
044500001030     C                   SETON                                        99
044600001030     C                   ENDIF
044700001030     C*
044800001030     C     *IN99         IFEQ      *ON                                          *FINE FILE
044900970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
045000001030     C                   ELSE
045100970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
045200001030     C                   ENDIF
045300001030     C                   ENDDO
045400970526     C*
045500970526     C                   ENDSR
045600970526     C*------------------------------------------------------------------------*
045700970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
045800970526     C*------------------------------------------------------------------------*
045900970526     C     CHKREC        BEGSR
046000970526     C*
046100970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
046200970530     C*
046300001106     C* SE RICHIESTO ORDINAMENTO O SELEZIONE FILTRA I RECORD
046400001106     C                   MOVE(P)   VIDRIC        NUMRIC            9 0
046500001106     C     WRECOK        IFEQ      'S'
046600001106     C     NUMRIC        ANDNE     *ZEROS
046700001106     C     NUMRIC        ANDGT     WFLSPE
046800001106     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046900001106     C                   ENDIF
047000001031     C     WRECOK        IFEQ      'S'
047100001031     C     VIDPNA        ANDNE     *BLANKS
047200001031     C     VIDPNA        ANDGT     WFLNAR
047300001031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047400001031     C                   ENDIF
047500001031     C     WRECOK        IFEQ      'S'
047600001031     C     VIDPCA        ANDNE     *BLANKS
047700001031     C     VIDPCA        ANDGT     WFLCAP
047800001031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047900001031     C                   ENDIF
048000001031     C     WRECOK        IFEQ      'S'
048100001031     C     VIDPLO        ANDNE     *BLANKS
048200001031     C     VIDPLO        ANDGT     WFLLOC
048300001031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
048400001031     C                   ENDIF
048500001005     C*
048600970526     C                   ENDSR
048700970526     C*------------------------------------------------------------------------*
048800001030     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
048900970526     C*------------------------------------------------------------------------*
049000970526     C     IRESFL        BEGSR
049100970529     C*
049200970529     C                   MOVEL     *BLANKS       OPZKEY
049300001030     C                   MOVEL     WFLNAR        VIDNAR
049400001030     C                   MOVEL     WFLPRV        VIDPRV
049500120523     C                   MOVEL     WFLCAP        VIDCAP
049600120523     C* reperisco se oboleto
049700120523     C                   EVAL      VIDCFI1 = *blank
049800120523     C                   EVAL      CPCVER = Versione
049900120523     C                   EVAL      CPCNAR = VIDNAR
050000120523     C                   EVAL      CPCCAP = VIDCAP
050100120523     C     K03CPC01      CHAIN     AZCPC000
050200120523     C                   IF        %found
050300120523     C                   EVAL      VIDCFI1 = %subst(CPCFI1 : 1 : 1)
050400120523     C                   ENDIF
050500001030     C                   MOVEL     WFLLOC        VIDLOC
050600120523     C* reperisco se oboleto
050700120523     C                   EVAL      VIDLFI1 = *blank
050800120523     C                   EVAL      CPLVER = Versione
050900120523     C                   EVAL      CPLNAR = VIDNAR
051000120523     C                   EVAL      CPLCAP = VIDCAP
051100120523     C                   EVAL      CPLLOC = WFLLOC
051200120523     C     K04CPL01      CHAIN     AZCPL000
051300120523     C                   IF        %found
051400120523     C                   EVAL      VIDLFI1 = %subst(CPLFI1 : 1 : 1)
051500120523     C                   ENDIF
051600001106     C                   Z-ADD     WFLSPE        VIDSPE
051700970526     C*
051800970526     C                   ENDSR
051900970526     C*------------------------------------------------------------------------*
052000970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
052100970526     C*------------------------------------------------------------------------*
052200970526     C     CHKPAR        BEGSR
052300970529     C*
052400970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
052500001030     C     VIDPCA        IFEQ      *BLANKS
052600970529     C     VIDPLO        ANDEQ     *BLANKS
052700970529     C                   MOVEL     'CAP'         WTIPRI            3
052800001030     C                   ENDIF
052900970529     C*
053000970529     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
053100001030     C     ERRL00        IFEQ      '0'
053200001030     C     VIDPCA        IFNE      *BLANKS
053300970529     C     VIDPLO        ANDNE     *BLANKS
053400970529     C                   MOVEL     MSG(6)        DSMSMS
053500970529     C                   MOVEL     '1'           ERRL00
053600970529     C                   SETON                                        808182
053700001030     C                   ENDIF
053800001030     C                   ENDIF
053900970529     C*
054000970529     C* CONTROLLA IL TIPO RICERCA
054100001030     C     ERRL00        IFEQ      '0'
054200001030     C     VIDPCA        IFNE      *BLANKS
054300970529     C                   MOVEL     'CAP'         WTIPRI
054400001030     C                   ENDIF
054500001030     C     VIDPLO        IFNE      *BLANKS
054600970529     C                   MOVEL     'LOC'         WTIPRI
054700001030     C                   ENDIF
054800001030     C                   ENDIF
054900970526     C*
055000970526     C                   ENDSR
055100970526     C*------------------------------------------------------------------------*
055200001030     C* LANPGM - LANCIA UN PROGRAMMA
055300970526     C*------------------------------------------------------------------------*
055400970526     C     LANPGM        BEGSR
055500970526     C*
055600970526     C* IMPOSTA LA DS DI PROCEDURA
055700970529     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
055800970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
055900970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
056000970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
056100970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
056200970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
056300970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
056400970602     C                   MOVEL     DCAP          D00CAP                         *CAP
056500001106     C                   MOVEL     DSIN          D00SIN                         *SINONIMO
056600001106     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
056700001116     C                   Z-ADD     DATRIF        D00DRI
056800970603     C                   Z-ADD     *ZEROS        D00TTC
056900970530     C                   MOVEL     *BLANKS       D00TSP
057000970530     C                   Z-ADD     *ZEROS        D00TFP
057100970530     C                   MOVEL     *BLANKS       D00EPA
057200970530     C                   Z-ADD     *ZEROS        D00ETP
057300970530     C                   Z-ADD     *ZEROS        D00ETA
057400970530     C                   Z-ADD     *ZEROS        D00EDD
057500970530     C                   MOVEL     *BLANKS       D00SPT
057600970530     C                   Z-ADD     *ZEROS        D00SLP
057700970530     C                   Z-ADD     *ZEROS        D00SLA
057800970530     C                   MOVEL     *BLANKS       D00STS
057900970530     C                   Z-ADD     *ZEROS        D00SDD
058000970526     C*
058100970526     C                   CALL      WPGM
058200970612     C                   PARM                    KPJBA
058300970612     C                   PARM                    DSSI00
058400970526     C*
058500970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
058600001030     C     D00F03        IFEQ      '1'                                          *PREMUTO F03
058700970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
058800970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
058900001030     C                   ELSE                                                   *NON PREMUTO F03
059000970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
059100001030     C                   ENDIF
059200970526     C*
059300970526     C                   ENDSR
059400970526     C*------------------------------------------------------------------------*
059500970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
059600970526     C*------------------------------------------------------------------------*
059700970526     C     CHKFOP        BEGSR
059800970626     C*
059900001031     C* ABILITA SEMPRE E COMUNQUE SOLO L'OPZIONE DI "VERIFICA"
060000001106     C                   MOVEL     OPD(8)        OPDRIG                         *VERIFICA
060100001106     C                   MOVEL     'S'           OPDATT
060200001106     C                   MOVEL     OPDRIG        OPD(8)
060300001031     C                   MOVEL     OPD(9)        OPDRIG                         *VERIFICA
060400970626     C                   MOVEL     'S'           OPDATT
060500001031     C                   MOVEL     OPDRIG        OPD(9)
060600970526     C*
060700970526     C* GESTIONE OPZIONI A VIDEO
060800970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
060900970526     C                   EXSR      OPZGES
061000970526     C*
061100970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
061200970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
061300970526     C                   EXSR      FUNGES
061400970526     C*
061500970526     C                   ENDSR
061600970526     C*------------------------------------------------------------------------*
061700970526     C* *INZSR - OPERAZIONI INIZIALI
061800970526     C*------------------------------------------------------------------------*
061900970526     C     *INZSR        BEGSR
062000970526     C*---
062100970526     C* RICEVIMENTO PARAMETRI
062200970526     C*---
062300970526     C     *ENTRY        PLIST
062400970612     C                   PARM                    KPJBA
062500001030     C                   MOVEL     KPJBU         D00OP0
062600001030     C*
062700001030     C* IMPOSTO PER DEFAULT L'ORDINAMENTO PER LOCALITA'
062800001030     C                   MOVEL     'LOC'         WTIPRI
062900970526     C*
063000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
063100970526     C     XMSGPA        PLIST
063200970526     C                   PARM                    DSMSMS
063300970526     C                   PARM                    DSMSPG
063400970526     C                   PARM                    DSMSKE
063500970526     C                   PARM                    DSMSER
063600970526     C*
063700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
063800970526     C                   CALL      'XKEY'
063900970526     C                   PARM                    DSKEY
064000001030     C*---
064100001030     C* VARIABILI RIFERITE AL DATA BASE
064200001030     C*---
064300001030     C     *LIKE         DEFINE    VIDPCA        DEPCAP
064400001030     C     *LIKE         DEFINE    VIDPLO        DEPLOC
064500001030     C     *LIKE         DEFINE    VIDPNA        DEPNAR
064600001106     C     *LIKE         DEFINE    VIDRIC        DEPRIC
064700970526     C*---
064800970526     C* VARIABILI NON RIFERITE AL DATA BASE
064900970526     C*---
065000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
065100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
065200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
065300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
065400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
065500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
065600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
065700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
065800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
065900970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
066000970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
066100970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
066200970529     C     *LIKE         DEFINE    D00OP1        WOP1
066300970602     C     *LIKE         DEFINE    D00NAR        DNAR
066400970602     C     *LIKE         DEFINE    D00CAP        DCAP
066500001106     C     *LIKE         DEFINE    D00LOC        DLOC
066600001031     C     *LIKE         DEFINE    D00SIN        DSIN
066700120523     C*
066800120523     C* CHIAVI DI LETTURA
066900120523     C* AZCPC01L - COMPLETA
067000120523     C     K03CPC01      KLIST
067100120523     C                   KFLD                    CPCVER                         *VERSIONE
067200120523     C                   KFLD                    CPCNAR                         *NAZIONE
067300120523     C                   KFLD                    CPCCAP                         *CAP
067400120523     C* AZCPL01L - COMPLETA
067500120523     C     K04CPL01      KLIST
067600120523     C                   KFLD                    CPLVER                         *VERSIONE
067700120523     C                   KFLD                    CPLNAR                         *NAZIONE
067800120523     C                   KFLD                    CPLCAP                         *CAP
067900120523     C                   KFLD                    CPLLOC                         *LOCALITA
068000970526     C*---
068100970526     C* VARIABILI DI CONTROLLO
068200970526     C*---
068300970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
068400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
068500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
068600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
068700970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
068800970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
068900001030     C                   MOVEL     '0'           CHKCURSOR         1
069000001106     C                   MOVEL     *BLANKS       WPGM              8
069100001106     C                   MOVEL     *ZEROS        VIDRIC
069200001106     C                   MOVEL     *ZEROS        DEPRIC
069300970526     C*
069400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
069500001030     C                   MOVEL     'TISI77R'     DSMSPG
069600001030     C                   MOVEL     'TISI77R'     MSGQUE
069700970526     C                   MOVEL     *BLANKS       MSGKEY
069800970526     C                   MOVEL     *BLANKS       DSMSMS
069900970526     C                   MOVEL     *BLANKS       DSMSKE
070000970526     C                   Z-ADD     *ZEROS        DSMSER
070100001030     C*
070200001030     C* CALCOLA LA DATA CORRENTE
070300001030     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
070400001030     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
070500001030     C                   Z-ADD     WN8           DATA              8 0
070600001030     C                   Z-ADD     WN8           G08DAT
070700001030     C                   Z-ADD     *ZEROS        G08INV
070800001030     C                   MOVEL     '0'           G08ERR
070900001030     C                   CALL      'XSRDA8'
071000001030     C                   PARM                    WLBDA8
071100001030     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
071200001116     C*
071300001116     C* REPERIMENTO DATA RIFERIMENTO ULTIMA VERSIONE ESISTENTE
071400001116     C     *HIVAL        SETGT     AZCPR01L
071500001116     C                   READP     AZCPR01L
071600001116     C                   IF        not %eof(AZCPR01L)
071700001116     C                   Z-ADD     CPRDDE        DATRIF            8 0
071800120523     C* salvo la versione che serve come chiave a CPC e CPL
071900120523     C                   EVAL      Versione = CPRVER
072000001116     C                   ELSE
072100001116     C                   Z-ADD     DATCOR        DATRIF
072200001116     C                   ENDIF
072300970526     C*
072400970526     C* DEPOSITA LE OPZIONI DI ENTRATA
072500970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
072600970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
072700970526     C*
072800970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
072900970526     C                   EXSR      CHKFOP
073000970526     C*
073100970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
073200001030     C     DEPOP0        IFEQ      'L01'
073300970530     C                   MOVEL     DEC(3)        VIDING
073400001030     C                   ENDIF
073500001030     C     DEPOP0        IFEQ      'L02'
073600970530     C                   MOVEL     DEC(1)        VIDING
073700001030     C                   ENDIF
073800001030     C     DEPOP0        IFEQ      'L05'
073900970530     C                   MOVEL     DEC(2)        VIDING
074000001030     C                   ENDIF
074100970526     C*
074200970526     C                   ENDSR
074300970526     C*------------------------------------------------------------------------*
074400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
074600970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
074700970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
074800970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
074900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
075100970526     C*------------------------------------------------------------------------*
075200970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
075300970526     C*------------------------------------------------------------------------*
075400970526     C     POSSFL        BEGSR
075500970526     C*
075600970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
075700970526     C                   EXSR      CLRSFL
075800970526     C*
075900970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
076000970526     C                   EXSR      SETFIL
076100970526     C*
076200970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
076300970526     C     SFLEOF        IFEQ      0
076400970526     C                   EXSR      CARSFL
076500970526     C                   END
076600970526     C*
076700970526     C                   ENDSR
076800970526     C*------------------------------------------------------------------------*
076900970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
077000970526     C*------------------------------------------------------------------------*
077100970526     C     ENTATT        BEGSR
077200970526     C*
077300970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
077400970526     C                   Z-ADD     0             OPZOK             1 0
077500970526     C*
077600970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
077700970526     C                   Z-ADD     0             SFLVAR            5 0
077800970526     C*
077900970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
078000970526     C                   Z-ADD     0             SFLERR
078100970526     C*
078200970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
078300970526     C                   Z-ADD     SFLULT        WFLULT
078400970526     C*
078500970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
078600001030     C                   READC     SI77D05                                98
078700970526     C     *IN98         DOWEQ     '0'
078800970526     C*
078900970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
079000970526     C                   ADD       1             SFLVAR
079100970526     C*
079200970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
079300970526     C     OPZOK         IFEQ      0
079400970526     C*
079500970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
079600970526     C     OPZKEY        IFNE      *BLANKS
079700970526     C                   EXSR      OPZCON
079800970526     C                   END
079900970526     C*
080000970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
080100970526     C*            2. PULIZIA SCELTA IN SFL
080200970526     C     OPZOK         IFEQ      0
080300970526     C                   MOVEL     '0'           *IN41
080400970526     C                   MOVEL     *BLANKS       OPZKEY
080500970526     C                   ELSE
080600970526     C*
080700970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
080800970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
080900970526     C                   MOVEL     '1'           *IN41
081000970526     C                   Z-ADD     SFLULT        SFLERR
081100970526     C                   END
081200970526     C                   ELSE
081300970526     C*
081400970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
081500970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
081600970526     C                   MOVEL     '1'           *IN41
081700970526     C                   END                                                    OPZOK=0
081800970526     C*
081900970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
082000970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
082100970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
082200970526     C     OPZOK         IFNE      0
082300970526     C                   Z-ADD     SFLERR        SFLNBR
082400970526     C                   ELSE
082500970526     C                   Z-ADD     SFLULT        SFLNBR
082600970526     C                   END
082700970526     C*
082800970526     C* AGGIORNO RECORD SFL
082900001030     C                   UPDATE    SI77D05
083000970526     C*
083100001030     C                   READC     SI77D05                                98
083200970526     C                   END                                                    DO WHILE
083300970526     C*
083400970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
083500970526     C                   Z-ADD     WFLULT        SFLULT
083600970526     C*
083700970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
083800970526     C     SFLVAR        IFEQ      0
083900970526     C*---                 MOVEL'1'       FINL00
084000970526     C                   END
084100970526     C*
084200970526     C                   ENDSR
084300970526     C*------------------------------------------------------------------------*
084400970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
084500970526     C*------------------------------------------------------------------------*
084600970526     C     CLRSFL        BEGSR
084700970526     C*
084800970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
084900970526     C* (0=NON FINE FILE  1=FINE FILE)
085000970526     C                   Z-ADD     0             SFLEOF
085100970526     C*
085200970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
085300970526     C                   Z-ADD     0             SFLULT
085400970526     C*
085500970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
085600970526     C                   MOVEL     '1'           *IN40
085700001030     C                   WRITE     SI77D06
085800970526     C                   MOVEL     '0'           *IN40
085900970526     C*
086000970526     C                   ENDSR
086100970526     C*------------------------------------------------------------------------*
086200970526     C* CARSFL - CARICAMENTO SFL
086300970526     C*------------------------------------------------------------------------*
086400970526     C     CARSFL        BEGSR
086500970526     C*
086600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
086700970526     C     SFLULT        IFEQ      0
086800970526     C                   EXSR      SETFIL
086900970526     C                   END
087000970526     C*
087100970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
087200970526     C                   Z-ADD     SFLULT        WFLULT
087300970526     C*
087400970526     C* CICLO:
087500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
087600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
087700970526     C                   Z-ADD     1             SFLRIG
087800970526     C     SFLRIG        DOWLE     SFLPAG
087900970526     C     SFLEOF        ANDEQ     0
088000970526     C*
088100970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
088200970526     C                   MOVEL     *BLANKS       OPZKEY
088300970526     C*
088400970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
088500970526     C                   EXSR      IRESFL
088600970526     C*
088700970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
088800970526     C                   ADD       1             SFLULT
088900970526     C*
089000970526     C* SCRITTURA RECORD SFL
089100001030     C                   WRITE     SI77D05
089200970526     C                   ADD       1             SFLRIG
089300970526     C*
089400970526     C* LETTURA FILE DA CARICARE
089500970526     C                   EXSR      LETFIL
089600970526     C                   END                                                    DO WHILE
089700970526     C*
089800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
089900970526     C     WFLULT        IFNE      SFLULT
090000970526     C                   Z-ADD     WFLULT        SFLNBR
090100970526     C                   ADD       1             SFLNBR
090200970526     C                   END
090300970526     C*
090400970526     C                   ENDSR
090500970526     C*------------------------------------------------------------------------*
090600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
090700970526     C*------------------------------------------------------------------------*
090800970526     C     OUTSFL        BEGSR
090900970526     C*
091000970526     C* WRITE RECORD TESTATA VIDEO
091100001030     C                   WRITE     SI77D01
091200970526     C*
091300970526     C* VISUALIZZAZIONE MESSAGGIO
091400970526     C                   EXSR      SNDMSG
091500970526     C*
091600970526     C* WRITE RECORD OPZIONI VIDEO
091700001030     C                   WRITE     SI77D02
091800970526     C*
091900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
092000001030     C                   WRITE     SI77D03
092100970526     C*
092200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
092300001030     C                   WRITE     SI77D04
092400970526     C*
092500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
092600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
092700970526     C     SFLEOF        IFEQ      0
092800970526     C                   MOVEL     '0'           *IN42
092900970526     C                   ELSE
093000970526     C*
093100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
093200970526     C                   MOVEL     '1'           *IN42
093300970526     C                   END
093400970526     C*
093500970526     C* WRITE RECORD TASTI FUNZIONALI
093600001030     C                   WRITE     SI77D09
093700970526     C*
093800970526     C* CASO DI SFL VUOTO
093900970526     C     SFLULT        IFLE      0
094000970526     C*
094100970526     C*    - WRITE RECORD VUOTO
094200001030     C                   WRITE     SI77D0A
094300001030     C                   READ      SI77D0A                                99
094400970526     C*
094500970526     C                   ELSE
094600970526     C*
094700970526     C* CASO DI SFL CON RECORDS
094800970526     C*
094900970526     C*    - WRITE RECORD CONTROLLO SFL
095000001030     C                   WRITE     SI77D06
095100001030     C                   READ      SI77D06                                99
095200970526     C                   END
095300970526     C*
095400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
095500001030     C                   READ      SI77D03                                99
095600970526     C*
095700970526     C                   ENDSR
095800970526     C*------------------------------------------------------------------------*
095900970526     C* SNDMSG - MANDA MESSAGGIO
096000970526     C*------------------------------------------------------------------------*
096100970526     C     SNDMSG        BEGSR
096200970526     C*
096300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
096400970526     C                   MOVEL     '1'           *IN40
096500001030     C                   WRITE     SI77DMC
096600970526     C                   MOVEL     '0'           *IN40
096700970526     C*
096800970526     C* VISUALIZZAZIONE MESSAGGIO
096900970526     C                   Z-ADD     0             MSGULT
097000970526     C                   CALL      'XMSG'        XMSGPA
097100970526     C     DSMSER        IFEQ      0
097200970526     C                   MOVEL     DSMSKE        MSGKEY
097300970526     C                   END
097400970526     C                   Z-ADD     1             MSGULT
097500001030     C                   WRITE     SI77DMS                                      SFL MESS
097600001030     C                   WRITE     SI77DMC                                      CONTR MESS
097700970526     C*
097800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
097900970526     C                   MOVEL     *BLANKS       DSMSMS
098000970526     C*
098100970526     C                   ENDSR
098200970526     C*------------------------------------------------------------------------*
098300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
098400970526     C*------------------------------------------------------------------------*
098500970526     C     FUNCON        BEGSR
098600970526     C*
098700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
098800970526     C                   EXSR      FUNVAR
098900970526     C*
099000970526     C* PULISCO CAMPI DI LAVORO
099100970526     C                   Z-ADD     0             I                 5 0
099200970526     C                   Z-ADD     0             W                 5 0
099300970526     C*
099400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
099500970526     C     FUNKEY        IFNE      *BLANKS
099600970526     C     1             DO        24            I
099700970526     C                   MOVEL     FUD(I)        FUDRIG
099800970526     C*
099900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
100000970526     C     FUNKEY        IFEQ      FUDFUN
100100970526     C     FUDATT        ANDEQ     'S'
100200970526     C                   Z-ADD     I             W
100300970526     C                   END
100400970526     C                   END                                                    DO WHILE
100500970526     C*
100600970526     C* FUNZIONE NON ATTIVA (ERRORE)
100700970526     C     W             IFEQ      0
100800970526     C*
100900970526     C* EMISSIONE MESSAGGIO DI ERRORE
101000970526     C                   MOVEL     MSG(2)        DSMSMS
101100970526     C                   END                                                    W=0
101200970526     C                   END                                                    FUNKEY NE *BLANKS
101300970526     C*
101400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
101500970526     C     W             IFNE      0
101600970526     C     FUNKEY        OREQ      *BLANKS
101700970526     C*
101800970526     C     FUNKEY        IFEQ      *BLANKS
101900970526     C                   EXSR      ENTGES
102000970526     C                   END
102100970526     C     FUNKEY        IFEQ      '03'
102200970526     C                   EXSR      F03GES
102300970526     C                   END
102400970526     C     FUNKEY        IFEQ      '05'
102500970526     C                   EXSR      F05GES
102600970526     C                   END
102700970526     C     FUNKEY        IFEQ      '06'
102800970526     C                   EXSR      F06GES
102900970526     C                   END
103000970526     C     FUNKEY        IFEQ      '12'
103100970526     C                   EXSR      F12GES
103200970526     C                   END
103300970529     C     FUNKEY        IFEQ      '13'
103400970529     C                   EXSR      F13GES
103500970529     C                   END
103600970526     C     FUNKEY        IFEQ      '23'
103700970526     C                   EXSR      F23GES
103800970526     C                   END
103900970526     C     FUNKEY        IFEQ      '24'
104000970526     C                   EXSR      F24GES
104100970526     C                   END
104200970526     C     FUNKEY        IFEQ      'RU'
104300970526     C                   EXSR      RUPGES
104400970526     C                   END
104500970526     C*
104600970526     C                   END
104700970526     C*
104800970526     C                   ENDSR
104900970526     C*------------------------------------------------------------------------*
105000970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
105100970526     C*------------------------------------------------------------------------*
105200970526     C     FUNVAR        BEGSR
105300970526     C*
105400970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
105500970526     C     KEY           IFEQ      ENTER
105600970526     C                   MOVEL     *BLANKS       FUNKEY
105700970526     C                   END
105800970526     C     KEY           IFEQ      F1
105900970526     C                   MOVEL     '01'          FUNKEY
106000970526     C                   END
106100970526     C     KEY           IFEQ      F2
106200970526     C                   MOVEL     '02'          FUNKEY
106300970526     C                   END
106400970526     C     KEY           IFEQ      F3
106500970526     C                   MOVEL     '03'          FUNKEY
106600970526     C                   END
106700970526     C     KEY           IFEQ      F4
106800970526     C                   MOVEL     '04'          FUNKEY
106900970526     C                   END
107000970526     C     KEY           IFEQ      F5
107100970526     C                   MOVEL     '05'          FUNKEY
107200970526     C                   END
107300970526     C     KEY           IFEQ      F6
107400970526     C                   MOVEL     '06'          FUNKEY
107500970526     C                   END
107600970526     C     KEY           IFEQ      F7
107700970526     C                   MOVEL     '07'          FUNKEY
107800970526     C                   END
107900970526     C     KEY           IFEQ      F8
108000970526     C                   MOVEL     '08'          FUNKEY
108100970526     C                   END
108200970526     C     KEY           IFEQ      F9
108300970526     C                   MOVEL     '09'          FUNKEY
108400970526     C                   END
108500970526     C     KEY           IFEQ      F10
108600970526     C                   MOVEL     '10'          FUNKEY
108700970526     C                   END
108800970526     C     KEY           IFEQ      F11
108900970526     C                   MOVEL     '11'          FUNKEY
109000970526     C                   END
109100970526     C     KEY           IFEQ      F12
109200970526     C                   MOVEL     '12'          FUNKEY
109300970526     C                   END
109400970526     C     KEY           IFEQ      F13
109500970526     C                   MOVEL     '13'          FUNKEY
109600970526     C                   END
109700970526     C     KEY           IFEQ      F14
109800970526     C                   MOVEL     '14'          FUNKEY
109900970526     C                   END
110000970526     C     KEY           IFEQ      F15
110100970526     C                   MOVEL     '15'          FUNKEY
110200970526     C                   END
110300970526     C     KEY           IFEQ      F16
110400970526     C                   MOVEL     '16'          FUNKEY
110500970526     C                   END
110600970526     C     KEY           IFEQ      F17
110700970526     C                   MOVEL     '17'          FUNKEY
110800970526     C                   END
110900970526     C     KEY           IFEQ      F18
111000970526     C                   MOVEL     '18'          FUNKEY
111100970526     C                   END
111200970526     C     KEY           IFEQ      F19
111300970526     C                   MOVEL     '19'          FUNKEY
111400970526     C                   END
111500970526     C     KEY           IFEQ      F20
111600970526     C                   MOVEL     '20'          FUNKEY
111700970526     C                   END
111800970526     C     KEY           IFEQ      F21
111900970526     C                   MOVEL     '21'          FUNKEY
112000970526     C                   END
112100970526     C     KEY           IFEQ      F22
112200970526     C                   MOVEL     '22'          FUNKEY
112300970526     C                   END
112400970526     C     KEY           IFEQ      F23
112500970526     C                   MOVEL     '23'          FUNKEY
112600970526     C                   END
112700970526     C     KEY           IFEQ      F24
112800970526     C                   MOVEL     '24'          FUNKEY
112900970526     C                   END
113000970526     C     KEY           IFEQ      ROLLUP
113100970526     C                   MOVEL     'RU'          FUNKEY
113200970526     C                   END
113300970526     C     KEY           IFEQ      HELP
113400970526     C                   MOVEL     'HP'          FUNKEY
113500970526     C                   END
113600970526     C*
113700970526     C                   ENDSR
113800970526     C*------------------------------------------------------------------------*
113900970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
114000970526     C*------------------------------------------------------------------------*
114100970526     C     FUNGES        BEGSR
114200970526     C*
114300970526     C* PULISCO CAMPI DI LAVORO
114400970526     C                   Z-ADD     0             W                 5 0
114500970526     C*
114600970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
114700970526     C* E CARICO TABELLA FUNZIONI ATTIVE
114800970526     C     1             DO        24            I
114900970526     C                   MOVEL     FUD(I)        FUDRIG
115000970526     C*
115100970526     C* PULIZIA FUNZIONI PRECEDENTI
115200970526     C                   MOVEL     *BLANKS       FUV(I)
115300970526     C*
115400970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
115500970526     C* (SALTA F23 E F24)
115600970526     C     FUDATT        IFEQ      'S'
115700970526     C     FUDFUN        ANDNE     '23'
115800970526     C     FUDFUN        ANDNE     '24'
115900970526     C                   ADD       1             W
116000970526     C                   MOVEL     FUDDES        FUV(W)
116100970526     C                   END
116200970526     C                   END                                                    DO WHILE
116300970526     C*
116400970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
116500970526     C* ALTRIMENTI DISATTIVO F24
116600970526     C     1             DO        24            I
116700970526     C                   MOVEL     FUD(I)        FUDRIG
116800970526     C*
116900970526     C     FUDFUN        IFEQ      '24'
117000970526     C     W             IFGT      6
117100970526     C                   MOVEL     'S'           FUDATT
117200970526     C                   ELSE
117300970526     C                   MOVEL     *BLANKS       FUDATT
117400970526     C                   END                                                    W GT 8
117500970526     C                   MOVEL     FUDRIG        FUD(I)
117600970526     C                   END
117700970526     C                   END                                                    DO WHILE
117800970526     C*
117900970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
118000970526     C     W             DIV       6             I
118100970526     C                   MVR                     Z
118200970526     C*
118300970526     C     Z             IFNE      0
118400970526     C     I             ADD       1             Z
118500970526     C                   ELSE
118600970526     C                   Z-ADD     I             Z
118700970526     C                   END
118800970526     C*
118900970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
119000970526     C                   ADD       1             FUNGRU
119100970526     C     FUNGRU        IFGT      Z
119200970526     C     FUNGRU        ORLT      1
119300970526     C                   Z-ADD     1             FUNGRU
119400970526     C                   END
119500970526     C*
119600970526     C* PRIMO GRUPPO DI 6 FUNZIONI
119700970526     C     FUNGRU        IFEQ      1
119800970526     C                   MOVEL     FUV(1)        FUN001
119900970526     C                   MOVEL     FUV(2)        FUN002
120000970526     C                   MOVEL     FUV(3)        FUN003
120100970526     C                   MOVEL     FUV(4)        FUN004
120200970526     C                   MOVEL     FUV(5)        FUN005
120300970526     C                   MOVEL     FUV(6)        FUN006
120400970526     C                   END
120500970526     C*
120600970526     C* SECONDO GRUPPO DI 6 FUNZIONI
120700970526     C     FUNGRU        IFEQ      2
120800970526     C                   MOVEL     FUV(7)        FUN001
120900970526     C                   MOVEL     FUV(8)        FUN002
121000970526     C                   MOVEL     FUV(9)        FUN003
121100970526     C                   MOVEL     FUV(10)       FUN004
121200970526     C                   MOVEL     FUV(11)       FUN005
121300970526     C                   MOVEL     FUV(12)       FUN006
121400970526     C                   END
121500970526     C*
121600970526     C* TERZO GRUPPO DI 6 FUNZIONI
121700970526     C     FUNGRU        IFEQ      3
121800970526     C                   MOVEL     FUV(13)       FUN001
121900970526     C                   MOVEL     FUV(14)       FUN002
122000970526     C                   MOVEL     FUV(15)       FUN003
122100970526     C                   MOVEL     FUV(16)       FUN004
122200970526     C                   MOVEL     FUV(17)       FUN005
122300970526     C                   MOVEL     FUV(18)       FUN006
122400970526     C                   END
122500970526     C*
122600970526     C* QUARTO GRUPPO DI 6 FUNZIONI
122700970526     C     FUNGRU        IFEQ      4
122800970526     C                   MOVEL     FUV(19)       FUN001
122900970526     C                   MOVEL     FUV(20)       FUN002
123000970526     C                   MOVEL     FUV(21)       FUN003
123100970526     C                   MOVEL     FUV(22)       FUN004
123200970526     C                   MOVEL     *BLANKS       FUN005
123300970526     C                   MOVEL     *BLANKS       FUN006
123400970526     C                   END
123500970526     C*
123600970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
123700970526     C                   MOVEL     *BLANKS       FUN007
123800970526     C                   MOVEL     *BLANKS       FUN008
123900970526     C*
124000970526     C     1             DO        24            I
124100970526     C                   MOVEL     FUD(I)        FUDRIG
124200970526     C     FUDFUN        IFEQ      '23'
124300970526     C     FUDATT        ANDEQ     'S'
124400970526     C                   MOVEL     FUDDES        FUN007
124500970526     C                   END
124600970526     C     FUDFUN        IFEQ      '24'
124700970526     C     FUDATT        ANDEQ     'S'
124800970526     C                   MOVEL     FUDDES        FUN008
124900970526     C                   END
125000970526     C                   END                                                    DO
125100970526     C*
125200970526     C                   ENDSR
125300970526     C*------------------------------------------------------------------------*
125400970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
125500970526     C*------------------------------------------------------------------------*
125600970526     C     OPZGES        BEGSR
125700970526     C*
125800970526     C* PULISCO CAMPI DI LAVORO
125900970526     C                   Z-ADD     0             W                 5 0
126000970526     C*
126100970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
126200970526     C* E CARICO TABELLA OPZIONI ATTIVE
126300970526     C     1             DO        24            I
126400970526     C                   MOVEL     OPD(I)        OPDRIG
126500970526     C*
126600970526     C* PULIZIA OPZIONI PRECEDENTI
126700970526     C                   MOVEL     *BLANKS       OPV(I)
126800970526     C*
126900970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
127000970526     C     OPDATT        IFEQ      'S'
127100970526     C                   ADD       1             W
127200970526     C                   MOVEL     OPDDES        OPV(W)
127300970526     C                   END
127400970526     C                   END                                                    DO WHILE
127500970526     C*
127600970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
127700970526     C* ALTRIMENTI DISATTIVO F23
127800970526     C     1             DO        24            I
127900970526     C                   MOVEL     FUD(I)        FUDRIG
128000970526     C*
128100970526     C     FUDFUN        IFEQ      '23'
128200970526     C     W             IFGT      8
128300970526     C                   MOVEL     'S'           FUDATT
128400970526     C                   ELSE
128500970526     C                   MOVEL     *BLANKS       FUDATT
128600970526     C                   END                                                    W GT 8
128700970526     C                   MOVEL     FUDRIG        FUD(I)
128800970526     C                   END
128900970526     C                   END                                                    DO WHILE
129000970526     C*
129100970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
129200970526     C     W             DIV       8             I
129300970526     C                   MVR                     Z                 5 0
129400970526     C*
129500970526     C     Z             IFNE      0
129600970526     C     I             ADD       1             Z
129700970526     C                   ELSE
129800970526     C                   Z-ADD     I             Z
129900970526     C                   END
130000970526     C*
130100970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
130200970526     C                   ADD       1             OPZGRU
130300970526     C     OPZGRU        IFGT      Z
130400970526     C     OPZGRU        ORLT      1
130500970526     C                   Z-ADD     1             OPZGRU
130600970526     C                   END
130700970526     C*
130800970526     C* PRIMO GRUPPO DI 8 OPZIONI
130900970526     C     OPZGRU        IFEQ      1
131000970526     C                   MOVEL     OPV(1)        OPZ001
131100970526     C                   MOVEL     OPV(2)        OPZ002
131200970526     C                   MOVEL     OPV(3)        OPZ003
131300970526     C                   MOVEL     OPV(4)        OPZ004
131400970526     C                   MOVEL     OPV(5)        OPZ005
131500970526     C                   MOVEL     OPV(6)        OPZ006
131600970526     C                   MOVEL     OPV(7)        OPZ007
131700970526     C                   MOVEL     OPV(8)        OPZ008
131800970526     C                   END
131900970526     C*
132000970526     C* SECONDO GRUPPO DI 8 OPZIONI
132100970526     C     OPZGRU        IFEQ      2
132200970526     C                   MOVEL     OPV(9)        OPZ001
132300970526     C                   MOVEL     OPV(10)       OPZ002
132400970526     C                   MOVEL     OPV(11)       OPZ003
132500970526     C                   MOVEL     OPV(12)       OPZ004
132600970526     C                   MOVEL     OPV(13)       OPZ005
132700970526     C                   MOVEL     OPV(14)       OPZ006
132800970526     C                   MOVEL     OPV(15)       OPZ007
132900970526     C                   MOVEL     OPV(16)       OPZ008
133000970526     C                   END
133100970526     C*
133200970526     C* TERZO GRUPPO DI 8 OPZIONI
133300970526     C     OPZGRU        IFEQ      3
133400970526     C                   MOVEL     OPV(17)       OPZ001
133500970526     C                   MOVEL     OPV(18)       OPZ002
133600970526     C                   MOVEL     OPV(19)       OPZ003
133700970526     C                   MOVEL     OPV(20)       OPZ004
133800970526     C                   MOVEL     OPV(21)       OPZ005
133900970526     C                   MOVEL     OPV(22)       OPZ006
134000970526     C                   MOVEL     OPV(23)       OPZ007
134100970526     C                   MOVEL     OPV(24)       OPZ008
134200970526     C                   END
134300970526     C*
134400970526     C                   ENDSR
134500970526     C*------------------------------------------------------------------------*
134600970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
134700970526     C*------------------------------------------------------------------------*
134800970526     C     OPZCON        BEGSR
134900970526     C*
135000970526     C* PULISCO CAMPI DI LAVORO
135100970526     C                   Z-ADD     0             I                 5 0
135200970526     C                   Z-ADD     0             W                 5 0
135300970526     C*
135400970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
135500970526     C     1             DO        24            I
135600970526     C                   MOVEL     OPD(I)        OPDRIG
135700970526     C*
135800970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
135900970526     C     OPZKEY        IFEQ      OPDOPZ
136000970526     C     OPZKEY        OREQ      OPDOP1
136100970526     C*
136200970526     C     OPDATT        IFEQ      'S'
136300970526     C                   MOVEL     OPDOPZ        OPZKEY
136400970526     C                   Z-ADD     I             W
136500970526     C                   END
136600970526     C                   END
136700970526     C*
136800970526     C                   END                                                    DO WHILE
136900970526     C*
137000970526     C* OPZIONE NON ATTIVA (ERRORE)
137100970526     C     W             IFEQ      0
137200970526     C*
137300970526     C* OPZIONE NON OK
137400970526     C                   Z-ADD     1             OPZOK
137500970526     C*
137600970526     C* EMISSIONE MESSAGGIO DI ERRORE
137700970526     C                   MOVEL     MSG(1)        DSMSMS
137800970526     C                   ELSE
137900970526     C*
138000970526     C* OPZIONE ATTIVA (PROCEDI ...)
138100970526     C*
138200970526     C     OPZKEY        IFEQ      ' 1'
138300970526     C                   EXSR      O01GES
138400970526     C                   END
138500001106     C     OPZKEY        IFEQ      ' 8'
138600001106     C                   EXSR      O08GES
138700001106     C                   END
138800001031     C     OPZKEY        IFEQ      ' 9'
138900001031     C                   EXSR      O09GES
139000970526     C                   END
139100970611     C     OPZKEY        IFEQ      ' 3'
139200970611     C                   EXSR      O03GES
139300970611     C                   END
139400970526     C     OPZKEY        IFEQ      ' 4'
139500970526     C                   EXSR      O04GES
139600970526     C                   END
139700970526     C     OPZKEY        IFEQ      ' 5'
139800970526     C                   EXSR      O05GES
139900970526     C                   END
140000970526     C     OPZKEY        IFEQ      ' 7'
140100970526     C                   EXSR      O07GES
140200970526     C                   END
140300970529     C     OPZKEY        IFEQ      '13'
140400970529     C                   EXSR      O13GES
140500970529     C                   END
140600970526     C                   END                                                    OPZ.NON ATTIVA
140700970526     C*
140800970526     C                   ENDSR
140900970526     C*------------------------------------------------------------------------*
141000970526     C* NOTE COSTRUZIONE SCHIERE
141100970526     C*------------------------------------------------------------------------*
141200970526     C*
141300970526     C* -MSG-MESSAGGI
141400970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
141500970526     C* (NON TOCCARE MSG,1 E MSG,2)
141600970526     C*
141700970526     C* -CMD-COMANDI
141800970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
141900970526     C*
142000970526     C* -OPD-OPZIONI
142100970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
142200970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
142300970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
142400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
142500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
142600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
142700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
142800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
142900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
143000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
143100970526     C*
143200970526     C* -FUD-FUNZIONI
143300970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
143400970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
143500970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
143600970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
143700970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
143800970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
143900970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
144000970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
144100970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
144200970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
144300970526     O*------------------------------------------------------------------------*
144400970526** DEC - DECODIFICHE
144500970526    GESTIONE                                                     1
144600970530INTERROGAZIONE                                                   2
144700970530     SCELTA                                                      3
1448009705264
1449009705265
145000970526** MSG - MESSAGGI
145100001030TISI77R- Opzione non gestita                                                   1
145200001030TISI77R- Tasto funzionale non gestito                                          2
145300001030TISI77R- 3
145400001030TISI77R- 4
145500001030TISI77R- 5
145600001030TISI77R- Accettato solo un parametro di inizio lista                           6
145700001030TISI77R- 7                                                                     7
145800001030TISI77R- 8                                                                     8
145900001030TISI77R- 9                                                                     9
146000001030TISI77R- 10                                                                   10
146100001030TISI77R- 11                                                                   11
146200001030TISI77R- 12                                                                   12
146300001030TISI77R- 13                                                                   13
146400001030TISI77R- 14                                                                   14
146500001030TISI77R- 15                                                                   15
146600001030TISI77R- 16                                                                   16
146700001030TISI77R- 17                                                                   17
146800001030TISI77R- 18                                                                   18
146900001030TISI77R- 19                                                                   19
147000001030TISI77R- 20                                                                   20
147100970526** OPD - OPZIONI
147200001030                   -1 -     (S/N)=N                        opz. 01¬
147300001031                   -2 -     (S/N)=N                        opz. 02¬
147400001030                   -3 -     (S/N)=N                        opz. 03¬
147500001030                   -4 -     (S/N)=N                        opz. 04¬
147600001030                   -5 -     (S/N)=N                        opz. 05¬
147700970526                   -6 -     (S/N)=N                        opz. 06¬
147800001030                   -7 -     (S/N)=N                        opz. 07¬
147900001106 8=Verif. x LOC    -8 -     (S/N)=S                        opz. 08¬
148000001106 9=Verif. x CAP    -9 -     (S/N)=S                        opz. 09¬
148100970527                   -10-     (S/N)=N                        opz. 10¬
148200970529                   -11-     (S/N)=N                        opz. 11¬
148300970529                   -12-     (S/N)=N                        opz. 12¬
148400001030                   -13-     (S/N)=N                        opz. 13¬
148500970526                   -  -     (S/N)=N                        opz. 14¬
148600970526                   -  -     (S/N)=N                        opz. 15¬
148700970526                   -  -     (S/N)=N                        opz. 16¬
148800970526                   -  -     (S/N)=N                        opz. 17¬
148900970526                   -  -     (S/N)=N                        opz. 18¬
149000970526                   -  -     (S/N)=N                        opz. 19¬
149100970526                   -  -     (S/N)=N                        opz. 20¬
149200970526                   -  -     (S/N)=N                        opz. 21¬
149300970526                   -  -     (S/N)=N                        opz. 22¬
149400970526                   -  -     (S/N)=N                        opz. 23¬
149500970526                   -  -     (S/N)=N                        opz. 24¬
149600970526** FUD - FUNZIONI
149700970526                   -        (S/N)=N                        funz.01¬
149800970526                   -        (S/N)=N                        funz.02¬
149900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
150000970526                   -        (S/N)=N                        funz.04¬
150100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
150200001030                   -        (S/N)=N                        funz.06¬
150300970526                   -        (S/N)=N                        funz.07¬
150400970527                   -        (S/N)=N                        funz.08¬
150500970527                   -        (S/N)=N                        funz.09¬
150600970527                   -        (S/N)=N                        funz.10¬
150700970526                   -        (S/N)=N                        funz.11¬
150800970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
150900001030                   -        (S/N)=N                        funz.13¬
151000970526                   -        (S/N)=N                        funz.14¬
151100970526                   -        (S/N)=N                        funz.15¬
151200970526                   -        (S/N)=N                        funz.16¬
151300970526                   -        (S/N)=N                        funz.17¬
151400970526                   -        (S/N)=N                        funz.18¬
151500970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
151600970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
151700970527                   -        (S/N)=N                        funz.21¬
151800970527                   -        (S/N)=N                        funz.22¬
151900970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
152000970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
