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
001200001030     FTISI77D   CF   E             WORKSTN INFDS(DEVDS1)
001300001030     F                                     SFILE(SI77D05:SFLULT)
001400001030     F                                     SFILE(SI77DMS:MSGULT)
001500970526     D*------------------------------------------------------------------------*
001600970526     D* SCHIERE
001700970526     D*------------------------------------------------------------------------*
001800970526     D*------------
001900970526     D* DECODIFICHE
002000970526     D*------------
002100970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002200970526     D*------------
002300970526     D* MESSAGGI
002400970526     D*------------
002500970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002600970526     D*------------
002700970526     D* DESCRIZIONI OPZIONI
002800970526     D*------------
002900970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI TASTI FUNZIONALI
003200970526     D*------------
003300970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* OPZIONI DA CARICARE A VIDEO
003600970526     D*------------
003700970526     D OPV             S             19    DIM(24)
003800970526     D*------------
003900970526     D* FUNZIONI DA CARICARE A VIDEO
004000970526     D*------------
004100970527     D FUV             S             19    DIM(24)
004200970526     D*------------------------------------------------------------------------*
004300970526     D* INPUT
004400970526     D*------------------------------------------------------------------------*
004500970526     D*------------
004600970526     D* RIDEFINIZIONE OPZIONI VIDEO
004700970526     D*------------
004800970526     D OPDDS           DS
004900970526     D  OPDOPZ                 1      2
005000970526     D  OPDDE1                 3     19
005100970526     D  OPDOP1                21     22
005200970526     D  OPDFI0                23     34
005300970526     D  OPDATT                35     35
005400970526     D  OPDFI1                36     80
005500970526     D  OPDDES                 1     19
005600970526     D  OPDRIG                 1     80
005700970526     D*------------
005800970526     D* RIDEFINIZIONE FUNZIONI VIDEO
005900970526     D*------------
006000970526     D FUDDS           DS
006100970526     D  FUDDE1                 1      1
006200970526     D  FUDFUN                 2      3
006300970526     D  FUDDE2                 4     19
006400970526     D  FUDFI0                20     34
006500970526     D  FUDATT                35     35
006600970526     D  FUDFI1                36     80
006700970526     D  FUDDES                 1     19
006800970526     D  FUDRIG                 1     80
006900970526     D*------------
007000970526     D* DS GESTIONE TASTI FUNZIONALI
007100970526     D*------------
007200970526     D DSKEY         E DS
007300970526     D DEVDS1          DS
007400970526     D  KEY                  369    369
007500970526     D*------------
007600970526     D* DS "XMSG" - GESTIONE MESSAGGI
007700970526     D*------------
007800970526     D DSMSG         E DS
007900970527     D*------------------
008000001030     D* DS DI PROCEDURA
008100970526     D*------------
008200970526     D DSSI00        E DS                  EXTNAME(TISI00DS)
008300970526     D*------------
008400970526     D* ARCHITETTURA
008500970526     D*------------
008600970526     D KPJBA         E DS
008700001030     D*------------
008800001030     D* DEFINIZIONE DS PER LETTURA VIA SQL
008900001030     D*------------
009000001030     D DSWFLOC         DS
009100001030     D  WFLNAR                 1      3
009200001030     D  WFLPRV                 4      5
009300001030     D  WFLCAP                 6     14
009400001030     D  WFLLOC                15     49
009500001106     D  WFLSPE                50     58  0
009600001030     D*-------------------
009700001030     D* DS "XSRDA8" - CONTROLLO DATA(8)
009800001030     D*-------------------
009900001030     D WLBDA8          DS
010000001030     D  G08DAT                 1      8  0
010100001030     D  G08INV                 9     16  0
010200001030     D  G08ERR                17     17
010300001030     D  G08TGI                18     22  0
010400970526     C*------------------------------------------------------------------------*
010500970526     C* MAIN LINES
010600970526     C*------------------------------------------------------------------------*
010700970526     C*
010800970526     C* SE NON CI SONO ERRORI, PROSEGUE
010900001030     C     ERRL00        IFNE      '1'
011000970526     C*
011100970526     C* CARICA IL SFILE
011200970526     C                   EXSR      POSSFL
011300970526     C*
011400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011500001030     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
011600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
011700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
011800970526     C*
011900970526     C* REIMPOSTA ERRORE DI PROCERURA
012000970526     C                   MOVEL     '0'           ERRL00
012100970526     C*
012200970526     C* EMETTE IL SFILE
012300970526     C                   EXSR      OUTSFL
012400970526     C*
012500970526     C* GESTIONE TASTI FUNZIONALI
012600970526     C                   EXSR      FUNCON
012700970526     C*
012800001030     C                   ENDDO
012900970527     C*
013000970527     C* CI SONO ERRORI IN ENTRATA
013100001030     C                   ELSE
013200970527     C                   MOVEL     '1'           D00ERR
013300970527     C                   MOVEL     DSMSMS        D00MSG
013400001030     C                   ENDIF
013500970526     C* FINE PGM
013600970526     C                   SETON                                        LR
013700970526     C*------------------------------------------------------------------------*
013800970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013900970526     C*------------------------------------------------------------------------*
014000970526     C     F03GES        BEGSR
014100970526     C*
014200970526     C* IMPOSTA "PREMUTO F03"
014300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014400970526     C                   MOVEL     '1'           D00F03                         *DI PROCEDURA
014500970526     C*
014600970526     C                   ENDSR
014700970526     C*------------------------------------------------------------------------*
014800970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014900970526     C*------------------------------------------------------------------------*
015000970526     C     F05GES        BEGSR
015100970526     C*
015200970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
015300970526     C*
015400970526     C                   ENDSR
015500970526     C*------------------------------------------------------------------------*
015600970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
015700970526     C*------------------------------------------------------------------------*
015800970526     C     F06GES        BEGSR
015900970526     C*
016000970526     C*
016100970526     C                   ENDSR
016200970526     C*------------------------------------------------------------------------*
016300970526     C* F12GES - GESTIONE F12 -> RITORNO
016400970526     C*------------------------------------------------------------------------*
016500970526     C     F12GES        BEGSR
016600970526     C*
016700970526     C* IMPOSTA "PREMUTO F12"
016800970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
016900970526     C                   MOVEL     '1'           D00F12                         *DI PROCEDURA
017000970526     C*
017100970526     C                   ENDSR
017200970529     C*------------------------------------------------------------------------*
017300970529     C* F13GES - GESTIONE F13 -> GESTIONE SINONIMI
017400970529     C*------------------------------------------------------------------------*
017500970529     C     F13GES        BEGSR
017600970529     C*
017700970529     C*
017800970529     C                   ENDSR
017900970526     C*------------------------------------------------------------------------*
018000970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
018100970526     C*------------------------------------------------------------------------*
018200970526     C     F23GES        BEGSR
018300970526     C*
018400970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
018500970526     C*
018600970526     C                   ENDSR
018700970526     C*------------------------------------------------------------------------*
018800970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
018900970526     C*------------------------------------------------------------------------*
019000970526     C     F24GES        BEGSR
019100970526     C*
019200970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
019300970526     C*
019400970526     C                   ENDSR
019500970526     C*------------------------------------------------------------------------*
019600970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
019700970526     C*------------------------------------------------------------------------*
019800970526     C     RUPGES        BEGSR
019900970526     C*
020000970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
020100970526     C*
020200970526     C                   ENDSR
020300970526     C*------------------------------------------------------------------------*
020400970526     C* ENTGES - GESTIONE ENTER -> INVIO
020500970526     C*------------------------------------------------------------------------*
020600970526     C     ENTGES        BEGSR
020700970526     C*
020800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
020900970526     C                   EXSR      NEWPOS
021000970526     C*
021100001106     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
021200001030     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
021300001030     C     SWPOSI        IFNE      0
021400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
021500001030     C                   ELSE
021600001030     C     SFLULT        IFNE      0
021700970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
021800970526     C*
021900970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
022000001030     C     SWMODI        IFNE      0
022100970526     C                   EXSR      POSSFL
022200001030     C                   ENDIF
022300001030     C                   ENDIF
022400001030     C                   ENDIF
022500001030     C                   ENDIF
022600970526     C*
022700970526     C                   ENDSR
022800970526     C*------------------------------------------------------------------------*
022900001030     C* O01GES - GESTIONE OPZIONE 1=SCELTA
023000970526     C*------------------------------------------------------------------------*
023100970526     C     O01GES        BEGSR
023200970526     C*
023300970526     C*
023400970526     C                   ENDSR
023500001106     C*------------------------------------------------------------------------*
023600001106     C* O08GES - GESTIONE OPZIONE 8=VERIFICA X LOCALITA'
023700001106     C*------------------------------------------------------------------------*
023800001106     C     O08GES        BEGSR
023900001106     C*
024000001106     C* VERIFICA
024100001106     C                   CLEAR                   DSSI00
024200001106     C                   MOVEL     VIDNAR        DNAR
024300001106     C                   MOVEL     *BLANKS       DCAP
024400001106     C                   MOVEL     VIDLOC        DLOC
024500001106     C                   MOVEL     VIDLOC        DSIN
024600001106     C                   MOVEL     DEPOP0        WOP0
024700001106     C                   MOVEL     'S'           WOP0
024800001106     C                   MOVEL     'O08'         WOP1
024900001106     C                   MOVEL     'TISI35R'     WPGM
025000001106     C                   EXSR      LANPGM
025100001106     C*
025200001106     C                   ENDSR
025300970526     C*------------------------------------------------------------------------*
025400001106     C* O09GES - GESTIONE OPZIONE 9=VERIFICA X CAP
025500970526     C*------------------------------------------------------------------------*
025600001031     C     O09GES        BEGSR
025700970526     C*
025800001030     C* VERIFICA
025900001030     C                   CLEAR                   DSSI00
026000001030     C                   MOVEL     VIDNAR        DNAR
026100001106     C                   MOVEL     VIDCAP        DCAP
026200001106     C                   MOVEL     *BLANKS       DLOC
026300001106     C                   MOVEL     VIDLOC        DSIN
026400001030     C                   MOVEL     DEPOP0        WOP0
026500001030     C                   MOVEL     'S'           WOP0
026600001031     C                   MOVEL     'O09'         WOP1
026700001030     C                   MOVEL     'TISI35R'     WPGM
026800001030     C                   EXSR      LANPGM
026900970526     C*
027000970526     C                   ENDSR
027100970611     C*------------------------------------------------------------------------*
027200970611     C* O03GES - GESTIONE OPZIONE 3=DUPLICA
027300970611     C*------------------------------------------------------------------------*
027400970611     C     O03GES        BEGSR
027500970611     C*
027600970611     C*
027700970611     C                   ENDSR
027800970526     C*------------------------------------------------------------------------*
027900970526     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO
028000970526     C*------------------------------------------------------------------------*
028100970526     C     O04GES        BEGSR
028200970526     C*
028300970526     C*
028400970526     C                   ENDSR
028500970526     C*------------------------------------------------------------------------*
028600970526     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
028700970526     C*------------------------------------------------------------------------*
028800970526     C     O05GES        BEGSR
028900970526     C*
029000970526     C*
029100970526     C                   ENDSR
029200970526     C*------------------------------------------------------------------------*
029300970526     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO
029400970526     C*------------------------------------------------------------------------*
029500970526     C     O07GES        BEGSR
029600970526     C*
029700001030     C*
029800970526     C                   ENDSR
029900970529     C*------------------------------------------------------------------------*
030000970529     C* O13GES - GESTIONE OPZIONE 13=SINONIMI
030100970529     C*------------------------------------------------------------------------*
030200970529     C     O13GES        BEGSR
030300970529     C*
030400970529     C*
030500970529     C                   ENDSR
030600970526     C*------------------------------------------------------------------------*
030700001030     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
030800970526     C*------------------------------------------------------------------------*
030900970526     C     NEWPOS        BEGSR
031000970526     C*
031100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
031200970526     C*         1= RICHIESTO POSIZIONAMENTO
031300970526     C                   Z-ADD     0             SWPOSI            7 0
031400970526     C*
031500970526     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
031600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
031700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
031800970526     C                   Z-ADD     0             SWMODI            1 0
031900970530     C*
032000970530     C* REIMPOSTA INDICATORI DI ERRORE
032100970530     C                   SETOFF                                       808182
032200970526     C*
032300970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
032400001030     C     VIDPNA        IFNE      DEPNAR
032500970527     C     VIDPCA        ORNE      DEPCAP
032600970529     C     VIDPLO        ORNE      DEPLOC
032700001106     C     VIDRIC        ORNE      DEPRIC
032800970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
032900001030     C     ERRL00        IFEQ      '0'                                          *NO ERORI
033000970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
033100970527     C                   MOVEL     VIDPNA        DEPNAR                         *DEPOSITI
033200970527     C                   MOVEL     VIDPCA        DEPCAP
033300970529     C                   MOVEL     VIDPLO        DEPLOC
033400001106     C                   MOVEL     VIDRIC        DEPRIC
033500001030     C                   ENDIF
033600001030     C                   ENDIF
033700970526     C*
033800970526     C                   ENDSR
033900970526     C*------------------------------------------------------------------------*
034000970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
034100970526     C*------------------------------------------------------------------------*
034200970526     C     SETFIL        BEGSR
034300001030     C*
034400001030     C                   SETOFF                                       99
034500970529     C*
034600970529     C* POSIZIONAMENTO SUL FILE IN BASE AL TIPO DI RICERCA
034700001030     C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
034800001030     C/EXEC SQL
034900010713     C+ DECLARE C1 SCROLL CURSOR FOR
035000010713     C+ SELECT DISTINCT WFLNAR, WFLPRV, WFLCAP, WFLLOC, SUM(WFLSPE)
035100010713     C+ FROM WFLOC00F WHERE WFLNAR='   ' AND WFLLIA<'3'
035200010713     C+ GROUP BY WFLNAR, WFLPRV, WFLCAP, WFLLOC
035300010713     C+ ORDER BY WFLLOC
035400001030     C/END-EXEC
035500001030     C
035600001030     C                   IF        CHKCURSOR = '0'
035700001030     C/EXEC SQL
035800001030     C+ Open C1
035900001030     C/END-EXEC
036000001030     C                   MOVEL     '1'           CHKCURSOR
036100001030     C                   ELSE
036200001030     C/EXEC SQL
036300001030     C+ Fetch before from C1
036400001030     C/END-EXEC
036500001030     C                   ENDIF
036600001030     C                   ENDIF
036700001030     C*
036800001030     C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
036900001030     C/EXEC SQL
037000010713     C+ DECLARE C2 SCROLL CURSOR FOR
037100010713     C+ SELECT DISTINCT WFLNAR, WFLPRV, WFLCAP, WFLLOC, SUM(WFLSPE)
037200010713     C+ FROM WFLOC00F WHERE WFLNAR='   ' AND WFLLIA<'3'
037300010713     C+ GROUP BY WFLNAR, WFLPRV, WFLCAP, WFLLOC
037400010713     C+ ORDER BY WFLLOC
037500001030     C/END-EXEC
037600001030     C
037700001030     C                   IF        CHKCURSOR = '0'
037800001030     C/EXEC SQL
037900001030     C+ Open C2
038000001030     C/END-EXEC
038100001030     C                   MOVEL     '1'           CHKCURSOR
038200001030     C                   ELSE
038300001030     C/EXEC SQL
038400001030     C+ Fetch before from C2
038500001030     C/END-EXEC
038600001030     C                   ENDIF
038700001030     C                   ENDIF
038800001030     C*
038900970526     C* LETTURA RECORD
039000001030     C     SQLCOD        IFEQ      *ZEROS
039100970526     C                   EXSR      LETFIL
039200001030     C                   ENDIF
039300970526     C*
039400970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
039500001030     C     *IN99         IFEQ      *ON
039600970526     C                   Z-ADD     1             SFLEOF
039700001030     C                   ENDIF
039800970526     C*
039900970526     C                   ENDSR
040000970526     C*------------------------------------------------------------------------*
040100001030     C* LETFIL - LETTURA RECORD
040200970526     C*------------------------------------------------------------------------*
040300970526     C     LETFIL        BEGSR
040400970526     C*
040500970526     C* LEGGE FINO A:
040600970526     C*     - FINE FILE
040700970526     C*     - TROVATO RECORD VALIDO
040800970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
040900001030     C     *IN99         DOWEQ     *OFF
041000001030     C     WRECOK        ANDEQ     'N'
041100970529     C*
041200970529     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO DI RICERCA
041300001030     C     SQLCOD        IFEQ      *ZEROS
041400001030     C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
041500001030     C/EXEC SQL
041600001030     C+ Fetch C1 into :dswfloc
041700001030     C/END-EXEC
041800001030     C                   ENDIF
041900001030     C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
042000001030     C/EXEC SQL
042100001030     C+ Fetch C2 into :dswfloc
042200001030     C/END-EXEC
042300001030     C                   ENDIF
042400001030     C                   ELSE
042500001030     C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
042600001030     C/EXEC SQL
042700001030     C+ Close C1
042800001030     C/END-EXEC
042900001030     C                   MOVEL     '0'           CHKCURSOR
043000001030     C                   ENDIF
043100001030     C     WTIPRI        IFEQ      'LOC'                                        *PER LOCALITA'
043200001030     C/EXEC SQL
043300001030     C+ Close C2
043400001030     C/END-EXEC
043500001030     C                   MOVEL     '0'           CHKCURSOR
043600001030     C                   ENDIF
043700001030     C                   ENDIF
043800001030     C*
043900001030     C     SQLCOD        IFNE      *ZEROS
044000001030     C                   SETON                                        99
044100001030     C                   ENDIF
044200001030     C*
044300001030     C     *IN99         IFEQ      *ON                                          *FINE FILE
044400970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
044500001030     C                   ELSE
044600970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
044700001030     C                   ENDIF
044800001030     C                   ENDDO
044900970526     C*
045000970526     C                   ENDSR
045100970526     C*------------------------------------------------------------------------*
045200970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
045300970526     C*------------------------------------------------------------------------*
045400970526     C     CHKREC        BEGSR
045500970526     C*
045600970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
045700970530     C*
045800001106     C* SE RICHIESTO ORDINAMENTO O SELEZIONE FILTRA I RECORD
045900001106     C                   MOVE(P)   VIDRIC        NUMRIC            9 0
046000001106     C     WRECOK        IFEQ      'S'
046100001106     C     NUMRIC        ANDNE     *ZEROS
046200001106     C     NUMRIC        ANDGT     WFLSPE
046300001106     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046400001106     C                   ENDIF
046500001031     C     WRECOK        IFEQ      'S'
046600001031     C     VIDPNA        ANDNE     *BLANKS
046700001031     C     VIDPNA        ANDGT     WFLNAR
046800001031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
046900001031     C                   ENDIF
047000001031     C     WRECOK        IFEQ      'S'
047100001031     C     VIDPCA        ANDNE     *BLANKS
047200001031     C     VIDPCA        ANDGT     WFLCAP
047300001031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047400001031     C                   ENDIF
047500001031     C     WRECOK        IFEQ      'S'
047600001031     C     VIDPLO        ANDNE     *BLANKS
047700001031     C     VIDPLO        ANDGT     WFLLOC
047800001031     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047900001031     C                   ENDIF
048000001005     C*
048100970526     C                   ENDSR
048200970526     C*------------------------------------------------------------------------*
048300001030     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
048400970526     C*------------------------------------------------------------------------*
048500970526     C     IRESFL        BEGSR
048600970529     C*
048700970529     C                   MOVEL     *BLANKS       OPZKEY
048800001030     C                   MOVEL     WFLNAR        VIDNAR
048900001030     C                   MOVEL     WFLPRV        VIDPRV
049000001030     C                   MOVEL     WFLCAP        VIDCAP
049100001030     C                   MOVEL     WFLLOC        VIDLOC
049200001106     C                   Z-ADD     WFLSPE        VIDSPE
049300970526     C*
049400970526     C                   ENDSR
049500970526     C*------------------------------------------------------------------------*
049600970526     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
049700970526     C*------------------------------------------------------------------------*
049800970526     C     CHKPAR        BEGSR
049900970529     C*
050000970529     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA PER CAP
050100001030     C     VIDPCA        IFEQ      *BLANKS
050200970529     C     VIDPLO        ANDEQ     *BLANKS
050300970529     C                   MOVEL     'CAP'         WTIPRI            3
050400001030     C                   ENDIF
050500970529     C*
050600970529     C* ACCETTA SOLO UN PARAMETRO DI INIZIO RICERCA
050700001030     C     ERRL00        IFEQ      '0'
050800001030     C     VIDPCA        IFNE      *BLANKS
050900970529     C     VIDPLO        ANDNE     *BLANKS
051000970529     C                   MOVEL     MSG(6)        DSMSMS
051100970529     C                   MOVEL     '1'           ERRL00
051200970529     C                   SETON                                        808182
051300001030     C                   ENDIF
051400001030     C                   ENDIF
051500970529     C*
051600970529     C* CONTROLLA IL TIPO RICERCA
051700001030     C     ERRL00        IFEQ      '0'
051800001030     C     VIDPCA        IFNE      *BLANKS
051900970529     C                   MOVEL     'CAP'         WTIPRI
052000001030     C                   ENDIF
052100001030     C     VIDPLO        IFNE      *BLANKS
052200970529     C                   MOVEL     'LOC'         WTIPRI
052300001030     C                   ENDIF
052400001030     C                   ENDIF
052500970526     C*
052600970526     C                   ENDSR
052700970526     C*------------------------------------------------------------------------*
052800001030     C* LANPGM - LANCIA UN PROGRAMMA
052900970526     C*------------------------------------------------------------------------*
053000970526     C     LANPGM        BEGSR
053100970526     C*
053200970526     C* IMPOSTA LA DS DI PROCEDURA
053300970529     C                   MOVEL     WOP0          D00OP0                         *LIVELLO PROCEDURA
053400970528     C                   MOVEL     WOP1          D00OP1                         *LIVELLO PGM
053500970526     C                   MOVEL     '0'           D00F03                         *PREMUTO F03=FINE
053600970526     C                   MOVEL     '0'           D00F12                         *PREMUTO F12=RITORNO
053700970526     C                   MOVEL     '0'           D00ERR                         *'1'=ERRORE
053800970526     C                   MOVEL     *BLANKS       D00MSG                         *MESSAGGIO DI RITORN
053900970602     C                   MOVEL     DNAR          D00NAR                         *NAZIONE
054000970602     C                   MOVEL     DCAP          D00CAP                         *CAP
054100001106     C                   MOVEL     DSIN          D00SIN                         *SINONIMO
054200001106     C                   MOVEL     DLOC          D00LOC                         *LOCALITA'
054300001116     C                   Z-ADD     DATRIF        D00DRI
054400970603     C                   Z-ADD     *ZEROS        D00TTC
054500970530     C                   MOVEL     *BLANKS       D00TSP
054600970530     C                   Z-ADD     *ZEROS        D00TFP
054700970530     C                   MOVEL     *BLANKS       D00EPA
054800970530     C                   Z-ADD     *ZEROS        D00ETP
054900970530     C                   Z-ADD     *ZEROS        D00ETA
055000970530     C                   Z-ADD     *ZEROS        D00EDD
055100970530     C                   MOVEL     *BLANKS       D00SPT
055200970530     C                   Z-ADD     *ZEROS        D00SLP
055300970530     C                   Z-ADD     *ZEROS        D00SLA
055400970530     C                   MOVEL     *BLANKS       D00STS
055500970530     C                   Z-ADD     *ZEROS        D00SDD
055600970526     C*
055700970526     C                   CALL      WPGM
055800970612     C                   PARM                    KPJBA
055900970612     C                   PARM                    DSSI00
056000970526     C*
056100970526     C* SE RITORNA PREMUTO F3, CHIUDE IL PROGRAMMA
056200001030     C     D00F03        IFEQ      '1'                                          *PREMUTO F03
056300970526     C                   MOVEL     '1'           ERRL00                         *ERRORE
056400970526     C                   MOVEL     '1'           F03L00                         *FINE PGM
056500001030     C                   ELSE                                                   *NON PREMUTO F03
056600970526     C                   MOVEL     D00MSG        DSMSMS                         *MESSAGGIO DI RITORN
056700001030     C                   ENDIF
056800970526     C*
056900970526     C                   ENDSR
057000970526     C*------------------------------------------------------------------------*
057100970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
057200970526     C*------------------------------------------------------------------------*
057300970526     C     CHKFOP        BEGSR
057400970626     C*
057500001031     C* ABILITA SEMPRE E COMUNQUE SOLO L'OPZIONE DI "VERIFICA"
057600001106     C                   MOVEL     OPD(8)        OPDRIG                         *VERIFICA
057700001106     C                   MOVEL     'S'           OPDATT
057800001106     C                   MOVEL     OPDRIG        OPD(8)
057900001031     C                   MOVEL     OPD(9)        OPDRIG                         *VERIFICA
058000970626     C                   MOVEL     'S'           OPDATT
058100001031     C                   MOVEL     OPDRIG        OPD(9)
058200970526     C*
058300970526     C* GESTIONE OPZIONI A VIDEO
058400970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
058500970526     C                   EXSR      OPZGES
058600970526     C*
058700970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
058800970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
058900970526     C                   EXSR      FUNGES
059000970526     C*
059100970526     C                   ENDSR
059200970526     C*------------------------------------------------------------------------*
059300970526     C* *INZSR - OPERAZIONI INIZIALI
059400970526     C*------------------------------------------------------------------------*
059500970526     C     *INZSR        BEGSR
059600970526     C*---
059700970526     C* RICEVIMENTO PARAMETRI
059800970526     C*---
059900970526     C     *ENTRY        PLIST
060000970612     C                   PARM                    KPJBA
060100001030     C                   MOVEL     KPJBU         D00OP0
060200001030     C*
060300001030     C* IMPOSTO PER DEFAULT L'ORDINAMENTO PER LOCALITA'
060400001030     C                   MOVEL     'LOC'         WTIPRI
060500970526     C*
060600970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
060700970526     C     XMSGPA        PLIST
060800970526     C                   PARM                    DSMSMS
060900970526     C                   PARM                    DSMSPG
061000970526     C                   PARM                    DSMSKE
061100970526     C                   PARM                    DSMSER
061200970526     C*
061300970526     C* IMPOSTA LA DS -TASTI PREMUTI-
061400970526     C                   CALL      'XKEY'
061500970526     C                   PARM                    DSKEY
061600001030     C*---
061700001030     C* VARIABILI RIFERITE AL DATA BASE
061800001030     C*---
061900001030     C     *LIKE         DEFINE    VIDPCA        DEPCAP
062000001030     C     *LIKE         DEFINE    VIDPLO        DEPLOC
062100001030     C     *LIKE         DEFINE    VIDPNA        DEPNAR
062200001106     C     *LIKE         DEFINE    VIDRIC        DEPRIC
062300970526     C*---
062400970526     C* VARIABILI NON RIFERITE AL DATA BASE
062500970526     C*---
062600970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
062700970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
062800970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
062900970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
063000970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
063100970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
063200970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
063300970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
063400970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
063500970602     C     *LIKE         DEFINE    D00OP0        DEPOP0                         *DI DEPOSITO
063600970526     C     *LIKE         DEFINE    D00OP1        DEPOP1
063700970602     C     *LIKE         DEFINE    D00OP0        WOP0                           *DI LAVORO
063800970529     C     *LIKE         DEFINE    D00OP1        WOP1
063900970602     C     *LIKE         DEFINE    D00NAR        DNAR
064000970602     C     *LIKE         DEFINE    D00CAP        DCAP
064100001106     C     *LIKE         DEFINE    D00LOC        DLOC
064200001031     C     *LIKE         DEFINE    D00SIN        DSIN
064300970526     C*---
064400970526     C* VARIABILI DI CONTROLLO
064500970526     C*---
064600970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
064700970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
064800970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
064900970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
065000970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
065100970530     C                   Z-ADD     12            SFLPAG                         *N°RIGHE X PAG
065200001030     C                   MOVEL     '0'           CHKCURSOR         1
065300001106     C                   MOVEL     *BLANKS       WPGM              8
065400001106     C                   MOVEL     *ZEROS        VIDRIC
065500001106     C                   MOVEL     *ZEROS        DEPRIC
065600970526     C*
065700970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
065800001030     C                   MOVEL     'TISI77R'     DSMSPG
065900001030     C                   MOVEL     'TISI77R'     MSGQUE
066000970526     C                   MOVEL     *BLANKS       MSGKEY
066100970526     C                   MOVEL     *BLANKS       DSMSMS
066200970526     C                   MOVEL     *BLANKS       DSMSKE
066300970526     C                   Z-ADD     *ZEROS        DSMSER
066400001030     C*
066500001030     C* CALCOLA LA DATA CORRENTE
066600001030     C                   TIME                    WN14             14 0          *ORA (6)+ DATA (8)
066700001030     C                   MOVE      WN14          WN8               8 0          *DATA (8) IN G/M/AA
066800001030     C                   Z-ADD     WN8           DATA              8 0
066900001030     C                   Z-ADD     WN8           G08DAT
067000001030     C                   Z-ADD     *ZEROS        G08INV
067100001030     C                   MOVEL     '0'           G08ERR
067200001030     C                   CALL      'XSRDA8'
067300001030     C                   PARM                    WLBDA8
067400001030     C                   Z-ADD     G08INV        DATCOR            8 0          *DATA CORRENTE AA/M/
067500001116     C*
067600001116     C* REPERIMENTO DATA RIFERIMENTO ULTIMA VERSIONE ESISTENTE
067700001116     C     *HIVAL        SETGT     AZCPR01L
067800001116     C                   READP     AZCPR01L
067900001116     C                   IF        not %eof(AZCPR01L)
068000001116     C                   Z-ADD     CPRDDE        DATRIF            8 0
068100001116     C                   ELSE
068200001116     C                   Z-ADD     DATCOR        DATRIF
068300001116     C                   ENDIF
068400970526     C*
068500970526     C* DEPOSITA LE OPZIONI DI ENTRATA
068600970526     C                   MOVEL     D00OP0        DEPOP0                         *LIVELLO PROCEDURA
068700970526     C                   MOVEL     D00OP1        DEPOP1                         *LIVELLO PROGRAMMA
068800970526     C*
068900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
069000970526     C                   EXSR      CHKFOP
069100970526     C*
069200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
069300001030     C     DEPOP0        IFEQ      'L01'
069400970530     C                   MOVEL     DEC(3)        VIDING
069500001030     C                   ENDIF
069600001030     C     DEPOP0        IFEQ      'L02'
069700970530     C                   MOVEL     DEC(1)        VIDING
069800001030     C                   ENDIF
069900001030     C     DEPOP0        IFEQ      'L05'
070000970530     C                   MOVEL     DEC(2)        VIDING
070100001030     C                   ENDIF
070200970526     C*
070300970526     C                   ENDSR
070400970526     C*------------------------------------------------------------------------*
070500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
070800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
070900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
071000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
071200970526     C*------------------------------------------------------------------------*
071300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
071400970526     C*------------------------------------------------------------------------*
071500970526     C     POSSFL        BEGSR
071600970526     C*
071700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
071800970526     C                   EXSR      CLRSFL
071900970526     C*
072000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
072100970526     C                   EXSR      SETFIL
072200970526     C*
072300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
072400970526     C     SFLEOF        IFEQ      0
072500970526     C                   EXSR      CARSFL
072600970526     C                   END
072700970526     C*
072800970526     C                   ENDSR
072900970526     C*------------------------------------------------------------------------*
073000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
073100970526     C*------------------------------------------------------------------------*
073200970526     C     ENTATT        BEGSR
073300970526     C*
073400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
073500970526     C                   Z-ADD     0             OPZOK             1 0
073600970526     C*
073700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
073800970526     C                   Z-ADD     0             SFLVAR            5 0
073900970526     C*
074000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
074100970526     C                   Z-ADD     0             SFLERR
074200970526     C*
074300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
074400970526     C                   Z-ADD     SFLULT        WFLULT
074500970526     C*
074600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
074700001030     C                   READC     SI77D05                                98
074800970526     C     *IN98         DOWEQ     '0'
074900970526     C*
075000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
075100970526     C                   ADD       1             SFLVAR
075200970526     C*
075300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
075400970526     C     OPZOK         IFEQ      0
075500970526     C*
075600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
075700970526     C     OPZKEY        IFNE      *BLANKS
075800970526     C                   EXSR      OPZCON
075900970526     C                   END
076000970526     C*
076100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
076200970526     C*            2. PULIZIA SCELTA IN SFL
076300970526     C     OPZOK         IFEQ      0
076400970526     C                   MOVEL     '0'           *IN41
076500970526     C                   MOVEL     *BLANKS       OPZKEY
076600970526     C                   ELSE
076700970526     C*
076800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
076900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
077000970526     C                   MOVEL     '1'           *IN41
077100970526     C                   Z-ADD     SFLULT        SFLERR
077200970526     C                   END
077300970526     C                   ELSE
077400970526     C*
077500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
077600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
077700970526     C                   MOVEL     '1'           *IN41
077800970526     C                   END                                                    OPZOK=0
077900970526     C*
078000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
078100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
078200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
078300970526     C     OPZOK         IFNE      0
078400970526     C                   Z-ADD     SFLERR        SFLNBR
078500970526     C                   ELSE
078600970526     C                   Z-ADD     SFLULT        SFLNBR
078700970526     C                   END
078800970526     C*
078900970526     C* AGGIORNO RECORD SFL
079000001030     C                   UPDATE    SI77D05
079100970526     C*
079200001030     C                   READC     SI77D05                                98
079300970526     C                   END                                                    DO WHILE
079400970526     C*
079500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
079600970526     C                   Z-ADD     WFLULT        SFLULT
079700970526     C*
079800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
079900970526     C     SFLVAR        IFEQ      0
080000970526     C*---                 MOVEL'1'       FINL00
080100970526     C                   END
080200970526     C*
080300970526     C                   ENDSR
080400970526     C*------------------------------------------------------------------------*
080500970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
080600970526     C*------------------------------------------------------------------------*
080700970526     C     CLRSFL        BEGSR
080800970526     C*
080900970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
081000970526     C* (0=NON FINE FILE  1=FINE FILE)
081100970526     C                   Z-ADD     0             SFLEOF
081200970526     C*
081300970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
081400970526     C                   Z-ADD     0             SFLULT
081500970526     C*
081600970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
081700970526     C                   MOVEL     '1'           *IN40
081800001030     C                   WRITE     SI77D06
081900970526     C                   MOVEL     '0'           *IN40
082000970526     C*
082100970526     C                   ENDSR
082200970526     C*------------------------------------------------------------------------*
082300970526     C* CARSFL - CARICAMENTO SFL
082400970526     C*------------------------------------------------------------------------*
082500970526     C     CARSFL        BEGSR
082600970526     C*
082700970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
082800970526     C     SFLULT        IFEQ      0
082900970526     C                   EXSR      SETFIL
083000970526     C                   END
083100970526     C*
083200970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
083300970526     C                   Z-ADD     SFLULT        WFLULT
083400970526     C*
083500970526     C* CICLO:
083600970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
083700970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
083800970526     C                   Z-ADD     1             SFLRIG
083900970526     C     SFLRIG        DOWLE     SFLPAG
084000970526     C     SFLEOF        ANDEQ     0
084100970526     C*
084200970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
084300970526     C                   MOVEL     *BLANKS       OPZKEY
084400970526     C*
084500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
084600970526     C                   EXSR      IRESFL
084700970526     C*
084800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
084900970526     C                   ADD       1             SFLULT
085000970526     C*
085100970526     C* SCRITTURA RECORD SFL
085200001030     C                   WRITE     SI77D05
085300970526     C                   ADD       1             SFLRIG
085400970526     C*
085500970526     C* LETTURA FILE DA CARICARE
085600970526     C                   EXSR      LETFIL
085700970526     C                   END                                                    DO WHILE
085800970526     C*
085900970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
086000970526     C     WFLULT        IFNE      SFLULT
086100970526     C                   Z-ADD     WFLULT        SFLNBR
086200970526     C                   ADD       1             SFLNBR
086300970526     C                   END
086400970526     C*
086500970526     C                   ENDSR
086600970526     C*------------------------------------------------------------------------*
086700970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
086800970526     C*------------------------------------------------------------------------*
086900970526     C     OUTSFL        BEGSR
087000970526     C*
087100970526     C* WRITE RECORD TESTATA VIDEO
087200001030     C                   WRITE     SI77D01
087300970526     C*
087400970526     C* VISUALIZZAZIONE MESSAGGIO
087500970526     C                   EXSR      SNDMSG
087600970526     C*
087700970526     C* WRITE RECORD OPZIONI VIDEO
087800001030     C                   WRITE     SI77D02
087900970526     C*
088000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
088100001030     C                   WRITE     SI77D03
088200970526     C*
088300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
088400001030     C                   WRITE     SI77D04
088500970526     C*
088600970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
088700970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
088800970526     C     SFLEOF        IFEQ      0
088900970526     C                   MOVEL     '0'           *IN42
089000970526     C                   ELSE
089100970526     C*
089200970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
089300970526     C                   MOVEL     '1'           *IN42
089400970526     C                   END
089500970526     C*
089600970526     C* WRITE RECORD TASTI FUNZIONALI
089700001030     C                   WRITE     SI77D09
089800970526     C*
089900970526     C* CASO DI SFL VUOTO
090000970526     C     SFLULT        IFLE      0
090100970526     C*
090200970526     C*    - WRITE RECORD VUOTO
090300001030     C                   WRITE     SI77D0A
090400001030     C                   READ      SI77D0A                                99
090500970526     C*
090600970526     C                   ELSE
090700970526     C*
090800970526     C* CASO DI SFL CON RECORDS
090900970526     C*
091000970526     C*    - WRITE RECORD CONTROLLO SFL
091100001030     C                   WRITE     SI77D06
091200001030     C                   READ      SI77D06                                99
091300970526     C                   END
091400970526     C*
091500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
091600001030     C                   READ      SI77D03                                99
091700970526     C*
091800970526     C                   ENDSR
091900970526     C*------------------------------------------------------------------------*
092000970526     C* SNDMSG - MANDA MESSAGGIO
092100970526     C*------------------------------------------------------------------------*
092200970526     C     SNDMSG        BEGSR
092300970526     C*
092400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
092500970526     C                   MOVEL     '1'           *IN40
092600001030     C                   WRITE     SI77DMC
092700970526     C                   MOVEL     '0'           *IN40
092800970526     C*
092900970526     C* VISUALIZZAZIONE MESSAGGIO
093000970526     C                   Z-ADD     0             MSGULT
093100970526     C                   CALL      'XMSG'        XMSGPA
093200970526     C     DSMSER        IFEQ      0
093300970526     C                   MOVEL     DSMSKE        MSGKEY
093400970526     C                   END
093500970526     C                   Z-ADD     1             MSGULT
093600001030     C                   WRITE     SI77DMS                                      SFL MESS
093700001030     C                   WRITE     SI77DMC                                      CONTR MESS
093800970526     C*
093900970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
094000970526     C                   MOVEL     *BLANKS       DSMSMS
094100970526     C*
094200970526     C                   ENDSR
094300970526     C*------------------------------------------------------------------------*
094400970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
094500970526     C*------------------------------------------------------------------------*
094600970526     C     FUNCON        BEGSR
094700970526     C*
094800970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
094900970526     C                   EXSR      FUNVAR
095000970526     C*
095100970526     C* PULISCO CAMPI DI LAVORO
095200970526     C                   Z-ADD     0             I                 5 0
095300970526     C                   Z-ADD     0             W                 5 0
095400970526     C*
095500970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
095600970526     C     FUNKEY        IFNE      *BLANKS
095700970526     C     1             DO        24            I
095800970526     C                   MOVEL     FUD(I)        FUDRIG
095900970526     C*
096000970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
096100970526     C     FUNKEY        IFEQ      FUDFUN
096200970526     C     FUDATT        ANDEQ     'S'
096300970526     C                   Z-ADD     I             W
096400970526     C                   END
096500970526     C                   END                                                    DO WHILE
096600970526     C*
096700970526     C* FUNZIONE NON ATTIVA (ERRORE)
096800970526     C     W             IFEQ      0
096900970526     C*
097000970526     C* EMISSIONE MESSAGGIO DI ERRORE
097100970526     C                   MOVEL     MSG(2)        DSMSMS
097200970526     C                   END                                                    W=0
097300970526     C                   END                                                    FUNKEY NE *BLANKS
097400970526     C*
097500970526     C* FUNZIONE ATTIVA (PROCEDI ...)
097600970526     C     W             IFNE      0
097700970526     C     FUNKEY        OREQ      *BLANKS
097800970526     C*
097900970526     C     FUNKEY        IFEQ      *BLANKS
098000970526     C                   EXSR      ENTGES
098100970526     C                   END
098200970526     C     FUNKEY        IFEQ      '03'
098300970526     C                   EXSR      F03GES
098400970526     C                   END
098500970526     C     FUNKEY        IFEQ      '05'
098600970526     C                   EXSR      F05GES
098700970526     C                   END
098800970526     C     FUNKEY        IFEQ      '06'
098900970526     C                   EXSR      F06GES
099000970526     C                   END
099100970526     C     FUNKEY        IFEQ      '12'
099200970526     C                   EXSR      F12GES
099300970526     C                   END
099400970529     C     FUNKEY        IFEQ      '13'
099500970529     C                   EXSR      F13GES
099600970529     C                   END
099700970526     C     FUNKEY        IFEQ      '23'
099800970526     C                   EXSR      F23GES
099900970526     C                   END
100000970526     C     FUNKEY        IFEQ      '24'
100100970526     C                   EXSR      F24GES
100200970526     C                   END
100300970526     C     FUNKEY        IFEQ      'RU'
100400970526     C                   EXSR      RUPGES
100500970526     C                   END
100600970526     C*
100700970526     C                   END
100800970526     C*
100900970526     C                   ENDSR
101000970526     C*------------------------------------------------------------------------*
101100970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
101200970526     C*------------------------------------------------------------------------*
101300970526     C     FUNVAR        BEGSR
101400970526     C*
101500970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
101600970526     C     KEY           IFEQ      ENTER
101700970526     C                   MOVEL     *BLANKS       FUNKEY
101800970526     C                   END
101900970526     C     KEY           IFEQ      F1
102000970526     C                   MOVEL     '01'          FUNKEY
102100970526     C                   END
102200970526     C     KEY           IFEQ      F2
102300970526     C                   MOVEL     '02'          FUNKEY
102400970526     C                   END
102500970526     C     KEY           IFEQ      F3
102600970526     C                   MOVEL     '03'          FUNKEY
102700970526     C                   END
102800970526     C     KEY           IFEQ      F4
102900970526     C                   MOVEL     '04'          FUNKEY
103000970526     C                   END
103100970526     C     KEY           IFEQ      F5
103200970526     C                   MOVEL     '05'          FUNKEY
103300970526     C                   END
103400970526     C     KEY           IFEQ      F6
103500970526     C                   MOVEL     '06'          FUNKEY
103600970526     C                   END
103700970526     C     KEY           IFEQ      F7
103800970526     C                   MOVEL     '07'          FUNKEY
103900970526     C                   END
104000970526     C     KEY           IFEQ      F8
104100970526     C                   MOVEL     '08'          FUNKEY
104200970526     C                   END
104300970526     C     KEY           IFEQ      F9
104400970526     C                   MOVEL     '09'          FUNKEY
104500970526     C                   END
104600970526     C     KEY           IFEQ      F10
104700970526     C                   MOVEL     '10'          FUNKEY
104800970526     C                   END
104900970526     C     KEY           IFEQ      F11
105000970526     C                   MOVEL     '11'          FUNKEY
105100970526     C                   END
105200970526     C     KEY           IFEQ      F12
105300970526     C                   MOVEL     '12'          FUNKEY
105400970526     C                   END
105500970526     C     KEY           IFEQ      F13
105600970526     C                   MOVEL     '13'          FUNKEY
105700970526     C                   END
105800970526     C     KEY           IFEQ      F14
105900970526     C                   MOVEL     '14'          FUNKEY
106000970526     C                   END
106100970526     C     KEY           IFEQ      F15
106200970526     C                   MOVEL     '15'          FUNKEY
106300970526     C                   END
106400970526     C     KEY           IFEQ      F16
106500970526     C                   MOVEL     '16'          FUNKEY
106600970526     C                   END
106700970526     C     KEY           IFEQ      F17
106800970526     C                   MOVEL     '17'          FUNKEY
106900970526     C                   END
107000970526     C     KEY           IFEQ      F18
107100970526     C                   MOVEL     '18'          FUNKEY
107200970526     C                   END
107300970526     C     KEY           IFEQ      F19
107400970526     C                   MOVEL     '19'          FUNKEY
107500970526     C                   END
107600970526     C     KEY           IFEQ      F20
107700970526     C                   MOVEL     '20'          FUNKEY
107800970526     C                   END
107900970526     C     KEY           IFEQ      F21
108000970526     C                   MOVEL     '21'          FUNKEY
108100970526     C                   END
108200970526     C     KEY           IFEQ      F22
108300970526     C                   MOVEL     '22'          FUNKEY
108400970526     C                   END
108500970526     C     KEY           IFEQ      F23
108600970526     C                   MOVEL     '23'          FUNKEY
108700970526     C                   END
108800970526     C     KEY           IFEQ      F24
108900970526     C                   MOVEL     '24'          FUNKEY
109000970526     C                   END
109100970526     C     KEY           IFEQ      ROLLUP
109200970526     C                   MOVEL     'RU'          FUNKEY
109300970526     C                   END
109400970526     C     KEY           IFEQ      HELP
109500970526     C                   MOVEL     'HP'          FUNKEY
109600970526     C                   END
109700970526     C*
109800970526     C                   ENDSR
109900970526     C*------------------------------------------------------------------------*
110000970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
110100970526     C*------------------------------------------------------------------------*
110200970526     C     FUNGES        BEGSR
110300970526     C*
110400970526     C* PULISCO CAMPI DI LAVORO
110500970526     C                   Z-ADD     0             W                 5 0
110600970526     C*
110700970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
110800970526     C* E CARICO TABELLA FUNZIONI ATTIVE
110900970526     C     1             DO        24            I
111000970526     C                   MOVEL     FUD(I)        FUDRIG
111100970526     C*
111200970526     C* PULIZIA FUNZIONI PRECEDENTI
111300970526     C                   MOVEL     *BLANKS       FUV(I)
111400970526     C*
111500970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
111600970526     C* (SALTA F23 E F24)
111700970526     C     FUDATT        IFEQ      'S'
111800970526     C     FUDFUN        ANDNE     '23'
111900970526     C     FUDFUN        ANDNE     '24'
112000970526     C                   ADD       1             W
112100970526     C                   MOVEL     FUDDES        FUV(W)
112200970526     C                   END
112300970526     C                   END                                                    DO WHILE
112400970526     C*
112500970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
112600970526     C* ALTRIMENTI DISATTIVO F24
112700970526     C     1             DO        24            I
112800970526     C                   MOVEL     FUD(I)        FUDRIG
112900970526     C*
113000970526     C     FUDFUN        IFEQ      '24'
113100970526     C     W             IFGT      6
113200970526     C                   MOVEL     'S'           FUDATT
113300970526     C                   ELSE
113400970526     C                   MOVEL     *BLANKS       FUDATT
113500970526     C                   END                                                    W GT 8
113600970526     C                   MOVEL     FUDRIG        FUD(I)
113700970526     C                   END
113800970526     C                   END                                                    DO WHILE
113900970526     C*
114000970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
114100970526     C     W             DIV       6             I
114200970526     C                   MVR                     Z
114300970526     C*
114400970526     C     Z             IFNE      0
114500970526     C     I             ADD       1             Z
114600970526     C                   ELSE
114700970526     C                   Z-ADD     I             Z
114800970526     C                   END
114900970526     C*
115000970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
115100970526     C                   ADD       1             FUNGRU
115200970526     C     FUNGRU        IFGT      Z
115300970526     C     FUNGRU        ORLT      1
115400970526     C                   Z-ADD     1             FUNGRU
115500970526     C                   END
115600970526     C*
115700970526     C* PRIMO GRUPPO DI 6 FUNZIONI
115800970526     C     FUNGRU        IFEQ      1
115900970526     C                   MOVEL     FUV(1)        FUN001
116000970526     C                   MOVEL     FUV(2)        FUN002
116100970526     C                   MOVEL     FUV(3)        FUN003
116200970526     C                   MOVEL     FUV(4)        FUN004
116300970526     C                   MOVEL     FUV(5)        FUN005
116400970526     C                   MOVEL     FUV(6)        FUN006
116500970526     C                   END
116600970526     C*
116700970526     C* SECONDO GRUPPO DI 6 FUNZIONI
116800970526     C     FUNGRU        IFEQ      2
116900970526     C                   MOVEL     FUV(7)        FUN001
117000970526     C                   MOVEL     FUV(8)        FUN002
117100970526     C                   MOVEL     FUV(9)        FUN003
117200970526     C                   MOVEL     FUV(10)       FUN004
117300970526     C                   MOVEL     FUV(11)       FUN005
117400970526     C                   MOVEL     FUV(12)       FUN006
117500970526     C                   END
117600970526     C*
117700970526     C* TERZO GRUPPO DI 6 FUNZIONI
117800970526     C     FUNGRU        IFEQ      3
117900970526     C                   MOVEL     FUV(13)       FUN001
118000970526     C                   MOVEL     FUV(14)       FUN002
118100970526     C                   MOVEL     FUV(15)       FUN003
118200970526     C                   MOVEL     FUV(16)       FUN004
118300970526     C                   MOVEL     FUV(17)       FUN005
118400970526     C                   MOVEL     FUV(18)       FUN006
118500970526     C                   END
118600970526     C*
118700970526     C* QUARTO GRUPPO DI 6 FUNZIONI
118800970526     C     FUNGRU        IFEQ      4
118900970526     C                   MOVEL     FUV(19)       FUN001
119000970526     C                   MOVEL     FUV(20)       FUN002
119100970526     C                   MOVEL     FUV(21)       FUN003
119200970526     C                   MOVEL     FUV(22)       FUN004
119300970526     C                   MOVEL     *BLANKS       FUN005
119400970526     C                   MOVEL     *BLANKS       FUN006
119500970526     C                   END
119600970526     C*
119700970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
119800970526     C                   MOVEL     *BLANKS       FUN007
119900970526     C                   MOVEL     *BLANKS       FUN008
120000970526     C*
120100970526     C     1             DO        24            I
120200970526     C                   MOVEL     FUD(I)        FUDRIG
120300970526     C     FUDFUN        IFEQ      '23'
120400970526     C     FUDATT        ANDEQ     'S'
120500970526     C                   MOVEL     FUDDES        FUN007
120600970526     C                   END
120700970526     C     FUDFUN        IFEQ      '24'
120800970526     C     FUDATT        ANDEQ     'S'
120900970526     C                   MOVEL     FUDDES        FUN008
121000970526     C                   END
121100970526     C                   END                                                    DO
121200970526     C*
121300970526     C                   ENDSR
121400970526     C*------------------------------------------------------------------------*
121500970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
121600970526     C*------------------------------------------------------------------------*
121700970526     C     OPZGES        BEGSR
121800970526     C*
121900970526     C* PULISCO CAMPI DI LAVORO
122000970526     C                   Z-ADD     0             W                 5 0
122100970526     C*
122200970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
122300970526     C* E CARICO TABELLA OPZIONI ATTIVE
122400970526     C     1             DO        24            I
122500970526     C                   MOVEL     OPD(I)        OPDRIG
122600970526     C*
122700970526     C* PULIZIA OPZIONI PRECEDENTI
122800970526     C                   MOVEL     *BLANKS       OPV(I)
122900970526     C*
123000970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
123100970526     C     OPDATT        IFEQ      'S'
123200970526     C                   ADD       1             W
123300970526     C                   MOVEL     OPDDES        OPV(W)
123400970526     C                   END
123500970526     C                   END                                                    DO WHILE
123600970526     C*
123700970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
123800970526     C* ALTRIMENTI DISATTIVO F23
123900970526     C     1             DO        24            I
124000970526     C                   MOVEL     FUD(I)        FUDRIG
124100970526     C*
124200970526     C     FUDFUN        IFEQ      '23'
124300970526     C     W             IFGT      8
124400970526     C                   MOVEL     'S'           FUDATT
124500970526     C                   ELSE
124600970526     C                   MOVEL     *BLANKS       FUDATT
124700970526     C                   END                                                    W GT 8
124800970526     C                   MOVEL     FUDRIG        FUD(I)
124900970526     C                   END
125000970526     C                   END                                                    DO WHILE
125100970526     C*
125200970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
125300970526     C     W             DIV       8             I
125400970526     C                   MVR                     Z                 5 0
125500970526     C*
125600970526     C     Z             IFNE      0
125700970526     C     I             ADD       1             Z
125800970526     C                   ELSE
125900970526     C                   Z-ADD     I             Z
126000970526     C                   END
126100970526     C*
126200970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
126300970526     C                   ADD       1             OPZGRU
126400970526     C     OPZGRU        IFGT      Z
126500970526     C     OPZGRU        ORLT      1
126600970526     C                   Z-ADD     1             OPZGRU
126700970526     C                   END
126800970526     C*
126900970526     C* PRIMO GRUPPO DI 8 OPZIONI
127000970526     C     OPZGRU        IFEQ      1
127100970526     C                   MOVEL     OPV(1)        OPZ001
127200970526     C                   MOVEL     OPV(2)        OPZ002
127300970526     C                   MOVEL     OPV(3)        OPZ003
127400970526     C                   MOVEL     OPV(4)        OPZ004
127500970526     C                   MOVEL     OPV(5)        OPZ005
127600970526     C                   MOVEL     OPV(6)        OPZ006
127700970526     C                   MOVEL     OPV(7)        OPZ007
127800970526     C                   MOVEL     OPV(8)        OPZ008
127900970526     C                   END
128000970526     C*
128100970526     C* SECONDO GRUPPO DI 8 OPZIONI
128200970526     C     OPZGRU        IFEQ      2
128300970526     C                   MOVEL     OPV(9)        OPZ001
128400970526     C                   MOVEL     OPV(10)       OPZ002
128500970526     C                   MOVEL     OPV(11)       OPZ003
128600970526     C                   MOVEL     OPV(12)       OPZ004
128700970526     C                   MOVEL     OPV(13)       OPZ005
128800970526     C                   MOVEL     OPV(14)       OPZ006
128900970526     C                   MOVEL     OPV(15)       OPZ007
129000970526     C                   MOVEL     OPV(16)       OPZ008
129100970526     C                   END
129200970526     C*
129300970526     C* TERZO GRUPPO DI 8 OPZIONI
129400970526     C     OPZGRU        IFEQ      3
129500970526     C                   MOVEL     OPV(17)       OPZ001
129600970526     C                   MOVEL     OPV(18)       OPZ002
129700970526     C                   MOVEL     OPV(19)       OPZ003
129800970526     C                   MOVEL     OPV(20)       OPZ004
129900970526     C                   MOVEL     OPV(21)       OPZ005
130000970526     C                   MOVEL     OPV(22)       OPZ006
130100970526     C                   MOVEL     OPV(23)       OPZ007
130200970526     C                   MOVEL     OPV(24)       OPZ008
130300970526     C                   END
130400970526     C*
130500970526     C                   ENDSR
130600970526     C*------------------------------------------------------------------------*
130700970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
130800970526     C*------------------------------------------------------------------------*
130900970526     C     OPZCON        BEGSR
131000970526     C*
131100970526     C* PULISCO CAMPI DI LAVORO
131200970526     C                   Z-ADD     0             I                 5 0
131300970526     C                   Z-ADD     0             W                 5 0
131400970526     C*
131500970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
131600970526     C     1             DO        24            I
131700970526     C                   MOVEL     OPD(I)        OPDRIG
131800970526     C*
131900970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
132000970526     C     OPZKEY        IFEQ      OPDOPZ
132100970526     C     OPZKEY        OREQ      OPDOP1
132200970526     C*
132300970526     C     OPDATT        IFEQ      'S'
132400970526     C                   MOVEL     OPDOPZ        OPZKEY
132500970526     C                   Z-ADD     I             W
132600970526     C                   END
132700970526     C                   END
132800970526     C*
132900970526     C                   END                                                    DO WHILE
133000970526     C*
133100970526     C* OPZIONE NON ATTIVA (ERRORE)
133200970526     C     W             IFEQ      0
133300970526     C*
133400970526     C* OPZIONE NON OK
133500970526     C                   Z-ADD     1             OPZOK
133600970526     C*
133700970526     C* EMISSIONE MESSAGGIO DI ERRORE
133800970526     C                   MOVEL     MSG(1)        DSMSMS
133900970526     C                   ELSE
134000970526     C*
134100970526     C* OPZIONE ATTIVA (PROCEDI ...)
134200970526     C*
134300970526     C     OPZKEY        IFEQ      ' 1'
134400970526     C                   EXSR      O01GES
134500970526     C                   END
134600001106     C     OPZKEY        IFEQ      ' 8'
134700001106     C                   EXSR      O08GES
134800001106     C                   END
134900001031     C     OPZKEY        IFEQ      ' 9'
135000001031     C                   EXSR      O09GES
135100970526     C                   END
135200970611     C     OPZKEY        IFEQ      ' 3'
135300970611     C                   EXSR      O03GES
135400970611     C                   END
135500970526     C     OPZKEY        IFEQ      ' 4'
135600970526     C                   EXSR      O04GES
135700970526     C                   END
135800970526     C     OPZKEY        IFEQ      ' 5'
135900970526     C                   EXSR      O05GES
136000970526     C                   END
136100970526     C     OPZKEY        IFEQ      ' 7'
136200970526     C                   EXSR      O07GES
136300970526     C                   END
136400970529     C     OPZKEY        IFEQ      '13'
136500970529     C                   EXSR      O13GES
136600970529     C                   END
136700970526     C                   END                                                    OPZ.NON ATTIVA
136800970526     C*
136900970526     C                   ENDSR
137000970526     C*------------------------------------------------------------------------*
137100970526     C* NOTE COSTRUZIONE SCHIERE
137200970526     C*------------------------------------------------------------------------*
137300970526     C*
137400970526     C* -MSG-MESSAGGI
137500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
137600970526     C* (NON TOCCARE MSG,1 E MSG,2)
137700970526     C*
137800970526     C* -CMD-COMANDI
137900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
138000970526     C*
138100970526     C* -OPD-OPZIONI
138200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
138300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
138400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
138500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
138600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
138700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
138800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
138900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
139000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
139100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
139200970526     C*
139300970526     C* -FUD-FUNZIONI
139400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
139500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
139600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
139700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
139800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
139900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
140000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
140100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
140200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
140300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
140400970526     O*------------------------------------------------------------------------*
140500970526** DEC - DECODIFICHE
140600970526    GESTIONE                                                     1
140700970530INTERROGAZIONE                                                   2
140800970530     SCELTA                                                      3
1409009705264
1410009705265
141100970526** MSG - MESSAGGI
141200001030TISI77R- Opzione non gestita                                                   1
141300001030TISI77R- Tasto funzionale non gestito                                          2
141400001030TISI77R- 3
141500001030TISI77R- 4
141600001030TISI77R- 5
141700001030TISI77R- Accettato solo un parametro di inizio lista                           6
141800001030TISI77R- 7                                                                     7
141900001030TISI77R- 8                                                                     8
142000001030TISI77R- 9                                                                     9
142100001030TISI77R- 10                                                                   10
142200001030TISI77R- 11                                                                   11
142300001030TISI77R- 12                                                                   12
142400001030TISI77R- 13                                                                   13
142500001030TISI77R- 14                                                                   14
142600001030TISI77R- 15                                                                   15
142700001030TISI77R- 16                                                                   16
142800001030TISI77R- 17                                                                   17
142900001030TISI77R- 18                                                                   18
143000001030TISI77R- 19                                                                   19
143100001030TISI77R- 20                                                                   20
143200970526** OPD - OPZIONI
143300001030                   -1 -     (S/N)=N                        opz. 01¬
143400001031                   -2 -     (S/N)=N                        opz. 02¬
143500001030                   -3 -     (S/N)=N                        opz. 03¬
143600001030                   -4 -     (S/N)=N                        opz. 04¬
143700001030                   -5 -     (S/N)=N                        opz. 05¬
143800970526                   -6 -     (S/N)=N                        opz. 06¬
143900001030                   -7 -     (S/N)=N                        opz. 07¬
144000001106 8=Verif. x LOC    -8 -     (S/N)=S                        opz. 08¬
144100001106 9=Verif. x CAP    -9 -     (S/N)=S                        opz. 09¬
144200970527                   -10-     (S/N)=N                        opz. 10¬
144300970529                   -11-     (S/N)=N                        opz. 11¬
144400970529                   -12-     (S/N)=N                        opz. 12¬
144500001030                   -13-     (S/N)=N                        opz. 13¬
144600970526                   -  -     (S/N)=N                        opz. 14¬
144700970526                   -  -     (S/N)=N                        opz. 15¬
144800970526                   -  -     (S/N)=N                        opz. 16¬
144900970526                   -  -     (S/N)=N                        opz. 17¬
145000970526                   -  -     (S/N)=N                        opz. 18¬
145100970526                   -  -     (S/N)=N                        opz. 19¬
145200970526                   -  -     (S/N)=N                        opz. 20¬
145300970526                   -  -     (S/N)=N                        opz. 21¬
145400970526                   -  -     (S/N)=N                        opz. 22¬
145500970526                   -  -     (S/N)=N                        opz. 23¬
145600970526                   -  -     (S/N)=N                        opz. 24¬
145700970526** FUD - FUNZIONI
145800970526                   -        (S/N)=N                        funz.01¬
145900970526                   -        (S/N)=N                        funz.02¬
146000970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
146100970526                   -        (S/N)=N                        funz.04¬
146200970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
146300001030                   -        (S/N)=N                        funz.06¬
146400970526                   -        (S/N)=N                        funz.07¬
146500970527                   -        (S/N)=N                        funz.08¬
146600970527                   -        (S/N)=N                        funz.09¬
146700970527                   -        (S/N)=N                        funz.10¬
146800970526                   -        (S/N)=N                        funz.11¬
146900970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
147000001030                   -        (S/N)=N                        funz.13¬
147100970526                   -        (S/N)=N                        funz.14¬
147200970526                   -        (S/N)=N                        funz.15¬
147300970526                   -        (S/N)=N                        funz.16¬
147400970526                   -        (S/N)=N                        funz.17¬
147500970526                   -        (S/N)=N                        funz.18¬
147600970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
147700970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
147800970527                   -        (S/N)=N                        funz.21¬
147900970527                   -        (S/N)=N                        funz.22¬
148000970526F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
148100970526F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
