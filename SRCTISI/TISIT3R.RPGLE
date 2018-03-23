000100970526     H*------------------------------------------------------------------------*
000200020415     H* Gestione file sinonimi Destinatari disagiati/supermercati (AZDSS00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800020415     FTISIT3D   CF   E             WORKSTN INFDS(DEVDS1)
000900020415     F                                     SFILE(SIT3D05:SFLULT)
001000020415     F                                     SFILE(SIT3DMS:MSGULT)
001100970526     F*------------
001200040415     Fazdss01l  IF   E           K DISK
001300040415     Fazdss02l  IF   E           K DISK
001400020415     F                                     RENAME(azdss000:azdssxx2)
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
002600970528     D*------------
002700970528     D* COMANDI
002800970528     D*------------
002900970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003000970526     D*------------
003100970526     D* DESCRIZIONI OPZIONI
003200970526     D*------------
003300970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003400970526     D*------------
003500970526     D* DESCRIZIONI TASTI FUNZIONALI
003600970526     D*------------
003700970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003800970526     D*------------
003900970526     D* OPZIONI DA CARICARE A VIDEO
004000970526     D*------------
004100970526     D OPV             S             19    DIM(24)
004200970526     D*------------
004300970526     D* FUNZIONI DA CARICARE A VIDEO
004400970526     D*------------
004500970527     D FUV             S             19    DIM(24)
004600991108     D*------------
004700970526     D*------------------------------------------------------------------------*
004800970526     D* INPUT
004900970526     D*------------------------------------------------------------------------*
005000970526     D*------------
005100970526     D* RIDEFINIZIONE OPZIONI VIDEO
005200970526     D*------------
005300970526     D OPDDS           DS
005400970526     D  OPDOPZ                 1      2
005500970526     D  OPDDE1                 3     19
005600970526     D  OPDOP1                21     22
005700970526     D  OPDFI0                23     34
005800970526     D  OPDATT                35     35
005900970526     D  OPDFI1                36     80
006000970526     D  OPDDES                 1     19
006100970526     D  OPDRIG                 1     80
006200970526     D*------------
006300970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006400970526     D*------------
006500970526     D FUDDS           DS
006600970526     D  FUDDE1                 1      1
006700970526     D  FUDFUN                 2      3
006800970526     D  FUDDE2                 4     19
006900970526     D  FUDFI0                20     34
007000970526     D  FUDATT                35     35
007100970526     D  FUDFI1                36     80
007200970526     D  FUDDES                 1     19
007300970526     D  FUDRIG                 1     80
007400970526     D*------------
007500970526     D* DS GESTIONE TASTI FUNZIONALI
007600970526     D*------------
007700970526     D DSKEY         E DS
007800970526     D DEVDS1          DS
007900970526     D  KEY                  369    369
008000970526     D*------------
008100970526     D* DS "XMSG" - GESTIONE MESSAGGI
008200970526     D*------------
008300970526     D DSMSG         E DS
008400970526     D*------------
008500991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
008600020709     D*------------
008700970527     D WLBDA8          DS                  INZ
008800970527     D  G08DAT                 1      8  0
008900970527     D  G08INV                 9     16  0
009000970527     D  G08ERR                17     17
009100970527     D  G08TGI                18     22  0
009200040603     D*------------
009300040603     D* DS REPERIMENTO DATI UTENTE
009400040603     D*------------
009500040603     D TIBS34DS      E DS                                                       *Profili utente
009600040603     D DDATIUTE      E DS                                                       *Dati utente
009700040603     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009800040603     D DUTE01        E DS                                                       *Abilitazioni
009900970526     D*------------
010000991110     D* DS DI PROCEDURA
010100970526     D*------------
010200970526     D* ARCHITETTURA
010300970526     D*------------
010400970526     D KPJBA         E DS
010500060104      *
010600060104     d Status         sds           333
010700060104     d   SDSpgm          *proc
010800991108     D*------------------
010900991110     D* DS - SCOMPOSIZIONE MESSAGGIO A VIDEO
011000991110     D*------------------
011100991110     D                 DS                  INZ
011200991110     D  MS1                    1     40
011300991110     D  MS2                   41     80
011400991110     D  MESS                   1     80
011500991108     D*------------------
011600020412     D* DS DI PROCEDURA X PASSAGGIO PARAMETRI
011700000407     D*------------------
011800020412     D TISIT0DS      E DS                  INZ
011900020415     D*------------------
012000020415     D* DS DI RIDEFINIZIONE PARAMETRI IN INPUT
012100020415     D*------------------
012200040625     D PARAM           DS
012300040625     D  DEPOP0                        3
012400040625     D  DEPPRO                        1
012500040625     D  DEPLNA                        3  0
012600020412     D*------------------
012700020412     D* VARIABILI DI WRK
012800020412     D*------------------
012900020412     D wrkRICERCA      S             35    VARYING
013000040603     D DEPTISIT0DS     S                   LIKE(TISIT0DS)
013100970526     C*------------------------------------------------------------------------*
013200970526     C* MAIN LINES
013300970526     C*------------------------------------------------------------------------*
013400970526     C*
013500970526     C* SE NON CI SONO ERRORI, PROSEGUE
013600970526IF  1C     ERRL00        IFNE      '1'
013700020426     C*
013800020426     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
013900020426     C                   EXSR      NEWPOS
014000970526     C*
014100970526     C* CARICA IL SFILE
014200970526     C                   EXSR      POSSFL
014300970526     C*
014400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014500970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014600970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
014700970526     C     FINL00        OREQ      '1'                                          *FINE PGM
014800970526     C*
014900970526     C* REIMPOSTA ERRORE DI PROCERURA
015000970526     C                   MOVEL     '0'           ERRL00
015100970526     C*
015200970526     C* EMETTE IL SFILE
015300970526     C                   EXSR      OUTSFL
015400970526     C*
015500970526     C* GESTIONE TASTI FUNZIONALI
015600970526     C                   EXSR      FUNCON
015700970526     C*
015800970526E   2C                   ENDDO
015900970527     C*
016000970526E   1C                   ENDIF
016100991108     C*
016200970526     C* FINE PGM
016300970526     C                   SETON                                        LR
016400970526     C*------------------------------------------------------------------------*
016500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016600970526     C*------------------------------------------------------------------------*
016700970526     C     F03GES        BEGSR
016800970526     C*
016900970526     C* IMPOSTA "PREMUTO F03"
017000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017100970526     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F05GES        BEGSR
017700970526     C*
017800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017900970526     C*
018000970526     C                   ENDSR
018100970526     C*------------------------------------------------------------------------*
018200970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
018300970526     C*------------------------------------------------------------------------*
018400000407     C     F06GES        BEGSR
018500000407     C*
018600020426     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018700020426     C                   IF        SAVOP1 = 'O13' OR
018800020426     C                             SAVOP1 = 'P13'
018900020415     C                   ELSE
019000020415     C                   CLEAR                   TISIT0DS
019100020415     C                   ENDIF
019200021223     C                   IF        SAVOP1 <> *BLANKS
019300020426     C                   MOVEL     SAVOP1        IT0OP1
019400021223     C                   ELSE
019500021223     C                   MOVEL     'F06'         IT0OP1
019600021223     C                   ENDIF
019700020415     C                   CALL      'TISIT4R'
019800000407     C                   PARM                    KPJBA
019900020412     C                   PARM                    TISIT0DS
020000000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
020100020426     C                   ENDIF
020200000407     C*
020300970526     C                   ENDSR
020400970526     C*------------------------------------------------------------------------*
020500991108     C* F12GES - GESTIONE F12 -> RITORNO
020600970526     C*------------------------------------------------------------------------*
020700970526     C     F12GES        BEGSR
020800970526     C*
020900970526     C* IMPOSTA "PREMUTO F12"
021000970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021100970526     C*
021200970526     C                   ENDSR
021300970528     C*------------------------------------------------------------------------*
021400970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021500970528     C*------------------------------------------------------------------------*
021600970528     C     F21GES        BEGSR
021700970612     C*
021800991117     C* NON GESTITA
021900970528     C*
022000970528     C                   ENDSR
022100970526     C*------------------------------------------------------------------------*
022200970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022300970526     C*------------------------------------------------------------------------*
022400970526     C     F23GES        BEGSR
022500970526     C*
022600970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022700970526     C*
022800970526     C                   ENDSR
022900970526     C*------------------------------------------------------------------------*
023000970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023100970526     C*------------------------------------------------------------------------*
023200970526     C     F24GES        BEGSR
023300970526     C*
023400970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023500970526     C*
023600970526     C                   ENDSR
023700970526     C*------------------------------------------------------------------------*
023800970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023900970526     C*------------------------------------------------------------------------*
024000970526     C     RUPGES        BEGSR
024100970526     C*
024200970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024300970526     C*
024400970526     C                   ENDSR
024500970526     C*------------------------------------------------------------------------*
024600991108     C* ENTGES - GESTIONE ENTER -> INVIO
024700970526     C*------------------------------------------------------------------------*
024800970526     C     ENTGES        BEGSR
024900000407     C*
025000000407     C                   MOVEL     '0'           ERRL00
025100991108     C*
025200970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025300970526     C                   EXSR      NEWPOS
025400970526     C*
025500991108     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025600970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025700970526IF  2C     SWPOSI        IFNE      0
025800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025900970526X   2C                   ELSE
026000970526IF  3C     SFLULT        IFNE      0
026100970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026200970526     C*
026300970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026400970526IF  4C     SWMODI        IFNE      0
026500970526     C                   EXSR      POSSFL
026600970526E   4C                   ENDIF
026700970526E   3C                   ENDIF
026800970526E   2C                   ENDIF
026900970526E   1C                   ENDIF
027000991108     C*
027100970526     C                   ENDSR
027200000420     C*------------------------------------------------------------------------*
027300000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027400000420     C*------------------------------------------------------------------------*
027500000420     C     O01GES        BEGSR
027600000420     C*
027700000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027800020412     C                   EXSR      PASPAR
027900000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
028000000420     C*
028100000420     C                   ENDSR
028200000407     C*------------------------------------------------------------------------*
028300000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
028400000407     C*------------------------------------------------------------------------*
028500000407     C     O02GES        BEGSR
028600000407     C*
028700000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028800000411     C     VIDATB        IFEQ      'A'
028900000411     C                   MOVEL     MSG(4)        DSMSMS
029000000411     C                   ELSE
029100020412     C                   CLEAR                   TISIT0DS
029200020412     C                   MOVEL     'O02'         IT0OP1
029300000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
029400020415     C                   CALL      'TISIT4R'
029500000407     C                   PARM                    KPJBA
029600020412     C                   PARM                    TISIT0DS
029700000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
029800000407E   1C                   ENDIF
029900000411     C                   ENDIF
030000000407     C*
030100000407     C                   ENDSR
030200020412     C*------------------------------------------------------------------------*
030300020412     C* O03GES - GESTIONE OPZIONE 3=COPIA RECORD
030400020412     C*------------------------------------------------------------------------*
030500020412     C     O03GES        BEGSR
030600020412     C*
030700020412IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030800020412     C     VIDATB        IFEQ      'A'
030900020412     C                   MOVEL     MSG(4)        DSMSMS
031000020412     C                   ELSE
031100020412     C                   CLEAR                   TISIT0DS
031200020412     C                   MOVEL     'O03'         IT0OP1
031300020412     C                   EXSR      PASPAR                                       *RICARICA SFILE
031400020415     C                   CALL      'TISIT4R'
031500020412     C                   PARM                    KPJBA
031600020412     C                   PARM                    TISIT0DS
031700020412     C*                  EXSR      POSSFL                                       *RICARICA SFILE
031800020412E   1C                   ENDIF
031900020412     C                   ENDIF
032000020412     C*
032100020412     C                   ENDSR
032200991112     C*------------------------------------------------------------------------*
032300000407     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
032400991112     C*------------------------------------------------------------------------*
032500991112     C     O04GES        BEGSR
032600991112     C*
032700000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032800000411     C     VIDATB        IFEQ      'A'
032900000411     C                   MOVEL     MSG(4)        DSMSMS
033000000411     C                   ELSE
033100020412     C                   CLEAR                   TISIT0DS
033200020412     C                   MOVEL     'O04'         IT0OP1
033300000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
033400020415     C                   CALL      'TISIT4R'
033500000407     C                   PARM                    KPJBA
033600020412     C                   PARM                    TISIT0DS
033700000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
033800000407E   1C                   ENDIF
033900000411     C                   ENDIF
034000991112     C*
034100991112     C                   ENDSR
034200991014     C*------------------------------------------------------------------------*
034300000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
034400970526     C*------------------------------------------------------------------------*
034500970526     C     O05GES        BEGSR
034600000407     C*
034700000407IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034800020412     C                   CLEAR                   TISIT0DS
034900020412     C                   MOVEL     'O05'         IT0OP1
035000000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
035100020415     C                   CALL      'TISIT4R'
035200000407     C                   PARM                    KPJBA
035300020412     C                   PARM                    TISIT0DS
035400000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
035500000407E   1C                   ENDIF
035600000407     C*
035700970526     C                   ENDSR
035800000411     C*------------------------------------------------------------------------*
035900000411     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO RECORD
036000000411     C*------------------------------------------------------------------------*
036100000411     C     O07GES        BEGSR
036200000411     C*
036300000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036400000411     C     VIDATB        IFEQ      *BLANKS
036500000411     C                   MOVEL     MSG(5)        DSMSMS
036600000411     C                   ELSE
036700020412     C                   CLEAR                   TISIT0DS
036800020412     C                   MOVEL     'O07'         IT0OP1
036900000411     C                   EXSR      PASPAR                                       *RICARICA SFILE
037000020415     C                   CALL      'TISIT4R'
037100000411     C                   PARM                    KPJBA
037200020412     C                   PARM                    TISIT0DS
037300000411     C*                  EXSR      POSSFL                                       *RICARICA SFILE
037400000411E   1C                   ENDIF
037500000411     C                   ENDIF
037600000411     C*
037700000411     C                   ENDSR
037800991028     C*------------------------------------------------------------------------*
037900991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
038000991112     C*------------------------------------------------------------------------*
038100991112     C     PASPAR        BEGSR
038200991112     C*
038300020412     C                   EVAL      IT0NAZ = VIHNAZ
038400020415     C                   EVAL      IT0PRV = VIHPRV
038500020412     C                   EVAL      IT0CAP = VIDCAP
038600020412     C                   EVAL      IT0LOC = VIHLOC
038700020416     C                   EVAL      IT0KWI = VIHKWI
038800020412     C                   EVAL      IT0RAG = VIHRAG
038900020415     C                   EVAL      IT0SIN = VIHSIN
039000991112     C*
039100020412     C                   ENDSR
039200970526     C*------------------------------------------------------------------------*
039300970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
039400970526     C*------------------------------------------------------------------------*
039500970526     C     NEWPOS        BEGSR
039600970526     C*
039700970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
039800970526     C*         1= RICHIESTO POSIZIONAMENTO
039900970526     C                   Z-ADD     0             SWPOSI            7 0
040000970526     C*
040100991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
040200970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
040300970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
040400970526     C                   Z-ADD     0             SWMODI            1 0
040500970530     C*
040600970530     C* REIMPOSTA INDICATORI DI ERRORE
040700991108     C                   SETOFF                                       808182
040800991108     C                   SETOFF                                       83
040900970526     C*
041000970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
041100020424     C     VIDRS2        IFNE      deprs2
041200040419     C     VIDLO2        ORNE      deplo2
041300000407     C     VIDCA1        ORNE      depca1
041400020424     C     VIDSN1        ORNE      depSN1
041500970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
041600020412     C     ERRL00        IFEQ      '0'                                          *NO ERORI
041700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
041800020424     C                   MOVEL     VIDRS2        deprs2                         *DEPOSITI
041900040419     C                   MOVEL     VIDLO2        deplo2                         *DEPOSITI
042000000407     C                   MOVEL     VIDCA1        depca1                         *DEPOSITI
042100020424     C                   MOVEL     VIDSN1        depsn1                         *DEPOSITI
042200020412     C                   ENDIF
042300020412     C                   ENDIF
042400970526     C*
042500970526     C                   ENDSR
042600970526     C*------------------------------------------------------------------------*
042700970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
042800970526     C*------------------------------------------------------------------------*
042900970528     C     SETFIL        BEGSR
043000970526     C*
043100970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
043200020424     C     VIDSN1        IFEQ      *zeros
043300020424     C                   MOVEL     *loval        KDSSIN
043400991108     C                   ELSE
043500020424     C                   MOVEL     VIDSN1        KDSSIN
043600991108     C                   ENDIF
043700000407     C     VIDCA1        IFEQ      *zeros
043800000407     C     VIDCA1        OREQ      *blanks
043900020412     C                   MOVEL     *loval        KDSCAP
044000991108     C                   ELSE
044100020412     C                   MOVEL     VIDCA1        KDSCAP
044200991108     C                   ENDIF
044300020412     C*
044400020412     C     WTIPRI        IFEQ      'CAP'
044500020415     C     KEYDSS1       SETLL     azdss01l                           99
044600020412     C                   ENDIF
044700020424     C     WTIPRI        IFEQ      'SIN'
044800020415     C     KEYDSS2       SETLL     azdss02l                           99
044900020412     C                   ENDIF
045000980107     C*
045100970526     C* LETTURA RECORD
045200020412     C     *IN99         IFEQ      *OFF
045300970526     C                   EXSR      LETFIL
045400020412     C                   ENDIF
045500970526     C*
045600970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
045700020412     C     *IN99         IFEQ      *ON
045800970526     C                   Z-ADD     1             SFLEOF
045900020412     C                   ENDIF
046000970526     C*
046100970526     C                   ENDSR
046200970526     C*------------------------------------------------------------------------*
046300970526     C* LETFIL - LETTURA RECORD
046400970526     C*------------------------------------------------------------------------*
046500970526     C     LETFIL        BEGSR
046600970526     C*
046700970526     C* LEGGE FINO A:
046800970526     C*     - FINE FILE
046900970526     C*     - TROVATO RECORD VALIDO
047000970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
047100020412     C     *IN99         DOWEQ     *OFF
047200970526     C     WRECOK        ANDEQ     'N'
047300970526     C*
047400970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
047500020412     C     WTIPRI        IFEQ      'CAP'
047600020415     C                   READ      azdss01l                               99
047700020412     C                   ENDIF
047800020424     C     WTIPRI        IFEQ      'SIN'
047900020415     C                   READ      azdss02l                               99
048000020412     C                   ENDIF
048100020412     C*
048200020412     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
048300020412     C     *IN99         IFEQ      *ON                                          *FINE FILE
048400020412     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
048500020412     C                   ELSE
048600970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
048700020412     C                   ENDIF
048800020412     C                   ENDDO
048900020412     C*
049000020412     C                   ENDSR
049100020412     C*------------------------------------------------------------------------*
049200020412     C* CHKREC - CONTROLLA VALIDITA' RECORD
049300020412     C*------------------------------------------------------------------------*
049400020412     C     CHKREC        BEGSR
049500020412     C*
049600970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
049700020412     C*
049800020412     C* FILTRA SE RICHIESTE SELEZIONI
049900050418     C                   IF        WRECOK = 'S'
050000020424     C     VIDRS2        IFEQ      *blanks
050100020412     C                   ELSE
050200020424     C                   EVAL      %len(wrkRICERCA) = %len(%trim(VIDRS2))
050300020424     C                   EVAL      wrkRICERCA = %trim(VIDRS2)
050400020424     C     wrkRICERCA    SCAN      DSSRAG
050500020412     C                   IF        not %found
050600991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
050700020412     C                   ENDIF
050800020412     C                   ENDIF
050900050418     C                   ENDIF
051000991108     C*
051100050418     C                   IF        WRECOK = 'S'
051200040419     C     VIDLO2        IFEQ      *blanks
051300040419     C                   ELSE
051400040419     C                   EVAL      %len(wrkRICERCA) = %len(%trim(VIDLO2))
051500040419     C                   EVAL      wrkRICERCA = %trim(VIDLO2)
051600040419     C     wrkRICERCA    SCAN      DSSLOC
051700040419     C                   IF        not %found
051800040419     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
051900040419     C                   ENDIF
052000040419     C                   ENDIF
052100050418     C                   ENDIF
052200050418     C*
052300050418     C* Se entrata x sinonimi da Anagrafica "principale" verifico che corrisponda ANCHE la
052400050418     C* key-word d ricerca indirizzo d provenienza
052500050418     C                   IF        WRECOK = 'S'
052600050418     C                   IF        IT0OP1 = 'O13'
052700050418     C                   IF        DSSKWI = IT0KWI
052800050418     C                   ELSE
052900050418     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
053000050418     C                   ENDIF
053100050418     C                   ENDIF
053200050418     C                   ENDIF
053300040419     C*
053400970526     C                   ENDSR
053500970526     C*------------------------------------------------------------------------*
053600000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
053700970526     C*------------------------------------------------------------------------*
053800970526     C     IRESFL        BEGSR
053900970526     C*
054000000407     C                   MOVEL     *blanks       OPZKEY
054100020415     C                   MOVEL     DSSATB        VIDATB
054200020415     C                   MOVEL     DSSRAG        VIDRAG
054300020415     C                   MOVEL     DSSRAG        VIHRAG
054400020415     C                   MOVEL     DSSSIN        VIDSIN
054500020415     C                   MOVEL     DSSSIN        VIHSIN
054600020415     C                   MOVEL     DSSNAZ        VIHNAZ
054700020415     C                   MOVEL     DSSPRV        VIHPRV
054800020415     C                   MOVEL     DSSCAP        VIDCAP
054900040419     C                   MOVEL     DSSLOC        VIDLOC
055000020415     C                   MOVEL     DSSLOC        VIHLOC
055100020415     C                   MOVEL     DSSKWI        VIDKWI
055200020415     C                   MOVEL     DSSKWI        VIHKWI
055300991108     C*
055400970526     C                   ENDSR
055500970526     C*------------------------------------------------------------------------*
055600991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
055700970526     C*------------------------------------------------------------------------*
055800970526     C     CHKPAR        BEGSR
055900970528     C*
056000020412     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO X RAGIONE SOCIALE
056100020424     C                   MOVEL     'SIN'         WTIPRI            3
056200000407     C*
056300020424     C     VIDSN1        IFEQ      *blanks
056400000407     C     VIDCA1        ANDEQ     *blanks
056500020424     C                   MOVEL     'SIN'         WTIPRI
056600020412     C                   ENDIF                                                  *ORDINAMENTO DEFAULT
056700970528     C*
056800970528     C* CONTROLLA IL TIPO RICERCA
056900000407     C     ERRL00        IFEQ      '0'
057000020424     C     VIDSN1        IFNE      *blanks
057100020424     C                   MOVEL     'SIN'         WTIPRI
057200000407     C                   ENDIF
057300000407     C     VIDCA1        IFNE      *blanks
057400000407     C                   MOVEL     'CAP'         WTIPRI
057500000407     C                   ENDIF
057600000407     C                   ENDIF
057700000407     C*
057800970526     C                   ENDSR
057900970526     C*------------------------------------------------------------------------*
058000970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
058100970526     C*------------------------------------------------------------------------*
058200970526     C     CHKFOP        BEGSR
058300991112     C*
058400991112     C* OPZIONI
058500970626     C*
058600000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
058700000420IF  1C     DEPOP0        IFEQ      'P01'
058800000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
058900000420     C                   MOVEL     'S'           OPDATT
059000000420     C                   MOVEL     OPDRIG        OPD(1)
059100000420X   1C                   ELSE
059200000420     C* OPZIONI
059300970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
059400970626     C                   MOVEL     'S'           OPDATT
059500970626     C                   MOVEL     OPDRIG        OPD(5)
059600970626E   1C                   ENDIF
059700970626     C*
059800000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
059900970626IF  1C     DEPOP0        IFEQ      'P02'
060000000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
060100970626     C                   MOVEL     'S'           OPDATT
060200000407     C                   MOVEL     OPDRIG        OPD(2)
060300020412     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
060400020412     C                   MOVEL     'S'           OPDATT
060500020412     C                   MOVEL     OPDRIG        OPD(3)
060600000411     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
060700000411     C                   MOVEL     'S'           OPDATT
060800000411     C                   MOVEL     OPDRIG        OPD(4)
060900991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
061000991112     C                   MOVEL     'S'           OPDATT
061100991112     C                   MOVEL     OPDRIG        OPD(5)
061200020412     C                   MOVEL     OPD(13)       OPDRIG                         *CREA SINONIMO
061300020412     C                   MOVEL     'S'           OPDATT
061400020412     C                   MOVEL     OPDRIG        OPD(13)
061500040601     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
061600000407     C                   MOVEL     'S'           FUDATT
061700000407     C                   MOVEL     FUDRIG        FUD(6)
061800970626E   1C                   ENDIF
061900970526     C*
062000970526     C* GESTIONE OPZIONI A VIDEO
062100970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
062200970526     C                   EXSR      OPZGES
062300970526     C*
062400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
062500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
062600970526     C                   EXSR      FUNGES
062700970526     C*
062800970526     C                   ENDSR
062900040603     C*--------------------------------------------------------------------------------------------*
063000040603     C* REPERISCE DATI UTENTE
063100040603     C*--------------------------------------------------------------------------------------------*
063200040603     C     REPDATIUTE    BEGSR
063300040603     C*
063400040603     C* REimposta le variabili di lavoro
063500040603     C                   CLEAR                   TIBS34DS
063600040603     C                   CLEAR                   AZUTEDS
063700040603     C                   CLEAR                   DDATIUTE
063800040603     C*
063900040603     C* Reperisce i dati del profilo in ingresso
064000040603     C     *DTAARA       DEFINE    §AZUTE        AZUTEDS
064100040603     C     *DTAARA       DEFINE    §DATIUTE      DDATIUTE
064200040603     C                   IN(E)     *DTAARA
064300040603     C                   IF        %Error                                       *NO errori
064400040603     C                   EVAL      I34TLA = 'L'
064500040603     C                   CALL      'TIBS34R'
064600040603     C                   PARM                    TIBS34DS
064700040603     C                   IN        *DTAARA
064800040603     C                   ENDIF
064900040603     C*
065000040603     C* REPERISCO IL LIVELLO DI AUTIZZAZIONE SPECIFICO X QUESTO *PGM DAL FILE UTENTI
065100040603     C                   EVAL      DUTE01 = UTEFAF
065200040603     C*
065300040603     C                   ENDSR
065400970526     C*------------------------------------------------------------------------*
065500970526     C* *INZSR - OPERAZIONI INIZIALI
065600970526     C*------------------------------------------------------------------------*
065700970526     C     *INZSR        BEGSR
065800970526     C*---
065900970526     C* RICEVIMENTO PARAMETRI
066000970526     C*---
066100970526     C     *ENTRY        PLIST
066200970612     C                   PARM                    KPJBA
066300040603     C                   PARM                    DEPTISIT0DS
066400040603     C                   MOVEL     KPJBU         PARAM
066500060104     c                   movel     SDSpgm        V1Tpgm
066600040603     C*
066700040603     C* TESO IL NUMERO DI PARAMETRI PASSATO
066800040603     C                   IF        %PARMS = 1
066900040603     C                   MOVEL     '1'           NumPARMS          1
067000040603     C                   ENDIF
067100040603     C*
067200040603     C* REPERISCE I DATI DELL'UTENTE
067300040603     C                   EXSR      REPDATIUTE
067400040603     C*
067500040603     C* IN BASE ALLE AUTIZZAZIONI UTENTE PILOTO IL LIVELLO DI GESTIONE (QUINDI NN + DA KAZN)
067600040603     C* ...SE ENTRATA NN IN "SCELTA" VALORIZZO
067700040603     C                   IF        DEPOP0  <> 'P01'
067800040603     C* ...DI DEFAULT IMPOSTO SOLO VISUALIZZAZIONE
067900040603     C                   EVAL      DEPOP0  = 'P05'
068000040603     C* ...SE PREVISTO IMPOSTO GESTIONE COMPLETA
068100040603     C                   IF        §UTEDSS = 'S'
068200040603     C                   EVAL      DEPOP0  = 'P02'
068300040603     C                   ENDIF
068400040603     C                   ENDIF
068500040603     C*
068600040603     C* BY-PASSO L'ENTRATA IN KPJBU ORIGINALE CON QUELLA APPENA STABILITA
068700040625     C                   MOVEL     PARAM         KPJBU
068800040625     C*
068900040625     C* SE RICHIAMO DA PROPOSTE INIBISCO I CAMPI DI SELEZIONE A VIDEO
069000040625     C                   IF        DEPPRO = 'P' OR
069100040625     C                             DEPPRO = 'A'
069200040625     C                   SETON                                        55
069300040625     C                   ENDIF
069400020415     C*
069500020426     C* TESTO SUBITO I PARAMETRI RICEVUTI X DETERMINARE L'OPZIOINE/FUNZIONE RICHIESTA
069600040603     C                   IF        NumPARMS <> '1'
069700020415     C                   EVAL      TISIT0DS = DEPTISIT0DS
069800020416     C                   IF        IT0OP1 = 'O13' OR
069900020416     C                             IT0OP1 = 'P13'
070000020424     C                   EVAL      VIDRS2 = IT0RAG
070100040419     C                   EVAL      VIDLO2 = IT0LOC
070200020415     C                   EVAL      VIDCA1 = IT0CAP
070300020424     C                   EVAL      VIDSN1 = IT0SIN
070400020415     C                   ENDIF
070500040603     C                   ENDIF
070600020426     C*
070700020426     C* SALVO L'OPZIONE/FUNZIONE INIZIALE RICEVUTA IN INGRESSO (X DIVERSI LIVELLI DI CHIAMATA)
070800020426     C                   MOVEL     IT0OP1        SAVOP1            3
070900970526     C*
071000970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
071100970526     C     XMSGPA        PLIST
071200970526     C                   PARM                    DSMSMS
071300970526     C                   PARM                    DSMSPG
071400970526     C                   PARM                    DSMSKE
071500970526     C                   PARM                    DSMSER
071600970526     C*
071700970526     C* IMPOSTA LA DS -TASTI PREMUTI-
071800970526     C                   CALL      'XKEY'
071900970526     C                   PARM                    DSKEY
072000970526     C*---
072100970526     C* VARIABILI NON RIFERITE AL DATA BASE
072200970526     C*---
072300970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
072400970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
072500970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
072600970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
072700970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
072800970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
072900970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
073000970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
073100970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
073200970526     C*---
073300020424     C     *LIKE         DEFINE    vidrs2        deprs2
073400040419     C     *LIKE         DEFINE    vidlo2        deplo2
073500000407     C     *LIKE         DEFINE    vidca1        depca1
073600020424     C     *LIKE         DEFINE    vidsn1        depsn1
073700020424     C     *LIKE         DEFINE    dssrag        kdsrag
073800020424     C     *LIKE         DEFINE    dsssin        kdssin
073900020415     C     *LIKE         DEFINE    dsscap        kdscap
074000991108     C*---
074100970526     C* VARIABILI DI CONTROLLO
074200970526     C*---
074300970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
074400970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
074500970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
074600970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
074700970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
074800000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
074900970526     C*
075000970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
075100020415     C                   MOVEL     'TISIT3R'     DSMSPG
075200020415     C                   MOVEL     'TISIT3R'     MSGQUE
075300970526     C                   MOVEL     *BLANKS       MSGKEY
075400970526     C                   MOVEL     *BLANKS       DSMSMS
075500970526     C                   MOVEL     *BLANKS       DSMSKE
075600970526     C                   Z-ADD     *ZEROS        DSMSER
075700970526     C*---
075800970526     C* CHIAVI DI LETTURA
075900970526     C*---
076000020415     C* LETTURA azdss01l -
076100020415     C     KEYDSS1       KLIST
076200020412     C                   KFLD                    KDSCAP
076300020412     C                   KFLD                    KDSRAG
076400020415     C* LETTURA azdss02l -
076500020415     C     KEYDSS2       KLIST
076600020424     C                   KFLD                    KDSSIN
076700020412     C                   KFLD                    KDSCAP
076800970526     C*
076900970526     C* IMPOSTA I PARAMETRI DI INGRESSO
077000970526     C                   EXSR      CHKPAR
077100991115     C*
077200970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
077300970526     C                   EXSR      CHKFOP
077400970526     C*
077500970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
077600020412     C     DEPOP0        IFEQ      'P01'
077700040603     C                   MOVEL     DEC(1)        VIDING
077800020412     C                   ENDIF
077900020412     C     DEPOP0        IFEQ      'P02'
078000040603     C                   MOVEL     DEC(2)        VIDING
078100020412     C                   ENDIF
078200020412     C     DEPOP0        IFEQ      'P05'
078300040603     C                   MOVEL     DEC(5)        VIDING
078400020412     C                   ENDIF
078500020412     C*
078600020412     C                   ENDSR
078700970526     C*------------------------------------------------------------------------*
078800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
078900970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
079000970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
079100970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
079200970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
079300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
079400970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
079500970526     C*------------------------------------------------------------------------*
079600970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
079700970526     C*------------------------------------------------------------------------*
079800970526     C     POSSFL        BEGSR
079900970526     C*
080000970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
080100970526     C                   EXSR      CLRSFL
080200970526     C*
080300970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
080400970526     C                   EXSR      SETFIL
080500970526     C*
080600970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
080700970526     C     SFLEOF        IFEQ      0
080800970526     C                   EXSR      CARSFL
080900970526     C                   END
081000970526     C*
081100970526     C                   ENDSR
081200970526     C*------------------------------------------------------------------------*
081300970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
081400970526     C*------------------------------------------------------------------------*
081500970526     C     ENTATT        BEGSR
081600970526     C*
081700970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
081800970526     C                   Z-ADD     0             OPZOK             1 0
081900970526     C*
082000970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
082100970526     C                   Z-ADD     0             SFLVAR            5 0
082200970526     C*
082300970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
082400970526     C                   Z-ADD     0             SFLERR
082500970526     C*
082600970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
082700970526     C                   Z-ADD     SFLULT        WFLULT
082800970526     C*
082900970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
083000020415     C                   READC     SIT3D05                                98
083100970526     C     *IN98         DOWEQ     '0'
083200970526     C*
083300970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
083400970526     C                   ADD       1             SFLVAR
083500970526     C*
083600970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
083700970526     C     OPZOK         IFEQ      0
083800970526     C*
083900970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
084000970526     C     OPZKEY        IFNE      *BLANKS
084100970526     C                   EXSR      OPZCON
084200970526     C                   END
084300970526     C*
084400970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
084500970526     C*            2. PULIZIA SCELTA IN SFL
084600970526     C     OPZOK         IFEQ      0
084700970526     C                   MOVEL     '0'           *IN41
084800970526     C                   MOVEL     *BLANKS       OPZKEY
084900970526     C                   ELSE
085000970526     C*
085100970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
085200970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
085300970526     C                   MOVEL     '1'           *IN41
085400970526     C                   Z-ADD     SFLULT        SFLERR
085500970526     C                   END
085600970526     C                   ELSE
085700970526     C*
085800970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
085900970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
086000970526     C                   MOVEL     '1'           *IN41
086100970526     C                   END                                                    OPZOK=0
086200970526     C*
086300970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
086400970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
086500970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
086600970526     C     OPZOK         IFNE      0
086700970526     C                   Z-ADD     SFLERR        SFLNBR
086800970526     C                   ELSE
086900970526     C                   Z-ADD     SFLULT        SFLNBR
087000970526     C                   END
087100970526     C*
087200970526     C* AGGIORNO RECORD SFL
087300020415     C                   UPDATE    SIT3D05
087400970526     C*
087500020415     C                   READC     SIT3D05                                98
087600970526     C                   END                                                    DO WHILE
087700970526     C*
087800970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
087900970526     C                   Z-ADD     WFLULT        SFLULT
088000970526     C*
088100970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
088200970526     C     SFLVAR        IFEQ      0
088300970526     C                   END
088400970526     C*
088500040325     C***  OPZKEY        IFNE      ' 5'
088600040325     C***                EXSR      F05GES
088700040325     C***                ENDIF
088800991112     C*
088900970526     C                   ENDSR
089000970526     C*------------------------------------------------------------------------*
089100970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
089200970526     C*------------------------------------------------------------------------*
089300970526     C     CLRSFL        BEGSR
089400970526     C*
089500970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
089600970526     C* (0=NON FINE FILE  1=FINE FILE)
089700970526     C                   Z-ADD     0             SFLEOF
089800970526     C*
089900970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
090000970526     C                   Z-ADD     0             SFLULT
090100970526     C*
090200970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
090300970526     C                   MOVEL     '1'           *IN40
090400020415     C                   WRITE     SIT3D06
090500970526     C                   MOVEL     '0'           *IN40
090600970526     C*
090700970526     C                   ENDSR
090800970526     C*------------------------------------------------------------------------*
090900970526     C* CARSFL - CARICAMENTO SFL
091000970526     C*------------------------------------------------------------------------*
091100970526     C     CARSFL        BEGSR
091200970526     C*
091300970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
091400970526     C     SFLULT        IFEQ      0
091500970526     C                   EXSR      SETFIL
091600970526     C                   END
091700970526     C*
091800970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
091900970526     C                   Z-ADD     SFLULT        WFLULT
092000970526     C*
092100970526     C* CICLO:
092200970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
092300970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
092400970526     C                   Z-ADD     1             SFLRIG
092500970526     C     SFLRIG        DOWLE     SFLPAG
092600970526     C     SFLEOF        ANDEQ     0
092700970526     C*
092800970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
092900991116     C                   MOVEL     *BLANKS       OPZKEY
093000970526     C*
093100970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
093200970526     C                   EXSR      IRESFL
093300970526     C*
093400970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
093500970526     C                   ADD       1             SFLULT
093600970526     C*
093700970526     C* SCRITTURA RECORD SFL
093800020415     C                   WRITE     SIT3D05
093900970526     C                   ADD       1             SFLRIG
094000970526     C*
094100970526     C* LETTURA FILE DA CARICARE
094200970526     C                   EXSR      LETFIL
094300970526     C                   END                                                    DO WHILE
094400970526     C*
094500970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
094600970526     C     WFLULT        IFNE      SFLULT
094700970526     C                   Z-ADD     WFLULT        SFLNBR
094800970526     C                   ADD       1             SFLNBR
094900970526     C                   END
095000970526     C*
095100970526     C                   ENDSR
095200970526     C*------------------------------------------------------------------------*
095300970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
095400970526     C*------------------------------------------------------------------------*
095500970526     C     OUTSFL        BEGSR
095600970526     C*
095700970526     C* WRITE RECORD TESTATA VIDEO
095800020415     C                   WRITE     SIT3D01
095900970526     C*
096000970526     C* VISUALIZZAZIONE MESSAGGIO
096100970526     C                   EXSR      SNDMSG
096200970526     C*
096300970526     C* WRITE RECORD OPZIONI VIDEO
096400020415     C                   WRITE     SIT3D02
096500970526     C*
096600970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
096700020415     C                   WRITE     SIT3D03
096800970526     C*
096900970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
097000020415     C                   WRITE     SIT3D04
097100970526     C*
097200970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
097300970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
097400970526     C     SFLEOF        IFEQ      0
097500970526     C                   MOVEL     '0'           *IN42
097600970526     C                   ELSE
097700970526     C*
097800970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
097900970526     C                   MOVEL     '1'           *IN42
098000970526     C                   END
098100970526     C*
098200970526     C* WRITE RECORD TASTI FUNZIONALI
098300020415     C                   WRITE     SIT3D09
098400970526     C*
098500970526     C* CASO DI SFL VUOTO
098600970526     C     SFLULT        IFLE      0
098700970526     C*
098800970526     C*    - WRITE RECORD VUOTO
098900020415     C                   WRITE     SIT3D0A
099000020415     C                   READ      SIT3D0A                                99
099100970526     C*
099200970526     C                   ELSE
099300970526     C*
099400970526     C* CASO DI SFL CON RECORDS
099500970526     C*
099600970526     C*    - WRITE RECORD CONTROLLO SFL
099700020415     C                   WRITE     SIT3D06
099800020415     C                   READ      SIT3D06                                99
099900970526     C                   END
100000970526     C*
100100970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
100200020415     C                   READ      SIT3D03                                99
100300970526     C*
100400970526     C                   ENDSR
100500970526     C*------------------------------------------------------------------------*
100600970526     C* SNDMSG - MANDA MESSAGGIO
100700970526     C*------------------------------------------------------------------------*
100800970526     C     SNDMSG        BEGSR
100900970526     C*
101000970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
101100970526     C                   MOVEL     '1'           *IN40
101200020415     C                   WRITE     SIT3DMC
101300970526     C                   MOVEL     '0'           *IN40
101400970526     C*
101500970526     C* VISUALIZZAZIONE MESSAGGIO
101600970526     C                   Z-ADD     0             MSGULT
101700970526     C                   CALL      'XMSG'        XMSGPA
101800970526     C     DSMSER        IFEQ      0
101900970526     C                   MOVEL     DSMSKE        MSGKEY
102000970526     C                   END
102100970526     C                   Z-ADD     1             MSGULT
102200020415     C                   WRITE     SIT3DMS                                      SFL MESS
102300020415     C                   WRITE     SIT3DMC                                      CONTR MESS
102400970526     C*
102500970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
102600970526     C                   MOVEL     *BLANKS       DSMSMS
102700970526     C*
102800970526     C                   ENDSR
102900970526     C*------------------------------------------------------------------------*
103000970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
103100970526     C*------------------------------------------------------------------------*
103200970526     C     FUNCON        BEGSR
103300970526     C*
103400970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
103500970526     C                   EXSR      FUNVAR
103600970526     C*
103700970526     C* PULISCO CAMPI DI LAVORO
103800970526     C                   Z-ADD     0             I                 5 0
103900970526     C                   Z-ADD     0             W                 5 0
104000970526     C*
104100970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
104200970526     C     FUNKEY        IFNE      *BLANKS
104300970526     C     1             DO        24            I
104400970526     C                   MOVEL     FUD(I)        FUDRIG
104500970526     C*
104600970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
104700970526     C     FUNKEY        IFEQ      FUDFUN
104800970526     C     FUDATT        ANDEQ     'S'
104900970526     C                   Z-ADD     I             W
105000970526     C                   END
105100970526     C                   END                                                    DO WHILE
105200970526     C*
105300970526     C* FUNZIONE NON ATTIVA (ERRORE)
105400970526     C     W             IFEQ      0
105500970526     C*
105600970526     C* EMISSIONE MESSAGGIO DI ERRORE
105700970526     C                   MOVEL     MSG(2)        DSMSMS
105800970526     C                   END                                                    W=0
105900970526     C                   END                                                    FUNKEY NE *BLANKS
106000970526     C*
106100970526     C* FUNZIONE ATTIVA (PROCEDI ...)
106200970526     C     W             IFNE      0
106300970526     C     FUNKEY        OREQ      *BLANKS
106400970526     C*
106500970526     C     FUNKEY        IFEQ      *BLANKS
106600970526     C                   EXSR      ENTGES
106700970526     C                   END
106800970526     C     FUNKEY        IFEQ      '03'
106900970526     C                   EXSR      F03GES
107000970526     C                   END
107100970526     C     FUNKEY        IFEQ      '05'
107200970526     C                   EXSR      F05GES
107300970526     C                   END
107400970526     C     FUNKEY        IFEQ      '06'
107500970526     C                   EXSR      F06GES
107600970526     C                   END
107700970526     C     FUNKEY        IFEQ      '12'
107800970526     C                   EXSR      F12GES
107900970526     C                   END
108000970528     C     FUNKEY        IFEQ      '21'
108100970528     C                   EXSR      F21GES
108200970528     C                   END
108300970526     C     FUNKEY        IFEQ      '23'
108400970526     C                   EXSR      F23GES
108500970526     C                   END
108600970526     C     FUNKEY        IFEQ      '24'
108700970526     C                   EXSR      F24GES
108800970526     C                   END
108900970526     C     FUNKEY        IFEQ      'RU'
109000970526     C                   EXSR      RUPGES
109100970526     C                   END
109200970526     C*
109300970526     C                   END
109400970526     C*
109500970526     C                   ENDSR
109600970526     C*------------------------------------------------------------------------*
109700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
109800970526     C*------------------------------------------------------------------------*
109900970526     C     FUNVAR        BEGSR
110000970526     C*
110100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
110200970526     C     KEY           IFEQ      ENTER
110300970526     C                   MOVEL     *BLANKS       FUNKEY
110400970526     C                   END
110500970526     C     KEY           IFEQ      F1
110600970526     C                   MOVEL     '01'          FUNKEY
110700970526     C                   END
110800970526     C     KEY           IFEQ      F2
110900970526     C                   MOVEL     '02'          FUNKEY
111000970526     C                   END
111100970526     C     KEY           IFEQ      F3
111200970526     C                   MOVEL     '03'          FUNKEY
111300970526     C                   END
111400970526     C     KEY           IFEQ      F4
111500970526     C                   MOVEL     '04'          FUNKEY
111600970526     C                   END
111700970526     C     KEY           IFEQ      F5
111800970526     C                   MOVEL     '05'          FUNKEY
111900970526     C                   END
112000970526     C     KEY           IFEQ      F6
112100970526     C                   MOVEL     '06'          FUNKEY
112200970526     C                   END
112300970526     C     KEY           IFEQ      F7
112400970526     C                   MOVEL     '07'          FUNKEY
112500970526     C                   END
112600970526     C     KEY           IFEQ      F8
112700970526     C                   MOVEL     '08'          FUNKEY
112800970526     C                   END
112900970526     C     KEY           IFEQ      F9
113000970526     C                   MOVEL     '09'          FUNKEY
113100970526     C                   END
113200970526     C     KEY           IFEQ      F10
113300970526     C                   MOVEL     '10'          FUNKEY
113400970526     C                   END
113500970526     C     KEY           IFEQ      F11
113600970526     C                   MOVEL     '11'          FUNKEY
113700970526     C                   END
113800970526     C     KEY           IFEQ      F12
113900970526     C                   MOVEL     '12'          FUNKEY
114000970526     C                   END
114100970526     C     KEY           IFEQ      F13
114200970526     C                   MOVEL     '13'          FUNKEY
114300970526     C                   END
114400970526     C     KEY           IFEQ      F14
114500970526     C                   MOVEL     '14'          FUNKEY
114600970526     C                   END
114700970526     C     KEY           IFEQ      F15
114800970526     C                   MOVEL     '15'          FUNKEY
114900970526     C                   END
115000970526     C     KEY           IFEQ      F16
115100970526     C                   MOVEL     '16'          FUNKEY
115200970526     C                   END
115300970526     C     KEY           IFEQ      F17
115400970526     C                   MOVEL     '17'          FUNKEY
115500970526     C                   END
115600970526     C     KEY           IFEQ      F18
115700970526     C                   MOVEL     '18'          FUNKEY
115800970526     C                   END
115900970526     C     KEY           IFEQ      F19
116000970526     C                   MOVEL     '19'          FUNKEY
116100970526     C                   END
116200970526     C     KEY           IFEQ      F20
116300970526     C                   MOVEL     '20'          FUNKEY
116400970526     C                   END
116500970526     C     KEY           IFEQ      F21
116600970526     C                   MOVEL     '21'          FUNKEY
116700970526     C                   END
116800970526     C     KEY           IFEQ      F22
116900970526     C                   MOVEL     '22'          FUNKEY
117000970526     C                   END
117100970526     C     KEY           IFEQ      F23
117200970526     C                   MOVEL     '23'          FUNKEY
117300970526     C                   END
117400970526     C     KEY           IFEQ      F24
117500970526     C                   MOVEL     '24'          FUNKEY
117600970526     C                   END
117700970526     C     KEY           IFEQ      ROLLUP
117800970526     C                   MOVEL     'RU'          FUNKEY
117900970526     C                   END
118000970526     C     KEY           IFEQ      HELP
118100970526     C                   MOVEL     'HP'          FUNKEY
118200970526     C                   END
118300970526     C*
118400970526     C                   ENDSR
118500970526     C*------------------------------------------------------------------------*
118600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
118700970526     C*------------------------------------------------------------------------*
118800970526     C     FUNGES        BEGSR
118900970526     C*
119000970526     C* PULISCO CAMPI DI LAVORO
119100970526     C                   Z-ADD     0             W                 5 0
119200970526     C*
119300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
119400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
119500970526     C     1             DO        24            I
119600970526     C                   MOVEL     FUD(I)        FUDRIG
119700970526     C*
119800970526     C* PULIZIA FUNZIONI PRECEDENTI
119900970526     C                   MOVEL     *BLANKS       FUV(I)
120000970526     C*
120100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
120200970526     C* (SALTA F23 E F24)
120300970526     C     FUDATT        IFEQ      'S'
120400970526     C     FUDFUN        ANDNE     '23'
120500970526     C     FUDFUN        ANDNE     '24'
120600970526     C                   ADD       1             W
120700970526     C                   MOVEL     FUDDES        FUV(W)
120800970526     C                   END
120900970526     C                   END                                                    DO WHILE
121000970526     C*
121100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
121200970526     C* ALTRIMENTI DISATTIVO F24
121300970526     C     1             DO        24            I
121400970526     C                   MOVEL     FUD(I)        FUDRIG
121500970526     C*
121600970526     C     FUDFUN        IFEQ      '24'
121700970526     C     W             IFGT      6
121800970526     C                   MOVEL     'S'           FUDATT
121900970526     C                   ELSE
122000970526     C                   MOVEL     *BLANKS       FUDATT
122100970526     C                   END                                                    W GT 8
122200970526     C                   MOVEL     FUDRIG        FUD(I)
122300970526     C                   END
122400970526     C                   END                                                    DO WHILE
122500970526     C*
122600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
122700970526     C     W             DIV       6             I
122800970526     C                   MVR                     Z
122900970526     C*
123000970526     C     Z             IFNE      0
123100970526     C     I             ADD       1             Z
123200970526     C                   ELSE
123300970526     C                   Z-ADD     I             Z
123400970526     C                   END
123500970526     C*
123600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
123700970526     C                   ADD       1             FUNGRU
123800970526     C     FUNGRU        IFGT      Z
123900970526     C     FUNGRU        ORLT      1
124000970526     C                   Z-ADD     1             FUNGRU
124100970526     C                   END
124200970526     C*
124300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
124400970526     C     FUNGRU        IFEQ      1
124500970526     C                   MOVEL     FUV(1)        FUN001
124600970526     C                   MOVEL     FUV(2)        FUN002
124700970526     C                   MOVEL     FUV(3)        FUN003
124800970526     C                   MOVEL     FUV(4)        FUN004
124900970526     C                   MOVEL     FUV(5)        FUN005
125000970526     C                   MOVEL     FUV(6)        FUN006
125100970526     C                   END
125200970526     C*
125300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
125400970526     C     FUNGRU        IFEQ      2
125500970526     C                   MOVEL     FUV(7)        FUN001
125600970526     C                   MOVEL     FUV(8)        FUN002
125700970526     C                   MOVEL     FUV(9)        FUN003
125800970526     C                   MOVEL     FUV(10)       FUN004
125900970526     C                   MOVEL     FUV(11)       FUN005
126000970526     C                   MOVEL     FUV(12)       FUN006
126100970526     C                   END
126200970526     C*
126300970526     C* TERZO GRUPPO DI 6 FUNZIONI
126400970526     C     FUNGRU        IFEQ      3
126500970526     C                   MOVEL     FUV(13)       FUN001
126600970526     C                   MOVEL     FUV(14)       FUN002
126700970526     C                   MOVEL     FUV(15)       FUN003
126800970526     C                   MOVEL     FUV(16)       FUN004
126900970526     C                   MOVEL     FUV(17)       FUN005
127000970526     C                   MOVEL     FUV(18)       FUN006
127100970526     C                   END
127200970526     C*
127300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
127400970526     C     FUNGRU        IFEQ      4
127500970526     C                   MOVEL     FUV(19)       FUN001
127600970526     C                   MOVEL     FUV(20)       FUN002
127700970526     C                   MOVEL     FUV(21)       FUN003
127800970526     C                   MOVEL     FUV(22)       FUN004
127900970526     C                   MOVEL     *BLANKS       FUN005
128000970526     C                   MOVEL     *BLANKS       FUN006
128100970526     C                   END
128200970526     C*
128300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
128400970526     C                   MOVEL     *BLANKS       FUN007
128500970526     C                   MOVEL     *BLANKS       FUN008
128600970526     C*
128700970526     C     1             DO        24            I
128800970526     C                   MOVEL     FUD(I)        FUDRIG
128900970526     C     FUDFUN        IFEQ      '23'
129000970526     C     FUDATT        ANDEQ     'S'
129100970526     C                   MOVEL     FUDDES        FUN007
129200970526     C                   END
129300970526     C     FUDFUN        IFEQ      '24'
129400970526     C     FUDATT        ANDEQ     'S'
129500970526     C                   MOVEL     FUDDES        FUN008
129600970526     C                   END
129700970526     C                   END                                                    DO
129800970526     C*
129900970526     C                   ENDSR
130000970526     C*------------------------------------------------------------------------*
130100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
130200970526     C*------------------------------------------------------------------------*
130300970526     C     OPZGES        BEGSR
130400970526     C*
130500970526     C* PULISCO CAMPI DI LAVORO
130600970526     C                   Z-ADD     0             W                 5 0
130700970526     C*
130800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
130900970526     C* E CARICO TABELLA OPZIONI ATTIVE
131000991115     C     1             DO        24            I
131100970526     C                   MOVEL     OPD(I)        OPDRIG
131200970526     C*
131300970526     C* PULIZIA OPZIONI PRECEDENTI
131400970526     C                   MOVEL     *BLANKS       OPV(I)
131500970526     C*
131600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
131700970526     C     OPDATT        IFEQ      'S'
131800970526     C                   ADD       1             W
131900970526     C                   MOVEL     OPDDES        OPV(W)
132000970526     C                   END
132100970526     C                   END                                                    DO WHILE
132200970526     C*
132300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
132400970526     C* ALTRIMENTI DISATTIVO F23
132500991115     C     1             DO        24            I
132600970526     C                   MOVEL     FUD(I)        FUDRIG
132700970526     C*
132800970526     C     FUDFUN        IFEQ      '23'
132900970526     C     W             IFGT      8
133000970526     C                   MOVEL     'S'           FUDATT
133100970526     C                   ELSE
133200970526     C                   MOVEL     *BLANKS       FUDATT
133300970526     C                   END                                                    W GT 8
133400970526     C                   MOVEL     FUDRIG        FUD(I)
133500970526     C                   END
133600970526     C                   END                                                    DO WHILE
133700970526     C*
133800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
133900970526     C     W             DIV       8             I
134000970526     C                   MVR                     Z                 5 0
134100970526     C*
134200970526     C     Z             IFNE      0
134300970526     C     I             ADD       1             Z
134400970526     C                   ELSE
134500970526     C                   Z-ADD     I             Z
134600970526     C                   END
134700970526     C*
134800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
134900970526     C                   ADD       1             OPZGRU
135000970526     C     OPZGRU        IFGT      Z
135100970526     C     OPZGRU        ORLT      1
135200970526     C                   Z-ADD     1             OPZGRU
135300970526     C                   END
135400970526     C*
135500970526     C* PRIMO GRUPPO DI 8 OPZIONI
135600970526     C     OPZGRU        IFEQ      1
135700970526     C                   MOVEL     OPV(1)        OPZ001
135800970526     C                   MOVEL     OPV(2)        OPZ002
135900970526     C                   MOVEL     OPV(3)        OPZ003
136000970526     C                   MOVEL     OPV(4)        OPZ004
136100970526     C                   MOVEL     OPV(5)        OPZ005
136200970526     C                   MOVEL     OPV(6)        OPZ006
136300970526     C                   MOVEL     OPV(7)        OPZ007
136400970526     C                   MOVEL     OPV(8)        OPZ008
136500970526     C                   END
136600970526     C*
136700970526     C* SECONDO GRUPPO DI 8 OPZIONI
136800970526     C     OPZGRU        IFEQ      2
136900970526     C                   MOVEL     OPV(9)        OPZ001
137000970526     C                   MOVEL     OPV(10)       OPZ002
137100970526     C                   MOVEL     OPV(11)       OPZ003
137200970526     C                   MOVEL     OPV(12)       OPZ004
137300970526     C                   MOVEL     OPV(13)       OPZ005
137400970526     C                   MOVEL     OPV(14)       OPZ006
137500970526     C                   MOVEL     OPV(15)       OPZ007
137600970526     C                   MOVEL     OPV(16)       OPZ008
137700970526     C                   END
137800970526     C*
137900970526     C* TERZO GRUPPO DI 8 OPZIONI
138000970526     C     OPZGRU        IFEQ      3
138100970526     C                   MOVEL     OPV(17)       OPZ001
138200970526     C                   MOVEL     OPV(18)       OPZ002
138300970526     C                   MOVEL     OPV(19)       OPZ003
138400970526     C                   MOVEL     OPV(20)       OPZ004
138500970526     C                   MOVEL     OPV(21)       OPZ005
138600970526     C                   MOVEL     OPV(22)       OPZ006
138700970526     C                   MOVEL     OPV(23)       OPZ007
138800970526     C                   MOVEL     OPV(24)       OPZ008
138900970526     C                   END
139000970526     C*
139100970526     C                   ENDSR
139200970526     C*------------------------------------------------------------------------*
139300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
139400970526     C*------------------------------------------------------------------------*
139500970526     C     OPZCON        BEGSR
139600970526     C*
139700970526     C* PULISCO CAMPI DI LAVORO
139800970526     C                   Z-ADD     0             I                 5 0
139900970526     C                   Z-ADD     0             W                 5 0
140000970526     C*
140100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
140200970526     C     1             DO        24            I
140300970526     C                   MOVEL     OPD(I)        OPDRIG
140400970526     C*
140500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
140600970526     C     OPZKEY        IFEQ      OPDOPZ
140700970526     C     OPZKEY        OREQ      OPDOP1
140800970526     C*
140900970526     C     OPDATT        IFEQ      'S'
141000970526     C                   MOVEL     OPDOPZ        OPZKEY
141100970526     C                   Z-ADD     I             W
141200970526     C                   END
141300970526     C                   END
141400970526     C*
141500970526     C                   END                                                    DO WHILE
141600970526     C*
141700970526     C* OPZIONE NON ATTIVA (ERRORE)
141800970526     C     W             IFEQ      0
141900970526     C*
142000970526     C* OPZIONE NON OK
142100970526     C                   Z-ADD     1             OPZOK
142200970526     C*
142300970526     C* EMISSIONE MESSAGGIO DI ERRORE
142400970526     C                   MOVEL     MSG(1)        DSMSMS
142500970526     C                   ELSE
142600970526     C*
142700970526     C* OPZIONE ATTIVA (PROCEDI ...)
142800970526     C*
142900000420     C     OPZKEY        IFEQ      ' 1'
143000000420     C                   EXSR      O01GES
143100000420     C                   END
143200000407     C     OPZKEY        IFEQ      ' 2'
143300000407     C                   EXSR      O02GES
143400000407     C                   END
143500020415     C     OPZKEY        IFEQ      ' 3'
143600020415     C                   EXSR      O03GES
143700020415     C                   END
143800991110     C     OPZKEY        IFEQ      ' 4'
143900991110     C                   EXSR      O04GES
144000991014     C                   END
144100970526     C     OPZKEY        IFEQ      ' 5'
144200970526     C                   EXSR      O05GES
144300970526     C                   END
144400000411     C     OPZKEY        IFEQ      ' 7'
144500000411     C                   EXSR      O07GES
144600000411     C                   END
144700970526     C                   END                                                    OPZ.NON ATTIVA
144800970526     C*
144900970526     C                   ENDSR
145000970526     C*------------------------------------------------------------------------*
145100970526     C* NOTE COSTRUZIONE SCHIERE
145200970526     C*------------------------------------------------------------------------*
145300970526     C*
145400970526     C* -MSG-MESSAGGI
145500970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
145600970526     C* (NON TOCCARE MSG,1 E MSG,2)
145700970526     C*
145800970526     C* -CMD-COMANDI
145900970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
146000970526     C*
146100970526     C* -OPD-OPZIONI
146200970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
146300970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
146400970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
146500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
146600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
146700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
146800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
146900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
147000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
147100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
147200970526     C*
147300970526     C* -FUD-FUNZIONI
147400970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
147500970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
147600970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
147700970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
147800970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
147900970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
148000970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
148100970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
148200970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
148300970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
148400991108     C*
148500991115     C* -STS-CODICI STATI DEI RECORD DI LOG
148600991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
148700991110     C* ================> IMPORTANTE <=================
148800991115     C* per codici di errore in -STS- indicare in -DES- sempre
148900991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
149000991110     C* ERR
149100991108     C*
149200970526     O*------------------------------------------------------------------------*
149300970526** DEC - DECODIFICHE
149400040603     SCELTA                                                      1
149500040603    GESTIONE                                                     2
149600040603                                                                 3
149700040603                                                                 4
149800040603 INTERROGAZIONE                                                  5
149900970526** MSG - MESSAGGI
150000020415TISIT3R- Opzione non gestita.                                                  1
150100020415TISIT3R- Tasto funzionale non gestito.                                         2
150200020415TISIT3R- Non abilitato all inserimento in filiale.                             3
150300020415TISIT3R- Record già annullato, consentito solo visualizz. e ripristino         4
150400020415TISIT3R- Record non annullato, impossibile ripristinare                        5
150500020415TISIT3R- Accettato solo un parametro di inizio lista                           6
150600020415TISIT3R- 7                                                                     7
150700020415TISIT3R- 8                                                                     8
150800020415TISIT3R- 9                                                                     9
150900020415TISIT3R- 10                                                                   10
151000020415TISIT3R- 11                                                                   11
151100020415TISIT3R- 12                                                                   12
151200020415TISIT3R- 13                                                                   13
151300020415TISIT3R- 14                                                                   14
151400020415TISIT3R- 15                                                                   15
151500020415TISIT3R- 16                                                                   16
151600020415TISIT3R- 17                                                                   17
151700020415TISIT3R- 18                                                                   18
151800020415TISIT3R- 19                                                                   19
151900020415TISIT3R- 20                                                                   20
152000970528** CMD - COMANDI CL
152100991110DSPPFM FILE(
152200991110) MBR(
152300991110
152400970528
152500970528
152600970526** OPD - OPZIONI
152700000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
152800000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
152900020412 3=Copia           -3 -     (S/N)=N                        opz. 03¬
153000000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
153100000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
153200991115                   -  -     (S/N)=N                        opz. 06¬
153300000411 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
153400991110                   -  -     (S/N)=N                        opz. 08¬
153500991110                   -  -     (S/N)=N                        opz. 09¬
153600991110                   -  -     (S/N)=N                        opz. 10¬
153700000407                   -  -     (S/N)=N                        opz. 11¬
153800970526                   -  -     (S/N)=N                        opz. 12¬
153900020415                   -  -     (S/N)=N                        opz. 13¬
154000000407                   -  -     (S/N)=N                        opz. 14¬
154100000407                   -  -     (S/N)=N                        opz. 15¬
154200970526                   -  -     (S/N)=N                        opz. 16¬
154300970526                   -  -     (S/N)=N                        opz. 17¬
154400970526                   -  -     (S/N)=N                        opz. 18¬
154500970526                   -  -     (S/N)=N                        opz. 19¬
154600970526                   -  -     (S/N)=N                        opz. 20¬
154700970526                   -  -     (S/N)=N                        opz. 21¬
154800970526                   -  -     (S/N)=N                        opz. 22¬
154900970526                   -  -     (S/N)=N                        opz. 23¬
155000000407                   -  -     (S/N)=N                        opz. 25¬
155100970526** FUD - FUNZIONI
155200970526                   -        (S/N)=N                        funz.01¬
155300970526                   -        (S/N)=N                        funz.02¬
155400970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
155500970526                   -        (S/N)=N                        funz.04¬
155600970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
155700000407F06=Inserimento    -        (S/N)=N                        funz.06¬
155800970526                   -        (S/N)=N                        funz.07¬
155900970527                   -        (S/N)=N                        funz.08¬
156000970527                   -        (S/N)=N                        funz.09¬
156100970527                   -        (S/N)=N                        funz.10¬
156200970526                   -        (S/N)=N                        funz.11¬
156300970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
156400991116                   -        (S/N)=N                        funz.13¬
156500970526                   -        (S/N)=N                        funz.14¬
156600970526                   -        (S/N)=N                        funz.15¬
156700970526                   -        (S/N)=N                        funz.16¬
156800970526                   -        (S/N)=N                        funz.17¬
156900970526                   -        (S/N)=N                        funz.18¬
157000970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
157100970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
157200000407                   -        (S/N)=N                        funz.21¬
157300991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
157400991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
157500991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
