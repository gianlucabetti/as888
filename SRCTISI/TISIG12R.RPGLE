000100970526     H*------------------------------------------------------------------------*
000200150203     H* Gestione file anagrafico e Bancali
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150210     FTISIG12d  CF   E             WORKSTN INFDS(DEVDS1)
000900021211     F                                     SFILE(SIG1D05:SFLULT)
001000021211     F                                     SFILE(SIG1DMS:MSGULT)
001100970526     F*------------
001200150210     Fcnaco00f  IF   E           K DISK
001300150210     Fazfgb01l  IF   E           K DISK
001400970526     D*------------------------------------------------------------------------*
001500970526     D* SCHIERE
001600970526     D*------------------------------------------------------------------------*
001700970526     D*------------
001800970526     D* DECODIFICHE
001900970526     D*------------
002000970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002100970526     D*------------
002200970526     D* MESSAGGI
002300970526     D*------------
002400970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002500970528     D*------------
002600970528     D* COMANDI
002700970528     D*------------
002800970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002900970526     D*------------
003000150226
003100970526     D*------------
003200970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003300970526     D*------------
003400970526     D* DESCRIZIONI TASTI FUNZIONALI
003500970526     D*------------
003600970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
003700970526     D*------------
003800970526     D* OPZIONI DA CARICARE A VIDEO
003900970526     D*------------
004000970526     D OPV             S             19    DIM(24)
004100970526     D*------------
004200970526     D* FUNZIONI DA CARICARE A VIDEO
004300970526     D*------------
004400970527     D FUV             S             19    DIM(24)
004500991108     D*------------
004600970526     D*------------------------------------------------------------------------*
004700970526     D* INPUT
004800970526     D*------------------------------------------------------------------------*
004900970526     D*------------
005000970526     D* RIDEFINIZIONE OPZIONI VIDEO
005100970526     D*------------
005200970526     D OPDDS           DS
005300970526     D  OPDOPZ                 1      2
005400970526     D  OPDDE1                 3     19
005500970526     D  OPDOP1                21     22
005600970526     D  OPDFI0                23     34
005700970526     D  OPDATT                35     35
005800970526     D  OPDFI1                36     80
005900970526     D  OPDDES                 1     19
006000970526     D  OPDRIG                 1     80
006100970526     D*------------
006200970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006300970526     D*------------
006400970526     D FUDDS           DS
006500970526     D  FUDDE1                 1      1
006600970526     D  FUDFUN                 2      3
006700970526     D  FUDDE2                 4     19
006800970526     D  FUDFI0                20     34
006900970526     D  FUDATT                35     35
007000970526     D  FUDFI1                36     80
007100970526     D  FUDDES                 1     19
007200970526     D  FUDRIG                 1     80
007300970526     D*------------
007400970526     D* DS GESTIONE TASTI FUNZIONALI
007500970526     D*------------
007600970526     D DSKEY         E DS
007700970526     D DEVDS1          DS
007800970526     D  KEY                  369    369
007900970526     D*------------
008000970526     D* DS "XMSG" - GESTIONE MESSAGGI
008100970526     D*------------
008200970526     D DSMSG         E DS
008300970526     D*------------
008400020917     D* DS REPERIMENTO DATI UTENTE
008500970526     D*------------
008600020917     D TIBS34DS      E DS                                                       *Profili utente
008700020917     D DDATIUTE      E DS                                                       *Dati utente
008800020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008900970527     D*------------------
009000991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
009100970527     D*------------------
009200970527     D WLBDA8          DS                  INZ
009300970527     D  G08DAT                 1      8  0
009400970527     D  G08INV                 9     16  0
009500970527     D  G08ERR                17     17
009600970527     D  G08TGI                18     22  0
009700970526     D*------------
009800991110     D* DS DI PROCEDURA
009900970526     D*------------
010000970526     D* ARCHITETTURA
010100970526     D*------------
010200970526     D KPJBA         E DS
010300150226     D savkpjbu        s                   like(kpjbu)
010400150202      *
010500150202     d Tibs69DS      E DS                  INZ
010600150202     d DS_cnaco      E DS                  extname(CNACO00F)
010700150202     d DS_cnind      E DS                  extname(CNIND00F)
010800150202     d DS_cnclp      E DS                  extname(CNCLP00F)
010900150202     d DS_fncls      E DS                  extname(FNCLS00F)
011000150203      *
011100021211     D*------------------
011200021211     D* PASSAGGIO PARAMETRI
011300021211     D*------------------
011400021211     D PARAM           DS                  INZ
011500150210     D  Pop1                          3
011600150130     D  Pksu                          8
011700150210     D  Pksc                          8
011800150130     D  Pmsg                         80
011900970526     C*------------------------------------------------------------------------*
012000970526     C* MAIN LINES
012100970526     C*------------------------------------------------------------------------*
012200970526     C*
012300970526     C* SE NON CI SONO ERRORI, PROSEGUE
012400970526IF  1C     ERRL00        IFNE      '1'
012500970526     C*
012600970526     C* CARICA IL SFILE
012700970526     C                   EXSR      POSSFL
012800970526     C*
012900970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013000970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013100970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013200970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013300970526     C*
013400970526     C* REIMPOSTA ERRORE DI PROCERURA
013500970526     C                   MOVEL     '0'           ERRL00
013600970526     C*
013700150210      * cerco i dati anagrafici
013800150210     c                   clear                   tibs69ds
013900150211     c                   move      pksu          vidksu
014000150211     c                   move      vidksu        i69kac
014100150210     c                   call      'TIBS69R'
014200150210     c                   parm                    tibs69ds
014300150210     c                   parm                    ds_cnaco
014400150210     c                   parm                    ds_cnind
014500150210     c                   parm                    ds_cnclp
014600150210     c                   parm                    ds_fncls
014700150210      * ragione sociale
014800150211     c                   eval      vidksud = acorag
014900970526     C* EMETTE IL SFILE
015000970526     C                   EXSR      OUTSFL
015100970526     C*
015200970526     C* GESTIONE TASTI FUNZIONALI
015300970526     C                   EXSR      FUNCON
015400970526     C*
015500970526E   2C                   ENDDO
015600970527     C*
015700970526E   1C                   ENDIF
015800991108     C*
015900970526     C* FINE PGM
016000970526     C                   SETON                                        LR
016100970526     C*------------------------------------------------------------------------*
016200970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016300970526     C*------------------------------------------------------------------------*
016400970526     C     F03GES        BEGSR
016500970526     C*
016600970526     C* IMPOSTA "PREMUTO F03"
016700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
016800970526     C*
016900970526     C                   ENDSR
017000970526     C*------------------------------------------------------------------------*
017100970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017200970526     C*------------------------------------------------------------------------*
017300970526     C     F05GES        BEGSR
017400970526     C*
017500970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
017600970526     C*
017700970526     C                   ENDSR
017800970526     C*------------------------------------------------------------------------*
017900150204     C* F06GES - GESTIONE F06 -> INSERIMENTO
018000970526     C*------------------------------------------------------------------------*
018100150204     C     F06GES        BEGSR
018200970526     C*
018300021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
018400150213     C*    DUTLPO        IFNE      'S'
018500150213     C*                  MOVEL     '1'           ERRL00
018600150213     C*                  MOVEL     MSG(3)        DSMSMS
018700150213     C*                  ENDIF
018800000407     C*
018900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019000150226     c                   movel     kpjbu         savkpjbu
019100150204     C                   MOVEL     'F06'         POP1
019200000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
019300150210     C                   CALL      'TISIG13R'
019400000407     C                   PARM                    KPJBA
019500021211     C                   MOVEL     KPJBU         PARAM
019600150226     c                   movel     savkpjbu      kpjbu
019700000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
019800021211     C                   ENDIF
019900000407     C*
020000970526     C                   ENDSR
020100970526     C*------------------------------------------------------------------------*
020200991108     C* F12GES - GESTIONE F12 -> RITORNO
020300970526     C*------------------------------------------------------------------------*
020400970526     C     F12GES        BEGSR
020500970526     C*
020600970526     C* IMPOSTA "PREMUTO F12"
020700970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
020800970526     C*
020900970526     C                   ENDSR
021000970528     C*------------------------------------------------------------------------*
021100970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021200970528     C*------------------------------------------------------------------------*
021300970528     C     F21GES        BEGSR
021400970612     C*
021500991117     C* NON GESTITA
021600970528     C*
021700970528     C                   ENDSR
021800970528     C*------------------------------------------------------------------------*
021900970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
022000970528     C*------------------------------------------------------------------------*
022100970528     C     F22GES        BEGSR
022200970528     C*
022300021211     C* NON GESTITA
022400970528     C*
022500970528     C                   ENDSR
022600970526     C*------------------------------------------------------------------------*
022700970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
022800970526     C*------------------------------------------------------------------------*
022900970526     C     F23GES        BEGSR
023000970526     C*
023100970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023200970526     C*
023300970526     C                   ENDSR
023400970526     C*------------------------------------------------------------------------*
023500970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
023600970526     C*------------------------------------------------------------------------*
023700970526     C     F24GES        BEGSR
023800970526     C*
023900970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024000970526     C*
024100970526     C                   ENDSR
024200970526     C*------------------------------------------------------------------------*
024300970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024400970526     C*------------------------------------------------------------------------*
024500970526     C     RUPGES        BEGSR
024600970526     C*
024700970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
024800970526     C*
024900970526     C                   ENDSR
025000970526     C*------------------------------------------------------------------------*
025100991108     C* ENTGES - GESTIONE ENTER -> INVIO
025200970526     C*------------------------------------------------------------------------*
025300970526     C     ENTGES        BEGSR
025400000407     C*
025500000407     C                   MOVEL     '0'           ERRL00
025600991108     C*
025700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
025800970526     C                   EXSR      NEWPOS
025900021211     C*
026000021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026100021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026200021211     C     SWPOSI        IFNE      0
026300021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
026400021211     C                   ELSE
026500021211     C     SFLULT        IFNE      0
026600021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
026700021211     C*
026800021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026900021211     C     SWMODI        IFNE      0
027000021211     C                   EXSR      POSSFL
027100021211     C                   ENDIF
027200021211     C                   ENDIF
027300021211     C                   ENDIF
027400021211     C                   ENDIF
027500021211     C*
027600021211     C                   ENDSR
027700000420     C*------------------------------------------------------------------------*
027800000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027900000420     C*------------------------------------------------------------------------*
028000000420     C     O01GES        BEGSR
028100000420     C*
028200000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028300150130     C                   MOVEL     VIDksu        Pksu
028400000420     C                   MOVEL     PARAM         KPJBU
028500000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
028600000420     C*
028700000420     C                   ENDSR
028800000407     C*------------------------------------------------------------------------*
028900000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
029000000407     C*------------------------------------------------------------------------*
029100000407     C     O02GES        BEGSR
029200000407     C*
029300021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
029400150213     C*    DUTLPO        IFNE      'S'
029500150213     C*                  MOVEL     '1'           ERRL00
029600150213     C*                  MOVEL     MSG(3)        pmsg
029700150213     C*                  ENDIF
029800021211     C*
029900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030000150226     c                   movel     kpjbu         savkpjbu
030100150130     C                   MOVEL     VIDksu        Pksu
030200150210     C                   MOVEL     VIDksc        Pksc
030300000407     C                   MOVEL     'O02'         POP1
030400000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
030500150210     C                   CALL      'TISIG13R'
030600000407     C                   PARM                    KPJBA
030700021211     C                   MOVEL     KPJBU         PARAM
030800150226     c                   movel     savkpjbu      kpjbu
030900021211     C                   ENDIF
031000000407     C*
031100000407     C                   ENDSR
031200021211     C*------------------------------------------------------------------------*
031300021211     C* O03GES - GESTIONE OPZIONE 3= COPIA
031400021211     C*------------------------------------------------------------------------*
031500021211     C     O03GES        BEGSR
031600021211     C*
031700021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
031800150213     C*    DUTLPO        IFNE      'S'
031900150213     C*                  MOVEL     '1'           ERRL00
032000150213     C*                  MOVEL     MSG(3)        PMSG
032100150213     C*                  ENDIF
032200021211     C*
032300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032400150226     c                   movel     kpjbu         savkpjbu
032500150130     C                   MOVEL     VIDksu        Pksu
032600150210     C                   MOVEL     VIDksc        Pksc
032700021211     C                   MOVEL     'O03'         POP1
032800021211     C                   EXSR      PASPAR                                       *RICARICA SFILE
032900150210     C                   CALL      'TISIG13R'
033000021211     C                   PARM                    KPJBA
033100021211     C                   MOVEL     KPJBU         PARAM
033200150226     c                   movel     savkpjbu      kpjbu
033300021211     C                   ENDIF
033400021211     C*
033500021211     C                   ENDSR
033600991112     C*------------------------------------------------------------------------*
033700021211     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
033800991112     C*------------------------------------------------------------------------*
033900991112     C     O04GES        BEGSR
034000021211     C*
034100021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
034200150213     C*    DUTLPO        IFNE      'S'
034300150213     C*                  MOVEL     '1'           ERRL00
034400150213     C*                  MOVEL     MSG(3)        PMSG
034500150213     C*                  ENDIF
034600991112     C*
034700021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
034800021211     C*
034900150226     c                   movel     kpjbu         savkpjbu
035000150130     C                   MOVEL     VIDksu        Pksu
035100150210     C                   MOVEL     VIDksc        Pksc
035200000407     C                   MOVEL     'O04'         POP1
035300000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
035400150210     C                   CALL      'TISIG13R'
035500000407     C                   PARM                    KPJBA
035600021211     C                   MOVEL     KPJBU         PARAM
035700150226     c                   movel     savkpjbu      kpjbu
035800000411     C                   ENDIF
035900991112     C*
036000991112     C                   ENDSR
036100991014     C*------------------------------------------------------------------------*
036200000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
036300970526     C*------------------------------------------------------------------------*
036400970526     C     O05GES        BEGSR
036500000407     C*
036600021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
036700150226     c                   movel     kpjbu         savkpjbu
036800150130     C                   MOVEL     VIDksu        Pksu
036900150210     C                   MOVEL     VIDksc        Pksc
037000000407     C                   MOVEL     'O05'         POP1
037100000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
037200150210     C                   CALL      'TISIG13R'
037300000407     C                   PARM                    KPJBA
037400150226     c                   movel     savkpjbu      kpjbu
037500021211     C                   ENDIF
037600000407     C*
037700970526     C                   ENDSR
037800991028     C*------------------------------------------------------------------------*
037900991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
038000991112     C*------------------------------------------------------------------------*
038100991112     C     PASPAR        BEGSR
038200991112     C*
038300000407     C                   MOVEL     PARAM         KPJBU
038400991112     C*
038500991112     C                   ENDSR
038600970526     C*------------------------------------------------------------------------*
038700970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
038800970526     C*------------------------------------------------------------------------*
038900970526     C     NEWPOS        BEGSR
039000970526     C*
039100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
039200970526     C*         1= RICHIESTO POSIZIONAMENTO
039300970526     C                   Z-ADD     0             SWPOSI            7 0
039400970526     C*
039500991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
039600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
039700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
039800970526     C                   Z-ADD     0             SWMODI            1 0
039900970530     C*
040000970530     C* REIMPOSTA INDICATORI DI ERRORE
040100991108     C                   SETOFF                                       808182
040200991108     C                   SETOFF                                       83
040300970526     C*
040400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
040500150210     C*    VIDCO1        IFNE      depco1
040600150210     C*    VIDTs2        ORNE      depts2
040700150210     C*    VIDco2        ORNE      depco2
040800150210     C*                  EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
040900150210     C*    ERRL00        IFEQ      '0'                                          *NO ERORI
041000150210     C*                  Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
041100150210     C*                  MOVEL     VIDCO1        depco1                         *DEPOSITI
041200150210     C*                  MOVEL     VIDTs2        depts2                         *DEPOSITI
041300150210     C*                  MOVEL     VIDco2        depco2                         *DEPOSITI
041400150210     C*                  ENDIF
041500150210     C*                  ENDIF
041600970526     C*
041700970526     C                   ENDSR
041800970526     C*------------------------------------------------------------------------*
041900021211     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
042000970526     C*------------------------------------------------------------------------*
042100970528     C     SETFIL        BEGSR
042200970526     C*
042300970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
042400150210     C*    VIDCO1        IFEQ      *blanks
042500150210     C*                  MOVEL     *loval        KEYksu            8
042600150210     C*                  ELSE
042700150210     C*                  MOVEL     vIDCO1        KEYksu            8
042800150210     C*                  ENDIF
042900150210     C*    WTIPRI        IFEQ      'COD'
043000150210     C     pksu          SETLL     azfgb01l                           99
043100150210     C*                  ENDIF
043200980107     C*
043300021211     C* LETTURA RECORD
043400021211     C     *IN99         IFEQ      *OFF
043500970526     C                   EXSR      LETFIL
043600021211     C                   ENDIF
043700970526     C*
043800970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043900021211     C     *IN99         IFEQ      *ON
044000970526     C                   Z-ADD     1             SFLEOF
044100021211     C                   ENDIF
044200970526     C*
044300970526     C                   ENDSR
044400970526     C*------------------------------------------------------------------------*
044500970526     C* LETFIL - LETTURA RECORD
044600970526     C*------------------------------------------------------------------------*
044700970526     C     LETFIL        BEGSR
044800970526     C*
044900970526     C* LEGGE FINO A:
045000970526     C*     - FINE FILE
045100970526     C*     - TROVATO RECORD VALIDO
045200970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
045300021211     C     *IN99         DOWEQ     *OFF
045400970526     C     WRECOK        ANDEQ     'N'
045500970526     C*
045600970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
045700150210     C*    WTIPRI        IFEQ      'COD'                                        *PER COD. ARTICOLO
045800150210     C     pksu          READe     azfgb01l                               99
045900150210     C*                  ENDIF
046000970528     C*
046100970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
046200021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
046300970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
046400021211     C                   ELSE
046500970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
046600021211     C                   ENDIF
046700021211     C                   ENDDO
046800970526     C*
046900970526     C                   ENDSR
047000970526     C*------------------------------------------------------------------------*
047100970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
047200970526     C*------------------------------------------------------------------------*
047300970526     C     CHKREC        BEGSR
047400970526     C*
047500970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
047600021211     C* FILTRA SE RICHIESTO UN TIPO ARTICOLO
047700150210     C*    VIDTs2        ifeq      *blanks
047800150210     C*                  else
047900150210     C*    cgbTsc        ifne      VIDTs2
048000150210     C*                  movel     'N'           WRECOK                         *RECORD NON VALIDO
048100150210     C*                  endif
048200150210     C*                  endif
048300150210     C*    VIDco2        ifeq      *blanks
048400150210     C*                  else
048500150210     C*    cgbcod1       ifne      VIDco2
048600150210     C*    cgbcod2       andne     VIDco2
048700150210     C*    cgbcod3       andne     VIDco2
048800150210     C*                  movel     'N'           WRECOK                         *RECORD NON VALIDO
048900150210     C*                  endif
049000150210     C*
049100991108     C*
049200970526     C                   ENDSR
049300970526     C*------------------------------------------------------------------------*
049400000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
049500970526     C*------------------------------------------------------------------------*
049600970526     C     IRESFL        BEGSR
049700970526     C*
049800000407     C                   MOVEL     *blanks       OPZKEY
049900150219     C                   MOVEL     fgbann        VIDann
050000150210     C                   MOVEL     fgbksc        VIDksc
050100150210     C                   clear                   VIDkscd
050200150202      * cerco i dati anagrafici
050300150202     c                   clear                   tibs69ds
050400150210     c                   move      vidksc        i69kac
050500150202     c                   call      'TIBS69R'
050600150202     c                   parm                    tibs69ds
050700150202     c                   parm                    ds_cnaco
050800150202     c                   parm                    ds_cnind
050900150202     c                   parm                    ds_cnclp
051000150202     c                   parm                    ds_fncls
051100150202      * ragione sociale
051200150210     c                   eval      vidkscd = acorag
051300970526     C                   ENDSR
051400970526     C*------------------------------------------------------------------------*
051500991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
051600970526     C*------------------------------------------------------------------------*
051700970526     C     CHKPAR        BEGSR
051800970528     C*
051900021211     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (COD)
052000021211     C                   MOVEL     'COD'         WTIPRI            3
052100000407     C*
052200970526     C                   ENDSR
052300970526     C*------------------------------------------------------------------------*
052400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
052500970526     C*------------------------------------------------------------------------*
052600970526     C     CHKFOP        BEGSR
052700991112     C*
052800991112     C* OPZIONI
052900970626     C*
053000000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
053100021211     C     DEPOP0        IFEQ      'P01'
053200000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
053300000420     C                   MOVEL     'S'           OPDATT
053400000420     C                   MOVEL     OPDRIG        OPD(1)
053500021211     C                   ELSE
053600000420     C* OPZIONI
053700970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
053800970626     C                   MOVEL     'S'           OPDATT
053900970626     C                   MOVEL     OPDRIG        OPD(5)
054000150302     C                   MOVEL     OPD(4)        OPDRIG                         *INTERROGAZIONE
054100150302     C                   MOVEL     'N'           OPDATT
054200150302     C                   MOVEL     OPDRIG        OPD(4)
054300150302     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
054400150302     C                   MOVEL     'N'           FUDATT
054500150302     C                   MOVEL     FUDRIG        FUD(6)
054600021211     C                   ENDIF
054700970626     C*
054800000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
054900021211     C     DEPOP0        IFEQ      'P02'
055000000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
055100150219     C                   MOVEL     'N'           OPDATT
055200000407     C                   MOVEL     OPDRIG        OPD(2)
055300021211     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
055400150219     C                   MOVEL     'N'           OPDATT
055500021211     C                   MOVEL     OPDRIG        OPD(3)
055600000411     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
055700000411     C                   MOVEL     'S'           OPDATT
055800000411     C                   MOVEL     OPDRIG        OPD(4)
055900991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
056000991112     C                   MOVEL     'S'           OPDATT
056100991112     C                   MOVEL     OPDRIG        OPD(5)
056200021211     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
056300000407     C                   MOVEL     'S'           FUDATT
056400000407     C                   MOVEL     FUDRIG        FUD(6)
056500021211     C                   ENDIF
056600970626     C*
056700970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
056800150130     C*m   DUTLPO        IFNE      'S'                                          *FILIALE
056900150130     C*m                 MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
057000150130     C*m                 MOVEL     'N'           OPDATT
057100150130     C*m                 MOVEL     OPDRIG        OPD(2)
057200150130     C*m                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
057300150130     C*m                 MOVEL     'N'           OPDATT
057400150130     C*m                 MOVEL     OPDRIG        OPD(4)
057500150130     C*m                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
057600150130     C*m                 MOVEL     'N'           OPDATT
057700150130     C*m                 MOVEL     FUDRIG        FUD(6)
057800150130     C*m                 ENDIF
057900970526     C*
058000970526     C* GESTIONE OPZIONI A VIDEO
058100970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
058200970526     C                   EXSR      OPZGES
058300970526     C*
058400970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
058500970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
058600970526     C                   EXSR      FUNGES
058700970526     C*
058800970526     C                   ENDSR
058900020917     C*--------------------------------------------------------------------------------------------*
059000020917     C* REPERISCE I DATI UTENTE
059100020917     C*--------------------------------------------------------------------------------------------*
059200020917     C     REPDATIUTE    BEGSR
059300020917     C*
059400020917     C* INIZIALIZZA VARIABILI DI WRK
059500020917     C                   CLEAR                   TIBS34DS
059600020917     C                   CLEAR                   AZUTEDS
059700020917     C                   CLEAR                   DDATIUTE
059800020917     C*
059900020917     C     *DTAARA       DEFINE    §azute        azuteds
060000020917     C     *DTAARA       DEFINE    §datiute      ddatiute
060100020917     C                   IN(E)     *DTAARA
060200021211     C                   IF        %Error
060300021211     C                   EVAL      I34Tla = 'L'
060400020917     C                   CALL      'TIBS34R'
060500020917     C                   PARM                    Tibs34Ds
060600020917     C                   IN        *DTAARA
060700021211     C                   ENDIF
060800020917     C*
060900020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
061000020917     C                   MOVEL(P)  RSUT          DSFIRS           20
061100020917     C*
061200020917     C                   ENDSR
061300970526     C*------------------------------------------------------------------------*
061400970526     C* *INZSR - OPERAZIONI INIZIALI
061500970526     C*------------------------------------------------------------------------*
061600970526     C     *INZSR        BEGSR
061700970526     C*---
061800970526     C* RICEVIMENTO PARAMETRI
061900970526     C*---
062000970526     C     *ENTRY        PLIST
062100970612     C                   PARM                    KPJBA
062200021211     C                   MOVEL     KPJBU         PARAM
062300150202     c                   if        pop1 <> *blank
062400021211     C                   MOVEL     POP1          DEPOP0            3
062500150202     c                   else
062600150202     C                   MOVEL     'P02'         DEPOP0
062700150202     c                   endif
062800970526     C*
062900970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
063000970526     C     XMSGPA        PLIST
063100970526     C                   PARM                    DSMSMS
063200970526     C                   PARM                    DSMSPG
063300970526     C                   PARM                    DSMSKE
063400970526     C                   PARM                    DSMSER
063500970526     C*
063600970526     C* IMPOSTA LA DS -TASTI PREMUTI-
063700970526     C                   CALL      'XKEY'
063800970526     C                   PARM                    DSKEY
063900970526     C*---
064000970526     C* VARIABILI NON RIFERITE AL DATA BASE
064100970526     C*---
064200970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
064300970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
064400970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
064500970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
064600970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
064700970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
064800970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
064900970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
065000970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
065100970526     C*---
065200150210     C*    *LIKE         DEFINE    VIDCO1        depco1
065300150210     C*    *LIKE         DEFINE    VIDts2        depts2
065400150210     C*    *LIKE         DEFINE    VIDco2        depco2
065500991108     C*---
065600970526     C* VARIABILI DI CONTROLLO
065700970526     C*---
065800970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
065900970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
066000970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
066100970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
066200970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
066300000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
066400970526     C*
066500970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
066600150210     C                   MOVEL     'TISIG12R'    DSMSPG
066700150210     C                   MOVEL     'TISIG12R'    MSGQUE
066800970526     C                   MOVEL     *BLANKS       MSGKEY
066900970526     C                   MOVEL     *BLANKS       DSMSMS
067000970526     C                   MOVEL     *BLANKS       DSMSKE
067100970526     C                   Z-ADD     *ZEROS        DSMSER
067200970526     C*---
067300970526     C* CHIAVI DI LETTURA
067400970526     C*---
067500150130     C* LETTURA azcgb01l - COMPLETA
067600991108     C*
067700970527     C* REPERIMENTO DATI UTENTE
067800020917     C                   EXSR      REPDATIUTE
067900970526     C*
068000970526     C* IMPOSTA I PARAMETRI DI INGRESSO
068100970526     C                   EXSR      CHKPAR
068200991115     C*
068300970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
068400970526     C                   EXSR      CHKFOP
068500970526     C*
068600970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
068700021211     C     DEPOP0        IFEQ      'P01'
068800970530     C                   MOVEL     DEC(3)        VIDING
068900021211     C                   ENDIF
069000021211     C     DEPOP0        IFEQ      'P02'
069100150219     C     DEPOP0        oreQ      'O02'
069200970526     C                   MOVEL     DEC(1)        VIDING
069300021211     C                   ENDIF
069400021211     C     DEPOP0        IFEQ      'P05'
069500150219     C     DEPOP0        oreQ      'O05'
069600970526     C                   MOVEL     DEC(2)        VIDING
069700021211     C                   ENDIF
069800970526     C*
069900970526     C                   ENDSR
070000970526     C*------------------------------------------------------------------------*
070100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
070400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
070500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
070600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070800970526     C*------------------------------------------------------------------------*
070900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
071000970526     C*------------------------------------------------------------------------*
071100970526     C     POSSFL        BEGSR
071200970526     C*
071300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
071400970526     C                   EXSR      CLRSFL
071500970526     C*
071600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
071700970526     C                   EXSR      SETFIL
071800970526     C*
071900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
072000970526     C     SFLEOF        IFEQ      0
072100970526     C                   EXSR      CARSFL
072200970526     C                   END
072300970526     C*
072400970526     C                   ENDSR
072500970526     C*------------------------------------------------------------------------*
072600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
072700970526     C*------------------------------------------------------------------------*
072800970526     C     ENTATT        BEGSR
072900970526     C*
073000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
073100970526     C                   Z-ADD     0             OPZOK             1 0
073200970526     C*
073300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
073400970526     C                   Z-ADD     0             SFLVAR            5 0
073500970526     C*
073600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
073700970526     C                   Z-ADD     0             SFLERR
073800970526     C*
073900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
074000970526     C                   Z-ADD     SFLULT        WFLULT
074100970526     C*
074200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
074300021211     C                   READC     SIG1D05                                98
074400970526     C     *IN98         DOWEQ     '0'
074500970526     C*
074600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
074700970526     C                   ADD       1             SFLVAR
074800970526     C*
074900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
075000970526     C     OPZOK         IFEQ      0
075100970526     C*
075200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
075300970526     C     OPZKEY        IFNE      *BLANKS
075400970526     C                   EXSR      OPZCON
075500970526     C                   END
075600970526     C*
075700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
075800970526     C*            2. PULIZIA SCELTA IN SFL
075900970526     C     OPZOK         IFEQ      0
076000970526     C                   MOVEL     '0'           *IN41
076100970526     C                   MOVEL     *BLANKS       OPZKEY
076200970526     C                   ELSE
076300970526     C*
076400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
076500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
076600970526     C                   MOVEL     '1'           *IN41
076700970526     C                   Z-ADD     SFLULT        SFLERR
076800970526     C                   END
076900970526     C                   ELSE
077000970526     C*
077100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
077200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
077300970526     C                   MOVEL     '1'           *IN41
077400970526     C                   END                                                    OPZOK=0
077500970526     C*
077600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
077700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
077800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
077900970526     C     OPZOK         IFNE      0
078000970526     C                   Z-ADD     SFLERR        SFLNBR
078100970526     C                   ELSE
078200970526     C                   Z-ADD     SFLULT        SFLNBR
078300970526     C                   END
078400970526     C*
078500970526     C* AGGIORNO RECORD SFL
078600021211     C                   UPDATE    SIG1D05
078700970526     C*
078800021211     C                   READC     SIG1D05                                98
078900970526     C                   END                                                    DO WHILE
079000970526     C*
079100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
079200970526     C                   Z-ADD     WFLULT        SFLULT
079300970526     C*
079400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
079500970526     C     SFLVAR        IFEQ      0
079600970526     C                   END
079700970526     C*
079800991115     C     OPZKEY        IFNE      ' 5'
079900991115     C                   EXSR      F05GES
080000991115     C                   ENDIF
080100991112     C*
080200970526     C                   ENDSR
080300970526     C*------------------------------------------------------------------------*
080400970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
080500970526     C*------------------------------------------------------------------------*
080600970526     C     CLRSFL        BEGSR
080700970526     C*
080800970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
080900970526     C* (0=NON FINE FILE  1=FINE FILE)
081000970526     C                   Z-ADD     0             SFLEOF
081100970526     C*
081200970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
081300970526     C                   Z-ADD     0             SFLULT
081400970526     C*
081500970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
081600970526     C                   MOVEL     '1'           *IN40
081700021211     C                   WRITE     SIG1D06
081800970526     C                   MOVEL     '0'           *IN40
081900970526     C*
082000970526     C                   ENDSR
082100970526     C*------------------------------------------------------------------------*
082200970526     C* CARSFL - CARICAMENTO SFL
082300970526     C*------------------------------------------------------------------------*
082400970526     C     CARSFL        BEGSR
082500970526     C*
082600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
082700970526     C     SFLULT        IFEQ      0
082800970526     C                   EXSR      SETFIL
082900970526     C                   END
083000970526     C*
083100970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
083200970526     C                   Z-ADD     SFLULT        WFLULT
083300970526     C*
083400970526     C* CICLO:
083500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
083600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
083700970526     C                   Z-ADD     1             SFLRIG
083800970526     C     SFLRIG        DOWLE     SFLPAG
083900970526     C     SFLEOF        ANDEQ     0
084000970526     C*
084100970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
084200991116     C                   MOVEL     *BLANKS       OPZKEY
084300970526     C*
084400970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
084500970526     C                   EXSR      IRESFL
084600970526     C*
084700970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
084800970526     C                   ADD       1             SFLULT
084900970526     C*
085000970526     C* SCRITTURA RECORD SFL
085100021211     C                   WRITE     SIG1D05
085200970526     C                   ADD       1             SFLRIG
085300970526     C*
085400970526     C* LETTURA FILE DA CARICARE
085500970526     C                   EXSR      LETFIL
085600970526     C                   END                                                    DO WHILE
085700970526     C*
085800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
085900970526     C     WFLULT        IFNE      SFLULT
086000970526     C                   Z-ADD     WFLULT        SFLNBR
086100970526     C                   ADD       1             SFLNBR
086200970526     C                   END
086300970526     C*
086400970526     C                   ENDSR
086500970526     C*------------------------------------------------------------------------*
086600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
086700970526     C*------------------------------------------------------------------------*
086800970526     C     OUTSFL        BEGSR
086900970526     C*
087000970526     C* WRITE RECORD TESTATA VIDEO
087100021211     C                   WRITE     SIG1D01
087200970526     C*
087300970526     C* VISUALIZZAZIONE MESSAGGIO
087400970526     C                   EXSR      SNDMSG
087500970526     C*
087600970526     C* WRITE RECORD OPZIONI VIDEO
087700021211     C                   WRITE     SIG1D02
087800970526     C*
087900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
088000021211     C                   WRITE     SIG1D03
088100970526     C*
088200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
088300021211     C                   WRITE     SIG1D04
088400970526     C*
088500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
088600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
088700970526     C     SFLEOF        IFEQ      0
088800970526     C                   MOVEL     '0'           *IN42
088900970526     C                   ELSE
089000970526     C*
089100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
089200970526     C                   MOVEL     '1'           *IN42
089300970526     C                   END
089400970526     C*
089500970526     C* WRITE RECORD TASTI FUNZIONALI
089600021211     C                   WRITE     SIG1D09
089700970526     C*
089800970526     C* CASO DI SFL VUOTO
089900970526     C     SFLULT        IFLE      0
090000970526     C*
090100970526     C*    - WRITE RECORD VUOTO
090200021211     C                   WRITE     SIG1D0A
090300021211     C                   READ      SIG1D0A                                99
090400970526     C*
090500970526     C                   ELSE
090600970526     C*
090700970526     C* CASO DI SFL CON RECORDS
090800970526     C*
090900970526     C*    - WRITE RECORD CONTROLLO SFL
091000021211     C                   WRITE     SIG1D06
091100021211     C                   READ      SIG1D06                                99
091200970526     C                   END
091300970526     C*
091400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
091500021211     C                   READ      SIG1D03                                99
091600970526     C*
091700970526     C                   ENDSR
091800970526     C*------------------------------------------------------------------------*
091900970526     C* SNDMSG - MANDA MESSAGGIO
092000970526     C*------------------------------------------------------------------------*
092100970526     C     SNDMSG        BEGSR
092200970526     C*
092300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
092400970526     C                   MOVEL     '1'           *IN40
092500021211     C                   WRITE     SIG1DMC
092600970526     C                   MOVEL     '0'           *IN40
092700970526     C*
092800970526     C* VISUALIZZAZIONE MESSAGGIO
092900970526     C                   Z-ADD     0             MSGULT
093000970526     C                   CALL      'XMSG'        XMSGPA
093100970526     C     DSMSER        IFEQ      0
093200970526     C                   MOVEL     DSMSKE        MSGKEY
093300970526     C                   END
093400970526     C                   Z-ADD     1             MSGULT
093500021211     C                   WRITE     SIG1DMS                                      SFL MESS
093600021211     C                   WRITE     SIG1DMC                                      CONTR MESS
093700970526     C*
093800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
093900970526     C                   MOVEL     *BLANKS       DSMSMS
094000970526     C*
094100970526     C                   ENDSR
094200970526     C*------------------------------------------------------------------------*
094300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
094400970526     C*------------------------------------------------------------------------*
094500970526     C     FUNCON        BEGSR
094600970526     C*
094700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
094800970526     C                   EXSR      FUNVAR
094900970526     C*
095000970526     C* PULISCO CAMPI DI LAVORO
095100970526     C                   Z-ADD     0             I                 5 0
095200970526     C                   Z-ADD     0             W                 5 0
095300970526     C*
095400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
095500970526     C     FUNKEY        IFNE      *BLANKS
095600970526     C     1             DO        24            I
095700970526     C                   MOVEL     FUD(I)        FUDRIG
095800970526     C*
095900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
096000970526     C     FUNKEY        IFEQ      FUDFUN
096100970526     C     FUDATT        ANDEQ     'S'
096200970526     C                   Z-ADD     I             W
096300970526     C                   END
096400970526     C                   END                                                    DO WHILE
096500970526     C*
096600970526     C* FUNZIONE NON ATTIVA (ERRORE)
096700970526     C     W             IFEQ      0
096800970526     C*
096900970526     C* EMISSIONE MESSAGGIO DI ERRORE
097000970526     C                   MOVEL     MSG(2)        DSMSMS
097100970526     C                   END                                                    W=0
097200970526     C                   END                                                    FUNKEY NE *BLANKS
097300970526     C*
097400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
097500970526     C     W             IFNE      0
097600970526     C     FUNKEY        OREQ      *BLANKS
097700970526     C*
097800970526     C     FUNKEY        IFEQ      *BLANKS
097900970526     C                   EXSR      ENTGES
098000970526     C                   END
098100970526     C     FUNKEY        IFEQ      '03'
098200970526     C                   EXSR      F03GES
098300970526     C                   END
098400970526     C     FUNKEY        IFEQ      '05'
098500970526     C                   EXSR      F05GES
098600970526     C                   END
098700150204     C     FUNKEY        IFEQ      '06'
098800150204     C                   EXSR      F06GES
098900970526     C                   END
099000970526     C     FUNKEY        IFEQ      '12'
099100970526     C                   EXSR      F12GES
099200970526     C                   END
099300970528     C     FUNKEY        IFEQ      '21'
099400970528     C                   EXSR      F21GES
099500970528     C                   END
099600970528     C     FUNKEY        IFEQ      '22'
099700970528     C                   EXSR      F22GES
099800970528     C                   END
099900970526     C     FUNKEY        IFEQ      '23'
100000970526     C                   EXSR      F23GES
100100970526     C                   END
100200970526     C     FUNKEY        IFEQ      '24'
100300970526     C                   EXSR      F24GES
100400970526     C                   END
100500970526     C     FUNKEY        IFEQ      'RU'
100600970526     C                   EXSR      RUPGES
100700970526     C                   END
100800970526     C*
100900970526     C                   END
101000970526     C*
101100970526     C                   ENDSR
101200970526     C*------------------------------------------------------------------------*
101300970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
101400970526     C*------------------------------------------------------------------------*
101500970526     C     FUNVAR        BEGSR
101600970526     C*
101700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
101800970526     C     KEY           IFEQ      ENTER
101900970526     C                   MOVEL     *BLANKS       FUNKEY
102000970526     C                   END
102100970526     C     KEY           IFEQ      F1
102200970526     C                   MOVEL     '01'          FUNKEY
102300970526     C                   END
102400970526     C     KEY           IFEQ      F2
102500970526     C                   MOVEL     '02'          FUNKEY
102600970526     C                   END
102700970526     C     KEY           IFEQ      F3
102800970526     C                   MOVEL     '03'          FUNKEY
102900970526     C                   END
103000970526     C     KEY           IFEQ      F4
103100970526     C                   MOVEL     '04'          FUNKEY
103200970526     C                   END
103300970526     C     KEY           IFEQ      F5
103400970526     C                   MOVEL     '05'          FUNKEY
103500970526     C                   END
103600970526     C     KEY           IFEQ      F6
103700970526     C                   MOVEL     '06'          FUNKEY
103800970526     C                   END
103900970526     C     KEY           IFEQ      F7
104000970526     C                   MOVEL     '07'          FUNKEY
104100970526     C                   END
104200970526     C     KEY           IFEQ      F8
104300970526     C                   MOVEL     '08'          FUNKEY
104400970526     C                   END
104500970526     C     KEY           IFEQ      F9
104600970526     C                   MOVEL     '09'          FUNKEY
104700970526     C                   END
104800970526     C     KEY           IFEQ      F10
104900970526     C                   MOVEL     '10'          FUNKEY
105000970526     C                   END
105100970526     C     KEY           IFEQ      F11
105200970526     C                   MOVEL     '11'          FUNKEY
105300970526     C                   END
105400970526     C     KEY           IFEQ      F12
105500970526     C                   MOVEL     '12'          FUNKEY
105600970526     C                   END
105700970526     C     KEY           IFEQ      F13
105800970526     C                   MOVEL     '13'          FUNKEY
105900970526     C                   END
106000970526     C     KEY           IFEQ      F14
106100970526     C                   MOVEL     '14'          FUNKEY
106200970526     C                   END
106300970526     C     KEY           IFEQ      F15
106400970526     C                   MOVEL     '15'          FUNKEY
106500970526     C                   END
106600970526     C     KEY           IFEQ      F16
106700970526     C                   MOVEL     '16'          FUNKEY
106800970526     C                   END
106900970526     C     KEY           IFEQ      F17
107000970526     C                   MOVEL     '17'          FUNKEY
107100970526     C                   END
107200970526     C     KEY           IFEQ      F18
107300970526     C                   MOVEL     '18'          FUNKEY
107400970526     C                   END
107500970526     C     KEY           IFEQ      F19
107600970526     C                   MOVEL     '19'          FUNKEY
107700970526     C                   END
107800970526     C     KEY           IFEQ      F20
107900970526     C                   MOVEL     '20'          FUNKEY
108000970526     C                   END
108100970526     C     KEY           IFEQ      F21
108200970526     C                   MOVEL     '21'          FUNKEY
108300970526     C                   END
108400970526     C     KEY           IFEQ      F22
108500970526     C                   MOVEL     '22'          FUNKEY
108600970526     C                   END
108700970526     C     KEY           IFEQ      F23
108800970526     C                   MOVEL     '23'          FUNKEY
108900970526     C                   END
109000970526     C     KEY           IFEQ      F24
109100970526     C                   MOVEL     '24'          FUNKEY
109200970526     C                   END
109300970526     C     KEY           IFEQ      ROLLUP
109400970526     C                   MOVEL     'RU'          FUNKEY
109500970526     C                   END
109600970526     C     KEY           IFEQ      HELP
109700970526     C                   MOVEL     'HP'          FUNKEY
109800970526     C                   END
109900970526     C*
110000970526     C                   ENDSR
110100970526     C*------------------------------------------------------------------------*
110200970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
110300970526     C*------------------------------------------------------------------------*
110400970526     C     FUNGES        BEGSR
110500970526     C*
110600970526     C* PULISCO CAMPI DI LAVORO
110700970526     C                   Z-ADD     0             W                 5 0
110800970526     C*
110900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
111000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
111100970526     C     1             DO        24            I
111200970526     C                   MOVEL     FUD(I)        FUDRIG
111300970526     C*
111400970526     C* PULIZIA FUNZIONI PRECEDENTI
111500970526     C                   MOVEL     *BLANKS       FUV(I)
111600970526     C*
111700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
111800970526     C* (SALTA F23 E F24)
111900970526     C     FUDATT        IFEQ      'S'
112000970526     C     FUDFUN        ANDNE     '23'
112100970526     C     FUDFUN        ANDNE     '24'
112200970526     C                   ADD       1             W
112300970526     C                   MOVEL     FUDDES        FUV(W)
112400970526     C                   END
112500970526     C                   END                                                    DO WHILE
112600970526     C*
112700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
112800970526     C* ALTRIMENTI DISATTIVO F24
112900970526     C     1             DO        24            I
113000970526     C                   MOVEL     FUD(I)        FUDRIG
113100970526     C*
113200970526     C     FUDFUN        IFEQ      '24'
113300970526     C     W             IFGT      6
113400970526     C                   MOVEL     'S'           FUDATT
113500970526     C                   ELSE
113600970526     C                   MOVEL     *BLANKS       FUDATT
113700970526     C                   END                                                    W GT 8
113800970526     C                   MOVEL     FUDRIG        FUD(I)
113900970526     C                   END
114000970526     C                   END                                                    DO WHILE
114100970526     C*
114200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
114300970526     C     W             DIV       6             I
114400970526     C                   MVR                     Z
114500970526     C*
114600970526     C     Z             IFNE      0
114700970526     C     I             ADD       1             Z
114800970526     C                   ELSE
114900970526     C                   Z-ADD     I             Z
115000970526     C                   END
115100970526     C*
115200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
115300970526     C                   ADD       1             FUNGRU
115400970526     C     FUNGRU        IFGT      Z
115500970526     C     FUNGRU        ORLT      1
115600970526     C                   Z-ADD     1             FUNGRU
115700970526     C                   END
115800970526     C*
115900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
116000970526     C     FUNGRU        IFEQ      1
116100970526     C                   MOVEL     FUV(1)        FUN001
116200970526     C                   MOVEL     FUV(2)        FUN002
116300970526     C                   MOVEL     FUV(3)        FUN003
116400970526     C                   MOVEL     FUV(4)        FUN004
116500970526     C                   MOVEL     FUV(5)        FUN005
116600970526     C                   MOVEL     FUV(6)        FUN006
116700970526     C                   END
116800970526     C*
116900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
117000970526     C     FUNGRU        IFEQ      2
117100970526     C                   MOVEL     FUV(7)        FUN001
117200970526     C                   MOVEL     FUV(8)        FUN002
117300970526     C                   MOVEL     FUV(9)        FUN003
117400970526     C                   MOVEL     FUV(10)       FUN004
117500970526     C                   MOVEL     FUV(11)       FUN005
117600970526     C                   MOVEL     FUV(12)       FUN006
117700970526     C                   END
117800970526     C*
117900970526     C* TERZO GRUPPO DI 6 FUNZIONI
118000970526     C     FUNGRU        IFEQ      3
118100970526     C                   MOVEL     FUV(13)       FUN001
118200970526     C                   MOVEL     FUV(14)       FUN002
118300970526     C                   MOVEL     FUV(15)       FUN003
118400970526     C                   MOVEL     FUV(16)       FUN004
118500970526     C                   MOVEL     FUV(17)       FUN005
118600970526     C                   MOVEL     FUV(18)       FUN006
118700970526     C                   END
118800970526     C*
118900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
119000970526     C     FUNGRU        IFEQ      4
119100970526     C                   MOVEL     FUV(19)       FUN001
119200970526     C                   MOVEL     FUV(20)       FUN002
119300970526     C                   MOVEL     FUV(21)       FUN003
119400970526     C                   MOVEL     FUV(22)       FUN004
119500970526     C                   MOVEL     *BLANKS       FUN005
119600970526     C                   MOVEL     *BLANKS       FUN006
119700970526     C                   END
119800970526     C*
119900970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
120000970526     C                   MOVEL     *BLANKS       FUN007
120100970526     C                   MOVEL     *BLANKS       FUN008
120200970526     C*
120300970526     C     1             DO        24            I
120400970526     C                   MOVEL     FUD(I)        FUDRIG
120500970526     C     FUDFUN        IFEQ      '23'
120600970526     C     FUDATT        ANDEQ     'S'
120700970526     C                   MOVEL     FUDDES        FUN007
120800970526     C                   END
120900970526     C     FUDFUN        IFEQ      '24'
121000970526     C     FUDATT        ANDEQ     'S'
121100970526     C                   MOVEL     FUDDES        FUN008
121200970526     C                   END
121300970526     C                   END                                                    DO
121400970526     C*
121500970526     C                   ENDSR
121600970526     C*------------------------------------------------------------------------*
121700970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
121800970526     C*------------------------------------------------------------------------*
121900970526     C     OPZGES        BEGSR
122000970526     C*
122100970526     C* PULISCO CAMPI DI LAVORO
122200970526     C                   Z-ADD     0             W                 5 0
122300970526     C*
122400970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
122500970526     C* E CARICO TABELLA OPZIONI ATTIVE
122600991115     C     1             DO        24            I
122700970526     C                   MOVEL     OPD(I)        OPDRIG
122800970526     C*
122900970526     C* PULIZIA OPZIONI PRECEDENTI
123000970526     C                   MOVEL     *BLANKS       OPV(I)
123100970526     C*
123200970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
123300970526     C     OPDATT        IFEQ      'S'
123400970526     C                   ADD       1             W
123500970526     C                   MOVEL     OPDDES        OPV(W)
123600970526     C                   END
123700970526     C                   END                                                    DO WHILE
123800970526     C*
123900970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
124000970526     C* ALTRIMENTI DISATTIVO F23
124100991115     C     1             DO        24            I
124200970526     C                   MOVEL     FUD(I)        FUDRIG
124300970526     C*
124400970526     C     FUDFUN        IFEQ      '23'
124500970526     C     W             IFGT      8
124600970526     C                   MOVEL     'S'           FUDATT
124700970526     C                   ELSE
124800970526     C                   MOVEL     *BLANKS       FUDATT
124900970526     C                   END                                                    W GT 8
125000970526     C                   MOVEL     FUDRIG        FUD(I)
125100970526     C                   END
125200970526     C                   END                                                    DO WHILE
125300970526     C*
125400970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
125500970526     C     W             DIV       8             I
125600970526     C                   MVR                     Z                 5 0
125700970526     C*
125800970526     C     Z             IFNE      0
125900970526     C     I             ADD       1             Z
126000970526     C                   ELSE
126100970526     C                   Z-ADD     I             Z
126200970526     C                   END
126300970526     C*
126400970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
126500970526     C                   ADD       1             OPZGRU
126600970526     C     OPZGRU        IFGT      Z
126700970526     C     OPZGRU        ORLT      1
126800970526     C                   Z-ADD     1             OPZGRU
126900970526     C                   END
127000970526     C*
127100970526     C* PRIMO GRUPPO DI 8 OPZIONI
127200970526     C     OPZGRU        IFEQ      1
127300970526     C                   MOVEL     OPV(1)        OPZ001
127400970526     C                   MOVEL     OPV(2)        OPZ002
127500970526     C                   MOVEL     OPV(3)        OPZ003
127600970526     C                   MOVEL     OPV(4)        OPZ004
127700970526     C                   MOVEL     OPV(5)        OPZ005
127800970526     C                   MOVEL     OPV(6)        OPZ006
127900970526     C                   MOVEL     OPV(7)        OPZ007
128000970526     C                   MOVEL     OPV(8)        OPZ008
128100970526     C                   END
128200970526     C*
128300970526     C* SECONDO GRUPPO DI 8 OPZIONI
128400970526     C     OPZGRU        IFEQ      2
128500970526     C                   MOVEL     OPV(9)        OPZ001
128600970526     C                   MOVEL     OPV(10)       OPZ002
128700970526     C                   MOVEL     OPV(11)       OPZ003
128800970526     C                   MOVEL     OPV(12)       OPZ004
128900970526     C                   MOVEL     OPV(13)       OPZ005
129000970526     C                   MOVEL     OPV(14)       OPZ006
129100970526     C                   MOVEL     OPV(15)       OPZ007
129200970526     C                   MOVEL     OPV(16)       OPZ008
129300970526     C                   END
129400970526     C*
129500970526     C* TERZO GRUPPO DI 8 OPZIONI
129600970526     C     OPZGRU        IFEQ      3
129700970526     C                   MOVEL     OPV(17)       OPZ001
129800970526     C                   MOVEL     OPV(18)       OPZ002
129900970526     C                   MOVEL     OPV(19)       OPZ003
130000970526     C                   MOVEL     OPV(20)       OPZ004
130100970526     C                   MOVEL     OPV(21)       OPZ005
130200970526     C                   MOVEL     OPV(22)       OPZ006
130300970526     C                   MOVEL     OPV(23)       OPZ007
130400970526     C                   MOVEL     OPV(24)       OPZ008
130500970526     C                   END
130600970526     C*
130700970526     C                   ENDSR
130800970526     C*------------------------------------------------------------------------*
130900970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
131000970526     C*------------------------------------------------------------------------*
131100970526     C     OPZCON        BEGSR
131200970526     C*
131300970526     C* PULISCO CAMPI DI LAVORO
131400970526     C                   Z-ADD     0             I                 5 0
131500970526     C                   Z-ADD     0             W                 5 0
131600970526     C*
131700970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
131800970526     C     1             DO        24            I
131900970526     C                   MOVEL     OPD(I)        OPDRIG
132000970526     C*
132100970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
132200970526     C     OPZKEY        IFEQ      OPDOPZ
132300970526     C     OPZKEY        OREQ      OPDOP1
132400970526     C*
132500970526     C     OPDATT        IFEQ      'S'
132600970526     C                   MOVEL     OPDOPZ        OPZKEY
132700970526     C                   Z-ADD     I             W
132800970526     C                   END
132900970526     C                   END
133000970526     C*
133100970526     C                   END                                                    DO WHILE
133200970526     C*
133300970526     C* OPZIONE NON ATTIVA (ERRORE)
133400970526     C     W             IFEQ      0
133500970526     C*
133600970526     C* OPZIONE NON OK
133700970526     C                   Z-ADD     1             OPZOK
133800970526     C*
133900970526     C* EMISSIONE MESSAGGIO DI ERRORE
134000970526     C                   MOVEL     MSG(1)        DSMSMS
134100970526     C                   ELSE
134200970526     C*
134300970526     C* OPZIONE ATTIVA (PROCEDI ...)
134400970526     C*
134500021211     C                   MOVEL     *BLANKS       PMSG
134600000420     C     OPZKEY        IFEQ      ' 1'
134700000420     C                   EXSR      O01GES
134800000420     C                   END
134900000407     C     OPZKEY        IFEQ      ' 2'
135000000407     C                   EXSR      O02GES
135100000407     C                   END
135200021211     C     OPZKEY        IFEQ      ' 3'
135300021211     C                   EXSR      O03GES
135400021211     C                   END
135500991110     C     OPZKEY        IFEQ      ' 4'
135600991110     C                   EXSR      O04GES
135700991014     C                   END
135800970526     C     OPZKEY        IFEQ      ' 5'
135900970526     C                   EXSR      O05GES
136000970526     C                   END
136100030123     C                   MOVEL     PMSG          DSMSMS
136200021211     C                   END                                                    OPZ.NON ATTIVA
136300970526     C*
136400970526     C                   ENDSR
136500970526     C*------------------------------------------------------------------------*
136600970526     C* NOTE COSTRUZIONE SCHIERE
136700970526     C*------------------------------------------------------------------------*
136800970526     C*
136900970526     C* -MSG-MESSAGGI
137000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
137100970526     C* (NON TOCCARE MSG,1 E MSG,2)
137200970526     C*
137300970526     C* -CMD-COMANDI
137400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
137500970526     C*
137600970526     C* -OPD-OPZIONI
137700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
137800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
137900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
138000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
138100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
138200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
138300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
138400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
138500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
138600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
138700970526     C*
138800970526     C* -FUD-FUNZIONI
138900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
139000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
139100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
139200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
139300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
139400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
139500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
139600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
139700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
139800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
139900991108     C*
140000991115     C* -STS-CODICI STATI DEI RECORD DI LOG
140100991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
140200991110     C* ================> IMPORTANTE <=================
140300991115     C* per codici di errore in -STS- indicare in -DES- sempre
140400991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
140500991110     C* ERR
140600991108     C*
140700970526     O*------------------------------------------------------------------------*
140800970526** DEC - DECODIFICHE
140900970526    GESTIONE                                                     1
141000000407 INTERROGAZIONE                                                  2
141100970530     SCELTA                                                      3
1412009705264
1413009705265
141400970526** MSG - MESSAGGI
141500150210TISIG12R- Opzione non gestita.                                                  1
141600150210TISIG12R- Tasto funzionale non gestito.                                         2
141700150210TISIG12R- Utente non abilitato all'inserimento.                                 3
141800150210TISIG12R- Impossibile annullare, presenti movimenti su tale codice articolo.    4
141900150210TISIG12R- 5                                                                     5
142000150210TISIG12R- 6                                                                     6
142100150210TISIG12R- 7                                                                     7
142200150210TISIG12R- 8                                                                     8
142300150210TISIG12R- 9                                                                     9
142400150210TISIG12R- 10                                                                   10
142500150210TISIG12R- 11                                                                   11
142600150210TISIG12R- 12                                                                   12
142700150210TISIG12R- 13                                                                   13
142800150210TISIG12R- 14                                                                   14
142900150210TISIG12R- 15                                                                   15
143000150210TISIG12R- 16                                                                   16
143100150210TISIG12R- 17                                                                   17
143200150210TISIG12R- 18                                                                   18
143300150210TISIG12R- 19                                                                   19
143400150210TISIG12R- 20                                                                   20
143500970528** CMD - COMANDI CL
143600021211
143700021211
143800991110
143900970528
144000970528
144100970526** OPD - OPZIONI
144200000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
144300150219 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
144400150219 3=Copia           -3 -     (S/N)=N                        opz. 03¬
144500150202 4=Cancellazione   -4 -     (S/N)=S                        opz. 04¬
144600150202 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
144700991115                   -  -     (S/N)=N                        opz. 06¬
144800021211                   -  -     (S/N)=N                        opz. 07¬
144900991110                   -  -     (S/N)=N                        opz. 08¬
145000991110                   -  -     (S/N)=N                        opz. 09¬
145100991110                   -  -     (S/N)=N                        opz. 10¬
145200000407                   -  -     (S/N)=N                        opz. 11¬
145300970526                   -  -     (S/N)=N                        opz. 12¬
145400970526                   -  -     (S/N)=N                        opz. 13¬
145500000407                   -  -     (S/N)=N                        opz. 14¬
145600000407                   -  -     (S/N)=N                        opz. 15¬
145700970526                   -  -     (S/N)=N                        opz. 16¬
145800970526                   -  -     (S/N)=N                        opz. 17¬
145900970526                   -  -     (S/N)=N                        opz. 18¬
146000970526                   -  -     (S/N)=N                        opz. 19¬
146100970526                   -  -     (S/N)=N                        opz. 20¬
146200970526                   -  -     (S/N)=N                        opz. 21¬
146300970526                   -  -     (S/N)=N                        opz. 22¬
146400970526                   -  -     (S/N)=N                        opz. 23¬
146500000407                   -  -     (S/N)=N                        opz. 25¬
146600970526** FUD - FUNZIONI
146700970526                   -        (S/N)=N                        funz.01¬
146800970526                   -        (S/N)=N                        funz.02¬
146900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
147000970526                   -        (S/N)=N                        funz.04¬
147100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
147200150204F06=Inserimento    -        (S/N)=S  <== S-FISSO           funz.06¬
147300970526                   -        (S/N)=N                        funz.07¬
147400970527                   -        (S/N)=N                        funz.08¬
147500970527                   -        (S/N)=N                        funz.09¬
147600150204                   -        (S/N)=N  <== S-FISSO           funz.10¬
147700970526                   -        (S/N)=N                        funz.11¬
147800970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
147900991116                   -        (S/N)=N                        funz.13¬
148000970526                   -        (S/N)=N                        funz.14¬
148100970526                   -        (S/N)=N                        funz.15¬
148200970526                   -        (S/N)=N                        funz.16¬
148300970526                   -        (S/N)=N                        funz.17¬
148400970526                   -        (S/N)=N                        funz.18¬
148500970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
148600970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
148700000407                   -        (S/N)=N                        funz.21¬
148800991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
148900991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
149000991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
