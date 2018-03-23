000100970526     H*------------------------------------------------------------------------*
000200150203     H* Gestione file anagrafico e Bancali
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800150130     FTISIG10d  CF   E             WORKSTN INFDS(DEVDS1)
000900021211     F                                     SFILE(SIG1D05:SFLULT)
001000021211     F                                     SFILE(SIG1DMS:MSGULT)
001100970526     F*------------
001200150224     Fazcgb01l  iF   E           K DISK
001300150130     Fazmgb04l  IF   E           K DISK
001400150210     Fazfgb01l  IF   E           K DISK
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
008500020917     D* DS REPERIMENTO DATI UTENTE
008600970526     D*------------
008700020917     D TIBS34DS      E DS                                                       *Profili utente
008800020917     D DDATIUTE      E DS                                                       *Dati utente
008900020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009000150212     D dute01        E DS                                                       *Utenti
009100150212     d dLat          e ds
009200150212     d TIBS02DS      E DS
009300150212     D*-------------------
009400150212     D TRUL31DS      E DS
009500150212     D TRUL31DS2     E DS
009600150212     d wabi            s                   like(UteAut)
009700150212     d* p.o. abilitati
009800150212     D $pog            s              3    dim(250)
009900150212     D $DIG            s              1    dim(20)
010000150212     D $ARG            s              3    dim(50)
010100970527     D*------------------
010200991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
010300970527     D*------------------
010400970527     D WLBDA8          DS                  INZ
010500970527     D  G08DAT                 1      8  0
010600970527     D  G08INV                 9     16  0
010700970527     D  G08ERR                17     17
010800970527     D  G08TGI                18     22  0
010900970526     D*------------
011000991110     D* DS DI PROCEDURA
011100970526     D*------------
011200970526     D* ARCHITETTURA
011300970526     D*------------
011400970526     D KPJBA         E DS
011500150211     D savKPJBu        s                   like(kpjbu)
011600150202      *
011700150202     d Tibs69DS      E DS                  INZ
011800150202     d DS_cnaco      E DS                  extname(CNACO00F)
011900150202     d DS_cnind      E DS                  extname(CNIND00F)
012000150202     d DS_cnclp      E DS                  extname(CNCLP00F)
012100150202     d DS_fncls      E DS                  extname(FNCLS00F)
012200150203      *
012300021211     D*------------------
012400021211     D* PASSAGGIO PARAMETRI
012500021211     D*------------------
012600021211     D PARAM           DS                  INZ
012700150130     d  pop1                          3
012800150130     D  Pksu                          8
012900150130     D  Ptsc                          1
013000150302     D  Pdti                          8  0
013100150130     D  Pmsg                         80
013200150211     D PARAMb          DS                  INZ
013300150211     d  pop1b                         3
013400150211     D  Pksub                         8
013500150211     D  Pkscb                         1
013600150211     D  Pmsgb                        80
013700970526     C*------------------------------------------------------------------------*
013800970526     C* MAIN LINES
013900970526     C*------------------------------------------------------------------------*
014000970526     C*
014100970526     C* SE NON CI SONO ERRORI, PROSEGUE
014200970526IF  1C     ERRL00        IFNE      '1'
014300970526     C*
014400970526     C* CARICA IL SFILE
014500970526     C                   EXSR      POSSFL
014600970526     C*
014700970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
014800970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
014900970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
015000970526     C     FINL00        OREQ      '1'                                          *FINE PGM
015100970526     C*
015200970526     C* REIMPOSTA ERRORE DI PROCERURA
015300970526     C                   MOVEL     '0'           ERRL00
015400970526     C*
015500970526     C* EMETTE IL SFILE
015600970526     C                   EXSR      OUTSFL
015700970526     C*
015800970526     C* GESTIONE TASTI FUNZIONALI
015900970526     C                   EXSR      FUNCON
016000970526     C*
016100970526E   2C                   ENDDO
016200970527     C*
016300970526E   1C                   ENDIF
016400991108     C*
016500970526     C* FINE PGM
016600970526     C                   SETON                                        LR
016700970526     C*------------------------------------------------------------------------*
016800970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
016900970526     C*------------------------------------------------------------------------*
017000970526     C     F03GES        BEGSR
017100970526     C*
017200970526     C* IMPOSTA "PREMUTO F03"
017300970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
017400970526     C*
017500970526     C                   ENDSR
017600970526     C*------------------------------------------------------------------------*
017700970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
017800970526     C*------------------------------------------------------------------------*
017900970526     C     F05GES        BEGSR
018000970526     C*
018100970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
018200970526     C*
018300970526     C                   ENDSR
018400970526     C*------------------------------------------------------------------------*
018500150204     C* F06GES - GESTIONE F06 -> INSERIMENTO
018600970526     C*------------------------------------------------------------------------*
018700150204     C     F06GES        BEGSR
018800970526     C*
018900021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
019000150213     C*    DUTLPO        IFNE      'S'
019100150213     C*                  MOVEL     '1'           ERRL00
019200150213     C*                  MOVEL     MSG(3)        DSMSMS
019300150213     C*                  ENDIF
019400000407     C*
019500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
019600150204     C                   MOVEL     'F06'         POP1
019700000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
019800150130     C                   CALL      'TISIG11R'
019900000407     C                   PARM                    KPJBA
020000021211     C                   MOVEL     KPJBU         PARAM
020100000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
020200021211     C                   ENDIF
020300000407     C*
020400970526     C                   ENDSR
020500970526     C*------------------------------------------------------------------------*
020600991108     C* F12GES - GESTIONE F12 -> RITORNO
020700970526     C*------------------------------------------------------------------------*
020800970526     C     F12GES        BEGSR
020900970526     C*
021000970526     C* IMPOSTA "PREMUTO F12"
021100970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
021200970526     C*
021300970526     C                   ENDSR
021400970528     C*------------------------------------------------------------------------*
021500970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
021600970528     C*------------------------------------------------------------------------*
021700970528     C     F21GES        BEGSR
021800970612     C*
021900991117     C* NON GESTITA
022000970528     C*
022100970528     C                   ENDSR
022200970528     C*------------------------------------------------------------------------*
022300970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
022400970528     C*------------------------------------------------------------------------*
022500970528     C     F22GES        BEGSR
022600970528     C*
022700021211     C* NON GESTITA
022800970528     C*
022900970528     C                   ENDSR
023000970526     C*------------------------------------------------------------------------*
023100970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
023200970526     C*------------------------------------------------------------------------*
023300970526     C     F23GES        BEGSR
023400970526     C*
023500970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
023600970526     C*
023700970526     C                   ENDSR
023800970526     C*------------------------------------------------------------------------*
023900970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
024000970526     C*------------------------------------------------------------------------*
024100970526     C     F24GES        BEGSR
024200970526     C*
024300970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
024400970526     C*
024500970526     C                   ENDSR
024600970526     C*------------------------------------------------------------------------*
024700970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
024800970526     C*------------------------------------------------------------------------*
024900970526     C     RUPGES        BEGSR
025000970526     C*
025100970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
025200970526     C*
025300970526     C                   ENDSR
025400970526     C*------------------------------------------------------------------------*
025500991108     C* ENTGES - GESTIONE ENTER -> INVIO
025600970526     C*------------------------------------------------------------------------*
025700970526     C     ENTGES        BEGSR
025800000407     C*
025900000407     C                   MOVEL     '0'           ERRL00
026000991108     C*
026100970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
026200970526     C                   EXSR      NEWPOS
026300021211     C*
026400021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
026500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
026600021211     C     SWPOSI        IFNE      0
026700021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
026800021211     C                   ELSE
026900021211     C     SFLULT        IFNE      0
027000021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
027100021211     C*
027200021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
027300021211     C     SWMODI        IFNE      0
027400021211     C                   EXSR      POSSFL
027500021211     C                   ENDIF
027600021211     C                   ENDIF
027700021211     C                   ENDIF
027800021211     C                   ENDIF
027900021211     C*
028000021211     C                   ENDSR
028100000420     C*------------------------------------------------------------------------*
028200000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
028300000420     C*------------------------------------------------------------------------*
028400000420     C     O01GES        BEGSR
028500000420     C*
028600000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
028700150130     C                   MOVEL     VIDksu        Pksu
028800000420     C                   MOVEL     PARAM         KPJBU
028900000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
029000000420     C*
029100000420     C                   ENDSR
029200000407     C*------------------------------------------------------------------------*
029300000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
029400000407     C*------------------------------------------------------------------------*
029500000407     C     O02GES        BEGSR
029600000407     C*
029700021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
029800150213     C*    DUTLPO        IFNE      'S'
029900150213     C*                  MOVEL     '1'           ERRL00
030000150213     C*                  MOVEL     MSG(3)        pmsg
030100150213     C*                  ENDIF
030200021211     C*
030300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030400150130     C                   MOVEL     VIDksu        Pksu
030500150130     C                   MOVEL     VIDtsc        Ptsc
030600150302     c                   movel     viddtig       pdti
030700000407     C                   MOVEL     'O02'         POP1
030800000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
030900150130     C                   CALL      'TISIG11R'
031000000407     C                   PARM                    KPJBA
031100021211     C                   MOVEL     KPJBU         PARAM
031200021211     C                   ENDIF
031300000407     C*
031400000407     C                   ENDSR
031500021211     C*------------------------------------------------------------------------*
031600021211     C* O03GES - GESTIONE OPZIONE 3= COPIA
031700021211     C*------------------------------------------------------------------------*
031800021211     C     O03GES        BEGSR
031900021211     C*
032000021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
032100150213     C*    DUTLPO        IFNE      'S'
032200150213     C*                  MOVEL     '1'           ERRL00
032300150213     C*                  MOVEL     MSG(3)        PMSG
032400150213     C*                  ENDIF
032500021211     C*
032600021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032700150130     C                   MOVEL     VIDksu        Pksu
032800150130     C                   MOVEL     VIDtsc        Ptsc
032900150302     c                   movel     viddtig       pdti
033000021211     C                   MOVEL     'O03'         POP1
033100021211     C                   EXSR      PASPAR                                       *RICARICA SFILE
033200150130     C                   CALL      'TISIG11R'
033300021211     C                   PARM                    KPJBA
033400021211     C                   MOVEL     KPJBU         PARAM
033500021211     C                   ENDIF
033600021211     C*
033700021211     C                   ENDSR
033800991112     C*------------------------------------------------------------------------*
033900021211     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
034000991112     C*------------------------------------------------------------------------*
034100991112     C     O04GES        BEGSR
034200021211     C*
034300021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
034400150213     C*    DUTLPO        IFNE      'S'
034500150213     C*                  MOVEL     '1'           ERRL00
034600150213     C*                  MOVEL     MSG(3)        PMSG
034700150213     C*                  ENDIF
034800991112     C*
034900021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
035000021211     C*
035100021211     C* VERIFICO CHE NN VI SIANO PRESENTI MOVIMENTO PER L'ARICOLO CHE SI VUOLE ANNULLARE
035200150130     C                   MOVEL     VIDksu        KEYksu
035300150130     C     KEYMGBP       SETLL     azmgb04l                               55
035400021211     C*
035500021211     C                   IF        *IN55 = *ON
035600030123     C                   MOVEL     MSG(4)        PMSG
035700021211     C                   ELSE
035800150130     C                   MOVEL     VIDksu        Pksu
035900150130     C                   MOVEL     VIDtsc        Ptsc
036000150302     c                   movel     viddtig       pdti
036100000407     C                   MOVEL     'O04'         POP1
036200000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
036300150130     C                   CALL      'TISIG11R'
036400000407     C                   PARM                    KPJBA
036500021211     C                   MOVEL     KPJBU         PARAM
036600021211     C                   ENDIF
036700000411     C                   ENDIF
036800991112     C*
036900991112     C                   ENDSR
037000991014     C*------------------------------------------------------------------------*
037100000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
037200970526     C*------------------------------------------------------------------------*
037300970526     C     O05GES        BEGSR
037400000407     C*
037500021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
037600150130     C                   MOVEL     VIDksu        Pksu
037700150130     C                   MOVEL     VIDtsc        Ptsc
037800150302     c                   movel     viddtig       pdti
037900000407     C                   MOVEL     'O05'         POP1
038000000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
038100150130     C                   CALL      'TISIG11R'
038200000407     C                   PARM                    KPJBA
038300021211     C                   ENDIF
038400000407     C*
038500970526     C                   ENDSR
038600150211     C*------------------------------------------------------------------------*
038700150211     C* O0FGES - GESTIONE OPZIONE F=VISUALIZZAZIONE abbinati
038800150211     C*------------------------------------------------------------------------*
038900150211     C     O0FGES        BEGSR
039000150211     C*
039100150211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
039200150211     C
039300150211     C                   clear                   Pkscb
039400150211     c                   movel     vidksu        pksub
039500150302     c                   if        pop1 = 'P01'
039600150302     C                   MOVEL     'P05'         POP1b
039700150302     c                   else
039800150211     C                   MOVEL     'P02'         POP1b
039900150302     c                   endif
040000150211     C                   movel     kpjbu         savkpjbu                       *RICARICA SFILE
040100150211     C                   movel     paramb        kpjbu
040200150211     C                   CALL      'TISIG12R'
040300150211     C                   PARM                    KPJBA
040400150211     C                   movel     kpjbu         paramb
040500150211     C                   movel     savkpjbu      kpjbu
040600150211     C                   ENDIF
040700150211     C*
040800150211     C                   ENDSR
040900991028     C*------------------------------------------------------------------------*
041000991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
041100991112     C*------------------------------------------------------------------------*
041200991112     C     PASPAR        BEGSR
041300991112     C*
041400000407     C                   MOVEL     PARAM         KPJBU
041500991112     C*
041600991112     C                   ENDSR
041700970526     C*------------------------------------------------------------------------*
041800970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
041900970526     C*------------------------------------------------------------------------*
042000970526     C     NEWPOS        BEGSR
042100970526     C*
042200970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
042300970526     C*         1= RICHIESTO POSIZIONAMENTO
042400970526     C                   Z-ADD     0             SWPOSI            7 0
042500970526     C*
042600991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
042700970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
042800970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
042900970526     C                   Z-ADD     0             SWMODI            1 0
043000970530     C*
043100970530     C* REIMPOSTA INDICATORI DI ERRORE
043200991108     C                   SETOFF                                       808182
043300991108     C                   SETOFF                                       83
043400970526     C*
043500970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
043600021211     C     VIDCO1        IFNE      depco1
043700150210     C     VIDTs2        ORNE      depts2
043800150210     C     VIDco2        ORNE      depco2
043900150219     C     VIDdt1        ORNE      depdt1
044000970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
044100021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
044200970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
044300021211     C                   MOVEL     VIDCO1        depco1                         *DEPOSITI
044400150210     C                   MOVEL     VIDTs2        depts2                         *DEPOSITI
044500150210     C                   MOVEL     VIDco2        depco2                         *DEPOSITI
044600150219     C                   MOVEL     VIDdt1        depdt1                         *DEPOSITI
044700021211     C                   ENDIF
044800021211     C                   ENDIF
044900970526     C*
045000970526     C                   ENDSR
045100970526     C*------------------------------------------------------------------------*
045200021211     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
045300970526     C*------------------------------------------------------------------------*
045400970528     C     SETFIL        BEGSR
045500970526     C*
045600970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
045700021211     C     VIDCO1        IFEQ      *blanks
045800150130     C                   MOVEL     *loval        KEYksu            8
045900991108     C                   ELSE
046000150130     C                   MOVEL     VIDCO1        KEYksu
046100991108     C                   ENDIF
046200021211     C     WTIPRI        IFEQ      'COD'
046300150130     C     KEYcGB        SETLL     azcgb01l                           99
046400021211     C                   ENDIF
046500980107     C*
046600021211     C* LETTURA RECORD
046700021211     C     *IN99         IFEQ      *OFF
046800970526     C                   EXSR      LETFIL
046900021211     C                   ENDIF
047000970526     C*
047100970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
047200021211     C     *IN99         IFEQ      *ON
047300970526     C                   Z-ADD     1             SFLEOF
047400021211     C                   ENDIF
047500970526     C*
047600970526     C                   ENDSR
047700970526     C*------------------------------------------------------------------------*
047800970526     C* LETFIL - LETTURA RECORD
047900970526     C*------------------------------------------------------------------------*
048000970526     C     LETFIL        BEGSR
048100970526     C*
048200970526     C* LEGGE FINO A:
048300970526     C*     - FINE FILE
048400970526     C*     - TROVATO RECORD VALIDO
048500970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
048600021211     C     *IN99         DOWEQ     *OFF
048700970526     C     WRECOK        ANDEQ     'N'
048800970526     C*
048900970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
049000021211     C     WTIPRI        IFEQ      'COD'                                        *PER COD. ARTICOLO
049100150130     C                   READ      azcgb01l                               99
049200021211     C                   ENDIF
049300970528     C*
049400970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
049500021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
049600970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
049700021211     C                   ELSE
049800970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
049900021211     C                   ENDIF
050000021211     C                   ENDDO
050100970526     C*
050200970526     C                   ENDSR
050300970526     C*------------------------------------------------------------------------*
050400970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
050500970526     C*------------------------------------------------------------------------*
050600970526     C     CHKREC        BEGSR
050700970526     C*
050800970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
050900021211     C* FILTRA SE RICHIESTO UN TIPO ARTICOLO
051000150210     C     VIDTs2        ifeq      *blanks
051100021211     C                   else
051200150210     C     cgbTsc        ifne      VIDTs2
051300021211     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
051400021211     C                   endif
051500021211     C                   endif
051600150210     C     VIDco2        ifeq      *blanks
051700150210     C                   else
051800150219     C     '?'           SCAN      VIDCO2                                 30
051900150219     c                   if        *in30
052000150219     C* RICHIESTO ELENCO ARTICOLI
052100150219     C                   MOVEL     KPJBU         SAVKPJBU
052200150219     C                   MOVEL(P)  'P01'         KPJBU
052300150219     C                   CALL      'TISIG1R'
052400150219     C                   PARM                    KPJBA
052500150219     C                   EVAL      VIDCO2 = %subst(KPJBU:4:15)
052600150219     C                   MOVEL     SAVKPJBU      KPJBU
052700150219     c                   endif
052800150210     C     cgbcod1       ifne      VIDco2
052900150210     C     cgbcod2       andne     VIDco2
053000150210     C     cgbcod3       andne     VIDco2
053100150210     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
053200150210     C                   endif
053300150210     C                   endif
053400991108     C*
053500150219     C     VIDdt1        ifeq      0
053600150219     C                   else
053700150219     c                   if        viddt1g <> g08inv
053800150219     C                   Z-ADD     viddt1        g08dat
053900150219     C                   Z-ADD     *ZEROS        g08inv
054000150219     C                   MOVEL     '0'           G08ERR
054100150219     C                   CALL      'XSRDA8'
054200150219     C                   PARM                    WLBDA8
054300150219     C                   Z-ADD     G08dat        viddt1
054400150219     c                   move      g08inv        viddt1g           8 0
054500150219     c                   endif
054600150219     C     cgbdti        ifgt      VIDdt1g
054700150219     C     cgbdtf        orlt      VIDdt1g
054800150219     C                   movel     'N'           WRECOK                         *RECORD NON VALIDO
054900150219     C                   endif
055000150219     C                   endif
055100970526     C                   ENDSR
055200970526     C*------------------------------------------------------------------------*
055300000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
055400970526     C*------------------------------------------------------------------------*
055500970526     C     IRESFL        BEGSR
055600970526     C*
055700000407     C                   MOVEL     *blanks       OPZKEY
055800150130     C                   MOVEL     cgbksu        VIDksu
055900150130     C                   MOVEL     cgbTsc        VIDtsc
056000150202     C                   clear                   VIDksud
056100150202      * cerco i dati anagrafici
056200150202     c                   clear                   tibs69ds
056300150202     c                   move      vidksu        i69kac
056400150202     c                   call      'TIBS69R'
056500150202     c                   parm                    tibs69ds
056600150202     c                   parm                    ds_cnaco
056700150202     c                   parm                    ds_cnind
056800150202     c                   parm                    ds_cnclp
056900150202     c                   parm                    ds_fncls
057000150202      * ragione sociale
057100150202     c                   eval      vidksud = acorag
057200150210     C* date
057300150210     c                   if        cgbdti > 0
057400150210     C                   Z-ADD     cgbdti        g08inv
057500150210     C                   Z-ADD     *ZEROS        g08dat
057600150210     C                   MOVEL     '3'           G08ERR
057700150210     C                   CALL      'XSRDA8'
057800150210     C                   PARM                    WLBDA8
057900150210     C                   Z-ADD     G08dat        viddti
058000150302     C                   Z-ADD     G08inv        viddtig
058100150210     c                   endif
058200150210     c                   if        cgbdtf > 0
058300150210     C                   Z-ADD     cgbdtf        g08inv
058400150210     C                   Z-ADD     *ZEROS        g08dat
058500150210     C                   MOVEL     '3'           G08ERR
058600150210     C                   CALL      'XSRDA8'
058700150210     C                   PARM                    WLBDA8
058800150210     C                   Z-ADD     G08dat        viddtf
058900150210     c                   endif
059000150219     C                   Z-ADD     0             viddte
059100150210     c                   if        cgbdte > 0
059200150210     C                   Z-ADD     cgbdte        g08inv
059300150210     C                   Z-ADD     *ZEROS        g08dat
059400150210     C                   MOVEL     '3'           G08ERR
059500150210     C                   CALL      'XSRDA8'
059600150210     C                   PARM                    WLBDA8
059700150210     C                   Z-ADD     G08dat        viddte
059800150210     c                   endif
059900150210     c                   clear                   vidfig
060000150210     c     vidksu        setll     azfgb01l
060100150210     c                   do        *hival
060200150210     c     vidksu        reade     azfgb01l
060300150210     c                   if        %eof(azfgb01l)
060400150210     c                   leave
060500150210     c                   endif
060600150226     c                   if        fgbksc = vidksu or
060700150226     c                             fgbann = 'A'
060800150210     c                   iter
060900150210     c                   endif
061000150210     c                   move      '*'           vidfig
061100150210     c                   leave
061200150210     c                   enddo
061300970526     C                   ENDSR
061400970526     C*------------------------------------------------------------------------*
061500991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
061600970526     C*------------------------------------------------------------------------*
061700970526     C     CHKPAR        BEGSR
061800970528     C*
061900021211     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (COD)
062000021211     C                   MOVEL     'COD'         WTIPRI            3
062100000407     C*
062200970526     C                   ENDSR
062300970526     C*------------------------------------------------------------------------*
062400970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
062500970526     C*------------------------------------------------------------------------*
062600970526     C     CHKFOP        BEGSR
062700991112     C*
062800991112     C* OPZIONI
062900970626     C*
063000000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
063100021211     C     DEPOP0        IFEQ      'P01'
063200000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
063300000420     C                   MOVEL     'S'           OPDATT
063400000420     C                   MOVEL     OPDRIG        OPD(1)
063500150225     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
063600150225     C                   MOVEL     'N'           OPDATT
063700150225     C                   MOVEL     OPDRIG        OPD(5)
063800150225     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
063900150225     C                   MOVEL     'N'           OPDATT
064000150225     C                   MOVEL     OPDRIG        OPD(2)
064100150225     C                   MOVEL     OPD(3)        OPDRIG                         *NO MODIFICA
064200150225     C                   MOVEL     'N'           OPDATT
064300150225     C                   MOVEL     OPDRIG        OPD(3)
064400150225     C*                  MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
064500150225     C*                  MOVEL     'N'           OPDATT
064600150225     C*                  MOVEL     OPDRIG        OPD(4)
064700150225     C                   MOVEL     OPD(6)        OPDRIG                         *NO CANCELLAZIONE
064800150302     C                   MOVEL     'S'           OPDATT
064900150225     C                   MOVEL     OPDRIG        OPD(6)
065000150225     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
065100150302     C                   MOVEL     'N'           fudATT
065200150225     C                   MOVEL     FUDRIG        FUD(6)
065300150212     C                   endif
065400000420     C* OPZIONI
065500150212     C     DEPOP0        IFEQ      'P05'
065600970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
065700970626     C                   MOVEL     'S'           OPDATT
065800970626     C                   MOVEL     OPDRIG        OPD(5)
065900150212     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
066000150212     C                   MOVEL     'N'           OPDATT
066100150212     C                   MOVEL     OPDRIG        OPD(2)
066200150212     C                   MOVEL     OPD(3)        OPDRIG                         *NO MODIFICA
066300150212     C                   MOVEL     'N'           OPDATT
066400150212     C                   MOVEL     OPDRIG        OPD(3)
066500150225     C*                  MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
066600150225     C*                  MOVEL     'N'           OPDATT
066700150225     C*                  MOVEL     OPDRIG        OPD(4)
066800150212     C                   MOVEL     OPD(6)        OPDRIG                         *NO CANCELLAZIONE
066900150212     C                   MOVEL     'N'           OPDATT
067000150212     C                   MOVEL     OPDRIG        OPD(6)
067100150212     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
067200150212     C                   MOVEL     'N'           fudATT
067300150212     C                   MOVEL     FUDRIG        FUD(6)
067400021211     C                   ENDIF
067500970626     C*
067600000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
067700021211     C     DEPOP0        IFEQ      'P02'
067800000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
067900970626     C                   MOVEL     'S'           OPDATT
068000000407     C                   MOVEL     OPDRIG        OPD(2)
068100021211     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
068200021211     C                   MOVEL     'S'           OPDATT
068300021211     C                   MOVEL     OPDRIG        OPD(3)
068400150225     C*                  MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
068500150225     C*                  MOVEL     'S'           OPDATT
068600150225     C*                  MOVEL     OPDRIG        OPD(4)
068700991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
068800991112     C                   MOVEL     'S'           OPDATT
068900991112     C                   MOVEL     OPDRIG        OPD(5)
069000021211     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
069100000407     C                   MOVEL     'S'           FUDATT
069200000407     C                   MOVEL     FUDRIG        FUD(6)
069300021211     C                   ENDIF
069400970626     C*
069500970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
069600150130     C*m   DUTLPO        IFNE      'S'                                          *FILIALE
069700150130     C*m                 MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
069800150130     C*m                 MOVEL     'N'           OPDATT
069900150130     C*m                 MOVEL     OPDRIG        OPD(2)
070000150130     C*m                 MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
070100150130     C*m                 MOVEL     'N'           OPDATT
070200150130     C*m                 MOVEL     OPDRIG        OPD(4)
070300150130     C*m                 MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
070400150130     C*m                 MOVEL     'N'           OPDATT
070500150130     C*m                 MOVEL     FUDRIG        FUD(6)
070600150130     C*m                 ENDIF
070700970526     C*
070800970526     C* GESTIONE OPZIONI A VIDEO
070900970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
071000970526     C                   EXSR      OPZGES
071100970526     C*
071200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
071300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
071400970526     C                   EXSR      FUNGES
071500970526     C*
071600970526     C                   ENDSR
071700020917     C*--------------------------------------------------------------------------------------------*
071800020917     C* REPERISCE I DATI UTENTE
071900020917     C*--------------------------------------------------------------------------------------------*
072000020917     C     REPDATIUTE    BEGSR
072100020917     C*
072200020917     C* INIZIALIZZA VARIABILI DI WRK
072300020917     C                   CLEAR                   TIBS34DS
072400020917     C                   CLEAR                   AZUTEDS
072500020917     C                   CLEAR                   DDATIUTE
072600150212     C                   CLEAR                   dute01
072700020917     C*
072800020917     C     *DTAARA       DEFINE    §azute        azuteds
072900020917     C     *DTAARA       DEFINE    §datiute      ddatiute
073000020917     C                   IN(E)     *DTAARA
073100021211     C                   IF        %Error
073200021211     C                   EVAL      I34Tla = 'L'
073300020917     C                   CALL      'TIBS34R'
073400020917     C                   PARM                    Tibs34Ds
073500020917     C                   IN        *DTAARA
073600021211     C                   ENDIF
073700150212
073800150212     c                   Clear                   wabi
073900150212     c                   Clear                   dLat
074000150212
074100150212      * Verifica errori e autorità profilo
074200150212s   1c                   Select
074300150212      * se ho errori nei dati utente esco dal pgm
074400150212w   1c                   When      DutErr = 'E'
074500150212     c                   seton                                        lr
074600150212     c                   return
074700150212      * se non c'è l'abilitazione
074800150212      * --> se 1° livello, abilitazioni al terminal
074900150212      *     se 2° livello, abilitazioni al punto operativo
075000150212w   1c                   When      UteAut = *Blanks
075100150212if  2c                   If        DutLpo = '1'
075200150212     c                   Eval      wabi   = 'TP'
075300150212e   2c                   EndIf
075400150212if  2c                   If        DutLpo = '2'
075500150212     c                   Eval      wabi   = 'PO'
075600150212e   2c                   EndIf
075700150212if  2c                   If        DutLpo = 'S'
075800150212     c                   Eval      wabi   = 'AZ'
075900150212e   2c                   EndIf
076000150212      * carica le abilitazioni del profilo
076100150212x   1c                   Other
076200150212     c                   Movel     UteFaf        Dute01
076300150212     c                   If        §Utecgb <> *Blanks
076400150212     c                   Eval      wabi = §Utecgb
076500150212     c                   Else
076600150212     c                   Eval      wabi = UteAut
076700150212     c                   EndIf
076800150212e   1c                   EndSl
076900150212      * controllo se ok l'abilitazione dell'utente
077000150212     c                   Clear                   Tibs02ds
077100150212     c                   Eval      T02Mod = 'C'
077200150212     c                   Eval      T02Sif = knsif
077300150212     c                   Eval      T02Cod = 'LAT'
077400150212     c                   Movel(p)  wabi          T02Ke1
077500150212     c                   Call      'TIBS02R'
077600150212     c                   Parm                    kpjba
077700150212     c                   Parm                    Tibs02ds
077800150212if  1c                   If        T02Err = *Blanks
077900150212     c                   Eval      dLat = T02Uni
078000150212e   1c                   EndIf
078100150212      * errore o non abilitato (impossibile xchè controllato nel filtro)
078200150212if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
078300150212     C                   MOVEL     'P05'         DEPOP0
078400150212   x1c                   Else
078500150212      * Reperimento dei P.O. gestibili dall'utente
078600150212     c                   clear                   TRUL31DS
078700150212     c                   clear                   TRUL31DS2
078800150212     c                   eval      I31abi = wabi
078900150212     c                   eval      I31cdi = DUTdis
079000150212     c                   eval      I31car = DUTare
079100150212     c                   eval      I31cpo = DUTpou
079200150212     c                   call      'TRUL31R'
079300150212     c                   parm                    KPJBA
079400150212     c                   parm                    TRUL31DS
079500150212     c                   parm                    TRUL31DS2
079600150212if  2c                   if        O31pog > *zeros
079700150212     c                   movea     O31pog        $pog
079800150212     c                   movea     O31arg        $arg
079900150212     c                   movea     O31dig        $dig
080000150212x   2c                   else
080100150212     c* impossibile perchè controllato nel filtrp
080200150212     c                   seton                                        lr
080300150212     c                   return
080400150212e   2c                   endif
080500150212    1c                   endif
080600020917     C*
080700020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
080800020917     C                   MOVEL(P)  RSUT          DSFIRS           20
080900020917     C*
081000020917     C                   ENDSR
081100970526     C*------------------------------------------------------------------------*
081200970526     C* *INZSR - OPERAZIONI INIZIALI
081300970526     C*------------------------------------------------------------------------*
081400970526     C     *INZSR        BEGSR
081500970526     C*---
081600970526     C* RICEVIMENTO PARAMETRI
081700970526     C*---
081800970526     C     *ENTRY        PLIST
081900970612     C                   PARM                    KPJBA
082000021211     C                   MOVEL     KPJBU         PARAM
082100150202     c                   if        pop1 <> *blank
082200021211     C                   MOVEL     POP1          DEPOP0            3
082300150202     c                   else
082400150202     C                   MOVEL     'P02'         DEPOP0
082500150202     c                   endif
082600970526     C*
082700970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
082800970526     C     XMSGPA        PLIST
082900970526     C                   PARM                    DSMSMS
083000970526     C                   PARM                    DSMSPG
083100970526     C                   PARM                    DSMSKE
083200970526     C                   PARM                    DSMSER
083300970526     C*
083400970526     C* IMPOSTA LA DS -TASTI PREMUTI-
083500970526     C                   CALL      'XKEY'
083600970526     C                   PARM                    DSKEY
083700970526     C*---
083800970526     C* VARIABILI NON RIFERITE AL DATA BASE
083900970526     C*---
084000970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
084100970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
084200970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
084300970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
084400970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
084500970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
084600970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
084700970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
084800970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
084900970526     C*---
085000021211     C     *LIKE         DEFINE    VIDCO1        depco1
085100150210     C     *LIKE         DEFINE    VIDts2        depts2
085200150210     C     *LIKE         DEFINE    VIDco2        depco2
085300150219     C     *LIKE         DEFINE    VIDdt1        depdt1
085400991108     C*---
085500970526     C* VARIABILI DI CONTROLLO
085600970526     C*---
085700970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
085800970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
085900970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
086000970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
086100970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
086200000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
086300970526     C*
086400970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
086500150130     C                   MOVEL     'TISIG10R'    DSMSPG
086600150130     C                   MOVEL     'TISIG10R'    MSGQUE
086700970526     C                   MOVEL     *BLANKS       MSGKEY
086800970526     C                   MOVEL     *BLANKS       DSMSMS
086900970526     C                   MOVEL     *BLANKS       DSMSKE
087000970526     C                   Z-ADD     *ZEROS        DSMSER
087100970526     C*---
087200970526     C* CHIAVI DI LETTURA
087300970526     C*---
087400150130     C* LETTURA azcgb01l - COMPLETA
087500150130     C     KEYcGB        KLIST
087600150130     C                   KFLD                    KEYksu
087700021211     C* LETTURA azmgb02l - PARZIALE
087800021211     C     KEYMGBP       KLIST
087900150204     C                   KFLD                    KEYcli            1
088000150130     C                   KFLD                    KEYksu            8
088100150210     C                   move      'C'           KEYcli            1
088200991108     C*
088300970527     C* REPERIMENTO DATI UTENTE
088400020917     C                   EXSR      REPDATIUTE
088500970526     C*
088600970526     C* IMPOSTA I PARAMETRI DI INGRESSO
088700970526     C                   EXSR      CHKPAR
088800991115     C*
088900970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
089000970526     C                   EXSR      CHKFOP
089100970526     C*
089200970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
089300021211     C     DEPOP0        IFEQ      'P01'
089400970530     C                   MOVEL     DEC(3)        VIDING
089500021211     C                   ENDIF
089600021211     C     DEPOP0        IFEQ      'P02'
089700970526     C                   MOVEL     DEC(1)        VIDING
089800021211     C                   ENDIF
089900021211     C     DEPOP0        IFEQ      'P05'
090000970526     C                   MOVEL     DEC(2)        VIDING
090100021211     C                   ENDIF
090200970526     C*
090300970526     C                   ENDSR
090400970526     C*------------------------------------------------------------------------*
090500970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
090600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
090700970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
090800970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
090900970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
091000970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
091200970526     C*------------------------------------------------------------------------*
091300970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
091400970526     C*------------------------------------------------------------------------*
091500970526     C     POSSFL        BEGSR
091600970526     C*
091700970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
091800970526     C                   EXSR      CLRSFL
091900970526     C*
092000970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
092100970526     C                   EXSR      SETFIL
092200970526     C*
092300970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
092400970526     C     SFLEOF        IFEQ      0
092500970526     C                   EXSR      CARSFL
092600970526     C                   END
092700970526     C*
092800970526     C                   ENDSR
092900970526     C*------------------------------------------------------------------------*
093000970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
093100970526     C*------------------------------------------------------------------------*
093200970526     C     ENTATT        BEGSR
093300970526     C*
093400970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
093500970526     C                   Z-ADD     0             OPZOK             1 0
093600970526     C*
093700970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
093800970526     C                   Z-ADD     0             SFLVAR            5 0
093900970526     C*
094000970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
094100970526     C                   Z-ADD     0             SFLERR
094200970526     C*
094300970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
094400970526     C                   Z-ADD     SFLULT        WFLULT
094500970526     C*
094600970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
094700021211     C                   READC     SIG1D05                                98
094800970526     C     *IN98         DOWEQ     '0'
094900970526     C*
095000970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
095100970526     C                   ADD       1             SFLVAR
095200970526     C*
095300970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
095400970526     C     OPZOK         IFEQ      0
095500970526     C*
095600970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
095700970526     C     OPZKEY        IFNE      *BLANKS
095800970526     C                   EXSR      OPZCON
095900970526     C                   END
096000970526     C*
096100970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
096200970526     C*            2. PULIZIA SCELTA IN SFL
096300970526     C     OPZOK         IFEQ      0
096400970526     C                   MOVEL     '0'           *IN41
096500970526     C                   MOVEL     *BLANKS       OPZKEY
096600970526     C                   ELSE
096700970526     C*
096800970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
096900970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
097000970526     C                   MOVEL     '1'           *IN41
097100970526     C                   Z-ADD     SFLULT        SFLERR
097200970526     C                   END
097300970526     C                   ELSE
097400970526     C*
097500970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
097600970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
097700970526     C                   MOVEL     '1'           *IN41
097800970526     C                   END                                                    OPZOK=0
097900970526     C*
098000970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
098100970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
098200970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
098300970526     C     OPZOK         IFNE      0
098400970526     C                   Z-ADD     SFLERR        SFLNBR
098500970526     C                   ELSE
098600970526     C                   Z-ADD     SFLULT        SFLNBR
098700970526     C                   END
098800970526     C*
098900970526     C* AGGIORNO RECORD SFL
099000021211     C                   UPDATE    SIG1D05
099100970526     C*
099200021211     C                   READC     SIG1D05                                98
099300970526     C                   END                                                    DO WHILE
099400970526     C*
099500970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
099600970526     C                   Z-ADD     WFLULT        SFLULT
099700970526     C*
099800970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
099900970526     C     SFLVAR        IFEQ      0
100000970526     C                   END
100100970526     C*
100200991115     C     OPZKEY        IFNE      ' 5'
100300991115     C                   EXSR      F05GES
100400991115     C                   ENDIF
100500991112     C*
100600970526     C                   ENDSR
100700970526     C*------------------------------------------------------------------------*
100800970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
100900970526     C*------------------------------------------------------------------------*
101000970526     C     CLRSFL        BEGSR
101100970526     C*
101200970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
101300970526     C* (0=NON FINE FILE  1=FINE FILE)
101400970526     C                   Z-ADD     0             SFLEOF
101500970526     C*
101600970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
101700970526     C                   Z-ADD     0             SFLULT
101800970526     C*
101900970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
102000970526     C                   MOVEL     '1'           *IN40
102100021211     C                   WRITE     SIG1D06
102200970526     C                   MOVEL     '0'           *IN40
102300970526     C*
102400970526     C                   ENDSR
102500970526     C*------------------------------------------------------------------------*
102600970526     C* CARSFL - CARICAMENTO SFL
102700970526     C*------------------------------------------------------------------------*
102800970526     C     CARSFL        BEGSR
102900970526     C*
103000970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
103100970526     C     SFLULT        IFEQ      0
103200970526     C                   EXSR      SETFIL
103300970526     C                   END
103400970526     C*
103500970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
103600970526     C                   Z-ADD     SFLULT        WFLULT
103700970526     C*
103800970526     C* CICLO:
103900970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
104000970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
104100970526     C                   Z-ADD     1             SFLRIG
104200970526     C     SFLRIG        DOWLE     SFLPAG
104300970526     C     SFLEOF        ANDEQ     0
104400970526     C*
104500970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
104600991116     C                   MOVEL     *BLANKS       OPZKEY
104700970526     C*
104800970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
104900970526     C                   EXSR      IRESFL
105000970526     C*
105100970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
105200970526     C                   ADD       1             SFLULT
105300970526     C*
105400970526     C* SCRITTURA RECORD SFL
105500021211     C                   WRITE     SIG1D05
105600970526     C                   ADD       1             SFLRIG
105700970526     C*
105800970526     C* LETTURA FILE DA CARICARE
105900970526     C                   EXSR      LETFIL
106000970526     C                   END                                                    DO WHILE
106100970526     C*
106200970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
106300970526     C     WFLULT        IFNE      SFLULT
106400970526     C                   Z-ADD     WFLULT        SFLNBR
106500970526     C                   ADD       1             SFLNBR
106600970526     C                   END
106700970526     C*
106800970526     C                   ENDSR
106900970526     C*------------------------------------------------------------------------*
107000970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
107100970526     C*------------------------------------------------------------------------*
107200970526     C     OUTSFL        BEGSR
107300970526     C*
107400970526     C* WRITE RECORD TESTATA VIDEO
107500021211     C                   WRITE     SIG1D01
107600970526     C*
107700970526     C* VISUALIZZAZIONE MESSAGGIO
107800970526     C                   EXSR      SNDMSG
107900970526     C*
108000970526     C* WRITE RECORD OPZIONI VIDEO
108100021211     C                   WRITE     SIG1D02
108200970526     C*
108300970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
108400021211     C                   WRITE     SIG1D03
108500970526     C*
108600970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
108700021211     C                   WRITE     SIG1D04
108800970526     C*
108900970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
109000970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
109100970526     C     SFLEOF        IFEQ      0
109200970526     C                   MOVEL     '0'           *IN42
109300970526     C                   ELSE
109400970526     C*
109500970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
109600970526     C                   MOVEL     '1'           *IN42
109700970526     C                   END
109800970526     C*
109900970526     C* WRITE RECORD TASTI FUNZIONALI
110000021211     C                   WRITE     SIG1D09
110100970526     C*
110200970526     C* CASO DI SFL VUOTO
110300970526     C     SFLULT        IFLE      0
110400970526     C*
110500970526     C*    - WRITE RECORD VUOTO
110600021211     C                   WRITE     SIG1D0A
110700021211     C                   READ      SIG1D0A                                99
110800970526     C*
110900970526     C                   ELSE
111000970526     C*
111100970526     C* CASO DI SFL CON RECORDS
111200970526     C*
111300970526     C*    - WRITE RECORD CONTROLLO SFL
111400021211     C                   WRITE     SIG1D06
111500021211     C                   READ      SIG1D06                                99
111600970526     C                   END
111700970526     C*
111800970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
111900021211     C                   READ      SIG1D03                                99
112000970526     C*
112100970526     C                   ENDSR
112200970526     C*------------------------------------------------------------------------*
112300970526     C* SNDMSG - MANDA MESSAGGIO
112400970526     C*------------------------------------------------------------------------*
112500970526     C     SNDMSG        BEGSR
112600970526     C*
112700970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
112800970526     C                   MOVEL     '1'           *IN40
112900021211     C                   WRITE     SIG1DMC
113000970526     C                   MOVEL     '0'           *IN40
113100970526     C*
113200970526     C* VISUALIZZAZIONE MESSAGGIO
113300970526     C                   Z-ADD     0             MSGULT
113400970526     C                   CALL      'XMSG'        XMSGPA
113500970526     C     DSMSER        IFEQ      0
113600970526     C                   MOVEL     DSMSKE        MSGKEY
113700970526     C                   END
113800970526     C                   Z-ADD     1             MSGULT
113900021211     C                   WRITE     SIG1DMS                                      SFL MESS
114000021211     C                   WRITE     SIG1DMC                                      CONTR MESS
114100970526     C*
114200970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
114300970526     C                   MOVEL     *BLANKS       DSMSMS
114400970526     C*
114500970526     C                   ENDSR
114600970526     C*------------------------------------------------------------------------*
114700970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
114800970526     C*------------------------------------------------------------------------*
114900970526     C     FUNCON        BEGSR
115000970526     C*
115100970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
115200970526     C                   EXSR      FUNVAR
115300970526     C*
115400970526     C* PULISCO CAMPI DI LAVORO
115500970526     C                   Z-ADD     0             I                 5 0
115600970526     C                   Z-ADD     0             W                 5 0
115700970526     C*
115800970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
115900970526     C     FUNKEY        IFNE      *BLANKS
116000970526     C     1             DO        24            I
116100970526     C                   MOVEL     FUD(I)        FUDRIG
116200970526     C*
116300970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
116400970526     C     FUNKEY        IFEQ      FUDFUN
116500970526     C     FUDATT        ANDEQ     'S'
116600970526     C                   Z-ADD     I             W
116700970526     C                   END
116800970526     C                   END                                                    DO WHILE
116900970526     C*
117000970526     C* FUNZIONE NON ATTIVA (ERRORE)
117100970526     C     W             IFEQ      0
117200970526     C*
117300970526     C* EMISSIONE MESSAGGIO DI ERRORE
117400970526     C                   MOVEL     MSG(2)        DSMSMS
117500970526     C                   END                                                    W=0
117600970526     C                   END                                                    FUNKEY NE *BLANKS
117700970526     C*
117800970526     C* FUNZIONE ATTIVA (PROCEDI ...)
117900970526     C     W             IFNE      0
118000970526     C     FUNKEY        OREQ      *BLANKS
118100970526     C*
118200970526     C     FUNKEY        IFEQ      *BLANKS
118300970526     C                   EXSR      ENTGES
118400970526     C                   END
118500970526     C     FUNKEY        IFEQ      '03'
118600970526     C                   EXSR      F03GES
118700970526     C                   END
118800970526     C     FUNKEY        IFEQ      '05'
118900970526     C                   EXSR      F05GES
119000970526     C                   END
119100150204     C     FUNKEY        IFEQ      '06'
119200150204     C                   EXSR      F06GES
119300970526     C                   END
119400970526     C     FUNKEY        IFEQ      '12'
119500970526     C                   EXSR      F12GES
119600970526     C                   END
119700970528     C     FUNKEY        IFEQ      '21'
119800970528     C                   EXSR      F21GES
119900970528     C                   END
120000970528     C     FUNKEY        IFEQ      '22'
120100970528     C                   EXSR      F22GES
120200970528     C                   END
120300970526     C     FUNKEY        IFEQ      '23'
120400970526     C                   EXSR      F23GES
120500970526     C                   END
120600970526     C     FUNKEY        IFEQ      '24'
120700970526     C                   EXSR      F24GES
120800970526     C                   END
120900970526     C     FUNKEY        IFEQ      'RU'
121000970526     C                   EXSR      RUPGES
121100970526     C                   END
121200970526     C*
121300970526     C                   END
121400970526     C*
121500970526     C                   ENDSR
121600970526     C*------------------------------------------------------------------------*
121700970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
121800970526     C*------------------------------------------------------------------------*
121900970526     C     FUNVAR        BEGSR
122000970526     C*
122100970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
122200970526     C     KEY           IFEQ      ENTER
122300970526     C                   MOVEL     *BLANKS       FUNKEY
122400970526     C                   END
122500970526     C     KEY           IFEQ      F1
122600970526     C                   MOVEL     '01'          FUNKEY
122700970526     C                   END
122800970526     C     KEY           IFEQ      F2
122900970526     C                   MOVEL     '02'          FUNKEY
123000970526     C                   END
123100970526     C     KEY           IFEQ      F3
123200970526     C                   MOVEL     '03'          FUNKEY
123300970526     C                   END
123400970526     C     KEY           IFEQ      F4
123500970526     C                   MOVEL     '04'          FUNKEY
123600970526     C                   END
123700970526     C     KEY           IFEQ      F5
123800970526     C                   MOVEL     '05'          FUNKEY
123900970526     C                   END
124000970526     C     KEY           IFEQ      F6
124100970526     C                   MOVEL     '06'          FUNKEY
124200970526     C                   END
124300970526     C     KEY           IFEQ      F7
124400970526     C                   MOVEL     '07'          FUNKEY
124500970526     C                   END
124600970526     C     KEY           IFEQ      F8
124700970526     C                   MOVEL     '08'          FUNKEY
124800970526     C                   END
124900970526     C     KEY           IFEQ      F9
125000970526     C                   MOVEL     '09'          FUNKEY
125100970526     C                   END
125200970526     C     KEY           IFEQ      F10
125300970526     C                   MOVEL     '10'          FUNKEY
125400970526     C                   END
125500970526     C     KEY           IFEQ      F11
125600970526     C                   MOVEL     '11'          FUNKEY
125700970526     C                   END
125800970526     C     KEY           IFEQ      F12
125900970526     C                   MOVEL     '12'          FUNKEY
126000970526     C                   END
126100970526     C     KEY           IFEQ      F13
126200970526     C                   MOVEL     '13'          FUNKEY
126300970526     C                   END
126400970526     C     KEY           IFEQ      F14
126500970526     C                   MOVEL     '14'          FUNKEY
126600970526     C                   END
126700970526     C     KEY           IFEQ      F15
126800970526     C                   MOVEL     '15'          FUNKEY
126900970526     C                   END
127000970526     C     KEY           IFEQ      F16
127100970526     C                   MOVEL     '16'          FUNKEY
127200970526     C                   END
127300970526     C     KEY           IFEQ      F17
127400970526     C                   MOVEL     '17'          FUNKEY
127500970526     C                   END
127600970526     C     KEY           IFEQ      F18
127700970526     C                   MOVEL     '18'          FUNKEY
127800970526     C                   END
127900970526     C     KEY           IFEQ      F19
128000970526     C                   MOVEL     '19'          FUNKEY
128100970526     C                   END
128200970526     C     KEY           IFEQ      F20
128300970526     C                   MOVEL     '20'          FUNKEY
128400970526     C                   END
128500970526     C     KEY           IFEQ      F21
128600970526     C                   MOVEL     '21'          FUNKEY
128700970526     C                   END
128800970526     C     KEY           IFEQ      F22
128900970526     C                   MOVEL     '22'          FUNKEY
129000970526     C                   END
129100970526     C     KEY           IFEQ      F23
129200970526     C                   MOVEL     '23'          FUNKEY
129300970526     C                   END
129400970526     C     KEY           IFEQ      F24
129500970526     C                   MOVEL     '24'          FUNKEY
129600970526     C                   END
129700970526     C     KEY           IFEQ      ROLLUP
129800970526     C                   MOVEL     'RU'          FUNKEY
129900970526     C                   END
130000970526     C     KEY           IFEQ      HELP
130100970526     C                   MOVEL     'HP'          FUNKEY
130200970526     C                   END
130300970526     C*
130400970526     C                   ENDSR
130500970526     C*------------------------------------------------------------------------*
130600970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
130700970526     C*------------------------------------------------------------------------*
130800970526     C     FUNGES        BEGSR
130900970526     C*
131000970526     C* PULISCO CAMPI DI LAVORO
131100970526     C                   Z-ADD     0             W                 5 0
131200970526     C*
131300970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
131400970526     C* E CARICO TABELLA FUNZIONI ATTIVE
131500970526     C     1             DO        24            I
131600970526     C                   MOVEL     FUD(I)        FUDRIG
131700970526     C*
131800970526     C* PULIZIA FUNZIONI PRECEDENTI
131900970526     C                   MOVEL     *BLANKS       FUV(I)
132000970526     C*
132100970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
132200970526     C* (SALTA F23 E F24)
132300970526     C     FUDATT        IFEQ      'S'
132400970526     C     FUDFUN        ANDNE     '23'
132500970526     C     FUDFUN        ANDNE     '24'
132600970526     C                   ADD       1             W
132700970526     C                   MOVEL     FUDDES        FUV(W)
132800970526     C                   END
132900970526     C                   END                                                    DO WHILE
133000970526     C*
133100970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
133200970526     C* ALTRIMENTI DISATTIVO F24
133300970526     C     1             DO        24            I
133400970526     C                   MOVEL     FUD(I)        FUDRIG
133500970526     C*
133600970526     C     FUDFUN        IFEQ      '24'
133700970526     C     W             IFGT      6
133800970526     C                   MOVEL     'S'           FUDATT
133900970526     C                   ELSE
134000970526     C                   MOVEL     *BLANKS       FUDATT
134100970526     C                   END                                                    W GT 8
134200970526     C                   MOVEL     FUDRIG        FUD(I)
134300970526     C                   END
134400970526     C                   END                                                    DO WHILE
134500970526     C*
134600970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
134700970526     C     W             DIV       6             I
134800970526     C                   MVR                     Z
134900970526     C*
135000970526     C     Z             IFNE      0
135100970526     C     I             ADD       1             Z
135200970526     C                   ELSE
135300970526     C                   Z-ADD     I             Z
135400970526     C                   END
135500970526     C*
135600970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
135700970526     C                   ADD       1             FUNGRU
135800970526     C     FUNGRU        IFGT      Z
135900970526     C     FUNGRU        ORLT      1
136000970526     C                   Z-ADD     1             FUNGRU
136100970526     C                   END
136200970526     C*
136300970526     C* PRIMO GRUPPO DI 6 FUNZIONI
136400970526     C     FUNGRU        IFEQ      1
136500970526     C                   MOVEL     FUV(1)        FUN001
136600970526     C                   MOVEL     FUV(2)        FUN002
136700970526     C                   MOVEL     FUV(3)        FUN003
136800970526     C                   MOVEL     FUV(4)        FUN004
136900970526     C                   MOVEL     FUV(5)        FUN005
137000970526     C                   MOVEL     FUV(6)        FUN006
137100970526     C                   END
137200970526     C*
137300970526     C* SECONDO GRUPPO DI 6 FUNZIONI
137400970526     C     FUNGRU        IFEQ      2
137500970526     C                   MOVEL     FUV(7)        FUN001
137600970526     C                   MOVEL     FUV(8)        FUN002
137700970526     C                   MOVEL     FUV(9)        FUN003
137800970526     C                   MOVEL     FUV(10)       FUN004
137900970526     C                   MOVEL     FUV(11)       FUN005
138000970526     C                   MOVEL     FUV(12)       FUN006
138100970526     C                   END
138200970526     C*
138300970526     C* TERZO GRUPPO DI 6 FUNZIONI
138400970526     C     FUNGRU        IFEQ      3
138500970526     C                   MOVEL     FUV(13)       FUN001
138600970526     C                   MOVEL     FUV(14)       FUN002
138700970526     C                   MOVEL     FUV(15)       FUN003
138800970526     C                   MOVEL     FUV(16)       FUN004
138900970526     C                   MOVEL     FUV(17)       FUN005
139000970526     C                   MOVEL     FUV(18)       FUN006
139100970526     C                   END
139200970526     C*
139300970526     C* QUARTO GRUPPO DI 6 FUNZIONI
139400970526     C     FUNGRU        IFEQ      4
139500970526     C                   MOVEL     FUV(19)       FUN001
139600970526     C                   MOVEL     FUV(20)       FUN002
139700970526     C                   MOVEL     FUV(21)       FUN003
139800970526     C                   MOVEL     FUV(22)       FUN004
139900970526     C                   MOVEL     *BLANKS       FUN005
140000970526     C                   MOVEL     *BLANKS       FUN006
140100970526     C                   END
140200970526     C*
140300970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
140400970526     C                   MOVEL     *BLANKS       FUN007
140500970526     C                   MOVEL     *BLANKS       FUN008
140600970526     C*
140700970526     C     1             DO        24            I
140800970526     C                   MOVEL     FUD(I)        FUDRIG
140900970526     C     FUDFUN        IFEQ      '23'
141000970526     C     FUDATT        ANDEQ     'S'
141100970526     C                   MOVEL     FUDDES        FUN007
141200970526     C                   END
141300970526     C     FUDFUN        IFEQ      '24'
141400970526     C     FUDATT        ANDEQ     'S'
141500970526     C                   MOVEL     FUDDES        FUN008
141600970526     C                   END
141700970526     C                   END                                                    DO
141800970526     C*
141900970526     C                   ENDSR
142000970526     C*------------------------------------------------------------------------*
142100970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
142200970526     C*------------------------------------------------------------------------*
142300970526     C     OPZGES        BEGSR
142400970526     C*
142500970526     C* PULISCO CAMPI DI LAVORO
142600970526     C                   Z-ADD     0             W                 5 0
142700970526     C*
142800970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
142900970526     C* E CARICO TABELLA OPZIONI ATTIVE
143000991115     C     1             DO        24            I
143100970526     C                   MOVEL     OPD(I)        OPDRIG
143200970526     C*
143300970526     C* PULIZIA OPZIONI PRECEDENTI
143400970526     C                   MOVEL     *BLANKS       OPV(I)
143500970526     C*
143600970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
143700970526     C     OPDATT        IFEQ      'S'
143800970526     C                   ADD       1             W
143900970526     C                   MOVEL     OPDDES        OPV(W)
144000970526     C                   END
144100970526     C                   END                                                    DO WHILE
144200970526     C*
144300970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
144400970526     C* ALTRIMENTI DISATTIVO F23
144500991115     C     1             DO        24            I
144600970526     C                   MOVEL     FUD(I)        FUDRIG
144700970526     C*
144800970526     C     FUDFUN        IFEQ      '23'
144900970526     C     W             IFGT      8
145000970526     C                   MOVEL     'S'           FUDATT
145100970526     C                   ELSE
145200970526     C                   MOVEL     *BLANKS       FUDATT
145300970526     C                   END                                                    W GT 8
145400970526     C                   MOVEL     FUDRIG        FUD(I)
145500970526     C                   END
145600970526     C                   END                                                    DO WHILE
145700970526     C*
145800970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
145900970526     C     W             DIV       8             I
146000970526     C                   MVR                     Z                 5 0
146100970526     C*
146200970526     C     Z             IFNE      0
146300970526     C     I             ADD       1             Z
146400970526     C                   ELSE
146500970526     C                   Z-ADD     I             Z
146600970526     C                   END
146700970526     C*
146800970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
146900970526     C                   ADD       1             OPZGRU
147000970526     C     OPZGRU        IFGT      Z
147100970526     C     OPZGRU        ORLT      1
147200970526     C                   Z-ADD     1             OPZGRU
147300970526     C                   END
147400970526     C*
147500970526     C* PRIMO GRUPPO DI 8 OPZIONI
147600970526     C     OPZGRU        IFEQ      1
147700970526     C                   MOVEL     OPV(1)        OPZ001
147800970526     C                   MOVEL     OPV(2)        OPZ002
147900970526     C                   MOVEL     OPV(3)        OPZ003
148000970526     C                   MOVEL     OPV(4)        OPZ004
148100970526     C                   MOVEL     OPV(5)        OPZ005
148200970526     C                   MOVEL     OPV(6)        OPZ006
148300970526     C                   MOVEL     OPV(7)        OPZ007
148400970526     C                   MOVEL     OPV(8)        OPZ008
148500970526     C                   END
148600970526     C*
148700970526     C* SECONDO GRUPPO DI 8 OPZIONI
148800970526     C     OPZGRU        IFEQ      2
148900970526     C                   MOVEL     OPV(9)        OPZ001
149000970526     C                   MOVEL     OPV(10)       OPZ002
149100970526     C                   MOVEL     OPV(11)       OPZ003
149200970526     C                   MOVEL     OPV(12)       OPZ004
149300970526     C                   MOVEL     OPV(13)       OPZ005
149400970526     C                   MOVEL     OPV(14)       OPZ006
149500970526     C                   MOVEL     OPV(15)       OPZ007
149600970526     C                   MOVEL     OPV(16)       OPZ008
149700970526     C                   END
149800970526     C*
149900970526     C* TERZO GRUPPO DI 8 OPZIONI
150000970526     C     OPZGRU        IFEQ      3
150100970526     C                   MOVEL     OPV(17)       OPZ001
150200970526     C                   MOVEL     OPV(18)       OPZ002
150300970526     C                   MOVEL     OPV(19)       OPZ003
150400970526     C                   MOVEL     OPV(20)       OPZ004
150500970526     C                   MOVEL     OPV(21)       OPZ005
150600970526     C                   MOVEL     OPV(22)       OPZ006
150700970526     C                   MOVEL     OPV(23)       OPZ007
150800970526     C                   MOVEL     OPV(24)       OPZ008
150900970526     C                   END
151000970526     C*
151100970526     C                   ENDSR
151200970526     C*------------------------------------------------------------------------*
151300970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
151400970526     C*------------------------------------------------------------------------*
151500970526     C     OPZCON        BEGSR
151600970526     C*
151700970526     C* PULISCO CAMPI DI LAVORO
151800970526     C                   Z-ADD     0             I                 5 0
151900970526     C                   Z-ADD     0             W                 5 0
152000970526     C*
152100970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
152200970526     C     1             DO        24            I
152300970526     C                   MOVEL     OPD(I)        OPDRIG
152400970526     C*
152500970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
152600970526     C     OPZKEY        IFEQ      OPDOPZ
152700970526     C     OPZKEY        OREQ      OPDOP1
152800970526     C*
152900970526     C     OPDATT        IFEQ      'S'
153000970526     C                   MOVEL     OPDOPZ        OPZKEY
153100970526     C                   Z-ADD     I             W
153200970526     C                   END
153300970526     C                   END
153400970526     C*
153500970526     C                   END                                                    DO WHILE
153600970526     C*
153700970526     C* OPZIONE NON ATTIVA (ERRORE)
153800970526     C     W             IFEQ      0
153900970526     C*
154000970526     C* OPZIONE NON OK
154100970526     C                   Z-ADD     1             OPZOK
154200970526     C*
154300970526     C* EMISSIONE MESSAGGIO DI ERRORE
154400970526     C                   MOVEL     MSG(1)        DSMSMS
154500970526     C                   ELSE
154600970526     C*
154700970526     C* OPZIONE ATTIVA (PROCEDI ...)
154800970526     C*
154900021211     C                   MOVEL     *BLANKS       PMSG
155000000420     C     OPZKEY        IFEQ      ' 1'
155100000420     C                   EXSR      O01GES
155200000420     C                   END
155300000407     C     OPZKEY        IFEQ      ' 2'
155400000407     C                   EXSR      O02GES
155500000407     C                   END
155600021211     C     OPZKEY        IFEQ      ' 3'
155700021211     C                   EXSR      O03GES
155800021211     C                   END
155900991110     C     OPZKEY        IFEQ      ' 4'
156000991110     C                   EXSR      O04GES
156100991014     C                   END
156200970526     C     OPZKEY        IFEQ      ' 5'
156300970526     C                   EXSR      O05GES
156400970526     C                   END
156500150211     C     OPZKEY        IFEQ      ' F'
156600150211     C                   EXSR      O0FGES
156700150211     C                   END
156800030123     C                   MOVEL     PMSG          DSMSMS
156900021211     C                   END                                                    OPZ.NON ATTIVA
157000970526     C*
157100970526     C                   ENDSR
157200970526     C*------------------------------------------------------------------------*
157300970526     C* NOTE COSTRUZIONE SCHIERE
157400970526     C*------------------------------------------------------------------------*
157500970526     C*
157600970526     C* -MSG-MESSAGGI
157700970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
157800970526     C* (NON TOCCARE MSG,1 E MSG,2)
157900970526     C*
158000970526     C* -CMD-COMANDI
158100970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
158200970526     C*
158300970526     C* -OPD-OPZIONI
158400970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
158500970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
158600970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
158700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
158800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
158900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
159000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
159100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
159200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
159300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
159400970526     C*
159500970526     C* -FUD-FUNZIONI
159600970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
159700970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
159800970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
159900970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
160000970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
160100970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
160200970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
160300970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
160400970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
160500970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
160600991108     C*
160700991115     C* -STS-CODICI STATI DEI RECORD DI LOG
160800991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
160900991110     C* ================> IMPORTANTE <=================
161000991115     C* per codici di errore in -STS- indicare in -DES- sempre
161100991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
161200991110     C* ERR
161300991108     C*
161400970526     O*------------------------------------------------------------------------*
161500970526** DEC - DECODIFICHE
161600970526    GESTIONE                                                     1
161700000407 INTERROGAZIONE                                                  2
161800970530     SCELTA                                                      3
1619009705264
1620009705265
162100970526** MSG - MESSAGGI
162200150202TISIG10R- Opzione non gestita.                                                  1
162300150202TISIG10R- Tasto funzionale non gestito.                                         2
162400150202TISIG10R- Utente non abilitato all'inserimento.                                 3
162500150202TISIG10R- Impossibile annullare, presenti movimenti su tale codice articolo.    4
162600150202TISIG10R- 5                                                                     5
162700150202TISIG10R- 6                                                                     6
162800150202TISIG10R- 7                                                                     7
162900150202TISIG10R- 8                                                                     8
163000150202TISIG10R- 9                                                                     9
163100150202TISIG10R- 10                                                                   10
163200150202TISIG10R- 11                                                                   11
163300150202TISIG10R- 12                                                                   12
163400150202TISIG10R- 13                                                                   13
163500150202TISIG10R- 14                                                                   14
163600150202TISIG10R- 15                                                                   15
163700150202TISIG10R- 16                                                                   16
163800150202TISIG10R- 17                                                                   17
163900150202TISIG10R- 18                                                                   18
164000150202TISIG10R- 19                                                                   19
164100150202TISIG10R- 20                                                                   20
164200970528** CMD - COMANDI CL
164300021211
164400021211
164500991110
164600970528
164700970528
164800970526** OPD - OPZIONI
164900150211 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
165000150202 2=Modifica        -2 -     (S/N)=S                        opz. 02¬
165100150202 3=Copia           -3 -     (S/N)=S                        opz. 03¬
165200150225                   -  -     (S/N)=N                        opz. 04¬
165300150202 5=Visualizzazione -5 -     (S/N)=S                        opz. 05¬
165400150211 F=Abbinati        -F -     (S/N)=S                        opz. 06¬
165500021211                   -  -     (S/N)=N                        opz. 07¬
165600991110                   -  -     (S/N)=N                        opz. 08¬
165700991110                   -  -     (S/N)=N                        opz. 09¬
165800991110                   -  -     (S/N)=N                        opz. 10¬
165900000407                   -  -     (S/N)=N                        opz. 11¬
166000970526                   -  -     (S/N)=N                        opz. 12¬
166100970526                   -  -     (S/N)=N                        opz. 13¬
166200000407                   -  -     (S/N)=N                        opz. 14¬
166300000407                   -  -     (S/N)=N                        opz. 15¬
166400970526                   -  -     (S/N)=N                        opz. 16¬
166500970526                   -  -     (S/N)=N                        opz. 17¬
166600970526                   -  -     (S/N)=N                        opz. 18¬
166700970526                   -  -     (S/N)=N                        opz. 19¬
166800970526                   -  -     (S/N)=N                        opz. 20¬
166900970526                   -  -     (S/N)=N                        opz. 21¬
167000970526                   -  -     (S/N)=N                        opz. 22¬
167100970526                   -  -     (S/N)=N                        opz. 23¬
167200000407                   -  -     (S/N)=N                        opz. 25¬
167300970526** FUD - FUNZIONI
167400970526                   -        (S/N)=N                        funz.01¬
167500970526                   -        (S/N)=N                        funz.02¬
167600970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
167700970526                   -        (S/N)=N                        funz.04¬
167800970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
167900150212F06=Inserimento    -        (S/N)=S                        funz.06¬
168000970526                   -        (S/N)=N                        funz.07¬
168100970527                   -        (S/N)=N                        funz.08¬
168200970527                   -        (S/N)=N                        funz.09¬
168300150204                   -        (S/N)=N  <== S-FISSO           funz.10¬
168400970526                   -        (S/N)=N                        funz.11¬
168500970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
168600991116                   -        (S/N)=N                        funz.13¬
168700970526                   -        (S/N)=N                        funz.14¬
168800970526                   -        (S/N)=N                        funz.15¬
168900970526                   -        (S/N)=N                        funz.16¬
169000970526                   -        (S/N)=N                        funz.17¬
169100970526                   -        (S/N)=N                        funz.18¬
169200970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
169300970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
169400000407                   -        (S/N)=N                        funz.21¬
169500991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
169600991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
169700991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
