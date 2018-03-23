000100970526     H*------------------------------------------------------------------------*
000200030122     H* Gestione nuove tabelle: Tabella "GGB"
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800030122     FTNTB31D   CF   E             WORKSTN INFDS(DEVDS1)
000900030122     F                                     SFILE(TB31D05:SFLULT)
001000030122     F                                     SFILE(TB31DMS:MSGULT)
001100970526     F*------------
001200030122     Ftntbe01l  IF   E           K DISK
001300970526     D*------------------------------------------------------------------------*
001400970526     D* SCHIERE
001500970526     D*------------------------------------------------------------------------*
001600970526     D*------------
001700970526     D* DECODIFICHE
001800970526     D*------------
001900970526     D DEC             S             66    DIM(5) CTDATA PERRCD(1)
002000970526     D*------------
002100970526     D* MESSAGGI
002200970526     D*------------
002300970526     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002400970528     D*------------
002500970528     D* COMANDI
002600970528     D*------------
002700970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
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
004400991108     D*------------
004500970526     D*------------------------------------------------------------------------*
004600970526     D* INPUT
004700970526     D*------------------------------------------------------------------------*
004800970526     D*------------
004900970526     D* RIDEFINIZIONE OPZIONI VIDEO
005000970526     D*------------
005100970526     D OPDDS           DS
005200970526     D  OPDOPZ                 1      2
005300970526     D  OPDDE1                 3     19
005400970526     D  OPDOP1                21     22
005500970526     D  OPDFI0                23     34
005600970526     D  OPDATT                35     35
005700970526     D  OPDFI1                36     80
005800970526     D  OPDDES                 1     19
005900970526     D  OPDRIG                 1     80
006000970526     D*------------
006100970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006200970526     D*------------
006300970526     D FUDDS           DS
006400970526     D  FUDDE1                 1      1
006500970526     D  FUDFUN                 2      3
006600970526     D  FUDDE2                 4     19
006700970526     D  FUDFI0                20     34
006800970526     D  FUDATT                35     35
006900970526     D  FUDFI1                36     80
007000970526     D  FUDDES                 1     19
007100970526     D  FUDRIG                 1     80
007200970526     D*------------
007300970526     D* DS GESTIONE TASTI FUNZIONALI
007400970526     D*------------
007500970526     D DSKEY         E DS
007600970526     D DEVDS1          DS
007700970526     D  KEY                  369    369
007800970526     D*------------
007900970526     D* DS "XMSG" - GESTIONE MESSAGGI
008000970526     D*------------
008100970526     D DSMSG         E DS
008200970526     D*------------
008300020917     D* DS REPERIMENTO DATI UTENTE
008400970526     D*------------
008500020917     D TIBS34DS      E DS                                                       *Profili utente
008600020917     D DDATIUTE      E DS                                                       *Dati utente
008700020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
008800030122     D*------------
008900030122     D* DS RIDEFINIZIONE NUOVA TABELLA "GGB"
009000030122     D*------------
009100030122     D DGGB          E DS
009200970526     D*------------
009300970526     D* ARCHITETTURA
009400970526     D*------------
009500970526     D KPJBA         E DS
009600021211     D*------------------
009700030122     D* PASSAGGIO PARAMETRI
009800021211     D*------------------
009900021211     D PARAM           DS                  INZ
010000030122     D  POP1                   1      3
010100030122     D  PCAU                   4      7
010200030123     D  PDES                   8     42
010300030123     D  PMSG                  43    122
010400970526     C*------------------------------------------------------------------------*
010500970526     C* MAIN LINES
010600970526     C*------------------------------------------------------------------------*
010700970526     C*
010800970526     C* SE NON CI SONO ERRORI, PROSEGUE
010900030122     C     ERRL00        IFNE      '1'
011000970526     C*
011100970526     C* CARICA IL SFILE
011200970526     C                   EXSR      POSSFL
011300970526     C*
011400970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
011500030122     C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
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
012800030122     C                   ENDDO
012900970527     C*
013000030122     C                   ENDIF
013100991108     C*
013200970526     C* FINE PGM
013300970526     C                   SETON                                        LR
013400970526     C*------------------------------------------------------------------------*
013500970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
013600970526     C*------------------------------------------------------------------------*
013700970526     C     F03GES        BEGSR
013800970526     C*
013900970526     C* IMPOSTA "PREMUTO F03"
014000970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
014100970526     C*
014200970526     C                   ENDSR
014300970526     C*------------------------------------------------------------------------*
014400970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
014500970526     C*------------------------------------------------------------------------*
014600970526     C     F05GES        BEGSR
014700970526     C*
014800970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
014900970526     C*
015000970526     C                   ENDSR
015100970526     C*------------------------------------------------------------------------*
015200030122     C* F06GES - GESTIONE F06 -> INSERIMENTO
015300970526     C*------------------------------------------------------------------------*
015400000407     C     F06GES        BEGSR
015500970526     C*
015600021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
015700021211     C     DUTLPO        IFNE      'S'
015800000407     C                   MOVEL     '1'           ERRL00
015900000407     C                   MOVEL     MSG(3)        DSMSMS
016000021211     C                   ENDIF
016100000407     C*
016200021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
016300030122     C                   MOVEL     *blanks       PCAU
016400000407     C                   MOVEL     'F06'         POP1
016500000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
016600030122     C                   CALL      'TNTB32R'
016700000407     C                   PARM                    KPJBA
016800021211     C                   MOVEL     KPJBU         PARAM
016900000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
017000021211     C                   ENDIF
017100000407     C*
017200970526     C                   ENDSR
017300970526     C*------------------------------------------------------------------------*
017400991108     C* F12GES - GESTIONE F12 -> RITORNO
017500970526     C*------------------------------------------------------------------------*
017600970526     C     F12GES        BEGSR
017700970526     C*
017800970526     C* IMPOSTA "PREMUTO F12"
017900970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
018000970526     C*
018100970526     C                   ENDSR
018200970528     C*------------------------------------------------------------------------*
018300970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
018400970528     C*------------------------------------------------------------------------*
018500970528     C     F21GES        BEGSR
018600970612     C*
018700991117     C* NON GESTITA
018800970528     C*
018900970528     C                   ENDSR
019000970528     C*------------------------------------------------------------------------*
019100970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
019200970528     C*------------------------------------------------------------------------*
019300970528     C     F22GES        BEGSR
019400970528     C*
019500021211     C* NON GESTITA
019600970528     C*
019700970528     C                   ENDSR
019800970526     C*------------------------------------------------------------------------*
019900970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
020000970526     C*------------------------------------------------------------------------*
020100970526     C     F23GES        BEGSR
020200970526     C*
020300970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
020400970526     C*
020500970526     C                   ENDSR
020600970526     C*------------------------------------------------------------------------*
020700970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
020800970526     C*------------------------------------------------------------------------*
020900970526     C     F24GES        BEGSR
021000970526     C*
021100970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
021200970526     C*
021300970526     C                   ENDSR
021400970526     C*------------------------------------------------------------------------*
021500970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
021600970526     C*------------------------------------------------------------------------*
021700970526     C     RUPGES        BEGSR
021800970526     C*
021900970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
022000970526     C*
022100970526     C                   ENDSR
022200970526     C*------------------------------------------------------------------------*
022300991108     C* ENTGES - GESTIONE ENTER -> INVIO
022400970526     C*------------------------------------------------------------------------*
022500970526     C     ENTGES        BEGSR
022600000407     C*
022700000407     C                   MOVEL     '0'           ERRL00
022800991108     C*
022900970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
023000970526     C                   EXSR      NEWPOS
023100021211     C*
023200021211     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
023300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
023400021211     C     SWPOSI        IFNE      0
023500021211     C                   EXSR      POSSFL                                       *RICARICA SFILE
023600021211     C                   ELSE
023700021211     C     SFLULT        IFNE      0
023800021211     C                   EXSR      ENTATT                                       *GESTIONE ENTER
023900021211     C*
024000021211     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
024100021211     C     SWMODI        IFNE      0
024200021211     C                   EXSR      POSSFL
024300021211     C                   ENDIF
024400021211     C                   ENDIF
024500021211     C                   ENDIF
024600021211     C                   ENDIF
024700021211     C*
024800021211     C                   ENDSR
024900000420     C*------------------------------------------------------------------------*
025000000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
025100000420     C*------------------------------------------------------------------------*
025200000420     C     O01GES        BEGSR
025300000420     C*
025400000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
025500030122     C                   MOVEL     VIDCAU        PCAU
025600030123     C                   MOVEL     VIDDES        PDES
025700000420     C                   MOVEL     PARAM         KPJBU
025800000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
025900000420     C*
026000000420     C                   ENDSR
026100000407     C*------------------------------------------------------------------------*
026200000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
026300000407     C*------------------------------------------------------------------------*
026400000407     C     O02GES        BEGSR
026500000407     C*
026600021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
026700021211     C     DUTLPO        IFNE      'S'
026800021211     C                   MOVEL     '1'           ERRL00
026900021211     C                   MOVEL     MSG(3)        DSMSMS
027000021211     C                   ENDIF
027100021211     C*
027200021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
027300030122     C                   MOVEL     VIDCAU        PCAU
027400000407     C                   MOVEL     'O02'         POP1
027500000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
027600030122     C                   CALL      'TNTB32R'
027700000407     C                   PARM                    KPJBA
027800021211     C                   MOVEL     KPJBU         PARAM
027900021211     C                   ENDIF
028000000407     C*
028100000407     C                   ENDSR
028200991112     C*------------------------------------------------------------------------*
028300021211     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
028400991112     C*------------------------------------------------------------------------*
028500991112     C     O04GES        BEGSR
028600021211     C*
028700021211     C* OPERAZIONE CONSENTITO SOLO IN SEDE
028800021211     C     DUTLPO        IFNE      'S'
028900021211     C                   MOVEL     '1'           ERRL00
029000021211     C                   MOVEL     MSG(3)        DSMSMS
029100021211     C                   ENDIF
029200991112     C*
029300021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
029400030122     C                   MOVEL     VIDCAU        PCAU
029500000407     C                   MOVEL     'O04'         POP1
029600000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
029700030122     C                   CALL      'TNTB32R'
029800000407     C                   PARM                    KPJBA
029900021211     C                   MOVEL     KPJBU         PARAM
030000000411     C                   ENDIF
030100991112     C*
030200991112     C                   ENDSR
030300991014     C*------------------------------------------------------------------------*
030400000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
030500970526     C*------------------------------------------------------------------------*
030600970526     C     O05GES        BEGSR
030700000407     C*
030800021211     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030900030122     C                   MOVEL     VIDCAU        PCAU
031000000407     C                   MOVEL     'O05'         POP1
031100000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
031200030122     C                   CALL      'TNTB32R'
031300000407     C                   PARM                    KPJBA
031400021211     C                   ENDIF
031500000407     C*
031600970526     C                   ENDSR
031700030124     C*------------------------------------------------------------------------*
031800030124     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO RECORD
031900030124     C*------------------------------------------------------------------------*
032000030124     C     O07GES        BEGSR
032100030124     C*
032200030124     C* OPERAZIONE CONSENTITO SOLO IN SEDE
032300030124     C     DUTLPO        IFNE      'S'
032400030124     C                   MOVEL     '1'           ERRL00
032500030124     C                   MOVEL     MSG(3)        DSMSMS
032600030124     C                   ENDIF
032700030124     C*
032800030124     C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032900030124     C                   MOVEL     VIDCAU        PCAU
033000030124     C                   MOVEL     'O07'         POP1
033100030124     C                   EXSR      PASPAR                                       *RICARICA SFILE
033200030124     C                   CALL      'TNTB32R'
033300030124     C                   PARM                    KPJBA
033400030124     C                   MOVEL     KPJBU         PARAM
033500030124     C                   ENDIF
033600030124     C*
033700030124     C                   ENDSR
033800991028     C*------------------------------------------------------------------------*
033900991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
034000991112     C*------------------------------------------------------------------------*
034100991112     C     PASPAR        BEGSR
034200991112     C*
034300000407     C                   MOVEL     PARAM         KPJBU
034400991112     C*
034500991112     C                   ENDSR
034600970526     C*------------------------------------------------------------------------*
034700970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
034800970526     C*------------------------------------------------------------------------*
034900970526     C     NEWPOS        BEGSR
035000970526     C*
035100970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
035200970526     C*         1= RICHIESTO POSIZIONAMENTO
035300970526     C                   Z-ADD     0             SWPOSI            7 0
035400970526     C*
035500991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
035600970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
035700970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
035800970526     C                   Z-ADD     0             SWMODI            1 0
035900970530     C*
036000970530     C* REIMPOSTA INDICATORI DI ERRORE
036100991108     C                   SETOFF                                       808182
036200991108     C                   SETOFF                                       83
036300970526     C*
036400970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
036500030122     C     VIDTIM        IFNE      deptim
036600021211     C     ERRL00        IFEQ      '0'                                          *NO ERORI
036700970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
036800030122     C                   MOVEL     VIDTIM        deptim                         *DEPOSITI
036900021211     C                   ENDIF
037000021211     C                   ENDIF
037100970526     C*
037200970526     C                   ENDSR
037300970526     C*------------------------------------------------------------------------*
037400021211     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
037500970526     C*------------------------------------------------------------------------*
037600970528     C     SETFIL        BEGSR
037700970526     C*
037800970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
037900030122     C                   MOVEL     'GGB'         tbeCOD
038000030122     C     KEYTBE        SETLL     tntbe01l                           99
038100980107     C*
038200021211     C* LETTURA RECORD
038300021211     C     *IN99         IFEQ      *OFF
038400970526     C                   EXSR      LETFIL
038500021211     C                   ENDIF
038600970526     C*
038700970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
038800021211     C     *IN99         IFEQ      *ON
038900970526     C                   Z-ADD     1             SFLEOF
039000021211     C                   ENDIF
039100970526     C*
039200970526     C                   ENDSR
039300970526     C*------------------------------------------------------------------------*
039400970526     C* LETFIL - LETTURA RECORD
039500970526     C*------------------------------------------------------------------------*
039600970526     C     LETFIL        BEGSR
039700970526     C*
039800970526     C* LEGGE FINO A:
039900970526     C*     - FINE FILE
040000970526     C*     - TROVATO RECORD VALIDO
040100970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
040200021211     C     *IN99         DOWEQ     *OFF
040300970526     C     WRECOK        ANDEQ     'N'
040400970526     C*
040500970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
040600030122     C     KEYTBE        READE     tntbe01l                               99
040700970528     C*
040800970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
040900021211     C     *IN99         IFEQ      *ON                                          *FINE FILE
041000970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
041100021211     C                   ELSE
041200030122     C                   EVAL      DGGB = tbeUNI
041300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
041400021211     C                   ENDIF
041500021211     C                   ENDDO
041600970526     C*
041700970526     C                   ENDSR
041800970526     C*------------------------------------------------------------------------*
041900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
042000970526     C*------------------------------------------------------------------------*
042100970526     C     CHKREC        BEGSR
042200970526     C*
042300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
042400030122     C*
042500030122     C* FILTRA SE RICHIESTO UN TIPO CAUSALE
042600030122     C     VIDTIM        IFEQ      *blanks
042700030122     C                   ELSE
042800030122     C     VIDTIM        IFNE      §GGBTIM
042900030122     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043000030122     C                   ENDIF
043001150305     c* se richiamato da immissione movimenti filtro anche per utente
043002150305     c                   if        *in90 and §GGBAUT <> ' '
043003150305     c                   if        §GGBAUT ='S' and dutpou <> 046
043004150305     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043005150305     c                   end
043006150305     c                   if        §GGBAUT ='F' and dutpou = 046
043007150305     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
043008150305     c                   end
043009150305     c                   end
043100030122     C                   ENDIF
043200991108     C*
043300970526     C                   ENDSR
043400970526     C*------------------------------------------------------------------------*
043500000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
043600970526     C*------------------------------------------------------------------------*
043700970526     C     IRESFL        BEGSR
043800970526     C*
043900000407     C                   MOVEL     *blanks       OPZKEY
044000030122     C                   MOVEL     tbeATB        VIDATB
044100030122     C                   MOVEL     tbeKE1        VIDCAU
044200030122     C                   MOVEL     §GGBDES       VIDDES
044300150210     c                   clear                   autv
044400150210     c                   if        §ggbaut ='S'
044500150217     c                   eval      autv = 'Sede'
044600150210     c                   end
044700150210     c                   if        §ggbaut ='F'
044800150217     c                   eval      autv = 'Fil.'
044900150210     c                   end
045000150210     c                   clear                   segv
045100150210     c                   if        §ggbseg ='E'
045200150210     c                   eval      segv = '+'
045300150210     c                   end
045400150210     c                   if        §ggbseg ='U'
045500150210     c                   eval      segv = '-'
045600150210     c                   end
045700150210     c                   movel     §ggbtim       timv
045800150217     c                   if        §ggbtip ='D'
045900150217     c                   eval      tipv = 'Des.'
046000150217     c                   end
046100150217     c                   if        §ggbtip ='F'
046200150217     c                   eval      tipv = 'Fil.'
046300150217     c                   end
046400150217     c                   if        §ggbtip ='C'
046500150217     c                   eval      tipv = 'Cli.'
046600150217     c                   end
046700150210     c                   movel     §ggbls01      ls01v
046800150304     c                   movel     §ggbtsc       tscv
046801150225     c                   movel     §ggbsco       scov
046900991108     C*
047000970526     C                   ENDSR
047100970526     C*------------------------------------------------------------------------*
047200030122     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
047300970526     C*------------------------------------------------------------------------*
047400970526     C     CHKFOP        BEGSR
047500991112     C*
047600991112     C* OPZIONI
047700970626     C*
047800000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
047900021211     C     DEPOP0        IFEQ      'P01'
048000000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
048100000420     C                   MOVEL     'S'           OPDATT
048200000420     C                   MOVEL     OPDRIG        OPD(1)
048300021211     C                   ELSE
048400000420     C* OPZIONI
048500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
048600970626     C                   MOVEL     'S'           OPDATT
048700970626     C                   MOVEL     OPDRIG        OPD(5)
048800021211     C                   ENDIF
048900970626     C*
049000000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
049100021211     C     DEPOP0        IFEQ      'P02'
049200000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
049300970626     C                   MOVEL     'S'           OPDATT
049400000407     C                   MOVEL     OPDRIG        OPD(2)
049500021211     C                   MOVEL     OPD(3)        OPDRIG                         *COPIA
049600021211     C                   MOVEL     'S'           OPDATT
049700021211     C                   MOVEL     OPDRIG        OPD(3)
049800000411     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
049900000411     C                   MOVEL     'S'           OPDATT
050000000411     C                   MOVEL     OPDRIG        OPD(4)
050100991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
050200991112     C                   MOVEL     'S'           OPDATT
050300991112     C                   MOVEL     OPDRIG        OPD(5)
050400030124     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
050500030124     C                   MOVEL     'S'           OPDATT
050600030124     C                   MOVEL     OPDRIG        OPD(7)
050700021211     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
050800000407     C                   MOVEL     'S'           FUDATT
050900000407     C                   MOVEL     FUDRIG        FUD(6)
051000021211     C                   ENDIF
051100970626     C*
051200970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
051300021211     C     DUTLPO        IFNE      'S'                                          *FILIALE
051400000407     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
051500000407     C                   MOVEL     'N'           OPDATT
051600000407     C                   MOVEL     OPDRIG        OPD(2)
051700991112     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
051800991112     C                   MOVEL     'N'           OPDATT
051900991112     C                   MOVEL     OPDRIG        OPD(4)
052000030124     C                   MOVEL     OPD(7)        OPDRIG                         *NO RIPRISTINO
052100030124     C                   MOVEL     'N'           OPDATT
052200030124     C                   MOVEL     OPDRIG        OPD(7)
052300021211     C                   MOVEL     FUD(6)        FUDRIG                         *NO INSERIMENTO
052400021211     C                   MOVEL     'N'           OPDATT
052500021211     C                   MOVEL     FUDRIG        FUD(6)
052600021211     C                   ENDIF
052700970526     C*
052800970526     C* GESTIONE OPZIONI A VIDEO
052900970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
053000970526     C                   EXSR      OPZGES
053100970526     C*
053200970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
053300970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
053400970526     C                   EXSR      FUNGES
053500970526     C*
053600970526     C                   ENDSR
053700020917     C*--------------------------------------------------------------------------------------------*
053800020917     C* REPERISCE I DATI UTENTE
053900020917     C*--------------------------------------------------------------------------------------------*
054000020917     C     REPDATIUTE    BEGSR
054100020917     C*
054200020917     C* INIZIALIZZA VARIABILI DI WRK
054300020917     C                   CLEAR                   TIBS34DS
054400020917     C                   CLEAR                   AZUTEDS
054500020917     C                   CLEAR                   DDATIUTE
054600020917     C*
054700020917     C     *DTAARA       DEFINE    §azute        azuteds
054800020917     C     *DTAARA       DEFINE    §datiute      ddatiute
054900020917     C                   IN(E)     *DTAARA
055000021211     C                   IF        %Error
055100021211     C                   EVAL      I34Tla = 'L'
055200020917     C                   CALL      'TIBS34R'
055300020917     C                   PARM                    Tibs34Ds
055400020917     C                   IN        *DTAARA
055500021211     C                   ENDIF
055600020917     C*
055700020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
055800020917     C                   MOVEL(P)  RSUT          DSFIRS           20
055900020917     C*
056000020917     C                   ENDSR
056100970526     C*------------------------------------------------------------------------*
056200970526     C* *INZSR - OPERAZIONI INIZIALI
056300970526     C*------------------------------------------------------------------------*
056400970526     C     *INZSR        BEGSR
056500970526     C*---
056600970526     C* RICEVIMENTO PARAMETRI
056700970526     C*---
056800970526     C     *ENTRY        PLIST
056900970612     C                   PARM                    KPJBA
057000021211     C                   MOVEL     KPJBU         PARAM
057100030123     C*
057200030123     C* EFFETTUO ALCUNE CONSIDERAZIONI SUI PARAMETRI DI ENTRATA
057300030123     C                   IF        POP1 = *blanks
057400030123     C                   EVAL      POP1 = 'P02'
057500030123     C                   ENDIF
057600030123     C                   IF        POP1 = 'P1M'
057700030123     C                   EVAL      VIDTIM = 'M'
057800030123     C                   EVAL      deptim = 'M'
057900030123     C                   EVAL      POP1 = 'P01'
058000030123     C                   SETON                                        90
058100030123     C                   ENDIF
058200021211     C                   MOVEL     POP1          DEPOP0            3
058300970526     C*
058400970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
058500970526     C     XMSGPA        PLIST
058600970526     C                   PARM                    DSMSMS
058700970526     C                   PARM                    DSMSPG
058800970526     C                   PARM                    DSMSKE
058900970526     C                   PARM                    DSMSER
059000970526     C*
059100970526     C* IMPOSTA LA DS -TASTI PREMUTI-
059200970526     C                   CALL      'XKEY'
059300970526     C                   PARM                    DSKEY
059400970526     C*---
059500970526     C* VARIABILI NON RIFERITE AL DATA BASE
059600970526     C*---
059700970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
059800970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
059900970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
060000970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
060100970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
060200970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
060300970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
060400970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
060500970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
060600970526     C*---
060700030122     C     *LIKE         DEFINE    VIDTIM        deptim
060800991108     C*---
060900970526     C* VARIABILI DI CONTROLLO
061000970526     C*---
061100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
061200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
061300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
061400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
061500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
061600000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
061700970526     C*
061800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
061900030122     C                   MOVEL     'TNTB31R'     DSMSPG
062000030122     C                   MOVEL     'TNTB31R'     MSGQUE
062100970526     C                   MOVEL     *BLANKS       MSGKEY
062200970526     C                   MOVEL     *BLANKS       DSMSMS
062300970526     C                   MOVEL     *BLANKS       DSMSKE
062400970526     C                   Z-ADD     *ZEROS        DSMSER
062500970526     C*---
062600970526     C* CHIAVI DI LETTURA
062700970526     C*---
062800030122     C* LETTURA tntbe01l - PARZIALE
062900030122     C     KEYTBE        KLIST
063000030122     C                   KFLD                    tbeCOD
063100991108     C*
063200970527     C* REPERIMENTO DATI UTENTE
063300020917     C                   EXSR      REPDATIUTE
063400991115     C*
063500970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
063600970526     C                   EXSR      CHKFOP
063700970526     C*
063800970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
063900021211     C     DEPOP0        IFEQ      'P01'
064000970530     C                   MOVEL     DEC(3)        VIDING
064100021211     C                   ENDIF
064200021211     C     DEPOP0        IFEQ      'P02'
064300970526     C                   MOVEL     DEC(1)        VIDING
064400021211     C                   ENDIF
064500021211     C     DEPOP0        IFEQ      'P05'
064600970526     C                   MOVEL     DEC(2)        VIDING
064700021211     C                   ENDIF
064800970526     C*
064900970526     C                   ENDSR
065000970526     C*------------------------------------------------------------------------*
065100970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065300970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
065400970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
065500970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
065600970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
065800970526     C*------------------------------------------------------------------------*
065900970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
066000970526     C*------------------------------------------------------------------------*
066100970526     C     POSSFL        BEGSR
066200970526     C*
066300970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
066400970526     C                   EXSR      CLRSFL
066500970526     C*
066600970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
066700970526     C                   EXSR      SETFIL
066800970526     C*
066900970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
067000970526     C     SFLEOF        IFEQ      0
067100970526     C                   EXSR      CARSFL
067200970526     C                   END
067300970526     C*
067400970526     C                   ENDSR
067500970526     C*------------------------------------------------------------------------*
067600970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
067700970526     C*------------------------------------------------------------------------*
067800970526     C     ENTATT        BEGSR
067900970526     C*
068000970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
068100970526     C                   Z-ADD     0             OPZOK             1 0
068200970526     C*
068300970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
068400970526     C                   Z-ADD     0             SFLVAR            5 0
068500970526     C*
068600970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
068700970526     C                   Z-ADD     0             SFLERR
068800970526     C*
068900970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
069000970526     C                   Z-ADD     SFLULT        WFLULT
069100970526     C*
069200970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
069300030122     C                   READC     TB31D05                                98
069400970526     C     *IN98         DOWEQ     '0'
069500970526     C*
069600970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
069700970526     C                   ADD       1             SFLVAR
069800970526     C*
069900970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
070000970526     C     OPZOK         IFEQ      0
070100970526     C*
070200970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
070300970526     C     OPZKEY        IFNE      *BLANKS
070400970526     C                   EXSR      OPZCON
070500970526     C                   END
070600970526     C*
070700970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
070800970526     C*            2. PULIZIA SCELTA IN SFL
070900970526     C     OPZOK         IFEQ      0
071000970526     C                   MOVEL     '0'           *IN41
071100970526     C                   MOVEL     *BLANKS       OPZKEY
071200970526     C                   ELSE
071300970526     C*
071400970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
071500970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
071600970526     C                   MOVEL     '1'           *IN41
071700970526     C                   Z-ADD     SFLULT        SFLERR
071800970526     C                   END
071900970526     C                   ELSE
072000970526     C*
072100970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
072200970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
072300970526     C                   MOVEL     '1'           *IN41
072400970526     C                   END                                                    OPZOK=0
072500970526     C*
072600970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
072700970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
072800970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
072900970526     C     OPZOK         IFNE      0
073000970526     C                   Z-ADD     SFLERR        SFLNBR
073100970526     C                   ELSE
073200970526     C                   Z-ADD     SFLULT        SFLNBR
073300970526     C                   END
073400970526     C*
073500970526     C* AGGIORNO RECORD SFL
073600030122     C                   UPDATE    TB31D05
073700970526     C*
073800030122     C                   READC     TB31D05                                98
073900970526     C                   END                                                    DO WHILE
074000970526     C*
074100970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
074200970526     C                   Z-ADD     WFLULT        SFLULT
074300970526     C*
074400970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
074500970526     C     SFLVAR        IFEQ      0
074600970526     C                   END
074700970526     C*
074800991115     C     OPZKEY        IFNE      ' 5'
074900991115     C                   EXSR      F05GES
075000991115     C                   ENDIF
075100991112     C*
075200970526     C                   ENDSR
075300970526     C*------------------------------------------------------------------------*
075400970526     C* CLRSFL - ELIMINAZIONE CONTENUTO SFL
075500970526     C*------------------------------------------------------------------------*
075600970526     C     CLRSFL        BEGSR
075700970526     C*
075800970526     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
075900970526     C* (0=NON FINE FILE  1=FINE FILE)
076000970526     C                   Z-ADD     0             SFLEOF
076100970526     C*
076200970526     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
076300970526     C                   Z-ADD     0             SFLULT
076400970526     C*
076500970526     C* ACCENSIONE INDICATORE PER CLEAR SFL
076600970526     C                   MOVEL     '1'           *IN40
076700030122     C                   WRITE     TB31D06
076800970526     C                   MOVEL     '0'           *IN40
076900970526     C*
077000970526     C                   ENDSR
077100970526     C*------------------------------------------------------------------------*
077200970526     C* CARSFL - CARICAMENTO SFL
077300970526     C*------------------------------------------------------------------------*
077400970526     C     CARSFL        BEGSR
077500970526     C*
077600970526     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
077700970526     C     SFLULT        IFEQ      0
077800970526     C                   EXSR      SETFIL
077900970526     C                   END
078000970526     C*
078100970526     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
078200970526     C                   Z-ADD     SFLULT        WFLULT
078300970526     C*
078400970526     C* CICLO:
078500970526     C*    -CARICO NUMERO DI RIGHE=SFLPAG
078600970526     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
078700970526     C                   Z-ADD     1             SFLRIG
078800970526     C     SFLRIG        DOWLE     SFLPAG
078900970526     C     SFLEOF        ANDEQ     0
079000970526     C*
079100970526     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
079200991116     C                   MOVEL     *BLANKS       OPZKEY
079300970526     C*
079400970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
079500970526     C                   EXSR      IRESFL
079600970526     C*
079700970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
079800970526     C                   ADD       1             SFLULT
079900970526     C*
080000970526     C* SCRITTURA RECORD SFL
080100030122     C                   WRITE     TB31D05
080200970526     C                   ADD       1             SFLRIG
080300970526     C*
080400970526     C* LETTURA FILE DA CARICARE
080500970526     C                   EXSR      LETFIL
080600970526     C                   END                                                    DO WHILE
080700970526     C*
080800970526     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
080900970526     C     WFLULT        IFNE      SFLULT
081000970526     C                   Z-ADD     WFLULT        SFLNBR
081100970526     C                   ADD       1             SFLNBR
081200970526     C                   END
081300970526     C*
081400970526     C                   ENDSR
081500970526     C*------------------------------------------------------------------------*
081600970526     C* OUTSFL - EMISSIONE SUBFILE A VIDEO
081700970526     C*------------------------------------------------------------------------*
081800970526     C     OUTSFL        BEGSR
081900970526     C*
082000970526     C* WRITE RECORD TESTATA VIDEO
082100030122     C                   WRITE     TB31D01
082200970526     C*
082300970526     C* VISUALIZZAZIONE MESSAGGIO
082400970526     C                   EXSR      SNDMSG
082500970526     C*
082600970526     C* WRITE RECORD OPZIONI VIDEO
082700030122     C                   WRITE     TB31D02
082800970526     C*
082900970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
083000030122     C                   WRITE     TB31D03
083100970526     C*
083200970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
083300030122     C                   WRITE     TB31D04
083400970526     C*
083500970526     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
083600970526     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
083700970526     C     SFLEOF        IFEQ      0
083800970526     C                   MOVEL     '0'           *IN42
083900970526     C                   ELSE
084000970526     C*
084100970526     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
084200970526     C                   MOVEL     '1'           *IN42
084300970526     C                   END
084400970526     C*
084500970526     C* WRITE RECORD TASTI FUNZIONALI
084600030122     C                   WRITE     TB31D09
084700970526     C*
084800970526     C* CASO DI SFL VUOTO
084900970526     C     SFLULT        IFLE      0
085000970526     C*
085100970526     C*    - WRITE RECORD VUOTO
085200030122     C                   WRITE     TB31D0A
085300030122     C                   READ      TB31D0A                                99
085400970526     C*
085500970526     C                   ELSE
085600970526     C*
085700970526     C* CASO DI SFL CON RECORDS
085800970526     C*
085900970526     C*    - WRITE RECORD CONTROLLO SFL
086000030122     C                   WRITE     TB31D06
086100030122     C                   READ      TB31D06                                99
086200970526     C                   END
086300970526     C*
086400970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
086500030122     C                   READ      TB31D03                                99
086600970526     C*
086700970526     C                   ENDSR
086800970526     C*------------------------------------------------------------------------*
086900970526     C* SNDMSG - MANDA MESSAGGIO
087000970526     C*------------------------------------------------------------------------*
087100970526     C     SNDMSG        BEGSR
087200970526     C*
087300970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
087400970526     C                   MOVEL     '1'           *IN40
087500030122     C                   WRITE     TB31DMC
087600970526     C                   MOVEL     '0'           *IN40
087700970526     C*
087800970526     C* VISUALIZZAZIONE MESSAGGIO
087900970526     C                   Z-ADD     0             MSGULT
088000970526     C                   CALL      'XMSG'        XMSGPA
088100970526     C     DSMSER        IFEQ      0
088200970526     C                   MOVEL     DSMSKE        MSGKEY
088300970526     C                   END
088400970526     C                   Z-ADD     1             MSGULT
088500030122     C                   WRITE     TB31DMS                                      SFL MESS
088600030122     C                   WRITE     TB31DMC                                      CONTR MESS
088700970526     C*
088800970526     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
088900970526     C                   MOVEL     *BLANKS       DSMSMS
089000970526     C*
089100970526     C                   ENDSR
089200970526     C*------------------------------------------------------------------------*
089300970526     C* FUNCON - GESTIONE SCELTE DELL'OPERATORE A VIDEO
089400970526     C*------------------------------------------------------------------------*
089500970526     C     FUNCON        BEGSR
089600970526     C*
089700970526     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
089800970526     C                   EXSR      FUNVAR
089900970526     C*
090000970526     C* PULISCO CAMPI DI LAVORO
090100970526     C                   Z-ADD     0             I                 5 0
090200970526     C                   Z-ADD     0             W                 5 0
090300970526     C*
090400970526     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
090500970526     C     FUNKEY        IFNE      *BLANKS
090600970526     C     1             DO        24            I
090700970526     C                   MOVEL     FUD(I)        FUDRIG
090800970526     C*
090900970526     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
091000970526     C     FUNKEY        IFEQ      FUDFUN
091100970526     C     FUDATT        ANDEQ     'S'
091200970526     C                   Z-ADD     I             W
091300970526     C                   END
091400970526     C                   END                                                    DO WHILE
091500970526     C*
091600970526     C* FUNZIONE NON ATTIVA (ERRORE)
091700970526     C     W             IFEQ      0
091800970526     C*
091900970526     C* EMISSIONE MESSAGGIO DI ERRORE
092000970526     C                   MOVEL     MSG(2)        DSMSMS
092100970526     C                   END                                                    W=0
092200970526     C                   END                                                    FUNKEY NE *BLANKS
092300970526     C*
092400970526     C* FUNZIONE ATTIVA (PROCEDI ...)
092500970526     C     W             IFNE      0
092600970526     C     FUNKEY        OREQ      *BLANKS
092700970526     C*
092800970526     C     FUNKEY        IFEQ      *BLANKS
092900970526     C                   EXSR      ENTGES
093000970526     C                   END
093100970526     C     FUNKEY        IFEQ      '03'
093200970526     C                   EXSR      F03GES
093300970526     C                   END
093400970526     C     FUNKEY        IFEQ      '05'
093500970526     C                   EXSR      F05GES
093600970526     C                   END
093700970526     C     FUNKEY        IFEQ      '06'
093800970526     C                   EXSR      F06GES
093900970526     C                   END
094000970526     C     FUNKEY        IFEQ      '12'
094100970526     C                   EXSR      F12GES
094200970526     C                   END
094300970528     C     FUNKEY        IFEQ      '21'
094400970528     C                   EXSR      F21GES
094500970528     C                   END
094600970528     C     FUNKEY        IFEQ      '22'
094700970528     C                   EXSR      F22GES
094800970528     C                   END
094900970526     C     FUNKEY        IFEQ      '23'
095000970526     C                   EXSR      F23GES
095100970526     C                   END
095200970526     C     FUNKEY        IFEQ      '24'
095300970526     C                   EXSR      F24GES
095400970526     C                   END
095500970526     C     FUNKEY        IFEQ      'RU'
095600970526     C                   EXSR      RUPGES
095700970526     C                   END
095800970526     C*
095900970526     C                   END
096000970526     C*
096100970526     C                   ENDSR
096200970526     C*------------------------------------------------------------------------*
096300970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
096400970526     C*------------------------------------------------------------------------*
096500970526     C     FUNVAR        BEGSR
096600970526     C*
096700970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
096800970526     C     KEY           IFEQ      ENTER
096900970526     C                   MOVEL     *BLANKS       FUNKEY
097000970526     C                   END
097100970526     C     KEY           IFEQ      F1
097200970526     C                   MOVEL     '01'          FUNKEY
097300970526     C                   END
097400970526     C     KEY           IFEQ      F2
097500970526     C                   MOVEL     '02'          FUNKEY
097600970526     C                   END
097700970526     C     KEY           IFEQ      F3
097800970526     C                   MOVEL     '03'          FUNKEY
097900970526     C                   END
098000970526     C     KEY           IFEQ      F4
098100970526     C                   MOVEL     '04'          FUNKEY
098200970526     C                   END
098300970526     C     KEY           IFEQ      F5
098400970526     C                   MOVEL     '05'          FUNKEY
098500970526     C                   END
098600970526     C     KEY           IFEQ      F6
098700970526     C                   MOVEL     '06'          FUNKEY
098800970526     C                   END
098900970526     C     KEY           IFEQ      F7
099000970526     C                   MOVEL     '07'          FUNKEY
099100970526     C                   END
099200970526     C     KEY           IFEQ      F8
099300970526     C                   MOVEL     '08'          FUNKEY
099400970526     C                   END
099500970526     C     KEY           IFEQ      F9
099600970526     C                   MOVEL     '09'          FUNKEY
099700970526     C                   END
099800970526     C     KEY           IFEQ      F10
099900970526     C                   MOVEL     '10'          FUNKEY
100000970526     C                   END
100100970526     C     KEY           IFEQ      F11
100200970526     C                   MOVEL     '11'          FUNKEY
100300970526     C                   END
100400970526     C     KEY           IFEQ      F12
100500970526     C                   MOVEL     '12'          FUNKEY
100600970526     C                   END
100700970526     C     KEY           IFEQ      F13
100800970526     C                   MOVEL     '13'          FUNKEY
100900970526     C                   END
101000970526     C     KEY           IFEQ      F14
101100970526     C                   MOVEL     '14'          FUNKEY
101200970526     C                   END
101300970526     C     KEY           IFEQ      F15
101400970526     C                   MOVEL     '15'          FUNKEY
101500970526     C                   END
101600970526     C     KEY           IFEQ      F16
101700970526     C                   MOVEL     '16'          FUNKEY
101800970526     C                   END
101900970526     C     KEY           IFEQ      F17
102000970526     C                   MOVEL     '17'          FUNKEY
102100970526     C                   END
102200970526     C     KEY           IFEQ      F18
102300970526     C                   MOVEL     '18'          FUNKEY
102400970526     C                   END
102500970526     C     KEY           IFEQ      F19
102600970526     C                   MOVEL     '19'          FUNKEY
102700970526     C                   END
102800970526     C     KEY           IFEQ      F20
102900970526     C                   MOVEL     '20'          FUNKEY
103000970526     C                   END
103100970526     C     KEY           IFEQ      F21
103200970526     C                   MOVEL     '21'          FUNKEY
103300970526     C                   END
103400970526     C     KEY           IFEQ      F22
103500970526     C                   MOVEL     '22'          FUNKEY
103600970526     C                   END
103700970526     C     KEY           IFEQ      F23
103800970526     C                   MOVEL     '23'          FUNKEY
103900970526     C                   END
104000970526     C     KEY           IFEQ      F24
104100970526     C                   MOVEL     '24'          FUNKEY
104200970526     C                   END
104300970526     C     KEY           IFEQ      ROLLUP
104400970526     C                   MOVEL     'RU'          FUNKEY
104500970526     C                   END
104600970526     C     KEY           IFEQ      HELP
104700970526     C                   MOVEL     'HP'          FUNKEY
104800970526     C                   END
104900970526     C*
105000970526     C                   ENDSR
105100970526     C*------------------------------------------------------------------------*
105200970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
105300970526     C*------------------------------------------------------------------------*
105400970526     C     FUNGES        BEGSR
105500970526     C*
105600970526     C* PULISCO CAMPI DI LAVORO
105700970526     C                   Z-ADD     0             W                 5 0
105800970526     C*
105900970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
106000970526     C* E CARICO TABELLA FUNZIONI ATTIVE
106100970526     C     1             DO        24            I
106200970526     C                   MOVEL     FUD(I)        FUDRIG
106300970526     C*
106400970526     C* PULIZIA FUNZIONI PRECEDENTI
106500970526     C                   MOVEL     *BLANKS       FUV(I)
106600970526     C*
106700970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
106800970526     C* (SALTA F23 E F24)
106900970526     C     FUDATT        IFEQ      'S'
107000970526     C     FUDFUN        ANDNE     '23'
107100970526     C     FUDFUN        ANDNE     '24'
107200970526     C                   ADD       1             W
107300970526     C                   MOVEL     FUDDES        FUV(W)
107400970526     C                   END
107500970526     C                   END                                                    DO WHILE
107600970526     C*
107700970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
107800970526     C* ALTRIMENTI DISATTIVO F24
107900970526     C     1             DO        24            I
108000970526     C                   MOVEL     FUD(I)        FUDRIG
108100970526     C*
108200970526     C     FUDFUN        IFEQ      '24'
108300970526     C     W             IFGT      6
108400970526     C                   MOVEL     'S'           FUDATT
108500970526     C                   ELSE
108600970526     C                   MOVEL     *BLANKS       FUDATT
108700970526     C                   END                                                    W GT 8
108800970526     C                   MOVEL     FUDRIG        FUD(I)
108900970526     C                   END
109000970526     C                   END                                                    DO WHILE
109100970526     C*
109200970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
109300970526     C     W             DIV       6             I
109400970526     C                   MVR                     Z
109500970526     C*
109600970526     C     Z             IFNE      0
109700970526     C     I             ADD       1             Z
109800970526     C                   ELSE
109900970526     C                   Z-ADD     I             Z
110000970526     C                   END
110100970526     C*
110200970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
110300970526     C                   ADD       1             FUNGRU
110400970526     C     FUNGRU        IFGT      Z
110500970526     C     FUNGRU        ORLT      1
110600970526     C                   Z-ADD     1             FUNGRU
110700970526     C                   END
110800970526     C*
110900970526     C* PRIMO GRUPPO DI 6 FUNZIONI
111000970526     C     FUNGRU        IFEQ      1
111100970526     C                   MOVEL     FUV(1)        FUN001
111200970526     C                   MOVEL     FUV(2)        FUN002
111300970526     C                   MOVEL     FUV(3)        FUN003
111400970526     C                   MOVEL     FUV(4)        FUN004
111500970526     C                   MOVEL     FUV(5)        FUN005
111600970526     C                   MOVEL     FUV(6)        FUN006
111700970526     C                   END
111800970526     C*
111900970526     C* SECONDO GRUPPO DI 6 FUNZIONI
112000970526     C     FUNGRU        IFEQ      2
112100970526     C                   MOVEL     FUV(7)        FUN001
112200970526     C                   MOVEL     FUV(8)        FUN002
112300970526     C                   MOVEL     FUV(9)        FUN003
112400970526     C                   MOVEL     FUV(10)       FUN004
112500970526     C                   MOVEL     FUV(11)       FUN005
112600970526     C                   MOVEL     FUV(12)       FUN006
112700970526     C                   END
112800970526     C*
112900970526     C* TERZO GRUPPO DI 6 FUNZIONI
113000970526     C     FUNGRU        IFEQ      3
113100970526     C                   MOVEL     FUV(13)       FUN001
113200970526     C                   MOVEL     FUV(14)       FUN002
113300970526     C                   MOVEL     FUV(15)       FUN003
113400970526     C                   MOVEL     FUV(16)       FUN004
113500970526     C                   MOVEL     FUV(17)       FUN005
113600970526     C                   MOVEL     FUV(18)       FUN006
113700970526     C                   END
113800970526     C*
113900970526     C* QUARTO GRUPPO DI 6 FUNZIONI
114000970526     C     FUNGRU        IFEQ      4
114100970526     C                   MOVEL     FUV(19)       FUN001
114200970526     C                   MOVEL     FUV(20)       FUN002
114300970526     C                   MOVEL     FUV(21)       FUN003
114400970526     C                   MOVEL     FUV(22)       FUN004
114500970526     C                   MOVEL     *BLANKS       FUN005
114600970526     C                   MOVEL     *BLANKS       FUN006
114700970526     C                   END
114800970526     C*
114900970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
115000970526     C                   MOVEL     *BLANKS       FUN007
115100970526     C                   MOVEL     *BLANKS       FUN008
115200970526     C*
115300970526     C     1             DO        24            I
115400970526     C                   MOVEL     FUD(I)        FUDRIG
115500970526     C     FUDFUN        IFEQ      '23'
115600970526     C     FUDATT        ANDEQ     'S'
115700970526     C                   MOVEL     FUDDES        FUN007
115800970526     C                   END
115900970526     C     FUDFUN        IFEQ      '24'
116000970526     C     FUDATT        ANDEQ     'S'
116100970526     C                   MOVEL     FUDDES        FUN008
116200970526     C                   END
116300970526     C                   END                                                    DO
116400970526     C*
116500970526     C                   ENDSR
116600970526     C*------------------------------------------------------------------------*
116700970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
116800970526     C*------------------------------------------------------------------------*
116900970526     C     OPZGES        BEGSR
117000970526     C*
117100970526     C* PULISCO CAMPI DI LAVORO
117200970526     C                   Z-ADD     0             W                 5 0
117300970526     C*
117400970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
117500970526     C* E CARICO TABELLA OPZIONI ATTIVE
117600991115     C     1             DO        24            I
117700970526     C                   MOVEL     OPD(I)        OPDRIG
117800970526     C*
117900970526     C* PULIZIA OPZIONI PRECEDENTI
118000970526     C                   MOVEL     *BLANKS       OPV(I)
118100970526     C*
118200970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
118300970526     C     OPDATT        IFEQ      'S'
118400970526     C                   ADD       1             W
118500970526     C                   MOVEL     OPDDES        OPV(W)
118600970526     C                   END
118700970526     C                   END                                                    DO WHILE
118800970526     C*
118900970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
119000970526     C* ALTRIMENTI DISATTIVO F23
119100991115     C     1             DO        24            I
119200970526     C                   MOVEL     FUD(I)        FUDRIG
119300970526     C*
119400970526     C     FUDFUN        IFEQ      '23'
119500970526     C     W             IFGT      8
119600970526     C                   MOVEL     'S'           FUDATT
119700970526     C                   ELSE
119800970526     C                   MOVEL     *BLANKS       FUDATT
119900970526     C                   END                                                    W GT 8
120000970526     C                   MOVEL     FUDRIG        FUD(I)
120100970526     C                   END
120200970526     C                   END                                                    DO WHILE
120300970526     C*
120400970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
120500970526     C     W             DIV       8             I
120600970526     C                   MVR                     Z                 5 0
120700970526     C*
120800970526     C     Z             IFNE      0
120900970526     C     I             ADD       1             Z
121000970526     C                   ELSE
121100970526     C                   Z-ADD     I             Z
121200970526     C                   END
121300970526     C*
121400970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
121500970526     C                   ADD       1             OPZGRU
121600970526     C     OPZGRU        IFGT      Z
121700970526     C     OPZGRU        ORLT      1
121800970526     C                   Z-ADD     1             OPZGRU
121900970526     C                   END
122000970526     C*
122100970526     C* PRIMO GRUPPO DI 8 OPZIONI
122200970526     C     OPZGRU        IFEQ      1
122300970526     C                   MOVEL     OPV(1)        OPZ001
122400970526     C                   MOVEL     OPV(2)        OPZ002
122500970526     C                   MOVEL     OPV(3)        OPZ003
122600970526     C                   MOVEL     OPV(4)        OPZ004
122700970526     C                   MOVEL     OPV(5)        OPZ005
122800970526     C                   MOVEL     OPV(6)        OPZ006
122900970526     C                   MOVEL     OPV(7)        OPZ007
123000970526     C                   MOVEL     OPV(8)        OPZ008
123100970526     C                   END
123200970526     C*
123300970526     C* SECONDO GRUPPO DI 8 OPZIONI
123400970526     C     OPZGRU        IFEQ      2
123500970526     C                   MOVEL     OPV(9)        OPZ001
123600970526     C                   MOVEL     OPV(10)       OPZ002
123700970526     C                   MOVEL     OPV(11)       OPZ003
123800970526     C                   MOVEL     OPV(12)       OPZ004
123900970526     C                   MOVEL     OPV(13)       OPZ005
124000970526     C                   MOVEL     OPV(14)       OPZ006
124100970526     C                   MOVEL     OPV(15)       OPZ007
124200970526     C                   MOVEL     OPV(16)       OPZ008
124300970526     C                   END
124400970526     C*
124500970526     C* TERZO GRUPPO DI 8 OPZIONI
124600970526     C     OPZGRU        IFEQ      3
124700970526     C                   MOVEL     OPV(17)       OPZ001
124800970526     C                   MOVEL     OPV(18)       OPZ002
124900970526     C                   MOVEL     OPV(19)       OPZ003
125000970526     C                   MOVEL     OPV(20)       OPZ004
125100970526     C                   MOVEL     OPV(21)       OPZ005
125200970526     C                   MOVEL     OPV(22)       OPZ006
125300970526     C                   MOVEL     OPV(23)       OPZ007
125400970526     C                   MOVEL     OPV(24)       OPZ008
125500970526     C                   END
125600970526     C*
125700970526     C                   ENDSR
125800970526     C*------------------------------------------------------------------------*
125900970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
126000970526     C*------------------------------------------------------------------------*
126100970526     C     OPZCON        BEGSR
126200970526     C*
126300970526     C* PULISCO CAMPI DI LAVORO
126400970526     C                   Z-ADD     0             I                 5 0
126500970526     C                   Z-ADD     0             W                 5 0
126600970526     C*
126700970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
126800970526     C     1             DO        24            I
126900970526     C                   MOVEL     OPD(I)        OPDRIG
127000970526     C*
127100970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
127200970526     C     OPZKEY        IFEQ      OPDOPZ
127300970526     C     OPZKEY        OREQ      OPDOP1
127400970526     C*
127500970526     C     OPDATT        IFEQ      'S'
127600970526     C                   MOVEL     OPDOPZ        OPZKEY
127700970526     C                   Z-ADD     I             W
127800970526     C                   END
127900970526     C                   END
128000970526     C*
128100970526     C                   END                                                    DO WHILE
128200970526     C*
128300970526     C* OPZIONE NON ATTIVA (ERRORE)
128400970526     C     W             IFEQ      0
128500970526     C*
128600970526     C* OPZIONE NON OK
128700970526     C                   Z-ADD     1             OPZOK
128800970526     C*
128900970526     C* EMISSIONE MESSAGGIO DI ERRORE
129000970526     C                   MOVEL     MSG(1)        DSMSMS
129100970526     C                   ELSE
129200970526     C*
129300970526     C* OPZIONE ATTIVA (PROCEDI ...)
129400970526     C*
129500021211     C                   MOVEL     *BLANKS       PMSG
129600000420     C     OPZKEY        IFEQ      ' 1'
129700000420     C                   EXSR      O01GES
129800000420     C                   END
129900000407     C     OPZKEY        IFEQ      ' 2'
130000000407     C                   EXSR      O02GES
130100000407     C                   END
130200991110     C     OPZKEY        IFEQ      ' 4'
130300991110     C                   EXSR      O04GES
130400991014     C                   END
130500970526     C     OPZKEY        IFEQ      ' 5'
130600970526     C                   EXSR      O05GES
130700970526     C                   END
130800030124     C     OPZKEY        IFEQ      ' 7'
130900030124     C                   EXSR      O07GES
131000030124     C                   END
131100021211     C                   MOVEL     PMSG          DSMSMS
131200021211     C                   END                                                    OPZ.NON ATTIVA
131300970526     C*
131400970526     C                   ENDSR
131500970526     C*------------------------------------------------------------------------*
131600970526     C* NOTE COSTRUZIONE SCHIERE
131700970526     C*------------------------------------------------------------------------*
131800970526     C*
131900970526     C* -MSG-MESSAGGI
132000970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
132100970526     C* (NON TOCCARE MSG,1 E MSG,2)
132200970526     C*
132300970526     C* -CMD-COMANDI
132400970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
132500970526     C*
132600970526     C* -OPD-OPZIONI
132700970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
132800970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
132900970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
133000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
133100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
133200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
133300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
133400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
133500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
133600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
133700970526     C*
133800970526     C* -FUD-FUNZIONI
133900970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
134000970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
134100970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
134200970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
134300970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
134400970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
134500970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
134600970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
134700970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
134800970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
134900991108     C*
135000991115     C* -STS-CODICI STATI DEI RECORD DI LOG
135100991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
135200991110     C* ================> IMPORTANTE <=================
135300991115     C* per codici di errore in -STS- indicare in -DES- sempre
135400991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
135500991110     C* ERR
135600991108     C*
135700970526     O*------------------------------------------------------------------------*
135800970526** DEC - DECODIFICHE
135900970526    GESTIONE                                                     1
136000000407 INTERROGAZIONE                                                  2
136100970530     SCELTA                                                      3
1362009705264
1363009705265
136400970526** MSG - MESSAGGI
136500030122TNTB31R- Opzione non gestita.                                                  1
136600030122TNTB31R- Tasto funzionale non gestito.                                         2
136700030122TNTB31R- Utente non abilitato all'inserimento.                                 3
136800030122TNTB31R- Impossibile annullare, record già annullato.                          4
136900030122TNTB31R- 5                                                                     5
137000030122TNTB31R- 6                                                                     6
137100030122TNTB31R- 7                                                                     7
137200030122TNTB31R- 8                                                                     8
137300030122TNTB31R- 9                                                                     9
137400030122TNTB31R- 10                                                                   10
137500030122TNTB31R- 11                                                                   11
137600030122TNTB31R- 12                                                                   12
137700030122TNTB31R- 13                                                                   13
137800030122TNTB31R- 14                                                                   14
137900030122TNTB31R- 15                                                                   15
138000030122TNTB31R- 16                                                                   16
138100030122TNTB31R- 17                                                                   17
138200030122TNTB31R- 18                                                                   18
138300030122TNTB31R- 19                                                                   19
138400030122TNTB31R- 20                                                                   20
138500970528** CMD - COMANDI CL
138600021211
138700021211
138800991110
138900970528
139000970528
139100970526** OPD - OPZIONI
139200000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
139300000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
139400030122                   -  -     (S/N)=N                        opz. 03¬
139500000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
139600000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
139700991115                   -  -     (S/N)=N                        opz. 06¬
139800030124 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
139900991110                   -  -     (S/N)=N                        opz. 08¬
140000991110                   -  -     (S/N)=N                        opz. 09¬
140100991110                   -  -     (S/N)=N                        opz. 10¬
140200000407                   -  -     (S/N)=N                        opz. 11¬
140300970526                   -  -     (S/N)=N                        opz. 12¬
140400970526                   -  -     (S/N)=N                        opz. 13¬
140500000407                   -  -     (S/N)=N                        opz. 14¬
140600000407                   -  -     (S/N)=N                        opz. 15¬
140700970526                   -  -     (S/N)=N                        opz. 16¬
140800970526                   -  -     (S/N)=N                        opz. 17¬
140900970526                   -  -     (S/N)=N                        opz. 18¬
141000970526                   -  -     (S/N)=N                        opz. 19¬
141100970526                   -  -     (S/N)=N                        opz. 20¬
141200970526                   -  -     (S/N)=N                        opz. 21¬
141300970526                   -  -     (S/N)=N                        opz. 22¬
141400970526                   -  -     (S/N)=N                        opz. 23¬
141500000407                   -  -     (S/N)=N                        opz. 25¬
141600970526** FUD - FUNZIONI
141700970526                   -        (S/N)=N                        funz.01¬
141800970526                   -        (S/N)=N                        funz.02¬
141900970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
142000970526                   -        (S/N)=N                        funz.04¬
142100970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
142200000407F06=Inserimento    -        (S/N)=N                        funz.06¬
142300970526                   -        (S/N)=N                        funz.07¬
142400970527                   -        (S/N)=N                        funz.08¬
142500970527                   -        (S/N)=N                        funz.09¬
142600970527                   -        (S/N)=N                        funz.10¬
142700970526                   -        (S/N)=N                        funz.11¬
142800970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
142900991116                   -        (S/N)=N                        funz.13¬
143000970526                   -        (S/N)=N                        funz.14¬
143100970526                   -        (S/N)=N                        funz.15¬
143200970526                   -        (S/N)=N                        funz.16¬
143300970526                   -        (S/N)=N                        funz.17¬
143400970526                   -        (S/N)=N                        funz.18¬
143500970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
143600970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
143700000407                   -        (S/N)=N                        funz.21¬
143800991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
143900991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
144000991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
