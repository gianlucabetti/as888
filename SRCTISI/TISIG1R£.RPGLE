000100970526     H*------------------------------------------------------------------------*
000200000407     H* Gestione file anagrafico Uffici Postali (AZUPI00F)
000300970526     H*------------------------------------------------------------------------*
000400970526     H DECEDIT('0,') DATEDIT(*DMY.)
000500970526     F*------------------------------------------------------------------------*
000600970526     F* DATA BASE
000700970526     F*------------------------------------------------------------------------*
000800000413     FTISP01D   CF   E             WORKSTN INFDS(DEVDS1)
000900000413     F                                     SFILE(SP01D05:SFLULT)
001000000413     F                                     SFILE(SP01DMS:MSGULT)
001100970526     F*------------
001200000407     Fazupi01l  UF   E           K DISK
001300000407     Fazupi02l  UF   E           K DISK
001400000407     F                                     RENAME(azupi000:azupixx2)
001500000407     Fazupi03l  UF   E           K DISK
001600000407     F                                     RENAME(azupi000:azupixx3)
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
002800970528     D*------------
002900970528     D* COMANDI
003000970528     D*------------
003100970528     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
003200970526     D*------------
003300970526     D* DESCRIZIONI OPZIONI
003400970526     D*------------
003500970526     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
003600970526     D*------------
003700970526     D* DESCRIZIONI TASTI FUNZIONALI
003800970526     D*------------
003900970526     D FUD             S             80    DIM(24) CTDATA PERRCD(1)
004000970526     D*------------
004100970526     D* OPZIONI DA CARICARE A VIDEO
004200970526     D*------------
004300970526     D OPV             S             19    DIM(24)
004400970526     D*------------
004500970526     D* FUNZIONI DA CARICARE A VIDEO
004600970526     D*------------
004700970527     D FUV             S             19    DIM(24)
004800991108     D*------------
004900970526     D*------------------------------------------------------------------------*
005000970526     D* INPUT
005100970526     D*------------------------------------------------------------------------*
005200970526     D*------------
005300970526     D* RIDEFINIZIONE OPZIONI VIDEO
005400970526     D*------------
005500970526     D OPDDS           DS
005600970526     D  OPDOPZ                 1      2
005700970526     D  OPDDE1                 3     19
005800970526     D  OPDOP1                21     22
005900970526     D  OPDFI0                23     34
006000970526     D  OPDATT                35     35
006100970526     D  OPDFI1                36     80
006200970526     D  OPDDES                 1     19
006300970526     D  OPDRIG                 1     80
006400970526     D*------------
006500970526     D* RIDEFINIZIONE FUNZIONI VIDEO
006600970526     D*------------
006700970526     D FUDDS           DS
006800970526     D  FUDDE1                 1      1
006900970526     D  FUDFUN                 2      3
007000970526     D  FUDDE2                 4     19
007100970526     D  FUDFI0                20     34
007200970526     D  FUDATT                35     35
007300970526     D  FUDFI1                36     80
007400970526     D  FUDDES                 1     19
007500970526     D  FUDRIG                 1     80
007600970526     D*------------
007700970526     D* DS GESTIONE TASTI FUNZIONALI
007800970526     D*------------
007900970526     D DSKEY         E DS
008000970526     D DEVDS1          DS
008100970526     D  KEY                  369    369
008200970526     D*------------
008300970526     D* DS "XMSG" - GESTIONE MESSAGGI
008400970526     D*------------
008500970526     D DSMSG         E DS
008600970526     D*------------
008700020917     D* DS REPERIMENTO DATI UTENTE
008800970526     D*------------
008900020917     D TIBS34DS      E DS                                                       *Profili utente
009000020917     D DDATIUTE      E DS                                                       *Dati utente
009100020917     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
009200970527     D*------------------
009300991108     D* DS "XSRDA8" - INVERSIONE DATA (8)
009400970527     D*------------------
009500970527     D WLBDA8          DS                  INZ
009600970527     D  G08DAT                 1      8  0
009700970527     D  G08INV                 9     16  0
009800970527     D  G08ERR                17     17
009900970527     D  G08TGI                18     22  0
010000970526     D*------------
010100991110     D* DS DI PROCEDURA
010200970526     D*------------
010300970526     D* ARCHITETTURA
010400970526     D*------------
010500970526     D KPJBA         E DS
010600991108     D*------------------
010700991110     D* DS - SCOMPOSIZIONE MESSAGGIO A VIDEO
010800991110     D*------------------
010900991110     D                 DS                  INZ
011000991110     D  MS1                    1     40
011100991110     D  MS2                   41     80
011200991110     D  MESS                   1     80
011300991108     D*------------------
011400000407     D* PASSAGGIO PARAMETRI
011500000407     D*------------------
011600991110     D PARAM           DS                  INZ
011700000420     D  POP1                   1      3
011800000420     D  PUPI                   4      8  0
011900000420     D  PMSG                   9     88
012000000420     D  POPZ                  89     91
012100970526     C*------------------------------------------------------------------------*
012200970526     C* MAIN LINES
012300970526     C*------------------------------------------------------------------------*
012400970526     C*
012500970526     C* SE NON CI SONO ERRORI, PROSEGUE
012600970526IF  1C     ERRL00        IFNE      '1'
012700970526     C*
012800970526     C* CARICA IL SFILE
012900970526     C                   EXSR      POSSFL
013000970526     C*
013100970526     C* CICLO FINO A CHE NON VIENE PREMUTO F03 O F12
013200970526DO  2C     F03L00        DOUEQ     '1'                                          *F03 PREMUTO
013300970526     C     F12L00        OREQ      '1'                                          *F12 PREMUTO
013400970526     C     FINL00        OREQ      '1'                                          *FINE PGM
013500970526     C*
013600970526     C* REIMPOSTA ERRORE DI PROCERURA
013700970526     C                   MOVEL     '0'           ERRL00
013800970526     C*
013900970526     C* EMETTE IL SFILE
014000970526     C                   EXSR      OUTSFL
014100970526     C*
014200970526     C* GESTIONE TASTI FUNZIONALI
014300970526     C                   EXSR      FUNCON
014400970526     C*
014500970526E   2C                   ENDDO
014600970527     C*
014700970526E   1C                   ENDIF
014800991108     C*
014900970526     C* FINE PGM
015000970526     C                   SETON                                        LR
015100970526     C*------------------------------------------------------------------------*
015200970526     C* F03GES - GESTIONE F03 -> FINE PROGRAMMA
015300970526     C*------------------------------------------------------------------------*
015400970526     C     F03GES        BEGSR
015500970526     C*
015600970526     C* IMPOSTA "PREMUTO F03"
015700970526     C                   MOVEL     '1'           F03L00                         *A PROGRAMMA
015800970526     C*
015900970526     C                   ENDSR
016000970526     C*------------------------------------------------------------------------*
016100970526     C* F05GES - GESTIONE F05 -> RIVISUALIZZA
016200970526     C*------------------------------------------------------------------------*
016300970526     C     F05GES        BEGSR
016400970526     C*
016500970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
016600970526     C*
016700970526     C                   ENDSR
016800970526     C*------------------------------------------------------------------------*
016900970526     C* F06GES - GESTIONE F06 -> INSERIMENTO
017000970526     C*------------------------------------------------------------------------*
017100000407     C     F06GES        BEGSR
017200970526     C*
017300000407     C* INSERIMENTO CONSENTITO SOLO IN SEDE
017400020917IF  1C     DUTLPO        IFNE      'S'
017500000407     C                   MOVEL     '1'           ERRL00
017600000407     C                   MOVEL     MSG(3)        DSMSMS
017700000407E   1C                   ENDIF
017800000407     C*
017900000407IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
018000000407     C                   MOVEL     *zeros        PUPI
018100000407     C                   MOVEL     'F06'         POP1
018200000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
018300000413     C                   CALL      'TISP02R'
018400000407     C                   PARM                    KPJBA
018500000407     C                   EXSR      POSSFL                                       *RICARICA SFILE
018600000407E   1C                   ENDIF
018700000407     C*
018800970526     C                   ENDSR
018900970526     C*------------------------------------------------------------------------*
019000991108     C* F12GES - GESTIONE F12 -> RITORNO
019100970526     C*------------------------------------------------------------------------*
019200970526     C     F12GES        BEGSR
019300970526     C*
019400970526     C* IMPOSTA "PREMUTO F12"
019500970526     C                   MOVEL     '1'           F12L00                         *A PROGRAMMA
019600970526     C*
019700970526     C                   ENDSR
019800970528     C*------------------------------------------------------------------------*
019900970528     C* F21GES - GESTIONE F21 -> STAMPA ELENCO
020000970528     C*------------------------------------------------------------------------*
020100970528     C     F21GES        BEGSR
020200970612     C*
020300991117     C* NON GESTITA
020400970528     C*
020500970528     C                   ENDSR
020600970528     C*------------------------------------------------------------------------*
020700970528     C* F22GES - GESTIONE F22 -> GESTIONE STAMPE
020800970528     C*------------------------------------------------------------------------*
020900970528     C     F22GES        BEGSR
021000970528     C*
021100970528     C                   Z-ADD     80            LENGH            15 5
021200970529     C                   CALL      'QCAEXEC'                                    *LANCIA PGM
021300970528     C                   PARM                    CMD(1)
021400970528     C                   PARM                    LENGH
021500970528     C*
021600970528     C                   ENDSR
021700970526     C*------------------------------------------------------------------------*
021800970526     C* F23GES - GESTIONE F23 -> ALTRE OPZIONI
021900970526     C*------------------------------------------------------------------------*
022000970526     C     F23GES        BEGSR
022100970526     C*
022200970526     C                   EXSR      OPZGES                                       *GESTIONE OPZIONI
022300970526     C*
022400970526     C                   ENDSR
022500970526     C*------------------------------------------------------------------------*
022600970526     C* F24GES - GESTIONE F24 -> ALTRE FUNZIONI
022700970526     C*------------------------------------------------------------------------*
022800970526     C     F24GES        BEGSR
022900970526     C*
023000970526     C                   EXSR      FUNGES                                       *GESTIONE FUNZIONI
023100970526     C*
023200970526     C                   ENDSR
023300970526     C*------------------------------------------------------------------------*
023400970526     C* RUPGES - GESTIONE RU -> PAGINA SUCCESSIVA
023500970526     C*------------------------------------------------------------------------*
023600970526     C     RUPGES        BEGSR
023700970526     C*
023800970526     C                   EXSR      CARSFL                                       *LEGGE DATABASE
023900970526     C*
024000970526     C                   ENDSR
024100970526     C*------------------------------------------------------------------------*
024200991108     C* ENTGES - GESTIONE ENTER -> INVIO
024300970526     C*------------------------------------------------------------------------*
024400970526     C     ENTGES        BEGSR
024500000407     C*
024600000407     C                   MOVEL     '0'           ERRL00
024700991108     C*
024800970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
024900970526     C                   EXSR      NEWPOS
025000970526     C*
025100991108     C* SWPOSI-> 0=NO NUOVO POSIZIONAMENTO, 1=NUOVO POSIZIONAMENTO
025200970526IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
025300970526IF  2C     SWPOSI        IFNE      0
025400970526     C                   EXSR      POSSFL                                       *RICARICA SFILE
025500970526X   2C                   ELSE
025600970526IF  3C     SFLULT        IFNE      0
025700970526     C                   EXSR      ENTATT                                       *GESTIONE ENTER
025800970526     C*
025900970526     C* SWMODI-> 0=NO RICARICA SFILE, 1=RICARICA SFILE
026000970526IF  4C     SWMODI        IFNE      0
026100970526     C                   EXSR      POSSFL
026200970526E   4C                   ENDIF
026300970526E   3C                   ENDIF
026400970526E   2C                   ENDIF
026500970526E   1C                   ENDIF
026600991108     C*
026700970526     C                   ENDSR
026800000420     C*------------------------------------------------------------------------*
026900000420     C* O01GES - GESTIONE OPZIONE 1=SCELTA
027000000420     C*------------------------------------------------------------------------*
027100000420     C     O01GES        BEGSR
027200000420     C*
027300000420     C* RITORNA IL RECORD SELEZIONATO ED ESCE DAL PROGRAMMA
027400000420     C                   MOVEL     VIDUPI        PUPI
027500000420     C                   MOVEL     PARAM         KPJBU
027600000420     C                   MOVEL     '1'           FINL00                         *FINE PGM
027700000420     C*
027800000420     C                   ENDSR
027900000407     C*------------------------------------------------------------------------*
028000000407     C* O02GES - GESTIONE OPZIONE 2=MODIFICA RECORD
028100000407     C*------------------------------------------------------------------------*
028200000407     C     O02GES        BEGSR
028300000407     C*
028400000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
028500000411     C     VIDATB        IFEQ      'A'
028600000411     C                   MOVEL     MSG(4)        DSMSMS
028700000411     C                   ELSE
028800000407     C                   MOVEL     VIDUPI        PUPI
028900000407     C                   MOVEL     'O02'         POP1
029000000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
029100000413     C                   CALL      'TISP02R'
029200000407     C                   PARM                    KPJBA
029300000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
029400000407E   1C                   ENDIF
029500000411     C                   ENDIF
029600000407     C*
029700000407     C                   ENDSR
029800991112     C*------------------------------------------------------------------------*
029900000407     C* O04GES - GESTIONE OPZIONE 4=ANNULLAMENTO RECORD
030000991112     C*------------------------------------------------------------------------*
030100991112     C     O04GES        BEGSR
030200991112     C*
030300000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
030400000411     C     VIDATB        IFEQ      'A'
030500000411     C                   MOVEL     MSG(4)        DSMSMS
030600000411     C                   ELSE
030700000407     C                   MOVEL     VIDUPI        PUPI
030800000407     C                   MOVEL     'O04'         POP1
030900000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
031000000413     C                   CALL      'TISP02R'
031100000407     C                   PARM                    KPJBA
031200000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
031300000407E   1C                   ENDIF
031400000411     C                   ENDIF
031500991112     C*
031600991112     C                   ENDSR
031700991014     C*------------------------------------------------------------------------*
031800000407     C* O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE RECORD
031900970526     C*------------------------------------------------------------------------*
032000970526     C     O05GES        BEGSR
032100000407     C*
032200000407IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
032300000407     C                   MOVEL     VIDUPI        PUPI
032400000407     C                   MOVEL     'O05'         POP1
032500000407     C                   EXSR      PASPAR                                       *RICARICA SFILE
032600000413     C                   CALL      'TISP02R'
032700000407     C                   PARM                    KPJBA
032800000407     C*                  EXSR      POSSFL                                       *RICARICA SFILE
032900000407E   1C                   ENDIF
033000000407     C*
033100970526     C                   ENDSR
033200000411     C*------------------------------------------------------------------------*
033300000411     C* O07GES - GESTIONE OPZIONE 7=RIPRISTINO RECORD
033400000411     C*------------------------------------------------------------------------*
033500000411     C     O07GES        BEGSR
033600000411     C*
033700000411IF  1C     ERRL00        IFEQ      '0'                                          *NO ERRORI
033800000411     C     VIDATB        IFEQ      *BLANKS
033900000411     C                   MOVEL     MSG(5)        DSMSMS
034000000411     C                   ELSE
034100000411     C                   MOVEL     VIDUPI        PUPI
034200000411     C                   MOVEL     'O07'         POP1
034300000411     C                   EXSR      PASPAR                                       *RICARICA SFILE
034400000413     C                   CALL      'TISP02R'
034500000411     C                   PARM                    KPJBA
034600000411     C*                  EXSR      POSSFL                                       *RICARICA SFILE
034700000411E   1C                   ENDIF
034800000411     C                   ENDIF
034900000411     C*
035000000411     C                   ENDSR
035100991028     C*------------------------------------------------------------------------*
035200991112     C* PASPAR - ASSEGNAZIONE VALORI PER PASAGIO PARAMETRI
035300991112     C*------------------------------------------------------------------------*
035400991112     C     PASPAR        BEGSR
035500991112     C*
035600000407     C                   MOVEL     PARAM         KPJBU
035700991112     C*
035800991112     C                   ENDSR
035900970526     C*------------------------------------------------------------------------*
036000970526     C* NEWPOS - CONTROLLA RICHIESTA NUOVO POSIZIONAMENTO
036100970526     C*------------------------------------------------------------------------*
036200970526     C     NEWPOS        BEGSR
036300970526     C*
036400970526     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAMENTO
036500970526     C*         1= RICHIESTO POSIZIONAMENTO
036600970526     C                   Z-ADD     0             SWPOSI            7 0
036700970526     C*
036800991108     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
036900970526     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
037000970526     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
037100970526     C                   Z-ADD     0             SWMODI            1 0
037200970530     C*
037300970530     C* REIMPOSTA INDICATORI DI ERRORE
037400991108     C                   SETOFF                                       808182
037500991108     C                   SETOFF                                       83
037600970526     C*
037700970526     C* CONTROLLA SE RICHIESTO NUOVO POSIZIONAMENTO
037800000407IF  1C     VIDUP1        IFNE      depup1
037900000407     C     VIDCA1        ORNE      depca1
038000000407     C     VIDPR1        ORNE      deppr1
038100000407     C     VIDPR2        ORNE      deppr2
038200970526     C                   EXSR      CHKPAR                                       *CONTROLLA PARAMETRI
038300970526IF  2C     ERRL00        IFEQ      '0'                                          *NO ERORI
038400970526     C                   Z-ADD     1             SWPOSI                         *NUOVO POSIZIONAMENT
038500000407     C                   MOVEL     VIDUP1        depup1                         *DEPOSITI
038600000407     C                   MOVEL     VIDCA1        depca1                         *DEPOSITI
038700000407     C                   MOVEL     VIDPR1        deppr1                         *DEPOSITI
038800000407     C                   MOVEL     VIDPR2        deppr2                         *DEPOSITI
038900970526E   2C                   ENDIF
039000970526E   1C                   ENDIF
039100970526     C*
039200970526     C                   ENDSR
039300970526     C*------------------------------------------------------------------------*
039400970526     C* SETFIL - POSIZIONAMENTO E PRIMA LETTURA RECORD
039500970526     C*------------------------------------------------------------------------*
039600970528     C     SETFIL        BEGSR
039700970526     C*
039800970528     C* POSIZIONAMENTO DEL FILE IN BASE AL TIPO DI RICERCA
039900000407     C     VIDUP1        IFEQ      *zeros
040000000407     C                   MOVEL     *loval        KUPUPI
040100991108     C                   ELSE
040200000407     C                   Z-ADD     VIDUP1        KUPUPI
040300991108     C                   ENDIF
040400000407     C     VIDCA1        IFEQ      *zeros
040500000407     C     VIDCA1        OREQ      *blanks
040600000407     C                   MOVEL     *loval        KUPCAP
040700991108     C                   ELSE
040800000407     C                   MOVEL     VIDCA1        KUPCAP
040900991108     C                   ENDIF
041000000407     C     VIDPR1        IFEQ      *blanks
041100000407     C                   MOVEL     *loval        KUPPRV
041200000407     C                   ELSE
041300000407     C                   MOVEL     VIDPR1        KUPPRV
041400000407     C                   ENDIF
041500000407IF  1C     WTIPRI        IFEQ      'UPI'
041600000407     C     KEYUPI1       SETLL     azupi01l                           99
041700000407E   1C                   ENDIF
041800000407IF  1C     WTIPRI        IFEQ      'PRV'
041900000407     C     KEYUPI2       SETLL     azupi02l                           99
042000000407E   1C                   ENDIF
042100000407IF  1C     WTIPRI        IFEQ      'CAP'
042200000407     C     KEYUPI3       SETLL     azupi03l                           99
042300000407E   1C                   ENDIF
042400980107     C*
042500970526     C* LETTURA RECORD
042600970526IF  1C     *IN99         IFEQ      *OFF
042700970526     C                   EXSR      LETFIL
042800970526E   1C                   ENDIF
042900970526     C*
043000970526     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL
043100970526IF  1C     *IN99         IFEQ      *ON
043200970526     C                   Z-ADD     1             SFLEOF
043300970526E   1C                   ENDIF
043400970526     C*
043500970526     C                   ENDSR
043600970526     C*------------------------------------------------------------------------*
043700970526     C* LETFIL - LETTURA RECORD
043800970526     C*------------------------------------------------------------------------*
043900970526     C     LETFIL        BEGSR
044000970526     C*
044100970526     C* LEGGE FINO A:
044200970526     C*     - FINE FILE
044300970526     C*     - TROVATO RECORD VALIDO
044400970526     C                   MOVEL     'N'           WRECOK                         *RECORD VALIDO
044500970526DO  1C     *IN99         DOWEQ     *OFF
044600970526     C     WRECOK        ANDEQ     'N'
044700970526     C*
044800970528     C* LETTURE SUCCESSIVE ALLA PRIMA IN BASE AL TIPO RICERCA
044900000407IF  2C     WTIPRI        IFEQ      'UPI'                                        *PER UFF. POST.
045000000407     C                   READ      azupi01l                               99
045100970528E   2C                   ENDIF
045200000407IF  2C     WTIPRI        IFEQ      'PRV'                                        *PER PROVINCIA
045300000407     C                   READ      azupi02l                               99
045400991117E   2C                   ENDIF
045500000407IF  2C     WTIPRI        IFEQ      'CAP'                                        *PER CAP
045600000407     C                   READ      azupi03l                               99
045700000407E   2C                   ENDIF
045800970528     C*
045900970526     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL
046000970526IF  2C     *IN99         IFEQ      *ON                                          *FINE FILE
046100970526     C                   Z-ADD     1             SFLEOF                         *FINE LETTURA
046200970530X   2C                   ELSE
046300970526     C                   EXSR      CHKREC                                       *CONTROLLA RECORD
046400970530E   2C                   ENDIF
046500970526E   1C                   ENDDO
046600970526     C*
046700970526     C                   ENDSR
046800970526     C*------------------------------------------------------------------------*
046900970526     C* CHKREC - CONTROLLA VALIDITA' RECORD
047000970526     C*------------------------------------------------------------------------*
047100970526     C     CHKREC        BEGSR
047200970526     C*
047300970526     C                   MOVEL     'S'           WRECOK                         *VALIDITA' RECORD
047400000407     C*filtra se impostata la provincia
047500000407     C     vidpr2        ifeq      *blanks
047600000407X   2C                   else
047700000407     C     upiprv        ifne      vidpr2
047800991108     C                   MOVEL     'N'           WRECOK                         *RECORD NON VALIDO
047900000407E   2C                   endif
048000000407E   2C                   endif
048100991108     C*
048200970526     C                   ENDSR
048300970526     C*------------------------------------------------------------------------*
048400000407     C* IRESFL - IMPOSTA I CAMPI DEL SFILE DAL FILE DI DATA BASE
048500970526     C*------------------------------------------------------------------------*
048600970526     C     IRESFL        BEGSR
048700970526     C*
048800000407     C                   MOVEL     *blanks       OPZKEY
048900000411     C                   MOVEL     upiatb        VIDATB
049000000407     C                   Z-ADD     upiupi        VIDUPI
049100000407     C                   MOVEL     upinom        VIDNOM
049200000407     C                   MOVEL     upiloc        VIDLOC
049300000407     C                   MOVEL     upicap        VIDCAP
049400000407     C                   MOVEL     upiprv        VIDPRV
049500991108     C*
049600970526     C                   ENDSR
049700970526     C*------------------------------------------------------------------------*
049800991108     C* CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
049900970526     C*------------------------------------------------------------------------*
050000970526     C     CHKPAR        BEGSR
050100970528     C*
050200000407     C* SE NESSUN ORDINAMENTO RICHIESTO, UTILIZZA LA RICERCA SECONDO *01L (UPI)
050300000407     C                   MOVEL     'UPI'         WTIPRI            3
050400000407     C*
050500000407     C     VIDPR1        IFEQ      *blanks
050600000407     C     VIDCA1        ANDEQ     *blanks
050700000407     C                   MOVEL     'UPI'         WTIPRI
050800000407     C                   ENDIF                                                  *ORDINAMENTO RICHIES
050900970528     C*
051000970528     C* CONTROLLA IL TIPO RICERCA
051100000407     C     ERRL00        IFEQ      '0'
051200000407     C     VIDPR1        IFNE      *blanks
051300000407     C                   MOVEL     'PRV'         WTIPRI
051400000407     C                   ENDIF
051500000407     C     VIDCA1        IFNE      *blanks
051600000407     C                   MOVEL     'CAP'         WTIPRI
051700000407     C                   ENDIF
051800000407     C                   ENDIF
051900000407     C*
052000970526     C                   ENDSR
052100970526     C*------------------------------------------------------------------------*
052200970526     C* CHKFOP - CONTROLLA IL TIPO DI GESTIONE E IMPOSTA OPZIONI E FUNZIONI
052300970526     C*------------------------------------------------------------------------*
052400970526     C     CHKFOP        BEGSR
052500991112     C*
052600991112     C* OPZIONI
052700970626     C*
052800000420     C* SE L'UTENTE E' IN SCELTA, ABILITA SOLO QUELLA
052900000420IF  1C     DEPOP0        IFEQ      'P01'
053000000420     C                   MOVEL     OPD(1)        OPDRIG                         *SCELTA
053100000420     C                   MOVEL     'S'           OPDATT
053200000420     C                   MOVEL     OPDRIG        OPD(1)
053300000420X   1C                   ELSE
053400000420     C* OPZIONI
053500970626     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
053600970626     C                   MOVEL     'S'           OPDATT
053700970626     C                   MOVEL     OPDRIG        OPD(5)
053800970626E   1C                   ENDIF
053900970626     C*
054000000407     C* SE L'UTENTE E' IN GESTIONE, ABILITA TUTTE LE OPZIONI/FUNZIONI DI GESTIONE
054100970626IF  1C     DEPOP0        IFEQ      'P02'
054200000407     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
054300970626     C                   MOVEL     'S'           OPDATT
054400000407     C                   MOVEL     OPDRIG        OPD(2)
054500000411     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
054600000411     C                   MOVEL     'S'           OPDATT
054700000411     C                   MOVEL     OPDRIG        OPD(4)
054800991112     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
054900991112     C                   MOVEL     'S'           OPDATT
055000991112     C                   MOVEL     OPDRIG        OPD(5)
055100000411     C                   MOVEL     OPD(7)        OPDRIG                         *CANCELLAZIONE
055200000411     C                   MOVEL     'S'           OPDATT
055300000411     C                   MOVEL     OPDRIG        OPD(7)
055400000407     C                   MOVEL     FUD(6)        FUDRIG                         *INTERROGAZIONE
055500000407     C                   MOVEL     'S'           FUDATT
055600000407     C                   MOVEL     FUDRIG        FUD(6)
055700970626E   1C                   ENDIF
055800970626     C*
055900970626     C* SE IN FILIALE NON CONCEDE CANCELLAZIONE, INSERIMENTO, RIPRISTINO
056000020917IF  1C     DUTLPO        IFNE      'S'                                          *FILIALE
056100000407     C                   MOVEL     OPD(2)        OPDRIG                         *NO MODIFICA
056200000407     C                   MOVEL     'N'           OPDATT
056300000407     C                   MOVEL     OPDRIG        OPD(2)
056400991112     C                   MOVEL     OPD(4)        OPDRIG                         *NO CANCELLAZIONE
056500991112     C                   MOVEL     'N'           OPDATT
056600991112     C                   MOVEL     OPDRIG        OPD(4)
056700970626E   1C                   ENDIF
056800970526     C*
056900970526     C* GESTIONE OPZIONI A VIDEO
057000970526     C                   Z-ADD     0             OPZGRU            5 0          *INDICE X OPZIONI
057100970526     C                   EXSR      OPZGES
057200970526     C*
057300970526     C* GESTIONE TASTI FUNZIONALI A VIDEO
057400970526     C                   Z-ADD     *ZEROS        FUNGRU            5 0          *INDICE X FUNZIONI
057500970526     C                   EXSR      FUNGES
057600970526     C*
057700970526     C                   ENDSR
057800020917     C*--------------------------------------------------------------------------------------------*
057900020917     C* REPERISCE I DATI UTENTE
058000020917     C*--------------------------------------------------------------------------------------------*
058100020917     C     REPDATIUTE    BEGSR
058200020917     C*
058300020917     C* INIZIALIZZA VARIABILI DI WRK
058400020917     C                   CLEAR                   TIBS34DS
058500020917     C                   CLEAR                   AZUTEDS
058600020917     C                   CLEAR                   DDATIUTE
058700020917     C*
058800020917     C     *DTAARA       DEFINE    §azute        azuteds
058900020917     C     *DTAARA       DEFINE    §datiute      ddatiute
059000020917     C                   IN(E)     *DTAARA
059100020917if  1C                   IF        %Error
059200020917     c                   EVAL      I34Tla = 'L'
059300020917     C                   CALL      'TIBS34R'
059400020917     C                   PARM                    Tibs34Ds
059500020917     C                   IN        *DTAARA
059600020917e   1C                   ENDIF
059700020917     C*
059800020917     C* ASSEGNO LA DESCRIZIONE DEL S.I./UTENTE AL CAMPO DEL VIDEO
059900020917     C                   MOVEL(P)  RSUT          DSFIRS           20
060000020917     C*
060100020917     C                   ENDSR
060200970526     C*------------------------------------------------------------------------*
060300970526     C* *INZSR - OPERAZIONI INIZIALI
060400970526     C*------------------------------------------------------------------------*
060500970526     C     *INZSR        BEGSR
060600970526     C*---
060700970526     C* RICEVIMENTO PARAMETRI
060800970526     C*---
060900970526     C     *ENTRY        PLIST
061000970612     C                   PARM                    KPJBA
061100000420     C                   MOVEL     kpjbu         PARAM
061200000420     C     POPZ          IFEQ      'P01'
061300000420     C                   MOVEL     POPZ          DEPOP0            3
061400000420     C                   ELSE
061500000420     C                   MOVEL     POP1          DEPOP0            3
061600000420     C                   ENDIF
061700970526     C*
061800970526     C* DS PARAMETRI PER "XMSG" - MESSAGGI PGM
061900970526     C     XMSGPA        PLIST
062000970526     C                   PARM                    DSMSMS
062100970526     C                   PARM                    DSMSPG
062200970526     C                   PARM                    DSMSKE
062300970526     C                   PARM                    DSMSER
062400970526     C*
062500970526     C* IMPOSTA LA DS -TASTI PREMUTI-
062600970526     C                   CALL      'XKEY'
062700970526     C                   PARM                    DSKEY
062800970526     C*---
062900970526     C* VARIABILI NON RIFERITE AL DATA BASE
063000970526     C*---
063100970526     C     *LIKE         DEFINE    SFLNBR        SFLPAG
063200970526     C     *LIKE         DEFINE    SFLPAG        WFLPAG
063300970526     C     *LIKE         DEFINE    SFLNBR        SFLEOF
063400970526     C     *LIKE         DEFINE    SFLNBR        SFLERR
063500970526     C     *LIKE         DEFINE    SFLNBR        SFLULT
063600970526     C     *LIKE         DEFINE    SFLNBR        WFLULT
063700970526     C     *LIKE         DEFINE    SFLNBR        MSGULT
063800970526     C     *LIKE         DEFINE    SFLNBR        SFLRIG
063900970526     C     *LIKE         DEFINE    FUDFUN        FUNKEY
064000970526     C*---
064100000407     C     *LIKE         DEFINE    vidup1        depup1
064200000407     C     *LIKE         DEFINE    vidpr1        deppr1
064300000407     C     *LIKE         DEFINE    vidca1        depca1
064400000407     C     *LIKE         DEFINE    vidpr2        deppr2
064500000407     C     *LIKE         DEFINE    upiupi        kupupi
064600000407     C     *LIKE         DEFINE    upiprv        kupprv
064700000407     C     *LIKE         DEFINE    upicap        kupcap
064800991108     C*---
064900970526     C* VARIABILI DI CONTROLLO
065000970526     C*---
065100970526     C                   MOVEL     '0'           FINL00            1            *FINE PGM
065200970526     C                   MOVEL     '0'           F03L00            1            *PREMUTO F03
065300970526     C                   MOVEL     '0'           F12L00            1            *PREMUTO F12
065400970526     C                   MOVEL     '0'           ERRL00            1            *ERRORE
065500970526     C                   MOVEL     'S'           WRECOK            1            *VALIDITA' RECORD
065600000407     C                   Z-ADD     10            SFLPAG                         *N°RIGHE X PAG
065700970526     C*
065800970526     C* IMPOSTA LA DS -INVIO MESSAGGI-
065900000413     C                   MOVEL     'TISP01R'     DSMSPG
066000000413     C                   MOVEL     'TISP01R'     MSGQUE
066100970526     C                   MOVEL     *BLANKS       MSGKEY
066200970526     C                   MOVEL     *BLANKS       DSMSMS
066300970526     C                   MOVEL     *BLANKS       DSMSKE
066400970526     C                   Z-ADD     *ZEROS        DSMSER
066500970526     C*---
066600970526     C* CHIAVI DI LETTURA
066700970526     C*---
066800000407     C* LETTURA azupi01l -
066900000407     C     KEYUPI1       KLIST
067000000407     C                   KFLD                    KUPUPI
067100000407     C* LETTURA azupi02l -
067200000407     C     KEYUPI2       KLIST
067300000407     C                   KFLD                    KUPPRV
067400000407     C                   KFLD                    KUPUPI
067500000407     C* LETTURA azupi03l -
067600000407     C     KEYUPI3       KLIST
067700000407     C                   KFLD                    KUPCAP
067800000407     C                   KFLD                    KUPUPI
067900991108     C*
068000970527     C* REPERIMENTO DATI UTENTE
068100020917     C                   EXSR      REPDATIUTE
068200970526     C*
068300970526     C* IMPOSTA I PARAMETRI DI INGRESSO
068400970526     C                   EXSR      CHKPAR
068500991115     C*
068600970526     C* IMPOSTA LE OPZIONI E FUNZIONI A VIDEO
068700970526     C                   EXSR      CHKFOP
068800970526     C*
068900970530     C* IMPOSTA IL TIPO DI GESTIONE IN TESTATA
069000970530IF  1C     DEPOP0        IFEQ      'P01'
069100970530     C                   MOVEL     DEC(3)        VIDING
069200970530E   1C                   ENDIF
069300970530IF  1C     DEPOP0        IFEQ      'P02'
069400970526     C                   MOVEL     DEC(1)        VIDING
069500970530E   1C                   ENDIF
069600970530IF  1C     DEPOP0        IFEQ      'P05'
069700970526     C                   MOVEL     DEC(2)        VIDING
069800970526E   1C                   ENDIF
069900970526     C*
070000970526     C                   ENDSR
070100970526     C*------------------------------------------------------------------------*
070200970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070300970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070400970526     C*  LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
070500970526     C*  STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
070600970526     C*  NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
070700970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070800970526     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
070900970526     C*------------------------------------------------------------------------*
071000970526     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA CARICARE
071100970526     C*------------------------------------------------------------------------*
071200970526     C     POSSFL        BEGSR
071300970526     C*
071400970526     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
071500970526     C                   EXSR      CLRSFL
071600970526     C*
071700970526     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
071800970526     C                   EXSR      SETFIL
071900970526     C*
072000970526     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
072100970526     C     SFLEOF        IFEQ      0
072200970526     C                   EXSR      CARSFL
072300970526     C                   END
072400970526     C*
072500970526     C                   ENDSR
072600970526     C*------------------------------------------------------------------------*
072700970526     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
072800970526     C*------------------------------------------------------------------------*
072900970526     C     ENTATT        BEGSR
073000970526     C*
073100970526     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
073200970526     C                   Z-ADD     0             OPZOK             1 0
073300970526     C*
073400970526     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
073500970526     C                   Z-ADD     0             SFLVAR            5 0
073600970526     C*
073700970526     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
073800970526     C                   Z-ADD     0             SFLERR
073900970526     C*
074000970526     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
074100970526     C                   Z-ADD     SFLULT        WFLULT
074200970526     C*
074300970526     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
074400000413     C                   READC     SP01D05                                98
074500970526     C     *IN98         DOWEQ     '0'
074600970526     C*
074700970526     C* ADD CONTATORE DI RIGHE SFL VARIATE
074800970526     C                   ADD       1             SFLVAR
074900970526     C*
075000970526     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
075100970526     C     OPZOK         IFEQ      0
075200970526     C*
075300970526     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
075400970526     C     OPZKEY        IFNE      *BLANKS
075500970526     C                   EXSR      OPZCON
075600970526     C                   END
075700970526     C*
075800970526     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
075900970526     C*            2. PULIZIA SCELTA IN SFL
076000970526     C     OPZOK         IFEQ      0
076100970526     C                   MOVEL     '0'           *IN41
076200970526     C                   MOVEL     *BLANKS       OPZKEY
076300970526     C                   ELSE
076400970526     C*
076500970526     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
076600970526     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
076700970526     C                   MOVEL     '1'           *IN41
076800970526     C                   Z-ADD     SFLULT        SFLERR
076900970526     C                   END
077000970526     C                   ELSE
077100970526     C*
077200970526     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
077300970526     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
077400970526     C                   MOVEL     '1'           *IN41
077500970526     C                   END                                                    OPZOK=0
077600970526     C*
077700970526     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
077800970526     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
077900970526     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
078000970526     C     OPZOK         IFNE      0
078100970526     C                   Z-ADD     SFLERR        SFLNBR
078200970526     C                   ELSE
078300970526     C                   Z-ADD     SFLULT        SFLNBR
078400970526     C                   END
078500970526     C*
078600970526     C* AGGIORNO RECORD SFL
078700000413     C                   UPDATE    SP01D05
078800970526     C*
078900000413     C                   READC     SP01D05                                98
079000970526     C                   END                                                    DO WHILE
079100970526     C*
079200970526     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
079300970526     C                   Z-ADD     WFLULT        SFLULT
079400970526     C*
079500970526     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
079600970526     C     SFLVAR        IFEQ      0
079700970526     C                   END
079800970526     C*
079900991115     C     OPZKEY        IFNE      ' 5'
080000991115     C                   EXSR      F05GES
080100991115     C                   ENDIF
080200991112     C*
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
081800000413     C                   WRITE     SP01D06
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
084300991116     C                   MOVEL     *BLANKS       OPZKEY
084400970526     C*
084500970526     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
084600970526     C                   EXSR      IRESFL
084700970526     C*
084800970526     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
084900970526     C                   ADD       1             SFLULT
085000970526     C*
085100970526     C* SCRITTURA RECORD SFL
085200000413     C                   WRITE     SP01D05
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
087200000413     C                   WRITE     SP01D01
087300970526     C*
087400970526     C* VISUALIZZAZIONE MESSAGGIO
087500970526     C                   EXSR      SNDMSG
087600970526     C*
087700970526     C* WRITE RECORD OPZIONI VIDEO
087800000413     C                   WRITE     SP01D02
087900970526     C*
088000970526     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
088100000413     C                   WRITE     SP01D03
088200970526     C*
088300970526     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
088400000413     C                   WRITE     SP01D04
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
089700000413     C                   WRITE     SP01D09
089800970526     C*
089900970526     C* CASO DI SFL VUOTO
090000970526     C     SFLULT        IFLE      0
090100970526     C*
090200970526     C*    - WRITE RECORD VUOTO
090300000413     C                   WRITE     SP01D0A
090400000413     C                   READ      SP01D0A                                99
090500970526     C*
090600970526     C                   ELSE
090700970526     C*
090800970526     C* CASO DI SFL CON RECORDS
090900970526     C*
091000970526     C*    - WRITE RECORD CONTROLLO SFL
091100000413     C                   WRITE     SP01D06
091200000413     C                   READ      SP01D06                                99
091300970526     C                   END
091400970526     C*
091500970526     C* READ RECORD PER POSIZIONAMEMTO VIDEO
091600000413     C                   READ      SP01D03                                99
091700970526     C*
091800970526     C                   ENDSR
091900970526     C*------------------------------------------------------------------------*
092000970526     C* SNDMSG - MANDA MESSAGGIO
092100970526     C*------------------------------------------------------------------------*
092200970526     C     SNDMSG        BEGSR
092300970526     C*
092400970526     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
092500970526     C                   MOVEL     '1'           *IN40
092600000413     C                   WRITE     SP01DMC
092700970526     C                   MOVEL     '0'           *IN40
092800970526     C*
092900970526     C* VISUALIZZAZIONE MESSAGGIO
093000970526     C                   Z-ADD     0             MSGULT
093100970526     C                   CALL      'XMSG'        XMSGPA
093200970526     C     DSMSER        IFEQ      0
093300970526     C                   MOVEL     DSMSKE        MSGKEY
093400970526     C                   END
093500970526     C                   Z-ADD     1             MSGULT
093600000413     C                   WRITE     SP01DMS                                      SFL MESS
093700000413     C                   WRITE     SP01DMC                                      CONTR MESS
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
099400970528     C     FUNKEY        IFEQ      '21'
099500970528     C                   EXSR      F21GES
099600970528     C                   END
099700970528     C     FUNKEY        IFEQ      '22'
099800970528     C                   EXSR      F22GES
099900970528     C                   END
100000970526     C     FUNKEY        IFEQ      '23'
100100970526     C                   EXSR      F23GES
100200970526     C                   END
100300970526     C     FUNKEY        IFEQ      '24'
100400970526     C                   EXSR      F24GES
100500970526     C                   END
100600970526     C     FUNKEY        IFEQ      'RU'
100700970526     C                   EXSR      RUPGES
100800970526     C                   END
100900970526     C*
101000970526     C                   END
101100970526     C*
101200970526     C                   ENDSR
101300970526     C*------------------------------------------------------------------------*
101400970526     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
101500970526     C*------------------------------------------------------------------------*
101600970526     C     FUNVAR        BEGSR
101700970526     C*
101800970526     C* FUNKEY=*BLANKS SIGNIFICA ENTER
101900970526     C     KEY           IFEQ      ENTER
102000970526     C                   MOVEL     *BLANKS       FUNKEY
102100970526     C                   END
102200970526     C     KEY           IFEQ      F1
102300970526     C                   MOVEL     '01'          FUNKEY
102400970526     C                   END
102500970526     C     KEY           IFEQ      F2
102600970526     C                   MOVEL     '02'          FUNKEY
102700970526     C                   END
102800970526     C     KEY           IFEQ      F3
102900970526     C                   MOVEL     '03'          FUNKEY
103000970526     C                   END
103100970526     C     KEY           IFEQ      F4
103200970526     C                   MOVEL     '04'          FUNKEY
103300970526     C                   END
103400970526     C     KEY           IFEQ      F5
103500970526     C                   MOVEL     '05'          FUNKEY
103600970526     C                   END
103700970526     C     KEY           IFEQ      F6
103800970526     C                   MOVEL     '06'          FUNKEY
103900970526     C                   END
104000970526     C     KEY           IFEQ      F7
104100970526     C                   MOVEL     '07'          FUNKEY
104200970526     C                   END
104300970526     C     KEY           IFEQ      F8
104400970526     C                   MOVEL     '08'          FUNKEY
104500970526     C                   END
104600970526     C     KEY           IFEQ      F9
104700970526     C                   MOVEL     '09'          FUNKEY
104800970526     C                   END
104900970526     C     KEY           IFEQ      F10
105000970526     C                   MOVEL     '10'          FUNKEY
105100970526     C                   END
105200970526     C     KEY           IFEQ      F11
105300970526     C                   MOVEL     '11'          FUNKEY
105400970526     C                   END
105500970526     C     KEY           IFEQ      F12
105600970526     C                   MOVEL     '12'          FUNKEY
105700970526     C                   END
105800970526     C     KEY           IFEQ      F13
105900970526     C                   MOVEL     '13'          FUNKEY
106000970526     C                   END
106100970526     C     KEY           IFEQ      F14
106200970526     C                   MOVEL     '14'          FUNKEY
106300970526     C                   END
106400970526     C     KEY           IFEQ      F15
106500970526     C                   MOVEL     '15'          FUNKEY
106600970526     C                   END
106700970526     C     KEY           IFEQ      F16
106800970526     C                   MOVEL     '16'          FUNKEY
106900970526     C                   END
107000970526     C     KEY           IFEQ      F17
107100970526     C                   MOVEL     '17'          FUNKEY
107200970526     C                   END
107300970526     C     KEY           IFEQ      F18
107400970526     C                   MOVEL     '18'          FUNKEY
107500970526     C                   END
107600970526     C     KEY           IFEQ      F19
107700970526     C                   MOVEL     '19'          FUNKEY
107800970526     C                   END
107900970526     C     KEY           IFEQ      F20
108000970526     C                   MOVEL     '20'          FUNKEY
108100970526     C                   END
108200970526     C     KEY           IFEQ      F21
108300970526     C                   MOVEL     '21'          FUNKEY
108400970526     C                   END
108500970526     C     KEY           IFEQ      F22
108600970526     C                   MOVEL     '22'          FUNKEY
108700970526     C                   END
108800970526     C     KEY           IFEQ      F23
108900970526     C                   MOVEL     '23'          FUNKEY
109000970526     C                   END
109100970526     C     KEY           IFEQ      F24
109200970526     C                   MOVEL     '24'          FUNKEY
109300970526     C                   END
109400970526     C     KEY           IFEQ      ROLLUP
109500970526     C                   MOVEL     'RU'          FUNKEY
109600970526     C                   END
109700970526     C     KEY           IFEQ      HELP
109800970526     C                   MOVEL     'HP'          FUNKEY
109900970526     C                   END
110000970526     C*
110100970526     C                   ENDSR
110200970526     C*------------------------------------------------------------------------*
110300970526     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
110400970526     C*------------------------------------------------------------------------*
110500970526     C     FUNGES        BEGSR
110600970526     C*
110700970526     C* PULISCO CAMPI DI LAVORO
110800970526     C                   Z-ADD     0             W                 5 0
110900970526     C*
111000970526     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
111100970526     C* E CARICO TABELLA FUNZIONI ATTIVE
111200970526     C     1             DO        24            I
111300970526     C                   MOVEL     FUD(I)        FUDRIG
111400970526     C*
111500970526     C* PULIZIA FUNZIONI PRECEDENTI
111600970526     C                   MOVEL     *BLANKS       FUV(I)
111700970526     C*
111800970526     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
111900970526     C* (SALTA F23 E F24)
112000970526     C     FUDATT        IFEQ      'S'
112100970526     C     FUDFUN        ANDNE     '23'
112200970526     C     FUDFUN        ANDNE     '24'
112300970526     C                   ADD       1             W
112400970526     C                   MOVEL     FUDDES        FUV(W)
112500970526     C                   END
112600970526     C                   END                                                    DO WHILE
112700970526     C*
112800970526     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
112900970526     C* ALTRIMENTI DISATTIVO F24
113000970526     C     1             DO        24            I
113100970526     C                   MOVEL     FUD(I)        FUDRIG
113200970526     C*
113300970526     C     FUDFUN        IFEQ      '24'
113400970526     C     W             IFGT      6
113500970526     C                   MOVEL     'S'           FUDATT
113600970526     C                   ELSE
113700970526     C                   MOVEL     *BLANKS       FUDATT
113800970526     C                   END                                                    W GT 8
113900970526     C                   MOVEL     FUDRIG        FUD(I)
114000970526     C                   END
114100970526     C                   END                                                    DO WHILE
114200970526     C*
114300970526     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
114400970526     C     W             DIV       6             I
114500970526     C                   MVR                     Z
114600970526     C*
114700970526     C     Z             IFNE      0
114800970526     C     I             ADD       1             Z
114900970526     C                   ELSE
115000970526     C                   Z-ADD     I             Z
115100970526     C                   END
115200970526     C*
115300970526     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
115400970526     C                   ADD       1             FUNGRU
115500970526     C     FUNGRU        IFGT      Z
115600970526     C     FUNGRU        ORLT      1
115700970526     C                   Z-ADD     1             FUNGRU
115800970526     C                   END
115900970526     C*
116000970526     C* PRIMO GRUPPO DI 6 FUNZIONI
116100970526     C     FUNGRU        IFEQ      1
116200970526     C                   MOVEL     FUV(1)        FUN001
116300970526     C                   MOVEL     FUV(2)        FUN002
116400970526     C                   MOVEL     FUV(3)        FUN003
116500970526     C                   MOVEL     FUV(4)        FUN004
116600970526     C                   MOVEL     FUV(5)        FUN005
116700970526     C                   MOVEL     FUV(6)        FUN006
116800970526     C                   END
116900970526     C*
117000970526     C* SECONDO GRUPPO DI 6 FUNZIONI
117100970526     C     FUNGRU        IFEQ      2
117200970526     C                   MOVEL     FUV(7)        FUN001
117300970526     C                   MOVEL     FUV(8)        FUN002
117400970526     C                   MOVEL     FUV(9)        FUN003
117500970526     C                   MOVEL     FUV(10)       FUN004
117600970526     C                   MOVEL     FUV(11)       FUN005
117700970526     C                   MOVEL     FUV(12)       FUN006
117800970526     C                   END
117900970526     C*
118000970526     C* TERZO GRUPPO DI 6 FUNZIONI
118100970526     C     FUNGRU        IFEQ      3
118200970526     C                   MOVEL     FUV(13)       FUN001
118300970526     C                   MOVEL     FUV(14)       FUN002
118400970526     C                   MOVEL     FUV(15)       FUN003
118500970526     C                   MOVEL     FUV(16)       FUN004
118600970526     C                   MOVEL     FUV(17)       FUN005
118700970526     C                   MOVEL     FUV(18)       FUN006
118800970526     C                   END
118900970526     C*
119000970526     C* QUARTO GRUPPO DI 6 FUNZIONI
119100970526     C     FUNGRU        IFEQ      4
119200970526     C                   MOVEL     FUV(19)       FUN001
119300970526     C                   MOVEL     FUV(20)       FUN002
119400970526     C                   MOVEL     FUV(21)       FUN003
119500970526     C                   MOVEL     FUV(22)       FUN004
119600970526     C                   MOVEL     *BLANKS       FUN005
119700970526     C                   MOVEL     *BLANKS       FUN006
119800970526     C                   END
119900970526     C*
120000970526     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
120100970526     C                   MOVEL     *BLANKS       FUN007
120200970526     C                   MOVEL     *BLANKS       FUN008
120300970526     C*
120400970526     C     1             DO        24            I
120500970526     C                   MOVEL     FUD(I)        FUDRIG
120600970526     C     FUDFUN        IFEQ      '23'
120700970526     C     FUDATT        ANDEQ     'S'
120800970526     C                   MOVEL     FUDDES        FUN007
120900970526     C                   END
121000970526     C     FUDFUN        IFEQ      '24'
121100970526     C     FUDATT        ANDEQ     'S'
121200970526     C                   MOVEL     FUDDES        FUN008
121300970526     C                   END
121400970526     C                   END                                                    DO
121500970526     C*
121600970526     C                   ENDSR
121700970526     C*------------------------------------------------------------------------*
121800970526     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
121900970526     C*------------------------------------------------------------------------*
122000970526     C     OPZGES        BEGSR
122100970526     C*
122200970526     C* PULISCO CAMPI DI LAVORO
122300970526     C                   Z-ADD     0             W                 5 0
122400970526     C*
122500970526     C* RICERCO QUANTE OPZIONI SONO ATTIVE
122600970526     C* E CARICO TABELLA OPZIONI ATTIVE
122700991115     C     1             DO        24            I
122800970526     C                   MOVEL     OPD(I)        OPDRIG
122900970526     C*
123000970526     C* PULIZIA OPZIONI PRECEDENTI
123100970526     C                   MOVEL     *BLANKS       OPV(I)
123200970526     C*
123300970526     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
123400970526     C     OPDATT        IFEQ      'S'
123500970526     C                   ADD       1             W
123600970526     C                   MOVEL     OPDDES        OPV(W)
123700970526     C                   END
123800970526     C                   END                                                    DO WHILE
123900970526     C*
124000970526     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
124100970526     C* ALTRIMENTI DISATTIVO F23
124200991115     C     1             DO        24            I
124300970526     C                   MOVEL     FUD(I)        FUDRIG
124400970526     C*
124500970526     C     FUDFUN        IFEQ      '23'
124600970526     C     W             IFGT      8
124700970526     C                   MOVEL     'S'           FUDATT
124800970526     C                   ELSE
124900970526     C                   MOVEL     *BLANKS       FUDATT
125000970526     C                   END                                                    W GT 8
125100970526     C                   MOVEL     FUDRIG        FUD(I)
125200970526     C                   END
125300970526     C                   END                                                    DO WHILE
125400970526     C*
125500970526     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
125600970526     C     W             DIV       8             I
125700970526     C                   MVR                     Z                 5 0
125800970526     C*
125900970526     C     Z             IFNE      0
126000970526     C     I             ADD       1             Z
126100970526     C                   ELSE
126200970526     C                   Z-ADD     I             Z
126300970526     C                   END
126400970526     C*
126500970526     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
126600970526     C                   ADD       1             OPZGRU
126700970526     C     OPZGRU        IFGT      Z
126800970526     C     OPZGRU        ORLT      1
126900970526     C                   Z-ADD     1             OPZGRU
127000970526     C                   END
127100970526     C*
127200970526     C* PRIMO GRUPPO DI 8 OPZIONI
127300970526     C     OPZGRU        IFEQ      1
127400970526     C                   MOVEL     OPV(1)        OPZ001
127500970526     C                   MOVEL     OPV(2)        OPZ002
127600970526     C                   MOVEL     OPV(3)        OPZ003
127700970526     C                   MOVEL     OPV(4)        OPZ004
127800970526     C                   MOVEL     OPV(5)        OPZ005
127900970526     C                   MOVEL     OPV(6)        OPZ006
128000970526     C                   MOVEL     OPV(7)        OPZ007
128100970526     C                   MOVEL     OPV(8)        OPZ008
128200970526     C                   END
128300970526     C*
128400970526     C* SECONDO GRUPPO DI 8 OPZIONI
128500970526     C     OPZGRU        IFEQ      2
128600970526     C                   MOVEL     OPV(9)        OPZ001
128700970526     C                   MOVEL     OPV(10)       OPZ002
128800970526     C                   MOVEL     OPV(11)       OPZ003
128900970526     C                   MOVEL     OPV(12)       OPZ004
129000970526     C                   MOVEL     OPV(13)       OPZ005
129100970526     C                   MOVEL     OPV(14)       OPZ006
129200970526     C                   MOVEL     OPV(15)       OPZ007
129300970526     C                   MOVEL     OPV(16)       OPZ008
129400970526     C                   END
129500970526     C*
129600970526     C* TERZO GRUPPO DI 8 OPZIONI
129700970526     C     OPZGRU        IFEQ      3
129800970526     C                   MOVEL     OPV(17)       OPZ001
129900970526     C                   MOVEL     OPV(18)       OPZ002
130000970526     C                   MOVEL     OPV(19)       OPZ003
130100970526     C                   MOVEL     OPV(20)       OPZ004
130200970526     C                   MOVEL     OPV(21)       OPZ005
130300970526     C                   MOVEL     OPV(22)       OPZ006
130400970526     C                   MOVEL     OPV(23)       OPZ007
130500970526     C                   MOVEL     OPV(24)       OPZ008
130600970526     C                   END
130700970526     C*
130800970526     C                   ENDSR
130900970526     C*------------------------------------------------------------------------*
131000970526     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
131100970526     C*------------------------------------------------------------------------*
131200970526     C     OPZCON        BEGSR
131300970526     C*
131400970526     C* PULISCO CAMPI DI LAVORO
131500970526     C                   Z-ADD     0             I                 5 0
131600970526     C                   Z-ADD     0             W                 5 0
131700970526     C*
131800970526     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
131900970526     C     1             DO        24            I
132000970526     C                   MOVEL     OPD(I)        OPDRIG
132100970526     C*
132200970526     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
132300970526     C     OPZKEY        IFEQ      OPDOPZ
132400970526     C     OPZKEY        OREQ      OPDOP1
132500970526     C*
132600970526     C     OPDATT        IFEQ      'S'
132700970526     C                   MOVEL     OPDOPZ        OPZKEY
132800970526     C                   Z-ADD     I             W
132900970526     C                   END
133000970526     C                   END
133100970526     C*
133200970526     C                   END                                                    DO WHILE
133300970526     C*
133400970526     C* OPZIONE NON ATTIVA (ERRORE)
133500970526     C     W             IFEQ      0
133600970526     C*
133700970526     C* OPZIONE NON OK
133800970526     C                   Z-ADD     1             OPZOK
133900970526     C*
134000970526     C* EMISSIONE MESSAGGIO DI ERRORE
134100970526     C                   MOVEL     MSG(1)        DSMSMS
134200970526     C                   ELSE
134300970526     C*
134400970526     C* OPZIONE ATTIVA (PROCEDI ...)
134500970526     C*
134600000420     C     OPZKEY        IFEQ      ' 1'
134700000420     C                   EXSR      O01GES
134800000420     C                   END
134900000407     C     OPZKEY        IFEQ      ' 2'
135000000407     C                   EXSR      O02GES
135100000407     C                   END
135200991110     C     OPZKEY        IFEQ      ' 4'
135300991110     C                   EXSR      O04GES
135400991014     C                   END
135500970526     C     OPZKEY        IFEQ      ' 5'
135600970526     C                   EXSR      O05GES
135700970526     C                   END
135800000411     C     OPZKEY        IFEQ      ' 7'
135900000411     C                   EXSR      O07GES
136000000411     C                   END
136100970526     C                   END                                                    OPZ.NON ATTIVA
136200970526     C*
136300970526     C                   ENDSR
136400970526     C*------------------------------------------------------------------------*
136500970526     C* NOTE COSTRUZIONE SCHIERE
136600970526     C*------------------------------------------------------------------------*
136700970526     C*
136800970526     C* -MSG-MESSAGGI
136900970526     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
137000970526     C* (NON TOCCARE MSG,1 E MSG,2)
137100970526     C*
137200970526     C* -CMD-COMANDI
137300970526     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
137400970526     C*
137500970526     C* -OPD-OPZIONI
137600970526     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
137700970526     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
137800970526     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
137900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
138000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
138100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
138200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
138300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
138400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
138500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
138600970526     C*
138700970526     C* -FUD-FUNZIONI
138800970526     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
138900970526     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
139000970526     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
139100970526     C*  . POSSONO ESSERE IN ORDINE SPARSO
139200970526     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
139300970526     C*    VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO
139400970526     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA
139500970526     C*    VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
139600970526     C*  . METTERE F23 E F24 CON S/N=S (RICALCOLATE DAL PGM)
139700970526     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
139800991108     C*
139900991115     C* -STS-CODICI STATI DEI RECORD DI LOG
140000991110     C* -DES-DESCRIZIONI STATI DEI RECORD DI LOG
140100991110     C* ================> IMPORTANTE <=================
140200991115     C* per codici di errore in -STS- indicare in -DES- sempre
140300991110     C* la dicitura "Err", altrimenti coreggere specifica marcata
140400991110     C* ERR
140500991108     C*
140600970526     O*------------------------------------------------------------------------*
140700970526** DEC - DECODIFICHE
140800970526    GESTIONE                                                     1
140900000407 INTERROGAZIONE                                                  2
141000970530     SCELTA                                                      3
1411009705264
1412009705265
141300970526** MSG - MESSAGGI
141400000413TISP01R- Opzione non gestita.                                                  1
141500000413TISP01R- Tasto funzionale non gestito.                                         2
141600000413TISP01R- Non abilitato all inserimento in filiale.                             3
141700000413TISP01R- Record già annullato, consentito solo visualizz. e ripristino         4
141800000413TISP01R- Record non annullato, impossibile ripristinare                        5
141900000413TISP01R- Accettato solo un parametro di inizio lista                           6
142000000413TISP01R- 7                                                                     7
142100000413TISP01R- 8                                                                     8
142200000413TISP01R- 9                                                                     9
142300000413TISP01R- 10                                                                   10
142400000413TISP01R- 11                                                                   11
142500000413TISP01R- 12                                                                   12
142600000413TISP01R- 13                                                                   13
142700000413TISP01R- 14                                                                   14
142800000413TISP01R- 15                                                                   15
142900000413TISP01R- 16                                                                   16
143000000413TISP01R- 17                                                                   17
143100000413TISP01R- 18                                                                   18
143200000413TISP01R- 19                                                                   19
143300000413TISP01R- 20                                                                   20
143400970528** CMD - COMANDI CL
143500991110DSPPFM FILE(
143600991110) MBR(
143700991110
143800970528
143900970528
144000970526** OPD - OPZIONI
144100000420 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
144200000407 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
144300991110                   -  -     (S/N)=N                        opz. 03¬
144400000407 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
144500000407 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
144600991115                   -  -     (S/N)=N                        opz. 06¬
144700000411 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
144800991110                   -  -     (S/N)=N                        opz. 08¬
144900991110                   -  -     (S/N)=N                        opz. 09¬
145000991110                   -  -     (S/N)=N                        opz. 10¬
145100000407                   -  -     (S/N)=N                        opz. 11¬
145200970526                   -  -     (S/N)=N                        opz. 12¬
145300970526                   -  -     (S/N)=N                        opz. 13¬
145400000407                   -  -     (S/N)=N                        opz. 14¬
145500000407                   -  -     (S/N)=N                        opz. 15¬
145600970526                   -  -     (S/N)=N                        opz. 16¬
145700970526                   -  -     (S/N)=N                        opz. 17¬
145800970526                   -  -     (S/N)=N                        opz. 18¬
145900970526                   -  -     (S/N)=N                        opz. 19¬
146000970526                   -  -     (S/N)=N                        opz. 20¬
146100970526                   -  -     (S/N)=N                        opz. 21¬
146200970526                   -  -     (S/N)=N                        opz. 22¬
146300970526                   -  -     (S/N)=N                        opz. 23¬
146400000407                   -  -     (S/N)=N                        opz. 25¬
146500970526** FUD - FUNZIONI
146600970526                   -        (S/N)=N                        funz.01¬
146700970526                   -        (S/N)=N                        funz.02¬
146800970526F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
146900970526                   -        (S/N)=N                        funz.04¬
147000970526F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
147100000407F06=Inserimento    -        (S/N)=N                        funz.06¬
147200970526                   -        (S/N)=N                        funz.07¬
147300970527                   -        (S/N)=N                        funz.08¬
147400970527                   -        (S/N)=N                        funz.09¬
147500970527                   -        (S/N)=N                        funz.10¬
147600970526                   -        (S/N)=N                        funz.11¬
147700970526F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
147800991116                   -        (S/N)=N                        funz.13¬
147900970526                   -        (S/N)=N                        funz.14¬
148000970526                   -        (S/N)=N                        funz.15¬
148100970526                   -        (S/N)=N                        funz.16¬
148200970526                   -        (S/N)=N                        funz.17¬
148300970526                   -        (S/N)=N                        funz.18¬
148400970526 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.19¬
148500970526 RD=Pagina giù     -        (S/N)=S  <== S-FISSO           funz.20¬
148600000407                   -        (S/N)=N                        funz.21¬
148700991110F22=Gestione stampe-        (S/N)=N                        funz.22¬
148800991110F23=Altre opzioni  -        (S/N)=N  <== S-FISSO           funz.23¬
148900991110F24=Altri tasti    -        (S/N)=N  <== S-FISSO           funz.24¬
