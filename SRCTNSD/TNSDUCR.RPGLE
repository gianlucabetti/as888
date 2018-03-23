000100020322     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020322      *------------------------------------------------------------------------*
000300020322      * DOTAZIONI P.O. UNITA' EDP - SCELTA
000400020322      *------------------------------------------------------------------------*
000500020322
000600020322      *------------------------------------------------------------------------*
000700020322      * DATA BASE
000800020322      *------------------------------------------------------------------------*
000900000000     FTNSDUCD   CF   E             WORKSTN INFDS(DEVDS1)
001000000000     F                                     SFILE(SDUCD05:SFLULT)
001100000000     F                                     SFILE(SDUCDMS:MSGULT)
001200020710     funana01c  if   e           k disk
001300020710     funana02c  if   e           k disk    rename(Unana000:unana02)
001400020710     f                                     rename(Undot000:undot02)
001500020322     Ftntbe01l  IF   E           K DISK
001600020322
001700000000     D*------------------------------------------------------------------------*
001800000000     D* SCHIERE
001900000000     D*------------------------------------------------------------------------*
002000000000     D* MESSAGGI
002100000000     D MSG             S             78    DIM(20) CTDATA PERRCD(1)
002200000000     D* COMANDI
002300000000     D CMD             S             80    DIM(5) CTDATA PERRCD(1)
002400000000     D* DESCRIZIONI OPZIONI
002500000000     D OPD             S             80    DIM(24) CTDATA PERRCD(1)
002600000000     D* DESCRIZIONI TASTI FUNZIONALI
002700100922     D FUD             S             80    DIM(25) CTDATA PERRCD(1)
002800000000     D* TABELLA OPZIONI DA CARICARE A VIDEO
002900000000     D OPV             S             19    DIM(24)
003000000000     D* TABELLA TASTI FUNZIONALI DA CARICARE A VIDEO
003100000000     D FUV             S             19    DIM(24)
003200000000     D* FILIALI E LORO DECODIFICHE
003300000000     D* TIPI UNITA E LORO DECODIFICHE
003400000000     D UNI             S              2  0 DIM(100)
003500000000     D DUNI            S             25    DIM(100)
003600000000     D*------------------------------------------------------------------------*
003700000000     D* INPUT
003800000000     D*------------------------------------------------------------------------*
003900020326
004000020711     d WokUni          s              1    inz('0')
004100020711     d WokDot          s              1    inz('0')
004200020326     d wnosfl          s              1    inz('0')
004300020326     d wunmag          s                   like(unamag)
004400020326     d wuntip          s                   like(unatip)
004500020711     d SavMag          s                   Like(UnaMag)
004600020711     d SavTip          s                   Like(UnaTip)
004700020711     d SavAtb          s                   Like(DotAtb)
004800020711     d SavQta          s                   Like(DotQta)
004900020326     d depdot          s                   like(vdpdot)
005000020405     d depmag          s                   like(vdpmag)
005100020326     d kdofil          s                   like(dotfil)
005200100930     d wmag            s              3
005300100930     d wdmag           s             20
005400020326
005500000000     D*----------------
005600000000     D* RIDEFINIZIONE SCHIERA -OPD-
005700000000     D*----------------
005800000000     D OPDDS           DS
005900000000     D*                -OPZIONE PRINCIPALE
006000000000     D  OPDOPZ                 1      2
006100000000     D*                -DESCRIZIONE PARZIALE
006200000000     D  OPDDE1                 3     19
006300000000     D*                -OPZIONE ALTERNATIVA
006400000000     D  OPDOP1                21     22
006500000000     D*                -FILLER
006600000000     D  OPDFI0                23     34
006700000000     D*                -FLAG OPZ. ATTIVA (S/N)
006800000000     D  OPDATT                35     35
006900000000     D*                -FILLER
007000000000     D  OPDFI1                36     80
007100000000     D*                -DESCRIZIONE COMPLETA
007200000000     D  OPDDES                 1     19
007300000000     D*                -RIDEFINIZIONE RIGA
007400000000     D  OPDRIG                 1     80
007500000000     D*----------------
007600000000     D* RIDEFINIZIONE SCHIERA -FUD-
007700000000     D*----------------
007800000000     D FUDDS           DS
007900000000     D*                -DESCRIZIONE PARZIALE
008000000000     D  FUDDE1                 1      1
008100000000     D*                -FUNZIONE
008200000000     D  FUDFUN                 2      3
008300000000     D*                -DESCRIZIONE PARZIALE
008400000000     D  FUDDE2                 4     19
008500000000     D*                -FILLER
008600000000     D  FUDFI0                20     34
008700000000     D*                -FLAG FUNZ.ATTIVA (S/N)
008800000000     D  FUDATT                35     35
008900000000     D*                -FILLER
009000000000     D  FUDFI1                36     80
009100000000     D*                -DESCRIZIONE COMPLETA
009200000000     D  FUDDES                 1     19
009300000000     D*                -RIDEFINIZIONE RIGA
009400000000     D  FUDRIG                 1     80
009500000000     D*----------------
009600000000     D* PARAMETRI CHIAMATA CL XMSG (GESTIONE MESSAGGI AL PGM)
009700000000     D*----------------
009800000000     D DSMSG         E DS
009900000000     D*----------------
010000000000     D* PARAMETRI PER LANCIO ALTRI PGM DELLA PROCEDURA
010100000000     D*----------------
010200000000     D DSUNIT        E DS
010300000000     D*----------------
010400000000     D* DS PER GESTIONE TASTI FUNZIONALI
010500000000     D*----------------
010600000000     D DSKEY         E DS
010700000000     D DEVDS1          DS
010800000000     D  KEY                  369    369
010900000000     D*----------------
011000000000     D* ARCHITETTURA
011100000000     D*----------------
011200000000     D KPJBA         E DS
011300020709
011400020709     d ddatiute      e ds
011500020709     d azuteds       e ds                  extname(azute00f)
011600020709     d tibs34ds      e ds                  inz
011700100930     d tnsdmagds     e ds                  inz
011800020710      *------------------------------------------------------------------------*
011900020710     Iunana000      01
012000020710     Iundot000      02
012100020710     Iunana02       03
012200020710     Iundot02       04
012300000000     C*------------------------------------------------------------------------*
012400000000     C* MAIN LINE
012500000000     C*------------------------------------------------------------------------*
012600000000     C*
012700000000     C* SE CONTROLLI PRELIMINARI NON OK VADO A FINE PGM
012800000000     C     ERRL00        IFNE      '1'                                          --- 1 -->
012900000000     C*
013000000000     C* CARICA IL SFL DAI PARAMETRI IN INPUT AL PROGRAMMA
013100000000     C                   EXSR      POSSFL
013200000000     C*
013300000000     C* CICLO FINO A CHE:
013400000000     C*  -F03 NON VIENE ACCESO (IN QUESTO PGM)                 OR
013500000000     C*  -F12 NON VIENE ACCESO (IN QUESTO PGM)                 OR
013600000000     C*  -FINE PGM PER ENTER SENZA SCELTA
013700000000     C     F03L00        DOUEQ     '1'                                          --- 2 -->
013800000000     C     F12L00        OREQ      '1'
013900000000     C*
014000000000     C* AZZERA FLAG DI ERROE
014100000000     C                   MOVEL     '0'           ERRL00
014200020717      * Se Profilo non OK devo uscire dal pgm
014300020717     c                   If        *In76
014400020717     c                   eval      f03l00 = *on
014500020717     c                   EndIf
014600000000     C*
014700000000     C* EMETTE SFL A VIDEO
014800000000     C                   EXSR      OUTSFL
014900000000     C*
015000000000     C* CONTROLLO E GESTIONE TASTI FUNZIONALI -ROUTINE FUNCON
015100000000     C                   EXSR      FUNCON
015200000000     C*
015300000000     C                   END                                                    <-- 2 ---
015400000000     C*
015500000000     C                   END                                                    <-- 1 --
015600000000     C*
015700000000     C                   MOVEL     DSUNIT        KPJBU
015800000000     C                   SETON                                        LR
015900000000     C*================================================================
016000000000     C* F03GES -GEST.TASTO FUNZ. F03 ===> FINE
016100000000     C*================================================================
016200000000     C     F03GES        BEGSR
016300000000     C*
016400000000     C* IMPOSTO FLAG DI PREMUTO TASTO F03 (A LIVELLO PGM 00)
016500000000     C                   MOVEL     '1'           F03L00
016600000000     C*
016700000000     C                   ENDSR
016800000000     C*================================================================
016900000000     C* F05GES -GEST.TASTO FUNZ. F05 ===> RIVISUALIZZAZIONE
017000000000     C*================================================================
017100000000     C     F05GES        BEGSR
017200000000     C*
017300000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
017400000000     C                   EXSR      POSSFL
017500000000     C*
017600000000     C                   ENDSR
017700000000     C*================================================================
017800000000     C* F06GES -GEST.TASTO FUNZ. F06 ===> INSERIMENTO
017900000000     C*================================================================
018000000000     C     F06GES        BEGSR
018100000000     C*
018200000000     C* INSERIMENTO CONSENTITO SOLO IN SEDE
018300020709     c                   If        DutLpo <> 'S'
018400000000     C                   MOVEL     '1'           ERRL00
018500000000     C                   MOVEL     MSG(3)        DSMSMS
018600000000     C                   END
018700000000     C*
018800000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
018900000000     C*
019000000000     C* LANCIO IL PGM PER L'INSERIMENTO DEL RECORD (TNSDUDR)
019100000000     C                   MOVEL     'F06'         WOPZ1             3
019200000000     C                   MOVEL     'TNSDUDR'     WPGM              8
019300000000     C                   EXSR      LANPGM
019400000000     C*
019500000000     C* LANCIA LA ROUTINE DI CARICAMENTO DEL SUBFILE
019600000000     C                   EXSR      POSSFL
019700000000     C*
019800000000     C                   END                                                    <-- 1 ---
019900000000     C*
020000000000     C                   ENDSR
020100000000     C*================================================================
020200000000     C* F12GES -GEST.TASTO FUNZ. F12 ===> RITORNO
020300000000     C*================================================================
020400000000     C     F12GES        BEGSR
020500000000     C*
020600000000     C* IMPOSTO FLAG DI PREMUTO TASTO F12 (A LIVELLO PGM 00)
020700000000     C                   MOVEL     '1'           F12L00
020800000000     C*
020900000000     C                   ENDSR
021000000000     C*================================================================
021100000000     C* F17GES -GEST.TASTO FUNZ. F17 ===> INIZIO DATI
021200000000     C*================================================================
021300000000     C     F17GES        BEGSR
021400000000     C*
021500000000     C* IMPOSTA SFLRCDNBR PER PRIMA PAGINA
021600000000     C                   Z-ADD     1             SFLNBR
021700000000     C*
021800000000     C                   ENDSR
021900000000     C*================================================================
022000000000     C* F18GES -GEST.TASTO FUNZ. F18 ===> FINE DATI
022100000000     C*================================================================
022200000000     C     F18GES        BEGSR
022300000000     C*
022400000000     C* IMPOSTA SFLRCDNBR PER ULTIMA PAGINA
022500000000     C                   Z-ADD     SFLULT        SFLNBR
022600000000     C*
022700000000     C                   ENDSR
022800000000     C*================================================================
022900000000     C* F21GES -GEST.TASTO FUNZ. F21 ===> STAMPA DIFFERENZE
023000000000     C*================================================================
023100000000     C     F21GES        BEGSR
023200000000     C*
023300000000     C* LANCIA PROGRAMMA DI STAMPA DELLE UNITA
023400000000     C                   MOVEL     DSUNIT        KPJBU
023500000000     C                   CALL      'TNSDUER'
023600000000     C                   PARM                    KPJBA
023700000000     C                   MOVEL     KPJBU         DSUNIT
023800000000     C*
023900000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
024000000000     C     DSUF03        IFEQ      '1'
024100000000     C                   MOVEL     '1'           F03L00
024200000000     C                   MOVEL     '1'           ERRL00
024300000000     C                   ENDIF
024400000000     C*
024500000000     C                   ENDSR
024600000000     C*================================================================
024700000000     C* F22GES -GEST.TASTO FUNZ. F22 ===> STAMPE
024800000000     C*================================================================
024900000000     C     F22GES        BEGSR
025000000000     C*
025100000000     C* ESEGUE IL COMANDO INDICATO NELLA SCHIERA CMD,1
025200000000     C                   Z-ADD     80            LENGH            15 5
025300000000     C                   CALL      'QCAEXEC'
025400000000     C                   PARM                    CMD(1)
025500000000     C                   PARM                    LENGH
025600000000     C*
025700000000     C                   ENDSR
025800000000     C*================================================================
025900000000     C* F23GES -GEST.TASTO FUNZ. F23 ===> ALTRE OPZIONI
026000000000     C*================================================================
026100000000     C     F23GES        BEGSR
026200000000     C*
026300000000     C* GESTIONE VISUALIZZAZIONE OPZIONI
026400000000     C                   EXSR      OPZGES
026500000000     C*
026600000000     C                   ENDSR
026700000000     C*================================================================
026800000000     C* F24GES -GEST.TASTO FUNZ. F24 ===> ALTRI TASTI
026900000000     C*================================================================
027000000000     C     F24GES        BEGSR
027100000000     C*
027200000000     C* GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI
027300000000     C                   EXSR      FUNGES
027400000000     C*
027500000000     C                   ENDSR
027600000000     C*================================================================
027700000000     C* RUPGES -GEST.TASTO FUNZ. ROLLUP ===> PAGINA SUCCESSIVA
027800000000     C*================================================================
027900000000     C     RUPGES        BEGSR
028000000000     C*
028100000000     C* CARICA IL SFL DA FILE "FISICO"
028200000000     C                   EXSR      CARSFL
028300000000     C*
028400000000     C                   ENDSR
028500000000     C*================================================================
028600000000     C* HELGES - GEST.TASTO FUNZ. HELP ===> AIUTO
028700000000     C*================================================================
028800000000     C     HELGES        BEGSR
028900000000     C*
029000000000     C                   ENDSR
029100000000     C*================================================================
029200000000     C* ENTGES - GEST.TASTO FUNZ. ENTER ===> INVIO
029300000000     C*================================================================
029400000000     C     ENTGES        BEGSR
029500000000     C*
029600000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO FILE DA CARICARE
029700000000     C                   EXSR      NEWPOS
029800000000     C*
029900000000     C* SE I PARAMETRI DI POSIZIONAMENTO SONO ERRATI NON RIEMETTE IL SUBFILE
030000000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
030100000000     C*
030200000000     C* SWPOSI: 0=NO NEW POSIZION.   1=NEW POSIZION.
030300000000     C     SWPOSI        IFNE      0                                            --- 2 -->
030400000000     C                   EXSR      POSSFL
030500000000     C*
030600000000     C                   ELSE                                                   <-- 2 -->
030700000000     C*
030800000000     C* GESTIONE SFL CON TASTO FUNZIONALE ENTER
030900000000     C     SFLULT        IFNE      0                                            --- 3 -->
031000000000     C                   EXSR      ENTATT
031100000000     C*
031200000000     C* SWMODI: 0=NON RICARICO IL SUBFILE   1=RICARICO IL SUBFILE
031300000000     C     SWMODI        IFNE      0                                            --- 4 -->
031400000000     C                   EXSR      POSSFL
031500000000     C                   END                                                    <-- 4 ---
031600000000     C*
031700000000     C                   END                                                    <-- 3 ---
031800000000     C                   END                                                    <-- 2 ---
031900000000     C                   END                                                    <-- 1 ---
032000000000     C*
032100000000     C                   ENDSR
032200000000     C*================================================================
032300000000     C*  O01GES - GESTIONE OPZIONE 1=SCELTA
032400000000     C*================================================================
032500000000     C     O01GES        BEGSR
032600000000     C                   ENDSR
032700000000     C*================================================================
032800000000     C*  O02GES - GESTIONE OPZIONE 2=MODIFICA
032900000000     C*================================================================
033000000000     C     O02GES        BEGSR
033100000000     C*
033200000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA MODIFICA
033300000000     C     VIDATB        IFEQ      'A'
033400000000     C                   MOVEL     '1'           ERRL00
033500000000     C                   MOVEL     MSG(4)        DSMSMS
033600000000     C                   END
033700000000     C*
033800000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
033900000000     C*
034000000000     C* LANCIO IL PGM PER LA MODIFICA DEL DETTAGLIO DELL'ANAGRAFICA
034100000000     C                   MOVEL     'O02'         WOPZ1
034200000000     C                   MOVEL     'TNSDUDR'     WPGM
034300000000     C                   EXSR      LANPGM
034400000000     C*
034500000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
034600000000     C                   Z-ADD     1             SWMODI
034700000000     C*
034800000000     C                   END                                                    <-- 1 ---
034900000000     C*
035000000000     C                   ENDSR
035100000000     C*================================================================
035200000000     C*  O03GES - GESTIONE OPZIONE 3=COPIA
035300000000     C*================================================================
035400000000     C     O03GES        BEGSR
035500000000     C*
035600000000     C                   ENDSR
035700000000     C*================================================================
035800000000     C*  O04GES - GESTIONE OPZIONE 4=CANCELLAZIONE
035900000000     C*================================================================
036000000000     C     O04GES        BEGSR
036100000000     C*
036200000000     C* SE IL RECORD E' ANNULLATO NON CONSENTO LA CANCELLAZIONE
036300000000     C     VIDATB        IFEQ      'A'
036400000000     C                   MOVEL     '1'           ERRL00
036500000000     C                   MOVEL     MSG(4)        DSMSMS
036600000000     C                   END
036700000000     C*
036800000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
036900000000     C*
037000000000     C* LANCIO IL PGM DI GESTIONE DELLE UNITA
037100000000     C                   MOVEL     'O04'         WOPZ1
037200000000     C                   MOVEL     'TNSDUDR'     WPGM
037300000000     C                   EXSR      LANPGM
037400000000     C*
037500000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
037600000000     C                   Z-ADD     1             SWMODI
037700000000     C*
037800000000     C                   END                                                    <-- 1 ---
037900000000     C*
038000000000     C                   ENDSR
038100000000     C*================================================================
038200000000     C*  O05GES - GESTIONE OPZIONE 5=VISUALIZZAZIONE
038300000000     C*================================================================
038400000000     C     O05GES        BEGSR
038500000000     C*
038600000000     C* LANCIO ROUTINE PER LA VISUALIZZAZIONE DEL DETTAGLIO DELLA RIGA
038700000000     C                   MOVEL     'O05'         WOPZ1
038800000000     C                   MOVEL     'TNSDUDR'     WPGM
038900000000     C                   EXSR      LANPGM
039000000000     C*
039100000000     C                   ENDSR
039200000000     C*================================================================
039300000000     C*  O06GES - GESTIONE OPZIONE 6=STAMPA
039400000000     C*================================================================
039500000000     C     O06GES        BEGSR
039600000000     C                   ENDSR
039700000000     C*================================================================
039800000000     C*  O07GES - GESTIONE OPZIONE 7=RIPRISTINO
039900000000     C*================================================================
040000000000     C     O07GES        BEGSR
040100000000     C*
040200000000     C* SE IL RECORD NON E' ANNULLATO NON CONSENTO IL RIPRISTINO
040300000000     C     VIDATB        IFNE      'A'
040400000000     C                   MOVEL     '1'           ERRL00
040500000000     C                   MOVEL     MSG(5)        DSMSMS
040600000000     C                   END
040700000000     C*
040800000000     C     ERRL00        IFEQ      '0'                                          --- 1 -->
040900000000     C*
041000000000     C* LANCIO IL PGM PER IL RIPRISTINO DI UN ANNULLAMENTO
041100000000     C                   MOVEL     'O07'         WOPZ1
041200000000     C                   MOVEL     'TNSDUDR'     WPGM
041300000000     C                   EXSR      LANPGM
041400000000     C*
041500000000     C* IMPOSTO LO SWITCH PER RICARICARE IL SUBFILE
041600000000     C                   Z-ADD     1             SWMODI
041700000000     C*
041800000000     C                   END                                                    <-- 1 ---
041900000000     C*
042000000000     C                   ENDSR
042100000000     C*================================================================
042200000000     C* NEWPOS - CONTROLLO DELLA RICHIESTA DI NUOVO POSIZIONAMENTO
042300000000     C*================================================================
042400000000     C     NEWPOS        BEGSR
042500000000     C*
042600000000     C* SWPOSI: 0= NESSUNA RICHIESTA DI POSIZIONAM.
042700000000     C*         1= RICHIESTO POSIZIONAM.
042800000000     C                   Z-ADD     0             SWPOSI            7 0
042900000000     C*
043000000000     C* SWMODI: 0= NESSUN AGGIORNAMENTO AFFETTUATO SUL FILE DALL'UTENTE
043100000000     C*            CON L'OPZIONE 2, NON OCCORRE RICARICARE IL SUBFILE
043200000000     C*         1= AGGIORNAMENTI EFFETTUATI, OCCORRE RICARICARE
043300000000     C                   Z-ADD     0             SWMODI            1 0
043400100930     C
043500100930     C                   IF        VDPMAG=*ZEROS
043600100930     C                   CLEAR                   VDPMAG
043700100930     C                   ENDIF
043800000000     C*
043900000000     C* CONTROLLO SE RICHIESTO NUOVO POSIZIONAMENTO
044000020405     c                   if        vdptip <> deptip or vdpdot <> depdot or      --- 1 -->
044100020405     c                             vdpmag <> depmag
044200000000     C*
044300000000     C* E' RICHIESTO POSIZIONAMENTO, CONTROLLO I PARAMETRI DI PARTENZA
044400000000     C                   EXSR      CHKPAR
044500000000     C*
044600000000     C* SE I PARAMETRI SONO OK, LI MEMORIZZO E IMPOSTO FLAG PARAM.MODIF.
044700000000     C     ERRL00        IFEQ      '0'                                          --- 2 -->
044800000000     C                   Z-ADD     1             SWPOSI
044900000000     C                   Z-ADD     VDPTIP        DEPTIP
045000020326     c                   eval      depdot = vdpdot
045100020405     c                   eval      depmag = vdpmag
045200000000     C                   END                                                    <-- 2 ---
045300000000     C*
045400000000     C                   END                                                    <-- 1 ---
045500000000     C*
045600000000     C                   ENDSR
045700000000     C*================================================================
045800000000     C* LETTURA RECORD FILE DA CARICARE
045900000000     C*================================================================
046000000000     C     LETFIL        BEGSR
046100020711
046200020711     c                   eval      *In01 = *off
046300020711     c                   eval      *In02 = *off
046400020711     c                   eval      *In03 = *off
046500020711     c                   eval      *In04 = *off
046600000000     C*
046700000000     C     WTIPRI        IFEQ      'TIP'
046800020710     C                   READ      unana01c                               99
046900000000     C                   END
047000020405
047100020405     c                   if        wtipri = 'MAG'
047200020710     c     kanmag        reade     unana02c                               99
047300020405     c                   endif
047400020711
047500020711     c                   If        Not *In99
047600020711
047700020711     c                   If        *In01 or *In03
047800020711     c                   Eval      SavTip = UnaTip
047900020711     c                   Eval      SavMag = UnaMag
048000020711     c                   EndIf
048100020711     c                   If        *In02 or *In04
048200020711     c                   Eval      SavTip = DotTip
048300020711     c                   Eval      SavMag = DotFil
048400020711     c                   EndIf
048500020711
048600020711
048700020711     c                   EndIf
048800020405
048900000000     C*
049000000000     C* SE FINE FILE, IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
049100000000     C     *IN99         IFEQ      '1'
049200000000     C                   Z-ADD     1             SFLEOF
049300000000     C                   END
049400000000     C*
049500000000     C                   ENDSR
049600000000     C*================================================================
049700000000     C* SETFIL - SETLL DEL FILE DA CARICARE
049800000000     C*================================================================
049900000000     C     SETFIL        BEGSR
050000020711
050100020711     c                   eval      *In01 = *off
050200020711     c                   eval      *In02 = *off
050300020711     c                   eval      *In03 = *off
050400020711     c                   eval      *In04 = *off
050500000000     C*
050600000000     C* POSIZIONAMENTO DEL FILE DA CARICARE E PRIMA LETTURA
050700000000     C     WTIPRI        IFEQ      'TIP'
050800020326     c                   eval      kantip = vdptip
050900020710     c     KaNtip        SETLL     unana01c                           99        *FINE-ARCHIV
051000020710     c  N99              READ      unana01c                               99
051100000000     C                   END
051200020405
051300020405     c                   if        wtipri = 'MAG'
051400020405     c                   move      vdpmag        kanmag
051500020710     c     kanmag        setll     unana02c                           99
051600020710     c  n99kanmag        reade     unana02c                               99
051700020405     c                   endif
051800020322
051900020322     c                   if        not *in99
052000020711
052100020711     c                   If        *In01 or *In03
052200020711     c                   If        WunTip = *Zeros
052300020711     c                   Eval      WunTip = UnaTip
052400020711     c                   EndIf
052500020711     c                   If        WunMag = *Zeros
052600020711     c                   Eval      WunMag = UnaMag
052700020711     c                   EndIf
052800020711     c                   Eval      SavTip = UnaTip
052900020711     c                   Eval      SavMag = UnaMag
053000020711     c                   EndIf
053100020711
053200020711     c                   If        *In02 or *In04
053300020711     c                   If        WunTip = *Zeros
053400020711     c                   Eval      WunTip = DotTip
053500020711     c                   EndIf
053600020711     c                   If        WunMag = *Zeros
053700020711     c                   Eval      WunMag = DotFil
053800020711     c                   EndIf
053900020711     c                   Eval      SavTip = DotTip
054000020711     c                   Eval      SavMag = DotFil
054100020711     c                   EndIf
054200020711
054300020322     c                   endif
054400020326
054500000000     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
054600000000     C     *IN99         IFEQ      '1'                                          --- 1 -->
054700000000     C                   Z-ADD     1             SFLEOF
054800000000     C                   END                                                    <-- 1 ---
054900000000     C*
055000000000     C                   ENDSR
055100020326      *================================================================
055200020326      * CTRSFL - CONTROLLO SE RECORD DA SCRIVERE NEL SUBFILE
055300020326      *================================================================
055400020326     C     CTRSFL        BEGSR
055500020326
055600020711     c                   If        VdpDot = 'S' and
055700020711     c                             WokUni = *On and WokDot = *On
055800020711     c                   Eval      WnoSfl = *On
055900020711     c                   Endif
056000020711     c                   goto      passa
056100020326      * controllo se richieste solo unità NON in dotazione
056200020403     c                   if        (vdpdot = 'S' and vidqta <> 0) or
056300020403     c                             (vdpdot = 'S' and viddif = 0  and
056400020403     c                              vidqta = 0)
056500020326     c                   eval      wnosfl = *on
056600020326     c                   endif
056700020711     c     passa         tag
056800020326
056900020326     C                   ENDSR
057000000000     C*================================================================
057100000000     C* IRESFL - IMPOSTA I CAMPI DEL RECORD DEL SFL DAL FILE FISICO
057200000000     C*================================================================
057300000000     C     IRESFL        BEGSR
057400000000     C*
057500000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
057600020322     C                   Z-ADD     wunTIP        VIDTIP
057700020322     C                   Z-ADD     wunmag        VIDFIL
057800020326
057900020322
058000020711     C                   movel     Savatb        VIDatb
058100020711     C                   Z-ADD     SavQTA        VIDQTA
058200950124     C*
058300950124     C* CALCOLA LA DIFFERENZA FRA LE QUANTITA' IN DOTAZIONE E QUELLE IN ESSERE
058400950124     C                   Z-ADD     *ZEROS        VIDDIF
058500020711     C     TOTFIL        SUB       SavQta        VIDDIF
058600000000     C* DECODIFICHE
058700000000     C                   Z-ADD     1             J
058800000000     C                   SETOFF                                       28
058900020326     C     wuntip        LOOKUP    UNI(J)                                 28
059000000000     C     *IN28         IFEQ      *ON
059100000000     C                   MOVEL     DUNI(J)       VIDTID
059200000000     C                   ELSE
059300000000     C                   MOVEL     *ALL'*'       VIDTID
059400000000     C                   ENDIF
059500000000     C*
059600100930     c                   movel     wunmag        wmag
059700100930     c                   exsr      chkmag
059800100930     c                   movel     wdmag         vidfid
059900000000     C*
060000000000     C                   ENDSR
060100000000     C*================================================================
060200000000     C*  CHKPAR - CONTROLLA I PARAMETRI DI INIZIO RICERCA
060300000000     C*================================================================
060400000000     C     CHKPAR        BEGSR
060500020405
060600020405      * IMPOSTO FLAG EVIDENZAZIONE TIPO DI RICERCA
060700020405     c                   eval       *in90 = *off                                x tipo
060800020405     c                   eval       *in91 = *off                                x magazzino
060900000000     C*
061000000000     C* IMPOSTO FLAG EVIDENZAZIONE ERRORI SUI PARAMETRI
061100020405     C                   SETOFF                                       80        tipo
061200020405     c                   eval       *in81 = *off                                magazzino
061300000000     C*
061400000000     C* IMPOSTO A BLANK LE DECODIFICHE
061500000000     C                   MOVEL     *BLANKS       VDPTID
061600000000     C*
061700000000     C* SE TUTTI I PARAMETRI SONO UGUALI A BLANK O ZERO, UTILIZZO CHIAVE X TIP
061800100930     c                   if        vdptip = *zeros and (vdpmag=*blanks or
061900100930     c                             vdpmag=*zeros)
062000000000     C                   MOVEL     'TIP'         WTIPRI            3
062100000000     C                   SETON                                        90
062200000000     C*
062300000000X   1C                   ELSE
062400000000     C*
062500000000     C                   MOVEL     *BLANKS       WTIPRI
062600000000     C* ===> ACCETTO SOLO UN PARAMETRO DI INIZIO RICERCA
062700000000     C* CONTROLLO E DECODIFICO IL TIPO UNITA
062800000000IF  2C     VDPTIP        IFNE      *ZEROS
062900000000     C                   Z-ADD     1             J
063000000000     C                   SETOFF                                           28
063100000000     C     VDPTIP        LOOKUP    UNI(J)                                 28
063200000000IF  3C     *IN28         IFEQ      *OFF
063300000000     C                   MOVEL     *ALL'*'       VDPTID
063400000000     C                   MOVEL     '1'           ERRL00
063500000000     C                   MOVEL     MSG(7)        DSMSMS
063600000000     C                   SETON                                        80
063700000000     C                   ELSE
063800000000     C                   MOVEL     DUNI(J)       VDPTID
063900000000     C                   MOVEL     'TIP'         WTIPRI
064000020405     C                   SETON                                        90
064100000000E   3C                   END
064200000000E   2C                   END
064300020405
064400020405      * Magazzino / tipo unità
064500100930     c                   if        vdpmag <> *blanks and errl00 = *off  and     --- 1 -->
064600100930     c                             vdpmag<>*zeros
064700020405     c                   if        wtipri <> *blanks                            --- 2 -->
064800020405     c                   eval      errl00 = *on
064900020405     c                   eval      dsmsms = msg(6)
065000020405     c                   else                                                   <-- 2 -->
065100020405      * controllo magazzino
065200100930     c                   if        vdpmag <> *blanks  and vdpmag<>*zeros
065300100930     c                   eval      wmag=vdpmag
065400020405     c                   exsr      chkmag
065500020405     c                   endif
065600020405     c                   eval      wtipri = 'MAG'
065700020405     c                   eval      *in91 = *on
065800020405     c                   endif                                                  <-- 2 ---
065900020405     c                   endif                                                  <-- 1 ---
066000000000     C*
066100000000E   1C                   END
066200000000     C*
066300000000     C                   ENDSR
066400020405     C*================================================================
066500020405     C*  CHKMAG - CONTROLLA IL MAGAZZINO UBICAZIONE
066600020405     C*================================================================
066700020405     C     CHKMAG        BEGSR
066800100930     c                   clear                   tnsdmagds
066900100930     c                   eval      §magmag=wmag
067000100930     c                   eval      §imagtorg='E  '
067100100930     c                   call      'TNSDMAGR'
067200100930     c                   parm                    kpjba
067300100930     c                   parm                    tnsdmagds
067400100930     c
067500100930     c* Eseguita Ricerca
067600100930    1c                   if        §omagerr='R'
067700100930     c                   eval      wmag=§magmag
067800100930     c                   eval      wdmag=§omagdes
067900100930   x1c                   else
068000100930     c* Errore nel magazzino
068100100930    2c                   if        §omagerr='E'
068200100930     C                   MOVEL     '1'           ERRL00
068300100930     C                   MOVEL     MSG(8)        DSMSMS
068400100930     C                   SETON                                        81
068500100930     c                   eval      wdmag=*all'?'
068600100930     c* se tutto ok prendo decodifica e flag
068700100930   x2c                   else
068800100930     c                   eval      wdmag=§omagdes
068900100930    2c                   endif
069000100930    1c                   endif
069100020405     C*
069200020405     C                   ENDSR
069300000000     C*================================================================
069400000000     C* LANPGM - LANCIA UN PROGRAMMA
069500000000     C*================================================================
069600000000     C     LANPGM        BEGSR
069700000000     C*
069800000000     C* IMPOSTO PARAMETRI PER IL PGM
069900000000     C                   MOVEL     '0'           DSUF03                         PREMUTO F03
070000000000     C                   MOVEL     '0'           DSUERR                         '1'=ERRORE
070100000000     C                   MOVEL     *BLANKS       DSUMSG                         MESSAGGIO
070200000000     C                   MOVE      WOPZ1         DSUOP1                         *OPZIONE LIV.PGM
070300000000     C                   Z-ADD     VIDTIP        DSUTIP                         TIPO
070400000000     C                   Z-ADD     VIDFIL        DSUMAG                         MAGAZZINO
070500000000     C*
070600000000     C* LANCIO PROGRAMMA
070700000000     C                   MOVEL     DSUNIT        KPJBU
070800000000     C                   CALL      WPGM
070900000000     C                   PARM                    KPJBA
071000000000     C                   MOVEL     KPJBU         DSUNIT
071100000000     C*
071200000000     C* SE MI RITORNA PREMUTO F3 CHIUDO IL PROGRAMMA
071300000000     C     DSUF03        IFEQ      '1'
071400000000     C                   MOVEL     '1'           F03L00
071500000000     C                   MOVEL     '1'           ERRL00
071600000000     C                   ELSE
071700000000     C* ALTRIMENTI MANDO MESSAGGIO RICEVUTO DAL PGM CHIAMATO
071800000000     C                   MOVEL     DSUMSG        DSMSMS
071900000000     C                   END
072000000000     C*
072100000000     C                   ENDSR
072200000000     C*================================================================
072300000000     C* CHKFOP - CONTROLLA TIPO GESTIONE E IMPOSTA FUNZ/OPZ
072400000000     C*================================================================
072500000000     C     CHKFOP        BEGSR
072600000000     C*
072700000000     C* CONTROLLO CHE VI SIA UNA OPZIONE DI PROCEDURA VALIDA IN INPUT
072800000000     C* ALTRIMENTI FORZO INTERROGAZIONE
072900000000     C     DSUOP0        IFNE      ' 2'                                         *GESTIONE
073000000000     C     DSUOP0        ANDNE     ' 5'                                         *INTERROGAZIONE
073100000000     C                   MOVE      ' 5'          DSUOP0
073200000000     C                   END
073300000000     C*
073400000000     C* SE L'UTENTE E' ENTRATO IN GESTIONE LO ABILITO A TUTTE LE FUNZIONI
073500000000     C     DSUOP0        IFEQ      ' 2'
073600000000     C                   MOVEL     OPD(2)        OPDRIG                         *MODIFICA
073700000000     C                   MOVEL     'S'           OPDATT
073800000000     C                   MOVEL     OPDRIG        OPD(2)
073900000000     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
074000000000     C                   MOVEL     'S'           OPDATT
074100000000     C                   MOVEL     OPDRIG        OPD(4)
074200000000     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
074300000000     C                   MOVEL     'S'           OPDATT
074400000000     C                   MOVEL     OPDRIG        OPD(5)
074500000000     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
074600000000     C                   MOVEL     'S'           OPDATT
074700000000     C                   MOVEL     OPDRIG        OPD(7)
074800000000     C                   MOVEL     OPD(8)        OPDRIG                         *GEST.MOVIMENTI
074900000000     C                   MOVEL     'S'           OPDATT
075000000000     C                   MOVEL     OPDRIG        OPD(8)
075100000000     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
075200000000     C                   MOVEL     'S'           FUDATT
075300000000     C                   MOVEL     FUDRIG        FUD(6)
075400000000     C                   END
075500000000     C*
075600000000     C* SE IL PGM E' STATO LANCIATO IN INTERROGAZIONE ABILITO SOLO QUELLA
075700000000     C     DSUOP0        IFEQ      ' 5'
075800000000     C                   MOVEL     OPD(5)        OPDRIG                         *INTERROGAZIONE
075900000000     C                   MOVEL     'S'           OPDATT
076000000000     C                   MOVEL     OPDRIG        OPD(5)
076100000000     C                   MOVEL     OPD(8)        OPDRIG                         *GEST.MOVIMENTI
076200000000     C                   MOVEL     'S'           OPDATT
076300000000     C                   MOVEL     OPDRIG        OPD(8)
076400000000     C                   END
076500000000     C*
076600000000     C* LANCIO ROUTINE PER VERIFICARE SE
076700000000     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
076800000000     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
076900020709
077000020709     c     *dtaara       define    §azute        azuteds
077100020709     c     *dtaara       define    §datiute      ddatiute
077200020709
077300020709     c                   in(E)     *dtaara
077400020709     c                   if        %Error or rsut = *blanks
077500020709     c                   clear                   tibs34ds
077600020709     c                   call      'TIBS34R'
077700020709     c                   parm                    Tibs34Ds
077800020709     c                   in        *dtaara
077900020709     c                   endif
078000020717
078100020717      * Controllo il profilo utente
078200020717     c                   ExSr      CtrUte
078300000000     C*
078400000000     C* SE SONO IN FILIALE NON CONCEDO CANCELLAZIONE,INSERIMENTO,RIPRISTINO
078500020709     c                   If        DutLpo <> 'S'
078600000000     C                   MOVEL     OPD(4)        OPDRIG                         *CANCELLAZIONE
078700000000     C                   MOVEL     'N'           OPDATT
078800000000     C                   MOVEL     OPDRIG        OPD(4)
078900000000     C                   MOVEL     OPD(7)        OPDRIG                         *RIPRISTINO
079000000000     C                   MOVEL     'N'           OPDATT
079100000000     C                   MOVEL     OPDRIG        OPD(7)
079200000000     C                   MOVEL     FUD(6)        FUDRIG                         *INSERIMENTO
079300000000     C                   MOVEL     'N'           FUDATT
079400000000     C                   MOVEL     FUDRIG        FUD(6)
079500000000     C                   END
079600000000     C*
079700000000     C* IMPOSTO INDICE PER EMISSIONE OPZIONI A VIDEO
079800000000     C                   Z-ADD     0             OPZGRU            5 0
079900000000     C*
080000000000     C* GESTIONE OPZIONI A VIDEO
080100000000     C                   EXSR      OPZGES
080200000000     C*
080300000000     C* IMPOSTO INDICE PER EMISSIONE FUNZIONI A VIDEO
080400000000     C                   Z-ADD     0             FUNGRU            5 0
080500000000     C*
080600000000     C* GESTIONE TASTI FUNZIONALI A VIDEO
080700000000     C                   EXSR      FUNGES
080800000000     C*
080900000000     C                   ENDSR
081000020717      *------------------------------------------------------------------------*
081100020717      *  CTRUTE - ROUTINE CONTROLLO UTENTE
081200020717      *------------------------------------------------------------------------*
081300020717     c     CtrUte        BegSr
081400020717
081500020717     c                   Eval      *In76 = *Off
081600020717
081700020717      * Controllo se il profilo è ok da tibs34
081800020717     c                   If        DutErr = 'E'
081900020717     c                   Eval      DsmSms = DutMsg
082000020717     c                   Eval      *In76 = *On
082100020717     c                   EndIf
082200020717
082300020717      * Controllo se il profilo di sede
082400020717     c                   If        DutLpo <> 'S'
082500020717     c                   Eval      DsmSms = Msg(9)
082600020717     c                   Eval      *In76 = *On
082700020717     c                   EndIf
082800020717
082900020717     c                   endSr
083000000000     C*================================================================
083100000000     C* *INZSR - ROUTINE INIZIALE
083200000000     C*================================================================
083300000000     C     *INZSR        BEGSR
083400000000     C*
083500000000     C* RICEVIMENTO PARAMETRI DALL'AZIONE (' 2'=GEST.,' 5'=INTERR.)
083600000000     C     *ENTRY        PLIST
083700000000     C                   PARM                    KPJBA
083800000000     C                   MOVEL     KPJBU         DSUNIT
083900000000     C*
084000000000     C* PARAMETRI PER RICHIAMO PGM (ROUTINE) XMSG (MESSAGGI)
084100000000     C     XMSGPA        PLIST
084200000000     C                   PARM                    DSMSMS
084300000000     C                   PARM                    DSMSPG
084400000000     C                   PARM                    DSMSKE
084500000000     C                   PARM                    DSMSER
084600000000     C* DEFINIZIONE CAMPI
084700000000     C     *LIKE         DEFINE    DOTTIP        KDOTIP                         *UNDOT00F
084800950125     C     *LIKE         DEFINE    UNAMAG        KANMAG                         *UNANA00F
084900950125     C     *LIKE         DEFINE    UNATIP        KANTIP
085000000000     C*
085100000000     C* CHIAVE LETTURA UNDOT01L - PARZIALE
085200000000     C     KUNDOT        KLIST
085300020322     C                   KFLD                    KDOTIP
085400020322     C                   KFLD                    KDOfil
085500950124     C*
085600020322     C* CHIAVE LETTURA UNANA09L - PARZIALE
085700950124     C     KEYANA        KLIST
085800020322     C                   KFLD                    KANTIP
085900950124     C                   KFLD                    KANMAG
086000000000     C*
086100000000     C* CHIAMA ROUTINE PER RIEMPIRE LA DS DSKEY (TASTI PREMUTI)
086200000000     C                   CALL      'XKEY'
086300000000     C                   PARM                    DSKEY
086400000000     C*
086500000000     C* DEFINISCE LE VARIABILI DI PROGRAMMA
086600000000     C     *LIKE         DEFINE    SFLNBR        SFLPAG
086700000000     C     *LIKE         DEFINE    SFLPAG        WFLPAG
086800000000     C     *LIKE         DEFINE    SFLNBR        SFLEOF
086900000000     C     *LIKE         DEFINE    SFLNBR        SFLERR
087000000000     C     *LIKE         DEFINE    SFLNBR        SFLULT
087100000000     C     *LIKE         DEFINE    SFLNBR        WFLULT
087200000000     C     *LIKE         DEFINE    SFLNBR        MSGULT
087300000000     C     *LIKE         DEFINE    SFLNBR        SFLRIG
087400000000     C     *LIKE         DEFINE    FUDFUN        FUNKEY
087500000000     C     *LIKE         DEFINE    VDPTIP        DEPTIP
087600000000     C*
087700000000     C* IMPOSTA PER CL=XMSG  (FISSI IN TUTTO IL PGM) :
087800000000     C*   -NOME PGM CHIAMANTE
087900000000     C*   -NOME CODA MESSAGGI DI DESTINAZIONE
088000000000     C                   MOVEL     'TNSDUCR'     DSMSPG
088100000000     C                   MOVEL     'TNSDUCR'     MSGQUE
088200000000     C                   MOVEL     *BLANKS       MSGKEY
088300000000     C                   MOVEL     *BLANKS       DSMSMS
088400000000     C                   MOVEL     *BLANKS       DSMSKE
088500000000     C                   Z-ADD     0             DSMSER
088600000000     C*
088700000000     C* INIZIALIZZO SWITCH DI LIVELLO 0 (PGM)
088800000000     C                   MOVEL     '0'           F12L00            1
088900000000     C                   MOVEL     '0'           F03L00            1
089000000000     C                   MOVEL     '0'           FINL00            1
089100000000     C                   MOVEL     '0'           ERRL00            1
089200000000     C                   MOVEL     '0'           FLFINE            1
089300950124     C                   MOVEL     '0'           $FIN2             1
089400000000     C*
089500000000     C* IMPOSTO DEPOSITI PER CONFRONTI
089600000000     C                   MOVEL     *BLANKS       DEPTIP
089700020405     c                   clear                   depdot
089800020405     c                   clear                   depmag
089900020322     c                   clear                   wuntip
090000020322     c                   clear                   wunmag
090100000000     C*
090200000000     C* IMPOSTO TIPO DI RICERCA DI DEFAULT --> 'TIP'
090300000000     C                   MOVEL     'TIP'         WTIPRI
090400000000     C                   SETON                                        90
090500000000     C*
090600000000     C* INIZIALIZZO CONTATORE DI NUMERO RIGHE PER PAGINA
090700000000     C* (DEVE ESSERE UGUALE A SFLPAG (IN RECORD CONTROLLO SFL)
090800020405     C                   Z-ADD     11            SFLPAG
090900000000     C*
091000000000     C* IMPOSTO LE OPZIONI E LE FUNZIONI A VIDEO
091100000000     C                   EXSR      CHKFOP
091200000000     C*
091300000000     C* IMPOSTO INTERROGAZIONE/GESTIONE NELLA TESTATA DELLA VIDEATA
091400000000     C     DSUOP0        IFEQ      ' 2'
091500000000     C                   MOVEL     '    GEST'    VIDING
091600000000     C                   MOVE      'IONE    '    VIDING
091700000000     C                   ELSE
091800000000     C                   MOVEL     ' INTERRO'    VIDING
091900000000     C                   MOVE      'GAZIONE '    VIDING
092000000000     C                   END
092100000000     C*
092200000000     C* CARICA TABELLE
092300000000     C                   EXSR      CARTAB
092400000000     C*
092500000000     C                   ENDSR
092600000000     C*================================================================
092700000000     C* CARTAB - CARICA TABELLE
092800000000     C*================================================================
092900000000     C     CARTAB        BEGSR
093000000000     C*--------------------
093100000000     C* TIPI UNITA'
093200000000     C*--------------------
093300000000     C                   Z-ADD     0             J                 3 0          *INDICE SCHIERA
093400020322     C                   MOVEL     'UNT'         tbecod
093500020322     C     tbecod        SETLL     tntbe01l                           99        *FINE FILE
093600020322     C  N99tbecod        READE     tntbe01l                               99
093700000000     C*
093800000000     C     *IN99         DOWEQ     *OFF
093900020322     C     TBeatb        IFEQ      *BLANKS
094000000000     C                   ADD       1             J
094100020322     C                   MOVEL     TBeke1        UNI(J)
094200020322     C                   MOVEL     tbeuni        DUNI(J)
094300000000     C                   ENDIF
094400000000     C*
094500020322     C  N99tbecod        READE     tntbe01l                               99
094600000000     C                   ENDDO
094700000000     C*
094800000000     C                   ENDSR
094900000000     C*================================================================
095000000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095100000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095200000000     C* LA PARTE DI SPECIFICHE 'C' SOTTOSTANTE SI RIFA' A PARTICOLARI
095300000000     C* STANDARDS DI PROGRAMMAZIONE E NELLA MAGGIOR PARTE DEI CASI
095400000000     C* NON E' NECESSARIO MODIFICARLA, NEL CASO FARE ATTENZIONE.
095500000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095600000000     C* §§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§§
095700000000     C*================================================================
095800000000     C* POSSFL - RICARICAMENTO SFL DA NUOVO POSIZIONAMENTO FILE DA
095900000000     C*          CARICARE
096000000000     C*================================================================
096100000000     C     POSSFL        BEGSR
096200000000     C*
096300000000     C* CLEAR SFL (ELIMINA CONTENUTO SFL)
096400000000     C                   EXSR      CLRSFL
096500000000     C*
096600000000     C* CONTROLLO SE VI SONO REC. CHE SODDISFANO I PARAMETRI IN INPUT
096700000000     C                   EXSR      SETFIL
096800000000     C*
096900000000     C* SE ESISTONO RECORD VALIDI CARICO IL SUBFILE
097000000000     C     SFLEOF        IFEQ      0
097100000000     C                   EXSR      CARSFL
097200000000     C                   END
097300000000     C*
097400000000     C                   ENDSR
097500000000     C*================================================================
097600000000     C* ENTATT - GESTIONE SFL CON TASTO FUNZIONALE ENTER
097700000000     C*================================================================
097800000000     C     ENTATT        BEGSR
097900000000     C*
098000000000     C* AZZERO SWITCH DI ERRORI IN RECORD SFL (OPZOK: 1=ERRORE 0=OK)
098100000000     C                   Z-ADD     0             OPZOK             1 0
098200000000     C*
098300000000     C* AZZERO CONTATORE DI RIGHE SFL VARIATE
098400000000     C                   Z-ADD     0             SFLVAR            5 0
098500000000     C*
098600000000     C* INIZIALIZZO COMODO CON PRIMO RECORD SFL IN ERRORE
098700000000     C                   Z-ADD     0             SFLERR
098800000000     C*
098900000000     C* SALVO ULTIMO RECORD SCRITTO IN SFL IN COMODO
099000000000     C                   Z-ADD     SFLULT        WFLULT
099100000000     C*
099200000000     C* SCORRIMENTO DEI SOLI RECORD VARIATI NEL SFL
099300000000     C                   READC     SDUCD05                                98
099400000000     C     *IN98         DOWEQ     '0'
099500000000     C*
099600000000     C* ADD CONTATORE DI RIGHE SFL VARIATE
099700000000     C                   ADD       1             SFLVAR
099800000000     C*
099900000000     C* SE RIGHE PRECEDENTI SENZA ERRORI (OPZOK: 1=ERRORE 0=OK)
100000000000     C     OPZOK         IFEQ      0
100100000000     C*
100200000000     C* CONTROLLO ED ESECUZIONE OPZIONI RICHIESTE (OPZOK: 1=ERRORE 0=OK)
100300000000     C     OPZKEY        IFNE      *BLANKS
100400000000     C                   EXSR      OPZCON
100500000000     C                   END
100600000000     C*
100700000000     C* SE OK    : 1. AZZERO SFLNXTCHG PER RECORD SFL
100800000000     C*            2. PULIZIA SCELTA IN SFL
100900000000     C     OPZOK         IFEQ      0
101000000000     C                   MOVEL     '0'           *IN41
101100000000     C                   MOVEL     *BLANKS       OPZKEY
101200000000     C                   ELSE
101300000000     C*
101400000000     C* SE NON OK: 1. ACCENDO SFLNXTCHG COSI' COSTRINGO ALLA CORREZIONE
101500000000     C*            2. IMPOSTO RECORD CON PRIMO ERRORE
101600000000     C                   MOVEL     '1'           *IN41
101700000000     C                   Z-ADD     SFLULT        SFLERR
101800000000     C                   END
101900000000     C                   ELSE
102000000000     C*
102100000000     C* SE RIGHE PRECEDENTI CON ERRORI (SFLERR DIVERSO DA 0):
102200000000     C*   . ACCENDO SFLNXTCHG COSI' RIPROPONE LA RIGA IL PROSSIMO GIRO
102300000000     C                   MOVEL     '1'           *IN41
102400000000     C                   END                                                    OPZOK=0
102500000000     C*
102600000000     C* IMPOSTO RIGA SFL SUL QUALE POSIZIONARSI:
102700000000     C*   . SE ERRORE        MI POSIZIONO SULLA PRIMA RIGA IN ERRORE
102800000000     C*   . SE NESSUN ERRORE MI POSIZIONO SULL'ULTIMA RIGA VARIATA
102900000000     C     OPZOK         IFNE      0
103000000000     C                   Z-ADD     SFLERR        SFLNBR
103100000000     C                   ELSE
103200000000     C                   Z-ADD     SFLULT        SFLNBR
103300000000     C                   END
103400000000     C*
103500000000     C* AGGIORNO RECORD SFL
103600000000     C                   UPDATE    SDUCD05
103700000000     C*
103800000000     C                   READC     SDUCD05                                98
103900000000     C                   END                                                    DO WHILE
104000000000     C*
104100000000     C* REIMPOSTO ULTIMO RECORD SCRITTO IN SFL DA COMODO
104200000000     C                   Z-ADD     WFLULT        SFLULT
104300000000     C*
104400000000     C* SE NESSUNA RIGA VARIATA, CON ENTER SI RITORNA AL PGM CHIAMANTE
104500000000     C     SFLVAR        IFEQ      0
104600000000     C                   MOVEL     '1'           FINL00
104700000000     C                   END
104800000000     C*
104900000000     C                   ENDSR
105000000000     C*================================================================
105100000000     C* ELIMINAZIONE CONTENUTO SFL
105200000000     C*================================================================
105300000000     C     CLRSFL        BEGSR
105400000000     C*
105500000000     C* INIZIALIZZO SWITCH DI FINE FILE "FISICO"
105600000000     C* (0=NON FINE FILE  1=FINE FILE)
105700000000     C                   Z-ADD     0             SFLEOF
105800000000     C*
105900000000     C* INIZIALIZZO CONTATORI DI ULTIMO RECORD IN SFL
106000000000     C                   Z-ADD     0             SFLULT
106100000000     C*
106200000000     C* ACCENSIONE INDICATORE PER CLEAR SFL
106300000000     C                   MOVEL     '1'           *IN40
106400000000     C                   WRITE     SDUCD06
106500000000     C                   MOVEL     '0'           *IN40
106600000000     C*
106700000000     C                   ENDSR
106800000000     C*================================================================
106900000000     C* CARSFL - CARICAMENTO SFL
107000000000     C*================================================================
107100000000     C     CARSFL        BEGSR
107200000000     C*
107300000000     C* SE IL SFL E' VUOTO INIZIO LETTURA DA POSIZ. RICHIESTA
107400000000     C     SFLULT        IFEQ      0
107500020322     c                   clear                   totfil
107600020322     c                   clear                   wuntip
107700020322     c                   clear                   wunmag
107800020711     c                   Clear                   SavTip
107900020711     c                   Clear                   SavMag
108000020711     c                   Clear                   SavAtb
108100020711     c                   Clear                   SavQta
108200000000     C                   EXSR      SETFIL
108300000000     C                   END
108400000000     C*
108500000000     C* SALVO SFLRCDNBR PER CONFRONTI SUCCESSIVI
108600000000     C                   Z-ADD     SFLULT        WFLULT
108700000000     C*
108800000000     C* CICLO:
108900000000     C*    -CARICO NUMERO DI RIGHE=SFLPAG
109000000000     C*    -SFLEOF=0 (NON FINE FILE "FISICO")
109100000000     C                   Z-ADD     1             SFLRIG
109200000000     C     SFLRIG        DOWLE     SFLPAG
109300000000     C     SFLEOF        ANDEQ     0
109400020326
109500020326     c                   eval      wnosfl = *off
109600020326
109700000000     C*
109800000000     C* IMPOSTO CAMPI DEL RECORD SFL SOLO SE RECORD E' OK CIOE' ...
109900000000     C                   MOVEL     *BLANKS       OPZKEY
110000020326
110100020322      * emetto solo a rottura di magazzino
110200020710     c**!!!              if        unamag <> wunmag or unatip <> wuntip
110300020711
110400020711if  2c                   If        SavMag <> WunMag or SavTip <> WunTip
110500020711if  3c                   if        wunmag > *zeros
110600000000     C*
110700000000     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
110800000000     C                   EXSR      IRESFL
110900020326
111000020322     C                   Z-ADD     *ZEROS        TOTFIL            7 0
111100020711     c                   Clear                   SavAtb
111200020711     c                   Clear                   SavQta
111300020711
111400020711     c                   If        *In01 or *In03
111500020711     c                   eval      wunmag = unamag
111600020711     c                   eval      wuntip = unatip
111700020711     c                   EndIf
111800020711     c                   If        *In02 or *In04
111900020711     c                   eval      wunmag = dotfil
112000020711     c                   eval      wuntip = dottip
112100020711     c                   EndIf
112200020326
112300020326      * controllo se devo scrivere il record nel subfile
112400020326     c                   exsr      ctrsfl
112500020711     c                   Eval      WokUni = *Off
112600020711     c                   Eval      WokDot = *Off
112700020326
112800020711if  4c                   if        wnosfl = *off
112900000000     C*
113000000000     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
113100000000     C                   ADD       1             SFLULT
113200000000     C*
113300000000     C* SCRITTURA RECORD SFL
113400000000     C                   WRITE     SDUCD05
113500000000     C                   ADD       1             SFLRIG
113600020711e   4c                   endif
113700020322
113800020711     c                   If        *In01 or *In03
113900020322     C                   ADD       1             TOTFIL
114000020711     c                   Eval      WokUni = *On
114100020711     c                   EndIf
114200020711e   3c                   EndIf
114300020711     c                   If        *In02 or *In04
114400020711     C                   Eval      SavAtb = DotAtb
114500020711     C                   Eval      SavQta = DotQta
114600020711     c                   Eval      WokDot = *On
114700020711     c                   EndIf
114800020322
114900020322      * conto le unità presenti in magazzino
115000020711x   2c                   else
115100020711     c                   If        *In01 or *In03
115200020711     C                   ADD       1             TOTFIL
115300020711     c                   Eval      WokUni = *On
115400020711     c                   EndIf
115500020711     c                   If        *In02 or *In04
115600020711     C                   Eval      SavAtb = DotAtb
115700020711     C                   Eval      SavQta = DotQta
115800020711     c                   Eval      WokDot = *On
115900020711     c                   EndIf
116000020711e   2c                   EndIf
116100000000     C*
116200000000     C* LETTURA FILE DA CARICARE
116300000000     C                   EXSR      LETFIL
116400000000     C                   END                                                    DO WHILE
116500020322
116600020322      * emetto l'ultimo record del file
116700020322     c                   if        wunmag > *zeros and sfleof = 1
116800020711     c                             and not *in42
116900020322     C*
117000020322     C* IMPOSTA I CAMPI DEL RECORD DEL SFL DA FILE DA CARICARE
117100020322     C                   EXSR      IRESFL
117200020326
117300020326      * controllo se devo scrivere il record nel subfile
117400020326     c                   exsr      ctrsfl
117500020326
117600020326     c                   if        wnosfl = *off
117700020322     C*
117800020322     C* AGGIORNA CHIAVE PER RECORD SFL E SFLRCDNBR
117900020322     C                   ADD       1             SFLULT
118000020322     C*
118100020322     C* SCRITTURA RECORD SFL
118200020322     C                   WRITE     SDUCD05
118300020322     C                   ADD       1             SFLRIG
118400020326     c                   endif
118500020322     c                   endif
118600000000     C*
118700000000     C* IMPOSTO SFLRCDNBR DELLA SUCCESSIVA PAGINA SE CARICATI ALTRI RECORD
118800000000     C     WFLULT        IFNE      SFLULT
118900000000     C                   Z-ADD     WFLULT        SFLNBR
119000000000     C                   ADD       1             SFLNBR
119100000000     C                   END
119200000000     C*
119300000000     C                   ENDSR
119400000000     C*================================================================
119500000000     C* EMISSIONE SUBFILE A VIDEO
119600000000     C*================================================================
119700000000     C     OUTSFL        BEGSR
119800000000     C*
119900000000     C* WRITE RECORD TESTATA VIDEO
120000000000     C                   WRITE     SDUCD01
120100000000     C*
120200000000     C* VISUALIZZAZIONE MESSAGGIO
120300000000     C                   EXSR      SNDMSG
120400000000     C*
120500000000     C* WRITE RECORD OPZIONI VIDEO
120600000000     C                   WRITE     SDUCD02
120700000000     C*
120800000000     C* WRITE RECORD PER POSIZIONAMEMTO VIDEO
120900000000     C                   WRITE     SDUCD03
121000000000     C*
121100000000     C* WRITE RECORD CON DESCRIZIONI COLONNE SFL
121200000000     C                   WRITE     SDUCD04
121300000000     C*
121400000000     C* GESTIONE AUTOMATICA SFLEND (+ A VIDEO)
121500000000     C* SFLEOF=0 NON RAGGIUNTO FINE FILE "FISICO"
121600000000     C     SFLEOF        IFEQ      0
121700000000     C                   MOVEL     '0'           *IN42
121800000000     C                   ELSE
121900000000     C*
122000000000     C* SFLEOF=1 RAGGIUNTO FINE FILE "FISICO"
122100000000     C                   MOVEL     '1'           *IN42
122200000000     C                   END
122300000000     C*
122400000000     C* WRITE RECORD TASTI FUNZIONALI
122500000000     C                   WRITE     SDUCD09
122600000000     C*
122700000000     C* CASO DI SFL VUOTO
122800000000     C     SFLULT        IFLE      0
122900000000     C*
123000000000     C*    - WRITE RECORD VUOTO
123100000000     C                   WRITE     SDUCD0A
123200000000     C                   READ      SDUCD0A                                99
123300000000     C*
123400000000     C                   ELSE
123500000000     C*
123600000000     C* CASO DI SFL CON RECORDS
123700000000     C*
123800000000     C*    - WRITE RECORD CONTROLLO SFL
123900000000     C                   WRITE     SDUCD06
124000000000     C                   READ      SDUCD06                                99
124100000000     C                   END
124200000000     C*
124300000000     C* READ RECORD PER POSIZIONAMEMTO VIDEO
124400000000     C                   READ      SDUCD03                                99
124500000000     C*
124600000000     C                   ENDSR
124700000000     C*================================================================
124800000000     C* SNDMSG - MANDA MESSAGGIO
124900000000     C*================================================================
125000000000     C     SNDMSG        BEGSR
125100000000     C*
125200000000     C* PULIZIA SUBFILE MESSAGGI (WRITE RECORD CONTROLLO SFLMSG)
125300000000     C                   MOVEL     '1'           *IN40
125400000000     C                   WRITE     SDUCDMC
125500000000     C                   MOVEL     '0'           *IN40
125600000000     C*
125700000000     C* VISUALIZZAZIONE MESSAGGIO
125800000000     C                   Z-ADD     0             MSGULT
125900000000     C                   CALL      'XMSG'        XMSGPA
126000000000     C     DSMSER        IFEQ      0
126100000000     C                   MOVEL     DSMSKE        MSGKEY
126200000000     C                   END
126300000000     C                   Z-ADD     1             MSGULT
126400000000     C                   WRITE     SDUCDMS                                      SFL MESS
126500000000     C                   WRITE     SDUCDMC                                      CONTR MESS
126600000000     C*
126700000000     C* MANDO MESSAGGIO *BLANKS PER PULIRE SFLMSG
126800000000     C                   MOVEL     *BLANKS       DSMSMS
126900000000     C*
127000000000     C                   ENDSR
127100000000     C*================================================================
127200000000     C* GESTIONE SCELTE DELL'OPERATORE A VIDEO (ATTRAVERSO TASTI FXX)
127300000000     C*================================================================
127400000000     C     FUNCON        BEGSR
127500000000     C*
127600000000     C* TRADUCE TASTI FUNZIONALI IN VARIABILE -FUNKEY-
127700000000     C                   EXSR      FUNVAR
127800000000     C*
127900000000     C* PULISCO CAMPI DI LAVORO
128000000000     C                   Z-ADD     0             I                 5 0
128100000000     C                   Z-ADD     0             W                 5 0
128200000000     C*
128300000000     C* RICERCA FUNZIONE (DIVERSA DA ENTER) SCELTA IN TABELLA FUNZIONI
128400000000     C     FUNKEY        IFNE      *BLANKS
128500100922     C     1             DO        25            I
128600000000     C                   MOVEL     FUD(I)        FUDRIG
128700000000     C*
128800000000     C* SALVO INDICE DI FUNZIONE TROVATA ED ATTIVA (W)
128900000000     C     FUNKEY        IFEQ      FUDFUN
129000000000     C     FUDATT        ANDEQ     'S'
129100000000     C                   Z-ADD     I             W
129200000000     C                   END
129300000000     C                   END                                                    DO WHILE
129400000000     C*
129500000000     C* FUNZIONE NON ATTIVA (ERRORE)
129600000000     C     W             IFEQ      0
129700000000     C*
129800000000     C* EMISSIONE MESSAGGIO DI ERRORE
129900000000     C                   MOVEL     MSG(2)        DSMSMS
130000000000     C                   END                                                    W=0
130100000000     C                   END                                                    FUNKEY NE *BLANKS
130200000000     C*
130300000000     C* FUNZIONE ATTIVA (PROCEDI ...)
130400000000     C     W             IFNE      0
130500000000     C     FUNKEY        OREQ      *BLANKS
130600000000     C*
130700000000     C     FUNKEY        IFEQ      *BLANKS
130800000000     C                   EXSR      ENTGES
130900000000     C                   END
131000000000     C     FUNKEY        IFEQ      '03'
131100000000     C                   EXSR      F03GES
131200000000     C                   END
131300000000     C     FUNKEY        IFEQ      '05'
131400000000     C                   EXSR      F05GES
131500000000     C                   END
131600000000     C     FUNKEY        IFEQ      '06'
131700000000     C                   EXSR      F06GES
131800000000     C                   END
131900000000     C     FUNKEY        IFEQ      '12'
132000000000     C                   EXSR      F12GES
132100000000     C                   END
132200000000     C     FUNKEY        IFEQ      '17'
132300000000     C                   EXSR      F17GES
132400000000     C                   END
132500000000     C     FUNKEY        IFEQ      '18'
132600000000     C                   EXSR      F18GES
132700000000     C                   END
132800000000     C     FUNKEY        IFEQ      '21'
132900000000     C                   EXSR      F21GES
133000000000     C                   END
133100000000     C     FUNKEY        IFEQ      '22'
133200000000     C                   EXSR      F22GES
133300000000     C                   END
133400000000     C     FUNKEY        IFEQ      '23'
133500000000     C                   EXSR      F23GES
133600000000     C                   END
133700000000     C     FUNKEY        IFEQ      '24'
133800000000     C                   EXSR      F24GES
133900000000     C                   END
134000000000     C     FUNKEY        IFEQ      'RU'                                         ROLLUP
134100000000     C                   EXSR      RUPGES
134200000000     C                   END
134300000000     C     FUNKEY        IFEQ      'HP'                                         HELP
134400000000     C                   EXSR      HELGES
134500000000     C                   END
134600000000     C*
134700000000     C                   END
134800000000     C*
134900000000     C                   ENDSR
135000000000     C*================================================================
135100000000     C* FUNVAR -TRADUCE INDICATORI DI TASTO FUNZIONALE IN VARIABILE
135200000000     C*================================================================
135300000000     C     FUNVAR        BEGSR
135400000000     C*
135500000000     C* INIZIALIZZA VARIABILE -FUNKEY-
135600000000     C*
135700000000     C* FUNKEY=*BLANKS SIGNIFICA ENTER
135800000000     C     KEY           IFEQ      ENTER
135900000000     C                   MOVEL     *BLANKS       FUNKEY
136000000000     C                   END
136100000000     C     KEY           IFEQ      F1
136200000000     C                   MOVEL     '01'          FUNKEY
136300000000     C                   END
136400000000     C     KEY           IFEQ      F2
136500000000     C                   MOVEL     '02'          FUNKEY
136600000000     C                   END
136700000000     C     KEY           IFEQ      F3
136800000000     C                   MOVEL     '03'          FUNKEY
136900000000     C                   END
137000000000     C     KEY           IFEQ      F4
137100000000     C                   MOVEL     '04'          FUNKEY
137200000000     C                   END
137300000000     C     KEY           IFEQ      F5
137400000000     C                   MOVEL     '05'          FUNKEY
137500000000     C                   END
137600000000     C     KEY           IFEQ      F6
137700000000     C                   MOVEL     '06'          FUNKEY
137800000000     C                   END
137900000000     C     KEY           IFEQ      F7
138000000000     C                   MOVEL     '07'          FUNKEY
138100000000     C                   END
138200000000     C     KEY           IFEQ      F8
138300000000     C                   MOVEL     '08'          FUNKEY
138400000000     C                   END
138500000000     C     KEY           IFEQ      F9
138600000000     C                   MOVEL     '09'          FUNKEY
138700000000     C                   END
138800000000     C     KEY           IFEQ      F10
138900000000     C                   MOVEL     '10'          FUNKEY
139000000000     C                   END
139100000000     C     KEY           IFEQ      F11
139200000000     C                   MOVEL     '11'          FUNKEY
139300000000     C                   END
139400000000     C     KEY           IFEQ      F12
139500000000     C                   MOVEL     '12'          FUNKEY
139600000000     C                   END
139700000000     C     KEY           IFEQ      F13
139800000000     C                   MOVEL     '13'          FUNKEY
139900000000     C                   END
140000000000     C     KEY           IFEQ      F14
140100000000     C                   MOVEL     '14'          FUNKEY
140200000000     C                   END
140300000000     C     KEY           IFEQ      F15
140400000000     C                   MOVEL     '15'          FUNKEY
140500000000     C                   END
140600000000     C     KEY           IFEQ      F16
140700000000     C                   MOVEL     '16'          FUNKEY
140800000000     C                   END
140900000000     C     KEY           IFEQ      F17
141000000000     C                   MOVEL     '17'          FUNKEY
141100000000     C                   END
141200000000     C     KEY           IFEQ      F18
141300000000     C                   MOVEL     '18'          FUNKEY
141400000000     C                   END
141500000000     C     KEY           IFEQ      F19
141600000000     C                   MOVEL     '19'          FUNKEY
141700000000     C                   END
141800000000     C     KEY           IFEQ      F20
141900000000     C                   MOVEL     '20'          FUNKEY
142000000000     C                   END
142100000000     C     KEY           IFEQ      F21
142200000000     C                   MOVEL     '21'          FUNKEY
142300000000     C                   END
142400000000     C     KEY           IFEQ      F22
142500000000     C                   MOVEL     '22'          FUNKEY
142600000000     C                   END
142700000000     C     KEY           IFEQ      F23
142800000000     C                   MOVEL     '23'          FUNKEY
142900000000     C                   END
143000000000     C     KEY           IFEQ      F24
143100000000     C                   MOVEL     '24'          FUNKEY
143200000000     C                   END
143300000000     C     KEY           IFEQ      ROLLUP
143400000000     C                   MOVEL     'RU'          FUNKEY
143500000000     C                   END
143600000000     C     KEY           IFEQ      HELP
143700000000     C                   MOVEL     'HP'          FUNKEY
143800000000     C                   END
143900000000     C*
144000000000     C                   ENDSR
144100000000     C*================================================================
144200000000     C* FUNGES - GESTIONE VISUALIZZAZIONE TASTI FUNZIONALI GESTITI DA PGM
144300000000     C*================================================================
144400000000     C     FUNGES        BEGSR
144500000000     C*
144600000000     C* PULISCO CAMPI DI LAVORO
144700000000     C                   Z-ADD     0             W                 5 0
144800000000     C*
144900000000     C* RICERCO QUANTE FUNZIONI SONO ATTIVE
145000000000     C* E CARICO TABELLA FUNZIONI ATTIVE
145100000000     C     1             DO        24            I
145200000000     C                   MOVEL     FUD(I)        FUDRIG
145300000000     C*
145400000000     C* PULIZIA FUNZIONI PRECEDENTI
145500000000     C                   MOVEL     *BLANKS       FUV(I)
145600000000     C*
145700000000     C* IMPOSTA FUNZIONI ATTIVE IN TABELLA
145800000000     C* (SALTA F23 E F24)
145900000000     C     FUDATT        IFEQ      'S'
146000000000     C     FUDFUN        ANDNE     '23'
146100000000     C     FUDFUN        ANDNE     '24'
146200000000     C                   ADD       1             W
146300000000     C                   MOVEL     FUDDES        FUV(W)
146400000000     C                   END
146500000000     C                   END                                                    DO WHILE
146600000000     C*
146700000000     C* SE VI SONO PIU' DI 6 FUNZIONI ATTIVE, ATTIVO FUNZIONE F24
146800000000     C* ALTRIMENTI DISATTIVO F24
146900000000     C     1             DO        24            I
147000000000     C                   MOVEL     FUD(I)        FUDRIG
147100000000     C*
147200000000     C     FUDFUN        IFEQ      '24'
147300000000     C     W             IFGT      6
147400000000     C                   MOVEL     'S'           FUDATT
147500000000     C                   ELSE
147600000000     C                   MOVEL     *BLANKS       FUDATT
147700000000     C                   END                                                    W GT 8
147800000000     C                   MOVEL     FUDRIG        FUD(I)
147900000000     C                   END
148000000000     C                   END                                                    DO WHILE
148100000000     C*
148200000000     C* IMPOSTO QUANTI GRUPPI DI 6 FUNZIONI SI POSSONO VISUALIZZARE
148300000000     C     W             DIV       6             I
148400000000     C                   MVR                     Z
148500000000     C*
148600000000     C     Z             IFNE      0
148700000000     C     I             ADD       1             Z
148800000000     C                   ELSE
148900000000     C                   Z-ADD     I             Z
149000000000     C                   END
149100000000     C*
149200000000     C* IMPOSTO SUCCESSIVE 6 FUNZIONI ATTIVE A VIDEO
149300000000     C                   ADD       1             FUNGRU
149400000000     C     FUNGRU        IFGT      Z
149500000000     C     FUNGRU        ORLT      1
149600000000     C                   Z-ADD     1             FUNGRU
149700000000     C                   END
149800000000     C*
149900000000     C* PRIMO GRUPPO DI 6 FUNZIONI
150000000000     C     FUNGRU        IFEQ      1
150100000000     C                   MOVEL     FUV(1)        FUN001
150200000000     C                   MOVEL     FUV(2)        FUN002
150300000000     C                   MOVEL     FUV(3)        FUN003
150400000000     C                   MOVEL     FUV(4)        FUN004
150500000000     C                   MOVEL     FUV(5)        FUN005
150600000000     C                   MOVEL     FUV(6)        FUN006
150700000000     C                   END
150800000000     C*
150900000000     C* SECONDO GRUPPO DI 6 FUNZIONI
151000000000     C     FUNGRU        IFEQ      2
151100000000     C                   MOVEL     FUV(7)        FUN001
151200000000     C                   MOVEL     FUV(8)        FUN002
151300000000     C                   MOVEL     FUV(9)        FUN003
151400000000     C                   MOVEL     FUV(10)       FUN004
151500000000     C                   MOVEL     FUV(11)       FUN005
151600000000     C                   MOVEL     FUV(12)       FUN006
151700000000     C                   END
151800000000     C*
151900000000     C* TERZO GRUPPO DI 6 FUNZIONI
152000000000     C     FUNGRU        IFEQ      3
152100000000     C                   MOVEL     FUV(13)       FUN001
152200000000     C                   MOVEL     FUV(14)       FUN002
152300000000     C                   MOVEL     FUV(15)       FUN003
152400000000     C                   MOVEL     FUV(16)       FUN004
152500000000     C                   MOVEL     FUV(17)       FUN005
152600000000     C                   MOVEL     FUV(18)       FUN006
152700000000     C                   END
152800000000     C*
152900000000     C* QUARTO GRUPPO DI 6 FUNZIONI
153000000000     C     FUNGRU        IFEQ      4
153100000000     C                   MOVEL     FUV(19)       FUN001
153200000000     C                   MOVEL     FUV(20)       FUN002
153300000000     C                   MOVEL     FUV(21)       FUN003
153400000000     C                   MOVEL     FUV(22)       FUN004
153500000000     C                   MOVEL     *BLANKS       FUN005
153600000000     C                   MOVEL     *BLANKS       FUN006
153700000000     C                   END
153800000000     C*
153900000000     C* F24 E F23 SE VI SONO, VENGONO SEMPRE VISUALIZZATI
154000000000     C                   MOVEL     *BLANKS       FUN007
154100000000     C                   MOVEL     *BLANKS       FUN008
154200000000     C*
154300000000     C     1             DO        24            I
154400000000     C                   MOVEL     FUD(I)        FUDRIG
154500000000     C     FUDFUN        IFEQ      '23'
154600000000     C     FUDATT        ANDEQ     'S'
154700000000     C                   MOVEL     FUDDES        FUN007
154800000000     C                   END
154900000000     C     FUDFUN        IFEQ      '24'
155000000000     C     FUDATT        ANDEQ     'S'
155100000000     C                   MOVEL     FUDDES        FUN008
155200000000     C                   END
155300000000     C                   END                                                    DO
155400000000     C*
155500000000     C                   ENDSR
155600000000     C*================================================================
155700000000     C* OPZGES - GESTIONE VISUALIZZAZIONE OPZIONI GESTITE DAL PGM
155800000000     C*================================================================
155900000000     C     OPZGES        BEGSR
156000000000     C*
156100000000     C* PULISCO CAMPI DI LAVORO
156200000000     C                   Z-ADD     0             W                 5 0
156300000000     C*
156400000000     C* RICERCO QUANTE OPZIONI SONO ATTIVE
156500000000     C* E CARICO TABELLA OPZIONI ATTIVE
156600000000     C     1             DO        24            I
156700000000     C                   MOVEL     OPD(I)        OPDRIG
156800000000     C*
156900000000     C* PULIZIA OPZIONI PRECEDENTI
157000000000     C                   MOVEL     *BLANKS       OPV(I)
157100000000     C*
157200000000     C* IMPOSTA OPZIONE ATTIVA IN TABELLA
157300000000     C     OPDATT        IFEQ      'S'
157400000000     C                   ADD       1             W
157500000000     C                   MOVEL     OPDDES        OPV(W)
157600000000     C                   END
157700000000     C                   END                                                    DO WHILE
157800000000     C*
157900000000     C* SE VI SONO PIU' DI 8 OPZIONI ATTIVE, ATTIVO FUNZIONE F23
158000000000     C* ALTRIMENTI DISATTIVO F23
158100000000     C     1             DO        24            I
158200000000     C                   MOVEL     FUD(I)        FUDRIG
158300000000     C*
158400000000     C     FUDFUN        IFEQ      '23'
158500000000     C     W             IFGT      8
158600000000     C                   MOVEL     'S'           FUDATT
158700000000     C                   ELSE
158800000000     C                   MOVEL     *BLANKS       FUDATT
158900000000     C                   END                                                    W GT 8
159000000000     C                   MOVEL     FUDRIG        FUD(I)
159100000000     C                   END
159200000000     C                   END                                                    DO WHILE
159300000000     C*
159400000000     C* IMPOSTO QUANTI GRUPPI DI 8 OPZIONI SI POSSONO VISUALIZZARE
159500000000     C     W             DIV       8             I
159600000000     C                   MVR                     Z                 5 0
159700000000     C*
159800000000     C     Z             IFNE      0
159900000000     C     I             ADD       1             Z
160000000000     C                   ELSE
160100000000     C                   Z-ADD     I             Z
160200000000     C                   END
160300000000     C*
160400000000     C* IMPOSTO SUCCESSIVE 8 OPZIONI ATTIVE A VIDEO
160500000000     C                   ADD       1             OPZGRU
160600000000     C     OPZGRU        IFGT      Z
160700000000     C     OPZGRU        ORLT      1
160800000000     C                   Z-ADD     1             OPZGRU
160900000000     C                   END
161000000000     C*
161100000000     C* PRIMO GRUPPO DI 8 OPZIONI
161200000000     C     OPZGRU        IFEQ      1
161300000000     C                   MOVEL     OPV(1)        OPZ001
161400000000     C                   MOVEL     OPV(2)        OPZ002
161500000000     C                   MOVEL     OPV(3)        OPZ003
161600000000     C                   MOVEL     OPV(4)        OPZ004
161700000000     C                   MOVEL     OPV(5)        OPZ005
161800000000     C                   MOVEL     OPV(6)        OPZ006
161900000000     C                   MOVEL     OPV(7)        OPZ007
162000000000     C                   MOVEL     OPV(8)        OPZ008
162100000000     C                   END
162200000000     C*
162300000000     C* SECONDO GRUPPO DI 8 OPZIONI
162400000000     C     OPZGRU        IFEQ      2
162500000000     C                   MOVEL     OPV(9)        OPZ001
162600000000     C                   MOVEL     OPV(10)       OPZ002
162700000000     C                   MOVEL     OPV(11)       OPZ003
162800000000     C                   MOVEL     OPV(12)       OPZ004
162900000000     C                   MOVEL     OPV(13)       OPZ005
163000000000     C                   MOVEL     OPV(14)       OPZ006
163100000000     C                   MOVEL     OPV(15)       OPZ007
163200000000     C                   MOVEL     OPV(16)       OPZ008
163300000000     C                   END
163400000000     C*
163500000000     C* TERZO GRUPPO DI 8 OPZIONI
163600000000     C     OPZGRU        IFEQ      3
163700000000     C                   MOVEL     OPV(17)       OPZ001
163800000000     C                   MOVEL     OPV(18)       OPZ002
163900000000     C                   MOVEL     OPV(19)       OPZ003
164000000000     C                   MOVEL     OPV(20)       OPZ004
164100000000     C                   MOVEL     OPV(21)       OPZ005
164200000000     C                   MOVEL     OPV(22)       OPZ006
164300000000     C                   MOVEL     OPV(23)       OPZ007
164400000000     C                   MOVEL     OPV(24)       OPZ008
164500000000     C                   END
164600000000     C                   ENDSR
164700000000     C*================================================================
164800000000     C*  OPZCON - CONTROLLO ED ESECUZIONE OPZIONI SCELTE
164900000000     C*================================================================
165000000000     C     OPZCON        BEGSR
165100000000     C*
165200000000     C* PULISCO CAMPI DI LAVORO
165300000000     C                   Z-ADD     0             I                 5 0
165400000000     C                   Z-ADD     0             W                 5 0
165500000000     C*
165600000000     C* RICERCA OPZIONE SCELTA IN TABELLA OPZIONI
165700000000     C     1             DO        24            I
165800000000     C                   MOVEL     OPD(I)        OPDRIG
165900000000     C*
166000000000     C* SALVO INDICE DI OPZIONE TROVATA ED ATTIVA (W)
166100000000     C     OPZKEY        IFEQ      OPDOPZ
166200000000     C     OPZKEY        OREQ      OPDOP1
166300000000     C*
166400000000     C     OPDATT        IFEQ      'S'
166500000000     C                   MOVEL     OPDOPZ        OPZKEY
166600000000     C                   Z-ADD     I             W
166700000000     C                   END
166800000000     C                   END
166900000000     C*
167000000000     C                   END                                                    DO WHILE
167100000000     C*
167200000000     C* OPZIONE NON ATTIVA (ERRORE)
167300000000     C     W             IFEQ      0
167400000000     C*
167500000000     C* OPZIONE NON OK
167600000000     C                   Z-ADD     1             OPZOK
167700000000     C*
167800000000     C* EMISSIONE MESSAGGIO DI ERRORE
167900000000     C                   MOVEL     MSG(1)        DSMSMS
168000000000     C                   ELSE
168100000000     C*
168200000000     C* OPZIONE ATTIVA (PROCEDI ...)
168300000000     C*
168400000000     C     OPZKEY        IFEQ      ' 1'
168500000000     C                   EXSR      O01GES
168600000000     C                   END
168700000000     C     OPZKEY        IFEQ      ' 2'
168800000000     C                   EXSR      O02GES
168900000000     C                   END
169000000000     C     OPZKEY        IFEQ      ' 3'
169100000000     C                   EXSR      O03GES
169200000000     C                   END
169300000000     C     OPZKEY        IFEQ      ' 4'
169400000000     C                   EXSR      O04GES
169500000000     C                   END
169600000000     C     OPZKEY        IFEQ      ' 5'
169700000000     C                   EXSR      O05GES
169800000000     C                   END
169900000000     C     OPZKEY        IFEQ      ' 6'
170000000000     C                   EXSR      O06GES
170100000000     C                   END
170200000000     C     OPZKEY        IFEQ      ' 7'
170300000000     C                   EXSR      O07GES
170400000000     C                   END
170500000000     C*
170600000000     C                   END                                                    OPZ.NON ATTIVA
170700000000     C*
170800000000     C                   ENDSR
170900000000     C*================================================================
171000000000     C* NOTE PER COMPILAZIONE SCHIERE
171100000000     C*================================================================
171200000000     C* -MSG-
171300000000     C* INSERIRE I VARI MESSAGGI EMESSI DAL PGM
171400000000     C* (NON TOCCARE MSG,1 E MSG,2
171500000000     C*
171600000000     C* -CMD-
171700000000     C* INSERIRE I VARI COMANDI DI SISTEMA RICHIESTI NEL PGM
171800000000     C*
171900000000     C* -OPD-
172000000000     C* INSERIRE LE VARIE OPZIONI GESTITE DAL PGM
172100000000     C*  . SI PUO' USARE LA OPZIONE ALTERNATIVA PER GESTIRE
172200000000     C*    I CASI DI OPZIONI CON UN SOLO CARATTERE
172300000000     C*    ESEMPIO: OPZIONI -1 -
172400000000     C*                     - 1-  SONO LA STESSA COSA
172500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
172600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
172700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
172800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
172900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
173000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
173100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM NON FA NULLA
173200000000     C*
173300000000     C* -FUD-
173400000000     C* INSERIRE LE VARIE FUNZIONI GESTITE DAL PGM
173500000000     C*  . DEVONO ESSERE IN ORDINE DI PRESENTAZIONE A VIDEO
173600000000     C*  . METTERE S/N (SI/NO) PER INDICARE QUELLE GESTITE A PGM
173700000000     C*  . POSSONO ESSERE IN ORDINE SPARSO
173800000000     C*  . PUO' ESSERE VARIATO IL CONTENUTO NEL PGM
173900000000     C*    (VIENE RICARICATA LA VISUALIZZAZIONE OGNI CICLO)
174000000000     C*  . SE MESSO S/N=S E NEL PGM NON E' GESTITA ALLORA:
174100000000     C*    . VIENE VISUALIZZATA A VIDEO MA IL PGM DICE "NON GESTITA"
174200000000     C*  . METTERE F23 E F24 CON S/N=S  (RICALCOLATE NEL PGM)
174300000000     C*  . SEGUIRE LE NOTE MESSE NELLA SCHIERA
174400000000     C*
174500000000     C*================================================================
174600000000** ======== SCHIERA: MSG  (MESSAGGI)                       ================
174700000000TNSDUCR- Opzione non gestita.                                                  1
174800000000TNSDUCR- Tasto funzionale non gestito.                                         2
174900000000TNSDUCR- Non abilitato alla gestione di questa Unità.                          3
175000960927TNSDUCR- Dotazione P.operativo annullata. Consentito solo il ripristino        4
175100960927TNSDUCR- Dotazione P.operativo non annullata, impossibile ripristinare.        5
175200000000TNSDUCR- Accettato solo un parametro di inizio lista.                          6
175300000000TNSDUCR- Tipo unità non accettabile.                                           7
175400020405TNSDUCR- Magazzino ubicazione non accettabile.                                 8
175500020717TNSDUCR- Profilo non abilitato alla funzione                                   9
175600000000TNSDUCR- 10
175700000000TNSDUCR- 11
175800000000TNSDUCR- 12
175900000000TNSDUCR- 13
176000000000TNSDUCR- 14
176100000000TNSDUCR- 15
176200000000TNSDUCR- 16
176300000000TNSDUCR- 17
176400000000TNSDUCR- 18
176500000000TNSDUCR- 19
176600000000TNSDUCR- 20
176700000000** ======== SCHIERA: CMD  (COMANDI)                        ================
176800000000WRKSPLF
176900000000
177000000000
177100000000
177200000000
177300000000** ======== SCHIERA: OPD  OPZIONI GESTITE NEL PGM (ATTIVA=S) ==========
177400000000 1=Scelta          -1 -     (S/N)=N                        opz. 01¬
177500000000 2=Modifica        -2 -     (S/N)=N                        opz. 02¬
177600000000                   -3 -     (S/N)=N                        opz. 03¬
177700000000 4=Cancellazione   -4 -     (S/N)=N                        opz. 04¬
177800000000 5=Visualizzazione -5 -     (S/N)=N                        opz. 05¬
177900000000                   -6 -     (S/N)=N                        opz. 06¬
178000000000 7=Ripristino      -7 -     (S/N)=N                        opz. 07¬
178100000000                   -8 -     (S/N)=N                        opz. 08¬
178200000000                   -9 -     (S/N)=N                        opz. 09¬
178300000000                   -  -     (S/N)=N                        opz. 10¬
178400000000                   -  -     (S/N)=N                        opz. 11¬
178500000000                   -  -     (S/N)=N                        opz. 12¬
178600000000                   -  -     (S/N)=N                        opz. 13¬
178700000000                   -  -     (S/N)=N                        opz. 14¬
178800000000                   -  -     (S/N)=N                        opz. 15¬
178900000000                   -  -     (S/N)=N                        opz. 16¬
179000000000                   -  -     (S/N)=N                        opz. 17¬
179100000000                   -  -     (S/N)=N                        opz. 18¬
179200000000                   -  -     (S/N)=N                        opz. 19¬
179300000000                   -  -     (S/N)=N                        opz. 20¬
179400000000                   -  -     (S/N)=N                        opz. 21¬
179500000000                   -  -     (S/N)=N                        opz. 22¬
179600000000                   -  -     (S/N)=N                        opz. 23¬
179700000000                   -  -     (S/N)=N                        opz. 24¬
179800000000** ======== SCHIERA: FUD  TASTI FUNZION. GESTITI NEL PGM (ATTIVA=S)
179900000000                   -        (S/N)=N                        funz.01¬
180000000000                   -        (S/N)=N                        funz.02¬
180100000000F03=Fine           -        (S/N)=S  <== S-FISSO           funz.03¬
180200000000                   -        (S/N)=N                        funz.04¬
180300971013F05=Rivisualizza   -        (S/N)=S  <== S-FISSO           funz.05¬
180400000000F06=Inserimento    -        (S/N)=N                        funz.06¬
180500000000                   -        (S/N)=N                        funz.07¬
180600000000                   -        (S/N)=N                        funz.08¬
180700000000                   -        (S/N)=N                        funz.09¬
180800100922                   -        (S/N)=N                        funz.10¬
180900100922                   -        (S/N)=N                        funz.11¬
181000000000F12=Ritorno        -        (S/N)=S  <== S-FISSO           funz.12¬
181100000000                   -        (S/N)=N                        funz.13¬
181200000000                   -        (S/N)=N                        funz.14¬
181300000000                   -        (S/N)=N                        funz.15¬
181400000000                   -        (S/N)=N                        funz.16¬
181500000000F17=Inizio dati    -        (S/N)=S  <== S-FISSO           funz.17¬
181600000000F18=Fine dati      -        (S/N)=S  <== S-FISSO           funz.18¬
181700000000                   -        (S/N)=N                        funz.19¬
181800000000                   -        (S/N)=N                        funz.20¬
181900020405F21=Stampa         -        (S/N)=S                        funz.21¬
182000941220F22=Gestione stampe-        (S/N)=S                        funz.22¬
182100000000F23=Altre opzioni  -        (S/N)=S  <== S-FISSO           funz.23¬
182200000000F24=Altri tasti    -        (S/N)=S  <== S-FISSO           funz.24¬
182300100922 RU=Pagina su      -        (S/N)=S  <== S-FISSO           funz.10¬
